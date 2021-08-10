!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2020.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

! $Id: fourier_analysis.f90 65778 2020-01-14 14:07:42Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/fourier_analysis.f90 $
!-------------------------------------------------------------------------------------------------------
!  Origin:
!     URL: https://svn.oss.deltares.nl/repos/delft3d/trunk/src/engines_gpl/flow2d3d/packages/data/include/fourier.igs
!     Revision: 5108

module m_fourier_analysis

! TODO:
!     * ucx en ucy on (1:#nods) is altijd de diepte gemiddelde, ook in 2d (dan is het de enige snelheid)

    use precision
    use string_module, only: str_lower
    use unstruc_netcdf
    use m_flow, only : kmx
    use m_alloc
    use m_flowtimes, only : dt_user, irefdate, Tzone
    implicit none

    !> struct to enable different sizes of suma and sumb
    type fdata
       real(kind=fp), allocatable :: rdata(:)  !< actual data for suma and sumb of fourier analysis
       real(kind=sp), allocatable :: sdata(:)  !< actual data for min/max calculations
    end type

    type gd_fourier
       !
       ! integers
       !
       real(kind=fp) :: fouwrt       ! Time to write fourier file
       integer       :: nofouvar = 0 ! Number of parameters to write to NetCDF file
       integer       :: ibluc
       integer       :: iblws
       !
       ! pointers
       !
       integer      , dimension(:)    , pointer :: fconno  => null() !< Constituent number for Fourier analysis
       integer      , dimension(:)    , pointer :: fnumcy  => null() !< Number of cycles for fourier analysis
       real(kind=fp), dimension(:)    , pointer :: ftmsto  => null() !< stop time for fourier analysis
       real(kind=fp), dimension(:)    , pointer :: ftmstr  => null() !< start time for fourier analysis
       integer      , dimension(:)    , pointer :: foumask => null() !< 0: no additional mask, 1: initial dry points only
       integer      , dimension(:,:)  , pointer :: idvar   => null() !< Ids of the variables in UGRID format
       integer      , dimension(:,:)  , pointer :: fouref  => null() !< Reference table: (ifou,1): fouid (line in input file)
                                                                     !!                  (ifou,2): fouvarstart (first index in fouvarnam/idvar to be used by this ifou
       !
       real(kind=fp), dimension(:)    , pointer :: fknfac  => null() !< Fourier amplitude correction factor
       real(kind=fp), dimension(:)    , pointer :: foufas  => null() !< Frequency for fourier analysis
       type(fdata),   dimension(:)    , pointer :: fousma  => null() !< Suma of fourier analysis
       type(fdata),   dimension(:)    , pointer :: fousmb  => null() !< Sumb of fourier analysis
       real(kind=fp), dimension(:)    , pointer :: fv0pu   => null() !< Fourier phase correction
       !
       character(len=1) , dimension(:)    , pointer :: fouelp        => null() !< Y/N: Yes/No requesting elliptic parameters
                                                                               !! X/I: Max/Min values  requested instead of fourier analysis
                                                                               !! E  : Max Energy head requested instead of fourier analysis
       character(len=16), dimension(:)    , pointer :: founam        => null() !< Names of variables for fourier analysis
       character(len=50), dimension(:)    , pointer :: fouvarnam     => null() !< Names of variables for fourier analysis as written to NetCDF file
       character(len=50), dimension(:)    , pointer :: fouvarnamlong => null() !< Part of the long names of variables for fourier analysis as written to NetCDF file
       character(len=50), dimension(:)    , pointer :: fouvarunit    => null() !< Unit of variables for fourier analysis as written to NetCDF file
    end type gd_fourier
!-------------------------------------------------------------------------------------------------------

    type(gd_fourier), target :: gdfourier
    character(len=:), allocatable   :: FouOutputFile

    private

    integer                   :: nofou  !< Number of fourier components to be analyzed
    type(t_unc_mapids)        :: fileids!< Set of file and variable ids for this file.

    real(kind=fp)             :: ag_fouana = 9.81d0
    real(kind=fp)             :: time_unit_factor

    real(kind=fp), parameter   :: defaultd = -999.0_fp ! Default value for doubles
    real(kind=fp), parameter   :: tol_time = 1d-9      ! tolerance for comparing times

    public :: fouini
    public :: alloc_fourier_analysis_arrays
    public :: reafou
    public :: postpr_fourier

    public :: fourierIsActive, fourierWithUc
    public :: nofou
    public :: FouOutputFile

    contains

!> fourier is (still) active
   logical function fourierIsActive()
       fourierIsActive = (nofou > 0)
    end function fourierIsActive

!> do fourier with flow magnitude or not
    logical function fourierWithUc()
       fourierWithUc = (gdfourier%ibluc>0)
    end function fourierWithUc

!> count the number of fourier/min/max quantities
    subroutine count_fourier_variables
       implicit none
       integer :: ivar

       gdfourier%iblws = 0
       gdfourier%ibluc = 0
       do ivar=1, nofou
          !
          select case (gdfourier%founam(ivar))
          case ('ws')
             gdfourier%iblws = gdfourier%iblws + 1
          case ('uc')
             gdfourier%ibluc = gdfourier%ibluc + 1
          end select
       enddo
    end subroutine count_fourier_variables

!> allocate the arrays for fourier and min/max
   subroutine alloc_fourier_analysis_arrays()
   !!--declarations----------------------------------------------------------------
       use precision

       !
       implicit none
       !

       integer                      ::    istat
       integer, parameter           ::    imissval = -1
       double precision, parameter  ::    rmissval = -999.999_fp

       istat = 0
       !
       ! Arrays for Fourier analysis (fourier.igs)
       !
       if (istat == 0) call reallocp (gdfourier%fconno, nofou, stat = istat, keepExisting = .false.)
       if (istat == 0) call reallocp (gdfourier%fnumcy ,nofou, stat = istat, keepExisting = .false.)
       if (istat == 0) call reallocp (gdfourier%ftmsto ,nofou, stat = istat, keepExisting = .false.)
       if (istat == 0) call reallocp (gdfourier%ftmstr ,nofou, stat = istat, keepExisting = .false.)
       if (istat == 0) call reallocp (gdfourier%foumask,nofou, stat = istat, keepExisting = .false.)
       if (istat == 0) call reallocp (gdfourier%idvar, [MAX_ID_VAR, gdfourier%nofouvar], stat = istat, keepExisting = .false.)
       if (istat == 0) call reallocp (gdfourier%fouref, [nofou, 2], stat = istat, keepExisting = .false.)
       !
       if (istat == 0) call reallocp (gdfourier%fknfac , nofou, stat = istat, keepExisting = .false.)
       if (istat == 0) call reallocp (gdfourier%foufas , nofou, stat = istat, keepExisting = .false.)
       if (istat == 0) allocate (gdfourier%fousma(nofou), stat = istat)
       if (istat == 0) allocate (gdfourier%fousmb(nofou), stat = istat)
       if (istat == 0) call reallocp (gdfourier%fv0pu  , nofou, stat = istat, keepExisting = .false.)
       !
       if (istat == 0) call reallocp (gdfourier%fouelp, nofou, stat = istat, keepExisting = .false.)
       if (istat == 0) call reallocp (gdfourier%founam, nofou, stat = istat, keepExisting = .false.)
       if (istat == 0) call reallocp (gdfourier%fouvarnam     , gdfourier%nofouvar, stat = istat, keepExisting = .false.)
       if (istat == 0) call reallocp (gdfourier%fouvarnamlong , gdfourier%nofouvar, stat = istat, keepExisting = .false.)
       if (istat == 0) call reallocp (gdfourier%fouvarunit    , gdfourier%nofouvar, stat = istat, keepExisting = .false.)

       if (istat /= 0) then
          ! Exception handling for allocation of fourier arrays
          msgbuf = 'Allocation error in alloc_fourier_analysis_arrays; Fourier disabled.'
          call err_flush()
          nofou = 0
       else
          ! Initialise arrays for Fourier analysis
          gdfourier%fnumcy   = imissval
          !
          gdfourier%ftmsto   = rmissval
          gdfourier%ftmstr   = rmissval
          gdfourier%fknfac   = rmissval
          gdfourier%foufas   = rmissval
          gdfourier%fv0pu    = rmissval
          !
          gdfourier%founam        = ' '
          gdfourier%fouvarnam     = ' '
          gdfourier%fouvarnamlong = ' '
          gdfourier%fouvarunit    = ' '
          !
       endif
   end subroutine alloc_fourier_analysis_arrays

!> - Read fourier input file and stores the
!! variables necessary for the analysis in arrays.
   subroutine reafou(lunfou, filfou, kmax, lstsc, lsal, ltem, tstart, tstop, success)
   !!--declarations----------------------------------------------------------------
       use precision
       use mathconsts
       use string_module
       use unstruc_messages
       !
       implicit none
   !
   ! Global variables
   !
       integer      , intent(in   ) :: lsal        !< Description and declaration in dimens.igs
       integer      , intent(in   ) :: lstsc       !< Description and declaration in dimens.igs
       integer      , intent(in   ) :: ltem        !< Description and declaration in dimens.igs
       integer      , intent(in   ) :: lunfou      !< Unit number fourier input file
       character(*) , intent(in   ) :: filfou      !< File name for fourier analysis input
       integer      , intent(in   ) :: kmax        !< number of vertical layers
       real(kind=fp), intent(in   ) :: tstart      !< simulation start time
       real(kind=fp), intent(in   ) :: tstop       !< simulation stop time
       logical      , intent(  out) :: success     !< function result
   !
   ! Local variables
   !
       integer          , dimension(:)     , pointer :: fconno
       integer          , dimension(:)     , pointer :: fnumcy
       integer          , dimension(:)     , pointer :: foumask
       integer          , dimension(:,:)   , pointer :: idvar
       integer          , dimension(:,:)   , pointer :: fouref
       real(kind=fp)                       , pointer :: fouwrt
       integer                             , pointer :: nofouvar
       real(kind=fp)    , dimension(:)     , pointer :: ftmsto
       real(kind=fp)    , dimension(:)     , pointer :: ftmstr
       real(kind=fp)    , dimension(:)     , pointer :: fknfac
       real(kind=fp)    , dimension(:)     , pointer :: foufas
       type(fdata)      , dimension(:)     , pointer :: fousma
       type(fdata)      , dimension(:)     , pointer :: fousmb
       real(kind=fp)    , dimension(:)     , pointer :: fv0pu
       character(len=1) , dimension(:)     , pointer :: fouelp
       character(len=16), dimension(:)     , pointer :: founam
       character(len=50), dimension(:)     , pointer :: fouvarnam
       character(len=50), dimension(:)     , pointer :: fouvarnamlong
       character(len=50), dimension(:)     , pointer :: fouvarunit

       integer                               :: fouid               ! Counter linenumber-commentlines
       integer                               :: i                   ! Counter
       integer                               :: ifou                ! Counter
       integer                               :: ivar                ! Counter
       integer                               :: flayno              ! layer number (ignored)
       integer                               :: sizea, sizeb        ! size of the fourier arrays a and b
       integer                               :: irelp
       integer                               :: linenumber          ! Line number in Fourier input file
       integer                               :: nveld               ! actual number of fields encountered in a record
       real(kind=fp)                         :: rstart              ! Start time for fourier analysis
       real(kind=fp)                         :: rstop               ! Stop  time for fourier analysis
       real(kind=fp)                         :: fillValue           ! value to initialize array
       character(len=4)                      :: cdummy              ! Help string to read FOUELP
                                                                    ! The message depends on the error.
       character(len=300)                    :: message
       character(len=132)                    :: record              ! Used for format free reading
       character(len=30), allocatable        :: columns(:)          ! each record is split into separate fields (columns)
       character(len=3)                      :: cref                ! ref. number converted into a string
       integer                               :: iostat              ! error code file io
       integer                               :: ierr                ! error code allocate
   !
   !! executable statements -------------------------------------------------------
   !
       fknfac        => gdfourier%fknfac
       foufas        => gdfourier%foufas
       fv0pu         => gdfourier%fv0pu
       fconno        => gdfourier%fconno
       fnumcy        => gdfourier%fnumcy
       foumask       => gdfourier%foumask
       idvar         => gdfourier%idvar
       fouref        => gdfourier%fouref
       fouwrt        => gdfourier%fouwrt
       nofouvar      => gdfourier%nofouvar
       ftmsto        => gdfourier%ftmsto
       ftmstr        => gdfourier%ftmstr
       fouelp        => gdfourier%fouelp
       founam        => gdfourier%founam
       fouvarnam     => gdfourier%fouvarnam
       fouvarnamlong => gdfourier%fouvarnamlong
       fouvarunit    => gdfourier%fouvarunit
       fousma        => gdfourier%fousma
       fousmb        => gdfourier%fousmb

       !
       success = .false.
       ifou = 1
       do i = 1, nofou
          fconno(i)   = 1
          foumask(i)  = 0
          fouref(i,:) = -1
          fouelp(i)   = 'n'
       enddo
       do i = 1, nofouvar
          idvar(:,i)       = 0
       enddo
       !
       cdummy = ' '
       !
       linenumber = 0
       fouid      = 0
       !
       ! reading file
       !
       ! -->
      20 continue
       read (lunfou, '(a)',iostat=iostat) record
       if (iostat/=0) then
          write (msgbuf, '(a)') 'Error reading record from .fou file.'
          call warn_flush()
          goto 6666
       endif
       !
       linenumber = linenumber + 1
       if (record(1:1)=='*' .or. record == ' ') goto 20
       fouid      = fouid      + 1
       !
       call str_lower(record, 132)
       if (allocated(columns)) deallocate(columns)
       call strsplit(record, 1, columns, 1)
       nveld = size(columns)
       !
       ! determine array names and type (scalar or vectorial) for fourier analysis
       !
       founam(ifou) = columns(1)
       !
       select case (founam(ifou))
       case ('wl')
          founam(ifou)   = 's1'
          fouref(ifou,1) = fouid
       case ('ws')             ! absolute wind-speed
          founam(ifou)   = 'ws'
          fouref(ifou,1) = fouid
       case ('ux', 'uxa')
          fouref(ifou,1) = fouid
       case ('uy', 'uya')
          fouref(ifou,1) = fouid
       case ('eh')
          !
          ! founam must be s1 to pass through s1 to fouana
          ! use fouelp to flag energy head
          !
          founam(ifou)   = 's1'               ! waterlevel
          fouref(ifou,1) = fouid
          fouelp(ifou)   = 'e'
       case ('uc')        ! absolute cell-centre velocity magnitude
          founam(ifou)   = 'uc'
          fouref(ifou,1) = fouid
       case ('qf')        ! interpolated cell-centre velocities (vector)
          founam(ifou)     = 'qxk'             ! ucx
          fouref(ifou,1)   = fouid
       case ('bs')
          founam(ifou)     = 'ta'
          fouref(ifou,1)   = fouid
       case ('ct')        ! constituent: temperature (scalar)
          if (ltem/=0) then
             founam(ifou)   = 'r1'
             fconno(ifou)   = ltem
             fouref(ifou,1) = fouid
          else
             ! Exception: no temperature
             msgbuf = 'Temperature specified in .fou file, but no temperature available. '
             call warn_flush()
             goto 6666
          endif
       case ('cs')                     ! constituent: salinity (scalar)
          if (lsal/=0) then
             founam(ifou)   = 'r1'
             fconno(ifou)   = lsal
             fouref(ifou,1) = fouid
          else
             ! Exception: no salt
             msgbuf = 'Salinity specified in .fou file, but no salinity available.'
             call warn_flush()
             goto 6666
          endif
       case default                    ! constituent, anything else
          read (founam(ifou)(2:2), '(i1)', iostat=iostat) fconno(ifou)
          if (iostat /= 0) then
             msgbuf = 'Unable to initialize fourier analysis for ''' // trim(founam(ifou)) // '''.'
             call warn_flush()
             goto 6666
          endif
          fconno(ifou) = fconno(ifou) + max(lsal, ltem)
          if (fconno(ifou)>lstsc) then
             msgbuf = 'Unable to initialize fourier analysis for constituent ''' // trim(founam(ifou)) // '''.'
             call warn_flush()
             goto 6666
          endif
          founam(ifou)   = 'r1'
          fouref(ifou,1) = fouid
       end select
       !
       ! read start time, stop time, number of cycles
       !       determine corresponding integer time step numbers and frequency
       !
       read (columns(2), *, err=6666) rstart
       rstart = rstart * time_unit_factor           ! convert to kernel time unit
       !
       ftmstr(ifou) = rstart
       !
       if (rstart<tstart) then
          msgbuf = 'Fourier sample interval start preceeds simulation start TStart.'
          call warn_flush()
          goto 6666
       endif
       !
       read (columns(3), *, err=6666) rstop
       rstop = rstop * time_unit_factor           ! convert to kernel time unit
       !
       ftmsto(ifou) = rstop
       !
       if (rstop>tstop) then
          msgbuf = 'Fourier sample interval stop exceeds simulation end TStop.'
          call warn_flush()
          goto 6666
       endif
       !
       ! Fouwrt catches the end of all fourier analyses
       !
       fouwrt = max(fouwrt, ftmsto(ifou))
       !
       read (columns(4), *, err=6666) fnumcy(ifou)
       !
       foufas(ifou) = twopi * real(fnumcy(ifou),fp) / (ftmsto(ifou) - ftmstr(ifou))
       !
       ! read nodal amplifications and phase shifts for comparison
       !       with cotidal maps
       !
       read (columns(5), *, err=6666) fknfac(ifou)
       !
       read (columns(6), *, err=6666) fv0pu(ifou)
       !
       if (fv0pu(ifou)<0.0_fp) fv0pu(ifou) = fv0pu(ifou) + 360.0_fp
       fv0pu(ifou) = mod(fv0pu(ifou), 360.0_fp)
       !
       irelp = 7
       !
       if ( founam(ifou)(1:2)/='s1'  .and. &
            founam(ifou)(1:2)/='ta' .and. &
            founam(ifou)(1:3)/='uxa' .and. &
            founam(ifou)(1:3)/='uya' .and. &
            founam(ifou)(1:2)/='ws') then
          !
          read (columns(7), *, iostat=iostat) flayno
          if (iostat /= 0) then
             write (msgbuf, '(a,i0,2a)') 'Could not read layer number in line ', linenumber, ' and column 7 of file ', trim(filfou)
             call warn_flush()
             goto 6666
          else if (flayno>kmax) then
             write (msgbuf, '(a,i0,a,i0,a)') 'Requested layer exceeds,',flayno,' exceeds max layer, ',kmax,'.'
             call warn_flush()
             goto 6666
          endif
          irelp = irelp + 1
       endif
       !
       ! Elliptic parameters requested / MAX - MIN added
       !
       if (nveld>=irelp) then
          cdummy = trim(columns(irelp))
          !
          ! check for MAX and or MIN before Y/N
          !
          if (cdummy=='max' .or. fouelp(ifou)=='e' .or. cdummy=='min' .or. cdummy=='avg') then
             if (fnumcy(ifou)>0) then
                fnumcy(ifou) = 0
                foufas(ifou) = 0.0_fp
             endif
             if (fouelp(ifou)=='e') then
                select case (cdummy)
                   case ('min')
                      write (msgbuf, '(3a,i0,a)') 'in file ', trim(filfou), ' line ', linenumber, &
                            & ': energy head in combination with "min" is not supported'
                      call warn_flush()
                      goto 6666
                end select
             else
                select case (cdummy)
                   case ('max')
                      fouelp(ifou) = 'x'
                   case ('min')
                      fouelp(ifou) = 'i'
                   case ('avg')
                      fouelp(ifou) = 'a'
                end select
             endif
          else
             if (cdummy /= 'mean') then
                write (msgbuf, '(3a,i0,2a)') 'in file ', trim(filfou), ' line ', linenumber, &
                      & ': expecting avg, min, max or mean, instead of ', trim(cdummy)
                call warn_flush()
                goto 6666
             endif
          endif
       endif
       !
       if ((fouelp(ifou)=='x'.or.fouelp(ifou)=='e') .and. founam(ifou)=='s1') then
          !
          ! Currently, inidryonly is only implemented for fouelp=x, founam=s1 or eh
          !
          if (index(record,'inidryonly') > 0) then
             foumask(ifou) = 1
             write (message,'(3a,i0,a)') 'in file ', trim(filfou), ' line ', linenumber, &
                   & ': Fourier analysis only for initially dry points'
          endif
       endif
       !
       ifou = ifou + 1
       !
       if (ifou<=nofou) goto 20
       ! <--
       !
       success = .True.
       !
       ! Define all variable names to be written
       ! Add the (start-)index ivar to fouref(..,2)
       !
       ivar = 0
       do ifou = 1, nofou
          fouref(ifou,2)   = ivar + 1
          write(cref,'(i3.3)') fouref(ifou,1)
          select case (fouelp(ifou))
          case ('x')
             ivar = ivar + 1
             fouvarnam(ivar) = "fourier" // cref // "_max"
             fouvarnamlong(ivar) = "maximum value"
             call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
             if (founam(ifou) == 's1') then
                ivar = ivar + 1
                fouvarnam(ivar) = "fourier" // cref // "_max_depth"
                fouvarnamlong(ivar) = "maximum depth value"
                call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
             endif
             if (foumask(ifou) == 1) then
                fouvarnam    (ivar) = trim(fouvarnam    (ivar)) // "_inidryonly"
                fouvarnamlong(ivar) = trim(fouvarnamlong(ivar)) // ", initially dry points only"
             endif
          case ('e')
             ivar = ivar + 1
             fouvarnam(ivar) = "fourier" // cref // "_max"
             fouvarnamlong(ivar) = "maximum value"
             fouvarunit(ivar) = 'm'
             if (foumask(ifou) == 1) then
                fouvarnam    (ivar) = trim(fouvarnam    (ivar)) // "_inidryonly"
                fouvarnamlong(ivar) = trim(fouvarnamlong(ivar)) // ", initially dry points only"
             endif
          case ('i')
             ivar = ivar + 1
             fouvarnam(ivar) = "fourier" // cref // "_min"
             fouvarnamlong(ivar) = "minimum value"
             call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
             if (founam(ifou) == 's1') then
                ivar = ivar + 1
                fouvarnam(ivar) = "fourier" // cref // "_min_depth"
                fouvarnamlong(ivar) = "minimum depth value"
                call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
             endif
          case ('a')
             ivar = ivar + 1
             fouvarnam(ivar) = "fourier" // cref // "_avg"
             fouvarnamlong(ivar) = "average value"
             call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
          case default
             if (fnumcy(ifou)==0) then          ! zero fourier mode without further notice means 'MEAN'
                ivar = ivar + 1
                fouvarnam(ivar) = "fourier" // cref // "_mean"
                fouvarnamlong(ivar) = "average value"
                call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
             else                               ! non-zero fourier mode
                ivar = ivar + 1
                fouvarnam(ivar) = "fourier" // cref // "_amp"
                fouvarnamlong(ivar) = "Fourier amplitude"
                call setfouunit(founam(ifou), lsal, ltem, fconno(ifou), fouvarunit(ivar))
                ivar = ivar + 1
                fouvarnam(ivar) = "fourier" // cref // "_phs"
                fouvarnamlong(ivar) = "Fourier phase"
                fouvarunit(ivar)    = "degree"
             endif
          end select
       enddo

       ! init fourier arrays
       ierr = 0
       do ifou = 1, nofou
          call getsizes(ifou, sizea, sizeb)
          select case (fouelp(ifou))
             case ('e', 'x')
                fillValue = -1.0e+30_fp
             case ('i')
                fillValue =  1.0e+30_fp
             case default
                fillValue =  0.0_fp
          end select
          if (fouelp(ifou) == 'x' .or. fouelp(ifou) == 'i') then
             if (ierr == 0) call realloc(fousma(ifou)%sdata, sizea, stat=ierr, fill=real(fillValue,sp))
             if (ierr == 0) call realloc(fousmb(ifou)%sdata, sizeb, stat=ierr, fill=real(fillValue,sp))
          else
             if (ierr == 0) call realloc(fousma(ifou)%rdata, sizea, stat=ierr, fill=fillValue)
             if (ierr == 0) call realloc(fousmb(ifou)%rdata, sizeb, stat=ierr, fill=fillValue)
          endif
       enddo
       if (ierr /= 0) then
          msgbuf = 'allocation error in reafou'
          call err_flush()
       endif

       call count_fourier_variables()

       return
6666   continue   ! abort fourier analysis, something went wrong
       msgbuf = 'Invalid input for Fourier analysis, record=''' // trim(record) // ''''
       call warn_flush()
       msgbuf = 'Switching off fourier analysis......'
       call err_flush()
       nofou = 0
   end subroutine reafou

!> helper routine to get the size of suma and sumb arrays
subroutine getsizes(ifou, sizea, sizeb)
   integer, intent(in)  :: ifou     !< counter for fourier quantities
   integer, intent(out) :: sizea    !< size of suma
   integer, intent(out) :: sizeb    !< size of sumb

   sizea = name_dependent_size(trim(gdfourier%founam(ifou)))

   select case (gdfourier%fouelp(ifou))
      case('a', 'e')
         ! avg and max energy: sumb is not used (in avg only for time, so 1 element)
         sizeb = 1
      case('x', 'i')
         ! min and min: sumb is only used for waterdepth
         sizeb = merge(sizea, 1, gdfourier%founam(ifou) == 's1')
      case default
         ! real fourier analyse: sumb has the same size as suma
         sizeb = sizea
   end select
end subroutine getsizes

!> helper routine to get the size of suma arrays
function name_dependent_size(fourier_name) result(nmaxus)
   use m_flowgeom, only : gddimens
   character(len=*), intent(in) :: fourier_name  !< name of the fourier quantity
   integer                      :: nmaxus        !< function result: size of suma array

   ! The name of the variable for Fourier analysis fully specified the number of elements for the fourier loop
   select case (fourier_name)
   case('s1')
        nmaxus = gddimens%ndx
   case('ws')
        nmaxus = gddimens%lnx
   case('u1', 'v1')
        nmaxus = gddimens%lnkx
   case('qxk')
        nmaxus = gddimens%lnx
   case('ta')
        nmaxus = gddimens%ndx
   case('ux','uy','uc')
        nmaxus = gddimens%ndkx
   case('uxa','uya')
        nmaxus = gddimens%ndx
   case('r1')
        nmaxus = gddimens%ndkx
   case default
        nmaxus = gddimens%nub
   end select
end function name_dependent_size

subroutine setfouunit(founam, lsal, ltem, fconno, fouvarunit)
    !
    ! parameters
    character(*), intent(in)  :: founam
    integer     , intent(in)  :: lsal
    integer     , intent(in)  :: ltem
    integer     , intent(in)  :: fconno
    character(*), intent(out) :: fouvarunit
    !
    ! body
    select case (founam)
    case ('s1')
       fouvarunit = 'm'
    case ('ws', 'u1', 'v1', 'ux', 'uy', 'uc', 'uxa', 'uya')
       fouvarunit = 'm/s'
    case ('ta')
       fouvarunit = 'N m-2'
    case ('r1')
       if (fconno == ltem) then
          fouvarunit = 'degrees_Celsius'
       elseif (fconno == lsal) then
          fouvarunit = 'ppt'
       else
          fouvarunit = 'kg/m3'
       endif
    case default
       fouvarunit = ' '
    end select
end subroutine setfouunit

!> performs fourier analysis i.e. computes suma and sumb
!! - calculates MAX or MIN value
   subroutine fouana( ifou, time0, rarray, bl, dtw, nfou)
   !!--declarations----------------------------------------------------------------
       use precision
       use m_d3ddimens
       !
       implicit none
       !
   !
   ! Global variables
   !
       integer                      , intent(in)            :: ifou   !<  Counter
       real(kind=fp)                , intent(in)            :: time0  !<  Current time
       real(kind=fp)  , dimension(:), intent(in)            :: rarray !<  Array for fourier analysis
       real(kind=fp)                , intent(in)            :: dtw    !<  weight for time step
       real(kind=prec), dimension(:), intent(in)            :: bl
       integer                      , intent(inout)         :: nfou   !< counter for update fou quantities
       !
       ! The following list of pointer parameters is used to point inside the gdp structure
       !
       real(kind=fp)              :: ftmsto
       real(kind=fp)              :: ftmstr
       real(kind=fp)              :: foufas
       real(kind=fp)    , pointer :: fousma(:)
       real(kind=fp)    , pointer :: fousmb(:)
       real(kind=sp)    , pointer :: fousmas(:)
       real(kind=sp)    , pointer :: fousmbs(:)
       character(len=16), pointer :: founam
       integer                    :: nmaxus
   !
   ! Local variables
   !
       integer         :: n       ! Loop counter over NMAXUS
       real(kind=fp)   :: angl, cosangl_dtw, sinangl_dtw
   !
   !! executable statements -------------------------------------------------------
   !
       ftmsto    =  gdfourier%ftmsto(ifou)
       ftmstr    =  gdfourier%ftmstr(ifou)
       foufas    =  gdfourier%foufas(ifou)
       fousma    => gdfourier%fousma(ifou)%rdata
       fousmb    => gdfourier%fousmb(ifou)%rdata
       fousmas   => gdfourier%fousma(ifou)%sdata
       fousmbs   => gdfourier%fousmb(ifou)%sdata
       founam    => gdfourier%founam(ifou)

       ! Perform fourier analysis, every timestep as long as NST value
       ! lies in requested time interval FTMSTR and FTMSTO
       !
       ! The name of the variable for Fourier analysis fully specified the number of elements for the fourier loop
       nmaxus = name_dependent_size(founam)
       if (comparereal(time0, ftmstr, tol_time) /= -1 .and. comparereal(time0, ftmsto, tol_time) /= 1) then
          nfou = nfou + 1
          select case (gdfourier%fouelp(ifou))
          case ('x')
             !
             ! Calculate MAX value
             !
             if (founam == 's1') then

                do n = 1, nmaxus
                   !
                   ! Waterlevel (fousma) and waterdepth (fousmb),
                   !
                   fousmas(n) = max(fousmas(n), rarray(n))
                   fousmbs(n) = max(fousmbs(n), rarray(n) - real(bl(n),sp))               ! NOTE: bl is a HEIGHT (as bl in fm) and NOT a DEPTH (delft3d)
                enddo
             else
                do n = 1, nmaxus
                   fousmas(n) = max(fousmas(n), rarray(n))
                enddo
             endif
          case ('i')
             !
             ! Calculate MIN value
             !
             do n = 1, nmaxus
                fousmas(n) = min(fousmas(n), rarray(n))
             enddo
             if (founam == 's1') then
                do n = 1, nmaxus
                   fousmbs(n) = min(fousmbs(n), rarray(n) - real(bl(n),sp))
                enddo
             endif
          case ('a')
             !
             ! Calculate AVG value
             !
             do n = 1, nmaxus
                fousma(n) = fousma(n) + dtw * rarray(n)
             enddo
             fousmb(1) = fousmb(1) + dtw
          case default
             !
             ! Calculate total for fourier analyse
             !
             angl = (time0  - ftmstr) * foufas
             cosangl_dtw = cos(angl) * dtw
             sinangl_dtw = sin(angl) * dtw
             do n = 1, nmaxus
                fousma(n) = fousma(n) + rarray(n) * cosangl_dtw
                fousmb(n) = fousmb(n) + rarray(n) * sinangl_dtw
             enddo
          end select
       endif
   end subroutine fouana

!> - Checks if fourier analysis are requested
!!  and determines the number of variables for which a fourier analysis is requested
   subroutine fouini(lunfou, success, ag, time_unit_user, time_unit_kernel)
   !!--declarations----------------------------------------------------------------
       use precision
       use unstruc_messages
       !
       implicit none
   !
   ! Global variables
   !
       integer , intent(in)            :: lunfou   !!  Unit number for fourier input file
       real(kind=fp), intent(in)       :: ag       !!  override gravitational constant
       character(len=*), intent(in)    :: time_unit_user, time_unit_kernel
       logical                         :: success  !!  Flag=True if no error is encountered

   !
   ! Local variables
   !
       integer                            :: nveld  ! Used for format free reading
       character(len=300)                 :: record ! Used for format free reading 300 = 256 + a bit (field, =, ##, etc.)
       character(len=30), allocatable     :: columns(:)! each record is split into separate fields (columns)
       integer                            :: numcyc
       integer, pointer                   :: nofouvar
   !
   !! executable statements -------------------------------------------------------
   !
       time_unit_factor=1.0
       select case (time_unit_user)
       case('D')
          time_unit_factor=3600.*24.
       case('H')
          time_unit_factor=3600.
       case('M')
          time_unit_factor=60.
       case('S')
          time_unit_factor=1.
       end select

       select case (time_unit_kernel)
       case('D')
          time_unit_factor=time_unit_factor/(3600.*24.)
       case('H')
          time_unit_factor=time_unit_factor/(3600.)
       case('M')
          time_unit_factor=time_unit_factor/(60.)
       end select

       ! The user specified times in the .fou files need to be multiplied by the time_unit_factor, to correspond with the kernel times

       ag_fouana = ag
       nofouvar => gdfourier%nofouvar
       ! initialisation
       !
       nofou    = 0
       nofouvar = 0
       !
       ! cycle through file, while reading records
       !
       ! -->
       !
      10 continue
       read (lunfou, '(a)', end = 999) record
       if (record(1:1) == '*' .or. record == ' ') goto 10
       !
       ! reset record in smaller case characters and define contents
       !
       call str_lower(record, 300)
       if (allocated(columns)) deallocate(columns)
       call strsplit(record, 1, columns, 1)
       nveld = size(columns)
       !
       ! test for continuation record
       !
       ! requested fourier analysis water-level
       !
       read(columns(4),*,err=999) numcyc
       if (columns(1)(1:2)=='wl') then
          nofou = nofou + 1
          if (index(record,'max')>0 .or. index(record,'min')>0 ) then
             !
             ! max and min: also write max depth
             !
             nofouvar = nofouvar + 2
          else if (numcyc == 0) then
             nofouvar = nofouvar + 1
          else
             nofouvar = nofouvar + 2
          endif
       !
       ! requested fourier analysis wind-speed
       !
       elseif (columns(1)(1:2)=='ws') then
          nofou = nofou + 1
          if (numcyc == 0) then
             nofouvar = nofouvar + 1
          else
             nofouvar = nofouvar + 2
          endif
       !
       ! requested fourier analysis temperature
       !
       elseif (columns(1)(1:2)=='ct') then
          nofou = nofou + 1
          if (numcyc == 0) then
             nofouvar = nofouvar + 1
          else
             nofouvar = nofouvar + 2
          endif
       !
       ! requested fourier analysis cell-centred eastward and northward velocity
       !
       elseif (columns(1)(1:2)=='ux' .or. columns(1)(1:2)=='uy') then
          nofou = nofou + 1
          if (numcyc == 0) then
             nofouvar = nofouvar + 1
          else
             nofouvar = nofouvar + 2
          endif
       !
       ! requested fourier analysis cell centred velocity magnitude   ucmag
       !
       elseif (columns(1)(1:2)=='uc') then
          nofou = nofou + 1
          if (numcyc == 0) then
             nofouvar = nofouvar + 1
          else
             nofouvar = nofouvar + 2
          endif
       !
       ! requested fourier analysis salinity
       !
       elseif (columns(1)(1:2)=='cs') then
          nofou = nofou + 1
          if (numcyc == 0) then
             nofouvar = nofouvar + 1
          else
             nofouvar = nofouvar + 2
          endif
       !
       ! requested fourier analysis bed shear stress
       !
       elseif (columns(1)(1:2)=='bs') then
          nofou = nofou + 1
          if (numcyc == 0) then
             nofouvar = nofouvar + 1
          else
             nofouvar = nofouvar + 2
          endif
       !
       ! requested fourier analysis constituent
       !
       elseif ((columns(1)(1:1)=='c') .and. (index('12345',columns(1)(2:2))>0)) then
          nofou = nofou + 1
          if (numcyc == 0) then
             nofouvar = nofouvar + 1
          else
             nofouvar = nofouvar + 2
          endif
       else
          !
          ! requested fourier analysis undefined
          !
          msgbuf = 'Fourier analysis: variable keyword ''' // trim(columns(1)) // ''' not recognized, ignored'
          call msg_flush()
       endif
       !
       goto 10
       !
       ! <-- next record
       !
       !
   999 continue
       fileids%ncid = 0
       success = .True.
     rewind(lunfou)
   end subroutine fouini

!> do the actual fourier and min/max update
!! write to file after last update
    subroutine postpr_fourier(time0, dts)
    use m_transport, only : constituents
    use m_flowgeom, only : bl, lnx
    use m_flow
    implicit none

    real(kind=fp)     , intent(in) :: time0     !< Current time [seconds]
    real(kind=fp)     , intent(in) :: dts       !< Internal time step [seconds]

    ! NOTE: In DELFT3D depth is used, but bl is passed (positive bottomlevel). Defined direction is different

    !
    ! Perform analysis and write to Fourier file
    !
    integer                                  :: ifou
    integer                                  :: ierr
    integer                                  :: n
    real(kind=fp)                  , pointer :: fouwrt
    integer                                  :: nfou     ! counter for updated fou quantities
    character(len=16), dimension(:), pointer :: founam
    character(len=20)                        :: cnum     ! string to hold a number
    character(len=20)                        :: cquant   ! 'quantity' or 'quantities'
    double precision, pointer                :: fieldptr1(:)
    double precision, allocatable, target    :: wmag(:)  ! [m/s] wind magnitude    (m/s) at u point {"location": "edge", "shape": ["lnx"]}
    real(kind=fp)                            :: dtw

    fouwrt     => gdfourier%fouwrt
    founam     => gdfourier%founam
    dtw        =  dts / dt_user

    if (gdfourier%iblws > 0) then
       allocate(wmag(lnx), stat=ierr)
       if (ierr /= 0) then
          msgbuf = 'Allocation error in postpr_fourier; Fourier disabled.'
          call err_flush()
          nofou = 0
       else
          do n = 1, lnx
             wmag(n) = hypot(wx(n), wy(n))
          enddo
       endif
    endif

    if (nofou > 0) then
       nfou = 0
       do ifou = 1, nofou
          !
          ! Perform Fourier analysis, as long TIME0 lies in requested time interval FTMSTR and FTMSTO
          !
          ! Scalar type Fourier component
          !
          ! Get Fourier component pointer
          !
          select case (founam(ifou))
          case ('s1')                        ! waterlevels
             fieldptr1 => s1
          case ('ws')                        ! absolute wind magnitude
             fieldptr1 => wmag
          case ('ux')
             fieldptr1 => ucx
          case ('uy')
             fieldptr1 => ucy
          case ('uxa')
             fieldptr1 => ucxq
          case ('uya')
             fieldptr1 => ucyq
          case ('uc')                        ! ucmag, velocity magnitude
             fieldptr1 => ucmag
          case ('r1')
             fieldptr1 => constituents(gdfourier%fconno(ifou),:)
          case ('ta')
             call gettaus(1)
             fieldptr1 => taus
          case default
             continue         ! Unknown FourierVariable exception
          end select
          call fouana(ifou, time0, fieldptr1, bl, dtw, nfou)
       enddo

       if (allocated(wmag)) deallocate(wmag)

       if (nfou > 0) then
          write(cnum,*) real(time0)
          cquant = merge('quantities', 'quantity  ', nfou > 1)
          write(msgbuf,'(a,i0,4a)') 'Updated ', nfou, ' Fourier ', trim(cquant), ' for t = ', adjustl(cnum)
          call dbg_flush
       endif
       !
       ! Write results of fourier analysis to data file
       ! only once when all fourier periods are complete
       !
       if (comparereal(time0, fouwrt, tol_time) /= -1) then
          if (fileids%ncid == 0) then
             call wrfou()
             !
             ! clean up large fourier arrays
             !
             do ifou = 1, nofou
                if (allocated(gdfourier%fousma(ifou)%rdata)) deallocate(gdfourier%fousma(ifou)%rdata)
                if (allocated(gdfourier%fousmb(ifou)%rdata)) deallocate(gdfourier%fousmb(ifou)%rdata)
                if (allocated(gdfourier%fousma(ifou)%sdata)) deallocate(gdfourier%fousma(ifou)%sdata)
                if (allocated(gdfourier%fousmb(ifou)%sdata)) deallocate(gdfourier%fousmb(ifou)%sdata)
             enddo
             deallocate(gdfourier%fousma, gdfourier%fousmb)
             nullify(gdfourier%fousma)
             nullify(gdfourier%fousmb)
             nofou = 0  ! this stops calling postpr_fourier
          endif
       endif
    endif
    end subroutine postpr_fourier

!> - open fourier analysis output file
!! - writes results of fourier analysis to output file
!! - closes fourier analysis output file
    subroutine wrfou()
    !!--declarations----------------------------------------------------------------
        use precision, only : fp
        use mathconsts, only : raddeg
        use netcdf
        use unstruc_netcdf
        use m_sferic, only: jsferic
        use m_transport, only : namcon => const_names
        !
        implicit none
        !
        integer                                    :: nofouvar
        character(len=1) , dimension(:)  , pointer :: fouelp
        character(len=16), dimension(:)  , pointer :: founam
        character(len=50), dimension(:)  , pointer :: fouvarnam
        character(len=50), dimension(:)  , pointer :: fouvarnamlong
        character(len=50), dimension(:)  , pointer :: fouvarunit
        integer          , dimension(:,:), pointer :: fouref
        real(kind=fp)    , dimension(:)  , pointer :: foufas
        integer          , dimension(:)  , pointer :: fnumcy
        integer          , dimension(:,:), pointer :: idvar
    !
    ! Local variables
    !
        integer                       :: ierr
        integer                       :: ifou         ! Local teller for fourier functions
        integer                       :: ivar         ! Local teller for fourier functions
        real(kind=fp)                 :: freqnt       ! Frequency in degrees per hour
        real(kind=fp)                 :: tfasto       ! Stop time in minutes
        real(kind=fp)                 :: tfastr       ! Start time in minutes
        character(len=:), allocatable :: namfun       ! Local name for fourier function
        character(len=:), allocatable :: namfunlong   ! Local name for fourier function, including reference to the line in the fourier input file
        character(len=3)              :: cnumber      ! temp string for int2str conversion
        integer, parameter            :: imissval = -1
        integer                       :: unc_loc
        integer, allocatable          :: all_unc_loc(:)

        integer, parameter            :: REQUESTTYPE_DEFINE =  1
        integer, parameter            :: REQUESTTYPE_WRITE  =  2

    !
    !! executable statements -------------------------------------------------------
    !
        nofouvar      =  gdfourier%nofouvar
        fouelp        => gdfourier%fouelp
        founam        => gdfourier%founam
        fouvarnam     => gdfourier%fouvarnam
        fouvarnamlong => gdfourier%fouvarnamlong
        fouvarunit    => gdfourier%fouvarunit
        fouref        => gdfourier%fouref
        foufas        => gdfourier%foufas
        fnumcy        => gdfourier%fnumcy
        idvar         => gdfourier%idvar

        !
        ierr = unc_create(trim(FouOutputFile), 0, fileids%ncid)
        ierr = ug_addglobalatts(fileids%ncid, ug_meta_fm)
        call unc_write_flowgeom_filepointer_ugrid(fileids%ncid, fileids%id_tsp, 1)

        call realloc(all_unc_loc, nofou)
        !
        ifou  = 1
        do ivar=1, nofouvar
           if (ifou < nofou) then
              if (fouref(ifou+1,2) <= ivar) then
                 ifou = ifou + 1
              endif
           endif
           freqnt = foufas(ifou)*raddeg*3600.0_fp
           tfastr = gdfourier%ftmstr(ifou) / 60.0_fp
           tfasto = gdfourier%ftmsto(ifou) / 60.0_fp
           !
           select case (founam(ifou))
           case ('s1')
              unc_loc = UNC_LOC_S
              if (fouelp(ifou)=='e') then
                 namfun = 'energy head'
              else
                 namfun = 'water level'
              endif
           case ('ws')
              unc_loc = UNC_LOC_U
              namfun = 'wind speed'
           case ('ux')
              unc_loc = merge(UNC_LOC_S3D, UNC_LOC_S, kmx > 0)
              namfun = 'U-component of cell-centre velocity'
           case ('uy')
              unc_loc = merge(UNC_LOC_S3D, UNC_LOC_S, kmx > 0)
              namfun = 'V-component of cell-centre velocity'
           case ('uxa')
              unc_loc = UNC_LOC_S
              namfun = 'U-component velocity, column average'
           case ('uya')
              unc_loc = UNC_LOC_S
              namfun = 'V-component velocity, column average'
           case ('uc')
              unc_loc = merge(UNC_LOC_S3D, UNC_LOC_S, kmx > 0)
              namfun = 'velocity magnitude'
           case ('r1')
              unc_loc = merge(UNC_LOC_S3D, UNC_LOC_S, kmx > 0)
              namfun = trim(namcon(gdfourier%fconno(ifou)))
           case ('u1')
              unc_loc = UNC_LOC_U
              namfun = 'velocity'
           case ('qx')
              unc_loc = UNC_LOC_U
              namfun = 'unit discharge'
           case ('ta')
              unc_loc = UNC_LOC_S
              namfun = 'bed stress'
           end select
           all_unc_loc(ifou) = unc_loc
           write(cnumber,'(i3.3)') fouref(ifou,1)
           namfunlong = cnumber // ": " // namfun
           !
           idvar(:,ivar) = imissval
           ierr = unc_def_var_map(fileids%ncid,fileids%id_tsp, idvar(:,ivar), NF90_DOUBLE, unc_loc, trim(fouvarnam(ivar)), trim(fouvarnam(ivar)), &
                          'Fourier analysis ' // namfunlong // ', ' // trim(fouvarnamlong(ivar)), fouvarunit(ivar), 0)
           ierr = unc_put_att(fileids%ncid,idvar(:,ivar), 'long_name','Fourier analysis '// namfunlong // ', ' // trim(fouvarnamlong(ivar)))
           ierr = unc_put_att(fileids%ncid,idvar(:,ivar), 'units',fouvarunit(ivar))
           ierr = unc_put_att(fileids%ncid,idvar(:,ivar), 'Reference_date_in_yyyymmdd', irefdate)
           ierr = unc_put_att(fileids%ncid,idvar(:,ivar), 'Starttime_fourier_analysis_in_minutes_since_reference_date', tfastr)
           ierr = unc_put_att(fileids%ncid,idvar(:,ivar), 'Stoptime_fourier_analysis_in_minutes_since_reference_date', tfasto)

           ierr = unc_add_gridmapping_att(fileids%ncid, idvar(:,ivar), jsferic)
           select case (founam(ifou))
           case('s1','r1','u1','ux','uy','uxa','uya','uc','ta')
              ierr = unc_put_att(fileids%ncid, idvar(:,ivar),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           case('qxk','ws')
              ierr = unc_put_att(fileids%ncid, idvar(:,ivar),  'coordinates'  , 'FlowLink_xu FlowLink_yu')
           end select
           ierr = unc_put_att(fileids%ncid,idvar(:,ivar), 'Number_of_cycles', fnumcy(ifou))
           ierr = unc_put_att(fileids%ncid,idvar(:,ivar), 'Frequency_degrees_per_hour', freqnt)
           !
        enddo
        !

        ! END DEFINITION MODE
        ierr =  nf90_enddef(fileids%ncid)

        ! START DATA MODE
        !
        ! Write requested fourier function output
        !
        do ifou = 1, nofou
           !
           unc_loc = all_unc_loc(ifou)
           !
           call wrfous(ifou, fileids, unc_loc)
        enddo
        !
        ! Close fourier output file
        !
        ierr = nf90_close(fileids%ncid)         ! ncid NOT set to zero, to avoid writing the fourier output file again.
    end subroutine wrfou

!> - writes results of fourier analysis to output
   subroutine wrfous(ifou, fileids, iloc)
   !!--declarations----------------------------------------------------------------
       use precision
       use m_d3ddimens
       use netcdf
       use unstruc_netcdf
       !
       implicit none
   !
   ! Global variables
   !
       integer                     , intent(in) :: ifou   !< Fourier counter
       type(t_unc_mapids)          , intent(in) :: fileids!< Set of file and variable ids for this file.
       integer                     , intent(in) :: iloc
   !
   ! Local variables
   !
       integer      , dimension(:,:)        , pointer :: idvar
       real(kind=fp), dimension(:)          , pointer :: fousma
       real(kind=fp), dimension(:)          , pointer :: fousmb
       real(kind=sp), dimension(:)          , pointer :: fousmas
       real(kind=sp), dimension(:)          , pointer :: fousmbs

       integer                    :: nmaxus
       integer                    :: ierror
       integer                    :: fouvar
       integer                    :: sizeb
       integer                    :: n                    ! Loop counter over NMAXUS

   !
   !! executable statements -------------------------------------------------------
   !
       idvar         => gdfourier%idvar
       fousma        => gdfourier%fousma(ifou)%rdata
       fousmb        => gdfourier%fousmb(ifou)%rdata
       fousmas       => gdfourier%fousma(ifou)%sdata
       fousmbs       => gdfourier%fousmb(ifou)%sdata

       !
       call getsizes(ifou, nmaxus, sizeb)

       !
       ! Write data for user defined dimensions, hence NMAXUS
       !
       fouvar = gdfourier%fouref(ifou,2)
       select case (gdfourier%fouelp(ifou))
       case ('x','i')
       ! First for Maximum or Minimum
          ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:,fouvar), iloc, fousmas)
          if (gdfourier%founam(ifou)=='s1') then
             ! min/max water depth
             ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:,fouvar+1), iloc, fousmbs)
          endif
       case ('e')
          ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:,fouvar), iloc, fousma)
       case ('a')
       ! For average
          if( fousmb(1) > 0d0 ) then
             do n = 1, nmaxus
                fousma(n) = fousma(n) / fousmb(1)
             enddo 
          else
             fousma = defaultd
          endif
          ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:,fouvar), iloc, fousma)
       case default
          ! Fourier
          !
          call fourier_final(ifou, nmaxus)
          ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:,fouvar),   iloc, fousma)
          ierror = unc_put_var_map(fileids%ncid, fileids%id_tsp, idvar(:,fouvar+1), iloc, fousmb)
       end select
   end subroutine wrfous

   !> final update of fousma and fousmb
   subroutine fourier_final(ifou, nmaxus)
       use mathconsts, only : raddeg
       integer      , intent(in) :: ifou     !< Fourier counter
       integer      , intent(in) :: nmaxus   !< dimension of current quantity
       real(kind=fp)             :: hdt      ! Half Integration time step [seconds]
       real(kind=fp)             :: freqnt   ! Frequency in degrees per hour
       real(kind=fp)             :: shift    ! Phase shift
       integer                   :: n        ! loop counter
       logical                   :: ltest    ! Help variable for atan2 function test
       real(kind=fp)             :: amp      ! Fourier amplitude
       real(kind=fp)             :: fas      ! Fourier phase
       real(kind=fp)             :: wdt      ! weight factor for time interval
       real(kind=fp)             :: fas_term ! term to add to phase
       integer                   :: fnumcy
       real(kind=fp)             :: ftmsto
       real(kind=fp)             :: ftmstr
       real(kind=fp)             :: fknfac
       real(kind=fp)             :: foufas
       real(kind=fp), pointer    :: fousma(:)
       real(kind=fp), pointer    :: fousmb(:)
       real(kind=fp)             :: fv0pu
   !
   !! executable statements -------------------------------------------------------
   !
       hdt    =  0.5_fp * dt_user
       fnumcy =  gdfourier%fnumcy(ifou)
       ftmsto =  gdfourier%ftmsto(ifou) / dt_user
       ftmstr =  gdfourier%ftmstr(ifou) / dt_user
       fknfac =  gdfourier%fknfac(ifou)
       foufas =  gdfourier%foufas(ifou)
       fousma => gdfourier%fousma(ifou)%rdata
       fousmb => gdfourier%fousmb(ifou)%rdata
       fv0pu  =  gdfourier%fv0pu(ifou)
       !
       ! Initialize local variables
       !
       ! Frequention := 360 degree / period
       ! where period = [ (FTMSTO - FMTSTR) ] / [ FNUMCY * 3600 ]
       ! FOUFAS is defined in RDFOUR as
       ! FOUFAS =  2 * PI * FNUMCY / [(FTMSTO - FMTSTR) ]
       ! so FREQNT = FOUFAS * RADDEG * 3600 is OK
       !
       shift = ftmstr * foufas
       freqnt = foufas * raddeg * 3600.0_fp
       wdt = 2.0_fp/(ftmsto - ftmstr)
       fas_term = fv0pu - tzone * freqnt
       do n = 1, nmaxus
          ltest = (fousma(n) == 0.0_fp .and. fousmb(n) == 0.0_fp)
          !
          ! Test for active point and non-zero values
          !
          if (.not. ltest) then
             fousma(n) = fousma(n) * wdt
             fousmb(n) = fousmb(n) * wdt
             amp = hypot(fousma(n), fousmb(n))
             fas = atan2(fousmb(n), fousma(n)) + shift
             if (fnumcy==0) then
                amp = 0.5_fp*amp*cos(fas)
                fas = 0.0_fp
             endif
             !
             ! Timezone correction added timezone*phase [degrees/hr].
             ! foufas       is in [rad/timestep]
             ! halftimestep is in [sec/timestep]
             ! => timezonecorr = tzone [-] * foufas [rad] * raddeg [deg/rad] * [sec/hr]
             !
             fas = fas * raddeg + fas_term
             !
             ! To define FAS between 0 and 360. add 720. to the MOD of
             ! FAS and re-use the MOD function
             !
             fousmb(n) = mod(mod(fas, 360.0_fp) + 720.0_fp, 360.0_fp)
             fousma(n) = amp/fknfac
          else
             !
             ! Inactive point (not inside grid, can be open boundary)
             !
             fousma(n) = defaultd
             fousmb(n) = defaultd
          endif
       enddo
   end subroutine fourier_final

end module m_fourier_analysis
