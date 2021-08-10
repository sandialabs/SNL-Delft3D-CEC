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

! $Id: manholes.f90 65778 2020-01-14 14:07:42Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/manholes.f90 $

!> Manholes connect 1D networks with 2D/3D grids or other 1D networks.
!!
!! A manhole has an x,y-location, which determines the unique 2D flow cell
!! that it lies in. A manhole can also be 'pressurized' (closed lid), then
!! no 2D cell is associated, only 1D netlinks can be connected then.
module m_manholes
use properties
use unstruc_messages
implicit none

integer, parameter :: MANHOLE_CLOSED          = 1
integer, parameter :: MANHOLE_RESERVOIR       = 2
integer, parameter :: MANHOLE_OPEN_MOMENTUM   = 3
integer, parameter :: MANHOLE_OPEN_NOMOMENTUM = 4

type manhole_t
    double precision :: x      !< exact x-coordinate
    double precision :: y      !< exact y-coordinate
    character(len=40):: name   !< name of manhole

    integer          :: itype  !< type of manhole (see module parameters)

    ! More attributes...
end type manhole_t

type(manhole_t), allocatable :: manholes(:)  !< Global array of manholes
integer                      :: nummh        !< Current number of manholes in array.

contains
!
!------------------------------------------------------------------------------

subroutine init_manholes()
    nummh = 0
end subroutine init_manholes
!
!------------------------------------------------------------------------------


!> Adds a new manhole at the end of the global manhole list.
subroutine add_manhole(x, y, name, itype)
    double precision, intent(in) :: x, y   !< Location coordinates
    character(len=*), intent(in) :: name   !< Name of manhole
    integer,          intent(in) :: itype  !< Type of manhole (see module parameters)

    integer :: maxmh

    maxmh = size(manholes)
    if (nummh >= maxmh) then
        maxmh = ceiling(1.2*max(1,nummh))
        call realloc_manholes(manholes, maxmh, .true.)
    end if

    nummh = nummh + 1
    manholes(nummh)%x     = x
    manholes(nummh)%y     = y
    manholes(nummh)%name  = name
    manholes(nummh)%itype = itype

end subroutine add_manhole
!
!------------------------------------------------------------------------------


!> Resized an allocatable array of manholes.
subroutine realloc_manholes(mhs, n, keepExisting)
    type(manhole_t), allocatable, intent(inout) :: mhs(:)       !< Array of manholes.
    integer,                      intent(in)    :: n            !< new size
    logical,                      intent(in)    :: keepExisting !< Preserve existing manholes in array or not (set to .false. to save on copy overhead).

    type(manhole_t), allocatable :: mhb(:)
    integer :: nold, ncopy
    logical :: equalSize

    ! 1. Backup if necessary.
    if (allocated(mhs)) then
        nold = ubound(mhs,1)
        equalSize = nold == n
        if (equalSize .and. keepExisting) return ! output=input
        !
        if (keepExisting) then
            ncopy = min(n, nold)
            allocate (mhb(1:ncopy))
            mhb(1:ncopy) = mhs(1:ncopy)
            ! TODO: If manhole_t will contain allocatables/arrays itself, make allocate and copy more sophisticated.
        endif
        if (.not.equalSize) deallocate(mhs)
    endif

    ! Reallocate if necessary.
    if (.not. allocated(mhs)) then
        allocate(mhs(1:n))
    end if

    ! Put back backup if necessary.
    if (allocated(mhb)) then
        mhs(1:ncopy) = mhb(1:ncopy)
        deallocate(mhb)
    endif
end subroutine realloc_manholes
!
!------------------------------------------------------------------------------


!> Deletes all manholes and deallocates global manhole array.
subroutine delete_manholes()
    if (allocated(manholes)) then
        deallocate(manholes)
    end if

    nummh = 0
end subroutine delete_manholes
!
!------------------------------------------------------------------------------


!> Reads manholes from file.
subroutine load_manholes(filename, jadoorladen)
    character(len=*), intent(in) :: filename
    integer,          intent(in) :: jadoorladen !< Append to existing manholespoints or not

    type(tree_data), pointer :: mhs_ptr, mh_ptr
    integer :: istat, i, itype
    logical :: success
    double precision :: x, y

    call tree_create(trim(filename), mhs_ptr)
    call prop_file('ini',trim(filename),mhs_ptr,istat)
    if (istat /= 0) then
        call mess(LEVEL_ERROR, 'Manhole file '''//trim(filename)//''' not read. Code: ', istat)
        return
    else
        call mess(LEVEL_DEBUG, 'Opened manhole file : ', trim(filename) )
    endif


    do i = 1,size(mhs_ptr%child_nodes)
        mh_ptr => mhs_ptr%child_nodes(i)%node_ptr
        if (trim(tree_get_name(mh_ptr)) == 'manhole') then
            call prop_get_double  ( mh_ptr, '*', 'x',      x,     success)
            if (.not. success) then
               cycle
            end if

            call prop_get_double  ( mh_ptr, '*', 'y',      y,     success)
            if (.not. success) then
               cycle
            end if

            call prop_get_integer ( mh_ptr, '*', 'Type',   itype, success)
            if (.not. success) then
               cycle
            end if

        end if
        call add_manhole(x, y, "Manhole", itype)
    end do

end subroutine load_manholes
!
!------------------------------------------------------------------------------


end module m_manholes

subroutine flgsfurufm(formno, m, teken, husb, hdsb, velhght, zs, ds, dg, dc, wstr,&
                  & cwfa, cwd, mugfa, cgfa, cgda, strdamf, lambda)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------

   ! use m_GlobalParameters
   ! use cpluv

   use m_flowgeom, only : dx
   use m_flow
   use m_flowtimes

    implicit none
!
! Local parameters
!
    double precision, parameter    :: relax = 0.0D0, alfa = 0.9D0
!
! Global variables
!
    integer, intent(in)            :: formno
    integer, intent(in)            :: m
    double precision, intent(in)   :: cgda
    double precision, intent(in)   :: cgfa
    double precision, intent(in)   :: cwd
    double precision, intent(in)   :: cwfa
    double precision, intent(in)   :: dc
    double precision, intent(in)   :: dg
    double precision, intent(in)   :: ds
    double precision, intent(in)   :: hdsb
    double precision, intent(in)   :: husb
    double precision, intent(in)   :: mugfa
    double precision, intent(in)   :: strdamf, lambda
    double precision, intent(in)   :: teken
    double precision, intent(in)   :: velhght
    double precision, intent(in)   :: wstr
    double precision, intent(in)   :: zs
!
!
! Local variables
!
    integer                        :: itgenstr
    logical                        :: again
    double precision               :: cu
    double precision               :: dh
    double precision               :: dsqrt
    double precision               :: dxdt
    double precision               :: hs1
    double precision               :: mu
    double precision               :: rhsc
    double precision               :: ustru
    double precision               :: su
    double precision               :: sd

    logical, external              :: iterfurufm
!
!
!! executable statements -------------------------------------------------------
!
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Kuipers
    !
    ! Module:             FLGSFURU (FLow General Structure
    !                               calculate FU and RU)
    !
    ! Module description: The linearization coefficients FU and RU are
    !                     calculated for the general structure
    !
    !                     The stage of the flow was already determined.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    ! 16 cgda              I  Contraction coefficient for drowned gate flow
    !                         (adapted)
    ! 15 cgfa              I  Contraction coefficient for gate flow
    !                         (adapted)
    ! 13 cwd               I  Contraction coefficient for drowned weir flow.
    ! 12 cwfa              I  Contraction coefficient for free weir flow.
    !                         (adapted)
    ! 10 dc                I  Critical water level (free gate flow)
    !  9 dg                I  Gate opening height.
    !  8 ds                I  Water level immediately downstream the gate.
    !  1 formno            I  Flow condition of general structure:
    !                         0 : closed or dry
    !                         1 : free weir flow
    !                         2 : drowned weir flow
    !                         3 : free gate flow
    !                         4 : drowned gate flow
    !  5 hdsb              I  Downstream water level.
    !  4 husb              I  Upstream water level.
    !  2 m                 I  Grid index of structure
    ! 14 mugfa             I  Vertical contraction coefficient for free gate
    !                         flow (adapted)
    !  3 teken             I  Flow direction (+1/-1).
    !  6 velhght           I  Velocity height
    ! 11 wstr              I  Width at centre of structure.
    !  7 zs                I  Bed level at centre of structure.
    !=======================================================================
    !     Include Pluvius data space
    !     Declaration of parameters:
    !     Declaration of local variables:
    !
    if (formno==0) then
                           ! closed or dry
        ! hu(m) = 0d0
        au(m) = 0d0 ; fu(m) = 0d0 ; ru(m) = 0d0
    else
        again = .true.
    endif
    !
    !     Calculate upstream energy level w.r.t sill
    !
    hs1 = husb + velhght - zs

    !
    itgenstr = 0
    dxdt = strdamf*dx(m)/dts
    do while (again)
       itgenstr = itgenstr + 1
       if (formno==1) then
          !           free weir flow
          cu = cwfa**2*ag /1.5D0
          !TEM        WRITE (11,*) cu,cwfa
          au(m) = wstr*hs1*2.0D0/3.0D0
          ustru = cwfa*sqrt(ag*2.0D0/3.0D0*hs1)
          rhsc = cu*(hdsb + velhght - zs)*teken
       elseif (formno==2) then
          !           drowned weir flow
          cu = cwd**2*2.0D0*ag
          au(m) = wstr*ds
          dh = max(hs1 - ds, 0.D0)
          ustru = cwd*dsqrt(ag*2.0D0*dh)
          rhsc = cu*(hdsb + velhght - (ds + zs))*teken
       elseif (formno==3) then
          !           free gate flow
          mu = mugfa*cgfa
          cu = mu**2*2.0D0*ag
          au(m) = wstr*dg
          dh = max(hs1 - dc, 0.D0)
          ustru = mu*dsqrt(ag*2.0D0*dh)
          rhsc = cu*(hdsb + velhght - (dc + zs))*teken
       elseif (formno==4) then
          !           drowned gate flow
          mu = mugfa*cgda
          cu = mu**2*2.0D0*ag
          au(m) = wstr*dg
          dh = max(hs1 - ds, 0.D0)
          ustru = mu*dsqrt(ag*2.0D0*dh)
          rhsc = cu*(hdsb + velhght - (ds + zs))*teken
       else
       endif

       ! dads(m) = wstr

       if (teken>0) then
           su = husb
           sd = hdsb
       else
           sd = husb
           su = hdsb
       endif
       again = iterfurufm(m, su, sd, ustru, cu, rhsc, dxdt, lambda) .and. itgenstr<100
    enddo
    q1(m) = au(m)*u1(m)   !  this may be done without
end subroutine flgsfurufm

logical function iterfurufm(m, su, sd, ustru, cu, rhsc, dxdt, lambda)

   !=======================================================================
   !                       Deltares
   !                One-Two Dimensional Modelling System
   !                           S O B E K
   !
   ! Subsystem:          Flow Module
   !
   ! Programmer:         Guus Stelling
   !
   ! Module:             iterfurufm (ITERFURU)
   !
   ! Module description: coefficients for momentum equation in wet weir point
   !
   !
   !     update information
   !     person                    date
   !
   !
   !
   !     Include Pluvius data space
   !
   ! use m_GlobalParameters
   ! use cpluv
   use m_strucs
   use m_flow
   use m_flowgeom, only : dx

   implicit none
!
! Global variables
!
!
   integer, intent(in)              :: m
   double precision, intent(in)     :: ustru, lambda
   double precision, intent(in)     :: cu
   double precision, intent(in)     :: rhsc
   double precision                 :: su   ! not s(up) but s(k1)
   double precision                 :: sd   ! not s(do) but s(k2), see switch in calling routine
   double precision                 :: dxdt
!
! Local variables
!
!
   double precision, parameter      :: relax = 0d0
   double precision                 :: bu
   double precision                 :: du, Cz
   double precision                 :: u1mi, dxfrL

!
!! executable statements -------------------------------- -----------------------
!
   dxfrL = 0d0
   if (lambda == 0) then        ! if structure defined friction == 0, use standard friction
      if (kmx == 0) then
         dxfrL = dx(m)*cfuhi(m)
      else if (frcu(m) > 0d0 ) then
         call getcz(hu(m), frcu(m), ifrcutp(m), Cz, m)      ! standard Chezy coeff
         dxfrl = dx(m)*ag/(Cz*Cz*hu(m))
      endif
   endif

   bu    = dxdt + (1.0 + relax + dxfrL)*ustru
   du    = (strucalfa*q1(m)/max(au(m),1d-4) + (1-strucalfa)*u0(m))*dxdt + relax*ustru*u1(m) + rhsc
   fu(m) = cu/bu
   ru(m) = du/bu
   u1mi  = u1(m)
   u1(m) = ru(m) + fu(m)*(su - sd)
   if (relax == 0.0) then
      iterfurufm = .false.
   else if (abs(u1mi - u1(m))>1.0D-6) then
      iterfurufm = .true.
   else
      iterfurufm = .false.
   endif
end function iterfurufm

subroutine flgsfm( n, ng, L, firstiter, jarea)
use m_flowgeom
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    ! use cpluv
    ! use m_strucs
    ! use ident

    use m_strucs
    use m_flow

    implicit none
!
! Global variables
!
    integer, intent(in)  :: n          !< general structure point n
    integer, intent(in)  :: ng         !< is a member of general structure sigal ng
    integer, intent(in)  :: L          !< Flow link number, signed! If L < 0 then flow link is in opposite direction than structure left-right orientation.
    logical, intent(in)  :: firstiter
    logical              :: jarea


!
!
! Local variables
!
    integer                        :: il, ir, k1, k2, kL, kR, m, Lf
    integer                        :: L0
    logical                        :: velheight
    double precision               :: cgd
    double precision               :: cgf
    double precision               :: crest
    double precision               :: cwd
    double precision               :: cwf
    double precision               :: dg
    double precision               :: ds
    double precision               :: ds1
    double precision               :: ds2
    double precision               :: hdsb
    double precision               :: husb
    double precision               :: lambda
    double precision               :: mugf
    double precision               :: relax
    double precision               :: rholeft
    double precision               :: rhoright
    double precision               :: strdamf
    double precision               :: teken, tekenstr
    double precision               :: ud
    double precision               :: uu
    double precision               :: w2
    double precision               :: wsd
    double precision               :: wstr
    double precision               :: zb2
    double precision               :: zs, gateloweredgelevel, gatedoorheight
    double precision               :: DsL
    double precision               :: gatefraction, g1, fu_sav, ru_sav, au_sav

!
!! executable statements -------------------------------------------------------
!
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Kuipers
    !
    ! Module:             FLGS (FLow General Structure)
    !
    ! Module description: In subroutine FLGS the QH-relationship for a
    !                     general structure will be transformed to a
    !                     linearized equation
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    !  2 il                I  Grid point on left side of structure (lower
    !                         index).
    !  3 ir                I  Grid point on right side of structure (upper
    !                         index).
    !  8 jarea             I  If True then claculate only area
    !  1 m                 I  Grid index of structure
    !  4 istru             I  Number of structure.
    !  7 firstiter         I  True in case of first iteration step.

    ! Subprogram calls:
    ! NAME    DESCRIPTION
    ! flgtar  FLow get General sTructure ARguments
    ! flupdg  FLow UP/Downstream near General structure
    ! errmsg  generate ERRer MeSsaGe to log file
    ! flqhgs  FLow QH relation for General Structure
    !=======================================================================
    !     Include Pluvius data space
    !     Include identifiers of objects
    !
    !     Declaration of parameters:
    !
    !
    !     Declaration of local variables:
    !
    !
    !
    !TEM  WRITE (11,*) 'Call structure',istru,'(',m,il,ir,istru,')'

    Lf = abs(L)
    ! NOTE: Since a single general structure may be crossed by multiple flow links,
    ! pay attention to proper directions: structure parameters are typically determined
    ! by the structure's left-right direction, whereas upwinding and furu-computations
    ! are typically in the flow link's 1-2 direction.
    k1 = ln(1,Lf) ; k2 = ln(2,Lf)      ! 1 -> 2 flow link direction
    kL = kcgen(1,n) ; kR = kcgen(2,n)  ! L -> R structure direction

    m  = L
    il = k1
    ir = k2
    L0 = n - L1cgensg(ng)+1

    zs                 = min  ( bob(1,Lf), bob(2,Lf) )         ! == zcgen(3*ng - 2) crest/silllevel
    gateloweredgelevel = generalstruc(ng)%gateheightonlink(L0) ! == zcgen(3*ng - 1) under gate door and infinity in open part.
    gatefraction = generalstruc(ng)%gateclosedfractiononlink(L0)

    ! TODO: RTC: AvD/Herman: hier ook wu'tjes en zb1-tjes etc gaan zetten, voordat we de flupd/flgtar-subroutines gaan callen?
    ! Velheight is always true for river structures
    ! velheight = istrtyp(7, istru)==1
    velheight = .true.

    relax = 1.0D0
    au(Lf) = 0d0 ; fu(Lf) = 0d0 ; ru(Lf) = 0d0
    !
    ! ng instead of istru

    dg = gateloweredgelevel - zs

    call flupdofm(m, il, ir, ng, velheight, rholeft, rhoright, crest, husb, hdsb,     &
                  uu, ud, teken, relax)

    gatedoorheight = 0d0

    tekenstr = teken*sign(1, L) ! if flow link abs(L) is in opposite orientation to the structure's orientation, then negate the just computed upwind (flow) teken.

    if (husb > zs) then
       call flgtarfm(ng, L0, wu(Lf), bl(kL), bl(kR), tekenstr, zs, wstr, w2, wsd, zb2, dg, ds1, ds2, cgf, cgd,   &
                     cwf, cwd, mugf, lambda, strdamf, gatedoorheight)

       DsL   = s1(k2) - s1(k1)
       u1(Lf) = Rusav(1,n) - Fusav(1,n)*DsL ; u0(Lf) = u1(Lf) ; q1(Lf) = Ausav(1,n)*u1(Lf)
       call flqhgsfm(Lf, teken, husb, hdsb, uu, zs, wstr, w2, wsd, zb2, ds1, ds2, dg,  &
                     cgf, cgd, cwf, cwd, mugf, lambda, strdamf, jarea, ds)
       fusav(1,n) = fu(Lf) ; Rusav(1,n) = ru(Lf) ; Ausav(1,n) = au(Lf)
    endif

    if (gatedoorheight > 0d0) then  ! now add water overflowing top of gate
       zs = gateloweredgelevel + gatedoorheight
       if (husb > zs) then          ! husb = upwind waterlevel instead of height
          dg    = 1d9               ! sky is the limit, this gate fully open
          u1(Lf) = rusav(2,n) - Fusav(2,n)*dsL ; u0(Lf) = u1(Lf) ; q1(Lf) = Ausav(2,n)*u1(Lf)
          call flgtarfm(ng, L0, wu(Lf), bl(kL), bl(kR), tekenstr, zs, wstr, w2, wsd, zb2, dg, ds1, ds2, cgf, cgd,   &
                        cwf, cwd, mugf, lambda, strdamf, gatedoorheight)
          call flqhgsfm(Lf, teken, husb, hdsb, uu, zs, wstr, w2, wsd, zb2, ds1, ds2, dg,  &
                        cgf, cgd, cwf, cwd, mugf, lambda, strdamf, jarea, ds)
          fusav(2,n) = fu(Lf) ; rusav(2,n) = ru(Lf) ; ausav(2,n) = au(Lf)

          au(Lf) =  ausav(1,n)            +            ausav(2,n)
          if (au(Lf) >0d0) then
             fu(Lf) = (fusav(1,n)*ausav(1,n) + fusav(2,n)*ausav(2,n) ) / au(Lf)
             ru(Lf) = (rusav(1,n)*ausav(1,n) + rusav(2,n)*ausav(2,n) ) / au(Lf)
          end if
       else
          fusav(2,n) = 0d0 ; rusav(2,n) = 0d0 ; ausav(2,n) = 0d0
       endif
    endif

    if ( gatefraction.lt.1d0 .and. gatefraction.gt.0d0 ) then
       fu_sav = fu(Lf)
       ru_sav = ru(Lf)
       au_sav = au(Lf)

       zs =  min  ( bob(1,Lf), bob(2,Lf) )
       dg = huge(1d0)
       u1(Lf) = rusav(3,n) - Fusav(3,n)*dsL ; u0(Lf) = u1(Lf) ; q1(Lf) = Ausav(3,n)*u1(Lf)
       call flgtarfm(ng, L0, wu(Lf), bl(kL), bl(kR), tekenstr, zs, wstr, w2, wsd, zb2, dg, ds1, ds2, cgf, cgd,   &
                     cwf, cwd, mugf, lambda, strdamf, gatedoorheight)
       call flqhgsfm(Lf, teken, husb, hdsb, uu, zs, wstr, w2, wsd, zb2, ds1, ds2, dg,  &
                     cgf, cgd, cwf, cwd, mugf, lambda, strdamf, jarea, ds)
       fusav(3,n) = fu(Lf) ; rusav(3,n) = ru(Lf) ; ausav(3,n) = au(Lf)

       g1 = 1d0-gatefraction

       fu(Lf) = gatefraction * fu_sav + g1 * fu(Lf)
       ru(Lf) = gatefraction * ru_sav + g1 * ru(Lf)
       au(Lf) = gatefraction * au_sav + g1 * au(Lf)
    end if

    if (au(Lf) == 0d0) then
        hu(Lf) =  0d0
    endif

    ! TEMP = laatste statement
    ! strhis(15, istru) = ds + crest     ! waterlevel on crest
end subroutine flgsfm

subroutine flgsareafm(formno, m, husb, velhght, zs, ds, dg, wstr)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    ! use cpluv
    use m_flow, only: au

    implicit none
!
! Global variables
!
    integer, intent(in)            :: formno
    integer, intent(in)            :: m
    double precision, intent(in)   :: dg
    double precision, intent(in)   :: ds
    double precision, intent(in)   :: husb
    double precision, intent(in)   :: velhght
    double precision, intent(in)   :: wstr
    double precision, intent(in)   :: zs
!
!
! Local variables
!
    double precision               :: hs1
!
!
!! executable statements -------------------------------------------------------
!
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Kuipers
    !
    ! Module:             FLGSAREA (FLow General Structure
    !                               calculate AREA thru structure)
    !
    ! Module description: The area through the general structure will
    !                     be deermined.
    !
    !                     The stage of the flow was already determined.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    !  7 dg                I  Gate opening height.
    !  6 ds                I  Water level immediately downstream the gate.
    !  1 formno            I  Flow condition of general structure:
    !                         0 : closed or dry
    !                         1 : free weir flow
    !                         2 : drowned weir flow
    !                         3 : free gate flow
    !                         4 : drowned gate flow
    !  3 husb              I  Upstream water level.
    !  2 m                 I  Grid index of structure
    !  4 velhght           I  Velocity height
    !  8 wstr              I  Width at centre of structure.
    !  5 zs                I  Bed level at centre of structure.
    !=======================================================================
    !     Include Pluvius data space
    !     Declaration of parameters:
    !     Declaration of local variables:
    !
    if (formno==0) then
       !        closed or dry
       !  au(m) = 0.0
       ! kfu(m) = 0
    else
       !
       !        Calculate upstream energy level w.r.t sill
       !
       hs1 = husb + velhght - zs
       ! kfu(m) = 1
       !
       if (formno==1) then
          !           free weir flow
          au(m) = wstr*hs1*2.0D0/3.0D0
       elseif (formno==2) then
          !           drowned weir flow
          au(m) = wstr*ds
       elseif (formno==3) then
          !           free gate flow
          au(m) = wstr*dg
       elseif (formno==4) then
          !           drowned gate flow
          au(m) = wstr*dg
       else
       endif
    endif
end subroutine flgsareafm

subroutine flgsd2fm(wsd, wstr, zs, w2, zb2, dg, ds1, ds2, elu, hd, rhoast,    &
                & cgd, imag, ds, lambda)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local parameters
!
    double precision, parameter :: c23 = 2.0D0/3.0D0, c13 = 1.0D0/3.0D0
!
! Global variables
!
    logical, intent(out)           :: imag
    double precision, intent(in)   :: cgd
    double precision, intent(in)   :: dg
    double precision, intent(out)  :: ds
    double precision, intent(in)   :: ds1
    double precision, intent(in)   :: ds2
    double precision, intent(in)   :: elu
    double precision, intent(in)   :: hd
    double precision, intent(in)   :: lambda
    double precision, intent(in)   :: rhoast
    double precision, intent(in)   :: w2
    double precision, intent(in)   :: wsd
    double precision, intent(in)   :: wstr
    double precision, intent(in)   :: zb2
    double precision, intent(in)   :: zs
!
!
! Local variables
!
    double precision               :: ag
    double precision               :: bg
    double precision               :: cg
    double precision               :: d2
    double precision               :: det
    double precision               :: hsl
    double precision               :: terma
    double precision               :: termb
!
!
!! executable statements -------------------------------------------------------
!
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Brouwer/J.Kuipers
    !
    ! Module:             FLGSD2 (FLow Gen. Struct. Depth sill 2nd ord. eq.)
    !
    ! Module description: Compute water depth ds at the sill by a second
    !                     order algebraic equation.
    !
    !                     In case of drowned gate flow the water level at
    !                     the sill is required. The water depth is calcu-
    !                     lated in this routine.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    ! 12 cgd               I  Correction coefficient for drowned gate flow.
    !  6 dg                I  Gate opening height.
    !  7 ds1               I  Delta s1 general structure.
    !  8 ds2               I  Delta s2 general structure.
    ! 14 ds                IO Water level immediately downstream the gate.
    !  9 elu               I  Upstream energy level.
    ! 10 hd                I  Downstream water level.
    ! 13 imag              O  Logical indicator, = TRUE when determinant of
    !                         second order algebraic equation less than
    !                         zero.
    ! 15 lambda            I  Extra resistance in general structure.
    ! 11 rhoast            I  Downstream water density divided by upstream
    !                         water density.
    !  4 w2                I  Width at right side of structure.
    !  1 wsd               I  Width structure right or left side.
    !  2 wstr              I  Width at centre of structure.
    !  5 zb2               I  Bed level at right side of structure.
    !  3 zs                I  Bed level at centre of structure.
    !=======================================================================
    !
    !     Declaration of parameters:
    !
    !
    !     Declaration of local variables:
    !
    !
    !JK   LOGICAL uitput
    !JK   COMMON /UITPUT/uitput
    !
    !     Calculate Ag, Bg and Cg according to appendix C of
    !     the design document River Rural integratietraject deel 3.
    !JK   WRITE  (11,*)  'IN FLGSD2 ----'
    !
    ag = (1.0D0 - rhoast)*(w2/12.0D0 + wsd/4.0D0) + 0.5D0*(rhoast + 1.0D0)      &
       & *(c13*w2 + c23*wsd)
    d2 = hd - zb2
    !
    terma = (4.0D0*rhoast*cgd*cgd*dg*dg*wstr*wstr)/(w2*d2)*(1.0D0 + lambda/d2)
    termb = 4.0D0*cgd*dg*wstr
    !
    bg = (1.0D0 - rhoast)*((d2 + ds1)*(w2 + wsd)/6.D0 + ds1*wsd*c13)            &
       & + 0.5D0*(rhoast + 1.0D0)                                               &
       & *((ds1 + ds2 - d2)*(c13*w2 + c23*wsd) + (c23*d2 + c13*ds1)             &
       & *w2 + (c13*d2 + c23*ds1)*wsd) + terma - termb
    !
    hsl = elu - zs
    !
    cg = (1.0D0 - rhoast)*((d2 + ds1)**2*(w2 + wsd)/12.D0 + ds1**2*wsd/6.0D0)   &
       & + 0.5D0*(rhoast + 1.0D0)*(ds1 + ds2 - d2)                              &
       & *((c23*d2 + c13*ds1)*w2 + (c13*d2 + c23*ds1)*wsd) - terma*hsl +        &
       & termb*hsl
    !
    det = bg*bg - 4.0D0*ag*cg
    if (det<0.0D0) then
       imag = .true.
    !JK      WRITE (11,*) 'Det=',det
    else
       imag = .false.
       ds = ( - bg + sqrt(det))/(2.0D0*ag)
    endif
end subroutine flgsd2fm

subroutine flgsd3fm(wsd, wstr, zs, w2, zb2, ds1, ds2, elu, hd, rhoast, cwd,   &
                & ds, lambda)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local parameters
!
    double precision, parameter :: c23 = 2.0D0/3.0D0, c13 = 1.0D0/3.0D0
!
! Global variables
!
    double precision, intent(in)   :: cwd
    double precision, intent(out)  :: ds
    double precision, intent(in)   :: ds1
    double precision, intent(in)   :: ds2
    double precision, intent(in)   :: elu
    double precision, intent(in)   :: hd
    double precision, intent(in)   :: lambda
    double precision, intent(in)   :: rhoast
    double precision, intent(in)   :: w2
    double precision, intent(in)   :: wsd
    double precision, intent(in)   :: wstr
    double precision, intent(in)   :: zb2
    double precision, intent(in)   :: zs
!
!
! Local variables
!
    double precision               :: aw
    double precision               :: bw
    double precision               :: cw
    double precision               :: d2
    double precision               :: fac
    double precision               :: h2a
    double precision               :: h2b
    double precision               :: h2c
    double precision               :: hsl
    double precision               :: hulp
    double precision               :: hulp1
    double precision               :: p
    double precision               :: phi
    double precision               :: q
    double precision               :: r60
    double precision               :: term
    double precision               :: u
    double precision               :: v
!
!
!! executable statements -------------------------------------------------------
!
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Brouwer/J.Kuipers
    !
    ! Module:             FLGSD3 (FLow Gen. Struct. Depth sill 3rd ord. eq.)
    !
    ! Module description: Compute water depth ds at the sill by solving a
    !                     third order algebraic equation.
    !
    !                     In case of drowned weir flow the water level at
    !                     the sill is required. The water depth is calcu-
    !                     lated in this routine.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    ! 11 cwd               I  Correction coefficient for drowned weir flow.
    !  6 ds1               I  Delta s1 general structure.
    !  7 ds2               I  Delta s2 general structure.
    ! 12 ds                IO Water level immediately downstream the gate.
    !  8 elu               I  Upstream energy level.
    !  9 hd                I  Downstream water level.
    ! 13 lambda            I  Extra resistance in general structure.
    ! 10 rhoast            I  Downstream water density divided by upstream
    !                         water density.
    !  4 w2                I  Width at right side of structure.
    !  1 wsd               I  Width structure right or left side.
    !  2 wstr              I  Width at centre of structure.
    !  5 zb2               I  Bed level at right side of structure.
    !  3 zs                I  Bed level at centre of structure.
    !=======================================================================
    !
    !     Declaration of parameters:
    !
    !
    !     Declaration of local variables:
    !
    !
    !JK   LOGICAL uitput
    !JK   COMMON /UITPUT/uitput
    !
    !     Calculate Dw (=term), Aw, Bw and Cw according to appendix C of
    !     the design document River Rural integratietraject deel 3.
    !
    !JK   WRITE (11,*) 'FLGSD3'
    !JK   WRITE (11,*)      'wsd,wstr,zs ,w2 ,zb2,ds1 ,ds2 ,elu ,hd'    ,
    !JK  +                   wsd,wstr,zs ,w2 ,zb2,ds1 ,ds2 ,elu ,hd
    d2  = hd - zb2
    hsl = elu - zs
    !JK   WRITE (11,*)  'hsl',hsl
    term = ((4.0D0*cwd*cwd*rhoast*wstr*wstr)/(w2*d2))*(1.0D0 + lambda/d2)
    !
    aw = ( - term*hsl - 4.0D0*cwd*wstr + (1.0D0 - rhoast)                       &
       & *(w2/12.0D0 + wsd/4.0D0) + 0.5D0*(rhoast + 1.0D0)*(c13*w2 + c23*wsd))  &
       & /term
    !
    bw = (4.0D0*cwd*wstr*hsl + (1.0D0 - rhoast)                                 &
       & *((d2 + ds1)*(w2 + wsd)/6.D0 + ds1*wsd*c13) + 0.5D0*(rhoast + 1.0D0)   &
       & *((ds1 + ds2 - d2)*(c13*w2 + c23*wsd) + (c23*d2 + c13*ds1)             &
       & *w2 + (c13*d2 + c23*ds1)*wsd))/term
    !
    cw = ((1.0D0 - rhoast)*((d2 + ds1)**2*(w2 + wsd)/12.D0 + ds1**2*wsd/6.0D0)  &
       & + 0.5D0*(rhoast + 1.0D0)*(ds1 + ds2 - d2)                              &
       & *((c23*d2 + c13*ds1)*w2 + (c13*d2 + c23*ds1)*wsd))/term
    !
    !     Solve the equation ds**3 + aw*ds**2 + bw*ds +cw to get the water
    !     level at the sill
    !
    p = bw/3.0D0 - aw*aw/9.0D0
    q = aw*aw*aw/27.0D0 - aw*bw/6.0D0 + cw/2.0D0
    hulp = q*q + p*p*p
    !
    if (hulp<0.0D0) then
       p = abs(p)
       phi = acos(abs(q)/p/sqrt(p))/3.0D0
       r60 = acos(0.5D0)
       fac = sign(2.D0, q)*sqrt(p)
       h2a = -fac*cos(phi)
       h2b = fac*cos(r60 - phi)
       h2c = fac*cos(r60 + phi)
       ds = max(h2a, h2b, h2c) - aw/3.0D0
    else
       hulp = sqrt(hulp)
       hulp1 = -q + hulp
       if (abs(hulp1)<1E-6) then
          u = 0 ; v = 0
       else       ! hk: ook fix for Erwin, ARS 15132
          u = abs(hulp1)**c13*sign(1.0D0, hulp1)
          hulp1 = -q - hulp
          v = abs(hulp1)**c13*sign(1.0D0, hulp1)
       endif
       ds = u + v - aw/3.0D0
    endif
end subroutine flgsd3fm

subroutine flgtarfm(ng, L0, wuL, bl1, bl2, teken, zs, wstr, w2, wsd, zb2, dg, ds1, ds2, cgf,  &   ! fromgeneral
                    cgd, cwf, cwd, mugf, lambda, strdamf, gatedoorheight)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    ! use cpluv
    use m_strucs
    use m_missing
    implicit none
!
! Global variables
!
    integer                        :: ng
    integer,          intent(in)   :: L0 !< counter for the current flow link under genstru #ng (1:ncgen for each separate genstru)
    double precision, intent(in)   :: wuL !< wu of this flow link.
    double precision, intent(in)   :: bl1 !< bl of nod1
    double precision, intent(in)   :: bl2 !< bl of nod2
    double precision, intent(out)  :: cgd
    double precision, intent(out)  :: cgf
    double precision, intent(out)  :: cwd
    double precision, intent(out)  :: cwf
    double precision               :: dg
    double precision, intent(out)  :: ds1
    double precision, intent(out)  :: ds2
    double precision               :: lambda
    double precision, intent(out)  :: mugf
    double precision               :: strdamf
    double precision, intent(in)   :: teken !< Flow direction, w.r.t. the structure's orientation. So: based on both upwind *and* flow-link<-->str-pli crossing.
    double precision               :: w2
    double precision, intent(out)  :: wsd
    double precision, intent(out)  :: wstr
    double precision, intent(out)  :: gatedoorheight
    double precision               :: zb2
    double precision               :: zs
!
!
! Local variables
!
    double precision               :: help
    double precision               :: w1
    double precision               :: wsdl
    double precision               :: wsdr
    double precision               :: zb1
    double precision               :: zbsl
    double precision               :: zbsr
!
!
!! executable statements -------------------------------------------------------
!
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Brouwer
    !
    ! Module:             FLGTAR (FLow get General sTructure ARguments)
    !
    ! Module description: Parameters for the general structure are extracted
    !                     from the structures module.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    ! 13 cgd               O  Correction coefficient for drowned gate flow.
    ! 12 cgf               O  Correction coefficient for free gate flow.
    ! 15 cwd               O  Correction coefficient for drowned weir flow.
    ! 14 cwf               O  Correction coefficient for free weir flow.
    !  9 dg                O  Gate opening height.
    ! 10 ds1               O  Delta s1 general structure.
    ! 11 ds2               O  Delta s2 general structure.
    !  1 istru             I  Number of structure.
    ! 17 lambda            O  Extra resistance
    ! 16 mugf              O  Contraction coefficient for free gate flow.
    !  3 tekenstr          I  Flow direction, w.r.t. structure orientation (+/-).
    !  6 w2                O  Width at right side of structure.
    !  7 wsd               O  Width structure right or left side.
    !  5 wstr              O  Width at centre of structure.
    !  8 zb2               O  Bed level at right side of structure.
    !  4 zs                O  Bed level at centre of structure.
    !=======================================================================
    !     Include Pluvius data space
    !
    !     Declaration of parameters:
    !
    !
    !     Declaration of local variables:
    !
    !
    !     Fetch parameters from structure info array
    !

    if (generalstruc(ng)%numlinks <= 1) then ! Structure crosses just one link, use user-specified widths ! TODO: AvD: can this be merged with else block, also incase of timeseries and RTC (why only do that for numlinks>1 ?)
       w1   = min(wuL, generalstruc(ng)%widthleftW1)
       wsdl = min(wuL, generalstruc(ng)%widthleftWsdl)
!    wstr = generalstruc(ng)%widthcenter
       wstr = min(wuL, generalstruc(ng)%widthcenteronlink(L0)) ! Possible realtime-controlled, even when crossing one link, so use widthcenteronlink here already.
       wsdr = min(wuL, generalstruc(ng)%widthrightWsdr)
       w2   = min(wuL, generalstruc(ng)%widthrightW2)
    else                                     ! Structure crosses more than one link: nonsensible to use single width left/right etc. same for all links. Use center linkwidth instead (i.e., typically wu(Lf))
       ! TODO: UNST-695: Support sideways movement/closing of a gate.
       w1   = min(wuL, generalstruc(ng)%widthcenteronlink(L0)) !widthleftW1
       wsdl = min(wuL, generalstruc(ng)%widthcenteronlink(L0)) !widthleftWsdl
       ! wstr = generalstruc(ng)%widthcenter
       wstr = min(wuL, generalstruc(ng)%widthcenteronlink(L0))
       wsdr = min(wuL, generalstruc(ng)%widthcenteronlink(L0)) !widthrightWsdr
       w2   = min(wuL, generalstruc(ng)%widthcenteronlink(L0)) !widthrightW2
    end if

    ! zs   = generalstruc(ng)%levelcenter        ! comes from ec
    zb1  = max(bl1, generalstruc(ng)%levelleftZb1)
    zbsl = max(bl1, generalstruc(ng)%levelleftZbsl)
    zbsr = max(bl2, generalstruc(ng)%levelrightZbsr)
    zb2  = max(bl2, generalstruc(ng)%levelrightZb2)
    ! dg   = generalstruc(ng)%gateheight - zs    ! also comes from ec
    lambda = generalstruc(ng)%extraresistance
    strdamf = generalstruc(ng)%dynstructext
    gatedoorheight = generalstruc(ng)%gatedoorheight

    !if (strdamf< - 0.5D0) strdamf = dynstructext
    !if (lambda < - 0.5D0) lambda = extra_resist_genstruc

    !
    !     Determine cgf, cgd, cwf, cwd, mugf
    !     (flow direction dependent)
    !
    if (teken>0.0D0) then
       cgf = generalstruc(ng)%pos_freegateflowcoeff
       cgd = generalstruc(ng)%pos_drowngateflowcoeff
       cwf = generalstruc(ng)%pos_freeweirflowcoeff
       cwd = generalstruc(ng)%pos_drownweirflowcoeff
       mugf = generalstruc(ng)%pos_contrcoeffreegate
    else
       cgf = generalstruc(ng)%neg_freegateflowcoeff
       cgd = generalstruc(ng)%neg_drowngateflowcoeff
       cwf = generalstruc(ng)%neg_freeweirflowcoeff
       cwd = generalstruc(ng)%neg_drownweirflowcoeff
       mugf = generalstruc(ng)%neg_contrcoeffreegate
    endif
    !
    !     Determine flow direction dependent parameters
    !
    if (teken>0.0D0) then
       wsd = wsdr
       ds1 = zs - zbsr
       ds2 = zbsr - zb2
    else
       wsd = wsdl
       ds1 = zs - zbsl
       ds2 = zbsl - zb1
       help = w1
       w1 = w2
       w2 = help
       help = zb1
       zb1 = zb2
       zb2 = help
    endif
end subroutine flgtarfm

subroutine togeneral(ng, hulp, ngen, widths)
use m_strucs
use m_alloc
implicit none
integer,          intent(in) :: ng        !< Index of this general structure in the generalstruc(:) array
double precision, intent(in) :: hulp(26)  !< genstru params read from file
integer,          intent(in) :: ngen      !< Number of flow links crossed by this single general structure
double precision, intent(in) :: widths(ngen) !< wu(L) values for all links crossed by this single general structure

generalstruc(ng)%widthleftW1             = hulp( 1)    !< this and following: see Sobek manual
generalstruc(ng)%levelleftZb1            = hulp( 2)
generalstruc(ng)%widthleftWsdl           = hulp( 3)
generalstruc(ng)%levelleftZbsl           = hulp( 4)
generalstruc(ng)%widthcenter             = hulp( 5)
generalstruc(ng)%levelcenter             = hulp( 6)
generalstruc(ng)%widthrightWsdr          = hulp( 7)
generalstruc(ng)%levelrightZbsr          = hulp( 8)
generalstruc(ng)%widthrightW2            = hulp( 9)
generalstruc(ng)%levelrightZb2           = hulp(10)
generalstruc(ng)%gateheight              = hulp(11)
generalstruc(ng)%gateheightintervalcntrl = hulp(12)
generalstruc(ng)%pos_freegateflowcoeff   = hulp(13)
generalstruc(ng)%pos_drowngateflowcoeff  = hulp(14)
generalstruc(ng)%pos_freeweirflowcoeff   = hulp(15)
generalstruc(ng)%pos_drownweirflowcoeff  = hulp(16)
generalstruc(ng)%pos_contrcoeffreegate   = hulp(17)
generalstruc(ng)%neg_freegateflowcoeff   = hulp(18)
generalstruc(ng)%neg_drowngateflowcoeff  = hulp(19)
generalstruc(ng)%neg_freeweirflowcoeff   = hulp(20)
generalstruc(ng)%neg_drownweirflowcoeff  = hulp(21)
generalstruc(ng)%neg_contrcoeffreegate   = hulp(22)
generalstruc(ng)%extraresistance         = hulp(23)   ! lambda = L*g/ (C*C)
generalstruc(ng)%dynstructext            = hulp(24)
if (hulp(25) > 0d0) then
   generalstruc(ng)%gatedoorheight           = hulp(25)
endif
generalstruc(ng)%dooropeningwidth        = hulp(26)
generalstruc(ng)%stabilitycounter        = 0d0

call realloc(generalstruc(ng)%widthcenteronlink, ngen)
generalstruc(ng)%widthcenteronlink(1:ngen) = widths(1:ngen)
call realloc(generalstruc(ng)%gateheightonlink, ngen)
generalstruc(ng)%gateheightonlink(1:ngen) = generalstruc(ng)%gateheight
generalstruc(ng)%numlinks                = ngen

call realloc(generalstruc(ng)%gateclosedfractiononlink, ngen, fill=0d0)
end subroutine togeneral



!> Determines flow link' upwind/downwind parameters based on current velocities and water levels.
!! NOTE that this is purely for this flow link, independent of left-right orientation of the structure itself.
!! (Motivation: a single structure in 2D may be crossed by multiple flow links, with varying 1->2 orientation.)
subroutine flupdofm(m, il, ir, istru, velheight, rholeft, rhoright, crest, &
                    husb, hdsb, uu, ud, teken, relax)

use m_strucs
use m_flowgeom
use m_flow

implicit none
!
! Global variables
!
integer, intent(in)            :: m !< Flow link number, signed! If m < 0 then flow link is in opposite direction than structure left-right orientation.
integer, intent(in)            :: il,ir,istru
logical, intent(in)            :: velheight
double precision, intent(in)   :: crest, relax

double precision               :: hdsb
double precision               :: husb
double precision               :: rholeft
double precision               :: rhoright
double precision               :: teken
double precision               :: ud
double precision               :: uu

double precision               :: tem
!double precision               :: ucxku, ucyku
integer                        :: L, k, LL,iflip


! Parameters:
! NR NAME              IO DESCRIPTION
!  7 crest             I  crest.
! 10 hd                O  Downstream water level at t=(n+1).
!  9 husb                O  Upstream water level at t=(n+1).
!  2 il                I  Grid point on left side of structure (lower
!                         index).
!  3 ir                I  Grid point on right side of structure (upper
!                         index).
!  4 istru             I  structure number
!  1 m                 I  Index of velocity point, signed! If m < 0 then flow link is in opposite direction than structure left-right orientation.
!  5 rholeft           O  Density of diluted water on left grid point.
!  6 rhoright          O  Density of diluted water on left grid point.
! 13 teken             O  Flow direction (+1/-1).
! 12 ud                O  Downstream velocity.
! 11 uu                O  Upstream velocity.
!  8 velheight         I  True if velicity height will be taken into account

L    = abs(m)
iflip = max(0, sign(1,m)) ! iflip: 0 if flow link has same orientation as structure's left-right, 1 if opposite (because then the 'left' str point == ln(2,Lf))
if (relax .ne. 1d0) then
   husb = s1(il)*relax + (1.D0 - relax)*strhis2(9+iflip,istru) ! TODO: HK: strhis2 is not yet filled anywhere (no relaxation possible)
   hdsb = s1(ir)*relax + (1.D0 - relax)*strhis2(10-iflip,istru)
else
   husb = s1(il)
   hdsb = s1(ir)
endif
uu   = 0.D0
ud   = 0.D0
if (velheight) then
   uu = 0d0 ; ud = 0d0
   do k = 1,nd(il)%lnx
      LL = iabs( nd(il)%ln(k) )
      if (iadv(LL) .ne. 22) then  ! any non-structure point
         uu = max(uu, abs(u1(LL)) )
      endif
   enddo

   do k = 1,nd(ir)%lnx
      LL = iabs( nd(ir)%ln(k) )
      if (iadv(LL) .ne. 22) then  ! any non-structure point
         ud = max(ud, abs(u1(LL)) )
      endif
   enddo

   !uu = call getucxucynoweirs(il, ucxku, ucyku, 6)
   !uu = csu(L)*ucxku + snu(L)*ucyku
   !call getucxucynoweirs(ir, ucxku, ucyku, 6)
   !ud = csu(L)*ucxku + snu(L)*ucyku
endif

if (u1(L) > 0d0) then
   teken =  1d0
else if (u1(L) < 0d0) then
   teken = -1d0
else if (s1(iL) > s1(ir) ) then
   teken = 1d0
elseif (s1(iL) < s1(ir) ) then
   teken = -1d0
else ! s1(iL) == s1(ir)
   teken = -dble(sign(1,m)) ! account for orientation of flow link w.r.t. structure
endif

if (teken < 0) then
   tem   = hdsb ; hdsb = husb ; husb = tem
   tem   = ud   ; ud   = uu   ; uu   = tem
endif

end subroutine flupdofm

subroutine flqhgsfm(m, teken, husb, hdsb, uu, zs, wstr, w2, wsd, zb2, ds1, ds2,   &
                    dg, cgf, cgd, cwf, cwd, mugf, lambda, strdamf, jarea, ds)
    use m_flow, only : au, fu, ru

    use m_physcoef, only : ag

    implicit none
!
! Global variables
!
    integer         :: m
    logical, intent(in)            :: jarea
    double precision :: cgd
    double precision :: cgf
    double precision :: cwd
    double precision, intent(in)   :: cwf
    double precision :: dg
    double precision :: ds
    double precision :: ds1
    double precision :: ds2
    double precision :: hdsb
    double precision :: husb
    double precision :: lambda
    double precision :: mugf
    double precision :: rhoast=1d0
    double precision :: strdamf
    double precision :: teken
    double precision, intent(in)   :: uu
    double precision :: w2
    double precision :: wsd
    double precision :: wstr
    double precision :: zb2
    double precision :: zs
!
!
! Local variables
!
    integer                        :: formno
    logical                        :: dpsequfm
    logical                        :: imag
    double precision               :: cgd2
    double precision               :: cgda
    double precision               :: cgfa
    double precision               :: cwfa
    double precision               :: dc
    double precision               :: dlim
    double precision               :: elu
    double precision               :: hd1
    double precision               :: hs1
    double precision               :: mugfa
    double precision               :: velhght, tr
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Kuipers
    !
    ! Module:             FLQHGS (FLow QH relation for General Structure)
    !
    ! Module description: The QH-relationship for a general structure
    !                     will be transformed to a linearized equation
    !
    !                     In this subroutine for given upstream and down-
    !                     stream water levels and upstream velocity
    !                     the flow condition will be determined.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    ! 16 cgd               I  Contraction coefficient for drowned gate flow
    ! 15 cgf               I  Contraction coefficient for gate flow
    ! 18 cwd               I  Contraction coefficient for drowned weir flow.
    ! 17 cwf               I  Contraction coefficient for free weir flow.
    ! 13 dg                I  Gate opening height.
    ! 22 ds                O  Water level immediately downstream the gate.
    ! 11 ds1               I  Delta s1 general structure.
    ! 12 ds2               I  Delta s2 general structure.
    !  4 hdsb              I  Downstream water level.
    !  3 husb              I  Upstream water level.
    ! 21 jarea             I  If True then claculate only area
    !  1 m                 I  Grid index of structure
    ! 20 lambda            I  Extra resistance
    ! 19 mugf              I  Vertical contraction coefficient for free
    !                         gate flow.
    ! 14 rhoast            I  Ratio of density right and left of structure
    !  2 teken             I  Flow direction (+1/-1).
    !  5 uu                I  Upstream velocity.
    !  8 w2                I  Width at right side of structure.
    !  9 wsd               I  Width structure right or left side.
    !  7 wstr              I  Width at centre of structure.
    ! 10 zb2               I  Bed level at right side of structure.
    !  6 zs                I  Bed level at centre of structure.
    !
    ! Subprogram calls:
    ! NAME     DESCRIPTION
    ! flccgs   FLow contraction coefficients for general structure
    ! flgsd2   FLow general structure depth sill 2nd order equation
    ! flgsd3   FLow general structure depth sill 3rd order equation
    ! flgsfuru FLow general structure calculate FU and RU
    !=======================================================================
    !     Include Pluvius data space
    !     Declaration of parameters:
    !     Declaration of local variables:
    !     Function declaration:
    !
    !     Compute upstream velocity height and energy level
    !



    velhght = uu*uu/(2.0D0*ag)
    elu = husb + velhght
    hs1 = elu - zs
    !
    tr  = 1d-4
    if (hs1 < tr .or. wstr < tr .or. dg < tr .or. min(cgf, cgd, cwf, cwd) <= 0.) then   !  & dg<.0001) then !hk: or gate closed

       formno = 0 ; return

    else
       !
       !        Compute critical water depth at the
       !        sill, dc and water depth at the sill,ds
       !
       dlim = hs1*(wstr/w2*2./3.*sqrt(2./3.))**(2.0/3.0)
       hd1 = max(hdsb, zb2 + dlim*0.9D0)
       !
       dc = 2.0D0/3.0D0*hs1
       !
       !        Calculate ds by solving third order algebraic equation
       !


       call flgsd3fm(wsd, wstr, zs, w2, zb2, ds1, ds2, elu, hd1, rhoast, cwd, ds, &
                 & lambda)

       !
       if (ds>=dc) then    ! waterheight on crest larger than critical height on crest
          if (dg>=ds) then
             !
             !              - drowned weir -
             !
             formno = 2
          else
             !
             !              - gate flow -
             !
             formno = 3
             !
             !              adapt coefficients on basis of Ds & Cwd
             !
             call flccgsfm(dg, ds, cgd, cgf, cwd, mugf, cgda, cgfa, mugfa)
          endif
       else
          !
          !           Adapt Cwf coefficient
          !
          if (cwf<cwd) then
             if (dpsequfm(dc, 0.0D0, 1.0D-20)) then
                cwfa = cwf
             else
                cwfa = max(ds/dc*cwd, cwf)
             endif
          elseif (ds>0.0D0) then
             cwfa = min(dc/ds*cwd, cwf)
          else
             cwfa = cwf
          endif
          !
          if (dg>=dc) then
             !
             !              - free weir -
             !
             formno = 1
             ds = dc
          else
             !
             !              - gate flow -
             !
             formno = 3
             !
             !              adapt coefficients on basis of Dc & Cwf
             !
             call flccgsfm(dg, dc, cgd, cgf, cwfa, mugf, cgda, cgfa, mugfa)
          endif
       endif
       !
       !        In case of gate flow determine type of gate flow
       !        (drowned or free)
       !
       if (formno==3) then
          dc = mugfa*dg
          !
          !      Cgd for second order equation = Cgd' * Mu'
          !
          cgd2 = cgda*mugfa
          !
          call flgsd2fm(wsd, wstr, zs, w2, zb2, dg, ds1, ds2, elu, hd1, rhoast,   &
                    & cgd2, imag, ds, lambda)
          !
          if (imag) then
             !
             !              - free gate -
             !
             formno = 3
             ds = dc
          elseif (ds<=dc) then
             !
             !              - free gate -
             !
             formno = 3
             !
             !             Adapt coefficients
             !
             if (cgda>cgfa) then
                if (.not.dpsequfm(dc, 0.0D0, 1.0D-20)) then
                   cgfa = max(ds/dc*cgda, cgfa)
                endif
             elseif (ds>0.0D0) then
                cgfa = min(dc/ds*cgda, cgfa)
             else
             endif
             ds = dc
          !TEM          WRITE (11,*) 'cgfa,mugfa',cgfa,mugfa
          else
             !
             !             - drowned gate -
             !
             formno = 4
          endif
       endif
    !
    !
    endif
    !
    !TEM    WRITE (11,*) 'formno,ds,dc,dg',formno,ds,dc,dg
    !
    !       The flowe condition is known so calculate
    !       the linearization coefficients FU and RU
    !
    if (jarea) then
       call flgsareafm(formno, m, husb, velhght, zs, ds, dg, wstr)
    else
       call flgsfurufm(formno, m, teken, husb, hdsb, velhght, zs, ds, dg, dc, wstr,   &
                       cwfa, cwd, mugfa, cgfa, cgda, strdamf, lambda)
    endif
end subroutine flqhgsfm

subroutine flccgsfm(dg, dsc, cgd, cgf, cw, mugf, cgda, cgfa, mugfa)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    double precision, intent(in)   :: cgd
    double precision, intent(out)  :: cgda
    double precision, intent(in)   :: cgf
    double precision, intent(out)  :: cgfa
    double precision, intent(in)   :: cw
    double precision :: dg
    double precision :: dsc
    double precision, intent(in)   :: mugf
    double precision, intent(out)  :: mugfa
!
!
! Local variables
!
    logical                        :: dpsequfm
!
!
!! executable statements -------------------------------------------------------
!
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Brouwer/J.Kuipers
    !
    ! Module:             FLCCGS (FLow Corr. Coefficients for General Structure)
    !
    ! Module description: Correct coefficients for gate flow
    !
    !                     In the formulas for the gate and weir several
    !                     coefficients are applied. To avoid discontinuities
    !                     in the transition from weir to gate flow, the
    !                     correction coefficient cgd should be corrected.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    !  3 cgd               I  Correction coefficient for drowned gate flow.
    !  7 cgda              O  Adapted correction coefficient for drowned
    !                         gate flow.
    !  4 cgf               I  Correction coefficient for free gate flow.
    !  8 cgfa              O  Adapted correction coefficient for free gate
    !                         flow.
    !  5 cw                I  Correction coefficient for weir flow.
    !  1 dg                I  Gate opening height.
    !  2 dsc               I  Depth at sill or critical depth.
    !  6 mugf              I  Contraction coefficient for free gate flow.
    !  9 mugfa             O  Adapted contraction coefficient for free gate
    !                         flow.
    !=======================================================================
    !
    !     Declaration of parameters:
    !
    !
    !     Logical function
    !
    !
    !     dsc contains ds or dc
    !
    if (.not.dpsequfm(dsc, 0.0D0, 1.D-20)) then
       !
       if (dg/dsc>mugf) then
          mugfa = dg/dsc
       else
          mugfa = mugf
       endif
       !
       if (cgd>cw) then
          if (dpsequfm(dg, 0.0D0, 1.0D-20)) then
             cgda = cgd
          else
             cgda = min(dsc/dg*cw, cgd)
          endif
       else
          cgda = max(dg/dsc*cw, cgd)
       endif
       !
       if (cgf>cw) then
          if (dpsequfm(dg, 0.0D0, 1.0D-20)) then
             cgfa = cgf
          else
             cgfa = min(dsc/dg*cw, cgf)
          endif
       else
          cgfa = max(dg/dsc*cw, cgf)
       endif
    !
    else
       mugfa = mugf
       cgda = cgd
       cgfa = cgf
    endif
end subroutine flccgsfm

function dpsequfm(dvar1, dvar2, eps) ! equal within eps?
implicit none
logical                        :: dpsequfm
double precision, intent(in)   :: dvar1, dvar2, eps
dpsequfm = abs(dvar1 - dvar2)<eps
end function dpsequfm

subroutine readandallocstructures( )
use m_strucs
use m_alloc
implicit none
integer :: i, ierr

call realloc(strhis, mxstrhis, nstru)
call aerr('strhis(mxstrhis,nstru)' ,ierr , mxstrhis*nstru )

call realloc(strhis2, mxstrhis, nstru)
call aerr('strhis2(mxstrhis,nstru)' ,ierr , mxstrhis*nstru )

mxgeneral   = 0d0
mxuniversal = 0d0

do i = 1,nstru
   if (itypstr(i) == ST_GENERAL_ST) then
      mxgeneral   = mxgeneral + 1
      ntypstr(i)  = mxgeneral
   else if (itypstr(i) == ST_UNI_WEIR) then
      mxuniversal = mxuniversal + 1
      ntypstr(i)  = mxuniversal
   endif
enddo

if (mxgeneral > 0) then
   if (allocated (generalstruc) ) deallocate (generalstruc)
   allocate (generalstruc(mxgeneral), stat=ierr)
endif

if (mxuniversal > 0) then
   if (allocated (universalstruc) ) deallocate (universalstruc)
   allocate (universalstruc(mxuniversal), stat=ierr)
endif

end subroutine readandallocstructures


subroutine furusobekstructures()
use m_flow
use m_flowgeom
use m_strucs
implicit none
integer :: ng, n, L, Ls, LL, Lb, Lt
double precision :: zup, bup, a, fac

logical :: firstiter=.true. , jarea= .false.

firstiter = .true.
jarea     = .false.

do ng = 1, ncgensg      ! loop over generalstruc signals, sethu
   do n  = L1cgensg(ng), L2cgensg(ng)
      L  = kcgen(3,n)
      if (kcgen(1,n) == ln(2,L)) then
         Ls = -L ! Flow link has opposite orientation to structure's orientation.
      else
         Ls = L
      end if

      if (hu(L) > 0d0) then ! hu is above lowest sill
         call flgsfm( n, ng, Ls, firstiter , jarea )
      endif
      if (kmx > 0) then
         call getLbotLtop(L,Lb,Lt)
         do LL = Lb, Lt
            fu(LL) = fu(L) ; ru(LL) = ru(L)
            au(LL) = au(L)*( hu(LL)-hu(LL-1) ) / ( hu(Lt)-hu(Lb-1) )
         enddo
      endif
   enddo
enddo

end subroutine furusobekstructures

!> Computes and sets the widths and gate lower edge levels on each of the flow links
!! crossed by a general structure (gate/weir/true genstru).
!! This is now an extended version of SOBEK's setLineStructure, because it also enables
!! a sideways closing gate with two doors from the left and right side, where the partially
!! closed portions have gate flow, and the center open portion still only has normal weir
!! flow across the sill.
subroutine update_zcgen_widths_and_heights()
use m_strucs
use m_flowexternalforcings
use m_flowgeom
use m_structures
implicit none

double precision :: crestwidth, totalWidth, closedWidth, closedGateWidthL, closedGateWidthR, help
integer :: ng, L, L0, Lf

do ng=1,ncgensg ! Loop over general structures

   ! Crest level is the same across all crossed flow links. Possibly time-dependent:
   generalstruc(ng)%levelcenter = zcgen((ng-1)*3+1)

   ! 1: First determine total width of all genstru links (TODO: AvD: we should not recompute this every user time step)
   totalWidth = 0d0
   if (generalstruc(ng)%numlinks == 0) then
      cycle ! Only upon invalid input (see warnings in log about missing structure params)
   end if

   do L=L1cgensg(ng),L2cgensg(ng)
      L0 = L-L1cgensg(ng)+1
      Lf = kcgen(3,L)
      generalstruc(ng)%widthcenteronlink(L0) = wu(Lf)
      totalWidth = totalWidth + wu(Lf)
   end do

   ! 2a: the desired crest width for this overall structure (hereafter, the open links for this genstru should add up to this width)
   !     Also: only for gates, the desired door opening width for this overall structure
   !           (should be smaller than crestwidth, and for this portion the open gate door is emulated by dummy very high lower edge level)
   if (cgen_type(ng) == ICGENTP_WEIR) then
      crestwidth = zcgen((ng-1)*3+3) ! TODO: AvD: this is probably always 1d10, weir has no crest_width attribute yet.
      closedGateWidthL = 0d0
      closedGateWidthR = 0d0
   else if (cgen_type(ng) == ICGENTP_GENSTRU) then
      !crestwidth = totalWidth ! No crest/sill-width setting for true general structure yet (not old ext, nor new ext)
      crestwidth = min(totalWidth, generalstruc(ng)%widthcenter)
!      crestwidth = zcgen((ng-1)*3+3) ! NOTE: AvD: this now comes from scalar attribute 'widthcenter', no timeseries yet.
      ! genstru: always IOPENDIR_SYMMETRIC (TODO: UNST-1935)
      closedGateWidthL = max(0d0, .5d0*(totalWidth - zcgen((ng-1)*3+3)))
      closedGateWidthR = max(0d0, .5d0*(totalWidth - zcgen((ng-1)*3+3)))
      !closedGateWidthL = 0d0 ! max(0d0, .5d0*(totalWidth - zcgen((ng-1)*3+3))) ! Default symmetric opening
      !closedGateWidthR = 0d0 ! max(0d0, .5d0*(totalWidth - zcgen((ng-1)*3+3)))
      generalstruc(ng)%gateheightonlink(1:generalstruc(ng)%numlinks) = huge(1d0) ! As a start, gate door is open everywhere. Below, we will close part of the gate doors.
   else if (cgen_type(ng) == ICGENTP_GATE) then
      ! For a gate: zcgen(3,ng) is limited to the door opening width, but we want to open all links
      ! *underneath* the two doors as well, (if lower_edge_level is still high enough above sill_level)
      crestwidth = min(totalWidth, gates(cgen2str(ng))%sill_width)
      if (gates(cgen2str(ng))%opening_direction == IOPENDIR_FROMLEFT) then
         closedGateWidthL = max(0d0, totalWidth - zcgen((ng-1)*3+3))
         closedGateWidthR = 0d0
      else if (gates(cgen2str(ng))%opening_direction == IOPENDIR_FROMRIGHT) then
         closedGateWidthL = 0d0
         closedGateWidthR = max(0d0, totalWidth - zcgen((ng-1)*3+3))
      else ! IOPENDIR_SYMMETRIC
         closedGateWidthL = max(0d0, .5d0*(totalWidth - zcgen((ng-1)*3+3)))
         closedGateWidthR = max(0d0, .5d0*(totalWidth - zcgen((ng-1)*3+3)))
      end if
      generalstruc(ng)%gateheightonlink(1:generalstruc(ng)%numlinks) = huge(1d0) ! As a start, gate door is open everywhere. Below, we will close part of the gate doors.
   end if


   ! 2b: Determine the width that needs to be fully closed on 'left' side
   ! close the line structure from the outside to the inside: first step increasing increments
   ! NOTE: closed means: fully closed because sill_width (crest_width) is smaller that totalwidth.
   !       NOT because of gate door closing: that is handled by closedGateWidthL/R and may still
   !       have flow underneath doors if they are up high enough.
   closedWidth = max(0d0, totalWidth - crestwidth)/2d0 ! Intentionally symmetric: if crest/sill_width < totalwidth. Only gate door motion may have a direction, was already handled above.

   generalstruc(ng)%gateclosedfractiononlink = 0d0

   do L=L1cgensg(ng),L2cgensg(ng)
      L0 = L-L1cgensg(ng)+1
      Lf = kcgen(3,L)

      if (closedWidth > 0d0) then
         help = min (wu(Lf), closedWidth)
         generalstruc(ng)%widthcenteronlink(L0) = wu(Lf) - help ! 0d0 if closed
         closedWidth = closedWidth - help
      else
         generalstruc(ng)%widthcenteronlink(L0) = wu(Lf)
      end if

      if ((cgen_type(ng) == ICGENTP_GATE .or. cgen_type(ng) == ICGENTP_GENSTRU) .and. closedGateWidthL > 0d0 ) then
         !if (closedGateWidthL > .5d0*wu(Lf)) then
         generalstruc(ng)%gateheightonlink(L0) = zcgen((ng-1)*3+2)
         help = min (wu(Lf), closedGateWidthL)
         closedGateWidthL = closedGateWidthL - help
         !end if

         if ( wu(Lf).gt.0d0 ) then
            generalstruc(ng)%gateclosedfractiononlink(L0) = generalstruc(ng)%gateclosedfractiononlink(L0) + help/wu(Lf)
         end if
      else

      end if

      if (closedWidth <= 0d0 .and. closedGateWidthL <= 0d0) then
         ! finished
         exit
      endif
   enddo

   ! 2c: Determine the width that needs to be fully closed on 'right' side
   ! close the line structure from the outside to the inside: first step increasing increments
   ! NOTE: closed means: fully closed because sill_width (crest_width) is smaller that totalwidth.
   !       NOT because of gate door closing: that is handled by closedGateWidthL/R and may still
   !       have flow underneath doors if they are up high enough.
   closedWidth = max(0d0, totalWidth - crestwidth)/2d0 ! Intentionally symmetric: if crest/sill_width < totalwidth. Only gate door motion may have a direction, was already handled above.
   do L=L2cgensg(ng),L1cgensg(ng),-1
      L0 = L-L1cgensg(ng)+1
      Lf = kcgen(3,L)

      if (closedWidth > 0d0) then
         help = min (wu(Lf), closedWidth)
         generalstruc(ng)%widthcenteronlink(L0) = wu(Lf) - help ! 0d0 if closed
         closedWidth = closedWidth - help
      else
         generalstruc(ng)%widthcenteronlink(L0) = wu(Lf)
      end if

      if ((cgen_type(ng) == ICGENTP_GATE .or. cgen_type(ng) == ICGENTP_GENSTRU) .and. closedGateWidthR > 0d0) then
         !if (closedGateWidthL > .5d0*wu(Lf)) then
         generalstruc(ng)%gateheightonlink(L0) = zcgen((ng-1)*3+2)
         help = min (wu(Lf), closedGateWidthR)
         closedGateWidthR = closedGateWidthR - help
         !end if

         if ( wu(Lf).gt.0d0 ) then
            generalstruc(ng)%gateclosedfractiononlink(L0) = generalstruc(ng)%gateclosedfractiononlink(L0) + help/wu(Lf)
         end if
      end if

       if (closedWidth <= 0d0 .and. closedGateWidthR <= 0d0) then
         ! finished
         exit
      endif
   enddo

   !if ( L2cgensg(ng) == L1cgensg(ng) ) then
   !   generalstruc(ng)%widthcenteronlink(L0) = min( wu(Lf), zcgen((ng-1)*3+3) )
   !endif

end do ! 1,ngensg

end subroutine update_zcgen_widths_and_heights

subroutine enloss(ag        ,d1        ,eweir     ,hkruin    ,hov       , &
                & qunit     ,qvolk     ,toest     ,vov       , &
                & ewben     ,wsbov     ,wsben     ,dte       , &
                & dtefri    ,iflagweir , &
                & crestl    ,rmpbov    ,rmpben    ,veg      )
!-------------------------------------------------------------------------------
!  Original URL: https://svn.oss.deltares.nl/repos/delft3d/trunk/src/engines_gpl/flow2d3d/packages/kernel/src/compute/enloss.f90
!!--description-----------------------------------------------------------------
!
! Function: Determines additional energy loss due to weir.
!           Energy loss dependent on velocity perpendicular
!           to weir (Schonfeld, 1955).
!           Subroutine based on subroutine ENLOSS in WAQUA.
!           Energyloss based on Carnot relation for
!           decellerated flow VOV < 0.25 m/s
!           or on "Tables" VOV > 0.5 m/s or
!           average for 0.25 < VOV < 0.50 m/s.
!           In 2003 the improved formulations described in
!           "Beoordeling nieuwe overlaatroutines WAQUA"
!           WL-report Z3063, have been implemented and tested.
!
!!--pseudo code and references--------------------------------------------------
!
! "Weergave van extra energieverlies in RIVCUR."
!  (J.H.A. Wybenga, report Deltares Q779 voor RWS/RIZA, 1989).
! "Energieverliezen door overlaten: een gewijzigde berekeningsprocedure voor
!  WAQUA-rivieren versie."
!  (H. Vermaas, verslag onderzoek Deltares Q92, 1987)
! "Beoordeling nieuwe overlaatroutines WAQUA"
!  (J. van Kester, verslag Z3063, juli 2001)
!
!!--declarations----------------------------------------------------------------
    use m_flowgeom
    use precision
    implicit none
!
! Global variables
!
    real(fp)    , intent(in)    :: ag     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , intent(in)    :: d1     !!  Distance between crest and downstream depth
    real(fp)    , intent(out)   :: dte    !!  Subgrid energy loss due to weir
    real(fp)    , intent(in)    :: dtefri !!  Energy loss due to friction
    real(fp)    , intent(in)    :: ewben  !!  Energy height downstream
    real(fp)    , intent(in)    :: eweir  !!  Energy height at weir
    real(fp)    , intent(in)    :: hkruin !!  Crest height (downward positive).
    real(fp)    , intent(in)    :: hov    !!  Total water depth at crest weir
    real(fp)    , intent(in)    :: qunit  !!  Discharge at weir crest
    real(fp)    , intent(in)    :: qvolk  !!  Maximum discharge (super critical flow)
    real(fp)    , intent(in)    :: vov    !!  Velocity at crest of weir
    real(fp)    , intent(in)    :: wsben  !!  Downstream water level
    real(fp)    , intent(in)    :: wsbov  !!  Upstream water level
    character(4), intent(inout) :: toest  !!  State weir:
                                          !!  volk = perfect weir
                                          !!  onvo = imperfect weir
    integer    , intent(in)     :: iflagweir  !!  Flag to switch between Tabellenboek and Villemonte
    real(fp)   , intent(in)     :: crestl !!  crest length of weir
    real(fp)   , intent(in)     :: rmpben !!  ramp (talud) downstream of weir
    real(fp)   , intent(in)     :: rmpbov !!  ramp (talud) upstream of weir
    real(fp)   , intent(in)     :: veg    !!  Vegetation on weir
!
! Local variables
!
    real(fp) :: dtecar
    real(fp) :: dteonv
    real(fp) :: dtetab
    real(fp) :: dtevol
    real(fp) :: qqv
    double precision :: tabellenboek
    real(fp) :: theta
    real(fp) :: vilcd(1:2)  !! These parameters have to be read in
    real(fp) :: p, pref, qvolkvil, qweir, q2rat, cd0, cd0ref, sqfac, alfitp, re
    real(fp) :: ddive, vil1, vil2
!
!! executable statements -------------------------------------------------------
!
    if (iflagweir == 24) then    !! Tabellenboek
       !
       ! Determine energy loss for Tabellenboek
       !
       qqv = qunit/qvolk
       !
       ! Imperfect weir (sub critical flow)
       !
       if ((wsben + hkruin + d1)==0.0) then
          !
          ! Dry bed downstream, could perhaps also check on qunit==0.0
          !
          dteonv = 0.0
       elseif (abs(vov)<=0.25) then
          !
          ! Energy loss according Carnot law
          ! WSBEN+HKRUIN+D1 := H0 (S0+DPS)
          !
          dteonv = (vov - qunit/(wsben + hkruin + d1))**2/(2.*ag)
       elseif (abs(vov)>0.25 .and. abs(vov)<0.5) then
          !
          ! Weigthing between Carnot and Tables
          ! WSBEN+HKRUIN+D1 := H0 (S0+DPS)
          !
          dtecar = (vov - qunit/(wsben + hkruin + d1))**2/(2.*ag)
          dtetab = tabellenboek(d1, eweir, qunit, qvolk)
          theta = (abs(vov) - 0.25)/0.25
          dteonv = (1 - theta)*dtecar + theta*dtetab
       elseif (abs(vov)>=0.5) then
          !
          ! Energy loss according to Tables from Vermaas
          !
          dteonv = tabellenboek(d1, eweir, qunit, qvolk)
       else
       endif

    elseif (iflagweir == 25) then     !! Villemonte
       !
       ! Set Villemonte coefficients

       vilcd(1) = VillemonteCD1
       vilcd(2) = VillemonteCD2
       !
       ! Determine energy loss for Villemonte
       !
       ! Sieben2010 including vegetation:
       ! Noted that cd0_ref and P-ref are only used for determining
       ! whether (on)volkomen flow appears. Vegetation is excluded.
       ! cd0 and p are used to compute the energy loss, in which
       ! vegetation is included.

       alfitp = exp(-0.5d0*eweir/max(0.01d0,crestl))
       cd0ref = vilcd(1) *                                             &
                ( alfitp      * (1.0d0-0.25d0*exp(-0.5d0*rmpbov)) +          &
                  (1.-alfitp) * (0.8d0+0.65d0*exp(-0.1d0*rmpben)) )
       cd0    = cd0ref * (1.0d0 + veg/3.0d0)**(-1.5d0)

       !
       ! Sieben' formula of 3 February 2010:
       !
       ! pref = 3.0**3 / (4.0  * cd0**2) *                              &
       !      (1 + min(5.0d0,d1/eweir)*(1-exp(-rmpben/vilcd(2))))**2
       !
       ! Sieben' formula of 6 August 2010:
       !
       ddive = min(5.0d0,d1/eweir)
       vil1 = 1 + ddive * (1-exp(-rmpben/vilcd(2)))
       vil1 = 1.0 / (vil1**2)
       vil2 = (1 + ddive)
       vil2 = 1.0 / (vil2**2)
       pref = 3.0**3 / (4.0 * cd0**2) / (max(0.001d0,vil1 - vil2))

       p = (1.0 + veg/3.0)**3 / (1.0+2*veg) * pref

       qvolkvil = 2.0/3.0 * eweir * sqrt(2.0/3.0 * ag * eweir) * cd0

       sqfac = sqrt(max(0.0d0,1.0d0-max(0.0d0,ewben/eweir)**pref))
       qweir = qvolkvil * sqfac
       !
       ! determine energy loss for submerged weir flow with Sieben
       !
       q2rat = max(0.0d0, qunit**2/qvolkvil**2)
       if (q2rat .lt. 1.0d0) then
          re =  max(0.00000001d0, 1.0d0 -( 1.0d0-q2rat )**(1.0d0/p))
       else
          re = 1.
       endif

       q2rat  = sqfac
       dteonv = re * eweir
       qvolkvil = max(0.00000001d0, qweir)
       qqv = 0.
    endif

    !
    ! Determine energy loss for free weir flow
    !
    dtevol = wsbov - wsben - dtefri
    !
    ! Determine if it is free weir flow or submerged weir flow
    !
    if (iflagweir == 24) then    !! Tabellenboek
       !
       if (dtevol*qqv**2>=dteonv) then
           !
           ! It is a free weir flow
           !
           toest = 'volk'
        else
           !
           ! It is a submerged weir flow
           !
           toest = 'onvo'
       endif
    elseif (iflagweir == 25) then     !! Villemonte
        !
        if (q2rat .gt. 0.99d0) then
           !
           ! It is a free weir flow
           !
           toest = 'volk'
        else
           !
           ! It is a submerged weir flow
           !
           toest = 'onvo'
       endif
    endif
    !
    ! Energy loss
    !
    if (toest=='volk') then
       !
       ! It is a free weir flow
       !
       dte = dtevol
    elseif (toest=='onvo') then
       !
       ! It is a submerged weir flow
       !
       dte = dteonv
    else
    endif

!    if (iflagweir == 24) then         !! Tabellenboek
!       write(88,'(2a,10f8.4)')  'Tabellenboek  ', toest, dteonv, dtevol
!    elseif (iflagweir == 25) then     !! Villemonte
!       write(88,'(2a,10f8.4)')  'Villemonte  ', toest, dteonv, qweir, qunit, qvolkvil, q2rat, p
!    endif
end subroutine enloss

double precision function tabellenboek(d1        ,eweir     ,qunit     ,qvolk     )
!-------------------------------------------------------------------------------
!  $Id: manholes.f90 65778 2020-01-14 14:07:42Z mourits $
!   Original URL: https://svn.oss.deltares.nl/repos/delft3d/trunk/src/engines_gpl/flow2d3d/packages/kernel/src/compute/tabel.f90
!!--description-----------------------------------------------------------------
!
!    Function: Determines additional energy loss due to weir.
!              Energy loss dependent on velocity perpendicular
!              to weir (Schonfeld, 1955).
!              Subroutine based on subroutine TABEL in WAQUA.
!              Energyloss based on Carnot relation for
!              decellerated flow, or on "Tabellenboek".
!              This subroutine is also called for supercritical
!              flow (qqv = 1.0), see the formulations in
!              "Beoordeling nieuwe overlaatroutines WAQUA"
!              WL-report Z3063.
! Method used: Reference : Weergave van extra energieverlies
!              in RIVCUR. (J.H.A. Wybenga, report
!              Deltares Q779 voor RWS/RIZA, 1989).
!              Energieverliezen door overlaten: een gewijzigde
!              berekeningsprocedure voor WAQUA-rivieren versie.
!              (H. Vermaas, verslag onderzoek Deltares
!               Q92, 1987)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    real(fp)          , intent(in) :: d1     !!  Distance between crest and downstream depth
    real(fp)          , intent(in) :: eweir  !!  Energy level at weir
    real(fp)          , intent(in) :: qunit  !!  Discharge at weir crest
    real(fp)          , intent(in) :: qvolk  !!  Maximum discharge (super critical flow)

!
!
! Local variables
!
    real(fp)                       :: f1
    real(fp)                       :: f1low
    real(fp)                       :: f1up
    real(fp)                       :: f2
    real(fp)                       :: f2low
    real(fp)                       :: f2up
    real(fp)                       :: qqv
    real(fp)                       :: qqvlow
    real(fp)                       :: qqvup
    real(fp)                       :: theta
!
!
!! executable statements -------------------------------------------------------
!
    !
    qqv = min(qunit/qvolk, 1.0_fp)
    !
    !-----Calculate energy loss according to tables (based on experinments
    !     in the delta flums)
    !
    if (qqv<0.3) then
       qqvlow = 0.
       qqvup = 0.3
       if (eweir<0.5) then
          f1low = 0.0
          f1up = 0.0092*eweir
          f2low = 0.0
          f2up = 0.328
       else
          f1low = 0.0
          f1up = 0.0057 + 0.002*log10(eweir - 0.21)
          f2low = 0.0
          f2up = 0.314 + 0.0237*eweir**0.808
       endif
    !
    elseif (qqv<0.6) then
       qqvlow = 0.3
       qqvup = 0.6
       if (eweir<0.5) then
          f1low = 0.0092*eweir
          f1up = 0.0211*eweir
          f2low = 0.328
          f2up = 0.316
       else
          f1low = 0.0057 + 0.002*log10(eweir - 0.21)
          f1up = -0.0017 + 0.033*log10(eweir + 1.85)
          f2low = 0.314 + 0.0237*eweir**0.808
          f2up = 0.283 + 0.0531*eweir**0.702
       endif
    !
    elseif (qqv<0.8) then
       qqvlow = 0.6
       qqvup = 0.8
       if (eweir<0.5) then
          f1low = 0.0211*eweir
          f1up = 0.0463*eweir
          f2low = 0.316
          f2up = 0.295
       else
          f1low = -0.0017 + 0.033*log10(eweir + 1.85)
          f1up = -0.0022 + 0.064*log10(eweir + 1.99)
          f2low = 0.283 + 0.0531*eweir**0.702
          f2up = 0.280 + 0.0272*eweir**0.912
       endif
    !
    elseif (qqv<0.9) then
       qqvlow = 0.8
       qqvup = 0.9
       if (eweir<0.5) then
          f1low = 0.0463*eweir
          f1up = 0.0657*eweir
          f2low = 0.295
          f2up = 0.238
       else
          f1low = -0.0022 + 0.064*log10(eweir + 1.99)
          f1up = 0.039 + 0.076*log10(eweir + 0.33)
          f2low = 0.280 + 0.0272*eweir**0.912
          f2up = 0.224 + 0.0256*eweir**0.869
       endif
    !
    elseif (qqv<0.95) then
       qqvlow = 0.9
       qqvup = 0.95
       if (eweir<0.5) then
          f1low = 0.0657*eweir
          f1up = 0.0772*eweir
          f2low = 0.238
          f2up = 0.206
       else
          f1low = 0.039 + 0.076*log10(eweir + 0.33)
          f1up = 0.065 + 0.116*log10(eweir + 0.092)
          f2low = 0.224 + 0.0256*eweir**0.869
          f2up = 0.124 + 0.11*eweir**0.422
       endif
    !
    elseif (qqv<0.99) then
       qqvlow = 0.95
       qqvup = 0.99
       if (eweir<0.5) then
          f1low = 0.0772*eweir
          f2low = 0.206
       else
          f1low = 0.065 + 0.116*log10(eweir + 0.092)
          f2low = 0.124 + 0.11*eweir**0.422
       endif
       if (eweir<1.0) then
          f1up = 0.115*eweir**0.9
          f2up = 0.242
       else
          f1up = 0.133 + 0.213*log10(eweir - 0.173)
          f2up = 0.133 + 0.109*eweir**0.619
       endif
    !
    elseif (qqv>=0.99) then
       qqvlow = 0.99
       qqvup = 1.0
       if (eweir<1.0) then
          f1low = 0.115*eweir**0.9
          f2low = 0.242
       else
          f1low = 0.133 + 0.213*log10(eweir - 0.173)
          f2low = 0.133 + 0.109*eweir**0.619
       endif
       if (eweir<2.0) then
          f1up = 0.156*eweir**0.75
          f2up = 0.343
       else
          f1up = 0.244 + 0.172*log10(eweir - 0.718)
          f2up = 0.075 + 0.176*eweir**0.609
       endif
    !
    else
    endif
    !
    !-----Calculate terms for energy loss
    !
    theta = (qqv - qqvlow)/(qqvup - qqvlow)
    f1 = (1 - theta)*f1low + theta*f1up
    f2 = (1 - theta)*f2low + theta*f2up
    tabellenboek = f1*d1**f2
end function tabellenboek

