module m_rdmor
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2015.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: rdmor.f90 4612 2015-01-21 08:48:09Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_gpl/morphology/packages/morphology_io/src/rdmor.f90 $
!-------------------------------------------------------------------------------

private

!
! functions and subroutines
!
public rdmor
public echomor

contains

subroutine rdmor(lundia    ,error     ,filmor    ,lsec      ,lsedtot   , &
               & lsed      ,nmaxus    ,nto       , &
               & nambnd    ,julday    ,mor_ptr   ,sedpar    ,morpar    , &
               & fwfac     ,morlyr    ,griddim   )
!!--description-----------------------------------------------------------------
!
! Reads attribute file for 3D morphology computation
!
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use table_handles
    use bedcomposition_module
    use morphology_data_module
    use grid_dimens_module, only: griddimtype
    use sediment_basics_module
    use string_module
    use m_rdmorlyr
    use message_module, only: write_error, write_warning, FILE_NOT_FOUND, FILE_READ_ERROR, PREMATURE_EOF
    !
    implicit none
    !
    ! The following list of pointer parameters is used to point inside the data structures
    !
    integer                                , pointer :: subiw
    integer                                , pointer :: i10
    integer                                , pointer :: i50
    integer                                , pointer :: i90
    integer                                , pointer :: ihidexp
    integer                                , pointer :: itmor
    integer                                , pointer :: iopkcw
    integer                                , pointer :: islope
    integer                                , pointer :: morfacpar
    integer                                , pointer :: morfacrec
    integer                                , pointer :: morfactable
    integer                                , pointer :: nxx
    integer                                , pointer :: nmudfrac
    real(fp)                               , pointer :: morfac
    real(fp)                               , pointer :: thresh
    real(fp)                               , pointer :: aksfac
    real(fp)                               , pointer :: rwave
    real(fp)                               , pointer :: alfabs
    real(fp)                               , pointer :: alfabn
    real(fp)                               , pointer :: camax
    real(fp)                               , pointer :: dzmax
    real(fp)                               , pointer :: sus
    real(fp)                               , pointer :: bed
    real(fp)                               , pointer :: tmor
    real(fp)                               , pointer :: thetsd
    real(fp)                               , pointer :: susw
    real(fp)                               , pointer :: sedthr
    real(fp)                               , pointer :: hmaxth
    real(fp)                               , pointer :: bedw
    real(fp)                               , pointer :: factsd
    real(fp)                               , pointer :: rdc
    real(fp)                               , pointer :: rdw
    real(fp)                               , pointer :: espir
    real(fp)                               , pointer :: ashld
    real(fp)                               , pointer :: bshld
    real(fp)                               , pointer :: cshld
    real(fp)                               , pointer :: dshld
    real(fp)                               , pointer :: coulfri
    real(fp)                               , pointer :: flfdrat
    real(fp)                               , pointer :: alfpa
    real(fp)                               , pointer :: thcrpa
    real(fp)                               , pointer :: asklhe
    real(fp)                               , pointer :: mwwjhe
    real(fp)                               , pointer :: pangle
    real(fp)                               , pointer :: fpco
    real(fp)                               , pointer :: factcr
    real(fp)                               , pointer :: wetslope
    real(fp)                               , pointer :: avaltime
    real(fp)              , dimension(:)   , pointer :: xx
    logical                                , pointer :: bedupd
    logical                                , pointer :: cmpupd
    logical                                , pointer :: eqmbcsand
    logical                                , pointer :: eqmbcmud
    logical                                , pointer :: densin
    logical                                , pointer :: rouse
    logical                                , pointer :: epspar
    logical                                , pointer :: updinf
    logical                                , pointer :: neglectentrainment
    logical                                , pointer :: oldmudfrac
    logical                                , pointer :: varyingmorfac
    logical                                , pointer :: multi
    logical                                , pointer :: anymud
    logical                                , pointer :: eulerisoglm
    logical                                , pointer :: glmisoeuler
    character(256)                         , pointer :: bcmfilnam
    character(20)          , dimension(:)  , pointer :: namsed
    type(handletype)                       , pointer :: bcmfile
    type(handletype)                       , pointer :: morfacfile
    type(moroutputtype)                    , pointer :: moroutput
    type(mornumericstype)                  , pointer :: mornum
    type(bedbndtype)       , dimension(:)  , pointer :: morbnd
!
! Local parameters
!
    integer, parameter :: maxfld     = 20
    integer, parameter :: maxfldprog = 10
!
! Call variables
!
    integer                        , intent(in)  :: julday
    integer                        , intent(in)  :: nmaxus
    integer                        , intent(in)  :: nto
    integer                                      :: lundia  !  Description and declaration in inout.igs
    integer                        , intent(in)  :: lsec
    integer                        , intent(in)  :: lsed    !  Description and declaration in esm_alloc_int.f90
    integer                        , intent(in)  :: lsedtot !  Description and declaration in esm_alloc_int.f90
    logical                        , intent(out) :: error
    character(*)                                 :: filmor
    character(20) , dimension(nto)               :: nambnd  !  Description and declaration in esm_alloc_char.f90
    type(tree_data)                , pointer     :: mor_ptr
    type(sedpar_type)              , pointer     :: sedpar
    type(morpar_type)              , pointer     :: morpar
    type(bedcomp_data)             , pointer     :: morlyr
    real(fp)                       , intent(out) :: fwfac
    type(griddimtype)   , target   , intent(in)  :: griddim
!
! Local variables
!
    integer                                                           :: i
    integer                                                           :: ilist
    integer                                                           :: ilun     ! Unit number for attribute file
    integer                                                           :: istat
    integer                                                           :: j
    integer                                                           :: jj
    integer                                                           :: jmin
    integer                                                           :: lenc
    integer                                                           :: lfile    ! Length of file name
    integer                                                           :: nxxprog
    integer                                                           :: nxxuser
    integer                                                           :: version
    integer                    , external                             :: newunit
    integer                    , dimension(:) , allocatable           :: itype
    integer                    , dimension(:) , allocatable           :: ifield
    integer                    , dimension(:) , allocatable           :: lenchr
    real(fp)                                                          :: rmissval
    real(fp)                                                          :: xxmin
    real(fp)                   , dimension(:) , allocatable           :: xxprog
    real(fp)                   , dimension(:) , allocatable           :: rfield
    logical                                                           :: ex       ! Logical flag for file existence
    logical                                                           :: found
    character(10)                                                     :: versionstring
    character(20)                                                     :: fluxlimstring
    character(80)                                                     :: bndname
    character(256)                                                    :: errmsg
    character(256)                                                    :: pxxstr
    character(256)                                                    :: string
    character(10)              , dimension(:) , allocatable           :: cfield
    type(tree_data)                                         , pointer :: morbound_ptr
!
!! executable statements -------------------------------------------------------
!
    !
    allocate(itype  (maxfld))
    allocate(ifield (maxfld))
    allocate(lenchr (maxfld))
    allocate(rfield (maxfld))
    allocate(cfield (maxfld))
    allocate(xxprog (maxfldprog))
    !
    error         = .false.
    rmissval      = -999.0_fp
    rfield        = -999.0_fp
    version       = -1
    nxxuser       = 0
    !
    ! allocate memory for boundary conditions
    !
    istat = 0
    allocate (morpar%morbnd(nto), stat = istat)
    !
    if (istat /= 0) then
       call write_error('RDMOR: memory alloc error',unit=lundia)
       error = .true.
       return
    endif
    !
    ! Let local variables point to fields within data structures
    !
    morfac              => morpar%morfac
    thresh              => morpar%thresh
    aksfac              => morpar%aksfac
    rwave               => morpar%rwave
    alfabs              => morpar%alfabs
    alfabn              => morpar%alfabn
    camax               => morpar%camax
    dzmax               => morpar%dzmax
    sus                 => morpar%sus
    bed                 => morpar%bed
    tmor                => morpar%tmor
    thetsd              => morpar%thetsd
    susw                => morpar%susw
    sedthr              => morpar%sedthr
    hmaxth              => morpar%hmaxth
    bedw                => morpar%bedw
    factsd              => morpar%factsd
    rdc                 => morpar%rdc
    rdw                 => morpar%rdw
    espir               => morpar%espir
    ashld               => morpar%ashld
    bshld               => morpar%bshld
    cshld               => morpar%cshld
    dshld               => morpar%dshld
    coulfri             => morpar%coulfri
    flfdrat             => morpar%flfdrat
    alfpa               => morpar%alfpa
    thcrpa              => morpar%thcrpa
    asklhe              => morpar%asklhe
    mwwjhe              => morpar%mwwjhe
    i10                 => morpar%i10
    i50                 => morpar%i50
    i90                 => morpar%i90
    ihidexp             => morpar%ihidexp
    itmor               => morpar%itmor
    iopkcw              => morpar%iopkcw
    islope              => morpar%islope
    morfacpar           => morpar%morfacpar
    morfacrec           => morpar%morfacrec
    morfactable         => morpar%morfactable
    nxx                 => morpar%nxx
    bcmfile             => morpar%bcmfile
    morfacfile          => morpar%morfacfile
    moroutput           => morpar%moroutput
    mornum              => morpar%mornum
    morbnd              => morpar%morbnd
    wetslope            => morpar%wetslope
    avaltime            => morpar%avaltime
    xx                  => morpar%xx
    bedupd              => morpar%bedupd
    cmpupd              => morpar%cmpupd
    eqmbcsand           => morpar%eqmbcsand
    eqmbcmud            => morpar%eqmbcmud
    densin              => morpar%densin
    rouse               => morpar%rouse
    epspar              => morpar%epspar
    updinf              => morpar%updinf
    neglectentrainment  => morpar%neglectentrainment
    oldmudfrac          => morpar%oldmudfrac
    varyingmorfac       => morpar%varyingmorfac
    multi               => morpar%multi
    bcmfilnam           => morpar%bcmfilnam
    nmudfrac            => sedpar%nmudfrac
    namsed              => sedpar%namsed
    anymud              => sedpar%anymud
    pangle              => morpar%pangle
    fpco                => morpar%fpco
    factcr              => morpar%factcr
    subiw               => morpar%subiw
    eulerisoglm         => morpar%eulerisoglm
    glmisoeuler         => morpar%glmisoeuler
    !
    do j = 1, nto
       morbnd(j)%icond = 1
       morbnd(j)%ibcmt = 0
       morbnd(j)%npnt  = 0
       nullify(morbnd(j)%alfa_dist)
       nullify(morbnd(j)%alfa_mag)
       nullify(morbnd(j)%idir)
       nullify(morbnd(j)%nm)
       nullify(morbnd(j)%nxmx)
    enddo
    versionstring = 'n.a.'
    !
    ! Sediment percentiles needed by the program
    !
    nxxprog   = 3       ! limited to parameter maxfldprog
    xxprog(1) = 10.0_fp
    xxprog(2) = 50.0_fp
    xxprog(3) = 90.0_fp
    !
    ! Set default value for FWFac (only used if no mor file specified and
    ! parameter not specified in mdf file)
    !
    fwfac     = 1.0_fp
    !
    ! locate 'Filmor' record for attribute file containing parameters
    ! for morphological computation
    !
    if (filmor == ' ') then
       !
       ! file does not exist
       !
       errmsg = 'No morphological file defined. Using default values.'
       call write_warning(errmsg,unit=lundia)
       goto 8888
    endif 
    !
    ! Put mor-file in input tree
    !
    call prop_file('ini', trim(filmor), mor_ptr, istat)
    if (istat /= 0) then
       select case (istat)
       case(1)
          call write_error(FILE_NOT_FOUND//trim(filmor), unit=lundia)
       case(3)
          call write_error(PREMATURE_EOF//trim(filmor), unit=lundia)
       case default
          call write_error(FILE_READ_ERROR//trim(filmor), unit=lundia)
       endselect
       error = .true.
       return
    endif
    !
    ! Check version number of mor input file
    !
    call prop_get_string(mor_ptr, 'MorphologyFileInformation', 'FileVersion', versionstring)
    if (trim(versionstring) == '02.00') then
       version = 2
       !
       ! === morphological timescale factor
       ! First assume that 'MorFac' contains a filename
       ! If the file does not exist, assume that 'MorFac' contains a uniform value (real)
       !
       string = ' '
       call prop_get_string(mor_ptr, 'Morphology', 'MorFac', string)
       !
       ! Intel 7.0 crashes on an inquire statement when file = ' '
       !
       if (string == ' ') then
          ex = .false.
       else
          call combinepaths(filmor, string)
          inquire (file = trim(string), exist = ex)
       endif
       if (.not. ex) then
          varyingmorfac = .false.
          call prop_get(mor_ptr, 'Morphology', 'MorFac', morfac)
       else
          varyingmorfac = .true.
          morfac        = -1.0
          call readtable(morfacfile, trim(string), julday, errmsg)
          if (errmsg /= ' ') then
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
       endif
       endif
       !
       ! === start for calculating morphological changes
       !
       call prop_get(mor_ptr, 'Morphology', 'MorStt', tmor)
       !
       ! calculation of ITMOR has been moved to TRICOM
       ! === threshold value for slowing erosion near a fixed layer (m)
       !
       call prop_get(mor_ptr, 'Morphology', 'Thresh', thresh)
       !
       ! === flags for doing morphological updates
       ! First, there was morupd
       ! Then there came bedupd and cmpupd
       ! morupd corresponds to the new bedupd.
       ! By replacing morupd with bedupd, the parameter morupd is no longer needed.
       ! By default, cmpupd must be true
       !
       call prop_get_logical(mor_ptr, 'Morphology', 'MorUpd', bedupd)
       cmpupd = .true.
       !
       ! flag for doing bed level updates
       !
       call prop_get_logical(mor_ptr, 'Morphology', 'BedUpd', bedupd)
       !
       ! flag for doing composition updates
       !
       call prop_get_logical(mor_ptr, 'Morphology', 'CmpUpd', cmpupd)
       !
       call prop_get_logical(mor_ptr, 'Morphology', 'NeglectEntrainment', neglectentrainment)
       !
       ! === flag for setting equilibrium sediment concentration profiles
       ! at the open boundaries for sand sediment
       ! First read it as string to check whether the keyword NeuBcSand is present.
       ! If not, try the old keyword EqmBc
       ! NeuBcSand will be used when both keywords are present
       !
       string = ' '
       call prop_get_string(mor_ptr, 'Morphology', 'NeuBcSand', string)
       if (string /= ' ') then
          call prop_get_logical(mor_ptr, 'Morphology', 'NeuBcSand', eqmbcsand)
       else
          call prop_get_logical(mor_ptr, 'Morphology', 'EqmBc', eqmbcsand)
       endif
       !
       ! === flag for setting equilibrium sediment concentration profiles
       ! at the open boundaries for mud sediment
       !
       call prop_get_logical(mor_ptr, 'Morphology', 'NeuBcMud', eqmbcmud)
       !
       ! === flag for including sediment in fluid density calculations
       !
       call prop_get_logical(mor_ptr, 'Morphology', 'DensIn', densin)
       !
       ! === factor for setting aks height
       !
       call prop_get(mor_ptr, 'Morphology', 'AksFac', aksfac)
       !
       ! === factor for calculating wave-related roughness from ripple dimensions
       !
       call prop_get(mor_ptr, 'Morphology', 'RWave', rwave)
       !
       ! Flag Rouse is skipped
       !
       ! === factor for longitudinal bed load transport
       !
       call prop_get(mor_ptr, 'Morphology', 'AlfaBs', alfabs)
       !
       ! === factor for transverse bed load transport
       !
       call prop_get(mor_ptr, 'Morphology', 'AlfaBn', alfabn)
       !
       ! === maximum slope used in avalanching
       !
       call prop_get(mor_ptr, 'Morphology', 'WetSlope', wetslope)
       !
       ! === time scale for avalanching
       !
       call prop_get(mor_ptr, 'Morphology', 'AvalTime', avaltime)
       !
       ! === factor for calculating suspended load transport
       !
       call prop_get(mor_ptr, 'Morphology', 'Sus', sus)
       !
       ! === factor for calculating bed load transport
       !
       call prop_get(mor_ptr, 'Morphology', 'Bed', bed)
       !
       ! === wave-related suspended sediment factor
       !
       call prop_get(mor_ptr, 'Morphology', 'SusW', susw)
       !
       ! === wave-related bed-load sediment factor
       !
       call prop_get(mor_ptr, 'Morphology', 'BedW', bedw)
       !
       ! === minimum depth for sediment calculations
       !
       call prop_get(mor_ptr, 'Morphology', 'SedThr', sedthr)
       !
       ! === global / maximum dry cell erosion factor
       !
       call prop_get(mor_ptr, 'Morphology', 'ThetSD', thetsd)
       !
       ! === maximum depth for variable dry cell erosion factor
       !
       call prop_get(mor_ptr, 'Morphology', 'HMaxTH', hmaxth)
       !
       ! === factor for adjusting intensity of energy dissipation in wave boundary layer
       ! fwfac should be read from mdf-file (see rdnum)
       ! The reading of fwfac is left here for backwards compatibility
       ! [This value will only be used if fwfac is not read from mdf-file]
       !
       call prop_get(mor_ptr, 'Morphology', 'FWFac', fwfac)
       !
       ! === factor for adjusting shields critical shear stress
       !
       call prop_get(mor_ptr, 'Morphology', 'Factcr', factcr)
       !
       ! === flag for using eulerian vel iso glm velocities for susp transports
       !
       call prop_get(mor_ptr, 'Morphology', 'EulerisoGLM', eulerisoglm)
       !
       ! === flag for using glm vel iso eulerian velocities for bed load transports and ref concentration
       !
       call prop_get(mor_ptr, 'Morphology', 'GLMisoEuler', glmisoeuler)
       !
       ! === phase lead for bed shear stress of Nielsen (1992) in TR2004
       !
       call prop_get(mor_ptr, 'Morphology', 'Pangle', pangle)
       !
       ! === coefficient for phase lag effects in wave-induced suspended transport in TR2004
       !
       call prop_get(mor_ptr, 'Morphology', 'Fpco', fpco)        
       !
       ! === wave period subdivision in TR2004
       !
       call prop_get(mor_ptr, 'Morphology', 'Subiw', subiw)
       ! === flag for parametric epsilon distribution in case of K-Eps model
       !
       call prop_get_logical(mor_ptr, 'Morphology', 'EpsPar', epspar)
       !
       call prop_get_integer(mor_ptr, 'Morphology', 'IopKCW', iopkcw)
       !
       ! === calibration factor for 2D suspended load relaxation time
       !
       call prop_get(mor_ptr, 'Morphology', 'FacTsd', factsd)
       !
       call prop_get(mor_ptr, 'Morphology', 'RDC', rdc)
       !
       call prop_get(mor_ptr, 'Morphology', 'RDW', rdw)
       !
       ! === flag for updating bed level at inflow boundary
       !
       call prop_get_logical(mor_ptr, 'Morphology', 'UpdInf', updinf)
       !
       ! === flag for merging bottoms of parallel runs
       !
       call prop_get_logical(mor_ptr, 'Morphology', 'Multi', multi)
       !
       if (updinf) then
          !
          ! set bed boundary conditions to "free boundaries"
          !
          do j = 1, nto
             morbnd(j)%icond = 0
          enddo
       endif
       !
       call prop_get(mor_ptr, 'Morphology', 'DzMax', dzmax)
       call prop_get(mor_ptr, 'Morphology', 'CaMax', camax)
       call prop_get_logical(mor_ptr, 'Morphology', 'OldMudFrac', oldmudfrac)
       !
       ! Hiding & exposure
       !
       call prop_get_integer(mor_ptr, 'Morphology', 'IHidExp', ihidexp)
       if (ihidexp < 1 .or. ihidexp > 5) then
          errmsg = 'IHidExp should be in the range 1 to 5 in ' // trim(filmor)
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       elseif (ihidexp>1 .and. anymud) then
          errmsg = 'Mud fractions included: IHidExp should be 1 in ' // trim(filmor)
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       select case(ihidexp)
       case(4) ! Parker, Klingeman, McLean
          call prop_get(mor_ptr, 'Morphology', 'ASKLHE', asklhe)
          if (comparereal(asklhe,rmissval) == 0) then
             errmsg = 'ASKLHE missing: Alpha exponent for Soehngen et.al. hiding & exposure in file '//trim(filmor)
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
       case(5) ! Wu, Wang, Jia
          call prop_get(mor_ptr, 'Morphology', 'MWWJHE', mwwjhe)
          if (comparereal(mwwjhe,rmissval) == 0) then
             errmsg = 'MWWJHE missing: M exponent for Wu, Wang, Jia hiding & exposure in file '//trim(filmor)
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
       endselect
       !
       ! MORSYS parameters
       !
       if (lsec > 0) then
          ! factor for spiral flow intensity effect
          call prop_get(mor_ptr, 'Morphology', 'Espir', espir)
       endif
       call prop_get_integer(mor_ptr, 'Morphology', 'ISlope', islope)
       if (islope < 1 .or. islope > 4) then
          ! bedslope effect formulation
          errmsg = 'ISlope should be 1 to 4 in ' // trim(filmor)
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       if (islope == 3) then
          ! bedslope effect parameters
          call prop_get(mor_ptr, 'Morphology', 'AShld', ashld)
          call prop_get(mor_ptr, 'Morphology', 'BShld', bshld)
          call prop_get(mor_ptr, 'Morphology', 'CShld', cshld)
          call prop_get(mor_ptr, 'Morphology', 'DShld', dshld)
       elseif (islope == 4) then
          call prop_get(mor_ptr, 'Morphology', 'CoulFri', coulfri)
          call prop_get(mor_ptr, 'Morphology', 'FlFdRat', flfdrat)
          alfpa = coulfri / (1.0 + flfdrat * coulfri)
          call prop_get(mor_ptr, 'Morphology', 'ThetaCr', thcrpa)
       endif
       !
       ! Boundary conditions
       !
       bcmfilnam = ' '
       call prop_get_string(mor_ptr, 'Morphology', 'BcFil', bcmfilnam)
       if (bcmfilnam /= ' ') then
          call combinepaths(filmor, bcmfilnam)
          !write (lundia, '(3a)') txtput1, ': ', trim(bcmfilnam)
          call readtable(bcmfile, bcmfilnam, julday, errmsg)
          if (errmsg /= ' ') then
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
       endif
       endif
       !
       do i = 1, size(mor_ptr%child_nodes)
          !
          ! Does mor_ptr contain a child with name 'Boundary' (converted to lower case)?
          !
          morbound_ptr => mor_ptr%child_nodes(i)%node_ptr
          bndname = tree_get_name( morbound_ptr )
          if ( trim(bndname) /= 'boundary') cycle
          bndname = ' '
          call prop_get_string(morbound_ptr, '*', 'Name', bndname)
          found = .false.
          do j = 1,nto
             !
             ! Search known boundaries for match
             !
             if (bndname == nambnd(j)) then
                found = .true.
                exit
             endif
          enddo
          if (.not.found) then
             errmsg = 'Unknown boundary "'//trim(bndname)//'" in '//trim(filmor)
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
          !
          ! Read bed boundary condition for open boundary
          !
          call prop_get_integer(morbound_ptr, '*', 'IBedCond', morbnd(j)%icond)
          if (morbnd(j)%icond<0 .or. morbnd(j)%icond>5) then
             errmsg = 'Invalid bed boundary condition at "'//trim(bndname)//'" in '//trim(filmor)
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
       enddo
       !
       ! Numerical settings
       !
       call prop_get_logical(mor_ptr, 'Numerics', 'UpwindBedload', mornum%upwindbedload)
       call prop_get_logical(mor_ptr, 'Numerics', 'LaterallyAveragedBedload', mornum%laterallyaveragedbedload)
       call prop_get_logical(mor_ptr, 'Numerics', 'MaximumWaterdepth', mornum%maximumwaterdepth)
       fluxlimstring = ' '
       call prop_get_string(mor_ptr, 'Numerics', 'FluxLimiter', fluxlimstring)       
       call str_lower(fluxlimstring)
       select case(fluxlimstring)
           case('minmod')
               mornum%fluxlim = FLUX_LIMITER_MINMOD  
           case('mc')     
               mornum%fluxlim = FLUX_LIMITER_MC  
           case default 
               mornum%fluxlim = FLUX_LIMITER_NONE  
       end select
       !
       ! Output options
       !
       call prop_get_logical(mor_ptr, 'Output', 'VelocAtZeta', moroutput%uuuvvv)
       call prop_get_logical(mor_ptr, 'Output', 'VelocMagAtZeta', moroutput%umod)
       call prop_get_logical(mor_ptr, 'Output', 'VelocZAtZeta', moroutput%zumod)
       call prop_get_logical(mor_ptr, 'Output', 'ShearVeloc', moroutput%ustar)
       !
       call prop_get_integer(mor_ptr, 'Output', 'TranspType',moroutput%transptype)
       if (moroutput%transptype < 0 .or. moroutput%transptype > 2) then
          errmsg = 'Invalid transport type in '//trim(filmor)
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       call prop_get_logical(mor_ptr, 'Output', 'BedTranspDueToCurrentsAtZeta', moroutput%sbcuv)
       call prop_get_logical(mor_ptr, 'Output', 'BedTranspDueToCurrentsAtFlux', moroutput%sbcuuvv)
       call prop_get_logical(mor_ptr, 'Output', 'BedTranspDueToWavesAtZeta'   , moroutput%sbwuv)
       call prop_get_logical(mor_ptr, 'Output', 'BedTranspDueToWavesAtFlux'   , moroutput%sbwuuvv)
       call prop_get_logical(mor_ptr, 'Output', 'SuspTranspDueToWavesAtZeta'  , moroutput%sswuv)
       call prop_get_logical(mor_ptr, 'Output', 'SuspTranspDueToWavesAtFlux'  , moroutput%sswuuvv)
       call prop_get_logical(mor_ptr, 'Output', 'NearBedTranspCorrAtFlux'     , moroutput%suvcor)
       call prop_get_logical(mor_ptr, 'Output', 'SourceSinkTerms'             , moroutput%sourcesink)
       call prop_get_logical(mor_ptr, 'Output', 'ReferenceHeight'             , moroutput%aks)
       !
       call prop_get_logical(mor_ptr, 'Output', 'Bedslope'                    , moroutput%dzduuvv)
       call prop_get_logical(mor_ptr, 'Output', 'Taurat'                      , moroutput%taurat)
       !
       call prop_get_logical(mor_ptr, 'Output', 'Dm'                          , moroutput%dm)
       call prop_get_logical(mor_ptr, 'Output', 'Dg'                          , moroutput%dg)
       call prop_get_logical(mor_ptr, 'Output', 'Dgsd'                        , moroutput%dgsd)
       call prop_get_logical(mor_ptr, 'Output', 'Frac'                        , moroutput%frac)
       call prop_get_logical(mor_ptr, 'Output', 'MudFrac'                     , moroutput%mudfrac)
       call prop_get_logical(mor_ptr, 'Output', 'SandFrac'                    , moroutput%sandfrac)
       call prop_get_logical(mor_ptr, 'Output', 'FixFac'                      , moroutput%fixfac)
       call prop_get_logical(mor_ptr, 'Output', 'HidExp'                      , moroutput%hidexp)
       !
       call prop_get_logical(mor_ptr, 'Output', 'AverageAtEachOutputTime'     , moroutput%cumavg)
       !
       ! Sediment percentiles
       ! Requested by the user via the .mor attribute file
       !
       pxxstr = ' '
       call prop_get_string(mor_ptr, 'Output', 'Percentiles', pxxstr)
       lenc = 999
       call str_lower(pxxstr, lenc)
       call remove_leading_spaces(pxxstr, lenc)
       if (pxxstr == ' ' .or. pxxstr == 'false') then
          pxxstr = ' '
          moroutput%percentiles = .false.
       elseif (pxxstr == 'true') then
          pxxstr = ' '
          moroutput%percentiles = .true.
       else
          moroutput%percentiles = .true.
       endif
       !
       ! Process string
       !
       call scannr( pxxstr,       1,      len(pxxstr), nxxuser,   itype, &
                 &  ifield,  rfield,  cfield,  lenchr,  maxfld,  .true., &
                 & .false., .false.)
       if (nxxuser < 0) then
          errmsg = 'Cannot interpret Percentiles string in '//trim(filmor)
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       do i = 1, nxxuser
          if (itype(i) == 1) then
             rfield(i) = ifield(i)
          elseif (itype(i) == 3) then
             errmsg = 'Cannot interpret Percentiles string in '//trim(filmor)
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
       enddo
    else
       call remove_leading_spaces(filmor    ,lfile     )
       !
       inquire (file = filmor(1:lfile), exist = ex)
       if (ex) then
          ilun = newunit()
          open (ilun, file = filmor(1:lfile), form = 'formatted',               &
              & status = 'old')
          !
          version = 0
          lenc    = 999
          read (ilun, '(a)') string
          do while (string(:1) == '*')
             call str_lower(string, lenc)
             i = index(string, 'version')
             if (i /= 0) then
                read (string(i + 8:), '(i2)') version
             endif
             read (ilun, '(a)') string
          enddo
          write (versionstring, '(i4)') version
          !
          rewind (ilun)
          call skipstarlines(ilun)
          !
          if (version == 0) then
             call rdmor0(ilun      ,morfac    ,tmor      ,thresh    ,bedupd    , &
                       & eqmbcsand ,densin    ,aksfac    ,rwave     ,rouse     , &
                       & alfabs    ,alfabn    ,sus       ,bed       ,susw      , &
                       & bedw      ,sedthr    ,thetsd    ,hmaxth    ,fwfac     )
          else
             call rdmor1(ilun      ,morfac    ,tmor      ,thresh    ,bedupd    , &
                       & eqmbcsand ,densin    ,aksfac    ,rwave     ,alfabs    , &
                       & alfabn    ,sus       ,bed       ,susw      ,bedw      , &
                       & sedthr    ,thetsd    ,hmaxth    ,fwfac     ,epspar    , &
                       & iopkcw    ,rdc       ,rdw       )
          endif
          cmpupd = .true.
          close (ilun)
       else
          !
          ! file not found
          !
          call write_error(FILE_NOT_FOUND//trim(filmor), unit=lundia)
       endif
    endif
    !
 8888 continue
    !
    ! Remove double percentiles
    !
    i = 0
    do while (i < nxxuser)
       i = i+1
       j = 0
       do while (j < nxxprog)
          j = j+1
          if (abs(rfield(i)-xxprog(j)) < 0.01) then
             rfield(i) = xxprog(j)
             do jj = j+1, nxxprog
                xxprog(jj-1) = xxprog(jj)
             enddo
             nxxprog = nxxprog - 1
          endif
       enddo
    enddo
    nxx = nxxuser + nxxprog
    !
    ! allocate memory for percentiles
    !
    allocate (morpar%xx(nxx), stat = istat)
    !
    if (istat /= 0) then
       errmsg = 'RDMOR: memory alloc error'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
    endif
    !
    ! Update local pointer
    !
    xx => morpar%xx
    !
    ! Copy and sort percentiles
    !
    do i = 1, nxx
       jmin = -1
       xxmin = 100.0_fp
       ilist = 1
       do j = 1, nxxuser
          if (rfield(j) < xxmin) then
             xxmin = rfield(j)
             jmin  = j
          endif
       enddo
       !
       do j = 1, nxxprog
          if (xxprog(j) < xxmin) then
             xxmin = xxprog(j)
             jmin  = j
             ilist = 2
          endif
       enddo
       !
       if (xxmin <= 0.0_fp .or. xxmin >= 100.0_fp) then
          errmsg = 'Sediment diameter percentiles should lie between 0 and 100%'
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       xx(i) = xxmin / 100.0_fp
       !
       if (abs(xxmin-10.0_fp) < 0.01_fp) i10 = i
       if (abs(xxmin-50.0_fp) < 0.01_fp) i50 = i
       if (abs(xxmin-90.0_fp) < 0.01_fp) i90 = i
       !
       if (ilist == 1) then
          rfield(jmin) = 200.0_fp
       else ! ilist==2
          xxprog(jmin) = 200.0_fp
       endif
    enddo
    !
    if (anymud) then
       call rdflufflyr(lundia    ,error    ,filmor    ,lsed    , &
                     & mor_ptr   ,morpar%flufflyr     ,sedpar  , &
                     & griddim   )
    endif
    !
    call rdmorlyr (lundia    ,error     ,filmor    , &
                 & nmaxus    ,nto       , &
                 & nambnd    ,version   ,lsedtot   ,namsed    , &
                 & morpar    ,morlyr    ,sedpar    ,mor_ptr   , &
                 & griddim   )
    !
    deallocate(itype)
    deallocate(ifield)
    deallocate(lenchr)
    deallocate(rfield)
    deallocate(cfield)
    deallocate(xxprog)
end subroutine rdmor


subroutine rdmor0(ilun      ,morfac    ,tmor      ,thresh    ,morupd    , &
                & eqmbc     ,densin    ,aksfac    ,rwave     ,rouse     , &
                & alfabs    ,alfabn    ,sus       ,bed       ,susw      , &
                & bedw      ,sedthr    ,thetsd    ,hmaxth    ,fwfac     )
!!--description-----------------------------------------------------------------
!
! Reads morphology input version 0 (or no version number found)
!
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Call variables
!
    integer, intent(in)  :: ilun
    logical, intent(out) :: densin !  Description and declaration in morpar.igs
    logical, intent(out) :: eqmbc !  Description and declaration in morpar.igs
    logical, intent(out) :: morupd !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: aksfac !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: alfabn !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: alfabs !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: bed !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: bedw !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: fwfac !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: hmaxth !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: morfac !  Description and declaration in morpar.igs
    logical, intent(out)    :: rouse !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: rwave !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: sedthr !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: sus !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: susw !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: thetsd !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: thresh !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: tmor !  Description and declaration in morpar.igs
!
!
!! executable statements -------------------------------------------------------
!
    ! morphological timescale factor
    read (ilun, *) morfac
    ! start for calculating morphological changes
    read (ilun, *) tmor
    ! calculation of ITMOR has been moved to TRICOM
    ! threshold value for slowing erosion near a fixed layer (m)
    read (ilun, *) thresh
    ! flag for doing bottom updates
    read (ilun, *) morupd
    ! flag for setting equilibrium sediment concentration profiles
    ! at the open boundaries for sand sediment
    read (ilun, *) eqmbc
    ! flag for including sediment in fluid density calculations
    read (ilun, *) densin
    ! factor for setting aks height
    read (ilun, *) aksfac
    ! factor for calculating wave-related roughness from ripple dimensions
    read (ilun, *) rwave
    ! flag for setting equilibrium concentrations to standard Rouse profiles
    read (ilun, *) rouse
    ! factor for longitudinal bed load transport
    read (ilun, *) alfabs
    ! factor for transverse bed load transport
    read (ilun, *) alfabn
    ! factor for calculating suspended load transport
    read (ilun, *) sus
    ! factor for calculating bed load transport
    read (ilun, *) bed
    ! wave-related suspended sediment factor
    read (ilun, *) susw
    ! wave-related bed-load sediment factor
    read (ilun, *) bedw
    ! minimum depth for sediment calculations
    read (ilun, *) sedthr
    ! global / maximum dry cell erosion factor
    read (ilun, *) thetsd
    ! maximum depth for variable dry cell erosion factor
    read (ilun, *) hmaxth
    ! factor for adjusting intensity of energy dissipation in wave boundary layer
    read (ilun, *) fwfac
end subroutine rdmor0


subroutine rdmor1(ilun      ,morfac    ,tmor      ,thresh    ,morupd    , &
                & eqmbc     ,densin    ,aksfac    ,rwave     ,alfabs    , &
                & alfabn    ,sus       ,bed       ,susw      ,bedw      , &
                & sedthr    ,thetsd    ,hmaxth    ,fwfac     ,epspar    , &
                & iopkcw    ,rdc       ,rdw       )
!!--description-----------------------------------------------------------------
!
! Read  morphology input version 1
! The first line in the file must be:
! * version 1
!
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Call variables
!
    integer, intent(in)  :: ilun
    integer, intent(out) :: iopkcw
    logical, intent(out) :: densin !  Description and declaration in morpar.igs
    logical, intent(out) :: epspar
    logical, intent(out) :: eqmbc !  Description and declaration in morpar.igs
    logical, intent(out) :: morupd !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: aksfac !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: alfabn !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: alfabs !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: bed !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: bedw !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: fwfac !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: hmaxth !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: morfac !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: rdc
    real(fp), intent(out)    :: rdw
    real(fp), intent(out)    :: rwave !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: sedthr !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: sus !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: susw !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: thetsd !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: thresh !  Description and declaration in morpar.igs
    real(fp), intent(out)    :: tmor !  Description and declaration in morpar.igs
!
!
! Local variables
!
    integer       :: iost
    character(78) :: string
!
!
!! executable statements -------------------------------------------------------
!
    ! morphological timescale factor
    read (ilun, *) morfac
    ! start for calculating morphological changes
    read (ilun, *) tmor
    ! calculation of ITMOR has been moved to TRICOM
    ! threshold value for slowing erosion near a fixed layer (m)
    read (ilun, *) thresh
    ! flag for doing bottom updates
    read (ilun, *) morupd
    ! flag for setting equilibrium sediment concentration profiles
    ! at the open boundaries for sand sediment
    read (ilun, *) eqmbc
    ! flag for including sediment in fluid density calculations
    read (ilun, *) densin
    ! factor for setting aks height
    read (ilun, *) aksfac
    ! factor for calculating wave-related roughness from ripple dimensions
    read (ilun, *) rwave
    !
    ! IF Rouse skip
    !
    read (ilun, '(A)') string
    call small(string, len(string))
    if (index(string, 'rouse')==0) backspace (ilun)
    ! factor for longitudinal bed load transport
    read (ilun, *) alfabs
    ! factor for transverse bed load transport
    read (ilun, *) alfabn
    ! factor for calculating suspended load transport
    read (ilun, *) sus
    ! factor for calculating bed load transport
    read (ilun, *) bed
    ! wave-related suspended sediment factor
    read (ilun, *) susw
    ! wave-related bed-load sediment factor
    read (ilun, *) bedw
    ! minimum depth for sediment calculations
    read (ilun, *) sedthr
    ! global / maximum dry cell erosion factor
    read (ilun, *) thetsd
    ! maximum depth for variable dry cell erosion factor
    read (ilun, *) hmaxth
    ! factor for adjusting intensity of energy dissipation in wave boundary layer
    read (ilun, *) fwfac
    ! flag for parametric epsilon distribution in case of K-Eps model
    read (ilun, *) epspar
    !
    read (ilun, *, iostat = iost) iopkcw, rdc, rdw
    ! IOSTAT to prevent problems if rdc and rdw are not given
end subroutine rdmor1


subroutine echomor(lundia    ,error     ,lsec      ,lsedtot   ,nto       , &
                 & nambnd    ,sedpar    ,morpar    )
!!--description-----------------------------------------------------------------
!
! Report morphology settings to diag file
!
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use table_handles
    use morphology_data_module
    use sediment_basics_module
    use message_module, only: write_error
    !
    implicit none
    !
    ! The following list of pointer parameters is used to point inside the data structures
    !
    integer                                , pointer :: subiw
    integer                                , pointer :: i10
    integer                                , pointer :: i50
    integer                                , pointer :: i90
    integer                                , pointer :: ihidexp
    integer                                , pointer :: itmor
    integer                                , pointer :: iopkcw
    integer                                , pointer :: islope
    integer                                , pointer :: morfacpar
    integer                                , pointer :: morfacrec
    integer                                , pointer :: morfactable
    integer                                , pointer :: nxx
    integer                                , pointer :: nmudfrac
    integer                , dimension(:)  , pointer :: sedtyp
    real(fp)                               , pointer :: morfac
    real(fp)                               , pointer :: thresh
    real(fp)                               , pointer :: aksfac
    real(fp)                               , pointer :: rwave
    real(fp)                               , pointer :: alfabs
    real(fp)                               , pointer :: alfabn
    real(fp)                               , pointer :: camax
    real(fp)                               , pointer :: dzmax
    real(fp)                               , pointer :: sus
    real(fp)                               , pointer :: bed
    real(fp)                               , pointer :: tmor
    real(fp)                               , pointer :: thetsd
    real(fp)                               , pointer :: susw
    real(fp)                               , pointer :: sedthr
    real(fp)                               , pointer :: hmaxth
    real(fp)                               , pointer :: bedw
    real(fp)                               , pointer :: rdc
    real(fp)                               , pointer :: rdw
    real(fp)                               , pointer :: espir
    real(fp)                               , pointer :: ashld
    real(fp)                               , pointer :: bshld
    real(fp)                               , pointer :: cshld
    real(fp)                               , pointer :: dshld
    real(fp)                               , pointer :: coulfri
    real(fp)                               , pointer :: flfdrat
    real(fp)                               , pointer :: alfpa
    real(fp)                               , pointer :: thcrpa
    real(fp)                               , pointer :: asklhe
    real(fp)                               , pointer :: mwwjhe
    real(fp)                               , pointer :: pangle
    real(fp)                               , pointer :: fpco
    real(fp)                               , pointer :: factcr
    real(fp)                               , pointer :: wetslope
    real(fp)                               , pointer :: avaltime
    real(fp)              , dimension(:)   , pointer :: xx
    logical                                , pointer :: bedupd
    logical                                , pointer :: cmpupd
    logical                                , pointer :: eqmbcsand
    logical                                , pointer :: eqmbcmud
    logical                                , pointer :: densin
    logical                                , pointer :: rouse
    logical                                , pointer :: epspar
    logical                                , pointer :: updinf
    logical                                , pointer :: neglectentrainment
    logical                                , pointer :: oldmudfrac
    logical                                , pointer :: varyingmorfac
    logical                                , pointer :: multi
    logical                                , pointer :: eulerisoglm
    logical                                , pointer :: glmisoeuler
    character(256)                         , pointer :: bcmfilnam
    character(20)          , dimension(:)  , pointer :: namsed
    type(handletype)                       , pointer :: bcmfile
    type(handletype)                       , pointer :: morfacfile
    type(moroutputtype)                    , pointer :: moroutput
    type(mornumericstype)                  , pointer :: mornum
    type(bedbndtype)       , dimension(:)  , pointer :: morbnd
!
! Local parameters
!
    integer, parameter :: maxfld     = 20
    integer, parameter :: maxfldprog = 10
!
! Call variables
!
    integer                        , intent(in)  :: nto
    integer                                      :: lundia  !  Description and declaration in inout.igs
    integer                        , intent(in)  :: lsec
    integer                        , intent(in)  :: lsedtot !  Description and declaration in iidim.f90
    logical                        , intent(out) :: error
    character(20) , dimension(nto)               :: nambnd  !  Description and declaration in ckdim.f90
    type(sedpar_type)              , pointer     :: sedpar
    type(morpar_type)              , pointer     :: morpar
!
! Local variables
!
    integer                                                           :: i
    integer                                                           :: ibndtyp
    integer                                                           :: istat
    integer                                                           :: j
    integer                                                           :: l
    integer                                                           :: nval
    character(20)                                                     :: parname
    character(40)                                                     :: txtput1
    character(20)                                                     :: txtput2
    character(120)                                                    :: txtput3
    character(256)                                                    :: errmsg
    character(MAXTABLECLENGTH) , dimension(:)               , pointer :: parnames
!
!! executable statements -------------------------------------------------------
!
    !
    ! Let local variables point to fields within data structures
    !
    morfac              => morpar%morfac
    thresh              => morpar%thresh
    aksfac              => morpar%aksfac
    rwave               => morpar%rwave
    alfabs              => morpar%alfabs
    alfabn              => morpar%alfabn
    camax               => morpar%camax
    dzmax               => morpar%dzmax
    sus                 => morpar%sus
    bed                 => morpar%bed
    tmor                => morpar%tmor
    thetsd              => morpar%thetsd
    susw                => morpar%susw
    sedthr              => morpar%sedthr
    hmaxth              => morpar%hmaxth
    bedw                => morpar%bedw
    rdc                 => morpar%rdc
    rdw                 => morpar%rdw
    espir               => morpar%espir
    ashld               => morpar%ashld
    bshld               => morpar%bshld
    cshld               => morpar%cshld
    dshld               => morpar%dshld
    coulfri             => morpar%coulfri
    flfdrat             => morpar%flfdrat
    alfpa               => morpar%alfpa
    thcrpa              => morpar%thcrpa
    asklhe              => morpar%asklhe
    mwwjhe              => morpar%mwwjhe
    i10                 => morpar%i10
    i50                 => morpar%i50
    i90                 => morpar%i90
    ihidexp             => morpar%ihidexp
    itmor               => morpar%itmor
    iopkcw              => morpar%iopkcw
    islope              => morpar%islope
    morfacpar           => morpar%morfacpar
    morfacrec           => morpar%morfacrec
    morfactable         => morpar%morfactable
    nxx                 => morpar%nxx
    bcmfile             => morpar%bcmfile
    morfacfile          => morpar%morfacfile
    moroutput           => morpar%moroutput
    mornum              => morpar%mornum
    morbnd              => morpar%morbnd
    xx                  => morpar%xx
    bedupd              => morpar%bedupd
    cmpupd              => morpar%cmpupd
    eqmbcsand           => morpar%eqmbcsand
    eqmbcmud            => morpar%eqmbcmud
    densin              => morpar%densin
    rouse               => morpar%rouse
    epspar              => morpar%epspar
    updinf              => morpar%updinf
    neglectentrainment  => morpar%neglectentrainment
    oldmudfrac          => morpar%oldmudfrac
    varyingmorfac       => morpar%varyingmorfac
    multi               => morpar%multi
    bcmfilnam           => morpar%bcmfilnam
    nmudfrac            => sedpar%nmudfrac
    namsed              => sedpar%namsed
    sedtyp              => sedpar%sedtyp
    pangle              => morpar%pangle
    fpco                => morpar%fpco
    factcr              => morpar%factcr
    wetslope            => morpar%wetslope
    avaltime            => morpar%avaltime
    subiw               => morpar%subiw
    eulerisoglm         => morpar%eulerisoglm
    glmisoeuler         => morpar%glmisoeuler
    !
    ! output values to file
    !
    write (lundia, '(a)' ) '*** Start  of morphological input'
    !write (lundia, '(2a)') '    Morphology File Version: ', trim(versionstring)
    txtput1 = 'Morphological Timescale Factor'
    if (varyingmorfac) then
       write (lundia, '(3a)') txtput1, ': ', trim(getfilename(morfacfile))
       call gettable(morfacfile, ' ' , 'MorFac'  , &
                   & morfactable, morfacpar , nval, 1, errmsg    )
       if (errmsg /= ' ') then
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       if (nval /= 1) then
          write(errmsg,'(i3,3a)') nval, ' MorFac parameters specified in file ', &
                                & trim(getfilename(morfacfile)), 'instead of 1.'
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       call checktable(morfacfile , morfactable    , &
                         & morfacpar  , 1              , &
                         & CHKTAB_POSITIVE+CHKTAB_BLOCK, &
                     & errmsg     )
       if (errmsg /= ' ') then
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       morfacrec = 1
    else
       write (lundia, '(2a,e20.4)') txtput1, ':', morfac
    endif
    txtput1 = 'Morphological Changes Start Time (min)'
    write (lundia, '(2a,e20.4)') txtput1, ':', tmor
    txtput1 = 'Fixed Layer Erosion Threshold'
    write (lundia, '(2a,e20.4)') txtput1, ':', thresh
    !
    txtput1 = 'Bed level updating  '
    if (bedupd) then
       txtput2 = '              ACTIVE'
    else
       txtput2 = '            INACTIVE'
    endif
    write (lundia, '(3a)') txtput1, ':', txtput2
    txtput1 = 'Composition updating'
    if (cmpupd) then
       txtput2 = '              ACTIVE'
    else
       txtput2 = '            INACTIVE'
    endif
    write (lundia, '(3a)') txtput1, ':', txtput2
    txtput1 = 'Entrainment/deposition flux in mass bal.'
    if (neglectentrainment) then
       txtput2 = '           NEGLECTED'
    else
       txtput2 = '            INCLUDED'
    endif
    write (lundia, '(3a)') txtput1, ':', txtput2
    !
    txtput1 = 'Sand Equili. conc. profs. at boundaries'
    if (eqmbcsand) then
       txtput2 = '                USED'
    else
       txtput2 = '            NOT USED'
    endif
    write (lundia, '(3a)') txtput1, ':', txtput2
    !
    txtput1 = 'Mud  Equili. conc. profs. at boundaries'
    if (eqmbcmud) then
       txtput2 = '                USED'
    else
       txtput2 = '            NOT USED'
    endif
    write (lundia, '(3a)') txtput1, ':', txtput2
    !
    txtput1 = 'Sediment included in fluid density calc.'
    if (densin) then
       txtput2 = '                 YES'
    else
       txtput2 = '                  NO'
    endif
    write (lundia, '(3a)') txtput1, ':', txtput2
    txtput1 = 'AKSFAC'
    write (lundia, '(2a,e20.4)') txtput1, ':', aksfac
    txtput1 = 'RWAVE'
    write (lundia, '(2a,e20.4)') txtput1, ':', rwave
    txtput1 = 'Equilibrium sed. conc. profiles'
    if (rouse) then
       txtput2 = '      Rouse profiles'
    else
       txtput2 = 'calculated (D3D mix)'
    endif
    write (lundia, '(3a)') txtput1, ':', txtput2
    txtput1 = 'Suspended sed. multiplication factor'
    write (lundia, '(2a,e20.4)') txtput1, ':', sus
    txtput1 = 'Bed load transp. multiplication factor'
    write (lundia, '(2a,e20.4)') txtput1, ':', bed
    txtput1 = 'wave-rel. susp.sed.transp.fact.(SUSW)'
    write (lundia, '(2a,e20.4)') txtput1, ':', susw
    txtput1 = 'wave-rel. bed-ld.sed.transp.fact.(BEDW)'
    write (lundia, '(2a,e20.4)') txtput1, ':', bedw
    txtput1 = 'Min.depth for sed. calculations(SEDTHR)'
    write (lundia, '(2a,e20.4)') txtput1, ':', sedthr
    txtput1 = 'Glob./max. dry cell erosion fact(THETSD)'
    write (lundia, '(2a,e20.4)') txtput1, ':', thetsd
    txtput1 = 'Max depth for variable THETSD (HMAXTH)'
    write (lundia, '(2a,e20.4)') txtput1, ':', hmaxth
    if (hmaxth<=sedthr) then
       txtput1 = 'CONSTANT THETSD for dry bank erosion'
    else
       txtput1 = 'Computing THETSD for dry bank erosion'
    endif
    write (lundia, '(a)') txtput1
    txtput1 = 'Tuning param. Shields Taucr (FACTCR)'
    write (lundia, '(2a,e20.4)') txtput1, ':', factcr
    txtput3 = 'Eulerian velocities i.s.o GLM velocities for' //       &
             & ' suspended transports'
    if (eulerisoglm) then
       txtput2 = '                 YES'
    else
       txtput2 = '                  NO'
    endif
    txtput3 = 'GLM velocities i.s.o Eulerian velocities for' //       &
             & ' bed load transport and reference concentrations'
    if (glmisoeuler) then
       txtput2 = '                 YES'
    else
       txtput2 = '                  NO'
    endif
    write (lundia, '(3a)') txtput3(1:82), ':', txtput2    
    txtput3 = 'EPSPAR: Always use Van Rijns param. mix. dist.'
    if (epspar) then
       txtput2 = '                 YES'
    else
       txtput2 = '                  NO'
    endif
    write (lundia, '(3a)') txtput3(1:47), ':', txtput2
    if (iopkcw == 1) then
       txtput3 = 'Standard option: Rc from Flow, Rw=RWAVE*0.025'
       write (lundia, '(2a,i20)') txtput3(1:46), ':', iopkcw
    else
       txtput1 = 'Constant Rc and Rw Prescribed'
       write (lundia, '(2a,2e20.4)') txtput1, ':', rdc, rdw
    endif
    !
    txtput1 = 'Update bed level at inflow boundaries'
    if (updinf) then
       txtput2 = '                 YES'
    else
       txtput2 = '                  NO'
    endif
    write (lundia, '(3a)') txtput1, ':', txtput2
    !
    txtput1 = 'Merge bottoms from parallel runs     '
    if (multi) then
       txtput2 = '                 YES'
    else
       txtput2 = '                  NO'
    endif
    write (lundia, '(3a)') txtput1, ':', txtput2
    !
    txtput1 = 'Source/sink limiter DZMAX(depth frac.)'
    write (lundia, '(2a,e20.4)') txtput1, ':', dzmax
    !
    txtput1 = 'Max. volumetric ref. conc. CAMAX'
    write (lundia, '(2a,e20.4)') txtput1, ':', camax
    !
    txtput1 = 'Hiding & exposure formulation'
    select case(ihidexp)
    case(1)
       txtput2 = '                None'
    case(2)
       txtput2 = '          Egiazaroff'
    case(3)
       txtput2 = '    Ashida & Michiue'
    case(4)
       txtput2 = '       Parker et.al.'
    case(5)
       txtput2 = '      Wu, Wang & Jia'
    case default
       txtput2 = '                   ?'
    endselect
    write (lundia, '(3a)') txtput1, ':', txtput2
    !
    if (lsec > 0) then
       txtput1 = 'Spiral flow factor'
       write (lundia, '(2a,e20.4)') txtput1, ':', espir
    endif
    !
    txtput1 = 'Bed slope effect formulation'
    select case(islope)
    case(1)
       txtput2 = '                None'
    case(2)
       txtput2 = '             Bagnold'
    case(3)
       txtput2 = '     Koch & Flokstra'
    case(4)
       txtput2 = '    Parker & Andrews'
    case default
       txtput2 = '                   ?'
    endselect
    write (lundia, '(3a)') txtput1, ':', txtput2
    select case(islope)
    case(2)
       txtput1 = 'ALFABS'
       write (lundia, '(2a,e20.4)') txtput1, ':', alfabs
       txtput1 = 'ALFABN'
       write (lundia, '(2a,e20.4)') txtput1, ':', alfabn
    case(3)
       txtput1 = 'ALFABS'
       write (lundia, '(2a,e20.4)') txtput1, ':', alfabs
       txtput1 = 'AShield parameter'
       write (lundia, '(2a,e20.4)') txtput1, ':', ashld
       txtput1 = 'BShield parameter'
       write (lundia, '(2a,e20.4)') txtput1, ':', bshld
       txtput1 = 'CShield parameter'
       write (lundia, '(2a,e20.4)') txtput1, ':', cshld
       txtput1 = 'DShield parameter'
       write (lundia, '(2a,e20.4)') txtput1, ':', dshld
    case(4)
       txtput1 = 'ALFABS'
       write (lundia, '(2a,e20.4)') txtput1, ':', alfabs
       txtput1 = 'Coulomb friction'
       write (lundia, '(2a,e20.4)') txtput1, ':', coulfri
       txtput1 = 'Lift/drag ratio'
       write (lundia, '(2a,e20.4)') txtput1, ':', flfdrat
       txtput1 = 'Critical Shields'
       write (lundia, '(2a,e20.4)') txtput1, ':', thcrpa
    case default
    endselect
    !
    if (wetslope<9.99_fp) then
       txtput1 = 'Maximum wet slope used for avalanching'
       write (lundia, '(2a,e20.4)') txtput1, ':', wetslope
       txtput1 = 'Time scale avalanching (in seconds)'
       write (lundia, '(2a,f20.0)') txtput1, ':', avaltime
    endif
    !
    ! User requested sediment percentiles
    !
    txtput1 = 'Requested percentile(s)'
    do i = 1, nxx
       write (lundia, '(2a,f20.4)') txtput1, ':', xx(i)*100.0
       txtput1 = ' '
    enddo
    !
    txtput1 = 'Output transport rates'
    select case(moroutput%transptype)
    case (0)
       txtput2 = '                mass'
    case (1)
       txtput2 = '  volume incl. pores'
    case (2)
       txtput2 = '  volume excl. pores'
    end select
    write (lundia, '(3a)') txtput1, ':', txtput2
    if (oldmudfrac) then
       txtput1 = 'Old mud source term calculation         '
       txtput2 = '                 YES'
       write (lundia, '(3a)') txtput1, ':', txtput2
    endif
    !
    ! errortrap in case user is using old morph.inp file
    !
    if (sus < 0.0_fp .or. bed < 0.0_fp) then
       error  = .true.
       errmsg = 'SUS or BED less than 0.0'
       call write_error(errmsg, unit=lundia)
    endif
    !
    ! errortrap THETSD
    !
    if (thetsd < 0.0_fp .or. thetsd > 1.0_fp) then
       error  = .true.
       errmsg = 'THETSD must be in range 0 - 1'
       call write_error(errmsg, unit=lundia)
    endif
    !
    allocate(parnames(lsedtot*2), stat = istat)
    if (istat /= 0) then
       errmsg = 'RDMOR: memory alloc error'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
    endif
    do j = 1, nto
       txtput1 = 'Boundary name'
       write (lundia, '(2a,a20)') txtput1, ':', trim(nambnd(j))
       !
       ! Display boundary conditions
       !
       txtput1 = '  Depth prescribed'
       select case(morbnd(j)%icond)
       case (0)
          txtput2 = '                free'
          parname = ' '
       case (1)
          txtput2 = '               fixed'
          parname = ' '
       case (2)
          txtput2 = '         time series'
          parname = 'depth               '
          ibndtyp = 1
          nval    = 1
       case (3)
          txtput1 = '  Depth change prescribed'
          txtput2 = '         time series'
          parname = 'depth change        '
          ibndtyp = 1
          nval    = 1
       case (4)
          txtput1 = '  Transport incl pores prescribed'
          txtput2 = '         time series'
          parname = 'transport incl pores'
          ibndtyp = 2
          nval    = lsedtot - nmudfrac
       case (5)
          txtput1 = '  Transport excl pores prescribed'
          txtput2 = '         time series'
          parname = 'transport excl pores'
          ibndtyp = 2
          nval    = lsedtot - nmudfrac
       end select
       write (lundia, '(3a)') txtput1, ':', txtput2
       !
       ! Check boundary conditions
       !
       if (parname /= ' ') then
          if (bcmfilnam /= ' ') then
             !
             ! Find entries in table
             !
             call gettable(bcmfile     ,nambnd(j) ,parname   , &
                    & morbnd(j)%ibcmt(1)   ,morbnd(j)%ibcmt(2)   , &
                & morbnd(j)%ibcmt(3)   ,1         ,errmsg    )
             if (errmsg /= ' ') then
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
             morbnd(j)%ibcmt(4) = 1
             txtput1 = '  Variation along boundary'
             !
             ! Check entries in table
             !
             if (morbnd(j)%ibcmt(3) == nval) then
                !
                ! Uniform values
                !
                txtput2 = '             uniform'
                write (lundia, '(3a)') txtput1, ':', txtput2
                if (ibndtyp == 1) then
                   parnames(1) = trim(parname)
                else
                   i = 0
                   do l = 1, lsedtot
                      if (sedtyp(l) /= SEDTYP_COHESIVE) then
                         i = i + 1
                         parnames(i) = trim(parname) // ' ' // trim(namsed(l))
                      endif
                   enddo
                endif
                !
                call checktableparnames(bcmfile   ,parnames  , &
                    & morbnd(j)%ibcmt(1)   ,morbnd(j)%ibcmt(2)   , &
                    & morbnd(j)%ibcmt(3)   ,errmsg    )
                if (errmsg /= ' ') then
                   call write_error(errmsg, unit=lundia)
                   error = .true.
                   return
                endif
             elseif (morbnd(j)%ibcmt(3) == nval*2) then
                !
                ! Values at "end A" and "end B"
                !
                txtput2 = '              linear'
                write (lundia, '(3a)') txtput1, ':', txtput2
                if (ibndtyp == 1) then
                   parnames(1) = trim(parname) // ' end A'
                   parnames(2) = trim(parname) // ' end B'
                else
                   i = 0
                   do l = 1, lsedtot
                      if (sedtyp(l) /= SEDTYP_COHESIVE) then
                         i = i + 1
                         parnames(i)      = trim(parname) // ' ' // trim(namsed(l)) // ' end A'
                         parnames(nval+i) = trim(parname) // ' ' // trim(namsed(l)) // ' end B'
                      endif
                   enddo
                endif
                !
                call checktableparnames(bcmfile   ,parnames  , &
                   & morbnd(j)%ibcmt(1)   , morbnd(j)%ibcmt(2)   , &
                   & morbnd(j)%ibcmt(3)   , errmsg    )
                if (errmsg /= ' ') then
                   call write_error(errmsg, unit=lundia)
                   error = .true.
                   return
                endif
             else
                !
                ! Invalid number of values specified
                !
                errmsg = 'Invalid number of parameters specified for ''' // &
                   & trim(parname) // ''' at ''' // nambnd(j) // ''' in ' // &
                   & trim(bcmfilnam)
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
          else
             errmsg = 'Missing input file for morphological boundary conditions'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
       endif
    enddo
    !
    write (lundia, '(a)') '*** End    of morphological input'
    write (lundia, *)
    !
    deallocate(parnames)
    !
    call echoflufflyr(lundia    ,error    ,morpar%flufflyr)
end subroutine echomor

subroutine rdflufflyr(lundia   ,error    ,filmor   ,lsed     ,mor_ptr ,flufflyr,sedpar ,griddim )
!!--description-----------------------------------------------------------------
!
! Read fluff layer parameters from an input file
!
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use sediment_basics_module, only: SEDTYP_COHESIVE
    use morphology_data_module
    use message_module, only: write_error !, write_warning, FILE_NOT_FOUND, FILE_READ_ERROR, PREMATURE_EOF
    use grid_dimens_module, only: griddimtype
    !
    implicit none
!
! Global variables
!
    character(*)                                           :: filmor
    type(fluffy_type)                        , pointer     :: flufflyr
    type(tree_data)                          , pointer     :: mor_ptr
    integer                                  , intent(in)  :: lsed     ! number of suspended fractions
    integer                                                :: lundia
    logical                                  , intent(out) :: error
    type(sedpar_type)                        , pointer     :: sedpar
    type(griddimtype)             , target   , intent(in)  :: griddim
!
! Local variables
!
    integer                         , pointer :: iflufflyr
    real(fp)      , dimension(:)    , pointer :: mfluni
    real(fp)      , dimension(:,:)  , pointer :: bfluff0
    real(fp)      , dimension(:,:)  , pointer :: bfluff1
    real(fp)      , dimension(:,:)  , pointer :: depfac
    character(256), dimension(:)    , pointer :: mflfil
    !
    integer                   :: i
    integer                   :: istat
    integer                   :: l
    integer                   :: nm
    integer                   :: nmlb
    integer                   :: nmub
    real(fp)                  :: rmissval
    logical                   :: ex
    logical                   :: success
    type(tree_data), pointer  :: sedblock_ptr
    character(256)            :: filfluff
    character(11)             :: fmttmp       ! Format file ('formatted  ') 
    character(256)            :: parname
    character(256)            :: errmsg
!
!! executable statements -------------------------------------------------------
!
    iflufflyr            => flufflyr%iflufflyr
    !
    error      = .false.
    rmissval   = -999.0
    fmttmp     = 'formatted'
    !
    call prop_get(mor_ptr, 'FluffLayer', 'Type', iflufflyr)
    if (iflufflyr==0) return
    !
    nmlb = griddim%nmlb
    nmub = griddim%nmub
    !
    istat = allocfluffy(flufflyr, lsed, nmlb, nmub)
    !
    if (istat/=0) then
       errmsg = 'RDFLUFFLYR: memory alloc error'
       call write_error(errmsg, unit=lundia)
       error = .true.
       return
    endif
    !
    mfluni  => flufflyr%mfluni
    depfac  => flufflyr%depfac
    bfluff0 => flufflyr%bfluff0
    bfluff1 => flufflyr%bfluff1
    mflfil  => flufflyr%mflfil
    !
    mfluni = 0.0_fp
    mflfil = ' '
    do l = 1,lsed
        sedblock_ptr => sedpar%sedblock(l)
        if (sedpar%sedtyp(l) == SEDTYP_COHESIVE) then
            call prop_get(sedblock_ptr, '*', 'IniFluffMass', mflfil(l))
            !
            ! Intel 7.0 crashes on an inquire statement when file = ' '
            !
            if (mflfil(l) == ' ') mflfil(l) = 'dummyname'
            inquire (file = mflfil(l), exist = ex)
            !
            if (.not.ex) then
                call prop_get(sedblock_ptr, '*', 'IniFluffMass', mfluni(l))
            endif
        endif
    enddo
    !
    if (iflufflyr==1) then
        bfluff0 = 0.0_fp
        bfluff1 = 0.0_fp
        !
        ! Burial term 1 fluff layer constant in time:
        ! uniform or spatially varying value
        !
        filfluff = ''
        call prop_get_string(mor_ptr, 'FluffLayer', 'BurFluff0', filfluff)
        !
        ! Intel 7.0 crashes on an inquire statement when file = ' '
        !
        if (filfluff == ' ') filfluff = 'dummyname'
        inquire (file = filfluff, exist = ex)
        !
        if (ex) then
            !
            ! read data from file
            !
            call depfil(lundia    ,error     ,filfluff  ,fmttmp    , &
                      & bfluff0   ,lsed      ,1         ,griddim)
            if (error) then
                errmsg = 'Unable to read burial term 1 from ' // trim(filfluff)
                call write_error(errmsg, unit=lundia)
                return
            endif
            flufflyr%bfluff0_fil = filfluff
            !
            ! check input
            !
            do nm = nmlb, nmub
                if (bfluff0(1,nm) < 0.0_fp .and. bfluff0(1,nm) /= rmissval ) then
                    errmsg = 'Burial term 1 should be positive in ' // trim(filfluff)
                    call write_error(errmsg, unit=lundia)
                    return
                endif
            enddo
        else
            filfluff = ' '
            call prop_get(mor_ptr, 'FluffLayer', 'BurFluff0', bfluff0(1,1))
            if (bfluff0(1,1) < 0.0_fp) then
                errmsg = 'Burial term 1 should be positive in ' // trim(filmor)
                call write_error(errmsg, unit=lundia)
                return
            endif
            bfluff0(1,:) = bfluff0(1,1)
        endif
        do l = 2, lsed
            bfluff0(l,:) = bfluff0(1,:)
        enddo
        !
        ! Burial term 2 fluff layer constant in time:
        ! uniform or spatially varying value
        !
        filfluff = ''
        call prop_get_string(mor_ptr, 'FluffLayer', 'BurFluff1', filfluff)
        !
        ! Intel 7.0 crashes on an inquire statement when file = ' '
        !
        if (filfluff == ' ') filfluff = 'dummyname'
        inquire (file = filfluff, exist = ex)
        !
        if (ex) then
            !
            ! read data from file
            !
            call depfil(lundia    ,error     ,filfluff  ,fmttmp    , &
                      & bfluff1   ,lsed      ,1         ,griddim)
            if (error) then
                errmsg = 'Unable to read burial term 2 from ' // trim(filfluff)
                call write_error(errmsg, unit=lundia)
                return
            endif
            flufflyr%bfluff1_fil = filfluff
            !
            ! check input
            !
            do nm = nmlb, nmub
                if (bfluff1(1,nm) < 0.0_fp .and. bfluff1(1,nm) /= rmissval ) then
                    errmsg = 'Burial term 2 should be positive in ' // trim(filfluff)
                    call write_error(errmsg, unit=lundia)
                    return
                endif
            enddo
        else
            filfluff = ' '
            call prop_get(mor_ptr, 'FluffLayer', 'BurFluff1', bfluff1(1,1))
            if (bfluff1(1,1) < 0.0_fp ) then
                errmsg = 'Burial term 2 should be positive in ' // trim(filmor)
                call write_error(errmsg, unit=lundia)
                return
            endif
            bfluff1(1,:) = bfluff1(1,1)
        endif
        do l = 2, lsed
            bfluff1(l,:) = bfluff1(1,:)
        enddo
        !
    elseif (iflufflyr==2) then
        depfac = 0.0_fp   
        !
        ! Deposition factor constant in time:
        ! uniform or spatially varying value
        !
        filfluff = ''
        call prop_get_string(mor_ptr, 'FluffLayer', 'DepFac', filfluff)
        !
        ! Intel 7.0 crashes on an inquire statement when file = ' '
        !
        if (filfluff == ' ') filfluff = 'dummyname'
        inquire (file = filfluff, exist = ex)
        !
        if (ex) then
            !
            ! read data from file
            !
            call depfil(lundia    ,error     ,filfluff  ,fmttmp    , &
                      & depfac    ,lsed      ,1         ,griddim)
            if (error) then
                errmsg = 'Unable to read deposition factor from ' // trim(filfluff)
                call write_error(errmsg, unit=lundia)
                return
            endif
            flufflyr%depfac_fil = filfluff
            !
            ! check input
            !
            do nm = nmlb, nmub
                if ((depfac(1,nm) < 0.0_fp .or. depfac(1,nm)>1.0_fp) .and. depfac(1,nm) /= rmissval  ) then
                    errmsg = 'Deposition factor should be between 0 and 1 in ' // trim(filfluff)
                    call write_error(errmsg, unit=lundia)
                    return
                endif
            enddo
        else
            filfluff = ' '
            call prop_get(mor_ptr, 'FluffLayer', 'DepFac', depfac(1,1))
            if (depfac(1,1) < 0.0_fp .or. depfac(1,1) > 1.0_fp) then
                errmsg = 'Deposition factor should be between 0 and 1 in ' // trim(filmor)
                call write_error(errmsg, unit=lundia)
                return
            endif
            depfac(1,:) = depfac(1,1)
        endif
    endif
    do l = 2, lsed
        depfac(l,:) = depfac(1,:)
    enddo                                                                                                                                                             
end subroutine rdflufflyr

subroutine echoflufflyr(lundia    ,error    ,flufflyr)
!!--description-----------------------------------------------------------------
!
! Report fluff layer settings to diag file
!
!!--declarations----------------------------------------------------------------
    use precision
    use morphology_data_module
    !
    implicit none
!
! Global variables
!
    type(fluffy_type)                        , pointer     :: flufflyr
    integer                                                :: lundia
    logical                                  , intent(out) :: error
!
! Local variables
!
    integer         , pointer :: iflufflyr
    !
    character(30)             :: txtput1
    character(10)             :: txtput2
!
!! executable statements -------------------------------------------------------
!
    error      = .false.
    iflufflyr            => flufflyr%iflufflyr
    if (iflufflyr==0) return
    !
    write (lundia, '(a)')   '*** Start  of fluff layer input'
    txtput1 = 'Fluff layer mechanism'
    write (lundia, '(2a,i20)') txtput1, ':', iflufflyr
    !
    if (iflufflyr==1) then
        txtput1 = 'Burial coefficient 1'
        if (flufflyr%bfluff0_fil /= ' ') then
            write(lundia,'(3a)') txtput1, ':', trim(flufflyr%bfluff0_fil)
        else
            write(lundia,'(2a,e20.4)') txtput1, ':', flufflyr%bfluff0(1,1)
        endif
        !
        txtput1 = 'Burial coefficient 2'
        if (flufflyr%bfluff1_fil /= ' ') then
            write(lundia,'(3a)') txtput1, ':', trim(flufflyr%bfluff1_fil)
        else
            write(lundia,'(2a,e20.4)') txtput1, ':', flufflyr%bfluff1(1,1)
        endif
    elseif (iflufflyr==2) then
        txtput1 = 'Deposition factor'
        if (flufflyr%depfac_fil /= ' ') then
            write(lundia,'(3a)') txtput1, ':', trim(flufflyr%depfac_fil)
        else
            write(lundia,'(2a,e20.4)') txtput1, ':', flufflyr%depfac(1,1)
        endif
    endif
    !
    write (lundia, '(a)')   '*** End    of fluff layer input'
    write (lundia, *)
end subroutine echoflufflyr

end module m_rdmor
