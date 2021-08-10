module waqsim_module
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2020.                                
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
!  $Id: waqsim_module.F90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/morphology/packages/morphology_waq/src/waqsim_module.F90 $
!-------------------------------------------------------------------------------
    use precision
    use string_module, only: str_token
    use system_utils, only: split_filename, cat_filename
    implicit none
      
    integer, parameter :: wqprec=sp ! WAQ precision
    integer, parameter :: WAQSIM_GEOMETRY_CURVGRID = 1
    integer, parameter :: WAQSIM_GEOMETRY_TELEMAC  = 2
    integer, parameter :: WAQSIM_GEOMETRY_UNSTRUCT = 3
    integer, parameter :: WAQSIM_VERTDIFF_CALC = 1
!
type hydtype
    !
    ! pointers/allocatables
    !
    real(wqprec), dimension(:), allocatable :: hydrodynamic_layers
    real(wqprec), dimension(:), allocatable :: water_quality_layers
    !
    ! reals
    !
    real(wqprec), dimension(3) :: minimum_vert_diffusion
    real(wqprec), dimension(3) :: constant_dispersion
    !
    ! integers
    !
    integer               :: geometry                    = -999
    integer               :: vertical_diffusion          = -999
    integer, dimension(6) :: reference_time              = -999
    integer, dimension(6) :: hydrodynamic_start_time     = -999
    integer, dimension(6) :: hydrodynamic_stop_time      = -999
    integer, dimension(6) :: hydrodynamic_timestep       = -999
    integer, dimension(6) :: conversion_ref_time         = -999
    integer, dimension(6) :: conversion_start_time       = -999
    integer, dimension(6) :: conversion_stop_time        = -999
    integer, dimension(6) :: conversion_timestep         = -999
    integer               :: grid_cells_first_direction  = -999
    integer               :: grid_cells_second_direction = -999
    integer               :: number_horizontal_exchanges = -999
    integer               :: number_hydrodynamic_layers  = -999
    integer               :: number_water_quality_layers = -999
    !
    ! logicals
    !
    logical :: horizontal_aggregation      = .false.
    logical :: minimum_vert_diffusion_used = .false.
    logical :: discharges                  = .false.
    !
    ! characters
    !
    character(len=40) , dimension(3) :: description = ' '
    character(len=256) :: task                      = ' '
    character(len=256) :: hydrodynamic_file         = ' '
    character(len=256) :: aggregation_file          = ' '
    character(len=256) :: grid_indices_file         = ' '
    character(len=256) :: grid_coordinates_file     = ' '
    character(len=256) :: volumes_file              = ' '
    character(len=256) :: areas_file                = ' '
    character(len=256) :: flows_file                = ' '
    character(len=256) :: pointers_file             = ' '
    character(len=256) :: lengths_file              = ' '
    character(len=256) :: salinity_file             = ' '
    character(len=256) :: temperature_file          = ' '
    character(len=256) :: vert_diffusion_file       = ' '
    character(len=256) :: surfaces_file             = ' '
    character(len=256) :: total_grid_file           = ' '
    character(len=256) :: discharges_file           = ' '
    character(len=256) :: chezy_coefficients_file   = ' '
    character(len=256) :: shear_stresses_file       = ' '
    character(len=256) :: walking_discharges_file   = ' '
    character(len=256) :: attributes_file           = ' '
    character(len=256) :: depths_file               = ' '
    character(len=256) :: x_velocities_file         = ' '
    character(len=256) :: y_velocities_file         = ' '
end type hydtype
!
type waqsimtype
    !
    ! reals
    !
    real(wqprec), dimension(3)                   :: disp            ! array for uniform dispersion coefficients  
    !
    ! integers
    !
    integer                                      :: int_method   =0 ! integration option
    integer                                      :: nbnd         =0 ! number of boundaries
    integer                                      :: nprocess     =0 ! number of processes
    integer                                      :: npar         =0 ! number of constant process parameters
    integer                                      :: nfldex       =0 ! number of spatially varying process parameters at exchanges
    integer                                      :: nfld         =0 ! number of spatially varying process parameters (at segments)
    integer                                      :: nsubs        =0 ! total number of substances
    integer                                      :: nsubs_transp =0 ! number of substances to be transported
    integer                                      :: nsegatr      =0 ! number of segment attributes
    integer                                      :: start_time   =0 ! simulation start time
    integer                                      :: stop_time    =0 ! simulation stop time
    integer                                      :: time_step    =0 ! simulation time step
    integer                                      :: start_mon    =0 ! monitoring output start time
    integer                                      :: stop_mon     =0 ! monitoring output stop time
    integer                                      :: step_mon     =0 ! monitoring output time step
    integer                                      :: start_his    =0 ! history output start time
    integer                                      :: stop_his     =0 ! history output stop time
    integer                                      :: step_his     =0 ! history output time step
    integer                                      :: start_map    =0 ! map output start time
    integer                                      :: stop_map     =0 ! map output stop time
    integer                                      :: step_map     =0 ! map output time step
    integer                                      :: nsegments    =0 ! number of segments
    integer                                      :: noutput      =0 ! number of output quantities
    integer, dimension(4)                        :: nexchange    =0 ! number of exchanges
    integer, dimension(6)                        :: refdate      =0 ! year, month, day, hour, minute, second of reference date
    !
    ! pointers/allocatables
    !
    integer     , dimension(:)  , allocatable    :: segatrnr       ! segment attribute number
    integer     , dimension(:,:), allocatable    :: segatr         ! segment attributes
    real(wqprec), dimension(:), allocatable      :: subsval        ! substance initial scalar conditions in delwaq precision
    real(wqprec), dimension(:), allocatable      :: vol            ! volumes in delwaq precision
    real(wqprec), dimension(:), allocatable      :: area           ! areas in delwaq precision
    real(wqprec), dimension(:), allocatable      :: qag            ! flows in delwaq precision
    real(wqprec), dimension(:,:), allocatable    :: lng            ! length/distance array
    real(wqprec), dimension(:), allocatable      :: tmpval         ! temporary array for sending data to and retrieving data from delwaq
    !
    integer, dimension(:,:), allocatable         :: poi            ! from/to-pointer array
    !
    character(len=20), dimension(:), allocatable :: output         ! output parameter names
    character(len=20), dimension(:), allocatable :: process        ! process names
    character(len=20), dimension(:), allocatable :: procpar        ! process parameter names
    character(len=20), dimension(:), allocatable :: subsname       ! substance names
    logical, dimension(:), allocatable           :: substran       ! substance transportable flags
    integer, dimension(:), allocatable           :: subsmult       ! substance number of fractions
    character(len=20), dimension(:), allocatable :: parname
    real(wqprec), dimension(:), allocatable      :: parval
    character(len=20), dimension(:), allocatable :: fldexname
    real(wqprec), dimension(:,:), allocatable    :: fldexval
    character(len=20), dimension(:), allocatable :: fldname
    real(wqprec), dimension(:,:), allocatable    :: fldval
    !
    ! logicals
    !
    logical                                      :: disp_flow_zero ! dispersion active when flow is zero
    logical                                      :: disp_bound     ! dispersion active at open boundaries
    logical                                      :: first_order    ! lower order scheme at open boundaries
    logical                                      :: forester       ! forester filter active
    logical                                      :: anticreep      ! anti-creep active
    logical                                      :: initialized = .false.
    !
    ! characters
    !
    character(len=40) , dimension(3) :: description = ' '
end type waqsimtype

!interface waqsim_set
!   module procedure waqsim_setconst
!   module procedure waqsim_setfld
!end interface waqsim_set

interface dealloc
    module procedure dealloc_waqsim
end interface dealloc

contains

function poi_read(waqsim, filename) result (success)
!!--description-----------------------------------------------------------------
!
! XXXX
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Call variables
!
    type (waqsimtype)                                      :: waqsim
    character(*)                                           :: filename
    logical                                                :: success
!
! Local variables
!
   integer :: i
   integer :: istat
   integer :: lun
   integer, external :: newunit
!
!! executable statements -------------------------------------------------------
!
   success = .false.
   lun = newunit()
#ifdef HAVE_FC_FORM_BINARY
   open  ( lun , file=filename , form = 'binary' , SHARED )
#else
! standardized way if binary is not available
   open  ( lun , file=filename , form = 'unformatted' , access='stream' )
#endif
   read(lun) waqsim%poi
   close(lun)
   success = .true.
end function poi_read



function hyd_read(hyd_file, hyd) result (success)
!!--description-----------------------------------------------------------------
!
! XXXX
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Call variables
!
    type (hydtype)                                         :: hyd
    character(*)                                           :: hyd_file
    logical                                                :: success
!
! Local variables
!
   integer :: i
   integer :: istat
   integer :: lun
   integer, external :: newunit
   character(1024) :: line
   character(256)  :: path
   character(256)  :: file
   character(128)  :: token
!
!! executable statements -------------------------------------------------------
!
   success = .false.
   lun = newunit()
   call split_filename(hyd_file,path,file)
   open(lun, file = hyd_file)
   istat = 0
   !
   do while (.true.)
      read(lun,'(A)', end=10) line
      call str_token(line, token)
      select case (token)
         case ('task')
            call str_token(line,hyd%task)
         case ('geometry')
            call str_token(line,token)
            select case (token)
               case ('curvilinear-grid')
                  hyd%geometry = WAQSIM_GEOMETRY_CURVGRID
               case ('finite-elements')
                  hyd%geometry = WAQSIM_GEOMETRY_TELEMAC
               case ('unstructured')
                  hyd%geometry = WAQSIM_GEOMETRY_UNSTRUCT
            end select
         case ('horizontal-aggregation')
            call str_token(line,token)
            hyd%horizontal_aggregation = token == 'yes'
         case ('minimum-vert-diffusion-used')
            call str_token(line,token)
            hyd%minimum_vert_diffusion_used = token == 'yes'
         case ('vertical-diffusion')
            call str_token(line,token)
            select case (token)
               case ('calculated')
                  hyd%vertical_diffusion = WAQSIM_VERTDIFF_CALC
            end select
         case ('description')
            read(lun,'(A)', end=10) line
            call str_token(line,hyd%description(1),'''')
            read(lun,'(A)', end=10) line
            call str_token(line,hyd%description(2),'''')
            read(lun,'(A)', end=10) line
            call str_token(line,hyd%description(3),'''')
            read(lun,'(A)', end=10) line ! end-description
         case ('reference-time')
            call str_token(line,token,'''')
            call token2time(token,hyd%reference_time)
         case ('hydrodynamic-start-time')
            call str_token(line,token,'''')
            call token2time(token,hyd%hydrodynamic_start_time)
         case ('hydrodynamic-stop-time')
            call str_token(line,token,'''')
            call token2time(token,hyd%hydrodynamic_stop_time)
         case ('hydrodynamic-timestep')
            call str_token(line,token,'''')
            call token2time(token,hyd%hydrodynamic_timestep)
         case ('conversion-ref-time')
            call str_token(line,token,'''')
            call token2time(token,hyd%conversion_ref_time)
         case ('conversion-start-time')
            call str_token(line,token,'''')
            call token2time(token,hyd%conversion_start_time)
         case ('conversion-stop-time')
            call str_token(line,token,'''')
            call token2time(token,hyd%conversion_stop_time)
         case ('conversion-timestep')
            call str_token(line,token,'''')
            call token2time(token,hyd%conversion_timestep)
         case ('grid-cells-first-direction')
            read(line,'(I100)') hyd%grid_cells_first_direction
         case ('grid-cells-second-direction')
            read(line,'(I100)') hyd%grid_cells_second_direction
         case ('number-horizontal-exchanges')
            read(line,'(I100)') hyd%number_horizontal_exchanges
         case ('number-hydrodynamic-layers')
            read(line,'(I100)') hyd%number_hydrodynamic_layers
         case ('number-water-quality-layers')
            read(line,'(I100)') hyd%number_water_quality_layers    
         case ('hydrodynamic-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%hydrodynamic_file = cat_filename(path,file)
         case ('aggregation-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%aggregation_file = cat_filename(path,file)
         case ('grid-indices-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%grid_indices_file = cat_filename(path,file)
         case ('grid-coordinates-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%grid_coordinates_file = cat_filename(path,file)
         case ('volumes-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%volumes_file = cat_filename(path,file)
         case ('areas-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%areas_file = cat_filename(path,file)
         case ('flows-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%flows_file = cat_filename(path,file)
         case ('pointers-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%pointers_file = cat_filename(path,file)
         case ('lengths-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%lengths_file = cat_filename(path,file)
         case ('salinity-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%salinity_file = cat_filename(path,file)
         case ('temperature-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%temperature_file = cat_filename(path,file)
         case ('vert-diffusion-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%vert_diffusion_file = cat_filename(path,file)
         case ('surfaces-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%surfaces_file = cat_filename(path,file)
         case ('total-grid-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%total_grid_file = cat_filename(path,file)
         case ('discharges-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%discharges_file = cat_filename(path,file)
         case ('chezy-coefficients-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%chezy_coefficients_file = cat_filename(path,file)
         case ('shear-stresses-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%shear_stresses_file = cat_filename(path,file)
         case ('walking-discharges-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%walking_discharges_file = cat_filename(path,file)
         case ('attributes-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%attributes_file = cat_filename(path,file)
         case ('depths-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%depths_file = cat_filename(path,file)
         case ('x-velocities-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%x_velocities_file = cat_filename(path,file)
         case ('y-velocities-file')
            call str_token(line,file,'''')
            if (file == 'none') file = ' ' 
            hyd%y_velocities_file = cat_filename(path,file)
         case ('minimum-vert-diffusion')
            read(lun,'(A)', end=10) line
            call str_token(line,token)
            i = 1
            do while (token /= 'end-minimum-vert-diffusion')
               read(line,*) hyd%minimum_vert_diffusion(i)
               i = i+1
               read(lun,'(A)', end=10) line
               call str_token(line,token)
            end do
         case ('constant-dispersion')
            read(lun,'(A)', end=10) line
            call str_token(line,token)
            i = 1
            do while (token /= 'end-constant-dispersion')
               read(line,*) hyd%constant_dispersion(i)
               i = i+1
               read(lun,'(A)', end=10) line
               call str_token(line,token)
            end do
         case ('hydrodynamic-layers')
            allocate(hyd%hydrodynamic_layers(hyd%number_hydrodynamic_layers))
            do i = 1,hyd%number_hydrodynamic_layers
               read(lun,'(A)', end=10) line
               read(line,*) hyd%hydrodynamic_layers(i)
            enddo
            read(lun,'(A)', end=10) line ! end-hydrodynamic-layers
         case ('water-quality-layers')
            allocate(hyd%water_quality_layers(hyd%number_water_quality_layers))
            do i = 1,hyd%number_water_quality_layers
               read(lun,'(A)', end=10) line
               read(line,*) hyd%water_quality_layers(i)
            enddo
            read(lun,'(A)', end=10) line ! end-water-quality-layers
         case ('discharges')
            read(lun,'(A)', end=10) line
            call str_token(line,token)
            do while (token /= 'end-discharges')
               read(lun,'(A)', end=10) line
               call str_token(line,token)
            end do
         case default
             write(*,'(2A)') 'Skipping unknown token: ',trim(token)
      end select
   end do
10 continue
   !
   close(lun)
   success = .true.
end function hyd_read


subroutine token2time(token,time)
!!--description-----------------------------------------------------------------
!
! XXXX
!
!!--declarations----------------------------------------------------------------
    implicit none
!
! Call variables
!
    character(*)                                           :: token
    integer, dimension(6)                                  :: time
!
! Local variables
!
!   NONE
!
!! executable statements -------------------------------------------------------
!
   read(token,'(I4,I2,I2,I2,I2,I2)') time(1:6)
end subroutine token2time


!function waqsim_initialize(waqsim, procdef_file, sfrac_file, runid) result (success)
!!!--description-----------------------------------------------------------------
!!
!! XXXX
!!
!!!--pseudo code and references--------------------------------------------------
!! NONE
!!!--declarations----------------------------------------------------------------
!    implicit none
!!
!! Call variables
!!
!    type (waqsimtype)                        , target      :: waqsim
!    character(*)                                           :: procdef_file
!    character(*)                                           :: sfrac_file
!    logical                                                :: success
!    character(*)                             , optional    :: runid
!!
!! Local variables
!!
!    logical, external :: SetModel_Id
!    logical, external :: SetProcessDefinitionX
!    logical, external :: DefineWQSchematisation
!    logical, external :: DefineWQDispersion
!    logical, external :: SetInitialVolume
!    logical, external :: SetIntegrationOptions
!    logical, external :: SetReferenceDate
!    logical, external :: SetSimulationTimes
!    logical, external :: SetOutputTimers
!    logical, external :: DefineWQExtraOutputParameters
!    logical, external :: DefineWQProcessesX
!    logical, external :: SetAttributeInit
!    logical, external :: SetCurrentValueScalarInit
!    integer, external :: ModelInitialize
!!
!    integer           :: i
!    integer           :: j
!    integer           :: istat
!    character(len=20) :: name
!    character(len=4)  :: mode
!    character(3)      :: num      ! number string for fraction
!
!!
!!! executable statements -------------------------------------------------------
!!
!    mode = '-waq'
!    if (present(runid)) then
!       success = SetModel_Id(runid)
!    endif
!    success = SetProcessDefinitionX(mode, procdef_file, sfrac_file)
!    success = DefineWQSchematisation(waqsim%nsegments, waqsim%poi, waqsim%nexchange)
!    success = DefineWQDispersion(waqsim%disp,waqsim%lng)
!    success = SetInitialVolume(waqsim%vol)
!    success = SetIntegrationOptions(waqsim%int_method, waqsim%disp_flow_zero, waqsim%disp_bound, &
!                                  & waqsim%first_order, waqsim%forester, waqsim%anticreep)
!    success = SetReferenceDate(waqsim%refdate(1), waqsim%refdate(2), waqsim%refdate(3), &
!                             & waqsim%refdate(4), waqsim%refdate(5), waqsim%refdate(6))
!    success = SetSimulationTimes(waqsim%start_time, waqsim%stop_time, waqsim%time_step)
!    !success = SetOutputTimers(1,start_time,stop_time,time_step) ! Mon
!    success = SetOutputTimers(2,waqsim%start_his,waqsim%stop_his,waqsim%step_his) ! His
!    success = SetOutputTimers(3,waqsim%start_map,waqsim%stop_map,waqsim%step_map) ! Map
!    if (waqsim%noutput>0) then
!       success = DefineWQExtraOutputParameters(waqsim%output, waqsim%noutput)
!    endif
!    success = DefineWQProcessesX(waqsim%subsname, waqsim%subsmult, waqsim%nsubs, &
!                               & waqsim%nsubs_transp, waqsim%parname, waqsim%npar, &
!                               & waqsim%fldname, waqsim%nfld, waqsim%process, waqsim%nprocess)
!    do i = 1, waqsim%nsegatr
!       success = SetAttributeInit(i,waqsim%segatr(1,i))
!    enddo
!    do i = 1,waqsim%nsubs
!       if (waqsim%subsmult(i)==1) then
!          name = waqsim%subsname(i);
!          success = SetCurrentValueScalarInit(waqsim%subsname(i),waqsim%subsval(i))
!       else
!          do j = 1,waqsim%subsmult(i)
!             if (j<100) then
!                write(num(1:2),'(I2.2)') j
!             else
!                write(num(1:3),'(I3.3)') j
!             endif
!             name = trim(waqsim%subsname(i))//num
!             success = SetCurrentValueScalarInit(name,waqsim%subsval(i))
!          enddo
!       endif
!    enddo
!    do i = 1,waqsim%npar
!        success = SetCurrentValueScalarInit(waqsim%parname(i),waqsim%parval(i))
!    enddo
!    success = SetCurrentValueScalarInit('ZB',0.0_wqprec)
!    success = ModelInitialize()==0
!    waqsim%initialized = .true.
!end function waqsim_initialize
!
!
!function waqsim_step(waqsim) result (success)
!!!--description-----------------------------------------------------------------
!!
!! XXXX
!!
!!!--pseudo code and references--------------------------------------------------
!! NONE
!!!--declarations----------------------------------------------------------------
!    implicit none
!!
!! Call variables
!!
!    type (waqsimtype)                        , target      :: waqsim
!    logical                                                :: success
!!
!! Local variables
!!
!    logical, external :: SetFlowData
!    integer, external :: ModelPerformTimeStep
!!
!!! executable statements -------------------------------------------------------
!!
!    ! send new volumes, fluxes and areas
!    success = SetFlowData(waqsim%vol,waqsim%area,waqsim%qag)
!    ! perform waq step
!    success = ModelPerformTimeStep()==0
!    !call GetCurrentTime(x)
!end function waqsim_step
!
!
!function waqsim_setconst(waqsim, quant, val) result (success)
!!!--description-----------------------------------------------------------------
!!
!! XXXX
!!
!!!--pseudo code and references--------------------------------------------------
!! NONE
!!!--declarations----------------------------------------------------------------
!    implicit none
!!
!! Call variables
!!
!    type (waqsimtype)                                      :: waqsim
!    character(*)                             , intent(in)  :: quant
!    real(wqprec)                             , intent(in)  :: val
!    logical                                                :: success
!!
!! Local variables
!!
!    logical, external :: SetCurrentValueScalarRun
!!
!!! executable statements -------------------------------------------------------
!!
!    if (.not.waqsim%initialized) then
!       success = .false.
!    else
!       success = SetCurrentValueScalarRun(quant,val)
!    endif
!end function waqsim_setconst
!
!
!function waqsim_setfld(waqsim, quant, val) result (success)
!!!--description-----------------------------------------------------------------
!!
!! XXXX
!!
!!!--pseudo code and references--------------------------------------------------
!! NONE
!!!--declarations----------------------------------------------------------------
!    implicit none
!!
!! Call variables
!!
!    type (waqsimtype)                                      :: waqsim
!    character(*)                             , intent(in)  :: quant
!    real(wqprec), dimension(:)               , intent(in)  :: val
!    logical                                                :: success
!!
!! Local variables
!!
!    logical, external :: SetCurrentValueFieldRun
!    integer           :: len
!    integer           :: i
!!
!!! executable statements -------------------------------------------------------
!!
!    len = size(val,1)
!    if (.not.waqsim%initialized .or. len/=waqsim%nsegments) then
!       success = .false.
!    else
!       success = SetCurrentValueFieldRun(quant,val)
!    endif
!end function waqsim_setfld
!
!
!function waqsim_get(waqsim, quant, val) result (success)
!!!--description-----------------------------------------------------------------
!!
!! XXXX
!!
!!!--pseudo code and references--------------------------------------------------
!! NONE
!!!--declarations----------------------------------------------------------------
!    implicit none
!!
!! Call variables
!!
!    type (waqsimtype)                                      :: waqsim
!    character(*)                             , intent(in)  :: quant
!    real(wqprec), dimension(:)               , intent(in)  :: val
!    logical                                                :: success
!!
!! Local variables
!!
!    logical, external :: GetCurrentValue
!    integer           :: len
!    integer           :: i
!!
!!! executable statements -------------------------------------------------------
!!
!    len = size(val,1)
!    if (.not.waqsim%initialized .or. len/=waqsim%nsegments) then
!       success = .false.
!    else
!       success = GetCurrentValue(quant,val)
!    endif
!end function waqsim_get
!
!function waqsim_finalize(waqsim) result (success)
!!!--description-----------------------------------------------------------------
!!
!! XXXX
!!
!!!--pseudo code and references--------------------------------------------------
!! NONE
!!!--declarations----------------------------------------------------------------
!    implicit none
!!
!! Call variables
!!
!    type (waqsimtype)                        , target      :: waqsim
!    logical                                                :: success
!!
!! Local variables
!!
!    integer, external :: ModelFinalize
!!
!!! executable statements -------------------------------------------------------
!!
!    ! perform waq finalize
!    success = ModelFinalize()==0
!end function waqsim_finalize


subroutine waqsim_writeinp(waqsim, filename, hyd)
!!--description-----------------------------------------------------------------
!
! Convert morphology ini properties to waq process definition
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use time_module, only: sec2ddhhmmss
    implicit none
!
! Call variables
!
    type (waqsimtype)                        , target      :: waqsim
    type (hydtype)                           , target      :: hyd
    character(*)                                           :: filename
!
! Local variables
!
   integer :: i
   integer :: iattr
   integer :: j
   integer :: nseg2D
   integer :: lun
   integer :: lunatr
   integer, external :: newunit
   character(256)    :: filestr
   character(1024)   :: line
!
!! executable statements -------------------------------------------------------
!
   lun = newunit()
   open(lun, file = trim(filename)//'.inp')
   !
   write(lun,'(A)') '1000 132 '';'''
   write(lun,'(A)') ';DELWAQ_VERSION_4.91                         ; Use "new" file format'
   write(lun,'(A)') ';PRINT_OUTPUT_OPTION_4                       ; Diagnostic output level 4'
   write(lun,'(A)') '; First input block: characteristics, substance list'
   write(lun,'(3A)') '''',waqsim%description(1),''''
   write(lun,'(3A)') '''',waqsim%description(2),''''
   write(lun,'(3A)') '''',waqsim%description(3),''''
   write(lun,'(A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A)') '''T0: ', &
       & waqsim%refdate(1),'.',waqsim%refdate(2),'.',waqsim%refdate(3),' ', &
       & waqsim%refdate(4),':',waqsim%refdate(5),':',waqsim%refdate(6),'  (scu=       1s)'''

   write(lun,'(A)') '; substance list'
   write(lun,'(A)') ';'
   write(lun,'(2I3,A)') waqsim%nsubs_transp, waqsim%nsubs-waqsim%nsubs_transp, '; number of active and inactive substances'
   write(lun,'(A)') '; Index  Name'
   do i = 1,waqsim%nsubs
      if (waqsim%subsmult(i)>1) then
         if (waqsim%subsmult(i)>10) then
            write(lun,'(I7,3A,I2)') i,' ''',trim(waqsim%subsname(i)),'''*',waqsim%subsmult(i)
         else
            write(lun,'(I7,3A,I1)') i,' ''',trim(waqsim%subsname(i)),'''*',waqsim%subsmult(i)
         endif
      else
         write(lun,'(I7,3A)') i,' ''',trim(waqsim%subsname(i)),''''
      endif
   enddo
   !
   write(lun,'(A)') '#1;'
   write(lun,'(A)') ';###############################################################################'
   write(lun,'(A)') '; Second input block: timers, numerical settings, etc.'
   write(lun,'(I7,A)') 86400,' ''DDHHMMSS'' ''DDHHMMSS''  ; system clock'
   write(lun,'(I7,A)') waqsim%int_method,' ; integration option'
   if (waqsim%disp_flow_zero) then
      write(lun,'(A)') 'NODISP-AT-NOFLOW     ; dispersion active when flow is zero'
   endif
   if (waqsim%disp_bound) then
      write(lun,'(A)') 'NODISP-AT-BOUND      ; dispersion active at open boundaries'
   endif
   if (waqsim%first_order) then
      write(lun,'(A)') 'LOWER-ORDER-AT-BOUND ; lower order scheme at open boundaries'
   endif
   if (waqsim%forester) then
      write(lun,'(A)') 'FORESTER             ; forester filter active'
   endif
   if (waqsim%anticreep) then
      write(lun,'(A)') 'ANTICREEP            ; anti-creep active'
   endif
   write(lun,'(I20.8,A)') sec2ddhhmmss(waqsim%start_time), ' ; simulation start time'
   write(lun,'(I20.8,A)') sec2ddhhmmss(waqsim%stop_time) , ' ; simulation stop time'
   write(lun,'(A)') '                   0 ; timestep constant'
   write(lun,'(I20.8,A)') sec2ddhhmmss(waqsim%time_step) , ' ; simulation timestep'
   write(lun,'(A)') '                   1 ; monitoring areas in this file'
   write(lun,'(A)') '                   0 ; nr of monitoring points'
   write(lun,'(A)') '                   1 ; transects in this file'
   write(lun,'(A)') '                   0 ; nr of transects'
   write(lun,'(A)') ';     start        stop        step'
   write(lun,'(A)') ';  ddhhmmss    ddhhmmss    ddhhmmss'
   write(lun,'(I11.8,A,I11.8,A,I11.8,A)') sec2ddhhmmss(waqsim%start_mon),' ',sec2ddhhmmss(waqsim%stop_mon),' ',sec2ddhhmmss(waqsim%step_mon),'  ; of monitoring'
   write(lun,'(I11.8,A,I11.8,A,I11.8,A)') sec2ddhhmmss(waqsim%start_map),' ',sec2ddhhmmss(waqsim%stop_map),' ',sec2ddhhmmss(waqsim%step_map),'  ; of map, dump'
   write(lun,'(I11.8,A,I11.8,A,I11.8,A)') sec2ddhhmmss(waqsim%start_his),' ',sec2ddhhmmss(waqsim%stop_his),' ',sec2ddhhmmss(waqsim%step_his),'  ; of history'

   write(lun,'(A)') '#2;'
   write(lun,'(A)') ';###############################################################################'
   write(lun,'(A)') '; Third input block: segments, volumes'
   write(lun,'(I0,A)') waqsim%nsegments, ' ; Number of segments'
   write(lun,'(I0,A)') 2, ' ; Option ''grid layout not used'''
   if (hyd%attributes_file == ' ') then
      write(lun,'(I0,A)') 1, ' ; One time-independent contribution'
      write(lun,'(I0,A)') 1, ' ; Number of items'
      write(lun,'(I0,A)') 2, ' ; Only feature 2 is specified'
      write(lun,'(I0,A)') 1, ' ; Input on this file'
      write(lun,'(I0,A)') 1, ' ; Input option without default'
      if (hyd%number_water_quality_layers == 1) then
         write(lun,'(I0,A)') waqsim%nsegments, '*0 ; Segments extend from surface to bed'
      else
         nseg2D = waqsim%nsegments - waqsim%nexchange(3)
         write(lun,'(I0,A)') nseg2D, '*1 ; Surface segments'
         write(lun,'(I0,A)') nseg2D*(hyd%number_water_quality_layers-2), '*2 ; Mid column segments'
         write(lun,'(I0,A)') nseg2D, '*3 ; Bottom segments'
      endif
      write(lun,'(I0,A)') 0, ' ; no time-dependent contributions'
   else
      lunatr = newunit()
      open(lunatr, file=hyd%attributes_file, action='read', status='old')
      read(lunatr,'(A)') line
      close(lunatr)
#ifdef HAVE_FC_FORM_BINARY
      open  ( lunatr , file=hyd%attributes_file , action='read', status='old' , form = 'binary' , SHARED )
#else
! standardized way if binary is not available
      open  ( lunatr , file=hyd%attributes_file , action='read', status='old' , form = 'unformatted' , access='stream'  )
#endif
      read(lunatr) iattr
      close(lunatr)
      if (index(line,'DELWAQ_COMPLETE_ATTRIBUTES')>0) then
         write(lun,'(3A)') 'INCLUDE ''',trim(hyd%attributes_file),''' ; attributes specified in separate file'
      elseif (iattr==0 .or. iattr==1 .or. iattr==11) then ! 21 or 31 shouldn't be on top since delwaq counts segments from top to bottom
         write(lun,'(I0,A)') 1, ' ; One time-independent contribution'
         write(lun,'(I0,A)') 2, ' ; Number of items'
         write(lun,'(I0,A,I0,A)') 1,' ',2, ' ; Features 1 and 2 specified'
         write(lun,'(I0,A)') 0, ' ; From binary file'
         write(lun,'(3A)') '''',trim(hyd%attributes_file),''' ; name of attribute file'
         write(lun,'(I0,A)') 0, ' ; no time-dependent contributions'
      else
         write(lun,'(I0,A)') 1, ' ; One time-independent contribution'
         write(lun,'(I0,A)') 1, ' ; Number of items'
         write(lun,'(I0,A)') 2, ' ; Only feature 2 is specified'
         write(lun,'(I0,A)') -1, ' ; From ASCII file'
         write(lun,'(3A)') '''',trim(hyd%attributes_file),''' ; name of attribute file'
         write(lun,'(I0,A)') 0, ' ; no time-dependent contributions'
      endif
   endif
   write(lun,'(I0,A)') -2, ' ; volumes from unformatted intermediate file'
   write(lun,'(3A)') '''',trim(hyd%volumes_file),''' ; name of volumes file'

   write(lun,'(A)') '#3;'
   write(lun,'(A)') ';###############################################################################'
   write(lun,'(A)') '; Fourth input block: exchanges, pointer table, flows, lengths'
   write(lun,'(I0,A,I0,A,I0,A)') waqsim%nexchange(1), ' ', waqsim%nexchange(2), ' ', waqsim%nexchange(3), ' ; Number of exchanges'
   write(lun,'(I0,A)') 0,' ; number of additional dispersions'
   write(lun,'(I0,A)') waqsim%nfldex,' ; number of additional velocities'
   if (waqsim%nfldex>0) then
      do i = 1, waqsim%nfldex
         write(lun,'(3A)') '   ''',trim(waqsim%fldexname(i)),''''
      enddo
      filestr = '';
      do i = 1, waqsim%nsubs
         do j = 1, waqsim%subsmult(i)
            if (waqsim%substran(i)) filestr = '0 '//filestr
         enddo
      enddo
      write(lun,'(2A)') trim(filestr),' ; the additional velocities are not actually used as velocities'
   endif
   write(lun,'(I0,A)') 1,' ; input format 1'
   write(lun,'(I0,A)') 0,' ; exchange pointers in unformatted file'
   write(lun,'(3A)') '''',trim(hyd%pointers_file),''' ; name of exchange pointer file'
   write(lun,'(I0,A)') 1,' ; dispersions in this file'
   write(lun,'(A)') '1.0      1.0      1.0   ; scale factors in three directions'
   write(lun,'(A)') '0.0      0.0      0.0   ; values in m2/s in three directions'
   write(lun,'(I0,A)') -2,' ; exchange surfaces in unformatted file'
   write(lun,'(3A)') '''',trim(hyd%areas_file),''' ; name of areas file'
   write(lun,'(I0,A)') -2,' ; flows in unformatted file'
   write(lun,'(3A)') '''',trim(hyd%flows_file),''' ; name of flows file'
   if (waqsim%nfldex>0) then
      write(lun,'(I0,A)') 0,' ; additional velocities in unformatted file'
      filestr = trim(filename)//'-additional.vel'
      call writeveloc(filestr, waqsim%fldexval)
      write(lun,'(3A)') '''',trim(filestr),''''
   endif
   write(lun,'(I0,A)') 1,' ; lengths vary over the area'
   write(lun,'(I0,A)') 0,' ; in unformatted file'
   write(lun,'(3A)') '''',trim(hyd%lengths_file),''' ; name of length file'

   write(lun,'(A)') '#4;'
   write(lun,'(A)') ';###############################################################################'
   write(lun,'(A)') '; Fifth input block: boundary conditions'
   write(lun,'(I0,A)') 3*waqsim%nbnd,'*'' '''                                                                     
   write(lun,'(I0,A)') 1,'; time lags at the boundaries'
   write(lun,'(A)') '; nr. ddhhmmss'
   write(lun,'(I0,A)') waqsim%nbnd,'*00010000               ; nr of boundaries times 1 hour'
   write(lun,'(A)') 'ITEM ''Boundary type 1'''
   do i = 1,waqsim%nsubs
      if (waqsim%subsname(i) == 'ISS') then
         if (waqsim%subsmult(i)>1) then
            do j = 1,waqsim%subsmult(i)
               if (j>=100) then
                  write(lun,'(2A,I3.3,A)') 'CONCENTRATION ',trim(waqsim%subsname(i)),j,' DATA 0.0'
               else
                  write(lun,'(2A,I2.2,A)') 'CONCENTRATION ',trim(waqsim%subsname(i)),j,' DATA 0.0'
               endif
            enddo
         else
            write(lun,'(3A)') 'CONCENTRATION ',trim(waqsim%subsname(i)),' DATA 0.0'
         endif
      endif
   enddo

   write(lun,'(A)') '#5;'
   write(lun,'(A)') ';###############################################################################'
   write(lun,'(A)') '; Sixth input block: dry waste loads block'
   write(lun,'(I0,A)') 0, ' ; No dry waste loads'

   write(lun,'(A)') '#6;'
   write(lun,'(A)') ';###############################################################################'
   write(lun,'(A)') '; Seventh input block: constants, processes, segment function, etc'
   write(lun,'(A)') ';'
   do i = 1, waqsim%npar
      write(lun,'(3A,G13.6)') 'CONSTANTS ''',trim(waqsim%parname(i)),''' DATA ',waqsim%parval(i)
   enddo
   write(lun,'(A)') 'CONSTANTS ''ONLY_ACTIVE'' DATA 1.0'
   do i = 1, waqsim%nprocess
      write(lun,'(3A)') 'CONSTANTS ''ACTIVE_',trim(waqsim%process(i)),''' DATA 1.0'
   enddo
   write(lun,'(A)') ';'
   do i = 1, waqsim%nfld
      select case (waqsim%fldname(i))
         case ('Surf')
            filestr = trim(filename)//'-surf.par'
            call srf2par(hyd%surfaces_file, filestr, waqsim%nsegments, waqsim%nexchange(3), hyd%geometry)
         case ('CHEZY')
            filestr = trim(filename)//'-chezy.par'
            call chz2par(hyd%chezy_coefficients_file, filestr, waqsim%nsegments, waqsim%nexchange(3))
         case ('Veloc1')
            filestr = hyd%x_velocities_file
         case ('Veloc2')
            filestr = hyd%y_velocities_file
         case default
            filestr = trim(waqsim%fldname(i))//'.par'
            j = index(filestr,'*')
            do while (j>0)
               filestr(j:j) = '.'
               j = index(filestr,'*')
            enddo
            call write2par(filestr, waqsim%fldval(i,:))
      end select
      write(lun,'(5A)') 'PARAMETERS ''',trim(waqsim%fldname(i)),''' ALL BINARY_FILE ''',trim(filestr),''''
   enddo

   write(lun,'(A)') '#7;'
   write(lun,'(A)') ';###############################################################################'
   write(lun,'(A)') '; Eighth input block: initial conditions'
   do i = 1,waqsim%nsubs
      if (waqsim%subsmult(i)>1) then
         do j = 1,waqsim%subsmult(i)
            write(lun,'(A)') 'INITIALS'
            if (j>=100) then
               write(lun,'(2A,I3.3)') '    ',trim(waqsim%subsname(i)),j
            else
               write(lun,'(2A,I2.2)') '    ',trim(waqsim%subsname(i)),j
            endif
            write(lun,'(A)') 'INPUTGRID ''Base grid'''
            write(lun,'(A)') 'DATA'
            write(lun,'(I0,A)') waqsim%nsegments,'*0.0'
         enddo
      else
         write(lun,'(A)') 'INITIALS'
         write(lun,'(2A)') '    ',trim(waqsim%subsname(i))
         write(lun,'(A)') 'INPUTGRID ''Base grid'''
         write(lun,'(A)') 'DATA'
         write(lun,'(I0,A)') waqsim%nsegments,'*0.0'
      endif
   enddo

   write(lun,'(A)') '#8;'
   write(lun,'(A)') ';###############################################################################'
   write(lun,'(A)') '; Ninth input block: model output'
   write(lun,'(I0,A)') 1, '; output information in this file'
   write(lun,'(I0,A)') 0, '; no output for monitoring file'
   write(lun,'(I0,A)') 0, '; no output for dump file'
   write(lun,'(I0,A)') 0, '; no output for history file'
   !write(lun,'(I0,A)') 1, '; default simulated quantities on map file'
   write(lun,'(I0,A)') 2, '; default simulated quantities plus following quantities on map file'
   write(lun,'(I0,A)') waqsim%noutput, '; number of additional quantities'
   do i = 1,waqsim%noutput
      write(lun,'(3A)') '   ''',trim(waqsim%output(i)),''''
   enddo
   write(lun,'(I0,A)') 1, '; binary history file'
   write(lun,'(I0,A)') 1, '; binary map file'
   write(lun,'(I0,A)') 0, '; nefis history file'
   write(lun,'(I0,A)') 0, '; nefis map file'

   write(lun,'(A)') '#9;'
   close(lun)
end subroutine waqsim_writeinp


subroutine waqsim_allocate(waqsim)
!!--description-----------------------------------------------------------------
!
! Convert morphology ini properties to waq process definition
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Call variables
!
    type (waqsimtype)                                      :: waqsim
!
! Local variables
!
    integer :: istat
    integer :: nex
!
!! executable statements -------------------------------------------------------
!
    nex = sum(waqsim%nexchange)
    allocate(waqsim%poi(4,nex)                              , stat = istat)
    allocate(waqsim%lng(2,nex)                              , stat = istat)
    allocate(waqsim%vol(waqsim%nsegments)                   , stat = istat)
    allocate(waqsim%qag(nex)                                , stat = istat)
    allocate(waqsim%area(nex)                               , stat = istat)
    allocate(waqsim%segatrnr(waqsim%nsegatr)                , stat = istat)
    allocate(waqsim%segatr(waqsim%nsegments, waqsim%nsegatr), stat = istat)
    allocate(waqsim%tmpval(waqsim%nsegments)                , stat = istat)
end subroutine waqsim_allocate


subroutine dealloc_waqsim(waqsim)
    ! modules

    implicit none
    ! variables
    type (waqsimtype)                 :: waqsim

    ! local variables

	! program code
    
    if (allocated(waqsim%poi))      deallocate(waqsim%poi)
    if (allocated(waqsim%lng))      deallocate(waqsim%lng)
    if (allocated(waqsim%vol))      deallocate(waqsim%vol)
    if (allocated(waqsim%qag))      deallocate(waqsim%qag)
    if (allocated(waqsim%area))     deallocate(waqsim%area)
    if (allocated(waqsim%segatrnr)) deallocate(waqsim%segatrnr)
    if (allocated(waqsim%segatr))   deallocate(waqsim%segatr)
    if (allocated(waqsim%tmpval))   deallocate(waqsim%tmpval)
         
end subroutine dealloc_waqsim


subroutine srf2par(srffile, parfile, nsegments, nvertexchanges, geometry)
    character(*), intent(in)      :: srffile
    character(*), intent(in)      :: parfile
    integer, intent(in)           :: nsegments
    integer, intent(in)           :: nvertexchanges
    integer, intent(in)           :: geometry
    !
    integer :: lun
    integer :: nsegments2D
    integer :: nlayers
    integer :: k
    integer :: itime
    integer, dimension(6) :: idummy
    integer, external :: newunit
    real(wqprec), dimension(:), allocatable :: srf
    !
    nsegments2D = nsegments - nvertexchanges
    nlayers = nsegments / nsegments2D
    allocate(srf(nsegments2D))
    !
    lun = newunit()
    if (geometry == WAQSIM_GEOMETRY_TELEMAC) then
       open(lun, file = srffile, form = 'unformatted', convert='big_endian', status = 'old')
    else
#ifdef HAVE_FC_FORM_BINARY
       open  ( lun , file = srffile , status='old' , form = 'binary' , SHARED )
#else
! standardized way if binary is not available
       open  ( lun , file = srffile , status='old' , form = 'unformatted' , access='stream'  )
#endif
    endif
    read(lun) idummy
    read(lun) srf
    close(lun)
    !
    itime = 0
#ifdef HAVE_FC_FORM_BINARY
       open  ( lun , file = parfile , status='replace' , form = 'binary' , SHARED )
#else
! standardized way if binary is not available
       open  ( lun , file = parfile , status='replace' , form = 'unformatted' , access='stream'  )
#endif
    write(lun) itime
    do k = 1, nlayers
       write(lun) srf
    enddo
    close(lun)
    !
    deallocate(srf)
end subroutine srf2par

subroutine chz2par(chzfile, parfile, nsegments, nvertexchanges)
    character(*), intent(in) :: chzfile
    character(*), intent(in) :: parfile
    integer, intent(in)      :: nsegments
    integer, intent(in)      :: nvertexchanges
    !
    integer :: lun
    integer :: nsegments2D
    integer :: nlayers
    integer :: k
    integer :: itime
    integer, dimension(7) :: idummy
    integer, external :: newunit
    real(wqprec), dimension(:), allocatable :: chz
    !
    nsegments2D = nsegments - nvertexchanges
    nlayers = nsegments / nsegments2D
    allocate(chz(nsegments2D))
    !
    lun = newunit()
#ifdef HAVE_FC_FORM_BINARY
       open  ( lun , file = chzfile , status='old' , form = 'binary' , SHARED )
#else
! standardized way if binary is not available
       open  ( lun , file = chzfile , status='old' , form = 'unformatted' , access='stream'  )
#endif
    read(lun) idummy
    read(lun) chz
    close(lun)
    !
    itime = 0
#ifdef HAVE_FC_FORM_BINARY
       open  ( lun , file = parfile , status='replace' , form = 'binary' , SHARED )
#else
! standardized way if binary is not available
       open  ( lun , file = parfile , status='replace' , form = 'unformatted' , access='stream'  )
#endif
    write(lun) itime
    do k = 1, nlayers
       write(lun) chz
    enddo
    close(lun)
    !
    deallocate(chz)
end subroutine chz2par

subroutine write2par(parfile, par)
    character(*), intent(in) :: parfile
    real(wqprec), dimension(:), intent(in) :: par
    !
    integer :: lun
    integer :: itime
    integer, external :: newunit
    !
    lun = newunit()
    itime = 0
#ifdef HAVE_FC_FORM_BINARY
       open  ( lun , file = parfile , status='replace' , form = 'binary' , SHARED )
#else
! standardized way if binary is not available
       open  ( lun , file = parfile , status='replace' , form = 'unformatted' , access='stream'  )
#endif
    write(lun) itime
    write(lun) par
    close(lun)
end subroutine write2par

subroutine writeveloc(velfile, addvel)
    character(*), intent(in)            :: velfile
    real(wqprec), dimension(:,:), intent(in) :: addvel
    !
    integer :: lun
    integer :: itime
    integer, external :: newunit
    !
    lun = newunit()
    itime = 0
#ifdef HAVE_FC_FORM_BINARY
       open  ( lun , file = velfile , status='replace' , form = 'binary' , SHARED )
#else
! standardized way if binary is not available
       open  ( lun , file = velfile , status='replace' , form = 'unformatted' , access='stream'  )
#endif
    write(lun) itime
    write(lun) addvel
    close(lun)
end subroutine writeveloc

subroutine computegridprop(hyd, poi, array)
!!--description-----------------------------------------------------------------
!
! XXXX
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Call variables
!
    type (hydtype)                           , intent(in)  :: hyd
    integer, dimension(:,:)                  , intent(in)  :: poi
    real(wqprec), dimension(:,:)             , intent(out) :: array
!
! Local variables
!
!   NONE
!
!! executable statements -------------------------------------------------------
!
    select case (hyd%geometry)
       case (WAQSIM_GEOMETRY_CURVGRID)
          call gridpropcurv(hyd, poi, array)
       case (WAQSIM_GEOMETRY_TELEMAC)
          call gridproptelm(hyd, poi, array)
       case (WAQSIM_GEOMETRY_UNSTRUCT)
          write(*,*) 'computegridprop for Unstruct not yet implemented'
    end select
end subroutine computegridprop

subroutine gridpropcurv(hyd, poi, array)
!!--description-----------------------------------------------------------------
!
! XXXX
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Call variables
!
    type (hydtype)                           , intent(in)  :: hyd
    integer, dimension(:,:)                  , intent(in)  :: poi
    real(wqprec), dimension(:,:)             , intent(out) :: array
!
! Local variables
!
    integer :: lun
    integer, external :: newunit
    integer :: mmax
    integer :: m
    integer :: nmax
    integer :: n
    integer :: kmax
    integer :: noseg2D
    integer :: noq2D
    integer :: noq1
    integer :: noq2
    integer :: noq3
    integer :: i
    integer :: iq
    integer :: ifrom
    integer :: ito
    integer :: j
    integer :: dum1
    integer :: dum2
    integer :: iqstart
    integer :: iqend
    real(wqprec) :: dx
    real(wqprec) :: dy
    real(wqprec) :: w
    real(4) :: x0
    real(4) :: y0
    integer, dimension(:,:), allocatable :: aggrtable
    real(4), dimension(:,:), allocatable :: x
    real(4), dimension(:,:), allocatable :: y
!
!! executable statements -------------------------------------------------------
!
    lun = newunit()
#ifdef HAVE_FC_FORM_BINARY
      open  ( lun , file=hyd%grid_indices_file , action='read', status='old' , form = 'binary' , SHARED )
#else
! standardized way if binary is not available
      open  ( lun , file=hyd%grid_indices_file , action='read', status='old' , form = 'unformatted' , access='stream'  )
#endif
    read(lun) mmax, nmax, noseg2D, kmax, noq1, noq2, noq3
    allocate(aggrtable(mmax, nmax))
    read(lun) aggrtable
    close(lun)
    !
#ifdef HAVE_FC_FORM_BINARY
      open  ( lun , file=hyd%grid_coordinates_file , action='read', status='old' , form = 'binary' , SHARED )
#else
! standardized way if binary is not available
      open  ( lun , file=hyd%grid_coordinates_file , action='read', status='old' , form = 'unformatted' , access='stream'  )
#endif
    read(lun) nmax, mmax, x0, y0, dum1, dum2, kmax
    do i = 1, 9
       read(lun) dum1
    enddo
    allocate(x(mmax, nmax), y(mmax, nmax))
    read(lun) x, y
    close(lun)
    !
    do j = 1, 2
       if (j==1) then
          iqstart = 1
          iqend   = noq1/kmax
       else
          iqstart = noq1+1
          iqend   = noq1+noq2/kmax
       endif
       do n = 1, nmax
          do m = 1, mmax
             if (n==1 .or. n==nmax .or. m==1 .or. m==mmax) then
                cycle
             endif
             !
             if (j==1) then
                dx = x(m,n)-x(m,n-1)
                dy = y(m,n)-y(m,n-1)
                ifrom = aggrtable(m,n)
                ito   = aggrtable(m+1,n)
             else
                dx = x(m-1,n)-x(m,n)
                dy = y(m-1,n)-y(m,n)
                ifrom = aggrtable(m,n)
                ito   = aggrtable(m,n+1)
             endif
             if (ifrom<=0 .and. ito<=0) then
                cycle
             elseif (ifrom==0 .or. ito==0) then
                cycle
             endif
             do iq = iqstart, iqend
                if (poi(1,iq)==ifrom .and. poi(2,iq)==ito) exit
             enddo
             !
             w = sqrt( dx**2 + dy**2 )
             array(5,iq) = array(5,iq) + w
             array(1,iq) = array(1,iq) + dy ! w*(dy/w) - weighted averaging of normalized normal
             array(2,iq) = array(2,iq) - dx ! w*(dx/w)
          enddo
       enddo
    enddo
    !
    noq2D = noq1/kmax
    do iq = 1, noq2D
       if (array(5,iq)>0.0) then
          array(1,iq) = array(1,iq)/array(5,iq)
          array(2,iq) = array(2,iq)/array(5,iq)
          array(3,iq) = array(1,iq)
          array(4,iq) = array(2,iq)
       endif
    enddo
    do j = 1, kmax-1
       do iq = 1, noq2D
          array(:,iq+noq2D*j) = array(:,iq)
       enddo
    enddo
    !
    noq2D = noq2/kmax
    do iq = noq1+1, noq1+noq2D
       if (array(5,iq)>0.0) then
          array(1,iq) = array(1,iq)/array(5,iq)
          array(2,iq) = array(2,iq)/array(5,iq)
          array(3,iq) = array(1,iq)
          array(4,iq) = array(2,iq)
       endif
    enddo
    do j = 1, kmax-1
       do iq = 1, noq2D
          array(:,noq1+iq+noq2D*j) = array(:,noq1+iq)
       enddo
    enddo
    !
    deallocate(aggrtable, x, y)
end subroutine gridpropcurv

subroutine gridproptelm(hyd, poi, array)
!!--description-----------------------------------------------------------------
!
! XXXX
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Call variables
!
    type (hydtype)                           , intent(in)  :: hyd
    integer, dimension(:,:)                  , intent(in)  :: poi
    real(wqprec), dimension(:,:)             , intent(out) :: array
!
! Local variables
!
    integer :: lun
    integer, external :: newunit
    character(80) :: header
    integer :: nvar1
    integer :: nvar2
    integer :: i
    integer :: ifrom
    integer :: ito
    integer :: j
    integer :: j2
    integer :: iq
    integer :: noq2D
    character(32) :: varchar
    integer, dimension(10) :: ipar
    integer, dimension(6) :: time
    integer :: nelm
    integer :: nelm2
    integer :: npnt
    integer :: npnt2
    integer :: npntelm
    integer :: npntelm2
    integer :: eltyp
    integer :: eltyp2
    integer, dimension(:), allocatable :: edgecnt
    integer, dimension(:,:), allocatable :: edge
    integer, dimension(:,:), allocatable :: elm
    integer, dimension(:,:), allocatable :: elm2
    integer, dimension(:), allocatable :: bound
    integer, dimension(:), allocatable :: bound2
    integer :: found
    integer :: iq0
    real(4) :: xc
    real(4) :: yc
    real(4) :: xe
    real(4) :: ye
    real(4) :: dx
    real(4) :: dy
    real(4) :: w
    real(4), dimension(:), allocatable :: x
    real(4), dimension(:), allocatable :: x2
    real(4), dimension(:), allocatable :: y
    real(4), dimension(:), allocatable :: y2
!
!! executable statements -------------------------------------------------------
!
    lun = newunit()
    open(lun, file = hyd%grid_indices_file, form = 'unformatted', convert='big_endian', status = 'old')
    read(lun) header
    read(lun) nvar1, nvar2
    do i = 1, nvar1+nvar2
       read(lun) varchar
    enddo
    read(lun) ipar
    if (ipar(10) /= 0) then
       read(lun) time
    endif
    read(lun) nelm, npnt, npntelm, eltyp
    if (nvar2 > 0) then
       read(lun) nelm2, npnt2, npntelm2, eltyp2
    else
       nelm2    = 0
       npnt2    = 0
       npntelm2 = 0
       eltyp2   = 0
    endif
    !
    allocate( elm(npntelm, nelm), elm2(npntelm2, nelm2), bound(npnt), bound2(npnt2), x(npnt), x2(npnt2), y(npnt), y2(npnt2) )
    !
    read(lun) elm
    if (nvar2 > 0) then
       read(lun) elm2
    endif
    !
    read(lun) bound
    if (nvar2 > 0) then
       read(lun) bound2
    endif
    !
    read(lun) x
    if (nvar2 > 0) then
       read(lun) x2
    endif
    !
    read(lun) y
    if (nvar2 > 0) then
       read(lun) y2
    endif
    !
    close(lun)
    !
    ! don't rely on the bound array obtained from the Selafin file. Use the
    ! negative segment numbers in the poi array since we need to be consistent
    ! with that delwaq data set anyway.
    !
    bound = 0
    noq2D = size(poi,2)
    do iq = 1,noq2D
       if ( poi(1,iq)>npnt .or. poi(2,iq)>npnt ) then
          noq2D = iq-1
          exit
       endif
       if ( poi(1,iq)<0 ) bound(poi(2,iq)) = iq
       if ( poi(2,iq)<0 ) bound(poi(1,iq)) = iq
    enddo
    !
    ! allocate memory to check identify edges associated with elements, and
    ! check which edges are located on the boundary
    !
    allocate( edge(npntelm, nelm), edgecnt(noq2D) )
    edgecnt = 0
    edge    = 0
    !
    iq0 = 1
    do i = 1,nelm
       do j = 1, 3
          ifrom = elm(j,i)
          j2    = mod(j,3)+1
          ito   = elm(j2,i)
          !
          found = 0
          do iq = iq0, noq2D
             if ( poi(1,iq)==ifrom .and. poi(2,iq)==ito ) then
                found = 1
                iq0 = iq
                exit
             elseif ( poi(1,iq)==ito .and. poi(2,iq)==ifrom ) then
                found = -1
                iq0 = iq
                exit
             endif
          enddo
          if (found==0) then
             do iq = iq0-1, 1, -1
                if ( poi(1,iq)==ifrom .and. poi(2,iq)==ito ) then
                   found = 1
                   iq0 = iq
                   exit
                elseif ( poi(1,iq)==ito .and. poi(2,iq)==ifrom ) then
                   found = -1
                   iq0 = iq
                   exit
                endif
             enddo
          endif
          !
          edge(j,i)   = iq*found
          edgecnt(iq) = edgecnt(iq)+1
       enddo
    enddo
    !
    ! compute distances
    !
    do i = 1,nelm
       xc = ( x(elm(1,i)) + x(elm(2,i)) + x(elm(3,i)) )/3.0
       yc = ( y(elm(1,i)) + y(elm(2,i)) + y(elm(3,i)) )/3.0
       do j = 1, 3
          ifrom = elm(j,i)
          j2    = mod(j,3)+1
          ito   = elm(j2,i)
          iq    = edge(j,i)
          !
          xe = ( x(ifrom) + x(ito) )/2.0
          ye = ( y(ifrom) + y(ito) )/2.0
          dx = xc - xe
          dy = yc - ye
          w = sqrt(dx**2+dy**2)
          !
          ! assume element nodes are specified counter clockwise
          !
          if ( iq>0 ) then
             array(1,iq) = array(1,iq) + dy
             array(2,iq) = array(2,iq) - dx
             array(5,iq) = array(5,iq) + w
          elseif ( iq<0 ) then
             iq = -iq
             array(1,iq) = array(1,iq) - dy
             array(2,iq) = array(2,iq) + dx
             array(5,iq) = array(5,iq) + w
          endif
          !
          if (bound(ifrom)/=0 .and. bound(ito)/=0 .and. edgecnt(iq)==1) then
             dx = xe - x(ifrom)
             dy = ye - y(ifrom)
             w = sqrt(dx**2+dy**2)
             iq = bound(ifrom)
             if ( poi(1,iq)==ifrom ) then
                array(1,iq) = array(1,iq) + dy
                array(2,iq) = array(2,iq) - dx
                array(5,iq) = array(5,iq) + w
             else
                array(1,iq) = array(1,iq) - dy
                array(2,iq) = array(2,iq) + dx
                array(5,iq) = array(5,iq) + w
             endif
             !
             dx = x(ito) - xe
             dy = y(ito) - ye
             w = sqrt(dx**2+dy**2)
             iq = bound(ito)
             if ( poi(1,iq)==ito ) then
                array(1,iq) = array(1,iq) + dy
                array(2,iq) = array(2,iq) - dx
                array(5,iq) = array(5,iq) + w
             else
                array(1,iq) = array(1,iq) - dy
                array(2,iq) = array(2,iq) + dx
                array(5,iq) = array(5,iq) + w
             endif
          endif
       enddo
    enddo
    !
    do iq = 1, noq2D
       if (array(5,iq)>0.0) then
          array(1,iq) = array(1,iq)/array(5,iq)
          array(2,iq) = array(2,iq)/array(5,iq)
          array(3,iq) = array(1,iq)
          array(4,iq) = array(2,iq)
       endif
    enddo
    do j = 1, size(poi,2)/noq2D-1
       do i = 1, noq2D
          array(:,i+noq2D*j) = array(:,i)
       enddo
    enddo
    !
    deallocate(elm, elm2, bound, bound2, edge, edgecnt)
end subroutine gridproptelm

end module waqsim_module
