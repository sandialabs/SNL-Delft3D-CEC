module wave_main
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
!  $Id: wave_main.f90 65790 2020-01-15 13:52:06Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/wave/packages/manager/src/wave_main.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use precision
   use swan_flow_grid_maps
   use swan_input
   use flow_data
   use sync_flowwave
   use wave_data
   use meteo
!
! Module variables
!
   integer                                      :: i
   integer                                      :: ierr
   integer                                      :: n_swan_grids ! number of SWAN grids
   integer                                      :: n_flow_grids ! number of FLOW grids
   integer                                      :: i_flow       ! counter
   integer                                      :: i_swan       ! counter
   integer                                      :: i_meteo      ! counter
   integer                                      :: timtscale    ! time in tscale units, integer representation
   integer                                      :: it01flow     ! reference date obtained from FLOW
   integer                                      :: command
   real                                         :: tscaleflow   ! basic time unit == flow time step (s)
   real(fp)       , dimension(:,:), allocatable :: x_fp         ! Copy of x-coordinate of grid in flexible precision, needed for meteo module
   real(fp)       , dimension(:,:), allocatable :: y_fp         ! Copy of y-coordinate of grid in flexible precision, needed for meteo module
   logical                                      :: success      ! flag indicating whether delftio communication went correct
   logical                                      :: mud
   character(20)                                :: tmpchar
   character(256), dimension(:), pointer        :: meteotypes
   character(500)                               :: message
   type(wave_data_type),target                  :: wavedata


contains


!
! ====================================================================================
function wave_main_init(mode_in, mdw_file) result(retval)
   !
   ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
   ! Activate the following line
   ! See also statements below
   !
   ! use ifcore
   ! 
   implicit none
!
! return value
!
   integer :: retval
!
! Global variables
!
   integer      :: mode_in
   character(*) :: mdw_file     ! filename mdw file
!
! Local variables
!
   integer :: mtdim
   !
   ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
   ! Activate the following line
   ! See also statements below
   !
   ! INTEGER*4 OLD_FPE_FLAGS, NEW_FPE_FLAGS
!
!! executable statements -----------------------------------------------
!
   ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
   ! Activate the following two lines
   ! See also use statement above
   !
   ! NEW_FPE_FLAGS = FPE_M_TRAP_OVF + FPE_M_TRAP_DIV0 + FPE_M_TRAP_INV
   ! OLD_FPE_FLAGS = FOR_SET_FPE (NEW_FPE_FLAGS)
   !
   call checklicense(success)
   if ( .not. success ) then
      write(*,'(a)') '*** ERROR: No authorization'
      call wavestop(1, '*** ERROR: No authorization')
   endif
   !
   call initialize_wavedata(wavedata)
   call setmode(wavedata, mode_in)
   !
   ! Read mdw file
   !
   call read_swan(mdw_file, swan_run, wavedata)
   n_swan_grids = swan_run%nnest
   if (wavedata%mode/=stand_alone .and. swan_run%flowgridfile/=' ') then
      swan_run%nttide = 1
   endif
   !
   ! Initialisation from flow (write file runid(s))
   !
   if (wavedata%mode/=stand_alone .and. swan_run%flowgridfile==' ') then
      it01flow = 0
      call flow_init(wavedata%mode, it01flow, tscaleflow)
      call settscale(wavedata%time, tscaleflow)
      if (wavedata%time%refdate == 0) then
         !
         ! No reference date in mdw-file or waves_alone
         ! Use reference date from flow
         !
         call setrefdate(wavedata%time, it01flow)
      else
         if (wavedata%time%refdate == it01flow) then
            !
            ! Reference date from flow is identical to reference date from mdw-file/waves_alone
            !
         else
            write(*,'(a,i8,a,i8,a)') '*** ERROR: Reference date from FLOW (', &
                & it01flow, ') differs from WAVE (', wavedata%time%refdate, ').'
            call wavestop(1, '*** ERROR: Reference date from FLOW differs from WAVE')
         endif
      endif
   else
      !
      ! stand_alone, flow data may be used or written
      !
      if (swan_run%useflowdata .or. swan_run%swwav) then
         !
         ! In this case, refdate and tscale are read from the com-file
         ! Only tscale must be set in wavedata%time
         !
         call flow_init(wavedata%mode, it01flow, tscaleflow)
         if (swan_run%flowgridfile == ' ') then
            !
            ! tscaleflow is only set when reading from com-file
            ! not when reading from netcdf-file
            !
            call settscale (wavedata%time, tscaleflow)
         endif
      endif
   endif
   if (wavedata%time%refdate == 0) then
      write(*,'(a)') '*** ERROR: Reference date not set'
      write(*,'(a)') '           Use Delft3D-WAVE-GUI version 4.90.00 or higher to create the mdw-file.'
      call wavestop(1, '*** ERROR: Reference date not set')
   endif
   !
   ! Read wave grids and flow grids; make grid-maps
   !
   call grids_and_gridmaps(n_swan_grids, n_flow_grids, swan_run, wavedata%mode)
   !
   ! Allocate swan output fields defined on flow grids; they have to be
   ! stored and updated over multiple nested swan runs
   !
   do i_flow=1,n_flow_grids
      flow_output_fields(i_flow)%n_outpars = 0
      call alloc_output_fields(flow_grids(i_flow),flow_output_fields(i_flow))
   enddo
   !
   ! Set mode to spherical if first swan grid is spherical
   !
   swan_run%sferic = swan_grids(1)%sferic
   !
   ! Meteo data from file?
   ! Only if 1 or more meteoitems have been specified
   ! and meteo information has not been received from FLOW
   !
   do i_swan = 1, n_swan_grids
      if (swan_run%dom(i_swan)%n_meteofiles_dom > 0 .and. swan_run%dom(i_swan)%qextnd(q_wind) == 0) then
         !
         ! Grid coordinates of all swan grids are needed by the meteo module
         !
         success  = initmeteo(swan_grids(i_swan)%grid_name)
         call checkmeteoresult_wave(success)
         !
         ! Allocate local copies of coordinate arrays
         ! Must be in flexible precision for the meteo module
         !
         allocate(x_fp(swan_grids(i_swan)%mmax,swan_grids(i_swan)%nmax))
         allocate(y_fp(swan_grids(i_swan)%mmax,swan_grids(i_swan)%nmax))
         x_fp = real(swan_grids(i_swan)%x, fp)
         y_fp = real(swan_grids(i_swan)%y, fp)
         !
         ! Read the meteo files
         !
         do i_meteo = 1, swan_run%dom(i_swan)%n_meteofiles_dom
            success = addmeteoitem(swan_grids(i_swan)%grid_name               , &
                                 & swan_run%dom(i_swan)%meteofile_dom(i_meteo), &
                                 & swan_grids(i_swan)%sferic                  , &
                                 & swan_grids(i_swan)%mmax                    , &
                                 & swan_grids(i_swan)%nmax                    )
            call checkmeteoresult_wave(success)
            !
            success  = gridtometeo(   swan_grids(i_swan)%grid_name, &
                                 &    swan_grids(i_swan)%nmax     , &
                                 &    swan_grids(i_swan)%mmax     , &
                                 & 1, swan_grids(i_swan)%nmax     , &
                                 & 1, swan_grids(i_swan)%mmax     , &
                                 &    swan_grids(i_swan)%kcs      , &
                                 &    x_fp                        , &
                                 &    y_fp                        )
            call checkmeteoresult_wave(success)
         enddo
         !
         ! Deallocate local copies of coordinate arrays
         !
         deallocate(x_fp)
         deallocate(y_fp)
         nullify(meteotypes)
         mtdim = 0
         success = getmeteotypes(swan_grids(i_swan)%grid_name, meteotypes,mtdim)
         call checkmeteoresult_wave(success)
         do i_meteo = 1, size(meteotypes)
            if (meteotypes(i_meteo) == "meteo_on_computational_grid") then
               write(*,'(a)') '*** ERROR: "meteo on computational grid" (flow grid) is not supported by Delft3D-WAVE'
               call wavestop(1, '*** ERROR: "meteo on computational grid" (flow grid) is not supported by Delft3D-WAVE')
            endif
         enddo
         deallocate(meteotypes)
      endif
   enddo
   !
   ! ====================================================================================
   ! CHECK
   ! ====================================================================================
   !
   call check_input(swan_run, wavedata)
   
   retval = 0
end function wave_main_init



!
! ====================================================================================
function wave_main_step(stepsize) result(retval)
   implicit none
!
! return value
!
   integer :: retval
!
! Globals
!
   real(hp) :: stepsize
!
! Locals
!
   integer  :: iold
   real(hp) :: tend
!
!! executable statements -----------------------------------------------
!
   if (wavedata%mode /= stand_alone) then
      !
      ! In combination with flow, perform the swan computation (including mapping etc.)
      !
      command = flow_wave_comm_perform_step
      do while (command == flow_wave_comm_perform_step)
         if (swan_run%flowgridfile == ' ') then
            write(*,'(a)') 'Waiting for communication with Delft3D-FLOW ...'
            mud     = .false.
            success = wave_from_flow_command(command, mud, timtscale)
            if ( .not. success ) then
               write(*,'(a)') '*** ERROR: Communication with Delft3D-FLOW failed.'
               call wavestop(1, '*** ERROR: Communication with Delft3D-FLOW failed.')
            endif
            if (wavedata%mode == flow_mud_online) then
               write(*,'(a)') 'Waiting for communication with MUD ...'
               mud     = .true.
               success = wave_from_flow_command(command, mud, timtscale)
               if ( .not. success ) then
                  write(*,'(a)') '*** ERROR: Communication with MUD layer failed.'
                  call wavestop(1, '*** ERROR: Communication with MUD layer failed.')
               endif
            endif
         else
            command = 0
         endif
         !
         write(*,'(a)')'*****************************************************************'
         write(*,'(a)')'*  Start of Delft3D-WAVE ...'
         !
         ! Update wave and wind conditions
         !
         if (swan_run%flowgridfile == ' ') then
            call settimtscale(wavedata%time, timtscale, swan_run%modsim, swan_run%nonstat_interval)
         else
            call settimsec(wavedata%time, wavedata%time%timsec + real(stepsize,sp), swan_run%modsim, swan_run%nonstat_interval)
         endif
         !
         ! Run n_swan nested SWAN runs
         !
         call swan_tot(n_swan_grids, n_flow_grids, wavedata, 0)
         write(*,'(a)')'*  End of Delft3D-WAVE'
         write(*,'(a)')'*****************************************************************'
         !
         if (swan_run%flowgridfile == ' ') then
            mud = .false.
            call wave_to_flow_status(flow_wave_comm_result_ok, mud)
            if (wavedata%mode == flow_mud_online) then
               mud = .true.
               call wave_to_flow_status(flow_wave_comm_result_ok, mud)
            endif
         endif
      enddo
      call deallocate_flow_data()
   else
      !
      ! Standalone swan computation
      !
      if (swan_run%timwav(1) < 0.0) then
         !
         ! No times specified in mdw file: just do one computation with specified timestep
         !
         swan_run%timwav(1) = wavedata%time%timsec + stepsize / 60.0_hp
         swan_run%nttide    = 1
         call swan_tot(n_swan_grids, n_flow_grids, wavedata, 0)
      else
         !
         ! Times specified in mdw file: compute all specified times between tcur en tcur+tstep
         !
         tend = wavedata%time%timmin + stepsize / 60.0_hp
         iold = 0
         do while (wavedata%time%timmin <= tend)
            !
            ! Search an i for which timwav(i) lies between the "current time" and the "step_end_time",
            ! including the boundaries "current time" and the "step_end_time":
            ! This is needed when timwav(1)=0
            !
            i = iold + 1
            do while (i <= swan_run%nttide)
               if (swan_run%timwav(i) >= wavedata%time%timmin .and. swan_run%timwav(i) <= tend) then
                  exit
               else
                  i = i + 1
               endif
            enddo
            if (i <= swan_run%nttide .and. i/=iold) then
               !
               ! Found an i to do a computation
               ! iold is needed when timwav(i)="current time" or "step_end_time",
               ! to avoid doing the same computation twice
               !
               call swan_tot(n_swan_grids, n_flow_grids, wavedata, i)
               iold = i
            else
               !
               ! No computation left to be done
               ! Set the current time to "step_end_time" (to be sure it has the correct value)
               ! and exit the do-loop
               !
               call settimmin(wavedata%time, real(tend,sp), swan_run%modsim, swan_run%nonstat_interval)
               exit
            endif
         enddo
      endif
   endif
   retval = 0
end function wave_main_step



!
! ====================================================================================
function wave_main_finish() result(retval)
   use swan_input
   implicit none
!
! return value
!
   integer :: retval
!
!! executable statements -----------------------------------------------
!
   call del_temp_files(n_swan_grids)
   !
   ! Deallocate memory used by meteo module
   !
   do i_swan = 1, n_swan_grids
      call deallocmeteo(swan_grids(i_swan)%grid_name)
   enddo
   !
   call dealloc_swan(swan_run)
   write(*,'(a)') 'Delft3D-WAVE finished normally.'
   retval = 0
end function wave_main_finish



!
! ====================================================================================
function wave_main_set_com_interval(interval) result(retval)
   implicit none
!
! return value
!
   integer :: retval
!
! Globals
!
   real(hp) :: interval
!
!! executable statements -----------------------------------------------
!
   swan_run%deltcom = real( interval / 60.0_hp, sp)
   retval = 0
end function wave_main_set_com_interval

end module wave_main
