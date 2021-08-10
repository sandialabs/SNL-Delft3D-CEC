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

! $Id: xbeach_wave_boundary_main.f90 65778 2020-01-14 14:07:42Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/xbeach_wave_boundary_main.f90 $
module wave_boundary_main_module
   implicit none
   private
   public create_incident_waves_surfbeat
   ! This module is the main entry to generating wave boundary conditions in XBeach
   ! and other models. This module is accessed by wave_boundary_update_module. 
   ! The interface 'create_incident_wave', which returns wave boundary conditions at  
   ! the correct time step can be called from outside the module. 
   ! 
   ! NOTE!
   ! 'wave_boundary' should only be called by processes with a boundary
   ! for which wave boundary conditions are required (in XBeach only if xmpi_istop).
   ! Else processes will waste I/O resources and computational time generating
   ! useless information.
   !
   ! TO FIX: 
   ! 
   ! - generate_wave_train_properties_per_offshore_point
   ! - read all spectrum files
   ! - continue at line 2117 wave_boundary_update
   !
   !
   !
   ! generate an interface so we don't have to pass unnecessary vectors
   ! when using different types of boundary conditions
   !
   !   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !                                                                            !
   !                      INTERFACE TO OTHER MODELS                             !
   !                                                                            !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !
   !interface create_incident_waves
   !   module procedure create_incident_waves_surfbeat
   !   !module procedure generate_wave_boundary_nonhydrostatic
   !end interface create_incident_waves
   !
   !
   ! 
contains
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                                !
!                       SUBROUTINES CALLED BY INTERFACE                          !
!                                                                                !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
subroutine create_incident_waves_surfbeat(np,ibnd,xb,yb,ntheta,dtheta,theta,t, &
                                           bctype,bcfile, &
                                           x0,y0,hboundary, &
                                           randomseed, &
                                           eebc,qxbc,qybc, &
                                           Hbc,Tbc,Dbc,isRecomputed, &
                                           nonhspectrum, &
                                           sprdthr,trepfac,nmax,fcutoff,rho, &
                                           Tm01switch,nspr, swkhmin)
   ! This subroutine handles all calls for surf-beat wave boundary conditions.
   ! The subroutine automatically initialises all variables if needed, and returns
   ! boundary condition information at all offshore points at the required point
   ! in time.
   ! 
   ! Input variables
   ! np              : number of offshore grid points (-)
   ! ibnd            : boundary section number (-)
   ! xb,yb           : vectors of x and y coordinates of offshore grid points (m)
   ! ntheta          : number of computational wave bins in directional space (-)
   ! dtheta          : (constant) grid size of wave direction bins (rad)
   ! theta           : vector of centre of wave direction bins (rad). Angles are in 
   !                   cartesian system relative to the coordinate system x (UTM East) axis
   ! t               : current time (s)
   ! bctype          : integer specifying the type of boundary conditions to produce,
   !                   equal to par%instat in XBeach (-), see paramsconst.F90 for 
   !                   options
   ! bcfile          : name of main wave boundary condition file to read, equal to 
   !                 : par%bcfile in XBeach (-)
   ! x0,y0           : reference point coordinates for wave conditions (for instance
   !                   origin of model grid). Used to determine phase of wave 
   !                   components in boundary conditions. Must be identical on all 
   !                   processes (m)
   ! hboundary       : average water depth along the entire offshore boundary (m). This
   !                   value should be constant, or not vary strongly, across all 
   !                   processes.
   ! randomseed      : integer containing initial random seed value between 1 and 2^31-2.
   !                   This should have an identical value on all processes, and is used 
   !                   to generate identical random numbers sequences on all processors (-).
   !                   If set to same integer throughout simulation, identical random numbers
   !                   will be generated. Use int(clocktime) to have new random numbers for
   !                   each time series generation.
   !
   ! Output variables
   ! eebc            : array of size (np,ntheta) containing wave energy density per 
   !                   offshore point and wave direction at time=t (J/m2/rad)
   ! qxbc,qybc       : vector of size (np) containing depth-avaraged discharge per along-boundary 
   !                   meter in the x (landward) and y (longshore) direction per 
   !                   offshore point at time=t (m2/s)
   ! Hbc             : Hm0 computed for the entire boundary based on the input spectra, valid for
   !                   the duration of the entire spectrum (m)
   ! Tbc             : Trep computed for the entire boundary based on the input spectra, valid for
   !                   the duration of the entire spectrum (s)
   ! Dbc             : mean wave direction computed for the entire boundary based on the input spectra, 
   !                   valid for the duration of the entire spectrum (rad)
   ! isRecomputed    : logical, indicating whether a new spectrum has been read and computed and therfore
   !                   showing new values for Hbc, Tbc and Dbc
   !
   ! Optional input variables
   ! nonhspectrum    : generate a non-hydrostatic time series instead of a surf-beat time
   !                   series. Default = .false.
   ! sprdthr         : Threshold ratio to maxval of S above which spec dens are read in, see XBeach. 
   !                   Default = 0.08
   ! trepfac         : Compute mean wave period over energy band: par%trepfac*maxval(Sf); converges to Tm01 
   !                   for trepfac = 0.0. Default = 0.01
   ! nmax            : maximum ratio of cg/c fro computing long wave boundary conditions. Default = 0.8d0
   ! fcutoff         : Low-frequency cutoff frequency for long-wave interaction components. Default = 0.d0
   ! rho             : Density of water (kg/m3). Default = 1025.0
   ! Tm01switch      : Turn on Tm01 (1) or Tm-10 (0) to compute Trep. Default = 0
   ! nspr            :  nspr = 1 long wave direction forced into centres of short wave bins, 
   !                    nspr = 0 regular long wave spreading. Default = 0
   use wave_boundary_datastore
   use wave_boundary_update_module, only: generate_wave_boundary_surfbeat

   use interp

   !
   implicit none
   !
   ! Input variables
   integer                    ,intent(in)     :: np,ibnd,ntheta,bctype
   real*8                     ,intent(in)     :: t,x0,y0,hboundary,dtheta
   character(len=*)           ,intent(in)     :: bcfile
   real*8,dimension(np)       ,intent(in)     :: xb,yb
   real*8,dimension(ntheta)   ,intent(in)     :: theta
   integer                    ,intent(in)     :: randomseed
   
   ! output variables
   real*8                     ,intent(out)    :: Hbc,Tbc,Dbc
   logical                    ,intent(out)    :: isRecomputed
   real*8,dimension(np)       ,intent(out)    :: qxbc,qybc
   real*8,dimension(np,ntheta),intent(out)    :: eebc
   
   ! Optional variables
   logical   ,optional        ,intent(in)     :: nonhspectrum
   real*8    ,optional        ,intent(in)     :: sprdthr,trepfac,nmax,rho,fcutoff,swkhmin
   integer   ,optional        ,intent(in)     :: Tm01switch,nspr
   
   ! internal variables
   integer                                    :: i,itheta,l,dummy
   real*8                                     :: durationlength
   real*8    ,dimension(:)    ,allocatable    :: inttemp 
   !
   !
   ! Check function input arguments and set defaults
   if(.not.present(nonhspectrum)) then
      waveBoundaryParameters(ibnd)%nonhspectrum = .false.
   else
      waveBoundaryParameters(ibnd)%nonhspectrum = nonhspectrum
   endif
   if(.not.present(sprdthr)) then
      waveBoundaryParameters(ibnd)%sprdthr = 0.08d0
   else
      waveBoundaryParameters(ibnd)%sprdthr = sprdthr
   endif
   if(.not.present(trepfac)) then
      waveBoundaryParameters(ibnd)%trepfac = 0.01d0
   else
      waveBoundaryParameters(ibnd)%trepfac = trepfac
   endif
   if(.not.present(Tm01switch)) then
      waveBoundaryParameters(ibnd)%Tm01switch = 0
   else
      waveBoundaryParameters(ibnd)%Tm01switch = Tm01switch
   endif
   if(.not.present(nspr)) then
      waveBoundaryParameters(ibnd)%nspr = 0
   else
      waveBoundaryParameters(ibnd)%nspr = nspr
   endif
   if(.not.present(nmax)) then
      waveBoundaryParameters(ibnd)%nmax = 0.8d0
   else
      waveBoundaryParameters(ibnd)%nmax = nmax
   endif
   if(.not.present(fcutoff)) then
      waveBoundaryParameters(ibnd)%fcutoff = 0.0d0
   else
      waveBoundaryParameters(ibnd)%fcutoff = fcutoff
   endif
   if(.not.present(rho)) then
      waveBoundaryParameters(ibnd)%rho = 1025.d0
   else
      waveBoundaryParameters(ibnd)%rho = rho                        ! JRE TO DO: accomodate varying rho
   endif
   if (.not.present(swkhmin)) then
      waveboundaryParameters(ibnd)%swkhmin = -0.01d0
   else
      waveboundaryParameters(ibnd)%swkhmin = swkhmin
   end if
   !
   !
   ! Generate or interpolate boundary condition time series
   if (t>=waveBoundaryAdministration(ibnd)%startComputeNewSeries) then
      ! The start of the current boundary condition should be the end of the previous
      ! boundary condition
      waveBoundaryAdministration(ibnd)%startCurrentSeries = waveBoundaryAdministration(ibnd)%startComputeNewSeries
      ! Call subroutine to generate wave boundary condition time series from spectral
      ! input
      call generate_wave_boundary_surfbeat(ibnd, durationlength)
      !
      !
      ! Update time administration
      waveBoundaryAdministration(ibnd)%startComputeNewSeries = waveBoundaryAdministration(ibnd)%startComputeNewSeries + &
                                                                durationlength
      

      isRecomputed = .true.
   endif
   
   ! Interpolate energy and discharge at all locations in time

   l = size(waveBoundaryTimeSeries(ibnd)%tbc)
   allocate(inttemp(l))
   !
   do itheta=1,ntheta
      do i=1,np
         inttemp = waveBoundaryTimeSeries(ibnd)%eebct(i,:,itheta)
         call linear_interp(waveBoundaryTimeSeries(ibnd)%tbc,inttemp,l,&
                            t,eebc(i,itheta),dummy)
      enddo
   enddo
   do i=1,np
      inttemp = waveBoundaryTimeSeries(ibnd)%qxbct(i,:)
      call linear_interp(waveBoundaryTimeSeries(ibnd)%tbc,inttemp,l,&
                            t,qxbc(i),dummy)
      inttemp = waveBoundaryTimeSeries(ibnd)%qybct(i,:)
      call linear_interp(waveBoundaryTimeSeries(ibnd)%tbc,inttemp,l,&
                            t,qybc(i),dummy)
   enddo

   Hbc = waveSpectrumAdministration(ibnd)%Hbc
   Tbc = waveSpectrumAdministration(ibnd)%Tbc
   Dbc = waveSpectrumAdministration(ibnd)%Dbc

end subroutine create_incident_waves_surfbeat
                                           
!subroutine initialise_wave_spectrum_parameters
!    ! This subroutine reads 
!    use wave_boundary_datastore
!
!    use m_xbeach_filefunctions
!    use m_xbeach_errorhandling
!
!    
!    !internal variables
!    integer                     :: fid,err
!    integer                     :: i,nspectra
!    character(1024)             :: testline
!    character(1)                :: testchar
!    
!
!    call writelog('l','','--------------------------------')
!    call writelog('l','','Initializing spectral wave boundary conditions ')
!
!    ! Initialize that wave boundary conditions need to be calculated (first time at least)
!    ! Stored and defined in wave_boundary_main_module
!    waveSpectrumAdministration%repeatwbc = .false.
!    ! Initialize the number of times wave boundary conditions have been generated.
!    ! Stored and defined in wave_boundary_main_module
!    waveSpectrumAdministration%bccount  = 0
!    ! Initialize bcendtime to zero.
!    ! Stored and defined in wave_boundary_main_module
!    waveSpectrumAdministration%spectrumendtime = 0.d0
!    ! Initialise lastwaveheight to zero
!    ! Stored and defined in wave_boundary_main_module
!    allocate(waveSpectrumAdministration%lastwaveelevation(waveBoundaryParameters%np,&
!                                                          waveBoundaryParameters%ntheta))
!    !
!    ! open location list file
! 
!    fid = create_new_fid()
!    err = 0
!    open(fid,file=waveBoundaryParameters%masterFileName,status='old',form='formatted')
!    ! check for LOCLIST
!    read(fid,*)testline
!    if (trim(testline)=='LOCLIST') then
!       ! check the length of this file
!       i = 0
!       do while (err==0)
!          i=i+1
!          read(fid,*,IOSTAT=err)testchar
!       enddo
!       rewind(fid)
!       nspectra = i-1
!       ! store this information in the main module
!       waveSpectrumAdministration%nspectra = nspectra
!       ! We need at least 1 spectrum location
!       if (nspectra<1) then
!         call writelog('ewls','(a,a)','Error reading file ', & 
!                       trim(waveBoundaryParameters%masterFileName))
!         call writelog('ewls','(a,a)','Ensure that if LOCLIST option is used, ', & 
!                                      'at least one spectrum location is specified')
!         call xbeach_errorhandler
!       endif
!       ! read first line again to set cursor at correct point in the file
!       read(fid,*,IOSTAT=err)testline       
!       ! 
!       !
!       ! Now allocate space for variables
!       !allocate(waveSpectrumAdministration%bcfiles(nspectra))      ! already allocated and assigned in spectral_wave_init
!       !allocate(waveSpectrumAdministration%xspec(nspectra))
!       !allocate(waveSpectrumAdministration%yspec(nspectra))
!       do i=1,nspectra
!          ! read x,y and file name per location
!          read(fid,*,IOSTAT=err)waveSpectrumAdministration%xspec(i), &
!                                waveSpectrumAdministration%yspec(i), &
!                                waveSpectrumAdministration%bcfiles(i)%fname
!          waveSpectrumAdministration%bcfiles(i)%listline = 0
!          if (err /= 0) then
!            
!             ! something has gone wrong during the read of this file
!             call writelog('ewls','(a,i0,a,a)','error reading line ',i+1,' of file ', &
!                                           trim(waveBoundaryParameters%masterFileName))
!             call writelog('ewls','','check file for format errors')
!             call xbeach_errorhandler
!            
!          endif
!       enddo
!    else
!       nspectra = 1
!       allocate(waveSpectrumAdministration%bcfiles(nspectra))
!       allocate(waveSpectrumAdministration%xspec(nspectra))
!       allocate(waveSpectrumAdministration%yspec(nspectra))
!       waveSpectrumAdministration%bcfiles(1)%fname = waveBoundaryParameters%masterFileName
!       waveSpectrumAdministration%bcfiles(1)%listline = 0
!       waveSpectrumAdministration%xspec = waveBoundaryParameters%x0
!       waveSpectrumAdministration%yspec = waveBoundaryParameters%y0
!       waveSpectrumAdministration%nspectra = nspectra
!    endif
!    close(fid)
!
!    call writelog('l','','--------------------------------')
!
!end subroutine initialise_wave_spectrum_parameters
!   
                                           
end module wave_boundary_main_module


      
   