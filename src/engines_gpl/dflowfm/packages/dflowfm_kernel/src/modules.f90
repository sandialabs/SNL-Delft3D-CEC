!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2020.!
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

! $Id: modules.f90 65931 2020-02-05 10:39:47Z kernkam $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/modules.f90 $

!> @file modules.f90
!! Modules with global data.
!! call default_*() routines upon program startup and when loading a new MDU.
!! call only reset_*() routines when reinitialising an active flow model.


! NOTE: this document is automatically parsed
! CONVENTION:
! please use the following variable notation so the parser will pickup variables for dynamic exchange
! {=optional}
! typename, {allocatable, }target :: name{(:)} !< {(altname)} [units] description {JSON}
! NOTE: only one variable definition per line, the variable should not continue on the next line.
!
! The JSON part can contain additional key-value pairs in JSON format, e.g.:
! !< [m] waterlevel at previous timestep {"state":true,"slice":"1:nodtot","standard_name":"sea_surface_height"}
!
! For state variables values the following JSON key-value pairs are required:
! "standard_name" is the netcdf standard name for the variable, e.g. "sea_surface_height"


!NOTE: the modules
! m_dimens, m_polygon moved to gridgeom
! m_save_ugrid_state saves the variable names for saving UGrid format

 module m_physcoef
 implicit none
 double precision                  :: ag         !< 10.0   ! 9.81    ! (m/s2)
 double precision                  :: sag        !< sqrt(ag)
 integer                           :: jahelmert=0 !< 1=use Helmerts equation for agp only
 double precision                  :: vonkar     !< von Karman constant ()
 double precision                  :: vonkarw    !< von Karman constant used in wind formulations, on Firmijns request ()
 double precision                  :: frcuni     !< uniform friction coeff 2D
 double precision                  :: frcuni1D   !< uniform friction coeff 1D
 double precision                  :: frcuni1D2D !< uniform friction coeff 1D2D
 double precision                  :: frcunistreetinlet    = 0.035
 double precision                  :: frcuniroofgutterpipe = 0.035
 double precision                  :: frcuniroof           = 0.030
 double precision                  :: frcuni1Dgrounlay !< uniform friction coeff groundlayer
 double precision                  :: frcmax           !< max friction coeff in frcu

 integer                           :: ifrctypuni !< 0=chezy, 1=manning, 2=white colebrook D3D, 3=white colebrook Waqua (now only 2D)
 double precision                  :: frcunilin  !<60.    ! 6      ! 66     ! uniform friction coeff
 double precision                  :: umodlin    !< linear friction umod , ifrctyp 4,5,6

 double precision                  :: wall_ks !< vertical wall nIKURADSE ROUGHNESSs (m)
 double precision                  :: wall_z0 !< z0 for vertical walls, ~= Ks/30    (m)
                                              !! z0 for bottom follows from ifrctyp==3 and z0=frcuni
 double precision                  :: z0      !< z0
 double precision                  :: ee      !< natural e
 double precision                  :: ee9     !< natural e/c9of1

 double precision                  :: vicouv      !< constant horizontal eddy viscosity   (m2/s) mom
 double precision                  :: dicouv      !< constant horizontal eddy diffusivity (m2/s) sal, sed

 double precision                  :: Elder       !< add Elder viscosity
 double precision                  :: Smagorinsky !< add Smagorinsky Cs coefficient, vic = vic + (Cs*dx)**2 * S
 double precision                  :: viuchk  !< if < 0.5 then eddy viscosity cell peclet check viu<viuchk*dx*dx/dt

 double precision                  :: vicoww  !< 1D-6   !                 ! user specified constant vertical   eddy viscosity  (m2/s)
 double precision                  :: dicoww  !< 1D-6   !                 ! user specified constant vertical   eddy diffusivity(m2/s)

 double precision                  :: rhomean !< mean ambient rho ! (kg/m3)
 double precision                  :: rhog    !< rhomean*g
 double precision                  :: c9of1   !< vonkar/log(c9of1 + dzb / z0)

                                                 !< Molecular diffusivity coefficients (m2/s):
 double precision                  :: viskin     !< kinematic  viscosity
 double precision                  :: difmolsal  !< molecular diffusivity of salinity
 double precision                  :: difmoltem  !<           diffusivity of temperature
 double precision                  :: difmolsed  !<           diffusivity of sediment
 double precision                  :: difmoltr   !<           diffusivity of tracers

 double precision                  :: vicwminb   ! minimum eddy viscosity in production terms shear and buoyancy
 double precision                  :: xlozmidov  ! Ozmidov length scale (m)

 double precision                  :: viskinair  !< kinematic  viscosity
 double precision                  :: backgroundairtemperature   !< background air   temp (C)
 double precision                  :: backgroundwatertemperature !< background water temp (C)
 double precision                  :: backgroundsalinity         !< background salinity (ppt)
 double precision                  :: backgroundcloudiness       !< (%) cloudiness        for non-specified points
 double precision                  :: backgroundhumidity         !< (%) relative humidity for non-specified points
 double precision                  :: secchidepth                !< (m) secchidepth
 double precision                  :: secchidepth2               !< (m) secchidepth2
 double precision                  :: secchidepth2fraction       !< (m) fraction of total absorbed by profile 2
 double precision                  :: zab(2), sfr(2)             !< help variables

 double precision                  :: cp0                        !< eckart density parameters
 double precision                  :: clam                       !< eckart density parameters
 double precision                  :: clam0                      !< eckart density parameters
 double precision                  :: alph0                      !< eckart density parameters
 integer                           :: idensform                  !< 0 = no, 1 = eckart
 integer                           :: limiterhordif              !< 0=No, 1=Horizontal gradient densitylimiter, 2=Finite volume

 double precision                  :: Stanton                    !< coeff for convective  heat flux, if negative , take wind Cd
 double precision                  :: Dalton                     !< coeff for evaporative heat flux, if negative , take wind Cd
 double precision                  :: Tempmax = -999d0           !< limit
 double precision                  :: Tempmin = 0d0              !< limit
 double precision                  :: Salimax = -999d0           !< limit
 double precision                  :: Salimin = 0d0              !< limit
 double precision                  :: epshstem = 0.001d0         !< only compute heatflx + evap if depth > trsh
 double precision                  :: surftempsmofac = 0.0d0     !< surface temperature smoothing factor 0-1d0
 double precision                  :: Soiltempthick   = 0.10d0   !< if soil buffer desired make thick > 0, e.g. 0.2 m

 integer                           :: Jadelvappos                !< only positive forced evaporation fluxes

 double precision                  :: tetav                      !< vertical teta transport
 double precision                  :: tetavkeps                  !< vertical teta k-eps
 double precision                  :: tetavmom                   !< vertical teta momentum

 double precision                  :: locsaltlev, locsaltmin, locsaltmax
 contains
!> Sets ALL (scalar) variables in this module to their default values.
subroutine default_physcoef()
ag          = 9.81d0    ! 10.0      ! (m/s2)
sag         = sqrt(ag)
vonkar      = 0.41d0    ! von Karman constant ()
vonkarw     = 0.40d0    ! von Karman constant for wind ()
ee          = exp(1d0)  ! natural e ()
ee9         = 9d0*ee    !
frcuni      = 0.023d0   ! 60.    ! 6      ! 66     ! uniform friction coeff
frcuni1D    = 0.023d0   ! 60.    ! 6      ! 66     ! uniform friction coeff
frcuni1D2D  = 0.023d0   ! 60.    ! 6      ! 66     ! uniform friction coeff
frcuni1Dgrounlay  = 0.05d0   ! 60.    ! 6      ! 66     ! uniform friction coeff
frcmax      = 0d0
ifrctypuni  = 1         ! 0=chezy, 1=manning, 2=white colebrook (D3D), 3=white colebrook (WAQUA)
frcunilin   = 0d0       !
umodlin     = 1.d0      ! linear friction umod , ifrctyp 4,5,6
wall_ks     = 0.0d0     ! vertical wall nIKURADSE ROUGHNESSs (m)
vicouv      = 1d0       ! constant horizontal eddy viscosity (m2/s) mom
dicouv      = 1d0       ! constant horizontal eddy diffusivity (m2/s) sal, sed

Elder       = 0d0       ! add Elder viscosity
Smagorinsky = 0d0       ! add Smagorinsky Cs coefficient, vic = vic + (Cs*dx)**2 * S
viuchk      = 0.24      ! if < 0.5 then eddy viscosity cell check viu<viuchk*dx*dx/dt

vicoww      = 5d-5      ! 1D-6   !                 ! constant vertical   eddy viscosity (m2/s)
dicoww      = 5d-5      ! 1D-6   !                 ! constant vertical   eddy viscosity (m2/s)

rhomean     = 1000d0    ! mean ambient rho ! (kg/m3)
rhog        = ag*rhomean
c9of1       = 9d0       ! vonkar/log(c9of1 + dzb / z0)


backgroundairtemperature    = 20d0          ! background air   temp (degC)
backgroundwatertemperature  = 20d0          ! background water temp (degC)
backgroundsalinity          = 30d0          ! background salinity (ppt), in eq of state, if salinity not computed
backgroundcloudiness        = 50d0          ! (%) cloudiness        for non-specified points
backgroundhumidity          = 50d0          !<(%) relative humidity for non-specified points
secchidepth                 = 1d0           !< (m) secchidepth
secchidepth2                = 0d0           !< (m) secchidepth2
secchidepth2fraction        = 0d0           !< (m) fraction of total absorbed by profile 2

                                            ! Molecular diffusivity coefficients:
viskin                      = 1.D-6         ! kinematic  viscosity water in keps model
viskinair                   = 1.5d-5        ! kinematic  viscosity air
difmolsal                   = viskin/700d0  ! molecular diffusivity of salinity
difmoltem                   = viskin/6.7d0  !           diffusivity of temperature
difmolsed                   = 0d0
difmoltr                    = 0d0

vicwminb                    = 0d-7          ! was 0d0, minimum viscosity in production terms shear and buoyancy
xlozmidov                   = 0d0           ! Ozmidov length scale

alph0                       = 0.698d0       ! =Eckart density parameters

idensform                   = 2             !< 0 = no, 1 = Eckart, 2 = UNESCO
limiterhordif               = 2             !< 0=No, 1=Horizontal gradient densitylimiter, 2=Finite volume

Stanton                     = -1            !< coeff for convective  heat flux, if negative , take wind Cd
Dalton                      = -1            !< coeff for evaporative heat flux, if negative , take wind Cd

Jadelvappos                 = 1             !< only positive forced evaporation fluxes

tetav                       = 0.55d0        !< vertical teta transport
tetavkeps                   = 0.55d0        !< vertical teta k-eps
tetavmom                    = 0.55d0        !< vertical teta momentum

locsaltlev                  = 1d0           !< salinity level for case of lock exchange
locsaltmin                  = 5d0           !< minimum salinity for case of lock exchange
locsaltmax                  = 10d0          !< maximum salinity for case of lock exchange

end subroutine default_physcoef
end module m_physcoef

module m_einstein_garcia              ! integral polynomial coefficients, (sedimentation engineering garcia table 2.5
implicit none
double precision :: c1(5,0:6), c2(5,0:6), d(1:5)
end module m_einstein_garcia

subroutine einstein_init()
use m_einstein_garcia
implicit none

c1(1,:)  = (/    8.03210,  -26.27300, -114.69000,  501.43000, -229.51000,   41.94000,   -2.77220  /)
c1(2,:)  = (/    2.11420,   -3.45020,   12.49100,   60.34500,  -29.42100,    5.42150,   -0.35770  /)
c1(3,:)  = (/    1.48520,    0.20250,   14.08700,   20.91800,  -10.91000,    2.03400,   -0.13450  /)
c1(4,:)  = (/    1.10380,    2.66260,    5.64970,    0.38220,   -0.61740,    0.13150,   -0.00910  /)
c1(5,:)  = (/    1.12660,    2.62390,    3.08380,   -0.36360,   -0.07340,    0.02460,   -0.00190  /)

c2(1,:)  = (/    2.57790,  -12.41800,   47.35300,   17.63900,  -13.55400,    2.83920,   -0.20030  /)
c2(2,:)  = (/    1.26230,    1.03300,   13.54300,    0.76550,   -1.66460,    0.38030,   -0.02750  /)
c2(3,:)  = (/    1.15100,    2.17870,    7.65720,   -0.27770,   -0.57000,    0.14240,   -0.01050  /)
c2(4,:)  = (/    1.25740,    2.31590,    1.92390,   -0.35580,    0.00750,    0.00640,   -0.00060  /)
c2(5,:)  = (/    1.49520,    2.20410,    1.05520,   -0.23720,    0.02650,   -0.00080,   -0.00005  /)

d(1:5)   = (/    0.001d0,    0.005d0,     0.01d0,     0.05d0,      0.1d0                          /)

end subroutine einstein_init

module m_waves

 implicit none
 integer, parameter                         :: TPWAVDEFAULT  = 0    !< Indicator for TP
 integer, parameter                         :: TPWAVSMOOTH   = 1    !< Indicator for TPS
 integer, parameter                         :: TPWAVRELATIVE = 2    !< Indicator for RTP
 integer                                    :: nwf                  !< nr of fetch wind dirs + 1
 integer                                    :: ndx2Dr               !< (reduced) total nr 2D cells
 double precision, allocatable              :: fetch(:,:)           !< wind dir dep. fetch lenght (m) of each cell, dimension 5,*, or 13, * nr of wind dirs + 1
 double precision, allocatable              :: fetdp(:,:)           !< wind dir dep. waterdepth (m)   of each cell, dimension 5,*, or 13, * nr of wind dirs + 1
 double precision, allocatable              :: fett(:,:)            !< reduce array, (2,ndx)

 double precision, allocatable, target      :: hwav(:)              !< [m] root mean square wave height (m) from external source, {"location": "face", "shape": ["ndx"]}
 double precision, allocatable, target      :: hwavcom(:)           !< [m] root mean square wave height (m) from external source
 double precision, allocatable, target      :: twav(:)              !< [s] wave period {"location": "face", "shape": ["ndx"]}
 double precision, allocatable, target      :: phiwav(:)            !< [degree] mean wave direction (degrees) from external source
 double precision, allocatable, target      :: Uorb(:)              !< [m/s] orbital velocity {"location": "face", "shape": ["ndx"]}
 double precision, allocatable, target      :: ustokes(:)           !< [m/s] wave induced velocity, link-based and link-oriented
 double precision, allocatable, target      :: vstokes(:)           !< [m/s] wave induced velocity, link-based and link-oriented
 double precision, allocatable              :: rlabda(:)            !< [m] wave length
 double precision, allocatable              :: ustk(:)              !< [m/s] Ustokes depth averaged cell centres


 double precision, allocatable, target      :: dsurf(:)             !< [w/m2] wave energy dissipation rate due to breaking at the free surface, "DISSURF" in WAVE
 double precision, allocatable, target      :: dwcap(:)             !< [w/m2] wave energy dissipation rate due to white capping
 integer         , allocatable, target      :: kdismx(:)            !< help array to determine the layer of hrms effect

 double precision                           :: hwavuni   = 0d0      !< uniform (*.mdu) value of ...
 double precision                           :: twavuni   = 0d0      !< uniform (*.mdu) value of ...
 double precision                           :: phiwavuni = 0d0      !< uniform (*.mdu) value of ...

 double precision                           :: wavenikuradse        !< nikuradse roughness for waves (m)
 double precision                           :: z0wav                !< plus z0waves (m)
 double precision                           :: ftauw = 1d0          !< Swartfactor

 character(len=4)                           :: rouwav               !< Friction model for wave induced shear stress

 double precision, allocatable, target      :: sxwav(:)             !< [N/m2] wave force in x (east)  direction on water surface (N/m2) from external source, "FX"   in WAVE
 double precision, allocatable, target      :: sywav(:)             !< [N/m2] wave force in y (north) direction on water surface (N/m2) from external source, "FY"   in WAVE
 double precision, allocatable, target      :: sbxwav(:)            !< [N/m2] wave force in x (east)  direction on water column  (N/m2) from external source, "WSBU" in WAVE
 double precision, allocatable, target      :: sbywav(:)            !< [N/m2] wave force in y (north) direction on water column  (N/m2) from external source, "WSBV" in WAVE
 double precision, allocatable, target      :: uorbwav(:)           !< [m/s] orbital velocity (m/s) from external source
 double precision, allocatable, target      :: wlenwav(:)           !< [m] wave length (m) from external source

 ! additional data for WAVE/SWAN-coupling
 double precision, allocatable, target      :: mxwav(:)             !< wave induced volume flux, in x-direction at flow-nodes
 double precision, allocatable, target      :: mywav(:)             !< wave induced volume flux, in y-direction at flow-nodes

 double precision, allocatable              :: taubxu(:)            !< Maximal bed shear stress
 double precision, allocatable              :: ypar(:)
 double precision, allocatable              :: cfwavhi(:)
 double precision, allocatable              :: cfhi_vanrijn(:)
 double precision, allocatable              :: wblt(:)
 double precision, allocatable              :: taux_cc(:), tauy_cc(:)

 double precision                           :: facmax               !< maximum wave force

 ! for visualisation
 integer                                    :: waveparopt
 integer                                    :: numoptwav

 double precision, allocatable              :: ust_mag(:)
 double precision, allocatable              :: fwav_mag(:)

 ! parameters, may be overwritten by user in mdu-file
 double precision                           :: gammax               !< Maximum wave height/water depth ratio
 double precision                           :: alfdeltau = 20d0     !< coeff for thickness of wave bed boundary layer
 double precision                           :: hminlw               !< [m] minimum depth for wave forcing in flow momentum equation RHS.
 integer                                    :: jatpwav=TPWAVDEFAULT !< TPWAV, TPWAVSMOOTH, TPWAVRELATIVE
 integer                                    :: jauorb               !< multiply with factor sqrt(pi)/2 (=0), or not (=1). Default 0, delft3d style
 integer                                    :: jahissigwav          !< 1: sign wave height on his output; 0: hrms wave height on his output.
 integer                                    :: jamapsigwav          !< 1: sign wave height on map output; 0: hrms wave height on map output.
 integer                                    :: jauorbfromswan       !< 1: get uorb from SWAN, compare with Delft3D
logical                                     :: extfor_wave_initialized !< is set to .true. when the "external forcing"-part that must be initialized for WAVE during running (instead of during initialization) has actually been initialized

contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_waves() instead.
subroutine default_waves()
   use m_physcoef

   rouwav                  = 'FR84'
   gammax                  = 1.0d0        !< Maximum wave height/water depth ratio
   hminlw                  = 0.2d0        !< [-] minimum depth for wave forcing in flow momentum equation RHS.
   jatpwav                 = TPWAVDEFAULT !< TPWAV, TPWAVSMOOTH, TPWAVRELATIVE
   jauorb                  = 0
   jahissigwav             = 1
   jamapsigwav             = 0            ! Present behaviour
   jauorbfromswan          = 0
   facmax                  = 0.25d0*sag*rhomean*gammax**2

   call reset_waves()
end subroutine default_waves

!> Resets only waves variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, call default_waves() instead.
subroutine reset_waves()
   extfor_wave_initialized = .false.      !< is set to .true. when the "external forcing"-part that must be initialized for WAVE during running (instead of during initialization) has actually been initialized
end subroutine reset_waves

end module m_waves

module m_solver
   type tsolver
!     matrix
      integer                                       :: numrows               !< number of rows
      integer                                       :: numnonzeros           !< number of non-zero entries
      double precision, dimension(:),   allocatable :: a                     !< matrix entries,   dim(numnonzeros)
      integer,          dimension(:),   allocatable :: ia                    !< matrix pointer,   dim(numrows+1)
      integer,          dimension(:),   allocatable :: ja                    !< matrix row index, dim(numnonzeros)

!     right-hand side
      double precision, dimension(:),   allocatable :: rhs

!     preconditioner
      integer                                       :: numnonzerosprecond    !< number of non-zero entries in preconditioning matrix
      double precision, dimension(:),   allocatable :: alu                   !< precond. matrix entries,   dim(numnonzerosprecond)
      integer,          dimension(:),   allocatable :: ju                    !< precond. matrix pointer,   dim(numrows)
      integer,          dimension(:),   allocatable :: jlu                   !< precond. matrix row index, dim(numnonzerosprecond)

!     work array
      integer                                       :: nwork
      double precision, dimension(:),   allocatable :: work                  !< work array, dimension(nwork)     !< size of work array
      integer,          dimension(:),   allocatable :: jw                    !< nonzero indicater, dim(2*nrows)

!     settings
      integer,          dimension(16)               :: ipar
      double precision, dimension(16)               :: fpar
      double precision                              :: alpha
      integer                                       :: lfil
      double precision                              :: tol
      double precision                              :: eps
      integer                                       :: jabcgstab
   end type tsolver
end module m_solver

module m_advec
   use m_solver

   integer                                     :: jaoutput   = 1 !< output matrices to file (1) or not (0)

   type(tsolver)                               :: solver_advec

   integer,          dimension(:), allocatable :: iI, jI
   double precision, dimension(:), allocatable :: aI        !< interpolation and projection matrix in CRS format

   integer,          dimension(:), allocatable :: iR, jR
   double precision, dimension(:), allocatable :: aR        !< reconstruction matrix in CRS format

   integer,          dimension(:), allocatable :: iC, jC
   double precision, dimension(:), allocatable :: aC        !< collocated discretization matrix in CRS format

   integer,          dimension(:), allocatable :: iW, jW
   double precision, dimension(:), allocatable :: aW        !< work matrix in RCS format
   integer,          dimension(:), allocatable :: iwork

   double precision, dimension(:,:), allocatable :: dfluxfac   !< flux(L) = dfluxfac(1,L) * something(ln(1,L)) + dfluxfac(2,L) * something(ln(2,L)), positive from ln(1,L) to ln(2,L)
end module

module m_xbeach_data
   use m_xbeach_typesandkinds
   use m_solver
   !==================================================================================================================================
   ! XBeach related variables
   !==================================================================================================================================
   !! Hydrodynamics arrays, allocatables
   double precision, allocatable              :: s1initial(:)    !< initial waterlevel (m ) for Riemann boundary condition
   double precision, allocatable              :: ee0(:,:)        !< wave energy at begin of timestep
   double precision, allocatable              :: ee1(:,:)        !< wave energy at end of timestep
   double precision, allocatable              :: cwav(:)         !< phase speed (m/s)
   double precision, allocatable              :: cgwav(:)        !< wave group velocity (m/s)
   double precision, allocatable              :: kwav(:)         !< wavenumber k (rad/m)
   double precision, allocatable              :: nwav(:)         !< cg/c (-)
   double precision, allocatable              :: km(:)           !< wave number k with wci
   double precision, allocatable              :: umwci(:)        !< weighted velocity components wci
   double precision, allocatable              :: vmwci(:)
   double precision, allocatable              :: zswci(:)
   double precision, allocatable              :: ctheta(:,:)     !< propagation speed in theta space
   double precision, allocatable              :: sigmwav(:)      !< wave frequency (rad/s)
   double precision, allocatable              :: sigt(:,:)
   double precision, allocatable              :: horadvec(:,:)   !< horizontal advection
   double precision, allocatable              :: thetaadvec(:,:) !< directional advection
   double precision, allocatable              :: rhs(:,:)        !< right-hand side
   double precision, allocatable              :: rrthetaadvec(:,:) !< directional advection roller
   double precision, allocatable              :: rrhoradvec(:,:) !< horz advection roller
   double precision, allocatable              :: rr(:,:)         !< directional advection roller
   double precision, allocatable              :: csx(:)
   double precision, allocatable              :: snx(:)
   double precision, allocatable              :: H(:)            !< hrms golfhoogte, onafh van instat
   double precision, allocatable              :: E(:)            !< bulk wave energy in nodes
   double precision, allocatable              :: DR(:)           !< Bulk roller dissipation
   double precision, allocatable              :: R(:)            !< Bulk roller energy
   double precision, allocatable              :: thet(:,:)       !< centre angle dir bin in each node
   double precision, allocatable              :: costh(:,:)
   double precision, allocatable              :: sinth(:,:)
   double precision, allocatable              :: Sxx(:)          !< Radiation stresses
   double precision, allocatable              :: Syy(:)
   double precision, allocatable              :: Sxy(:)
   double precision, allocatable              :: dhsdx(:)
   double precision, allocatable              :: dhsdy(:)
   double precision, allocatable              :: Fx(:)           !< wave forces, on links
   double precision, allocatable              :: Fy(:)
   double precision, allocatable              :: Fx_cc(:)        !< wave forces in cc, for output
   double precision, allocatable              :: Fy_cc(:)
   double precision, allocatable              :: urms(:)         !< contribution for wave induced bed shear stress, on links = urms_cc/sqrt(2)
   double precision, allocatable              :: urms_cc(:)      !< orbital velocity, in cell centre, uorb from xbeach, equal to uorb in orbvel and trab11, trab12 in D3D
   double precision, allocatable              :: ust(:)          !< Stokes drift east
   double precision, allocatable              :: vst(:)          !< Stokes drift north
   double precision, allocatable              :: xbdsdx(:)         !< water level gradient
   double precision, allocatable              :: xbdsdy(:)         !< water level gradient
   double precision, allocatable              :: xbducxdx(:)       !< velocity gradients
   double precision, allocatable              :: xbducydx(:)       !<
   double precision, allocatable              :: xbducxdy(:)       !<
   double precision, allocatable              :: xbducydy(:)       !<

   double precision, allocatable              :: thetamean(:)    !< mean wave angle
   double precision, allocatable              :: Qb(:)           !< Wave breaking proportion
   double precision, allocatable              :: D(:)            !< Wave breaking dissipation
   double precision, allocatable              :: Df(:)           !< Bottom frictional dissipation
   double precision, allocatable              :: Dtot(:)
   double precision, allocatable              :: BR(:)           !< Roller surface slope, also important for morph
   double precision, allocatable              :: uin(:)          !< xcomponent incoming long wave induced velocity
   double precision, allocatable              :: vin(:)          !< ycomponent incoming long wave induced velocity
   double precision, allocatable              :: bi(:)           !< long wave component bichromatic bc
   double precision, allocatable              :: ktb(:)          !< Short wave induced turbulence near the bottom in flow nodes
   double precision, allocatable              :: Tbore(:)        !< Bore period

   double precision, allocatable              :: Ltemp(:)
   double precision, allocatable              :: L1(:)
   double precision, allocatable              :: e01(:)
   double precision, allocatable              :: tE(:), dataE(:), databi(:)
   double precision, allocatable              :: L0(:),khdisp(:),hdisp(:)

   double precision                           :: newstatbc       !< stationary bc generated
   double precision                           :: xref0, yref0    !< reference coordinates phase shift bc
   integer         , allocatable              :: randomseed(:)

   integer                                     :: maxnumbnds = 0
   integer,          allocatable, dimension(:) :: kbndu2kbndw    !< mapping velocity bc pts to wave bc pts
   integer,          allocatable, dimension(:) :: kbndw2kbndu    !< mapping velocity bc pts to wave bc pts
   integer,          allocatable, dimension(:) :: kbndz2kbndw    !< mapping water level bc pts to wave bc pts
   double precision, allocatable, dimension(:) :: uave           !< boundary averaged velocity
   double precision, allocatable, dimension(:) :: vave           !<
   double precision, allocatable, dimension(:) :: dlengthrm        !< boundary length
   double precision, allocatable, dimension(:) :: umeanrm          !< relaxated velocity riemann bnd
   double precision, allocatable, dimension(:) :: vmeanrm          !<
   double precision, allocatable, dimension(:) :: u1rm             !<

   !  for stationary solver
   !integer,          dimension(:,:), allocatable :: isweepup, isweepdown
   type(tsolver)                                 :: solver

   !  for plotting
   integer                                     :: itheta_view=5
   double precision, allocatable               :: ustx_cc(:),usty_cc(:)

   !! Model parameters
   !! 1. DFLOW specific
   !integer                                    :: limtypw         !< 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC, 21=central voor wave action transport
   double precision                           :: dtmaxwav        !< subtimestepping for xbeach wave driver
   double precision                           :: dtmaximp        !< pseudotimestepping for implicit wave driver

   integer                                    :: xb_started=0

   !! 2. XBeach specific
   !  Type             name                   initialize    !  [unit] (advanced/deprecated) description
   ! [Section] Physical processes
   integer                 :: swave                      = -123    !  [-] Include short waves (1), exclude short waves (0)
   integer                 :: lwave                      = -123    !  [-] Include short wave forcing on NLSW equations and boundary conditions (1), or exclude (0)

   ! [Section] Wave boundary condition parameters
   character(slen)         :: instat                     = 'abc'   !  [-] Wave boundary condtion type
   double precision        :: taper                      = -123    !  [s] Spin-up time of wave boundary conditions, in morphological time
   double precision        :: Hrms                       = -123    !  [m] Hrms wave height for instat = 0,1,2,3
   double precision        :: Tm01                       = -123    !  [s] (deprecated) Old name for Trep
   double precision        :: Trep                       = -123    !  [s] Representative wave period for instat = 0,1,2,3
   double precision        :: Tlong                      = -123    !  [s] Wave group period for case instat = 1
   double precision        :: dir0                       = -123    !  [deg] Mean wave direction (Nautical convention) for instat = 0,1,2,3
   double precision        :: nwavmax                    = -123    !  [-] (advanced) maximum ratio of cg/c fro computing long wave boundary conditions
   integer                 :: m                          = -123    !  [-] Power in cos^m directional distribution for instat = 0,1,2,3

   ! [Section] Wave-spectrum boundary condition parameters
   character(slen)         :: bcfile                     = 'abc'   !  [-] Name of spectrum file
   integer                 :: random                     = -123    !  [-] (advanced) Random seed on (1) or off (0) for instat = 4,5,6 boundary conditions
   double precision        :: fcutoff                    = -123    !  [Hz] (advanced) Low-freq cutoff frequency for instat = 4,5,6 boundary conditions
   integer                 :: nspr                       = -123    !  [-] (advanced) nspr = 1 long wave direction forced into centres of short wave bins, nspr = 0 regular long wave spreadin
   double precision        :: trepfac                    = -123    !  [-] (advanced) Compute mean wave period over energy band: trepfac*maxval(Sf) for instat 4,5,6; converges to Tm01 for trepfac = 0.0 and
   double precision        :: sprdthr                    = -123    !  [-] (advanced) Threshold ratio to maxval of S above which spec dens are read in (default 0.08*maxval)
   integer                 :: correctHm0                 = -123    !  [-] (advanced) Turn off or on Hm0 correction
   integer                 :: Tm01switch                 = -123    !  [-] (advanced) Turn off or on Tm01 or Tm-10 switch
   double precision        :: rt                         = -123    !  [s] Duration of wave spectrum at offshore boundary, in morphological time
   double precision        :: dtbc                       = -123    !  [s] (advanced) Timestep used to describe time series of wave energy and long wave flux at offshore boundary (not affected by morfac)
   double precision        :: dthetaS_XB                 = -123    !  [deg] (advanced) The (counter-clockwise) angle in the degrees needed to rotate from the x-axis in SWAN to the x-axis pointing East
   integer                 :: nspectrumloc               = -123    !  [-] (advanced) Number of input spectrum locations
   integer                 :: oldnyq                     = -123    !  [-] (advanced) Turn off or on old nyquist switch
   double precision        :: swkhmin                    = -123    !  [-] (advanced,silent) Minimum kh value to include in wave action balance, lower included in NLSWE (default -1.d0)

   ! [Section] Flow boundary condition parameters
   integer                 :: order                      = -123    !  [-] (advanced) Switch for order of wave steering, 1 = first order wave steering (short wave energy only), 2 = second oder wave steering (bound long wave corresponding to short wave forcing is added)
   integer                 :: freewave                   = -123    !  [-] (advanced) Switch for free wave propagation 0 = use cg (default); 1 = use sqrt(gh) in instat = 3
   double precision        :: epsi                       = -123    !  [-] (advanced) Ratio of mean current to time varying current through offshore boundary
   character(slen)         :: tidetype                   = 'abc'   !  [-] (advanced) Switch for offfshore boundary, velocity boundary or instant water level boundary (default)
   integer                 :: ARC                        = -123    !  [-] (advanced) Switch for active reflection compensation at seaward boundary: 0 = reflective, 1 = weakly (non) reflective
   double precision        :: hminlw                     = -123    !  [-] minimum depth for wave forcing in flow momentum equation RHS

   ! [Section] Wave breaking parameters
   character(slen)                :: break          = 'abc'   !  [-] Type of breaker formulation (1=roelvink, 2=baldock, 3=roelvink adapted, 4=roelvink on/off breaking)
   double precision               :: gamma          = -123    !  [-] Breaker parameter in Baldock or Roelvink formulation
   double precision               :: gamma2         = -123    !  [-] End of breaking parameter in break = 4 formulation
   double precision               :: alpha          = -123    !  [-] (advanced) Wave dissipation coefficient in Roelvink formulation
   double precision               :: nroelvink      = -123    !  [-] (advanced) Power in Roelvink dissipation model
   double precision               :: gammax         = -123    !  [-] (advanced) Maximum ratio wave height to water depth
   double precision               :: deltaH         = -123    !  [-] (advanced) Fraction of wave height to add to water depth
   double precision, allocatable  :: fw(:)                    !  [-] (advanced) Internally used bed friction factor
   double precision               :: fwcutoff       = -123    !  [-] Depth greater than which the bed friction factor is NOT applied
   character(slen)                :: wavefricfile   = 'abc'   !  [-] (advanced) Filename spatially varying sample file bed friction factor
   double precision               :: wavefricval    = -123    !  [-] Bed friction factor from params file

   ! [Section] Roller parameters
   integer                 :: roller                     = -123    !  [-] (advanced) Turn on (1) or off(0) roller model
   double precision        :: beta                       = -123    !  [-] (advanced) Breaker slope coefficient in roller model
   integer                 :: rfb                        = -123    !  [-] (advanced) Switch to feed back maximum wave surface slope in roller energy balance, otherwise rfb = par%Beta

   ! [Section] Wave-current interaction parameters
   integer                 :: wci                        = -123    !  [-] Turns on (1) or off (0) wave-current interaction
   double precision        :: hwci                       = -123    !  [m] (advanced) Minimum depth until which wave-current interaction is used
   double precision        :: hwcimax                    = -123    !  [m] (advanced) Maximum depth until which wave-current interaction is used
   double precision        :: cats                       = -123    !  [Trep] (advanced) Current averaging time scale for wci, in terms of mean wave periods

   ! [Section] Wave numerics parameters
   double precision        :: wavint                     = -123    !  [s] Interval between wave module calls (only in stationary wave mode)
   double precision        :: maxerror                   = -123    !  [m] (advanced) Maximum wave height error in wave stationary iteration
   integer                 :: maxiter                    = -123    !  [-] (advanced) Maximum number of iterations in wave stationary
   integer                 :: tsmult                     = -123    !  [-] multiplier, maximizes implicit timestep based on CFL based timestep for implicit solver
   double precision        :: waveps                     = -123    !  [-] eps for wave related quantities, for comparison with XBeach
   double precision        :: d_relaxfac                 = -123    !  [-] Relaxation factor for wave dissipation in stationary solver
   !
   ! [Section] Roller and wave turbulence parameters
   double precision        :: BRfac                      = -123    !  [-] (advanced) Calibration factor surface slope
   integer                 :: turb                       = -123    !  [name] (advanced) Switch to include short wave turbulence
   double precision        :: Tbfac                      = -123    !  [-] (advanced) Calibration factor for bore interval Tbore: Tbore = Tbfac*Tbore
   !
   !
   ! [Section] Hydrodynamics for FI (frequency integrated) approach as opposed to FF (fixed frequency)
   integer                 :: windmodel                  = -123    !   [-] Turns on (1) or off (0) the frequency integrated 2-equation approach
   integer                 :: advecmod                   = -123    !   [-] advect moments m^E_-1 an m^E_0 (1) or moments m^E_0 and m^E_1
   double precision        :: Trepini                    = -123    !   [s] Initial fill value for Trep in entire domain
   double precision        :: Eini                       = -123    !   [J/rad/m2] Initial fill value for ee1 in entire domain
   !arrays
   double precision, allocatable              :: tt1(:,:)          !   [s] wave period per itheta-bin
   double precision, allocatable              :: cwavt(:,:)        !   [m/s] phase speed  per itheta-bin
   double precision, allocatable              :: cgwavt(:,:)       !   [m/s] wave group velocity per itheta-bin
   double precision, allocatable              :: kwavt(:,:)        !   [rad/m] wavenumber k per itheta-bin
   double precision, allocatable              :: nwavt(:,:)        !   [-] cg/c per itheta-bin
   !double precision, allocatable             :: kmt(:,:)          !   [rad/m] wave number k with wci
   double precision, allocatable              :: horadvec2(:,:)    !   [] horizontal advection 2nd moment
   double precision, allocatable              :: thetaadvec2(:,:)  !   [] directional advection 2nd moment

   double precision, allocatable              :: Ltempt(:,:)       !   [m] wave length temp per itheta-bin
   double precision, allocatable              :: L1t(:,:)          !   [m] wave length end per itheta-bin
   double precision, allocatable              :: L0t(:,:)          !   [m] wave length start per itheta-bin

   double precision, allocatable              :: ma(:,:)           !   [varying] pointer to moment a (depends on advecmod)
   double precision, allocatable              :: mb(:,:)           !   [varying] pointer to moment b (depends on advecmod)


   ! [Section] Windmodel source numerics parameters
   double precision        :: mwind                      = -123    !  [-] ideal distribution shape parameter wind source
   double precision        :: ndissip                    = -123    !  [-] wave shape parameter in wavenumber spectrum (Booij (1999))
   integer                 :: jawsource                  = -123    !  [-] switch wind source term or not
   integer                 :: jagradcg                   = -123    !  [-] switch include grad(cg) in windsource term
   double precision        :: coefdispT                  = -123    !  [-] taperfactor on wave period dissipation
   double precision        :: coefdispk                  = -123    !  [-] shape factor on wave number limitation on wave period dissipation
   double precision        :: Eful                       = 0.0036d0!  [-] fully developed dimensionless wave energy (Pierson Moskowitz 1964)
   double precision        :: Tful                       = 7.69d0  !  [-] fully developed dimensionless peak period (Pierson Moskowitz 1964)
   double precision        :: aa1                        = 0.00288d0! [-] shape parameter wave growth curves (Kahma Calkoen (1992))
   double precision        :: bb1                        = 0.45d0  !  [-] shape parameter wave growth curves (Kahma Calkoen (1992))
   double precision        :: aa2                        = 0.459d0 !  [-] shape parameter wave growth curves (Kahma Calkoen (1992))
   double precision        :: bb2                        = 0.27d0  !  [-] shape parameter wave growth curves (Kahma Calkoen (1992))
   double precision        :: CE1                        = -123    !  [-] wind source term parameter (MSc thesis MvdL)
   double precision        :: CE2                        = -123    !  [-] wind source term parameter (MSc thesis MvdL)
   double precision        :: CT1                        = -123    !  [-] wind source term parameter (MSc thesis MvdL)
   double precision        :: CT2                        = -123    !  [-] wind source term parameter (MSc thesis MvdL)
   ! arrays
   double precision, allocatable              :: wmagcc(:)           !  [m/s] wind speed magnitude cell centered
   double precision, allocatable              :: windspreadfac(:,:)!  [-] distribution of inproducts thetabins per cell with wind direction
   double precision, allocatable              :: SwE(:)            !  [-] nodal wind source term energy
   double precision, allocatable              :: SwT(:)            !  [-] nodal wind source term period
   double precision, allocatable              :: wsorE(:,:)        !  [J/m2/s] wind source term for ee1
   double precision, allocatable              :: wsorT(:,:)        !  [s/s] wind source term for tt1
   double precision, allocatable              :: egradcg(:,:)       !  [m/s/m] spatial gradient of cg
   double precision, allocatable              :: ddT(:)             !  [s/s] dissipation of wave period
end module m_xbeach_data

module m_xbeach_avgoutput

   double precision, allocatable      :: H_mean(:), H_var(:), H_min(:), H_max(:), H_varcross(:), H_varsquare(:)                                !< Sign wave height
   double precision, allocatable      :: urms_mean(:), urms_var(:), urms_max(:), urms_min(:), urms_varcross(:), urms_varsquare(:)                     !< orbital velocity
   double precision, allocatable      :: ust_mean(:), ust_var(:), ust_max(:), ust_min(:), ust_varcross(:), ust_varsquare(:)                         !< orbital velocity
   double precision, allocatable      :: vst_mean(:), vst_var(:), vst_max(:), vst_min(:), vst_varcross(:), vst_varsquare(:)                         !< orbital velocity
   double precision, allocatable      :: Fx_mean(:),Fx_var(:), Fx_min(:), Fx_max(:) , Fx_varcross(:), Fx_varsquare(:)                             !< x-comp wave forcing
   double precision, allocatable      :: Fy_mean(:),Fy_var(:), Fy_min(:), Fy_max(:), Fy_varcross(:), Fy_varsquare(:)                              !< y-comp wave forcing
   double precision, allocatable      :: E_mean(:),E_var(:), E_min(:), E_max(:), E_varcross(:), E_varsquare(:)                                  !< Bulk wave energy
   double precision, allocatable      :: R_mean(:),R_var(:), R_min(:), R_max(:), R_varcross(:), R_varsquare(:)                                  !< Bulk roller energy
   double precision, allocatable      :: D_mean(:),D_var(:), D_min(:), D_max(:), D_varcross(:), D_varsquare(:)                                  !< Bulk wave dissipation
   double precision, allocatable      :: DR_mean(:),DR_var(:), DR_min(:), DR_max(:), DR_varcross(:), DR_varsquare(:)                              !< Bulk roller dissipation
   double precision, allocatable      :: s1_mean(:),s1_var(:), s1_min(:), s1_max (:), s1_varcross(:), s1_varsquare(:)                             !< Water level
   double precision, allocatable      :: u_mean(:),u_var(:), u_min(:), u_max(:), u_varcross(:), u_varsquare(:)                              !< velocity
   double precision, allocatable      :: v_mean(:),v_var(:),v_min(:), v_max(:), v_varcross(:), v_varsquare(:)                              !< velocity
   double precision, allocatable      :: thetamean_mean(:),thetamean_var(:), thetamean_min(:), thetamean_max(:), &
                                         thetamean_varcross(:), thetamean_varsquare(:), &
                                         thetamean_sin(:), thetamean_cos(:)
   double precision, allocatable      :: cwav_mean(:),cwav_var(:), cwav_min(:), cwav_max(:), cwav_varcross(:), cwav_varsquare(:)
   double precision, allocatable      :: cgwav_mean(:),cgwav_var(:), cgwav_min(:), cgwav_max(:), cgwav_varcross(:), cgwav_varsquare(:)
   double precision, allocatable      :: sigmwav_mean(:),sigmwav_var(:), sigmwav_min(:), sigmwav_max(:), sigmwav_varcross(:), sigmwav_varsquare(:)

   double precision                   :: multcum

   integer                            :: jaavgwriteall
   integer                            :: jaavgwriteH
   integer                            :: jaavgwriteE
   integer                            :: jaavgwriteR
   integer                            :: jaavgwriteD
   integer                            :: jaavgwriteCel
   integer                            :: jaavgwriteDir
   integer                            :: jaavgwriteU
   integer                            :: jaavgwriteF
   integer                            :: jaavgwriteUrms
   integer                            :: jaavgwriteS
   integer                            :: jaavgwriteSigm

   contains

   subroutine default_xbeach_avgoutput()
      implicit none
      jaavgwriteall         = 0
      jaavgwriteH           = 0
      jaavgwriteE           = 0
      jaavgwriteR           = 0
      jaavgwriteD           = 0
      jaavgwriteCel         = 0
      jaavgwriteDir         = 0
      jaavgwriteU           = 0
      jaavgwriteF           = 0
      jaavgwriteUrms        = 0
      jaavgwriteS           = 0
      jaavgwriteSigm        = 0

   end subroutine

end module m_xbeach_avgoutput


!>
!! Trachytope module containing FM data for trachytopes.
!! Note that arrays are dimensioned on the number of net links
!! This was done so that roughness characteristics can be specified
!! not related to the location of open boundaries, which are not
!! yet available at the time the model is constructed.
!! Besides that certain sediment transport formulae require a
!! Cf at the flow node, which can only accurately be deterimined if the
!! values at net links are known.
!!
module m_trachy
 use trachytopes_data_module
 use properties
 implicit none

 type(TREE_DATA), pointer          :: trtdef_ptr                  !< Property tree structure containing the trachytopes definitions from MDU [Trachytopes] chapter.
 !
 !type(trachy_type)                 :: trachy_nl                   !< Structure containing trachytope definitions on net links
 type(trachy_type)                 :: trachy_fl                   !< Structure containing trachytope definitions on flow links
 !
 logical                           :: linit                       !< Logical for initial step of trachytopes computation (not used)
 logical                           :: waqol                       !< Logical for waq-online coupling in trachytopes computation
 logical                           :: lfdxx                       !< Logical for sediment diameters  of trachytopes computation (not used yet)
 !logical                           :: spatial_bedform             !< Logical for inclusion of spatially varying dune properties in trachytopes computation (not used yet)
 logical                           :: update_umag                 !< Logical for updating cell-centred velocity magnitude in trachytopes computation
 logical                           :: trachy_resistance = .false. !< Logical for additional resistance term in momentum equation
 !
 !double precision, allocatable     :: rhosol(:)                   !< Density of sediment (lsedtot)
 double precision, allocatable     :: sig(:)                      !< sigma layer notation as in Delft3D in trachytopes computation
 double precision, allocatable     :: umag(:)                     !< velocity magnitude in trachytopes computation (ndx)
 !double precision, allocatable     :: bedformD50(:)               !< 50-th percentile of the sediment considered for bedform in trachytopes computation (ndx)
 !double precision, allocatable     :: bedformD90(:)               !< 90-th percentile of the sediment considered for bedform in trachytopes computation (ndx)
 !double precision, allocatable     :: rksr(:)                     !< roughness due to ripples in trachytopes computation (cf. Van Rijn 20..) (ndx)
 !double precision, allocatable     :: rksmr(:)                    !< roughness due to mega-ripples in trachytopes computation (cf. Van Rijn 20..) (ndx)
 !double precision, allocatable     :: rksd(:)                     !< roughness due to dunes in trachytopes computation (cf. Van Rijn 20..)(ndx)
 !double precision, allocatable     :: dxx(:,:)                    !< sediment percentiles in trachytopes computation (cf. Van Rijn 20..) (ndx,nxx)
 double precision, allocatable     :: z0rou(:)                    !< z0rou in trachytopes computation (numl)
 double precision, allocatable     :: hu_trt(:)                   !< water depth on net links in trachytopes computation (numl)
 double precision, allocatable     :: dx_trt(:)                   !< length of net links in trachytopes computation (numl)
 !
 integer                           :: kmaxtrt                     !< number of sigma layers as in Delft3D in trachytopes computation
 !integer                           :: lsedtot                     !< number of sediment fractions in trachytopes computation
 !integer                           :: i50                         !< index of sediment percentile in trachytopes computation
 !integer                           :: i90                         !< index of sediment percentile in trachytopes computation
 integer                           :: itimtt                      !< update frequency (multiple of dt_max, default = 1 --> every dt_max timestep)
 !integer                           :: nxx                         !< dimension of sediment percentiles in trachytopes computation
 integer, allocatable              :: kcu_trt(:)                  !< temporary array for kcu on net-links instead of flow-links in trachytopes computation (numl)
 !
 character(4)                      :: rouflo                      !< roughness method as described by Delft3D in trachytopes computation
 !
contains

!> Sets ALL (scalar) variables in this module to their default values.
!! Make sure to call this at least once for each newly loaded model.
subroutine default_trachy()
   call tree_destroy(trtdef_ptr)
   linit = .false.             !< Logical for initial step of trachytopes computation (not used)
   waqol = .false.             !< Logical for waq-online coupling in trachytopes computation
   lfdxx = .false.             !< Logical for sediment diameters  of trachytopes computation (not used yet)
   !spatial_bedform = .false.   !< Logical for inclusion of spatially varying dune properties in trachytopes computation (not used yet)
   !lsedtot = 1                 !< number of sediment fractions in trachytopes computation
   !i50 = 1                     !< index of sediment percentile in trachytopes computation
   !i90 = 2                     !< index of sediment percentile in trachytopes computation
   itimtt = 1                  !< update frequency (multiple of dt_user, default = 1 --> every dt_user timestep)
   !nxx = 2                     !< dimension of sediment percentiles in trachytopes computation

end subroutine default_trachy

end module m_trachy


module m_sediment
 use precision, only: fp
 use m_rdstm, only: stmtype
 use morphology_data_module, only: sedtra_type
 use message_module, only: message_stack
 use m_waves
 implicit none

 !-------------------------------------------------- new sediment transport and morphology
 type mortmpdummy
    real(fp), dimension(:), pointer        :: uau      !< velocity asymmetry in u points
    real(fp), dimension(:), pointer        :: rhowat   !< Temporary dummy variable for flow rhowat
    real(fp), dimension(:,:), pointer      :: ws       !< Temporary variable Fall velocity
    real(fp), dimension(:,:), pointer      :: seddif   !< Temporary variable vertical sediment diffusivity
    real(fp), dimension(:,:), pointer      :: sed      !< sediment concentration
    real(fp), dimension(:), pointer        :: blchg    !< bed level change  [m]
    real(fp), dimension(:), pointer        :: dzbdt    !< bed level change rate [m/s]
    type (message_stack)    , pointer      :: messages
 end type mortmpdummy
 !
 logical                           :: stm_included  !< Include sediment transport (and optionally morphology)
 type(stmtype), target             :: stmpar        !< All relevant parameters for sediment-transport-morphology module.
 ! NOTE: bodsed and dpsed only non-NULL for stmpar%morlyr%settings%iunderlyr==1
 !$BMIEXPORT double precision      :: bodsed(:,:)   !< [kg m-2] Available sediment in the bed in flow cell center.            {"location": "face", "shape": ["stmpar%morlyr%settings%nfrac", "ndx"], "internal": "stmpar%morlyr%state%bodsed"}
 !$BMIEXPORT double precision      :: dpsed(:)      !< [m] Sediment thickness in the bed in flow cell center.                 {"location": "face", "shape": ["ndx"], "internal": "stmpar%morlyr%state%dpsed"}
 ! NOTE: msed and thlyr only non-NULL for stmpar%morlyr%settings%iunderlyr==2
 !$BMIEXPORT double precision      :: msed(:,:,:)   !< [kg m-2] Available sediment in a layer of the bed in flow cell center. {"location": "face", "shape": ["stmpar%morlyr%settings%nfrac", "stmpar%morlyr%settings%nlyr", "ndx"], "internal": "stmpar%morlyr%state%msed"}
 !$BMIEXPORT double precision      :: thlyr(:)      !< [m] Thickness of a layer of the bed in flow cell center.               {"location": "face", "shape": ["stmpar%morlyr%settings%nlyr","ndx"], "internal": "stmpar%morlyr%state%thlyr"}

 type(sedtra_type), target         :: sedtra        !< All sediment-transport-morphology fields.
 !$BMIEXPORT double precision      :: rsedeq(:,:)   !< [kg m-3] Equilibrium sediment concentration. {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%rsedeq"}
 !$BMIEXPORT double precision      :: sbcx(:,:)     !< [kg s-1 m-1] bed load transport due to currents, x-component.       {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbcx"}
 !$BMIEXPORT double precision      :: sbcy(:,:)     !< [kg s-1 m-1] bed load transport due to currents, y-component.       {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbcy"}
 !$BMIEXPORT double precision      :: sbwx(:,:)     !< [kg s-1 m-1] bed load transport due to waves, x-component.          {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbwx"}
 !$BMIEXPORT double precision      :: sbwy(:,:)     !< [kg s-1 m-1] bed load transport due to waves, y-component.          {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbwy"}

 !$BMIEXPORT double precision      :: sscx(:,:)     !< [kg s-1 m-1] suspended load transport due to currents, x-component. {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sscx"}
 !$BMIEXPORT double precision      :: sscy(:,:)     !< [kg s-1 m-1] suspended load transport due to currents, y-component. {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sscy"}
 !$BMIEXPORT double precision      :: sswx(:,:)     !< [kg s-1 m-1] suspended load transport due to waves, x-component.    {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sswx"}
 !$BMIEXPORT double precision      :: sswy(:,:)     !< [kg s-1 m-1] suspended load transport due to waves, y-component.    {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sswy"}

 type(mortmpdummy), target         :: mtd           !< Dummy quantities not yet available in D-Flow FM

 double precision, allocatable     :: sbcx_raw(:,:) !< Arrays for raw transport outputs WO
 double precision, allocatable     :: sbcy_raw(:,:)
 double precision, allocatable     :: sswx_raw(:,:)
 double precision, allocatable     :: sswy_raw(:,:)
 double precision, allocatable     :: sbwx_raw(:,:)
 double precision, allocatable     :: sbwy_raw(:,:)

 !-------------------------------------------------- old sediment transport and morphology
 integer                           :: jased         !< Include sediment, 1=Krone, 2=Soulsby van Rijn 2007, 3=Bert's morphology module
 integer                           :: jaseddenscoupling=0    !< Include sediment in rho 1 = yes , 0 = no
 double precision                  :: vismol        !< molecular viscosity (m2/s)
 integer                           :: mxgr          !< nr of grainsizes
 integer                           :: jatranspvel   !< transport velocities: 0=all lagr, 1=eul bed+lagr sus, 2=all eul; default=1
 integer, allocatable              :: sedtot2sedsus(:) !< mapping of suspended fractions to total fraction index; name is somewhat misleading, but hey, who said this stuff should make sense..
 integer                           :: sedparopt=1   !< for interactor plotting
 integer                           :: numoptsed
 integer                           :: jaBndTreatment
 integer                           :: jasedtranspveldebug
 integer                           :: jaupdates1
 integer                           :: jamorcfl
 double precision                  :: dzbdtmax
 !
 !-------------------------------------------------- old sediment transport and morphology
 integer                           :: mxgrKrone     !< mx grainsize index nr that followsKrone. Rest follows v.Rijn
 double precision, allocatable     :: D50(:)        !< mean sand diameter (m)         ! used only if Ws ==0
 double precision, allocatable     :: D90(:)        !< 90percentile sand diameter (m) ! not in Krone Partheniades
 double precision, allocatable     :: rhosed(:)     !< rho of sediment (kg/m3)
 double precision, allocatable     :: rhodelta(:)   !< relative density diff  (rhosed-rhomean)/rhomean ( )
 double precision, allocatable     :: dstar(:)      !< dimensionless particle diameter( )
 double precision, allocatable     :: dstar03(:)    !< dimensionless particle diameter( ) **-0.3d0
 double precision, allocatable     :: Ws(:)         !< Fall velocity (m/s) ( used only if D50=0)
 double precision, allocatable     :: erosionpar(:) !< Pickup erosion parameter ( kg/(m2s) ) Krone
 double precision, allocatable     :: Ustcre2(:)    !< ustar critic erosion **2  ( m2/s2)
 double precision, allocatable     :: sqsgd50(:)    !< sqrt( ((s-1)gD50) ) (m/s)
 double precision, allocatable     :: Accr(:)       !  save time
 double precision, allocatable     :: Awcr(:)       !  save time, see below
 double precision, allocatable     :: Bwcr(:)       !  save time, see below
 double precision, allocatable     :: D50ca(:), D50cb(:), D50wa(:), D50wb(:), D50wc(:) !< SvR definitions + user defined for < 0.000062 (m)
 double precision, allocatable     :: Uniformerodablethickness(:) !< Uniform erodable thickness per fraction (m)
 double precision, allocatable     :: sedini(:)            !< uniform initial sedcon     (kg/m3)


 double precision                  :: rhobulkrhosed = 1650d0/2650d0  !< rho of bulk sand / rho of sedimentmaterial
 double precision                  :: sedmax        !< user defined max sediment concentration (kg/m3)
 double precision                  :: dmorfac       ! morphological acceleration factor() , 0.0 = no bottom update, 1.0 = realtime, 10.0 = ten times faster
 double precision                  :: tmorfspinup   ! time period without morfac
 double precision                  :: alfabed=1d0   ! calibration par bed       load
 double precision                  :: alfasus=1d0   ! calibration par suspended load
 double precision                  :: crefcav=20d0  ! cref / caverage in Engelund Hansen wse = ws*crefcav

 integer                           :: jamorf        ! 0 or 1 do morf

 double precision, allocatable     :: sedh  (:)     !< help sed arr for initial
 double precision, allocatable     :: sed   (:,:)   !< sediment concentraton kg/m3 (mxgr,ndkx)
 double precision, allocatable     :: sedi  (:,:)   !< sediment concentraton increment, kg/m3 only needed for jaceneqtr == 2
 double precision, allocatable     :: sdupq  (:,:)  !< sediment flux kg/s
 double precision, allocatable     :: blinc (:)     !< bottom level increment (m)
 double precision, allocatable     :: grainlay(:,:) !< spatial erodable grain layer thickness for each grain size fraction (m)
 integer                           :: jagrainlayerthicknessspecified = 0 !< specified non-uniformly yes or no
 integer                           :: isusandorbed = 2  !< Supended and or Bedload: 1= S, 2=S+B
 integer                           :: jaceneqtr = 2     !< equilibrium transport in cell centre=1, in net nodes=2
 integer                           :: jgrtek = 1        !< grainsize fraction nr to plot
 integer                           :: numintverticaleinstein = 10 !< number of vertical intervals in einstein integrals

 double precision, allocatable     :: taucx (:)     !< cell centre tau current in x dir N/m2
 double precision, allocatable     :: taucy (:)     !< cell centre tau in y dir N/m2
 double precision, allocatable     :: tauu  (:)     !< link tau     in link dir N/m2, to cx,cy through Perot



 contains

 subroutine default_sediment()
 use m_physcoef
 implicit none

 mxgr          = 0
 mxgrKrone     = 0

 wavenikuradse = 0.01d0
 z0wav         = wavenikuradse / 30d0

 sedmax              = 30d0
 dmorfac             = 1d0
 tmorfspinup         = 0d0
 alfabed             = 1d0
 alfasus             = 1d0
 jamorf              = 0
 jaBndTreatment      = 0
 jasedtranspveldebug = 0
 jaupdates1          = 0
 jamorcfl            = 1
 dzbdtmax            = 0.1d0
 end subroutine default_sediment

 subroutine allocgrains() ! for all fractions:
 use MessageHandling
 use m_physcoef
 implicit none
 integer :: m
 double precision :: Taucre
 if (allocated (D50) ) then
    deallocate (D50, rhosed, erosionpar, Ustcre2, Ws, sedini, Uniformerodablethickness,  &
                D50ca, D50cb, D50wa, D50wb, D50wc, Bwcr  )
 endif
 if (mxgr == 0) return
 m = mxgr
 allocate (D50(m), rhosed(m), erosionpar(m), Ustcre2(m), Ws(m), sedini(m), Uniformerodablethickness(m),  &
           D50ca(m), D50cb(m), D50wa(m), D50wb(m), D50wc(m), Bwcr(m)  )
 D50           = 0.2d-3   ! 1d-3
 rhosed        = 2650.0
 erosionpar    = 1d-4                  ! krone
 Taucre        = 0.3d0
 Ustcre2       = Taucre/rhomean       ! krone, i.e. taucre = 0.3
 ws            = 3d-4
 sedini        = 0d0
 Uniformerodablethickness = 1d0
 D50ca         = 0.19d0
 D50cb         = 0.1d0
 D50wa         = 0.24d0
 D50wb         = 0.66d0
 D50wc         = 0.33d0
 Bwcr          = 0.33d0

 end subroutine allocgrains

end module m_sediment

module m_dad
   use m_dredge_data
!
! dredging related
!
   logical                           :: dad_included  !< Include dredging and dumping
   type(sv_dredge), target           :: dadpar        !< Dredging related parameters

end module m_dad

module m_bedform
   use m_bedform_data
   !
   ! bedform prediction related
   !
   logical                           :: bfm_included   !< Include bedforms
   type(bedformpar_type), target     :: bfmpar         !< Bedform related parameters
   !
end module m_bedform

 module m_grw
 integer                               :: jagrw                !< include ground water
 integer                               :: infiltrationmodel    !< 0=nogrw, 1 = Hinterceptionlayer, 2=Conductivity=constant, 3=function of pressure
 double precision, allocatable         :: sgrw0(:)             !< ground water level start
 double precision, allocatable         :: sgrw1(:)             !< ground water level end of timestep
 double precision, allocatable         :: pgrw (:)             !< pressure and plotting of sgrw
 double precision, allocatable         :: h_unsat(:)           !< initial height unsaturated zone
 double precision, allocatable         :: bgrw(:)              !< initial height unsaturated zone

 double precision, allocatable, target :: infilt(:)            !< [m3 s-1] Actual infiltration flux at current time {"location": "face", "shape": ["ndx"]}
 double precision, allocatable, target :: infiltcap(:)         !< [m s-1] Maximum infiltration capacity on each cell {"location": "face", "shape": ["ndx"]}
 double precision, allocatable         :: infiltcaproofs(:)    !< temporary of the same

 integer                               :: jaintercept2D        !< 1 = uniform, 2 = spatially variable
 double precision                      :: Hinterceptionlayer   !< thickness of interception layer in  (m) only if infiltrationmodel == 1
 double precision                      :: infiltcapuni         !< (m/s), Only used if infiltrationmodel == 2
 double precision                      :: Conductivity         !< non dimensionless K conductivity   saturated (m/s), Q = K*A*i (m3/s)
 double precision                      :: Unsatfac             !< reduction factor for conductivity in unsaturated zone

 double precision                      :: h_aquiferuni         !< uniform height of carrying layer
 double precision                      :: h_unsatini           !< initial level groundwater is bedlevel - h_unsatini
 double precision                      :: sgrwini              !< initial level groundwater. If specified, h_unsatini wiil not be used
 double precision                      :: bgrwuni              !< initial level groundwater. If specified, h_unsatini wiil not be used
 double precision                      :: h_capillair          !< Capillary rising height (m)
 double precision                      :: h_transfer           !< uniform thickness (numerical) transfer zone grw <-> openw

 double precision                      :: porosgrw             !< porosity of soil = Vair / (Vsoil+Vair)  , or,
                                                                 !< porosity of soil = (Rhoparticle - Rhobulk) / Rhoparticle
                                                                 !< e.g.
contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_grw() instead.
subroutine default_grw()
   jagrw             = 0       !< include ground water
   infiltrationmodel = 0       !< 0=nogrw, 1 = Hinterceptionlayer, 2=Conductivity=constant, 3=function of pressure
   jaintercept2D     = 0       !< 1 = uniform, 2 = spatially variable
   !Hinterceptionlayer          !< thickness of interception layer in  (m) only if infiltrationmodel == 1
   !infiltcapuni                !< (m/s), Only used if infiltrationmodel == 2
   Conductivity      = 0d-4    !< non dimensionless K conductivity   saturated (m/s), Q = K*A*i (m3/s)
   Unsatfac          = 1.0d0   !< reduction factor for conductivity in unsaturated zone

   h_aquiferuni      = 20d0    !< uniform height of carrying layer
   h_unsatini        = 0.2     !< initial level groundwater is bedlevel - h_unsatini
   sgrwini           = -999d0  !< initial level groundwater. If specified, h_unsatini wiil not be used
   bgrwuni           = -999d0  !< initial level groundwater. If specified, h_unsatini wiil not be used
   h_capillair       = 0.5     !< Capillary rising height (m)
   h_transfer        = 0.1d0   !< uniform thickness (numerical) transfer zone grw <-> openw

   porosgrw          = 0.25d0  !< porosity of soil = Vair / (Vsoil+Vair)  , or,
                               !< porosity of soil = (Rhoparticle - Rhobulk) / Rhoparticle
    ! Remaining of variables is handled in reset_grw()
    call reset_grw()
end subroutine default_grw

!> Resets only groundwater variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, call default_grw() instead.
subroutine reset_grw()
end subroutine reset_grw

 end module m_grw

 module m_nudge
    double precision, allocatable, target :: nudge_tem(:) !< 3D temperature for nudging
    double precision, allocatable, target :: nudge_sal(:) !< 3D salinity for nudging
    double precision, allocatable         :: nudge_time(:)   !< nudge relaxation time
    double precision, allocatable         :: nudge_rate(:)   !< nudge relaxation time, 1/days
    double precision, parameter           :: NUDGE_RATE_UNIT_TO_SECI = 1d0/(24d0 * 3600d0)
 end module

module m_ship
 integer                       :: nshiptxy = 0, iniship                             !< nr of ships / initialised 0,1
 integer,          allocatable :: kship(:)                                      !< index array
 double precision, allocatable, target :: xyship(:)                                     !< new position or velocity provided by module
 double precision, allocatable, target :: shx(:) !< [m] current position {"shape": ["nshiptxy"]}
 double precision, allocatable, target :: shy(:) !< [m] current position {"shape": ["nshiptxy"]}
 double precision, allocatable, target :: shi(:) !< [m] current position {"shape": ["nshiptxy"]}
 double precision, allocatable :: shu(:), shv(:), sho(:)                        !< current velocity
 double precision, allocatable, target :: zsp(:)     !< [m] ship depth at flownodes {"shape": ["ndx"]}
 double precision, allocatable, target :: zsp0(:)    !< [m] ship depth at flownodes prev step {"shape": ["ndx"]}
 double precision, allocatable, target :: zspc(:)    !< [m] ship depth at netnodes  {"shape": ["numk"]}
 double precision, allocatable, target :: zspc0(:)   !< [m] ship depth at netnodes  {"shape": ["numk"]}
 double precision, allocatable, target :: v0ship(:)  !< [m] ship 0 volume {"shape": ["ndx"]}
 double precision, allocatable, target :: v1ship(:)  !< [m] ship 1 volume {"shape": ["ndx"]}
 double precision, allocatable, target :: qinship(:) !< [m] ship flux (v1-v0)/dt  {"shape": ["ndx"]}
 double precision, allocatable, target :: vicushp(:) !< [m] eddyvisc ship {"shape": ["lnx"]}

 double precision, allocatable, target :: shL(:) !< [m] ship size L/2, B/2, D  ! for now, fixed max nr =2 {"shape": [2]}
 double precision, allocatable, target :: shB(:) !< [m] ship size L/2, B/2, D  ! for now, fixed max nr =2 {"shape": [2]}
 double precision, allocatable, target :: shd(:) !< [m] ship size L/2, B/2, D  ! for now, fixed max nr =2 {"shape": [2]}
 double precision              :: epsi=1d0
 double precision              :: fx2(2)   =0d0, fy2(2)    =0d0, fm2(2)   =0d0  !< pressure force in global coordinate sys (interacting with flow)
 double precision              :: squat(2) =0d0, squatbow(2) = 0d0              !< squat and squat bow (m)
 double precision              :: fricx (2)=0d0, fricy  (2)=0d0, fricm (2)=0d0  !< friction force in global coordinate sys (interacting with flow)
 double precision              :: fricxe(2)=0d0, fricye (2)=0d0, fricme(2)=0d0  !< friction force in global coordinate sys (interacting with flow) explicit
 double precision              :: fricxi(2)=0d0, fricyi (2)=0d0, fricmi(2)=0d0  !< friction force in global coordinate sys (interacting with flow) implicit
 double precision              :: fricxnet(2)=0d0, fricynet(2)=0d0, fricmnet(2)=0d0  !< net friction forces
 double precision              :: stuwx (2)=0d0, stuwy  (2)=0d0, stuwm (2)=0d0  !< thrust    force in global coordinate sys (interacting with flow)
 double precision              :: fextx (2)=0d0, fexty  (2)=0d0, fextm (2)=0d0  !< external  force in global coordinate sys (          not on flow)
 double precision, allocatable, target :: stuw(:)   !< [N] actual thrust force in ship dir  {"shape": [2]}
 double precision, allocatable, target :: fstuw(:)  !< [-] thrust setting 0-1 {"shape": [2]}
 double precision, allocatable, target :: stuwmx(:) !< [N] max thrust {"shape": [2]}
 double precision, allocatable, target :: roer(:)   !< [degree] actual rudder angle {"shape": [2]}
 double precision, allocatable, target :: froer(:)  !< [degree] actual rudder setting 0-1 {"shape": [2]}
 double precision, allocatable, target :: roermx(:) !< [degree] max rudder angle {"shape": [2]}
 double precision          :: dxcog(2) = 0d0                                !< delta x c.o.g.
 double precision          :: powermx(2)   , speedmx(2)                     !< mx engine power (Hp on input, then Watts), max ship velocity (Knts on input, then m/s)
 double precision          :: deadw(2)     , deadwi (2), checkdw(2)         !< inertia (x,y), moment
 double precision          :: xmxs, xmns, ymxs, ymns                        !< minmax of shipping domain
 double precision          :: Trelax = 4d0, depmin = 18d0                   !< relax period pressureforces (s), ships no deeper than depmi
 double precision          :: Cfskin = 0.0015d0                             !< skin friction coefficient tau/rho=Cfskin*Udif**2
 double precision          :: alfahull = 0d0                                !< 0d0 = pressure forcing just hydrostatic, 1.0 = plus correction previous step
 double precision          :: vicuship = 0d0                                !< increase background eddy viscosity under ship
 integer                   :: japhifromtxy      = 1                         !< for Icontroltyp 1,2 compute phi from txy yesno
 integer                   :: icontroltyp(2)    = 3                         !< 1 = prescribed t,x,y and flow blocakage sluides,
                                                                            !< 2 = prescribed t,x,y, ship
                                                                            !< 3 = prescribed t,u,v, ship
                                                                            !< 4 = keycontrolled ship
                                                                            !< 5 = keycontrolled ship plus gyring

 integer                   :: japressurehull    = 1                         !< apply pressure field on hull yes/no
 integer                   :: japrop            = 1                         !< apply propellor forces
 integer                   :: jafric            = 1                         !< apply friction forces
 integer                   :: jashfricimpl      = 1                         !< frcition forces on ship implicit yes/no
 integer                   :: ithull, ithullmx
 integer                   :: ihullmethod       = 0                         !< 0 = some analytic, 1 = arcinfo cellcentre, 2=arcinfo netnode
 integer                   :: numsmo            = 2                         !< nr of hull smooting steps
 double precision          :: wsmo              = 0.1d0                     !< smooting factor
 double precision          :: cfav              = 0.d0                      !< average skin friction
 double precision          :: Returb            = 5700d0                    !< Transition from laminar to turbulent at Reynolds = Returb

end module m_ship


module m_wind
implicit none

double precision, allocatable, target :: wx(:)    !< [m/s] wind x velocity   (m/s) at u point {"location": "edge", "shape": ["lnx"]}
double precision, allocatable, target :: wy(:)    !< [m/s] wind y velocity   (m/s) at u point {"location": "edge", "shape": ["lnx"]}
double precision, allocatable, target :: ec_pwxwy_x(:)  !< Temporary array, for comparing EC-module to Meteo1.
double precision, allocatable, target :: ec_pwxwy_y(:)  !< Temporary array, for comparing EC-module to Meteo1.
double precision, allocatable, target :: ec_pwxwy_c(:)  !< Temporary array, for comparing EC-module to Meteo1.
double precision, allocatable, target :: wcharnock(:)   !< space var charnock (-) at u point {"location": "edge", "shape": ["lnx"]}

double precision, allocatable, target :: patm(:)     !< atmospheric pressure user specified in (N/m2), internally reworked to (m2/s2)
                                                      !! so that it can be merged with tidep later and difpatm/dx = m/s2, saves 1 array , using mode = 'add'
double precision, allocatable, target :: rain(:)     !< [mm/day] rain at xz,yz {"location": "face", "shape": ["ndx"]}
double precision, allocatable, target :: evap(:)     !< [m/s] evaporation at xz,yz {"location": "face", "shape": ["ndx"]}
integer :: id_first_wind, id_last_wind  !< counters to avoid looping over all ec_etims when only interessed in wind

!!
!! Laterals
!!
integer, parameter :: ILATTP_ALL = 0 !< Type code for laterals that apply to both 2D and 1D nodes.
integer, parameter :: ILATTP_1D  = 1 !< Type code for laterals that only apply to 1D nodes.
integer, parameter :: ILATTP_2D  = 2 !< Type code for laterals that only apply to 2D nodes.

integer                      , target :: numlatsg    !< [-] nr of lateral discharge providers  {"rank": 0}
double precision, allocatable, target :: qplat(:)    !< [m3/s] Lateral discharge of provider {"shape": ["numlatsg"]}
double precision, allocatable, target :: qqlat(:)    !< [m3/s] Lateral discharge at xz,yz {"location": "face", "shape": ["ndx"]}
double precision, allocatable, target :: balat(:)    !< [m2] total area of all cells in provider numlatsg {"shape": ["numlatsg"]}
character(len=128), allocatable       :: lat_ids(:)  !< id of laterals {"shape": ["numlatsg"]}

!! Lateral lookup tables: n1/n2latsg(ilat) = n1/n2, nnlat(n1:n2) = { flow node nrs affected by lateral ilat }
integer                               :: nlatnd      !< lateral nodes dimension, counter of nnlat(:)
integer,          allocatable, target :: n1latsg(:)  !< [-] first  nlatnd point in lateral signal numlatsg {"shape": ["numlatsg"]}
integer,          allocatable, target :: n2latsg(:)  !< [-] second nlatnd point in lateral signal numlatsg {"shape": ["numlatsg"]}
integer,          allocatable, target :: nnlat(:)    !< [-] for each lateral node, flow node number == pointer to qplat/balat {"shape": ["nlatnd"]}
integer,          allocatable, target :: kclat(:)    !< [-] for each cell: 0 when not accepting lateral discharge (e.g. pipe) {"location": "face", "shape": ["ndx"]}

double precision, allocatable, target :: qinext(:)      !< [m3/s] External discharge per cell {"location": "face", "shape": ["ndkx"]}
double precision, allocatable, target :: qinextreal(:)  !< [m3/s] Realized external discharge per cell {"location": "face", "shape": ["ndkx"]}
double precision, allocatable, target :: vincum(:)      !< [m3] Cumulative realized volume through qinext {"location": "face", "shape": ["ndkx"]}

double precision, allocatable, target :: tair(:)     !< air temperature       (degC)
double precision, allocatable, target :: rhum(:)     !< air relative humidity (%)
double precision, allocatable, target :: clou(:)     !< air cloudiness        (%)
double precision, allocatable, target :: qrad(:)     !< solar radiation       (W/m2)
double precision, allocatable         :: heatsrc (:) !< resulting 2D or 3D heat source per cell (Km3/s)
double precision, allocatable         :: heatsrc0(:) !< resulting 2D or 3D heat source per cell, only set at timeuser (Km3/s)
double precision, allocatable         :: salsrc (:)  !< salinity source per cell (pptm3/s)
double precision, allocatable         :: tbed(:)     !< bed temperature       (degC)


double precision, allocatable         :: cdwcof(:)   !< wind stress cd coefficient () , only if jatemp ==5

integer         , allocatable         :: kcw (:)     !< mask array

integer                           :: jawind              !< use wind yes or no
integer                           :: japatm              !< use patm yes or no
integer                           :: jaspacevarcharn     !< use space and time varying Charnock coefficients yes or no
integer                           :: jawindstressgiven   !< wind given as stress, no conversion needed
integer                           :: jarain              !< use rain yes or no
integer                           :: jaevap              !< use evap yes or no
integer                           :: jatair              !< use air temperature   yes or no
integer                           :: jarhum              !< use relative humidity yes or no
integer                           :: jaclou              !< use cloudiness        yes or no
integer                           :: jasol = 0           !< use 1 = use solrad, 2 = use cloudiness
integer                           :: jaheat_eachstep = 0 !< if 1, do it each step, else in externalforcings (default)
integer                           :: jaQinext            !< use Qin externally provided yes or no
integer                           :: jaqin               !< use qin , sum of all in fluxes

double precision                  :: windxav, windyav  !< average wind for plotting

double precision                  :: windsp
double precision                  :: winddir         !< deg from north sailor
double precision, target          :: rainuni         !< [mm/hr] uniform rain intensity. {"rank": 0}
double precision                  :: wsx
double precision                  :: wsy
double precision                  :: rhoair          !< (kg/m3)
double precision                  :: PavBnd          !< average ambient pressure (N/m2) for correction on open boundaries
double precision                  :: PavIni          !< average ambient pressure (N/m2) for initial waterlevel correction
double precision                  :: paver           !< Average ambient pressure (N/m2)
double precision                  :: patmfac         !< 100 if Mbar, 1 if Pascal

double precision                  :: cdb(3)          !< breakpoints cd function cd coefficient
double precision                  :: wdb(3)          !< breakpoints cd function windspeed
integer                           :: ICdtyp          !< 1=Const; 2=Smith&Banke (2 pts); 3=S&B (3 pts); 4=Charnock 1955, 5=Hwang 2005, 6=Wuest 2005
integer                           :: jarelativewind  !< 1 = relative, 0 not relative
integer                           :: jawindhuorzwsbased   !< 1 = finite volume , 0 = hu
integer                           :: jawindpartialdry     !< Reduce windstress on water if link partially dry, only for bedlevtyp=3, 0 = no, 1 = yes
contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_wind() instead.
subroutine default_wind()
use m_physcoef, only : rhomean
    windsp  = 0
    winddir = 90d0        !< deg from north sailor
    rhoair  = 1.2d0
    paver   = 101325.0
    Pavini  = 0d0
    PavBnd  = 0d0         !< default: no pressure correction on open boundaries.
                          !< choose ambient pressure on boundaries equal to overall standard ambient pressure
    patmfac = 1d0         !< 100 if Mbar, 1 if Pascal

    cdb(1)  = 0.00063d0   !< first  wind breakpoint
    wdb(1)  = 0
    cdb(2)  = 0.00723d0   !< second wind breakpoint
    wdb(2)  = 100
    cdb(3)  = 0.003d0     !< third  wind breakpoint
    wdb(3)  = 30
    icdtyp  = 2
    jarelativewind = 0    !< wind relative
    jawindhuorzwsbased   = 0    !< default: HU-based both in 2D and 3D (and not zws-based)
    jawindpartialdry     = 1    !< default: partially dry cells switched off

    windxav = 0d0
    windyav = 0d0

    ! Rain+qin+wind not reset every re-init, only upon new MDU load, because rain can be
    ! enabled by user in MDU (for BMI use, even without rain in external forcings file)
    jarain  = 0         !< use rain yes or no
    jaevap  = 0         !< use evap yes or no
    jaqin   = 0         !< use qin , sum of all in fluxes
    jaQinext= 0         !< use Qin externally provided yes or no
    jawind  = 0         !< use wind yes or no

    ! Remaining of variables is handled in reset_wind()
    call reset_wind()
   end subroutine default_wind

   !> Resets only wind variables intended for a restart of flow simulation.
   !! Upon loading of new model/MDU, call default_wind() instead.
   subroutine reset_wind()
   japatm   = 0           !< use patm yes or no
   numlatsg = 0           !< [] nr of lateral discharge providers
   nlatnd   = 0           !< lateral nodes dimension, counter of nnlat(:)
   jaspacevarcharn = 0   !< use space varying Charnock coefficients
   jawindstressgiven = 0 !< wind stress given in meteo file
   end subroutine reset_wind
end module m_wind


 module m_heatfluxes
 implicit none

 double precision                  :: albedo          ! reflection coefficient of water () at average incidence angle of 60 deg,
                                                      ! (albedo is .025 at angle 0 deg, 0.13 at angle 70 deg)
 double precision                  :: em              ! Emissivity ()
 double precision                  :: cpa             ! Specific heat air   [J/kg/K]
 double precision                  :: rcpa            !
 double precision                  :: cpw             ! Specific heat water [J/kg/K]
 double precision                  :: rcpi            ! m3K/J
 double precision                  :: stf             ! Stefan's constant =5.67e-8 [W/m^2/K^4]
 double precision                  :: emstf           ! Em*Stf [W/m^2/K^4]
 double precision                  :: tkelvn          ! Absolute zero

 double precision                  :: QSUNav          ! Solar influx              (W/m2)
 double precision                  :: QEVAav          ! Evaporative heat loss     (W/m2)
 double precision                  :: QCONav          ! Convective heat loss      (W/m2)
 double precision                  :: QLongav         ! Long wave back radiation  (W/m2)
 double precision                  :: Qfreeav           ! Free conv + evap heat loss (W/m2)
 double precision                  :: Qfrconav        ! Free convection heat loss (W/m2)
 double precision                  :: Qfrevaav        ! Free evaporation heat loss (W/m2)

 double precision                  :: sarea           ! Only for excess temp model jatem=3, lake area
 double precision                  :: fwind           ! Only for excess temp model jatem=3, wind factor

 integer                           :: jamapheatflux   !< write heatfluxes to map
 integer                           :: jaRichardsononoutput !< write Richardson nr to his
 integer                           :: jaSecchisp           !< Spatial Secchi 0,1
 integer                           :: jaRoro               !< Use roair(n)/rho(ntop) in windstress 0,1

 double precision, allocatable     :: Qsunmap(:)
 double precision, allocatable     :: Qevamap(:)
 double precision, allocatable     :: Qconmap(:)
 double precision, allocatable     :: Qlongmap(:)
 double precision, allocatable     :: Qfrevamap(:)
 double precision, allocatable     :: Qfrconmap(:)
 double precision, allocatable     :: Qtotmap(:)

 double precision, allocatable     :: Rich(:)
 double precision, allocatable     :: Secchisp(:)
 double precision, allocatable     :: Roair(:)

contains

subroutine default_heatfluxes()
use m_physcoef, only : rhomean
use m_wind    , only : rhoair
                                      !< Heat flux model comnstants
albedo  = 0.06d0                      !< reflection coefficient of water () at average incidence angle of 60 deg,
                                      !< (albedo is .025 at angle 0 deg, 0.13 at angle 70 deg)
em      = 0.985d0                     !< Emissivity ()
cpa     = 1004d0                      !< Specific heat air   [J/kg/K]
rcpa    = rhoair*cpa                  !
cpw     = 3986d0                      !< Specific heat water [J/kg/K]
rcpi    = 1d0/(rhomean*cpw)           !< [m3K/J] or mKs2/kg
stf     = 5.67d-8                     !< Stefan's constant =5.67e-8 [W/m^2/K^4]
emstf   = em*stf
tkelvn  = 273.15d0                    !< Absolute 0

jamapheatflux = 0
jaRichardsononoutput = 0
jaroro = 0

end subroutine default_heatfluxes


end module m_heatfluxes


   ! todo: MERGE THIS WITH UNSTRUC_BOUNDARIES
module m_bnd   !< boundary-type module
implicit none
   integer, parameter :: NAMLEN = 128

   type bndtype
      character(len=NAMLEN)                                 :: name     !< boundary-type name
      integer                                               :: N        !< number of boundary points
      double precision, dimension(:),   allocatable         :: x        !< inner node x-coordinates
      double precision, dimension(:),   allocatable         :: y        !< inner node y-coordinates
      double precision, dimension(:),   allocatable         :: sigma    !< sigma-values
      double precision, dimension(:),   allocatable         :: zminmax  !< zmin and zmax
      double precision, dimension(:),   allocatable         :: z        !< boundary condition values
      double precision, dimension(:,:), allocatable         :: xy2      !< outer-node (x,y)-coordinates
      integer,          dimension(:),   allocatable         :: kd       !< boundary points
      integer,          dimension(:,:), allocatable         :: k        !< index array, see e.g. kbnd
      double precision, dimension(:),   allocatable         :: tht      !< Thatcher-Harleman outflow time
      double precision, dimension(:),   allocatable         :: thz      !< Thatcher-Harleman concentration
   end type bndtype

   contains

!> deallocate boundary-type
   subroutine dealloc_bnd(bnd)
      implicit none

      type(bndtype), intent(inout) :: bnd       !< boundary data

      if ( allocated(bnd%x)     ) deallocate(bnd%x)
      if ( allocated(bnd%y)     ) deallocate(bnd%y)
      if ( allocated(bnd%sigma) ) deallocate(bnd%sigma )
      if ( allocated(bnd%zminmax)) deallocate(bnd%zminmax )
      if ( allocated(bnd%z)     ) deallocate(bnd%z)
      if ( allocated(bnd%xy2)   ) deallocate(bnd%xy2)
      if ( allocated(bnd%kd)    ) deallocate(bnd%kd)
      if ( allocated(bnd%k)     ) deallocate(bnd%k)

      return
   end subroutine dealloc_bnd

!> (re)allocate boundary-type
   subroutine alloc_bnd(N, kmx, bnd)
      implicit none

      integer,       intent(in)    :: N         !< number of boundary points
      integer,       intent(in)    :: kmx       !< maximum number of layers
      type(bndtype), intent(inout) :: bnd       !< boundary data

      call dealloc_bnd(bnd)

      if ( N.gt.0 ) then
         allocate(bnd%x(N))
         allocate(bnd%y(N))

         allocate(bnd%xy2(2,N))
         allocate(bnd%kd(N))
         allocate(bnd%k(5,N))

         bnd%x   = 0d0
         bnd%y   = 0d0
         bnd%xy2 = 0d0
         bnd%kd  = 0
         bnd%k   = 0

         if ( kmx.gt.0 ) then
            allocate(bnd%sigma(kmx*N))
            allocate(bnd%zminmax(2*N))
            allocate(bnd%z(kmx*N))
            bnd%sigma = 0d0
            bnd%z     = 0d0
         else
            allocate(bnd%z(N))
            bnd%z = 0d0
         end if
      end if

      return
   end subroutine alloc_bnd

   !> deallocate bndtr
   subroutine dealloc_bndarr(bndarr)
      implicit none

      type(bndtype), dimension(:), allocatable, intent(inout) :: bndarr

      integer :: i, numbnd


      if ( allocated(bndarr) ) then
         numbnd = ubound(bndarr,1)
         do i=1,numbnd
            call dealloc_bnd(bndarr(i))
         end do
         deallocate(bndarr)
      end if

      return
   end subroutine dealloc_bndarr

   end module m_bnd


!> A cross-section path is defined by a polyline.
!! On the unstructured grid it then results in a set of flow links that
!! cross the polyline (both 1D and 2D).
!! Used for cross sections, and thin dams and dykes.
module m_crspath
implicit none

!> Data type for storing the the polyline path and set of crossed flow
!! links.
type tcrspath
    integer                       :: np            !< Nr of polyline points
    integer                       :: lnx           !< Nr. of flow links that cross the crs path
    integer, allocatable          :: ln(:)         !< Flow links (size=len) (sign defines orientation)
    integer, allocatable          :: indexp(:)     !< Index of segment in xp by which each link is crossed.
                                                   !! (between xp(i) and xp(i+1))
    double precision, allocatable :: wfp(:)        !< Weightfactor of first point in crossed segment
                                                   !! as indicated in indexp (between 0 and 1).
    double precision, allocatable :: xp(:), yp(:), &
                                     zp(:)         !< Polyline points that define the crs (size=np)
    double precision, allocatable :: xk(:,:), yk(:,:) !< For plotting only (size=2,lnx).
                                                   !! for all 'lnx' flow links, store both start
                                                   !! and end point because segments will not be ordered
                                                   !! nor connected.
    integer,          allocatable :: iperm(:)      !! permutation array of crossed flow links in increasing arc length order along cross section polyline
    double precision, allocatable :: sp(:)         !! polygon arclength of flow link, dim()
    double precision, allocatable :: wfk1k2(:)     !! per-flowlink interpolation weight factor between k1 (1) and k2 (0), dim(lnx)
end type tcrspath

contains

!> Allocates the internal data for one crs path.
!! Based on polyline length and flow links upper limit.
subroutine increaseCrossSectionPath(path, maxnp, maxlnx)
use m_alloc
    type(tcrspath), intent(inout) :: path   !< The path structure of a cross section.
    integer,        intent(in)    :: maxnp  !< Max number of polyline points. If 0, nothing is done.
    integer,        intent(in)    :: maxlnx !< Max number of crossed flow links. If 0, nothing is done.

    integer :: m, mcur

    mcur = 0
    if (allocated(path%xp)) then
        mcur = size(path%xp)
    end if

    if (maxnp > 0 .and. maxnp > mcur) then
        m = max(2, int(1.5d0*maxnp))
        call realloc(path%xp, m)
        call realloc(path%yp, m)
        call realloc(path%zp, m)
    end if

    mcur = 0
    if (allocated(path%ln)) then
        mcur = size(path%ln)
    end if

    if (maxlnx > 0 .and. maxlnx > mcur) then
        m = max(5, int(1.5d0*maxlnx))
        call realloc(path%ln,     m)


! GD: memory problems with realloc
     if (allocated(path%xk)) then
        call realloc(path%xk, (/2,m/))
        call realloc(path%yk, (/2,m/))
     else
        allocate(path%xk(2,m))
        allocate(path%yk(2,m))
     end if

        !if(allocated(path%xk)) deallocate(path%xk)
        !allocate(path%xk(2,m))

        !if(allocated(path%yk)) deallocate(path%yk)
        !allocate(path%yk(2,m))



        call realloc(path%indexp, m)
        call realloc(path%wfp,    m)
        call realloc(path%wfk1k2, m)
        call realloc(path%sp,     m)
        call realloc(path%iperm,  m)
    end if
end subroutine increaseCrossSectionPath


!> Deallocates the internal data for one crs path.
subroutine deallocCrossSectionPath(path)
    type(tcrspath), intent(inout) :: path !< The path structure of a cross section

    if (allocated(path%xp)) then
        deallocate(path%xp)
        deallocate(path%yp)
        deallocate(path%zp)
    end if
    if (allocated(path%ln)) then
        deallocate(path%ln)
        deallocate(path%indexp)
        deallocate(path%wfp)
    end if
    if (allocated(path%xk)) then
        deallocate(path%xk, path%yk)
    end if
    if (allocated(path%sp)) then
        deallocate(path%sp)
    end if
    if (allocated(path%wfk1k2)) then
        deallocate(path%wfk1k2)
    end if
    if (allocated(path%iperm)) then
        deallocate(path%iperm)
    end if
end subroutine deallocCrossSectionPath


!> Sets the cross section definition path to specified polyline coordinates.
subroutine setCrossSectionPathPolyline(path, xp, yp, zp)
    type(tcrspath),   intent(inout) :: path         !< The crs path to be updated.
    double precision, intent(in)    :: xp(:), yp(:) !< Polyline coordinates to define the crs path.
    double precision, optional, intent(in) :: zp(:) !< Optional z-values at xp/yp coordinates.

    integer :: i, n

    n = size(xp)
    if (n <= 0) return

    call increaseCrossSectionPath(path, n, 0)
    do i=1,n
        path%xp(i) = xp(i)
        path%yp(i) = yp(i)
    end do

    if (present(zp)) then
        do i=1,n
            path%zp(i) = zp(i)
        end do
    end if

    path%np = n
end subroutine setCrossSectionPathPolyline


!> Copies a crspath into another, allocating memory for all points and links.
! AvD: TODO: repeated copying will increase the xp and ln arrays (because of grow factor)
subroutine copyCrossSectionPath(pfrom, pto)
    type(tcrspath), intent(in)    :: pfrom
    type(tcrspath), intent(inout) :: pto

    !integer :: maxnp, maxlnx
    !
    !if (allocated(pfrom%xp)) then
    !   maxnp  = size(pfrom%xp)
    !else
    !   maxnp = 0
    !end if
    !
    !if (allocated(pfrom%ln)) then
    !   maxlnx = size(pfrom%ln)
    !else
    !   maxlnx = 0
    !end if
    !
    !call increaseCrossSectionPath(pto, maxnp, maxlnx)

    ! Structures may directly be copied, including their allocatable components (F2003)
    pto = pfrom
end subroutine copyCrossSectionPath


!> Increases the size of an *array* of crspath elements.
!! All existing elements (up to #numcur) are copied.
subroutine increaseCRSPaths(paths, numnew, numcur)
    type(tcrspath), allocatable, intent(inout) :: paths(:)
    integer,                     intent(inout) :: numnew !< Desired new size (may turn out larger).
    integer,                     intent(in)    :: numcur !< Current nr of paths in array
                                                         !! (will be copied, actual array size may be larger)


    type(tcrspath), allocatable :: pathst(:)
    integer :: i, numcurmax

    if (allocated(paths)) then
        numcurmax = size(paths)
        if (numnew < numcurmax) then
            return
        end if
    else
        numcurmax = 0
    end if
    numnew    = max(numnew, int(numcurmax*1.2))

    ! Allocate temp array of cross section paths.
    allocate(pathst(numcur))

    ! Fill temp paths and deallocate each original cross section path.
    do i=1,numcurmax
        if (i <= numcur) then
            call copyCrossSectionPath(paths(i), pathst(i))
        end if
        call deallocCrossSectionPath(paths(i))
    end do
    ! Deallocate original crspath array
    if (allocated(paths)) then
        deallocate(paths)
    end if

    ! Re-allocate original crspath array at bigger size and fill it.
    allocate(paths(numnew))
    do i=1,numcur
        call copyCrossSectionPath(pathst(i), paths(i))
        call deallocCrossSectionPath(pathst(i))
    end do
    deallocate(pathst)
end subroutine increaseCRSPaths


!> Check for crossing of a (flow) link by a crs path.
!! When crossed, the link info (its number and coordinates) are stored
!! in the path structure. Any existing link info is preserved!
!! This routine can be used with 'network geometry' (e.g. for thin dams)
!! and 'flow geometry' (e.g. for cross sections and fixed weirs).
subroutine crspath_on_singlelink(path, linknr, xk3, yk3, xk4, yk4, xza, yza, xzb, yzb)

   use geometry_module, only: crossinbox
   use m_sferic, only: jsferic
   use m_missing, only : dmiss
   implicit none

   type(tcrspath),   intent(inout) :: path   !< Path that is checked for link crossing, will be updated with link info.
    integer,          intent(in)    :: linknr !< Number of link that is being checked, will be stored in path%ln
    double precision, intent(in)    :: xk3, yk3, xk4, yk4 !< Net node coordinates of this link (or fictious coords for a 1D link)
    double precision, intent(in)    :: xza, yza, xzb, yzb !< cell circum. coordinates of this link.

    integer :: ip, jacros
    double precision :: SL, SM, XCR, YCR, CRP

!   Check whether flow link intersects with a polyline segment of this cross section path.
    do ip=1,path%np-1
        crp = 0d0
        CALL CROSSinbox(path%XP(ip), path%YP(ip), path%XP(ip+1), path%YP(ip+1), xza, yza, xzb, yzb, jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
        if (jacros == 1) then
            if (SM == 1d0) then
               if (crp > 0d0) then
                  cycle
               end if
            else if (SM == 0d0) then
               if (crp < 0d0) then
                  cycle
               end if
            end if

            call increaseCrossSectionPath(path, 0, path%lnx+1)
            path%lnx = path%lnx + 1

            path%indexp(path%lnx) =  ip
            path%wfp(path%lnx)    =  1d0-SL ! SL=rel.pos on segment. Weight of left points is 1-SL
            path%wfk1k2(path%lnx) =  1d0-SM ! SM=rel.pos on flow link       of left points is 1-SM

            if (crp < 0d0) then
                path%ln(path%lnx)   =  linknr
                path%xk(1,path%lnx) = xk3
                path%yk(1,path%lnx) = yk3
                path%xk(2,path%lnx) = xk4
                path%yk(2,path%lnx) = yk4
            else
!               Flip flow link orientation, such that its flow direction is rightward through crs path polygon
                path%ln(path%lnx) = -linknr
                path%xk(1,path%lnx) = xk4
                path%yk(1,path%lnx) = yk4
                path%xk(2,path%lnx) = xk3
                path%yk(2,path%lnx) = yk3
            end if


        endif
    enddo
end subroutine crspath_on_singlelink

!> Converts a set of polylines into paths.
!! The input arrays (xpl, ypl, zpl) have the structure of the global polygon:
!! one or more polylines separated by dmiss values.
subroutine pol_to_flowlinks(xpl, ypl, zpl, npl, ns, paths)
    use m_missing

    double precision, intent(in)    :: xpl(:), ypl(:), zpl(:) !< Long array with one or more polylines, separated by dmiss
    integer,          intent(in)    :: npl                    !< Total number of polyline points
    type (tcrspath),  allocatable   :: paths(:)
    integer, intent(out)            :: ns


    integer :: i, i1, i2, maxfxw

    ns = 0

    i1 = 1 ! First possible start index
    i2 = 0 ! No end index found yet.
    do i = 1,npl
        if (xpl(i) == dmiss .or. i == npl) then
            if (i == npl .and. xpl(i) /= dmiss) then
                i2 = i ! Last polyline, no dmiss separator, so also include last point #npl.
            end if
            if (i1 <= i2) then
                maxfxw = ns+1
                call increaseCRSPaths(paths, maxfxw, ns)
                ns = ns+1
                call setCrossSectionPathPolyline(paths(ns), xpl(i1:i2), ypl(i1:i2), zpl(i1:i2))
            end if
            i1 = i+1
            cycle
        else
            i2 = i ! Advance end point by one.
        end if
    end do
end subroutine pol_to_flowlinks

end module m_crspath


! unstruc.f90
 module m_flowexternalforcings
 use m_wind
 use m_nudge
 use m_bnd
 implicit none


 logical                           :: success           !< want je wil maar liever succes
 integer                           :: jatimespace       !< doen ja/nee 1/0
 integer                           :: mhis              !< unit nr external forcings history *.exthis
 integer, save                     :: kx, filetype, mext
 character(len=256)                :: qid
 character(len=1)                  :: operand
 integer                           :: numbnp            !< total nr of open boundary cells for network extension
 integer                           :: jaoldrstfile      !< using old-version rst file, which does not contain boundary info
 ! For postprocessing, each boundary polyline is named as a open boundary section.
 ! For each section, all underlying netlinks are administered, along with the name of the pli file.
 integer, parameter                :: IBNDTP_UNKNOWN = -1 !< Uninitialized boundary type/fill value
 integer, parameter                :: IBNDTP_CLOSED  =  0 !< Closed boundary (not used yet)
 integer, parameter                :: IBNDTP_ZETA    =  1 !< Water level boundary
 integer, parameter                :: IBNDTP_U       =  2 !< Velocity boundary (not detailed in q/u/normal/.. yet)
 integer, parameter                :: IBNDTP_1D2D    =  3 !< Special 1D2D boundary

 integer                           :: nopenbndsect      !< Nr. of open boundary sections.
 integer, allocatable              :: openbndtype(:)    !< (nopenbndsect) Type of this boundary section (one of IBNDTP_ZETA, etc...)
 integer, allocatable              :: nopenbndlin(:)    !< (nopenbndsect) Nr. of links/exchanges per section.
 integer, allocatable              :: openbndlin(:)     !< (sum(nopenbndlin(1:nopenbndsect)) Net link nrs for each open boundary.
 character(len=256), allocatable   :: openbndname(:)    !< (nopenbndsec)
 character(len=256), allocatable   :: openbndfile(:)    !< (nopenbndsec)

 integer,          parameter       :: NTRANSFORMCOEF=26
 double precision                  :: transformcoef(NTRANSFORMCOEF) !< Transform coefficients a+b*x

 integer         , allocatable     :: kez  (:)          !< temp (numl) edge oriented z lev
 integer         , allocatable     :: keu  (:)          !< temp (numl) edge oriented u vel
 integer         , allocatable     :: kes  (:)          !< temp (numl) edge oriented s sal
 integer         , allocatable     :: ketm (:)          !< temp (numl) edge oriented tm sem
 integer         , allocatable     :: kesd (:)          !< temp (numl) edge oriented sd sed
 integer         , allocatable     :: ket  (:)          !< temp (numl) edge oriented t tang.  vel.
 integer         , allocatable     :: keuxy(:)          !< temp (numl) edge oriented uxuy vel.
 integer         , allocatable     :: ken  (:)          !< temp (numl) edge oriented n normal vel.
 integer         , allocatable, target     :: ke1d2d(:) !< temp (numl) edge oriented 1d2d bnd
 integer         , allocatable     :: keg  (:)          !< temp (numl) edge oriented g gate
 integer         , allocatable     :: ked  (:)          !< temp (numl) edge oriented d cdam
 integer         , allocatable     :: kegen(:)          !< temp (numl) edge oriented general structure
 integer         , allocatable     :: kegs (:)          !< temp (numl) edge oriented general structure new style
 integer         , allocatable     :: kep  (:)          !< temp (numl) edge oriented p pump
 integer         , allocatable     :: keklep(:)         !< temp (numl) edge oriented check valve
 integer         , allocatable     :: kevalv(:)         !< temp (numl) edge oriented check valve
 integer         , allocatable     :: kew  (:)          !< temp (numl) edge oriented w waves
 integer         , allocatable     :: ketr (:,:)        !< temp (numl) edge oriented tracer
 integer         , allocatable     :: kesf (:,:)        !< temp (numl) edge oriented sedfrac
 integer         , allocatable     :: kedb (:)          !< temp (numl) edge oriented dambreak

 integer,          allocatable     :: itpez(:)          !< temp (numl) edge oriented,
                                                        !! 1,*=boundary typ, see type indicator kbndz(4,*) below
 integer,          allocatable     :: itpeu(:)          !< hulp (numl) edge oriented,
                                                        !! 1,*=boundary typ, see type indicator kbndz(4,*) below
 integer,          allocatable     :: itpenz(:)         !< temp (numl) edge oriented,
                                                        !! 1,*=boundary number (nopenbndsect), see type indicator kbndz(5,*) below
 integer,          allocatable     :: itpenu(:)         !< temp (numl) edge oriented,
                                                        !! 1,*=boundary number (nopenbndsect), see type indicator kbndu(5,*) below
 double precision, allocatable     :: ftpet(:)

 double precision, allocatable     :: threttim(:,:)     !< (NUMCONST,nopenbndsect) Thatcher-Harleman return times

 character(len=256), allocatable   :: thrtq(:)          !< temp array for Thatcher-Harleman return time readout, stores constituents
 double precision, allocatable     :: thrtt(:)          !< temp array for Thatcher-Harleman return time readout, stores return times
 integer,          allocatable     :: thrtn(:)          !< temp array for Thatcher-Harleman return time readout, stores cell indices (first one)

 integer                           :: nzbnd             !< number of waterlevel boundary segments
 integer                           :: nbndz             !< waterlevel boundary points dimension
 double precision, allocatable     :: xbndz(:)          !< waterlevel boundary points xcor
 double precision, allocatable     :: ybndz(:)          !< waterlevel boundary points ycor
 double precision, allocatable, target :: zbndz(:)      !< [m] waterlevel boundary points function  {"location": "edge", "shape": ["nbndz"]}
 double precision, allocatable     :: zbndz0(:)         !< waterlevel boundary points function
 double precision, allocatable     :: xy2bndz(:,:)      !< waterlevel boundary 'external tolerance point'
 integer         , allocatable     :: kdz  (:)          !< waterlevel boundary points temp array
 integer         , allocatable     :: kbndz(:,:)        !< waterlevel boundary points index array
                                                        !! 1,* = index in s1 boundary point
                                                        !! 2,* = index in s1 first point on the inside
                                                        !! 3,* = index in u1 of their connecting link (always positive to the inside)
                                                        !! 4,* = type indicator :
                                                        !!                        1 = waterlevel boundary
                                                        !!                        2 = waterlevel neumann
                                                        !!                        3 = velocity   normal ingoing component
                                                        !!                        4 = velocity   flux boundary
                                                        !!                        5 = velocity   Riemann boundary
                                                        !!                        6 = waterlevel outflow
                                                        !! 5,* = member of boundary number somuch of this type
                                                        !! 6,* = riemann relaxation time for this point (s)
 double precision, allocatable     :: zkbndz(:,:)       !< only for jaceneqtr == 2 : left and right vertical netnode zk levels
 double precision                  :: zbndzval1=-999d0, zbndzval2 = -999d0
 integer         , allocatable     :: kbanz(:,:)        !< ban pointer 2,*

 integer                           :: nubnd             !< number of velocity boundary segments
 integer                           :: nbndu             !< velocity   boundary points dimension
 double precision, allocatable     :: xbndu(:)          !< velocity   boundary points xcor
 double precision, allocatable     :: ybndu(:)          !< velocity   boundary points ycor
 double precision, allocatable, target :: zbndu(:)      !< [m/s] velocity   boundary points function   {"location": "edge", "shape": ["nbndu"]}
 double precision, allocatable, target :: zbndq(:)      !< [m3/s] discharge  boundary points function   {"location": "edge", "shape": ["nbndu"]}
 double precision, allocatable     :: zbndu0(:)         !< velocity   boundary points function in start time
 double precision, allocatable     :: xy2bndu(:,:)      !< velocity   boundary 'external tolerance point'
 integer         , allocatable     :: kdu  (:)          !< velocity   boundary points temp array
 integer         , allocatable     :: kbndu(:,:)        !< velocity   boundary points index array, see lines above
 integer         , allocatable     :: L1qbnd(:)         !< first  nbndu point in discharge bnd nqbnd
 integer         , allocatable     :: L2qbnd(:)         !< second nbndu point in discharge bnd nqbnd
 double precision, allocatable     :: at_all(:)         !< "at" for all qbnd's, dim(nqbnd)
 double precision, allocatable     :: at_sum(:)         !< "at" for all qbnd's, summed over all domains, dim(nqbnd)
 double precision, allocatable     :: wwssav_all(:,:)   !< "wwav" and "ssav" for all qnbd's, dim(2,nqbnd)
 double precision, allocatable     :: wwssav_sum(:,:)   !< "wwav" and "ssav" for all qnbd's, summed over all domains, dim(2,nqbnd)
 integer                           :: japartqbnd        !< one or more of the discharge boundaries is partitioned (1) or not (0)
 double precision, allocatable     :: huqbnd(:)         !< hu used in normalised Manning discharge boundary condition, based on average water-level
 integer                           :: nqbnd             !<
 double precision                  :: qbndhutrs = 0.1d0 !< only discharge bnd here if hu>qbndhutrs
 double precision, allocatable     :: zkbndu(:,:)       !< only for jaceneqtr == 2 : left and right vertical netnode zk levels
 integer         , allocatable     :: kbanu(:,:)        !< ban pointer 2,*

 integer                           :: nbnds             !< salinity   boundary points dimension in 1D and 2D
 double precision, allocatable     :: xbnds(:)          !< salinity   boundary points xcor
 double precision, allocatable     :: ybnds(:)          !< salinity   boundary points ycor
 double precision, allocatable, target :: zminmaxs(:)   !< salinity   boundary points zmin and zmax
 double precision, allocatable, target :: sigmabnds(:)  !< salinity   boundary points sigma coordinates (for now: dim = (nbnds*kmx) )
 double precision, allocatable, target :: zbnds(:)      !< salinity   boundary points function
 double precision, allocatable     :: xy2bnds(:,:)      !< salinity   boundary 'external tolerance point'
 integer         , allocatable     :: kds  (:)          !< satinity   boundary points temp array
 integer         , allocatable     :: kbnds(:,:)        !< salinity   boundary points index array, see lines above
 double precision, allocatable     :: thtbnds(:)        !< salinity Thatcher-Harleman outflow times (dim = (nbnds))
 double precision, allocatable     :: thzbnds(:)        !< salinity Thatcher-Harleman outflow concentrations (dim = (nbnds*kmx))

 integer                           :: nbndw             !< wave    boundary points dimension
 double precision, allocatable     :: xbndw(:)          !< wave    boundary points xcor
 double precision, allocatable     :: ybndw(:)          !< wave    boundary points ycor
 double precision, allocatable     :: zbndw(:,:)        !< wave    boundary points function
 double precision, allocatable     :: xy2bndw(:,:)      !< wave    boundary 'external tolerance point'
 integer         , allocatable     :: kdw  (:)          !< wave    boundary points temp array
 integer         , allocatable     :: kbndw(:,:)        !< wave    boundary points index array, see lines above
 integer         , allocatable     :: L1wbnd(:)         !< first  nbndw point in wave-energy bnd nwbnd
 integer         , allocatable     :: L2wbnd(:)         !< second nbndw point in wave-energy bnd nwbnd

 integer                           :: nbndtm            !< temperature boundary points dimension
 double precision, allocatable     :: xbndtm(:)         !< temperature boundary points xcor
 double precision, allocatable     :: ybndtm(:)         !< temperature boundary points ycor
 double precision, allocatable, target :: zminmaxTM(:)  !< temperature boundary points zmin and zmax
 double precision, allocatable, target :: sigmabndTM(:) !< temperature boundary points sigma coordinates
 double precision, allocatable, target :: zbndtm(:)     !< temperature boundary points function
 double precision, allocatable     :: xy2bndtm(:,:)     !< temperature external tolerance point'
 integer         , allocatable     :: kdtm  (:)         !< temperature boundary points temp array
 integer         , allocatable     :: kbndtm(:,:)       !< temperature boundary points index array, see lines above
 double precision, allocatable     :: thtbndtm(:)       !< temperature Thatcher-Harleman outflow times
 double precision, allocatable     :: thzbndtm(:)       !< temperature Thatcher-Harleman outflow concentrations

 integer                           :: nbndsd            !< sediment   boundary points dimension
 double precision, allocatable     :: xbndsd(:)         !< sediment   boundary points xcor
 double precision, allocatable     :: ybndsd(:)         !< sediment   boundary points ycor
 double precision, allocatable, target :: zminmaxsd(:)  !< sediment   boundary points zmin and zmax
 double precision, allocatable, target :: sigmabndsd(:) !< sediment   boundary points sigma coordinates
 double precision, allocatable, target :: zbndsd(:)     !< sediment   boundary points function
 double precision, allocatable     :: xy2bndsd(:,:)     !< sediment   boundary 'external tolerance point'
 integer         , allocatable     :: kdsd  (:)         !< sediment   boundary points temp array
 integer         , allocatable     :: kbndsd(:,:)       !< sediment   boundary points index array, see lines above
 double precision, allocatable     :: thtbndsd(:)       !< sediment Thatcher-Harleman outflow times
 double precision, allocatable     :: thzbndsd(:)       !< sediment Thatcher-Harleman outflow concentrations

 integer,          allocatable     :: nbndtr(:)           !< tracer boundary points dimension
 integer                           :: nbndtr_all          !< all tracer boundary points dimension (max(nbndtr))
 integer                           :: numtracers        !< number of tracers with boundary conditions
 integer,          parameter       :: NAMTRACLEN = 128
 character(len=NAMTRACLEN), allocatable :: trnames(:)   !< tracer names (boundary conditions only, used for look-up)
 character(len=NAMTRACLEN), allocatable :: trunits(:)   !< tracer units
 type(bndtype),    allocatable, target  :: bndtr(:)
 double precision, allocatable          :: wstracers(:) !< tracer fall velocity pos is downward (m/s)
 double precision, allocatable          :: decaytimetracers(:) !< tracer decaytimes (s)
 integer                                :: jadecaytracers      !< 0 = no, 1 =yes 

 ! JRE sedfracbnds
 integer,          allocatable          :: nbndsf(:)         !< sedfrac   boundary points dimension
 integer                                :: nbndsf_all        !< all sedfrac boundary points dimension (max(nbndsf))
 integer                                :: numfracs          !< number of fractions with boundary conditions
 integer,          parameter            :: NAMSFLEN = 128
 character(len=NAMSFLEN), allocatable   :: sfnames(:)   !< sedfrac names (boundary conditions only, used for look-up)
 type(bndtype),    allocatable, target  :: bndsf(:)
 !\ sedfracbnds

 integer                           :: nbndt             !< tang.velocity boundary points dimension
 double precision, allocatable     :: xbndt(:)          !< tang.velocity boundary points xcor
 double precision, allocatable     :: ybndt(:)          !< tang.velocity boundary points ycor
 double precision, allocatable, target :: zbndt(:)      !< tang.velocity boundary points function
 double precision, allocatable     :: xy2bndt(:,:)      !< tang.velocity boundary 'external tolerance point'
 integer         , allocatable     :: kdt  (:)          !< tang.velocity boundary points temp array
 integer         , allocatable     :: kbndt(:,:)        !< tang.velocity boundary points index array, see lines above

 integer                           :: nbnduxy           !< uxuyadvectionvelocity boundary points dimension
 double precision, allocatable     :: xbnduxy(:)        !< uxuyadvectionvelocity boundary points xcor
 double precision, allocatable     :: ybnduxy(:)        !< uxuyadvectionvelocity boundary points ycor
 double precision, allocatable, target :: zminmaxuxy(:)  !< uxuyadvectionvelocity boundary points zmin and zmax
 double precision, allocatable, target :: sigmabnduxy(:) !< uxuyadvectionvelocity boundary points sigma coordinates (for now: dim = (nbnds*kmx) )
 double precision, allocatable, target :: zbnduxy(:)    !< uxuyadvectionvelocity boundary points function
 double precision, allocatable     :: xy2bnduxy(:,:)    !< uxuyadvectionvelocity boundary 'external tolerance point'
 integer         , allocatable     :: kduxy  (:)        !< uxuyadvectionvelocity boundary points temp array
 integer         , allocatable     :: kbnduxy(:,:)      !< uxuyadvectionvelocity boundary points index array, see lines above
 double precision                  :: zbnduxyval = -999d0

 integer                           :: nbndn             !< norm.velocity boundary points dimension
 double precision, allocatable     :: xbndn(:)          !< norm.velocity boundary points xcor
 double precision, allocatable     :: ybndn(:)          !< norm.velocity boundary points ycor
 double precision, allocatable, target :: zminmaxu(:)   !< norm.velocity boundary points zmin and zmax
 double precision, allocatable, target :: sigmabndu(:)  !< norm.velocity boundary points sigma coordinates (for now: dim = (nbndn*kmx) )
 double precision, allocatable, target :: zbndn(:)      !< norm.velocity boundary points function
 double precision, allocatable     :: xy2bndn(:,:)      !< norm.velocity boundary 'external tolerance point'
 integer         , allocatable     :: kdn  (:)          !< norm.velocity boundary points temp array
 integer         , allocatable     :: kbndn(:,:)        !< norm.velocity boundary points index array, see lines above

 integer                           :: ndxbnd_own        !< boundary waterlevel points (without ghost points) dimension
 integer         , allocatable     :: ibnd_own(:)       !< Index mapping own boundary points (without ghost points) to the index in all boundary points

 integer                           :: ngate             !< gates links dimension, to specify gate lower edge level
 double precision, allocatable     :: xgate(:)          !< gates links xcor = xz(k1)
 double precision, allocatable     :: ygate(:)          !< gates links ycor
 double precision, allocatable, target :: zgate(:)      !< gates lower_edge_level value
 double precision, allocatable     :: xy2gate(:,:)      !< gates links second point xcor = xz(k2)
 integer         , allocatable     :: kgate(:,:)        !< gates links index array, see lines above
 integer         , allocatable, target :: kdg (:)       !< helper for multiple_uni
 integer         , allocatable     :: L1gatesg(:)       !< first  ngate point in gate signal ngatesg
 integer         , allocatable     :: L2gatesg(:)       !< second ngate point in gate signal ngatesg
 integer                           :: ngatesg           !< nr of gate signals specified
 character(len=128), allocatable, target :: gate_ids(:)

 integer                           :: ncdam             !< nr of controllable dam points
 double precision, allocatable     :: xcdam(:)          !< dam nodes xcor = xz(k1)
 double precision, allocatable     :: ycdam(:)          !< dam nodes ycor
 double precision, allocatable, target :: zcdam(:)      !< dam nodes zvalue {"shape": ["ncdam"]}
 double precision, allocatable     :: xy2cdam(:,:)      !< cdams links second point xcor = xz(k2)
 integer         , allocatable     :: kcdam(:,:)        !< cdams links index array, see lines above
 integer         , allocatable, target :: kdd(:)        !< helper for multiple_uni_damlevel
 integer         , allocatable     :: L1cdamsg(:)       !< first  ncdam point in cdam signal ncdamsg
 integer         , allocatable     :: L2cdamsg(:)       !< second ncdam point in cdam signal ncdamsg
 integer                           :: ncdamsg           !< nr of cdam signals specified
 character(len=128), allocatable, target :: cdam_ids(:)

 integer         , allocatable     :: kdryarea(:)       !< dry area net links index array
 integer                           :: nDryLinks         !< number of net linls of dry are

 type pillar_type
    integer                                     :: np    !< number of pillars
    double precision, dimension(:), allocatable :: xcor  !< x-coordinates of pillars
    double precision, dimension(:), allocatable :: ycor  !< y-coordinates of pillars
    double precision, dimension(:), allocatable :: dia   !< radius od pillars
    double precision, dimension(:), allocatable :: cd    !< Cd coefficient of pillars
 end type pillar_type
 type(pillar_Type), dimension(:), allocatable :: pillar
 double precision,  dimension(:), allocatable :: Cpil


 integer                           :: ncgen             !< nr of controllable generalstr points
 double precision, allocatable     :: xcgen(:)          !< generalstr nodes xcor = xz(k1)
 double precision, allocatable     :: ycgen(:)          !< generalstr nodes ycor
 double precision, allocatable, target :: zcgen(:)      !< generalstr nodes zvalue (kx=3)
 double precision, allocatable     :: xy2cgen(:,:)      !< cgen links second point xcor = xz(k2)

 double precision, allocatable     :: Fusav(:,:)          !< only needed if gatedoorheight > 0 , dim = ncgen
 double precision, allocatable     :: Rusav(:,:)          !< only needed if gatedoorheight > 0
 double precision, allocatable     :: Ausav(:,:)          !< only needed if gatedoorheight > 0

 integer         , allocatable     :: kcgen(:,:)        !< cgen links index array, see lines above
                                                        !! 1,* = index in s1 point "left" of genstru
                                                        !! 2,* = index in s1 point "right" of genstru
                                                        !! 3,* = index in u1 of their connecting link (may point from #2 -> #1 if flow link is in opposite direction through the genstru polyline)
                                                        !! 4,* = pointer to general structure signal nr n
 integer         , allocatable, target :: kdgen(:)        !< helper for multiple_uni_damlevel
 integer         , allocatable     :: L1cgensg(:)       !< first  ncdam point in cdam signal ncdamsg
 integer         , allocatable     :: L2cgensg(:)       !< second ncdam point in cdam signal ncdamsg
 integer                           :: ncgensg           !< nr of cdam signals specified

 integer, parameter                      :: ICGENTP_WEIR    = 1 !< general structure type: a weir
 integer, parameter                      :: ICGENTP_GATE    = 2 !< general structure type: a gate
 integer, parameter                      :: ICGENTP_GENSTRU = 3 !< general structure type: a true general structure
 character(len=128), allocatable, target :: cgen_ids(:)
 integer, allocatable                    :: cgen_type(:) !< (1:ngensg) The type for each general structure, one of ICGENTP_WEIR|GATE|GENSTRU
 integer, allocatable                    :: cgen2str(:)  !< (1:ngensg) Mapping from overall ngensg index to underlying structure index in either 1:nweirgen, 1:ngategen, or 1:ngenstru (inverse from *2cgen arrays below)

 ! The user may specify different 'gate'/'weir'/'generalstructure',
 ! and all are translated into a general structure (in computations
 ! and external forcings). To distinguish them, maintain counters for each.
 ! This should hold: ncgensg = nweirgen + ngategen + ngenstru
 integer                           :: nweirgen          !< nr of weirs in the generalstructure set
 integer                           :: ngategen          !< nr of gates in the generalstructure set
 integer                           :: ngenstru          !< nr of real general structures in the generalstructure set
 integer         , allocatable, target :: weir2cgen(:)      !< (1:nweirgen) Mapping from weir number to underlying generalstructure number
 integer         , allocatable, target :: gate2cgen(:)      !< (1:ngategen) Mapping from gate number to underlying generalstructure number
 integer         , allocatable, target :: genstru2cgen(:)   !< (1:ngenstru) Mapping from true general structure number to underlying generalstructure number

 ! Pumps and pumps with levels
 integer                                 :: npump                    !< nr of pump links
 double precision, allocatable           :: xpump(:)                 !< pump nodes xcor = xz(k1)
 double precision, allocatable           :: ypump(:)                 !< pump nodes ycor
 double precision, allocatable, target   :: qpump(:)                 !< pump discharge m3/s
 double precision, allocatable           :: xy2pump(:,:)             !< pump links second point xcor = xz(k2)
 integer         , allocatable           :: kpump(:,:)               !< pump links index array, see lines above
 integer         , allocatable, target   :: kdp(:)                   !< helper for multiple_uni_pump
 integer         , allocatable           :: L1pumpsg(:)              !< first  npump point in pump signal npumpsg
 integer         , allocatable           :: L2pumpsg(:)              !< second npump point in pump signal npumpsg
 double precision, allocatable     :: pumponoff(:,:)    !< 1=suct on, 2=suct off, 3=deliv on, 4=deliv off , *)
 integer                                 :: npumpsg                  !< nr of pump signals specified
 integer         , allocatable           :: L1strucsg(:)              !< first  nstru point in pump signal
 integer         , allocatable           :: L2strucsg(:)              !< second nstru point in pump signal
 !variables for pump with levels
 ! time varying
 double precision, allocatable           :: waterLevelsPumpLeft(:)   !< left considering flow direction
 double precision, allocatable           :: waterLevelsPumpRight(:)  !< right considering flow direction
 double precision, allocatable           :: pumpAveraging(:,:)       !< to avoid allocations/deallocations
 ! constant in time
 integer                                 :: nPumpsWithLevels         !< nr of pump signals with levels (sobek format)
 integer, allocatable                    :: pumpsWithLevels(:)       !< -1 = legacy, not 1 = new pump
 character(len=128), allocatable, target :: pump_ids(:)              !< the pumps ids

 ! Dambreak
 !time varying
 double precision, allocatable, target   :: waterLevelsDambreakUpStream(:)        !< the water levels computed each time step upstream
 double precision, allocatable, target   :: waterLevelsDambreakDownStream(:)      !< the water levels computed each time step downstream
 double precision, allocatable, target   :: breachDepthDambreak(:)                !< the dambreak breach width (as a level)
 double precision, allocatable, target   :: breachWidthDambreak(:)                !< the dambreak breach width (as a level)
 double precision, allocatable           :: normalVelocityDambreak(:)             !< dambreak normal velocity
 double precision, allocatable           :: dambreakAveraging(:,:)                !< to avoid allocations/deallocations
 double precision, allocatable           :: breachWidthDerivativeDambreak(:)      !< breach width derivatives
 double precision, allocatable           :: waterLevelJumpDambreak(:)             !< water level jumps
 !constant in time
 double precision, allocatable           :: maximumDambreakWidths(:)              !< the total dambreak width (from pli file)
 double precision, allocatable           :: dambreakLinksEffectiveLength(:)       !< dambreak links index array
 integer        , allocatable            :: dambreaks(:)                          !< store the dambreaks indexes among all structures
 integer                                 :: ndambreak                             !< nr of dambreak links
 integer                                 :: ndambreaksg                           !< nr of dambreak signals
 integer         , allocatable           :: L1dambreaksg(:)                       !< first dambreak link for each signal
 integer         , allocatable           :: L2dambreaksg(:)                       !< second dambreak link for each signal
 integer         , allocatable           :: activeDambreakLinks(:)                !< activeDambreakLinks, open dambreak links
 integer         , allocatable           :: LStartBreach(:)                       !< the starting link, the closest to the breach point
 integer         , allocatable           :: kdambreak(:,:)                        !< dambreak links index array
 double precision, allocatable, target   :: dambreakLevelsAndWidthsFromTable(:)   !< dambreak widths and heights
 character(len=128), allocatable, target :: dambreak_ids(:)                       !< the dambreak ids
 ! Upstream water level
 integer                                 :: nDambreakLocationsUpstream                 !< nr of dambreak signals with locations upstream
 integer         , allocatable           :: dambreakLocationsUpstreamMapping(:)        !< mapping of dambreak locations upstream
 integer         , allocatable           :: dambreakLocationsUpstream(:)               !< store cell ids for water level locations upstream
 integer                                 :: nDambreakAveragingUpstream                 !< nr of dambreak signals upstream with averaging
 integer         , allocatable           :: dambreakAverigingUpstreamMapping(:)        !< mapping of dambreak averaging upstream
 ! Downstream water level
 integer                                 :: nDambreakLocationsDownstream               !< nr of dambreak signals with locations downstream
 integer         , allocatable           :: dambreakLocationsDownstreamMapping(:)      !< mapping of dambreak locations downstream
 integer         , allocatable           :: dambreakLocationsDownstream(:)             !< store cell ids for water level locations downstream
 integer                                 :: nDambreakAveragingDownstream               !< nr of dambreak signals downstream with averaging
 integer         , allocatable           :: dambreakAverigingDownstreamMapping(:)      !< mapping of dambreak averaging in the dambreak arrays


 type polygon
   double precision, dimension(:), allocatable :: xp, yp
   integer :: np
 end type polygon
 type(polygon), dimension(:), allocatable :: dambreakPolygons

 integer                           :: nklep              !< nr of kleps
 integer         , allocatable     :: Lklep(:)           !< klep links index array, pos=allow 1->2, neg= allow 2->1

 integer                           :: nvalv              !< nr of valvs
 integer         , allocatable     :: Lvalv(:)           !< valv links index array, pos=allow 1->2, neg= allow 2->1
 double precision, allocatable     :: valv(:)            !< open fraction of Au

 integer                           :: nbndqh             !< q-h boundary points dimension
 double precision, allocatable     :: xbndqh(:)          !< q-h boundary points xcor
 double precision, allocatable     :: ybndqh(:)          !< q-h boundary points ycor
 double precision, allocatable     :: zbndqh(:)          !< q-h boundary points function
 integer         , allocatable     :: kdqh  (:)          !< q-h boundary points temp array
 integer         , allocatable     :: kbndqh(:,:)        !< q-h boundary points index array
                                                        !! 1,* = index in s1 boundary point
                                                        !! 2,* = index in s1 first point on the inside
                                                        !! 3,* = index in u1 of their connecting link (always positive to the inside)
                                                        !! 4,* = type indicator :
                                                        !!                        1 = waterlevel boundary
                                                        !!                        2 = waterlevel neumann
                                                        !!                        3 = velocity   normal ingoing component
                                                        !!                        4 = velocity   flux boundary
                                                        !!                        5 = velocity   Riemann boundary
                                                        !!                        6 = waterlevel outflow
                                                        !!                        7 = q-h boundary
 integer                           :: nqhbnd            !< number of qh boundaries
 character(len=255), allocatable   :: qhpliname(:)      !< name of the location extracted from the pli-file
 integer         , allocatable     :: L1qhbnd(:)        !< first  nbndz point in discharge bnd nqbnd
 integer         , allocatable     :: L2qhbnd(:)        !< second nbndz point in discharge bnd nqbnd
 double precision, allocatable, target :: qhbndz(:)     !< temporary array for storing boundary values per qh boundary segment
 double precision, allocatable, target :: atqh_all(:)       !< temporary array for computing discharge through qh boundary per domain
 double precision, allocatable     :: atqh_sum(:)       !< temporary array for computing total discharge through qh boundary
 double precision                  :: qhrelax = 1d-2    !< relaxation factor for h signal

 integer                                       :: nwbnd    !< number of wave-energy boundaries
 character(len=255), dimension(:), allocatable :: fnamwbnd !< polyline filenames associated with wave-energy boundary

 integer                           :: numsrc            !< nr of point sources/sinks
 integer                           :: numvalssrc        !< nr of point constituents
 integer                           :: msrc = 0          !< maximal number of points that polylines contains for all sources/sinks
 integer, allocatable              :: ksrc(:,:)         !< index array, 1=nodenr sink, 2 =kbsin , 3=ktsin, 4 = nodenr source, 5 =kbsor , 6=ktsor
 double precision, allocatable     :: qsrc(:)           !< cell influx (m3/s) if negative: outflux
 double precision, allocatable     :: sasrc(:)          !< q*salinity    (ppt) (m3/s)  if ksrc 3,4 == 0, else delta salinity
 double precision, allocatable     :: tmsrc(:)          !< q*temperature (degC) (m3/s) if ksrc 3,4 == 0, else delta temperature
 double precision, allocatable     :: ccsrc(:,:)        !< dimension (numvalssrc,numsrc), keeps sasrc, tmsrc etc
 double precision, allocatable     :: qcsrc(:,:)        !< q*constituent (c) (m3/s)  )
 double precision, allocatable     :: vcsrc(:,:)        !< v*constituent (c) (m3)    )
 double precision, allocatable     :: arsrc(:)          !< pipe cross sectional area (m2). If 0, no net momentum
 double precision, allocatable     :: cssrc(:,:)        !< (1:2,numsrc) cosine discharge dir pipe on start side (1) and end side (2) of pipe.
 double precision, allocatable     :: snsrc(:,:)        !< (1:2,numsrc) sine discharge dir pipe on start side (1) and end side (2) of pipe.
 double precision, allocatable     :: zsrc (:,:)        !< vertical level (m) bot
 double precision, allocatable     :: zsrc2(:,:)        !< vertical level (m) top (optional)
 double precision, allocatable     :: srsn (:,:)        !< 2*(1+numvalssrc),numsrc, to be reduced
 integer, allocatable              :: jamess(:)         !< issue message mess for from or to point, 0, 1, 2
 integer, allocatable, target      :: kdss (:)          !< helper for multiple_uni_discharge_salinity_temperature
 double precision, allocatable, target :: qstss(:)      !< array to catch multiple_uni_discharge_salinity_temperature
 character(len=255), allocatable   :: srcname(:)        !< sources/sinks name (numsrc)
 double precision, allocatable     :: vsrccum(:)        !< cumulative volume at each source/sink from Tstart to now
 double precision, allocatable     :: vsrccum_pre(:)    !< cumulative volume at each source/sink from Tstart to the previous His-output time
 double precision, allocatable     :: qsrcavg(:)        !< average discharge in the past his-interval at each source/sink
 double precision, allocatable     :: xsrc(:,:)         !< x-coordinates of source/sink
 double precision, allocatable     :: ysrc(:,:)         !< y-coordinates of source/sink
 integer, allocatable              :: nxsrc(:)          !< mx nr of points in xsrc, ysrc
 integer, allocatable              :: ksrcwaq(:)        !< index array, starting point in qsrcwaq
 double precision, allocatable     :: qsrcwaq (:)       !< Cumulative qsrc within current waq-timestep
 double precision, allocatable     :: qsrcwaq0 (:)      !< Cumulative qsrc at the beginning of the time step before possible reduction
 double precision                  :: addksources = 0d0 !< Add k of sources to turkin 1/0

 contains
!> Sets ALL (scalar) variables in this module to their default values.
!! For external forcings it is equivalent with default_flowexternalforcings().
subroutine reset_flowexternalforcings()
    call default_flowexternalforcings()
end subroutine reset_flowexternalforcings


!> Resets external forcing variables intended for a restart of flow simulation.
!! For external forcings it is equivalent with reset_flowexternalforcings().
subroutine default_flowexternalforcings()
    jatimespace = 0   ! doen ja/nee 1/0
    mhis   = 0        ! unit nr external forcings history *.exthis
    numbnp = 0        ! total nr of open boundary cells for network extension
    nopenbndsect = 0  ! Nr. of open boundary sections.
    nbndz  = 0        ! waterlevel boundary points dimension
    ndxbnd_own = 0    ! boundary points(without ghost boundary points) dimension
    nbndu  = 0        ! velocity   boundary points dimension
    nbndqh = 0        ! q-h boundary points dimension
    nbnds  = 0        ! salinity   boundary points dimension
    nbndtm = 0        ! temperature boundary points dimension
    nbndsd = 0        ! sediment   boundary points dimension
    nbndw  = 0        ! JRE: wave boundary points dimension
    nbndt  = 0        ! tang.velocity boundary points dimension
    nbnduxy = 0       ! uxuy adv vel bnd
    nbndn  = 0        ! norm.velocity boundary points dimension

    ngate   = 0       ! gates links dimension, to specify gate lower edge level
    ngatesg = 0       ! nr of gate control signals
    ncdam   = 0       ! controllable dams nodes dimension, to specify local bottom level
    ncdamsg = 0       ! nr of controllable dam signals
    ncgen   = 0       ! general structure nodes dimension, to apply gen struc defs
    ncgensg = 0       ! nr of general structure signals
    ncgen   = 0       ! general structure nodes dimension, to apply gen struc defs
    ncgensg = 0       ! nr of general structure signals
    nweirgen = 0      ! nr of weirs in the generalstructure set
    ngategen = 0      ! nr of gates in the generalstructure set
    ngenstru = 0      ! nr of real general structures in the generalstructure set
    npump   = 0       ! npump dimension
    npumpsg = 0       ! nr of pump signals
    ndambreak = 0     ! nr of dambreak links
    ndambreaksg = 0   ! nr of dambreak signals
    nklep   = 0       ! nr of kleps
    nvalv   = 0       ! nr of valves
    nqbnd   = 0       ! nr of q bnd's
    ! JRE
    nzbnd = 0
    nubnd = 0
    numsrc  = 0
end subroutine default_flowexternalforcings

end module m_flowexternalforcings

module unstruc_channel_flow
use m_network
implicit none
type(t_network)              :: network
integer                      :: CSCalculationOption  !< calculation option for total area computation in 1d
contains


!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_*() instead.
subroutine default_channel_flow()
    CSCalculationOption = CS_TYPE_PREISMAN     !< calculation option for total area computation in 1d
!call dealloc(network)
end subroutine default_channel_flow

end module unstruc_channel_flow

 module m_turbulence
 implicit none

                                    ! Coefficients of k-e model:
 double precision                  :: cmukep
 double precision                  :: sqcmukep
 double precision                  :: sqcmukepi
 double precision                  :: cewall
 double precision                  :: c1e
 double precision                  :: c2e
 double precision                  :: sigdif
 double precision                  :: sigtke, sigtkei
 double precision                  :: sigeps, sigepsi
 double precision                  :: sigsal, sigsali
 double precision                  :: sigtem, sigtemi
 double precision                  :: sigsed, sigsedi
 double precision                  :: sigrho

 !                                    c1e    = c2e-vonkar**2/(sigeps*sqrt(cmukep))

 double precision                  :: c1t
 double precision                  :: c2t
 double precision                  :: c3tsta
 double precision                  :: c3tuns


 double precision                  :: coefn2
 double precision                  :: skmy
 double precision                  :: a1ph
 double precision                  :: a2
 double precision                  :: b1
 double precision                  :: b2
 double precision                  :: c1
 double precision                  :: e1
 double precision                  :: e2
 !double precision :: e2     = e1-1+vonkar**2*b1**0.6667/skmy
 double precision                  :: ghmin
 double precision                  :: ghmax

 !quasi-equilibrium coefficients:
 !double precision :: csb1   = a2*(1-6*a1ph/b1)
 !double precision :: csb2   = 3*a2*(6*a1ph+b2)
 !double precision :: csu1   = a1*(1-3*c1-6*a1ph/b1)
 !double precision :: csu2   = 3*a1ph*a2*((b2-3*a2)*(1-6*a1ph/b1)-3*c1*(6*a1ph+b2))
 !double precision :: csu3   = 3*a2*(6*a1ph+b2)
 !double precision :: csu4   = 9*a1*a2

 integer, parameter                :: kmxx = 2000     ! max dim of nr of vertical layers
 integer, parameter                :: mg   = 4        ! max dim of nr of sediment fractions

 double precision                  :: dijdij (0:kmxx) ! dudz(k)**2+dvdz(k)**2 vertical shear squared
 double precision                  :: buoflu (  kmxx)
 double precision                  :: bruva  (  kmxx)
 double precision                  :: tkepro (0:kmxx)

 double precision                  :: ak     (0:kmxx)                       ! local arrays, (0:
 double precision                  :: bk     (0:kmxx)
 double precision                  :: ck     (0:kmxx)
 double precision                  :: dk     (0:kmxx)
 double precision                  :: ek     (0:kmxx)
 double precision                  :: dz     (0:kmxx)
 double precision                  :: dke    (0:kmxx)

 double precision                  :: ucxref   (  kmxx)                     ! and for reference/plotting:
 double precision                  :: ucm      (  kmxx)                     ! and for reference/plotting:
 double precision                  :: dijdijref(0:kmxx)
 double precision                  ::  tkin1ref(0:kmxx)
 double precision                  ::  teps1ref(0:kmxx)
 double precision                  ::   vicwref(0:kmxx)
 double precision                  ::     hcref(  kmxx)                     ! mid-layer heigths
 double precision                  ::     hwref(0:kmxx)                     ! layer interface height, 0=bed


 double precision                  :: epstke = 1d-32    ! D3D: - 7, dpm: -32
 double precision                  :: epseps = 1d-32    ! D3D: - 7, dpm: -32
 double precision                  :: epsd = 1d-32      ! D3D: - 7, dpm: -32

 double precision, allocatable     :: turkin0  (:)      ! k old (m2/s2)  , at layer interface at u     these will become global, rename to : turkinwu0
 double precision, allocatable, target :: turkin1(:)    !< [m2/s2] turbulent kinectic energy at layer interface u {"location": "edge", "shape": ["lnkx"]}

 double precision, allocatable     :: tureps0  (:)      ! eps old (1/s)  , at layer interface at u
 double precision, allocatable     :: tureps1  (:)      ! eps new        , at layer interface at u

 double precision, allocatable     :: vicwwu   (:)      ! vertical eddy viscosity (m2/s) at layer interface at u point
 double precision, allocatable, target :: vicwws   (:)  !< [m2/s] vertical eddy viscosity at layer interface at s point {"location": "face", "shape": ["ndkx"]}

 !real            , allocatable     :: tkepro   (:)      ! vertical production t
 !real            , allocatable     :: tkedis   (:)      ! vertical dissipation
 double precision, allocatable     :: rho      (:)      ! density at cell centres (kg/m3)
 double precision, allocatable     :: rho0     (:)      ! density at cell centres (kg/m3), previous step
 double precision, allocatable     :: dpbdx0   (:)      ! previous step baroclinic pressure gradient, at u points

 double precision, allocatable     :: rhou     (:)      ! density at flow links   (kg/m3)

 double precision, allocatable     :: sigdifi  (:)      ! inverse prandtl schmidt nrs
 double precision, allocatable     :: wsf      (:)      ! fall velocities of all numconst constituents

 double precision, allocatable     :: turkinepsws (:,:) ! k and eps,1,2     at layer interface at c , horizontal transport of k and eps
 double precision, allocatable     :: tqcu(:)           ! sum of q*turkinws at layer interface at cupw , horizontal transport of k and eps
 double precision, allocatable     :: eqcu(:)           ! sum of q*turepsws at layer interface at cupw , horizontal transport of k and eps
 double precision, allocatable     :: sqcu(:)           ! sum of q          at layer interface at cupw , horizontal transport of k and eps


 double precision, allocatable     :: tttu(:), ttqc(:), tttc(:) ! test12

 integer         , allocatable     :: ln0(:,:)          ! links in transport trimmed to minimum of ktop,ktop0 for z-layers

contains
!> Sets ALL (scalar) variables in this module to their default values.
subroutine default_turbulence()
use m_physcoef


! Coefficients of k-e model:
    sigdif    = 1d0
    sigtke    = 1.0d0  ; sigtkei = 1d0/sigtke
    sigeps    = 1.3d0  ; sigepsi = 1d0/sigeps
    sigrho    = 0.7d0  ! bouyancy
    sigsal    = 0.7d0  ; sigsali = 1d0/sigsal
    sigtem    = 0.7d0  ; sigtemi = 1d0/sigtem
    sigsed    = 1.0d0  ; sigsedi = 1d0/sigsed

    cmukep    = 0.09d0
    sqcmukep  = sqrt(cmukep)
    sqcmukepi = 1.d0 / sqcmukep

    cewall    = cmukep**0.75d0/vonkar   ! 0.4769d0  !        0.09**0.75/0.41  ! /vonkar
    c2e       = 1.92d0
    c1e       = 1.44d0
    c1e       = c2e-vonkar**2/(sigeps*sqcmukep)

    c1t       = (1d0-c1e) * cmukep
    c2t       = 1d0-c2e
    c3tsta    = 1d0 * cmukep
    c3tuns    = (1d0-c1e) * cmukep

    coefn2    = - ag / (sigrho*rhomean)

    skmy   = 1.96d0
    a1ph   = 0.92d0
    a2     = 0.74d0
    b1     = 16.6d0
    b2     = 10.1d0
    c1     = 0.08d0
    e1     = 1.80d0
    e2     = 1.33d0
!e2     = e1-1+vonkar**2*b1**0.6667/skmy
    ghmin  =-0.280d0
    ghmax  = 0.0233d0
end subroutine default_turbulence

 end module m_turbulence





 module m_flowparameters
 use m_sediment, only: jased
 ! use m_d3ddimens
 implicit none

 integer                           :: jatransportmodule = 1    !< use transport module (1) or subroutine (0), or no transport (2)
 integer                           :: itstep            !< time step 0=no, 1 =step_explicit, 2=step_reduce, 3=step_jacobi, 4: explicit
 integer                           :: iadvec            !< adv type, 0=no, 1 = Wenneker vol, qu-udzt array, 2=1, function,
                                                        !< 3 =Perot in uit, 4 =Perot in, explicit
                                                        !< 5 =Perot in uit, 6 =Perot in, piaczek teta
                                                        !< 7 =Perot in uit, 8 =Perot in, piaczek fully implicit
                                                        !< 9 =Perot in uit, 10=Perot in, piaczek fully implicit, weir + factor
                                                        !< 11=Perot in uit, 12=Perot in, piaczek fully implicit, gate + factor
                                                        !< 20=Energy conserving compact, piaczek fully implicit, weir
 integer                           :: maxNonlinearIterations!< maximal iterations in non linear iteration loop before a time step reduction is applied
 logical                           :: setHorizontalBobsFor1d2d !< bobs are set to 2d bedlevel, to prevent incorrect storage in sewer system.
 integer                           :: iadvec1D          !< same, now for 1D links

 integer                           :: lincontin         !< 0 = no, 1 = yes linear continuity

 integer                           :: iPerot            !< Perot weigthing type of cell center velocities ucx, ucy
                                                        !! in vectoren:
                                                        !! 0 : uc*sum(w) = sum (u W)
                                                        !! 1 : uc*A      = sum(u dxa W)
                                                        !! 2 : uc*A*hs   = sum(u dxa W hu ), ie waterdepth dependent
                                                        !! 2 : uc*V      = sum(q dxa      ), ie waterdepth dependent
                                                        !! 3 : uc*A*humx = sum(u dxa W hu ), humx = max(hu)
                                                        !! 4 : uc*A*humx = sum(u dxa W hu ), humx = max(hu)

 integer                           :: jacomp = 1        !! same now for netnodes, 0 = default, 1 = use cs, sn in weighting, 2=regular scalar x,y interpolation based on banf

 integer                           :: icorio            !< Coriolis weigthing
 
 integer                           :: newcorio = 0      !< 0=up to 27-11-2019 , 1 = after 
 
 integer                           :: jacorioconstant=0 !< Coriolis constant in sferic models anyway if set to 1
 
 double precision                  :: Corioadamsbashfordfac = 0d0  !< Coriolis Adams Bashford , 0d0 = explicit, 0.5 = AB 

 double precision                  :: hhtrshcor         !< if > 0 safety for hu/hs in corio for now, ==0

 double precision                  :: trshcorio         !< below this depth coriolis force scaled down linearly to 0

 integer                           :: jatidep           !< use tide potential forcing yes no

 integer                           :: jaselfal          !< use self attraction and loading yes no
 integer                           :: jaSELFALcorrectWLwithIni   !< correct water level with initial water level in SAL

 double precision                  :: doodsonstart, doodsonstop , doodsoneps

 integer                           :: jatrt             !< Trtrou = #Y# --> 1 , Trtrou = #N# --> 0  (Delf3D style input)

 integer                           :: jacali            !< use calibration factors (0 = no, 1 = yes)

 integer                           :: jasal             !< Include salinity set in mdf

 integer                           :: jatem             !< Temperature model (0=no, 5=heatfluxmodel)

 integer                           :: janudge           !< temperature and salinity nudging
 integer                           :: jainiwithnudge   !< initialize salinity and temperature with nudge variables

 integer                           :: itempforcingtyp   !< Forcing parameter types 1,2 humidity, 3,4 dewpoint see code

 integer                           :: jarhoxu           !< rho effects in momentum, 0=no, 1=in horizontal adv, 2=+ in vertical adv, 3 = + in pressure term

 integer                           :: jawave            !< Include wave model nr, 0=no, 1=fetchlimited hurdle stive + swart, 4: XBeach wave driver, 3:SWan, 5=Const

 integer                           :: jawavestreaming   !< Switch on in D3D model: >=1 : streaming mom , >= 2 : streaming mom + turb

 integer                           :: jawaveStokes      !< Vertical Stokes profile: 0=no, 1 = uniform, 2 = second order Stokes profile

 integer                           :: jawaveRoller      !< Roller contribution: 0=no, 1 = Rol1, 2 = Rol2

 integer                           :: jawaveSwartDelwaq !< communicate to Delwaq taucur + tauwaveswart instead of taucur, specify z0wav

 integer                           :: modind = 1        !< Nr of wave-current bed friction model, 9 = vanrijn, 1 = fredsoe, etc like d3d

 integer                           :: jaavgwavquant = 0 !< 0=no time avg'd output for jawave==4; 1=time avg'd output for jawave==4. Avg'ing period = ti_avgwav

 integer                           :: ihorvic           !< 0=no visc, 1=do visc

 integer                           :: jacreep           !< Include anti-creep calculation, (0=no, 1=yes)

 integer                           :: jainirho          !< Initialise rho at start at flowinit (otherwise first step without barocl)

 integer                           :: jasecflow         !< 0: no, 1: yes

 integer                           :: japillar          !< 0: no, 1: yes

 integer                           :: jaequili          !< secondary flow intensity gets calculated as equilibrium (0=no, 1=yes)

 integer                           :: javiusp           !< if 1 spatially varying horizontal eddy viscosity

 integer                           :: jadiusp           !< if 1 spatially varying horizontal eddy viscosity

 integer                           :: jaCdwusp          !< if 1 spatially varying windstress coefficient

 integer                           :: jaWindspeedfac    !< if 1 spatially varying windstress coefficient
 
 integer                           :: javiuplus3D = 1   !< add vertical eddy viscosity to horizontal eddy viscosity (1 = yes, 0 = no)

 integer                           :: jafrculin         !< use linear friction yes/no

 integer                           :: jaFrcInternalTides2D  !< use internal tides friction (1) or not (0)

 integer                           :: iuvfield          !< intialise this velocityfield: 0 = no
                                                        !! 1:u=y**2, 2:idem, 60 deg, 3:rotation, 4=lin, 5=lin 60 deg

 integer                           :: istresstyp        !< 1 : full stress tensor, semi  link oriented horvic2
                                                        !! 2 : full stress tensor, fully link oriented dvxc = ok and fast
                                                        !! 3 : 2, volume weighted
                                                        !! 4 : full node oriented
                                                        !! 5 : 4, volume weighted

 integer                           :: irov              !< 0 : free slip
                                                        !! 1 : partial slip
                                                        !! 2 : no slip
                                                        !! 3 : glass  (mind you, in D3DFLOW 3 means free slip)


 integer                           :: ibedlevmode       !< 1 : See BLMODE_DFM
                                                        !! 2 : See BLMODE_D3D
 integer, parameter                :: BLMODE_DFM = 1    !< ibedlevmode value: Compute bed levels solely by ibedlevtyp, i.e., derived from velocity points (or direct bl tiles)
 integer, parameter                :: BLMODE_D3D = 2    !< ibedlevmode value: Compute bed levels as D3D, i.e., derived from corner point depths. Currently always deepest (== DPSOPT=MAX).

 integer                           :: ibedlevtyp        !< 1 : Bed levels at waterlevel cells (=flow nodes), like tiles xz, yz, bl , bob = max(bl left, bl right)
                                                        !! 2 : Bed levels at velocity points  (=flow links),            xu, yu, blu, bob = blu,    bl = lowest connected link
                                                        !! 3 : Bed levels at velocity points  (=flow links), using mean network levels xk, yk, zk  bl = lowest connected link
                                                        !! 4 : Bed levels at velocity points  (=flow links), using min  network levels xk, yk, zk  bl = lowest connected link

 integer                           :: ibedlevtyp1D      !< 1 : same, 1D, 1 = tiles, xz(flow)=zk(net), bob(1,2) = max(zkr,zkl) , 3=mean netnode based

 integer                           :: izbndpos          !< 0 : waterlevel boundary location as in D3DFLOW, 1=on network boundary, 2=on specified boundary polyline

 double precision                  :: blmeanbelow       !<  : if not -999d0, below this level the cell centre bedlevel is the mean of surrouding netnodes
 double precision                  :: blminabove        !<  : if not -999d0, above this level the cell centre bedlevel is the min of surrouding netnodes
 double precision                  :: blmin             !<  : lowest bedlevel point in model
 double precision                  :: upot0=-999d0      !<  : initial potential energy
 double precision                  :: ukin0=-999d0      !<  : initial kinetic   energy

 integer                           :: jaupdbndbl        !< Update bl at boundary (1 = update, 0 = no update)
 integer                           :: jaupdbobbl1d     !< Update bl and bobs for 1d network (call to setbobs_1d only at initialization)

 integer                           :: nonlin            !< 1 : non-linear continuity , == max(nonlin, nonlin2D) , 2 == pressurized nonlin
 integer                           :: nonlin1D          !< 1 : non-linear continuity eq for 1D points, only for non-rectangles
 integer                           :: nonlin2D          !< 1 : non-linear continuity eq for 2D points, only for skewed bed, i.e. jaconveyance2D >= 1

 integer                           :: iproftypuni       !< 1 : circle, 2 : rectan, 3 = rectan R=H, negative = closed for rain and grw
 integer                           :: iproftypuni5      !< idem, for streetinlets
 integer                           :: iproftypuni7      !< idem for roofgutterpipes
 double precision                  :: slotw2D           !< minimum slotwidth 2D
 double precision                  :: slotw1D           !< minimum slotwidth 1D

 integer                           :: jaconveyance2D    !< 1 : yes, 0 : no
 integer                           :: nums1it           !<   : nr of non-linear continuity iterations
 integer                           :: nums1mit          !<   : nr of non-linear continuity iterations outer loop ic nested
 double precision                  :: dnums1it          !<   : total nr of non-linear continuity iterations
 integer                           :: isimplefixedweirs !< 1=only links stored, 0=complete crossection paths stored

 integer                           :: iNormalMethod     !< 0: take normal in direction of flowlinks "1-2", 1: take normal perpendicular to netlinks "3-4"
 integer                           :: jaimplicit        !< implicit momentum eqn. (1) or not (0)
 integer                           :: jafilter          !< apply horizontal filter (1:explicit, 2:implicit) or not (0)
 integer                           :: filterorder       !< order of filter
 integer                           :: jacheckmonitor    !< compute and output "checkerboard" mode monitor

!    Secondary Flow
! integer                           :: jasftesting       !< (secondary flow testing: 0: no just compute fm velocitie, 1: prescribe exact ucx,ucy point values 2: prescribe exact edge velocities (FM reconstructs ucx,ucy) 3: prescribe exact ucx,ucy cell-averaged values
! integer                           :: jasfinttype       !< 1: perot reconstruction 2: standard gauss
! integer                           :: jasftesttype      !< -1: u_theta = 0.1*200/r; u_r = 0 | k: k=0..3, u_x = x^k, u_y = 0

 double precision                  :: Uniformhu         !< Uniformhu for arjen's membranes
 double precision                  :: bedslope          !< bed inclination testcases
 double precision                  :: bedwaveamplitude=0d0  !< bed testcases
 double precision                  :: bedwavelength=0d0     !< bed testcases

 double precision                  :: Slopedrop2D       !< Apply losses for 'rain from the roof', only if local bottom slope > Slopedrop2D, only for Slopedrop2D  > 0.0
 logical                           :: Slopedrop1D       !< Apply losses for all 1d links,
 double precision                  :: drop3D            !< Apply losses in or 3D if downwind z below bob + 2/3 hu
 double precision                  :: zwsbtol = 0d0     !< zws(kb0) = bl - zwsbtol
 integer                           :: keepzlayeringatbed=1 !< only for z layers zws(kb0) = zslay instead of bl

 double precision                  :: cflmx             !< max Courant nr ()
 double precision                  :: cflw              !< wave velocity fraction, total courant vel = u + cflw*wavevelocity
 double precision                  :: teta0             !< 1.00d0   ! .52      ! uniform teta in horizontal (),
 integer                           :: ivariableteta     !< 0=fully implicit,   1=teta constant,        2=variable teta
                                                        !! (set teta=1.0)      (set teta=0.51->0.99)   (set teta<0)
 integer                           :: japiaczek33 = 1   ! testing 1 2

 integer                           :: jacstbnd           !< Delft-3D type cell-centered velocities at boundaries (ucx, ucy)
 integer                           :: jaLogprofatubndin  !< ubnds inflow: 0=uniform U1, 1 = log U1, 2 = log U1 and k-eps accordingly
 integer                           :: jaLogprofkepsbndin !< ubnds inflow: 0=uniform U1, 1 = log U1, 2 = log U1 and k-eps accordingly
 integer                           :: jamodelspecific = 0 !< override for above two parameters

 integer                           :: limtypsa          !< 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC, 21=central voor stof transport
 integer                           :: limtypTM          !< 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC, 21=central voor stof transport
 integer                           :: limtypsed         !< 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC, 21=central voor stof transport
 integer                           :: limtyphu          !< 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC, 21=central voor hu
 integer                           :: limtypmom         !< 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC, 21=central voor momentum transport
 integer                           :: jalimnor          !< 0=limit x/y components, 1=limit normal/tangetial components
 integer                           :: limtypw           !< 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC, 21=central voor wave action transport

 integer                           :: ifixedweirscheme     !< 0 = no, 1 = compact stencil, 2 = whole tile lifted, full subgrid weir + factor
 integer                           :: ifxedweirfrictscheme   !< 0 = friction based on hu, 1 = friction based on subgrid weirfriction scheme
 double precision                  :: fixedweircontraction !< flow width = flow width*fixedweircontraction
 double precision                  :: fixedweirtopwidth    !< , e.g. 4.00 (m)
 double precision                  :: fixedweirtopfrictcoef     !< if .ne. dmiss, use this friction coefficient on top width
 double precision                  :: fixedweirtalud       !< , e.g. 4 ( ) for 1 to 4 talud
 double precision                  :: waquaweirthetaw=0.6d0 !< , e.g. 0.6


 double precision                  :: sini              !< uniform initial waterlevel (m),     (uniform bottom level = zkuni)
 double precision                  :: waterdepthini1D   !< uniform initial depth (m)
 double precision                  :: uini              !< uniform initial velociy    (m/s),
 double precision                  :: salini            !< uniform initial sal        (ppt)
 double precision                  :: deltasalinity=-999d0    !< uniform initial sal        (ppt)
 double precision                  :: Sal0abovezlev     !< sal=0 above lev= zlev      (m)
 double precision                  :: temini            !< uniform initial temp       (degC)
 double precision                  :: spirini           !< uniform initial spirint    (m/s)

 double precision                  :: zbnd              !< for now only, uniform waterlevel on boundary
 double precision                  :: zkdropstep        !< Amount of bottomlevel to be added with dropland (m)
 double precision                  :: sdropstep         !< Amount of water to be added with dropwater (m)


 double precision                  :: eps4              !< min au in poshchk
 double precision                  :: eps6              !<
 double precision                  :: eps8              !< implicit diffusion
 double precision                  :: eps10             !<
 double precision                  :: epshsdif=1d-2     !< hs < epshsdif: no vertical diffusion if hs < epshsdif
 double precision                  :: s01max            !< water level threshold (m) between s0 and s1 in validation routine
 double precision                  :: u01max            !< velocity threshold (m/s) between u0 and u1 in validation routine
 ! See also m_flowtimes::dtminbreak

 ! parameters controlling flooding/drying/solving
 double precision                  :: epshu             !< minimum waterdepth for setting hu>0
 double precision                  :: epshs             !< minimum waterdepth for setting cfu
 double precision                  :: epswav            !< minimum waterdepth for wave calculations
 double precision                  :: chkhuexpl         !< only for step_explicit:  check computed flux beneath this waterdepth
 double precision                  :: chkadvd           !< check advection  for 'drying' below this (upwind) waterdepth
 double precision                  :: chktempdep        !< check heatfluxes for 'drying' below this waterdepth
 double precision                  :: trsh_u1Lb = 0.0d0
 integer                           :: jposhchk          !< check for positive waterdepth; 0 = no
                                                        !!                                1 = 0.7*dts, just redo
                                                        !!                                2 = 1.0*dts, close all links
                                                        !!                                3 = 0.7*dts, close all links
                                                        !!                                4 = 1.0*dts, reduce au
                                                        !!                                5 = 0.7*dts, reduce au
 integer                           :: jsolpos           !< in iterative solver force solution above bottom level
 integer                           :: Icgsolver         !< 'Solver type , 1 = sobekGS_OMP, 2 = sobekGS_OMPthreadsafe, 3 = sobekGS, 4 = sobekGS + Saadilud, 5 = parallel/global Saad, 6 = parallel/Petsc, 7 = parallel/GS '
 integer                           :: ipre              !< Preconditioner, 0=rowscaling, 1=GS, 2=trial
 integer                           :: jajipjan          !< arrays in gauss substi
 integer                           :: jacheckmatrix     !< checkmatrix

 integer                           :: mdump             ! dump file unit nr

 double precision                  :: hwetbed           !< for case wetbed

 integer                           :: javau             !< vert. adv. u1   : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL

integer                            :: javau3onbnd = 0   !< vert. adv. u1 bnd UpwimpL: 0=follow javau , 1 = on bnd, 2= on and near bnd

 integer                           :: javakeps          !< vert. adv. keps : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL

 integer                           :: javasal           !< vert. adv. sa1  : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL, switched to 3 for neg. strat.

 integer                           :: javatem           !< vert. adv. tem1 : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL, switched to 3 for neg. strat.

 integer                           :: javased           !< vert. adv. suspended sediment concentrations : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL, 5=switched to 3 for neg stratif., 6=higher-order upwind/explicit

 integer                           :: javatest          !< vert. adv. keps : test, 0 = no

 integer                           :: jahazlayer        !< vertical treatment of horizontal advection in z layers 1=org, 2=sigma, 3=node volumes

 integer                           :: jaZlayeratubybob=0  !< 0 = BL left/right based, 1: Linkbob based

 integer                           :: jaZlayercenterbedvel=1 !< In z-layer model copy lowest u-velocity to lower cell centres

 integer                           :: jaZerozbndinflowadvection=0 !< set zero advection velocity on inflow at z boundaries 0=no, 1=yes

 integer                           :: jabaroctimeint    !< time integration baroclini pressure, 1 = Euler, abs() = 2; rho at n+1/2, 3: AdamsB

 integer                           :: jabarocterm       !< 1 or 2 for original or revised term, we only document the revised term, keep org for backw. comp.

 integer                           :: jalts = 1         ! local time-stepping (1) or not (0)

 integer                           :: jatransportautotimestepdiff = 0 ! Auto Timestep in Transport module, 0 = limitation of diffusion, but no limitation of time-step due to diffusion, 1 = no limitation of diffusion, but limitation of time step due to diffusion, 2: no limitation of diffusion and no limitation of time step due to diffusion

 integer                           :: jaexplicitsinks = 1

 integer                           :: jaanalytic        !< analytic solution available in black sideview => do not also show computed surface in black

 integer                           :: jaustarint              !< 1=integral bed layer velocity,  0=velocity at half bed layer

 double precision                  :: Eddyviscositybedfacmax  !< eddyviscosityatbed = min(eddyviscosityatbed, eddyviscosityatbedfacmax*eddyviscosityatbed+1 )

 double precision                  :: Eddyviscositysurfacmax  !< eddyviscosityatbed = min(eddyviscosityatsur, eddyviscosityatseufacmax*eddyviscosityatsur-1 )

 integer                           :: jaqaisq1 = 0           !< 1 : qa = q1, 0 : qa = au*u1

 integer                           :: inisal2D               !< 1 if specified through meteo module

 integer                           :: initem2D               !< 1 if specified through meteo module

 integer                           :: inised2D               !< 1 if specified through meteo module

 integer                           :: inivel                 !< initial velocity (1) or not (0)

 double precision                  :: cffacver = 0d0         !< switch to low order at high cf in constituent transport vertical, 1d0=yes, 0d0 = no

 double precision                  :: toplayminthick         !< minimum top layer thickness (m)

 double precision                  :: botlayminthick         !< minimum bot layer thickness (m)

 double precision                  :: uniformsalinityabovez = -999d0 !< above this level uniform inisaltop (m) dmiss==do not use

 double precision                  :: uniformsalinitybelowz = -999d0 !< below this level uniform inisal    (m) dmiss==do not use

 integer                           :: jbasqbnddownwindhs        !< 0 : original hu on qbnd, 1 = downwind hs on qbnd

 integer                           :: maxitverticalforestersal  !< 100, max iterations vertical forester

 integer                           :: maxitverticalforestertem  !< 100, max iterations vertical forester

 double precision                  :: salmax                    !< filter if sal > maxsal

 integer                           :: jaupwindsrc               !< 1st-order upwind advection (1) or higher-order (0)

 integer                           :: jajre                     !< 0: default, 1: sb

 integer                           :: jasourcesink              !< 1: source+sink 2:source 3:sink for sediment

 ! written to his file yes or no
 integer                           :: jahisbal                  !< Write mass balance/volume totals to his file, 0: no, 1: yes
 integer                           :: jahissourcesink           !< Write discharge/volume at sources/sinks, 0: no, 1: yest
 integer                           :: jahistur                  !< Write k, eps and vicww to his file, 0: no, 1: yes
 integer                           :: jahiswind                 !< Write wind velocities to his file, 0: no, 1: yes
 integer                           :: jahisrain                 !< Write precipitation intensity  (depth per time) to this file, 0: no, 1: yes
 integer                           :: jahistem                  !< Write temperature to his file, 0: no, 1: yes
 integer                           :: jahisheatflux             !< Write heatfluxes to his file, 0: no, 1: yes
 integer                           :: jahissal                  !< Write salinity to his file, 0: no, 1: yes
 integer                           :: jahisrho                  !< Write density  to his file, 0: no, 1: yes
 integer                           :: jahiswatlev               !< Write water level to his file, 0: no, 1: yes
 integer                           :: jahisbedlev               !< Write bed level to his file, 0: no, 1: yes
 integer                           :: jahiswatdep               !< Write waterd epth to his file, 0: no, 1: yes
 integer                           :: jahisvelvec               !< Write velocity vectors to his file, 0: no, 1: yes
 integer                           :: jahisww                   !< Write upward velocity to his file, 0: no, 1: yes
 integer                           :: jahissed                  !< Write sediment transport to his file, 0: no, 1: yes
 integer                           :: jahisconst                !< Write tracers to his file, 0: no, 1: yes
 integer                           :: jahiszcor                 !< Write the vertical coordinate to his file, 0: no, 1: yes
 integer                           :: jahiswav                  !< Write wave data to his file, 0: no, 1: yes

 ! written to map file yes or no
 integer                           :: jamaps0                   !< previous step water levels to map file, 0: no, 1: yes
 integer                           :: jamaps1                   !< water levels to map file, 0: no, 1: yes
 integer                           :: jamapvol1                 !< Volumes to map file, 0: no, 1: yes
 integer                           :: jamaphu                   !< Water depths on u point to map file, 0: no, 1: yes
 integer                           :: jamapanc                  !< Ancillary variables attribute added to map file, 0: no, 1: yes (http://cfconventions.org/cf-conventions/v1.6.0/cf-conventions.html#ancillary-data)
 integer                           :: jamapau                   !< Normal flow areas au to map file, 0: no, 1: yes
 integer                           :: jamapu1                   !< velocities to map file, 0: no, 1: yes
 integer                           :: jamapu0                   !< previous step velocities to map file, 0: no, 1: yes
 integer                           :: jamapucvec                !< velocity vectors to map file, 0: no, 1: yes
 integer                           :: jamapucmag                !< velocity vector magnitude to map file, 0: no, 1: yes
 integer                           :: jamapucqvec               !< velocity vectors (discharge based) to map file, 0: no, 1: yes
 integer                           :: jamapww1                  !< upward velocity on flow link to map file, 0: no, 1: yes
 integer                           :: jamapnumlimdt             !< num limdt to map file, 0: no, 1: yes
 integer                           :: jamaptaucurrent           !< shear stress to map file, 0: no, 1: yes
 integer                           :: jamapchezy                !< chezy to map file, 0: no, 1: yes
 integer                           :: jamapsal                  !< salinity to map file, 0: no, 1: yes
 integer                           :: jamaptem                  !< temperature to map file, 0: no, 1: yes
 integer                           :: jamapcali                 !< roughness calibration factors to map file, 0: no, 1: yes
 integer                           :: jamapconst                !< constituents to map file, 0: no, 1: yes
 integer                           :: jamapsed                  !< sediment fractions to map file, 0: no, 1: yes
 integer                           :: jamaptur                  !< k, eps and vicww to map file, 0: no, 1: yes
 integer                           :: jamaptrachy               !< trachytope roughnesses to map file, 0: no, 1: yes
 integer                           :: jamaprain                 !< wind velocities to map file, 0: no, 1: yes
 integer                           :: jamapwind                 !< wind velocities to map file, 0: no, 1: yes
 integer                           :: jamapwindstress           !< wind stress to map file, 0: no, 1: yes
 integer                           :: jamapviu                  !< horizontal viscosity to map file, 0: no, 1: yes
 integer                           :: jamapdiu                  !< horizontal diffusity to map file, 0: no, 1: yes
 integer                           :: jamaprho                  !< flow density to map file, 0: no, 1: yes
 integer                           :: jamapq1                   !< flow flux to map file, 0: no, 1: yes
 integer                           :: jamapq1main               !< main channel flow flux to map file, 0: no, 1: yes
 integer                           :: jamapspir                 !< spiral flow to map file, 0: no, 1: yes
 integer                           :: jamaptidep                !< tidal potential to map file, 0: no, 1: yes
 integer                           :: jamapselfal               !< self attraction and loading potential to map file, 0: no, 1: yes
 integer                           :: jamapIntTidesDiss         !< internal tides dissipation to map file, 0: no, 1: yes
 integer                           :: jamapNudge                !< output nudging to map file, 0: no, 1: yes
 integer                           :: jamapwav                  !< output waves to map file, 0: no, 1: yes
 integer                           :: jamapdtcell               !< output time steps per cell based on CFL
 integer                           :: jamapTimeWetOnGround      !< output to map file the cumulative time when water is above ground level, 0: no, 1: yes
 integer                           :: jamapFreeboard            !< output freeboard to map file, 0: no, 1: yes
 integer                           :: jamapDepthOnGround        !< output waterdepth above ground level, 0: no, 1: yes
 integer                           :: jamapVolOnGround          !< output volume above ground level, 0: no, 1: yes
 integer                           :: jamapTotalInflow1d2d      !< output total 1d2d inflow to map file, 0: no, 1: yes
 integer                           :: jamapTotalInflowLat       !< output total lateral inflow to map file, 0: no, 1: yes
 integer                           :: jamapS1Gradient           !< output water level gradient to map file, 0: no, 1: yes
 integer                           :: jatekcd                   !< tek output with wind cd coefficients, 0=no (default), 1=yes
 integer                           :: jafullgridoutput          !< 0:compact, 1:full time-varying grid data
 integer                           :: jaeulervel                !< 0:GLM, 1:Euler velocities
 integer                           :: jamombal                  !< records some gradients of primitives 0:no, 1:yes
 integer                           :: jarstbnd                  !< Waterlevel, bedlevel and coordinates of boundaries, 0: no, 1: yes
! Write partition domain file
 integer                           :: japartdomain              !< Write a separate netcdf file for partition domain info., 0: no, 1: yes


! Write shape files
 integer                           :: jashp_crs                 !< Write a shape file for cross sections
 integer                           :: jashp_obs                 !< Write a shape file for observation points
 integer                           :: jashp_weir                !< Write a shape file for weirs
 integer                           :: jashp_thd                 !< Write a shape file for thin dams
 integer                           :: jashp_gate                !< Write a shape file for gates
 integer                           :: jashp_emb                 !< Write a shape file for Embankments
 integer                           :: jashp_fxw                 !< Write a shape file for fixed weirs
 integer                           :: jashp_src                 !< Write a shape file for source-sinks
 integer                           :: jashp_pump                !< Write a shape file for pumps
 integer                           :: jashp_dry                 !< Write a shape file for dry areas
 integer                           :: jashp_genstruc            !< Write a shape file for general structures

 integer                           :: jawriteDFMinterpretedvalues = 0 !< Write interpretedvalues

! parameters for parms solver
 integer,                                   parameter :: NPARMS_INT=2              !< for parms solver, number of integer parameters
 integer,                                   parameter :: IPARMS_ILUTYPE=1
 integer,                                   parameter :: IPARMS_NLEVEL=2
 character(len=128), dimension(NPARMS_INT), parameter :: iparmsnam= [ character(len=128):: 'ilutype', 'nlevel' ]
 integer,            dimension(NPARMS_INT)            :: iparms

 integer,                                   parameter :: NPARMS_DBL=1              !< for parms solver, number of double precision parameters
 integer,                                   parameter :: IPARMS_DTOL=1
 character(len=128), dimension(NPARMS_DBL), parameter :: dparmsnam= [ character(len=128):: 'dtol' ]
 double precision,   dimension(NPARMS_DBL)            :: dparms

! parameters for nudging
  double precision                                     :: Tnudgeuni=3600d0        !< uniform nudge relaxation time

! parameters for internal tides dissipation
 double precision                  :: ITcap              !< limit to Internal Tides Dissipation / area (J/(m^2 s))

! Advection modelling at barriers
  integer                           :: jabarrieradvection = 1

 ! parameter for bed roughness and transport
 integer                                              :: v2dwbl

 ! determines whether md1d file exist
 integer                                              :: jamd1dfile = 0

 ! parameter for secondary flow
 integer                                              :: ispirparopt ! for visualization

 contains
!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_flowparameters() instead.
subroutine default_flowparameters()
    jatransportmodule = 1    ! transportmethod 1 (module) or 0 hk (subroutine) or no transport (2)
    itstep   = 2      ! time step 0=only transport, 1=transport + velocity update, 2=full implicit step_reduce
    iadvec   = 33     ! adv type, 0=no, 1= Wenneker vol, qu-udzt array, 2=1, function, 3=Perot in uit, 4=Perot in, 5=3,piaczek
    iadvec1D = 33     ! same, now for 1D links

    lincontin= 0      ! 0 = no, 1 = yes linear continuity

    iPerot   = 1      ! Perot weigthing type of cell center velocities ucx, ucy
                      ! in vectoren:
                      ! 0 : uc*sum(w) = sum (u W)
                      ! 1 : uc*A      = sum(u dxa W)
                      ! 2 : uc*A*hs   = sum(u dxa W hu ), ie waterdepth dependent
                      ! 2 : uc*V      = sum(q dxa      ), ie waterdepth dependent
                      ! 3 : uc*A*humx = sum(u dxa W hu ), humx = max(hu)
                      ! 4 : uc*A*humx = sum(u dxa W hu ), humx = max(hu)
                      ! 5 : uc*Vc     = sum(u dxa W hu ), Vc = dxa W hu based volume in cell
                      ! 6 : as 5, also for Coriolis

    icorio = 5        ! Coriolis weigthing
                      ! (Tx,Ty) = tangential unit vector at u-point
                      ! uk      = u1 at layer k, 
                      ! hu      = hu(2D)                                     ; huk   = hu(L)   
                      ! hs      = hs(2D)                                     ; hsk   = zws(k) - zws(k-1)                 
                      ! ahu     = alfa(hs) = acL(LL)*hs1 + (1-acL(LL))*hs2   ; ahuk  = alfa(hsk) 
                      ! hus     = areaweighted( hu) at s point               ; husk  = hus(k)  
                      ! ahus    = areaweighted(ahu) at s point               ; ahusk = ahus(k)
                      ! .       = dotp
                      ! avolu   = alfa(vol1)                                 ; avoluk = alfa(vol1(k))
      
                      ! unweighted
                      ! 3 : vk = alfa LR (Tx, Ty) . (ucxk , ucyk)              ucxk, ucyk  =   Perotsum (uk), (== ucx,ucy), f node based, fcori     
                      ! 4 : vk = alfa LR (Tx, Ty) . (ucxk , ucyk)              ucxk, ucyk  =   Perotsum (uk), (== ucx,ucy), f link based, fcor
                      
                      ! Olga type weightings 
                      ! 5 : vk = alfa LR (Tx, Ty) . (ucxqk, ucyqk)             ucxqk,ucyqk = ( Perotsum (uk*huk) )    / hsk Olga
                      ! 6 : vk = alfa LR (Tx, Ty) . (ucxqk, ucyqk)             ucxqk,ucyqk = ( Perotsum (uk*hu)  )    / hs 
                       
                      ! 7 : vk = alfa LR (Tx, Ty) . (ucxqk, ucyqk)             ucxqk,ucyqk = ( Perotsum (uk*ahuk) )   / ahusk
                      ! 8 : vk = alfa LR (Tx, Ty) . (ucxqk, ucyqk)             ucxqk,ucyqk = ( Perotsum (uk*ahu)  )   / ahus 
     
                      ! 9 : vk = alfa LR (Tx, Ty) . (ucxqk, ucyqk)             ucxqk,ucyqk = ( Perotsum (uk*avoluk) ) / volk
                      !10 : vk = alfa LR (Tx, Ty) . (ucxqk, ucyqk)             ucxqk,ucyqk = ( Perotsum (uk*avolu)  ) / vol 
     
                 
                      ! David type weightings 
                      !25 : vk = alfa LR (Tx, Ty) . (ucxk*hsk   , ucyk*hsk )   / huk
                      !26 : vk = alfa LR (Tx, Ty) . (ucxk*hs    , ucyk*hs  )   / hu 

                      !27 : vk = alfa LR (Tx, Ty) . (ucxk*hsk   , ucyk*ahusk ) / ahuk 
                      !28 : vk = alfa LR (Tx, Ty) . (ucxk*hs    , ucyk*ahus  ) / ahu 
  
                      !29 : vk = alfa LR (Tx, Ty) . (ucxk*vol1k , ucyk*vol1k ) / avoluk   identical to advec33 
                      !30 : vk = alfa LR (Tx, Ty) . (ucxk*vol1  , ucyk*vol1  ) / avolu 

    hhtrshcor = 0d0   ! 0=no safety on hu/hs in corio 

    trshcorio = 1.0   ! below this depth coriolis force scaled down linearly to 0

    jatidep   = 1     ! use tide potential forcing yes no

    jaselfal  = 0     ! use self attraction and loading yes no
    jaSELFALcorrectWLwithIni = 0   !< correct water level with initial atmospheric pressure in SAL

    ! DOODSONSTART = 55.565D0 ; DOODSONSTOP = 375.575D0 ; Doodsoneps = .00D0    ! standaard triwaq alle 484 cmp
      DOODSONSTART = 55.565D0 ; DOODSONSTOP = 375.575D0 ; Doodsoneps = .03D0    ! standaard triwaq       60 cmp
    ! DOODSONSTART = 57.555D0 ; DOODSONSTOP = 275.555D0 ; Doodsoneps = .03D0    ! Delft3d

    jasecflow = 0     ! include secondary flow (0=no, 1=yes)

    japillar  = 0     ! include pillar (0=no, 1=yes)

    jaequili  = 0     ! equilibrium secondary flow (0=no, 1=yes)

    jainirho = 1      ! Initialise rho at start at flowinit (otherwise first step without barocl)

    jacreep  = 0      ! Include anti-creep calculation, (0=no, 1=yes)

    jasal    = 0      ! Include salinity (autoset by flow_initexternalforcings())

    jatem    = 0      ! Temperature model

    janudge  = 0      ! temperature and salinity nudging
    jainiwithnudge = 0   !< initialize salinity and temperature with nudge variables

    jarhoxu  = 0      ! rho effects in momentum, 0=no, 1=in horizontal adv, 2=+ in vertical adv, 3 = + in pressure term

    jased    = 0      ! Include sediment

    jatrt    = 0      !< Include alluvial and vegetation roughness (trachytopes)

    jacali   = 0      !< Include calibration factor for roughness

    jawave   = 0      ! Include wave model nr

    jawavestreaming = 0   ! Switch on in D3D model: >=1 : streaming mom , >= 2 : streaming mom + turb

    jawaveStokes = 0      ! Vertical Stokes profile: 0=no, 1 = uniform, 2 = second order Stokes profile

    jawaveRoller = 0      ! Roller contribution: 0=no, 1 = Rol1, 2 = Rol2

    jawaveSwartDelwaq = 0 !< communicate to Delwaq taucur + tauwaveswart instead of taucur, specify z0wav

    modind = 0            !< Nr of wave-current bed friction model, 9 = vanrijn, 1 = fredsoe, etc like d3d

    jafrculin = 0     !< do not use linear friction

    jafrcInternalTides2D = 0   !< do not use internal tides friction

    javiusp  = 0      !< spatially varying eddyviscosity yes/no 1/0

    jadiusp  = 0      !< spatially varying eddydiffusivity yes/no 1/0

    jaCdwusp = 0

    jawindspeedfac = 0 !< use windspeedfac 1/0  

    ihorvic  = 0      !< 0=no visc, 1=do visc

    iuvfield = 0      ! intialise this velocityfield: 0 = no
                      ! 1:u=y**2, 2:idem, 60 deg, 3:rotation, 4=lin, 5=lin 60 deg

    istresstyp = 3    ! 1 : full stress tensor, semi  link oriented horvic2
                      ! 2 : full stress tensor, fully link oriented dvxc = ok and fast
                      ! 3 : 2, volume weighted
                      ! 4 : full node oriented
                      ! 5 : 4, volume weighted

    irov     = 0      ! 0 : free slip
                      ! 1 : partial slip
                      ! 2 : no slip
                      ! 3 : glass  (mind you, in D3DFLOW 3 means free slip)

    ibedlevmode = BLMODE_DFM !< Default: Compute bed levels solely by ibedlevtyp, i.e., derived from velocity points (or direct bl tiles).

    ibedlevtyp =  3   ! 1 : Bottom levels at waterlevel cells (=flow nodes), like tiles xz, yz, bl , bob = max(bl left, bl right)
                      ! 2 : Bottom levels at velocity points  (=flow links),            xu, yu, blu, bob = blu,    bl = lowest connected link
                      ! 3 : Bottom levels at velocity points  (=flow links), using mean network levels xk, yk, zk  bl = lowest connected link
                      ! 4 : Bottom levels at velocity points  (=flow links), using min  network levels xk, yk, zk  bl = lowest connected link
                      ! 5 : Bottom levels at velocity points  (=flow links), using max  network levels xk, yk, zk  bl = lowest connected link

    blmeanbelow  = -999d0
    blminabove   = -999d0

    ibedlevtyp1D = 3  !< 1 : same, 1D, 1 = tiles, xz(flow)=zk(net), bob(1,2) = max(zkr,zkl) , 3=mean netnode based

    izbndpos     = 0  !< 0 : waterlevel boundary location as in D3DFLOW, 1=on network boundary, 2=on specified boundary polyline
    jaupdbndbl   = 1  !< Update bl at boundary (1 = update, 0 = no update)

    nonlin1D     = 0  !< 1 : non-linear continuity eq, now governed by iproftyp in volsur, 0 for rectan, else 1
    nonlin2D     = 0  !< 1 : non-linear continuity eq in 2D, sets nonlin

    iproftypuni  = 3  !< 1 : circle, 2 : rectan R=A/P , 3 = rectan, R=H
    iproftypuni5 = -2 !< 1 : circle, 2 : rectan R=A/P , 3 = rectan, R=H
    iproftypuni7 = -2 !< 1 : circle, 2 : rectan R=A/P , 3 = rectan, R=H

    slotw2D      = 0d-3
    slotw1D      = 1d-3
    jafullgridoutput = 0 !output grid in compact manner or full manner
    jaeulervel       = 0 !GLM velocities

    jaconveyance2D  = 1 !

    bedslope    = 0d0    ! bottom inclination testcases
    Slopedrop2D = 0d0    ! Apply droplosses only if local bottom slope > Slopedrop2D, negative = no droplosses
    SLopedrop1D = .false.
    drop3D      = 1d0    ! Apply droplosses in 3D yes or no 1 or 0
    jacstbnd    = 0
    jajre       = 0
    jasourcesink= 1

    cflmx    = 0.7d0    ! max Courant nr ()
    cflw     = 0.1d0    ! wave velocity fraction, total courant vel = u + cflw*wavevelocity
    teta0    = 0.55d0   ! 1.00d0   ! .52      ! uniform teta in horizontal (),
    ivariableteta = 0   ! 0=fully implicit,   1=teta constant,        2=variable teta
                        ! (set teta=1.0)      (set teta=0.51->0.99)   (set teta<0)

    jaLogprofatubndin  = 1
    jaLogprofkepsbndin = 0

    limtypsa   = 4      ! 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC voor scalar tr-ansport SALINITY
    limtypTM   = 4      ! 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC voor scalar transport TEMPERATURE
    limtypsed  = 4      ! 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC voor scalar transport SEDIMENT
    limtyphu   = 0      ! 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC voor hu WATERHEIGHT AT U POINT
    limtypmom  = 4      ! 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC voor MOMENTUM transport
    jalimnor   = 0      ! 0=limit x/y components, 1=limit normal/tangetial components
    limtypw    = 4

    ifixedweirscheme      = 6      !< 0 = no special treatment, setbobs only, 1 = compact stencil, 2 = whole tile lifted, full subgrid weir + factor
    fixedweircontraction  = 1d0    !< flow width = flow width*fixedweircontraction
    fixedweirtopwidth     = 3d0    !< e.g. 4.00 (m)
    fixedweirtopfrictcoef = -999d0 !< if .ne. dmiss, use this friction coefficient on top width
    fixedweirtalud        = 4d0    !< e.g. 1 to 4 talud
    isimplefixedweirs     = 1
    ifxedweirfrictscheme  = 0   !< 0 = friction based on hu, 1 = friction based on subgrid weirfriction scheme

    iNormalMethod         = 0
    jaimplicit            = 0
    jafilter              = 0
    filterorder           = 2
    jacheckmonitor        = 0

    sini       = 0d0     ! uniform initial waterlevel (m),   (uniform bottom level = zkuni)
    waterdepthini1D = -999d0
    uini       = 0       ! uniform initial velocity   (m/s)
    salini     = 0d0     !< uniform initial sal       (ppt)
    temini     = 6d0     !< uniform initial temp      (degC)
    spirini    = 0d0     !< uniform initial spirint   (m/s)

    Sal0abovezlev = -999d0 !< if not dmiss, only seta salini below this level
    zkdropstep = 1d-2    !< Amount of bottomlevel to be added with dropland (m)
    sdropstep  = 1d0     !< Amount of water to be added with dropwater (m)
    uniformhu  = -999d0  !< Uniformhu

    zbnd       = 2d0     ! for now only, uniform waterlevel on boundary

    eps4       = 1d-4    ! min au in poshchk
    eps6       = 1d-6    !
    eps8       = 1d-8    ! implicit diffusion
    eps10      = 1d-10   !

    s01max     = 0d0     ! max. water level change: off
    u01max     = 0d0     ! max. velocity change: off
    ! See also: m_flowtimes::dtminbreak

                         ! parameters controlling flooding/drying/solving
    epshu      = 1d-4    ! minimum waterdepth for setting hu>0
    epshs      = .2d0*epshu ! minimum waterdepth for setting cfu
    epswav     = 1d-2    ! minimum waterdepth for wave calculations
    chkhuexpl  = 0.1d0   ! only for step_explicit:  check computed flux beneath this waterdepth
    chkadvd    = 0.1d0   ! check advection  for 'drying' below this (upwind) waterdepth
    chktempdep = 0.1d0   ! check heatfluxes for 'drying' below this waterdepth

    jposhchk   = 2       ! check for positive waterdepth; 0 = no
                         !                                1 = 0.7*dts, just redo
                         !                                2 = 1.0*dts, close all links
                         !                                3 = 0.7*dts, close all links
                         !                                4 = 1.0*dts, reduce au
                         !                                5 = 0.7*dts, reduce au
    jsolpos    = 0       ! in iterative solver force solution above bottom level
    Icgsolver  = 4       !    Icgsolver = 1      ! 1 = GS_OMP, 2 = GS_OMPthreadsafe, 3 = GS, 4 = Saadilud
    ipre       = 0       ! preconditioner, 0=rowscaling, 1=GS, 2=trial
    jajipjan   = 5       ! jipjan arrays in gauss

    hwetbed    = 0.2d0   ! for case wetbed

    javau      = 3       !< vert. adv. u1   : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL, 5=Quick, 6=centerbased upw. expl.
    javakeps   = 3       !< vert. adv. keps : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL
    javasal    = 6       !< vert. adv. sa1  : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL, 5=switched to 3 for neg stratif, 6=hoexplicit.
    javatem    = 6       !< vert. adv. tem1 : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL, 5=switched to 3 for neg stratif.
    javased    = 6       !< vert. adv. suspended sediment concentrations : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL, 5=switched to 3 for neg stratif., 6=higher-order upwind/explicit
    jahazlayer = 0       !<
    JaZlayercenterbedvel = 1
    JaZerozbndinflowadvection = 0

    jabaroctimeint = -3  !< time integration baroclini pressure, 1 = explicit, abs() = 2; adams bashford , 3 = ab3, 5 = adv rho
    jabarocterm = 2      ! revised baroc term

    jaanalytic = 0                    !< analytic solution available in black sideview => do not also show computed surface in black
    jaustarint                = 1     !< 1=integral bed layer velocity,  0=velocity at half bed layer
    Eddyviscositybedfacmax    = 0d0   !< eddyviscosityatbed = min(eddyviscosityatbed, eddyviscosityatbedfacmax*eddyviscosityatbed+1 )
    Eddyviscositysurfacmax    = 0d0   !< eddyviscosityatbed = min(eddyviscosityatsur, eddyviscosityatseufacmax*eddyviscosityatsur-1 )
    inisal2D                  = 0     !< 1 if specified through meteo module
    initem2D                  = 0     !< 1 if specified through meteo module
    inised2D                  = 0     !< 1 if specified through meteo module
    inivel                    = 0     !< initial velocities prescribed (1) or not (other)

    toplayminthick            = 0.01d0 ! minimum top layer thickness (m)

    jbasqbnddownwindhs = 0            !< 0 : original hu on qbnd, 1 = downwind hs on qbnd

    maxitverticalforestersal  = 0     !< 100, max iterations vertical forester
    maxitverticalforestertem  = 0     !< 100, max iterations vertical forester

    salmax = 0d0                      !< filter if sal > maxsal
    ! Remaining of variables is handled in reset_flowparameters()
    ! call reset_flowparameters()

    iparms = 0    ! parms-default
    dparms = 0d0  ! parms-default

    jaupwindsrc = 1

    jahisbal = 1
    jahissourcesink = 1
    jahistur = 1
    jahiswind = 1
    jahisrain = 1
    jahistem = 1
    jahisheatflux = 1
    jahissal = 1
    jahisrho = 1
    jahiswatlev = 1
    jahisbedlev = 1
    jahiswatdep = 0
    jahisvelvec = 1
    jahisww = 0
    jahissed = 1
    jahisconst = 1
    jahiszcor  = 1
    jahiswav = 1

    jamaps0 = 1
    jamaps1 = 1
    jamapvol1 = 0
    jamaphu = 0
    jamapanc = 0
    jamapau = 0
    jamapu0 = 1
    jamapu1 = 1
    jamapucvec = 1
    jamapucmag = 1
    jamapucqvec = 0
    jamapww1 = 1
    jamapnumlimdt = 1
    jamaptaucurrent = 1
    jamapchezy = 1
    jamapsal = 1
    jamaptem = 1
    jamapconst = 1
    jamapsed = 1
    jamaptur = 1
    jamaptrachy = 1
    jamapcali = 1
    jamaprain = 0
    jamapwind = 1
    jamapwindstress = 0
    jamapviu = 1
    jamapdiu = 1
    jamaprho = 1
    jamapq1  = 1
    jamapq1main = 0
    jamapspir = 1
    jamaptidep = 1
    jamapselfal = 1
    jamapIntTidesDiss = 1
    jamapNudge = 1
    jamapwav = 1
    jamapdtcell = 0
    jamapTimeWetOnGround = 0
    jamapFreeboard = 0
    jamapDepthOnGround = 0
    jamapVolOnGround = 0
    jamapTotalInflow1d2d = 0
    jamapTotalInflowLat = 0
    jamapS1Gradient = 0
    jatekcd = 1     ! wind cd coeffs on tek
    jarstbnd = 1
    japartdomain = 1
    jashp_crs = 0
    jashp_obs = 0
    jashp_weir= 0
    jashp_thd = 0
    jashp_gate= 0
    jashp_emb = 0
    jashp_fxw = 0
    jashp_src = 0
    jashp_pump= 0
    jashp_dry = 0
    jashp_genstruc = 0

    ispirparopt = 1


    Tnudgeuni                   = 3600d0        !< uniform nudge relaxation time (s)
    ITcap                       = 0d0           !< limit to Internal Tides Dissipation / area (J/(m^2 s))

    jatransportautotimestepdiff = 0

    call reset_flowparameters()
end subroutine default_flowparameters

!> Resets only flowparameters variables intended for a restart of an existing flow simulation (same MDU).
!! Upon loading of new model/MDU, call default_flowparameters() instead.
subroutine reset_flowparameters()
end subroutine reset_flowparameters

end module m_flowparameters


module m_statistics
implicit none
 double precision                  :: avedif     !< for now only, cum dif with analytic sol
 double precision                  :: sqadif     !< for now only, cum dif with analytic sol
 double precision                  :: rmsdif     !< for now only, cum dif with analytic sol
 double precision                  :: dmxdif     !< for now only, cum dif with analytic sol
 integer                           :: numdif

 double precision                  :: cumavedif  !< for now only, cum dif with analytic sol
 double precision                  :: cumrmsdif  !< for now only, cum dif with analytic sol
 double precision                  :: cumdmxdif  !< for now only, cum dif with analytic sol
 integer                           :: numcum
contains
subroutine reset_statistics()
    avedif    = 0d0    ! for now only, cum dif with analytic sol
    sqadif    = 0d0
    rmsdif    = 0d0    ! for now only, cum dif with analytic sol
    dmxdif    = 0d0    ! for now only, cum dif with analytic sol
    numdif    = 0


    cumavedif = 0d0    ! for now only, cum dif with analytic sol
    cumrmsdif = 0d0    ! for now only, cum dif with analytic sol
    cumdmxdif = 0d0    ! for now only, cum dif with analytic sol
    numcum    = 0
end subroutine reset_statistics
end module m_statistics

module m_vegetation
 integer                           :: javeg     = 0           ! 0,1,2,3 , jabaptist is javeg
                                                              ! only for kmx == 0:
 integer                           :: jabaptist = 0           ! 1 = use standard baptist, only cfuhi      unfortunately, taubed/ro is not computed correctly
                                                              ! 2 = use DFM formulation for cfuhi and alfaveg, such that taubed/ro=cfu*umod**2
                                                              ! 3 = use cfuveg and alfaveg provided by python, such that taubed/ro=cfu*umod**2
 double precision                  :: densvegminbap = 0d0     ! minimum vegetation density for baptist formulation (1/m2)
 integer                           :: jaCdvegsp = 0           ! 1 = use bmi for Cdvegsp
 double precision, allocatable, target :: rnveg (:)           !< [1/m2] 3D plant density , 2D part is basis input (1/m2) {"location": "face", "shape": ["ndkx"]}
 double precision, allocatable, target :: diaveg(:)           !< [m] 3D plant diameter, 2D part is basis input (m) {"location": "face", "shape": ["ndkx"]}
 double precision, allocatable, target :: cfuveg(:)           !< [ ]   2D only, g/C2 in 2D such that bedstress is computed correctly {"location": "face", "shape": ["lnx"]}
 double precision, allocatable, target :: alfaveg(:)          !< [1/m] 2D only, stem contribution {"location": "face", "shape": ["lnx"]}
 double precision, allocatable, target :: stemdens(:)         !< [1/m2] TEMP 2D plant density (1/m2) {"location": "face", "shape": ["ndx"]}
 double precision, allocatable, target :: stemdiam(:)         !< [m] TEMP 2D plant diameters (m) {"location": "face", "shape": ["ndx"]}
 double precision, allocatable, target :: stemheight(:)       !< [m] 2D plant heights (m) {"location": "face", "shape": ["ndx"]}
 double precision, allocatable, target :: Cdvegsp(:)          !< [m] spatial plant Cdveg () {"location": "face", "shape": ["ndkx"]}
 double precision, allocatable     :: alfav(:)                !< [1/m] used in DFM, computed onboard for jabaptist==2, or pyton if jabaptist==3
 double precision, allocatable     :: phiv(:)                 ! 2D plant stem angle ()
 double precision, allocatable     :: phivt(:)                ! 2D plant angle velocity (1/s)
 double precision                  :: Clveg   = 0.8d0         ! factor on average stem distance ( ) (eps. eq.)
 double precision                  :: Cdveg   = 0.7d0         ! Cd drag coefficient  ( )
 double precision                  :: Cbveg   = 0.0d0         ! Bend stiffness coefficient (kg.m2/s2) Moment=Cbveg.phiv
 double precision                  :: Rhoveg  = 0d0           ! if > 0d0 then floatmodel
 double precision                  :: Stemheightstd = 0d0     ! stemheight standard deviation
 double precision                  :: r3      = .333333d0     !
 double precision                  :: growthunidicouv         ! uniform values in veg growth model diffusion coef
 double precision                  :: growthunidiam           ! uniform values in veg growth model diam
 double precision                  :: growthuniheight         ! uniform values in veg growth model height

 double precision                  :: expchistem = 0d0
 double precision                  :: uchistem   = 0d0
 double precision                  :: expchileaf = 0d0
 double precision                  :: uchileaf   = 0d0
 double precision                  :: arealeaf   = 0d0
 double precision                  :: Cdleaf     = 1d0

end module m_vegetation

 module m_flow   ! flow arrays-999
 use    m_flowparameters
 use    m_flowexternalforcings
 use    m_physcoef
 use    m_turbulence
 use    m_grw
! use    m_fixedweirs
 use    m_heatfluxes
 use    m_alloc
 use    m_vegetation
 use    m_ship

 implicit none


 ! 3D parameters
 integer                           :: kmx               !< nr of 3d layers, increasing in positive upward direction
                                                        !! if kmx==0 then 2D code. if kmx==1 then 3D code
 integer                           :: kmx1              !< kmx + 1, for dimensioning arrays that used to be (0:kmax)
 integer                           :: kmxd              !< dim of kmx, >= 1
 integer                           :: ndkx              !< dim of 3d flow nodes (internal + boundary)
 integer                           :: ndkx1             !< dim of 3d flow horizontal interfaces (internal + boundary), (0:kmx)
 integer                           :: lnkx              !< dim of 3d flow links (internal + boundary)
 integer                           :: numvertdis        !< number of           vertical layer distributions
 integer                           :: mxlayz            !< max nr of z     layers in flow domain
 integer                           :: mxlays            !< max nr of sigma layers in flow domain
 integer                           :: kplot             !< layer nr to be plotted
 integer                           :: nplot             !< vertical profile to be plotted at node nr
 integer                           :: kplotfrombedorsurface = 1 !< up or down k
 integer                           :: kplotordepthaveraged  = 1 !< 1 = kplot, 2 = averaged
 integer                           :: layertype         !< 1= all sigma, 2 = all z, 3 = left sigma, 4 = left z
 integer                           :: numtopsig = 0     !< number of top layers in sigma
 double precision                  :: Tsigma = 100      !< relaxation period density controlled sigma
 integer, parameter                :: LAYTP_SIGMA     = 1
 integer, parameter                :: LAYTP_Z         = 2
 integer, parameter                :: LAYTP_LEFTSIGMA = 3
 integer, parameter                :: LAYTP_LEFTZ     = 4

 integer                           :: iStrchType      = -1 !< Stretching type for non-uniform layers, 1=user defined, 2=exponential, otherwise=uniform
 integer, parameter                :: STRCH_USER      = 1
 integer, parameter                :: STRCH_EXPONENT  = 2

 integer                           :: iturbulencemodel  !< 0=no, 1 = constant, 2 = algebraic, 3 = k-eps
 integer                           :: ieps              !< bottom boundary type eps. eqation, 1=dpmorg, 2 = dpmsandpit, 3=D3D, 4=Dirichlethdzb
 double precision                  :: sigmagrowthfactor !<layer thickness growth factor from bed up
 double precision                  :: dztopuniabovez  = -999d0     !< bottom level of lowest uniform layer == blmin if not specified
 double precision                  :: Floorlevtoplay  = -999d0     !< floor  level of top zlayer, == sini if not specified
 double precision                  :: dztop = -999d0     !< if specified, dz of top layer, kmx = computed, if not, dz = (ztop-zbot)/kmx
 integer                           :: jaorgFloorlevtoplaydef=0 !< 0=correct floorlevtoplay, 1 = org wrong floorlevtoplay
 double precision                  :: zlaybot = -999d0  !< if specified, first zlayer starts from zlaybot, if not, it starts from the lowest bed point
 double precision                  :: zlaytop = -999d0  !< if specified, highest zlayer ends at zlaytop, if not, it ends at the initial water level
 double precision, allocatable     :: aak (:)           !< coefficient vertical mom exchange of kmx layers
 double precision, allocatable     :: bbk (:)           !< coefficient vertical mom exchange of kmx layers
 double precision, allocatable     :: cck (:)           !< coefficient vertical mom exchange of kmx layers
 double precision, allocatable     :: ddk (:)           !< coefficient vertical mom exchange of kmx layers
 double precision, allocatable     :: eek (:)           !< coefficient vertical mom exchange of kmx layers
 double precision, allocatable     :: uuk (:)           !< coefficient vertical mom exchange of kmx layers

 double precision, allocatable     ::laycof(:)          !< coefficients for sigma layer
                                                        !    1: Percentages of the layers, user defined, laycof(kmx)
                                                        !    2: Stretching level, and two coefficients for layers growth, laycof(3)
                                                        !
 double precision, allocatable     ::dzslay(:,:)        ! the normalized thickness of layer, dim = (: , maxlaydefs)

                                                        !double precision, allocatable     :: dzu(:)           !< vertical layer size at layer centre    at u-velocity points (m) 1:kmx Local
 !double precision, allocatable     :: dzw(:)           !< vertical layer size at layer interface at u-velocity points (m) 1:kmx Local
                                                        !< 1:kmx: bottom interface not included

 double precision, allocatable,target :: zws (:)        !< [m] z levels  (m) of interfaces (w-points) at cell centres (s-points) (m)    (1:ndkx) {"shape": ["ndkx"]}
 double precision, allocatable        :: zws0(:)        !< z levels  (m) of interfaces (w-points) at cell centres (s-points) (m)    (1:ndkx), be

                                                        !!
                                                        !!-------------------------------------  zws(2) = interface(2), zws (ktop(ndx) ) == s1(ndx)
                                                        !!                     |
                                                        !!                     |
                                                        !!          +          |                          layer (2)
                                                        !!                     |
                                                        !!                     |
                                                        !!-------------------------------------  zws(1) = interface(1)
                                                        !!                     |
                                                        !!                     |
                                                        !!          +          |                          layer (1)
                                                        !!                     |
                                                        !!                     |
                                                        !!-------------------------------------  zws(0) = interface(0) = bl
                                                        !!

 double precision, allocatable, target :: zcs(:)        !< z levels at layer mid-points, only for nudging

                                                        !< [m] waterlevel    (m ) at start of timestep {"location": "face", "shape": ["ndx"]}
 integer, allocatable, target      :: kbot(:)           !< [-] layer-compressed bottom layer cell number: for each of ndx horizontal cells, we have indices to bot and top ndxk cells {"location": "face", "shape": ["ndx"]}
 integer, allocatable, target      :: ktop(:)           !< [-] layer-compressed top layer cell number: for each of ndx horizontal cells, we have indices to bot and top ndxk cells {"location": "face", "shape": ["ndx"]}
 integer, allocatable              :: ktop0(:)          !< store of ktop
 integer, allocatable              :: kmxn(:)           !< max nr of vertical cells per base cell n
 integer, allocatable, target      :: Lbot(:)           !< [-] layer-compressed bottom layer edge number: for each of lnx horizontal links, we have indices to bot and top lnxk links {"location": "edge", "shape": ["lnx"]}
 integer, allocatable, target      :: Ltop(:)           !< [-] layer-compressed top layer edge number: for each of lnx horizontal links, we have indices to bot and top lnxk links {"location": "edge", "shape": ["lnx"]}
 integer, allocatable              :: kmxL(:)           !< max nr of vertical links per base link L
 integer, allocatable              :: kbotc(:)          !< as kbot, for cornerpoints
 integer, allocatable              :: kmxc(:)           !< as kmxn, for cornerpoints

 integer                           :: mxlaydefs=4       !< max nr of layering definitions
 integer, allocatable              :: laydefnr(:)       !< dim = (ndx), pointer to laydef, if positive to unique laydef, otherwise interpolate in 1,2, and 3
 integer, allocatable              :: laytyp(:)         !< dim = (mxlaydefs), 1 = sigma, 2 = z
 integer, allocatable              :: laymx(:)          !< dim = (mxlaydefs), max nr of layers
 double precision, allocatable     :: zslay(:,:)        !< dim = (: , maxlaydefs) z or s coordinate,
 double precision, allocatable     :: wflaynod(:,:)     !< dim = (3 , ndx) weight factors to flownodes indlaynod
 integer,          allocatable     :: indlaynod(:,:)    !< dim = (3 , ndx)
 double precision, allocatable     :: dkx(:)            !< dim = ndx, density controlled sigma, sigma level of interface height
 double precision, allocatable     :: sdkx(:)           !< dim = ndx, density controlled sigma, sum of .., only layertype == 4

 double precision, allocatable     :: asig(:)           !< alfa of sigma at nodes, 1d0=full sigma, 0d0=full z, 0.5d0=fifty/fifty
 double precision, allocatable     :: ustb(:)           !< ustar at Lbot, dim=Lnx,
 double precision, allocatable     :: ustw(:)           !< ustar at Ltop, dim=Lnx
 double precision, allocatable     :: ustbc(:)          !< ustar at bed at netnodes, dim=numk

 integer                           :: nfixed, nsigma

 ! flow arrays

 ! node related, dim = ndx
 double precision, allocatable, target :: s0(:)       !< [m] waterlevel    (m ) at start of timestep {"location": "face", "shape": ["ndx"]}
 double precision, allocatable, target :: s1(:)       !< [m] waterlevel    (m ) at end   of timestep {"location": "face", "shape": ["ndx"]}
 double precision, allocatable         :: s00(:)      !< waterlevel    (m ) for checking iteration in nonlin
 double precision, allocatable, target :: a0(:)       !< [m2] storage area at start of timestep {"location": "face", "shape": ["ndx"]}
 double precision, allocatable, target :: a1(:)       !< [m2] storage area at end of timestep {"location": "face", "shape": ["ndx"]}
 double precision, allocatable, target :: vol0(:)     !< [m3] total volume at start of timestep {"location": "face", "shape": ["ndx"]}
 double precision, allocatable, target :: vol1(:)     !< [m3] total volume at end of timestep {"location": "face", "shape": ["ndx"]}
 double precision, allocatable, target :: vol1_f(:)     !< [m3] flow volume volume at end of timestep {"location": "face", "shape": ["ndx"]}
 double precision, allocatable         :: sq(:)       !< total  influx (m3/s) at s point
 double precision, allocatable         :: sqa(:)      !< total  out! flux (m3/s) at s point, u1 based, non-conservative for iadvec == 38
 double precision, allocatable, target :: hs(:)       !< [m] waterdepth at cell centre = s1 - bl  (m) {"location": "face", "shape": ["ndx"]}
 double precision, allocatable         :: cfs(:)      !< dimensionless friction coefficient sag/C in cell centre
 double precision, allocatable         :: volerror(:) !< volume error

 double precision, allocatable         :: voldhu(:)   !< node volume based on downwind hu

 double precision, allocatable         :: s1m(:)      !< waterlevel   pressurized nonlin minus part
 double precision, allocatable         :: s1mini(:)   !< initial of s1m
 double precision, allocatable         :: a1m(:)      !< surface area pressurized nonlin minus part

! node related, dim = ndkx

 double precision, allocatable         :: volau   (:)   !< trial, au based cell volume (m3)
 double precision, allocatable, target :: ucx   (:)   !< [m/s] cell center velocity, global x-dir (m/s) {"location": "face", "shape": ["ndkx"]}
 double precision, allocatable, target :: ucy   (:)   !< [m/s] cell center velocity, global y-dir (m/s) {"location": "face", "shape": ["ndkx"]}
 double precision, allocatable, target :: ucz   (:)   !< [m/s] cell center velocity, global z-dir (m/s) {"location": "face", "shape": ["ndkx"]}
 double precision, allocatable, target :: ucxq  (:)   !< cell center velocity, q based  global x-dir (m/s)
 double precision, allocatable, target :: ucyq  (:)   !< cell center velocity, q based  global y-dir (m/s)
 double precision, allocatable         :: uqcx  (:)   !< cell center incoming momentum, global x-dir (m4/s2), only for iadvec = 1
 double precision, allocatable         :: uqcy  (:)   !< cell center incoming momentum, global y-dir (m4/s2), only for iadvec = 1
 double precision, allocatable, target :: ucmag (:)   !< [m/s] cell center velocity magnitude {"location": "face", "shape": ["ndkx"]}
 double precision, allocatable         :: cfli  (:)   !< sum of incoming courants (    ) = sum( Dt*Qj/Vi)
 double precision, allocatable         :: dvxc  (:)   !< cell center stress term, global x-dir (m3/s2)
 double precision, allocatable         :: dvyc  (:)   !< cell center stress term, global y-dir (m3/s2)
 double precision, allocatable         :: squ   (:)   !< cell center outgoing flux (m3/s)
 double precision, allocatable         :: sqi   (:)   !< cell center incoming flux (m3/s)
 double precision, allocatable         :: squ2D (:)   !< cell center outgoing 2D flux (m3/s)
 double precision, allocatable         :: sqwave(:)   !< cell center outgoing flux, including gravity wave velocity (m3/s) (for explicit time-step)
 double precision, allocatable         :: squcor(:)    !< cell center outgoing flux with some corrections to exclude structure links (if enabled)
 double precision, allocatable         :: hus   (:)   !< hu averaged at 3D cell 
 double precision, allocatable         :: workx (:)   !< Work array
 double precision, allocatable         :: worky (:)   !< Work array
 double precision, allocatable         :: work0 (:,:) !< Work array
 double precision, allocatable         :: work1 (:,:) !< Work array


 double precision, allocatable         :: dsadx   (:)   !< cell center sa gradient, (ppt/m)
 double precision, allocatable         :: dsady   (:)   !< cell center sa gradient, (ppt/m)

! node related, dim = ndxi
 double precision, allocatable         :: freeboard(:)  !< [m] For output purposes: freeboard at cell center
 double precision, allocatable         :: hsOnGround(:) !< [m] For output purposes: waterdepth above ground level
 double precision, allocatable         :: volOnGround(:)!< [m3] For output purposes: volume above ground level
 double precision, allocatable         :: qCur1d2d(:)   !< [m3/s] total 1d2d net inflow, current discharge
 double precision, allocatable         :: vTot1d2d(:)   !< [m3] total 1d2d net inflow, cumulative volume
 double precision, allocatable         :: qCurLat(:)    !< [m3/s] total lateral net inflow, current discharge
 double precision, allocatable         :: vTotLat(:)    !< [m3] total lateral net inflow, cumulative volume
 
 ! link related, dim = lnx
 double precision, allocatable         :: s1Gradient(:) !< [1] For output purposes: water level gradient on flow links

!    Secondary Flow
 double precision, allocatable         :: ducxdx   (:)   !< cell center gradient of x-velocity in x-dir,    (1/s)
 double precision, allocatable         :: ducxdy   (:)   !< cell center gradient of x-velocity in y-dir,    (1/s)
 double precision, allocatable         :: ducydx   (:)   !< cell center gradient of y-velocity in x-dir,    (1/s)
 double precision, allocatable         :: ducydy   (:)   !< cell center gradient of y-velocity in y-dir,    (1/s)
! double precision, allocatable, target     :: dsdx   (:)   !< cell center gradient of waterlevel in x-dir,    ( )
! double precision, allocatable, target     :: dsdy   (:)   !< cell center gradient of waterlevel in y-dir,    ( )
! double precision, allocatable, target     :: dvdx   (:)   !< cell center gradient of y-velocity in x-dir,    (1/s)
! double precision, allocatable, target     :: dvdy   (:)   !< cell center gradient of y-velocity in y-dir,    (1/s)
! double precision, allocatable, target     :: rsi    (:)   !< 1/R_s inverse streamline curvature         ,    (1/m)
! double precision, allocatable, target     :: rsiexact(:)   !< 1/R_s inverse streamline curvature (exact) ,    (1/m)
! double precision, allocatable, target     :: uc3rsi (:)   !< cell center u_mod^3/R_s                    ,    (m^2/s^3)
 double precision, dimension(:), allocatable :: spircrv   !< 1/R_s streamline curvature                 ,    (1/m)
 double precision, dimension(:), allocatable :: spirint   !< spiral flow intensity                      ,    (m/s)
 double precision, dimension(:), allocatable :: spirsrc   !< source term for spiral flow intensity      ,    (m/s^2)
 double precision, dimension(:), allocatable :: spirfx    !< Secondary flow force for momentum in x-dir ,    (m/s^2)
 double precision, dimension(:), allocatable :: spirfy    !< Secondary flow force for momentum in y-dir ,    (m/s^2)
 double precision, dimension(:), allocatable :: spirucm   !< velocity in the flow node                  ,    (m/s)
 double precision, dimension(:), allocatable :: ht_xx     !< array hT_xx, for calculation of spirfx and spirfy
 double precision, dimension(:), allocatable :: ht_xy     !< array hT_xy, for calculation of spirfx and spirfy
 double precision, dimension(:), allocatable :: czusf       !< Chezy coefficient on flow link
 double precision, dimension(:), allocatable :: czssf       !< Chezy coefficient in flow node
 double precision, dimension(:), allocatable :: fcoris    !< Coriolis force in the flow node

 double precision, dimension(:), allocatable :: spiratx   !< x component of normalised vector in direction of depth averaged velocity    (-)
 double precision, dimension(:), allocatable :: spiraty   !< y component of normalised vector in direction of depth averaged velocity    (-)

 double precision                            :: spirE = 0d0     !< factor for weighing the effect of the spiral flow intensity on transport angle, Eq 11.45 of Delft3D manual
 double precision                            :: spirbeta = 0d0  !< factor for weighing the effect of the spiral flow on flow dispersion stresses, Eq 9.155 of Delft3D manual
 integer                                     :: numoptsf

! Anti-creep
 double precision, dimension(:),     allocatable :: dsalL   ! the flux of salinity    on flow linkes for anti-creep
 double precision, dimension(:),     allocatable :: dtemL   ! the flux of temperature on flow nodes  for anti-creep

 double precision, allocatable, target     :: sa0(:)   !< [1e-3] salinity (ppt) at start of timestep {"location": "face", "shape": ["ndkx"]}
 double precision, allocatable, target     :: sa1(:)   !< [1e-3] salinity (ppt) at end   of timestep {"location": "face", "shape": ["ndkx"]}
 double precision, allocatable, target     :: satop(:)   !< [1e-3] salinity (ppt) help in initialise , deallocated {"location": "face", "shape": ["ndx"]}
 double precision, allocatable, target     :: sabot(:)   !< [1e-3] salinity (ppt) help in initialise , deallocated {"location": "face", "shape": ["ndx"]}
 double precision, allocatable     :: supq  (:)   !< summed upwind salinity fluxes (ppt*m3/s)
 double precision, allocatable     :: qsho  (:)   !< higher order part of upwind salinity    fluxes (ppt*m3/s) (dim=lnkx)
 double precision, allocatable, target     :: tem0  (:)   !< [degC] water temperature at end of timestep {"location": "face", "shape": ["ndkx"]}
 double precision, allocatable, target     :: tem1  (:)   !< [degC] water temperature at end of timestep {"location": "face", "shape": ["ndkx"]}
 double precision, allocatable     :: Tupq  (:)   !< upwind temperature flux (deg*m3/s)
 double precision, allocatable     :: qtho  (:)   !< higher order part of upwind temperature fluxes (ppt*m3/s) (dim=lnkx)

 double precision, allocatable     :: sam0  (:)   !< salinity mass       (pptm3) at start of timestep  ! remove later
 double precision, allocatable     :: sam1  (:)   !< salinity mass       (pptm3) at end   of timestep  ! remove later
 double precision, allocatable     :: same  (:)   !< salinity mass error (pptm3) at end   of timestep  ! remove later

 double precision, allocatable     :: ww1   (:)   !< vertical velocity (m/s) end of timestep
 double precision, allocatable     :: qw    (:)   !< vertical flux through interface (m3/s)
 double precision, allocatable     :: tidep (:,:) !< tidal potential (m2/s2)
 double precision, allocatable     :: tidef (:)   !< tidal force (m/s2)
 double precision, allocatable     :: s1init (:)   !< initial water level, for correction in SAL

 double precision, allocatable     :: steric(:,:) !< sal and temp for steric correction in 1,* and 2,*
 double precision                  :: rhosteric   !< later maybe in spatial refdensity
 integer                           :: jasteric=0  !< use steric correction on open waterlevel bnds yes/no

 double precision, allocatable     :: vih   (:)   !< horizontal eddy viscosity in cell center (m2/s)
 double precision, allocatable     :: qin   (:)   !< rain, evap, qlat and src netto inloop (m3/s)

 double precision                     errmas      !< (cumulative) mass   error ()


! link related, dim = lnkx
 double precision, allocatable     :: u0    (:)   !< flow velocity (m/s)  at start of timestep
 double precision, allocatable, target     :: u1(:)   !< [m/s]  flow velocity (m/s)  at   end of timestep {"location": "edge", "shape": ["lnkx"]}
 double precision, allocatable, target     :: u_to_umain(:)   !< [-]  Factor for translating general velocity to the flow velocity in the main channel at end of timestep (1d) {"location": "edge", "shape": ["lnkx"]}
 double precision, allocatable, target     :: q1(:)   !< [m3/s] discharge     (m3/s) at   end of timestep n, used as q0 in timestep n+1, statement q0 = q1 is out of code, saves 1 array {"location": "edge", "shape": ["lnkx"]}
 double precision, allocatable, target     :: q1_main(:)   !< [m3/s] discharge     (m3/s) in main channel at {"location": "edge", "shape": ["lnkx"]}
 double precision, allocatable     :: qa    (:)   !< discharge (m3/s) used in advection, qa=au(n)*u1(n+1) instead of
 double precision, allocatable     :: cflj  (:)   !< courant nr link j to downwind volume i (    ) = Dt*Qj/Vi
 double precision, allocatable     :: tetaj (:)   !< 1-1/sum(upwind incoming courants)      (    )
 double precision, allocatable, target     :: au    (:)   !< [m2] flow area     (m2)   at u point {"location": "edge", "shape": ["lnkx"]}
 double precision, allocatable     :: ucxu  (:)   !< upwind link ucx (m/s)
 double precision, allocatable     :: ucyu  (:)   !< upwind link ucy (m/s)
 double precision, allocatable     :: advi  (:)   !< advection implicit part (1/s)
 double precision, allocatable     :: adve  (:)   !< advection explicit part (m/s2)
 double precision, allocatable     :: adve0 (:)   !< advection explicit part (m/s2) prevstep
 double precision, allocatable, target     :: hu    (:)   !< [m] upwind waterheight at u-point (m) {"location": "edge", "shape": ["lnx"]}
 double precision, allocatable     :: huvli (:)   !< inverse alfa weighted waterheight at u-point (m) (volume representative)
 double precision, allocatable     :: v     (:)   !< tangential velocity in u point (m/s)
 double precision, allocatable     :: suu   (:)   !< stress u dir (m/s2)
 double precision, allocatable     :: cfuhi (:)   !< g/(hCC) u point (1/m)
 double precision, allocatable, target :: frcu(:) !< [TODO] friction coefficient set by initial fields {"location": "edge", "shape": ["lnx"]}
 double precision, allocatable     :: frcu_mor(:) !< friction coefficient in morphologically active region set by initial fields {"location": "edge", "shape": ["lnx"]}
 double precision, allocatable     :: frcu_bkp(:) !< Backup of friction coefficient set by initial fields {"location": "edge", "shape": ["lnx"]}
 double precision, allocatable     :: cfclval(:)  !< array for calibration factor for friction coefficients
 double precision, allocatable     :: cftrt(:,:)  !< array for friction coefficients due to trachytopes
 double precision, allocatable     :: cftrtfac(:) !< array for optional multiplication factor for trachytopes's returned roughness values
 integer                           :: jacftrtfac  !< Whether or not (1/0) a multiplication factor field was specified for trachytopes's Chezy roughness values.
 double precision, allocatable     :: czs(:)      !< array for chezy friction at cell centers {"location": "face", "shape": ["ndxi"]}
 double precision, allocatable     :: frculin(:)  !< friction coefficient set by initial fields ( todo mag later ook single real worden)
 integer,          allocatable     :: ifrcutp(:)  !< friction coefficient type   initial fields ( todo mag later ook single real worden)
 double precision, allocatable     :: Cdwusp(:)   !< Wind friction coefficient at u point set by initial fields ( todo mag later ook single real worden)
 double precision, allocatable     :: Windspeedfac(:) !< Wind friction coefficient at u point set by initial fields ( todo mag later ook single real worden)
 double precision, allocatable     :: z0ucur(:)   !< current related roughness, moved from waves, always needed
 double precision, allocatable     :: z0urou(:)   !< current and wave related roughness

 double precision, allocatable     :: frcuroofs(:)!< temp

 double precision, allocatable     :: frcInternalTides2D(:) !< internal tides friction coefficient gamma, tau/rho = - gamma u.grad h grad h

 double precision, allocatable     :: wavfu (:)   !< wave force u point
 double precision, allocatable     :: wavfv (:)   !< wave force u point
 real            , allocatable     :: wdsu  (:)   !< windstress u point  (m2/s2)
 real            , allocatable     :: wdsu_x(:)   !< windstress u point  (m2/s2) x-component
 real            , allocatable     :: wdsu_y(:)   !< windstress u point  (m2/s2) y-component
 double precision, allocatable     :: wavmubnd (:)   !< wave-induced mass flux (on open boundaries)
 real            , allocatable     :: vicLu   (:) !< horizontal eddy viscosity coefficient at u point (m2/s)  (limited only if ja_timestep_auto_visc==0)
 real            , allocatable     :: viu   (:)   !< horizontal eddy viscosity coefficient at u point (m2/s), modeled part of viscosity = vicLu - viusp
 double precision, allocatable, target    :: viusp(:)   !< [m2/s] user defined spatial eddy viscosity coefficient at u point (m2/s) {"location": "edge", "shape": ["lnx"]}
 double precision, allocatable, target    :: diusp(:)   !< [m2/s] user defined spatial eddy diffusivity coefficient at u point (m2/s) {"location": "edge", "shape": ["lnx"]}
                                                        !< so in transport, total diffusivity = viu*sigdifi + diusp
 real            , allocatable     :: fcori (:)   !< spatially variable fcorio coeff at u point (1/s)
double precision, allocatable     :: fvcoro (:)  !< 3D adamsbashford u point (m/s2)

 real            , allocatable     :: tidgs (:)   !< spatially variable earth tide potential at s point (m2/s2)
 double precision, allocatable     :: plotlin(:)  !< for plotting on u points
 integer         , allocatable     :: numlimdt(:) !< nr of times this point was the timestep limiting point
 integer                           :: numlimdt_baorg = 0  !< nr of times limiting > numlimdt_baorg, keep org ba
 double precision                  :: baorgfracmin   = 0  !< ba = max(cutarea, ba*baorgfracmin)

 double precision, allocatable     :: zn2rn (:)   !< weight from zn to rn, flownode to netnode

 double precision, allocatable, target :: taus  (:)   !< cell centre tau N/m2
 double precision, allocatable     :: q1waq (:)   !< Cumulative q1 within current waq-timestep
 double precision, allocatable     :: qwwaq (:)   !< Cumulative qw within current waq-timestep


 ! solving related, dim = ndx for 2D, otherwise ndx*kmxd
 double precision, allocatable     :: fu    (:)   !< main diag (lnx)
 double precision, allocatable     :: ru    (:)   !< rhs       (lnx)
 double precision, allocatable     :: bb    (:)   !< main diag (ndx)
 double precision, allocatable     :: dd    (:)   !< rhs       (ndx)

 integer, allocatable :: struclink(:)

 ! basis
 double precision                  :: vol0tot     !< Total volume start of timestep            (m3)
 double precision                  :: vol1tot     !< Total volume   end of timestep            (m3)
 double precision                  :: vol1ini     !< Total volume   initially                  (m3)
 double precision                  :: Volgrw      !< Total volume grw end of timestep          (m3)
 double precision                  :: Volgrwini   !< Total volume grw initially                (m3)

 double precision                  :: qinbnd      !< Actual influx boundaries                  (m3/s)
 double precision                  :: qoutbnd     !< Actual outflux boundaries                 (m3/s)
 double precision                  :: qincel      !< Actual influx cells                       (m3/s)
 double precision                  :: qoutcel     !< Actual outflux cells                      (m3/s)

 double precision                  :: vinbnd      !< Volume in  boundaries of timestep         (m3)
 double precision                  :: voutbnd     !< Volume out boundaries of timestep         (m3)
 double precision                  :: vincel      !< Volume in  cells      of timestep         (m3)
 double precision                  :: voutcel     !< Volume out cells      of timestep         (m3)
 double precision                  :: volerr      !< Volume error of timestep vol1tot - vol0tot - vinbnd + voutbnd - vincel + voutcel   (m3)

 double precision                  :: vinbndcum   !< Cumulative volume through boundaries in   (m3) Cumulative values
 double precision                  :: voutbndcum  !< Cumulative volume through boundaries out  (m3)
 double precision                  :: vincelcum   !< Cumulative volume in  cells               (m3/s) Actual values
 double precision                  :: voutcelcum  !< Cumulative volume out cells               (m3/s)
 double precision                  :: volerrcum   !< Volume error since start of computation   (m3)

 double precision                  :: dvolbot     !<     (m3), associated with jamorf

 ! extra
 double precision                  :: qinrain     !< total influx rain                         (m3/s)
 double precision                  :: qouteva     !< total outflux evaporation                 (m3/s)
 double precision                  :: qinlat      !< total influx diffuse laterals             (m3/s)
 double precision                  :: qoutlat     !< total outflux diffuse laterals            (m3/s)
 double precision                  :: qingrw      !< total influx groundwater                  (m3/s)
 double precision                  :: qoutgrw     !< total outflux groundwater                 (m3/s)
 double precision                  :: qinsrc      !< total influx local point sources          (m3/s)
 double precision                  :: qoutsrc     !< total outflux local pount sources         (m3/s)

 double precision                  :: vinrain     !< total volume in  rain                     (m3) in the last time step
 double precision                  :: vouteva     !< total volume out evaporation              (m3) "
 double precision                  :: vinlat      !< total volume in  diffuse laterals         (m3) "
 double precision                  :: voutlat     !< total volume out diffuse laterals         (m3) "
 double precision                  :: vingrw      !< total volume in  groundwater              (m3) "
 double precision                  :: voutgrw     !< total volume out groundwater              (m3) "
 double precision                  :: vinsrc      !< total volume in  local point sources      (m3) "
 double precision                  :: voutsrc     !< total volume out local pount sources      (m3) "

 double precision                  :: vinraincum  !< total inflow from rain                    (m3) integrated over all time steps
 double precision                  :: voutevacum  !< total outflow to evaporation              (m3) "
 double precision                  :: vinlatcum   !< total inflow from diffuse laterals        (m3) "
 double precision                  :: voutlatcum  !< total outflow to diffuse laterals         (m3) "
 double precision                  :: vingrwcum   !< total inflow from groundwater             (m3) "
 double precision                  :: voutgrwcum  !< total outflow to groundwater              (m3) "
 double precision                  :: vinsrccum   !< total inflow from local point sources     (m3) "
 double precision                  :: voutsrccum  !< total outflow to local pount sources      (m3) "

 double precision                  :: DissInternalTides  !< total Internal Tides Dissipation (J/s)
 double precision, allocatable     :: DissInternalTidesPerArea(:)  !< internal tides dissipation / area (J/(m^2 s))
 double precision                  :: GravInput          !< total Gravitational Input (incl. SAL) (J/s)
 double precision                  :: SALInput           !< total SAL Input (J/s)
 double precision                  :: SALInput2          !< total SAL Input (J/s), different formulation


 double precision                  :: a0tot       !< total wet surface area start of timestep (m2)
 double precision                  :: a1tot       !< total wet surface area   end of timestep (m2)
 double precision                  :: a1ini       !< total model area rain evap               (m2)
 double precision                  :: ek1tot      !< volume averaged kin energy (m2/s2) end of timestep
 double precision                  :: ep1tot      !< volume averaged pot energy (m2/s2) end of timestep
 double precision                  :: ep1rela     !< time av ep1tot
 double precision                  :: hsaver      !< average waterdepth (m), vol/are

 ! basis zout
 double precision                  :: sam0tot      !< Total mass start of timestep            (m3ppt)
 double precision                  :: sam1tot      !< Total mass   end of timestep            (m3ppt)
 double precision                  :: sam1ini=-1d0 !< Total mass initially                    (m3ppt)

 double precision                  :: saminbnd     !< Actual mass in  boundaries of timestep  (m3ppt)
 double precision                  :: samoutbnd    !< Actual mass out boundaries of timestep  (m3ppt)
 double precision                  :: samerr       !< vol1tot - vol0tot - vinbnd + voutbnd - vincel + voutcel   (m3)

 double precision                  :: saminbndcum  !< Cumulative mass in  boundaries          (m3ppt) Cumulative values
 double precision                  :: samoutbndcum !< Cumulative mass out boundaries          (m3ppt)
 double precision                  :: samerrcum    !< Mass error since start of computation   (m3ppt)

 double precision                  :: epsmaxvol       !< eps vol diff (m3) ! both not used now
 double precision                  :: difmaxlev       !< max lev diff (m)
 double precision                  :: epsmaxlev =1d-8 !< eps lev diff (m)
 double precision                  :: epsmaxlevm=1d-8 !< eps lev diff (m) minus part

 logical                           :: debugon       !< texts  yes or no
 logical                           :: validateon    !< should we validate flow state yes or no (switched off at water drop)
 integer                           :: noddifmaxlev  !< node number of max lev diff ()
 integer                           :: nodneg        !< node nr with negative hs
 integer                           :: kkcflmx       !< 2D Node nr with max courant
 integer                           :: kcflmx        !< 3D Node nr with max courant
 integer                           :: itsol         !< act nr. of iterations in solve
 integer                           :: nochkadv      !< nr of chkadvd checks
 integer                           :: numnodneg     !< nr of posh checks
 integer                           :: nrimptran     !< nr of implicit transport points
 integer                           :: ndmin         !< node nr where min znod is found in viewing area
 integer                           :: ndmax         !< node nr where max znod is found in viewing area
 integer                           :: Lnmin         !< link nr where min zlin is found in viewing area
 integer                           :: Lnmax         !< link nr where max zlin is found in viewing area

 integer, parameter :: MAX_IDX        = 22
 double precision, dimension(MAX_IDX)    :: volcur !< Volume totals in *current* timestep only (only needed for MPI reduction)
 double precision, dimension(MAX_IDX)    :: cumvolcur =0d0 !< Cumulative volume totals starting from the previous His output time, cumulate with volcur (only needed for MPI reduction)
 double precision, dimension(MAX_IDX)    :: voltot
 character(len=100), dimension(MAX_IDX)  :: voltotname
 integer, parameter :: IDX_VOLTOT     = 1
 integer, parameter :: IDX_STOR       = 2
 integer, parameter :: IDX_VOLERR     = 3
 integer, parameter :: IDX_BNDIN      = 4
 integer, parameter :: IDX_BNDOUT     = 5
 integer, parameter :: IDX_BNDTOT     = 6
 integer, parameter :: IDX_EXCHIN     = 7
 integer, parameter :: IDX_EXCHOUT    = 8
 integer, parameter :: IDX_EXCHTOT    = 9
 integer, parameter :: IDX_PRECIP     = 10
 integer, parameter :: IDX_EVAP       = 11
 integer, parameter :: IDX_SOUR       = 12
 integer, parameter :: IDX_InternalTidesDissipation = 13
 integer, parameter :: IDX_GravInput  = 14
 integer, parameter :: IDX_SALInput   = 15
 integer, parameter :: IDX_SALInput2  = 16
 integer, parameter :: IDX_GRWIN      = 17
 integer, parameter :: IDX_GRWOUT     = 18
 integer, parameter :: IDX_GRWTOT     = 19
 integer, parameter :: IDX_LATIN      = 20
 integer, parameter :: IDX_LATOUT     = 21
 integer, parameter :: IDX_LATTOT     = 22


! Delft3D structure of grid dimensions
! type(gd_dimens)     :: dimenstruct               !not used anywhere

contains
!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_flow() instead.
subroutine default_flow()
! 3D parameters
   ! kmx         = 0    ! nr of 3d layers, increasing in positive upward direction
                       ! if kmx==0 then 2D code. if kmx==1 then 3D code
   ! kmx1        = kmx+1 ! kmx + 1, for dimensioning arrays that used be (0:kmax)
   ! kmxd        = 0    ! dim of kmx, >= 1
   ! ndkx        = 0    ! dim of 3d flow nodes (internal + boundary)
   ! ndkx1       = 1    ! dim of 3d flow horizontal interfaces (internal + boundary), (0:kmx)
   ! lnkx        = 0    ! dim of 3d flow links (internal + boundary)

   mxlayz            = 1   ! max nr of z     layers in flow domain
   mxlays            = 1   ! max nr of sigma layers in flow domain
   kplot             = 1   ! layer nr to be plotted
   nplot             = 1   ! vertical profile to be plotted at node nr
   layertype         = 1   !< 1= all sigma, 2 = all z, 3 = left sigma, 4 = left z
   iturbulencemodel  = 3   !< 0=no, 1 = constant, 2 = algebraic, 3 = k-eps, 4 = k-tau
   ieps              = 2   !< bottom boundary type eps. eqation, 1=dpmorg, 2 = dpmsandpit, 3=D3D, 4=Dirichlethdzb
   sigmagrowthfactor = 1d0 !<layer thickness growth factor from bed up

   ! Remaining of variables is handled in reset_flow()
   call reset_flow()
end subroutine default_flow

!> Resets only flow variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, call default_flow() instead.
subroutine reset_flow()
! node related

! basis
    vol0tot     = 0    ! total volume start of timestep          (m3)
    vol1tot     = 0    ! total volume   end of timestep          (m3)
    vol1ini     = -1d0 ! total volume   initially                (m3)
    Volgrw      = 0    ! total grw volume                        (m3)
    Volgrwini   = 0d0  ! total grw volume initially              (m3)


    qinbnd      = 0    ! total inflow boundaries                 (m3/s) Actual values
    qoutbnd     = 0    ! total outflow boundaries                (m3/s)
    qincel      = 0    ! total inflow cells                      (m3/s) Actual values
    qoutcel     = 0    ! total outflow cells                     (m3/s)

    vinbnd      = 0    ! total volume in  boundaries             (m3) Actual values
    voutbnd     = 0    ! total volume out boundaries             (m3)
    vincel      = 0    ! total volume in  cells                  (m3) Actual values
    voutcel     = 0    ! total volume out cells                  (m3)
    volerr      = 0    ! vol1tot - vol0tot - vinbnd + voutbnd - vincel + voutcel   (m3)

    vinbndcum   = 0    ! total inflow boundaries                 (m3) Cumulative values
    voutbndcum  = 0    ! total outflow boundaries                (m3)
    vincelcum   = 0    ! total volume in  cells                  (m3) Actual values
    voutcelcum  = 0    ! total volume out cells                  (m3)
    volerrcum   = 0    !     (m3)

    dvolbot     = 0d0  !     (m3)

 ! extra
    qinrain     = 0    ! total inflow rain                       (m3/s)
    qouteva     = 0    ! total outflow evaporation               (m3/s)
    qinlat      = 0    ! total inflow diffuse laterals           (m3/s)
    qoutlat     = 0    ! total outflow diffuse laterals          (m3/s)
    qingrw      = 0    ! total inflow from groundwater           (m3/s)
    qoutgrw     = 0    ! total outflow to groundwater            (m3/s)
    qinsrc      = 0    ! total inflow local point sources        (m3/s)
    qoutsrc     = 0    ! total outflow local pount sources       (m3/s)
    qinlat      = 0    !
    qoutlat     = 0    !

    vinrain     = 0    ! total volume in  rain                   (m3)
    vouteva     = 0    ! total volume out evaporation            (m3)
    vinlat      = 0    ! total volume in  diffuse laterals       (m3)
    voutlat     = 0    ! total volume out diffuse laterals       (m3)
    vingrw     = 0    ! total volume in  groundwater            (m3)
    voutgrw    = 0    ! total volume out groundwater            (m3)
    vinsrc      = 0    ! total volume in  local point sources    (m3)
    voutsrc     = 0    ! total volume out local pount sources    (m3)

    vinraincum  = 0    ! total inflow rain                       (m3)
    voutevacum  = 0    ! total outflow evaporation               (m3)
    vinlatcum   = 0    ! total inflow diffuse laterals           (m3)
    voutlatcum  = 0    ! total outflow diffuse laterals          (m3)
    vingrwcum   = 0    ! total inflow groundwater                (m3)
    voutgrwcum  = 0    ! total outflow groundwater               (m3)
    vinsrccum   = 0    ! total inflow local point sources        (m3)
    voutsrccum  = 0    ! total outflow local pount sources       (m3)

    DissInternalTides = 0d0   !< total Internal Tides Dissipation (J/s)

    a0tot       = 0    ! total wet surface area start of timestep (m2)
    a1tot       = 0    ! total wet surface area   end of timestep (m2)
    a1ini       = 0    ! total model       area                   (m2)
    ek1tot      = 0    ! volume averaged kin energy (m2/s2) end of timestep
    ep1tot      = 0    ! volume averaged pot energy (m2/s2) end of timestep
    ep1rela     = 0    ! time relaxe ep1tot
    hsaver      = 0    ! average waterdepth (m), vol/are

    epsmaxvol   = 1d-9 ! eps vol diff (m3) ! both not used now
    difmaxlev   = 0    ! max lev diff (m3)
    !epsmaxlev   = 1d-8 ! eps lev diff (m)  ! max waterlevel difference in Newton iterations
    !epsmaxlevm  = 1d-8 ! eps lev diff (m)  ! max waterlevel difference in Newton iterations

    debugon     = .false. ! texts  yes or no
    validateon  = .true.  ! validate flow state yes or no

    nodneg      = 0    ! node nr with negative hs
    kkcflmx     = 0    ! 2D node nr with max courant
    itsol       = 0    ! act nr. of iterations in solve
    nochkadv    = 0    ! nr of chkadvd checks
    numnodneg   = 0    ! nr of posh checks
    nrimptran   = 0    ! nr of implicit transport points
    ndmin       = 0    ! node nr where min znod is found in viewing area
    ndmax       = 0    ! node nr where max znod is found in viewing area
    Lnmin       = 0    ! link nr where min zlin is found in viewing area
    Lnmax       = 0    ! link nr where max zlin is found in viewing area

    sam0tot     = 0d0  !< Total mass start of timestep            (m3ppt)
    sam1tot     = 0d0  !< Total mass   end of timestep            (m3ppt)
    samerr      = 0d0  !< vol1tot - vol0tot - vinbnd + voutbnd - vincel + voutcel   (m3)

    voltot(:)   = 0d0
    voltotname(IDX_VOLTOT ) = 'total_volume'
    voltotname(IDX_STOR   ) = 'storage'
    voltotname(IDX_VOLERR ) = 'volume_error'
    voltotname(IDX_BNDIN  ) = 'boundaries_in'
    voltotname(IDX_BNDOUT ) = 'boundaries_out'
    voltotname(IDX_BNDTOT ) = 'boundaries_total'
    voltotname(IDX_EXCHIN ) = 'exchange_with_1D_in'
    voltotname(IDX_EXCHOUT) = 'exchange_with_1D_out'
    voltotname(IDX_EXCHTOT) = 'exchange_with_1D_total'
    voltotname(IDX_PRECIP ) = 'precipitation'
    voltotname(IDX_EVAP   ) = 'evaporation'
    voltotname(IDX_SOUR   ) = 'source_sink'
    voltotname(IDX_InternalTidesDissipation) = 'InternalTidesDissipation'
    voltotname(IDX_GravInput) = 'Gravitational_Input'
    voltotname(IDX_SalInput) = 'SAL_Input'
    voltotname(IDX_SalInput2) = 'SAL_Input_2'
    voltotname(IDX_GRWIN  ) = 'groundwater_in'
    voltotname(IDX_GRWOUT ) = 'groundwater_out'
    voltotname(IDX_GRWTOT ) = 'groundwater_total'
    voltotname(IDX_LATIN  ) = 'laterals_in'
    voltotname(IDX_LATOUT ) = 'laterals_out'
    voltotname(IDX_LATTOT ) = 'laterals_total'

    jacftrtfac  = 0   !< Whether or not (1/0) a multiplication factor field was specified for trachytopes's returned roughness values.

end subroutine reset_flow

end module m_flow


module m_profiles
 ! profile related :
 type tprof                                          !< this is a profile type
   integer                        :: ityp            !< 1 = circle, 2=rectan1dlumped, 3=rectan2d, 9=yzlumped, 10=yzconveyance
   integer                        :: frctp           !< friction type chezy manning etc
   double precision               :: frccf           !< friction coefficient
   double precision               :: width           !< max width
   double precision               :: height          !< max height
   real, allocatable              :: xx(:), yy(:)    !< y z point coordinates
   real, allocatable              :: y (:), z (:)    !< y and z arrays
   real                           :: zmin            ! absolute z lavel of lowest point
 end type tprof

 integer                          :: nprofdefs       !< nr of unique  profile definitions
 type(tprof), allocatable         :: profiles1D(:)   !< these are the profiles

 integer                          :: nproflocs = 0      !< nr of profile locations, always <= nprofdefs
 integer                          :: maxproflocnr       !< highest referred profnr in profloc.xyz
 integer                          :: minproflocnr       !< lowest  referred profnr in profloc.xyz
 double precision, allocatable    :: xpr(:), ypr(:), zpr(:) !< profile locations, x,y,z
 integer,          allocatable    :: npr(:)                 !< at these locations, reference to profdefs
 integer                          :: jainterpolatezk1D = 1
 double precision                 :: tolzprof = 0.1d0
 integer                          :: ntolsave = 0
end module m_profiles


 !> in m_flowgeom: nd and ln apply to waterlevel nodes and links
 !! in m_netw    : nod and lin apply to 'grid' or 'net' nodes and links
 module m_flowgeom

 use m_profiles
 use grid_dimens_module
 use m_d3ddimens
 use m_flowparameters, only: jawave
 use m_cell_geometry

 implicit none

 ! node (s) related : dim=ndx
 type tnode                                          !< node administration
   integer                         :: lnx            !< max nr of links attached to this node
   integer, allocatable            :: ln (:)         !< linknrs attached to this node, >0: to this flownode, <0: from this flownode

   integer, allocatable            :: nod(:)         !< Mapping to net nodes
   double precision, allocatable   :: x  (:)         !< for now, this is only for quick/aligned plotting, the corners of a cell
   double precision, allocatable   :: y  (:)         !< for now, this is only for quick/aligned plotting, the corners of a cell
 end type tnode

 double precision                  :: bamin          !< minimum 2D cell area
 double precision                  :: bamin1D        !< minimum cell area 1d nodes
 double precision                  :: dxmin=1d-3     !< minimum link length 1D (m)
 double precision                  :: dxmin1D        !< minimum link length 1D (m)
 double precision                  :: dxmin2D        !< minimum link length 2D (m)
 double precision                  :: dxwuimin2D     !< smallest fraction dx/wu , may increase dx if > 0


 double precision                  :: wu1DUNI        !< uniform 1D profile width
 double precision                  :: hh1DUNI        !< uniform 1D profile height

 double precision                  :: wu1DUNI5       !< uniform 1D profile width in  streetinlet kn(3,L) = 5
 double precision                  :: hh1DUNI5       !< uniform 1D profile height in streetinlet

 double precision                  :: wu1DUNI7       !< uniform 1D profile width in  roofgutterpipe kn(3,L) = 7
 double precision                  :: hh1DUNI7       !< uniform 1D profile height in roofgutterpipe

 integer                           :: ja1D2Dinternallinktype = 1

 type (griddimtype)                :: griddim
 type (gd_dimens), target          :: gddimens
 type (gd_dimens), pointer         :: gddimens_ptr


 ! Flow node numbering:
 ! 1:ndx2D, ndx2D+1:ndxi, ndxi+1:ndx1Db, ndx1Db:ndx
 ! ^ 2D int ^ 1D int      ^ 1D bnd       ^ 2D bnd ^ total
 ! the following variables have been moved in m_cell_geometry (module of gridgeom)
 ! integer, target                   :: ndx2d          !< [-] Number of 2D flow cells (= NUMP). {"rank": 0}
 ! integer, target                   :: ndx            !< [-] Number of flow nodes (internal + boundary). {"rank": 0}
 ! double precision, allocatable, target :: ba (:)     !< [m2] bottom area, if < 0 use table in node type {"location": "face", "shape": ["ndx"]}
 ! double precision, allocatable         :: ba0(:)     ! Backup of ba
 ! double precision, allocatable, target :: xz (:)     !< [m/degrees_east] waterlevel point / cell centre, x-coordinate (m) {"location": "face", "shape": ["ndx"]}
 ! double precision, allocatable         :: xz0(:)     !< backup of xz
 ! double precision, allocatable, target :: yz (:)     !< [m/degrees_north] waterlevel point / cell centre, y-coordinate (m) {"location": "face", "shape": ["ndx"]}
 ! double precision, allocatable         :: yz0(:)     !< backup of yz



 integer, target                   :: ndxi           !< [-] Number of internal flowcells  (internal = 2D + 1D ). {"rank": 0}
 integer, target                   :: ndx1db         !< [-] Number of flow nodes incl. 1D bnds (internal 2D+1D + 1D bnd). {"rank": 0}
 type (tnode),     allocatable     :: nd(:)          !< (ndx) flow node administration
 integer,          allocatable     :: kcs(:)         !< node code permanent
 integer,          allocatable     :: kcsini(:)      !< node code during initialization, e.g., for initialwaterlevel1d/2d
 integer,          allocatable, target :: kfs(:)     !< [-] node code flooding {"shape": ["ndx"]}

 double precision, allocatable, target :: bare(:)         !< [m2] bottom area, for rain and evaporaton {"location": "face", "shape": ["ndx"]}
 double precision, allocatable     :: bai(:)         !< inv bottom area (m2), if < 0 use table in node type
 double precision, allocatable, target :: ba_mor (:) !< [m2] morphologically active bottom area, if < 0 use table in node type {"location": "face", "shape": ["ndx"]}
 double precision, allocatable, target :: bai_mor(:) !< [m-2] inv morphologically active bottom area (m2)
 double precision, allocatable, target :: bl(:)      !< [m] bottom level (m) (positive upward) {"location": "face", "shape": ["ndx"]}
 double precision, allocatable     :: aif(:)         !< cell based skewness ai factor sqrt(1+(dz/dy)**2) = abed/asurface
                                                     !< so that cfu=g(Au/conveyance)**2 = g*aif*(Au/convflat)**2
                                                     !< convflat is flat-bottom conveyance
 double precision, allocatable     :: aifu(:)        !< bed skewness at u point (Lnx)
 double precision, allocatable     :: bz(:)          !< averaged bed level at cell center (Ndx)
 double precision, allocatable     :: groundLevel(:) !< For output purposes only: ground level of node (ndxi-ndx2d), only for 1D.
 integer,          allocatable     :: groundStorage(:)     !< For output purposes only: whether or not (1/0) storage on ground occurs (not for closed pipes) (ndxi-ndx2d), only for 1D.
 double precision, allocatable     :: volMaxUnderground(:) !< For output purposes only: maximal volume of node, under ground level (ndxi-ndx2d), only for 1D
 ! link (u) related : dim = lnx
 ! Flow link numbering:
 ! 1:lnx1d, lnx1d+1:lnxi, lnxi+1:lnx1Db, lnx1Db+1:lnx
 ! ^ 1D int ^ 2D int      ^ 1D bnd       ^ 2D bnd ^ total
 integer, target                   :: lnx1D          !< [-] nr of 1D flow links (so first 1D, next 2D, next boundaries). {"rank": 0}
 integer, target                   :: lnxi           !< [-] nr of flow links (internal, 1D+2D    ). {"rank": 0}
 integer, target                   :: lnx1Db         !< [-] nr of flow links including 1D bnds (internal, 1D+2D, boundary: only 1D. 2D bnd behind it). {"rank": 0}
 integer, target                   :: lnx            !< [-] nr of flow links (internal + boundary). First we have 1D links, next 2D links, next boundary links (first 1D, then 2D). {"rank": 0}
 integer,          allocatable, target   :: ln    (:,:)    !< [-] 1D link (2,*) node   administration, 1=nd1,  2=nd2   linker en rechter celnr {"shape": [2, "lnkx"]}
 integer,          allocatable, target   :: LLkkk (:,:)    !< [-]    Link Link admin (5,*) , 1=lowL 2=hihL, 3=leftk, 4= midk, 5=rightk {"shape": [5, "lnx"]}
 integer,          allocatable, target   :: lncn  (:,:)    !< [-] 2D link (2,*) corner administration, 1=nod1, 2=nod2  linker en rechter netnr {"shape": [2, "lnkx"]}
 integer,          allocatable     :: kcu   (:)      !< link code, 1=1D link, 2=2D link, -1= bc 1D, -2=bc 2D, 3=2D parall wall, 4=1D2Dlink, 5=Pump, 7=1d2d internal link
 integer,          allocatable, target :: iadv(:)    !< [-] type of advection for this link {"location": "edge", "shape": ["lnx"]}
 double precision, allocatable     :: teta  (:)      !< link teta (m)
 integer,          allocatable     :: klnup (:,:)    !< link upwind cell pointer if q> 0 use (1:3,L), else (4:6,L)
 double precision, allocatable, target :: dx    (:)      !< [m] link length (m) {"location": "edge", "shape": ["lnx"]}
 double precision, allocatable     :: dxi   (:)      !< inverse dx
 double precision, allocatable, target :: wu(:)      !< [m] link initial width (m), if < 0 pointer to convtab {"location": "edge", "shape": ["lnx"]}
 double precision, allocatable, target :: wu_mor(:)      !< [m] morphologically active width (m), if < 0 pointer to convtab {"location": "edge", "shape": ["lnx"]}
 double precision, allocatable     :: wui   (:)      !< inverse link initial width (m), if < 0 pointer to convtab
 double precision, allocatable, target :: wu1D2D(:)      !< [m] Custom input for 1D2D link widths. {"location": "edge", "shape": ["lnx1D"]}
 double precision, allocatable, target :: hh1D2D(:)      !< [m] Custom input for 1D2D link height. {"location": "edge", "shape": ["lnx1D"]}
 double precision, allocatable     :: prof1D (:,:)   !< dim = (3,lnx1D) 1= 1D prof width, 2=1D profile height, 3=proftyp, or: if 1,2< 0, pointers to prof 1,2, then 3=alfa1
 integer,          allocatable     :: jaduiktmp(:)  !< temparr
 double precision, allocatable, target     :: bob   (:,:)    !< [m] left and right inside lowerside tube (binnenkant onderkant buis) HEIGHT values (m) (positive upward), adjusted for structures {"location": "edge", "shape": [2, "lnx"]}
 double precision, allocatable, target     :: bob0  (:,:)    !< [m] left and right inside lowerside tube (binnenkant onderkant buis) HEIGHT values (m) (positive upward), NOT adjusted for structures {"location": "edge", "shape": [2, "lnx"]}
 integer,          allocatable     :: ibot  (:)      !< local ibedlevtype for setting min or max network depths (temporary, result goes to bobs)

 double precision, allocatable     :: acl   (  :)    !< left dx fraction, alfacl
 double precision, allocatable     :: acn   (:,:)    !< 2,L left and right wu fraction
 double precision, allocatable, target   :: xu    (:)      !< [m] velocity point x {"location": "edge", "shape": ["lnx"]}
 double precision, allocatable, target   :: yu    (:)      !< [m] velocity point y {"location": "edge", "shape": ["lnx"]}
 double precision, allocatable     :: blu   (:)      !< velocity point bottom level positive up (m)
 double precision, allocatable     :: csu   (:)      !< cosine comp of u0, u1
 double precision, allocatable     :: snu   (:)      !< sine   comp of u0, u1
 double precision, allocatable     :: wcl   (:,:)    !< link weights (2,lnx) for center scalar , 1,L for k1, 2,L for k2 Ln
 double precision, allocatable     :: wcLn  (:,:)    !< link weights (2,lnx) for corner scalar , 1,L for k3, 2,L for k4 Lncn
 double precision, allocatable     :: wcx1(:)        !< link weights (lnx) for cartesian comps center vectors k3
 double precision, allocatable     :: wcy1(:)        !< link weights (lnx) for cartesian comps center vectors k3
 double precision, allocatable     :: wcx2(:)        !< link weights (lnx) for cartesian comps center vectors k4
 double precision, allocatable     :: wcy2(:)        !< link weights (lnx) for cartesian comps center vectors k4
 double precision, allocatable     :: wcnx3(:)       !< link weights (lnx) for corner velocities k3
 double precision, allocatable     :: wcny3(:)       !< link weights (lnx) for corner velocities k3
 double precision, allocatable     :: wcnx4(:)       !< link weights (lnx) for corner velocities k4
 double precision, allocatable     :: wcny4(:)       !< link weights (lnx) for corner velocities k4

 double precision, allocatable     :: csb(:,:)       !< cosine orientation from left/right neighboring flownode to flowlink, left/right as ln
 double precision, allocatable     :: snb(:,:)       !< sine   orientation from left/right neighboring flownode to flowlink, left/right as ln

 double precision, allocatable     :: csbn(:,:)      !< cosine orientation from left/right netnode to flowlink, left/right as lncn
 double precision, allocatable     :: snbn(:,:)      !< sine   orientation from left/right netnode to flowlink, left/right as lncn

 double precision, allocatable     :: slnup (:,:)    !< link upwind cell weight, if q> 0 use (1:3,L), else (4:6,L)
 double precision, allocatable     :: csbup (:,:)    !< cosine orientation from upwind cell to flowlink
 double precision, allocatable     :: snbup (:,:)    !< sine   orientation from upwind cell to flowlink

 double precision, allocatable     :: csbw(:,:)      !< cosine orientation from left/right flowlink to wall (netlink), left/right as in walls(10,:) (left), walls(11,:) (right)
 double precision, allocatable     :: snbw(:,:)      !< sine   orientation from left/right flowlink to wall (netlink), left/right as in walls(10,:) (left), walls(11,:) (right)

 double precision, allocatable     :: csbwn(:)       !< cosine orientation from flownode to wall (netlink)
 double precision, allocatable     :: snbwn(:)       !< sine   orientation from flownode to wall (netlink)

 integer,          allocatable     :: ln2lne(:)      !< flowlink to netlink nr dim = lnx
 integer,          allocatable     :: lne2ln(:)      !< netlink to flowlink nr dim = numL

 double precision, allocatable     :: grounlay(:)               !< spatially varying ground layer thickness
 double precision, allocatable     :: argr(:)                   !< spatially varying ground layer area
 double precision, allocatable     :: wigr(:)                   !< spatially varying ground layer top width
 double precision, allocatable     :: pergr(:)                  !< spatially varying ground layer perimeter

 double precision                  :: grounlayuni    = -999d0   !< used if >= 0, default = dmiss
 integer                           :: jagrounlay     = 0        !< use groundlayer 0/1

 ! cell corner related, the links attached to a cell corner
 type tcorn                                          !< corner administration
   integer                         :: lnx            !< max nr of links attached to this corner
   integer, allocatable            :: ln (:)         !< linknrs attached to this corner
   integer                         :: nwx            !< nr of walls attached
   integer, allocatable            :: nw(:)          !< wallnrs attached to this corner

 end type tcorn                                      !< corner administration

 type(tcorn)     , allocatable     :: cn  (:)        !< cell cornerpoints, (in counting order of nod)
 double precision, allocatable     :: ucnx(:)        !< cell corner velocity, global x-dir (m/s)
 double precision, allocatable     :: ucny(:)        !< cell corner velocity, global y-dir (m/s) (in m_flowgeom...)
 double precision, allocatable, target :: vort(:)        !< [s-1] vorticity at netnodes {"shape": ["ndx"], "comment": "Currently not available, is nowhere allocated nor filled."}


 ! fixed wall related, may be expanded to closed internal walls later for now, dim=(7,*)
integer                            :: mxwalls            !< max nr of walls
double precision, allocatable      :: walls(:,:)     !< 1,* : inside waterlevel point (node)
                                                     !! 2,* : first  cornerpoint
                                                     !! 3,* : second cornerpoint
                                                     !! 4,* : flow link 1 attached to first  cornerpoint
                                                     !! 5,* : flow link 2 attached to second cornerpoint
                                                     !! 6,* : stress contribution to link 1
                                                     !! 7,* : stress contribution to link 1
integer                            :: nwcnx          !< max nr of cornerpoints to which walls are attached
integer,          allocatable      :: nwalcnw(:,:)   !< pointer to those walls, 1 = left wall, 2 =right wall

! closed wall corner (netnode) related
integer                            :: nrcnw          !< nr of cn points attached to 2 closed walls
integer         , allocatable      ::  kcnw (:)      !< closed corner point nr k, reference to net nodes
real            , allocatable      :: cscnw (:)      !< closed corner alignment cos (1:nrcnw)
real            , allocatable      :: sncnw (:)      !< closed corner alignment sin (1:nrcnw)
real            , allocatable      :: sfcnw (:)      !< closed corner partial slip sf = u*/u  (based on walls average)

! thin dam related
integer                            :: nthd
double precision, allocatable      :: thindam(:,:)

 ! branch related :
 type tbranch                                        !< this is a branch type
   integer                         :: nx             !< with nx links and nx + 1 nodes in it
   integer, allocatable            :: ln (:)         !< successive flow linknrs
 end type tbranch

 integer                           :: mxflowbr       !< max nr of flow branches
 type(tbranch), allocatable        :: flowbr(:)      !< this is a list of flow branches


 integer, allocatable              :: Lbnd1D(:)      !< for prof1D, boundary links refer to regular attached 1D links

 ! 1D endnode related
 integer                           :: mx1Dend        !< nr of 1D endnodes
 integer,          allocatable     :: n1Dend(:)      !< node nrs of 1D endnodes


! netnode/flownode  related, dim = mxban
 double precision, allocatable     :: banf  (:)     !< horizontal netnode/flownode area (m2)
 double precision, allocatable     :: ban  (:)      !< horizontal netnode          area (m2)
 integer         , allocatable     :: nban  (:,:)   !< base area pointers to banf, 1,* = netnode number, 2,* = flow node number, 3,* = link number
 integer                           :: mxban         !< max dim of ban

 ! 1D2D link properties
 ! useful parameters :
 double precision                  :: rrtol            !< relative cellsize factor in search tolerance ()
 double precision, allocatable     :: xyen(:,:)        !< temp boundary opposite point (end of EdgeNormal) (replaces ebtol tolerance)
 integer                           :: jarenumber       !< renumberFlowNodes
 integer                           :: jaFlowNetChanged !< To enforce various net(link)-related init routines after renumbering
 integer                           :: jaAllowBndAtBifurcation !< allow 1d boundary at endnode when connecting branch leads to bifurcation

! JRE Stuff related to setting up wave directional grid
 integer                                     :: ntheta          !< Number of wave direction bins
 double precision                            :: thetamax        !< upper limit wave directional sector
 double precision                            :: thetamin        !< lower limit wave directional sector
 integer                                     :: thetanaut       !< nautical convention or not
 double precision                            :: dtheta          !< directional resolution
 double precision                            :: theta0          !< mean theta-grid direction
 double precision, allocatable               :: thetabin(:)     !< bin-means of theta-grid

 ! Villemonte calibration coefficients :
 double precision                            :: VillemonteCD1 = 1.0d0      !< default for VillemonteCD1 = 1
 double precision                            :: VillemonteCD2 = 10.0d0     !< default for VillemonteCD2 = 10

! Debug parameter
 integer                                     :: jabatch  = 0       !< dobatch
 integer                                     :: cmd_icgsolver = 4  !< save commandline icgsolver


contains
!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, call reset_flowgeom() instead.
subroutine default_flowgeom()
    bamin    = 1d-6     ! 1d0    ! minimum 2D cell area
    bamin1D  = 0d-2     ! minimum cell area 1d nodes
    dxmin1D  = 1D-3     ! minimum link length 1D (m)
    dxmin2D  = 1D-3     ! minimum link length 2D (m)
    dxwuimin2D = 0.0d0  ! smallest fraction dx/wu , may increase dx if > 0

    wu1DUNI  =  2d0   ! Uniform 1D profile width
    hh1DUNI  =  3d0   ! Uniform 1D profile height

    wu1DUNI5 = 0.2d0  !< uniform 1D profile width in drain or street inlet
    hh1DUNI5 = 0.1d0  !< uniform 1D profile height in drain or street inlet

    wu1DUNI7 = 0.1d0  !< uniform 1D profile width roofgutterpipe
    hh1DUNI7 = 0.1d0  !< uniform 1D profile height roofgutterpipe


    ! useful parameters :
    rrtol      = 3d0 ! relative cellsize factor in search tolerance ()
    jaAllowBndAtBifurcation = 0


    jarenumber = 1
    ! Remaining of variables is handled in reset_flowgeom()
    call reset_flowgeom()
end subroutine default_flowgeom


!> Resets only flow geometry variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, use default_flowgeom() instead.
subroutine reset_flowgeom()
! node (s) related : dim=ndx
    ndx2D   = 0      ! nr of 2d FLOW CELLS = NUMP
    ndxi    = 0      ! max nr of internal flowcells  (internal = 2D + 1D )
    ndx1Db  = 0      ! nr of flow nodes incl. 1D bnds (internal 2D+1D + 1D bnd)
    ndx     = 0      ! nr of flow nodes (internal + boundary)

! link (u) related : dim = lnx
    lnx1D   = 0      ! nr of 1D flow links
    lnxi    = 0      ! nr of flow links (internal           )
    lnx1Db  = 0      ! nr of flow links incl. 1D bnds (internal 1D+2D + 1D bnd)
    lnx     = 0      ! nr of flow links (internal + boundary)

! useful parameters :
    jaFlowNetChanged = 1 ! To enforce various net(link)-related init routines after renumbering

    if (jawave .eq. 4) then  ! reinitialize wave directional grid
       ntheta = 0
    end if
end subroutine reset_flowgeom
 end module m_flowgeom


 !> this module contains the real flow times, only to be managed by setting times in module m_usertimes
 module m_flowtimes
 implicit none

 character (len=8)                 :: refdat      !< Reference date (e.g., '20090101'). All times (tstart_user, tend_user, etc.) are w.r.t. to this date.
 integer                           :: julrefdat   !< will be set by calling settimespacerefdat
 double precision                  :: refdate_mjd !< Reference date as modified Julian date
 integer                           :: irefdate    !< Reference date (e.g., 20090101)
 double precision                  :: Tzone       !< Data Sources in GMT are interrogated with time in minutes since refdat-Tzone*60
 integer, parameter                :: tunit = 1   !< Times to EC-module are in seconds
 double precision                  :: Timjan      !< time in hours of refdat relative to Januari 1 of the same year
 double precision                  :: dt_user     !< User specified time step (s) for external forcing update.
 double precision                  :: dt_nodal    !< User specified time step (s) for nodal factors update.
 double precision                  :: dt_max      !< Computational timestep limit by user.
 double precision                  :: dt_init     !< dt of first timestep, if not specified, use dt_max, if that also not specified, use 1 s

 integer                           :: ja_timestep_auto      !< Use CFL-based dt (with dt_max as upper bound)
 integer                           :: ja_timestep_auto_visc !< Use explicit time step restriction based on viscosity term
 integer                           :: ja_timestep_nostruct  !< Exclude (structure) links without advection from the time step limitation
 double precision                  :: tstart_user !< User specified time start (s) w.r.t. refdat
 double precision                  :: tstop_user  !< User specified time stop  (s) w.r.t. refdat
 double precision                  :: time_user   !< Next time of external forcings update (steps increment by dt_user).

 double precision                  :: dts         !< internal computational timestep (s)
 double precision                  :: dtsc        !< max timstep of limiting point kkcflmx, zero if larger than dt_max
 double precision                  :: dtfacmax    !< max dts increase factor
 double precision                  :: dti         !< clinverse  computational timestep (1/s)
 double precision                  :: dtprev      !< previous computational timestep (s)  (1s is a bit like sobek)
 double precision                  :: dtmin       !< dt < dtmin : surely crashed
 double precision                  :: dtminbreak  !< smallest allowed timestep (in s), checked on a sliding average of several timesteps in validation routine.
 double precision                  :: dtminhis    !< smallest timestep within most recent his interval
 double precision                  :: tfac        !< time unit in seconds
 double precision, allocatable     :: tvalswindow(:) !< (NUMDTWINDOWSIZE) Time1 values in a moving time window to compute sliding average dt
 integer         , parameter       :: NUMDTWINDOWSIZE = 100 !< Number of time steps to include in the sliding average, don't set this too optimistic to avoid too fast simulation breaks.
 integer                           :: idtwindow_start !< Current start index in tvalswindow(:) array. This array is filled in a cyclic order, with never more than NUMDTWINDOWSIZE time values.
 double precision                  :: time0       !< current   julian (s) of s0
 double precision                  :: time1       !< current   julian (s) of s1  ! and of course, time1 = time0 + dt
 double precision                  :: tim1bnd     !< last time boundary signals were given
 double precision                  :: tim1fld     !< last time field    signals were given
 integer                           :: jatimestepanalysis = 0
 double precision, allocatable     :: dtcell(:)   !< time step per cell based on CFL (s), size:ndkx
 double precision, allocatable     :: time_wetground(:) !< Cumulative time when water is above ground level, size: ndxi (now only for 1d, later also for 2d)

 !TODO: use in the trachytopes this variable and fully remove reading from rdtrt
 double precision                  :: dt_trach    !< DtTrt Trachytope roughness update time interval (s)

 double precision                  :: dnt_user    !< counter for nr of user steps    ( )
 double precision                  :: dnt         !< number of timesteps ( )

 double precision                  :: fhr         !< Factor sec hrs
 double precision                  :: fday        !< Factor sec day

 double precision                  :: ti_map      !< map interval (s)
 double precision                  :: ti_maps     !< Start of map output period (as assigned in mdu-file) (s)
 double precision                  :: ti_mape     !< End   of map output period (as assigned in mdu-file) (s)
 double precision                  :: ti_his      !< history interval (s)
 double precision                  :: ti_hiss     !< Start of his output period (as assigned in mdu-file) (s)
 double precision                  :: ti_hise     !< End   of his output period (as assigned in mdu-file) (s)
 double precision                  :: ti_wav      !< averaging interval spatial wave quantities (s)
 double precision                  :: ti_wavs     !< averaging interval spatial wave quantities
 double precision                  :: ti_wave     !< averaging interval spatial wave quantities
 double precision                  :: ti_sed      !< averaging interval sedmor quantities (s)
 double precision                  :: ti_seds     !< averaging interval sedmor wave quantities
 double precision                  :: ti_sede     !< averaging interval sedmor wave quantities
 double precision                  :: ti_xls      !< history interval (s) xls
 double precision                  :: ti_rst      !< restart interval (s)
 double precision                  :: ti_rsts     !< Start of restart output period (as assigned in mdu-file) (s)
 double precision                  :: ti_rste     !< End   of restart output period (as assigned in mdu-file) (s)
 double precision                  :: ti_waq      !< Interval between output in delwaq files (s).
 double precision                  :: ti_waqs     !< Start of WAQ output period
 double precision                  :: ti_waqe     !< End   of WAQ output period
 logical                           :: wrwaqon = .false. !< Waq output was initialised
 double precision                  :: ti_waqproc  !< Time step for water quality processes
 double precision                  :: ti_waqbal   !< Time step for water quality mass balance output

 double precision                  :: ti_classmap        !< class map interval (s)
 double precision                  :: ti_classmaps       !< Start of class map output period (as assigned in mdu-file) (s)
 double precision                  :: ti_classmape       !< End   of class map output period (as assigned in mdu-file) (s)
 double precision, allocatable, target :: map_classes_s1(:)  !< classes for water level
 double precision, allocatable, target :: map_classes_hs(:)  !< classes for water depth
 double precision, allocatable, target :: map_classes_ucmag(:)  !< classes for the magnitude of the velocity
 double precision, allocatable, target :: map_classes_ucdir(:)  !< classes for the direction of the velocity
 double precision                  :: map_classes_ucdirstep     !< step size of classes for the direction of the velocity
 double precision                  :: ti_stat     !< Interval between simulation statistics output (s).
 double precision                  :: ti_timings  !< (parallel) timings output interval
 double precision                  :: ti_split    !< Time interval for time splitting: time after which new his/map file will be created (e.g. montly), see also the unit below.
                                                  !! Default is 0 to have no time-splitting of output files.
 character(len=1)                  :: ti_split_unit !< Unit for time splitting interval: Y: years, M: months, D: days, h:hours, m: minutes, s: seconds.
 double precision, allocatable     :: ti_mpt(:)      !< times for writing map-files (s), possibly non-equidistant in time
 double precision, allocatable     :: ti_mpt_rel(:)  !< times for writing map-files (s) relative to current time, possibly non-equidistant in time

 double precision                  :: tmini         !< Initial time for updating map/his/rst

 double precision                  :: time_choice   !< Time consisting the next time_user / time_map
 double precision                  :: time_out      !< Next time for output in the most general sense (map, his, etc.)
 double precision                  :: time_map      !< Map output interval
 double precision                  :: time_wav      !< Time-avg'd output interval xb JRE
 double precision                  :: time_sed      !< Time-avg'd output interval sedmor
 double precision                  :: time_his      !< Next time for his output
 double precision                  :: time_xls      !< Next time for his output
 double precision                  :: time_rst      !< Next time for restart output
 double precision                  :: time_classmap !< Next time for class map output
 double precision                  :: time_waq      !< Next time for delwaq output
 double precision                  :: time_waqset   !< Next time to reset the quantitis for waq
 double precision                  :: time_waqproc  !< Next time to calcualate waq processes
 double precision                  :: time_mba      !< Next time to process mass balances
 double precision                  :: time_stat     !< Next time for simulation statistics output
 double precision                  :: time_timings  !< Next time for timings output
 double precision                  :: time_split    !< Next time for a new time-split output file.
 double precision                  :: time_split0   !< Start time for the current time-split output file.
 double precision                  :: time_fetch    !< next time fetchlength will be established
 double precision                  :: tifetch = 0   !< fetchlength comp. interval


 integer                           :: it_map      !< Nr of snapshots presently in map file
 integer                           :: it_wav      !< Nr of snapshots presently in time-avg'd wave output file JRE
 integer                           :: it_sed      !< Nr of snapshots presently in time-avg'd sedmor output file JRE
 integer                           :: it_map_tec  !< Nr of snapshots presently in map file, Tecplot format
 integer                           :: it_his      !< Nr of snapshots presently in his file
 integer                           :: it_inc      !< Nr of lines     presently in inc file
 integer                           :: it_rst      !< Nr of snapshots presently in rst file
 integer                           :: it_waq      !< Nr of snapshots presently in delwaq files.
 integer                           :: it_stat     !< Nr of simulation statistics presently in log file.
 ! for performance timings
 logical                           :: debugtimeon !< timing yes or no
 double precision                  :: cpusteps(3) !< cputime timesteps (1)=start,(2)=stop,(3)=total
 double precision                  :: cpuumod (3) !< cputime set-umod  (1)=start,(2)=stop,(3)=total
 double precision                  :: cpusol  (3) !< cputime conj-grad (1)=start,(2)=stop,(3)=total
 double precision                  :: cpufuru (3) !< cputime conj-grad (1)=start,(2)=stop,(3)=total
 double precision                  :: cpuall  (3) !< cputime steps + plots 1,2,3 idem
 double precision                  :: cpuinistep(3) !< cputime inistep 1,2,3 idem
 double precision                  :: cpuiniext (3) !< cputime init externalforcings (1)=start,(2)=stop,(3)=total
 double precision                  :: cpuext    (3) !< cputime externalforcings      (1)=start,(2)=stop,(3)=total
 double precision                  :: cpuextbnd (3) !< cputime externalforcingsonbnd (1)=start,(2)=stop,(3)=total
 double precision                  :: cpu_extra(3,45) !< cputime, extra timers (1)=start,(2)=stop,(3)=total
 double precision                  :: cpuwri      !< cputime writing (s)
 double precision                  :: dsetb       !< number of setbacks ()
 double precision                  :: walltime0   !< wall time at start of timeloop (s)

 character(len=20)                 :: rundat0     !< start and end date (wallclock) of computer run
 character(len=20)                 :: rundat2     !< start and end date (wallclock) of computer run format = _yymmddhhmmss
 character(len=20)                 :: restartdatetime = ' '!< desired time to be taken from restart map files
 character(len=14)                 :: Startdatetime   = ' '!< optional replacement of Tstart_user
 character(len=14)                 :: Stopdatetime    = ' '!< optional replacement of Tstop_user
 integer                           :: jarestart   !< use restart yes/no, 1/0

 double precision                  :: tlfsmo = 0d0  !< fourier bnd smoothing times
 double precision                  :: alfsmo = 1d0  !< fourier bnd smoothing weight factor
 integer                           :: keepstbndonoutflow = 0 !< keep them on outflow = 1

 double precision                  :: Tspinupturblogprof = 0d0 !< From Tstart to Tstart+Tspinupturblogprof, Turbulent profiles based on log profiles
                                                              !< 0d0 = No
contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, call reset_flowtimes() instead.
subroutine default_flowtimes()
    refdat      = '20010101'        !< Reference date (e.g., '20090101'). All times (tstart_user, tend_user, etc.) are w.r.t. to this date.
    Tzone       = 0d0
    dt_user     = 120d0             !< User specified time step (s) for external forcing update.
    dt_nodal    = 21600d0           !< User specified time step (s) for nodal factors update.
    dt_max      = 30d0              !< Computational timestep limit by user.
    dtmin       = 1d-4              !< dt < dtmin : surely crashed
    dtminbreak  = 0d0               !< smallest allowed timestep, otherwise break: off
    dtminhis    = 9d9               !< smallest timestep within most recent his interval
    dt_init     = 1d0
    dt_trach    = 1200d0            !< User specified DtTrt Trachytope roughness update time interval (s)
    dtfacmax    = 1.1d0             !< default setting
    ja_timestep_auto = 1            !< Use CFL-based dt (with dt_max as upper bound)
    ja_timestep_auto_visc = 0       !< Use explicit time step restriction based on viscosity term
    ja_timestep_nostruct = 0        !< Exclude (structure) links without advection from the time step limitation
    tstart_user = 0d0               !< User specified time start (s) w.r.t. refdat
    tstop_user  = 100*24*3600       !< User specified time stop  (s) w.r.t. refdat
    time_user   = tstart_user       !< Next time of external forcings update (steps increment by dt_user).

    dnt_user    = 0                 !< counter for nr of user steps    ( )
    dnt         = 0                 !< number of timesteps ( )

    fhr         = 1d0/3600d0        !< Factor sec hrs
    fday        = 1d0/(3600d0*24d0) !< Factor sec day

    ti_map      = 1200d0            !< map interval (s)
    ti_wav      = 1200d0            !< wave avg'ing interval (s), 20 minutes okay default  JRE
    ti_wavs     = 0d0
    ti_wave     = 0d0
    ti_maps     = 0d0               !< start interval (s)
    ti_mape     = 0d0               !< end   interval (s)
    ti_his      = 120d0             !< history interval (s)
    ti_seds     = 0d0
    ti_sede     = 0d0
    ti_xls      = 0d0               !< history interval (s) xls
    ti_rst      = 24d0*3600d0       !< restart interval (s)
    ti_waq      = 0d0               !< delwaq interval (s) (Default: off)
    ti_stat     = -60d0             !< simulation statistics interval (s) (Default: off, will later default to dt_user), <0: use wc-time
    ti_timings  = 0d0               !< timings output interval
    ti_split    = 0d0               !< Time interval for time splitting of output files.
    ti_split_unit= 's'              !< Unit for time partitioning interval

    ti_classmap           = -999d0  !< default no class map
    map_classes_ucdirstep = -999d0  !< default no step size given for classes of flow direction
    if (allocated(map_classes_ucdir)) deallocate(map_classes_ucdir)

    tmini       = -1d9              !< initial time for updating the 4 above

    ! Remaining of variables is handled in reset_flowtimes()
    call reset_flowtimes()
end subroutine default_flowtimes


!> Resets only flow times variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, use default_flowtimes() instead.
subroutine reset_flowtimes()
    dtprev       = dt_init           !< previous computational timestep (s)  (1s is a bit like sobek)
    dts          = dt_init           !< internal computational timestep (s)
    !tfac         = 1d0               !< Time unit in seconds JRE: disabled, and handled in readMDU
    time0        = 0d0               !< current   julian (s) of s0
    time1        = 0d0               !< current   julian (s) of s1  ! and of course, time1 = time0 + dt
    tim1bnd      = -9d9              !< last time bnd signals were given
    tim1fld      = -9d9              !< last time bnd signals were given

    time_map     = tstart_user       !< next time for map output
    time_wav     = tstart_user       !< same, wav
    time_sed     = tstart_user       !< same, morstats
    time_his     = tstart_user       !< next time for his output
    time_xls     = tstart_user       !< next time for his output
    time_rst     = tstart_user       !< next time for restart output
    time_classmap= tstart_user       !< next time for class map output
    time_fetch   = tstart_user       !< next time for fetch establ.

    time_waq     = ti_waqs           !< next time for waq output, starting at the output start time
    time_waqset  = tstart_user       !< next time for reset the quantities for waq output
    time_waqproc = tstart_user+ti_waqproc !< next time for wq processes
    time_mba = tstart_user+ti_waqbal !< next time for balance update
    if ( ti_stat.gt.0d0 ) then
       time_stat    = tstart_user    !< next model time for simulation statistics output
    else
       time_stat    = 0d0            !< next wall-clock time for simulation statistics output
    end if
    time_timings = tstart_user       !< next time for timing output
    time_split   = tstart_user       !< next time for a new time-split output file
    time_split0  = time_split        !< Start time for the current time-split output file.
    if (dtminbreak > 0d0) then
       if (.not. allocated(tvalswindow)) then
          allocate(tvalswindow(NUMDTWINDOWSIZE))
       end if
       idtwindow_start = 1 ! Start fresh, with first time0 on pos #1.
       tvalswindow(idtwindow_start) = tstart_user
    end if

    it_map       = 0                 !< Nr of snapshots presently in map file
    it_wav       = 0                 !< Nr of snapshots presently in time-avg'd file JRE
    it_sed       = 0                 !< Nr of snapshots presently in time-avg'd sed file JRE
    it_map_tec   = 0                 !< Nr of snapshots presently in map file
    it_his       = 0                 !< Nr of snapshots presently in his file
    it_inc       = 0                 !< Nr of lines     presently in inc file
    it_rst       = 0                 !< Nr of snapshots presently in rst file
    it_waq       = 0                 !< Nr of snapshots presently in waq couple files
    it_stat      = 0                 !< Nr of simulation statistics presently in log file.

! for performance timings
    debugtimeon   = .false.          !< timing yes or no
    cpusteps  (3) = 0                !< cputime timesteps (1)=start,(2)=stop,(3)=total
    cpufuru   (3) = 0                !< cputime furu      (1)=start,(2)=stop,(3)=total
    cpusol    (3) = 0                !< cputime conj-grad (1)=start,(2)=stop,(3)=total
    cpuall    (3) = 0                !< cputime steps + plots 1,2,3 idem
    cpuinistep(3) = 0                !< cputime steps + plots 1,2,3 idem
    cpuiniext (3) = 0                !< init external forcing
    cpuext    (3) = 0                !< external forcing
    cpuextbnd (3) = 0                !< external forcing bnd
    cpuwri        = 0                !< cputime writing (s)
    dsetb         = 0                !< number of setbacks ()
    alfsmo        = 1d0              !<
end subroutine reset_flowtimes

end module m_flowtimes



 module m_jacobi                                        ! arrays needed for solving jacobi
 implicit none
 integer                           :: ndxjac   = 0      ! nr. of nodes already allocated for jacobi should be ndx
 integer                           :: lnxjac   = 0      ! nr. of links already allocated for jacobi should be lnx
 integer                           :: itmxjac  = 6666   ! max nr. of iterations in solve-jacobi
 double precision                  :: epsjac   = 1d-13  ! epsilon waterlevels jacobi method (maximum)
 double precision, allocatable     :: rr    (:)         ! resid
 double precision, allocatable     :: db    (:)         ! solving related, right-hand side
 double precision, allocatable     :: bbi   (:)         ! solving related, diagonal
 end module m_jacobi


! unstruc.f90

!---------------------------------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------------------------------

! solve_guus.f90
 module m_reduce

 implicit none
    !
    !
    ! Derived Type definitions
    !
 type list
    integer l
    integer, allocatable :: j(:)
 end type list
 type listb
    integer l
    integer, allocatable :: j(:)
    logical, allocatable :: b(:)
 end type listb
 type listc
    integer l
    integer, allocatable :: j(:)
    integer, allocatable :: a(:)
 end type listc
 type listd
    integer l
    integer, allocatable :: j(:)
    integer, allocatable :: a(:)
    integer, allocatable :: f(:)
 end type listd
 !
 ! Local variables
 !
 integer                       :: maxdge   = 6 ! 500
 integer                       :: noactive = 0
 integer                       :: nodtot   = 0 ! (nodtot=ndx)
 integer                       :: lintot   = 0 ! (lintot=lnx)
 integer                       :: nocg     = 0
 integer                       :: nocg0    = 0
 integer                       :: nogauss  = 0
 integer                       :: nogauss0 = 0
 integer                       :: noexpl   = 0
 integer                       :: nowet    = 0
 integer                       :: ijstck   = 0
 integer                       :: npmax    = 0  ! ccc arrays size in gauss_elimination
 integer, allocatable          :: noel(:)
 integer, allocatable          :: noel0(:)
 integer, allocatable          :: nbrstk(:)
 integer, allocatable          :: nodstk(:)
 integer, allocatable          :: nodbr2(:)

 integer, allocatable          :: lv2(:) ! old linev(2,L), linev(5,L) and (6,L) are now ln(1,L) and ln(2,L)

 integer, allocatable          :: jagauss(:)


 type (listb), allocatable     :: ij(:)
 type (listc), allocatable     :: ia(:)
 type (listd), allocatable     :: row(:)

 double precision              :: epscg   = 1d-14    ! epsilon waterlevels cg method (maximum)
 double precision              :: epsdiff = 1d-3     ! tolerance in (outer) Schwarz iterations (for Schwarz solver)
 integer                       :: maxmatvecs  = 100000 ! maximum number of matrix-vector multiplications in Saad solver

 double precision, allocatable :: bbr(:), bbl(:)     ! not left !
 double precision, allocatable :: ccr(:), ccrsav(:)
 double precision, allocatable :: ddr(:)

 double precision, allocatable :: d0 (:)
 double precision, allocatable :: zkr(:)
 double precision, allocatable :: pk (:)
 double precision, allocatable :: apk(:)
 double precision, allocatable :: rk (:)

 double precision, allocatable :: ccc(:)  !< work array in gauss_elimination

 integer, allocatable          :: L1row(:), L2row(:), iarow(:) , jrow(:), ifrow(:), ift(:)  ! for jipjan

 integer                       :: nbr
 integer                       :: nodl
 integer                       :: nodr
 integer                       :: ndn
 integer                       :: mindgr
 integer                       :: nocgiter

 double precision, allocatable, dimension(:) :: s1_ghost ! for testsolver

 end module m_reduce
! solve_guus.f90

!---------------------------------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------------------------------

! rest.f90

 MODULE M_BITMAP
   implicit none
   INTEGER, ALLOCATABLE, SAVE  :: IPIX(:)
   INTEGER, SAVE               :: MXP, NXP
   double precision,    SAVE   :: XB(4), YB(4), XP(4), YP(4)
 END MODULE M_BITMAP

 MODULE M_ARCINFO
   implicit none
   double precision, ALLOCATABLE :: D(:,:)
   INTEGER                       :: MCa = 0, NCa
   double precision              :: X0=0, Y0=0, DXa=1d0, DYa=1d0, RMIS=-999d0
 END MODULE M_ARCINFO


!---------------------------------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------------------------------

! net.f90

Module m_oldz
  implicit none
  double precision              :: OZ = 999
end module m_oldz

module m_makenet
  implicit none
  integer                       :: NTYP = 0, NRX = 3, NRY = 3
  double precision              :: ANGLE = 0, SIZE = 50, THICK = 8, HSIZE = 50
  double precision              :: X0 = 0, Y0 = 0, Z0 = 0, DX0 = 10, DY0 = 10
end module m_makenet

module m_mergenet
  implicit none
  integer                       :: NUMM = 2, JXYZ = 1
end module m_mergenet

module m_seastate
  implicit none
  double precision              :: TWOPI, WAVLEN, WAVKX, WAVOM
  double precision              :: WAVCEL = 5 , WAVPER = 15, WAVAMP = .1
end module m_seastate

module m_boat
  implicit none
  double precision, allocatable :: XBOAT (:), YBOAT(:), ZBOAT(:)  ! INLEZEN &
  double precision, allocatable :: XBOOT (:), YBOOT(:), ZBOOT(:)  ! AFBEELDEN LIJNEN
  integer, allocatable          :: NCBOAT(:)                      !
  integer                       :: MXBOAT, MAXBOAT, NCLBOAT
  double precision              :: BLEN = 25, BHEIGHT = 4, BWIDTH = 5, BHPMAX = 1000, BHPPERC = 0.5d0
  integer                       :: KKB(10) = 0
end module m_boat


MODULE M_ISOSCALEUNIT
  implicit none
  CHARACTER (LEN=12)            :: UNIT(2)     = ' '
  CHARACTER (LEN=20)            :: PARAMTEX(2) = ' '
END MODULE M_ISOSCALEUNIT

MODULE M_RAAITEK
  implicit none
  double precision              :: ZMINrai=-999, ZMAXrai=-999
  INTEGER                       :: JTEXTFLOW = 1
  real                          :: xs1m,ys1m,xs2m,ys2m
  double precision              :: xw1m,yw1m,xw2m,yw2m ! screen and world corners in module
END MODULE M_RAAITEK

module m_grid
  implicit none
  integer                       :: MET  = 1  ! drawing method
  integer                       :: mc   = 0  , nc   = 0  ! actuele dimensies
  integer                       :: MMAX = 0  , NMAX = 0  , MNMAX = 0 ! array   dimensies
  integer                       :: mch  = 0  , nch  = 0  ! actuele dimensies van hulprooster
  double precision, allocatable :: xc (:,:), yc (:,:), zc (:,:)
  double precision, allocatable :: xch(:,:), ych(:,:), zch(:,:)  ! save/restore
  integer, allocatable          :: ijc(:,:), ijyes(:,:)
end module m_grid

!> Regular grid generation settings. All orthogonalisation settings are in
!! module m_orthosettings.
MODULE M_GRIDSETTINGS
implicit none

integer :: MFAC  = 2000 !< M-refinement factor for regular grid generation.
integer :: NFAC  = 40 !< N-refinement factor for regular grid generation.
integer :: ITSMO = 10 !< Nr. of inner iterations in regular grid smoothing.
integer :: ITSMA      !< Not in use, old rgfgrid
integer :: JADEPDESIGN = 0
integer :: MDESIGN
double precision :: BFAC=1d0, CSMO = 0.5d0, RFAC
double precision :: SRM,SRN,DEPSLO,FSMA, ALINEN, ALINEM
INTEGER :: KEEPSTARTDIR = 1
double precision :: BAAS2 = 0.5d0, FACMIR = 1.2d0
double precision :: SPLFAC, SPLFAC2
INTEGER :: JDEMO = 0

! Pillar grid settings
double precision :: pil_rad  = 0d0  !< pillar radius
double precision :: pil_x    = 0d0  !< pillar center point x-coordinate
double precision :: pil_y    = 0d0  !< pillar center point y-coordinate
double precision :: pil_grow = 1d0   !< pillar grid growth factor *not used*

END MODULE M_GRIDSETTINGS


!> Orthogonalisation settings, both for regular grids and unstructured nets.
module m_orthosettings
implicit none
integer          :: ITATP = 2   !< Nr. of outer    iterations in grid/net orthogonalisation.
integer          :: ITBND = 25  !< Nr. of boundary iterations in grid/net orthogonalisation. (within ITATP)
integer          :: ITIN  = 25  !< Nr. of inner    iterations in grid/net orthogonalisation. (within ITBND)
                                !! Also used within structured grid orthogonalisation.
integer          :: JAPROJECT = 1   !< Project nodes back to boundary (2: yes, all, 1:yes, net bounds only, 0:no)
double precision :: ATPF = 0.975d0  !< Factor (0.<=ATPF<=1.) between grid smoothing and grid ortho resp.
double precision :: ATPF_B = 1d0 !< minimum ATPF on the boundary
double precision :: circumormasscenter = 1d0          !< 1.0 = circumcentre,      0.0 = masscentre, 1.0 -> 0.0 : weighted
double precision :: smoothorarea    = 1d0   !< Factor between smoother (1d0) and area-homogenizer (0d0)
integer          :: adapt_method    = 1     !< Mesh-adaptation method; 0: Winslow, 1: arc-length, 2: harmonic map
double precision :: adapt_beta      = 0.0d0 !< Mesh-refinement factor; between 0d0 and 1d0
integer          :: adapt_niter_u   = 0     !< number of smoothing iterations of `solution` u in adaptation
integer          :: adapt_niter_G   = 4     !< number of smoothing iterations of monitor matrix G in adaptation
double precision :: ortho_pure      = 0.5d0   !< curvi-linear-like (0d0) or pure (1d0) orthogonalisation

end module m_orthosettings


MODULE M_XYTEXTS
implicit none
integer, parameter :: maxtxt = 2000
double precision   :: xtxt(maxtxt), ytxt(maxtxt),heitxt(maxtxt)
integer            :: symtxt(maxtxt), coltxt(maxtxt)
integer            :: ntxt
character(len=120) :: xytexts(maxtxt)
   END MODULE M_XYTEXTS


module m_roofs
 double precision  :: roofheightuni        = 2.7d0                  ! if not dmiss, rooflevel = av gr
 double precision  :: roofedgeheight       = 0.1d0
 double precision  :: dxminroofgutterpipe  = 10.0d0
end module m_roofs

module m_equatorial
 double precision :: x,fr,Ue0,k,h,g,L,utyp,period,ap,am,om,Zp,ufac
 integer          :: nmode=0, nfreq=1
 integer          :: ndxfreeL, ndxforced, ndtfreeL, ndtforced
 double precision :: app = 0.0d0 , amm = 0d0, ztyp = 0.20d0
 double precision :: ampliforced, amplitotal, amplifreeL, cflfreeL, cfLforced, TfreeL, Tforce, amplicomp
end module m_equatorial

!> inverse-map smoother in orthogonalisenet
module m_inverse_map

   type tops                                                           !< operator type
      double precision, allocatable, dimension(:, :) :: Az             !< cell-center coefficient matrix; node-to-cell
      double precision, allocatable, dimension(:, :) :: Gxi, Geta      !< netcell-gradient coefficient matrices; node-to-link
      double precision, allocatable, dimension(:)    :: Divxi, Diveta  !< netnode-gradient coefficient matrices; link-to-node
      double precision, allocatable, dimension(:)    :: Jxi, Jeta      !< netnode-gradient coefficient matrices; node-to-node
      double precision, allocatable, dimension(:)    :: ww2            !< weights in Laplacian smoother
   end type

   type tadm                                                           !< administration type (per node)
      integer                                        :: Ncell          !< number of netcells connected to the center node
      integer,          allocatable, dimension(:)    :: icell          !< netcells connected to node k0
      integer                                        :: nmk
      integer                                        :: nmk2
      integer,          allocatable, dimension(:)    :: kk2            ! local node administration
      integer,          allocatable, dimension(:,:)  :: kkc            ! position of netcell nodes in kk2 array
   end type

   type ttop                                                           !< topology array type (for unique topologies)
      integer,          allocatable, dimension(:)    :: nmk, nmk2      !< stored number of links and nodes in stencil resp.
      double precision, allocatable, dimension(:,:)  :: xi, eta        !< stored node coordinates (xi, eta)
   end type

!---------------------------
!  administration
!---------------------------
   integer                                       :: nmkx               !< maximum number of links connected to center node k0
   integer, save                                 :: nmkx2=4            !< maximum number of nodes in stencil
   integer, allocatable, dimension(:)            :: nmk2               !< number of nodes in stencil
   integer, allocatable, dimension(:,:)          :: kk2                !< node administration; first row, i.e. kk2(1,:), points to center nodes

!---------------------------
!  unique topologies
!---------------------------
   integer                                        :: numtopo           !< number of unique topologies
   integer,          allocatable, dimension(:)    :: ktopo             !< index in topology array

!--------------------------------------------
!  parameters
!--------------------------------------------
   integer,          parameter                    :: M=6               !< maximum number of nodes per netcell

end module m_inverse_map




!> global variables for spline2curvi
module m_spline2curvi
   integer, parameter                               :: Nsubmax = 10      !< maximum number of subintervals of grid layers, each having their own exponential grow factor
   type tspline                                                          !< center spline type that contains information derived from cross splines
!     cross spline data
      integer                                       :: id                !< 0: center spline, 1: true cross spline, 2: unassociated bounding spline, 3: artificial cross spline, -i, where i>0: associated bounding spline, with centerspline -i
      integer                                       :: ncs               !< number of cross splines
      double precision                              :: length            !< spline path length
      double precision                              :: hmax              !< maximum grid height
      integer,          allocatable, dimension(:)   :: ics               !< cross spline numbers
      Logical,          allocatable, dimension(:)   :: Lorient           !< cross spline is left to right (.true.) or not (.false.) w.r.t. center spline
      double precision, allocatable, dimension(:)   :: t                 !< center spline coordinates of cross splines
      double precision, allocatable, dimension(:)   :: cosphi            !< cosine of crossing angle
      double precision, allocatable, dimension(:,:) :: hL                !< left-hand side grid heights at cross spline locations for each grid layer subinterval, hL(1,:) being the height of the first subinterval, etc.
      double precision, allocatable, dimension(:,:) :: hR                !< right-hand side grid heights at cross spline locations for each grid layer subinterval, hR(1,:) being the height of the first subinterval, etc.
      integer,          allocatable, dimension(:)   :: NsubL             !< number of subintervals of grid layers at cross spline locations at the left-hand side of the spline, each having their own exponential grow factor
      integer,          allocatable, dimension(:)   :: NsubR             !< number of subintervals of grid layers at cross spline locations at the right-hand side of the spline, each having their own exponential grow factor

!     grid data
      integer                                           :: mfac              !< number of grid intervals on the spline
      integer,                       dimension(Nsubmax) :: nfacL             !< number of grid layers in each subinterval at the left-hand side of the spline *not used yet*
      integer,                       dimension(Nsubmax) :: nfacR             !< number of grid layers in each subinterval at the right-hand side of the spline *not used yet*
      integer                                           :: iL                !< index in the whole gridline array of the first grid point on the left-hand side of the spline
      integer                                           :: iR                !< index in the whole gridline array of the first grid point on the right-hand side of the spline
   end type

   type(tspline),    allocatable, dimension(:)      :: splineprops           !< spline properties

   double precision, allocatable, dimension(:)      :: xg1, yg1              !< coordinates of the first gridline
   double precision, allocatable, dimension(:)      :: sg1                   !< center spline coordinates of the first gridline

   integer                                        :: jacirc    = 0           !< circularly connected grid (1) or not (0) (for netbound2curvi), note: valid for one gridline only

   integer                                        :: jaoutside = 1           ! grow the grid outside the prescribed grid height

   double precision                               :: daspect   = 0.1d0       ! aspect ratio
   double precision                               :: dgrow     = 1.1d0       ! grow factor of aspect ratio
   double precision                               :: dheight0  = 1.0d1       ! grid layer height
   double precision                               :: maxaspect = 1.0d0       ! maximum cell aspect ratio *inoperative*
   double precision                               :: dwidth    = 0.5d3       ! average mesh width on center spline

   double precision                               :: dtolLR    = 1d-4        ! on-top-of-each-other tolerance *IMPORTANT*
   double precision                               :: dtolcos   = 0.95d0      ! minimum allowed absolute value of crossing-angle cosine

   integer                                        :: NFACUNIMAX = 5          ! maximum number of layers in the uniform part

   integer                                        :: jaCheckFrontCollision = 0    ! check for collisions with other parts of the front (1) or not (0)

   double precision                               :: dunigridsize = 0d0      ! uniform grid size (netboundary to grid only)

   integer                                        :: jacurv = 1              ! curvature adapted grid spacing (1) or not (0)


end module m_spline2curvi

module m_samples_refine     ! used in refinecellsandfaces2 and in sample paths
   integer                                         :: NDIM=5             !< sample vector dimension
   double precision, allocatable, dimension(:,:,:) :: zss                !< sample data [zs, direction vector x-component, direction vector y-component, refinement criterion, ridge distance], dim(NDIM,MXSAM,MYSAM)

   integer                                         :: Nsamplesmooth  = 0     !< number of sample smoothing iterations
   integer                                         :: Nsamplesmooth_last = -1 !< last number of sample smoothing iterations
   integer                                         :: MAXLEVEL       = 10    !< maximum number of refinement levels
   double precision                                :: threshold      = 1d2   !< typical obstacle height in grid refinement
   double precision                                :: thresholdmin   = 1d0   !< minimum obstacle height grid refinement
   double precision                                :: hmin           = 5d4   !< minimum cell size
   integer                                         :: jadirectional  = 0     !< directional refinement (1) or not (0)

   integer, parameter                              :: iHesstat_OK    = 0     !< sample Hessians up-to-date
   integer, parameter                              :: iHesstat_DIRTY = 1     !< sample Hessians out-of-date
   integer                                         :: iHesstat       = 0     !< sample Hessians up-to-date (0) or not (1)

   integer, parameter                              :: ITYPE_RIDGE       = 1     !< critetrion based on ridge-detection
   integer, parameter                              :: ITYPE_WAVECOURANT = 2     !< critetrion based on wave Courant number
   integer, parameter                              :: ITYPE_MESHWIDTH   = 3     !< criterion based on maximum mesh width
   integer                                         :: irefinetype       = ITYPE_WAVECOURANT     !< refinement criterion type
   integer                                         :: jaconnect         = 1     !< connect hanging nodes (1) or not (0)
   double precision                                :: Dt_maxcour        = 0d0   !< maximum time-step in courant grid
   double precision                                :: dminsampledist    = 0d0   !< minimum sample distance
   integer                                         :: jaoutsidecell     = 1     !< take samples outside cell into account (1) or not (0)
end module m_samples_refine

module m_kml_parameters
implicit none

    integer          :: kml_janet      !< Whether or not (1/0) to export flat view of 2D+1D grid (faster)
    integer          :: kml_jadepth    !< Whether or not (1/0) to export bathymetry view of grid cells (nicer).
    integer          :: kml_jadepth3d  !< Whether or not (1/0) to export bathymetry view in 3D.
    double precision :: kml_altfact    !< Altitude exaggeration factor: altitude differences are multiplied by this.
    integer          :: kml_jaoffsetzk !< Whether or not (1/0) to offset all altitudes with deepest zk-value.
    double precision :: kml_useroffset !< Additional user offset for altitude values.
    double precision :: kml_dmiss      !< Dummy altitude to replace missing zk values.
    double precision :: kml_zmin, kml_zmax !< Min/max values used for color scaling.

contains

!> This subroutine should be called during program initialization.
subroutine default_kml_parameters()
    kml_janet       = 1    !< Whether or not (1/0) to export flat view of 2D+1D grid (faster)
    kml_jadepth     = 0    !< Whether or not (1/0) to export bathymetry view of grid cells (nicer).
    kml_jadepth3d   = 0    !< Whether or not (1/0) to export bathymetry view in 3D.
    kml_altfact     = 5    !< Altitude exaggeration factor: altitude differences are multiplied by this.
    kml_jaoffsetzk  = 1    !< Whether or not (1/0) to offset all altitudes with deepest zk-value.
    kml_useroffset  = 0d0  !< Additional user offset for altitude values.
    kml_dmiss       = 99d0 !< Dummy altitude to replace missing zk values.
end subroutine default_kml_parameters

end module m_kml_parameters

module m_timer
implicit none
   integer, parameter                      :: jatimer = 1  !< time parallel solver (1) or not (0)
   integer, parameter                      :: NUMT=29      !< number of timings
   double precision,  dimension(3,NUMT)    :: t            !< wall-clock timings, (1,:): start, (2,:): end, (3,:): sum
   double precision,  dimension(3,NUMT)    :: tcpu         !< CPU        timings, (1,:): start, (2,:): end, (3,:): sum
   integer,           dimension(NUMT)      :: itstat       !< timer status, 0: not timing (stopped), 1: timing (started)

   integer                                 :: numtsteps    !< number of time steps
   integer                                 :: numcgits     !< number of CG iterations

   integer, parameter                      :: IREDUCE    = 1
   integer, parameter                      :: IMAKEMAT   = 2
   integer, parameter                      :: IPACKMAT   = 3
   integer, parameter                      :: IGAUSSEL   = 4
   integer, parameter                      :: ICG        = 5
   integer, parameter                      :: IGAUSSSU   = 6
   integer, parameter                      :: IMPICOMM   = 7
   integer, parameter                      :: ITOTALSOL  = 8
   integer, parameter                      :: ITIMESTEP  = 9
   integer, parameter                      :: ITOTAL     = 10
   integer, parameter                      :: IPOSTMPI   = 11
   integer, parameter                      :: IUPDSALL   = 12
   integer, parameter                      :: IUPDU      = 13
   integer, parameter                      :: IMPIREDUCE = 14
   integer, parameter                      :: IPARTINIT  = 15
   integer, parameter                      :: IOUTPUT    = 16
   integer, parameter                      :: IOUTPUTMPI = 17
   integer, parameter                      :: ITRANSPORT = 18
   integer, parameter                      :: IXBEACH    = 19
   integer, parameter                      :: IAXPY      = 20
   integer, parameter                      :: IDEBUG     = 21
   integer, parameter                      :: IFMWAQ     = 22
   integer, parameter                      :: IFILT_COEF = 23
   integer, parameter                      :: IFILT_SOLV = 24
   integer, parameter                      :: IFILT      = 25
   integer, parameter                      :: IFILT_MAT  = 26
   integer, parameter                      :: IFILT_COPYBACK = 27
   integer, parameter                      :: IFILT_OTHER = 28
   integer, parameter                      :: IEROSED    = 29


   character(len=10), dimension(numt), parameter :: tnams= [character(len=10) :: &
                                                               'reduce',      &
                                                               'make_mat.',   &
                                                               'pack',        &
                                                               'Gauss_elem',  &
                                                               'CG',          &
                                                               'Gauss_subs',  &
                                                               'MPI_comm',    &
                                                               'total_sol',   &
                                                               'timestep',    &
                                                               'total',       &
                                                               'MPI_post',    &
                                                               'MPI_sall',    &
                                                               'MPI_u',       &
                                                               'MPI_reduce',  &
                                                               'part_init',   &
                                                               'output',      &
                                                               'MPI_output',  &
                                                               'transport',   &
                                                               'XBeach',      &
                                                               'Axpy',        &
                                                               'debug',       &
                                                               'fmwaq',       &
                                                               'filt_coef',   &
                                                               'filt_solv',   &
                                                               'filter',      &
                                                               'filter_mat',  &
                                                               'filter_cpb',  &
                                                               'filter_oth',  &
                                                               'erosed' ]
   contains

!> initialize timers
   subroutine initimer()
      implicit none

      t      = 0d0
      tcpu   = 0d0
      itstat = 0

      numtsteps = 0
      numcgits  = 0
   end subroutine initimer

!> start the timer
   subroutine starttimer(itvar)
      implicit none

      integer, intent(in) :: itvar  !< timer number

      double precision    :: tloc

!     check status
      if ( itstat(itvar).ne.0 ) then
         write (6,'("starttimer: status error for timer ", I0)') itvar
      end if

      call klok(tloc)
      t(1,itvar) = tloc

      call cpu_time(tcpu(1,itvar))

!     set status
      itstat(itvar) = 1

      return
   end subroutine starttimer

!> stop the timer
   subroutine stoptimer(itvar)
      use MessageHandling

      implicit none
      integer, intent(in)         :: itvar  !< timer number

      double precision            :: tloc
      double precision, parameter :: dtol=1d-3 !< timer tolerance

!     check status
      if ( itstat(itvar).ne.1 ) then
         write (6,'("stoptimer: status error for timer ", I0)') itvar
      else
         call klok(tloc)
         t(2,itvar) = tloc
         t(3,itvar) = t(3,itvar) + tloc - t(1,itvar)

         call cpu_time(tcpu(2,itvar))
         tcpu(3,itvar) = tcpu(3,itvar) + tcpu(2,itvar) - tcpu(1,itvar)

!         if ( (tcpu(3,itvar)-t(3,itvar)).gt.dtol  ) then   ! should not happen
!            write(6,*) tnams(itvar)
!            write(6,*) 't=',    t(:,itvar)
!            write(6,*) 'tcpu=', tcpu(:,itvar)
!            call mess(LEVEL_ERROR, 'stoptimer: timer error')
!         end if
      end if

!     set status
      itstat(itvar) = 0

      return
   end subroutine stoptimer

   !> get timer value
   double precision function gettimer(itype, itvar)
      implicit none
      integer, intent(in)         :: itype  !< timer type, cpu-time (0) or wall-clock time (other)
      integer, intent(in)         :: itvar  !< timer number

      if ( itype.eq.0 ) then
         gettimer = tcpu(3,itvar)
      else
         gettimer = t(3,itvar)
      end if

      return
   end function gettimer

end module m_timer


!> process command line option
module m_commandline_option
implicit none
   integer, parameter  :: MAXOPTLEN = 255          !< maximum option string length
   integer, parameter  :: MAXKEYLEN = 255          !< maximum key string length
   integer, parameter  :: MAXKEYS   = 16           !< maximum number of key-value pairs
   integer, parameter  :: MAXSTRLEN = 255          !< maximum value string length
   integer, parameter  :: IMISS = -999999          !< unset value

!  input files from command line
   integer, parameter                             :: lenfile  = 255     !< maximum filename length
   integer, parameter                             :: maxnumfiles = 10   !< maximum number of files
   integer                                        :: numfiles           !< number of files to be loaded
   character(len=lenfile), dimension(maxnumfiles) :: inputfiles         !< files to be loaded
   character(len=lenfile)                         :: iarg_outfile = ' ' !< Output filename for several commandline/batch-mode operations (not related to model runs).
   integer                                        :: iarg_autostart     !< autostart/autstartstop or not set (-1)
   integer                                        :: iarg_usecaching    !< use cache file or not or not set (-1)

contains
!> read, from a string, command line option with key-value pair(s) of the form
!>  <->[-[...]]<option>[:key1=val1[:key2=val2[...]]]
!> key=val are string-integer pairs
   subroutine read_commandline_option(str, Soption, Nkeys, Skeys, ivals, svals)
      implicit none

      character(len=*),                             intent(in)  :: str        !< string
      character(len=MAXOPTLEN),                     intent(out) :: Soption    !< option
      integer,                                      intent(out) :: Nkeys      !< number of keys
      character(len=MAXKEYLEN), dimension(MAXKEYS), intent(out) :: Skeys      !< keys
      integer,                  dimension(MAXKEYS), intent(out) :: ivals      !< values
      character(len=MAXSTRLEN), dimension(MAXKEYS), intent(out) :: svals

      integer                                                   :: ipoint, ibegin, iend, iequal, Lenstr

      Soption = ''
      Nkeys = 0
      Skeys = ''
      ivals = IMISS

      Lenstr = len_trim(str)

   !  ignore leading dashes -/--/... (but dashes inside option names/values are allowed)
      ipoint=1
      do while (ipoint <= Lenstr)
         if (str(ipoint:ipoint) /= '-') exit
         ipoint = ipoint+1
      end do

   !  proceed if argument is option only
      if ( ipoint.eq.1 ) return

   !  read option
      ibegin = ipoint
      iend   = index(str(ipoint:Lenstr), ':')+ibegin-2
      if ( iend.lt.ibegin ) iend = Lenstr
      Soption=trim(adjustL(str(ibegin:iend)))
      ipoint = iend+1

   !  read key-value pairs
      do while ( ipoint.lt.Lenstr )
   !     find begin of this substring, that succeeds the last colon
         ibegin = ipoint+1

   !     find end of this substring, that preceeds the next colon or end of string
         iend = index(str(ibegin:Lenstr), ':')+ibegin-2
         if ( iend.le.ibegin ) iend = Lenstr

   !     find equal sign
         iequal = index(str(ibegin:iend), '=')+ibegin-1
         if ( iequal.lt.ibegin ) then
   !        no equal sign: no value
            iequal = iend+1
         end if
         if ( iequal.gt.ibegin ) then
            Nkeys=Nkeys+1
   !        read key
            Skeys(Nkeys) = trim(adjustL(str(ibegin:iequal-1)))
            if ( iequal.lt.iend ) then
   !           read value as string
               read(str((iequal+1):iend), *, err=666) svals(Nkeys)
   !           read value as integer
               read(str((iequal+1):iend), *, err=666) ivals(Nkeys)
   666         continue
            end if
         end if

         ipoint = iend+1
      end do

      return
   end subroutine read_commandline_option

end module

!> transport of many (scalar) constituents is performed with the transport module
!!   -constituents are stored in the constituents array
!!   -salt and temperature are filled from and copied to the sa1 and tem1 arrays, respectively
!!   -salt and temperature boundary and initial conditions are applied to sa1 and tem1, not to the constituents directly
!! tracers:
!!   -tracers intial and boundary conditions are directly applied to the constituents
!!   -the tracers always appear at the end of the whole constituents array
!!   -the constituents numbers of the tracers are from "ITRA1" to "ITRAN", where ITRAN=0 (no tracers) or ITRAN=NUMCONST (tracers come last)
!!   -tracers with boundary conditions (not necessarily all tracers) have their own numbering
!!   -the tracer (with bc's) to consituent mapping is called "itrac2const"
!!   -boundary condition related information of the tracers are stored in "bndtr" of type "bndtype"
module m_transport
   integer, parameter                            :: NAMLEN = 128
   integer                                       :: NUMCONST       ! Total number of constituents
   integer                                       :: NUMCONST_MDU   ! number of constituents as specified in mdu/ext file
   integer                                       :: ISALT  ! salt
   integer                                       :: ITEMP  ! temperature
   integer                                       :: ISED1  ! first sediment fraction
   integer                                       :: ISEDN  ! last  sediment fraction
   integer                                       :: ISPIR  ! secondary flow intensity
   integer                                       :: ITRA1  ! first tracer
   integer                                       :: ITRAN  ! last  tracer, should be at the back
   integer                                       :: ITRAN0         ! back up of ITRAN

!  tracers
   integer,          dimension(:),   allocatable :: itrac2const   ! constituent number of tracers (boundary conditions only)
   integer,          dimension(:),   allocatable :: ifrac2const   ! constituent number of sediment fractions
   double precision, dimension(:,:), allocatable, target :: constituents    ! constituents, dim(NUMCONST,Ndkx)

   character(len=NAMLEN), dimension(:), allocatable :: const_names    ! constituent names
   character(len=NAMLEN), dimension(:), allocatable :: const_units    ! constituent units
   character(len=NAMLEN), parameter                 :: DEFTRACER = 'default_tracer'

   integer,          dimension(:,:), allocatable :: id_const   ! consituent id's in map-file

   double precision, dimension(:,:), allocatable :: fluxhor  ! horizontal fluxes
   double precision, dimension(:,:), allocatable :: fluxver  ! vertical   fluxes
   double precision, dimension(:,:), allocatable :: fluxhortot ! sum of horizontal fluxes (fluxhor) at local time stepping
   double precision, dimension(:,:), allocatable :: sinksetot    ! sum of sed sinks at local time stepping
   double precision, dimension(:,:), allocatable :: sinkftot    ! sum of  fluff sinks at local time stepping

   double precision, dimension(:),   allocatable :: thetavert  ! vertical advection fluxes explicit (0) or implicit (1)

   double precision, dimension(:),   allocatable :: difsedu  ! sum of molecular and user-specified diffusion coefficient
   double precision, dimension(:),   allocatable :: difsedw  ! sum of molecular and user-specified diffusion coefficient

   double precision, allocatable                 :: dsedx   (:,:) !< cell center constituent gradient
   double precision, allocatable                 :: dsedy   (:,:) !< cell center constituent gradient

   double precision, dimension(:,:), allocatable :: const_sour  ! sources in transport, dim(NUMCONST,Ndkx)
   double precision, dimension(:,:), allocatable :: const_sink  ! linear term of sinks in transport, dim(NUMCONST,Ndkx)

!  work arrays
   double precision, dimension(:,:), allocatable :: rhs      ! right-hand side, dim(NUMCONST,Ndkx)
   double precision, dimension(:,:), allocatable :: a,b,c,d  ! aj(i,j)*sed(j,k-1) + bj(i,j)*sed(j,k) + c(i,j)*sed(j,k+1) = d(i), i=k-kb+1
   double precision, dimension(:),   allocatable :: sol, e   ! solution and dummy array in tridag, respectively

!  for visualisation
   integer                                       :: iconst_cur ! active constituent (for visualization)

!  for local timestepping
   double precision, dimension(:,:), allocatable :: sumhorflux    !< sum of horizontal fluxes, dim(NUMCONST,Ndkx)
   integer                                       :: nsubsteps     !< total number of substeps
   integer,          dimension(:),   allocatable :: ndeltasteps   !< cell-based number of subtimesteps between updates, dim(Ndx)
   integer,          dimension(:),   allocatable :: jaupdate      !< update cell (1) or not (0), dim(Ndx)
   integer,          dimension(:),   allocatable :: jaupdatehorflux  !< update horizontal flux (1) or not (0), dim(Lnx)
   double precision, dimension(:),   allocatable :: dtmax         !< maximum local time-step (for water columns)
   double precision                              :: dtmin_transp  !< limiting time-step
   integer                                       :: kk_dtmin      !< flownode of limiting time-step
   double precision                              :: time_dtmax    !< time for which maximum local time-step is evaluated
   integer                                       :: numnonglobal  !< number of cells not at the global time step
   double precision, dimension(:),   allocatable :: sumdifflim    !< contribution of diffusion to transport time-step limitation
   double precision, dimension(:),   allocatable :: dxiAu         !< area of horizontal diffusive flux divided by Dx

!  for sediment advection velocity
   integer,          dimension(:),   allocatable :: jaupdateconst !< update constituent (1) or not (0)
   integer,          dimension(:),   allocatable :: noupdateconst !< do not update constituent (1) or do (0)

!  time step related
   integer :: jalimitdiff
   integer :: jalimitdtdiff

   double precision :: dsum

   ! DEBUG
   double precision, dimension(:),   allocatable :: u1sed
   double precision, dimension(:),   allocatable :: q1sed
   double precision, dimension(:),   allocatable :: ucxsed
   double precision, dimension(:),   allocatable :: ucysed
   double precision, dimension(:),   allocatable :: qcxsed
   double precision, dimension(:),   allocatable :: qcysed
   double precision, dimension(:,:),   allocatable :: xsedflux
   double precision, dimension(:,:),   allocatable :: ysedflux
   !\ DEBUG
end module m_transport

module m_fm_wq_processes
   use precision
   use processes_input
   use processes_pointers
   use dlwq_data
   use processet
   use output

   character(20), allocatable                :: syunit_sub(:)               !< substance unit from substance file
   character(20), allocatable                :: coname_sub(:)               !< constant names from substance file
   real         , allocatable                :: covalue_sub(:)              !< values for contants from substance file
   character(20), allocatable                :: ouname_sub(:)               !< output names from substance file
   character(80), allocatable                :: oudesc_sub(:)               !< output decriptions from substance file
   integer( 4)                               :: noout_sub                   !< number of outputs requested in substance file

   character(20), allocatable                :: syname_eho(:)               !< substance names from extra history output file
   character(20), allocatable                :: syunit_eho(:)               !< substance names from extra history output file
   character(20), allocatable                :: coname_eho(:)               !< constant names from extra history output file
   real         , allocatable                :: covalue_eho(:)              !< values for contants from extra history output file
   character(20), allocatable                :: ouname_eho(:)               !< output names from extra history output file
   character(80), allocatable                :: oudesc_eho(:)               !< output decriptions from extra history output file
   integer( 4)                               :: noout_eho                   !< number of outputs requested in extra history output file

   character(len=256)                        :: substance_file              !< substance file
   character(len=256)                        :: his_output_file             !< extra history output file
   character(len=256)                        :: proc_log_file               !< processes log file
   character(len=256)                        :: proc_def_file               !< processes definition file
   character(len=256)                        :: bloom_file                  !< BLOOM algae spiecies paramter file
   character(len=256)                        :: statistics_file             !< file with configuration for statistics

   integer, parameter                        :: NAMWAQLEN = 128
   integer                                   :: ithndlwq = 0                !< overall timer for water quality processes
   integer                                   :: jawaqproc = 0               !< switch for water quality processes (1 = substances initiated, 2 = processes activated too)
   real(hp)                                  :: waq_vol_dry_thr = 1.0d-3    !< minimum volume for processes to be active
   real(hp)                                  :: waq_dep_dry_thr = 1.0d-3    !< minimum depth for processes to be active
   integer                                   :: flux_int                    !< flux integration by WAQ (1) or by FM (2, not implemented)
   integer                                   :: kbx                         !< pointer of first segment to D-Flow FM 3D administration
   integer                                   :: ktx                         !< pointer of last  segment to D-Flow FM 3D administration

   integer                                   :: noseg                       !< Nr. of computational volumes
   integer                                   :: noq1                        !< Number of exchanges first direction
   integer                                   :: noq2                        !< Number of exchanges second direction
   integer                                   :: noq3                        !< Number of exchanges vertical
   integer                                   :: noq4                        !< Number of exchanges in the bed
   integer,  allocatable, dimension(:,:)     :: iexpnt                      !< Exchange pointer

   real(hp), allocatable, dimension(:,:)     :: amass                       !< mass array to be updated
   logical , allocatable, dimension(:)       :: wetdry                      !< wet/dry indicator (wet=true/dry=false)
   logical , allocatable, dimension(:)       :: doproc                      !< do processes indicator (do processes=true/no processes=false)
   integer , allocatable, dimension(:)       :: iknmrk                      !< segment characteristics.
                                                                            !< 1st digit from the right indicates wet/dry (1/0),
                                                                            !< 2nd digit indicates top/middle/bottom/top&bottom in water column (1/2/3/0).

   integer                                   :: sizepmsa                    !< size of (pms)a-array
   real(sp), allocatable, dimension(:)       :: pmsa                        !< the actual data array

   real(sp), allocatable, dimension(:,:)     :: deriv                       !< Model derivatives in mass/m3/s (= stochi(notot ,noflux) * flux(noflux, noseg))
   real(sp), allocatable, dimension(:,:)     :: flux                        !< Proces fluxes in mass/m3/s

   integer,  allocatable, dimension(:)       :: isys2const                  !< WAQ substance to D-Flow FM constituents
   integer,  allocatable, dimension(:)       :: iconst2sys                  !< WAQ substance to D-Flow FM constituents
   integer,  allocatable, dimension(:)       :: isys2trac                   !< WAQ active system to D-FlowFM tracer
   integer,  allocatable, dimension(:)       :: isys2wqbot                  !< WAQ inactive system to D-FlowFM water quality bottom variable
   integer,  allocatable, dimension(:)       :: ifall2vpnw                  !< substance-with-fall-velocity to WAQ numbering in fall-velocity array

   integer                                   :: numwqbots                   !< number of water quality bottom variables
   character(len=NAMWAQLEN), dimension(:), allocatable :: wqbotnames        !< water quality bottom variable names
   character(len=NAMWAQLEN), dimension(:), allocatable :: wqbotunits        !< water quality bottom variable units
   integer,  allocatable, dimension(:,:)     :: id_wqb                      !< wqbot id's in map-file
   real(hp), allocatable, dimension(:,:)     :: wqbot                       !< water quality bottom variable values in double precission

   type(outputcoll)                          :: outputs                     !< output structure
   integer,  allocatable, dimension(:,:)     :: id_waq                      !< waq output id's in map-file
   integer,  allocatable, dimension(:,:)     :: id_wqst                     !< waq stat time output id's in map-file
   integer,  allocatable, dimension(:,:)     :: id_wqse                     !< waq stat end output id's in map-file
   real(hp), allocatable, dimension(:,:)     :: waqoutputs                  !< waq outputs, dim(noout,Ndkx)

   integer                                   :: isfsurf                     !< pointer to surface         segment function
   integer                                   :: isftau                      !< pointer to tau             segment function
   integer                                   :: isfvel                      !< pointer to velocity        segment function
   integer                                   :: isfsal                      !< pointer to Salinity        segment function
   integer                                   :: isftem                      !< pointer to Temperature     segment function
   integer                                   :: isfvwind                    !< pointer to wind vel. magn. segment function
   integer                                   :: isfwinddir                  !< pointer to wind direction  segment function
   integer                                   :: isffetchl                   !< pointer to fetch length    segment function
   integer                                   :: isffetchd                   !< pointer to fetch depth     segment function
   integer                                   :: isfradsurf                  !< pointer to solar radiation segment function
   integer                                   :: isfrain                     !< pointer to rain            segment function
!
!     Balance output
!
   integer                                   :: ibflag                      !< if 1 then mass balance output
   real(hp), allocatable, dimension(:,:,:)   :: flxdmp                      !< Fluxes at dump segments
   real(hp), allocatable, dimension(:,:,:)   :: flxdmpreduce                !< Fluxes at dump segments
   real(hp), allocatable, dimension(:,:,:)   :: flxdmptot                   !< Total fluxes at dump segments

!   double precision           :: dum

   integer                                   :: jamba                       !< switch for mass balance areas being active
   integer                                   :: nomba = 0                   !< number of mass balance areas
   integer                                   :: nombabnd                    !< number of mass balance areas and boundaries
   character(len=NAMWAQLEN),allocatable      :: mbaname(:)                  !< parameter names
   integer, allocatable                      :: mbadef(:)                   !< mass balance area (mba) definition
   integer, allocatable                      :: mbadefdomain(:)             !< mass balance area (mba) definition without ghost cells
   integer                                   :: id_mba(3)                   !< mbd id's in map-file
   integer, allocatable                      :: mbalnfromto(:,:)            !< from mba (1:lnxi) or bnd (lnxi+1:lnx) to mba for each link (2D)
   integer, allocatable                      :: mbalnused(:,:)              !< number of links between mda and mbabnd that are actually active
   integer, allocatable                      :: mbasorsin(:,:)              !< mba for each side of a source sink
   integer, allocatable                      :: mbasorsinout(:,:)           !< (reduced) mba for each side of a source sink for output
   integer                                   :: nombaln                     !< number of links needed for mass balance (2D)
   integer, allocatable                      :: mbalnlist(:)                !< list of links needed for the mass balance (2D)
   logical                                   :: mbaremaining                !< mass balance area for ramaining cells added?
   integer                                   :: nomon                       !< number of mass balance areas
   character(len=NAMWAQLEN),allocatable      :: monname(:)                  !< parameter names
   integer, allocatable                      :: mondef(:,:)                 !< monitoring area definition
   integer                                   :: lunmbahis                   !< logical unit of mba his-file
   integer                                   :: lunmbatothis                !< logical unit of mba total his-file
   integer                                   :: lunmbabal                   !< logical unit of mba bal-file
   integer                                   :: lunmbatotbal                !< logical unit of mba total bal-file
   integer                                   :: itimembastart               !< start time of balance period
   integer                                   :: itimembastarttot            !< start time of balance period
   integer                                   :: itimembaend                 !< end time of balance period
   double precision                          :: timembastart                !< start time of balance period
   double precision                          :: timembastarttot             !< start time of balance period
   double precision                          :: timembaend                  !< end time of balance period

   real(hp), allocatable, dimension(:)       :: mbaarea                     !< surface area of mass balance area

   real(hp), allocatable, dimension(:)       :: mbavolumebegin              !< begin volume in mass balance area
   real(hp), allocatable, dimension(:)       :: mbavolumebegintot           !< total begin volume in mass balance area
   real(hp), allocatable, dimension(:)       :: mbavolumeend                !< end volume in mass balance area

   real(hp), allocatable, dimension(:,:,:)   :: mbaflowhor                  !< periodical flow between balance areas and between boundaries and balance areas
   real(hp), allocatable, dimension(:,:,:)   :: mbaflowhortot               !< total flow between balance areas and between boundaries and balance areas
   real(hp), allocatable, dimension(:,:)     :: mbaflowsorsin               !< periodical flow from source sinks
   real(hp), allocatable, dimension(:,:)     :: mbaflowsorsintot            !< total flow from source sinks

   real(hp), allocatable, dimension(:,:)     :: mbamassbegin                !< begin volume in mass balance area
   real(hp), allocatable, dimension(:,:)     :: mbamassbegintot             !< total begin volume in mass balance area
   real(hp), allocatable, dimension(:,:)     :: mbamassend                  !< end volume in mass balance area

   real(hp), allocatable, dimension(:,:,:,:) :: mbafluxhor                  !< periodical fluxes between balance areas and between boundaries and balance areas
   real(hp), allocatable, dimension(:,:,:,:) :: mbafluxhortot               !< total fluxes between balance areas and between boundaries and balance areas
   real(hp), allocatable, dimension(:,:,:,:) :: mbafluxsorsin               !< periodical fluxes from source sinks
   real(hp), allocatable, dimension(:,:,:,:) :: mbafluxsorsintot            !< total fluxes from source sinks

   real(hp), allocatable, dimension(:)       :: mbavolumereduce             !< begin volume in mass balance area
   real(hp), allocatable, dimension(:,:,:)   :: mbaflowhorreduce            !< periodical flow between balance areas and between boundaries and balance areas
   real(hp), allocatable, dimension(:,:)     :: mbaflowsorsinreduce         !< periodical flow from sources sinks
   real(hp), allocatable, dimension(:,:)     :: mbamassreduce               !< begin volume in mass balance area
   real(hp), allocatable, dimension(:,:,:,:) :: mbafluxhorreduce            !< periodical fluxes between balance areas and between boundaries and balance areas
   real(hp), allocatable, dimension(:,:,:,:) :: mbafluxsorsinreduce         !< periodical fluxes from source sinks

end module

module dfm_error
implicit none

   integer, parameter :: DFM_NOERR         = 0   !< Success code.
   integer, parameter :: DFM_GENERICERROR  = 1   !< Error without further details.
   integer, parameter :: DFM_EXIT          = 10  !< Exit program without error.
   integer, parameter :: DFM_USERINTERRUPT = 11  !< No error, but execution stopped because of user interrupt.
   integer, parameter :: DFM_MISSINGARGS   = 13  !< Missing input arguments
   integer, parameter :: DFM_SIGINT        = 121 !< SIGINT signal was give from outside the program (Ctrl-C)

   integer, parameter :: DFM_EFILEFORMAT   = -13 !< Error code: File has wrong format.
   integer, parameter :: DFM_WRONGINPUT    = 14  !< Error code: Generic error about wrong data in input.
   integer, parameter :: DFM_EXTFORCERROR  = 16  !< Error code: Related to external forcing (EC-module etc.)
   integer, parameter :: DFM_NOMODEL             = 20  !< No model loaded.
   integer, parameter :: DFM_MODELNOTINITIALIZED = 21  !< Model was empty or not properly initialized.
   integer, parameter :: DFM_INVALIDSTATE  = 22 !< Model state has become unphysical
   integer, parameter :: DFM_INVALIDTARGETTIME = 30 !< Invalid target time or timestep requested (via API).
   integer, parameter :: DFM_TIMESETBACK       = 31 !< Warning only: time setbacks occurred in step_reduce.

   integer, parameter :: DFM_NOTIMPLEMENTED    = 999 !< Generic error that functionality is intended but not yet implemented.

   logical            :: do_check_bmi_timestep =.true. !< SH: temporary flag to be able to run both DFlowFM/Wave and 1d2d
                                                       !<     to be removed when 1d2d time step handling is fixed
                                                       !<     (see also routine deactivate_time_step_checking() in unstruc_bmi.f90)

   contains
   !> Returns a human-readable error string for a given integer error code.
   subroutine dfm_strerror(errorstring, ierr)
   character(len=*), intent(out) :: errorstring !< The string variable in which the error text will be put.
   integer,          intent(in)  :: ierr        !< The error code for which the error string should be returned.

   select case (ierr)
   case (DFM_NOERR)
      errorstring = 'No error'
   case (DFM_GENERICERROR)
      errorstring = 'Generic error'
   case (DFM_EXIT)
      errorstring = 'Program stopped'
   case (DFM_USERINTERRUPT)
      errorstring = 'User interrupted the model run.'
   case (DFM_MISSINGARGS)
      errorstring = 'Input arguments are missing.'
   case (DFM_SIGINT)
      errorstring = 'Interrupt signal was given from outside the program (Ctrl-C / SIGINT).'
   case (DFM_EFILEFORMAT)
      errorstring = 'Wrong file format.'
   case (DFM_WRONGINPUT)
      errorstring = 'Invalid data in input.'
   case (DFM_NOMODEL)
      errorstring = 'No model loaded.'
   case (DFM_MODELNOTINITIALIZED)
      errorstring = 'Model was not initialized or model is empty.'
   case (DFM_INVALIDSTATE)
       errorstring = 'Model has reached an invalid state.'
   case (DFM_INVALIDTARGETTIME)
       errorstring = 'Invalid target time or timestep requested.'
   case default
      write (errorstring, '(a,i0,a)') 'Unknown error (', ierr, ')'
   end select

   end subroutine dfm_strerror
end module dfm_error

module m_strucs
use m_GlobalParameters
integer                                  :: nstru               !< total nr of structures

integer                                  :: mxgeneral           !< total nr of general structures
integer                                  :: mxuniversal         !< total nr of unversal weirs    etc etc

integer, allocatable                     :: Lstruc (:)          !< Flow Linknumbers on which structures are defined
integer, allocatable                     :: Itypstr(:)          !< Type indication for each type
integer, allocatable                     :: Ntypstr(:)          !< So many-st nr of this type e.g. (1:mxgeneral)

integer                                  :: mxstrhis = 16       !< leading dimension of
double precision, allocatable            :: strhis(:,:)         !< For all structures. when computing n+1, strhis has values of step n
                                                                !< strhis( 1,:) : Gate Opening Height
                                                                !< strhis( 2,:) : Crest level
                                                                !< strhis( 3,:) : Crest width
                                                                !< strhis( 4,:) : Discharge
                                                                !< strhis( 5,:) : Velocity
                                                                !< strhis( 6,:) :
                                                                !< strhis( 7,:) :
                                                                !< strhis( 8,:) :
                                                                !< strhis( 9,:) : Water level left
                                                                !< strhis(10,:) : Water level right
                                                                !< strhis(11,:) :
                                                                !< strhis(12,:) :
                                                                !< strhis(13,:) : reduction factor (ST_RIVER_WEIR)
                                                                !< strhis(14,:) : pressure difference
                                                                !< strhis(15,:) : Waterlevel on crest (general structure)
                                                                !< strhis(16,:) : Area

double precision, allocatable            :: strhis2(:,:)        !< holds values of strhis of step n-1


type tgeneralstruc
   double precision                      :: widthleftW1         !< this and following: see Sobek manual
   double precision                      :: levelleftZb1
   double precision                      :: widthleftWsdl
   double precision                      :: levelleftZbsl
   double precision                      :: widthcenter
   double precision                      :: levelcenter
   double precision                      :: widthrightWsdr
   double precision                      :: levelrightZbsr
   double precision                      :: widthrightW2
   double precision                      :: levelrightZb2
   double precision                      :: gateheight
   double precision                      :: gateheightintervalcntrl
   double precision                      :: pos_freegateflowcoeff
   double precision                      :: pos_drowngateflowcoeff
   double precision                      :: pos_freeweirflowcoeff
   double precision                      :: pos_drownweirflowcoeff
   double precision                      :: pos_contrcoeffreegate
   double precision                      :: neg_freegateflowcoeff
   double precision                      :: neg_drowngateflowcoeff
   double precision                      :: neg_freeweirflowcoeff
   double precision                      :: neg_drownweirflowcoeff
   double precision                      :: neg_contrcoeffreegate
   double precision                      :: extraresistance
   double precision                      :: dynstrucfact
   double precision                      :: dynstructext
   double precision                      :: gatedoorheight
   double precision                      :: dooropeningwidth
   double precision                      :: stabilitycounter
   double precision, allocatable         :: widthcenteronlink(:) !< For each crossed flow link the the center width portion of this genstr. (sum(widthcenteronlink(1:numlink)) should equal widthcenter)
   double precision, allocatable         :: gateheightonlink(:)  !< For each crossed flow link the the gate height portion of this genstr. (will be set to dummy high value in open part of sideways closing gates.)
   double precision, allocatable         :: gateclosedfractiononlink(:) !< part of the link width that is closed by the gate
   integer                               :: numlinks !< Nr of flow links that cross this generalstructure.

end type tgeneralstruc

integer, parameter :: numgeneralkeywrd = 26
character(len=256) :: generalkeywrd(numgeneralkeywrd) = (/ character(len=256) :: &
   'Upstream1Width',          & ! ( 1)
   'Upstream1Level',          & ! ( 2)
   'Upstream2Width',          & ! ( 3)
   'Upstream2Level',          & ! ( 4)
   'CrestWidth',              & ! ( 5)
   'CrestLevel',              & ! ( 6)
   'Downstream1Width',        & ! ( 7)
   'Downstream1Level',        & ! ( 8)
   'Downstream2Width',        & ! ( 9)
   'Downstream2Level',        & ! (10)
   'GateLowerEdgeLevel',      & ! (11)
   'gateheightintervalcntrl', & ! (12)
   'pos_freegateflowcoeff',   & ! (13)
   'pos_drowngateflowcoeff',  & ! (14)
   'pos_freeweirflowcoeff',   & ! (15)
   'pos_drownweirflowcoeff',  & ! (16)
   'pos_contrcoeffreegate',   & ! (17)
   'neg_freegateflowcoeff',   & ! (18)
   'neg_drowngateflowcoeff',  & ! (19)
   'neg_freeweirflowcoeff',   & ! (20)
   'neg_drownweirflowcoeff',  & ! (21)
   'neg_contrcoeffreegate',   & ! (22)
   'extraresistance',         & ! (23)
   'dynstructext',            & ! (24)
   'GateHeight',              & ! (25)
   'GateOpeningWidth'         & ! (26)
   /)
character(len=256) :: generalkeywrd_old(numgeneralkeywrd) = (/ character(len=256) :: &
   'widthleftW1',             & ! ( 1)
   'levelleftZb1',            & ! ( 2)
   'widthleftWsdl',           & ! ( 3)
   'levelleftZbsl',           & ! ( 4)
   'widthcenter',             & ! ( 5)
   'levelcenter',             & ! ( 6)
   'widthrightWsdr',          & ! ( 7)
   'levelrightZbsr',          & ! ( 8)
   'widthrightW2',            & ! ( 9)
   'levelrightZb2',           & ! (10)
   'gateheight',              & ! (11)
   'gateheightintervalcntrl', & ! (12)
   'pos_freegateflowcoeff',   & ! (13)
   'pos_drowngateflowcoeff',  & ! (14)
   'pos_freeweirflowcoeff',   & ! (15)
   'pos_drownweirflowcoeff',  & ! (16)
   'pos_contrcoeffreegate',   & ! (17)
   'neg_freegateflowcoeff',   & ! (18)
   'neg_drowngateflowcoeff',  & ! (19)
   'neg_freeweirflowcoeff',   & ! (20)
   'neg_drownweirflowcoeff',  & ! (21)
   'neg_contrcoeffreegate',   & ! (22)
   'extraresistance',         & ! (23)
   'dynstructext',            & ! (24)
   'gatedoorheight',          & ! (25)
   'door_opening_width'       & ! (26)
   /)
type(tgeneralstruc), allocatable, target :: generalstruc(:)


type tuniversalstruc

   integer :: idum
end type tuniversalstruc
type(tuniversalstruc), allocatable       :: universalstruc(:)

end module m_strucs


module m_plotdots
   implicit none
   integer                                     :: numdots      ! number of dots
   integer                                     :: NSIZE=0      ! array size
   double precision, dimension(:), allocatable :: xdots, ydots ! dot coordinates, dim(NSIZE)
   double precision, dimension(:), allocatable :: zdots        ! dot z-value

   double precision,               parameter   :: ZDOTDEFAULT = 0d0

   contains

   subroutine reallocdots(N)
      use m_alloc
      use m_missing
      implicit none

      integer, intent(in) :: N

      if ( N.gt.NSIZE) then
         NSIZE = 1+int(1.2d0*dble(N))

         call realloc(xdots, NSIZE, keepExisting=.true., fill=DMISS)
         call realloc(ydots, NSIZE, keepExisting=.true., fill=DMISS)
         call realloc(zdots, NSIZE, keepExisting=.true., fill=ZDOTDEFAULT)
      end if

      return
   end subroutine reallocdots

!> add a dot
   subroutine adddot(x,y,z)
      implicit none

      double precision,           intent(in) :: x, y
      double precision, optional, intent(in) :: z

      double precision :: zloc

      zloc = ZDOTDEFAULT
      if ( present(z) ) then
         zloc = z
      end if

      numdots = numdots+1
      call reallocdots(numdots)
      xdots(numdots) = x
      ydots(numdots) = y
      zdots(numdots) = zloc

      return
   end subroutine adddot

!  write dots to sample file
   subroutine write_dots(FNAM, jawritten)
      implicit none

      character(len=*), intent(in)  :: FNAM
      integer,          intent(out) :: jawritten

      integer                       :: i, id

      jawritten=0

      if ( numdots.lt.1 ) goto 1234

      call newfil(id,FNAM)

      do i=1,numdots
         write(id, "(3E15.5)") xdots(i), ydots(i), zdots(i)
      end do

      call doclose(id)

      jawritten=1

 1234 continue

      return
   end subroutine

   subroutine deldots()
      implicit none

      Numdots = 0

      return
   end subroutine

end module m_plotdots
module m_particles
   integer                                        :: japart       !< particles (1) or not (0)

   integer                                        :: Npart        !< number of particles
   double precision,  dimension(:),   allocatable :: xpart, ypart !< coordinates of particles, dim(Npart)
   double precision,  dimension(:),   allocatable :: zpart        !< z-coordinates of particles, dim(Npart), for spherical models
   double precision,  dimension(:),   allocatable :: dtremaining  !< remaining time, dim(Npart)
   integer,           dimension(:),   allocatable :: kpart        !< cell (flownode) number, dim(Npart)
!   integer,           dimension(:),   allocatable :: Lpart        !< edge (netlink)  number, dim(Npart)
!   character(len=40), dimension(:),   allocatable :: namepart     !< name of particle, dim(Npart)
   integer,           dimension(:),   allocatable :: iglob        !< global number, dim(Npart)
   integer                                        :: Nglob        !< maximum global number

   integer,           dimension(:),   allocatable :: numzero      !< number of consecutive (sub)times a particle was not displaces within a time-step

   integer                                        :: jatracer     !< add tracer with particle concentration (1) or not (0)
   double precision                               :: starttime    !< start time
   double precision                               :: timestep     !< time step (>0) or every computational timestep
   double precision                               :: timelast     !< last time of particle update
   double precision                               :: timenext     !< next time of particle update
   double precision                               :: timepart     !< time of particles
   character(len=22), parameter                   :: PART_TRACERNAME = 'particle_concentration' !< particle tracer name
   integer                                        :: part_iconst  !< particle tracer constituent number
   integer                                        :: threeDtype       !< depth averaged or 2D (0), free surface (1)

   double precision, dimension(:),    allocatable :: sbegin      !< water level at begin of time interval, dim(Ndx)
   double precision, dimension(:),    allocatable :: qpart       !< cummulative fluxes from begin of time interval, dim(Lnx)

   double precision, dimension(:),    allocatable :: qfreesurf       !< free surface flux (for threeDtype=1)

!   integer :: mfile
end module m_particles

module m_partrecons
   double precision,  dimension(:),   allocatable :: qe           !< fluxes at all edges, including internal
   double precision,  dimension(:),   allocatable :: u0x, u0y, alpha !< reconstruction of velocity fiels in cells, dim(numcells)
   double precision,  dimension(:),   allocatable :: u0z          !< reconstruction of velocity fiels in cells, dim(numcells), for spherical models

   integer,           dimension(:),   allocatable :: ireconst    !< sparse storage of velocity reconstructin, edges,        dim(jreconst(numcells+1)-1)
   integer,           dimension(:),   allocatable :: jreconst    !< sparse storage of velocity reconstructin, startpointer, dim(numcells+1)
   double precision,  dimension(:,:), allocatable :: Areconst    !< sparse storage of velocity reconstructin, [ucx, ucy, (ucz,) alpha] dim(jreconst(3 (4), numcells+1)-1)
end module m_partrecons

module m_partfluxes
   integer,          dimension(:),    allocatable :: iflux2link   !< sparse storage of edge-based flux to flowlink-based flux (prescribed), flowlinks, dim(jflux2link(numedges+1)-1)
   integer,          dimension(:),    allocatable :: jflux2link   !< sparse storage of edge-based flux to flowlink-based flux (prescribed), startpointer, dim(numedges+1)
   double precision, dimension(:),    allocatable :: Aflux2link   !< sparse storage of edge-based flux to flowlink-based flux (prescribed), coefficients, dim(jflux2link(numedges+1)-1)
end module m_partfluxes

module m_partmesh
!  mesh data
   integer                                         :: numnodes    !< number of nodes, >= numk
   integer                                         :: numedges    !< number of edges, >= numL
   integer                                         :: numcells    !< number of cells, >= nump
   integer                                         :: numorigedges  !< number of original "non-internal" edges (numL)

   integer,          dimension(:,:),   allocatable :: edge2node   !< edge-to-node, dim(2,numedges)
   integer,          dimension(:,:),   allocatable :: edge2cell   !< edge-to-cell, dim(2,numedges)

   double precision, dimension(:),     allocatable :: xnode       !< x-coordinate of nodes, dim(numnodes)
   double precision, dimension(:),     allocatable :: ynode       !< y-coordinate of nodes, dim(numnodes)
   double precision, dimension(:),     allocatable :: znode       !< z-coordinate of nodes, dim(numnodes), for spherical models

   double precision, dimension(:),     allocatable :: xzwcell     !< x-coordinate of cell c/g, dim(numcells)
   double precision, dimension(:),     allocatable :: yzwcell     !< y-coordinate of cell c/g, dim(numcells)
   double precision, dimension(:),     allocatable :: zzwcell     !< z-coordinate of cell c/g, dim(numcells), for spherical models
   double precision, dimension(:),     allocatable :: areacell    !< area of cell, dim(numcells)

   integer,          dimension(:),     allocatable :: icell2edge   !< sparse storage of cell-to-edge, data, dim(jcell2edge(numcells+1)-1)
   integer,          dimension(:),     allocatable :: jcell2edge   !< sparse storage of cell-to-edge, startpointer, dim(numcells+1)

   integer,          dimension(:),     allocatable :: edge2link    !< edge to "flowlink" (>0), no flowlink (0), or new inner link (<0), dim(numedges)
!   integer,          dimension(:),     allocatable :: nod2cell     !< "flownode" to cell (>0), first new "inner" triangle (<0), dim(numcells), note: numcells can be too large for array dimension
   integer,          dimension(:),     allocatable :: cell2nod     !< cell to "flownode" (>0), new triangle (<0), dim(numcells), note: numcells can be too large for array dimension

   double precision, dimension(:,:),   allocatable :: dnn          ! cell normal vector, dim(3,numcells), for spherical models
   double precision, dimension(:,:),   allocatable :: dnx          !< x-coordinate of edge normal vector (positive outward for edge2cell(1,:), positive inward for edge2cell(2,:)), dim(1,numedges) for 2D Cartesion, dim(2,numedges) for spherical models
   double precision, dimension(:,:),   allocatable :: dny          !< y-coordinate of edge normal vector (positive outward for edge2cell(1,:), positive inward for edge2cell(2,:)), dim(1,numedges) for 2D Cartesion, dim(2,numedges) for spherical models
   double precision, dimension(:,:),   allocatable :: dnz          !< y-coordinate of edge normal vector (positive outward for edge2cell(1,:), positive inward for edge2cell(2,:)), dim(2,numedges), for spherical models
   double precision, dimension(:),     allocatable :: w            !< edge width, dim(numedges)

   integer,                            parameter :: MAXSUBCELLS=10
end module m_partmesh

module m_partparallel
   double precision, dimension(:,:), allocatable :: worksnd, workrecv   ! work arrays for sending/receiving data

   integer,          dimension(:),   allocatable :: icellother   ! cell number in other domain
   integer,          dimension(:),   allocatable :: isend, jsend ! send list in CRS-format
   integer,          dimension(:),   allocatable :: jrecv        ! start pointers in receive list
   integer,          dimension(:),   allocatable :: jpoint       ! work array
   integer,          dimension(:),   allocatable :: irequest

   integer,          dimension(:), allocatable   :: numsendarr
   integer,          dimension(:), allocatable   :: numrecvarr

   integer                                       :: NDIM         ! data dimension per particle in send/receive arrays
   integer                                       :: INDX_XPART=1
   integer                                       :: INDX_YPART=2
   integer                                       :: INDX_DTREM=3
   integer                                       :: INDX_IGLOB=4
   integer                                       :: INDX_KPART=5
   integer                                       :: INDX_ZPART=0

   integer                                       :: japartsaved   ! particles saved to work array (1) or not (safety)
end module m_partparallel


!> Save data for later writing of Ugrid
module m_save_ugrid_state

   use meshdata

   type(t_ug_meshgeom)                                :: meshgeom1d
   character(len=ug_idsLen),allocatable               :: nbranchids(:), nnodeids(:), nodeids(:)
   character(len=ug_idsLongNamesLen), allocatable     :: nbranchlongnames(:), nnodelongnames(:), nodelongnames(:)
   character(len=255)                                 :: network1dname, mesh2dname, mesh1dname, contactname !MAXSTRLEN = 255
   character(len=ug_idsLen), allocatable              :: mesh1dNodeIds(:)
   integer, allocatable, dimension(:)                 :: mesh1dUnmergedToMerged(:)
   !integer, allocatable, dimension(:)                 :: mesh1dMergedToUnMerged(:)
   integer                                            :: numMesh1dBeforeMerging

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_save_ugrid_state() instead.
contains
   subroutine default_save_ugrid_state()
      implicit none
      call reset_save_ugrid_state()
      network1dname = 'network1d'
      mesh1dname    = 'mesh1d'
      mesh2dname    = 'mesh2d'
      contactname   = 'contacts'
      numMesh1dBeforeMerging = 0
   end subroutine default_save_ugrid_state

   !> Resets only waves variables intended for a restart of flow simulation.
   !! Upon loading of new model/MDU, call default_save_ugrid_state() instead.
   subroutine reset_save_ugrid_state()
      implicit none
      if (allocated(mesh1dNodeIds)) deallocate(mesh1dNodeIds)
      if (allocated(mesh1dUnmergedToMerged)) deallocate(mesh1dUnmergedToMerged)
      !if (allocated(mesh1dMergedToUnMerged)) deallocate(mesh1dMergedToUnMerged)
   end subroutine reset_save_ugrid_state

end module m_save_ugrid_state
