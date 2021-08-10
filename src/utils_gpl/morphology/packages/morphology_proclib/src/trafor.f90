      subroutine trafor ( pmsa   , fl     , ipoint , increm , noseg  , &
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   , &
     &                    noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'TRAFOR' :: TRAFOR
!>\file
!>       Process: TraFor - Transport formula for one ISS or IBS fraction

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
!------------------------------------------------------------------------------
      use precision
      use sediment_basics_module
      use morphology_data_module
      !
      IMPLICIT NONE

!
!     Type                      Name          I/O Description
!
      real(4), dimension(*)  :: pmsa        !<I/O Process Manager System Array, window of routine to process library
      real(4), dimension(*)  :: fl          !< O  Array of fluxes made by this process in mass/volume/time
      integer, dimension(85) :: ipoint      !< I  Array of pointers in pmsa to get and store the data
      integer, dimension(85) :: increm      !< I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer                :: noseg       !< I  Number of computational elements in the whole model schematisation
      integer                :: noflux      !< I  Number of fluxes, increment in the fl array
      integer, dimension(4,*):: iexpnt      !< I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer, dimension(*)  :: iknmrk      !< I  Active-Inactive, Surface-water-bottom, see manual for use
      integer                :: noq1        !< I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer                :: noq2        !< I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer                :: noq3        !< I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer                :: noq4        !< I  Nr of exchanges in the bottom (bottom layers, specialist use only)
!
!------------------------------------------------------------------------------
!
!     Type                                  Name          I/O Description                                        Unit
!
      real(fp)                           :: UxEff2D     !< I  x-component of effective depth averaged velocity   (m/s)
      real(fp)                           :: UyEff2D     !< I  y-component of effective depth averaged velocity   (m/s)
      real(fp)                           :: UEff2D      !< I  effective depth averaged velocity magnitude        (m/s)
      real(fp)                           :: UxMor       !< I  x-component of velocity for morphology             (m/s)
      real(fp)                           :: UyMor       !< I  y-component of velocity for morphology             (m/s)
      real(fp)                           :: UMor        !< I  velocity magnitude for morphology                  (m/s)
      real(fp)                           :: zUMor       !< I  height above bed for morphology velocity           (m)
      real(fp)                           :: UxBed       !< I  x-component of velocity in lowest layer            (m/s)
      real(fp)                           :: UyBed       !< I  y-component of velocity in lowest layer            (m/s)
      real(fp)                           :: UBed        !< I  velocity in lowest layer                           (m/s)
      real(fp)                           :: zUBed       !< I  height above bed of velocity of lowest layer       (m)
      real(fp)                           :: UxDepAvg    !< I  x-component of depth averaged velocity             (m/s)
      real(fp)                           :: UyDepAvg    !< I  y-component of depth averaged velocity             (m/s)
      real(fp)                           :: UDepAvg     !< I  depth averaged velocity                            (m/s)
      real(fp)                           :: UStar       !< I  shear velocity u_star                              (m/s)
      real(fp)                           :: Tau         !< I  total bottom shear stress                          (N/m2)
      real(fp)                           :: TauAdd      !< I  extra shear stress                                 (-)
      real(fp)                           :: LocalDepth  !< I  depth from water surface to bottom of segment      (m)
      real(fp)                           :: RhoWater    !< I  density of water                                   (kg/m3)
      real(fp)                           :: Salinity    !< I  Salinity                                           (g/kg)
      real(fp)                           :: NatTemp     !< I  natural temperature                                (oC)
      real(fp)                           :: GRAV        !< I  Gravitational acceleration                         (m/s2)
      real(fp)                           :: VonKarman   !< I  Von Karman constant                                (-)
      real(fp)                           :: KinViscos   !< I  Kinematic viscosity                                (m2/s)
      real(fp)                           :: WaveHeight  !< I  calculated height of a wind induced wave           (m)
      real(fp)                           :: WaveLength  !< I  calculated length of a wind induced wave           (m)
      real(fp)                           :: WavePeriod  !< I  calculated period of a wind induced wave           (s)
      real(fp)                           :: WaveAngle   !< I  direction of waves                                 (deg)
      real(fp)                           :: UOrb        !< I  (peak) orbital velocity at bottom layer            (m/s)
      real(fp)                           :: RhoSolid    !< I  density of sediment fraction                       (kg/m3)
      integer                            :: TypeSed     !< I  type of sediment fraction (0=bedl, 1=noncoh, 2=coh)(-)
      real(fp)                           :: D50_frac    !< I  D50 of sediment fraction                           (m)
      real(fp)                           :: Dss_frac    !< I  Dss of sediment fraction                           (m)
      real(fp)                           :: D10_mix     !< I  D10 of sediment mixture                            (m)
      real(fp)                           :: D90_mix     !< I  D90 of sediment mixture                            (m)
      real(fp)                           :: conc        !< I  concentration of this fraction                     (gDM/m3)
      real(fp)                           :: fracMud     !< I  mud fraction in bed                                (-)
      real(fp)                           :: frac        !< I  mass fraction of this fraction in bed              (-)
      real(fp)                           :: hidexp      !< I  hiding and exposure factor for this fraction       (-)
      real(fp)                           :: Wsettle     !< I  settling velocity for this fraction                (m/s)
      real(fp)                           :: factcr      !< I  adjusting shields critical shear stress            (-)
      real(fp)                           :: cal_sus     !< I  calibration factor suspended load                  (-)
      real(fp)                           :: cal_bed     !< I  calibration factor bed load                        (-)
      real(fp)                           :: cal_susw    !< I  calibration factor suspended load due to waves     (-)
      real(fp)                           :: cal_bedw    !< I  calibration factor bed load due to waves           (-)
      real(fp)                           :: cal_espir   !< I  calibration factor spiral flow effect              (-)
      real(fp)                           :: SpirFloInt  !< I  spiral flow intensity                              (-)
      real(fp)                           :: SlopeX      !< I  x-component of slope in segment                    (m/m)
      real(fp)                           :: SlopeY      !< I  y-component of slope in segment                    (m/m)
      real(fp)                           :: ksRip       !< I  ripple roughness height                            (-)
      real(fp)                           :: z0cur       !< I  current related bed roughness z_0 height           (m)
      real(fp)                           :: z0rou       !< I  wave enhanced bed roughness z_0 height             (m)
      real(fp)                           :: TraFrm      !< I  transport formula number                           (-)
      real(fp)                           :: TraPar01    !< I  transport formula specific parameter 1             (-)
      real(fp)                           :: TraPar02    !< I  transport formula specific parameter 2             (-)
      real(fp)                           :: TraPar03    !< I  transport formula specific parameter 3             (-)
      real(fp)                           :: TraPar04    !< I  transport formula specific parameter 4             (-)
      real(fp)                           :: TraPar05    !< I  transport formula specific parameter 5             (-)
      real(fp)                           :: TraPar06    !< I  transport formula specific parameter 6             (-)
      real(fp)                           :: TraPar07    !< I  transport formula specific parameter 7             (-)
      real(fp)                           :: TraPar08    !< I  transport formula specific parameter 8             (-)
      real(fp)                           :: TraPar09    !< I  transport formula specific parameter 9             (-)
      real(fp)                           :: TraPar10    !< I  transport formula specific parameter 10            (-)
      real(fp)                           :: ITIME       !< I  DELWAQ time                                        (scu)
      real(fp)                           :: DELT        !< I  timestep for processes                             (d)
      real(fp)                           :: AuxSys      !< I  ratio between days and system clock                (scu/d)
      real(fp)                           :: fixfac      !< I  non-erodible layer factor                          (-)
!
      real(fp), parameter                :: day2sec = 86400.0_fp
!
      real(fp)                           :: SBCx        !< O  x-comp of bed-load due to currents                 (kg/m)
      real(fp)                           :: SBCy        !< O  y-comp of bed-load due to currents                 (kg/m)
      real(fp)                           :: SBWx        !< O  x-comp of bed-load due to waves                    (kg/m)
      real(fp)                           :: SBWy        !< O  y-comp of bed-load due to waves                    (kg/m)
      real(fp)                           :: SSWx        !< O  x-comp of susp load due to waves                   (kg/m)
      real(fp)                           :: SSWy        !< O  y-comp of susp load due to waves                   (kg/m)
      real(fp)                           :: AKS         !< O  Van Rijn's reference height                        (m)                 
      real(fp)                           :: CAKS        !< O  Equilibrium concentration at reference level aks   (kg/m3)             
      real(fp)                           :: TAURAT      !< O  bed shear stress ratio (cw over critical)          (-)                 
      real(fp)                           :: Conc2D      !< O  Concentration in 2D                                (kg/m3)             
      real(fp)                           :: caks_ss3d   !< O  caks_ss3d                                          (-)                 
      real(fp)                           :: aks_ss3d    !< O  aks_ss3d                                           (-)                 
      real(fp)                           :: USt2        !< O  UStar**2                                           (-)                 
      real(fp)                           :: T_relax     !< O  T_relax                                            (-)                 
      real(fp)                           :: DSS         !< O  Dss of ISS fraction output                         (m)                 
      real(fp)                           :: SourSe      !< O  SourSe for ISS frac                                (kg/m3/s)           
      real(fp)                           :: sour_im     !< O  SourSe for ISS frac (implicit part)                (kg/m3/s)           
      real(fp)                           :: SinkSe      !< O  SinkSe for ISS frac                                (1/s)               
!
      real(4)                            :: flxsrc      !< F  source flux for ISS frac                           (kg/m3/d)
      real(4)                            :: flxsnk      !< F  sink flux for ISS frac                             (kg/m3/d)
!
      integer                            :: iflux       !<    Local index for pointering the fluxes
      integer, dimension(85)             :: ipnt        !<    Local work array for the pointering
      integer                            :: iseg        !<    Local loop counter for computational element loop
!
      integer                            :: kmax        !<    number of layers
      integer, parameter                 :: kmax2d = 20 !<    number of layers to be used in 2D
      integer                            :: ltur        !<    turbulence model number
      integer                            :: lundia      !<    handle of diagnostic file
      integer                            :: i2d3d       !<    flag depth-averaged (2) or 3D (3)
      integer                            :: iform       !<    transport formula number (integer of TraFrm)
      integer                            :: istat       !<    error status flag
      integer                            :: segtype     !<    segment type (surface, column, bottom, all)
      integer(pntrsize)                  :: dllhandle   !<    handle for shared sediment transport formula library
      integer                            :: lsecfl      !<    1 if secondary flow included, 0 otherwise
      integer                            :: nsegments2D !<    number of segments (noseg) in one layer
      integer                            :: nlayers     !<    number of layers in delwaq model
      logical                            :: scour       !<    .TRUE. if AddTau included
      logical                            :: suspfrac    !<    .TRUE. if suspended sediment fraction
      logical                            :: ubot_from_com !<  .TRUE. if ubot should be used instead of local formula
      logical                            :: wave        !<    .TRUE. if waves included
      real(fp)                           :: chezy       !<    chezy number
      real(fp)                           :: dstar       !<    Dstar
      real(fp)                           :: taucr0      !<    critical shear stress
      real(fp)                           :: tetacr      !<    critical Shields stress
      real(fp)                           :: ee          !<    exp(1)                                             (-)
      real(fp)                           :: third       !<    1/3                                                (-)
      real(fp)                           :: Delta       !<    relative sediment density                          (-)
      real(fp)                           :: timsec      !<    time since simulation start in seconds             (s)
      character(256)                     :: dllfunc     !<    name of transport formula routine in shared library
      logical                            :: error       !<    .FALSE. Suppose there would be no error 
      logical                            :: oldmudfrac  !<    ignore frac factor for cohesive fractions
      logical                            :: flmd2l      !<    2 layer fluid mud model
!
      real(fp)                           :: srcmax      !<    maximum entrainment rate                           (kg/m2/s)
      real(fp)                           :: dzmax       !<    maximum bed level change as fraction of LocalDepth (-)
      real(fp)                           :: dz          !<    bed level change                                   (m)
      real(fp)                           :: wstau       !<    special sink term for flmd2l
      real(fp)                           :: reducfac    !<    reduction factor in case of too high erosion rate  (-)
      real(fp)                           :: sigmol      !<    Prandtl-Schmidt number                             (-)
      real(fp)                           :: thick0      !<    thickness of gridcell kmaxsd during entrainment    (m)
      real(fp)                           :: thick1      !<    thickness of gridcell kmaxsd during deposition     (m)
      real(fp)                           :: camax       !<    maximum sediment concentration (approx 1600/2650)  (-)
      real(fp)                           :: eps         !<    precision threshold                                (-)
      real(fp), dimension(:), allocatable:: sig         !<    sigma-level of centre of layer                     (-)
      real(fp), dimension(:), allocatable:: thick       !<    sigma-thickness of layer                           (-)
      real(fp), dimension(:), allocatable:: concin      !<    concentration vertical
      real(fp), dimension(:), allocatable:: ws          !<    settling velocity array (at interfaces)
      real(fp), dimension(:), allocatable:: dicww       !<    diffusion coefficient array for fluid
      real(fp), dimension(:), allocatable:: seddif      !<    diffusion coefficient array for sediment
      real(fp), dimension(:), allocatable:: rsedeq      !<    diffusion coefficient array
      integer                            :: active      !<    attribute 1 - segment active flag
      integer                            :: kmaxsd      !<    flag depth-averaged (2) or 3D (3)!
      integer                            :: k           !<    layer number
      integer                            :: kmaxloc     !<    local number of layers (less or equal kmax)
!
      integer                            :: iflufflyr
      real(fp)                           :: mflufftot
      real(fp)                           :: fracf
      real(fp)                           :: SinkTot
      real(fp)                           :: SourFl
!
      real(fp), dimension(:), allocatable:: par         !<    old array for real transport formula parameters
      real(hp), dimension(:), allocatable:: realpar     !<    real transport formula parameters
      integer, dimension(:), allocatable :: intpar      !<    integer transport formula parameters
      character(256), dimension(:), allocatable :: strpar !<    string transport formula parameters
!
!------------------------------------------------------------------------------
!
!     Initialise pointers
!
      ipnt        = ipoint
      iflux       = 0
      nsegments2D = noseg - noq3
      nlayers     = noseg / nsegments2D
      kmax        = nlayers
      if (nlayers==1) kmax = kmax2d
!
!     Initialize constants and arrays
!
      ee          = exp(1.0)
      third       = 1.0/3.0
      ITIME       = pmsa( ipnt( 65) ) ! TODO: check that increm(65) and increm(66) are 0
      DELT        = pmsa( ipnt( 66) ) ! TODO: check that increm(65) and increm(66) are 0
      AuxSys      = pmsa( ipnt( 67) )
      timsec      = ITIME * (day2sec/AuxSys) ! default value of AuxSys is day2sec, thus scu = s
      lundia      = 308 ! "MOR" diagnostic file
      istat       = 0
      if (istat==0) allocate(concin (kmax)  , stat=istat)
      if (istat==0) allocate(dicww  (0:kmax), stat=istat)
      if (istat==0) allocate(rsedeq (kmax)  , stat=istat)
      if (istat==0) allocate(seddif (0:kmax), stat=istat)
      if (istat==0) allocate(sig    (kmax)  , stat=istat)
      if (istat==0) allocate(thick  (kmax)  , stat=istat)
      if (istat==0) allocate(ws     (0:kmax), stat=istat)
      if (istat==0) allocate(par    (30)    , stat=istat)
      if (istat==0) allocate(realpar(MAX_RP), stat=istat)
      if (istat==0) allocate(intpar (MAX_IP), stat=istat)
      if (istat==0) allocate(strpar (MAX_SP), stat=istat)
      concin  = 0.0
      dicww   = 0.0
      rsedeq  = 0.0
      seddif  = 0.0
      sig     = 0.0
      thick   = 0.0
      ws      = 0.0
      par     = 0.0
      realpar = 0.0
      intpar  = 0
      strpar  = ' '
!
      if (nlayers == 1) then
         thick(1:kmax2d) = (/ 0.1747, 0.1449, 0.1202, 0.0997, 0.0827, 0.0686, 0.0569, 0.0472, &
           & 0.0391, 0.0325, 0.0269, 0.0223, 0.0185, 0.0154, 0.0127, 0.0106, 0.0088,&
           & 0.0073, 0.0060, 0.0050/)
         sig(1:kmax2d) = (/ - 0.0874, -0.2472, -0.3797, -0.4897, -0.5809, -0.6565, -0.7193, &
           & -0.7713, -0.8145, -0.8503, -0.8800, -0.9046, -0.9250, -0.9419, -0.9560,&
           & -0.9676, -0.9773, -0.9854, -0.9920, -0.9975/)
      endif
!
!     Loop over all segments
!
      do iseg = 1, noseg
         call dhkmrk(1, iknmrk(iseg), active)
         call dhkmrk(2, iknmrk(iseg), segtype)
         if (active==0 .or. (segtype /= 0 .and. segtype /= 3)) then ! 1 (3D at surface), 2 (3D between surface and bottom)
            fl  ( iflux +  1  ) = 0.0
            fl  ( iflux +  2  ) = 0.0
            !
            ! Increment pointers and continue
            !
            iflux       = iflux       + noflux
            ipnt        = ipnt        + increm
            cycle
         endif
!
!        Get input values from arrays
!
         UxEff2D     = real(pmsa( ipnt(  1) ),fp)
         UyEff2D     = real(pmsa( ipnt(  2) ),fp)
         UEff2D      = real(pmsa( ipnt(  3) ),fp)
         UxMor       = real(pmsa( ipnt(  4) ),fp)
         UyMor       = real(pmsa( ipnt(  5) ),fp)
         UMor        = real(pmsa( ipnt(  6) ),fp)
         zUMor       = real(pmsa( ipnt(  7) ),fp)
         UxBed       = real(pmsa( ipnt(  8) ),fp)
         UyBed       = real(pmsa( ipnt(  9) ),fp)
         UBed        = real(pmsa( ipnt( 10) ),fp)
         zUBed       = real(pmsa( ipnt( 11) ),fp)
         UxDepAvg    = real(pmsa( ipnt( 12) ),fp)
         UyDepAvg    = real(pmsa( ipnt( 13) ),fp)
         UDepAvg     = real(pmsa( ipnt( 14) ),fp)
         UStar       = real(pmsa( ipnt( 15) ),fp)
         Tau         = real(pmsa( ipnt( 16) ),fp)
         TauAdd      = real(pmsa( ipnt( 17) ),fp)
         LocalDepth  = real(pmsa( ipnt( 18) ),fp)
         RhoWater    = real(pmsa( ipnt( 19) ),fp)
         Salinity    = real(pmsa( ipnt( 20) ),fp)
         NatTemp     = real(pmsa( ipnt( 21) ),fp)
         GRAV        = real(pmsa( ipnt( 22) ),fp)
         VonKarman   = real(pmsa( ipnt( 23) ),fp)
         KinViscos   = real(pmsa( ipnt( 24) ),fp)
         WaveHeight  = real(pmsa( ipnt( 25) ),fp)
         wave        = increm(25)>0 .or. WaveHeight>0.0_fp
         WaveLength  = real(pmsa( ipnt( 26) ),fp)
         WavePeriod  = real(pmsa( ipnt( 27) ),fp)
         WaveAngle   = real(pmsa( ipnt( 28) ),fp)
         UOrb        = real(pmsa( ipnt( 29) ),fp)
         RhoSolid    = real(pmsa( ipnt( 30) ),fp)
         TypeSed     = int(pmsa( ipnt( 31) ))
         D50_frac    = real(pmsa( ipnt( 32) ),fp)
         if (TypeSed==1) then
            Dss_frac = real(pmsa( ipnt( 33) ),fp)
         else
            Dss_frac = 0.0
         endif
         D10_mix     = real(pmsa( ipnt( 34) ),fp)
         D90_mix     = real(pmsa( ipnt( 35) ),fp)
         if (TypeSed==0) then
            conc     = 0.0
         else
            conc     = real(pmsa( ipnt( 36) ),fp)
         endif
         fracMud     = real(pmsa( ipnt( 37) ),fp)
         frac        = real(pmsa( ipnt( 38) ),fp)
         hidexp      = real(pmsa( ipnt( 39) ),fp)
         if (TypeSed==0) then
            Wsettle  = 0.0
         else
            Wsettle  = real(pmsa( ipnt( 40) ),fp)
         endif
         factcr      = real(pmsa( ipnt( 41) ),fp)
         cal_sus     = real(pmsa( ipnt( 42) ),fp)
         cal_bed     = real(pmsa( ipnt( 43) ),fp)
         cal_susw    = real(pmsa( ipnt( 44) ),fp)
         cal_bedw    = real(pmsa( ipnt( 45) ),fp)
         cal_espir   = real(pmsa( ipnt( 46) ),fp)
         SpirFloInt  = real(pmsa( ipnt( 47) ),fp)
         SlopeX      = real(pmsa( ipnt( 48) ),fp)
         SlopeY      = real(pmsa( ipnt( 49) ),fp)
         ksRip       = real(pmsa( ipnt( 50) ),fp)
         z0cur       = real(pmsa( ipnt( 51) ),fp)
         z0rou       = real(pmsa( ipnt( 52) ),fp)
         TraFrm      = real(pmsa( ipnt( 53) ),fp)
         TraPar01    = real(pmsa( ipnt( 54) ),fp)
         TraPar02    = real(pmsa( ipnt( 55) ),fp)
         TraPar03    = real(pmsa( ipnt( 56) ),fp)
         TraPar04    = real(pmsa( ipnt( 57) ),fp)
         TraPar05    = real(pmsa( ipnt( 58) ),fp)
         TraPar06    = real(pmsa( ipnt( 59) ),fp)
         TraPar07    = real(pmsa( ipnt( 60) ),fp)
         TraPar08    = real(pmsa( ipnt( 61) ),fp)
         TraPar09    = real(pmsa( ipnt( 62) ),fp)
         TraPar10    = real(pmsa( ipnt( 63) ),fp)
         !ITIME       = real(pmsa( ipnt( 64) ),fp)
         !DELT        = real(pmsa( ipnt( 65) ),fp)
         !AuxSys      = real(pmsa( ipnt( 66) ),fp)
         fixfac      = real(pmsa( ipnt( 67) ),fp)
!
!   *****     Insert your code here  *****
!
         if (segtype == 0) then
            !
            ! single segment in vertical: 2D (might be 2D cell in 3D Z-layer model: do I want to do this then?)
            !
            i2d3d  = 2
            ws     = Wsettle
            dicww  = 0.0
            concin = conc
         elseif (segtype == 3) then ! bottom segment 
            !
            ! bottom segment in vertical with multiple segments: 3D
            !
            i2d3d  = 3
         endif
         !
         if (nlayers>1) then
            sig     = LocalDepth
            !
            ! determine the depth for the bottom of every layer
            !
            kmaxloc = 1 + (iseg-1)/nsegments2D
            do k = kmaxloc-1,1,-1
               sig(k)    = real(pmsa( ipnt( 18) - increm(18)*nsegments2D*(kmaxloc-k) ),fp)
               concin(k) = real(pmsa( ipnt( 36) - increm(36)*nsegments2D*(kmaxloc-k) ),fp)
               ws(k)     = real(pmsa( ipnt( 40) - increm(40)*nsegments2D*(kmaxloc-k) ),fp)
            enddo
            !
            ! determine for every layer the relative thickness
            !
            thick(1) = sig(1)/LocalDepth
            do k = 2, kmax
               thick(k) = (sig(k)-sig(k-1))/LocalDepth
            enddo
            !
            ! determine for the mid point of every layer the relative depth
            !
            do k = 1, kmax
               sig(k) = -sig(k)/LocalDepth + thick(k)/2.0
            enddo
         endif
         !
         iform         = int(TraFrm)
         chezy         = sqrt(GRAV) * log( 1.0 + LocalDepth/max(1.0e-8,ee*z0rou) ) / VonKarman
         Delta         = (RhoSolid - RhoWater) / RhoWater
         dstar         = D50_frac * (Delta * GRAV / KinViscos**2)**third
         if (dstar < 1.0_fp) then
            if (iform == -2) then
               tetacr = 0.115 / (dstar**0.5_fp)
            else
               tetacr = 0.24 / dstar
            endif
         elseif (dstar <= 4.0_fp) then
            if (iform == -2) then
               tetacr = 0.115 / (dstar**0.5)
            else
               tetacr = 0.24 / dstar
            endif
         elseif (dstar>4.0_fp .and. dstar<=10.0_fp) then
            tetacr = 0.14  / (dstar**0.64)
         elseif (dstar>10.0_fp .and. dstar<=20.0_fp) then
            tetacr = 0.04  / (dstar**0.1)
         elseif (dstar>20.0 .and. dstar<=150.0_fp) then
            tetacr = 0.013 * (dstar**0.29)
         else
            tetacr = 0.055
         endif
         taucr0        = factcr * (RhoSolid-RhoWater) * GRAV * D50_frac * tetacr
         !
         ltur          = 0
         sigmol        = 1.0_fp
         lsecfl        = 1
         suspfrac      = TypeSed>0
         scour         = TauAdd>0.0
         ubot_from_com = .true.
         camax         = 0.65_fp
         eps           = 1e-6_fp
         !
         realpar(RP_TIME ) = real(timsec     ,hp)
         realpar(RP_EFUMN) = real(UxEff2D    ,hp)
         realpar(RP_EFVMN) = real(UyEff2D    ,hp)
         realpar(RP_EFVLM) = real(UEff2D     ,hp)
         realpar(RP_UCHAR) = real(UxMor      ,hp)
         realpar(RP_VCHAR) = real(UyMor      ,hp)
         realpar(RP_VELCH) = real(UMor       ,hp)
         realpar(RP_ZVLCH) = real(zUMor      ,hp)
         realpar(RP_DEPTH) = real(LocalDepth ,hp)
         realpar(RP_CHEZY) = real(chezy      ,hp)
         realpar(RP_HRMS ) = real(WaveHeight ,hp)
         realpar(RP_TPEAK) = real(WavePeriod ,hp)
         realpar(RP_TETA ) = real(WaveAngle  ,hp)
         realpar(RP_RLAMB) = real(WaveLength ,hp)
         realpar(RP_UORB ) = real(UOrb       ,hp)
         realpar(RP_D50  ) = real(D50_frac   ,hp)
         realpar(RP_DSS  ) = real(Dss_frac   ,hp)
         realpar(RP_DSTAR) = real(dstar      ,hp)
         realpar(RP_D10MX) = real(D10_mix    ,hp)
         realpar(RP_D90MX) = real(D90_mix    ,hp)
         realpar(RP_MUDFR) = real(fracMud    ,hp)
         realpar(RP_HIDEX) = real(hidexp     ,hp)
         realpar(RP_SETVL) = real(Wsettle    ,hp)
         realpar(RP_RHOSL) = real(RhoSolid   ,hp)
         realpar(RP_RHOWT) = real(RhoWater   ,hp)
         realpar(RP_SALIN) = real(Salinity   ,hp)
         realpar(RP_TEMP ) = real(NatTemp    ,hp)
         realpar(RP_GRAV ) = real(GRAV       ,hp)
         realpar(RP_VICML) = real(KinViscos  ,hp)
         realpar(RP_TAUB ) = real(Tau        ,hp) ! should be increased by TauAdd
         realpar(RP_UBED ) = real(UxBed      ,hp)
         realpar(RP_VBED ) = real(UyBed      ,hp)
         realpar(RP_VELBD) = real(UBed       ,hp)
         realpar(RP_ZVLBD) = real(zUBed      ,hp)
         realpar(RP_VNKAR) = real(VonKarman  ,hp)
         realpar(RP_Z0CUR) = real(z0cur      ,hp)
         realpar(RP_Z0ROU) = real(z0rou      ,hp)
         realpar(RP_UMEAN) = real(UxDepAvg   ,hp)
         realpar(RP_VMEAN) = real(UyDepAvg   ,hp)
         realpar(RP_VELMN) = real(UDepAvg    ,hp)
         realpar(RP_USTAR) = real(UStar      ,hp)
         !
         par( 1) = GRAV
         par( 2) = RhoWater
         par( 3) = RhoSolid
         par( 4) = Delta
         par( 5) = KinViscos
         par( 6) = D50_frac
         par(11) = TraPar01
         par(12) = TraPar02
         par(13) = TraPar03
         par(14) = TraPar04
         par(15) = TraPar05
         par(16) = TraPar06
         par(17) = TraPar07
         par(18) = TraPar08
         par(19) = TraPar09
         par(20) = TraPar10
         !
         aks       = -999.0_fp
         caks      = -999.0_fp
         taurat    = -999.0_fp
         seddif    = -999.0_fp
         rsedeq    = -999.0_fp
         kmaxsd    = -999
         conc2d    = -999.0_fp
         SBCx      = -999.0_fp
         SBCy      = -999.0_fp
         SBWx      = -999.0_fp
         SBWy      = -999.0_fp
         SSWx      = -999.0_fp
         SSWy      = -999.0_fp
         dss       = -999.0_fp
         caks_ss3d = -999.0_fp
         aks_ss3d  = -999.0_fp
         ust2      = -999.0_fp
         T_relax   = -999.0_fp
         error     = .false.
         !
         thick0 = thick(kmaxsd) * LocalDepth
         thick1 = thick0 ! deposition term treated explicitly in delwaq
         !
         if (TypeSed<2) then
            ! non-cohesive sediment fraction
            call eqtran(sig       ,thick     ,kmax      ,ws        ,ltur      , &
                      & frac      ,sigmol    ,dicww     ,lundia    ,taucr0    , &
                      & ksRip     ,i2d3d     ,lsecfl    ,SpirFloInt,suspfrac  , &
                      & tetacr    ,concin    , &
                      & SlopeX    ,SlopeY    ,UOrb      ,TauAdd    ,cal_sus   , &
                      & cal_bed   ,cal_susw  ,cal_bedw  ,cal_espir ,wave      , &
                      & scour     ,ubot_from_com        ,camax     ,eps       , &
                      & iform     ,par       ,MAX_IP    ,MAX_RP    ,MAX_SP    , &
                      & dllfunc   ,dllhandle ,intpar    ,realpar   ,strpar    , &
!output:
                      & aks       ,caks      ,taurat    ,&  ! output
                      & seddif    ,rsedeq    , &            ! local variables in array
                      & kmaxsd    ,&                        ! local variables
                      & conc2d    ,SBCx      ,SBCy      ,SBWx      , &
                      & SBWy      ,SSWx      ,SSWy      ,dss       ,caks_ss3d , &
                      & aks_ss3d  ,ust2      ,T_relax   ,error     )
            !       
            if (suspfrac) then
               ! suspended load
               if (kmax == 1) then
                  ! 2D model
                  call soursin_2d(UEff2D    ,UStar    ,LocalDepth ,LocalDepth, &
                                & Wsettle   ,T_relax  ,rsedeq(1)  ,1.0_fp,   &
!output
                                & SourSe    ,sour_im  ,SinkSe   )
               else
                  ! 3D model
                  call soursin_3d(LocalDepth    , thick0  , thick1    , sig(kmaxsd), &
                                & thick(kmaxsd) , conc    , KinViscos , sigmol     , &
                                & seddif(kmaxsd), RhoSolid, caks_ss3d , ws(kmaxsd) , &
                                & aks_ss3d      , &
!output
                                & SourSe        , sour_im , SinkSe     )
               endif
               !
               !dz = (SourSe*thick0 - SinkSe*conc*thick1)*DELT*day2sec*MorFac/RhoBed
               !if (dz > dzmax*LocalDepth) then
               !   reducfac = (LocalDepth*dzmax)/abs(dz)
               !   SinkSe = SinkSe*reducfac
               !   SourSe = SourSe*reducfac
               !endif
               !
               SinkSe = SinkSe*fixfac
               SourSe = SourSe*fixfac
               do k = 1, kmax
                  rsedeq = rsedeq*fixfac
               enddo
            else
               ! bedload/total load
               SourSe = 0.0_fp
               SinkSe = 0.0_fp
            endif
         else
            ! cohesive sediment fraction
            oldmudfrac = .false.
            flmd2l     = .false.
            srcmax     = 1.0e10_fp ! not yet supported
            !
            !if (flmd2l) then
            !   par(1) = entr(nm,l)
            !   par(2) = tcrdep(nm,l)
            !endif
            !
            iflufflyr = 0
            mflufftot = 0.0_fp
            fracf     = 0.0_fp
            call erosilt(thick    ,kmax      ,ws       ,lundia   , &
                       & thick0   ,thick1    ,fixfac   ,srcmax   , &
                       & frac     ,oldmudfrac,flmd2l   ,iform    , &
                       & par      ,MAX_IP    ,MAX_RP   ,MAX_SP   , &
                       & dllfunc  ,dllhandle ,intpar   ,realpar  , &
                       & strpar   ,iflufflyr ,mflufftot,fracf    , &
                       & 0.0_fp   ,0.0_fp    , &
! output:
                       & error    ,wstau     ,SinkTot  ,SourSe   , &
                       & SourFl   )
            SinkSe = SinkTot
         endif
         !
         !if (sourse > 0.0) then
         !   write(iseg,'(42E13.5)') realpar
         !   write(iseg,'(30E13.5)') par
         !   write(iseg,'(4E13.5)') conc2d    ,SBCx      ,SBCy      ,SBWx     
         !   write(iseg,'(5E13.5)') SBWy      ,SSWx      ,SSWy      ,dss       ,caks_ss3d 
         !   write(iseg,'(5E13.5)') aks_ss3d  ,ust2      ,T_relax   ,SourSe    ,SinkSe 
         !   write(iseg,'(A)') '--'
         !endif
         !
         ! The source/sink terms are computed as kg/s, but we need g/d for delwaq
         !
         flxsrc = SourSe*day2sec*1000.0_fp
         flxsnk = SinkSe*conc*day2sec*1000.0_fp
        
!
!   *****     End of your code       *****
!
!        Put output and flux values into arrays
!
         fl  ( iflux +  1  ) = flxsrc
         fl  ( iflux +  2  ) = flxsnk
         pmsa( ipnt( 68)   ) = real(SBCx     ,4)
         pmsa( ipnt( 69)   ) = real(SBCy     ,4)
         pmsa( ipnt( 70)   ) = real(SBWx     ,4)
         pmsa( ipnt( 71)   ) = real(SBWy     ,4)
         pmsa( ipnt( 72)   ) = real(SSWx     ,4)
         pmsa( ipnt( 73)   ) = real(SSWy     ,4)
         pmsa( ipnt( 74)   ) = real(AKS      ,4) 
         pmsa( ipnt( 75)   ) = real(CAKS     ,4) 
         pmsa( ipnt( 76)   ) = real(TAURAT   ,4) 
         pmsa( ipnt( 77)   ) = real(Conc2D   ,4) 
         pmsa( ipnt( 78)   ) = real(caks_ss3d,4) 
         pmsa( ipnt( 79)   ) = real(aks_ss3d ,4) 
         pmsa( ipnt( 80)   ) = real(USt2     ,4) 
         pmsa( ipnt( 81)   ) = real(T_relax  ,4) 
         pmsa( ipnt( 82)   ) = real(DSS      ,4)  
         pmsa( ipnt( 83)   ) = real(SourSe   ,4)
         pmsa( ipnt( 84)   ) = real(SinkSe   ,4)
!
!        Increment pointers
!
         iflux       = iflux       + noflux
         ipnt        = ipnt        + increm
!
      enddo
!
      deallocate(concin , stat=istat)
      deallocate(dicww  , stat=istat)
      deallocate(rsedeq , stat=istat)
      deallocate(seddif , stat=istat)
      deallocate(sig    , stat=istat)
      deallocate(thick  , stat=istat)
      deallocate(ws     , stat=istat)
      deallocate(realpar, stat=istat)
      deallocate(intpar , stat=istat)
      deallocate(strpar , stat=istat)
!
      end subroutine trafor
