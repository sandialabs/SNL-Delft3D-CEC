module m_GlobalParameters
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2020.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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
!  $Id: GlobalParameters.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/GlobalParameters.f90 $
!-------------------------------------------------------------------------------
   
   use MessageHandling
   use ModelTypes
   use m_node
   
   
   public
   
   double precision                 :: gravity                       = 9.81d0
   double precision                 :: sqrtg                         = 3.132d0
   logical                          :: anyGroundLayer                = .false.
   logical                          :: anySummerDike                 = .false.
   logical                          :: readNetworkFromUgrid          = .false.
   logical                          :: writeNetCDF                   = .false.
   logical                          :: useImplicitSolver             = .false.
   logical                          :: fillCulvertsWithGL            = .false.
   logical                          :: doReadCache                   = .false.
   logical                          :: doWriteCache                  = .false.
   double precision                 :: thresholdDry                  = 0.001d0
   double precision                 :: thresholdFlood                = 0.01d0
   double precision                 :: factorFloodingDividedByDrying = 10.0d0
   double precision                 :: summerDikeTransitionHeight    = 0.50d0
   double precision                 :: summerDikeThreshold           = 0.4d+0
   double precision                 :: ThresholdSiphon               = 0.1d0
   double precision                 :: StructureDynamicsFactor       = 1.0d0
   double precision                 :: strucalfa                     = 0.9d0
   double precision                 :: sl                            = 0.01d0        !< width at top of closed profile (Preisman lock)
   double precision                 :: pi                            = 3.141592653589793d0
   double precision                 :: ThresholdForPreismannLock     = 0.02d0
   double precision                 :: minSectionLength              = 1.0
   double precision, public         :: dynstructext                  = 1.0d0
   double precision, public         :: missingvalue                  =-999.999d0
   double precision, public         :: latitude                      = 52.25
   double precision, public         :: longitude                     = 0d0
   double precision, public         :: time_zone                     = 0d0
   integer, public                  :: maxlenpar                     = 100000   
   !TODO temporary unit to be removed, when finished
   integer, public :: luntrans = 0

   ! storage table controls
   double precision, public      :: tb_inc = 0.01
   double precision, public      :: tb_extra_height = 0.0
   character(len=charln), public :: st_filename
   logical, public               :: write_tables

   
   character(len=charln)            :: wlevStateFileIn
   character(len=charln)            :: wlevStateFileOut
   
   character(len=20)                :: obsIntPolType

   type(ModelType)                  :: modelTimeStepData
   
   integer, parameter  :: ADV_UPWIND  = 1
   integer, parameter  :: ADV_VANLEER = 2
   
   integer, parameter  :: DENS_ECKART_MODIFIED = 1
   integer, parameter  :: DENS_ECKART          = 2
   integer, parameter  :: DENS_UNESCO          = 3
   
   type, public :: t_chainage2cross
      integer :: c1 = -1           !< cross section index 1
      integer :: c2 = -1           !< cross section index 2
      double precision :: f        !< fraction: c_loc = f * c1 + (1-f)*c2
      double precision :: distance !< geometric distance between two cross sections
   end type
 
   type t_filenames
      character(len=255) :: onednetwork                  = ' ' !< 1d Network definition             (e.g., flow1d.md1d)
      character(len=255) :: cross_section_definitions    = ' ' !< 1d cross section definitions
      character(len=255) :: cross_section_locations      = ' ' !< 1d cross section locations
      character(len=1024):: roughness                    = ' ' !< 1d roughness files
      character(len=255) :: roughnessdir                 = ' ' !< location of roughness files
      character(len=255) :: storage_nodes                = ' ' !< 1d cross section retention manhole definitions
      character(len=255) :: structures                   = ' ' !< structure file
   end type

   
   type t_constituent_helper
      integer              :: boundary_index
      integer              :: initial_values_index
      character(len=idLen) :: name
      double precision     :: default
   end type t_constituent_helper
   
   type t_transport
      logical                         :: do_salt
      logical                         :: do_temp
      integer                         :: salt_index
      integer                         :: temp_index
      integer                         :: constituents_count
      integer                         :: density
      double precision                :: teta
      
      !data for branch own mouth relations
      type(t_node), pointer           :: mouth
      double precision                :: tidal_period
      double precision                :: start_time_tidal_period    !< start time of this tidal period
      double precision                :: rho_fresh_water            !< reference density of sea water
      logical                         :: start_tidal_period         !< indicates whether the first tidal period has started.
      logical                         :: first_tidal_period         !< indicates whether this is the first tidal period has started.
      integer                         :: dis_loc                    !< location of discharge point near mouth 
      integer                         :: dis_dir                    !< direction of discharge point if dis_dir*Q > 0 then flow is into the model  
      logical                         :: use_f4_dispersion          !< logical indicating, whether f4 dispersion is to be used 
      logical, allocatable, dimension(:) :: use_f4_dispersion_for_branch !< logical indicating, whether f4 dispersion is defined on this branch

      double precision                :: n
      double precision, dimension(11) :: c

      integer                                  :: advection_scheme
      type(t_constituent_helper), dimension(2) :: co_h 
   end type
   type(t_transport), target                   :: transportPars
   
   ! Structure Types
   integer, public, parameter              :: ST_UNSET      = -1
   integer, public, parameter              :: ST_WEIR       =  2
   integer, public, parameter              :: ST_ORIFICE    =  3
   integer, public, parameter              :: ST_PUMP       =  4
   integer, public, parameter              :: ST_GATE       =  5
   integer, public, parameter              :: ST_GENERAL_ST =  6
   integer, public, parameter              :: ST_UNI_WEIR   =  7
   integer, public, parameter              :: ST_DAMBREAK   =  8
   integer, public, parameter              :: ST_CULVERT    =  9
   integer, public, parameter              :: ST_BRIDGE     = 10
   integer, public, parameter              :: ST_COMPOUND   = 11
   integer, public, parameter              :: ST_MAX_TYPE   = 11 !< Max id of structure types. The preceding ids must be lower than this.

   ! Flow geometry / computational grid
   integer, public, parameter              :: INDTP_1D      = 1  !< Type code for flow nodes that are 1D
   integer, public, parameter              :: INDTP_2D      = 2  !< Type code for flow nodes that are 2D
   integer, public, parameter              :: INDTP_ALL     = 3  !< Type code for flow nodes that are 1D or 2D

   ! element set integer ids
   integer, public, parameter :: CFiBranchNodes             = 1
   integer, public, parameter :: CFiBranchLinks             = 2
   integer, public, parameter :: CFiUniqueNodesElementSet   = 3
   integer, public, parameter :: CFiGridpointsOnBranches    = 4
   integer, public, parameter :: CFiReachSegElmSet          = 5
   integer, public, parameter :: CFiStructures              = 6
   integer, public, parameter :: CFiPumps                   = 7
   integer, public, parameter :: CFiControllers             = 8
   integer, public, parameter :: CFiHBoundaries             = 9
   integer, public, parameter :: CFiQBoundaries             = 10
   integer, public, parameter :: CFiLaterals                = 11
   integer, public, parameter :: CFiMeasurements            = 12
   integer, public, parameter :: CFiBranches                = 13
   integer, public, parameter :: CFiObservationpoints       = 14
   integer, public, parameter :: CFiStructuresAndPumps      = 15
   integer, public, parameter :: CFiStorageNodes            = 16
   integer, public, parameter :: CFiVolumesOnReachSegments  = 17
   integer, public, parameter :: CFiVolumesOnGridPoints     = 18
   integer, public, parameter :: CFiLateralsOnReachSegments = 19
   integer, public, parameter :: CFiLateralsOnGridPoints    = 20
   integer, public, parameter :: CFiModelWide               = 21
   integer, public, parameter :: CFiCrossSection            = 22
   integer, public, parameter :: CFiBoundaries              = 23


   ! quantity integer ids
   integer, public, parameter :: CFiWaterlevel              = 1
   integer, public, parameter :: CFiWaterDepth              = 2
   integer, public, parameter :: CFiBedlevel                = 3
   integer, public, parameter :: CFiSurfaceArea             = 4
   integer, public, parameter :: CFiVolume                  = 5
   integer, public, parameter :: CFiSalinity                = 6
   integer, public, parameter :: CFiDispersion              = 7
   integer, public, parameter :: CFiDischarge               = 8
   integer, public, parameter :: CFiVelocity                = 9
   integer, public, parameter :: CFiFlowarea                = 10
   integer, public, parameter :: CFiFlowperi                = 11
   integer, public, parameter :: CFiFlowhydrad              = 12
   integer, public, parameter :: CFiFlowconv                = 13
   integer, public, parameter :: CFiFlowchezy               = 14
   integer, public, parameter :: CFiTotalarea               = 15
   integer, public, parameter :: CFiTotalwidth              = 16
   integer, public, parameter :: CFiHyddepth                = 17
   integer, public, parameter :: CFiWaterlevelUp            = 23
   integer, public, parameter :: CFiWaterlevelDown          = 24
   integer, public, parameter :: CFiHead                    = 25
   integer, public, parameter :: CFiPressureDifference      = 26
   integer, public, parameter :: CFiPumpCapacity            = 27
   integer, public, parameter :: CFiFlux                    = 28
   integer, public, parameter :: CFiXcor                    = 30
   integer, public, parameter :: CFiYcor                    = 31
   integer, public, parameter :: CFiWindShield              = 32
   integer, public, parameter :: CFiNoIteration             = 33
   integer, public, parameter :: CFiNegativeDepth           = 34
   integer, public, parameter :: CFiTimeStepEstimation      = 35
   integer, public, parameter :: CFiWaterLevelAtCrest       = 36
   integer, public, parameter :: CFiWaterLevelGradient      = 37
   integer, public, parameter :: CFiFroude                  = 38
   integer, public, parameter :: CFiDischargeMain           = 39
   integer, public, parameter :: CFiDischargeFP1            = 40
   integer, public, parameter :: CFiDischargeFP2            = 41
   integer, public, parameter :: CFiChezyMain               = 42
   integer, public, parameter :: CFiChezyFP1                = 43
   integer, public, parameter :: CFiChezyFP2                = 44
   integer, public, parameter :: CFiAreaMain                = 45
   integer, public, parameter :: CFiAreaFP1                 = 46
   integer, public, parameter :: CFiAreaFP2                 = 47
   integer, public, parameter :: CFiWidthMain               = 48
   integer, public, parameter :: CFiWidthFP1                = 49
   integer, public, parameter :: CFiWidthFP2                = 50
   integer, public, parameter :: CFiHydradMain              = 51
   integer, public, parameter :: CFiHydradFP1               = 52
   integer, public, parameter :: CFiHydradFP2               = 53
   integer, public, parameter :: CFiCellLength              = 54
   integer, public, parameter :: CFiLateral                 = 55
   integer, public, parameter :: CFiFiniteVolumeGridIndex   = 56
   integer, public, parameter :: CFiLateralIndex            = 57
   integer, public, parameter :: CFiReservedFiniteGridType  = 58
   integer, public, parameter :: CFiDischargeDemanded       = 59
   integer, public, parameter :: CFiTH_F1                   = 60             ! Thatcher-Harleman coefficient F1
   integer, public, parameter :: CFiTH_F3                   = 61             ! Thatcher-Harleman coefficient F3
   integer, public, parameter :: CFiTH_F4                   = 62             ! Thatcher-Harleman coefficient F4
   integer, public, parameter :: CFiDensity                 = 63
   integer, public, parameter :: CFiLateralDefined          = 64
   integer, public, parameter :: CFiLateralDifference       = 65
   integer, public, parameter :: CFiEnergyLevels            = 66
   integer, public, parameter :: CFiBalBoundariesIn         = 67
   integer, public, parameter :: CFiBalBoundariesOut        = 68
   integer, public, parameter :: CFiBalBoundariesTot        = 69
   integer, public, parameter :: CFiBalError                = 70
   integer, public, parameter :: CFiBalLatIn                = 71
   integer, public, parameter :: CFiBalLatOut               = 72
   integer, public, parameter :: CFiBalLatTot               = 73
   integer, public, parameter :: CFiBalStorage              = 74
   integer, public, parameter :: CFiBalVolume               = 75
   integer, public, parameter :: CFiCSLevels                = 76             ! Cross section depth (dpw)
   integer, public, parameter :: CFiCSFlowWidth             = 77             ! Cross section Flow width (wiu)
   integer, public, parameter :: CFiCSTotalWidth            = 78             ! Cross section Total width (wi)
   integer, public, parameter :: CFiQZeta_1d2d              = 79
   integer, public, parameter :: CFiQLat_1d2d               = 80
   integer, public, parameter :: CFiQTotal_1d2d             = 81
   integer, public, parameter :: CFiBal2d1dIn               = 82
   integer, public, parameter :: CFiBal2d1dOut              = 83
   integer, public, parameter :: CFiBal2d1dTot              = 84
   integer, public, parameter :: CFiConvLength              = 85
   integer, public, parameter :: CFiTemperature             = 86
   integer, public, parameter :: CFiSuctionSideLevel        = 87
   integer, public, parameter :: CFiDeliverySideLevel       = 88
   integer, public, parameter :: CFiPumpStage               = 89
   integer, public, parameter :: CFiReductionFactor         = 90
   integer, public, parameter :: CFiTotalHeatFlux           = 91
   integer, public, parameter :: CFiRadFluxClearSky         = 92
   integer, public, parameter :: CFiHeatLossConv            = 93
   integer, public, parameter :: CFiNetSolarRad             = 94
   integer, public, parameter :: CFiEffectiveBackRad        = 95
   integer, public, parameter :: CFiHeatLossEvap            = 96
   integer, public, parameter :: CFiHeatLossForcedEvap      = 97
   integer, public, parameter :: CFiHeatLossFreeEvap        = 98
   integer, public, parameter :: CFiHeatLossForcedConv      = 99
   integer, public, parameter :: CFiHeatLossFreeConv        = 100
   integer, public, parameter :: CFimomAdvection            = 101
   integer, public, parameter :: CFimomPressure             = 102
   integer, public, parameter :: CFimomAcceleration         = 103
   integer, public, parameter :: CFimomBedStress            = 104
   integer, public, parameter :: CFimomLosses               = 105
   integer, public, parameter :: CFimomLateralCorrection    = 106
   integer, public, parameter :: CFimomWindStress           = 107
   integer, public, parameter :: CFiChangeArea              = 108
   integer, public, parameter :: CFiMeanBedLevelMain        = 109
   integer, public, parameter :: CFiAdaptedCrossSec         = 110
   integer, public, parameter :: CFiCumChangeArea           = 111
   integer, public, parameter :: CFiIntSediTrans            = 112
   integer, public, parameter :: CFiGrainSizeD50            = 113
   integer, public, parameter :: CFiGrainSizeD90            = 114
   integer, public, parameter :: CFiSedimentTransport       = 115
   integer, public, parameter :: CFiSedimentTransportLeft   = 116
   integer, public, parameter :: CFiSedimentTransportRight  = 117
   integer, public, parameter :: CFiShieldsParameter        = 118
   integer, public, parameter :: CFiMorWaterLevel           = 119
   integer, public, parameter :: CFiMorVelocity             = 120
   integer, public, parameter :: CFiMorWidth                = 121
   integer, public, parameter :: CFiMorDepth                = 122
   integer, public, parameter :: CFiPumpHead                = 123
   integer, public, parameter :: CFiState                   = 124
   integer, public, parameter :: CFiWindVelocity            = 125
   integer, public, parameter :: CFiWindDirection           = 126
   
end module m_GlobalParameters                                 
