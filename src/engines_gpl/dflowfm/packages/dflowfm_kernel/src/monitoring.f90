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

! $Id: monitoring.f90 65934 2020-02-05 13:17:27Z spee $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/monitoring.f90 $

!> @file monitoring.f90
!! Monitoring modules (data+routines).
!! m_observations and m_monitoring_crosssections
!<

!> Observation stations can be used to monitor flow data at fixed points
!! in the domain. Which data is monitored is configured elsewhere
!! (output routine history file)
!! In arrays: (1:numobs = normal obs, numobs+1:numobs+nummovobs = moving obs)
module m_observations

use m_alloc
use m_missing
use m_flowexternalforcings

implicit none

    integer                           :: numobs    = 0  !< nr of observation stations
    integer                           :: nummovobs = 0  !< nr of *moving* observation stations
    double precision, allocatable     :: xobs(:)        !< x-coord of observation points (1:numobs = normal obs from *.xyn and *.ini files, numobs+1:numobs+nummovobs = moving obs)
    double precision, allocatable     :: yobs(:)        !< y-coord of observation points
    double precision, allocatable, target :: xyobs(:)   !< xy-coord of *moving* observation points (work array for meteo)
    double precision, allocatable     :: smxobs(:)      !< maximum waterlevel of observation points
    double precision, allocatable     :: cmxobs(:)      !< maximum 2D flow velocity of observation points, 3D: maximum over all layers and time
    integer, allocatable              :: kobs(:)        !< node nrs of ACTIVE observation points
    ! NOTE: kobs is not maintained here (so also not after deleteObservation, etc.) All done once by obs_on_flowgrid.
    character(len=40), allocatable    :: namobs(:)      ! names of observation points
    integer, allocatable              :: locTpObs(:)    !< location type of observation points, determining to which flownodes to snap to (0=1d2d, 1=1d, 2=2d, 3=1d defined by branchID+chainage)
    integer, allocatable              :: obs2OP(:)      !< mapping from global m_observation::obs index to m_network::network%obs index (i.e., the ones defined via a *.ini file)

    integer, parameter, private       :: capacity_ = 1  !< Nr of additionally allocated elements when lists are full
    integer, private                  :: iUniq_ = 1
    character(len=*), parameter, private :: defaultName_ = 'Obs'
    integer                           :: mxls           !< Unit nr hisdump to excel
    integer                           :: jafahrenheit=0 !< Output in Celsius, otherwise Fahrenheit 
    

    double precision                  :: tlastupd_valobs !< Time at which the valobs array was last updated.
    double precision, dimension(:,:), allocatable, target :: valobs     !< work array with 2d and 3d values stored at observation stations, dim(MAXNUMVALOBS2D+MAXNUMVALOBS3D*max(kmx,1)+MAXNUMVALOBS3Dw*(max(kmx,1)+1),numobs+nummovobs)
    double precision, dimension(:,:), allocatable         :: valobs_all !< work array with 2d and 3d values stored at observation stations, dim(MAXNUMVALOBS2D+MAXNUMVALOBS3D*max(kmx,1)+MAXNUMVALOBS3Dw*(max(kmx,1)+1),numobs+nummovobs)
    
    integer                           :: MAXNUMVALOBS2D   ! maximum number of outputted values at observation stations
    integer                           :: MAXNUMVALOBS3D   ! maximum number of outputted values at observation stations, 3D layer centers
    integer                           :: MAXNUMVALOBS3Dw  ! maximum number of outputted values at observation stations, 3D layer interfaces (e.g. zws)
    integer                           :: IVAL_S1          ! 2D first
    integer                           :: IVAL_HS
    integer                           :: IVAL_BL
    integer                           :: IVAL_SMX
    integer                           :: IVAL_CMX
    integer                           :: IVAL_WX  
    integer                           :: IVAL_WY
    integer                           :: IVAL_PATM
    integer                           :: IVAL_RAIN
    integer                           :: IVAL_WAVEH
    integer                           :: IVAL_WAVET
    integer                           :: IVAL_WAVED
    integer                           :: IVAL_WAVEL
    integer                           :: IVAL_WAVER
    integer                           :: IVAL_WAVEU
    integer                           :: IVAL_WAVETAU
    integer                           :: IVAL_UCX         ! 3D, layer centered after 2D
    integer                           :: IVAL_UCY
    integer                           :: IVAL_UCZ
    integer                           :: IVAL_UCXQ
    integer                           :: IVAL_UCYQ
    integer                           :: IVAL_SA1
    integer                           :: IVAL_TEM1
    integer                           :: IVAL_TRA1
    integer                           :: IVAL_TRAN
    integer                           :: IVAL_HWQ1
    integer                           :: IVAL_HWQN
    integer                           :: IVAL_WQB1
    integer                           :: IVAL_WQBN
    integer                           :: IVAL_SED ! HK code
    integer                           :: IVAL_SF1 ! stm code
    integer                           :: IVAL_SFN
    integer                           :: IVAL_ZCS
    integer                           :: IVAL_ZWS         ! 3D, layer interfaces after layer centered
    integer                           :: IVAL_TKIN
    integer                           :: IVAL_TEPS
    integer                           :: IVAL_VICWW
    integer                           :: IVAL_WS1
    integer                           :: IVAL_WSN
    integer                           :: IVAL_SEDDIF1
    integer                           :: IVAL_SEDDIFN
    integer                           :: IVAL_RICH
    integer                           :: IVAL_TAIR
    integer                           :: IVAL_WIND
    integer                           :: IVAL_RHUM
    integer                           :: IVAL_CLOU
    integer                           :: IVAL_QSUN
    integer                           :: IVAL_QEVA
    integer                           :: IVAL_QCON
    integer                           :: IVAL_QLON
    integer                           :: IVAL_QFRE
    integer                           :: IVAL_QFRC
    integer                           :: IVAL_QTOT
    integer                           :: IVAL_RHO
    integer                           :: IVAL_SBCX1
    integer                           :: IVAL_SBCXN
    integer                           :: IVAL_SBCY1
    integer                           :: IVAL_SBCYN
    integer                           :: IVAL_SBWX1
    integer                           :: IVAL_SBWXN
    integer                           :: IVAL_SBWY1
    integer                           :: IVAL_SBWYN
    integer                           :: IVAL_SSCX1
    integer                           :: IVAL_SSCXN
    integer                           :: IVAL_SSCY1
    integer                           :: IVAL_SSCYN
    integer                           :: IVAL_SSWX1
    integer                           :: IVAL_SSWXN
    integer                           :: IVAL_SSWY1  
    integer                           :: IVAL_SSWYN
    integer                           :: IVAL_SOUR1    
    integer                           :: IVAL_SOURN    
    integer                           :: IVAL_SINK1    
    integer                           :: IVAL_SINKN    
    
    integer                           :: IPNT_S1            ! pointers in valobs work array
    integer                           :: IPNT_HS
    integer                           :: IPNT_BL
    integer                           :: IPNT_SMX
    integer                           :: IPNT_CMX
    integer                           :: IPNT_WX
    integer                           :: IPNT_WY
    integer                           :: IPNT_RAIN
    integer                           :: IPNT_PATM
    integer                           :: IPNT_WAVEH
    integer                           :: IPNT_WAVET
    integer                           :: IPNT_WAVEL
    integer                           :: IPNT_WAVED
    integer                           :: IPNT_WAVER
    integer                           :: IPNT_WAVEU
    integer                           :: IPNT_WAVETAU
    integer                           :: IPNT_UCX
    integer                           :: IPNT_UCY
    integer                           :: IPNT_UCZ
    integer                           :: IPNT_UCXQ
    integer                           :: IPNT_UCYQ
    integer                           :: IPNT_SA1
    integer                           :: IPNT_TEM1
    integer                           :: IPNT_TRA1
    integer                           :: IPNT_TRAN
    integer                           :: IPNT_WQB1
    integer                           :: IPNT_WQBN
    integer                           :: IPNT_HWQ1
    integer                           :: IPNT_HWQN
!    integer                           :: IPNT_SPIR1
    integer                           :: IPNT_SF1
    integer                           :: IPNT_SFN
    integer                           :: IPNT_SED
    integer                           :: IPNT_ZCS
    integer                           :: IPNT_ZWS
    integer                           :: IPNT_TKIN
    integer                           :: IPNT_TEPS
    integer                           :: IPNT_VICWW
    integer                           :: IPNT_WS1
    integer                           :: IPNT_WSN
    integer                           :: IPNT_SEDDIF1
    integer                           :: IPNT_SEDDIFN
    integer                           :: IPNT_RICH
    integer                           :: IPNT_TAIR
    integer                           :: IPNT_WIND
    integer                           :: IPNT_RHUM
    integer                           :: IPNT_CLOU
    integer                           :: IPNT_QSUN
    integer                           :: IPNT_QEVA
    integer                           :: IPNT_QCON
    integer                           :: IPNT_QLON
    integer                           :: IPNT_QFRE
    integer                           :: IPNT_QFRC
    integer                           :: IPNT_QTOT
    integer                           :: IPNT_NUM
    integer                           :: IPNT_RHO
    integer                           :: IPNT_SBCX1           ! should be done per fraction
    integer                           :: IPNT_SBCXN
    integer                           :: IPNT_SBCY1
    integer                           :: IPNT_SBCYN
    integer                           :: IPNT_SBWX1
    integer                           :: IPNT_SBWXN
    integer                           :: IPNT_SBWY1
    integer                           :: IPNT_SBWYN
    integer                           :: IPNT_SSCX1
    integer                           :: IPNT_SSCXN
    integer                           :: IPNT_SSCY1
    integer                           :: IPNT_SSCYN
    integer                           :: IPNT_SSWX1
    integer                           :: IPNT_SSWXN
    integer                           :: IPNT_SSWY1    
    integer                           :: IPNT_SSWYN    
    integer                           :: IPNT_SOUR1    
    integer                           :: IPNT_SOURN    
    integer                           :: IPNT_SINK1    
    integer                           :: IPNT_SINKN    
contains

!> (re)initialize valobs and set pointers for observation stations
subroutine init_valobs()
   implicit none
   
   tlastupd_valobs = dmiss
   call init_valobs_pointers()
   
   call alloc_valobs()
   
   return
end subroutine init_valobs

!> (re)allocate valobs work array
subroutine alloc_valobs()
   use m_partitioninfo
   implicit none
   
   if ( allocated(valobs) ) then
      deallocate(valobs)
   end if
   
   if ( IPNT_NUM.gt.0 ) then
      allocate(valobs(IPNT_NUM,numobs+nummovobs))
      valobs = 0d0   ! should not be DMISS, since DMISS is used for global reduction in parallel computations
   end if
   
   if ( jampi.eq.1 ) then
      if ( allocated(valobs_all) ) then
         deallocate(valobs_all)
      end if
      allocate(valobs_all(IPNT_NUM,numobs+nummovobs))
   end if
   
   return
end subroutine alloc_valobs

!> set the pointers in the valobs work array
!! only include variables that are available
!! IVAL_XXX are the enumerators >0 when available
!! IPNT_XXX are the pointers in the "valobs" array,
!! which is being reduced in parallel runs
subroutine init_valobs_pointers()
   use m_flowparameters
   use m_flow, only: iturbulencemodel, idensform, kmx
   use m_transport, only: ITRA1, ITRAN, ISED1, ISEDN
   use m_fm_wq_processes, only: noout, numwqbots
   use m_sediment, only: stm_included, stmpar
   implicit none
   
   integer             :: i, i0, numfracs
   
   MAXNUMVALOBS2D  = 0
   MAXNUMVALOBS3D  = 0
   MAXNUMVALOBS3Dw = 0
   
!  initialize
   IVAL_S1         = 0
   IVAL_HS         = 0
   IVAL_BL         = 0
   IVAL_SMX        = 0
   IVAL_CMX        = 0
   IVAL_WX         = 0
   IVAL_WY         = 0
   IVAL_PATM       = 0
   IVAL_WAVEH      = 0
   IVAL_WAVET      = 0
   IVAL_WAVED      = 0
   IVAL_WAVEL      = 0
   IVAL_WAVER      = 0
   IVAL_WAVEU      = 0
   IVAL_WAVETAU    = 0
   IVAL_UCX        = 0
   IVAL_UCY        = 0
   IVAL_UCZ        = 0
   IVAL_UCXQ       = 0
   IVAL_UCYQ       = 0
   IVAL_SA1        = 0
   IVAL_TEM1       = 0
   IVAL_TRA1       = 0
   IVAL_TRAN       = 0
   IVAL_HWQ1       = 0
   IVAL_HWQN       = 0
   IVAL_WQB1       = 0
   IVAL_WQBN       = 0
   IVAL_SF1        = 0
   IVAL_SFN        = 0
   IVAL_SED        = 0
   IVAL_ZCS        = 0
   IVAL_ZWS        = 0
   IVAL_TKIN       = 0
   IVAL_TEPS       = 0
   IVAL_VICWW      = 0
   IVAL_RICH       = 0
   IVAL_WS1        = 0
   IVAL_WSN        = 0
   IVAL_SEDDIF1    = 0
   IVAL_SEDDIFN    = 0
   IVAL_TAIR       = 0
   IVAL_WIND       = 0
   IVAL_RHUM       = 0
   IVAL_CLOU       = 0
   IVAL_QSUN       = 0
   IVAL_QEVA       = 0
   IVAL_QCON       = 0
   IVAL_QLON       = 0
   IVAL_QFRE       = 0
   IVAL_QFRC       = 0
   IVAL_QTOT       = 0
   IVAL_RAIN       = 0
   IVAL_RHO        = 0
   IVAL_SBCX1      = 0          ! should be done per fraction
   IVAL_SBCXN      = 0
   IVAL_SBCY1      = 0
   IVAL_SBCYN      = 0
   IVAL_SBWX1      = 0
   IVAL_SBWXN      = 0
   IVAL_SBWY1      = 0
   IVAL_SBWYN      = 0
   IVAL_SSCX1      = 0
   IVAL_SSCXN      = 0
   IVAL_SSCY1      = 0
   IVAL_SSCYN      = 0
   IVAL_SSWX1      = 0
   IVAL_SSWXN      = 0
   IVAL_SSWY1      = 0   
   IVAL_SSWYN      = 0
   IVAL_SOUR1      = 0
   IVAL_SOURN      = 0
   IVAL_SINK1      = 0
   IVAL_SINKN      = 0   
!  2D
   i=0
   i0=i;
   i=i+1;               IVAL_S1         = i
   i=i+1;               IVAL_HS         = i
   i=i+1;               IVAL_BL         = i
   i=i+1;               IVAL_SMX        = i
   i=i+1;               IVAL_CMX        = i
   if ( jawind.gt.0 ) then
      i=i+1;            IVAL_WX         = i
      i=i+1;            IVAL_WY         = i
   end if
   if ( japatm.gt.0 ) then
      i=i+1;            IVAL_PATM       = i
   end if
   if ( jawave.gt.0 ) then
      i=i+1;            IVAL_WAVEH      = i
      i=i+1;            IVAL_WAVED      = i
      i=i+1;            IVAL_WAVET      = i
      i=i+1;            IVAL_WAVEL      = i
      i=i+1;            IVAL_WAVER      = i
      i=i+1;            IVAL_WAVEU      = i
      i=i+1;            IVAL_WAVETAU    = i
   end if
   if ( jatem.gt.1 ) then
      i=i+1;            IVAL_TAIR       = i
   end if
   if ( jawind.gt.0 ) then
      i=i+1;            IVAL_WIND       = i
   end if
   if ( jatem.eq.5 ) then
      i=i+1;            IVAL_RHUM       = i
      i=i+1;            IVAL_CLOU       = i
      i=i+1;            IVAL_QSUN       = i
      i=i+1;            IVAL_QEVA       = i
      i=i+1;            IVAL_QCON       = i
      i=i+1;            IVAL_QLON       = i
      i=i+1;            IVAL_QFRE       = i
      i=i+1;            IVAL_QFRC       = i
   end if
   if ( jatem.gt.1 ) then
      i=i+1;            IVAL_QTOT       = i
   end if
   if ( jahisrain.gt.0 ) then
      i=i+1;            IVAL_RAIN       = i
   end if
   if ( numwqbots.gt.0 ) then
      i=i+1;            IVAL_WQB1       = i
      i=i+numwqbots-1; IVAL_WQBN       = i
   end if
   if (stm_included .and. jased>0) then
      numfracs = stmpar%lsedtot
      i=i+1;          IVAL_SBCX1      = i          ! should be done per fraction
      i=i+numfracs-1; IVAL_SBCXN      = i
      i=i+1;          IVAL_SBCY1      = i
      i=i+numfracs-1; IVAL_SBCYN      = i
      i=i+1;          IVAL_SSCX1      = i
      i=i+numfracs-1; IVAL_SSCXN      = i
      i=i+1;          IVAL_SSCY1      = i
      i=i+numfracs-1; IVAL_SSCYN      = i
      if (jawave>0) then
         i=i+1;          IVAL_SBWX1      = i
         i=i+numfracs-1; IVAL_SBWXN      = i
         i=i+1;          IVAL_SBWY1      = i
         i=i+numfracs-1; IVAL_SBWYN      = i
         i=i+1;          IVAL_SSWX1      = i
         i=i+numfracs-1; IVAL_SSWXN      = i
         i=i+1;          IVAL_SSWY1      = i   
         i=i+numfracs-1; IVAL_SSWYN      = i
      end if
      numfracs = stmpar%lsedsus
      i=i+1;          IVAL_SOUR1      = i
      i=i+numfracs-1; IVAL_SOURN      = i
      i=i+1;          IVAL_SINK1      = i
      i=i+numfracs-1; IVAL_SINKN      = i      
   end if
   MAXNUMVALOBS2D                       = i-i0
   
!  3D, layer centered
   i0=i;
   i=i+1;               IVAL_UCX        = i
   i=i+1;               IVAL_UCY        = i
   if ( kmx.gt.0 ) then
      i=i+1;            IVAL_UCZ        = i
      i=i+1;            IVAL_UCXQ       = i
      i=i+1;            IVAL_UCYQ       = i
   end if
   if ( jasal.gt.0 ) then
      i=i+1;            IVAL_SA1        = i
   end if
   if ( jatem.gt.0 ) then
      i=i+1;            IVAL_TEM1       = i
   end if
   if ( ITRA1.gt.0 ) then
      i=i+1;            IVAL_TRA1       = i
      i=i+ITRAN-ITRA1;  IVAL_TRAN       = i  !< All tracers (NOT only the ones with bnd)
   end if
   if ( noout.gt.0 ) then
      i=i+1;            IVAL_HWQ1       = i
      i=i+noout-1;      IVAL_HWQN       = i  !< All waq history outputs
   end if
   if ( stm_included .and. ISED1.gt.0 ) then
      i=i+1;              IVAL_SF1       = i
      i=i+ISEDN-ISED1;    IVAL_SFN       = i 
   end if
   if ( jased.gt.0 .and. .not. stm_included) then
      i=i+1;            IVAL_SED        = i
   end if
   if ( kmx.gt.0 ) then
      i=i+1;            IVAL_ZCS        = i
   end if
   if( jasal > 0 .or. jatem > 0 .or. jased > 0 ) then
      i=i+1;            IVAL_RHO        = i
   endif
   MAXNUMVALOBS3D                       = i-i0

!  3D, layer interfaces
   i0=i;
   if ( kmx.gt.0 ) then
      i=i+1;            IVAL_ZWS        = i
      if ( iturbulencemodel.gt.0 ) then
         i=i+1;         IVAL_TKIN       = i
         i=i+1;         IVAL_TEPS       = i
         i=i+1;         IVAL_VICWW      = i
      end if
      if ( idensform.gt.0 ) then
         i=i+1;         IVAL_RICH       = i
      end if
      if (jased>0 .and. stm_included .and. ISED1.gt.0) then
         i=i+1;              IVAL_SEDDIF1   = i
         i=i+ISEDN-ISED1;    IVAL_SEDDIFN   = i
      endif
   end if
   if (jased>0 .and. stm_included .and. ISED1.gt.0) then     ! also 2d
      i=i+1;              IVAL_WS1       = i
      i=i+ISEDN-ISED1;    IVAL_WSN       = i
   endif
   MAXNUMVALOBS3Dw                      = i-i0
   
!  set pointers in valobs array   
   IPNT_S1    = ivalpoint(IVAL_S1,    kmx)  ! kmx > 1 for non 3D quantitites?  antwoord: nee, omdat bijv. IVAL_S1 <= MAXNUMVALOBS2D
   IPNT_HS    = ivalpoint(IVAL_HS,    kmx)
   IPNT_BL    = ivalpoint(IVAL_BL,    kmx)
   IPNT_SMX   = ivalpoint(IVAL_SMX,   kmx)
   IPNT_CMX   = ivalpoint(IVAL_CMX,   kmx)
   IPNT_UCX   = ivalpoint(IVAL_UCX,   kmx)
   IPNT_UCY   = ivalpoint(IVAL_UCY,   kmx)
   IPNT_UCZ   = ivalpoint(IVAL_UCZ,   kmx)   
   IPNT_UCXQ  = ivalpoint(IVAL_UCXQ,  kmx)   
   IPNT_UCYQ  = ivalpoint(IVAL_UCYQ,  kmx)   
   IPNT_SA1   = ivalpoint(IVAL_SA1,   kmx)
   IPNT_TEM1  = ivalpoint(IVAL_TEM1,  kmx)
   IPNT_TRA1  = ivalpoint(IVAL_TRA1,  kmx)
   IPNT_TRAN  = ivalpoint(IVAL_TRAN,  kmx)
   IPNT_HWQ1  = ivalpoint(IVAL_HWQ1,  kmx)
   IPNT_HWQN  = ivalpoint(IVAL_HWQN,  kmx)
   IPNT_SF1   = ivalpoint(IVAL_SF1,   kmx)
   IPNT_SFN   = ivalpoint(IVAL_SFN,   kmx)
!   IPNT_SPIR1 = ivalpoint(IVAL_SPIR1, kmx)
   IPNT_SED   = ivalpoint(IVAL_SED,   kmx)
   IPNT_WX    = ivalpoint(IVAL_WX ,   kmx)
   IPNT_WY    = ivalpoint(IVAL_WY ,   kmx)
   IPNT_PATM  = ivalpoint(IVAL_PATM,  kmx)
   IPNT_WAVEH = ivalpoint(IVAL_WAVEH, kmx)
   IPNT_WAVET = ivalpoint(IVAL_WAVET, kmx)
   IPNT_WAVED = ivalpoint(IVAL_WAVED, kmx)
   IPNT_WAVETAU = ivalpoint(IVAL_WAVETAU, kmx)
   IPNT_WAVEL = ivalpoint(IVAL_WAVEL, kmx)
   IPNT_WAVER = ivalpoint(IVAL_WAVER, kmx)
   IPNT_WAVEU = ivalpoint(IVAL_WAVEU, kmx)
   IPNT_ZCS   = ivalpoint(IVAL_ZCS,   kmx)
   IPNT_ZWS   = ivalpoint(IVAL_ZWS,   kmx)
   IPNT_TKIN  = ivalpoint(IVAL_TKIN,  kmx)
   IPNT_TEPS  = ivalpoint(IVAL_TEPS,  kmx)
   IPNT_VICWW = ivalpoint(IVAL_VICWW, kmx)
   IPNT_RICH  = ivalpoint(IVAL_RICH,  kmx)
   IPNT_RHO   = ivalpoint(IVAL_RHO,   kmx)
   IPNT_WS1   = ivalpoint(IVAL_WS1,   kmx)
   IPNT_WSN   = ivalpoint(IVAL_WSN,   kmx)
   IPNT_SEDDIF1 = ivalpoint(IVAL_SEDDIF1,   kmx)
   IPNT_SEDDIFN = ivalpoint(IVAL_SEDDIFN,   kmx)
   IPNT_SBCX1 = ivalpoint(IVAL_SBCX1,   kmx)
   IPNT_SBCXN = ivalpoint(IVAL_SBCXN,   kmx)
   IPNT_SBCY1 = ivalpoint(IVAL_SBCY1,   kmx)
   IPNT_SBCYN = ivalpoint(IVAL_SBCYN,   kmx)
   IPNT_SSCX1 = ivalpoint(IVAL_SSCX1,   kmx)
   IPNT_SSCXN = ivalpoint(IVAL_SSCXN,   kmx)
   IPNT_SSCY1 = ivalpoint(IVAL_SSCY1,   kmx)
   IPNT_SSCYN = ivalpoint(IVAL_SSCYN,   kmx)
   IPNT_SOUR1 = ivalpoint(IVAL_SOUR1,   kmx)
   IPNT_SOURN = ivalpoint(IVAL_SOURN,   kmx)
   IPNT_SINK1 = ivalpoint(IVAL_SINK1,   kmx)
   IPNT_SINKN = ivalpoint(IVAL_SINKN,   kmx)
   if (jawave>0) then
      IPNT_SBWX1 = ivalpoint(IVAL_SBWX1,   kmx)
      IPNT_SBWXN = ivalpoint(IVAL_SBWXN,   kmx)
      IPNT_SBWY1 = ivalpoint(IVAL_SBWY1,   kmx)
      IPNT_SBWYN = ivalpoint(IVAL_SBWYN,   kmx)
      IPNT_SSWX1 = ivalpoint(IVAL_SSWX1,   kmx)
      IPNT_SSWXN = ivalpoint(IVAL_SSWXN,   kmx)
      IPNT_SSWY1 = ivalpoint(IVAL_SSWY1,   kmx)
      IPNT_SSWYN = ivalpoint(IVAL_SSWYN,   kmx)
   endif
   
   IPNT_TAIR  = ivalpoint(IVAL_TAIR,  kmx)
   IPNT_WIND  = ivalpoint(IVAL_WIND,  kmx)
   IPNT_RHUM  = ivalpoint(IVAL_RHUM,  kmx)
   IPNT_CLOU  = ivalpoint(IVAL_CLOU,  kmx)
   IPNT_QSUN  = ivalpoint(IVAL_QSUN,  kmx)
   IPNT_QEVA  = ivalpoint(IVAL_QEVA,  kmx)
   IPNT_QCON  = ivalpoint(IVAL_QCON,  kmx)
   IPNT_QLON  = ivalpoint(IVAL_QLON,  kmx)
   IPNT_QFRE  = ivalpoint(IVAL_QFRE,  kmx)
   IPNT_QFRC  = ivalpoint(IVAL_QFRC,  kmx)
   IPNT_QTOT  = ivalpoint(IVAL_QTOT,  kmx)
   IPNT_RAIN  = ivalpoint(IVAL_RAIN,  kmx)
   IPNT_WQB1  = ivalpoint(IVAL_WQB1,  kmx)
   IPNT_WQBN  = ivalpoint(IVAL_WQBN,  kmx)
   
   IPNT_NUM   = ivalpoint(0,          kmx)-1
   
   return
   
end subroutine init_valobs_pointers

!> pointer of variable in valobs work array
integer function ivalpoint(ivar, kmx)
   use messageHandling
   
   implicit none
   
   integer, intent(in) :: ivar   !< observation station variable number
   integer, intent(in) :: kmx    !< maximum number of layers
   
   integer             :: i, istart, iend
   
   ivalpoint = 1
   
   istart = 0
   iend   = 0
   
!  2D
   istart = iend+1
   iend   = iend+MAXNUMVALOBS2D
   do i=1,MAXNUMVALOBS2D
      if ( i.eq.ivar ) return
      ivalpoint = ivalpoint + 1
   end do
   
!  3D, layer centers (dim(kmx))
   istart = iend+1
   iend   = iend+MAXNUMVALOBS3D
   do i=istart,iend
      if ( i.eq.ivar ) return
      ivalpoint = ivalpoint + max(kmx,1)
   end do
   
!  3D, layer interfaces (dim(kmx+1))
   istart = iend+1
   iend   = iend+MAXNUMVALOBS3Dw
   do i=istart,iend
      if ( i.eq.ivar ) return
      ivalpoint = ivalpoint + max(kmx,1) + 1
   end do
   
   if ( ivar.ne.0 ) then
      call mess(LEVEL_ERROR, 'ivalpoint: numbering error')
   end if
   
   return
end function ivalpoint

!> Returns the index/position of a named station in the global set arrays of this module.
subroutine getObservationIndex(statname, index)
   character(len=*), intent(in)  :: statname
   integer,          intent(out) :: index !< The position of the (possibly moving) observation station in all set arrays. 0 if not present.

   integer :: i

   index = 0
   do i=1,numobs+nummovobs
      if (trim(namobs(i)) == trim(statname)) then
         index = i
         exit
      end if
   end do
end subroutine getObservationIndex

   
!> Removes the observation point at indicated list position.
subroutine updateObservationXY(pos, xnew, ynew)
    integer, intent(in) :: pos
    double precision, intent(in) :: xnew, ynew

    if (pos <= numobs+nummovobs) then
        xobs(pos) = xnew
        yobs(pos) = ynew
    end if
end subroutine updateObservationXY


!> Adds an observation point to the existing points.
!! New observation point may be a moving one or not.
subroutine addObservation(x, y, name, isMoving, loctype, iOP)
use m_alloc
use m_GlobalParameters, only: INDTP_ALL
    double precision, intent(in) :: x !< x-coordinate
    double precision, intent(in) :: y !< y-coordinate
    character(len=*), optional, intent(in) :: name !< Name of the station, appears in output file.
    logical, optional, intent(in) :: isMoving !< Whether point is a moving station or not. Default: .false.
    integer, optional, intent(in) :: loctype  !< location type (one of INDTP_1D/2D/ALL)
    integer, optional, intent(in) :: iOP      !< local index of obs that are defined via *.ini, in the m_network%network%obs set.

    logical :: isMoving_
    integer :: i, inew, isize, loctype_

    character(len=40) :: name_
    name_ = ' '

    if (present(name)) then
        name_ = name
    else
        write(name_, '(a,i2.2)') trim(defaultName_), iUniq_
        iUniq_ = iUniq_ + 1
    end if

    if (present(loctype)) then
       loctype_ = loctype
    else
       loctype_ = INDTP_ALL
    end if
    
    if (present(isMoving)) then
        isMoving_ = isMoving
    else
        isMoving_ = .false.
    end if

    if (allocated(xobs)) then
       isize = size(xobs)
    else
       isize = 0
    end if

    if (isize <= numobs+nummovobs) then
        call realloc(xobs,   numobs+nummovobs+capacity_)
        call realloc(yobs,   numobs+nummovobs+capacity_)
        call realloc(xyobs,  2*(nummovobs+capacity_))
        call realloc(kobs,   numobs+nummovobs+capacity_)
        call realloc(namobs, numobs+nummovobs+capacity_)
        call realloc(smxobs, numobs+nummovobs+capacity_)
        call realloc(cmxobs, numobs+nummovobs+capacity_)
        call realloc(locTpObs, numobs+nummovobs+capacity_)
        call realloc(obs2OP, numobs+nummovobs+capacity_)
    end if

    ! Before adding new normal observation station:
    ! shift all moving stations (if any) one to the right in arrays.
    if (.not. isMoving_) then
        do i=numobs+nummovobs,numobs+1,-1
            xobs(i+1)   = xobs(i)
            yobs(i+1)   = yobs(i)
            kobs(i+1)   = kobs(i)
            namobs(i+1) = namobs(i)
            smxobs(i+1) = smxobs(i)
            cmxobs(i+1) = cmxobs(i)
            locTpObs(i+1) = locTpObs(i)
            obs2OP(i+1) = obs2OP(i)
        end do
        numobs = numobs+1
        inew   = numobs
    else
        nummovobs = nummovobs + 1
        inew      = numobs+nummovobs
    end if

    ! Add the actual station (moving or static)
    xobs(inew)   = x
    yobs(inew)   = y
    namobs(inew) = name_
    kobs(inew)   = -999   ! Cell number is set elsewhere
    smxobs(inew) = -999d0 ! max waterlevel
    cmxobs(inew) = -999d0 ! max velocity mag.
    locTpObs(inew) = loctype_
    if (present(iOP)) then
       obs2OP(inew) = iOP ! mapping from global obs index to local *.ini obs
    else
       obs2OP(inew) = 0
    end if

end subroutine addObservation


!> Adds observation points that are read from *.ini file to the normal obs adm
subroutine addObservation_from_ini(network, filename)
   use m_network
   use m_sferic, only:jsferic
   use m_ObservationPoints
   use odugrid
   use m_save_ugrid_state
   use dfm_error
   implicit none
   type(t_network),  intent(inout)       :: network            !< network
   character(len=*), intent(in   )       :: filename           !< filename of the obs file
   
   integer                               :: nByBrch            ! number of obs that are defined by branchID and chainage
   integer                               :: ierr, nobsini, i
   type(t_ObservationPoint), pointer     :: pOPnt
   integer,              allocatable     :: branchIdx_tmp(:), ibrch2obs(:)
   double precision    , allocatable     :: Chainage_tmp(:), xx_tmp(:), yy_tmp(:)
   integer                               :: loctype_
   
   
   ierr    = DFM_NOERR
   nByBrch = 0
   nobsini = network%obs%Count
   
   !! Step 1. get x- and y-coordinates of obs that are defined by branchID and chainage
   ! 1a. save their branchIdx and chainage to temporary arrays
   allocate(branchIdx_tmp(nobsini))
   allocate(Chainage_tmp(nobsini))
   allocate(ibrch2obs(nobsini))
   
   do i=1, nobsini
      pOPnt => network%obs%OPnt(i)
      if (pOPnt%branchIdx > 0) then
         nByBrch = nByBrch + 1
         branchIdx_tmp(nByBrch) = pOPnt%branchIdx
         Chainage_tmp(nByBrch)  = pOPnt%chainage
         ibrch2obs(nByBrch)     = i
      end if
   end do
         
   ! 1b. get the corresponding x- and y-coordinates
   allocate(xx_tmp(nByBrch))
   allocate(yy_tmp(nByBrch))
   if (nByBrch > 0) then
      ierr = odu_get_xy_coordinates(branchIdx_tmp(1:nByBrch), Chainage_tmp(1:nByBrch), meshgeom1d%ngeopointx, meshgeom1d%ngeopointy, &
                                     meshgeom1d%nbranchgeometrynodes, meshgeom1d%nbranchlengths, jsferic, xx_tmp, yy_tmp)
   endif
   if (ierr /= DFM_NOERR) then
      call mess(LEVEL_ERROR, "Error occurs when getting the x- and y-coordinates for obs from file '"//trim(filename)//".")
   end if
   
   do i=1, nByBrch
      pOPnt => network%obs%OPnt(ibrch2obs(i))
      pOPnt%x = xx_tmp(i)
      pOPnt%y = yy_tmp(i)
   end do
   
   ! Step 2. add all obs from *.ini file
   do i =1, nobsini
      pOPnt => network%obs%OPnt(i)
      call addObservation(pOPnt%x, pOPnt%y, pOPnt%name, loctype = pOPnt%locationtype, iOP = i)
   end do
   
    
   if(allocated(branchIdx_tmp))deallocate(branchIdx_tmp)
   if(allocated(Chainage_tmp)) deallocate(Chainage_tmp)
   if(allocated(ibrch2obs))    deallocate(ibrch2obs)
   if(allocated(xx_tmp))       deallocate(xx_tmp)
   if(allocated(yy_tmp))       deallocate(yy_tmp)

end subroutine addObservation_from_ini

!> Adds a moving observation point to the existing points.
subroutine addMovingObservation(x, y, name)
    double precision, intent(in) :: x !< x-coordinate
    double precision, intent(in) :: y !< y-coordinate
    character(len=*), optional, intent(in) :: name

    if (present(name)) then
        call addObservation(x, y, name, isMoving = .true.)
    else
        call addObservation(x, y,       isMoving = .true.)
    end if

end subroutine addMovingObservation

!> Removes the observation point at indicated list position.
subroutine deleteObservation(pos)
    integer, intent(in) :: pos

    if (pos <= numobs+nummovobs) then
        xobs(pos) = dmiss
        yobs(pos) = dmiss
    end if
end subroutine deleteObservation

!> Cleans up 'deleted' observation points. All gaps in the *obs arrays are
!! filled again by shifting the remaining observation points.
subroutine purgeObservations()
    integer :: i, k, kk
    k = 0  ! Counts total nr of remaining obs (both static and moving)
    kk = 0 ! Counts total nr of remaining moving obs
    do i=1,numobs+nummovobs
        if (xobs(i) /= dmiss) then
            k = k+1
            xobs(k)   = xobs(i)
            yobs(k)   = yobs(i)
            kobs(k)   = kobs(i)
            namobs(k) = namobs(i)
            if (i <= numobs) then
                kk = k
            end if
        end if
    end do
    numobs = kk
    nummovobs = k-kk

end subroutine purgeObservations

!> Removes all observation points
subroutine deleteObservations()
use m_ObservationPoints
use unstruc_channel_flow, only: network
    if (allocated(xobs)) then
       deallocate(xobs)
       deallocate(yobs)
       deallocate(xyobs)
       deallocate(kobs)
       deallocate(namobs)
       deallocate(smxobs)
       deallocate(cmxobs)
       deallocate(locTpObs)
       deallocate(obs2OP)
    end if
    
    call dealloc(network%obs) ! deallocate obs (defined in *.ini file)
    
    allocate(xobs(capacity_))
    allocate(yobs(capacity_))
    allocate(xyobs(2*capacity_))
    allocate(kobs(capacity_))
    allocate(namobs(capacity_))
    allocate(smxobs(capacity_))
    allocate(cmxobs(capacity_))
    allocate(locTpObs(capacity_))
    allocate(obs2OP(capacity_))


    kobs = -999

    numobs = 0
    nummovobs = 0
    tlastupd_valobs = dmiss
    call doclose(mxls)
end subroutine deleteObservations


!> Reads observation points from file.
!! Two file types are supported: *_obs.xyn and *_obs.ini.
subroutine loadObservations(filename, jadoorladen)
    use messageHandling
    use m_readObservationPoints, only: readObservationPoints
    use unstruc_channel_flow, only: network
    use m_inquire_flowgeom
    use dfm_error
    implicit none
    character(len=*), intent(in) :: filename    !< File containing the observation points. Either a *_obs.xyn or a *_obs.ini.
    integer,          intent(in) :: jadoorladen !< Append to existing observation points or not

    logical :: jawel
    integer :: tok

    inquire(file = filename, exist = jawel)
    if (jawel) then
        if (jadoorladen == 0) then
            call deleteObservations()
        end if

        tok = index(filename, '.xyn')
        if (tok > 0) then
           call loadObservations_from_xyn(filename)
        else
           tok = index(filename, '.ini')
           if (tok > 0) then
              call readObservationPoints(network, filename)
              call addObservation_from_ini(network, filename)  
           end if
        end if
    else
        call mess(LEVEL_ERROR, "Observation file '"//trim(filename)//"' not found!")
    endif

end subroutine loadObservations


!> Reads observation points from an *.xyn file.
! Typically called via loadObservations().
subroutine loadObservations_from_xyn(filename)
    use messageHandling
    use dfm_error
    implicit none
    character(len=*), intent(in) :: filename

    integer :: mobs, n, L, L2
    double precision :: xp, yp
    character (len=256) :: rec
    character (len=40) :: nam

    call oldfil(mobs,filename)

    n=0
20  read(mobs,'(a)',end =889) rec
    
    read(rec,*,err=888) xp, yp, nam
    
    L  = index(rec,'''')
    if (L > 0) then
        L  = L + 1  
        L2 = index(rec(L:),'''') - 2 + L
        nam = rec(L:L2)
    endif
    
    call addObservation(xp, yp, nam)
    n = n+1
    goto 20

889 call doclose(mobs)
    return

888 call readerror('reading x,y,nam but getting ',rec,mobs)

end subroutine loadObservations_from_xyn


subroutine saveObservations(filename)
    use m_sferic, only: jsferic
    implicit none

    character(len=*), intent(in)  :: filename

    integer :: mobs, i
    call newfil(mobs, filename)

    if ( jsferic.ne.1 ) then
       do i=1,numobs
           write(mobs, '(f12.3,f12.3,a,a,a)'), xobs(i), yobs(i), ' ''', trim(namobs(i)), ''''
       end do
    else
       do i=1,numobs
           write(mobs, '(f12.6,f12.6,a,a,a)'), xobs(i), yobs(i), ' ''', trim(namobs(i)), ''''
       end do
    end if
    call doclose(mobs)

end subroutine saveObservations
end module m_observations


!> Cross sections (crs) are used to monitor summed flow data across a line
!! over time. The definition of crs is by a crspath, which is a polyline
!! with all flow links (1D and 2D, including orientation) that cross it.
!! Given a norhtward crs, the positive  transport direction is eastward.
module m_monitoring_crosssections
use m_crspath
use m_missing
implicit none

type tcrs
    character(len=64)             :: name          !< Name
    integer                       :: nval          !< Nr. of different quantities monitored
    type(tcrspath)                :: path          !< Polyline+crossed flow links that defines this cross section.
    integer                       :: loc2OC = 0    !< mapping from global obs index to obs that are defined by branchID and chainage 
    double precision, allocatable :: sumvalcur(:)  !< Values integrated over the crs
    double precision, allocatable :: sumvalcum(:)  !< Values integrated over crs *and* time
    double precision, allocatable :: sumvalavg(:)  !< Values integrated over crs and averaged in time.
                                                   !! Size is nval: nr of monitored quantities.
end type tcrs

! Indices in sumvalcur and other arrays: postfix 'C' means cumulative/sum, 'A' means averaged.
integer                              :: IPNT_Q1C = 1           ! pointers in sumval* arrays
integer                              :: IPNT_AUC = 2           ! pointers in sumval* arrays
integer                              :: IPNT_U1A = 3           ! pointers in sumval* arrays
integer                              :: IPNT_S1A = 4           ! pointers in sumval* arrays
integer                              :: IPNT_HUA = 5           ! pointers in sumval* arrays

type (tcrs), allocatable, target     :: crs(:)
integer                              :: ncrs = 0, maxcrs = 2, maxnval = 5

integer, private                     :: iUniq_ = 1
character(len=*), parameter, private :: defaultName_ = 'Crs'
double precision                     :: tlastupd_sumval        !< Time at which the sumval* arrays were last updated.
double precision, allocatable        :: sumvalcur_tmp(:,:)     !< Store the temporary values for MPI communication of partial sums across cross sections monitoring.
double precision, allocatable        :: sumvalcumQ_mpi(:)      !< Store the time-integrated discharge in each history output interval, only used for parallel run
double precision, allocatable        :: sumvalcum_timescale(:) !< Store the time-scale multiplication (e.g. morfac in the case of sediment).

contains

!> Returns the index/position of a named crosssection in the global set arrays of this module.
subroutine getCrosssectionIndex(crsname, index)
   character(len=*), intent(in)  :: crsname
   integer,          intent(out) :: index !< The position of the (possibly moving) observation station in all set arrays. 0 if not present.

   integer :: i

   index = 0
   do i=1,ncrs
      if (trim(crs(i)%name) == trim(crsname)) then
         index = i
         exit
      end if
   end do
end subroutine getCrosssectionIndex

!> Allocates an array of cross sections, deallocating any existing memory.
subroutine allocCrossSections(cs, n)

use m_transport , only: NUMCONST
implicit none

    type(tcrs), allocatable, intent(inout) :: cs(:)   !< Array of cross sections
    integer,                 intent(in)    :: n       !< Desired nr of cross sections
    
    call deallocCrossSections(cs)
    allocate(cs(n))
    call ReallocCrossSectionSums(cs)                  !< needed only for old interactor
end subroutine allocCrossSections

subroutine ReallocCrosssectionSums(cs)
use m_transport , only: NUMCONST
use m_alloc
use m_sediment, only: jased, stmpar
implicit none
    type(tcrs), allocatable, intent(inout) :: cs(:)   !< Array of cross sections
    integer :: i
    integer                                :: maxnval
   
    maxnval = 5 + NUMCONST
        
    if( jased == 4 .and. stmpar%lsedtot > 0 ) then
       maxnval = maxnval + 1
       if( stmpar%lsedsus > 0 ) then
          maxnval = maxnval + 1
       endif
    endif
    
    
    do i=1,size(cs)
        call realloc(cs(i)%sumvalcur, maxnval, fill=0.0d0, keepExisting=.True.)
        call realloc(cs(i)%sumvalcum, maxnval, fill=0.0d0, keepExisting=.True.)
        call realloc(cs(i)%sumvalavg, maxnval, fill=0.0d0, keepExisting=.True.)
    end do
end subroutine ReallocCrossSectionSums

!> Deallocates an array of crs
subroutine deallocCrossSections(cs)
    type(tcrs), allocatable, intent(inout) :: cs(:)

    integer :: i, n

    if (.not. allocated(cs)) return

    n = size(cs)
    do i=1,n
        call deallocCrossSectionPath(cs(i)%path)
        if (allocated(cs(i)%sumvalcur)) then
            deallocate(cs(i)%sumvalcur)
            deallocate(cs(i)%sumvalcum)
            deallocate(cs(i)%sumvalavg)
        end if
    end do
    deallocate(cs)
end subroutine deallocCrossSections


!> Copies array of crs into another array of crs.
subroutine copyCrossSections(rfrom, rto)
use m_alloc
    type(tcrs), intent(inout) :: rfrom(:)
    type(tcrs), intent(inout) :: rto(:)

    integer :: i, n

    n = size(rfrom)
    if (n > size(rto) .or. n == 0) return

    do i=1,n
        !maxnp  = size(rfrom(i)%path%xp)
        !maxlnx = size(rfrom(i)%path%ln)
        !call increaseCrossSectionPath(rto(i)%path, maxnp, maxlnx)
        rto(i) = rfrom(i)
    end do
end subroutine copyCrossSections


!> Increases memory for crs
subroutine increaseCrossSections(n)
    integer, intent(in) :: n !< Desired number of cross sections.

    type(tcrs), allocatable :: crst(:) ! Temp storage
    integer                 :: jacopy
    
    jacopy = 0

    if (n < maxcrs .and. allocated(crs)) then
        return
    end if

    call allocCrossSections(crst, maxcrs)

    if (n > maxcrs) then
        maxcrs    = max(maxcrs, int(1.2*n))
    end if

    if (allocated(crs)) then
       call copyCrossSections(crs, crst)
    end if
    call allocCrossSections(crs, maxcrs)
    call copyCrossSections(crst, crs)

    call deallocCrossSections(crst)

end subroutine increaseCrossSections


!> Starts a new cross section in the active array of crs, increasing memory when necessary.
subroutine addCrossSections(name, xp, yp, iOC)
    character(len=*), intent(in) :: name
    double precision, intent(in) :: xp(:), yp(:)
    integer, optional, intent(in):: iOC          !< local index of cross sections that are defined via *.ini, in the m_network%network%observcrs set.
    
    integer :: m
    character(len=1) :: cdigits

    call increaseCrossSections(ncrs+1)

    ncrs           = ncrs + 1
    call setCrossSectionPathPolyline(crs(ncrs)%path, xp, yp)
    crs(ncrs)%path%lnx  = 0

    ! Set name (or generate one)
    m = len_trim(name)
    if (m > 0) then
        m = min(len(crs(ncrs)%name), len(name))
        crs(ncrs)%name = ' '
        crs(ncrs)%name(1:m) = name(1:m)
    else ! No name given, generate one.
        write(cdigits, '(i1)') max(2, int(floor(log10(dble(iUniq_))+1)))
        write(crs(ncrs)%name, '(a,i'//cdigits//'.'//cdigits//')'), trim(defaultName_), iUniq_
        iUniq_ = iUniq_ + 1
    end if
    
    ! Set mapping from global index to local crs that are defined by branchId and chainage
    if (present(iOC)) then
       crs(ncrs)%loc2OC = iOC
    else
       crs(ncrs)%loc2OC = 0
    end if
    
end subroutine addCrossSections


!> Deletes all cross sections from crs.
!! Does not free up memory, use deallocCrossSections for that.
subroutine delCrossSections()
    ncrs = 0
    iUniq_ = 1

    if (allocated(sumvalcur_tmp)) then
       deallocate(sumvalcur_tmp)
    end if
    if (allocated(sumvalcum_timescale)) then
       deallocate(sumvalcum_timescale)
    end if
    tlastupd_sumval = dmiss

    ! Do not reset crs data, just let it be overwritten later.
end subroutine delCrossSections

!> Reads observation cross sections and adds them to the normal crs adm
!! Two file types are supported: *_crs.pli and *_crs.ini.
subroutine loadObservCrossSections(filename, jadoorladen)
   use unstruc_messages
   use m_readObservCrossSections, only: readObservCrossSections
   use unstruc_channel_flow, only: network
   
   implicit none
   character(len=*), intent(in   ) :: filename    !< File containing the observation cross sections. Either a *_crs.pli or a *_crs.ini.
   integer,          intent(in   ) :: jadoorladen !< Append to existing observation cross sections or not

   logical :: jawel
   integer :: tok_pli, tok_ini

   !!!!!
   inquire(file = filename, exist = jawel)
   if (jawel) then
      if (jadoorladen == 0) then
         call delCrossSections()
      end if
      tok_pli = index(filename, '.pli')
      tok_ini = index(filename, '.ini')
      if (tok_pli > 0) then
         call loadObservCrossSections_from_pli(filename)
      else if (tok_ini > 0) then
         call readObservCrossSections(network, filename)
         call addObservCrsFromIni(network, filename)
      else
         call mess(LEVEL_WARN, "Observation cross section file ('"//trim(filename)//"') does not end with .pli or .ini.")
      end if
   else
       call mess(LEVEL_ERROR, "Observation cross section file '"//trim(filename)//"' not found!")
   endif
end subroutine loadObservCrossSections


!> Reads observation points from an *.pli file.
! Typically called via loadObservCrossSections().
subroutine loadObservCrossSections_from_pli(filename)
   use messageHandling
   use dfm_error
   use m_polygon
   implicit none
   character(len=*), intent(in) :: filename

   integer :: minp, ipli

   call oldfil(minp, filename)
   ipli = 0
   call reapol_nampli(minp, 0, 1, ipli)
   call pol_to_crosssections(xpl, ypl, npl, names=nampli)
   call doclose(minp)

end subroutine loadObservCrossSections_from_pli

   
!> Adds observation cross sections, that are read from *.ini file, to the normal cross section adm
subroutine addObservCrsFromIni(network, filename)
   use m_network
   use m_sferic, only:jsferic
   use odugrid
   use m_save_ugrid_state
   use dfm_error
   use m_missing
   use m_ObservCrossSections
   implicit none
   type(t_network),  intent(inout)       :: network            !< network
   character(len=*), intent(in   )       :: filename           !< filename of the cross section file
   
   integer                               :: nByBrch            ! number of cross sections that are defined by branchID and chainage
   integer                               :: ierr, ncrsini, i, numv
   type(t_observCrossSection), pointer   :: pCrs
   integer,              allocatable     :: branchIdx_tmp(:), ibrch2crs(:)
   double precision    , allocatable     :: Chainage_tmp(:), xx_tmp(:), yy_tmp(:)
   
   
   ierr    = DFM_NOERR
   nByBrch   = 0
   ncrsini = network%observcrs%count
   
   !! Step 1. get x- and y-coordinates of crs that are defined by branchID and chainage
   ! 1a. save their branchIdx and chainage to temporary arrays
   allocate(branchIdx_tmp(ncrsini))
   allocate(Chainage_tmp(ncrsini))
   allocate(ibrch2crs(ncrsini))
   
   do i=1, ncrsini
      pCrs => network%observcrs%observcross(i)
      if (pCrs%branchIdx > 0) then
         nByBrch = nByBrch + 1
         branchIdx_tmp(nByBrch) = pCrs%branchIdx
         Chainage_tmp(nByBrch)  = pCrs%chainage
         ibrch2crs(nByBrch)     = i
      end if
   end do
         
   ! 1b. get the corresponding x- and y-coordinates
   if (nByBrch > 0) then
      allocate(xx_tmp(nByBrch))
      allocate(yy_tmp(nByBrch))
      ierr = odu_get_xy_coordinates(branchIdx_tmp(1:nByBrch), Chainage_tmp(1: nByBrch), meshgeom1d%ngeopointx, meshgeom1d%ngeopointy, &
                                    meshgeom1d%nbranchgeometrynodes, meshgeom1d%nbranchlengths, jsferic, xx_tmp, yy_tmp)
      
      if (ierr /= DFM_NOERR) then
         call mess(LEVEL_ERROR, "Error occurs when getting xy coordinates for observation cross sections from file '"//trim(filename)//".")
      end if
      
      do i=1, nByBrch
         pCrs => network%observcrs%observcross(ibrch2crs(i))
         pCrs%x(1) = xx_tmp(i)
         pCrs%y(1) = yy_tmp(i)
      end do
   endif
   
   ! Step 2. add all observation crs from *.ini file
   do i =1, ncrsini
      pCrs => network%observcrs%observcross(i)
      numv = pCrs%numValues
      if (pCrs%branchIdx > 0) then ! crs which is defined by branchID and chainage
         call addCrossSections(pCrs%name, pCrs%x(1:numv), pCrs%y(1:numv), iOC = i)
      else
         call addCrossSections(pCrs%name, pCrs%x(1:numv), pCrs%y(1:numv))
      end if
   end do
    
   if(allocated(branchIdx_tmp))deallocate(branchIdx_tmp)
   if(allocated(Chainage_tmp)) deallocate(Chainage_tmp)
   if(allocated(ibrch2crs))    deallocate(ibrch2crs)
   if(allocated(xx_tmp))       deallocate(xx_tmp)
   if(allocated(yy_tmp))       deallocate(yy_tmp)

end subroutine addObservCrsFromIni


!> Converts a set of polylines into cross sections
!! The input arrays have the structure of the global polygon:
!! one or more polylines separated by dmiss values.
subroutine pol_to_crosssections(xpl, ypl, npl, names)
    use m_missing

    double precision, intent(in) :: xpl(:), ypl(:) !< Long array with one or more polylines, separated by dmiss
    integer,          intent(in) :: npl            !< Total number of polyline points
    character(len=*), optional, intent(in) :: names(:) ! Optional names for cross sections

    integer :: i, i1, i2, ic, numnam
    character(len=64) :: name

    if (present(names)) then
        numnam = size(names)
    else
        numnam = 0
    end if

    i1 = 1 ! First possible start index
    i2 = 0 ! No end index found yet.
    ic = 0 ! Nr of polylines found so far
    do i = 1,npl
        if (xpl(i) == dmiss .or. i == npl) then
            if (i == npl .and. xpl(i) /= dmiss) then
                i2 = i ! Last polyline, no dmiss separator, so also include last point #npl.
            end if
            if (i1 <= i2) then
                ! 1: Special name for this CRS or not?
                ic = ic + 1
                if (ic < numnam) then
                    name = names(ic)
                else
                    name = ' '
                end if

                ! 2: add the current polyline as a new crs.
                call addCrossSections(name, xpl(i1:i2), ypl(i1:i2))
            end if
            i1 = i+1
            cycle
        else
            i2 = i ! Advance end point by one.
        end if
    end do
end subroutine pol_to_crosssections


end module m_monitoring_crosssections

!> Module for maintaining (time-integral) statistics on flow quantities.
!! NOTE: could be the successor of Fourier analysis. Just maintain some first max/avg quantities for now.
module m_integralstats

integer            :: is_numndvals !< Number of variables on flow nodes for which statistics are recorded.
integer, parameter :: IDX_TAUS = 1 !< Index for bed shear stress
integer, parameter :: IDX_UCM  = 2 !< Index for avg cell center velocity magnitude
integer, parameter :: IDX_HS   = 3 !< Index for avg water depth

double precision, allocatable, target :: is_sumvalsnd(:,:) !< [-] Integral values on flow nodes. {"location": "face", "shape": ["is_numndvals", "ndx"]}
double precision, allocatable, target :: is_maxvalsnd(:,:) !< [-] Integral values on flow nodes. {"location": "face", "shape": ["is_numndvals", "ndx"]}

character(len=1024), allocatable, target :: is_valnamesnd(:) !NOBMI [-] Names of the variables for which statistics are maintained {"shape": ["is_numndvals"]}
double precision, target :: is_dtint !< [s] total time interval since last statistics reset.  {"rank": 0}

contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_integralstats() instead.
subroutine default_integralstats()

   is_numndvals = 0

   ! Remaining of variables is handled in reset_integralstats()
   call reset_integralstats()
end subroutine default_integralstats

!> Resets only integralstats variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, call default_integralstats() instead.
subroutine reset_integralstats()
! node related
    is_sumvalsnd(1:is_numndvals,:) = 0d0
    is_maxvalsnd(1:is_numndvals,:) = -huge(1d0)
    is_valnamesnd(:) = ''
    is_valnamesnd(1) = 'taus'
    is_valnamesnd(2) = 'ucm'
    is_valnamesnd(3) = 'hs'
    
    is_dtint = 0d0
end subroutine reset_integralstats


!> Update the (time-)integral statistics for all flow nodes, typically after each time step.
subroutine update_integralstats()
   use m_flowtimes
   use m_flow
   use m_flowgeom

   integer :: k

   if (is_numndvals <= 0) then
      return
   end if

   call gettaus(1)

   do k=1,ndxi
      is_sumvalsnd(IDX_TAUS, k) =     is_sumvalsnd(IDX_TAUS, k) + dts * taus(k)
      is_sumvalsnd(IDX_UCM,  k) =     is_sumvalsnd(IDX_UCM,  k) + dts * sqrt(ucx(k)*ucx(k) + ucy(k)*ucy(k))
      is_sumvalsnd(IDX_HS, k)   =     is_sumvalsnd(IDX_HS,   k) + dts * hs(k)

      is_maxvalsnd(IDX_TAUS, k) = max(is_maxvalsnd(IDX_TAUS, k), taus(k))
      is_maxvalsnd(IDX_UCM,  k) = max(is_maxvalsnd(IDX_UCM,  k), sqrt(ucx(k)*ucx(k) + ucy(k)*ucy(k)))
      is_maxvalsnd(IDX_HS, k)   = max(is_maxvalsnd(IDX_HS,   k), hs(k))
   end do

   is_dtint = is_dtint + dts

end subroutine update_integralstats

end module m_integralstats
   

!> Contains the global data for all thin dams.
!! thd is the array of cross section paths.
module m_thindams
    use m_crspath
    implicit none

    type (tcrspath), allocatable    :: thd(:)
    integer                         :: nthd = 0

contains

!> Increases memory for thin dams
subroutine increaseThinDams(n)
    integer, intent(inout) :: n !< Desired number of thin dams

    call increaseCRSPaths(thd, n, nthd)
end subroutine increaseThinDams


!> Converts a set of polylines into thin dams.
!! The input arrays have the structure of the global polygon:
!! one or more polylines separated by dmiss values.
subroutine pol_to_thindams(xpl, ypl, npl)
    use m_missing

    double precision, intent(in) :: xpl(:), ypl(:) !< Long array with one or more polylines, separated by dmiss
    integer,          intent(in) :: npl            !< Total number of polyline points

    integer :: i, i1, i2, maxthd

    nthd = 0

    i1 = 1 ! First possible start index
    i2 = 0 ! No end index found yet.
    do i = 1,npl
        if (xpl(i) == dmiss .or. i == npl) then
            if (i == npl .and. xpl(i) /= dmiss) then
                i2 = i ! Last polyline, no dmiss separator, so also include last point #npl.
            end if
            if (i1 <= i2) then
                maxthd = nthd+1
                call increaseThinDams(maxthd)
                nthd = nthd+1
                call setCrossSectionPathPolyline(thd(nthd), xpl(i1:i2), ypl(i1:i2))
            end if
            i1 = i+1
            cycle
        else
            i2 = i ! Advance end point by one.
        end if
    end do
end subroutine pol_to_thindams


!> Deletes all thin dams from thd.
!! Does not free up memory, use m_crspath::deallocCrossSectionPaths for that.
subroutine delThinDams()
    nthd = 0
    ! Do not reset thd data, just let it be overwritten later.
end subroutine delThinDams

end module m_thindams


!> Contains the global data for all fixed weirs.
!! fxw is the array of cross section paths.
module m_fixedweirs
    use m_crspath
    implicit none

    type (tcrspath), allocatable    :: fxw(:)

    integer                         :: nfxw = 0
    integer, allocatable            :: lnfxw(:)              ! Links with fixed weirs (dim=nfxw)
    integer, allocatable            :: nfxwL(:)              ! fixed weirs on links   (dim=Lnx) 
    double precision, allocatable   :: csfxw(:)              ! fixed weir direction 
    double precision, allocatable   :: snfxw(:)              ! fixed weir direction
    double precision, allocatable   :: crestlxw(:)           ! crest length of a weir
    double precision, allocatable   :: crestlevxw(:)         ! crest level of a weir
    double precision, allocatable   :: shlxw(:)              ! sill height left of a weir
    double precision, allocatable   :: shrxw(:)              ! sill height right of a weir
    double precision, allocatable   :: taludlxw(:)           ! talud left of a weir
    double precision, allocatable   :: taludrxw(:)           ! talud right of a weir
    double precision, allocatable   :: vegxw(:)              ! vegetation code on a weir
    double precision, allocatable   :: weirdte(:)            ! loss coeff
    integer         , allocatable   :: iweirtxw(:)           ! weir type

    double precision                :: sillheightmin    = 0.5d0 ! waqua dams with both sillheights > sillheightmin go to fixedweirs.pli
                                                                ! the rest goes to
contains



!> Deletes all fixed weirs from fxw.
!! Does not free up memory, use m_crspath::deallocCrossSectionPaths for that.
subroutine delFixedWeirs()
    nfxw = 0
    ! Do not reset fxw data, just let it be overwritten later.
end subroutine delFixedWeirs

   end module m_fixedweirs
