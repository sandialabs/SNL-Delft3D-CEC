subroutine tram2 (numrealpar,realpar   ,wave      ,i2d3d     ,par       , &
                & kmax      ,bed       ,dzduu     ,dzdvv     ,rksrs     , &
                & tauadd    ,taucr0    ,aks       ,eps       ,camax     , &
                & frac      ,sig       ,thick     ,ws        , &
                & dicww     ,ltur      ,aks_ss3d  , &
                & kmaxsd    ,taurat    ,caks      ,caks_ss3d ,concin    , &
                & seddif    ,sigmol    ,rsedeq    ,scour     ,bedw      , &
                & susw      ,sbcu      ,sbcv      ,sbwu      ,sbwv      , &
                & sswu      ,sswv      ,tetacr    ,conc2d    ,error     , &
                & message   )
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
!  $Id: tram2.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/morphology/packages/morphology_kernel/src/tram2.f90 $
!!--description-----------------------------------------------------------------
!
! computes sediment transport according to
! the formula of Van Rijn 2004
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use morphology_data_module
    use sediment_basics_module, only: dsand, dgravel
    !
    implicit none
!
! Call variables
!
    integer                         , intent(in)   :: numrealpar
    real(hp), dimension(numrealpar) , intent(inout):: realpar
    !
    logical                         , intent(in)   :: wave
    integer                         , intent(in)   :: i2d3d
    integer                         , intent(in)   :: kmax
    real(fp)                        , intent(in)   :: bed
    real(fp)                        , intent(in)   :: dzduu    !  Description and declaration in rjdim.f90
    real(fp)                        , intent(in)   :: dzdvv    !  Description and declaration in rjdim.f90
    real(fp)                        , intent(in)   :: rksrs    !  Description and declaration in rjdim.f90
    real(fp)                        , intent(in)   :: tauadd
    real(fp)                        , intent(in)   :: taucr0
    real(fp)                        , intent(in)   :: eps
    real(fp)                        , intent(in)   :: camax
    real(fp)                        , intent(in)   :: frac     !  Description and declaration in rjdim.f90
    real(fp), dimension(kmax)       , intent(in)   :: sig      !  Description and declaration in rjdim.f90
    real(fp), dimension(kmax)       , intent(in)   :: thick    !  Description and declaration in rjdim.f90
    real(fp), dimension(0:kmax)     , intent(in)   :: ws       !  Description and declaration in rjdim.f90
    real(fp), dimension(0:kmax)     , intent(in)   :: dicww    !  Description and declaration in rjdim.f90
    integer                         , intent(in)   :: ltur     !  Description and declaration in iidim.f90
    real(fp)                        , intent(in)   :: sigmol   !  Description and declaration in rjdim.f90
    logical                         , intent(in)   :: scour
    real(fp)                        , intent(in)   :: bedw
    real(fp)                        , intent(in)   :: susw
    real(fp)                        , intent(in)   :: tetacr
    real(fp), dimension(30)         , intent(in)   :: par
    !
    real(fp)                        , intent(out)  :: aks
    real(fp), dimension(kmax)       , intent(out)  :: rsedeq   ! undefined if (i2d3d==3 .and. .not. epspar .and. caks>1e-6)
    real(fp)                        , intent(out)  :: aks_ss3d
    integer                         , intent(out)  :: kmaxsd
    real(fp)                        , intent(out)  :: taurat
    real(fp)                        , intent(out)  :: caks
    real(fp)                        , intent(out)  :: caks_ss3d
    real(fp)                        , intent(out)  :: conc2d
    real(fp), dimension(kmax)       , intent(inout):: concin   ! if (i2d3d==2 .or. epspar) then output else input
    real(fp), dimension(0:kmax)     , intent(out)  :: seddif   !  Description and declaration in rjdim.f90
    real(fp)                        , intent(out)  :: sbcu
    real(fp)                        , intent(out)  :: sbcv
    real(fp)                        , intent(out)  :: sbwu
    real(fp)                        , intent(out)  :: sbwv
    real(fp)                        , intent(out)  :: sswu
    real(fp)                        , intent(out)  :: sswv
    logical                         , intent(out)  :: error
    character(*)                    , intent(out)  :: message     ! Contains error message
!
! Local variables
!
    integer :: iopsus
    real(fp):: pangle
    real(fp):: fpco
    integer :: subiw
    logical :: epspar
    !
    real(fp):: ag       
    real(fp):: chezy    
    real(fp):: d10      
    real(fp):: d90      
    real(fp):: di50     
    real(fp):: dstar
    real(fp):: h1       
    real(fp):: hidexp   
    real(fp):: hrms     
    real(fp):: mudfrac  
    real(fp):: rhosol   
    real(fp):: rhowat   
    real(fp):: rlabda   
    real(fp):: salinity 
    real(fp):: salmax
    real(fp):: teta     
    real(fp):: tp       
    real(fp):: umod     
    real(fp):: uorb     
    real(fp):: uuu      
    real(fp):: vicmol   
    real(fp):: vonkar   
    real(fp):: vvv      
    real(fp):: ws0
    real(fp):: z0cur    
    real(fp):: z0rou    
    real(fp):: zumod    
    real(fp):: gamtcr
    !
    integer  :: k
    real(fp) :: avgcu
    real(fp) :: avgu
    real(fp) :: bakdif
    real(fp) :: betam
    real(fp) :: delm
    real(fp) :: deltas
    real(fp) :: delw
    real(fp) :: drho
    real(fp) :: dss
    real(fp) :: dz
    real(fp) :: fc1
    real(fp) :: fcc
    real(fp) :: ff
    real(fp) :: fi
    real(fp) :: fsilt
    real(fp) :: fw1
    real(fp) :: lci
    real(fp) :: muc
    real(fp) :: phicur
    real(fp) :: psi
    real(fp) :: ra
    real(fp) :: sag
    real(fp) :: ta
    real(fp) :: tauc
    real(fp) :: taucr1
    real(fp) :: tauwav
    real(fp) :: u
    real(fp) :: u2dhim
    real(fp) :: uoff
    real(fp) :: uon
    real(fp) :: ustarc
    real(fp) :: usus
    real(fp) :: utot
    real(fp) :: uwb
    real(fp) :: uwbih
    real(fp) :: uwc
    real(fp) :: v
    real(fp) :: z
    real(fp) :: zusus
!
!! executable statements -------------------------------------------------------
!
    uuu       = real(realpar(RP_UCHAR),fp)
    vvv       = real(realpar(RP_VCHAR),fp)
    umod      = real(realpar(RP_VELCH),fp)
    zumod     = real(realpar(RP_ZVLCH),fp)
    h1        = real(realpar(RP_DEPTH),fp)
    chezy     = real(realpar(RP_CHEZY),fp)
    hrms      = real(realpar(RP_HRMS) ,fp)
    tp        = real(realpar(RP_TPEAK),fp)
    teta      = real(realpar(RP_TETA) ,fp)
    rlabda    = real(realpar(RP_RLAMB),fp)
    uorb      = real(realpar(RP_UORB) ,fp)
    di50      = real(realpar(RP_D50)  ,fp)
    dss       = real(realpar(RP_DSS)  ,fp)
    dstar     = real(realpar(RP_DSTAR),fp)
    d10       = real(realpar(RP_D10MX),fp)
    d90       = real(realpar(RP_D90MX),fp)
    mudfrac   = real(realpar(RP_MUDFR),fp)
    hidexp    = real(realpar(RP_HIDEX),fp)
    !ws        = real(realpar(RP_SETVL),fp)
    rhosol    = real(realpar(RP_RHOSL),fp)
    rhowat    = real(realpar(RP_RHOWT),fp)
    salinity  = real(realpar(RP_SALIN),fp)
    !temp      = real(realpar(RP_TEMP) ,fp)
    ag        = real(realpar(RP_GRAV) ,fp)
    vicmol    = real(realpar(RP_VICML),fp)
    !taub      = real(realpar(RP_TAUB) ,fp)
    !ubed      = real(realpar(RP_UBED ),fp)
    !vbed      = real(realpar(RP_VBED ),fp)
    !velb      = real(realpar(RP_VELBD),fp)
    !zvelb     = real(realpar(RP_ZVLBD),fp)
    vonkar    = real(realpar(RP_VNKAR),fp)
    z0cur     = real(realpar(RP_Z0CUR),fp)
    z0rou     = real(realpar(RP_Z0ROU),fp)
    !
    iopsus = int(par(11))
    pangle = par(12)
    fpco   = par(13)
    subiw  = int(par(14))
    epspar = par(15)>0.0_fp
    gamtcr = par(16)
    salmax = par(17)
    betam  = par(18)
    !
    drho  = (rhosol-rhowat) / rhowat
    !
    if (di50 < 1.5_fp*dsand) then
       ws0 = drho*ag*di50**2/(18.0_fp*vicmol)
    elseif (di50 < 0.5_fp*dgravel) then
       ws0 = 10.0_fp*vicmol/di50 &
          & *(sqrt(1.0_fp + drho*ag*di50**3/(100.0_fp*vicmol**2)) - 1.0_fp)
    else
       ws0 = 1.1_fp*sqrt(drho*ag*di50)
    endif
    !
    sag    = sqrt(ag)
    !
    call bedbc2004(tp        ,rhowat    , &
                 & h1        ,umod      ,d10       ,zumod     ,di50      , &
                 & d90       ,z0cur     ,z0rou     ,drho      ,dstar     , &
                 & taucr0    ,u2dhim    ,aks       ,ra        ,usus      , &
                 & zusus     ,uwb       ,muc       ,tauwav    ,ustarc    , &
                 & tauc      ,taurat    ,ta        ,caks      ,dss       , &
                 & uwc       ,uuu       ,vvv       ,rlabda    , &
                 & hrms      ,delw      ,uon       ,uoff      ,uwbih     , &
                 & delm      ,fc1       ,fw1       ,phicur    ,rksrs     , &
                 & i2d3d     ,mudfrac   ,fsilt     ,taucr1    ,psi       , &
                 & dzduu     ,dzdvv     ,eps       ,camax     ,iopsus    , &
                 & ag        ,wave      ,tauadd    ,gamtcr    ,betam     ) 
    realpar(RP_DSS)   = real(dss    ,hp)
    !
    ! Find bottom cell for SAND sediment calculations and store for use
    ! in DIFU and DIF_WS
    !
    kmaxsd = 1
    do k = kmax - 1, 1, -1
       !
       ! Calculate level of lower cell interface
       !
       lci = (1.0_fp + sig(k) - thick(k)/2.0_fp) * h1
       if (lci >= aks) then
          kmaxsd = k
          exit
       endif
    enddo
    !
    ! Adjust caks for presence of multiple sediment fractions.
    !
    caks    = caks * frac
    !
    ! Calculate sediment mixing due to waves following
    ! Van Rijn 2004 - intra-wave approach for bed load (original TR2004)
    !
    caks_ss3d = caks
    aks_ss3d  = aks
    if (caks > 1.0e-6_fp) then
       call calseddf2004(ustarc    ,ws        ,tp        ,hrms      ,h1        , &
                       & seddif    ,kmax      ,sig       ,thick     ,dicww     , &
                       & tauwav    ,tauc      ,ltur      ,delw      ,rhowat    , &
                       & uwbih     ,aks       ,caks      ,caks_ss3d ,deltas    , &
                       & aks_ss3d  ,di50      ,salinity  ,ws0       ,psi       , &
                       & epspar    ,eps       ,vonkar    ,salmax    ,wave      )
    else
       do k = 1, kmax
          seddif(k) = dicww(k)
       enddo
    endif
    !
    ! Calculate equilibrium concentration profile for sediment
    ! Note: option of selecting either Rouse profile or solution
    ! by numerical integration has been removed; only numerical
    ! integration.
    ! set background diffusion and effective beta factor
    !
    bakdif = vicmol / sigmol
    !
    ! Use simple expression based on upwind approximation for
    ! concentration and fall velocity, and central difference for
    ! concentration gradient.
    ! solution to stationary advection/diffusion equation in vertical.
    !
    if (i2d3d==2 .or. epspar) then
       !
       ! write concentration concin in an empty array if 2D or
       ! Van Rijn's parametric model and susp. transp. due to waves is used       
       !
       if (caks>1.0e-6_fp) then
          rsedeq(kmaxsd+1) = caks_ss3d
          do k = kmaxsd, 1, -1
             if (rsedeq(k+1) > 1.0e-6_fp) then
                dz        = h1 * (sig(k)-sig(k+1))
                fi        = max(1.0_fp + ((rsedeq(k+1)/0.65_fp)**0.8_fp) - 2.0_fp*((rsedeq(k+1)/0.65_fp)**0.4_fp), 0.01_fp)
                seddif(k) = seddif(k) * fi                      
                fcc       = -ws(k) * rsedeq(k+1) * (max(1.0_fp-rsedeq(k+1),0.5_fp))**5.0_fp / seddif(k)
                ff        = 1.0_fp / rsedeq(k+1) * fcc
                rsedeq(k) = exp(log(rsedeq(k+1))+dz*ff)
             else
                rsedeq(k) = 0.0_fp
             endif
          enddo
          !
          ! And then work down
          !
          if (kmax > kmaxsd+1) then
             do k = kmaxsd + 2, kmax
                rsedeq(k) = rsedeq(k-1)
             enddo
          endif
          do k = 1,kmax
             rsedeq(k) = rsedeq(k) * rhosol
             concin(k) = rsedeq(k)
          enddo
       else
          !
          ! caks<=1.0e-6_fp
          !
          do k = 1, kmax
             rsedeq(k) = 0.0_fp
             concin(k) = 0.0_fp
          enddo
       endif
    else
       !
       ! use the r0 values copied into concin array by erosed
       !
       if (caks<=1.0e-6_fp) then
          do k = 1, kmax
             rsedeq(k) = 0.0_fp
          enddo
       endif
    endif
    !
    ! Compute depth-averaged velocity, representative concentration and transport
    !
    ! imaginary "depth-averaged current" which has a logarithmic
    ! velocity profile, and a velocity at the bottom zeta point equivalent
    ! to that calculated by the model for 3D current and waves is
    ! calculated in bedbc2004/ (also 1993??) = u2dhim
    !
    avgu     = 0.0_fp
    avgcu    = 0.0_fp
    if (zumod > 0.0_fp) then
       do k = 1, kmax
          z     = (1.0_fp + sig(k)) * h1
          u     = log(1.0_fp + z/z0rou)
          avgu  = avgu  + u*thick(k)
          avgcu = avgcu + u*rsedeq(k)*thick(k)
       enddo
       conc2d = avgcu / max(avgu,eps)
       avgu   = avgu * umod / log(1.0_fp + zumod/z0rou)
    else
       conc2d = 0.0_fp
    endif
    !
    if (scour) then
       utot = ustarc * chezy / sag
    else
       utot = avgu
    endif
    u     = utot * uuu / (umod+eps)
    v     = utot * vvv / (umod+eps)
    !
    ! VAN RIJN 2004 Instantaneous bed load
    !
    if ((bed>0.0_fp .or. bedw>0.0_fp .or. susw>0.0_fp) .and. caks>0.0_fp) then
       call bedtr2004(u2dhim    ,di50      ,d90       ,h1        ,rhosol    , &
                    & tp        ,teta      ,uon       ,uoff      ,uwb       , &
                    & taucr1    ,delm      ,ra        ,z0cur     ,fc1       , &
                    & fw1       ,dstar     ,drho      ,phicur    ,sbcu      , &
                    & sbcv      ,sbwu      ,sbwv      ,sswu      ,sswv      , &
                    & tetacr    ,aks       ,fsilt     ,sig       ,thick     , &
                    & concin    ,kmax      ,deltas    ,ws(1)     ,rksrs     , &
                    & dzduu     ,dzdvv     ,rhowat    ,ag        ,bedw      , &
                    & pangle    ,fpco      ,susw      ,wave      ,eps       , &
                    & subiw     ,error     ,message   )
       if (error) return
    else
       error = .false.
    endif
end subroutine tram2
