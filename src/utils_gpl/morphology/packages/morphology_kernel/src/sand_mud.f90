subroutine sand_mud(nfrac, E, frac, mudfrac, sedtyp, pmcrit)
!
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2012-2015.
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
!  $Id: sand_mud.f90 4612 2015-01-21 08:48:09Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_gpl/morphology/packages/morphology_kernel/src/sand_mud.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes erosion velocities based
!              on sand-mud interaction (Van Ledden (2003), Van Kessel (2002))
!              Array E is recomputed.
! Method used:
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sediment_basics_module, only: SEDTYP_COHESIVE
    !
    implicit none
    !
    integer                                     , intent(in)    :: nfrac    ! number of sediment fractions
    integer     , dimension(nfrac)              , intent(in)    :: sedtyp   ! sediment type
    real(fp)    , dimension(nfrac)              , intent(in)    :: frac     ! sediment (mass) fraction [-]
    real(fp)                                    , intent(in)    :: mudfrac  ! mud fraction [-]
    real(fp)                                    , intent(in)    :: pmcrit   ! critical mud fraction [-]
    real(fp)    , dimension(nfrac)              , intent(inout) :: E        ! sediment erosion velocity [m/s]
!
! Local variables
!
    integer                         :: istat        ! error flag
    integer                         :: l            ! sediment counter
    real(fp)                        :: Es_avg       ! average erosion velocity for sand fractions [m/s]
    real(fp)                        :: Em_avg       ! average erosion velocity for mud fractions [m/s]
    !
!
!! executable statements ------------------
!
    !            
    ! No sand mud interaction if there is no mud, only mud or pmcrit<0
    if (pmcrit<0.0_fp) return
    if (mudfrac<=0.0_fp .or. mudfrac>=1.0_fp) return
    !
    Es_avg = 0.0_fp
    Em_avg = 0.0_fp
    ! 
    ! Compute average erosion velocity for sand fractions
    !
    do l = 1, nfrac
        if (sedtyp(l)/= SEDTYP_COHESIVE) then
            Es_avg = Es_avg + frac(l)*E(l)
        endif
    enddo 
    Es_avg = Es_avg/(1-mudfrac)
    ! 
    if ( mudfrac <= pmcrit ) then
        !
        ! Non-cohesive regime
        ! (mud is proportionally eroded with the sand)
        !
        do l = 1, nfrac
            if (sedtyp(l) == SEDTYP_COHESIVE .and. Es_avg>0.0_fp ) then
                E(l) = Es_avg
            endif
        enddo
    else
        !
        ! Cohesive regime
        !
        ! erosion velocity for mud is interpolated between the non-cohesive and fully mud regime 
        ! fully mud regime   : mudfrac = 1       -> E(l) is not changed
        ! non-cohesive regime: mudfrac = pmcrit  -> E(l) = Es_avg
        !
        do l = 1, nfrac
            if ( sedtyp(l)==SEDTYP_COHESIVE .and. E(l)>0.0_fp ) then  
                if (Es_avg>0.0_fp) then
                    E(l) = E(l)*(Es_avg/E(l))**((1.0_fp-mudfrac)/(1.0_fp-pmcrit))
                endif
                Em_avg     = Em_avg + frac(l)*E(l)
            endif
        enddo
        Em_avg = Em_avg/mudfrac
        !
        ! sand is proportionally eroded with the mud
        !
        do l = 1, nfrac
            if (sedtyp(l) /= SEDTYP_COHESIVE) then
                E(l) = Em_avg
            endif
        enddo
    endif
end subroutine sand_mud
