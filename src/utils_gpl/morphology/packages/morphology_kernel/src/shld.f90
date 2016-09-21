function shld(dstar     )
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
!  $Id: shld.f90 4612 2015-01-21 08:48:09Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_gpl/morphology/packages/morphology_kernel/src/shld.f90 $
!!--description-----------------------------------------------------------------
!
! determines shields parameter according
! to shields curve
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Call variables
!
    real(fp), intent(in) :: dstar ! critical dimensionless grain size parameter
    real(fp)         :: shld
!
!! executable statements -------------------------------------------------------
!
    if (dstar<=4.) then
       shld = 0.240/dstar
    elseif (dstar<=10.) then
       shld = 0.140/dstar**0.64
    elseif (dstar<=20.) then
       shld = 0.040/dstar**0.10
    elseif (dstar<=150.) then
       shld = 0.013*dstar**0.29
    else
       shld = 0.055
    endif
end function shld
