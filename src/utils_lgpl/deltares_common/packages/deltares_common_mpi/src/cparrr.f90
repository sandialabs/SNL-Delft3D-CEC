subroutine cparrr ( iarr1, iarr2, length )
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
!  $Id: cparrr.f90 4612 2015-01-21 08:48:09Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/deltares_common/packages/deltares_common_mpi/src/cparrr.f90 $
!!--description-----------------------------------------------------------------
!
!   Copies single precision real array IARR1 to IARR2
!
!!--pseudo code and references--------------------------------------------------
!
!
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Global variables
!
    integer                      , intent(in)  :: length ! array length
    real(sp), dimension(1:length), intent(in)  :: iarr1  ! source array
    real(sp), dimension(1:length), intent(out) :: iarr2  ! target array
!
! Local variables
!
    integer :: i ! loop counter
!
!! executable statements -------------------------------------------------------
!
    do i = 1, length
       iarr2(i) = iarr1(i)
    enddo
end subroutine cparrr
