module bed_ensemble
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
!  $Id: bed_ensemble.f90 4612 2015-01-21 08:48:09Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_gpl/morphology/packages/morphology_proclib/src/bed_ensemble.f90 $
!!--module description----------------------------------------------------------
!
! This module keeps track of a number of bed compositions for e.g. use in
! an ensemble setting.
!
!!--module declarations---------------------------------------------------------

use bedcomposition_module, only:bedcomp_data
use message_module, only:message_stack
private

!
! public data types
!
public bed_data

!
! public routines
!
public getbedcomp
public setbedcomp

type bed_data
   type (bedcomp_data) , pointer :: comp
   type (message_stack), pointer :: messages
   logical                       :: isInitialized
end type bed_data

!
! persistent variables
!
type(bed_data), dimension(:), allocatable :: bed_stack

contains
!
!
!==============================================================================
subroutine getbedcomp(bed,idx,ntot)
   implicit none
   integer         , intent(in) :: idx
   integer         , intent(in) :: ntot
   type(bed_data)  , intent(out):: bed
   !
   integer                      :: i
   !
   if (.not.allocated(bed_stack)) then
      allocate(bed_stack(ntot))
      do i = 1,ntot
         bed_stack(i)%isInitialized = .false.
      enddo
   endif
   bed = bed_stack(idx)
end subroutine getbedcomp
!
!
!==============================================================================
subroutine setbedcomp(bed,idx)
   implicit none
   integer         , intent(in) :: idx
   type(bed_data)  , intent(in) :: bed
   
   bed_stack(idx) = bed
end subroutine setbedcomp

end module bed_ensemble