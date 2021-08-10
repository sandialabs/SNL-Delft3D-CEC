function stringsequalinsens(string1, string2) result(retval)
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2020.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!  $Id: stringsequal.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_lgpl/deltares_common/packages/deltares_common/src/stringsequal.f90 $
!!--description-----------------------------------------------------------------
!
!  Subroutine obsolete; this routine will be removed.
!  Use string_module routine strcmpi instead
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use string_module
    !
    implicit none
!
! return value
!
    logical                                     :: retVal  !  .true.  if strings are equal
                                                           !  .false. if strings are not equal or len1 /= len2
!
! arguments
!
    character(*)                  , intent(in)  :: string1 ! incoming string1
    character(*)                  , intent(in)  :: string2 ! incoming string2
!
!! executable statements -------------------------------------------------------
!
    retval = strcmpi(string1, string2)
end function stringsequalinsens
