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
   !  $Id: time_class.f90 65778 2020-01-14 14:07:42Z mourits $
   !  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_lgpl/deltares_common/packages/deltares_common/src/time_class.f90 $
   !!--description-----------------------------------------------------------------
   !
   !    Function: - time class with double counters for mjd and seconds since reference time
   !
   !!--pseudo code and references--------------------------------------------------
   ! NONE
module time_class
   use precision
   implicit none
   private

   type, public :: c_time
      real(kind=hp), private :: as_mjd
      real(kind=hp), private :: as_seconds
   contains
      procedure :: mjd => c_time_mjd
      procedure :: set => c_time_set
      procedure :: set2 => c_time_set2
      procedure :: seconds => c_time_seconds
   end type c_time

contains
   subroutine c_time_set(this, mjd)
      class(c_time), intent(inout) :: this
      real(kind=hp), intent(in) :: mjd

      this%as_mjd = mjd
      this%as_seconds = 0.0_hp
   end subroutine c_time_set

   subroutine c_time_set2(this, mjd, seconds)
      class(c_time), intent(inout) :: this
      real(kind=hp), intent(in) :: mjd, seconds

      this%as_mjd = mjd + seconds
      this%as_seconds = seconds * 86400.0_hp
   end subroutine c_time_set2

   function c_time_mjd(this) result(t)
      class(c_time), intent(in) :: this
      real(kind=hp) :: t

      t = this%as_mjd
   end function c_time_mjd

   function c_time_seconds(this) result(t)
      class(c_time), intent(in) :: this
      real(kind=hp) :: t

      t = this%as_seconds
   end function c_time_seconds
end module time_class
