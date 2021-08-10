module m_ExtraResistance
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
!  $Id: ExtraResistance.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/ExtraResistance.f90 $
!-------------------------------------------------------------------------------
   
   use m_tables
   
   implicit none
   
   private

   public realloc
   public dealloc

   interface dealloc
      module procedure deallocExtraResistance
   end interface dealloc
   
   !> Extra resistance description
   type, public :: t_ExtraResistance
      !> Extra resistance type
      !! - 1 : formulering: dz = ksi*q*q         so for q = 2 and ksi = 0.25 : dz = 1.000
      !! - 2 : formulering: dz = eta             so for any q and ksi = 0.25 : dz = 0.25
      !! - 3 : formulering: dz = ksiu*u*u/(2*g)  so for u = 2 and ksi = 0.25 : dz = 0.05
      integer                 :: erType = 1                    ! Only Type Supported         
      type(t_table), pointer  :: values => null()              !< table containing function ksi(s1_up)
   end type t_ExtraResistance

   contains

   subroutine deallocExtraResistance(extraRes)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_ExtraResistance), intent(inout)          :: extraRes
      
      ! Local variables
   
      ! Program code
      call dealloc(extraRes%values)
      extraRes%values => null()
      
   end subroutine deallocExtraResistance
   
end module 

    