   !> This module contains callback functions for channel flow 
   !! These callback functions are required for communicating with
   !! the 2d part.   
   module m_callbackFunctions
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
!  $Id: callback_functions.F90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/callback_functions.F90 $
!-------------------------------------------------------------------------------
   
   abstract interface
      function info_callbackiface_logical(l)
         logical :: info_callbackiface_logical
         integer, intent(in) :: l
      end function info_callbackiface_logical
   end interface

   abstract interface
      function info_callbackiface_int(l)
         integer :: info_callbackiface_int
         integer, intent(in) :: l
      end function info_callbackiface_int
   end interface

   procedure(info_callbackiface_logical), pointer :: cb_is_1d_link => null()
   procedure(info_callbackiface_logical), pointer :: cb_is_1d_boundary => null()
   procedure(info_callbackiface_logical), pointer :: cb_is_1d_structure => null()
   procedure(info_callbackiface_int),     pointer :: cb_findInflowLine => null()

end module m_callbackFunctions