!module c_structures
!!----- AGPL --------------------------------------------------------------------
!!                                                                               
!!  Copyright (C)  Stichting Deltares, 2017-2020.                                
!!                                                                               
!!  This program is free software: you can redistribute it and/or modify              
!!  it under the terms of the GNU Affero General Public License as               
!!  published by the Free Software Foundation version 3.                         
!!                                                                               
!!  This program is distributed in the hope that it will be useful,                  
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!!  GNU Affero General Public License for more details.                          
!!                                                                               
!!  You should have received a copy of the GNU Affero General Public License     
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.             
!!                                                                               
!!  contact: delft3d.support@deltares.nl                                         
!!  Stichting Deltares                                                           
!!  P.O. Box 177                                                                 
!!  2600 MH Delft, The Netherlands                                               
!!                                                                               
!!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!!  are registered trademarks of Stichting Deltares, and remain the property of
!!  Stichting Deltares. All rights reserved.
!!                                                                               
!!-------------------------------------------------------------------------------
!!  $Id: c_structures.f90 65778 2020-01-14 14:07:42Z mourits $
!!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/c_structures.f90 $
!!-------------------------------------------------------------------------------
!
!  ! This module contains c compatible structures
!  use iso_c_binding
!  use iso_c_utils
!  use m_network
!  use MessageHandling
!
!  implicit none
!
!  type(t_network), pointer:: pnetwork => null()
!
!include 'c_structures.inc'
!contains
!
!include 'update_structures.inc'
!
!
!
!
!!!!!!!! alternatief:
!!  !! HIER libsubgrid:
!!    subroutine set_structure_by_id(var_name, structure_id, field_name, fieldvalue)
!!    use cpluv, only: structures
!!! ??? structure_type ??
!!      select case(var_name)
!!      case ('pump') :
!!         structure_type = ST_PUMP
!!
!!      set_structure_field_by_id(structures, structure_type, structure_id, field_name, fieldvalue)
!!
!!    end subroutine set_structure_by_id_key
!!
!!
!!  !! delft_model_data, structure.f90
!!    subroutine set_structure_field_by_id(structures, structure_type, structure_id, field_name, fieldvalues)
!!    type(T_StructureSet) : structures
!!
!!    structptr = get_by_id_and_type(structures, id, structure_type)
!!
!!    select case(field_name)
!!       'x'
!!       ..
!!       default(
!!       select case (structure_type)
!!       case('pump') :
!!         call set_pump_field(structures(i)%pumpdata, field_name, fieldvalue)
!!       end select
!!    set_structure_field(structures, structure_type, structure_id, field_name, fieldvalue)
!!
!!    end subroutine set_structure_by_id_key
!!
!!    !! delft_model_data, pump.f90
!!    subroutine set_pump_field(pump, field_name, fieldvalue)
!!    type(t_pump), pointer :: pump
!!    select case(field_name)
!!    case ('capacity')
!!       pump%cap   = fieldvalue%x_0d_double
!!    case ('stage')
!!       pump%stage = fieldvalue%x_0d_integer
!!
!!!!!!!!!!!!!!! einde nieuwe voorstel, hieronder Fedors routine:
!
!include 'get_structure_by_id.inc'
!include 'set_structure_by_id.inc'
!
!end module c_structures
