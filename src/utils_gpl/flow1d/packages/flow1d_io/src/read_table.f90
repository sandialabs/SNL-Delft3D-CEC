module m_read_table
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
!  $Id: read_table.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_io/src/read_table.f90 $
!-------------------------------------------------------------------------------

   use m_tables
      
   implicit none
   
   private
   
   public read_table_cache
   public write_table_cache
   
   contains
   
   subroutine read_table_cache(ibin, table)
      
      integer, intent(in)          :: ibin
      type(t_table), pointer, intent(inout) :: table
 
      integer length, i
      
      allocate(table)
      
      read(ibin) length
      table%length = length
      
      allocate(table%x(length))
      read(ibin) (table%x(i), i = 1, length)
      
      allocate(table%y(length))
      read(ibin) (table%y(i), i = 1, length)

      read(ibin) table%interpoltype
      
   end subroutine read_table_cache   
   
   subroutine write_table_cache(ibin, table)

      integer, intent(in) :: ibin
      type(t_table), intent(in) :: table
 
      integer length, i
      
      length = table%length   
      write(ibin) length
      write(ibin) (table%x(i), i = 1, length)
      write(ibin) (table%y(i), i = 1, length)
      write(ibin) table%interpoltype
      
   end subroutine write_table_cache   
   
end module m_read_table
   