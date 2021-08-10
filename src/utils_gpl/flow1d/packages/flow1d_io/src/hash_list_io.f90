module m_hash_list
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
!  $Id: hash_list_io.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_io/src/hash_list_io.f90 $
!-------------------------------------------------------------------------------
   
   use m_hash_search
      
   implicit none
   
   private

   public read_hash_list_cache
   public write_hash_list_cache
   
   contains
   
   subroutine read_hash_list_cache(ibin, hash_list)
      
      integer, intent(in)             :: ibin
      type(t_hashlist), intent(inout) :: hash_list
 
      integer                         :: i

      read(ibin) hash_list%id_count
      
      if (hash_list%id_count <= 0) return  ! Nothing to Hash, Skip Rest
      
      read(ibin) hash_list%hashcon

      allocate(hash_list%id_list(hash_list%id_count))
      read(ibin) (hash_list%id_list(i), i = 1, hash_list%id_count)
      
      allocate(hash_list%hashfirst(0:hashcon))
      read(ibin) (hash_list%hashfirst(i), i = 0, hash_list%hashcon)

      allocate(hash_list%hashnext(hash_list%id_count))
      read(ibin) (hash_list%hashnext(i), i = 1, hash_list%id_count)

   end subroutine read_hash_list_cache
   
   subroutine write_hash_list_cache(ibin, hash_list)

      integer, intent(in)             :: ibin
      type(t_hashlist), intent(in)    :: hash_list
 
      integer                         :: i
      
      write(ibin) hash_list%id_count
      
      if (hash_list%id_count <= 0) return  ! Nothing to Hash, Skip Rest
      
      write(ibin) hash_list%hashcon
      
      write(ibin) (hash_list%id_list(i), i = 1, hash_list%id_count)
      write(ibin) (hash_list%hashfirst(i), i = 0, hash_list%hashcon)
      write(ibin) (hash_list%hashnext(i), i = 1, hash_list%id_count)
 
   end subroutine write_hash_list_cache 
   
end module m_hash_list
   