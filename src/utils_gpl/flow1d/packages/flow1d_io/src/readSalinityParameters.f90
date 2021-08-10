module m_readSalinityParameters
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
!  $Id: readSalinityParameters.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_io/src/readSalinityParameters.f90 $
!-------------------------------------------------------------------------------

   use MessageHandling
   use ModelParameters
   use m_network
   use properties
   use m_hash_search

   implicit none

   private

   public readSalinityParameters

   contains

   subroutine readSalinityParameters(network, salinityFile)
      use m_GlobalParameters
      use m_df1d_transport
      use m_node
      use m_network
      
      implicit none
      
      type(t_network), intent(inout) :: network
      character*(*), intent(in)      :: salinityFile

      logical                                       :: success
      type(tree_data), pointer                      :: md_ptr 
      integer                                       :: istat
      integer                                       :: i
      integer                                       :: nbr
      integer                                       :: inode
      integer                                       :: ibr

      character(len=IdLen)                          :: nodeId
      character(len=IdLen)                          :: scheme
      type(t_mouthparameters)                       :: mp
      type(t_mouthparameters), pointer              :: pmp(:,:)
      character(len=5)                              :: parstring
      character(len=8)                              :: valString
     
      
      if (.not. transportPars%do_salt) then
         ! no salinity computation, nothing to do
         return
      endif
      
      call tree_create(trim(salinityFile), md_ptr, maxlenpar)
      call prop_file('ini',trim(salinityFile),md_ptr, istat)

      ! read numerical options
      transportPars%teta = 1.0d0
      call prop_get_double(md_ptr, 'NumericalOptions', 'teta', transportPars%teta, success)
      write(valstring, '(f7.4)') transportPars%teta
      call AddOrReplaceParameter('NumericalOptions', 'teta', valstring, .true.)

      transportPars%tidal_period = 12.417
      call prop_get_double(md_ptr, 'NumericalOptions', 'tidalPeriod', transportPars%tidal_period, success)
      write(valstring, '(f6.5)') transportPars%tidal_period
      call AddOrReplaceParameter('NumericalOptions', 'tidalPeriod', valstring, .true.)
      transportPars%tidal_period = transportPars%tidal_period*3600

      transportPars%n = 0.25

      scheme = 'vanLeer-2'
      call prop_get_string(md_ptr, 'NumericalOptions', 'advectionScheme', scheme, success)
      if (scheme == 'vanLeer-2') then
         transportPars%advection_scheme = ADV_VANLEER
         call AddOrReplaceParameter('NumericalOptions', 'advectionScheme', 'vanLeer-2', .true.)
      else
         transportPars%advection_scheme = ADV_UPWIND
         call AddOrReplaceParameter('NumericalOptions', 'advectionScheme', 'upwind', .true.)
      endif
            
      transportPars%use_f4_dispersion = .true.
            
      do i = 3, 11
         write(parstring, '(''c'', i0)') i
         call prop_get_double(md_ptr, 'NumericalOptions', parstring, transportPars%c(i), success)
         if (.not. success) then
            transportPars%use_f4_dispersion = .false.
            call setmessage(LEVEL_INFO, 'F4 dispersion ignored')
         endif 
      enddo

      ! reading mouth parameters allocate space for mouth parameters

      call prop_get_string(md_ptr, 'mouth', 'nodeId', nodeId, success)
      if (success .and. transportPars%use_f4_dispersion) then
         inode = hashsearch(network%nds%hashlist, nodeId)
         if (inode <=0) then
            call SetMessage(LEVEL_ERROR, 'Error Reading mouth parameters node id '''//trim(nodeId)//''' does not exist.')
            return
         endif
         transportPars%mouth => network%nds%node(inode)
         
         nbr = network%brs%Count
         allocate(network%trans%mp(2,nbr))
         
         transportPars%start_time_tidal_period = 0d0
         transportPars%start_tidal_period      = .false.
         transportPars%first_tidal_period      = .true.
         transportPars%tidal_period = 12d0*3600d0 + 25.2d0*60d0 
         

         mp%u_max = 0d0
         mp%Qf = 0d0
         mp%Pe = 0d0
         mp%au = 0d0
         mp%tp = transportPars%tidal_period
         
         nbr = network%brs%count
         pmp => network%trans%mp
         do i = 1, 2
            do ibr = 1, nbr
               pmp(i, ibr) = mp
            enddo
         enddo
         transportPars%use_f4_dispersion = .true.
         
      else
         transportPars%use_f4_dispersion = .false.
         call setmessage(LEVEL_INFO, 'F4 dispersion ignored')
      endif
      
      call tree_destroy(md_ptr)
      
   end subroutine readSalinityParameters

end module m_readSalinityParameters
