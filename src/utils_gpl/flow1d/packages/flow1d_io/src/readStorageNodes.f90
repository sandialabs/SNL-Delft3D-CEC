module m_readStorageNodes
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
!  $Id: readStorageNodes.f90 65913 2020-01-30 12:20:14Z dam_ar $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_io/src/readStorageNodes.f90 $
!-------------------------------------------------------------------------------

   use MessageHandling
   use m_network
   use m_Storage
   use m_tables

   use properties
   use m_hash_search
   use string_module, only: str_lower, strcmpi


   implicit none

   private

   public readStorageNodes
   
   ! Storage nodes file current version: 2.00
   integer, parameter :: storgNodesFileMajorVersion = 2
   integer, parameter :: storgNodesFileMinorVersion = 0
   
   ! History storage nodes file versions:

   ! 2.00 (2019-08-27): renamed to storage nodes, added x/y as possible location, added storage table option.
   ! 1.00 (2018-08-13): initial "urban" version of storage nodes ('retentions').

   contains

   !> Read storage nodes file, giving the file name
   subroutine readStorageNodes(network, storgNodesFile)

      implicit none
      
      type(t_network), intent(inout)                :: network
      character*(*),   intent(in   )                :: storgNodesFile

      logical                                       :: success
      logical                                       :: success1
      type(tree_data), pointer                      :: md_ptr
      type(tree_data), pointer                      :: node_ptr
      integer                                       :: istat
      integer                                       :: numstr
      integer                                       :: i

      character(len=IdLen)                          :: blockname
      character(len=IdLen)                          :: fileType
      character(len=IdLen)                          :: storgNodeId
      character(len=IdLen)                          :: storgNodeName
      character(len=IdLen)                          :: nodeId
      character(len=IdLen)                          :: sStorageType
      integer                                       :: storageType
      logical                                       :: useTable1
      
      double precision                              :: x, y
      double precision, allocatable, dimension(:)   :: x_tmp, y_tmp
      integer                                       :: branchIdx
      integer                                       :: nodeIdx
      integer                                       :: local_grid_index
      integer                                       :: gridPoint
      type(t_storage), pointer                      :: pSto
      
      double precision                              :: bedLevel
      double precision                              :: area
      double precision, allocatable, dimension(:)   :: streetLevel
      double precision, allocatable, dimension(:)   :: streetStorageArea
      integer                                       :: numLevels
      character(len=IdLen)                          :: sInterpolate
      integer                                       :: interpol
      
      logical                                       :: useStreetStorage
      double precision, allocatable, dimension(:)   :: storagelevels
      double precision, allocatable, dimension(:)   :: storageAreas
      integer                                       :: major, minor
      integer                                       :: jaxy, jageneral

      call tree_create(trim(storgNodesFile), md_ptr, maxlenpar)
      call prop_file('ini',trim(storgNodesFile),md_ptr, istat)
      
      ! check FileVersion
      major = 0
      minor = 0
      call prop_get_version_number(md_ptr, major = major, minor = minor, success = success1)
      if (.not. success1 .or. major < storgNodesFileMajorVersion) then
         write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of storage nodes file detected in '''//trim(storgNodesFile)//''': v', major, minor, '. Current format: v',storgNodesFileMajorVersion,storgNodesFileMinorVersion,'. Ignoring this file.'
         call warn_flush()
         goto 999
      end if
      
      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      jageneral = 0
      success = .true.
      ! Read each block
      do i = 1, numstr
         node_ptr => md_ptr%child_nodes(i)%node_ptr
         blockname = tree_get_name(node_ptr)
         
         if (strcmpi(blockname, 'general')) then  ! Read [General] block
            if (jageneral > 0) then
               write(msgbuf, '(3a)') 'Found more than one [General] blocks in file ''', trim(storgNodesFile), '''. Only the first [General] block is read, others are ignored.'
               call warn_flush()
               cycle
            end if
               
            ! read fileType
            call prop_get_string(node_ptr, '', 'fileType', fileType, success)
            if ((.not. success) .or. (.not. strcmpi(fileType,'storagenodes'))) then
               write(msgbuf, '(5a)') 'Wrong block in file ''', trim(storgNodesFile), ''': [', trim(blockname), ']. Field ''fileType'' is missing or not correct. Support fileType = storagenodes. Ignoring this file'
               call warn_flush()
               goto 999
            endif
            
            ! read useStreetStorage
            call prop_get_logical(node_ptr, '', 'useStreetStorage', useStreetStorage, success)
            if (.not. success) then
               useStreetStorage = .true.
               write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(storgNodesFile), ''': [', trim(blockname), ']. Field ''useStreetStorage'' is missing. Use default value useStreetStorage = true.'
               call warn_flush()
            endif
            jageneral = 1
            cycle
         else if (strcmpi(blockname,'StorageNode')) then   ! Read [StorageNode] block
            success = .true.
            jaxy    = 0
            ! read id
            call prop_get_string(node_ptr, '', 'id', storgNodeId, success1)
            if (.not. success1) then
               write (msgbuf, '(a,i0,a)') 'Error Reading storage node #', network%storS%Count + 1, ', id is missing.'
               call err_flush()
               success = .false.
            end if
            
            ! read name
            call prop_get_string(node_ptr, '', 'name', storgNodeName, success1)
            success = success .and. check_input(success1, storgNodeId, 'name')
            
            ! read nodeId
            call prop_get_string(node_ptr, '', 'nodeId', nodeId, success1)
            if (.not. success1) then
               ! read x-, y-coordinates
               call prop_get_double(node_ptr, '', 'x', x, success1)
               success = success .and. success1
               call prop_get_double(node_ptr, '', 'y', y, success1)
               success = success .and. success1
               if (.not. success) then
                  write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(storgNodesFile), ''': [', trim(blockname), ']. Either "nodeId" or "x, y" must be specified.'
                  call err_flush()
               else
                  jaxy      = 1
                  gridPoint = -1
               end if
            else
               nodeIdx = hashsearch(network%nds%hashlist, nodeId)
               if (nodeIdx <= 0) Then
                  call SetMessage(LEVEL_ERROR, 'Error Reading Storage Node '''//trim(storgNodeID)//''': node: '''//trim(nodeID)//''' not Found.')
                  exit
               endif
            end if
            
            ! read useTable
            call prop_get_logical(node_ptr, '', 'useTable', useTable1, success1)
            success = success .and. check_input(success1, storgNodeId, 'useTable')
            
            if (.not. useTable1) then
               numLevels = 1
            else
               ! read numLevels
               call prop_get_integer(node_ptr, '', 'numLevels', numLevels, success1)
               success = success .and. check_input(success1, storgNodeId, 'numLevels')
            end if
            
            ! Allocate Arrays storageLevels, storageAreas
            call realloc(storageLevels, numLevels, stat=istat)
            if (istat == 0) then
               call realloc(storageAreas, numLevels, stat=istat, fill = 0d0)
            else
               call SetMessage(LEVEL_FATAL, 'Reading storage nodes: Error Allocating Arrays')
            endif
            
            ! read data
            if (.not. useTable1) then
               call realloc(streetLevel, numLevels, stat=istat)
               if (istat == 0) then
                  call realloc(streetStorageArea, numLevels, stat=istat, fill = 0d0)
               else
                  call SetMessage(LEVEL_FATAL, 'Reading storage nodes: Error Allocating Arrays')
               endif
            
               sInterpolate = 'block'
               call interpolateStringToInteger(sInterpolate, interpol)
               ! read bedLevel
               call prop_get_double(node_ptr, '', 'bedLevel', storageLevels(1), success1)
               success = success .and. check_input(success1, storgNodeId, 'bedLevel')
               
               ! read area
               call prop_get_double(node_ptr, '', 'area', storageAreas(1), success1)
               success = success .and. check_input(success1, storgNodeId, 'area')   
               
               ! read streetLevel
               call prop_get_double(node_ptr, '', 'streetLevel', streetLevel(1), success1)
               success = success .and. check_input(success1, storgNodeId, 'streetLevel')
               
               ! read storageType
               call prop_get_string(node_ptr, '', 'storageType', sStorageType, success1)
               if (.not. success1) then 
                  sStorageType = 'reservoir'
               end if
               
               call storageTypeStringToInteger(sStorageType, StorageType)
               if (StorageType /= nt_Reservoir .and. StorageType /= nt_Closed) then
                  write(msgbuf, '(5a)') 'Wrong block in file ''', trim(storgNodesFile), ''': [', trim(blockname), ']. Field ''storageType'' is not correct. Supported values are "reservoir" and "closed".'
                  call err_flush()
               end if
               
               ! read streetStorageArea
               if (useStreetStorage) then
                  if (strcmpi(sStorageType, 'reservoir')) then
                     call prop_get_double(node_ptr, '', 'streetStorageArea', streetStorageArea(1), success1)
                     success = success .and. check_input(success1, storgNodeId, 'streetStorageArea')
                  else if (strcmpi(sStorageType, 'closed')) then
                     streetStorageArea(1) = slot_area
                  end if
               end if
            else
               ! read levels
               call prop_get_doubles(node_ptr, '', 'levels', storageLevels, numLevels, success1)
               success = success .and. check_input(success1, storgNodeId, 'levels')
               
               ! read storageArea
               call prop_get_doubles(node_ptr, '', 'storageArea', storageAreas, numLevels, success1)
               success = success .and. check_input(success1, storgNodeId, 'storageArea')
               
               ! read interpolate
               call prop_get_string(node_ptr, '', 'interpolate', sInterpolate, success1)
               if (.not. success1) then
                  sInterpolate = 'linear'
                  write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(storgNodesFile), ''': [', trim(blockname), ']. Field ''interpolate'' is missing. Use default value interpolate = linear.'
                  call warn_flush()
               end if
               if ((.not. strcmpi(sInterpolate, 'linear')) .and. (.not. strcmpi(sInterpolate, 'block'))) then
                  success1 = .false.
                  write(msgbuf, '(5a)') 'Wrong block in file ''', trim(storgNodesFile), ''': [', trim(blockname), ']. Field ''interpolate'' is not correct. Supported values are "linear" and "block".'
                  call err_flush()
               end if
               success = success .and. success1
               call interpolateStringToInteger(sInterpolate, interpol)
            end if
            
            if (storageAreas(1) <= 0d0) then
               call setMessage(LEVEL_ERROR, 'Area at Bed Level for storage node ' // trim(storgNodeId) // ' <= 0.0. Please enter a positive value')
            endif
         end if
                     
         if (success) then ! If reading variables are successful, then store the obtained info. to the corresponding places
            network%storS%Count = network%storS%Count + 1
            if (network%storS%Count > network%storS%Size) then
               call realloc(network%storS)
            endif
      
            pSto => network%storS%stor(network%storS%Count)
            nullify(pSto%storageArea)
            nullify(pSto%streetArea)

            ! Bcause of the complicated data structure of SOBEK storage in 'connection nodes'
            ! must be separated from the ordinary gridpoints
            pSto%id        = storgNodeId
            pSto%name      = storgNodeName
            if (jaxy == 0) then
               pSto%nodeId    = nodeId
               pSto%node_index= nodeIdx
            else
               network%storS%Count_xy = network%storS%Count_xy + 1
               pSto%x         = x
               pSto%y         = y
               pSto%node_index = -1 ! node_index will be computed later when calling subroutine set_node_numbers_for_xy_storage_nodes 
            end if
            pSto%useStreetStorage = useStreetStorage
            pSto%useTable         = useTable1
            
            
            ! setTable
            call setTable(pSto%storageArea, interpol, storageLevels, storageAreas, numLevels)
            if (.not. useTable1) then
               pSto%storageType = storageType
               if (storageType == nt_Closed) then
                  network%storS%Count_closed = network%storS%Count_closed + 1
               end if
               if (useStreetStorage) then
                  call setTable(pSto%streetArea, interpol, streetLevel, streetStorageArea, numLevels)
               end if
            else
               pSto%storageType = nt_Reservoir
            end if               
         endif
      end do
      
      ! Clear Arrays
      istat = 0
      if (allocated(storageLevels)) deallocate(storageLevels, stat=istat)
      if (istat == 0 .and. allocated(storageAreas)) deallocate(storageAreas, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_ERROR, 'Reading storage nodes file: Error Deallocating Arrays')
      endif
      
      write(msgbuf,'(a,a,i10,a)') 'Done reading storage nodes file,', trim(storgNodesFile), network%storS%Count, ' storage nodes have been read.'
      call msg_flush()

      call fill_hashtable(network%storS)
999   continue
      call tree_destroy(md_ptr)

   end subroutine readStorageNodes
   
   !> Converts interpolate type string to an integer.
   !! Returns -1 when an invalid type string is given.
   subroutine interpolateStringToInteger(sinterpol, interpol)
      implicit none
      character(len=*), intent(in   ) :: sinterpol        !< interpolate type string
      integer,          intent(  out) :: interpol         !< interpolate type integer
      
      call str_lower(sinterpol)
      select case (trim(sinterpol))
         case ('linear')
            interpol = 0
         case ('block')
            interpol = 1
         case default
            interpol = -1
      end select
      return

   end subroutine interpolateStringToInteger
   
   !> Converts storage type string to an integer.
   !! Returns nt_None when an invalid type string is given.
   subroutine storageTypeStringToInteger(sStorgType, storgType)
      implicit none
      character(len=*), intent(in   ) :: sStorgType        !< storage type string
      integer,          intent(  out) :: storgType         !< storage type integer
      
      call str_lower(sStorgType)
      select case (trim(sStorgType))
         case ('reservoir')
            storgType = nt_Reservoir
         case ('closed')
            storgType = nt_Closed
         case default
            storgType = nt_None
      end select
      return

   end subroutine storageTypeStringToInteger

   !> Helper routine to check the result status of a read/prop_get action.
   !! Checks if success is true or false, when false generate an error message.
   !! Result value is the original success value.
   function check_input(success, st_id, key) result (res)
      logical         , intent(in   )    :: success   !< Result value of the prop_get subroutine.
      character(len=*), intent(in   )    :: st_id     !< Id of the current storage node.
      character(len=*), intent(in   )    :: key       !< Key of the input value.
      logical                            :: res       !< Result status, is equal to the original success value.
                                                      !< Recommended use: successall = successall .and. check_input_result(success, ..)

      if (.not. success) then
         write (msgbuf, '(a,a,a,a,a)') 'Error Reading storage Node ''', trim(st_id), ''', ''', trim(key), ''' is missing.'
         call err_flush()
      endif
      res = success
      return 
   end function check_input
end module m_readStorageNodes