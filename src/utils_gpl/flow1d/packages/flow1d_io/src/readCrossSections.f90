module m_readCrossSections
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
!  $Id: readCrossSections.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_io/src/readCrossSections.f90 $
!-------------------------------------------------------------------------------

   use M_newcross
   use m_CrossSections
   use MessageHandling
   use properties
   use m_network
   use m_GlobalParameters
   use m_hash_search
   use m_spatial_data
   use string_module, only: strcmpi


   implicit none

   private
   
   public readCrossSectionDefinitions
   public readCrossSectionLocationFile
   public write_cross_section_definition_cache
   public write_cross_section_cache
   public write_convtab
   public read_convtab

   !> The file version number of the cross section definition file format: d.dd, [config_major].[config_minor], e.g., 1.03
   !!
   !! Note: read config_minor as a 2 digit-number, i.e., 1.1 > 1.02 (since .1 === .10 > .02).
   !! Convention for format version changes:
   !! * if a new format is backwards compatible with old files, only
   !!   the minor version number is incremented.
   !! * if a new format is not backwards compatible (i.e., old files
   !!   need to be converted/updated by user), then the major version number
   !!   is incremented.
   
   ! Cross section definition file current version: 3.00
   integer, parameter :: CrsDefFileMajorVersion      = 3
   integer, parameter :: CrsDefFileMinorVersion      = 0
   integer, parameter :: CrsDefFileMajorVersionSobek = 1  !< Version number as used in Sobek 3
   
   ! History cross section definition file versions:
   
   ! 3.00 (2019-06-18): use strings, instead of integers, for "closed" and "frictionType(s)".
   ! 2.00 (2019-05-29): A completely new description of cross section definition file. See more details in issue UNST-2387.
   ! 1.01 (2019-03-12): First version of *.ini type cross section definition file.

   contains
    
   !> Read the cross section location file
   subroutine readCrossSectionLocationFile(network, CrossSectionfile)
      use m_CrossSections
      type(t_network), intent(inout) :: network                 !< Network structure
      character(len=*), intent(in)   :: CrossSectionFile        !< name of the crossection location input file 

      type(tree_data), pointer       :: md_ptr
      integer                        :: istat
      integer                        :: numstr
      integer                        :: i
      character(len=IdLen)           :: branchid
      character(len=IdLen)           :: defid
      integer                        :: iref
      integer                        :: inext
      integer                        :: indx
      logical                        :: success
      type(t_CrossSection), pointer  :: pCrs
      type(t_CSType), pointer        :: pCrsDef
      character(len=Charln)          :: binfile
      logical                        :: file_exist
      integer                        :: pos, ibin
      integer                        :: numcrs


      pos = index(CrossSectionFile, '.', back = .true.)
      binfile = CrossSectionFile(1:pos)//'cache'
      inquire(file=binfile, exist=file_exist)
      if (doReadCache .and. file_exist) then
         open(newunit=ibin, file=binfile, status='old', form='unformatted', access='stream', action='read', iostat=istat)
         if (istat /= 0) then
            call setmessage(LEVEL_FATAL, 'Error opening Cross-Section Location Cache file')
            ibin = 0
         endif
         call read_cross_section_cache(ibin, network%crs, network%CSDefinitions)
         close(ibin)
         !call dumpCross(network%crs, 'dumpCrossCacheRead')
         return
      endif
      
      call tree_create(trim(CrossSectionfile), md_ptr, maxlenpar)
      call prop_file('ini',trim(CrossSectionfile),md_ptr,istat)
      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      success = .true.
      numcrs = 0
      do i = 1, numstr
         if (network%crs%count+1 > network%crs%size) then
            call realloc(network%crs)
         endif
         inext = network%crs%count+1
         pCrs => network%crs%cross(inext)
         
         if (.not. strcmpi(tree_get_name(md_ptr%child_nodes(i)%node_ptr), 'CrossSection')) then
            cycle
         else
            numcrs = numcrs + 1
         endif
         
         call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'id', pCrs%csid, success)
         if (.not. success) then
            write (msgbuf, '(a,i0,a)') 'Incorrect CrossSection input for CrossSection #', numcrs, &
               ' in '''//trim(CrossSectionFile)//'''. No id was given.'
            call err_flush()
            cycle
         endif

         call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'branchId', branchid, success)
         if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection id '''//trim(pCrs%csid)// &
               ''' in '''//trim(CrossSectionFile)//'''. No branchId was given.')
            cycle
         endif
         
         indx = hashsearch(network%brs%hashlist, branchid)
         pCrs%branchid = indx
         call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'chainage', pCrs%chainage, success)
         if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection on branch '//trim(branchid)// &
               '. No chainage was given.')
            cycle
         endif
         call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'definitionId', defid, success)
         if (.not. success) call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'definition', defid, success) ! Backwards compatibility
         if (success) then
            iref = hashsearch(network%CSDefinitions%hashlist, defid)
            if (iref < 1) then
               call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection '''//trim(pCrs%csid)//''' on branch '''//trim(branchid)// &
                     '''. Specified definitionId '''//trim(defid)//''' was not found in definitions.')
               cycle
            endif
         else
            call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection '''//trim(pCrs%csid)//''' on branch '''//trim(branchid)// &
                  '''. No definitionId was given.')
            cycle
         end if

         pCrs%bedLevel = 0.0d0
         call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'shift', pCrs%shift, success)
         if (.not. success) pCrs%shift = 0.0d0
         
         pCrs%itabDef             = iref
         pCrs%tabDef              => network%CSDefinitions%CS(iref)
         
         call SetParsCross(network%CSDefinitions%CS(iref), network%crs%cross(inext))
         pCrs => network%crs%cross(inext)
         
         ! Allocate and Copy Section Data form Definition
         if (pCrs%tabDef%frictionSectionsCount > 0) then
            allocate(pCrs%frictionSectionID(pCrs%tabDef%frictionSectionsCount))      !< Friction Section Identification
            allocate(pCrs%frictionSectionFrom(pCrs%tabDef%frictionSectionsCount))    !<
            allocate(pCrs%frictionSectionTo(pCrs%tabDef%frictionSectionsCount))      !<
            allocate(pCrs%frictionTypePos(pCrs%tabDef%frictionSectionsCount))        !< Friction type for positive flow direction
            allocate(pCrs%frictionValuePos(pCrs%tabDef%frictionSectionsCount))       !< Friction value for positive flow direction
            allocate(pCrs%frictionTypeNeg(pCrs%tabDef%frictionSectionsCount))        !< Friction type for negative flow direction
            allocate(pCrs%frictionValueNeg(pCrs%tabDef%frictionSectionsCount))       !< Friction value for negative flow direction
            call realloc(pCrs%tabdef%frictionSectionFrom, pCrs%tabDef%frictionSectionsCount)
            call realloc(pCrs%tabdef%frictionSectionto, pCrs%tabDef%frictionSectionsCount)

            pCrs%frictionSectionsCount = pCrs%tabDef%frictionSectionsCount
            pCrs%frictionSectionID     = pCrs%tabDef%frictionSectionID
            pCrs%frictionSectionFrom   = pCrs%tabDef%frictionSectionFrom
            pCrs%frictionSectionTo     = pCrs%tabDef%frictionSectionTo

         endif
         
         network%crs%count = inext
         
         ! Retrieve Roughness for Profile from Spatial Data
         call GetRougnessForProfile(network, network%crs%cross(inext))
         if (network%CSDefinitions%CS(iref)%crossType == cs_YZ_Prof) then
            ! Prematurely to facilitate Conveyance Data to Delta Shell
            call CalcConveyance(network%crs%cross(inext))
            
         endif
         
      end do

      call tree_destroy(md_ptr)
      
   end subroutine readCrossSectionLocationFile

   !> Read the cross section definitions file, taking the file version into account.
   subroutine readCrossSectionDefinitions(network, CrossSectionDefinitionFile)

      type(t_network), target, intent(inout) :: network                          !< network structure
      character(len=*), intent(in)           :: CrossSectionDefinitionFile       !< name of the cross section definition file
      type(tree_data), pointer  :: md_ptr
      integer :: istat
      logical :: success
      integer                       :: pos
      integer                       :: ibin = 0
      character(len=Charln)         :: binfile
      logical                       :: file_exist
      character(len=Idlen)          :: fileVersion
      integer                       :: major 
      integer                       :: minor
      
      pos = index(CrossSectionDefinitionFile, '.', back = .true.)
      binfile = CrossSectionDefinitionFile(1:pos)//'cache'
      inquire(file=binfile, exist=file_exist)
      if (doReadCache .and. file_exist) then
         open(newunit=ibin, file=binfile, status='old', form='unformatted', access='stream', action='read', iostat=istat)
         if (istat /= 0) then
            call setmessage(LEVEL_FATAL, 'Error opening Cross-Section Definition Cache file')
            ibin = 0
         endif
         call read_cross_section_definition_cache(ibin, network%CSDefinitions)
         close(ibin)
         ibin = 0
         !call dumpCrossDefs(network%CSDefinitions, 'dumpCrossDefCacheRead')
         return
      endif

      call tree_create(trim(CrossSectionDefinitionFile), md_ptr, maxlenpar)
      call prop_file('ini',trim(CrossSectionDefinitionFile),md_ptr,istat)

      call prop_get_version_number(md_ptr, major = major, minor = minor, success = success)
      if (.not. success) then
         major = 1
         minor = 0
      endif
      
      select case (major)
      case (CrsDefFileMajorVersionSobek)
         call parseCrossSectionDefinitionFile_v100(md_ptr, network)
      case (CrsDefFileMajorVersion)
         call parseCrossSectionDefinitionFile(md_ptr, network)
      case default
         call SetMessage(LEVEL_FATAL,'Unsupported fileVersion for cross section definition file:'//trim(fileVersion))
      end select

      call tree_destroy(md_ptr)
      call fill_hashtable(network%CSDefinitions)
      
   end subroutine readCrossSectionDefinitions
      
      
   !> Parse cross section definition file of the current version.
   !! file must already have been read into an ini tree.
   subroutine parseCrossSectionDefinitionFile(md_ptr, network)
      use m_hash_search
      use string_module, only: strcmpi
      use m_read_roughness, only: frictionTypeStringToInteger
   
      type(t_network), target,  intent(inout)   :: network        !< network structure
      type(tree_data), pointer, intent(in   )   :: md_ptr         !< treedata pointer to cross section definitions, already created.
   
      !integer :: istat
      integer :: numstr
      integer :: i, j
      integer :: crosstype
      logical :: success
      character(len=IdLen) :: id
      character(len=IdLen) :: typestr
      double precision :: diameter
      integer :: numLevels
      double precision, allocatable :: level(:)
      double precision, allocatable :: width(:)
      double precision              :: plains(3)
      double precision              :: crestLevel
      double precision              :: baseLevel
      double precision              :: flowArea
      double precision              :: totalArea
      logical                       :: closed
      
      
      logical                       :: groundlayerUsed = .false.
      double precision              :: groundlayer
      double precision              :: height
      integer                       :: inext
      logical                       :: plural                 !< indicates whether friction input is plural or not (e.g. frictionId or frictionIds)
      type(t_CSType), pointer       :: pCS
      character(len=IdLen), allocatable :: fricTypes(:)
      integer                       :: maxnumsections ! Max number of friction sections, to realloc some arrays
      
      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      maxnumsections = 3

  crs:do i = 1, numstr
         
         if (.not. strcmpi(tree_get_name(md_ptr%child_nodes(i)%node_ptr), 'Definition')) then
            cycle
         endif
         
         call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'id', id, success)
         if (.not. success) then
            write (msgbuf, '(a,i0,a)') 'Incorrect CrossSection input for CrossSection #', i, '. No id was given.'
            call err_flush()
            cycle
         endif
         call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'type', typestr, success)
         if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection '//trim(id)// &
               '. No type was given.')
            cycle
         endif
         crossType = getCrosstype(typestr)

         inext = network%CSDefinitions%count + 1
         if (network%CSDefinitions%count + 1 > network%CSDefinitions%size) then
            call realloc(network%CSDefinitions)
         endif
         
         plural = .false.

         pCS => network%CSDefinitions%CS(inext)
         pCS%id = id
         pCS%crossType = crosstype
         
         select case (crossType)
         case(CS_TABULATED)
            
            if (trim(typestr) == 'zwRiver') then
               plural = .true.
            endif
            success = readTabulatedCS(pCS, md_ptr%child_nodes(i)%node_ptr) 
            
         case(CS_RECTANGLE)
            
            numlevels = 1
            allocate(level(numlevels + 2))
            allocate(width(numlevels + 2))
            level(numlevels) = 0.0d0
            call prop_get_doubles(md_ptr%child_nodes(i)%node_ptr, '', 'width', width, numlevels, success)
            if (.not. success) then
                call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection Definition with type '//trim(typestr)//' and id: '//trim(id)// &
               '. No width was given.')
            endif
            
            call prop_get_logical(md_ptr%child_nodes(i)%node_ptr, '', 'closed', closed, success)
            if (.not. success) closed = .false. ! Default
            
            if (closed) then
               call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'height', height, success)
               if (.not. success) then
                  call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection Definition with type '//trim(typestr)//' and id: '//trim(id)// &
                  '. No height was given.')
                  cycle
               endif
               numlevels = numlevels + 1
               level(numLevels) = height
               width(numLevels) = width(1)
               
            endif

            plains     = 0.0d0
            crestLevel = 0.0d0
            baseLevel  = 0.0d0
            flowArea   = 0.0d0
            totalArea  = 0.0d0

            pCs%frictionSectionsCount = 1
                
            inext = AddCrossSectionDefinition(network%CSDefinitions, id, numLevels, level, width,               &
                                              width, plains, crestLevel, baseLevel, flowArea, totalArea,        &
                                              closed, groundlayerUsed, groundlayer)
            deallocate(level, width)            
         
         case(CS_CIRCLE, CS_EGG)
            success = .true.
            ! use analytical description of circle and egg profile
            call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'diameter', diameter, success)
            if (.not. success) then
               call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection input for CrossSection Definition with type '//trim(typestr)//' and id: '//trim(id)// &
               '. No diameter was given.')
            endif

            pCs%frictionSectionsCount = 1
            inext = AddCrossSectionDefinition(network%CSDefinitions, id, diameter, crossType, groundlayerUsed, groundlayer)
            
         case(CS_YZ_PROF)
            plural = .true.
            success = readYZCS(pCS, md_ptr%child_nodes(i)%node_ptr, network%sferic) 
            
         case default
            call SetMessage(LEVEL_ERROR, 'Incorrect CrossSection type for CrossSection Definition with given type '//trim(typestr)//' and id: '//trim(id))
            success = .false.
            
         end select
            
         allocate(pCs%frictionSectionID  (pCs%frictionSectionsCount))      !< Friction Section Identification
         allocate(pCS%frictionSectionIndex(pCs%frictionSectionsCount))
         allocate(pCS%frictionType       (pCs%frictionSectionsCount))
         maxnumsections = max(maxnumsections, pCs%frictionSectionsCount)
         call realloc(fricTypes, maxnumsections, keepExisting = .false.)
         allocate(pCS%frictionValue      (pCs%frictionSectionsCount))

         if (plural) then
            call prop_get_strings(md_ptr%child_nodes(i)%node_ptr, '', 'frictionIds', pCs%frictionSectionsCount, pCS%frictionSectionID, success)
         else
            call prop_get_strings(md_ptr%child_nodes(i)%node_ptr, '', 'frictionId', pCs%frictionSectionsCount, pCS%frictionSectionID, success)
         endif
            
         if (.not. success) then
            if (plural) then
               call prop_get_strings(md_ptr%child_nodes(i)%node_ptr, '', 'frictionTypes', pCs%frictionSectionsCount, fricTypes, success)
            else
               call prop_get_strings(md_ptr%child_nodes(i)%node_ptr, '', 'frictionType' , pCs%frictionSectionsCount, fricTypes, success)
            end if
            
            if (success) then
               do j = 1, pCs%frictionSectionsCount
                  call frictionTypeStringToInteger(fricTypes(j), pCS%frictionType(j))
                  if (pCS%frictionType(j) < 0) then
                     write(msgbuf, '(a,i0,a)') 'Incorrect CrossSection input for CrossSection Definition with type '//trim(typestr)//' and id: '//trim(id)// &
                                               '. frictionType '''//trim(fricTypes(j))//''' is wrong in section #', j, '.'
                     call err_flush()
                     cycle crs ! Skip this entire cross section
                  endif
               end do
               
               if (plural) then                  
                  call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'frictionValues', pCS%frictionValue, pCs%frictionSectionsCount, success)
               else
                  call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'frictionValue' , pCS%frictionValue, pCs%frictionSectionsCount, success)
               end if
            endif
               
            if (.not. success) then
               pCs%frictionSectionID(1) = 'Main'
               if (pCs%frictionSectionsCount >=2) then
                  pCs%frictionSectionID(2) = 'FloodPlain1'
               endif
               if (pCs%frictionSectionsCount ==3) then
                  pCs%frictionSectionID(3) = 'FloodPlain2'
               endif
            else
               do j = 1, pCs%frictionSectionsCount
                  pCS%frictionSectionID(j) = ''
               enddo
            endif
               
         endif
         success = .true.
         
         if (success) then
            network%CSDefinitions%count = inext
         endif
         
         do j = 1, pCs%frictionSectionsCount
            pCs%frictionSectionIndex(j) = hashsearch(network%rgs%hashlist, pCS%frictionSectionID(j))
         enddo
         
      enddo crs

   end subroutine parseCrossSectionDefinitionFile

   !> read tabulated cross section definition from treedata input
   logical function readTabulatedCS(pCS, node_ptr)  
   
      use precision_basics
      
      type(t_CSType), pointer, intent(inout) :: pCS           !< cross section definition
      type(tree_data), pointer, intent(in)   :: node_ptr      !< treedata node pointer to current cross section definition
      
      integer          :: numlevels, level_index_intersect
      logical          :: success
      double precision :: crestLevel
      double precision :: baseLevel
      double precision :: maxFlowWidth
      double precision :: Main
      double precision :: FP1
      double precision :: FP2
      double precision :: flowArea
      double precision :: totalArea
      double precision :: wintersect
      double precision :: factor
      double precision, dimension(:), allocatable :: height
      double precision, dimension(:), allocatable :: width
      double precision, dimension(:), allocatable :: totalwidth
      integer :: i, j, k
      double precision, parameter   :: eps = 1d-5

      numlevels = 0
      readTabulatedCS= .false.
      call prop_get_integer(node_ptr, '', 'numLevels', numlevels, success)
      if (numlevels == 0) then
            call SetMessage(LEVEL_ERROR, 'Incorrect Cross-Section input for Cross-Section Definition with id: '//trim(pCS%id)//'. NumLevels should be > 0.')
            return
      end if

      ! reserve space for extra support points at main - floodplain1 - floodplain2 transitions
      allocate(height(numlevels+2))
      allocate(width(numlevels+2))
      allocate(TotalWidth(numlevels+2))
   !
      pCS%levelsCount = numlevels
      
      if (success) then
         call prop_get_doubles(node_ptr, '', 'levels', height, numlevels, success)
      endif
      if (success) then
         call prop_get_doubles(node_ptr, '', 'flowWidths', width, numlevels, success)
      endif
      if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'Incorrect Cross-Section input for Cross-Section Definition id: '//trim(pCS%id)//'. Invalid levels/widths.')
            return
      endif
      call prop_get_double(node_ptr, '', 'totalWidths', totalWidth(1), success)
      if (success) then
         call prop_get_doubles(node_ptr, '', 'totalWidths', totalWidth, numlevels, success)
      else
         totalWidth = width
      endif
   
      ! summerdike
      
      call prop_get_double(node_ptr, '', 'leveeCrestLevel', crestLevel, success)
      if (success) call prop_get_double(node_ptr, '', 'leveeBaseLevel', baseLevel, success)
      if (success) call prop_get_double(node_ptr, '', 'leveeFlowArea',  flowArea,  success)
      if (success) call prop_get_double(node_ptr, '', 'leveeTotalArea', totalArea, success)
      if (success) then
         if (flowArea > 1.0d-5 .or. totalArea > 1.0d-5) then
            allocate(pCS%summerdike)
            anySummerDike = .true.
            pCS%summerdike%crestLevel = crestLevel
            pCS%summerdike%baseLevel  = baseLevel
            pCS%summerdike%flowArea   = flowArea
            pCS%summerdike%totalArea  = totalArea
         endif
      endif
      
      ! Initialize groundlayer information of the newly added cross-section
      allocate(pCS%groundlayer)
      
      pCS%closed = .false.
            
      pCS%plains = 0.0d0
      
      maxFlowWidth = width(1)
      do i = 2, numlevels
         maxFlowWidth = max( maxFlowWidth, width(i))
      enddo
      
      call prop_get_double(node_ptr, '', 'mainWidth', Main, success)
      if (.not. success)  Main = maxFlowWidth
      call prop_get_double(node_ptr, '', 'fp1Width', FP1, success)
      if (.not. success)  FP1 = 0.0d0
      call prop_get_double(node_ptr, '', 'fp2Width', FP2, success)
      if (.not. success)  FP2 = 0.0d0

      ! Check and Make Consistent if Needed
      if ((Main + FP1 + FP2) < (maxFlowWidth) - 0.001d0) then
            call SetMessage(LEVEL_ERROR, 'Sum of all Sections less than Flow Width for Cross-Section Definition ID: '//trim(pCS%id))
      endif
         
      if (FP1 <= 0.0d0 .and. FP2 > 0) then
            call SetMessage(LEVEL_ERROR, 'Floodplain2 only allowed when Floodplain1 exists for Cross-Section Definition ID: '//trim(pCS%id))
      endif
         
      ! Compensate for round off if needed
      if ((Main + FP1 + FP2) < maxFlowWidth) then
         Main = Main + 0.001d0
      endif 
            
      pCS%plains(1) = Main
      pCS%plains(2) = FP1
      pCS%plains(3) = FP2
                  
      if ( (pCS%plains(2) == 0.0d0) .and. (pCS%plains(3) == 0.0d0) ) then
         pCs%plainsLocation(1) = numlevels
         pCs%plainsLocation(2) = 0
         pCs%plainsLocation(3) = 0
      else
       
         ! make sure transitions main - floodplain1 and floodplain1 - floodplain2 are always in table
        
         wintersect = 0d0
         do i = 1, 2
            wintersect = wintersect + pCs%plains(i)
            level_index_intersect = 0
            do j = 1, numlevels
               if (wintersect+1d-5 < width(j)) then
                  ! found an intersection
                  level_index_intersect = j
                  exit
               endif
            enddo
            if ( level_index_intersect /= 0) then
               if (j == 1) then
                  pCs%plains(1) = width(1)
               elseif ( abs(wintersect - width(level_index_intersect-1) ) < 1d-5 ) then
                  pCs%plainsLocation(i) = level_index_intersect-1
               elseif ( abs(wintersect - width(level_index_intersect) ) < 1d-5 ) then
                  pCs%plainsLocation(i) = level_index_intersect
               else
                  ! extra level needed.
                  factor = (wintersect - width(level_index_intersect-1))/(width(level_index_intersect) - width(level_index_intersect-1))
                  do k = numlevels+1, level_index_intersect, -1
                     width(k) = width(k-1)
                     height(k) = height(k-1)
                     totalwidth(k) = totalwidth(k-1)
                  enddo
                  width(level_index_intersect)      = factor * width(level_index_intersect+1)      + (1d0-factor) * width(level_index_intersect)
                  height(level_index_intersect)     = factor * height(level_index_intersect+1)     + (1d0-factor) * height(level_index_intersect)
                  totalwidth(level_index_intersect) = factor * totalwidth(level_index_intersect+1) + (1d0-factor) * totalwidth(level_index_intersect)
                  pCs%plainsLocation(i) = level_index_intersect
                  numlevels = numlevels+1
               endif
            elseif (comparerealdouble(wintersect, width(numlevels), eps) == 0) then
                pCs%plainsLocation(i) = numlevels
            endif
         
         enddo
         pCs%plainsLocation(3) = numlevels
      endif
      
      call realloc(pCS%height, numlevels)
      call realloc(pCS%flowWidth, numlevels)
      call realloc(pCS%totalWidth, numlevels)

      call realloc(pCS%af_sub, 3, numlevels)
      call realloc(pCS%width_sub, 3, numlevels)
      call realloc(pCS%perim_sub, 3, numlevels)
      call realloc(pCS%flowArea, numlevels)
      call realloc(pCS%wetPerimeter, numlevels)
      call realloc(pCS%totalArea, numlevels)
      call realloc(pCS%area_min, numlevels)
      call realloc(pCS%width_min, numlevels)
      
      pCs%levelsCount = numlevels
      pCS%height      = height(1:numlevels)
      pCS%flowWidth   = width(1:numlevels)
      pCS%totalWidth  = totalwidth(1:numlevels)
      
      if (pCs%plains(3) > 0.0d0) then
         pCs%frictionSectionsCount = 3
      elseif (pCs%plains(2) > 0.0d0) then
         pCs%frictionSectionsCount = 2
      else
         pCs%frictionSectionsCount = 1
      endif
            
      ! Create Interpolation Tables
      call createTablesForTabulatedProfile(pCs)
      
      deallocate(height)
      deallocate(width)
      deallocate(TotalWidth)

      readTabulatedCS =  .true.
      
   end function readTabulatedCS

   !> read tabulated cross section definition from treedata input
   logical function readTabulatedCS_v100(pCS, node_ptr)  
   
      use precision_basics
      
      type(t_CSType), pointer, intent(inout) :: pCS           !< cross section definition
      type(tree_data), pointer, intent(in)   :: node_ptr      !< treedata node pointer to current cross section definition
      
      integer          :: numlevels, level_index_intersect
      logical          :: success
      double precision :: crestLevel
      double precision :: baseLevel
      double precision :: maxFlowWidth
      double precision :: Main
      double precision :: FP1
      double precision :: FP2
      double precision :: flowArea
      double precision :: totalArea
      double precision :: wintersect
      double precision :: factor
      double precision, dimension(:), allocatable :: height
      double precision, dimension(:), allocatable :: width
      double precision, dimension(:), allocatable :: totalwidth
      integer :: i, j, k
      double precision, parameter   :: eps = 1d-5

      numlevels = 0
      readTabulatedCS_v100= .false.
      call prop_get_integer(node_ptr, '', 'numLevels', numlevels, success)
      if (numlevels == 0) then
            call SetMessage(LEVEL_ERROR, 'Incorrect Cross-Section input for Cross-Section Definition with id: '//trim(pCS%id)//'. NumLevels should be > 0.')
            return
      end if

      ! reserve space for extra support points at main - floodplain1 - floodplain2 transitions
      allocate(height(numlevels+2))
      allocate(width(numlevels+2))
      allocate(TotalWidth(numlevels+2))
   !
      pCS%levelsCount = numlevels
      
      if (success) then
         call prop_get_doubles(node_ptr, '', 'levels', height, numlevels, success)
      endif
      if (success) then
         call prop_get_doubles(node_ptr, '', 'flowWidths', width, numlevels, success)
      endif
      if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'Incorrect Cross-Section input for Cross-Section Definition id: '//trim(pCS%id)//'. Invalid levels/widths.')
            return
      endif
      call prop_get_double(node_ptr, '', 'totalWidths', totalWidth(1), success)
      if (success) then
         call prop_get_doubles(node_ptr, '', 'totalWidths', totalWidth, numlevels, success)
      else
         totalWidth = width
      endif
   
      ! summerdike
      
      call prop_get_double(node_ptr, '', 'sd_crest', crestLevel, success)
      if (success) call prop_get_double(node_ptr, '', 'sd_baseLevel', baseLevel, success)
      if (success) call prop_get_double(node_ptr, '', 'sd_flowArea',  flowArea,  success)
      if (success) call prop_get_double(node_ptr, '', 'sd_totalArea', totalArea, success)
      if (success) then
         if (flowArea > 1.0d-5 .or. totalArea > 1.0d-5) then
            allocate(pCS%summerdike)
            anySummerDike = .true.
            pCS%summerdike%crestLevel = crestLevel
            pCS%summerdike%baseLevel  = baseLevel
            pCS%summerdike%flowArea   = flowArea
            pCS%summerdike%totalArea  = totalArea
         endif
      endif
      
      ! Initialize groundlayer information of the newly added cross-section
      allocate(pCS%groundlayer)
      call prop_get_logical(node_ptr, '', 'groundlayerUsed', pCS%groundlayer%used, success)
      if (pCS%groundlayer%used .and. success) then
         anyGroundLayer = .true.
         call prop_get_double(node_ptr, '', 'groundlayer', pCS%groundlayer%thickness, success)
      else
         pCS%groundlayer%thickness = 0.0d0
      endif
      
      pCS%closed = .false.
            
      pCS%plains = 0.0d0
         
      if (width(numLevels) >= ThresholdForPreismannLock) then
      
         maxFlowWidth = width(numlevels)

         call prop_get_double(node_ptr, '', 'main', Main, success)
         if (.not. success)  Main = 0.0d0
         call prop_get_double(node_ptr, '', 'floodPlain1', FP1, success)
         if (.not. success)  FP1 = 0.0d0
         call prop_get_double(node_ptr, '', 'floodPlain2',FP2, success)
         if (.not. success)  FP2 = 0.0d0

         ! Check and Make Consistent if Needed
         if ((Main + FP1 + FP2) < (maxFlowWidth) - 0.001d0) then
             call SetMessage(LEVEL_ERROR, 'Sum of all Sections less than Flow Width for Cross-Section Definition ID: '//trim(pCS%id))
         elseif (FP1 <= 0.0d0 .and. FP2 > 0) then
             call SetMessage(LEVEL_ERROR, 'Floodplain2 only allowed when Floodplain1 exists for Cross-Section Definition ID: '//trim(pCS%id))
         else
         
            ! Compensate for rounf off if needed
            if ((Main + FP1 + FP2) < maxFlowWidth) then
               Main = Main + 0.001d0
               endif 
         
            if (Main >= maxFlowWidth) then
               Main = maxFlowWidth
               FP1  = 0.0d0
               FP2  = 0.0d0
            elseif ((Main + FP1) >= maxFlowWidth) then
               FP1 = maxFlowWidth - Main
               FP2 = 0.0d0
            else
               FP2 = maxFlowWidth - Main - FP1
               endif
            endif                  
            
            pCS%plains(1) = Main
            pCS%plains(2) = FP1
            pCS%plains(3) = FP2
                  
         else
            pCS%plains(1) = maxval(width(1:numlevels))
            pCS%plains(2) = 0.0d0
            pCS%plains(3) = 0.0d0
         endif
         
      if ( (pCS%plains(2) == 0.0d0) .and. (pCS%plains(3) == 0.0d0) ) then
         pCs%plainsLocation(1) = numlevels
         pCs%plainsLocation(2) = 0
         pCs%plainsLocation(3) = 0
      else
       
         ! make sure transitions main - floodplain1 and floodplain1 - floodplain2 are always in table
        
         wintersect = 0d0
         do i = 1, 2
            wintersect = wintersect + pCs%plains(i)
            level_index_intersect = 0
            do j = 1, numlevels
               if (wintersect+1d-5 < width(j)) then
                  ! found an intersection
                  level_index_intersect = j
                  exit
               endif
            enddo
            if ( level_index_intersect /= 0) then
               if (j == 1) then
                  pCs%plains(1) = width(1)
               elseif ( abs(wintersect - width(level_index_intersect-1) ) < 1d-5 ) then
                  pCs%plainsLocation(i) = level_index_intersect-1
               elseif ( abs(wintersect - width(level_index_intersect) ) < 1d-5 ) then
                  pCs%plainsLocation(i) = level_index_intersect
               else
                  ! extra level needed.
                  factor = (wintersect - width(level_index_intersect-1))/(width(level_index_intersect) - width(level_index_intersect-1))
                  do k = numlevels+1, level_index_intersect, -1
                     width(k) = width(k-1)
                     height(k) = height(k-1)
                     totalwidth(k) = totalwidth(k-1)
                  enddo
                  width(level_index_intersect)      = factor * width(level_index_intersect+1)      + (1d0-factor) * width(level_index_intersect)
                  height(level_index_intersect)     = factor * height(level_index_intersect+1)     + (1d0-factor) * height(level_index_intersect)
                  totalwidth(level_index_intersect) = factor * totalwidth(level_index_intersect+1) + (1d0-factor) * totalwidth(level_index_intersect)
                  pCs%plainsLocation(i) = level_index_intersect
                  numlevels = numlevels+1
               endif
            elseif (comparerealdouble(wintersect, width(numlevels), eps) == 0) then
                pCs%plainsLocation(i) = numlevels
            endif
         
         enddo
         pCs%plainsLocation(3) = numlevels
      endif
      
      
      call realloc(pCS%height, numlevels)
      call realloc(pCS%flowWidth, numlevels)
      call realloc(pCS%totalWidth, numlevels)

      call realloc(pCS%af_sub, 3, numlevels)
      call realloc(pCS%width_sub, 3, numlevels)
      call realloc(pCS%perim_sub, 3, numlevels)
      call realloc(pCS%flowArea, numlevels)
      call realloc(pCS%wetPerimeter, numlevels)
      call realloc(pCS%totalArea, numlevels)
      call realloc(pCS%area_min, numlevels)
      call realloc(pCS%width_min, numlevels)
      
      pCs%levelsCount = numlevels
      pCS%height      = height(1:numlevels)
      pCS%flowWidth   = width(1:numlevels)
      pCS%totalWidth  = totalwidth(1:numlevels)
      
      if (pCs%plains(3) > 0.0d0) then
         pCs%frictionSectionsCount = 3
      elseif (pCs%plains(2) > 0.0d0) then
         pCs%frictionSectionsCount = 2
      else
         pCs%frictionSectionsCount = 1
      endif
            
      ! Create Interpolation Tables
      call createTablesForTabulatedProfile(pCs)
      
      deallocate(height)
      deallocate(width)
      deallocate(TotalWidth)

      readTabulatedCS_v100 =  .true.
      
   end function readTabulatedCS_v100
   
   !> read YZ cross section from ini file
   logical function readYZCS(pCS, node_ptr, sferic) 
      use precision
      use physicalconsts, only: earth_radius
      
      type(t_CSType), pointer,  intent(inout) :: pCS             !< cross section item
      type(tree_data), pointer, intent(in)    :: node_ptr        !< treedata pointer to input for cross section
      logical                 , intent(in)    :: sferic          !< indicates whether spherical coordinates are used or metric
      integer :: numlevels
      integer :: frictionCount
      logical :: success, sferic_local
      double precision, allocatable, dimension(:) :: positions
      double precision, allocatable, dimension(:) :: xcoordinates, ycoordinates
      integer          :: i
      double precision :: locShift
      logical          :: xyz_cross_section 
      character(len=idlen) :: conv_text

      
      readYZCS = .false.
      sferic_local = .false.
      call prop_get_integer(node_ptr, '', 'yzCount', numlevels, success)
      if (.not. success) then
         xyz_cross_section = .true.
         call prop_get_integer(node_ptr, '', 'xyzCount', numlevels, success)
         ! only for xyz cross sections the coordinates may be spherical 
         sferic_local = sferic
      else
         xyz_cross_section = .false.
      endif
      
      if (success) call prop_get_integer(node_ptr, '', 'sectionCount', frictionCount, success)
      if (.not. success .or. numLevels <= 0 .or. frictionCount <= 0) then
            call SetMessage(LEVEL_ERROR, 'Error while reading number of levels/sections for YZ-Cross-Section Definition ID: '//trim(pCS%id))
            return
      endif

      pCS%conveyanceType = CS_VERT_SEGM
      conv_text = 'segmented'
      call prop_get(node_ptr, '', 'conveyance', conv_text)
      if (trim(conv_text) =='segmented') then
         pCS%conveyanceType = CS_VERT_SEGM
      elseif(trim(conv_text) =='lumped') then
         pCS%conveyanceType = CS_LUMPED
      else
         msgbuf = 'Incorrect conveyance type for cross section definition '//trim(pCS%id)//': '//trim(conv_text)
      endif
         
      if (frictionCount > 1 .and. pCS%conveyanceType == CS_LUMPED) then
         msgbuf = 'In cross section definition '//trim(pCS%id)//' lumped conveyance in combination with multiple friction sections is used, this is not allowed'
         call err_flush()
      endif
      
      pCS%levelsCount           = numLevels
      pCS%frictionSectionsCount = frictionCount
      pCS%storLevelsCount       = 0
      
      call realloc(xcoordinates, numlevels)
      call realloc(ycoordinates, numlevels)
      call realloc(pCS%y, numlevels)
      call realloc(pCS%z, numlevels)
      call realloc(pCS%storLevels, 2)
      call realloc(pCS%YZstorage, 2)
      call realloc(pCS%frictionSectionFrom, frictionCount)
      call realloc(pCS%frictionSectionTo, frictionCount)
      allocate(positions(frictionCount+1))
      
      xcoordinates = 0d0
      call prop_get_doubles(node_ptr, '', 'xCoordinates', xcoordinates, numlevels, success)
      call prop_get_doubles(node_ptr, '', 'yCoordinates', ycoordinates, numlevels, success)
      if (success) call prop_get_doubles(node_ptr, '', 'zCoordinates', pCS%z, numlevels, success)
      if (.not. success) then
          call SetMessage(LEVEL_ERROR, 'Error while reading number of yz-levels for YZ-Cross-Section Definition ID: '//trim(pCS%id))
      endif
      
      if (xyz_cross_section) then
         pCS%y(1) = 0d0
         do i = 2, numlevels
            call distance(sferic_local, xcoordinates(i-1), ycoordinates(i-1), xcoordinates(i), ycoordinates(i), pCS%y(i), earth_radius)
            pCS%y(i) = pCS%y(i-1) + pCS%y(i) 
         enddo
      else
         pcs%y = ycoordinates
      endif
      
      pCS%storLevels = 0
      
      call prop_get_doubles(node_ptr, '', 'frictionPositions', positions, frictionCount+1, success)
      
      if (success) then
         
         ! Check Consistency of Rougness Positions
         if (positions(1) .ne. pCS%y(1) .or. positions(frictionCount + 1) .ne. pCS%y(numLevels)) then
            
            if (positions(1) == 0.0d0  .and. positions(frictionCount+1) == (pCS%y(numLevels) - pCS%y(1))) then
               ! Probably lined out wrong because of import from SOBEK2
               locShift = positions(frictionCount + 1) - pCS%y(numLevels)
               do i = 1, frictionCount + 1
                  positions(i) = positions(i) - locShift
               enddo
               call SetMessage(LEVEL_WARN, 'Friction sections corrected for YZ-Cross-Section Definition ID: '//trim(pCS%id))
            else
               call SetMessage(LEVEL_ERROR, 'Section data not consistent for YZ-Cross-Section Definition ID: '//trim(pCS%id))
            endif
         
         endif
         
      elseif (.not.success .and. frictionCount==1) then
         positions(1) = pCS%y(1)
         positions(2) = pCS%y(numLevels)
         success = .true.
      endif
      
      
      pCS%frictionSectionFrom = positions(1:frictionCount)
      pCS%frictionSectionTo = positions(2:frictionCount+1)
         
      allocate(pCS%groundlayer)
      pCS%groundlayer%used      = .false.
      pCS%groundlayer%thickness = 0.0d0
      
      deallocate(positions)
      readYZCS = success
   end function readYZCS
   
   !> read YZ cross section from ini file
   logical function readYZCS_v100(pCS, node_ptr) 
      type(t_CSType), pointer, intent(inout) :: pCS             !< cross section item
      type(tree_data), pointer, intent(in)    :: node_ptr       !< treedata pointer to input for cross section
      
      integer :: numlevels
      integer :: frictionCount
      logical :: success
      double precision, allocatable, dimension(:) :: positions

      integer          :: i
      double precision :: locShift

      
      readYZCS_v100 = .false.
      call prop_get_integer(node_ptr, '', 'yzCount', numlevels, success)
      if (.not. success) then
         call prop_get_integer(node_ptr, '', 'xyzCount', numlevels, success)
      endif
      
      if (success) call prop_get_integer(node_ptr, '', 'sectionCount', frictionCount, success)
      if (.not. success .or. numLevels <= 0 .or. frictionCount <= 0) then
            call SetMessage(LEVEL_ERROR, 'Error while reading number of levels/sections for YZ-Cross-Section Definition ID: '//trim(pCS%id))
            return
      endif
      
      pCS%levelsCount           = numLevels
      pCS%frictionSectionsCount = frictionCount
      pCS%storLevelsCount       = 0
      
      call realloc(pCS%y, numlevels)
      call realloc(pCS%z, numlevels)
      call realloc(pCS%storLevels, 2)
      call realloc(pCS%YZstorage, 2)
      call realloc(pCS%frictionSectionID, frictionCount)
      call realloc(pCS%frictionSectionFrom, frictionCount)
      call realloc(pCS%frictionSectionTo, frictionCount)
      allocate(positions(frictionCount+1))
      
      call prop_get_doubles(node_ptr, '', 'yValues', pCS%y, numlevels, success)
      if (success) call prop_get_doubles(node_ptr, '', 'zValues', pCS%z, numlevels, success)
      if (.not. success) then
          call SetMessage(LEVEL_ERROR, 'Error while reading number of yz-levels for YZ-Cross-Section Definition ID: '//trim(pCS%id))
      endif
      
      pCS%storLevels = 0
      
      call prop_get_strings(node_ptr, '', 'roughnessNames', frictionCount, pCS%frictionSectionID, success)
      if (.not. success) then
          call SetMessage(LEVEL_ERROR, 'Error while reading section IDs for YZ-Cross-Section Definition ID: '//trim(pCS%id))
      endif
      
      call prop_get_doubles(node_ptr, '', 'roughnessPositions', positions, frictionCount+1, success)
      
      if (success) then
         
         ! Check Consistency of Rougness Positions
         if (positions(1) .ne. pCS%y(1) .or. positions(frictionCount + 1) .ne. pCS%y(numLevels)) then
            
            if (positions(1) == 0.0d0  .and. positions(frictionCount+1) == (pCS%y(numLevels) - pCS%y(1))) then
               ! Probably lined out wrong because of import from SOBEK2
               locShift = positions(frictionCount + 1) - pCS%y(numLevels)
               do i = 1, frictionCount + 1
                  positions(i) = positions(i) - locShift
               enddo
               call SetMessage(LEVEL_WARN, 'Rougness sections corrected for YZ-Cross-Section Definition ID: '//trim(pCS%id))
            else
               call SetMessage(LEVEL_ERROR, 'Section data not consistent for YZ-Cross-Section Definition ID: '//trim(pCS%id))
            endif
         
         endif
         
      elseif (.not.success .and. frictionCount==1) then
         positions(1) = pCS%y(1)
         positions(2) = pCS%y(numLevels)
         success = .true.
      endif
      
      
      pCS%frictionSectionFrom = positions(1:frictionCount)
      pCS%frictionSectionTo = positions(2:frictionCount+1)
         
      allocate(pCS%groundlayer)
      pCS%groundlayer%used      = .false.
      pCS%groundlayer%thickness = 0.0d0
      
      deallocate(positions)
      readYZCS_v100 = success
   end function readYZCS_v100
   
   !> write cross section definitions to cache file
   subroutine write_cross_section_definition_cache(ibin, defs)
   
      type(t_CSDefinitionSet), intent(inout) :: defs      !< cross section sdefinition
      integer, intent(in) :: ibin                         !< binary cache file
      
      integer :: i, j, k
      type(t_CSType), pointer :: pdef

      write(ibin) defs%count
      do i = 1, defs%count
      
         pdef => defs%CS(i)
         
         write(ibin) pdef%id
         write(ibin) pdef%crossType
         write(ibin) pdef%levelsCount
         write(ibin) pdef%closed
         write(ibin) pdef%diameter
         
         select case(pdef%crossType)
            case (CS_TABULATED) 
               write(ibin) (pdef%height(j), j = 1, pdef%levelscount)
               write(ibin) (pdef%flowWidth(j), j = 1, pdef%levelscount)
               write(ibin) (pdef%totalWidth(j), j = 1, pdef%levelscount)
               write(ibin) ((pdef%af_sub(j, k), j = 1, 3), k = 1, pdef%levelscount)
               write(ibin) ((pdef%width_sub(j, k), j = 1, 3), k = 1, pdef%levelscount)
               write(ibin) ((pdef%perim_sub(j, k), j = 1, 3), k = 1, pdef%levelscount)
               write(ibin) (pdef%flowArea(j), j = 1, pdef%levelscount)
               write(ibin) (pdef%wetPerimeter(j), j = 1, pdef%levelscount)
               write(ibin) (pdef%totalArea(j), j = 1, pdef%levelscount)
               write(ibin) (pdef%area_min(j), j = 1, pdef%levelscount)
               write(ibin) (pdef%width_min(j), j = 1, pdef%levelscount)
               write(ibin) (pdef%plains(j), j = 1, 3)
               write(ibin) (pdef%plainsLocation(j), j = 1, 3)
            
               write(ibin) associated(pdef%summerdike)
               if (associated(pdef%summerdike)) then
                  write(ibin) pdef%summerdike%crestLevel
                  write(ibin) pdef%summerdike%baseLevel
                  write(ibin) pdef%summerdike%flowArea
                  write(ibin) pdef%summerdike%totalArea
               endif
            case (CS_YZ_PROF)
               write(ibin) (pdef%y(j), j = 1, pdef%levelscount)
               write(ibin) (pdef%z(j), j = 1, pdef%levelscount)
            
               write(ibin) pdef%storageType
               write(ibin) pdef%storLevelsCount

               write(ibin) (pdef%storLevels(j), j = 1, 2)
               write(ibin) (pdef%YZstorage(j), j = 1, 2)
         end select
         
         ! Groundlayer
         write(ibin) associated(pdef%groundlayer)
         if (associated(pdef%groundlayer)) then
            write(ibin) pdef%groundlayer%used  
            write(ibin) pdef%groundlayer%thickness    
            write(ibin) pdef%groundlayer%area     
            write(ibin) pdef%groundlayer%perimeter 
            write(ibin) pdef%groundlayer%width    
         endif
         
      enddo
      
   end subroutine write_cross_section_definition_cache
   
   !> read cross section definitions from cache
   subroutine read_cross_section_definition_cache(ibin, defs)
      type(t_CSDefinitionSet), intent(inout) :: defs         !< cross section definitions
      integer, intent(in)                    :: ibin         !< binary file
      
      integer                 :: i, j, k
      logical                 :: isAssociated
      type(t_CSType), pointer :: pdef

      read(ibin) defs%count
      defs%growsBy = defs%count + 2
      call realloc(defs)
      
      do i = 1, defs%count

         pdef => defs%CS(i)
         
         read(ibin) pdef%id
         read(ibin) pdef%crossType
         read(ibin) pdef%levelsCount
         read(ibin) pdef%closed
         read(ibin) pdef%diameter
         
         select case(pdef%crossType)
            case (CS_TABULATED) 
               allocate(pdef%height(pdef%levelscount))
               allocate(pdef%flowWidth(pdef%levelscount))
               allocate(pdef%totalWidth(pdef%levelscount))
               allocate(pdef%af_sub(3, pdef%levelscount))
               allocate(pdef%width_sub(3, pdef%levelscount))
               allocate(pdef%perim_sub(3, pdef%levelscount))
               allocate(pdef%flowArea(pdef%levelscount))
               allocate(pdef%wetPerimeter(pdef%levelscount))
               allocate(pdef%totalArea(pdef%levelscount))
               allocate(pdef%area_min(pdef%levelscount))
               allocate(pdef%width_min(pdef%levelscount))
               read(ibin) (pdef%height(j), j = 1, pdef%levelscount)
               read(ibin) (pdef%flowWidth(j), j = 1, pdef%levelscount)
               read(ibin) (pdef%totalWidth(j), j = 1, pdef%levelscount)
               read(ibin) ((pdef%af_sub(j, k), j = 1, 3), k = 1, pdef%levelscount)
               read(ibin) ((pdef%width_sub(j, k), j = 1, 3), k = 1, pdef%levelscount)
               read(ibin) ((pdef%perim_sub(j, k), j = 1, 3), k = 1, pdef%levelscount)
               read(ibin) (pdef%flowArea(j), j = 1, pdef%levelscount)
               read(ibin) (pdef%wetPerimeter(j), j = 1, pdef%levelscount)
               read(ibin) (pdef%totalArea(j), j = 1, pdef%levelscount)
               read(ibin) (pdef%area_min(j), j = 1, pdef%levelscount)
               read(ibin) (pdef%width_min(j), j = 1, pdef%levelscount)
               read(ibin) (pdef%plains(j), j = 1, 3)
               read(ibin) (pdef%plainsLocation(j), j = 1, 3)
               
               ! Summer Dike
               read(ibin) isAssociated
               if (isAssociated) then
                  anySummerDike = .true.
                  allocate(pdef%summerdike)
                  read(ibin) pdef%summerdike%crestLevel
                  read(ibin) pdef%summerdike%baseLevel
                  read(ibin) pdef%summerdike%flowArea
                  read(ibin) pdef%summerdike%totalArea
               endif
            case (CS_YZ_PROF)
               allocate(pdef%y(pdef%levelscount))
               allocate(pdef%z(pdef%levelscount))
               read(ibin) (pdef%y(j), j = 1, pdef%levelscount)
               read(ibin) (pdef%z(j), j = 1, pdef%levelscount)

               read(ibin) pdef%storageType
               read(ibin) pdef%storLevelsCount

               allocate(pdef%storLevels(2))
               allocate(pdef%YZstorage(2))
               read(ibin) (pdef%storLevels(j), j = 1, 2)
               read(ibin) (pdef%YZstorage(j), j = 1, 2)
               
         end select
         
         ! Groundlayer
         read(ibin) isAssociated
         if (isAssociated) then
            allocate(pdef%groundlayer)
            read(ibin) pdef%groundlayer%used   
            read(ibin) pdef%groundlayer%thickness   
            read(ibin) pdef%groundlayer%area     
            read(ibin) pdef%groundlayer%perimeter 
            read(ibin) pdef%groundlayer%width
            
            if (pdef%groundlayer%used) anyGroundLayer = .true.
         endif
         
      enddo
      
      call fill_hashtable(defs)

   end subroutine read_cross_section_definition_cache
   !
   !> write cross section data from binary file
   subroutine write_cross_section_cache(ibin, crs)
   
      type(t_CrossSectionSet), intent(inout) :: crs      !< cross sections 
      integer, intent(in)                    :: ibin     !< binary file
      
      integer                       :: i
      integer                       :: j
      type(t_CrossSection), pointer :: pcross

      write(ibin) crs%count
      
      do i = 1, crs%count
      
         pcross => crs%cross(i)
         
         write(ibin) pcross%csid
         write(ibin) pcross%crossIndx
         write(ibin) pcross%crossType
         write(ibin) pcross%IsCopy
         write(ibin) pcross%closed
         
         write(ibin) pcross%branchid
         write(ibin) pcross%chainage
         
         write(ibin) pcross%surfaceLevel
         write(ibin) pcross%bedLevel
         write(ibin) pcross%charHeight
         write(ibin) pcross%charWidth
         write(ibin) pcross%shift
         
         write(ibin) pcross%groundFrictionType
         write(ibin) pcross%groundFriction
         
         write(ibin) pcross%iTabDef
         
         write(ibin) pcross%frictionSectionsCount
         if (pcross%frictionSectionsCount > 0) then
            write(ibin) (pcross%frictionSectionID(j), j = 1, pcross%frictionSectionsCount)
            write(ibin) (pcross%frictionSectionFrom(j), j = 1, pcross%frictionSectionsCount)
            write(ibin) (pcross%frictionSectionTo(j), j = 1, pcross%frictionSectionsCount)
            write(ibin) (pcross%frictionTypePos(j), j = 1, pcross%frictionSectionsCount)
            write(ibin) (pcross%frictionValuePos(j), j = 1, pcross%frictionSectionsCount)
            write(ibin) (pcross%frictionTypeNeg(j), j = 1, pcross%frictionSectionsCount)
            write(ibin) (pcross%frictionValueNeg(j), j = 1, pcross%frictionSectionsCount)
         endif
         
         write(ibin) associated(pcross%convtab)
         if (associated(pcross%convtab) ) then
            call write_convtab(ibin, pcross%convtab)
         endif
         
      enddo
      
      write(ibin) associated(crs%crossSectionIndex)
      if (associated(crs%crossSectionIndex)) then
         write(ibin) (crs%crossSectionIndex(i), i = 1, crs%count)
      endif
      
   end subroutine write_cross_section_cache
   !
   !< read cross section data from binary file
   subroutine read_cross_section_cache(ibin, crs, defs)
   
      type(t_CSDefinitionSet), intent(inout) :: defs      !< cross section definitions
      type(t_CrossSectionSet), intent(inout) :: crs       !< cross sections 
      integer, intent(in)                    :: ibin      !< binary file
      
      integer                                :: i
      integer                                :: j
      type(t_CrossSection), pointer          :: pcross
      logical                                :: isAssociated

      read(ibin) crs%count
      crs%growsby = crs%count + 2
      call realloc(crs)
      
      do i = 1, crs%count
      
         pcross => crs%cross(i)
         
         read(ibin) pcross%csid
         read(ibin) pcross%crossIndx
         read(ibin) pcross%crossType
         read(ibin) pcross%IsCopy
         read(ibin) pcross%closed
         
         read(ibin) pcross%branchid
         read(ibin) pcross%chainage
         
         read(ibin) pcross%surfaceLevel
         read(ibin) pcross%bedLevel
         read(ibin) pcross%charHeight
         read(ibin) pcross%charWidth
         read(ibin) pcross%shift
         
         read(ibin) pcross%groundFrictionType
         read(ibin) pcross%groundFriction
         
         read(ibin) pcross%iTabDef

         pcross%tabDef => defs%CS(pcross%iTabDef)
         
         read(ibin) pcross%frictionSectionsCount
         if (pcross%frictionSectionsCount > 0) then
            allocate(pcross%frictionSectionID(pcross%frictionSectionsCount))
            allocate(pcross%frictionSectionFrom(pcross%frictionSectionsCount))
            allocate(pcross%frictionSectionTo(pcross%frictionSectionsCount))
            allocate(pcross%frictionTypePos(pcross%frictionSectionsCount))
            allocate(pcross%frictionValuePos(pcross%frictionSectionsCount))
            allocate(pcross%frictionTypeNeg(pcross%frictionSectionsCount))
            allocate(pcross%frictionValueNeg(pcross%frictionSectionsCount))
            
            read(ibin) (pcross%frictionSectionID(j), j = 1, pcross%frictionSectionsCount)
            read(ibin) (pcross%frictionSectionFrom(j), j = 1, pcross%frictionSectionsCount)
            read(ibin) (pcross%frictionSectionTo(j), j = 1, pcross%frictionSectionsCount)
            read(ibin) (pcross%frictionTypePos(j), j = 1, pcross%frictionSectionsCount)
            read(ibin) (pcross%frictionValuePos(j), j = 1, pcross%frictionSectionsCount)
            read(ibin) (pcross%frictionTypeNeg(j), j = 1, pcross%frictionSectionsCount)
            read(ibin) (pcross%frictionValueNeg(j), j = 1, pcross%frictionSectionsCount)
         endif

         read(ibin) isAssociated
         if (isAssociated) then
            allocate(pcross%convtab)
            call read_convtab(ibin, pcross%convtab)
         endif
      enddo
      
      read(ibin) isAssociated
      if (isAssociated) then
         allocate(crs%crossSectionIndex(crs%count))
         read(ibin) (crs%crossSectionIndex(i), i = 1, crs%count)
      endif
      
   end subroutine read_cross_section_cache

   !> Write conveyance table from cache file
   subroutine write_convtab(ibin, convtab)
      integer, intent(in)      :: ibin           !< unit number of binary file
      type(t_crsu), intent(in) :: convtab        !< conveyance table
      
      integer                  :: i, j, nhmax
      
      write(ibin) convtab%jopen
      write(ibin) convtab%msec
      write(ibin) convtab%nru
      write(ibin) convtab%iolu
      write(ibin) convtab%negcon
      
      write(ibin) convtab%a_pos_extr
      write(ibin) convtab%a_neg_extr
      write(ibin) convtab%b_pos_extr
      write(ibin) convtab%b_neg_extr
      
      write(ibin) convtab%chezy_act

      write(ibin) (convtab%hu (i), i = 1, convtab%nru) 
      write(ibin) (convtab%af (i), i = 1, convtab%nru) 
      write(ibin) (convtab%wf (i), i = 1, convtab%nru) 
      write(ibin) (convtab%pf (i), i = 1, convtab%nru) 
      write(ibin) (convtab%co1(i), i = 1, convtab%nru) 
      write(ibin) (convtab%co2(i), i = 1, convtab%nru) 
      write(ibin) (convtab%cz1(i), i = 1, convtab%nru) 
      write(ibin) (convtab%cz2(i), i = 1, convtab%nru) 
      
      write(ibin) (convtab%nrhh(i), i = 1, 2)
      write(ibin) (convtab%iolh(i), i = 1, 2)
      write(ibin) (convtab%bob (i), i = 1, 2)
      
      nhmax = max(convtab%nrhh(1), convtab%nrhh(2))
      write(ibin) ((convtab%hh(i, j), i = 1, nhmax), j = 1, 2) 
      write(ibin) ((convtab%at(i, j), i = 1, nhmax), j = 1, 2)
      write(ibin) ((convtab%wt(i, j), i = 1, nhmax), j = 1, 2)
      
   end subroutine write_convtab
   
   !> Read conveyance table from cache file
   subroutine read_convtab(ibin, convtab)
   
      integer, intent(in)         :: ibin        !< unit number of binary file
      type(t_crsu), intent(inout) :: convtab     !< conveyance table
      
      integer                     :: i, j, nhmax
      
      read(ibin) convtab%jopen
      read(ibin) convtab%msec
      read(ibin) convtab%nru
      read(ibin) convtab%iolu
      read(ibin) convtab%negcon
      
      read(ibin) convtab%a_pos_extr
      read(ibin) convtab%a_neg_extr
      read(ibin) convtab%b_pos_extr
      read(ibin) convtab%b_neg_extr
      
      read(ibin) convtab%chezy_act


      allocate(convtab%hu (convtab%nru))
      allocate(convtab%af (convtab%nru))
      allocate(convtab%wf (convtab%nru))
      allocate(convtab%pf (convtab%nru))
      allocate(convtab%co1(convtab%nru))
      allocate(convtab%co2(convtab%nru))
      allocate(convtab%cz1(convtab%nru))
      allocate(convtab%cz2(convtab%nru))
      
      read(ibin) (convtab%hu (i), i = 1, convtab%nru) 
      read(ibin) (convtab%af (i), i = 1, convtab%nru) 
      read(ibin) (convtab%wf (i), i = 1, convtab%nru) 
      read(ibin) (convtab%pf (i), i = 1, convtab%nru) 
      read(ibin) (convtab%co1(i), i = 1, convtab%nru) 
      read(ibin) (convtab%co2(i), i = 1, convtab%nru) 
      read(ibin) (convtab%cz1(i), i = 1, convtab%nru) 
      read(ibin) (convtab%cz2(i), i = 1, convtab%nru) 
      
      read(ibin) (convtab%nrhh(i), i = 1, 2)
      read(ibin) (convtab%iolh(i), i = 1, 2)
      read(ibin) (convtab%bob (i), i = 1, 2)
      
      nhmax = max(convtab%nrhh(1), convtab%nrhh(2))
      allocate(convtab%hh(nhmax, 2))
      allocate(convtab%at(nhmax, 2))
      allocate(convtab%wt(nhmax, 2))
      
      read(ibin) ((convtab%hh(i, j), i = 1, nhmax), j = 1, 2) 
      read(ibin) ((convtab%at(i, j), i = 1, nhmax), j = 1, 2)
      read(ibin) ((convtab%wt(i, j), i = 1, nhmax), j = 1, 2)

   end subroutine read_convtab
   
   !> write cross section definitions to file
   subroutine dumpCrossDefs(CSDEfs, fileName)

      type(t_CSDefinitionSet), intent(inout) :: CSDEfs            !< Set containing cross section definitions
      character(len=*),        intent(in   ) :: fileName          !< name of the file
   
      type(t_CSType), pointer       :: pCSDef
      integer                       :: iDef
      integer                       :: dmpUnit = 8744
      integer                       :: j
      
      ! DUMP
      open(newunit=dmpUnit, file=fileName)
      do iDef = 1, CSDEfs%count
      
         pCSDef => CSDEfs%CS(iDef)
      
         write(dmpUnit, *) pCSDef%id
         write(dmpUnit, *) pCSDef%crossType
         write(dmpUnit, *) pCSDef%levelsCount
         write(dmpUnit, *) pCSDef%closed
         write(dmpUnit, *) pCSDef%diameter
         
         select case(pCSDef%crossType)
            case (CS_TABULATED)
               write(dmpUnit, *) '#################### TABULATED #######################'
               write(dmpUnit, *) (pCSDef%height(j), j = 1, pCSDef%levelscount)
               write(dmpUnit, *) (pCSDef%flowWidth(j), j = 1, pCSDef%levelscount)
               write(dmpUnit, *) (pCSDef%totalWidth(j), j = 1, pCSDef%levelscount)
               write(dmpUnit, *) (pCSDef%plains(j), j = 1, 3)
            
               write(dmpUnit, *) associated(pCSDef%summerdike)
               if (associated(pCSDef%summerdike)) then
                  write(dmpUnit, *) '################# SUMMERDIKE ######################'
                  write(dmpUnit, *) pCSDef%summerdike%crestLevel
                  write(dmpUnit, *) pCSDef%summerdike%baseLevel
                  write(dmpUnit, *) pCSDef%summerdike%flowArea
                  write(dmpUnit, *) pCSDef%summerdike%totalArea
               endif
            case (CS_YZ_PROF)
               write(dmpUnit, *) '##################### YZ #########################'
               write(dmpUnit, *) pCSDef%storageType, pCSDef%frictionSectionsCount, pCSDef%storLevelsCount
               write(dmpUnit, *) (pCSDef%y(j), j = 1, pCSDef%levelscount)
               write(dmpUnit, *) (pCSDef%z(j), j = 1, pCSDef%levelscount)
               write(dmpUnit, *) (pCSDef%storLevels(j), j = 1, 2)
               write(dmpUnit, *) (pCSDef%YZstorage(j), j = 1, 2)
               write(dmpUnit, *) (pCSDef%frictionSectionID(j), j = 1, pCSDef%frictionSectionsCount)
               write(dmpUnit, *) (pCSDef%frictionSectionFrom(j), j = 1, pCSDef%frictionSectionsCount)
               write(dmpUnit, *) (pCSDef%frictionSectionTo(j), j = 1, pCSDef%frictionSectionsCount)
         end select
         
         ! Groundlayer
         write(dmpUnit, *) associated(pCSDef%groundlayer)
         if (associated(pCSDef%groundlayer)) then
            write(dmpUnit, *) '################# GROUNDLAYER #####################'
            write(dmpUnit, *) pCSDef%groundlayer%used  
            write(dmpUnit, *) pCSDef%groundlayer%thickness    
            write(dmpUnit, *) pCSDef%groundlayer%area     
            write(dmpUnit, *) pCSDef%groundlayer%perimeter 
            write(dmpUnit, *) pCSDef%groundlayer%width    
         endif

         write(dmpUnit, *) '################ END CROSS #########################'
         
      enddo
      
      close(dmpUnit)
      
   end subroutine dumpCrossDefs
   
   !> Write all cross section information to file (not called in current solution)
   subroutine dumpCross(crs, fileName)
   
      type(t_CrossSectionSet), intent(inout) :: crs              !< Cross section set
      character(len=*),        intent(in   ) :: fileName         !< filename for writing output
      
      integer                       :: i
      type(t_CrossSection), pointer :: pcross

      integer                       :: dmpUnit = 8744

      open(newunit=dmpUnit, file=fileName)
      
      write(dmpUnit, *) crs%count
      
      do i = 1, crs%count
      
         pcross => crs%cross(i)
         
         write(dmpUnit, *) pcross%csid
         write(dmpUnit, *) pcross%crossIndx
         write(dmpUnit, *) pcross%crossType
         write(dmpUnit, *) pcross%IsCopy
         write(dmpUnit, *) pcross%closed
         
         write(dmpUnit, *) '############### chainage ####################'
         write(dmpUnit, *) pcross%branchid
         write(dmpUnit, *) pcross%chainage
                 
         write(dmpUnit, *) '############### LEVELS AND SIZES ####################'
         write(dmpUnit, *) pcross%surfaceLevel
         write(dmpUnit, *) pcross%bedLevel
         write(dmpUnit, *) pcross%charHeight
         write(dmpUnit, *) pcross%charWidth
         write(dmpUnit, *) pcross%shift
         
         write(dmpUnit, *) '################### FRICTION ######################'
         write(dmpUnit, *) pcross%groundFrictionType
         write(dmpUnit, *) pcross%groundFriction
         
         write(dmpUnit, *) pcross%iTabDef
         
         write(dmpUnit, *) associated(pcross%convtab)
         if (associated(pcross%convtab) ) then
            write(dmpUnit, *) '################### CONVTAB ######################'
            call dumpConvtab(dmpUnit, pcross%convtab)
         endif
         
         write(dmpUnit, *) '############## END CROSS #####################'

      enddo
      
      write(dmpUnit, *) associated(crs%crossSectionIndex)
      if (associated(crs%crossSectionIndex)) then
         write(dmpUnit, *) '############## Cross-Section Index #####################'
         write(dmpUnit, *) (crs%crossSectionIndex(i), i = 1, crs%count)
      endif
      
      close(dmpUnit)
      
   end subroutine dumpCross
   
   !> Subroutine for writing conveyance table to file (not called in current solution)
   subroutine dumpConvtab(dmpUnit, convtab)
   
      integer, intent(in)      :: dmpUnit        !< unit number of file
      type(t_crsu), intent(in) :: convtab        !< conveyance table
      
      integer                  :: i, j, nhmax
      
      write(dmpUnit, *) convtab%jopen
      write(dmpUnit, *) convtab%msec
      write(dmpUnit, *) convtab%nru
      write(dmpUnit, *) convtab%iolu
      write(dmpUnit, *) convtab%negcon
      
      write(dmpUnit, *) convtab%a_pos_extr
      write(dmpUnit, *) convtab%a_neg_extr
      write(dmpUnit, *) convtab%b_pos_extr
      write(dmpUnit, *) convtab%b_neg_extr
      
      write(dmpUnit, *) convtab%chezy_act

      write(dmpUnit, *) (convtab%hu (i), i = 1, convtab%nru) 
      write(dmpUnit, *) (convtab%af (i), i = 1, convtab%nru) 
      write(dmpUnit, *) (convtab%wf (i), i = 1, convtab%nru) 
      write(dmpUnit, *) (convtab%pf (i), i = 1, convtab%nru) 
      write(dmpUnit, *) (convtab%co1(i), i = 1, convtab%nru) 
      write(dmpUnit, *) (convtab%co2(i), i = 1, convtab%nru) 
      write(dmpUnit, *) (convtab%cz1(i), i = 1, convtab%nru) 
      write(dmpUnit, *) (convtab%cz2(i), i = 1, convtab%nru) 
      
      write(dmpUnit, *) (convtab%nrhh(i), i = 1, 2)
      write(dmpUnit, *) (convtab%iolh(i), i = 1, 2)
      write(dmpUnit, *) (convtab%bob (i), i = 1, 2)
      
      nhmax = max(convtab%nrhh(1), convtab%nrhh(2))
      write(dmpUnit, *) ((convtab%hh(i, j), i = 1, nhmax), j = 1, 2) 
      write(dmpUnit, *) ((convtab%at(i, j), i = 1, nhmax), j = 1, 2)
      write(dmpUnit, *) ((convtab%wt(i, j), i = 1, nhmax), j = 1, 2)
      
   end subroutine dumpConvtab
   
   !> Retrieve the roughness for given cross section
   subroutine getRougnessForProfile(network, crs)
      use m_read_roughness, only: RoughFileMajorVersion
   
      type(t_network), intent(inout)      :: network      !< Network structure
      type(t_CrossSection), intent(inout) :: crs          !< cross section
      
      integer                        :: i
      integer                        :: iRough
      type(t_Roughness), pointer     :: pRgs
      type(t_spatial_data), pointer  :: pSpData
      double precision               :: frictionValue
      integer                        :: frictionType
      integer                        :: iStatus
      
      if (crs%frictionSectionsCount <= 0) then
         call SetMessage(LEVEL_ERROR, 'No Friction Section Data for Cross-Section ID: '//trim(crs%csid))
         return
      endif
           
      do i = 1, crs%frictionSectionsCount
           
         iRough = hashsearch(network%rgs%hashlist, crs%frictionSectionID(i))
         if (iRough <= 0) then
            call SetMessage(LEVEL_ERROR, 'No Data found for Section '//trim(crs%frictionSectionID(i))//' of Cross-Section ID: '//trim(crs%csid))
            cycle
         endif
         
         pRgs => network%rgs%rough(iRough)
         if (network%rgs%version == RoughFileMajorVersion) then
            call getFrictionParameters(pRgs,  1d0, crs%branchid, crs%chainage, crs%frictionTypePos(i), crs%frictionValuePos(i))
            call getFrictionParameters(pRgs, -1d0, crs%branchid, crs%chainage, crs%frictionTypeNeg(i), crs%frictionValueNeg(i))
            cycle
         endif
         
         if (pRgs%iSection == 0) then
            ! roughness section does not exist
            call setMessage(LEVEL_ERROR, 'Roughness section '// trim(crs%frictionSectionID(i)) //', used in '//trim(crs%csid)//' does not exist')
            cycle
         endif
            
         
         crs%frictionTypePos(i) = pRgs%rgh_type_pos(crs%branchid)
         if (associated(pRgs%rgh_type_neg)) then
            crs%frictionTypeNeg(i) = pRgs%rgh_type_neg(crs%branchid)
         else
            crs%frictionTypeNeg(i) = pRgs%rgh_type_pos(crs%branchid)
         endif
         
         if (pRgs%spd_pos_idx <= 0 .and. pRgs%spd_neg_idx <= 0) then
            call SetMessage(LEVEL_ERROR, 'No Spatial Data specified for Section '//trim(crs%frictionSectionID(i))//' of Cross-Section ID: '//trim(crs%csid))
            cycle
         endif
         
         ! Positive direction
         if (pRgs%spd_pos_idx > 0) then
         
            pSpData => network%spData%quant(pRgs%spd_pos_idx)
            
            iStatus = getValueAtLocation(pSpData, crs%branchid, crs%chainage, frictionValue, frictionType)
            
            if (istatus >= 0) crs%frictionValuePos(i) = frictionValue
            if (istatus > 0)  crs%frictionTypePos(i)  = frictionType

         endif
           
         ! Negative direction
         if (pRgs%spd_neg_idx > 0) then
         
            pSpData => network%spData%quant(pRgs%spd_neg_idx)
            
            iStatus = getValueAtLocation(pSpData, crs%branchid, crs%chainage, frictionValue, frictionType)
            
            if (istatus >= 0) crs%frictionValueNeg(i) = frictionValue
            if (istatus > 0)  crs%frictionTypeNeg(i)  = frictionType

         endif
         
         if (pRgs%spd_pos_idx > 0 .and. pRgs%spd_neg_idx <= 0) then
            crs%frictionValueNeg(i) = crs%frictionValuePos(i)
            crs%frictionTypeNeg(i)  = crs%frictionTypePos(i)
         endif
         
         if (pRgs%spd_pos_idx <= 0 .and. pRgs%spd_neg_idx > 0) then
            crs%frictionValuePos(i) = crs%frictionValueNeg(i)
            crs%frictionTypePos(i)  = crs%frictionTypeNeg(i)
         endif
           
      enddo
         
         
   end subroutine getRougnessForProfile
   
   !==========================================================================================================
   !
   !              Compatibility 
   !
   !==========================================================================================================
   
   !> Parse cross section definition file with fileVersion 1.00
   subroutine parseCrossSectionDefinitionFile_v100(md_ptr, network)
   
      type(t_network), target,  intent(inout)   :: network        !< network structure
      type(tree_data), pointer, intent(in   )   :: md_ptr         !< treedata pointer to cross section definitions
   
      !integer :: istat
      integer :: numstr
      integer :: i
      integer :: crosstype
      logical :: success
      character(len=IdLen) :: id
      character(len=IdLen) :: typestr
      double precision :: diameter
      integer :: numLevels
      double precision, allocatable :: level(:)
      double precision, allocatable :: width(:)
      double precision              :: plains(3)
      double precision              :: crestLevel
      double precision              :: baseLevel
      double precision              :: flowArea
      double precision              :: totalArea
      logical                       :: closed
      
      double precision              :: slope                  ! Slope of trapezium (m)
      double precision              :: maximumFlowWidth       ! Maximum flow width of trapezium (m)
      double precision              :: bottomWidth            ! Bottom width of trapezium (m)
      
      integer                       :: hasGroundLayer
      integer                       :: j
      logical                       :: groundlayerUsed
      double precision              :: groundlayer
      double precision              :: height
      integer                       :: inext
      type(t_CSType), pointer       :: pCS
    
      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      do i = 1, numstr
         
         if (tree_get_name(md_ptr%child_nodes(i)%node_ptr) .ne. 'definition') then
            cycle
         endif
         
         call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'id', id, success)
         if (success) call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'type', typestr, success)
         if (.not. success) then
            call SetMessage(LEVEL_ERROR, 'Incorrect Cross-Section input for Cross-Section Definition with type '//trim(typestr)//' and id: '//trim(id))
            cycle
         endif
         crossType = getCrosstype(typestr)

         inext = network%CSDefinitions%count + 1
         if (network%CSDefinitions%count + 1 > network%CSDefinitions%size) then
            call realloc(network%CSDefinitions)
         endif
         
         pCS => network%CSDefinitions%CS(inext)
         pCS%id = id
         pCS%crossType = crosstype
         
         select case (crossType)
            case(CS_TABULATED)
            
               success = readTabulatedCS_v100(pCS, md_ptr%child_nodes(i)%node_ptr) 
            
            case(CS_RECTANGLE)
               
               numlevels = 1
               allocate(level(numlevels + 1))
               allocate(width(numlevels + 1))
               level(numlevels) = 0.0d0
               call prop_get_doubles(md_ptr%child_nodes(i)%node_ptr, '', 'width', width, numlevels, success)
               if (.not. success) then
                   call SetMessage(LEVEL_ERROR, 'Incorrect Cross-Section input for Cross-Section Definition with type '//trim(typestr)//' and id: '//trim(id))
               endif
               
               call prop_get_logical(md_ptr%child_nodes(i)%node_ptr, '', 'closed', closed, success)
               if (.not. success) closed = .false. ! Default
               
               if (closed) then
                  call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'height', height, success)
                  if (.not. success) then
                     call SetMessage(LEVEL_ERROR, 'HEIGHT is obligatory for closed rectangular cross sections. Refer to cross-section definition with id '//trim(id))
                     cycle
                  endif
                  numlevels = numlevels + 1
                  level(numLevels) = height
                  width(numLevels) = width(1)
               endif

               plains     = 0.0d0
               crestLevel = 0.0d0
               baseLevel  = 0.0d0
               flowArea   = 0.0d0
               totalArea  = 0.0d0

               pCs%frictionSectionsCount = 1
                   
               ! Ground Layer
               call prop_get_integer(md_ptr%child_nodes(i)%node_ptr, '', 'groundlayerUsed', hasGroundLayer, success)
               if (success) then
                  groundlayerUsed = (hasgroundlayer == 1)
               else 
                  groundlayerUsed =  .false.
               endif
               success = .true.

               if (groundlayerUsed) then
                  call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'groundlayer', groundlayer, success)
               else
                  groundlayer = 0.0d0
               endif
               inext = AddCrossSectionDefinition(network%CSDefinitions, id, numLevels, level, width,               &
                                                 width, plains, crestLevel, baseLevel, flowArea, totalArea,        &
                                                 closed, groundlayerUsed, groundlayer)
               deallocate(level, width)            
            
            case(CS_TRAPEZIUM)

               call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'bottomWidth', bottomWidth, success)
               if (success) call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'maximumFlowWidth', maximumFlowWidth, success)
               if (success) call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'slope', slope, success)
               if (.not. success) then
                   call SetMessage(LEVEL_ERROR, 'Incorrect Cross-Section input for Cross-Section Definition with type '//trim(typestr)//' and id: '//trim(id))
               endif
               
               numlevels = 2
               allocate(level(numlevels))
               allocate(width(numlevels))
               
               level(1) = 0.0d0
               width(1) = bottomWidth
               level(2) = (maximumFlowWidth - bottomWidth) * 0.5d0 / slope
               width(2) = maximumFlowWidth
                
               pCs%frictionSectionsCount = 1

               plains     = 0.0d0
               crestLevel = 0.0d0
               baseLevel  = 0.0d0
               flowArea   = 0.0d0
               totalArea  = 0.0d0
            
               ! Ground Layer
               call prop_get_integer(md_ptr%child_nodes(i)%node_ptr, '', 'groundlayerUsed', hasGroundLayer, success)
               if (success) then
                  groundlayerUsed = (hasgroundlayer == 1)
               else 
                  groundlayerUsed =  .false.
                  success = .true.
               endif
               if (groundlayerUsed) then
                  call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'groundlayer', groundlayer, success)
               else
                  groundlayer = 0.0d0
               endif
               
               inext = AddCrossSectionDefinition(network%CSDefinitions, id, numLevels, level, width,  &
                                               & width, plains, crestLevel, baseLevel, flowArea, totalArea,              &
                                               & .false., groundlayerUsed, groundlayer)
               deallocate(level, width)            
            
            case(CS_CIRCLE, CS_EGG)
               success = .true.
               call prop_get_integer(md_ptr%child_nodes(i)%node_ptr, '', 'numLevels', numlevels, success)
               
               if (success) then
                  ! also tabulated definition is available. Use this definition
                  success = readTabulatedCS_v100(pCS, md_ptr%child_nodes(i)%node_ptr) 
                  pCS%crossType = CS_TABULATED
               else
                  success = .true.
                  ! use analytical description of circle and egg profile
                  call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'diameter', diameter, success)
                  if (.not. success) then
                     call SetMessage(LEVEL_ERROR, 'DIAMETER not found for Cross-Section Definition with type '//trim(typestr)//' and id: '//trim(id))
                  endif

                  pCs%frictionSectionsCount = 1
                  pCS%plains(1) = diameter
                  pCS%plains(2) = 0.0d0
                  pCS%plains(3) = 0.0d0

                  ! Ground Layer
                  call prop_get_integer(md_ptr%child_nodes(i)%node_ptr, '', 'groundlayerUsed', hasGroundLayer, success)
                  if (success) then
                     groundlayerUsed = (hasgroundlayer == 1)
                  else 
                     groundlayerUsed =  .false.
                  endif
                  if (groundlayerUsed) then
                     call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'groundlayer', groundlayer, success)
                  else
                     groundlayer = 0.0d0
                  endif
                  success = .true.
                  inext = AddCrossSectionDefinition(network%CSDefinitions, id, diameter, crossType, groundlayerUsed, groundlayer)
      
               endif
               
            case(CS_YZ_PROF)
               success = readYZCS_v100(pCS, md_ptr%child_nodes(i)%node_ptr) 
               
            case default
               call SetMessage(LEVEL_ERROR, 'Incorrect Cross-Section type for Cross-Section Definition with given type '//trim(typestr)//' and id: '//trim(id))
               success = .false.
               
            end select
            
            if (success .and. crossType /= CS_YZ_PROF) then
               allocate(pCs%frictionSectionID  (pCs%frictionSectionsCount))      !< Friction Section Identification
               allocate(pCs%frictionSectionFrom(pCs%frictionSectionsCount))    !<
               allocate(pCs%frictionSectionTo  (pCs%frictionSectionsCount))      !<
               allocate(pCs%frictionSectionIndex(pCs%frictionSectionsCount))      !<
               
               call prop_get_strings(md_ptr%child_nodes(i)%node_ptr, '', 'roughnessNames', pCs%frictionSectionsCount, pCS%frictionSectionID, success)

               if (.not. success) then
                  ! use defaults
                  pCs%frictionSectionID(1) = 'Main'
                  if (pCs%frictionSectionsCount >=2) then
                     pCs%frictionSectionID(2) = 'FloodPlain1'
                  endif
                  if (pCs%frictionSectionsCount ==3) then
                     pCs%frictionSectionID(3) = 'FloodPlain2'
                  endif
               endif
               success = .true.
            endif
            
         if (success) then
            network%CSDefinitions%count = inext
         endif
         
         call realloc( pCs%frictionSectionIndex, pCs%frictionSectionsCount)
         do j = 1, pCs%frictionSectionsCount
            pCs%frictionSectionIndex(j) = hashsearch(network%rgs%hashlist, pCS%frictionSectionID(j))
         enddo
         
      enddo

      !call dumpCrossDefs(network%CSDefinitions, 'dumpCrossDefFileRead')
   end subroutine parseCrossSectionDefinitionFile_v100

end module m_readCrossSections
