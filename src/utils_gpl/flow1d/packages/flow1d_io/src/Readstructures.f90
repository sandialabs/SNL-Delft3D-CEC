module m_readstructures
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
!  $Id: Readstructures.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_io/src/Readstructures.f90 $
!-------------------------------------------------------------------------------

   use MessageHandling
   use string_module
   use m_network
   use m_CrossSections
   use m_1d_structures
   use m_Weir
   use m_Universal_Weir
   use m_Culvert
   use m_Bridge
   use m_pump
   use m_Orifice
   use m_General_Structure
   use m_ExtraResistance
   use m_Dambreak

   use properties
   use m_hash_list
   use m_hash_search
   use m_tables
   use m_read_table

   implicit none

   private

   public readStructures
   public read_structure_cache
   public write_structure_cache
   public readPump
   public readDambreak

   !> The file version number of the structure file format: d.dd, [config_major].[config_minor], e.g., 1.03
   !!
   !! Note: read config_minor as a 2 digit-number, i.e., 1.1 > 1.02 (since .1 === .10 > .02).
   !! Convention for format version changes:
   !! * if a new format is backwards compatible with old files, only
   !!   the minor version number is incremented.
   !! * if a new format is not backwards compatible (i.e., old files
   !!   need to be converted/updated by user), then the major version number
   !!   is incremented.
   
   ! Structure file current version: 1.00
   integer, parameter :: StructureFileMajorVersion = 2
   integer, parameter :: StructureFileMinorVersion = 0
   
   ! History structure file versions:

   ! 0.00 (pre-2019  ): Unversioned flow1d/SOBEK3 version of *.ini type structure file.
   ! 1.00 (2019-07-05): Consistent renaming,
   !                    * pumps: nrStages -> numStages, reductionFactorLevels -> numReductionLevels
   !                    * culverts: lossCoeffCount -> numLossCoeff
   !                    * universal weir: levelsCount -> numLevels
   ! 2.00 (2019-07-22): Consistent renaming,
   

   contains

   !> Read structure.ini file(s).
   subroutine readStructures(network, structureFiles)
      use string_module, only: str_token

      type(t_network),  intent(inout)    :: network        !< The network data structure into whose Structure Set the file(s) will be read.
      character(len=*), intent(in   )    :: structurefiles !< File name(s) to be read. Separate multiple files by semicolon: "file with spaces 1.ini;file2.ini;file 3.ini".
   
      integer  :: isemi, ispace
      character(len=CharLn) :: file
      character(len=CharLn) :: inputFiles
      
      inputFiles = structurefiles
      do while (len_trim(inputfiles) > 0) 
         call str_token(inputfiles, file, DELIMS=';')
         call readStructureFile(network, adjustl(trim(file)))
      enddo

      if (.not. allocated(network%sts%restartData) .and. (network%sts%count > 0)) then
         allocate(network%sts%restartData(network%sts%count, CFiHighestParameter))
         network%sts%restartData = missingValue
      endif
      ! Fill indirection tables and set indices for compoundss
      if (network%sts%currentFileVersion >= 2) then
         call finishReading(network%sts, network%cmps, network%crs)
      else
         ! fill the hashtable for searching on Id's
         call fill_hashtable(network%sts)
      endif
      
   end subroutine readStructures

   !> read a single ini file and add the structures to the structure sets
   subroutine readStructureFile(network, structureFile)
      use m_GlobalParameters
      use m_1d_Structures
      use m_compound
      
      implicit none
      
      type(t_network), intent(inout) :: network              !< Network pointer
      character*(*), intent(in)      :: structureFile        !< Name of the structure file

      logical                                                :: success, success1
      type(tree_data), pointer                               :: md_ptr 
      integer                                                :: istat
      integer                                                :: numstr
      integer                                                :: i, j
      character(len=:), allocatable                          :: str_buf

      character(len=IdLen)                                   :: typestr
      character(len=IdLen)                                   :: st_id
      character(len=IdLen)                                   :: branchID
      
      logical                                                :: isPillarBridge

      integer                                                :: iCompound
      character(len=IdLen)                                   :: compoundName
      character(len=IdLen), allocatable, dimension(:)        :: structureNames
      
      integer                                                :: iStrucType
      integer                                                :: istru
      type(t_structure), pointer                             :: pstru
      type(t_compound), pointer                              :: pcompound
      integer                                                :: nweir
      integer                                                :: nculvert
      integer                                                :: norifice
      integer                                                :: ngenstru
      integer                                                :: nbridge
      integer                                                :: ngate
      integer                                                :: numStructures

      integer                                                :: pos
      integer                                                :: ibin = 0
      character(len=Charln)                                  :: binfile
      logical                                                :: file_exist
      integer                                                :: major, minor, ierr
      
      success = .true.
      pos = index(structureFile, '.', back = .true.)
      binfile = structureFile(1:pos)//'cache'
      inquire(file=binfile, exist=file_exist)
      if (doReadCache .and. file_exist) then
         open(newunit=ibin, file=binfile, status='old', form='unformatted', access='stream', action='read', iostat=istat)
         if (istat /= 0) then
            call setmessage(LEVEL_FATAL, 'Error opening Structure Cache file')
            ibin = 0
         endif
         call read_structure_cache(ibin, network)
         close(ibin)
         ibin = 0
         return
      endif

      call tree_create(trim(structurefile), md_ptr, maxlenpar)
      call prop_file('ini',trim(structurefile),md_ptr,istat)
      
      ! check FileVersion
      ierr = 0
      major = 1
      minor = 0
      call prop_get_version_number(md_ptr, major = major, minor = minor, success = success1)
      ! by exception, we backwards-support majorVersion=1 (for orifice and weir)
      if ((major /= StructureFileMajorVersion .and. major /= 1) .or. minor > StructureFileMinorversion) then
         write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of structure file detected in '''//trim(structurefile)//''': v', major, minor, '. Current format: v',StructureFileMajorVersion,StructureFileMinorVersion,'. Ignoring this file.'
         call err_flush()
         ierr = 1
      end if

      if (ierr /= 0) then
         goto 999
      end if

      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      if (network%sts%count == 0) then
         network%sts%currentFileVersion = major
      else
         network%sts%currentFileVersion = min(major, network%sts%currentFileVersion)
      endif

      do i = 1, numstr
         success = .true.
         if (strcmpi(tree_get_name(md_ptr%child_nodes(i)%node_ptr), 'Structure')) then
            
            if (network%sts%count+1 > network%sts%Size) then
               call realloc(network%sts)
            endif
            pstru => network%sts%struct(network%sts%count+1)
            ! Read Common Structure Data
            
            ! TODO: UNST-2799: temporary check on polylinefile to prevent stopping on old (1.00) structure files. They will be read by dflowfm kernel itself.
            call prop_get_alloc_string(md_ptr%child_nodes(i)%node_ptr, '', 'polylinefile', str_buf, success1)
            if (success1) then
               write (msgbuf, '(a,i0,a)') 'Detected structure #', i, ' from '''//trim(structureFile)//''' as an old v1.00 structure. Will be read later by main program.'
               call dbg_flush()
               cycle
            end if

            call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'id', st_id, success1)
            if (.not. success1) then
               write (msgbuf, '(a,i0,a)') 'Error Reading Structure #', i, ', id is missing.'
               call err_flush()
               success = .false.
            end if

            pstru%id = st_id

            pstru%name = pstru%id
            call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'name', pstru%name)
            
            call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'type', typestr, success1)
            success = success .and. check_input_result(success1, st_id, 'type')
      
            iStrucType = GetStrucType_from_string(typestr)
            pstru%type = iStrucType
            if (iStrucType == ST_COMPOUND) then
               call readCompound(network%cmps, md_ptr%child_nodes(i)%node_ptr, success)
               cycle
            endif
            
            call prop_get_string(md_ptr%child_nodes(i)%node_ptr, '', 'branchId', branchID, success1)
            if (success1) call prop_get_double(md_ptr%child_nodes(i)%node_ptr, '', 'chainage', pstru%chainage, success1)

            pstru%numCoordinates = 0
            if (success1) then
               pstru%ibran = hashsearch(network%brs%hashlist, branchID)
               if (pstru%ibran <= 0) then
                  write (msgbuf, '(a)') 'Error Reading Structure '''//trim(st_id)//''' from '''//trim(structureFile)//''', branchId '''//trim(branchID)//''' not found.'
                  call err_flush()
                  success = .false.
               endif
            else 
               call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'numCoordinates', pstru%numCoordinates, success1)
               success = success .and. check_input_result(success1, st_id, 'numCoordinates')
               if (success) then
                  allocate(pstru%xCoordinates(pstru%numCoordinates), pstru%yCoordinates(pstru%numCoordinates))
                  
                  call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'xCoordinates', pstru%xCoordinates, &
                                pstru%numCoordinates, success1)
                  success = success .and. check_input_result(success1, st_id, 'xCoordinates')
                  
                  call prop_get(md_ptr%child_nodes(i)%node_ptr, '', 'yCoordinates', pstru%yCoordinates, &
                                pstru%numCoordinates, success1)
                  success = success .and. check_input_result(success1, st_id, 'yCoordinates')
               endif
            endif
            
            pstru%compound = 0
            if (.not. success) then
               ! Error(s) found while scanning the structuretype independent stuff. Do not read the type dependent items
               cycle
            endif
            
            select case (iStrucType)
            case (ST_WEIR)
               if (major ==1) then 
                  ! support for version 1.0 structure files weirs as weir 
                  call readWeir(pstru%weir, md_ptr%child_nodes(i)%node_ptr, success)
               else
                  ! support for version >= 2.0 structure files weirs as general structure
                  call readWeirAsGenstru(pstru%generalst, md_ptr%child_nodes(i)%node_ptr, st_id, network%forcinglist, success)
               endif
                  
            case (ST_UNI_WEIR)
               call readUniversalWeir(pstru%uniweir, md_ptr%child_nodes(i)%node_ptr, st_id, success)
            case (ST_CULVERT)
               call readCulvert(pstru%culvert, network, md_ptr%child_nodes(i)%node_ptr, st_id, network%forcinglist, success)
            case (ST_BRIDGE)
               call readBridge(pstru%bridge, network, md_ptr%child_nodes(i)%node_ptr, st_id, success)
            case (ST_PUMP)
               call readPump(pstru%pump, md_ptr%child_nodes(i)%node_ptr, st_id, network%forcinglist, success)
            case (ST_ORIFICE, ST_GATE)
               if (major ==1) then 
                  ! support for version 1.0 structure files weirs as weir 
                  call readOrifice(pstru%orifice, md_ptr%child_nodes(i)%node_ptr, success)
               else
                  ! support for version >= 2.0 structure files weirs as general structure
                  call readOrificeAsGenstru(pstru%generalst, md_ptr%child_nodes(i)%node_ptr, st_id, network%forcinglist, success)
               endif
            case (ST_GENERAL_ST)
               call readGeneralStructure(pstru%generalst, md_ptr%child_nodes(i)%node_ptr, st_id, network%forcinglist, success)
            case (ST_COMPOUND)
               ! Compound structures have been cycled above already.
               continue
            case default
               call setmessage(LEVEL_ERROR,  'Structure type: '//trim(typestr)//' not supported, see '//trim(pstru%id))
               success = .false.
            end select
            
            if (success) then
               network%sts%Count = network%sts%Count + 1
               call incStructureCount(network%sts, iStrucType)
            endif
         
         endif

         
      end do

999   continue
      call tree_destroy(md_ptr)

   end subroutine readStructureFile
   
   !> Read the data for a compound structure and store the ids of the structure  elements
   subroutine readCompound(cmps, md_ptr, success)
   
      use messageHandling
      
      type(t_compoundSet),                intent(inout) :: cmps         !< compound data set
      type(tree_data), pointer,           intent(in   ) :: md_ptr       !< ini tree pointer with user input.
      logical,                            intent(  out) :: success 

      type(t_compound), pointer           :: pcompound
      integer                             :: i
      character(len=IdLen)                :: st_id
      logical                             :: success1
      
      if (cmps%count+1 > cmps%Size) then
         call realloc(cmps)
      endif

      success = .true.
      pcompound => cmps%compound(cmps%count+1)
      call prop_get(md_ptr, '', 'id', st_id, success1)
      if (.not. success1) then
         write (msgbuf, '(a,i0,a)') 'Error Reading Structure #', i, ', id is missing.'
         call err_flush()
         success = .false.
      end if

      pcompound%id = st_id
      pcompound%name = pcompound%id
      call prop_get(md_ptr, '', 'name', pcompound%name)
            
      call prop_get_integer(md_ptr, '', 'numStructures', pcompound%numstructs, success1)
      success = success .and. check_input_result(success1, st_id, 'numStructures')
      
      if (.not. success) then
         return
      endif
      
      allocate(pcompound%structureIds(pcompound%numstructs))

      call prop_get_strings(md_ptr, '', 'structureIds', pcompound%numstructs, pcompound%structureIds, success1)
      success = success .and. check_input_result(success1, st_id, 'structureIds')
      if (.not. success) then
         ! Stop processing this structure
         return
      endif
            
      allocate(pcompound%structure_indices(pcompound%numstructs))
      if (success) then
         cmps%Count = cmps%Count + 1
      endif
      
   end subroutine readCompound
   
   !> At the end of the reading of all structure files, fill the indices arrays for the different
   !! structure types. And find the integer indices for all compound structures
   subroutine finishReading(sts, cmps, crs)
      type(t_structureSet),               intent(inout) :: sts    !< structure data set
      type(t_compoundSet),                intent(inout) :: cmps   !< compound data set
      type(t_CrossSectionSet),            intent(inout) :: crs    !< cross section set
   
      integer :: i
      integer :: icross
      integer :: istru
      integer :: nweir
      integer :: nculvert
      integer :: norifice
      integer :: nbridge
      integer :: ngate
      integer :: ngenstru
      integer :: nuniweir
      integer,          dimension(:), pointer :: indices
      character(len=IdLen), dimension(:), pointer :: ids
      
      ! Set counters for number of weirs, culverts, etc
      sts%numweirs    = 0
      sts%numculverts = 0
      sts%numPumps    = 0
      sts%numOrifices = 0
      sts%numBridges  = 0
      sts%numGates    = 0
      sts%numGeneralStructures = 0
      sts%numUniWeirs = 0
      sts%numweirs    = sts%countByType(ST_WEIR)
      sts%numculverts = sts%countByType(ST_CULVERT)
      sts%numPumps    = sts%countByType(ST_PUMP)
      sts%numOrifices = sts%countByType(ST_ORIFICE)
      sts%numBridges  = sts%countByType(ST_BRIDGE)
      sts%numGates    = sts%countByType(ST_GATE)
      sts%numGeneralStructures = sts%countByType(ST_GENERAL_ST)
      sts%numUniWeirs = sts%countByType(ST_UNI_WEIR)
      allocate(sts%weirIndices(sts%numweirs))
      allocate(sts%culvertIndices(sts%numCulverts))
      allocate(sts%pumpIndices(sts%numPumps))
      allocate(sts%orificeIndices(sts%numOrifices))
      allocate(sts%gateIndices(sts%numGates))
      allocate(sts%bridgeIndices(sts%numBridges))
      allocate(sts%generalStructureIndices(sts%numGeneralStructures))
      allocate(sts%uniWeirIndices(sts%numUniWeirs))

      !set structure indices for different structure types
      nweir = 0
      nculvert = 0
      norifice = 0
      ngenstru = 0
      nbridge = 0
      ngate = 0
      nuniweir = 0
      do istru = 1, sts%Count
         select case (sts%struct(istru)%type)
         case (ST_WEIR)
            nweir = nweir+1
            sts%weirIndices(nweir) = istru
            ! From now on this is a general structure
            sts%struct(istru)%type = ST_GENERAL_ST
         case (ST_CULVERT)
            nculvert = nculvert + 1
            sts%culvertIndices(nculvert) = istru

            ! Extra step for culvert: re-pointer to the cross section (as the network%crs may have been reallocted during reading).
            icross = sts%struct(istru)%culvert%crosssectionnr
            if (icross > 0) then
               sts%struct(istru)%culvert%pcross => crs%cross(icross)
            end if
         case (ST_ORIFICE)
            norifice = norifice + 1
            sts%orificeIndices(norifice) = istru
            ! From now on this is a general structure
            sts%struct(istru)%type = ST_GENERAL_ST
         case (ST_GATE)
            ngate = ngate + 1
            sts%gateIndices(ngate) = istru
            ! From now on this is a general structure
            sts%struct(istru)%type = ST_GENERAL_ST
         case (ST_BRIDGE)
            nbridge = nbridge + 1
            sts%bridgeIndices(nbridge) = istru

            ! Extra step for bridge: re-pointer to the cross section (as the network%crs may have been reallocted during reading).
            icross = sts%struct(istru)%bridge%crosssectionnr
            if (icross > 0) then
               sts%struct(istru)%bridge%pcross => crs%cross(icross)
            end if
         case (ST_GENERAL_ST)
            ngenstru = ngenstru + 1
            sts%generalStructureIndices(ngenstru) = istru
         case (ST_UNI_WEIR)
            nuniweir = nuniweir + 1
            sts%uniWeirIndices(nuniweir) = istru
         end select
      enddo
      
      call fill_hashtable(sts)
            
      ! Initialise compounds and searc for all structure elements
      do istru = 1, cmps%Count
         indices => cmps%compound(istru)%structure_indices
         ids =>  cmps%compound(istru)%structureIds
         do i = 1,  cmps%compound(istru)%numstructs
            indices(i) = hashsearch(sts%hashlist_structure, ids(i))
            if (indices(i) <=0) then
               msgbuf = 'Error in compound '''//trim(cmps%compound(istru)%id)//''' structure element with id '''//trim(ids(i))//&
                        ''' was not found in the structure list'
               call err_flush()
            else
               sts%struct(indices(i))%compound = 1 ! mark the structure that belongs to a compound structure
            endif
         enddo
         
      enddo
   end subroutine finishreading
   
   
   subroutine read_structure_cache(ibin, network)
   
      type(t_network), intent(inout)         :: network
      integer, intent(in)                    :: ibin
      
      type(t_structure), pointer             :: pstr
      integer                                :: i
      integer                                :: j

      read(ibin) network%sts%count
      network%sts%growsby = network%sts%count + 2
      call realloc(network%sts)

      read(ibin) (network%sts%countByType(i), i = 1, ST_MAX_TYPE)
      
      read(ibin) network%sts%compoundCount
      read(ibin) network%sts%hasExtraResistance

      do i = 1, network%sts%Count
      
         pstr => network%sts%struct(i)
         
         read(ibin) pstr%id
         read(ibin) pstr%name
         read(ibin) pstr%type
         read(ibin) pstr%ibran
         read(ibin) pstr%linknumbers(1)
         read(ibin) pstr%xCoordinates(1)
         read(ibin) pstr%yCoordinates(1)
         read(ibin) pstr%chainage
         read(ibin) pstr%compound
         
         select case(pstr%type)
            case(ST_WEIR)
               allocate(pstr%weir)
               read(ibin) pstr%weir%allowedflowdir
               read(ibin) pstr%weir%crestlevel
               read(ibin) pstr%weir%crestwidth
               read(ibin) pstr%weir%dischargecoeff
               read(ibin) pstr%weir%latdiscoeff
               read(ibin) pstr%weir%crestlevel
               read(ibin) pstr%weir%cmu
               
            case(ST_ORIFICE)
               allocate(pstr%orifice)
               read(ibin) pstr%orifice%allowedflowdir
               read(ibin) pstr%orifice%crestlevel
               read(ibin) pstr%orifice%crestwidth
               read(ibin) pstr%orifice%contrcoeff
               read(ibin) pstr%orifice%latcontrcoeff
               read(ibin) pstr%orifice%openlevel
               read(ibin) pstr%orifice%uselimitflowpos
               read(ibin) pstr%orifice%limitflowpos
               read(ibin) pstr%orifice%uselimitflowneg
               read(ibin) pstr%orifice%limitflowneg
               
            case(ST_PUMP)
               allocate(pstr%pump)
               read(ibin) pstr%pump%direction
               read(ibin) pstr%pump%nrstages
            
               allocate(pstr%pump%capacity(pstr%pump%nrstages))
               allocate(pstr%pump%ss_onlevel(pstr%pump%nrstages))
               allocate(pstr%pump%ss_offlevel(pstr%pump%nrstages))
               allocate(pstr%pump%ds_onlevel(pstr%pump%nrstages))
               allocate(pstr%pump%ds_offlevel(pstr%pump%nrstages))
               allocate(pstr%pump%ss_trigger(pstr%pump%nrstages))
               allocate(pstr%pump%ds_trigger(pstr%pump%nrstages))
               
               read(ibin) (pstr%pump%capacity(j), j = 1, pstr%pump%nrstages)
               read(ibin) (pstr%pump%ss_onlevel(j), j = 1, pstr%pump%nrstages)
               read(ibin) (pstr%pump%ss_offlevel(j), j = 1, pstr%pump%nrstages)
               read(ibin) (pstr%pump%ds_onlevel(j), j = 1, pstr%pump%nrstages)
               read(ibin) (pstr%pump%ds_offlevel(j), j = 1, pstr%pump%nrstages)
               read(ibin) (pstr%pump%ss_trigger(j), j = 1, pstr%pump%nrstages)
               read(ibin) (pstr%pump%ds_trigger(j), j = 1, pstr%pump%nrstages)
            
               call read_table_cache(ibin, pstr%pump%reducfact)
               
               ! Initialize Parameters for Pump
               pstr%pump%actual_stage      = 0
               pstr%pump%discharge         = 0.0d0
               pstr%pump%is_active         = .true.
               pstr%pump%pump_head         = 0.0d0
               pstr%pump%reduction_factor  = 1.0d0
               pstr%pump%ss_level          = 0.0d0
               pstr%pump%ds_level          = 0.0d0
               pstr%pump%current_capacity  = 0.0d0
            
            case(ST_CULVERT)
               allocate(pstr%culvert)
               read(ibin) pstr%culvert%culvertType
               read(ibin) pstr%culvert%leftlevel
               read(ibin) pstr%culvert%rightlevel
               read(ibin) pstr%culvert%crosssectionnr
               pstr%culvert%pcross => network%crs%cross(pstr%culvert%crosssectionnr)
               read(ibin) pstr%culvert%allowedflowdir
               read(ibin) pstr%culvert%length
               read(ibin) pstr%culvert%inletlosscoeff
               read(ibin) pstr%culvert%outletlosscoeff
               read(ibin) pstr%culvert%has_valve
               if (pstr%culvert%has_valve) then
                   read(ibin) pstr%culvert%valveOpening
                   call read_table_cache(ibin, pstr%culvert%losscoeff)
               endif
            
            case(ST_UNI_WEIR)
               allocate(pstr%uniweir)
               read(ibin) pstr%uniweir%allowedflowdir
               read(ibin) pstr%uniweir%crestlevel
               read(ibin) pstr%uniweir%dischargecoeff
            
               read(ibin) pstr%uniweir%yzcount
               
               allocate(pstr%uniweir%y(pstr%uniweir%yzcount))
               allocate(pstr%uniweir%z(pstr%uniweir%yzcount))

               read(ibin) (pstr%uniweir%y(j), j = 1, pstr%uniweir%yzcount)
               read(ibin) (pstr%uniweir%z(j), j = 1, pstr%uniweir%yzcount)
               
            case(ST_BRIDGE)
               allocate(pstr%bridge)
               read(ibin) pstr%bridge%allowedflowdir
               read(ibin) pstr%bridge%bedLevel
               read(ibin) pstr%bridge%pillarwidth
               read(ibin) pstr%bridge%formfactor
               read(ibin) pstr%bridge%useOwnCrossSection
               read(ibin) pstr%bridge%crosssectionnr
               if (pstr%bridge%useOwnCrossSection) then
                  pstr%bridge%pcross => network%crs%cross(pstr%bridge%crosssectionnr)
               endif
               read(ibin) pstr%bridge%bedFrictionType
               read(ibin) pstr%bridge%bedFriction
               read(ibin) pstr%bridge%length
               read(ibin) pstr%bridge%inletlosscoeff
               read(ibin) pstr%bridge%outletlosscoeff
            
            case(ST_GENERAL_ST)
               allocate(pstr%generalst)
               read(ibin) pstr%generalst%wu1
               read(ibin) pstr%generalst%zu1
               read(ibin) pstr%generalst%wu2
               read(ibin) pstr%generalst%zu2
               read(ibin) pstr%generalst%ws
               read(ibin) pstr%generalst%zs
               read(ibin) pstr%generalst%wd1
               read(ibin) pstr%generalst%zd1
               read(ibin) pstr%generalst%wd2
               read(ibin) pstr%generalst%zd2
               read(ibin) pstr%generalst%gateloweredgelevel
               read(ibin) pstr%generalst%cgf_pos
               read(ibin) pstr%generalst%cgd_pos
               read(ibin) pstr%generalst%cwf_pos
               read(ibin) pstr%generalst%cwd_pos
               read(ibin) pstr%generalst%mugf_pos
               read(ibin) pstr%generalst%cgf_neg
               read(ibin) pstr%generalst%cgd_neg
               read(ibin) pstr%generalst%cwf_pos
               read(ibin) pstr%generalst%cwd_neg
               read(ibin) pstr%generalst%mugf_neg
               read(ibin) pstr%generalst%extraresistance
            
         end select
      
      enddo
 
      call read_hash_list_cache(ibin, network%sts%hashlist_weir)
      call read_hash_list_cache(ibin, network%sts%hashlist_culvert)
      call read_hash_list_cache(ibin, network%sts%hashlist_bridge)
      call read_hash_list_cache(ibin, network%sts%hashlist_pump)

      ! Allocate and Initialize Restart Data, will be filled when Reading Restart File
      if (network%sts%count > 0) then
         allocate(network%sts%restartData(network%sts%count, CFiHighestParameter))
         network%sts%restartData = missingValue
      endif
      
   end subroutine read_structure_cache

   subroutine write_structure_cache(ibin, sts)
   
      type(t_structureSet), intent(in)    :: sts
      integer, intent(in)                 :: ibin
      
      integer                    :: i
      integer                    :: j
      type(t_structure), pointer :: pstr

      write(ibin) sts%Count

      write(ibin) (sts%countByType(i), i = 1, ST_MAX_TYPE)
      
      write(ibin) sts%compoundCount
      write(ibin) sts%hasExtraResistance

      do i = 1, sts%Count
      
         pstr => sts%struct(i)
         
         write(ibin) pstr%id
         write(ibin) pstr%name
         write(ibin) pstr%type
         write(ibin) pstr%ibran
         write(ibin) pstr%linknumbers(1)
         write(ibin) pstr%xCoordinates(1)
         write(ibin) pstr%yCoordinates(1)
         write(ibin) pstr%chainage
         write(ibin) pstr%compound
         
         select case(pstr%type)
            case(ST_WEIR)
               write(ibin) pstr%weir%allowedflowdir
               write(ibin) pstr%weir%crestlevel
               write(ibin) pstr%weir%crestwidth
               write(ibin) pstr%weir%dischargecoeff
               write(ibin) pstr%weir%latdiscoeff
               write(ibin) pstr%weir%crestlevel
               write(ibin) pstr%weir%cmu
               
            case(ST_ORIFICE)
               write(ibin) pstr%orifice%allowedflowdir
               write(ibin) pstr%orifice%crestlevel
               write(ibin) pstr%orifice%crestwidth
               write(ibin) pstr%orifice%contrcoeff
               write(ibin) pstr%orifice%latcontrcoeff
               write(ibin) pstr%orifice%openlevel
               write(ibin) pstr%orifice%uselimitflowpos
               write(ibin) pstr%orifice%limitflowpos
               write(ibin) pstr%orifice%uselimitflowneg
               write(ibin) pstr%orifice%limitflowneg
               
            case(ST_PUMP)
               write(ibin) pstr%pump%direction
               write(ibin) pstr%pump%nrstages
            
               write(ibin) (pstr%pump%capacity(j), j = 1, pstr%pump%nrstages)
               write(ibin) (pstr%pump%ss_onlevel(j), j = 1, pstr%pump%nrstages)
               write(ibin) (pstr%pump%ss_offlevel(j), j = 1, pstr%pump%nrstages)
               write(ibin) (pstr%pump%ds_onlevel(j), j = 1, pstr%pump%nrstages)
               write(ibin) (pstr%pump%ds_offlevel(j), j = 1, pstr%pump%nrstages)
               write(ibin) (pstr%pump%ss_trigger(j), j = 1, pstr%pump%nrstages)
               write(ibin) (pstr%pump%ds_trigger(j), j = 1, pstr%pump%nrstages)
            
               call write_table_cache(ibin, pstr%pump%reducfact)
            
            case(ST_CULVERT)
               write(ibin) pstr%culvert%culvertType
               write(ibin) pstr%culvert%leftlevel
               write(ibin) pstr%culvert%rightlevel
               write(ibin) pstr%culvert%crosssectionnr      
               write(ibin) pstr%culvert%allowedflowdir
               write(ibin) pstr%culvert%length
               write(ibin) pstr%culvert%inletlosscoeff
               write(ibin) pstr%culvert%outletlosscoeff
               write(ibin) pstr%culvert%has_valve
               if (pstr%culvert%has_valve) then
                  write(ibin) pstr%culvert%valveOpening
                  call write_table_cache(ibin, pstr%culvert%losscoeff)
               endif
      
            case(ST_UNI_WEIR)
               write(ibin) pstr%uniweir%allowedflowdir
               write(ibin) pstr%uniweir%crestlevel
               write(ibin) pstr%uniweir%dischargecoeff
            
               write(ibin) pstr%uniweir%yzcount
               write(ibin) (pstr%uniweir%y(j), j = 1, pstr%uniweir%yzcount)
               write(ibin) (pstr%uniweir%z(j), j = 1, pstr%uniweir%yzcount)
               
            case(ST_BRIDGE)
               write(ibin) pstr%bridge%allowedflowdir
               write(ibin) pstr%bridge%bedLevel
               write(ibin) pstr%bridge%pillarwidth
               write(ibin) pstr%bridge%formfactor
               write(ibin) pstr%bridge%useOwnCrossSection
               write(ibin) pstr%bridge%crosssectionnr
               write(ibin) pstr%bridge%bedFrictionType
               write(ibin) pstr%bridge%bedFriction
               write(ibin) pstr%bridge%length
               write(ibin) pstr%bridge%inletlosscoeff
               write(ibin) pstr%bridge%outletlosscoeff
            
            case(ST_GENERAL_ST)
               write(ibin) pstr%generalst%wu1
               write(ibin) pstr%generalst%zu1
               write(ibin) pstr%generalst%wu2
               write(ibin) pstr%generalst%zu2
               write(ibin) pstr%generalst%ws
               write(ibin) pstr%generalst%zs
               write(ibin) pstr%generalst%wd1
               write(ibin) pstr%generalst%zd1
               write(ibin) pstr%generalst%wd2
               write(ibin) pstr%generalst%zd2
               write(ibin) pstr%generalst%gateLowerEdgeLevel
               write(ibin) pstr%generalst%cgf_pos
               write(ibin) pstr%generalst%cgd_pos
               write(ibin) pstr%generalst%cwf_pos
               write(ibin) pstr%generalst%cwd_pos
               write(ibin) pstr%generalst%mugf_pos
               write(ibin) pstr%generalst%cgf_neg
               write(ibin) pstr%generalst%cgd_neg
               write(ibin) pstr%generalst%cwf_neg
               write(ibin) pstr%generalst%cwd_neg
               write(ibin) pstr%generalst%mugf_neg
               write(ibin) pstr%generalst%extraresistance

         end select
      
      enddo
 
      call write_hash_list_cache(ibin, sts%hashlist_weir)
      call write_hash_list_cache(ibin, sts%hashlist_culvert)
      call write_hash_list_cache(ibin, sts%hashlist_bridge)
      call write_hash_list_cache(ibin, sts%hashlist_pump)
      
   end subroutine write_structure_cache

   
   subroutine readWeir(weir, md_ptr, success)
   
      type(t_weir), pointer, intent(inout)     :: weir
      type(tree_data), pointer, intent(in)     :: md_ptr
      logical, intent(inout)                   :: success 

      allocate(weir)

      call prop_get_double(md_ptr, 'structure', 'crestlevel', weir%crestlevel, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'crestwidth', weir%crestwidth, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'dischargecoeff', weir%dischargecoeff, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'latdiscoeff', weir%latdiscoeff, success)

      weir%cmu            = weir%dischargecoeff * weir%latdiscoeff

      if (success) call prop_get_integer(md_ptr, 'structure', 'allowedflowdir', weir%allowedflowdir, success)

   end subroutine readWeir


   !> Read specific data for the universal weir structure.
   !! The common fields for the structure (e.g. branchId) must have been read elsewhere.
   subroutine readUniversalWeir(uniweir, md_ptr, st_id, success)
   
      type(t_uni_weir), pointer, intent(inout) :: uniweir    !< Universal weir structure to be read into.
      type(tree_data),  pointer, intent(in)    :: md_ptr     !< ini tree pointer with user input.
      character(IdLen),          intent(in   ) :: st_id      !< Structure character Id.
      logical,                   intent(  out) :: success    !< Result status, whether reading of the structure was successful.

      integer                                      :: istat
      integer                                      :: i
      double precision                             :: lowestz
      character(len=Idlen)                         :: txt
      logical                                      :: success1

      success = .true.
      allocate(uniweir)
      
      call prop_get_double(md_ptr, '', 'crestLevel', uniweir%crestlevel, success1)
      success = success .and. check_input_result(success1, st_id, 'crestLevel')
      
      call prop_get_double(md_ptr, '', 'dischargeCoeff', uniweir%dischargecoeff, success1)
      success = success .and. check_input_result(success1, st_id, 'dischargeCoeff')
      
      call prop_get_string(md_ptr, '', 'allowedFlowDir', txt, success1)
      success = success .and. check_input_result(success1, st_id, 'allowedFlowDir')
      
      uniweir%allowedflowdir = allowedFlowDirToInt(txt)
      
      call prop_get_integer(md_ptr, '', 'numLevels', uniweir%yzcount, success1) 
      success = success .and. check_input_result(success1, st_id, 'numLevels')

      if (success) then
         call realloc(uniweir%y, uniweir%yzcount, stat=istat)
         if (istat == 0) call realloc(uniweir%z, uniweir%yzcount, stat=istat)
         if (istat .ne. 0) then
            call SetMessage(LEVEL_ERROR, 'Reading Universal Weir: '''//trim(st_id)//'''. Error Allocating Y/Z Arrays')
            success = .false.
            return
         endif

         call prop_get_doubles(md_ptr, '', 'yValues', uniweir%y, uniweir%yzcount, success1)
         success = success .and. check_input_result(success1, st_id, 'yValues')
      
         call prop_get_doubles(md_ptr, '', 'zValues', uniweir%z, uniweir%yzcount, success1)
         success = success .and. check_input_result(success1, st_id, 'zValues')
      endif
      
      ! The z-values contains a relative height with respect to the crest level.
      ! As a result the minimal value for Z must be 0. Shift-user-specified values to 0.
      lowestz = huge(1d0)
      do i = 1, uniweir%yzcount
         lowestz = min(lowestz, uniweir%z(i))
      enddo
      do i = 1, uniweir%yzcount
         uniweir%z(i) = uniweir%z(i) - lowestz
      enddo

   end subroutine readUniversalWeir


   !> Read the culvert specific data.
   !! The common fields for the structure (e.g. branchId) must have been read elsewhere.
   subroutine readCulvert(culvert, network, md_ptr, st_id, forcinglist, success)
   
      use messageHandling
      
      type(t_culvert), pointer,           intent(inout) :: culvert         !< Culvert structure to be read into.
      type(t_network),                    intent(inout) :: network         !< Network data structure, to which a crosssection may be added for the culvert.
      type(tree_data), pointer,           intent(in   ) :: md_ptr          !< ini tree pointer with user input.
      character(IdLen),                   intent(in   ) :: st_id           !< Structure character Id.
      type(t_forcinglist),                intent(inout) :: forcinglist     !< List of all (structure) forcing parameters, to which pump forcing will be added if needed.
      logical,                            intent(  out) :: success         !< Result status, whether reading of the structure was successful.
 
      character(len=IdLen)                        :: CrsDefID, txt
      integer                                     :: CrsDefIndx
      integer                                     :: icross
      
      integer                                     :: bedFrictionType
      double precision                            :: bedFriction
      integer                                     :: groundFrictionType
      double precision                            :: groundFriction
      integer                                     :: valveonoff
      
      integer                                     :: lossCoeffCount
      integer                                     :: istat
      double precision, allocatable, dimension(:) :: relOpen
      double precision, allocatable, dimension(:) :: lossCoeff
      logical                                     :: success1 

      success = .true.
      allocate(culvert)

      call prop_get_string(md_ptr, '', 'csDefId', CrsDefID, success1)
      success = success .and. check_input_result(success1, st_id, 'csDefId')
      
      if (success) then
         CrsDefIndx = hashsearch(network%CSDefinitions%hashlist, CrsDefID)
         if (CrsDefIndx <= 0) then
            call setMessage(LEVEL_ERROR, 'Error Reading Culvert '''//trim(st_id)//''': Cross-Section Definition '''//trim(CrsDefID)//''' not found.')
            success = .false.
         endif
      endif
   
      call prop_get_string(md_ptr, '', 'bedFrictionType', txt, success1)
      success = success .and. check_input_result(success1, st_id, 'bedFrictionType')
      if (success) then
         call frictionTypeStringToInteger(txt, bedFrictionType)
         if (bedFrictionType < 0) then
            call setMessage(LEVEL_ERROR, 'Error Reading Culvert '''//trim(st_id)//''': invalid bedFrictionType '''//trim(txt)//'''.')
            success = .false.
         end if
      end if

      call prop_get_double(md_ptr, '', 'bedFriction', bedFriction, success1)
      success = success .and. check_input_result(success1, st_id, 'bedFriction')
      
      groundFrictionType = 0
      groundFriction = 45d0
      call prop_get_integer(md_ptr, '', 'groundFrictionType', groundFrictionType)
      call prop_get_double(md_ptr, '', 'groundFriction', groundFriction)
         
      icross = AddCrossSection(network%crs, network%CSDefinitions, 0, 0.0d0, CrsDefIndx, 0.0d0, &
                               bedFrictionType, bedFriction, groundFrictionType, groundFriction)
      network%crs%cross(icross)%branchid = -1
         
      culvert%pcross         => network%crs%cross(icross)
      culvert%crosssectionnr = icross
      
      culvert%culvertType    = ST_CULVERT
      
      call prop_get_string(md_ptr, '', 'allowedFlowDir', txt, success1)
      success = success .and. check_input_result(success1, st_id, 'allowedFlowDir')
      if (success) culvert%allowedflowdir = allowedFlowDirToInt(txt)
      
      call prop_get_double(md_ptr, '', 'length', culvert%length, success1) 
      success = success .and. check_input_result(success1, st_id, 'length')
      
      call prop_get_double(md_ptr, '', 'leftLevel', culvert%leftlevel, success1) 
      success = success .and. check_input_result(success1, st_id, 'leftLevel')
      
      call prop_get_double(md_ptr, '', 'rightLevel', culvert%rightlevel, success1) 
      success = success .and. check_input_result(success1, st_id, 'rightLevel')
      
      call prop_get_double(md_ptr, '', 'inletLossCoeff', culvert%inletlosscoeff, success1) 
      success = success .and. check_input_result(success1, st_id, 'inletLossCoeff')
      
      call prop_get_double(md_ptr, '', 'outletLossCoeff', culvert%outletlosscoeff, success1) 
      success = success .and. check_input_result(success1, st_id, 'outletLossCoeff')

      call prop_get_integer(md_ptr, '', 'valveOnOff', valveonoff, success1)
      success = success .and. check_input_result(success1, st_id, 'valveOnOff')
      
      if (valveonoff == 1) then
         
         culvert%has_valve = .true.
         
         call get_value_or_addto_forcinglist(md_ptr, 'valveOpeningHeight', culvert%valveOpening, st_id, ST_CULVERT, forcinglist, success1)
         success = success .and. check_input_result(success1, st_id, 'valveOpeningHeight')
         
         call prop_get_integer(md_ptr, '', 'numLossCoeff', lossCoeffCount, success1) ! UNST-2710: new consistent keyword
         success = success .and. check_input_result(success1, st_id, 'numLossCoeff')
         if (success1) then   
            call realloc(relOpen, lossCoeffCount, stat=istat)
            if (istat == 0) call realloc(lossCoeff, lossCoeffCount, stat=istat)
            if (istat /= 0) then
               call SetMessage(LEVEL_ERROR, 'Reading Culvert: Error Allocating Valve Loss Arrays')
               success = .false.
            endif

            call prop_get_doubles(md_ptr, '', 'relOpening', relOpen, lossCoeffCount, success1)
            success = success .and. check_input_result(success1, st_id, 'relOpening')
            
            call prop_get_doubles(md_ptr, '', 'lossCoeff', lossCoeff, lossCoeffCount, success1)
            success = success .and. check_input_result(success1, st_id, 'lossCoeff')
         
            call setTable(culvert%lossCoeff, 0, relOpen, lossCoeff, lossCoeffCount)
            ! Clear Valve Loss Arrays
            istat = 0
            if (allocated(relOpen)) deallocate(relOpen, stat=istat)
            if (istat == 0 .and. allocated(lossCoeff)) deallocate(lossCoeff, stat=istat)
            if (istat .ne. 0) then
               call SetMessage(LEVEL_ERROR, 'Reading Culvert: Error Deallocating Valve Loss Arrays')
               success = .false.
            endif
         endif
         
      else
         culvert%has_valve = .false.
         culvert%valveOpening = 0.0d0
      endif
   
   end subroutine readCulvert
   
   !> Read the bridge specific data for a bridge structure.
   !! The common fields for the structure (e.g. branchId) must have been read elsewhere.
   subroutine readBridge(bridge,network, md_ptr, st_id, success)
      
      type(t_bridge),  pointer,  intent(inout) :: bridge          !< Bridge structure to be read into.
      type(t_network),           intent(inout) :: network         !< Network data structure, to which a crosssection may be added for the bridge.
      type(tree_data), pointer,  intent(in   ) :: md_ptr          !< ini tree pointer with user input.
      character(IdLen),          intent(in   ) :: st_id           !< Structure character Id.
      logical,                   intent(  out) :: success         !< Result status, whether reading of the structure was successful.

      character(len=IdLen)                       :: txt
      character(len=IdLen)                       :: CrsDefID
      integer                                    :: CrsDefIndx
      integer                                    :: icross
      logical                                    :: isPillarBridge
      logical                                    :: success1
      
      
      success = .true.
      allocate(bridge)

      call prop_get_string(md_ptr, 'structure', 'allowedFlowDir', txt, success1)
      success = success .and. check_input_result(success1, st_id, 'allowedFlowDir')
      if (success) bridge%allowedflowdir = allowedFlowDirToInt(txt)
      
      ! Make distinction between a pillar bridge and a standard bridge
      
      bridge%pillarwidth = 0d0
      call prop_get_double(md_ptr, '', 'pillarWidth', bridge%pillarwidth, success1)
      if (success1) then
         ! pillar bridge
         call setMessage(LEVEL_ERROR, 'Error Reading Bridge '''//trim(st_id)//''': pillar bridge is currently unsupported. Only standard bridge is supported.')
         success = .false.
         ! UNST-2907: pillar bridge support removed. Keep code below for future re-enabling.
         !call prop_get_double(md_ptr, '', 'formFactor', bridge%formfactor, success1)
         !success = success .and. check_input_result(success1, st_id, 'formFactor')
         !
         !bridge%bedLevel           = 0.0d0
         !bridge%useOwnCrossSection = .false.
         !bridge%pcross             => null()
         !bridge%crosssectionnr     = 0
         !bridge%bedFrictionType    = 0
         !bridge%groundFrictionType = 0
         !bridge%bedFriction        = 0.0d0
         !bridge%groundFriction     = 0.0d0
         !bridge%length             = 0.0d0
         !bridge%inletlosscoeff     = 0.0d0
         !bridge%outletlosscoeff    = 0.0d0
         
      else
         ! Standard bridge
         call prop_get_string(md_ptr, '', 'csDefId', CrsDefID, success1)
         success = success .and. check_input_result(success1, st_id, 'csDefId')
         if (success) then
            CrsDefIndx = hashsearch(network%CSDefinitions%hashlist, CrsDefID)
            if (CrsDefIndx <= 0) then
               call setMessage(LEVEL_ERROR, 'Error Reading Bridge '''//trim(st_id)//''': Cross-Section Definition '''//trim(CrsDefID)// ''' not found.')
               success = .false.
            endif
         endif
         
         call prop_get_string(md_ptr, '', 'frictionType', txt, success1)
         success = success .and. check_input_result(success1, st_id, 'frictionType')
         if (success) then
            call frictionTypeStringToInteger(txt, bridge%bedFrictionType)
            if (bridge%bedFrictionType < 0) then
               call setMessage(LEVEL_ERROR, 'Error Reading Bridge '''//trim(st_id)//''': invalid frictionType '''//trim(txt)//'''.')
               success = .false.
            end if
         end if
         
         call prop_get_double(md_ptr, '', 'friction', bridge%bedFriction, success1)
         success = success .and. check_input_result(success1, st_id, 'friction')
         
         if (success) then
            icross = AddCrossSection(network%crs, network%CSDefinitions, 0, 0.0d0, CrsDefIndx, 0.0d0, &
                                     bridge%bedFrictionType, bridge%bedFriction, groundFrictionType = -1, groundFriction = -1d0)
            network%crs%cross(icross)%branchid = -1
         
            bridge%useOwnCrossSection = .true.
            bridge%pcross             => network%crs%cross(icross)
            bridge%crosssectionnr     = icross
            if (network%crs%cross(icross)%crossType == cs_YZ_Prof) then
               call CalcConveyance(network%crs%cross(icross))
            endif

         endif
         
         call prop_get_double(md_ptr, '', 'bedLevel', bridge%bedLevel, success1)
         success = success .and. check_input_result(success1, st_id, 'bedLevel')
         
         call prop_get_double(md_ptr, '', 'length', bridge%length, success1)
         success = success .and. check_input_result(success1, st_id, 'length')
         
         call prop_get_double(md_ptr, '', 'inletLossCoeff', bridge%inletlosscoeff, success1)
         success = success .and. check_input_result(success1, st_id, 'inletLossCoeff')
         
         call prop_get_double(md_ptr, '', 'outletLossCoeff', bridge%outletlosscoeff, success1)
         success = success .and. check_input_result(success1, st_id, 'outletLossCoeff')
         
         
         bridge%pillarwidth = 0.0d0
         bridge%formfactor  = 0.0d0

      endif
      
   end subroutine readBridge


   subroutine readDambreak(dambr, md_ptr, success)

      type(t_dambreak), pointer, intent(inout) :: dambr      
      type(tree_data), pointer, intent(in)     :: md_ptr
      logical, intent(inout)                   :: success
      
      logical :: localsuccess

      allocate(dambr)

      call prop_get_double(md_ptr, 'Structure', 'StartLocationX',  dambr%startLocationX, success)
      if (.not. success) return

      call prop_get_double(md_ptr, 'Structure', 'StartLocationY',  dambr%startLocationY, success)
      if (.not. success) return

      call prop_get_integer(md_ptr, 'Structure', 'Algorithm', dambr%algorithm, success)
      if (.not. success) return

      call prop_get_double(md_ptr, 'Structure', 'CrestLevelIni', dambr%crestLevelIni, success)
      if (.not. success) return
         
      if (dambr%algorithm == 2) then
         
         call prop_get_double(md_ptr, 'Structure', 'BreachWidthIni', dambr%breachWidthIni, success)
         if (.not. success) return

         call prop_get_double(md_ptr, 'Structure', 'CrestLevelMin', dambr%crestLevelMin, success)
         if (.not. success) return

         call prop_get_double(md_ptr, 'Structure', 'TimeToBreachToMaximumDepth', dambr%timeToBreachToMaximumDepth, success)
         if (.not. success) return

         call prop_get_double(md_ptr, 'Structure', 'F1', dambr%f1, success)
         if (.not. success) return

         call prop_get_double(md_ptr, 'Structure', 'F2', dambr%f2, success)
         if (.not. success) return

         call prop_get_double(md_ptr, 'Structure', 'Ucrit', dambr%ucrit, success)
         if (.not. success) return
         
         ! optional extra fields
         call prop_get_double(md_ptr, 'Structure', 'WaterLevelUpstreamLocationX', dambr%waterLevelUpstreamLocationX, localsuccess)
         
         call prop_get_double(md_ptr, 'Structure', 'WaterLevelUpstreamLocationY', dambr%waterLevelUpstreamLocationY, localsuccess)

         call prop_get_double(md_ptr, 'Structure', 'WaterLevelDownstreamLocationX', dambr%waterLevelDownstreamLocationX, localsuccess)
         
         call prop_get_double(md_ptr, 'Structure', 'WaterLevelDownstreamLocationY', dambr%waterLevelDownstreamLocationY, localsuccess)
         
      endif
      
      ! get the name of the tim file 
      if (dambr%algorithm == 3) then
         call prop_get_string(md_ptr, 'Structure', 'DambreakLevelsAndWidths', dambr%levelsAndWidths, success)
         if (.not. success) return         
      endif

      call prop_get_double(md_ptr, 'Structure', 'T0', dambr%t0, success)
      if (.not. success) return
      
      call setCoefficents(dambr)
      
   end subroutine

   !> Read the pump specific data for a pump structure.
   !! The common fields for the structure (e.g. branchId) must have been read elsewhere.
   subroutine readPump(pump, md_ptr, st_id, forcinglist, success)
   
      type(t_pump), pointer,        intent(inout) :: pump        !< Pump structure to be read into.
      type(tree_data), pointer,     intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      character(IdLen),             intent(in   ) :: st_id       !< Structure character Id.
      type(t_forcinglist),          intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which pump forcing will be added if needed.
      logical,                      intent(  out) :: success     !< Result status, whether reading of the structure was successful.
      
      integer                                      :: tabsize
      integer                                      :: istat
      double precision, allocatable, dimension(:)  :: head
      double precision, allocatable, dimension(:)  :: redfac
      integer :: numcap, numred, iside
      logical :: success1
      character(len=IdLen) :: txt

      success = .true.
      allocate(pump)

      ! Compute the pump%direction from two parts: orientation and optionally controlSide.
      txt = 'positive'
      call prop_get_string(md_ptr, '', 'orientation', txt)
      pump%direction = orientationToInt(txt) ! will become +1 or -1
      if (abs(pump%direction) /= 1) then
         call setMessage(LEVEL_ERROR, 'Error Reading Pump '''//trim(st_id)//''': orientation has invalid value '''//trim(txt)// '''.')
         success = .false.
      end if

      pump%nrstages = 0
      call prop_get_integer(md_ptr, '', 'numStages', pump%nrstages) ! UNST-2709: new consistent keyword

      numcap = max(1, pump%nrstages)
      allocate(pump%capacity(numcap), stat=istat)
      ! NOTE: numStages may be 0, but always read at least 1 capacity value.
      if (istat == 0) allocate(pump%ss_onlevel(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ss_offlevel(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ds_onlevel(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ds_offlevel(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ss_trigger(pump%nrstages), stat=istat)
      if (istat == 0) allocate(pump%ds_trigger(pump%nrstages), stat=istat)

      
      if (pump%nrstages == 0) then
         ! In case of 0 stages, capacity is either scalar double, or filename, or 'realtime'.
         call get_value_or_addto_forcinglist(md_ptr, 'capacity', pump%capacity(1), st_id, ST_PUMP, forcinglist, success1)
         ! addtimespace gaat qid='pump' gebruiken, de bc provider moet via FM juiste name doorkrijgen (==structureid)
         success = success .and. check_input_result(success1, st_id, 'capacity')
      else
         ! Stages with control: only support table with double precision values.
         pump%capacity = -1d0
         call prop_get_doubles(md_ptr, '', 'capacity', pump%capacity, pump%nrstages, success1)
         success = success .and. check_input_result(success1, st_id, 'capacity')
         if (any(pump%capacity < 0)) then
            call setMessage(LEVEL_ERROR, 'Error Reading Pump '''//trim(st_id)//''': a staged pump (numStages > 0) must have nonnegative capacity, and cannot be combined with time series or realtime setting.')
            success = .false.
         end if

         txt = '' ! No default controlSide
         call prop_get_string(md_ptr, '', 'controlSide', txt, success1)
         success = success .and. check_input_result(success1, st_id, 'controlSide')
         iside = controlSideToInt(txt) ! will become +1 or -1
         if (.not. (iside >= 1 .and. iside <= 3)) then
            call setMessage(LEVEL_ERROR, 'Error Reading Pump '''//trim(st_id)//''': controlSide has invalid value '''//trim(txt)// '''.')
            success = .false.
         end if

         pump%direction = pump%direction * iside  ! (+/-1 * 1 or 2 or 3)

         if (iabs(pump%direction) == 1 .or. iabs(pump%direction) == 3) then
            call prop_get_doubles(md_ptr, '', 'startLevelSuctionSide', pump%ss_onlevel, pump%nrstages, success1)
            success = success .and. check_input_result(success1, st_id, 'startLevelSuctionSide')
            call prop_get_doubles(md_ptr, '', 'stopLevelSuctionSide', pump%ss_offlevel, pump%nrstages, success1)
            success = success .and. check_input_result(success1, st_id, 'stopLevelSuctionSide')
         end if
      
         if (iabs(pump%direction) == 2 .or. iabs(pump%direction) == 3) then
            call prop_get_doubles(md_ptr, '', 'startLevelDeliverySide', pump%ds_onlevel, pump%nrstages, success1)
            success = success .and. check_input_result(success1, st_id, 'startLevelDeliverySide')
            call prop_get_doubles(md_ptr, '', 'stopLevelDeliverySide', pump%ds_offlevel, pump%nrstages, success1)
            success = success .and. check_input_result(success1, st_id, 'stopLevelDeliverySide')
         end if
      end if
      
      if (.not. success) return

      pump%ss_trigger = .true.
      pump%ds_trigger = .true.

      ! Reduction Table
      tabsize = 0
      call prop_get_integer(md_ptr, '', 'numReductionLevels', tabsize) ! UNST-2709: new consistent keyword

      numred = max(1, tabsize)
      call realloc(head, numred, stat=istat)
      if (istat == 0) call realloc(redfac, numred, stat=istat)
      if (istat /= 0) then
         call SetMessage(LEVEL_ERROR, 'Error Reading Pump: Error Allocating Reduction Factor Arrays.')
         success = .false.
         return
      end if

      if (tabsize > 0) then
         call prop_get_doubles(md_ptr, '', 'head', head, tabsize, success1)
         success = success .and. check_input_result(success1, st_id, 'head')

         call prop_get_doubles(md_ptr, '', 'reductionFactor', redfac, tabsize, success1)
         success = success .and. check_input_result(success1, st_id, 'reductionFactor')
      else
         ! When no reduction table given in input, always create one dummy entry in table with 100% pump capacity.
         tabsize   = 1
         head(1)   = 0.0d0
         redfac(1) = 1.0d0
      endif

      call setTable(pump%reducfact, 0, head, redfac, tabsize)

      ! Clear Arrays
      istat = 0
      if (allocated(head)) deallocate(head, stat=istat)
      if (istat == 0 .and. allocated(redfac)) deallocate(redfac, stat=istat)
      if (istat .ne. 0) then
         call SetMessage(LEVEL_ERROR, 'Error Reading Pump: Error Dellocating Reduction Factor Arrays.')
      endif
      
      ! Initialize Parameters for Pump
      pump%actual_stage      = 0
      pump%discharge         = 0.0d0
      pump%is_active         = .true.
      pump%pump_head         = 0.0d0
      pump%reduction_factor  = 1.0d0
      pump%ss_level          = 0.0d0
      pump%ds_level          = 0.0d0
      pump%current_capacity  = 0.0d0
      
   end subroutine readPump


   !> Either retrieve a constant value for parameter KEY, or get the filename for the time series.
   subroutine get_value_or_addto_forcinglist(md_ptr, key, value, st_id, st_type, forcinglist, success)
      type(tree_data), pointer,     intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      character(len=*),             intent(in   ) :: key         !< name of the item in the input file
      double precision, target,     intent(  out) :: value       !< The variable into which the read value may be stored.
                                                                 !< In case of a time series/realtime forcing, this variable will be pointed to in the forcinglist.
      character(IdLen),             intent(in   ) :: st_id       !< Structure character Id.
      integer,                      intent(in   ) :: st_type     !< structure type
      type(t_forcinglist),          intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which structure forcing will be added if needed.
      logical,          optional,   intent(inout) :: success     
      
      integer           :: istat   
      character(CharLn) :: tmpstr
      logical           :: success1
      
      call prop_get_string(md_ptr, '', key, tmpstr, success1)
      if (success1) then
         read(tmpstr, *, iostat = istat) value
         if (istat /= 0) then ! No number, so assume it was a filename
            forcinglist%Count = forcinglist%Count+1
            if (forcinglist%Count > forcinglist%Size) then
               call realloc(forcinglist)
            end if
            forcinglist%forcing(forcinglist%Count)%st_id      = st_id
            forcinglist%forcing(forcinglist%Count)%st_type    = st_type
            forcinglist%forcing(forcinglist%Count)%param_name = key
            forcinglist%forcing(forcinglist%Count)%targetptr  => value
            forcinglist%forcing(forcinglist%Count)%filename   = tmpstr
         end if
      endif
      
      if (present(success)) then
         success = success1
      endif
      
   end subroutine get_value_or_addto_forcinglist
   
   subroutine readOrifice(orifice, md_ptr, success)
   
      type(t_orifice), pointer, intent(inout)     :: orifice
      type(tree_data), pointer, intent(in)        :: md_ptr
      logical, intent(inout)                      :: success 
      
      double precision :: area, height
   
      allocate(orifice)
      
      call prop_get_double(md_ptr, 'structure', 'crestlevel', orifice%crestlevel, success)
      if (success) then
         call prop_get_double(md_ptr, 'structure', 'crestwidth', orifice%crestwidth, success)
         if (success) call prop_get_double(md_ptr, 'structure', 'openlevel', orifice%openlevel, success)
      else
         success = .true.
         orifice%crestlevel = 0d0
         area = 0d0
         call prop_get_double(md_ptr, 'structure', 'bottomlevel', orifice%crestlevel, success)
         call prop_get_double(md_ptr, 'structure', 'area',         area, success)
         height = sqrt(area)
         orifice%crestwidth = height
         orifice%openlevel = orifice%crestlevel+height
      endif
      
      if (success) call prop_get_integer(md_ptr, 'structure', 'allowedflowdir', orifice%allowedflowdir, success)

      if (success) call prop_get_double(md_ptr, 'structure', 'contractioncoeff', orifice%contrcoeff, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'latcontrcoeff', orifice%latcontrcoeff, success)

      if (success) then
         call prop_get_logical(md_ptr, 'structure', 'uselimitflowpos', orifice%uselimitflowpos, success)
         if (success) then
            call prop_get_double(md_ptr, 'structure', 'limitflowpos', orifice%limitflowpos, success)
         else
            orifice%uselimitflowpos = .false.
            success = .true.
         endif
      endif
      
      if (success) then
         call prop_get_logical(md_ptr, 'structure', 'uselimitflowneg', orifice%uselimitflowneg, success)
         if (success) then
            call prop_get_double(md_ptr, 'structure', 'limitflowneg', orifice%limitflowneg, success)
         else
            orifice%uselimitflowneg = .false.
            success = .true.
         endif
      endif
   
   end subroutine readOrifice

   !> Read the weir parameters and define a general structure.
   !! The common fields for the structure (e.g. branchId) must have been read elsewhere.
   subroutine readWeirAsGenStru(generalst, md_ptr, st_id, forcinglist, success)
   
      use messageHandling
      
      type(t_GeneralStructure), pointer,  intent(inout) :: generalst   !< General structure to be read into.
      type(tree_data), pointer,           intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      character(IdLen),                   intent(in   ) :: st_id       !< Structure character Id.
      type(t_forcinglist),                intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which weir forcing will be added if needed.
      logical,                            intent(  out) :: success     !< Result status, whether reading of the structure was successful.
      
      logical           :: success1
      success = .true.
      
      allocate(generalst)

      generalst%ws = 1d10
      call prop_get_double(md_ptr, '', 'crestWidth', generalst%ws)

      call get_value_or_addto_forcinglist(md_ptr, 'crestLevel', generalst%zs, st_id, ST_WEIR, forcinglist, success1)
      success = success .and. check_input_result(success1, st_id, 'crestLevel')

      generalst%mugf_pos = 1d0
      if (success) call prop_get_double(md_ptr, '', 'corrCoeff',  generalst%cgf_pos)

      generalst%velheight = .true.
      call prop_get(md_ptr, '', 'useVelocityHeight',  generalst%velheight)
      
      ! all levels are set to -1d-10. In the time loop these parameters will be set to the bed level.
      generalst%zu1                = -1d10
      generalst%zu2                = -1d10
      generalst%zd1                = -1d10
      generalst%zd2                = -1d10
      generalst%wu1                = generalst%ws
      generalst%wu2                = generalst%ws
      generalst%wd1                = generalst%ws
      generalst%wd2                = generalst%ws
      generalst%gateLowerEdgeLevel = 1d10
      generalst%cgf_pos            = generalst%cgf_pos
      generalst%cgd_pos            = generalst%cgf_pos
      generalst%cwf_pos            = generalst%cgf_pos
      generalst%cwd_pos            = generalst%cgf_pos
      generalst%cgf_neg            = generalst%cgf_pos
      generalst%cgd_neg            = generalst%cgf_pos
      generalst%cwf_neg            = generalst%cgf_pos
      generalst%cwd_neg            = generalst%cgf_pos
      generalst%mugf_neg           = 1d0
      generalst%mugf_pos           = 1d0
      generalst%extraresistance    = 0d0
      generalst%gatedoorheight     = 1d10
      generalst%gateopeningwidth   = generalst%ws
      generalst%crestlength        = 0d0
      generalst%openingDirection   = GEN_SYMMETRIC

   end subroutine readWeirAsGenStru
 
   !> Read the orifice or gate parameters and define a general structure.
   !! The common fields for the structure (e.g. branchId) must have been read elsewhere.
   subroutine readOrificeAsGenStru(generalst, md_ptr, st_id, forcinglist, success)
   
      use messageHandling
      
      type(t_GeneralStructure), pointer,  intent(inout) :: generalst   !< General structure to be read into. 
      type(tree_data), pointer,           intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      character(IdLen),                   intent(in   ) :: st_id       !< Structure character Id.
      type(t_forcinglist),                intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which orifice forcing will be added if needed.
      logical,                            intent(  out) :: success     !< Result status, whether reading of the structure was successful.
      
      logical        :: success1
      
      success = .true.
      allocate(generalst)

      call get_value_or_addto_forcinglist(md_ptr, 'crestLevel', generalst%zs, st_id, ST_GENERAL_ST, forcinglist, success1)
      success = success .and. check_input_result(success1, st_id, 'crestLevel')

      generalst%mugf_pos = 1d0
      call prop_get_double(md_ptr, '', 'corrCoeff',  generalst%cgf_pos)

      generalst%ws = 1d10
      call prop_get_double(md_ptr, '', 'crestWidth',  generalst%ws)
      
      call get_value_or_addto_forcinglist(md_ptr, 'gateLowerEdgeLevel', generalst%gateLowerEdgeLevel, st_id, ST_GENERAL_ST, &
                                                       forcinglist, success1)
      success = success .and. check_input_result(success1, st_id, 'gateLowerEdgeLevel')

      generalst%velheight = .true.
      call prop_get(md_ptr, '', 'useVelocityHeight',  generalst%velheight)

      ! Set default/standard values for orifice
      ! all levels are set to -1d-10. In the time loop these parameters will be set to the bed level.
      generalst%zu1                = -1d10
      generalst%zu2                = -1d10
      generalst%zd1                = -1d10
      generalst%zd2                = -1d10
      generalst%wu1                = generalst%ws
      generalst%wu2                = generalst%ws
      generalst%wd1                = generalst%ws
      generalst%wd2                = generalst%ws
      generalst%cgf_pos            = generalst%cgf_pos
      generalst%cgd_pos            = generalst%cgf_pos
      generalst%cwf_pos            = generalst%cgf_pos
      generalst%cwd_pos            = generalst%cgf_pos
      generalst%cgf_neg            = generalst%cgf_pos
      generalst%cgd_neg            = generalst%cgf_pos
      generalst%cwf_neg            = generalst%cgf_pos
      generalst%cwd_neg            = generalst%cgf_pos
      generalst%mugf_neg           = 1d0
      generalst%mugf_pos           = 1d0
      generalst%extraresistance    = 0d0
      generalst%gatedoorheight     = 1d10
      generalst%gateopeningwidth   = 0d0
      generalst%crestlength        = 0d0
      generalst%openingDirection   = GEN_SYMMETRIC ! TODO: once 2D structures are being read by this reader, also support fromleft and fromright

   end subroutine readOrificeAsGenStru


   !> Helper routine to check the result status of a read/prop_get action.
   !! Checks if success is true or false, when false generate an error message.
   !! Result value is the original success value.
   function check_input_result(success, st_id, key) result (res)
      logical         , intent(in   )    :: success   !< Result value of the prop_get subroutine.
      character(len=*), intent(in   )    :: st_id     !< Id of the current structure.
      character(len=*), intent(in   )    :: key       !< Key of the input value.
      logical                            :: res       !< Result status, is equal to the original success value.
                                                      !< Recommended use: successall = successall .and. check_input_result(success, ..)

      if (.not. success) then
         write (msgbuf, '(a,a,a,a,a)') 'Error Reading Structure ''', trim(st_id), ''', ''', trim(key), ''' is missing.'
         call err_flush()
      endif
      res = success
      return 
   end function check_input_result


   !> Read the general structure parameters.
   !! The common fields for the structure (e.g. branchId) must have been read elsewhere.
   subroutine readGeneralStructure(generalst, md_ptr, st_id, forcinglist, success)
   
      use messageHandling
      
      type(t_GeneralStructure), pointer,  intent(inout) :: generalst   !< General structure to be read into.
      type(tree_data), pointer,           intent(in   ) :: md_ptr      !< ini tree pointer with user input.
      character(IdLen),                   intent(in   ) :: st_id       !< Structure character Id.
      type(t_forcinglist),                intent(inout) :: forcinglist !< List of all (structure) forcing parameters, to which general structure forcing will be added if needed.
      logical,                            intent(  out) :: success     !< Result status, whether reading of the structure was successful.
      
      character(len=Idlen) :: dirstring
      logical              :: success1

      success = .true.
      allocate(generalst)

      generalst%wu1                = 10d0
      call prop_get_double(md_ptr, '', 'upstream1Width', generalst%wu1, success1)
      generalst%wu2                = 10d0
      call prop_get_double(md_ptr, '', 'upstream2Width',  generalst%wu2, success1)
      generalst%ws                 = 10d0
      call get_value_or_addto_forcinglist(md_ptr, 'crestWidth', generalst%ws, st_id, ST_GENERAL_ST, forcinglist)
      generalst%wd1                = 10d0
      call prop_get_double(md_ptr, '', 'downstream1Width', generalst%wd1, success1)
      generalst%wd2                = 10d0
      call prop_get_double(md_ptr, '', 'downstream2Width',   generalst%wd2, success1)

      generalst%zu1                = 0d0
      call prop_get_double(md_ptr, '', 'upstream1Level',   generalst%zu1, success1)
      generalst%zu2                = 0d0
      call prop_get_double(md_ptr, '', 'upstream2Level',  generalst%zu2, success1)
      generalst%zs                 = 0d0
      call get_value_or_addto_forcinglist(md_ptr, 'crestLevel',    generalst%zs, st_id, ST_GENERAL_ST, forcinglist, success1)
      generalst%zd1                = 0d0
      call prop_get_double(md_ptr, '', 'downstream1Level', generalst%zd1, success1)
      generalst%zd2                = 0d0
      call prop_get_double(md_ptr, '', 'downstream2Level',  generalst%zd2, success1)

      generalst%gateLowerEdgeLevel = 11d0
      call get_value_or_addto_forcinglist(md_ptr, 'gateLowerEdgeLevel', generalst%gateLowerEdgeLevel, st_id, ST_GENERAL_ST, forcinglist, success1)
      generalst%crestlength        = 0d0
      call prop_get_double(md_ptr, '', 'crestLength',   generalst%crestlength)
      generalst%gatedoorheight     = 1d10
      call prop_get_double(md_ptr, '', 'gateHeight',   generalst%gatedoorheight, success1)
      generalst%gateopeningwidth   = 0d0
      call get_value_or_addto_forcinglist(md_ptr, 'gateOpeningWidth', generalst%gateopeningwidth, st_id, ST_GENERAL_ST, forcinglist, success1)

      dirString = 'symmetric'
      call prop_get_string(md_ptr, '', 'gateOpeningHorizontalDirection',   dirString)
      generalst%openingDirection = openingDirectionToInt(dirString)
      

      generalst%cgf_pos            = 1d0
      call prop_get_double(md_ptr, '', 'posFreeGateflowCoeff',  generalst%cgf_pos)
      generalst%cgd_pos            = 1d0
      call prop_get_double(md_ptr, '', 'posDrownGateFlowCoeff', generalst%cgd_pos)
      generalst%cwf_pos            = 1d0
      call prop_get_double(md_ptr, '', 'posFreeWeirFlowCoeff',  generalst%cwf_pos)
      generalst%cwd_pos            = 1d0
      call prop_get_double(md_ptr, '', 'posDrownWeirFlowCoeff', generalst%cwd_pos)
      generalst%mugf_pos           = 1d0  
      call prop_get_double(md_ptr, '', 'posContrCoefFreeGate',  generalst%mugf_pos)
      
      generalst%cgf_neg            = 1d0
      call prop_get_double(md_ptr, '', 'negFreeGateFlowCoeff',  generalst%cgf_neg)
      generalst%cgd_neg            = 1d0
      call prop_get_double(md_ptr, '', 'negDrownGateFlowCoeff', generalst%cgd_neg)
      generalst%cwf_neg            = 1d0
      call prop_get_double(md_ptr, '', 'negFreeWeirFlowCoeff',  generalst%cwf_neg)
      generalst%cwd_neg            = 1d0
      call prop_get_double(md_ptr, '', 'negDrownWeirFlowCoeff', generalst%cwd_neg)
      generalst%mugf_neg           = 1d0
      call prop_get_double(md_ptr, '', 'negContrCoefFreeGate',  generalst%mugf_neg)
      
      generalst%extraresistance    = 0d0
      call prop_get_double(md_ptr, '', 'extraResistance', generalst%extraresistance)
      
      generalst%velheight = .true.
      call prop_get(md_ptr, '', 'useVelocityHeight',  generalst%velheight)

   end subroutine readGeneralStructure


   !> Parses a (pump's) orientation string into an integer
   !! that can be used in its direction field.
   function orientationToInt(orientationString) result (res)
      character(len=*), intent(in) :: orientationString !< Orientation value as given in input file.
      integer                      :: res               !< The returned orientation integer code. +1 for positive, -1 for negative, 0 for invalid input.

      select case(str_tolower(trim(orientationString)))
      case('positive')
         res = 1
      case('negative')
         res = -1
      case default
         res = 0
      end select
      
   end function orientationToInt


   !> Parses a (pump's) controlSide string into an integer
   !! that can be used as part of its direction field.
   function controlSideToInt(controlSideString) result (res)
      character(len=*), intent(in) :: controlSideString !< controlSide value as given in input file.
      integer                      :: res               !< The returned controlSide integer code. 1 for suctionSide, 2 for deliverySide, 3 for both, 0 for invalid input.

      select case(str_tolower(trim(controlSideString)))
      case('suctionside')
         res = 1
      case('deliveryside')
         res = 2
      case('both')
         res = 3
      case default
         res = 0
      end select
      
   end function controlSideToInt


   integer function  openingDirectionToInt(dirString)
      character(len=*), intent(inout) :: dirString
      call str_lower(dirString)
      select case(dirString)
      case('symmetric')
         openingDirectionToInt = GEN_SYMMETRIC
      case('fromleft')
         openingDirectionToInt = GEN_FROMLEFT
      case('fromright')
         openingDirectionToInt = GEN_FROMRIGHT
      case default
         openingDirectionToInt = GEN_SYMMETRIC
      end select
      
   end function  openingDirectionToInt
   
   integer function allowedFlowDirToInt(flowdirString)
      character(len=*), intent(inout) :: flowdirString
   
      call str_lower(flowdirString)
      select case(flowdirString)
      case('both')
         allowedFlowDirToInt = 0
      case('positive')
         allowedFlowDirToInt = 1
      case('negative')
         allowedFlowDirToInt = 2
      case('none')
         allowedFlowDirToInt = 3
      case default
         allowedFlowDirToInt = 0
      end select
      
   end function  allowedFlowDirToInt
  
   !> Read the general structure parameters for version 1.00 files
   subroutine readGeneralStructure_v100(generalst, md_ptr, success)
   
      type(t_GeneralStructure), pointer, intent(inout)     :: generalst     !< general structure to be read into 
      type(tree_data), pointer, intent(in)                 :: md_ptr        !< ini tree pointer with user input.
      logical, intent(inout)                               :: success 
      
      allocate(generalst)

      allocate(generalst%fu(3,1), generalst%ru(3,1), generalst%au(3,1))
      allocate(generalst%widthcenteronlink(1), generalst%gateclosedfractiononlink(1), generalst%sOnCrest(1), generalst%state(3,1))
      generalst%numlinks = 1
      generalst%gateclosedfractiononlink(1) = 1d0
      generalst%gatedoorheight = huge(1d0)
      generalst%velheight = .true.
      call prop_get_double(md_ptr, 'structure', 'widthLeftW1', generalst%wu1, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'widthleftWsdl',  generalst%wu2, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'widthcenter',    generalst%ws, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'widthrightWsdr', generalst%wd1, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'widthrightW2',   generalst%wd2, success)
                                                                               
      if (success) call prop_get_double(md_ptr, 'structure', 'levelleftZb1',   generalst%zu1, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'levelleftZbsl',  generalst%zu2, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'levelcenter',    generalst%zs, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'levelrightZbsr', generalst%zd1, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'levelrightZb2',  generalst%zd2, success)

      if (success) call prop_get_double(md_ptr, 'structure', 'gateLowerEdgeLevel', generalst%gateLowerEdgeLevel, success)

      if (success) call prop_get_double(md_ptr, 'structure', 'posfreegateflowcoeff',  generalst%cgf_pos, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'posdrowngateflowcoeff', generalst%cgd_pos, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'posfreeweirflowcoeff',  generalst%cwf_pos, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'posdrownweirflowcoeff', generalst%cwd_pos, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'poscontrcoeffreegate',  generalst%mugf_pos, success)
      
      if (success) call prop_get_double(md_ptr, 'structure', 'negfreegateflowcoeff',  generalst%cgf_neg, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negdrowngateflowcoeff', generalst%cgd_neg, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negfreeweirflowcoeff',  generalst%cwf_pos, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negdrownweirflowcoeff', generalst%cwd_neg, success)
      if (success) call prop_get_double(md_ptr, 'structure', 'negcontrcoeffreegate',  generalst%mugf_neg, success)
      
      if (success) call prop_get_double(md_ptr, 'structure', 'extraresistance', generalst%extraresistance, success)
     
   end subroutine readGeneralStructure_v100
  
end module m_readstructures
