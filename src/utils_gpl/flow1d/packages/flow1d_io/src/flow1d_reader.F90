module m_flow1d_reader
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
!  $Id: flow1d_reader.F90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_io/src/flow1d_reader.F90 $
!-------------------------------------------------------------------------------
  
   use ModelParameters
   
   implicit none
   
   private
   
   public read_1d_mdu
   public read_1d_model
   public read_1d_attributes
   
   logical :: files_have_been_read = .false. ! temporary flag (needed as long as WaterflowModel1D
   public files_have_been_read               ! calls 'ReadFiles' and d_hydro.exe does not)
   ! files_have_been_read also used to retrieve interpolated cross-section data by delta-shell!!
   
   contains
   
   subroutine read_1d_mdu(filenames, network, got_1d_network)

      use string_module
      use m_globalParameters
      use messageHandling
      use m_readModelParameters
      use m_readCrossSections
      use m_readSpatialData
      use m_read_roughness
      use m_network
      use m_readSalinityParameters
      use m_1d_networkreader
      use m_readstructures
      use m_readObservationPoints
      use m_readStorageNodes
      use properties
      use cf_timers
   
      implicit none
      
      ! Variables
      type(t_filenames), intent(in)       :: filenames
      type(t_network), intent(inout)      :: network
      logical, intent(out)                :: got_1d_network
      
      type(tree_data), pointer        :: md_ptr
      character(len=charln)           :: inputfile
      integer                         :: numstr
      integer                         :: backslash
      integer                         :: slash
      integer                         :: posslash
      integer                         :: maxErrorLevel
      logical                         :: success
      
      integer                         :: istat
      character(len=255)              :: md1d_flow1d_file

      md1d_flow1d_file = filenames%onednetwork

      ! Check on Empty File Name
      if (len_trim(md1d_flow1d_file) <= 0) then
         got_1d_network = .false.
         return
      endif

      ! Convert c string to fortran string and read md1d file into tree
      call tree_create(trim(md1d_flow1d_file), md_ptr, maxlenpar)
      call prop_inifile(trim(md1d_flow1d_file), md_ptr, istat)
      if (istat /= 0) then
            call setmessage(LEVEL_FATAL, 'Error opening md1d file ' // trim(md1d_flow1d_file))
      endif

      success = .true.

      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if
      
      slash = index(md1d_flow1d_file, '/', back = .true.)
      backslash = index(md1d_flow1d_file, '\', back = .true.)
      posslash = max(slash, backslash)

      call SetMessage(LEVEL_INFO, 'Reading Network ...')
      
      ! Get network data
      inputfile=''
      success = .true.
         
         ! Try the INI-File due to Morphology 
         call prop_get_string(md_ptr, 'files', 'networkFile', inputfile, success)
         inputfile = md1d_flow1d_file(1:posslash)//inputfile
         if (success .and. len_trim(inputfile) > 0) then
            call NetworkReader(network, inputfile)
            if (network%nds%Count < 2 .or. network%brs%Count < 1) then
            got_1d_network = .false.
            else
               got_1d_network = .true.
            endif

         endif

      call SetMessage(LEVEL_INFO, 'Reading Network Done')

      call tree_destroy(md_ptr)
      
      ! Stop in case of errors
      maxErrorLevel = getMaxErrorLevel()
      if (maxErrorLevel >= LEVEL_ERROR) then
         call LogAllParameters()
         call SetMessage(LEVEL_FATAL, 'Error(s) during reading model data from files')
      endif
      
      call SetMessage(LEVEL_INFO, '1D-Network Reading Done')

      files_have_been_read = .true.
      
      call tree_destroy(md_ptr)
      
   end subroutine read_1d_mdu

   subroutine set_filename(md_ptr, head, tag, inputfile, folder)
      use properties
      type(tree_data), pointer, intent(in)        :: md_ptr
      character(len=*), intent(in)                :: head
      character(len=*), intent(in)                :: tag
      character(len=*), intent(in)                :: folder
      character(len=*), intent(inout)             :: inputfile
      
      logical :: success
      
      if (len_trim(inputfile) == 0) then
         call prop_get_string(md_ptr, head, tag, inputfile, success)
         inputfile = trim(folder)//inputfile
      endif

   end subroutine set_filename
   
   
   subroutine read_1d_attributes(filenames, network)

      use string_module
      use m_globalParameters
      use messageHandling
      use m_readModelParameters
      use m_readCrossSections
      use m_readSpatialData
      use m_read_roughness
      use m_network
      use m_readSalinityParameters
      use m_1d_networkreader
      use m_readstructures
      use m_readObservationPoints
      use m_readStorageNodes
      use properties
      use cf_timers
   
      implicit none
      
      ! Variables
      type(t_filenames), intent(inout):: filenames
      type(t_network), intent(inout)  :: network
      
      type(tree_data), pointer        :: md_ptr
      character(len=charln)           :: folder
      integer                         :: numstr
      integer                         :: backslash
      integer                         :: slash
      integer                         :: posslash
      integer                         :: maxErrorLevel
      logical                         :: success
      
      integer                         :: istat
      
      integer                         :: timerRead          = 0
      integer                         :: timerReadCsDefs    = 0
      integer                         :: timerReadCsLocs    = 0
      integer                         :: timerReadStructs   = 0
      integer                         :: timerReadStorgNodes= 0
      integer                         :: timerReadRoughness = 0
      integer                         :: timerFileUnit
      character(len=255)              :: md1d_flow1d_file

      ! Convert c string to fortran string and read md1d file into tree
      
      nullify(md_ptr)
      md1d_flow1d_file = filenames%onednetwork
      folder = filenames%roughnessdir
      call timini()
      timon = .true.
      
      if (len_trim(md1d_flow1d_file) > 0) then
         call tree_create(trim(md1d_flow1d_file), md_ptr, maxlenpar)
         call prop_inifile(trim(md1d_flow1d_file), md_ptr, istat)
         if (istat /= 0) then
               call setmessage(LEVEL_FATAL, 'Error opening md1d file ' // trim(md1d_flow1d_file))
         endif
      
         success = .true.
      
         numstr = 0
         if (associated(md_ptr%child_nodes)) then
            numstr = size(md_ptr%child_nodes)
         end if
      
         slash = index(md1d_flow1d_file, '/', back = .true.)
         backslash = index(md1d_flow1d_file, '\', back = .true.)
         posslash = max(slash, backslash)
      
         if (posslash > 0) then
            folder = md1d_flow1d_file(1:posslash)
         else
            folder = ' '
 
         endif
         
         if (len_trim(filenames%roughness) == 0) then
            filenames%roughnessdir = folder
         else
            folder = '  '
            filenames%roughnessdir = '  '
         endif
         if (len_trim(filenames%onednetwork) > 0) then
            call set_filename(md_ptr, 'files', 'roughnessfile' , filenames%roughness,                  ' ')
            call set_filename(md_ptr, 'files', 'crossDefFile'  , filenames%cross_section_definitions,  folder)
            call set_filename(md_ptr, 'files', 'crossLocFile'  , filenames%cross_section_locations,    folder)
            call set_filename(md_ptr, 'files', 'structureFile' , filenames%structures,                 folder)
            call set_filename(md_ptr, 'files', 'StorageNodeFile',filenames%storage_nodes,              folder)
         endif
      endif
      
      call timini()
      timon = .true.

      success = .true.
     
      call timstrt('ReadFiles', timerRead)

      ! Read roughnessFile file
      call timstrt('ReadRoughness', timerReadRoughness)
      call SetMessage(LEVEL_INFO, 'Reading Roughness ...')

      call roughness_reader(network, filenames%roughness, folder)
     
      call SetMessage(LEVEL_INFO, 'Reading Roughness Done')
      call timstop(timerReadRoughness)

      ! Read cross section definitions
      call timstrt('ReadCsDefs', timerReadCsDefs)
      call SetMessage(LEVEL_INFO, 'Reading Cross Section Definitions ...')
     
      call readCrossSectionDefinitions(network, filenames%cross_section_definitions)
      
      if (network%CSDefinitions%Count < 1) then
         call SetMessage(LEVEL_WARN, 'No Cross_Section Definitions Found')
      endif
     
      call SetMessage(LEVEL_INFO, 'Reading Cross Section Definitions Done')
      call timstop(timerReadCsDefs)
      
      ! Read cross section locations
      call timstrt('ReadCsLocs', timerReadCsLocs)
      call SetMessage(LEVEL_INFO, 'Reading Cross Section Locations ...')

      call readCrossSectionLocationFile(network, filenames%cross_section_locations)
         
     if (network%crs%Count < 1) then
        call SetMessage(LEVEL_WARN, 'No Cross Sections Found')
     endif

     call SetMessage(LEVEL_INFO, 'Reading Cross Section Locations Done')
     call timstop(timerReadCsLocs)
  
     if (len_trim(filenames%storage_nodes) > 0) then ! if a storage node file is specified
        call timstrt('ReadStorageNodes', timerReadStorgNodes)
        call SetMessage(LEVEL_INFO, 'Reading Storage Nodes ...')
        
        ! Read storage nodes file
        call readStorageNodes(network, filenames%storage_nodes)
         
        call SetMessage(LEVEL_INFO, 'Reading Storage Nodes Done')
        call timstop(timerReadStorgNodes)
     end if

     if (len_trim(md1d_flow1d_file) > 0) then

        call SetMessage(LEVEL_INFO, 'Reading Advanced Parameters ...')
        call prop_get_double(md_ptr, 'advancedoptions', 'transitionheightsd', summerDikeTransitionHeight, success)
        if (.not. success) then 
           call SetMessage(LEVEL_FATAL, 'Error reading Advanced Parameters')
        endif
        call SetMessage(LEVEL_INFO, 'Reading Advanced Parameters Done')
     else 
        summerDikeTransitionHeight = 0.5d0
     endif
     
      
     ! log timings
     call timstop(timerRead)
     open(newunit=timerFileUnit, file='read-model-timings.log')
     call timdump(timerFileUnit)
     close(timerFileUnit)
     
     call tree_destroy(md_ptr)
     
     ! Stop in case of errors
     maxErrorLevel = getMaxErrorLevel()
     if (maxErrorLevel >= LEVEL_ERROR) then
        call LogAllParameters()
        call SetMessage(LEVEL_FATAL, 'Error(s) during reading model data from files')
     endif
     
     call SetMessage(LEVEL_INFO, 'All 1D-Reading Done')

     files_have_been_read = .true.
     
     call tree_destroy(md_ptr)

   end subroutine read_1d_attributes

   subroutine read_1d_model(md_flow1d_file, md_ptr, network, nc_outputdir)
   
      use system_utils
      use string_module
      use m_globalParameters
      use messageHandling
      use m_readModelParameters
      use m_readCrossSections
      use m_readSpatialData
      use m_read_roughness
      use m_network
      use m_readSalinityParameters
      use m_1d_networkreader
      use m_readstructures
      use m_readObservationPoints
      use m_readStorageNodes
      use properties
      use cf_timers

      character(len=*), intent(inout) :: nc_outputdir
      character(len=*), intent(inout) :: md_flow1d_file
      type(t_network), intent(inout)  :: network
      type(tree_data), pointer        :: md_ptr
      character(len=charln)           :: inputfile
      integer                         :: numstr
      integer                         :: backslash
      integer                         :: slash
      integer                         :: posslash
      integer                         :: posdot
      integer                         :: quantity
      integer                         :: isp
      integer                         :: maxErrorLevel
      logical                         :: success
      logical                         :: extra_output
      logical                         :: UseInitialWaterDepth
      double precision                :: default
      
      character(len=Charln)           :: binfile
      character(len=1024  )           :: filestring
      logical                         :: file_exist
      integer                         :: istat
      integer                         :: ibin
      
      integer                         :: timerRead          = 0
      integer                         :: timerReadNetwork   = 0
      integer                         :: timerReadCsDefs    = 0
      integer                         :: timerReadCsLocs    = 0
      integer                         :: timerReadStructs   = 0
      integer                         :: timerReadBoundLocs = 0
      integer                         :: timerReadLatLocs   = 0
      integer                         :: timerReadObsPoints = 0
      integer                         :: timerReadStorgNodes= 0
      integer                         :: timerReadInitial   = 0
      integer                         :: timerReadRoughness = 0
      integer                         :: timerReadBoundData = 0
      integer                         :: timerFileUnit
      integer                         :: res 
      
      call timini()
      timon = .true.

      success = .true.
      
      call timstrt('ReadFiles', timerRead)

      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if
      
      slash = index(md_flow1d_file, '/', back = .true.)
      backslash = index(md_flow1d_file, '\', back = .true.)
      posslash = max(slash, backslash)

      nc_outputdir = trim(md_flow1d_file(1:posslash))//'output'
      res = makedir(nc_outputdir)
      nc_outputdir = trim(nc_outputdir)//'/'
      ! Model Parameters
      inputfile=''
      success = .true.
      call prop_get_string(md_ptr, 'files', 'sobekSimIniFile', inputfile, success)
      if (success) then
         inputfile = md_flow1d_file(1:posslash)//inputfile
      else
         inputfile = ' '
      endif
      call readModelParameters(md_ptr, inputfile)
      
      ! Read or Set Name of Water Level State Files
      call prop_get_string(md_ptr, 'files', 'wlevstatefilein', wlevStateFileIn, success)
      if (.not. success) wlevStateFileIn = 'wlevStateFileIn.xyz'
      call prop_get_string(md_ptr, 'files', 'wlevstatefileout', wlevStateFileOut, success)
      if (.not. success) wlevStateFileOut = 'wlevStateFileOut.xyz'

      call timstrt('ReadNetwork', timerReadNetwork)
      call SetMessage(LEVEL_INFO, 'Reading Network ...')
      
      ! Get network data
      inputfile=''
      success = .true.
      if (readNetworkFromUgrid) then
         call prop_get_string(md_ptr, 'files', 'networkUgridFile', inputfile, success)
      else
         call prop_get_string(md_ptr, 'files', 'networkFile', inputfile, success)
      endif
      inputfile = md_flow1d_file(1:posslash)//inputfile
      if (success .and. len_trim(inputfile) > 0) then
      
         posdot = index(inputFile, '.', back = .true.)
         binfile = inputFile(1:posdot)//'cache'
         inquire(file=binfile, exist=file_exist)
         if (doReadCache .and. file_exist) then
            open(newunit=ibin, file=binfile, status='old', form='unformatted', access='stream', action='read', iostat=istat)
            if (istat /= 0) then
               call setmessage(LEVEL_FATAL, 'Error opening Network Cache file')
               ibin = 0
            endif
            call read_network_cache(ibin, network)
            close(ibin)
            ibin = 0
            
         else
         
            if (readNetworkFromUgrid) then
               ! Read from UGRID-File
               call NetworkUgridReader(network, inputfile)
            else
               ! Read from INI-File
               call NetworkReader(network, inputfile)

            endif
            
         endif

      endif
      
      if (network%nds%Count < 2 .or. network%brs%Count < 1) then
         call SetMessage(LEVEL_FATAL, 'No Proper Network Found')
      endif

      call SetMessage(LEVEL_INFO, 'Reading Network Done')
      call timstop(timerReadNetwork)
      call timstrt('ReadRoughness', timerReadRoughness)
      call SetMessage(LEVEL_INFO, 'Reading Roughness ...')

      ! Read roughnessFile file
      
      !
      ! NOTE: UNST-2871: the MD1D keyword roughnessFile is still supported.
      !       But when used directly in the D-Flow FM .mdu file, use frictFile instead.
      !
      call prop_get_string(md_ptr, 'Files', 'roughnessFile', filestring, success)
      if (.not. success) then
         return
      endif

      call roughness_reader(network, filestring, md_flow1d_file(1:posslash))
      
      call SetMessage(LEVEL_INFO, 'Reading Roughness Done')
      call timstop(timerReadRoughness)
      call timstrt('ReadCsDefs', timerReadCsDefs)
      call SetMessage(LEVEL_INFO, 'Reading Cross Section Definitions ...')
      
      ! Read cross section definition file
      inputfile=''
      call prop_get_string(md_ptr, 'files', 'crossDefFile', inputfile, success)
      inputfile = md_flow1d_file(1:posslash)//inputfile
      if (success .and. len_trim(inputfile) > 0) then
         call readCrossSectionDefinitions(network, inputfile)
      endif

      if (network%CSDefinitions%Count < 1) then
         call SetMessage(LEVEL_FATAL, 'No Any Cross_Section Definition Found')
      endif
      
      call SetMessage(LEVEL_INFO, 'Reading Cross Section Definitions Done')
      call timstop(timerReadCsDefs)
      call timstrt('ReadCsLocs', timerReadCsLocs)
      call SetMessage(LEVEL_INFO, 'Reading Cross Section Locations ...')

      ! Read cross section location file
      inputfile=''
      call prop_get_string(md_ptr, 'files', 'crossLocFile', inputfile, success)
      inputfile = md_flow1d_file(1:posslash)//inputfile
      if (success .and. len_trim(inputfile) > 0) then
         call readCrossSectionLocationFile(network, inputfile)
      endif

      if (network%crs%Count < 1) then
         call SetMessage(LEVEL_FATAL, 'Not Any Cross Section Found')
      endif
      
      call prop_get_logical(md_ptr, 'SimulationOptions', 'OutputYZConveyanceTables', extra_output, success)
      if (success .and. extra_output) then
         call write_crosssection_data(network%crs, network%brs)
      endif
      
      call SetMessage(LEVEL_INFO, 'Reading Cross Section Locations Done')
      call timstop(timerReadCsLocs)

      call timstrt('ReadObsPoints', timerReadObsPoints)
      call SetMessage(LEVEL_INFO, 'Reading Observation Points ...')

      ! Read observation points file
      inputfile=''
      call prop_get_string(md_ptr, 'files', 'obsPointsFile', inputfile, success)
      if (success) then
         call prop_get_string(md_ptr, 'Observations', 'interpolationType', obsIntPolType, success)
         call str_lower(obsIntPolType)    
         if (success .and. obsIntPolType== 'linear') then
            network%obs%interpolationType = OBS_LINEAR
         else
            network%obs%interpolationType = OBS_NEAREST
         endif
         
         inputfile = md_flow1d_file(1:posslash)//inputfile
         if (success .and. len_trim(inputfile) > 0) then
            call readObservationPoints(network, inputfile)
         endif
      endif

      call SetMessage(LEVEL_INFO, 'Reading Observation Points Done')
      call timstop(timerReadObsPoints)
      call timstrt('ReadStorageNodes', timerReadStorgNodes)
      call SetMessage(LEVEL_INFO, 'Reading Storage Nodes ...')

      ! Read Storage Nodes file
      inputfile=''
      call prop_get_string(md_ptr, 'files', 'StorageNodeFile', inputfile, success)
      inputfile = md_flow1d_file(1:posslash)//inputfile
      if (success .and. len_trim(inputfile) > 0) then
         call readStorageNodes(network, inputfile)
      endif
      
      call SetMessage(LEVEL_INFO, 'Reading Storage Nodes Done')
      call timstop(timerReadStorgNodes)
      call timstrt('ReadInitData', timerReadInitial)
      call SetMessage(LEVEL_INFO, 'Reading Initial Data ...')
      
      binfile = md_flow1d_file(1:posslash)//'SpatialData.cache'
      inquire(file=binfile, exist=file_exist)
      if (doReadCache .and. file_exist) then
         open(newunit=ibin, file=binfile, status='old', form='unformatted', access='stream', action='read', iostat=istat)
         if (istat /= 0) then
            call setmessage(LEVEL_FATAL, 'Error Opening Spatial Data Cache File: '//trim(binfile))
            ibin = 0
         endif
         call read_spatial_data_cache(ibin, network)
         close(ibin)
      else
      
         ! Read initial data files
         UseInitialWaterDepth = .false.
         call prop_get_logical(md_ptr, 'GlobalValues', 'UseInitialWaterDepth', UseInitialWaterDepth, success)
         inputfile=''
         default = 0d0
         if (UseInitialWaterDepth) then
            call prop_get_string(md_ptr, 'files', 'initialWaterDepthFile', inputfile, success)
            call prop_get_double(md_ptr, 'GlobalValues', 'initialWaterDepth', default, success)
            quantity = CFiWaterDepth
         else
            call prop_get_string(md_ptr, 'files', 'initialWaterLevelFile', inputfile, success)
            call prop_get_double(md_ptr, 'GlobalValues', 'initialWaterLevel', default, success)
            quantity = CFiWaterlevel
         endif
         if (success .and. len_trim(inputfile) > 0) then
            inputfile = md_flow1d_file(1:posslash)//inputfile
            call spatial_data_reader(isp, network%spdata, network%brs, inputfile, default, quantity, .true.)
            call SetMessage(LEVEL_INFO, 'Initial Water Level/Depth Loaded')
         endif
      
         inputfile=''
         call prop_get_string(md_ptr, 'files', 'initialDischargeFile', inputfile, success)
         if (success .and. len_trim(inputfile) > 0) then
            inputfile = md_flow1d_file(1:posslash)//inputfile
            default = 0d0
            call prop_get_double(md_ptr, 'GlobalValues', 'InitialDischarge', default, success)
            call spatial_data_reader(isp, network%spdata, network%brs, inputfile, default, CFiDischarge, .true.)
            call SetMessage(LEVEL_INFO, 'Initial Discharge Data Loaded')
         endif
      
         inputfile=''
         call prop_get_string(md_ptr, 'files', 'initialSalinityFile', inputfile, success)
         inputfile = md_flow1d_file(1:posslash)//inputfile
         default = 5d0
         call prop_get_double(md_ptr, 'GlobalValues', 'InitialSalinity', default, success)
         call spatial_data_reader(isp, network%spdata, network%brs, inputfile, default, CFiSalinity, .true.)
         if (transportPars%salt_index > 0) then
            transportPars%co_h(transportPars%salt_index)%initial_values_index = isp
            transportPars%co_h(transportPars%salt_index)%name                 = 'water_salinity'
         endif
         
         call SetMessage(LEVEL_INFO, 'Initial Salinity Data Loaded')
         !endif
      
         inputfile=''
         call prop_get_string(md_ptr, 'files', 'initialTemperatureFile', inputfile, success)
         inputfile = md_flow1d_file(1:posslash)//inputfile
         default = 14d0
         call prop_get_double(md_ptr, 'GlobalValues', 'InitialTemperature', default, success)
         call spatial_data_reader(isp, network%spdata, network%brs, inputfile, default, CFiTemperature, .true.)
         transportPars%co_h(transportPars%temp_index)%initial_values_index = isp
         transportPars%co_h(transportPars%temp_index)%name                 = 'water_temperature'
         
         call SetMessage(LEVEL_INFO, 'Initial Temperature Data Loaded')
      
         inputfile=''
         call prop_get_string(md_ptr, 'files', 'dispersionFile', inputfile, success)    
         if (success .and. len_trim(inputfile) > 0) then
            inputfile = md_flow1d_file(1:posslash)//inputfile
            default = 0d0
            call prop_get_double(md_ptr, 'GlobalValues', 'Dispersion', default, success)
            call spatial_data_reader(isp, network%spdata, network%brs, inputfile, default, CFiTH_F1, .true.)
         else
            inputfile=''
            call prop_get_string(md_ptr, 'files', 'f1File', inputfile, success)    
            default = 0d0
            call prop_get_double(md_ptr, 'GlobalValues', 'f1', default, success)
            if (success .or. len_trim(inputfile) > 0) then
               inputfile = md_flow1d_file(1:posslash)//inputfile
               call spatial_data_reader(isp, network%spdata, network%brs, inputfile, default, CFiTH_F1, .true.)
               call SetMessage(LEVEL_INFO, 'Thatcher-Harleman F1 Data Loaded')
            endif
         endif

         inputfile=''
         call prop_get_string(md_ptr, 'files', 'f3File', inputfile, success)    
         if (success .and. len_trim(inputfile) > 0) then
            inputfile = md_flow1d_file(1:posslash)//inputfile
            default = 0d0
            call prop_get_double(md_ptr, 'GlobalValues', 'f3', default, success)
            call spatial_data_reader(isp, network%spdata, network%brs, inputfile, default, CFiTH_F3, .true.)
            call SetMessage(LEVEL_INFO, 'Thatcher-Harleman F3 Data Loaded')
         endif

         inputfile=''
         call prop_get_string(md_ptr, 'files', 'f4File', inputfile, success)    
         if (success .or. len_trim(inputfile) > 0) then
            inputfile = md_flow1d_file(1:posslash)//inputfile
            default = 0d0
            call prop_get_double(md_ptr, 'GlobalValues', 'f4', default, success)
            call spatial_data_reader(isp, network%spdata, network%brs, inputfile, default, CFiTH_F4, .true.)
            call SetMessage(LEVEL_INFO, 'Thatcher-Harleman F4 Data Loaded')
         endif

         inputfile=''
         call prop_get_string(md_ptr, 'files', 'ConvLengthFile', inputfile, success)    
         if (success .and. len_trim(inputfile) > 0) then
            inputfile = md_flow1d_file(1:posslash)//inputfile
            default = 0d0
            call prop_get_double(md_ptr, 'GlobalValues', 'ConvLength', default, success)
            call spatial_data_reader(isp, network%spdata, network%brs, inputfile, default, CFiConvLength, .true.)
            call SetMessage(LEVEL_INFO, 'ConvLength Data Loaded')
         endif

         inputfile=''
         call prop_get_string(md_ptr, 'files', 'windShieldingFile', inputfile, success)    
         if (success .and. len_trim(inputfile) > 0) then
            inputfile = md_flow1d_file(1:posslash)//inputfile
            default = 1d0
            call spatial_data_reader(isp, network%spdata, network%brs, inputfile, default, CFiWindShield, .true.)
            call SetMessage(LEVEL_INFO, 'Windshielding Data Loaded')
         endif
         
      endif

      inputfile=''
      transportpars%USE_F4_DISPERSION = .false.
      call prop_get_string(md_ptr, 'files', 'salinityParametersFile', inputfile, success)
      if (success .and. len_trim(inputfile) > 0) then
         inputfile = md_flow1d_file(1:posslash)//inputfile
         call readSalinityParameters(network, inputfile)
         call SetMessage(LEVEL_INFO, 'Salinity Parameters Loaded')
      endif
      
      call SetMessage(LEVEL_INFO, 'Reading Initial Data Done')
      call timstop(timerReadInitial)

      ! log timings
      call timstop(timerRead)
      open(newunit=timerFileUnit, file='read-model-timings.log')
      call timdump(timerFileUnit)
      close(timerFileUnit)
      
      call tree_destroy(md_ptr)
      
     call SetMessage(LEVEL_INFO, 'Clean Up Spatial Data')
     call freeLocationData(network%spData)
     call SetMessage(LEVEL_INFO, 'Cleaning Up Spatial Data Done')
      
      ! Stop in case of errors
      maxErrorLevel = getMaxErrorLevel()
      if (maxErrorLevel >= LEVEL_ERROR) then
         call LogAllParameters()
         call SetMessage(LEVEL_FATAL, 'Error(s) during reading model data from files')
      endif
      
      call SetMessage(LEVEL_INFO, 'All Reading Done')

   end subroutine read_1d_model
   
end module m_flow1d_reader
    
    
   
