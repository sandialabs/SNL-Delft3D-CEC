module m_readModelParameters
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
!  $Id: readModelParameters.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_io/src/readModelParameters.f90 $
!-------------------------------------------------------------------------------

   use MessageHandling
   use m_globalParameters
   use ModelParameters
   use properties
   use string_module

   implicit none

   private

   public readModelParameters

   contains

   subroutine readModelParameters(md_ptr, sobekSimIniFile)
      use m_temperature
      
      implicit none
      
      
      type(tree_data), pointer, intent(in)      :: md_ptr
      character(len=Charln)                     :: sobekSimIniFile

      integer                                   :: iValue
      double precision                          :: Value
      logical                                   :: success
      character(len=Idlen)                      :: scheme, heat_model, dens_comp

      call setModelParameterDefaults()
      
      call readFromModelDefinitionFile(md_ptr)
      
      call ReadSobekSimIni(sobekSimIniFile)
      
      ! Retrieve Minimum Distance between Grid Points
      call prop_get_double(md_ptr, 'NumericalParameters', 'MinimumLength', Value, success)
      if (success) then
         minSectionLength = Value
      endif
      
      ! Read Interpolation Type for Obsevation Points
      call prop_get_string(md_ptr, 'Observations', 'interpolationType', obsIntPolType, success)
      if (.not. success) then
         obsIntPolType = 'OBS_NEAREST'
      endif
      
      ! Read Salt Switch
      call prop_get_integer(md_ptr, 'Salinity', 'SaltComputation', iValue, success)
      if (success .and. (iValue .ne. 0)) then
         transportPars%do_salt = .true.
         transportPars%salt_index = 1
         transportPars%constituents_count = 1
         call AddOrReplaceParameter('Salinity', 'SaltComputation', 'true', .true.)
      else
         transportPars%do_salt = .false.
         transportPars%salt_index = -1
         transportPars%constituents_count = 0
         call AddOrReplaceParameter('Salinity', 'SaltComputation', 'false', .true.)
      endif
      
      ! Read Temperature Switch
      call prop_get_integer(md_ptr, 'TransportComputation', 'Temperature', iValue, success)
      if (success .and. (iValue .ne. 0)) then
         transportPars%do_temp = .true.
         call AddOrReplaceParameter('TransportComputation', 'Temperature', 'true', .true.)
         transportPars%constituents_count = transportPars%constituents_count + 1
         transportPars%temp_index = transportPars%constituents_count 
         call default_heatfluxes()
         value = 15d0
         call prop_get_double(md_ptr, 'Temperature', 'BackgroundTemperature', value, success)
         call set_par_temperature('air_temperature', value)
         value = 1d6
         call prop_get_double(md_ptr, 'Temperature', 'surfaceArea', value, success)
         call set_par_temperature('s_area', value)
         value = 1d5
         call prop_get_double(md_ptr, 'Temperature', 'atmosphericPressure', value, success)
         call set_par_temperature('p_atm', value)
         value = 0.0013
         call prop_get_double(md_ptr, 'Temperature', 'daltonNumber', value, success)
         call set_par_temperature('c_e_dalton', value)
         value = 3930d0
         call prop_get_double(md_ptr, 'Temperature', 'heatCapacityWater', value, success)
         call set_par_temperature('c_p', value)
         value = 0.0013
         call prop_get_double(md_ptr, 'Temperature', 'stantonNumber', value, success)
         call set_par_temperature('c_h_stanton', value)
      else
         transportPars%do_temp = .false.
         transportPars%temp_index = -1
         call AddOrReplaceParameter('TransportComputation', 'Temperature', 'false', .true.)
      endif
      
      if (.not. transportPars%do_salt) then
         transportPars%salt_index = transportPars%constituents_count + 1
      endif
      if (.not. transportPars%do_temp) then
         transportPars%temp_index = max(transportPars%constituents_count + 1, transportPars%salt_index + 1)
      endif
      
      dens_comp = 'eckart_modified'
      call prop_get_string(md_ptr, 'TransportComputation', 'Density', dens_comp, success)
      call str_lower(dens_comp, 999)
      select case (trim(dens_comp))
      case ('eckart_modified', 'eckhart_modified')
         transportPars%density = DENS_ECKART_MODIFIED
         msgbuf = 'Eckart(modified)'
         call AddOrReplaceParameter('TransportComputation', 'Density', 'true', .true.)
      case ('eckart', 'eckhart')
         transportPars%density = DENS_ECKART
         msgbuf = 'Eckart'
         call AddOrReplaceParameter('TransportComputation', 'Density', 'true', .true.)
      case ('unesco')
         transportPars%density = DENS_UNESCO
         msgbuf = 'Unesco'
         call AddOrReplaceParameter('TransportComputation', 'Density', 'true', .true.)
      case default
         msgbuf = 'unknown density type'
         call err_flush()
      end select
      msgbuf = 'Density computation set to '//trim(msgbuf)
      call msg_flush()
         
      heat_model = 'transport'
      call prop_get_string(md_ptr, 'TransportComputation', 'HeatTransferModel', heat_model, success)
      call str_lower(heat_model, 999)
      select case (trim(heat_model))
      case ('transport')
         tempPars%heat_model = HEAT_TRANSPORT
         call AddOrReplaceParameter('TransportComputation', 'HeatTransferModel', 'transport', .true.)
      case ('excess')
         tempPars%heat_model = HEAT_EXCESS
         call AddOrReplaceParameter('TransportComputation', 'HeatTransferModel', 'excess', .true.)
      case ('composite')
         tempPars%heat_model = HEAT_COMPOSITE
         call AddOrReplaceParameter('TransportComputation', 'HeatTransferModel', 'composite', .true.)
      case default
         tempPars%heat_model = HEAT_TRANSPORT
         call AddOrReplaceParameter('TransportComputation', 'HeatTransferModel', 'transport', .true.)
      end select
      
      call prop_get_double(md_ptr, 'Salinity', 'Teta', transportPars%teta, success)
      
      scheme = 'vanLeer-2'
      call prop_get_string(md_ptr, 'salinity', 'advectionScheme', scheme, success)
      if (scheme == 'vanLeer-2') then
         transportPars%advection_scheme = ADV_VANLEER
      else
         transportPars%advection_scheme = ADV_UPWIND
      endif
      
      ! Read Read-From-UGrid Switch
      call prop_get_integer(md_ptr, 'SimulationOptions', 'ReadNetworkFromUgrid', iValue, success)
      if (success .and. (iValue .ne. 0)) then
         readNetworkFromUgrid = .true.
         call AddOrReplaceParameter('SimulationOptions', 'ReadNetworkFromUgrid', 'true', .true.)
      else
         readNetworkFromUgrid = .false.
         call AddOrReplaceParameter('SimulationOptions', 'ReadNetworkFromUgrid', 'false', .true.)
      endif

      ! Read Write-NetCDF Switch
      call prop_get_integer(md_ptr, 'SimulationOptions', 'WriteNetCDF', iValue, success)
      if (success .and. (iValue .ne. 0)) then
         writeNetCDF = .true.
         call AddOrReplaceParameter('SimulationOptions', 'WriteNetCDF', 'true', .true.)
      else
         writeNetCDF = .false.
         call AddOrReplaceParameter('SimulationOptions', 'WriteNetCDF', 'false', .true.)
      endif

      latitude = 0d0
      longitude = 0d0
      time_zone = 0d0
      call prop_get_double(md_ptr, 'AdvancedOptions', 'Latitude' , latitude,  success)
      call prop_get_double(md_ptr, 'AdvancedOptions', 'Longitude', longitude, success)
      call prop_get_double(md_ptr, 'AdvancedOptions', 'timeZone', time_zone, success)
      
      write_tables = .false.
      call prop_get_logical(md_ptr, 'StorageTable', 'WriteStorageTables', write_tables, success)
      if (write_tables) then
         call prop_get_string(md_ptr, 'StorageTable', 'StorageOutputFile', st_filename, success)
         if (.not. success) then
            call setmessage(LEVEL_ERROR, 'StorageOutputFile not found in md1d file')
         endif
         tb_inc = 0.1d0
         call prop_get_double(md_ptr, 'StorageTable', 'StorageTableIncrement', tb_inc, success)
         tb_extra_height = 0d0
         call prop_get_double(md_ptr, 'StorageTable', 'ExtraHeight', tb_extra_height, success)
      endif
      
      call prop_get_integer(md_ptr, 'Morphology', 'CalculateMorphology', iValue, success)
      if (success .and. iValue==1) then
         call AddOrReplaceParameter('Morphology', 'CalculateMorphology', 'true', .true.)
         
         call prop_get_string(md_ptr, 'Morphology', 'SedimentInputFile', scheme, success)
         if (success .and. iValue==1) then
            call AddOrReplaceParameter('Morphology', 'SedimentInputFile', scheme, .true.)
         endif  
         
         call prop_get_string(md_ptr, 'Morphology', 'MorphologyInputFile', scheme, success)
         if (success .and. iValue==1) then
            call AddOrReplaceParameter('Morphology', 'MorphologyInputFile', scheme, .true.)
         endif    
 
    endif
     
   end subroutine readModelParameters
   
   subroutine readFromModelDefinitionFile(md_ptr)

      type(tree_data), pointer, intent(in)      :: md_ptr
   
      integer                                   :: numstr
      character(len=40)                         :: category
      character(len=40)                         :: keyWord
      character(len=40)                         :: keyValue
      type(tree_data), pointer                  :: cat_ptr
      integer                                   :: numkeys
      integer                                   :: icat
      integer                                   :: ikey
   
      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if
   
      do icat = 1, numstr  ! Skipping 'General', 'Files' and 'GlobalValues'
         
         category = tree_get_name(md_ptr%child_nodes(icat)%node_ptr)
         call str_lower(category)
         
         if (category == 'general' .or. category == 'files' .or. category == 'globalvalues') cycle
         
         if (category == 'time') then
            call readDateTimeStepData(md_ptr)
            cycle
         endif
         
         cat_ptr => md_ptr%child_nodes(icat)%node_ptr
         numkeys = 0
         if (associated(cat_ptr%child_nodes)) then
            numkeys = size(cat_ptr%child_nodes)
         endif
         
         do ikey = 1, numkeys
            
            keyWord = tree_get_name(cat_ptr%child_nodes(ikey)%node_ptr)
            keyValue = tree_get_data(cat_ptr%child_nodes(ikey)%node_ptr)

            call str_lower(keyWord)
            call str_lower(keyValue)
            
            call AddOrReplaceParameter(category, keyWord, keyValue, .true.)
                        
            ! Set CacheMode Switches
            if (category == 'advancedoptions' .and. keyWord == 'cachemode') then
            
               select case (keyValue)
                  case ('none')
                     doReadCache  = .false.
                     doWriteCache = .false.
                  case ('read')
                     doReadCache  = .true.
                     doWriteCache = .false.
                  case ('write')
                     doReadCache  = .false.
                     doWriteCache = .true.
                  case default
                     doReadCache  = .false.
                     doWriteCache = .false.
               end select
            endif
            
         enddo
         
      enddo

   
   end subroutine readFromModelDefinitionFile
   
   subroutine readDateTimeStepData(md_ptr)

      type(tree_data), pointer, intent(in)      :: md_ptr
      
      character(Len=40)                         :: startTime
      character(Len=40)                         :: stopTime
      double precision                          :: timeStep
      double precision                          :: mapOutput
      double precision                          :: hisOutput
      logical                                   :: success
      
      integer                                   :: iYear
      integer                                   :: iMonth
      integer                                   :: iDay
      integer                                   :: iHour
      integer                                   :: iMinute
      integer                                   :: iSecond

      double precision                          :: julDate
      double precision                          :: jul1jan
      integer                                   :: restartTime
     
      call prop_get_string(md_ptr, 'time', 'starttime', startTime, success)   
      if (success) call prop_get_string(md_ptr, 'time', 'stoptime', stopTime, success)   
      if (success) call prop_get_double(md_ptr, 'time', 'timestep', timeStep, success)   
   
      if (.not. success) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Date Time Data')
      endif
      
      if (timeStep <= 0.0d0) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Date Time Data: Time Step must be > 0.0')
      endif

      read (startTime, '(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)') iYear, iMonth, iDay, iHour, iMinute, iSecond
      
      julDate = julian(iYear, iMonth, iDay, iHour, iMinute, iSecond)
      if (julDate <= 0.0d0) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Date Time Data: Invalid Start Date/Time')
      endif
      
      modelTimeStepData%julianStart = juldate
      
      modelTimeStepData%startDate = iYear * 10000 + iMonth * 100 + iDay
      modelTimeStepData%startTime = iHour * 10000 + iMinute * 100 + iSecond     
      
      jul1jan = julian(iYear, 1, 1, 0, 0, 0)
      modelTimeStepData%hoursToStartFromFirstOfJanuari = (juldate - jul1jan)*24d0
      
      read (stopTime, '(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)') iYear, iMonth, iDay, iHour, iMinute, iSecond

      julDate = julian(iYear, iMonth, iDay, iHour, iMinute, iSecond)
      if (julDate <= 0.0d0) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Date Time Data: Invalid Stop Date/Time')
      endif
      
      modelTimeStepData%julianEnd = juldate

      modelTimeStepData%endDate = iYear * 10000 + iMonth * 100 + iDay
      modelTimeStepData%endTime = iHour * 10000 + iMinute * 100 + iSecond
      
      if (modelTimeStepData%julianEnd < modelTimeStepData%julianStart) then
         call SetMessage(LEVEL_FATAL, 'Error Reading Date Time Data: Stop Time must be after Start Time')
      endif
      
      ! Map Output Time Step
      call prop_get_double(md_ptr, 'time', 'mapoutputtimestep', mapOutput, success)
      if (success) then 
         if (mod(mapOutput, timeStep) > 0.0d0 .or. mapOutput < timeStep) then
            call SetMessage(LEVEL_ERROR, 'Error Reading Date Time Data: Map Output Time Step must be multiple of Time Step')
         endif
      else
         ! Check for 'outtimestepgridpoints' can be removed in future
         call prop_get_double(md_ptr, 'time', 'outtimestepgridpoints', mapOutput, success)
         if (success) then 
            call SetMessage(LEVEL_WARN, 'Keyword OutTimeStepGridPoints has been depreciated, better use MapOutputTimeStep')
            if (mod(mapOutput, timeStep) > 0.0d0 .or. mapOutput < timeStep) then
               call SetMessage(LEVEL_ERROR, 'Error Reading Date Time Data: Map Output Time Step must be multiple of Time Step')
            endif
         else
            call SetMessage(LEVEL_WARN, 'No Map Output Time Step specified, Calculation Time Step will be used')
            mapOutput = timeStep
         endif
      endif
      
      ! His Output Time Step
      call prop_get_double(md_ptr, 'time', 'hisoutputtimestep', hisOutput, success)
      if (success) then 
         if (mod(hisOutput, timeStep) > 0.0d0 .or. hisOutput < timeStep) then
            call SetMessage(LEVEL_ERROR, 'Error Reading Date Time Data: His Output Time Step must be multiple of Time Step')
         endif
      else
         ! Check for 'outtimestepstructures' can be removed in future
         call prop_get_double(md_ptr, 'time', 'outtimestepstructures', hisOutput, success)
         if (success) then
            call SetMessage(LEVEL_WARN, 'Keyword OutTimeStepStructures has been depreciated, better use HisOutputTimeStep')
            if (mod(hisOutput, timeStep) > 0.0d0 .or. hisOutput < timeStep) then
               call SetMessage(LEVEL_ERROR, 'Error Reading Date Time Data: His Output Time Step must be multiple of Time Step')
            endif
         else
            call SetMessage(LEVEL_WARN, 'No His Output Time Step specified, Calculation Time Step will be used')
            hisOutput = timeStep
         endif
      endif
      
      modelTimeStepData%timeStep          = timeStep
      modelTimeStepData%mapOutputTimeStep = mapOutput
      modelTimeStepData%hisOutputTimeStep = hisOutput
         
      ! restart data
      call prop_get_string(md_ptr, 'restart', 'restartstarttime', startTime, success)   
      if (success) call prop_get_string(md_ptr, 'restart', 'restartstoptime', stopTime, success)   
      if (success) call prop_get_double(md_ptr, 'restart', 'restarttimestep', timeStep, success)   
      if (success) call prop_get_logical(md_ptr, 'restart', 'writeRestart', modelTimeStepData%writerestart, success)   
      
      if (success .and. modelTimeStepData%writerestart) then
         
         if (timeStep <= 0.0d0) then
            call SetMessage(LEVEL_FATAL, 'Error Reading Date Time Data: Time Step must be > 0.0')
         endif

         read (startTime, '(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)') iYear, iMonth, iDay, iHour, iMinute, iSecond
         julDate = julian(iYear, iMonth, iDay, iHour, iMinute, iSecond)
         RestartTime = nint((juldate - modelTimeStepData%julianStart)*86400)
         modelTimeStepData%nextRestarttimestep =restartTime/modelTimeStepData%timeStep
         
         read (stopTime, '(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)') iYear, iMonth, iDay, iHour, iMinute, iSecond
         julDate = julian(iYear, iMonth, iDay, iHour, iMinute, iSecond)
         RestartTime = nint((juldate - modelTimeStepData%julianStart)*86400)
         modelTimeStepData%restartendTimestep = restartTime/modelTimeStepData%timeStep
         
         if (mod(timeStep, modelTimeStepData%timeStep) > 0.0d0 .or. timeStep < modelTimeStepData%timeStep) then
            call SetMessage(LEVEL_FATAL, 'Error Reading Date Time Data: Output Time Step must be multiple of Time Step')
         endif
         modelTimeStepData%restartInterval = nint(timeStep/modelTimeStepData%timeStep)
         
         if (modelTimeStepData%nextRestarttimestep < 0) then
            modelTimeStepData%nextRestarttimestep = modelTimeStepData%nextRestarttimestep - &
                                   (modelTimeStepData%nextRestarttimestep/modelTimeStepData%restartInterval)*modelTimeStepData%restartInterval
            if (modelTimeStepData%nextRestarttimestep < 0) then
               modelTimeStepData%nextRestarttimestep = modelTimeStepData%nextRestarttimestep + modelTimeStepData%restartInterval
            endif
         endif
         
      elseif (.not. success) then
         
         ! TODO remove this part in due time, for now it stays compatibility reasons:
         modelTimeStepData%nextRestarttimestep = nint((modelTimeStepData%julianEnd - modelTimeStepData%julianStart) * 86400)

         call prop_get_logical(md_ptr, 'SimulationOptions', 'WriteRestart', modelTimeStepData%writeRestart, success)
         
         if (success) then
            call SetMessage(LEVEL_WARN, 'Keyword WriteRestart under SimulationOptions has been depreciated, better use WriteRestart under Restart')
         endif

      endif
      
      modelTimeStepData%restartFile =  '  '
      modelTimeStepData%useRestart = .false.
      call prop_get_logical(md_ptr, 'Restart', 'UseRestart',   modelTimeStepData%useRestart,   success) 
      if (success) then 
         call prop_get_string (md_ptr, 'Restart', 'restartfile', modelTimeStepData%restartFile, success)   
      else
         ! TODO remove this part in due time, for now it stays compatibility reasons:
         call prop_get_logical(md_ptr, 'SimulationOptions', 'UseRestart',   modelTimeStepData%useRestart, success) 
         if (success) then
            call SetMessage(LEVEL_WARN, 'Keyword UseRestart under SimulationOptions has been depreciated, better use UseRestart under Restart')
         endif
      endif
      
      
   end subroutine readDateTimeStepData
   
   subroutine setModelParameterDefaults()
   
      character(len=40)                    :: category
   
      category = 'InitialConditions'
      call AddOrReplaceParameter(category, "InitialEmptyWells", '0', .true.)

      category = 'ResultsNodes'
      call AddOrReplaceParameter(category, 'Lateral1d2d',          '0', .true.)
      call AddOrReplaceParameter(category, 'LateralOnNodes',       '0', .true.)
      call AddOrReplaceParameter(category, 'LevelFromStreetLevel', '0', .true.)
      call AddOrReplaceParameter(category, 'RunOff',               '0', .true.)
      call AddOrReplaceParameter(category, 'TimeWaterOnStreet',    '0', .true.)
      call AddOrReplaceParameter(category, 'TotalArea',            '0', .true.)
      call AddOrReplaceParameter(category, 'TotalWidth',           '0', .true.)
      call AddOrReplaceParameter(category, 'Volume',               '0', .true.)
      call AddOrReplaceParameter(category, 'VolumeError',          '0', .true.)
      call AddOrReplaceParameter(category, 'VolumesOnStreet',      '0', .true.)
      call AddOrReplaceParameter(category, 'WaterDepth',           '0', .true.)
      call AddOrReplaceParameter(category, 'WaterLevel',           '1', .true.)
      call AddOrReplaceParameter(category, 'WaterOnStreet',        '0', .true.)

      ! Parameters for Salt
      call AddOrReplaceParameter(category, 'Density',  '0', .true.)
      call AddOrReplaceParameter(category, 'Salinity', '0', .true.)
      
      ! Parameters for Temperature Model
      call AddOrReplaceParameter(category, 'EffectiveBackRad',   '0', .true.)
      call AddOrReplaceParameter(category, 'HeatLossConv',       '0', .true.)
      call AddOrReplaceParameter(category, 'HeatLossEvap',       '0', .true.)
      call AddOrReplaceParameter(category, 'HeatLossForcedConv', '0', .true.)
      call AddOrReplaceParameter(category, 'HeatLossForcedEvap', '0', .true.)
      call AddOrReplaceParameter(category, 'HeatLossFreeConv',   '0', .true.)
      call AddOrReplaceParameter(category, 'HeatLossFreeEvap',   '0', .true.)
      call AddOrReplaceParameter(category, 'NetSolarRad',        '0', .true.)
      call AddOrReplaceParameter(category, 'RadFluxClearSky',    '0', .true.)
      call AddOrReplaceParameter(category, 'TotalHeatFlux',      '0', .true.)

      ! Parameters for Morphology
      call AddOrReplaceParameter(category, 'AdaptedCrossSec',        '0', .true.)
      call AddOrReplaceParameter(category, 'BedLevel',               '0', .true.)
      call AddOrReplaceParameter(category, 'CumIncreaseCrossSec',    '0', .true.)
      call AddOrReplaceParameter(category, 'GrainSizeD50',           '0', .true.)
      call AddOrReplaceParameter(category, 'GrainSizeD90',           '0', .true.)
      call AddOrReplaceParameter(category, 'IncreaseCrossSec',       '0', .true.)
      call AddOrReplaceParameter(category, 'IntegrSedTransp',        '0', .true.)
      call AddOrReplaceParameter(category, 'MeanBedLevelMain',       '0', .true.)
      call AddOrReplaceParameter(category, 'MorDepth',               '0', .true.)
      call AddOrReplaceParameter(category, 'MorVelocity',            '0', .true.)
      call AddOrReplaceParameter(category, 'MorWaterLevel',          '0', .true.)
      call AddOrReplaceParameter(category, 'MorWidth',               '0', .true.)
      call AddOrReplaceParameter(category, 'SedimentTransport',      '0', .true.)
      call AddOrReplaceParameter(category, 'SedimentTransportLeft',  '0', .true.)
      call AddOrReplaceParameter(category, 'SedimentTransportRight', '0', .true.)
      call AddOrReplaceParameter(category, 'ShieldsParameter',       '0', .true.)
      
      category = 'ResultsBranches'
      call AddOrReplaceParameter(category, 'AreaFP1',              '0', .true.)
      call AddOrReplaceParameter(category, 'AreaFP2',              '0', .true.)
      call AddOrReplaceParameter(category, 'AreaMain',             '0', .true.)
      call AddOrReplaceParameter(category, 'ChezyFP1',             '0', .true.)
      call AddOrReplaceParameter(category, 'ChezyFP2',             '0', .true.)
      call AddOrReplaceParameter(category, 'ChezyMain',            '0', .true.)
      call AddOrReplaceParameter(category, 'Discharge',            '1', .true.)
      call AddOrReplaceParameter(category, 'DischargeFP1',         '0', .true.)
      call AddOrReplaceParameter(category, 'DischargeFP2',         '0', .true.)
      call AddOrReplaceParameter(category, 'DischargeMain',        '0', .true.)
      call AddOrReplaceParameter(category, 'EnergyLevels',         '0', .true.)
      call AddOrReplaceParameter(category, 'FlowArea',             '0', .true.)
      call AddOrReplaceParameter(category, 'FlowChezy',            '0', .true.)
      call AddOrReplaceParameter(category, 'FlowConv',             '0', .true.)
      call AddOrReplaceParameter(category, 'FlowHydrad',           '0', .true.)
      call AddOrReplaceParameter(category, 'Froude',               '0', .true.)
      call AddOrReplaceParameter(category, 'HydradFP1',            '0', .true.)
      call AddOrReplaceParameter(category, 'HydradFP2',            '0', .true.)
      call AddOrReplaceParameter(category, 'HydradMain',           '0', .true.)
      call AddOrReplaceParameter(category, 'MomAcceleration',      '0', .true.)
      call AddOrReplaceParameter(category, 'MomAdvection',         '0', .true.)
      call AddOrReplaceParameter(category, 'MomBedStress',         '0', .true.)
      call AddOrReplaceParameter(category, 'MomLateralCorrection', '0', .true.)
      call AddOrReplaceParameter(category, 'MomLosses',            '0', .true.)
      call AddOrReplaceParameter(category, 'MomPressure',          '0', .true.)
      call AddOrReplaceParameter(category, 'MomWindStress',        '0', .true.)
      call AddOrReplaceParameter(category, 'TimeStepEstimation',   '0', .true.)
      call AddOrReplaceParameter(category, 'Velocity',             '0', .true.)
      call AddOrReplaceParameter(category, 'WaterLevelGradient',   '0', .true.)
      call AddOrReplaceParameter(category, 'WidthFp1',             '0', .true.)
      call AddOrReplaceParameter(category, 'WidthFp2',             '0', .true.)
      call AddOrReplaceParameter(category, 'WidthMain',            '0', .true.)

      category = 'ResultsStructures'
      call AddOrReplaceParameter(category, 'CrestLevel',         '0', .true.)
      call AddOrReplaceParameter(category, 'CrestWidth',         '0', .true.)
      call AddOrReplaceParameter(category, 'Discharge',          '1', .true.)
      call AddOrReplaceParameter(category, 'GateLowerEdgeLevel', '0', .true.)
      call AddOrReplaceParameter(category, 'GateOpeningHeight',  '0', .true.)
      call AddOrReplaceParameter(category, 'Head',               '0', .true.)
      call AddOrReplaceParameter(category, 'OpeningsArea',       '0', .true.)
      call AddOrReplaceParameter(category, 'PressureDifference', '0', .true.)
      call AddOrReplaceParameter(category, 'State',              '0', .true.)
      call AddOrReplaceParameter(category, 'Velocity',           '0', .true.)
      call AddOrReplaceParameter(category, 'WaterLevel',         '0', .true.)
      call AddOrReplaceParameter(category, 'WaterlevelAtCrest',  '0', .true.)
      call AddOrReplaceParameter(category, 'WaterlevelDown',     '0', .true.)
      call AddOrReplaceParameter(category, 'WaterlevelUp',       '0', .true.)

      category = 'ResultsPumps'
      call AddOrReplaceParameter(category, 'SuctionSideLevel',  '0', .true.)
      call AddOrReplaceParameter(category, 'DeliverySideLevel', '0', .true.)
      call AddOrReplaceParameter(category, 'PumpHead',          '0', .true.)
      call AddOrReplaceParameter(category, 'ActualPumpStage',   '0', .true.)
      call AddOrReplaceParameter(category, 'PumpCapacity',      '0', .true.)
      call AddOrReplaceParameter(category, 'ReductionFactor',   '0', .true.)
      call AddOrReplaceParameter(category, 'PumpDischarge',     '0', .true.)

      category = 'ResultsLaterals'
      call AddOrReplaceParameter(category, 'ActualDischarge',   '0', .true.)
      call AddOrReplaceParameter(category, 'DefinedDischarge' , '0', .true.)
      call AddOrReplaceParameter(category, 'LateralDifference', '0', .true.)
      call AddOrReplaceParameter(category, 'WaterLevel',        '0', .true.)

      category = 'ResultsWaterBalance'
      call AddOrReplaceParameter(category, '1d2dflows', 'none', .true.)

      category = 'ResultsGeneral'
      call AddOrReplaceParameter(category, 'ActualValue', '1', .true.)
      call AddOrReplaceParameter(category, 'DelwaqNoStaggeredGrid', '0', .true.)
      call AddOrReplaceParameter(category, 'FlowAnalysisTimeSeries', '0', .true.)
      call AddOrReplaceParameter(category, 'MeanValue', '0', .true.)
      call AddOrReplaceParameter(category, 'MaximumValue', '0', .true.)
      call AddOrReplaceParameter(category, 'SobeksimStamp', '1', .true.)

      category = 'Sediment'
      call AddOrReplaceParameter(category, 'D50', '0.0005', .true.)
      call AddOrReplaceParameter(category, 'D90', '0.001', .true.)
      call AddOrReplaceParameter(category, 'DepthUsedForSediment', '0.3', .true.)

      category = 'Specials'
      call AddOrReplaceParameter(category, 'DesignFactorDLG', '1.0', .true.)

      category = 'Indication'
      call AddOrReplaceParameter(category, 'VelocityReachSegments', '0.5', .true.)
      call AddOrReplaceParameter(category, 'VelocityStructures', '0.75', .true.)

      category = 'NumericalParameters'
      call AddOrReplaceParameter(category, 'AccelerationTermFactor', '1.0', .true.)
      call AddOrReplaceParameter(category, 'AccurateVersusSpeed', '3', .true.)
      call AddOrReplaceParameter(category, 'CourantNumber', '1.0', .true.)
      call AddOrReplaceParameter(category, 'DtMinimum', '0.001', .true.)
      call AddOrReplaceParameter(category, 'EpsilonValueVolume', '0.0001', .true.)
      call AddOrReplaceParameter(category, 'EpsilonValueWaterDepth', '0.0001', .true.)
      call AddOrReplaceParameter(category, 'FloodingDividedByDrying', '10.0', .true.)
      call AddOrReplaceParameter(category, 'Gravity', '9.81', .true.)
      call AddOrReplaceParameter(category, 'MaxDegree', '6', .true.)
      call AddOrReplaceParameter(category, 'MaxIterations', '8', .true.)
      call AddOrReplaceParameter(category, 'MaxTimeStep', '0', .true.)  ! TODO: Default must be corrected when Time Chapter is available
      call AddOrReplaceParameter(category, 'MinimumSurfaceatStreet', '0.1', .true.)
      call AddOrReplaceParameter(category, 'MinimumSurfaceinNode', '0.1', .true.)
      call AddOrReplaceParameter(category, 'MinimumLength', '1.0', .true.)
      call AddOrReplaceParameter(category, 'RelaxationFactor', '1.0', .true.)
      call AddOrReplaceParameter(category, 'Rho', '1000', .true.)
      call AddOrReplaceParameter(category, 'StructureInertiaDampingFactor', '1.0', .true.)
      call AddOrReplaceParameter(category, 'Theta', '1.0', .true.)
      call AddOrReplaceParameter(category, 'ThresholdValueFlooding', '0.01', .true.)
      call AddOrReplaceParameter(category, 'UseOmp', '0', .true.)
      call AddOrReplaceParameter(category, 'UseTimeStepReducerStructures', '0', .true.)

      category = 'SimulationOptions'
      call AddOrReplaceParameter(category, 'allowablelargertimestep', '0', .true.)
      call AddOrReplaceParameter(category, 'allowabletimesteplimiter', '30', .true.)
      call AddOrReplaceParameter(category, 'AllowableVolumeError', '10.0', .true.)
      call AddOrReplaceParameter(category, 'AllowCrestLevelBelowBottom', '0', .true.)
      call AddOrReplaceParameter(category, 'Cflcheckalllinks', '0', .true.)
      call AddOrReplaceParameter(category, 'Channel', '1', .true.)
      call AddOrReplaceParameter(category, 'Debug', '0', .true.)
      call AddOrReplaceParameter(category, 'DebugTime', '0', .true.)
      call AddOrReplaceParameter(category, 'DepthsBelowBobs', '0', .true.)
      call AddOrReplaceParameter(category, 'DumpInput', '0', .true.)
      call AddOrReplaceParameter(category, 'Iadvec1D', '2', .true.)
      call AddOrReplaceParameter(category, 'Jchecknans', '0', .true.)
      call AddOrReplaceParameter(category, 'LaboratoryTest', '0', .true.)
      call AddOrReplaceParameter(category, 'LaboratoryTimeStep', '1', .true.)
      call AddOrReplaceParameter(category, 'LaboratoryTotalStep', '1', .true.)
      call AddOrReplaceParameter(category, 'Limtyphu1D', '1', .true.)
      call AddOrReplaceParameter(category, 'LoggingLevel', 'info', .true.)
      call AddOrReplaceParameter(category, 'Manhloss', '0', .true.)
      call AddOrReplaceParameter(category, 'ManholeLosses', '0', .true.)
      call AddOrReplaceParameter(category, 'MissingValue', '-999.999', .true.)
      call AddOrReplaceParameter(category, 'Morphology', '0', .true.)
      call AddOrReplaceParameter(category, 'NoSuperCriticalInflow', '0', .true.)
      call AddOrReplaceParameter(category, 'PercentAllowableolumeError', '1.0', .true.)
      call AddOrReplaceParameter(category, 'PreissmannMinClosedManholes', '0.001', .true.)
      call AddOrReplaceParameter(category, 'ReadNetworkFromUgrid', '0', .true.)
      call AddOrReplaceParameter(category, 'River', '1', .true.)
      call AddOrReplaceParameter(category, 'Sewer', '0', .true.)
      call AddOrReplaceParameter(category, 'SiphonUpstreamThresholdSwitchOff', '0.1', .true.)
      call AddOrReplaceParameter(category, 'StrucAlfa', '0.9', .true.)
      call AddOrReplaceParameter(category, 'StrucFlowDirectionAccuracyFactor', '1.0', .true.)
      call AddOrReplaceParameter(category, 'StructureDynamicsFactor', '1.0', .true.)
      call AddOrReplaceParameter(category, 'StructureStabilityFactor', '0', .true.)
      call AddOrReplaceParameter(category, 'ThresholdForSummerDike', '0.4', .true.)
      call AddOrReplaceParameter(category, 'TimersOutputFrequency', '0', .true.)
      call AddOrReplaceParameter(category, 'use1d2dcoupling', '0', .true.)
      call AddOrReplaceParameter(category, 'UseEnergyHeadStructures', '0', .true.)
      call AddOrReplaceParameter(category, 'UseTimers', '0', .true.)
      call AddOrReplaceParameter(category, 'Usevariableteta', '0', .true.)
      call AddOrReplaceParameter(category, 'UseWlevStateFile', '0', .true.)
      call AddOrReplaceParameter(category, 'VolumeCheck', '0', .true.)
      call AddOrReplaceParameter(category, 'VolumeCorrection', '0', .true.)
      call AddOrReplaceParameter(category, 'WaterQualityInUse', '0', .true.)
      call AddOrReplaceParameter(category, 'WriteNetCDF', '0', .true.)

      category = 'AdvancedOptions'
      call AddOrReplaceParameter(category, 'CacheMode', 'none', .true.)
      call AddOrReplaceParameter(category, 'CalculateDelwaqOutput', '0', .true.)
      call AddOrReplaceParameter(category, 'ExtraResistanceGeneralStructure', '0.0', .true.)
      call AddOrReplaceParameter(category, 'LateralLocation', '0', .true.)
      call AddOrReplaceParameter(category, 'Latitude', '52.00667', .true.)
      call AddOrReplaceParameter(category, 'Longitude', '4.35556', .true.)
      call AddOrReplaceParameter(category, 'FillCulvertsWithGL', '0', .true.)
      call AddOrReplaceParameter(category, 'MaxLoweringCrossAtCulvert', '0.0', .true.)
      call AddOrReplaceParameter(category, 'MaxVolFact', '0.9', .true.)
      call AddOrReplaceParameter(category, 'NoNegativeQlatWhenThereIsNoWater', '1', .true.)
      call AddOrReplaceParameter(category, 'TransitionHeightSD', '1.0', .true.)

      category = 'Salinity'
      call AddOrReplaceParameter(category, 'SaltComputation', '0', .true.)
      call AddOrReplaceParameter(category, 'DiffusionAtBoundaries', '0', .true.)

      category = 'NumericalOptions'
      call AddOrReplaceParameter(category, 'teta', '1.0', .true.)
      call AddOrReplaceParameter(category, 'tidalPeriod', '12.417', .true.)
      call AddOrReplaceParameter(category, 'maxMouthsPerBranch', '1', .true.)
      call AddOrReplaceParameter(category, 'advectionScheme', 'vanLeer-2', .true.)

      category = 'TransportComputation'
      call AddOrReplaceParameter(category, 'Temperature', 'false', .true.)
      call AddOrReplaceParameter(category, 'Density', 'eckart_modified', .true.)
      call AddOrReplaceParameter(category, 'HeatTransferModel', 'transport', .true.)

      category = 'Temperature'
      call AddOrReplaceParameter(category, 'SurfaceArea', '1.0d6', .true.)
      call AddOrReplaceParameter(category, 'AtmosphericPressure', '1.0d5', .true.)
      call AddOrReplaceParameter(category, 'DaltonNumber', '0.0013', .true.)
      call AddOrReplaceParameter(category, 'HeatCapacityWater', '3930', .true.)
      call AddOrReplaceParameter(category, 'StantonNumber', '0.0013', .true.)

      category = 'Observations'
      call AddOrReplaceParameter(category, 'interpolationType', 'OBS_NEAREST', .true.)
   
   end subroutine setModelParameterDefaults

   
   double precision function julian(iyear, imonth, iday, ihour, imin, isec)

      !***********************************************************************
      !
      !     Description of module :
      !
      !        This functions returns the so called Julian day of a date, or
      !        the value -1.0 if an error occurred.
      !
      !        The Julian day of a date is the number of days that has passed
      !        since January 1, 4712 BC at 12h00 ( Gregorian). It is usefull
      !        to compute differces between dates. ( See SUBROUTINE GREGOR
      !        for the reverse proces ).
      !
      !***********************************************************************
      !
      !     Name   Type            Description
      !     ------ -----           ---------------------------
      !     IYEAR  integer         Year   ( -4713-.. )
      !     IMONTH integer         Month  ( 1-12 )
      !     IDAY   integer         Day    ( 1-28,29,30 or 31 )
      !     IHOUR  integer         Hour   ( 0-23 )
      !     IMIN   integer         Minute ( 0-59 )
      !     ISEC   integer         Second ( 0-59 )

      !     Name   Type     Size   Description
      !     ------ -----    ------ ------------------------
      !     TEMP1  real*8   -      Temporary variable
      !     TEMP2  real*8   -      Temporary variable
      !     MONLEN integer  12     Length of month in days
      !
      !     Calls to : none
      !
      !***********************************************************************
      !
      !     Variables :
      !
      implicit none

      integer, intent(in)                 :: iyear
      integer, intent(in)                 :: imonth
      integer, intent(in)                 :: iday
      integer, intent(in)                 :: ihour
      integer, intent(in)                 :: imin
      integer, intent(in)                 :: isec
      integer, dimension(12)              :: monlen
      double precision                    :: temp1
      double precision                    :: temp2

      ! Initialize lenghts of months :
      data monlen / 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /

      if (( iyear  .lt. -4713 ) .or. ( imonth .lt.  1 ) .or.       &
            ( imonth .gt.    12 ) .or. ( iday   .lt.  1 ) .or.     &
            ( iday   .gt. monlen(imonth) ) .or.                    &
            ( ihour  .lt.     0 ) .or. ( ihour  .gt. 23 ) .or.     &
            ( imin   .lt.     0 ) .or. ( imin   .gt. 59 ) .or.     &
            ( isec   .lt.     0 ) .or. ( isec   .gt. 60 )) then
         julian = -1.0d0
      else
         temp1  = int ((imonth - 14.0) / 12.0)
         temp2  = iday - 32075.0 + &
                  int ( 1461.0 * ( iyear + 4800.0 + temp1 ) / 4.0 ) + &
                  int ( 367.0 * ( imonth - 2.0 - temp1 * 12.0 ) / 12.0 ) - &
                  int ( 3.0 * int ( ( iyear + 4900.0 + temp1 ) / 100.0 ) / &
                  4.0 )
         temp1  = float ( ihour ) * 3600.0 + &
                  float ( imin  ) *   60.0 + &
                  float ( isec  ) - 43200.0
         julian = temp2 + ( temp1 / 86400.0 )
      endif

   end function julian
   
   
end module m_readModelParameters
