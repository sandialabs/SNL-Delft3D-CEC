!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.            
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     

!  $Id: ec_provider.F90 5646 2015-12-11 16:25:12Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/ec_module/packages/ec_module/src/ec_provider.F90 $

!> This module instantiates FileReaders and their source Items, just as a GUI/framework instantiates kernels and their target Items.
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.bos@deltares.nl
module m_ec_provider
   use m_ec_typedefs
   use m_ec_parameters
   use m_ec_support
   use m_ec_elementSet
   use m_ec_field
   use m_ec_item
   use m_ec_connection
   use m_ec_converter
   use m_ec_filereader
   use m_ec_filereader_read
   use m_ec_stringbuffer
   use m_ec_quantity
   use m_ec_bcreader
   use time_module
   use m_ec_instance
   use m_ec_message
   use m_ec_parameters
   use precision
   use string_module
   use netcdf
   use multi_file_io
   
   implicit none
   
   private
   
   public :: ecSetFileReaderProperties
   public :: ecInstanceCreateUniformItems
   public :: ecProviderCreateTimeInterpolatedItem
   public :: ecProviderInitializeTimeFrame
   public :: items_from_bc_quantities

   
   interface ecSetFileReaderProperties
      module procedure ecProviderInitializeFileReader
   end interface ecSetFileReaderProperties

   interface ecInstanceCreateUniformItems
      module procedure ecProviderCreateUniformItems
   end interface ecInstanceCreateUniformItems

   public :: ecAtLeastOnePointIsCorrection            ! TODO: Refactor this shortcut (UNST-180).
   logical :: ecAtLeastOnePointIsCorrection = .false. ! TODO: Refactor this shortcut (UNST-180).

   contains
      
      ! =======================================================================
      
      !> Initialize a new BCBlock item, which in turn constructs and initializes a filereader 

      function ecProviderInitializeBCBlock(instancePtr, bcBlockId, k_refdat, k_tzone, k_tsunit, fileReaderId, fileName, quantityName, plilabel, dtnodal, istat) result(success)
      use m_ec_filereader_read
      use m_ec_netcdf_timeseries
         logical                             :: success      !< function status
         type(tEcInstance),      pointer     :: instancePtr  !< intent(in)
         integer,                intent(in)  :: bcBlockId    !< unique bcBlock id
         integer,                intent(in)  :: k_refdat     !< kernel ref date 
         real(hp),               intent(in)  :: k_tzone      !< kernel time zone 
         integer,                intent(in)  :: k_tsunit     !< kernel timestep unit (1=sec, 2=min, 3=hour)
         integer,                intent(out) :: fileReaderId !< unique fileReader id
         character(*),           intent(in)  :: fileName     !< relative path of data file
         character(*),           intent(in)  :: quantityName !< name of quantity, needed for structured input files (NetCDF and BC)
         character(*),           intent(in)  :: plilabel     !< identify a (set of) pli-points
         real(hp), optional,     intent(in)  :: dtnodal      !< Nodal factors in astronomical bc update interval
         integer,                intent(out) :: istat        !< Detailed result status. \see{m_ec_parameters}.

         type(tEcBCBlock),    pointer :: bcBlockPtr     !< BCBlock corresponding to bcBlockId
         type(tEcFileReader), pointer :: fileReaderPtr  !< FileReader associated with the BC instance 
         integer(kind=8)              :: filehandle 
         integer                      :: iostat, istat_
         integer                      :: unit
         real(hp)                     :: ref_date

         integer                      :: netCDFId

         success = .false.
         istat = EC_UNKNOWN_ERROR

         bcBlockPtr => ecSupportFindBCBlock(instancePtr, bcBlockId)
         if (.not.associated(bcBlockPtr)) return
             
         if (index(trim(fileName)//'|','.bc')>0) then                               ! ASCII: bc-format  : detection is extension-based
            bcBlockPtr%ftype=BC_FTYPE_ASCII
         endif
         if (index(trim(fileName)//'|','.nc')>0) then                               ! NETCDF: nc-format 
            !if (index(plilabel,'_')<=0) then 
            !   return                                                               ! If this was not pli-label  bla_0001 then its is a qhbnd
            !endif                                                                   ! not supported in combination with netcdf-files 
                                                                                    ! This is something dirty, which deserves refactoring
                                                                                    ! but no alternative for it as we speak 
            bcBlockPtr%ncptr => ecSupportFindNetCDFByFilename(instancePtr, fileName)! is there a netCDF instance with this file ?
            if (.not.associated(bcBlockPtr%ncptr)) then                             ! if not ...
                netCDFId = ecInstanceCreateNetCDF(instancePtr)                      !   ... create a new instance 
                bcBlockPtr%ncptr => ecSupportFindNetCDF(instancePtr,netCDFId)
                if (.not.ecNetCDFInit(fileName, bcBlockPtr%ncptr, iostat)) then     ! initialise 
                   return                                                           ! optionally add a message to the EC-error stack 
                endif 
            endif
            bcBlockPtr%ftype=BC_FTYPE_NETCDF
         endif 
         if (.not.ecBCInit (instancePtr, filename, quantityName, plilabel, bcBlockPtr, iostat)) return

         ! Every BC block (instance) needs an associated filereader referring to it  
         fileReaderId = ecInstanceCreateFileReader(instancePtr)    
         fileReaderPtr => ecSupportFindFileReader(instancePtr, fileReaderId)
         if (.not.associated(fileReaderPtr)) return
         
         fileReaderPtr%bc => bcBlockPtr
         fileReaderPtr%filename = fileName
         fileReaderPtr%ofType = provFile_bc

         if(present(dtnodal)) then
            if (.not. ecProviderInitializeTimeFrame(fileReaderPtr, k_refdat, k_tzone, k_tsunit, dtnodal)) return
         else
            if (.not. ecProviderInitializeTimeFrame(fileReaderPtr, k_refdat, k_tzone, k_tsunit)) return
         end if

         success = .false.
         select case(fileReaderPtr%bc%func)
            case (BC_FUNC_TSERIES)
               success = ecProviderCreateUniformItems(instancePtr, fileReaderPtr)
            case (BC_FUNC_HARMONIC, BC_FUNC_ASTRO)
               success = ecProviderCreateFourierItems(instancePtr, fileReaderPtr)
            case (BC_FUNC_HARMOCORR, BC_FUNC_ASTROCORR) 
               success = ecApplyCorrectionToCmp(instancePtr, fileReaderPtr)
            case (BC_FUNC_QHTABLE)
               success = ecProviderCreateQhtableItems(instancePtr, fileReaderPtr)
            case (BC_FUNC_TIM3D)
               success = ecProviderCreatet3DItems(instancePtr, fileReaderPtr)
            case default
               call setECMessage("ERROR: unknown function type.")
         end select
         if (.not.success) then
            return
         else 
            success = .True. 
         endif 
      end function ecProviderInitializeBCBlock
      ! =======================================================================
      
      !> Initialize a new FileReader, by constructing the complete tree of source Items.
      !! On the opposite end of the EC-module is a kernel, which constructs the complete tree of target Items.
      recursive function ecProviderInitializeFileReader(instancePtr, fileReaderId, fileType, fileName, refdat, tzone, tsunit, quantityName, forcingFile, bcBlockId, dtnodal) result(success)
         logical                                :: success      !< function status
         type(tEcInstance),          pointer    :: instancePtr  !< intent(in)
         integer,                    intent(in) :: fileReaderId !< unique FileReader id
         integer,                    intent(in) :: fileType     !< type of data file, see provFile enumeration
         character(len=*),           intent(in) :: fileName     !< relative path of data file
         integer,                    intent(in) :: refdat       !< Kernel's reference date, format: Gregorian yyyymmdd
         real(kind=hp),              intent(in) :: tzone        !< Kernel's timezone.
         integer,                    intent(in) :: tsunit       !< Kernel's timestep unit (1=sec 2=min 3=sec).
         character(len=*), optional, intent(in) :: quantityName !< name of quantity, needed for structured input files (NetCDF and BC)
         character(len=*), optional, intent(in) :: forcingFile  !< name of quantity, needed for structured input files (NetCDF and BC)
         integer,          optional, intent(in) :: bcBlockId    !< if this filereader needs to be connected to a BC header block
         real(kind=hp),    optional, intent(in) :: dtnodal      !< Nodal factors update interval
         !
         type(tEcFileReader), pointer :: fileReaderPtr  !< FileReader corresponding to fileReaderId
         character(maxFileNameLen)    :: fName          !< relative path of data file, converted to the correct length
         integer                      :: i              !< loop counter
         character(maxNameLen)        :: l_quantityName !< explicit length version of quantityName
         integer                      :: iostat         !< status returned from various file operations 
         !
         success = .false.
         fileReaderPtr => null()
         fName = ''
         l_quantityName = ''
         !
         if (len_trim(fileName) > maxFileNameLen) then
            call setECMessage("ERROR: ec_provider::ecProviderInitializeFileReader: The filename string is too long.")
            return
         end if
         !
         fName = fileName
         fileReaderPtr => ecSupportFindFileReader(instancePtr, fileReaderId)         
         if (associated(fileReaderPtr)) then
            fileReaderPtr%ofType = fileType
            fileReaderPtr%fileName = fName
            fileReaderPtr%fileHandle = -1                      ! The filereader itself has now an invalid filehandle 

            if (.not. ecSupportOpenExistingFile(fileReaderPtr%fileHandle, fileReaderPtr%fileName)) return
            if(present(dtnodal)) then
               if (.not. ecProviderInitializeTimeFrame(fileReaderPtr, refdat, tzone, tsunit, dtnodal)) return
            else
               if (.not. ecProviderInitializeTimeFrame(fileReaderPtr, refdat, tzone, tsunit)) return
            end if

            fileReaderPtr%nItems = 0

            ! Create source Items and their contained types, based on file type and file header.
            if (present(quantityName)) then
               l_quantityName = quantityName
               if (.not. ecProviderCreateItems(instancePtr, fileReaderPtr, forcingFile, l_quantityName)) return 
            else
               if (.not. ecProviderCreateItems(instancePtr, fileReaderPtr)) return 
            end if
         end if
         success = .true.

      end function ecProviderInitializeFileReader
      
      ! =======================================================================
      
      !> Create source Items and their contained types, based on file type and file header.
      function ecProviderCreateItems(instancePtr, fileReaderPtr, bctfilename, quantityname) result(success)
         logical                         :: success          !< function status
         type(tEcInstance),     pointer  :: instancePtr      !< intent(in)
         type(tEcFileReader),   pointer  :: fileReaderPtr    !< intent(inout)
         character(maxNameLen), optional :: quantityname     !< Names of the quantities read from file, needed for structured files (NetCDF),
                                                             !< but also for bct-file 
         character(maxNameLen), optional :: bctfilename      !< file name of bct-file with data
         integer                         :: iitem
         !
         success = .false.
         select case(fileReaderPtr%ofType)
            case (provFile_undefined)
               call setECMessage("ERROR: ec_provider::ecProviderCreateItems: Unsupported file type.")
            case (provFile_uniform, provFile_unimagdir)
               success = ecProviderCreateUniformItems(instancePtr, fileReaderPtr)
            case (provFile_svwp)
               call setECMessage("ERROR: ec_provider::ecProviderCreateItems: Unsupported file type.")
            case (provFile_svwp_weight)
               call setECMessage("ERROR: ec_provider::ecProviderCreateItems: Unsupported file type.")
            case (provFile_arcinfo)
               success = ecProviderCreateArcinfoItems(instancePtr, fileReaderPtr)
            case (provFile_spiderweb)
               success = ecProviderCreateSpiderwebItems(instancePtr, fileReaderPtr)
            case (provFile_curvi)
               success = ecProviderCreateCurviItems(instancePtr, fileReaderPtr)
            case (provFile_curvi_weight)
               call setECMessage("ERROR: ec_provider::ecProviderCreateItems: Unsupported file type.")
            case (provFile_samples)
               success = ecProviderCreateSampleItems(instancePtr, fileReaderPtr)
            case (provFile_triangulationmagdir)
               call setECMessage("ERROR: ec_provider::ecProviderCreateItems: Unsupported file type.")
            case (provFile_qhtable)
               success = ecProviderCreateQhtableItems(instancePtr, fileReaderPtr)
            case (provFile_poly_tim)
                  if (present(bctfilename)) then 
                     if (.not.present(quantityname)) then 
                        call setECMessage("ERROR: ec_provider::ecProviderCreateItems: BC type, but no quantity name.")
                        return
                     endif 
                     success = ecProviderCreatePolyTimItemsBC(instancePtr, fileReaderPtr, bctfilename, quantityname)
                  else
                     success = ecProviderCreatePolyTimItems(instancePtr, fileReaderPtr)
                  endif 
            case (provFile_fourier)
               success = ecProviderCreateFourierItems(instancePtr, fileReaderPtr)
            case (provFile_grib)
               call setECMessage("ERROR: ec_provider::ecProviderCreateItems: Unsupported file type.")
            case (provFile_netcdf)
               if (present(quantityname)) then
                  select case(quantityname)
                     case ("rainfall","airpressure_windx_windy","windxy","windx","windy","atmosphericpressure")
                        success = ecProviderCreateNetcdfItems(instancePtr, fileReaderPtr, quantityname)
                     case default
                        success = ecProviderCreateWaveNetcdfItems(instancePtr, fileReaderPtr, quantityname)
                  end select
               else
                  call setECMessage("ERROR: ec_provider::ecProviderCreateItems: NetCDF requires a quantity name.")
               end if
            case (provFile_t3D)
               success = ecProviderCreatet3DItems(instancePtr, fileReaderPtr)
            case default
               call setECMessage("ERROR: ec_provider::ecProviderCreateItems: Unknown file type.")
         end select
      end function ecProviderCreateItems
      
      ! =======================================================================

      !> Create source Items and their contained types, from a qh-table file.
      function ecProviderCreateQhtableItems(instancePtr, fileReaderPtr) result(success)
         logical                      :: success       !< function status
         type(tEcInstance),   pointer :: instancePtr   !< intent(in)
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
         !
         real(hp), dimension(:), allocatable :: discharges      !< the table's discharge values
         real(hp), dimension(:), allocatable :: waterlevels     !< the table's water level values
         integer                             :: nr_rows         !< the number of rows in the table
         integer                             :: itemId          !< helper variable 
         integer                             :: quantityId      !< helper variable 
         integer                             :: elementSetId    !< helper variable 
         integer                             :: field0Id        !< helper variable 
         integer                             :: field1Id        !< helper variable
         type(tEcItem),              pointer :: item_discharge  !< Item
         type(tEcItem),              pointer :: item_waterlevel !< Item
         type(tEcItem),              pointer :: item_slope      !< Item
         type(tEcItem),              pointer :: item_crossing   !< Item
         integer :: i !< loop counter
         integer :: n1, n2 !< helper variables
         !
         success = .true.
         item_discharge  => null()
         item_waterlevel  => null()
         item_slope  => null()
         item_crossing  => null()
         n1 = 12345 ! arbitrary large number
         !
         ! Determine the number of rows and read the data.
         success = ecQhtableReadAll(fileReaderPtr, discharges, waterlevels, nr_rows)
         if (.not. success) return
         ! Create the item 'discharge'.
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. (ecQuantitySetName(instancePtr, quantityId, 'discharge'))) then
            success = .false.
         end if
         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian) .and. &
                    ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nr_rows))) then
            success = .false.
         end if
         field0Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, nr_rows))) then
            success = .false.
         end if
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, nr_rows))) then
            success = .false.
         end if
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                    ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                    ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                    ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                    ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                    ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
            success = .false.
         else
            item_discharge => ecSupportFindItem(instancePtr, itemId)
         end if
         ! Create the item 'waterlevel'.
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. (ecQuantitySetName(instancePtr, quantityId, 'waterlevel'))) then
            success = .false.
         end if
         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian) .and. &
                    ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nr_rows))) then
            success = .false.
         end if
         field0Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, nr_rows))) then
            success = .false.
         end if
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, nr_rows))) then
            success = .false.
         end if
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                    ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                    ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                    ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                    ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                    ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
            success = .false.
         else
            item_waterlevel => ecSupportFindItem(instancePtr, itemId)
         end if
         ! Create the item 'slope'.
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. (ecQuantitySetName(instancePtr, quantityId, 'slope'))) then
            success = .false.
         end if
         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian) .and. &
                    ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nr_rows))) then
            success = .false.
         end if
         field0Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, nr_rows))) then
            success = .false.
         end if
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, nr_rows))) then
            success = .false.
         end if
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                    ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                    ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                    ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                    ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                    ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
            success = .false.
         else
            item_slope => ecSupportFindItem(instancePtr, itemId)
         end if
         ! Create the item 'crossing'.
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. (ecQuantitySetName(instancePtr, quantityId, 'crossing'))) then
            success = .false.
         end if
         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian) .and. &
                    ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nr_rows))) then
            success = .false.
         end if
         field0Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, nr_rows))) then
            success = .false.
         end if
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, nr_rows))) then
            success = .false.
         end if
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                    ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                    ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                    ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                    ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                    ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
            success = .false.
         else
            item_crossing => ecSupportFindItem(instancePtr, itemId)
         end if
         ! ===== finish initialization of Fields =====
         ! This FileReader has constant values, so we store them in T0.
         ! The values in T1 are never used, so to comply with the uniform EC-module mechanism, it is set to a large, increasable value.
         ! Rewind the file
         if (success) then
            item_slope%sourceT0FieldPtr%arr1dPtr = 0.0_hp
            item_crossing%sourceT0FieldPtr%arr1dPtr = 0.0_hp
            do i=1, nr_rows
               ! initialize Field T0
               item_discharge%sourceT0FieldPtr%arr1dPtr(i) = discharges(i)
               item_waterlevel%sourceT0FieldPtr%arr1dPtr(i) = waterlevels(i)
               n2 = i-1 
               if (i > 1) then
                  if (discharges(i) <= discharges(n2)) then
                     call setECMessage("ERROR: ec_provider::ecProviderCreateQhtableItems: First column should be ordered increasingly.")
                     success = .false.
                     exit
                  end if
                  item_slope%sourceT0FieldPtr%arr1dPtr(n2) = (waterlevels(i)-waterlevels(n2))/(discharges(i)-discharges(n2))
                  item_crossing%sourceT0FieldPtr%arr1dPtr(n2) = waterlevels(n2) - item_slope%sourceT0FieldPtr%arr1dPtr(n2) * discharges(n2)
               end if
            end do
            item_discharge%sourceT0FieldPtr%timesteps = 0.0_hp
            item_waterlevel%sourceT0FieldPtr%timesteps = 0.0_hp
            item_slope%sourceT0FieldPtr%timesteps = 0.0_hp
            item_crossing%sourceT0FieldPtr%timesteps = 0.0_hp
            ! initialize Field T1 (arbitrary far future)
            item_discharge%sourceT1FieldPtr%timesteps = 10000.0_hp
            item_waterlevel%sourceT1FieldPtr%timesteps = 10000.0_hp
            item_slope%sourceT1FieldPtr%timesteps = 10000.0_hp
            item_crossing%sourceT1FieldPtr%timesteps = 10000.0_hp
         end if
         ! Add successfully created source Items to the FileReader
         if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item_discharge%id)
         if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item_waterlevel%id)
         if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item_slope%id)
         if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item_crossing%id)
         ! finalize
         if (allocated(discharges)) deallocate(discharges)
         if (allocated(waterlevels)) deallocate(waterlevels)
      end function ecProviderCreateQhtableItems
      
      ! =======================================================================
      
      !> Create source Items and their contained types, based on Fourier file header.
      function ecProviderCreateFourierItems(instancePtr, fileReaderPtr) result(success)
      use m_ec_message
      implicit none 
         logical                      :: success       !< function status
         type(tEcInstance),   pointer :: instancePtr   !< intent(in)
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
         !
         integer :: i            !< loop counter
         integer :: quantityId   !< helper variable 
         integer :: elementSetId !< helper variable 
         integer :: field0Id     !< helper variable 
         integer :: field1Id     !< helper variable 
         integer :: itemId       !< helper variable
         integer :: istat        !< error status
         !
         integer                                      :: nPeriods      !< number of periods
         type(tEcItem),          pointer              :: itemPeriod    !< Item containing the Fourier period
         type(tEcItem),          pointer              :: itemMagnitude !< Item containing the Fourier magnitude
         type(tEcItem),          pointer              :: itemPhase     !< Item containing the Fourier phase
         real(hp), dimension(:), allocatable          :: periods       !< Fourier components transformed into periods
         character(len=8), dimension(:), allocatable  :: components !< Astro component names read from file
         real(hp), dimension(:), allocatable          :: magnitudes    !< seed values for the magnitudes of the Fourier components
         real(hp), dimension(:), allocatable          :: phases        !< seed values for the phases of the Fourier components
         !
         success = .true.
         itemPeriod    => null()
         itemMagnitude => null()
         itemPhase     => null()
         !
         if (ecFourierReadAll(fileReaderPtr, periods, components, magnitudes, phases, nPeriods)) then
            ! ===== all periods =====
            quantityId = ecInstanceCreateQuantity(instancePtr)
            if (.not. (ecQuantitySetName(instancePtr, quantityId, 'period') .and. &
                        ecQuantitySetUnits(instancePtr, quantityId, 'minute'))) then
               success = .false.
            end if
            elementSetId = ecInstanceCreateElementSet(instancePtr)
            if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian) .and. &
                        ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nPeriods))) then
               success = .false.
            end if
            field0Id = ecInstanceCreateField(instancePtr)
            if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, nPeriods))) then
               success = .false.
            end if
            field1Id = ecInstanceCreateField(instancePtr)
            if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, nPeriods))) then
               success = .false.
            end if
            itemId = ecInstanceCreateItem(instancePtr)
            if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                        ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                        ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                        ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                        ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                        ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
               success = .false.
            else
               itemPeriod => ecSupportFindItem(instancePtr, itemId)
               if(allocated(components)) then
                  if(allocated(itemPeriod%sourceT0FieldPtr%astro_components)) then
                     deallocate(itemPeriod%sourceT0FieldPtr%astro_components)
                  end if
                  allocate(itemPeriod%sourceT0FieldPtr%astro_components(nPeriods))
                  if(allocated(itemPeriod%sourceT1FieldPtr%astro_components)) then
                     deallocate(itemPeriod%sourceT1FieldPtr%astro_components)
                  end if
                  allocate(itemPeriod%sourceT1FieldPtr%astro_components(nPeriods))
               end if
            end if
            ! ===== magnitude =====
            quantityId = ecInstanceCreateQuantity(instancePtr)
            if (.not. (ecQuantitySetName(instancePtr, quantityId, 'magnitude') .and. &
                        ecQuantitySetUnits(instancePtr, quantityId, 'm'))) then
               success = .false.
            end if
            elementSetId = ecInstanceCreateElementSet(instancePtr)
            if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian) .and. &
                        ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nPeriods))) then
               success = .false.
            end if
            field0Id = ecInstanceCreateField(instancePtr)
            if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, nPeriods))) then
               success = .false.
            end if
            field1Id = ecInstanceCreateField(instancePtr)
            if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, nPeriods))) then
               success = .false.
            end if
            itemId = ecInstanceCreateItem(instancePtr)
            if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                        ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                        ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                        ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                        ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                        ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
               success = .false.
            else
               itemMagnitude => ecSupportFindItem(instancePtr, itemId)
            end if
            ! ===== phase =====
            quantityId = ecInstanceCreateQuantity(instancePtr)
            if (.not. (ecQuantitySetName(instancePtr, quantityId, 'phase') .and. &
                        ecQuantitySetUnits(instancePtr, quantityId, 'degree'))) then
               success = .false.
            end if
            elementSetId = ecInstanceCreateElementSet(instancePtr)
            if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian) .and. &
                        ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nPeriods))) then
               success = .false.
            end if
            field0Id = ecInstanceCreateField(instancePtr)
            if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, nPeriods))) then
               success = .false.
            end if
            field1Id = ecInstanceCreateField(instancePtr)
            if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, nPeriods))) then
               success = .false.
            end if
            itemId = ecInstanceCreateItem(instancePtr)
            if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                        ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                        ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                        ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                        ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                        ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
               success = .false.
            else
               itemPhase => ecSupportFindItem(instancePtr, itemId)
            end if
            ! ===== finish initialization of Fields =====
            ! This FileReader calculates it's values, rather then reading them in, so we store the seed values in T0 and T1.
            ! The values in T1 are used to cache the original values, while the values in T0 are updated with asc every dtnodal cycle.
            ! The actual calculation is performed by the Converter.
            if (success) then
               istat = 0
               do i=1, nPeriods
                  ! initialize Field T0
                  if(allocated(periods)) then
                     itemPeriod%sourceT0FieldPtr%arr1dPtr(i) = periods(i)
                     itemPeriod%sourceT1FieldPtr%arr1dPtr(i) = periods(i)
                  end if
                  itemMagnitude%sourceT0FieldPtr%arr1dPtr(i) = magnitudes(i)
                  itemMagnitude%sourceT1FieldPtr%arr1dPtr(i) = magnitudes(i)
                  itemPhase%sourceT0FieldPtr%arr1dPtr(i) = phases(i)
                  itemPhase%sourceT1FieldPtr%arr1dPtr(i) = phases(i)
                  
                  if(allocated(components)) then
                     itemPeriod%sourceT0FieldPtr%astro_components(i) = components(i)
                     itemPeriod%sourceT1FieldPtr%astro_components(i) = components(i)
                  end if
               end do
               
               if(istat /= 0) then 
                  if (associated(fileReaderPtr%bc)) then 
                     call setECMessage(trim(getECMessage())//" in file "//trim(fileReaderPtr%bc%fname))
                  else 
                     call setECMessage(trim(getECMessage())//" in file "//trim(fileReaderPtr%fileName))
                  endif 
                   
                  success=.false.
                  return
               endif
               itemPeriod%sourceT0FieldPtr%timesteps = 0.0_hp
               itemMagnitude%sourceT0FieldPtr%timesteps = 0.0_hp
               itemPhase%sourceT0FieldPtr%timesteps = 0.0_hp
               itemPeriod%sourceT1FieldPtr%timesteps = 0.0_hp
               itemMagnitude%sourceT1FieldPtr%timesteps = 0.0_hp
               itemPhase%sourceT1FieldPtr%timesteps = 0.0_hp
            end if
            ! Add successfully created source Items to the FileReader
            if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, itemPeriod%id)
            if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, itemMagnitude%id)
            if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, itemPhase%id)
         else
            success = .false.
         end if
      end function ecProviderCreateFourierItems
      
      ! =======================================================================
      
      !> Create source Item and its contained types, for timeseries source data,
      !! which may come from a time series file ('uniform'), or a BC-file block.
      !! Uniform file records (rows) are treated as vector quantities, reading all physical quantities (columns) into a single Item.
      function ecProviderCreateUniformItems(instancePtr, fileReaderPtr) result(success)
      use m_ec_message
      implicit none 
         logical                      :: success       !< function status
         type(tEcInstance),   pointer :: instancePtr   !< intent(in)
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
         !
         character(len=132)     :: rec          !< first data line in file
         integer                :: n_quantities !< number of quantities in the file
         integer                :: quantityId   !< helper variable 
         integer                :: elementSetId !< helper variable 
         integer                :: field0Id     !< helper variable 
         integer                :: field1Id     !< helper variable 
         integer                :: itemId       !< helper variable 
         integer                :: indx         !< 
         real(hp)               :: time_steps   !< number of time steps for next data block
         type(tEcItem), pointer :: item         !< Item containing all components
         character(len=1)       :: excl         !< 
         character(len=300)     :: msgbuf 
         character(len=:), allocatable :: elementSetName
         character(len=:), allocatable :: quantityName
         !
         success = .true.
         item => null()
         !
         ! At this point fileReaderPtr%vectormax holds the vectormax requested from DEMAND side (toplevel), 
         ! here inferring the vectormax from the SUPPLY side (bottomlevel)
         select case (fileReaderPtr%ofType)
            case (provFile_uniform, provFile_unimagdir)
               rec = ecUniReadFirstLine(fileReaderPtr)
               n_quantities = count_words(rec) - 1
               ! RL: The check below fails because the higher-level vectormax tells nothing about the lower-level vectormax.
               !     The latter can sometimes exceed the former (which became aparent in the case of unimagdir). The unimagdir converter
               !     splits a vectormax=2 uniform source item into two separate vectormax=1 target items. TODO: define new criterium.  
               !if (n_quantities<fileReaderPtr%vectormax) then 
               !   write(msgbuf,'(a,i0,a,i0,a)') 'ERROR : Insufficient columns in file '//trim(fileReaderPtr%fileName)//'(',n_quantities,  &
               !                        ', whereas ',fileReaderPtr%vectormax,' were requested).'
               !   call setECMessage(trim(msgbuf))                        
               !   return                                 ! TODO: error message: tim-file contains less columns than requested 
               !else 
               !   n_quantities=fileReaderPtr%vectormax   ! only use the requested number of columns, ignore the rest of them 
               !endif 
            case (provFile_bc)
               n_quantities = fileReaderPtr%bc%quantity%vectormax
               if (n_quantities/=fileReaderPtr%vectormax) then 
                  write(msgbuf,'(a,i0,a,i0,a)') 'ERROR : Mismatch between vectormax requested (',fileReaderPtr%vectormax,') and supplied by file '   &
                                        // trim(fileReaderPtr%bc%fname) // ', quantity '// trim(fileReaderPtr%bc%qname),' (',n_quantities,').' 
                  call setECMessage(trim(msgbuf))                        
                  return                                 ! TODO: error message: vector definition in the the bc-block with the requested quantity-name
                                                         !       supplies a number of elements different from the demanded number of elements 
               end if
         end select 

         ! time [minute], quantities, elementsetname
         quantityId = ecInstanceCreateQuantity(instancePtr)
         select case (fileReaderPtr%ofType)
            case (provFile_uniform, provFile_unimagdir)
               if (.not. ecQuantitySetName(instancePtr, quantityId, 'uniform_item')) then
                  success = .false.
               end if
               elementSetName = fileReaderPtr%fileName
               if (index(elementSetName,'.')>0) then
                  elementSetName = elementSetName(1:index(elementSetName,'.'))
               end if 
            case (provFile_bc)
               if (.not. ecQuantitySetName(instancePtr, quantityId, fileReaderPtr%bc%quantity%name)) then ! trim(fileReaderPtr%bc%qname))) then
                  success = .false.
               end if
               elementSetName = fileReaderPtr%bc%bcname
         end select 

         ! N_quantities number of scalar quantities.
         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (.not. (ecElementSetSetName(instancePtr, elementSetId, elementSetName))) then 
            success = .false.
         end if
         if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_scalar))) then 
            success = .false.
         end if
         ! N_quantities scalars in a Field array.
         field0Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, n_quantities))) then
            success = .false.
         end if
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, n_quantities))) then
            success = .false.
         end if
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. (ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                    ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                    ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                    ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                    ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                    ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
            success = .false.
         else
            item => ecSupportFindItem(instancePtr, itemId)
         end if

         ! ===== finish initialization of Fields =====
         ! Read the first two records into tEcItem%sourceT0FieldPtr and tEcItem%sourceT1FieldPtr.
         select case (fileReaderPtr%ofType)
            case (provFile_uniform, provFile_unimagdir)
               rewind(unit=fileReaderPtr%fileHandle)
               if (success) then
                   success = ecUniReadBlock(fileReaderPtr, item%sourceT0FieldPtr%timesteps, item%sourceT0FieldPtr%arr1dPtr)
               endif 
               if (success) then
                   success = ecUniReadBlock(fileReaderPtr, item%sourceT1FieldPtr%timesteps, item%sourceT1FieldPtr%arr1dPtr)
               endif 
            case (provFile_bc)
               if (success) then
                   success = ecBCReadBlock(fileReaderPtr, item%sourceT0FieldPtr%timesteps, item%sourceT0FieldPtr%arr1dPtr)
               endif 
               if (success) then
                  if (fileReaderPtr%bc%func /= BC_FUNC_CONSTANT) then
                     ! read second line for T1-Field
                     success = ecBCReadBlock(fileReaderPtr, item%sourceT1FieldPtr%timesteps, item%sourceT1FieldPtr%arr1dPtr)
                  else
                     item%sourceT1FieldPtr%timesteps = 54321.0D+10
                     item%sourceT1FieldPtr%arr1dPtr = item%sourceT0FieldPtr%arr1dPtr
                  endif
               endif 
         end select 
         ! Add successfully created source Item to the FileReader
         if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item%id)
         item%quantityPtr%vectorMax = n_quantities 
      end function ecProviderCreateUniformItems
      
      ! =======================================================================
      
      !> Create source Items and their contained types, based on Arcinfo file header.
      function ecProviderCreateArcinfoItems(instancePtr, fileReaderPtr) result(success)
         logical                      :: success       !< function status
         type(tEcInstance),   pointer :: instancePtr   !< intent(in)
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
         !
         integer  :: n_cols       !< number of columns
         integer  :: n_rows       !< number of rows
         real(hp) :: x0           !< seed x-coordinate
         real(hp) :: y0           !< seed y-coordinate
         real(hp) :: dxa          !< step size in x
         real(hp) :: dya          !< step size in y
         real(hp) :: dmiss        !< missing data value
         integer  :: quantityId   !< helper variable 
         integer  :: elementSetId !< helper variable 
         integer  :: field0Id     !< helper variable 
         integer  :: field1Id     !< helper variable 
         integer  :: itemId       !< helper variable
         type(tEcItem), pointer :: item
         character(len=20) :: name   !< 
         integer :: i !< loop counter
         !
         success = .true.
         item => null()
         ! Read relevant information from the first arcinfo file's header.
         if (readarcinfoheader(fileReaderPtr%fileHandle, n_cols, n_rows, x0, y0, dxa, dya, dmiss)) then
            ! One common ElementSet.
            elementSetId = ecInstanceCreateElementSet(instancePtr)
            if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian_equidistant) .and. &
                       ecElementSetSetX0Dx(instancePtr, elementSetId, x0, dxa) .and. &
                       ecElementSetSetY0Dy(instancePtr, elementSetId, y0, dya) .and. &
                       ecElementSetSetRowsCols(instancePtr, elementSetId, n_rows, n_cols))) then
               success = .false.
            end if
            !
            if (index(fileReaderPtr%fileName, '.amu') /= 0) then
               ! ===== quantity: wind component u (usually == x) =====
               quantityId = ecInstanceCreateQuantity(instancePtr)
               if (.not. (ecQuantitySetName(instancePtr, quantityId, 'wind_u') .and. &
                           ecQuantitySetUnits(instancePtr, quantityId, trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
                  success = .false.
               end if
            else if (index(fileReaderPtr%fileName, '.amv') /= 0) then
               ! ===== quantity: wind component v (usually == y) =====
               quantityId = ecInstanceCreateQuantity(instancePtr)
               if (.not. (ecQuantitySetName(instancePtr, quantityId, 'wind_v') .and. &
                           ecQuantitySetUnits(instancePtr, quantityId, trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
                  success = .false.
               end if
            else if (index(fileReaderPtr%fileName, '.amp') /= 0) then
               ! ===== quantity: wind component p =====
               quantityId = ecInstanceCreateQuantity(instancePtr)
               if (.not. (ecQuantitySetName(instancePtr, quantityId, 'wind_p') .and. &
                           ecQuantitySetUnits(instancePtr, quantityId, trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
                  success = .false.
               end if
            end if
            field0Id = ecInstanceCreateField(instancePtr)
            if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, n_cols*n_rows) .and. &
                        ecFieldSetMissingValue(instancePtr, field0Id, dmiss))) then
               success = .false.
            end if
            field1Id = ecInstanceCreateField(instancePtr)
            if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, n_cols*n_rows) .and. &
                        ecFieldSetMissingValue(instancePtr, field1Id, dmiss))) then
               success = .false.
            end if
            itemId = ecInstanceCreateItem(instancePtr)
            if (.not. ( ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                        ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                        ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                        ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                        ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                        ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
               success = .false.
            else
               item => ecSupportFindItem(instancePtr, itemId)
            end if
         else
            success = .false. ! failed reading header
         end if
         ! ===== finish initialization of Fields =====
         ! Read the first two records into tEcItem%sourceT0FieldPtr and tEcItem%sourceT1FieldPtr.
         if (success) then
            success = ecSpiderAndCurviAndArcinfoReadToBody(fileReaderPtr%fileHandle)
         end if
         if (success) then
            success = ecArcinfoAndT3dReadBlock(fileReaderPtr, fileReaderPtr%fileHandle, 0, n_cols, n_rows, item)
         end if
         if (success) then
            success = ecArcinfoAndT3dReadBlock(fileReaderPtr, fileReaderPtr%fileHandle, 1, n_cols, n_rows, item)
         end if
         ! Add successfully created source Item to the FileReader
         if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item%id)
      end function ecProviderCreateArcinfoItems
      
      ! =======================================================================

      !> Construct an ElementSet from a curvilinear grid file.
      !! meteo1: reaarc_curv_tim
      function ecProviderCreateCurviElementSet(instancePtr, fileReaderPtr, elementSetId, n_cols, n_rows) result(success)
         logical                                  :: success       !< function status
         type(tEcInstance),   pointer             :: instancePtr   !< intent(in)
         type(tEcFileReader), pointer             :: fileReaderPtr !< intent(inout)
         integer,                     intent(out) :: elementSetId  !< if of new ElementSet
         integer,                     intent(out) :: n_cols, n_rows
         !
         character(len=132)                  :: rec       !< a read line
         character(len=maxFileNameLen)       :: grid_file !< file name of curvilinear grid
         integer                             :: minp      !< IO unit number
         integer                             :: mx, nx    !< n_clos, n_rows
         real(hp), dimension(:), allocatable :: x, y      !< coordinate arrays
         integer                             :: i, j      !< loop counters
         character(len=10)                   :: dummy     !< helper variable for ignored data
         integer                             :: istat     !< status of operation
         integer                             :: elmSetType  !< spherical (elmSetType_spheric) or not (elmSetType_Cartesian)
         !
         success = .false.
         ! Find and open the curvilinear grid file.
         rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'grid_file')
         read(rec, *) grid_file
         success = ecSupportOpenExistingFile(minp, grid_file)
         if (.not. success) return
         ! Read the file header.
         elmSetType = elmSetType_Cartesian
20       read(minp,'(a)') rec
         if (index (rec,'=') == 0) then
         
            ! Backwards compatible: first line could contain spherical keyword         
            if (index(rec, 'Spherical') >= 1  .or. &
                index(rec, 'SPHERICAL') >= 1  ) then
               ! grid has spherical coordinates.
               elmSetType = elmSetType_spheric
            endif
         
            goto 20
         end if
         
         read(minp,*) mx, nx
         read(minp,*) ! skips a line
         ! Read the file body.
         allocate(x(mx*nx))
         allocate(y(mx*nx))
         ! Read the data row-by-row into a 1D array.
         do j=0, nx-1
            read(minp, *, iostat = istat) dummy, dummy, (x(j*mx+i), i=1, mx)
         end do
         do j=0, nx-1
            read(minp, *, iostat = istat) dummy, dummy, (y(j*mx+i), i=1, mx)
         end do
         close(minp)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_provider::ecProviderCreateCurviElementSet: Unable to read data block from curvilinear grid file.")
            success = .false.
            return
         end if
         !
         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType) .and. &
                    ecElementSetSetXArray(instancePtr, elementSetId, x) .and. &
                    ecElementSetSetYArray(instancePtr, elementSetId, y) .and. &
                    ecElementSetSetRowsCols(instancePtr, elementSetId, nx, mx))) then
            success = .false.
         end if
         deallocate(x)
         deallocate(y)
         n_cols = mx
         n_rows = nx
      end function ecProviderCreateCurviElementSet
      
      ! =======================================================================
      
      !> Create source Items with their contained types, based on curvilinear file header.
      !! metteo1: reaarc_curv_tim
      function ecProviderCreateCurviItems(instancePtr, fileReaderPtr) result(success)
         logical                      :: success       !< function status
         type(tEcInstance),   pointer :: instancePtr   !< intent(in)
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
         !
         integer                   :: quantityId      !< id of new Quantity
         integer                   :: elementSetId    !< id of new ElementSet
         integer                   :: field0Id        !< id of new Field
         integer                   :: field1Id        !< id of new Field
         integer                   :: itemId          !< id of new Item
         character(len=132)        :: rec             !< a read line
         real(hp)                  :: missingValue    !< helper variable
         integer                   :: n_cols, n_rows
         integer                   :: n_quantity
         integer                   :: i
         character(len=1)          :: postfix
         !
         success = .false.
         !
         ! ==================================
         ! Interrogate the curvi file header.
         ! ==================================
         rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'NODATA_value')
         if (len_trim(rec) /= 0) then
            read(rec, *) missingValue
         else
            missingValue = -9999.0_hp
         end if
         !
         rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'n_quantity')
         if (len_trim(rec) == 0) then
            call setECMessage("ERROR: ec_provider::ecProviderCreateCurviItems: Missing keyword n_quantity in file header: "//trim(fileReaderPtr%fileName))
            return
         end if
         read(rec, *) n_quantity
         if (n_quantity < 1) then
            call setECMessage("ERROR: ec_provider::ecProviderCreateCurviItems: Less then one quantity specified in header: "//trim(fileReaderPtr%fileName))
            return
         end if
         if (n_quantity > 9) then
            call setECMessage("ERROR: ec_provider::ecProviderCreateCurviItems: More then nine quantities specified in header: "//trim(fileReaderPtr%fileName))
            return
         end if
         !
         ! ============================================================
         ! Construct a source Item for each quantity in the curvi file.
         ! ============================================================
         ! Use a common ElementSet for the Items.
         if (.not. ecProviderCreateCurviElementSet(instancePtr, fileReaderPtr, elementSetId, n_cols, n_rows)) return
         !
         do i=1, n_quantity
            write(postfix, '(I1)') i
            ! Quantity.
            quantityId = ecInstanceCreateQuantity(instancePtr)
            if (.not. ecQuantitySetName(instancePtr, quantityId, 'curvi_source_item_'//postfix)) return
            ! Field for T0.
            field0Id = ecInstanceCreateField(instancePtr)
            if (.not.      ecFieldCreate1dArray(instancePtr, field0Id, n_cols*n_rows)) return
            if (.not. ecFieldSetMissingValue(instancePtr, field0Id, missingValue )) return
            ! Field for T1.
            field1Id = ecInstanceCreateField(instancePtr)
            if (.not.      ecFieldCreate1dArray(instancePtr, field1Id, n_cols*n_rows)) return
            if (.not. ecFieldSetMissingValue(instancePtr, field1Id, missingValue )) return
            ! Item.
            itemId = ecInstanceCreateItem(instancePtr)
            if (.not.          ecItemSetRole(instancePtr, itemId, itemType_source      )) return
            if (.not.          ecItemSetType(instancePtr, itemId, accessType_fileReader)) return
            if (.not.      ecItemSetQuantity(instancePtr, itemId, quantityId           )) return
            if (.not.    ecItemSetElementSet(instancePtr, itemId, elementSetId         )) return
            if (.not. ecItemSetSourceT0Field(instancePtr, itemId, field0Id             )) return
            if (.not. ecItemSetSourceT1Field(instancePtr, itemId, field1Id             )) return
            ! Add successfully created source Items to the FileReader.
            if (.not. ecFileReaderAddItem(instancePtr, fileReaderPtr%id, itemId)) return
         end do
         ! ===== finish initialization of Fields =====
         ! Read the first two records into tEcItem%sourceT0FieldPtr and tEcItem%sourceT1FieldPtr.
         if (.not. ecSpiderAndCurviAndArcinfoReadToBody(fileReaderPtr%fileHandle)) return
         if (.not. ecCurviReadBlock(fileReaderPtr, fileReaderPtr%fileHandle, 0)) return
         if (.not. ecCurviReadBlock(fileReaderPtr, fileReaderPtr%fileHandle, 1)) return
         !
         success = .true.
      end function ecProviderCreateCurviItems

      ! =======================================================================
      
      !> Create source Items and their contained types, based on the 'sample' file format.
      function ecProviderCreateSampleItems(instancePtr, fileReaderPtr) result(success)
         logical                      :: success       !< function status
         type(tEcInstance),   pointer :: instancePtr   !< intent(in)
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
         !
         real(hp),            dimension(:),   allocatable :: xs
         real(hp),            dimension(:),   allocatable :: ys
         real(hp),            dimension(:,:), allocatable :: zs

         integer  :: nSamples     !< number of samples
         integer  :: kx           !< vector max of values at sample points

         integer  :: quantityId   !< helper variable 
         integer  :: elementSetId !< helper variable 
         integer  :: field0Id     !< helper variable 
         integer  :: field1Id     !< helper variable 
         integer  :: itemId       !< helper variable
         type(tEcItem), pointer :: item
         character(len=20) :: name   !< 
         integer :: i !< loop counter
         !
         success = .false.
         item => null()
         
         if (.not. ecSampleReadAll(fileReaderPtr, xs, ys, zs, nSamples, kx)) return

         ! Construct the samples Item.
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. (ecQuantitySetName(instancePtr, quantityId, 'samples_item'))) return

         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (.not. ecElementSetSetType(instancePtr, elementSetId, elmSetType_samples)) return
         if (.not. ecElementSetSetXArray(instancePtr, elementSetId, xs)) return
         if (.not. ecElementSetSetYArray(instancePtr, elementSetId, ys)) return
         if (.not. ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, nSamples)) return     

         field0Id = ecInstanceCreateField(instancePtr)
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, kx*nSamples))) return

         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. ecItemSetRole(instancePtr, itemId, itemType_source)) return
         if (.not. ecItemSetType(instancePtr, itemId, accessType_fileReader)) return 
         if (.not. ecItemSetQuantity(instancePtr, itemId, quantityId)) return
         if (.not. ecItemSetElementSet(instancePtr, itemId, elementSetId)) return
         if (.not. ecItemSetSourceT0Field(instancePtr, itemId, field0Id)) return
         if (.not. ecItemSetSourceT1Field(instancePtr, itemId, field1Id)) return

         item => ecSupportFindItem(instancePtr, itemId)
         item%quantityPtr%vectorMax = kx

         do i=1,nSamples
            ! initialize Field T1 (only one containing sample data)
            item%sourceT1FieldPtr%arr1dPtr((i-1)*kx+1:i*kx) = zs(1:kx,i)
         end do

         item%sourceT0FieldPtr%timesteps = 0.0_hp
         item%sourceT1FieldPtr%timesteps = huge(0.0_hp)

         if (.not. ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item%id)) return

         success = .true.
      end function ecProviderCreateSampleItems
      
      ! =======================================================================

      !> Create source Items with their contained types, based on t3D file header.
      function ecProviderCreatet3DItems(instancePtr, fileReaderPtr) result(success)
         logical                      :: success       !< function status
         type(tEcInstance),   pointer :: instancePtr   !< intent(in)
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
         !
         integer                             :: quantityId    !< id of new Quantity
         integer                             :: elementSetId  !< id of new ElementSet
         integer                             :: field0Id      !< id of new Field
         integer                             :: field1Id      !< id of new Field
         integer                             :: itemId        !< id of new Item
         integer                             :: layer_type    !< type of layer
         integer                             :: zInterpolationType    !< vertical interpolation type
         type(tEcItem), pointer              :: valueptr      !< Item containing z/sigma-dependent values
         type(tEcBCBlock), pointer           :: bcptr
         character(len=132)                  :: rec           !< a read line
         integer, parameter                  :: MAXSTRLEN=128 !<
         integer, parameter                  :: MAXLAY=256    !<
         integer                             :: numlay, i     !<
         real(hp), dimension(:), allocatable :: xws           !< x-values
         real(hp), dimension(:), allocatable :: yws           !< y-values
         real(hp), dimension(:), allocatable :: zws           !< z-values of vertical velocities
         real(hp), dimension(MAXLAY)         :: a             !< 

         integer                             :: ilay, jlay 
         integer                             :: col_tmp 
         real(hp)                            :: zws_tmp
         integer                             :: vectormax, iostat
             
            
         !
         success = .false.
         valueptr => null()
         vectormax = 1                    ! assumed scalar if vector dimensions not made explicit                      
         zInterpolationType = zinterpolate_unknown
	      select case (fileReaderPtr%ofType)
         case (provFile_t3D)
            zInterpolationType = zinterpolate_linear
            a = ec_undef_hp
            !
            ! check if these are z-layers or sigma-layers
            rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'LAYER_TYPE')
            if (len_trim(rec) == 0) then
               call setECMessage("Unable to find LAYER_TYPE in file header.")
               return
            end if
            !
            call str_lower(rec)
            if ( index(rec,'sigma') /= 0  ) then
               layer_type = 0
            else if ( index(rec,'z') /= 0 ) then
               layer_type = 1
            else
               call setECMessage("Invalid LAYER_TYPE specified in header.")
               return
            end if
            ! 
            ! Acquire the number of layers by the number of readable columns in LAYERS field of header 
            rec = ect3DFindInFile(fileReaderPtr%fileHandle, 'LAYERS')
            if (len_trim(rec) == 0) then
               call setECMessage("Unable to find LAYER in file header.")
               return
            end if
            !
            read(rec,*, end = 666) (a(i),i=1,MAXLAY)
        666 continue

            ! Acquire the number of vector elements (i.e. vectormax) from the header in case it is specified explicitly
            rec = ect3DFindInFile(fileReaderPtr%fileHandle, 'VECTORMAX')
            if (len_trim(rec) > 0) then
               read(rec,*,iostat=iostat) vectormax
            end if 

            !
            numlay = 0           
            do i=1,MAXLAY
               if ( a(i) /= ec_undef_hp ) then
                  numlay = numlay+1
               end if
            end do
            !
            ! allocate x and y array of size 1, currently not used, but its size is inquired lateron
            allocate(xws(1))
            allocate(yws(1))
            ! allocate vertical mesh array
            if ( numlay > 0 ) then
               allocate(zws(numlay))
            else
               call setECMessage("No LAYERS found in header")
               return
            end if
            !
!           xws = (/ (-1.0) /)             
!           yws = (/ (-1.0) /)
            zws = (/ (a(i), i=1,numlay) /)
            rewind(fileReaderPtr%fileHandle)          

	      case (provFile_bc)
            ! Check if these are z-layers or sigma-layers
            if (.not.associated(fileReaderPtr%bc)) then 
               call setECMessage("BC-filetype, but no bc instance associated to filereader")
               return 
            endif 
            bcptr => fileReaderPtr%bc
            select case (bcptr%vptyp)
              case (BC_VPTYP_PERCBED)
                 layer_type = 0
              case (BC_VPTYP_ZBED)
                 layer_type = 1
              case default
                 ! Invalid vertical position specification type 
                 call setECMessage("Invalid or missing vertical position type.")
		           return
              end select 
              zInterpolationType = bcptr%zInterpolationType
	     !   Defined types (yet to be implemented):
	     !   integer, parameter :: BC_VPTYP_PERCBED     = 1   !< precentage from bed 
	     !   integer, parameter :: BC_VPTYP_ZDATUM      = 2   !< z above datum 
	     !   integer, parameter :: BC_VPTYP_BEDSURF     = 3   !< bedsurface 
	     !   integer, parameter :: BC_VPTYP_PERCSURF    = 4   !< percentage from surface 
	     !   integer, parameter :: BC_VPTYP_ZBED        = 5   !< z from bed 
	     !   integer, parameter :: BC_VPTYP_ZSURF       = 6   !< z from surface 
        !   Acquire the number of layers, 
        !   Corresponds with the number of matching quantity blocks in a bc-header

            numlay = bcptr%numlay
            vectormax = bcptr%quantity%vectormax

            ! Prepare the z-coordinates for the levels in zws(dimension=numlay)
            ! Corresponds with the SELECTED vertical positions in vp(:)
            ! allocate x and y array of size 1, currently not used, but its size is inquired lateron
            allocate(xws(1))
            allocate(yws(1))
            ! allocate vertical mesh array
            if ( numlay > 0 ) then
               allocate(zws(numlay))
            else
               call setECMessage("ERROR: ec_provider::ecProviderCreatet3DItems: no layers found in header")
               return
            end if
            xws = (/ (-1.0) /)             
            yws = (/ (-1.0) /)
            zws = (/ (bcptr%vp(i), i=1,numlay) /)

         case default
            call setECMessage("ERROR: ec_provider::ecProviderCreatet3DItems: Unknown file type.")
            return 
	      end select
         !
         ! ===== single quantity: quant ===== (Further code in this sub independent of file type)
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not.ecQuantitySetName(instancePtr, quantityId, 'quant'))                   return 
         if (.not.ecQuantitySetUnits(instancePtr, quantityId, ' '))                      return 
         if (.not.ecQuantitySetVectorMax(instancePtr, quantityId, vectormax))            return 

         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (.not.ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian))  return
         if (.not.ecElementSetSetXArray(instancePtr, elementSetId, xws))                 return
         if (.not.ecElementSetSetYArray(instancePtr, elementSetId, yws))                 return
         if (.not.ecElementSetSetZArray(instancePtr, elementSetId, zws))                 return 
         if (.not.ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, 1))      return 

         field0Id = ecInstanceCreateField(instancePtr)
         if (.not.(ecFieldCreate1dArray(instancePtr, field0Id, numlay*vectormax)))        return
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not.(ecFieldCreate1dArray(instancePtr, field1Id, numlay*vectormax)))        return
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not.ecItemSetRole(instancePtr, itemId, itemType_source))       return
         if (.not.ecItemSetType(instancePtr, itemId, accessType_fileReader)) return
         if (.not.ecItemSetQuantity(instancePtr, itemId, quantityId))        return
         if (.not.ecItemSetElementSet(instancePtr, itemId, elementSetId))    return
         if (.not.ecItemSetSourceT0Field(instancePtr, itemId, field0Id))     return
         if (.not.ecItemSetSourceT1Field(instancePtr, itemId, field1Id))     return
         valueptr => ecSupportFindItem(instancePtr, itemId)              

         ! ===== finish initialization of Fields =====
         ! Read the first two records into tEcItem%sourceT0FieldPtr and tEcItem%sourceT1FieldPtr.
         select case (fileReaderPtr%ofType)
         case (provFile_t3D)
            if (.not. ecArcinfoAndT3dReadBlock(fileReaderPtr, fileReaderPtr%fileHandle, 0, numlay*vectormax, 1, valueptr)) return 
            if (.not. ecArcinfoAndT3dReadBlock(fileReaderPtr, fileReaderPtr%fileHandle, 1, numlay*vectormax, 1, valueptr)) return 
         case (provFile_bc)
            if (.not. ecBCReadLine(fileReaderPtr, valueptr%sourceT0FieldPtr%arr1dPtr, valueptr%sourceT0FieldPtr%timesteps))  return 
            if (.not. ecBCReadLine(fileReaderPtr, valueptr%sourceT1FieldPtr%arr1dPtr, valueptr%sourceT1FieldPtr%timesteps))  return
         case default
            call setECMessage("ERROR: ec_provider::ecProviderCreatet3DItems: Unknown file type.")
            return 
         end select
         !Assign vertical interpolation type        
         valueptr%quantityPtr%zInterpolationType = zInterpolationType
         ! Add successfully created source Items to the FileReader
         if (.not.ecFileReaderAddItem(instancePtr, fileReaderPtr%id, valueptr%id)) return 
         success = .true.
    end function ecProviderCreatet3DItems

      !> Create subproviders, which create source Items and their contained types.
      !! meteo1.f90: read1polylin
      function ecProviderCreatePolyTimItems(instancePtr, fileReaderPtr) result(success)
         logical                                   :: success       !< function status
         type(tEcInstance),   pointer              :: instancePtr   !< intent(in)
         type(tEcFileReader), pointer              :: fileReaderPtr !< intent(inout)

         !
         real(hp), dimension(:), allocatable :: xs    !< x-coordinates of support points
         real(hp), dimension(:), allocatable :: ys    !< y-coordinates of support points
         real(hp), dimension(:), allocatable :: zs    !< z/sigma-coordinates of support points
         integer,  dimension(:), allocatable :: mask  !< support point mask array (for polytime ElementSet)
         integer                             :: n_points !< number of support points
         integer                             :: n_signals !< Number of forcing signals created (at most n_signals==n_points, but warn if n_signals==0)
         character(len=132)                  :: rec      !< a read line
         integer                             :: i, j     !< loop counters
         integer                             :: istat    !< status of read operation
         integer                             :: L        !< helper index
         character(len=4)                    :: tex      !< helper string for constructin file names
         character(len=maxFileNameLen)       :: filename !< helper string containing subprovider file name.
         character(len=maxFileNameLen)       :: plipointlbl   !< temporary name of current pli-point in bct context 
         character(len=maxFileNameLen), &
         &   dimension(:), allocatable       :: plipointlbls  !< user-specified name for all pli-point in bct context 
         character(len=maxFileNameLen)       :: polyline_name !< polyline name read from pli-file 
         logical                             :: exists   !< helper boolian, indicating file existence
         integer                             :: id       !< dummy, catches ids which are not used
         integer                             :: k_yyyymmdd !< calculated Gregorian calender date, serving as reference date
         integer                             :: quantityId, elementSetId, fieldId, itemId, subconverterId, connectionId, BCBlockID
         integer                             :: wind_x, wind_y
         integer                             :: magnitude, discharge, waterlevel, slope, crossing, maxLay
         type(tEcItem), pointer              :: itemPT
         type(tEcItem), pointer              :: itemt3D
         type(tEcItem), pointer              :: sourceItem
         integer,  dimension(:), allocatable :: itemIDList
         integer                             :: vectormax
          
         logical		                     ::	is_tim, is_cmp, is_tim3d, is_qh
         logical                             :: has_label 
         integer                             :: lblstart, lblend                
         type(tEcFileReader), pointer	     :: fileReaderPtr2
         !

!        initialization         
         success = .false.
         itemPT => null()
         sourceItem => null()
         itemt3D => null()
         maxlay = 0
         vectormax = 1
         !
         ! Skip the lead comment lines plus one additional line.
         do
            read(fileReaderPtr%fileHandle, '(a)', iostat = istat) rec
            if (istat /= 0) then
               call setECMessage("ERROR: ec_provider::ecProviderCreatePolyTimItems: Unexpected end of file.")
               return
            end if
            if (rec(1:1) /= '*') exit
         end do
         ! Read the polyline name from the first line of a tekal-block  
         polyline_name = trim(adjustl(rec))
         ! Read the number of support points.
         read(fileReaderPtr%fileHandle, *, iostat = istat) n_points
         if (istat /= 0) then
            call setECMessage("ERROR: ec_provider::ecProviderCreatePolyTimItems: Unable to read the number of points.")
            return
         end if
         ! Sanity check
         if (n_points < 2) then
            call setECMessage("ERROR: ec_provider::ecProviderCreatePolyTimItems: Less then two support points found.")            
            return
         end if
         ! Read the support point coordinate pairs.
         allocate(xs(n_points))
         allocate(ys(n_points))
         allocate(mask(n_points))
         mask = 1
         allocate(itemIDList(n_points))
         itemIDList = ec_undef_int
         allocate(plipointlbls(n_points))
         plipointlbls = ''


         do i=1, n_points
            read(fileReaderPtr%fileHandle,'(a132)', iostat = istat) rec
            if (index(rec,'!')>0) rec = rec(1:index(rec,'!')-1)          ! trim commented  (!)
            if (index(rec,'#')>0) rec = rec(1:index(rec,'#')-1)          ! trim commented  (#)
            if (len(trim(rec)) == 0) cycle                               ! skip empty lines     (or commented-out coordinate pairs) 
            istat = 0
            read(rec, *, iostat = istat) xs(i), ys(i)                    ! see if we can at least read a coordinate pair 
            if (istat /= 0) then
               call setECMessage("   '"//trim(rec)//"'")
               call setECMessage("Unable to read a coordinate pair from file "//trim(fileReaderPtr%fileName))
               return
            end if
            !lblstart = index(rec,'{')
            !lblend = index(rec,'}')
            !plipointlbls(i) = ''
            !if (lblstart>0 .and. lblend>lblstart) then
            !   plipointlbls  = rec(lblstart+1:lblend-1)
            !endif
            lblstart = index(rec,'label=')
            plipointlbls(i) = ''
            if (lblstart>0) then
               read(rec(lblstart+6:len_trim(rec)),*,iostat=istat)  plipointlbls(i)
            endif
         enddo
         
         ! Construct the poly_tim Item.
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. (ecQuantitySetName(instancePtr, quantityId, 'polytim_item'))) return

         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (.not. ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian)) return
         if (.not. ecElementSetSetXArray(instancePtr, elementSetId, xs)) return
         if (.not. ecElementSetSetYArray(instancePtr, elementSetId, ys)) return
         if (.not. ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, n_points)) return     

         fieldId = ecInstanceCreateField(instancePtr)
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. ecItemSetRole(instancePtr, itemId, itemType_target)) return
         if (.not. ecItemSetType(instancePtr, itemId, accessType_fileReader)) return 
         if (.not. ecItemSetQuantity(instancePtr, itemId, quantityId)) return
         if (.not. ecItemSetElementSet(instancePtr, itemId, elementSetId)) return
         if (.not. ecItemSetTargetField(instancePtr, itemId, fieldId)) return

         itemPT => ecSupportFindItem(instancePtr, itemId)
         call jul2ymd(int(fileReaderPtr%tframe%k_refdate + 2400000.5_hp), k_yyyymmdd)

         ! Init BCBlock for (global) qh-bound 
         is_qh = .false. 
         ! Determine the end of the base of the fileName.
         L = index(fileReaderPtr%fileName, '.', back = .true.) - 1
         ! Create providers at each support point, depending on the availability of specific files.
         call jul2ymd(int(fileReaderPtr%tframe%k_refdate + 2400000.5_hp), k_yyyymmdd)
         ! Exceptional case: A single qh-table supplies all support points of the pli-file.
         filename = fileReaderPtr%fileName(1:L)//'.qh'
         inquire (file = trim(filename), exist = exists)
         if (exists) then
            ! Process a *.qh file.
            ! Construct a new FileReader
            id = ecInstanceCreateFileReader(instancePtr)
            if (id == ec_undef_int) then
               return
            end if

            ! Initialize the new FileReader.
            if (.not. ecProviderInitializeFileReader(instancePtr, id, provFile_qhtable, filename, k_yyyymmdd, &
                           fileReaderPtr%tframe%k_timezone, fileReaderPtr%tframe%k_timestep_unit)) return 
            ! All fine:
            is_qh = .true.
         endif

         n_signals = 0 ! Record whether at least one child provider is created for this polytim.

         if (is_qh) then
            ! Construct a new Converter.
            subconverterId = ecInstanceCreateConverter(instancePtr)
            ! Determine the source Items.
            discharge = ecFileReaderFindItem(instancePtr, id, 'discharge')
            waterlevel = ecFileReaderFindItem(instancePtr, id, 'waterlevel')
            slope = ecFileReaderFindItem(instancePtr, id, 'slope')
            crossing = ecFileReaderFindItem(instancePtr, id, 'crossing')
            if (discharge /= ec_undef_int .and. waterlevel /= ec_undef_int .and. &
               slope /= ec_undef_int .and. crossing /= ec_undef_int) then
               !do i=1, n_points ! commented: only one value per polyline
               ! Initialize the new Converter.
               if (.not. (ecConverterSetType(instancePtr, subconverterId, convType_qhtable) .and. &
                          ecConverterSetOperand(instancePtr, subconverterId, operand_replace_element) .and. &
                          ecConverterSetInterpolation(instancePtr, subconverterId, interpolate_passthrough) .and. &
                          ecConverterSetElement(instancePtr, subconverterId, 1))) return ! set to 1 from i: only one value per polyline
               ! Construct a new Connection.
               connectionId = ecInstanceCreateConnection(instancePtr)
               if (.not. ecConnectionSetConverter(instancePtr, connectionId, subconverterId)) return
               ! Initialize the new Connection.
               if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, discharge)) return
               if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, waterlevel)) return
               if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, slope)) return
               if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, crossing)) return
               if (.not. ecConnectionAddTargetItem(instancePtr, connectionId, itemId)) return
               if (.not. ecItemAddConnection(instancePtr, itemId, connectionId)) return
               n_signals = 1
               !end do
            end if
         else                            ! .not.is_qh
            n_signals = 0
            do i=1, n_points
               is_tim = .false.
               is_cmp = .false.
               is_tim3d = .false.
               ! plipoint labels read from the third column in the pli-file. Currently this goes wrong if in the test third-column labels are not unique 
               if (len_trim(plipointlbls(i))==0) then 
                  write(plipointlbl,'(a,i4.4)') fileReaderPtr%fileName(1:L)//'_',i
                  has_label = .False.
               else
                  plipointlbl = trim(plipointlbls(i))
                  has_label = .True.
               endif

               filename = trim(plipointlbl)//'.tim'
               inquire (file = trim(filename), exist = exists)
               if (exists) then
                  id = ecInstanceCreateFileReader(instancePtr)
                  if (id == ec_undef_int) return
                  fileReaderPtr2=>ecSupportFindFileReader(instancePtr, id)
                  fileReaderPtr2%vectormax = fileReaderPtr%vectormax
                  if (.not. (ecProviderInitializeFileReader(instancePtr, id, provFile_uniform, filename, k_yyyymmdd,       &
                                        fileReaderPtr%tframe%k_timezone, fileReaderPtr%tframe%k_timestep_unit))) return
                  is_tim = .true.
               else 
                  filename = trim(plipointlbl)//'.cmp'
                  inquire (file = trim(filename), exist = exists)
                  if (exists) then
                     id = ecInstanceCreateFileReader(instancePtr)
                     if (id == ec_undef_int) return
                     fileReaderPtr2=>ecSupportFindFileReader(instancePtr, id)
                     fileReaderPtr2%vectormax = fileReaderPtr%vectormax
                     if (.not. (ecProviderInitializeFileReader(instancePtr, id, provFile_fourier, filename, k_yyyymmdd,       &
                                        fileReaderPtr%tframe%k_timezone, fileReaderPtr%tframe%k_timestep_unit, dtnodal=fileReaderPtr%tframe%dtnodal))) return

                     is_cmp = .true.
                  else 
                     filename = trim(plipointlbl)//'.t3d'
                     inquire (file = trim(filename), exist = exists)
                     if (exists) then 
                        id = ecInstanceCreateFileReader(instancePtr)
                        if (id == ec_undef_int) return
                        fileReaderPtr2=>ecSupportFindFileReader(instancePtr, id)
                        fileReaderPtr2%vectormax = fileReaderPtr%vectormax
                        if (.not. (ecProviderInitializeFileReader(instancePtr, id, provFile_t3D, filename, k_yyyymmdd,       &
                                        fileReaderPtr%tframe%k_timezone, fileReaderPtr%tframe%k_timestep_unit))) return

                        is_tim3d = .true.
                     else                           ! No file with data for this point 
                        if (has_label) then    ! Report explicitly labelled point without data 
                           call setECMessage("No .tim, .cmp or .t3d file found for labelled point '" &
                                          //  trim(plipointlbl)//"' (required).")
                           return
                        endif ! labelled point ? 
                     endif    ! tim3d-file ? 
                  endif       ! cmp file ? 
               endif          ! tim-file ?
               !if (.not. ecProviderConnectSourceItemsToTargets(instancePtr, is_tim, is_cmp, is_tim3d, 'uniform_item',      &
               !                                                id, itemId, i, n_signals, maxlay, itemIDList)) then
               if (.not. ecProviderConnectSourceItemsToTargets(instancePtr, is_tim, is_cmp, is_tim3d, id, itemId, i,            &
                                                           n_signals, maxlay, itemIDList)) then
                  ! No sub-FileReader made.
                  mask(i) = 0
               endif
            end do               ! loop over support points
         endif                   ! switch between qh/cmp/tim
         if (n_signals <= 0) then
            call setECMessage("ERROR: ec_provider::ecProviderPolyTimItems: No forcing signals (.tim/.cmp/.t3d/.qh) could be attached for polyline file '"//trim(fileReaderPtr%filename)//"'.")
            return
         end if

         ! itemID refers to the source item (providing to the polytim item) for the last support point we came across in the above loop.
         if (.not. ecProvider3DVectmax(instancePtr, itemPT, mask ,maxlay, n_points, itemIDList)) return 

         ! Since the main FileReader's Item is a target, the TimeFrame is not set.
         ! Add successfully created source Item to the main FileReader
         if (.not. ecFileReaderAddItem(instancePtr, fileReaderPtr%id, itemPT%id)) return 
         !
         ! close pli file
         close(fileReaderPtr%fileHandle, iostat = istat)
         success = .true. 
      end function ecProviderCreatePolyTimItems
      
      
!==============================================================================================================

      !> Create subproviders, which create source Items and their contained types.
      !! meteo1.f90: read1polylin
      function ecProviderCreatePolyTimItemsBC(instancePtr, fileReaderPtr, bctfilename, quantityname) result(success)
         logical                         :: success       !< function status
         type(tEcInstance),   pointer    :: instancePtr   !< intent(in)
         type(tEcFileReader), pointer    :: fileReaderPtr !< intent(inout)
         character(len=*),    intent(in) :: bctfilename   !< in case of bct-data, we neeed the explicit filename 
         character(len=*),    intent(in) :: quantityname  !< in case of bct-data, we neeed the explicit quantityname 
         !
         real(hp), dimension(:), allocatable :: xs    !< x-coordinates of support points
         real(hp), dimension(:), allocatable :: ys    !< y-coordinates of support points
         real(hp), dimension(:), allocatable :: zs    !< z/sigma-coordinates of support points
         integer,  dimension(:), allocatable :: mask  !< support point mask array (for polytime ElementSet)
         integer                             :: n_points !< number of support points
         integer                             :: n_signals !< Number of forcing signals created (at most n_signals==n_points, but warn if n_signals==0)
         character(len=132)                  :: rec      !< a read line
         integer                             :: i, j     !< loop counters
         integer                             :: istat    !< status of read operation
         integer                             :: L        !< helper index
         character(len=4)                    :: tex      !< helper string for constructin file names
         character(len=maxFileNameLen)       :: filename !< helper string containing subprovider file name.
         character(len=maxFileNameLen)       :: plipointlbl   !< temporary name of current pli-point in bct context 
         character(len=maxFileNameLen), &
         &   dimension(:), allocatable       :: plipointlbls  !< user-specified name for all pli-point in bct context 
         character(len=maxFileNameLen)       :: polyline_name !< polyline name read from pli-file 
         logical                             :: exists   !< helper boolian, indicating file existence
         integer                             :: id       !< dummy, catches ids which are not used
         integer                             :: k_yyyymmdd !< calculated Gregorian calender date, serving as reference date
         integer                             :: quantityId, elementSetId, fieldId, itemId, subconverterId, connectionId, BCBlockID
         integer                             :: wind_x, wind_y
         integer                             :: magnitude, discharge, waterlevel, slope, crossing, maxLay
         type(tEcItem), pointer              :: itemPT
         type(tEcItem), pointer              :: itemt3D
         type(tEcItem), pointer              :: sourceItem
         integer,  dimension(:), allocatable :: itemIDList
         integer                             :: vectormax
         logical		                        ::	is_tim, is_cmp, is_tim3d, is_qh
         type(tEcBCBlock), pointer	         :: bcBlockPtr
         type(tEcFileReader), pointer	      :: fileReaderPtr2
         logical :: all_points_are_corr
         logical :: has_label
         integer :: lblstart, lblend                
         !

!        initialization         
         success = .false.
         itemPT => null()
         sourceItem => null()
         itemt3D => null()
         maxlay = 0                 
         vectormax = 1
         !
         ! Skip the lead comment lines plus one additional line.
         do
            read(fileReaderPtr%fileHandle, '(a)', iostat = istat) rec
            if (istat /= 0) then
               call setECMessage("ERROR: ec_provider::ecProviderCreatePolyTimItems: Unexpected end of file.")
               return
            end if
            if (rec(1:1) /= '*') exit
         end do
         ! Read the polyline name from the first line of a tekal-block  
         polyline_name = trim(adjustl(rec))
         ! Read the number of support points.
         read(fileReaderPtr%fileHandle, *, iostat = istat) n_points
         if (istat /= 0) then
            call setECMessage("ERROR: ec_provider::ecProviderCreatePolyTimItems: Unable to read the number of points.")
            return
         end if
         ! Sanity check
         if (n_points < 2) then
            call setECMessage("ERROR: ec_provider::ecProviderCreatePolyTimItems: Less then two support points found.")            
            return
         end if
         ! Read the support point coordinate pairs.
         allocate(xs(n_points))
         allocate(ys(n_points))
         
         allocate(mask(n_points))
         mask = 1
         allocate(itemIDList(n_points))
         itemIDList = ec_undef_int
         allocate(plipointlbls(n_points))
         plipointlbls = ''
         do i=1, n_points
            read(fileReaderPtr%fileHandle,'(a132)', iostat = istat) rec
            if (index(rec,'!')>0) rec = rec(1:index(rec,'!')-1)          ! trim commented  (!)
            if (index(rec,'#')>0) rec = rec(1:index(rec,'#')-1)          ! trim commented  (#)
            if (len(trim(rec)) == 0) cycle                               ! skip empty lines     (or commented-out coordinate pairs) 
            istat = 0
            read(rec, *, iostat = istat) xs(i), ys(i)                    ! see if we can at least read a coordinate pair 
            if (istat /= 0) then
               call setECMessage("   '"//trim(rec)//"'")
               call setECMessage("Unable to read a coordinate pair from file "//trim(fileReaderPtr%fileName))
               return
            end if
            !lblstart = index(rec,'{')
            !lblend = index(rec,'}')
            !plipointlbls(i) = ''
            !if (lblstart>0 .and. lblend>lblstart) then
            !   plipointlbls  = rec(lblstart+1:lblend-1)
            !endif
            lblstart = index(rec,'label=')
            plipointlbls(i) = ''
            if (lblstart>0) then
               read(rec(lblstart+6:len_trim(rec)),*,iostat=istat)  plipointlbls(i)
            endif
         enddo
         call jul2ymd(int(fileReaderPtr%tframe%k_refdate + 2400000.5_hp), k_yyyymmdd)

         ! Construct the poly_tim Item
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. (ecQuantitySetName(instancePtr, quantityId, 'polytim_item'))) return

         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (.not. ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian)) return
         if (.not. ecElementSetSetXArray(instancePtr, elementSetId, xs)) return
         if (.not. ecElementSetSetYArray(instancePtr, elementSetId, ys)) return
         if (.not. ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, n_points)) return     

         fieldId = ecInstanceCreateField(instancePtr)
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. ecItemSetRole(instancePtr, itemId, itemType_target)) return
         if (.not. ecItemSetType(instancePtr, itemId, accessType_fileReader)) return 
         if (.not. ecItemSetQuantity(instancePtr, itemId, quantityId)) return
         if (.not. ecItemSetElementSet(instancePtr, itemId, elementSetId)) return
         if (.not. ecItemSetTargetField(instancePtr, itemId, fieldId)) return

         itemPT => ecSupportFindItem(instancePtr, itemId)

         all_points_are_corr       = .true.
         ! Init BCBlock for (global) qh-bound 
         is_qh = .false. 
         n_signals = 0                                   ! Record whether at least one child provider is created for this polytim.
         bcBlockId = ecInstanceCreateBCBlock(InstancePtr)
         bcBlockPtr=>ecSupportFindBCBlock(instancePtr, bcBlockId)
         write(plipointlbl,'(a)') trim(polyline_name)   
         call str_upper(quantityname)
         if (ecProviderInitializeBCBlock(InstancePtr, bcBlockId, k_yyyymmdd, fileReaderPtr%tframe%k_timezone, fileReaderPtr%tframe%k_timestep_unit,   &
                                    id, bctfilename, quantityname, plipointlbl, fileReaderPtr%tframe%dtnodal, istat)) then
            is_qh = (bcBlockPtr%func == BC_FUNC_QHTABLE) ! if a polylinename exist as a label without a number
                                                         ! it might refer to a qh forcing 
            ! SHRL: TODO: check of this is always the case
            n_signals = 1                ! ????? ????  RL: moet n_signals hier niet op 1 gezet worden of iets anders ????
         else                            ! .not.is_qh
            n_signals = 0
            do i=1, n_points
               is_tim = .false.
               is_cmp = .false.
               is_tim3d = .false.
               ! Process a *.tim file.
               bcBlockId = ecInstanceCreateBCBlock(InstancePtr) 
               bcBlockPtr=>ecSupportFindBCBlock(instancePtr, bcBlockId)
               ! id van de filereader

               ! plipoint labels read from the third column in the pli-file. Currently this goes wrong if in the test third-column labels are not unique 
               if (len_trim(plipointlbls(i))==0) then 
                  write(plipointlbl,'(a,i4.4)') trim(polyline_name)//'_', i     ! using polyline_name from tekal-block 
                  has_label = .False. 
               else
                  plipointlbl = trim(plipointlbls(i))
                  has_label = .True. 
               endif
               
               if (.not. ecProviderInitializeBCBlock(InstancePtr, bcBlockId, k_yyyymmdd, fileReaderPtr%tframe%k_timezone, fileReaderPtr%tframe%k_timestep_unit,   &
                                     id, bctfilename, quantityname, plipointlbl, fileReaderPtr%tframe%dtnodal, istat)) then
                  !call setECMessage("WARNING: ec_provider::ecProviderPolyTimItems: Error initializing EC Block.")
                  mask(i) = 0
                  mask(i) = 0
                  if (has_label) then               ! for explicitly labelled pli-points, require data 
                     call setECMessage("BC-File "//trim(bctfilename)//" contains no data for labelled point '" &
                                                 //trim(plipointlbl)//"' and quantity '"//trim(quantityname)//"' (required).")
                     return
                  endif 
                  cycle
               endif
               if (bcBlockPtr%func == BC_FUNC_HARMOCORR .or. bcBlockPtr%func == BC_FUNC_ASTROCORR) then
                  ecAtLeastOnePointIsCorrection = .true. ! TODO: Refactor this shortcut (UNST-180).
!                 n_signals = n_signals + 1 
                  cycle
               else
                  all_points_are_corr = .false.
               endif
               is_tim = (bcBlockPtr%func == BC_FUNC_TSERIES)
               is_cmp = ((bcBlockPtr%func == BC_FUNC_HARMONIC) .or. (bcBlockPtr%func == BC_FUNC_ASTRO))
               is_tim3d = (bcBlockPtr%func == BC_FUNC_TIM3D)

               if (.not. ecProviderConnectSourceItemsToTargets(instancePtr, is_tim, is_cmp, is_tim3d, id, itemId, i,        &
                                                        n_signals, maxlay, itemIDList,qname=quantityname)) then
                  !
                  ! No sub-FileReader made.
                  mask(i) = 0
               endif
            end do               ! loop over support points
         endif                   ! switch between qh/cmp/tim

         if (ecAtLeastOnePointIsCorrection) then  ! TODO: Refactor this shortcut (UNST-180).
             if (all_points_are_corr) then
                success = .true.
             else
                ! TODO: error: bc file currently should only contain corrections
             endif
             return
         endif

         if (n_signals <= 0) then
            call setECMessage("    for polyline "//trim(polyline_name)//" and quantity "//trim(quantityname)//".")
            call setECMessage("No signals for polyline file "//trim(fileReaderPtr%filename)//" found in "//trim(bctfilename))
            return
         end if

         if (is_qh) then
            ! Construct a new Converter.
            subconverterId = ecInstanceCreateConverter(instancePtr)
            ! Determine the source Items.
            discharge = ecFileReaderFindItem(instancePtr, id, 'discharge')
            waterlevel = ecFileReaderFindItem(instancePtr, id, 'waterlevel')
            slope = ecFileReaderFindItem(instancePtr, id, 'slope')
            crossing = ecFileReaderFindItem(instancePtr, id, 'crossing')
            if (discharge /= ec_undef_int .and. waterlevel /= ec_undef_int .and. &
               slope /= ec_undef_int .and. crossing /= ec_undef_int) then
               !do i=1, n_points ! commented: only one value per polyline
               ! Initialize the new Converter.
               if (.not. (ecConverterSetType(instancePtr, subconverterId, convType_qhtable) .and. &
                          ecConverterSetOperand(instancePtr, subconverterId, operand_replace_element) .and. &
                          ecConverterSetInterpolation(instancePtr, subconverterId, interpolate_passthrough) .and. &
                          ecConverterSetElement(instancePtr, subconverterId, 1))) return ! set to 1 from i: only one value per polyline
               ! Construct a new Connection.
               connectionId = ecInstanceCreateConnection(instancePtr)
               if (.not. ecConnectionSetConverter(instancePtr, connectionId, subconverterId)) return
               ! Initialize the new Connection.
               if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, discharge)) return
               if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, waterlevel)) return
               if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, slope)) return
               if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, crossing)) return
               if (.not. ecConnectionAddTargetItem(instancePtr, connectionId, itemId)) return
               if (.not. ecItemAddConnection(instancePtr, itemId, connectionId)) return
               n_signals = 1
               !end do
            end if
         endif

         ! itemID refers to the source item (providing to the polytim item) for the last support point we came across in the above loop.
         if (.not. ecProvider3DVectmax(instancePtr, itemPT, mask ,maxlay, n_points, itemIDList)) return 

         ! Since the main FileReader's Item is a target, the TimeFrame is not set.
         ! Add successfully created source Item to the main FileReader
         if (.not. ecFileReaderAddItem(instancePtr, fileReaderPtr%id, itemPT%id)) return 
         !
         ! close pli file
         close(fileReaderPtr%fileHandle, iostat = istat)
         success = .true. 
      end function ecProviderCreatePolyTimItemsBC
      
      !TODO: pass vertical interpolation type from source item to target
      function ecProvider3DVectmax(instancePtr, itemPT, mask ,maxlay, n_points, itemIDList) result (success)
      implicit none

      logical                                   :: success 
 
      type(tEcInstance),   pointer              :: instancePtr   !< intent(in)
      type(tEcItem), pointer                    :: itemPT
      integer, intent(in)                       :: mask(:)
      integer, intent(in)                       :: maxlay
      integer, intent(in)                       :: n_points
      integer,  dimension(:), allocatable       :: itemIDList
        
      real(hp), dimension(:), allocatable :: zs    !< z/sigma-coordinates of support points
      integer                             :: i, magnitude, vectormax, zInterpolationType
      type(tEcItem), pointer              :: itemt3D, itemSRC
      integer                             :: elementSetId
      integer                             :: fieldId
      integer   :: iconn, isrc
      vectormax = 1
      zInterpolationType = 0

      fieldId = itemPT%targetFieldPtr%id
      elementSetId = itemPT%elementSetPtr%id 
      success = ecElementSetSetMaskArray(instancePtr, elementSetId, mask)

      if ( maxlay > 0 ) then ! t3D type
         allocate(zs(maxlay*n_points))
         zs = ec_undef_hp
         do i=1, n_points
            magnitude = itemIDList(i)
            if ( magnitude /= ec_undef_int ) then
               Itemt3D => ecSupportFindItem(instancePtr, magnitude)
               zs((i-1)*maxlay+1:(i-1)*maxlay+size(itemt3D%elementSetPtr%z)) = itemt3D%elementSetPtr%z
            end if
         end do
         if (success) success = ecElementSetSetZArray(instancePtr, elementSetId, zs)
         if (.not. ecFieldCreate1dArray(instancePtr, fieldId, n_points*maxlay)) then
            success = .false.
         end if
      endif 
      ! Determine vectormax.                                            
      itemPT%quantityptr%vectormax = 0
      do iconn= 1,itemPT%nConnections
         if (itemPT%connectionsptr(iconn)%ptr%nSourceItems>0) then                        ! assume those of the first source item .... 
            itemSRC => itemPT%connectionsptr(iconn)%ptr%sourceItemsptr(1)%ptr
            zInterpolationType = itemSRC%quantityptr%zInterpolationType
            vectormax = itemSRC%quantityptr%vectormax
            itemPT%quantityptr%zInterpolationType = zInterpolationType
            itemPT%quantityptr%vectormax = vectormax
         endif 
         do isrc = 1, itemPT%connectionsptr(iconn)%ptr%nSourceItems                       ! ... check with all followinf source items 
            itemSRC => itemPT%connectionsptr(iconn)%ptr%sourceItemsptr(isrc)%ptr
            if (itemPT%quantityptr%vectormax /= vectormax) then 
               call setECMessage("ERROR: ec_provider::ecProvider3DVectmax: Polytim subitems have different vectormax.")
               continue 
            endif 
            if (maxlay>0) then
               if (itemPT%quantityptr%zInterpolationType /= zInterpolationType) then 
                  call setECMessage("ERROR: ec_provider::ecProvider3DVectmax: Polytim subitems have different interpolation types.")
                  continue     
               endif 
            endif 
         enddo 
      enddo 
      
      ! Allocate the target Field.
      if (.not. ecFieldCreate1dArray(instancePtr, fieldId, n_points*max(maxlay,1)*vectormax)) then
         success = .false.
      end if
      end function ecProvider3DVectmax

!==============================================================================================================
      
      function ecProviderConnectSourceItemsToTargets(instancePtr, is_tim, is_cmp, is_tim3d, fileReaderId, targetItemId, targetIndex, n_signals, maxlay, itemIDList, qname) result(itemFound)
         logical :: itemFound
         type(tEcInstance),   pointer              :: instancePtr   !< intent(in)
         integer :: fileReaderId ! file reader id
         integer :: targetItemId ! target item id
         integer :: targetIndex ! index in target item's values
         integer :: n_signals, maxlay ! INOUT
         integer,  dimension(:) :: itemIDList  ! INOUT
         logical ::	is_tim, is_cmp, is_tim3d
         integer :: subconverterId, magnitude, j, connectionId, nr_fourier_items, anItemId
         type(tEcItem), pointer :: itemt3D
         character(len=*), optional :: qname
         
         itemFound = .false.

         if (is_tim) then
            ! Construct a new Converter.
            subconverterId = ecInstanceCreateConverter(instancePtr)
            ! Determine the source Items.
            if (present(qname)) then 
                magnitude = ecFileReaderFindItem(instancePtr, fileReaderId, trim(qname))
            else 
                magnitude = ecFileReaderFindItem(instancePtr, fileReaderId, 'uniform_item')
            end if

            if (magnitude /= ec_undef_int) then
               ! Initialize the new Converter.
               if (.not. (ecConverterSetType(instancePtr, subconverterId, convType_uniform) .and. &
                          ecConverterSetOperand(instancePtr, subconverterId, operand_replace_element) .and. &
                          ecConverterSetInterpolation(instancePtr, subconverterId, interpolate_timespace) .and. &
                          ecConverterSetElement(instancePtr, subconverterId, targetIndex))) return
               ! Construct a new Connection.
               connectionId = ecInstanceCreateConnection(instancePtr)
               if (.not. ecConnectionSetConverter(instancePtr, connectionId, subconverterId)) return
               ! Initialize the new Connection.
               if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, magnitude)) return
               if (.not. ecConnectionAddTargetItem(instancePtr, connectionId, targetItemId)) return
               if (.not. ecItemAddConnection(instancePtr, targetItemId, connectionId)) return
               n_signals = n_signals + 1
            else 
                ! error handling
            end if
            itemFound = .true.
         end if
         if (is_cmp) then
            ! Construct a new Converter.
            subconverterId = ecInstanceCreateConverter(instancePtr)
            ! Determine the source Items.
            nr_fourier_items = ecFileReaderGetNumberOfItems(instancePtr, fileReaderId)
            ! Initialize the new Converter.
            if (.not. (ecConverterSetType(instancePtr, subconverterId, convType_fourier) .and. &
                       ecConverterSetOperand(instancePtr, subconverterId, operand_replace_element) .and. &
                       ecConverterSetInterpolation(instancePtr, subconverterId, interpolate_passthrough) .and. &
                       ecConverterSetElement(instancePtr, subconverterId, targetIndex))) return
            ! Construct a new Connection.
            connectionId = ecInstanceCreateConnection(instancePtr)
            if (.not. ecConnectionSetConverter(instancePtr, connectionId, subconverterId)) return
            ! Initialize the new Connection.      
            do j=1, nr_fourier_items
               anItemId = ecFileReaderGetItem(instancePtr, fileReaderId, j) 
               if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, anItemId)) return
            end do
            if (.not. ecConnectionAddTargetItem(instancePtr, connectionId, targetItemId)) return
            if (.not. ecItemAddConnection(instancePtr, targetItemId, connectionId)) return
            n_signals = n_signals + 1
            itemFound = .true.
         end if
         if (is_tim3d) then
            ! Construct a new Converter.
            subconverterId = ecInstanceCreateConverter(instancePtr)
            ! Determine the source Items.
            magnitude = ecFileReaderFindItem(instancePtr, fileReaderId, 'quant')
            ! update maximum number of layers
            Itemt3D => ecSupportFindItem(instancePtr, magnitude)
            maxlay = max(maxlay,size(Itemt3D%elementSetPtr%z))
            ItemIDList(targetIndex) = magnitude
            ! Initialize the new Converter.
            if (.not. (ecConverterSetType(instancePtr, subconverterId, convType_uniform) .and. &
                       ecConverterSetOperand(instancePtr, subconverterId, operand_replace_element) .and. &
                       ecConverterSetInterpolation(instancePtr, subconverterId, interpolate_time) .and. &
                       ecConverterSetElement(instancePtr, subconverterId, targetIndex))) return
            ! Construct a new Connection.
            connectionId = ecInstanceCreateConnection(instancePtr)
            if (.not. ecConnectionSetConverter(instancePtr, connectionId, subconverterId)) return
            ! Initialize the new Connection.
            if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, magnitude)) return
            if (.not. ecConnectionAddTargetItem(instancePtr, connectionId, targetItemId)) return
            if (.not. ecItemAddConnection(instancePtr, targetItemId, connectionId)) return
            n_signals = n_signals + 1
            itemFound = .true.
         endif
         
      end function ecProviderConnectSourceItemsToTargets
     ! itemIDList

      ! =======================================================================

      !> Create source Items and their contained types, based on a spiderweb file's header.
      !! meteo1.f90: reaspwheader
      function ecProviderCreateSpiderwebItems(instancePtr, fileReaderPtr) result(success)
         logical                      :: success       !< function status
         type(tEcInstance),   pointer :: instancePtr   !< intent(in)
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
         !
         integer                   :: quantityId      !< helper variable 
         integer                   :: elementSetId    !< helper variable 
         integer                   :: field0Id        !< helper variable 
         integer                   :: field1Id        !< helper variable 
         integer                   :: itemId          !< helper variable 
         integer                   :: n_cols          !< helper variable
         integer                   :: n_rows          !< helper variable
         real(hp)                  :: missingValue    !< helper variable
         real(hp)                  :: radius          !< helper variable
         character(len=maxNameLen) :: radius_unit     !< helper variable
         type(tEcItem), pointer    :: item1           !< Item containing quantity1
         type(tEcItem), pointer    :: item2           !< Item containing quantity2
         type(tEcItem), pointer    :: item3           !< Item containing quantity3
         character(len=maxNameLen) :: rec             !< helper variable
         !
         success = .false.
         item1 => null()
         item2 => null()
         item3 => null()
         !
         rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'n_cols')
         read(rec, *) n_cols
         n_cols = n_cols + 1 ! an additional column for 360 == 0 degrees
         rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'n_rows')
         read(rec, *) n_rows
         n_rows = n_rows + 1 ! an additional row for the eye
         rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'NODATA_value') 
         read(rec, *) missingValue
         rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'spw_radius')
         read(rec, *) radius
         rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'spw_rad_unit')
         read(rec, *) radius_unit
         !
         ! One common ElementSet.
         elementSetId = ecInstanceCreateElementSet(instancePtr)
         if (.not. (ecElementSetSetType(instancePtr, elementSetId, elmSetType_spw) .and. &
                    ecElementSetSetRadius(instancePtr, elementSetId, radius, radius_unit) .and. &
                    ecElementSetSetRowsCols(instancePtr, elementSetId, n_rows, n_cols))) then
            success = .false.
         end if
         !
         ! ===== quantity1: wind_speed =====
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. (ecQuantitySetName(instancePtr, quantityId, 'windspeed') .and. &
                    ecQuantitySetUnits(instancePtr, quantityId, trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit1'))))) then
            success = .false.
         end if
         field0Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, n_cols*n_rows) .and. &
                    ecFieldSetMissingValue(instancePtr, field0Id, missingValue))) then
            success = .false.
         end if
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, n_cols*n_rows) .and. &
                    ecFieldSetMissingValue(instancePtr, field1Id, missingValue))) then
            success = .false.
         end if
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. ( ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                     ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                     ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                     ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                     ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                     ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
            success = .false.
         else
            item1 => ecSupportFindItem(instancePtr, itemId)
         end if
         ! ===== quantity2: wind_from_direction =====
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. (ecQuantitySetName(instancePtr, quantityId, 'winddirection') .and. &
                    ecQuantitySetUnits(instancePtr, quantityId, trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit2'))))) then
            success = .false.
         end if
         field0Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, n_cols*n_rows) .and. &
                    ecFieldSetMissingValue(instancePtr, field0Id, missingValue))) then
            success = .false.
         end if
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, n_cols*n_rows) .and. &
                    ecFieldSetMissingValue(instancePtr, field1Id, missingValue))) then
            success = .false.
         end if
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. ( ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                     ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                     ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                     ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                     ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                     ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
            success = .false.
         else
            item2 => ecSupportFindItem(instancePtr, itemId)
         end if
         ! ===== quantity3: p_drop =====
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. (ecQuantitySetName(instancePtr, quantityId, 'p_drop') .and. &
                    ecQuantitySetUnits(instancePtr, quantityId, trim(ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'unit3'))))) then
            success = .false.
         end if
         field0Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, n_cols*n_rows) .and. &
                    ecFieldSetMissingValue(instancePtr, field0Id, missingValue))) then
            success = .false.
         end if
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, n_cols*n_rows) .and. &
                    ecFieldSetMissingValue(instancePtr, field1Id, missingValue))) then
            success = .false.
         end if
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. ( ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                     ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                     ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                     ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                     ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                     ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
            success = .false.
         else
            item3 => ecSupportFindItem(instancePtr, itemId)
         end if
         ! ===== finish initialization of Fields =====
         ! Read the first two records into tEcItem%sourceT0FieldPtr and tEcItem%sourceT1FieldPtr.
         success = ecSpiderAndCurviAndArcinfoReadToBody(fileReaderPtr%fileHandle)
         if (success) then
            success = ecSpiderwebReadBlock(fileReaderPtr, item1, item2, item3, 0, n_cols, n_rows)
         end if
         if (success) then
            success = ecSpiderwebReadBlock(fileReaderPtr, item1, item2, item3, 1, n_cols, n_rows)
         end if
        ! Add successfully created source Items to the FileReader
         if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item1%id)
         if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item2%id)
         if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item3%id)
      end function ecProviderCreateSpiderwebItems
      
      ! =======================================================================
      function ecProviderCreateTimeInterpolatedItem(instancePtr, sourceItemId, tgtNdx) result(itemId)
          use m_ec_item
          use m_ec_converter,  only: ecConverterSetType, ecConverterSetInterpolation, ecConverterSetOperand, ecConverterSetElement
          use m_ec_instance,   only: ecInstanceCreateConverter, ecInstanceCreateConnection, ecInstanceCreateItem, ecInstanceCreateField, ecInstanceCreateQuantity
          use m_ec_connection, only: ecConnectionAddTargetItem, ecConnectionAddSourceItem, ecConnectionSetConverter 
          use m_ec_quantity,   only: ecQuantitySetName
          use m_ec_field,      only: ecFieldCreate1dArray
          use m_ec_item


          type(tEcInstance), pointer    :: instancePtr    !< EC-instance
          integer, intent(in)           :: sourceItemId   !< Source item id, before temporal interpolation
          integer, intent(in), optional :: tgtNdx         !< Optional target index, 1 is assumed as default
          integer                       :: targetItemId   !< Target item id, after temporal interpolation
          integer                       :: itemId         !< returned  target item ID, if successful, otherwise -1 
          integer                       :: convertId 
          type(tECItem), pointer        :: sourceItemPtr => null() 
          type(tECItem), pointer        :: targetItemPtr => null()
          character(len=:), allocatable :: quantityName
          integer                       :: arraySize

          integer :: targetIndex 
          integer :: converterId, connectionId, quantityId, elementSetId, fieldId
          
          if (present(tgtNdx)) then 
             targetIndex = tgtNdx
          else
             targetIndex = 1
          end if 

          sourceItemPtr => ecSupportFindItem(instancePtr, sourceItemId)

          ! TODO: create target item:
          !       . elementset-name = source_item's elementset-name
          !       . quantity-name = source_item's quantity-name + '-interpolated'
          itemId = -1 

          ! Set up the target item 
          targetItemId = ecInstanceCreateItem(instancePtr)
          fieldId = ecInstanceCreateField(instancePtr)

          arraySize = size(sourceItemPtr%sourceT0FieldPtr%arr1d)
          if (.not. (ecFieldCreate1dArray(instancePtr, fieldId, arraySize))) return

          if (.not. ecItemSetRole(instancePtr, targetItemId, itemType_target)) return
          if (.not. ecItemSetTargetField(instancePtr, targetItemId, fieldId)) return
          if (.not. ecItemSetType(instancePtr, targetItemId, accessType_evaluate)) return 
          quantityId = ecInstanceCreateQuantity(instancePtr)
          quantityName = trim(sourceItemPtr%quantityPtr%name)
          if (.not. ecItemSetQuantity(instancePtr, targetItemId, quantityId)) return
          if (.not. (ecQuantitySetName(instancePtr, quantityId, quantityName//'_interpolated'))) return
          elementSetId = sourceItemPtr%elementSetPtr%id
          if (.not. ecItemSetElementSet(instancePtr, targetItemId, elementSetId)) return

          ! Construct a new Converter.
          converterId = ecInstanceCreateConverter(instancePtr)

          ! Initialize the new Converter.
          if (.not. (ecConverterSetType(instancePtr, converterId, convType_uniform))) return
          if (.not. (ecConverterSetOperand(instancePtr, converterId, operand_replace_element))) return
          if (.not. (ecConverterSetInterpolation(instancePtr, converterId, interpolate_time_extrapolation_ok))) return
          if (.not. (ecConverterSetElement(instancePtr, converterId, targetIndex))) return

          ! Construct a new Connection.
          connectionId = ecInstanceCreateConnection(instancePtr)
          if (.not. ecConnectionSetConverter(instancePtr, connectionId, converterId)) return

          ! Initialize the new Connection.
          if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, sourceItemId)) return
          if (.not. ecConnectionAddTargetItem(instancePtr, connectionId, targetItemId)) return
          if (.not. ecItemAddConnection(instancePtr, targetItemId, connectionId)) return
          itemId = targetItemId
      end function ecProviderCreateTimeInterpolatedItem

      ! =======================================================================
      
      !> Create source Items and their contained types, based on a NetCDF file's header.
      function ecProviderCreateNetcdfItems(instancePtr, fileReaderPtr, quantityName) result(success)
      use transform_poleshift
      use m_ec_message
      use m_alloc 
      implicit none 
         logical                           :: success           !< function status
         type(tEcInstance),     pointer    :: instancePtr       !< intent(in)
         type(tEcFileReader),   pointer    :: fileReaderPtr     !< intent(inout)
         character(len=maxNameLen), intent(in) :: quantityName  !< name of quantity to read
         character(len=maxMessageLen)          :: message
         !
         integer                                                 :: ierror                !< return value of NetCDF function calls

         integer                                                 :: idvar(10)
         integer                                                 :: n_quantity            !< number of requested variables from the netcdf-file 
         
         integer                                                 :: idvar_coord           !< id as obtained from NetCDF
         integer                                                 :: idvar_time            !< id as obtained from NetCDF
         integer                                                 :: ndims, ifgd, isgd     !< helper variables
         integer,                      dimension(:), allocatable :: dimids                !< ids of a variable's dimensions
         integer,                      dimension(:), allocatable :: dimids_tmp            !< temporary ids of a variable's dimensions (compare vars)
         integer,                      dimension(:), allocatable :: coord_ids             !< helper variable
         character(len=NF90_MAX_NAME), dimension(:), allocatable :: names             !< helper variable, containing dimension names
         integer                                                 :: i,j                   !< loop counter
         integer                                                 :: grid_mapping_id       !< id of the applied grid mapping 
         integer                                                 :: fgd_id                !< var_id for elementset X or latitude
         integer                                                 :: sgd_id                !< var_id for elementset Y or longitude
         integer                                                 :: fgd_grid_type         !< helper variable for consistency check on grid_type
         integer                                                 :: sgd_grid_type         !< helper variable for consistency check on grid_type
         integer                                                 :: grid_type             !< elmSetType enum
         integer                                                 :: fgd_size              !< number of grid points in first dimension
         integer                                                 :: sgd_size              !< number of grid points in second dimension
         real(hp)                                                :: gnplon,gnplat         !< coordinates of shifted north pole obtained from gridmapping 
         real(hp)                                                :: gsplon,gsplat         !< coordinates of shifted south pole obtained from gridmapping 
         real(hp),                   dimension(:,:), allocatable :: fgd_data              !< coordinate data along first dimension's axis
         real(hp),                   dimension(:),   allocatable :: fgd_data_1d           !< coordinate data along first dimension's axis
         real(hp),                   dimension(:),   allocatable :: fgd_data_trans        !< coordinate data along first dimension's axis transformed, rotating pole 
         real(hp),                   dimension(:,:), allocatable :: sgd_data              !< coordinate data along second dimension's axis
         real(hp),                   dimension(:),   allocatable :: sgd_data_1d           !< coordinate data along second dimension's axis
         real(hp),                   dimension(:),   allocatable :: sgd_data_trans        !< coordinate data along first dimension's axis transformed, rotating pole
         real(hp),                   dimension(:),   allocatable :: pdiri                 !< 
         real(hp)                                                :: fdg_miss              !< missing data value in first dimension
         real(hp)                                                :: sdg_miss              !< missing data value in second dimension
         character(len=NF90_MAX_NAME)                            :: grid_mapping          !< name of the applied grid mapping 
         character(len=NF90_MAX_NAME)                            :: units                 !< helper variable for variable's units
         character(len=NF90_MAX_NAME)                            :: coord_name            !< helper variable
         character(len=NF90_MAX_NAME)                            :: coord_name_tmp        !< helper variable
         character(len=NF90_MAX_NAME), dimension(:), allocatable :: coord_names           !< helper variable
         character(len=NF90_MAX_NAME)                            :: name                  !< helper variable
         character(len=NF90_MAX_NAME), dimension(15)             :: ncvarnames            !< helper variable : temp. list of variable names to search for in netcdf  
         integer                                                 :: quantityId            !< helper variable 
         integer                                                 :: elementSetId          !< helper variable 
         integer                                                 :: field0Id              !< helper variable 
         integer                                                 :: field1Id              !< helper variable 
         integer                                                 :: itemId                !< helper variable 
         integer                                                 :: istat                 !< helper variable 
         type(tEcItem),              pointer                     :: itemPtr               !< Item containing quantity
         integer                                                 :: t0t1                  !< t0 / t1 switch
         logical                                                 :: dummy                 !< temp
         integer,                    dimension(:), allocatable   :: dim_sizes             !< helper variable
         character(len=20)                                       :: attstr 
         logical                                                 :: rotate_pole
         integer                                                 :: nvar, ivar            !< number/loopvariable of varids in this netcdf file 
         double precision                                        :: PI 
         character(len=NF90_MAX_NAME)                            :: expected_fgd, expected_sgd
         
         !
         success = .false.
         itemPtr => null()
         fgd_grid_type = ec_undef_int
         sgd_grid_type = ec_undef_int
         grid_type = ec_undef_int
         units  = ''
         coord_name  = ''
         name  = ''
         ndims = 0

         ! =============================================================================
         ! Find the Quantity corresponding to quantityName. (configurable in the future)
         ! =============================================================================
         ! TODO: Check on standard variable name.
         ! TODO: Check on variable's long_name.
         ! Check for variable named "rainfall". FEWS calls this "Rainfall"

         ! quantityName -> set of netcdf names -> set of varids 
         ! TODO: map var id's to standard names in the netcdf (loop)
         ! TODO: quantity names ARE standardnames 
         ! TODO: loop over id's 
         
         ! Make a list of standard names of variables available in the netcdf file 
         nvar = 0 
         ierror = nf90_inquire(fileReaderPtr%fileHandle, nvariables = nvar)
         if (nvar>0) then 
            allocate(fileReaderPtr%standard_names(nvar))          ! Note: one of these may be obsolete (if we only check standard names)
            allocate(fileReaderPtr%variable_names(nvar))
            fileReaderPtr%standard_names = ''
            fileReaderPtr%variable_names = ''
            do ivar = 1,nvar 
               ierror = nf90_get_att(fileReaderPtr%fileHandle, ivar, 'standard_name', fileReaderPtr%standard_names(ivar))
               ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, ivar, name=fileReaderPtr%variable_names(ivar))
            enddo 
         else 
            ! no variables in the file or netcdf inquiry error .... handle exception 
            return
         endif 

         ! ncvarnames now provided with the standard names, used to label the netcdf quantities as well as search for the varids 
         ! (already stored in the filereader)
         ! For now assuming the MATROOS-definitions of variables, listed at 
         ! https://publicwiki.deltares.nl/display/NETCDF/Matroos+Standard+names
         ncvarnames(:) = '' 
         idvar = -1 
         select case (trim(quantityName))
         case ('rainfall') 
            ncvarnames(1) = 'precipitation' 
         case ('windx') 
            ncvarnames(1) = 'eastward_wind'
         case ('windy') 
            ncvarnames(1) = 'northward_wind'
         case ('windxy') 
            ncvarnames(1) = 'eastward_wind'
            ncvarnames(2) = 'northward_wind'
         case ('atmosphericpressure') 
            ncvarnames(1) = 'air_pressure'
         case ('airpressure_windx_windy') 
            ncvarnames(1) = 'air_pressure'
            ncvarnames(2) = 'eastward_wind'
            ncvarnames(3) = 'northward_wind'
         end select 

         do i = 1, count(ncvarnames>' ')
            do ivar = 1,nvar                                                           ! Find the varid 
               if (fileReaderPtr%standard_names(ivar)==ncvarnames(i) .or. fileReaderPtr%variable_names(ivar)==ncvarnames(i)) then 
                  idvar(i) = ivar
                  exit
               endif 
            enddo 
            if (ivar>nvar) then 
               write (message,'(a)') "Variable '"//trim(ncvarnames(i))//"' not found in NetCDF file '"//trim(fileReaderPtr%filename)
               call setECMessage(message)
               return
            endif 
            ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, idvar(i), ndims=ndims)              ! get dimensions 
            ierror = nf90_get_att(fileReaderPtr%fileHandle, idvar(i), "coordinates", coord_name)         ! get coordinates attribute 
            if (allocated(coord_names)) deallocate(coord_names)
            allocate(coord_names(ndims))
            coord_names = ''
            read(coord_name, *,iostat=istat) ( coord_names(j), j=1,ndims )
            ! As temporary leniency, we will tolerate refrainment of time coordinate specification.
            if (istat .ne. 0) then                                                                       ! dit is superlelijk, weghalen 
               coord_names(ndims) = 'time'
            endif 
            if (allocated(dimids)) deallocate (dimids)
            allocate(dimids(ndims))
            ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, idvar(i), dimids=dimids)

            grid_mapping=''
            rotate_pole=.False. 
            ierror = nf90_get_att(fileReaderPtr%fileHandle, idvar(i), "grid_mapping", grid_mapping)      ! check if there is a gridmapping variable for this var 
            if (len_trim(grid_mapping)>0) then
               ierror = nf90_inq_varid(fileReaderPtr%fileHandle, grid_mapping, grid_mapping_id)
               if (ierror == NF90_NOERR) then 
                  gsplon = -999.9
                  gsplat = -999.9
                  gnplon = -999.9
                  gnplat = -999.9
                  attstr=''
                  ierror = nf90_get_att(fileReaderPtr%fileHandle, grid_mapping_id, "grid_north_pole_longitude", attstr)
                  read(attstr,*,iostat=ierror) gnplon
                  attstr=''
                  ierror = nf90_get_att(fileReaderPtr%fileHandle, grid_mapping_id, "grid_north_pole_latitude", attstr)
                  read(attstr,*,iostat=ierror) gnplat
                  attstr=''
                  ierror = nf90_get_att(fileReaderPtr%fileHandle, grid_mapping_id, "grid_south_pole_longitude", attstr)
                  read(attstr,*,iostat=ierror) gsplon
                  attstr=''
                  ierror = nf90_get_att(fileReaderPtr%fileHandle, grid_mapping_id, "grid_south_pole_latitude", attstr)
                  read(attstr,*,iostat=ierror) gsplat
                  if ((gnplon > -900.d0) .and. (gnplat > -900.d0)) then         ! northpole given 
                     gsplon =  gnplon + 180.d0 
                     gsplat = -gnplat
                  endif 
                  if ((gsplon > -900.d0) .and. (gsplat > -900.d0)) then         ! southpole given 
                     rotate_pole = .True.
                  endif 
               endif 
            endif 
            if (allocated(coord_ids)) deallocate(coord_ids)
            allocate(coord_ids(ndims))
            sgd_id = -1 
            fgd_id = -1 
            if (ndims > 1) then
               do j=1, ndims
                  ierror = nf90_inq_varid(fileReaderPtr%fileHandle, coord_names(j), coord_ids(j))
                  name = '' ! NetCDF fails to overwrite the entire string, so re-initialize each iteration.
                  ierror = nf90_get_att(fileReaderPtr%fileHandle, coord_ids(j), "standard_name", name)
                  ! If the instance is in carthesian mode, ignore sferic coordinates in source, latitude and longitude
                  ! If the instance is in sferic mode, ignore carthesian coordinates in source, projection coordinates 
                  if (instancePtr%coordsystem == EC_COORDS_CARTHESIAN) then 
                     expected_fgd='projection_x_coordinate'
                     expected_sgd='projection_y_coordinate'
                     if (strcmpi(name, expected_fgd)) then
                        fgd_id = coord_ids(j)
                        ierror = nf90_inquire_dimension(fileReaderPtr%fileHandle, dimids(j), len=fgd_size)        ! fgd is always x 
                        fgd_grid_type = elmSetType_cartesian
                     end if
                     if (strcmpi(name, expected_sgd)) then
                        sgd_id = coord_ids(j)
                        ierror = nf90_inquire_dimension(fileReaderPtr%fileHandle, dimids(j), len=sgd_size)        ! sgd is always y
                        sgd_grid_type = elmSetType_cartesian
                     end if
                  else if (instancePtr%coordsystem == EC_COORDS_SFERIC) then 
                     expected_fgd='longitude'
                     expected_sgd='latitude'
                     if (strcmpi(name, expected_fgd)) then
                        fgd_id = coord_ids(j)
                        ierror = nf90_inquire_dimension(fileReaderPtr%fileHandle, dimids(j), len=sgd_size)
                        fgd_grid_type = elmSetType_spheric
                     end if
                     if (strcmpi(name, expected_sgd)) then
                        sgd_id = coord_ids(j)
                        ierror = nf90_inquire_dimension(fileReaderPtr%fileHandle, dimids(j), len=fgd_size)
                        sgd_grid_type = elmSetType_spheric
                     end if
                  end if
               end do
               success = .True.
               if (sgd_id<=0) then 
                  call setECMessage("  Expecting '"//trim(expected_sgd)//"'.")
                  call setECMessage("Second spatial coordinate not found in "//trim(fileReaderPtr%filename)//    &
                        ", quantity '"//trim(ncvarnames(i))//"'.")
                  success = .False.
               endif 
               if (fgd_id<=0) then 
                  call setECMessage("  Expecting '"//trim(expected_fgd)//"'.")
                  call setECMessage("First spatial coordinate not found in "//trim(fileReaderPtr%filename)//    &
                        ", quantity '"//trim(ncvarnames(i))//"'.")
                  success = .False.
               endif 
               if (.not.success) return 
               if (fgd_grid_type /= sgd_grid_type) then
                  call setECMessage("Mismatching grid types between dimensions in "//trim(fileReaderPtr%filename)// &
                        ", quantity '"//trim(ncvarnames(i))//"'.")
                  return
               endif
               grid_type = fgd_grid_type
            end if

            ! =========================================
            ! Create the ElementSet for this quantity
            ! =========================================
            elementSetId = ecInstanceCreateElementSet(instancePtr)
            if (grid_type == ec_undef_int) then
               dummy = ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, 0)
            else
               if (allocated(fgd_data)) deallocate(fgd_data)
               if (allocated(sgd_data)) deallocate(sgd_data)
               if (allocated(fgd_data_1d)) deallocate(fgd_data_1d)
               if (allocated(sgd_data_1d)) deallocate(sgd_data_1d)
               ierror = nf90_inquire_variable(fileReaderPtr%fileHandle,fgd_id,ndims=ndims)

               allocate(fgd_data(fgd_size,sgd_size), sgd_data(fgd_size,sgd_size))
               allocate(fgd_data_1d(fgd_size*sgd_size), sgd_data_1d(fgd_size*sgd_size))
               if (ndims==2) then 
                  ierror = nf90_get_var(fileReaderPtr%fileHandle, fgd_id, fgd_data, start=(/1,1/), count=(/fgd_size,sgd_size/))
                  ierror = nf90_get_var(fileReaderPtr%fileHandle, sgd_id, sgd_data, start=(/1,1/), count=(/fgd_size,sgd_size/))
               else if (ndims==1) then 
                  ierror = nf90_get_var(fileReaderPtr%fileHandle, fgd_id, fgd_data_1d(1:fgd_size), start=(/1/), count=(/fgd_size/))
                  ierror = nf90_get_var(fileReaderPtr%fileHandle, sgd_id, sgd_data_1d(1:sgd_size), start=(/1/), count=(/sgd_size/))
                  ! Make a crossproduct array  
                  do ifgd = 1,fgd_size
                     do isgd = 1,sgd_size
                        sgd_data(ifgd,isgd) = sgd_data_1d(isgd)
                        fgd_data(ifgd,isgd) = fgd_data_1d(ifgd)
                     enddo
                  enddo 
               else
                  ! Something wrong with the coordinate dimensions 
               endif 

               ! transform fgd and sgd here if necessary
               fgd_data_1d = reshape(fgd_data, (/fgd_size*sgd_size/))
               sgd_data_1d = reshape(sgd_data, (/fgd_size*sgd_size/))
   
               if (grid_type == elmSetType_cartesian) then
                  if (.not. (ecElementSetSetType(instancePtr, elementSetId, grid_type) .and. &
                             ecElementSetSetXArray(instancePtr, elementSetId, fgd_data_1d) .and. &
                             ecElementSetSetYArray(instancePtr, elementSetId, sgd_data_1d) .and. &
                             ecElementSetSetRowsCols(instancePtr, elementSetId, sgd_size, fgd_size) .and. &
                             ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, fgd_size*sgd_size))) then
                     return
                  end if
               else if (grid_type == elmSetType_spheric) then
                  if (allocated(fgd_data_trans)) deallocate(fgd_data_trans)
                  if (allocated(sgd_data_trans)) deallocate(sgd_data_trans)
                  if (allocated(pdiri)) deallocate(pdiri)
                  allocate(fgd_data_trans(fgd_size*sgd_size), sgd_data_trans(fgd_size*sgd_size))
                  if (.not.ecElementSetSetType(instancePtr, elementSetId, grid_type)) then 
                     call setECMessage("Setting element type failed for "//trim(fileReaderPtr%filename)//".")
                     return
                  endif 

                  if (rotate_pole) then 
                     if (allocated(pdiri)) deallocate(pdiri)
                     allocate(pdiri(fgd_size*sgd_size))
                     call gb2lla(fgd_data_1d, sgd_data_1d, fgd_data_trans, sgd_data_trans, pdiri, fgd_size*sgd_size, &
                          gsplon, gsplat, 0.0_hp, 0.0_hp, -90.0_hp, 0.0_hp) 
                     if (.not.ecElementSetSetLatitudeArray(instancePtr, elementSetId, sgd_data_trans)) then 
                        call setECMessage("Setting latitude array failed for "//trim(fileReaderPtr%filename)//".")
                        return
                     endif 
                     if (.not.ecElementSetSetLongitudeArray(instancePtr, elementSetId, fgd_data_trans)) then
                        call setECMessage("Setting longitude array failed for "//trim(fileReaderPtr%filename)//".")
                        return
                     endif 
                     if (.not.ecElementSetSetDirectionArray(instancePtr, elementSetId, pdiri)) then
                        call setECMessage("Setting rotation array for vector for transformed vector quantities failed for "//trim(fileReaderPtr%filename)//".")
                        return
                     endif 
                  else 
                     if (.not.ecElementSetSetLatitudeArray(instancePtr, elementSetId, sgd_data_1d)) then 
                        call setECMessage("Setting latitude array failed for "//trim(fileReaderPtr%filename)//".")
                        return
                     endif 
                     if (.not.ecElementSetSetLongitudeArray(instancePtr, elementSetId, fgd_data_1d)) then
                        call setECMessage("Setting longitude array failed for "//trim(fileReaderPtr%filename)//".")
                        return
                     endif 
                  endif 
                  if (.not.ecElementSetSetRowsCols(instancePtr, elementSetId, sgd_size, fgd_size)) then
                     call setECMessage("Setting number of rows and columns failed for "//trim(fileReaderPtr%filename)//".")
                     return
                  endif 
                  if (.not.ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, fgd_size*sgd_size)) then
                     call setECMessage("Setting number of points failed for "//trim(fileReaderPtr%filename)//".")
                     return
                  endif 
                endif
            end if

            ! ===================
            ! Create the Quantity
            ! ===================
            quantityId = ecInstanceCreateQuantity(instancePtr)
            ierror = nf90_get_att(fileReaderPtr%fileHandle, idvar(i), 'units', units)
            call str_upper(units)                                                ! make units attribute case-insensitive 
            if (.not. (ecQuantitySetName(instancePtr, quantityId, ncvarnames(i)) .and. & 
                       ecQuantitySetUnits(instancePtr, quantityId, units))) then
                  return
            end if
            ! ========================
            ! Create the source Fields 
            ! ========================
            !  --- Determine missingDataValue ---
            if (.not. ecSupportNetcdfCheckError(nf90_get_att(fileReaderPtr%fileHandle, fgd_id, "_FillValue", fdg_miss), "reading _FillValue", fileReaderPtr%fileName)) then
               if (.not. ecSupportNetcdfCheckError(nf90_get_att(fileReaderPtr%fileHandle, NF90_GLOBAL, "NF90_FILL_DOUBLE", fdg_miss), "reading _FillValue", fileReaderPtr%fileName)) then
                  fdg_miss = ec_undef_hp
               end if
            end if
            if (.not. ecSupportNetcdfCheckError(nf90_get_att(fileReaderPtr%fileHandle, fgd_id, "_FillValue", sdg_miss), "reading _FillValue", fileReaderPtr%fileName)) then
               if (.not. ecSupportNetcdfCheckError(nf90_get_att(fileReaderPtr%fileHandle, NF90_GLOBAL, "NF90_FILL_DOUBLE", sdg_miss), "reading _FillValue", fileReaderPtr%fileName)) then
                  sdg_miss = ec_undef_hp
               end if
            end if
            !
            field0Id = ecInstanceCreateField(instancePtr)
            if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, fgd_size*sgd_size) .and. &
                       ecFieldSetMissingValue(instancePtr, field0Id, fdg_miss))) then
                  return
            end if
            field1Id = ecInstanceCreateField(instancePtr)
            if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, fgd_size*sgd_size) .and. &
                       ecFieldSetMissingValue(instancePtr, field1Id, sdg_miss))) then
                  return
            end if
            ! ==================
            ! Create source Item
            ! ==================
            itemId = ecInstanceCreateItem(instancePtr)
            if (.not. ( ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                        ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                        ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                        ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                        ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                        ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
                  return
            else
               itemPtr => ecSupportFindItem(instancePtr, itemId)
            end if
            ! ===== finish initialization of Fields =====
            ! Read the first two records into tEcItem%sourceT0FieldPtr and tEcItem%sourceT1FieldPtr.
            ! Add successfully created source Item to the FileReader
            success = ecNetcdfReadNextBlock(fileReaderPtr, itemPtr, 0)
            if (success) then
               ! trick: set fieldT1%timesteps to fieldT0%timesteps, so that T1 will be filled with the block after T0's block.
               itemPtr%sourceT1FieldPtr%timesteps = itemPtr%sourceT0FieldPtr%timesteps
               success = ecNetcdfReadNextBlock(fileReaderPtr, itemPtr, 1)
            end if
            ! Add successfully created source Items to the FileReader
            if (success) success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, itemPtr%id)

         enddo !                i = 1, size(ncvarnames) quantities in requested set of quantities 

      end function ecProviderCreateNetcdfItems

      ! =======================================================================
      
      !> Create source Items and their contained types, based on NetCDF file header.
      function ecProviderCreateWaveNetcdfItems(instancePtr, fileReaderPtr, quantityName) result(success)
         logical                      :: success       !< function status
         type(tEcInstance),   pointer :: instancePtr   !< intent(in)
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(inout)
         character(maxNameLen), intent(in) :: quantityName  !< name of quantity to read
         !
         integer                   :: i              !< loop counter
         integer                   :: ierror         !< return value of function calls
         integer                   :: iddim_netelem  !< id as obtained from NetCDF
         integer                   :: idvar_x        !< id as obtained from NetCDF
         integer                   :: idvar_q        !< id as obtained from NetCDF
         integer                   :: ivar           !< loopvariable of varids in this netcdf file
         integer                   :: n              !< number of values
         integer                   :: nvar           !< number of varids in this netcdf file 
         integer                   :: quantityId     !< helper variable 
         integer                   :: elementSetId   !< helper variable 
         integer                   :: field0Id       !< helper variable 
         integer                   :: field1Id       !< helper variable 
         integer                   :: itemId         !< helper variable
         integer                   :: t0t1           !< indicates whether the 0 or the 1 field is read. -1: choose yourself
         logical                   :: local_success  !< when the return flag should not be influenced
         real(hp)                  :: dmiss          !< missing data value
         type(tEcItem), pointer    :: item
         character(20)             :: name           !< 
         character(NF90_MAX_NAME)  :: string         !< read from NetCDF file
         character(300)            :: message

         !
         ! body
         success      = .false.
         item => null()
         dmiss        = -999.0_hp
         elementSetId = ecInstanceCreateElementSet(instancePtr)

         ! Make a list of standard names of variables available in the netcdf file 
         nvar = 0 
         ierror = nf90_inquire(fileReaderPtr%fileHandle, nvariables = nvar)
         if (nvar>0) then 
            allocate(fileReaderPtr%standard_names(nvar))          ! Note: one of these may be obsolete (if we only check standard names)
            allocate(fileReaderPtr%variable_names(nvar))
            fileReaderPtr%standard_names = ''
            fileReaderPtr%variable_names = ''
            do ivar = 1,nvar 
               ierror = nf90_get_att(fileReaderPtr%fileHandle, ivar, 'standard_name', fileReaderPtr%standard_names(ivar))
               ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, ivar, name=fileReaderPtr%variable_names(ivar))
            enddo 
         else 
            ! no variables in the file or netcdf inquiry error .... handle exception 
            return
         endif 
         

         ! Make a list of standard names of variables available in the netcdf file 
         nvar = 0 
         ierror = nf90_inquire(fileReaderPtr%fileHandle, nvariables = nvar)
         if (nvar>0) then 
            if (allocated(fileReaderPtr%standard_names)) then
               deallocate(fileReaderPtr%standard_names, stat=ierror)
            endif
            if (allocated(fileReaderPtr%variable_names)) then
               deallocate(fileReaderPtr%variable_names, stat=ierror)
            endif
            allocate(fileReaderPtr%standard_names(nvar), stat=ierror)          ! Note: one of these may be obsolete (if we only check standard names)
            allocate(fileReaderPtr%variable_names(nvar), stat=ierror)
            fileReaderPtr%standard_names = ''
            fileReaderPtr%variable_names = ''
            do ivar = 1,nvar 
               ierror = nf90_get_att(fileReaderPtr%fileHandle, ivar, 'standard_name', fileReaderPtr%standard_names(ivar))
               ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, ivar, name=fileReaderPtr%variable_names(ivar))
            enddo 
         else 
            ! no variables in the file or netcdf inquiry error .... handle exception 
            return
         endif 
         
         !
         ! Cartesian or spheric
         ! WARNING: elementSetType must be set before elementSetNumberOfCoordinates (why???)
         !
         string = ' '
         ierror = nf90_get_att(fileReaderPtr%fileHandle, nf90_global,  'grid_mapping', string); success = ecSupportNetcdfCheckError(ierror, "get_att global grid_mapping", fileReaderPtr%filename)
         if (string == 'wgs84') then
            success = ecElementSetSetType(instancePtr, elementSetId, elmSetType_spheric)
         else
            success = ecElementSetSetType(instancePtr, elementSetId, elmSetType_cartesian)
         endif
         !
         ! Read the number of values on the NetCDF file; this should exactly match the number of cells in dflowfm
         !
         ierror  = nf90_inq_dimid(fileReaderPtr%fileHandle, 'nFlowElemWithBnd', iddim_netelem); success = ecSupportNetcdfCheckError(ierror, "inq_dimid netelem", fileReaderPtr%fileName)
         ierror  = nf90_inquire_dimension(fileReaderPtr%fileHandle, iddim_netelem, string, n); success = ecSupportNetcdfCheckError(ierror, "inq_dim netelem", fileReaderPtr%fileName)
         success = ecElementSetSetNumberOfCoordinates(instancePtr, elementSetId, n)
         !
         ! Use quantityName
         !
         string = ' '
         ierror     = nf90_inq_varid(fileReaderPtr%fileHandle, quantityName, idvar_q); success = ecSupportNetcdfCheckError(ierror, "inq_varid " // quantityName, fileReaderPtr%filename)
         ierror     = nf90_get_att(fileReaderPtr%fileHandle, idvar_q, 'units', string); success = ecSupportNetcdfCheckError(ierror, "inq_att " // quantityName, fileReaderPtr%filename)
         if (.not.success) return
         !
         quantityId = ecInstanceCreateQuantity(instancePtr)
         if (.not. (ecQuantitySetName(instancePtr, quantityId, quantityName) .and. &
                    ecQuantitySetUnits(instancePtr, quantityId, trim(string)))) then
            success = .false.
         endif
         field0Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field0Id, n) .and. &
                    ecFieldSetMissingValue(instancePtr, field0Id, dmiss))) then
            success = .false.
         endif
         field1Id = ecInstanceCreateField(instancePtr)
         if (.not. (ecFieldCreate1dArray(instancePtr, field1Id, n) .and. &
                    ecFieldSetMissingValue(instancePtr, field1Id, dmiss))) then
            success = .false.
         endif
         itemId = ecInstanceCreateItem(instancePtr)
         if (.not. ( ecItemSetRole(instancePtr, itemId, itemType_source) .and. &
                     ecItemSetType(instancePtr, itemId, accessType_fileReader) .and. &
                     ecItemSetQuantity(instancePtr, itemId, quantityId) .and. &
                     ecItemSetElementSet(instancePtr, itemId, elementSetId) .and. &
                     ecItemSetSourceT0Field(instancePtr, itemId, field0Id) .and. &
                     ecItemSetSourceT1Field(instancePtr, itemId, field1Id))) then
            success = .false.
         else
            item => ecSupportFindItem(instancePtr, itemId)
         endif
         !
         ! finish initialization of Fields
         ! Read the first two records into tEcItem%sourceT0FieldPtr and tEcItem%sourceT1FieldPtr
         !
         if (success) then
            t0t1 = 0
            success = ecNetcdfReadBlock(fileReaderPtr, item, t0t1, n)
         endif
         if (success) then
            t0t1 = 1
            local_success = ecNetcdfReadBlock(fileReaderPtr, item, t0t1, n)
         endif
         ! Add successfully created source Item to the FileReader
         if (success) then
            success = ecFileReaderAddItem(instancePtr, fileReaderPtr%id, item%id)
         endif
         if (.not. success) then
             write(message,'(2a)') "ERROR: ec_provider::ecProviderCreateWaveNetcdfItems: unable to create sourceItem for ", quantityName
             call setECMessage(trim(message))
         endif
      end function ecProviderCreateWaveNetcdfItems
      
      ! =======================================================================
      
      !> Set the TimeFrame's properties, based on a file header when present, or to refdat and hardcoded values otherwise.
      function ecProviderInitializeTimeFrame(fileReaderPtr, k_refdate, k_timezone, k_timestep_unit, dtnodal) result(success)
         logical                         :: success          !< function status
         type(tEcFileReader), pointer    :: fileReaderPtr    !< intent(inout)
         integer,             intent(in) :: k_refdate        !< Kernel's reference date, format: Gregorian yyyymmdd
         real(hp),            intent(in) :: k_timezone       !< Kernel's timezone.
         integer,             intent(in) :: k_timestep_unit  !< Kernel's time step unit (1=seconds, 2=minutes, 3=hours)
         real(hp), optional,  intent(in) :: dtnodal          !< Nodal factors update interval
         real(hp) :: julianDay
         !
         success = .false.
         !
         if (k_refdate > -1) then

            fileReaderPtr%tframe%k_refdate = dble(ymd2jul(k_refdate) - 2400000.5_hp)
            fileReaderPtr%tframe%k_timezone = k_timezone
            fileReaderPtr%tframe%k_timestep_unit = k_timestep_unit

            fileReaderPtr%tframe%ec_refdate = fileReaderPtr%tframe%k_refdate
            fileReaderPtr%tframe%ec_timezone = fileReaderPtr%tframe%k_timezone
            fileReaderPtr%tframe%ec_timestep_unit = fileReaderPtr%tframe%k_timestep_unit 

            if(present(dtnodal) .and. dtnodal /= 0.0_hp) then
               fileReaderPtr%tframe%dtnodal = dtnodal
            else
               fileReaderPtr%tframe%dtnodal = 1e+20_hp
            endif

         else
            ! no kernel ref date defined
            fileReaderPtr%tframe%k_refdate = -1
         endif

         !
         select case(fileReaderPtr%ofType)
            case (provFile_undefined)
               call setECMessage("ERROR: ec_provider::ecProviderInitializeTimeFrame: No file type specified.")
            case (provFile_uniform, provFile_unimagdir)
               success = ecUniInitializeTimeFrame(fileReaderPtr)
            case (provFile_svwp)
               call setECMessage("ERROR: ec_provider::ecProviderInitializeTimeFrame: Unsupported file type.")
            case (provFile_svwp_weight)
               call setECMessage("ERROR: ec_provider::ecProviderInitializeTimeFrame: Unsupported file type.")
            case (provFile_arcinfo)
               success = ecDefaultInitializeTimeFrame(fileReaderPtr)
            case (provFile_spiderweb)
               success = ecDefaultInitializeTimeFrame(fileReaderPtr)
            case (provFile_curvi)
               success = ecDefaultInitializeTimeFrame(fileReaderPtr)
            case (provFile_curvi_weight)
               call setECMessage("ERROR: ec_provider::ecProviderInitializeTimeFrame: Unsupported file type.")
            case (provFile_samples)
               ! Time independent: ec timeframe params were already defaulted to kernel's timeframe params (above).
               success = .true.
            case (provFile_triangulationmagdir)
               call setECMessage("ERROR: ec_provider::ecProviderInitializeTimeFrame: Unsupported file type.")
            case (provFile_qhtable)
               fileReaderPtr%tframe%ec_refdate = fileReaderPtr%tframe%k_refdate
               ! The nr_timesteps is infinity.
               success = .true.
            case (provFile_poly_tim)
               ! TODO : For now the top-level fileReader (*.pli) needs no TimeFrame.
               fileReaderPtr%tframe%ec_refdate = fileReaderPtr%tframe%k_refdate
               ! TODO : The total number of available timesteps is not yet known here.
               success = .true.
            case (provFile_fourier)
               ! Boundary condition components file containing: either period[minute] or component name, amplitude[m], phase[degree].
               ! Time_unit explicit in the Converter, as values at a moment in time are calculated from the seeds in the file, not read from file as records.
               fileReaderPtr%tframe%ec_refdate = fileReaderPtr%tframe%k_refdate
               ! The nr_timesteps is infinity, as values at a moment in time are calculated from the seeds in the file, not read from file as records.
               success = .true.
            case (provFile_grib)
               call setECMessage("ERROR: ec_provider::ecProviderInitializeTimeFrame: Unsupported file type.")
            case (provFile_netcdf)
               success = ecNetcdfInitializeTimeFrame(fileReaderPtr)
            case (provFile_t3D)
               success = ecDefaultInitializeTimeFrame(fileReaderPtr)
            case (provFile_bc)
               ! Filereader was created thru a BC-instance. This instance has a timeunit (netcdf-style) property
               if (fileReaderPtr%bc%func == BC_FUNC_TSERIES .or. fileReaderPtr%bc%func == BC_FUNC_TIM3D) then 
                  success = ecSupportTimestringToUnitAndRefdate(fileReaderPtr%bc%timeunit, &
                                                                fileReaderPtr%tframe%ec_timestep_unit, fileReaderPtr%tframe%ec_refdate)
                  if (success) then
                     ! TODO: handle MJD in a proper way. For now, abstract the .5 day that originated
                     !       from the fact that in ecSupportTimestringToUnitAndRefdate the
                     !       call to ymd2jul in leads to a rounded off integer value.
                     fileReaderPtr%tframe%ec_refdate = fileReaderPtr%tframe%ec_refdate - 0.5d+0
                  endif
               else 
                  success = .true.
               endif 
            case default
               call setECMessage("ERROR: ec_provider::ecProviderInitializeTimeFrame: Unknown file type.")
         end select
      end function ecProviderInitializeTimeFrame
      
      ! =======================================================================
      
      !> Set the TimeFrame's properties, based on a default time string format.
      !! meteo1.f90: reaspwtim
      function ecDefaultInitializeTimeFrame(fileReaderPtr) result(success)
         logical                      :: success       !< function status
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(in)
         !
         character(len=maxNameLen) :: keyword    !< helper variable
         character(len=maxNameLen) :: rec        !< helper variable
         character(len=maxNameLen) :: prev_rec   !< helper variable
         integer                   :: indx       !< helper variable
         integer                   :: year       !< helper variable
         integer                   :: month      !< helper variable
         integer                   :: day        !< helper variable
         real(hp)                  :: time_steps !< helper variable
         !
         success = .false.
         keyword = 'TIME'
         prev_rec = ' '
         !
         rewind(unit=fileReaderPtr%fileHandle)
         !
         rec = ecFindInFile(fileReaderPtr%fileHandle, keyword)
         if (len_trim(rec) > 0) then
            ! Determine the timestep unit and reference date for the time data in the file.
            if (.not. ecSupportTimestringToUnitAndRefdate(rec, fileReaderPtr%tframe%ec_timestep_unit, fileReaderPtr%tframe%ec_refdate)) return
         else
            call setECMessage("ERROR: ec_provider::ecDefaultInitializeTimeFrame: Unable to identify the first data block.")
            return
         end if
         ! =========================================
         ! Determine the total number of time steps.
         ! =========================================
         !!! This read action is too slow on multi-gigabyte files. Commented for now.
         !!!do
         !!!   prev_rec = rec
         !!!   rec = ecFindInFile(fileReaderPtr%fileHandle, keyword)
         !!!   if (len_trim(rec) == 0) exit
         !!!end do
         !!!if (len_trim(prev_rec) /= 0) then
         !!!   ! Read and convert the timesteps to seconds.
         !!!   if (.not. ecGetTimesteps(prev_rec, time_steps, .false.)) return
         !!!   ! Store the total number of available timesteps in the ecFileReader's ecTimeFrame.
         !!!   fileReaderPtr%tframe%nr_timesteps = time_steps
         !!!else
         !!!   call setECMessage("ERROR: ec_provider::ecDefaultInitializeTimeFrame: Unable to read the stop time.")
         !!!end if
         success = .true.
      end function ecDefaultInitializeTimeFrame
      
      ! =======================================================================
      
      !> Set the TimeFrame's properties, based on a uni* file's header.
      !! Headerless column-based ASCII file.
      !! uniform: time[minute], wind_x[m s-1](, wind_y[m s-1])
      !! unimagdir: time[minute], windspeed[m s-1], winddirection[degree]
      function ecUniInitializeTimeFrame(fileReaderPtr) result(success)
         logical                                 :: success       !< function status
         type(tEcFileReader), pointer            :: fileReaderPtr !< FileReader to initialize
         !
         real(hp) :: time_steps      !< number of timesteps
         real(hp) :: prev_time_steps !< helper variable for determining tstop
         !
         success = .false.
         fileReaderPtr%tframe%ec_refdate = fileReaderPtr%tframe%k_refdate
         ! Obtain the total number of timesteps in this file.
         rewind(unit=fileReaderPtr%fileHandle)

         !!! This read action is too slow on multi-gigabyte files. Commented for now.
         !!!time_steps = 0.0_hp
         !!!do
         !!!   prev_time_steps = time_steps
         !!!   if (.not. ecUniReadTimeSteps(fileReaderPtr, time_steps)) exit ! TODO: EB: do we really need to count all time steps beforehand?
         !!!end do
         !!!! Convert timesteps from minutes to seconds.
         !!!fileReaderPtr%tframe%nr_timesteps = prev_time_steps * 60.0_hp
         success = .true.

! ecSupportTimestringToUnitAndRefdate(units, fileReaderPtr%tframe%ec_timestep_unit, fileReaderPtr%tframe%ec_refdate)         
      end function ecUniInitializeTimeFrame
      
      ! =======================================================================
      
      !> Set the TimeFrame's properties, based on a NetCDF file.
      function ecNetcdfInitializeTimeFrame(fileReaderPtr) result(success)
         use netcdf
         !
         logical                      :: success       !< function status
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(in)
         !
         integer                      :: nVariables !< number of variables in NetCDF file
         integer                      :: time_id    !< integer id of variable with standard_name "time"
         integer                      :: i          !< loop counter
         character(len=NF90_MAX_NAME) :: name       !< name of a variable
         character(len=NF90_MAX_NAME) :: units      !< units attribute of a variable
         integer, dimension(1)        :: dimid      !< integer id of time variable's dimension variable
         integer                      :: length     !< number of time steps
         integer                      :: istat      !< status of allocation operation
         !
         success = .false.
         nVariables = 0
         time_id = ec_undef_int
         !
         ! Determine the total number of variables inside the NetCDF file.
         if (.not. ecSupportNetcdfCheckError(nf90_inquire(fileReaderPtr%fileHandle, nVariables=nVariables), "obtain nVariables", fileReaderPtr%fileName)) return
         !
         ! Inspect the standard_name attribute of all variables to find "time" and store that variable's id.
         do i=1, nVariables
            name = '' ! NetCDF does not completely overwrite a string, so re-initialize.
            if (.not. ecSupportNetcdfCheckError(nf90_get_att(fileReaderPtr%fileHandle, i, "standard_name", name), "obtain standard_name", fileReaderPtr%fileName)) then
               cycle
            endif
            if (strcmpi(name, 'time')) then
               time_id = i
               exit
            end if
         end do
         if (time_id == ec_undef_int) then
            call setECMessage("ERROR: ec_provider::ecNetcdfInitializeTimeFrame: Unable to find variable with standard_name: time.")
            return
         end if
         !
         ! Determine the timestep unit and reference date for the time data in the NetCDF file.
         ! Surprisingly, the reference date is part of the "units" attribute.
         units = '' ! NetCDF does not completely overwrite a string, so re-initialize.
         if (.not. ecSupportNetcdfCheckError(nf90_get_att(fileReaderPtr%fileHandle, time_id, "units", units), "obtain units", fileReaderPtr%fileName)) return
         if (.not. ecSupportTimestringToUnitAndRefdate(units, fileReaderPtr%tframe%ec_timestep_unit, fileReaderPtr%tframe%ec_refdate)) return
         !
         ! Determine the total number of timesteps.
         if (.not. ecSupportNetcdfCheckError(nf90_inquire_variable(fileReaderPtr%fileHandle, time_id, dimids=dimid), "obtain time dimension ids", fileReaderPtr%fileName)) return
         if (.not. ecSupportNetcdfCheckError(nf90_inquire_dimension(fileReaderPtr%fileHandle, dimid(1), len=length), "obtain time dimension length", fileReaderPtr%fileName)) return
         fileReaderPtr%tframe%nr_timesteps = 0.0_hp + length
         allocate(fileReaderPtr%tframe%times(length), stat = istat)
         ! Store the times at which data is available.
         if (.not. ecSupportNetcdfCheckError(nf90_get_var(fileReaderPtr%fileHandle, time_id, fileReaderPtr%tframe%times, start=(/1/), count=(/length/)), "obtain time data", fileReaderPtr%fileName)) return
         !
         success = .true.
      end function ecNetcdfInitializeTimeFrame
      
      ! =======================================================================
      
      !> Read the header of a Arcinfo file.
      !! Does not check whether all expected information is present in the header.
      !! meteo1:: readarcinfoheader
      function readarcinfoheader(minp, mmax, nmax, x0, y0, dxa, dya, dmiss) result(success)
         logical               :: success !< function status
         integer               :: minp    !< 
         integer,  intent(out) :: mmax    !< 
         integer,  intent(out) :: nmax    !< 
         real(hp), intent(out) :: x0      !< 
         real(hp), intent(out) :: y0      !< 
         real(hp), intent(out) :: dxa     !< 
         real(hp), intent(out) :: dya     !< 
         real(hp), intent(out) :: dmiss   !< 
         !
         integer                   :: jacornerx
         integer                   :: jacornery
         integer                   :: k
         integer                   :: l
         integer                   :: equal_sign_index
         character(len=maxNameLen) :: rec
         !
         success = .false.
         jacornerx = 0
         jacornery = 0
         !
         rewind(unit = minp)
10       continue
         read (minp, '(A)', end = 100) rec
         mmax = -1
         nmax = -1
         if (index(rec, '### START OF HEADER') > 0) then ! new d3dflow header
20          continue
            read (minp, '(A)', end = 100) rec
            equal_sign_index = index(rec, '=')
            L = equal_sign_index + 1
            !
            if (index(rec, 'NODATA_value') > 0 .and. index(rec, 'NODATA_value') < equal_sign_index) then
               read (rec(L:), *, err = 106) dmiss
            endif
            !
            if (index(rec, 'n_cols') > 0 .and. index(rec, 'n_cols') < equal_sign_index) then
               read (rec(L:), *, err = 101) mmax
            endif
            !
            if (index(rec, 'n_rows') > 0 .and. index(rec, 'n_rows') < equal_sign_index) then
               read (rec(L:), *, err = 102) nmax
            endif
            !
            if (index(rec, 'x_llcenter') > 0 .and. index(rec, 'x_llcenter') < equal_sign_index) then
               read (rec(L:), *, err = 103) x0
            endif
            !
            if (index(rec, 'x_llcorner') > 0 .and. index(rec, 'x_llcorner') < equal_sign_index) then
               read (rec(L:), *, err = 103) x0
               jacornerx = 1
            endif
            !
            if (index(rec, 'y_llcenter') > 0 .and. index(rec, 'y_llcenter') < equal_sign_index) then
               read (rec(L:), *, err = 104) y0
            endif
            !
            if (index(rec, 'y_llcorner') > 0 .and. index(rec, 'y_llcorner') < equal_sign_index) then
               read (rec(L:), *, err = 104) y0
               jacornery = 1 
            endif
            !
            if (index(rec, 'dx') > 0 .and. index(rec, 'dx') < equal_sign_index) then
               read (rec(L:), *, err = 105) dxa
            endif
            !
            if (index(rec, 'dy') > 0 .and. index(rec, 'dy') < equal_sign_index) then
               read (rec(L:), *, err = 105) dya
            endif
            !
            if (index(rec, '### END OF HEADER') == 0) then  ! new d3dflow header
               goto 20
            endif
         else 
            !
            if (rec(1:1)=='*' .or. rec(2:2)=='*') goto 10
            read (rec(13:), *, err = 101) mmax
            read (minp, '(A)', end = 100) rec
            read (rec(13:), *, err = 102) nmax
            !
            read (minp, '(A)', end = 100) rec
            read (rec(13:), *, err = 103) x0
            jacornerx = 0
            if (index(rec, 'corner')/=0) jacornerx = 1
            !
            read (minp, '(A)', end = 100) rec
            read (rec(13:), *, err = 104) y0
            jacornery = 0
            if (index(rec, 'corner')/=0) jacornery = 1
            !
            read (minp, '(A)', end = 100) rec
            l = index(rec, 'cellsize') + 8
            k = numbersonline(rec(l:))
            if (k==1) then
               read (rec(13:), *, err = 105) dxa
               dya = dxa
               jacornery = jacornerx
            else
               read (rec(13:), *, err = 105) dxa, dya
            endif
            read (minp, '(A)', end = 100) rec
            read (rec(13:), *, err = 106) dmiss
            !
            ! Data in an arcinfo grid file is always defined in the cell centres.
            ! Data is assumed to be given at the points x0+i*dx,y0+j*dy
            ! If the x0/y0 line contains the word corner, the corner coordinates
            ! have been specified in the file. Therefore, shift x0 and y0 by half
            ! a grid cell to the cell centres.
            !
         endif

         if (mmax < 0) then
            call setECMessage('ERROR: Could not find n_cols/mmax value in header of arcinfo file')
            goto 999
         end if
         if (nmax < 0) then
            call setECMessage('ERROR: Could not find n_rows/nmax value in header of arcinfo file')
            goto 999
         end if

         !
         if (jacornerx == 1) x0 = x0 + dxa/2
         if (jacornery == 1) y0 = y0 + dya/2
         !
         success = .true.
         return
         !
         ! error handling
         !
         100 continue
         call setECMessage('ERROR: Unexpected end of file while reading header of arcinfo file')
         goto 999
         101 continue
         call setECMessage('ERROR: Looking for ncols (arc-info), but getting: ',trim(rec))
         goto 999
         102 continue
         call setECMessage('ERROR: Looking for nrows (arc-info), but getting: ',trim(rec))
         goto 999
         103 continue
         call setECMessage('ERROR: Looking for xll (arc-info), but getting: ',trim(rec))
         goto 999
         104 continue
         call setECMessage('ERROR: Looking for yll (arc-info), but getting: ',trim(rec))
         goto 999
         105 continue
         call setECMessage('ERROR: Looking for cellsize (dx, dy) (arc-info), but getting: ',trim(rec))
         goto 999
         106 continue
         call setECMessage('ERROR: Looking for missing value (arc-info), but getting: ',trim(rec))
         goto 999
         999 continue
         success = .false.
      end function readarcinfoheader
      
      ! =======================================================================
      
      function numbersonline(rec)
         integer      :: numbersonline
         character(*) :: rec
         !
         integer :: i
         integer :: istarti
         integer :: leeg
         integer :: lend
         !
         numbersonline = 0
         leeg = 1
         lend = len_trim(rec)
         do i = 1, lend
            if (index(rec(i:i), ' ')==0) then
               ! there is nothing here
               if (leeg==1) then
                  leeg = 0
                  istarti = i
                  numbersonline = numbersonline + 1
               endif
            else
               leeg = 1
            endif
         enddo
    end function numbersonline


!============================================================================================================================
! Items_from_bc_quantities: Produce a separate (source) item from every quantity in a bcblock and connect it to the filereader.
   function items_from_bc_quantities(instancePtr,fileReaderPtr) result (success)
!  use m_ec_provider
   implicit none

   type(tEcBCBlock), pointer              ::      bcPtr                      !< Pointer to a BCBlock instance
   type(tEcFileReader), pointer           ::      fileReaderPtr              !< Pointer to a filereader instance
   type(tEcInstance), pointer             ::      instancePtr

   integer                                ::      ic, nc
   logical                                ::      success

   bcPtr => fileReaderPtr%bc
   nc = bcPtr%numcols
   allocate(bcPtr%columns(bcPtr%numcols))
   do ic = 1, nc
      if (ic==bcPtr%timecolumn) then
         print *, '   Quantity "',trim(bcPtr%quantities(ic)%name),'", (time)'
      else
         print *, '   Quantity "',trim(bcPtr%quantities(ic)%name),'"'
         bcPtr%quantity => bcPtr%quantities(ic)
         if (.not.(ecInstanceCreateUniformItems(instancePtr, fileReaderPtr))) then
         endif
      endif
   enddo

   success = .True.
   end function items_from_bc_quantities

end module m_ec_provider
