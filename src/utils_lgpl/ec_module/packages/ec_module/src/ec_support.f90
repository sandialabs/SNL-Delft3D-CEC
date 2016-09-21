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

!  $Id: ec_support.f90 5640 2015-12-10 09:24:34Z hummel $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/ec_module/packages/ec_module/src/ec_support.f90 $

!> This module contains support methods for the EC-module.
!! @author adri.mourits@deltares.nl
!! @author edwin.bos@deltares.nl
module m_ec_support
   use m_ec_message
   use m_ec_typedefs
   use m_alloc
   use string_module
   use m_ec_parameters
   use time_module

   implicit none
   
   private
   
   public :: ecTimeFrameRealHpTimestepsToModifiedJulianDate
   public :: ecTimeFrameRealHpTimestepsToDateTime
   public :: ecGetTimesteps
   public :: ecSupportOpenExistingFile
   public :: ecSupportOpenExistingFileGnu
   public :: ecSupportAddUniqueInt
   public :: ecSupportAddUniqueChar
   public :: ecSupportFindItemByQuantityLocation
   public :: ecSupportFindItem
   public :: ecSupportFindQuantity
   public :: ecSupportFindElementSet
   public :: ecSupportFindField
   public :: ecSupportFindConnection
   public :: ecSupportFindConverter
   public :: ecSupportFindFileReader
   public :: ecSupportFindBCBlock
   public :: ecSupportFindNetCDF
   public :: ecSupportFindNetCDFByFilename
   public :: ecSupportNetcdfCheckError
   public :: ecSupportTimestringToUnitAndRefdate
   public :: ecSupportTimeUnitConversionFactor
   public :: ecSupportTimeToTimesteps
   public :: ecSupportThisTimeToTimesteps
   public :: ecSupportFindRelatedBCBlock
   
   interface ecTimeFrameRealHpTimestepsToModifiedJulianDate
      module procedure ecTimeFrameRealHpTimestepsToModifiedJulianDate
   end interface ecTimeFrameRealHpTimestepsToModifiedJulianDate
   
   contains
      
      ! ==========================================================================
      
      !> Calculate a Julian Date from the number of timesteps in seconds since reference_date.
      function ecTimeFrameRealHpTimestepsToModifiedJulianDate(timeFramePtr, steps, mjd) result(success)
         logical                                 :: success      !< function status
         type(tEcTimeFrame), pointer             :: timeFramePtr !< intent(inout)
         real(hp),                   intent(in)  :: steps        !< number of time steps
         real(hp),                   intent(out) :: mjd          !< calculated Modified Julian Date
         !
         success = .true.
         !
         mjd = timeFramePtr%k_refdate + (steps / 60.0_hp / 60.0_hp / 24.0_hp)
         
      end function ecTimeFrameRealHpTimestepsToModifiedJulianDate
      
      ! =======================================================================
      
      !> Calculate a Gregorian date and hour-minutes-seconds integer since reference date
      function ecTimeFrameRealHpTimestepsToDateTime(timeFramePtr, steps, yyyymmdd, hhmmss) result(success)
         logical                                 :: success      !< function status
         type(tEcTimeFrame), pointer             :: timeFramePtr !< intent(inout)
         real(hp),                   intent(in)  :: steps        !< number of time steps
         integer,                    intent(out) :: yyyymmdd     !< calculated Gregorian date
         integer,                    intent(out) :: hhmmss     !< time of the day
         real(hp)                                :: jd           !< julian date helper variable
         real(hp)                                :: spd          !< seconds per day helper variable
         real(hp)                                :: ssm          !< seconds since midnight helper variable
         integer                                 :: hh, mm, ss   !< hours, minutes, seconds helper variables
         !
         success = .true.
         !
         spd = 60.0_hp * 60.0_hp * 24.0_hp
         jd = timeFramePtr%k_refdate + (steps / spd) + 2400000.5_hp
         call jul2ymd(int(jd), yyyymmdd)
         ssm = mod(steps, spd)
         hh = int(ssm) / 3600
         mm = int(ssm) / 60 - hh * 60
         ss = int(ssm) - hh * 3600 - mm * 60
         hhmmss = hh*10000 + mm*100 + ss
         
      end function ecTimeFrameRealHpTimestepsToDateTime
      
      ! =======================================================================
      
      !> Read and convert the timesteps to seconds.
      !! Takes a string of format: TIME = 0 hours since 2006-01-01 00:00:00 +00:00
      function ecGetTimesteps(rec, time_steps, convert) result(success)
         logical                                :: success    !< function status
         character(len=maxNameLen), intent(in)  :: rec        !< time information string
         real(hp),                  intent(out) :: time_steps !< timesteps in seconds
         logical, optional,         intent(in)  :: convert    !< convert to seconds or leave unconverted
         !
         success = .false.
         !
         if (len(trim(rec)) == 0) then
            call setECMessage("ERROR: ec_provider::ecGetTimesteps: Input string is empty.")
            return
         end if
         read(rec(index(rec, '=')+1 : index(rec, 'since')-1), *) time_steps
         call str_lower(rec)
         if (present(convert)) then
            if (.not. convert) then
                success = .true.
                return
            end if
         end if
         if (index(rec, 'seconds') /= 0) then
            continue
         else if (index(rec, 'minutes') /= 0) then
            time_steps = time_steps * 60.0_hp
         else if (index(rec, 'hours') /= 0 .or. index( rec, 'hrs') /= 0) then
            time_steps = time_steps * 60.0_hp * 60.0_hp
         else
            call setECMessage("ERROR: ec_provider::ecGetTimesteps: Unable to identify the time unit.")
            return
         end if
         success = .true.
      end function ecGetTimesteps
      
      ! ==========================================================================
      
      !> Attempt to open an file for reading that might already have been opened under another handle.
      !> Workaround for GNU Fortran (which normally does not support multiple file openings of the same file)
      function ecSupportOpenExistingFileGnu(minp, filename) result(success)
         !
         use multi_file_io

         logical                                      :: success  !< function status
         integer(kind=8),               intent(inout) :: minp     !< IO unit number
         character(len=*), intent(in)                 :: filename !< relative path
         success = .false.

         minp = mf_open(filename)
         if (minp<=0) then 
            call setECMessage("ERROR: ec_support::ecSupportOpenExistingFileGnu: Opening "//trim(filename)//" failed.")
            return
         endif 
         success = .true.
      end function ecSupportOpenExistingFileGnu

      ! ==========================================================================
      
      !> Attempt to open an existing file.
      function ecSupportOpenExistingFile(minp, filename) result(success)
         use netcdf
         !
         logical                         :: success  !< function status
         integer,          intent(inout) :: minp     !< IO unit number
         character(len=*), intent(in)    :: filename !< relative path
         !
         integer :: ierror   !< netcdf helper variable
         integer :: i        !< loop counter
         logical :: unitused !< IO unit number already in use
         integer :: istat    !< status of file open operation
         !
         success = .false.
         unitused = .false.
         ! Sanity checks.
         if (len_trim(filename) == 0) then
            call setECMessage("ERROR: ec_support::ecSupportOpenExistingFile: Name is empty")
            return
         endif
         inquire(file = trim(filename), exist = success)
         if (.not. success) then
            call setECMessage("ERROR: ec_support::ecSupportOpenExistingFile: File does not exist: ", trim(filename))
            return
         endif
         ! Special case: NetCDF.
         if (index(filename, '.nc') > 0) then
            ierror = nf90_open(filename, NF90_NOWRITE, minp)
            if (ecSupportNetcdfCheckError(ierror, "opening file", filename)) then
               success = .true.
            else
               success = .false.
            end if
            return
         endif
         ! Locate an unused file unit.
         do i = 10, maxFileUnits
            inquire (unit = i, opened = unitused) 
            if (.not. unitused) exit
         enddo
         if (unitused) then
            call setECMessage("ERROR: ec_support::ecSupportOpenExistingFile: No free unit number available")
            success = .false.
            return
         endif
         minp = i
         ! Open the data file.
         open(minp, file = trim(filename), action = 'READ', iostat = istat)
         if (istat == 0) success = .true.
      end function ecSupportOpenExistingFile
   
      ! ==========================================================================
   
      !> Add an integer to a set of integers.
      function ecSupportAddUniqueInt(intArr, anInt) result(success)
         logical                        :: success !< function status
         integer, dimension(:), pointer :: intArr !< array containing unique integers (a set)
         integer, intent(in)            :: anInt  !< integer to be added to the set of integers
   
         integer                        :: i         !< loop counter
         integer                        :: istat     !< deallocate() status
         integer                        :: lenArr    !< lenght of intArr
         integer, dimension(:), pointer :: newIntArr !< larger version of intArr
      
         success = .false.
         newIntArr => null()
         i = 0
         istat = 1
         lenArr = 0
      
         if (.not. associated(intArr)) then
            call setECMessage("ERROR: ec_support::ecSupportAddUniqueInt: Dummy argument pointer intArr is not associated.")
         else
            lenArr = size(intArr)
            do i=1, lenArr
               if (intArr(i) == anInt) then
                  ! This integer is already in intArr
                  success = .true.
                  return
               endif
            enddo
            ! This integer is not yet in intArr, so add it.
            allocate(newIntArr(lenArr+1), STAT = istat)
            if (istat /= 0 ) then
               call setECMessage("ERROR: ec_support::ecSupportAddUniqueInt: Unable to allocate additional memory.")
            else
               do i=1, lenArr
                  newIntArr(i) = intArr(i) ! Copy existing integers.
               enddo
               newIntArr(lenArr+1) = anInt ! Add the new integer.
               deallocate(intArr, STAT = istat)
               if (istat /= 0 ) then
                  call setECMessage("WARNING: ec_support::ecSupportAddUniqueInt: Unable to deallocate old memory.")
               end if
               intArr => newIntArr
               success = .true.
            end if
         endif
      end function ecSupportAddUniqueInt
   
      ! ==========================================================================
   
      !> Add a char to a set of chars.
      function ecSupportAddUniqueChar(charArr, aChar) result(success)
         logical                                             :: success !< function status
         character(len=maxNameLen), dimension(:), pointer    :: charArr !< array containing unique chars (a set)
         character(len=*),                        intent(in) :: aChar   !< char to be added to the set of chars
         !
         integer                                          :: i          !< loop counter
         integer                                          :: istat      !< deallocate() status
         integer                                          :: lenArr     !< lenght of charArr
         character(len=maxNameLen), dimension(:), pointer :: newCharArr !< larger version of charArr
         !
         success = .false.
         newCharArr => null()
         i = 0
         istat = 1
         lenArr = 0
         !
         !if (.not. associated(charArr)) then
         !   reallocP(charArr, 1, STAT = istat)
         !   if (istat == 0) then
         !      charArr(1) = aChar
         !   else
         !      call setECMessage("ERROR: ec_support::ecSupportAddUniqueChar: Unable to allocate additional memory.")
         !   end if
         !   return
         !end if
         !! 
         !lenArr = size(charArr)
         !do i=1, lenArr
         !   if (charArr(i) == aChar) then
         !      ! This char is already in charArr
         !      success = .true.
         !      return
         !   endif
         !enddo
         !! This char is not yet in charArr, so add it.
         !reallocP(charArr, lenArr+1, STAT = istat)
         !if (istat == 0) then
         !   charArr(lenArr+1) = aChar
         !   success = .true.
         !else
         !   call setECMessage("ERROR: ec_support::ecSupportAddUniqueChar: Unable to allocate additional memory.")
         !end if
      end function ecSupportAddUniqueChar
      
      ! =======================================================================
      ! Find methods
      ! =======================================================================
      
      !> Retrieve the pointer to the Quantity with id == quantityId.
      function ecSupportFindQuantity(instancePtr, quantityId) result(quantityPtr)
         type(tEcQuantity), pointer            :: quantityPtr !< Quantity corresponding to quantityId
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: quantityId  !< unique Quantity id
         !
         integer :: i !< loop counter
         !
         quantityPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nQuantities
               if (instancePtr%ecQuantitiesPtr(i)%ptr%id == quantityId) then
                  quantityPtr => instancePtr%ecQuantitiesPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ERROR: ec_support::ecSupportFindQuantity: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindQuantity
      
      ! =======================================================================
      !> Retrieve the pointer to a NetCDF object by filename 
      function ecSupportFindNetCDFByFilename(instancePtr, ncname) result(netCDFPtr)
         type(tEcNetCDF), pointer            :: netCDFPtr   !< Quantity corresponding to quantityId
         type(tEcInstance), pointer          :: instancePtr !< intent(in)
         character(len=*),  intent(in)       :: ncname      !< netCDF filename 
         !
         integer :: i !< loop counter
         !
         netCDFPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nNetCDFs
               if (instancePtr%ecNetCDFsPtr(i)%ptr%ncname == ncname) then
                  netCDFPtr => instancePtr%ecNetCDFsPtr(i)%ptr
               end if
            end do
         end if
      end function ecSupportFindNetCDFByFilename

      ! =======================================================================
      
      !> Retrieve the pointer to the ElementSet with id == elementSetId.
      function ecSupportFindElementSet(instancePtr, elementSetId) result(elementSetPtr)
         type(tEcElementSet), pointer            :: elementSetPtr !< ElementSet corresponding to elementSetId
         type(tEcInstance),   pointer            :: instancePtr   !< intent(in)
         integer,                     intent(in) :: elementSetId  !< unique ElementSet id
         !
         integer :: i !< loop counter
         !
         elementSetPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nElementSets
               if (instancePtr%ecElementSetsPtr(i)%ptr%id == elementSetId) then
                  elementSetPtr => instancePtr%ecElementSetsPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ERROR: ec_support::ecSupportFindElementSet: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindElementSet
      
      ! =======================================================================
      
      !> Retrieve the pointer to the Field with id == fieldId.
      function ecSupportFindField(instancePtr, fieldId) result(fieldPtr)
         type(tEcField),    pointer            :: fieldPtr    !< Field corresponding to fieldId
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: fieldId     !< unique Field id
         !
         integer :: i !< loop counter
         !
         fieldPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nFields
               if (instancePtr%ecFieldsPtr(i)%ptr%id == fieldId) then
                  fieldPtr => instancePtr%ecFieldsPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ERROR: ec_support::ecSupportFindField: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindField

subroutine ecInstanceListSourceItems(instancePtr,dev)
         implicit none
         ! List source items by quantity and location
         type(tEcInstance), pointer :: instancePtr           !< EC instance 
         integer, intent(in)        :: dev 
         type(tEcItem), pointer     :: sourceItemPtr
         integer  :: ii 
         do ii=1, instancePtr%nItems 
            sourceItemPtr => instancePtr%ecItemsPtr(ii)%ptr
            if (sourceItemPtr%role == itemType_source) then
                     write(dev,'(a,i4.4,a,i1,a)') 'Source Item ',sourceItemPtr%id
                     write(dev,'(a,i4.4,a,i1,a)') '  Quantity = '//trim(sourceItemPtr%quantityPtr%name)
                     write(dev,'(a,i4.4,a,i1,a)') '  Location = '//trim(sourceItemPtr%elementsetPtr%name)
                     write(dev,*) ''
            endif 
         enddo
end subroutine ecInstanceListSourceItems
      
      !! =======================================================================
      !!> Retrieve the item ID given a quantitystring and locationstring
      !!> Loop over all items in the EC instance qualified as 'source' 
      !function ecSupportFindItemByQuantityLocation(instancePtr, quantityname, locationname ) result(itemID)
      !   type(tEcInstance), pointer            :: instancePtr    !< EC-instance
      !   character(len=*), intent(in)         :: quantityname   !< Desired quantity  
      !   character(len=*), intent(in)         :: locationname   !< Desired location 
      !   integer                               :: itemID         !< returned item ID
      !   integer :: i                                            !< loop counter over items 
      !   type (tEcItem), pointer               :: itemPtr
      !   
      !   itemID = -1 
      !   if (associated(instancePtr)) then
      !      do i=1, instancePtr%nItems
      !         itemPtr => instancePtr%ecItemsPtr(i)%ptr
      !         if (itemPtr%role==itemType_source) then
      !            if (itemPtr%quantityPtr%name==quantityname .and. itemPtr%elementSetPtr%name==locationname) then
      !               itemID = itemPtr%id
      !               exit
      !            end if
      !         end if
      !      end do
      !   end if 
      !end function ecSupportFindItemByQuantityLocation

      ! =======================================================================
      !> Retrieve the item ID given a quantitystring and locationstring
      !> Use the fact that each filereader is associated with ONE location, but possibly MULTIPLE quantities
      !> i.e., select filereader first and check its items. 
      function ecSupportFindItemByQuantityLocation(instancePtr, locationname, quantityname ) result(itemID)
         type(tEcInstance), pointer            :: instancePtr    !< EC-instance
         character(len=*), intent(in)          :: quantityname   !< Desired quantity  
         character(len=*), intent(in)          :: locationname   !< Desired location 
         integer                               :: itemID         !< returned item ID
         integer :: i, j                                         !< loop counter over filereader, items 
         type (tEcItem), pointer               :: itemPtr
         type (tEcFileReader), pointer         :: fileReaderPtr
         character(len=:), allocatable         :: quantityname_upper, locationname_upper
!        character(len=maxNameLen)         :: quantityname_upper, locationname_upper
         

         quantityname_upper = trim(quantityname)
         call str_upper(quantityname_upper)
         locationname_upper = trim(locationname)
         call str_upper(locationname_upper)
         itemID = -1 
         if (associated(instancePtr)) then
           frs:do i=1, instancePtr%nFileReaders
               fileReaderPtr => instancePtr%ecFileReadersPtr(i)%ptr
               if (fileReaderPtr%nItems<=0) cycle                                               ! No items to check 
               if (fileReaderPtr%items(1)%ptr%elementSetPtr%name/=locationname_upper) cycle     ! Items have the wrong location 
               do j=1, fileReaderPtr%nItems
                  itemPtr => fileReaderPtr%items(j)%ptr
                  if (itemPtr%quantityPtr%name==quantityname_upper) then
                     itemID = itemPtr%id
                     exit frs
                  end if
               end do 
            end do frs
         end if 
      end function ecSupportFindItemByQuantityLocation
      ! =======================================================================

      !function ecSupportCreateTimeInterpolatedItem(instancePtr, sourceItemId, tgtNdx) result(itemId)
      !    use m_ec_item
      !    use m_ec_converter,  only: ecConverterSetType, ecConverterSetInterpolation, ecConverterSetOperand, ecConverterSetElement
      !    use m_ec_instance,   only: ecInstanceCreateConverter, ecInstanceCreateConnection, ecInstanceCreateItem, ecInstanceCreateField, ecInstanceCreateQuantity
      !    use m_ec_connection, only: ecConnectionAddTargetItem, ecConnectionAddSourceItem, ecConnectionSetConverter 
      !    use m_ec_quantity,   only: ecQuantitySetName
      !    use m_ec_field,      only: ecFieldCreate1dArray
      !    use m_ec_item
      !
      !
      !    type(tEcInstance), pointer    :: instancePtr    !< EC-instance
      !    integer, intent(in)           :: sourceItemId   !< Source item id, before temporal interpolation
      !    integer, intent(in), optional :: tgtNdx         !< Optional target index, 1 is assumed as default
      !    integer                       :: targetItemId   !< Target item id, after temporal interpolation
      !    integer                       :: itemId         !< returned  target item ID, if successful, otherwise -1 
      !    integer                       :: convertId 
      !    type(tECItem), pointer        :: sourceItemPtr => null() 
      !    type(tECItem), pointer        :: targetItemPtr => null()
      !    character(len=:), allocatable :: quantityName
      !    integer                       :: arraySize
      !
      !    integer :: targetIndex 
      !    integer :: converterId, connectionId, quantityId, elementSetId, fieldId
      !    
      !    if (present(tgtNdx)) then 
      !       targetIndex = tgtNdx
      !    else
      !       targetIndex = 1
      !    end if 
      !
      !    sourceItemPtr => ecSupportFindItem(instancePtr, sourceItemId)
      !
      !    ! TODO: create target item:
      !    !       . elementset-name = source_item's elementset-name
      !    !       . quantity-name = source_item's quantity-name + '-interpolated'
      !    itemId = -1 
      !
      !    ! Set up the target item 
      !    targetItemId = ecInstanceCreateItem(instancePtr)
      !    fieldId = ecInstanceCreateField(instancePtr)
      !
      !    arraySize = size(sourceItemPtr%sourceT0FieldPtr%arr1d)
      !    if (.not. (ecFieldCreate1dArray(instancePtr, fieldId, arraySize))) return
      !
      !    if (.not. ecItemSetRole(instancePtr, targetItemId, itemType_target)) return
      !    if (.not. ecItemSetTargetField(instancePtr, targetItemId, fieldId)) return
      !    if (.not. ecItemSetType(instancePtr, targetItemId, accessType_evaluate)) return 
      !    quantityId = ecInstanceCreateQuantity(instancePtr)
      !    quantityName = trim(sourceItemPtr%quantityPtr%name)
      !    if (.not. ecItemSetQuantity(instancePtr, targetItemId, quantityId)) return
      !    if (.not. (ecQuantitySetName(instancePtr, quantityId, quantityName//'_interpolated'))) return
      !    elementSetId = sourceItemPtr%elementSetPtr%id
      !    if (.not. ecItemSetElementSet(instancePtr, targetItemId, elementSetId)) return
      !
      !    ! Construct a new Converter.
      !    converterId = ecInstanceCreateConverter(instancePtr)
      !
      !    ! Initialize the new Converter.
      !    if (.not. (ecConverterSetType(instancePtr, converterId, convType_uniform))) return
      !    if (.not. (ecConverterSetOperand(instancePtr, converterId, operand_replace_element))) return
      !    if (.not. (ecConverterSetInterpolation(instancePtr, converterId, interpolate_timespace))) return
      !    if (.not. (ecConverterSetElement(instancePtr, converterId, targetIndex))) return
      !
      !    ! Construct a new Connection.
      !    connectionId = ecInstanceCreateConnection(instancePtr)
      !    if (.not. ecConnectionSetConverter(instancePtr, connectionId, converterId)) return
      !
      !    ! Initialize the new Connection.
      !    if (.not. ecConnectionAddSourceItem(instancePtr, connectionId, sourceItemId)) return
      !    if (.not. ecConnectionAddTargetItem(instancePtr, connectionId, targetItemId)) return
      !    if (.not. ecItemAddConnection(instancePtr, targetItemId, connectionId)) return
      !    itemId = targetItemId
      !end function ecSupportCreateTimeInterpolatedItem
      ! =======================================================================

      
      !> Retrieve the pointer to the Item with id == itemId.
      function ecSupportFindItem(instancePtr, itemId) result(itemPtr)
         type(tEcItem),     pointer            :: itemPtr     !< Item corresponding to itemId
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: itemId      !< unique Item id
         !
         integer :: i !< loop counter
         !
         itemPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nItems
               if (instancePtr%ecItemsPtr(i)%ptr%id == itemId) then
                  itemPtr => instancePtr%ecItemsPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ERROR: ec_support::ecSupportFindItem: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindItem
      
      ! =======================================================================
      
      !> Retrieve the pointer to the Connection with id == connectionId.
      function ecSupportFindConnection(instancePtr, connectionId) result(connectionPtr)
         type(tEcConnection),     pointer            :: connectionPtr !< Item corresponding to connectionId
         type(tEcInstance),       pointer            :: instancePtr   !< intent(in)
         integer,                         intent(in) :: connectionId  !< unique Connection id
         !
         integer :: i !< loop counter
         !
         connectionPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nConnections
               if (instancePtr%ecConnectionsPtr(i)%ptr%id == connectionId) then
                  connectionPtr => instancePtr%ecConnectionsPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ERROR: ec_support::ecSupportFindConnection: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindConnection
      
      ! =======================================================================
      
      !> Retrieve the pointer to the Converter with id == converterId.
      function ecSupportFindConverter(instancePtr, converterId) result(converterPtr)
         type(tEcConverter), pointer            :: converterPtr !< Item corresponding to converterId
         type(tEcInstance),  pointer            :: instancePtr  !< intent(in)
         integer,                    intent(in) :: converterId  !< unique Converter id
         !
         integer :: i !< loop counter
         !
         converterPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nConverters
               if (instancePtr%ecConvertersPtr(i)%ptr%id == converterId) then
                  converterPtr => instancePtr%ecConvertersPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ERROR: ec_support::ecSupportFindConverter: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindConverter
      
      ! =======================================================================
      
      !> Retrieve the pointer to the FileReader with id == converterId.
      function ecSupportFindFileReader(instancePtr, fileReaderId) result(fileReaderPtr)
         type(tEcFileReader), pointer            :: fileReaderPtr !< FileReader corresponding to fileReaderId
         type(tEcInstance),   pointer            :: instancePtr   !< intent(in)
         integer,                     intent(in) :: fileReaderId  !< unique FileReader id
         !
         integer :: i !< loop counter
         !
         fileReaderPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nFileReaders
               if (instancePtr%ecFileReadersPtr(i)%ptr%id == fileReaderId) then
                  fileReaderPtr => instancePtr%ecFileReadersPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ERROR: ec_support::ecSupportFindFileReader: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindFileReader
      
      ! =======================================================================

      !> Retrieve the pointer to the BCBlock with id == bcBlockId.
      function ecSupportFindBCBlock(instancePtr, bcBlockId) result(bcBlockPtr)
         type(tEcBCBlock), pointer               :: bcBlockPtr    !< BCBlock corresponding to bcBlockId
         type(tEcInstance), pointer              :: instancePtr   !< intent(in)
         integer, intent(in)                     :: bcBlockId     !< unique BCBlock id
         !
         integer :: i !< loop counter
         !
         bcBlockPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nBCBlocks
               if (instancePtr%ecBCBlocksPtr(i)%ptr%id == bcBlockId) then
                  bcBlockPtr => instancePtr%ecBCBlocksPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ERROR: ec_support::ecSupportFindBCBlock: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindBCBlock

      ! =======================================================================

      !> Retrieve the pointer to the BCBlock with id == bcBlockId.
      function ecSupportFindNetCDF(instancePtr, netCDFId) result(netCDFPtr)
         type(tEcNetCDF), pointer                :: netCDFPtr     !< NetCDF instance for the given Id
         type(tEcInstance), pointer              :: instancePtr   !< intent(in)
         integer, intent(in)                     :: netCDFId      !< unique NetCDF id
         !
         integer :: i !< loop counter
         !
         netCDFPtr => null()
         !
         if (associated(instancePtr)) then
            do i=1, instancePtr%nNetCDFs
               if (instancePtr%ecNetCDFsPtr(i)%ptr%id == netCDFId) then
                  netCDFPtr => instancePtr%ecNetCDFsPtr(i)%ptr
               end if
            end do
         else
            call setECMessage("ERROR: ec_support::ecSupportFindNetCDF: Dummy argument instancePtr is not associated.")
         end if
      end function ecSupportFindNetCDF

      ! =======================================================================
      !> Find the file reader for the bc block that contains the component definition for the comp.correction block
      function ecSupportFindRelatedBCBlock(instancePtr, corrFileReaderPtr, qname, bcname, func) result(cmpFileReaderPtr)
         type(tEcFileReader), pointer :: cmpFileReaderPtr  !< resulting file reader
         type(tEcInstance),   pointer :: instancePtr       !< intent(in)
         type(tEcFileReader), pointer :: corrFileReaderPtr !< intent(inout)
         character(len=*)             :: qname             !< quantity name
         character(len=*)             :: bcname            !< point on poly name
         integer, intent(in)          :: func              !< function type                  

         integer                      :: iFileReader
         type (tEcBCBlock), pointer   :: BCBlockptr 
         !
         cmpFileReaderPtr => null()
         do iFileReader = 1, instancePtr%nFileReaders
            BCBlockptr => instancePtr%EcFileReadersPtr(iFileReader)%ptr%bc
            if (associated(BCBlockptr)) then 
               if (trim(BCBlockptr%bcname)==trim(bcname) .and. (trim(BCBlockptr%qname)==trim(qname))  &
                                                         .and. BCBlockptr%func == func) then 
                  cmpFileReaderPtr => instancePtr%EcFileReadersPtr(iFileReader)%ptr 
                  exit 
               endif 
            else 
            endif 
         enddo 
      end function ecSupportFindRelatedBCBlock

      
      !> Translate NetCDF error code into a NetCDF error message.
      function ecSupportNetcdfCheckError(ierror, description, filename) result(success)
         use netcdf
         !
         logical                                   :: success     !< 
         integer,                       intent(in) :: ierror      !< 
         character(len=*),              intent(in) :: description !< 
         character(len=maxFileNameLen), intent(in) :: filename    !< 
         !
         character(3000) :: message
         !
         if (ierror /= nf90_noerr) then
            write (message,'(6a)') 'ERROR ', trim(description), '. NetCDF file : "', trim(filename), '". Error message:', nf90_strerror(ierror)
            call setECMessage(message)
            success = .false.
         else
            success = .true.
         endif
      end function ecSupportNetcdfCheckError

      ! =======================================================================
      
      !> Extracts time unit and reference date from a standard time string.
      !! ASCII example: "TIME = 0 hours since 2006-01-01 00:00:00 +00:00"
      !! NetCDF example: "minutes since 1970-01-01 00:00:00.0 +0000"
      function ecSupportTimestringToUnitAndRefdate(string, unit, ref_date) result(success)
         use netcdf
         use time_module
         !
         logical                          :: success  !< function status
         character(len=*),   intent(in)   :: string   !< units string
         integer,  optional, intent(out)  :: unit     !< time unit enumeration
         real(hp), optional, intent(out)  :: ref_date !< reference date formatted as Modified Julian Date
         !
         integer :: i        !< helper index
         integer :: temp     !< helper variable
         integer :: yyyymmdd !< reference date as Gregorian yyyymmdd
         !
         success = .false.
         yyyymmdd = 0
         !
         call str_lower(string)
         ! Determine the time unit.
         if (present(unit)) then
            if (index(string, 'seconds') /= 0) then
               unit = ec_second
            else if (index(string, 'minutes') /= 0) then
               unit = ec_minute
            else if (index(string, 'hours') /= 0 .or. index( string, 'hrs') /= 0) then
               unit = ec_hour
            else
               call setECMessage("ERROR: ec_support::ecSupportTimestringToUnitAndRefdate: Unable to identify the time unit.")
               return
            end if
         end if
         ! Determine the reference date.
         if (present(ref_date)) then
            i = index(string, 'since') + 6
            if (i /= 0) then
               ! Date
               read(string(i : i+4), '(I4)') temp
               yyyymmdd = yyyymmdd + 10000*temp
               read(string(i+5 : i+7), '(I2)') temp
               yyyymmdd = yyyymmdd + 100*temp
               read(string(i+8 : i+10), '(I2)') temp
               yyyymmdd = yyyymmdd + temp
               ref_date = real(ymd2jul(yyyymmdd)) - 2400000.5_hp ! Julian Day to Modified Julian Date.
               ! Time
               read(string(i+11 : i+13), '(I2)') temp
               ref_date = ref_date + dble(temp) / 24.0_hp
               read(string(i+14 : i+16), '(I2)') temp
               ref_date = ref_date + dble(temp) / 24.0_hp / 60.0_hp
               read(string(i+17 : i+19), '(I2)') temp
               ref_date = ref_date + dble(temp) / 24.0_hp / 60.0_hp / 60.0_hp
            else
               call setECMessage("ERROR: ec_support::ecSupportTimestringToUnitAndRefdate: Unable to identify keyword: since.")
               return
            end if
         end if
         !
         success = .true.
      end function ecSupportTimestringToUnitAndRefdate

      ! =======================================================================
      
      !> Calculate conversion factor from ec_timestep_unit to seconds
      function ecSupportTimeUnitConversionFactor(unit) result(factor)
         integer             :: factor
         integer, intent(in) :: unit !< time unit enum
         !
         factor = 1 ! default return value
         !
         if (unit == ec_second) then
            factor = 1
         else if (unit == ec_minute) then
            factor = 60
         else if (unit == ec_hour) then
            factor = 3600
         end if
      end function ecSupportTimeUnitConversionFactor

      ! =======================================================================
      
      !> Convert times(i) * ec_timestep_unit since ec_refdate to seconds since k_refdate.
      function ecSupportTimeToTimesteps(tframe, index) result(timesteps)
         real(hp)                       :: timesteps !< function result, seconds since k_refdate
         type(tEcTimeFrame), intent(in) :: tframe    !< TimeFrame containing input data for conversion
         integer,            intent(in) :: index     !< index in times array, indicating which time needs to be converted
         !
         integer :: factor !< conversion factor from ec_timestep_unit to seconds
         !
         factor = ecSupportTimeUnitConversionFactor(tframe%ec_timestep_unit)
         !
         timesteps = tframe%times(index) * factor + (tframe%ec_refdate - tframe%k_refdate) * 60.0_hp*60.0_hp*24.0_hp
         !
         ! Correct for Kernel's timzone.
         timesteps = timesteps + tframe%k_timezone*3600.0_hp
      end function ecSupportTimeToTimesteps

      ! =======================================================================
      
      !> Convert thistime * ec_timestep_unit since ec_refdate to seconds since k_refdate.
      function ecSupportThisTimeToTimesteps(tframe, thistime) result(timesteps)
         real(hp)                       :: timesteps !< function result, seconds since k_refdate
         type(tEcTimeFrame), intent(in) :: tframe    !< TimeFrame containing input data for conversion
         real(hp),           intent(in) :: thistime  !< this time needs to be converted
         !
         integer :: factor_in    !< conversion factor from ec_timestep_unit to seconds    (EC-module)
         integer :: factor_out   !< conversion factor from k_timestep_unit to seconds     (Kernel)
         integer :: factor       !< resulting conversion factor 
         !
         if (tframe%k_refdate > (-1.0d+0 + 1.0d-10)) then
            ! convert time stamp in file (*.tmp) to kernel time stamp
            factor_in = ecSupportTimeUnitConversionFactor(tframe%ec_timestep_unit)
            factor_out = ecSupportTimeUnitConversionFactor(tframe%k_timestep_unit)
            factor = real(factor_in)/real(factor_out)
            !
            timesteps = thistime * factor + (tframe%ec_refdate - tframe%k_refdate) * 60.0_hp*60.0_hp*24.0_hp
            !
            ! Correct for Kernel's timzone.
            timesteps = timesteps + tframe%k_timezone*3600.0_hp
         else
            ! no kernel ref date defined, convert to modified julian day
            factor_in = ecSupportTimeUnitConversionFactor(tframe%ec_timestep_unit)
            timesteps = tframe%ec_refdate + factor_in * thistime / 86400.0_hp
         endif
         
      end function ecSupportThisTimeToTimesteps

end module m_ec_support

! =============================================================================

!> This module contains the allocation methods for the EC-module's pointer arrays.
module m_ec_alloc
   use m_ec_typedefs
   use m_ec_message
   
   implicit none
   
   private
   
   public :: ecArrayIncrease
   
   interface ecArrayIncrease
      module procedure ecConnectionPtrArrayIncrease
      module procedure ecConverterPtrArrayIncrease
      module procedure ecElementSetPtrArrayIncrease
      module procedure ecFieldPtrArrayIncrease
      module procedure ecFileReaderPtrArrayIncrease
      module procedure ecBCBlockPtrArrayIncrease
      module procedure ecNetCDFPtrArrayIncrease
      module procedure ecItemPtrArrayIncrease
      module procedure ecQuantityPtrArrayIncrease
   end interface ecArrayIncrease
   
   contains
      
      !> Increases the size of an array of tEcConnectionPtr instances by 10.
      function ecConnectionPtrArrayIncrease(ptr, nConnections) result(success)
         logical                                       :: success      !< function status
         type(tEcConnectionPtr), dimension(:), pointer :: ptr          !< intent(inout)
         integer                                       :: nConnections !< Number of tEcConnectionPtrs =< size(ptr)
         !
         integer                                       :: istat   !< allocate() status
         type(tEcConnectionPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                       :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ERROR: ec_alloc::ecConnectionPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nConnections
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ERROR: ec_alloc::ecConnectionPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecConnectionPtrArrayIncrease
      
      ! =======================================================================
      
      !> Increases the size of an array of tEcConverterPtr instances by 10.
      function ecConverterPtrArrayIncrease(ptr, nConverters) result(success)
         logical                                      :: success      !< function status
         type(tEcConverterPtr), dimension(:), pointer :: ptr          !< intent(inout)
         integer                                      :: nConverters  !< Number of tEcConverterPtrs =< size(ptr)
         !
         integer                                      :: istat   !< allocate() status
         type(tEcConverterPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                      :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ERROR: ec_alloc::ecConverterPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nConverters
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ERROR: ec_alloc::ecConverterPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecConverterPtrArrayIncrease
      
      ! =======================================================================
      
      !> Increases the size of an array of tEcElementSetPtr instances by 10.
      function ecElementSetPtrArrayIncrease(ptr, nElementSets) result(success)
         logical                                       :: success      !< function status
         type(tEcElementSetPtr), dimension(:), pointer :: ptr          !< intent(inout)
         integer                                       :: nElementSets !< Number of tEcElementSetPtrs =< size(ptr)
         !
         integer                                       :: istat   !< allocate() status
         type(tEcElementSetPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                       :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ERROR: ec_alloc::ecElementSetPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nElementSets
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ERROR: ec_alloc::ecElementSetPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecElementSetPtrArrayIncrease
      
      ! =======================================================================
      
      !> Increases the size of an array of tEcFieldPtr instances by 10.
      function ecFieldPtrArrayIncrease(ptr, nFields) result(success)
         logical                                  :: success !< function status
         type(tEcFieldPtr), dimension(:), pointer :: ptr     !< intent(inout)
         integer                                  :: nFields !< Number of tEcFieldPtrs =< size(ptr)
         !
         integer                                  :: istat   !< allocate() status
         type(tEcFieldPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                  :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ERROR: ec_alloc::ecFieldPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nFields
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ERROR: ec_alloc::ecFieldPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecFieldPtrArrayIncrease
      
      ! =======================================================================
      
      !> Increases the size of an array of tEcFileReaderPtr instances by 10.
      function ecFileReaderPtrArrayIncrease(ptr, nFileReaders) result(success)
         logical                                       :: success      !< function status
         type(tEcFileReaderPtr), dimension(:), pointer :: ptr          !< intent(inout)
         integer                                       :: nFileReaders !< Number of tEcFileReaderPtrs =< size(ptr)
         !
         integer                                       :: istat   !< allocate() status
         type(tEcFileReaderPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                       :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ERROR: ec_alloc::ecFileReaderPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nFileReaders
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ERROR: ec_alloc::ecFileReaderPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecFileReaderPtrArrayIncrease

      ! =======================================================================
      
      !> Increases the size of an array of tEcBCBlockPtr instances by 10.
      function ecBCBlockPtrArrayIncrease(ptr, nBCBlocks) result(success)
         logical                                       :: success      !< function status
         type(tEcBCBlockPtr), dimension(:), pointer    :: ptr          !< intent(inout)
         integer                                       :: nBCBlocks    !< Number of tEcBCBlockPtrs =< size(ptr)
         !
         integer                                       :: istat   !< allocate() status
         type(tEcBCBlockPtr), dimension(:), pointer    :: new_ptr !< new array
         integer                                       :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ERROR: ec_alloc::ecBCBlockPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nBCBlocks
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ERROR: ec_alloc::ecBCBlockPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecBCBlockPtrArrayIncrease
      
      ! =======================================================================
      
      !> Increases the size of an array of tEcNetCDFPtr instances by 10.
      function ecNetCDFPtrArrayIncrease(ptr, nNetCDFs) result(success)
         logical                                       :: success      !< function status
         type(tEcNetCDFPtr), dimension(:), pointer     :: ptr          !< intent(inout)
         integer                                       :: nNetCDFs     !< Number of tEcBCBlockPtrs =< size(ptr)
         !
         integer                                       :: istat   !< allocate() status
         type(tEcNetCDFPtr), dimension(:), pointer     :: new_ptr !< new array
         integer                                       :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ERROR: ec_alloc::ecNetCDFPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nNetCDFs
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ERROR: ec_alloc::ecNetCDFPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecNetCDFPtrArrayIncrease
      
      ! =======================================================================

      !> Increases the size of an array of tEcItemPtr instances by 10.
      function ecItemPtrArrayIncrease(ptr, nItems) result(success)
         logical                                 :: success !< function status
         type(tEcItemPtr), dimension(:), pointer :: ptr     !< intent(inout)
         integer                                 :: nItems  !< Number of tEcItemPtrs =< size(ptr)
         !
         integer                                 :: istat   !< allocate() status
         type(tEcItemPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                 :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ERROR: ec_alloc::ecItemPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nItems
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ERROR: ec_alloc::ecItemPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecItemPtrArrayIncrease
      
      ! =======================================================================
      
      !> Increases the size of an array of tEcQuantityPtr instances by 10.
      function ecQuantityPtrArrayIncrease(ptr, nQuantitys) result(success)
         logical                                     :: success    !< function status
         type(tEcQuantityPtr), dimension(:), pointer :: ptr        !< intent(inout)
         integer                                     :: nQuantitys !< Number of tEcQuantityPtrs =< size(ptr)
         !
         integer                                     :: istat   !< allocate() status
         type(tEcQuantityPtr), dimension(:), pointer :: new_ptr !< new array
         integer                                     :: i       !< loop counter
         !
         success = .false.
         istat = 1
         !
         if (.not. associated(ptr)) then
            call setECMessage("ERROR: ec_alloc::ecQuantityPtrArrayIncrease: Dummy argument ptr is not associated.")
         else
            allocate(new_ptr(size(ptr)+10), STAT = istat)
            if (istat == 0) then
               do i=1, nQuantitys
                  new_ptr(i)%ptr => ptr(i)%ptr
                  ptr(i)%ptr => null()
               end do
               ptr => new_ptr
               new_ptr => null()
               success = .true.
            else
               call setECMessage("ERROR: ec_alloc::ecQuantityPtrArrayIncrease: Unable to allocate additional memory.")
            end if
         end if
      end function ecQuantityPtrArrayIncrease

end module m_ec_alloc
