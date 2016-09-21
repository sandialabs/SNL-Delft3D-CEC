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

!  $Id: ec_item.f90 5640 2015-12-10 09:24:34Z hummel $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/ec_module/packages/ec_module/src/ec_item.f90 $

!> This module contains all the methods for the datatype tEcItem.
!! @author arjen.markus@deltares.nl
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.bos@deltares.nl
module m_ec_item
   use m_ec_typedefs
   use m_ec_parameters
   use m_ec_message
   use m_ec_support
   use m_ec_alloc
   use m_ec_converter
   use m_ec_filereader
   
   implicit none
   
   private
   
   public :: ecItemCreate
   public :: ecItemFree1dArray
   public :: ecItemGetValues
   public :: ecItemSetRole
   public :: ecItemSetType
   public :: ecItemSetQuantity
   public :: ecItemSetProperty
   public :: ecItemSetElementSet
   public :: ecItemSetSourceT0Field
   public :: ecItemSetSourceT1Field
   public :: ecItemSetTargetField
   public :: ecItemAddConnection
   
   contains
      
      ! =======================================================================
      
      !> Construct a new Item with the specified id.
      !! Failure is indicated by returning a null pointer
      function ecItemCreate(itemId) result(itemPtr)
         type(tEcItem), pointer            :: itemPtr !< the new Item, intent(out)
         integer,               intent(in) :: itemId  !< unique Item id
         !
         integer :: istat !< allocate() status
         !
         ! allocation
         allocate(itemPtr, stat = istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_item::ecItemCreate: Unable to allocate additional memory")
            itemPtr => null()
            return
         end if
         allocate(itemPtr%connectionsPtr(1), stat = istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_item::ecItemCreate: Unable to allocate additional memory")
            itemPtr => null()
            return
         end if
         ! initialization
         itemPtr%id = itemId
         itemPtr%role = itemType_undefined
         itemPtr%accessType = accessType_undefined
         itemPtr%nConnections = 0
      end function ecItemCreate
      
      ! =======================================================================
      
      !> Free a tEcItem, after which it can be deallocated.
      function ecItemFree(item) result(success)
         logical                      :: success !< function status
         type(tEcItem), intent(inout) :: item    !< Item to free
         !
         integer :: i !< loop counter
         integer :: istat !< deallocate() status
         !
         success = .true.
         !
         ! An item does not own the tEcQuantities, the tEcInstance does, so nullify rather then ecQuantityFree().
         item%quantityPtr => null()
         ! An item does not own the tEcElementSets, the tEcInstance does, so nullify rather then ecElementSetFree().
         item%elementSetPtr => null()
         ! An item does not own the tEcFields, the tEcInstance does, so nullify rather then ecFieldFree().
         item%sourceT0FieldPtr => null()
         item%sourceT1FieldPtr => null()
         item%targetFieldPtr => null()
         ! An Item does not own the tEcConnections, the tEcInstance does, so nullify rather then ecConnectionFree().
         do i=1, item%nConnections
            item%connectionsPtr(i)%ptr => null()
         end do
         ! Finally deallocate the array of tEcConnection pointers.
         deallocate(item%connectionsPtr, STAT = istat)
         if (istat /= 0) success = .false.
      end function ecItemFree
      
      ! =======================================================================
      
      !> Frees a 1D array of tEcItemPtrs, after which the itemPtr is deallocated.
      function ecItemFree1dArray(itemPtr, nItems) result(success)
         logical                                 :: success !< function status
         type(tEcItemPtr), dimension(:), pointer :: itemPtr !< intent(inout)
         integer                                 :: nItems  !< number of Items
         !
         integer :: i     !< loop counter
         integer :: istat !< deallocate() status
         !
         success = .true.
         !
         if (.not. associated(itemPtr)) then
            call setECMessage("WARNING: ec_item::ecItemFree1dArray: Dummy argument itemPtr is already disassociated.")
         else
            ! Free and deallocate all tEcItemPtrs in the 1d array.
            do i=1, nItems
               if (ecItemFree(itemPtr(i)%ptr)) then
                  deallocate(itemPtr(i)%ptr, stat = istat)
                  if (istat /= 0) success = .false.
               else
                  success = .false.
               end if
            end do
            ! Finally deallocate the tEcItemPtr(:) pointer.
            if (success) then
               deallocate(itemPtr, stat = istat)
               if (istat /= 0) success = .false.
            end if
         end if
      end function ecItemFree1dArray
      
      ! =======================================================================
      
      !> Retrieve the data of an Item for a specific number of timesteps since kernel's reference date and put it in the target Item's Field.
      recursive function ecItemGetValues(instancePtr, itemId, timesteps, target_array) result(success)
         use m_ec_message
         implicit none
         logical                                                 :: success      !< function status
         type(tEcInstance),      pointer                         :: instancePtr  !< intent(in)
         integer,                                  intent(in)    :: itemID       !< unique Item id
         real(hp),                                 intent(in)    :: timesteps    !< get data corresponding to this number of timesteps since k_refdate
         real(hp), dimension(:), target, optional, intent(inout) :: target_array !< kernel's data array for the requested values
         !
         integer                :: i       !< loop counter
         type(tEcItem), pointer :: itemPtr !< Item under consideration
         !
         success = .false.
         !
         ! Find the Item.
         do i=1, instancePtr%nItems ! TODO: This lookup loop of items may be expensive for large models, use a lookup table with ids.
            itemPtr => instancePtr%ecItemsPtr(i)%ptr
! TODO: Edwin: UNST-683: somehow this itemPtr is *not* thread-safe. When called via ec_gettimespacevalue (see unstruc.f90, ec_gettimespacevalue(ecInstancePtr, item_damlevel, tim, zcdam), with OMP SECTIONS)
!       Access violations and irregular end of files occur. Setting OMP_NUM_THREADS=1 'solves' the errors.
            if ((itemPtr%id == itemId) .and. (itemPtr%role == itemType_target)) then
               ! Optionally set the Item's target array.
               if (present(target_array)) then
                  itemPtr%targetFieldPtr%arr1dPtr => target_array
               end if
              ! Update the Item's data if needed.
              ! if (comparereal(itemPtr%targetFieldPtr%timesteps, timesteps) == 0) then
              !    success = .true.
              ! else
                  success = ecItemUpdateTargetItem(instancePtr, itemPtr, timesteps)
                  if (.not.success) then
                     call setECMessage("Updating target failed, quantity='"//trim(itemPtr%QUANTITYPTR%NAME)//"', item=",itemId)
                  endif 
              ! end if
               exit
            end if
         end do
      end function ecItemGetValues
      
      ! =======================================================================
      
      !> Retrieve data for a specific number of timesteps since the kernel's reference date by first updating the source Items.
      !! Their data is processed through a Converter, which updates the Field of each of the Connection's target Items.
      function ecItemUpdateTargetItem(instancePtr, item, timesteps) result(success)
         logical                          :: success     !< function status
         type(tEcInstance), pointer       :: instancePtr !< intent(inout)
         type(tEcItem),     intent(inout) :: item        !< the target item
         real(hp),          intent(in)    :: timesteps   !< get data corresponding to this number of timesteps since k_refdate
         !
         integer :: i, j !< loop variables
         character(len=1000)              :: message
         !
         success = .true.
         !
         ! update the source Items
         do i=1, item%nConnections
            do j=1, item%connectionsPtr(i)%ptr%nSourceItems
               if (.not. (ecItemUpdateSourceItem(instancePtr, item%connectionsPtr(i)%ptr%sourceItemsPtr(j)%ptr, timesteps, &
                          item%connectionsPtr(i)%ptr%converterPtr%interpolationType))) then
                  !
                  ! No interpolation in time possible.
                  ! Check whether extrapolation is allowed
                  if (item%connectionsPtr(i)%ptr%converterPtr%interpolationType /= interpolate_time_extrapolation_ok) then
                     write(message,'(a,i5.5)') "Updating source failed, quantity='"//trim(item%connectionsPtr(i)%ptr%sourceItemsPtr(j)%ptr%QUANTITYPTR%NAME)   &
                              &       //"', item=",item%connectionsPtr(i)%ptr%sourceItemsPtr(j)%ptr%id
                     call setECMessage(trim(message))
                     success = .false.
                     return
                  end if
               end if
            end do
         end do
         ! update the weight factors
         if (success) then
            do i=1, item%nConnections
               if (.not. (ecConverterUpdateWeightFactors(instancePtr, item%connectionsPtr(i)%ptr))) then
                     write(message,'(a,i5.5)') "Updating weights failed, connection='",item%connectionsPtr(i)%ptr%id
                     call setECMessage(trim(message))
                     success = .false.
                  success = .false.
                  return
               end if
            end do
         end if
         ! Always try to perform the conversions, which update the target Items
         if (success) then
            do i=1, item%nConnections
               if (.not. (ecConverterPerformConversions(item%connectionsPtr(i)%ptr, timesteps))) then
                     write(message,'(a,i5.5)') "Converter operation failed, connection='",item%connectionsPtr(i)%ptr%id
                     call setECMessage(trim(message))
                  success = .false.
                  return
               end if
            end do
         end if
      end function ecItemUpdateTargetItem
      
      ! =======================================================================
      
      !> Retrieve data from a FileReader as needed to achieve t0<=timesteps<=t1.
      function ecItemUpdateSourceItem(instancePtr, item, timesteps, interpol_type) result(success)
         logical                                  :: success       !< function status
         type(tEcInstance), pointer               :: instancePtr   !< intent(inout)
         type(tEcItem),             intent(inout) :: item          !< the source item
         real(hp),                  intent(in)    :: timesteps     !< objective: t0<=timesteps<=t1
         integer ,                  intent(in)    :: interpol_type !< interpolation
         !
         integer                        :: i                       !< loop counter
         integer                        :: j                       !< loop counter
         type(tEcFileReader), pointer   :: fileReaderPtr           !< helper pointer for a file reader 
         character(len=300) :: str
         !
         success = .false.
         fileReaderPtr => null()
         !
         ! Check whether the source Items are actually poly_tim Items of type target.
         if (item%role == itemType_target) then
            if (.not. ecItemGetValues(instancePtr, item%id, timesteps)) then
               return
            end if
            success = .true.
            return
         end if
         !
         if (item%accessType==accessType_fileReader) then 
            ! Find the FileReader which can update this source Item.
            frs: do i=1, instancePtr%nFileReaders
               do j=1, instancePtr%ecFileReadersPtr(i)%ptr%nItems
                  if (instancePtr%ecFileReadersPtr(i)%ptr%items(j)%ptr%id == item%id) then
                     fileReaderPtr => instancePtr%ecFileReadersPtr(i)%ptr
                     exit frs ! exits outer named do loop
                  end if
               end do
            end do frs
         endif 
         !
         
         ! timesteps < t0 : not supported 
         if (comparereal(timesteps, item%sourceT0FieldPtr%timesteps) == -1) then
            if (interpol_type /= interpolate_time_extrapolation_ok) then
               write(str, '(a,f13.3,a,f8.1,a,a)') "             Requested: t=", timesteps, ' seconds'
               call setECMessage(str)
               write(str, '(a,f13.3,a,f8.1,a,a)') "       Current EC-time: t=", item%sourceT0FieldPtr%timesteps,' seconds'
               call setECMessage(str)
               write(str, '(a,i0,a,f10.3,a,a)')    "Requested time preceeds current forcing EC-timelevel by ", &
                   &        int(item%sourceT0FieldPtr%timesteps-timesteps)," seconds = ", (item%sourceT0FieldPtr%timesteps-timesteps)/86400.," days."
               call setECMessage(str)
            else
               success = .true.
            endif
         ! t0<=timesteps<=t1 : no update required
         else if (comparereal(item%sourceT1FieldPtr%timesteps, timesteps) /= -1) then
            success = .true.
         ! timesteps > t1: update untill t0<=timesteps<=t1
         else
            ! Update all source Items which belong to the found FileReader, if associated .
            if (associated(fileReaderPtr)) then
               if (.not. fileReaderPtr%end_of_data) then
                  do ! read next record untill t0<=timesteps<=t1
                     if (ecFileReaderReadNextRecord(fileReaderPtr, timesteps)) then
                        write(6,*) 'Read OK: ', timesteps
                        if (comparereal(item%sourceT1FieldPtr%timesteps, timesteps) /= -1) then
                           write(6,*) 'Read OK: ', timesteps, ' success = .true., exit'
                           success = .true.
                           exit
                        end if
                     else
                        write(6,*) 'Read NOT OK: ', timesteps
                        if (interpol_type == interpolate_time_extrapolation_ok) then
                           write(6,*) 'Read NOT OK: interpolate_time_extrapolation_ok, exit'
                           exit
                        end if
                     end if
                  end do
               endif
            end if
         end if
      end function ecItemUpdateSourceItem
      
      ! =======================================================================
      ! Set methods
      ! =======================================================================
      !> Place holder for setting several properties of an item in one go,
      !> all specified as optional arguments, which should be passed with names 
      !> (RL: Now only used for the vectormax, but easily extensible)
      function ecItemSetProperty(instancePtr, itemId, vectorMax) result(success)
         logical                                 :: success     !< function status
         type(tEcInstance), pointer              :: instancePtr !< intent(in)
         integer,           intent(in)           :: itemId      !< unique Item id
         integer,           intent(in), optional :: vectorMax   !< number of dimensions, in case of multi-dimensional data 
         !
         type(tEcItem), pointer :: itemPtr                      !< Item corresponding to itemId
         !
         success = .false.
         itemPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         if (associated(itemPtr)) then
            if (present(vectorMax)) then 
               itemPtr%quantityPtr%vectorMax = vectorMax
            endif 
            success = .true.
         else
            call setECMessage("ERROR: ec_item::ecItemSetRole: Cannot find an Item with the supplied id.")
         end if
      end function ecItemSetProperty
      
      !> Change the role of the Item corresponding to itemId.
      function ecItemSetRole(instancePtr, itemId, newRole) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: itemId      !< unique Item id
         integer,                   intent(in) :: newRole     !< new role of the Item
         !
         type(tEcItem), pointer :: itemPtr !< Item corresponding to itemId
         !
         success = .false.
         itemPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         if (associated(itemPtr)) then
            itemPtr%role = newRole
            success = .true.
         else
            call setECMessage("ERROR: ec_item::ecItemSetRole: Cannot find an Item with the supplied id.")
         end if
      end function ecItemSetRole
      
      ! =======================================================================
      
      !> Change the accessType of the Item corresponding to itemId.
      function ecItemSetType(instancePtr, itemId, newType) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: itemId      !< unique Item id
         integer,                   intent(in) :: newType     !< new accessType of the Item
         !
         type(tEcItem), pointer :: itemPtr !< Item corresponding to itemId
         !
         success = .false.
         itemPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         if (associated(itemPtr)) then
            itemPtr%accessType = newType
            success = .true.
         else
            call setECMessage("ERROR: ec_item::ecItemSetType: Cannot find an Item with the supplied id.")
         end if
      end function ecItemSetType
      
      ! =======================================================================
      
      !> Assign an existing Quantity to the Item corresponding to itemId.
      function ecItemSetQuantity(instancePtr, itemId, quantityId) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: itemId      !< unique Item id
         integer,                   intent(in) :: quantityId  !< unique Quantity id
         !
         type(tEcItem),     pointer :: itemPtr     !< Item corresponding to itemId
         type(tEcQuantity), pointer :: quantityPtr !< Quantity corresponding to quantityId
         !
         success = .false.
         itemPtr => null()
         quantityPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         quantityPtr => ecSupportFindQuantity(instancePtr, quantityId)
         if (associated(itemPtr) .and. associated(quantityPtr)) then
            itemPtr%quantityPtr => quantityPtr
            success = .true.
         else
            call setECMessage("ERROR: ec_item::ecItemSetQuantity: Cannot find an Item or Quantity with the supplied id.")
         end if
      end function ecItemSetQuantity
      
      ! =======================================================================
      
      !> Assign an existing ElementSet to the Item corresponding to itemId.
      function ecItemSetElementSet(instancePtr, itemId, elementSetId) result(success)
         logical                               :: success      !< function status
         type(tEcInstance), pointer            :: instancePtr  !< intent(in)
         integer,                   intent(in) :: itemId       !< unique Item id
         integer,                   intent(in) :: elementSetId !< unique ElementSet id
         !
         type(tEcItem),       pointer :: itemPtr       !< Item corresponding to itemId
         type(tEcElementSet), pointer :: elementSetPtr !< ElementSet corresponding to elementSetId
         !
         success = .false.
         itemPtr => null()
         elementSetPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         elementSetPtr => ecSupportFindElementSet(instancePtr, elementSetId)
         if (associated(itemPtr) .and. associated(elementSetPtr)) then
            itemPtr%elementSetPtr => elementSetPtr
            success = .true.
         else
            call setECMessage("ERROR: ec_item::ecItemSetElementSet: Cannot find an Item or ElementSet with the supplied id.")
         end if
      end function ecItemSetElementSet
      
      ! =======================================================================
      
      !> Assign an existing Field to the source Item for t=t0.
      function ecItemSetSourceT0Field(instancePtr, itemId, fieldId) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: itemId      !< unique Item id
         integer,                   intent(in) :: fieldId     !< unique Field id
         !
         type(tEcItem),  pointer :: itemPtr  !< Item corresponding to itemId
         type(tEcField), pointer :: fieldPtr !< Field corresponding to fieldId
         !
         success = .false.
         itemPtr => null()
         fieldPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         fieldPtr => ecSupportFindField(instancePtr, fieldId)
         if (associated(itemPtr) .and. associated(fieldPtr)) then
            if (itemPtr%role == itemType_source) then
               itemPtr%sourceT0FieldPtr => fieldPtr
               success = .true.
            else
               call setECMessage("WARNING: ec_item::ecItemSetTargetField: Won't assign to the source Field of a non-source Item.")
            end if
         else
            call setECMessage("ERROR: ec_item::ecItemSetSourceT0Field: Cannot find an Item or Field with the supplied id.")
         end if
      end function ecItemSetSourceT0Field
      
      ! =======================================================================
      
      !> Assign an existing Field to the source Item for t=t1.
      function ecItemSetSourceT1Field(instancePtr, itemId, fieldId) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: itemId      !< unique Item id
         integer,                   intent(in) :: fieldId     !< unique Field id
         !
         type(tEcItem),  pointer :: itemPtr  !< Item corresponding to itemId
         type(tEcField), pointer :: fieldPtr !< Field corresponding to fieldId
         !
         success = .false.
         itemPtr => null()
         fieldPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         fieldPtr => ecSupportFindField(instancePtr, fieldId)
         if (associated(itemPtr) .and. associated(fieldPtr)) then
            if (itemPtr%role == itemType_source) then
               itemPtr%sourceT1FieldPtr => fieldPtr
               success = .true.
            else
               call setECMessage("WARNING: ec_item::ecItemSetTargetField: Won't assign to the source Field of a non-source Item.")
            end if
         else
            call setECMessage("ERROR: ec_item::ecItemSetSourceT1Field: Cannot find an Item or Field with the supplied id.")
         end if
      end function ecItemSetSourceT1Field
      
      ! =======================================================================
      
      !> Assign an existing Field to the target Item.
      function ecItemSetTargetField(instancePtr, itemId, fieldId) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: itemId      !< unique Item id
         integer,                   intent(in) :: fieldId     !< unique Field id
         !
         type(tEcItem),  pointer :: itemPtr  !< Item corresponding to itemId
         type(tEcField), pointer :: fieldPtr !< Field corresponding to fieldId
         !
         success = .false.
         itemPtr => null()
         fieldPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         fieldPtr => ecSupportFindField(instancePtr, fieldId)
         if (associated(itemPtr) .and. associated(fieldPtr)) then
            if (itemPtr%role == itemType_target) then
               itemPtr%targetFieldPtr => fieldPtr
               success = .true.
            else
               call setECMessage("WARNING: ec_item::ecItemSetTargetField: Won't assign to the target Field of a non-target Item.")
            end if
         else
            call setECMessage("ERROR: ec_item::ecItemSetTargetField: Cannot find an Item or Field with the supplied id.")
         end if
      end function ecItemSetTargetField
      
      ! =======================================================================
      
      !> Add a Connection an Item's array of Connections.
      function ecItemAddConnection(instancePtr, itemId, connectionId) result(success)
         logical                               :: success      !< function status
         type(tEcInstance), pointer            :: instancePtr  !< intent(in)
         integer,                   intent(in) :: itemId       !< unique Item id
         integer,                   intent(in) :: connectionId !< additional Connection for the Item
         !
         type(tEcItem),       pointer :: itemPtr       !< Item corresponding to itemId
         type(tEcConnection), pointer :: connectionPtr !< Connection corresponding to connectionId
         !
         success = .false.
         itemPtr => null()
         connectionPtr => null()
         !
         itemPtr => ecSupportFindItem(instancePtr, itemId)
         connectionPtr => ecSupportFindConnection(instancePtr, connectionId)
         if (associated(itemPtr) .and. associated(connectionPtr)) then
            if (itemPtr%role == itemType_target) then
               ! ensure capacity
               if (itemPtr%nConnections == size(itemPtr%connectionsPtr)) then
                  if (.not. ecArrayIncrease(itemPtr%connectionsPtr, itemPtr%nConnections)) then
                     return
                  end if
               end if
               itemPtr%nConnections = itemPtr%nConnections + 1
               itemPtr%connectionsPtr(itemPtr%nConnections)%ptr => connectionPtr
               success = .true.
            else
               call setECMessage("WARNING: ec_item::ecItemAddConnection: Won't add a Connection to a non-target Item.")
            end if
         else
            call setECMessage("ERROR: ec_item::ecItemAddConnection: Cannot find an Item or Connection with the supplied id.")
         end if
      end function ecItemAddConnection
end module m_ec_item
