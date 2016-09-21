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

!  $Id: ec_field.f90 5609 2015-11-25 17:21:04Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/ec_module/packages/ec_module/src/ec_field.f90 $

!> This module contains all the methods for the datatype tEcField.
!! @author arjen.markus@deltares.nl
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.bos@deltares.nl
module m_ec_field
   use m_ec_typedefs
   use m_ec_message
   use m_ec_support
   use m_ec_alloc
   
   implicit none
   
   private
   
   public :: ecFieldCreate
   public :: ecFieldFree1dArray
   public :: ecFieldSetMissingValue
   public :: ecFieldSet1dArray
   public :: ecFieldCreate1dArray
   
   contains
      
      ! =======================================================================
      
      !> Construct a new Field with the specified id.
      !! Failure is indicated by returning a null pointer.
      function ecFieldCreate(fieldId) result(fieldPtr)
         type(tEcField), pointer            :: fieldPtr !< the new Field, intent(out)
         integer,                intent(in) :: fieldId  !< unique Field id
         !
         integer :: istat !< allocate() status
         !
         ! allocation
         allocate(fieldPtr, stat = istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_field::ecFieldCreate: Unable to allocate additional memory")
            fieldPtr => null()
            return
         end if
         ! The arr1d array is allocated on-demand.
         !
         ! initialization
         fieldPtr%id = fieldId
         fieldPtr%timesteps = ec_undef_hp
         fieldPtr%missingValue = ec_undef_hp
         fieldPtr%x_spw_eye = ec_undef_hp
         fieldPtr%y_spw_eye = ec_undef_hp
      end function ecFieldCreate
      
      ! =======================================================================
      
      !> Free a tEcField, after which it can be deallocated.
      function ecFieldFree(field) result (success)
         logical                       :: success !< function status
         type(tEcField), intent(inout) :: field   !< Field to free
         !
         integer :: istat !< deallocate() status
         !
         success = .true.
         ! The array is not dynamically allocated so nullify.
         field%arr1dPtr => null()
         ! Either this array is used, or the above pointer points to an array outside the EC-module.
         if (allocated(field%arr1d)) then
            deallocate(field%arr1d, stat = istat)
            if (istat /= 0) success = .false.
         end if
         if (allocated(field%astro_components)) then
            deallocate(field%astro_components, stat = istat)
            if (istat /= 0) success = .false.
         end if            
      end function ecFieldFree
      
      ! =======================================================================
      
      !> Frees a 1D array of tEcFieldPtrs, after which the fieldPtr is deallocated.
      function ecFieldFree1dArray(fieldPtr, nFields) result (success)
         logical                                  :: success  !< function status
         type(tEcFieldPtr), dimension(:), pointer :: fieldPtr !< intent(inout)
         integer                                  :: nFields  !< number of Fields
         !
         integer :: i      !< loop counter
         integer :: istat  !< deallocate() status
         !
         success = .true.
         !
         if (.not. associated(fieldPtr)) then
            call setECMessage("WARNING: ec_field::ecFieldFree1dArray: Dummy argument fieldPtr is already disassociated.")
         else
            ! Free and deallocate all tEcFieldPtrs in the 1d array.
            do i=1, nFields
               if (ecFieldFree(fieldPtr(i)%ptr)) then
                  deallocate(fieldPtr(i)%ptr, stat = istat)
                  if (istat /= 0) success = .false.
               else
                  success = .false.
               end if
            end do
            ! Finally deallocate the tEcFieldPtr(:) pointer.
            if (success) then
               deallocate(fieldPtr, stat = istat)
               if (istat /= 0) success = .false.
            end if
         end if
      end function ecFieldFree1dArray
      
      ! =======================================================================
      
      !> Change the missing value of the Field corresponding to fieldId.
      function ecFieldSetMissingValue(instancePtr, fieldId, missingValue) result(success)
         logical                               :: success      !< function status
         type(tEcInstance), pointer            :: instancePtr  !< intent(in)
         integer,                   intent(in) :: fieldId      !< unique Field id
         real(hp),                  intent(in) :: missingValue !< new missing value of the Field
         !
         type(tEcField), pointer :: fieldPtr !< Field corresponding to fieldId
         !
         success = .false.
         fieldPtr => null()
         fieldPtr => ecSupportFindField(instancePtr, fieldId)
         if (associated(fieldPtr)) then
            fieldPtr%missingValue = missingValue
            success = .true.
         else
            call setECMessage("ERROR: ec_field::ecFieldSetMissingValue: Cannot find a Field with the supplied id.")
         end if
      end function ecFieldSetMissingValue
      
      ! =======================================================================
      
      !> Associate a Field's 1D array pointer with an external 1D data array.
      !! This allows a target Item to write directly into a kernel's data array.
      function ecFieldSet1dArray(instancePtr, fieldId, arrayPtr) result(success)
         logical                                    :: success     !< function status
         type(tEcInstance),      pointer            :: instancePtr !< intent(in)
         integer,                        intent(in) :: fieldId     !< unique Field id
         real(hp), dimension(:), pointer            :: arrayPtr    !< external 1D array data pointer
         !
         type(tEcField), pointer :: fieldPtr !< Field corresponding to fieldId
         !
         success = .false.
         fieldPtr => null()
         !
         fieldPtr => ecSupportFindField(instancePtr, fieldId)
         if (associated(fieldPtr)) then
            fieldPtr%arr1dPtr => arrayPtr
            success = .true.
         else
            call setECMessage("ERROR: ec_field::ecFieldSet1dArray: Cannot find a Field with the supplied id.")
         end if
      end function ecFieldSet1dArray
      
      ! =======================================================================
      
      !> Allocate the Field's 1D array and associate the Field's 1D array pointer with it.
      function ecFieldCreate1dArray(instancePtr, fieldId, arraySize) result(success)
         logical                               :: success     !< function status
         type(tEcInstance), pointer            :: instancePtr !< intent(in)
         integer,                   intent(in) :: fieldId     !< unique Field id
         integer,                   intent(in) :: arraySize   !< requested size of 1D array
         !
         type(tEcField), pointer :: fieldPtr !< Field corresponding to fieldId
         integer                 :: istat    !< (de)allocate status
         !
         success = .true.
         fieldPtr => null()
         !
         fieldPtr => ecSupportFindField(instancePtr, fieldId)
         if (associated(fieldPtr)) then
            ! Reallocate 1D real(hp) array.
            if (allocated(fieldPtr%arr1d)) then
               deallocate(fieldPtr%arr1d, stat = istat)
               if (istat /= 0) then
                  call setECMessage("ERROR: ec_field::ecFieldCreate1dArray: Unable to deallocate memory.")
                  success = .false.
               end if
            end if
            if (success) then
               allocate(fieldPtr%arr1d(arraySize), stat = istat)
               if (istat /= 0) then
                  call setECMessage("ERROR: ec_field::ecFieldCreate1dArray: Unable to allocate additional memory.")
                  success = .false.
               else
                  fieldPtr%arr1d = ec_undef_hp
               end if
            end if
            if (success) then
               fieldPtr%arr1dPtr => fieldPtr%arr1d
            end if
         else
            call setECMessage("ERROR: ec_field::ecFieldCreate1dArray: Cannot find a Field with the supplied id.")
         end if
      end function ecFieldCreate1dArray
end module m_ec_field
