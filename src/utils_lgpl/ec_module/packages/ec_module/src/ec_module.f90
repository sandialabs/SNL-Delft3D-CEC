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

!  $Id: ec_module.f90 5609 2015-11-25 17:21:04Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/ec_module/packages/ec_module/src/ec_module.f90 $

!> This module contains the interfaces of the EC-module.
!! It is the main access point to the EC-module.
!! All interaction with an instance of the EC-module should take place through these interfaces.
!! An instance of the EC-module is a pointer of type tEcInstance.
!!
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.bos@deltares.nl
module m_ec_module
   use m_ec_typedefs
   use m_ec_connection
   use m_ec_converter
   use m_ec_elementSet
   use m_ec_field
   use m_ec_item
   use m_ec_quantity
   use m_ec_filereader
   use m_ec_bcreader
   use m_ec_bccollect
   use m_ec_instance
   use m_ec_message
   use m_ec_support
   use m_ec_provider
   
   implicit none
     
   !> The EC-module contains a single public pointer to a tEcInstance that is visible/usable by all kernels.
   !! Kernels can have their own private pointers to tEcInstances, which are only visible/usable by them.
   !! All these pointers should dynamically allocate memory for their tEcInstances, making the tEcInstances unnamed data objects.
   !! This is achieved by calling the interface procedure ecInstanceCreate(type(tEcInstance), pointer).
   !! All these pointers should therefor be deallocated, not directly nullified.
   !! This is achieved by calling the interface procedure ecInstanceFree(type(tEcInstance), pointer).
   type(tEcInstance), pointer, save :: public_EC_ptr => null() !< No kernel should ever call ecInstanceFree(public_EC_ptr).
   
   ! ===== constructors =====
   
   !> Construct and initialize a new EC-module instance.
   interface ecCreateInstance
      module procedure ecInstanceCreate
   end interface ecCreateInstance
   
   !> Construct and initialize a new FileReader, including its full range of source Items.
   interface ecCreateFileReader
      module procedure ecInstanceCreateFileReader
   end interface ecCreateFileReader
   
   !> Construct and initialize a new BCBlock-instance
   interface ecCreateBCBlock
      module procedure ecInstanceCreateBCBlock
   end interface ecCreateBCBlock

   !> Construct and initialize a new NetCDF-instance
   interface ecCreateNetCDF
      module procedure ecInstanceCreateNetCDF
   end interface ecCreateNetCDF
   
   !> Construct and initialze a new Quantity, returning a unique integer identifier.
   interface ecCreateQuantity
      module procedure ecInstanceCreateQuantity
   end interface ecCreateQuantity
   
   !> Construct and initialze a new ElementSet, returning a unique integer identifier.
   interface ecCreateElementSet
      module procedure ecInstanceCreateElementSet
   end interface ecCreateElementSet
   
   !> Construct and initialze a new Field, returning a unique integer identifier.
   interface ecCreateField
      module procedure ecInstanceCreateField
   end interface ecCreateField
   
   !> Construct and initialze a new Item, returning a unique integer identifier.
   interface ecCreateItem
      module procedure ecInstanceCreateItem
   end interface ecCreateItem
   
   !> Construct and initialze a new Converter, returning a unique integer identifier.
   interface ecCreateConverter
      module procedure ecInstanceCreateConverter
   end interface ecCreateConverter
   
   !> Construct and initialze a new Connection, returning a unique integer identifier.
   interface ecCreateConnection
      module procedure ecInstanceCreateConnection
   end interface ecCreateConnection
   
   ! ===== destructor =====
   
   !> Recursively free and destruct the entire data tree, which is an existing EC-module instance.
   interface ecFreeInstance
      module procedure ecInstanceFree
   end interface ecFreeInstance
   
   ! ===== getters =====
   
   !> 
   interface ecGetValues
      module procedure ecItemGetValues
   end interface ecGetValues
      
   interface ecFindItemInFileReader
      module procedure ecFileReaderFindItem
   end interface ecFindItemInFileReader
   
   interface ecGetFileReaderNumberOfItems
      module procedure ecFileReaderGetNumberOfItems
   end interface ecGetFileReaderNumberOfItems
   
   interface ecFindXthItemInFileReader
      module procedure ecFileReaderGetItem
   end interface ecFindXthItemInFileReader
   
   interface ecGetMessage
      module procedure getECMessage
   end interface ecGetMessage
   
   ! ===== setters =====
   ! No convenience methods defined yet.
   
   ! Quantity
   
   interface ecSetQuantity
      module procedure ecQuantitySet
   end interface ecSetQuantity
   
   interface ecSetQuantityName
      module procedure ecQuantitySetName
   end interface ecSetQuantityName
   
   interface ecSetQuantityUnits
      module procedure ecQuantitySetUnits
   end interface ecSetQuantityUnits

   interface ecSetQuantityVectorMax
      module procedure ecQuantitySetVectorMax
   end interface ecSetQuantityVectorMax

   
   ! ElementSet
   
   interface ecSetElementSetName
      module procedure ecElementSetSetName
   end interface ecSetElementSetName

   interface ecSetElementSetType
      module procedure ecElementSetSetType
   end interface ecSetElementSetType
   
   interface ecSetElementSetXArray
      module procedure ecElementSetSetXArray
   end interface ecSetElementSetXArray
   
   interface ecSetElementSetYArray
      module procedure ecElementSetSetYArray
   end interface ecSetElementSetYArray
   
   interface ecSetElementSetZArray
      module procedure ecElementSetSetZArray
   end interface ecSetElementSetZArray

   interface ecSetElementSetItype3D
      module procedure ecElementSetSetItype3D
   end interface ecSetElementSetItype3D
   
   interface ecSetElementSetX0Dx
      module procedure ecElementSetSetX0Dx
   end interface ecSetElementSetX0Dx
   
   interface ecSetElementSetY0Dy
      module procedure ecElementSetSetY0Dy
   end interface ecSetElementSetY0Dy
   
   interface ecSetElementSetLatitudeArray
      module procedure ecElementSetSetLatitudeArray
   end interface ecSetElementSetLatitudeArray
   
   interface ecSetElementSetLongitudeArray
      module procedure ecElementSetSetLongitudeArray
   end interface ecSetElementSetLongitudeArray
   
   interface ecSetElementSetDirectionArray
      module procedure ecElementSetSetDirectionArray
   end interface ecSetElementSetDirectionArray
   
   interface ecSetElementSetLat0Dlat
      module procedure ecElementSetSetLat0Dlat
   end interface ecSetElementSetLat0Dlat
   
   interface ecSetElementSetLon0Dlon
      module procedure ecElementSetSetLon0Dlon
   end interface ecSetElementSetLon0Dlon
   
   interface ecSetElementSetMaskArray
      ! Error: Ambiguous interfaces 'ecelementsetsetmaskarray' and 'ecelementsetsetmaskpointer' in 
      ! generic interface 'ecsetelementsetmaskarray' at (1)
      
      ! module procedure ecElementSetSetMaskPointer
      module procedure ecElementSetSetMaskArray
   end interface ecSetElementSetMaskArray
   
   interface ecSetElementSetNumberOfCoordinates
      module procedure ecElementSetSetNumberOfCoordinates
   end interface ecSetElementSetNumberOfCoordinates
   
   interface ecSetElementSetSouthPoleLatitude
      module procedure ecElementSetSetSouthPoleLatitude
   end interface ecSetElementSetSouthPoleLatitude
   
   interface ecSetElementSetSouthPoleLongitude
      module procedure ecElementSetSetSouthPoleLongitude
   end interface ecSetElementSetSouthPoleLongitude
   
   interface ecSetElementSetLocations
      module procedure ecElementSetSetLocations
   end interface ecSetElementSetLocations
   
   interface ecSetElementSetRadius
      module procedure ecElementSetSetRadius
   end interface ecSetElementSetRadius
   
   interface ecSetElementSetRowsCols
      module procedure ecElementSetSetRowsCols
   end interface ecSetElementSetRowsCols
   
   interface ecSetElementSetXyen
      module procedure ecElementSetSetXyen
   end interface ecSetElementSetXyen
   
   ! Field
   
   interface ecSetFieldMissingValue
      module procedure ecFieldSetMissingValue
   end interface ecSetFieldMissingValue
   
   interface ecSetField1dArray
      module procedure ecFieldSet1dArray
      module procedure ecFieldCreate1dArray
   end interface ecSetField1dArray
   
   ! Item

   interface ecSetItemProperty
      module procedure ecItemSetProperty
   end interface ecSetItemProperty
   
   interface ecSetItemRole
      module procedure ecItemSetRole
   end interface ecSetItemRole
   
   interface ecSetItemType
      module procedure ecItemSetType
   end interface ecSetItemType
   
   interface ecSetItemQuantity
      module procedure ecItemSetQuantity
   end interface ecSetItemQuantity
   
   interface ecSetItemElementSet
      module procedure ecItemSetElementSet
   end interface ecSetItemElementSet
   
   interface ecSetItemSourceT0Field
      module procedure ecItemSetSourceT0Field
   end interface ecSetItemSourceT0Field
   
   interface ecSetItemSourceT1Field
      module procedure ecItemSetSourceT1Field
   end interface ecSetItemSourceT1Field
   
   interface ecSetItemTargetField
      module procedure ecItemSetTargetField
   end interface ecSetItemTargetField
   
   interface ecAddItemConnection
      module procedure ecItemAddConnection
   end interface ecAddItemConnection
   
   ! Converter
   
   interface ecSetConverterType
      module procedure ecConverterSetType
   end interface ecSetConverterType
   
   interface ecSetConverterOperand
      module procedure ecConverterSetOperand
   end interface ecSetConverterOperand

   interface ecSetConverterMask
      module procedure ecConverterSetMask
   end interface ecSetConverterMask

   interface ecSetConverterInterpolation
      module procedure ecConverterSetInterpolation
   end interface ecSetConverterInterpolation
   
   interface ecSetConverterElement
      module procedure ecConverterSetElement
   end interface ecSetConverterElement
   
   ! Connection
   
   interface ecSetConnectionConverter
      module procedure ecConnectionSetConverter
   end interface ecSetConnectionConverter
   
   interface ecAddConnectionSourceItem
      module procedure ecConnectionAddSourceItem
   end interface ecAddConnectionSourceItem
   
   interface ecAddConnectionTargetItem
      module procedure ecConnectionAddTargetItem
   end interface ecAddConnectionTargetItem
   
   ! FileReader
   interface ecAddItemToFileReader
      module procedure ecFileReaderAddItem
   end interface ecAddItemToFileReader

   ! Provider
   interface ecCreateTimeInterpolatedItem
      module procedure ecProviderCreateTimeInterpolatedItem
   end interface ecCreateTimeInterpolatedItem

   ! Support
   interface ecFindItemByQuantityLocation
      module procedure ecSupportFindItemByQuantityLocation
   end interface ecFindItemByQuantityLocation
end module m_ec_module
