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

!  $Id: ec_typedefs.f90 5640 2015-12-10 09:24:34Z hummel $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/ec_module/packages/ec_module/src/ec_typedefs.f90 $

!> This module contains all the user defined datatypes.
!! @author edwin.spee@deltares.nl
!! @author edwin.bos@deltares.nl
module m_ec_typedefs
   use precision
   use m_ec_parameters
   
   implicit none
   
   !===========================================================================
   ! Top level data types.
   !===========================================================================
   
   !> Datatype representing an instance of the EC-module.
   !! It contains arrays which administrate all created objects.
   !! A user defined datatype together with its corresponding methods file (*.f90) is considered a class.
   type tEcInstance
      type(tEcConnectionPtr),   dimension(:), pointer :: ecConnectionsPtr  => null()
      integer                                         :: nConnections                !< Number of tEcConnectionPtrs <= size(ecConnectionsPtr)
      type(tEcConverterPtr),    dimension(:), pointer :: ecConvertersPtr   => null()
      integer                                         :: nConverters                 !< Number of tEcConverterPtrs <= size(ecConvertersPtr)
      type(tEcElementSetPtr),   dimension(:), pointer :: ecElementSetsPtr  => null()
      integer                                         :: nElementSets                !< Number of tEcElementSetPtrs <= size(ecElementSetsPtr)
      type(tEcFieldPtr),        dimension(:), pointer :: ecFieldsPtr       => null()
      integer                                         :: nFields                     !< Number of tEcFieldPtrs <= size(ecFieldsPtr)
      type(tEcFileReaderPtr),   dimension(:), pointer :: ecFileReadersPtr  => null()
      integer                                         :: nFileReaders                !< Number of tEcFileReaderPtrs <= size(ecFileReadersPtr)
      type(tEcBCBlockPtr),      dimension(:), pointer :: ecBCBlocksPtr     => null()
      integer                                         :: nBCBlocks                   !< Number of tEcBCBlocksPtrs <= size(ecBCBlocksPtr)
      type(tEcNetCDFPtr),       dimension(:), pointer :: ecNetCDFsPtr      => null()
      integer                                         :: nNetCDFs                    !< Number of tEcNetCDFPtrs <= size(ecNetCDFPtr)
      type(tEcItemPtr),         dimension(:), pointer :: ecItemsPtr        => null()
      integer                                         :: nItems                      !< Number of tEcItemPtrs <= size(ecItemsPtr)
      type(tEcQuantityPtr),     dimension(:), pointer :: ecQuantitiesPtr   => null()
      type(tEcStringbufferPtr), dimension(:), pointer :: ecStringbufferPtr => null()
      integer                                         :: nQuantities                 !< Number of tEcQuantityPtrs <= size(ecQuantitiesPtr)
      integer                                         :: idCounter                   !< helper variable for assigning unique ids to the Instance's stored objects
      integer                                         :: coordsystem = -1            !< Coordinate system of 
   end type tEcInstance
   
   !===========================================================================
   
    type tEcStringbuffer
       integer                                     ::  nlines = 0           !< Number of lines currently buffered 
       integer                                     ::  nblocks = 0          !< Number of bc blocks currently buffered 
       character(len=maxRecordlen), allocatable    ::  lines(:)             !< Array of lines currently buffered 
       integer, allocatable                        ::  numbers(:)           !< Line numbers 
       character(len=maxFileNameLen), allocatable  ::  bcnames(:)           !< Names of stored blocks 
       integer, allocatable                        ::  from_line(:)         !< From where thru where is this block stored in the stringbuffer ? 
       integer, allocatable                        ::  thru_line(:)        
    end type tEcStringbuffer 

   type tEcStringbufferPtr
      type(tEcStringbuffer), pointer :: ptr => null()
   end type tEcStringbufferPtr
   
   !===========================================================================

    ! TODO : fill in default invalid values for some of the fields to detect reading failure or missing header fields  
    ! A bc-object has a SINGLE quantity-object, which corresponds to a SINGLE vertical level, possibly associated with MULTIPLE columns in the data  
    type :: tEcBCQuantity
        integer                 ::  qtype       !< Type number    
        logical, allocatable    ::  jacolumn(:) !< If(jacolumn(i) then i-th column contains the requested quantity (possibly more than one column)
        integer, allocatable    ::  col2elm(:)  !< Map column to vector element number to column in a t3D-block
        character(len=25)       ::  name        !< User-specified name 
        integer                 ::  column      !< Unique column number in the data for this quantity 
        character(len=25)       ::  unit        !< unit specification 
        real(hp)                ::  offset = 0.d0  !< to be added to all data for this quantity
        real(hp)                ::  factor = 1.d0  !< to be multiplied with all data for this quantity
!       integer, allocatable    ::  vertndx(:)  !< vertical position nr (indices into the global vertical position array)
        integer                 ::  vertndx     !< vertical position nr (indices into the global vertical position array)
        integer                 ::  astro_component_column = -1  !< number of the column, containing astronomic components
        integer                 ::  astro_amplitude_column = -1  !< number of the column, containing astronomic amplitudes
        integer                 ::  astro_phase_column     = -1  !< number of the column, containing astronomic phases
        integer                 ::  qh_waterlevel_column   = -1  !< number of the column, containing qh-type waterlevel (h)
        integer                 ::  qh_discharge_column    = -1  !< number of the column, containing qh-type discharge (q)
        integer                 ::  vectormax              = 1   !< number of vector elements, default scalar 

        character(len=8), allocatable  :: astro_component(:)     !< original component label read 
        real(hp), allocatable          :: astro_amplitude(:)     !< original amplitude read 
        real(hp), allocatable          :: astro_phase(:)         !< original phase read  
    end type tEcBCQuantity

   
   !===========================================================================

   type :: tEcBCBlock
        integer                                    ::  id              !< unique BCBlock number, set by ecInstanceCreateBCBlock
        integer                                    ::  numcols         !< number of data columns 
        character(len=maxFileNameLen), allocatable ::  columns(:)      !< temporary substrings from columns, used by the line reader 
        integer                                    ::  from_line = -1  !< Data for this block start linenumber in temporary string buffer
        integer                                    ::  thru_line = -1  !< Data for this block final linenumber in temporary string buffer 
        integer                                    ::  readpos         !< Current reading position in the stringbuffer
        integer                                    ::  func
        integer                                    ::  timecolumn      !< Number of the column holding the time strings, compul
        character(len=50)                          ::  timeunit        !< netcdf-convention time unit definition 
        integer                                    ::  timeint         !< Type of time interpolation 
        integer                                    ::  vptyp           !< Type of specification of vertical position
        real(hp), allocatable                      ::  vp(:)           !< vertical positions  
        integer                                    ::  numlay = 1      !< number of vertical layers 
        integer                                    ::  zInterpolationType  !< Type of vertical interpolation 
        real(hp)                                   ::  missing         !< Missing value 
        character(len=maxFileNameLen)              ::  bcname          !< Name (identifier) for this BC block (assumed to be uniq)
        character(len=maxFileNameLen)              ::  qname           !< Quantity name with which all found quantities must identify 
        character(len=maxFileNameLen)              ::  fname           !< Filename the data originates from 
        integer(kind=8)                            ::  fhandle= -1     !< (C) filehandle to open file 
        integer                                    ::  ftype  = -1     !< ASCII, NetCDF, ....
        type (tEcBCQuantity), pointer              ::  quantity        !< Quantity object 
        type (tEcBCQuantity), allocatable          ::  quantities(:)   !< Array of quantity objects for each quantity with the same name  
        type (tEcStringbuffer), pointer            ::  strbufptr       !< pointer to a stringbuffer (to be set upon construction) 
        type (tEcNetCDF), pointer                  ::  ncptr => null() !< pointer to a NetCDF instance, responsible for a connected NetCDF file 
        integer                                    ::  ncvarndx = -1   !< varid in the associated netcdf for the requested quantity 
        integer                                    ::  nclocndx = -1   !< index in the timeseries_id dimension for the requested location 
        integer                                    ::  nctimndx =  1   !< record number to be read 
   end type tEcBCBlock

   type tEcBCBlockPtr
      type(tEcBCBlock), pointer :: ptr => null()
   end type tEcBCBlockPtr

   !===========================================================================

   type :: tEcNetCDF
        integer                                      ::  id              !< unique NCBlock number, set by ecInstanceCreateNCBlock
        integer                                      ::  ncid            !< unique NetCDF ncid 
        character(len=maxFileNameLen)                ::  ncname          !< netCDF filename
        integer, allocatable, dimension(:)           ::  dimlen          !< lengths of dimensions 
        character(len=maxFileNameLen), allocatable, dimension(:)       ::  standard_names          !< list of standard names
        integer                                      ::  nDims = 0       !< Number of dimensions 
        integer                                      ::  nTims = 0       !< Number of timeseries 
        integer                                      ::  nVars = 0       !< Number of variables 
        character(len=maxNameLen), allocatable, dimension(:)  ::  tsid   !< list of timeseries identifiers
        integer                                      ::  tsidid = -1     !< var_id for the timeseries ID variable 
        integer                                      ::  tsiddimid = -1  !< dim_id for the timeseries IDs coordinate
        integer                                      ::  timeid = -1     !< var_id for the designated time variable 
        integer                                      ::  timedimid = -1  !< dim_id for the time coordinate 
        integer                                      ::  layerid = -1    !< var_id for the verical layer variable 
        integer                                      ::  layerdimid = -1 !< dim_id for the vertical coordinate
        character(len=50)                            ::  timeunit        !< netcdf-convention time unit definition 
   end type 

   type tEcNetCDFPtr
      type(tEcNetCDF), pointer :: ptr => null()
   end type tEcNetCDFPtr

   !===========================================================================

   !> Datatype which links Items and registers the converter which transforms the data of the source Items to the format desired by the target Items.
   type tEcConnection
      integer                                   :: id                       !< unique Connection number, set by ecInstanceCreateConnection
      type(tEcConverter),               pointer :: converterPtr   => null() !< converter between source and target items
      type(tEcItemPtr),   dimension(:), pointer :: sourceItemsPtr => null() !< source items, stored in tEcInstance%ecItemsPtr
      integer                                   :: nSourceItems             !< Number of source Items <= size(sourceItemsPtr)
      type(tEcItemPtr),   dimension(:), pointer :: targetItemsPtr => null() !< target items, stored in tEcInstance%ecItemsPtr
      integer                                   :: nTargetItems             !< Number of target Items <= size(targetItemsPtr)
   end type tEcConnection
   
   type tEcConnectionPtr
      type(tEcConnection), pointer :: ptr => null()
   end type tEcConnectionPtr

   !===========================================================================

   !> 
   type tEcMask
      integer                       :: mmin                  !< Represents the leftmost horizontal index 
      integer                       :: nmin                  !< Represents the bottommost vertical index 
      integer                       :: mmax                  !< Represents the rightmost horizontal index 
      integer                       :: nmax                  !< Represents the topmost vertical index 
      integer                       :: mrange                !< Equals mmax-mmin+1 (redundant, but convenient)
      integer                       :: nrange                !< Equals nmax-nmin+1 
      integer, allocatable          :: msk(:)                !< Array of mask values (TODO: should this be 2d?) 
   end type tEcMask
   
   !> 
   type tEcConverter
      integer                       :: id                    !< unique Converter number, set by ecInstanceCreateConverter
      integer                       :: ofType                !< Converter type, using the convType enumeration
      integer                       :: operandType           !< operand type, using operand enumeration; What to do to the target Item Field.
      integer                       :: interpolationType     !< interpolation type, using the interpolateType enumeration
      integer                       :: targetIndex           !< Write to the target Item's Field's array element number targetIndex (vectormax (here called n_data) should already be accounted for, that offset is *not* recomputed in the converter).
      type(tEcIndexWeight), pointer :: indexWeight => null() !< 
      type(tEcMask)                 :: srcmask               !< Array with mask info on selection of gridpoints
   end type tEcConverter
   
   type tEcConverterPtr
      type(tEcConverter), pointer :: ptr => null()
   end type tEcConverterPtr

   !===========================================================================
   
   !> Datatype representing the geometry of an item's data.
   type tEcElementSet
      integer                             :: id             !< unique ElementSet number, set by ecInstanceCreateElementSet
      integer                             :: ofType         !< contained geometry type, using the elmSetType enumeration
      character(len=maxNameLen)           :: name           !< Optional name for this elementset = locationname
      ! Data variables for ElementSet derived types. Usage depends on tEcElementSet%ofType.
      real(hp), dimension(:), pointer     :: x    => null() !< array of x-coordinates
      real(hp), dimension(:), pointer     :: y    => null() !< array of y-coordinates
      real(hp), dimension(:), pointer     :: z    => null() !< array of z/sigma-coordinates
      real(hp), dimension(:), pointer     :: lat  => null() !< array of latitude coordinates
      real(hp), dimension(:), pointer     :: lon  => null() !< array of longitude coordinates
      real(hp), dimension(:), pointer     :: dir  => null() !< array of directions (angles) related to a poleshift coordinate transformation 
      integer,  dimension(:), pointer     :: mask => null() !< points to a 1-dim array field, stored in maskArray OR in a kernel
      integer,  dimension(:), allocatable :: maskArray      !< value = 0: invalid point; value /= 0: valid point
      integer                             :: nCoordinates   !< number of coordinate pairs
      integer                             :: n_cols         !< number of columns in a data field
      integer                             :: n_rows         !< number of rows in a data field
      integer                             :: itype3D        !< sigma (0) or z (1)
      real(hp)                            :: x0             !< seed coordinate for equidistant x-coordinates
      real(hp)                            :: y0             !< seed coordinate for equidistant x-coordinates
      real(hp)                            :: dx             !< step size in x for equidistant x-coordinates
      real(hp)                            :: dy             !< step size in y for equidistant y-coordinates
      real(hp)                            :: lat0           !< seed coordinate for equidistant latitude coordinates
      real(hp)                            :: lon0           !< seed coordinate for equidistant longitude coordinates
      real(hp)                            :: dlat           !< step size in x for equidistant latitude coordinates
      real(hp)                            :: dlon           !< step size in y for equidistant longitude coordinates
      real(hp)                            :: latsp          !< latitude of south pole (rotated spherical coordinates)
      real(hp)                            :: lonsp          !< longitude of south pole (rotated spherical coordinates)
      real(hp)                            :: radius         !< radius of a spiderweb
      character(len=maxNameLen)           :: radius_unit    !< unit of the radius of a spiderweb
      character(len=maxNameLen), dimension(:),   pointer :: ids  => null() !< string array with locations
      real(hp),                  dimension(:,:), pointer :: xyen => null() !< 
   end type tEcElementSet

   type tEcElementSetPtr
      type(tEcElementSet), pointer :: ptr => null()
   end type tEcElementSetPtr

   !===========================================================================
   
   !> 
   type tEcField
      integer                                 :: id                 !< unique Field number, set by ecInstanceCreateField
      real(hp)                                :: timesteps          !< Numer of seconds since tEcTimeFrame%k_refdate.
      real(hp)                                :: missingValue       !< value to use for missing data in the data arrays
      real(hp), dimension(:),     pointer     :: arr1dPtr => null() !< points to a 1-dim array field, stored in arr1d OR in a kernel
      real(hp), dimension(:),     allocatable :: arr1d              !< 1-dim array field
      real(hp)                                :: x_spw_eye          !< x-coordinate of spiderweb eye
      real(hp)                                :: y_spw_eye          !< y-coordinate of spiderweb eye
      character(len=8), allocatable           :: astro_components(:)!< astronomical components for astro bc's
   end type tEcField
   
   type tEcFieldPtr
      type(tEcField), pointer :: ptr => null()
   end type tEcFieldPtr
   
   !===========================================================================
   
   !> 
   type tEcFileReader
      integer                                             :: id                      !< unique FileReader number, set by ecInstanceCreateFileReader
      integer                                             :: ofType                  !< type of FileReader, see provFile enumeration
      character(len=maxFileNameLen)                       :: fileName                !< relative path of data file
      integer                                             :: fileHandle              !< IO unit number of opened data file
      type(tEcItemPtr),             dimension(:), pointer :: items => null()         !< items to be updated by this fileReader, stored in tEcInstance%ecItemsPtr
      integer                                             :: nItems                  !< Number of items <= size(items)
      type(tEcTimeFrame),                         pointer :: tframe => null()        !< TimeFrame at which data is available
      real(hp)                                            :: lastReadTime
      type(tEcBCBlock),                           pointer :: bc => null()            !< BC-fileheader information
      integer                                             :: vectormax = 1           !< number of vector elements (from the demand side) 
                                                                                     !  This field is used to pass the dimensionality from the 
                                                                                     !            highest to the lowest level upon creation
      logical                                             :: end_of_data             !< End of data reached?
      character(len=100), dimension(:), allocatable :: standard_names                ! Standard names by varid in a netcdf-file 
      character(len=100), dimension(:), allocatable :: variable_names                ! Variable names by varid in a netcdf file 
   end type tEcFileReader

   type tEcFileReaderPtr
      type(tEcFileReader), pointer :: ptr => null()
   end type tEcFileReaderPtr
   
   !===========================================================================
   
   !> 
   type tEcItem
      integer                                       :: id                           !< unique Item number, set by ecInstanceCreateItem
      integer                                       :: role                         !< source or target Item, see itemType enumeration
      integer                                       :: accessType                   !< file reader, file writer, data provider
      type(tEcQuantity),                    pointer :: quantityPtr        => null() !< Quantity, stored in tEcInstance%ecQuantitiesPtr
      type(tEcElementSet),                  pointer :: elementSetPtr      => null() !< ElementSet, stored in tEcInstance%ecElementSetsPtr
      type(tEcField),                       pointer :: sourceT0FieldPtr   => null() !< Field containing source data of second to last read data block.
      type(tEcField),                       pointer :: sourceT1FieldPtr   => null() !< Field containing source data of last read data block.
      type(tEcField),                       pointer :: targetFieldPtr     => null() !< Field containing target data at current time.
      type(tEcConnectionPtr), dimension(:), pointer :: connectionsPtr     => null() !< Connections in which this Item is a target Item
      integer                                       :: nConnections                 !< Number of Connections <= size(connectionsPtr)
   end type tEcItem
   
   type tEcItemPtr
      type(tEcItem), pointer :: ptr => null()
   end type tEcItemPtr
   
   !===========================================================================
   
   !> Datatype containing metadata which describe an ecItem's data.
   type tEcQuantity
      integer                   :: id                       !< unique Quantity number, set by ecInstanceCreateQuantity
      character(len=maxNameLen) :: name                     !< description of the quantity
      character(len=maxNameLen) :: units                    !< physical units of the quantity
      integer                   :: vectorMax = 1            !< number of dimensions (vector data) or 1 in case of scalar
      integer                   :: zInterpolationType       !< Vertical interpolation type ! TODO: Add initialization in the constructor. (4748)
   end type tEcQuantity
   
   type tEcQuantityPtr
      type(tEcQuantity), pointer :: ptr => null()
   end type tEcQuantityPtr
   
   !===========================================================================
   ! Support data types.
   !===========================================================================
   
   !> 
   type tEcIndexWeight
      ! FM: tdataprovider indxn
      integer , dimension(:,:),   pointer :: indices       => null() !< indices: ([row,column]:nCoordinates)
      ! FM: tdataprovider wfn
      real(hp), dimension(:,:),   pointer :: weightFactors => null() !< weightfactors: ([1,2,3,4]:nCoordinates)
   end type tEcIndexWeight
   
   ! ==========================================================================
   
   !> 
   type tEcTimeFrame
      real(hp)                            :: k_refdate        !< Kernel's reference date formatted as Modified Julian Date
      integer                             :: k_timestep_unit  !< Time unit of a timestep in the kernel.
      real(hp)                            :: k_timezone       !< Timezone of the kernel.
      real(hp)                            :: ec_refdate       !< EC file's reference date formatted as Modified Julian Date
      integer                             :: ec_timestep_unit !< Time unit of a timestep in the input data file.
      real(hp)                            :: ec_timezone      !< Timezone of the EC file
      real(hp)                            :: nr_timesteps     !< Total number of available timesteps [ec_timestep_unit].
      real(hp), dimension(:), allocatable :: times            !< The timesteps [ec_timestep_unit] at which data is available.
      real(hp)                            :: dtnodal          !< Nodal factors update interval
   end type tEcTimeFrame
end module m_ec_typedefs
