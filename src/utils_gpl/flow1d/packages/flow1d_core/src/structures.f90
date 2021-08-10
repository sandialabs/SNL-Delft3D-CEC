module m_1d_structures
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
!  $Id: structures.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/structures.f90 $
!-------------------------------------------------------------------------------

   use MessageHandling
   use m_GlobalParameters
   use m_alloc
   use m_branch
   use m_tables
   use m_CrossSections
   use m_Weir
   use m_Culvert
   use m_pump
   use m_Orifice
   use m_General_Structure
   use m_Universal_Weir
   use m_Bridge
   use m_ExtraResistance
   use m_hash_search
   use m_Dambreak
   use iso_c_utils

   implicit none

   private

   public realloc
   public dealloc

   public addStructure
   public getStructureCount
   public setRestartDataForStructures
   public setValue
   public getValue
   public reIndexCrossSections
   public getStrucType_from_string

   public getTableValue
   public getCrossSection
   public getStructureById
   public GetStrucType_from_int
   public get_crest_level
   public get_crest_level_c_loc
   public get_gate_lower_edge_level_c_loc
   public get_valve_opening_height_c_loc
   public get_gate_opening_width_c_loc
   public get_gate_door_height_c_loc
   public get_width
   public get_watershed_threshold
   public get_gle
   public get_opening_height
   public get_valve_opening
   public get_culvert_state
   public fill_hashtable
   public set_crest_level
   public set_crest_width
   public set_gle
   public set_opening_height
   public set_valve_opening
   public incStructureCount
   public GetPumpCapacity
   public get_pump_capacity_c_loc
   public GetPumpStage
   public GetPumpReductionFactor
   public getPumpSsLevel
   public getPumpDsLevel
   public initialize_structure_links
   public set_fu_ru_structure
   public check_for_changes_on_structures
   public initialize_structures_actual_params
   public get_discharge_under_compound_struc
   public set_u0isu1_structures
   public set_u1q1_structure
   public reset_fu_ru_for_structure_link

   public printData

   interface fill_hashtable
      module procedure fill_hashtable_sts
   end interface 
   
   interface AddStructure
      module procedure AddStructure_short
      module procedure AddStructureByCalcPoints
   end interface

   interface getTableValue
      module procedure getTableValueStruc
   end interface

   interface SetValue
      module procedure setValueStruc
   end interface

   interface GetValue
      module procedure getValueStruc
   end interface

   interface realloc
      module procedure reallocstructure
      module procedure reallocForcingList
   end interface

   interface dealloc
      module procedure deallocstructure
      module procedure deallocForcingList
   end interface dealloc

   ! TODO: the next declarations are duplicates of OMI_CF_DATA.
   integer, public, parameter :: CFiCrestLevel         = 18
   integer, public, parameter :: CFiCrestWidth         = 19
   integer, public, parameter :: CFiGateLowerEdgeLevel = 20
   integer, public, parameter :: CFiGateOpeningHeight  = 21
   integer, public, parameter :: CFiValveOpening       = 22
   integer, public, parameter :: CFiSetpoint           = 29
   integer, public, parameter :: CFiHighestParameter   = 31
    integer,         parameter :: MaxWarnings = 50
   integer                    :: numberOfWarnings = 0

    !---------------------------------------------------------
   type, public :: t_structure
      character(IdLen)                 :: id             !< Id of the structure
      character(IdLen)                 :: name           !< (long) name of the structure
      integer                          :: type           !< integer structure type
      integer                          :: ibran          !< branch index
      double precision                 :: chainage       !< Chainage
      integer                          :: numCoordinates !< number of coordinates in the location polygon
      double precision, pointer, dimension(:)   :: xCoordinates   !< x-coordinates of the location polygon
      double precision, pointer, dimension(:)   :: yCoordinates   !< y-coordinates of the location polygon
      
      integer                          :: numlinks       !< number of links in structure
      integer, pointer, dimension(:)   :: linknumbers    !< link numbers of structure (length = numlinks)
      double precision, pointer, dimension(:)   :: fu    !< fu coefficient for momentum equation
      double precision, pointer, dimension(:)   :: ru    !< ru coefficient for momentum equation
      double precision, pointer, dimension(:)   :: au    !< flow area
      double precision, pointer, dimension(:)   :: u0    !< flow velocity at previous time step
      double precision, pointer, dimension(:)   :: u1    !< flow velocity at current time step
    
      integer                          :: compound
      type(t_weir), pointer            :: weir => null()
      type(t_orifice), pointer         :: orifice => null()
      type(t_pump), pointer            :: pump => null()
      type(t_culvert),pointer          :: culvert => null()
      type(t_uni_weir),pointer         :: uniweir => null()
      type(t_bridge),pointer           :: bridge => null()
      type(t_GeneralStructure),pointer :: generalst => null()
      type(t_ExtraResistance),pointer  :: extrares => null()
      type(t_dambreak),pointer         :: dambreak => null()
   end type

   type, public :: t_structureSet
      integer                                               :: Size               = 0
      integer                                               :: growsBy            = 2000
      integer                                               :: Count              = 0 !< Current number of structures in this set.
      integer, dimension(ST_MAX_TYPE)                       :: countByType        = 0
      integer                                               :: compoundCount      = 0
      logical                                               :: hasExtraResistance = .false.
      type(t_structure), pointer, dimension(:)              :: struct
      !> Contains information on
      real, dimension(:,:), allocatable                     :: restartData
      type(t_hashlist)                                      :: hashlist_weir
      type(t_hashlist)                                      :: hashlist_culvert
      type(t_hashlist)                                      :: hashlist_bridge
      type(t_hashlist)                                      :: hashlist_pump
      type(t_hashlist)                                      :: hashlist_structure
      integer                                               :: currentFileVersion       !< Lowest file version used in the input 
      integer                                               :: numWeirs                 !< Total number of weirs in this structure set. See indices array below.
      integer                                               :: numCulverts              !< Total number of culverts in this structure set. See indices array below.
      integer                                               :: numPumps                 !< Total number of pumps in this structure set. See indices array below.
      integer                                               :: numBridges               !< Total number of bridges in this structure set. See indices array below.
      integer                                               :: numOrifices              !< Total number of orifices in this structure set. See indices array below.
      integer                                               :: numGates                 !< Total number of gates in this structure set. See indices array below.
      integer                                               :: numGeneralStructures     !< Total number of general structures in this structure set. See indices array below.
      integer                                               :: numUniWeirs              !< Total number of universal weirs in this structure set. See indices array below.
      integer, pointer, dimension(:)                        :: weirIndices              !< (numWeirs) indices of the weirs in the overall struct(:) array. Note: some may actually be of type ST_GENERAL_ST.
      integer, pointer, dimension(:)                        :: culvertIndices           !< (numCulverts) indices of the culverts in the overall struct(:) array.
      integer, pointer, dimension(:)                        :: pumpIndices              !< (numPumps) indices of the pumps in the overall struct(:) array.
      integer, pointer, dimension(:)                        :: bridgeIndices            !< (numBridges) indices of the bridges in the overall struct(:) array.
      integer, pointer, dimension(:)                        :: orificeIndices           !< (numOrifices) indices of the orifices in the overall struct(:) array. Note: some may actually be of type ST_GENERAL_ST.
      integer, pointer, dimension(:)                        :: gateIndices              !< (numGates) indices of the gates in the overall struct(:) array. Note: some may actually be of type ST_GENERAL_ST.
      integer, pointer, dimension(:)                        :: generalStructureIndices  !< (numGeneralStructures) indices of the general structures in the overall struct(:) array.
      integer, pointer, dimension(:)                        :: uniWeirIndices           !< (numUniWeirs) indices of the universal weirs in the overall struct(:) array.
   end type t_structureSet

   !> Data type to store user input for structure forcings, to be processed later by a kernel.
   !! For example, a pump's capacity may be prescribed by a time series in a .bc file.
   !! The flow1d structure reader only reads all user-supplied input, and later it is up to
   !! the calling kernel to initialize that forcing provider.
   type, public :: t_forcing
      character(IdLen)                 :: st_id      !< The structure's character Id.
      integer                          :: st_type    !< Structure type (e.g., ST_PUMP).
      character(IdLen)                 :: param_name !< Name of the structure's parameter that this forcing data is for.
      double precision, pointer        :: targetptr  !< Pointer to scalar variable in which the provided
                                                     !< parameter value(s) can later be stored.
                                                     !< For example => pump%capacity.
      character(Charln)                :: filename   !< Name of file that contains the forcing data (e.g., a time series file).
   end type

   !> An ordered list of structure forcing items.
   type, public :: t_forcingList
      integer                                :: Size     = 0  !< Current maximum size of the forcing list.
      integer                                :: growsBy  = 20 !< Increment upon each realloc call.
      integer                                :: Count    = 0  !< Current actual number of items in the forcing list.
      type(t_forcing), pointer, dimension(:) :: forcing       !< Actual forcing list.
   end type

   contains
   
   integer function AddStructure_short(sts, leftcalc, rightcalc, linknumber, icompound, compoundName, id, structureType)
      ! Modules
   
      implicit none
   
      ! Input/output parameters
      type(t_StructureSet) :: sts
      integer              :: leftcalc
      integer              :: rightcalc
      integer              :: linknumber
      integer              :: icompound
      character(*)         :: compoundName
      character(*)         :: id
      ! In 3Di branches have both xy and branchid, chainage
   
      integer              :: structureType
   
      ! Local variables
      integer :: ibranch
      double precision :: x
      double precision :: y
      double precision :: chainage
      
   
      ! Program code
      chainage = 0d0
      x = 0d0
      y = 0d0
      ibranch = 0
      AddStructure_short = AddStructureByCalcPoints(sts, linknumber, chainage, icompound, compoundName, id, structureType, x, y, ibranch)
   end function AddStructure_short

   integer function AddStructureByCalcPoints(sts, linknumber, chainage, icompound, compoundName, id, structureType, x, y, ibranch)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_StructureSet) :: sts
      integer              :: linknumber
      double precision     :: chainage
      integer              :: icompound
      character(*)         :: compoundName
      character(*)         :: id
      double precision, optional :: x
      double precision, optional :: y
      integer, optional :: ibranch
      ! In 3Di branches have both xy and branchid, chainage

      integer              :: structureType

      ! Local variables
      integer              :: i, j

      type(t_structure), pointer       :: pstru

      ! Program code
      sts%Count = sts%Count+1
      i = sts%Count
      if (sts%Count > sts%Size) then
         call realloc(sts)
      endif
      call incStructureCount(sts, structureType)

      allocate(sts%struct(i)%linknumbers(1), sts%struct(i)%xCoordinates(1), sts%struct(i)%yCoordinates(1))
      
      sts%struct(i)%id                 = id
      sts%struct(i)%linknumbers(1)     = linknumber
      sts%struct(i)%chainage           = chainage
      sts%struct(i)%compound           = icompound
      sts%struct(i)%type               = structureType
      if (present(x) .and. present(y)) then
         sts%struct(i)%xCoordinates(1) = x
         sts%struct(i)%yCoordinates(1) = y
      else
         sts%struct(i)%xCoordinates(1) = 0d0
         sts%struct(i)%yCoordinates(1) = 0d0
      endif
      if (present(ibranch)) then
         sts%struct(i)%ibran = ibranch
      else
         sts%struct(i)%ibran = 0
      end if

      AddStructureByCalcPoints = sts%count
   end function AddStructureByCalcPoints

   !> Increments the counter for a specific type in the overall structure set.
   subroutine incStructureCount(sts, type)
      implicit none
      type(t_StructureSet), intent(inout) :: sts
      integer,              intent(in)    :: type !< Type id of the new structure.

      if (type > ST_MAX_TYPE) then
         call mess(LEVEL_ERROR, 'incStructureCount: invalid structure type: ', type)
         return
      end if

      sts%countByType(type) = sts%countByType(type) + 1
   end subroutine incStructureCount


   !> Gets the number of structures of a specific type.
   integer function getStructureCount(sts, type)
      implicit none
      type(t_StructureSet), intent(in) :: sts
      integer,              intent(in) :: type

      getStructureCount = 0
      if (type > ST_MAX_TYPE) then
         call mess(LEVEL_ERROR, 'incStructureCount: invalid structure type: ', type)
         return
      end if

      getStructureCount = sts%countByType(type)
   end function getStructureCount

subroutine deallocstructure(sts)
   ! Modules

   implicit none

   ! Input/output parameters
   type(t_structureSet), intent(inout)          :: sts

   ! Local variables
   integer                 :: i
   integer                 :: length

   ! Program code
   if (associated(sts%struct)) then
      length = sts%size
      do i = 1, length
         if (associated(sts%struct(i)%weir))       deallocate(sts%struct(i)%weir)
         if (associated(sts%struct(i)%orifice))    deallocate(sts%struct(i)%orifice)
         if (associated(sts%struct(i)%pump))       call dealloc(sts%struct(i)%pump)
         if (associated(sts%struct(i)%culvert))    call dealloc(sts%struct(i)%culvert)
         if (associated(sts%struct(i)%uniweir))    call dealloc(sts%struct(i)%uniweir)
         if (associated(sts%struct(i)%bridge))     deallocate(sts%struct(i)%bridge)
         if (associated(sts%struct(i)%generalst))  call dealloc(sts%struct(i)%generalst)
         if (associated(sts%struct(i)%extrares))   call dealloc(sts%struct(i)%extrares)
         if (associated(sts%struct(i)%xCoordinates)) deallocate(sts%struct(i)%xCoordinates)
         if (associated(sts%struct(i)%yCoordinates)) deallocate(sts%struct(i)%yCoordinates)
         if (associated(sts%struct(i)%linknumbers))  deallocate(sts%struct(i)%linknumbers)
         if (associated(sts%struct(i)%fu))           deallocate(sts%struct(i)%fu)
         if (associated(sts%struct(i)%ru))           deallocate(sts%struct(i)%ru)
         if (associated(sts%struct(i)%au))           deallocate(sts%struct(i)%au)
         if (associated(sts%struct(i)%u0))           deallocate(sts%struct(i)%u0)
         if (associated(sts%struct(i)%u1))           deallocate(sts%struct(i)%u1)
         
         sts%struct(i)%weir         => null()
         sts%struct(i)%orifice      => null()
         sts%struct(i)%pump         => null()
         sts%struct(i)%culvert      => null()  
         sts%struct(i)%uniweir      => null() 
         sts%struct(i)%bridge       => null() 
         sts%struct(i)%generalst    => null()
         sts%struct(i)%extrares     => null()
         sts%struct(i)%xCoordinates => null()
         sts%struct(i)%yCoordinates => null()
         sts%struct(i)%linknumbers  => null()
         sts%struct(i)%fu           => null()
         sts%struct(i)%ru           => null()
         sts%struct(i)%au           => null()
         sts%struct(i)%u0           => null()
         sts%struct(i)%u1           => null()
      enddo
      deallocate(sts%struct)
   endif

   if (allocated(sts%restartData)) then
      deallocate(sts%restartData)
   endif
   
   call dealloc(sts%hashlist_weir)
   call dealloc(sts%hashlist_pump)
   call dealloc(sts%hashlist_bridge)
   call dealloc(sts%hashlist_culvert)

   sts%struct       => null()
   sts%count         = 0
   sts%size          = 0
   sts%countByType   = 0
   sts%compoundcount = 0
   sts%hasExtraResistance = .false.
   numberOfWarnings = 0

end subroutine deallocstructure
!
   subroutine reallocstructure(sts)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_structureSet), intent(inout)          :: sts

      ! Local variables
      type(t_structure), pointer, dimension(:)     :: oldsts

      ! Program code

      if (sts%Size > 0) then
         oldsts=>sts%struct
      endif

      if (sts%growsBy <=0) then
         sts%growsBy = 200
      endif
      allocate(sts%struct(sts%Size+sts%growsBy))

      if (sts%Size > 0) then
         sts%struct(1:sts%Size) = oldsts(1:sts%Size)
         deallocate(oldsts)
      endif
      sts%Size = sts%Size+sts%growsBy
   end subroutine

!> Deallocates a forcing list and sets all counters to zero.
subroutine deallocForcingList(fs)
   ! Modules

   implicit none

   ! Input/output parameters
   type(t_forcingList), intent(inout) :: fs !< The forcing list.

   ! Local variables

   ! Program code
   if (associated(fs%forcing)) then
      deallocate(fs%forcing)
   endif
   
   fs%forcing => null()
   fs%size  = 0
   fs%count = 0

end subroutine
!
!

subroutine reallocForcingList(fs)
   ! Modules

   implicit none

   ! Input/output parameters
   type(t_forcingList), intent(inout)          :: fs

   ! Local variables
   type(t_forcing), pointer, dimension(:)      :: oldforcing

   ! Program code

   if (fs%Size > 0) then
      oldforcing=>fs%forcing
   endif

   if (fs%growsBy <=0) then
      fs%growsBy = 200
   endif
   allocate(fs%forcing(fs%Size+fs%growsBy))

   if (fs%Size > 0) then
      fs%forcing(1:fs%Size) = oldforcing(1:fs%Size)
      deallocate(oldforcing)
   endif
   fs%Size = fs%Size+fs%growsBy
end subroutine

   double precision function getTableValueStruc(pstru, x) result (res)
      double precision :: x
      type(t_structure) :: pstru

      select case (pstru%type)
      case (ST_CULVERT)
          res = interpolate(pstru%culvert%lossCoeff, x)
      case default
          res = 0.0d0
      end select
   end function


   subroutine getCrossSection(sts, crs, istru, pcross)
      type(t_StructureSet)                :: sts
      type(t_CrossSectionSet)             :: crs
      integer                             :: istru
      type(t_crossSection), pointer       :: pcross

      integer           :: icross

      select case(sts%struct(istru)%type)
         case (ST_CULVERT)
            icross = sts%struct(istru)%culvert%crosssectionnr
         case (ST_BRIDGE)
            icross = sts%struct(istru)%bridge%crosssectionnr
         case default
            icross = 0
      end select
      if (icross==0) then
         pcross => null()
      else
         pcross => crs%cross(icross)
      endif
   end subroutine getCrossSection

   !> Set structure parameter
   logical function setValueStruc(sts, istru, iparam, value)
      use m_GlobalParameters
      
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_structureSet)             :: sts         !< set containing structure data
      integer                          :: istru       !< structure sequence number
      integer                          :: iparam      !< parameter to be changed
      double precision                 :: value       !< new value

!
!
! Local variables
!
      character(CharLn)                   :: line
!
!
!! executable statements -------------------------------------------------------
!
       SetValueStruc = .true.
       if (iparam==CFiCrestWidth .and. value < 0.0) then
          line = 'The crest width for structure with id: '//trim(sts%struct(istru)%id) //' is less than zero.'
          call setMessage(LEVEL_ERROR, line)
          return
       endif

       select case (sts%struct(istru)%type)
       case (ST_WEIR)
          select case (iparam)
          case (CFiCrestLevel)
             sts%struct(istru)%weir%crestlevel=value
          case (CFiCrestWidth)
             sts%struct(istru)%weir%crestwidth=value
          case default
             SetValueStruc = .false.
          end select
       case (ST_ORIFICE)
          select case (iparam)
          case (CFiCrestLevel)
             sts%struct(istru)%orifice%crestlevel=value
          case (CFiCrestWidth)
             sts%struct(istru)%orifice%crestwidth=value
          case (CFiGateOpeningHeight)
             sts%struct(istru)%orifice%openlevel=value - sts%struct(istru)%orifice%crestlevel
          case (CFiGateLowerEdgeLevel)
             sts%struct(istru)%orifice%openlevel =value
          case default
             SetValueStruc = .false.
          end select
       case (ST_CULVERT)
          if (iparam==CFiValveOpening) then
             sts%struct(istru)%culvert%valveOpening=value
          else
            SetValueStruc = .false.
          endif
       case (ST_PUMP)
          if (iparam==CFiPumpCapacity) then
             sts%struct(istru)%pump%capacity(1) = value
           else
             SetValueStruc = .false.
           endif
       case (ST_GENERAL_ST)
          select case (iparam)
          case (CFiCrestLevel)
             sts%struct(istru)%generalst%zs=value
          case (CFiCrestWidth)
             sts%struct(istru)%generalst%ws=value
          case (CFiGateLowerEdgeLevel)
             sts%struct(istru)%generalst%gateLowerEdgeLevel =value
          case (CFiGateOpeningHeight)
             sts%struct(istru)%generalst%gateLowerEdgeLevel =value + sts%struct(istru)%generalst%zs
          case default
             SetValueStruc = .false.
          end select
       case default
         !nothing
       end select

       if (.not. allocated(sts%restartData).and. (sts%count > 0)) then
          allocate(sts%restartData(sts%count, CFiHighestParameter))
          sts%restartData = missingValue
       endif

       if (iparam <= CFiHighestParameter) then
          sts%restartData(istru, iparam) = value
       else
          SetValueStruc = .false.
       endif

   end function setValueStruc

   !> Get structure parameter
   double precision function getValueStruc(sts, istru, iparam)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_structureSet)             :: sts         !< set containing structure data
      integer                          :: istru       !< structure sequence number
      integer                          :: iparam      !< parameter of interest
!
!
! Local variables
!
      character(CharLn)                   :: line
!
!
!! executable statements -------------------------------------------------------
!
       select case (sts%struct(istru)%type)
       case (ST_WEIR)
           if (iparam == CFiCrestLevel) getValueStruc = sts%struct(istru)%weir%crestlevel
           if (iparam == CFiCrestWidth) getValueStruc = sts%struct(istru)%weir%crestwidth
       case (ST_ORIFICE)
           if (iparam == CFiCrestLevel)         getValueStruc = sts%struct(istru)%orifice%crestlevel
           if (iparam == CFiCrestWidth)         getValueStruc = sts%struct(istru)%orifice%crestwidth
           if (iparam == CFiGateLowerEdgeLevel) getValueStruc = sts%struct(istru)%orifice%openlevel
           if (iparam == CFiGateOpeningHeight)  getValueStruc = sts%struct(istru)%orifice%openlevel - sts%struct(istru)%orifice%crestlevel
       case (ST_CULVERT)
           if (iparam == CFiCrestLevel)         getValueStruc = sts%struct(istru)%orifice%crestlevel
           if (iparam == CFiGateLowerEdgeLevel) getValueStruc = sts%struct(istru)%orifice%openlevel
           if (iparam == CFiGateOpeningHeight)  getValueStruc = sts%struct(istru)%orifice%openlevel - sts%struct(istru)%orifice%crestlevel
       case (ST_PUMP)
           getValueStruc = sts%struct(istru)%pump%capacity(1)
       case (ST_GENERAL_ST)
           if (iparam == CFiCrestLevel)         getValueStruc = sts%struct(istru)%generalst%zs
           if (iparam == CFiCrestWidth)         getValueStruc = sts%struct(istru)%generalst%ws
           if (iparam == CFiGateLowerEdgeLevel) getValueStruc = sts%struct(istru)%generalst%gateLowerEdgeLevel
           if (iparam == CFiGateOpeningHeight)  getValueStruc = sts%struct(istru)%generalst%gateLowerEdgeLevel - sts%struct(istru)%generalst%zs
       case default
         !nothing
       end select

       if (iparam == CFiCrestWidth .and. getValueStruc < 0.0) then
          line = 'The crest width for structure with id: '//trim(sts%struct(istru)%id) //' is less than zero.'
          call setMessage(LEVEL_ERROR, line)
          return
       endif

   end function getValueStruc

   subroutine reIndexCrossSections(sts, crs)
      ! modules

      implicit none
      ! variables
      type(t_structureSet)             :: sts       !< Current structure set
      type(t_CrossSectionSet)          :: crs       !< Current cross-section set
      ! local variables
      integer i
      !program code

      ! Check for structures with cross sections.
      ! since the cross section list is now sorted the locations are changed
      do i = 1, sts%count
         select case(sts%struct(i)%type)
            case(ST_CULVERT)
               if (sts%struct(i)%culvert%crosssectionnr > 0) then
                  sts%struct(i)%culvert%crosssectionnr = crs%crossSectionIndex(sts%struct(i)%culvert%crosssectionnr)
                  sts%struct(i)%culvert%pcross => crs%cross(sts%struct(i)%culvert%crosssectionnr)
               endif
            case(ST_BRIDGE)
               if (sts%struct(i)%bridge%crosssectionnr > 0) then
                  sts%struct(i)%bridge%crosssectionnr = crs%crossSectionIndex(sts%struct(i)%bridge%crosssectionnr)
                  sts%struct(i)%bridge%pcross => crs%cross(sts%struct(i)%bridge%crosssectionnr)
               endif
         end select
      enddo

   end subroutine reIndexCrossSections

   subroutine SetRestartDataForStructures(sts)

      ! modules
      use m_globalParameters
      implicit none

      ! variables
      type(t_structureSet)             :: sts       !< Current structure set

      ! local variables
      integer iparam, istru
      logical success

      !program code
      success = .true.
      do istru = 1, sts%count
         do iparam = 1, CFiHighestParameter
            if (abs(sts%restartData(istru, iparam) - missingValue) > 1d0) then
               success = success .and. setValueStruc(sts, istru, iparam, dble(sts%restartData(istru, iparam)))
            endif
         enddo
      enddo
      if (.not. success) then
         call setMessage(LEVEL_FATAL,"INTERNAL ERROR: inconsistent restart data for RTC-controlled structure data")
      endif

   end subroutine SetRestartDataForStructures

   integer function GetStrucType_from_string(string)
      use string_module

      character(len=*) :: string

      call str_lower(string, 999)
      select case(trim(string))
      case ('pump')
         GetStrucType_from_string = ST_PUMP
      case ('generalstructure')
         GetStrucType_from_string = ST_GENERAL_ST
      case ('weir')
         GetStrucType_from_string = ST_WEIR
      case ('orifice')
         GetStrucType_from_string = ST_ORIFICE
      case ('gate')
         GetStrucType_from_string = ST_GATE
      case ('culvert')
         GetStrucType_from_string = ST_CULVERT
      case ('universalweir')
         GetStrucType_from_string = ST_UNI_WEIR
      case ('dambreak')
         GetStrucType_from_string = ST_DAMBREAK
      case ('bridge')
         GetStrucType_from_string = ST_BRIDGE
      case ('compound')
         GetStrucType_from_string = ST_COMPOUND
      case default
         GetStrucType_from_string = -1
      end select
   end function GetStrucType_from_string


   subroutine GetStrucType_from_int(istrtype, strng)
      integer, intent(in) :: istrtype

      character (len=*), intent(out) :: strng
      
      select case(istrtype)
         case (ST_PUMP)
            strng = 'pump'
         case (ST_GENERAL_ST)
            strng = 'general_structure'
         case (ST_WEIR)
            strng = 'weir'
         case (ST_ORIFICE)
            strng = 'orifice'
         case (ST_GATE)
            strng = 'gate'
         case (ST_CULVERT)
            strng = 'culvert'
         case (ST_UNI_WEIR)
            strng = 'universalweir'
         case (ST_DAMBREAK)
            strng = 'dambreak'
         case (ST_BRIDGE)
            strng = 'bridge'
         case default
            strng = 'unknown'
      end select
   end subroutine GetStrucType_from_int

   function getStructureById(sts, id) result(pstru)
      type(t_structureSet), intent(in)    :: sts       !< Current structure set
      character(len=*), intent(in)        :: id
      type(t_structure), pointer          :: pstru

      integer :: istruc

      pstru => null()
      do istruc = 1, sts%count
         if (trim(sts%struct(istruc)%id) == trim(id)) then
            pstru => sts%struct(istruc)
         endif
      enddo
      ! not found: return -1

   end function getStructureById

   !> Gets the current value of the crest level for a given structure.
   !! If the type of the given structure does not have a crest, then it gets a dummy high value 1d10.
   !! The value is the actual value, e.g., %zs_actual, so it may differ from %zs
   !! if sanity  checks have been applied (see also: check_for_changes_on_structure()).
   double precision function get_crest_level(struc)
      type(t_structure), intent(in) :: struc
      
       select case (struc%type)
          case (ST_WEIR)
             get_crest_level = struc%weir%crestlevel
          case (ST_UNI_WEIR)
             get_crest_level = struc%uniweir%crestlevel_actual
          case (ST_ORIFICE)
             get_crest_level = struc%orifice%crestlevel
          case (ST_CULVERT)
             get_crest_level = max(struc%culvert%leftlevel, struc%culvert%rightlevel)
          case (ST_PUMP)
             get_crest_level = huge(1d0)
          case (ST_GENERAL_ST)
             get_crest_level = struc%generalst%zs_actual
          case (ST_BRIDGE)
             get_crest_level = struc%bridge%bedLevel
          case default
             get_crest_level = huge(1d0)
       end select

   end function get_crest_level

   !> Gets the pointer of the crest level for a given structure.
   !! If the type of the given structure does not have a crest, then it gets a null pointer
   !! This pointer points directly to, e.g., the %zs (i.e., *not* to %zs_actual),
   !! so it is suitable for adjusting the %zs.
   type(c_ptr) function get_crest_level_c_loc(struc)
      type(t_structure), intent(in) :: struc
      
       select case (struc%type)
          case (ST_WEIR)
             get_crest_level_c_loc = c_loc(struc%weir%crestlevel)
          case (ST_UNI_WEIR)
             get_crest_level_c_loc = c_loc(struc%uniweir%crestlevel)
          case (ST_ORIFICE)
             get_crest_level_c_loc = c_loc(struc%orifice%crestlevel)
          case (ST_GENERAL_ST)
             get_crest_level_c_loc = c_loc(struc%generalst%zs)
          case default
             get_crest_level_c_loc = C_NULL_PTR
       end select

   end function get_crest_level_c_loc
   
   !> Gets the pointer of the gate lower edge level for a given structure.
   !! If the type of the given structure is not general structure, then it gets a null pointer
   !! This pointer points directly to the %gateLowerEdgeLevel (i.e., *not* to %gateLowerEdgeLevel_actual),
   !! so it is suitable for adjusting the %gateLowerEdgeLevel.
   type(c_ptr) function get_gate_lower_edge_level_c_loc(struc)
      type(t_structure), intent(in) :: struc
      
       select case (struc%type)
          case (ST_ORIFICE)
             get_gate_lower_edge_level_c_loc = c_loc(struc%orifice%openlevel)
          case (ST_GENERAL_ST)
             get_gate_lower_edge_level_c_loc = c_loc(struc%generalst%gateLowerEdgeLevel)
          case default
             get_gate_lower_edge_level_c_loc = C_NULL_PTR
       end select

   end function get_gate_lower_edge_level_c_loc
   
   !> Gets the pointer of the valve opening height for a given culvert structure.
   !! If the type of the given structure is not culvert, then it gets a null pointer
   !! This pointer points directly to the %culvert%valveOpening.
   type(c_ptr) function get_valve_opening_height_c_loc(struc)
      type(t_structure), intent(in) :: struc
      
       select case (struc%type)
          case (ST_CULVERT)
             get_valve_opening_height_c_loc = c_loc(struc%culvert%valveOpening)
          case default
             get_valve_opening_height_c_loc = C_NULL_PTR
       end select

   end function get_valve_opening_height_c_loc
   
   !> Gets the pointer of the gate opening width for a given structure.
   !! If the type of the given structure is not general structure, then it gets a null pointer
   !! This pointer points directly to the %gateopeningwidth (i.e., *not* to %gateopeningwidth_actual),
   !! so it is suitable for adjusting the %gateopeningwidth.
   type(c_ptr) function get_gate_opening_width_c_loc(struc)
      type(t_structure), intent(in) :: struc
      
       select case (struc%type)
          case (ST_GENERAL_ST)
             get_gate_opening_width_c_loc = c_loc(struc%generalst%gateopeningwidth)
          case default
             get_gate_opening_width_c_loc = C_NULL_PTR
       end select

   end function get_gate_opening_width_c_loc


   !> Gets the pointer of the gate door height for a given structure.
   !! If the type of the given structure is not general structure, then it gets a null pointer
   !! This pointer points directly to the %gatedoorheight.
   type(c_ptr) function get_gate_door_height_c_loc(struc)
      type(t_structure), intent(in) :: struc
      
       select case (struc%type)
          case (ST_GENERAL_ST)
             get_gate_door_height_c_loc = c_loc(struc%generalst%gatedoorheight)
          case default
             get_gate_door_height_c_loc = C_NULL_PTR
       end select

   end function get_gate_door_height_c_loc   


   double precision function get_width(struc)
      type(t_structure), intent(in) :: struc
      
       select case (struc%type)
          case (ST_WEIR)
             get_width = struc%weir%crestwidth
          case (ST_GENERAL_ST)
             get_width = struc%generalst%ws_actual
          case (ST_ORIFICE)
             get_width = struc%orifice%crestwidth
          case default
             get_width = huge(1d0)
       end select

   end function get_width


   !> Returns the threshold level for a structure that determines how it blocks incoming water levels.
   !! This can typically be used to initialize 1D water levels along branches, in between structures.
   !! Most structures have their watershed threshold identical to their crest level, but some (orifice)
   !! always keep the left and right levels separated.
   double precision function get_watershed_threshold(struc)
      type(t_structure), intent(in) :: struc
      
       select case (struc%type)
       case (ST_ORIFICE)
          get_watershed_threshold = huge(1d0)
       case default
          get_watershed_threshold = get_crest_level(struc)
       end select

   end function get_watershed_threshold
   
   double precision function get_gle(struc)
      
      type (t_structure), intent(inout) :: struc
      
      select case(struc%type)
      case (ST_ORIFICE)
         get_gle = struc%orifice%openlevel 
      case (ST_GENERAL_ST)
         get_gle = struc%generalst%gateLowerEdgeLevel_actual
      case (ST_CULVERT)
         get_gle = max(struc%culvert%leftlevel, struc%culvert%rightlevel) + struc%culvert%valveOpening
      case default
         get_gle = huge(1d0)
      end select
   end function get_gle
   
   double precision function get_opening_height(struc)
      
      type (t_structure), intent(inout) :: struc
      
      select case(struc%type)
      case (ST_ORIFICE)
         get_opening_height = struc%orifice%openlevel - struc%orifice%crestlevel
      case (ST_GENERAL_ST)
         get_opening_height = struc%generalst%gateLowerEdgeLevel_actual - struc%generalst%zs_actual
      case (ST_CULVERT)
         get_opening_height = struc%culvert%valveOpening
     end select
   end function get_opening_height
   
   double precision function get_valve_opening(struc)
      
      type (t_structure), intent(inout) :: struc
      
      select case(struc%type)
      case (ST_CULVERT)
         get_valve_opening = struc%culvert%valveOpening
      end select
   end function get_valve_opening
 
   !> Gets culvert state for a given structure
   integer function get_culvert_state(struc)
      
      type (t_structure), intent(inout) :: struc
      
      select case(struc%type)
      case (ST_CULVERT)
         get_culvert_state = struc%culvert%state
      case default
         get_culvert_state = -999 ! unknown
      end select
   end function get_culvert_state

   subroutine fill_hashtable_sts(sts)
   
      type (t_structureSet), intent(inout), target :: sts
      
      integer                                      :: ist
      character(len=idlen), dimension(:), pointer  :: ids_weir
      character(len=idlen), dimension(:), pointer  :: ids_culvert
      character(len=idlen), dimension(:), pointer  :: ids_bridge
      character(len=idlen), dimension(:), pointer  :: ids_pump
      character(len=idlen), dimension(:), pointer  :: ids_structure
      
      if (sts%Count <= 0) return    ! Nothing to hash
      
      allocate(sts%hashlist_weir%id_list(sts%Count))
      allocate(sts%hashlist_culvert%id_list(sts%Count))
      allocate(sts%hashlist_bridge%id_list(sts%Count))
      allocate(sts%hashlist_pump%id_list(sts%Count))
      allocate(sts%hashlist_structure%id_list(sts%Count))

      ! TODO: needs if(allocated's)
      sts%hashlist_weir%id_count = sts%Count
      sts%hashlist_culvert%id_count = sts%Count
      sts%hashlist_bridge%id_count = sts%Count
      sts%hashlist_pump%id_count = sts%Count
      sts%hashlist_structure%id_count = sts%Count
      
      ids_weir => sts%hashlist_weir%id_list
      ids_culvert => sts%hashlist_culvert%id_list
      ids_bridge => sts%hashlist_bridge%id_list
      ids_pump => sts%hashlist_pump%id_list
      ids_structure => sts%hashlist_structure%id_list
      ids_weir    = ' '
      ids_culvert = ' '
      ids_bridge  = ' '
      ids_pump    = ' '
      ids_structure = ' '
      
      
      do ist = 1, sts%count
         ids_structure(ist) = sts%struct(ist)%id
         !
         ! NOTE: UNST-2576: the mapping below are still old and aimed at SOBEK3.
         !       Under D-Flow FM/RHU, we'll consider separate hashlists for each type, not alltogether as weir. 
         !
         select case(sts%struct(ist)%type)
         case (ST_WEIR, ST_ORIFICE,ST_GENERAL_ST, ST_UNI_WEIR)
            ids_weir(ist) = sts%struct(ist)%id
         case (ST_CULVERT)
            ids_culvert(ist) = sts%struct(ist)%id
         case (ST_BRIDGE)
            ids_bridge(ist) = sts%struct(ist)%id
         case (ST_PUMP)
            ids_pump(ist) = sts%struct(ist)%id
         end select            
      enddo
      
      call hashfill(sts%hashlist_weir   )
      call hashfill(sts%hashlist_culvert)
      call hashfill(sts%hashlist_bridge )
      call hashfill(sts%hashlist_pump   )
      call hashfill(sts%hashlist_structure   )
      
   end subroutine fill_hashtable_sts
   
   subroutine set_crest_level(struc, value)
   
      type (t_structure), intent(inout) :: struc
      double precision, intent(in)      :: value
   
      select case(struc%type)
      case (ST_WEIR)
         struc%weir%crestlevel=value
      case (ST_ORIFICE)
         struc%orifice%crestlevel=value
      case (ST_UNI_WEIR)
         struc%uniweir%crestlevel=value
      case (ST_GENERAL_ST)
         struc%generalst%zs=value
      case default
         !nothing
      end select
   end subroutine set_crest_level

   subroutine set_crest_width(struc, value)
      
      type (t_structure), intent(inout) :: struc
      double precision, intent(in)      :: value
      
      select case(struc%type)
      case (ST_WEIR)
         struc%weir%crestwidth=value
      case (ST_ORIFICE)
         struc%orifice%crestwidth=value
      case (ST_GENERAL_ST)
         struc%generalst%ws=value
      end select
   end subroutine set_crest_width

   subroutine set_gle(struc, value)
      
      type (t_structure), intent(inout) :: struc
      double precision, intent(in)      :: value
      
      select case(struc%type)
      case (ST_ORIFICE)
         struc%orifice%openlevel =value
      case (ST_GENERAL_ST)
         struc%generalst%gateLowerEdgeLevel =value
      end select
   end subroutine set_gle
   
   subroutine set_opening_height(struc, value)
      
      type (t_structure), intent(inout) :: struc
      double precision, intent(in)      :: value
      
      select case(struc%type)
      case (ST_ORIFICE)
         struc%orifice%openlevel=value + struc%orifice%crestlevel
      case (ST_GENERAL_ST)
         struc%generalst%gateLowerEdgeLevel =value + struc%generalst%zs
      end select
   end subroutine set_opening_height
   
   subroutine set_valve_opening(struc, value)
      
      type (t_structure), intent(inout) :: struc
      double precision, intent(in)      :: value
      
      select case(struc%type)
      case (ST_CULVERT)
         struc%culvert%valveOpening=value
      end select
   end subroutine set_valve_opening
   
   !> Gets pump capacity, in the direction of the pump's orientation.
   !! (So possibly negative, when direction < 0.)
   double precision function GetPumpCapacity(stru)
      implicit none
      type(t_structure), intent(in)   :: stru !< Structure
         
      if (stru%type /= ST_PUMP) then
         return
      end if

      if (stru%pump%is_active) then
         GetPumpCapacity = stru%pump%current_capacity
      else
         GetPumpCapacity = 0d0
      end if

   end function GetPumpCapacity


   !> Gets a c_ptr to the pump capacity field.
   !! Use this function when it is needed to later adjust the pump capacity via the c_ptr.
   !! When read-only getting is sufficient, use getPumpCapacity instead.
   type(c_ptr) function get_pump_capacity_c_loc(struc)
      type(t_structure), intent(in) :: struc !< The structure object containing the pump.
      
      select case (struc%type)
      case (ST_PUMP)
         get_pump_capacity_c_loc = c_loc(struc%pump%capacity(1))
      case default
         get_pump_capacity_c_loc = C_NULL_PTR
       end select

   end function get_pump_capacity_c_loc   


   !> Gets pump actual stage.
   double precision function GetPumpStage(stru)
      implicit none   
      type(t_structure), intent(in)   :: stru !< Structure 

      if (stru%type /= ST_PUMP) then
         return
      end if
         
      if (stru%pump%nrstages > 0) then
         GetPumpStage = dble(stru%pump%actual_stage)
      else
         GetPumpStage = -1
      end if

      
   end function GetPumpStage
   
   !> Gets pump reduction factor.
   double precision function GetPumpReductionFactor(stru)
      implicit none   
      type(t_structure), intent(in)   :: stru !< Structure
         
      if (stru%type /= ST_PUMP) then
         return
      end if

      GetPumpReductionFactor = stru%pump%reduction_factor
   end function GetPumpReductionFactor


   !> Gets pump suction side level that was used in latest prepareComputePump call.
   double precision function getPumpSsLevel(stru)
      implicit none   
      type(t_structure), intent(in)   :: stru !< Structure
         
      if (stru%type /= ST_PUMP) then
         return
      end if

      getPumpSsLevel = stru%pump%ss_level
   end function getPumpSsLevel


   !> Gets pump delivery side level that was used in latest prepareComputePump call.
   double precision function getPumpDsLevel(stru)
      implicit none   
      type(t_structure), intent(in)   :: stru !< Structure
         
      if (stru%type /= ST_PUMP) then
         return
      end if

      getPumpDsLevel = stru%pump%ds_level
   end function getPumpDsLevel


   !> Initializes the flow link administration for a single structure.
   function initialize_structure_links(struct, numlinks, links, wu) result(istat)

      type(t_structure),               intent(inout) :: struct   !< The structure object to be initialized.
      integer,                         intent(in   ) :: numlinks !< The number of flow links affected by this structure.
      integer, dimension(:),           intent(in   ) :: links    !< (numlinks) The flow link numbers affected by this structure.
      double precision, dimension(:),  intent(in   ) :: wu       !< (numlinks) The width of the flow links affected by this structure.
      integer                                        :: istat    !< Result status (0 if successful).

      istat = 0
      allocate(struct%linknumbers(numlinks), struct%fu(numlinks), struct%ru(numlinks), struct%au(numlinks), struct%u0(numlinks), struct%u1(numlinks))
      struct%numlinks = numlinks
      struct%linknumbers = links(1:numlinks)
      struct%fu = 0d0
      struct%ru = 0d0
      struct%au = 0d0
      struct%u0 = 0d0
      struct%u1 = 0d0
      
      select case(struct%type)
      case (ST_GENERAL_ST) ! REMARK: for version 2 files weirs, orifices and gates are implemented as general structures
         allocate(struct%generalst%widthcenteronlink(numlinks), struct%generalst%gateclosedfractiononlink(numlinks), struct%generalst%sOnCrest(numlinks), struct%generalst%state(3,numlinks))
         struct%generalst%sOnCrest(1:numlinks) = 0d0
         struct%generalst%state = 0
         allocate(struct%generalst%fu(3,numlinks), struct%generalst%ru(3,numlinks), struct%generalst%au(3,numlinks))
         struct%generalst%fu = 0d0
         struct%generalst%ru = 0d0
         struct%generalst%au = 0d0
      case (ST_CULVERT, ST_UNI_WEIR, ST_ORIFICE, ST_GATE, ST_WEIR, ST_PUMP, ST_BRIDGE)
         if (numlinks > 1) then
            istat = 1
            call setmessage(LEVEL_ERROR, 'Multiple links for culvert structures is not supported, check structure'//trim(struct%id))
         endif
      case default
         ! A reminder not to forget other structures that are added:
         istat = 1
         call setMessage(LEVEL_ERROR, 'Internal error, this structure type is not (yet) implemented in initialize_structure')
      end select

   end function initialize_structure_links

   !> Set fu, ru and au in a structure. This subroutine is essential for compound
   !! structures. Since the compound structure relies on the fact that FU, RU and 
   !! AU are set for all of its underlying structures.
   subroutine set_fu_ru_structure(struct, L0, fu, ru, au)
      type (t_structure) , intent(inout)  :: struct    !< Structure object.
      integer,             intent(in   )  :: L0        !< Internal link index.
      double precision,    intent(in   )  :: fu        !< FU coefficient.
      double precision,    intent(in   )  :: ru        !< RU coefficient.
      double precision,    intent(in   )  :: au        !< Flow area.
      
      struct%fu(L0) = fu
      struct%ru(L0) = ru
      struct%au(L0) = au
   end subroutine set_fu_ru_structure


   !> Initialize the %*_actual parameters for all network structure to the
   !! currently set direct parameters (e.g., %zs_actual = %zs).
   !! This routine is intended to maintain up-to-date actual parameter values
   !! for output purposes, even when the structure was dry (then the sanity
   !! checks were not applied).
   subroutine initialize_structures_actual_params(sts)
      use messagehandling
      use precision_basics
      type(t_StructureSet), intent(in) :: sts !< Structure set that must be initialized.

      type (t_structure), pointer :: pstru
      integer :: istru
      
      do istru=1,sts%Count
         pstru => sts%struct(istru)
         select case(pstru%type)
         case (ST_GENERAL_ST)
            pstru%generalst%zs_actual                 = pstru%generalst%zs
            pstru%generalst%ws_actual                 = pstru%generalst%ws
            pstru%generalst%gateLowerEdgeLevel_actual = pstru%generalst%gateLowerEdgeLevel
            pstru%generalst%gateopeningwidth_actual   = pstru%generalst%gateopeningwidth
         case(ST_BRIDGE)
            pstru%bridge%bedLevel_actual              = pstru%bridge%bedLevel
         case(ST_UNI_WEIR)
            pstru%uniweir%crestlevel_actual           = pstru%uniweir%crestlevel
         end select
      end do

   end subroutine initialize_structures_actual_params


   !> check for differences between input parameters and actual parameters
   subroutine check_for_changes_on_structures(level, pstru, bob0)
      use messagehandling
      use precision_basics
      type (t_structure), pointer, intent(in) :: pstru
      integer,                     intent(in) :: level
      double precision,            intent(in) :: bob0(2)
      
      select case(pstru%type)
      case (ST_GENERAL_ST)
         call compare_and_warn(level, pstru%generalst%zs, pstru%generalst%zs_actual, 'crest level', pstru%id)
         call compare_and_warn(level, pstru%generalst%ws, pstru%generalst%ws_actual, 'crest width', pstru%id)
         call compare_and_warn(level, pstru%generalst%gateLowerEdgeLevel, pstru%generalst%gateLowerEdgeLevel_actual, 'gate lower edge level', pstru%id)
         call compare_and_warn(level, pstru%generalst%gateopeningwidth, pstru%generalst%gateopeningwidth_actual, 'gate opening width', pstru%id)
      case(ST_BRIDGE)
         call compare_and_warn(level, pstru%bridge%bedLevel, pstru%bridge%bedLevel_actual, 'bed level', pstru%id)
         call compare_and_warn(level, pstru%bridge%flowArea, pstru%bridge%flowArea_actual, 'flow area', pstru%id)
      case(ST_UNI_WEIR)
         call compare_and_warn(level, pstru%uniweir%crestlevel, pstru%uniweir%crestlevel_actual, 'crest level', pstru%id)
      case(ST_CULVERT)
         call compare_and_warn(level, pstru%culvert%bob_orig(1), bob0(1), 'bed level of the channel at the left side', pstru%id)
         call compare_and_warn(level, pstru%culvert%bob_orig(2), bob0(2), 'bed level of the channel at the right side', pstru%id)
      end select
         
   end subroutine check_for_changes_on_structures
   
   !> Check for changes in "actual"  parameters and produce a message.
   !! The number of messsages is limited to MAXWARNINGS
   subroutine compare_and_warn(level, val_org, val_new, par, id)
      use precision
      integer         ,  intent(in) :: level        !< Message level 
      double precision,  intent(in) :: val_org      !< Original value
      double precision,  intent(in) :: val_new      !< Adjusted value (_actual)
      character (len=*), intent(in) :: par          !< Name of the parameter
      character (len=*), intent(in) :: id           !< Id of the sturcture
      
      if (numberOfWarnings == Maxwarnings+1) then
         numberOfWarnings = numberOfWarnings+1
         write (msgbuf, '(a,i0,a)') 'The number of warnings for changed structure parameters exceeds ', &
                     Maxwarnings, ', subsequent warnings are supppressed'
         call setmessage(LEVEL_INFO,msgbuf)
      else if (numberOfWarnings <= Maxwarnings) then
         if (comparereal(val_org, val_new) /= 0) then
            numberOfWarnings = numberOfWarnings+1
            write(msgbuf,'(a,f8.2,a,f8.2,a)') 'The '// trim(par)//' for '''//trim(id)//''' is changed from ', val_org, ' into ', val_new, '.'
            call SetMessage(level, msgbuf)
         endif
      endif
      
      
   end subroutine compare_and_warn


   !> Gets discharge of a structure that belongs to a compound structure.
   double precision function get_discharge_under_compound_struc(pstru, L0, s1k1, s1k2, teta)
      type (t_structure), intent(inout) :: pstru       !< structure
      integer,            intent(in)    :: L0          !< local link index
      double precision,   intent(in)    :: s1k1, s1k2  !< water level on nodes k1 and k2
      double precision,   intent(in)    :: teta        !< Theta-value of theta-time-integration for this flow link.

      double precision :: u1
      
      ! NOTE: pstru%u1 must have been calculated before in set_u1q1_structure()
      get_discharge_under_compound_struc = pstru%au(L0)* (teta * pstru%u1(L0) + (1d0-teta) * pstru%u0(L0))

   end function get_discharge_under_compound_struc


   !> Sets u0 (flow velocity at previous timestep) to u1 for a structure's flow links.
   !! These are the structure's own velocities, not the flow link's velocities,
   !! this is relevant under compound structures.
   !! This routine typically should be called once (at the start of) every timestep.
   subroutine set_u0isu1_structures(sts)
      type(t_structureset), intent(inout) :: sts       !< Dataset containing structure information.

      integer :: istru, count
      type(t_structure), pointer :: pstru
      
      count = sts%Count
      do istru = 1, count
         pstru => sts%struct(istru)
         if (pstru%numlinks > 0 ) then
            pstru%u0(:) = pstru%u1(:)
         endif
         
      enddo
      
   end subroutine set_u0isu1_structures


   !> Sets u1 for each structure's flow links.
   !! These are the structure's own velocities, not the flow link's velocities,
   !! this is relevant under compound structures.
   !! This routine typically should be called once (at the end of) every timestep.
   subroutine set_u1q1_structure(pstru, L0, s1k1, s1k2, teta)
      type (t_structure), intent(inout) :: pstru       !< structure
      integer,            intent(in)    :: L0          !< local link index
      double precision,   intent(in)    :: s1k1, s1k2  !< water level on nodes k1 and k2
      double precision,   intent(in)    :: teta        !< Theta-value of theta-time-integration for this flow link. (not used yet)

      pstru%u1(L0) = pstru%ru(L0) - pstru%fu(L0)*( s1k2 - s1k1 )
      ! NOTE: No q1 part here, since pstru%q1 does not exist.
      
   end subroutine set_u1q1_structure

   !> Set fu and ru to zero, when a structure link is closed
   subroutine reset_fu_ru_for_structure_link(L, lin2str, struct)
      integer,                         intent(in   )  :: L           !< Link number
      integer, dimension(:),           intent(in   )  :: lin2str     !< Indirection table from L to structure number
      type(t_structure), dimension(:), intent(inout)  :: struct      !< Array containing structure information
      
      integer :: istru
      integer :: i, L0
      
      if (L > size(lin2str) ) then
         return
      endif
      
      if (lin2str(L) > 0) then
         istru = lin2str(L)
         do i = 1, struct(istru)%numlinks
            if (L==struct(istru)%linknumbers(i)) then
               L0 = i
            endif
         enddo
         struct(istru)%fu(L0) = 0d0
         struct(istru)%ru(L0) = 0d0
         struct(istru)%au(L0) = 0d0
         if (struct(istru)%type == ST_GENERAL_ST) then
            struct(istru)%generalst%fu(:,L0) = 0d0
            struct(istru)%generalst%ru(:,L0) = 0d0
            struct(istru)%generalst%au(:,L0) = 0d0
         endif
      endif
      
   end subroutine reset_fu_ru_for_structure_link
   
end module m_1d_structures
