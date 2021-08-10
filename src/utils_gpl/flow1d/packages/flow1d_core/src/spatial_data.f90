!> Define grid values on the grid (level, discharge, salinity, dispersion or Windshield)
module m_spatial_data
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
!  $Id: spatial_data.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/spatial_data.f90 $
!-------------------------------------------------------------------------------

   use m_alloc
   use m_tables
   use MessageHandling
   use m_GlobalParameters
   implicit none
   
   private

   public realloc
   public dealloc
   public AddValues
   public getQuantityNumber
   public GetDispersionParameters
   public ValuesToGridPoints
   public getValueAtLocation
   public freeLocationData
   
   !> Realloc grid values set
   interface realloc
      module procedure reallocspatial_data
   end interface

   !> Dealloc grid values set
   interface dealloc
      module procedure deallocspatial_data
   end interface dealloc

   !> Derived type containg the grid values
   type, public:: t_spatial_data                               !< Derived type for spatial varying data values
      integer                         :: numValues             !< Array size
      logical                         :: interpolate           !< Flag indicates values to the grid points have to be interpolated
      integer                         :: def_type              !< Default type
                                                               
      integer, allocatable            :: brIndex(:)            !< Array containing indices to the branches for each input value (length NUMVALUES)
      double precision, allocatable   :: chainage(:)           !< Array containing chainage on the branch for each input value (length NUMVALUES)
      double precision, allocatable   :: valuesOnLocation(:)   !< Array containing input values (length NUMVALUES)
                                  
      double precision                :: default               !< Default or global value
      integer                         :: quantity              !< Name (integer type) of the quantity
      integer, allocatable            :: tblIndex(:)           !< Indirection table from grid point to table index
      double precision, allocatable   :: values(:)             !< Values of the quantity
      
      type(t_tableSet)                :: tables                !< tables for water level or discharge dependent values
      
   end type    

   !> Derived type containing the grid values set
   type, public   :: t_spatial_dataSet
      integer                                       :: size   = 0           !< current length of array quant
      integer                                       :: growsBy = 2000       !< used increment for extending array quant
      integer                                       :: count   = 0          !< number of registered quantial Conditions
      type(t_spatial_data), pointer, dimension(:)   :: quant   => null()            
      integer                                       :: level      = -1      !< index of level in quant array
      integer                                       :: depth      = -1      !< index of level in quant array
      integer                                       :: discharge  = -1      !< index of discharge in quant array
      integer                                       :: salinity   = -1      !< index of salinity in quant array
      integer                                       :: dispersion = -1      !< index of dispersion in quant array
      integer                                       :: windShield = -1      !< index of wind shield parameter in quant array
      integer                                       :: TH_F1      = -1      !< index of Thatcher Harleman coefficient F1
      integer                                       :: TH_F3      = -1      !< index of Thatcher Harleman coefficient F3
      integer                                       :: TH_F4      = -1      !< index of Thatcher Harleman coefficient F4
      integer                                       :: ConvLength = -1      !< index of convergence length of estuary
   end type
      
   type, public :: t_ptable
      type(t_table), pointer :: p
   end type

contains

   !> Adds an array of values for a quantity to the complete grid
   subroutine AddValues(spData, quantity, values)
      ! Modules               
      use m_alloc
                              
      implicit none
      
      ! Input/output parameters
      type(t_spatial_dataSet) , intent(inout)                  :: spData     !< Grid values set           
      integer                 , intent(in)                     :: quantity   !< Name (integer) of the quantity           
      double precision        , intent(in), dimension(:)       :: values     !< Array containing grid values

      integer                 :: iQuant
      integer                 :: length
      type(t_spatial_data), pointer :: psp

      ! Program code
      
      length = ubound(values,1)
      if (spData%Count == 0) then
         call realloc(spData)
         spData%quant(1)%quantity = CFiWaterLevel
         spData%quant(2)%quantity = CFiDischarge
         spData%quant(3)%quantity = CFiSalinity
         spData%Count = 3
         spData%level      = -1
         spData%discharge  = -1
         spData%salinity   = -1
         spData%dispersion = -1
         spData%windShield = -1
      endif
      
      select case (quantity)
      case (CFiWaterlevel) 
         iQuant = 1
         spData%level      = 1
      case (CFiDischarge)
         iQuant = 2
         spData%discharge  = 2
      case (CFiSalinity)
         iQuant = 3
         spData%salinity   = 3
      case default
         spData%count = spData%count+1
         iQuant = spData%count
         if (spData%Count > spData%Size) then
            call realloc(spData)
         endif
         psp => spData%quant(iQuant)
         spData%quant(iQuant)%quantity = quantity
         select case (quantity)
            case (CFiDispersion)
               spData%dispersion = spData%count
            case (CFiWindShield)
               spData%windShield = spData%count
            case (CFiTH_F1)
               spData%TH_F1 = spData%count
            case (CFiTH_F3)
               spData%TH_F3 = spData%count
            case (CFiTH_F4)
               spData%TH_F4 = spData%count
         end select
      end select
      
      call realloc(spData%quant(iQuant)%values, length)
      spData%quant(iQuant)%values = values
      spData%quant(iQuant)%values = values
   end subroutine AddValues
   
   !> Get index number in the spatial_data set of a quantity
   integer function getQuantityNumber(spData, quantity)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_spatial_dataSet) , intent(inout)          :: spData    !< spatial data set 
      integer                 , intent(in)             :: quantity  !< Integer name of the quantity

      ! Local variables
      integer i
      
      ! Program code
      getQuantityNumber = -1
      do i = 1, spData%count
         if (spData%quant(i)%quantity == quantity ) then
            getQuantityNumber = i
            exit
         endif
      enddo
   end function getQuantityNumber
 
   !> Free the spatial_data set
   subroutine deallocspatial_data(spData)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_spatial_dataSet), intent(inout)          :: spData        !< Spatial data set

      ! Local variables
      integer                       :: i
      integer                       :: length
  
      ! Program code
      spdata%level      = -1
      spdata%depth      = -1
      spdata%discharge  = -1
      spdata%salinity   = -1
      spdata%dispersion = -1
      spdata%windShield = -1
      spdata%TH_F1 = -1     
      spdata%TH_F3 = -1     
      spdata%TH_F4 = -1     
      
      if (associated(spData%quant)) then
         length = size(spData%quant)
         do i = 1, length
            if (allocated(spData%quant(i)%values)) deallocate(spData%quant(i)%values)
            if (allocated(spData%quant(i)%brIndex)) deallocate(spData%quant(i)%brIndex)
            if (allocated(spData%quant(i)%chainage)) deallocate(spData%quant(i)%chainage)
            if (allocated(spData%quant(i)%valuesOnLocation)) deallocate(spData%quant(i)%valuesOnLocation)
            if (allocated(spData%quant(i)%tblIndex)) deallocate(spData%quant(i)%tblIndex)
            call dealloc(spData%quant(i)%tables)
            spData%quant(i)%numvalues = 0
         enddo   
         deallocate(spData%quant)
      endif
      spData%quant => null()
      spData%Size  = 0
      spData%Count = 0
   end subroutine
!
!  !> Realloc spatial_data 
   subroutine reallocspatial_data(spData)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_spatial_dataSet), intent(inout)          :: spData         !< Spatial data set
      
      ! Local variables
      type(t_spatial_data), pointer, dimension(:)     :: oldspData
      
      ! Program code
      
      if (spData%Size > 0) then
         oldspData=>spData%quant
      endif
      
      if (spData%growsBy <=0) then
         spData%growsBy = 200
      endif
      allocate(spData%quant(spData%Size+spData%growsBy))
      
      if (spData%Size > 0) then
         spData%quant(1:spData%Size) = oldspData(1:spData%Size)
         deallocate(oldspData)
      endif
      spData%Size = spData%Size+spData%growsBy
   end subroutine
   
   !> Calculates dispersion using dispersion array or Thatcher-Harleman formulation
   subroutine GetDispersionParameters(spData, igrid, f1, f3, f4)
   use m_GlobalParameters

      implicit none
      !
      ! Global variables
      !
      type(t_spatial_dataSet), intent(in   )          :: spData         !< structure containing grid related values
      integer                , intent(in   )          :: igrid          !< gridnumber
      double precision       , intent(  out)          :: f1             !< Thatcher Harleman F1 term
      double precision       , intent(  out)          :: f3             !< Thatcher Harleman F3 term
      double precision       , intent(  out)          :: f4             !< Thatcher Harleman F4 term
      !
 
      !! executable statements -------------------------------------------------------
      if (spData%TH_F1 > 0) then
         f1 = spData%quant(spData%TH_F1)%values(igrid)
      else
         f1 = 0d0
      endif
      
      if(spData%TH_f3 > 0) then
         f3 = spData%quant(spData%TH_f3)%values(igrid)
      else
         f3 = -huge(1d0)
      endif
      
      if(spData%TH_f4 > 0) then
         f4 = spData%quant(spData%TH_f4)%values(igrid)
      else
         f4 = -huge(1d0)
      endif
      
   end subroutine GetDispersionParameters
   
   !> Interpolate the inputvalues to grid points
   subroutine ValuesToGridPoints(spData, brs, tbls, interpolateOverBranches)
      use m_branch
   
      type(t_spatial_data), intent(inout)          :: spData                   !< Spatial data derived type
      type(t_branchSet), intent(in)                :: brs                      !< Branches
      logical, intent(in)                          :: interpolateOverBranches  !< Flag indicating if interpolation over branches is required
      type(t_ptable), dimension(:), intent(inout)  :: tbls                     !< Tables
      
      integer :: i
      integer :: isp1
      integer :: isp2
      integer :: ibr
      integer :: ngrid
      integer :: iter
      integer :: ipoint
      integer :: pointscount
      logical :: found
      integer :: minindex
      integer :: minbranchindex
      double precision :: minchainage
      double precision :: dhelp
      double precision :: chainage1, chainage2
      double precision :: value1, value2
      double precision :: chainage
      double precision :: f
      integer, allocatable, dimension(:,:)      :: ibr2spDataIndex
      type(t_branch), pointer                   :: pbr
      double precision, pointer, dimension(:)   :: points
      type(t_table), pointer :: phelp
      
      ngrid = 0
      do ibr = 1, brs%count
         ngrid = ngrid + brs%branch(ibr)%gridpointsCount
      enddo
      
      allocate(spData%values(ngrid))
      allocate(spData%tblIndex(ngrid))
      
      allocate(ibr2spDataIndex(brs%count,2))
      ibr2spDataIndex = -1
      
      if (spData%numValues == 0) then
         ! no local values defined, so use default value for complete grid
         do i = 1, ngrid
            spData%values(i) = spData%default
         enddo
         
         !----- finished ----!
         return
      endif
      
      ! sort spData items first branch Index, then chainage

      do iter = 1, spData%numValues
         minindex = iter
         ibr = spData%brIndex(iter)
         minBranchindex = ibr
         minchainage = spData%chainage(iter)
         
         do i = iter +1, spData%numValues
            ibr = spData%brIndex(i)
            if (minBranchIndex > ibr) then
               minBranchindex = ibr
               minchainage = spData%chainage(i)
               minIndex = i
            elseif (minBranchIndex == ibr .and. minchainage > spData%chainage(i)) then
               minchainage = spData%chainage(i)
               minIndex = i
            endif
         enddo
         
         ! brIndex
         ibr = spData%brIndex(minindex)
         spData%brIndex(minindex) = spData%brIndex(iter)
         spData%brIndex(iter) = ibr
         
         ! fill ibr2spDataIndex
         if (ibr2spDataIndex(ibr,1) == -1) then
            ibr2spDataIndex(ibr,1) = iter
            ibr2spDataIndex(ibr,2) = iter
         else
            ibr2spDataIndex(ibr,2) = iter
         endif
         
         ! chainage
         dhelp = spData%chainage(minindex)
         spData%chainage(minindex) = spData%chainage(iter)
         spData%chainage(iter) = dhelp

         ! valuesOnLocation
         dhelp = spData%valuesOnLocation(minindex)
         spData%valuesOnLocation(minindex) = spData%valuesOnLocation(iter)
         spData%valuesOnLocation(iter) = dhelp
         
         ! tbls
         phelp => tbls(minindex)%p
         tbls(minindex)%p => tbls(iter)%p
         tbls(iter)%p => phelp
      enddo

      ! Now interpolate spatial data to grid
      ipoint = 0
      do ibr = 1, brs%count
         ! isp1 is connected to first data item on branch ibr
         isp1 = ibr2spDataIndex(ibr, 1)
         isp2 = isp1
         if (isp1 < 1) then
            value1 = spData%default
            value2 = value1
            chainage1 = 0d0
         else
            value1 = spData%valuesOnLocation(isp1)
            chainage1 = 0d0
            chainage2 = spData%chainage(isp1)
            value2 = value1
         endif
         
         ! When interpolating over branches isp1 might be located on another branch
         ! isp2 == isp1
         pbr => brs%branch(ibr)
         found = findNeighbourValue(brs, ibr, .true., spData, ibr2spDataIndex, value1, isp1, chainage, interpolateOverBranches)
         chainage1 = - chainage
         
         if (.not. found .and. isp1 < 0) then
            ! no actual value found on branch or from begin node, search at end of branch
            found = findNeighbourValue(brs, ibr, .false., spData, ibr2spDataIndex, value2, isp2, chainage, interpolateOverBranches)
            chainage2 = pbr%length + chainage
            if (found) then
               isp1 = isp2
               value1 = value2
            endif
         endif
         
         ! Now loop over all gridpoints on the branch
         pointsCount = brs%branch(ibr)%gridPointsCount
         points => brs%branch(ibr)%gridPointschainages
         do i = 1, pointsCount
            
            ! When points(i) > Chainage2 -> point(i) is not between isp1 and isp2
            ! So look for the combination of data items chainage1 < points(i) < chainage2
            do while (points(i) > chainage2)
               isp1 = isp2
               chainage1 = chainage2
               value1    = value2
               if (isp2 >= ibr2spDataIndex(ibr,1) .and. isp2 < ibr2spDataIndex(ibr,2)) then
                  isp2 = isp2+1
                  chainage2 = spData%chainage(isp2)
                  value2 = spData%valuesOnLocation(isp2)
               else
                  found = findNeighbourValue(brs, ibr, .false., spData, ibr2spDataIndex, value2, isp2, chainage, interpolateOverBranches)
                  chainage2 = pbr%length + chainage
                  if (points(i) > chainage2) then
                     ! truncation error
                     chainage2 = points(i)
                  endif
               endif
            enddo
            
            ipoint = ipoint+1
            if (abs(chainage2-chainage1) < 1d-6) then
                f = 1d0
            elseif (spData%interpolate) then
               f = (points(i) - chainage1)/(chainage2 - chainage1)
            elseif ((points(i) - chainage1)/(chainage2 - chainage1) ==1d0) then
               f = 1d0
            else
               f = 0d0
            endif
            
            if (isp1 > 0 .and. isp2 > 0 .and. associated(tbls(max(1,isp1))%p)) then
               spData%tblIndex(ipoint) = CombineTables(spData%tables, tbls(isp1)%p, tbls(isp2)%p, 1d0-f)
            else
               spData%values(ipoint) = (1d0-f) * value1 + f * value2
            endif
         enddo
      enddo
      
      deallocate(ibr2spDataIndex)
      
   end subroutine ValuesToGridPoints

   !> Find value definition on other branch, using branch orders
   recursive function findNeighbourValue(brs, ibr, beginNode, spData, ibr2spDataIndex, value, isp, chainage, interpolateOverBranches) result(found)
      ! modules
      use m_branch
   
      implicit none
   
      ! variables
      logical :: found
      
      type(t_branchSet)       , intent(in   ) :: brs                       !< Set of reaches
      integer                 , intent(in   ) :: ibr                       !< Branch index
      logical                 , intent(in   ) :: interpolateOverBranches   !< Flag indicating if interpolation over branches is required
      type(t_spatial_data)    , intent(in   ) :: spData                    !< Grid value object  
      integer, dimension (:,:), intent(in   ) :: ibr2spDataIndex           !< Branch to spatial data index table
      double precision        , intent(  out) :: chainage                  !< Distance from begin or end of branch
      double precision        , intent(inout) :: value                     !< Location of roughness section on branch
      logical                 , intent(in   ) :: beginNode                 !< Indicates whether the begin or end node is to be used of the branch
      integer                 , intent(inout) :: isp
   
      ! local variables
      integer                          :: nodeIndex
      integer                          :: i
      type(t_branch), pointer          :: pbr
   
      !program code
   
      pbr => brs%branch(ibr)
      
      found = .false.
      
      !set values, for the case no value is found
      
      if (pbr%orderNumber <= 0 .or. .not. interpolateOverBranches) then
         chainage = 0d0
         return
      endif
      
      if (beginNode) then
         nodeIndex = pbr%FromNode%index
      else
         nodeIndex = pbr%ToNode%index
      endif
      
      do i = 1, brs%count
         if ( (pbr%orderNumber /= brs%branch(i)%orderNumber) .or. (ibr == i) ) then
            cycle
         endif
         
         ! branch(i) has same ordernumber as input branch and is not identical. Look for
         ! same begin or end node
         
         if (nodeIndex == brs%branch(i)%FromNode%index) then
            ! branch(i) is directly connected to pbr
            if (ibr2spDataIndex(i,1) > 0) then
               ! 
               isp = ibr2spDataIndex(i,1)
               chainage = spData%chainage(isp)
               value  = spData%valuesOnLocation(isp)
               found = .true.
            else
               ! no spatial data item on branch, look for next neighbouring branch
               found = findNeighbourValue(brs, i, .false., spData, ibr2spDataIndex, value, isp, chainage, interpolateOverBranches)
               if (found) then
                  chainage = chainage + brs%branch(i)%length   
               endif
            endif
         endif
         
         if (nodeIndex == brs%branch(i)%ToNode%index) then
            ! branch(i) is directly connected to pbr
            if (ibr2spDataIndex(i,1) > 0) then
               ! 
               isp = ibr2spDataIndex(i,2)
               chainage = brs%branch(i)%length - spData%chainage(isp)
               value  = spData%valuesOnLocation(isp)
               found = .true.
            else
               ! no spatial data item on branch, look for next neighbouring branch
               found = findNeighbourValue(brs, i, .true., spData, ibr2spDataIndex, value, isp, chainage, interpolateOverBranches)
               if (found) then
                  chainage = chainage + brs%branch(i)%length
               endif
            endif
         endif
            
      enddo
         
      if (.not. found) then
         chainage = 0d0
      endif

   end function findNeighbourValue

   !> Get the function value at a given location (branchid, chainage)
   integer function getValueAtLocation(sp, branchidx, chainage, value, valuetype)
   
      ! Return Values: 0 = Value found at Location
      !                1 = Default Value
      !               -1 = Error, No Value determined
   
      type(t_spatial_data), intent(in)            :: sp         !< spatial data 
      integer, intent(in)                         :: branchidx  !< branch index
      double precision, intent(in)                :: chainage   !< chainage
      double precision, intent(out)               :: value      !< value
      integer, intent(out)                        :: valuetype  !< valuetype
      
      integer                 :: i
      integer                 :: icount
      double precision        :: chainages(2) = 0.0d0
      double precision        :: values(2)   = 0.0d0
      
      getValueAtLocation = -1
      value              = 0.0d0
      valuetype          = 0
      
      icount = 0
      do i = 1, sp%numvalues
         
         if (sp%brIndex(i) == branchidx) then
         
            if (icount == 0) then
               icount = 1
               chainages = sp%chainage(i)
               values   = sp%valuesOnLocation(i)
               if (chainages(1) >= chainage) exit
            else
               icount = icount + 1
               chainages(1) = chainages(2)
               values  (1) = values(2)
               chainages(2) = sp%chainage(i)
               values(2)   = sp%valuesOnLocation(i)
               if (chainages(2) >= chainage) exit
            endif   

         else
            if (icount > 0 ) exit
         endif
      
      enddo
      
      if (icount == 0) then
         
         ! No Data at Location found, use Default and Default Type
         value = sp%default
         valuetype = sp%def_type
         getValueAtLocation = 1
         
      elseif (icount == 1) then
      
         ! Only one Value Found on Branch, no interpolation needed
         value = values(1)
         getValueAtLocation = 0
      
      elseif (chainages(1) >= chainage) then
      
         ! chainage before first point
         value = values(1)
         getValueAtLocation = 0
      
      elseif (chainages(2) <= chainage) then
      
         ! chainage after last point
         value = values(2)
         getValueAtLocation = 0
      
      else
      
         ! Now we need interpolation
         if (chainages(2) > chainages(1)) then
            value = values(1) + (values(2) - values(1)) * (chainage - chainages(1)) / (chainages(2) - chainages(1))
         else
            ! Prevent zero devide
            value = (values(1) + values(2)) * 0.50d0
         endif
         
         getValueAtLocation = 0
         
      endif
      
   
   end function getValueAtLocation
   
   !> Free Location Data which is not used anymore \n
   !! After this the function getValueAtLocation will only give default data
   subroutine freeLocationData(spdSet)
   
   
      type(t_spatial_dataSet), intent(inout)           :: spdSet     !< spatial data set
      
      type(t_spatial_data), pointer                    :: pspData
      integer                                          :: i
   
      
      do i = 1, spdSet%count
      
         pspData => spdSet%quant(i)
         
         if (allocated(pspData%brIndex))          deallocate(pspData%brIndex)
         if (allocated(pspData%chainage))         deallocate(pspData%chainage)
         if (allocated(pspData%valuesOnLocation)) deallocate(pspData%valuesOnLocation)
         
         pspData%numValues = 0
      
      enddo
   
   end subroutine freeLocationData

   
end module m_spatial_data
