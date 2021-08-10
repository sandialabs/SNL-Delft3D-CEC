!> Module containing data definition of branches in Delft_model_data
module m_branch
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
!  $Id: branches.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/branches.f90 $
!-------------------------------------------------------------------------------

   use MessageHandling
   use m_alloc
   use m_node
   use m_hash_search
   
   implicit none
   
   private

   public realloc
   public dealloc
   public fill_hashtable
   public getLinkIndex
   public getGridPointNumber
   public admin_branch
   
   interface fill_hashtable
      module procedure fill_hashtable_brs
   end interface 

   interface realloc
      module procedure reallocbranch
   end interface

   interface dealloc
      module procedure deallocbranch
   end interface dealloc

   !> branch information of network
   type, public :: t_branch
      character(IdLen)               :: id                      !< unique identification
      integer                        :: index                   !< sequence number
      character(IdLen)               :: name                    !< Name of the branch
      double precision               :: length                  !< length of branch
      integer                        :: orderNumber             !< order number to interpolate cross sections over branches

      integer                        :: brType                  !< channel type of 1D channnel
      integer                        :: iTrench                 !< Trench Index, 0 = No Trench
      
      integer                        :: flapGate = 0            !< 0 = None, 1 = Only Positive Flow, 2 = Only Negative Flow
                                                                !< Not implemeted in Readers yet

      integer                        :: nextBranch(2)           !< neighbouring branch with same ordernumber at start or end of branch
      integer                        :: nodeIndex(2)            !< indexes of Begin Node and End Node
      type(t_node), pointer          :: FromNode => null()      !< node at start of branch
      type(t_node), pointer          :: ToNode => null()        !< node at end of branch


      integer                        :: gridPointsCount         !< number of grid points on branch
      double precision, allocatable  :: gridPointsChainages(:)  !< chainage of grid points on branch
      character(IdLen), allocatable  :: gridPointIDs(:)         !< ID's of grid points on branch
      double precision, allocatable  :: Xs(:)                   !< X-coordinates of grid points
      double precision, allocatable  :: Ys(:)                   !< Y-coordinates of grid points

      integer                        :: uPointsCount            !< number of u points on branch (gridpointsCount -1)
      double precision, allocatable  :: uPointsChainages(:)     !< chainage of velocity points on branch (each upoint 
      double precision, allocatable  :: Xu(:)                   !< X-coordinates of u points
      double precision, allocatable  :: Yu(:)                   !< Y-coordinates of u points
      double precision, allocatable  :: dx(:)                   !< distance between two gridpoints  

      integer                        :: Points(2)               !< Calculation Points at Start and End of Branch
      integer                        :: uPoints(2)              !< Velocity Points at Start and End of Branch

      integer, allocatable           :: lin(:)                  !< link numbers for links in this channel
      integer, allocatable           :: grd(:)                  !< gridpoint numbers for links in this channel
   
   end type t_branch

   !> Set of branches in network
   type, public :: t_branchSet
      integer                                               :: Size=0                !< current length of array branch
      integer                                               :: growsBy= 2000         !< used increment for extending array branch
      integer                                               :: Count=0               !< number of registered branches
      integer                                               :: gridpointsCount = 0   !< nr of grid points in network (total over branches) 
      type(t_branch), pointer, dimension(:)                 :: branch => null()      !< array containing branch information
      type(t_hashlist)                                      :: hashlist
   end type t_branchSet

   integer, parameter    :: BR_BOUNDARY  = -1 
   integer, parameter    :: BR_EMBEDDED  = 0
   integer, parameter    :: BR_ISOLATED  = 1
   integer, parameter    :: BR_CONNECTED = 2
   
   contains
   
   !> Deallocate given branch
   subroutine deallocbranch(brs)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_branchSet), intent(inout) :: brs !< Current branch set

      ! Local variables
      integer                       :: i
      integer                       :: length
  
      ! Program code
      if (associated(brs%branch)) then
         length = size(brs%branch)
         do i = 1, length
            if (allocated(brs%branch(i)%gridPointschainages)) deallocate(brs%branch(i)%gridPointschainages)
            if (allocated(brs%branch(i)%gridPointIDs))      deallocate(brs%branch(i)%gridPointIDs)
            if (allocated(brs%branch(i)%uPointschainages))    deallocate(brs%branch(i)%uPointschainages)
            if (allocated(brs%branch(i)%dx))                deallocate(brs%branch(i)%dx)
            if (allocated(brs%branch(i)%xs))                deallocate(brs%branch(i)%xs)
            if (allocated(brs%branch(i)%ys))                deallocate(brs%branch(i)%ys)
            if (allocated(brs%branch(i)%xu))                deallocate(brs%branch(i)%xu)
            if (allocated(brs%branch(i)%yu))                deallocate(brs%branch(i)%yu)
            if (allocated(brs%branch(i)%lin))               deallocate(brs%branch(i)%lin)
            if (allocated(brs%branch(i)%grd))               deallocate(brs%branch(i)%grd)
         enddo   
         deallocate(brs%branch)
      endif
      call dealloc(brs%hashlist)
      
      brs%branch => null()
      brs%Size  = 0
      brs%Count = 0
      brs%gridpointsCount = 0    ! nr of points
   end subroutine
!
   !> Reallocate given branch
   subroutine reallocbranch(brs)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_branchSet), intent(inout)          :: brs !< Current branch set
      
      ! Local variables
      type(t_branch), pointer, dimension(:)     :: oldBrs
      
      ! Program code
      
      if (brs%Size > 0) then
         oldBrs=>brs%branch
      else
         brs%gridpointsCount = 0
      endif
      
      if (brs%growsBy <=0) then
         brs%growsBy = 200
      endif
      allocate(brs%branch(brs%Size+brs%growsBy))
      
      if (brs%Size > 0) then
         brs%branch(1:brs%Size) = oldBrs(1:brs%Size)
         deallocate(oldBrs)
      endif
      brs%Size = brs%Size+brs%growsBy
   end subroutine
   
   !> this function returns the link index based on (branch/chainage), to be used in FM, where subarray
   !! LIN is filled correctly
   integer function getLinkIndex(branch, chainage) 

       type(t_branch)                  :: branch
       double precision, intent(in)    :: chainage  !< Chainage

       integer                         :: i
       
       do i = 2, branch%gridPointsCount
           if (branch%gridPointschainages(i) >= chainage) then !found
              getLinkIndex = branch%lin(i-1)
              exit
           endif
       enddo
       if (branch%gridPointschainages(branch%gridPointsCount) < chainage) then
          getLinkIndex = branch%lin(branch%gridPointsCount-1)
       endif
       
   end function getLinkIndex
   
   integer function getLinkNumber(brs, ibranch, dist)
       type(t_branchSet)               :: brs       !< Current branche set
       integer, intent(in)             :: ibranch   !< Current branch
       double precision, intent(inout) :: dist      !< Distance along current branch

       integer                         :: i
       double precision                :: dist_in_b
       type(t_branch), pointer         :: pbran
       
       pbran => brs%branch(ibranch)
       
       dist_in_b= 0.0
       dist = max(0.2, min(dist, pbran%length-0.2)) !< JanM: Waarom is de afstand afgeknot en bestaat er een minSectionLength
       do i = 2, pbran%gridPointsCount
           if (pbran%gridPointschainages(i) > dist) then !found
              getLinkNumber = pbran%lin(i-1)
              return
           endif
       enddo
       
       getlinknumber = -1
   end function getLinkNumber
   
   integer function getGridPointNumber(branch, chainage)
      type (t_branch)   , intent(in   ) :: branch              !< branch object
      double precision  , intent(in   ) :: chainage            !< chainage of object on branch
      
      integer :: i
      
      do i = 1, branch%uPointsCount
          if (branch%uPointschainages(i) > chainage) then !found
              getGridPointNumber     = branch%grd(i)
              return
          endif
      enddo
      ! return end point of branch
      getGridPointNumber = branch%grd(branch%gridpointscount)
      
   end function getGridPointNumber
  
   subroutine fill_hashtable_brs(brs)
   
      type (t_branchSet), intent(inout), target :: brs
      
      integer ibr
      character(len=idlen), dimension(:), pointer :: ids
      
      allocate(brs%hashlist%id_list(brs%Count))
      brs%hashlist%id_count = brs%Count
      ids => brs%hashlist%id_list
      
      do ibr= 1, brs%count
         ids(ibr) = brs%branch(ibr)%id
      enddo
      
      call hashfill(brs%hashlist)
   end subroutine fill_hashtable_brs

   !> Sets up the administration for all branches:
   !! * from/to topology and %grd and %lin discretization points.
   subroutine admin_branch(brs, nlink)
   
      type (t_branchSet), target, intent(inout) :: brs   !< Branch set from the network.
      integer,                    intent(inout) :: nlink !< Total number of links. (Upon input, any existing links from the call site.
                                                         !< Upon output: total number of links after administering all branches.)
      
      integer ibr, i, ngrid
      type(t_branch), pointer :: pbr
      
      do ibr= 1, brs%count
         pbr => brs%branch(ibr)
         if (allocated(pbr%lin)) deallocate(pbr%lin) 
         allocate(pbr%lin(pbr%uPointsCount))
         do i = 1, pbr%uPointsCount
            nlink = nlink + 1
            pbr%lin(i) = nlink
         enddo
         
         if (allocated(pbr%grd)) deallocate(pbr%grd) 
         allocate(pbr%grd(pbr%gridPointsCount))
         ngrid = pbr%Points(1) - 1
         if (pbr%FromNode%gridNumber == -1) then
            pbr%FromNode%gridNumber = ngrid + 1
         endif
         do i = 1, pbr%gridPointsCount
            ngrid = ngrid + 1
            pbr%grd(i) = ngrid
         enddo
         if (pbr%ToNode%gridNumber == -1) then
            pbr%ToNode%gridNumber = ngrid
         endif
      enddo
      
   end subroutine admin_branch
   
end module m_branch
