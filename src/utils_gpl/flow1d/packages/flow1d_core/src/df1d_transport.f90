module m_df1d_transport
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
!  $Id: df1d_transport.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/df1d_transport.f90 $
!-------------------------------------------------------------------------------

   use messagehandling

   implicit none
   
   private
   
   public dealloc
   public realloc
   public admin_transport
   
   interface dealloc
      module procedure deallocTrans
   end interface dealloc
   
   interface realloc
      module procedure reallocTrans
   end interface realloc
   
   
    type, public :: t_mouthparameters
       character(len=idlen)            :: nodeid
       double precision                :: Le                !< Estuary length
       integer                         :: gridPoint         !< Location of gridpoint of mouth
       integer                         :: branchIndex       !< branch index connected to mouth
       integer                         :: dis_loc           !< Location of discharge point
       double precision                :: dis_dir           !< direction of interface dis_dir*Q(dis_loc) > 0 means flow is directed into the model
       double precision, dimension(3)  :: Qf                !< Fresh water discharge
       double precision, dimension(3)  :: Pe                !< Flood volume entering the estuary through the mouth
       double precision, dimension(3)  :: u_max                !< Maximum flood velocity at mouth
       double precision, dimension(3)  :: au                !< average flow area at mouth
       double precision, dimension(3)  :: tp                !< tidal period
       double precision, dimension(3)  :: h                 !< average waterdepth in branch
       double precision                :: Edp                !< Estuarine Richardson number ^ transportPars%power
       
    end type t_mouthparameters
 
    type, public :: t_transportSet
       integer    :: mouthSize = 0
       integer    :: constSize = 0
       integer    :: growsBy   = 200
       integer    :: mouthCount= 0
       integer    :: constCount= 0
       type(t_mouthParameters), pointer, dimension(:,:) :: mp
       type(t_constituent), pointer, dimension(:)     :: co
    end type t_transportSet
    
    type, public :: t_constituent
       double precision, allocatable, dimension(:)        :: c0
       double precision, allocatable, dimension(:)        :: c1
       double precision, allocatable, dimension(:)        :: load
       double precision, allocatable, dimension(:)        :: qload
       double precision, allocatable, dimension(:,:)      :: sbdscr
    end type t_constituent
    
   contains
   
   subroutine deallocTrans(trans)
      type(t_transportSet), intent(inout) :: trans
      
      integer :: i
      
      if (associated(trans%mp)) then
         deallocate(trans%mp)
         trans%mp => null()
         trans%mouthSize  = 0
         trans%growsBy    = 200
         trans%mouthCount = 0
      endif
      
      do i = 1, trans%constCount
         if (allocated(trans%co(i)%c0))     deallocate(trans%co(i)%c0)
         if (allocated(trans%co(i)%c1))     deallocate(trans%co(i)%c1)
         if (allocated(trans%co(i)%load))   deallocate(trans%co(i)%load)
         if (allocated(trans%co(i)%qload))  deallocate(trans%co(i)%qload)
         if (allocated(trans%co(i)%sbdscr)) deallocate(trans%co(i)%sbdscr)
      enddo
      
      if (associated(trans%co)) deallocate(trans%co)
      trans%co => null()
      
      trans%constSize = 0
      trans%constCount = 0
      
   end subroutine deallocTrans

   subroutine reallocTrans(trans)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_transportSet), intent(inout) :: trans
      
      ! Local variables
      type(t_constituent), pointer, dimension(:)     :: oldco
      
      ! Program code
      
      if (trans%constSize > 0) then
         oldco=>trans%co
      endif
      
      if (trans%growsBy <=0) then
         trans%growsBy = 200
      endif
      allocate(trans%co(trans%constSize+trans%growsBy))
      
      if (trans%constSize > 0) then
         trans%co(1:trans%constSize) = oldco(1:trans%constSize)
         deallocate(oldco)
      endif
      trans%constSize = trans%constSize+trans%growsBy
   end subroutine reallocTrans

   
   subroutine admin_transport(trans, brs)
      use m_branch
      use m_GlobalParameters
      
      type(t_transportSet), intent(inout)  :: trans
      type(t_branchSet), intent(in)        :: brs
      
      type(t_mouthparameters), pointer :: pmp
      type(t_branch), pointer          :: pbr
      
      integer ibr, nbr
      
      if (.not. transportPars%use_f4_dispersion) then
         ! no branch mouth relations
         return
      endif
      
      nbr = brs%count
         
      do ibr = 1, nbr
         pbr => brs%branch(ibr)
         
         pmp => trans%mp(1,ibr)
         pmp%nodeid        = pbr%FromNode%id
         pmp%gridPoint     = pbr%FromNode%gridNumber     
         pmp%branchIndex   = ibr       
         pmp%dis_loc       = pbr%lin(1)   
         pmp%dis_dir       = 1    

         pmp => trans%mp(2,ibr)
         pmp%nodeid        = pbr%ToNode%id
         pmp%gridPoint     = pbr%ToNode%gridNumber     
         pmp%branchIndex   = ibr       
         pmp%dis_loc       = pbr%lin(pbr%uPointsCount)   
         pmp%dis_dir       = -1    

         if (transportpars%mouth%index == pbr%FromNode%index) then
            transportPars%dis_loc = pbr%lin(1) 
            transportPars%dis_dir = 1    
         elseif (transportpars%mouth%index == pbr%ToNode%index) then
            transportPars%dis_loc = pbr%lin(pbr%uPointsCount) 
            transportPars%dis_dir = -1
         endif
      enddo
      
   end subroutine admin_transport
   
end module m_df1d_transport   
