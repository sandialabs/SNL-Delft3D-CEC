module m_ObservationPoints
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
!  $Id: ObservationPoints.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/ObservationPoints.f90 $
!-------------------------------------------------------------------------------

   use MessageHandling
   use m_alloc
   use m_branch
   use m_hash_search
   
   implicit none
   
   private 
     
   public realloc
   public GetObservationPointsCount
   public dealloc

   public getObservationPointById
   public GetObservationPointValue
   public fill_hashtable
   
   interface fill_hashtable
      module procedure fill_hashtable_obs
   end interface 

   interface realloc
      module procedure reallocObservationPoint
   end interface

   interface dealloc
      module procedure deallocObservationPoint
   end interface dealloc

    !---------------------------------------------------------
   type, public :: t_ObservationPoint                       !< Observation point information
    
        character(IdLen)            :: id                   !< id of OPnt
        character(IdLen)            :: name                 !< name of OPnt
        integer                     :: p1                   !< first point number
        integer                     :: p2                   !< second point number
        !> weight for determining function value. F(observation) = (weight -1)*f(p1) + weight*f(p2)
        double precision            :: pointWeight          
        integer                     :: l1                   !< first segment number
        integer                     :: l2                   !< second segment number
        !> weight for determining function value. F(observation) = (weight -1)*f(l1) + weight*f(l2)
        double precision            :: linkWeight
        integer                     :: branchIdx            !< index of branch on which the observation point is located
        type(t_branch), pointer     :: branch               !< pointer to branch on which the observation point is located
        double precision            :: chainage             !< chainage of observation point on branch
        !> optional 
        double precision            :: x                    !< x-coordinate
        double precision            :: y                    !< y-coordinate
        integer                     :: locationtype = 0     !< location type, one of INDTP_1D/2D/ALL :=1 (or 2) snap to 1d (or 2d) flownodes
   end type
   
   type, public :: t_ObservationPointSet
      integer                                               :: maxNumberOfConnections=0    ! maximum nr of connections to a OPnt
      integer                                               :: Size = 0
      integer                                               :: growsBy = 2000
      integer                                               :: Count= 0
      type(t_ObservationPoint), pointer, dimension(:)       :: OPnt
     
      integer                                               :: interpolationType
      type(t_hashlist)                                      :: hashlist
   end type t_ObservationPointSet
   
   integer, parameter, public                               :: OBS_NEAREST = 1
   integer, parameter, public                               :: OBS_LINEAR = 2
   
contains
    
   integer function GetObservationPointsCount(obs)
      type(t_ObservationPointSet)                  :: obs

      GetObservationPointsCount = obs%count
   end function GetObservationPointsCount
   
   subroutine deallocObservationPoint(obs)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_ObservationPointSet), intent(inout)          :: obs
      
      ! Local variables
   
      ! Program code
      if (associated(obs%OPnt)) deallocate(obs%OPnt)
      obs%Size  = 0
      obs%Count = 0
      call dealloc(obs%hashlist)
   end subroutine
!
!
   subroutine reallocObservationPoint(obs)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_ObservationPointSet), intent(inout)          :: obs
      
      ! Local variables
      type(t_ObservationPoint), pointer, dimension(:)     :: oldobs
      
      ! Program code
      
      if (obs%Size > 0) then
         oldobs=>obs%OPnt
      endif
      
      if (obs%growsBy <=0) then
         obs%growsBy = 200
      endif
      allocate(obs%OPnt(obs%Size+obs%growsBy))
      
      if (obs%Size > 0) then
         obs%OPnt(1:obs%Size) = oldobs(1:obs%Size)
         deallocate(oldobs)
      endif
      obs%Size = obs%Size+obs%growsBy
   end subroutine
   
   integer function getObservationPointById(obs, OPnt1)
      type(t_ObservationPointSet), intent(in)   :: obs
      character(IdLen)                          :: OPnt1
      
      integer i
      
      do i = 1, obs%count
         if (trim(OPnt1) == trim(obs%OPnt(i)%id)) then
            getObservationPointById = i
            return
         endif
      enddo
      ! not found
      getObservationPointById = -1
   end function getObservationPointById

   double precision function GetObservationPointValue(pObs, values, mapIndex, onGridPoint)
   
      type(t_ObservationPoint) :: pObs
      double precision, dimension (0:)  :: values
      integer, dimension(:) :: mapIndex
      logical onGridPoint
      
      integer :: p1
      integer :: p2
      double precision :: weight
      
      if (onGridPoint) then
         p1 = pObs%p1
         p2 = pObs%p2
         weight = pObs%pointWeight
      else
         p1 = pObs%l1
         p2 = pObs%l2
         weight = pObs%linkWeight
      endif
      GetObservationPointValue = weight*values(mapIndex(p1)) + (1.0-weight)*values(mapIndex(p2))

   end function GetObservationPointValue

   subroutine fill_hashtable_obs(obs)
   
      type (t_ObservationPointSet), intent(inout), target :: obs
      
      integer                                      :: iob
      character(len=idlen), dimension(:), pointer  :: ido
      
      allocate(obs%hashlist%id_list(obs%Count))
      obs%hashlist%id_count = obs%Count
      ido => obs%hashlist%id_list
      
      do iob = 1, obs%count
         ido(iob) = obs%OPnt(iob)%id
      enddo
      
      call hashfill(obs%hashlist)
      
   end subroutine fill_hashtable_obs

end module m_ObservationPoints