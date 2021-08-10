module m_trenches
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
!  $Id: Trenches.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/Trenches.f90 $
!-------------------------------------------------------------------------------

   use m_alloc
   use m_tables
   use m_branch
   
   implicit none
   
   private 
    
   public realloc
   public dealloc
   public addTrench
   public getfPipe2Trench
   public getfTrench2GroundWater

   interface realloc
     module procedure realloctrench
   end interface

   interface dealloc
      module procedure dealloctrench
   end interface dealloc


  type, public :: t_trench
    integer                :: branchId            !< Branch Number                                                      (= trench(1,:))
    integer                :: crossIndx           !< Index of Cross section id                                          (= trench(2,:)) di
    double precision       :: distInvert          !< Distance of invert level                                           (= trench(3,:)) dl
    double precision       :: trenchHeight        !< Height of the Trench                                               (= trench(4,:)) dh
    double precision       :: trenchWidth         !< Width of the Trench at Top
    double precision       :: trenchBedWidth      !< Width of the Trench at Bottom
    double precision       :: trenchArea          !< Total Area of the Trench at Top
    double precision       :: resistancePipe      !< Restance factor pipe-trench                                        (= trench(5,:)) rt
    double precision       :: porosity            !< Porosity trench                                                    (= trench(6,:)) pt
                                                  !> Option of initial water level trench (oi = 0,1 or 2)               (= trench(7,:)) oi  \n
                                                  !! 0 = constant water level \n
                                                  !! 1 = same as water level pipe \n
                                                  !! 2 = same as ground water level \n 
    integer                :: waterLevelOption      
    double precision       :: waterLevelTrench    !< Constant value initial water level trench (il; used if oi = 0)     (= trench(8,:))  il
    integer                :: permeability        !< Permeability of ground/soil (pm = 0 :impermeable or 1: permeable)  (= trench(9,:))  pm
    double precision       :: resistanceTrench    !< Resistance factor trench-ground (rg; used if permeability = 1)     (= trench(10,:)) rg
    type(t_table), pointer :: groundWaterLevel    !< Table containing groundwater level
                                                  !> Type of Pipe
                                                  !! 1 = Rectangular
                                                  !! 2 = Circle
                                                  !! 3 = EggShaped
    integer                :: pipeType
    double precision       :: bob_pipe            !< (Average) Inside level bottom of pipe (positive down)
                                                  !> Type of Trench
                                                  !! 0 = Trapezoidal
                                                  !! 1 = Rectangular
    logical                :: useGroundLayer      !< Flag for Ground Layer
    double precision       :: groundLayer         !< Ground Layer
    integer                :: trenchType
    double precision       :: bob_trench          !< Inside level bottom of trench (positive down)
    double precision       :: trenchTop          !< Inside level bottom of trench (positive down)
    double precision       :: maxVolTrench        !< Maximum water volume in trench used for limiting the lateral discharge in order to reduce numerical instabilities
    double precision       :: minVolTrench        !< Minimum water volume in trench used As Threshold
    logical                :: no_boundary_beg     !< True if start of pipe is no boundary
    logical                :: no_boundary_end     !< True if end of pipe is no boundary
    
    integer                :: index_lowest        !< Index of Lowest Point, 0 = Pipe is Horizontal
    
    ! calculation results:
    double precision       :: qiTrench            !< Flow trench - pipe  ; < 0.0: Pipe Out: > 0.0: Pipe In
    double precision       :: qiGroundw           !< Flow trench - ground; < 0.0: Trench In: > 0.0: Trench Out
    double precision       :: s1                  !< Water level in trench
    double precision       :: gwl                 !< Actual groundwater level
    double precision       :: volume              !< Water volume in trench

  end type t_trench

  type, public :: t_trenchSet
      integer                                               :: Size = 0
      integer                                               :: growsBy = 2000
      integer                                               :: Count= 0
     type(t_trench), pointer, dimension(:)                  :: trench

     double precision                                       :: accuracy = 1d-8      !< accuracy parameter for determining waterlevels in iteration loop
  end type t_trenchSet

  integer, public, parameter              :: TR_ConstantLevel = 0
  integer, public, parameter              :: TR_LevelPipe = 1
  integer, public, parameter              :: TR_GroundwaterLevel = 2
  double precision, parameter             :: TR_eps = 1d-10
  
  integer, public                         :: TR_DimensionErrorCount = 0
contains

   subroutine realloctrench(trs)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_trenchSet), intent(inout)        :: trs
      
      ! Local variables
      type(t_trench), pointer, dimension(:)   :: oldtrs
      
      ! Program code
      
      if (trs%Size > 0) then
         oldtrs=>trs%trench
      endif
      
      if (trs%growsBy <=0) then
         trs%growsBy = 200
      endif
      allocate(trs%trench(trs%Size+trs%growsBy))
      
      if (trs%Size > 0) then
         trs%trench(1:trs%Size) = oldtrs(1:trs%Size)
         deallocate(oldtrs)
      endif
      trs%Size = trs%Size+trs%growsBy
   end subroutine

   subroutine dealloctrench(trs)
      ! Modules
      
      implicit none
      
      integer                                   :: i
      
      ! Input/output parameters
      type(t_trenchSet), intent(inout)          :: trs
      
      ! Local variables
   
      ! Program code
      if (associated(trs%trench)) then
      
         do i = 1, trs%size
         
            if (associated(trs%trench(i)%groundWaterLevel)) then
               deallocate(trs%trench(i)%groundWaterLevel)
               trs%trench(i)%groundWaterLevel => null()
            endif
            
         enddo
         deallocate(trs%trench)
         
      endif
      trs%trench => null()
      trs%Size    = 0
      trs%Count   = 0
   end subroutine
   
   integer function addTrench(trs, rptrench, interpolType, x, y, length, brs)
      ! Modules               
                              
      implicit none
      
      ! Input/output parameters
      type(t_trenchSet)                      :: trs
      type(t_branchSet)                      :: brs
      double precision, dimension(10)        :: rptrench
      integer                                :: interpoltype
      integer                                :: length
      double precision, dimension(length)    :: x, y
      
      ! Local variables
      integer                    :: index
      
      ! Program code

      trs%Count = trs%Count+1
      index = trs%Count 
      if (trs%Count > trs%Size) then
         call realloc(trs)
      endif

      trs%trench(index)%branchId         = nint(rptrench(1))
      trs%trench(index)%crossIndx        = nint(rptrench(2))
      trs%trench(index)%distInvert       = rptrench(3)
      trs%trench(index)%trenchHeight     = rptrench(4)
      ! resistance is given in [hours] this must be converted to seconds
      trs%trench(index)%resistancePipe   = rptrench(5) * 3600.0
      trs%trench(index)%porosity         = rptrench(6)
      trs%trench(index)%waterLevelOption = nint(rptrench(7))
      trs%trench(index)%waterLevelTrench = rptrench(8)
      trs%trench(index)%permeability     = nint(rptrench(9))
      trs%trench(index)%resistanceTrench = rptrench(10) * 3600.0
      nullify(trs%trench(index)%groundWaterLevel)
      call setTable(trs%trench(index)%groundWaterLevel, interpoltype, x, y, length)   

      trs%trench(index)%no_boundary_beg = .true.
      trs%trench(index)%no_boundary_end = .true.
      
      trs%trench(index)%useGroundLayer = .false.
      trs%trench(index)%groundLayer    = -1.0d0

      trs%trench(index)%index_lowest   = 0
      
      trs%trench(index)%qiTrench     = 0.0d0
      trs%trench(index)%qiGroundw    = 0.0d0
      trs%trench(index)%volume       = 0.0d0
      trs%trench(index)%bob_pipe     = 0.0d0
      trs%trench(index)%bob_trench   = 0.0d0
      trs%trench(index)%pipeType     = -1
      trs%trench(index)%trenchType   = -1
      trs%trench(index)%s1           = trs%trench(index)%waterLevelTrench
      trs%trench(index)%gwl          = 0.0d0
      trs%trench(index)%maxVolTrench = 0.0d0

      if (brs%Count > 0) then
        brs%branch(trs%trench(index)%branchId)%iTrench = index
      endif
      
      addTrench = index

   end function addTrench
   
   double precision function getfPipe2Trench(trench, waterLevel)
      ! Modules               
                              
      implicit none
      
      ! Input/output parameters
      type(t_trench)             :: trench         !< trench data
      double precision           :: waterlevel     !< water level in pipe/channel
      
      ! Local variables
      
      ! Program code
      if (waterlevel <= -trench%bob_pipe .and. trench%s1 <= -trench%bob_pipe) then
         getfPipe2Trench = 0.0d0
      else   
         getfPipe2Trench = (waterLevel - max(trench%s1, -trench%bob_pipe) ) / trench%resistancePipe
      endif

   end function getfPipe2Trench
   
   double precision function getfTrench2GroundWater(trench)
      ! Modules 
                              
      implicit none
      
      ! Input/output parameters
      type(t_trench)             :: trench         !< trench data
      
      ! Local variables
      double precision           :: trch_bob       !< trench bottom
      
      ! Program code
      trch_bob = trench%bob_trench
   
      if (trench%s1 <= -trch_bob .and. trench%gwl <= -trch_bob) then
         getfTrench2GroundWater = 0.0d0
      elseif (trench%resistanceTrench < TR_EPS) then
         getfTrench2GroundWater = 0.0d0
      else
         getfTrench2GroundWater = (trench%s1 - max(trench%gwl, -trch_bob)) / trench%resistanceTrench
      endif
   end function getfTrench2GroundWater

end module m_trenches     
