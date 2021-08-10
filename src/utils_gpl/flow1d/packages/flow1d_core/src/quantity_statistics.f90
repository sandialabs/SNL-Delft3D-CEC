module QSCallBack
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
!  $Id: quantity_statistics.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/quantity_statistics.f90 $
!-------------------------------------------------------------------------------

   abstract interface
      double precision function QS_callbackiface(igrid)
         integer, intent(in) :: igrid !grid number
      end function QS_callbackiface
   end interface
end module QSCallBack


  module m_QuantityStatistics
   use QSCallBack
   use messageHandling
   IMPLICIT NONE
   !
   integer, parameter :: double = 8
   double precision, parameter :: accur = 1d-13
   !
   private
   !
   public dealloc
   public realloc
   public addOperation
   public calculateStatistics
   public resetStatistics
   public getStatisticOutput
   public QS_GetStatisticalOutputIndex
   public calculateAllStatistics 

   interface dealloc
      module procedure deallocQS
   end interface dealloc

   interface realloc
      module procedure reallocQS
   end interface realloc


    !
    !> type t_quantityStatisticsSet contains an array of quantities of type t_quantity_statistics
    !! A quantity, uniquely identified with the member variable string quantity_ID, is a property defined on
    !! a subset of grid points, i.e. its domain. Given a site that lies inside of its domain,
    !! at least one statistical property is defined. This module can be used to manage those statistics.
    !! Each statistical property has its own subdomain, the possible properties are
    !! - current
    !! - average
    !! - minimum
    !! - maximum
    !! indices of some importantant quantities are stored so that the memory can be efficiently addressed.
    !!
    !
    !> type t_quantityStatistics contains statistics associated with the grid points of which the indexes are stored in array domain_sites,
    !! defining the domain of quantity.
    type, public :: t_quantityStatistics
        character(len=idlen)          :: name                             !< name in netcdf file
        character(len=idlen)          :: long_name                        !< long name in netcdf file
        character(len=idlen)          :: unit                             !< unit in netcdf file
        integer                       :: varid = -1                       !< NetCDF var ID
        integer                       :: ncid = -1                        !< NetCDF file ID
        integer                        :: quantityID                       !< Quantity id of variable to perform operation on
        integer                        :: elementsetID                     !< Elementset id of variable to perform operation on
        integer                        :: operation = 0                    !< indicates the statistical operation that has to be performed
        integer                        :: valuesCount                      !< number of values in array Values
        double precision, pointer, dimension(:)    :: Values            => null()   !< contains intermediate results for the statistical operation
        integer, pointer, dimension(:)             :: maskIndex         => null()   !< maskIndex of length ValuesCount. Used for indirection from inputValues to Values
        double precision               :: timeInterval = 0d0                        !< duration of the current step
        double precision               :: outputInterval = 0d0                      !< interval to produce output (when outputInterval==timeInterval, output must be requested.)
        double precision               :: timeForOutput = 0d0                       !< next time instance to request output
        procedure(QS_callbackiface), pointer, nopass  :: QS_GetValue => null()      !< function pointer for retrieving results from the model engine
        double precision, pointer, dimension(:)       :: inputValues => null()      !< Pointer to array in model engine, that contains the results
    end type
	 
    type, public :: t_quantityStatisticsSet
        integer                                            :: size             = 0   !< current length of array quantities
        integer                                            :: count            = 0   !< number of defined and initialized quantities
        integer                                            :: growsBy          = 20  !< used increment for extending array quantities
        type(t_quantityStatistics), pointer, dimension(:) :: quantities => null()   !< the actual quantities
        !
        ! indexes of several quantities in the quantities array
    end type

    double precision, parameter              :: maxValue =  1d20           !< Large value for computing minimum values
    double precision, parameter              :: minValue = -1d20           !< Small Value for computing maximum values
    integer, parameter, public               :: QS_ComputeMaximum = 1
    integer, parameter, public               :: QS_ComputeMinimum = 2
    integer, parameter, public               :: QS_ComputeAverage = 3
    integer, parameter, public               :: QS_ComputeCurrent = 4
    integer, parameter, public               :: QS_ComputeEnd = 4          !< highest number in statistical options
contains
    !
! -------------------------------------------------------------------------
! ---           INTERFACE TYPE t_quantityStatisticsSet                ---
! -------------------------------------------------------------------------
!    !

   integer function AddOperation(quantitySet, quantityID, elementSetId, operation,           &
                                 valuesCount, maskIndex, outputInterval, inputValues, QS_Getvalue )!, outputtimestep

      ! modules

      implicit none
      ! variables
      type(t_quantityStatisticsSet)             :: quantitySet
      integer                                   :: quantityId
      integer                                   :: elementsetId
      integer                                   :: operation
      integer                                   :: valuesCount
      integer, dimension(:)                     :: maskIndex
      double precision                          :: outputInterval
      procedure(QS_callbackiface), pointer      :: QS_GetValue
      double precision, dimension(:), pointer   :: inputValues

      ! local variables
      type(t_quantityStatistics), pointer       :: quant

      !program code

      quantitySet%count = quantitySet%count+1

      if (quantitySet%count > quantitySet%size) then
         call realloc(quantitySet)
      endif

      quant => quantitySet%quantities(quantitySet%count)

      quant%quantityID = quantityID
      quant%elementSetID = elementsetID
      quant%operation    = operation
      quant%valuesCount    = valuesCount
      allocate(quant%maskIndex(valuesCount))
      quant%maskIndex      = maskIndex
      if (.not. associated(QS_GetValue)) then
         quant%inputValues => inputValues
      else
         quant%QS_GetValue => QS_GetValue
      endif
      quant%outputInterval = outputInterval

      allocate(quant%Values(valuesCount))

      if ( (elementSetId < 17) .or. (elementSetId > 20) ) then
         call calculateStatistics(quantitySet, quantitySet%count, 0d0)
      endif

      AddOperation = quantitySet%count

   end function AddOperation

   subroutine deallocQS(quantitySet)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_quantityStatisticsSet), intent(inout) :: quantitySet

      ! Local variables
      integer                       :: i

      ! Program code

      if (associated(quantitySet%quantities)) then

         do i = 1, quantitySet%count
            if (associated(quantitySet%quantities(i)%Values) ) then
                    deallocate(quantitySet%quantities(i)%Values)
                    quantitySet%quantities(i)%Values => null()
            endif
            if (associated(quantitySet%quantities(i)%maskIndex) ) then
                    deallocate(quantitySet%quantities(i)%maskIndex)
            endif
         enddo
         deallocate(quantitySet%quantities)
         quantitySet%quantities => null()
         quantitySet%count = 0
         quantitySet%size  = 0
      endif
   end subroutine
   !
   !
   subroutine reallocQS(quantitySet)
      ! Modules

      implicit none
      ! Input/output parameters
      type(t_quantityStatisticsSet), intent(inout)      :: quantitySet

      ! Local variables
      type(t_quantityStatistics), pointer, dimension(:) :: oldquants

      ! Program code

      if (quantitySet%size > 0) then
         oldquants=>quantitySet%quantities
      endif

      if (quantitySet%growsBy <=0) then
         quantitySet%growsBy = 200
      endif
      allocate(quantitySet%quantities(quantitySet%size+quantitySet%growsBy))

      if (quantitySet%size > 0) then
         quantitySet%quantities(1:quantitySet%size) = oldquants(1:quantitySet%size)
         deallocate(oldquants)
      endif
      quantitySet%size = quantitySet%size+quantitySet%growsBy
   end subroutine

   subroutine calculateStatistics(quantitySet, index, dt) !, time
      ! modules

      implicit none
      ! variables
      type(t_quantityStatisticsSet), intent(inout)      :: quantitySet
      integer                                           :: index
      double precision                                  :: dt

      ! local variables
      integer           :: i
      type(t_quantityStatistics), pointer       :: quant

      !program code
      ! if abs(time - (time*nint(time/dtout)) < 1e-5 then
      !    flush
      ! endif

      quant => quantitySet%quantities(index)
      quant%timeInterval = quant%timeInterval + dt

      if ( associated(quant%QS_GetValue) ) then
         select case(quant%operation)
         case (QS_computeAverage)
            do i = 1, quant%valuesCount
               quant%values(i) = quant%values(i) + quant%QS_GetValue(quant%maskIndex(i))*dt
            enddo
         case (QS_computeMinimum)
            do i = 1, quant%valuesCount
               quant%Values(i) = min(quant%Values(i), quant%QS_GetValue(quant%maskIndex(i)))
            enddo
         case (QS_computeMaximum)
            do i = 1, quant%valuesCount
               quant%Values(i) = max(quant%Values(i), quant%QS_GetValue(quant%maskIndex(i)))
            enddo
         end select
      else
         select case(quant%operation)
         case (QS_computeAverage)
            do i = 1, quant%valuesCount
               quant%values(i) = quant%values(i) + quant%inputValues(quant%maskIndex(i))*dt
            enddo
         case (QS_computeMinimum)
            do i = 1, quant%valuesCount
               quant%Values(i) = min(quant%Values(i), quant%inputValues(quant%maskIndex(i)))
            enddo
         case (QS_computeMaximum)
            do i = 1, quant%valuesCount
               quant%Values(i) = max(quant%Values(i), quant%inputValues(quant%maskIndex(i)))
            enddo
         end select
      endif
   end subroutine calculateStatistics

   subroutine resetStatistics(statsSet, time)
      implicit none

      type(t_quantityStatisticsSet)      :: statsSet
      double precision                   :: time

      integer i
      type(t_quantityStatistics), pointer ::  quant


      do i = 1, statsSet%count
         if (time >= statsSet%quantities(i)%timeForOutput*(1-accur)) then
            quant => statsSet%quantities(i)
            quant%timeForOutput = quant%timeForOutput + quant%outputInterval
            quant%timeInterval = 0d0
            select case (quant%operation)
            case (QS_computeAverage)
               quant%Values = 0d0
            case (QS_computeMinimum)
               quant%Values = maxValue
            case (QS_computeMaximum)
               quant%Values = minValue
            end select
         endif
      enddo
   end subroutine resetStatistics

   subroutine getStatisticOutput (quantitySet, index, values, count)
      ! modules
      use MessageHandling

      implicit none
      ! variables
      type (t_quantityStatisticsSet)         :: quantitySet
      integer                                :: index
      integer                                :: count
      double precision, dimension(count)     :: values

      ! local variables
      integer        :: i
      type (t_quantityStatistics), pointer :: quant

      !program code
      quant => quantitySet%quantities(index)
      if (count /= quant%valuesCount) then
         call setmessage(LEVEL_FATAL, 'getStatisticalOutput: Size of array is incorrect')
      endif

      ! check if full time interval is performed
      if ( quant%TimeForOutput/=0d0 .and.            &
           abs(quant%TimeInterval - quant%outputInterval) > 1d-3) then
         call setmessage(LEVEL_FATAL, 'getStatisticalOutput: incorrect time for retrieving data')
      endif

      if (quant%TimeForOutput==0d0) then
         if (associated(quant%QS_GetValue) ) then
            do i = 1, quant%valuesCount
               values(i) = quant%QS_GetValue(quant%maskIndex(i))
            enddo
         else
            do i = 1, quant%valuesCount
               values(i) = quant%inputValues(quant%maskIndex(i))
            enddo
         endif
      else
         select case(quant%operation)
         case (QS_ComputeCurrent)
            if (associated(quant%QS_GetValue) ) then
               do i = 1, quant%valuesCount
                  values(i) = quant%QS_GetValue(quant%maskIndex(i))
               enddo
            else
               do i = 1, quant%valuesCount
                  values(i) = quant%inputValues(quant%maskIndex(i))
               enddo
            endif

         case (QS_computeAverage)
            values = quant%values / quant%timeInterval
         case default
            values = quant%values
         end select
      endif
   end subroutine getStatisticOutput

   integer function QS_GetStatisticalOutputIndex (quantitySet, elementsetId, quantityId, operation, outputInterval)
      ! modules
      use MessageHandling

      implicit none
      ! variables
      type(t_quantityStatisticsSet)         :: quantitySet
      integer elementsetId
      integer quantityId
      integer operation
      double precision outputInterval

      ! local variables
      type(t_quantityStatistics), pointer         :: quant
      character(len=CharLn)                       :: line
      integer i

      !program code
      QS_GetStatisticalOutputIndex = -1
      do i = 1, quantitySet%count
         quant=>quantitySet%quantities(i)
         if ( (quant%quantityID     == quantityId)    .and.             &
              (quant%elementsetID   == elementsetId)  .and.             &
              (quant%operation      == operation)     .and.             &
              (quant%outputInterval == outputInterval)      ) then
            QS_GetStatisticalOutputIndex = i
            exit
         endif
      enddo
      if (QS_GetStatisticalOutputIndex <= 0) then
         write(line, '(''Error in Statistical output definition, the combination '',      &
                       ''QuantityId = '', i5, '' ElementSetId = '', i5,             &
                       '' Operation = '', i5, '' OutputInterval  = '', g10.3, ''. Does not exist'')') quantityId, elementsetId, operation, outputInterval
         call SetMessage(LEVEL_FATAL, line)
      endif
   end function QS_GetStatisticalOutputIndex

   subroutine CalculateAllStatistics(quantitySet, dt)
      ! modules

      implicit none
      ! variables
      type(t_quantityStatisticsSet) :: quantitySet
      double precision dt
      ! local variables
      integer     :: i
      !program code
      do i=1, quantitySet%count
         call CalculateStatistics(quantitySet, i, dt)
      enddo
   end subroutine CalculateAllStatistics


end module m_QuantityStatistics
