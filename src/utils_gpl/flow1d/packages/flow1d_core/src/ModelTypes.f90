module ModelTypes
   use messageHandling 
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
!  $Id: ModelTypes.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/ModelTypes.f90 $
!-------------------------------------------------------------------------------

    ! model run-time parameters, model state variables
    type ModelType
        double precision :: julianStart               ! Start date of simulation in Julian date format
        double precision :: julianEnd                 ! end date of simulation in Julian date format
        double precision :: hoursToStartFromFirstOfJanuari
        integer          :: currentTimeStep
        double precision :: currentTime               ! current time in seconds
        double precision :: timeStep                  ! time step in seconds
        double precision :: mapOutputTimeStep         ! output timestep in seconds for grid points and reach segments
        double precision :: hisOutputTimeStep         ! output timestep in seconds for other data
        integer          :: startDate
        integer          :: endDate
        integer          :: startTime
        integer          :: endTime
        integer          :: restartendTimestep
        integer          :: restartInterval 
        integer          :: nextRestarttimestep
        logical          :: writerestart
        logical          :: userestart
        
        character(len=Charln) :: restartFile
        ! model state variables with dimension of number of points / cells
    end type
end module
