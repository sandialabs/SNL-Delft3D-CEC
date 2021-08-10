!!  Copyright (C)  Stichting Deltares, 2012-2020.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module test_MDU_File_Version
    use ftnunit
    use precision

    implicit none
    real(fp), parameter :: eps = 1.0e-6_fp

contains
!
!
!==============================================================================
subroutine tests_MDU_fileversion
    call test( test_MDU_fielversion_model, 'Tests checking MDU file version (old [model] block).' )
    call test( test_MDU_fielversion_general, 'Tests checking MDU file version (new [General] block).' )
end subroutine tests_MDU_fileversion
!
!
!==============================================================================
subroutine test_MDU_fielversion_model
    use unstruc_model
    use dfm_error
    use m_partitioninfo, only: jampi
    use unstruc_files
    use ifport
    ! Locals 
    integer                   :: istat, ierr
    
    !
    ! Body
    jampi = 0
    !
    istat = CHANGEDIRQQ("MDUversion")
    ! read MDU
    call readMDUFile('old_model.mdu', ierr)
    istat = CHANGEDIRQQ("..")
        
    call assert_equal(ierr, DFM_NOERR, 'Error when reading old MDU file version with [model] block.' ) 
 

end subroutine test_MDU_fielversion_model

subroutine test_MDU_fielversion_general
    use unstruc_model
    use dfm_error
    use m_partitioninfo, only: jampi
    use unstruc_files
    use ifport
    ! Locals 
    integer                   :: istat, ierr
    
    !
    ! Body
    jampi = 0
    !
    istat = CHANGEDIRQQ("MDUversion")
    ! read MDU
    call readMDUFile('new_general.mdu', ierr)
    istat = CHANGEDIRQQ("..")
        
    call assert_equal(ierr, DFM_NOERR, 'Error when reading new MDU file version with [General] block.' ) 
 

end subroutine test_MDU_fielversion_general

end module test_MDU_File_Version
