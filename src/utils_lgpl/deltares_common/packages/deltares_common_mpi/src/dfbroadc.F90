#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
subroutine dfbroadc ( iptr, ilen, itype, error, msgstr )
!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2020.
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
!
!-------------------------------------------------------------------------------
!  $Id: dfbroadc.F90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_lgpl/deltares_common/packages/deltares_common_mpi/src/dfbroadc.F90 $
!!--description-----------------------------------------------------------------
!
!   Broadcasts data from the master to all other processes
!
!!--pseudo code and references--------------------------------------------------
!
!   wrapper for MPI_BCAST
!
!
!!--declarations----------------------------------------------------------------

#ifdef HAVE_MPI
    use mpi
#endif
    use dfparall
    !
    implicit none
!
! Global variables
!
    integer     , intent(inout) :: iptr   ! pointer to first element of array to be sent/received
    integer     , intent(in)    :: ilen   ! length of array to be sent/received
    integer     , intent(inout) :: itype  ! type of data
    logical     , intent(out)   :: error  ! error flag
    character(*), intent(out)   :: msgstr ! string to pass message
!
! Local variables
!
    integer       :: ierr   ! error value of MPI call
!
!! executable statements -------------------------------------------------------
!
    msgstr = ' '
    error  = .false.
    !
    ! if not parallel, return
    !
    if (.not.parll) return
    !
#ifdef HAVE_MPI
    call mpi_bcast ( iptr, ilen, itype, master-1, engine_comm_world, ierr )
    if ( ierr /= MPI_SUCCESS ) then
       write (msgstr,'(a,i5)') 'MPI produces some internal error - return code is ',ierr
       error = .true.
    endif
#endif

end subroutine dfbroadc
