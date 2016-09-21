!!  Copyright (C)  Stichting Deltares, 2012-2015.
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

module close_all_files_mod
!
!  module declarations
!
      use timers
implicit none  ! force explicit typing
!
contains
      subroutine close_all_files(lun, nfiles)
      integer :: nfiles, i
!
      integer, dimension(nfiles) :: lun
      logical                    :: open
      character(len=256)         :: file_name
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "close_all_files", ithndl )

      do i=1,nfiles
         inquire(unit=lun(i),opened=open,name=file_name)
         if (open) close(lun(i))
      end do
      if ( timon ) call timstop ( ithndl )
      end subroutine close_all_files
end module
