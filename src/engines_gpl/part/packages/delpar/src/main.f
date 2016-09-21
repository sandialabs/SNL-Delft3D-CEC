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

!
! Wrapper for FORTRAN main program: PART
!
! Converted from C++ to fortran 17 dec 2014
!

      program delpar

      implicit none

      integer                          :: argc
      character(len=256)               :: filpar
      character(len=256)               :: filename
      character(len=256)               :: runid
      logical                          :: exi
      integer(4)                       :: ioerr
      integer(4)                       :: i

!     
!     Call F90 routine dlmain (former main program)
!     (Use a variety of methods to determine the
!     name of the file that holds all relevant
!     file names)
!     
      argc = iargc() + 1
      if ( argc == 1 ) then
         filpar = "runid.par"
         inquire ( file = filpar , exist = exi )
         if ( exi ) then
            open(9,file=filpar)
            read(9,'(a)',iostat=ioerr) runid
            if ( ioerr .ne. 0 ) runid = ' '
            close(9)
            filename = trim(runid)//".mdp"
         else
            filename = "filename.dat"
         endif
      else
          call getarg(1, filename)
      end if
      
      call dlmain (filename)

      end program
