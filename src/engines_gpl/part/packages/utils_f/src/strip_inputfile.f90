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

module strip_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use precision_part             ! single and double precision
      use timers
!
!  module procedure(s)
!
use stop_exit_mod         ! explicit interface
!
implicit none             ! force explicit typing
!
contains
      subroutine strip ( lunin  , lfile  , lunut  , iout   , npos   ,  &
                         char   ,                            car    )
!
!                   Deltares (former: Deltares)
!
!                        d e l p a r    v2.00
!
!
!     system administration : m. zeeuw
!
!
!     created               : march '88  by  m.e.sileon
!
!
!     function              : deletes empty lines and comment in the input file
!
!
!     note                  : - within delpar, this routine is invocated
!                               only once, so no value should be saved.
!                               suppress compile variables and/or
!                               options  for enabling static memory in
!                               stead of dynamically memory. also
!                               dynamic memory allocation is assumed
!                               default for this routine.
!
!
!     modified              : june 1993, by m. zeeuw
!                             - added error numbers
!
!
!     logical unit numbers  : lunin = unitnumber auxilary input file
!                             lunut = unitnumber scratch file
!                             iout  = unitnumber error messages
!
!
!     subroutines called    : stop_exit.
!
!
!
!     parameters            :
!
!     name    kind     length      funct.  description
!     ----    -----    ------      ------  -----------
!     car     char*npos  1         local   character workspace
!     char    char*1     1         input   'comment' indicator
!     lfile   char*20    1         input   file name
!     npos    integer    1         input   number of significant
!                                          positions in one line

!     declarations
!
      character (len=*)  :: car
      character (len=1)  :: char
      character (len=20) :: lfile
      logical            :: empty
!
      integer(ip) :: i , lunin , lunut , iout , npos , il , is
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "strip", ithndl )
!
      do 200, il = 1, 1000000
        read (lunin, '(a)', end = 300, err = 400) car(1:npos)
        empty = .true.
!
!       write line until char in lunut, strip heading spaces
!
        is = 0
        do 50 i = 1, npos
          if (car(i:i) == char) then
            goto 100
          endif
          if (car(i:i)  /=  ' ') then
            empty = .false.
          endif
          if ( .not. empty) then
            is = is + 1
            car(is:is) = car(i:i)
          endif
   50   continue
  100   if (.not. empty) then
           write (lunut, '(a)') car(1:is)
        endif
  200 continue
!
!     end of file encountered
!
  300 rewind(lunut)
!
!     end of routine
!
      if ( timon ) call timstop ( ithndl )
      return
!
!     errors during read
!
  400 write (iout, 99001) lunin, lfile
      call stop_exit(1)
!
99001 format (  ' Error 4601. Reading file on unit',i3,',',  &
               /'             filename is: ',a20, '.')
!
      end subroutine
end module

