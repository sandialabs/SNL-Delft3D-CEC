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

module stop_exit_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use precision_part  ! single and double precision
!
implicit none  ! force explicit typing
!
contains
      subroutine stop_exit( iexit )
!
!
!                   Deltares (former: Deltares)
!
!     created            : june  '91  by  jan van beek
!
!     modified           : 23 july 1198 by robert vos for cmt/pmt
!
!     function           : stops execution if possible with return value
!
!     note               : watch //char(0) for c-routine
!
!*********************************************************************
!     system dependent routine
!     configuration
!
!     unix systems
!*********************************************************************
!
!     logical units      : -
!
!     parameters    :
!
!     name    kind     length      funct.  description
!     ---------------------------------------------------------
!     iexit   integer    1         input   return value
!     messag  char*40    1         input   message to screen on stop
!     ---------------------------------------------------------
!
      use wait_mod
      implicit none

      integer           :: iexit
      character(len=40) :: messag
!
!     call wait
      if (iexit == 1) then
         write(*,*) 'Simulation stopped because of errors - check the report'
      endif
      stop
!
      end subroutine
end module
