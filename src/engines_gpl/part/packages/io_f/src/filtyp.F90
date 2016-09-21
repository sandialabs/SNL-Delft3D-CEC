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

module filtyp_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use precision_part   ! single and double precision
      use timers
use fileinfo    ! file information for all input/output files
!
implicit none   ! force explicit typing
!
contains
      subroutine filtyp()
!
!                        d e l p a r    v3.60
!
!     created               : december 2000, by antoon koster
!
!     function              : define platform-dependent file types
!                                  'binary'      for pc
!                                  'unformatted' for unix
!
!     subroutines called    : -
!
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "filtyp", ithndl )
#ifdef HAVE_CONFIG_H

      ftype(1) = 'unformatted'
      ftype(2) = 'unformatted'
#else

      ftype(1) = 'binary'
      ftype(2) = 'binary'
#endif

      if ( timon ) call timstop ( ithndl )
      return
      end subroutine
end module
