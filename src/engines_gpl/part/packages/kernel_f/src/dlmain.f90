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

      subroutine dlmain( ifnam )
!
!     Deltares (former: Deltares)  - particle tracking program
!
!     function              : main steering module for 'dynamic' delpar
!
!     programmer            : antoon koster
!
!     created               : january 2001
!     revised                 october 2008
!                             Part code fully Fortran90 compliant
!                             IDE MS Visual Studio 2005
!                             see also: http://issues.deltares.nl/browse/DELFT3D-12076
!
!
!     subroutines called    : getdim  - retrieve proper array dimensions
!                                       by scanning input files
!                             pointrs - make pointers and allocate arrays
!                             delpar  - activate former 'static' main program
!
!     explanation:
!     this (new) main program was required for the 'dynamic memory' version
!     of delpar, and replaces the former 'static' main program.
!
!     for delpar the dynamic memory allocation is based on fmm (flexible
!     memory manager) as developped by wl|Deltares, and fully
!     fortran 77.
!
!     for the 'dynamic version' of delpar the 'static' main program was
!     converted to a subroutine delpar. the changes could be restricted
!     to commenting out the parameter statement used by former static arrays.
!     in the call to delpar at the end of this module all former static
!     arrays are now passed dynamically by their pointer referring to the
!     fmm arrays , like
!          i for integer
!          r for reals
!          c for character
!     notice that we use a shorthand notation for the original (longer)
!     fmm names (see the include-file alias.inc)
!
!     first in routine getdim the 'static' array dimensions will be retrieved
!     from scanning the input files.
!     after that in routine pointrs all former static arrays will be allocated
!     dynamically, with their actual size based on the retrieved user input files,
!     thus allocating exactly the proper size for that delpar run.
!
!     major objective for implementing the 'dynamic' version of delpar was
!     1) keeping the 'dynamic' code as close as possible to the 'static' code
!        by introducing the dynamic fmm arrays already on the top level
!        (=main program), rather than inserting this fmm code into the 'old'
!        main program.
!     2) preserving an optimal coherence (traceability) between the 'static'
!        and the 'dynamic' memory allocations by using same naming conventions.
!        example for memory allocation for concentration array aconc:
!            static  memory: dimension aconc (nosubx*nwmax)
!            dynamic memory: aconc = mkgtrp('aconc',nosubx*nwmax)
!            (mkgtrp = make and get real pointer for fmm)
!
      use precision_part                     ! single/double precision
      use delpar_mod                    ! explicit interface
      implicit none                     ! force explicit typing

!     arguments

      character(len=*) :: ifnam

!     call actual computational part

      call delpar(ifnam )

      stop
      end subroutine
