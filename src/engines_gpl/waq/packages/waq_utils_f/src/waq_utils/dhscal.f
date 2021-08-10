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

      SUBROUTINE DHSCAL ( ARRAY1, NOVAL , RSCALE )
!
!     Deltares           SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     Created : June 1994 by Jan van Beek
!
!     Function            : Scales a real array.
!
!     Parameters          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ARRAY1  REAL     *          IN/OUT  Array to be scaled
!     NOVAL   INTEGER  1          INPUT   Number of value's to be scaled
!     RSCALE  REAL     1          INPUT   Scale factor
!
!     Declaration of arguments
!
      INTEGER    NOVAL
      REAL       RSCALE
      REAL       ARRAY1(*)
!
!     Local declaration
!
!     IVAL    INTEGER  1          LOCAL   Loop counter on NOVAL
!
      INTEGER    IVAL
!
      DO 100 IVAL = 1 , NOVAL
         ARRAY1(IVAL) = ARRAY1(IVAL) * RSCALE
  100 CONTINUE
!
      RETURN
      END
