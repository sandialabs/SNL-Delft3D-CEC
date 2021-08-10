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

      SUBROUTINE WRWRK3 ( LUN   , LCHAR ,ITOTA , ITOTI , ITOTC  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : march 93 by Jan van Beek
!
!     FUNCTION            : Writes DELWAQ2 boot file
!
!     LOGICAL UNITNUMBERS : LUN(1)  - DELWAQ boot file
!
!     SUBROUTINES CALLED  : SRSTOP, stops execution
!                           DHOPNF, opens files
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     LUN     INTEGER    *         IN      Logical unit numbers
!     LCHAR   CHAR*(*)   *         IN      File names
!     ITOTA   INTEGER    1         IN      Dimension real array
!     ITOTI   INTEGER    1         IN      Dimension integer array
!     ITOTC   INTEGER    1         IN      Dimension character array
!
!     declarations
!
      INTEGER       LUN(*)
      CHARACTER*(*) LCHAR(*)
!
!     COMMON  /  SYSN   /   System characteristics
!
      INCLUDE 'sysn.inc'
!
!     COMMON  /  SYSI  /    Timer characteristics
!
      INCLUDE 'sysi.inc'
!
!     input structure for boot-file
!
      DIMENSION           IN(INSIZE)       , II(IISIZE)
      EQUIVALENCE       ( IN(1)  , NOSEG ) , ( II(1), ITSTRT  )
!
!     Local declarations
!
      INTEGER       IERR
!
!     write the boot file
!
      CALL DHOPNF ( LUN(1) , LCHAR(1), 1     , 1     , IERR  )
!
      WRITE ( LUN(1) )   IN
      WRITE ( LUN(1) )   II
      WRITE ( LUN(1) )   ITOTA , ITOTI , ITOTC
      WRITE ( LUN(1) ) ( LUN  (K) , K = 1, NOLUN )
      WRITE ( LUN(1) ) ( LCHAR(K) , K = 1, NOLUN )
!
      CLOSE ( LUN(1) )
!
      RETURN
      END
