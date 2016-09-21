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

      SUBROUTINE RDWRK3 ( LUN   , LCHAR ,ITOTA , ITOTI , ITOTC  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : march 93 by Jan van Beek
!
!     FUNCTION            : Reads DELWAQ boot file
!
!     LOGICAL UNITNUMBERS : LUN(1)   - DELWAQ boot file
!
!     SUBROUTINES CALLED  : DHOPNF, opens files
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     LUN     INTEGER    *         OUT     Logical unit numbers
!     LCHAR   CHAR*(*)   *         OUT     File names
!     ITOTA   INTEGER    1         OUT     Dimension real array
!     ITOTI   INTEGER    1         OUT     Dimension integer array
!     ITOTC   INTEGER    1         OUT     Dimension character array
!
!     declarations
!
      INTEGER       ITOTA , ITOTI , ITOTC
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
      INTEGER             LUNIN
      DIMENSION           IN(INSIZE)       , II(IISIZE)
      EQUIVALENCE       ( IN(1)  , NOSEG ) , ( II(1), ITSTRT  )
!
!         boot the system
!
      LCHMAX = LEN(LCHAR(1))
      CALL DHGNAM(LCHAR(1),' ')
      INDX = INDEX ( LCHAR(1) , ' ' )
      IF ( INDX .EQ. 0 ) INDX = LCHMAX + 1
      LCHAR(1) = LCHAR(1)(1:INDX-1)//'-delwaq03.wrk'
      LUNIN    = 14
      CALL DHOPNF ( LUNIN , LCHAR(1), 1     , 2     , IERR  )
!
      READ  ( LUNIN )   IN
      READ  ( LUNIN )   II
      READ  ( LUNIN )  ITOTA  , ITOTI  , ITOTC
      READ  ( LUNIN ) ( LUN(K) , K = 1,NOLUN )
      READ  ( LUNIN ) ( LCHAR(K) , K=1 , NOLUN )
!
      CLOSE ( LUNIN )
!
      RETURN
      END
