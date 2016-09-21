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
! Integer function to search for the first occurrence of a needle in
! a haystack.
! The arguments of this function are similar to the EXEC2 function
! &LOCATION OF.
!
      INTEGER FUNCTION LOCATE (SOURCE,NEEDLE,MAXSO,MAXNE)
      CHARACTER*(*) SOURCE,NEEDLE
      INTEGER MAXSO,MAXNE,LENSTR
!
! Determine the lengths of SOURCE and NEEDLE. Compare the first
! character in NEEDLE with each character in SOURCE. If a match is
! found, continue up to the last character in NEEDLE to get a complete
! match.
!
      IF (MAXSO .LE. 0 .OR. MAXSO .GT. 255) GO TO 100
      IF (MAXNE .LE. 0 .OR. MAXNE .GT. 255) GO TO 100
      LENSO = LENSTR (SOURCE,MAXSO)
      LENNE = LENSTR (NEEDLE,MAXNE)
      DO 20 I = 1,LENSO
      IF (NEEDLE(1:1) .NE. SOURCE(I:i)) GO TO 20
      DO 10 J = 2,LENNE
      IF (NEEDLE(J:j) .NE. SOURCE(I+J-1:i+j-1)) GO TO 15
10    CONTINUE
      LOCATE = I
      RETURN
15    CONTINUE
20    CONTINUE
!
! No match is found. Set LOCATE to 0.
!
      LOCATE = 0
      RETURN
!
! An error is determined. Set LOCATE to -1.
!
100   LOCATE = -1
      RETURN
      END
