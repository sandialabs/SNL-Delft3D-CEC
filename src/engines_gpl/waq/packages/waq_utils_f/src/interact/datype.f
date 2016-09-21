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
! Integer function to obtain the datatype of a character.
! Notice that numbers in exponential notation such as 2.3E3 are
! recognised as valid numerical data by this function.
! DATYPE = 0 for numerical data and 1 otherwise.
! The arguments of this function are similar to the EXEC2 function
! &DATATYPE OF.
!
      INTEGER FUNCTION DATYPE (SOURCE,LENGTH)
      CHARACTER*1 SOURCE(*)
      CHARACTER*255 RESULT
      INTEGER GETS, STOR, POS, LENGTH
!
! Use STOR to determine whether SOURCE contains a valid number.
!
      POS = 1
      IRCS = STOR (SOURCE, POS, LENGTH, RNUM)
      IF (IRCS .EQ. 0) GO TO 100
      IRCG = GETS(SOURCE,POS,LENGTH,255,RESULT,LENOUT)
      IF (IRCG .EQ. 0) GO TO 200
      DATYPE = IRCS
      RETURN
!
100   DATYPE = 0
      RETURN
!
200   DATYPE = 1
      RETURN
      END
