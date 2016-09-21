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
! FORTRAN 77 version of INTERACT routines.
!
! Most functions can be executed normally, as long as CHARACTER*1
! is available.
!
! NOT AVAILABLE: CMS.
!
! Note: CHARACTER and INTEGER variables should be equivalenced in
! several routines. In IBM FORTRAN this is possible, according to ANSI
! standard it is not. To emulate this equivalence, the build-in
! functions "ICHAR" and "CHAR" are used here, WHENEVER one of the
! variables to be equivalenced is changed in one of the programs.
!
! Further note: default delimiters are set in block data according to
! EBDIC ordering. When routines are used on computers using ASCI code,
! a number 1 should be set for elements 33 and 45 of arrays DLM and
! IGN.
!
      INTEGER FUNCTION GETS(SOURCE, STPOS, LENGTH, MAXOUT,OUTPUT,LENOUT)
!
      IMPLICIT INTEGER (A-Z)
      CHARACTER*1 SOURCE(1), OUTPUT(1), BLANK
      COMMON /ZDLMTZ/DLM(256), IGN(256)
      DATA CHR/0/, BLANK/' '/
!
      IF (STPOS.LE.0) GOTO 1001
      IF (LENGTH.LT.STPOS) GOTO 1002
      IF (MAXOUT.LE.0.OR.MAXOUT.GT.256) GOTO 1003
      DO 10 IFND=STPOS,LENGTH
! *** Convert CHARACTER to INTEGER.
      CHR = ICHAR(SOURCE(IFND))
! *** Conversion completed.
      IF (IGN(CHR+1).EQ.0) GOTO 15
10    CONTINUE
      GOTO 100
15    DO 20 JFND=IFND,LENGTH
! *** Convert CHARACTER to INTEGER.
      CHR = ICHAR(SOURCE(JFND))
! *** Conversion completed.
      IF (DLM(CHR+1).NE.0) GOTO 22
20    CONTINUE
      JFND = LENGTH + 1
22    IF (IFND.EQ.JFND) JFND = JFND + 1
      STPOS = JFND
      LENOUT = JFND - IFND
      IF (LENOUT.GT.MAXOUT) LENOUT = MAXOUT
      DO 25 I=1,LENOUT
25    OUTPUT(I) = SOURCE(IFND+I-1)
      GETS = 0
      IF (LENOUT.EQ.MAXOUT) RETURN
      LOUT = LENOUT + 1
      DO 30 I=LOUT,MAXOUT
30    OUTPUT(I) = BLANK
      RETURN
100   GETS = -1
      RETURN
1001  GETS = 1
      RETURN
1002  GETS = 2
      RETURN
1003  GETS = 3
      RETURN
      END
