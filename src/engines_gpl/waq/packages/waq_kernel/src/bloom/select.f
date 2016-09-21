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

!    Date:       13 Dec 1989
!    Time:       07:50
!    Program:    SELECT.FOR
!    Version:    1.0
!    Programmer: ??????
!    Previous version(s):
!    0.0 -- 12 Dec 1989 -- 10:19 -- Operating System: DOS
!
!  *********************************************************************
!  *  SUBROUTINE SELECT TO CHOOSE ALTERNATIVES ANYWHERE IN THE PROGRAM *
!  *********************************************************************
!
! PC program version for Microsoft fortran.
!
      SUBROUTINE BLSELECT (ARRAY, NELEM, LABEL)
      CHARACTER*8 ARRAY (*)
      CHARACTER*71 LINE2
      CHARACTER*1 CKEY,BLANK, LINE3(71)
      INTEGER NELEM, CHOISE, WIPE, GETS, LABEL, CURLIN, CURCOL,
     1        FRSLIN, UPRCAS
      INTEGER RIGHT, LEFT, UP, DOWN, CR, KEY, BKSP, HOME
      LOGICAL LHOME
      EQUIVALENCE (LINE2,LINE3)
!
! Set keys to scan codes.
!
      PARAMETER (RIGHT=77, LEFT=75, UP=72, DOWN=80, CR=13, BKSP=8,
     1           HOME=71)
      PARAMETER (FRSLIN=3, MAXCO8=8)
      INCLUDE 'ioblck.inc'
      DATA BLANK  /' '/
!
! Check if there are any unprocessed arguments left in LINE. If this is
! the case, exit and let the old BLOOM II input routines deal with the
! input.
!
      IPOS = POSIT
!     IF (GETS (LINE,IPOS,80,8,TOKEN,LENTOK) .EQ. 0) RETURN
!
! Clear the screen, wipe lines and set or compute control variables.
!
      WRITE (OUUNI, 5)
5     FORMAT (' ')
      CALL MORESC
      CALL CLRSCR
      IRC = WIPE (LINE, 1, 80)
      IRC = WIPE (LINE2, 1, 71)
      POSIT = 1
      IPTR = 0
      CURLIN = 1
      CURCOL = 1
      CHOISE = 1
      MAXCOL = MIN0 (MAXCO8, NELEM)
      RELM = MAXCOL
      RELM = REAL (NELEM / RELM)
      MAXLIN = 1 + INT (RELM-0.01)
      LHOME = .FALSE.
!
! Write options which may be selected to screen.
! Call PROMPT to get specific command indicator. Save the cursor position
! following the prompt.
!
      CALL CURSOR (1,1)
      WRITE (OUUNI,10)
   10 FORMAT (1X,'Select one of the following:',2(' ',/))
      WRITE (OUUNI,20) (ARRAY(I), I = 1, NELEM)
   20 FORMAT (8(2X,A8))
!     CALL CURSOR (24,1)
      CALL CURSOR (23,1)
      CALL VIDEO (7)
!     CALL CURSOR (24,1)
      CALL CURSOR (23,1)
      WRITE (OUUNI,25)
   25 FORMAT (2X,'Cursor keys = Select      Home = Choose input method',
     1        '       Enter = Confirm   ')
!     CALL CURSOR (24,1)
      CALL CURSOR (23,1)
      CALL VIDEO (0)
      CALL CURSOR (MAXLIN+8,1)
      CALL BLPROMPT (LABEL, 0)
      CALL PSHCUR
!
! Major program loop. Highlight currently selected option. Also write
! it following the prompt at the previously saved cursor position.
! If additional options were typed at the prompt (IPTR > 0) write
! them following the selected option.
!
   30 CALL CURSOR (CURLIN+FRSLIN,(CURCOL-1)*9+CURCOL)
      CALL VIDEO (7)
      CALL CURSOR (CURLIN+FRSLIN,(CURCOL-1)*9+CURCOL)
      WRITE (OUUNI,40) ARRAY(CHOISE)
   40 FORMAT (1X,A8)
      CALL VIDEO (0)
   50 CALL CURSOR (MAXLIN+8,1)
      CALL POPCUR
      IF ( .NOT. LHOME) WRITE (OUUNI,60) ARRAY (CHOISE)
   60 FORMAT (1X,A8,' ')
      IF (IPTR .GT. 0) WRITE (OUUNI,70) (LINE3(I),I=1,IPTR)
   70 FORMAT (71A1)
!
! Wait for keyboard input.
!
   80 CALL INKEY (KEY)
      IF (KEY .EQ. 0) GO TO 80
      IF (KEY .LT. 256) THEN
         IF (KEY .GE. 58 .AND. KEY .LE. 64) GO TO 80
         IF (KEY .GE. 123) GO TO 80
         CKEY = CHAR(KEY)
         GO TO 90
      END IF
!
! This is an extended key. Wipe LINE2 and clear the screen line at the
! prompt.
!
      KEY = KEY / 256
      IRC = WIPE (LINE2, 1, 71)
      IF (IPTR .GT. 0) THEN
         IPTR = 0
         CALL CURSOR (MAXLIN+8,1)
         CALL POPCUR
         CALL CLRLIN
      END IF
!
! Check if KEY is Home. Toggle its current value.
!
       IF (KEY .EQ. HOME) THEN
          IF (LHOME) THEN
             LHOME = .FALSE.
             GO TO 30
          ELSE
             LHOME = .TRUE.
             CALL CURSOR (CURLIN+FRSLIN,(CURCOL-1)*9+CURCOL)
             WRITE (OUUNI,40) ARRAY(CHOISE)
             CALL CURSOR (MAXLIN+8,1)
             CALL POPCUR
             CALL CLRLIN
             GO TO 50
          END IF
       END IF
!
! Extended key, but not HOME: set HOME toggle = false
!
       LHOME = .FALSE.
!
! Check if KEY is cursor down.
!
      IF (KEY .EQ. DOWN) THEN
         CALL CURSOR (CURLIN+FRSLIN,(CURCOL-1)*9+CURCOL)
         WRITE (OUUNI,40) ARRAY(CHOISE)
         IF (CURLIN .LT. MAXLIN) CURLIN = CURLIN + 1
         CHOISE = (CURLIN-1)*MAXCOL + CURCOL
         IF (CHOISE .GT. NELEM) THEN
            CURLIN = CURLIN - 1
            CHOISE = (CURLIN-1)*MAXCOL + CURCOL
         END IF
         GO TO 30
      END IF
!
! Check if KEY is cursor up.
!
      IF (KEY .EQ. UP) THEN
         CALL CURSOR (CURLIN+FRSLIN,(CURCOL-1)*9+CURCOL)
         WRITE (OUUNI,40) ARRAY(CHOISE)
         IF (CURLIN .GT. 1) CURLIN = CURLIN - 1
         CHOISE = (CURLIN-1)*MAXCOL + CURCOL
         GO TO 30
      END IF
!
! Check if KEY is cursor right.
!
      IF (KEY .EQ. RIGHT) THEN
         CALL CURSOR (CURLIN+FRSLIN,(CURCOL-1)*9+CURCOL)
         WRITE (OUUNI,40) ARRAY(CHOISE)
         IF (CURCOL .LT. MAXCOL) THEN
            CURCOL = CURCOL + 1
         ELSE
            CURCOL = 1
         END IF
         CHOISE = (CURLIN-1)*MAXCOL + CURCOL
         IF (CHOISE .GT. NELEM) THEN
            CURCOL = 1
            CHOISE = (CURLIN-1)*MAXCOL + CURCOL
         END IF
         GO TO 30
      END IF
!
! Check if KEY is cursor left.
!
      IF (KEY .EQ. LEFT) THEN
         CALL CURSOR (CURLIN+FRSLIN,(CURCOL-1)*9+CURCOL)
         WRITE (OUUNI,40) ARRAY(CHOISE)
         IF (CURCOL .GT. 1) THEN
            CURCOL = CURCOL - 1
         ELSE
            CURCOL = MAXCOL
         END IF
   85    CHOISE = (CURLIN-1)*MAXCOL + CURCOL
         IF (CHOISE .GT. NELEM) THEN
            CURCOL = CURCOL - 1
            GO TO 85
         END IF
         GO TO 30
      END IF
!
! No special key. Return waiting for new input.
!
      GO TO 30
!
! Check if key is carriage return, back space or a alphanumeric or
! numeric key.
!
   90 CONTINUE
      IF (KEY .EQ. CR) GO TO 100
!
! Backspace key: move column pointer one position to the left, put a
! blank in LINE2 and erase the previous character from the display.
! Any other key: increase the column pointer and write the next
! character to LINE2.
!
      IF (KEY .EQ. BKSP) THEN
         IF (IPTR .GT. 0) THEN
            LINE3 (IPTR) = BLANK
            IPTR = IPTR - 1
            CALL CURSOR (MAXLIN+8,1)
            CALL POPCUR
            CALL CLRLIN
         END IF
      ELSE
         IPTR = IPTR + 1
         LINE3 (IPTR) = CKEY
      END IF
      GO TO 50
!
! Carrige return. Write the currently selected option(s) stored in LINE2
! to LINE, which is were they would have been if the old input routines
! had been used. Have these routines deal with LINE.
!
  100 CONTINUE
      IF (LHOME) THEN
         DO 105 I = 1,8
  105    LINE(1) (I:I) = BLANK
      ELSE
         LINE(1) = ARRAY(CHOISE)
      END IF
      LINE(2) (1:1) = BLANK
      LINE(2) (2:8) = LINE2 (1:7)
      K = 0
      DO 110 I = 3,8
         K = K + 8
         LINE(I) = LINE2 (K:K+8)
  110 CONTINUE
      IRC = UPRCAS (LINE, LINE, 80)
!
! Erase the last line of the screen. Write a blank to get the cursor
! in column 1 (without this statement, it may end up in column 1 or 2).
!
!     CALL CURSOR (MAXLIN+9,1)
      CALL CURSOR (24,1)
      CALL CLRLIN
      CALL CURSOR (14,1)
      WRITE (OUUNI, 5)
      RETURN
      END
