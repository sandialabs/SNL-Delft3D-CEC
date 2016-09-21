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

!    Date:       14 Dec 1989
!    Time:       08:17
!    Program:    PLOTGR.FOR
!    Version:    1.0
!    Programmer: ??????
!    Previous version(s):
!    0.0 -- 12 Dec 1989 -- 10:19 -- Operating System: DOS
!
!  *********************************************************************
!  *      SUBROUTINE PLOTGR TO PRODUCE A PLOT OF BLOOM'S OUTPUT        *
!  *********************************************************************
!
      SUBROUTINE PLOTGR (A, STITLE, MTITLE, XNAM, YNAM, XMAX, YMAX,
     *                   YSYM, IPLT, IVAR, NREP, LDEV, NOT, IOFLAG)
      IMPLICIT INTEGER*2 (I-N)
      INTEGER*4 IPLT(*), IVAR, NREP, LDEV, NOT, IOFLAG, NEWNUM, KEY
      REAL A(52,10), XMAX, YMAX
      CHARACTER MTITLE*(*), STITLE*(*), XNAM*(*), YNAM*(*), YSYM*(*)
      CHARACTER*20 CURFIL
      CHARACTER*8 WAARDE
      INCLUDE 'halo.inc'
      DATA NEWNUM /0/
      DATA IFCOL /2/
      DATA ILCOL /4/
      DATA ISCOL /5/
      DATA IBCOL /15/
!
! Note: this subroutine is functionally equivalent with subroutine
! "PLOTGR" used in the mainframe program version or in previous PC
! versions.
! This version, however, uses HALO88 routines to create graphical
! plots rather than print plots. The call to PLOTGR therefore is NOT
! the same as in other program versions!!
! Note: the value of MODE is used as a flag to determine whether the
! program is running on a color monitor. If this is the case, the
! pallet is set and the colors are changed several times.
!
      LHALO = 1
      IF (MODE .GE. 2) THEN
         LCOLOR = 1
      ELSE
         LCOLOR = 0
      END IF
!
! Initialize the HALO environment.
!
      ISEG = 0
      CALL HALOST (SCRDEV, MODE,PRTDEV,
     1              720, 960, 2, ISEG)
      IF (LCOLOR .EQ. 1) THEN
         CALL SETXPA (IBCOL, IFCOL)
         CALL SETCOL (IFCOL)
         CALL SETTCL (IFCOL, IBCOL)
      END IF
!
! Use functions SETSCR and DISPLA to create plots on an invisible
! graphics screen in batch jobs or for the "PRTPLOT" program option.
! Note: this setting does not work on all display adapters! It works
! on a HERCULES or EGA board, but not on an IBM CGA adapter.
! However, no harm is being done: plots are only visible.
! Note: colors are (re)set to white on black.
!
      IF (IOFLAG .NE. 1 .OR. LDEV .NE. 1) THEN
         CALL SETSCR(2)
         CALL DISPLA(1)
      END IF
      CALL SETIEE(1)
      CALL SETTEXT(1,1,0,0)
!
! Set screen coordinates.
!
      X1 = 0.0
      Y1 = 0.0
      X2 = 1000.0
      Y2 = 500.0
      CALL SETWORLD (X1 , Y1, X2, Y2 )
!
! Choose position of the origin.
!
      X0=90.
      Y0=80.
!
! Write text for X-axis.
!
      CALL MOVTCA(750.0,Y0-75)
      CALL TEXT(XNAM)
!
! Write text for Y-axis.
!
      X=X0-55
      CALL MOVTCA(X,430.0)
      CALL TEXT(YNAM)
!
! Write two header lines: the main title and the subtitle.
!
      IF ( LCOLOR .EQ. 1) CALL SETTCL (ILCOL, IBCOL)
      CALL MOVTCA(X0,480.0)
      CALL TEXT(MTITLE)
      IF ( LCOLOR .EQ. 1) CALL SETTCL (IFCOL, IBCOL)
      CALL MOVTCA(110.0,445.0)
      CALL TEXT(STITLE)
!
! Start plotting the data. Use function "GETAL" to get a rounded value
! for Y-max.
!
      YMAX=GETAL(YMAX)
      CALL MOVABS(X0-10,Y0-8)
      CALL LNABS(960.0,Y0-8)
      CALL MOVABS(X0-10,Y0-8)
      CALL LNABS(X0-10,400.0)
!
! Divide the X-axis and print the axis values.
!
      DO 40 I=0,26
        X=X0+I*(960-X0)/26
        Y=Y0-8
        CALL MOVABS(X,Y)
        IF(INT(I/5.).EQ.(I/5.)) THEN
          Y=Y-10
          CALL LNABS(X,Y)
!
          Y=Y0-45
          X=X-45.
          CALL MOVTCA(X,Y)
          INCXAS=XMAX/26*I
          WRITE(WAARDE,5) INCXAS
    5     FORMAT(I5)
          CALL TEXT(WAARDE)
        ELSE
          Y=Y-4
          CALL LNABS(X,Y)
        ENDIF
!
   40 CONTINUE
!
! Divide the Y-axis and print the axis values.
!
      DO 50 I=0,10
        X=X0-10
        Y=Y0+I*(400-Y0)/10
        CALL MOVABS(X,Y)
        IF(INT(I/2.).EQ.(I/2.)) THEN
          X=X0-22
          CALL LNABS(X,Y)
!
          X=X0-90.
          CALL MOVTCA(X,Y)
          INCYAS=YMAX/10*I
          WRITE(WAARDE,5) INCYAS
          CALL TEXT(WAARDE)
        ELSE
          X=X0-16
          CALL LNABS(X,Y)
        ENDIF
   50 CONTINUE
!
! Plot variable 2 (=first Y-variable) agains variable number 1
! (=X-variable).
!
      IF (LCOLOR .EQ. 1) CALL SETCOL (ILCOL)
      X=X0+(960-X0)/XMAX*A(1,IPLT(1))
      Y=Y0+(400-Y0)/YMAX*A(1,IPLT(2))
      CALL MOVABS(X,Y)
      CALL SETLNSTY(1)

      DO 60 I=2,NREP
      X=X0+(960-X0)/XMAX*A(I,IPLT(1))
      Y=Y0+(400-Y0)/YMAX*A(I,IPLT(2))
      CALL LNABS(X,Y)
  60  CONTINUE
!
! Plot variable 3 (=second Y-variable) agains variable number 1
! (=X-variable).
!
      IF (LCOLOR .EQ. 1) CALL SETTCL (ISCOL, IBCOL)
      DO 75 J=3,IVAR
      DO 70 I=1,NREP
      IF(A(I,IPLT(J)) .LT. 0.0) GOTO 70
      X=X0+(960-X0)/XMAX*A(I,IPLT(1))
      Y=Y0+(400-Y0)/YMAX*A(I,IPLT(J))
      CALL MOVTCA(X,Y)
      CALL TEXT(YSYM)
  70  CONTINUE
  75  CONTINUE
!
! Write final graph. Pause when a screen plot is requested.
! Optionally copy the output to a file.
! Do not pause in a batch job, or if LDEV <> 1:
! no screen plot requested.
!
      IF (LCOLOR .EQ. 1) CALL SETTCL (ILCOL, IBCOLG)
      CALL MOVTCA(X0-80,Y0-80)
      IF (LDEV .EQ. 1 .AND. IOFLAG .EQ. 1) THEN
         CALL TEXT('| Press any key to continue ...|')
         CALL DELTCU
120      CALL INKEY (KEY)
         IF (KEY .EQ. 0) GO TO 120
      ELSE
         CALL DELTCU
      END IF
      NEWNUM = NEWNUM + 1
      CALL GETNAM (CURFIL,NEWNUM,NOT)
      CALL GWRITE (CURFIL)
      CALL CLOSEG
      RETURN
      END
