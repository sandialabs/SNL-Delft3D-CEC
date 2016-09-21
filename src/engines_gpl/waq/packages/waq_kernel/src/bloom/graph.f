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
!    Program:    GRAPH.FOR
!    Version:    1.0
!    Programmer: ??????
!    Previous version(s):
!    0.0 -- 12 Dec 1989 -- 10:19 -- Operating System: DOS
!
!  *********************************************************************
!  *      SUBROUTINE GRAPH TO PRODUCE A PLOT OF BLOOM'S OUTPUT         *
!  *********************************************************************
!
      SUBROUTINE GRAPH(NVAR,NREP,IPL,NIT,NOT)
      INCLUDE 'ioblck.inc'
      INCLUDE 'halo.inc'
      INTEGER*4 NVAR,NREP,IPL,NIT,NOT, DELSTR
      INTEGER*4 IPLT(5), IVAR, LDEV, MESS
      REAL A(52,10), XMAX, YMAX
      CHARACTER*90 MTITLE
      CHARACTER*80 STITLE
      CHARACTER*8 XNAM, YNAM, YSYM
      CHARACTER*4 CVAR(10), CPLT(5), BLANK
      DATA BLANK /'    '/
      DATA MESS  /0/
!
! Note: this subroutine is functionally equivalent with subroutine
! "GRAPH" used in the mainframe program version or in previous PC
! versions. The subroutine call is identical and the program reads
! the same input files according to the same formats. This version,
! however, passes its in information to a completely different
! subroutine PLOTGR, which uses HALO routines to create graphical
! plots rather than print plots. The call to PLOTGR therefore is NOT
! the same as in other program versions!!
!
!
! Check if HALO device drivers are available. If not, print a message
! (once) and exit without calling PLOTGR.
!
      IF (LHALO .LT. 0) THEN
         IF (MESS .EQ. 0) THEN
            CALL VIDEO (7)
            WRITE (OUUNI,10)
   10       FORMAT (' No HALO device drivers available for graphic',
     *              ' plots.',/,' Plot request denied. Execution',
     *              ' continues.')
            CALL VIDEO (0)
            MESS = 1
         END IF
         RETURN
      END IF
!
! Read the input from units NIT (plot data) and IPL (plot control
! file). All necessary information for the HALO plots can be obtained
! from the normal files; part of the data is extraneous and therefore
! ignored.
!
      DO 20 I=1,NREP
   20 READ(NIT,30) (A(I,J),J=1,NVAR)
   30 FORMAT (4X,F3.0,5X,9F12.2)
      READ(IPL,40) MTITLE,(CVAR(I),I=1,NVAR)
   40 FORMAT (A90,/,10(A4,2X))
!
! Delimit string, which is used for each plot (title), enabling
! HALO to determine its length.
!
!     IRC = DELSTR (MTITLE, MTITLE, 90)
!
! Read control data for next plot.
!
   50 CONTINUE
      READ(IPL,60,END=140) (CPLT(I),I=1,5)
   60 FORMAT (5(A4,2X))
      READ(IPL,70) YMAX, LDEV, STITLE
   70 FORMAT (F12.0,60X,I4,/,A80)
!
! Look which variables must be plotted. Note: it is assumed that
! variable 1 is the X variable, and variable 2 is the first Y variable.
! The latter will be plotted as a drawn line and therefore no plot
! symbol is determined are transfered to PLOTGR.
!
      IVAR = 0
      DO 100 I = 1,5
      IF (CPLT(I) .EQ. BLANK) GO TO 110
      DO 80 J = 1,NVAR
      IF (CPLT(I) .EQ. CVAR(J) ) GO TO 90
   80 CONTINUE
   90 CONTINUE
      IVAR = IVAR + 1
      IPLT(IVAR) = J
  100 CONTINUE
  110 CONTINUE
!
! If YMAX was not specified, determine it.
!
      IF (YMAX .LT. 1.0D-6) THEN
         DO 130 J = 2, IVAR
         K = IPLT(J)
         DO 120 I = 1, NREP
         YMAX = AMAX1(YMAX, A(I,K))
  120    CONTINUE
  130    CONTINUE
      END IF
      IF (YMAX .LE. 0.0) YMAX = 1.0
!
! Set addional variables to be used in PLOTGR. Call this subroutine
! as many times as there are input data left in the plot control file.
! Delimit strings, enabling HALO to determine their lengths.
!
      XNAM = CPLT(1)
      YNAM = 'ug/l'
      XMAX = 52.
      YSYM = CPLT(3) (1:1)
!     IRC = DELSTR (STITLE, STITLE, 80)
!     IRC = DELSTR (XNAM  , XNAM  , 8)
!     IRC = DELSTR (YNAM  , YNAM  , 8)
!     IRC = DELSTR (YSYM  , YSYM  , 8)
      CALL PLOTGR(A, STITLE, MTITLE, XNAM, YNAM, XMAX, YMAX,
     *            YSYM, IPLT, IVAR, NREP, LDEV, NOT, IOFLAG)
      GO TO 50
  140 CONTINUE
      RETURN
      END
