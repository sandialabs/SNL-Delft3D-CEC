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

!    Date:       24 Feb 1992
!    Time:       20:53
!    Program:    DYNRUN.FOR
!    Version:    1.91
!    Programmer: Hans Los
!    Previous version(s):
!    DYNRUN FORTRAN -- 14 Nov 1989 -- 18:35 -- Operating System: DOS
!    1.8 -- 14 Nov 1989 -- 18:35 -- Operating System: CMS
!    1.7 -- 14 Nov 1989 -- 11:34 -- Operating System: CMS
!    1.6 -- 14 Nov 1989 -- 11:14 -- Operating System: CMS
!    1.5 -- 9 Nov 1989 -- 15:18 -- Operating System: CMS
!    1.4 -- 9 Nov 1989 -- 15:10 -- Operating System: CMS
!    1.3 -- 31 Oct 1989 -- 14:36 -- Operating System: CMS
!    1.2 -- 31 Oct 1989 -- 11:24
!    1.1 -- 31 Oct 1989 -- 09:03
!    1.0 -- 23 Oct 1989 -- 13:21
!    0.0 -- 23 Oct 1989 --  9:46
!
!  *********************************************************************
!  *         SUBROUTINE DYNRUN TO SOLVE BLOOM PROBLEM                  *
!  *********************************************************************
!
!  *********************************************************************
!  *      SPECIAL ECOLUMN - BLOOM II PROGRAM VERSION                   *
!  *********************************************************************
!
!
!  Dynamic version of subroutine RUN. Initial conditions are specified
!  by the caller. Final conditions are returned to the caller.
!
!  This module sets up the call to the actual BLOOM II modules.
!  It is based upon similar, though not identical versions used in
!  JSBACH and the DELWAQ - BLOOM II coupling.
!
!  Boundary conditions used in the computation of constraints for
!  BLOOM II are determined by computations in other program modules
!  for example for nutrients.
!  The program returns several variables such as the total biomass
!  expressed in various units, which are not returned in the stand-alone
!  version of BLOOM II.
!
      SUBROUTINE DYNRUN(EXTTOT,EXTB,TMP,SOL,DEP,DAYL,CHLOR,ID,ISEG,
     1                  LCOUPL,NSET,EXTLIM,DEAT,TOTCHL,TOTDRY,TOTCAR)
      IMPLICIT REAL*8 (A-H,O-Z)

      INCLUDE 'blmdim.inc'
      INCLUDE 'putin1.inc'
      INCLUDE 'putin2.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'graas.inc'
      INCLUDE 'ioblck.inc'
      INCLUDE 'sumout.inc'
      INCLUDE 'dynam.inc'
      INCLUDE 'xvect.inc'
!
      CHARACTER*8 CDATE
      INTEGER NONUN(MT)
      REAL*8  ZOODII(0:MG)
!
!  Check whether a selective dump is requested for this period.
!
      IF (ISDUMP .EQ. 1) THEN
         IF (ID .GE. ISDPER(1) .AND. ID .LE. ISDPER(2)) THEN
            IDUMP = 1
         ELSE
            IDUMP = 0
         END IF
      END IF
!
!  Calculate solarradion level for week; correct for total radiadion.
!
      SOL=SOLACO * SOL
!
!  Calculate mixing depths of species.
!
      DO 110 K=1,NUSPEC
  110 DMIX(K)=SDMIX(K) * DEP
!
!  Construct date indicator.
!  Print heading for output on unit IOU(6) if "DUMP" is specified.
!  Update 1.91: allow large segment numbers: use I5
!
      WRITE (CDATE, 115) ISEG, ID
115   FORMAT (I4,1X,I2)
      IF ( IDUMP .EQ. 0) GO TO 120
      WRITE (IOU(6),99960) CDATE
!
!  Print parameter values on unit IOU(6) if "DUMP" is specified.
!
      WRITE (IOU(6),99950) TMP,SOL,DEP
  120 CONTINUE
!
!  Call subroutine BLOOM to set up and solve the linear programs
!  for week I; BLOOM will call all other subroutines
!  to solve the problem.
!  **** Update for ECOLUMN version:
!       TOTDRY (total dry weight) passed in position NUCOLS+2 of XDEF.
!
      DO 129 I=0,NUGRAZ
        ZOODII(I) = ZOOD(ID,I)
  129 CONTINUE
      CALL BLOOM(CDATE,ID,MI,TMP,SOL,CHLOR,EXTB,DAYL,DEAT,ZOODII,
     1           DEP,XINIT,XDEF,XECO,TOTCHL,EXTTOT,EXTLIM,NSET,INFEAS,
     2           NONUN,NUMUN,LCOUPL)
      TOTDRY = XDEF (NUCOLS+2)
      TOTCAR = 0.0
      DO 130 I = 1, NUSPEC
         TOTCAR = TOTCAR + XDEF(I+NUROWS)/CTODRY(I)
 130  CONTINUE
!
!  Print warning message if potential degenenarate solutions have been
!  detected.
!
!     IF (NUMUN .NE. 0 .AND. LPRINT .EQ. 1)
!    1    WRITE (IOU(21),99930) ID,(NONUN(K),K=1,NUMUN)
!
99980 FORMAT(2X,'The following species have reduced relative depth',/,
     1       2X,'for buoyancy control: ')
99970 FORMAT(2X,'Species ',A8,' has relative depth of ',F5.2)
99960 FORMAT (/,23X,'****** TIME PERIOD ',2X,A8,2X,'******',/)
99950 FORMAT(2X,'Important parameter values for this week:',/,
     1       2X,'Temperature =',F5.1,4X,'Solar radiation =',F8.1,
     2       4X,'Total depth =',F5.2)
99930 FORMAT (' Period: ',I4,' Potential degeneracy for species: ',20I3)
      RETURN
      END
