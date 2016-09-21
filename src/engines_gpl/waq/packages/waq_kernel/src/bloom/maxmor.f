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

!    Date:       4 Nov 1992
!    Time:       14:25
!    Program:    MAXMOR.FOR
!    Version:    1.0
!    Programmer: Hans Los
!    Previous version(s):
!    0.0 -- 6 Jun 1989 -- 10:38 -- Operating System: DOS
!
!  *********************************************************************
!  *  SUBROUTINE TO CALCULATE THE REMAINING BIOMASSES FOR NEXT PERIOD  *
!  *  MvdV 961014 added subtraction of mortality due to grazing GRAMOR *
!  *  from mortality constraint                                        *
!  *********************************************************************
!
      SUBROUTINE MAXMOR(X,MI,EXTLIM,INFEAS,GRAMOR)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'matri.inc'
      INCLUDE 'dynam.inc'
      DIMENSION X(*),B2(MS),GRAMOR(MT)

!  MvdV 961014 added

!
! To (re)initialize the model, put all mortality constraints to 0.0
! Set EXTLIM  = 0.0
! If NREP = 1 also set X(I), I = NUROWS+1, NUROWS+NUSPEC to 0.0.
!
      IF (NREP .EQ. 1) THEN
         I1 = NUROWS
         J = NUEXRO + NUECOG
         DO 10 I = 1,NUECOG
            DO 5 K=IT2(I,1),IT2(I,2)
               I1 = I1 + 1
    5          X(I1) = 0.0D0
            J = J + 1
            B(J) = 0.0D0
   10    CONTINUE
         EXTLIM = 0.0
         RETURN
      END IF
!
      IF (INFEAS .EQ. 1) THEN
         J = NUEXRO + NUECOG
         DO 15 I = 1,NUECOG
            J = J + 1
            B(J) = 0.0D0
   15    CONTINUE
         EXTLIM = 0.0
         RETURN
      END IF
!
! Compute the mortality of each phytoplankton type, the total biomass
! of each species AFTER applying the mortality, and compute the
! extinction of living phytoplankton and detritus.
! The new, minimum biomass levels are stored in B2 AND in the original
! X-vector. This is to enable the program to deal with infeasible
! solutions.
!
!  Update nov 4 1992: don't divide by SDMIX. Questionable for species
!  with buoyancy regulation; definitaly incorrect for species at the
!  bottom.
!

!  MvdV 961014 added GRAMOR to RMORT to subtract mortality due to grazing
!              from the mortality constraint
      I1 = NUROWS
      EXDEAD = 0.0
      EXLIVE = 0.0
      DO 40 I=1,NUECOG
         SUMSP = 0.0
         DO 30 K=IT2(I,1),IT2(I,2)
            XDELT = 0.0
            I1 = I1 + 1
            IF (X(I1) .LT. 1.D-6) GO TO 20
            XDELT = X(I1) * DEXP(-MI * TSTEP * (RMORT(K) + GRAMOR(K)))
            SUMSP = SUMSP + XDELT
            EKXI = EKX (K) * XDELT
            EXDEAD = EXDEAD + QMREM * RMORT(K) * EKXI
*           EXLIVE = EXLIVE + EKXI/SDMIX(K)
            EXLIVE = EXLIVE + EKXI
   20       X(NUROWS + K) = XDELT
   30    CONTINUE
         B2(I) = DMAX1(SUMSP,0.0D0)
   40 CONTINUE
!
! Summarize the extinction of detritus and living phytoplankton
! obtaining EXTLIM: the minimum value of the planktonic extinction
! in the next time-step.
!
      EXTLIM = EXDEAD + EXLIVE
!
! Store the minimum biomass levels in the B-vector; if they have
! fallen below the toplevel (TOPLEV), release the mortality constraint.
!
      I1 = NUEXRO + NUECOG
      DO 50 I = 1,NUECOG
         I1 = I1 + 1
         IF (B2(I) .GT. TOPLEV) THEN
            B(I1) = B2(I)
         ELSE
            B(I1) = 0.0
         END IF
   50 CONTINUE
      RETURN
      END
