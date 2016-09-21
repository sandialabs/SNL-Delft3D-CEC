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

!  *********************************************************************
!  *    SUBROUTINE FOR SETTING UP, SOLVING AND ARRANGING OUTPUT        *
!  *                     OF LINEAR PROGRAM.                            *
!  *********************************************************************
!
      SUBROUTINE SOLVLP(INOW,X,BIOMAX,IER,IRS,NONUNI,NUMUNI,LIB)
      IMPLICIT REAL*8 (A-H,O-Z)
!
! This subroutine sets up the A matrix, and B and C vectors for the
! linear programming routine QSLP.
! A considerable increase in speed is obtained by dropping all
! variables and constraints from consideration which are redundant
! for a particular extinction interval. These are:
! 1.  All types not considered in an interval.
! 2.  All constraints with a zero value. (i.e. Mortality constraints).
! 3.  All constraints with only zero A coefficients. (i.e. growth
!     constraints of species not permitted).
!
!
! When using DOSP change following dimension:
!     REAL*8 D(30,29), P(81)
!     INTEGER LIB(98)
!     PARAMETER (IA=29, ID=30)
! NOT necessary for DOSP:
!     REAL*8 XTMP(50),BTMP(29),ATMP(29,20)
!     INTEGER LIBTMP(50),LIBBAS(50),LSCTMP(29),LBASIS(50)
!
      INCLUDE 'blmdim.inc'
      INCLUDE 'ioblck.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'matri.inc'
      DIMENSION CTMP(MT),D(1),X(MX),P(1),XTMP(MX),ATMP(IA,MT),
     1          BTMP(IA)
      INTEGER LSC(IA),LSCTMP(IA),IOPT(4),IRS(3),LIB(MX),LIBTMP(MX),
     1        LIBBAS(MX),NONUNI(MT)
      SAVE    LSC, IOPT
      LOGICAL LBASIS (MX)
      DATA NSOLV /0/
!
!  Indicate additional run through the subroutine.
!
      NSOLV = NSOLV + 1
      IF (NSOLV .GT. 1) GO TO 80
!
! Set all constraints to less or equal LSC(I)=-1, except the minimum
! energy constraint.
! Put the B-value of the exclusion row to 0.0.
!
      INDEX = 0
      DO 10 I=1,NUEXRO
      INDEX = INDEX + 1
   10 LSC(INDEX)=-1
      LSC(NUFILI) = 1
      DO 20 I=1,NUECOG
      INDEX = INDEX + 1
   20 LSC(INDEX)=-1
      DO 30 I=1,NUECOG
      INDEX = INDEX + 1
   30 LSC(INDEX)=1
      B(NUEXRO) = 0.0D0
!
!     WRITE (6,19) (LSC(I),I=1,NUROWS)
!19   FORMAT (' LSC = ',20I4)
!
!  Set values for maximum number of iterations: IOPT(1).
!  Set number of iterations before checking for numerical errors:
!  IOPT(2)
!  Indicate that the original matrices do not have to be retored by
!  QSLP: IOPT(3)
!  NOTE: change minimum dimension of D to (NR+1)*(NC+1) when IOPT(3)
!  is NOT equal to 1!!
!  Indicate that the objective is to maximize: IOPT(4).
!
      IOPT(1)=50
      IOPT(2)=10
      IOPT(3)=1
      IOPT(4)=1
!
! Put coefficients for growth constraints into A-matrix.
!
      K = NUEXRO
      DO 50 I=1,NUECOG
      K = K + 1
      DO 40 J=IT2(I,1),IT2(I,2)
   40 A(K,J)=1.0
   50 CONTINUE
!
! Put coefficients for mortality constraints into A-matrix.
!
      DO 70 I=1,NUECOG
      K = K + 1
      DO 60 J=IT2(I,1),IT2(I,2)
   60 A(K,J)=1.0
   70 CONTINUE
!
! Determine which species might occur. Copy their C value into CTMP,
! count their number and set LBASIS = true.
!
   80 CONTINUE
      NUSTMP = 0
      DO 100 J = 1,NUSPEC
      IF (A(NUEXRO,J) .LT. 1.0D-6) THEN
         NUSTMP = NUSTMP + 1
         LBASIS(NUROWS+J) = .TRUE.
         CTMP(NUSTMP) = C(J)
      ELSE
         LBASIS(NUROWS+J) = .FALSE.
      END IF
  100 CONTINUE
!
! Determine which constraints might be active.
!
! Bug fix 08/25/2005 by Hans Los
! Nutrient contraints should never be dropped when levels become 0.
! Hence the first NUNUCO rows are now always considered by the
! optimization subroutine
!
! Drop a constraint if either
! 1. B(I) = 0.0 or
! 2. All A(I,J) coefficients are 0.0 for each J considered.
! If a "<" constraint is negative, the problem is infeasible. Skip
! calling QSLP and directly set relevant exit values.
!
! Copy relevant part of B vector into BTMP,
! count the number of potential constraint and set LBASIS = true.
! Set the conversion array LIBBAS to indicate original numbers of
! constraints to be considered. Copy relevant entries of LSC.
!
! Always include nutrient constraints
!
      NURTMP = 0
      DO 101 I = 1,NUNUCO
         NURTMP = NURTMP + 1
         LBASIS(I) = .TRUE.
         BTMP(NURTMP) = B(I)
         LIBBAS(NURTMP) = I
         LSCTMP(NURTMP) = LSC(I)
101   CONTINUE
!
! Next statement modified 08/25/2005
!     DO 110 I = 1,NUROWS
! Only for non-nutrient constraints
!
      DO 110 I = NUNUCO+1,NUROWS
      IF (B(I) .LT. -1.0D-12 .AND. LSC(I) .LE. 0) THEN
            IER = 100
            IRS(2) = 4
            IRS(3) = I
            GO TO 138
         END IF
      IF (B(I) .GT. 1.0D-12) THEN
         DO 105 J = 1,NUSPEC
            IF (.NOT. LBASIS (J+NUROWS)) GO TO 105
            IF (A(I,J) .GT. 1.0D-12) THEN
               NURTMP = NURTMP + 1
               LBASIS(I) = .TRUE.
               BTMP(NURTMP) = B(I)
               LIBBAS(NURTMP) = I
               LSCTMP(NURTMP) = LSC(I)
               GO TO 110
            END IF
105      CONTINUE
      END IF
      LBASIS(I) = .FALSE.
  110 CONTINUE
!
! Set the conversion array LIBBAS to indicate original numbers
! of variables to be considered.
! Contruct A-matrix for LP routine.
!
      J1 = 0
      DO 120 J = 1,NUSPEC
      IF (LBASIS(NUROWS+J)) THEN
         J1 = J1 + 1
         LIBBAS(NURTMP+J1) = NUROWS+J
         DO 115 I = 1,NURTMP
            ATMP(I,J1) = A(LIBBAS(I),J)
115      CONTINUE
      END IF
120   CONTINUE
!
! If a dump is requested, print the objective function.
!
      IF (IDUMP .EQ. 1 .AND. INOW .EQ. 1)
     *                  WRITE (IOU(6),125) (C(J),J=1,NUSPEC)
 125  FORMAT (' Objective function of types:',/,2X,20(F5.2,2X))
! ----------------------------------------------------------------------
!  AD HOC -- Print matrix tableau for simplex routine.
!
!     WRITE (15,999) NREP,INOW,NURTMP,NUSTMP
!999  FORMAT (' MATRIX TABLEAU IN SOLVLP. PERIOD = ',I3,' INTERVAL: ',
!    *        I3,' NURTMP = ',I3,' NUSTMP = ',I3)
!     DO 1000 I=1,NUROWS
!     DO 1000 I=1,NURTMP
!     WRITE (15,1010) (A(I,J),J=1,NUSPEC),B(I)
!     WRITE (15,1010) (ATMP(I,J),J=1,NUSTMP),BTMP(I)
!1000 CONTINUE
!1010 FORMAT (2X,20(D9.3,1X))
!     WRITE (15,1010) (CTMP(J),J=1,NUSPEC)
!     WRITE (15,1010) (CTMP(J),J=1,NUSTMP)
!     WRITE (15,1020) (LIBBAS(K),K=1,NURTMP+NUSTMP)
!1020 FORMAT (' LIBBAS = ',20(I3))
!
!  END AD HOC
! ----------------------------------------------------------------------
!
!  Call subroutine "QSLP" to solve the linear program by the ordinary
!  simplex algorithm.
!
      CALL QSLP(ATMP,IA,NURTMP,NUSTMP,BTMP,LSCTMP,CTMP,IOPT,IRS,LIBTMP,
     1     D,MT,XTMP,P,IER)
!
!  Put results in appropriate form. Construct X, and LIB as if the
!  complete problem had been solved by QSLP.
!
      IF (IER .EQ. 0) THEN
         IRS(3) = NUCOLS + 1
      ELSE
         IF ( IRS(3) .LE. MX ) THEN
            IRS(3) = LIBBAS(IRS(3))
         END IF
      END IF
!
!  Copy or set values for variables and constraints. Note: values
!  for ">" constraints, which were NOT considered by QSLP, are
!  incorrect. These values are, however, overriden in the next section.
!
      K = 0
      DO 126 I = 1, NUROWS
      IF (LBASIS(I)) THEN
         K = K + 1
         X(I) = XTMP(K)
         LIB(I) = LIBBAS(LIBTMP(K))
      ELSE
         LIB(I) = I
         X(I) = B(I)
      END IF
 126  CONTINUE
      DO 128 I = NUROWS+1, NUCOLS
      IF (LBASIS(I)) THEN
         K = K + 1
         X(I) = XTMP(K)
         LIB(I) = LIBBAS(LIBTMP(K))
      ELSE
         LIB(I) = I
         X(I) = 0.0D0
      END IF
 128  CONTINUE
      X(NUCOLS+1) = XTMP (NURTMP+NUSTMP+1)
!
! Compute slacks for ">" constraints not considered in QSLP.
! For the lower extinction contraint, use the information on the
! maximum extinction contraint.
! Put the mortality constraints not conisered by QSLP equal to the
! total biomass of each species.
! Note: if a species' growth constraint was not considered, its
! total biomass must be 0.0 so we do not have to compute it!
!
      X(NUFILI) = B(NUABCO) - X(NUABCO) - B(NUFILI)
      IF (LMORCH .NE. 1) GO TO 138
      K = NUEXRO + NUECOG
      DO 135 J = 1,NUECOG
      K = K + 1
      IF (LBASIS(K)) GO TO 135
      IF (.NOT. LBASIS(NUEXRO+J)) GO TO 135
         SUMX = 0.0
         DO 129 L = IT2(J,1),IT2(J,2)
            SUMX = SUMX + X(NUROWS+L)
 129     CONTINUE
      X(K) = SUMX
 135  CONTINUE
 138  CONTINUE
!     WRITE (15,1050) (X(K),K=1,NUCOLS+1)
!1050 FORMAT (' X-VECTOR: ',/,5(2X,F10.2))
!     WRITE (15,1060) (LIB(K),K=1,NUCOLS)
!1060 FORMAT (' LIB: ',20(I3))
!     WRITE (15,1070) IRS(3),X(IRS(3)),BIOMAX
!1070 FORMAT (' IRS(3) = ',I3,' X(IRS(3) = ',F10.3,' BIOMAX = ',F10.3)
      IF (IER .NE. 0) RETURN
!
!  Determine, which species have a reduced cost coefficient of 0.0:
!  these might have replaced one of the species in the optimal solution.
!  Note: use the resulting CTMP. Thisis only possible when IOPT(3) = 1!
!
      J1 = 0
      NUMUNI = 0
      DO 170 J = 1,NUSPEC
      NONUNI(J) = 0
      IF (.NOT. LBASIS (J+NUROWS)) GO TO 170
      J1 = J1 + 1
      IF (X(J+NUROWS) .GT. 1.0D-12) GO TO 170
      IF (CTMP(J1) .GT. 1.0D-12) GO TO 170
      NUMUNI = NUMUNI + 1
      NONUNI(NUMUNI) = J
  170 CONTINUE
!
! Compute maximum biomass of solution and store in BIOMAX.
! Note: this is NOT equal to X(NUCOLS + 1)
! when growth rather than biomass is maximized.
!
      IF (LOBFUN .EQ. 1) THEN
         BIOMAX = 0.0
         DO 180 J = 1,NUSPEC
         BIOMAX = BIOMAX + X (NUROWS + J)
 180     CONTINUE
      ELSE
         BIOMAX = X(NUCOLS+1)
      END IF
      RETURN
      END
