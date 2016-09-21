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
!  *********************************************************************
!  *    SUBROUTINE TO CALCULATE MAXIMAL PRIMARY PRODUCTION AND         *
!  *                  RESPIRATION RATES                                *
!  *********************************************************************
!
      SUBROUTINE MAXPRD(T)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'ioblck.inc'
!
!  Calculate respiration rate constants.
!  Calculate the maximum gross growth rate per day as a linear or
!  exponential function of the temperature T.
!  It is assumed that the input function should be incremented with
!  the respiration rate constant. As an option the mortality rate
!  constant can be added as well (LPMORT=1).
!  As another option, Pmax can be set to some small number (BASMOR) if
!  the temperature is below the value stored in TEMLIM.
!  If, however, NREP is 1, the temperature limitation option is ignored
!  because otherwise the model does not have an intial solution for the
!  first time step.
!
      IF (LTLIM .EQ. 0) GO TO 20
      IF (T .GE. TEMLIM) GO TO 20
      IF (NREP .EQ. 1) GO TO 20
         DO 10 I = 1,NUSPEC
         RESP(I) = RES1(I) * RES2(I) ** T
         PMAX(I) = BASMOR
   10    CONTINUE
         GO TO 40
   20 CONTINUE
      DO 30 I = 1,NUSPEC
      RESP(I) = RES1(I) * RES2(I) ** T
      IF (LPMAX(I) .EQ. 0) THEN
        PMAX(I) = (PMAX1(I) * PMAX2(I) ** T) + RESP(I) + LPMORT*RMORT(I)
      ELSE
        IF (T .LE. PMAX2(I)) THEN
           PMAX(I) = 0.01
        ELSE
          PMAX(I) = PMAX1(I) * (T - PMAX2(I)) + RESP(I) +LPMORT*RMORT(I)
        END IF
      END IF
   30 CONTINUE
   40 CONTINUE
!
!  If option "DUMP" was turned on, print PMAX, RESP and RMORT.
!
      IF (IDUMP .NE. 0) THEN
         WRITE (IOU(6),50) (PMAX(K),K=1,NUSPEC)
   50    FORMAT ('  Pmax(T,j): ',10(F5.2,1X))
         WRITE (IOU(6),60) (RESP(K),K=1,NUSPEC)
   60    FORMAT ('  Resp(T,j): ',10(F5.2,1X))
         WRITE (IOU(6),70) (RMORT (K),K=1,NUSPEC)
   70    FORMAT ('  Rmort(T,j): ',10(F5.2,1X))
         WRITE (IOU(6),80) (SDMIX (K),K=1,NUSPEC)
   80    FORMAT ('  Sdmix(j): ',10(F5.2,1X))
      END IF
      RETURN
      END
