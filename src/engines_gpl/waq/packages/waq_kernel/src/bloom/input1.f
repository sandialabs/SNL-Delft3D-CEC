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
!  *   SUBROUTINE INPUT1 TO READ TITLE AND SOME LAKE-SPECIFIC INPUTS   *
!  *********************************************************************
!
!  0895 MvdV
!  read time series biomass for several grazer types
!
      SUBROUTINE INPUT1 (NDEC,INPU)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'putin1.inc'
      INCLUDE 'putin2.inc'
!
!  Note: this subroutine is NOT used by the coupled BLOOM II/CHARON
!  program.
!
!  Read year number and comments for particular run.
!
      READ (INPU,99999) IYEAR,(CASE(I),I=1,9)
      READ (INPU,99998) COM
!
!  Read number of weeks in the input data set
!
      READ (INPU,99997) NDEC
!
!  Read data for all weeks of the year:
!  Read: DAYLGT--day length; DEATH--death rate for weeks of year
!  Read: zooplankton dryweigth for weeks; depth for weeks.
!
!  0895 MvdV
!  read time series biomass for several grazer types
!
      DO 10 I=1,NDEC
      READ (INPU,99996) DAYLGT(I),DEATH(I),ZOOD(0,I),DEPTH(I),
     1     (ZOOD(J,I),J=1,MG)
   10 CONTINUE

99999 FORMAT (I4,1X,9A8)
99998 FORMAT (9A8,8X)
99997 FORMAT (I5,10X,F5.2)
99996 FORMAT (40F10.2)
      RETURN
      END
