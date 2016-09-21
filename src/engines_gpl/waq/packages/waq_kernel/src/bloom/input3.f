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
!  *     SUBROUTINE INPUT3 TO READ LAKE-SPECIFIC INPUTS                *
!  *********************************************************************
!
      SUBROUTINE INPUT3 (NDEC,INPU)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'putin1.inc'
      INCLUDE 'putin2.inc'
      INCLUDE 'phyt2.inc'
!
!  Note: this subroutine is NOT used by the coupled BLOOM II/CHARON
!  program.
!
!
!  Read lake specific inputs for each week from unit 11:
!  Read: DATE--week name; IDEC--week position; TEMP--water
!   temperature; SOLINT--solar radiation level; PHYT--chlorophyll
!   concentration; CONNUT--nutrient concentrations;
!   BACKGR--background extinction.
!
      DO 10 I=1,NDEC
   10 READ (INPU,99999) DATE(I),IDEC(I),TEMP(I),SOLINT(I),PHYT(I),
     1                (CONNUT(I,K),K=1,NUNUCO),BACKGR(I)
99999 FORMAT (A4,1X,I5,7F10.0)
      RETURN
      END
