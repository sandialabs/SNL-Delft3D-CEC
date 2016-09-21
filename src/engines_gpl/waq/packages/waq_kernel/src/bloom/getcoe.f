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

!    Date:       5 Augustus 1994
!    Program:    GETCOE.FOR
!    Version:    0.1
!    Programmer: Jos van Gils
!
!    Get characteristices of algae
!
!    Called by: PDFBLO
!    Calls    : -

      SUBROUTINE GETCOE (NTYPE , DMCRAT, NCRAT , PCRAT , SCRAT , SPEXT )
!
!     Arguments
!
!     Name    Type  Length   I/O  Description
!
!     NTYPE   I     1        I    Number of species
!     DMCRAT  R     NTYPE    O    DM/C Ratio
!     NCRAT   R     NTYPE    O    N/C Ratio
!     PCRAT   R     NTYPE    O    P/C Ratio
!     SCRAT   R     NTYPE    O    SI/C Ratio
!     SPEXT   R     NTYPE    O    Specific extinction (m2/gC)
!
      INTEGER         NTYPE
      REAL            DMCRAT(NTYPE), NCRAT(NTYPE), PCRAT(NTYPE),
     J                SCRAT(NTYPE), SPEXT(NTYPE)

!     Common block variables used
!
!     Name    Type  Length   I/O  Inc-file  Description
!
!     MN      I     1        I    blmdim    Max. number of nutrients
!     MT      I     1        I    blmdim    Max. number of species
!     NUSPEC  I     1        I    phyt2     Actual number of species
!     MS      I     1        I    blmdim    Max. number of groups
!     NUECOG  I     1        I    phyt2     Actual number of groups
!     AA      R*8   MN,MT    I    phyt1     Stoichiometry matrix (g/gDW)
!     CTODRY  R*8   MT       I    size      Conversion (gDW/gC)
!     EKX     R*8   MT       I    phyt1     Specific extinction
!                                           (1/m/(gDW/m3))
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'size.inc'

!     Local variables

      INTEGER     I

      IF (NUSPEC.NE.NTYPE )
     JSTOP 'Getcoe: Error 001, consult system manager.'

      DO 10 I = 1,NUSPEC
         DMCRAT(I) =           CTODRY(I)
         NCRAT(I)  = AA(1,I) * CTODRY(I)
         PCRAT(I)  = AA(2,I) * CTODRY(I)
         SCRAT(I)  = AA(3,I) * CTODRY(I)
         SPEXT(I)  = EKX(I)  * CTODRY(I)
   10 CONTINUE

      RETURN

      END
