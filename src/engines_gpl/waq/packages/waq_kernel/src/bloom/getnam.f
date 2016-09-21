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
!    Program:    GETNAM.FOR
!    Version:    0.1
!    Programmer: Jos van Gils
!
!    Get names of types of algae and groups of algae
!
!    Called by: PDFBLO
!    Calls    : -

      SUBROUTINE GETNAM ( NTYPE , NAMTYP , NGROUP , NAMGRO )
!
!     Arguments
!
!     Name    Type  Length   I/O  Description
!
!     NTYPE   I     1        I    Number of species
!     NGROUP  I     1        I    Number of groups
!     NAMTYP  C*10  NTYPE    O    Names of species
!     NAMGRO  C*10  NGROUP   O    Names of groups
!
      INTEGER         NTYPE , NGROUP
      CHARACTER*10    NAMTYP(NTYPE), NAMGRO(NGROUP)

!     Common block variables used
!
!     Name    Type  Length   I/O  Inc-file  Description
!
!     MT      I     1        I    blmdim    Max. number of species
!     NUSPEC  I     1        I    phyt2     Actual number of species
!     SPNAME  C*8   MT       I    phyt1     Species names
!     MS      I     1        I    blmdim    Max. number of groups
!     NUECOG  I     1        I    phyt2     Actual number of groups
!     GRNAME  C*8   MS       I    phyt1     Group names
!     IT2     I     MS,2     I    phyt2     Administration of groups/types
!
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'

!     Local variables

      INTEGER     I,J,ITEL

      IF (NUSPEC.NE.NTYPE )
     JSTOP 'Getnam: Error 001, consult system manager.'
      IF (NUECOG.NE.NGROUP)
     JSTOP 'Getnam: Error 002, consult system manager.'

      DO 10 I = 1,NUSPEC
   10 NAMTYP(I)(1:8) = SPNAME(I)

      DO 20 J = 1,NUECOG
         NAMGRO(J)(1:8) = GRNAME(J)
         ITEL = 0
         DO 15 I = IT2(J,1), IT2(J,2)
            ITEL = ITEL + 1
            WRITE (NAMTYP(I)(9:10),'(I2.2)') ITEL
   15    CONTINUE
   20 CONTINUE

      RETURN

      END
