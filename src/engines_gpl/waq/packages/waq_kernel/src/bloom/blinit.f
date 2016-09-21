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

!    Date:       7 Januari 1994
!    Program:    BLINIT.FOR
!    Version:    0.1
!    Programmer: Jos van Gils
!
!    Initialization
!
!    Called by: BLOOMC
!    Calls    : CVRBLM, HDRBLM

      SUBROUTINE BLINIT (LPRINO,LDUMPO)
!
!     Arguments
!
!     Name    Type  Length   I/O  Description
!
!     LPRINO  I     1        O    Saves original value of LPRINT
!     LDUMPO  I     1        O    Saves original value of IDUMP

      INTEGER         LPRINO, LDUMPO

!     Common block variables used
!
!     Name    Type  Length   I/O  Inc-file  Description
!
!     NUSPEC  I     1        I    phyt2     Number of types
!     NUNUCO  I     1        I    phyt2     Number of nutrients
!     NUFILI  I     1        I    phyt2     First position of EKX in A
!     NUABCO  I     1        I    phyt2     Last position of EKX in A
!     A       R*8   IA,MT    O    matri     System matrix Bloom
!     AA      R*8   MN,MT    I    phyt1     Stoichiometry matrix (g/gDW)
!     EKX     R*8   MT       I    phyt1     Specific extinctions, converted
!                                           to be (1/m/(gDW/m3))
!     LPRINT  I     1        I    sumout    Print flag
!     IDUMP   I     1        I    sumout    Print flag

      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'matri.inc'
      INCLUDE 'sumout.inc'
!
!     Local variables
!
!     Name    Type  Length   I/O  Description
!
!     J       I     1
!     I       I     1

      INTEGER         J    , I

!     Convert BLOOM II specific units to DLWQWQ specific units
!     (The module still converts SPEXDE, although this variable
!     is not used anymore)

      CALL CVRBLM
!
! Set A-matrix. Copy nutrient rows from AA (stochiometry matrix).
! Copy the extinction rows.
! Note: in steady state version of BLOOM the A matrix is updated each
! call of subroutine SETABC. This is not necessary now; the section
! in SETABC is skipped in the the dynamic version of the model.
!
      DO 80 J = 1,NUSPEC
         DO 75 I = 1,NUNUCO
            A(I,J) = AA(I,J)
   75    CONTINUE
   80 CONTINUE
      DO 90 J = 1,NUSPEC
         DO 85 I = NUFILI,NUABCO
            A(I,J) = EKX(J)
   85    CONTINUE
   90 CONTINUE
!
!  Call subroutine HDRBLM to write the headers for a number of output
!  files.
!
      IF (LPRINT .GT. 1) CALL HDRBLM

!  Save originals of print flags for later use in BLOUTC

      LPRINO = LPRINT
      LDUMPO = IDUMP
!
!  Exit
!
      RETURN
      END

