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
!    Program:    BLOUTC.FOR
!    Version:    0.1
!    Programmer: Jos van Gils
!
!    Set output controls for BLOOM
!
!    Called by: BLOOMC
!    Calls    : -

      SUBROUTINE BLOUTC (HISTOR,LPRINO,LDUMPO)
!
!     Arguments
!
!     Name    Type  Length   I/O  Description
!
!     HISTOR  L     1        I    Flag to activate output
!     LPRINO  I     1        I    Saves original value of LPRINT
!     LDUMPO  I     1        I    Saves original value of IDUMP

      LOGICAL         HISTOR
      INTEGER         LPRINO, LDUMPO

!     Common block variables used
!
!     Name    Type  Length   I/O  Inc-file  Description
!
!     LPRINT  I     1        I    sumout    Print flag
!     IDUMP   I     1        I    phyt2     Print flag

      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'sumout.inc'
!
!     Local variables
!
!     Name    Type  Length   I/O  Description
!
!     -
!
!     LPRINT and IDUMP are output control flags of Bloom
!     They have been read from the input and their value has been
!     saved in LPRINO and LDUMPU
!     Here we set them to their original values, ONLY if HISTOR is
!     true, that is for history elements at history times.
!     This is to avoid excessively sized output files of Bloom

      LPRINT = 0
      IDUMP  = 0
      IF (HISTOR) THEN
          LPRINT = LPRINO
          IDUMP  = LDUMPO
      ENDIF

      RETURN
      END

