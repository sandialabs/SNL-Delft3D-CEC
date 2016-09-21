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

!    Date:       30 December 1993
!    Program:    BLFILE.FOR
!    Version:    0.1
!    Programmer: Jos van Gils
!
!    Module that opens files for autonomous I/O of BLOOM
!
!    Called by: BLOOMC
!    Calls    : SETUNI

      SUBROUTINE BLFILE (RUNNAM)

!     Arguments
!
!     Name    Type  Length   I/O  Description
!
!     RUNNAM  C*(*) 1        I    Filename consisting of runid (no ext)
!
      CHARACTER*12   RUNNAM
!
!     Common block variables used
!
!     Name    Type  Length   I/O  Inc-file  Description
!
!     IOU     I     99       I    ioblck    Array of logical unit numbers
!
      INCLUDE 'ioblck.inc'
!
!     Local variables
!
!     Name    Type  Length   I/O  Description
!
!     IOST    I     1             I/O-status

      INTEGER      IOST

!
!  Call subroutine SETUNI to set I/O unit numbers for BLOOM II.
!
      CALL SETUNI
!
!  Open statement for BLOOM II input files.
!
      WRITE (RUNNAM(10:12),'(''frm'')')
      OPEN (IOU(12),FILE=RUNNAM,IOSTAT = IOST)
      IF (IOST .NE. 0) GOTO 901

      WRITE (RUNNAM(10:12),'(''d09'')')
      OPEN (IOU( 9),FILE=RUNNAM,IOSTAT = IOST)
      IF (IOST .NE. 0) GOTO 902
!
! Open BLOOM output file as unformatted, binary = transparent.
!
      WRITE (RUNNAM(10:12),'(''blm'')')
      OPEN (IOU(26),FILE=RUNNAM,
     &         FORM='UNFORMATTED', IOSTAT=IOST)
!     ENDFILE (IOU(26))
      IF (IOST .NE. 0) GOTO 903
!
!  Open statement for BLOOM II debug file.
!
      WRITE (RUNNAM(10:12),'(''dbg'')')
      OPEN (IOU(10),FILE=RUNNAM,IOSTAT = IOST)
      IF (IOST .NE. 0) GOTO 904

      RETURN

!     $Neatly process these error messages

  901 STOP 'BLFILE: Error opening .frm file'
  902 STOP 'BLFILE: Error opening .d09 file'
  903 STOP 'BLFILE: Error opening .blm file'
  904 STOP 'BLFILE: Error opening .dbg file'

      END
