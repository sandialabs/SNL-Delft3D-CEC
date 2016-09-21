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

!    Date:       23 Oct 1989
!    Time:       13:24
!    Program:    SETUNI   FORTRAN
!    Version:    1.0
!    Programmer: Hans Los
!    (c) 1989 Deltares Sektor W&M
!    Previous versions:
!    0.0 -- 28 Sep 1989 -- 20:01
!
!  *********************************************************************
!  *          SUBROUTINE TO SET I/O UNIT NUMBERS FOR BLOOM II          *
!  *********************************************************************
!
!  *********************************************************************
!  *      SPECIAL NZBLOOM PROGRAM VERSION                              *
!  *********************************************************************
!
!  This module determines ALL I/O units for BLOOM II.
!  For historic reasons this subroutine
!  is configured such that BLOOM II units will be number 51 and up,
!  with the exception of units 5 and 6, which are the default console
!  units.
!
      SUBROUTINE SETUNI
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'ioblck.inc'
!
! In ECOLUMN/NZBLOOM version:
!
      IOUX = 50
      DO 10 I = 1, 30
         IOU (I) = I + IOUX
10    CONTINUE
      IOU(5) = 5
      IOU(6) = 6
!
! NZBLOOM: change IOU(10) to 70.
!
      IOU(10) = 70
!
! In ECOLUMN/NZBLOOM version:
!
      IOUX = 40
      DO 20 I = 41, 45
         IOU (I) = I + IOUX
20    CONTINUE
!
! In ECOLUMN/NZBLOOM version:
!
      IOUX = 30
      DO 30 I = 61, 69
         IOU (I) = I + IOUX
30    CONTINUE
!
!  Initialize (old) unit names previously set in various other
!  subroutines of BLOOM II.
!
      INUNI  = IOU(9)
      OUUNI  = IOU(10)
      IPL1 = IOU(41)
      IPL2 = IOU(42)
      IPL3 = IOU(43)
      OPL  = IOU(45)
!
! In PC version: use the standard BLOOM II file OUUNI also for EKOBLM
! BLOOM II and DLWQWQ, which use IOU(61), IOU(62), IOU(6) and
! IOU(3).
!
      IOU(61) = OUUNI
      IOU(62) = OUUNI
      IOU(6)  = OUUNI
      IOU(3)  = OUUNI
      RETURN
      END
