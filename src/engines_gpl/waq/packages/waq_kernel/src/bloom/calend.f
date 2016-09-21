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

!    Date:       12 Dec 1989
!    Time:       14:08
!    Program:    CALEND.FOR
!    Version:    1.11
!    Programmer: ??????
!    Previous version(s):
!    1.10 -- 12 Dec 1989 -- 11:12 -- Operating System: DOS
!    1.9 -- 12 Dec 1989 -- 11:11 -- Operating System: DOS
!    1.8 -- 12 Dec 1989 -- 11:11 -- Operating System: DOS
!    1.7 -- 12 Dec 1989 -- 11:10 -- Operating System: DOS
!    1.6 -- 12 Dec 1989 -- 11:10 -- Operating System: DOS
!    1.5 -- 12 Dec 1989 -- 11:09 -- Operating System: DOS
!    1.4 -- 12 Dec 1989 -- 11:08 -- Operating System: DOS
!    1.3 -- 12 Dec 1989 -- 11:07 -- Operating System: DOS
!    1.2 -- 12 Dec 1989 -- 11:07 -- Operating System: DOS
!    1.1 -- 12 Dec 1989 -- 10:49 -- Operating System: DOS
!    1.0 -- 12 Dec 1989 -- 10:48 -- Operating System: DOS
!    0.0 -- 12 Dec 1989 -- 10:18 -- Operating System: DOS
!
!  *********************************************************************
!  *    SUBROUTINE TO OBAIN THE CURRENT SYSTEM DATE                    *
!  *********************************************************************
!
      SUBROUTINE CALEND (DATUM,ITERM)
      CHARACTER*1 DATUM(1)
      CHARACTER*8 MMDDYY
      INTEGER STOS, STOSH
!
! Call LAHEY internal routine to get date from DOS.
!
! Note: ITERM is not referenced here, but it is in the mainframe
! version of this routine!
!
!     CALL DATE (MMDDYY)
      MMDDYY=' '
!
! Construct character variable DATUM.
!
      IRC = STOS ('Date:',1,5,datum,lend)
      IRC = STOSH (MMDDYY,1,8,DATUM,LEND+2,LEND2)
      RETURN
      END
