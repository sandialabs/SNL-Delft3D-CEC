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

!    Date:       4 Dec 1989
!    Time:       11:50
!    Program:    CVRBLM   FORTRAN
!    Version:    1.6
!    Programmer: Hans Los
!    Previous version(s):
!    1.5 -- 4 Dec 1989 -- 11:25 -- Operating System: CMS
!    1.4 -- 4 Dec 1989 -- 11:18 -- Operating System: CMS
!    1.3 -- 4 Dec 1989 -- 11:14 -- Operating System: CMS
!    1.2 -- 4 Dec 1989 -- 10:50 -- Operating System: CMS
!    1.1 -- 24 Oct 1989 -- 11:17 -- Operating System: CMS
!    1.0 -- 23 Oct 1989 -- 13:25
!    0.0 -- 3 Oct 1989 --  8:06
!
!  *********************************************************************
!  *  SUBROUTINE TO CONVERT UNITS BETWEEN BLOOM II AND ECOLUMN         *
!  *********************************************************************
!
!  *********************************************************************
!  *      SPECIAL ECOLUMN - BLOOM II PROGRAM VERSION                   *
!  *********************************************************************
!
!  This module converts some of the inputs of BLOOM II to enable the
!  program to use g/m3 rather than mg/m3 as basic concentration unit.
!  Notes:
!  1. No checks are made that all units are indeed correct! Thus it
!     is the user's responsibility to provide a consistent data set.
!  2. The output formats of BLOOM II are not changed. Thus some
!     numbers of some variables are printed in a rather akward format.
!     It is assumed, however, that ECOLUMN itself will handle all
!     essential BLOOM II outputs in the future.
!
      SUBROUTINE CVRBLM
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'dynam.inc'
!
! Assuming that concentration units in the calling program are g/m3,
! where as BLOOM II uses mg/m3, it is necessary to convert
! 1.  the specific extinction coefficients
! 2.  the carbon to chlorophyll ratio (and hence the dry weight to
!     chlorophyll ratio)
! of all phytoplankton types.
! The specific extinction coefficient of detritus.
! The base and top levels of the growth and mortality constraints.
!
      DO 10 I = 1, NUSPEC
         CHLTOC(I) = CHLTOC(I) * 1.0D-3
         CHLR(I)   = CHLTOC(I) * CTODRY(I)
         EKX(I)    = EKX(I)    * 1000.0D0
10    CONTINUE
      SPEXDE = SPEXDE * 1000.0D0
!     BIOBAS = BIOBAS * TSTEP * 1.0D-3
!     TOPLEV = TOPLEV * TSTEP * 1.0D-3
      BIOBAS = BIOBAS * 1.0D-3
      TOPLEV = TOPLEV * 1.0D-3
      RETURN
      END
