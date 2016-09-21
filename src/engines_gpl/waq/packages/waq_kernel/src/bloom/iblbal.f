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

      SUBROUTINE IBLBAL( NTYP_M, NTYP_A, ALGTYP, IPOINT)
!
!     Function : set common CBLBAL communication with balance routines
!     Jos van Gils, May 2011: bug fix for N-fixers and heterotrophs
!
!     NTYP_M    INPUT   Max number of types
!     NTYP_A    INPUT   Actual number of types
!     ALGTYP    INPUT   Characteristics per algae type
!     IPOINT    INPUT   pointers to bloom algea concentration array
!
      INTEGER         NTYP_M, NTYP_A
      INTEGER         IPOINT(NTYP_A)
      REAL            ALGTYP(0:20,NTYP_M)

!                     index  4 is NC-ratio
!                     index  5 is PC-ratio
!                     index 16 is NC-ratio detritus uptake
!                     index 17 is PC-ratio detritus uptake
!                     index 18 is NC-ratio N fixers
!
      INCLUDE 'cblbal.inc'
      INCLUDE 'sysa.inc'
!
      NTYPA2 = NTYP_A
      DO IALG = 1 , NTYP_A
         IBLSUB(IALG) = IPOINT(IALG) - ICONC + 1
!         NCRALG(IALG) = ALGTYP(4,IALG)
         NCRALG(IALG) = MAX(ALGTYP(4,IALG),0.0)
     J                + MAX(ALGTYP(16,IALG),0.0)
     J                + MAX(ALGTYP(18,IALG),0.0)
!         PCRALG(IALG) = ALGTYP(5,IALG)
         PCRALG(IALG) = MAX(ALGTYP(5,IALG),0.0)
     J                + MAX(ALGTYP(17,IALG),0.0)
      ENDDO
!
      RETURN
      END
      BLOCK DATA BBLBAL
!
!     Function : set common CBLBAL for the case no bloom
!
      INCLUDE 'cblbal.inc'
!
      DATA   NTYPA2 / 0 /
!
      END
