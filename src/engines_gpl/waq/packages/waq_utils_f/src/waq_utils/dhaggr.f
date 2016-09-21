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

      SUBROUTINE DHAGGR ( NOSEG1, NOSEG2, NOTOTI, NOTOTW, NOTOTH,
     +                    NOTOTO, ISYSI , ISYSW , ISYSH , ISYSO ,
     +                    IPGRID, IAGTYP, ARRINP, WEIGHT, ARRHLP,
     +                    ARROUT)
!
!     Deltares
!
!     Created             : June 1998 by Jan van Beek
!
!     Function            : Aggregates value to coarser grid
!
!     Subroutines called  : GETMLU, Get unit number report file
!                           SRSTOP, Stops execution
!                           ZERO  , Zero's a real array
!
!     Arguments           :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NOSEG1  INTEGER  1          INPUT   Number of segments on finer grid
!     NOSEG2  INTEGER  1          INPUT   Number of segments on coarser grid
!     IPGRID  INTEGER  NOSEG1     INPUT   Grid pointers to coarser grid
!     IAGTYP  INTEGER  1          INPUT   Aggregation type
!                                         1 = Accumulation
!                                         2 = Average
!                                         3 = Average weighted with WEIGHT
!     ARRINP  REAL     NOSEG1     INPUT   Array to be aggregated
!     WEIGHT  REAL     NOSEG1     INPUT   Weigth in averaging
!     ARRHLP  REAL     NOSEG2     LOCAL   Local help array
!     ARROUT  REAL     NOSEG2     OUTPUT  Aggregated array
!
!     Declaration of arguments
!
      INTEGER        NOSEG1, NOSEG2, NOTOTI, NOTOTW, NOTOTH,
     +               NOTOTO, ISYSI , ISYSW , ISYSH , ISYSO ,
     +               IAGTYP
      INTEGER        IPGRID(NOSEG1)
      REAL           ARRINP(NOTOTI,NOSEG1) , WEIGHT(NOTOTW,NOSEG1) ,
     +               ARRHLP(NOTOTH,NOSEG2) , ARROUT(NOTOTO,NOSEG2)
!
!     Local declaration
!
!     ISEG1   INTEGER  1          LOCAL   Segment index finer grid
!     ISEG2   INTEGER  1          LOCAL   Segment index coarser grid
!     LUREP   INTEGER  1          LOCAL   Unit number report file
!
      INTEGER        ISEG1 , ISEG2 , LUREP
!
!     Zero accumulation arrays
!
      IF ( IAGTYP .EQ. 1 ) THEN
         DO ISEG2 = 1 , NOSEG2
            ARROUT(ISYSO,ISEG2) = 0.0
         ENDDO
      ELSEIF ( IAGTYP .EQ. 2 .OR. IAGTYP .EQ. 3 ) THEN
         DO ISEG2 = 1 , NOSEG2
            ARROUT(ISYSO,ISEG2) = 0.0
            ARRHLP(ISYSH,ISEG2) = 0.0
         ENDDO
      ENDIF
!
!     Accumulate
!
      IF ( IAGTYP .EQ. 1 ) THEN
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               ARROUT(ISYSO,ISEG2) = ARROUT(ISYSO,ISEG2) +
     +                               ARRINP(ISYSI,ISEG1)
            ENDIF
         ENDDO
      ELSEIF ( IAGTYP .EQ. 2 ) THEN
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               ARROUT(ISYSO,ISEG2) = ARROUT(ISYSO,ISEG2) +
     +                               ARRINP(ISYSI,ISEG1)
               ARRHLP(ISYSH,ISEG2) = ARRHLP(ISYSH,ISEG2) + 1.0
            ENDIF
         ENDDO
      ELSEIF ( IAGTYP .EQ. 3 ) THEN
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               ARROUT(ISYSO,ISEG2) = ARROUT(ISYSO,ISEG2) +
     +                               ARRINP(ISYSI,ISEG1) *
     +                               WEIGHT(ISYSW,ISEG1)
               ARRHLP(ISYSH,ISEG2) = ARRHLP(ISYSH,ISEG2) +
     +                               WEIGHT(ISYSW,ISEG1)
            ENDIF
         ENDDO
      ELSE
         CALL GETMLU(LUREP)
         WRITE(LUREP,2000) IAGTYP
         CALL SRSTOP(1)
      ENDIF
!
!     Average
!
      IF ( IAGTYP .EQ. 2 .OR. IAGTYP .EQ. 3 ) THEN
         DO ISEG2 = 1 , NOSEG2
            IF ( ABS(ARRHLP(ISYSH,ISEG2)) .GT. 1.E-20 ) THEN
               ARROUT(ISYSO,ISEG2) = ARROUT(ISYSO,ISEG2) /
     +                               ARRHLP(ISYSH,ISEG2)
            ELSE
               ARROUT(ISYSO,ISEG2) = 0.0
            ENDIF
         ENDDO
      ENDIF
!
      RETURN
 2000 FORMAT ( ' ERROR: undefind aggregation type in DHAGGR :',I8 )
      END
