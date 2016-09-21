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

!     This file contains the subroutines concerning the chlorinity
!     dependend mortality rates:
!     BLCLST - adapt the rates
!     BLCLRS - reset the rates
!     And a routine to set ppmax for a specific alg
!     BLSPPM - set ppmax

      SUBROUTINE BLCLST (MRTM1,MRTM2,MRTB1,MRTB2,NTYP_A,CL)

!     Arguments
!
!     Name    Type  Length   I/O  Description
!
!     MRTM1   R     NTYP_A   O    Original mortality rates
!     MRTM2   R     NTYP_A   I    M2 mort rate coeff
!     MRTB1   R     NTYP_A   I    B1 mort rate sal stress coeff
!     MRTB2   R     NTYP_A   I    B2 mort rate sal stress coeff
!
      INTEGER NTYP_A
      REAL    MRTM1(NTYP_A),MRTM2(NTYP_A),MRTB1(NTYP_A),MRTB2(NTYP_A),
     1        CL
!
!     Common block variables used
!
!     Name    Type  Length   I/O  Inc-file  Description
!
!     SIZE    I     ??       I/O  size      A.o.mortality rate in RMORT1
!
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
!
!     Local variables
!
!     Name    Type  Length   I/O  Description
!
!     IALG    I     1             counter over algae types

      INTEGER      IALG

!     Loop over algae types
      DO 10 IALG = 1, NTYP_A
!       Store the original value
        MRTM1(IALG) = RMORT1(IALG)
!       Salinity dep. mortality ??
        IF (MRTM2(IALG).GT.0.) THEN
          CL = MIN(CL,35000.)
          RMORT1(IALG) =  (MRTM2(IALG)-MRTM1(IALG))/
     1      (1.+EXP(MRTB1(IALG)*(CL-MRTB2(IALG))))+MRTM1(IALG)
        ENDIF
   10 CONTINUE

      RETURN

      END
      SUBROUTINE BLCLRS (MRTM1,NTYP_A)

!     Arguments
!
!     Name    Type  Length   I/O  Description
!
!     MRTM1   R     NTYP_A   O    Original mortality rates
!
      INTEGER NTYP_A
      REAL    MRTM1(NTYP_A)
!
!     Common block variables used
!
!     Name    Type  Length   I/O  Inc-file  Description
!
!     SIZE    I     ??       I/O  size      A.o.mortality rate in RMORT1
!
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
!
!     Local variables
!
!     Name    Type  Length   I/O  Description
!
!     IALG    I     1             counter over algae types

      INTEGER      IALG

!     Loop over algae types
      DO 20 IALG = 1, NTYP_A
!       Store the original value
        RMORT1(IALG)= MRTM1(IALG)
   20 CONTINUE

      RETURN

      END
      SUBROUTINE BLSPPM (IALG  , PPMAX )
!
!     Set PPMAX in BLOOM array for specific alg
!
!     Arguments
!
!     Name    Type  Length   I/O  Description
!
!     IALG    1     1        I    index alg involved
!     PPMAX   R     1        I    PPMAX value to be set
!
      INTEGER IALG
      REAL    PPMAX
!
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
!
      PMAX1(IALG) = PPMAX
!
      RETURN
      END
      SUBROUTINE BLSSDM (IALG  , SDMIXN )
!
!     Set SDMIX in BLOOM array for specific alg
!
!     Arguments
!
!     Name    Type  Length   I/O  Description
!
!     IALG    1     1        I    index alg involved
!     SDMIXN  R     1        I    SDMIX value to be set
!
      INTEGER IALG
      REAL    SDMIXN
!
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
!
      SDMIX(IALG) = SDMIXN
!
      RETURN
      END
