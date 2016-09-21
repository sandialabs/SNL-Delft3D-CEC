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

!    Version 0.3 16 August 2010
!    Version 0.2 22 July 1994
!    Version 0.1 7 Januari 1994
!    Program:    BLMORT.FOR
!    Programmer: Jos van Gils
!
!    Compute fluxes associated with mortality
!
!    Called by: BLOOMC
!    Calls    : NATMOR

      SUBROUTINE BLMORT (BIOMAS, TEMP  , FAUT  , FDET  , FLAUTN, FLDETN,
     J                   FLOOXN, FLMORA, DEAT4 , TSTEPI, LMIXO , LFIXN ,
     J                   LCARB , NUTCON, FLXCON)

      IMPLICIT NONE

!     Arguments
!
!     Name    Type  Length   I/O  Description
!
!     BIOMAS  R*4   NUSPEC   I    Biomass (gC/m3)
!     TEMP    R*4   1        I    Temperature (deg.C)
!     FAUT    R*4   NUSPEC   I    Fraction autolysis (-)
!     FDET    R*4   NUSPEC   I    Fraction detritus (-)
!     FLAUTN  R*4   4        O    Nutrient autolysis fluxes (g/m3/d)
!     FLDETN  R*4   4        O    Detritus production fluxes (g/m3/d)
!     FLOOXN  R*4   4        O    OOX production fluxes (g/m3/d)
!     FLMORA  R*4   NUSPEC   O    Algae mortality fluxes (gC/m3/d)
!     DEAT4   R*4   1        O    ??$Check necessity to transfer$
!     TSTEPI  R*4   1        I    Time step (d)
!     LMIXO   L     1        O    Flag mixotrophy
!     LFIXN   L     1        O    Flag N-fixation
!     LCARB   L     1        I    Flag carbon limitation
!     NUTCON  I*4   8        O    Nutrients involved in active nutrient constraints
!     FLXCON  I*4   8        O    Uptake fluxes involved in active nutrient constraints

      LOGICAL      LMIXO,LFIXN,LCARB
      INTEGER      NUTCON(*), FLXCON(*)

      REAL            BIOMAS(*), TEMP, FAUT(*), FDET(*), FLAUTN(*),
     J                FLDETN(*), FLOOXN(*), FLMORA(*), DEAT4, TSTEPI

!     Common block variables used
!
!     Name    Type  Length   I/O  Inc-file  Description
!
!     NUSPEC  I     1        I    phyt2     Number of types
!     RMORT   R*8   MT       O    size      Mortality rate (1/day)
!     AA      R*8   MN,MT    I    phyt1     Stoichiometry matrix (g/gDW)
!     CTODRY  R*8   MT       I    size      Conversion (gDW/gC)

      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'size.inc'
!
!     Local variables
!
!     Name    Type  Length   I/O  Description
!
!     TEMP8   R*8   1             Temperature (deg.C)
!     DEAT    R*8   1             ??
!     ZOODD   R*8   1             Dummy??$Check$
!     CPHYT   R*4   1             Biomass (gC/m3)
!     CMORT   R*4   1             Mortality flux (gC/m3/d)
!     CMORTA  R*4   1             Autolysis flux (gC/m3/d)
!     CMORTD  R*4   1             Detritus prod. (gC/m3/d)
!     CMORTO  R*4   1             OOx production (gC/m3/d)
!     J       I     1

      REAL            CMORT , CMORTA, CMORTD, CMORTO, CPHYT
      REAL*8          FOOX  , TEMP8 , ZOODD , DEAT
      INTEGER         I, J, K
!
!  Zero fluxes
!
      DO 1 J = 1,4
         FLAUTN(J) = 0.0
         FLDETN(J) = 0.0
         FLOOXN(J) = 0.0
    1 CONTINUE
!
!  Call subroutine NATMOR: calculate natural mortality rate constants.
!
      DEAT  = 0D0
      ZOODD = 0D0
      TEMP8 = DBLE(TEMP)
      CALL NATMOR ( DEAT  , ZOODD , TEMP8 , 1)
      DEAT4 = SNGL(DEAT)
!
!  Mortality module.
!
!  Objective: obtain nutrient fluxes to detritus, OOx and dissolved
!  nutrient pools due to mortality.
!
!  Again note that nutrient fluxes are computed from BLOOM's
!  stochiometry matrix and hence follow from biomasses in units dry
!  weight. The biomass mortality flux for DLWQWQ, however, is in units
!  of carbon.
!
!  Loop over algae species

      DO J=1,NUSPEC
         CPHYT = MAX ( BIOMAS(J) , 0.0 )

!  Compute total mortality for this species and store the flux
!  JvG 16-8-2010 avoid undershoots leading to negative biomass

         CMORT = MIN ( CPHYT * SNGL(RMORT(J)) , CPHYT/TSTEPI )
         FLMORA(J) = CMORT
!
! Partition the mortality flux over detritus(D)/OOx(O)/autolysis(A)
!
         FOOX   = (1. - FAUT(J) - FDET(J))
         CMORTA = CMORT * FAUT(J)
         CMORTD = CMORT * FDET(J)
         CMORTO = CMORT * FOOX
!
! Detritus production for C, N, P, Si (for C including part autolysis)
! Autolysis for C, N, P, Si (NOT for carbon)
! OOx production for C, N, P, Si (for C including part autolysis)
!
         FLDETN(1) = FLDETN(1) + CMORTD + CMORTA *FDET(J)/(FDET(J)+FOOX)
         FLOOXN(1) = FLOOXN(1) + CMORTO + CMORTA * FOOX / (FDET(J)+FOOX)
         DO K=1,NUNUCO
            I = NUTCON(K)
            IF (I.LE.3) THEN
            FLDETN(I+1) = FLDETN(I+1) + CMORTD * SNGL(CTODRY(J)*AA(K,J))
            FLAUTN(I+1) = FLAUTN(I+1) + CMORTA * SNGL(CTODRY(J)*AA(K,J))
            FLOOXN(I+1) = FLOOXN(I+1) + CMORTO * SNGL(CTODRY(J)*AA(K,J))
            ENDIF
         ENDDO

      ENDDO

      RETURN
      END

