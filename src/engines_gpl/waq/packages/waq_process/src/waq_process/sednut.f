!!  Copyright (C)  Stichting Deltares, 2012-2020.
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

      subroutine sednut ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Sedimentation of nutrients in the organic carbon matrix (GEM)

!
!     Description of the module :
!
! Name    T   L I/O   Description                                    Units
! ----    --- -  -    -------------------                            -----
! SFL     R*4 1 I  sedimention flux organic                      [gX/m2/d]
! CN      R*4 1 I  CN ratio substance                             [gC/gN]
! CP      R*4 1 I  CP ratio substance                             [gC/gP]
! CS      R*4 1 I  CS ratio substance                             [gC/gS]
!
!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

!     Segment pointers en incrementen
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
      IP8  = IPOINT( 8)
      IP9  = IPOINT( 9)
      IP10 = IPOINT(10)
      IP11 = IPOINT(11)
      IP12 = IPOINT(12)
!
      IN1  = INCREM( 1)
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
      IN4  = INCREM( 4)
      IN5  = INCREM( 5)
      IN6  = INCREM( 6)
      IN7  = INCREM( 7)
      IN8  = INCREM( 8)
      IN9  = INCREM( 9)
      IN10 = INCREM(10)
      IN11 = INCREM(11)
      IN12 = INCREM(12)
!
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
!
      SFL    = PMSA(IP1 )
      CN     = PMSA(IP2 )
      CP     = PMSA(IP3 )
      CS     = PMSA(IP4 )
      DEPTH  = PMSA(IP5 )

!*******************************************************************************
!**** Processes connected to the SEDIMENTATION of nutrients in C-Matrix
!***********************************************************************

!     INITIALISATIE
      FSEDDN = 0.0
      FSEDDP = 0.0
      FSEDDS = 0.0

!     SEDIMENTATION

      IF ( CN .GT. 0.1 ) FSEDDN = SFL / CN
      IF ( CP .GT. 0.1 ) FSEDDP = SFL / CP
      IF ( CS .GT. 0.1 ) FSEDDS = SFL / CS
!
      PMSA ( IP7 ) = FSEDDN
      PMSA ( IP8 ) = FSEDDP
      PMSA ( IP9 ) = FSEDDS
!
      IF ( DEPTH .GT. 0.0 ) THEN
          FL( 1 + IFLUX ) =  FSEDDN /DEPTH
          FL( 2 + IFLUX ) =  FSEDDP /DEPTH
          FL( 3 + IFLUX ) =  FSEDDS /DEPTH
      ELSE
          FL( 1 + IFLUX ) =  0.0
          FL( 2 + IFLUX ) =  0.0
          FL( 3 + IFLUX ) =  0.0
      ENDIF
!
      ENDIF
      ENDIF
!
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + IN1
      IP2   = IP2   + IN2
      IP3   = IP3   + IN3
      IP4   = IP4   + IN4
      IP5   = IP5   + IN5
      IP7   = IP7   + IN7
      IP8   = IP8   + IN8
      IP9   = IP9   + IN9
!
 9000 CONTINUE

!.....Exchangeloop over de horizontale richting
      DO 8000 IQ=1,NOQ1+NOQ2

!........VxSedNut op nul (exchange uitvoer)
         PMSA(IP10) = 0.0
         PMSA(IP11) = 0.0
         PMSA(IP12) = 0.0

         IP10 = IP10 + IN10
         IP11 = IP11 + IN11
         IP12 = IP12 + IN12

 8000 CONTINUE
!.....initialisatie segment gerelateerde items
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
!
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
      IN4  = INCREM( 4)

!.....Startwaarden VxSedCMatrix
      IP6 = IP6 + ( NOQ1+NOQ2 ) * IN6

!.....Exchangeloop over de verticale richting
      DO 7000 IQ=NOQ1+NOQ2+1,NOQ1+NOQ2+NOQ3

         IVAN  = IEXPNT(1,IQ)

         IF ( IVAN .GT. 0 ) THEN
            VCMAT = PMSA( IP6 )
            CN    = PMSA( IP2 + (IVAN-1) * IN2 )
            CP    = PMSA( IP3 + (IVAN-1) * IN3 )
            CS    = PMSA( IP4 + (IVAN-1) * IN4 )

!...........Berekenen VxNut
            VN   = 0.0
            VP   = 0.0
            VS   = 0.0
            IF ( CN .GT. 0.0 ) VN   = VCMAT/CN
            IF ( CP .GT. 0.0 ) VP   = VCMAT/CP
            IF ( CS .GT. 0.0 ) VS   = VCMAT/CS

!...........VxNuts toekennen aan de PMSA
            PMSA(IP10) = VN
            PMSA(IP11) = VP
            PMSA(IP12) = VS
         ENDIF

!........Exchangepointers ophogen
         IP6 = IP6 + IN6
         IP10 = IP10 + IN10
         IP11 = IP11 + IN11
         IP12 = IP12 + IN12

 7000 CONTINUE
!
      RETURN
!
      END
