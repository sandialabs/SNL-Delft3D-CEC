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

      subroutine resdm  ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Resuspension total bottom material (dry mass)

!
!     Description of the module :
!
! Name    T   L I/O   Description                                    Units
! ----    --- -  -    -------------------                            -----
! DM1     R*4 1 I  amount dry matter in layer S1                     [gDM/m2]
! DM2     R*4 1 I  amount dry matter in layer S2                     [gDM/m2]
! DELT    R*4 1 I  DELWAQ timestep                                   [scu]
! DEPTH   R*4 1 I  depth water column                                  [m]
! FLRES1  R*4 1 O  resuspension flux DM from layer S1           [gDM/m2/d]
! FLRES2  R*4 1 O  resuspension flux DM from layer S2           [gDM/m2/d]
! IAUSYS  R*4 1 I  ratio between auxiliary and system clock unit       [-]
! MRDMS1  R*4 1 L  max. res. flux (contents of layer S1)        [gDM/m2/d]
! MRDMS2  R*4 1 L  max. res. flux (contents of layer S2)        [gDM/m2/d]
! MINDEP  R*4 1 I  minimal depth for resuspension                      [m]
! PRESS1  R*4 1 L  resuspension probability from S1 (0 - endless)      [-]
! PRESS2  R*4 1 L  resuspension probability from S2 (0 - endless)      [-]
! POTRES  R*4 1 L  potential resuspension flux                  [gDM/m2/d]
! FLRES1  R*4 1 L  resuspension flux DM from layer S1           [gDM/m2/d]
! FLRES2  R*4 1 L  resuspension flux DM from layer S2           [gDM/m2/d]
! TAU     R*4 1 I  calculated sheerstress                        [kg/m/s2]
! TAUVEL  R*4 1 I  total velocity calcualted from tau                [m/s]
! TCRRS1  R*4 1 I  critical sheerstress resuspension S1          [kg/m/s2]
! TCRRS2  R*4 1 I  critical sheerstress resuspension S2          [kg/m/s2]
! VCRRS1  R*4 1 I  critical velocity resuspension S1                 [m/s]
! VCRRS2  R*4 1 I  critical velocity resuspension S2                 [m/s]
! VRES    R*4 1 I  first order resuspensionrate constant             [1/d]
! VOLUME  R*4 1 I  volume computed by DELWAQ                          [m3]
! ZRES    R*4 1 I  zeroth order resuspension flux               [gDM/m2/d]

!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

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
      IP13 = IPOINT(13)
      IP14 = IPOINT(14)
      IP15 = IPOINT(15)
!
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
!

      DMS1    = PMSA(IP1 )
      DMS2    = PMSA(IP2 )
      ZRES    = PMSA(IP3 )
      VRES    = PMSA(IP4 )
      TAU     = PMSA(IP5 )
      TCRRS1  = PMSA(IP6 )
      TCRRS2  = PMSA(IP7 )
      DEPTH   = PMSA(IP8 )
      DELT    = PMSA(IP9 )
      MINDEP  = PMSA(IP10)
      SURF    = PMSA(IP11)

!*******************************************************************************
!**** Processes connected to the RESUSENSION
!***********************************************************************

      PRESS1 = 0.0
      PRESS2 = 0.0

!        Bereken de resuspensie kansen in S1
         IF (TAU .EQ. -1.0) THEN
              PRESS1 = 1.0
         ELSE
!           vergelijking met critische schuifspanning
             PRESS1 = MAX ( 0.0, (TAU/TCRRS1 - 1.0) )
         ENDIF

!        Bereken de resuspensie kansen in S2
         IF (TAU .EQ. -1.0) THEN
            PRESS2 = 1.0
         ELSE
!           vergelijking met critische schuifspanning
            PRESS2 = MAX ( 0.0, (TAU/TCRRS2 - 1.0) )
         ENDIF

!     BEREKENING RESUSPENSION

!     Green resuspension by ondiepe vakken
      IF ( DEPTH .LT. MINDEP) THEN
         FLRES1 = 0.0
         FLRES2 = 0.0
      ELSE

!        Resuspensie uit S1
         RFDMS1 = ZRES + ( VRES * DMS1 )

!        Testen of genoeg materiaal aanwezig is in laag 1
         MRDMS1 = MAX (0.0, DMS1 / DELT )

         FLRES1 = MIN ( RFDMS1 * PRESS1,  MRDMS1 )

!        If first layer is exhausted then resuspension from the second layer for the remaining of the timestep (DELTS2)

         IF ( RFDMS1*PRESS1 .GT. 1E-20 ) THEN
            DELTS2 = MAX(0.0,(1.-FLRES1/(RFDMS1*PRESS1))*DELT)
         ELSE
            DELTS2 = 0.0
         ENDIF

         RFDMS2 = ZRES + ( VRES * DMS2 )

!        Testen of genoeg materiaal aanwezig is
         MRDMS2 = MAX (0.0, DMS2 / DELT )

         FLRES2 = MIN ( RFDMS2 * PRESS2 * DELTS2/DELT , MRDMS2 )

      ENDIF

      PMSA (IP12) = FLRES1
      PMSA (IP13) = FLRES2
      PMSA (IP14) = PRESS1
      PMSA (IP15) = PRESS2

      ENDIF
      ENDIF
!
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
      IP4   = IP4   + INCREM (  4 )
      IP5   = IP5   + INCREM (  5 )
      IP6   = IP6   + INCREM (  6 )
      IP7   = IP7   + INCREM (  7 )
      IP8   = IP8   + INCREM (  8 )
      IP9   = IP9   + INCREM (  9 )
      IP10  = IP10  + INCREM ( 10 )
      IP11  = IP11  + INCREM ( 11 )
      IP12  = IP12  + INCREM ( 12 )
      IP13  = IP13  + INCREM ( 13 )
      IP14  = IP14  + INCREM ( 14 )
      IP15  = IP15  + INCREM ( 15 )
!
 9000 CONTINUE
!
      RETURN
!
      END
