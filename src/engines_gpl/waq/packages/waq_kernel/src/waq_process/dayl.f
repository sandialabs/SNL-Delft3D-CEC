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

      subroutine dayl   ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Daylength calculation in hours

!
!     Description of the module :
!
!        Computes daylength in hours.
!        Formulea 6.2.7 from "Zonnestraling in Nederland",
!        C.A.Velds, Thieme/KNMI, 1992, 1st imp., ISBN 90-5210-140-X
!        General water quality module for DELWAQ:
!
! Name    T   L I/O   Description                                    Units
! ----    --- -  -    -------------------                            -----
! TIME    R*4 1 I  DELWAQ time in scu                              [scu]
! TREF    R*4 1 I  Refernce tim in days                              [d]
! RLAT    R*4 1 I  Latitude, north pos., south neg. [ radians ]   [grad]
!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      DOUBLE PRECISION LATITU, DECLIN, TEMP
      DOUBLE PRECISION SIN50M, E     , PI
      PARAMETER ( SIN50M = -1.454389765D-2 )
      PARAMETER ( E  = 1.721420632D-2 )
      PARAMETER ( PI = 3.141592654D0)
      LOGICAL  VARFLG

      IN1  = INCREM( 1)
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
      IN4  = INCREM( 4)
      IN5  = INCREM( 5)

      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
!
      VARFLG = .TRUE.
      IF ( IN1 .EQ. 0 .AND. IN2 .EQ. 0 .AND. IN3 .EQ. 0 .AND.
     +     IN4 .EQ. 0                                        ) THEN
!
         VARFLG = .FALSE.
!
         TIME    = PMSA( IP1 )
!        Conversion Latitude to rads
         LATITU  = PMSA( IP2 ) / 360 * 2 * PI
         TREF    = PMSA( IP3 )
         AUXSYS  = PMSA( IP4 )

!***********************************************************************
!****    Processes connected to the DAYLENGTH calculation
!***********************************************************************

!        Conversion time to daynumbers relative to tref
         DAYNR =  MOD (TIME / AUXSYS + TREF, 365.)

!        Computes declination of sun on day DAYNR.
         IF (( DAYNR .LT. 0.) .OR. ( DAYNR .GT. 365.)) THEN
            DECLIN = 9.9999D9
         ELSE
            DECLIN = 6.918D-3 -
     1               3.99912D-1 * DCOS ( E * DAYNR) -
     2               6.758D-3   * DCOS ( 2.0D0 * E * DAYNR) -
     3               2.697D-3   * DCOS ( 3.0D0 * E * DAYNR) +
     4               7.0257D-2  * DSIN ( E * DAYNR) +
     5               9.07D-4    * DSIN ( 2.0D0 * E * DAYNR) +
     6               1.480D-3   * DSIN ( 3.0D0 * E * DAYNR)
         ENDIF

!       Computes daylenth

         TEMP = (( SIN50M - DSIN ( DECLIN) * DSIN ( LATITU)) /
     &                    ( DCOS ( DECLIN) * DCOS ( LATITU)))

         IF ( TEMP .GT. 1.0) THEN
            TEMP   = 0.0
         ELSEIF ( TEMP .LT. -1.0) THEN
            TEMP   = 24.0
         ELSE
            TEMP   = 7.639437268D0 * ACOS ( TEMP)
         ENDIF
         TEMP = TEMP / 24.0
!
      ENDIF
!
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.GT.0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN

      IF ( VARFLG ) THEN
!
         TIME    = PMSA( IP1 )
!        Conversion Latitude to rads
         LATITU  = PMSA( IP2 ) / 360 * 2 * PI
         TREF    = PMSA( IP3 )
         AUXSYS  = PMSA( IP4 )

!***********************************************************************
!****    Processes connected to the DAYLENGTH calculation
!***********************************************************************

!        Conversion time to daynumbers relative to tref
         DAYNR =  MOD (TIME / AUXSYS + TREF, 365.)

!        Computes declination of sun on day DAYNR.
         IF (( DAYNR .LT. 0) .OR. ( DAYNR .GT. 365.)) THEN
            DECLIN = 9.9999D9
         ELSE
            DECLIN = 6.918D-3 -
     1               3.99912D-1 * DCOS ( E * DAYNR) -
     2               6.758D-3   * DCOS ( 2.0D0 * E * DAYNR) -
     3               2.697D-3   * DCOS ( 3.0D0 * E * DAYNR) +
     4               7.0257D-2  * DSIN ( E * DAYNR) +
     5               9.07D-4    * DSIN ( 2.0D0 * E * DAYNR) +
     6               1.480D-3   * DSIN ( 3.0D0 * E * DAYNR)
         ENDIF

!       Computes daylenth

         TEMP = (( SIN50M - DSIN ( DECLIN) * DSIN ( LATITU)) /
     &                    ( DCOS ( DECLIN) * DCOS ( LATITU)))

         IF ( TEMP .GT. 1.0) THEN
            TEMP   = 0.0
         ELSEIF ( TEMP .LT. -1.0) THEN
            TEMP   = 24.0
         ELSE
            TEMP   = 7.639437268D0 * ACOS ( TEMP)
         ENDIF
         TEMP = TEMP / 24.0
!
      ENDIF
!
      PMSA (IP5) = TEMP
!
      ENDIF
!
         IP1   = IP1   + IN1
         IP2   = IP2   + IN2
         IP3   = IP3   + IN3
         IP4   = IP4   + IN4
      IP5   = IP5   + IN5
!
 9000 CONTINUE

      RETURN
!
      END
