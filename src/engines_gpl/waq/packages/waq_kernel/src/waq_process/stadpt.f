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

      subroutine stadpt ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Depth-averaged, max and min value per timestep

!
!     Description of the module :
!
!        General water quality module for DELWAQ:
!
! Name    T   L I/O   Description                                  Units
! ----    --- -  -    -------------------                          -----
!
! CONC           I    Concentration of the substance              1
! VOLUME         I    Volume of the computational cells           2
!
! DPTAVG         O    Average over depth                          3
! DPTMAX         O    Maximum over depth                          4
! DPTMIN         O    Minimum over depth                          5
!

!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      IMPLICIT NONE

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
!
      INTEGER  IP1   , IP2   , IP3   , IP4   , IP5   ,
     +         IN1   , IN2   , IN3   , IN4   , IN5
      INTEGER  IPP2  , IPP3  , IPP4  , IPP5  , ISEGL , NOSEGL, LUNREP
      INTEGER  NOLAY , ILAY
      INTEGER  IKMRK , IKMRK1, IKMRK2, ISEG  , IQ    , IFROM , ITO
      REAL     VOLUME

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)

      IN1 = INCREM(1)
      IN2 = INCREM(2)
      IN3 = INCREM(3)
      IN4 = INCREM(4)
      IN5 = INCREM(5)

!
!     The averaging is independent of a time interval, as the outcome
!     is itself time-dependent (there is only a reduction in the
!     spatial coordinates)
!
!     Problem:
!     DELWAQ does not use explicit layer information. So:
!     - Assume that the segments are numbered per layer
!     - Assume that the exchanges are numbered per layer
!     - Then the number of layers is: NOQ1/(NOQ1-NOQ3)
!
!     Also: Simple check to catch obvious errors
!

      NOSEGL = NOSEG - NOQ3
      NOLAY  = NOSEG / NOSEGL

      IF ( NOSEG .NE. NOSEGL*NOLAY ) THEN
         CALL GETMLU( LUNREP )
         WRITE( LUNREP, * ) 'ERROR in STADPT'
         WRITE( LUNREP, * )
     &'Number of segments inconsistent with expected number of layers'
         WRITE( LUNREP, * )
     &'Segments, layers: ', NOSEG, NOLAY
         WRITE( *     , * ) 'ERROR in STADPT'
         WRITE( *     , * )
     &'Number of segments inconsistent with expected number of layers'
         WRITE( LUNREP, * )
     &'Segments, layers: ', NOSEG, NOLAY
         CALL SRSTOP( 1 )
      ENDIF

!
!     Initialise the output arrays
!     Assumption:
!     If a segment in the first layer is active, all others are active
!     as well. This need not be true for the layers below the first.
!
      DO 1000 ISEGL=1,NOSEGL
         CALL DHKMRK( 1, IKNMRK(ISEGL), IKMRK )
         IF ( IKMRK .NE. 0 ) THEN
            PMSA(IP3) = PMSA(IP1) * PMSA(IP2)
            PMSA(IP4) = PMSA(IP1)
            PMSA(IP5) = PMSA(IP1)
         ENDIF
         IP1       = IP1 + IN1
         IP2       = IP2 + IN2
         IP3       = IP3 + IN3
         IP4       = IP4 + IN4
         IP5       = IP5 + IN5
 1000 CONTINUE

      DO 9200 ISEGL=1,NOSEGL
!
!        The first layer is already done. So prepare for the next ...
!
         IP1 = IPOINT(1) + IN1*(ISEGL-1)
         IP2 = IPOINT(2) + IN2*(ISEGL-1)
         IP3 = IPOINT(3) + IN3*(ISEGL-1)
         IP4 = IPOINT(4) + IN4*(ISEGL-1)
         IP5 = IPOINT(5) + IN5*(ISEGL-1)

         IPP2= IP2
         IPP3= IP3
         IPP4= IP4
         IPP5= IP5

         VOLUME = PMSA(IPP2)

         DO 9000 ILAY=2,NOLAY

            IP1 = IP1 + IN1*NOSEGL
            IP2 = IP2 + IN2*NOSEGL

!
!           Only look at active segments
!
            CALL DHKMRK( 1, IKNMRK(ISEGL+(ILAY-1)*NOSEGL), IKMRK )
            IF ( IKMRK .EQ. 0 ) GOTO 9000

            VOLUME = VOLUME + PMSA(IP2)
            IF ( PMSA(IP2) .GT. 0.0 ) THEN
               PMSA(IPP3) = PMSA(IPP3) + PMSA(IP1) * PMSA(IP2)
               PMSA(IPP4) = MAX( PMSA(IPP4), PMSA(IP1) )
               PMSA(IPP5) = MIN( PMSA(IPP5), PMSA(IP1) )
            ENDIF

 9000    CONTINUE

         IF ( VOLUME .GT. 0.0 ) THEN
            PMSA(IPP3) = PMSA(IPP3) / VOLUME
         ENDIF

         IP3 = IPOINT(3) + IN3*(NOSEGL+ISEGL-1)
         IP4 = IPOINT(4) + IN4*(NOSEGL+ISEGL-1)
         IP5 = IPOINT(5) + IN5*(NOSEGL+ISEGL-1)

         DO 9100 ILAY=2,NOLAY

            PMSA(IP3) = PMSA(IPP3)
            PMSA(IP4) = PMSA(IPP4)
            PMSA(IP5) = PMSA(IPP5)

            IP3 = IP3 + IN3*NOSEGL
            IP4 = IP4 + IN4*NOSEGL
            IP5 = IP5 + IN5*NOSEGL

 9100    CONTINUE

 9200 CONTINUE

      RETURN
      END
