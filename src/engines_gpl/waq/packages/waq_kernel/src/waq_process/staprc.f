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

      subroutine staprc ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Exceedence frequency, its complement and the mean

!
!     Description of the module :
!
!        General water quality module for DELWAQ:
!
! Name    T   L I/O   Description                                  Units
! ----    --- -  -    -------------------                          -----
!
! CONC           I    Concentration of the substance              1
! TSTART         I    Start of statistical period                 2
! TSTOP          I    Stop of statistical period                  3
! TIME           I    Time in calculation                         4
! DELT           I    Timestep                                    5
! CCRIT          I    Critical value                              6
! ABOVE          I    Whether to register values above or below   7
!
! TCOUNT         O    Count of timesteps                          8
! CEXCD          O    Fraction of the time the value is exceeded  9
! CEXMN          O    Mean value over that period                10
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
     +         IP6   , IP7   , IP8   , IP9   , IP10  ,
     +         IN1   , IN2   , IN3   , IN4   , IN5   ,
     +         IN6   , IN7   , IN8   , IN9   , IN10
      INTEGER  IKMRK , IKMRK1, IKMRK2, ISEG  , IQ    , IFROM , ITO
      INTEGER  ITYPE
      REAL     TSTART, TSTOP , TIME  , DELT
      REAL     CCRIT , TCOUNT
      REAL     ABOVE , BELOW

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)
      IP6 = IPOINT(6)
      IP7 = IPOINT(7)
      IP8 = IPOINT(8)
      IP9 = IPOINT(9)
      IP10= IPOINT(10)

      IN1 = INCREM(1)
      IN2 = INCREM(2)
      IN3 = INCREM(3)
      IN4 = INCREM(4)
      IN5 = INCREM(5)
      IN6 = INCREM(6)
      IN7 = INCREM(7)
      IN8 = INCREM(8)
      IN9 = INCREM(9)
      IN10= INCREM(10)

!
!     There are five cases, defined by the time:
!                        TIME <  TSTART-0.5*DELT : do nothing
!     TSTART-0.5*DELT <= TIME <  TSTART+0.5*DELT : initialise
!     TSTART          <  TIME <  TSTOP           : accumulate
!     TSTOP           <= TIME <  TSTOP+0.5*DELT  : finalise
!     TSTOP+0.5*DELT  <  TIME                    : do nothing
!
!     (Use a safe margin)
!
      TSTART = PMSA(IP2)
      TSTOP  = PMSA(IP3)
      TIME   = PMSA(IP4)
      DELT   = PMSA(IP5)
      CCRIT  = PMSA(IP6)
      IF ( PMSA(IP7) .EQ. 1.0 ) THEN
         ABOVE  = DELT
         BELOW  = 0.0
      ELSE
         ABOVE  = 0.0
         BELOW  = DELT
      ENDIF

      TCOUNT = PMSA(IP8)

!
!      Start and stop criteria are somewhat involved. Be careful
!      to avoid spurious calculations (initial and final) when
!      none is expected.
!      Notes:
!      - The initial value for TCOUNT must be 0.0
!      - Time is expected to be the model time (same time frame
!        as the start and stop times of course)
!      - Check that the NEXT timestep will not exceed the stop time,
!        otherwise this is the last one
!
      ITYPE  = 0
      IF ( TIME .GE. TSTART-0.001*DELT ) THEN
         ITYPE = 2
         IF ( TCOUNT .EQ. 0.0 ) ITYPE = 1
      ENDIF
      IF ( TIME .GE. TSTOP-0.999*DELT ) THEN
         ITYPE  = 3
         IF ( TCOUNT .LE. 0.0 ) ITYPE = 0
      ENDIF

      IF ( ITYPE  .EQ. 0 ) RETURN

      TCOUNT    = TCOUNT + DELT
      PMSA(IP8) = TCOUNT

      DO 9000 ISEG=1,NOSEG
         IF (BTEST(IKNMRK(ISEG),0)) THEN

!
!        The first time is special. Initialise the arrays.
!        The last time requires additional processing.
!
         IF ( ITYPE .EQ. 1 ) THEN
            PMSA(IP9) = 0.0
            PMSA(IP10)= 0.0
         ENDIF

         IF ( PMSA(IP1) .GE. CCRIT ) THEN
            PMSA(IP9)  = PMSA(IP9) + ABOVE
            PMSA(IP10) = PMSA(IP10)+ ABOVE * PMSA(IP1)
         ELSE
            PMSA(IP9)  = PMSA(IP9) + BELOW
            PMSA(IP10) = PMSA(IP10)+ BELOW * PMSA(IP1)
         ENDIF

         IF ( ITYPE .EQ. 3 ) THEN
            IF ( TCOUNT .GT. 0.0 ) THEN
               IF ( PMSA(IP9) .GT. 0 ) THEN
                  PMSA(IP10)= PMSA(IP10)/PMSA(IP9)
               ENDIF
               PMSA(IP9) = PMSA(IP9) / TCOUNT
            ENDIF
         ENDIF

         ENDIF

         IP1  = IP1  + IN1
         IP9  = IP9  + IN9
         IP10 = IP10 + IN10

 9000 CONTINUE

!
!     Be sure to turn off the statistical procedure, once the end has been
!     reached (by setting TCOUNT (PMSA(IP6)) to a non-positive value)
!
      IF ( ITYPE .EQ. 3 ) THEN
         PMSA(IP8) = -TCOUNT
      ENDIF

      RETURN
      END
