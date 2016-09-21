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

      subroutine staqtl ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Quantiles for a given substance during a given period

!
!     Description of the module :
!
! Name    T   L I/O   Description                                  Units
! ----    --- -  -    -------------------                          -----
!
! CONC           I    Concentration of the substance            1
! TSTART         I    Start of statistical period               2
! TSTOP          I    Stop of statistical period                3
! TIME           I    Time in calculation                       4
! DELT           I    Timestep                                  5
! NOBUCK         I    Number of buckets to use                  6
! CLOBND         I    Expected lower bound                      7
! CUPBND         I    Expected upper bound                      8
! CQLEV          I    Quantile (in %) to be reported            9
! TCOUNT       I/O    Count of times (must be imported!)       10
! CWRK01       I/O    First work array                    10 +  1
!    ...
! CWRKxx       I/O    Last work array (NOBUCK in total)   10 + NOBUCK
! CQUANT         O    Estimated quantile                  11 + NOBUCK
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
     +         IP11  ,
     +         IN1   , IN2   , IN3   , IN4   , IN5   ,
     +         IN6   , IN7   , IN8   , IN9   , IN10  ,
     +         IN11
      INTEGER  IKMRK , IKMRK1, IKMRK2, ISEG  , IQ    , IFROM , ITO
      INTEGER  IB    , IPBUCK, IPTCNT, LUNREP, ITYPE

      INTEGER  NOBUCK
      INTEGER  MAXBCK
      PARAMETER ( MAXBCK = 101 )
      INTEGER  IBUCK(MAXBCK), INCBCK(MAXBCK)
      REAL     BCKLIM(MAXBCK)
      REAL     BMIN  , BMAX  , BDIFF , BSUM
      REAL     PQUANT
      REAL     TSTART, TSTOP , TIME  , DELT  , TCOUNT

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)
      IP6 = IPOINT(6)
      NOBUCK= NINT(PMSA(IP6)) + 1
      IP7 = IPOINT(7)
      IP8 = IPOINT(8)
      IP9 = IPOINT(9)
      IP10= IPOINT(10)
      IP11= IPOINT(11+NOBUCK)
!
!     Names for indices that turn up in various places
!
      IPTCNT=IP10
      IPBUCK=10

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
      IN11= INCREM(11+NOBUCK)

      BMIN  = PMSA(IP7)
      BMAX  = PMSA(IP8)

!     IF ( NOBUCK .LT. 1 ) THEN
!        CALL GETMLU( LUNREP )
!        WRITE( LUNREP, * ) 'ERROR in STAQTL'
!        WRITE( LUNREP, * )
!    &'Number of buckets must be at least 1'
!        WRITE( LUNREP, * )
!    &'Number of buckets: ', NOBUCK
!        CALL SRSTOP( 1 )
!     ENDIF
      IF ( NOBUCK .GT. MAXBCK ) THEN
         CALL GETMLU( LUNREP )
         WRITE( LUNREP, * ) 'ERROR in STAQTL'
         WRITE( LUNREP, * )
     &'Number of buckets too large'
         WRITE( LUNREP, * )
     &'Number of buckets: ', NOBUCK-1, ' - maximum: ', MAXBCK-1
         CALL SRSTOP( 1 )
      ENDIF
!
!     IF ( BMIN .GE. BMAX ) THEN
!        CALL GETMLU( LUNREP )
!        WRITE( LUNREP, * ) 'ERROR in STAQTL'
!        WRITE( LUNREP, * )
!    &'Expected range inappropriate: minimum must lower than maximum'
!        WRITE( LUNREP, * )
!    &'Minimum: ', BMIN, ' - maximum: ', BMAX
!        CALL SRSTOP( 1 )
!     ENDIF

      BDIFF = ( BMAX - BMIN ) / REAL(NOBUCK-1)

      DO 110 IB = 1,NOBUCK
         IBUCK(IB)  = IPOINT(IPBUCK+IB)
         INCBCK(IB) = INCREM(IPBUCK+IB)
         BCKLIM(IB) = BMIN  + REAL(IB-1) * BDIFF
  110 CONTINUE

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
      TCOUNT = PMSA(IPTCNT)

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

      TCOUNT       = TCOUNT + DELT
      PMSA(IPTCNT) = TCOUNT

!
!     Determine the length of the period for the quantile
!     (only used if ITYPE is 3)
!
      PQUANT = PMSA(IP9) * 0.01 * TCOUNT

      DO 9000 ISEG=1,NOSEG
         IF (BTEST(IKNMRK(ISEG),0)) THEN

!
!        The first time is special. Initialise the arrays.
!        The last time requires additional processing.
!
         IF ( ITYPE .EQ. 1 ) THEN
            DO 2010 IB = 1,NOBUCK
               PMSA(IBUCK(IB)) = 0.0
 2010       CONTINUE
         ENDIF

         DO 2020 IB = 1,NOBUCK
            IF ( PMSA(IP1) .LE. BCKLIM(IB) ) THEN
               PMSA(IBUCK(IB)) = PMSA(IBUCK(IB)) + DELT
               GOTO 2030
            ENDIF
 2020    CONTINUE

 2030    CONTINUE

         IF ( ITYPE .EQ. 3 .AND. TCOUNT .GT. 0.0 ) THEN
!
!           Accumulate the values in the buckets until we add up
!           to at least the requested percentage of total time.
!           Then interpolate assuming a uniform distribution
!           within each bucket.
!           Special note:
!           If the ranges have been set wrongly, then the
!           outer buckets will contain the quantile. In that
!           case: set to -999.0
!
            PMSA(IP11) = -999.0
            BSUM   = PMSA(IBUCK(1))
            IF ( BSUM .LT. PQUANT ) THEN
               DO 7010 IB = 2,NOBUCK
                  BSUM = BSUM + PMSA(IBUCK(IB))
                  IF ( BSUM .GT. PQUANT ) THEN
                     PMSA(IP11) = BCKLIM(IB) -
     +                               (BSUM-PQUANT)*BDIFF/PMSA(IBUCK(IB))
                     GOTO 7020
                  ENDIF
 7010          CONTINUE
            ENDIF

 7020       CONTINUE
         ENDIF

         ENDIF

         IP1  = IP1  + IN1
         IP11 = IP11 + IN11

         DO 8010 IB = 1,NOBUCK
            IBUCK(IB)  = IBUCK(IB) + INCBCK(IB)
 8010    CONTINUE

 9000 CONTINUE

!
!     Be sure to turn off the statistical procedure, once the end has been
!     reached (by setting TCOUNT (PMSA(IP6)) to a non-positive value)
!
      IF ( ITYPE .EQ. 3 ) THEN
         PMSA(IPTCNT) = -TCOUNT
      ENDIF

      RETURN
      END
