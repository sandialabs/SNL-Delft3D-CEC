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

!     Module DELWAQ2:
!     - Encapsulate the interface of DELWQ2 and DLWQI0:
!       A, J and C are now pointers to arrays
!
      MODULE DELWAQ2

      CONTAINS

      SUBROUTINE DELWQ2 ( A, J, C, IMAXA, IMAXI, IMAXC, INIT,
     &                    ACTION, DLWQD                     )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!            DELWAQ - Deltares WAter Quality programme
!
!                     Version 4.30           june 1997
!                     Replaces:
!                     Version 4.22           feb 1997
!                     Version 4.21           jan 1997
!                     Version 4.20           may 1996
!                     Version 4.01           may 1994
!                     Version 4.00           september 1993
!                     Release 3.05           november 1991.
!                     Release 3.0 - RWS-DIV, june 1988.
!                     Release 2.0 of july     1984.
!                     Release 1.0 of december 1981.
!
!     INFORMATION   : Deltares
!                     L. Postma,
!                     Rotterdamse weg 185,
!                     P.O. Box 177,
!                     2600 MH Delft,
!                     Netherlands.
!                     telephone (31) 15-569353
!                     telefax   (31) 15-619674
!
!     FUNCTION      : Performs the waterquality simulations on a
!                     consistent set of binary intermediate files.
!
!     LOGICAL UNITS : LUNIN  , input , binary common-block file
!                       *    , output, user console file
!
!     SUBROUTINES CALLED : DLWQI0, initialises the system
!                          DLWQN1, first   integration procedure
!                          DLWQN2, second  integration procedure
!                          DLWQN3, third   integration procedure
!                          DLWQN4, fourth  integration procedure
!                          DLWQN5, fifth   integration procedure
!                          DLWQN6, sixth   integration procedure
!                          DLWQN7, seventh integration procedure
!                          DLWQN8, eighth  integration procedure
!                          DLWQN9, nineth  integration procedure
!                          DLWQNB, tenth   integration procedure
!                          DLWQNC, 11th    integration procedure
!                          DLWQND, 12th    integration procedure
!                          DLWQNE, 13th    integration procedure
!                          DLWQNA, 14th    integration procedure
!                          DLWQNF, 15th    integration procedure
!                          DLWQNG, 16th    integration procedure
!                          DLWQNH, 17th    integration procedure
!                          DLWQNI, 18th    integration procedure
!                          DLWQNJ, 19+20   integration procedure
!                          SRSTOP, stops execution
!                          DHOPNF, opens files
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH  DESCRIPTION
!     ---------------------------------------------------------
!     A       REAL     IMAXA   real      workspace array
!     J       INTEGER  IMAXJ   integer   workspace array
!     C       CHAR*20  IMAXC   character workspace array
!     DLWQD   TYPE(..) 1       derived type for persistent storage
!     IMAXA   INTEGER  1       maximum real      workspace array
!     IMAXI   INTEGER  1       maximum integer   workspace array
!     IMAXC   INTEGER  1       maximum character workspace array
!     INIT    LOGICAL  1       if T boot the system if F no initialisation
!     ACTION  INTEGER  1       indication of the action to be performed
!
      USE DIO_PLT_RW
      use grids
      USE DLWQI0_MOD
      USE Timers
      use m_timers_waq
      use m_couplib
      use delwaq2_data

      implicit none

      include 'actions.inc'
!
!     Declaration of arguments
!
      INTEGER       IMAXA , IMAXI , IMAXC
      INTEGER, DIMENSION(:), POINTER          :: J
      REAL, DIMENSION(:), POINTER             :: A
      CHARACTER(LEN=*), DIMENSION(:), POINTER :: C
      LOGICAL                                 :: INIT
      LOGICAL                                 :: exists
      INTEGER                                 :: ACTION
      TYPE(DELWAQ_DATA), TARGET               :: DLWQD
      type(GridPointerColl), pointer          :: GridPs               ! collection of all grid definitions

!
!     COMMON  /  SYSN   /   System characteristics
!
      INCLUDE 'sysn.inc'
!
!     COMMON  /  SYSI  /    Timer characteristics
!
      INCLUDE 'sysi.inc'
!
!     COMMON  /  SYSA   /   Pointers in real array workspace
!
      INCLUDE 'sysa.inc'
!
!     COMMON  /  SYSJ   /   Pointers in integer array workspace
!
      INCLUDE 'sysj.inc'
!
!     COMMON  /  SYSC   /   Pointers in character array workspace
!
      INCLUDE 'sysc.inc'
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH  DESCRIPTION
!     ---------------------------------------------------------
!     LUNIN   INTEGER  1       unit nummer of the common BOOT-file
!     IPAGE   INTEGER  1       pagelength for output in lines
!     NLUN    INTEGER  1       number of unit numbers
!     LCHMAX  INTEGER  1       length file names
!
!
!     Local declarations
!
      INTEGER, PARAMETER ::   LUNIN  =   914
      INTEGER, PARAMETER ::   IPAGE  =    64
      INTEGER, PARAMETER ::   NLUN   =    50
      INTEGER, PARAMETER ::   LCHMAX =   255
!
!           input structure for boot-file
!
      INTEGER, DIMENSION(INSIZE)  :: IN
      INTEGER, DIMENSION(IISIZE)  :: II
      EQUIVALENCE ( IN( 1) , NOSEG ) , ( II( 1) , ITSTRT )
!
      INTEGER, SAVE            :: LUN(NLUN)
      CHARACTER*(LCHMAX), SAVE :: LCHAR(NLUN)
      integer, save            :: filtype(nlun)
      CHARACTER*(LCHMAX), SAVE :: RUNID
      LOGICAL, SAVE            :: INIT2        = .TRUE. ! To suppress the start-up screen
      LOGICAL, SAVE            :: INIT_COUPLIB = .TRUE. ! Initialise the couplib library only once

!     Common to define external communications in SOBEK
!     OLCFWQ             Flag indicating ONLINE running of CF and WQ
!     SRWACT             Flag indicating active data exchange with SRW
!     RTCACT             Flag indicating output for RTC

      character*(lchmax)       :: inifil, dioconfig
      logical                  :: lfound
      integer                  :: idummy, ierr2
      real                     :: rdummy
      CHARACTER                :: cdummy
      CHARACTER*2              :: C2
      LOGICAL                  :: OLCFWQ, SRWACT, RTCACT, DDWAQ
      COMMON /COMMUN/             OLCFWQ, SRWACT, RTCACT, DDWAQ
!
      integer(4), save         :: ithndl = 0
!
!     Local variables
!
      INTEGER, SAVE            :: INDX
      INTEGER                  :: IERR
      INTEGER                  :: IMR
      INTEGER                  :: IMI
      INTEGER                  :: IMC
      INTEGER                  :: ILUN
      INTEGER                  :: IERRD
      INTEGER                  :: K
      LOGICAL                  :: NOLIC
!
      IF ( INIT ) THEN
         call timini ( )
         ! for openda-usage, where multiple instances are launched,
         ! the time module does not work correctly.
         if ( dlwqd%set_timer ) timon = .true.
         timon = .true.
         if (timon) call timstrt( "delwaq2", ithndl )
!
!        boot the system; read dimensions of sysn from delwaq03.wrk-file
!
         CALL DHGNAM(RUNID,'.mon')
         LCHAR(1) = TRIM(RUNID)//'-delwaq03.wrk'

!
!        produce a user-friendly message if the 03 work file is missing,
!        an indication that DELWAQ1 was not able to complete its job properly.
!
         inquire( file = lchar(1), exist = exists )
         if ( .not. exists ) then
             write( *, '(a)'  ) 'DELWAQ2 cannot run - the system work file is missing'
             write( *, '(2a)' ) '    File name: ', trim(lchar(1))
             write( *, '(2a)' ) '    Please check if DELWAQ1 ran correctly'
             call srstop( 1 )
         endif

!
!        the file does exist, so continue processing
!
         CALL DHOPNF ( LUNIN , LCHAR(1), 1     , 2     , IERR  )
         IF ( IERR .GT. 0 ) GOTO 999
         READ  ( LUNIN )   IN
         READ  ( LUNIN )   II
         READ  ( LUNIN )   IMR     , IMI  , IMC
         READ  ( LUNIN ) ( LUN    (K), K = 1, NOLUN )
         READ  ( LUNIN ) ( LCHAR  (K), K = 1, NOLUN )
         READ  ( LUNIN ) ( filtype(K), K = 1, NOLUN )
         DO 5 ILUN = 1, NOLUN
            CLOSE ( LUN(ILUN) )
    5    CONTINUE
         close(lunin)
!
!        start couplib
!
         if ( init_couplib ) then
             init_couplib = .false.
             call couplib_init(lunout=lun(19), idebug=0)
         endif
!
!        store number of processes and own process number in sysn
!
         mypart = myprc
         npartp = numprc
!
         if (mypart .gt. 1) then
! open lokale monitor-files voor uitvoer tijdens initialisatie
            write(lchar(19),'(a,i3.3,a)') 'part-', mypart,'.mon'
         end if

         CALL DHOPNF ( LUN(19) , LCHAR(19) , 19    , 1    , IERRD  )
         CALL SETMLU ( LUN(19) )

         IF (MYPART .EQ. 1) THEN

!      Initialise communication options SOBEK

            OLCFWQ = .FALSE.
            SRWACT = .FALSE.
            RTCACT = .FALSE.
            DDWAQ  = .FALSE.
            LCHAR(44) = ' '
            LUN(44)   = LUN(43) + 1

            call getcom ( '-i'  , 3    , lfound, idummy, rdummy,
     +                   inifil, ierr2)
            if ( lfound ) then
               if ( ierr2.ne. 0 ) then
                  inifil = ' '
               endif
            else
               inifil = 'delwaq.ini'
            endif
            open(lunin,file=inifil,status='old',err=123)
            write(lun(19),*) ' Using options from ini file : ',trim(inifil)
            call gkwini(lunin,'SimulationOptions','OnLineWQ',c2)
            if ( c2 .eq. '-1' ) then
               olcfwq = .true.
               write(lun(19),*) ' online coupling with FLOW module activated'
            endif
            call gkwini(lunin,'SimulationOptions','OutputRTC',c2)
            if ( c2 .eq. '-1' ) then
               rtcact = .true.
               write(lun(19),*) ' RTC coupling activated'
            endif
            call gkwini(lunin,'SimulationOptions','SRW',c2)
            if ( c2 .eq. '-1' ) then
               srwact = .true.
               write(lun(19),*) ' SRW coupling activated'
            endif
            call gkwini(lunin,'General','DIOConfigFile',dioconfig)
            call gkwini(lunin,'General','ProgressFile',lchar(44))
            close (lunin)
 123        continue

         ! check ddwaq from commandline

            call getcom ( '-d'  , 0    , lfound, idummy, rdummy,
     +           cdummy, ierr2)
            if ( lfound ) then
               DDWAQ = .TRUE.
               write(lun(19),*) ' DDWAQ coupling activated'
            endif

       ! initialise DIO

            IF ( OLCFWQ .OR. SRWACT .OR. RTCACT .OR. DDWAQ ) THEN
               if ( dioconfig .ne. ' ' ) then
                  write(lun(19),*) ' Using DelftIO ini file : ',trim(dioconfig)
                  CALL DIOINIT(dioconfig)
               else
                  write(lun(19),*) ' Using default DelftIO ini file'
                  CALL DIOINIT()
               endif
            ENDIF
!
!     The unlocking !
!
            IF ( INIT2 ) THEN
               INIT2 = .FALSE.
               IF ( NOQ3 .GT. 0 ) THEN
                  CALL UNLOCK (LUN(19),.TRUE.,NOLIC)
               ELSE
                  CALL UNLOCK (LUN(19),.FALSE.,NOLIC)
               ENDIF
            ENDIF

            IF (ACTION .EQ. ACTION_FULLCOMPUTATION) THEN
               WRITE(*,*)
               WRITE(*,'(A9,A)') '  runid: ',TRIM(RUNID)
               WRITE(*,*)
            ENDIF

            if ( nolic .and. noseg > 150 ) then
               write(*,'(//a)') 'Error: Authorisation problem'
               write(*,'(a)')   '       No valid license, so the number
     & of segments is limited to 150'
               call srstop(1)
            endif
         endif
!
!        end of reading master proces
!
!        initialize timers
!
         call timers_waq_init(lun(19))
         call couplib_timers_init(timer_start_couplib,
     +       couplib_max_timers, measr_idle=.false.)
!
         call timer_start(timer_total)
!
         call timer_start(timer_init)

! collaborative call to i0
         call sync_processes()
!
         IERR = 0
         gridps => dlwqd%gridps
         call dlwqi0 ( nlun   , a      , j      , c      , imaxa  ,
     &                 imaxi  , imaxc  , ipage  , lun    , lchar  ,
     &                 filtype, gridps , dlwqd  , ierr   )
!
         if (mypart .eq. 1) then
            CLOSE ( LUNIN )
            IF ( IERR .GT. 0 ) GOTO 992
!
!        end of initialisation
!
            WRITE ( * , *)
            WRITE ( * , * ) ' SIMULATION STARTED '
            WRITE ( * , * )
            WRITE ( * , * ) ' INTEGRATION ROUTINE =', intsrt
         endif
         call timer_stop(timer_init)
      ENDIF
!     SOBEK external communications ONLY implemented in scheme 10!
      IF ( OLCFWQ .OR. SRWACT .OR. RTCACT ) THEN
          IF ( INTSRT .NE. 10 .AND. (INTSRT .NE. 15 .AND. INTSRT .NE. 19) ) GOTO 991
      ENDIF

!
!     Store the local persistent variables
!
      DLWQD%II = II
      DLWQD%IN = IN

!
!     Check restrictions of Parallel computing
!
      IF ( NPARTp .GT. 1 ) THEN
!
!        Parallel computing only allowed for schemes 1, 5, 12
!
         IF ( INTSRT.NE.1 .AND. INTSRT.NE.5 .AND. INTSRT.NE.12 ) GOTO 993
!
!        Bottom layers not allowed in parallel runs
!
         IF ( NOQ4.GT.0 ) GOTO 994
!
!        Multiple grids not allowed in parallel runs
!
         IF ( NOGRID.GT.1 ) GOTO 995
!
!        Use of DelftIO coupling not allowed in parallel runs
!
         IF ( OLCFWQ .OR. SRWACT .OR. RTCACT .OR. DDWAQ ) GOTO 996
      ENDIF

!         branch to the appropriate integration option

      select case ( intsrt )

         case (  0 )     !      not transport, just processes
            call timer_start(timer_offs_intsrt+0)
            call dlwqn0 ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+0)

         case (  1 )     !      backward in space and time
            call timer_start(timer_offs_intsrt+1)
            call dlwqn1 ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+1)

         case (  2 )     !      modified 2nd order Runge Kutta
            call timer_start(timer_offs_intsrt+2)
            call dlwqn2 ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+2)

         case (  3 )     !      2nd order Lax Wendroff
            call timer_start(timer_offs_intsrt+3)
            call dlwqn3 ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+3)

         case (  4 )     !      Aternating direction implicit
            call timer_start(timer_offs_intsrt+4)
            call dlwqn4 ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+4)

         case (  5 )     !      Flux corrected transport
            call timer_start(timer_offs_intsrt+5)
            call dlwqn5 ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+5)

         case (  6 )     !      Direct steady state, backward differences in space
            call timer_start(timer_offs_intsrt+6)
            call dlwqn6 ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+6)

         case (  7 )     !      Direct steady state, central differences in space
            call timer_start(timer_offs_intsrt+7)
            call dlwqn7 ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+7)

         case (  8 )     !      Iteratively steady state, backward differences in space
            call timer_start(timer_offs_intsrt+8)
            call dlwqn8 ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+8)

         case (  9 )     !      Iteratively steady state, central differences in space
            call timer_start(timer_offs_intsrt+9)
            call dlwqn9 ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+9)

         case ( 10 )     !      Fully implicit, direct method, upwind
            call timer_start(timer_offs_intsrt+10)
            call dlwqnb ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+10)

         case ( 11 )     !      Horizontal explicit upwind, vertical implicit central
            call timer_start(timer_offs_intsrt+11)
            call dlwqnc ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+11)

         case ( 12 )     !      Horizontal explicit FCT   , vertical implicit central
            call timer_start(timer_offs_intsrt+12)
            call dlwqnd ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+12)

         case ( 13 )     !      Horizontal explicit upwind, vertical implicit upwind
            call timer_start(timer_offs_intsrt+13)
            call dlwqne ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+13)

         case ( 14 )     !      Horizontal explicit FCT   , vertical implicit upwind
            call timer_start(timer_offs_intsrt+14)
            call dlwqna ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+14)

         case ( 15 )     !      GMRES, horizontal upwind, vertical upwind
            call timer_start(timer_offs_intsrt+15)
            call dlwqnf ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+15)

         case ( 16 )     !      GMRES, horizontal upwind, vertical central
            call timer_start(timer_offs_intsrt+16)
            call dlwqng ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+16)

         case ( 17 )     !      stationary GMRES, horizontal upwind, vertical upwind
            call timer_start(timer_offs_intsrt+17)
            call dlwqnh ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+17)

         case ( 18 )     !      stationary GMRES, horizontal upwind, vertical central
            call timer_start(timer_offs_intsrt+18)
            call dlwqni ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+18)

         case ( 19 )     !      TRISULA-ADI 1 (vertically upwind)
            call timer_start(timer_offs_intsrt+19)
            call dlwqnj ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+19)

         case ( 20 )     !      TRISULA-ADI 2 (vertically central)
            call timer_start(timer_offs_intsrt+20)
            call dlwqnj ( a , j , c , lun , lchar, action, dlwqd, gridps )
            call timer_stop(timer_offs_intsrt+20)

         case ( 21 )     !      Self adjusting teta method (limiter Salezac)
            call dlwqnm ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 22 )     !      Self adjusting teta method (limiter Boris and Book)
            call dlwqnm ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 23 )     !      Leonards QUICKEST
            call dlwqno ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 24 )     !      Local flexible time step method by Leonard Postma
            call dlwqnp ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 25 )     !      Special for emission module
            call dlwqnq ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case default
            goto 990

      end select

      IF ( ACTION == ACTION_FINALISATION    .OR.
     &     ACTION == ACTION_FULLCOMPUTATION        ) THEN

!     print timer-results
!     Note: removed printing of timers to monitoring file

          call timer_stop(timer_total)
!         call timers_waq_print()

          call sync_processes()
          call couplib_stop()

          if ( timon ) then
             call timstop ( ithndl )
             call timdump ( TRIM(RUNID)//'-timers.out' )
             call timfinalize()
          endif

      endif

      return

  990 WRITE ( * , * ) ' ERROR: INTEGRATION OPTION NOT IMPLEMENTED'
      CALL SRSTOP(1)
  991 WRITE ( * , * ) ' ERROR: INTEGRATION OPTION NOT IMPLEMENTED in online mode'
      CALL SRSTOP(1)
  992 WRITE ( * , * ) ' ERROR : INITIALISATION FAILED'
      CALL SRSTOP(1)
  993 WRITE ( * , '(/,1x,a,i4,/)' )
     + ' INTEGRATION OPTION NOT IMPLEMENTED in parallel mode, npart=',npartp
      CALL SRSTOP(1)
  994 WRITE ( * , '(/,1x,a,i4,/)' )
     + ' BOTTOM LAYERS NOT SUPPORTED in parallel mode, npart=',npartp
      CALL SRSTOP(1)
  995 WRITE ( * , '(/,1x,a,i4,/)' )
     + ' MULTIPLE GRIDS NOT SUPPORTED in parallel mode, npart=',npartp
      CALL SRSTOP(1)
  996 WRITE ( * , '(/,1x,a,i4,/)' )
     + ' ON-LINE MODE NOT SUPPORTED in parallel mode, npart=',npartp
      CALL SRSTOP(1)
  999 WRITE ( * , * ) ' ERROR: NO VALID SET OF MODEL-INTERMEDIATE-FILES'
      CALL SRSTOP(1)
      END SUBROUTINE DELWQ2

      END MODULE DELWAQ2
