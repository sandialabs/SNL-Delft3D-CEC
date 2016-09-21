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

      subroutine dlwqn1 ( a     , j     , c     , lun   , lchar  ,
     &                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         First order upwind in space and time (1)
!>
!>                         Performs first order explicit time integration using
!>                         upwind discretization in space. The method is explict
!>                         and thus has a time step stability constraint.

!     CREATED            : june 1988 by L. Postma

!     LOGICAL UNITS      : LUN(19) , output, monitoring file
!                          LUN(20) , output, formatted dump file
!                          LUN(21) , output, unformatted hist. file
!                          LUN(22) , output, unformatted dump file
!                          LUN(23) , output, unformatted dump file

!     SUBROUTINES CALLED : DLWQTR          , user transport routine
!                          PROCES          , DELWAQ proces system
!                          DLWQO2          , DELWAQ output system
!                          DLWQT0          , sets time functions
!                          DLWQ13          , system postpro-dump routine
!                          DLWQ14          , scales waterquality
!                          DLWQ15          , wasteload routine
!                          DLWQ16          , transport routine
!                          DLWQ17          , boundary routine
!                          DLWQ18          , integration step
!                          DLWQ41          , updates volumes to new time level
!                          DLWQCE          , closure error correction
!                          dryfld          , detect drying and flooding from volumes
!                          dryfle          , detect drying and flooding from flows
!                          MOVE            , moves one array to another
!                          PROINT          , integration of fluxes
!                          SETSET          , variable grid settings
!                          ZERCUM          , zero's the cummulative array's
!                          BOUNDIO         , hand to external boundary resolve
!               Delwaq system timer:
!                          timstrt         , start of Delwaq system timer
!                          timstop         , stop of Delwas system timer
!               Some timer by someone:
!                          CPU_TIME, Fortran timer routine
!               VORTECH parallel timer and communication routines
!                          timer_start     , VORTECH parallel timer start
!                          timer_stop      , VORTECH parallel timer stop
!                          update_rdata    , VORTECH parallel data updat
!                          collect_rdata   , VORTECH parallel data collect
!                          distribute_rdata, VORTECH parallel data distribution

      use grids
      use timers
      use m_couplib
      use m_timers_waq
      use delwaq2_data
      use m_openda_exchange_items, only : get_openda_buffer
      use report_progress
      use waqmem          ! module with the more recently added arrays

      implicit none

!     Parameters         :

!     kind           function         name                Descriptipon

      real     ( 4), intent(inout) :: a    (*)          !< System total real array space
      integer  ( 4), intent(inout) :: j    (*)          !< System total integer array space
      character*(*), intent(inout) :: c    (*)          !< System total character array space
      integer  ( 4), intent(in   ) :: lun  (*)          !< array with unit numbers
      character*(*), intent(in   ) :: lchar(*)          !< array with file names
      integer  ( 4), intent(in   ) :: action            !< type of action to perform
      type(delwaq_data)   , target :: dlwqd             !< delwaq data structure
      type(GridPointerColl)        :: gridps            !< collection of all grid definitions

      include 'actions.inc'
      include 'sysn.inc'          !   common with system characteristics
      include 'sysi.inc'          !   common with timer characteristics
      include 'sysa.inc'          !   common with pointers in the real workspace array
      include 'sysj.inc'          !   common with pointers in the integer workspace array
      include 'sysc.inc'          !   common with pointers in the character workspace array

!     Local declarations

      integer( 4) nosss           !  noseg_total  = noseg_water + noseg_bed
      integer( 4) noqtt           !  noq_total    = noq_water + noq_bed
      integer( 4) inwtyp          !  pointer to wasteload types (was missing)
      LOGICAL         IMFLAG , IDFLAG , IHFLAG
      LOGICAL         LDUMMY , LSTREC , LREWIN
      LOGICAL         FORESTER
      REAL            RDUMMY(1)
      INTEGER         IFFLAG
      INTEGER         IAFLAG
      INTEGER         IBFLAG
      INTEGER         NDDIM
      INTEGER         NVDIM
      INTEGER         ITIME
      INTEGER         NSTEP
      INTEGER         NOWARN
      INTEGER         IBND
      INTEGER         ISYS
      INTEGER         IERROR

      INTEGER         NOQT
      INTEGER         LLENG
      INTEGER         IDTOLD
      INTEGER         sindex

      integer          :: ithandl
      !
      ! Dummy variables - used in DLWQD
      !
      integer          :: ioptzb
      integer          :: nopred
      integer          :: itimel
      logical          :: updatr
      real(kind=kind(1.0d0)) :: tol

      INCLUDE 'state_data.inc'

      if ( ACTION == ACTION_FINALISATION ) then
          include 'dlwqdata_restore.inc'
          goto 20
      endif

      if ( ACTION == ACTION_INITIALISATION  .or.
     &     ACTION == ACTION_FULLCOMPUTATION        ) then

!          some initialisation

          ithandl = 0
          ITIME   = ITSTRT
          NSTEP   = (ITSTOP-ITSTRT)/IDT
          IFFLAG  = 0
          IAFLAG  = 0
          IBFLAG  = 0

!     Dummy variables - used in DLWQD
          ITIMEL  = ITIME
          lleng   = 0
          ioptzb  = 0 
          nopred  = 6
          NOWARN  = 0
          tol     = 0.0D0
          forester = .FALSE.
          updatr = .FALSE.

          nosss  = noseg + nseg2
          noqtt  = noq   + noq4
          NOQT   = NOQ + NOQ4
          inwtyp = intyp + nobnd

          IF ( MOD(INTOPT,16) .GE. 8 ) IBFLAG = 1
          LDUMMY = .FALSE.
          IF ( NDSPN .EQ. 0 ) THEN
             NDDIM = NODISP
          ELSE
             NDDIM = NDSPN
          ENDIF
          IF ( NVELN .EQ. 0 ) THEN
             NVDIM = NOVELO
          ELSE
             NVDIM = NVELN
          ENDIF
          LSTREC = ICFLAG .EQ. 1
          NOWARN   = 0
          IF ( ILFLAG .EQ. 0 ) LLENG = ILENG+2

          call initialise_progress( dlwqd%progress, nstep, lchar(44) )

!          Initialize second volume array with the first one

          nosss  = noseg + nseg2
          CALL MOVE   ( A(IVOL ), A(IVOL2) , NOSSS   )

      endif
!
!     Save/restore the local persistent variables,
!     if the computation is split up in steps
!
!     Note: the handle to the timer (ithandl) needs to be
!     properly initialised and restored
!
      IF ( ACTION == ACTION_INITIALISATION ) THEN
          if ( timon ) call timstrt ( "dlwqn1", ithandl )
          INCLUDE 'dlwqdata_save.inc'
          if ( timon ) call timstop ( ithandl )
          RETURN
      ENDIF

      IF ( ACTION == ACTION_SINGLESTEP ) THEN
          INCLUDE 'dlwqdata_restore.inc'
          call apply_operations( dlwqd )
      ENDIF

      if ( timon ) call timstrt ( "dlwqn1", ithandl )

!======================= simulation loop ============================

   10 continue

!        Determine the volumes and areas that ran dry,
!        They cannot have explicit processes during this time step

         call hsurf  ( noseg    , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &                 c(isfna) , a(isfun) , surface  , lun(19)  )
         call dryfld ( noseg    , nosss    , nolay    , a(ivol)  , noq1+noq2,
     &                 a(iarea) , nocons   , c(icnam) , a(icons) , surface  ,
     &                 j(iknmr) , iknmkv   )

!        mt3d coupling

         call dlwq_mt3d ( lun (19) , itime    , idt      , itstop   , notot    ,
     &                    nosys    , nosss    , nobnd    , c(isnam) , c(ibnid) ,
     &                    j(ibpnt) , a(iconc) , a(ibset) , noqtt    , j(ixpnt) ,
     &                    a(iflow) , ndmpq    , j(iqdmp) , a(idmpq) )

!        user transport processes

         call dlwqtr ( notot    , nosys    , nosss    , noq      , noq1     ,
     &                 noq2     , noq3     , nopa     , nosfun   , nodisp   ,
     &                 novelo   , j(ixpnt) , a(ivol)  , a(iarea) , a(iflow) ,
     &                 a(ileng) , a(iconc) , a(idisp) , a(icons) , a(iparm) ,
     &                 a(ifunc) , a(isfun) , a(idiff) , a(ivelo) , itime    ,
     &                 idt      , c(isnam) , nocons   , nofun    , c(icnam) ,
     &                 c(ipnam) , c(ifnam) , c(isfna) , ldummy   , ilflag   ,
     &                 npartp   )

!jvb     Temporary ? set the variables grid-setting for the DELWAQ variables

         call setset ( lun(19)  , nocons   , nopa     , nofun    , nosfun   ,
     &                 nosys    , notot    , nodisp   , novelo   , nodef    ,
     &                 noloc    , ndspx    , nvelx    , nlocx    , nflux    ,
     &                 nopred   , novar    , nogrid   , j(ivset) )

!        return conc and take-over from previous step or initial condition,
!        and do particle tracking of this step (will be back-coupled next call)

         call delpar01( itime   , noseg    , nolay    , noq      , nosys    ,
     &                  notot   , a(ivol)  , surface  , a(iflow) , c(isnam) ,
     &                  nosfun  , c(isfna) , a(isfun) , a(imass) , a(iconc) ,
     &                  iaflag  , intopt   , ndmps    , j(isdmp) , a(idmps) ,
     &                  a(imas2))

!        call PROCES subsystem

         call proces ( notot    , nosss    , a(iconc) , a(ivol)  , itime    ,
     &                 idt      , a(iderv) , ndmpar   , nproc    , nflux    ,
     &                 j(iipms) , j(insva) , j(iimod) , j(iiflu) , j(iipss) ,
     &                 a(iflux) , a(iflxd) , a(istoc) , ibflag   , ipbloo   ,
     &                 ipchar   , ioffbl   , ioffch   , a(imass) , nosys    ,
     &                 itfact   , a(imas2) , iaflag   , intopt   , a(iflxi) ,
     &                 j(ixpnt) , iknmkv   , noq1     , noq2     , noq3     ,
     &                 noq4     , ndspn    , j(idpnw) , a(idnew) , nodisp   ,
     &                 j(idpnt) , a(idiff) , ndspx    , a(idspx) , a(idsto) ,
     &                 nveln    , j(ivpnw) , a(ivnew) , novelo   , j(ivpnt) ,
     &                 a(ivelo) , nvelx    , a(ivelx) , a(ivsto) , a(idmps) ,
     &                 j(isdmp) , j(ipdmp) , ntdmpq   , a(idefa) , j(ipndt) ,
     &                 j(ipgrd) , j(ipvar) , j(iptyp) , j(ivarr) , j(ividx) ,
     &                 j(ivtda) , j(ivdag) , j(ivtag) , j(ivagg) , j(iapoi) ,
     &                 j(iaknd) , j(iadm1) , j(iadm2) , j(ivset) , j(ignos) ,
     &                 j(igseg) , novar    , a        , nogrid   , ndmps    ,
     &                 c(iprna) , intsrt   , j(iowns) , j(iownq) , mypart   ,
     &                 j(iprvpt), j(iprdon), nrref    , j(ipror) , nodef    ,
     &                 surface  , lun(19)  )

!        communicate boundaries (for domain decomposition)

         call dlwq_boundio ( lun(19)  , notot    , nosys    , nosss    , nobnd    ,
     &                       c(isnam) , c(ibnid) , j(ibpnt) , a(iconc) , a(ibset) ,
     &                       lchar(19))

!          set new boundaries

         call timer_start(timer_bound)

         if ( itime .ge. 0   ) then
             ! first: adjust boundaries by OpenDA
             if ( dlwqd%inopenda ) then
                 do ibnd = 1,nobnd
                     do isys = 1,nosys
                         call get_openda_buffer(isys,ibnd, 1,1,
     &                                   A(ibset+(ibnd-1)*nosys + isys-1))
                     enddo
                 enddo
             endif
             call dlwq17 ( a(ibset), a(ibsav), j(ibpnt), nobnd   , nosys   ,
     &                     notot   , idt     , a(iconc), a(iflow), a(iboun))
         endif
         call timer_stop(timer_bound)

!     Call OUTPUT system

      call timer_start(timer_output)
      CALL DLWQO2 ( NOTOT   , NOSSS   , NOPA    , NOSFUN  , ITIME   ,
     +              C(IMNAM), C(ISNAM), C(IDNAM), J(IDUMP), NODUMP  ,
     +              A(ICONC), A(ICONS), A(IPARM), A(IFUNC), A(ISFUN),
     +              A(IVOL) , NOCONS  , NOFUN   , IDT     , NOUTP   ,
     +              LCHAR   , LUN     , J(IIOUT), J(IIOPO), A(IRIOB),
     +              C(IONAM), NX      , NY      , J(IGRID), C(IEDIT),
     +              NOSYS   , A(IBOUN), J(ILP)  , A(IMASS), A(IMAS2),
     +              A(ISMAS), NFLUX   , A(IFLXI), ISFLAG  , IAFLAG  ,
     +              IBFLAG  , IMSTRT  , IMSTOP  , IMSTEP  , IDSTRT  ,
     +              IDSTOP  , IDSTEP  , IHSTRT  , IHSTOP  , IHSTEP  ,
     +              IMFLAG  , IDFLAG  , IHFLAG  , NOLOC   , A(IPLOC),
     +              NODEF   , A(IDEFA), ITSTRT  , ITSTOP  , NDMPAR  ,
     +              C(IDANA), NDMPQ   , NDMPS   , J(IQDMP), J(ISDMP),
     +              J(IPDMP), A(IDMPQ), A(IDMPS), A(IFLXD), NTDMPQ  ,
     +              C(ICBUF), NORAAI  , NTRAAQ  , J(IORAA), J(NQRAA),
     +              J(IQRAA), A(ITRRA), C(IRNAM), A(ISTOC), NOGRID  ,
     +              NOVAR   , J(IVARR), J(IVIDX), J(IVTDA), J(IVDAG),
     +              J(IAKND), J(IAPOI), J(IADM1), J(IADM2), J(IVSET),
     +              J(IGNOS), J(IGSEG), A       , NOBND   , NOBTYP  ,
     +              C(IBTYP), J(INTYP), C(ICNAM), noqtt   , J(IXPNT),
     +              INTOPT  , C(IPNAM), C(IFNAM), C(ISFNA), J(IDMPB),
     +              NOWST   , NOWTYP  , C(IWTYP), J(IWAST), J(INWTYP),
     +              A(IWDMP), iknmkv  , J(IOWNS), MYPART  , isegcol )
         call timer_stop(timer_output)

!          zero cummulative array's

         call timer_start(timer_output)
         if ( imflag .or. ( ihflag .and. noraai .gt. 0 ) ) then
            call zercum ( notot   , nosys   , nflux   , ndmpar  , ndmpq   ,
     &                    ndmps   , a(ismas), a(iflxi), a(imas2), a(iflxd),
     &                    a(idmpq), a(idmps), noraai  , imflag  , ihflag  ,
     &                    a(itrra), ibflag  , nowst   , a(iwdmp))
         endif
         if (mypart.eq.1) call write_progress( dlwqd%progress )
         call timer_stop(timer_output)

!          simulation done ?

         if ( itime .lt. 0      ) goto 9999
         if ( itime .ge. itstop ) goto 20

!        add processes

         call timer_start(timer_transport)
         call dlwq14 ( a(iderv), notot   , nosss   , itfact  , a(imas2),
     &                 idt     , iaflag  , a(idmps), intopt  , j(isdmp),
     &                 j(iowns), mypart )
         call timer_stop(timer_transport)
                                            ! correct new volumes come in a(ivol2)
!        get new volumes                    ! at rewind a(ivoll) contains the new volume
                                            ! after rewind.
         itimel = itime                     ! For case 2 a(ivoll) contains the incorrect
         itime  = itime + idt               ! new volume from file and mass correction
         call timer_start(timer_readdata)   ! takes place every time step.
         select case ( ivflag )
            case ( 1 )                 !     computation of volumes for computed volumes only
               call move   ( a(ivol) , a(ivol2), noseg   )
               call dlwqb3 ( a(iarea), a(iflow), a(ivnew), j(ixpnt), notot   ,
     &                       noq     , nvdim   , j(ivpnw), a(ivol2), intopt  ,
     &                       a(imas2), idt     , iaflag  , nosys   , a(idmpq),
     &                       ndmpq   , j(iqdmp))
               updatr = .true.
            case ( 2 )                 !     the fraudulent computation option
               call dlwq41 ( lun     , itime   , itimel  , a(iharm), a(ifarr),
     &                       j(inrha), j(inrh2), j(inrft), noseg   , a(ivoll),
     &                       j(ibulk), lchar   , ftype   , isflag  , ivflag  ,
     &                       updatr  , j(inisp), a(inrsp), j(intyp), j(iwork),
     &                       lstrec  , lrewin  , a(ivol2), mypart  , dlwqd   )
               if ( lrewin ) call move ( a(ivol2), a(ivoll) , noseg   )
               call dlwqf8 ( noseg   , noq     , j(ixpnt), idt     , iknmkv  ,
     &                       a(ivol ), a(iflow), a(ivoll), a(ivol2))
               updatr = .true.
               lrewin = .true.
               lstrec = .true.  ! always closure error correction
            case default               !     read new volumes from files
               call dlwq41 ( lun     , itime   , itimel  , a(iharm), a(ifarr),
     &                       j(inrha), j(inrh2), j(inrft), noseg   , a(ivol2),
     &                       j(ibulk), lchar   , ftype   , isflag  , ivflag  ,
     &                       updatr  , j(inisp), a(inrsp), j(intyp), j(iwork),
     &                       lstrec  , lrewin  , a(ivoll), mypart  , dlwqd   )
         end select
         call timer_stop(timer_readdata)

!        update the info on dry volumes with the new volumes

         call dryfle ( noseg    , nosss    , a(ivol2) , nolay    , nocons   ,
     &                 c(icnam) , a(icons) , surface  , j(iknmr) , iknmkv   )

!          add the waste loads

         call timer_start(timer_wastes)
         call dlwq15 ( nosys    , notot    , noseg    , noq      , nowst    ,
     &                 nowtyp   , ndmps    , intopt   , idt      , itime    ,
     &                 iaflag   , c(isnam) , a(iconc) , a(ivol)  , a(ivol2) ,
     &                 a(iflow) , j(ixpnt) , c(iwsid) , c(iwnam) , c(iwtyp) ,
     &                 j(inwtyp), j(iwast) , iwstkind , a(iwste) , a(iderv) ,
     &                 iknmkv   , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &                 c(isfna) , a(isfun) , j(isdmp) , a(idmps) , a(imas2) ,
     &                 a(iwdmp) , 1        , notot    , j(iowns ), mypart   )
         call timer_stop(timer_wastes)

!        do the transport itself

         call timer_start(timer_transport)
         call dlwq16 ( nosys    , notot    , nosss    , noq1     , noq2     ,
     &                 noq3     , noqtt    , nddim    , nvdim    , a(idisp) ,
     &                 a(idnew) , a(ivnew) , a(iarea) , a(iflow) , a(ileng) ,
     &                 j(ixpnt) , iknmkv   , j(idpnw) , j(ivpnw) , a(iconc) ,
     &                 a(iboun) , intopt   , ilflag   , idt      , a(iderv) ,
     &                 iaflag   , a(imas2) , ndmpq    , j(iqdmp) , a(idmpq) ,
     &                 j(iowns) , mypart   )
         call timer_stop(timer_transport)

!        new time values, volumes excluded

         call timer_start(timer_readdata)
         idtold = idt
         call dlwqt0 ( lun      , itime    , itimel   , a(iharm) , a(ifarr) ,
     &                 j(inrha) , j(inrh2) , j(inrft) , idt      , a(ivol)  ,
     &                 a(idiff) , a(iarea) , a(iflow) , a(ivelo) , a(ileng) ,
     &                 a(iwste) , a(ibset) , a(icons) , a(iparm) , a(ifunc) ,
     &                 a(isfun) , j(ibulk) , lchar    , c(ilunt) , ftype    ,
     &                 intsrt   , isflag   , ifflag   , ivflag   , ilflag   ,
     &                 updatr   , j(iktim) , j(iknmr) , j(inisp) , a(inrsp) ,
     &                 j(intyp) , j(iwork) , .false.  , ldummy   , rdummy   ,
     &                 .false.  , gridps   , dlwqd    )
         call timer_stop(timer_readdata)

!        set a time step

         call timer_start(timer_transport)
         call dlwq18 ( nosys    , notot    , nototp   , nosss    , a(ivol2) ,
     &                 surface  , a(imass) , a(iconc) , a(iderv) , idtold   ,
     &                 ivflag   , lun(19)  , j(iowns) , mypart   )

!        update new concentrations for subdomain boundaries

         call update_rdata(A(imass), notot, 'noseg', 1, 'stc1', ierror)
         call update_rdata(A(iconc), notot, 'noseg', 1, 'stc1', ierror)

         if (.false. .and. itime.ge.itstop) then
            call collect_rdata(mypart, A(iconc), notot,'noseg',1, ierror)
            call collect_rdata(mypart, A(imass), notot,'noseg',1, ierror)
         endif

!        calculate closure error

         if ( lrewin .and. lstrec ) then
!           collect information on master for computation of closure error before rewind
            call collect_rdata(mypart,A(IMASS), notot, 'noseg', 1, ierror)
            if ( mypart .eq. 1 )
     &      call dlwqce ( a(imass), a(ivoll), a(ivol2), nosys , notot ,
     &                    noseg   , lun(19) )
            call distribute_rdata(mypart,A(IMASS),notot,'noseg',1,'distrib_itf', ierror)
            call move   ( a(ivoll), a(ivol) , noseg   )
         else
!           replace old by new volumes
            call move   ( a(ivol2), a(ivol) , noseg   )
         endif

!          integrate the fluxes at dump segments fill ASMASS with mass

         if ( ibflag .gt. 0 ) then
            call proint ( nflux   , ndmpar  , idtold  , itfact  , a(iflxd),
     &                    a(iflxi), j(isdmp), j(ipdmp), ntdmpq  )
         endif
         call timer_stop(timer_transport)

!          end of loop

         if ( ACTION == ACTION_FULLCOMPUTATION ) goto 10

   20 continue

      if ( ACTION == ACTION_FINALISATION    .or.
     &     ACTION == ACTION_FULLCOMPUTATION      ) then
          if (mypart.eq.1) then

!             close files, except monitor file

              call timer_start(timer_close)
              call CloseHydroFiles( dlwqd%collcoll )
              call close_files( lun )

!             write restart file

              CALL DLWQ13 ( LUN      , LCHAR , A(ICONC) , ITIME , C(IMNAM) ,
     &                      C(ISNAM) , NOTOT , NOSSS    )
              call timer_stop(timer_close)
          end if ! mypart.eq.1
      endif

 9999 if ( timon ) call timstop ( ithandl )

      dlwqd%itime = itime

      RETURN
      END SUBROUTINE
