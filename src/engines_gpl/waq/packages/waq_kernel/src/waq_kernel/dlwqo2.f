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

      SUBROUTINE DLWQO2 ( NOTOT , NOSEG , NOPA  , NOSFUN, ITIME ,
     +                    MONAME, SYNAME, DUNAME, IDUMP , NODUMP,
     +                    CONC  , CONS  , PARAM , FUNC  , SEGFUN,
     +                    VOLUME, NOCONS, NOFUN , IDT   , NOUTP ,
     +                    LCHAR , LUN   , IOUTPS, IOPOIN, RIOBUF,
     +                    OUNAM , NX    , NY    , LGRID , CGRID ,
     +                    NOSYS , BOUND , IP    , AMASS , AMASS2,
     +                    ASMASS, NOFLUX, FLXINT, ISFLAG, IAFLAG,
     +                    IBFLAG, IMSTRT, IMSTOP, IMSTEP, IDSTRT,
     +                    IDSTOP, IDSTEP, IHSTRT, IHSTOP, IHSTEP,
     +                    IMFLAG, IDFLAG, IHFLAG, NOLOC , PROLOC,
     +                    NODEF , DEFAUL, ITSTRT, ITSTOP, NDMPAR,
     +                    DANAM , NDMPQ , NDMPS , IQDMP , ISDMP ,
     +                    IPDMP , DMPQ  , DMPS  , FLXDMP, NTDMPQ,
     +                    NAMBUF, NORAAI, NTRAAQ, IORAAI, NQRAAI,
     +                    IQRAAI, TRRAAI, RANAM , STOCHI, NOGRID,
     +                    NOVAR , VARARR, VARIDX, VARTDA, VARDAG,
     +                    ARRKND, ARRPOI, ARRDM1, ARRDM2, VGRSET,
     +                    GRDNOS, GRDSEG, A     , NOBND , NOBTYP,
     +                    BNDTYP, INWTYP, CONAME, NOQ   , IPOINT,
     +                    INTOPT, PANAME, FUNAME, SFNAME, DMPBAL,
     +                    NOWST , NOWTYP, WSTTYP, IWASTE, INXTYP,
     +                    WSTDMP, iknmrk, OWNERS, MYPART, ISEGCOL)
!
!
!     Deltares      SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : january 1993 Jan van Beek
!
!
!     FUNCTION            : Driver output system
!
!     LOGICAL UNITS       : -
!
!     SUBROUTINES CALLED  : BALDMP, fills balance for dump area's
!                           FIOUTV, fills output buffer, single cel grids
!                           FIOSUB, fills output buffer, sub-grids
!                           OUTMON, performs a monitor output step
!                           OUTDMP, performs a grid dump output step
!                           OUTHIS, performs a history output step
!                           OUTHNF, performs a history NEFIS step
!                           OUTMAP, performs a map output step
!                           OUTMNF, performs a map NEFIS step
!                           OUTBAL, performs a balance output step
!                           RAATRA, fills transport for raaien
!                           STEPYN, evaluates timers
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NOTOT   INTEGER       1     INPUT   Total number of substances
!     NOSEG   INTEGER       1     INPUT   Nr. of computational elements
!     NOPA    INTEGER       1     INPUT   Number of parameters
!     NOSFUN  INTEGER       1     INPUT   Number of segment functions
!     ITIME   INTEGER       1     INPUT   Time in system clock units
!     MONAME  CHAR*40       4     INPUT   Model and run names
!     SYNAME  CHAR*20    NOTOT    INPUT   names of substances
!     DUNAME  CHAR*20    NODUMP   INPUT   names of dump locations
!     IDUMP   INTEGER    NODUMP   INPUT   dump segment numbers
!     NODUMP  INTEGER       1     INPUT   number of dump locations
!     CONC    REAL   NOTOT,NOSEG  INPUT   Model concentrations
!     CONS    REAL          *     IN/OUT  Model constants
!     PARAM   REAL    NOPA,NOSEG  IN/OUT  Model parameters
!     FUNC    REAL          *     IN/OUT  Model functions at ITIME
!     SEGFUN  REAL   NOSEG,NOSFUN IN/OUT  Segment functions at ITIME
!     VOLUME  REAL      NOSEG     INPUT   Segment volumes
!     NOCONS  INTEGER       1     INPUT   Number of constants used
!     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
!     IDT     INTEGER       1     INPUT   Simulation timestep
!     NOUTP   INTEGER       1     INPUT   Number of output files
!     LCHAR   CHAR*(*)      *     INPUT   File names
!     LUN     INTEGER       *     INPUT   Uint numbers
!     IOUTPS  INTEGER 7*NOUTP    IN/OUT   Output structure
!                                            index 1 = start time
!                                            index 2 = stop time
!                                            index 3 = time step
!                                            index 4 = number of vars
!                                            index 5 = kind of output
!                                            index 6 = grid of output
!                                            index 7 = initialize flag
!     IOPOIN  INTEGER       *     INPUT   Pointer to DELWAQ array's
!     RIOBUF  REAL          *     LOCAL   Output buffer
!     OUNAM   CHAR*20       *     INPUT   name of output variable
!     NX      INTEGER       1     INPUT   Width of output grid
!     NY      INTEGER       1     INPUT   Depth of output grid
!     LGRID   INTEGER     NX*NY   INPUT   grid-layout
!     CGRID   CHAR*20       *     LOCAL   Char buffer for dmp output
!     NOSYS   INTEGER       1     INPUT   Number of active substances
!     BOUND   REAL          *     INPUT   Bounary conditions
!     IP      INTEGER       *     IN/OUT  Paging structure
!     AMASS   REAL       NOTOT,*  INPUT   Mass array
!     AMASS2  REAL       NOTOT,*  IN/OUT  Cummulative balance on whole
!     ASMASS  REAL       NOTOT,*  IN/OUT  Cummulative balance per segment
!     NOFLUX  INTEGER       1     INPUT   Number of fluxes
!     FLXINT  REAL  NOFLUX*NDMPAR IN/OUT  Integrated fluxes at dump segments
!     ISFLAG  INTEGER       1     INPUT   if 1 then dd-hh:mm'ss"
!     IAFLAG  INTEGER       1     OUTPUT  if 1 then accumulate mass bal
!     IBFLAG  INTEGER       1     INPUT   Flag = 1 then balances
!     IMSTRT  INTEGER       1     INPUT   Monitoring start time ( scu )
!     IMSTOP  INTEGER       1     INPUT   Monitoring stop time ( scu )
!     IMSTEP  INTEGER       1     INPUT   Monitoring time step ( scu )
!     IDSTRT  INTEGER       1     INPUT   Dump start time ( scu )
!     IDSTOP  INTEGER       1     INPUT   Dump stop time ( scu )
!     IDSTEP  INTEGER       1     INPUT   Dump time step ( scu )
!     IHSTRT  INTEGER       1     INPUT   History start time ( scu )
!     IHSTOP  INTEGER       1     INPUT   History stop time ( scu )
!     IHSTEP  INTEGER       1     INPUT   History time step ( scu )
!     IMFLAG  LOGICAL       1     OUTPUT  If .T. then monitor step
!     IDFLAG  LOGICAL       1     OUTPUT  If .T. then dump step
!     IHFLAG  LOGICAL       1     OUTPUT  If .T. then history step
!     NOLOC   INTEGER       1     INPUT   Number of variables in PROLOC
!     PARAM   REAL   NOLOC,NOSEG  INPUT   Parameters local in PROCES system
!     NODEF   INTEGER       1     INPUT   Number of used defaults
!     DEFAUL  REAL          *     INPUT   Default proces parameters
!     ITSTRT  INTEGER     1       INPUT   start time
!     ITSTOP  INTEGER     1       INPUT   stop time
!     NDMPAR  INTEGER     1       INPUT   Number of dump areas
!     DANAM   CHAR*20  NDMPAR     INPUT   Dump area names
!     NDMPQ   INTEGER     1       INPUT   Number of dumped exchanges
!     NDMPS   INTEGER     1       INPUT   Number of dumped segments
!     IQDMP   INTEGER       *     INPUT   Exchange to dumped exchange pointer
!     ISDMP   INTEGER       *     INPUT   Segment to dumped segment pointer
!     IPDMP   INTEGER       *     INPUT   pointer structure dump area's
!     DMPQ    REAL  NOTOT*NDMPS*? INPUT   mass balance dumped segments
!     DMPS    REAL  NOSYS*NDMPQ*? INPUT   mass balance dumped exchange
!     FLXDMP  REAL  NOFLUX*NDMPS  INPUT   Integrated fluxes
!     NAMBUF  CHAR*20       *     INPUT   Buffer for names
!     NORAAI  INTEGER       1     INPUT   Number of raaien
!     NTRAAQ  INTEGER       1     INPUT   Total number of exch. in raaien
!     IORAAI  INTEGER       *     INPUT   Output option for raai
!     NQRAAI  INTEGER       *     INPUT   Number of exchanges in raai
!     IQRAAI  INTEGER       *     INPUT   Exchanges in raai
!     TRRAAI  REAL NOTOT*NDMPAR*6 IN/OUT  Cummulative transport over raai
!     RANAM   CHAR*20       *     INPUT   Raaien names
!     STOCHI  REAL   NOTOT*NOFLUX INPUT   Proces stochiometry
!     INTOPT  INTEGER     1       INPUT   Integration and balance suboptions
!     OWNERS  INTEGER   NOSEG     INPUT   ownership of segments
!     MYPART  INTEGER     1       INPUT   number of current part/subdomain
!     ==================================================================
!
      use timers
      use m_couplib
!
      INTEGER       NOTOT , NOSEG , NOPA  , NOSFUN, ITIME ,
     +              NODUMP, NOCONS, NOFUN , IDT   , NOUTP ,
     +              NX    , NY    , NOSYS , NOFLUX, ISFLAG,
     +              IAFLAG, IBFLAG, IMSTRT, IMSTOP, IMSTEP,
     +              IDSTRT, IDSTOP, IDSTEP, IHSTRT, IHSTOP,
     +              IHSTEP, NOLOC , NODEF , ITSTRT, ITSTOP,
     +              NDMPAR, NDMPQ , NDMPS , NTDMPQ, NORAAI,
     +              NTRAAQ, NOGRID, NOVAR , NOBND , NOBTYP,
     +              NOQ   , MYPART
      INTEGER       IDUMP(*)      , LUN(*)        ,
     +              IOUTPS(7,*)   , IOPOIN(*)     ,
     +              LGRID(*)      , IP(*)         ,
     +              IQDMP(*)      , ISDMP(*)      ,
     +              IPDMP(*)      , IORAAI(*)     ,
     +              NQRAAI(*)     , IQRAAI(*)     ,
     +              VARARR(NOVAR) , VARIDX(NOVAR) ,
     +              VARTDA(NOVAR) , VARDAG(NOVAR) ,
     +              ARRKND(*)     , ARRPOI(*)     ,
     +              ARRDM1(*)     , ARRDM2(*)     ,
     +              VGRSET(NOVAR,*),GRDNOS(NOGRID),
     +              GRDSEG(NOSEG,NOGRID)          ,
     +              INWTYP(*)     , IPOINT( 4,NOQ),
     +              OWNERS(NOSEG)
      integer(4), intent(in   ) :: iknmrk(noseg)      ! Feature array. Bit zero set means active.
      REAL          conc ( notot, noseg ),
     &                              CONS(*)       ,
     &              param( nopa , noseg ),
     &                              FUNC(*)       ,
     &              segfun(noseg, nosfun),
     &                              VOLUME(*)     ,
     +              RIOBUF(*)     , BOUND(*)      ,
     +              amass( notot, noseg ),
     &                              AMASS2(NOTOT,5),
     +              ASMASS(*)     , FLXINT(*)     ,
     +              PROLOC(*)     , DEFAUL(*)     ,
     +              DMPQ(*)       , DMPS(*)       ,
     +              FLXDMP(*)     , TRRAAI(NOSYS,*),
     +              STOCHI(NOTOT,NOFLUX), A(*)
      CHARACTER*20  SYNAME(*)     , DUNAME(*)     ,
     +              OUNAM(*)      , CGRID(*)      ,
     +              DANAM(*)      , NAMBUF(*)     ,
     +              RANAM(*)      , BNDTYP(*)     ,
     +              CONAME(*)     , PANAME(*)     ,
     +              FUNAME(*)     , SFNAME(*)
      CHARACTER*40  MONAME(4)
      CHARACTER*(*) LCHAR (*)
      LOGICAL       IMFLAG, IDFLAG, IHFLAG
      integer                    :: dmpbal(ndmpar)        ! indicates if dump area is included in the balance
      integer                    :: nowst                 ! number of wasteloads
      integer                    :: nowtyp                ! number of wasteload types
      character(len=20)          :: wsttyp(nowtyp)        ! wasteload types names
      integer                    :: iwaste(nowst)         ! segment numbers of the wasteloads
      integer                    :: inxtyp(nowst)         ! wasteload type number (index in wsttyp)
      real                       :: wstdmp(notot,nowst,2) ! accumulated wasteloads 1/2 in and out
      integer, intent(in   )     :: isegcol(*)            ! pointer from segment to top of column
!
!     Local declarations
!
      PARAMETER   ( IMON = 1 , IMO2 = 2 , IDMP = 3 , IDM2 = 4 ,
     +              IHIS = 5 , IHI2 = 6 , IMAP = 7 , IMA2 = 8 ,
     +              IBAL = 9 , IHNF =10 , IHN2 =11 , IMNF =12 ,
     +              IMN2 =13 , IMO3 =14 , IMO4 =15 , IHI3 =16 ,
     +              IHI4 =17 , IHN3 =18 , IHN4 =19 , IBA2 =20 ,
     +              IBA3 =21 )
      PARAMETER   ( IGSEG = 1, IGMON = 2, IGGRD = 3, IGSUB= 4 )
      PARAMETER   ( LUOFF = 18 )
      PARAMETER   ( LUOFF2= 36 )
      INTEGER       K1    , IOSTRT, IOSTOP, IOSTEP, NRVAR ,
     +              ISRTOU, IGRDOU, INIOUT, LUNOUT, IOUT
      CHARACTER*255 LCHOUT
      CHARACTER*20  NAME
      LOGICAL       LOFLAG, LMFIRS, LDFIRS, LHFIRS, LDUMMY
      LOGICAL       LGET  , LREAD
      REAL, ALLOCATABLE :: SURF(:)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqo2", ithandl )
!
!     Evaluate standard DELWAQ output timers
!
      CALL STEPYN (ITIME , IDT   , IMSTRT, IMSTOP, IMSTEP,
     +             IMFLAG, LMFIRS)
      CALL STEPYN (ITIME , IDT   , IDSTRT, IDSTOP, IDSTEP,
     +             IDFLAG, LDFIRS)
      CALL STEPYN (ITIME , IDT   , IHSTRT, IHSTOP, IHSTEP,
     +             IHFLAG, LHFIRS)
!
!     Fill mass in AMASS2 array by summing AMASS over all segments
!
      IF ( IMFLAG ) THEN
         call collect_data(mypart, amass , notot,'noseg',1,ierr)
         call combine_1d_rdata(amass2, notot*5, CP_SUM, ierr)
         IAFLAG = 1
         if (mypart.eq.1) then
            DO 20 I2 = 1,NOTOT
               AMASS2(I2,1) = 0.0
               DO 10 I1 = 1,NOSEG
                  AMASS2(I2,1) = AMASS2(I2,1) + AMASS(I2,I1)
   10          CONTINUE
   20       CONTINUE
         endif
      ENDIF
!
!     Fill mass in ASMASS array using DMPQ and DMPS
!
      IF ( IMFLAG .OR. ( IHFLAG .AND. NORAAI .GT. 0) ) THEN
         IF ( IBFLAG .EQ. 1 ) THEN
            call collect_data(mypart, flxdmp, noflux,'ndmps',1, ierr)
            call collect_data(mypart, dmps  , notot ,'ndmps',3, ierr)
            call collect_data(mypart, dmpq  , nosys ,'ndmpq',2, ierr)
            if (mypart.eq.1) then
               CALL BALDMP (NOTOT , NOSYS , NOFLUX, NDMPAR, NDMPQ ,
     +                      NDMPS , NTDMPQ, IQDMP , ISDMP , IPDMP ,
     +                      DMPQ  , AMASS , DMPS  , FLXDMP, ASMASS,
     +                      FLXINT)
            endif
         ENDIF

         IF ( NORAAI .GT. 0 ) THEN
            IF ( LHFIRS ) THEN
               CALL ZERO   (TRRAAI, NORAAI*NOSYS  )
            ELSE
               call collect_data(mypart, dmpq  , nosys , 'ndmps',2,ierr)
               if (mypart.eq.1) then
                  CALL RAATRA (NOSYS , NDMPQ , NORAAI, NTRAAQ, IORAAI,
     +                         NQRAAI, IQRAAI, IQDMP , DMPQ  , TRRAAI)
               endif
            ENDIF
         ENDIF
!
      ENDIF
!
!     Initialize K1, pointer in IOPOIN and OUNAM
!
      lread = .true.
      K1 = 1
!
!     Loop over the output files
!
      DO 200 IOUT = 1 , NOUTP
!
!        Map output structure to single variables part 1
!
         IOSTRT = IOUTPS(1,IOUT)
         IOSTOP = IOUTPS(2,IOUT)
         IOSTEP = IOUTPS(3,IOUT)
         NRVAR  = IOUTPS(4,IOUT)
!
!        Output required ?
!
         CALL STEPYN (ITIME , IDT   , IOSTRT, IOSTOP, IOSTEP,
     +                LOFLAG, LDUMMY)
!
         IF ( .NOT. LOFLAG ) GOTO 100
!
!        Collect data on master-process
!
         if (lread) then
            call collect_data(mypart, conc  , notot ,'noseg',1, ierr)
            call collect_data(mypart, volume, 1     ,'noseg',1, ierr)
            call collect_data(mypart, proloc, noloc ,'noseg',1, ierr)
            if (ibflag.gt.0)
     +         call accumulate_data(flxint, noflux,'ndmpar',1, 'my_dmpar', ierr)
            lread = .false.
         endif

         if (mypart.eq.1) then
!
!        Map output structure to single variables part 2
!
            ISRTOU = IOUTPS(5,IOUT)
            IGRDOU = IOUTPS(6,IOUT)
            INIOUT = IOUTPS(7,IOUT)
            IF ( IOUT .LE. 4 ) THEN
               IFI = IOUT + LUOFF
            ELSEIF ( IOUT .LE. 7 ) THEN
               IFI = IOUT + LUOFF2 - 4
            ELSE
               IFI = IOUT + LUOFF2 - 2
            ENDIF
            LUNOUT = LUN(IFI)
            LCHOUT = LCHAR(IFI)
!
!        No balance output if they are not active
!
            IF ( ( ISRTOU .EQ. IBAL .OR. ISRTOU .EQ. IBA2 .OR.
     +             ISRTOU .EQ. IBA2) .AND. IBFLAG .NE. 1 ) GOTO 100
!
!        Set all local variables used active on base grid
!
            CALL ACTLOC (IOPOIN, NRVAR , NOCONS, NOPA  , NOFUN ,
     +                   NOSFUN, NOTOT , NOSEG , NOLOC , NOGRID,
     +                   NOVAR , VARARR, VARIDX, VARTDA, VARDAG,
     +                   ARRKND, ARRPOI, ARRDM1, ARRDM2, VGRSET,
     +                   GRDNOS, GRDSEG, A     )
!
!        Fill output buffer
!
            IF ( ISRTOU .EQ. IBA2 ) THEN
!
               CALL FLXBAL (NOTOT , NOFLUX, NDMPAR, NRVAR , STOCHI,
     +                      FLXINT, ASMASS, RIOBUF)
!
            ELSEIF ( ISRTOU .EQ. IBA3 ) THEN
!     jos doet het zelf
            ELSEIF ( IGRDOU .EQ. IGSUB ) THEN
               IF (ISRTOU .EQ. IMO3 .OR.
     +             ISRTOU .EQ. IHI3 .OR.
     +             ISRTOU .EQ. IHN3     ) THEN
                  NCOUT = NOTOT
               ELSE
                  NCOUT = 0
               ENDIF
               NRVAR2 = NRVAR/2
!
!           For the dump area's
!
               CALL FIOSUB (RIOBUF, IOPOIN(K1), NRVAR2, NOCONS, NOPA  ,
     +                      NOFUN , NOSFUN    , NOTOT , CONC  , SEGFUN,
     +                      FUNC  , PARAM     , CONS  , IDT   , ITIME ,
     +                      VOLUME, NOSEG     , NOSYS , NDMPAR, IPDMP ,
     +                      BOUND , NOLOC     , PROLOC, NODEF , DEFAUL,
     +                      NCOUT , NTDMPQ    , paname, sfname, funame,
     +                      danam )
!
!           For the raaien
!
               IF ((ISRTOU .EQ. IHI3 .OR.
     +              ISRTOU .EQ. IHN3     ) .AND.
     +              NORAAI .GT. 0               ) THEN
                  NRVAR3 = NOTOT + NRVAR2
                  IP1 = (NCOUT+NRVAR2)*NDMPAR + 1
                  CALL FIORAA (RIOBUF(IP1), NRVAR3, TRRAAI, NORAAI, NOSYS)
               ENDIF
!
            ELSE
               NRVAR2 = NRVAR
               CALL FIOUTV ( RIOBUF, IOPOIN(K1), NRVAR , NOCONS, NOPA  ,
     +                       NOFUN , NOSFUN    , NOTOT , CONC  , SEGFUN,
     +                       FUNC  , PARAM     , CONS  , IDT   , ITIME ,
     +                       VOLUME, NOSEG     , NOSYS , NODUMP, IDUMP ,
     +                       NX    , NY        , LGRID , IGRDOU, BOUND ,
     +                       NOLOC , PROLOC    , NODEF , DEFAUL)
            ENDIF
!
!        Fill character buffer with substance names and output names
!
            IF ( ISRTOU .EQ. IMNF .OR.
     +           ISRTOU .EQ. IHNF .OR.
     +           ISRTOU .EQ. IMO3 .OR.
     +           ISRTOU .EQ. IHI3 .OR.
     +           ISRTOU .EQ. IHN3     ) THEN
               DO 30 I = 1 , NOTOT
                  NAMBUF(I) = SYNAME(I)
   30          CONTINUE
               DO 40 I = 1 , NRVAR2
                  NAMBUF(NOTOT+I) = OUNAM(K1+I-1)
   40          CONTINUE
            ENDIF
!
!        Perform output
!
            IF ( ISRTOU .EQ. IMON ) THEN
!
               CALL OUTMON ( LUNOUT   , IDUMP , CONC  , AMASS2, ITIME ,
     +                       DUNAME   , SYNAME, MONAME, NODUMP, NOTOT ,
     +                       IP       , ISFLAG, ASMASS, IBFLAG, NRVAR ,
     +                       OUNAM(K1), RIOBUF, ITSTRT, ITSTOP, NDMPAR,
     +                       DANAM    )
!
            ELSEIF ( ISRTOU .EQ. IMO2 ) THEN
!
               CALL OUTMON ( LUNOUT   , IDUMP , CONC  , AMASS2, ITIME ,
     +                       DUNAME   , SYNAME, MONAME, NODUMP, 0     ,
     +                       IP       , ISFLAG, ASMASS, IBFLAG, NRVAR ,
     +                       OUNAM(K1), RIOBUF, ITSTRT, ITSTOP, NDMPAR,
     +                       DANAM    )
!
            ELSEIF ( ISRTOU .EQ. IMO3 ) THEN
!
               CALL OUTMO3 ( LUNOUT, AMASS2   , ITIME , SYNAME, MONAME,
     +                       NOTOT , IP       , ISFLAG, ASMASS, IBFLAG,
     +                       NRVAR2, OUNAM(K1), RIOBUF, ITSTRT, ITSTOP,
     +                       NDMPAR, DANAM    )
!
            ELSEIF ( ISRTOU .EQ. IMO4 ) THEN
!
               CALL OUTMO3 ( LUNOUT, AMASS2   , ITIME , SYNAME, MONAME,
     +                       0     , IP       , ISFLAG, ASMASS, IBFLAG,
     +                       NRVAR2, OUNAM(K1), RIOBUF, ITSTRT, ITSTOP,
     +                       NDMPAR, DANAM    )
!
            ELSEIF ( ISRTOU .EQ. IDMP ) THEN
!
               CALL OUTDMP (LUNOUT, LCHOUT, ITIME , MONAME, NX       ,
     +                      NY    , LGRID , CGRID , NOTOT , NOSYS    ,
     +                      SYNAME, CONC  , BOUND , NRVAR , OUNAM(K1),
     +                      RIOBUF, IP(5) , ISFLAG, INIOUT)
!
            ELSEIF ( ISRTOU .EQ. IDM2 ) THEN
!
               CALL OUTDMP (LUNOUT, LCHOUT, ITIME , MONAME, NX       ,
     +                      NY    , LGRID , CGRID , 0     , 0        ,
     +                      SYNAME, CONC  , BOUND , NRVAR , OUNAM(K1),
     +                      RIOBUF, IP(5) , ISFLAG, INIOUT)
!
            ELSEIF ( ISRTOU .EQ. IHIS ) THEN
!
               CALL OUTHIS (LUNOUT, LCHOUT   , ITIME , MONAME, NODUMP,
     +                      IDUMP , DUNAME   , NOTOT , SYNAME, CONC  ,
     +                      NRVAR , OUNAM(K1), RIOBUF, INIOUT)
!
            ELSEIF ( ISRTOU .EQ. IHNF ) THEN
!
               IOF = NRVAR*NODUMP + 1
               CALL OUTHNF (LUNOUT, LCHOUT     , ITIME , MONAME, NOSEG ,
     +                      NOTOT , CONC       , NAMBUF, NRVAR , RIOBUF,
     +                      IOSTRT, IOSTOP     , IOSTEP, NODUMP, IDUMP ,
     +                      DUNAME, RIOBUF(IOF), INIOUT)
!
            ELSEIF ( ISRTOU .EQ. IHI2 ) THEN
!
               CALL OUTHIS (LUNOUT, LCHOUT   , ITIME , MONAME, NODUMP,
     +                      IDUMP , DUNAME   , 0     , SYNAME, CONC  ,
     +                      NRVAR , OUNAM(K1), RIOBUF, INIOUT)
!
            ELSEIF ( ISRTOU .EQ. IHN2 ) THEN
!
               IOF = NRVAR*NODUMP + 1
               CALL OUTHNF (LUNOUT, LCHOUT     , ITIME    , MONAME, NOSEG ,
     +                      0     , CONC       , OUNAM(K1), NRVAR , RIOBUF,
     +                      IOSTRT, IOSTOP     , IOSTEP   , NODUMP, IDUMP ,
     +                      DUNAME, RIOBUF(IOF), INIOUT   )
!
            ELSEIF ( ISRTOU .EQ. IHI3 ) THEN
!
!           Let op RANAM achter DANAM
!
               NRVAR3 = NOTOT + NRVAR2
               NSEGOU = NDMPAR + NORAAI
               CALL OUTHIS (LUNOUT, LCHOUT   , ITIME , MONAME, NSEGOU,
     +                      IDUMP , DANAM    , 0     , SYNAME, CONC  ,
     +                      NRVAR3, NAMBUF   , RIOBUF, INIOUT)
!
            ELSEIF ( ISRTOU .EQ. IHN3 ) THEN
!
!           Let op RANAM achter DANAM
!
               NRVAR3 = NOTOT + NRVAR2
               NSEGOU = NDMPAR + NORAAI
               IOF = NRVAR3*NSEGOU + 1
               CALL OUTHNF (LUNOUT, LCHOUT     , ITIME    , MONAME, NOSEG ,
     +                      0     , CONC       , NAMBUF   , NRVAR3, RIOBUF,
     +                      IOSTRT, IOSTOP     , IOSTEP   , NSEGOU, IDUMP ,
     +                      DANAM , RIOBUF(IOF), INIOUT   )
!
            ELSEIF ( ISRTOU .EQ. IHI4 ) THEN
!
               CALL OUTHIS (LUNOUT, LCHOUT   , ITIME , MONAME, NDMPAR,
     +                      IDUMP , DANAM    , 0     , SYNAME, CONC  ,
     +                      NRVAR2, OUNAM(K1), RIOBUF, INIOUT)
!
            ELSEIF ( ISRTOU .EQ. IHN4 ) THEN
!
               IOF = NRVAR2*NDMPAR + 1
               CALL OUTHNF (LUNOUT, LCHOUT     , ITIME    , MONAME, NOSEG ,
     +                      0     , CONC       , OUNAM(K1), NRVAR2, RIOBUF,
     +                      IOSTRT, IOSTOP     , IOSTEP   , NDMPAR, IDUMP ,
     +                      DANAM , RIOBUF(IOF), INIOUT   )
!
            ELSEIF ( ISRTOU .EQ. IMAP ) THEN
!
               CALL OUTMAP (LUNOUT   , LCHOUT, ITIME , MONAME, NOSEG ,
     +                      NOTOT    , CONC  , SYNAME, NRVAR , RIOBUF,
     +                      OUNAM(K1), iknmrk, INIOUT)
!
            ELSEIF ( ISRTOU .EQ. IMNF ) THEN
!
               IOF = NRVAR*NOSEG + 1
               CALL OUTMNF (LUNOUT   , LCHOUT, ITIME , MONAME, NOSEG      ,
     +                      NOTOT    , CONC  , SYNAME, NRVAR , RIOBUF     ,
     +                      OUNAM(K1), IOSTRT, IOSTOP, IOSTEP, RIOBUF(IOF),
     +                      INIOUT   )
!
            ELSEIF ( ISRTOU .EQ. IMA2 ) THEN
!
               CALL OUTMAP (LUNOUT   , LCHOUT, ITIME , MONAME, NOSEG ,
     +                      0        , CONC  , SYNAME, NRVAR , RIOBUF,
     +                      OUNAM(K1), iknmrk, INIOUT)
!
            ELSEIF ( ISRTOU .EQ. IMN2 ) THEN
!
               IOF = NRVAR*NOSEG + 1
               CALL OUTMNF (LUNOUT   , LCHOUT, ITIME , MONAME, NOSEG      ,
     +                      0        , CONC  , SYNAME, NRVAR , RIOBUF     ,
     +                      OUNAM(K1), IOSTRT, IOSTOP, IOSTEP, RIOBUF(IOF),
     +                      INIOUT   )
!
            ELSEIF ( ISRTOU .EQ. IBAL ) THEN
!
               CALL OUTBAL (LUNOUT, LCHOUT, ITIME , MONAME, NOTOT ,
     +                      NOFLUX, SYNAME, NDMPAR, DANAM , ASMASS,
     +                      FLXINT, NRVAR2, RIOBUF, INIOUT)
!
            ELSEIF ( ISRTOU .EQ. IBA2 ) THEN
!
               CALL OUTHIS (LUNOUT, LCHOUT   , ITIME , MONAME, NDMPAR,
     +                      IDUMP , DANAM    , 0     , SYNAME, CONC  ,
     +                      NRVAR , OUNAM(K1), RIOBUF, INIOUT)
!
            ELSEIF ( ISRTOU .EQ. IBA3 ) THEN
!
               ALLOCATE(SURF(NOSEG))
               NAME = 'SURF'
               LGET = .TRUE.
               CALL VALUES ( NAME   , NOSEG  , SURF   , NOCONS , NOPA   ,
     +                       NOFUN  , NOSFUN , CONS   , CONAME , PARAM  ,
     +                       PANAME , FUNC   , FUNAME , SEGFUN , SFNAME ,
     +                       LGET   , IERR   )

               CALL SOBBAL ( NOTOT , ITIME , NOSYS , NOFLUX   , NDMPAR,
     J                       NDMPQ , NTDMPQ, ITSTOP, IMSTRT   , IMSTOP,
     J                       IQDMP , IPDMP , ASMASS, FLXINT   , STOCHI,
     J                       SYNAME, DANAM , MONAME, DMPQ     , NOBND ,
     J                       NOBTYP, BNDTYP, INWTYP, NOCONS   , CONAME,
     J                       CONS  , NOQ   , IPOINT, OUNAM(K1), INTOPT,
     J                       VOLUME, SURF  , NOSEG , LUNOUT   , LCHOUT,
     J                       INIOUT, DMPBAL, NOWST , NOWTYP   , WSTTYP,
     J                       IWASTE, INXTYP, WSTDMP, ISEGCOL  )
               DEALLOCATE (SURF)
!
            ENDIF
!
            IOUTPS(7,IOUT) = INIOUT
!
         endif !(mypart.eq.1)
!
  100    CONTINUE
!
!        Update K1, pointer in IOPOIN and OUNAM
!
         K1 = K1 + NRVAR
!
  200 CONTINUE

      if ( timon ) call timstop ( ithandl )
      RETURN
      END

