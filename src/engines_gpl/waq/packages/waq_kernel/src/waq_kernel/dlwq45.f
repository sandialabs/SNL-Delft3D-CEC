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

      SUBROUTINE DLWQ45 ( LUN    , ITIME  , ITIMEL , HARMAT , ARRAY  ,
     *                    IHARM  , NRHARM , NRFTOT , NOSYS  , NOTOT  ,
     *                    NOSEG  , NOQ    , NODISP , NOVELO , NOBND  ,
     *                    NOWST  , NOFUN  , NOSFUN , IDT    , VOLUME ,
     *                    DISPER , AREA   , FLOW   , VELO   , ALENG  ,
     *                    WASTES , BOUNDS , FUNCS  , SFUNCS , IPOINT ,
     *                    LUNTXT , ISFLAG , IFFLAG , IVFLAG , ILFLAG ,
     *                    UPDATR , IKTIM  , IKNMRK , IFIOPK , INWSPC ,
     *                                      ANWSPC , INWTYP , IWORK  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: april- 8-1988 by L.Postma
!
!     FUNCTION            : Makes values at ITIME for all the time
!                                                   dependent aspects.
!
!     LOGICAL UNITS       : LUN(..) -
!
!     SUBROUTINES CALLED  : DLWQT1, makes one time function
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     LUN     INTEGER       ?     INPUT   array with unit numbers
!     ITIME   INTEGER       1     INPUT   Model timer
!     ITIMEL  INTEGER       1     INPUT   Model timer last time step
!     HARMAT  REAL          ?     IN/OUT  matrices harmonic components
!     ARRAY   REAL          ?     LOCAL   set of double file buffers
!     IHARM   INTEGER       ?     INPUT   harmonics time space
!     NRHARM  INTEGER       ?     INPUT   set of nrs of harmonic records
!     NRFTOT  INTEGER       ?     INPUT   set of record lengthes
!     NOSYS   INTEGER       1     INPUT   nr of active substances
!     NOTOT   INTEGER       1     INPUT   nr of total  substances
!     NOSEG   INTEGER       1     INPUT   nr of computational elements
!     NOQ     INTEGER       1     INPUT   nr of exchange surfaces
!     NODISP  INTEGER       1     INPUT   nr of dispersion arrays
!     NOVELO  INTEGER       1     INPUT   nr of velocity arrays
!     NOBND   INTEGER       1     INPUT   nr of boundary conditions
!     NOWST   INTEGER       1     INPUT   nr of wasteloads
!     NOFUN   INTEGER       1     INPUT   nr of functions
!     NOSFUN  INTEGER       1     INPUT   nr of segment functions
!     IDT     INTEGER       1     OUTPUT  integration time step size
!     VOLUME  REAL       NOSEG    OUTPUT  array of segment volumes
!     DISPER  REAL        NOQ     OUTPUT  array of dispersions
!     AREA    REAL        NOQ     OUTPUT  array of exchange surfaces
!     FLOW    REAL        NOQ     OUTPUT  array of flows
!     VELO    REAL    NOQ*NOVELO  OUTPUT  array of velocities
!     ALENG   REAL       NOQ*2    OUTPUT  array of from and to lengthes
!     WASTES  REAL   NOSYS*NOWST  OUTPUT  array of wasteloads
!     BOUNDS  REAL   NOSYS*NOBND  OUTPUT  array of boundary conditions
!     FUNCS   REAL       NOFUN    OUTPUT  array of function values
!     SFUNCS  REAL      NOSFUN    OUTPUT  array of segment functions
!     IPOINT  INTEGER       ?     INPUT   set of pointers to destination
!     LUNTXT  CHAR*(*)      ?     INPUT   text with the unit numbers
!     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
!     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
!     IVFLAG  INTEGER       1     INPUT   = 1 then computed volumes
!     ILFLAG  INTEGER       1     INPUT   = 0 then constant lengthes
!     UPDATR  LOGICAL       1     OUTPUT  .T. = transport matrix changed
!     IKTIM   INTEGER       *     IN/OUT  Timers in file
!     IKNMRK  INTEGER   NOSEG,*   IN/OUT  Kenmerk array
!     IFIOPK  INTEGER       1     IN/OUT  file option kenmerk array
!     INWSPC  INTEGER       *     IN/OUT  Integer space new time funs
!     ANWSPC  REAL          *     IN/OUT  Real space new time functions
!     INWTYP  INTEGER       *     INPUT   Types of items
!     IWORK   INTEGER       *     LOCAL   Integer workspace
!
!     DECLARATIONS        :
!
      use timers
      DIMENSION    LUN   (*) , HARMAT(*) , ARRAY (*) , IHARM (*) ,
     *             NRHARM(*) , NRFTOT(*) , VOLUME(*) , DISPER(*) ,
     *             AREA  (*) , FLOW  (*) , VELO  (*) , ALENG (*) ,
     *             WASTES(*) , BOUNDS(*) , FUNCS (*) , SFUNCS(*) ,
     *             IPOINT(*) , ADT   (1) , IKTIM (*) , IKNMRK(*) ,
     *             INWSPC(*) , ANWSPC(*) , INWTYP(*) , IWORK (*)
      CHARACTER*(*)LUNTXT(*)
      LOGICAL      UPDATR
!
!     Local
!
      LOGICAL      UPDATE, LSTREC, LREWIN
      REAL         RDUMMY(1)
      REAL         ADUMMY(1)
!
!     COMMON  /  SYST   /   System time function flags
!
      INCLUDE 'syst.inc'
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq45", ithandl )
!
!         initialisation
!
      IPH  = 1
      IPF  = 1
      IPA  = 1
      IPI  = 1
      IPNI = 1
      IPNA = 1
      IT   = 1
      UPDATR = .FALSE.
      ISNUL  = 0
      IDUMMY = 0
      LSTREC = .FALSE.
!
!         integration step size IDT
!
      IF ( NRFTOT( 1) .GT. 0 ) THEN
      CALL DLWQT1 ( LUN        , ITIME     , ITIMEL     , IHARM(IPF),
     *              HARMAT(IPH), ARRAY(IPA), IPOINT(IPI), ADT       ,
     *              1          , NRHARM( 1), 1          , NRFTOT( 1),
     *              IPA        , IPH       , IPF        , IPI       ,
     *              LUNTXT     , 5         , ISFLAG     , IFFLAG    ,
     *              UPDATE     , OTHSET    , 0          , IWORK     ,
     *              LSTREC     , LREWIN    , RDUMMY     )
      IDT = ADT(1) +   0.5
      IF ( UPDATE ) UPDATR = .TRUE.
      ENDIF
!
!         volumes
!
      IF ( NRHARM( 2)+NRFTOT( 2) .GT. 0 ) THEN
      IF ( IVFLAG     .EQ. 0 .OR. IFFLAG .EQ. 1 ) THEN
      CALL DLWQT1 ( LUN        , ITIME     , ITIMEL     , IHARM(IPF),
     *              HARMAT(IPH), ARRAY(IPA), IPOINT(IPI), VOLUME    ,
     *              1          , NRHARM( 2), NOSEG      , NRFTOT( 2),
     *              IPA        , IPH       , IPF        , IPI       ,
     *              LUNTXT     , 7         , ISFLAG     , IFFLAG    ,
     *              UPDATE     , OTHSET    , 0          , IWORK     ,
     *              LSTREC     , LREWIN    , RDUMMY     )
      IF ( UPDATE ) UPDATR = .TRUE.
      ENDIF
      ENDIF
!
!         dispersions
!
      IF ( NRHARM( 3) .GE. 0 ) THEN
      CALL DLWQT1 ( LUN        , ITIME     , ITIMEL     , IHARM(IPF),
     *              HARMAT(IPH), ARRAY(IPA), IPOINT(IPI), DISPER    ,
     *              NODISP     , NRHARM( 3), NOQ        , NRFTOT( 3),
     *              IPA        , IPH       , IPF        , IPI       ,
     *              LUNTXT     , 9         , ISFLAG     , IFFLAG    ,
     *              UPDATE     , OTHSET    , 0          , IWORK     ,
     *              LSTREC     , LREWIN    , RDUMMY     )
      IF ( UPDATE ) UPDATR = .TRUE.
      ENDIF
!
!         area
!
      IF ( NRHARM( 4) .GE. 0 ) THEN
      CALL DLWQT1 ( LUN        , ITIME     , ITIMEL     , IHARM(IPF),
     *              HARMAT(IPH), ARRAY(IPA), IPOINT(IPI), AREA      ,
     *              1          , NRHARM( 4), NOQ        , NRFTOT( 4),
     *              IPA        , IPH       , IPF        , IPI       ,
     *              LUNTXT     , 10        , ISFLAG     , IFFLAG    ,
     *              UPDATE     , OTHSET    , 0          , IWORK     ,
     *              LSTREC     , LREWIN    , RDUMMY     )
      IF ( UPDATE ) UPDATR = .TRUE.
      ENDIF
!
!         flow
!
      IF ( NRHARM( 5) .GE. 0 ) THEN
      CALL DLWQT1 ( LUN        , ITIME     , ITIMEL     , IHARM(IPF),
     *              HARMAT(IPH), ARRAY(IPA), IPOINT(IPI), FLOW      ,
     *              1          , NRHARM( 5), NOQ        , NRFTOT( 5),
     *              IPA        , IPH       , IPF        , IPI       ,
     *              LUNTXT     , 11        , ISFLAG     , IFFLAG    ,
     *              UPDATE     , OTHSET    , 0          , IWORK     ,
     *              LSTREC     , LREWIN    , RDUMMY     )
      IF ( UPDATE ) UPDATR = .TRUE.
      ENDIF
!
!         velocities
!
      IF ( NRHARM( 6) .GE. 0 ) THEN
      CALL DLWQT1 ( LUN        , ITIME     , ITIMEL     , IHARM(IPF),
     *              HARMAT(IPH), ARRAY(IPA), IPOINT(IPI), VELO      ,
     *              NOVELO     , NRHARM( 6), NOQ        , NRFTOT( 6),
     *              IPA        , IPH       , IPF        , IPI       ,
     *              LUNTXT     , 12        , ISFLAG     , IFFLAG    ,
     *              UPDATE     , OTHSET    , 0          , IWORK     ,
     *              LSTREC     , LREWIN    , RDUMMY     )
      IF ( UPDATE ) UPDATR = .TRUE.
      ENDIF
!
!         'from'- and 'to'-length
!
      IF ( NRHARM( 7) .GE. 0 .AND. ILFLAG .EQ. 1 ) THEN
      CALL DLWQT1 ( LUN        , ITIME     , ITIMEL     , IHARM(IPF),
     *              HARMAT(IPH), ARRAY(IPA), IPOINT(IPI), ALENG     ,
     *              2          , NRHARM( 7), NOQ        , NRFTOT( 7),
     *              IPA        , IPH       , IPF        , IPI       ,
     *              LUNTXT     , 13        , ISFLAG     , IFFLAG    ,
     *              UPDATE     , OTHSET    , 0          , IWORK     ,
     *              LSTREC     , LREWIN    , RDUMMY     )
      IF ( UPDATE ) UPDATR = .TRUE.
      ENDIF
!
!         boundaries
!
      IF ( NRHARM( 8) .GE. 0 .AND. .NOT. BNDSET )
     *CALL DLWQT1 ( LUN        , ITIME     , ITIMEL     , IHARM(IPF),
     *              HARMAT(IPH), ARRAY(IPA), IPOINT(IPI), BOUNDS    ,
     *              NOSYS      , NRHARM( 8), NOBND      , NRFTOT( 8),
     *              IPA        , IPH       , IPF        , IPI       ,
     *              LUNTXT     , 14        , ISFLAG     , IFFLAG    ,
     *              UPDATE     , BNDSET    , 0          , IWORK     ,
     *              LSTREC     , LREWIN    , RDUMMY     )
      IF ( BNDSET ) THEN
         CALL DLWQT1 ( LUN         , ITIME , ITIMEL    , INWSPC(IPNI),
     *                 ANWSPC(IPNA), ADUMMY, INWTYP(IT), BOUNDS      ,
     *                 NOSYS       , 0     , NOBND     , ISNUL       ,
     *                 IPNI        , IPNA  , IDUMMY    , IBNDMX      ,
     *                 LUNTXT      , 14    , ISFLAG    , IFFLAG      ,
     *                 UPDATE      , BNDSET, 0         , IWORK       ,
     *                 LSTREC      , LREWIN, RDUMMY    )
         IT     = IT + NOBND
!        ILP1 = 0
!        WRITE ( * , * ) ITIME
!        DO 2 ILPP = 1,NOBND
!           WRITE ( * , * ) ILPP
!           WRITE ( * , '(10F12.2)' ) (WASTES(ILP1+K),K=1,NOSYS)
!           ILP1 = ILP1 + NOSYS
!   2    CONTINUE
      ENDIF
!
!         wastes
!
      IF ( NRHARM( 9) .GE. 0 .AND. .NOT. WSTSET )
     *CALL DLWQT1 ( LUN        , ITIME     , ITIMEL     , IHARM(IPF),
     *              HARMAT(IPH), ARRAY(IPA), IPOINT(IPI), WASTES    ,
     *              NOTOT+1    , NRHARM( 9), NOWST      , NRFTOT( 9),
     *              IPA        , IPH       , IPF        , IPI       ,
     *              LUNTXT     , 15        , ISFLAG     , IFFLAG    ,
     *              UPDATE     , WSTSET    , 1          , IWORK     ,
     *              LSTREC     , LREWIN    , RDUMMY     )
      IF ( WSTSET ) THEN
         CALL DLWQT1 ( LUN         , ITIME , ITIMEL    , INWSPC(IPNI),
     *                 ANWSPC(IPNA), ADUMMY, INWTYP(IT), WASTES      ,
     *                 NOTOT+1     , 0     , NOWST     , ISNUL       ,
     *                 IPNI        , IPNA  , IDUMMY    , IWSTMX      ,
     *                 LUNTXT      , 15    , ISFLAG    , IFFLAG      ,
     *                 UPDATE      , WSTSET, 1         , IWORK       ,
     *                 LSTREC      , LREWIN, RDUMMY    )
         IT     = IT + NOWST
!        ILP1 = 0
!        WRITE ( * , * ) ITIME
!        DO 1 ILPP = 1,NOWST
!           WRITE ( * , * ) ILPP
!           WRITE ( * , '(20F6.2)' ) (WASTES(ILP1+K),K=1,NOTOT+1)
!           ILP1 = ILP1 + NOTOT + 1
!   1    CONTINUE
      ENDIF
!
!         functions
!
      IF ( NRHARM(10) .GE. 0 )
     *CALL DLWQT1 ( LUN        , ITIME     , ITIMEL     , IHARM(IPF),
     *              HARMAT(IPH), ARRAY(IPA), IPOINT(IPI), FUNCS     ,
     *              1          , NRHARM(10), NOFUN      , NRFTOT(10),
     *              IPA        , IPH       , IPF        , IPI       ,
     *              LUNTXT     , 16        , ISFLAG     , IFFLAG    ,
     *              UPDATE     , OTHSET    , 0          , IWORK     ,
     *              LSTREC     , LREWIN    , RDUMMY     )
!
!         segment functions
!
      IF ( NRHARM(11) .GE. 0 )
     *CALL DLWQT1 ( LUN        , ITIME     , ITIMEL     , IHARM(IPF),
     *              HARMAT(IPH), ARRAY(IPA), IPOINT(IPI), SFUNCS    ,
     *              NOSEG      , NRHARM(11), NOSFUN     , NRFTOT(11),
     *              IPA        , IPH       , IPF        , IPI       ,
     *              LUNTXT     , 17        , ISFLAG     , IFFLAG    ,
     *              UPDATE     , OTHSET    , 0          , IWORK     ,
     *              LSTREC     , LREWIN    , RDUMMY     )
!
!     property array
!
      CALL DLWQTK ( LUN    , ITIME  , IKTIM  , IKNMRK , NOSEG  ,
     +              40     , LUNTXT , ISFLAG , IFFLAG , IFIOPK )
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
