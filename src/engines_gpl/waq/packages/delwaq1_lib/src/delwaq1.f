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

      subroutine delwaq1(argc,  argv, errorcode)

      !DEC$ ATTRIBUTES DLLEXPORT::delwaq1

!>\file
!>                    DELWAQ - INPUT PROGRAMME
!>
!>                    Reads the DELWAQ inputfiles and generates
!>                    a consistent set of binairy intermediate files.

!     INFORMATION   : Deltares
!                     L. Postma,
!                     Rotterdamse weg 185,
!                     P.O. Box 177,
!                     2600 MH Delft,
!                     Netherlands.
!                     telephone (31) 15-569353
!                     telefax   (31) 15-619674
!
!     LOGICAL UNITS : LUN(29), output, formatted report file
!                     LUN( 1), output, binary common-block file
!                     LUN( 2), output, binary system file
!
!     SUBROUTINES CALLED :*UNLOCK, unlocks user dependent data
!                         *UNISET, reads input filename
!                          DLWQ01, reads block 1 of user data
!                          DLWQ02, reads block 2 of user data
!                          DLWQ03, reads block 3 of user data
!                          DLWQ04, reads block 4 of user data
!                          DLWQ05, reads block 5 of user data
!                          DLWQ06, reads block 6 of user data
!                          DLWQ07, reads block 7 of user data
!                          DLWQ7A, reads block 7 of user data new style
!                          DLWQ08, reads block 8 of user data
!                          DLWQ09, reads block 9 of user data
!                          DLWQS1, reads block 10 , statistical definition
!                          DLWQP1, proces pre-processor
!                          SPACE , computes space needed
!                          DLWQDI, writes dimensions of arrays for DELWAQ2
!                         *DHOPNF, opens files ( if neccesary )
!                         *SRSTOP, stops execution
!
!                         *, this routines can contain sytem dependencies
!
!
      use Grids        !   for the storage of contraction grids
      use dlwq_data    !   for definition and storage of data
      use Output       !   for the output names and pointers
      use timers       !   performance timers
      use dhcommand
!
      use     D00SUB
      use     ProcesSet
      use     Workspace
      use     Rd_token

      implicit none
!
!     common  /  SYSN   /   System characteristics
!
      include 'sysn.inc'
!
!     common  /  SYSI  /    Timer characteristics
!
      include 'sysi.inc'
      include 'sysa.inc'
      include 'sysj.inc'
      include 'sysc.inc'

      integer, intent(in)                           :: argc
      character(len=*), dimension(argc), intent(in) :: argv
      integer, intent(out)                          :: errorcode
!
!     output structure common blocks
!
      integer             in(insize)       , ii(iisize)         ! arrays to write common block to file
      equivalence       ( in(1)  , noseg ) , ( ii(1), itstrt  ) ! equivalence output array with common block
!
!     work arrays
!
      integer, parameter             :: iimax = 2500000         ! default size integer work array
      integer, parameter             :: irmax =10000000         ! default size real work array
      integer, parameter             :: icmax = 1000000         ! default size character work array
      integer                        :: imax                    ! dynamic size integer work array
      integer                        :: rmax                    ! dynamic size real work array
      integer                        :: cmax                    ! dynamic size character work array
      integer          , allocatable :: iar(:)                  ! integer work array
      real             , allocatable :: rar(:)                  ! real work array
      character(len=20), allocatable :: car(:)                  ! character work array

      real,              dimension(:), pointer :: abuf  => null()
      integer,           dimension(:), pointer :: ibuf  => null()
      character(len=20), dimension(:), pointer :: chbuf => null()

!     files, unit numbers, include file stack, input file settings
!
      integer, parameter             :: nlun   = 45              ! number of input / output files
!     integer, parameter             :: lstack = 4               ! size include files stack
!     integer, parameter             :: lchmax = 255             ! sring length file name variables
      integer                        :: lun(nlun)                ! unit numbers input / output files
      integer                           filtype(nlun)
      character(len=lchmax)          :: runid                    ! runid
      character(len=lchmax)          :: lchar(nlun)              ! file names input / output files
!     character(len=lchmax)          :: lch(lstack)              ! file names include files stack
!     integer                        :: ilun(lstack)             ! unit numbers include files stack
!     character                      :: cchar                    ! comment character
      logical                        :: dtflg1                   ! first flag concerning time formats
      logical                        :: dtflg2                   ! second flag concerning time formats
      logical                        :: dtflg3                   ! third flag concerning time formats
      type(inputfilestack)           :: inpfil                   ! input file strucure with include stack and flags
!
!     variaous input-output structures
!
      integer, parameter             :: noitm  = 11              ! number of items with time-functions
      integer, parameter             :: noint  = 192             ! number of integration options implemented
      integer, parameter             :: nooutp = 9               ! number of output files
      integer                        :: nrftot(noitm)            ! number of function per item
      integer                        :: nrharm(noitm)            ! number of harmoncs per item
      integer                        :: iopt(noint)              ! integration option list
      integer                        :: ioutps(7,nooutp)         ! output file defintion structure
      character(len=20), pointer     :: psynam(:)                ! substance names read buffer copies into syname
      integer( 4)      , pointer     :: multp(:,:)               ! multiplication substances pointer copies into imultp
      character(len=20), allocatable :: syname(:)                ! substance names final array
      integer( 4)      , allocatable :: imultp(:,:)              ! multiplication substances pointer
      integer           ,pointer     :: nsegdmp(:)               ! number of monitored segments
      integer           ,pointer     :: isegdmp(:)               ! segment numbers of monitored segments
      integer           ,pointer     :: nexcraai(:)              ! number of exchanges used in transects
      integer           ,pointer     :: iexcraai(:)              ! exchange numbers used in transects
      integer           ,pointer     :: ioptraai(:)              ! option number for transects
      type(ProcesPropColl)           :: StatProcesDef            ! the statistical proces definition
      type(ItemPropColl)             :: AllItems                 ! all items of the proces system
      type(t_dlwq_item)              :: constants                ! delwaq constants list
!
!     help variables
!
      logical                        :: nolic                    ! No valid license?
      logical                        :: lfound                   ! help varaiable indicating if command line argument is found
      character(len=20)              :: rundat                   ! execution date-time string
      character                      :: cdummy
      real                           :: rdummy
      integer( 4)                    :: nomult            !< number of multiple substances
      integer( 4)                    :: iwidth            !< width of the output file
      real( 4)                       :: vrsion            !< version number of this input
      integer( 4)                    :: ioutpt            !< flag for more or less output
      integer                           ierr                     ! cumulative number of errors
      integer                           iwar                     ! cumulative number of warnings
      type(GridPointerColl) GridPs
      type(OutputColl     ) Outputs
      integer                           narg        ! nr of command line arguments
      character(lchmax)                 arg         ! a command line argument

      integer                        :: i, k, icmak
      integer                        :: itota
      integer                        :: itoti
      integer                        :: itotc
      integer                        :: ibflag
      integer                        :: lunrep
      integer                        :: nosss
      integer                        :: noinfo
      integer                        :: ierr_alloc
      logical                        :: unitop
      character(len=200)             :: nameoffile
      integer                        :: ioerr
!
!       initialisations
!
      data      lun / 14 , 15 , 16 , 17 , 18 , 19 , 20 , 21 , 22 , 23 ,
     *                24 , 25 , 26 , 27 , 28 , 29 , 30 , 31 , 32 , 33 ,
     *                34 , 35 , 36 , 37 , 38 , 39 , 40 , 41 , 42 , 43 ,
     *                44 , 45 , 46 , 47 , 48 , 49 , 50 , 51 , 52 , 53 ,
     *                54 , 55 , 56 , 57 , 58 /
      data      lchar  / '-delwaq03.wrk' , '-delwaq04.wrk' ,
     *                   '-harmonic.wrk' , '-pointers.wrk' ,
     *                   '-timestep.wrk' , '-gridding.wrk' ,
     *                   '-volumes.wrk ' , '-to_from.wrk ' ,
     *                   '-dispersi.wrk' , '-areas.wrk   ' ,
     *                   '-flows.wrk   ' , '-velocity.wrk' ,
     *                   '-lengthes.wrk' , '-boundary.wrk' ,
     *                   '-wastload.wrk' , '-function.wrk' ,
     *                   '-segfunc.wrk ' , '-initials.wrk' ,
     *                   '.mon         ' , '.dmp         ' ,
     *                   '.his         ' , '.map         ' ,
     *                   '.res         ' , '-proces.wrk  ' ,
     *                   '-output.wrk  ' , '.inp         ' ,
     *                   '             ' , '-delwaq02.wrk' ,
     *                   '.lst         ' , '-dlwqstrt.inc' ,
     *                   '-scratch1opt3' , '-scratch2opt3' ,
     *                   '-auxfileop1  ' , '-proces.def  ' ,
     *                   '.lsp         ' , '-stochi.inp  ' ,
     *                   '-bal.his     ' , '.hdf         ' ,
     *                   '.adf         ' , '-kenmerk.wrk ' ,
     *                   '-filenaam.wrk' , '-stat.map    ' ,
     *                   '-stat.mon    ' , '             ' ,
     *                   ' '             /
      data    iopt / 10 , 11 , 12 , 13 , 14 , 15 , 16 , 17 ,
     *               20 , 21 , 22 , 23 , 24 , 25 , 26 , 27 ,
     *               30 , 31 , 32 , 33 , 34 , 35 , 36 , 37 ,
     *               40 , 41 , 42 , 43 , 44 , 45 , 46 , 47 ,
     *               50 , 51 , 52 , 53 , 54 , 55 , 56 , 57 ,
     *               60 , 61 , 62 , 63 , 64 , 65 , 66 , 67 ,
     *               70 , 71 , 72 , 73 , 74 , 75 , 76 , 77 ,
     *               80 , 81 , 82 , 83 , 84 , 85 , 86 , 87 ,
     *               90 , 91 , 92 , 93 , 94 , 95 , 96 , 97 ,
     *              100 ,101 ,102 ,103 ,104 ,105 ,106 ,107 ,
     *              110 ,111 ,112 ,113 ,114 ,115 ,116 ,117 ,
     *              120 ,121 ,122 ,123 ,124 ,125 ,126 ,127 ,
     *              130 ,131 ,132 ,133 ,134 ,135 ,136 ,137 ,
     *              140 ,141 ,142 ,143 ,144 ,145 ,146 ,147 ,
     *              150 ,151 ,152 ,153 ,154 ,155 ,156 ,157 ,
     *              160 ,161 ,162 ,163 ,164 ,165 ,166 ,167 ,
     *              170 ,171 ,172 ,173 ,174 ,175 ,176 ,177 ,
     *              180 ,181 ,182 ,183 ,184 ,185 ,186 ,187 ,
     *              190 ,191 ,192 ,193 ,194 ,195 ,196 ,197 ,
     *              200 ,201 ,202 ,203 ,204 ,205 ,206 ,207 ,
     *              210 ,211 ,212 ,213 ,214 ,215 ,216 ,217 ,
     *              220 ,221 ,222 ,223 ,224 ,225 ,226 ,227 ,
     *              230 ,231 ,232 ,233 ,234 ,235 ,236 ,237 ,
     *              240 ,241 ,242 ,243 ,244 ,245 ,246 ,247 /

!     Special system init

      integer(4), save         :: ithndl = 0
      call timini ( )                          ! initializes timer
      
      call dhstore_command( argv )

      narg = dhstored_number_args()            ! but timer is switched 'off' by default
      if ( narg .eq. 0 ) narg = iargc() + 1
      do ierr = 1, narg
         call dhgarg ( ierr, arg )
         if ( arg .eq. "timer" .or. arg .eq. "TIMER" ) then
            timon = .true.                     ! optionally switch it 'on'
            exit
         endif
      enddo
      if (timon) call timstrt( "delwaq1", ithndl )
      call avundf

!        initialise values

      ierr   = 0
      iwar   = 0
      lunrep = lun(29)
      nolun  = nlun
      filtype = 0
      noitem = noitm
      noutp  = nooutp
      noinfo = 0
      nharms = 0
      niharm = 0
      nlines = 0
      npoins = 0
      newrsp = 0
      newisp = 0
      ivflag = 0
      itflag = 0
      ncbufm = 0
      novar  = 0
      noarr  = iasize + ijsize + icsize
      nufil  = 0
      do 10 i=1, noitem
        nrftot(i) = 0
        nrharm(i) = 0
   10 continue
      StatProcesDef%maxsize = 0
      StatProcesDef%cursize = 0
      AllItems%maxsize = 0
      AllItems%cursize = 0
      GridPs%cursize=0
      GridPs%maxsize=0
!
      call uniset ( lun    , lchar , nolun , runid )
!
!     unscramble name user
!
      call unlock (lunrep,.false.,nolic)
      write(*,*)
      write(*,*) ' runid : ',trim(runid)
      write(*,*)
!
!     allocate workspace
!
      call getcom ( '-imax', 1 , lfound, imax  , rdummy, cdummy, ierr )
      if ( lfound ) then
         if ( ierr .eq. 0 ) then
            write(lunrep,2010) imax
         else
            write(lunrep,2020)
            ierr = 1
            goto 900
         endif
      else
         imax = iimax
      endif
      call getcom ( '-rmax', 1 , lfound, rmax  , rdummy, cdummy, ierr )
      if ( lfound ) then
         if ( ierr .eq. 0 ) then
            write(lunrep,2030) rmax
         else
            write(lunrep,2040)
            ierr = 1
            goto 900
         endif
      else
         rmax = irmax
      endif
      call getcom ( '-cmax', 1 , lfound, cmax  , rdummy, cdummy, ierr )
      if ( lfound ) then
         if ( ierr .eq. 0 ) then
            write(lunrep,2050) cmax
         else
            write(lunrep,2060)
            ierr = 1
            goto 900
         endif
      else
         cmax = icmax
      endif
      allocate(iar(imax),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then
         write ( lunrep , 2070 ) ierr_alloc,imax
         ierr = 1
         goto 900
      endif
      allocate(rar(rmax),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then
         write ( lunrep , 2080 ) ierr_alloc,rmax
         ierr = 1
         goto 900
      endif
      allocate(car(cmax),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then
         write ( lunrep , 2090 ) ierr_alloc,cmax
         ierr = 1
         goto 900
      endif
!
      cchar   = ' '
      ilun    = 0
      ilun(1) = lun  (26)
      lch (1) = lchar(26)
      lunut   = lun(29)
      call dlwq01 ( lun     , psynam  , nosys   , notot   , nomult  ,
     &              multp   , iwidth  , otime   , isfact  , vrsion  ,
     &              ioutpt  , ierr    , iwar    )
      allocate(syname(notot+nomult),stat=ierr_alloc)
      allocate(imultp( 2 ,  nomult),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then
         write ( lunrep , 2000 ) ierr_alloc
         ierr = ierr + 1
         goto 900
      endif
      syname = psynam
      imultp = multp
      deallocate(psynam)
      deallocate(multp )
      deltim = otime
      car(1) = ' '
      k = 2
      icmak = cmax   - 1

      nullify(nsegdmp)
      nullify(isegdmp)
      nullify(nexcraai)
      nullify(iexcraai)
      nullify(ioptraai)
      call dlwq02 ( lun     , lchar   , filtype , nrftot  , nlines  ,
     &              npoins  , dtflg1  , dtflg2  , nodump  , iopt    ,
     &              noint   , iwidth  , dtflg3  , ndmpar  , ntdmps  ,
     &              noraai  , ntraaq  , nosys   , notot   , nototp  ,
     &              vrsion  , ioutpt  , nsegdmp , isegdmp , nexcraai,
     &              iexcraai, ioptraai, ierr    , iwar    )

      if ( mod(intopt,16) .gt. 7 ) then
         ibflag = 1
      else
         ibflag = 0
      endif

      call dlwq03 ( lun     , lchar   , filtype , nrftot  , nrharm  ,
     &              ivflag  , dtflg1  , iwidth  , dtflg3  , vrsion  ,
     &              ioutpt  , gridps  , syname  , ierr    , iwar    )

      if ( nolic .and. noseg > 150 ) then
         write(*,'(//a)') 'Error: Authorisation problem'
         write(*,'(a)')   '       No valid license, so the number of segments is limited to 150'
         call srstop(1)
      endif

      if ( .not. associated(nsegdmp)  ) allocate(nsegdmp(1))
      if ( .not. associated(isegdmp)  ) allocate(isegdmp(1))
      if ( .not. associated(nexcraai) ) allocate(nexcraai(1))
      if ( .not. associated(iexcraai) ) allocate(iexcraai(1))
      if ( .not. associated(ioptraai) ) allocate(ioptraai(1))
      call dlwq04 ( lun     , lchar   , filtype , nrftot  , nrharm  ,
     &              ilflag  , dtflg1  , iwidth  , intsrt  , dtflg3  ,
     &              vrsion  , ioutpt  , nsegdmp , isegdmp , nexcraai,
     &              iexcraai, ioptraai, gridps  , ierr    , iwar    )
      if ( associated(nsegdmp)  ) deallocate(nsegdmp)
      if ( associated(isegdmp)  ) deallocate(isegdmp)
      if ( associated(nexcraai) ) deallocate(nexcraai)
      if ( associated(iexcraai) ) deallocate(iexcraai)
      if ( associated(ioptraai) ) deallocate(ioptraai)

      deltim = otime
      call dlwq05 ( lun    , lchar  , filtype, car(k) , iar    ,
     *              rar    , nrftot , nrharm , nobnd  , nosys  ,
     *              notot  , nobtyp , rmax   , imax   , dtflg1 ,
     *              iwidth , intsrt , ierr   , dtflg3 , syname ,
     *              icmak  , vrsion , ioutpt , iwar   )
!
      deltim = otime

      nosss = noseg + nseg2     ! increase with bottom segments
      call dlwq06 ( lun    , lchar  , filtype, icmak  , car(k) ,
     &              imax   , iar    , rmax   , rar    , notot  ,
     &              nosss  , syname , nowst  , nowtyp , nrftot ,
     &              nrharm , dtflg1 , dtflg3 , iwidth , vrsion ,
     &              ioutpt , ierr   , iwar   )
!
      novec = 50
      inpfil%dtflg1 = dtflg1
      inpfil%dtflg2 = dtflg2
      inpfil%dtflg3 = dtflg3
      inpfil%itfact = itfact
      inpfil%vrsion = vrsion
      if ( vrsion .le. 4.90 ) then
         nrharm(10) = 0
         call dlwq07 ( lun    , lchar    , filtype, noseg  , nocons ,
     &                 nopa   , nofun    , nosfun , itfact , dtflg2 ,
     &                 dtflg3 , iwidth   , novec  , vrsion , ioutpt ,
     &                 nothrd , constants, ierr   , iwar   )
      else
         nrharm(10) = 0
         deltim     = otime
         call dlwq7a ( lun    , lchar  , filtype, inpfil   , syname ,
     &                 iwidth , ioutpt , gridps , constants, ierr   ,
     &                 iwar   )
      endif
!
!     Finish and close system file ( DLWQ09 can re-read it )
!
      write ( lun(2) ) ( nrftot(i) , i = 1,noitem )
      write ( lun(2) ) ( nrharm(i) , i = 1,noitem )
      close ( lun(2) )

      call dlwq08 ( lun    , lchar  , filtype, nosss  , notot  ,
     &              syname , iwidth , vrsion , ioutpt , inpfil ,
     &              gridps , ierr   , iwar   )

      call dlwq09 ( lun    , lchar  , filtype, car    , iar    ,
     +              icmak  , iimax  , iwidth , ibflag , vrsion ,
     +              ioutpt , ioutps , outputs, ierr   , iwar   )
!
      call dlwqs1 ( lunrep       , npos         ,
     +              cchar        , vrsion       ,
     +              ilun         , lch          ,
     +              lstack       , ioutpt       ,
     +              dtflg1       , dtflg3       ,
     +              statprocesdef, allitems     ,
     +              noinfo       , iwar         ,
     +              ierr         )
      write ( lunrep,'(//'' Messages presented in this .lst file:'')')
!jvb  write ( lunrep,'( /'' Number of INFOrmative messages:'',I6)') noinfo
      write ( lunrep,'( /'' Number of WARNINGS            :'',I6)') iwar
      write ( lunrep,'(  '' Number of ERRORS during input :'',I6)') ierr
      write ( lunrep,'(  '' '')')
!
      call dlwqp1 ( lun          , lchar        ,
     +              statprocesdef, allitems     ,
     +              ioutps       , outputs      ,
     +              nomult       , imultp       ,
     +              constants    , noinfo       ,
     +              iwar         , ierr         )

      deallocate(syname)
      deallocate(imultp)
!
  900 continue
      write ( lunrep,'(//'' Messages presented including .lsp file:'')')
!jvb  write ( lunrep,'( /'' Number of INFOrmative messages:'',I6)') noinfo
      write ( lunrep,'(  '' Number of WARNINGS            :'',I6)') iwar
      write ( lunrep,'( /'' Number of ERRORS during input :'',I6)') ierr
      write (   *   ,'(  '' Number of WARNINGS            :'',I6)') iwar
      write (   *   ,'(  '' Number of ERRORS during input :'',I6)') ierr
      write (   *   ,'(  '' '')')
!
      if ( ierr .eq. 0 ) then
         novec = min(novec,(nosss+nobnd-1))
         itota = 0
         itoti = 0
         itotc = 0
         call space  ( lunrep, .false., abuf   , ibuf   , chbuf  ,
     +                 itota , itoti  , itotc  )
!
         call dhopnf  ( lun(1) , lchar(1) , 1     , 1     , ioerr )
         write ( lun(1) )   in
         write ( lun(1) )   ii
         write ( lun(1) )   itota , itoti , itotc
         write ( lun(1) ) ( lun    (k) , k = 1,nolun  )
         write ( lun(1) ) ( lchar  (k) , k = 1,nolun  )
         write ( lun(1) ) ( filtype(k) , k = 1,nolun  )
      else
         write ( lunrep , '(  '' SIMULATION PROHIBITED !!!!!!!!'')' )
         call dhopnf  ( lun(1) , lchar(1) , 1     , 3     , ioerr )
         call srstop ( 1 )
      endif
!
      call dattim(rundat)
      write (lunrep,'(2A)') ' Execution stop : ',rundat
      close ( lunrep )
      
!
! Close all open LUN files
!
      do i = 1, nlun
          inquire (unit=lun(i), opened=unitop)
          if (unitop) then
              close (unit = lun(i))
          endif
      end do

      if ( timon ) then
         call timstop ( ithndl )
         call timdump ( TRIM(RUNID)//'-delwaq1-timers.out' )
      endif

! Delwaq1_lib should never use a stop, but must be modified to return an error code instead (0 = normal end)
! Currently a return from the delwaq1_lib assumes a normal end.
      errorcode = 0
      return

 2000 format (  /,' ERROR: allocating memory for system names:',I6)
 2010 format (  /,' Command line argument -IMAX, size of integer work array:',I12)
 2020 format (  /,' ERROR: interpreting command line argument -IMAX, size of integer work array:')
 2030 format (  /,' Command line argument -RMAX, size of real work array:',I12)
 2040 format (  /,' ERROR: interpreting command line argument -RMAX, size of real work array:')
 2050 format (  /,' Command line argument -CMAX, size of character work array:',I12)
 2060 format (  /,' ERROR: interpreting command line argument -CMAX, size of character work array:')
 2070 format (  /,' ERROR: allocating integer work array:',I6,' with length:',I12)
 2080 format (  /,' ERROR: allocating real work array:',I6,' with length:',I12)
 2090 format (  /,' ERROR: allocating character work array:',I6,' with length:',I12)
      end
