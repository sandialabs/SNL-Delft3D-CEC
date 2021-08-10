!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2020.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id: unstruc_startup.f90 65778 2020-01-14 14:07:42Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/unstruc_startup.f90 $

module unstruc_startup
!! Separates some startup/initialization procedures from the main program in net.f90

use unstruc_ini
use unstruc_files
use properties
use unstruc_messages
!use unstruc_version

implicit none

contains

!> Initialized global program settings
!! Used to be SUBROUTINE OPENING()
subroutine initProgram()
    use m_flowparameters
    use unstruc_colors
    use unstruc_model

    character(len=76)  :: filnam

    logical   :: jawel
    integer   :: istat
    integer :: numuni

    call initSysEnv                   ! Init paths



    call initGUI(1) ! READINI + INTINI

    ! Read hlp file
    FILNAM = trim(unstruc_basename)//'.hlp'
    INQUIRE(FILE = FILNAM,EXIST = JAWEL)
    IF (JAWEL) THEN
        call oldfil(MHLP,FILNAM)
    Endif
    if (mhlp < 1) then
        CALL SYSFIL(MHLP,FILNAM)
    ENDIF

    CALL HELPIN() ! TODO: help module? [AvD]

    !CALL IUPPERCASE(unstruc_program)

    CALL SETCOLTABFILE(coltabfile,0)
    CALL SETCOLTABFILE(coltabfile2,1)

    RETURN
END subroutine initProgram


!> Initializes some info on the system environment.
!! Used to be biggest part of SUBROUTINE OPENING()
subroutine initSysEnv()
    use unstruc_files
    use unstruc_version_module, only : unstruc_program, unstruc_basename
    implicit none

    integer :: larch
    integer :: lendum
    integer :: lenp
    integer :: lertxt
    integer :: lslash
    integer :: numuni
    integer :: nval
    CHARACTER  FILNAM*76
    character  errtxt*8,arch*10,hlpstr*999,slash*1
    LOGICAL JAWEL ,d3dhom
    character(1), external :: get_dirsep
!-----------------------------------------------------------------------
!-----Environment variable defined as D3D_HOME-ARCH-PROGNM-
!     or RGForQN_PATH-
!-----------------------------------------------------------------------
      hlpstr = ' '
      pathdi = ' '
      nval   = 0
      d3dhom = .false.
!

!-----------------------------------------------------------------------
!-----Environment variable D3D_HOME not defined or directory not found
!     Initialize PROGRAM PATH
!-----------------------------------------------------------------------
      if (nval   .ne. 0) then
         d3dhom = .false.
         lendum = LEN(pathdi) - 1
         nval   = 0
         if (unstruc_program .eq. 'QUICKIN' ) then
            errtxt = 'QN_PATH'
         else if (unstruc_program .eq. 'rgfgrid')then
            errtxt = 'RGF_PATH'
         else if (unstruc_program .eq. 'KERNfl' ) then
            errtxt = 'FLS_PATH'
         else if (unstruc_program .eq. 'NETWORK' ) then
            errtxt = 'NET_PATH'
         endif
         LERTXT = len_trim(ERRTXT)
         hlpstr = errtxt(:lertxt)//CHAR (0)
         call  HCACCESS(nval     ,lendum    ,hlpstr    )
      endif
!-----------------------------------------------------------------------
!-----If not found just give error messages and go ahead
!-----------------------------------------------------------------------
      if (nval .ne. 0) then
!        if (nval .eq. -111) then
!           write(*,*) '*** WARNING Environment variable '//errtxt//
!    *                 ' not found. Check Installation procedure'
!        elseif (nval .eq. -11) then
!           write(*,*) '*** WARNING Environment variable '//errtxt//
!    *                 ' to long. Check Installation procedure'
!        else
!           write(*,*) '*** WARNING Directory for '//errtxt//
!    *                 ' not found. Check Installation procedure'
!        endif
      else
!-----------------------------------------------------------------------
!--------Find out if system is PC (directory seperator character \ (92)
!        or UNIX (directory seperator character / (47))
!-----------------------------------------------------------------------
         slash = get_dirsep()
!-----------------------------------------------------------------------
!--------Define directory when environment variable is D3D_HOME etc.
!-----------------------------------------------------------------------
         LENDUM = len_trim(HLPSTR)
         LARCH  = len_trim(ARCH)
         if (d3dhom) then
            if (larch .eq. 0) then
               pathdi = hlpstr(:lendum)//slash//unstruc_basename//slash
            else
               pathdi = hlpstr(:lendum)//slash//arch  (:larch)//slash//unstruc_basename//slash
            endif
         else
!-----------------------------------------------------------------------
!-----------Define directory when environment variable is QN/RGF_PATH
!-----------------------------------------------------------------------
            pathdi = hlpstr(:lendum)//slash
         endif
      endif
end subroutine

SUBROUTINE HCACCESS(nval     ,larch     ,arch      )
      implicit none
      integer :: infoopsystem
      integer :: l
      integer :: larch
      integer :: lendum
      integer :: nopsys
      integer :: nval
      CHARACTER ARCH*(*), HULPSTR*64
      NOPSYS = INFOOPSYSTEM(1)
      HULPSTR =  &
      '                                                                '
      CALL get_environment_variable(trim(arch), HULPSTR)
      LENDUM = len_trim(HULPSTR)
      IF (LENDUM .GT. 0) THEN
         IF (LENDUM .LE. LARCH) THEN
            NVAL = 0
            WRITE(ARCH,'(A)') HULPSTR(1:LENDUM)
         ELSE
            NVAL = -11
         ENDIF
      ELSE
         NVAL = -111
      ENDIF
      RETURN
      END subroutine hcaccess

    !> Initializes interface/screen settings
    !! Used to be SUBROUTINE REACOL
subroutine initGUI(INTINIT)
    USE M_MISSING
    use unstruc_display
    use unstruc_version_module, only : unstruc_basename

    implicit none
    double precision :: croshrsz
    double precision :: dv
    double precision :: dx
    double precision :: dxshow
    double precision :: dy
    integer :: i, INTINIT, ISTAT
    integer :: iblue
    integer :: icl
    integer :: ifltyp
    integer :: igreen
    integer :: ihcopts
    integer :: ihmous
    integer :: ired
    integer :: ivmous
    integer :: ja
    integer :: jaauto
    integer :: jvga
    integer :: k
    integer :: keepstartdir
    integer :: limslo
    integer :: limtel
    integer :: limwat
    integer :: ncols
    integer :: ndec
    integer :: ndraw
    integer :: nhcdev
    integer :: nie
    integer :: nis
    integer :: ntxcols
    integer :: ntxrows
    integer :: numhcopts
    integer :: nv
    integer :: nvec
    integer :: nxpix
    integer :: nypix, jaopengl_loc
    double precision :: rmiss
    double precision :: scalesize
    double precision :: signz
    double precision :: val
    double precision :: vfac
    double precision :: vfacforce
    double precision :: vmax
    double precision :: vmin
    double precision :: x0
    double precision :: xd
    double precision :: xsc
    double precision :: y0
    double precision :: ysc
    double precision :: tsize
    integer, dimension(4,50) :: rgbvalues
    logical :: jawel

    character(len=76)  :: filnam
    character(len=180) :: inifilename

    CHARACTER REC*132
    COMMON /CSPEED/  LIMTEL, LIMSLO, LIMWAT, IHMOUS, IVMOUS
    COMMON /INITSCREEN/  CROSHRSZ,JVGA,NXPIX,NYPIX,NTXCOLS,NTXROWS
    COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
    COMMON /TEXTSIZE/ TSIZE

    COMMON /HARDCOPY/ NHCDEV,NUMHCOPTS,IHCOPTS(2,20)
    COMMON /OLDORNEWNAMES/ IFLTYP
    COMMON /STARTDIR/  KEEPSTARTDIR
    COMMON /SCALEPOS/ XSC,YSC,SCALESIZE,NDEC
    COMMON /VFAC/ VFAC, VFACFORCE, NVEC
    COMMON /ARCINFO/ DX, DY, X0, Y0, RMISS, DXSHOW, XD
    COMMON /DRAWTHIS/ ndraw(50)


    ! Read ini file
    FILNAM = trim(unstruc_basename)//'.ini'
    INQUIRE(FILE = FILNAM,EXIST = JAWEL)
    IF (JAWEL) THEN
        inifilename = filnam
    else
        call sysfilepath(filnam, inifilename)
    end if

    call readIniFile(inifilename, istat)


    !CALL ZOEKAL (MINI,REC, 'INITSCREEN',JA)
    !READ (MINI,'(A)',END = 888) REC
    !READ ( REC,*,ERR = 999) JVGA,NXPIX,NYPIX,NTXCOLS,NTXROWS
    call get_req_integer(ini_ptr, 'screen', 'JVGA',    JVGA)
    call get_req_integer(ini_ptr, 'screen', 'NXPIX',   NXPIX)
    call get_req_integer(ini_ptr, 'screen', 'NYPIX',   NYPIX)
    call get_req_integer(ini_ptr, 'screen', 'NTXCOLS', NTXCOLS)
    call get_req_integer(ini_ptr, 'screen', 'NTXROWS', NTXROWS)


    !      CALL ZOEKAL (MINI,REC, '@GRAFCOL',JA)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLDG',    NCOLDG)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLRG',    NCOLRG)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLDN',    NCOLDN)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLRN',    NCOLRN)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLNN',    NCOLNN)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLSP',    NCOLSP)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLLN',    NCOLLN)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLTX',    NCOLTX)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLPL',    NCOLPL)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLCRS',   NCOLCRS)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLTHD',   NCOLTHD)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLFXW',   NCOLFXW)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLMH',    NCOLMH)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLWARN1', NCOLWARN1)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLWARN2', NCOLWARN2)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLHL',    NCOLHL)
    call prop_get_integer(ini_ptr, 'grafcol', 'NCOLANA',   NCOLANA)

    call prop_get_integer(ini_ptr, 'grafcol', 'KLVEC',  KLVEC)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLAXS',  KLAXS)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLSCL',  KLSCL)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLTEX',  KLTEX)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLFRA',  KLFRA)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLOBS',  KLOBS)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLSAM',  KLSAM)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLZM',   KLZM)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLANK',  KLANK)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLPROF', KLPROF)
    call prop_get_integer(ini_ptr, 'grafcol', 'KLSRC',  KLSRC)
    
    ! Cursor speed (in graphic mode)
    LIMTEL=200
    LIMSLO=20
    LIMWAT=400
    IHMOUS=40
    IVMOUS=40 !(orgiginal values 200, 20, 400)
    !Decrease LIMTEL if cursor movement responds too slow to the arrow keys.
    !LIMSLO gives the maximum increase of the cursor-postion
    !per time step in pixels. Increase it for higher maximum speeds.
    !LIMWAT gives the number of cycles to wait after the
    !'Ins'- or 'Enter'-key or left/right mouse buttons have been pressed.
    !Decrease if response is too slow, increase if response is too fast.
    !IHMOUS, IVMOUS mouse sensitivity, larger numbers, more hand movement

    ! size + position of HELP text screen
    NPOS(1)=2
    NPOS(2)=2
!    NPOS(3)=78
    NPOS(3)=120
    NPOS(4) = 16

    CR = .004             ! size of circle relative to screen size
    CROSHRSZ = .01        ! size of crosshair cursor relative to screen size

    ! Color scheme isolines
    call prop_get_string(ini_ptr, 'isocol', 'COLTABFILE', coltabfile)
    inquire(file = trim(coltabfile), exist = jawel)
    if (.not. jawel) then
        coltabfile = 'ISOCOLOUR.hls'
    end if

    coltabfile2 = coltabfile
    
    call get_req_integer(ini_ptr, 'isocol', 'AUTO', JAAUTO)
    call get_req_integer(ini_ptr, 'isocol', 'NV',   NV)
    call get_req_double (ini_ptr, 'isocol', 'VMIN', VMIN)
    call get_req_double (ini_ptr, 'isocol', 'VMAX', VMAX)
    NIS = 46   !INDEX FIRST ISOLINE COLOUR <1, 250>
    NIE = 224  !INDEX LAST  ISOLINE COLOUR <NIS+NV, 254>
    call prop_get_integer(ini_ptr, 'isocol', 'NIS', NIS)
    call prop_get_integer(ini_ptr, 'isocol', 'NIE', NIE)

    DV   = VMAX - VMIN
    DO I = 1,NV
        VAL(I) = VMIN + (I-1)*DV/(NV-1)
    ENDDO
    call inidepmax2()

    ! Text size
    call get_req_double(ini_ptr, 'text', 'TSIZE', TSIZE)

    ! Harcopy output
    ! (format of hardcopy output file)
    NHCDEV=6   ! (1:hpgl, 2:ps , 3:acorn, 4:raster,
               ! 5:tek , 6:pcx, 7:pic  , 8:dxf   ,
               ! 9:cgm ,12: hpgl2)
               ! (and windows only: 10 print manager, 11 windows metafile)

    call prop_get_integers(ini_ptr, 'hardcopyoptions', 'IHCOPTS', IHCOPTS, size(ihcopts))
    NUMHCOPTS = 0
    ! Determine actual number of HC-options read.
    do
        if (numhcopts >= size(ihcopts,2)) then
            exit
        endif
        if (ihcopts(1,numhcopts+1) == 0) then
            exit
        endif
        numhcopts = numhcopts + 1
    enddo

    call prop_get_integer(ini_ptr, 'display', 'NTEK', NTEK)
    call prop_get_integer(ini_ptr, 'display', 'PLOTTOFILE', plottofile)
    call prop_get_integer(ini_ptr, 'display', 'JADATETIME', jadatetime)
    jaopengl_loc = -1   ! unset
    call prop_get_integer(ini_ptr, 'display', 'JAOPENGL'  , jaopengl_loc)
    if ( jaopengl_loc.ne.-1 ) then
       call iset_jaopengl(jaopengl_loc)
    end if


    if (plottofile==1) then
        ndraw(10) = 1
    end if
    VFAC   = 1
    NVEC   = 1
    call prop_get_double(ini_ptr, 'display', 'VFAC', vfac)

    ! Old or new file names
    IFLTYP = 1       ! 0, OLD FILENAMES TELMCRGF.*, RGFLANDB.*
                     ! 1, NEW FILENAMES *.GRD, *.LDB, *.DEP, *.XYZ, *.A*,

    KEEPSTARTDIR = 0 ! 1 : always go back to startup directory
                     ! 0 : keep directory of latest directory change

! TODO: rgfspul Wordt elders gezet, maar niet alles (bijv fsma)
!    ! RGF SETTINGS
!    MFAC=5
!    NFAC=5
!    ITATP=3
!    ITBND=15
!    ITIN=25
!    ATPF=1.0
!    BFAC=1.0
!
!    CSMO=0.2
!    RFAC=.10
!    BAAS2=0.5
!    SRM=1
!    SRN=0.2          ! (SIZERATIO DEPTH/SLOPE DESIGN)
!    DEPSLO=1.00      ! (DEPTH/SLOPE DESIGN WEIGHT)
!    ITSMA=10
!    FSMA=.10         ! (DEPTH/SLOPE WEIGHT SMOOTHING)
!    ALINEN=0.0
!    ALINEM=0.0       ! LINE/FIELD WEIGHT, FIELD = 0, LINE = 1



!     Interactor klaarzetten
    IF (INTINIT == 1) THEN
       CALL INTINI()
    ENDIF

    NREDS    = 0
    NGREENS  = 0
    NBLUES   = 0
    NREDP    = 255
    NGREENP  = 255
    NBLUEP   = 200
    call prop_get_integer(ini_ptr, 'grafcol', 'NREDS'  , NREDS  )
    call prop_get_integer(ini_ptr, 'grafcol', 'NGREENS', NGREENS)
    call prop_get_integer(ini_ptr, 'grafcol', 'NBLUES' , NBLUES )
    call prop_get_integer(ini_ptr, 'grafcol', 'NREDP'  , NREDP  )
    call prop_get_integer(ini_ptr, 'grafcol', 'NGREENP', NGREENP)
    call prop_get_integer(ini_ptr, 'grafcol', 'NBLUEP' , NBLUEP )
    CALL IGRPALETTERGB(0,NREDS,NGREENS,NBLUES)

    call prop_get_integer(ini_ptr, 'display', 'JAFULLBOTTOMLINE' , jafullbottomline )

! TODO: Supporten we nog HLS? [AvD]
!      CALL ZOEKAL(MINI,REC,'HLSVALUES',JA)
!      IF (JA .EQ. 1) THEN
!  564    CONTINUE
!         READ(MINI,*,ERR=992,END=992) ICL,IRED,IGREEN,IBLUE
!         ICL   = MAX(1,MIN(ICL,255))
!         IRED  = MAX(0,MIN(IRED   ,255))
!         IGREEN= MAX(0,MIN(IGREEN ,255))
!         IBLUE = MAX(0,MIN(IBLUE  ,255))
!         CALL IGRPALETTEHLS(ICL,IRED,IGREEN,IBLUE)
!         GOTO 564
!      ENDIF
!
!  992 CONTINUE

!      CALL ZOEKAL(MINI,REC,'RGBVALUES',JA)
    rgbvalues(:,:) = 0
    rgbvalues(1:4,1)  = (/ 210,    3,    3,    3 /)
    rgbvalues(1:4,2)  = (/ 211,    1,  128,  255 /)  ! NCOLRN = SHOW ALL LINKS/prev net
    rgbvalues(1:4,3)  = (/ 212,  255,  160,  192 /)  ! NCOLRG = prev grid
    rgbvalues(1:4,4)  = (/ 210,  200,  200,  200 /)  ! NCOLTX = SOME TEXTST 
    rgbvalues(1:4,5)  = (/ 230,   32,  176,    0 /)  ! NCOLCRS = CROSS SECTIONS
    rgbvalues(1:4,6)  = (/ 231,  255,    0,    0 /)  ! NCOLTHD = THIN DAMS
    rgbvalues(1:4,7)  = (/ 232,  255,  106,    0 /)  ! NCOLFXW = FIXED WEIRS
    rgbvalues(1:4,8)  = (/ 227,    0,  200,  200 /)  ! KLOBS = OBS.STATIONS
    rgbvalues(1:4,9)  = (/ 203,    0,  255,  255 /)  ! NCOLLN = LAND BOUNDARY
    rgbvalues(1:4,10) = (/ 204,  255,  255,  150 /)  ! NCOLSP = SPLINES
    rgbvalues(1:4,11) = (/ 205,  255,  255,  150 /)  ! NCOLNN = NET NODES (in case they differ from splines)

    ! Initialise more standard colors.
    ! Used most colors from the HTML 4.01 specification, see http://www.w3.org/TR/REC-html40/types.html#h-6.5
    ! and added some basic colors.
    !call IGRPALETTERGB(ncolgray, 128, 128, 128) ! gray is already set by default (background color).
    i = 11
    i=i+1; rgbvalues(1:4, i) = (/ ncolblack, 0, 0, 0 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolwhite, 255, 255, 255 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolred, 255, 0, 0 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolyellow, 255, 255, 0 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolgreen, 0, 255, 0 /) !< lime
    i=i+1; rgbvalues(1:4, i) = (/ ncolcyan, 0, 255, 255 /) !< aqua
    i=i+1; rgbvalues(1:4, i) = (/ ncolblue, 0, 0, 255 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolmagenta, 255, 0, 255 /) !< fuchsia
    i=i+1; rgbvalues(1:4, i) = (/ ncolmaroon, 128, 0, 0 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncoldarkgreen, 0, 128, 0 /) !< green
    i=i+1; rgbvalues(1:4, i) = (/ ncolteal, 0, 128, 128 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolpink, 255, 0, 128 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolorange, 255, 128, 0 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncollavender, 128, 128, 255 /)
    i=i+1; rgbvalues(1:4, i) = (/ ncolbrown, 128, 64, 0 /)
    K=1
    ! First load default colours into Interacter colors:
    do
        if (rgbvalues(1,k) == 0) then
            exit
        end if
        ICL   = MAX(1,MIN(rgbvalues(1,k),255))
        IRED  = MAX(0,MIN(rgbvalues(2,k),255))
        IGREEN= MAX(0,MIN(rgbvalues(3,k),255))
        IBLUE = MAX(0,MIN(rgbvalues(4,k),255))
        K=K+1
        CALL IGRPALETTERGB(ICL,IRED,IGREEN,IBLUE)
    end do


    ! Reset again
    rgbvalues(:,:) = 0
    ! And override with colors from inifile.
    call prop_get_integers(ini_ptr, 'grafcol','rgbvalues', rgbvalues, size(rgbvalues))
    k = 1
    do
        if (rgbvalues(1,k) == 0) then
            exit
        end if
        ICL   = MAX(1,MIN(rgbvalues(1,k),255))
        IRED  = MAX(0,MIN(rgbvalues(2,k),255))
        IGREEN= MAX(0,MIN(rgbvalues(3,k),255))
        IBLUE = MAX(0,MIN(rgbvalues(4,k),255))
        k = k+1
        CALL IGRPALETTERGB(ICL,IRED,IGREEN,IBLUE)
    end do
!     CALL READXYMIS(MINI)
!     CALL READAMISS(MINI)

      TXLIN  =  ' ' ! alle drie leeg

      TXSIZE = 0.75d0
      TXXpos = 0.5d0
      TXYpos = 0.015d0

      XSC    = 0.01d0
      YSC    = 0.07d0
      NDEC   = 3
      SCALESIZE = 0.5d0

      X0         = 1d0
      Y0         = 1d0
      DX         = 100
      DY         = 100


      RETURN
END subroutine initGUI

end module unstruc_startup
