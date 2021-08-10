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

! $Id: unstruc_files.f90 65935 2020-02-05 14:57:26Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/unstruc_files.f90 $

module unstruc_files
!! Centralizes unstruc file management (formerly in REST.F90)

use unstruc_messages
use unstruc_version_module

implicit none

    integer, parameter :: ERR_FILENOTEXIST    = 1
    integer, parameter :: ERR_FILEALREADYOPEN = 2
    integer, parameter :: ERR_FILEACCESSDENIED= 3

    integer, parameter :: maxlength = 256  !< Max length of a file name (not checked upon).
    integer            :: maxnum = 0       !< Current length of filenames list.
    character(maxlength), dimension(:), allocatable :: filenames

    integer :: mdia = 0 !< File pointer to .dia file
    integer :: mini = 0 !< File pointer to .ini file
    integer :: mhlp = 0 !< File pointer to help file
    
    character(len=60)  :: pathdi          ! TODO: AvD: TEMP. moved from hwsw.inc
    character(len=86)  :: Filnammenu      ! name of selected file in nfiles
   
contains

subroutine init_unstruc_files()

end subroutine init_unstruc_files


!> Registers in the filenames list that a file is opened.
!! Use this instead of directly writing in the list (automatic realloc).
!! The actual open is not performed here.
subroutine reg_file_open(mfil, filename)

   use m_alloc

   integer,          intent(in) :: mfil     !< File unit number (e.g., from numuni)
   character(len=*), intent(in) :: filename


    if (mfil > maxnum) then
        MAXNUM = MFIL + 50
        call realloc(filenames, maxnum, fill=' ')
    end if

    filenames(mfil) = filename
end subroutine


!> 'Unregisters' a file name from the list upon closing.
!! The actual close is not performed here.
subroutine reg_file_close(mfil)
    integer,          intent(in) :: mfil

    filenames(mfil) = ' '
end subroutine


!> Closes all remaining files in the file list.
subroutine close_all_files()
    integer :: mfil

    do mfil = maxnum,1,-1
       if (mfil .ne. mdia) then ! SPvdP: need to close dia-file last
          close(mfil) ! No need to check file status, just attempt to close.
          if (filenames(mfil) /= ' ') then
             call mess(LEVEL_INFO, 'Closed file : ', filenames(mfil))
          end if
       end if
    end do
    
    if (allocated(filenames)) deallocate(filenames)

    maxnum = 0
end subroutine close_all_files

!> Proposes a filename for a certain file type.
!! If a similar file was previously loaded, that same filename is returned.
!! Otherwise, the basename of the model definition is used with an appropriate suffix.
!! Otherwise, a wildcard is returned. TODO [AvD]
!!
!! When an output directory is configured, the filename is also prefixed with that, unless switched off by prefixWithDirectory=.false..
function defaultFilename(filecat, timestamp, prefixWithDirectory, allowWildcard)
    use unstruc_model
    use m_flowtimes
    implicit none
    
    character(len=*), intent(in)  :: filecat             !< File category for which the filename is requested, e.g. 'obs', 'map', 'hyd'.
    logical, optional, intent(in) :: prefixWithDirectory !< Optional, default true. Prefix file name with the configured output directory. Set this to .false. to get purely the filename.
    logical, optional, intent(in) :: allowWildcard       !< Optional, default false. Allow the result to fall back to *.<ext>, in case no model id or other basename could be found. 
    double precision, optional, intent(in) :: timestamp  !< Optional, default disabled. Form a datetime string out of the timestamp (in seconds) and include it in the filename.

    character(len=255) :: activeFile
    character(len=255) :: basename
    character(len=255) :: shapeOutputDir
    character(1), external :: get_dirsep
    character(len=20)  :: suffix
    character(len=255) :: defaultFilename
    character(len=16)  :: dateandtime
    logical :: prefix_dir
    integer :: i

    if (present(prefixWithDirectory)) then
        prefix_dir = prefixWithDirectory
    else
        prefix_dir = .true.
    end if

    
    activeFile = ' '
    basename   = ' '
    suffix     = ' '
    
    defaultFilename = ' '
    
    select case (trim(filecat))
    case ('obs')
        activeFile = md_obsfile
        suffix     = '_obs.xyn'
    case ('bal')
        activeFile = ''
        suffix     = '_bal.tek'
    case ('histek')
        activeFile = ''
        suffix     = '_his.tek'
    case ('map')
        activeFile = md_mapfile
        suffix     = '_map.nc'
    case ('clm')
        activeFile = md_classmap_file
        suffix     = '_clm.nc'
    case ('fou')
        activeFile = ''
        suffix     = '_fou.nc'
    case ('avgwavquant')                         !! JRE
        activeFile = md_avgwavquantfile
        suffix     = '_wav.nc'    
    case ('avgsedquant')                         
        activeFile = md_avgsedquantfile
        suffix     = '_sed.nc'
    case ('tec')
        activeFile = ''
        suffix     = '.dat'
    case ('map.plt')
        activeFile = md_mapfile
        suffix     = '_map.plt'
    case ('net.plt')
        activeFile = md_mapfile
        suffix     = '_net.plt'
    case ('net')
        activeFile = md_netfile
        suffix     = '_net.nc'
    case ('ldb')
        activeFile = md_ldbfile
        suffix     = '.ldb'
    case ('rstold')
        activeFile = ''
        suffix     = '.rst'
    case ('rst')
        activeFile = ' '
        suffix     = '_rst.nc'
    case ('his')
        activeFile = md_hisfile
        suffix     = '_his.nc'
    case ('inc_s1')
        activeFile =  ''
        suffix     = '_inc_s1.nc'
    case ('bot')
        activeFile = ''
        suffix     = '.xyb'
    case ('pipe')
        activeFile = md_pipefile
        suffix     = '_pipes.pliz'
        defaultFilename = 'duikers.pliz' ! Backwards compatible if no md_pipefile/md_ident present
    case ('pipe2')
        ! input pipes.pliz --> output pipes2.pliz
        i = index(md_pipefile, '.', back=.true.)
        if (i > 0) then
           activeFile = md_pipefile(1:i-1) // '2' // md_pipefile(i:len_trim(md_pipefile))
        end if
        suffix     = '_pipes2.pliz'
        defaultFilename = 'duikers2.pliz' ! Backwards compatible if no md_pipefile/md_ident present
        
    case ('com')
        activeFile = md_comfile
        suffix     = '_com.nc'

    !---------------------------------------------------------!
    ! Shape files
    !---------------------------------------------------------!    
    case ('shpcrs')
        activeFile = ''
        suffix     = '_snapped_crs' ! .shp extension will be added automatically (and .shx/.dbf)
        
    case ('shpobs')
        activeFile = ''
        suffix     = '_snapped_obs' ! .shp extension will be added automatically (and .shx/.dbf)
        
    case ('shpweir')
        activeFile = ''
        suffix     = '_snapped_weir' ! .shp extension will be added automatically (and .shx/.dbf)
    
    case ('shpthd')
        activeFile = ''
        suffix     = '_snapped_thd' ! .shp extension will be added automatically (and .shx/.dbf)
    
    case ('shpgate')
        activeFile = ''
        suffix     = '_snapped_gate' ! .shp extension will be added automatically (and .shx/.dbf)
    
    case ('shpemb')
        activeFile = ''
        suffix     = '_snapped_emb' ! .shp extension will be added automatically (and .shx/.dbf)
    
    case ('shpfxw')
        activeFile = ''
        suffix     = '_snapped_fxw' ! .shp extension will be added automatically (and .shx/.dbf)
    
    case ('shpsrc')
        activeFile = ''
        suffix     = '_snapped_src' ! .shp extension will be added automatically (and .shx/.dbf)
        
    case ('shppump')
        activeFile = ''
        suffix     = '_snapped_pump' ! .shp extension will be added automatically (and .shx/.dbf)
        
   case ('shpdry')
        activeFile = ''
        suffix     = '_snapped_dryarea' ! .shp extension will be added automatically (and .shx/.dbf)
   case ('shpgenstruc')
        activeFile = ''
        suffix     = '_snapped_genstruc' ! .shp extension will be added automatically (and .shx/.dbf)
        
    !---------------------------------------------------------!
    ! Shape files
    !---------------------------------------------------------!

    ! Delwaq files: filecat is identical to file extension
    case ('hyd','vol','are','flo','vel','poi','len','srf','tau','vdf','tem','sal','atr','bnd','waqgeom')
        if (prefix_dir) then
           basename = getoutputdir('waq')
        end if
        basename = trim(basename)//trim(md_waqfilebase)
        if (trim(filecat) == 'waqgeom') then
           suffix = '_waqgeom.nc'
        else
           suffix = '.'//trim(filecat)
        end if

        activeFile = ''

    case ('wq_lsp')
        activeFile = ''
        suffix = '_wq_proc.lsp'
    case ('wq_bal')
        activeFile = ''
        suffix = '_wq_proc_bal.txt'
    case ('wq_timers')
        activeFile = ''
        suffix = '_wq_proc_timers.txt'
    end select

    if (present(timestamp)) then
        dateandtime = '_'
        call maketime(dateandtime(2:), timestamp)
    else
        dateandtime = ' '
    end if

    ! Now choose the most sensible filename:
    if (len_trim(activeFile) > 0) then                  ! File of this type already active, use that one.
        defaultFileName = activeFile
    elseif (len_trim(basename) > 0) then                ! Create new filename, based on a certain prefix/basename
        defaultFileName = trim(basename)//trim(dateandtime)//suffix
    elseif (len_trim(md_ident) > 0) then                ! No active, no basename, use md_ident as basename
        defaultFilename = trim(md_ident)//trim(dateandtime)//suffix
    elseif (len_trim(defaultFilename) == 0) then        ! Not even a md_ident and no hardcoded default filaname, then use unstruc_basename as basename
        defaultFilename = trim(unstruc_basename)//trim(dateandtime)//suffix
    else if(present(allowWildcard)) then                ! Final resort: just a wildcard with proper file extention.
        if (allowWildcard) then
            defaultFilename = '*'//suffix
        end if
    end if
    
    ! Output files are generally stored in a subfolder, so prefix them here with that.
    select case (trim(filecat))
    case ('his', 'map', 'clm', 'rstold', 'rst', 'bal', 'histek', 'inc_s1', 'tec', 'map.plt', 'net.plt', 'avgwavquant', 'com','avgsedquant', 'wq_lsp', 'wq_bal', 'wq_timers') !! JRE
        if (prefix_dir) then
            defaultFilename = trim(getoutputdir())//trim(defaultFilename)
        end if
    case ('shpcrs','shpobs', 'shpweir', 'shpthd', 'shpgate', 'shpemb', 'shpfxw', 'shpsrc', 'shppump', 'shpdry', 'shpgenstruc')
        if (prefix_dir) then        
            shapeOutputDir = trim(getoutputdir())//'snapped'
            call makedir(shapeOutputDir)
            defaultFilename = trim(shapeOutputDir)//get_dirsep()//trim(defaultFilename)
        end if
    end select
    
end function defaultFilename

!> Initializes file pointer to diagnostics file.
!!
!! The filename is determined by the program name and possibly a sequence
!! number. File-open attempts will not continue indefinitely (program may stop).
subroutine inidia(basename)
    use unstruc_model
  
    character(len=*)  :: basename

    integer, external :: numuni
    integer :: ierr
    integer :: k
    integer :: L
    CHARACTER(*) FILENAME*256, BASE*256
    character(*) RW*20
   
    if (mdia > 0) return
    
    mdia    = numuni()
   
    L = len_trim(md_ident)
    if (L == 0) then   
       base = trim(basename)
       L    = len_trim(basename)
    else
       base = trim(md_ident)
    end if
    
    filename = trim(base)//'.dia'
    
    K = 0
    ierr = 0
 10 OPEN(mdia, FILE=trim(filename), action='readwrite', IOSTAT=ierr)
    inquire(mdia, readwrite=rw)
    IF (ierr .GT. 0 .or. trim(rw)/='YES') THEN
        K = K + 1
        filename = basename(1:l)//'_000.dia'
        WRITE(filename(L+2:L+4),'(I3.3)') K
        GOTO 10
    ENDIF

    if (k > 0) then
        write (*,*) 'Warning: could not open default diagnostics file.'
        write (*,*) 'Now using '''//trim(filename)//''' instead.'
    end if

    call initMessaging(mdia)

end subroutine inidia



!> Constructs the full path to a file in the system directory.
subroutine sysfilepath(filename, fullpath)
    character(len=*), intent(in)  :: filename
    character(len=*), intent(out) :: fullpath

    fullpath = trim(pathdi)//trim(filename)
end subroutine sysfilepath


SUBROUTINE SYSFIL(LUNID,FILNAM)
    integer, intent(inout)   :: lunid
    CHARACTER, intent(in)    :: FILNAM*76
    integer, external        :: numuni
    character                :: FULNAM*180
    logical                  :: ja

    call sysfilepath(filnam, fulnam)
    INQUIRE(FILE= FULNAM,EXIST=JA)
    IF (JA) THEN
        LUNID = NUMUNI()
        OPEN(LUNID,FILE= FULNAM)
        call reg_file_open(lunid, fulnam)
     ENDIF

    RETURN
END SUBROUTINE SYSFIL


!> Constructs the full path to a file in the system directory.
function getfilename(ftype, success)
    character(len=255) :: getfilename
    character(len=*), intent(in) :: ftype
    logical, optional, intent(out) :: success

!    fullpath = trim(pathdi)//trim(filename)
    getfilename='todo' 
    success = .true.
end function getfilename

!> Gets the basename of a file. By default this is the filename without its extension.
!! Optionally, a file category may be specified, such that e.g., '_net.nc'
!! is stripped off (instead of .nc only)
subroutine basename(filename, filebase, filecat)
    implicit none
    character(len=*),           intent(in)  :: filename
    character(len=*),           intent(out) :: filebase
    character(len=*), optional, intent(in)  :: filecat
    
    character(1),     external              :: get_dirsep

    integer :: L1,L2

    ! Strip off file extension
    L2 = len_trim(filename)
    if (present(filecat)) then
        select case (trim(filecat))
        case ('net')
            L2 = L2 - 7 ! '_net.nc'
        case default
            L2 = index(filename, '.', .true.)-1
        end select
    else
        L2 = index(filename, '.', .true.)-1
    end if

    ! Also strip off any preceding dir names.
    L1 = index(filename, get_DIRSEP(), .true.)+1

    filebase = ' '
    filebase = filename(L1:L2)
end subroutine basename


!> Resolves an input path (typically a file path) to its
!! actual location. This routine selects whether the path
!! needs to be resolved relative to a given basedir, or
!! relative to the MDU current working dir.
!! If inpath is absolute, then that path is returned unchanged.
subroutine resolvePath(inpath, basedir, outpath)
use system_utils, only: is_abs, cat_filename
use unstruc_model, only: md_paths_relto_parent
character(len=*), intent(in   ) :: inpath  !< Input path
character(len=*), intent(in   ) :: basedir !< Basedir w.r.t. which the input path *might* be resolved, depending on PathsRelativeToParent setting.
character(len=*), intent(  out) :: outpath !< Resolved path

character(len=len_trim(inpath)+len_trim(basedir)+1) :: tmppath

if (is_abs(inpath) .or. md_paths_relto_parent == 0) then
   outpath = inpath
else
   if (md_paths_relto_parent > 0) then
      tmppath = cat_filename(basedir, inpath)
      outpath = tmppath
   end if
end if
end subroutine resolvePath

end module unstruc_files

!>
!> Find out if system is PC (directory seperator character \ (92)
!>   or UNIX (directory seperator character / (47))
function get_dirsep()
   implicit none
   
   character(len=1)     :: get_dirsep
   
   integer :: lslash
   character  hlpstr*999,slash*1
   
   CALL GET_ENVIRONMENT_VARIABLE('PATH',hlpstr)
   
   slash  = CHAR  (47)
   lslash = INDEX (hlpstr,slash)
   if (lslash .eq. 0) then
      slash  = CHAR  (92)
   endif
   get_dirsep = slash
end function get_dirsep
