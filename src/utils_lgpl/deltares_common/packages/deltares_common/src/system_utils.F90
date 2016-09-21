module system_utils
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2015.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: system_utils.F90 4612 2015-01-21 08:48:09Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/deltares_common/packages/deltares_common/src/system_utils.F90 $
!-------------------------------------------------------------------------------
!
!   Support for low level system routines
!
!-------------------------------------------------------------------------------
!

#if (defined(HAVE_CONFIG_H))
    character(3), parameter :: SHARED_LIB_EXTENSION = '.so'
    character(1), parameter :: FILESEP = '/'
#else
    character(4), parameter :: SHARED_LIB_EXTENSION = '.dll'
    character(1), parameter :: FILESEP = '\'
#endif

contains


function cat_filename(path, file, ext) result(name)
!!--description-----------------------------------------------------------------
!
!    Function: A function to concatenate a path and file name into an extended
!              file specification.
!
!!--declarations----------------------------------------------------------------
    !
    implicit none
    !
    ! Call variables
    !
    character(*)          , intent(in) :: path   ! Path name
    character(*)          , intent(in) :: file   ! File name
    character(*), optional, intent(in) :: ext    ! File name extension
    character(1024)                    :: name   ! Full name of file (path,file,ext)
    !
    ! Local variables
    !
    integer      :: lenpath ! length of path name
    character(1) :: sep     ! separator
!
!! executable statements -------------------------------------------------------
!
    ! don't create a name out of an empty name
    if (file == ' ') then
       name = ' '
       return
    endif
    !
    lenpath = len_trim(path)
    sep = ' '
    if (lenpath>0) then
       if (path(lenpath:lenpath) /= FILESEP                      &
#ifndef HAVE_CONFIG_H
          ! on Windows also check forward slash
          & .and. path(lenpath:lenpath) /= '/'                   &
#endif
          &) then
          sep = FILESEP
       endif
    endif
    name = trim(path) // trim(sep) // file
    if (present(ext)) then
       name = trim(name) // ext
    endif
end function cat_filename


subroutine split_filename(name, path, file, ext)
!!--description-----------------------------------------------------------------
!
!    Function: A subroutine to split a full file name into a path, file name
!              and file name extension.
!
!!--declarations----------------------------------------------------------------
    !
    implicit none
    !
    ! Call variables
    !
    character(*)          , intent(in)  :: name   ! Full name of file (path,file,ext)
    character(*)          , intent(out) :: path   ! Path name
    character(*)          , intent(out) :: file   ! File name (excluding extension if ext is present)
    character(*), optional, intent(out) :: ext    ! File name extension
    !
    ! Local variables
    !
    integer    :: ifilesep   ! index of last file separator
    integer    :: idot       ! index of last dot
!
!! executable statements -------------------------------------------------------
!
    ! find last file separator
    ifilesep = index(name, FILESEP, back=.true.)
#ifndef HAVE_CONFIG_H
    ! on Windows also check forward slash
    ifilesep = max(ifilesep,index(name, '/', back=.true.))
#endif
    !
    ! split name
    if (ifilesep>0) then
       path = name(1:ifilesep)
    else
       path = ' '
    endif
    if (present(ext)) then
       ! find last dot
       idot = index(name, '.', back=.true.)
       if (idot>ifilesep) then
          file = name(ifilesep+1:idot-1)
          ext  = name(idot:len_trim(name))
       else
          file = name(ifilesep+1:len_trim(name))
          ext = ' '
       endif
    else
       file = name(ifilesep+1:len_trim(name))
    endif
end subroutine split_filename



function exifil(name, unit)
!!--description-----------------------------------------------------------------
!
!    Function: A logical function which checks the existence of a
!              specified file (path may be included). Set to TRUE
!              when the file is found, FALSE otherwise.
!
!!--declarations----------------------------------------------------------------
    use string_module
    use message_module
    !
    implicit none
    !
    ! Call variables
    !
    integer  , optional  :: unit   ! File unit number for 
    logical              :: exifil
    character(*)         :: name   ! Name of file
    !
    ! Local variables
    !
    integer    :: ipos   ! Help var. 
    logical    :: ex     ! Help flag = TRUE when file is found 
!
!! executable statements -------------------------------------------------------
!
    call remove_leading_spaces(name , ipos)
    !
    inquire (file = name(:ipos), exist = ex)
    if (.not.ex) then
       if (present(unit)) then
          call write_error(FILE_NOT_FOUND // trim(name), unit = unit)
       endif
       !
       exifil = .false.
    else
       exifil = .true.
    endif
end function exifil

end module system_utils
