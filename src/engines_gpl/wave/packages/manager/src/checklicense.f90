subroutine checklicense(success)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2020.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
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
!  $Id: checklicense.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/wave/packages/manager/src/checklicense.f90 $
!!--description-----------------------------------------------------------------
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use message_module
!
implicit none
!
! Global variables
!
logical     , intent(out) :: success
!
! Local variables
!
    integer                :: n
    character(message_len) :: txthlp       ! Help var.
    character(80)          :: txtfil
    character(256)         :: version_full  !  Version nr. of the module of the current package
!
!! executable statements -------------------------------------------------------
!
    ! get source code location
    !
    call getsourcecodelocation_WAVE(version_full)
    n = index(txthlp,'/src/utils_lgpl') ! regular checkout with src and examples level
    if (n==0) then
        n = index(txthlp,'/utils_lgpl') ! reduced checkout with src and examples level
    endif
    if (n==0) then
        txthlp = 'unknown source code location'
    else
        txthlp = txthlp(16:n-1)
    endif
    !
    txtfil        = '--------------------------------------------------------------------------------'
    version_full  = ' '
    call getfullversionstring_WAVE(version_full)
    write (*, '(a)') txtfil
    write (*, '(a)') '-  Delft3D'
    write (*, '(2a)') '-  ', trim(version_full)
    ! write (*, '(2a)')  '-  Built from : ', trim(txthlp)
    write (*, '(a)') '-  Open source'
    write (*, '(a)') '-'
    write (*, '(a)') txtfil
    success = .true.
end subroutine checklicense
