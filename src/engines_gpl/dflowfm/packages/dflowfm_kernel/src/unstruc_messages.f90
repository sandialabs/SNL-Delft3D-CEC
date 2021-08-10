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

! $Id: unstruc_messages.f90 65778 2020-01-14 14:07:42Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/unstruc_messages.f90 $
!> Info and error messages within Unstruc.
!! Messages will be printed on stdout and the diagnostics file.
!! A buffer 'msgbuf' is available to write to.
module unstruc_messages
use MessageHandling

implicit none

logical, parameter, private :: printToStdout = .true.
integer :: threshold_abort = LEVEL_ERROR

! Verbosity levels for logging on screen and in diagnostics file.
! Configurable at runtime with '--verbose:...'-flag.
integer :: loglevel_StdOut = LEVEL_INFO
integer :: loglevel_file   = LEVEL_DEBUG

!> The message buffer allows you to write any number of variables in any
!! order to a character string. Call msg_flush or err_flush to output
!! the actual message or error.

contains

!> Initializes the MessageHandling module with the mdia file pointer.
subroutine initMessaging(mdia)
    integer, intent(in) :: mdia
    external :: unstruc_errorhandler, unstruc_guimessage

    call SetMessagehandling(printToStdout, .false., mdia, unstruc_errorhandler, &
         thresholdLevel_stdout = loglevel_StdOut, thresholdLevel_file = loglevel_file)

    ! Set the qnerror wrapper, for use in gridoperations, etc.
    call set_msgbox_callback(unstruc_guimessage)

end subroutine initMessaging

subroutine callback_msg(lvl,msg)
   integer, intent(in)              :: lvl
   character(len=*), intent(in)    :: msg 
   call mess(lvl,trim(msg))
end subroutine


end module unstruc_messages