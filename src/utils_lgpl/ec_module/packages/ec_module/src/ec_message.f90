!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                
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

!  $Id: ec_message.f90 5609 2015-11-25 17:21:04Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/ec_module/packages/ec_module/src/ec_message.f90 $

!> This module contains the messaging system.
!! @author arjen.markus@deltares.nl
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.bos@deltares.nl
module m_ec_message
   use precision
   
   implicit none
   
   private
   
    public :: maxMessageLen
    public :: clearECMessage
    public :: setECMessage
    public :: getECMessage
    public :: getECMsgLenTrim
    public :: dumpECMessageStack
   
   integer,                 parameter :: maxMessageLen = 1000
   character(maxMessageLen)           :: ECMessage     = ' '
   
   interface setECMessage
      module procedure setECMessage_char
      module procedure setECMessage_int
   end interface
   
   contains
      
      ! =======================================================================
      
      !> 
      subroutine clearECMessage()
         ECMessage = ''
      end subroutine clearECMessage
            
      subroutine setECMessage_char(string, suffix)
         character(len=*), intent(in)           :: string
         character(len=*), intent(in), optional :: suffix
         !
         if (present(suffix)) then
!           ECMessage = trim(string) // ' ' // suffix
            ECMessage = "|"//trim(string)//" "// suffix //"| "// trim(ECMessage)
         else
!           ECMessage = string
            ECMessage = "|"//trim(string) //"| "// trim(ECMessage)
         endif
      end subroutine setECMessage_char
      
      ! =======================================================================
      
      !> 
      subroutine setECMessage_int(string, val)
         character(len=*), intent(in) :: string
         integer,          intent(in) :: val
         !
         integer :: nlen, nrem
         
         nlen = len_trim(string)
         nrem = min(len_trim(ECMessage), maxMessageLen - nlen-1-1-2-10)
         ! NOTE: AvD: all this string trimming here can cause enormous delays
         ! When calling setECMessage every timestep for some diagnostics, total running time
         ! for FM testmodel e02_f01_c010 went from 53s to 66s!
         ! TODO: introduce a proper message stack, not string concatenation based.
         write(ECMessage, '(a,a,i0,a)') "|"//trim(string), ' ', val,"| "//ECMessage(1:nrem)
      end subroutine setECMessage_int

      ! =======================================================================

      !> 
      function getECMessage() result(retval)
         character(len=len(ECMessage)) :: retval
         !
         retval    = trim(ECMessage)
         ECMessage = ' '
      end function getECMessage

      ! =======================================================================

      function dumpECMessageStack(msglevel,messenger) result(retval)
      implicit none 
      integer, intent(in)           :: msglevel
      interface 
         subroutine messenger(lvl,msg)
         integer, intent(in)              :: lvl
         character(len=*), intent(in)    :: msg 
         end subroutine
      end interface
      character(len=maxMessageLen) :: retval, message 
      integer                      :: i, i0, iostat 
      character(len=maxMessageLen) :: messages(15)
      message = getECMessage()
      messages = ''
      i0=1

      call messenger (msglevel,"...")! separator 
      do i=1,len_trim(message)
         if (message(i:i)=='|') then
            if (len_trim(message(i0:i-1))>0) then
               call messenger (msglevel,message(i0:i-1))
            endif
            i0=i+1
         endif
      enddo
      if (len_trim(message(i0:i-1))>0) then
         call messenger (msglevel,message(i0:i-1))
      endif
      retval = 'Fatal EC-error !!'           ! TODO: make this a meaningful return string 
      end function dumpECMessageStack
      

      !> 
      function getECMsgLenTrim () result(ECMsgLenTrim)
         integer :: ECMsgLenTrim
         !
         ECMsgLenTrim = len_trim(ECMessage)
      end function getECMsgLenTrim
end module m_ec_message
