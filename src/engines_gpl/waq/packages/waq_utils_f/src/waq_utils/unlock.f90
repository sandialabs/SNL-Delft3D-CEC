!!  Copyright (C)  Stichting Deltares, 2012-2020.
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

      subroutine unlock(lunrep,l3dmod,nolic)

!>\File
!>        Write intro to screen and report file

      ! Declaration of arguments

      use timers
      implicit none

      integer      , intent(in   ) :: lunrep   !< Unit number report file
      logical      , intent(in   ) :: l3dmod   !< Check 3D feature or not
      logical      , intent(  out) :: nolic    !< No license found

      ! local declarations

      save

      character*20  rundat
      character*120 idstr
      logical       first
      integer (4)   i, j
      save          first
      character*75  opkom(8)

      data     first /.true./
      data     opkom  / &
      '+-----------------------------------------------------------------------+', &
      '|                      D e l f t 3 D - D E L W A Q                      |', &
      '|                                                                       |', &
      '| D-Water Quality         Water quality simulation in 1D/2D/3D models   |', &
      '| D-Ecology               Algae simulation in 1D/2D/3D models           |', &
      '|                                                                       |', &
      '| Version xx.xxxx  xx-xx-xxxx                                           |', &
      '+-----------------------------------------------------------------------+'/
     
      integer(4) ithndl /0/
      if ( timon ) call timstrt( "unlock", ithndl )

      nolic =.false.
      if ( first ) then
         first = .false.
         ! set idstr
         call getidentification(idstr)
         do i = 1 , size(opkom)
            if ( opkom(i)(3:15) .eq. 'Version xx.xx' ) then
               write(opkom(i)(3:72),'(a)') idstr(1:70)
            end if
            write(*,*) opkom(i)
         enddo
      endif
      write (lunrep,'(1x,a)') trim(idstr)
      call dattim(rundat)
      write (lunrep,'(2a)') ' Execution start: ',rundat

      if ( timon ) call timstop( ithndl )
      return
      end
