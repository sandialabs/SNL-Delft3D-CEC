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

module report_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use precision_part       ! single and double precision
      use timers
!
implicit none       ! force explicit typing
!
contains
      subroutine report (lunpr  ,nopart, modtyp,  &
                         floil  ,stoil , mpart ,  &
                         npart , kpart , xpart ,  &
                         ypart , zpart , vrtdsp ,wsettl)
!
!     created             : feb, 2003 by antoon koster
!
!     function            : reports particle positions and vertical
!                           displacements
!
      integer(ip),dimension(:)   :: floil  ,stoil
      integer(ip),dimension(:)   :: mpart  ,npart , kpart
      real   (sp),dimension(:)   :: wsettl
      real   (sp),dimension(:)   :: xpart  ,ypart , zpart
      real   (sp),dimension(:,:) :: vrtdsp
!
      character(len=20),dimension(0:2) :: flag = &
                        (/'[dispersed]','[floating ]','[sticky   ]'/)
!
!     local scalars
!
      integer :: i , ifl , j , modtyp , nopart ,lunpr
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "report", ithndl )
!
      write(lunpr,'(//)')
      write(lunpr,'(6x,a,a)')  &
      'Particle positions(*) - vertical displacements', &
      ' (due to vertical dispersion and settling)'
      write(lunpr,'(18x,a,a)') &
      'Dispers. coeff.  dz-disp[m]   dz-settling[m]    z-total[m]',  &
      '        layer           total     segment'
      write(lunpr,'(20x,a,58x,a)')  &
      '[m2/s]','depth[m]        depth[m]   no.[-]'
      do i=1,nopart
         write(lunpr,'(3x,2i4,6e16.4,i8)') i,kpart(i), &
                (vrtdsp(j,i),j=1,6),int(vrtdsp(7,i))
      enddo
      write(lunpr,'(6x,a)') '(*) without advection'
!
!
      write(lunpr,'(//6x,a)') &
        'Particle positions)(*) (relative) and settling velocities'
      write(lunpr,'(10x,a)')  &
      'm   n   k    x-rel.[0-1]    y-rel.[0-1]    z-rel.[0-1]'// &
      '   settling(m/s) '
!
      if (modtyp /= 4) then
         do i=1,nopart
            write(lunpr,'(3x,i4,3i4,4e15.4,5x,a)')          &
            i,mpart(i),npart(i),kpart(i),xpart(i),ypart(i), &
              zpart(i),wsettl(i)
         enddo
      else
         do i=1,nopart
            if (stoil(i)==0) then
                ifl = floil(i)
            else
                ifl = 2
            endif
            write(lunpr,'(3x,i4,3i4,4e15.4,5x,a)')          &
            i,mpart(i),npart(i),kpart(i),xpart(i),ypart(i), &
              zpart(i),wsettl(i),flag(ifl)
         enddo
      endif
      write(lunpr,'(6x,a)') '(*) final results for this time step'
!
      if ( timon ) call timstop ( ithndl )
      return
      end subroutine
end module
