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

      subroutine getdps ( lunpr  , lundp  , lnam   , nmax   , mmax   ,      &
     &                    nosegl , dps    , cellpnt, ltrack )
!
!     programmer : antoon koster
!     function   : read bathymetry from *.dps file,
!                  as created by coup203
!                  note that the bathymetry is defined
!                  w.r.t. reference level
!     date       : feb 2003
!
!     note:
!       1. note that the depths in this routine refer to
!          depths w.r.t. reference level, so called
!          bathymetry.
!
!       2. as these depths are only required in case of
!          particle tracking, this file may be missing
!          in other cases (see itrack)

      use precision_part      ! single and double precision
      use timers
      use fileinfo       ! file information for all input/output files
      use openfl_mod     ! explicit interface for subroutine calls
      implicit none

!     Arguments

!     kind           function         name                  description

      integer  ( ip), intent(in   ) :: lunpr               !< unit nr of the diagnostics file
      integer  ( ip), intent(in   ) :: lundp               !< unit nr of the depth file
      character( * ), intent(in   ) :: lnam                !< name of the depth file
      integer  ( ip), intent(in   ) :: nmax                !< first dimension of the grid
      integer  ( ip), intent(in   ) :: mmax                !< second dimension of the grid
      integer  ( ip), intent(in   ) :: nosegl              !< nr of active segments of a layer
      real     ( rp), intent(  out) :: dps   (nmax*mmax)   !< array with depth values
      integer  ( ip), intent(in   ) :: cellpnt(nosegl)     !< backpointer of nosegl to mnmax
      logical       , intent(in   ) :: ltrack              !< if .true. then particle tracing

!     local scalars

      integer :: idum, nmnw, iocond
      real(sp), allocatable :: depwrk(:)          !  work array to read depth
      logical :: ex

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "getdps", ithndl )

      dps = 0.0
      if ( ltrack ) then

!        for particle tracking, get depth
         
         inquire (file = trim(lnam), exist = ex)
         if (ex) then
            call openfl ( lundp, lnam, ftype(2), 0 )
            read (lundp) idum,idum,nmnw,nmnw,nmnw,idum
            if ( nmnw .ne. nosegl ) then
               write ( lunpr, * ) 'ERROR, dimension in dps file does not match!',nmnw,nosegl
               call stop_exit(1)
            endif
            allocate ( depwrk(nosegl) )
            read (lundp, iostat = iocond) depwrk
            close(lundp)
            if (iocond  .ne.  0 ) goto 100
            dps(cellpnt(:)) = depwrk(:)
         else
            write ( lunpr, * ) 'WARNING: Depths file does not exist. Will continue assuming'
            write ( lunpr, * ) '         uniform bathymetry of 0.0 for particle tracks.'
            write ( lunpr, * ) '         z-values in the particle tracks file are not absolute!'
         endif
      endif

      if ( timon ) call timstop ( ithndl )
      return

  100 write (lunpr, *) 'Error 4407. Reading the depth file :', lnam(:len_trim(lnam))
      write (lunpr, *) '            (file maybe missing ??)      '
      write (*,  *)   'Error 4407. Reading the depth file :', lnam(:len_trim(lnam))
      write (lunpr, * ) 'WARNING: Depths not read correctly, will continue assuming'
      write (lunpr, * ) '         uniform bathymetry of 0.0 for particle tracks.'
      write (lunpr, * ) '         z-values in the particle tracks file are not absolute!'
      write (*, * ) 'WARNING: Depths not read correctly, will continue assuming'
      write (*, * ) '         uniform bathymetry of 0.0 for particle tracks.'
      write (*, * ) '         z-values in the particle tracks file are not absolute!'
      return
      end subroutine
