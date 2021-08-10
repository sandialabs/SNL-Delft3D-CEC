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
!  $Id: read_srf.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_lgpl/io_hyd/packages/io_hyd/src/read_srf.f90 $

      subroutine read_srf(file_srf, mmax  , nmax  , nosegl, surf )

      ! function : read a srf file and check dimensions

      ! global declarations

      use filmod                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_dlwqfile)                       :: file_srf               ! aggregation-file
      integer                                :: mmax                   ! grid cells m direction
      integer                                :: nmax                   ! grid cells n direction
      integer                                :: nosegl                 ! nosegl
      real                                   :: surf(nosegl)           ! property of the cells per layer

      ! local declarations

      integer                                :: mmaxd                  ! grid cells m direction from srf file
      integer                                :: nmaxd                  ! grid cells n direction from srf file
      integer                                :: i3,i4,i5,i6            ! nosegl from srf file
      integer                                :: ioerr                  ! error on file
      integer                                :: i                      ! loop counter
      integer                                :: lunrep                 ! unit number report file

      call getmlu(lunrep)

      call dlwqfile_open(file_srf)
      read(file_srf%unit_nr,iostat=ioerr) nmaxd, mmaxd, i3, i4, i5, i6
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading srf file'
         call srstop(1)
      endif

      if ( mmax*nmax .ne. mmaxd*nmaxd ) then
         write(lunrep,*) ' dimensions srf file differ from input hydrodynamics'
         call srstop(1)
      endif

      read(file_srf%unit_nr,iostat=ioerr) (surf(i),i=1,nosegl)
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading srf file'
         call srstop(1)
      endif

      close(file_srf%unit_nr)
      file_srf%status = FILE_STAT_UNOPENED

      return
      end

      subroutine read_hsrf(file_hsrf, noseg, surf )

      ! function : read a horizontal srf file

      ! global declarations

      use filmod                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_dlwqfile)                       :: file_hsrf              ! aggregation-file
      integer                                :: noseg                  ! number of segments
      real                                   :: surf(noseg)            ! horizontal surfaces

      ! local declarations

      integer                                :: i                      ! loop counter
      integer                                :: lunrep                 ! unit number report file
      integer                                :: idum                   ! dummy time label
      integer                                :: ioerr                  ! error on file

      call getmlu(lunrep)
      call dlwqfile_open(file_hsrf)
      read(file_hsrf%unit_nr,iostat=ioerr) idum, (surf(i),i=1,noseg)
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading horizontal srf file'
         call srstop(1)
      endif

      close(file_hsrf%unit_nr)
      file_hsrf%status = FILE_STAT_UNOPENED

      return
      end
