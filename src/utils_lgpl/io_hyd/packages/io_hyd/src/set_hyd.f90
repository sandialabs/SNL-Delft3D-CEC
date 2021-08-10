      subroutine set_hyd(hyd,name)
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
!  $Id: set_hyd.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_lgpl/io_hyd/packages/io_hyd/src/set_hyd.f90 $
   
      ! function : set the filenames of the hyd file

      ! global declarations

      use hydmod
      implicit none

      ! declaration of the arguments

      type(t_hyd)         :: hyd     ! description of the hydrodynamics
      character(len=*)    :: name    ! base name hyd files

      ! local declarations

      integer             :: platform
      integer             :: ft_dat
      integer             :: lunrep                 ! unit number report file
      integer             :: idummy                 ! idummy
      real                :: rdummy                 ! rdummy
      character           :: cdummy                 ! cdummy
      integer             :: ierr2                  ! ierr2
      logical             :: lfound                 ! indication if command line argument was found

      ! determine the filetype for the binary data files

      platform = dlwq_platform()
      if ( platform .eq. fs_dos ) then
         ft_dat = ft_bin
      elseif ( platform .eq. fs_unx ) then
         ft_dat = ft_unf
      elseif ( platform .eq. fs_asc ) then
         ft_dat = ft_asc
      else
         ft_dat = 0
      endif

      if ( hyd%l_ascii ) then
         ft_dat = ft_asc
      endif

      ! set the names and types

      hyd%file_hyd%name = trim(name)//'.hyd'
      hyd%file_dwq%name = ' '
      if (hyd%geometry .eq. HYD_GEOM_CURVI) then
         hyd%file_lga%name = trim(name)//'.lga'
         hyd%file_cco%name = trim(name)//'.cco'
      else if (hyd%geometry .eq. HYD_GEOM_UNSTRUC) then
         hyd%file_bnd%name = trim(name)//'.bnd'
         hyd%file_geo%name = trim(name)//'_waqgeom.nc'
         hyd%file_lga%name = trim(name)//'.bnd'
         hyd%file_cco%name = trim(name)//'_waqgeom.nc'
      end if                  
      hyd%file_vol%name = trim(name)//'.vol'
      hyd%file_are%name = trim(name)//'.are'
      hyd%file_flo%name = trim(name)//'.flo'
      hyd%file_poi%name = trim(name)//'.poi'
      hyd%file_len%name = trim(name)//'.len'
      hyd%file_sal%name = trim(name)//'.sal'
      hyd%file_tem%name = trim(name)//'.tem'
      hyd%file_vdf%name = trim(name)//'.vdf'
      if (hyd%geometry .eq. HYD_GEOM_CURVI) then
      hyd%file_srf%name = trim(name)//'.srf'
      else if (hyd%geometry .eq. HYD_GEOM_UNSTRUC) then
         hyd%file_srf%name = trim(name)//'.srfold'
         hyd%file_hsrf%name = trim(name)//'.srf'
      endif
      hyd%file_lgt%name = trim(name)//'.lgt'
      hyd%file_src%name = trim(name)//'.src'
      hyd%file_chz%name = trim(name)//'.chz'
      hyd%file_tau%name = trim(name)//'.tau'
      hyd%file_wlk%name = trim(name)//'.wlk'
      hyd%file_atr%name = trim(name)//'.atr'
      hyd%file_dps%name = trim(name)//'.dps'
      hyd%file_hyd%type = ft_asc
      hyd%file_com%type = ft_nef
      hyd%file_dwq%type = ft_asc
      hyd%file_vag%type = ft_asc
      hyd%file_lga%type = ft_dat
      hyd%file_cco%type = ft_dat
      hyd%file_bnd%type = ft_asc
      hyd%file_geo%type = ft_dat
      hyd%file_vol%type = ft_dat
      hyd%file_are%type = ft_dat
      hyd%file_flo%type = ft_dat
      hyd%file_poi%type = ft_dat
      hyd%file_len%type = ft_dat
      hyd%file_sal%type = ft_dat
      hyd%file_tem%type = ft_dat
      hyd%file_vdf%type = ft_dat
      hyd%file_srf%type = ft_dat
      hyd%file_hsrf%type = ft_dat
      hyd%file_lgt%type = ft_dat
      hyd%file_src%type = ft_asc
      hyd%file_chz%type = ft_dat
      hyd%file_tau%type = ft_dat
      hyd%file_wlk%type = ft_asc
      hyd%file_atr%type = ft_asc
      hyd%file_dps%type = ft_dat
      hyd%file_hyd%status = 0
      hyd%file_com%status = 0
      hyd%file_dwq%status = 0
      hyd%file_vag%status = 0
      hyd%file_lga%status = 0
      hyd%file_cco%status = 0
      hyd%file_bnd%status = 0
      hyd%file_geo%status = 0
      hyd%file_vol%status = 0
      hyd%file_are%status = 0
      hyd%file_flo%status = 0
      hyd%file_poi%status = 0
      hyd%file_len%status = 0
      hyd%file_sal%status = 0
      hyd%file_tem%status = 0
      hyd%file_vdf%status = 0
      hyd%file_srf%status = 0
      hyd%file_hsrf%status = 0
      hyd%file_lgt%status = 0
      hyd%file_src%status = 0
      hyd%file_chz%status = 0
      hyd%file_tau%status = 0
      hyd%file_wlk%status = 0
      hyd%file_atr%status = 0
      hyd%file_dps%status = 0

      return
      end