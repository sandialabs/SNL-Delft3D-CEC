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

! $Id: xbeach_netcdf.f90 65778 2020-01-14 14:07:42Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/xbeach_netcdf.f90 $
module m_xbeach_netcdf
!! xbeach time-averaged spatial output
!! to do for the future: add flexibility to add variables using mnemonics
   use io_ugrid
   use netcdf
   use unstruc_netcdf
   implicit none
   
   type t_unc_wavids
      
      integer                  :: ncid = 0 !< NetCDF data set id (typically NetCDF file pointer)
      type(t_unc_timespace_id) :: id_tsp
      
      integer                  :: id_time               = -1  
      integer                  :: id_timestep           = -1  
      integer                  :: id_H_mean(4)          = -1
      integer                  :: id_H_var(4)           = -1
      integer                  :: id_H_min(4)           = -1 
      integer                  :: id_H_max(4)           = -1 
      integer                  :: id_E_mean(4)          = -1 
      integer                  :: id_E_var(4)           = -1 
      integer                  :: id_E_min(4)           = -1
      integer                  :: id_E_max(4)           = -1 
      integer                  :: id_R_mean(4)          = -1
      integer                  :: id_R_var(4)           = -1 
      integer                  :: id_R_min(4)           = -1 
      integer                  :: id_R_max(4)           = -1 
      integer                  :: id_D_mean(4)          = -1 
      integer                  :: id_D_var(4)           = -1 
      integer                  :: id_D_min(4)           = -1
      integer                  :: id_D_max(4)           = -1 
      integer                  :: id_Fx_mean(4)         = -1
      integer                  :: id_Fx_var(4)          = -1 
      integer                  :: id_Fx_min(4)          = -1 
      integer                  :: id_Fx_max(4)          = -1 
      integer                  :: id_Fy_mean(4)         = -1
      integer                  :: id_Fy_var(4)          = -1 
      integer                  :: id_Fy_min(4)          = -1 
      integer                  :: id_Fy_max(4)          = -1                     
      integer                  :: id_DR_mean(4)         = -1
      integer                  :: id_DR_var(4)          = -1 
      integer                  :: id_DR_min(4)          = -1 
      integer                  :: id_DR_max(4)          = -1 
      integer                  :: id_s1_mean(4)         = -1 
      integer                  :: id_s1_var(4)          = -1
      integer                  :: id_s1_min(4)          = -1
      integer                  :: id_s1_max(4)          = -1 
      integer                  :: id_u_mean(4)          = -1 
      integer                  :: id_u_var(4)           = -1 
      integer                  :: id_u_min(4)           = -1 
      integer                  :: id_u_max(4)           = -1 
      integer                  :: id_v_mean(4)          = -1 
      integer                  :: id_v_var(4)           = -1 
      integer                  :: id_v_min(4)           = -1 
      integer                  :: id_v_max(4)           = -1 
      integer                  :: id_cwav_mean(4)       = -1 
      integer                  :: id_cwav_var(4)        = -1
      integer                  :: id_cwav_min(4)        = -1
      integer                  :: id_cwav_max(4)        = -1 
      integer                  :: id_cgwav_mean(4)      = -1
      integer                  :: id_cgwav_var(4)       = -1 
      integer                  :: id_cgwav_min(4)       = -1 
      integer                  :: id_cgwav_max(4)       = -1 
      integer                  :: id_urms_mean(4)       = -1
      integer                  :: id_urms_var(4)        = -1 
      integer                  :: id_urms_min(4)        = -1 
      integer                  :: id_urms_max(4)        = -1 
      integer                  :: id_ustx_mean(4)       = -1 
      integer                  :: id_ustx_var(4)        = -1
      integer                  :: id_ustx_min(4)        = -1 
      integer                  :: id_ustx_max(4)        = -1 
      integer                  :: id_usty_mean(4)       = -1
      integer                  :: id_usty_var(4)        = -1 
      integer                  :: id_usty_min(4)        = -1
      integer                  :: id_usty_max(4)        = -1 
      integer                  :: id_thetamean_mean(4)  = -1
      integer                  :: id_thetamean_var(4)   = -1 
      integer                  :: id_thetamean_min(4)   = -1 
      integer                  :: id_thetamean_max(4)   = -1 
      integer                  :: id_sigmwav_mean(4)    = -1 
      integer                  :: id_sigmwav_var(4)     = -1
      integer                  :: id_sigmwav_min(4)     = -1 
      integer                  :: id_sigmwav_max(4)     = -1
      integer                  :: id_ucx_mean(4)        = -1
      integer                  :: id_ucx_var(4)         = -1 
      integer                  :: id_ucx_min(4)         = -1 
      integer                  :: id_ucx_max(4)         = -1
      integer                  :: id_ucy_mean(4)        = -1
      integer                  :: id_ucy_var(4)         = -1 
      integer                  :: id_ucy_min(4)         = -1 
      integer                  :: id_ucy_max(4)         = -1

   end type t_unc_wavids

contains

subroutine xbeach_write_stats(tim)
   use m_flowparameters, only: jawave, jaavgwavquant, eps10
   use m_flowtimes, only: ti_wav, ti_wavs, ti_wave, tstop_user, time_wav   
   use precision_basics
   
   implicit none
   
   double precision, intent(in)      :: tim
   integer                           :: ierr
   
   ierr = 1
   if ((jawave.eq.4) .and. (ti_wav > 0) .and. (jaavgwavquant .eq. 1)) then
      if (comparereal(tim, time_wav, eps10) >= 0) then
         call unc_write_wav(tim)
         call xbeach_clearaverages()
         if (ti_wav > 0) then
             time_wav = max(ti_wavs + (floor((tim-ti_wavs)/ti_wav)+1)*ti_wav,ti_wavs)
         else
             time_wav = tstop_user
         endif
         if (comparereal(time_wav, ti_wave, eps10) == 1) then
             time_wav = tstop_user
         endif
      endif
   end if
   
   ierr = 0
   
1234 continue
   return
end subroutine

subroutine unc_write_wav(tim)
    use m_flow
    use m_flowtimes
    use unstruc_netcdf
    use unstruc_model
    use unstruc_files , only: defaultFilename
    implicit none

    double precision, intent(in) :: tim

    type(t_unc_wavids), save :: wavids
    integer                  :: ierr
    character(len=256)       :: filnam

    if ( md_mapformat.eq.IFORMAT_NETCDF .or. md_mapformat.eq.IFORMAT_NETCDF_AND_TECPLOT .or. md_mapformat == IFORMAT_UGRID) then   !   NetCDF output
       if (wavids%ncid /= 0 .and. ((md_unc_conv == UNC_CONV_UGRID .and. wavids%id_tsp%idx_curtime == 0) .or. (md_unc_conv == UNC_CONV_CFOLD .and. it_wav == 0))) then
          ierr = unc_close(wavids%ncid)
          wavids%ncid = 0
       end if


       if (wavids%ncid == 0) then
          filnam = defaultFilename('avgwavquant')
            ierr = unc_create(filnam , 0, wavids%ncid)
            if (ierr /= nf90_noerr) then
                call mess(LEVEL_WARN, 'Could not create wave averaged quantity file.')
                wavids%ncid = 0
            end if
       endif

       if (wavids%ncid .ne. 0) then
          if (md_unc_conv == UNC_CONV_UGRID) then
             call unc_write_wav_filepointer_ugrid(wavids,tim)  
          else
             call unc_write_wav_filepointer(wavids%ncid,tim)  
          endif
       endif

       ierr = nf90_sync(wavids%ncid) ! Flush file
    end if

end subroutine unc_write_wav

subroutine unc_write_wav_filepointer_ugrid(wavids, tim)
   use io_ugrid
   use unstruc_netcdf
   use m_xbeach_avgoutput
   use m_flowgeom
   use m_flowtimes, only: refdat
   use m_sferic, only: pi
   
   implicit none
   
   type(t_unc_wavids), intent(inout)           :: wavids
   double precision, intent(in)                :: tim
                                               
   integer                                     :: k
   integer                                     :: ndim
   integer                                     :: itim
   integer                                     :: ierr
   character(len=125)                          :: tmpstr
   
   double precision, allocatable, dimension(:) :: temp
   
   if (jaavgwriteall>0 .or. jaavgwriteH>0 .or. jaavgwriteUrms>0) then
      allocate(temp(ndx), stat=ierr)
      temp = 0d0
   end if
                                              
   ! Use nr of dimensions in netCDF file a quick check whether vardefs were written
   ! before in previous calls.
   ndim = 0
   ierr = nf90_inquire(wavids%ncid, nDimensions=ndim)

   ! Only write net and flow geometry data the first time, or for a separate map file.
   if (ndim == 0) then

      ierr = ug_addglobalatts(wavids%ncid, ug_meta_fm)
      call unc_write_flowgeom_filepointer_ugrid(wavids%ncid, wavids%id_tsp)

      ! Current time t1
      ierr = nf90_def_dim(wavids%ncid, 'time', nf90_unlimited, wavids%id_tsp%id_timedim)
      call check_error(ierr, 'def time dim')
      tmpstr = 'seconds since '//refdat(1:4)//'-'//refdat(5:6)//'-'//refdat(7:8)//' 00:00:00'
      ierr = unc_def_var_nonspatial(wavids%ncid, wavids%id_time, nf90_double, (/ wavids%id_tsp%id_timedim /), 'time', 'time', '', trim(tmpstr))
      
      if (jaavgwriteall>0 .or. jaavgwriteH>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_mean, nf90_double, UNC_LOC_S, 'H_mean','mean rms wave height', 'mean rms wave height', 'm')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_var, nf90_double, UNC_LOC_S, 'H_var','variance rms wave height', 'variance rms wave height', 'm2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_max, nf90_double, UNC_LOC_S, 'H_max','max rms wave height', 'max rms wave height', 'm')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_min, nf90_double, UNC_LOC_S, 'H_min','min rms wave height', 'min rms wave height', 'm')
      end if
      
      if (jaavgwriteall>0 .or. jaavgwriteE>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_mean, nf90_double, UNC_LOC_S, 'E_mean','mean wave energy', 'mean wave energy', 'J m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_var, nf90_double, UNC_LOC_S,  'E_var', 'variance wave energy', 'variance wave energy', 'J2 m-4')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_max, nf90_double, UNC_LOC_S,  'E_max', 'max wave energy', 'max wave energy', 'J m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_min, nf90_double, UNC_LOC_S,  'E_min', 'min wave energy', 'min wave energy', 'J m-2')
      end if
      
      if (jaavgwriteall>0 .or. jaavgwriteR>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_mean, nf90_double, UNC_LOC_S, 'R_mean','mean roller energy', 'mean roller energy', 'J m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_var, nf90_double, UNC_LOC_S,  'R_var','variance roller energy', 'variance roller energy', 'J2 m-4')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_max, nf90_double, UNC_LOC_S,  'R_max','max roller energy', 'max roller energy', 'J m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_min, nf90_double, UNC_LOC_S,  'R_min','min roller energy', 'min roller energy', 'J m-2')
      end if
      
      if (jaavgwriteall>0 .or. jaavgwriteD>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_mean, nf90_double, UNC_LOC_S, 'D_mean','mean wave breaking dissipation', 'mean wave breaking dissipation', 'W m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_var, nf90_double, UNC_LOC_S,  'D_var','variance wave breaking dissipation', 'variance wave breaking dissipation', 'W2 m-4')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_max, nf90_double, UNC_LOC_S,  'D_max','max wave breaking dissipation', 'max wave breaking dissipation', 'W m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_min, nf90_double, UNC_LOC_S,  'D_min','min wave breaking dissipation', 'min wave breaking dissipation', 'W m-2')
         
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_mean, nf90_double, UNC_LOC_S, 'DR_mean','mean roller breaking dissipation', 'mean roller breaking dissipation', 'W m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_var, nf90_double, UNC_LOC_S,  'DR_var','variance roller breaking dissipation', 'variance roller breaking dissipation', 'W2 m-4')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_max, nf90_double, UNC_LOC_S,  'DR_max','max roller breaking dissipation', 'max roller breaking dissipation', 'W m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_min, nf90_double, UNC_LOC_S,  'DR_min','min roller breaking dissipation', 'min roller breaking dissipation', 'W m-2')
      end if
      
      if (jaavgwriteall>0 .or. jaavgwriteCel>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_mean, nf90_double, UNC_LOC_S, 'cwav_mean','mean wave celerity', 'mean wave celerity', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_var, nf90_double, UNC_LOC_S,  'cwav_var','variance wave celerity', 'variance wave celerity', 'm2 s-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_max, nf90_double, UNC_LOC_S,  'cwav_max','max wave celerity', 'max wave celerity', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_min, nf90_double, UNC_LOC_S,  'cwav_min','min wave celerity', 'min wave celerity', 'm s-1')
         
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_mean, nf90_double, UNC_LOC_S, 'cgwav_mean','mean wave group celerity', 'mean wave group celerity', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_var, nf90_double, UNC_LOC_S,  'cgwav_var','variance wave group celerity', 'variance wave group celerity', 'm2 s-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_max, nf90_double, UNC_LOC_S,  'cgwav_max','max wave group celerity', 'max wave group celerity', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_min, nf90_double, UNC_LOC_S,  'cgwav_min','min wave group celerity', 'min wave group celerity', 'm s-1')
      end if
      
      if (jaavgwriteall>0 .or. jaavgwriteS>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_mean, nf90_double, UNC_LOC_S, 's1_mean','mean water level', 'mean water level', 'm')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_var, nf90_double, UNC_LOC_S,  's1_var','variance water level', 'variance water level', 'm2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_max, nf90_double, UNC_LOC_S,  's1_max','max water level', 'max water level', 'm')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_min, nf90_double, UNC_LOC_S,  's1_min','min water level', 'min water level', 'm')   
      endif
      
      if (jaavgwriteall>0 .or. jaavgwriteU>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_mean, nf90_double, UNC_LOC_S, 'ustx_mean','mean stokes drift, x-component', 'mean stokes drift, x-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_var, nf90_double, UNC_LOC_S,  'ustx_var','variance stokes drift, x-component', 'variance stokes drift, x-component', 'm2 s-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_max, nf90_double, UNC_LOC_S,  'ustx_max','max stokes drift, x-component', 'max stokes drift, x-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_min, nf90_double, UNC_LOC_S,  'ustx_min','min stokes drift, x-component', 'min stokes drift, x-component', 'm s-1') 
         
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_mean, nf90_double, UNC_LOC_S, 'usty_mean','mean stokes drift, y-component', 'mean stokes drift, y-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_var, nf90_double, UNC_LOC_S,  'usty_var','variance stokes drift, y-component', 'variance stokes drift, y-component', 'm2 s-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_max, nf90_double, UNC_LOC_S,  'usty_max','max stokes drift, y-component', 'max stokes drift, y-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_min, nf90_double, UNC_LOC_S,  'usty_min','min stokes drift, y-component', 'min stokes drift, y-component', 'm s-1')
         
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_mean, nf90_double, UNC_LOC_S, 'ucx_mean','mean velocity, x-component', 'mean velocity, x-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_var, nf90_double, UNC_LOC_S,  'ucx_var','variance velocity, x-component', 'variance stokes drift, x-component', 'm2 s-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_max, nf90_double, UNC_LOC_S,  'ucx_max','max velocity, x-component', 'max velocity, x-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_min, nf90_double, UNC_LOC_S,  'ucx_min','min velocity, x-component', 'min velocity, x-component', 'm s-1')
         
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_mean, nf90_double, UNC_LOC_S, 'ucy_mean','mean velocity, y-component', 'mean velocity, y-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_var, nf90_double, UNC_LOC_S,  'ucy_var','variance velocity, y-component', 'variance stokes drift, y-component', 'm2 s-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_max, nf90_double, UNC_LOC_S,  'ucy_max','max velocity, y-component', 'max velocity, y-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_min, nf90_double, UNC_LOC_S,  'ucy_min','min velocity, y-component', 'min velocity, y-component', 'm s-1')
      endif
      
      if (jaavgwriteall>0 .or. jaavgwriteF>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_mean, nf90_double, UNC_LOC_S, 'Fx_mean','mean wave force, x-component', 'mean wave force, x-component', 'N m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_var, nf90_double, UNC_LOC_S,  'Fx_var','variance wave force, x-component', 'variance wave force, x-component', 'N2 m-4')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_max, nf90_double, UNC_LOC_S,  'Fx_max','max wave force, x-component', 'max wave force, x-component', 'N m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_min, nf90_double, UNC_LOC_S,  'Fx_min','min wave force, x-component', 'min wave force, x-component', 'N m-2')
                                                                                                        
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_mean, nf90_double, UNC_LOC_S, 'Fy_mean','mean wave force, y-component', 'mean wave force, y-component', 'N m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_var, nf90_double, UNC_LOC_S,  'Fy_var','variance wave force, y-component', 'variance wave force, y-component', 'N2 m-4')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_max, nf90_double, UNC_LOC_S,  'Fy_max','max wave force, y-component', 'max wave force, y-component', 'N m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_min, nf90_double, UNC_LOC_S,  'Fy_min','min wave force, y-component', 'min wave force, y-component', 'N m-2')
      endif
      
      if (jaavgwriteall>0 .or. jaavgwriteUrms>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_mean, nf90_double, UNC_LOC_S, 'urms_mean','mean rms orbital velocity', 'mean rms orbital velocity', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_var, nf90_double, UNC_LOC_S,  'urms_var','variance rms orbital velocity', 'variance rms orbital velocity', 'm2 s-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_max, nf90_double, UNC_LOC_S,  'urms_max','max rms orbital velocity', 'max rms orbital velocity', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_min, nf90_double, UNC_LOC_S,  'urms_min','min rms orbital velocity', 'min rms orbital velocity', 'm s-1')     
      endif
      
      if (jaavgwriteall>0 .or. jaavgwriteDir>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_mean, nf90_double, UNC_LOC_S, 'thetamean_mean','mean of mean wave angle', 'mean of mean wave angle', 'deg from N')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_var, nf90_double, UNC_LOC_S,  'thetamean_var','variance of mean wave angle', 'variance of mean wave angle', 'deg from N')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_max, nf90_double, UNC_LOC_S,  'thetamean_max','max of mean wave angle', 'max of mean wave angle', 'deg from N')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_min, nf90_double, UNC_LOC_S,  'thetamean_min','min of mean wave angle', 'min of mean wave angle', 'deg from N')      
      endif
      
      ierr = nf90_enddef(wavids%ncid)      
   end if
   
   wavids%id_tsp%idx_curtime = wavids%id_tsp%idx_curtime+1   
   itim                      = wavids%id_tsp%idx_curtime
   ierr                      = nf90_put_var(wavids%ncid, wavids%id_time, tim, (/ itim /))
   
   if (jaavgwriteall>0 .or. jaavgwriteH>0) then
      temp = 0d0
      do k = 1, ndx     ! stack overflow
         temp(k) = sqrt(H_varsquare(k))    
      end do
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_mean, UNC_LOC_S, temp)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_var,  UNC_LOC_S, H_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_max,  UNC_LOC_S, H_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_min,  UNC_LOC_S, H_min)
   end if
   
   if (jaavgwriteall>0 .or. jaavgwriteE>0) then
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_mean, UNC_LOC_S, E_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_var,  UNC_LOC_S, E_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_max,  UNC_LOC_S, E_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_min,  UNC_LOC_S, E_min)
   end if
   
   if (jaavgwriteall>0 .or. jaavgwriteR>0) then
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_mean, UNC_LOC_S, R_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_var,  UNC_LOC_S, R_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_max,  UNC_LOC_S, R_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_min,  UNC_LOC_S, R_min)
   end if
   
   if (jaavgwriteall>0 .or. jaavgwriteD>0) then
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_mean, UNC_LOC_S, D_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_var,  UNC_LOC_S, D_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_max,  UNC_LOC_S, D_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_min,  UNC_LOC_S, D_min)
      
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_mean, UNC_LOC_S, DR_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_var,  UNC_LOC_S, DR_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_max,  UNC_LOC_S, DR_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_min,  UNC_LOC_S, DR_min)
   end if
   
   if (jaavgwriteall>0 .or. jaavgwriteCel>0) then
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_mean, UNC_LOC_S, cwav_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_var,  UNC_LOC_S, cwav_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_max,  UNC_LOC_S, cwav_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_min,  UNC_LOC_S, cwav_min)
      
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_mean, UNC_LOC_S, cgwav_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_var,  UNC_LOC_S, cgwav_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_max,  UNC_LOC_S, cgwav_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_min,  UNC_LOC_S, cgwav_min)
   end if
   
   if (jaavgwriteall>0 .or. jaavgwriteS>0) then
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_mean, UNC_LOC_S, s1_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_var,  UNC_LOC_S, s1_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_max,  UNC_LOC_S, s1_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_min,  UNC_LOC_S, s1_min)
   endif
   
   if (jaavgwriteall>0 .or. jaavgwriteU>0) then
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_mean, UNC_LOC_S, ust_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_var,  UNC_LOC_S, ust_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_max,  UNC_LOC_S, ust_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_min,  UNC_LOC_S, ust_min)
      
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_mean, UNC_LOC_S, vst_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_var,  UNC_LOC_S, vst_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_max,  UNC_LOC_S, vst_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_min,  UNC_LOC_S, vst_min)
      
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_mean, UNC_LOC_S, u_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_var,  UNC_LOC_S, u_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_max,  UNC_LOC_S, u_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_min,  UNC_LOC_S, u_min)
      
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_mean, UNC_LOC_S, v_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_var,  UNC_LOC_S, v_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_max,  UNC_LOC_S, v_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_min,  UNC_LOC_S, v_min)
   endif
   
   if (jaavgwriteall>0 .or. jaavgwriteF>0) then
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_mean, UNC_LOC_S, Fx_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_var,  UNC_LOC_S, Fx_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_max,  UNC_LOC_S, Fx_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_min,  UNC_LOC_S, Fx_min)
      
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fy_mean, UNC_LOC_S, Fy_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fy_var,  UNC_LOC_S, Fy_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fy_max,  UNC_LOC_S, Fy_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fy_min,  UNC_LOC_S, Fy_min)
   
   endif
   
   if (jaavgwriteall>0 .or. jaavgwriteUrms>0) then
      temp = 0d0
      do k=1, ndx
         temp(k) = sqrt(urms_varsquare(k))
      end do
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_mean, UNC_LOC_S, temp)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_var,  UNC_LOC_S, urms_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_max,  UNC_LOC_S, urms_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_min,  UNC_LOC_S, urms_min)
   endif
   
   if (jaavgwriteall>0 .or. jaavgwriteDir>0) then 
      temp = 0d0
      do k = 1, ndx
         temp(k) = 270.d0 - mod(2.d0*pi + atan2(nint(thetamean_mean(k))/1d7, mod(thetamean_mean(k),1.d0)*1d1), 2.d0*pi) / pi * 180d0
      end do
      
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_mean, UNC_LOC_S, temp)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_var,  UNC_LOC_S, thetamean_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_max,  UNC_LOC_S, thetamean_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_min,  UNC_LOC_S, thetamean_min)
   endif
   

end subroutine unc_write_wav_filepointer_ugrid


!> Writes time-averaged spatial wave output to an already opened netCDF dataset.
subroutine unc_write_wav_filepointer(imapfile, tim,  jaseparate)
    use m_flow
    use m_flowtimes
    use m_flowgeom
    use m_sferic
    use network_data
    use unstruc_netcdf
	 use m_xbeach_avgoutput

    implicit none

    integer,           intent(in) :: imapfile
    real(kind=hp),     intent(in) :: tim

    integer                       :: idims(2)
    logical, save                 :: firststep  = .true.
    
    integer, save :: ierr, ndim, &
                     id_flowelemdim, &
                     id_flowlinkdim, &
                     id_timedim,     &
                     id_time, &
                     id_H_mean, id_H_var, id_H_min, id_H_max, &
                     id_E_mean, id_E_var, id_E_min, id_E_max, &
                     id_R_mean, id_R_var, id_R_min, id_R_max, &
                     id_D_mean, id_D_var, id_D_min, id_D_max, &
                     id_Fx_mean, id_Fx_var, id_Fx_min, id_Fx_max, &
                     id_Fy_mean, id_Fy_var, id_Fy_min, id_Fy_max, &                     
                     id_DR_mean, id_DR_var, id_DR_min, id_DR_max, &
                     id_s1_mean, id_s1_var, id_s1_min, id_s1_max, &
                     id_u_mean, id_u_var, id_u_min, id_u_max, &
                     id_v_mean, id_v_var, id_v_min, id_v_max, &
                     id_cwav_mean, id_cwav_var, id_cwav_min, id_cwav_max, &
                     id_cgwav_mean, id_cgwav_var, id_cgwav_min, id_cgwav_max, &
                     id_urms_mean, id_urms_var, id_urms_min, id_urms_max, &
                     id_ustx_mean, id_ustx_var, id_ustx_min, id_ustx_max, &
                     id_usty_mean, id_usty_var, id_usty_min, id_usty_max, &
                     id_thetamean_mean, id_thetamean_var, id_thetamean_min, id_thetamean_max, &
                     id_sigmwav_mean, id_sigmwav_var, id_sigmwav_min, id_sigmwav_max

    integer                :: itim, k
    integer,optional       :: jaseparate
    
    double precision, allocatable    :: temp(:)
    allocate(temp(1:ndx), stat=ierr)

    ! Use nr of dimensions in netCDF file a quick check whether vardefs were written
    ! before in previous calls.
    ndim = 0
    ierr = nf90_inquire(imapfile, nDimensions=ndim)

    ! Only write net and flow geometry data the first time, or for a separate map file.
    if (ndim == 0) then
       call unc_write_flowgeom_filepointer(imapfile) ! UNC_CONV_CFOLD ! Write time-independent flow geometry data
       
       ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim)
       ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim)
       
       ! Time
       ierr = nf90_def_dim(imapfile, 'time', nf90_unlimited, id_timedim)
       call check_error(ierr, 'def time dim')
       ierr = nf90_def_var(imapfile, 'time', nf90_double, id_timedim,  id_time)
       ierr = nf90_put_att(imapfile, id_time,  'units'        , 'seconds since '//refdat(1:4)//'-'//refdat(5:6)//'-'//refdat(7:8)//' 00:00:00')
       ierr = nf90_put_att(imapfile, id_time,  'standard_name', 'time') 
              
       ! Shortcut 1
       idims(1) = id_flowelemdim 
       idims(2) = id_timedim 
       
       ! Flow data on centres
      if (jaavgwriteall>0 .or. jaavgwriteH>0) then
       call definencvar(imapfile,id_H_mean  ,nf90_double,idims,2, 'H_mean'  , 'mean rms wave height', 'm', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_H_var   ,nf90_double,idims,2, 'H_var'  , 'variance rms wave height', 'm2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_H_min   ,nf90_double,idims,2, 'H_min'  , 'min rms wave height', 'm', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_H_max   ,nf90_double,idims,2, 'H_max'  , 'max rms wave height', 'm', 'FlowElem_xcc FlowElem_ycc')
      end if 
       
      if (jaavgwriteall>0 .or. jaavgwriteE>0) then
       call definencvar(imapfile,id_E_mean  ,nf90_double,idims,2, 'E_mean'  , 'mean bulk wave energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_E_var   ,nf90_double,idims,2, 'E_var'  , 'variance bulk wave energy', 'J2 m-4', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_E_min   ,nf90_double,idims,2, 'E_min'  , 'min bulk wave energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_E_max   ,nf90_double,idims,2, 'E_max'  , 'max bulk wave energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
      end if
       
      if (jaavgwriteall>0 .or. jaavgwriteR>0) then
       call definencvar(imapfile,id_R_mean  ,nf90_double,idims,2, 'R_mean'  , 'mean roller energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_R_var   ,nf90_double,idims,2, 'R_var'  , 'variance roller energy', 'J2 m-4', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_R_min   ,nf90_double,idims,2, 'R_min'  , 'min roller energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_R_max   ,nf90_double,idims,2, 'R_max'  , 'max roller energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
      end if
              
      if (jaavgwriteall>0 .or. jaavgwriteD>0) then
       call definencvar(imapfile,id_D_mean  ,nf90_double,idims,2, 'D_mean'  , 'mean wave breaking dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_D_var   ,nf90_double,idims,2, 'D_var'  , 'variance wave breaking dissipation', 'W2 m-4', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_D_min   ,nf90_double,idims,2, 'D_min'  , 'min wave breaking dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_D_max   ,nf90_double,idims,2, 'D_max'  , 'max wave breaking dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
              
       call definencvar(imapfile,id_DR_mean  ,nf90_double,idims,2, 'DR_mean'  , 'mean roller energy dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_DR_var   ,nf90_double,idims,2, 'DR_var'  , 'variance roller energy dissipation', 'W2 m-4', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_DR_min   ,nf90_double,idims,2, 'DR_min'  , 'min roller energy dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_DR_max   ,nf90_double,idims,2, 'DR_max'  , 'max roller energy dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
      end if
       
      if (jaavgwriteall>0 .or. jaavgwriteCel>0) then
       call definencvar(imapfile,id_cwav_mean  ,nf90_double,idims,2, 'cwav_mean'  , 'mean wave phase velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cwav_var   ,nf90_double,idims,2, 'cwav_var'  , 'variance wave phase velocity', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cwav_min   ,nf90_double,idims,2, 'cwav_min'  , 'min wave phase velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cwav_max   ,nf90_double,idims,2, 'cwav_max'  , 'max wave phase velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
              
       call definencvar(imapfile,id_cgwav_mean  ,nf90_double,idims,2, 'cgwav_mean'  , 'mean wave group velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cgwav_var   ,nf90_double,idims,2, 'cgwav_var'  , 'variance wave group velocity', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cgwav_min   ,nf90_double,idims,2, 'cgwav_min'  , 'min wave group velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cgwav_max   ,nf90_double,idims,2, 'cgwav_max'  , 'max wave group velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
      end if

      if (jaavgwriteall>0 .or. jaavgwriteS>0) then
         call definencvar(imapfile,id_s1_mean  ,nf90_double,idims,2, 's1_mean'  , 'mean water level', 'm', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_s1_var   ,nf90_double,idims,2, 's1_var'  , 'variance water level', 'm2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_s1_min   ,nf90_double,idims,2, 's1_min'  , 'min water level', 'm', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_s1_max   ,nf90_double,idims,2, 's1_max'  , 'max water level', 'm', 'FlowElem_xcc FlowElem_ycc')
      end if
       
      if (jaavgwriteall>0 .or. jaavgwriteSigm>0) then
       call definencvar(imapfile,id_sigmwav_mean  ,nf90_double,idims,2, 'sigmwav_mean'  , 'mean of mean frequency', 'rad s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_sigmwav_var   ,nf90_double,idims,2, 'sigmwav_var'  , 'variance mean frequency', 'rad2 s-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_sigmwav_min   ,nf90_double,idims,2, 'sigmwav_min'  , 'min mean frequency', 'rad s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_sigmwav_max   ,nf90_double,idims,2, 'sigmwav_max'  , 'max mean frequency', 'rad s-1', 'FlowElem_xcc FlowElem_ycc')
      end if
       
      if (jaavgwriteall>0 .or. jaavgwriteDir>0) then
         call definencvar(imapfile,id_thetamean_mean  ,nf90_double,idims,2, 'thetamean_mean'  , 'mean of mean wave angle', 'deg from N', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_thetamean_var   ,nf90_double,idims,2, 'thetamean_var'  , 'variance mean wave angle', 'deg2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_thetamean_min   ,nf90_double,idims,2, 'thetamean_min'  , 'min mean wave angle', 'deg', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_thetamean_max   ,nf90_double,idims,2, 'thetamean_max'  , 'max mean wave angle', 'deg', 'FlowElem_xcc FlowElem_ycc')
      end if

      if (jaavgwriteall>0 .or. jaavgwriteF>0) then
         call definencvar(imapfile,id_Fx_mean  ,nf90_double,idims,2, 'fx_mean'  , 'mean of wave force, x-component', 'N m-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_Fx_var   ,nf90_double,idims,2, 'fx_var'  , 'variance wave force, x-component', 'N2 m-4', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_Fx_min   ,nf90_double,idims,2, 'fx_min'  , 'min wave force, x-component', 'N m-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_Fx_max   ,nf90_double,idims,2, 'fx_max'  , 'max wave force, x-component', 'N m-2', 'FlowElem_xcc FlowElem_ycc')

         call definencvar(imapfile,id_Fy_mean  ,nf90_double,idims,2, 'fy_mean'  , 'mean of wave force, y-component', 'N m-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_Fy_var   ,nf90_double,idims,2, 'fy_var'  , 'variance wave force, y-component', 'N2 m-4', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_Fy_min   ,nf90_double,idims,2, 'fy_min'  , 'min wave force, y-component', 'N m-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_Fy_max   ,nf90_double,idims,2, 'fy_max'  , 'max wave force, y-component', 'N m-2', 'FlowElem_xcc FlowElem_ycc')
      end if

      if (jaavgwriteall>0 .or. jaavgwriteU>0) then
         call definencvar(imapfile,id_ustx_mean  ,nf90_double,idims,2, 'ustx_mean'  , 'mean of stokes drift, x-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_ustx_var   ,nf90_double,idims,2, 'ustx_var'  , 'variance stokes drift, x-component', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_ustx_min   ,nf90_double,idims,2, 'ustx_min'  , 'min stokes drift, x-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_ustx_max   ,nf90_double,idims,2, 'ustx_max'  , 'max stokes drift, x-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')

         call definencvar(imapfile,id_usty_mean  ,nf90_double,idims,2, 'usty_mean'  , 'mean of stokes drift, y-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_usty_var   ,nf90_double,idims,2, 'usty_var'  , 'variance stokes drift, y-component', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_usty_min   ,nf90_double,idims,2, 'usty_min'  , 'min stokes drift, y-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_usty_max   ,nf90_double,idims,2, 'usty_max'  , 'max stokes drift, y-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')

         call definencvar(imapfile,id_u_mean  ,nf90_double,idims,2, 'ucx_mean'  , 'mean of cell centre velocity, x-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_u_var   ,nf90_double,idims,2, 'ucx_var'  , 'variance cell centre velocity, x-component', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_u_min   ,nf90_double,idims,2, 'ucx_min'  , 'min cell centre velocity, x-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_u_max   ,nf90_double,idims,2, 'ucx_max'  , 'max cell centre velocity, x-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')

         call definencvar(imapfile,id_v_mean  ,nf90_double,idims,2, 'ucy_mean'  , 'mean of cell centre velocity, y-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_v_var   ,nf90_double,idims,2, 'ucy_var'  , 'variance cell centre velocity, y-component', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_v_min   ,nf90_double,idims,2, 'ucy_min'  , 'min cell centre velocity, y-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_v_max   ,nf90_double,idims,2, 'ucy_max'  , 'max cell centre velocity, y-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
      end if

      if (jaavgwriteall>0 .or. jaavgwriteUrms>0) then
         call definencvar(imapfile,id_urms_mean  ,nf90_double,idims,2, 'urms_mean'  , 'mean of rms orbital velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_urms_var   ,nf90_double,idims,2, 'urms_var'  , 'variance rms orbital velocity', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_urms_min   ,nf90_double,idims,2, 'urms_min'  , 'min rms orbital velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_urms_max   ,nf90_double,idims,2, 'urms_max'  , 'max rms orbital velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
      end if

       ierr = nf90_enddef(imapfile)
       firststep = .false.
    endif   
    ! End of writing time-independent flow geometry data.

    ! -- Inquire id's belonging to map file ------------------------
    if (firststep .and. ndim>0) then 
       ! 
       ! 
       ! this step is necessary because if a snapshot_map.nc file is written
       ! in between two map file outputs the saved id's may have changed
       !
       firststep = .false. 
       !
       ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim)
       ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim)
       !
       ! Time
       ierr = nf90_inq_dimid(imapfile, 'time', id_timedim)
       ierr = nf90_inq_varid(imapfile, 'time', id_time)       
       !    
       ! Size of latest timestep
       ! ierr = nf90_inq_varid(imapfile, 'timestep', id_timestep)
      if (jaavgwriteall>0 .or. jaavgwriteE>0) then
       ierr = nf90_inq_varid(imapfile, 'E_mean', id_E_mean)
       ierr = nf90_inq_varid(imapfile, 'E_var', id_E_var)
       ierr = nf90_inq_varid(imapfile, 'E_min', id_E_min)
       ierr = nf90_inq_varid(imapfile, 'E_max', id_E_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteH>0) then
       ierr = nf90_inq_varid(imapfile, 'H_mean', id_H_mean)
       ierr = nf90_inq_varid(imapfile, 'H_var', id_H_var)
       ierr = nf90_inq_varid(imapfile, 'H_min', id_H_min)
       ierr = nf90_inq_varid(imapfile, 'H_max', id_H_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteR>0) then
       ierr = nf90_inq_varid(imapfile, 'R_mean', id_R_mean)
       ierr = nf90_inq_varid(imapfile, 'R_var', id_R_var)
       ierr = nf90_inq_varid(imapfile, 'R_min', id_R_min)
       ierr = nf90_inq_varid(imapfile, 'R_max', id_R_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteD>0) then
       ierr = nf90_inq_varid(imapfile, 'D_mean', id_D_mean)
       ierr = nf90_inq_varid(imapfile, 'D_var', id_D_var)
       ierr = nf90_inq_varid(imapfile, 'D_min', id_D_min)
       ierr = nf90_inq_varid(imapfile, 'D_max', id_D_max)

       ierr = nf90_inq_varid(imapfile, 'DR_mean', id_DR_mean)
       ierr = nf90_inq_varid(imapfile, 'DR_var', id_DR_var)
       ierr = nf90_inq_varid(imapfile, 'DR_min', id_DR_min)
       ierr = nf90_inq_varid(imapfile, 'DR_max', id_DR_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteF>0) then
       ierr = nf90_inq_varid(imapfile, 'Fx_mean', id_Fx_mean)
       ierr = nf90_inq_varid(imapfile, 'Fx_var', id_Fx_var)
       ierr = nf90_inq_varid(imapfile, 'Fx_min', id_Fx_min)
       ierr = nf90_inq_varid(imapfile, 'Fx_max', id_Fx_max)

       ierr = nf90_inq_varid(imapfile, 'Fy_mean', id_Fy_mean)
       ierr = nf90_inq_varid(imapfile, 'Fy_var', id_Fy_var)
       ierr = nf90_inq_varid(imapfile, 'Fy_min', id_Fy_min)
       ierr = nf90_inq_varid(imapfile, 'Fy_max', id_Fy_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteU>0) then
         ierr = nf90_inq_varid(imapfile, 'ustx_mean', id_ustx_mean)
         ierr = nf90_inq_varid(imapfile, 'ustx_var', id_ustx_var)
         ierr = nf90_inq_varid(imapfile, 'ustx_min', id_ustx_min)
         ierr = nf90_inq_varid(imapfile, 'ustx_max', id_ustx_max)

         ierr = nf90_inq_varid(imapfile, 'usty_mean', id_usty_mean)
         ierr = nf90_inq_varid(imapfile, 'usty_var', id_usty_var)
         ierr = nf90_inq_varid(imapfile, 'usty_min', id_usty_min)
         ierr = nf90_inq_varid(imapfile, 'usty_max', id_usty_max)

         ierr = nf90_inq_varid(imapfile, 'u_mean', id_u_mean)
         ierr = nf90_inq_varid(imapfile, 'u_var', id_u_var)
         ierr = nf90_inq_varid(imapfile, 'u_min', id_u_min)
         ierr = nf90_inq_varid(imapfile, 'u_max', id_u_max)

         ierr = nf90_inq_varid(imapfile, 'v_mean', id_v_mean)
         ierr = nf90_inq_varid(imapfile, 'v_var', id_v_var)
         ierr = nf90_inq_varid(imapfile, 'v_min', id_v_min)
         ierr = nf90_inq_varid(imapfile, 'v_max', id_v_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteUrms>0) then
       ierr = nf90_inq_varid(imapfile, 'urms_mean', id_urms_mean)
       ierr = nf90_inq_varid(imapfile, 'urms_var', id_urms_var)
       ierr = nf90_inq_varid(imapfile, 'urms_min', id_urms_min)
       ierr = nf90_inq_varid(imapfile, 'urms_max', id_urms_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteCel>0) then
       ierr = nf90_inq_varid(imapfile, 'cwav_mean', id_cwav_mean)
       ierr = nf90_inq_varid(imapfile, 'cwav_var', id_cwav_var)
       ierr = nf90_inq_varid(imapfile, 'cwav_min', id_cwav_min)
       ierr = nf90_inq_varid(imapfile, 'cwav_max', id_cwav_max)

       ierr = nf90_inq_varid(imapfile, 'cgwav_mean', id_cgwav_mean)
       ierr = nf90_inq_varid(imapfile, 'cgwav_var', id_cgwav_var)
       ierr = nf90_inq_varid(imapfile, 'cgwav_min', id_cgwav_min)
       ierr = nf90_inq_varid(imapfile, 'cgwav_max', id_cgwav_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteDir>0) then
       ierr = nf90_inq_varid(imapfile, 'thetamean_mean', id_thetamean_mean)
       ierr = nf90_inq_varid(imapfile, 'thetamean_var', id_thetamean_var)
       ierr = nf90_inq_varid(imapfile, 'thetamean_min', id_thetamean_min)
       ierr = nf90_inq_varid(imapfile, 'thetamean_max', id_thetamean_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteSigm>0) then
       ierr = nf90_inq_varid(imapfile, 'sigmwav_mean', id_sigmwav_mean)
       ierr = nf90_inq_varid(imapfile, 'sigmwav_var', id_sigmwav_var)
       ierr = nf90_inq_varid(imapfile, 'sigmwav_min', id_sigmwav_min)
       ierr = nf90_inq_varid(imapfile, 'sigmwav_max', id_sigmwav_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteS>0) then
       ierr = nf90_inq_varid(imapfile, 's1_mean', id_s1_mean)
       ierr = nf90_inq_varid(imapfile, 's1_var', id_s1_var)
       ierr = nf90_inq_varid(imapfile, 's1_min', id_s1_min)
       ierr = nf90_inq_varid(imapfile, 's1_max', id_s1_max)
      end if
    end if    
    
    ! -- Start data writing (flow data) ------------------------
    it_wav   = it_wav+1
    itim     = it_wav ! Increment time dimension index  

    ! Time
    ierr = nf90_put_var(imapfile, id_time    , tim, (/  itim /))

    ! Data on flow nodes
   if (jaavgwriteall>0 .or. jaavgwriteE>0) then
    ierr = nf90_put_var(imapfile, id_E_mean, E_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_E_var, E_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_E_max, E_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_E_min, E_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if

   if (jaavgwriteall>0 .or. jaavgwriteH>0) then
      temp = 0d0
      do k = 1, ndxi     ! stack overflow
         temp(k) = sqrt(H_varsquare(k))    
      end do
      ierr = nf90_put_var(imapfile, id_H_mean, temp(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_H_var, H_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_H_max, H_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_H_min, H_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if

   if (jaavgwriteall>0 .or. jaavgwriteR>0) then
    ierr = nf90_put_var(imapfile, id_R_mean, R_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_R_var, R_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_R_max, R_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_R_min, R_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if

   if (jaavgwriteall>0 .or. jaavgwriteD>0) then
    ierr = nf90_put_var(imapfile, id_D_mean, D_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_D_var, D_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_D_max, D_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_D_min, D_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

    ierr = nf90_put_var(imapfile, id_DR_mean, DR_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_DR_var, DR_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_DR_max, DR_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_DR_min, DR_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if

   if (jaavgwriteall>0 .or. jaavgwriteCel>0) then
    ierr = nf90_put_var(imapfile, id_cwav_mean, cwav_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cwav_var, cwav_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cwav_max, cwav_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cwav_min, cwav_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

    ierr = nf90_put_var(imapfile, id_cgwav_mean, cgwav_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cgwav_var, cgwav_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cgwav_max, cgwav_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cgwav_min, cgwav_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if

   if (jaavgwriteall>0 .or. jaavgwriteDir>0) then
      temp = 0d0
      do k = 1, ndxi ! stack
         temp(k) = 270.d0 - mod(2.d0*pi + atan2(nint(thetamean_mean(k))/1d7, mod(thetamean_mean(k),1.d0)*1d1), 2.d0*pi) / pi * 180d0
      end do
      ierr = nf90_put_var(imapfile, id_thetamean_mean, temp(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_thetamean_var, thetamean_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_thetamean_max, thetamean_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_thetamean_min, thetamean_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if

   if (jaavgwriteall>0 .or. jaavgwriteSigm>0) then
    ierr = nf90_put_var(imapfile, id_sigmwav_mean, sigmwav_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_sigmwav_var, sigmwav_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_sigmwav_max, sigmwav_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_sigmwav_min, sigmwav_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if
 
   if (jaavgwriteall>0 .or. jaavgwriteS>0) then
    ierr = nf90_put_var(imapfile, id_s1_mean, s1_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_s1_var, s1_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_s1_max, s1_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_s1_min, s1_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if

   if (jaavgwriteall>0 .or. jaavgwriteF>0) then
      ierr = nf90_put_var(imapfile, id_Fx_mean, Fx_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_Fx_var, Fx_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_Fx_max, Fx_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_Fx_min, Fx_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

      ierr = nf90_put_var(imapfile, id_Fy_mean, Fy_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_Fy_var, Fy_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_Fy_max, Fy_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_Fy_min, Fy_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if
      
   if (jaavgwriteall>0 .or. jaavgwriteU>0) then
      ierr = nf90_put_var(imapfile, id_ustx_mean, ust_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_ustx_var, ust_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_ustx_max, ust_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_ustx_min, ust_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

      ierr = nf90_put_var(imapfile, id_usty_mean, vst_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_usty_var,  vst_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_usty_max,  vst_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_usty_min,  vst_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

      ierr = nf90_put_var(imapfile, id_u_mean, u_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_u_var,  u_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_u_max,  u_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_u_min,  u_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 

      ierr = nf90_put_var(imapfile, id_v_mean, v_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_v_var,  v_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_v_max,  v_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_v_min,  v_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
   endif

   if (jaavgwriteall>0 .or. jaavgwriteUrms>0) then      
      temp = sqrt(urms_varsquare)
      ierr = nf90_put_var(imapfile, id_urms_mean, temp, (/ 1, itim /), (/ ndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_urms_var, urms_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_urms_max, urms_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_urms_min, urms_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if
   end subroutine unc_write_wav_filepointer

!! Construct averages for netcdf output
!! (Re)allocation in flow_waveinit
subroutine xbeach_makeaverages(dt)
   use m_flow
   use m_flowgeom
   use m_flowtimes
   use m_xbeach_data
   use m_xbeach_avgoutput
   use m_alloc
   use m_sferic
   implicit none

   double precision, intent(in)              :: dt                         ! timestep
   double precision                          :: mult, fill
   integer                                   :: ierr, result1, result2

   double precision, allocatable                  :: tvar_sin(:)
   double precision, allocatable                  :: tvar_cos(:)
   double precision, allocatable                  :: oldmean(:), ux(:), uy(:)

   ierr = 1
   call realloc(tvar_sin, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call realloc(tvar_cos, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call realloc(oldmean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call realloc(ux, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call realloc(uy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)

   mult = max(dt/ti_wav,0.d0)
   !multcum = multcum + mult
   !write(*,*) 'Multiplier: ', mult, ', cumulative proportion: ', multcum
   
   !! Data on flow nodes
   ! H
   oldmean = H_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   H_mean = H_mean + mult*H
   H_varcross = H_varcross/oldmean*H_mean + mult*2.d0*H*H_mean
   H_varsquare = H_varsquare + mult*(H)**2
   H_var = H_varsquare - H_varcross + H_mean**2
   H_max = max(H_max,H)
   H_min = min(H_min,H)

   ! E
   oldmean = E_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   E_mean = E_mean + mult*E
   E_varcross = E_varcross/oldmean*E_mean + mult*2.d0*E*E_mean
   E_varsquare = E_varsquare + mult*(E)**2
   E_var = E_varsquare - E_varcross + E_mean**2
   E_max = max(E_max,E)
   E_min = min(E_min,E)

   !R
   oldmean = R_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   R_mean = R_mean + mult*R
   R_varcross = R_varcross/oldmean*R_mean + mult*2.d0*R*R_mean
   R_varsquare = R_varsquare + mult*(R)**2
   R_var = R_varsquare - R_varcross + R_mean**2
   R_max = max(R_max,R)
   R_min = min(R_min,R)

   ! D
   oldmean = D_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   D_mean = D_mean + mult*D
   D_varcross = D_varcross/oldmean*D_mean + mult*2.d0*D*D_mean
   D_varsquare = D_varsquare + mult*(D)**2
   D_var = D_varsquare - D_varcross + D_mean**2
   D_max = max(D_max,D)
   D_min = min(D_min,D)

   ! DR
   oldmean = DR_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere

   !< DEBUG
   DR_mean = DR_mean + mult*DR
   DR_varcross = 1d-20 + DR_varcross/oldmean*DR_mean + mult*2.d0*DR*DR_mean
   DR_varsquare = DR_varsquare + mult*(DR)**2
   DR_var = DR_varsquare - DR_varcross + DR_mean**2
   DR_max = max(DR_max,DR)
   DR_min = min(DR_min,DR)
   !</ DEBUG

  ! cwav
   oldmean = cwav_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   cwav_mean = cwav_mean + mult*cwav
   cwav_varcross = cwav_varcross/oldmean*cwav_mean + mult*2.d0*cwav*cwav_mean
   cwav_varsquare = cwav_varsquare + mult*(cwav)**2
   cwav_var = cwav_varsquare - cwav_varcross + cwav_mean**2
   cwav_max = max(cwav_max,cwav)
   cwav_min = min(cwav_min,cwav)

  ! cgwav
   oldmean = cgwav_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   cgwav_mean = cgwav_mean + mult*cgwav
   cgwav_varcross = cgwav_varcross/oldmean*cgwav_mean + mult*2.d0*cgwav*cgwav_mean
   cgwav_varsquare = cgwav_varsquare + mult*(cgwav)**2
   cgwav_var = cgwav_varsquare - cgwav_varcross + cgwav_mean**2
   cgwav_max = max(cgwav_max,cgwav)
   cgwav_min = min(cgwav_min,cgwav)

  ! sigmwav
   oldmean = sigmwav_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   sigmwav_mean = sigmwav_mean + mult*sigmwav
   sigmwav_varcross = sigmwav_varcross/oldmean*sigmwav_mean + mult*2.d0*sigmwav*sigmwav_mean
   sigmwav_varsquare = sigmwav_varsquare + mult*(sigmwav)**2
   sigmwav_var = sigmwav_varsquare - sigmwav_varcross + sigmwav_mean**2
   sigmwav_max = max(sigmwav_max,sigmwav)
   sigmwav_min = min(sigmwav_min,sigmwav)

   ! thetamean
   oldmean = thetamean_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   thetamean_sin = nint(thetamean_mean) / 1d1 + nint(mult*sin(thetamean)*1e6)
   thetamean_cos = mod(thetamean_mean,1.d0) * 1d7 + nint(mult*cos(thetamean)*1e6)
   thetamean_mean = thetamean_sin*1e1 + thetamean_cos/1e7
   thetamean_varcross = thetamean_varcross/oldmean*thetamean_mean + mult*2.d0*thetamean*thetamean_mean
   thetamean_varsquare = thetamean_varsquare + mult*(thetamean)**2
   thetamean_var = thetamean_varsquare - thetamean_varcross + thetamean_mean**2
   thetamean_max = max(thetamean_max,thetamean)
   thetamean_min = min(thetamean_min,thetamean)
   
   call realloc(oldmean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('oldmean  (ndx)', ierr, ndx)

   ! u: x-component
   oldmean = u_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   ux = ucx
   u_mean = u_mean + mult*ux
   u_varcross = u_varcross/oldmean*u_mean + mult*2.d0*ux*u_mean
   u_varsquare = u_varsquare + mult*(ux)**2
   u_var = u_varsquare - u_varcross + u_mean**2
   u_max = max(u_max,ux)
   u_min = min(u_min,ux)

   ! v: y-component
   oldmean = v_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   uy = ucy
   v_mean = v_mean + mult*uy
   v_varcross = v_varcross/oldmean*v_mean + mult*2.d0*uy*v_mean
   v_varsquare = v_varsquare + mult*(uy)**2
   v_var = v_varsquare - v_varcross + v_mean**2
   v_max = max(v_max,uy)
   v_min = min(v_min,uy)

   ! Fx: y-component
   oldmean = Fx_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   Fx_mean = Fx_mean + mult*Fx_cc
   Fx_varcross = Fx_varcross/oldmean*Fx_mean + mult*2.d0*Fx_cc*Fx_mean
   Fx_varsquare = Fx_varsquare + mult*(Fx_cc)**2
   Fx_var = Fx_varsquare - Fx_varcross + Fx_mean**2
   Fx_max = max(Fx_max,Fx_cc)
   Fx_min = min(Fx_min,Fx_cc)

   ! Fy
   oldmean = Fy_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   Fy_mean = Fy_mean + mult*Fy_cc
   Fy_varcross = Fy_varcross/oldmean*Fy_mean + mult*2.d0*Fy_cc*Fy_mean
   Fy_varsquare = Fy_varsquare + mult*(Fy_cc)**2
   Fy_var = Fy_varsquare - Fy_varcross + Fy_mean**2
   Fy_max = max(Fy_max,Fy_cc)
   Fy_min = min(Fy_min,Fy_cc)

   ! ust
   oldmean = ust_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   ux = ustx_cc
   ust_mean = ust_mean + mult*ux
   ust_varcross = ust_varcross/oldmean*ust_mean + mult*2.d0*ux*ust_mean
   ust_varsquare = ust_varsquare + mult*(ux)**2
   ust_var = ust_varsquare - ust_varcross + ust_mean**2
   ust_max = max(ust_max,ux)
   ust_min = min(ust_min,ux)

   ! vst
   oldmean = vst_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   uy = usty_cc
   vst_mean = vst_mean + mult*uy
   vst_varcross = vst_varcross/oldmean*vst_mean + mult*2.d0*uy*vst_mean
   vst_varsquare = vst_varsquare + mult*(uy)**2
   vst_var = vst_varsquare - vst_varcross + vst_mean**2
   vst_max = max(vst_max,uy)
   vst_min = min(vst_min,uy)

   ! urms
   oldmean = urms_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   urms_mean = urms_mean + mult*urms_cc
   urms_varcross = urms_varcross/oldmean*urms_mean + mult*2.d0*urms_cc*urms_mean
   urms_varsquare = urms_varsquare + mult*(urms_cc)**2
   urms_var = urms_varsquare - urms_varcross + urms_mean**2
   urms_max = max(urms_max,urms_cc)
   urms_min = min(urms_min,urms_cc)
     
   ! s1
   oldmean = s1_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   s1_mean = s1_mean + mult*s1
   s1_varcross = s1_varcross/oldmean*s1_mean + mult*2.d0*s1*s1_mean
   s1_varsquare = s1_varsquare + mult*(s1)**2
   s1_var = s1_varsquare - s1_varcross + s1_mean**2
   s1_max = max(s1_max,s1)
   s1_min = min(s1_min,s1)

   ierr = 0
1234 continue
   deallocate(ux, uy, tvar_sin,tvar_cos,oldmean, stat=ierr)
   return

end subroutine xbeach_makeaverages

subroutine xbeach_clearaverages()
   use m_xbeach_avgoutput
   implicit none

   integer               :: ierr

   ierr = 1
   !multcum = 0d0
   H_mean = 0d0; H_var  = 0d0; H_min  = huge(0d0); H_max  = -1d0*huge(0d0); H_varcross = 0d0; H_varsquare = 0d0
   E_mean = 0d0; E_var  = 0d0; E_min  = huge(0d0); E_max  = -1d0*huge(0d0); E_varcross = 0d0; E_varsquare = 0d0
   R_mean = 0d0; R_var  = 0d0; R_min  = huge(0d0); R_max  = -1d0*huge(0d0); R_varcross = 0d0; R_varsquare = 0d0
   D_mean = 0d0; D_var  = 0d0; D_min  = huge(0d0); D_max  = -1d0*huge(0d0); D_varcross = 0d0; D_varsquare = 0d0
   DR_mean = 0d0; DR_var  = 0d0; DR_min  = huge(0d0); DR_max  = -1d0*huge(0d0); DR_varcross = 0d0; DR_varsquare = 0d0
   ust_mean = 0d0; ust_var  = 0d0; ust_min  = huge(0d0); ust_max  = -1d0*huge(0d0); ust_varcross = 0d0; ust_varsquare = 0d0
   urms_mean = 0d0; urms_var  = 0d0; urms_min  = huge(0d0); urms_max  = -1d0*huge(0d0); urms_varcross = 0d0; urms_varsquare = 0d0
   thetamean_mean = 0d0; thetamean_var  = 0d0; thetamean_min  = huge(0d0); thetamean_max  = -1d0*huge(0d0); thetamean_varcross = 0d0; 
   thetamean_varsquare = 0d0; thetamean_sin = 0d0; thetamean_cos = 0d0
   cwav_mean = 0d0; cwav_var  = 0d0; cwav_min  = huge(0d0); cwav_max  = -1d0*huge(0d0); cwav_varcross = 0d0; cwav_varsquare = 0d0
   cgwav_mean = 0d0; cgwav_var  = 0d0; cgwav_min  = huge(0d0); cgwav_max  = -1d0*huge(0d0); cgwav_varcross = 0d0; cgwav_varsquare = 0d0
   Fx_mean = 0d0; Fx_var  = 0d0; Fx_min  = huge(0d0); Fx_max  = -1d0*huge(0d0); Fx_varcross = 0d0; Fx_varsquare = 0d0
   Fy_mean = 0d0; Fy_var  = 0d0; Fy_min  = huge(0d0); Fy_max  = -1d0*huge(0d0); Fy_varcross = 0d0; Fy_varsquare = 0d0
   s1_mean = 0d0; s1_var  = 0d0; s1_min  = huge(0d0); s1_max  = -1d0*huge(0d0); s1_varcross = 0d0; s1_varsquare = 0d0
   u_mean = 0d0; u_var  = 0d0; u_min  = huge(0d0); u_max  = -1d0*huge(0d0); u_varcross = 0d0; u_varsquare = 0d0
   v_mean = 0d0; v_var  = 0d0; v_min  = huge(0d0);v_max  = -1d0*huge(0d0); v_varcross = 0d0; v_varsquare = 0d0
   sigmwav_mean = 0d0; sigmwav_var  = 0d0; sigmwav_min  = huge(0d0); sigmwav_max  = -1d0*huge(0d0); sigmwav_varcross = 0d0; sigmwav_varsquare = 0d0

   ierr = 0
1234 continue
   return

end subroutine xbeach_clearaverages

end module m_xbeach_netcdf
