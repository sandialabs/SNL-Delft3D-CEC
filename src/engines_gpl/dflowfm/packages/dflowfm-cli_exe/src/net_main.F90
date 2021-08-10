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

! $Id: net_main.F90 65778 2020-01-14 14:07:42Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm-cli_exe/src/net_main.F90 $
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
!> @file net_main.F90
!! The main program 'network' and all net-related routines.
!! Flow-related routines are in unstruc.f90
!<

!> \mainpage Unstruc API docs
!! \li \b Main \b program: net.f90
!! \li \b Model \b setup: unstruc_model.f90
!! \li \b Network \b data: network.f90 + network_data.f90
!! \li \b Global \b data: modules.f90 (flow, geometry, times, parameters, ...)
!! \li \b GUI \b and \b network \b algorithms: net.f90
!! \li \b Unstructured \b flow \b solver: unstruc.f90
!! \li \b Matrix \b solver: solve_guus.f90
!! \li \b Stations \b and \b cross-sections: monitoring.f90
!! \li \b Various \b helpers: REST.F90 unstruc_ini.f90, unstruc_files.f90, unstruc_startup.f90, unstruc_display.f90, rcm.f90
!! \li \b RGFgrid \b routines: RGFSTUFF.f90
!! \li \b NetCDF-IO: unstruc_netcdf.f90
!! \li \b WAQ-output: waq.f90, wrwaq.f90
!! \li \b More: see file list
   PROGRAM unstruc
   USE M_GRID
   USE M_POLYGON
   USE M_LANDBOUNDARY
   USE M_BOAT
   use m_netw
   use unstruc_startup
   use unstruc_model
   use netcdf
   use properties
   use m_observations
   use unstruc_netcdf
   use unstruc_messages
   USE UNSTRUC_DISPLAY
   USE M_WEARELT
   use m_flowparameters
   use unstruc_api
   use dfm_error
   use gridoperations
   use m_commandline_option
   
   use m_partitioninfo
#ifdef HAVE_MPI
   use mpi
#endif
   
   !use ifcore  !nanrelease

   
!   use ftnunit
!   use unstruc_tests
   implicit none

   integer :: MODE,NFLD, KEY
   integer :: JQN
   integer :: JDEMO

   COMMON /MODENOW/ MODE,NFLD
   COMMON /QNRGF/ JQN
   COMMON /DEMO/ JDEMO
   integer :: ierr, lastmode, IDUM
   LOGICAL :: JAWEL
   character*4 domain 
   
   integer                   :: i, L, istat, n12
   integer                   :: Lrst = 0, Lmap = 0, L_merge = 0, jamergedrst = 0, Lmap1 = 0
   integer, parameter        :: numlen=4        !< number of digits in domain number string/filename
   integer, parameter        :: maxnamelen=256  !< number of digits in filename 
   character(len=numlen)     :: sdmn_loc        !< domain number string
   character(len=maxnamelen) :: filename        
   character(len=maxnamelen) :: restartfile     !< storing the name of the restart files
   character(len=maxnamelen) :: md_mapfile_base !< storing the user-defined map file
   character(len=maxnamelen) :: md_flowgeomfile_base !< storing the user-defined flowgeom file
   character(len=maxnamelen) :: md_classmapfile_base !< storing the user-defined class map file
    
   integer, external         :: iget_jaopengl
   integer, external         :: read_commandline
   integer, external         :: flow_modelinit

#if HAVE_DISPLAY==0
! For dflowfm-cli executable, switch off all GUI calls here at *runtime*,
! by setting jaGUI = 0.
! All kernel code does not need to be recompiled, because there is only
! two places that preprocess HAVE_DISPLAY at *compiletime*, all the rest
! is done by `if (jaGUI .. )`
    jaGUI = 0          !< GUI (1) or not (0)
#endif

#ifdef HAVE_MPI
   ! From calling C/C++ side, construct an MPI communicator, and call
   ! MPI_Fint MPI_Comm_c2f(MPI_Comm comm) to convert the C comm handle
   ! to a FORTRAN comm handle.
   call mpi_init(ierr)
   call mpi_comm_rank(DFM_COMM_DFMWORLD,my_rank,ierr)
   call mpi_comm_size(DFM_COMM_DFMWORLD,numranks,ierr)
   ja_mpi_init_by_fm = 1

   if ( numranks.le.1 ) then
      jampi = 0
   end if
   
!  make domain number string as soon as possible
   write(sdmn, '(I4.4)') my_rank
   !write(6,*) 'my_rank =', my_rank
   
!   call pressakey()
#else
   numranks=1
   !write(6,*) 'NO MPI'
!  call pressakey()
#endif



   !INTEGER*4 OLD_FPE_FLAGS, NEW_FPE_FLAGS                                ! nanrelease
   !NEW_FPE_FLAGS = FPE_M_TRAP_OVF + FPE_M_TRAP_DIV0 + FPE_M_TRAP_INV     ! nanrelease
   !OLD_FPE_FLAGS = FOR_SET_FPE (NEW_FPE_FLAGS)                           ! nanrelease
 

    ! Only run in test mode
!    call runtests_init
!    call runtests( unstruc_test_all )
!    call runtests_final
   !               1        1         2         3         4         5          6
!   WHATST       = '@(#)   | Kernkamp Herman  ,      NETWORK, Version 1.0000; 04-07-2001'//char(0)
!   WHATST       = '@(#)WL | Deltares,               Unstruc, Version 1.0000; 20-03-2007'//char(0)   ! Starting date
!   WHATST       = '@(#)WL | Deltares,               Unstruc, Version 1.0011; 01-06-2009'//char(0)
   JDEMO        = 0
   JQN          = 2

   MMAX   = 0
   NMAX   = MMAX
   KMAX   = MMAX*NMAX
   KNX    = 8
   MXB    = 10
   LMAX   = (MMAX-1)*NMAX + (NMAX-1)*MMAX + 2*(MMAX-1)*(NMAX-1)
   MAXLAN = 500
   MAXPOL = MAXLAN
   MAXBOAT = MAXLAN
 
    md_jaopenGL = -1 ! no commandline option read for OpenGL (yet)
  
    ierr = read_commandline()
    select case(ierr)
    case (DFM_NOERR)
       continue
    case (DFM_EXIT) ! Harmless exit, no error (e.g. --help)
       goto 1234
    case default    ! Nonzero error code. Return.
       goto 1234
    end select
    

!   set jaopengl from commandline option (if available)
    call iset_jaopengl(md_jaopengl)
    CALL START()
    call resetFullFlowModel()
    CALL INIDAT()
    CALL RESETB(0)
    

#ifdef HAVE_PETSC
      call startpetsc()
#endif
  
    MODE = 1
    lastmode = 1
    NFLD = 1
    KEY  = 3
    
    if ( md_pressakey.eq.1 ) then
       call pressakey()
    end if
    
    if ( md_jatest.eq.1 ) then
       call initimer()
       do i=1,md_Nruns
     !     call axpy(md_M, md_N)
       end do
!      output timings  
       write(6,'(a,E8.2,a,E8.2)') ' WC-time Axpy test [s]: ' , gettimer(1,IAXPY), ' CPU-time Axpy test [s]: ' , gettimer(0,IAXPY)
       
       goto 1234
    end if
    
    if ( md_soltest.eq.1 ) then
       call soltest(md_CFL,md_icgsolver,md_maxmatvecs,md_epsdiff,md_epscg)
       
       goto 1234
    end if

    if ( md_convnetcells.eq.1 ) then
       ! call soltest(md_CFL,md_icgsolver,md_maxmatvecs,md_epsdiff,md_epscg)
       ! read net, write net ... md_netfile
       call findcells(0)
       call find1dcells()
       call unc_write_net(md_netfile, janetcell = 1, janetbnd = 0, jaidomain = 0)
       goto 1234
    end if

    if ( md_jamake1d2dlinks .eq. 1 ) then
       ! Make 1D2D links for already loaded net file.
       imake1d2dtype = I1D2DTP_1TO1
       ierr = make1D2Dinternalnetlinks() ! TODO: replace this by call to make1D2Dconnections, but check FILEMENU in batchmode.
       if (ierr /= DFM_NOERR) then
          write (msgbuf, '(a,a,a,i0,a)') 'Error, failed to create 1D2D links for file ''', trim(md_netfile), '''. Error code: ', ierr, '.'
          call warn_flush()
          goto 1234
       end if
       if (len_trim(iarg_outfile) == 0) then
          iarg_outfile = md_netfile ! Overwrite existing file.
       end if
       call unc_write_net(iarg_outfile, janetcell = 1, janetbnd = 0, jaidomain = 0, iconventions = UNC_CONV_UGRID)
       goto 1234
    end if

    if (jabatch == 1) then 
       call dobatch()
    endif 
    

    if ( md_japartition.eq.1 ) then
        
       if ( len_trim(md_ident) > 0 ) then ! partitionmduparse
          call partition_from_commandline(md_netfile, md_Ndomains, md_jacontiguous, md_icgsolver, md_pmethod, md_dryptsfile, md_encfile, md_genpolygon)
          L    = index(md_netfile, '_net')-1
          md_mdu = md_ident
          if (len_trim(md_restartfile) > 0) then ! If there is a restart file
             L_merge = index(md_restartfile, '_merged')
             if (L_merge > 0) then
                jamergedrst = 1
             else ! restart file is not a merged map file, then provide _rst or _map file of each subdomain
                restartfile = md_restartfile
                Lrst = index(restartfile, '_rst.nc')
                Lmap = index(restartfile, '_map.nc')
            endif
          endif

          md_mapfile_base = md_mapfile
          md_flowgeomfile_base = md_flowgeomfile
          md_classmapfile_base = md_classmap_file

          do i = 0,  Ndomains - 1
             write(sdmn_loc, '(I4.4)') i
             md_netfile = trim(md_netfile(1:L)//'_'//sdmn_loc//'_net.nc')
             if (md_genpolygon .eq. 1) then
                md_partitionfile = trim(md_netfile(1:L))//'_part.pol' 
             endif
             if (jamergedrst == 0) then ! restart file is not a merged map file, then provide _rst or _map file of each subdomain 
               if (Lrst > 0) then      ! If the restart file is a rst file
                  md_restartfile = trim(restartfile(1:Lrst-16)//sdmn_loc//'_'//restartfile(Lrst-15: Lrst+7))
               else if (Lmap > 0) then ! If the restart file is a map file
                  md_restartfile = trim(restartfile(1:Lmap)//sdmn_loc//'_map.nc')
               endif
             endif
             if (len_trim(md_mapfile_base)>0) then
                Lmap1 = index(md_mapfile_base, '_map.nc')
                if (Lmap1 == 0) then    ! Customized map-file name
                   md_mapfile = md_mapfile_base(1:index(trim(md_mapfile_base)//'.','.')-1)//'_'//sdmn_loc//".nc"
                else
                   md_mapfile = md_mapfile_base(1:index(trim(md_mapfile_base)//'.','_map')-1)//'_'//sdmn_loc//"_map.nc"
                endif
             endif
             if (len_trim(md_flowgeomfile_base)>0) then
                md_flowgeomfile = md_flowgeomfile_base(1:index(md_flowgeomfile_base,'.nc',back=.true.)-1)//'_'//sdmn_loc//".nc"
             endif
             if (len_trim(md_classmapfile_base)>0) then
                md_classmap_file = md_classmapfile_base(1:index(md_classmapfile_base,'.nc',back=.true.)-1)//'_'//sdmn_loc//".nc"
             endif
             call generatePartitionMDUFile(trim(md_ident)//'.mdu', trim(md_mdu)//'_'//sdmn_loc//'.mdu')
          enddo
       else
          call partition_from_commandline(md_netfile,md_ndomains,md_jacontiguous,md_icgsolver, md_pmethod, md_dryptsfile, md_encfile, md_genpolygon)
       end if

       goto 1234  !      stop
    end if

    if ( md_jagridgen.eq.1 ) then
       call makenet(0)
       call gridtonet()
       call unc_write_net('out_net.nc')
       goto 1234
    end if

    if ( md_jarefine.eq.1 ) then
       call refine_from_commandline()
       goto 1234
    end if
    
    if ( md_cutcells.eq.1 ) then
       n12 = 3
       call findcells(0)
       call cutcell_list(n12, '*.cut',5, 0)
       call unc_write_net('out_net.nc')
    end if
    
    if ( jagui.eq.1 .and. len_trim(md_cfgfile).gt.0 ) then
       call load_displaysettings(md_cfgfile)
    end if

   
    if (len_trim(md_ident) > 0) then
        ! An MDU file was read.
        ierr = flow_modelinit()
        if ( ierr /= DFM_NOERR ) goto 1234  ! error: finalize and stop
      
        if ( jaGUI.eq.1 .and. len_trim(md_cfgfile).eq.0 ) then
           inquire (file = trim(md_ident)//'.cfg', exist = jawel)
           if (jawel) then 
              call load_displaysettings(trim(md_ident)//'.cfg')
           endif
   
           CALL DRAWNU(KEY) ! Draw model for the first time
        end if

        if (md_jaAutoStart == MD_AUTOSTART .or. md_jaAutoStart >= MD_AUTOSTARTSTOP) then
           if (md_jaAutoStart > MD_AUTOSTARTSTOP) ntek = 0
           idum = FLOW()
           ! TODO: check whether all data (net/s1) is available before starting flow. [AvD]
           if (idum == DFM_SIGINT .or. md_jaAutoStart >= MD_AUTOSTARTSTOP) then
              goto 1234   ! finalize and stop
           end if
        end if
    end if
    
   if ( jaGUI.eq.1 ) then

   10 CONTINUE
      IF (MODE .EQ. 1) THEN
         CALL EDITPOL(MODE,KEY,NETFLOW)
      ELSE IF (MODE .EQ. 2) THEN
         CALL EDITNETW(MODE,KEY)
      ELSE IF (MODE .EQ. 3) THEN
         CALL EDITSPLINES(MODE,KEY)
      ELSE IF (MODE .EQ. 4) THEN
         IF (NFLD .EQ. 1 .OR. NFLD .EQ. 2) THEN
            CALL EDITGRID(MODE,NFLD,KEY)
         ELSE IF (NFLD .GE. 4 .AND. NFLD .LE. 12) THEN
            CALL EDITGRIDLINEBLOK(MODE,NFLD,KEY)
         ELSE IF (NFLD .GE. 14 .AND. NFLD .LE. 17)  THEN
            CALL EDITGRIDBLOK(MODE,NFLD,KEY)
         ELSE
            NFLD = 1
         ENDIF
      ELSE IF (MODE .EQ. 5) THEN
         CALL EDITSAM(MODE,KEY)
      ELSE IF (MODE .EQ. 6) THEN
         CALL EDITFLOW(MODE,KEY,51)
      ELSE IF (MODE .EQ. 7) THEN
         CALL EDITFLOW(MODE,KEY,52)
      ENDIF
      ! Catch invalid modes return from edit*-routines.
      ! Set back to last valid mode if necessary.
      if (mode >= 1 .and. mode <= 7) then
          lastmode = mode ! is a good mode
      else
          mode = lastmode ! return to last known good mode
      end if
      GOTO 10
   end if
   
1234 continue

!  finalize before exit
   call partition_finalize()


   end program unstruc
