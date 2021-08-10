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
!  $Id: write_hyd.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_lgpl/io_hyd/packages/io_hyd/src/write_hyd.f90 $

      subroutine write_hyd(hyd, version_full)

      ! function : write a hydrodynamic description file

      ! global declarations

      use hydmod
      use system_utils
      implicit none

      ! declaration of the arguments

      type(t_hyd)         :: hyd                    ! description of the hydrodynamics

      ! local declarations

      integer, parameter  :: nokey   = 77           ! number of keywords in hyd file
      character(len=40)   :: key(nokey)             ! keywords in the hyd file
      integer             :: ikey                   ! index keyword (first level)
      integer             :: ikey2                  ! index keyword (second level)
      integer             :: lunhyd                 ! unit number hyd file
      integer             :: lunrep                 ! unit number report file
      integer             :: ilay                   ! index layers
      integer             :: iwast                  ! index wasteloads
      character(len=40)   :: wtype                  ! wasteload type
      integer             :: n_domain               ! number of domains
      integer             :: i_domain               ! index in collection
      integer                   :: n_dd_bound             ! number of dd-boundaries
      integer                   :: i_dd_bound             ! index in collection
      type(t_dd_bound),pointer  :: dd_bound               ! one dd_bound description
      character(len=256)  :: filename               ! filename without path
      character(*)   version_full      !! Delft3D FLOW version information
      character(20)  rundat            !! Current date and time containing a combination of DATE and TIME
      character(21)  datetime          !! Date/time to be filled in the header

      character,parameter :: cs = ' '               ! space
      character,parameter :: cq = ''''              ! quote
      character(len=2),parameter :: cqs = ''' '     ! quote with space
      character(len=2),parameter :: csq = ' '''     ! space with quote

      key(1)  = 'task'
      key(2)  = 'geometry'
      key(3)  = 'horizontal-aggregation'
      key(4)  = 'minimum-vert-diffusion-used'
      key(5)  = 'vertical-diffusion'
      key(6)  = 'description'
      key(7)  = 'end-description'
      key(8)  = 'reference-time'
      key(9)  = 'hydrodynamic-start-time'
      key(10) = 'hydrodynamic-stop-time'
      key(11) = 'hydrodynamic-timestep'
      key(12) = 'conversion-ref-time'
      key(13) = 'conversion-start-time'
      key(14) = 'conversion-stop-time'
      key(15) = 'conversion-timestep'
      key(16) = 'grid-cells-first-direction'
      key(17) = 'grid-cells-second-direction'
      key(18) = 'number-hydrodynamic-layers'
      key(19) = 'number-water-quality-layers'
      key(20) = 'hydrodynamic-file'
      key(21) = 'aggregation-file'
      key(22) = 'grid-indices-file'
      key(23) = 'grid-coordinates-file'
      key(24) = 'volumes-file'
      key(25) = 'areas-file'
      key(26) = 'flows-file'
      key(27) = 'pointers-file'
      key(28) = 'lengths-file'
      key(29) = 'salinity-file'
      key(30) = 'temperature-file'
      key(31) = 'vert-diffusion-file'
      key(32) = 'surfaces-file'
      key(33) = 'total-grid-file'
      key(34) = 'discharges-file'
      key(35) = 'chezy-coefficients-file'
      key(36) = 'shear-stresses-file'
      key(37) = 'walking-discharges-file'
      key(38) = 'minimum-vert-diffusion'
      key(39) = 'upper-layer'
      key(40) = 'lower-layer'
      key(41) = 'interface-depth'
      key(42) = 'end-minimum-vert-diffusion'
      key(43) = 'constant-dispersion'
      key(44) = 'first-direction'
      key(45) = 'second-direction'
      key(46) = 'third-direction'
      key(47) = 'end-constant-dispersion'
      key(48) = 'hydrodynamic-layers'
      key(49) = 'end-hydrodynamic-layers'
      key(50) = 'water-quality-layers'
      key(51) = 'end-water-quality-layers'
      key(52) = 'discharges'
      key(53) = 'end-discharges'
      key(54) = 'domains'
      key(55) = 'end-domains'
      key(56) = 'dd-boundaries'
      key(57) = 'end-dd-boundaries'
      key(58) = 'normal'
      key(59) = 'inlet'
      key(60) = 'outlet'
      key(61) = 'full-coupling'
      key(62) = 'coupling-per-domain'
      key(63) = 'attributes-file'
      key(64) = 'depths-file'
      key(65) = 'curvilinear-grid'
      key(66) = 'yes'
      key(67) = 'no'
      key(68) = 'calculated'
      key(69) = 'unstructured'
      key(70) = 'number-horizontal-exchanges'
      key(71) = 'number-vertical-exchanges'
      key(72) = 'number-water-quality-segments-per-layer'
      key(73) = 'horizontal-surfaces-file'
      key(74) = 'boundaries-file'
      key(75) = 'waqgeom-file'
      key(76) = 'automatic'
      key(77) = 'walking'

      call getmlu(lunrep)

      call dlwqfile_open(hyd%file_hyd)
      lunhyd = hyd%file_hyd%unit_nr

      write(lunhyd,'(A,A)') 'file-created-by  '//trim(version_full)

      call dattim(rundat)
      datetime = rundat(1:4)//'-'//rundat(6:7)//'-'//rundat(9:10)//','//rundat(11:19)
      write(lunhyd,'(A,A)') 'file-creation-date  '//datetime

      write(lunhyd,'(a,'' '',a)') key(1), key(61)
      if ( hyd%geometry .eq. HYD_GEOM_CURVI ) then
         write(lunhyd,'(a,'' '',a)') key(2), key(65)
      elseif ( hyd%geometry .eq. HYD_GEOM_UNSTRUC ) then
         write(lunhyd,'(a,'' '',a)') key(2), key(69)
      else
         write(lunhyd,'(a,'' '',a)') key(2), 'unknown'
      endif
      write(lunhyd,'(a,'' '',a)') key(3), key(76)
      write(lunhyd,'(a,'' '',a)') key(4), key(67)
      write(lunhyd,'(a,'' '',a)') key(5), key(68)
      write(lunhyd,'(a)')         trim(key(6))
      write(lunhyd,'(''   '''''',a,'''''''')') trim(hyd%description(1))
      write(lunhyd,'(''   '''''',a,'''''''')') trim(hyd%description(2))
      write(lunhyd,'(''   '''''',a,'''''''')') trim(hyd%description(3))
      write(lunhyd,'(a)')         trim(key(7))
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(8) , hyd%hyd_ref
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(9) , hyd%hyd_start
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(10), hyd%hyd_stop
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(11), hyd%hyd_step
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(12), hyd%cnv_ref
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(13), hyd%cnv_start
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(14), hyd%cnv_stop
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(15), hyd%cnv_step
      write(lunhyd,'(a,'' '',i10)') key(16), hyd%mmax
      write(lunhyd,'(a,'' '',i10)') key(17), hyd%nmax
      write(lunhyd,'(a,'' '',i10)') key(18), hyd%kmax
      write(lunhyd,'(a,'' '',i10)') key(70), hyd%noq1 + hyd%noq2
      write(lunhyd,'(a,'' '',i10)') key(71), hyd%noq3
      write(lunhyd,'(a,'' '',i10)') key(72), hyd%nosegl
      write(lunhyd,'(a,'' '',i10)') key(19), hyd%nolay
      call remove_path(hyd%file_com%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(20), trim(filename)
      call remove_path(hyd%file_dwq%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(21), trim(filename)
      if (hyd%geometry .eq. HYD_GEOM_CURVI) then
         call remove_path(hyd%file_lga%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(22), trim(filename)
         call remove_path(hyd%file_cco%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(23), trim(filename)
      else if (hyd%geometry .eq. HYD_GEOM_UNSTRUC) then
         call remove_path(hyd%file_bnd%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(74), trim(filename)
         call remove_path(hyd%file_geo%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(75), trim(filename)
         call remove_path(hyd%file_bnd%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(22), trim(filename)
         call remove_path(hyd%file_geo%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(23), trim(filename)
      endif
      call remove_path(hyd%file_vol%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(24), trim(filename)
      call remove_path(hyd%file_are%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(25), trim(filename)
      call remove_path(hyd%file_flo%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(26), trim(filename)
      call remove_path(hyd%file_poi%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(27), trim(filename)
      call remove_path(hyd%file_len%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(28), trim(filename)
      if ( hyd%sal_present ) then
         call remove_path(hyd%file_sal%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(29), trim(filename)
      else
         write(lunhyd,'(a,''     '',a)') key(29), 'none'
      endif
      if ( hyd%tem_present ) then
         call remove_path(hyd%file_tem%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(30), trim(filename)
      else
         write(lunhyd,'(a,''     '',a)') key(30), 'none'
      endif
      if ( hyd%vdf_present ) then
         call remove_path(hyd%file_vdf%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(31), trim(filename)
      else
         write(lunhyd,'(a,''     '',a)') key(31), 'none'
      endif
      if (hyd%geometry .eq. HYD_GEOM_CURVI) then
         call remove_path(hyd%file_srf%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(32), trim(filename)
      else if (hyd%geometry .eq. HYD_GEOM_UNSTRUC) then
         call remove_path(hyd%file_hsrf%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(73), trim(filename)
         ! temporary for old user interface
         call remove_path(hyd%file_srf%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(32), trim(filename)
      endif
      if (hyd%geometry .eq. HYD_GEOM_CURVI) then
         call remove_path(hyd%file_lgt%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(33), trim(filename)
         call remove_path(hyd%file_src%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(34), trim(filename)
         call remove_path(hyd%file_chz%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(35), trim(filename)
      endif
      if ( hyd%tau_present ) then
         call remove_path(hyd%file_tau%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(36), trim(filename)
      else
         write(lunhyd,'(a,''     '',a)') key(36), 'none'
      endif
      if (hyd%geometry .eq. HYD_GEOM_CURVI) then
         call remove_path(hyd%file_wlk%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(37), trim(filename)
      endif
      call remove_path(hyd%file_atr%name,filename)
      if ( filename .eq. ' ' ) then
         write(lunhyd,'(a,''     '',a)') key(63), 'none'
      else
         call remove_path(hyd%file_atr%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(63), trim(filename)
      endif
      if (hyd%geometry .eq. HYD_GEOM_CURVI) then
         call remove_path(hyd%file_dps%name,filename) ; write(lunhyd,'(a,'' '''''',a,'''''''')') key(64), trim(filename)
      endif

      ! hydrodynamic-layers

      write(lunhyd,'(a)') key(48)
      do ilay = 1 , hyd%kmax
         write(lunhyd,'(''      '',F15.8)') hyd%hyd_layers(ilay)
      enddo
      write(lunhyd,'(a)') key(49)

      ! water-quality-layers

      write(lunhyd,'(a)') key(50)
      do ilay = 1 , hyd%nolay
         write(lunhyd,'(''      '',i6)') nint(hyd%waq_layers(ilay))
      enddo
      write(lunhyd,'(a)') key(51)

      ! discharges

      if(hyd%wasteload_coll%cursize .gt. 0) then
      write(lunhyd,'(a)') key(52)
         do iwast = 1 , hyd%wasteload_coll%cursize
            if ( hyd%wasteload_coll%wasteload_pnts(iwast)%type .eq. DLWQ_WASTE_NORMAL ) then
               wtype = key(58)
            elseif ( hyd%wasteload_coll%wasteload_pnts(iwast)%type .eq. DLWQ_WASTE_INLET ) then
               wtype = key(59)
            elseif ( hyd%wasteload_coll%wasteload_pnts(iwast)%type .eq. DLWQ_WASTE_OUTLET ) then
               wtype = key(60)
            elseif ( hyd%wasteload_coll%wasteload_pnts(iwast)%type .eq. DLWQ_WASTE_WALK ) then
               wtype = key(77)
            endif

            write(lunhyd,'(3(i6,1x),'''''''',a,'''''' '',a)') hyd%wasteload_coll%wasteload_pnts(iwast)%n, &
                                                              hyd%wasteload_coll%wasteload_pnts(iwast)%m, &
                                                              hyd%wasteload_coll%wasteload_pnts(iwast)%k, &
                                                              trim(hyd%wasteload_coll%wasteload_pnts(iwast)%name), &
                                                              trim(wtype)
         enddo
         write(lunhyd,'(a)') key(53)
      endif

      ! domains

      n_domain = hyd%domain_coll%cursize
      if ( n_domain .gt. 0 .and. hyd%geometry .eq. HYD_GEOM_CURVI) then
         write(lunhyd,'(a)') key(54)
         do i_domain = 1 , n_domain
            write(lunhyd,'(3a,i8,a,i8,3a)') &
                                cq,trim(hyd%domain_coll%domain_pnts(i_domain)%name),cqs, &
                                        hyd%domain_coll%domain_pnts(i_domain)%mmax ,cs , &
                                        hyd%domain_coll%domain_pnts(i_domain)%nmax ,     &
                               csq,trim(hyd%domain_coll%domain_pnts(i_domain)%aggr) ,cq
         enddo
         write(lunhyd,'(a)') key(55)
      endif

      ! dd-boundaries

      n_dd_bound = hyd%dd_bound_coll%cursize
      if ( n_dd_bound .gt. 0 ) then
         write(lunhyd,'(a)') key(56)
         do i_dd_bound = 1 , n_dd_bound
            dd_bound => hyd%dd_bound_coll%dd_bound_pnts(i_dd_bound)
            write(lunhyd,'(3a,4(i8,1x),3a,4(i8,1x))') &
                                cq,trim(dd_bound%name1),cqs, &
                                        dd_bound%m_begin1  , &
                                        dd_bound%n_begin1  , &
                                        dd_bound%m_end1    , &
                                        dd_bound%n_end1    , &
                                cq,trim(dd_bound%name2),cqs, &
                                        dd_bound%m_begin2  , &
                                        dd_bound%n_begin2  , &
                                        dd_bound%m_end2    , &
                                        dd_bound%n_end2
         enddo
         write(lunhyd,'(a)') key(57)
      endif

      call dlwqfile_close(hyd%file_hyd)

      return
      end subroutine write_hyd
