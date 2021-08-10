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

! $Id: gui.F90 65778 2020-01-14 14:07:42Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/gui.F90 $
! Copyright notice:
! Several of the graphical user interface routines below make use of the INTERACTER libraries
! (only when run on Windows platforms with display mode on).
! Copyright on the INTERACTER libraries resides with Interactive Software Services Ltd.
! More information: http://www.winteracter.com/iss

!----------------------------------------------------------------------
! subroutines from net.F90
!----------------------------------------------------------------------

    
    
   SUBROUTINE CHOICES(MODE,NUM,NWHAT,KEY)
   use m_netw
   use m_samples
   use m_grid
   USE M_MISSING
   use unstruc_display
   use m_polygon
   use m_partitioninfo
   use m_ec_interpolationsettings
   use gridoperations
   use m_oned_functions, only: convert_cross_to_prof
   use unstruc_model, only: md_ident

   implicit none
   integer :: ja, L, n12, ikey, mnx
   integer :: ndraw
   integer :: MODE,NUM,NWHAT,KEY,nwhat2
   integer :: JDEMO
   integer :: irerun          ! orthogonalisenet: rerun

   COMMON /DRAWTHIS/  ndraw(50)
   COMMON /DEMO/ JDEMO
   integer :: maxexp
   integer :: maxopt, ierr
   integer, parameter :: MAXOP = 64
   CHARACTER*40 OPTION(MAXOP),EXP(MAXOP)
   integer, external :: flow_modelinit

   if ( netstat.ne.NETSTAT_OK ) call setnodadm(0)

   IF (NUM .EQ. 1) THEN
   !     load en save files
      CALL NFILES(MODE, NUM,  NWHAT, KEY)
   ELSE IF (NUM .EQ. 2) THEN
   !     operations
!      if ( jins.ne.1 ) then  ! SPvdP: temporarily disabled
!         jins = 1
!         netstat = NETSTAT_CELLS_DIRTY
!      end if
      IF (NWHAT .EQ. 1) THEN
         CALL RESTORE()
      ELSE IF (NWHAT .EQ. 2) THEN
         CALL SAVENET()
         CALL MAKENET(1)
         CALL MINMXNS()
      ELSE IF (NWHAT .EQ. 3) THEN
         CALL curvilinearGRIDfromsplines()
      ELSE IF (NWHAT .EQ. 4) THEN
         CALL curvilinearGRIDinpolygon()
      ELSE IF (NWHAT .EQ. 5) THEN
         call CREATESAMPLESINPOLYGON()
      ELSE IF (NWHAT .EQ. 6) THEN
         CALL SAVENET()
         CALL Triangulatesamplestonetwork(1)
         netstat = NETSTAT_CELLS_DIRTY
      ELSE IF (NWHAT .EQ. 7) THEN
         CALL SAVENET()
         call gridtonet()
         call delgrd(key,0,0) ! no save, no delpol
      ELSE IF (NWHAT .EQ. 8) THEN
         CALL SAVENET()
         irerun = 1
         do while ( irerun.ne.0 )
            call ORTHOGONALISENET(irerun)
         end do
      ELSE IF (NWHAT .EQ. 9) THEN
      ELSE IF (NWHAT .EQ. 10) THEN
         ! call csmfinebnds2unstruc()
         call REFINEPOLYGON ()
      ELSE IF (NWHAT .EQ. 11) THEN
         CALL SAVENET()
         CALL REFINEQUADS()
      ELSE IF (NWHAT .EQ. 12) THEN
         ! CALL quadsTOTRI()
         CALL SAVENET()
         CALL REFINEQUADS_casulli()
      ELSE IF (NWHAT .EQ. 13) THEN
!         CALL RELINK()
!         CALL SAVENET()
!         CALL REFINECELLSANDFACES() !  REFINECELLSONLY()
         CALL SAVENET()
         CALL REFINECELLSANDFACES2() !  REFINECELLSONLY()
      ELSE IF (NWHAT .EQ. 14) THEN
         CALL SAVENET()
         call derefine_mesh(0d0, 0d0, .false.)
      ELSE IF (NWHAT .EQ. 15) THEN
         CALL SAVENET()
         CALL connectcurvilinearquadsddtype()
      ELSE IF (NWHAT .EQ. 16) THEN
         CALL SAVENET()
         CALL TIELDB()
         ! CALL CUTCELLS(1)
      ELSE IF (NWHAT .EQ. 17) THEN
         CALL COPYTRANS()
      ELSE IF (NWHAT .EQ. 18) THEN
         CALL SAVENET()
         call EXTERNALTRIANGLESTOOUTERQUADS()
      ELSE IF (NWHAT .EQ. 19) THEN
      ELSE IF (NWHAT .EQ. 20) THEN
         jareinitialize = 1
         ierr = flow_modelinit()
      ELSE IF (NWHAT .EQ. 21) THEN  ! Refresh net adm. (setnodadm + findcells)
         call findcells(100)        ! include folded cells
         call find1dcells()
!         call findcells(0)          ! do not include folded cells
         call delete_dry_points_and_areas()
         call makenetnodescoding()  ! killcell relies on node codes
      ELSE IF (NWHAT .EQ. 22) THEN
         call interpdivers(2) ! Network zk flow bathy
      ELSE IF (NWHAT .EQ. 23) THEN
         call interpdivers(interpolate_to) ! interpolate to interpolate_to in samples
         if (interpolate_to == 5) then ! plotlin?
            ndraw(36) = 1
         else if (interpolate_to == 1) then
            call setbobs()
         endif
         call setbobs()
      ELSE IF (NWHAT .EQ. 24) THEN
         call make1D2Dconnections()
      ELSE IF (NWHAT .EQ. 25) THEN

         !call flow_initfloodfill()
      ELSE IF (NWHAT .EQ. 26) THEN

      ELSE IF (NWHAT .EQ. 27) THEN
         call flow_spatietimestep()
      ELSE IF (NWHAT .EQ. 28) THEN
         CALL SAVENET()
         CALL MAKECOARSE2FINETRIANGLECONNECTIONCELLS()
      ELSE IF (NWHAT .EQ. 29) THEN
         CALL SAVENET()
           CALL renumberNodes()
!           call removewallfromsamples()  ! obsolete
         ! CALL REFINELINES()
      ELSE IF (NWHAT .EQ. 30) THEN
         CALL SAVENET()
         call fliplinks()
      ELSE IF (NWHAT .EQ. 31) THEN
         CALL SAVENET()
         call coarsen_mesh()
      ELSE IF (NWHAT .EQ. 32) THEN
         call savegrd()
!        delete grid
         mc = 0
         nc = 0
         ikey = 3
         call drawnu(ikey)
         call spline2curvi()
      ELSE IF (NWHAT .EQ. 33) THEN
         CALL SAVENET()
         call triangulate_quadsandmore(ja)
      ELSE IF (NWHAT .EQ. 34 ) THEN
         call detect_ridges(1)
      ELSE IF (NWHAT .EQ. 35 ) THEN
!
      ELSE IF (NWHAT .EQ. 36) THEN
!        intentionally left empty
      ELSE IF (NWHAT .EQ. 37 ) THEN
         call partition_to_idomain()
      ELSE IF (NWHAT .EQ. 38 ) THEN
         call make_dual_mesh()
      ELSE IF (NWHAT .EQ. 39) THEN
         call samdif()
      ELSE IF (NWHAT .EQ. 40) THEN
         call smooth_samples_from_GUI()
      ELSE IF (NWHAT .EQ. 41) THEN
         call maketrigrid()
      ENDIF
      KEY = 3
      NUM = 0
      CALL IMOUSECURSORSHAPE(1,'G')
      CALL IMouseCursorShow()
   ELSE IF (NUM .EQ. 3) THEN
   !     display opties
      CALL NDISPLAY(NWHAT,KEY)
      NUM = 0
   ELSE IF (NUM .EQ. 4) THEN
   !     dit zijn de edit nummers
   ELSE IF (NUM .EQ. 5) THEN
   !     addsubdel

      IF (NWHAT .EQ. 1) THEN
         CALL DELPOL()
      !  edit/modify polygon: netcell administration out of date
         netstat = NETSTAT_CELLS_DIRTY
      ELSE IF (NWHAT .EQ. 2) THEN
         CALL DELNET(KEY,0, 1)
      ELSE IF (NWHAT .EQ. 3) THEN
         CALL DELNET(KEY,2, 1)
      ELSE IF (NWHAT .EQ. 4) THEN
         CALL deleteSelectedSplines()
      ELSE IF (NWHAT .EQ. 5) THEN
         call delsam(1)
      ELSE IF (NWHAT .EQ. 6) THEN
         CALL ZEROLAN( KEY)
      ELSE IF (NWHAT .EQ. 7) THEN
         CALL DELgrd(key,1,0)
      ELSE IF (NWHAT .EQ. 8) THEN
         CALL deleteSelectedObservations()
      ELSE IF (NWHAT .EQ. 9) THEN
         CALL REMOVESMALLLINKS()
      ELSE IF (NWHAT .EQ.10) THEN
         CALL MERGENODESINPOLYGON()
      !  netcell administration out of date
         netstat = NETSTAT_CELLS_DIRTY
      ELSE IF (NWHAT .EQ. 12) THEN
         CALL zerowaterdepth()
      ELSE IF (NWHAT .EQ. 13) THEN
         call plusabs_flow(1)
      ELSE IF (NWHAT .EQ. 14) THEN              !****     **
         call plusabs_flow(2)
      ELSE IF (NWHAT .EQ. 15) THEN              !****     **
         ! call plusabs_flow(3)
         mnx  = mmax*nmax
         call PLUSABSD(XC,YC,ZC,mnx,KEY,zc)
      ELSE IF (NWHAT .EQ. 16) THEN              !****     **
         CALL SAVENET()
         CALL PLUSABSD(XK,YK,ZK,NUMK,KEY,XK)
      ELSE IF (NWHAT .EQ.17) THEN              !****     **
         CALL SAVENET()
         CALL PLUSABSD(XK,YK,ZK,NUMK,KEY,YK)
      ELSE IF (NWHAT .EQ.18) THEN              !****     **
         CALL SAVENET()
         CALL PLUSABSD(XK,YK,ZK,NUMK,KEY,ZK)
      ELSE IF (NWHAT .EQ.19) THEN              !****     **
         CALL PLUSABSD(Xs,Ys,Zs,NS,KEY,Zs)
      ELSE IF (NWHAT .EQ.20) THEN              !****     **
         CALL PLUSABSD(Xpl,Ypl,Zpl,NPL,KEY,Zpl)
      ELSE IF (NWHAT .EQ.21) THEN              !****     **
         CALL PLUSABSI(XK,YK,ZK,KN,NUMK,NUML,KEY,kn3typ)
      ELSE IF (NWHAT .EQ.23) THEN
        EXP(1)    = 'MENU                                    '
        EXP(2)    = 'COPY ... TO POLYGON                     '
        OPTION(1) = 'Copy land boundary  to polygon          '
        OPTION(2) = 'Copy net bounds     to polygon          '
        OPTION(3) = 'Copy cross sections to polygon          '
        OPTION(4) = 'Copy thin dams      to polygon          '
        OPTION(5) = 'Copy fixed weirs     to polygon         '
        OPTION(6) = 'Copy splines        to polygon (fine)   '
        OPTION(7) = 'Copy splines        to polygon          '
        OPTION(8) = 'Copy curvigrid bnds to polygon          '
        OPTION(9) = 'Copy 1D netw        to polygon          '
        OPTION(10)= 'Copy whole netw     to polygon          '
        OPTION(11)= 'Copy samples        to polygon          '

        MAXOPT    = 11
        NWHAT2    = 0
        CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
        if (nwhat2 == 1) then
            CALL COPYLDBTOPOL()
        else if (nwhat2 == 2) then
            call copynetboundstopol(0, 1, 0, 1)
        else if (nwhat2 == 3) then
            CALL copycrosssectionstopol()
        else if (nwhat2 == 4) then
            CALL copythindamstopol()
        else if (nwhat2 == 5) then
           CALL copyfixedweirstopol()
        else if (nwhat2 == 6) then
           CALL copysplinestofinepol(11)
        else if (nwhat2 == 7) then
           CALL copysplinestofinepol(1)
        else if (nwhat2 == 8) then
           CALL copycurvigridboundstopol()
        else if (nwhat2 == 9) then
           CALL regrid1D(0) ! 1D netw to pol
        else if (nwhat2 == 10) then
           CALL copynetwtopol()  
        else if (nwhat2 == 11) then
           CALL copysamtopol()  
        end if
        KEY = 3
      ELSE IF (NWHAT .EQ.24) THEN
        EXP(1)    = 'MENU                                    '
        EXP(2)    = 'COPY POLYGON TO ...                     '
        OPTION(1) = 'Copy polygon to land boundary           '
        OPTION(2) = 'Copy polygon to observation points      '
        OPTION(3) = 'Copy polygon to samples                 '
        OPTION(4) = 'Copy polygon to spline                  '
        MAXOPT    = 4
        NWHAT2    = 0
        CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
        if (nwhat2 == 1) then
            CALL COPYPOLTOLDB()
        else if (nwhat2 == 2) then
            call copyPolygonToObservations()
        else if (nwhat2 == 3) then
            CALL copyPolygonToSamples()
        else if (nwhat2 == 4) then
            CALL copyPolToSpline()
        end if
        KEY = 3
      ELSE IF (NWHAT .EQ.25) THEN
        EXP(1)    = 'MENU                                    '
        EXP(2)    = 'COPY ... TO SAMPLES                     '
        OPTION(1) = 'Copy polygon              to samples    '
        OPTION(2) = 'Copy values on network nodes to samples '
        OPTION(3) = 'Copy values on network links to samples '
        OPTION(4) = 'Copy values on network cells to samples '                
        OPTION(5) = 'Copy values on flow nodes to samples    '
        OPTION(6) = 'Copy values on flow links to samples    '
        OPTION(7) = 'Swap samples and second samples         '
        OPTION(8) = 'Copy curvilinear grid     to samples    '
        OPTION(9) = 'Copy samples              to particles  '
        OPTION(10) = 'Copy dots                 to samples    '
        OPTION(11) = 'Copy samples              to dots       '
        MAXOPT    = 10
        NWHAT2    = 0
        CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
        if (nwhat2 == 1) then
         call copypolygontosamples()
        else if (nwhat2 == 2) then
         call copynetnodestosam()
        else if (nwhat2 == 3) then
         call copynetlinkstosam()
        else if (nwhat2 == 4) then 
        ! call copyflowcellsizetosamples ! copyzktosam()
         call copycellstosam() !subroutine to display the scalar values calculated in the cells  
        else if (nwhat2 == 5) then
         CALL copywaterlevelstosamples()
        else if (nwhat2 == 6) then
         CALL copyzlintosamples()
        else if (nwhat2 == 7) then
         call swapsamples()
        else if (nwhat2 == 8) then
         call copygridtosam()
        else if (nwhat2 == 9) then
         call copy_sam2part()
        else if (nwhat2 == 10 ) then
         call copy_dots2sam()
        else if (nwhat2 == 11 ) then
         call copy_sam2dots()
        end if
        KEY = 3
      ELSE IF (NWHAT .EQ.26) THEN
         CALL copylandboundaryto1dnetwork()
      ELSE IF (NWHAT .EQ.27) THEN
         CALL copynetwtonetw()
      ELSE IF (NWHAT .EQ.28) THEN
         n12 = 1
         call cutcell_list(n12, '*.POL',5, 0)
      ELSE IF (NWHAT .EQ.29) THEN
         n12 = 3
         call findcells(0)
         call cutcell_list(n12, '*.cut',5, 0)
      ELSE IF (NWHAT .EQ.30) THEN
!        intentionally left empty
      ELSE IF (NWHAT .EQ.31) THEN
         call merge_polylines()
      ELSE IF (NWHAT .EQ.32) THEN
         call delnetzkabovezkuni()
      ELSE iF (NWHAT .EQ.33) THEN
         call del_badortholinks()
      ELSE iF (NWHAT .EQ.34) THEN   
         call shift1Dnetnodestoduikers()
      ELSE iF (NWHAT .EQ.35) THEN   
         call convert_cross_to_prof(md_ident)
      ENDIF
      NUM  = 0
      KEY  = 3
      if ( jins.ne.1 ) then
         JINS    = 1                           !IMMEADIATELY SET BACK TO NORMAL BEHAVIOUR OR GO BESERK
         netstat = NETSTAT_CELLS_DIRTY
      end if
   ELSE IF (NUM .EQ. 6) THEN
   !     various
      IF (NWHAT .EQ. 1) THEN
         CALL STOPINT()
      ELSE IF (NWHAT .EQ. 2) THEN
         CALL SCHERM()
      ELSE IF (NWHAT .EQ. 3) THEN
         CALL CHANGEnetworkparameters()
      ELSE IF (NWHAT .EQ. 4) THEN
         CALL CHANGEorthoparameters()
      ELSE IF (NWHAT .EQ. 5) THEN
         CALL CHANGEGRIDPARAMETERS()
      ELSE IF (NWHAT .EQ. 6) THEN
         CALL CHANGEINTERPOLATIONPARAMETERS
      ELSE IF (NWHAT .EQ. 7) THEN
         CALL MAPPROJECTIONS(-1,JA) ! -1, INTERACTIEF
         if (ja == 1) then
            call minmxns()
            key = 3
         endif
      ELSE IF  (NWHAT .EQ. 8) THEN
         CALL CHANGETIMEPARAMETERS()
      ELSE IF (NWHAT .EQ. 9) THEN
         Call changegeometryparameters()
      ELSE IF (NWHAT .EQ. 10) THEN
         CALL CHANGEPHYSICALPARAMETERS()
      ELSE IF (NWHAT .EQ. 11) THEN
         CALL CHANGENUMERICALPARAMETERS()
      ELSE IF (NWHAT .EQ. 12) THEN
         CALL CHANGENUMERICALPARAMETERS2()
      ELSE IF (NWHAT .EQ. 13) THEN
         CALL CHANGENUMERICALPARAMETERS3()
      ELSE IF (NWHAT .EQ. 14) THEN
         CALL CHANGENUMERICALPARAMETERS4()
      ELSE IF (NWHAT .EQ. 15) THEN
         CALL CHANGEcolournumbers() ; KEY = 3
      ENDIF
      NUM = 0
   ENDIF

   RETURN
   END SUBROUTINE CHOICES

   SUBROUTINE MENUV1(NUM,NWHAT)
   use m_netw
   implicit none
   integer :: NUM, NWHAT
   integer :: maxexp
   integer :: maxopt
   integer, parameter :: MAXOP = 64
   CHARACTER*40 OPTION(MAXOP),EXP(MAXOP)
   integer :: MODE,NFLD, NFO
   integer :: jdemo
   COMMON /MODENOW/ MODE,NFLD
   COMMON /DEMO/ JDEMO
   
   logical, external :: get_japart

   IF (NUM .EQ. 1) THEN
      EXP(1)    = 'MENU 1                                  '
      EXP(2)    = 'FILES                                   '
      OPTION(1 )= 'Load MDU-file                    (*.mdu)'
      OPTION(2 )= 'Load network      (*.unt/*.net/*_net.nc)'
      OPTION(3 )= ' Add network      (*.unt/*.net/*_net.nc)'
      OPTION(4 )= 'Load curvilinear grid            (*.grd)'
      OPTION(5 )= 'Load arc-info grid               (*.aht)'
!!    OPTION(20)= 'LOAD Untrim grd file,            (*.unt)'
      OPTION(6 )= 'Load polygon                     (*.pol)'
      OPTION(7 )= 'Load splines                     (*.spl)'
      OPTION(8 )= 'Load land boundary               (*.ldb)'
      OPTION(9 )= 'Load observation points      (*_obs.xyn)'
      OPTION(10)= ' Add observation points      (*_obs.xyn)'
      OPTION(11)= 'Load cross sections          (*_crs.pli)'
      OPTION(12)= ' add cross sections          (*_crs.pli)'
      OPTION(13)= 'Load thin dams               (*_thd.pli)'
      OPTION(14)= ' add thin dams               (*_thd.pli)'
      OPTION(15)= 'Load samples         (*.xyz/*.dem/*.asc)'
      OPTION(16)= 'Load flow bathymetry (*.xybl or *.xyblu)'
      OPTION(17)= 'Load flow restart             (*_rst.nc)'
      OPTION(18)= 'Load bitmap                      (*.bmp)'
      OPTION(19)= '-                                       '
      OPTION(20)= 'Save MDU-file                    (*.mdu)'
      OPTION(21)= 'Save network                  (*_net.nc)'
      OPTION(22)= 'Save network with cell info   (*_net.nc)'
      OPTION(23)= 'Save network for Google Earth    (*.kml)'
#ifdef HAVE_TECPLOT
      OPTION(24)= 'Save network for Tecplot         (*.plt)'
#else
      OPTION(24)= 'Not available                           '
#endif
      OPTION(25)= 'Save curvilinear grid            (*.grd)'
      OPTION(26)= 'Save polygon                     (*.pol)'
      OPTION(27)= 'Save splines                     (*.spl)'
      OPTION(28)= 'Save land boundary               (*.ldb)'
      OPTION(29)= 'Save observation points      (*_obs.xyn)'
      OPTION(30)= 'Save cross sections          (*_crs.pli)'
      OPTION(31)= 'save samples                     (*.xyz)'
      OPTION(32)= 'save flow bathymetry (*.xybl or *.xyblu)'
      OPTION(33)= 'Save snapshot for restart     (*_rst.nc)'
      OPTION(34)= 'Save snapshot net+s1+u1       (*_map.nc)'
      OPTION(35)= 'TMP read manually preprocessed SVG      '
      OPTION(36)= 'Save SWAN files       (*.node and *.ele)'
      OPTION(37)= 'Save partition files     (*_NNNN_net.nc)'
      OPTION(38)= 'Stop program                            '
      MAXOPT    =  38
   ELSE IF (NUM .EQ. 2) THEN
      EXP(1)    = 'MENU 2                                  '
      EXP(2)    = 'OPERATIONS                              '
      OPTION(1) = 'Undo net                                '
      OPTION(2) = 'Create uniform curvilinear grid         '
      OPTION(3) = 'Create curvilinear grid from splines    '
      OPTION(4) = 'Create curvilinear grid in   polygon    '
      OPTION(5) = 'Create samples in polygon               '
      OPTION(6) = 'Triangulate samples to net in polygon   '
      OPTION(7) = 'Convert grid to net                     '
      OPTION(8) = 'Orthogonalise / Smooth net              '
      OPTION(9) = '-                                       '
      OPTION(10)= 'Refine Polygon                          '
      OPTION(11)= 'Refine quads factor 2 (triangle border) '
      OPTION(12)= 'Refine cells factor 2 (Casulli-type)    '
      OPTION(13)= 'Refine cells and faces factor 2         '
      OPTION(14)= 'Derefine quads factor 2 (Casulli-type)  '
      OPTION(15)= 'Connect curvilinear quads dd type       '
      OPTION(16)= 'Tie Landboundary in network             '
      !OPTION(16)= 'Connect hanging nodes                   '
      OPTION(17)= 'Copy and translate/rotate net           '
      OPTION(18)= 'External triangles to outer quads       '
      OPTION(19)= '-                                       '
      OPTION(20)= '(Re) initialise flow model geometry     '
      OPTION(21)= 'Refresh net adm. (setnodadm + findcells)'
      OPTION(22)= 'Interpolate Network ZK-values in samples'
      OPTION(23)= 'Interpolate Other, see Various/Int. Par.'
      OPTION(24)= 'Make1D2Dinternalnetlinks                '
      OPTION(25)= '                                        '
      ! OPTION(25)= 'Flood fill waterlevels S1 from samples  '
      OPTION(26)= '                                        '
      OPTION(27)= 'Do 1 FLOW step                          '
      OPTION(28)= 'MAKECOARSE2FINETRIANGLECONNECTIONCELLS  '
      OPTION(29)= 'Renumber nodes                          '
      OPTION(30)= 'Flip links                              '
      OPTION(31)= 'Coarsen mesh                            '
      OPTION(32)= 'Grow curvilinear grid from splines      '
      OPTION(33)= 'Make triangles from quads, pentas, hexas'
      OPTION(34)= 'Detect ridges in structured sample set  '
      OPTION(35)= '                                        '
      OPTION(36)= '                                        '
      OPTION(37)= 'Gen. domain numbers (polygons or METIS) '
      OPTION(38)= 'Generate dual mesh                      '
      OPTION(39)= 'Diff. samples w. 2nd samples (<tooclose)'
      OPTION(40)= 'Smooth. samples                         '
      OPTION(41) ='curv. grid to structured triangular grid'

      MAXOPT    = 41
   ELSE IF (NUM .EQ. 3) THEN
      EXP(1)    = 'MENU 3                                  '
      EXP(2)    = 'DISPLAY                                 '
      OPTION(1) = 'Display presets...                      '
      OPTION(2) = 'Display network                         '
      OPTION(3) = 'Display previous state network          '
      OPTION(4) = 'Display splines                         '
      OPTION(5) = 'Display land boundary                '
      OPTION(6) = 'Display mode net/flow nodes             '
      OPTION(7) = 'Display mode net/flow links             '
      OPTION(8) = 'Values at net  nodes                    '
      OPTION(9) = 'Values at net  links                    '
      OPTION(10)= 'Values at flow nodes                    '
      OPTION(11)= 'Values at flow links                    '
      OPTION(12)= 'Values at net cells                     '
      OPTION(13)= 'Values at cell corners                  '
      OPTION(14)= 'Show all flow links in white            '
      OPTION(15)= 'Display Velocity vectors                '
      OPTION(16)= 'Display Observation points              '
      OPTION(17)= 'Display Cross sections                  '
      OPTION(18)= 'Display Thin Dams                       '
      OPTION(19)= 'Display Fixed Weirs                     '
      OPTION(20)= '-                                       '
      OPTION(21)= 'Isoscale on or off                      '
      OPTION(22)= 'Load colourtable         (in file *.hls)'
      OPTION(23)= 'Change isocolour parameters             '
      OPTION(24)= 'Change display   parameters             '
      OPTION(25)= 'Change text      parameters             '
      OPTION(26)= '-                                       '
      OPTION(27)= 'Zoom in or set zoomwindow of x1y1x2     '
      OPTION(28)= 'Redraw                                  '
      OPTION(29)= 'Hardcopy                                '
      OPTION(30)= 'Show sideview                           '
      OPTION(31)= 'Perspective view                        '
      OPTION(32)= 'Display Samples                         '
      OPTION(33)= 'Show bitmap yes or no                   '
      OPTION(34)= 'Display banf                            '
      OPTION(35)= 'Display Polygon                         '
      OPTION(36)= 'Display Curvilinear grid                '
      OPTION(37)= 'tracers                                 '
      OPTION(38)= 'Display Sources & Sinks                 '
      OPTION(39)= 'Display dots                            '
      OPTION(40)= 'Display structures                      '
      
      MAXOPT    = 40
     
      if ( get_japart() ) then
         MAXOPT = MAXOPT+1
         OPTION(MAXOPT) = 'particles                               '
      end if

   ELSE IF (NUM .EQ. 4) THEN
      EXP(1)    = 'MENU 4                                  '
      EXP(2)    = 'Edit data                               '
      OPTION(1) = 'Edit polygon                            '
      OPTION(2) = 'Edit network                            '
      OPTION(3) = 'Edit splines                            '
      OPTION(4) = 'Edit curvilinear grid                   '
      OPTION(5) = 'Edit samples                            '
      OPTION(6) = 'Show flow nodes                         '
      OPTION(7) = 'Show flow links                         '
      MAXOPT    = 7
   ELSE IF (NUM .EQ. 5) THEN
      EXP(1)    = 'MENU 5                                  '
      EXP(2)    = 'ADDSUBDEL                               '
      OPTION(1) = 'Delete polygon                          '
      OPTION(2) = 'Delete network                          '
      OPTION(3) = 'Delete network based on cell centers.   '
      OPTION(4) = 'Delete splines                          '
      OPTION(5) = 'Delete samples                          '
      OPTION(6) = 'Delete land boundary                 '
      OPTION(7) = 'Delete curvilinear grid                 '
      OPTION(8) = 'Delete observation points               '
      OPTION(9) = 'Remove small flow links from network    '
      OPTION(10)= 'Merge nodes on top of each other        '
      OPTION(11)= '-                                       '
      OPTION(12)= 'Set zero waterdepth                     '
      OPTION(13)= 'Initial waterlevel     +-* uniform value'
      OPTION(14)= 'Initial salinity       +-* uniform value'
      OPTION(15)= 'Curvilinear grid ZC    +-* uniform value'
      OPTION(16)= 'Netw xk coordinates    +-* uniform value'
      OPTION(17)= 'Netw yk coordinates    +-* uniform value'
      OPTION(18)= 'Netw zk coordinates    +-* uniform value'
      OPTION(19)= 'Samples Z value        +-* uniform value'
      OPTION(20)= 'Polygon Z value        +-* uniform value'
      OPTION(21)= 'Netw link codes kn3    +-* uniform value'
      OPTION(22)= '-                                       '
      OPTION(23)= 'Copy ... to polygon                     '
      OPTION(24)= 'Copy polygon to ...                     '
      OPTION(25)= 'Copy ... to samples                     '
      OPTION(26)= 'Copy Land Boundary to 1D network        '
      OPTION(27)= 'Copy network to network (in polgon)     '
      OPTION(28)= 'Shape/Cut network to *.pol files        '
      OPTION(29)= 'Delete network in *.cut files (cutcell) '
      OPTION(30)= '                                        '
      OPTION(31)= 'Merge polylines                         '
      OPTION(32)= 'Delete netnodes with ZK > ZKuni         '
      OPTION(33)= 'Delete netlinks to improve orthogonality'
      OPTION(34)= 'Shift 1D netnodes to duikers.pliz (5col)'
      OPTION(35)= 'Convert crsdef/loc to profdef/loc files '
      MAXOPT    =  35
   ELSE IF (NUM .EQ. 6) THEN
      EXP(1)     = 'MENU 6                                  '
      EXP(2)     = 'VARIOUS                                 '
      OPTION(1 ) = 'Shortstop                               '
      OPTION(2 ) = 'Actual and maximum data dimensions      '
      OPTION(3 ) = 'Change network           parameters     '
      OPTION(4 ) = 'Change orthogonalisation parameters     '
      OPTION(5 ) = 'Change curvilinear grid  parameters     '
      OPTION(6 ) = 'Change interpolation parameters         '
      OPTION(7 ) = 'Coordinate transformation               '
      OPTION(8 ) = 'Change flow time         parameters     '
      OPTION(9 ) = 'Change flow geometry     parameters     '
      OPTION(10) = 'Change flow physical     parameters     '
      OPTION(11) = 'Change flow numerical    parameters     '
      OPTION(12) = 'Change flow numerical    parameters 2   '
      OPTION(13) = 'Change flow numerical    parameters 3   '
      OPTION(14) = 'Change flow numerical    parameters 4   '
      OPTION(15) = 'Change plot colour numbers              '
      MAXOPT     = 15
   ENDIF

    IF (NUM .EQ. 4 .AND. MODE .EQ. 4) THEN ! Edit grid submenu
      NFO = NFLD
      CALL FIELDOPT(NFLD)
      NWHAT = 4
      IF (NFLD .EQ. 22) THEN
         CALL MENUV2(NWHAT,OPTION,MAXOPT,EXP,MAXEXP)
         NFLD = NFO
      ENDIF
   ELSE
      CALL MENUV2(NWHAT,OPTION,MAXOPT,EXP,MAXEXP)
   ENDIF

   RETURN
   END SUBROUTINE MENUV1


   SUBROUTINE NFILES(MODE, NUM,  NWHAT,  KEY)
!  grid lijst
!  NUM = 0, GELUKT, NUM = 1, NIET GELUKT
   use m_netw
   use m_grid
   use m_observations
   use m_monitoring_crosssections
   use m_thindams
   USE M_SPLINES, notinusenump => nump
   use unstruc_model
   use m_samples
   use m_flowgeom
   use unstruc_display
   use m_flowparameters
   use unstruc_files, only:defaultFilename, close_all_files
   use unstruc_model
   use unstruc_netcdf
   use unstruc_opengis
   use io_openfoam
   use m_partitioninfo
   use m_sferic
   use m_flowtimes
   use dfm_error
   use gridoperations

   implicit none
   integer :: MODE, NUM,  NWHAT,  KEY
   integer :: ja, ierr
   integer :: mlan
   integer :: midp
   integer :: mtek
   integer :: ndraw
   integer :: i, k
   logical :: jawel

   interface
      subroutine realan(mlan, antot)
         integer, intent(inout)                ::  mlan
         integer, intent(inout), optional      ::  antot
      end subroutine realan
   end interface
   
   COMMON /DRAWTHIS/ ndraw(50)
   COMMON /BACKGROUND/ SCREENFILE
   CHARACTER FILNAM*86, SCREENFILE*86

   KEY    = 0

   IF (NWHAT .EQ. 1) THEN
      FILNAM = '*.mdu'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM)
      IF (MLAN .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (MLAN .EQ. -1) THEN   ! Cancel
         NUM = 1
      ELSE
        call doclose(mlan) ! TODO: change... [AvD]
        call inidat() ! TODO: call reset_display_settings() +  call dfm_reset_globaldata()
        call resetFullFlowModel()
        CALL loadModel(filnam)
        call minmxns()
        ! Check for presence of associated display presets
        inquire (file = trim(md_ident)//'.cfg', exist = jawel)
        if (jawel) then
            ja = 1
            !CALL CONFRM('Model-specific display presets found in '//trim(md_ident)//'.cfg. Do you want to load these?', JA)
            if (JA == 1) THEN
                call load_displaysettings(trim(md_ident)//'.cfg')
            end if
        end if

        NDRAW(2) = 1
        KEY = 3
        NUM = 0
      ENDIF
   ELSE IF (NWHAT .EQ. 2) THEN
      FILNAM = '*_net.nc'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM)
      IF (MLAN .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (MLAN .LE. 0) THEN
         NUM = 1
      ELSE
        call doclose(mlan) ! TODO: change... [AvD]
         CALL loadNetwork(filnam, JA, 0)
         IF (JA == 0) THEN
            CALL resetFlow()
            nump = 0 ! Reset cell data
            CALL MESSAGE('YOU LOADED ' , filnam, ' ')
            CALL MINMXNS()
            NDRAW(2) = 1
            KEY = 3
            NUM = 0
            md_netfile = ' '
            md_netfile = trim(filnam)
         ELSE
            CALL qnerror('NO NET LOADED', ' ', ' ')
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 3) THEN
      FILNAM = '*_net.nc'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM)
      IF (MLAN .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (MLAN .LE. 0) THEN
         NUM = 1
      ELSE
         IF (INDEX(FILNAM, '.jan') > 0) then 
            call REAJANET(Mlan,JA,1)
         ELSE IF (INDEX(FILNAM, '.adc') > 0) then 
            call READADCIRCNET(Mlan,JA,1) 
         else 
            call doclose(mlan) ! TODO: change... [AvD]
            call loadNetwork(filnam, JA, 1)
         endif
         
         IF (JA == 0) THEN
            CALL MESSAGE('YOU LOADED ' , filnam, ' ')
            CALL MINMXNS()
            NDRAW(2) = 1
            KEY = 3
            NUM = 0
            md_netfile = ' '
            md_netfile = trim(filnam)
         ELSE
            CALL qnerror('NO NET LOADED', ' ', ' ')
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 4) THEN
      FILNAM = '*.grd'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM)
      IF (MLAN .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (MLAN .LE. 0) THEN
         NUM = 1
      ELSE
         CALL REAgrid(MLAN,FILNAM,ja)  ! DOORLADEN
         IF (JA .GE. 1) THEN
            CALL MESSAGE('YOU LOADED ' , filnam, ' ')
            CALL MINMXNS()
            NDRAW(2) = 1
            KEY = 3
            NUM = 0
         ELSE
            CALL QNERROR('PREMATURE END OF FILE', FILNAM, ' ')
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 5) THEN
      FILNAM = '*.asc'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM)
      IF (MLAN .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (MLAN .LE. 0) THEN
         NUM = 1
      ELSE
         CALL readarcinfo(MLAN,ja)  ! DOORLADEN
         IF (JA .GE. 1) THEN
            CALL MESSAGE('YOU LOADED ' , filnam, ' ')
            CALL MINMXNS()
            NDRAW(2) = 1
            KEY = 3
            NUM = 0
         ELSE
            CALL QNERROR('PREMATURE END OF FILE', FILNAM, ' ')
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 6) THEN
      FILNAM = '*.pol,*.pli'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM)
      IF (MLAN .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (MLAN .LE. 0) THEN
         NUM = 1
      ELSE
         CALL REAPOL(MLAN, 0)
         IF (NPL .GT. 0) THEN
            CALL MESSAGE('YOU LOADED ' , filnam, ' ')
            CALL MINMXNS( )
            KEY = 3
            NUM = 0

   !        read polygon: netcell administration out of date
            netstat = NETSTAT_CELLS_DIRTY
         ELSE
            CALL qnerror('file' , filnam, 'not found ')
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 7) THEN
      FILNAM = '*.spl'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM)
      IF (MLAN .GE. 1) THEN
         CALL readSplines(mlan)
         IF (mcs .GT. 0) THEN
            CALL MESSAGE('You Opened File ', FILNAM, ' ')
            CALL MINMXNS()
            NUM  = 0
            NDRAW(15) = 1
            KEY  = 3
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 8) THEN
      FILNAM = '*.ldb'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM)
      IF (MLAN .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (MLAN .LE. 0) THEN
         NUM = 1
      ELSE
        i = len_trim(filnam)
        if (i > 3) then
            if (filnam(i-2:i) == '.nc') then
                call doclose(mlan)
                call read_land_boundary_netcdf(filnam)
                return
            end if
        end if

         CALL REALAN(MLAN)

         IF (MXLAN .GT. 0) THEN
            CALL MESSAGE('YOU LOADED ' , filnam, ' ')
            CALL MINMXNS()
            NDRAW(3) = 1
            KEY = 3
            NUM = 0
            md_ldbfile = ' '
            md_ldbfile = filnam
         ELSE
            CALL qnerror('MXLAN = 0',' ',' ')
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 9 .or. NWHAT .EQ. 10 ) THEN
      FILNAM = '*_obs.xyn'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM)
      IF (MLAN .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (MLAN .LE. 0) THEN
         NUM = 1
      ELSE
         ja = 0
         key = 3
         call doclose(mlan) ! Ugly, but loadObservations reads by filename, not filepointer [AvD]
         if (NWHAT == 10) then
            ja = 1 ! doorladen
         else
            ja = 0
         end if
         call loadObservations(filnam, ja)
         CALL MESSAGE('YOU LOADED ' , filnam, ' ')
         CALL MINMXNS()
         md_obsfile = ' '
         md_obsfile = filnam
      ENDIF
   ELSE IF (NWHAT .EQ. 11 .or. NWHAT .EQ. 12 ) THEN
      FILNAM = '*_crs.pli'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM)
      IF (MLAN .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (MLAN .LE. 0) THEN
         NUM = 1
      ELSE
         ja = 0
         key = 3
         if (NWHAT == 12) then
            ja = 1 ! doorladen
         else
            ja = 0
         end if
         CALL REAPOL(MLAN, ja) ! Read pol/pli as crs
         call pol_to_crosssections(xpl, ypl, npl, names=nampli)
         if ( NPL.gt.0 ) call delpol()
         CALL MESSAGE('YOU LOADED ' , filnam, ' ')
         CALL MINMXNS()
         md_crsfile = ' '
         md_crsfile = filnam
      ENDIF
   ELSE IF (NWHAT .EQ. 13 .or. NWHAT .EQ. 14 ) THEN
      FILNAM = '*_thd.pli'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM)
      IF (MLAN .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (MLAN .LE. 0) THEN
         NUM = 1
      ELSE
         ja = 0
         key = 3
         if (NWHAT == 14) then
            ja = 1 ! doorladen
         else
            ja = 0
         end if
         CALL REAPOL(MLAN, ja) ! Read pol/pli as thin dam-type crs
         call pol_to_thindams(xpl, ypl, npl)
         CALL MESSAGE('YOU LOADED ' , filnam, ' ')
         CALL MINMXNS()
         md_thdfile = ' '
         md_thdfile = filnam
      ENDIF
   ELSE IF (NWHAT .EQ. 15) THEN
      FILNAM = '*.xyz,*.dem,*.asc'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM)
      IF (MLAN .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (MLAN .LE. 0) THEN
         NUM = 1
      ELSE
         ja = 0
         key = 3
         i = len_trim(filnam)
         if (i > 3) then
            if (filnam(i-3:i) == '.dem' .or. filnam(i-3:i) == '.DEM') then
                call doclose(mlan)
                call read_samples_from_dem(trim(filnam), ja)
            else if (filnam(i-3:i) == '.asc' .or. filnam(i-3:i) == '.ASC') then
                call doclose(mlan)

!               delete all samples, regardless of selecting polygon
                call savepol()
                call delpol()
                call savesam()
                call delsam(0)
                call restorepol()

                call read_samples_from_arcinfo(trim(filnam), ja)  ! reaasc
            else
                CALL reasam(MLAN,ja)  ! DOORLADEN
            end if
         else
            CALL reasam(MLAN,ja)  ! DOORLADEN
         end if
         CALL MESSAGE('YOU LOADED ' , filnam, ' ')
         CALL MINMXNS()
      ENDIF
   ELSE IF (NWHAT .EQ. 16) THEN
      if (ndx == 0 .or. lnx == 0) then
         call qnerror('First reinitialise flow model, current dimensions are 0',' ',' ')
         return
      endif
      if (ibedlevtyp == 1) then
         FILNAM = '*.xybl'
      else if (ibedlevtyp == 2) then
         FILNAM = '*.xyblu'
      else
         CALL qnerror('Loading cell bottom levels bl (ibedlevtyp=1) or flow link bottom levels blu (ibedlevtyp=2)',' ',' ')
         CALL qnerror('Change parameter ibedlevtyp in Various, Change Geometry Parameters',' ',' ')
         return
      endif
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM)
      IF (MLAN .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (MLAN .LE. 0) THEN
         NUM = 1
      ELSE
         if (ibedlevtyp == 1) then
            CALL reabl(MLAN)
         else if (ibedlevtyp == 2) then
            CALL reablu(MLAN)
         endif
         CALL MESSAGE('YOU LOADED ' , filnam, ' ')
         ! CALL MINMXNS()
      ENDIF
   ELSE IF (NWHAT .EQ. 17) THEN
      FILNAM = '*_rst.nc'
      MLAN   = 0
      CALL FILEMENU(MLAN,FILNAM)
      IF (MLAN .EQ. -2) THEN
         CALL qnerror('file' , filnam, 'not found ')
         NUM = 1
      ELSE IF (MLAN .LE. 0) THEN
         NUM = 1
      ELSE
         i = len_trim(filnam)
         if (filnam(i-6:i) == '_rst.nc' .or. filnam(i-6:i) == '_RST.NC') then
            call doclose(mlan) ! TODO: change... [AvD] 
            call read_restart_from_map(FILNAM, ierr)
            if (ierr /= DFM_NOERR) then
               call qnerror('Error occurs when reading the restart file.',' ', ' ')
               JA = 0
            else
               JA = 1
            end if 
            call setucxucyucxuucyunew() ! reconstruct cell center velocities
         else 
            call rearst(MLAN,JA)
         endif   
         !else if (filnam(i-6:i) == '_map.nc' .or. filnam(i-6:i) == '_MAP.NC') then
         !   call doclose(MLAN)
         !   call read_restart_from_map(FILNAM,JA)
         !   ! TODO: AvD: No flow_setstarttime here?
         
         if (JA == 1) then
            call MESSAGE('YOU LOADED ' , filnam, ' ')
         else
            call qnerror('NO RESTART LOADED', ' ', ' ')
         endif
         ! CALL MINMXNS()
      ENDIF
   ELSE IF (NWHAT .EQ. 18) THEN
         NUM    = 0
         FILNAM = '*.bmp'
         MIDP   = 0
         CALL FILEMENU(MIDP,FILNAM)
         IF (MIDP .LE. 0) THEN
            NDRAW(26) = 0
         ELSE IF (MIDP .GE. 1) THEN
            CALL DOCLOSE(MIDP)
            CALL LOADBITMAP(FILNAM)
            CALL MESSAGE('YOU LOADED ' , filnam, ' ')
            CALL MINMXNS( )
         ENDIF
         KEY = 3
   ELSE IF (NWHAT .EQ. 20) THEN
      FILNAM = '*.mdu'
      MTEK   = 1
      CALL FILEMENU(MTEK,FILNAM)
      IF (MTEK .LE. 0) THEN
         NUM = 1
      ELSE
         call doclose(mtek)
         call writeMDUFile(filnam, ja)
         CALL MESSAGE('YOU SAVED ' , filnam, ' ')
         NUM = 0
      ENDIF
   ELSE IF (NWHAT .EQ. 21 .or. NWHAT .EQ. 22 .or. NWHAT .EQ. 24) THEN
      IF (NUMK .EQ. 0) THEN
         CALL QNERROR('NO NET TO SAVE',' ',' ')
         NUM = 0
      ELSE
         if ( nwhat.eq.21 .or. nwhat .eq. 22) then
         FILNAM = '*_net.nc'
         else if ( nwhat.eq.24 ) then
            FILNAM = '*_net.plt'
         end if
         
         MTEK   = 1
         CALL FILEMENU(MTEK,FILNAM)
         IF (MTEK .LE. 0) THEN
            NUM = 1
         ELSE
            call doclose(mtek)
            if (nwhat.eq.21) then
               call unc_write_net(filnam, janetcell = 0, janetbnd = 0)
            else if ( nwhat .eq. 22) then ! _net.nc with extra cell info (for example necessary for Baseline/Bas2FM input)
               if ( netstat.ne.NETSTAT_OK ) then
                  call findcells(0)
                  call find1dcells()
               end if
               call unc_write_net(filnam, janetcell = 1, janetbnd = 1) ! wrinet
!               !call unc_write_net_ugrid2(filnam, janetcell = 0, janetbnd = 0)

               !origial call unc_write_net(filnam, janetcell = 1, janetbnd = 0)
               call unc_write_net('UG'//filnam, janetcell = 1, janetbnd = 0, iconventions = UNC_CONV_UGRID)
            else if ( nwhat.eq.24 ) then
               call ini_tecplot()
               call wrinet_tecplot(filnam)
            end if
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            md_netfile = ' '
            md_netfile = filnam

            !CALL NEWFIL(MTEK, 'NET.NET' )
            !CALL WRINET(MTEK)
            !CALL MESSAGE('AUTOSAVED NET.NET',' ',' ')

            NUM = 0
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 23) THEN
      IF (NUMK .EQ. 0) THEN
         CALL QNERROR('NO NET TO SAVE',' ',' ')
         NUM = 0
      ELSE
          !call foam_write_polymesh('testfoam')
         FILNAM = '*.kml'
         MTEK   = 1
         ja = 1
         if (jsferic /= 1) then
             call confrm('Model is not in spherical coordinates. Proceed? (not recommended)', ja)
         end if
         if (ja == 1) then
            call change_kml_parameters(ja)
         else
             ja = 1 ! Hereafter, 1 means 'no/cancelled'
         end if
         if (ja==0) then ! 0: NOT cancelled
            CALL FILEMENU(MTEK,FILNAM)
            IF (MTEK .LE. 0) THEN
                NUM = 1
            ELSE
                call doclose(mtek)
                call kml_write_net(filnam)
                CALL MESSAGE('YOU SAVED ' , filnam, ' ')
                NUM = 0
            end if
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 24) THEN
   ELSE IF (NWHAT .EQ. 25) THEN
      IF (MC == 0 .or. NC == 0) THEN
         CALL QNERROR('NO GRID TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*.grd'
         MTEK   = 1
         CALL FILEMENU(MTEK,FILNAM)
         IF (MTEK .LE. 0) THEN
            NUM = 1
         ELSE
            call wrirgf(mtek, filnam)
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 26) THEN
      IF (NPL .EQ. 0) THEN
         CALL QNERROR('THERE IS NO POLYGON TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*.pol,*.pli'
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM)
         IF (MIDP .LE. 0) THEN
            NUM = 1
         ELSE
            CALL WRIPOL(MIDP)
            if ( index(Filnam,'crs') == 0 .and. index(Filnam,'CRS') == 0 .and. index(Filnam,'vlay') == 0 .and. index(Filnam,'VLAY') == 0) then
                call wricmps(filnam)
            endif
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
            md_plifile = ' ' ; md_plifile = filnam
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 27) THEN
      IF (mcs .EQ. 0) THEN
         CALL QNERROR('There Are No Splines to SAVE',' ',' ')
      ELSE
         FILNAM = '*.spl'
         MLAN   = 1
         CALL FILEMENU(MLAN,FILNAM)
         IF (MLAN .GE. 1) THEN
            CALL writeSplines(MLAN)
            CALL MESSAGE('You Saved File ', FILNAM, ' ')
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 28) THEN
      IF (MXLAN .EQ. 0) THEN
         CALL QNERROR('THERE IS NO LANDBOUNDARY TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*.ldb'
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM)
         IF (MIDP .LE. 0) THEN
            NUM = 1
         ELSE
            CALL WRILAN(MIDP)
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
            md_ldbfile = ' '
            md_ldbfile = filnam
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 29) THEN
      IF (numobs .EQ. 0) THEN
         CALL QNERROR('THERE are NO observation points TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = defaultFilename('obs')
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM)
         IF (MIDP .LE. 0) THEN
            NUM = 1
         ELSE
            call doclose(midp)
            CALL saveObservations(filnam)
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
            md_obsfile = ' '
            md_obsfile = filnam
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 30) THEN
      IF (ncrs .EQ. 0) THEN
         CALL QNERROR('THERE are NO cross sections TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*_crs.pli'
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM)
         IF (MIDP .LE. 0) THEN
            NUM = 1
         ELSE
            CALL WRICRS(MIDP)
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
            md_crsfile = ' '
            md_crsfile = filnam
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 31) THEN
      IF (Ns .EQ. 0) THEN
         CALL QNERROR('THERE are NO samples TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*.xyz'
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM)
         IF (MIDP .LE. 0) THEN
            NUM = 1
         ELSE
            CALL WRIsam(MIDP)
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 32) THEN
      if (ndx == 0 .or. lnx == 0) then
         call qnerror('First reinitialise flow model, current dimensions are 0',' ',' ')

         return
      else
         if (ibedlevtyp == 1) then
            FILNAM = '*.xybl'
         else if (ibedlevtyp == 2) then
            FILNAM = '*.xyblu'
         else
            CALL qnerror('Just saving the network is sufficient for (preferred option) ibedlevtyp = 3 ',' ',' ')
            CALL qnerror('See Various, Change Geometry Parameters ',' ',' ')
            return
         endif
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM)
         IF (MIDP .LE. 0) THEN
            NUM = 1
         ELSE
            if (ibedlevtyp == 1) then
               CALL WRIbl(MIDP)
            else if (ibedlevtyp == 2) then
               CALL WRIblu(MIDP)
            endif
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 33) THEN
      IF (NDX .EQ. 0) THEN
         CALL QNERROR('THERE IS NO FLOW TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*_rst.nc'
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM)
         IF (MIDP .LE. 0) THEN
            NUM = 1
         ELSE
            call doclose(midp)
            CALL unc_write_rst(filnam)
            call wrirstfileold(time1)  
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 34) THEN
      IF (NDX .EQ. 0) THEN
         CALL QNERROR('THERE IS NO FLOW TO SAVE',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*_map.nc'
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM)
         IF (MIDP .LE. 0) THEN
            NUM = 1
         ELSE
            call doclose(midp)
            CALL unc_write_map(filnam)
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 35) THEN
     FILNAM = '*'
     MIDP   = 0
     CALL FILEMENU(MIDP,FILNAM)
     IF (MIDP .LE. 0) THEN
        NUM = 1
     ELSE
        call doclose(midp)
        call parsekerst(filnam)
        NUM = 0
        KEY = 3
     ENDIF
!

!
!   ELSE IF (NWHAT .EQ. 20) THEN
!      FILNAM = '*.unt'
!      MLAN   = 0
!      CALL FILEMENU(MLAN,FILNAM)
!      IF (MLAN .EQ. -2) THEN
!         CALL qnerror('file' , filnam, 'not found ')
!         NUM = 1
!      ELSE IF (MLAN .LE. 0) THEN
!         NUM = 1
!      ELSE
!         CALL reajanet(MLAN,JA,1) !1=DOORLADEN
!         CALL MESSAGE('YOU LOADED ' , filnam, ' ')
!         CALL MINMXNS()
!         KEY = 3
!
!      ENDIF

   ELSE IF (NWHAT .EQ. 36) THEN

      IF (numk .EQ. 0) THEN
         CALL QNERROR('THERE is no network to save ',' ',' ')
         NUM = 0
      ELSE
         FILNAM = '*.node'
         MIDP   = 1
         CALL FILEMENU(MIDP,FILNAM)
         IF (MIDP .LE. 0) THEN
            NUM = 1
         ELSE
            CALL WRIswan(MIDP,filnam)
            CALL MESSAGE('YOU SAVED ' , filnam, ' ')
            NUM = 0
         ENDIF
      ENDIF
   ELSE IF (NWHAT .EQ. 37 ) THEN    ! partition files
      if ( ndomains.lt.1 ) then
         call qnerror('no partitions found', ' ', ' ')
      else
!         FILNAM = '*_net.nc'
         filnam = md_netfile
         MTEK   = 1
         CALL FILEMENU(MTEK,FILNAM)
         IF (MTEK .LE. 0) THEN
            NUM = 1
         ELSE
            call doclose(mtek)
            call partition_write_domains(filnam,6,1,1) ! make subdomains for default solver
            CALL MESSAGE('YOU SAVED ' , filnam, ' partitions')
            md_netfile = ' '
            md_netfile = filnam

            !CALL NEWFIL(MTEK, 'NET.NET' )
            !CALL WRINET(MTEK)
            !CALL MESSAGE('AUTOSAVED NET.NET',' ',' ')

            NUM = 0
         ENDIF
      end if

   ELSE IF (NWHAT .EQ. 38) THEN
      CALL STOPINT()
      NUM = 0
   ENDIF
   ! Nader uitwerken, of helemaal overboord ermee
   NUM = 0
   RETURN
   END SUBROUTINE NFILES

   subroutine plotnu(fnam)
   COMMON /DRAWTHIS/  ndraw(50)
   COMMON /PLOTFIL/   PLOTJE
   CHARACTER PLOTJE*255
   character (len=*) fnam
   
   plotje = fnam
   key = 3 ; ndraw(10) = 1
   call drawnu(key)
   plotje = ' '
   
   end subroutine plotnu
   
   SUBROUTINE DRAWNU(KEY)
   use m_netw
   USE M_SAMPLES
   use unstruc_display
   use unstruc_opengl
   implicit none

   double precision :: epsgs
   integer :: itgs
   integer :: maxitgs
   integer :: metdraw
   integer :: ndraw

   integer :: KEY, ja, nsiz

   COMMON /DRAWTHIS/  ndraw(50)

   COMMON /SOLVER/    EPSGS, MAXITGS, ITGS

!
   IF (KEY .NE. 3) RETURN

   METDRAW = NDRAW(9)

   CALL IMouseCursorHIDE()
   CALL PLOT(NDRAW(10))
   IF (NDRAW(10) .EQ. -1) THEN
      RETURN
   ENDIF

   if (jaOpengl == 0) then 
      IF (METDRAW .EQ. 1)   CALL FULLSCREEN()
      IF (NDRAW(1) .EQ. 1 .and. jaOpenGL.eq.0 )  CALL CLS1()
      IF (NDRAW(26) .EQ. 1) CALL SHOWBITMAP(0)
      IF (METDRAW .EQ. 1)   CALL SMALLSCREEN()
   else 
      CALL BEGINRENDER()
   endif

   
   METDRAW = NDRAW(9)
 ! ndraw(28)= show what on nodes   ndraw(19)=how to show on nodes , NDRAW(8) = SHOW WHAT ON NETNODES
 ! ndraw(29)= show what on links   ndraw(11)=how to show on links , NDRAW(7) = SHOW WHAT ON NETLINKS

   if (ndraw(3) > 4) CALL TEKLAN(NCOLLN)

   IF (NDRAW(7) .GE. 2) THEN
       CALL NETLINKVALS(NDRAW(7),NCOLLN)
       CALL MINMXNETLINS()
   ENDIF

   IF (NDRAW(8) .GE. 2) THEN
       CALL NETNODEVALS(NDRAW(8))
       CALL MINMXNETNODS()
   ENDIF

   IF (METDRAW .EQ. 1) THEN

      CALL TEKNETSTUFF(key)
      
      CALL TEKFLOWSTUFF(key)      
      
      call highlight_nodesnlinks()

      call TEKgrid(key)

      if (ns > 0) then
        call teksam(xs,ys,zs,ns,ndraw(32))
      endif

      if (ndraw(2) == 6) then
         CALL TEKNET(NCOLDN,key) ! network on top
         call tekpartmesh()
      end if

      if (ndraw(3) <= 4) CALL TEKLAN(NCOLLN)

      call plotObservations()
      
      call teksorsin()

      call plotSplines()
      
     ! obs plotting used to be here [AvD]
      if (NDRAW(18) > 1) then
         nsiz = ndraw(18)-1
         call tekrai(nsiz,ja)
      endif
 
      call tekprofs()         ! and initialise some turb parstm.amp
      
      call plotCrossSections()

      call plotThinDams()
      call plotFixedWeirs()

      call plotManholes()
      
      call tekwindvector()

      if (ndrawpol > 1) then 
         call tekpolygon()
      endif
      
      call plotdots()
   
      call plotStructures()
            
   ELSE IF (METDRAW .EQ. 2) THEN

      ! CALL PERSPC()

   ENDIF

   ! WARNING: Anything drawn up to this point with something other than OpenGL, is overwritten! 
   ! So make sure you use OpenGL for any rendering up to this point, move EndRender up, or place 
   ! that graphics code after EndRender.
   
   CALL ENDRENDER()
   
   IF (METDRAW .EQ. 1) CALL FULLSCREEN()
   CALL ISOSCALE()
   CALL ISOSCALE2()
   CALL TXTLINES()

   IF (METDRAW .EQ. 1) CALL SMALLSCREEN()
   IF (METDRAW .EQ. 1) CALL AXES()
   CALL ANCHORCLS()
   CALL DISPOS()

   CALL TEXTFLOW()
   if (idisLink /= 0) then ! Display info. screen for a 1D flowlink if it has been clicked
      call disln(idisLink)
      call dis_info_1d_link(idisLink)
   end if
  
   CALL IMouseCursorShow()

   IF (NDRAW(10) .EQ. 2) THEN
      CALL PLOT(NDRAW(10))
   ENDIF
   
!   if ( japart.eq.1 ) then
!      call tekpartmesh()
!   end if

   RETURN
   END SUBROUTINE DRAWNU
 


   subroutine tekpolygon()
   use m_polygon
   use unstruc_display
   use m_missing
   use gridoperations

   implicit none

   integer           :: k,ncol,kk, key,k2
   double precision  :: a,b,f,x,y,z,s,c,d,dx,dy,dz

   if (ndrawpol == 2) then
      CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLPL)

   else if (ndrawpol == 3) then

      call linewidth(3)  
      do k = 1,npl-1
         if (zpl(k) .ne. dmiss) then
            if ( inview( xpl(k), ypl(k) ) .AND. inview ( xpl(k+1), ypl(k+1) )  ) then
               call isocol( (zpl(k)+zpl(k+1))/2, ncol)
               call movabs(xpl(k)  , ypl(k))
               call lnabs (xpl(k+1), ypl(k+1))
            endif
         endif
      enddo
      call linewidth(1)

   else if (ndrawpol == 4 .or. ndrawpol == 5 .or. ndrawpol == 6) then

      CALL DISP2C(XPL, YPL, NPL, 0d0, NCOLPL)
      CALL SETCOL(NCOLBLACK)
      do k = 1,npl
         if ( inview(xpl(k), ypl(k) ) ) then
            if ( ndrawpol == 4) then
               call HTEXT(Zpl(k),Xpl(k),Ypl(k))
            else if ( ndrawpol == 5 .and. jakol45 > 0) then
               call HTEXT(dzL(k),Xpl(k),Ypl(k))
            else if ( ndrawpol == 6 .and. jakol45 > 0) then
               call HTEXT(dzr(k),Xpl(k),Ypl(k))
            endif
         endif
      enddo

     else if (ndrawpol == 7 .and. jakol45 > 0) then

      do k = 1,npl-1

         IF (MOD(k,100) ==  0) THEN
             CALL HALT2(KEY)
             IF (KEY .EQ. 1) RETURN
         ENDIF

         if (zpl(k) .ne. dmiss) then
            if ( inview( xpl(k), ypl(k) ) .AND. inview ( xpl(k+1), ypl(k+1) )  ) then
               call isocol( (zpl(k)+zpl(k+1))/2, ncol)
               call movabs(xpl(k)  , ypl(k))
               call lnabs (xpl(k+1), ypl(k+1))

               call sincosdis (xpl(k), ypl(k), xpl(k+1), ypl(k+1), s, c, d)

               dy = rcir*c
               dx = -rcir*s

               k2 = max(2, int (d /(3d0*rcir)) )
               do kk = 1, k2
                  a  = 1d0 - dble(kk)/dble(k2)
                  b = 1d0-a
                  x  = a*xpl(k) + b*xpl(k+1)
                  y  = a*ypl(k) + b*ypl(k+1)
                  z  = a*zpl(k) + b*zpl(k+1)
                  dz = a*dzl(k) + b*dzl(k+1)
                  f  = dz/5d0
                  call isocol( z-dz, ncol )
                  call movabs( x+f*dx, y+f*dy )
                  call  lnabs( x, y )

                  dz = a*dzr(k) + b*dzr(k+1)
                  f  = dz/5d0
                  call isocol( z-dz, ncol )
                  call lnabs ( x-f*dx, y-f*dy )
               enddo
            endif
         endif
      enddo

      
    else if ( ndrawpol == 8 ) then
         
         CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLPL)
         do k = 1,npl
            if ( inview(xpl(k), ypl(k) ) ) then
               call HTEXT(dble(k),Xpl(k),Ypl(k))
            endif
         enddo   
         call hTEXT(dble(k),Xpl(k),Ypl(k))
      
   endif
   end subroutine tekpolygon

   SUBROUTINE TEKNETSTUFF(key)
   use unstruc_colors
   use unstruc_display, only: jaHighlight
   use m_netw
   implicit none
   integer :: ndraw
   double precision :: XP, YP

   integer :: key, K1, K2

   COMMON /DRAWTHIS/ ndraw(50)

   IF (NDRAW(7) .GE. 2) CALL TEKLINKVALS(NDRAW(11))

   IF (NDRAW(8) .GE. 2) CALL TEKNODEVALS(NDRAW(19))

   CALL TEKNET(NCOLDN,key)

   CALL TEKPREVIOUSNET(NCOLRN)



   IF (NDRAW(7) .GE. 2) CALL TEKLINKNUMS(NDRAW(11),NCOLLN)

   IF (NDRAW(8) .GE. 2) CALL TEKNODENUMS(NDRAW(19),NCOLDN)


   CALL TEKNETCELLS(NDRAW(33),0,1)

   ! CALL TEKBOTTOM(NDRAW(27)) old net stuff

   if (jaHighlight == 1) then
      if (nOdmax .ne. 0) then
         call gtext( 'NETNODMax', xK(nOdmax), yK(nOdmax), 31  )
      endif
      if (nOdmin .ne. 0) then
         call gtext( 'NETNODMin', xK(nOdmin), yK(nOdmin), 221 )
      endif
      if (LINmax .ne. 0) then
         K1 = KN(1,LINMAX)
         K2 = KN(2,LINMAX)
         IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
            XP = 0.5D0*(XK(K1) + XK(K2) )
            YP = 0.5D0*(YK(K1) + YK(K2) )
         ENDIF
         call gtext( 'NETLINMax', XP, YP, 31  )
      endif
      if (LINmin .ne. 0) then
         K1 = KN(1,LINMIN)
         K2 = KN(2,LINMIN)
         IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
            XP = 0.5D0*(XK(K1) + XK(K2) )
            YP = 0.5D0*(YK(K1) + YK(K2) )
            call gtext( 'NETLINMin', XP, YP, 221 )
         ENDIF
      endif
      if (netcelmax .ne. 0) then
         call gtext( 'NETcelmax', xzw(netcelmax), yzw(netcelmax), 31  )
      endif
      if (netcelmin .ne. 0) then
         call gtext( 'NETcelmin', xzw(netcelmin), yzw(netcelmin), 221 )
      endif
   end if

   RETURN
   END SUBROUTINE TEKNETSTUFF

   subroutine teknetcells(netwhat, jahalt, jacol)
   use m_netw
   use m_flowgeom
   use unstruc_display
   use m_missing
   use m_partitioninfo
   use m_alloc
   use unstruc_model, only: md_netfile
   use unstruc_messages
   use m_sferic, only: jsferic, dg2rd
   use gridoperations
   
   implicit none

   integer, intent (in)          :: netwhat, jahalt, jacol

   double precision              :: xx(6), yy(6), zz(6), aspect, uu1, vv1, uu2, vv2, VFAC,VFACFORCE
   double precision, allocatable :: zn(:)

   double precision              :: xc, yc
   double precision              :: xfac

   integer                       :: k, kk, n,ja, ncol, nodemode, nn, nvec

   integer                       :: ntopology, numcellstoplot

   double precision, external    :: znetcell
   double precision, external    :: coarsening_info

   COMMON /VFAC/ VFAC,VFACFORCE,NVEC
   COMMON /DRAWTHIS/                ndraw(50)
   integer                       :: ndraw !, ierr, jaidomain

   if ( netwhat .le. 1 ) return


   nodemode = NDRAW(19)

   ntopology = numk + numl
   if ( ntopology .ne. lasttopology .and. lasttopology.gt.0 ) THEN  ! coarsening info
      if ( ubound(lnn,1).ne.numL ) then
 !        if (md_genpolygon == 0 .and. len_trim(md_netfile)>0) then
 !           if (netwhat.ge.15.and.netwhat.le.19 ) then
 !              jaidomain = 1
 !           else 
 !              jaidomain = 0
 !           end if
 !           call preparecells(md_netfile, jaidomain, ierr)
 !           if (ierr .ne. 0) then
 !              call mess(LEVEL_WARN, 'Cannot find cell info. and will enforce findcells')
 !           endif
 !        else
 !            ierr = -1 ! Force findcells hereafter.
 !        end if
 !        if (ierr /= 0) then
            call findcells(100)
            if ( netwhat .eq. 2 .or. netwhat.ge.15.and.netwhat.le.19 ) then
               call find1dcells()    ! partitioning info
            endif
            if (netwhat.ge.15.and.netwhat.le.19 ) then
                call mess(LEVEL_WARN, 'Cannot find partition info.')
                NDRAW(33) = 0
            end if
!         endif
      else
         call setnodadm(0)      ! in case the administration is out of date
      end if
      call makenetnodescoding()
   end if


   numcellstoplot = nump
   if ( netwhat.eq.2 .or. netwhat.ge.15.and.netwhat.le.19 ) numcellstoplot = nump1d2d ! only for cell or domain numbers

   if ( numcellstoplot.gt.size(rlin) ) then
      call realloc(rlin,numcellstoplot)
   end if

   call setcol(0)

!  uncomment the following to refresh netcell administration, based on module variable netstat
!   if ( netstat /= NETSTAT_OK ) then
!      call findcells(100)
!   end if

!  scalars
   if ( netwhat .ne. 4 ) then
      if ( netwhat .eq.7 ) then ! coarsening info
         do k = 1,nump
            rlin(k) = coarsening_info(k)
         enddo
      else if (netwhat < 14 ) then   ! default
         do k = 1,numcellstoplot
            rlin(k)  = znetcell(k)
         enddo
      else if ( ( netwhat.eq.15 .or. netwhat.eq.16 ) .and. allocated(idomain) ) then  ! partitioning info
         if ( size(idomain).ge.numcellstoplot ) then
            if ( netwhat.eq.15 ) then
               do k=1,numcellstoplot
                  rlin(k) = dble(idomain(k))
               end do
            else if ( netwhat.eq.16 .and. allocated(numndx) ) then  ! partitioning info
               do k=1,numcellstoplot
                  rlin(k) = dble(numndx(idomain(k)))
               end do
            end if
         end if
      else if ( netwhat.eq.17 .or. netwhat.eq.18 .or. netwhat.eq.19 ) then ! ghost levels
         if ( allocated(ighostlev) ) then
            if ( size(ighostlev).ge.numcellstoplot ) then
               do k=1,numcellstoplot
                  if ( netwhat.eq.17) then
                     rlin(k) = dble(ighostlev(k))
                  else if ( netwhat.eq.18 ) then
                     rlin(k) = dble(ighostlev_cellbased(k))
                  else
                     rlin(k) = dble(ighostlev_nodebased(k))
                  end if
               end do
            end if
         end if
      else if ( netwhat.eq.20 ) then   ! global cell number
         if ( allocated(iglobal_s) ) then
            if ( size(iglobal_s).ge.numcellstoplot ) then
               do k=1,numcellstoplot
                  rlin(k) = dble(iglobal_s(k))
               end do
            end if
          end if
      end if

      if(nodemode.eq.3 .or. nodemode.eq. 6 .and. netwhat < 14) then
         call copynetcellstonetnodes()
      endif

      call MINMXNETCELLS()

      do k = 1,numcellstoplot
         if (mod(k,200) == 0) then
            if (jahalt.ne.-1234) call halt2(ja)
            if (ja == 1) return
         endif

         if (inview( xzw(k), yzw(k) ) .and. rlin(k).ne.DMISS ) then
            if (nodemode.eq.2 .or. nodemode.eq.6 .or.   &
                nodemode.eq.7 .or. nodemode.eq.8) then        ! numbers
               call setcol(1)
               if ( netwhat.eq.2 .or. netwhat.eq.15 ) then                        ! cell numbers or domain numbers
                  call dhitext( int(rlin(k)), xzw(k)+RCIR, yzw(k)-RCIR, yzw(k) )
               else
                  call dhtext( dble(rlin(k)), xzw(k)+RCIR, yzw(k)-RCIR, yzw(k) )
               end if
            end if
            if (nodemode.eq.3 .or. nodemode.eq. 6) then       ! isolines within cell
               call ISOSMOOTHnet(k)
            else if (nodemode.eq.4 .or. nodemode.eq. 7) then  ! isofil= cellfill
               call isocol(dble(rlin(k)),ncol)
               nn = netcell(k)%n
               do kk=1,nn
                 xx(kk) = xk(netcell(k)%nod(kk))
                 yy(kk) = yk(netcell(k)%nod(kk))
               end do
               call PFILLER(xx, yy, nn, NCol, NCol)
            else if (nodemode.eq.5 .or. nodemode.eq.8 ) then
               call isocol(dble(rlin(k)),ncol)
               call drcirc(xz(k),yz(k),dble(rlin(k)))
            endif
         endif
      enddo
   end if

!  vectors
   if (netwhat == 4 .or. netwhat == 5) then
      do k = 1,numcellstoplot
         if (mod(k,200) == 0) then
            if (jahalt.ne.-1234) call halt2(ja)
            if (ja == 1) return
         endif

         if (inview( xzw(k), yzw(k) ) ) then
            call orthonet_compute_orientation(aspect, uu1, vv1, uu2, vv2, k)
            if ( jacol .eq. 1 ) call setcol(3)
            
            if ( jsferic.eq.1 ) then
               xfac=1d0/cos(yzw(k)*dg2rd)
            else
               xfac=1d0
            end if

            if ( uu1**2 + vv1**2 .lt. uu2**2 + vv2**2 ) then
               if ( jacol .eq. 1 ) call setcol(3)
               call arrowsxy( xzw(k), yzw(k), uu1*xfac, vv1,  0.5d0*VFAC)
               call arrowsxy( xzw(k), yzw(k), uu1*xfac, vv1, -0.5d0*VFAC)
               if ( jacol .eq. 1 ) call setcol(221)
            else
               if ( jacol .eq. 1 ) call setcol(221)
               call arrowsxy( xzw(k), yzw(k), uu1*xfac, vv1,  0.5d0*VFAC)
               call arrowsxy( xzw(k), yzw(k), uu1*xfac, vv1, -0.5d0*VFAC)
               if ( jacol .eq. 1 ) call setcol(3)
            end if
            call arrowsxy( xzw(k), yzw(k), uu2*xfac, vv2,  0.5d0*VFAC)
            call arrowsxy( xzw(k), yzw(k), uu2*xfac, vv2, -0.5d0*VFAC)
         endif
      enddo
   endif

   END SUBROUTINE TEKNETCELLS

   double precision function znetcell(k)

   use unstruc_display
   use m_netw
   use m_flowgeom
   use m_missing
   use geometry_module, only: dbdistance
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer          :: k, k1, k2, k3, n, ja
   double precision :: uu1, vv1, uu2, vv2 ! not used here
   double precision :: xdum, ydum, area, phimin, phimax
   double precision :: xx1,yy1,zz1,xx2,yy2,zz2,xx3, yy3, zz3, xy, rn, R3, XN, YN, ZN, DEPTH, TSIG, SLOPE, RK

   COMMON /DRAWTHIS/  ndraw(50)
   integer        ::  ndraw

   znetcell = DMISS

   if ( NDRAW(33)>= 3 .and. NDRAW(33)<=5) then
      call orthonet_compute_orientation(znetcell, uu1, vv1, uu2, vv2, k)
   else if ( ndraw(33)==6) then   ! cell area
      znetcell = ba(k)
   else if ( ndraw(33)==2 ) then  ! cell numbers
      if ( netcell(k)%N.gt.0 ) znetcell = dble(k)
   else if ( ndraw(33)==8 ) then  ! cell tri, 4, 5etc
       znetcell = dble( netcell(k)%n )
   else if ( ndraw(33)==9 ) then  ! cell normalised centre of gravity - circumcentre distance
       if (ba(k) > 0) then
           znetcell = dbdistance(xz(k),yz(k), xzw(k), yzw(k), jsferic, jasfer3D, dmiss) / sqrt(ba(k))
       else
           znetcell = 0d0
       endif
   else if ( ndraw(33)==10 .or. ndraw(33)==11 ) then ! slope
       k1 = netcell(k)%nod(1)
       k2 = netcell(k)%nod(2)
       k3 = netcell(k)%nod(3)

       XX1 =  Xk(k1) - Xk(k2)  ! getdx etc
       YY1 =  Yk(k1) - Yk(k2)
       ZZ1 =  Zk(k1) - Zk(k2)
       XX2 =  Xk(k1) - Xk(k3)
       YY2 =  Yk(k1) - Yk(k3)
       ZZ2 =  Zk(k1) - Zk(k3)
       XX3 =  (YY1*ZZ2 - YY2*ZZ1)
       YY3 = -(XX1*ZZ2 - XX2*ZZ1)
       ZZ3 =  (XX1*YY2 - XX2*YY1)
       R3 =  SQRT(XX3*XX3 + YY3*YY3 + ZZ3*ZZ3)
       IF (R3 .NE. 0) THEN
         XN = XX3/R3
         YN = YY3/R3
         ZN = ZZ3/R3
         XY = SQRT(XN*XN + YN*YN)
         IF (ZN .NE. 0) THEN
            slope = ABS(XY/ZN)
            znetcell = slope
            IF (ndraw(33) == 11) THEN
               DEPTH    = - (ZK(K1) + ZK(K2) + ZK(K3)) / 3
               IF (DEPTH .GE. .01) THEN
                  TSIG  = 5D0
                  CALL getwavenr(depth,tsig,rk)
                  znetcell = SLOPE/(DEPTH*RK)
               else
                  znetcell = dmiss
               ENDIF
            ENDIF
         ENDIF
      ENDIF

   else if ( ndraw(33)== 12 .or. ndraw(33)==13 ) then ! min, max angles
       call CHECKTRIANGLEnetcell(k,JA,phimin,phimax)
       if ( ndraw(33)== 12) then
           znetcell = phimin
       else
           znetcell = phimax
       endif
   endif

   end function znetcell

   SUBROUTINE NDISPLAY(NWHAT,KEY)
   USE M_FLOW
   USE M_FLOWGEOM
   use unstruc_display
   use unstruc_model,   only : md_ident
   use unstruc_startup, only : initgui
   use m_physcoef,      only : ifrctypuni
   use m_sediment
   use m_plotdots
   use m_transport
   use m_waves, only: waveparopt, numoptwav
   use m_xbeach_data,   only: windmodel
   use gridoperations
   
   implicit none
   integer :: maxexp, ium
   integer :: maxopt
   integer :: ndraw
   integer :: nputz
   integer :: nwhat2, MINP
   integer :: NWHAT,KEY
   logical :: jawel
   character(len=255) :: filnam
   integer :: mfil
   integer :: i
   integer :: numopt

   integer, parameter :: MAXOP = 64

   COMMON /DRAWTHIS/  ndraw(50)
   CHARACTER*40 OPTION(MAXOP),EXP(MAXOP)
   
1234 continue

   IF (NWHAT .EQ. 1) THEN
      EXP(1)    = 'MENU 9                                  '
      EXP(2)    = 'DISPLAY PRESETS                         '
      OPTION(1) = 'Network topology (nrs)                  '
      OPTION(2) = 'Network orthogonality                   '
      OPTION(3) = 'Flow display                            '
      OPTION(4) = 'Load display settings                   '
      OPTION(5) = 'Save current display settings           '

      MAXOPT    = 5
      NWHAT2    = 0
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 == 1) THEN ! Network topology
         NDRAW(2)  = 1 !    Network solid lines
         NDRAW(16) = 0 ! NO previous network
         NDRAW(19) = 2 ! MODE nodevalues as numbers
         NDRAW(11) = 2 ! MODE linkvalues as numbers
         NDRAW(7)  = 2 !    netlink values: numbers
         NDRAW(8)  = 2 !    netnode values: numbers
         NDRAW(28) = 1 ! NO flownode values
         NDRAW(29) = 1 ! NO flowlink values
         NDRAW(33) = 2 !    netcell values: numbers
         KEY = 3
      ELSEIF (NWHAT2 == 2) THEN ! Network orthogonality
         NDRAW(2)  = 1 !    Network solid lines
         NDRAW(16) = 0 ! NO previous network
         NDRAW(19) = 5 ! MODE nodevalues as dots
         NDRAW(11) = 5 ! MODE linkvalues as dots
         NDRAW(7)  = 4 !    netlink values: orthogonality
         NDRAW(8)  = 1 ! NO netnode values
         NDRAW(28) = 1 ! NO flownode values
         NDRAW(29) = 1 ! NO flowlink values
         NDRAW(33) = 1 ! NO netcell values
         KEY = 3
      ELSEIF (NWHAT2 == 3) THEN ! Flow display
         NDRAW(2)  = 1 !    Network solid lines
         NDRAW(16) = 0 ! NO previous network
         NDRAW(19) = 3 ! MODE nodevalues as isofil smooth
         NDRAW(11) = 5 ! MODE linkvalues as dots
         NDRAW(7)  = 4 !    netlink values: orthogonality
         NDRAW(8)  = 1 ! NO netnode values
         NDRAW(28) = 2 !    flownode values: waterlevel
         NDRAW(29) = 1 ! NO flowlink values
         NDRAW(33) = 1 ! NO netcell values
         KEY = 3
      ELSEIF (NWHAT2 == 4) THEN ! Load display preset
         FILNAM = '*.cfg'
         MFIL   = 0
         CALL FILEMENU(MFIL,FILNAM)
         IF (MFIL .EQ. -2) THEN
            CALL qnerror('file' , filnam, 'not found ')
         ELSE IF (mfil > 0) THEN
            call initGUI(0) ! NO INTINI
            call doclose(mfil)
            call load_displaysettings(filnam)
            CALL MESSAGE('YOU LOADED ' , filnam, ' ')
            key = 3
         end if
      ELSE IF (NWHAT2 .EQ. 5) THEN
         if (len_trim(md_ident) == 0) then
            FILNAM = '*.cfg'
         else
            FILNAM = trim(md_ident)//'.cfg'
         endif
         mfil   = 1
         CALL FILEMENU(mfil,FILNAM)
         IF (mfil > 0) THEN
             call doclose(mfil)
             CALL save_displaysettings(filnam)
             CALL MESSAGE('YOU SAVED ' , filnam, ' ')
         ENDIF
      ENDIF
   ELSEIF (NWHAT .EQ. 2) THEN
      EXP(1)    = 'MENU 9                                  '
      EXP(2)    = 'HOW TO DISPLAY THE NETWORK              '
      OPTION(1) = 'NO NETWORK                              '
      OPTION(2) = 'NETWORK SOLID LINES                     '
      OPTION(3) = 'NETWORK SOLID LINES + OUTLINE           '
      OPTION(4) = 'NETWORK OUTLINE ONLY                    '
      OPTION(5) = 'NETWORK + XZ,YZ                         '
      OPTION(6) = 'NETWORK + crossings/quality checks      '
      OPTION(7) = 'NETWORK + on top                        '

      MAXOPT    = 7
      NWHAT2    = NDRAW(2) + 1
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .GE. 1) THEN
         IF (NWHAT2 - 1 .NE. NDRAW(2) ) KEY = 3
         NDRAW(2) = NWHAT2 - 1
         if (NDRAW(2)>=2 .and. NDRAW(2)<=4) then
            call findcells(0)
         else if (NDRAW(2)==5) then
            call checknetwork()
            KEY = 3
         end if
      ENDIF
   ELSE IF (NWHAT .EQ. 3) THEN
      EXP(1)    = 'MENU 9                                  '
      EXP(2)    = 'HOW TO DISPLAY THE PREVIOUS NETWOK      '
      OPTION(1) = 'NO NETWORK                              '
      OPTION(2) = 'NETWORK SOLID LINES                     '
      OPTION(3) = 'OTHER                                   '
      OPTION(4) = 'GRID SPLINE SHAPE SOLID LINES           '
      OPTION(5) = 'GRID SPLINE SHAPE DOTTED LINES          '
      OPTION(6) = 'GRID SOLID LINES PLUS M,N COORDINATES   '
      MAXOPT    = 6
      NWHAT2    = NDRAW(16) + 1
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .GE. 1) THEN
         IF (NWHAT2 - 1 .NE. NDRAW(16) ) KEY = 3
         NDRAW(16) = NWHAT2 - 1
      ENDIF
   ELSE IF (NWHAT .EQ. 4) THEN
      EXP(1)    = 'MENU 9                                  '
      EXP(2)    = 'HOW TO DISPLAY THE SPLINES              '
      OPTION(1) = 'No Splines                              '
      OPTION(2) = 'Splines with Dots                       '
      OPTION(3) = 'Splines                                 '
      MAXOPT    = 3
      NWHAT2    = NDRAW(15) + 1
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .GE. 1) THEN
         IF (NWHAT2 - 1 .NE. NDRAW(15) ) KEY = 3
         NDRAW(15) = NWHAT2 - 1
      ENDIF
   ELSE IF (NWHAT .EQ. 5) THEN
      EXP(1)    = 'MENU 10                                 '
      EXP(2)    = 'HOW TO DISPLAY THE land boundary        '
      OPTION(1) = 'NO land boundary                        '
      OPTION(2) = 'LINES                                   '
      OPTION(3) = 'LINES + DOTS                            '
      OPTION(4) = 'LINES + NRS                             '
      OPTION(5) = 'THICK LINES                             '
      OPTION(6) = 'LINES,        first drawing object      '
      OPTION(7) = 'LINES + DOTS, first drawing object      '
      OPTION(8) = 'LINES + NRS , first drawing object      '
      OPTION(9) = 'THICK LINES , first drawing object      '
 
      MAXOPT    = 9
      NWHAT2    = NDRAW(3) + 1
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .GE. 1) THEN
         IF (NWHAT2 - 1 .NE. NDRAW(3) ) KEY = 3
         NDRAW(3) = NWHAT2 - 1
      ENDIF
   ELSE IF (NWHAT .EQ. 6) THEN
      EXP(1)    = 'MENU 8                                  '
      EXP(2)    = 'HOW TO DISPLAY NODE VALUES              '
      OPTION(1) = 'NO                                      '
      OPTION(2) = 'NUMBERS                                 '
      OPTION(3) = 'ISOFIL SMOOTH                           '
      OPTION(4) = 'ISOFIL                                  '
      OPTION(5) = 'DOTS                                    '
      OPTION(6) = 'ISOFIL SMOOTH + NUMBERS                 '
      OPTION(7) = 'ISOFIL        + NUMBERS                 '
      OPTION(8) = 'DOTS          + NUMBERS                 '
      OPTION(9) = 'highlight dots smallest 5 %             '
      OPTION(10)= 'highlight dots largest  5 %             '
      MAXOPT    = 10
      NWHAT2    = NDRAW(19)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .NE. NDRAW(19) ) KEY = 3
      NDRAW(19) = NWHAT2
   ELSE IF (NWHAT .EQ. 7) THEN
      EXP(1)    = 'MENU 8                                  '
      EXP(2)    = 'HOW TO DISPLAY LINK VALUES              '
      OPTION(1) = 'NO                                      '
      OPTION(2) = 'NUMBERS                                 '
      OPTION(3) = 'ISOfil SMOOTH                           '
      OPTION(4) = 'ISOFIL                                  '
      OPTION(5) = 'DOTS                                    '
      OPTION(6) = 'ISOLINE + NUMBERS                       '
      OPTION(7) = 'ISOFIL  + NUMBERS                       '
      OPTION(8) = 'DOTS    + NUMBERS                       '
      OPTION(9) = 'highlight dots smallest 5 %             '
      OPTION(10)= 'highlight dots largest  5 %             '
      MAXOPT    = 10
      NWHAT2    = NDRAW(11)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .NE. NDRAW(11) ) KEY = 3
      NDRAW(11) = NWHAT2
   ELSE IF (NWHAT .EQ. 8) THEN
      EXP(1)     = 'MENU 11                                 '
      EXP(2)     = 'SHOW NODE ADMINISTRATION                '
      OPTION(1)  = 'NO NODE VALUES                          '
      OPTION(2)  = 'NODE NUMBERS                            '
      OPTION(3)  = 'NUMBER OF LINKS ATTACHED TO NODE        '
      OPTION(4)  = 'LINK NUMBERS BASED ON NODES             '
      OPTION(5)  = 'NODE CODES                              '
      OPTION(6)  = 'Vertical level ZK                    (m)'
      OPTION(7)  = 'Distance to land boundary               '
      OPTION(8)  = 'Erodable Lay. Thickn.                (m)'
      OPTION(9)  = 'Vorticity at netnodes              (1/s)'
      OPTION(10) = 'Netnode area BAN                    (m2)'
      OPTION(11) = 'Ship hull vertical level            (m )'
      MAXOPT     = 11
      NWHAT2     = NDRAW(8)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .NE. NDRAW(8) ) KEY = 3
      NDRAW(8)  = NWHAT2
      ! Set default display mode to numbers for nodenums/codes, etc.
      if (nwhat2 == 2 .or. nwhat2 == 3 .or. nwhat2 == 4 .or. nwhat2 == 5 .or. nwhat2 == 7) then
         ndraw(19) = 2
      elseif (ndraw(19) == 2) then ! Set back to default if current is 'numbers'.
         ndraw(19) = 4
      end if
      IF (NWHAT2 > 0) THEN
         CALL PARAMTEXT(OPTION(NWHAT2),1)
      ENDIF
   ELSE IF (NWHAT .EQ. 9) THEN
      EXP(1)    = 'MENU 9                                  '
      EXP(2)    = 'HOW TO DISPLAY THE ELEMENT ADMIN        '
      OPTION(1) = 'NO LINK VALUES                    ( )   '
      OPTION(2) = 'LINK NUMBERS                      ( )   '
      OPTION(3) = 'NODE NUMBERS BASED ON LINKS       ( )   '
      OPTION(4) = 'LINK ORTHOGONALITY COSPHI         ( )   '
      OPTION(5) = '                                  ( )   '
      OPTION(6) = 'LINK CODE LC, branch nr           ( )   '
      OPTION(7) = 'nr of links on branch             ( )   '
      OPTION(8) = '                                  ( )   '
      OPTION(9) = '                                  ( )   '
      OPTION(10)= 'LINK LENGHT                     (  m)   '
      OPTION(11)= 'LINK CODE KN(3,L)               (   )   '
      OPTION(12)= 'LINK, NR OF CONNECTED CELLS LNN (   )   '
      OPTION(13)= 'LINK, CONNECTED CELL NR 1   LNE1(   )   '
      OPTION(14)= 'LINK, CONNECTED CELL NR 2   LNE2(   )   '
      OPTION(15)= 'decrease in topology functional (   )   '
      OPTION(16)= 'smoothness indicator            (   )   '
      OPTION(17)= 'small flow link criterion, dx/(ba12) ( )'
      MAXOPT    = 17
      if ( jatrt.eq.1 ) then 
          MAXOPT    = 18
          if (ifrctypuni == 0) then 
              OPTION(18)= 'Roughness from trachytopes     (Chezy) '
          elseif (ifrctypuni == 1) then 
              OPTION(18)= 'Roughness from trachytopes   (Manning) '
          elseif ((ifrctypuni == 2) .or. (ifrctypuni == 3)) then 
              OPTION(18)= 'Roughness from trachytopes   (WhitCol) '
          else
              OPTION(18)= 'Roughness from trachytopes      (    ) '
          end if
      end if     
      NWHAT2    = NDRAW(7)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .NE. NDRAW(7) ) KEY = 3
      NDRAW(7)  = NWHAT2
      ! Prepare data
      if ( nwhat2==4 .or. nwhat2==15) then
         if ( .not.allocated(xz) ) then
            call findcells(0)
         end if
      end if
      ! Set default display mode to numbers for linknums/codes, etc.
      if (nwhat2 == 2 .or. nwhat2 == 3 .or. nwhat2 == 11 .or. nwhat2 == 12 .or. nwhat2 == 13 .or. nwhat2 == 14 .or. nwhat2 == 15) then
         ndraw(11) = 2
      elseif (ndraw(11) == 2 .and. (nwhat2 == 4 .or. nwhat2 == 10)) then ! Set to dots for real values if current is numbers.
         ndraw(11) = 4
      end if
      IF (NWHAT2 .NE. 0) THEN
         CALL PARAMTEXT(OPTION(NWHAT2),2)
      ENDIF
      IF (NWHAT2 == 6 .or. NWHAT2 == 7) CALL SETBRANCH_LC(ium)
   ELSE IF (NWHAT .EQ. 10) THEN ! flow nodes
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW flow nodes                         '
      option    = ' '
      OPTION(1) = 'NO                                      '  
      OPTION(2) = 'Waterlevel                          (m )'   ! options for nodes , znod, ndraw(28)
      OPTION(3) = 'Bedlevel                            (m )'
      OPTION(4) = 'Cell area                           (m2)'
      OPTION(5) = 'Free surface area                   (m2)'
      OPTION(6) = 'Volume                              (m3)'
      OPTION(7) = 'Waterdepth                          (m )'
      OPTION(8) = 'Node velocity magnitude            (m/s)'
      OPTION(9) = 'Node x-velocity component          (m/s)'
      OPTION(10)= 'Node y-velocity component          (m/s)'
      OPTION(11)= 'Salinity                           (ppt)'
      OPTION(12)= 'Temperature                       (degC)'
      if (jased > 0) then 
      OPTION(13)= 'Sediment concentration           (kg/m3)'
      else if (jagrw > 0) then 
      OPTION(13)= 'Ground water level                   (m)'
      else
      OPTION(13)= '                                        '
      endif
      OPTION(14)= 'Froude nr                          (   )'
      OPTION(15)= 'Node nr                            (   )'
      OPTION(16)= 'Nr of links attached to this node  (   )'
      OPTION(17)= 'Kcs                                (   )'
      OPTION(18)= 'Squ sum of q out of cell          (m3/s)'
      OPTION(19)= 'Sqi sum of q in to  cell          (m3/s)'
      OPTION(20)= 'Sqi-squ                           (m3/s)'
      OPTION(21)= 'QW vertical interface velocity    (m /s)'
      OPTION(22)= 'Equilibrium Transport conc.      (kg/m3)'
      OPTION(23)= 'Qin                               (m3/s)'
      OPTION(24)= 'Erodable Lay. Thickn.                (m)'
      OPTION(25)= 'nr of layers                      (    )'
      OPTION(26)= 'vol1/squ                          (s)   '
      OPTION(27)= 'vicwws                            (m2/s)'
      OPTION(28)= 'cg=red, substi=white                    '
      OPTION(29)= 'Tidal potential                  (m2/s2)'
      OPTION(30)= 'Timestep for jaautotimestep >= 1 (   s )'
      OPTION(31)= 'Patm                             (N/m2 )'
      OPTION(32)= 'Numlimdt                                ' ! Velocity head                      (m  )'
      OPTION(33)= 'Total head                         (m  )'
      OPTION(34)= 'Volume error                       (m3 )'

      OPTION(35)= 'Rho                              (kg/m3)'
      OPTION(36)= 'cflmx*vol1(k)/squ(k)               (   )'
      
      if (soiltempthick == 0d0) then 
         OPTION(37)= 'salmase                            (   )'
      else
         OPTION(37)= 'soiltemp                           ( C )'
      endif
      
      OPTION(38)= 'Layer thickness                    (m  )'

      OPTION(39)= 'Taus 2D                           (N/m2)'
      OPTION(40)= 'Rainfall                        (mm/day)'
      
      OPTION(41)= 'Humidity                             (%)'
      OPTION(42)= 'Air temperature                      (C)'
      OPTION(43)= 'Cloudiness                           (%)'
      OPTION(44)= 'Solar radiation                   (W/m2)'
      
      OPTION(45)= 'Constituents                            '

      OPTION(46)= 'turkinws                                '
         
      if (jagrw > 0) then 
         OPTION(47)= 'ground water pressure                (m)'
      endif
         
      if (nonlin == 2) then
         OPTION(48)= 'a1m                                 (m2)'
      endif
         
      if (nshiptxy > 0) then 
         OPTION(49)= 'zspc                                 (m)'
      endif
      if (janudge > 0) then 
         OPTION(50)= 'Nudge time                           (s)'
      else if (nshiptxy > 0) then 
         OPTION(50)= 'v1ship                              (m3)'
      endif
      numopt=50
      numoptwav=-999
      numoptsf=-999
      numoptsed=-999
      if ( jawave.gt.0 ) then
         numopt = numopt+1
         numoptwav = numopt
         OPTION(numoptwav)= 'Wave parameters                         '
      end  if
      
      if ( jasecflow.gt.0 ) then
         numopt = numopt+1
         numoptsf = numopt
         OPTION(numoptsf)= 'Spiral flow parameters                  '
      end  if
      
      if ( stm_included ) then
         numopt = numopt + 1
         numoptsed = numopt
         OPTION(numoptsed)= 'Sediment transport parameters           '
      end  if

      MAXOPT    = numopt !53
      
      NWHAT2    = NDRAW(28)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      ! Set default display mode to numbers for nodenums/codes, etc.
      if (ndraw(19) == 1) then
         if (nwhat2 == 15 .or. nwhat2 == 16) then
            ndraw(19) = 2
         else                        ! Set back to default if current is 'no'
            ndraw(19) = 4
         endif
      end if
      IF (NWHAT2 > 1) THEN
          CALL PARAMTEXT(OPTION(NWHAT2),1)
      ENDIF

      IF (NWHAT2 .NE. NDRAW(28) ) KEY = 3
      NDRAW(28) = NWHAT2
      
      if (ndraw(28) == 24 .and. jaceneqtr == 2) then  ! transfer to net node drawing
          ndraw(28) = 0
          ndraw(8) = 8
          
      else if ( ndraw(28).eq.45 .and. NUMCONST.gt.0 ) then
         if ( NUMCONST.gt.0 ) then
            ndraw(28) = 1
            nwhat = 37
            goto 1234
         else
            ndraw(28) = 0
         end if

      else if (ndraw(28).eq.numoptwav) then
         if (jawave>0) then
            ndraw(28) = 1
            nwhat     = 42 ! WAVE submenu
            goto 1234
         else
            ndraw(28) = 0
         end if
      else if (ndraw(28).eq.numoptsf) then
         if (jasecflow>0) then
            ndraw(28) = 1
            nwhat     = 43 ! SECF submenu
            goto 1234
         else
            ndraw(28) = 0
         end if
      else if (ndraw(28).eq.numoptsed) then
         if (stm_included) then
            ndraw(28) = 1
            nwhat     = 45 ! STM flow nodes submenu
            goto 1234
         else
            ndraw(28) = 0
         end if
      end if

   ELSE IF (NWHAT .EQ. 11) THEN ! flow links
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW flow links                         '
      OPTION(1) = 'NO                                      '
      OPTION(2) = 'abs(u1)                            (m/s)'   ! options for links, zlin, ndraw(29)
      OPTION(3) = 'q1-specific                       (m2/s)'
      OPTION(4) = 'q1                                (m3/s)'
      OPTION(5) = 'au                                  (m2)'
      OPTION(6) = 'hu                                   (m)'
      OPTION(7) = 'user defined friction coefficient frcu  '
      OPTION(8) = 'dx                                      '
      OPTION(9) = 'wu                                      '
      OPTION(10)= 'bob         nd1                         '
      OPTION(11)= 'bob         nd2                         '
      OPTION(12)= 'kcu                                     '
      OPTION(13)= 'horizontal eddy viscosity coeff.  (m2/s)'
      OPTION(14)= 'teta (L)                             ( )'
      OPTION(15)= '                                        '
      OPTION(16)= 'u1                                 (m/s)'
      OPTION(17)= 'adve                              (m/s2)'
      OPTION(18)= 'advi                               (1/s)'
      OPTION(19)= 'FU                                      '
      OPTION(20)= 'RU                                      '
      OPTION(21)= 'suu                               (m/s2)'
      if (javeg == 0) then 
      OPTION(22)= 'aifu ()                                 '
      OPTION(23)= 'Local waterlevel slope               ( )'
      OPTION(24)= 'cfu=g/(HC2)                        (   )'
      else
      OPTION(22)= 'Plant diameter                       (m)'
      OPTION(23)= 'Plant density                     (1/m2)'
      OPTION(24)= 'Stem  height                       ( m )'
      endif
      OPTION(25)= 'wind x                             (m/s)'
      OPTION(26)= 'wind y                             (m/s)'
      OPTION(27)= 'windstress                        (N/m2)'
      OPTION(28)= 'cosphiu , link orthogonality          ()'
      OPTION(29)= 'link nr                                 '
      OPTION(30)= 'tangential velocity                (m/s)'
      OPTION(31)= 'Fu                                 (1/s)'
      OPTION(32)= 'Ru                                 (m/s)'
      OPTION(33)= 'iadv                               (m/s)'
      OPTION(34)= 'plotlin                          (     )'
      OPTION(35)= 'node nr 1, ln(1,L)               (     )'
      OPTION(36)= 'node nr 2, ln(2,L)               (     )'
      OPTION(37)= 'Vorticity                        ( 1/s )'
      OPTION(38)= 'Timestep if jaautotimestep == 2  ( s   )'
      OPTION(39)= 'bottom slope (bl2-bl1)/dx        (     )'
      OPTION(40)= 'IFRCUTP friction type            (     )'
      OPTION(41)= 'turkin0                          (m2/s2)'
      OPTION(42)= 'tureps0                          (1/s  )'
      OPTION(43)= 'vicwwu                           (m2/s )'
      OPTION(44)= 'ustb                             (     )'
      if (jawind > 0) then 
         OPTION(45)= 'ustw                             (m/s  )'
      else 
         OPTION(45)= 'womegu                           (m/s  )'
      endif
      OPTION(46)= 'Layer Thickness at u             (m    )'
      OPTION(47)= 'Linear friction coefficient      (m/s  )'
      OPTION(48)= '                                        '
      OPTION(49)= 'Number of active layers          (     )'
      OPTION(50)= 'Maximum nr of layers             (     )'
      OPTION(51)= 'Lbot                             (     )'
      OPTION(52)= 'Ttop                             (     )'
      numopt = 52
      if ( stm_included ) then
         numopt = numopt+1
         numoptsed = numopt
         OPTION(numopt)= 'Sediment transport parameters           '
      endif
      
      MAXOPT    = numopt
      NWHAT2    = NDRAW(29)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .NE. NDRAW(29) ) KEY = 3
      ! Set default display mode to numbers for linknums, etc.
      if (nwhat2 == 29) then
         ndraw(11) = 2
      end if
      IF (NWHAT2 > 1) THEN
         CALL PARAMTEXT(OPTION(NWHAT2),2)
      ENDIF
      NDRAW(29) = NWHAT2
      if (ndraw(29).eq.numoptsed) then
         if (stm_included) then
            ndraw(29) = 1
            nwhat     = 44 ! STM flow links submenu
            goto 1234
         else
            ndraw(28) = 0
         end if
      end if
   ELSE IF (NWHAT .EQ. 12) THEN ! show values at net cell
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW net cells                          '
      OPTION(1) = 'Do not show values at net cells         '
      OPTION(2) = 'Cell numbers                            '
      OPTION(3) = 'Show aspect ratio, <=1 by definition    '
      OPTION(4) = 'Show orientation vectors                '
      OPTION(5) = 'Show aspect ratio plus vectors          '
      OPTION(6) = 'Show cell size                      (m2)'
      OPTION(7) = 'Show cell coarsening information        '
      OPTION(8) = 'Show cell type: tri, quad, penta, hexa  '
      OPTION(9) = 'Normalised circumcentre - gravitycentre '
      OPTION(10)= 'Cell slope                           ( )'
      OPTION(11)= '                                        '
      OPTION(12)= 'Smallest angle                     (deg)'
      OPTION(13)= 'Largest  angle                     (deg)'

      OPTION(14)= '                                        '
      OPTION(15)= 'Partitioning info, domain number        '
      OPTION(16)= 'Partitioning info, number of cells      '
      OPTION(17)= 'Partitioning info, ghostlevels          '
      OPTION(18)= 'Partitioning info, cell-based ghostlevls'
      OPTION(19)= 'Partitioning info, node-based ghostlevls'
      OPTION(20)= 'Partitioning info, global cell number   '

      MAXOPT    = 20
      NWHAT2    = NDRAW(33)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP )
      IF (NWHAT2 .NE. NDRAW(33) ) KEY = 3
      NDRAW(33) = NWHAT2
      IF (NWHAT2 > 1) THEN
          CALL PARAMTEXT(OPTION(NWHAT2),1)
      ENDIF

      ! Set default display mode
      if (nwhat2 == 3 .or. nwhat2 == 5) then
         ndraw(19) = 3
      elseif ( nwhat2 == 2 .or. nwhat2 == 6 .or. nwhat2 == 7 ) then
         ndraw(19) = 2
      elseif ( nwhat2 == 15 .or. nwhat == 16 ) then
         ndraw(19) = 5
      end if
   ELSE IF (NWHAT .EQ. 13) THEN ! show values at cell corners
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW flow links                         '
      OPTION(1) = 'Do NOt show values at flow cell corners '
      OPTION(2) = 'Show x velocity comp                    '
      OPTION(3) = 'Show y velocity comp                    '
      OPTION(4) = 'Show velocity magnitude                 '
      OPTION(5) = 'Show corner velocity vectors            '
      MAXOPT    = 5
      NWHAT2    = NDRAW(31)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP )
      IF (NWHAT2 .NE. NDRAW(31) ) KEY = 3
      NDRAW(31) = NWHAT2
   ELSE IF (NWHAT .EQ. 14) THEN ! show all flow white line
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW flow links                         '
      OPTION(1) = 'Do NOt show flow links                  '
      OPTION(2) = 'Show All flow links                     '
      OPTION(3) = 'Show All flow link directions           '
      OPTION(4) = 'Show All dry flow links                 '
      MAXOPT    = 4
      NWHAT2    = NDRAW(30)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .NE. NDRAW(30) ) KEY = 3
      NDRAW(30) = NWHAT2
   ELSE IF (NWHAT .EQ. 15) THEN
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW vectors YES/NO                     '
      OPTION(1) = 'NO                                      '
      OPTION(2) = 'Velocity                                '
      OPTION(3) = 'Discharge                               '
      OPTION(4) = 'Momentum transport                      '
      OPTION(5) = 'Wind                                    '
      OPTION(6) = 'Wind arcuv                              '
      OPTION(7) = 'Atmospheric pressure arc                '
      OPTION(8) = 'Wind spiderweb                          '
      OPTION(9) = 'Primitive velocity u1                   '
      MAXOPT    = 9
      NWHAT2    = NDRAW(13)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .GE. 1) THEN
         NDRAW(13) = NWHAT2
      ENDIF
      key = 3
   ELSE IF (NWHAT .EQ. 16) THEN
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW observation stations               '
      OPTION(1) = 'NO observation stations                 '
      OPTION(2) = 'Cross                                   '
      OPTION(3) = 'Cross + name                            '
      OPTION(4) = 'Polyfil                                 '
      OPTION(5) = 'Polyfil + name                          '
      OPTION(6) = 'Cross   + waterlevel (m)                '
      OPTION(7) = 'Cross   + waterdepth (m)                '
      OPTION(8) = 'Cross   + velocity magnitudes (m/s)     '
      OPTION(9) = 'Cross   + znod                          '
      OPTION(10) = 'Cross   + temperatures surface + bed   '
      
      MAXOPT    = 10
      NWHAT2    = NDRAWobs
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .GE. 1) THEN
         IF (NWHAT2 .NE. NDRAWobs ) KEY = 3
         NDRAWobs = NWHAT2
      ENDIF
   ELSE IF (NWHAT .EQ. 17) THEN
      EXP(1)     = 'MENU                                    '
      EXP(2)     = 'SHOW CROSS SECTIONS                     '
      OPTION(1)  = 'NO cross sections                       '
      OPTION(2)  = 'Line only                               '
      OPTION(3)  = 'Line dir                                '
      OPTION(4)  = 'Line dir name                           '
      OPTION(5)  = 'Line dir discharge             (m3/s)   '
      OPTION(6)  = 'Line dir flow area             (m2)     '
      OPTION(7)  = 'Line dir ave. velocity         (m/s     '
      OPTION(8)  = 'Line dir ave. waterlevel       (m)      '
      OPTION(9)  = 'Line dir ave. waterdepth       (m)      '
      OPTION(10) = 'Line dir integrated transport 1(c*m3/sm)'
      OPTION(11) = 'Line dir integrated transport 2(c*m3/sm)'
      
      MAXOPT    = 9
      if (numconst >= 1) MAXOPT = 10
      if (numconst >= 2) MAXOPT = 11
      NWHAT2    = ndrawcrosssections
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .GE. 1) THEN
         IF (NWHAT2 .NE. NDRAWcrosssections)  KEY = 3
         NDRAWcrosssections = NWHAT2
      ENDIF
   ELSE IF (NWHAT .EQ. 18) THEN
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW THIN DAMS YES/NO                   '
      OPTION(1) = 'NO thin dams                            '
      OPTION(2) = 'Thin dam polylines                      '
      OPTION(3) = 'Thin dam net links                      '
      MAXOPT    = 3
      NWHAT2    = ndrawThinDams+1
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      ndrawThinDams = NWHAT2-1
      KEY = 3
   ELSE IF (NWHAT .EQ. 19) THEN
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW FIXED WEIRS YES/NO                 '
      OPTION(1) = 'NO fixed weirs                           '
      OPTION(2) = 'Fixed weir flow links                    '
      OPTION(3) = 'Fixed weir flow links + heights          '
      OPTION(4) = 'Fixed weir flow links isocol             '
      OPTION(5) = 'Fixed weir flow links + heights isocol   '
      OPTION(6) = 'Fixed weir only if above water surface   '
      MAXOPT    = 6
      NWHAT2    = ndrawFixedWeirs+1
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      ndrawFixedWeirs = NWHAT2-1
      KEY = 3
   ELSE IF (NWHAT .EQ. 20) THEN
!     Lege regel
   ELSE IF (NWHAT .EQ. 21) THEN
      EXP(1)    = 'MENU 12                                 '
      EXP(2)    = 'ISOSCALE YES OR NO                      '
      OPTION(1) = 'ISOSCALE  NODES ON                      '
      OPTION(2) = 'ISOSCALE  LINKS ON                      '
      OPTION(3) = 'ISOSCALES NODES AND LINKS ON            '
      OPTION(4) = 'ISOSCALES OFF                           '
      MAXOPT    = 4
      NWHAT2    = NDRAW(12)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .GE. 1) THEN
         IF (NWHAT2 .NE. NDRAW(12) ) KEY = 3
         NDRAW(12) = NWHAT2
      ENDIF
   ELSE IF (NWHAT .EQ. 22) THEN
      CALL SETCOLTABFILE('*.hls               ',0)
      KEY = 3
   ELSE IF (NWHAT .EQ. 23) THEN
      CALL CHANGEISOPARAMETERS()
      KEY = 3
   ELSE IF (NWHAT .EQ. 24) THEN
      CALL CHANGEDISPLAYPARAMETERS()
      KEY = 3
   ELSE IF (NWHAT .EQ. 25) THEN
      CALL TEXTPARAMETERS()
      KEY = 3
   ELSE IF (NWHAT .EQ. 26) THEN
!     Lege regel
   ELSE IF (NWHAT .EQ. 27) THEN
      KEY   = 90

      inquire (file = trim(md_ident)//'.x1y1x2' , exist = jawel )
      if (jawel) then
         call oldfil(minp, trim(md_ident)//'.x1y1x2')
         read (minp,*) x1,y1,x2
         call doclose(minp)
         call setwy(x1,y1,x2,y2)
         key = 3
      else
         NPUTZ = 2
         CALL ZOOM3(KEY,NPUTZ)
      endif

   ELSE IF (NWHAT .EQ. 28) THEN
      KEY = 3
   ELSE IF (NWHAT .EQ. 29) THEN
      NDRAW(10) = 1
      KEY = 3
   ELSE IF (NWHAT .EQ. 30) THEN
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW ORTHO YES/NO                       '
      OPTION(1) = 'NO rai                                  '
      OPTION(2) = 'small rai                               '
      OPTION(3) = 'somewhat larger rai                     '
      OPTION(4) = 'larger rai                              '
      OPTION(5) = 'velocity prof                           '
      MAXOPT    = 5
      NWHAT2    = NDRAW(18)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      NDRAW(18) = NWHAT2
      KEY = 3
   ELSE IF (NWHAT .EQ. 31) THEN
      NDRAW(9) = 2
      KEY = 3
   ELSE IF (NWHAT .EQ. 32) THEN
      EXP(1)    = 'MENU 9                                  '
      EXP(2)    = 'HOW TO DISPLAY SAMPLE POINTS            '
      OPTION(1) = 'NO SAMPLE POINTS                        '
      OPTION(2) = 'COLOURED DOTS                           '
      OPTION(3) = 'COLOURED DOTS AND CIRCLES               '
      OPTION(4) = 'SMALL POINTS                            '
      OPTION(5) = 'NUMBERS ISOCOLOUR                       '
      OPTION(6) = 'NUMBERS MONOCOLOUR                      '
      OPTION(7) = 'NUMBERS ISOCOLOUR + COLOURED DOTS       '
      OPTION(8) = 'COLOURED SQUARES                        '
      MAXOPT    = 8
      NWHAT2    = max(0,NDRAW(32)) + 1
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .GE. 1) THEN
         IF (NWHAT2 - 1 .NE. NDRAW(32) ) KEY = 3
         NDRAW(32) = NWHAT2 - 1
      ENDIF
   ELSE IF (NWHAT .EQ. 33) THEN
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW ORTHO YES/NO                       '
      OPTION(1) = 'YO BITMAP                               '
      OPTION(2) = 'NO BITMAP                               '
      MAXOPT    = 2
      NWHAT2    = NDRAW(26)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      NDRAW(26) = NWHAT2
      KEY = 3
    ELSE IF (NWHAT .EQ. 34) THEN
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW ORTHO YES/NO                       '
      OPTION(1) = 'No Banf                                 '
      OPTION(2) = 'Equilibrium concentration               '
      OPTION(3) = 'Banf flux (ceq - c)                     '
      OPTION(4) = 'Netnode  nr                             '
      OPTION(5) = 'Flownode nr                             '
      OPTION(6) = 'Ban      nr                             '

      MAXOPT    = 6
      NWHAT2    = NDRAW(34)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      NDRAW(34) = NWHAT2
      KEY = 3
    ELSE IF (NWHAT .EQ. 35) THEN
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW ORTHO YES/NO                       '
      OPTION(1) = 'No Polygon                              '
      OPTION(2) = 'Polygon red + white dots                '
      OPTION(3) = 'Polygon isocolour ZPL values            '
      OPTION(4) = 'Polygon + numbers ZPL values            '
      OPTION(5) = 'Polygon + numbers Left  Sillheights     '
      OPTION(6) = 'Polygon + numbers Right Sillheights     '
      OPTION(7) = 'Polygon isocolour Left/Right levels     '
      OPTION(8) = 'Polygon index nrs                       '

      MAXOPT    = 8
      NWHAT2    = NDRAWPOL
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      NDRAWPOL  = NWHAT2
      KEY = 3
    ELSE IF (NWHAT .EQ. 36) THEN
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW ORTHO YES/NO                       '
      OPTION(1) = 'No curvilinear grid                     '
      OPTION(2) = 'Lines                                   '
      OPTION(3) = '                                        '
      OPTION(4) = '                                        '
      OPTION(5) = 'Nr of netcells in gridcells (partition).'
      MAXOPT    = 5
      NWHAT2    = NDRAW(38)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      NDRAW(38) = NWHAT2
      if (ndraw(38) == 5) then
         call teknumnetcells(1)
      endif
      KEY = 3
      
   ELSE IF (NWHAT .EQ. 37) THEN     ! constituents
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW CONSTITUENTS YES/NO                '
      OPTION(1) = 'NEW TRACER                              '
      
      do i=1,NUMCONST
         OPTION(i+1)=const_names(i)
      end do
      MAXOPT    = 1+NUMCONST
      NWHAT2    = NDRAW(28)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      NDRAW(28) = NWHAT2
      KEY = 3
      iconst_cur = max(nwhat2-1,0)
      
      IF (NWHAT2 == 1) THEN  ! new tracer
         call add_tracer('', iconst_cur)
         nwhat2 = iconst_cur+1
         option(nwhat2) = const_names(iconst_cur)
      ENDIF
      NDRAW(28) = 45
      if ( nwhat2.gt.0 ) then
         CALL PARAMTEXT(option(nwhat2),1)
      end if
      
   ELSE IF (NWHAT .EQ. 38) THEN
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW sorsin                             '
      OPTION(1) = 'No Sources & Sinks                      '
      OPTION(2) = 'black (sin) + white (sor) dots          '
      OPTION(3) = 'idem + Names                            '
      OPTION(4) = 'idem + discharges  (m3/s)               '
      OPTION(5) = 'idem + salinity    (ppt)                '
      OPTION(6) = 'idem + temperature (degC)               '

      MAXOPT    = 6
      CALL MENUV3(Ndraw(41),OPTION,MAXOPT,EXP,MAXEXP)
      KEY = 3
      
   ELSE IF (NWHAT .EQ. 39 ) THEN
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW DOTS YES/NO                        '
      OPTION(1) = 'DO NOT SHOW DOTS                        '
      OPTION(2) = 'SHOW DOTS                               '
      OPTION(3) = 'CLEAR DOTS                              '
      MAXOPT    = 3
      NWHAT2 = NDRAWDOTS
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      if ( NWHAT2.eq.3 ) then
         call deldots()
      else
         NDRAWDOTS = NWHAT2
      end if
      KEY = 3
   else if (NWHAT == 40) then
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW STRUCTURES YES/NO                '
      OPTION(1) = 'DO NOT SHOW STRUCTURES                '
      OPTION(2) = 'SHOW STRUCTURES SYMBOLS ONLY          '
      OPTION(3) = 'SHOW STRUCTURES SYMBOLS AND IDS       '
      MAXOPT    = 3
      NWHAT2    = 1
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      ndrawStructures = NWHAT2
      KEY = 3
      
   ELSE IF (NWHAT .EQ. 41 ) THEN
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW PARTICLES YES/NO                   '
      OPTION(1) = 'DO NOT SHOW PARTICLES                   '
      OPTION(2) = 'SHOW PARTICLES                          '
      MAXOPT    = 2
      NWHAT2 = NDRAWPART
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      NDRAWPART = NWHAT2
      KEY = 3
   ELSE IF (NWHAT.EQ.42) THEN          ! wave stuff     
      EXP(1)     = 'MENU                                    '
      EXP(2)     = 'SHOW WAVEPARS YES/NO                    '
  

      if (jawave == 1 .or. jawave == 2) then 
      OPTION(1)  = 'RMS wave height  (~ 0.7*Hsig)        (m)'
      OPTION(2)  = 'Wave length                          (m)'
      OPTION(3)  = 'Peak wave period                     (s)'
      OPTION(4)  = 'Orbital velocity at bed            (m/s)'
      OPTION(5)  = 'Ustar(w)                           (m/s)'
      OPTION(6)  = 'Ustar(w+c)                         (m/s)'
      OPTION(7)  = 'Taus(w+c)                         (N/m2)'
      OPTION(8)  = 'Ustokes                            (m/s)'
      OPTION(9)  = 'Fetchlength                          (m)'
      OPTION(10) = 'Fetchdepth                           (m)'
      MAXOPT     = 10
      else
      OPTION(1)  = 'RMS wave height                      (m)'
      OPTION(2)  = 'Peak waveperiod                      (s)'
      OPTION(3)  = 'Total shear stress (c+w)          (N/m2)'
      OPTION(4)  = 'Wave force, magnitude             (N/m2)'
      OPTION(5)  = 'Ustokes, magnitude                 (m/s)'
      OPTION(6)  = 'Wave force, X component              (N)'
      OPTION(7)  = 'Wave force, Y component              (N)'
      OPTION(8)  = 'Bottom stress, X component        (N/m2)'
      OPTION(9)  = 'Bottom stress, Y component        (N/m2)'
      OPTION(10) = 'Stokes drift, X component          (m/s)'
      OPTION(11) = 'Stokes drift, Y component          (m/s)'
      OPTION(12) = 'Wave energy                          (J)'
      OPTION(13) = 'Roller energy                        (J)'
      OPTION(14) = 'RMS orbital velocity               (m/s)'
      OPTION(15) = 'Wave dissipation                  (W/m2)'
      OPTION(16) = 'Roller dissipation                (W/m2)'
      OPTION(17) = 'Bulk roller energy                   (J)'
      OPTION(18) = 'Radiation stress, X component     (N/m2)'
      OPTION(19) = 'Radiation stress, Y component     (N/m2)'
      OPTION(20) = 'Radiation stress, XY component    (N/m2)'
      OPTION(21) = 'Wave number                      (rad/m)'
      OPTION(22) = 'Wave direction              (deg from N)'
      OPTION(23) = 'Depth gradient, X component          (-)'
      OPTION(24) = 'Depth gradient, Y component          (-)'
      OPTION(25) = 'Wind source term             (J/rad/m/s)'
      OPTION(26) = 'Wave frequency                   (rad/s)'
      OPTION(27) = 'Wave group speed            (m/s in bin)'
      OPTION(28)= ''
      OPTION(29) = 'egradcg                       (J/m/s) '
      OPTION(30) = 'SwT                             (s/s) '
      OPTION(31) = 'SwE                          (J/m2/s) '
      OPTION(32) = 'horadvec                              '      
      OPTION(33) = 'horadvec2                             '
      OPTION(34) = 'ma                                    ' 
      MAXOPT     = 34
      endif     
      
      NWHAT2     = NDRAW(28)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      NDRAW(28) = NWHAT2
      KEY = 3
      waveparopt = nwhat2
      if ( nwhat2.gt.0 ) then
         CALL PARAMTEXT(option(nwhat2),1)
      end if
      NDRAW(28) = numoptwav
      
   ELSE IF (NWHAT .EQ. 43) THEN     ! Spiral flow parameters 
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW Spiral Flow Parameters YES/NO      '
      OPTION(1) = 'Streamlines curvature              (1/m)'
      OPTION(2) = 'Spiral flow intensity              (m/s)'
      OPTION(3) = 'Dispersion stress by spiral flow  (m/s2)'
      
      MAXOPT    = 3
      NWHAT2    = NDRAW(28)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      NDRAW(28) = NWHAT2
      KEY = 3
      ispirparopt = nwhat2
      
      NDRAW(28) = numoptsf
      if ( nwhat2.gt.0 ) then
         CALL PARAMTEXT(option(nwhat2),1)
      end if
      
   ELSE IF (NWHAT .EQ. 44) THEN     ! Sed trsp on flow links 
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW Mophology Parameters YES/NO        '
      OPTION(1) = 'Curr. rel. bedload transport    (kg/s/m)'
      OPTION(2) = 'Curr. rel. suspended transport  (kg/s/m)'
      OPTION(3) = 'Wave  rel. bedload transport    (kg/s/m)'
      OPTION(4) = 'Wave  rel. susp. transport      (kg/s/m)'
      OPTION(5) = 'Total transport                 (kg/s/m)'
      
      MAXOPT    = 5
      NWHAT2    = NDRAW(29)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      NDRAW(29) = NWHAT2
      KEY = 3
      sedparopt = nwhat2
      
      NDRAW(29) = numoptsed
      if ( nwhat2.gt.0 ) then
         CALL PARAMTEXT(option(nwhat2),2)
      end if

   ELSE IF (NWHAT .EQ. 45) THEN     ! Sed trsp on flow nodes 
      EXP(1)    = 'MENU                                    '
      EXP(2)    = 'SHOW Mophology Parameters YES/NO        '
      OPTION(1) = 'Bottom level change in last timestep (m)'
      OPTION(2) = 'Sediment source adv. eq.       (kg/m3/s)'
      OPTION(3) = 'Sediment sink   adv. eq.           (1/s)'
      !OPTION(4) = 'Wave  rel. susp. transport      (kg/s/m)'
      !OPTION(5) = 'Total transport                 (kg/s/m)'
      
      MAXOPT    = 3
      NWHAT2    = NDRAW(28)
      CALL MENUV3(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      NDRAW(28) = NWHAT2
      KEY = 3
      sedparopt = nwhat2
      
      NDRAW(28) = numoptsed
      if ( nwhat2.gt.0 ) then
         CALL PARAMTEXT(option(nwhat2),1)
      end if
   ENDIF
   RETURN
   END SUBROUTINE NDISPLAY



   SUBROUTINE changenetworkPARAMETERS()
   use network_data
   use unstruc_display
   use m_ec_triangle
   use m_missing
   use unstruc_version_module, only : unstruc_company, unstruc_program
   use unstruc_model, only: md_dryptsfile

   implicit none
   integer :: i
   integer :: ifexit
   integer :: ifinit
   integer :: ih
   integer :: il
   integer :: imp
   integer :: inp
   integer :: ir
   integer :: iw
   integer :: ixp
   integer :: iyp
   integer :: key
   integer :: nbut
   integer :: nlevel
   integer :: numfldactual
   integer :: numparactual
   integer :: jins_old  ! netcell administration out of date if jins changes
   integer :: iselect, minp
   CHARACTER*128 select(3)

   integer, parameter :: NUMPAR = 21, NUMFLD = 2*NUMPAR
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line

   jins_old = jins

   NLEVEL    = 4
   OPTION(1) = 'SELECT INSIDE POLYGON (1/0), 1 = INSIDE ' ; IT( 1*2)  = 2
   !OPTION(2) = 'TRIANGLEMINANGLE                       ' ; IT( 2*2)  = 6
   OPTION(2) = 'jadelnetlinktyp                         ' ; IT( 2*2)  = 2
   OPTION(3) = 'TRIANGLEMAXANGLE                        ' ; IT( 3*2)  = 6
   OPTION(4) = 'TRIANGLESIZEFACTOR, MAX.INSIDE/ AV.EDGE ' ; IT( 4*2)  = 6
   OPTION(5) = 'limit center; 1.0:in cell <-> 0.0:on c/g' ; IT( 5*2)  = 6
   OPTION(6 )= 'cosphiutrsh in geominit (good orhto)    ' ; IT( 6*2)  = 6
   OPTION(7 )= 'remove small links       0.0->          ' ; IT( 7*2)  = 6
   OPTION(8 )= 'TIME CONSUMING NETWORK CHECKS YES/NO 1/0' ; IT( 8*2)  = 2
   OPTION(9 )= 'NR OF SMOOTH. ITER. IN COURANT NETWORK  ' ; IT( 9*2)  = 2
   OPTION(10)= 'SMALLEST CELLSIZE IN COURANT NETWORK    ' ; IT(10*2)  = 6
   OPTION(11)= 'REMOVE SMALL TRIANGLES, TRIAREAREMFRAC  ' ; IT(11*2)  = 6
   OPTION(12)= 'REFINE NETWORK (QUADS) DIRECTION: 0,-1,1' ; IT(12*2)  = 2
   OPTION(13)= 'Merge nodes closer than tooclose (m)    ' ; IT(13*2)  = 6
   OPTION(14)= 'Connect 1D end nodes to branch if closer' ; IT(14*2)  = 6
   OPTION(15)= 'Uniform DX in copy landb to 1D netw     ' ; IT(15*2)  = 6
   OPTION(16)= 'snap-to-landbdy tolerance, netboundary  ' ; IT(16*2)  = 6
   OPTION(17)= 'snap-to-landbdy tolerance, inner network' ; IT(17*2)  = 6
   OPTION(18)= 'max nr of faces allowed in removesmallfl' ; IT(18*2)  = 2
!   OPTION(19)= 'dry/illegal/cutcells file (*.pol, *.lst)' ; IT(19*2)  = 4
   if ( len_trim(md_dryptsfile).eq.0 ) then
      OPTION(19) = 'DRY CELL FILE (none)'
   else
      OPTION(19) = 'DRY CELL FILE (' // trim(md_dryptsfile(1:min(len_trim(md_dryptsfile),25))) // ')'
   end if
   IT(19*2) = 4
   OPTION(20)= '1D2D link generation algorithm          ' ; IT(20*2)  = 2
   OPTION(21)= 'Lateral algorithm search radius         ' ; IT(21*2)  = 6



!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6
   HELPM (1) = &
   '1=inside, 0 = outside polygon (TO BE RESET AFTER USE)       '
   HELPM (2) = &
   '0 = delete nodes, > 0 = linktypkn3 to delete                '
   HELPM (3) = &
   '                                                            '
   HELPM (4) = &
   'MAX. INSIDE TRIANGLE SIZE / AVERAGE SIZE ON POLYGON         '
   HELPM (5 )= &
   '                                                            '
   ! 'in geominit, 1.0=inside, on edge ,  0.9=inside close to edge'

   HELPM (6) = &
   'No flow model created if cosphiu > cosphiutrsh              '

   HELPM (7 )= &
   '0.0 = remove no links, 0.1=remove links < 0.1 sqrt(baL+baR) '
   HELPM (8 )= &
   '                                                            '
   HELPM (9 )= &
   'NR OF SMOOTH. ITERATIONS IN COURANT NETWORK, SAMPLES RQUIRED'
   HELPM (10)= &
   'SMALLEST CELLSIZE IN COURANT NETWORK, SAMPLES REQUIRED      '
   HELPM (11)= &
   'SMALL TRIANGLE REMOVED IF TRIAREA < AV. ADJACENT AREAS      '
   HELPM (12)= &
   '0=BOTH DIRECTIONS, -1 = ONLY THIS, 1 = ONLY THAT            '
   HELPM (13)= &
   'Used in merge nodes on top of each other                    '
   HELPM (14)= &
   'than xx (m) to branch node, used in mergenodesontop         '
   HELPM (15)= &
   'used in copylandboundaryto1Dnetwork                         '
   HELPM (16)= &
   'tolerance in snap-to-landbdy, netboundary only (meshwidths) '
   HELPM (17)= &
   'tolerance in snap-to-landbdy, inner network    (meshwidths) '
   HELPM (18)= &
   'max nr of faces allowed in removesmallflowlinks             '
   HELPM (19)= &
   'choose                                                      '
   WRITE(HELPM (20), '(I0,A,I0,A,I0,A)') &
   I1D2DTP_1TO1, ': default (1-to-1), ', I1D2DTP_1TON_EMB, ': embedded 1-to-n, ', I1D2DTP_1TON_LAT, ': lateral 1-to-n.'


   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL     = IR + 1  ; IR     = IL + 1
      IS(IL) = 82      ; IS(IR) = 10
      IX(IL) = 10      ; IX(IR) = 92
      IY(IL) = 2*I     ; IY(IR) = 2*I
      IT(IL) = 1001    ! ir staat hierboven
   ENDDO

   ! Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW   = NPOS(3)
   IXP = NPOS(1) + (IWS-IW)/2
   IYP  = NPOS(2)
   IH  = IHS - 9

   ! Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program)// ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)

   ! Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = , Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

   ! Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)
   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

   ! Define a new form by supplying arrays containing Field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

   ! Define a help field and define help strings for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFORMpuTINTEGER (2*1 , jins    )
   !CALL IFormputDouble (2*2 , TRIANGLEMINANGLE  ,   '(F7.3)')
   CALL IFormputinteger (2*2 , jadelnetlinktyp   )
   CALL IFormputDouble  (2*3 , TRIANGLEMAXANGLE  ,   '(F7.3)')
   CALL IFormputDouble  (2*4 , TRIANGLESIZEFAC   ,   '(F7.3)')
   CALL IFormputDouble  (2*5 , dcenterinside,        '(F7.3)')
   CALL IFormputDouble  (2*6 , cosphiutrsh       ,   '(F7.3)')
   CALL IFormputDouble  (2*7 , removesmalllinkstrsh, '(F7.3)')
   CALL IFORMpuTINTEGER (2*8 , JOCHECKNET)
   CALL IFORMpuTINTEGER (2*9 , NUMITCOURANT)
   CALL IFormputDouble  (2*10, SMALLESTSIZEINCOURANT,'(F7.0)')
   CALL IFormputDouble  (2*11, TRIAREAREMFRAC       ,'(F7.3)')
   CALL IFORMpuTINTEGER (2*12, M13QUAD)
   CALL IFormputDouble  (2*13, Tooclose             ,'(F7.3)')
   CALL IFormputDouble  (2*14, CONNECT1DEND        ,'(F7.3)')
   CALL IFormputDouble  (2*15, Unidx1D              ,'(F7.3)')
   CALL IFormputDouble  (2*16, DCLOSE_bound         ,'(F7.3)')
   CALL IFormputDouble  (2*17, DCLOSE_whole         ,'(F7.3)')
   CALL IFormputinteger (2*18, maxfaceallow)
   
   CALL IFORMPUTSTRING  (2*19, md_dryptsfile)
   iselect=1
   select(1) = 'use'
   select(2) = 'new'
   select(3) = 'none'
   CALL IFORMPUTMENU(2*19, select,3,iselect)

   CALL IFormputinteger (2*20, imake1d2dtype)
   call IFormputDouble  (2*21, searchRadius1D2DLateral,'(F7.3)')

   ! Display the form with numeric fields left justified and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
   ! check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN
           ! netcell administration out of date if jins changes
           CALL IFORMGETINTEGER (2*1 , jins    )
           if ( jins.ne.jins_old ) netstat = NETSTAT_CELLS_DIRTY
           jins_old = jins

           !CALL IFormGetDouble  (2*2 , TRIANGLEMINANGLE)
           CALL IFormGetinteger  (2*2 , jadelnetlinktyp)
           CALL IFormGetDouble  (2*3 , TRIANGLEMAXANGLE)
           CALL IFormGetDouble  (2*4 , TRIANGLESIZEFAC)
           CALL IFormGetDouble  (2*5 , dcenterinside)
           CALL IFormgetDouble  (2*6 , cosphiutrsh    )
           CALL IFormGetDouble  (2*7 , removesmalllinkstrsh)
           CALL IFORMGETINTEGER (2*8 , JOCHECKNET)
           CALL IFORMGETINTEGER (2*9 , NUMITCOURANT)
           CALL IFormGetDouble  (2*10, SMALLESTSIZEINCOURANT)
           CALL IFormGetDouble  (2*11, TRIAREAREMFRAC)
           CALL IFORMGETINTEGER (2*12, M13QUAD)
           CALL IFormGetDouble  (2*13, Tooclose)
           CALL IFormGetDouble  (2*14, CONNECT1DEND)
           CALL IFormGetDouble  (2*15, Unidx1D)
           CALL IFormGetDouble  (2*16, DCLOSE_BOUND)
           CALL IFormGetDouble  (2*17, DCLOSE_WHOLE)
           CALL IFormGetinteger (2*18, maxfaceallow)
           
           CALL IFORMGETSTRING(2*19, md_dryptsfile)
           CALL IFORMGETMENU(2*19, iselect)
           if ( iselect.eq.2 ) then
              minp = 2  ! select file only
              call filemenu(minp,md_dryptsfile)
           else if ( iselect.eq.3 ) then
              md_dryptsfile = ''
           end if
           iselect = 1

           CALL IFormGetinteger (2*20, imake1d2dtype)
           call IFormGetDouble  (2*21, searchRadius1D2DLateral)
   
       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE changenetworkPARAMETERS

   SUBROUTINE changeorthoPARAMETERS()
   use m_orthosettings
   use unstruc_display
   use m_missing
   use m_netw
   use unstruc_version_module, only : unstruc_company, unstruc_program

   implicit none
   integer :: i
   integer :: ifexit
   integer :: ifinit
   integer :: ih
   integer :: il
   integer :: imp
   integer :: inp
   integer :: ir
   integer :: iw
   integer :: ixp
   integer :: iyp
   integer :: key
   integer :: nbut
   integer :: nlevel
   integer :: numfldactual
   integer :: numparactual

   integer, parameter :: NUMPAR = 15, NUMFLD = 2*NUMPAR
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*82, HELPM(NUMPAR)*102
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line

   NLEVEL    = 4
   OPTION(1) = 'ITERATIONS ORTHOGONALISE, ATTRACT. PARAM' ; IT( 1*2)  = 2
   OPTION(2) = 'ITERATIONS ORTHOGONALISE, BOUNDARY      ' ; IT( 2*2)  = 2
   OPTION(3) = 'ITERATIONS ORTHOGONALISE, INNER AREA    ' ; IT( 3*2)  = 2
   OPTION(4) = 'ORTHOGONALISE <-> SMOOTH;      1.0<->0.0' ; IT( 4*2)  = 6
   OPTION(5) = 'minimum ortho<->smooth on bdy; 1.0<->0.0' ; IT( 5*2)  = 6
   OPTION(6) = 'circumormasscenter;            1.0<->0.0' ; IT( 6*2)  = 6
   OPTION(7) = 'smoother <-> area homogenizer; 1.0<->0.0' ; IT( 7*2)  = 6
   OPTION(8) = 'project to (land)boundary               ' ; IT( 8*2)  = 2
   OPTION(9) = 'cornernode cosine threshold             ' ; IT( 9*2)  = 6
   OPTION(10)= 'mesh-adaptation method                  ' ; IT(10*2)  = 2
   OPTION(11)= 'mesh-refinement factor;        0.0<->1.0' ; IT(11*2)  = 6
   OPTION(12)= 'smooth. iters. ''solution''    in adapt.' ; IT(12*2)  = 2
   OPTION(13)= 'smooth. iters. monitor mat.    in adapt.' ; IT(13*2)  = 2
   OPTION(14)= 'curvi-like <-> pure ortho;     0.0<->1.0' ; IT(14*2)  = 6
   OPTION(15)= 'keep circumcenters (1) or not (0)       ' ; IT(15*2)  = 2


!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6
   HELPM (1) = &
   'Nr. of outer iterations in orthogonalise                    '
   HELPM (2) = &
   'Nr. of boundary iterations in orthogonalise = ITATP*ITBN    '
   HELPM (3) = &
   'Nr. of inner iterations in orthogonalise = ITATP*ITBND*ITIN '
   HELPM (4) = &
   'Balance between orthogonalisation and Laplacian smoothing   '
   HELPM (5) = &
   'Minimum balance between orthogonalisation and Laplacian smoothing on the boundary'
   HELPM (6) = &
   'CIRCUMCENTER = 1, MASSCENTER = 0                            '
   HELPM (7) = &
   'Balance between smoothing and cell-area homogenization      '
   HELPM (8) = &
   '0:no, 1:to org netb, 2:netb to Ldb, 3:''2''+inner net to Ldb, 4:whole net, 5:ok'
   HELPM (9) = &
   'corner if cosine of boundary edge angle < -threshold        '
   HELPM (10)= &
   '0: Winslow; 1: arc-length; 2: harmonic map                  '
   HELPM (11)= &
   'Concentration of mesh in refined region                     '
   HELPM (12) = &
   'Number of smoothing iterations of ''solution'' u in adapt.  '
   HELPM (13) = &
   'Number of smoothing iterations of monitor matrix G in adapt.'
   HELPM (14) = &
   'Pure orthogonalisation versus curvi-grid-like orth. in quads'
   HELPM (15)= &
   'keep circumcenters (1) or not (0)                           '


   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL     = IR + 1  ; IR     = IL + 1
      IS(IL) = 82      ; IS(IR) = 10
      IX(IL) = 10      ; IX(IR) = 92
      IY(IL) = 2*I     ; IY(IR) = 2*I
      IT(IL) = 1001    ! ir staat hierboven
   ENDDO

   ! Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW   = NPOS(3)
   IXP = NPOS(1) + (IWS-IW)/2
   IYP  = NPOS(2)
   IH  = IHS - 9

   ! Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program)// ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)

   ! Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = , Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

   ! Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)
   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

   ! Define a new form by supplying arrays containing Field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

   ! Define a help field and define help strings for 2 of the 4 input fields
!   CALL IFORMHELP(13,IH,60)
   CALL IFORMHELP(13,IH,102)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFORMpuTINTEGER (2*1 , ITATP)
   CALL IFORMpuTINTEGER (2*2 , ITBND)
   CALL IFORMpuTINTEGER (2*3 , ITIN)
   CALL IFormputDouble  (2*4 , ATPF,                 '(F7.3)')
   CALL IFormputDouble  (2*5 , ATPF_B,               '(F7.3)')
   CALL IFormputDouble  (2*6 , CIRCUMORMASSCENTER,   '(F7.3)')
   CALL IFORMputDouble  (2*7 , SMOOTHORAREA,         '(F7.3)')
   CALL IFORMpuTINTEGER (2*8 , JAPROJECT)
   CALL IFORMpuTDouble  (2*9 , CORNERCOS,            '(F7.3)')
   CALL IFORMpuTINTEGER (2*10, ADAPT_METHOD)
   CALL IFORMputDouble  (2*11, ADAPT_BETA,           '(F7.3)')
   CALL IFORMpuTINTEGER (2*12, ADAPT_NITER_U)
   CALL IFORMpuTINTEGER (2*13, ADAPT_NITER_G)
   CALL IFORMputDouble  (2*14, ORTHO_PURE,           '(F7.3)')
   CALL IFormputINTEGER (2*15, keepcircumcenters             )


   ! Display the form with numeric fields left justified and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
   ! check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN
           CALL IFORMGETINTEGER (2*1 , ITATP)
           CALL IFORMGETINTEGER (2*2 , ITBND)
           CALL IFORMGETINTEGER (2*3 , ITIN)
           CALL IFormGetDouble  (2*4 , ATPF)
           CALL IFormGetDouble  (2*5, ATPF_B)
           CALL IFormGETDouble  (2*6 , CIRCUMORMASSCENTER)
           CALL IFORMGETDouble  (2*7 , SMOOTHORAREA)
           CALL IFORMGETINTEGER (2*8 , JAPROJECT)
           CALL IFORMGETDOUBLE  (2*9 , CORNERCOS)
           CALL IFORMGETINTEGER (2*10, ADAPT_METHOD)
           CALL IFORMGETDouble  (2*11, ADAPT_BETA)
           CALL IFORMGETINTEGER (2*12, ADAPT_NITER_U)
           CALL IFORMGETINTEGER (2*13, ADAPT_NITER_G)
           CALL IFORMGETDouble  (2*14, ORTHO_PURE)
           CALL IFormGetInteger (2*15, keepcircumcenters)
       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE changeorthoPARAMETERS

   SUBROUTINE MAKENETPARAMETERS()
   USE M_MAKENET
   use unstruc_display
   use unstruc_version_module, only : unstruc_company, unstruc_program
   implicit none
   integer :: i
   integer :: ifexit
   integer :: ifinit
   integer :: ih
   integer :: il
   integer :: imp
   integer :: inp
   integer :: ir
   integer :: iw
   integer :: ixp
   integer :: iyp
   integer :: key
   integer :: nbut
   integer :: nlevel
   integer :: numfldactual
   integer :: numparactual

   integer, parameter :: NUMPAR = 13, NUMFLD = 2*NUMPAR
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line

   NLEVEL    = 4
   OPTION(1) = 'MAZE TYPE: SQUARE, WIEBER, HEX, TRI  ( )' ; IT(1*2)  = 2
   OPTION(2) = 'NR OF MAZES X                        ( )' ; IT(2*2)  = 2
   OPTION(3) = 'NR OF MAZES Y                        ( )' ; IT(3*2)  = 2
   OPTION(4) = 'MAZE ANGLE     1-90                (deg)' ; IT(4*2)  = 6
   OPTION(5) = 'MAZE SIZE                            (m)' ; IT(5*2)  = 6
   OPTION(6) = 'LINE THICKNESS                      (mm)' ; IT(6*2)  = 6
   OPTION(7) = 'ORIGIN X                             (m)' ; IT(7*2)  = 6
   OPTION(8) = 'ORIGIN Y                             (m)' ; IT(8*2)  = 6
   OPTION(9) = 'ORIGIN Z                             (m)' ; IT(9*2)  = 6
   OPTION(10)= 'DX (FOR TYPE 0 ONLY)                 (m)' ; IT(10*2) = 6
   OPTION(11)= 'DY (FOR TYPE 0 ONLY)                 (m)' ; IT(11*2) = 6
   OPTION(12)= '                                        ' ; IT(12*2) = 1001
   OPTION(13)= 'MAZE SIZE HORIZONTAL PART HEXAGON   (cm)' ; IT(13*2) = 6
!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6
   HELPM (1) = &
   'SQUARE = 0, WIEBER = 1, HEX1 = 2, HEX2 = 3, TRIANGLE = 4    '
   HELPM (2) = &
   '                                                            '
   HELPM (3) = &
   '                                                            '
   HELPM (4) = &
   '                                                            '
   HELPM (5) = &
   '                                                            '
   HELPM (6) = &
   '                                                            '
   HELPM (6) = &
   '                                                            '
   HELPM (7) = &
   '                                                            '
   HELPM (8) = &
   '                                                            '
   HELPM (9) = &
   '                                                            '
   HELPM (10)= &
   '                                                            '
   HELPM (11)= &
   '                                                            '
   HELPM (12)= &
   '                                                            '
   HELPM (13)= &
   'ONLY VALID FOR MAZE TYPE = 2, HEXAGON                       '

   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL     = IR + 1  ; IR     = IL + 1
      IS(IL) = 82      ; IS(IR) = 10
      IX(IL) = 10      ; IX(IR) = 100
      IY(IL) = 2*I     ; IY(IR) = 2*I
      IT(IL) = 1001    ! ir staat hierboven
   ENDDO

   ! Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW   = NPOS(3)
   IXP = NPOS(1) + (IWS-IW)/2
   IYP  = NPOS(2)
   IH  = IHS - 9

   ! Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program)// ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)

   ! Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = , Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

   ! Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)
   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

   ! Define a new form by supplying arrays containing Field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

   ! Define a help field and define help strings for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFORMPUTINTEGER (2*1, NTYP           )
   CALL IFORMPUTINTEGER (2*2, NRX            )
   CALL IFORMPUTINTEGER (2*3, NRY            )
   CALL IFormPutDouble  (2*4, ANGLE, '(F7.3)')
   CALL IFormPutDouble  (2*5, SIZE,  '(F7.3)')
   CALL IFormPutDouble  (2*6, THICK, '(F7.3)')
   CALL IFormPutDouble  (2*7, X0   , '(F7.3)')
   CALL IFormPutDouble  (2*8, Y0   , '(F7.3)')
   CALL IFormPutDouble  (2*9, Z0   , '(F7.3)')
   CALL IFormPutDouble  (2*10,DX0  , '(F7.3)')
   CALL IFormPutDouble  (2*11,DY0  , '(F7.3)')
   CALL IFormPutDouble  (2*13,HSIZE, '(F7.3)')

   ! Display the form with numeric fields left justified and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
   ! check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN
           CALL IFORMGETINTEGER (2*1 , NTYP )
           CALL IFORMGETINTEGER (2*2 , NRX  )
           CALL IFORMGETINTEGER (2*3 , NRY  )
           CALL IFormGetDouble  (2*4 , ANGLE)
           CALL IFormGetDouble  (2*5 , SIZE )
           CALL IFormGetDouble  (2*6 , THICK)
           CALL IFormGetDouble  (2*7 , X0   )
           CALL IFormGetDouble  (2*8 , Y0   )
           CALL IFormGetDouble  (2*9 , Z0   )
           CALL IFormGetDouble  (2*10, DX0  )
           CALL IFormGetDouble  (2*11, DY0  )
           CALL IFormGetDouble  (2*13, HSIZE)
       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE MAKENETPARAMETERS

   SUBROUTINE MERGENETPARAMETERS()
   USE M_MERGENET
   use unstruc_display
   use unstruc_version_module, only : unstruc_company, unstruc_program
   implicit none
   integer :: i
   integer :: ifexit
   integer :: ifinit
   integer :: ih
   integer :: il
   integer :: imp
   integer :: inp
   integer :: ir
   integer :: iw
   integer :: ixp
   integer :: iyp
   integer :: key
   integer :: nbut
   integer :: nlevel
   integer :: numfldactual
   integer :: numparactual

   integer, parameter :: NUMPAR = 2, NUMFLD = 2*NUMPAR
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line

   NLEVEL    = 4
   OPTION(1) = 'MAXIMUM NR OF LINKS OF A MERGING NODE( )' ; IT(1*2)  = 2
   OPTION(2) = 'MERGE NODES WITH SAME X/Y/Z:    1/2/3( )' ; IT(2*2)  = 2
   !123456789012345678901234567890123456789012345678901234567890
   !         1         2         3         4         5         6
   HELPM (1) = &
   'ONLY NODES WITH THIS NUMBER OF LINKS OR LESS WILL BE MERGED '
   HELPM (2) = &
   '1 = MERGING NODES MUST HAVE EQUAL X-COORDINATE, 2=EQUAL Y-CO'

   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL     = IR + 1  ; IR     = IL + 1
      IS(IL) = 82      ; IS(IR) = 10
      IX(IL) = 10      ; IX(IR) = 100
      IY(IL) = 2*I     ; IY(IR) = 2*I
      IT(IL) = 1001    ! ir staat hierboven
   ENDDO

   ! Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW   = NPOS(3)
   IXP = NPOS(1) + (IWS-IW)/2
   IYP  = NPOS(2)
   IH  = IHS - 9

   ! Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program)// ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)

   ! Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = , Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

   ! Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)
   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

   ! Define a new form by supplying arrays containing Field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

   ! Define a help field and define help strings for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFORMPUTINTEGER (2*1, NUMM )
   CALL IFORMPUTINTEGER (2*2, JXYZ )

   ! Display the form with numeric fields left justified and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
   ! check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN
           CALL IFORMGETINTEGER (2*1 , NUMM )
           CALL IFORMGETINTEGER (2*2 , JXYZ )
       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE MERGENETPARAMETERS

   SUBROUTINE DISPUT(NPUT)
   USE M_SFERIC
   USE M_DEVICES
   use network_data, only: kn3typ
   use m_missing, only: JINS
   implicit none
   integer :: jav
   integer :: jview
   double precision :: xyz
   integer :: NPUT

   COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
   CHARACTER TEX*32
   IF (NPUT .EQ. 0) THEN
      TEX =' GET A POINT                    '
   ELSE IF (NPUT .EQ. 1) THEN
      TEX =' PUT A POINT                    '
   ELSE IF (NPUT .EQ. -1) THEN
      TEX =' INSERT A POINT                 '
   ELSE IF (NPUT .EQ. -2) THEN
      TEX =' DELETE A POINT                 '
   ELSE IF (NPUT .EQ. -3) THEN
      TEX =' DELETE A SPLINE                '
   ELSE IF (NPUT .EQ. -4 .or. NPUT .EQ. -5) THEN
      TEX =' GET A SPLINE                   '
   ELSE IF (NPUT .EQ. -41 .or. NPUT .EQ. -51) THEN
      TEX =' PUT A SPLINE                   '
   ELSE IF (NPUT .EQ. -6 ) THEN
      TEX =' GET A SPLINE                   '
   ELSE IF (NPUT .EQ.  2) THEN
      TEX =' GET SECOND POINT               '
   ELSE IF (NPUT .EQ.  3) THEN
      TEX =' CLICK GRID POINT               '
   ELSE IF (NPUT .EQ.  4) THEN
      TEX ='                                '
   ELSE IF (NPUT .EQ.  5) THEN
      TEX= 'CLICK VIEWPOINT                 '
   ELSE IF (NPUT .EQ.  6) THEN
      TEX ='PRESS + OR -                    '
   ELSE IF (NPUT .EQ.  7) THEN
      TEX ='PRESS ANY KEY                   '
   ELSE IF (NPUT .EQ.  8) THEN
      TEX ='CLICK BLOCK POINT 1             '
   ELSE IF (NPUT .EQ.  9) THEN
      TEX ='CLICK BLOCK POINT 2             '
   ELSE IF (NPUT .EQ. 10) THEN
      TEX ='CLICK LINE POINT 1              '
   ELSE IF (NPUT .EQ. 11) THEN
      TEX ='CLICK LINE POINT 2              '
   ELSE IF (NPUT .EQ. 12) THEN
      TEX ='ENTER OR ESC                    '
   ELSE IF (NPUT .EQ. 13) THEN
      TEX ='GET POINT ON LINE               '
   ELSE IF (NPUT .EQ. 14) THEN
      TEX ='CLICK INFLUENCE 1 OR RIGHT MOUSE'
   ELSE IF (NPUT .EQ. 15) THEN
      TEX ='CLICK INFLUENCE 2 OR RIGHT MOUSE'
   ELSE IF (NPUT .EQ. 16) THEN
      TEX ='REPLACE POINT                   '
   ELSE IF (NPUT .EQ. 17) THEN
      TEX ='CLICK BLOCK 3 OR RIGHT MOUSE    '
   ELSE IF (NPUT .EQ. 18) THEN
      TEX ='CLICK BLOCK 4 OR RIGHT MOUSE    '
   ELSE IF (NPUT .EQ. 19) THEN
      TEX ='CLICK RIGHT MOUSE OR Escape     '
   ELSE IF (NPUT .EQ. 20) THEN
      TEX =' GET A POINT ON LINE OR RIGHT MS'
   ELSE IF (NPUT .EQ. 21) THEN
      TEX ='PRESS + OR - , SPACE BAR or Del '
   ELSE IF (NPUT .EQ. 22) THEN
      TEX =' CLICK DEPTH POINT              '
   ELSE IF (NPUT .EQ. 23) THEN
      TEX =' CLICK SAMPLE POINT             '
   ELSE IF (NPUT .EQ. 24) THEN
      TEX =' PUT SAMPLE POINT               '
   ELSE IF (NPUT .EQ. 25) THEN
      TEX =' INSERT SAMPLE POINT            '
   ELSE IF (NPUT .EQ. 26) THEN
      TEX =' DELETE SAMPLE POINT            '
   ELSE IF (NPUT .EQ. 27) THEN
      TEX =' CLICK SAMPLE POINT, CHANGE VAL.'
   ELSE IF (NPUT .EQ. 28) THEN
      TEX =' GET DDBOUNDARY POINT           '
   ELSE IF (NPUT .EQ. 29) THEN
      TEX =' PUT DDBOUNDARY POINT           '
   ELSE IF (NPUT .EQ. 30) THEN
      TEX =' INSERT DDBOUNDARY POINT 1      '
   ELSE IF (NPUT .EQ. 31) THEN
      TEX =' INSERT DDBOUNDARY POINT 2      '
   ELSE IF (NPUT .EQ. 32) THEN
      TEX =' DELETE DDBOUNDARY              '
   ELSE IF (NPUT .EQ. 33) THEN
      TEX =' CLICK COLOR TO CHANGE          '
   ELSE IF (NPUT .EQ. 34) THEN
      TEX =' CLICK COLOR IN TABLE           '
   ELSE IF (NPUT .EQ. 35) THEN
      TEX =' USE ARROW KEYS TO CHANGE COLOUR'
   ELSE IF (NPUT .EQ. 36) THEN
      TEX =' INDICATE WATER RELEASE POINT   '
   ELSE IF (NPUT .EQ. 37) THEN
      TEX =' PRESS + OR SPACE BAR           '
   ELSE IF (NPUT .EQ. 38) THEN
      TEX =' CLICK FIRST NODE               '
   ELSE IF (NPUT .EQ. 39) THEN
      TEX =' CLICK NEXT #D NODE             '
      write (TEX(13:13), '(i1)') KN3TYP ! 1D or 2D
   ELSE IF (NPUT .EQ. 40) THEN
      TEX =' CLICK FIRST POL.POINT NEAR LDB '
   ELSE IF (NPUT .EQ. 41) THEN
      TEX =' CLICK SECOND POL.POINT NEAR LDB'
   ELSE IF (NPUT .EQ. 42) THEN
      TEX =' CLICK FIRST POL.POINT NEAR NET '
   ELSE IF (NPUT .EQ. 43) THEN
      TEX =' CLICK SECOND POL.POINT NEAR NET'
   ELSE IF (NPUT .EQ. 44) THEN
      TEX =' CLICK 1ST POL. START/END POINT '
   ELSE IF (NPUT .EQ. 45) THEN
      TEX =' CLICK 2ND POL. START/END POINT '
   ELSE IF (NPUT .EQ. 46) THEN
      TEX =' CLICK 1ST POL. START/END POINT '
   ELSE IF (NPUT .EQ. 47) THEN
      TEX =' CLICK 2ND POL. START/END POINT '
   ELSE IF (NPUT .EQ. 48) THEN
      TEX =' CLICK LINE'
   ELSE IF (NPUT .EQ. 49) THEN
      TEX =' CLICK Sample for isocol minval '
   ELSE IF (NPUT .EQ. 50) THEN
      TEX =' CLICK Sample for isocol maxval '
   ELSE IF (NPUT .EQ. 51) THEN
      TEX =' CLICK FLOW NODE                '
   ELSE IF (NPUT .EQ. 52) THEN
      TEX =' CLICK FLOW LINK                '
   ELSE IF (NPUT .EQ. 53) THEN
      TEX =' CLICK flow node for isocol minval '
   ELSE IF (NPUT .EQ. 54) THEN
      TEX =' CLICK flow node for isocol maxval '
   ELSE IF (NPUT .EQ. 55) THEN
      TEX =' CLICK NET LINK                '
   ELSE IF (NPUT .EQ. 56) THEN
      TEX =' GET A POINT                    '
   ELSE IF (NPUT .EQ. 57) THEN
      TEX =' PUT A POINT                    '
   ELSE IF (NPUT .EQ. 58) THEN
      TEX =' CLICK FIRST POINT              '
   ELSE IF (NPUT .EQ. 59) THEN
      TEX =' CLICK A BOUNDARY POINT         '
   ELSE IF (NPUT .EQ. 60) THEN
      TEX =' CLICK NETWORK POINT, CHANGE VAL'
   ELSE IF (NPUT .EQ. 61) THEN
      TEX =' CLICK POLYGON POINT, CHANGE VAL'
   ELSE IF (NPUT .EQ. 62) THEN
      TEX =' CLICK FIRST POLYGON POINT      '
   ELSE IF (NPUT .EQ. 63) THEN
      TEX =' CLICK SECOND POLYGON POINT     '
   ELSE IF (NPUT .EQ. 64) THEN
      TEX =' CLICK THIRD POLYGON POINT      '
   ELSE IF (NPUT .EQ. 466) THEN
      TEX =' CLICK 1ST POL. START/END POINT '
   ELSE IF (NPUT .EQ. 477) THEN
      TEX =' CLICK 2ND POL. START/END POINT '
   ENDIF
   CALL KTEXT(TEX,1,4,15)
!

  ! IF (JVIEW .EQ. 1) THEN
  !    CALL KTEXT(' NORMAL   ',IWS-9,IHS-1,15)
  ! ELSE IF (JVIEW .EQ. 2) THEN
  !    CALL KTEXT(' FROM LEFT',IWS-9,IHS-1,15)
  ! ELSE IF (JVIEW .EQ. 3) THEN
  !    CALL KTEXT(' FROM TOP ',IWS-9,IHS-1,15)
  ! ELSE IF (JVIEW .EQ. 4) THEN
  !    CALL KTEXT(' PERSP-view ',IWS-11,IHS-1,15)
  ! ENDIF
   IF (JINS /= 1) THEN
      CALL KTEXT(' JINS=0',IWS-16,IHS-2,15)
   END IF

   IF (JSFERIC == 1) THEN
      CALL KTEXT(' SPHERICAL',IWS-9,IHS-2,15)
   ELSE
      CALL KTEXT(' CARTESIAN',IWS-9,IHS-2,15)
   ENDIF

   RETURN
   END SUBROUTINE DISPUT

    !> Selects the edit mode for a given keypress code.
    !! Alt-P/-N/-S/-G/-B/-F for the respective modes.
    subroutine selecteditmode(newmode, key)
    implicit none
    integer, intent(inout) :: newmode !< New mode (0 for invalid key presses).
    integer, intent(in)    :: key     !< Key press code

    if      (key == 512+80) then ! Alt+P: Edit Polygon
        newmode = 1
    else if (key == 512+78) then ! Alt+N: Edit Network
        newmode = 2
    else if (key == 512+83) then ! Alt+S: Edit Splines
        newmode = 3
    else if (key == 512+71) then ! Alt+G: Edit Grid
        newmode = 4
    else if (key == 512+66) then ! Alt+B: Edit Samples (bathymetry)
        newmode = 5
    else if (key == 512+70) then ! Alt+F: Edit Flow
        newmode = 6
    end if
    return
    end subroutine selecteditmode


   SUBROUTINE EDITPOL(MODE,KEY,NETFLOW)
   USE M_POLYGON
   use network_data, only: netstat, NETSTAT_CELLS_DIRTY
   USE M_MISSING
   use m_partitioninfo
   use unstruc_colors
   use unstruc_model
   use unstruc_display
   use m_flow, only : kmx, jasal, iturbulencemodel
   use unstruc_api
   use dfm_error
   use unstruc_messages
   implicit none
   double precision :: cdflow
   double precision :: cfric
   double precision :: fbouy
   double precision :: fdyn
   integer :: janet
   integer :: jaquit, jazoomshift, nshift
   integer :: k
   integer :: l1
   integer :: l2
   integer :: l3
   integer :: moments
   integer :: nlevel
   integer :: nput
   integer :: num
   integer :: numb
   integer :: nwhat

   integer :: MODE, KEY, NETFLOW
   integer :: newmode, mout
   double precision :: xp, yp, RD
   integer :: JQN
   integer :: iresult
   integer :: ja4
   logical, external :: ispolystartend

   COMMON /HELPNOW/ WRDKEY,NLEVEL
   COMMON /QNRGF/ JQN
   COMMON /SETTINGS/ FDYN, FBOUY, CDFLOW, CFRIC, MOMENTS, JANET

   CHARACTER TEX*26, WRDKEY*40, fnam*255
   
   integer :: iii

   if (jampi == 1) then
      write(tex,"(' EDITPOL:', I5)") my_rank
   else
      tex = 'EDITPOL'
   endif

   WRDKEY = TEX
   NLEVEL =  2
   NUM    =  0
   NWHAT  =  0
   NPUT   = -1
   NUMB   =  2
   JAQUIT = 0
   MP     = NPL
   L1     = 0


   CALL SAVEPOL()

   10 CONTINUE
      CALL DRAWNU(KEY)
      CALL KTEXT(TEX,1,2,15)
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)

      IF (KEY .NE. 81 .AND. KEY .NE. 81+32) JAQUIT = 0

      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            RETURN
         ELSE
            IF ((JQN .EQ. 1 .AND. NUM .EQ. 5 .AND. NWHAT .EQ. 1) .OR.   &
                (JQN .EQ. 2 .AND. NUM .EQ. 5 .AND. NWHAT .EQ. 8)) THEN
                MP  = 0
            ENDIF
         ENDIF
         CALL CHOICES(MODE,NUM,NWHAT,KEY)
      ELSE IF (KEY >= 577) THEN ! Alt+letter switches edit mode.
        call selecteditmode(newmode, key)
        if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
        end if
      ELSE IF (KEY .EQ. 21) THEN                                          ! Left mouse or ins
!        edit/modify polygon: netcell administration out of date
!        netstat = NETSTAT_CELLS_DIRTY	! unwanted during flow computations

!        INS KEY
         CALL SAVEPOL()
         IF (NPUT .EQ. 0 .OR. NPUT .EQ. -2 .OR. NPUT .EQ. 56 .OR. NPUT .EQ. 61) THEN
!           kijken welk punt bij deleten en bij oppakken
            CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
         ENDIF
         IF ( NPUT .EQ. 0 .AND. MP .NE. 0) THEN
!           punt oppakken
            CALL MOVABS(XP,YP)
            CALL HLCIR(RCIR, NCOLTX)
            NPUT = 1
         ELSE IF (NPUT .EQ. 1 .AND. MP .NE. 0) THEN
!           punt neerzetten
            CALL DISP2C(XPL, YPL, NPL, RCIR,      0)
            XPL(MP) = XP
            YPL(MP) = YP
            CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
            NPUT   = 0
         ELSE IF (NPUT .EQ. -1) THEN
!           punt toevoegen
            call increasepol(npl+1, 1)
            CALL DISP2C(XPL, YPL, NPL, RCIR,      0)
            CALL MODLN2(XPL, YPL, ZPL, MAXPOL, NPL, MP, XP, YP, NPUT)
            CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         ELSE IF ( NPUT .EQ. -2 .AND. MP .NE. 0) THEN
!           punt deleten
            CALL SETCOL(0)
            CALL MOVABS(XP,YP)
            IF (MP .EQ. 1) THEN
               CALL CIR(1.4d0*RCIR)
            ELSE
               CALL CIR(RCIR)
            ENDIF
            CALL DISP2C(XPL, YPL, NPL, RCIR,      0)
            CALL MODLN2(XPL, YPL, ZPL, MAXPOL, NPL, MP, XP, YP, NPUT)
            CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         ELSE IF ( NPUT == 40 .OR. NPUT == 41) THEN
!           Polyline to land boundary
            CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            IF (MP .NE. 0) THEN
               CALL MOVABS(XP,YP)
               CALL HLCIR(RCIR, NCOLTX)
               IF (L1 == 0) THEN
                   L1    = MP
                   NPUT = 41
               ELSE
                   L2   = MP
                   NPUT = 40
                   CALL POLTOLAND(L1,L2)
                   L1   = 0
                   L2 = 0
                   KEY  = 3
               ENDIF
            ENDIF
         ELSE IF ( NPUT == 42 .OR. NPUT == 43) THEN
!           Polyline to net boundary
            CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            IF (MP .NE. 0) THEN
               CALL MOVABS(XP,YP)
               CALL HLCIR(RCIR, NCOLTX)
               IF (L1 == 0) THEN
                   L1    = MP
                   NPUT = 43
               ELSE
                   L2   = MP
                   NPUT = 42
                   CALL POLTONET(L1,L2)
                   L1   = 0
                   L2 = 0
                   KEY  = 3
               ENDIF
            ENDIF
         ELSE IF ( NPUT == 44 .OR. NPUT == 45) THEN
!           Merge two polylines, click two end points.
            CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            if (mp /= 0 .and. .not. ispolystartend(xpl, ypl, npl, maxpol, mp)) then
                ! Clicked point was not an end point, discard it.
                mp = 0
            end if
            IF (MP .NE. 0) THEN
               CALL MOVABS(XP,YP)
               CALL HLCIR(RCIR, NCOLTX)
               IF (L1 == 0) THEN
                   L1    = MP
                   NPUT = 45
               ELSE
                   L2   = MP
                   NPUT = 44
                   call savepol()
                   CALL mergepoly(xpl, ypl, zpl, maxpol, npl, L1,L2)
                   L1   = 0
                   L2 = 0
                   KEY  = 3
               ENDIF
            ENDIF
         ELSE IF ( NPUT == 46 .OR. NPUT == 47 .OR. NPUT == 466 .OR. NPUT == 477 ) THEN
!           Refine polygon substring (click 2 points)
            CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            IF (MP .NE. 0) THEN
               CALL MOVABS(XP,YP)
               CALL HLCIR(RCIR, NCOLTX)
               IF (L1 == 0) THEN
                   L1    = MP
                   if ( NPUT.eq.46 ) then
                      NPUT = 47
                   else
                      NPUT = 477
                   end if
               ELSE
                   L2   = MP
                   if( NPUT.eq.46 .or. NPUT.eq.47 ) then
                      NPUT = 47
                      CALL refinepolygonpart(L1,L2,0)
                   else
                      NPUT = 477
!                     get uniform spacing
                      RD = dxuni
                      call TYPEVALUE(dxuni,key)
                      CALL refinepolygonpart(L1,L2,1)
                   end if
                   L1   = 0
                   L2 = 0
                   KEY  = 3
               ENDIF
            ENDIF
         ELSE IF ( NPUT .EQ. 56 .AND. MP .NE. 0) THEN
!           punt oppakken
            CALL MOVABS(XP,YP)
            CALL HLCIR(RCIR, NCOLTX)
            NPUT = 57
         ELSE IF (NPUT .EQ. 57 .AND. MP .NE. 0) THEN
!           punt neerzetten
            CALL DISP2C(XPL, YPL, NPL, RCIR,      0)
            call copypol(MP, XP, YP)
            CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
            NPUT   = 56
         ELSE IF ( NPUT .EQ. 61 .AND. MP .NE. 0) THEN
!           punt in waarde veranderen
            RD = zpl(MP)
            CALL TYPEVALUE(RD,KEY)
            CALL KCIR(XP,YP,RD)
            Zpl(MP) = RD
         ELSE IF ( NPUT.ge.62 .AND. NPUT.le. 67 ) THEN   ! NPUT == 62 .OR. NPUT == 63 .OR. NPUT == 64) THEN
!           Polyline to net boundary
            CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
            IF (MP .NE. 0) THEN
               CALL MOVABS(XP,YP)
               CALL HLCIR(RCIR, NCOLTX)
               IF (L1 == 0) THEN
                   L1    = MP
                   NPUT = NPUT +1   ! 63 or 66
               ELSE IF (L2 == 0) THEN
                   L2    = MP
                   NPUT = NPUT +1   ! 64 or 67
               ELSE
                   L3   = MP
                   NPUT = NPUT -2   ! 62 or 65
                   
                   if ( NPUT.eq.62 ) then
                      ja4 = 0
                      call confrm('use fourth side?', ja4)
                      CALL pol2curvi(L1,L2,L3,ja4)
                   else
                      CALL pol2curvi_tri(L1,L2,L3)
                   end if
                   
                   L1 = 0
                   L2 = 0
                   L3 = 0
                   KEY  = 3
               ENDIF
            ENDIF
         ENDIF
      ELSE IF (KEY .EQ. 22) THEN                                          !  ENTER KEY
         IF (NETFLOW == 2) THEN
             iresult = FLOW()
             if (iresult == DFM_SIGINT) then
                call mess(LEVEL_ERROR, 'Final handling of SIGINT signal. Stopping program.')
                call STOPINT()
             else if (iresult /= DFM_NOERR) then
                call qnerror('Error occurred while running, please inspect your diagnostic output.',' ', ' ')
             end if
!         ELSE IF (NPL .EQ. 0) THEN    
!             CALL SOLVE(0)
         ELSE IF (NPL .GE. 3 .AND. NPL .LE. 4) THEN
            CALL MAKEPANELXY(1-JANET)
            CALL DELPOL()
         ELSE IF (NPL .GE. 2) THEN
            CALL POLTOLINES()
            CALL DELPOL()
         ENDIF
         KEY = 3
      ELSE IF (KEY .EQ. 23) THEN                                          ! ESC
         CALL RESTOREPOL() 
         KEY = 3
         if (nput == 1) then
            NPUT = 0
         end if
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
!        CALL SHWXYZ2(X,Y,RD1,RD2,RD3,MC,NC,0,KEY,M,N)
      ELSE IF (KEY .EQ. 73 .OR. KEY .EQ. 73+32) THEN                      ! i key
         IF (NPUT .NE. 1) THEN
!           kijken welk punt dit is t.b.v insert mode
            CALL ISPOI1( XPL, YPL, NPL, XP, YP, MP)
            IF (MP == 0 .AND. NPL .NE. 0) THEN
               NPL = NPL + 1
               call increasepol(npl, 1)
               XPL(NPL) = dmiss
               YPL(NPL) = dmiss
               ZPL(NPL) = dmiss
            else if (mp /= 0) then
                ! Point was found, now highlight it temporarily on screen.
                CALL MOVABS(XP,YP)
                CALL HLCIR(RCIR, NCOLTX)
            ENDIF
         ENDIF
         NPUT = -1
      ELSE IF (KEY .EQ. 8) THEN                                           ! Backspace KEY
!        delete all polygons and stay in previous mode.
         call savepol()
         call delpol()
         key = 3
      ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32) THEN                      ! D KEY
!        delete mode
         NPUT = -2
      ELSE IF (KEY .EQ. 81) THEN ! .OR. KEY .EQ. 81+32) THEN                      ! Q-key
!         call split_pol(2,2,100,100)
      
          NPUT= 62
          L1 = 0
          L2 = 0
          
      ELSE IF (KEY .EQ. 81+32) THEN
         NPUT = 65
         L1 = 0
         L2 = 0
      ELSE IF (KEY .EQ. 82 .OR. KEY .EQ. 82+32 .AND. NPUT .NE. 1) THEN    ! R KEY
!        replace mode, maar niet bij zetten
         NPUT =  0
      ELSE IF (KEY .EQ. 67) then                                          ! C key
         NPUT =  56                      ! copy polygon orthogonally
      else if (KEY .EQ. 67+32 ) THEN                                      ! c KEY hk's original 
         NPUT =  61                      ! change zpl value 
      ELSE IF (KEY .EQ. 88 .OR. KEY .EQ. 88+32) THEN                      ! X KEY
!        Lijn openbreken met X
!         CALL SAVEP(MP,MPS,XPL,YPL,NPL,XPH,YPH,NPH,MAXPOL)
         CALL SAVEPOL()
         CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
         IF (MP .NE. 0) THEN
            CALL DISP2C(XPL, YPL, NPL, RCIR,      0)
            XPL(MP) = dmiss
            YPL(MP) = dmiss
            ZPL(MP) = dmiss
            CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         ENDIF
      ELSE IF (KEY .EQ. 69+32) THEN                                     ! e KEY
!        edit/modify polygon: netcell administration out of date
         netstat = NETSTAT_CELLS_DIRTY

!        Delete deelpolygoon met E
         CALL SAVEPOL()
         CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
         IF (MP .NE. 0) THEN
            CALL DISP2C(XPL, YPL, NPL, RCIR, 0)
            CALL MODLN2(XPL, YPL, ZPL, MAXPOL, NPL, MP, XP, YP, -3)
            CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         ENDIF
      ELSE IF ( KEY .EQ. 69 ) THEN                                      ! E key
!        edit/modify polygon: netcell administration out of date
         netstat = NETSTAT_CELLS_DIRTY

         CALL SAVEPOL()
         CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
         IF (MP .NE. 0) THEN
            CALL DISP2C(XPL, YPL, NPL, RCIR, 0)
            CALL MODLN2(XPL, YPL, ZPL, MAXPOL, NPL, MP, XP, YP, -4)
            CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         ENDIF

      ELSE IF (-KEY .EQ. 71 .OR. -KEY .EQ. 71+32) THEN                  ! G KEY
         ! MIRROR LAST POLYGON PART IN Y
         CALL SAVEPOL()
         !CALL SAVEP(MP,MPS,XPL,YPL,NPL,XPH,YPH,NPH,MAXPOL)
         DO K = MP-1,1,-1
            NPL = NPL + 1
            XPL(NPL) = XPL(K)
            YPL(NPL) = 2*YPL(MP) - YPL(K)
            ZPL(NPL) = ZPL(K)
         ENDDO
         CALL DISP2C(XPL, YPL, NPL, RCIR, NCOLTX)
         MP = NPL
      ELSE IF (KEY .EQ. 81 .OR. KEY .EQ. 81+32) THEN
         !  JAQUIT = JAQUIT + 1
         IF (JAQUIT .EQ. 2) CALL STOPINT()
      ELSE IF (KEY .EQ. 86 .OR. KEY .EQ. 86+32) THEN
         CALL VIEWCYCLE(KEY)
      ELSE IF (KEY .EQ. 43 .or. KEY .EQ. 140) THEN                      ! -
         CALL KPLOTPLUSMIN(-1)
         key = 3
      ELSE IF (KEY .EQ. 45 .or. KEY .EQ. 141) THEN                      ! +
         call KPLOTPLUSMIN(1)
         key = 3
      ELSE IF (KEY .EQ. 42) THEN                                        ! *
         CALL nPLOTPLUSMIN(1)
         key = 3
      ELSE IF (KEY .EQ. 47) THEN                                        ! /
         call nPLOTPLUSMIN(-1)
         key = 3
      ELSE IF (                 KEY .EQ. 87+32) THEN                    ! w for water  + 1 (m)

         call DROPWATER(XP,YP,1)
         key = 3

      ELSE IF (KEY .EQ. 87                    ) THEN                    ! W for water  - 1 (m)

         call DROPWATER(XP,YP,-1)
         key = 3

      ELSE IF (                 KEY .EQ. 66+32) THEN                    ! b for bed + 1 (m)

         call DROPland(XP,YP, 1)
         key = 3

      ELSE IF (KEY .EQ. 66                    ) THEN                    ! B for bed - 1 (m)

         call DROPland(XP,YP, -1)
         key = 3

      ELSE IF (jasal > 0 .and.  KEY .EQ. 83+32) THEN                    ! s for salt + 1 (ppt)

         call DROPzout( 1)
         key = 3

      ELSE IF (jasal > 0 .and.     KEY .EQ. 83) THEN                    ! S for salt - 1 (ppt)

         call DROPzout(-1)
         key = 3

      ELSE IF (kmx > 0 .and. iturbulencemodel == 3 .and. KEY .EQ. 75+32) THEN  ! k for kinetic + 0.01

         call DROPk(XP,YP,1)
         key = 3
         
      ELSE IF (KEY .EQ. 84+32) THEN                                     ! t add (to) tracer
         call droptracer(xp,yp,1d0)
!         call add_particles(1,xp,yp,0)
      ELSE IF (KEY .EQ. 84) THEN                                        ! T t  substract from tracer
         call droptracer(xp,yp,-1d0)

      ELSE IF (KEY .EQ. 32) THEN
         call flow_spatietimestep()
         key = 3
      ELSE IF (KEY .EQ. 76 .OR. KEY .EQ. 76+32) THEN                    ! L KEY

         NPUT = 40  ! TO LAND MODE

      ELSE IF (KEY .EQ. 70+32) THEN                    ! F KEY

         NPUT = 46  ! Refine polygon between two clicked points
         
      else if ( key.eq.70 ) then                       ! SHIFT-F KEY
         NPUT = 466  ! refine polygon between two points with uniform spacing
         
      else if ( key.eq. 70 ) then
!        refine polygon with uniform width
      
         NPUT = 466

      ELSE IF (KEY .EQ. 77 .OR. KEY .EQ. 77+32) THEN                    ! M KEY

         NPUT = 44  ! Merge twee deelpolygonen

      ELSE IF (KEY .EQ. 78 .OR. KEY .EQ. 78+32) THEN                    ! N KEY

         NPUT = 42  ! TO NET  MODE

      ELSE IF (KEY .EQ. 27 .OR. KEY .EQ. 27+32) THEN                    ! ; KEY

         jazoomshift = 0
         nshift = 0
         do while (jazoomshift .ne. 1 .and. nshift < numzoomshift*npl)
            call zoomshift(nshift)
            key = 3
            ndrawpol = 1
            CALL DRAWNU(KEY)
            call halt2(jazoomshift)
         enddo

         ndrawpol = 2
      ELSE IF (KEY .EQ. 46) THEN                                         ! . KEY
         CALL ISPOI1(XPL, YPL, NPL, XP, YP, MP)
         call flippo(MP)
         key = 3
      
      ELSE IF (KEY .EQ. 79 .OR. KEY == 79 + 32 ) THEN                    ! O - KEY
         L1 = index(md_plifile,'_0')
         IF (L1 == 0) THEN 
            md_plifile = 'plif_0001.pli'
            npolf      = 0
         endif   
         L1 = index(md_plifile,'_0')
         if (L1 > 0) then
            fnam  = md_plifile(1:L1)//'0001.pli'
            npolf = npolf + 1
            write(fnam(L1+1:L1+4),'(i4.4)') npolf
            call newnewfil(mout, fnam)
            if (mout > 0) then 
               call wripol(mout)
            endif  
            call plotnu(fnam)
         endif    
      
      ENDIF
!
      GOTO 10
!
      END SUBROUTINE EDITPOL

      SUBROUTINE EDITSPLINES(MODE,KEY)
      use unstruc_colors
      USE M_SPLINES
      use unstruc_display, only: plotSplines
      implicit none
      integer, intent(inout) :: mode, key
      integer :: newmode

!      use rgfblock
!
      CHARACTER WRDKEY*40
      integer :: nlevel
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      integer :: ndraw, IIJ
      COMMON /DRAWTHIS/ ndraw(50)

      integer :: ja, num, numb, ncol, nwhat, nput
      double precision :: xp, yp

      WRDKEY = 'EDIT SPLINES'
      NLEVEL =  2
      JA     =  0
      NUM    =  0
      NWHAT  =  0
      NPUT   = -1
      NUMB   =  9
      NCOL   =  NCOLSP
      NDRAW(15) = 1

      MP     = 0
      NP     = 0
      CALL BOTLIN(0,NUMB,KEY)
!!     TEST
      CALL saveSplines()

    10 CONTINUE
      CALL DRAWNU(KEY)
      CALL KTEXT(' Edit Splines       ',1,2,15)
      !CALL KTEXT(' Click Spline Points',1,3,15)
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)

      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            RETURN
         ELSE
            if (NUM .eq. 5 .and. NWHAT .eq. 2) then
                mp = 0
                np = 0
            endif
            CALL CHOICES(MODE,NUM,NWHAT,KEY)
         ENDIF
      ELSE IF (KEY >= 577) THEN ! Alt+letter switches edit mode.
        call selecteditmode(newmode, key)
        if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
        end if
      ELSE IF (KEY .EQ. 21) THEN
!        INS KEY
         IF (NPUT .EQ.  0 .OR. NPUT .EQ. -2 .OR. NPUT .EQ. -3 .OR. NPUT .EQ. -4 .OR. NPUT .EQ. -5 .OR. NPUT .EQ. -6) THEN
!           kijken welk punt bij deleten en bij oppakken
            CALL isSplinePoint(XP, YP, RCIR, MP, NP)
         ENDIF
         IF ( NPUT .EQ. 0 .AND. MP .NE. 0) THEN
!           punt oppakken
            CALL MOVABS(XP,YP)
            CALL SETCOL(0)
            CALL CIR(RCIR)
            CALL IGRFILLPATTERN(0,0,0)
            CALL SETCOL(NCOL)
            CALL CIR(RCIR)
            CALL IGRFILLPATTERN(4,0,0)
            NPUT = 1
         ELSE IF (NPUT .EQ. 1 .AND. MP .NE. 0) THEN
!           punt neerzetten
            CALL saveSplines()
            CALL plotSplines(MP,     MP,      0)
            call setSplinePoint(MP, NP, XP, YP)
            CALL plotSplines(MP,     MP,   NCOL)
            NPUT   = 0
         ELSE IF (NPUT .EQ. -1) THEN
!           punt toevoegen
            CALL saveSplines()
            CALL plotSplines(MP,     MP,      0)
            call insertSplinePoint(mp, np, xp, yp)
            CALL plotSplines(MP,     MP,   NCOL)
         ELSE IF ( NPUT .EQ. -2 .AND. MP .NE. 0) THEN
!           punt deleten
            CALL saveSplines()
            IIJ = 68
            CALL SETCOL(0)
            CALL MOVABS(XP,YP)
            IF (MP .EQ. 1) THEN
               CALL CIR(1.4*RCIR)
            ELSE
               CALL CIR(RCIR)
            ENDIF
            CALL plotSplines(MP,     MP,      0)
            call delSplinePoint(mp, np)
            CALL plotSplines(MP,     MP,   NCOL)
         ELSE IF ( NPUT .EQ. -3 .AND. MP .NE. 0) THEN
!           hele spline deleten
            CALL saveSplines()
            IIJ = 68
            CALL SETCOL(0)
            CALL MOVABS(XP,YP)
            IF (MP .EQ. 1) THEN
               CALL CIR(1.4*RCIR)
            ELSE
               CALL CIR(RCIR)
            ENDIF
            CALL plotSplines(MP,     MP,      0)
            call delSpline(mp)
            CALL plotSplines(MP,     MP,   NCOL)
         ELSE IF ( ( NPUT .eq. -4 .or. NPUT .eq. -5 ) .AND. MP .NE. 0) THEN
!           move or copy whole spline, get spline
            call savesplines()
            call setcol(0)
            call movabs(xp,yp)
            if ( mp .eq. 1 ) then
               call cir(1.4*rcir)
            else
               call cir(rcir)
            end if
            NPUT = 10*NPUT-1  ! -41 .or. -51
         ELSE IF ( NPUT .eq. -41 .AND. MP .NE. 0) THEN
!           move whole spline, put spline
            CALL plotSplines(MP,     MP,      0)
            call movespline(mp, np, xp, yp)
            CALL plotSplines(MP,     MP,   NCOL)
            NPUT = -4
         ELSE IF ( NPUT .eq. -51 .AND. MP .NE. 0) THEN
!           copy whole spline, put spline
            CALL plotSplines(MP,     MP,      NCOL)   ! plot original spline
            call copyspline(mp, np, xp, yp)
            CALL plotSplines(MP,     MP,   NCOL)
            NPUT = -5
          ELSE IF ( NPUT .eq. -6 .AND. MP .NE. 0) THEN
!           snap spline to landboundary
            CALL plotSplines(MP,     MP,      0)
            call snap_spline(MP)
            CALL plotSplines(MP,     MP,   NCOL)
            MP = 0
         ENDIF
         call dispnode2(mp, np)
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY
         IF (NPUT .EQ. -1 .AND. NP .GE. 2) THEN
            MP = 0
            NP = 0
         ENDIF
      ELSE IF (KEY .EQ. 23) THEN
!        ESC
         CALL restoreSplines()
         KEY = 3
      ELSE IF (KEY .EQ. 73 .OR. KEY .EQ. 73+32) THEN
         IF (NPUT .NE. 1) THEN
!           kijken welk punt dit is t.b.v insert mode (I)
            CALL isSplinePoint(XP, YP, RCIR, MP, NP)
            if (mp/=0 .and. np/=0) then
                ! Point was found, now highlight it temporarily on screen.
                CALL MOVABS(XP,YP)
                CALL SETCOL(0)
                CALL CIR(RCIR)
                CALL IGRFILLPATTERN(0,0,0)
                CALL SETCOL(NCOL)
                CALL CIR(RCIR)
                CALL IGRFILLPATTERN(4,0,0)
            endif
         ENDIF
         NPUT = -1
      ELSE IF (KEY .EQ. 8) THEN    ! Backspace KEY
!        delete all splines (within polygon if any) and stay in previous mode.
         call saveSplines()
         call deleteSelectedSplines()
         key = 3
      ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32) THEN
!        delete mode losse punten (D)
         NPUT = -2
      ELSE IF (KEY .EQ. 88 .OR. KEY .EQ. 88+32) THEN
!        delete mode hele splines (X)
         NPUT = -3
      ELSE IF (KEY .EQ. 77 .OR. KEY .EQ. 77+32) THEN
!        move whole spline (M)
         NPUT = -4
      ELSE IF (KEY .EQ. 67 .OR. KEY .EQ. 67+32) THEN
!        copy whole spline (C)
         NPUT = -5
      ELSE IF (KEY .EQ. 76 .OR. KEY .EQ. 76+32) THEN
!        snap whole spline to land (L)
         NPUT = -6
      ELSE IF (KEY .EQ. 82 .OR. KEY .EQ. 82+32 .AND. NPUT .NE. 1) THEN
!        replace mode, maar niet bij zetten (R)
         NPUT =  0
      ELSE IF (KEY .EQ. 98) THEN
!        b RINGS BELL
         CALL KTEXT(' B Rings Bell',2,6,11)
         CALL OKAY(0)
      ENDIF
!
      GOTO 10
!
      END subroutine editSplines
      

   SUBROUTINE CHANGEDISPLAYPARAMETERS()
   USE M_RAAITEK
   USE M_MISSING
   use unstruc_display
   use m_sediment
   use m_flow, only : kplotfrombedorsurface, kplotordepthaveraged 

   use unstruc_version_module, only : unstruc_company, unstruc_program
   use unstruc_opengl,         only : jaOpenGL

   implicit none
   double precision :: dv
   double precision :: dx
   double precision :: dxshow
   double precision :: dy
   integer :: i
   integer :: ifexit
   integer :: ifinit
   integer :: ih
   integer :: ihcopts
   integer :: il
   integer :: imp
   integer :: inp
   integer :: ir
   integer :: iw
   integer :: ixp
   integer :: iyp
   integer :: jaauto
   integer :: jaeps
   integer :: jaland
   integer :: jaxis
   integer :: key
   integer :: nbut
   integer :: ncols
   integer :: ndec
   integer :: ndraw
   integer :: nhcdev
   integer :: nie
   integer :: nis
   integer :: nlevel
   integer :: numfldactual
   integer :: numhcopts
   integer :: numparactual
   integer :: nv
   integer :: nvec
   double precision :: rmiss, val, vfac, vfacforce, vmax, vmin
   double precision :: x0
   double precision :: xd
   double precision :: xleft
   double precision :: xsc
   double precision :: y0
   double precision :: ybot
   double precision :: ysc
   double precision :: scalesize
   double precision :: tsize
   integer :: JQN

   integer, parameter :: NUMPAR = 33, NUMFLD = 2*NUMPAR
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
   COMMON /TEXTSIZE/ TSIZE
   COMMON /SCREENAREA/ XLEFT,YBOT,JAXIS

   COMMON /HARDCOPY/ NHCDEV,NUMHCOPTS,IHCOPTS(2,20)
   COMMON /SCALEPOS/ XSC,YSC,SCALESIZE,NDEC
   COMMON /VFAC/ VFAC,VFACFORCE,NVEC
   COMMON /ARCINFO/ DX, DY, X0, Y0, RMISS, DXSHOW, XD
   COMMON /DRAWTHIS/  ndraw(50)
   COMMON /QNRGF/ JQN
   double precision :: VS(4,4)
   integer, external :: infoinput
   external :: highlight_form_line
!
   NLEVEL    = 3
   OPTION(1) = 'HARDCOPY DRIVER NUMBER                  ' ; IT(2*1)  = 2
   OPTION(2) = 'ENCAPSULATED POSTSCRIPT                 ' ; IT(2*2)  = 2
   OPTION(3) = 'LANDSCAPE                               ' ; IT(2*3)  = 2
   OPTION(4) = 'SIZE OF DOTS                            ' ; IT(2*4)  = 6
   OPTION(5) = 'SIZE OF NUMBERS                         ' ; IT(2*5)  = 6
   OPTION(6) = 'DEFAULT VALUE                           ' ; IT(2*6)  = 6
   OPTION(7) = 'LEFT SCREEN MARGIN                      ' ; IT(2*7)  = 6
   OPTION(8) = 'BOTTOM SCREEN MARGIN                    ' ; IT(2*8)  = 6
   OPTION(9) = 'PLOTTING AXIS YES/NO                    ' ; IT(2*9)  = 2
   OPTION(10)= 'SCALEFACTOR FOR VECTORS                 ' ; IT(2*10) = 6
   OPTION(11)= 'VECTOR   INTERVAL                       ' ; IT(2*11) = 2
   OPTION(12)= 'PLOTTING INTERVAL NTEK                  ' ; IT(2*12) = 2
   OPTION(13)= 'PLOT TO FILE YES/NO                     ' ; IT(2*13) = 2
   OPTION(14)= 'MINIMUM ZLEVEL RAAITEK                  ' ; IT(2*14) = 6
   OPTION(15)= 'MAXIMUM ZLEVEL RAAITEK                  ' ; IT(2*15) = 6
   OPTION(16)= 'PLOT TOP ROWS INFORMATION TEXT          ' ; IT(2*16) = 2
   OPTION(17)= 'Number of zoomshift intervals, press ;  ' ; IT(2*17) = 2
   OPTION(18)= 'Enable/disable minmax highlighting      ' ; IT(2*18) = 2
   OPTION(19)= 'Highlight specific net node number      ' ; IT(2*19) = 2
   OPTION(20)= 'Highlight specific net link number      ' ; IT(2*20) = 2
   OPTION(21)= 'Highlight specific flow node number     ' ; IT(2*21) = 2
   OPTION(22)= 'Highlight specific flow link number     ' ; IT(2*22) = 2
   OPTION(23)= 'Node waterdepth plotting threshold      ' ; IT(2*23) = 6
   OPTION(24)= 'Plot sideview in cheap perspective 1/0  ' ; IT(2*24) = 6
   OPTION(25)= 'Grain size fraction nr to plot          ' ; IT(2*25) = 2
   OPTION(26)= 'Show vertical reference profiles 1/0    ' ; IT(2*26) = 2
   OPTION(27)= 'display flownodes minus plotlin: 1      ' ; IT(2*27) = 2
   OPTION(28)= '                                        ' ; IT(2*28) = 2
   OPTION(29)= 'use OpenGL (0:no, 1:yes)                ' ; IT(2*29) = 2
   OPTION(30)= 'show bedlevels (0:no, 1:yes)            ' ; IT(2*30) = 2
   OPTION(31)= 'show waterbal. on screen (0:no, 1:yes)  ' ; IT(2*31) = 2
   OPTION(32)= 'kplotfrombedorsurface (1:bed, 2:surf)   ' ; IT(2*32) = 2
   OPTION(33)= 'kplotordepthaveraged  (1:kplot, 2:averg)' ; IT(2*33) = 2

!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6
   HELPM (1)  = '1:hgl 2:ps 4:rgh 6:bmp 7:pcx 8:dxf 9:cgm 10:wpm 11:wmf 12gl2'
   HELPM (2)  = '1 = ENCAPSULATED POSTSCRIPT (eps), 0 = POSTSCRIPT (ps)      '
   HELPM (3)  = '1 = LANDSCAPE, 0 = PORTRAIT                                 '
   HELPM (4 ) = 'REAL VALUE, SIZE OF DOTS RELATIVE TO SCREENSIZE             '
   HELPM (5 ) = 'REAL VALUE, SIZE OF NUMBERS, STANDARD VALUE 0.5             '
   HELPM (6 ) = 'REAL VALUE, FOR MISSING POINTS (PARAMETER dmiss)            '
   HELPM (7 ) = 'REAL VALUE, LEFT MARGIN AS FRACTION SCREEN SIZE (0.0-0.25)  '
   HELPM (8 ) = 'REAL VALUE, BOTTOM MARGIN AS FRACTION OF SCREEN (0.0-0.25)  '
   HELPM (9 ) = 'PLOT AXIS, 1 = YES, 0 = NO                                  '
   HELPM (10) = 'REAL VALUE, 2 MEANS 1 CM ON SCREEN IS APPROXIMATELY 2 M/S   '
   HELPM (11) = '1= plot every vector, 2=plot every second vector etc.       '
   HELPM (12) = 'INTEGER PLOTTING INTERVAL                                   '
   HELPM (13) = 'PLOT TO FILE AFTER EACH NTEK, 1 = YES, 0 = NO               '
   HELPM (14) = 'IF -999, AUTO ADJUST TO BOTTOM                              '
   HELPM (15) = 'IF -999, AUTO ADJUST TO ZNOD                                '
   HELPM (16) = '1 = YES, 0 = NO                                             '
   HELPM (17) = 'press ;key in editpol with polygon present after zooming in '
   HELPM (18) = 'Enable/disable highlighting of nodes/links/nodmin. (1/0)    '
   HELPM (19) = 'Number of the net node to be highlighted.                   '
   HELPM (20) = 'Number of the net link to be highlighted.                   '
   HELPM (21) = 'Number of the flow node to be highlighted.                  '
   HELPM (22) = 'Number of the flow link to be highlighted.                  '
   HELPM (23) = 'Only plot node if waterdepth hs > wetplot                   '
   HELPM (24) = 'Plot raai with cheap perspective 1/0                        '
   HELPM (25) = 'Integer, <= mxgr                                            '
   HELPM (26) = '1=yes , 0=no , only for 3D                                  '
   HELPM (27) = '1=yes , 0=no                                                '
   HELPM (28) = 'Intentionally left blank                                    '
   HELPM (29) = '1=yes , 0=no                                                '
   HELPM (30) = '1=yes , 0=no                                                '
   HELPM (31) = '1=yes , 0=no                                                '
   HELPM (32) = '1=bed ,-1=surf                                              '
   HELPM (33) = '1=kplot , 2=depth averaged                                  '


   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL
   IR = 0
   DO I = 1,NUMPAR
      IL = IR + 1
      IR = IL + 1
      IX(IL) = 13
!      IX(IR) = 53
      IX(IR) = 95
      IY(IL) = I ! Many menu lines, use dense view to fit on screen.
      IY(IR) = I
!      IS(IL) = 40
      IS(IL) = 82
      IS(IR) = 10
      IT(IL) = 1001
    ENDDO


!  Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW     = NPOS(3)
   IXP    = NPOS(1) + (IWS-IW)/2
   IYP    = NPOS(2)
   IH     = IHS - 9

!  Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program)//' DISPLAY PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!  Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = , Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!  Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)

   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

!  NUMWNH = InfoWindow(1)
!  CALL IWinSelect(NUMWNH)

!  Define a new form by supplying arrays containing
!  field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

!  Define a help field and define help strings
!  for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   DO I = 1,NUMHCOPTs
      IF (IHCOPTS(1,I) .EQ. 22) JAEPS  = IHCOPTS(2,I)
      IF (IHCOPTS(1,I) .EQ. 5)  JALAND = IHCOPTS(2,I)
   ENDDO

   CALL IFORMPUTINTEGER(2*1 , NHCDEV)
   CALL IFORMPUTINTEGER(2*2 , JAEPS)
   CALL IFORMPUTINTEGER(2*3 , JALAND)
   CALL IFormPutDouble (2*4 , CR    ,'(F10.3)')
   CALL IFormPutDouble (2*5 , TSIZE ,'(F10.3)')
   CALL IFormPutDouble (2*6 , dmiss,'(F10.3)')
   CALL IFormPutDouble (2*7 , XLEFT,'(F10.3)')
   CALL IFormPutDouble (2*8 , YBOT ,'(F10.3)')
   CALL IFORMPUTINTEGER(2*9 , JAXIS)
   CALL IFormPutDouble (2*10, VFAC ,'(F10.3)')
   CALL IFormPutinteger(2*11, nvec) 
   CALL IFORMPUTINTEGER(2*12, NTEK)
   CALL IFORMPUTINTEGER(2*13, PLOTTOFILE)
   CALL IFormPutDouble (2*14, ZMINrai ,'(F10.3)')
   CALL IFormPutDouble (2*15, ZMAXrai ,'(F10.3)')
   CALL IFORMPUTINTEGER(2*16, JTEXTFLOW)
   CALL IFORMPUTINTEGER(2*17, numzoomshift)
   CALL IFORMPUTINTEGER(2*18, jaHighlight)
   CALL IFORMPUTINTEGER(2*19, nhlNetNode)
   CALL IFORMPUTINTEGER(2*20, nhlNetLink)
   CALL IFORMPUTINTEGER(2*21, nhlFlowNode)
   CALL IFORMPUTINTEGER(2*22, nhlFlowLink)
   CALL IFormPutDouble (2*23, wetplot,'(F10.5)')
   CALL IFormPutDouble (2*24, YFAC,'(F10.5)')
   CALL IFORMPUTINTEGER(2*25, jgrtek)    ! grain size fraction to plot
   CALL IFORMPUTINTEGER(2*26, ndraw(35)) ! 1/0
   CALL IFORMPUTINTEGER(2*27, ndraw(36)) ! 1/0
   CALL IFORMPUTINTEGER(2*29, jaOpenGL) ! 1/0
   CALL IFORMPUTINTEGER(2*30, ndraw(39)) ! 1/0
   CALL IFORMPUTINTEGER(2*31, ndraw(40)) ! show waterbal 
   CALL IFORMPUTINTEGER(2*32, kplotfrombedorsurface) ! kplotupordown 
   CALL IFORMPUTINTEGER(2*33, kplotordepthaveraged)  !  

   !  Display the form with numeric fields left justified
   !  and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!  check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.   &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN
           CALL IFORMGETINTEGER(2*1 , NHCDEV)
           CALL IFORMGETINTEGER(2*2 , JAEPS)
           CALL IFORMGETINTEGER(2*3 , JALAND)
           CALL IFormGetDouble (2*4 , CR)
           CALL IFormGetDouble (2*5 , TSIZE)
           CALL IFormGetDouble (2*6 , dmiss)
           CALL IFormGetDouble (2*7 , XLEFT)
           CALL IFormGetDouble (2*8 , YBOT )
           CALL IFORMGETINTEGER(2*9 , JAXIS)
           CALL IFormGetDouble (2*10, VFAC )
           CALL IFormGetinteger(2*11, nvec )
           CALL IFORMGETINTEGER(2*12, NTEK)
           CALL IFORMGETINTEGER(2*13, PLOTTOFILE)
           CALL IFormGetDouble (2*14, ZMINrai)
           CALL IFormGetDouble (2*15, ZMAXrai)
           CALL IFORMGETINTEGER(2*16, jtextflow)
           CALL IFORMGETINTEGER(2*17, numzoomshift)
           CALL IFORMGETINTEGER(2*18, jaHighlight)
           CALL IFORMGETINTEGER(2*19, nhlNetNode)
           CALL IFORMGETINTEGER(2*20, nhlNetLink)
           CALL IFORMGETINTEGER(2*21, nhlFlowNode)
           CALL IFORMGETINTEGER(2*22, nhlFlowLink)
           CALL IFormGetDouble (2*23, wetplot) 
           CALL IFormGetDouble (2*24, yfac) 
           CALL IFORMGETINTEGER(2*25, jgrtek )
           jgrtek = max(1,min(jgrtek,mxgr))
           CALL IFORMGETINTEGER(2*26, ndraw(35) ) 
           CALL IFORMGETINTEGER(2*27, ndraw(36) ) 
           CALL IFORMGETINTEGER(2*29, jaOpenGL  ) 
           CALL IFORMGETINTEGER(2*30, ndraw(39) ) 
           CALL IFORMGETINTEGER(2*31, ndraw(40) ) 
           CALL IFORMGETINTEGER(2*32, kplotfrombedorsurface ) 
           CALL IFORMGETINTEGER(2*33, kplotordepthaveraged  ) 

           RCIR  = CR*(X2 - X1)
           VFAC  = MAX( 0d0, VFAC)
           VFACFORCE  = MAX( 0d0, VFACFORCE)
           XLEFT = MAX( 0d0,(MIN(XLEFT,0.25d0) ) )
           YBOT  = MAX( 0d0,(MIN(YBOT ,0.25d0) ) )
           JAXIS = MIN( 1  ,(MAX(JAXIS,0) ) )
           IF (JAXIS .EQ. 1) THEN
              IF (XLEFT .EQ. 0) XLEFT = .15
              IF (YBOT  .EQ. 0) YBOT  = .10
           ENDIF
           !CALL NEWWORLD()
           call setwor(x1,y1,x2,y2)
           DO I = 1,NUMHCOPTS
              IF (IHCOPTS(1,I) .EQ. 22) IHCOPTS(2,I) = JAEPS
              IF (IHCOPTS(1,I) .EQ. 5)  IHCOPTS(2,I) = JALAND
           ENDDO
           CALL SETTEXTSIZE()
           if (plottofile == 1) then
               ndraw(10) = 1
           end if
       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE CHANGEDISPLAYPARAMETERS

subroutine getktoplot(kk,k)
 use m_flowgeom
 use m_flow
 if (kplotfrombedorsurface == 1) then  
     k = kbot(kk) - 1 + min( kplot, kmxn(kk) )
     k = min(k, ktop(kk) )
 else
     k = kbot(kk) + kmxn(kk) - kplot
     k = max(k, kbot(kk) )
 endif   
 end subroutine getktoplot 

subroutine getLtoplot(kk,k)
 use m_flowgeom
 use m_flow
 if (kplotfrombedorsurface == 1) then  
     k = Lbot(kk) - 1 + min( kplot, kmxL(kk) )
     k = min(k, Ltop(kk) )
 else
     k = Lbot(kk) + kmxL(kk) - kplot
     k = max(k, Lbot(kk) )
 endif   
 end subroutine getLtoplot 

     SUBROUTINE KPLOTPLUSMIN(IPM)
      USE M_FLOWGEOM
      USE M_FLOW
      use m_xbeach_data, only: itheta_view
      implicit none
      integer :: IP, IPM, NRLAY

      if (kmx >= 1) then

         ip = ipm
         if (kplotfrombedorsurface .ne. 1) then 
            ip = -1*ipm
         endif   

         KPLOT = KPLOT+ip
         kplot = max(1,min(kplot,kmx))
      
         CALL TEXTFLOW()
      else if ( jawave.eq.4 ) then
         itheta_view = max(min(itheta_view + sign(1,ipm), ntheta), 1)
      end if
     END SUBROUTINE KPLOTPLUSMIN

     SUBROUTINE nPLOTPLUSMIN(IPM)
     USE M_FLOW
     use M_flowgeom
     implicit none
     integer :: IPM, NRLAY


      IF (IPM == 1) THEN
!         nPLOT = MIN(nPLOT+1,ndx)
         nplot = nplot+1
         if ( nplot.gt.Ndx ) nplot = nplot - Ndx
      ELSE if (ipm == -1) then
!         nPLOT = MAX(nPLOT-1,1)
         nplot = nplot-1
         if ( nplot.lt.1 ) nplot = nplot + Ndx
      else
         nplot = ipm
      ENDIF
      if (kmx > 0) then
         NRLAY = KTOP(NPLOT) - KBOT(NPLOT) + 1
         KPLOT = MAX(1, MIN(KPLOT, NRLAY) )
      endif
      CALL TEXTFLOW()
      END SUBROUTINE nPLOTPLUSMIN


      SUBROUTINE EDITSAM(MODE,KEY)
      use m_samples
      USE M_MISSING
      use unstruc_colors
      use m_partitioninfo
      implicit none
      integer :: MODE, KEY
      double precision :: ddx
      integer :: jalinear
      integer :: jaspline
      integer :: jonce
      integer :: k, L1, L2
      integer :: newmode
      integer :: nlevel
      integer :: nput
      integer :: num
      integer :: numb
      integer :: numinp
      integer :: nwhat
      double precision :: ziso
      double precision :: ziso2
      double precision :: vmax2, vmin2, dv2, val2, ave
      integer :: ncols2, nv2, nis2, nie2, jaauto2, ierror
      COMMON /DEPMAX2/ VMAX2,VMIN2,DV2,VAL2(256),NCOLS2(256),NV2,NIS2,NIE2,JAAUTO2
      integer :: ndraw
      COMMON /DRAWTHIS/ ndraw(50)

      double precision :: xp, yp, rd
      integer :: mp, mps

      COMMON /HELPNOW/ WRDKEY,NLEVEL

      COMMON /ISOPOL/    ZISO,ZISO2,DDX,NUMINP,JASPLINE,JALINEAR

      CHARACTER TEX*26, WRDKEY*40

      TEX    = ' Edit Samples             '
      WRDKEY = TEX
      NLEVEL =  2
      NUM    =  0
      NWHAT  =  0
      NPUT   =  25
      NUMB   =  12
      MP     =  0
      MPS    = MP
      L1     = 0
      CALL SAVESAM()

!     user is editing samples: mark samples as unstructured
!      MXSAM = 0
!      MYSAM = 0
!      IPSTAT = IPSTAT_NOTOK

   10 CONTINUE
      CALL DRAWNU(KEY)
      CALL KTEXT(TEX,1,2,15)
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)
      IF (KEY .NE. 23) JONCE = 0

      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            RETURN
         ELSE
            CALL CHOICES(MODE,NUM,NWHAT,KEY)
         ENDIF
      ELSE IF (KEY >= 577) THEN ! Alt+letter switches edit mode.
        call selecteditmode(newmode, key)
        if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
        end if
      ELSE IF (KEY .EQ. 21) THEN
!        INS KEY
         MPS = MP
         CALL SAVESAM()
         IF (NPUT .EQ. 23 .OR. NPUT .EQ. 26 .OR. NPUT .EQ. 27 .or. &
             NPUT .EQ. 40 .OR. NPUT .EQ. 41 .or. NPUT .EQ. 49 .or. &
             NPUT .EQ. 50 .or. NPUT .EQ. 51 ) THEN
!           kijken welk punt bij deleten en bij oppakken, changen
            CALL ISPOI1(     XS,     YS,     NS,   XP,     YP,     MP)
         ENDIF
         IF ( NPUT .EQ. 23 .AND. MP .NE. 0) THEN
!           punt oppakken
            CALL CIRR(XP,YP,0)
            NPUT = 24
         ELSE IF (NPUT .EQ. 24 .AND. MP .NE. 0) THEN
!           punt neerzetten
            XS(MP) = XP
            YS(MP) = YP
            CALL KCIR(XP,YP,ZS(MP))
            NPUT   = 23
         ELSE IF (NPUT .EQ. 25) THEN
!           punt toevoegen
            CALL INCREASESAM(NS)
            IF (NS .GE. 1) THEN
               RD = ZS(NS)
            ELSE
               RD = ZISO
            ENDIF
            ! CALL TYPEVALUE(RD,KEY)
            NS     = NS + 1
            XS(NS) = XP
            YS(NS) = YP
            ZS(NS) = RD
            CALL KCIR(XP,YP,ZS(NS))
!           user is editing samples: mark samples as unstructured
            MXSAM = 0
            MYSAM = 0
            IPSTAT = IPSTAT_NOTOK
         ELSE IF ( NPUT .EQ. 26 .AND. MP .NE. 0) THEN
!           punt deleten
            CALL CIRR(XP,YP,0)
            DO 30 K = MP,NS
               XS(K) = XS(K+1)
               YS(K) = YS(K+1)
               ZS(K) = ZS(K+1)
   30       CONTINUE
            NS = NS - 1
!           user is editing samples: mark samples as unstructured
            MXSAM = 0
            MYSAM = 0
            IPSTAT = IPSTAT_NOTOK
         ELSE IF ( NPUT .EQ. 27 .AND. MP .NE. 0) THEN
!           punt in waarde veranderen
            RD = ZS(MP)
            CALL TYPEVALUE(RD,KEY)
            CALL KCIR(XP,YP,RD)
            ZS(MP) = RD
         ELSE IF ( NPUT == 40 .OR. NPUT == 41) THEN
            IF (MP .NE. 0) THEN
               IF (L1 == 0) THEN
                   L1   = MP
                   NPUT = 41
               ELSE
                   L2   = MP
                   NPUT = 40
                   CALL insertsamples(L1,L2)
                   L1   = 0
                   L2 = 0
                   KEY  = 3
               ENDIF
            ENDIF
         ELSE IF ( NPUT .EQ. 49 ) THEN ! Click sample point to set min value for isocol2
            KEY  = 3
            if (MP == 0) then ! Miss click: reset iscol2 scaling to auto.
                jaauto2 = 1
            else
                vmin2 = ZS(MP)
                jaauto2 = 0
                if (vmin2 > vmax2) then
                    key = 0
                end if
            end if
            call minmxsam()
         ELSE IF ( NPUT .EQ. 50 ) THEN ! Click sample point to set max value for isocol2
            KEY  = 3
            if (MP == 0) then ! Miss click: reset iscol2 scaling to auto.
                jaauto2 = 1
            else
                vmax2 = ZS(MP)
                jaauto2 = 0
                if (vmin2 > vmax2) then
                    key = 0
                end if
            end if
            call minmxsam()
         ELSE IF ( NPUT .EQ. 58 ) THEN ! Click start point of sample-based polygon
            call make_samplepath(xp,yp)
         ENDIF
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY
      ELSE IF (KEY .EQ. 23) THEN
!        ESC
         MP = MPS
         CALL RESTORESAM()
         KEY = 3
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
         CALL ISPOI1(     XS,     YS,     NS,  XP,     YP,     MP)
         IF (MP .NE. 0) THEN
            RD  = ZS(MP)
            MPS = MP
            CALL SAVESAM()
            CALL CHADEP(XP,YP,RD,KEY)
            ZS(MP) = RD
         ENDIF
      ELSE IF (KEY .EQ. 73 .OR. KEY .EQ. 73+32) THEN ! I
!        insert mode
         NPUT = 25
      ELSE IF (KEY .EQ. 8) THEN    ! Backspace KEY
!        delete all samples (within polygon if any) and stay in previous mode.
         call savesam()
         call delsam(1)
         key = 3
!        user is editing samples: mark samples as unstructured
         MXSAM = 0
         MYSAM = 0
         IPSTAT=IPSTAT_NOTOK
      ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32) THEN ! D
!       delete mode
         NPUT = 26
      ELSE IF (KEY .EQ. 70 .OR. KEY .EQ. 70+32) THEN ! f
         CALL SAVESAM()
         ns = 10
         call increasesam(ns)
         xs(1) = xp
         ys(1) = yp
         CALL TYPEVALUE(RD,KEY)
         zs(1) = rd
         NS = 1
         call flow_initfloodfill()
         call restoresam()
         key = 3
      ELSE IF (KEY .EQ. 82 .OR. KEY .EQ. 82+32 .AND. NPUT .NE. 24) THEN ! R
!       replace mode, maar niet bij zetten
         NPUT = 23
      ELSE IF (KEY .EQ. 76 .OR. KEY .EQ. 76+32) THEN  ! L
!        line mode
         NPUT = 40
      ELSE IF (KEY .EQ. 67 .OR. KEY .EQ. 67+32) THEN  ! C
!        change mode
         NPUT = 27
      ELSE IF (                 KEY .EQ. 77+32) THEN  ! m (case sensitive!)
!        click sample to set minimum for isocol2
         NPUT = 49
      ELSE IF (KEY .EQ. 77                    ) THEN  ! M (case sensitive!)
!        click sample to set maximum for isocol2
         NPUT = 50
      ELSE IF (KEY .EQ. 72 .OR. KEY .EQ. 72+32) THEN  ! H: hide/show samples
!        click sample to set maximum for isocol2
         ndraw(32) = -ndraw(32)
         key = 3
      ELSE IF (KEY .EQ. 98) THEN
!        b RINGS BELL
         CALL KTEXT('B Rings Bell',2,6,11)
         CALL OKAY(0)
      ELSE IF (KEY .EQ. 81 .or. KEY .EQ. 81+32 ) THEN ! Q (for testing only)
         call make_orthocenters(0.5d-2,1000)
!         call copy_sendlist_to_sam()
         NPUT = 58
      ENDIF

      GOTO 10

      END SUBROUTINE EDITSAM


      SUBROUTINE EDITNETW(MODE,KEY)
      use m_netw
      use unstruc_colors
      USE M_MISSING
      use unstruc_api
      use dfm_error
      use unstruc_messages
      use gridoperations
      implicit none
      integer :: MODE, KEY

      double precision :: ag
      double precision :: cfl
      double precision :: e0
      double precision :: eps
      integer :: newmode
      integer :: ja
      integer :: jadd
      integer :: k
      integer :: k1, k2, k3
      integer :: kp
      integer :: kpp
      integer :: LL 
      integer :: lnu
      integer :: ncol
      integer :: nl1
      integer :: nl2
      integer :: nlevel
      integer :: nput
      integer :: num
      integer :: numb
      integer :: nwhat
      integer :: ierror
      double precision :: pi
      double precision :: rho
      double precision :: rhow
      double precision :: xp1
      double precision :: yp1
      double precision :: zp1

      double precision :: xp, yp, zp, ZPP

      COMMON /HELPNOW/ WRDKEY,NLEVEL

      COMMON /CONSTANTS/ E0, RHO, RHOW, CFL, EPS, AG, PI

      CHARACTER TEX*26, WRDKEY*40
      integer :: iresult

      TEX    = ' Edit Network             '
      WRDKEY = TEX
      NLEVEL =  2
      NUM    =  0
      JA     =  0
      NWHAT  =  0
      NPUT   =  0
      NUMB   =  10
      JADD   =  2
      NCOL   =  NCOLDN
      K1     =  0
      KPP    =  0

      CALL SAVENET()

      K      = 0
      CALL BOTLIN(0,NUMB,KEY)

   10 CONTINUE
      CALL DRAWNU(KEY)
      CALL KTEXT(TEX,1,2,15)
      IF (JADD .EQ. 0) THEN
         CALL KTEXT(' DELETE NODES     ',1,3,15)          ! D
      ELSE IF (JADD .EQ. 1) THEN
         CALL KTEXT(' ADD NODES/ELMS   ',1,3,15)          ! I
      ELSE IF (JADD .EQ. 2) THEN
         CALL KTEXT(' REPLACE NODES    ',1,3,15)          ! R
      ELSE IF (JADD .EQ. 3) THEN
         CALL KTEXT(' MERGE NODES      ',1,3,15)          ! M
      ELSE IF (JADD .EQ. 4) THEN
         CALL KTEXT(' MERGE LINES      ',1,3,15)          ! O
      ELSE IF (JADD .EQ. 5) THEN
         CALL KTEXT(' CUT LINES        ',1,3,15)          ! C
      ELSE IF (JADD .EQ. 6) THEN
         CALL KTEXT(' DEL ND, LINK L/R ',1,3,15)          ! X
      ELSE IF (JADD .EQ. 7) THEN
         CALL KTEXT(' Toggle thin dam (LINKS) ',1,3,15)   ! T
      ELSE IF (JADD .EQ. 8) THEN
         CALL KTEXT(' Split LINES      ',1,3,15)          ! S
      ELSE IF (JADD .EQ. 88) THEN
         CALL KTEXT(' Insert meshline  ',1,3,15)          ! SHIFT-S
      ELSE IF (JADD .EQ. 9) THEN
         CALL KTEXT(' Toggle line attribute ',1,3,15)     ! 1
      ELSE IF (JADD .EQ. 10) THEN
         CALL KTEXT(' FIELD MOVE       ',1,3,15)          ! V
      ELSE IF (JADD .EQ. 11) THEN
         CALL KTEXT(' FIELD ROTATE     ',1,3,15)          ! R
      ELSE IF (JADD .EQ. 12) THEN
         CALL KTEXT(' ZKNODES          ',1,3,15)          ! +
      ELSE IF (JADD .EQ. 13) THEN
         CALL KTEXT(' TO LAND BOUNDARY ',1,3,15)          ! L
      ELSE IF (JADD .EQ. 14) THEN
         CALL KTEXT(' KILL CELL        ',1,3,15)          ! K
      ELSE IF (JADD .eq. 15) THEN
         CALL KTEXT(' ADD CELL LAYER   ',1,3,15)          ! E
      ENDIF
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)

      CALL SETCOL(NCOLDN)
      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            RETURN
         ELSE
            CALL CHOICES(MODE,NUM,NWHAT,KEY)
         ENDIF
      ELSE IF (KEY >= 577) THEN ! Alt+letter switches edit mode.
        call selecteditmode(newmode, key)
        if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
        end if
      ELSE IF (KEY .EQ. 21) THEN
!        INS KEY OF LINKERMUIS, kijken welk punt

         CALL ISNODE(KP, XP, YP, ZP)

         CALL SAVENET()
         IF (JADD .EQ. 1) THEN   ! insert mode
            netstat = NETSTAT_CELLS_DIRTY
            IF (KP .EQ. 0) THEN
               ! CALL GIVENEWNODENUM(KP)
               CALL SETNEWPOINT(XP,YP,ZP,KP)
            ELSE
               CALL DCIRR (XK(KP),YK(KP),ZK(KP),NCOLDN)
            ENDIF



            IF (K1 .NE. 0) THEN
               CALL CONNECTDBN(K1,KP,LNU)
                CALL TEKLINK(LNU,NCOLDN)
              ! CALL DMOVABS(XK(K1),YK(K1),ZK(K1))
              ! CALL  DLNABS(XK(KP),YK(KP),ZK(KP))

            ENDIF
            K1   = KP
            NPUT = 39

         ELSE IF (JADD .EQ. 2) THEN  !replace mode
            netstat = NETSTAT_CELLS_DIRTY
            IF (KP .NE. 0 .AND. NPUT .EQ.0 ) THEN
               NPUT = 1
               KPP  = KP
               ZPP = ZP
               CALL TEKNODE(KP,0)
            ELSE IF (KPP .NE. 0) THEN
               NPUT = 0
               CALL SAVENET()
               CALL SETPOINT(XP,YP,ZPP,KPP)
               CALL TEKNODE(KPP,NCOLDN)
               KPP = 0
            ENDIF
         ELSE IF (JADD .EQ. 3) THEN   ! MERGE NODES
            netstat = NETSTAT_CELLS_DIRTY
            IF (KP .NE. 0) THEN       !
               IF ( K1 .EQ. 0 ) THEN
!                 punt 1
                  K1  = KP
                  KP = -KP ! FLAG TO ISNODE; DO NOT AGAIN LOOK FOR THIS POINT
                  XP1 = XP
                  YP1 = YP
                  ZP1 = ZP
                  CALL DCIRR (XK(K1),YK(K1),ZK(K1),NCOLDN)
                  NPUT = 39
               ELSE
!                 punt 2
                  K2  = KP
                  CALL DCIRR (XK(K1),YK(K1),ZK(K1),0     )
                  CALL TEKNODE(K1,0)
                  CALL SAVENET()
                  CALL MERGENODES(K1,K2,JA)
                  CALL TEKNODE(K2,NCOLDN)
                  K1   = 0
                  K2   = 0
                  NPUT = 38
               ENDIF
            ELSE                    ! NO FIND
               CALL OKAY(0)
            ENDIF
         ELSE IF (JADD .EQ. 5 .and. kp .ne. 0) THEN   ! C - key now free for change ZK value
            zp1 = Zk(kP)
            CALL TYPEVALUE(zp1,KEY)
            CALL KCIR(XP,YP,zp1)
            Zk(kP) = zp1
         ELSE IF (JADD .EQ. 6) THEN   ! DELETE NODE, CONNECT LEFT/RIGHT
            netstat = NETSTAT_CELLS_DIRTY
            IF (KP .NE. 0) THEN       ! CUT LINE
               IF (NMK(KP) .EQ. 2) THEN
!                 punt 1
                  CALL TEKNODE(KP,0)

                  NL1  = NOD(KP)%LIN(1)
                  CALL TEKLINK(NL1,0)
                  CALL OTHERNODE(KP,NL1,K1)

                  NL2  = NOD(KP)%LIN(2)
                  CALL TEKLINK(NL2,0)
                  CALL OTHERNODE(KP,NL2,K2)

                  CALL CONNECTDBN(K1,K2,LNU)
                  CALL TEKLINK (LNU,NCOLDN)
                  CALL DELLINK(NL1)
                  CALL DELLINK(NL2)
                  NPUT = 0
               ENDIF
            ELSE                    ! NO FIND
               CALL OKAY(0)
            ENDIF
         ELSE IF (JADD .EQ. 0) THEN !delete mode
            netstat = NETSTAT_CELLS_DIRTY
            IF (KP .NE. 0) THEN
               CALL TEKNODE(KP,0)
               CALL SAVENET()
               CALL DELNODE(KP)
            ELSE
               CALL ISLINK(LL, XP, YP, ZP)
               IF (LL .NE. 0) THEN
                  CALL TEKLINK(LL,0)
                  CALL DELLINK(LL)
               ENDIF
            ENDIF
         ELSE IF (JADD .EQ. 7) THEN ! thin dam toggle mode
            IF (KP == 0) THEN
               CALL ISLINK(LL, XP, YP, ZP)
               IF (LL /= 0) THEN
                  KN(3,LL) = -KN(3,LL)
                  CALL TEKLINK(LL,NCOLDN)
               ENDIF
            ENDIF
         ELSE IF (JADD .EQ. 8) THEN ! split line
            IF (KP == 0) THEN
               call splitlink(xp, yp, 0, 0.9d0, 1, ierror)   ! use (xp,yp) and no link specified, use cos parallelogram tolerance and plot
            ENDIF
         ELSE IF (JADD .EQ. 88) THEN ! insert meshline
            IF (KP == 0) THEN
               call insert_netline(xp, yp, 0) ! , 1)
            ENDIF
         ELSE IF (JADD .EQ. 9) THEN ! line attribute TOGGLE , 1d OR 2d
            IF (KP == 0) THEN
               CALL ISLINK(LL, XP, YP, ZP)
               IF (LL /= 0) THEN
                  IF ( kn(3,LL) == 2) THEN
                     CALL TEKLINK(LL,221)
                     kn(3,LL) = 1
                  ELSE IF ( kn(3,LL) == 1) THEN
                     CALL TEKLINK(LL,3)
                     kn(3,LL) =  2
                  ENDIF
               ENDIF
            ENDIF
         ELSE IF (JADD .EQ. 10) THEN ! Field move
            IF (KP .NE. 0 .AND. NPUT .EQ.0 ) THEN
               NPUT = 1
               KPP  = KP
               ZPP = ZP
               CALL TEKNODE(KP,0)
            ELSE IF (KPP .NE. 0) THEN
               CALL SAVENET()
               CALL TEKNODE(KPP,NCOLDN)
               call netmodfld(xp,yp,zpp,kpp)
               NPUT  = 0
               kpp   = 0
               KEY   = 3
            ENDIF
         ELSE IF (JADD .EQ. 11) THEN ! Field rotate
            IF (KP .NE. 0 .AND. NPUT .EQ.0 ) THEN
               NPUT = 1
               KPP  = KP
               ZPP = ZP
               CALL TEKNODE(KP,0)
            ELSE IF (KPP .NE. 0) THEN
               CALL SAVENET()
               CALL TEKNODE(KPP,NCOLDN)
               call netrotfld(xp,yp,zpp,kpp)
               NPUT  = 0
               kpp   = 0
               KEY   = 3
            ENDIF
         ELSE IF (JADD .EQ. 12) THEN ! Field rotate
            IF (KP .NE. 0) THEN
!              punt in waarde veranderen
               CALL TYPEVALUE(ZP,KEY)
               CALL KCIR(XP,YP,ZP)
               ZK(KP) = ZP
            ENDIF
         ELSE IF (JADD .EQ. 15 ) THEN  ! Add cell layer
            IF (KP .NE. 0) THEN
               call netboundtocurvi(kp)
               KEY = 3
            ENDIF
         ENDIF
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY ENKEL DISPLAY
         CALL ISNODE(KP, XP, YP, ZP)
         IF (KP .NE. 0) THEN
            CALL DISPNODEVALS(KP)
         ELSE
            iresult = FLOW()
            if (iresult == DFM_SIGINT) then
               call mess(LEVEL_ERROR, 'Final handling of SIGINT signal. Stopping program.')
               call STOPINT()
            else if (iresult /= DFM_NOERR) then
               call qnerror('Error occurred while running, please inspect your diagnostic output.',' ', ' ')
            end if
         ENDIF
      ELSE IF (KEY .EQ. 23) THEN
!        ESCAPE KEY
         CALL RESTORE()
         KEY   = 3
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
      ELSE IF (KEY .EQ. 73 .OR. KEY .EQ. 73+32) THEN    ! I-key
         JADD = 1
         K1   = 0
         K2   = 0
         NPUT = 38
      ELSE IF (KEY .EQ. 8) THEN    ! Backspace KEY
!        delete entire network (within polygon if any) and stay in previous mode.
         call delnet(key,0, 1)
         K1 = 0
         K2 = 0
         key = 3
      ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32) THEN    ! D-key
!        delete mode
         JADD = 0
         K1   = 0
         K2   = 0
         NPUT = -2
      ELSE IF (KEY .EQ. 82 .OR. KEY .EQ. 82+32) THEN    ! R-key
!        replace mode, maar niet bij zetten
         JADD = 2
         K1   = 0
         K2   = 0
         NPUT = 0
      ELSE IF (KEY .EQ. 88 .OR. KEY .EQ. 88+32) THEN    ! X-key
!        DELNODE, CONNECT LEFT/RIGHT
         JADD = 6
         K1   = 0
         K2   = 0
         NPUT = -2
      ELSE IF (KEY .EQ. 77 .OR. KEY .EQ. 77+32) THEN    ! M-key  MERGE
         JADD =  3
         K1   =  0
         K2   =  0
         NPUT = 38
      ELSE IF (KEY .EQ. 99 .OR. KEY .EQ. 99+32) THEN    ! C-key  Change ZK value
         JADD =  5
         K1   =  0
         NPUT =  60
      ELSE IF (KEY .EQ. 33 .or. KEY .EQ. 49) THEN       ! 1, 1D link
         CALL ISLINK(LL, XP, YP, ZP)
         IF (LL /= 0) THEN
             kn(3,LL) = 1
             CALL TEKLINK(LL,1)
         ENDIF!123
      ELSE IF (KEY .EQ. 34 .or. KEY .EQ. 50) THEN       ! 2, 2D link
         CALL ISLINK(LL, XP, YP, ZP)
         IF (LL /= 0) THEN
             kn(3,LL) = 2
             CALL TEKLINK(LL,1)
         ENDIF
      ELSE IF (KEY .EQ. 35 .or. KEY .EQ. 51) THEN       ! 3, 1d2d internal
         CALL ISLINK(LL, XP, YP, ZP)
         IF (LL /= 0) THEN
             kn(3,LL) = 3
             CALL TEKLINK(LL,1)
         ENDIF
      ELSE IF (KEY .EQ. 36 .or. KEY .EQ. 52) THEN       ! 4, 1d2d lateral
         CALL ISLINK(LL, XP, YP, ZP)
         IF (LL /= 0) THEN
             kn(3,LL) = 4
             CALL TEKLINK(LL,1)
         ENDIF
      ELSE IF (KEY .EQ. 37 .or. KEY .EQ. 53) THEN       ! 5, 1d2d pipe
         CALL ISLINK(LL, XP, YP, ZP)
         IF (LL /= 0) THEN
             kn(3,LL) = 5
             CALL TEKLINK(LL,1)
         ENDIF
      ELSE IF (KEY .EQ. 38 .or. KEY .EQ. 54) THEN       ! 6, 1d branch 
         CALL ISLINK(LL, XP, YP, ZP)
         IF (LL /= 0) THEN
             kn(3,LL) = 6
             CALL TEKLINK(LL,1)
         ENDIF
      ELSE IF (KEY .EQ. 71 .OR. KEY .EQ. 71+32) THEN    ! G-key  netw2curv
         CALL NETW2CURV(XP,YP)
         KEY = 3
      ELSE IF (KEY .EQ. 86 .OR. KEY .EQ. 86+32) THEN    ! V-key  fieldmove
         JADD = 10
         K1   = 0
         K2   = 0
         NPUT = 0
      ELSE IF (KEY .EQ. 66 .OR. KEY .EQ. 66+32) THEN    ! B-key  fieldrotate
         JADD = 11
         K1   = 0
         K2   = 0
         NPUT = 0
      ELSE IF (KEY .EQ. 76 .OR. KEY .EQ. 76+32) THEN    ! L-key  nettoland
         JADD = 13
         CALL SAVENET()
         call nettoland()
         KEY  = 3
         NPUT = 38
      ELSE IF (KEY .EQ. 75 ) THEN                       ! K-key  derefine_mesh
         CALL SAVENET()
         call derefine_mesh(xp,yp,.true.)
      ELSE IF ( KEY .EQ. 75+32) THEN                    ! k-key  killcell
         CALL SAVENET()
         call killcell(xp,yp)
      ELSE IF (KEY .EQ. 70 .OR. KEY .EQ. 70+32) THEN    ! F-key  FIXED POINT
         CALL ISNODE(KP, XP, YP, ZP)
         IF (KP .NE. 0) THEN
            CALL SAVENET()
            IF (KC(KP) .EQ. -1) THEN
                KC(KP) = 1
                NCOL   = 0
            ELSE
                KC(KP) = -1
                NCOL   = NCOLDN
            ENDIF
            CALL DCIRR(XK(KP),YK(KP),ZK(KP),NCOL)
            CALL TEKNODE(KP,NCOLDN)
         ENDIF
      ELSE IF (KEY .EQ. 76 .OR. KEY .EQ. 76+32) THEN    ! L-key AANRIJGPUNT
         CALL ISNODE(KP, XP, YP, ZP)                    ! LINE
         IF (KP .NE. 0) THEN
            CALL SAVENET()
            IF (KC(KP) .EQ.  2) THEN
                KC(KP) = 4
                NCOL   = 0
            ELSE
                KC(KP) =  2
                NCOL   = NCOLRN
            ENDIF
            CALL DCIRR(XK(KP),YK(KP),ZK(KP),NCOL)
            CALL TEKNODE(KP,NCOLRN)
         ENDIF
      ELSE IF (KEY .EQ. 79 .OR. KEY .EQ. 79+32) THEN    ! O-key ONELINE
         CALL ISNODE(KP, XP, YP, ZP)
         JADD = 4
         IF (KP .NE. 0) THEN
            CALL TEKNODE(KP,0)
            CALL ONELINE(KP,99999d0)
         ENDIF
      ELSE IF (KEY .EQ. 84 .OR. KEY .EQ. 84+32) THEN    ! T-key
!        thin dam mode
         JADD = 7
         K1   = 0
         K2   = 0
         NPUT = 55
      ELSE IF (KEY .EQ. 83+32) THEN                     ! S-key
!        split link
         JADD = 8
         K1   = 0
         K2   = 0
         NPUT = 55
      ELSE IF (KEY .EQ. 83) THEN                        ! SHIFT-S-key
!        insert meshline
         JADD = 88
         K1   = 0
         K2   = 0
         NPUT = 55
      ELSE IF (KEY .EQ. 69 .OR. KEY .EQ. 69+32) THEN    ! E-key
!        add layer of cells
         JADD = 15
         K1   = 0
         K2   = 0
         NPUT = 59
      ELSE IF (KEY .EQ. 43 .OR. KEY .EQ. 43+32) THEN    ! +-key
!        CHANGE ZK VALUE mode
         JADD = 12
      ELSE IF (KEY .EQ. 44) THEN                        ! ,-key
!        INVERT JINS
         JINS = (1-JINS)
      ELSE IF (KEY .EQ. 86 .OR. KEY .EQ. 86+32) THEN    ! V-key
         CALL VIEWCYCLE(KEY)
      ELSE IF (KEY .EQ. 32) THEN
         call flow_spatietimestep()
         key = 3
!      ELSE IF (KEY .EQ. 75 .or. KEY .eq. 75+32) THEN  ! K-KEY
      ELSE IF (KEY .EQ. 96 ) THEN  ! `-KEY
         call checknetwork()
         !key = 3
      ELSE IF (KEY .EQ. 81 .OR. KEY .EQ. 81+32) THEN   ! Q-key
!         call bilin_interp(numk, xk, yk, zk)          ! testing subroutine
!         call net_delete_DMISS()
!         call sam2net_curvi()
         key = 3  ! redraw
         
    !     call removecell(xp,yp)
         call create_samples_in_triangle()
    !     call fix_global_polygons(1,0)
      ENDIF
!
      GOTO 10
!
      END SUBROUTINE EDITNETW

     SUBROUTINE EDITflow(MODE,KEY,NL)
      use m_netw
      use m_flowgeom, only : iadv
      use m_flow
      use unstruc_colors
      USE M_MISSING
      use unstruc_api
      use m_snappol
      use dfm_error
      use unstruc_messages
      use gridoperations
      use unstruc_display, only: idisLink, dis_info_1d_link, nhlFlowLink
      use m_inquire_flowgeom
      implicit none
      integer :: MODE, KEY, kb , kt ,k, NL
      integer :: newmode
      integer :: ncol, nput
      integer :: nlevel
      integer :: KK=0, LL, L
      integer :: num
      integer :: numb
      integer :: nwhat
      double precision :: xp, yp, zp, ZNOD

      double precision :: vmax, vmin, dv, val
      integer          :: ncols, nv, nis, nie, jaauto

      integer                                     :: i, Nin, Nout, ierror
      double precision, dimension(:), allocatable :: xin, yin, xout, yout  ! testing, for snappol
      integer,          dimension(:), allocatable :: ipoLout    ! testing, for snappol

      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO


      COMMON /HELPNOW/ WRDKEY,NLEVEL

      CHARACTER TEX*26, WRDKEY*40
      character(len=IdLen) :: strucid
      integer :: iresult
   
      TEX    = ' Edit FLOW            '
      WRDKEY = TEX
      NLEVEL =  2
      NUM    =  0
      NWHAT  =  0
      NPUT   =  NL
      NUMB   =  16
      NCOL   =  NCOLDN
      L      =  0

      CALL SAVENET()

      CALL BOTLIN(0,NUMB,KEY)

   10 CONTINUE
      CALL DRAWNU(KEY)
      CALL KTEXT(TEX,1,2,15)
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)

      CALL SETCOL(NCOLDN)
      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            RETURN
         ELSE
            CALL CHOICES(MODE,NUM,NWHAT,KEY)
         ENDIF
      ELSE IF (KEY >= 577) THEN ! Alt+letter switches edit mode.
        call selecteditmode(newmode, key)
        if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
        end if
      ELSE IF (KEY .EQ. 21) THEN
!        INS KEY OF LINKERMUIS, kijken welk punt

         ! key = 3
         IF (NPUT .EQ. 51 .or. NPUT == 53 .or. NPUT == 54) THEN   ! NODE mode
            call isflownode1D2D(xp, yp, KK)
            if (kk > 0) then
               nplot = kk
               call tekprofs()
               call textflow()
            endif
            CALL DISND(KK)
         ELSE IF (NPUT .EQ. 52 .or. NPUT .EQ. 57 ) THEN   ! LINK mode
            call isflowlink(xp, yp, LL)
            
            if (nput == 57 .and. LL > 0 ) then
                zp = iadv(LL)
                CALL TYPEVALUE(zp,KEY)
                iadv(LL) = int(zp)
            endif
            if ( nput.eq.52 .and. LL.gt.0 ) then
               call plotklnup(LL)

               if (abs(kcu(LL)) /= 2) then
                  idisLink = LL ! Save the link index for later display
                  call dis_info_1d_link(LL)
                  nhlFlowLink = LL
                  call highlight_nodesnlinks()
               end if
            end if

         ENDIF

         IF ( NPUT .EQ. 53 ) THEN ! Click flow node to set min value for isocol
            KEY  = 3
            if (KK == 0) then ! Miss click: reset iscol scaling to auto.
                jaauto = 1
            else
                vmin = znod(KK)
                jaauto = 0
                if (vmin > vmax) then
                    key = 0
                end if
            end if
            call minmxnds()
         ELSE IF ( NPUT .EQ. 54 ) THEN ! Click flow node to set max value for isocol
            KEY  = 3
            if (KK == 0) then ! Miss click: reset iscol scaling to auto.
                jaauto = 1
            else
                vmax = znod(KK)
                jaauto = 0
                if (vmin > vmax) then
                    key = 0
                end if
            end if
            call minmxnds()
         ENDIF

      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY ENKEL DISPLAY
         iresult = FLOW()
         if (iresult == DFM_SIGINT) then
            call mess(LEVEL_ERROR, 'Final handling of SIGINT signal. Stopping program.')
            call STOPINT()
         else if (iresult /= DFM_NOERR) then
            call qnerror('Error occurred while running, please inspect your diagnostic output.',' ', ' ')
         end if
         key = 3
      ELSE IF (KEY .EQ. 23) THEN
!        ESCAPE KEY
         KEY   = 3
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
      ELSE IF (KEY .EQ. 78 .OR. KEY .EQ. 78+32) THEN    ! N-key voor node mode
         NPUT = 51
      ELSE IF (KEY .EQ. 76 .OR. KEY .EQ. 76+32) THEN    ! L-key voor link mode
         NPUT = 52
      ELSE IF (KEY .EQ. 73 .OR. KEY .EQ. 73+32) THEN    ! I-key voor setiadvec mode
         NPUT = 57
      ELSE IF (                 KEY .EQ. 77+32) THEN    ! m (case sensitive!)
!        click flow node to set minimum for isocol
         NPUT = 53
      ELSE IF (KEY .EQ. 77                    ) THEN    ! M (case sensitive!)
!        click flow node to set maximum for isocol
         NPUT = 54
      ELSE IF (KEY .EQ. 86 .OR. KEY .EQ. 86+32) THEN    ! V-key
         CALL VIEWCYCLE(KEY)
      ELSE IF (KEY .EQ. 81 .OR. KEY .EQ. 81+32) THEN    ! Q-key stop flow info screen display for 1D flowlink
         idisLink = 0
         nhlFlowLink = 0
         key = 3
      else if (KEY == 72 .or. KEY == 72+32) then        ! H-key search for a hydraulic structure
         call getstring(' SEARCH: structure id = ', strucid)
         iresult = findlink(strucid, L)
         if (L > 0 .and. L <= lnx) then
            nhlFlowLink = L
            call highlight_nodesnlinks()
         end if
      else if (KEY == 70 .or. KEY == 70+32) then        ! F-key search for a flowlink
         call GETINT(' SEARCH: flowlink =  ', L)
         if (L > 0 .and. L <= lnx) then
            nhlFlowLink = L
            call highlight_nodesnlinks()
         end if
      ELSE IF (KEY .EQ. 83 .OR. KEY .EQ. 83+32) THEN    ! S-key add salt
         if (jasal > 0) then
            call getkbotktop(nplot,kb , kt )
            k = kb + kplot - 1
            sa1(k) = sa1(k) + 1d0
         endif
      ELSE IF (KEY .EQ. 43 .or. KEY .EQ. 140) THEN     ! -
         CALL KPLOTPLUSMIN(-1)
         key = 3
      ELSE IF (KEY .EQ. 45 .or. KEY .EQ. 141) THEN      ! +
         call KPLOTPLUSMIN(1)
         key = 3
      ELSE IF (KEY .EQ. 42) THEN                        ! *
         CALL nPLOTPLUSMIN(1)
         key = 3
      ELSE IF (KEY .EQ. 47) THEN                        ! /
         call nPLOTPLUSMIN(-1)
         key = 3
      ELSE IF (KEY .EQ. 32) THEN
         call flow_spatietimestep()
         key = 3
      ELSE IF (KEY .EQ. 119 .or. KEY .EQ. 119-32) then ! w key write diff with obs
         call write_flowdiff()
      ELSE IF (KEY .EQ. 81 .OR. KEY .EQ. 81+32) THEN    ! Q-key: snap polygon to flow network
         Nin = NPL
         allocate(xin(Nin), yin(Nin))
         do i=1,Nin
            xin(i) = XPL(i)
            yin(i) = YPL(i)
         end do
         !call snappol(Nin, xin, yin, DMISS, Nout, Xout, Yout, ipoLout, ierror)
         !call snappnt(Nin, xin, yin, DMISS, Nout, Xout, Yout, ipoLout, ierror)
         if ( KEY.eq.81 ) then
            call snapbnd('dischargebnd', Nin, xin, yin, DMISS, Nout, Xout, Yout, ipoLout, ierror)
         else
            call snapbnd('waterlevelbnd', Nin, xin, yin, DMISS, Nout, Xout, Yout, ipoLout, ierror)
         end if
         NPL = Nout
         call increasepol(NPL,0)
         do i=1,Nout
            XPL(i) = xout(i)
            YPL(i) = yout(i)
            ZPL(i) = dble(ipoLout(i))
         end do
         if ( allocated(xin)     ) deallocate(xin, yin)
         if ( allocated(xout)    ) deallocate(xout, yout)
         if ( allocated(ipoLout) ) deallocate(ipoLout)
         
      else if ( key.ge.49 .and. key.le.57 ) then   ! keypad, for moving around
         call moveprobe(key-48,kk,xp,yp)
         if (kk > 0) then
            nplot = kk
            call tekprofs()
            call textflow()
         endif
         CALL DISND(KK)
      ENDIF
!
      GOTO 10
!
      END SUBROUTINE EDITflow

      SUBROUTINE FIELDOPT(NFLD)
      USE M_GRID
      implicit none
      integer :: nfld
      integer, PARAMETER :: MAXOP = 64
      integer :: nwhat2, maxexp, maxopt, i
      CHARACTER*40 OPTION(MAXOP),EXP(MAXOP),FIELDOP
      EXP(1)    = 'MENU 10                                 '
      EXP(2)    = 'GRID EDIT OPTIONS                       '
      MAXOPT    = 22
      DO 10 I = 1,MAXOPT
         OPTION(I) =  FIELDOP(I)
    10 CONTINUE
      NWHAT2  = NFLD
      CALL MENUV2(NWHAT2,OPTION,MAXOPT,EXP,MAXEXP)
      IF (NWHAT2 .GE. 1) THEN
         IF (NWHAT2 == 19) THEN
            CALL ORTHOGRID(1,1,MC,NC)
         else IF (NWHAT2 == 20) THEN
            call LOCALREFINE(Nwhat2, 1, 1, mc, nc, 1)
         else IF (NWHAT2 == 21) THEN
            call LOCALREFINE(Nwhat2, 1, 1, mc, nc, 2)
         ELSE
            NFLD = NWHAT2
         ENDIF
      ENDIF
      RETURN
      END subroutine fieldopt

      FUNCTION FIELDOP(NUM)
      implicit none
      integer :: num
      CHARACTER*40 FIELDOP
      IF (NUM .EQ. 1) THEN
         FIELDOP = 'Point Mode                              '
      ELSE IF (NUM .EQ. 2) THEN
         FIELDOP = 'Field Mode                              '
      ELSE IF (NUM .EQ. 3) THEN
         FIELDOP = '                                        '
      ELSE IF (NUM .EQ. 4) THEN
         FIELDOP = 'Line Shift                              '
      ELSE IF (NUM .EQ. 5) THEN
         FIELDOP = 'Line Attraction                         '
      ELSE IF (NUM .EQ. 6) THEN
         FIELDOP = 'Line Repulsion                          '
      ELSE IF (NUM .EQ. 7) THEN
         FIELDOP = 'Line to Land Boundary                   '
      ELSE IF (NUM .EQ. 8) THEN
         FIELDOP = 'Line to Spline (only to spline nr 1)    '
      ELSE IF (NUM .EQ. 9) THEN
         FIELDOP = 'Line Smooth                             '
      ELSE IF (NUM .EQ. 10) THEN
         FIELDOP = 'Line Mirror                             '
      ELSE IF (NUM .EQ. 11) THEN
         FIELDOP = 'Refine Grid Locally                     '
      ELSE IF (NUM .EQ. 12) THEN
         FIELDOP = 'Derefine Grid Locally                   '
      ELSE IF (NUM .EQ. 13) THEN
         FIELDOP = '                                        '
      ELSE IF (NUM .EQ. 14) THEN
         FIELDOP = 'Block Delete                            '
      ELSE IF (NUM .EQ. 15) THEN
         FIELDOP = 'Block Cut                               '
      ELSE IF (NUM .EQ. 16) THEN
         FIELDOP = 'Block Orthogonalise                     '
      ELSE IF (NUM .EQ. 17) THEN
         FIELDOP = 'Block Smooth                            '
      ELSE IF (NUM .EQ. 18) THEN
         FIELDOP = '                                        '
      ELSE IF (NUM .EQ. 19) THEN
         FIELDOP = 'Orthogonise whole grid                  '
      ELSE IF (NUM .EQ. 20) THEN
         FIELDOP = 'Refine globally                         '
      ELSE IF (NUM .EQ. 21) THEN
         FIELDOP = 'Derefine globally                       '
      ELSE IF (NUM .EQ. 22) THEN
         FIELDOP = 'Back to Main Edit Modes                 '
      ENDIF
      RETURN
      END function fieldop


      SUBROUTINE EDITGRID(MODE,NFLD,KEY)
      use unstruc_colors
      use m_grid
      implicit none
      integer :: mode, nfld, key

      integer :: L, NLEVEL, JA, NUM, NWHAT, NPUT, NUMB, MP, NP, MD, ND, &
                 ML, NL, MH, NH, NUMP, NLOC, IN, JN, INSIDE, ndraw, NCOL
      integer :: newmode

      COMMON /HELPNOW/ WRDKEY,NLEVEL
      COMMON /DRAWTHIS/ ndraw(50)

      CHARACTER TEX*20, WRDKEY*40, FIELDOP*40

      double precision :: xp, yp, wf(4)

      TEX    =  ' '//FIELDOP(NFLD)
      L      =  len_trim(TEX)
      WRDKEY =  FIELDOP(NFLD)
      NLEVEL =  3
      JA     =  0
      NUM    =  0
      NWHAT  =  0
      NPUT   =  0
      NUMB   =  17
      NCOL   =  NCOLDG

      MP     = 0
      NP     = 0
      CALL BOTLIN(0,NUMB,KEY)

    10 CONTINUE
      CALL DRAWNU(KEY)
      CALL KTEXT(TEX,1,2,15)
      CALL KTEXT(' Click Grid Points  ',1,3,15)
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)

      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            RETURN
         ELSE
            CALL CHOICES(MODE,NUM,NWHAT,KEY)
         ENDIF
      ELSE IF (KEY >= 577) THEN ! Alt+letter switches edit mode.
        call selecteditmode(newmode, key)
        if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
        end if
      ELSE IF (KEY .EQ. 21) THEN
!        INS KEY
         IF (NPUT .EQ. 0 .OR. NPUT .EQ. -2) THEN
!           kijken welk punt bij deleten en bij oppakken
            CALL ISPOIN(     xc,     yc,     mmax, nmax, MC,     NC,   zc,    &
                             XP,     YP,     MP,     NP)
         ENDIF
         IF ( NPUT .EQ. 0 .AND. MP .NE. 0) THEN
!           punt oppakken
            CALL TEKGRPT(     xc,     yc,     mmax, nmax, MC,     NC,           &
                              MP,     NP,      0        )
            NPUT = 1
         ELSE IF (NPUT .EQ. 1 .AND. MP .NE. 0) THEN
!           punt neerzetten
            IF (NFLD .EQ. 1) THEN
               CALL SAVEGRD()
               xc(MP,NP) = XP
               yc(MP,NP) = YP
               CALL TEKGRPT(     xc,     yc,     mmax, nmax, MC,     NC,  &
                                 MP,     NP,   NCOL        )
            ELSE IF (NFLD .EQ. 2) THEN
               NUMP = 80
               NLOC = 1
               ML   = MAX(1,MP-NUMP)
               MH   = MIN(MC,MP+NUMP)
               NL   = MAX(1,NP-NUMP)
               NH   = MIN(NC,NP+NUMP)
               CALL TEKGRD(xc,yc,mmax, nmax, ML,NL,MH,NH,0,NDRAW(38),key,mc)
               CALL TEKGRD(xch,ych,mmax, nmax, ML,NL,MH,NH,0,NDRAW(16),key,mch)
               CALL SAVEGRD()
               xc(MP,NP) = XP
               yc(MP,NP) = YP
               CALL MODFLD(     xc,     yc,    xch,    ych,   mmax, nmax, &
                                MC,     NC,     MP,     NP,   &
                              NUMP,   NLOC,      1,      1)
               CALL TEKGRD(xc,yc,mmax, nmax, ML,NL,MH,NH,NCOL,NDRAW(38),key,mc)
               CALL TEKGRD(xch, ych, mmax, nmax, ML, NL, MH, NH, NCOLRG, NDRAW(16),key,mch)
            ENDIF
            NPUT   = 0
         ELSE IF (NPUT .EQ. -1) THEN
!           punt toevoegen
            CALL FINDNM(     XP,     YP,     xc,     yc, mmax, nmax,     &
                             MC,     NC, INSIDE,             &
                             MP,     NP,     IN,     JN, wf)
            IF (INSIDE .EQ. 1) THEN
               CALL SAVEGRD()
               CALL MODGR1(NPUT,             &! xc,  yc, mmax, nmax, MC, NC,
                           MP, NP, IN, JN)!, NCOL)
            ELSE
               CALL OKAY(0)
            ENDIF
         ELSE IF ( NPUT .EQ. -2 .AND. MP .NE. 0) THEN
!           punt deleten
            CALL SAVEGRD()
            CALL TEKGRPT(     xc,     yc,     mmax, nmax, MC,     NC,    &
                              MP,     NP,      0        )
            CALL MODGR1(NPUT,                &!xc, yc, mmax, nmax, MC, NC,
                        MP, NP, IN, JN)!, NCOL)
         ENDIF
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY ENKEL DISPLAY
         CALL ISPOIN(     xc,     yc,     mmax, nmax, MC,     NC,   zc, &
                          XP,     YP,     MD,     ND)
      ELSE IF (KEY .EQ. 23) THEN
!        ESCAPE KEY
         CALL RESTOREGRD()
         KEY   = 3
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
         !CALL SHWXYZ(xc, yc, zc,MC,NC,0,KEY,M,N)
      ELSE IF (KEY .EQ. 73 .OR. KEY .EQ. 73+32) THEN
         IF (NPUT .NE. 1) THEN
!           kijken welk punt dit is t.b.v insert mode
            CALL ISPOIN(     xc,     yc,     mmax, nmax, MC,     NC,   zc,  &
                             XP,     YP,     MP,     NP)
         ENDIF
         NPUT = -1
      ELSE IF (KEY .EQ. 8) THEN    ! Backspace KEY
!        delete entire network (within polygon if any) and stay in previous mode.
         call delgrd(KEY,1,1)
         key = 3
      ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32) THEN
!        delete mode
         NPUT = -2
      ELSE IF (KEY .EQ. 82 .OR. KEY .EQ. 82+32 .AND. NPUT .NE. 1) THEN
!        replace mode, maar niet bij zetten
         NPUT =  0
      ELSE IF (KEY .EQ. 85 .OR. KEY .EQ. 85+32 ) THEN ! U-KEY, UPDATE PARTITIONING COUNT
         CALL TEKnumnetcells(1)
         KEY = 3
      ELSE IF (KEY .EQ. 98) THEN
!        b RINGS BELL
         CALL KTEXT(' B Rings Bell',2,6,11)
         CALL OKAY(0)
      ELSE IF (KEY .EQ. 76 .OR. KEY .EQ. 76+32) THEN
!        CALL TEKHOOK(XP,YP)
      ENDIF
!
      GOTO 10
!
      END subroutine editgrid


      SUBROUTINE EDITGRIDLINEBLOK(MODE,NFLD,KEY)
      use unstruc_colors
      use m_grid
      implicit none

      integer :: mode, nfld, key
      integer :: newmode
      integer :: ndraw, nlevel, bm, nb, mb2, nb2, npt, npt2, nputo, itype, NCOL
      integer :: jonce

      COMMON /DRAWTHIS/ ndraw(50)
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      CHARACTER TEX*20, WRDKEY*40, FIELDOP*40
      integer :: num, nwhat, numb, nump, mp, np, ipt, ja, mb, m1b, n1b, m2b, n2b, m1, n1, m2, n2, m, n
      integer :: nput
      double precision :: xp, yp

      TEX    = ' '//FIELDOP(NFLD)
      WRDKEY = FIELDOP(NFLD)
      NLEVEL = 3
      NUM    = 0
      NWHAT  = 0
      NUMB   = 6
      NCOL   = NCOLRG
      NUMP   = 80
      MP     = 0
      NP     = 0
      ITYPE  = 1

      NPUT   = 10
      CALL RESETB(NPUT)
      CALL BOTLIN(0,NUMB,KEY)

    10 CONTINUE
      CALL DRAWNU(KEY)
      CALL TEKB(Xc,Yc,MMAX,NMAX,NCOLLN)
      CALL KTEXT(TEX,1,2,15)
      IF (NPT .LE. 1) THEN
         CALL KTEXT(' Indicate a Line    ',1,3,15)
      ELSE
         CALL KTEXT(' Influence or rght M',1,3,15)
      ENDIF

      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)
      IF (KEY .NE. 23) JONCE = 0

      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            CALL TEKB(Xc,Yc,MMAX,NMAX,0)
            RETURN
         ELSE
            CALL CHOICES(MODE,NUM,NWHAT,KEY)
         ENDIF
      ELSE IF (KEY >= 577) THEN ! Alt+letter switches edit mode.
        call selecteditmode(newmode, key)
        if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
        end if
      ELSE IF (KEY .EQ. 21) THEN
!        INS KEY
!        kijken welk punt
         CALL ISPOIN(     Xc,     Yc,     mmax, nmax, MC,     NC,   Zc,  &
                          XP,     YP,     MP,     NP)
         IF (MP .NE. 0) THEN
            IF (NPUT .EQ. 16) THEN
               CALL ONSAMELINE(IPT,MP,NP,JA)
               IF (JA .EQ. 1) THEN
                  MB(IPT) = MP
                  NB(IPT) = NP
                  CALL CIRR(Xc(MP,NP), Yc(MP,NP), NCOLLN)
                  IF (NPT .EQ. 1) NPUT = 11
                  IF (NPT .EQ. 2) NPUT = 14
                  IF (NPT .EQ. 3) NPUT = 15
                  IF (NPT .EQ. 4) NPUT = 19
               ELSE
                  CALL QNERROR('POINT 1 AND 2 SHOULD LIE',     &
                               'ON THE SAME GRIDLINE',' ')
               ENDIF
            ELSE
               CALL NEWBLOCKPOINT(MP,NP,JA,IPT)
               IF (JA .EQ. 1) THEN
!                 voeg punt toe
                  CALL ONSAMELINE(IPT,MP,NP,JA)
                  IF (JA .EQ. 1) THEN
                     CALL SAVEB(NPUT)
                     NPT = NPT + 1
                     MB(NPT) = MP
                     NB(NPT) = NP
                     CALL CIRR(Xc(MB(NPT),NB(NPT)), Yc(MB(NPT),NB(NPT)),NCOLLN)
                     IF (NPT .EQ. 1) NPUT = 11
                     IF (NPT .EQ. 2) NPUT = 14
                     IF (NPT .EQ. 3) NPUT = 15
                     IF (NPT .EQ. 4) NPUT = 19
                  ELSE
                     CALL QNERROR('POINT 1 AND 2 SHOULD LIE','ON THE SAME GRIDLINE',' ')
                  ENDIF
               ELSE IF (JA .EQ. -1) THEN
!                 niet meer toevoegen
                  CALL QNERROR('4 POINTS: CONTINUE = RIGHT MOUSE OR', 'Enter,',' ')
               ELSE IF (JA .EQ. 0) THEN
!                 oud punt geclickt; uitgummen
                  CALL SAVEB(NPUT)
                  CALL CIRR(Xc(MB(IPT),NB(IPT)),Yc(MB(IPT),NB(IPT)),0)
                  IF (IPT .LE. 2) CALL TEKB(Xc,Yc,MMAX,NMAX,0)
                  MB(IPT) = 0
                  NB(IPT) = 0
                  NPUT    = 16
               ENDIF
            ENDIF
         ENDIF
      ELSE IF (KEY .EQ. 22) THEN
         IF (NPT .LE. 1) THEN
           CALL QNERROR('FIRST PRESS MORE POINTS WITH LEFT MOUSE BUTTON',' ',' ')
         ELSE
!           ENTER KEY
            CALL TEKB(Xc,Yc,MMAX,NMAX,0)
            CALL POSITIVEBLOK()
            M1B = MAX(MB(3)-1,1)
            N1B = MAX(NB(3)-1,1)
            M2B = MIN(MB(4)+1,MC)
            N2B = MIN(NB(4)+1,NC)
            IF (NFLD .NE. 4) THEN
               CALL TEKGRD(Xc,Yc,mmax, nmax, M1B,N1B,M2B,N2B,0,NDRAW(38),key,mc)
            ENDIF
!           Begin Operatie
            CALL SAVEGRD()
            IF (NFLD .EQ. 4) THEN
               M1 = MB(1)
               M2 = MB(2)
               N1 = NB(1)
               N2 = NB(2)
               CALL EDITGRIDLINESHIFT(MODE,NFLD,KEY,M1,N1,M2,N2)
               IF (KEY .NE. 23) THEN
                  CALL TEKGRD(      Xc,     Yc,    mmax, nmax, M1B,           &
                                   N1B,    M2B,    N2B,0,NDRAW(38),key,mc)
                  CALL MODGR2(     Xc,     Yc,     Xch,     Ych,   mmax, nmax, &
                                   MC,     NC,   NUMP)
               ENDIF
            ELSE IF (NFLD .EQ. 5) THEN    ! Attraction
               CALL ATTRACTREPULSE(     Xc,     Yc,     Xch,   Ych, &
                                      mmax,   nmax, &
                                        MC,     NC,   NUMP,     -1)
            ELSE IF (NFLD .EQ. 6) THEN    ! Repulsion
               CALL ATTRACTREPULSE(     Xc,     Yc,     Xch,   Ych, &
                                      mmax,   nmax, &
                                        MC,     NC,   NUMP,      1)
            ELSE IF (NFLD .EQ. 7) THEN
               CALL MODGR4( NUMP,1  )
            ELSE IF (NFLD .EQ. 8) THEN
               CALL MODGR4( NUMP,2  )
            ELSE IF (NFLD .EQ. 9) THEN
               CALL DOSMOOTH(NFLD)!Xc,Yc,mmax, nmax, MC,NC,NFLD,IJC,IJYES)
            ELSE IF (NFLD .EQ. 10) THEN
               CALL LINEMIRROR()!Xc,Yc,mmax, nmax, MC,NC,IJC,IJYES)
            ELSE IF (NFLD .EQ. 11) THEN
               M1 = MB(1)
               M2 = MB(2)
               N1 = NB(1)
               N2 = NB(2)
               CALL LOCALREFINE(NUM, m1, n1, m2, n2, 1)
            ELSE IF (NFLD .EQ. 12) THEN
               M1 = MB(1)
               M2 = MB(2)
               N1 = NB(1)
               N2 = NB(2)
               CALL LOCALREFINE(NUM, m1, n1, m2, n2, 2)
            ENDIF
!           Einde Operatie
            CALL TEKGRD(      Xc,     Yc,    mmax,   nmax, M1B,                  &
                             N1B,    M2B,    N2B, NCOLDG, NDRAW(38),key,mc)
            CALL TEKGRD(     Xch,    Ych,    mmax,   nmax, M1B,                  &
                             N1B,    M2B,    N2B, NCOLRG, NDRAW(16),key,mch)
            IF (NPT .LE. 2) KEY = 3
            CALL RESETB(NPUT)
            NPUT = 10
         ENDIF
      ELSE IF (KEY .EQ. 23) THEN
!        ESC
         JONCE = JONCE + 1
         IF (JONCE .EQ. 1) THEN
            CALL RESTOREB(NPUT)
         ELSE IF (JONCE .EQ. 2) THEN
            NPUT = 10
            CALL RESETB(NPUT)
         ELSE IF (JONCE .EQ. 3) THEN
            CALL RESTOREgrd()
         ENDIF
         KEY = 3
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
         CALL SHWXYZ(Xc,Yc,Zc,mmax, nmax, MC,NC,0,KEY,M,N)
      ENDIF
!
      GOTO 10
!
      END subroutine editgridlineblok


      SUBROUTINE EDITGRIDLINESHIFT(MODE,NFLD,KEY,M1,N1,M2,N2)
      use m_grid
      use unstruc_colors
      implicit none
      integer :: MODE, NFLD, KEY, M1, N1, M2, N2
      integer :: newmode
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      CHARACTER TEX*20, WRDKEY*40, FIELDOP*40

      INTEGER :: NLEVEL, JA, NUM, NWHAT, NPUT, NUMB, JONCE, mp, np, m, n, NCOL
      double precision :: xp, yp
      TEX    =  ' '//FIELDOP(NFLD)
      WRDKEY =  FIELDOP(NFLD)
      NLEVEL =  3
      JA     =  0
      NUM    =  0
      NWHAT  =  0
      NPUT   =  20
      NUMB   =  7
      NCOL   =  NCOLRG
      JONCE  =  0

      MP     = 0
      NP     = 0
      CALL BOTLIN(0,NUMB,KEY)

    10 CONTINUE
      CALL DRAWNU(KEY)
      CALL KTEXT(TEX,1,2,15)
      CALL KTEXT(' Now Shift the Line ',1,3,15)
      CALL TEKLN2(Xc, Yc, mmax, nmax, M1, N1, M2, N2, NCOL)

    20 CONTINUE
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)
      IF (KEY .NE. 23) JONCE = 0

      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            RETURN
         ELSE
            CALL QNERROR('Menu is disabled, leave SHIFT LINE ',    &
                         '(Esc or right mouse button)',' ')
            NUM = 0
!           CALL CHOICES(MODE,NUM,NWHAT,KEY)
         ENDIF
      ELSE IF (KEY >= 577) THEN ! Alt+letter switches edit mode.
        call selecteditmode(newmode, key)
        if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
        end if
      ELSE IF (KEY .EQ. 21) THEN
!        INS KEY
         IF (NPUT .EQ. 20) THEN
!           kijken welk punt bij oppakken
            CALL ISPOIN(     Xc,     Yc,     mmax, nmax, MC,     NC,   Zc,    &
                             XP,     YP,     MP,     NP)
!           moet wel op lijn liggen
            IF (M1 .EQ. M2) THEN
               IF (MP .EQ. M1) THEN
                  IF (NP .LT. N1 .OR. NP .GT. N2) THEN
                     CALL QNERROR('Only shift points on the indicated','line',' ')
                     MP = 0
                  ENDIF
               ELSE
                  CALL QNERROR('Only shift points on the indicated','line',' ')
                  MP = 0
               ENDIF
            ENDIF
            IF (N1 .EQ. N2) THEN
               IF (NP .EQ. N1) THEN
                  IF (MP .LT. M1 .OR. MP .GT. M2) THEN
                     CALL QNERROR('Only shift points on the indicated','line',' ')
                     MP = 0
                  ENDIF
               ELSE
                  CALL QNERROR('Only shift points on the indicated','line',' ')
                  MP = 0
               ENDIF
            ENDIF
         ENDIF
         IF ( NPUT .EQ. 20 .AND. MP .NE. 0) THEN
!           punt oppakken
            CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC,        &
                              MP,     NP,      0        )
            NPUT = 1
         ELSE IF (NPUT .EQ. 1 .AND. MP .NE. 0) THEN
!           punt neerzetten
            Xc(MP,NP) = XP
            Yc(MP,NP) = YP
            CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC,        &
                              MP,     NP,   NCOL        )
            NPUT   = 20
         ENDIF
         GOTO 20
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY
         RETURN
      ELSE IF (KEY .EQ. 23) THEN
!        ESCAPE KEY
         JONCE = JONCE + 1
         IF (JONCE .EQ. 1) THEN
            CALL RESTOREgrd()
            KEY = 3
         ELSE
            RETURN
         ENDIF
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
         CALL SHWXYZ(Xc,Yc,Zc,mmax, nmax, MC,NC,0,KEY,M,N)
      ELSE IF (KEY .EQ. 98) THEN
!        b RINGS BELL
         CALL KTEXT('B Rings Bell',2,6,11)
         CALL OKAY(0)
      ENDIF
!
      GOTO 10
!                           7
      END subroutine editgridlineshift


      SUBROUTINE EDITGRIDBLOK(MODE,NFLD,KEY)
      use m_grid
      use unstruc_colors
      implicit none

      integer :: mode, nfld, key
      integer :: newmode

      integer :: ndraw, nlevel, num, nwhat, numb, mp, np
      COMMON /DRAWTHIS/ ndraw(50)
      COMMON /HELPNOW/ WRDKEY,NLEVEL

      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      CHARACTER TEX*20, WRDKEY*40, FIELDOP*40
      integer :: m1b, n1b, m2b, n2b, ipt, ja, jonce, m, n, nput
      double precision :: xp, yp

      TEX    = ' '//FIELDOP(NFLD)
      WRDKEY = FIELDOP(NFLD)
      NLEVEL = 3
      NUM    = 0
      NWHAT  = 0
      NUMB   = 8
      MP     = 0
      NP     = 0
      ITYPE  = 2
      jonce  = 0

      NPUT   = 8
      CALL RESETB(NPUT)
      CALL BOTLIN(0,NUMB,KEY)

    10 CONTINUE
      CALL DRAWNU(KEY)
      CALL TEKB(Xc,Yc,MMAX,NMAX,NCOLLN)
      CALL KTEXT(TEX,1,2,15)
      CALL KTEXT(' Indicate a Block   ',1,3,15)

      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)
      IF (KEY .NE. 23) JONCE = 0

      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            CALL TEKB(Xc,Yc,MMAX,NMAX,0)
            RETURN
         ELSE
            CALL CHOICES(MODE,NUM,NWHAT,KEY)
         ENDIF
      ELSE IF (KEY >= 577) THEN ! Alt+letter switches edit mode.
        call selecteditmode(newmode, key)
        if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
        end if
      ELSE IF (KEY .EQ. 21) THEN
!        INS KEY
!        kijken welk punt
         CALL ISPOIN(     Xc,     Yc,     mmax, nmax, MC,     NC,   Zc, &
                          XP,     YP,     MP,     NP)
         IF (MP .NE. 0) THEN
            IF (NPUT .EQ. 16) THEN
               MB(IPT) = MP
               NB(IPT) = NP
               CALL CIRR(Xc(MP,NP), Yc(MP,NP), NCOLLN)
               IF (NPT .EQ. 1) NPUT = 9
               IF (NPT .EQ. 2) NPUT = 17
               IF (NPT .EQ. 3) NPUT = 18
               IF (NPT .EQ. 4) NPUT = 19
            ELSE
               CALL NEWBLOCKPOINT(MP,NP,JA,IPT)
               IF (JA .EQ. 1) THEN
!                 voeg punt toe
                  CALL SAVEB(NPUT)
                  NPT = NPT + 1
                  MB(NPT) = MP
                  NB(NPT) = NP
                  CALL CIRR(Xc(MB(NPT),NB(NPT)),Yc(MB(NPT),NB(NPT)),NCOLLN)
                  IF (NPT .EQ. 1) NPUT = 9
                  IF (NPT .EQ. 2) NPUT = 17
                  IF (NPT .EQ. 3) NPUT = 18
                  IF (NPT .EQ. 4) NPUT = 19
               ELSE IF (JA .EQ. -1) THEN
!                 niet meer toevoegen
                  CALL QNERROR('4 POINTS: CONTINUE = RIGHT MOUSE OR', 'Enter,',' ')
               ELSE IF (JA .EQ. 0) THEN
!                 oud punt geclickt; uitgummen
                  CALL SAVEB(NPUT)
                  CALL CIRR(Xc(MB(IPT),NB(IPT)),Yc(MB(IPT),NB(IPT)),0)
                  IF (IPT .LE. 2) CALL TEKB(Xc,Yc,MMAX,NMAX,0)
                  MB(IPT) = 0
                  NB(IPT) = 0
                  NPUT    = 16
               ENDIF
            ENDIF
         ENDIF
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY
         IF (NPT .LE. 1) THEN
           CALL QNERROR('FIRST PRESS MORE POINTS WITH LEFT MOUSE BUTTON',' ',' ')
         ELSE
            CALL TEKB(Xc,Yc,MMAX,NMAX,0)
            CALL POSITIVEBLOK()
            M1B = MAX(MB(3)-1,1)
            N1B = MAX(NB(3)-1,1)
            M2B = MIN(MB(4)+2,MC)
            N2B = MIN(NB(4)+2,NC)
            CALL TEKGRD(      Xc,     Yc,    mmax, nmax, M1B,           &
                             N1B,    M2B,    N2B,0,NDRAW(38), key, mc)
            if (allocated(xch)) then
               CALL TEKGRD(     Xch,    Ych,    mmax, nmax, M1B,           &
                                N1B,    M2B,    N2B,0,NDRAW(16), key, mc)
            end if

!           Begin Operatie
            CALL SAVEgrd()
            IF (NFLD .EQ. 14) THEN
               CALL NULFIELD(Xc,Yc, mmax, nmax)
            ELSE IF (NFLD .EQ. 15) THEN
               CALL CUTFIELD(Xc,Yc,mmax, nmax, MC,NC)
            ELSE IF (NFLD .EQ. 16) THEN
               !CALL ORTHO(X, Y, MB(3), NB(3), MB(4), NB(4), MC, NC, NUM, MMAX,NMAX)!!!
               CALL ORTHOGRID(MB(3), NB(3), MB(4), NB(4))
            ELSE IF (NFLD .EQ. 17) THEN
               CALL DOSMOOTH(NFLD) !Xc,Yc,mmax, nmax, MC,NC,NFLD,IJC,IJYES)
            ENDIF
!           Einde Operatie
            CALL TEKGRD(     Xc,     Yc,    mmax, nmax, M1B,                  &
                             N1B,    M2B,    N2B, NCOLDG, NDRAW(38), key, mc)
            if (allocated(xch)) then
               CALL TEKGRD(     Xch,    Ych,    mmax, nmax, M1B,                   &
                                N1B,    M2B,    N2B, NCOLRG, NDRAW(16), key, mc)
            end if

            IF (NFLD .EQ. 14) THEN
               IF (MB(3) .EQ. 1 .OR. MB(4) .EQ. MC .OR.    &
                   NB(3) .EQ. 1 .OR. NB(4) .EQ. NC    ) THEN
                   CALL ADJUST(Xc, Yc, mmax, nmax, MC, NC)
               ENDIF
            ELSE IF (NFLD .EQ. 15) THEN
                CALL ADJUST(Xc, Yc, mmax, nmax, MC, NC)
                KEY = 3
            ENDIF
            CALL RESETB(NPUT)
            NPUT = 8
         ENDIF
      ELSE IF (KEY .EQ. 23) THEN
!        ESC
         JONCE = JONCE + 1
         IF (JONCE .EQ. 1) THEN
            CALL RESTOREB(NPUT)
         ELSE IF (JONCE .EQ. 2) THEN
            NPUT = 10
            CALL RESETB(NPUT)
         ELSE IF (JONCE .EQ. 3) THEN
            CALL RESTOREgrd()
         ENDIF
         KEY = 3
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
         CALL SHWXYZ(Xc,Yc,Zc,mmax, nmax, MC,NC,0,KEY,M,N)
      ENDIF
!
      GOTO 10
!
      END subroutine editgridblok


      SUBROUTINE TEKNET(NCOL,ja)
      
      use m_netw
      use unstruc_colors
      use geometry_module, only: dbdistance
      use gridoperations
      
      implicit none
      integer :: ncol, ja

      integer :: k, LMOD
      integer :: k0
      integer :: k1
      integer :: k2
      integer :: k3
      integer :: kk
      integer :: L, LL
      integer :: n
      integer :: ndraw
      double precision :: d1, d2, x, y
!      double precision :: t0, t1
      integer :: is, ie, ip
      integer :: iflip = 1


      COMMON /DRAWTHIS/ ndraw(50)

      IF (NDRAW(2) .LE. 0 .or. NUML == 0 ) RETURN

!      call klok(t0)

     if (ndraw(2) .ne. 3) then  ! net zelf
      
        ! iflip = -iflip
        ! if (.false. .and. allocated(netlinkpath_xk) .and. iflip==1) then
        ! write (*,*) 'Fast plotter'
        ! is = 1
        ! CALL SETCOL(NCOL)
        ! do L=1,numpath
        !    ie = netlinkpath_end(L)
        !    call POLYLINE(netlinkpath_xk(is:ie), &
        !                     netlinkpath_yk(is:ie), &
        !                     ie-is+1)
        !    is = ie+1
        ! end do
             
         call setcol(ncoldn)
         DO L = 1,NUML
            if (ja.ne.-1234 .and. mod(L,500) == 0) then
                call halt2(ja)
                if (ja == 1) exit
            endif

            if (kn(3,L) == 2) then  
               K1 = KN(1,L)
               K2 = KN(2,L)
               IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
                  IF (INVIEW(XK(K1),YK(K1)) .OR. INVIEW(XK(K2),YK(K2)) ) THEN
                     CALL MOVABS( XK(K1),YK(K1))   
                     CALL  LNABS( XK(K2),YK(K2))
                  ENDIF
               ENDIF
            endif   
         ENDDO
            
         CALL SETCOL(NCOLNN)
         DO K = 1,NUMK
            if (ja.ne.-1234 .and. mod(k,500) == 0) then
               call halt2(ja)
               if (ja == 1) exit
            endif

            if ( INVIEW( XK(K),YK(K) ) ) then
               CALL PTABS(XK(K),YK(K))
            ENDIF
         ENDDO
         
         if (ndraw(2) == 4) then 
            call setcol(ncoldg)
            Do L = 1, numl
               if (kn(3,L) == 1 .or. kn(3,L) == 3 .or. kn(3,L) == 4) then  
                  k1 = kn(1,L)
                  x  = xk(k1)
                  y  = yk(k1) 
                  call fbox(x-0.5d0*rcir,y-0.5d0*rcir,x+0.5d0*rcir,y+0.5d0*rcir)
                  k1 = kn(2,L)
                  x  = xk(k1)
                  y  = yk(k1) 
                  call fbox(x-0.5d0*rcir,y-0.5d0*rcir,x+0.5d0*rcir,y+0.5d0*rcir)
               endif
            enddo
         endif  
         
         DO L = 1,NUML
             if (ja.ne.-1234 .and. mod(L,500) == 0) then
                call halt2(ja)
                if (ja == 1) exit
             endif
             K3 = KN(3,L)
             if (k3 .ne. 2) then  
                K1 = KN(1,L)
                K2 = KN(2,L)
                IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
                   IF (INVIEW(XK(K1),YK(K1)) .OR. INVIEW(XK(K2),YK(K2)) ) THEN
                      CALL MOVABS( XK(K1),YK(K1))  ; 
                      CALL  LNABS( XK(K2),YK(K2))
                      CALL SETLINKCOLOUR(L,1)
                      CALL MOVABS( XK(K1),YK(K1))  ; 
                      call CIR(1.2d0*rcir)
                      CALL  LNABS( XK(K2),YK(K2))
                      call CIR(1.2d0*rcir)
                   ENDIF
                ENDIF
             endif   
         ENDDO
      endif


      IF ( (NDRAW(2) == 2 .or. NDRAW(2) == 3 ).AND. SIZE(LNN) .GE. NUML) THEN !outline
         CALL SETCOL(NCOLRN)
         LMOD = MAX(1,NUML/100)

         DO L = 1,NUML
             if (ja.ne.-1234 .and. mod(L,LMOD) == 0) then
                call halt2(ja)
                if (ja == 1) exit
             endif
             IF (LNN(L) == 1) THEN
                K1 = KN(1,L)
                K2 = KN(2,L)
                IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
                   CALL MOVABS( XK(K1),YK(K1) )
                   CALL  LNABS( XK(K2),YK(K2) )
                ENDIF
             ENDIF
         ENDDO
      ENDIF

      IF ( NDRAW(2) == 4) THEN
         CALL TEKXZ(221)
      ENDIF

      IF (NDRAW(22) .GE. 2) CALL TEKFACES()

      if (NDRAW(2)==5) then
        ! Draw link crossings (precomputed by checknet)
        DO L = 1,nlinkcross
          call TEKLINK (linkcross(1,L),NCOLWARN1)
          call TEKLINK (linkcross(2,L),NCOLWARN2)

          LL = linkcross(1,L)
          if (kn(1,linkcross(1,L)) <= 0 .or. kn(1,linkcross(1,L)) > numk .or. &
              kn(2,linkcross(1,L)) <= 0 .or. kn(2,linkcross(1,L)) > numk .or. &
              kn(1,linkcross(2,L)) <= 0 .or. kn(1,linkcross(2,L)) > numk .or. &
              kn(2,linkcross(2,L)) <= 0 .or. kn(2,linkcross(2,L)) > numk) cycle
          d1 = max(abs(xk(kn(2,linkcross(1,L)))-xk(kn(1,linkcross(1,L)))), &
                   abs(yk(kn(2,linkcross(1,L)))-yk(kn(1,linkcross(1,L)))))

          d2 = max(abs(xk(kn(2,linkcross(2,L)))-xk(kn(1,linkcross(2,L)))), &
                   abs(yk(kn(2,linkcross(2,L)))-yk(kn(1,linkcross(2,L)))))

          ! If zoom is very small: plot large dots to mark crossings clearly.
          if (max(d1, d2) < 2*RCIR) then
              CALL CIRR(xk(kn(1,linkcross(1,L))), yk(kn(1,linkcross(1,L))), NCOLWARN1)
          end if
        end do

        ! Also draw bad orthogonality links (precomputed by cosphiucheck)
        ! and too short flow links (precomputed by flow_geominit) .
        DO L = 1,nlinkbadortho+nlinktoosmall
          LL = linkbadqual(L)
          if (LL <= 0 .or. LL > numl) cycle
          if (kn(1,LL) <= 0 .or. kn(1,LL) > numk .or. &
              kn(2,LL) <= 0 .or. kn(2,LL) > numk) cycle
          call TEKLINK (LL,NCOLWARN3)
          d1 = max(abs(xk(kn(2,LL))-xk(kn(1,LL))), &
                   abs(yk(kn(2,LL))-yk(kn(1,LL))))

          ! If zoom is very small: plot large dots to mark crossings clearly.
          if (d1 < 2*RCIR) then
              CALL CIRR(xk(kn(1,LL)), yk(kn(1,LL)), NCOLWARN3)
          end if
        end do
      end if

!      call klok(t1)

!      write(6,"('time elapsed in teknet: ', F15.5, 'seconds')") t1-t0

      RETURN
      END SUBROUTINE TEKNET

      SUBROUTINE TEKXZ(NCOL)
      use m_netw
      USE M_FLOWGEOM
      implicit none
      INTEGER :: NCOL
      integer :: n
      double precision :: bar

      DO N = 1,NUMP
         CALL DCIRR ( xz(n), yz(n), YZw(N), NCOL )
      ENDDO

      RETURN
      END SUBROUTINE TEKXZ


      SUBROUTINE TEKFACES()
      
      use unstruc_colors
      use m_netw
      use sorting_algorithms, only: indexx
      use gridoperations
      
      implicit none
      integer :: ierr
      integer :: k
      integer :: l
      integer :: n
      integer :: ncol
      integer :: ni

      DOUBLE PRECISION   XX,YY,ZZ, XH(10), YH(10), ZH(10)
      INTEGER, ALLOCATABLE, SAVE        :: NP(:)
      double precision :: XP, YP
      double precision, ALLOCATABLE, SAVE        :: ZP(:)

      IF (SIZE(NP) .LT. NUMP) THEN
         IF ( ALLOCATED(NP) ) DEALLOCATE(NP,ZP)
         ALLOCATE (NP(NUMP),ZP(NUMP),STAT = IERR)
      ENDIF

      IF (NUMP .NE. 0) THEN
         DO N  = 1, NUMP
            XX = 0
            YY = 0
            ZZ = 0
            DO K = 1,netcell(N)%N
               XX = XX + XK(netcell(N)%NOD(K))
               YY = YY + YK(netcell(N)%NOD(K))
               ZZ = ZZ + ZK(netcell(N)%NOD(K))
            ENDDO
            XX = XX/netcell(N)%N
            YY = YY/netcell(N)%N
            ZZ = ZZ/netcell(N)%N
            CALL DRIETWEE(XX,YY,ZZ,XP,YP,ZP(N))
         ENDDO
         call indexx(NUMP,ZP,NP)

         DO L  = NUMP, 1, -1
            N  = NP(L)
            NI = netcell(N)%N
            DO K = 1, NI
               XH(K) = XK(netcell(N)%NOD(K))
               YH(K) = YK(netcell(N)%NOD(K))
               ZH(K) = ZK(netcell(N)%NOD(K))
            ENDDO
            IF (NI .EQ. 6) THEN
               NCOL = 221
            ENDIF
            IF (NI .EQ. 5) THEN
               NCOL = 111
            ENDIF
            IF (NI .EQ. 4) THEN
               NCOL =  31
            ENDIF
            IF (NI .EQ. 3) THEN
               NCOL = 171
            ENDIF
            CALL PFILLER (XH,YH,NI,NCOL,NCOLLN)
         ENDDO
      ENDIF

      RETURN
      END SUBROUTINE TEKFACES

      SUBROUTINE TEKPREVIOUSNET(NCOL)
      use m_netw
      implicit none
      integer :: NCOL

      integer :: k
      integer :: k1
      integer :: k2
      integer :: l
      integer :: ndraw

      COMMON /DRAWTHIS/ ndraw(50)
      IF (NDRAW(16) .LE. 0) RETURN
      CALL SETCOL(NCOL)
      DO L = 1,NUML0
         K1 = KN0(1,L)
         K2 = KN0(2,L)
         IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
            CALL DMOVABS( XK0(K1),YK0(K1),ZK0(K1) )
            CALL  DLNABS( XK0(K2),YK0(K2),ZK0(K2) )
         ENDIF
      ENDDO
      END SUBROUTINE TEKPREVIOUSNET

SUBROUTINE DISPFLOWNODEVALS(KP)
  use m_flowgeom
  use m_flow
  USE M_DEVICES

  implicit none
  DOUBLE PRECISION :: ZNOD
  integer :: KP

  integer :: l
  integer :: n
  CHARACTER TEX*23

  IF (KP .EQ. 0) RETURN
  CALL DRCIRC(XZ(KP),YZ(KP),BL(KP))

  TEX = 'NODE NR    :           '
  WRITE(TEX (14:),'(I10)') KP
  CALL KTEXT(TEX,IWS-22,4,15)

  TEX = 'X COORD    :           '
  WRITE(TEX (14:),'(E10.3)') Xz(KP)
  CALL KTEXT(TEX,IWS-22,13,15)

  TEX = 'Y COORD    :           '
  WRITE(TEX (14:),'(E10.3)') Yz(KP)
  CALL KTEXT(TEX,IWS-22,14,15)

  TEX = 'Z COORD    :           '
  WRITE(TEX (14:),'(E10.3)') bl(KP)
  CALL KTEXT(TEX,IWS-22,15,15)

  TEX = 'Z COORD    :           '
  WRITE(TEX (14:),'(e10.4)') znod(kp)
  CALL KTEXT(TEX,IWS-22,16,15)

  TEX = 'link       :           '
  DO N = 1,Nd(kp)%lnx
     L = ND(KP)%LN(N)
     WRITE(TEX ( 6:11),'(I6 )') N
     WRITE(TEX (14:23),'(I10)') L
     CALL KTEXT(TEX,IWS-22,16+N,15)
  ENDDO
  end SUBROUTINE DISPFLOWNODEVALS

  SUBROUTINE DISPNODEVALS(KP)
  use m_netw
  USE M_DEVICES
  implicit none
  integer :: KP

  double precision :: fff
  double precision :: fxx
  double precision :: fyy
  double precision :: fzz
  integer :: l
  integer :: n
  CHARACTER TEX*23
  IF (KP .EQ. 0) RETURN
  CALL DRCIRC(XK(KP),YK(KP),ZK(KP))

  TEX = 'NODE NR    :           '
  WRITE(TEX (14:),'(I10)') KP
  CALL KTEXT(TEX,IWS-22,4,15)

  TEX = 'X COORD    :           '
  WRITE(TEX (14:),'(E10.3)') XK(KP)
  CALL KTEXT(TEX,IWS-22,13,15)

  TEX = 'Y COORD    :           '
  WRITE(TEX (14:),'(E10.3)') YK(KP)
  CALL KTEXT(TEX,IWS-22,14,15)

  TEX = 'Z COORD    :           '
  WRITE(TEX (14:),'(E10.3)') ZK(KP)
  CALL KTEXT(TEX,IWS-22,15,15)

  TEX = 'ELEM       :           '
  DO N = 1,NMK(KP)
     L = NOD(KP)%LIN(N)
     WRITE(TEX ( 6:11),'(I6 )') N
     WRITE(TEX (14:23),'(I10)') L
     CALL KTEXT(TEX,IWS-22,15+N,15)
  ENDDO

  if (netflow .eq. 2) return


  TEX = 'NR OF ELEMS:           '
  WRITE(TEX (14:),'(I10)') NMK(KP)
  CALL KTEXT(TEX,IWS-22,6,15)


  RETURN
  END SUBROUTINE DISPNODEVALS




  SUBROUTINE NETLINKVALS(MET,NCOL)
  
  use m_flowgeom, ONLY : XZ, YZ, lne2ln
  use m_missing
  use network_data
  use m_alloc
  use m_flow, only: cftrt
  use geometry_module, only: dbdistance, dcosphi
  use m_sferic, only: jsferic, jasfer3D
  use gridoperations

  implicit none

  integer :: MET, NCOL
  integer :: jacftrt
  double precision :: ag
  double precision :: cfl
  double precision :: dv
  double precision :: e0
  double precision :: eal
  double precision :: eps
  double precision :: fsp
  integer :: jaauto
  integer :: k1, k2, L, jaxz, kL, kR
  integer :: ncols
  integer :: nie
  integer :: nis
  integer :: nv
  double precision :: pi
  double precision :: rd
  double precision :: rek
  double precision :: rho
  double precision :: rhow
  double precision :: sp
  double precision :: uu
  double precision :: v
  double precision :: val
  double precision :: vmax
  double precision :: vmin
  double precision :: VV, WW, X3, Y3, X4, Y4
  double precision :: xd, YD, ZD
  double precision :: areaL, areaR, xc, yc, aa

  double precision, external :: topo_info

  COMMON /CONSTANTS/ E0, RHO, RHOW, CFL, EPS, AG, PI

  COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
  IF (MET .EQ. 1) RETURN

  jaxz = 0
  if (allocated (xz)) then
     jaxz = 1
  end if

  jacftrt = 0
  if (allocated (cftrt)) then
     jacftrt = 1
  end if 
  
! refresh netcell administartion, based on module variable netstat
  if ( netstat /= NETSTAT_OK .and. (met == 4 .or. met == 5 .or. (met >= 7 .and. met <= 9) .or. met >= 12)) then
     call findcells(100)
     call find1dcells()
     netstat = NETSTAT_OK
  endif

  IF (MET .EQ. 15 .or. MET.EQ.16) THEN  ! topology information
     call makenetnodescoding()
  END IF

  if ( numL.gt.size(rlin) ) then
     call realloc(rlin,numL)
  end if

  DO L  = 1,NUML
     V  = dmiss
     K1 = KN(1,L)
     K2 = KN(2,L)
     IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
        IF (MET .EQ. 2) THEN
           V = L
        ELSE IF (MET .EQ. 3) THEN
           CALL DHITEXT(K1,XK(K1),YK(K1),ZK(K1))
           CALL DHITEXT(K2,XK(K2),YK(K2),ZK(K2))
        ELSE IF (MET .EQ. 4) THEN
           IF (NUMP > 0 .and. jaxz == 1 .and. L <= size(LNN)) THEN
              IF (LNN(L) == 2) THEN
                 X3 = XZ(iabs(LNE(1,L)))
                 X4 = XZ(iabs(LNE(2,L)))
                 Y3 = YZ(iabs(LNE(1,L)))
                 Y4 = YZ(iabs(LNE(2,L)))
                 V  = DCOSPHI(XK(K1), YK(K1), XK(K2), YK(K2), X3, Y3, X4, Y4, jsferic, jasfer3D, dxymis)
                 if (v /= dmiss) then
                    v = abs(v)
                 end if
              ENDIF
           ENDIF
        ELSE IF (MET .EQ. 5) THEN
           if ( size(lne2ln) .ge. numl) then
              V = lne2ln(L)
           else
              v = 0
           endif
        ELSE IF (MET .EQ. 6) THEN
           V = LC(L)
        ELSE IF (MET .EQ. 7) THEN
           if (lc(L) > 0) V = netbr(LC(L))%nx   
        ELSE IF (MET .GE. 7 .AND. MET .LE. 9) THEN
           XD  = XK(K2) - XK(K1)
           YD  = YK(K2) - YK(K1)
           ZD  = ZK(K2) - ZK(K1)
           RD  = SQRT(XD*XD + YD*YD + ZD*ZD)
           REK = 0 ! ( RD - RL(L) ) / RL(L)
           IF (LC(L) .EQ. 0) REK = MAX(0d0,REK)
           SP  = E0*REK
           FSP = 0 ! SP*EA(L)/1e3  ! spanning in kN
           IF (MET .EQ. 9) THEN
              V = 0d0 ! TODO: AvD: Mag weg
           ENDIF
        ELSE IF (MET .EQ. 10) THEN
           V = DBDISTANCE( XK(K1), YK(K1), XK(K2), YK(K2), jsferic, jasfer3D, dmiss)
        ELSE IF (MET .EQ. 11) THEN
           V = KN(3,L)
        ELSE IF (MET .EQ. 12) THEN
           V = 0
           IF ( L <= SIZE(LNN) ) V = LNN(L)
        ELSE IF (MET .EQ. 13) THEN
           V = 0
           IF (L <= SIZE(LNN)) V = LNE(1,L)
        ELSE IF (MET .EQ. 14) THEN
           V = 0
           IF (L <= SIZE(LNN)) V = LNE(2,L)
        ELSE IF (MET .EQ. 15) THEN  ! topology information
           V = topo_info(L)
        ELSE IF (MET .EQ. 16) THEN  ! area ratio
           if ( lnn(L).lt.2 ) then
              V = dmiss
           else
              kL = lne(1,L)
              kR = lne(2,L)
              call getcellsurface(kL,areaL,xc,yc)
              call getcellsurface(kR,areaR,xc,yc)
              if ( areaL.lt.1d-12 .or. areaR.lt.1d-12 ) cycle
              V = areaR/areaL
              if ( V.lt.1d0 ) V = 1d0/V
           end if
        ELSE IF (MET .EQ. 17) THEN  ! link size criterion
           if ( lnn(L).lt.2 ) then
              V = dmiss
           else
              kL = lne(1,L)
              kR = lne(2,L)
              call getcellsurface(kL,areaL,xc,yc)
              call getcellsurface(kR,areaR,xc,yc)
              if ( areaL.lt.1d-12 .or. areaR.lt.1d-12 ) cycle
              k1 = kn(1,L)
              k2 = kn(2,L)
              aa = dbdistance(xk(k1), yk(k1), xk(k2), yk(k2), jsferic, jasfer3D, dmiss) * dbdistance(xz(kL), yz(kL), xz(kR), yz(kR), jsferic, jasfer3D, dmiss)
              V  = aa  / (areaR+areaL)
           end if
        ELSE IF (MET .EQ. 18) THEN 
            if (jacftrt .eq. 1) then 
                V = cftrt(L,2)
            else
                V = 0
            end if
        ENDIF
        RLIN(L) = V
     ENDIF
  ENDDO
  RETURN
  END SUBROUTINE NETLINKVALS

  SUBROUTINE NETNODEVALS(MET)
  USE M_FLOW
  USE M_FLOWGEOM
  use m_netw
  use m_sediment
  USE M_MISSING
  use m_ship

  implicit none
  integer :: MET

  integer :: k, L, j, K1, K2, K3,K4
  double precision :: x, y, z, uar
  double precision :: xn, yn, dis, rL  ! for smallest distance to land boundary (method=7)


  IF (MET .EQ. 1) RETURN

  IF (MET == 9) THEN
     RNOD = 0D0
     ! u1  =  yu*csu
     DO L = 1,LNXi
        K3  = LNCN(1,L)
        K4 = LNCN(2,L)
       ! UAR = U1(L)
        K1  = LN(1,L)
        K2 = LN(2,L)
        UAR = CSU(L)*( ACL(L)*UCX(K1) + (1D0-ACL(L))*UCX(K2) )    +    &
              SNU(L)*( ACL(L)*UCY(K1) + (1D0-ACL(L))*UCY(K2) )
        UAR = UAR*DX(L)
        RNOD(K3) = RNOD(K3) - UAR
        RNOD(K4) = RNOD(K4) + UAR
     ENDDO
     DO L = LNXi+1,lnx
        K3  = LNCN(1,L)
        K4 = LNCN(2,L)
        UAR = DX(L)*(1d0-acl(L))*U1(L)
        RNOD(K3) = RNOD(K3) - UAR
        RNOD(K4) = RNOD(K4) + UAR
     ENDDO

     do k = 1, mxwalls
        k3  = walls(2,k)
        k4  = walls(3,k)
        if (irov == 0) then
           RNOD(K3) = 0d0
           RNOD(K4) = 0d0
        else
           if (irov == 1) then
              uar = walls(16,k)
           else if (irov == 2) then
              uar = 0.d0 ! walls(16,k) ! *(1d0/walls(6,k) - 1d0/vonkar)
           endif
           UAR = 0.5d0*UAR*WALLS(9,K)
           RNOD(K3) = RNOD(K3) + UAR
           RNOD(K4) = RNOD(K4) + UAR
        endif
     enddo

     DO K = 1,NUMK
        IF (BAN(K) > 0D0) THEN
           RNOD(K) = RNOD(K) / BAN(K)
        ENDIF
     ENDDO

  ELSE
     DO K  = 1,NUMK
       X = XK(K)
       Y = YK(K)
       Z = ZK(K)

       IF (MET .EQ. 2) THEN
          RNOD(K) = K
       ELSE IF (MET .EQ. 3) THEN
          RNOD(K) = NMK(K)
       ELSE IF (MET .EQ. 5) THEN
          if (allocated(NB)) then
              if (size(NB) /= NUMK) then
                  exit
              else
                  RNOD(K) = NB(K)
              end if
          else
              RNOD(K) = 0
          end if
       ELSE IF (MET .EQ. 6) THEN
          RNOD(K) = ZK(K)
       ELSE IF (MET .EQ. 7) THEN
          call toland(x,y,1,MXLAN,1,xn,yn,dis,j,rL)
          rnod(k) = dis
       ELSE IF (MET .EQ. 8 .and. jased > 0 .and. jaceneqtr > 1) THEN
          RNOD(K) = grainlay(jgrtek,k)  ! erodable layer
       ELSE IF (MET .EQ. 10) THEN
          RNOD(k) = BAN(K)
       ELSE IF (MET .EQ. 11) THEN
          RNOD(k) = zspc(k) 
       ENDIF
    ENDDO
  ENDIF
  RETURN
  END SUBROUTINE NETNODEVALS

  SUBROUTINE MINMXNETLINS()
  
  use m_netw
  use m_missing
  use gridoperations
  
  implicit none
  double precision :: dv
  integer :: i
  integer :: jaauto
  integer :: k1
  integer :: k2
  integer :: l
  integer :: ncols
  integer :: nie
  integer :: nis
  integer :: nv
  double precision :: rd
  double precision :: rmax
  double precision :: rmin
  double precision :: val
  double precision :: vmax
  double precision :: vmin
  double precision :: xp1
  double precision :: xp2
  double precision :: yp1
  double precision :: yp2
  double precision :: zp1
  double precision :: zp2
  ! BEPAAL MINIMUM EN MAXIMUM VAN WAARDES BINNEN VIEWING AREA
  COMMON /DEPMAX2/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

  IF (JAAUTO .EQ. 1) THEN
     RMIN =  1.0D30
     linmin = 0
     RMAX = -1.0d30
     linmax = 0
     DO L = 1,NUML
        K1   = KN(1,L)
        K2   = KN(2,L)
        IF (RLIN(L) .NE. DMISS .AND. K1 .NE. 0 .AND. K2 .NE. 0) THEN
           XP1  = XK(K1)
           YP1  = YK(K1)
           ZP1  = ZK(K1)
           XP2  = XK(K2)
           YP2  = YK(K2)
           ZP2  = ZK(K2)
           IF (DINVIEW(XK(K1),YK(K1),ZK(K1)) .OR. DINVIEW(XK(K2),YK(K2),ZK(K2)) ) THEN
               RD = RLIN(L)
               IF (RD < RMIN) THEN
                   RMIN = RD
                   LINMIN = L
               ENDIF
               IF (RD > RMAX) THEN
                   RMAX = RD
                   LINMAX = L
               ENDIF
           ENDIF
        ENDIF
     ENDDO

     VMAX = RMAX
     VMIN = RMIN
  ENDIF

  DV   = VMAX - VMIN
  DO I = 1,NV
     VAL(I) = VMIN + (I-1)*DV/(NV-1)
  ENDDO

  RETURN
  END SUBROUTINE MINMXNETLINS

  SUBROUTINE MINMXNETNODS()
  use m_netw
  use m_missing
  use gridoperations
  
  implicit none

  integer          :: i, k
  double precision :: rd, rmax, rmin

  double precision :: VMAX, VMIN, DV, VAL
  integer          :: NCOLS,NV,NIS,NIE,JAAUTO
  COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

  ! BEPAAL MINIMUM EN MAXIMUM VAN DIEPTES BINNEN VIEWING AREA

  IF (JAAUTO .EQ. 1) THEN
     RMIN =  1.0D30
     NODMIN = 0
     RMAX = -1.0D30
     NODMAX = 0
     DO K = 1,NUMK
        IF ( DINVIEW(XK(K),YK(K),ZK(K)) ) THEN
           RD = RNOD(K)
           IF (rd .ne. dmiss) then
              IF (RD < RMIN ) THEN
                 RMIN = RD
                 NODMIN = K
              ENDIF
              IF (RD > RMAX) THEN
                 RMAX = RD
                 NODMAX = K
              ENDIF
           ENDIF
        ENDIF
     ENDDO
     VMAX = RMAX
     VMIN = RMIN
  ENDIF

  DV   = VMAX - VMIN
  DO I = 1,NV
     VAL(I) = VMIN + (I-1)*DV/(NV-1)
  ENDDO

  RETURN
  END SUBROUTINE MINMXNETNODS

SUBROUTINE MINMXNETCELLS()
  
  use m_netw
  use m_flowgeom
  use m_missing
  use gridoperations 

  implicit none

  double precision :: dv
  integer :: i
  integer :: jaauto
  integer :: k
  integer :: ncols
  integer :: nie
  integer :: nis
  integer :: nv
  double precision :: rd
  double precision :: rmax
  double precision :: rmin
  double precision :: val
  double precision :: vmax
  double precision :: vmin

  double precision, external :: znetcell

  ! BEPAAL MINIMUM EN MAXIMUM VAN DIEPTES BINNEN VIEWING AREA
  COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

  IF (JAAUTO .EQ. 1) THEN
     RMIN =  1.0D30
     NODMIN = 0
     RMAX = -1.0D30
     NODMAX = 0
     DO K = 1,max(NUMP,nump1d2d)
        IF ( DINVIEW(XZ(K),YZ(K),YZ(K)) ) THEN
           RD = RLIN(K)
           IF (rd .ne. dmiss) then
              IF (RD < RMIN ) THEN
                 RMIN = RD
                 netcelMIN = K
              ENDIF
              IF (RD > RMAX) THEN
                 RMAX = RD
                 netcelMAX = K
              ENDIF
           ENDIF
        ENDIF
     ENDDO
     VMAX = RMAX
     VMIN = RMIN
  ENDIF

  DV   = VMAX - VMIN
  DO I = 1,NV
     VAL(I) = VMIN + (I-1)*DV/(NV-1)
  ENDDO

  RETURN
  END SUBROUTINE MINMXNETCELLS


  SUBROUTINE TEKNODEVALS(MET)
  USE m_missing
  use m_netw
  use geometry_module, only: getdxdy, getdx, getdy 
  use m_sferic, only: jsferic
  use unstruc_colors ! , ONLY :NCOLWARN1, ncolhl
  use gridoperations
  
  implicit none
  integer :: MET

  double precision :: d
  integer :: jav
  integer :: jview
  integer :: k1, k
  integer :: k2
  integer :: key
  integer :: l
  integer :: n
  integer :: ncol
  double precision :: rd
  double precision :: vv
  double precision :: xyz

  DOUBLE PRECISION XD,YD,ZD,DX,DY,DZ,XX1,YY1,ZZ1,XX2,YY2,ZZ2,X3,Y3,Z3,H
  double precision :: X(4), Y(4), Z(4)
  double precision :: getrcir
  LOGICAL INVNOD

  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4

  integer :: NCOLS,NV,NIS,NIE,JAAUTO
  double precision :: VMAX,VMIN,DV,VAL
  COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

  KMOD = MAX(1,NUMK/100)

  H = 0.5d0

  if (met == 3) then ! smooth iso of netnode stuff based upon netcells

     if ( numk + numl .ne. lasttopology ) THEN  ! coarsening info
         if ( ubound(lnn,1).ne.numL ) then
             call findcells(100)
         endif
     endif

     do k = 1,nump
        IF (MOD(K,KMOD) .EQ. 0) THEN
           CALL HALT2(KEY)
           IF (KEY .EQ. 1) then
              RETURN
           end if
        ENDIF
        IF (inview(xzw(k), yzw(k) )  ) then
           call isosmoothnet(k)
        endif
     enddo

  else IF (MET .Gt. 3) THEN
    D = 0.5D0*GETRCIR()  !
    DO K1 = 1,NUMK
       IF (MOD(K1,KMOD) .EQ. 0) THEN
         CALL HALT2(KEY)
         IF (KEY .EQ. 1) then
            RETURN
         end if
       ENDIF
       IF (.NOT. INVNOD(K1)) CYCLE
       VV = RNOD(K1)
       XX1 = XK(K1)
       YY1 = YK(K1)
       ZZ1 = ZK(K1)
       IF (VV .NE. dmiss) THEN
         CALL ISOCOL(VV,NCOL)
         IF (MET .EQ. 3 .OR. MET .EQ. 4 .OR. &
             MET .EQ. 6 .OR. MET .EQ. 7 ) THEN
            DO N  = 1,NMK(K1)
               L  = NOD(K1)%LIN(N)
               CALL OTHERNODE(K1,L,K2)
               IF (K2 == 0) then
                  CYCLE
               end if

               XX2 = H*(XK(K2)+XX1)
               YY2 = H*(YK(K2)+YY1)
               ZZ2 = H*(ZK(K2)+ZZ1)
               IF (MET .EQ. 6) THEN
                  CALL DMOVABS(XX1,YY1,ZZ1)
                  CALL DLNABS(XX2,YY2,ZZ2)
               ELSE IF (MET .EQ. 4 .OR. MET .EQ. 7) THEN
                  ! XD = getdx (XX1, yy1, xx2, yy2)
                  ! YD = getdy (XX1, yy1, xx2, yy2)
                  call getdxdy(XX1, yy1, xx2, yy2, xd, yd, jsferic)
                  RD = SQRT(XD*XD + YD*YD)
                  IF (RD .NE. 0) THEN
                     IF (JVIEW .EQ. 1 .OR. JVIEW .EQ. 4) THEN
                       DX = -D*YD/RD
                       DY = D*XD/RD
                       DZ = 0
                     ELSE IF (JVIEW .EQ. 2) THEN
                       DZ = -D*YD/RD
                       DY = D*ZD/RD
                       DX = 0
                     ELSE IF (JVIEW .EQ. 3) THEN
                       DX = -D*ZD/RD
                       DZ = D*XD/RD
                       DY = 0
                     ENDIF
                     CALL DRIETWEE(XX2+DX,YY2+DY,ZZ2+DZ,X(1),Y(1),Z(1))
                     CALL DRIETWEE(XX1+DX,YY1+DY,ZZ1+DZ,X(2),Y(2),Z(2))
                     CALL DRIETWEE(XX1-DX,YY1-DY,ZZ1-DZ,X(3),Y(3),Z(3))
                     CALL DRIETWEE(XX2-DX,YY2-DY,ZZ2-DZ,X(4),Y(4),Z(4))
                     CALL pfiller(X,Y,4,ncol,ncol)
                     !CALL IGRJOIN(real(x(1)),real(y(1)),real(x(2)),real(y(2)))
                  ENDIF
               ENDIF
            ENDDO
         ELSE IF (MET .EQ. 5 .OR. MET .EQ. 8) THEN
            CALL DRCIRC(XX1,YY1,ZZ1)
         ELSE IF (MET == 9) THEN
            IF (VV .NE. dmiss .and. VV < vmin + 0.05d0*(vmax-vmin)) THEN
                CALL CIRR(Xx1,Yy1,ncolhl)
            endif
         ELSE IF (MET == 10) THEN
            IF (VV .NE. dmiss .and. VV > vmax - 0.05d0*(vmax-vmin)) THEN
                CALL CIRR(Xx1,Yy1,ncolhl)
            endif
         ENDIF
       ELSE
          CALL CIRR(XX1,YY1,NCOLWARN1)
       ENDIF
    ENDDO
  ENDIF

  RETURN
  END SUBROUTINE TEKNODEVALS

 SUBROUTINE TEKNODENUMS(MET,NCOL)
  USE M_MISSING
  use m_netw
  implicit none
   integer :: MET, NCOL

  integer :: k
  integer :: k1
  integer :: k2
  integer :: key
  integer :: l
  integer :: n
  integer :: ndraw

  COMMON /DRAWTHIS/  ndraw(50)

  LOGICAL INVNOD
  DOUBLE PRECISION X, Y, Z
  CALL SETCOL(NCOL)
  KMOD = MAX(1,NUMK/100)
  DO K  = 1,NUMK
     IF (.NOT. INVNOD(K)) CYCLE
     X = XK(K)
     Y = YK(K)
     Z = ZK(K)

     IF (MOD(K,KMOD) .EQ. 0) THEN
         CALL HALT2(KEY)
         IF (KEY .EQ. 1) then
            RETURN
         end if
     ENDIF

     IF (RNOD(K) .NE. dmiss) THEN
        IF (MET .EQ. 2 .OR. MET .GE. 6) THEN
           IF (NDRAW(8) .EQ. 2 .OR. NDRAW(8) .EQ. 3 .OR. NDRAW(8) .EQ. 5 ) THEN
              CALL DHITEXT(INT(RNOD(K)),X,Y,Z)
           ELSE IF (MET .EQ. 4) THEN
              DO N  = 1,NMK(K)
                 L  = NOD(K)%LIN(N)
                 K1 = KN(1,L)
                 K2 = KN(2,L)
                 X  = 0.5d0*(XK(K1) + 0.5d0*XK(K2))
                 Y  = 0.5d0*(YK(K1) + 0.5d0*YK(K2))
                 Z  = 0.5d0*(ZK(K1) + 0.5d0*ZK(K2))
                 CALL DHITEXT(L,X,Y,Z)
              ENDDO
           ELSE
              CALL dHTEXT(dble(RNOD(K)),X,Y,Z)
           ENDIF
        ENDIF
     ENDIF
  ENDDO

  RETURN
  END SUBROUTINE TEKNODENUMS

  SUBROUTINE TEKLINKVALS(MET)
  USE m_missing
  use m_netw
  use unstruc_colors, only: ncolhl
  use geometry_module, only: getdx, getdy, getdxdy
  use m_sferic, only: jsferic
  use gridoperations
  
  implicit none
  integer :: MET

  double precision :: d
  integer :: jav
  integer :: jview
  integer :: k1
  integer :: k2
  integer :: l
  integer :: ncol, key
  double precision :: rd
  double precision :: vv
  double precision :: xyz

  DOUBLE PRECISION XD,YD,ZD,DX,DY,DZ,XX1,YY1,ZZ1,XX2,YY2,ZZ2,X3,Y3,Z3
  double precision :: X(4), Y(4), Z(4), GETRCIR
  logical :: invnod
  

  integer :: NCOLS,NV,NIS,NIE,JAAUTO
  double precision :: VMAX,VMIN,DV,VAL
  COMMON /DEPMAX2/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO



  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4

  D = 0.5D0*GETRCIR()  !
  IF (MET .GE. 3) THEN
     LMOD = MAX(1,NUML/100)
     DO L  = 1,NUML
        IF (MOD(L,LMOD) .EQ. 0) THEN
            CALL HALT2(KEY)
            IF (KEY .EQ. 1) then
               RETURN
            end if
        ENDIF
        VV = RLIN(L)
        IF (VV .NE. dmiss) THEN
           K1 = KN(1,L)
           K2 = KN(2,L)
           IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
              IF (.NOT. INVNOD(K1) .and. .NOT. INVNOD(K2) )  CYCLE
              XX1 = XK(K1)
              YY1 = YK(K1)
              ZZ1 = ZK(K1)
              XX2 = XK(K2)
              YY2 = YK(K2)
              ZZ2 = ZK(K2)
              CALL ISOCOL2(VV,NCOL)
              IF (MET .EQ. 3 .OR. MET .EQ. 6) THEN
                 CALL DMOVABS(XX1,YY1,ZZ1)
                 CALL DLNABS(XX2,YY2,ZZ2)
              ELSE IF (MET .EQ. 4 .OR. MET .EQ. 7) THEN
                 !XD = getdx (XX1, yy1, xx2, yy2)
                 !YD = getdy (XX1, yy1, xx2, yy2)
                 call getdxdy(XX1, yy1, xx2, yy2, xd, yd, jsferic)
                 RD = sqrt(xd*xd + yd*yd)
                 IF (RD .NE. 0) THEN
                    IF (JVIEW .EQ. 1 .OR. JVIEW .EQ. 4) THEN
                       DX = -D*YD/RD
                       DY = D*XD/RD
                       DZ = 0
                    ELSE IF (JVIEW .EQ. 2) THEN
                       DZ = -D*YD/RD
                       DY = D*ZD/RD
                       DX = 0
                    ELSE IF (JVIEW .EQ. 3) THEN
                       DX = -D*ZD/RD
                       DZ = D*XD/RD
                       DY = 0
                    ENDIF
                    CALL DRIETWEE(XX2+DX,YY2+DY,ZZ2+DZ,X(1),Y(1),Z(1))
                    CALL DRIETWEE(XX1+DX,YY1+DY,ZZ1+DZ,X(2),Y(2),Z(2))
                    CALL DRIETWEE(XX1-DX,YY1-DY,ZZ1-DZ,X(3),Y(3),Z(3))
                    CALL DRIETWEE(XX2-DX,YY2-DY,ZZ2-DZ,X(4),Y(4),Z(4))
                    CALL pfiller(X,Y,4,ncol,ncol)
!                    CALL IGRJOIN(real(x(1)),real(y(1)),real(x(2)),real(y(2)))
                    call movabs(x(1),y(1))
                    call lnabs(x(2),y(2))
                 ENDIF
              ELSE IF (MET .EQ. 5 .OR. MET .EQ. 8) THEN
                 X3 = 0.5d0*(XX1+XX2)
                 Y3 = 0.5d0*(YY1+YY2)
                 Z3 = 0.5d0*(ZZ1+ZZ2)
                 CALL DRCIRC(X3,Y3,Z3)
              ELSE IF (MET == 9) THEN
                 IF (VV .NE. dmiss .and. VV < vmin + 0.05d0*(vmax-vmin)) THEN
                     X3 = 0.5d0*(XX1+XX2)
                     Y3 = 0.5d0*(YY1+YY2)
                     Z3 = 0.5d0*(ZZ1+ZZ2)
                     CALL CIRR(X3,Y3,ncolhl)
                 endif
              ELSE IF (MET == 10) THEN
                 IF (VV .NE. dmiss .and. VV > vmax - 0.05d0*(vmax-vmin)) THEN
                     X3 = 0.5d0*(XX1+XX2)
                     Y3 = 0.5d0*(YY1+YY2)
                     Z3 = 0.5d0*(ZZ1+ZZ2)
                     CALL CIRR(X3,Y3,ncolhl)
                 endif
              ENDIF
           ENDIF
        ENDIF
     ENDDO

  ENDIF

  RETURN
  END SUBROUTINE TEKLINKVALS

  SUBROUTINE TEKLINKNUMS(MET,NCOL)
  USE M_MISSING
  use m_netw
  implicit none
  integer :: MET, NCOL

  integer :: k1
  integer :: k2
  integer :: key
  integer :: l
  integer :: ndraw
  double precision :: vv
  logical :: invnod

  COMMON /DRAWTHIS/  ndraw(50)

  DOUBLE PRECISION XP,YP,ZP
  CALL SETCOL(NCOL)
  IF (MET .EQ. 2 .OR. MET .GE. 6 .and. MET .LE. 8) THEN
     LMOD = MAX(1,NUML/100)
     DO L  = 1,NUML
        IF (MOD(L,LMOD) .EQ. 0) THEN
            CALL HALT2(KEY)
            IF (KEY .EQ. 1) then
               RETURN
            end if
        ENDIF
        VV = RLIN(L)
        IF (VV .NE. dmiss) THEN
           K1 = KN(1,L)
           K2 = KN(2,L)
           IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
              IF (.NOT. INVNOD(K1) .and. .NOT. INVNOD(K2)) CYCLE
              XP = 0.5d0*(XK(K1) + XK(K2))
              YP = 0.5d0*(YK(K1) + YK(K2))
              ZP = 0.5d0*(ZK(K1) + ZK(K2))
              IF (NDRAW(7) .EQ. 2 .OR. NDRAW(7) .EQ. 3 .OR. (NDRAW(7) >= 10 .and. ndraw(7).ne.16 .and. ndraw(7).ne.17 .and. ndraw(7).ne.18)) THEN
                 CALL DHITEXT(INT(VV),XP,YP,ZP)
              ELSE
                 CALL DHTEXT(VV,XP,YP,ZP)
              ENDIF
           ENDIF
        ENDIF
     ENDDO
  ENDIF
  RETURN
  END SUBROUTINE TEKLINKNUMS


  SUBROUTINE SETLINKCOLOUR(L,NCOL)
  use m_netw
  use unstruc_colors
  implicit none
  integer :: L, NCOL, NCL
  IF (NCOL == 0) THEN      ! ERASE
     NCL = 0
  ELSE IF (NCOL == 1) THEN ! 1 MEANS: DRAW IN KN3 PREDEFINED COLOUR
     if (KN(3,L) == 0) then
        NCL = 31
     else IF (KN(3,L) == 1) THEN ! 1D
        NCL = NCOLRG
     else IF (KN(3,L) == 2) THEN ! 2D
        NCL = NCOLDN
     else IF (KN(3,L) == 3) THEN ! 1d2d internal 
        NCL = NCOLNN 
     else IF (KN(3,L) == 4) THEN ! 1d2d longitudinal
        NCL = NCOLRN 
     else IF (KN(3,L) == 5) THEN ! 1d2d internal pipe streetinlet
        NCL = NCOLSP 
     else IF (KN(3,L) == 6) THEN ! 1d mainbranch
        NCL = KLSAM 
     else IF (KN(3,L) == 7) THEN ! 1d2d internal pipe roofgutter
        NCL = NCOLSP + 5
     ENDIF
  ELSE
     NCL = NCOL
  ENDIF
  CALL SETCOL(NCL)
  RETURN
  END

  SUBROUTINE TEKLINK(L,NCOL)
  use m_netw
  use unstruc_colors
  implicit none
  integer :: L, NCOL
  integer :: jaSmallCir
  double precision :: dlength

  integer :: k1
  integer :: k2

  CALL SETLINKCOLOUR(L,NCOL)

  K1 = KN(1,L)
  K2 = KN(2,L)
  IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
     CALL MOVABS( XK(K1),YK(K1) )
     CALL  LNABS( XK(K2),YK(K2) )
     IF (NCOL > 0) THEN
         CALL SETCOL(NCOLNN)
         CALL PTABS(XK(K1),YK(K1) )
         CALL PTABS(XK(K2),YK(K2) )
     ENDIF
  ENDIF
  RETURN
  END SUBROUTINE TEKLINK

  SUBROUTINE TEKNODE(KP,NCOL)
  use m_netw
  use unstruc_colors
  implicit none
  integer :: KP, NCOL
  integer :: k1
  integer :: k2
  integer :: l
  integer :: n

  CALL SETCOL(NCOL)
  DO N  = 1,NMK(KP)
     L  = NOD(KP)%LIN(N)
     K1 = KN(1,L)
     K2 = KN(2,L)
     if (k1 > 0 .and. k2 > 0) then
        CALL DMOVABS( XK(K1),YK(K1),ZK(K1) )
        CALL  DLNABS( XK(K2),YK(K2),ZK(K2) )
     endif
  ENDDO

  IF (NCOL > 0) THEN
     CALL SETCOL(NCOLNN)
     DO N  = 1,NMK(KP)
        L  = NOD(KP)%LIN(N)
        K1 = KN(1,L)
        K2 = KN(2,L)
        if (k1 > 0) then
           CALL DPTABS( XK(K1),YK(K1),ZK(K1) )
        endif
        if (k2 > 0) then
           CALL DPTABS( XK(K2),YK(K2),ZK(K2) )
        endif
     ENDDO
  ENDIF

  IF (KC(KP) .EQ. -1) CALL DCIRR(XK(KP),YK(KP),ZK(KP),NCOL)
  RETURN
  END SUBROUTINE TEKNODE


!> Highlights net/flow nodes and/or links, when specified in display parameters.
subroutine highlight_nodesnlinks()
    use unstruc_display
    use unstruc_colors
    use network_data
    use m_flowgeom
    implicit none

    integer :: L

    ! if (jaHighlight /= 1) return

    if (nhlNetNode > 0 .and. nhlNetNode <= numk) then
        call cirr(xk(nhlNetNode), yk(nhlNetNode), ncolhl)
    end if

    if (nhlNetLink > 0 .and. nhlNetLink <= numl) then
        call cirr(.5d0*(xk(kn(1,nhlNetLink))+xk(kn(2,nhlNetLink))), &
                  .5d0*(yk(kn(1,nhlNetLink))+yk(kn(2,nhlNetLink))), ncolhl)
        call teklink(nhlNetLink, ncolhl)
    end if

    if (nhlFlowNode > 0 .and. nhlFlowNode <= ndx) then
       call cirr(xz(nhlFlowNode), yz(nhlFlowNode), ncolhl)
    end if

    if (nhlFlowLink > 0 .and. nhlFlowLink <= lnx) then
        call cirr(xu(nhlFlowLink), yu(nhlFlowLink), ncolhl)
    end if

end subroutine highlight_nodesnlinks


      SUBROUTINE D1ARROWS(X,Y,Z,U,V,W,PSI0,VFAC)
      use gridoperations
      implicit none
      double precision :: psi0
      double precision :: vfac
      double precision :: X,Y,Z,U,V,W
      DOUBLE PRECISION XD,YD,ZD,XP,YP,ZP, &
                       UD,VD,WD,UR,VR,WR
      XD = X
      YD = Y
      ZD = Z
      UD = U
      VD = V
      WD = W
      CALL DRIETWEE(XD,YD,ZD,XP,YP,ZP)
      CALL DRIETWEE(UD,VD,WD,UR,VR,WR)
      CALL ARROWS(XP,YP,UR,VR,PSI0,VFAC)
      RETURN
      END SUBROUTINE D1ARROWS


  SUBROUTINE VIEWCYCLE(KEY)
  implicit none
  double precision :: deltx
  double precision :: delty
  double precision :: deltz
  double precision :: dscr
  integer :: jav
  integer :: jview
  double precision :: wpqr
  double precision :: xyz
  double precision :: zfac
  double precision :: zupw
  integer :: KEY
  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
  COMMON /PERSPX/ WPQR,DELTX,DELTY,DELTZ,ZFAC,DSCR,ZUPW
  JVIEW = JVIEW + 1
  IF (JVIEW .GT. JAV) JVIEW = 1
  KEY   = 3
  RETURN
  END SUBROUTINE VIEWCYCLE


  SUBROUTINE VIEMATn(the,phi)
  implicit none
  double precision :: cp
  double precision :: ct
  double precision :: deltx
  double precision :: delty
  double precision :: deltz
  double precision :: dscr
  double precision :: r
  double precision :: sp
  double precision :: st
  double precision :: t1
  double precision :: t2
  double precision :: t3
  double precision :: t4
  double precision :: vs
  double precision :: wpqr
  double precision :: x0s
  double precision :: y0s
  double precision :: z
  double precision :: zfac
  double precision :: zupw
  double precision :: the, phi
!
! Maak viewing matrix Vs
! phi (0 -- pi) en the (-pi/2 -- pi/2) : kijkhoekjes
! wpqr                                 : oog-object in wereldcoor
! deltx,delty,deltz                    : kijk door dit punt in were
! zfac (negatief:op z'n kop)           : oprekking verticaal
! Dscr                                 : oog-scherm in wereldcoor
! Vs                                   : Viewing matrix
!
  common /viewmat/ vs(4,4), x0s, y0s
  COMMON /PERSPX/ WPQR,DELTX,DELTY,DELTZ,ZFAC,DSCR,ZUPW
  dimension T1(4,4),T2(4,4),T3(4,4),T4(4,4),R(4,4),Z(4,4)

  T1 = 0
  T2 = 0
  T3 = 0
  T4 = 0
  R = 0
  CT = COS(THE)
  ST = SIN(THE)
  CP = COS(PHI)
  SP = SIN(PHI)
!
  T1(1,1) = 1.
  T1(2,2) = 1.
  T1(3,3) = 1.   ! ZFAC
  T1(4,4) = 1.
  T1(1,4) = -deltx
  T1(2,4) = -delty
  T1(3,4) =  deltz  ! *ZFAC
!
  T2(1,1) = 1.
  T2(2,2) = 1.
  T2(3,3) = 1.
  T2(4,4) = 1.
  T2(1,4) = -wpqr*CT*CP
  T2(3,4) = -wpqr*CT*SP   ! WAS 2
  T2(2,4) = -wpqr*ST      ! WAS 3
!
  T3(1,1) =  CP
  T3(1,3) =  SP
  T3(3,1) = -SP
  T3(3,3) =  CP
  T3(2,2) =  1.
  T3(4,4) =  1.
!
  T4(1,1) =  CT
  T4(1,2) =  ST
  T4(2,1) = -ST
  T4(2,2) =  CT
  T4(3,3) =  1.
  T4(4,4) =  1.
!
  R(1,2)  =  DSCR
  R(2,3)  =  DSCR
  R(3,1)  = -1.
  R(4,4)  =  1.


! nadat alles geinitialiseerd is de viewing transformatie-matr Vs =

  call matm4(R,T4,Z)

  call matm4(Z,T3,Vs)
  call matm4(Vs,T2,Z)
  call matm4(Z,T1,Vs)
  end SUBROUTINE VIEMATn


  SUBROUTINE MATM4(a,b,c)
  implicit none
  integer :: i
  integer :: j
  integer :: k
  ! matrix matrix
  double precision, dimension(4,4) :: a,b,c
  do 801 i = 1,4
     do 801 k = 1,4
        c(i,k) = 0d0
        do 801 j = 1,4
           c(i,k) = a(i,j) * b(j,k) + c(i,k)
  801 continue
  end SUBROUTINE MATM4
  !
  !

  subroutine viemat(the,phi)
  implicit none
  double precision :: cp
  double precision :: ct
  double precision :: deltx
  double precision :: delty
  double precision :: deltz
  double precision :: dscr
  double precision :: r
  double precision :: sp
  double precision :: st
  double precision :: t1
  double precision :: t2
  double precision :: t3
  double precision :: t4
  double precision :: vs
  double precision :: wpqr
  double precision :: x0s
  double precision :: y0s
  double precision :: z
  double precision :: zfac
  double precision :: zupw
  double precision :: the, phi
  common /viewmat/ vs(4,4), x0s, y0s
  COMMON /PERSPX/ WPQR,DELTX,DELTY,DELTZ,ZFAC,DSCR,ZUPW

  ! Maak viewing matrix Vs
  ! phi (0 -- pi) en the (-pi/2 -- pi/2) : kijkhoekjes
  ! wpqr                                 : oog-object in wereldcoor
  ! deltx,delty,deltz                    : kijk door dit punt in were
  ! zfac (negatief:op z'n kop)           : oprekking verticaal
  ! Dscr                                 : oog-scherm in wereldcoor
  ! Vs                                   : Viewing matrix
  !
  dimension T1(4,4),T2(4,4),T3(4,4),T4(4,4),R(4,4),Z(4,4)
  T1 = 0
  T2 = 0
  T3 = 0
  T4 = 0
  R = 0
  CT = COS(THE)
  ST = SIN(THE)
  CP = COS(PHI)
  SP = SIN(PHI)

  T1(1,1) = 1.
  T1(2,2) = 1.
  T1(3,3) = zfac
  T1(4,4) = 1.
  T1(1,4) = -deltx
  T1(2,4) = -delty
  T1(3,4) = -deltz*ZFAC

  T2(1,1) = 1.
  T2(2,2) = 1.
  T2(3,3) = 1.
  T2(4,4) = 1.
  T2(1,4) = -wpqr*ct*cp
  T2(3,4) = -wpqr*ct*sp
  T2(2,4) = -wpqr*st

  T3(1,1) =  cp
  T3(1,3) =  sp
  T3(3,1) = -sp
  T3(3,3) =  cp
  T3(2,2) =  1.
  T3(4,4) =  1.

  T4(1,1) =  ct
  T4(1,2) =  st
  T4(2,1) = -st
  T4(2,2) =  ct
  T4(3,3) =  1.
  T4(4,4) =  1.

  R(1,3)  = Dscr
  R(2,2)  = Dscr
  R(3,1)  = -1.
  R(4,4)  =  1.

  !  nadat alles geinitialiseerd is de viewing transformatie-matr Vs =

  call matm4(R,T4,Z)
  call matm4(Z,T3,Vs)
  call matm4(Vs,T2,Z)
  call matm4(Z,T1,Vs)
  end subroutine viemat



   SUBROUTINE TYPEVALUE(RD,KEY)
   USE M_DEVICES
   implicit none
   double precision :: rdin
   double precision :: RD
   integer :: KEY
   integer :: infoinput

   RDIN = RD
   CALL INPOPUP('ON')
   CALL INHIGHLIGHT('WHITE','RED')
   CALL INDOUBLEXYDEF(IWS/2-10,IHS-3,'VALUE : ',1,RD,11,'(F11.4)')
   KEY = InfoInput(55)
   IF (KEY .EQ. 23) THEN
      RD = RDIN
   ENDIF
   CALL INPOPUP('OFF')
   RETURN
   END SUBROUTINE TYPEVALUE

      SUBROUTINE CHADEP(XP,YP,RD,KEY)
      USE M_MISSING
      implicit none
      double precision :: XP,YP,RD
      INTEGER :: KEY

      double precision :: f
      double precision :: fac
      integer :: jplus
      double precision :: rdol
      CHARACTER WRDKEY*40
      WRDKEY = 'CHANGE SCALAR VALUE'
      RDOL  = RD
      JPLUS = 0
      CALL DISPUT(21)
   10 CONTINUE
      CALL DISVAL1(RD)
      CALL KCIR(XP,YP,RD)
      CALL INKEYEVENT(KEY)

      IF (KEY .EQ. 171) THEN
         CALL HELP(WRDKEY,3)
      ELSE IF (KEY .EQ. 45 .OR. KEY .EQ. 160) THEN
         IF (RD .EQ. dmiss) RD  = 6.9d0
         IF (JPLUS .NE. -1) THEN
            FAC = 1d0
            F   = MAX(.001d0,.01d0*RD)
         ENDIF
         RD     = RD - F*FAC
         FAC    = FAC*1.01d0
         JPLUS  = -1
      ELSE IF (KEY .EQ. 43 .OR. KEY .EQ. 162) THEN
         IF (RD .EQ. dmiss) RD = 6.9d0
         IF (JPLUS .NE. 1) THEN
            FAC = 1d0
            F   = MAX(.001d0,.01d0*RD)
         ENDIF
         RD     = RD + F*FAC
         FAC    = FAC*1.01d0
         JPLUS  = 1
      ELSE IF (KEY .EQ. 32) THEN
         CALL TYPEVALUE(RD,KEY)
         CALL DISVAL1(RD)
         CALL KCIR(XP,YP,RD)
         RETURN
      ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32 .OR. KEY .EQ. 143) THEN
         RD     = dmiss
         CALL DISVAL1(RD)
         CALL KCIR(XP,YP,RD)
         RETURN
      ELSE IF (KEY .EQ. 27) THEN
         RD = RDOL
         CALL DISVAL1(RD)
         CALL KCIR(XP,YP,RD)
         RETURN
      ELSE IF (KEY .NE. 254 .AND. KEY .NE. 257) THEN
         RETURN
      ENDIF
      GOTO 10
      END SUBROUTINE CHADEP


      SUBROUTINE DISVAL1(DEP)
      use unstruc_colors
      implicit none
      double precision :: DEP
      CHARACTER TEX*8
      IF (ABS(DEP) .LT. 10) THEN
         WRITE(TEX(1:),'(F8.5)') DEP
      ELSE IF (ABS(DEP) .LT. 100) THEN
         WRITE(TEX(1:),'(F8.4)') DEP
      ELSE IF (ABS(DEP) .LT. 1000) THEN
         WRITE(TEX(1:),'(F8.3)') DEP
      ELSE IF (ABS(DEP) .LT. 10000) THEN
         WRITE(TEX(1:),'(F8.2)') DEP
      ELSE IF (ABS(DEP) .LT. 100000) THEN
         WRITE(TEX(1:),'(F8.1)') DEP
      ELSE
         WRITE(TEX(1:),'(E8.1)') DEP
      ENDIF
      CALL KTEXT(TEX,IWS-7,4,15)
      RETURN
      END SUBROUTINE DISVAL1




   SUBROUTINE TEKSAM( XS,YS,ZS,NS,MET)
      
      use unstruc_colors
      use m_missing, only: DMISS
      use unstruc_opengl, only: jaopengl
      use gridoperations
      
      implicit none
      double precision :: deltx, RC
      double precision :: delty
      double precision :: deltz
      double precision :: dscr
      double precision :: hrc
      integer :: i, KMOD
      integer :: jastart
      integer :: key
      integer :: mcs
      integer :: ncol
      integer :: ncs
      integer :: ndraw
      integer :: ns1
      double precision :: wpqr
      double precision :: x
      double precision :: xold
      double precision :: y
      double precision :: yold
      double precision :: z
      double precision :: zfac
      double precision :: zupw
      integer :: NS,MET
      double precision :: XS(NS), YS(NS), ZS(NS)
!     TEKEN SAMPLES
      COMMON /PERSPX/ WPQR,DELTX,DELTY,DELTZ,ZFAC,DSCR,ZUPW
      COMMON /DRAWTHIS/ ndraw(50)
      COMMON /SAMPLESADM/  MCS,NCS,NS1
      double precision :: VS(4,4)
      IF (NS .EQ. 0 .OR. MET .EQ. 0) RETURN
      IF (MET .EQ. 4 .OR. MET .EQ. 5) CALL SETTEXTSIZE()
      RC      = 1.7d0*RCIR
      HRC     = RCIR/2
      JASTART = 0
      XOLD    = XS(1)
      YOLD    = YS(1)
      KMOD    = MAX(1,NS/100)
      key     = 0

!     Fix for OpenGL rendering
      if ( jaopengl.eq.1 .and. MET.eq.1 ) then
         MET = 7
      end if

      if (met <= 0) then
          return
      end if

      if (met == 5) then
          CALL SETCOL(KLSAM)
      else
          call minmxsam()
      endif

      DO 20 I = 1,NS
         IF (MOD(I,KMOD) .EQ. 0) THEN
            CALL HALT2(KEY)
            IF (KEY .EQ. 1) RETURN

         ENDIF
         X = XS(I)
         Y = YS(I)
         Z = ZS(I)

         if ( Z.EQ.DMISS ) cycle ! SPvdP: structured sample data may comprise missing values

         IF (INVIEW (X,Y) ) THEN
            IF (NDRAW(9) .EQ. 2) THEN
!               CALL VIEW(XS(I),YS(I),ZS(I),X0S,Y0S,VS,X,Y,ZC)
            ENDIF
            IF (MET .ne. 5) THEN
               CALL ISOCOL2(Z,NCOL)
            ENDIF
            IF (MET .EQ. 1 .OR. MET .EQ. 2) THEN
               IF (NDRAW(9) .EQ. 1) THEN
!
!                  CALL MOVABS(X,Y)
!                  CALL CIR(RCIR)
!!                 CALL HTEXT(ZS(I),X,Y)

                  call box(x-0.5d0*rcir,y-0.5d0*rcir,x+0.5d0*rcir,y+0.5d0*rcir)

                  IF (MET .EQ. 2) THEN
                     CALL MOVABS(X,Y)
                     CALL IGRFILLPATTERN(0,0,0)
                     CALL SETCOL(1)
                     CALL CIR(RCIR)
                     CALL IGRFILLPATTERN(4,0,0)
                  ENDIF

               ELSE IF (NDRAW(9) .EQ. 2) THEN
                  IF (MET .EQ. 1) THEN
!                     CALL PERREC(XS(I),YS(I),ZS(I),RC,NCOL,NCOL)
                  ELSE
!                     CALL PERREC(XS(I),YS(I),ZS(I),RC,NCOL,0)
                  ENDIF
               ENDIF
            ELSE IF (MET .EQ. 3) THEN
               CALL PTABS(X,Y)
            ELSE IF (MET .EQ. 4 .OR. MET .EQ. 5) THEN
               CALL HTEXT(ZS(I),X,Y)
            ELSE IF (MET .EQ. 6) THEN
               CALL MOVABS(X,Y)
               CALL CIR(RCIR)
               CALL HTEXT(ZS(I),X+rcir,Y)
            ELSE IF (MET .EQ. 7) THEN
               CALL KREC5(X,Y,HRC,HRC)
            ENDIF
         ELSE
            JASTART = 0
         ENDIF
   20 CONTINUE
      CALL IGRFILLPATTERN(4,0,0)
      CALL IGRCHARDIRECTION('H')
      RETURN
      END SUBROUTINE TEKSAM


   SUBROUTINE JGRLINE8(X,Y,N) ! TEKEN LIJN, INCL XYMISSEN, GEBRUIK VAN INVIEW EN PROJECTIE
   
   use m_missing
   use gridoperations
   
   implicit none
   double precision   :: X(N), Y(N)
   integer            :: n

   integer            :: i
   integer            :: in
   integer            :: k
   integer            :: l
   double precision   :: xa
   double precision   :: ya
   integer, parameter :: KMAX=4096     ! BEPERKING VAN INTERACTER
   real               :: XX(KMAX), YY(KMAX)

   K  = 0
   L  = 0
   IN = 0
   I=0
   DO WHILE (I .LT. N)
      I = I + 1
      IF ( X(I) .NE. dXYMIS) THEN
         IF ( INVIEW( X(I) ,Y(I) ) ) IN = 1
         IF (K .EQ. 0 .OR. IN .EQ. 1 .OR. I .EQ. L+1) K  = K + 1
         IF (K .EQ. 1 .OR. IN .EQ. 1 .OR. I .EQ. L+1) THEN
            !XX(K) = XA
            ! YY(K) = YA
            XX(K) = X(i)
            YY(K) = Y(i)
         ENDIF
         IF (IN .EQ. 1) L = I
      ENDIF
      IF (I .EQ. N .OR. X(I) .EQ. dXYMIS .OR. K .EQ. KMAX) THEN
         IF (K .NE. 0) THEN
            CALL POLYLINE(XX,YY,K)
            IF (K .EQ. KMAX) I = I - 1
            K = 0
            L = 0
            IN = 0
         ENDIF
      ENDIF
   ENDDO
   RETURN
   END SUBROUTINE JGRLINE8


   LOGICAL FUNCTION INVIEW2(X,Y,XX,YY)
   USE M_MISSING
   use m_wearelt
   implicit none
   double precision :: x,y,xx,yy

   ! ZIT IK IN ZOOMGEBIED? NULLEN EN DEFAULTS NIET, IN WERELDCOORD

   INVIEW2 = .FALSE.
   IF (X .NE. XYMIS) THEN
      CALL dPROJECT(X,Y,XX,YY,1)
      IF (XX .GT. X1 .AND. XX .LT. X2 .AND.  &
          YY .GT. Y1 .AND. YY .LT. Y2     ) THEN
         INVIEW2 = .TRUE.
      ENDIF
   ELSE
      XX = XYMIS
      YY = XYMIS
   ENDIF
   RETURN
   END FUNCTION INVIEW2

      SUBROUTINE dPROJECT(X8,Y8,XX4,YY4,MODE)
      use m_sferic
      implicit none
      double precision :: x8, y8, xx4, yy4
      integer          :: mode

      COMMON /SFERZOOM/ X0,Y0,FAC,X1W,Y1W,X2W,Y2W  ! GRADEN


      double precision :: X0,Y0,FAC,X1W,Y1W,X2W,Y2W
      double precision :: X,Y,XX,YY,SX,CX,SY,CY,SY0,CY0,RR,C,SC,CC,RN
      double precision, save :: EPS = 1.D-20
      X = X8
      Y = Y8
      IF (JSFERTEK .EQ. 0) THEN        ! Just Transfer
         XX  = X
         YY  = Y
      ELSE IF (JSFERTEK .EQ. 1) THEN   ! Stereographic
         SY0 = SIN(DG2RD*Y0)
         CY0 = COS(DG2RD*Y0)
         IF (MODE .EQ. 1) THEN         ! LON,LAT to X,Y
            SX = SIN(DG2RD*(X-X0))
            CX = COS(DG2RD*(X-X0))
            SY = SIN(DG2RD*(Y))
            CY = COS(DG2RD*(Y))
            RN = 1.D0+SY0*SY+CY0*CY*CX
            IF (ABS(RN) .LT. EPS) THEN
               RN = SIGN(1.D0,RN)*EPS
            ENDIF
            RR = FAC*2.D0*RD2DG/RN     ! FAC om naar X1,Y1,X2,Y2 te schalen
            XX = RR*CY*SX              ! Stereographic to Degrees
            YY = RR*(CY0*SY-SY0*CY*CX)
         ELSE IF (MODE .EQ. 2) THEN    ! X,Y to LON,LAT
            XX = X / FAC
            YY = Y / FAC
            RR = SQRT(XX*XX + YY*YY)
            IF (RR .GT. EPS) THEN
               SX = SIN(DG2RD*(XX-X0))
               CX = COS(DG2RD*(XX-X0))
               SY = SIN(DG2RD*(YY))
               CY = COS(DG2RD*(YY))
               C  = 2.D0*ATAN2(RR,2.D0*RD2DG)
               SC = SIN(C)
               CC = COS(C)
               XX = X0*DG2RD + ATAN2(XX*SC,RR*CY0*CC-YY*SY0*SC)
               YY = ASIN(CC*SY0+YY*SC*CY0/RR)
               XX = XX*RD2DG
               YY = YY*RD2DG
            ELSE
               XX = X
               YY = Y
            ENDIF
         ENDIF
      ELSE IF (JSFERTEK .EQ. 2) THEN     ! MERCATOR
         IF (MODE .EQ. 1) THEN
            IF (Y .GE.  89D0) Y =  89.D0
            IF (Y .LE. -89D0) Y = -89.D0
            YY = DG2RD*Y
            YY = DLOG( 1D0 + SIN(YY) ) / COS(YY)
            XX = DG2RD*X
         ELSE IF (MODE .EQ. 2) THEN
            YY = DATAN( SINH( Y ) )
            YY = RD2DG*YY
            XX = RD2DG*X
         ENDIF
      ENDIF
      XX4 = XX
      YY4 = YY
      RETURN
      END SUBROUTINE dPROJECT

      SUBROUTINE TEKTRI(XL,YL,NCOL)
      implicit none
      integer :: ncol
      double precision :: XL(3),YL(3)
      CALL SETCOL(NCOL)
      CALL MOVABS (XL(1),YL(1))
      CALL LNABS (XL(2),YL(2))
      CALL LNABS (XL(3),YL(3))
      CALL LNABS (XL(1),YL(1))
      RETURN
      END
      
      subroutine tekpoly(n,x,y,ncol)
      implicit none
      integer,                        intent(in) :: N    !< polygon dimension
      double precision, dimension(n), intent(in) :: x,y  !< polygon coordinates
      integer,                        intent(in) :: ncol !< color number
      integer                                    :: i
      
      if ( N.lt.3 ) return
      
      call setcol(ncol)
      call movabs(x(N),y(N))
      
      do i = 1,N
         call lnabs(x(i),y(i))
      end do
      
      return
      end

!> spline2curvi parameter menu
subroutine change_spline2curvi_param(jacancelled)
   use M_GRIDSETTINGS
   use unstruc_display
   use unstruc_version_module, only : unstruc_company, unstruc_program
   use m_spline2curvi

   implicit none
   integer, intent(out) :: jacancelled !< Whether or not (1/0) user has pressed 'Esc' in parameter screen.

   integer :: i
   integer :: ifexit
   integer :: ifinit
   integer :: ih
   integer :: il
   integer :: imp
   integer :: inp
   integer :: ir
   integer :: iw
   integer :: ixp
   integer :: iyp
   integer :: key
   integer :: nbut
   integer :: nlevel
   integer :: numfldactual
   integer :: numparactual
   integer :: idum=0

   integer, parameter :: NUMPAR = 14, NUMFLD = 2*NUMPAR
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line

   jacancelled = 0
   NLEVEL    = 4

   OPTION(1) = 'MAXIMUM NUMBER OF GRIDCELLS ALONG SPLINE' ; IT(1*2)  = 2
   OPTION(2) = 'MAXIMUM NUMBER OF GRIDCELLS PERP. SPLINE' ; IT(2*2)  = 2
   OPTION(3) = 'ASPECT RATIO OF FIRST GRID LAYER        ' ; IT(3*2)  = 6
   OPTION(4) = 'GRID LAYER HEIGHT GROWTH FACTOR         ' ; IT(4*2)  = 6
   OPTION(5) = 'MAXIMUM GRID LENGTH ALONG CENTER SPLINE ' ; IT(5*2)  = 6
   OPTION(6) = 'CURVATURE-ADAPTED GRID SPACING     (0,1)' ; IT(6*2)  = 2
   OPTION(7) = 'GROW GRID OUTSIDE FIRST PART       (0,1)' ; IT(7*2)  = 2
   OPTION(8) = 'MAX. NUM. OF GRIDCELL PERP. IN UNI. PART' ; IT(8*2)  = 2
   OPTION(9) = '                                        ' ; IT(9*2)  = 2
   OPTION(10) = 'GRIDPTS. ON TOP OF EACH OTHER TOLERANCE ' ; IT(10*2)  = 6
   OPTION(11) = 'MINIMUM ABS. SINE OF CROSSING ANGLES   ' ; IT(11*2) = 6
   OPTION(12) = 'PREVENT COLL.S W/OTHER GRIDPARTS  (0,1) ' ; IT(12*2) = 2
   OPTION(13) = '                                        ' ; IT(13*2) = 2
   OPTION(14) = 'UNIFORM GRIDSIZE (NETBND2GRID ONLY) (m) ' ; IT(14*2) = 6
!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6
   HELPM (1) = 'INTEGER VALUE <                                             '
   HELPM (2) = 'INTEGER VALUE <                                             '
   HELPM (3) = 'REAL    VALUE <                                             '
   HELPM (4) = 'REAL    VALUE <                                             '
   HELPM (5) = 'REAL    VALUE <                                             '
   HELPM (6) = 'INTEGER VALUE <                                             '
   HELPM (7) = 'INTEGER VALUE <                                             '
   HELPM (8) = 'INTEGER VALUE <                                             '
   HELPM (9) = '                                                            '
   HELPM (10) = 'REAL    VALUE <                                             '
   HELPM (11) = 'REAL    VALUE <                                             '
   HELPM (12) = 'INTEGER VALUE <                                             '
   HELPM (13) = '                                                            '
   HELPM (14) = 'REAL    VALUE <                                             '

   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL     = IR + 1  ; IR     = IL + 1
      IS(IL) = 82      ; IS(IR) = 10
      IX(IL) = 10      ; IX(IR) = 100
      IY(IL) = 2*I     ; IY(IR) = 2*I
      IT(IL) = 1001    ! ir staat hierboven
   ENDDO

   ! Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW   = NPOS(3)
   IXP = NPOS(1) + (IWS-IW)/2
   IYP  = NPOS(2)
   IH  = IHS - 9

   ! Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program)// ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)

   ! Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = , Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

   ! Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)
   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

   ! Define a new form by supplying arrays containing Field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

   ! Define a help field and define help strings for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFORMPUTINTEGER (2*1, MFAC               )
   CALL IFORMPUTINTEGER (2*2, NFAC               )
   CALL IFORMPUTDOUBLE  (2*3, daspect,   '(F7.3)')
!   CALL IFORMPUTDOUBLE  (2*4, maxaspect, '(F7.3)')
   CALL IFORMPUTDOUBLE  (2*4, dgrow,     '(F7.3)')
   CALL IFORMPUTDOUBLE  (2*5, dwidth,    '(F11.3)')
   CALL IFORMPUTINTEGER (2*6, jacurv             )
   CALL IFORMPUTINTEGER (2*7, jaoutside          )
   CALL IFORMPUTINTEGER (2*8, NFACUNIMAX         )
!   CALL IFORMPUTINTEGER (2*9, idum               )
   CALL IFORMPUTDOUBLE  (2*10, dtolLR,    '(F11.5)')
   CALL IFORMPUTDOUBLE  (2*11, dtolcos,   '(F11.3)')
   CALL IFORMPUTINTEGER (2*12, jaCheckFrontCollision)
   CALL IFORMPUTDOUBLE  (2*14, dunigridsize,   '(F11.3)')

   NFAC = max(NFAC,NFACUNIMAX)

   ! Display the form with numeric fields left justified and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
   ! check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN
           CALL IFORMGETINTEGER (2*1 , MFAC     )
           CALL IFORMGETINTEGER (2*2 , NFAC     )
           CALL IFORMGETDOUBLE  (2*3 , daspect  )
!           CALL IFORMGETDOUBLE  (2*4 , maxaspect)
           CALL IFORMGETDOUBLE  (2*4 , dgrow    )
           CALL IFORMGETDOUBLE  (2*5 , dwidth   )
           CALL IFORMGETINTEGER (2*6 , jacurv   )
           CALL IFORMGETINTEGER (2*7 , jaoutside)
           CALL IFORMGETINTEGER (2*8 , NFACUNIMAX)
!           CALL IFORMGETINTEGER (2*9 , idum)
           CALL IFORMGETDOUBLE  (2*10, dtolLR)
           CALL IFORMGETDOUBLE  (2*11, dtolcos)
           CALL IFORMGETINTEGER (2*12, jaCheckFrontCollision)
           CALL IFORMGETDOUBLE  (2*14, dunigridsize)
       ELSEIF (KEY .EQ. 23) THEN
          jacancelled = 1
       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

end subroutine change_spline2curvi_param

!> KML export parameter menu
subroutine change_kml_parameters(jacancelled)
   use m_kml_parameters
   use unstruc_display
   use unstruc_version_module, only : unstruc_company, unstruc_program

   implicit none
   integer, intent(out) :: jacancelled !< Whether or not (1/0) user has pressed 'Esc' in parameter screen.

   integer :: i
   integer :: ifexit
   integer :: ifinit
   integer :: ih
   integer :: il
   integer :: imp
   integer :: inp
   integer :: ir
   integer :: iw
   integer :: ixp
   integer :: iyp
   integer :: key
   integer :: nbut
   integer :: nlevel
   integer :: numfldactual
   integer :: numparactual
   integer :: idum=0

   integer, parameter :: NUMPAR = 9, NUMFLD = 2*NUMPAR
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line

   jacancelled = 0
   NLEVEL    = 4

   OPTION(1) = 'Export flat view of unstruct. grid (0/1)' ; IT(1*2)  = 2
   OPTION(2) = 'Export depth view of grid cells    (0/1)' ; IT(2*2)  = 2
   OPTION(3) = '* flat or 3D view of depths        (0/1)' ; IT(3*2)  = 2
   OPTION(4) = '* Altitude exaggeration factor          ' ; IT(4*2)  = 6
   OPTION(5) = '* Offset altitude with deepest pt. (0/1)' ; IT(5*2)  = 2
   OPTION(6) = '* Additional offset (+ = upward)        ' ; IT(6*2)  = 6
   OPTION(7) = '* Dummy altitude for missing values     ' ; IT(7*2)  = 6
   OPTION(8) = '* Minimal value for color scale         ' ; IT(8*2)  = 6
   OPTION(9) = '* Maximal value for color scale         ' ; IT(9*2)  = 6
!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6
   HELPM (1) = 'Integer value 0/1, flat grid view is faster.                '
   HELPM (2) = 'Integer value 0/1, depth view is nicer.                     '
   HELPM (3) = 'Integer value 0/1, 3D view is nicer, 2D aligns better.      '
   HELPM (4) = 'Altitude differences are multiplied by this factor.         '
   HELPM (5) = 'When set to 0, grid may disappear "under water".            '
   HELPM (6) = 'Additional offset, to lift/lower the 3D grid.               '
   HELPM (7) = 'Missing zk values will be replaced by this dummy in the kml.'
   HELPM (8) = 'Color scaling starts at this value, lower zks are clipped.  '
   HELPM (9) = 'Color scaling stops at this value, higher zks are clipped.  '

   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL     = IR + 1  ; IR     = IL + 1
      IS(IL) = 82      ; IS(IR) = 10
      IX(IL) = 10      ; IX(IR) = 100
      IY(IL) = 2*I     ; IY(IR) = 2*I
      IT(IL) = 1001    ! ir staat hierboven
   ENDDO

   ! Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW   = NPOS(3)
   IXP = NPOS(1) + (IWS-IW)/2
   IYP  = NPOS(2)
   IH  = IHS - 9

   ! Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program)// ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)

   ! Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = , Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

   ! Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)
   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

   ! Define a new form by supplying arrays containing Field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

   ! Define a help field and define help strings for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFORMPUTINTEGER (2*1, kml_janet           )
   CALL IFORMPUTINTEGER (2*2, kml_jadepth         )
   CALL IFORMPUTINTEGER (2*3, kml_jadepth3d       )
   CALL IFORMPUTDOUBLE  (2*4, kml_altfact,    '(F4.1)')
   CALL IFORMPUTINTEGER (2*5, kml_jaoffsetzk)
   CALL IFORMPUTDOUBLE  (2*6, kml_useroffset, '(F6.1)')
   CALL IFORMPUTDOUBLE  (2*7, kml_dmiss,      '(F6.1)')
   CALL IFORMPUTDOUBLE  (2*8, kml_zmin,       '(F6.1)')
   CALL IFORMPUTDOUBLE  (2*9, kml_zmax,       '(F6.1)')

   ! Display the form with numeric fields left justified and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
   ! check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN
           CALL IFORMGETINTEGER (2*1 , kml_janet)
           CALL IFORMGETINTEGER (2*2 , kml_jadepth)
           CALL IFORMGETINTEGER (2*3 , kml_jadepth3d)
           CALL IFORMGETDOUBLE  (2*4 , kml_altfact)
           CALL IFORMGETINTEGER (2*5 , kml_jaoffsetzk)
           CALL IFORMGETDOUBLE  (2*6 , kml_useroffset)
           CALL IFORMGETDOUBLE  (2*7 , kml_dmiss)
           CALL IFORMGETDOUBLE  (2*8 , kml_zmin)
           CALL IFORMGETDOUBLE  (2*9 , kml_zmax)
       ELSEIF (KEY .EQ. 23) THEN
          jacancelled = 1
       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

end subroutine change_kml_parameters


      SUBROUTINE PLUSABSD(XK,YK,ZK,NUMK,KEY,EA)
      use m_polygon
      use m_missing
      use geometry_module, only: dpinpok

      implicit none
      INTEGER, PARAMETER     :: MAXOP = 64
      CHARACTER*40           :: OPTION(MAXOP),EXP(MAXOP)
      INTEGER                :: NUMK, KEY
      DOUBLE PRECISION       :: XK(NUMK), YK(NUMK), ZK(NUMK), EA(NUMK)
      DOUBLE PRECISION       :: XI, YI, ZI, DA, AF, RD

      INTEGER                :: ichange, inhul, ja, k, maxexp, maxopt, nwhat, kk
      DOUBLE PRECISION, SAVE :: A   = 1D0

      JA         = 0
      EXP(1)     = 'MENU TIG                                '
      EXP(2)     = 'HOW TO REPLACE THE VALUES               '
      OPTION(1)  = 'FIELD = UNIFORM VALUE, only missings    '
      OPTION(2)  = 'FIELD = UNIFORM VALUE, all points       '
      OPTION(3)  = 'FIELD = MAX(FIELD,UNIFORM VALUE)        '
      OPTION(4)  = 'FIELD = MIN(FIELD,UNIFORM VALUE)        '
      OPTION(5)  = 'FIELD = FIELD + UNIFORM VALUE           '
      OPTION(6)  = 'FIELD = FIELD * UNIFORM VALUE           '
      OPTION(7)  = 'FIELD = MISSING VALUE -999.             '
      OPTION(8)  = 'FIELD = MISSING VALUE ABOVE UNIF VALUE  '
      OPTION(9)  = 'FIELD = MISSING VALUE BELOW UNIF VALUE  '
      OPTION(10) = 'SPECIFY UNIFORM VALUE                   '
      MAXOPT     = 10
      ICHANGE    = 1
   10 CONTINUE
      NWHAT = ICHANGE
      CALL SHOWREAL('UNIFORM VALUE = ',A)
      CALL MENUV3(NWHAT,OPTION,MAXOPT,EXP,MAXEXP)
      CALL IWINCLOSE(1)
      IF (NWHAT .EQ. 0) THEN
         KEY = 0
         RETURN
      ELSE
         IF (NWHAT .LE. 8) THEN
            ICHANGE = NWHAT
            IF (A .EQ. dmiss) THEN
               CALL GETREAL('FIRST SPECIFY UNIFORM VALUE = ',A)
               IF (A .NE. dmiss) JA = 1
               GOTO 10
            ELSE
               JA = 1
            ENDIF
         ELSE IF (NWHAT == 9) THEN
            ICHANGE = NWHAT
         ELSE IF (NWHAT == 10) THEN
            CALL GETREAL('SPECIFY UNIFORM VALUE = ',A)
            IF (A .NE. dmiss) JA = 1
            GOTO 10
         ENDIF
      ENDIF


      IF (NPL .LE. 2) THEN
         CALL CONFRM('NO POLYGON, SO INCLUDE all FIELD POINTS ? ',JA)
         IF (JA .EQ. 0) THEN
            KEY = 0
            RETURN
         ENDIF
      ENDIF
     ! CALL SAVENET()
      CALL READYY('CHANGE FIELD VALUES', 0d0)
      DO 20 K = 1,NUMK
         if (mod (k,1000) == 0) then
            AF = dble(K) / dble(NUMK)
            CALL READYY('CHANGE FIELD VALUES', AF)
         endif
         XI = XK(K)
         YI = YK(K)
         ZI = ZK(K)
         RD = EA(K)
         JA = 0
         IF (NPL .GE. 3) THEN
            CALL DPINPOK( XI, YI, ZI, NPL, XPL, YPL, INHUL, jins, dmiss)
            IF (INHUL .EQ. 1) JA = 1
         ELSE
            JA = 1
         ENDIF
           IF (JA .EQ. 1) THEN
              DA = A
              IF (ICHANGE .EQ. 1) THEN
                 IF (RD == dmiss) THEN
                    EA(K) = DA
                 ENDIF
              ELSE IF (ICHANGE .EQ. 2) THEN
                    EA(K) = DA
              ELSE IF (ICHANGE .EQ. 3) THEN
                 IF (RD /= dmiss) EA(K) = MAX(EA(K),DA)
              ELSE IF (ICHANGE .EQ. 4) THEN
                 IF (RD /= dmiss) EA(K) = MIN(EA(K),DA)
              ELSE IF (ICHANGE .EQ. 5) THEN
                 IF (RD /= dmiss) EA(K) = EA(K) + DA
              ELSE IF (ICHANGE .EQ. 6) THEN
                 IF (RD /= dmiss) EA(K) = EA(K) * DA
              ELSE IF (ICHANGE .EQ. 7) THEN
                 EA(K) = dmiss
              ELSE IF (ICHANGE .EQ. 8) THEN
                 IF (RD /= dmiss .AND. EA(K) > DA) EA(K) = DMISS 
              ELSE IF (ICHANGE .EQ. 9) THEN
                 IF (RD /= dmiss .AND. EA(K) < DA) EA(K) = DMISS 
              ENDIF
           ENDIF
   20 CONTINUE
      CALL READYY('CHANGE FIELD VALUES', -1d0)
      KEY = 3
      RETURN
      END SUBROUTINE PLUSABSD


      SUBROUTINE PLUSABSI(XK,YK,ZK,KN,NUMK,NUML,KEY,kndefault)
      use M_polygon
      USE m_missing
      use network_data, only: kn3typ
      use geometry_module, only: dpinpok
      use gridoperations
      implicit none
      integer, parameter :: MAXOP = 64
      integer :: NUMK, NUML, KEY
      DOUBLE PRECISION XK(NUMK), YK(NUMK), ZK(NUMK), XI, YI, ZI
      INTEGER KN(3,NUML)
      integer, intent(inout) :: kndefault !< Default uniform value (e.g. kn3typ), will be changed too at call site when user changes it in the dialog.
      CHARACTER*40 OPTION(MAXOP),EXP(MAXOP)

      double precision :: af
      integer :: ia
      integer :: ichange
      integer :: inhul
      integer :: ja
      integer :: k1
      integer :: k2
      integer :: l
      integer :: maxexp
      integer :: maxopt
      integer :: nwhat
      double precision :: rd

      double precision, save :: A
      integer, save :: INI = 0
      A   = kndefault

      JA        = 0
      EXP(1)    = 'MENU TIG                                '
      EXP(2)    = 'HOW TO REPLACE THE VALUES               '
      OPTION(1) = 'FIELD = UNIFORM VALUE, only missings    '
      OPTION(2) = 'FIELD = UNIFORM VALUE, all points       '
      OPTION(3) = 'FIELD = MAX(FIELD,UNIFORM VALUE)        '
      OPTION(4) = 'FIELD = MIN(FIELD,UNIFORM VALUE)        '
      OPTION(5) = 'FIELD = FIELD + UNIFORM VALUE           '
      OPTION(6) = 'FIELD = FIELD * UNIFORM VALUE           '
      OPTION(7) = 'FIELD = MISSING VALUE -999.             '
      OPTION(8) = 'SPECIFY UNIFORM VALUE                   '
      MAXOPT    = 8
      ICHANGE   = 1
   10 CONTINUE
      NWHAT = ICHANGE
      CALL SHOWREAL('UNIFORM VALUE = ',A)
      CALL MENUV3(NWHAT,OPTION,MAXOPT,EXP,MAXEXP)
      CALL IWINCLOSE(1)
      IF (NWHAT .EQ. 0) THEN
         KEY = 0
         RETURN
      ELSE
         IF (NWHAT .LE. 6) THEN
            ICHANGE = NWHAT
            IF (A .EQ. dmiss) THEN
               CALL GETREAL('FIRST SPECIFY UNIFORM VALUE = ',A)
               IF (A .NE. dmiss) JA = 1
               GOTO 10
            ELSE
               JA = 1
            ENDIF
         ELSE IF (NWHAT == 7) THEN
            ICHANGE = NWHAT
         ELSE IF (NWHAT == 8) THEN
            CALL GETREAL('SPECIFY UNIFORM VALUE = ',A)
            IF (A .NE. dmiss) then
                JA = 1
                kndefault = int(A)
            end if
            GOTO 10
         ENDIF
      ENDIF

      IF (NPL .LE. 2) THEN
         CALL CONFRM('NO POLYGON, SO INCLUDE all FIELD POINTS ? ',JA)
         IF (JA .EQ. 0) THEN
            KEY = 0
            RETURN
         ENDIF
      ENDIF
      CALL SAVENET()
      CALL READYY('CHANGE FIELD VALUES', 0d0)
      KMOD = MAX(1,NUML/100)
      DO 20 L = 1,NUML
         IF (MOD(L,KMOD) == 0) THEN
            AF = dble(L) / dble(NUML)
            CALL READYY('CHANGE FIELD VALUES', AF)
         ENDIF
         K1 = KN(1,L)
         K2 = KN(2,L)
         IF (K1 .EQ. 0 .OR. K2 .EQ. 0) GOTO 20
         XI = (XK(K1) + XK(K2))/2
         YI = (YK(K1) + YK(K2))/2
         ZI = (ZK(K1) + ZK(K2))/2
         RD = kn(3,L)
         JA = 0
         IF (NPL .GE. 3) THEN
            CALL DPINPOK( XI, YI, ZI, NPL, XPL, YPL, INHUL, jins, dmiss)
            IF (INHUL .EQ. 1) JA = 1
         ELSE
            JA = 1
         ENDIF
         IA = A
         IF (JA .EQ. 1) THEN
            IF (ICHANGE .EQ. 1) THEN
               IF (RD == dmiss) THEN
                  kn(3,L) = IA
               ENDIF
            ELSE IF (ICHANGE .EQ. 2) THEN
                  kn(3,L) = IA
            ELSE IF (ICHANGE .EQ. 3) THEN
               IF (RD == dmiss) kn(3,L) = MAX(kn(3,L),IA)
            ELSE IF (ICHANGE .EQ. 4) THEN
               IF (RD == dmiss) kn(3,L) = MIN(kn(3,L),IA)
            ELSE IF (ICHANGE .EQ. 5) THEN
               IF (RD == dmiss) kn(3,L) = kn(3,L) + IA
            ELSE IF (ICHANGE .EQ. 6) THEN
               IF (RD == dmiss) kn(3,L) = kn(3,L) * IA
            ELSE IF (ICHANGE .EQ. 7) THEN
               kn(3,L) = INT(dmiss)
            ENDIF
         ENDIF
   20 CONTINUE
      CALL READYY('CHANGE FIELD VALUES', -1d0)
      KEY = 3
      RETURN
      END SUBROUTINE PLUSABSI





  SUBROUTINE TEKLAN(NCOL)
  USE M_LANDBOUNDARY
  use m_wearelt
  USE unstruc_colors
  use gridoperations

  implicit none
  integer :: NCOL
  integer :: NDRAW
  COMMON /DRAWTHIS/ ndraw(50)

  integer :: j1
  integer :: k
  integer :: ncl
  integer :: ncold
  double precision :: rh

  IF (NDRAW(3) .EQ. 0) return
  
  IF (NDRAW(3) .EQ. 4 .or. NDRAW(3) .EQ. 8) then 
     call linewidth(3) 
  endif   
  
  CALL DISP3C(XLAN, YLAN, ZLAN, NCLAN, MXLAN, 0d0, NCOL)
  
  NCOLD = 0
  DO K = 1,MXLAN
     NCL = NCLAN(K)
     IF (NCL .LT. 0) THEN
        IF (NCOLD .EQ. 0) THEN
           NCOLD = ABS(NCL)
           J1    = K
        ELSE IF (ABS(NCL) .NE. NCOLD) THEN
           CALL PFILLER(XLAN(J1),YLAN(J1),K-J1,NCOLD,NCOLD)
           NCOLD = 0
        ENDIF
     ELSE IF (NCOLD .NE. 0) THEN
        CALL PFILLER(XLAN(J1),YLAN(J1),K-J1,NCOLD,NCOLD)
        NCOLD = 0
     ENDIF
  ENDDO
  if (ndraw(3) == 2 .or. ndraw(3) == 6) then
     CALL SETCOL(NCOLDG)
     rh = 0.2*rcir
     DO K = 1,MXLAN
        if ( inview(xlan(k), ylan(k) ) )  then
            !CALL PTABS( XLAN(K), YLAN(K) )
            call fbox(xlan(k)-rh, ylan(k)-rh, xlan(k)+rh, ylan(k)+rh)
        endif
     enddo
  endif

  if (ndraw(3) == 3 .or. ndraw(3) == 7) then
     CALL SETCOL(NCOLDG)
     DO K = 1,MXLAN
        if ( inview ( xlan(k), ylan(k) ) )  then
            RH = 0
            CALL DHITEXT(K,XLAN(K),YLAN(K),RH)
        endif
     enddo
  endif
  
  call linewidth(1)  
    
  RETURN
  END SUBROUTINE TEKLAN


  SUBROUTINE TEKBOTTOM(MET)
  use m_wearelt
  implicit none
  double precision :: dz
  integer :: i
  integer :: jav
  integer :: jview
  integer :: k
  integer :: k1
  integer :: k2
  integer :: nz
  double precision :: uf
  double precision :: vf
  double precision :: wd
  double precision :: wf
  double precision :: xyz
  double precision :: ybot
  double precision :: ytop
  integer :: MET
  COMMON /FLOWSTUFF/ UF, VF, WF, YBOT, YTOP
  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
  DOUBLE PRECISION XD,YD,ZD,XX1,XX2,ZZ1,ZZ2
  CALL SETCOL(160)
  IF (MET .EQ. 1) RETURN

  WD  = 1000
  XX2 = WD/2
  XX1 = -XX2
  ZZ2 = WD/2
  ZZ1 = -ZZ2
  DZ = 0
  NZ = 1

  IF (JVIEW .GE. 3) THEN
      NZ = 11
      DZ = WD / (NZ-1)
  ENDIF

  IF (MET .EQ. 2) THEN
     K1 = 1
     K2 = 2
  ELSE IF (MET .EQ. 3) THEN
     K1 = 2
     K2 = 2
  ELSE IF (MET .EQ. 4) THEN
     K1 = 1
     K2 = 1
  ENDIF

  YD = YTOP
  CALL SETCOL(128) ! (112)
  DO K = K1,K2
     IF (K .EQ. 2) THEN
        YD = YBOT
        CALL SETCOL(89) ! 128)
     ENDIF
     XD  = XX1
     ZD  = ZZ1
     DO I = 1,NZ
        CALL DMOVABS( XX1, YD, ZD)
        CALL DLNABS ( XX2, YD, ZD)
        CALL DMOVABS(  XD, YD, ZZ1)
        CALL DLNABS (  XD, YD, ZZ2)
        ZD = ZD + DZ
        XD = XD + DZ
     ENDDO
  ENDDO

  RETURN
  END SUBROUTINE TEKBOTTOM



!> select link for directional refinement in GUI
   subroutine getlink_GUI(xp, yp, L)
      implicit none

      double precision, intent(out) :: xp, yp   !< coordinates of clicked point
      integer,          intent(out) :: L        !< clicked link number

      double precision     :: zp

      integer              :: num, nwhat, nput, numb, key

      L     = 0

      call ktext(' Refine net       ',1,3,15)

      num   = 0
      nwhat = 0
      nput  = 55
      numb  = 10
      key   = 0

      do
         CALL DRAWNU(KEY)
         call putget_un(num,nwhat,nput,numb,xp,yp,key)

         if ( key.eq.23 )  then     ! escape
            exit
         else if ( key.eq.21) then  ! left mouse button
            call islink(L,xp,yp,zp)
            if ( L.gt.0 ) then   ! link found
               call teklink(L,31)
               exit
            end if
!        the following is copied from editnetw (zoom, panning)
         else if (key .eq. 43 .or. key .eq. 140) then
            call kplotplusmin(1)
            key = 3
         else if (key .eq. 45 .or. key .eq. 141) then
            call kplotplusmin(-1)
            key = 3
         else if (key .eq. 133) then        ! page down
            call nplotplusmin(1)
            key = 3
         else if (key .eq. 143) then        ! delete
            call nplotplusmin(-1)
            key = 3
         end if
      end do

      if ( L.lt.1 ) then
         call qnerror('no link clicked: exitting', ' ', ' ')
      end if

      return
   end subroutine




!> refinecellsandfaces2 parameter menu
subroutine change_samples_refine_param(jacancelled)
   use unstruc_display
   use unstruc_version_module, only : unstruc_company, unstruc_program
   use m_samples_refine

   implicit none
   integer, intent(out) :: jacancelled !< Whether or not (1/0) user has pressed 'Esc' in parameter screen.

   integer :: i
   integer :: ifexit
   integer :: ifinit
   integer :: ih
   integer :: il
   integer :: imp
   integer :: inp
   integer :: ir
   integer :: iw
   integer :: ixp
   integer :: iyp
   integer :: key
   integer :: nbut
   integer :: nlevel
   integer :: numfldactual
   integer :: numparactual

   integer, parameter :: NUMPAR = 14, NUMFLD = 2*NUMPAR
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*60, HELPM(NUMPAR)*60
   character(len=60) :: text
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   
   external :: highlight_form_line

   jacancelled = 0
   NLEVEL    = 4
   
   text = ''
   WRITE(text, "('TYPE: RIDGES (', I1, '), WAVE COURANT NUMBER (', I1,  ')')") ITYPE_RIDGE, ITYPE_WAVECOURANT
   
   OPTION(1)  = text                                         ; IT(1*2)  = 2
   OPTION(2)  = ''                                           ; IT(2*2)  = 0
   OPTION(3)  = 'RIDGE DETECTION'                            ; IT(3*2)  = 0
   OPTION(4)  = 'CELL SIZE * TYPICAL OBSTACLE HEIGHT   [m2]' ; IT(4*2)  = 6
   OPTION(5)  = 'MINIMUM     TYPICAL OBSTACLE HEIGHT   [m] ' ; IT(5*2)  = 6
   OPTION(6)  = 'MINIMUM CELL EDGE LENGTH              [m] ' ; IT(6*2)  = 6
   OPTION(7)  = 'NUMBER OF SAMPLE SMOOTHING ITERATIONS [-] ' ; IT(7*2)  = 2
   OPTION(8)  = '                                          ' ; IT(8*2)  = 0
   OPTION(9)  = 'WAVE COURANT NUMBER                       ' ; IT(9*2)  = 0
   OPTION(10) = 'MAXIMUM TIME-STEP                     [s] ' ; IT(10*2) = 6
   OPTION(11) = 'MINIMUM CELL EDGE LENGTH              [m] ' ; IT(11*2) = 6
   OPTION(12) = 'DIRECTIONAL REFINEMENT (1) OR NOT (0)     ' ; IT(12*2) = 2
   OPTION(13) = 'USE SAMPLES OUTSIDE CELL (1) OR NOT (0)   ' ; IT(13*2) = 2
   OPTION(14) = '                                          ' ; IT(14*2) = 0

   HELPM (1)  = 'INTEGER VALUE <                                             '
   HELPM (2)  = '                                                            '
   HELPM (3)  = '                                                            '
   HELPM (4)  = 'REAL    VALUE <                                             '
   HELPM (5)  = 'REAL    VALUE <                                             '
   HELPM (6)  = 'REAL    VALUE <                                             '
   HELPM (7)  = 'INTEGER VALUE <                                             '
   HELPM (8)  = '                                                            '
   HELPM (9)  = '                                                            '  
   HELPM (10) = 'REAL    VALUE <                                             '
   HELPM (11) = 'REAL    VALUE <                                             '
   HELPM (12) = 'INTEGER VALUE <                                             '
   HELPM (13) = 'INTEGER VALUE <                                             '
   HELPM (14) = '                                                            '

   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL     = IR + 1  ; IR     = IL + 1
      IS(IL) = 82      ; IS(IR) = 10
      IX(IL) = 10      ; IX(IR) = 100
      IY(IL) = 2*I     ; IY(IR) = 2*I
      IT(IL) = 1001    ! ir staat hierboven
      
      if ( IT(IR).eq.0 ) then
        IS(IR) = 0    
      end if
   ENDDO

   ! Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW   = NPOS(3)
   IXP = NPOS(1) + (IWS-IW)/2
   IYP  = NPOS(2)
   IH  = IHS - 9

   ! Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program)// ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)

   ! Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = , Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

   ! Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)
   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

   ! Define a new form by supplying arrays containing Field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

   ! Define a help field and define help strings for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFORMPUTINTEGER(2*1, irefinetype,  '(F12.3)')
   CALL IFORMPUTDOUBLE(2*4,  threshold,    '(F12.3)')
   CALL IFORMPUTDOUBLE(2*5,  thresholdmin, '(F12.3)')
   CALL IFORMPUTDOUBLE(2*6,  hmin,         '(F12.3)')
   CALL IFORMPUTINTEGER(2*7, Nsamplesmooth)
   CALL IFORMPUTDOUBLE(2*10, Dt_maxcour,         '(F12.3)')
   CALL IFORMPUTDOUBLE(2*11, hmin,           '(F12.3)')
   CALL IFORMPUTINTEGER(2*12, jadirectional)
   CALL IFORMPUTINTEGER(2*13, jaoutsidecell)

   ! Display the form with numeric fields left justified and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
   ! check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN
           CALL IFORMGETINTEGER(2*1, irefinetype)
           CALL IFORMGETDOUBLE(2*4 , threshold)
           CALL IFORMGETDOUBLE(2*5 , thresholdmin)
           CALL IFORMGETDOUBLE(2*6 , hmin)
           CALL IFORMGETINTEGER(2*7 , Nsamplesmooth)
           CALL IFORMGETDOUBLE(2*10 , Dt_maxcour)
           CALL IFORMGETDOUBLE(2*11 , hmin)
           CALL IFORMGETINTEGER(2*12 , jadirectional)
           CALL IFORMGETINTEGER(2*13 , jaoutsidecell)
           
       ELSEIF (KEY .EQ. 23) THEN
          jacancelled = 1
       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       goto 1234
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

1234 continue
   if ( Nsamplesmooth.ne.Nsamplesmooth_last ) then
      iHesstat = iHesstat_DIRTY
   end if

   return
end subroutine change_samples_refine_param



!> plot the ridges
subroutine plot_ridges(ierror)
   
   use m_samples
   use m_samples_refine
   use m_missing
   use geometry_module, only: dbdistance

   implicit none

   integer, intent(out) :: ierror   !< error (1) or not (0)

   integer :: i, j, ip

   double precision :: Dx, Dy, dum, Dh, x0, y0, x1, y1, x2, y2

   double precision, external :: comp_sampleDh

   ierror = 1

   if ( iHesstat.ne.iHesstat_OK ) goto 1234

!  plot ridge
   do i=1,MXSAM
      do j=1,MYSAM
!        compute sample mesh width
         Dh = comp_sampleDh(i,j)

         ip = i+(j-1)*MXSAM

         if ( abs(zss(5,i,j)).gt.0.5d0*Dh .or. zss(4,i,j).gt.-1d-8 .or. zss(5,i,j).eq.DMISS ) cycle

         Dx =  zss(3,i,j)
         Dy = -zss(2,i,j)
         dum = Dh/sqrt(Dx**2+Dy**2+1d-16)
         Dx = Dx*dum
         Dy = Dy*dum

         call setcol(204)

         x0 = xs(ip)+zss(2,i,j)*zss(5,i,j)
         y0 = ys(ip)+zss(3,i,j)*zss(5,i,j)
         x1 = min(max(x0-Dx,xs(ip)-0.5d0*Dh), xs(ip)+0.5*Dh)
         y1 = min(max(y0-Dy,ys(ip)-0.5d0*Dh), ys(ip)+0.5*Dh)
         x2 = min(max(x0+Dx,xs(ip)-0.5d0*Dh), xs(ip)+0.5*Dh)
         y2 = min(max(y0+Dy,ys(ip)-0.5d0*Dh), ys(ip)+0.5*Dh)

         call movabs(x1,y1)
         call lnabs(x2,y2)
      end do
   end do

!   call qnerror(' ', ' ', ' ')

   ierror = 0
1234 continue

   return
end subroutine plot_ridges

!----------------------------------------------------------------------
! subroutines from rest.F90
!----------------------------------------------------------------------
      SUBROUTINE CHANGECOLOR(XP,YP)
      use unstruc_colors
      implicit none
      double precision :: dv
      integer :: ic
      integer :: jaauto
      integer :: key
      integer :: n1
      integer :: n2
      integer :: n3
      integer :: ncols
      integer :: nie
      integer :: nis
      integer :: nlevel
      integer :: numcol
      integer :: nv
      double precision :: val
      double precision :: vmax
      double precision :: vmin
      double precision :: xp
      double precision :: yp
      CHARACTER TEX*26, WRDKEY*40

      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
      COMMON /HELPNOW/ WRDKEY,NLEVEL

      INTEGER NCL(3)

      IC = 1

      CALL IMOUSECURSORHIDE()
      CALL DISPUT(35)

      CALL GETCOLORNUMBER(XP,YP,NUMCOL,N1,N2,N3)
      NCL(1) = N1
      NCL(2) = N2
      NCL(3) = N3

      CALL SETCOL(NUMCOL)
      CALL DISVALCOLORS (NUMCOL,NCL(1),NCL(2),NCL(3),IC)

   20 CONTINUE

      CALL INKEYEVENT(KEY)

      IF (KEY .EQ. 131) THEN
         IC = IC - 1
         IF (IC .EQ. 0) IC = 3
      ELSE IF (KEY .EQ. 130) THEN
         IC = IC + 1
         IF (IC .EQ. 4) IC = 1
      ELSE IF (KEY .EQ. 128) THEN
         NCL(IC) = MIN(255,NCL(IC) + 1)
         CALL IGRPALETTERGB(NUMCOL,NCL(1),NCL(2),NCL(3))
      ELSE IF (KEY .EQ. 129) THEN
         NCL(IC) = MAX(0  ,NCL(IC) - 1)
         CALL IGRPALETTERGB(NUMCOL,NCL(1),NCL(2),NCL(3))
      ELSE IF (KEY .EQ. 171) THEN
         CALL HELP(WRDKEY,3)
      ELSE IF (KEY .EQ. 13 .OR. KEY .GE. 251 .AND. KEY .LE. 253) THEN
         CALL ORGLOCATOR(XP,YP)
         CALL IMOUSECURSORSHOW()
         RETURN
      ELSE IF (KEY .EQ. 27) THEN
         CALL IGRPALETTERGB(NUMCOL,N1,N2,N3)
         CALL ORGLOCATOR(XP,YP)
         CALL IMOUSECURSORSHOW()
         RETURN
      ENDIF

      CALL SETCOL(NUMCOL)
      CALL DISVALCOLORS(NUMCOL,NCL(1),NCL(2),NCL(3),IC)
      CALL ALLCOLOURS()

      GOTO 20
      END

      SUBROUTINE DISVALCOLORS(NUMCOL,N1,N2,N3,IC)
      USE M_DEVICES
      implicit none
      integer :: ic
      integer :: n1
      integer :: n2
      integer :: n3
      integer :: numcol
      CHARACTER TEXT*47
      IF (IC .EQ. 1) THEN
         TEXT = 'COLOR NUMBER:     RED:     g    :     b   :    '
      ELSE IF (IC .EQ. 2) THEN
         TEXT = 'COLOR NUMBER:     r  :     GREEN:     b   :    '
      ELSE
         TEXT = 'COLOR NUMBER:     r  :     g    :     BLUE:    '
      ENDIF
      WRITE(TEXT(15:17) ,'(I3)') NUMCOL
      WRITE(TEXT(23:25) ,'(I3)') N1
      WRITE(TEXT(34:36) ,'(I3)') N2
      WRITE(TEXT(44:46) ,'(I3)') N3
      CALL KTEXT(TEXT,IWS-46,4,15)
      RETURN
      END

      SUBROUTINE EDITCOLOURTABLE(MODE,KEY)
      use unstruc_colors
      implicit none
      integer :: key
      integer :: mode
      integer :: n1
      integer :: n1c
      integer :: n2
      integer :: n2c
      integer :: n3
      integer :: n3c
      integer :: nlevel
      integer :: nput
      integer :: num
      integer :: numb
      integer :: numcol
      integer :: numcolc
      integer :: nwhat
      double precision :: xp
      double precision :: yp

      COMMON /HELPNOW/ WRDKEY,NLEVEL

      CHARACTER TEX*26, WRDKEY*40, TEX2*4

      TEX    = ' EDIT COLORTABLE          '
      WRDKEY = TEX
      NLEVEL =  2
      NUM    =  0
      NWHAT  =  0
      NPUT   =  33
      NUMB   =  14

   10 CONTINUE
      CALL DRAWNU(KEY)
      CALL ALLCOLOURS()
      CALL KTEXT(TEX,1,2,15)
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)

      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            KEY = 3
            RETURN
         ELSE
            CALL CHOICES(MODE,NUM,NWHAT,KEY)
         ENDIF
      ELSE IF (KEY .EQ. 21) THEN
!        INS KEY
         IF (NPUT .EQ. 33) THEN
            CALL GETCOLORNUMBER(XP,YP,NUMCOLC,N1C,N2C,N3C)
!           WRITE(TEX2,'(I4)') NUMCOLC
!           CALL QNMESSAGE('COLOUR NR 1 = '//TEX2)
            NPUT = 34
         ELSE IF (NPUT .EQ. 34) THEN
            CALL GETCOLORNUMBER(XP,YP,NUMCOL,N1,N2,N3)
            WRITE(TEX2,'(I4)') NUMCOL
!           CALL QNMESSAGE('IS CHANGED TO THE COLOUR OF NR : '//TEX2)
            CALL IGRPALETTERGB(NUMCOLC,N1,N2,N3)
            NPUT = 33
         ENDIF
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY
         CALL CHANGECOLOR(XP,YP)
      ELSE IF (KEY .EQ. 23) THEN
!        ESC
         CALL IGRPALETTERGB(NUMCOLC,N1C,N2C,N3C)
      ELSE IF (KEY .EQ. 98) THEN
!        b RINGS BELL
         CALL KTEXT('B RINGS BELL',2,6,11)
         CALL OKAY(0)
      ENDIF
!
      GOTO 10
!
      END

      SUBROUTINE GETCOLORNUMBER(XP,YP,NUMCOL,N1O,N2O,N3O)
      implicit none
      integer :: i
      integer :: n1
      integer :: n1o
      integer :: n2
      integer :: n2o
      integer :: n3
      integer :: n3o
      integer :: numcol
      double precision :: xp
      double precision :: yp
      CALL IGRGETPIXELRGB(real(XP),real(YP),N1O,N2O,N3O)
      DO 10 I = 0,255
         CALL SETCOL(I)
         CALL PTABS(XP,YP)
         CALL IGRGETPIXELRGB(real(XP),real(YP),N1,N2,N3)
         IF (N1 .EQ. N1O .AND. N2 .EQ. N2O .AND. N3 .EQ. N3O) THEN
            NUMCOL = I
            CALL DISVALCOLORS(NUMCOL,N1,N2,N3,1)
            RETURN
         ENDIF
   10 CONTINUE
      RETURN
      END

      SUBROUTINE ALLCOLOURS()
      use m_wearelt
      implicit none
      double precision :: dx
      double precision :: dxc
      double precision :: dy
      double precision :: dyc
      integer :: i
      integer :: j
      integer :: ncol
      double precision :: x
      double precision :: xc
      double precision :: xl
      double precision :: xu
      double precision :: y
      double precision :: yc
      double precision :: yl
      double precision :: yu
      NCOL = 0
      XL   = X2-0.66d0*DSIX-RCIR*4
      XU   = XL+0.66d0*DSIX
      YL   = Y1+DSIX
      YU   = Y2-DSIX
      DX   = XU-XL
      DY   = YU-YL
      DXC  = DX/20
      DYC  = DY/20
      DO 10 J = 1,16
         DO 10 I = 1,16
            X  = dble(I-1)/15d0
            Y  = dble(J-1)/15d0
            XC = XL + X*DX
            YC = YL + Y*DY
            CALL SETCOL(NCOL)
            NCOL = NCOL + 1
            CALL FBOX(XC-DXC,YC-DYC,XC+DXC,YC+DYC)
            CALL SETCOL(0)
            CALL  BOX(XC-DXC,YC-DYC,XC+DXC,YC+DYC)
   10 CONTINUE
      RETURN
      END

      subroutine TEKTXT ()
      use m_wearelt
      implicit none
      integer :: ia
      integer, save :: ini = 0
      integer :: k
      integer :: maxtxt
      integer :: ntxt
!      ------------------------------------------------------------------
!     tekenen van de strings die in een file staan en ingelezen zijn met
!     REATXT
!     ------------------------------------------------------------------
      common /XYTEXT/    xtxt,ytxt,coltxt,symtxt,heitxt,ntxt
      common /TEXTSS/    xytexts
      parameter (maxtxt = 2000)
      double precision :: xtxt(maxtxt), ytxt(maxtxt),heitxt(maxtxt)
      integer    symtxt(maxtxt), coltxt(maxtxt)
      character  xytexts(maxtxt)*120

      if (ini .eq. 0) then
         ntxt = 0
         ini  = 1
      endif
      IF (NTXT .LE. 0) RETURN

!     call IGrSymbSet('calctek.smb')
!     call IGrCharSet('symbols.chr')
!     call IGrCharSize(3.0,3.0)
!     call IGrCharJustify ('C')

      do 10 k = 1,ntxt
         call SETCOL (coltxt(k))

!        call IGRMOVETO    ( xtxt(k),ytxt(k) )
!        call IGRCIRCLEREL ( rcir            )

         call IGrCharJustify ('C')
         call IGrCharSize (real(heitxt(k)),real(heitxt(k)))

         if (symtxt(k) .ne. 0) then
            call IGrSymbOut (real(xtxt(k)),real(ytxt(k)),symtxt(k))
         endif

         call IGrMoveTo    ( real(xtxt(k)+1.1*rcir),real(ytxt(k)) )
         ia = len_trim(xytexts(k))
         call IGrCharJustify ('L')
         call DRAWTEXT   ( real(xtxt(k)+1.1*rcir),real(ytxt(k)),xytexts(k)(1:ia))
   10 continue

      call IGrCharSize (0.5,0.5)

      return
      end
!
      SUBROUTINE GETPOS(X,Y)
      implicit none
      double precision :: x
      double precision :: y
      REAL INFOGRAPHICS
      X = INFOGRAPHICS(1)
      Y = INFOGRAPHICS(2)
      RETURN
      END

      SUBROUTINE SETWOR(XW1,YW1,XW2,YW2)
      use unstruc_opengl
      implicit none
      double precision :: XW1,YW1,XW2,YW2
      IF (XW1 .EQ. XW2 .OR. YW1 .EQ. YW2) THEN
         XW2 = XW1+1
         YW2 = YW1+1
      ENDIF

      IF (InOpenGLRendering) THEN
#ifdef HAVE_OPENGL
        !  CALL fglDisable(GL_DEPTH_TEST) ! no depth
          CALL fglMatrixMode (GL_PROJECTION)
          CALL fglLoadIdentity()
          CALL fglOrtho(XW1,XW2,YW1,YW2,0,1)
          CALL fglMatrixMode (GL_MODELVIEW)
#endif
      else
          CALL IGrUnits(real(XW1),real(YW1),real(XW2),real(YW2))
      endif
      RETURN
      END
!
      SUBROUTINE ASPECT(X1D,Y1D,X2D,Y2D)
      use m_devices
      implicit none
      double precision :: asp
      double precision :: x1d
      double precision :: x2d
      double precision :: y1d
      double precision :: y2d
!     RETURN Y2 AS 1.0 ASPECT RATIO VALUE
      CALL INQASP(ASP)
      Y2D = Y1D + (X2D - X1D)*ASP
      RETURN
      END


!
      SUBROUTINE SETWY(X1,Y1,X2,Y2)
      use unstruc_display, only : rcir, cr, dsix
      use m_sferic
      implicit none
      double precision :: x1, x2, y1, y2
      double precision :: yw, asp, xw, x0, y0
!     SET WORLD COORDINATES WITH Y2 AS 1.0 ASPECT RATIO VALUE
!     AND RETURN Y2
      CALL ASPECT(X1,Y1,X2,Y2)
      IF (JSFERIC .EQ. 1 .and. jsfertek==1) THEN
         call inqasp(asp)
         x0 = 0.5*(x1+x2)
         y0 = 0.5*(y1+y2)
         YW = y2 - y1
         xw = yw/ ( asp*COS( DG2RD*y0) )
         x1 = x0 - 0.5*xw
         x2 = x0 + 0.5*xw
      ENDIF
      CALL SETWOR(X1,Y1,X2,Y2)
      RCIR = CR*(X2 - X1)
      DSIX = (X2 - X1)/6
      CALL XYDISFORMAT()
      RETURN
      END
!
      SUBROUTINE TOPIX(X,Y,NX,NY)
      implicit none
      integer :: nx
      integer :: ny
      double precision :: x
      double precision :: y
!     GIVE SCREEN COORDINATES OF WORLDCOORDINATES
      CALL IGRUNITSTOPIXELS(real(X),real(Y),NX,NY)
      RETURN
      END
!
      SUBROUTINE TOWOR(NX,NY,X,Y)
      implicit none
      integer :: nx
      integer :: ny
      double precision :: x
      double precision :: y
      real :: rx, ry
!     GIVE WORLD COORDINATES OF SCREENCOORDINATES
      CALL IGRUNITSFROMPIXELS(NX,NY,rx, ry)
      X = dble(rx)
      Y = dble(ry)
      RETURN
      END

      SUBROUTINE ONETOPIX(X,Y,NX,NY)
      use m_devices
      implicit none
      integer :: nx
      integer :: ny
      double precision :: x
      double precision :: y
      NX = X*NPX
      NY = Y*NPY
      RETURN
      END
!
      SUBROUTINE MOVABS(X,Y)
      use unstruc_opengl
      implicit none
      double precision :: x,y
      IF (InOpenGLRendering) THEN
        CALL MoveTo(X,Y)
      ELSE
        CALL IGRMOVETO(real(X),real(Y))
      ENDIF
      END

      SUBROUTINE LNABS(X,Y)
      use unstruc_opengl
      implicit none
      double precision :: x,y
      real             :: xx, yy
    
      IF (InOpenGLRendering) THEN
        CALL LineTo(X,Y)
      ELSE
        xx = x ; yy = y 
        CALL IGRLINETO(xx,yy)
      ENDIF
      END

      SUBROUTINE LINEWIDTH(iW)
      use unstruc_opengl
      implicit none
      integer :: iw
      IF (InOpenGLRendering) THEN
        CALL SetLineWidth(iw)
      ELSE
        CALL IGRLINEWIDTH(iw,iw)
      ENDIF
      END SUBROUTINE

      SUBROUTINE cLNABS(X,Y,ncol)
      implicit none
      double precision :: x,y
      integer          :: ncol
      call setcol(ncol)
      CALL LNABS(X,Y)
      END


      SUBROUTINE RECTANGLE(x1,y1,x2,y2)
      use unstruc_opengl
      implicit none
      real x1,y1,x2,y2
      real x(4),y(4)

      IF (InOpenGLRendering) THEN
        x(1) = x1
        x(2) = x2
        x(3) = x2
        x(4) = x1
        y(1) = y1
        y(2) = y1
        y(3) = y2
        y(4) = y2
        CALL PFILLERCORE(x,y,4)
      ELSE
        CALL IGRRECTANGLE(x1,y1,x2,y2)
      ENDIF

      END SUBROUTINE


      SUBROUTINE PTABS(X,Y)
      use unstruc_opengl
      implicit none
      double precision :: x,y
      if (InOpenGLRendering) THEN
          CALL DrawPoint(real(x),real(y))
      ELSE
          CALL IGRPOINT(real(X),real(Y))
      ENDIF
      END

      SUBROUTINE DTEKTRI(X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,NCOL,NCOLR)
      use gridoperations
      implicit none
      integer :: ncol
      integer :: ncolr
      double precision :: zz
      double precision :: XX(3), YY(3)
      DOUBLE PRECISION X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3
      CALL DRIETWEE(X1,Y1,Z1,XX(1),YY(1),ZZ)
      CALL DRIETWEE(X2,Y2,Z2,XX(2),YY(2),ZZ)
      CALL DRIETWEE(X3,Y3,Z3,XX(3),YY(3),ZZ)
      CALL PFILLER(XX,YY,3,NCOL,NCOLR)
      RETURN
      END

      SUBROUTINE DTEKPENTA(X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,X5,Y5,Z5,NCOL,NCOLR)
      use gridoperations
      implicit none
      integer :: ncol
      integer :: ncolr
      double precision :: zz
      double precision :: XX(5), YY(5)
      DOUBLE PRECISION X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,X5,Y5,Z5
      CALL DRIETWEE(X1,Y1,Z1,XX(1),YY(1),ZZ)
      CALL DRIETWEE(X2,Y2,Z2,XX(2),YY(2),ZZ)
      CALL DRIETWEE(X3,Y3,Z3,XX(3),YY(3),ZZ)
      CALL DRIETWEE(X4,Y4,Z4,XX(4),YY(4),ZZ)
      CALL DRIETWEE(X5,Y5,Z5,XX(5),YY(5),ZZ)
      CALL PFILLER(XX,YY,5,NCOL,NCOLR)
      RETURN
      END

      SUBROUTINE DPFILLER(X,Y,Z,N,NCOL,NCOLR)
      use gridoperations
      implicit none
      integer :: k
      integer :: n
      integer :: ncol
      integer :: ncolr
      double precision :: zz
      DOUBLE PRECISION X(N),Y(N),Z(N)
      double precision :: XX(100), YY(100)
      DO K = 1,N
         CALL DRIETWEE(X(K),Y(K),Z(K),XX(K),YY(K),ZZ)
      ENDDO
      CALL PFILLER(XX,YY,N,NCOL,NCOLR)
      RETURN
      END

      SUBROUTINE DMOVABS(XD,YD,ZD)
      use m_oldz
      use gridoperations
      implicit none
      double precision :: x
      double precision :: y
      double precision :: z
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      CALL MOVABS(X,Y)
      OZ = Z
      END

      SUBROUTINE DLNABS(XD,YD,ZD)
      USE m_oldz
      USE m_missing
      use gridoperations
      implicit none
      double precision :: x
      double precision :: y
      double precision :: z
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      !IF (OZ .NE. DMISS .AND. Z .NE. DMISS) THEN
       CALL LNABS(X,Y)
      !ENDIF
      OZ = Z
      END

      SUBROUTINE DPTABS(XD,YD,ZD)
      use gridoperations
      implicit none
      double precision :: x
      double precision :: y
      double precision :: z
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      CALL PTABS(X,Y)
      END
!
      SUBROUTINE LNREL(X,Y)
      implicit none
      double precision :: x
      double precision :: y
      CALL IGRLINETOREL(real(X),real(Y))
      END
!
      SUBROUTINE BOX(XB1,YB1,XB2,YB2)
      implicit none
      double precision :: xb1
      double precision :: xb2
      double precision :: yb1
      double precision :: yb2
      call MOVABS(XB1,YB1)
      call LNABS(XB2,YB1)
      call LNABS(XB2,YB2)
      call LNABS(XB1,YB2)
      call LNABS(XB1,YB1)
      RETURN
      END

      SUBROUTINE FBOX(XB1,YB1,XB2,YB2)
      implicit none
      integer :: ndraw
      double precision :: xb1
      double precision :: xb2
      double precision :: yb1
      double precision :: yb2
      COMMON /DRAWTHIS/  ndraw(50)
      if (ndraw(10) == 0) then
         call RECTANGLE(real(XB1),real(YB1),real(XB2),real(YB2))
      else
         call fboxold(XB1,YB1,XB2,YB2)
      endif
      RETURN
      END

      SUBROUTINE FBOXOLD(XB1,YB1,XB2,YB2)
      implicit none
      integer :: n
      integer :: ncolnow
      double precision :: xb1
      double precision :: xb2
      double precision :: yb1
      double precision :: yb2
      COMMON /COLNOW/ NCOLNOW
      REAL X(4), Y(4)
      N    = 4
      X(1) = real(XB1)
      X(2) = real(XB2)
      X(3) = real(XB2)
      X(4) = real(XB1)
      Y(1) = real(YB1)
      Y(2) = real(YB1)
      Y(3) = real(YB2)
      Y(4) = real(YB2)
      IF (NCOLNOW .GE. 0) CALL PFILLERCORE(X,Y,N)
      RETURN
      END

      SUBROUTINE BOXX(X,Y,NCOL)
      implicit none
      integer :: ncol
      integer :: ncolnow
      double precision :: x
      double precision :: y
      COMMON /COLNOW/ NCOLNOW
      CALL SETCOL(NCOL)
      IF (NCOLNOW .GE. 0) CALL IGrMARKER(real(X),real(Y),3)
      RETURN
      END
!
      SUBROUTINE CLR()
      implicit none
      CALL IWINCLEAR()
      END
!
      SUBROUTINE CLS1()
      use unstruc_display
      implicit none
      integer :: ndraw
      COMMON /DRAWTHIS/  ndraw(50)

      Call IGRAREACLEAR()

      IF (NDRAW(10) .EQ. 2) THEN
         CALL IGRPALETTERGB(  2,NREDP,NGREENP,NBLUEP)
      ELSE
         CALL IGRPALETTERGB(  2,NREDS,NGREENS,NBLUES)
      ENDIF

      CALL SETCOL(2)

      CALL FBOX(X1,Y1,X2,Y2)

      RETURN
      END
!
      SUBROUTINE INQASP(ASP)
      USE M_DEVICES
      implicit none
      double precision :: asp
      double precision :: dx
      double precision :: dy
      integer :: jaxis
      integer :: nunix
      double precision :: xleft
      double precision :: xright
      double precision :: ybot
      double precision :: ytop
      COMMON /SCREENAREA/ XLEFT,YBOT,JAXIS
      YTOP   = MAX(0.95d0,1 - YBOT)
      XRIGHT = MAX(0.90d0,1 - XLEFT)
      DX     = XRIGHT- XLEFT
      DY     = YTOP  - YBOT
      ASP    = ( DY*dble(NPY) ) / ( DX*dble(NPX) )
      RETURN
      END
!
      SUBROUTINE SETXOR(I)
      implicit none
      integer :: i
!
      IF (I .EQ. 1) THEN
         CALL IGRPLOTMODE('E')
      ELSE IF (I .EQ. 0) THEN
         CALL IGRPLOTMODE('N')
      ENDIF
!
      RETURN
      END
!
      SUBROUTINE FRAMES(NCOL)
      USE M_DEVICES
      implicit none
      integer :: ncol
      IF (NOPSYS .Ge. 2) RETURN
      CALL SETCOL(NCOL)
      CALL IGRBORDER()
      RETURN
      END

      SUBROUTINE FRAMES2(NCOL)
      USE M_DEVICES
      implicit none
      integer :: ncol
      CALL SETCOL(NCOL)
      CALL IGRBORDER()
      RETURN
      END
!
      SUBROUTINE DISPF(Y,N,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
!     LAAT EENDIMENSIONALE FUNCTIE ZIEN
      double precision :: Y(N)
      CALL SETCOL(NCOL)
      CALL MOVABS(0d0,Y(1))
      DO 10 I = 1,N
         CALL LNABS(dble(I),Y(I))
   10 CONTINUE
      RETURN
      END
!
      SUBROUTINE DISPF2(X,Y,N,NMAX,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
      integer :: nmax
!     LAAT EENDIMENSIONALE FUNCTIE ZIEN
      double precision :: X(NMAX), Y(NMAX)
      CALL SETCOL(NCOL)
      CALL MOVABS(X(1),Y(1))
      DO I = 2,N
         CALL LNABS(X(I),Y(I))
      enddo
      RETURN
      END

      SUBROUTINE DISPF2closed(X,Y,N,NMAX,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
      integer :: nmax
!     closed polygon
      double precision :: X(NMAX), Y(NMAX)
      CALL SETCOL(NCOL)
      CALL MOVABS(X(1),Y(1))
      DO I = 2,N
         CALL LNABS(X(I),Y(I))
      enddo
      call lnABS(X(1),Y(1))
      RETURN
      END

      SUBROUTINE DISPF2cir(X,Y,N,Rcx,Rcy,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
      integer :: nmax
!     LAAT EENDIMENSIONALE FUNCTIE ZIEN met cirkels
      double precision :: X(N), Y(N), rcx, rcy
      CALL SETCOL(NCOL)
      CALL MOVABS(X(1),Y(1))
      DO I = 2,N
         CALL LNABS(X(I),Y(I))
      ENDDO 
      CALL MOVABS(X(1),Y(1))
      if (rcx > 0) CALL fbox( x(1)-rcx,y(1)-rcy,x(1)+rcx,y(1)+rcy )     ! CIR(RCIR)
      DO I = 2,N
         if (rcx > 0) CALL fbox( x(i)-rcx,y(i)-rcy,x(i)+rcx,y(i)+rcy )  ! CIR(RCIR)
      ENDDO 

      RETURN
      END


!
      SUBROUTINE DISP2C(X,Y,N,RCIR,NCOL)
      use m_missing
      use gridoperations
      implicit none
      integer          :: n, ncol
      double precision :: X(N), Y(N), rcir

      integer          :: i, istart, key, in
!     LAAT EEN TWEEDIMENSIONALE FUNCTIE ZIEN MET CIRKELS

      IF (N .LE. 0) RETURN
      CALL SETCOL(NCOL)

      CALL JGRLINE8(x,y,N)

      if (rcir == 0) return

      IF ( NCOL.NE.0 ) THEN

         in = 0
         DO I = 1,N
            if ( INVIEW(X(i),Y(i)) ) then
               CALL MOVABS(X(I),Y(I))
               CALL CIR(RCIR)
               in = in + 1
               if (in > 5000) exit
            endif
         enddo

         CALL SETCOL(31)
         ISTART = 0
         DO I = 1,N
            IF (X(I) .NE. dmiss) THEN
               IF (ISTART .EQ. 1) THEN
               ELSE
                  CALL MOVABS(X(I),Y(I))
                  CALL CIR(RCIR)
                  ISTART = 1
               ENDIF
            ELSE
               ISTART = 0
            ENDIF
         END DO

      END IF

      RETURN
      END

      SUBROUTINE DISP3C(X,Y,Z,NCL,N,RCIR,NCOL)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: istart
      integer :: key
      integer :: n
      integer :: ncol
      double precision :: rcir
!     LAAT EEN TWEEDIMENSIONALE FUNCTIE ZIEN MET CIRKELS EN KLEUREN
      DOUBLE PRECISION X(N), Y(N), Z(N)
      INTEGER NCL(N), ja, jacol


      IF (N .LE. 0) RETURN
      CALL SETCOL(NCOL)

      jacol = 0
      do i = 1,n
         if (ncl(i) .ne. 0) then
            jacol = 1
            exit
         endif
      enddo

      if (jacol == 0) then
         CALL JGRLINE8(x,y,N)
      else

         ISTART = 0
         ja  = 0
         DO I = 1,N
            IF (X(I) .NE. dmiss) THEN
               IF (ISTART .EQ. 1) THEN
                  CALL DLNABS(X(I),Y(I),Z(I))
               ELSE
                  IF (NCL(I) .NE. 0) THEN 
                      CALL SETCOL(NCL(I))
                  ENDIF 
                  CALL DMOVABS(X(I),Y(I),Z(I))
                  ISTART = 1
               ENDIF
               CALL CIR(RCIR)
            ELSE
               ISTART = 0
            ENDIF
            IF (MOD(I,50) .EQ. 0) THEN
                CALL HALT2(ja)
                IF (ja .EQ. 1) RETURN
            ENDIF
         enddo

      endif

      RETURN
      END

      SUBROUTINE DISP3CAB(X,Y,Z,NCL,N,RCIR,NCOL,A,B)
      USE M_MISSING
      implicit none
      double precision :: a
      double precision :: b
      integer :: i
      integer :: istart
      integer :: key
      integer :: n
      integer :: ncol
      double precision :: rcir
!     LAAT EEN TWEEDIMENSIONALE FUNCTIE ZIEN MET CIRKELS EN KLEUREN
      DOUBLE PRECISION X(N), Y(N), Z(N)
      INTEGER NCL(N)

      IF (N .LE. 0) RETURN
      CALL SETCOL(NCOL)
      ISTART = 0
      DO 10 I = 1,N
         IF (X(I) .NE. dmiss) THEN
            IF (ISTART .EQ. 1) THEN
               CALL DLNABS(A*X(I)+B,Y(I),Z(I))
            ELSE
               IF (NCL(I) .NE. 0) CALL SETCOL(NCL(I))
               CALL DMOVABS(A*X(I)+B,Y(I),Z(I))
               ISTART = 1
            ENDIF
            CALL CIR(RCIR)
         ELSE
            ISTART = 0
         ENDIF
         IF (MOD(I,50) .EQ. 0) THEN
             CALL HALT2(KEY)
             IF (KEY .EQ. 1) RETURN
         ENDIF
   10 CONTINUE
      RETURN
      END

      SUBROUTINE DISP4C(X,Y,N)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: istart
      integer :: key
      integer :: n
!     LAAT EEN TWEEDIMENSIONALE FUNCTIE ZIEN MET CIRKELS
      double precision :: X(N), Y(N)

      IF (N .LE. 0) RETURN
      ISTART = 0
      DO 10 I = 1,N
         IF (X(I) .NE. dmiss) THEN
            IF (ISTART .EQ. 1) THEN
               CALL LNABS(X(I),Y(I))
            ELSE
               CALL MOVABS(X(I),Y(I))
               ISTART = 1
            ENDIF
            CALL RCIRC(X(I),Y(I))
         ELSE
            ISTART = 0
         ENDIF
         IF (MOD(I,50) .EQ. 0) THEN
             CALL HALT2(KEY)
             IF (KEY .EQ. 1) RETURN
         ENDIF
   10 CONTINUE
      RETURN
      END
!
      SUBROUTINE DISPFP(X,Y,N,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
!     LAAT EEN EENDIMENSIONALE FUNCTIE ZIEN MET PUNTJES
      double precision :: X(N), Y(N)
      CALL SETCOL(NCOL)
      DO 10 I = 1,N
         CALL MOVABS(X(I),Y(I))
         CALL CIR(0d0)
   10 CONTINUE
      RETURN
      END
!
      SUBROUTINE DISP2P(X,Y,MMAX,MC,NC,NCOL)
      implicit none
      integer :: i
      integer :: j
      integer :: mc
      integer :: mmax
      integer :: nc
      integer :: ncol
!     LAAT EEN TWEEDIMENSIONALE FUNCTIE ZIEN MET PUNTJES
      double precision :: X(MMAX,MMAX), Y(MMAX,MMAX)
      CALL SETCOL(NCOL)
      DO 10 I = 1,MC
         DO 10 J = 1,NC
            IF (X(I,J) .NE. 0) THEN
               CALL MOVABS(X(I,J),Y(I,J))
               CALL CIR(0d0)
            ENDIF
   10 CONTINUE
      RETURN
      END

      SUBROUTINE DISPXP(X,Y,N,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
      double precision :: y
!     LAAT TWEEDIMENSIONALE FUNCTIE PUNTJES ZIEN
      double precision :: X(N)
      CALL SETCOL(NCOL)
      DO 10 I = 1,N
         CALL MOVABS(X(I),Y)
         CALL CIR(0d0)
   10 CONTINUE
      RETURN
      END


      SUBROUTINE DISPF1(Y,DX,N,NCOL)
      implicit none
      double precision :: dx
      integer :: i
      integer :: n
      integer :: ncol
      double precision :: x
!     LAAT EENDIMENSIONALE FUNCTIE ZIEN MET INTERVAL
      double precision :: Y(N)
      CALL SETCOL(NCOL)
      X = 0
      CALL MOVABS(X,Y(1))
      DO 10 I = 2,N
         X = X + DX
         CALL LNABS(X,Y(I))
   10 CONTINUE
      RETURN
      END

      !> Draw a highlighted circle at current position.
      !! Highlighted means: blank center, coloured outline.
      subroutine HLCIR(R, icol)
      implicit none
        double precision, intent(in) :: R    !< Radius in world coords.
        integer,          intent(in) :: icol !< Colour number

        call HLCIR2(R, 0, icol)
      end subroutine HLCIR

      !> Draw a filled circle at current position.
      !! Filled means: one colour for inside, one colour for edge.
      subroutine HLCIR2(R, icolfill, icoledge)
      implicit none
        double precision, intent(in) :: R    !< Radius in world coords.
        integer,          intent(in) :: icolfill !< Colour number for inner fill
        integer,          intent(in) :: icoledge !< Colour number for edge

        CALL IGRFILLPATTERN(4,0,0)
        CALL SETCOL(icolfill)
        CALL CIR(R)
        CALL IGRFILLPATTERN(0,0,0)
        CALL SETCOL(icoledge)
        CALL CIR(R)
        CALL IGRFILLPATTERN(4,0,0)
      end subroutine HLCIR2

      SUBROUTINE CIR(R)
      use unstruc_opengl
      implicit none
      integer :: ncolnow
      double precision :: r, Hr
      COMMON /COLNOW/ NCOLNOW

      if (r == 0d0) return
      IF (InOpenGLRendering) THEN
        HR = 0.5d0*R 
        CALL KREC5(dble(Xlast),dble(Ylast),HR,HR)
        !CALL SetPointSize(real(5))
        !CALL DrawPoint(xlast,ylast)
        !CALL SetPointSize(real(1))
      ELSE
         CALL IGrCircleRel(real(R))
      ENDIF
   END

   SUBROUTINE CIRasp(R) 
   use unstruc_opengl
   implicit none
   double precision :: R

   if (R == 0d0) return
   IF (InOpenGLRendering) THEN
      CALL SetPointSize(real(R))
      CALL DrawPoint(xlast,ylast)
      CALL SetPointSize(real(1))
   ELSE
      CALL IGrCircleRel(real(R))
   ENDIF
   END
   
      SUBROUTINE KCIR(X,Y,Z)
      use unstruc_colors
      USE M_MISSING
      use m_wearelt
      implicit none
      integer :: ncol
      double precision :: x
      double precision :: y
      double precision :: z

      IF (Z .NE. dmiss) THEN
         CALL ISOCOL(Z,NCOL)
         CALL MOVABS(X,Y)
         CALL CIR(RCIR)
      ELSE
         CALL SETCOL(ncolhl)
         CALL MOVABS(X,Y)
         CALL CIR(RCIR)
      ENDIF
      RETURN
      END

      SUBROUTINE DKCIR(XD,YD,ZD,V)
      use gridoperations
      implicit none
      double precision :: v
      double precision :: x
      double precision :: y
      double precision :: z
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      CALL KCIR(X,Y,V)
      RETURN
      END

      SUBROUTINE RCIRC(X,Y)
      use m_wearelt
      implicit none
      double precision :: x
      double precision :: y
      CALL MOVABS(X,Y)
      CALL CIR(RCIR)
      RETURN
      END

      subroutine plotCross(x, y)
      use m_wearelt
      implicit none
      double precision :: x
      double precision :: y

      CALL MOVABS(X-.5*RCIR,Y-.5*RCIR)
      CALL LNABS(X+.5*RCIR, Y+.5*RCIR)
      CALL MOVABS(X-.5*RCIR,Y+.5*RCIR)
      CALL LNABS(X+.5*RCIR, Y-.5*RCIR)
      RETURN
      END

      subroutine plotDiamond(x, y)
      use m_wearelt
      implicit none
      double precision :: x
      double precision :: y

      CALL MOVABS(X+.5*RCIR,Y)
      CALL LNABS(X, Y+.5*RCIR)
      CALL LNABS(X-.5*RCIR,Y)
      CALL LNABS(X, Y-.5*RCIR)
      CALL LNABS(X+.5*RCIR,Y)

      RETURN
      END

      SUBROUTINE DRCIRC(XD,YD,ZD)
      use gridoperations
      implicit none
      double precision :: x
      double precision :: y
      double precision :: z
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      CALL RCIRC(X,Y)
      RETURN
      END

      SUBROUTINE KREC(X,Y,Z,XD)
      implicit none
      integer :: ncol
      integer :: ncolnow
      double precision :: x
      double precision :: xd
      double precision :: y
      double precision :: z
      COMMON /COLNOW/ NCOLNOW
      CALL ISOCOL(Z,NCOL)
      IF (NCOLNOW .GE. 0) call RECTANGLE(real(X-XD),real(Y-XD),real(X+XD),real(Y+XD))
      RETURN
      END

      SUBROUTINE CIRR(X,Y,NCOL)
      use m_wearelt
      implicit none
      integer :: ncol
      double precision :: x
      double precision :: y
      CALL SETCOL(NCOL)
      CALL MOVABS(X,Y)
      CALL CIR(RCIR)
      RETURN
      END

      SUBROUTINE CIRR2(X,Y,NCOL,R)
      use m_wearelt
      implicit none
      integer :: ncol
      double precision :: x,y,r
      CALL SETCOL(NCOL)
      CALL MOVABS(X,Y)
      CALL CIR(RCIR*R)
      RETURN
      END
   
      SUBROUTINE DCIRR(XD,YD,ZD,NCOL)
      use gridoperations
      implicit none
      integer :: ncol
      double precision :: x
      double precision :: y
      double precision :: z
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      CALL CIRR(X,Y,NCOL)
      RETURN
    END

    SUBROUTINE PFILLER(X,Y,N_,NCOL,NCLR)
    use unstruc_opengl
    implicit none
    integer :: N_
    integer :: nclr
    integer :: ncol
    integer :: ncolnow
    integer :: ndraw
    double precision :: X(N_), Y(N_)
    COMMON /DRAWTHIS/ ndraw(50)
    COMMON /COLNOW/ NCOLNOW
    
    integer :: N
    
    integer, parameter :: NMAX = 128
    real xr(NMAX), yr(NMAX)

    CALL SETCOL(NCOL)
    
!   safety
    N = min(N_, NMAX)

    xr(1:N) = x(1:N)
    yr(1:N) = y(1:N)

    CALL PFILLERCORE(xr,yr,N)

    IF (.NOT. InOpenGLRendering .AND. (NCLR .NE. NCOL .or. ndraw(10) .ne. 0)) then
        CALL realPolygon(Xr,Yr,N,NCLR)
    ENDIF

    RETURN
   END
   
    SUBROUTINE PFILLERconcave(X,Y,N_,NCOL,NCLR)
    use unstruc_opengl
    implicit none
    integer :: N_
    integer :: nclr
    integer :: ncol
    integer :: ncolnow
    integer :: ndraw
    double precision :: X(N_), Y(N_)
    COMMON /DRAWTHIS/ ndraw(50)
    COMMON /COLNOW/ NCOLNOW
    
    integer :: N
    
    integer, parameter :: NMAX = 128
    real xr(NMAX), yr(NMAX)

    CALL SETCOL(NCOL)
    
!   safety
    N = min(N_, NMAX)

    xr(1:N) = x(1:N)
    yr(1:N) = y(1:N)

    CALL IGrPolygoncomplex(Xr,Yr,N) 

    IF (.NOT. InOpenGLRendering .AND. (NCLR .NE. NCOL .or. ndraw(10) .ne. 0)) then
        CALL realPolygon(Xr,Yr,N,NCLR)
    ENDIF

    RETURN
    END


    SUBROUTINE PFILLERCORE(XR,YR,N)
    use unstruc_opengl
    implicit none
    integer :: n
    real xr(N), yr(N)

    IF (InOpenGLRendering) THEN
        CALL FillPolygon(xr,yr,n)
    ELSE
        if (n .le. 4) then
            call igrpolygonsimple(xr,yr,n)
        else
            CALL IGrPolygoncomplex(Xr,Yr,N)
        endif
    ENDIF

    END SUBROUTINE


    SUBROUTINE POLYLINE(XR,YR,N)
    use unstruc_opengl
    implicit none
    integer :: n, I
    real xr(N), yr(N)

    IF (InOpenGLRendering) THEN
      CALL MOVABS(dble(XR(1)),dble(YR(1)))
      DO 10 I = 2,N
         call LNABS(dble(XR(I)),dble(YR(I)))
      10 CONTINUE
    ELSE
        CALL IGRPOLYLINE(XR,YR,N)
    ENDIF

    END SUBROUTINE


    SUBROUTINE POLYGON(X,Y,N,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
      integer :: ncolnow
      double precision :: X(N), Y(N)
      COMMON /COLNOW/ NCOLNOW
      CALL SETCOL(NCOL)
      call PTABS(X(1),Y(1))
      DO 10 I = 2,N
         call LNABS(X(I),Y(I))
   10 CONTINUE
      call LNABS(X(1),Y(1))
      RETURN
    END

      SUBROUTINE realPOLYGON(X,Y,N,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
      integer :: ncolnow
      real    :: X(N), Y(N)
      COMMON /COLNOW/ NCOLNOW
      CALL SETCOL(NCOL)
      call PTABS(dble(X(1)),dble(Y(1)))
      DO 10 I = 2,N
         call LNABS(dble(X(I)),dble(Y(I)))
   10 CONTINUE
      call LNABS(dble(X(1)),dble(Y(1)))
      RETURN
      END

      LOGICAL FUNCTION INVNOD(K)
      use m_netw
      use gridoperations
      implicit none
      integer :: k
      INVNOD = INVIEW( XK(K), YK(K) )
      RETURN
      END

      LOGICAL FUNCTION INVLIN(L)
      use m_netw
      use gridoperations
      implicit none
      integer :: k1
      integer :: k2
      integer :: l
      K1 = KN(1,L)
      K2 = KN(2,L)
	  INVLIN = INVIEW( XK(K1), YK(K1) ) .OR. INVIEW( XK(K2), YK(K2) )
      RETURN
      END


      SUBROUTINE ISOFIL(X,Y,Z,n4,NCOLR)
      implicit none
      integer          :: n4, ncolr
      double precision :: X(n4), Y(n4), Z(n4)

      double precision :: dv, dzn, frac
      integer :: i, ih, j, j1, j2, jaauto
      integer :: ncol
      integer :: ncols
      integer :: ndraw
      integer :: nie
      integer :: nis
      integer :: npics
      integer :: num
      integer :: nv
      integer :: nx1
      integer :: nx3
      integer :: ny1
      integer :: ny3
      double precision :: val
      double precision :: vmax
      double precision :: vmin
      double precision :: zmax
      double precision :: zmin
      double precision :: znex
      double precision :: znow
      double precision :: DX(12),DY(12), DZ(12), XH(12),YH(12)
      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
      COMMON /DRAWTHIS/ ndraw(50)

      DO 10 I = 1,n4
         J = I + 1
         IF (I .EQ. n4) J = 1
         DX(I) = X(J) - X(I)
         DY(I) = Y(J) - Y(I)
         DZ(I) = Z(J) - Z(I)
   10 CONTINUE

      ZMAX = Z(1)
      ZMIN = Z(1)
      DO 15 I = 2,n4
         ZMAX = MAX(ZMAX,Z(I))
         ZMIN = MIN(ZMIN,Z(I))
   15 CONTINUE

      IF (ZMAX .LE. VAL(1)) THEN
         NCOL = NCOLS(1)
         CALL PFILLER(X,Y,n4,NCOL,NCOL)
      ELSE IF (ZMIN .GE. VAL(NV)) THEN
         NCOL = NCOLS(NV+1)
         CALL PFILLER(X,Y,n4,NCOL,NCOL)
      ELSE
       DO 20 I = 0,NV
         IF (I .EQ. 0) THEN
            ZNOW = -1E+30
         ELSE
            ZNOW = VAL(I)
         ENDIF
         IF (I .EQ. NV) THEN
            ZNEX = 1E+30
         ELSE
            ZNEX = VAL(I+1)
         ENDIF
         NCOL = NCOLS(I + 1)
         IF (ZMIN .LE. ZNOW .AND. ZMAX .GE. ZNOW .OR.        &
             ZMIN .LE. ZNEX .AND. ZMAX .GE. ZNEX    ) THEN
            IH    = 1
            DO 30 J1 = 1,n4
               J2   = J1 + 1
               IF (J1 .EQ. n4) J2 = 1
               IF (Z(J1) .LT. ZNOW) THEN
                  IF (Z(J2) .GT. ZNOW) THEN
                     DZN  = ZNOW - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
                  IF (Z(J2) .GT. ZNEX) THEN
                     DZN  = ZNEX - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
               ELSE IF (Z(J1) .GT. ZNEX) THEN
                  IF (Z(J2) .LT. ZNEX) THEN
                     DZN  = ZNEX - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
                  IF (Z(J2) .LT. ZNOW) THEN
                     DZN  = ZNOW - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
               ELSE
                  XH(IH) = X(J1)
                  YH(IH) = Y(J1)
                  IH     = IH + 1
                  IF (Z(J2) .LT. ZNOW) THEN
                     DZN  = ZNOW - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ELSE IF (Z(J2) .GT. ZNEX) THEN
                     DZN  = ZNEX - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
               ENDIF
   30       CONTINUE

            NUM = IH - 1
            IF (NUM .GE. 3) THEN
               CALL PFILLER(XH,YH,NUM,NCOL,NCOL)
            ELSE IF (NUM .NE. 0) THEN
!              CALL OKAY(1)
            ENDIF
         ELSE IF (ZMIN .GE. ZNOW .AND. ZMAX .LE. ZNEX) THEN
            CALL PFILLER(X,Y,n4,NCOL,NCOL)
         ENDIF
   20  CONTINUE
      ENDIF

      IF (NDRAW(2) == -1 ) then  ! .GE. 1) THEN ! vintage
         CALL TOPIX(X(1),Y(1),NX1,NY1)
         CALL TOPIX(X(3),Y(3),NX3,NY3)
         NPICS = ABS(NX1-NX3) + ABS(NY1-NY3)
         IF (NCOLR .EQ. 0) THEN
            IF (NPICS .GE. 5) THEN
               CALL SETCOL(NCOLR)
               CALL PTABS(X(1),Y(1))
            ENDIF
         ELSE
            IF (NPICS .GE. 5) THEN
               NUM   = n4
               CALL POLYGON(X,Y,NUM,NCOLR)
            ELSE
               CALL SETCOL(NCOLR)
               CALL PTABS(X(1),Y(1))
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END subroutine isofil

      SUBROUTINE ISOFILb(X,Y,Z,n4,NCOLR) ! as isofil, now for depmax2
      implicit none
      integer          :: n4, ncolr
      double precision :: X(n4), Y(n4), Z(n4)

      double precision :: dv, dzn, frac
      integer :: i, ih, j, j1, j2, jaauto
      integer :: ncol
      integer :: ncols
      integer :: ndraw
      integer :: nie
      integer :: nis
      integer :: npics
      integer :: num
      integer :: nv
      integer :: nx1
      integer :: nx3
      integer :: ny1
      integer :: ny3
      double precision :: val
      double precision :: vmax
      double precision :: vmin
      double precision :: zmax
      double precision :: zmin
      double precision :: znex
      double precision :: znow
      double precision :: DX(10),DY(10), DZ(10), XH(10),YH(10)
      COMMON /DEPMAX2/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
      COMMON /DRAWTHIS/ ndraw(50)


      DO 10 I = 1,n4
         J = I + 1
         IF (I .EQ. n4) J = 1
         DX(I) = X(J) - X(I)
         DY(I) = Y(J) - Y(I)
         DZ(I) = Z(J) - Z(I)
   10 CONTINUE

      ZMAX = Z(1)
      ZMIN = Z(1)
      DO 15 I = 2,n4
         ZMAX = MAX(ZMAX,Z(I))
         ZMIN = MIN(ZMIN,Z(I))
   15 CONTINUE

      IF (ZMAX .LE. VAL(1)) THEN
         NCOL = NCOLS(1)
         CALL PFILLER(X,Y,n4,NCOL,NCOL)
      ELSE IF (ZMIN .GE. VAL(NV)) THEN
         NCOL = NCOLS(NV+1)
         CALL PFILLER(X,Y,n4,NCOL,NCOL)
      ELSE
       DO 20 I = 0,NV
         IF (I .EQ. 0) THEN
            ZNOW = -1E+30
         ELSE
            ZNOW = VAL(I)
         ENDIF
         IF (I .EQ. NV) THEN
            ZNEX = 1E+30
         ELSE
            ZNEX = VAL(I+1)
         ENDIF
         NCOL = NCOLS(I + 1)
         IF (ZMIN .LE. ZNOW .AND. ZMAX .GE. ZNOW .OR.        &
             ZMIN .LE. ZNEX .AND. ZMAX .GE. ZNEX    ) THEN
            IH    = 1
            DO 30 J1 = 1,n4
               J2   = J1 + 1
               IF (J1 .EQ. n4) J2 = 1
               IF (Z(J1) .LT. ZNOW) THEN
                  IF (Z(J2) .GT. ZNOW) THEN
                     DZN  = ZNOW - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
                  IF (Z(J2) .GT. ZNEX) THEN
                     DZN  = ZNEX - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
               ELSE IF (Z(J1) .GT. ZNEX) THEN
                  IF (Z(J2) .LT. ZNEX) THEN
                     DZN  = ZNEX - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
                  IF (Z(J2) .LT. ZNOW) THEN
                     DZN  = ZNOW - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
               ELSE
                  XH(IH) = X(J1)
                  YH(IH) = Y(J1)
                  IH     = IH + 1
                  IF (Z(J2) .LT. ZNOW) THEN
                     DZN  = ZNOW - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ELSE IF (Z(J2) .GT. ZNEX) THEN
                     DZN  = ZNEX - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
               ENDIF
   30       CONTINUE

            NUM = IH - 1
            IF (NUM .GE. 3) THEN
               CALL PFILLER(XH,YH,NUM,NCOL,NCOL)
            ELSE IF (NUM .NE. 0) THEN
!              CALL OKAY(1)
            ENDIF
         ELSE IF (ZMIN .GE. ZNOW .AND. ZMAX .LE. ZNEX) THEN
            CALL PFILLER(X,Y,n4,NCOL,NCOL)
         ENDIF
   20  CONTINUE
      ENDIF

      IF (NDRAW(2) == -1 ) then  ! .GE. 1) THEN ! vintage
         CALL TOPIX(X(1),Y(1),NX1,NY1)
         CALL TOPIX(X(3),Y(3),NX3,NY3)
         NPICS = ABS(NX1-NX3) + ABS(NY1-NY3)
         IF (NCOLR .EQ. 0) THEN
            IF (NPICS .GE. 5) THEN
               CALL SETCOL(NCOLR)
               CALL PTABS(X(1),Y(1))
            ENDIF
         ELSE
            IF (NPICS .GE. 5) THEN
               NUM   = n4
               CALL POLYGON(X,Y,NUM,NCOLR)
            ELSE
               CALL SETCOL(NCOLR)
               CALL PTABS(X(1),Y(1))
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END subroutine isofilb




      SUBROUTINE ISOFILTRI(X,Y,Z,NCOLR)
      implicit none
      double precision :: dv
      double precision :: dzn
      double precision :: frac
      integer :: i
      integer :: ih
      integer :: j
      integer :: j1
      integer :: j2
      integer :: jaauto
      integer :: ncol
      integer :: ncolr
      integer :: ncols
      integer :: nie
      integer :: nis
      integer :: npics
      integer :: num
      integer :: nv
      integer :: nx1
      integer :: nx3
      integer :: ny1
      integer :: ny3
      double precision :: val
      double precision :: vmax
      double precision :: vmin
      double precision :: zmax
      double precision :: zmin
      double precision :: znex
      double precision :: znow
      double precision :: X(3), Y(3), Z(3), DX(3),DY(3), DZ(3), XH(10),YH(10)
      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

      DO 10 I = 1,3
         J = I + 1
         IF (I .EQ. 3) J = 1
         DX(I) = X(J) - X(I)
         DY(I) = Y(J) - Y(I)
         DZ(I) = Z(J) - Z(I)
   10 CONTINUE

      ZMAX = Z(1)
      ZMIN = Z(1)
      DO 15 I = 2,3
         ZMAX = MAX(ZMAX,Z(I))
         ZMIN = MIN(ZMIN,Z(I))
   15 CONTINUE

      IF (ZMAX .LE. VAL(1)) THEN
         NCOL = NCOLS(1)
         CALL PFILLER(X,Y,3,NCOL,NCOL)
      ELSE IF (ZMIN .GE. VAL(NV)) THEN
         NCOL = NCOLS(NV+1)
         CALL PFILLER(X,Y,3,NCOL,NCOL)
      ELSE
       DO 20 I = 0,NV
         IF (I .EQ. 0) THEN
            ZNOW = -1E+30
         ELSE
            ZNOW = VAL(I)
         ENDIF
         IF (I .EQ. NV) THEN
            ZNEX = 1E+30
         ELSE
            ZNEX = VAL(I+1)
         ENDIF
         NCOL = NCOLS(I + 1)
         IF (ZMIN .LE. ZNOW .AND. ZMAX .GE. ZNOW .OR.         &
             ZMIN .LE. ZNEX .AND. ZMAX .GE. ZNEX    ) THEN
            IH    = 1
            DO 30 J1 = 1,3
               J2   = J1 + 1
               IF (J1 .EQ. 3) J2 = 1
               IF (Z(J1) .LT. ZNOW) THEN
                  IF (Z(J2) .GT. ZNOW) THEN
                     DZN  = ZNOW - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
                  IF (Z(J2) .GT. ZNEX) THEN
                     DZN  = ZNEX - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
               ELSE IF (Z(J1) .GT. ZNEX) THEN
                  IF (Z(J2) .LT. ZNEX) THEN
                     DZN  = ZNEX - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
                  IF (Z(J2) .LT. ZNOW) THEN
                     DZN  = ZNOW - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
               ELSE
                  XH(IH) = X(J1)
                  YH(IH) = Y(J1)
                  IH     = IH + 1
                  IF (Z(J2) .LT. ZNOW) THEN
                     DZN  = ZNOW - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ELSE IF (Z(J2) .GT. ZNEX) THEN
                     DZN  = ZNEX - Z(J1)
                     FRAC = DZN/DZ(J1)
                     IF (FRAC .GT. 0d0 .AND. FRAC .LE. 1d0) THEN
                        XH(IH) = X(J1) + FRAC*DX(J1)
                        YH(IH) = Y(J1) + FRAC*DY(J1)
                        IH     = IH + 1
                     ENDIF
                  ENDIF
               ENDIF
   30       CONTINUE

            NUM = IH - 1
            IF (NUM .GE. 3) THEN
               CALL PFILLER(XH,YH,NUM,NCOL,NCOL)
            ELSE IF (NUM .NE. 0) THEN
!              CALL OKAY(1)
            ENDIF
         ELSE IF (ZMIN .GE. ZNOW .AND. ZMAX .LE. ZNEX) THEN
            CALL PFILLER(X,Y,3,NCOL,NCOL)
         ENDIF
   20  CONTINUE
      ENDIF

      CALL TOPIX(X(1),Y(1),NX1,NY1)
      CALL TOPIX(X(3),Y(3),NX3,NY3)
      NPICS = ABS(NX1-NX3) + ABS(NY1-NY3)
      IF (NCOLR .EQ. 0) THEN
         IF (NPICS .GE. 5) THEN
            CALL SETCOL(NCOLR)
            CALL PTABS(X(1),Y(1))
         ENDIF
      ELSE
         IF (NPICS .GE. 5) THEN
            NUM   = 3
            CALL POLYGON(X,Y,NUM,NCOLR)
         ELSE
            CALL SETCOL(NCOLR)
            CALL PTABS(X(1),Y(1))
         ENDIF
      ENDIF
      RETURN
      END


      SUBROUTINE FILLUP(TEXT,CHAR,LEN)
      implicit none
      integer :: i, len
      CHARACTER TEXT*(*), CHAR*1
      DO 10 I = 1,LEN
         WRITE(TEXT(I:I),'(A)') CHAR
   10 CONTINUE
      RETURN
      END

      !> Plot for hardcopy needs to be called twice: one to open hardcopy
      !! driver (file), then perform actual plotting, and second call to
      !! plot() closes the driver/file again. Steered by nopen argument.
      !!     Normal snapshot sequence: nchdev .le. 12: 1 open , 2 close,    0 neutral
      !!     Interactive screendump  : nchdev .ge. 13: 1 dump ,-1 nothing , 0 neutral
      SUBROUTINE PLOT(NOPEN)
      use string_module
      use unstruc_colors
      use unstruc_display
      use unstruc_messages
      use unstruc_model,  only: md_ident, md_snapshotdir, md_snapshot_seqnr
      use unstruc_opengl, only: jaopengl
      implicit none
      integer :: i
      integer :: ihcopts
      integer :: l
      integer :: nhcdev
      integer :: nopen, mout
      integer :: numhcopts
      integer, external :: numuni
      CHARACTER PLOTJE*255,EXT*4
      COMMON /HARDCOPY/  NHCDEV,NUMHCOPTS,IHCOPTS(2,20)
      COMMON /PLOTFIL/   PLOTJE

      if (Jaopengl == 1)  then
         nhcdev = 14
      endif

!     file vullen: nhcdev .le. 12: 1 open , 2 dicht, 0 neutraal
!     screendump : nhcdev .ge. 13: 1 dump ,-1 niks , 0 neutraal
      IF (NOPEN .EQ. 1) THEN

         mout = numuni()
         open (mout, file = trim(md_ident)//'.x1y1x2')
         write(mout,*) x1,y1, x2
         close(mout)


         IF (NHCDEV .EQ. 1) THEN
            EXT = '.hgl'
         ELSE IF (NHCDEV .EQ. 2) THEN
            EXT = '.ps '
            DO 5 I = 1,NUMHCOPTs
               IF (IHCOPTS(1,I) .EQ. 22) THEN
                  IF (IHCOPTS(2,I) .EQ. 1) EXT = '.eps'
               ENDIF
    5       CONTINUE
         ELSE IF (NHCDEV .EQ. 3) THEN
            EXT = '.acd'
         ELSE IF (NHCDEV .EQ. 4) THEN
            EXT = '.rgh'
         ELSE IF (NHCDEV .EQ. 5) THEN
            EXT = '.tkx'
         ELSE IF (NHCDEV .EQ. 6) THEN
            EXT = '.bmp'
         ELSE IF (NHCDEV .EQ. 7) THEN
            EXT = '.pcx'
         ELSE IF (NHCDEV .EQ. 8) THEN
            EXT = '.dxf'
         ELSE IF (NHCDEV .EQ. 9) THEN
            EXT = '.cgm'
         ELSE IF (NHCDEV .EQ. 10) THEN
            EXT = '.wpm'
         ELSE IF (NHCDEV .EQ. 11) THEN
            EXT = '.wmf'
         ELSE IF (NHCDEV .EQ. 12) THEN
            EXT = '.gl2'
         ELSE IF (NHCDEV .EQ. 13) THEN
            EXT = '.bmp'
         ELSE IF (NHCDEV .EQ. 14) THEN
            EXT = '.pcx'
         ENDIF
         L  = len_trim( PLOTJE )
         IF (L .EQ. 0) THEN
            md_snapshot_seqnr = md_snapshot_seqnr + 1
            L = len_trim(md_snapshotdir)
            if (L > 0) then
                PLOTJE = md_snapshotdir
                L = L+1
                plotje(L:L) = '/'
            end if
            WRITE (PLOTJE(L+1:),'(I6.6,A4)') md_snapshot_seqnr,EXT
         ELSE
            ! Not in use now, but it's possible through common /plotfil/ to specify file name.
            ! md_snapshotdir is not used then...
            WRITE (PLOTJE(L+1:),'(A4)') EXT
         ENDIF

!        SET OPTIONS
         IF (NHCDEV .LE. 12) THEN
            NOPEN = 2
            CALL IGRPALETTERGB(  0,NREDP,NGREENP,NBLUEP)

            CALL IGrHardCopySelect(1,NHCDEV)
            IF (NHCDEV .EQ. 7) CALL IGrHardCopySelect(1,6)
            DO 10 I = 1,NUMHCOPTS
               CALL IGrHardCopyOptions( IHCOPTS(1,I), IHCOPTS(2,I) )
   10       CONTINUE
            IF (NHCDEV .EQ. 7) CALL IGrHardCopyOptions(26,0)
            CALL IGrHardCopy(trim(PLOTJE))
            !WRITE(msgbuf,'(2A)') 'You created plotfile ', trim(PLOTJE) ; call msg_flush()
            CALL IWINOPEN(1,1,20,1)
            CALL IWINOUTCENTRE(1,'creating '//trim(PLOTJE))
         ELSE
            NOPEN = 2

           !  CALL ISCREENSAVEIMAGE(trim(PLOTJE))
           !  CALL IGRSAVEIMAGE(trim(PLOTJE))
           !  PLOTJE = ' '
         ENDIF
      ELSE IF (NOPEN .EQ. 2) THEN
         IF (NHCDEV .LE. 12) THEN
            CALL IWINCLOSE(1)
            CALL IGrHardCopy('S')
            CALL IGRPALETTERGB( 0, NREDS, NGREENS, NBLUES)
            NOPEN = 0
         ELSE
            CALL ISCREENSAVEIMAGE(trim(PLOTJE))
            WRITE(msgbuf,'(2A)') 'You created SCREENDUMP ', trim(PLOTJE)
            call msg_flush()
            NOPEN = 0
         ENDIF
         PLOTJE = ' '
      ELSE IF (NOPEN .EQ. -1) THEN
         NOPEN = 0
         PLOTJE = ' '
      ENDIF
      RETURN
      END

      SUBROUTINE ISOCEL(X,Y,P,NCOLR)
      implicit none
      double precision :: dv
      integer :: i
      integer :: ih
      integer :: ja
      integer :: jaauto
      integer :: ncolr
      integer :: ncols
      integer :: nh
      integer :: nie
      integer :: nis
      integer :: nplus
      integer :: nv
      double precision :: p
      double precision :: p1
      double precision :: p2
      double precision :: val
      double precision :: vmax
      double precision :: vmin
      double precision :: vn
      double precision :: x
      double precision :: x1
      double precision :: x2
      double precision :: xh
      double precision :: xhit
      double precision :: y
      double precision :: y1
      double precision :: y2
      double precision :: yh
      double precision :: yhit
!     TEKENT ALLE NV ISOLIJNEN IN EEN CEL TEKAL-METHODE
      DIMENSION P(4),X(4),Y(4),XH(4),YH(4)
      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

      DO 10 I = 1,NV
         NPLUS = 1
         VN    = VAL(I)
         NH    = 0
         DO 20 IH = 1,4
            IF (IH .EQ. 4) NPLUS = -3
            P1 = P(IH)
            P2 = P(IH + NPLUS)
            X1 = X(IH)
            X2 = X(IH + NPLUS)
            Y1 = Y(IH)
            Y2 = Y(IH + NPLUS)
            CALL HITLIN(P1,P2,X1,Y1,X2,Y2,VN,XHIT,YHIT,JA)
            IF (JA .EQ. 1) THEN
               NH     = NH + 1
               XH(NH) = XHIT
               YH(NH) = YHIT
            ENDIF
   20    CONTINUE
!        IF (NH .GT. 1) CALL DISPF2(XH,YH,NH,4,NCOLS(I+1))
         IF (NH .GT. 1) CALL DISPF2(XH,YH,NH,4,0)
   10 CONTINUE

      IF (NCOLR .NE. 0) CALL DISPF2(X,Y,4,4,NCOLR)

      RETURN
      END

      SUBROUTINE ISOCELTRI(X,Y,P,NCOLR)
      implicit none
      double precision :: dv
      integer :: i
      integer :: ih
      integer :: ja
      integer :: jaauto
      integer :: ncolr
      integer :: ncols
      integer :: nh
      integer :: nie
      integer :: nis
      integer :: nplus
      integer :: nv
      double precision :: p
      double precision :: p1
      double precision :: p2
      double precision :: val
      double precision :: vmax
      double precision :: vmin
      double precision :: vn
      double precision :: x
      double precision :: x1
      double precision :: x2
      double precision :: xh
      double precision :: xhit
      double precision :: y
      double precision :: y1
      double precision :: y2
      double precision :: yh
      double precision :: yhit
!     TEKENT ALLE NV ISOLIJNEN IN EEN CEL TEKAL-METHODE
      DIMENSION P(3),X(3),Y(3),XH(3),YH(3)
      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

      DO 10 I = 1,NV
         NPLUS = 1
         VN    = VAL(I)
         NH    = 0
         DO 20 IH = 1,3
            IF (IH .EQ. 3) NPLUS = -2
            P1 = P(IH)
            P2 = P(IH + NPLUS)
            X1 = X(IH)
            X2 = X(IH + NPLUS)
            Y1 = Y(IH)
            Y2 = Y(IH + NPLUS)
            CALL HITLIN(P1,P2,X1,Y1,X2,Y2,VN,XHIT,YHIT,JA)
            IF (JA .EQ. 1) THEN
               NH     = NH + 1
               XH(NH) = XHIT
               YH(NH) = YHIT
            ENDIF
   20    CONTINUE
!        IF (NH .GT. 1) CALL DISPF2(XH,YH,NH,3,NCOLS(I+1))
         IF (NH .GT. 1) CALL DISPF2(XH,YH,NH,3,0)
   10 CONTINUE

      IF (NCOLR .NE. 0) CALL DISPF2(X,Y,3,3,NCOLR)

      RETURN
      END

      SUBROUTINE HITLIN(P1,P2,X1,Y1,X2,Y2,V,XHIT,YHIT,JA)
      implicit none
      double precision :: dp
      double precision :: dv
      double precision :: dx
      double precision :: dy
      double precision :: frac
      integer :: ja
      double precision :: p1
      double precision :: p2
      double precision :: v
      double precision :: x1
      double precision :: x2
      double precision :: xhit
      double precision :: y1
      double precision :: y2
      double precision :: yhit
!     SNIJDT EEN ISOLIJN EEN LIJNTJE ?
      DX   = X2 - X1
      DY   = Y2 - Y1
      DP   = P2 - P1
      DV   = V  - P1
      IF (DP .NE. 0) THEN
         FRAC = DV/DP
      ELSE IF (V .EQ. P2) THEN
         FRAC = 1d0
      ELSE
         FRAC = 0
      ENDIF
      JA = 0
      IF (0d0 .LT. FRAC .AND. FRAC .LE. 1d0) THEN
         JA   = 1
         XHIT = X1 + FRAC*DX
         YHIT = Y1 + FRAC*DY
      ENDIF
      RETURN
      END


      SUBROUTINE DISPOS()
      use m_devices
      implicit none
      integer :: jashow
      integer :: jav
      integer :: jmouse
      integer :: jview
      double precision :: xa
      double precision :: xlc
      double precision :: xyz
      double precision :: ya
      double precision :: ylc
      COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW
      common /dispfor/ xyform, zform, disform
      character*6      xyform, zform, disform
      CHARACTER POSITI*23

      POSITI = 'X,Y:         ,         '
      IF (JVIEW .EQ. 2) THEN
         POSITI = 'Z,Y:         ,         '
      ELSE IF (JVIEW .EQ. 3) THEN
         POSITI = 'X-Z:         ,         '
      ENDIF

      WRITE(POSITI (5:13),xyform) XLC
      WRITE(POSITI(15:23),xyform) YLC
      CALL KTEXT(POSITI,IWS-22,2,15)

      RETURN
      END

      SUBROUTINE DISPOS2(X,Y)
      USE M_DEVICES
      implicit none
      double precision :: x
      double precision :: y
      common /dispfor/ xyform, zform, disform
      character*6      xyform, zform, disform
      CHARACTER POSITI*23

      POSITI = 'X,Y:         ,         '
      WRITE(POSITI (5:13),xyform) X
      WRITE(POSITI(15:23),xyform) Y
      CALL KTEXT(POSITI,IWS-22,2,15)
      CALL DISDIS()

      RETURN
      END

      SUBROUTINE DISAREAM(AREAM)
      use m_devices
      implicit none
      double precision :: aream
      CHARACTER DISTAN*23
      DISTAN = 'CR. AR. M            M2'
      WRITE(DISTAN (11:20),'(E10.4)') AREAM
      CALL KTEXT(DISTAN,IWS-22,6,15)
      RETURN
      END

      SUBROUTINE DISAREAN(AREAN)
      use m_devices
      implicit none
      double precision :: arean
      CHARACTER DISTAN*23
      DISTAN = 'CR. AR. N            M2'
      WRITE(DISTAN (11:20),'(E10.4)') AREAN
      CALL KTEXT(DISTAN,IWS-22,5,15)
      RETURN
      END

      SUBROUTINE DISDEP2(DEP)
      use m_devices
      implicit none
      double precision :: dep
      CHARACTER DISTAN*23

      DISTAN = 'D2:                    '
      WRITE(DISTAN (5:),'(F8.3)') DEP
      CALL KTEXT(DISTAN,IWS-22,5,15)

      RETURN
      END

      SUBROUTINE DISCOUR(M,N,DEP)
      use m_devices
      implicit none
      double precision :: dep
      integer :: m
      integer :: n
      CHARACTER DISTAN*23

      DISTAN = 'M:    N:    CRT:       '
      WRITE(DISTAN (3:5),'(I3)') M
      WRITE(DISTAN (9:11),'(I3)') N
      WRITE(DISTAN (17:23),'(F7.2)') DEP
      CALL KTEXT(DISTAN,IWS-22,4,15)

      RETURN
      END

      SUBROUTINE DISVAL(M,N,DEP)
      use m_devices
      implicit none
      double precision :: dep
      integer :: m
      integer :: n
      integer :: ndraw
      COMMON /DRAWTHIS/ ndraw(50)
      CHARACTER DISTAN*23
      IF (NDRAW(14) .LE. 1) THEN
         DISTAN = 'M:     N:              '
      ELSE IF (NDRAW(14) .EQ. 2) THEN
         DISTAN = 'M:    N:    ZC:        '
      ELSE IF (NDRAW(14) .EQ. 3) THEN
         DISTAN = 'M:    N:    RES:       '
      ELSE IF (NDRAW(14) .EQ. 4) THEN
         DISTAN = 'M:    N:    MSM:       '
      ELSE IF (NDRAW(14) .EQ. 5) THEN
         DISTAN = 'M:    N:    NSM:       '
      ELSE IF (NDRAW(14) .EQ. 6) THEN
         DISTAN = 'M:    N:    MCU:       '
      ELSE IF (NDRAW(14) .EQ. 7) THEN
         DISTAN = 'M:    N:    NCU:       '
      ELSE IF (NDRAW(14) .EQ. 8) THEN
         DISTAN = 'M:    N:    MSZ:       '
      ELSE IF (NDRAW(14) .EQ. 9) THEN
         DISTAN = 'M:    N:    NSZ:       '
      ELSE IF (NDRAW(14) .EQ.10) THEN
         DISTAN = 'M:    N:    ASP:       '
      ELSE IF (NDRAW(14) .EQ.11) THEN
         DISTAN = 'M:    N:               '
      ELSE IF (NDRAW(14) .EQ.12) THEN
         DISTAN = 'M:    N:    DEP:       '
      ELSE IF (NDRAW(11) .EQ. 1) THEN
         DISTAN = 'M:    N:    CNM:       '
      ELSE IF (NDRAW(11) .EQ. 2) THEN
         DISTAN = 'M:    N:    CRM:       '
      ELSE IF (NDRAW(11) .EQ. 3) THEN
         DISTAN = 'M:    N:    CRN:       '
      ENDIF

      IF (M .EQ. 0) THEN
         DISTAN = 'NO POINT FOUND         '
      ELSE
         WRITE(DISTAN (3:6),'(I4)') M
         WRITE(DISTAN (10:13),'(I4)') N
         IF (NDRAW(14) .GE. 2 .AND. NDRAW(14) .LE. 10) THEN
            WRITE(DISTAN (16:23),'(F8.3)') DEP
         ELSE IF (NDRAW(14) .EQ. 11) THEN
            WRITE(DISTAN (17:23),'(F7.1)') DEP
         ELSE IF (NDRAW(11) .GE. 1 .AND. NDRAW(11) .LE. 3) THEN
            WRITE(DISTAN (17:23),'(F7.2)') DEP
         ENDIF
      ENDIF

      CALL KTEXT(DISTAN,IWS-22,4,15)

      RETURN
  END
  
    SUBROUTINE ISPOIN(      X,      Y,     mmax, nmax, MC,     NC,   RD1, &
                            XL,     YL,     MV,     NV)
      use m_missing
      use m_wearelt
      implicit none
      integer, intent(in)  :: mmax, nmax, mc, nc
      integer, intent(out) :: mv, nv
      double precision ::   X(MMAX,NMAX), Y(MMAX,NMAX), RD1(MMAX,NMAX)
      double precision :: xl, yl


      integer :: m1, n1, m2, n2, ishot, mvol, nvol, i, j

      DATA MVOL /0/, NVOL /0/
      MV    = 0
      NV    = 0
      ISHOT = 0

  666 CONTINUE
      IF (ISHOT .EQ. 0 .AND. MVOL .NE. 0) THEN
         M1    = MAX(1,MVOL - 3)
         N1    = MAX(1,NVOL - 3)
         M2    = MIN(MC,MVOL + 3)
         N2    = MIN(NC,NVOL + 3)
         ISHOT = 1
      ELSE
         M1    = 1
         N1    = 1
         M2    = MC
         N2    = NC
         ISHOT = 0
      ENDIF

      DO 10 I = M1,M2
         DO 10 J = N1,N2
            IF (X(I,J) .NE. XYMIS) THEN
               IF (ABS(XL - X(I,J)) .LT. RCIR) THEN
                  IF (ABS(YL - Y(I,J)) .LT. RCIR) THEN
                     MV   = I
                     NV   = J
                     XL   = X(I,J)
                     YL   = Y(I,J)
                     MVOL = MV
                     NVOL = NV
                     CALL DISVAL(MV,NV,RD1(MV,NV))
                     RETURN
                  ENDIF
               ENDIF
            ENDIF
   10 CONTINUE
      IF (ISHOT .EQ. 1) GOTO 666
      MVOL = 0
      CALL DISVAL(0,0,0d0)
      RETURN
      END subroutine ispoin
  
      SUBROUTINE ORGLOCATOR(XL,YL)
      use m_devices
      implicit none
      integer :: jashow
      integer :: jmouse
      integer :: ml
      integer :: nl
      double precision :: xa
      double precision :: xl
      double precision :: xlc
      double precision :: ya
      double precision :: yl
      double precision :: ylc
!     INITIATE CURSOR LOCATION
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW

      IF (XL .EQ. 0 .AND. YL .EQ. 0) THEN
         ML  = NPX/2
         NL  = NPY/2
         CALL TOWOR(ML,NL,XLC,YLC)
      ELSE
         XLC = XL
         YLC = YL
      ENDIF

      CALL IMOUSECURSORXYG(real(XLC),real(YLC))
      RETURN
      END

      SUBROUTINE WEAREL()
      use m_wearelt
      implicit none
      integer, save :: ini = 0
      X1   = XMIN
      Y1   = YMIN
      X2   = XMAX
      CALL SETWY(X1,Y1,X2,Y2)
      !IF (INI .EQ. 1) THEN
         CALL INILCA()
      !ELSE
      !   INI = 1
      !ENDIF
      RETURN
      END

      SUBROUTINE INILCA()
      implicit none
      integer :: jashow
      integer :: jmouse
      double precision :: xa
      double precision :: xla
      double precision :: xlb
      double precision :: xlc
      double precision :: ya
      double precision :: ylc
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW
      XLA    = 0
      XLB    = 0
!      CALL ORGLOCATOR(XLA,XLB)
      XLA    = 0
      XLB    = 0
      CALL ANCHOR(XLA,XLB)
      RETURN
      END

      SUBROUTINE TEKHOOK(XP,YP)
      use m_sferic
      implicit none
      double precision :: dx
      double precision :: dy
      integer :: jashow
      integer :: jmouse
      double precision :: xa
      double precision :: xlc
      double precision :: xp
      double precision :: ya
      double precision :: ylc
      double precision :: yp
      COMMON /LOCATORA/ XLC,YLC,XA,YA,JMOUSE,JASHOW


      DX = XA - XP
      DY = YA - YP
      CALL MOVABS(XA,YA)
      CALL  LNABS(XP-DX,YP-DY)
      CALL MOVABS(XP+DY,YP-DX)
      CALL  LNABS(XP-DY,YP+DX)
      RETURN
      END

      SUBROUTINE ANCHOR(X,Y)
      use unstruc_colors
      use m_flow, only: nplot
      use m_GlobalParameters, only: INDTP_ALL
      implicit none
      integer :: jashow
      integer :: jmouse
      integer :: ma
      integer :: na
      integer :: k
      double precision :: x
      double precision :: xa
      double precision :: xlc
      double precision :: y
      double precision :: ya
      double precision :: ylc
!     VEEG OUDE CROSS UIT EN ZET NIEUWE
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW

      IF (X .EQ. 0 .AND. Y .EQ. 0) THEN
         MA = 25
         NA = 40
         CALL TOWOR(MA,NA,XA,YA)
      ELSE
         CALL SETXOR(1)
         CALL SETCOL(KLANK)
         CALL IGrMARKER(real(XA),real(YA),2)
         CALL SETXOR(0)
         XA = X
         YA = Y
      ENDIF

      call inflowcell(XA,YA,k,1,INDTP_ALL) ! Use anchor for new nplot point (vertical profile)
      if (k > 0) nplot = k

      CALL SETXOR(1)
      CALL SETCOL(KLANK)
      CALL IGrMARKER(real(XA),real(YA),2)
      CALL SETXOR(0)

      CALL DISDIS()

      RETURN
      END

      SUBROUTINE ANCHORCLS()
      use unstruc_colors
      implicit none
      integer :: jashow
      integer :: jmouse
      double precision :: xa
      double precision :: xlc
      double precision :: ya
      double precision :: ylc
!     ZET ANCHOR NA CLEARSCREEN
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW

      CALL SETXOR(1)
      CALL SETCOL(KLANK)
      CALL IGrMARKER(real(XA),real(YA),2)
      CALL SETXOR(0)

      CALL DISDIS()

      RETURN
      END

      SUBROUTINE GEDULD()
      implicit none
      integer :: i
      integer :: numkey
      DO 10 I = 1,800
         CALL INKEYEVENTIMM(NUMKEY)
         IF (NUMKEY .NE. 0) RETURN
   10 CONTINUE
      RETURN
      END

      SUBROUTINE GEDULD2(JAKNOP)
      implicit none
      integer :: i
      integer :: jaknop
      integer :: numkey
      JAKNOP = 0
      DO 10 I = 1,160000
         CALL INKEYEVENTIMM(NUMKEY)
         IF (NUMKEY .NE. -999 .AND. NUMKEY .NE. 257) THEN
            JAKNOP = 1
            RETURN
         ENDIF
   10 CONTINUE
      RETURN
      END

      SUBROUTINE HALTESC()
      implicit none
      integer :: numkey
      numkey = 0
      do while (numkey .ne. 27)
         CALL INKEYEVENTIMM(NUMKEY)
      enddo
      end

      SUBROUTINE HALT3(JA)
      ! left   mouse button: 1
      ! middle mouse button: 2
      ! right  mouse button: 3
      implicit none
      integer :: ja
      integer :: numkey
!     kappen met muis
      JA = 0
      CALL INKEYEVENTIMM(NUMKEY)
      IF (NUMKEY .GE. 251 .AND. NUMKEY .LE. 253) then
         JA = NUMKEY-251+1
!         call inflush()
      endif
      RETURN
      END

      SUBROUTINE HALT2(JA)
      implicit none
      integer :: ja
      integer :: numkey
!     kappen met muis
      JA = 0
      CALL INKEYEVENTIMM(NUMKEY)
      IF (NUMKEY .GE. 251 .AND. NUMKEY .LE. 253) then
         JA = 1
!         call inflush()
      endif
      RETURN
      END

      SUBROUTINE get_s_key(JA) ! s or left mouse
      implicit none
      integer :: ja
      integer :: numkey
!     kappen met muis
      JA = 0
      CALL INKEYEVENTIMM(NUMKEY)
      IF (NUMKEY == 115 .or. NUMKEY == 115-32 .or. NUMKEY == 251) then
         JA = 1
         call inflush()
      endif
      RETURN
      END

      SUBROUTINE HALT(JA)
      implicit none
      integer, intent(out) :: ja
      integer :: numkey
!     kappen met ALLES
      JA = 0
      CALL INKEYEVENTIMM(NUMKEY)
      IF (NUMKEY .NE. -999 .AND. NUMKEY .NE. 257 .AND. NUMKEY .NE. 254) JA = 1
      RETURN
      END


      SUBROUTINE READLOCATOR(X,Y,KEY)
      use m_wearelt
      use m_devices
      use m_partitioninfo
      implicit none
      double precision :: dpx
      double precision, save :: f = 1d0
      integer :: ini
      integer :: jashow
      integer :: jmouse
      integer :: key, key_all
      integer, save :: keyold = 0
      real :: xloc, yloc
      double precision :: x
      double precision :: xa
      double precision :: xlc
      double precision :: y
      double precision :: ya
      double precision :: ylc
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW

      REAL, external :: INFOGRAPHICS

      DPX   = (X2-X1)/NPX
      CALL IMOUSECURSORSHAPE(1,'G')
      CALL IMouseCursorShow()
      INI = KEY

   10 CONTINUE

      IF (NOPSYS .EQ. 1) THEN
!        CALL InKeyEventIMM(KEY)
         CALL InKeyEvent(KEY)
      ELSE
         if ( jampi.eq.0 ) then
            CALL InKeyEvent(KEY)
         else
            CALL InKeyEventIMM(KEY)
!           reduce key
!            call reduce_key(key)
         end if
      ENDIF

      IF (KEY .EQ. -999) THEN
!        er gebeurt helemaal niets
         GOTO 10
!      ELSE IF (KEY .GE. 128 .AND. KEY .LE. 131) THEN
!        pijltjesbeweging
         IF (KEYOLD .NE. KEY) THEN
            F   = 1
         ENDIF
         KEYOLD = KEY
         F      = F*1.08d0
         F      = MIN(F,10d0)
         IF (KEY .EQ. 128) THEN
            YLC = YLC + DPX*F
         ELSE IF (KEY .EQ. 129) THEN
            YLC = YLC - DPX*F
         ELSE IF (KEY .EQ. 130) THEN
            XLC = XLC + DPX*F
         ELSE IF (KEY .EQ. 131) THEN
            XLC = XLC - DPX*F
         ENDIF
         CALL IMOUSECURSORXYG(real(XLC),real(YLC))
         X = XLC
         Y = YLC
         IF (INI .EQ. 999) THEN
            KEY = -10
            CALL IMOUSECURSORHIDE()
            CALL GIVEKEY(KEY)
            RETURN
         ENDIF
      ENDIF

!     muisbeweging
      Xloc   = InfoGraphics(5)
      Yloc   = InfoGraphics(6)
      X=dble(xloc)
      y=dble(yloc)
      y = min(max(y, y1), y2)

!     buiten veld?
      IF (INI .NE. 999) THEN
         IF (Y .GT. Y1 + 0.98d0*(Y2-Y1) ) THEN
            KEY = 1
            CALL IMOUSECURSORSHAPE(0,'G')
            RETURN
         ELSE IF (Y .LT. Y1 + 0.02d0*(Y2-Y1) ) THEN
            KEY = 2
            CALL IMOUSECURSORSHAPE(0,'G')
            RETURN
         ENDIF
      ENDIF

      XLC = X
      YLC = Y

      IF (INI .EQ. 999) THEN
         IF (KEY .GE. 254 .AND. KEY .LE. 257) THEN
!           zo snel mogelijk lopen, geen keys of display
            KEY = -10
            RETURN
         ELSE
            CALL DISPOS()
            CALL DISDIS()
            CALL GETKEY2(KEY)
            CALL GIVEKEY(KEY)
            CALL IMOUSECURSORHIDE()
            RETURN
         ENDIF
      ELSE
         CALL DISPOS()
         CALL DISDIS()
         IF ( (KEY .GE. 254 .AND. KEY .LE. 257) .OR.      &
              (KEY .GE. 128 .AND. KEY .LE. 131) ) THEN
!        IF (KEY .EQ. 257 .OR. KEY .GE. 128 .AND. KEY .LE. 131) THEN
!           zo snel mogelijk lopen
            GOTO 10
         ELSE
            CALL GETKEY2(KEY)
            CALL GIVEKEY(KEY)
            CALL TIMLIN()
            CALL IMOUSECURSORHIDE()
         ENDIF
      ENDIF
      RETURN
      END


      SUBROUTINE KTEXT(TEXNU,NX,NY,NCOL)
      implicit none
      integer :: ncol
      integer :: nx
      integer :: ny
!     tekst op normale text posities met standaard blauwe achtergrond
      CHARACTER* (*) TEXNU
      CALL ITEXTCOLOURN(NCOL,5)
      CALL IOUTSTRINGXY(NX,NY,trim(TEXNU))
      RETURN
      END

      SUBROUTINE KTEXT2(TEX,NX,NY,NCOL,NCOL2)
      implicit none
      integer :: ncol
      integer :: ncol2
      integer :: nx
      integer :: ny
!     tekst op normale text posities met EIGEN achtergrond
      CHARACTER* (*) TEX
      CALL ITEXTCOLOURN(NCOL,NCOL2)
      CALL IOUTSTRINGXY(NX,NY,trim(TEX))
      RETURN
      END

      SUBROUTINE LTEXT(TEX,NX,NY,NCOL)
      use unstruc_colors
      implicit none
      integer :: ncol
      integer :: ndraw
      integer :: nx
      integer :: ny
      double precision :: x
      double precision :: y
!     grafische tekst op normale text posities
      CHARACTER TEX*(*)
      COMMON /DRAWTHIS/ ndraw(50)
      X = X1 + (X2-X1)*dble(NX)/dble(IWS)
      Y = Y2 + (Y1-Y2)*dble(NY)/dble(IHS)
      IF (NDRAW(10) .EQ. 1) THEN
         CALL SETCOL(1)
      ELSE
         CALL SETCOL(KLTEX)
      ENDIF
      CALL DRAWTEXT(real(X),real(Y),TEX)
      RETURN
      END

      SUBROUTINE ITEXT(TEX,NX,NY)
      use unstruc_colors
      implicit none
      integer :: l
      integer :: nx
      integer :: ny
      double precision :: x
      double precision :: y
!     grafische tekst op normale text posities
      CHARACTER TEX*(*)
      X = X1 + (X2-X1)*dble(NX)/dble(IWS)
      Y = Y2 + (Y1-Y2)*dble(NY)/dble(IHS)
      CALL SETCOL(KLTEX)
      L = len_trim(TEX)
      CALL DRAWTEXT(real(X),real(Y),TEX(1:L))
      RETURN
      END

      SUBROUTINE ICTEXT(TEX,NX,NY,NCOL)
      use unstruc_colors
      implicit none
      integer :: l
      integer :: ncol
      integer :: nx
      integer :: ny
      double precision :: x
      double precision :: y
!     grafische tekst op normale text posities
      CHARACTER TEX*(*)
      X = X1 + (X2-X1)*dble(NX)/dble(IWS)
      Y = Y2 + (Y1-Y2)*dble(NY)/dble(IHS)
      CALL SETCOL(NCOL)
      L = len_trim(TEX)
      CALL DRAWTEXT(real(X),real(Y),TEX(1:L))
      RETURN
      END

      SUBROUTINE DGTEXT(TEX,XD,YD,ZD,NCOL)
      use gridoperations
      implicit none
      integer :: ncol
      double precision :: x
      double precision :: y
      double precision :: z
      CHARACTER TEX*(*)
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      CALL GTEXT(TEX,X,Y,NCOL)
      RETURN
      END

      SUBROUTINE GTEXT(TEX,X,Y,NCOL)
      implicit none
      integer :: ncol
      integer :: ncolnow
      double precision :: x
      double precision :: y
      COMMON /COLNOW/ NCOLNOW
!     grafische text op grafische posities
      CHARACTER TEX*(*)
      CALL SETCOL(NCOL)
      IF (NCOLNOW .GE. 0) THEN
        CALL DRAWTEXT(real(X),real(Y),TEX)
      ENDIF
      RETURN
    END

    SUBROUTINE DRAWTEXT(X,Y,TEX)
    use unstruc_opengl
    implicit none
    real :: x, y
    CHARACTER TEX*(*)

    IF (InOpenGLRendering) THEN
        CALL RenderText(X,Y,TEX)
    ELSE
        CALL IGRCHAROUT(X,Y,TEX)
    ENDIF
    END SUBROUTINE

    SUBROUTINE DHTEXT(VAL,XD,YD,ZD)
      use gridoperations
      implicit none
      double precision :: val
      double precision :: x
      double precision :: y
      double precision :: z
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      CALL HTEXT(VAL,X,Y)
      RETURN
      END


      SUBROUTINE HTEXT(VAL,X,Y)
      implicit none
      integer :: ncolnow
      double precision :: val
      double precision :: x
      double precision :: y
!     getal value op grafisch scherm in current color
      CHARACTER TEXT*6, TEXT2*10
      COMMON /COLNOW/ NCOLNOW
      IF (NCOLNOW .GE. 0) THEN
         IF (-1.000d0 .LT. VAL .AND. VAL .LT. 10.000d0) THEN
            WRITE(TEXT(1:6),'(F6.3)') VAL
            CALL DRAWTEXT(real(X),real(Y), TEXT)
         ELSE IF (-10.000d0 .LT. VAL .AND. VAL .LT. 100.000d0) THEN
            WRITE(TEXT(1:6),'(F6.2)') VAL
            CALL DRAWTEXT(real(X),real(Y), TEXT)
         ELSE IF (-100.000d0 .LT. VAL .AND. VAL .LT. 1000.000d0) THEN
            WRITE(TEXT(1:6),'(F6.1)') VAL
            CALL DRAWTEXT(real(X),real(Y), TEXT)
         else
            WRITE(TEXT2,'(e10.3)') VAL
            CALL DRAWTEXT(real(X),real(Y), TEXT2)
         ENDIF
      ENDIF

      RETURN
      END

      SUBROUTINE DHITEXT(IVAL,XD,YD,ZD)
      use gridoperations
      implicit none
      integer :: ival
      double precision :: x
      double precision :: y
      double precision :: z
      DOUBLE PRECISION XD,YD,ZD
      CALL DRIETWEE(XD,YD,ZD,X,Y,Z)
      CALL HITEXT(IVAL,X,Y)
      RETURN
      END

      SUBROUTINE HITEXT(IVAL,X,Y)
      implicit none
      integer :: ival
      integer :: l
      integer :: ncolnow
      double precision :: x
      double precision :: y
!     INTEGER grafisch scherm in current color
      CHARACTER TEX*8
      COMMON /COLNOW/ NCOLNOW
      IF (NCOLNOW .GE. 0) THEN
         IF (abs(IVAL) < 100) THEN
            WRITE(TEX,'(I3)') IVAL
         ELSE IF (abs(IVAL) < 10000) THEN
            WRITE(TEX,'(I5)') IVAL
         ELSE
            WRITE(TEX,'(I8)') IVAL
         ENDIF
         L = len_trim(TEX)
         CALL DRAWTEXT(real(X),real(Y), TEX(1:L))
      ENDIF
      RETURN
      END


      SUBROUTINE ZOOM2(KEY)
      use m_wearelt
      implicit none
      double precision :: aspect
      double precision :: dsixn
      double precision :: dxh
      double precision :: dyh
      integer :: jashow
      integer :: jmouse
      integer :: key

      double precision :: x1b
      double precision :: x2b
      double precision :: xa
      double precision :: xl
      double precision :: xlc
      double precision :: xln
      double precision :: y1b
      double precision :: y2b
      double precision :: ya
      double precision :: yl
      double precision :: ylc
      double precision :: yln
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW
!     ALLEEN ENTRY BIJ KEY = 90  (Z BIJ CAPS LOCK ON !)
!     EN NIET ALS NET BEZIG PUNT TE ZETTEN
!     BIJ VERLATEN MET KEY = 3, TEKEN OPNIEUW

      CALL INQASP(ASPECT)
      DXH   = (X2 - X1)
      DYH   = DXH*ASPECT
      DSIXN = DSIX
      XL    = (X1+X2)/2
      YL    = (Y1+Y2)/2
      X1B   = XL - DSIXN
      X2B   = XL + DSIXN
      Y1B   = YL - DSIXN*ASPECT
      Y2B   = YL + DSIXN*ASPECT
      X1    = X1B
      Y1    = Y1B
      X2    = X2B
      CALL SETWY(X1,Y1,X2,Y2)
      XLN   = 0d0
      YLN   = 0d0
      CALL ORGLOCATOR(XLN,YLN)
      KEY   = 3
      RETURN
      END

      SUBROUTINE ZOOM3(KEY,NPUT)
      use m_wearelt
      implicit none
      integer :: jashow
      integer :: jmouse
      integer :: key
      integer :: nput
      double precision :: xa
      double precision :: xlc
      double precision :: ya
      double precision :: ylc
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW
      XLC = (X1+X2)/2
      YLC = (Y1+Y2)/2
      CALL IMOUSECURSORXYG(real(XLC),real(YLC))
      CALL ZOOMIN(KEY,NPUT)
      RETURN
      END

      SUBROUTINE ZOOMIN(KEY,NPUT)
      use unstruc_colors
      implicit none
      double precision :: aspect
      double precision :: dsixn
      double precision :: dxh
      double precision :: dyh
      integer :: ja
      integer :: jadraw
      integer :: jashow
      integer :: jmouse
      integer :: k
      integer :: key
      integer :: maxzoom
      integer :: nlevel
      integer :: nnn
      integer :: nput
      integer, save :: numzoom = 0
      double precision :: x1b
      double precision :: x2b
      double precision :: xa
      double precision :: xl
      double precision :: xlc
      double precision :: xln
      double precision :: y1b
      double precision :: y2b
      double precision :: ya
      double precision :: yl
      double precision :: ylc
      double precision :: yln

      integer :: ndraw
      COMMON /DRAWTHIS/ ndraw(50)

      COMMON /LOCATORA/ XLC,YLC,XA,YA,JMOUSE,JASHOW
      CHARACTER WRDKEY*40
      PARAMETER (MAXZOOM = 4)
      double precision, save :: XYWOLD(MAXZOOM,4)

      IF (NUMZOOM .EQ. 0) THEN
         DO 5 K = 1,MAXZOOM
            XYWOLD(K,1) = XMIN
            XYWOLD(K,2) = YMIN
            XYWOLD(K,3) = XMAX
            XYWOLD(K,4) = YMAX
    5    CONTINUE
         NUMZOOM = 1
      ENDIF
!     geen entry ALS NET BEZIG PUNT TE ZETTEN
!     BIJ VERLATEN MET KEY = 3, TEKEN OPNIEUW
      WRDKEY   = 'Z   = ZOOMIN ;'
      NLEVEL   = 3
      JADRAW   = 1
      ndraw(1) = 1 ! set cls on

      IF (NPUT .EQ. 1) RETURN

      CALL LINEWIDTH(2)
      CALL SETCOL(KLZM)
      CALL SETXOR(1)
      CALL BOTLIN(0,5,NNN)
      CALL INQASP(ASPECT)
      DXH   = (X2 - X1)
      DYH   = DXH*ASPECT
      DSIXN = DSIX
      XL    = XLC
      YL    = YLC
      X1B   = XL - DSIXN
      X2B   = XL + DSIXN
      Y1B   = YL - DSIXN*ASPECT
      Y2B   = YL + DSIXN*ASPECT

   10 CONTINUE

      IF (JADRAW .EQ. 1) THEN
         CALL BOX(X1B,Y1B,X2B,Y2B)
         JADRAW = 0
      ENDIF
      JA   = 0
      KEY  = 999
      CALL READLOCATOR(XL,YL,KEY)

      IF (X2B .GT. X2 .OR. X1B .LT. X1 .OR. Y2B .GT. Y2 .OR. Y1B .LT. Y1    ) THEN
         X1 = XL - DXH/2
         Y1 = YL - DYH/2
         JA = 1
      ELSE IF (KEY .EQ. 21) THEN
         X1  = X1B
         Y1  = Y1B
         DXH = MAX((X2B - X1B),1.0D-3)
         JA  = 1
      ELSE IF (KEY .EQ. 22) THEN
         X1  = XMIN
         Y1  = YMIN
         DXH = XMAX - XMIN
         JA  = 3
      ELSE IF (KEY .EQ. 90 .OR. KEY .EQ. 90+32) THEN
         DXH = 3*DXH
         DYH = 3*DYH
         X1  = XL - DXH/2
         Y1  = YL - DYH/2
         JA  = 1
      ELSE IF (KEY .EQ. 23) THEN
         KEY = 3
         CALL SETXOR(0)
         CALL LINEWIDTH(1)
         CALL IMOUSECURSORHIDE()
         RETURN
      ELSE IF (KEY .EQ. 24) THEN
!        F1
         NLEVEL = 3
         CALL HELP(WRDKEY,NLEVEL)
      ELSE IF (KEY .EQ. 25) THEN
!        F2
         CALL HISTOR()
      ELSE IF (KEY .EQ. 162 .OR. KEY .EQ. 160 .OR. KEY .EQ. 45 .OR.    &
               KEY .EQ. 43  .OR. KEY .LT. 0                  ) THEN
         CALL BOX(X1B,Y1B,X2B,Y2B)
         JADRAW = 1
         IF (KEY .EQ. 162 .OR. KEY .EQ. 43) THEN
            DSIXN  = DSIXN + RCIR/2
         ELSE IF (KEY .EQ. 160 .OR. KEY .EQ. 45) THEN
            DSIXN  = MAX(RCIR,DSIXN - RCIR/2)
         ENDIF
         X1B  = XL - DSIXN
         X2B  = XL + DSIXN
         Y1B  = YL - DSIXN*ASPECT
         Y2B  = YL + DSIXN*ASPECT
      ELSE IF (KEY .EQ. 143) THEN
         NUMZOOM = NUMZOOM - 1
         IF (NUMZOOM .EQ. 0) NUMZOOM = MAXZOOM
         X1  = XYWOLD(NUMZOOM,1)
         Y1  = XYWOLD(NUMZOOM,2)
         DXH = XYWOLD(NUMZOOM,3) - XYWOLD(NUMZOOM,1)
         JA  = 2
      ENDIF

      IF (JA .GE. 1) THEN
         CALL IMOUSECURSORHIDE()
         X2   = X1 + DXH
         CALL SETWY(X1,Y1,X2,Y2)
         IF ( JA .NE. 2) THEN
!           alleen opslaan als in of uitgezoomd, niet als teruggezoomd
            NUMZOOM = NUMZOOM + 1
            IF (NUMZOOM .EQ. MAXZOOM+1) NUMZOOM = 1
            XYWOLD(NUMZOOM,1) = X1
            XYWOLD(NUMZOOM,2) = Y1
            XYWOLD(NUMZOOM,3) = X2
            XYWOLD(NUMZOOM,4) = Y2
         ENDIF
         XLN  = 0d0
         YLN  = 0d0
         CALL ORGLOCATOR(XLN,YLN)
         KEY  = 3
         CALL LINEWIDTH(1)
         CALL SETXOR(0)
         RETURN
      ENDIF
      GOTO 10

      END

      SUBROUTINE XYDISFORMAT ()
      use m_sferic
      use m_wearelt 
      implicit none

      double precision :: dv
      integer :: ix
      integer :: ixmax
      integer :: ixmin
      integer :: ixy
      integer :: iy
      integer :: iymax
      integer :: iymin
      integer :: izmax
      integer :: izmin
      integer :: jaauto, JMOUSE,JASHOW
      integer :: ncols
      integer :: ndec
      integer :: nie
      integer :: nis
      integer :: nv
      integer :: nxy
      integer :: nz
      double precision :: val
      double precision :: vmax, XLC,YLC,XA,YA
      double precision :: vmin
      double precision :: dlen


      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

      COMMON /DISPFOR/ XYFORM, ZFORM, DISFORM
      CHARACTER*6      XYFORM, ZFORM, DISFORM

      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW


      ZFORM  = '(F7.1)'

      xlc   = max(x1, min(x2, xlc) )
      ylc   = max(y1, min(y2, ylc) )

      IXMIN = INT(LOG10(MAX(1d-6,ABS(X1))))
      IXMAX = INT(LOG10(MAX(1d-6,ABS(X2))))
      IYMIN = INT(LOG10(MAX(1d-6,ABS(Y1))))
      IYMAX = INT(LOG10(MAX(1d-6,ABS(Y2))))
      IZMIN = INT(LOG10(MAX(1d0,ABS(VMIN))))
      IZMAX = INT(LOG10(MAX(1d0,ABS(VMAX))))

      IX  = MAX (IXMIN, IXMAX)
      IY  = MAX (IYMIN, IYMAX)
      IXY = MAX (IX,    IY   )

!     -------------------
!     1 VOOR +-
!     1 VOOR .
!     1 VOOR LOG(100) = 2
!     -------------------

      NXY  = IXY + 3
      NDEC = 9 - NXY
      IF (NDEC .GE. 0) THEN
         XYFORM = '(F9.1)'
         WRITE ( XYFORM(5:5),'(I1)') NDEC
      ELSE
         XYFORM = '(E9.3)'
      ENDIF

      !if (jsferic == 1) then ! nou ja, laat maar even staan
      !   dlen = DbdISTANCE( X1,Y1,X2,Y2)
      !   Ixy  = INT(LOG10(MAX(1d0,dlen ) ) )
      !   NXY  = IXY + 3
      !   NDEC = 9 - NXY
      !   IF (NDEC .GE. 1) THEN
      !      disFORM = '(F9.1)'
      !      WRITE ( disform(5:5),'(I1)') NDEC
      !   ELSE
      !     disFORM = '(E9.3)'
      !   ENDIF
      !endif
      ! DISFORM='F17.5'


      NZ  = IZMAX + 3
      WRITE (  ZFORM(5:5),'(I1)') max(0, 9 - NZ)

      RETURN
      END

      SUBROUTINE CHEKHW()
      implicit none
      integer :: infogrscreen
      integer :: key

!  check the hardware in use - must have graphics

      LOGICAL  NOGRAF

      NOGRAF = InfoGrScreen(1).EQ.0
      IF (NOGRAF) THEN
          CALL IOutError('Sorry, this program requires a display '      &
                    //'with graphics capability - Press a key')
          CALL InKeyEvent(KEY)
!         exit tidily, clearing the screen
          CALL IScreenQuit('C')
      ENDIF
      RETURN
      END

      SUBROUTINE INIKEYS()
      use m_devices
      implicit none
      integer :: i
      integer :: nkey
      integer :: numc
      integer :: numkeys
      COMMON /NKEYS/ NUMKEYS, NKEY(20), NUMC(20)
!     Keyboard
      NKEY( 1) = 142
      NKEY( 2) = 166
      NKEY( 3) =  13
      NKEY( 4) =  27
      NKEY( 5) = 171
      NKEY( 6) = 172
      NKEY( 7) = 173
      NKEY( 8) =   9

      IF (NOPSYS .GT. 1) THEN
         NKEY(9) = 259
      ELSE
         NKEY(9) = NKEY(8)
      ENDIF

!     Muistoetsen
      NKEY(10) = 251
      NKEY(11) = 252
      NKEY(12) = 253


      NUMC( 1) =  21
      NUMC( 2) =  22
      NUMC( 3) =  22
      NUMC( 4) =  23
      NUMC( 5) =  24
      NUMC( 6) =  25
      NUMC( 7) =  26
      NUMC( 8) =  27

      IF (NOPSYS .GT. 1) THEN
         NUMC(9) = 50
      ELSE
         NUMC(9) = NUMC(8)
      ENDIF

      NUMC(10) =  21
      NUMC(11) =  22
      NUMC(12) =  22

      NUMKEYS = 9
      DO 10 I = 1,NUMKEYS
         CALL INCONTROLKEY(NUMC(I),NKEY(I))
   10 CONTINUE
      NUMKEYS = 12

!     INS CONFIRM                  CALL INConTRoLkey(21, 142)
!     ENTER CONFIRM                CALL INConTRoLkey(22,  13)
!     ENTER KEYPAD CONFIRM         CALL INConTRoLkey(22, 166)
!     ESC                          CALL INConTRoLkey(23,  27)
!     F1 HELP                      CALL INConTRoLkey(24, 171)
!     F2 HISTORY                   CALL INConTRoLkey(25, 172)
!     F3 COMMAND                   CALL INConTRoLkey(26, 173)
!     TAB SWITCH TUSSEN 3 SCHERMEN CALL INConTRoLkey(27,   9)
!     EXPOSE RESIZE                CALL INCONTROLKEY(50, 259)
      RETURN
      END



      SUBROUTINE TEKADMIN(X,Y,I,J)
      implicit none
      integer :: i
      integer :: j
      integer :: l
      double precision :: x
      double precision :: y
      CHARACTER TEX*11
      IF (I .LE. 9) THEN
         WRITE(TEX(1:1) ,'(I1)') I
         L = 2
      ELSE IF (I .LE. 99) THEN
         WRITE(TEX(1:2) ,'(I2)') I
         L = 3
      ELSE IF (I .LE. 999) THEN
         WRITE(TEX(1:3) ,'(I3)') I
         L = 4
      ELSE
         WRITE(TEX(1:4) ,'(I4)') I
         L = 5
      ENDIF
      WRITE(TEX(L:L),'(A)') ','
      IF (J .LE. 9) THEN
         WRITE(TEX(L+1:L+1) ,'(I1)') J
         L = L + 1
      ELSE IF (J .LE. 99) THEN
         WRITE(TEX(L+1:L+2) ,'(I2)') J
         L = L + 2
      ELSE IF (J .LE. 999) THEN
         WRITE(TEX(L+1:L+3) ,'(I3)') J
         L = L + 3
      ELSE
         WRITE(TEX(L+1:L+4) ,'(I4)') J
         L = L + 4
      ENDIF
      CALL DRAWTEXT(real(X),real(Y), TEX(1:L))
      RETURN
      END

      subroutine DISDEP (m,n,dep)
      use m_devices
      implicit none
      double precision :: dep
      integer :: m
      integer :: n
      character distan*23
      character fmt*6

      DISTAN = 'M:    N:    D:         '
      WRITE(DISTAN (3:5),'(I3)') M
      WRITE(DISTAN (9:11),'(I3)') N
      fmt = '(f9.3)'
      call dispform (dep, fmt)
      WRITE(DISTAN (15:23),fmt) DEP
      CALL KTEXT(DISTAN,IWS-22,4,15)

      RETURN
      END

      SUBROUTINE DISPNODE (MP)
      use m_devices
      use m_netw, only : zk
      implicit none
      integer :: mp
      CHARACTER TEX*23

      IF (MP .LE. 0) THEN
         TEX = 'NODE NOT FOUND        '
         CALL KTEXT(TEX,IWS-22,4,15)
      ELSE
         TEX = 'NODE NR:              '
         WRITE(TEX (10:),'(I10)') MP
         CALL KTEXT(TEX,IWS-22,4,15)

!         TEX = 'ZK Lev :           (m)'
!         WRITE(TEX (10:18),'(F9.3)') zk(mp)
!         CALL KTEXT(TEX,IWS-22,5,15)
      ENDIF

      RETURN
      END

      SUBROUTINE DISPNODE2 (MP, NP)
      use m_grid, only : zc
      use m_devices
      implicit none
      integer :: mp, np
      CHARACTER TEX*23

      IF (MP .LE. 0) THEN
         TEX = 'NODE NOT FOUND        '
         CALL KTEXT(TEX,IWS-22,4,15)
      ELSE
         TEX = 'NODE NR:              '
         WRITE(TEX (10:),'(I4,A1,I4)') MP, ',', NP
         CALL KTEXT(TEX,IWS-22,4,15)
!         TEX = 'ZC Lev :           (m)'
!         WRITE(TEX (10:18),'(F9.3)') zc(mp,np)
!         CALL KTEXT(TEX,IWS-22,5,15)
      ENDIF

      RETURN
      END

      SUBROUTINE DISLINK (MP)
      use m_devices
      implicit none
      integer :: mp
      CHARACTER TEX*23

      IF (MP .LE. 0) THEN
         TEX = 'LINK NOT FOUND        '
      ELSE
         TEX = 'LINK NR:              '
         WRITE(TEX (10:),'(I10)') MP
      ENDIF
      CALL KTEXT(TEX,IWS-22,4,15)

      RETURN
      END

      SUBROUTINE DISDIS()
      
      use m_devices
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      
      double precision :: dis
      integer :: jashow
      integer :: jmouse
      double precision :: xa
      double precision :: xlc
      double precision :: ya
      double precision :: ylc
!     -------------------------------
!     write distance
!     -------------------------------
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW
      common /dispfor/ xyform, zform, disform
      character*6      xyform, zform, disform
      CHARACTER DISTAN*24

      DISTAN = 'DIS:'
      DIS    = dbdistance(xa,ya,xlc,ylc, jsferic, jasfer3D, dmiss)
      WRITE(DISTAN(6:),'(F17.5)') min(DIS,1d9)
      CALL KTEXT(DISTAN,IWS-22,3,15)

  !   checkdislin()

      RETURN
      END


!     -------------------------------------------------------------------

      SUBROUTINE FULLSCREEN()
      CALL viewport(0.0,0.0,1.0,1.0)
      RETURN
      END

      subroutine viewport(xs1, ys1, xs2, ys2)
      use unstruc_opengl
      implicit none
      real xs1, ys1, xs2, ys2
      IF (InOpenGLRendering) THEN
#ifdef HAVE_OPENGL
          ! screen coordinates extend
          CALL fglViewPort(int(xs1*currentWidth), int(ys1*currentHeight), int((xs2-xs1)*currentWidth), int((ys2-ys1)*currentHeight) )
#endif
      else
         call igrarea(xs1, ys1, xs2, ys2)
      endif
      end


      SUBROUTINE SMALLSCREEN()
      implicit none
      integer :: jaxis
      double precision :: xleft
      double precision :: xright
      double precision :: ybot
      double precision :: ytop
      COMMON /SCREENAREA/ XLEFT,YBOT,JAXIS

      YTOP   = MAX(0.95d0,1 - YBOT)
      XRIGHT = MAX(0.90d0,1 - XLEFT)
      call viewport(real(XLEFT),real(YBOT),real(XRIGHT),real(YTOP))

      RETURN
      END



      SUBROUTINE TXTLINES()
      use m_devices
      use m_textlines
      implicit none
      integer :: i

      CALL IGRCHARSIZE(real(TXSIZE),real(TXSIZE))

       do i = 1,3
          if (len_trim(TXLIN(i)) > 0) then
             CALL MTEXT(TXLIN(i), TXXpos, TXYpos+0.04d0*(4-i), 3)
          endif
       enddo

      CALL SETTEXTSIZE()

      RETURN
      END

      SUBROUTINE TXTTIM()
      use m_devices
      implicit none
      integer :: l
      double precision :: txtimsize
      double precision :: txtimx
      double precision :: txtimy
      COMMON /TEXTIM/  TXTIMSIZE, TXTIMX, TXTIMY, TXTIM

      CHARACTER TXTIM*60
      L = len_trim(TXTIM)
      IF (L .EQ. 0) RETURN
      CALL IGRCHARSIZE(real(TXTIMSIZE),real(TXTIMSIZE))
      CALL IGRCHARFONT(3)
      CALL MTEXT(TXTIM,TXTIMX,TXTIMY,35)
      CALL IGRCHARFONT(1)
      CALL SETTEXTSIZE()

      RETURN
      END

      SUBROUTINE MTEXT(TEX,X,Y,NCOL)
      use unstruc_colors
      implicit none
      double precision :: heigth
            integer :: l
      integer :: ncol
      double precision :: w1
      double precision :: width
      double precision :: x
      double precision :: xt
      double precision :: y
      double precision :: yt
!     grafische text op RELATIEVE grafische posities + achtergrondje
      REAL INFOGRAPHICS, IGRCHARLENGTH
      CHARACTER TEX*(*)
      L = len_trim(TEX)
      WIDTH  = IGRCHARLENGTH(TEX(1:L))*INFOGRAPHICS(3)
      W1     = IGRCHARLENGTH(TEX(1:1))*INFOGRAPHICS(3)
      HEIGTH = INFOGRAPHICS(4)
      XT = X1 + X*(X2-X1)
      YT = Y1 + Y*(Y2-Y1)
      CALL SETCOL(KLSCL)
      CALL FBOX(XT-WIDTH/2,YT-HEIGTH/2,XT+WIDTH/2+w1/2,YT+HEIGTH/2)
      CALL SETCOL(NCOL)
      CALL BOX (XT-WIDTH/2,YT-HEIGTH/2,XT+WIDTH/2+w1/2,YT+HEIGTH/2)
      CALL DRAWTEXT(real(XT+W1/2-WIDTH/2),real(YT),TEX)
      RETURN
      END

      SUBROUTINE SETTEXTSIZE()
      use unstruc_opengl
      implicit none
      double precision :: tsize
      COMMON /TEXTSIZE/ TSIZE
      IF (InOpenGLRendering) THEN
         CALL SetTextHeight(int(FontSize*TSIZE))
      ELSE
         CALL IGRCHARSIZE(real(TSIZE),real(TSIZE))
      ENDIF
      END

      SUBROUTINE SETTEXTSIZEFAC(T)
      use unstruc_opengl
      implicit none
      double precision :: tsize,t
      COMMON /TEXTSIZE/ TSIZE
      IF (InOpenGLRendering) THEN
         CALL SetTextHeight(int(FontSize*T*TSIZE))
      ELSE
         CALL IGRCHARSIZE(real(T*TSIZE),real(T*TSIZE))
      ENDIF
      END


      SUBROUTINE AXES()
      use unstruc_colors
      implicit none
      integer :: jaxis
      double precision :: xleft
      double precision :: ybot
      COMMON /SCREENAREA/ XLEFT,YBOT,JAXIS
      IF (JAXIS .EQ. 1) THEN
         CALL SETCOL(KLAXS)
         CALL viewport(0.0,0.0,1.0,1.0)
         CALL IPGBORDER()
         CALL IPGXTICKPOS(Y1,Y2)
         CALL IPGXSCALE     ('TN')
         CALL IPGXSCALETOP  ('TN')
         CALL IPGYTICKPOS(X1,X2)
         CALL IPGYSCALELEFT ('TN')
         CALL IPGYSCALERIGHT('TN')
         CALL SMALLSCREEN()
      ENDIF
      RETURN
      END

      SUBROUTINE NEWWORLD()
      use m_wearelt
      implicit none
      double precision :: asp
      integer :: jaxis
      double precision :: xleft
      double precision :: xright
      double precision :: xw
      double precision :: ybot
      double precision :: yc
      double precision :: ytop
      double precision :: ywn
      COMMON /SCREENAREA/ XLEFT,YBOT,JAXIS

      CALL INQASP(ASP)
      YTOP   = MAX(0.95d0,1 - YBOT)
      XRIGHT = MAX(0.90d0,1 - XLEFT)

      YC     = (Y1+Y2)/2
      XW     =  X2-X1
      YWN    =  XW*ASP
      Y1     =  YC - YWN/2
      Y2     =  YC + YWN/2

      CALL SETWY(X1,Y1,X2,Y2)

      YC     = (YMAX+YMIN)/2
      XW     =  XMAX-XMIN
      YWN    =  XW*ASP
      YMIN   =  YC - YWN/2
      YMAX   =  YC + YWN/2

      RETURN
      END

      SUBROUTINE VECSCALE_DFLOWFM(VFAC2)
      USE M_WEARELT
      implicit none
      double precision :: heightline
      integer :: ihcopts
      integer :: klscl
      integer :: ndec
      integer :: ndraw
      integer :: nhcdev
      integer :: numhcopts
      double precision :: vfac2
      double precision :: xp1
      double precision :: xsc
      double precision :: xsc1
      double precision :: xsc2
      double precision :: yp1
      double precision :: yp2
      double precision :: ysc
      double precision :: ysc1
      double precision :: ysc2
      real :: rx, ry
      double precision :: scalesize
!     tekenen legenda
      COMMON /HARDCOPY/ NHCDEV,NUMHCOPTS,IHCOPTS(2,20)
      COMMON /DRAWTHIS/ ndraw(50)
      COMMON /SCALEPOS/ XSC,YSC,SCALESIZE,NDEC

      CHARACTER TEXT2*9

      IF (NDRAW(12) .LE. 2) RETURN

      CALL IGRCHARSIZE(real(SCALESIZE),real(SCALESIZE))

      XSC1 = X1   + XSC*(X2-X1)
      XSC2 = XSC1 + 1.2d0*DSIX/2
      YSC2 = Y1   + YSC*(Y2-Y1) - RCIR
      CALL IGRUNITSFROMPIXELS(1,1,rx, ry)
      XP1 = dble(rx)
      YP1 = dble(ry)

      CALL IGRUNITSFROMPIXELS(1,1+NINT(16*SCALESIZE),rx, ry)
      YP2 = dble(ry)

      HEIGHTLINE = 2*(YP2 - YP1)
      YSC1 = YSC2 - (2d0)*HEIGHTLINE

      IF (NDRAW(10) .EQ. 0) THEN
         CALL SETCOL(KLSCL)
      ELSE
         IF (NHCDEV .EQ. 2) CALL SETCOL(0)
      ENDIF
      call RECTANGLE(real(XSC1),real(YSC1),real(XSC2),real(YSC2))
      CALL SETCOL(1)
      CALL BOX(XSC1,YSC1,XSC2,YSC2)
      RETURN
      END

      SUBROUTINE SETGRAFMOD()
      use m_devices
      implicit none
      integer :: infoscreen
      integer :: infoscreenmode
      integer :: mode

      MODE   = INFOSCREEN(1)
      IWS    = INFOSCREEN(2)
      IHS    = INFOSCREEN(3)
      NPX    = INFOSCREEN(4)
      NPY    = INFOSCREEN(5)
      NCOLR  = INFOSCREENMODE(6,MODE)
      NDEV   = MODE
      
     CALL ISCREENMODEOPTIONS(1,iws)
     CALL ISCREENMODEOPTIONS(2,ihs)
      
!     IF (NOPSYS .EQ. 1) THEN

!     ENDIF
      RETURN
      END


      SUBROUTINE INTINI()
      use m_sferic
      use unstruc_version_module, only : unstruc_company, unstruc_program, unstruc_version
      use m_wearelt
      use m_devices
      implicit none
      double precision :: croshrsz
      integer :: icrhf
      integer :: infoopsystem
      integer :: jashow
      integer :: jaxis
      integer :: jmouse
      integer :: jvga
      integer :: ncolnow
      integer :: ntxcols
      integer :: ntxrows
      integer :: nxpix
      integer :: nypix
      double precision :: xa
      double precision :: xlc
      double precision :: xleft
      double precision :: ya
      double precision :: ybot
      double precision :: ylc

      COMMON /INITSCREEN/  CROSHRSZ,JVGA,NXPIX,NYPIX,NTXCOLS,NTXROWS
      COMMON /SCREENAREA/ XLEFT,YBOT,JAXIS
      COMMON /COLNOW/ NCOLNOW
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW

      JSFERIC = 0
     ! CALL ISCREENMODEOPTIONS(1,NTXCOLS)
     ! CALL ISCREENMODEOPTIONS(2,NTXROWS)
      CALL ISCREENMODEOPTIONS(6,1)
      CALL ISCREENMODEOPTIONS(9,1)
      NOPSYS = INFOOPSYSTEM(1)
      NCOLR  = 256

      IF (NOPSYS .EQ. 1 .AND. JVGA .EQ. 1) THEN
         NXPIX    = 640
         NYPIX    = 480
         NCOLR    =  16
!        CALL VGA@()
      ENDIF
      CALL ISCREENOPEN(' ','GR',NXPIX,NYPIX,NCOLR)

      CALL ISCREENTITLE('G',trim(unstruc_company)//'-'//trim(unstruc_program)//' '//trim(unstruc_version))

      !CALL ISCREENTITLE('G', PROGNM)

      CALL SETGRAFMOD()
      CALL SETCOLORTABLE()

      CALL INIKEYS()
!      CALL INSERTOVER('OVER')

!     set size crosshair cursor
      ICRHF = 1d0/CROSHRSZ

      CALL IGRINPUTOPTIONS(5,ICRHF)
!
      CALL InEventSelect(0,0)
      IF (NOPSYS .EQ. 1) THEN
!        Mouse button down, up, move is an event
         CALL InEventSelect(0,1+2+8)
      ELSE
!        Mouse button down, up, resize an event
         CALL InEventSelect(0,1+2+32)
         CALL InEventSelect(0,1+2+8+32)
!        Enable processing of expose/resize events
         CALL InControlKey(50,259)
      ENDIF
!
      CALL ICURSOR(' ')

!     exit on mouse click outside input area
      CALL INMOUSEOPTIONS(2,1)

!     only BUTTON DOWN
      CALL INMOUSEOPTIONS(3,0)

      CALL IFRAMEOPTIONS(6,15)
      CALL IFRAMEOPTIONS(7,0)

!     CALL IFRAMETYPE(9)
!     CALL IFORMDEFAULTS(3)

      CALL SETTEXTSIZE()
      CALL IGRFILLPATTERN(4,0,0)

      YBOT  = 0d0
      XLEFT = 0d0
      JAXIS = 0
      CALL viewport(0.0,0.0,1.0,1.0)
!      CALL IPGAREA(0.0,0.0,1.0,1.0)

      XMIN   = 0d0
      XMAX   = 1d0
      YMIN   = 0d0
      YMAX   = 1d0
      X1     = XMIN
      X2     = XMAX
      Y1     = YMIN
      Y2     = YMAX
      NCOLNOW = 31
      XLC = 0
      YLC = 0
      CALL WEAREL()

      RETURN
      END


!> Highlights the 'string' field of a user-input form field.
!! Input fields are highlighted automatically, but the string label isn't.
!! This assumes that string field number is always input field number minus 1.
!! Only use this subroutine as the FMUSER argument to IFormEditUser(.., .., FMUSER).
subroutine highlight_form_line(ifield, iexitk)
implicit none
integer, intent(in) :: ifield !< Form field number that lost focus (infoform(3) contains 'next' field).
integer, intent(in) :: iexitk !< 'Exit' key that was used to leave this form field.

integer :: ifieldnext
integer, external :: InfoForm

ifieldnext = InfoForm(3)

! Reset the 'current' field back to defaults (no highlights)
if (ifield > 1) then
    call iformattributen(ifield-1, 0, -1, -1)
    call iformshowfield(ifield-1)
end if
if (ifieldnext > 1) then
    call iformattribute(ifieldnext-1, 'UB', ' ', ' ')
    call iformshowfield(ifieldnext-1)
end if

end subroutine highlight_form_line


      SUBROUTINE WAITESC()
      implicit none
      integer :: key
      CALL INFLUSH()
   10 CONTINUE
      CALL INKEYEVENTIMM(KEY)
      IF (KEY .EQ. 27) RETURN
      GOTO 10
      END

      SUBROUTINE WAIT()
      implicit none
      integer :: key
      CALL INFLUSH()
   10 CONTINUE
      CALL INKEYEVENTIMM(KEY)
      IF (KEY .NE. -999 .AND. KEY .NE. -32387) RETURN
      GOTO 10
      END

      SUBROUTINE WAITSECS(NSEC)
      implicit none
      integer :: i
      integer :: key
      integer :: nsec
      CALL INFLUSH()
      DO 10 I = 1,NSEC
         CALL IOSWAIT(100)
         CALL INKEYEVENTIMM(KEY)
         IF (KEY .NE. -999 .AND. KEY .NE. -32387) RETURN
   10 CONTINUE
      RETURN
      END


      SUBROUTINE SETCOLORTABLE()
      implicit none
      double precision :: dv, dv2
      integer :: i
      integer :: jaauto, jaauto2
      integer :: ncols, ncols2
      integer :: nie, nie2
      integer :: nis, nis2
      integer :: nv, nv2
      double precision :: val, val2
      double precision :: vmax, vmax2
      double precision :: vmin, vmin2
      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
      COMMON /DEPMAX2/ VMAX2,VMIN2,DV2,VAL2(256),NCOLS2(256),NV2,NIS2,NIE2,JAAUTO2

      NIS  = 72
      DO I = 1,256
         NCOLS(I) = MIN(255,NIS + I-1)
      enddo

      NIS2 = 136
      DO I = 1,256
         NCOLS2(I) = MIN(255,NIS2 + I-1)
      enddo
      end SUBROUTINE SETCOLORTABLE


      SUBROUTINE SETCOL(NCOL)
      use unstruc_opengl
      implicit none
      integer :: ncol
      integer :: ncolnow
      COMMON /COLNOW/ NCOLNOW
      IF (NCOL .NE. NCOLNOW ) THEN
          CALL IGRCOLOURN(NCOL)
          CALL SetColorFromColorNr(NCOL)
      ENDIF
      NCOLNOW = NCOL
      RETURN
      END

      SUBROUTINE HELP(WRDKEY,NLEVEL)
      use unstruc_display
      use unstruc_version_module, only : unstruc_company, unstruc_program
      implicit none
      integer :: i
      integer :: ih
      integer :: infowindow
      integer :: iw
      integer :: ixp
      integer :: ixs
      integer :: iyp
      integer :: iys
      integer :: japop
      integer :: jatab
      integer :: jofnd
      integer :: len
      integer :: line
      integer :: maxhlp
      integer :: maxkwd
      integer :: nahead
      integer :: nback
      integer :: nforg
      integer :: nlevel
      integer :: numchc
      integer :: numkey
      integer :: numpag
      integer :: numpgk
      integer :: numtop
      integer :: numtxt
      integer :: numwnb
      integer :: numwnh
      integer :: numwnk
      integer :: numwnt
!     Gives helptext starting from wrdkey in screen with dimensions npos
      PARAMETER (MAXHLP = 2000, MAXKWD = 400)
      INTEGER NHTONK(MAXHLP), NKTONH(MAXKWD)
      CHARACTER HLPTXT(MAXHLP)*80,WRDKEY*40,KEYWRD(MAXKWD)*40,LOOKUP*20,TEXLIN*80
      COMMON /HELPC/   HLPTXT,NUMTXT
!
!     Initialise
      CALL IWinWordWrap('OFF')
      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
      CALL INHIGHLIGHT('WHITE','RED')
!     IXP    = 1
!     IYP    = 1
!     IW     = IXP + IWS-IW
!     IH     = INFOSCREEN(3) - 9
      IW     = NPOS(3)
      IH     = IHS - 9
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2) - 1
      NAHEAD = 1
      JATAB  = 0
      JAPOP  = 0
      NUMTOP = NUMTXT + 1
      NUMCHC = 1
      NUMKEY = 0
      NUMPAG = 1 + (NUMTXT-IH+1) / IH
      LOOKUP = WRDKEY
!
!     Count the number of keywords in text and make cross references
      DO 10 I = 1,NUMTXT
         IF (HLPTXT(I)(1:3) .NE. '   ') THEN
            NUMKEY         = NUMKEY + 1
            KEYWRD(NUMKEY) = HLPTXT(I)
            NKTONH(NUMKEY) = I
         ENDIF
         NHTONK(I)      = NUMKEY
   10 CONTINUE
      NUMPGK = 1 + (NUMKEY-IH+1) / IH
!
!     Header of helpwindow
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP,IW,1)
      NUMWNT = InfoWindow(1)
      CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
      TEXLIN = '               '//trim(unstruc_company)//'-'//trim(unstruc_program)//' HELPWINDOW'
      CALL IWinOutSTRINGXY(1,1,TEXLIN)
      CALL IWinOutStringXY (IW-16,1,'page =    of   ')
      CALL IWinOutIntegerXY(IW-3,1,NUMPAG,2)

!     TEXLIN = '                     '//PROGNM//  ' HELPWINDOW
!    *        page =    of   '
!     CALL IWinOutStringXY(1,1,TEXLIN)
!     CALL IWinOutIntegerXY(IW-10,1,NUMPAG,2)

!     Explain keyfunctions in bottom window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP+6+IH,IW,2)
      NUMWNB = InfoWindow(1)
      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
      CALL IWinOutStringXY (1,1,'pages = PgUp/PgDn; scroll =   ; toggle keyword menu = Tab')
      CALL IWinOutStringXY (1,2,'top or bottom = Home/End; exit = Esc; search = F7')
!
!     Helpwindow is middelste window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP+4,IW,IH)
      NUMWNH = InfoWindow(1)
!
!     Start with keyword WRDKEY
      CALL SEARCH(NAHEAD,NLEVEL,HLPTXT,NUMTXT,WRDKEY,NUMCHC,JOFND)
!
   20 CONTINUE
!
!
!     Display one page of help
      IF (JATAB .EQ. 0) THEN
         CALL IWinSelect(NUMWNH)
         CALL SCRLPG(HLPTXT,NUMTXT,NUMTOP,NUMCHC,IH)
      ELSE
         CALL IWinSelect(NUMWNK)
         CALL SCRLPG(KEYWRD,NUMKEY,NUMTOP,NUMCHC,IH)
      ENDIF
!
!     Display pagenumber in top window
      CALL IWinSelect(NUMWNT)
      CALL IWinOutIntegerXY(IW-9,1,1+NUMTOP/IH,2)
!
!     Indicate present keyword level with cursor position
      CALL ITextAttribute('BRU')
      IF (JATAB .EQ. 0) THEN
      CALL IWinSelect(NUMWNH)
      CALL IWinOutStringXY (NLEVEL,NUMCHC-NUMTOP+1,HLPTXT(NUMCHC)(NLEVEL:NLEVEL))
      ELSE
      CALL IWinSelect(NUMWNK)
      CALL IWinOutStringXY (NLEVEL,NUMCHC-NUMTOP+1,KEYWRD(NUMCHC)(NLEVEL:NLEVEL))
      ENDIF
      CALL ITextAttribute(' ')
!
!     Get instructions
      IF (JATAB .EQ. 0) THEN
         CALL SCROLH(NUMCHC,HLPTXT,NUMTXT,NLEVEL,IH,JOFND,JATAB)
      ELSE
         CALL SCROLH(NUMCHC,KEYWRD,NUMKEY,NLEVEL,IH,JOFND,JATAB)
      ENDIF
!
      IF (JOFND .EQ. -1) THEN
!        Search for keyword
         IXS    = NPOS(1)+46
         IYS    = NPOS(2)+IH+6
         CALL InStringXYDef(IXS,IYS,' => ',0,LOOKUP,LEN)
         IF (JATAB .EQ. 0) THEN
            CALL SEARC2(NAHEAD,NLEVEL,HLPTXT,NUMTXT,LOOKUP,NUMCHC,JOFND)
         ELSE
            CALL SEARC2(NAHEAD,NLEVEL,KEYWRD,NUMKEY,LOOKUP,NUMCHC,JOFND)
         ENDIF
         CALL IWinSelect(NUMWNB)
         CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
         CALL IWinOutStringXY (1,2,'top or bottom = Home/End; exit = Esc; search : F7)                               . ')
         IF (JATAB .EQ. 1) CALL ITEXTCOLOURN(WNDFOR,WNDBCK)
      ELSE IF (JATAB .EQ. 1) THEN
!        met tab wordt popup keyword window geopend of gesloten
         IF (JAPOP .EQ. 0) THEN
            CALL IWinSelect(NUMWNT)
            CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
            TEXLIN = '               '//trim(unstruc_company)//'-'//trim(unstruc_program)//' KEYWORDWINDOW'
            CALL IWinOutSTRINGXY(1,1,TEXLIN)
            CALL IWinOutStringXY (IW-16,1,'page =    of   ')
            CALL IWinOutIntegerXY(IW-3,1,NUMPGK,2)
            CALL ITEXTCOLOURN(WNDFOR,WNDBCK)
            CALL IWinAction('PC')
            CALL IWinOpen(IXP+40,IYP+4,IW-40,IH)
            NUMWNK = InfoWindow(1)
            JAPOP  = 1
            LINE   = NUMCHC - NUMTOP
            NUMCHC = NHTONK(NUMCHC)
            NUMTOP = MAX( 1,MIN( NUMCHC - LINE,NUMKEY - IH + 1) )
         ENDIF
      ELSE
         IF (JAPOP .EQ. 1) THEN
            CALL IWinSelect(NUMWNK)
            CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
            CALL IWinClose(1)
            JAPOP  = 0
            CALL IWinSelect(NUMWNT)
            CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
            TEXLIN = '               '//trim(unstruc_company)//'-'//trim(unstruc_program)//' HELPWINDOW'
            CALL IWinOutSTRINGXY(1,1,TEXLIN)
            CALL IWinOutStringXY (IW-16,1,'page =    of   ')
            CALL IWinOutIntegerXY(IW-3,1,NUMPAG,2)
            LINE   = NUMCHC - NUMTOP
            NUMCHC = NKTONH(NUMCHC)
            NUMTOP = NUMCHC - LINE
            CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
         ENDIF
      ENDIF

      IF (NUMCHC .NE. 0) GOTO 20

      IF (JAPOP .EQ. 1) THEN
         CALL IWinClose(1)
      ENDIF
      CALL IWinClose(1)
      CALL IWinClose(1)
      CALL IWinClose(1)
      CALL ITEXTCOLOURN(NFORG,NBACK)
      RETURN
      END

      SUBROUTINE PAGE(HLPTXT,NUMTXT,NUMTOP,IH)
      implicit none
      integer :: i
      integer :: ih
      integer :: line
      integer :: numtop
      integer :: numtxt
!     Display one page, take care, numtop =< numtxt-ih
      CHARACTER HLPTXT(NUMTXT)*(*)
      LINE    = 0
      DO 10 I = NUMTOP,MIN(NUMTOP + IH - 1,NUMTXT)
         LINE = LINE + 1
         CALL IWinOutStringXY(1,LINE,HLPTXT(I))
   10 CONTINUE
      RETURN
      END
!
      SUBROUTINE SCRLPG(HLPTXT,NUMTXT,NUMTOP,NUMCHC,IH)
      implicit none
      integer :: ih
      integer :: numchc
      integer :: numtop
      integer :: numtxt
!     Display choiceline and one page, take care, numchc <= numtxt
      CHARACTER HLPTXT(NUMTXT)*(*)
!
      IF (NUMCHC .LT. NUMTOP) THEN
         NUMTOP = NUMCHC
      ELSE IF (NUMCHC .GE. NUMTOP+IH) THEN
         NUMTOP = NUMCHC - IH + 1
      ENDIF
!
      CALL PAGE(HLPTXT,NUMTXT,NUMTOP,IH)
!
      RETURN
      END
!
      SUBROUTINE SCROLH(NUMCHC,HLPTXT,NUMTXT,NLEVEL,IH,JOFND,JATAB)
      implicit none
      integer :: ih
      integer :: jatab
      integer :: jofnd
      integer :: key
      integer :: nlevel
      integer :: numchc
      integer :: numtxt
!     Controls NUMCHC, the desired line, 0 means exit
!     The value of NUMCHC is checked against limits in this routine
!     JOFIND : search, JATAB : keywordwindow
      CHARACTER HLPTXT(NUMTXT)*(*)
!
      CALL TIMLIN()
      CALL InKeyEvent(KEY)
      CALL TIMLIN()
      IF (KEY .EQ. 128) THEN
         CALL NEXT(-1,NLEVEL,NUMCHC,HLPTXT,NUMTXT)
      ELSE IF (KEY .EQ. 129) THEN
         CALL NEXT(1,NLEVEL,NUMCHC,HLPTXT,NUMTXT)
      ELSE IF (KEY .EQ. 130) THEN
         NLEVEL = MIN(4,NLEVEL + 1)
         CALL NEXT(1,NLEVEL,NUMCHC,HLPTXT,NUMTXT)
      ELSE IF (KEY .EQ. 131) THEN
         NLEVEL = MAX(1,NLEVEL - 1)
         CALL NEXT(-1,NLEVEL,NUMCHC,HLPTXT,NUMTXT)
      ELSE IF (KEY .EQ. 132) THEN
         NUMCHC = MAX(1,NUMCHC - IH)
      ELSE IF (KEY .EQ. 133) THEN
         NUMCHC = MIN(NUMTXT,NUMCHC + IH)
      ELSE IF (KEY .EQ. 140) THEN
         NUMCHC = 1
      ELSE IF (KEY .EQ. 141) THEN
         NUMCHC = NUMTXT
      ELSE IF (KEY .EQ. 177) THEN
         JOFND = -1
      ELSE IF (KEY .EQ. 27) THEN
         NUMCHC = 0
      ELSE IF (KEY .EQ. 9) THEN
         JATAB = 1 - JATAB
      ENDIF
      RETURN
      END

      SUBROUTINE SEARCH(NAHEAD,NLEVEL,HLPTXT,NUMTXT,WRDKEY,NUMCHC,JOFND)
      implicit none
      integer :: jofnd
      integer :: k
      integer :: len
      integer :: nahead
      integer :: nlevel
      integer :: numchc
      integer :: numtxt
!     Search at level NLEVEL
      CHARACTER HLPTXT(NUMTXT)*(*),WRDKEY*40

      LEN   = len_trim(WRDKEY)
      IF (LEN .EQ. 0) RETURN

      JOFND = 0
      K     = NUMCHC - NAHEAD

   10 CONTINUE
      K = K + NAHEAD
      IF (K .GT. NUMTXT .OR. K .LT. 1) THEN
         IF (JOFND .EQ. 0) CALL OKAY(0)
         RETURN
      ELSE
         IF (HLPTXT(K)(NLEVEL:NLEVEL+LEN-1) .NE. WRDKEY) GOTO 10
      ENDIF

      JOFND  = 1
      NUMCHC = K
      RETURN
      END

      SUBROUTINE SEARC2(NAHEAD,NLEVEL,HLPTXT,NUMTXT,LOOKUP,NUMCHC,JOFND)
      implicit none
      integer :: jofnd
      integer :: k,len
      integer :: nahead
      integer :: nlevel
      integer :: numchc
      integer :: numtxt
!     Search everywhere
      CHARACTER HLPTXT(NUMTXT)*(*),LOOKUP*20

      LEN   = len_trim(LOOKUP)
      IF (LEN .EQ. 0) RETURN

      JOFND = 0
      K     = NUMCHC - NAHEAD

   10 CONTINUE
      K = K + NAHEAD
      IF (K .GT. NUMTXT .OR. K .LT. 1) THEN
         IF (JOFND .EQ. 0) CALL OKAY(0)
         RETURN
      ELSE
         IF (INDEX(HLPTXT(K),LOOKUP(1:LEN)) .EQ. 0) GOTO 10
      ENDIF

      JOFND  = 1
      NUMCHC = MIN(NUMTXT,K + 1)
      RETURN
      END

      SUBROUTINE NEXT(NAHEAD,NLEVEL,NUMCHC,HLPTXT,NUMTXT)
      implicit none
      integer :: nahead
      integer :: nlevel
      integer :: numchc
      integer :: numtxt
!     Searches for previous or next keyword at level nlevel
      CHARACTER HLPTXT(NUMTXT)*(*)
   10 CONTINUE

      NUMCHC = NUMCHC + NAHEAD
      IF (NUMCHC .LE. 1) THEN
         NUMCHC = 1
      ELSE IF (NUMCHC .GE. NUMTXT) THEN
         NUMCHC = NUMTXT
      ELSE IF (HLPTXT(NUMCHC)(1:NLEVEL) .EQ. '   ') THEN
         GOTO 10
      ENDIF

      RETURN
      END

      SUBROUTINE HELPIN()
      use unstruc_files
      implicit none
      integer :: k
      integer :: maxhlp
      integer :: numtxt
!     reads NUMTXT lines of HELPTEXT
      PARAMETER (MAXHLP = 2000)
      CHARACTER HLPTXT(MAXHLP)*80
      COMMON /HELPC/  HLPTXT,NUMTXT

      NUMTXT = 0
      IF (MHLP == 0) RETURN

      K    = 0

   10 CONTINUE
      K = K + 1
      READ(MHLP,'(A)',END = 9999) HLPTXT(K)
      GOTO 10

 9999 CONTINUE
      call doclose(mhlp)
      NUMTXT = K - 1

      RETURN
      END

      SUBROUTINE TIMLIN()
      implicit none
      CHARACTER TIME*5
!     CALL IOsTime(IH,IM,IS)
!     WRITE (TIME,'(I2,1A,I2)') IH,':',IM
!     IF (IM .LE. 9) WRITE (TIME(4:4),'(1A)') '0'
!     IXP = INFOSCREEN(2) - 4
!     IYP = INFOSCREEN(3) - 1
!     CALL ITEXTCOLOUR('BRED','CYAN')
!     CALL IOutStringXY(IXP,IYP,TIME)
      RETURN
      END


      ! Now a double precision (double precision ::)
      SUBROUTINE GETREAL(TEXT,VALUE)
      use m_devices
      USE M_MISSING
      implicit none
      integer :: infoattribute
      integer :: infoinput
      integer :: ixp
      integer :: iyp
      integer :: key
      integer :: nbckgr
      integer :: nforgr
      integer :: nlevel
      double precision :: val
      double precision :: value
      CHARACTER WRDKEY*40, TEXT*(*)
      COMMON /HELPNOW/   WRDKEY,NLEVEL

      VAL    = VALUE
      IXP    = IWS/2
      IYP    = IHS/2
      NFORGR = InfoAttribute(13)
      NBCKGR = InfoAttribute(14)
      CALL INPOPUP('ON')
   20 CONTINUE
      CALL ITEXTCOLOUR('BWHITE','RED')
      CALL INHIGHLIGHT('BLUE','BWHITE')
      CALL TIMLIN()
!      CALL INDOUBLEXYDEF(IXP,IYP,TEXT,1,VAL,6,'(F6.1)')
      CALL INDOUBLEXYDEF(IXP,IYP,TEXT,1,VAL,12,'(F12.1)')
      CALL TIMLIN()
      KEY = InfoInput(55)
      IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         NLEVEL = 3
         WRDKEY = TEXT
         CALL FKEYS(KEY)
         IF (KEY .EQ. 3) THEN
            CALL INPOPUP('OFF')
            CALL ITEXTCOLOURN(NFORGR,NBCKGR)
            RETURN
         ENDIF
         GOTO 20
      ELSE IF (KEY .EQ. 21 .OR. KEY .EQ. 22) THEN
         VALUE = VAL
      ELSE
         VALUE = dmiss
      ENDIF
      CALL INPOPUP('OFF')
      CALL ITEXTCOLOURN(NFORGR,NBCKGR)
      RETURN
      END


      SUBROUTINE GETINT(TEXT,IVAL)
      use m_devices
      USE M_MISSING
      implicit none
      integer :: infoattribute
      integer :: infoinput
      integer :: ixp
      integer :: iyp
      integer :: key
      integer :: nbckgr
      integer :: nforgr
      integer :: nlevel
      integer :: iv
      integer :: ival
      CHARACTER WRDKEY*40, TEXT*(*)
      COMMON /HELPNOW/   WRDKEY,NLEVEL

      IV     = IVAL
      IXP    = IWS/2
      IYP    = IHS/2
      NFORGR = InfoAttribute(13)
      NBCKGR = InfoAttribute(14)
      CALL INPOPUP('ON')
   20 CONTINUE
      CALL ITEXTCOLOUR('BWHITE','RED')
      CALL INHIGHLIGHT('BLUE','BWHITE')
      CALL TIMLIN()
!      CALL INDOUBLEXYDEF(IXP,IYP,TEXT,1,VAL,6,'(F6.1)')
      CALL ININTEGERXYDEF(IXP,IYP,TEXT,1,IV,12)
      CALL TIMLIN()
      KEY = InfoInput(55)
      IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         NLEVEL = 3
         WRDKEY = TEXT
         CALL FKEYS(KEY)
         IF (KEY .EQ. 3) THEN
            CALL INPOPUP('OFF')
            CALL ITEXTCOLOURN(NFORGR,NBCKGR)
            RETURN
         ENDIF
         GOTO 20
      ELSE IF (KEY .EQ. 21 .OR. KEY .EQ. 22) THEN
         IVAL = IV
      ELSE
         IVAL = int(dmiss)
      ENDIF
      CALL INPOPUP('OFF')
      CALL ITEXTCOLOURN(NFORGR,NBCKGR)
      RETURN
   END

   !> Get a string
   subroutine getstring(text, string)
   use m_devices
   implicit none
   character(len=*), intent(in)     :: text
   character(len=*), intent(out)    :: string
   
   integer :: infoattribute
   integer :: infoinput
   integer :: ixp
   integer :: iyp
   integer :: key
   integer :: nbckgr
   integer :: nforgr
   integer :: nlevel
   integer :: lstring
   character string_tmp*40
   character wrdkey*40
   common /helpnow/   wrdkey,nlevel
   
   ixp = iws/2
   iyp = ihs/2
   nforgr = InfoAttribute(13)
   nbckgr = InfoAttribute(14)
   
   call inpopup('on')
20 continue
   call itextcolour('bwhite', 'red')
   call inhighlight('blue', 'bwhite')
   call timlin()
   call InStringXYDef(ixp,iyp,text,1,string_tmp,lstring)
   call timlin
   key = InfoInput(55)
   if (key >=24 .and. key <= 26) then
      nlevel = 3
      wrdkey = text
      call fkeys(key)
      if (key == 3) then
         call inpopup('off')
         call itextcolourn(nforgr, nbckgr)
         return
      end if
      goto 20
   else if (key == 21 .or. key ==22) then
      string = string_tmp(1:lstring)
   else
      string = ''
   end if
   call inpopup('off')
   call itextcolourn(nforgr, nbckgr)
   return 
   end subroutine getstring

      ! Now a double precision (double precision ::)
      SUBROUTINE SHOWREAL(TEXT,VALUE)
use m_devices
implicit none
integer :: infoattribute
integer :: ixp
integer :: iyp
integer :: len
integer :: nbckgr
integer :: nforgr
integer :: nlevel
double precision :: val
double precision :: value
      CHARACTER WRDKEY*40, TEXT*(*)
      COMMON /HELPNOW/   WRDKEY,NLEVEL
      VAL    = VALUE
      IXP    = IWS/2
      IYP    = IHS/2
      NFORGR = InfoAttribute(13)
      NBCKGR = InfoAttribute(14)
      LEN    = len_trim(TEXT)
      CALL INPOPUP('ON')
      CALL ITEXTCOLOUR('BWHITE','BLUE')
!      CALL IWINOPEN(IXP,IYP,LEN+8,1)
      CALL IWINOPEN(IXP,IYP,LEN+11,1)
      CALL ITEXTCOLOUR('BBLUE','BWHITE')
      CALL IWINOUTSTRINGXY(1,1,TEXT)
!      CALL IWINOUTDOUBLEXY(1+LEN,1,VALUE,'(F8.1)')
      CALL IWINOUTDOUBLEXY(1+LEN,1,VALUE,'(F11.1)')
      CALL ITEXTCOLOURN(NFORGR,NBCKGR)
      RETURN
      END

      SUBROUTINE GETKEY(KEY)
      implicit none
      integer :: i
      integer :: infoinput
      integer :: key
      integer :: keynum
      integer :: nkey
      integer :: numc
      integer :: numkeys
      COMMON /NKEYS/ NUMKEYS, NKEY(20), NUMC(20)
      KEY    = InfoInput(57)
      KEYNUM = -999
      DO 10 I = 1,NUMKEYS
         IF (KEY .EQ. NKEY(I)) KEYNUM = I
   10 CONTINUE
      IF (KEYNUM .NE. -999) KEY = NUMC(KEYNUM)
      RETURN
      END

      SUBROUTINE GETKEY2(KEY)
      implicit none
      integer :: i
      integer :: key
      integer :: keynum
      integer :: nkey
      integer :: numc
      integer :: numkeys
      COMMON /NKEYS/ NUMKEYS, NKEY(20), NUMC(20)
      KEYNUM = -999
      DO 10 I = 1,NUMKEYS
         IF (KEY .EQ. NKEY(I)) KEYNUM = I
   10 CONTINUE
      IF (KEYNUM .NE. -999) KEY = NUMC(KEYNUM)
      RETURN
      END

      SUBROUTINE OSC(KEY)
      use m_devices
      use unstruc_messages
      implicit none
      integer :: infoinput
      integer :: ixp
      integer :: iyp
      integer :: key
      integer :: len
      integer :: nlevel
      CHARACTER STRING*58, WRDKEY*40
      IXP = 2
      IYP = 10
      IF (NOPSYS .EQ. 1) THEN
         CALL ISCREENMODE('T',80,25,16)
      ELSE
         RETURN
      ENDIF
   10 CONTINUE
!     CALL BOTLIN(0,1,KEY)
!     CALL ITEXTCOLOURN(MNUFOR,MNUBCK)
      CALL ITEXTCOLOUR('WHITE','BLUE')
      CALL INPOPUP('ON')
      CALL InStringXY(IXP,IYP,'enter OS-command ; ',1,STRING,LEN)
      CALL INPOPUP('OFF')
      KEY = InfoInput(55)
      IF (KEY .EQ. 24) THEN
         WRDKEY = 'OS-command'
         NLEVEL = 2
         CALL HELP(WRDKEY,NLEVEL)
      ELSE IF (KEY .EQ. 25) THEN
         CALL HISTOR()
      ELSE IF ((KEY .EQ. 21 .OR. KEY .EQ. 22) .AND. LEN .GE. 1) THEN
         WRITE(msgbuf,'(A,A)') 'OPERATING SYSTEM COMMAND: ',STRING(:LEN)
         call msg_flush()
         CALL IOsCommand(STRING(:LEN))
      ELSE IF (KEY .EQ. 23) THEN
         IF (NOPSYS .EQ. 1) CALL ISCREENMODE('GR',NPX,NPY,NCOLR)
         KEY = 3
         RETURN
      ENDIF
      GOTO 10
      END


      SUBROUTINE DENY(IXP,IYP)
      implicit none
      integer :: infoattribute
      integer :: ixp
      integer :: iyp
      integer :: nbckgr
      integer :: nforgr
      NFORGR = InfoAttribute(13)
      NBCKGR = InfoAttribute(14)
      CALL IWinAction('FPC')
      CALL ITEXTCOLOUR('BWHITE','RED')
      CALL IWinOpen(IXP+40,IYP+9,24,2)
      CALL IWinOutStringXY(1,1,'THIS FILE DOES NOT EXIST')
      CALL IWinOutStringXY(1,2,'CHOOSE ANOTHER OR EXIT')
      CALL TOEMAAR()
      CALL IWinClose(1)
      CALL ITEXTCOLOURN(NFORGR,NBCKGR)
      RETURN
      END



      SUBROUTINE STOPJA(JA)
      use unstruc_files
      use m_devices
      implicit none
      integer :: imenutwo
      integer :: infocursor
      integer :: iopt
      integer :: ixp
      integer :: iyp
      integer :: ja
      IXP = INFOCURSOR(1)
      IYP = INFOCURSOR(2)
      CALL INPOPUP('ON')
      CALL ITEXTCOLOUR('BWHITE','RED')
      CALL INHIGHLIGHT('BLUE','BWHITE')
      CALL OKAY(0)
      IOPT = IMenuTwo                                                &
       ('NO','YES',(IWS-41)/2,IHS/2,'DO YOU REALLY WANT TO '//       &
        'QUIT THE PROGRAM ? ',1,1)
      CALL INPOPUP('OFF')
      IF (IOPT .EQ. 1) THEN
         JA = 0
      ELSE
         WRITE(msgbuf,'(A)') 'YOU STOPPED THE PROGRAM'
         call msg_flush()
         CALL IWinClose(1)
         CALL STOPINT()
      ENDIF
      RETURN
      END

      SUBROUTINE TOEMAAR()
      implicit none
      integer :: key
      CALL OKAY(0)
      CALL TIMLIN()
   10 CONTINUE
      CALL INFLUSH()
      CALL INKEYEVENT(KEY)
      IF (KEY .EQ. 50  .OR. (KEY .GE. 254 .AND. KEY .LE. 259)) THEN
         GOTO 10
      ELSE IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         CALL FKEYS(KEY)
         GOTO 10
      ENDIF
      CALL TIMLIN()
      RETURN
      END

      SUBROUTINE HISTOR()
      use unstruc_files
      use unstruc_display
      use unstruc_version_module, only : unstruc_company, unstruc_program
      implicit none
      integer :: ih
      integer :: infoinput
      integer :: infowindow
      integer :: ipos
      integer :: iw
      integer :: ixp
      integer :: iyp
      integer :: j
      integer :: jatab
      integer :: jofnd
      integer :: k
      integer :: key
      integer :: kstart
      integer :: maxtxt
      integer :: nlevel
      integer :: numchc
      integer :: numtop
      integer :: numtxt
      integer :: numwnh

      PARAMETER (MAXTXT = 400)
      CHARACTER DIATXT(MAXTXT)*70,WRDKEY*40
      COMMON /HELPNOW/ WRDKEY,NLEVEL
!
      REWIND(MDIA)
      K = 0
   10 CONTINUE
      READ(MDIA,'(A)',END = 888)
      K = K + 1
      GOTO 10
  888 CONTINUE
      KSTART = K - MAXTXT + 2
      REWIND(MDIA)
!
      K = 0
      J = 1
   20 CONTINUE
      K = K + 1
      IF (K .GE. KSTART) THEN
         READ(MDIA,'(A)',END = 999) DIATXT(J)
         J = J + 1
      ELSE
         READ(MDIA,'(A)',END = 999)
      ENDIF
      GOTO 20
  999 CONTINUE
!
      BACKSPACE(MDIA)
      NUMTXT = J - 1
      JATAB  = 0
      JOFND  = 0
      NUMTOP = NUMTXT
      NUMCHC = NUMTXT
      NLEVEL = 1

!     Initialise
      CALL IWinWordWrap('OFF')
      CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
      CALL INHIGHLIGHT('WHITE','RED')
      IW     = NPOS(3)
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2)
      IH     = IHS - 9

!     Header of filewindow
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP,IW,1)
      CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
      CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program)// ' HISTORY')
      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!     Explain keyfunctions in bottom window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IHS-1,IW,2)
      CALL IWinOutStringXY (1,1,'move = ,Pgup, Pgdwn, home; quit = Esc')
!
!     Filewindow is middelste window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP+3,IW,IH)
!
      NUMWNH = InfoWindow(1)
      CALL IWinSelect(NUMWNH)
!  30 CONTINUE
!     CALL SCRLPG(DIATXT,NUMTXT,NUMTOP,NUMCHC,IH)
!     CALL SCROLH(NUMCHC,DIATXT,NUMTXT,NLEVEL,IH,JOFND,JATAB)
   50 CONTINUE
      IPOS = MAX(1,NUMTXT - 10)
      CALL IWINBROWSETEXT(DIATXT,NUMTXT,10,IPOS,' ')
      KEY = INFOINPUT(55)
      IF (KEY .EQ. 24) THEN
         CALL HELP(WRDKEY,NLEVEL)
         GOTO 50
      ENDIF
!     IF (NUMCHC .NE. 0) GOTO 30
      CALL IWinClose(1)
      CALL IWinClose(1)
      CALL IWinClose(1)
      RETURN
      END




      SUBROUTINE putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)
      implicit none
      integer :: ja
      integer :: key
      integer :: ndraw
      integer :: nput
      integer :: num
      integer :: numb
      integer :: nwhat
      double precision :: xp
      double precision :: yp
      COMMON /DRAWTHIS/  ndraw(50)

!
      CALL DISPUT(NPUT)

!     IF (KEY .EQ. 3) THEN
         CALL MENUH(0,NUM,NWHAT)
         CALL BOTLIN(0,NUMB,KEY)
         CALL FRAMES(31)
!     ENDIF

!
   20 CONTINUE
      CALL READLOCATOR(XP,YP,KEY)
!
      IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         CALL FKEYS(KEY)
         IF (KEY .EQ. 3) RETURN
      ELSE IF (KEY .EQ. 1) THEN
!        BOVEN
         JA = KEY
         CALL MENUH(JA,NUM,NWHAT)
         CALL BOTLIN(0,NUMB,KEY)
         IF (JA .NE. 0) RETURN
      ELSE IF (KEY .EQ. 2) THEN
!        ONDER
         JA = KEY
         CALL BOTLIN(JA,NUMB,KEY)
         IF (JA .NE. 0) RETURN
      ELSE IF (KEY .EQ. 90 .OR. KEY .EQ. 90+32) THEN
!        Z(oomin)
         CALL ZOOMIN(KEY,NPUT)
         RETURN
      ELSE IF (KEY .EQ. 65 .OR. KEY .EQ. 65+32) THEN
!        A(nchor)
         CALL ANCHOR(XP,YP)
       ELSE IF (KEY .EQ. 170 .OR. KEY .EQ. 80 .OR. KEY .EQ. 80+32) THEN
         NDRAW(10) = 1
         KEY = 3
         RETURN
      ELSE
         RETURN
      ENDIF
      GOTO 20
      END

      SUBROUTINE MENUH (JA,NUM,NWHAT)
      use m_devices
      implicit none
      integer :: ja
      integer :: num
      integer :: nwhat

      integer :: infoinput
      integer :: imenuhoriz
      integer :: iw
      integer :: key
      integer :: maxop
      integer :: maxopt
      integer :: nlevel
      PARAMETER (MAXOP = 20)
      CHARACTER*10 OPTION(MAXOP)
      CHARACTER WRDKEY*40
      COMMON /HELPNOW/ WRDKEY,NLEVEL
!
!     Keuzemenu horizontaal
!
      OPTION (1) = 'FILES     '
      OPTION (2) = 'OPERATIONS'
      OPTION (3) = 'DISPLAY   '
      OPTION (4) = 'EDIT      '
      OPTION (5) = 'ADDSUBDEL '
      OPTION (6) = 'VARIOUS   '
      MAXOPT     = 6
      KEY        = 0
!
      IW    = IWS
!
   10 CONTINUE
!
   20 CONTINUE
      IF (JA .EQ. 1) THEN
         CALL TIMLIN()
         CALL BOTLIN(0,1,KEY)
         IF (NOPSYS .EQ. 1) THEN
            CALL ITEXTCOLOUR('BBLUE','BWHITE')
         ELSE
            CALL ITEXTCOLOUR('BLACK','BWHITE')
         ENDIF
         CALL INHIGHLIGHT('BWHITE','RED')
         NUM    = IMenuHoriz(OPTION,MAXOPT,1,1,IW,0,1)
         CALL TIMLIN()
      ENDIF
      IF (NOPSYS .EQ. 1) THEN
         CALL InHighlight('BWHITE','WHITE')
         CALL ITEXTCOLOUR('BWHITE','WHITE')
      ELSE
         CALL InHighlight('BLACK','WHITE')
         CALL ITEXTCOLOUR('BLACK','WHITE')
      ENDIF
      CALL IOUTMenuHoriz(OPTION,MAXOPT,1,1,IW,0,1)
      IF (JA .NE. 1) RETURN
!
      KEY = InfoInput(55)
      IF (KEY .NE. 23) THEN
         NLEVEL = 1
         WRDKEY = OPTION(NUM)
      ENDIF
      IF (KEY .EQ. 21 .OR. KEY .EQ. 22) THEN
!        INS KEY
         CALL MENUV1(NUM,NWHAT)
         IF (NWHAT .EQ. 0) GOTO 20
         CALL IOUTSTRINGXY(1,2,' OPTION : '//WRDKEY)
         RETURN
      ELSE IF (KEY .EQ. 23 .OR. KEY .EQ. -2) THEN
!        ESC OR OUTSIDE
         NUM = 0
         RETURN
      ELSE
         CALL FKEYS(KEY)
         IF (KEY .EQ. 3) RETURN
      ENDIF
      GOTO 10
!
      END

      SUBROUTINE MENUV2(NWHAT,OPTION,MAXOPT,EXP,MAXEXP)
      use unstruc_files
      use m_devices
      implicit none
      integer :: imenuvertic
      integer :: infoinput
      integer :: infocursor
      integer :: ja, IXP, IYP
      integer :: key
      integer :: maxexp
      integer :: maxop
      integer :: maxopt
      integer :: nlevel
      integer :: nstart
      integer :: nwhat
      PARAMETER (MAXOP = 64)
      CHARACTER*40 OPTION(MAXOP),EXP(MAXOP),WRDKEY
      COMMON /HELPNOW/ WRDKEY,NLEVEL
!     Keuzemenu verticaal
!
      NSTART = NWHAT
   10 CONTINUE
      CALL BOTLIN(0,1,KEY)
!
      IXP = INFOCURSOR(1)
      IXP = INFOINPUT(62) - 1
      IYP = 2
      CALL TIMLIN()
      IF (NOPSYS .EQ. 1) THEN
         CALL ITEXTCOLOUR('BBLUE','BWHITE')
      ELSE
         CALL ITEXTCOLOUR('BLACK','BWHITE')
      ENDIF
      CALL INHIGHLIGHT('BWHITE','RED')
      CALL INPOPUP('ON')
      NWHAT  = IMENUVERTIC(OPTION,MAXOPT,IXP,IYP,' ',0,0,NSTART)
      CALL INPOPUP('OFF')
      CALL TIMLIN()
!
      KEY = InfoInput(55)
      IF (KEY .NE. 23) THEN
         NLEVEL = 2
         WRDKEY = OPTION(NWHAT)
      ENDIF

      IF (KEY .EQ. 21) THEN
!        INS KEY
         WRITE(msgbuf,'(A)') WRDKEY
         call msg_flush()
         JA = 0
         RETURN
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY
         JA = 0
         RETURN
      ELSE IF (KEY .EQ. 23 .OR. KEY .EQ. -2) THEN
!        ESC OR OUTSIDE
         JA    = 0
         NWHAT = 0
         RETURN
      ELSE IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         CALL FKEYS(KEY)
         IF (KEY .EQ. 3) RETURN
      ENDIF
      GOTO 10
!
      END

      SUBROUTINE MENUV3(NWHAT,OPTION,MAXOPT,EXP,MAXEXP)
      use unstruc_files
      use m_devices
      implicit none
      integer :: imenuvertic, IXP, IYP
      integer :: infoinput
      integer :: infocursor
      integer :: ja
      integer :: key
      integer :: maxexp
      integer :: maxop
      integer :: maxopt
      integer :: nlevel
      integer :: nstart
      integer :: nwhat
      PARAMETER (MAXOP = 64)
      CHARACTER*40 OPTION(MAXOP),EXP(MAXOP),WRDKEY
      COMMON /HELPNOW/ WRDKEY,NLEVEL
!     Keuzemenu verticaal
!
      NSTART = NWHAT
   10 CONTINUE
      CALL BOTLIN(0,1,KEY)
!
      IXP = INFOCURSOR(1)
      IXP = INFOINPUT(62) - 1
      IYP = 2
      CALL TIMLIN()
      IF (NOPSYS .EQ. 1) THEN
         CALL ITEXTCOLOUR('BBLUE','BWHITE')
      ELSE
         CALL ITEXTCOLOUR('BLACK','BWHITE')
      ENDIF
      CALL INHIGHLIGHT('BWHITE','RED')
      CALL INPOPUP('ON')
      NWHAT  = IMENUVERTIC(OPTION,MAXOPT,IXP,IYP,' ',0,0,NSTART)
      CALL INPOPUP('OFF')
      CALL TIMLIN()
!
      KEY = InfoInput(55)
      IF (KEY .NE. 23) THEN
         NLEVEL = 3
         WRDKEY = OPTION(NWHAT)
      ENDIF

      IF (KEY .EQ. 21) THEN
!        INS KEY
         WRITE(msgbuf,'(A)') WRDKEY
         call msg_flush()
         JA = 0
         RETURN
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY
         JA = 0
         RETURN
      ELSE IF (KEY .EQ. 23 .OR. KEY .EQ. -2) THEN
!        ESC OR OUTSIDE
         JA    = 0
         NWHAT = 0
         RETURN
      ELSE IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         CALL FKEYS(KEY)
         IF (KEY .EQ. 3) RETURN
      ENDIF
      GOTO 10
!
      END

      SUBROUTINE CHANGEISOPARAMETERS()
      use unstruc_display
      use unstruc_version_module, only : unstruc_company, unstruc_program

      implicit none
      double precision :: dv, dv2
      double precision :: dvi, dvi2
      double precision :: dvnu
      integer :: i
      integer :: ifexit
      integer :: ifinit
      integer :: ih
      integer :: il
      integer :: imp
      integer :: inp
      integer :: ir
      integer :: iw
      integer :: ixp
      integer :: iyp
      integer :: jaauto, jaauto2
      integer :: key
      integer :: nbut
      integer :: ncols, ncols2
      integer :: ndec
      Integer :: nie, nie2
      integer :: nien
      integer :: nis, nis2
      integer :: nisn
      integer :: nlevel
      integer :: numfld
      integer :: numpar
      integer :: nv, nv2
      integer :: nvn
      double precision :: scalesize
      double precision :: val, val2
      double precision :: vmax, vmax2
      double precision :: vmaxn
      double precision :: vmin, vmin2
      double precision :: vminn
      double precision :: xsc
      double precision :: ysc
      PARAMETER  (NUMPAR = 19, NUMFLD = 2*NUMPAR)
      INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
      CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
      COMMON /DEPMAX2/ VMAX2,VMIN2,DV2,VAL2(256),NCOLS2(256),NV2,NIS2,NIE2,JAAUTO2
      COMMON /SCALEPOS/ XSC,YSC,SCALESIZE,NDEC
      integer, external :: infoinput
      external :: highlight_form_line
!
      NLEVEL    = 3
      OPTION(1) = 'AUTOSCALE ON OR OFF                     '
      OPTION(2) = 'NUMBER OF ISOCOLOURS                    '
      OPTION(3) = 'MINIMUM ISOLINE VALUE                   '
      OPTION(4) = 'MAXIMUM ISOLINE VALUE                   '
      OPTION(5) = 'ISOLINE INTERVAL                        '
      OPTION(6) = 'COLOUR NUMBER OF FIRST COLOUR           '
      OPTION(7) = 'COLOUR NUMBER OF LAST  COLOUR           '
      OPTION(8) = 'X COOR LOWER LEFT CORNER OF LEGEND (0-1)'
      OPTION(9) = 'Y COOR LOWER LEFT CORNER OF LEGEND (0-1)'
      OPTION(10)= 'NUMBER OF DECIMALS COLOURSCALE LEGEND   '
      OPTION(11)= 'FONTSIZE COLOURSCALE LEGEND (0.5-1.5)   '
      OPTION(12)= 'Settings for secondary legend:          '
      OPTION(13)= '  AUTOSCALE ON OR OFF                   '
      OPTION(14)= '  NUMBER OF ISOCOLOURS                  '
      OPTION(15)= '  MINIMUM ISOLINE VALUE                 '
      OPTION(16)= '  MAXIMUM ISOLINE VALUE                 '
      OPTION(17)= '  ISOLINE INTERVAL                      '
      OPTION(18)= '  COLOUR NUMBER OF FIRST COLOUR         '
      OPTION(19)= '  COLOUR NUMBER OF LAST  COLOUR         '
!      123456789012345678901234567890123456789012345678901234567890
!               1         2         3         4         5         6
      HELPM (1) = 'INTEGER VALUE , AUTOSCALE OFF = 0, ON = 1                   '
      HELPM (2) = 'INTEGER VALUE =< 30                                         '
      HELPM (3) = 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
      HELPM (4) = 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
      HELPM (5) = 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
      HELPM (6) = 'INTEGER VALUE, STARTINDEX OF ISOCOLOURS (0-255) DEFAULT 46  '
      HELPM (7) = 'INTEGER VALUE, ENDINDEX OF ISOCOLOURS (0-255)   DEFAULT 224 '
      HELPM (8) = 'REAL VALUE (0-1), X COORDINATE LOWER LEFT CORNER LEGEND     '
      HELPM (9) = 'REAL VALUE (0-1), Y COORDINATE LOWER LEFT CORNER LEGEND     '
      HELPM (10)= 'INTEGER, NR OF DECIMALS IN COLOURSCALE LEGEND               '
      HELPM (11)= 'REAL VALUE, FONTSIZE OF COLOURSCALE LEGEND TEXT, DEFAULT 0.5'
      HELPM (12)= '                                                            '
      HELPM (13)= 'INTEGER VALUE , AUTOSCALE OFF = 0, ON = 1                   '
      HELPM (14)= 'INTEGER VALUE =< 30                                         '
      HELPM (15)= 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
      HELPM (16)= 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
      HELPM (17)= 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
      HELPM (18)= 'INTEGER VALUE, STARTINDEX OF ISOCOLOURS (0-255) DEFAULT 46  '
      HELPM (19)= 'INTEGER VALUE, ENDINDEX OF ISOCOLOURS (0-255)   DEFAULT 224 '

      IR = 0
      DO 10 I = 1,NUMPAR
         IL = IR + 1
         IR = IL + 1
         IX(IL) = 13
         IX(IR) = 95
         IY(IL) = 2*I
         IY(IR) = 2*I
         IS(IL) = 82
         IS(IR) = 10
         IT(IL) = 1001
         IF (I .GE. 3 .AND. I .LE. 5 .or. I .GE. 15 .AND. I .LE. 17 ) THEN
            ! Real values:
            IT(IR) = 6
         ELSE
            ! Integer values:
            IT(IR) = 2
         ENDIF
   10 CONTINUE
      IT(2*8)  = 6
      IT(2*9)  = 6
      IT(2*11) = 6

!     Initialise
      CALL IWinWordWrap('OFF')
      CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
      CALL INHIGHLIGHT('WHITE','RED')
      IW     = NPOS(3)
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2)
      IH     = IHS - 9

!     Header of filewindow
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP,IW,1)
      CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
      CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program) // ' ISOPARAMETER FORM')
      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!     Explain keyfunctions in bottom window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IHS-1,IW,2)
      CALL IWinOutStringXY(1,1,'move = , Tab, confirm = Enter, no change = Esc, help = F3')
      CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!     Filewindow is middelste window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP+3,IW,IH)

      CALL InControlKey(29,129)
      CALL InControlKey(30,128)

!     NUMWNH = InfoWindow(1)
!     CALL IWinSelect(NUMWNH)

!     Define a new form by supplying arrays containing
!     field positions, sizes and types
      CALL IFORMDEFINE('W',NUMFLD,IX,IY,IS,IT)

!     Define a help field and define help strings
!     for 2 of the 4 input fields
      CALL IFORMHELP(13,IH,60)

      IR = 0
      DO 20 I = 1,NUMPAR
         IL = IR + 1
         IR = IL + 1
         CALL IFORMPUTSTRING (IL,OPTION(I))
         CALL IFORMPUTHELP   (IR,HELPM(I))
         CALL IFORMATTRIBUTEN(IR,0,0,7)
   20 CONTINUE

      CALL IFORMPUTINTEGER( 2,JAAUTO)
      CALL IFORMPUTINTEGER( 4,NV)
      CALL IFormPutDouble ( 6,VMIN ,'(F10.3)')
      CALL IFormPutDouble ( 8,VMAX ,'(F10.3)')
      DVI = DV/(NV-1)
      CALL IFormPutDouble (10,DVI  ,'(F10.3)')
      CALL IFORMPUTINTEGER(12,NIS)
      CALL IFORMPUTINTEGER(14,NIE)
      CALL IFormPutDouble (16,XSC  ,'(F10.3)')
      CALL IFormPutDouble (18,YSC  ,'(F10.3)')
      CALL IFORMPUTINTEGER(20,NDEC)
      CALL IFormPutDouble (22,SCALESIZE ,'(F10.3)')
      ! 2nd isocolour legend:
      CALL IFORMPUTINTEGER(26,JAAUTO2)
      CALL IFORMPUTINTEGER(28,NV2)
      CALL IFormPutDouble (30,VMIN2 ,'(F10.3)')
      CALL IFormPutDouble (32,VMAX2 ,'(F10.3)')
      DVI2 = DV2/(NV2-1)
      CALL IFormPutDouble (34,DVI2  ,'(F10.3)')
      CALL IFORMPUTINTEGER(36,NIS2)
      CALL IFORMPUTINTEGER(38,NIE2)


!  Display the form with numeric fields left justified
!  and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

   30 CONTINUE
      IFINIT = IFEXIT
      CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!     check for Help, Confirm, Quit
      KEY = INFOINPUT(55)
      IF (KEY .EQ. -2) THEN
          NBUT = INFOINPUT(61)
          IF (NBUT .GE. 1) THEN
             IMP = INFOINPUT(62) + 1
             INP = INFOINPUT(63) + 1
             IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.    &
                 INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
                IF (NBUT .EQ. 1) THEN
                   KEY = 21
                ELSE
                   KEY = 22
                ENDIF
             ELSE
                KEY = 23
             ENDIF
          ENDIF
      ELSE IF (KEY .EQ. -1) THEN
         KEY = INFOINPUT(57)
      ENDIF
      IF (KEY .EQ. 26) THEN
          WRDKEY = OPTION(IFEXIT/2)
          CALL HELP(WRDKEY,NLEVEL)
      ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
          IF (KEY .EQ. 22) THEN
              CALL IFORMGETINTEGER( 2,JAAUTO)
              JAAUTO = MAX(0,MIN(JAAUTO,1) )
              CALL IFORMGETINTEGER( 4,NVN)
              CALL IFormGetDouble ( 6,VMINN)
              CALL IFormGetDouble ( 8,VMAXN)
              CALL IFormGetDouble (10,DVNU)
              CALL IFORMGETINTEGER(12,NISN)
              CALL IFORMGETINTEGER(14,NIEN)
              IF (NV .NE. NVN .OR. NIS .NE. NISN .OR. NIE .NE. NIEN)THEN
                 NV  = MAX(2,NVN)
                 NIS = MAX(1,MIN(NISN,250))
                 NIE = MAX(NIS+NV+1,MIN(NIEN,254))
              ENDIF

              CALL IFormGetDouble (16,XSC)
              CALL IFormGetDouble (18,YSC)
              XSC = MAX(0d0,MIN(XSC,1d0))
              YSC = MAX(0d0,MIN(YSC,1d0))
              CALL IFORMGETINTEGER(20,NDEC)
              IF (NDEC .GT. 7) NDEC = 7
              CALL IFormGetDouble(22,SCALESIZE)
              SCALESIZE = MAX(0d0,MIN(SCALESIZE,1d0))

              IF (DVNU .NE. DVI .OR. VMAXN .NE. VMAX .OR. VMINN .NE. VMIN  ) JAAUTO = 0

              IF (JAAUTO .EQ. 0) THEN
                 DV = (NV-1)*DVNU
                 IF (VMIN .NE. VMINN .AND. VMAX .NE. VMAXN) THEN
                    VMIN = VMINN
                    VMAX = VMAXN
                    DV   = VMAX - VMIN
                 ELSE IF (VMAX .NE. VMAXN) THEN
                    VMAX = VMAXN
                    VMIN = VMAX - DV
                 ELSE
                    VMIN = VMINN
                    VMAX = VMIN + DV
                 ENDIF
                 DO I = 1,NV
                    VAL(I) = VMIN + (I-1)*DV/(NV-1)
                 ENDDO
              ENDIF

              ! Secondary isocolour legend
              CALL IFORMGETINTEGER(26,JAAUTO2)
              JAAUTO2 = MAX(0,MIN(JAAUTO2,1) )
              CALL IFORMGETINTEGER(28,NVN)
              CALL IFormGetDouble (30,VMINN)
              CALL IFormGetDouble (32,VMAXN)
              CALL IFormGetDouble (34,DVNU)
              CALL IFORMGETINTEGER(36,NISN)
              CALL IFORMGETINTEGER(38,NIEN)
              IF (NV2 .NE. NVN .OR. NIS2 .NE. NISN .OR. NIE2 .NE. NIEN)THEN
                 NV2  = MAX(2,NVN)
                 NIS2 = MAX(1,MIN(NISN,250))
                 NIE2 = MAX(NIS2+NV2+1,MIN(NIEN,254))
              ENDIF

              IF (DVNU .NE. DVI2 .OR. VMAXN .NE. VMAX2 .OR. &
                  VMINN .NE. VMIN2  ) JAAUTO2 = 0

              IF (JAAUTO2 .EQ. 0) THEN
                 DV2 = (NV2-1)*DVNU
                 IF (VMIN2 .NE. VMINN .AND. VMAX2 .NE. VMAXN) THEN
                    VMIN2 = VMINN
                    VMAX2 = VMAXN
                    DV2   = VMAX2 - VMIN2
                 ELSE IF (VMAX2 .NE. VMAXN) THEN
                    VMAX2 = VMAXN
                    VMIN2 = VMAX2 - DV2
                 ELSE
                    VMIN2 = VMINN
                    VMAX2 = VMIN2 + DV2
                 ENDIF
                 DO I = 1,NV2
                    VAL2(I) = VMIN2 + (I-1)*DV2/(NV2-1)
                 ENDDO
              ENDIF
          ENDIF
          CALL IWinClose(1)
          CALL IWinClose(1)
          CALL IWinClose(1)
          RETURN
      ELSE IF (KEY .EQ. 21) THEN
         IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
             WRDKEY = HELPM(IFEXIT)
             CALL HELP(WRDKEY,NLEVEL)
         ENDIF
      ENDIF
      GOTO 30

      END


      SUBROUTINE SAVEKEYS()
      implicit none
      integer :: i
      integer :: infoinput
      integer :: keycod
      integer :: maxkey
      PARAMETER (MAXKEY = 50)
      COMMON /KEYCODES/ KEYCOD(MAXKEY)
      DO 10 I = 1,MAXKEY
         KEYCOD(I) = INFOINPUT(I)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE RESTOREKEYS()
      implicit none
      integer :: i
      integer :: keycod
      integer :: maxkey
      PARAMETER (MAXKEY = 50)
      COMMON /KEYCODES/ KEYCOD(MAXKEY)
      DO 10 I = 1,MAXKEY
         CALL INCONTROLKEY(I,KEYCOD(I))
   10 CONTINUE
      RETURN
      END

      SUBROUTINE FKEYS(KEY)
      implicit none
      integer :: key
      integer :: nlevel
      CHARACTER WRDKEY*40
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      IF (KEY .EQ. 24) THEN
!        F1
         CALL HELP(WRDKEY,NLEVEL)
      ELSE IF (KEY .EQ. 25) THEN
!        F2
         CALL HISTOR()
      ELSE IF (KEY .EQ. 26) THEN
!        F3
         CALL OSC(KEY)
      ENDIF
      RETURN
      END

      SUBROUTINE GIVEKEY(KEY)
      implicit none
      integer :: key
      CHARACTER TEX*14
      TEX = ' KEYPRESS=    '
      WRITE(TEX(11:14),'(I4)') KEY
      CALL KTEXT(TEX,1,3,15)
      RETURN
      END

      SUBROUTINE TEXTPARAMETERS()
      use unstruc_display
      use unstruc_version_module, only : unstruc_company, unstruc_program

      implicit none
      integer :: i
      integer :: ifexit
      integer :: ifinit
      integer :: ih
      Integer :: il
      integer :: imp
      integer :: inp
      integer :: ir
      integer :: iw
      integer :: ixp
      integer :: iyp
      integer :: key
      integer :: nbut
      integer :: nlevel
      integer :: numfld
      integer :: numpar
      double precision :: txtimsize
      double precision :: txtimx
      double precision :: txtimy
      integer, external :: infoinput
      external :: highlight_form_line

      PARAMETER  (NUMPAR = 9, NUMFLD = 2*NUMPAR)
      INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
      CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
      CHARACTER TXTIM*60

      COMMON /HELPNOW/ WRDKEY,NLEVEL
      COMMON /TEXTIM/  TXTIMSIZE, TXTIMX, TXTIMY, TXTIM
!
      NLEVEL    = 3
      OPTION(1) = 'LINE 1:'
      OPTION(2) = 'LINE 2:'
      OPTION(3) = 'LINE 3:'
      OPTION(4) = 'FNTSIZ:'
      OPTION(5) = 'XPOS  :'
      OPTION(6) = 'YPOS  :'
      OPTION(7) = 'SIZE TM'
      OPTION(8) = 'XPOS TM'
      OPTION(9) = 'YPOS TM'

!      123456789012345678901234567890123456789012345678901234567890
!               1         2         3         4         5         6
      HELPM (1) =  'FIRST TEXTLINE                                              '
      HELPM (2) =  'SECOND TEXTLINE                                             '
      HELPM (3) =  'THIRD TEXTLINE                                              '
      HELPM (4) =  'FONTSIZE, ONLY FOR TEXTLINES, DEFAULT FONTSIZE = 0.5        '
      HELPM (5) =  'RELATIVE SCREEN X POSITION, 0 = LEFT, 1 = RIGHT             '
      HELPM (6) =  'RELATIVE SCREEN Y POSITION, 0 = BOTTOM, 1 = TOP             '
      HELPM (7) =  'FONTSIZE, FOR TIME/DATE IN ANIMATE INCREMENTAL              '
      HELPM (8) =  'SCREEN X POSITION, FOR TIME/DATE IN ANIMATE INCREMENTAL     '
      HELPM (9) =  'SCREEN Y POSITION, FOR TIME/DATE IN ANIMATE INCREMENTAL     '

      IR = 0
      DO 10 I = 1,NUMPAR
         IL = IR + 1
         IR = IL + 1
         IX(IL) = 2
!         IX(IR) = 14
         IX(IR) = 56
         IY(IL) = 2*I
         IY(IR) = 2*I
!         IS(IL) = 40
         IS(IL) = 82
         IS(IR) = 60
         IT(IL) = 1001
         IT(IR) = 1
         IF (I .GE. 4) THEN
            IS(IR) = 5
            IT(IR) = 6
         ENDIF
   10 CONTINUE

      CALL SAVEKEYS()
!     Initialise
      CALL IWinWordWrap('OFF')
      CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
      CALL INHIGHLIGHT('WHITE','RED')
      IW     = NPOS(3)
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2)
      IH     = IHS - 9

!     Header of filewindow
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP,IW,1)
      CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
      CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program) // ' PARAMETER FORM')
      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!     Explain keyfunctions in bottom window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IHS-1,IW,2)
      CALL IWinOutStringXY(1,1,'move = , Tab, confirm = Enter, no change = Esc, help = F3')
      CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!     Filewindow is middelste window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP+3,IW,IH)

      CALL InControlKey(29,129)
      CALL InControlKey(30,128)

!     NUMWNH = InfoWindow(1)
!     CALL IWinSelect(NUMWNH)

!     Define a new form by supplying arrays containing
!     field positions, sizes and types
      CALL IFORMDEFINE('W',NUMFLD,IX,IY,IS,IT)

!     Define a help field and define help strings
!     for 2 of the 4 input fields
      CALL IFORMHELP(13,IH,60)

      IR = 0
      DO 20 I = 1,NUMPAR
         IL = IR + 1
         IR = IL + 1
         CALL IFORMPUTSTRING (IL,OPTION(I))
         CALL IFORMPUTHELP   (IR,HELPM(I))
         CALL IFORMATTRIBUTEN(IR,0,0,7)
   20 CONTINUE

      CALL IFORMPUTSTRING (2*1,TXLIN(1))
      CALL IFORMPUTSTRING (2*2,TXLIN(2))
      CALL IFORMPUTSTRING (2*3,TXLIN(3))
      CALL IFormPutDouble (2*4,TXSIZE,'(F5.2)')
      CALL IFormPutDouble (2*5,TXXpos   ,'(F5.2)')
      CALL IFormPutDouble (2*6,TXYpos   ,'(F5.2)')
      CALL IFormPutDouble (2*7,TXTIMSIZE,'(F5.2)')
      CALL IFormPutDouble (2*8,TXTIMX   ,'(F5.2)')
      CALL IFormPutDouble (2*9,TXTIMY   ,'(F5.2)')

!  Display the form with numeric fields left justified
!  and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

   30 CONTINUE
      IFINIT = IFEXIT
      CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!     check for Help, Confirm, Quit
      KEY = INFOINPUT(55)
      IF (KEY .EQ. -2) THEN
          NBUT = INFOINPUT(61)
          IF (NBUT .GE. 1) THEN
             IMP = INFOINPUT(62) + 1
             INP = INFOINPUT(63) + 1
             IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.    &
                 INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
                IF (NBUT .EQ. 1) THEN
                   KEY = 21
                ELSE
                   KEY = 22
                ENDIF
             ELSE
                KEY = 23
             ENDIF
          ENDIF
      ELSE IF (KEY .EQ. -1) THEN
         KEY = INFOINPUT(57)
      ENDIF
      IF (KEY .EQ. 26) THEN
          WRDKEY = OPTION(IFEXIT/2)
          CALL HELP(WRDKEY,NLEVEL)
      ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
          IF (KEY .EQ. 22) THEN
             CALL IFORMGETSTRING (2*1,TXLIN(1))
             CALL IFORMGETSTRING (2*2,TXLIN(2))
             CALL IFORMGETSTRING (2*3,TXLIN(3))
             CALL IFormGetDouble (2*4,TXSIZE  )
             CALL IFormGetDouble (2*5,TXXpos     )
             CALL IFormGetDouble (2*6,TXYpos     )
             CALL IFormGetDouble (2*7,TXTIMSIZE  )
             CALL IFormGetDouble (2*8,TXTIMX     )
             CALL IFormGetDouble (2*9,TXTIMY     )
             TXSIZE = MAX(0d0,MIN(TXSIZE,10d0))
             TXXpos = MAX(0d0,MIN(TXXpos,1d0))
             TXYpos = MAX(0d0,MIN(TXYpos,1d0))
          ENDIF
          CALL IWinClose(1)
          CALL IWinClose(1)
          CALL IWinClose(1)
          CALL RESTOREKEYS()
          RETURN
      ELSE IF (KEY .EQ. 21) THEN
         IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
             WRDKEY = HELPM(IFEXIT)
             CALL HELP(WRDKEY,NLEVEL)
         ENDIF
      ENDIF
      GOTO 30

      END

      SUBROUTINE QNMESSAGE(TEX)
      use unstruc_display
      use unstruc_messages
      implicit none
      integer :: ih
      integer :: iw
      integer :: ixp
      integer :: iyp
      CHARACTER TEX*(*)

      IW     = NPOS(3)
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2)
      IH     = IHS - 9

      WRITE (msgbuf,'(A)') TEX
      call msg_flush()

      CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IHS-1,IW,2)
      CALL IWINOUTCENTRE(1,TEX)
      CALL IWINOUTCENTRE(2,'press F2 to read this message')
      CALL IOSWAIT(200)
      CALL IWinClose(1)

      RETURN
      END

      SUBROUTINE QNMESSAGEWAIT(TEX)
      use unstruc_messages
      use unstruc_display
      implicit none
      integer :: ih
      integer :: iw
      integer :: ixp
      integer :: iyp
      CHARACTER TEX*(*)

      IW     = NPOS(3)
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2)
      IH     = IHS - 9

      WRITE (msgbuf,'(A)') TEX
      call msg_flush()

      CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IHS-1,IW,2)
      CALL IWINOUTCENTRE(1,TEX)
      CALL IWINOUTCENTRE(2,'this message will also appear in HISTORY (F2)')
      CALL WAIT()
      CALL IWinClose(1)

      RETURN
      END




      SUBROUTINE BOTLIN(JA,NUMB,KEY)
      use m_devices
      use unstruc_display
      implicit none
      integer :: imenuhoriz
      integer :: infoinput
      integer :: iw
      integer :: ja
      integer :: key
      integer :: li
      integer :: maxop
      integer :: maxopt
      integer :: nlevel
      integer :: nput
      integer :: numb
      integer :: nwhat
      PARAMETER (MAXOP = 64)
      CHARACTER*14 OPTION(MAXOP), TEX*14
      CHARACTER WRDKEY*40
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      integer, save :: lastmenuheight = 1

   10 CONTINUE
      LI = IHS
      IW = IWS
      if (lastmenuheight == 2) then
        CALL ITEXTCOLOUR('BLACK','WHITE')
        call IClearLine(IHS-1) ! Clear second-last line to erase old menus.
      end if
      lastmenuheight = 1 ! Default (only some are two lines)
      IF (NUMB .EQ. 0) THEN
         OPTION(1) =  'CONTINUE     ;'
         OPTION(2) =  'F1 = help    ;'
         OPTION(3) =  'F2 = history ;'
         OPTION(4) =  'P/PRINTSCREEN;'
         OPTION(5) =  'STOP         ;'
         MAXOPT = 5
      ELSE IF (NUMB .EQ. 1) THEN
         OPTION(1) =  ' = choose  ;'
         OPTION(2) =  'F1 = help    ;'
         OPTION(3) =  'F2 = history ;'
         OPTION(4) =  'Esc= exit    ;'
         MAXOPT    =  4
      ELSE IF (NUMB .EQ. 2) THEN
         if (jafullbottomline== 1) then
         OPTION(1) =  'A   = ANCHOR ;'
         OPTION(2) =  'I   = INSERT ;'
         OPTION(3) =  'R   = REPLACE;'
         OPTION(4) =  'D   = DELETE ;'
         OPTION(5) =  'X   = SPLIT  ;'
         OPTION(6) =  'e   = ERASEPL;'
         OPTION(7) =  'E   = invERAS;'
         OPTION(8) =  'F   = REFINE ;'
         OPTION(9) =  'M   = MERGE  ;'
         OPTION(10)=  'L   = TO LAND;'
         OPTION(11)=  'N   = TO NET ;'
         OPTION(12)=  'w/W = dropwat;'
         OPTION(13)=  'b/B = droplnd;'
         OPTION(14)=  'TAB = DCURSOR;'
         OPTION(15)=  'ESC = UNDO   ;'
         OPTION(16)=  'Z   = ZOOMIN ;'
         maxopt    = 16
         lastmenuheight = 2
         else
         OPTION(1) =  'I=INS '
         OPTION(2) =  'R=REPL '
         OPTION(3) =  'D=DEL '
         OPTION(4) =  'X=SPLIT '
         OPTION(5) =  'e=ERAS '
         OPTION(6) =  'E=inve '
         OPTION(7) =  'F=REF '
         OPTION(8) =  'M=MERG '
         OPTION(9) =  'L=TOLA '
         OPTION(10)=  'N=TONE '
         OPTION(11)=  'w/W=wat '
         OPTION(12)=  'b/B=lnd '
         maxopt    = min(12,iws/12) 
         endif
      ELSE IF (NUMB .EQ. 3) THEN
         OPTION(1) =  'A   = ANCHOR ;'
         OPTION(2) =  'TAB = DCURSOR;'
         OPTION(3) =  'ESC = UNDO   ;'
         OPTION(4) =  'Z   = ZOOMIN ;'
         MAXOPT    =  4
      ELSE IF (NUMB .EQ. 4) THEN
         OPTION(1) =  '+   = DEEPER ;'
         OPTION(2) =  '-   = SHALLOW;'
         OPTION(3) =  'ESC = UNDO   ;'
         OPTION(4) =  'Z   = ZOOMIN ;'
         MAXOPT    =  4
      ELSE IF (NUMB .EQ. 5) THEN
         OPTION(1) =  'LMS = WINDOW ;'
         OPTION(2) =  'RMS = DEFAULT;'
         OPTION(3) =  'Z = ZOOM OUT ;'
         OPTION(4) =  '+   = LARGER ;'
         OPTION(5) =  '-   = SMALLER;'
         OPTION(6) =  'ESC = UNDO   ;'
         MAXOPT    =  6
      ELSE IF (NUMB .EQ. 6) THEN
!        editgridlineBLOK
         OPTION(1) =  'F1 = help    ;'
         OPTION(2) =  'F2 = history ;'
         OPTION(3) =  'P/PRINTSCREEN;'
         OPTION(4) =  'ESC = UNDO   ;'
         OPTION(5) =  'CLICK GRIDLINE'
         OPTION(6) =  'AND INFLUENCE '
         OPTION(7) =  'READY=RIGHT MS'
         MAXOPT    =  7
      ELSE IF (NUMB .EQ. 7) THEN
!        editgridshift
         OPTION(1) =  'F1 = help    ;'
         OPTION(2) =  'F2 = history ;'
         OPTION(3) =  'P/PRINTSCREEN;'
         OPTION(4) =  'ESC = UNDO   ;'
         OPTION(5) =  'SHIFT THE     '
         OPTION(6) =  'INDICATED LINE'
         OPTION(7) =  'READY=RIGHT MS'
         MAXOPT    =  7
      ELSE IF (NUMB .EQ. 8) THEN
!        editgridBLOK
         OPTION(1) =  'F1 = help    ;'
         OPTION(2) =  'F2 = history ;'
         OPTION(3) =  'P/PRINTSCREEN;'
         OPTION(4) =  'ESC = UNDO   ;'
         OPTION(5) =  'CLICK A BLOCK;'
         OPTION(6) =  'READY=RIGHT MS'
         MAXOPT    =  6
      ELSE IF (NUMB .EQ. 9) THEN
         OPTION(1) =  'A   = ANCHOR ;'
         OPTION(2) =  'I   = INSERT ;'
         OPTION(3) =  'R   = REPLACE;'
         OPTION(4) =  'D   = DELETE ;'
         OPTION(5) =  'ESC = UNDO   ;'
         OPTION(6) =  'Z   = ZOOMIN ;'
         OPTION(7) =  'NEW SPLINE RM;'
         OPTION(8) =  'C   = COPY   ;'
         OPTION(9) =  'M   = MOVE   ;'
         OPTION(10) = 'X   = DEL SPL;'
         OPTION(11) = 'L   = TO LAND;'
         MAXOPT    =  11
      ELSE IF (NUMB .EQ. 10) THEN
         OPTION(1) =  'A = ANCHOR; '
         OPTION(2) =  'I = INSERT; '
         OPTION(3) =  'R = REPLACE;'
         OPTION(4) =  'D = DELETE; '
         OPTION(5) =  'M = MERGE;  '  ! FFFFF
         OPTION(6) =  'G = NET2CURV' ! FFFFF
         OPTION(7) =  'C = CUT;    '
         OPTION(8) =  'X = DELCON; '
         OPTION(9) =  'S = SPLIT; '
         OPTION(10) = 'V = FIELDMOVE' ! fieldmove
         OPTION(11) = 'B = FLDROTATE' ! fieldrotate
         OPTION(12) = '1...4 = KN3;' ! kn(3,:) change 1/2/3/4
         OPTION(13) = 'L = TO LAND;' ! snap to land boundary
         OPTION(14) = 'k = KILL CELL;' ! delete cell and update administration
         OPTION(15) = 'K = DEREFINE; ' ! derefine by 'Casulli-type' killcell
         OPTION(16) = 'E = add layer; '   ! add layer of cells
         lastmenuheight = 2
         MAXOPT    = 16
      ELSE IF (NUMB .EQ. 11) THEN
         OPTION(1) =  '+   = INCREAS;'
         OPTION(2) =  '-   = DECREAS;'
         OPTION(3) =  'ESC = UNDO   ;'
         OPTION(4) =  'SPACE BAR =  ;'
         MAXOPT    =  4
      ELSE IF (NUMB .EQ. 12) THEN
         OPTION(1) =  'A   = ANCHOR ;'
         OPTION(2) =  'I   = INSERT ;'
         OPTION(3) =  'R   = REPLACE;'
         OPTION(4) =  'D   = DELETE ;'
         OPTION(5) =  'C   = CHANGEV;'
         OPTION(6) =  'm   = SET MIN;'
         OPTION(7) =  'M   = SET MAX;'
         OPTION(8) =  'H = hide/show;'
         OPTION(9) =  'ESC = UNDO   ;'
         OPTION(10)=  'Z   = ZOOMIN ;'
         OPTION(11)=  'Q   = sampath;'
         OPTION(12)=  'F   = fldfill;'
         MAXOPT    =  12
      ELSE IF (NUMB .EQ. 13) THEN
         OPTION(1) =  'A   = ANCHOR ;'
         OPTION(2) =  'I   = INSERTD;'
         OPTION(3) =  'R   = REPLACD;'
         OPTION(4) =  'D   = DELETED;'
         OPTION(5) =  'ESC = UNDO   ;'
         OPTION(6) =  'Z   = ZOOMIN ;'
         MAXOPT    =  6
      ELSE IF (NUMB .EQ. 14) THEN
!        colourchange
         OPTION(1) =  'LEFT MOUSE =  '
         OPTION(2) =  'INDICATE COLOR'
         OPTION(3) =  'RIGHT MOUSE = '
         OPTION(4) =  'CHANGE PALETTE'
         OPTION(5) =  'ESC = UNDO   ;'
         OPTION(6) =  'Z   = ZOOMIN ;'
         MAXOPT    =  6
      ELSE IF (NUMB .EQ. 15) THEN
         OPTION(1) =  'A   = ANCHOR ;'
         OPTION(2) =  '+   = +1 HOUR;'
         OPTION(3) =  '   SPACE BAR ='
         OPTION(4) =  'CONTINUE     ;'
         OPTION(5) =  'Yes SAVEIMAGES'
         OPTION(6) =  'No SAVEIMAGES '
         MAXOPT    =  6
      ELSE IF (NUMB .EQ. 16) THEN    ! editflow
         OPTION(1) =  'A = ANCHOR; '
         OPTION(2) =  'N = Node; '
         OPTION(3) =  'L = Link; '
         OPTION(4) =  'm = SET MIN;'
         OPTION(5) =  'M = SET MAX;'
         OPTION(6) =  'Z = ZOOMIN; '
         OPTION(7) =  'F = FIND link'
         OPTION(8) =  'H = FIND stru'
         MAXOPT    =  8
      ELSE IF (NUMB .EQ. 17) THEN    ! editgrid
         OPTION(1) =  'B = BELL; '
         OPTION(2) =  'D = DELETE; '
         OPTION(3) =  'I = INSERT; '
         OPTION(4) =  'R = REPLACE;'
         MAXOPT    =  4
      ENDIF

      IF (JA .EQ. 2) THEN
         CALL TIMLIN()
         IF (NOPSYS .EQ. 1) THEN
            CALL ITEXTCOLOUR('BBLUE','BWHITE')
         ELSE
            CALL ITEXTCOLOUR('BLACK','BWHITE')
         ENDIF
         CALL INHIGHLIGHT('BWHITE','RED')
         NWHAT  = IMenuHoriz(OPTION,MAXOPT,1,LI,IW,0,1)
         CALL TIMLIN()
      ENDIF
      IF (NOPSYS .EQ. 1) THEN
         CALL InHighlight('BWHITE','WHITE')
         CALL ITEXTCOLOUR('BWHITE','WHITE')
      ELSE
         CALL InHighlight('BLACK','WHITE')
         CALL ITEXTCOLOUR('BLACK','WHITE')
      ENDIF
      CALL IOUTMenuHoriz(OPTION,MAXOPT,1,LI,IW,0,1)
      IF (JA .NE. 2) RETURN


      KEY = InfoInput(55)
      IF (KEY .NE. 23) THEN
         NLEVEL = 3
         WRDKEY = OPTION(NWHAT)
      ENDIF

      IF (KEY .EQ. 21) THEN
!        ins, linker muis
         IF (NWHAT .GE. 1) THEN
            IF (OPTION(NWHAT) .EQ. 'F1 = help    ;') THEN
               KEY = 24
            ELSE IF (OPTION(NWHAT) .EQ. 'F2 = history ;') THEN
               KEY = 25
            ELSE IF (OPTION(NWHAT) .EQ. 'F3 = command ;') THEN
               KEY = 26
            ELSE IF (OPTION(NWHAT) .EQ. 'ESC = UNDO   ;') THEN
               KEY = 23
            ELSE IF (OPTION(NWHAT) .EQ. 'TAB = DCURSOR;') THEN
               KEY = 27
            ELSE IF (OPTION(NWHAT) .EQ. '+   = DEEPER ;') THEN
               KEY = 162
            ELSE IF (OPTION(NWHAT) .EQ. '-   = SHALLOW;') THEN
               KEY = 160
            ELSE IF (OPTION(NWHAT) .EQ. 'P/PRINTSCREEN;') THEN
               KEY = 80
            ELSE IF (OPTION(NWHAT) .EQ. 'DEL = CYCLE  ;') THEN
               KEY = 143
            ELSE IF (OPTION(NWHAT) .EQ. 'No SAVEIMAGES ') THEN
               KEY = 110
            ELSE IF (OPTION(NWHAT) .EQ. 'Yes SAVEIMAGES') THEN
               KEY = 121
            ELSE IF (OPTION(NWHAT) .EQ. 'Z   = ZOOMIN ;') THEN
               KEY  = 90
               NPUT = 2
               CALL ZOOM3(KEY,NPUT)
            ELSE IF (OPTION(NWHAT) .EQ. 'STOP         ;') THEN
               CALL STOPINT
               ! CALL STOPLOGO()
            ELSE
               KEY    = ICHAR( OPTION(NWHAT)(1:1) )
            ENDIF
         ENDIF
         TEX    = ' ACTIONKEY    '
         WRITE(TEX(12:14),'(I3)') KEY
         CALL KTEXT(TEX,1,5,15)
         RETURN
      ELSE IF (KEY .EQ. 23) THEN
!        ESC
         RETURN
      ELSE IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         CALL FKEYS(KEY)
         IF (KEY .EQ. 3) RETURN
         GOTO 10
      ELSE
         KEY = 0
         RETURN
      ENDIF

      END



      SUBROUTINE ARROWSXYzfac(X0,Y0,UX,UY,VFAC,JW,zfac)
      implicit none
      integer :: i
      integer :: jw
      double precision :: X0,Y0,UX,UY,VFAC,zfac

      IF (UX .EQ. 0 .AND. UY .EQ. 0) RETURN

      uy = uy

      CALL MOVABS(X0,Y0)
      CALL LNABS(x0+ux*vfac,y0+uy*vfac*zfac)
      RETURN
      END

      SUBROUTINE ARROWSxy(X0,Y0,UR,VR,VFAC)
      implicit none
      double precision :: alfa
      double precision :: csa
      integer :: i
      double precision :: psi0
      double precision :: sna
      double precision :: ur
      double precision :: vfac
      double precision :: vr
      double precision :: x0
      double precision :: xlen
      double precision :: y0
      double precision :: X(3), Y(3), XR(3), YR(3)
      DATA X(1)  /0.8d0/, X(2) /1d0/, X(3) /0.8d0/,  &
           Y(1) /-0.1d0/, Y(2) /0d0/, Y(3) /0.1d0/

      IF (UR .EQ. 0 .AND. VR .EQ. 0) RETURN

      DO 10 I = 1,3
         XR(I) = X0 + VFAC*(X(I)*UR - Y(I)*VR)
         YR(I) = Y0 + VFAC*(Y(I)*UR + X(I)*VR)
   10 CONTINUE

      CALL MOVABS(X0,Y0)
      CALL LNABS(XR(2),YR(2))
      CALL LNABS(XR(1),YR(1))

      CALL MOVABS(XR(2),YR(2))
      CALL LNABS(XR(3),YR(3))
      RETURN
      END

      SUBROUTINE ARROWS(X0,Y0,UR,VR,PSI0,VFAC)
      implicit none
      double precision :: alfa
      double precision :: csa
      integer :: i
      double precision :: psi0
      double precision :: sna
      double precision :: ur
      double precision :: vfac
      double precision :: vr
      double precision :: x0
      double precision :: xlen
      double precision :: y0
      double precision :: X(3), Y(3), XR(3), YR(3)
      DATA X(1)  /0.8d0/, X(2) /1d0/, X(3) /0.8d0/,  &
           Y(1) /-0.1d0/, Y(2) /0d0/, Y(3) /0.1d0/

      IF (UR .EQ. 0 .AND. VR .EQ. 0) RETURN
      ALFA = ATAN2(VR,UR) + PSI0
      CSA  = COS(ALFA)
      SNA  = SIN(ALFA)
      XLEN = SQRT(UR*UR+VR*VR)

      DO 10 I = 1,3
         XR(I) = X0 + VFAC*XLEN*(X(I)*CSA - Y(I)*SNA)
         YR(I) = Y0 + VFAC*XLEN*(Y(I)*CSA + X(I)*SNA)
   10 CONTINUE

      CALL MOVABS(X0,Y0)
      CALL LNABS(XR(2),YR(2))
      CALL LNABS(XR(1),YR(1))

      CALL MOVABS(XR(2),YR(2))
      CALL LNABS(XR(3),YR(3))
      RETURN
      END

      SUBROUTINE ARROWrcir(X0,Y0,cs,sn)
      USE M_WEARELT
      implicit none
      double precision :: cs
      integer :: i
      double precision :: sn
      double precision :: x0
      double precision :: y0
      double precision :: X(3), Y(3), XR(3), YR(3)
      DATA X(1)  /0.8d0/, X(2) /1d0/, X(3) /0.8d0/,  &
           Y(1) /-0.1d0/, Y(2) /0d0/, Y(3) /0.1d0/

      DO 10 I = 1,3
         XR(I) = X0 + 3*rcir*(X(I)*CS - Y(I)*SN)
         YR(I) = Y0 + 3*rcir*(Y(I)*CS + X(I)*SN)
   10 CONTINUE

      CALL MOVABS(X0,Y0)
      CALL LNABS(XR(2),YR(2))
      CALL LNABS(XR(1),YR(1))

      CALL MOVABS(XR(2),YR(2))
      CALL LNABS(XR(3),YR(3))
      RETURN
      END


      SUBROUTINE FILEMENU(MRGF,FILNAM)
      use unstruc_display
      use unstruc_version_module, only : unstruc_company, unstruc_program
      use unstruc_files, only : filnammenu
      implicit none
      integer :: ih, ihl, imenuscroll, imp, infoinput, inp, iw, ixp, iyp, jatab, jazekr, keepstartdir, key, l, len
      integer :: maxfil, maxhlp, mrgf, nahead, nbut, nlevel, numdir, numf, numfil, numtop, numtxi, numtxt

!     Gives menu with files filnam
!     call with mrgf = 0 means LOAD, mrgf = 1  means SAVE, mrgf = 2 means get filename only
!     return value -2 = old files not found, -1 = ESC
      PARAMETER (MAXHLP = 2000, MAXFIL = 2000)
      INTEGER IFDATE(MAXFIL), IFSIZE(MAXFIL)
      CHARACTER  HLPTXT(MAXHLP)*80,FILIST(MAXFIL)*86,FILNAM*86,WRDKEY*40
      CHARACTER  DIR*86, CURDIR*86, DIR2*86,FILNAM2*86
      LOGICAL JA
      COMMON /HELPC/     HLPTXT,NUMTXT
      COMMON /STARTDIR/  KEEPSTARTDIR
      
      integer jaopen ! open file (1) or not (0)
      
      filnammenu = ' '  
      jaopen = 1
      if ( mrgf.eq.2 ) then
         mrgf = 0
         jaopen = 0
      end if

      
!     Initialise
      CALL IWinWordWrap('OFF')
      CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
      CALL INHIGHLIGHT('WHITE','RED')

      L      = INDEX(FILNAM,'.')
      IW     = NPOS(3)
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2)
      IH     = IHS - 9

      IHL    = IH - 1
      NUMTXI = NUMTXT - IHL
      NAHEAD = 1
      NUMTOP = 1
      NUMF   = 1
      JAZEKR = 0
      JATAB  = 0
!
      CALL IOSDIRNAME(CURDIR)
      DIR = CURDIR
!
!     Header of filewindow
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP,IW,1)
      CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
      CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program)// ' FILEMENU')
      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!     Explain keyfunctions in bottom window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IHS-1,IW,2)
      CALL IWinOutStringXY(1,1,'Up or down arrow; confirm = Enter/left,right mouse;')
      CALL IWinOutStringXY(1,2,'help = F1; toggle between fields = Tab; quit = Esc')
!
!     Filewindow is middelste window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP+3,IW,IH)
!
      CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
      CALL IWinOutStringXY(2,7,'NAME / DIRECTORY                                               SIZE        DATE   TIME')
!

      IF (MRGF .EQ. 0) THEN
         CALL IOutStringXY(IXP+1,IYP+3,'LOAD FILENAME')
      ELSE
         CALL IOutStringXY(IXP+1,IYP+3,'SAVE FILENAME')
      ENDIF
      L = len_trim(FILNAM)
      CALL IOutStringXY(IXP+15,IYP+3,'('//FILNAM(1:L)//')')

      CALL IOutStringXY(IXP+1,IYP+6,'DIRECTORY')

      CALL ITEXTCOLOUR('BWHITE','BLU')
      CALL IOutStringXY(IXP+1,IYP+4, FILNAM)
      CALL IOutStringXY(IXP+1,IYP+7, DIR)
      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)

!     CALL IOutStringXY(IXP+47,IYP+6,'choose file in LEFT WINDOW')
!     CALL IOutStringXY(IXP+47,IYP+7,'or use TAB to toggle to')
!     CALL ITEXTCOLOUR('WHITE','BBLU')
!     CALL IOutStringXY(IXP+54,IYP+7,'TAB')
!     CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!     CALL IOutStringXY(IXP+47,IYP+8,'NAME or DIRECTORY')

   20 CONTINUE
      CALL UPDATEFILES(FILNAM,FILIST,NUMFIL,NUMDIR,IFDATE,IFSIZE,IXP,IYP,IH)

      CALL TIMLIN()
      IF (JATAB .EQ. 0) THEN
         CALL ITEXTCOLOUR('BWHITE','BLU')
         CALL INHIGHLIGHT('BLACK','WHITE')
         NUMF=IMenuScroll(FILIST,NUMFIL,IXP,IYP+10,' ',IH-7,0,NUMF)
      ELSE IF (JATAB .EQ. 1) THEN
         CALL INHIGHLIGHT('BLACK','WHITE')
         FILNAM2 = FILNAM
         CALL InStringXYDEF(IXP+1,IYP+4,' ',0,FILNAM2,LEN)
         CALL ITEXTCOLOUR('BWHITE','BLU')
         CALL IOutStringXY(IXP+1,IYP+4,FILNAM2)
         IF (INDEX(FILNAM2,'*') .NE. 0) THEN
            IF (FILNAM2 .NE. FILNAM) THEN
               FILNAM = FILNAM2
               JATAB  = 0
               GOTO 20
            ENDIF
         ELSE
            FILNAM = FILNAM2
         ENDIF
      ELSE IF (JATAB .EQ. 2) THEN
         DIR2 = DIR
         CALL INHIGHLIGHT('BLACK','WHITE')
         CALL InStringXYDEF(IXP+1,IYP+7,' ',0,DIR2,LEN)
         CALL ITEXTCOLOUR('BWHITE','BLU')
         CALL IOutStringXY(IXP+1,IYP+7,DIR2)
         IF (DIR2 .NE. DIR) THEN
            CALL IOSDIRCHANGE(DIR2)
            DIR = ' '
            CALL IOSDIRNAME(DIR)
!           IF (INFOERROR(3) .NE. 0) THEN
            IF (DIR .NE. DIR2) THEN
               CALL QNERROR('DIRECTORY',DIR2,'DOES NOT EXIST')
            ELSE
!              DIR   = DIR2
               CALL ITEXTCOLOUR('BWHITE','BLU')
               CALL IOutStringXY(IXP+1,IYP+7, DIR)
               JATAB = 0
            ENDIF
            GOTO 20
         ENDIF
      ENDIF

      CALL TIMLIN()
!
      KEY = InfoInput(55)

      IF (KEY .EQ. -2) THEN
          NBUT = INFOINPUT(61)
          IF (NBUT .GE. 1) THEN
             IMP = INFOINPUT(62) + 1
             INP = INFOINPUT(63) + 1
             IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.    &
                 INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
                 IF (INP .LE. 7) THEN
                    JATAB = 1
                 ELSE IF (INP .LE. 10) THEN
                    JATAB = 2
                 ELSE IF (INP .GE. 12) THEN
                    JATAB = 0
                 ENDIF
             ELSE
                KEY = 23   ! Buiten scherm = Esc
             ENDIF
          ENDIF
      ELSE IF (KEY .EQ. -1) THEN
         KEY = INFOINPUT(57)
      ENDIF

      IF (KEY .EQ. 24) THEN        ! F1 = HELP
         NLEVEL = 1
         WRDKEY = 'FILE-MENU INSTRUCTIONS'
         CALL HELP(WRDKEY,NLEVEL)
      ELSE IF (KEY .EQ. 23) THEN   ! Esc
         MRGF = -1
         GOTO 9999
      ELSE IF (KEY .EQ. 27) THEN   ! Tab
         JATAB = JATAB + 1
         IF (JATAB .EQ. 3) JATAB = 0
      ELSE IF (KEY .EQ. 21 .OR. KEY .EQ. 22) THEN   !Linker of rechter muis

         IF (JATAB .EQ. 0) THEN
            IF (NUMF .LE. NUMDIR) THEN
               CALL IOSDIRCHANGE( FILIST(NUMF)(1:54) )
               DIR = ' '
               CALL IOSDIRNAME(DIR)
               CALL ITEXTCOLOUR('BWHITE','BLU')
               CALL IOutStringXY(IXP+1,IYP+7, DIR)
               GOTO 20
            ELSE
               WRITE(FILNAM,'(A)') FILIST(NUMF)(1:54)
            ENDIF
         ENDIF

         L = len_trim(FILNAM)
         IF (L .EQ. 0) GOTO 20
         INQUIRE (FILE = FILNAM(1:L), EXIST = JA)

         IF (MRGF .EQ. 0) THEN
            IF (.NOT. JA) THEN
               CALL DENY(IXP,IYP)
            ELSE
               JAZEKR = 1
            ENDIF
         ELSE IF (MRGF .EQ. 1) THEN
            IF (JA) THEN
               CALL CONFRM(' FILE ALREADY EXISTS. OVERWRITE ANYWAY ? ', JAZEKR)
            ELSE
               JAZEKR = 1
            ENDIF
         ENDIF
!
         IF (JAZEKR .EQ. 1) THEN
            IF (INDEX(FILNAM,'*') .NE. 0) GOTO 20
            IF (DIR .NE. CURDIR) CALL IOSDIRCHANGE(DIR)
            if ( jaopen.eq.1 ) then
               CALL NEWFIL(MRGF,FILNAM)
            else
               if ( mrgf.lt.0 ) then
                  FILNAM = ''
               end if
            end if
            GOTO 9999
         ENDIF
      ENDIF

      GOTO 20
!
 9999 CONTINUE
      IF (KEEPSTARTDIR .EQ. 1) THEN
         IF (DIR .NE. CURDIR) CALL IOSDIRCHANGE(CURDIR)
      ENDIF
      CALL IWinClose(1)
      CALL IWinClose(1)
      CALL IWinClose(1)
      filnammenu = filnam
      RETURN
      END



      SUBROUTINE UPDATEFILES(FILNAM,FILIST,NUMFIL,NUMDIR,IFDATE,IFSIZE,IXP,IYP,IH)
      use unstruc_display
      implicit none
      integer :: i, j, k, L, ic, ic0
      integer :: iday
      integer :: ih
      integer :: ihour
      integer :: imonth
      integer :: isecnd
      integer :: ixp
      integer :: iyear
      integer :: iyp
      integer :: maxfil
      integer :: minute
      integer :: n
      integer :: numdir
      integer :: numfil
      PARAMETER (MAXFIL = 2000)
      INTEGER IFDATE(MAXFIL), IFSIZE(MAXFIL)
      CHARACTER FILIST(MAXFIL)*86,FILNAM*(*)
      ! Work arrays for merging+sorting two file lists
      ! when multiple wildcard patterns are used in filnam.
      character filistt(maxfil)*86
      integer ifdatet(maxfil), ifsizet(maxfil)

      NUMFIL = MAXFIL
      NUMDIR = MAXFIL
      CALL INHIGHLIGHT('WHITE','BLUE')
      DO 5 I = 1,MAXFIL
         FILIST(I) = '                                          '
    5 CONTINUE
      CALL IOUTMenuScroll(FILIST,80,IXP,IYP+10,' ',IH-7,0,1)

      CALL IOSDIRENTRYTYPE('D')
      CALL IOsDirInfo(' ','*',FILIST,NUMDIR,IFDATE,IFSIZE)
      IF (NUMDIR .EQ. MAXFIL) THEN
         NUMDIR = MAXFIL - 1
         CALL QNERROR('NOT ALL DIRECTORIES ARE LISTED', ' ', ' ')
      ENDIF

      IF (NOPSYS .EQ. 4) THEN
         DO 10 I = NUMDIR+1,2,-1
            FILIST(I) = FILIST(I-1)
            IFDATE(I) = IFDATE(I-1)
            IFSIZE(I) = IFSIZE(I-1)
   10    CONTINUE
         FILIST(1) = '..                                        '
         NUMDIR = NUMDIR + 1
      ENDIF

      IF (FILIST(1)(1:3) .EQ. '.  ') THEN
         NUMDIR = NUMDIR - 1
         DO 20 I = 1,NUMDIR
            FILIST(I) = FILIST(I+1)
            IFDATE(I) = IFDATE(I+1)
            IFSIZE(I) = IFSIZE(I+1)
   20    CONTINUE
      ENDIF

      NUMFIL = NUMDIR ! current nr of 'files'
      CALL IOSDIRENTRYTYPE('F')
      ic  = 0
      ic0 = 0
      do ! patterns...
          ic = index(filnam(ic0+1:), ',')
          N = NUMFIL + 1

          if (ic == 0) then
            ic = len(filnam)+1
          else
            ic = ic0+ic
          end if
          numfil = maxfil-numfil ! Max nr of files to read
          CALL IOsDirInfo(' ',FILNAM(ic0+1:ic-1),FILIST(N),NUMFIL,IFDATE(N),IFSIZE(N))
          ic0 = ic

          i = NUMDIR ! Start index(-1) of sorted files until now
          j = N-1    ! Start index(-1) of newly found files for next pattern
          L = 0      ! nr of elements in merged result filistt(:), etc.
          do
            if (i==N-1) then ! All 'old' files are already in merged result, just copy remaining 'new' files.
                do K=j+1,N+numfil-1
                    L = L+1
                    filistt(L) = filist(k)
                    ifdatet(L) = ifdate(k)
                    ifsizet(L) = ifsize(k)
                end do
                exit
            end if
            if (j==N+numfil-1) then ! All 'new' files are already in merged result, just copy remaining 'old' files.
                do K=i+1,N-1
                    L = L+1
                    filistt(L) = filist(k)
                    ifdatet(L) = ifdate(k)
                    ifsizet(L) = ifsize(k)
                end do
                exit
            end if

            ! Check which of the two next files (old and new) should come first
            if (lle(filist(i+1), filist(j+1))) then
                i = i+1 ! increase i and leave j
                k = i
            else
                j = j+1 ! increase j and leave i
                k = j
            end if
            L = L+1
            filistt(L) = filist(k)
            ifdatet(L) = ifdate(k)
            ifsizet(L) = ifsize(k)
          end do

          ! And now put the merged+sorted file list back into the actual file list.
          do k=1,L
            filist(NUMDIR+k) = filistt(k)
            ifdate(NUMDIR+k) = ifdatet(k)
            ifsize(NUMDIR+k) = ifsizet(k)
          end do
          NUMFIL = NUMFIL + N-1

          if (ic == len(filnam)+1) then
            exit ! No further patterns in filnam, proceed.
          end if
      end do

      IF (NUMFIL .EQ. MAXFIL) THEN
         CALL QNERROR('NOT ALL FILES ARE LISTED', ' ', ' ')
      ENDIF

      DO 30 I = 1,NUMFIL
         IF (I .LE. NUMDIR) THEN
            IF (NOPSYS .NE. 4) THEN
               CALL IUPPERCASE( FILIST(I)(1:44) )
            ENDIF
            IF (FILIST(I)(1:3) .EQ. '.. ') THEN
               WRITE(FILIST(I)(56:67),'(A12)') '      UP-DIR'
            ELSE
               WRITE(FILIST(I)(56:67),'(A12)') '     SUB-DIR'
            ENDIF
         ELSE
            IF (NOPSYS .NE. 4) THEN
               CALL ILOWERCASE( FILIST(I)(1:54) )
            ENDIF
            WRITE(FILIST(I)(56:67),'(I12)') IFSIZE(I)
         ENDIF

         CALL IOsFileDate(IFDATE(I),IYEAR,IMONTH,IDAY)
         WRITE(FILIST(I)(70:79),'(I2,A1,I2,A1,I4)') IDAY ,'-',IMONTH,'-',IYEAR
         IF (IMONTH .LE. 9) WRITE(FILIST(I)(73:73),'(A1)') '0'
         CALL IOsFileTime(IFDATE(I),IHOUR,MINUTE,ISECND)
         WRITE(FILIST(I)(82:86),'(I2,A1,I2)') IHOUR,':',MINUTE
         IF (MINUTE .LE. 9) WRITE(FILIST(I)(85:85),'(A1)') '0'
   30 CONTINUE
      CALL ITEXTCOLOUR('WHITE','BLU')
      CALL IOUTMenuScroll(FILIST,NUMFIL,IXP,IYP+10,' ',IH-7,0,1)
      RETURN
      END



      SUBROUTINE LOADBITMAP(FILNAM)
      USE M_BITMAP
      USE M_WEARELT
      use string_module, only: find_first_letter, find_first_char
      implicit none
      integer :: ierr
      integer :: k
      integer :: k1
      integer :: k2
      integer :: l
      integer :: minp
      integer :: ndraw
      integer :: num
      integer :: numbersonline
      LOGICAL JAWEL
      INTEGER INFO(10)
      COMMON /DRAWTHIS/  ndraw(50)
      CHARACTER FILNAM*(*),REC*132

      K1 = find_first_char(FILNAM)
      K2 = len_trim(FILNAM)
      CALL IGRFILEINFO(FILNAM(K1:K2),info,3)
      MXP = INFO(2)
      NXP = INFO(3)

      XB = 0
      YB = 0

      NDRAW(26) = 0
      ALLOCATE(IPIX(1),STAT=IERR)
      IF (MXP .GE. 1 .AND. NXP .GE. 1) THEN
         DEALLOCATE(IPIX)
         ALLOCATE(IPIX(MXP*NXP),STAT=IERR)
!        CALL AERR('IPIX(MXP*NXP)',IERR,MXP*NXP)
         IF (IERR .NE. 0) THEN
            CALL QNERROR('BITMAP TOO LARGE',' ',' ')
         ELSE
            CALL IGRLOADIMAGEDATA(FILNAM(K1:K2),IPIX)
         ENDIF

         L = INDEX(FILNAM,'.')
         INQUIRE(FILE = FILNAM(K1:L)//'xyx', EXIST = JAWEL)

         IF (JAWEL) THEN
            CALL OLDFIL(MINP,FILNAM(K1:L)//'xyx')
            READ(MINP,'(A)',END=999) REC
            NUM = NUMBERSONLINE(REC)
            IF (NUM .EQ. 4) THEN
               READ(REC,*,ERR=888) XP(1),YP(1),XP(3),YP(3)
               XP(2) = XP(3)
               YP(2) = YP(1)
               XP(4) = XP(1)
               YP(4) = YP(3)
            ELSE IF (NUM .EQ. 3) THEN
               READ(REC,*,ERR=777) XP(1),YP(1),XP(3)
               YP(3) = YP(1) + ( XP(3)-XP(1) )*dble(NXP)/dble(MXP)
               XP(2) = XP(3)
               YP(2) = YP(1)
               XP(4) = XP(1)
               YP(4) = YP(3)
            ELSE
               IF (FIND_FIRST_LETTER(REC) .EQ. 1) THEN
                  READ(MINP,'(A)',END=999) REC
                  DO K = 1,4
                     READ(MINP,'(A)',END=999) REC
                     IF (NUMBERSONLINE(REC) .EQ. 2) THEN
                        READ(REC,*,ERR=666) XP(K),YP(K)
                     ELSE IF (NUMBERSONLINE(REC) .EQ. 4) THEN
                        READ(REC,*,ERR=555) XP(K),YP(K),XB(K),YB(K)
                        YB(K) = NXP - YB(K) + 1
                     ENDIF
                  ENDDO
               ELSE
                  CALL QNERROR('Cannot Read *.xyx File', ' ',' ')
               ENDIF
            ENDIF
            call doclose(MINP)
         ELSE
            XP(1) = 0
            YP(1) = 0
            XP(3) = MXP
            YP(3) = NXP
            XP(2) = XP(3)
            YP(2) = YP(1)
            XP(4) = XP(1)
            YP(4) = YP(3)
         ENDIF
         NDRAW(26) = 1
      ENDIF

      IF (XB(1) .EQ. 0) XB(1) = -0.5d0
      IF (YB(1) .EQ. 0) YB(1) = -0.5d0
      IF (XB(2) .EQ. 0) XB(2) = MXP+0.5d0
      IF (YB(2) .EQ. 0) YB(2) = -0.5d0
      IF (XB(3) .EQ. 0) XB(3) = MXP+0.5d0
      IF (YB(3) .EQ. 0) YB(3) = NXP+0.5d0
      IF (XB(4) .EQ. 0) XB(4) = -0.5d0
      IF (YB(4) .EQ. 0) YB(4) = NXP+0.5d0

      RETURN

  999 CALL QNEOFERROR(MINP)
      call doclose(MINP)
      RETURN

  998 CALL QNREADERROR('Trying to Read X1,Y1,X2,XY2,X3,Y3,X4,Y4 but', 'get:'//REC,MINP)
      call doclose(MINP)
      RETURN

  888 CALL QNREADERROR('Trying to Read X1,Y1,X3,Y3 but Get:',REC,MINP)
      call doclose(MINP)
      RETURN

  777 CALL QNREADERROR('Trying to Read X1,Y1,X3 but Getting',REC,MINP)
      call doclose(MINP)
      RETURN

  666 CALL QNREADERROR('Trying to Read four lines X,Y but Getting',REC,MINP)
      call doclose(MINP)
      RETURN

  555 CALL QNREADERROR('Trying to Read four lines X,Y,MP,NP but Get',REC,MINP)
      call doclose(MINP)
      RETURN
      END

      SUBROUTINE SHOWBITMAP(jainterpolate)
      USE M_WEARELT
      USE M_BITMAP
      implicit none
      integer :: i
      integer :: ini
      integer :: j
      integer :: k
      integer :: key
      integer :: ndraw
      integer :: nko
      double precision :: xd
      double precision :: xs
      double precision :: xx
      double precision :: xx2
      double precision :: yd
      double precision :: ys
      double precision :: yy
      double precision :: yy2
      double precision :: zs
      integer :: jainterpolate
      COMMON /DRAWTHIS/  ndraw(50)


      CALL IGRCOLOURMODEL(24)

      INI = 1
      XX  = 2
      YY  = 2
      CALL BILINXY(XB, YB, XP, YP, XX, YY, XX2, YY2, INI)
      IF (INI .EQ. -1) RETURN
      INI = 0

      XD  = (XP(2)-XP(1))/(XB(2)-XB(1))
      YD  = (YP(3)-YP(1))/(YB(3)-YB(1))
      XD  = XD/2
      YD  = YD/2

      DO J = NXP,1,-1
         CALL HALT2(KEY)
         IF (KEY .EQ. 1) THEN
            CALL IGRCOLOURMODEL(8)
            RETURN
         ENDIF
         NKO    = -1
         DO I   = 1,MXP
            K   = (NXP-J)*MXP + I
            XX  = dble(I-1)
            YY  = dble(J-1)
            CALL BILINXY(XB, YB, XP, YP, XX, YY, XX2, YY2, INI)

            if (jainterpolate==1) then
               xs = xx2
               ys = yy2
               zs = 1e-6*ipix(k)
               call pixcount(xs,ys,zs,1)
            endif

            IF (XX2 .GT. X1 .AND. XX2 .LT. X2 .AND. YY2 .GT. Y1 .AND. YY2 .LT. Y2 ) THEN
                IF (NKO .NE. IPIX(K)) THEN
                   CALL SETCOL(IPIX(K))
                   NKO = IPIX(K)
                ENDIF
                IF (NDRAW(10) .EQ. 0) THEN
                   call RECTANGLE(real(XX2-XD),real(YY2-YD),real(XX2+XD),real(YY2+YD))
                 ! CALL IGRMOVETO(XX2-XD,YY2-YD)
                 ! CALL IGrRECTANGLEREL(XD*2,YD*2)
                ELSE
                   CALL KREC5(XX2,YY2,XD,YD)
                ENDIF
            ENDIF
         ENDDO
      ENDDO
      CALL IGRCOLOURMODEL(8)

      if (jainterpolate==1) then
         call pixcount(xs,ys,zs,2)
      endif

      RETURN
      END

      SUBROUTINE KREC5(XX,YY,XD,YD)
      implicit none
      double precision :: xd
      double precision :: xx
      double precision :: yd
      double precision :: yy
      real :: X(4), Y(4)
      X(1) = XX - XD
      Y(1) = YY - YD
      X(2) = XX + XD
      Y(2) = YY - YD
      X(3) = XX + XD
      Y(3) = YY + YD
      X(4) = XX - XD
      Y(4) = YY + YD
      CALL PFILLERCORE( X, Y,4)
      RETURN
      END

     SUBROUTINE ISOCOL(VALC,NCOL)
      implicit none
      integer :: i, ncol
      double precision :: valc

      integer          :: NCOLS,NV,NIS,NIE,JAAUTO
      double precision :: VMAX,VMIN,DV,VAL
      COMMON /DEPMAX/     VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
      DO 10 I = NV,1,-1
         IF (VALC .GE. VAL(I)) THEN
            NCOL = I + 1
            CALL SETCOL(NCOLS(NCOL))
            NCOL = NCOLS(NCOL)
            RETURN
         ENDIF
   10 CONTINUE
      NCOL = ncols(1)
      CALL SETCOL(NCOL)
      RETURN
      END


      SUBROUTINE ISOCOL2(VALC,NCOL)
      implicit none
      integer :: i, ncol
      double precision :: valc

      integer :: NCOLS,NV,NIS,NIE,JAAUTO
      double precision :: VMAX,VMIN,DV,VAL
      COMMON /DEPMAX2/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

      DO 10 I = NV,1,-1
         IF (VALC .GE. VAL(I)) THEN
            NCOL = I + 1
            CALL SETCOL(NCOLS(NCOL))
            NCOL = NCOLS(NCOL)
            RETURN
         ENDIF
   10 CONTINUE
      NCOL = ncols(1)
      CALL SETCOL(NCOL)
      RETURN
      END

  SUBROUTINE ISOSCALE()  !   COPY OF ISOSCALE, DIRTY BUT QUICK
  use unstruc_colors
  use M_isoscaleunit
  use m_flowgeom, only: ndx
  use m_netw,     only: nump, numk
  use m_polygon,  only: npl
  use unstruc_display

  implicit none
  double precision :: dv
  double precision :: dx
  double precision :: dxshow
  double precision :: dy
  double precision :: hic
  integer :: i, j, ihcopts, jaauto, ncols, ndec, ndraw, nhcdev, nie, nis, numhcopts, nv, nvec
  integer :: INC

  double precision :: rmiss
  double precision :: scalesize
  double precision :: val
  double precision :: vfac
  double precision :: vfacforce
  double precision :: vmax
  double precision :: vmin
  double precision :: wi
  double precision :: wic
  double precision :: x0
  double precision :: xd
  double precision :: xleg
  double precision :: xsc
  double precision :: xsc0
  double precision :: xsc1
  double precision :: xsc2
  double precision :: y0
  double precision :: yleg

  double precision :: ysc
  double precision :: ysc1
  double precision :: ysc2

  COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
  COMMON /HARDCOPY/ NHCDEV,NUMHCOPTS,IHCOPTS(2,20)
  COMMON /DRAWTHIS/  ndraw(50)
  COMMON /SCALEPOS/ XSC,YSC,SCALESIZE,NDEC
  COMMON /VFAC/     VFAC,VFACFORCE,NVEC
  COMMON /ARCINFO/  DX, DY, X0, Y0, RMISS, DXSHOW, XD
  CHARACTER TEXT2*10, FMT*7
  CHARACTER (LEN=8)  :: TEX
  CHARACTER (LEN=17) :: MINTEX, MAXTEX
  REAL INFOGRAPHICS

  IF (NDRAW(12) == 2 .OR. NDRAW(12) == 4) RETURN

  IF (NDRAW(8) .LE. 1 .and. NDRAW(28) .le. 1 .and. ndrawpol .le. 2 ) return

  if ( max(ndx,nump,npl,numk) == 0) return

  CALL IGRCHARSIZE(real(SCALESIZE),real(SCALESIZE))
  WIC = dble(INFOGRAPHICS(3))
  HIC = dble(INFOGRAPHICS(4))

  INC = NV/30 + 1 ! Max 30 color boxes, otherwise increment > 1

  WI  = 11*WIC + 1.8d0*HIC
  XSC0 = 1-XSC
  IF (XSC0 .LT. 0.6d0) THEN
     XSC1 = X1 + XSC0*(X2-X1)
  ELSE
     XSC1 = X2 - (1-XSC0)*(X2-X1) - WI
  ENDIF
  XSC2 = XSC1 + WI
  YSC1 = Y1 + YSC*(Y2-Y1)

  MINTEX = 'MN=  '
  MAXTEX = 'MX=  '
  WRITE(MINTEX(4:15),'(E11.4)') VMIN
  WRITE(MAXTEX(4:15),'(E11.4)') VMAX

  IF (VMAX .GT. VMIN .AND. NDRAW(19) .GE. 2) THEN
     YSC2 = MIN(YSC1 + (NV/INC+1d0)*HIC + 2.5d0*HIC,Y2)
  ELSE
     YSC2 = MIN(YSC1 + (   1d0)*HIC + 3.5d0*HIC,Y2)
     XSC2 = XSC2 + 2*WIC
  ENDIF

  CALL SETCOL(KLSCL)
  CALL FBOX(XSC1,YSC1,XSC2,YSC2)

  CALL SETCOL(KLTEX)
  CALL BOX(XSC1,YSC1,XSC2,YSC2)

  CALL IGRCHARJUSTIFY('L')

  CALL GTEXT(PARAMTEX(1),XSC1+WIC,YSC2-1*HIC,KLTEX)
  CALL GTEXT(UNIT(1)    ,XSC1+WIC,YSC2-2*HIC,KLTEX)

  IF (VMAX .GT. VMIN .AND. NDRAW(19) .GE. 2) THEN
     IF ( ABS(VMIN) .GT. ABS(VMAX) ) THEN
        CALL DISPFORMscale(VMIN,FMT,NDEC)
     ELSE
        CALL DISPFORMscale(VMAX,FMT,NDEC)
     ENDIF

     XLEG = XSC1 + WIC
     J = 1
     DO I = 1,NV,INC
        YLEG = YSC1 + J*HIC
        WRITE(TEXT2(1:10),FMT) VAL(I)
        CALL JGTEXT (TEXT2,XLEG,YLEG,NCOLS(I),WIC,HIC,0)
        J = J+1
     ENDDO
     TEXT2 = '          '
     CALL JGTEXT (TEXT2,XLEG,YLEG+HIC,NCOLS(NV+1),WIC,HIC,0)
  ELSE
     CALL GTEXT(MAXTEX,XSC1+WIC,YSC2-3*HIC,KLTEX)
     CALL GTEXT(MINTEX,XSC1+WIC,YSC2-4*HIC,KLTEX)
  ENDIF

  RETURN
  END

  SUBROUTINE ISOSCALE2()  !   tekenen legenda
  use M_isoscaleunit
  use unstruc_display
  use m_samples
  implicit none

  double precision :: dv
  double precision :: dx
  double precision :: dxshow
  double precision :: dy
  double precision :: hic
  integer :: i, j
  integer :: INC
  integer :: ihcopts
  integer :: jaauto
  integer :: ncols
  integer :: ndec
  integer :: ndraw
  integer :: nhcdev
  integer :: nie
  integer :: nis
  integer :: numhcopts
  integer :: nv
  integer :: nvec

  double precision :: rmiss
  double precision :: scalesize
  double precision :: val
  double precision :: vfac
  double precision :: vfac2
  double precision :: vfacforce
  double precision :: vmax
  double precision :: vmin
  double precision :: wi
  double precision :: wic
  double precision :: x0
  double precision :: xd
  double precision :: xleg
  double precision :: xsc
  double precision :: xsc1
  double precision :: xsc2
  double precision :: y0
  double precision :: yleg
  double precision :: ysc
  double precision :: ysc1
  double precision :: ysc2
  double precision :: yt

  COMMON /DEPMAX2/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
  COMMON /HARDCOPY/ NHCDEV,NUMHCOPTS,IHCOPTS(2,20)
  COMMON /DRAWTHIS/  ndraw(50)
  COMMON /SCALEPOS/ XSC,YSC,SCALESIZE,NDEC
  COMMON /VFAC/     VFAC, VFACFORCE,NVEC
  COMMON /ARCINFO/  DX, DY, X0, Y0, RMISS, DXSHOW, XD
  CHARACTER TEXT2*10, FMT*7
  CHARACTER (LEN=8)  :: TEX
  CHARACTER (LEN=16) :: MINTEX, MAXTEX
  REAL INFOGRAPHICS

  IF (NDRAW(12) == 1  .OR. NDRAW(12) == 4) RETURN  ! 1 = isoscale off
  IF (NDRAW(29) <= 1 .AND. NDRAW (7) <= 1) then
     if (ndraw(32) <= 0 .or. NS<1) RETURN  ! 1 = no, which linval
  endif

  CALL IGRCHARSIZE(real(SCALESIZE),real(SCALESIZE))
  WIC = INFOGRAPHICS(3)
  HIC = INFOGRAPHICS(4)

  INC = NV/30 + 1 ! Max 30 color boxes, otherwise increment > 1

  WI  = 10*WIC + 1.8d0*HIC
  IF (XSC .LT. 0.6d0) THEN
     XSC1 = X1 + XSC*(X2-X1)
  ELSE
     XSC1 = X2 - (1-XSC)*(X2-X1) - WI
  ENDIF
  XSC2 = XSC1 + WI
  YSC1 = Y1 + YSC*(Y2-Y1)

  MINTEX = 'MIN:            '
  MAXTEX = 'MAX:            '
  WRITE(MINTEX(6:16),'(E11.4)') VMIN
  WRITE(MAXTEX(6:16),'(E11.4)') VMAX

  IF (VMAX .GT. VMIN .AND. NDRAW(11) .GE. 2) THEN
     YSC2 = MIN(YSC1 + (NV/INC+1d0)*HIC + 2.5d0*HIC,Y2)
  ELSE
     YSC2 = MIN(YSC1 + (   1d0)*HIC + 3.5d0*HIC,Y2)
     XSC2 = XSC2 + 2*WIC
  ENDIF

  CALL SETCOL(KLSCL)
  CALL FBOX(XSC1,YSC1,XSC2,YSC2)

  CALL SETCOL(KLTEX)
  CALL BOX(XSC1,YSC1,XSC2,YSC2)

  CALL IGRCHARJUSTIFY('L')

  CALL GTEXT(PARAMTEX(2),XSC1+WIC,YSC2-1*HIC,KLTEX)
  CALL GTEXT(UNIT(2)    ,XSC1+WIC,YSC2-2*HIC,KLTEX)

  IF (VMAX .GT. VMIN .AND. NDRAW(11) .GE. 2) THEN
     IF ( ABS(VMIN) .GT. ABS(VMAX) ) THEN
        CALL DISPFORMscale(VMIN,FMT,NDEC)
     ELSE
        CALL DISPFORMscale(VMAX,FMT,NDEC)
     ENDIF

     XLEG = XSC1 + WIC
     J = 1
     DO I = 1,NV, INC
        YLEG = YSC1 + J*HIC
        WRITE(TEXT2(1:10),FMT) real(VAL(I))
        CALL JGTEXT (TEXT2,XLEG,YLEG,NCOLS(I),WIC,HIC,0)
        J = J+1
     ENDDO
     TEXT2 = '          '
     CALL JGTEXT (TEXT2,XLEG,YLEG+HIC,NCOLS(NV+1),WIC,HIC,0)
  ELSE
     CALL GTEXT(MAXTEX,XSC1+WIC,YSC2-3*HIC,KLTEX)
     CALL GTEXT(MINTEX,XSC1+WIC,YSC2-4*HIC,KLTEX)
  ENDIF

  IF (NDRAW(15) .EQ. 11 .OR. NDRAW(15) .EQ. 13 .OR. NDRAW(15) .EQ. 15 .OR. NDRAW(15) .EQ. 16) THEN
      CALL SETCOL(KLSCL)
      YT = YSC1-5*HIC
      CALL FBOX(XSC1,YT-4*HIC,XSC2,YT)
      CALL SETCOL(KLTEX)
      CALL BOX(XSC1,YT-4*HIC,XSC2,YT)
      VFAC2 = 0.3d0*(XSC2-XSC1)
      CALL SETCOL(KLVEC)
      CALL ARROWS(XSC1+WIC,YT-2*HIC,1d0,0d0,0d0,VFAC2)
      TEX = ' 2.3 m/s'
      ! WRITE(TEX(1:4),'(F4.1)')  VFAC2/(DX*VFAC)
      WRITE(TEX(1:4),'(F4.1)')  real(VFAC2/(VFAC))
      CALL IGRCHARJUSTIFY('R')
      CALL GTEXT(TEX,XSC2-WIC,YT-2*HIC,KLTEX)
  ENDIF

  RETURN
  END



  subroutine DISPFORMscale(value,fmt,NDEC)
  implicit none
  integer :: n1
  integer :: n2
  integer :: n3 ! nr of digits behind decimal dot
  integer :: ndec
  double precision :: value
  character fmt*(*)

  fmt='(f10.3)'

  if (value .eq. 0d0) then
     fmt='(f3.1)'
     return
  endif

  n1 = int(log10(abs(value)))

  if (n1 .lt. 6 .and. n1 .gt. 0) then
     n2 = min(9,n1 + 3)
     n3 = 9 - n2
  else if (n1 .ge. -5 .and. n1 .lt. 0) then
     n3 = 6
  else if ( n1 .eq. 0) then
     n3 = 6
  else
     fmt ='(e10.3)'
     return
  endif

  IF (NDEC .GT. 0) then
     n3 = min(n3, NDEC) ! try ndec, but only if it fits
  end if

  write (fmt(6:6),'(i1)') n3
  return
  end

  SUBROUTINE JGTEXT(TEX,X,Y,NCOL,WIC,HIC,JAHOOG) ! grafische tekst, grafische posities, met kleurblokjes ERONDER
    use unstruc_colors
  implicit none
  double precision :: hic, WIC
  integer :: jahoog
  integer :: ncol
  integer :: ndraw
  double precision :: x
  double precision :: xa
  double precision :: xb
  double precision :: xp
  double precision :: y
  double precision :: ya
  double precision :: yb
  double precision :: yp
  CHARACTER TEX*(*)
  COMMON /DRAWTHIS/  ndraw(50)

  CALL SETCOL(KLTEX)
  CALL DRAWTEXT(real(X),real(Y),TEX)
  CALL GETPOS(XP,YP)

  XA = XP + 0.3d0*WIC
  YA = YP - 0.8d0*HIC + JAHOOG*HIC
  XB = XA + 1.3d0*WIC
  YB = YA + 0.7d0*HIC

  IF (NCOL .NE. 0) THEN
     CALL SETCOL(NCOL)
     IF (JAHOOG .EQ. 0) THEN
        CALL FBOX(XA,YA,XB,YB)
        CALL SETCOL(KLTEX)
        CALL  BOX(XA,YA,XB,YB)
     ELSE
        CALL FBOX(XA,YA,XB,YB)
     ENDIF
  ENDIF
  RETURN
  END

SUBROUTINE SETCOLTABFILE(FILNAM,JASECOND)
    use unstruc_colors
    implicit none
    double precision :: dv, dv2
    integer :: i
    integer :: iblue
    integer :: igreen
    integer :: ihue
    integer :: ired
    integer :: isat
    integer :: jaauto, jaauto2
    integer :: jahls
    integer :: jasecond
    integer :: k
    integer :: light
    integer :: minp
    integer :: ncols, ncols2
    integer :: nie, nie2
    integer :: nis, nis2
    integer :: nisn
    integer :: nv, nv2
    double precision :: val, val2
    double precision :: vmax, vmax2
    double precision :: vmin, vmin2
    integer, parameter  :: mxq = 1, mxclass = 1
    COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
    COMMON /DEPMAX2/ VMAX2,VMIN2,DV2,VAL2(256),NCOLS2(256),NV2,NIS2,NIE2,JAAUTO2
    CHARACTER FILNAM*(*), FOLNAM*86
    FOLNAM = FILNAM
    IF (FILNAM(1:5) .EQ. '*.hls') THEN
       MINP   = 0
       CALL FILEMENU(MINP,FOLNAM)
    ELSE
       k = len_trim(filnam)
       folnam(1:k) = filnam(1:k)
       CALL SYSORLOCALFIL(MINP,FOLNAM,0)
    ENDIF
    IF (MINP .GT. 0) THEN
       IF (INDEX(FOLNAM,'HLS') .GE. 1 .OR. INDEX(FOLNAM,'hls') .GE. 1 ) THEN
           JAHLS = 1
       ELSE IF (INDEX(FOLNAM,'RGB') .GE. 1 .OR. INDEX(FOLNAM,'rgb') .GE. 1 ) THEN
           JAHLS = 2
       ELSE
           CALL QNMESSAGE('CHOOSE *.hls OR *.rgb FILE')
           RETURN
       ENDIF
       IF (JASECOND .EQ. 0) THEN
          coltabfile  = folnam
       else   
          coltabfile2 = folnam
       endif
          
       K = 1
       READ (MINP,*,END = 999,ERR=888)
 20    CONTINUE
       IF (JAHLS .EQ. 1) THEN
          READ (MINP,*,END = 999,ERR=888) IHUE,LIGHT,ISAT
          IHUE  = MAX(0,MIN(IHUE ,360))
          LIGHT = MAX(0,MIN(LIGHT,100))
          ISAT  = MAX(0,MIN(ISAT ,100))
          IF (JASECOND .EQ. 0) THEN
             CALL IGRPALETTEHLS(NCOLS(K),IHUE,LIGHT,ISAT)
          ELSE
             CALL IGRPALETTEHLS(NCOLS2(K),IHUE,LIGHT,ISAT)
          ENDIF
       ELSE IF (JAHLS .EQ. 2) THEN
          READ (MINP,*,END = 999,ERR=888) IRED,IGREEN,IBLUE
          IRED  = MAX(0,MIN(IRED   ,255))
          IGREEN= MAX(0,MIN(IGREEN ,255))
          IBLUE = MAX(0,MIN(IBLUE  ,255))
          IF (JASECOND .EQ. 0) THEN
             CALL IGRPALETTERGB(NCOLS(K),IRED,IGREEN,IBLUE)
          ELSE
             CALL IGRPALETTERGB(NCOLS2(K),IRED,IGREEN,IBLUE)
          ENDIF
       ENDIF
       K = K + 1
       GOTO 20
999    CONTINUE
       call doclose (MINP)
       IF (JASECOND .EQ. 0) THEN
          NV  = MAX(2,K-2)
          NIE = NIS + NV + 1
       else
          NV2  = MAX(2,K-2)
          NIE2 = NIS2 + NV2 + 1
       ENDIF
       RETURN
888    CONTINUE ! Read error in coltabfile, back to defaults.
       call doclose (MINP)
    ENDIF
    RETURN
    END


   SUBROUTINE CHANGENUMERICALPARAMETERS()
   use m_netw
   USE M_FLOW
   use m_flowgeom
   USE m_sferic
   use m_wind
   use unstruc_display
   use m_reduce
   use m_sediment, only: dmorfac
   use unstruc_version_module, only : unstruc_company, unstruc_program
   use unstruc_messages
   use m_fixedweirs
   implicit none

   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 22, NUMFLD = 2*NUMPAR)
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   integer :: nlevel
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line
!
   integer :: ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key, ja, niadvec
   integer :: nbut, imp, inp

   NLEVEL     = 4
   OPTION( 1) = 'COURANT NR                           ( )' ; it(2* 1) = 6
   OPTION( 2) = 'IADVEC                                  ' ; it(2* 2) = 2
   OPTION( 3) = 'IADVEC1D                                ' ; it(2* 3) = 2
   OPTION( 4) = 'Limtyp scalar   transport               ' ; it(2* 4) = 2
   OPTION( 5) = 'Limtyp hu                               ' ; it(2* 5) = 2
   OPTION( 6) = 'Limtyp momentum transport               ' ; it(2* 6) = 2
   OPTION( 7) = 'itstep                                  ' ; it(2* 7) = 2
   OPTION( 8) = 'teta                                ( ) ' ; it(2* 8) = 6
   OPTION( 9) = 'icgsolver                           ( ) ' ; it(2* 9) = 2
   OPTION(10) = 'Transport Method                    ( ) ' ; it(2*10) = 2
   OPTION(11) = 'Salinity included 0/1               ( ) ' ; it(2*11) = 2
   OPTION(12) = 'Temperature model nr, 0=no, 5=heatflx() ' ; it(2*12) = 2
   OPTION(13) = 'Anti creep                          ( ) ' ; it(2*13) = 2
   OPTION(14) = '                                    ( ) ' ; it(2*14) = 6
   OPTION(15) = 'irov 0,1,2,3                        ( ) ' ; it(2*15) = 2
   OPTION(16) = 'icorio, 0, 5=org def., even=2D weigh( ) ' ; it(2*16) = 2
   OPTION(17) = 'jatidep tidal potential forcing 0/1 ( ) ' ; it(2*17) = 2
   OPTION(18) = 'EpsCG, CG solver stop criterion     ( ) ' ; it(2*18) = 6
   OPTION(19) = 'Epshu, flooding criterion           (m) ' ; it(2*19) = 6
   OPTION(20) = 'JaExplicitsinks                     ( ) ' ; it(2*20) = 2
   OPTION(21) = 'Corioadamsbashfordfac               ( ) ' ; it(2*21) = 6
   OPTION(22) = 'Newcorio                            ( ) ' ; it(2*22) = 2
 
 
!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   HELPM ( 1) = 'Total    COURANT                                            '
   HELPM ( 2) = '0=N0, 33=Full Perot, 1=wenn tot, 2=wenn inoutdif, 3 =       '
   HELPM ( 3) = 'see iadvec                                                  '
   HELPM ( 4) = '0=No, 1=Minmod, 2=VanLeer, 3=Kooren, 4=Monotonized Central  '
   HELPM ( 5) = '0=No, 1=Minmod, 2=VanLeer, 3=Kooren, 4=Monotonized Central, 21=central'
   HELPM ( 6) = '0=No, 1=Minmod, 2=VanLeer, 3=Kooren, 4=Monotonized Central  '
   HELPM ( 7) = '2=implicit pressure, 1=no pressure, 0 = only transport      '
   HELPM ( 8) = '0.5 < teta =< 1.0                                           '
   HELPM ( 9) = '1 = GS_OMP, 2 = GS_OMPthreadsafe, 3 = GS, 4 = SaadILUD      '
   HELPM (10) = '0=Herman transport, 1=transport module (default), 2=no      '
   HELPM (11) = '0=no salinity, 1=yes salinity                               '
   HELPM (12) = 'Temperature model nr, 0=no temp, 5=heat flux 3=excess       '
   HELPM (13) = '0=No, 1=Yes anticreep  only in sigma layers                 '
   HELPM (14) = 'default 1d-8                                                '
   HELPM (15) = '0=free slip, 1 =partial slip, 2=no slip, 3 =hydraul. smooth '
   HELPM (16) = '0=no 5=default, 3,4 no weights, 5-10 Olga, 25-30 Ham        '
   HELPM (17) = '0=no tidal potential, 1=yes tidal potential                 '
   HELPM (18) = 'Guus, if max(abs(r/rk) < epscg , or Saad L2norm < epscg     '
   HELPM (19) = 'hu > epshu: link flows                                      '
   HELPM (20) = '1=expl, 0 = impl                                            '
   HELPM (21) = '>0 = Adams Bashford, standard= 0.5, only for Newcorio=1     '
   HELPM (22) = '0=prior to 27-11-2019, 1=no normal forcing on open bnds, 12#'
   
   
   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      IX(IL) = 13
      IX(IR) = 95
      IY(IL) = 2*I
      IY(IR) = 2*I
      IS(IL) = 82
      IS(IR) = 10
      IT(IL) = 1001
   ENDDO

!  Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW     = NPOS(3)
   IXP    = NPOS(1) + (IWS-IW)/2
   IYP    = NPOS(2)
   IH     = IHS - 9

!  Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program) // ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!  Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!  Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)

   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

!  NUMWNH = InfoWindow(1)
!  CALL IWinSelect(NUMWNH)

!  Define a new form by supplying arrays containing
!  field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

!  Define a help field and define help strings
!  for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   NIADVEC = IADVEC
   CALL IFormPutDouble  (2* 1 ,CFLmx,  '(F8.3)' )            
   CALL IFORMPUTINTEGER (2* 2 ,NIADVEC          )            
   CALL IFORMPUTINTEGER (2* 3 ,IADVEC1D         )            
   CALL IFORMPUTINTEGER (2* 4 ,Limtypsa         )            
   CALL IFORMPUTINTEGER (2* 5 ,Limtyphu         )            
   CALL IFORMPUTINTEGER (2* 6 ,Limtypmom        )            
   CALL IFORMPUTINTEGER (2* 7 ,itstep           )            
   CALL IFormPutDouble  (2* 8 ,teta0  ,'(F10.3)')           
   CALL IFORMPUTinteger (2* 9 ,icgsolver        )           
   CALL IFORMPUTinteger (2*10 ,jatransportmodule)           
   CALL IFORMPUTinteger (2*11 ,jasal            )           
   CALL IFORMPUTinteger (2*12 ,jatem            )           
   CALL IFORMPUTinteger (2*13 ,jacreep          )           
   CALL IFORMPUTdouble  (2*14 ,epsmaxlev,'(e10.5)' )     
   CALL IFORMPUTinteger (2*15 ,irov             )           
   CALL IFORMPUTinteger (2*16 ,icorio           )           
   CALL IFORMPUTinteger (2*17 ,jatidep          )           
   CALL IFormPutDouble  (2*18 ,epscg, '(e10.5)' )
   CALL IFormPutDouble  (2*19 ,epshu, '(e10.5)' )                                             
   CALL IFORMPUTinteger (2*20 ,jaexplicitsinks  )     
   CALL IFormputDouble  (2*21 ,Corioadamsbashfordfac,'(e10.5)')        
   CALL IFormputinteger (2*22 ,Newcorio)        


   !  Display the form with numeric fields left justified
   !  and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!  check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN

          CALL IFormgetDouble  (2* 1 ,CFLmx           )    
          CALL IFORMgeTINTEGER (2* 2 ,NIADVEC         )         
          CALL IFORMgeTINTEGER (2* 3 ,IADVEC1D        )         
          CALL IFORMgeTINTEGER (2* 4 ,Limtypsa        )  ;  limtypsa  = max(0, min (limtypsa ,30))     
          CALL IFORMgeTINTEGER (2* 5 ,Limtyphu        )  ;  limtyphu  = max(0, min (limtyphu ,30))     
          CALL IFORMgeTINTEGER (2* 6 ,Limtypmom       )  ;  limtypmom = max(0, min (limtypmom,30))     
          CALL IFORMgeTINTEGER (2* 7 ,itstep          )         
          CALL IFormgetDouble  (2* 8 ,teta0           )        
          CALL IFORMgeTinteger (2* 9 ,icgsolver       )        
          CALL IFORMgeTinteger (2*10 ,jatransportmodule)        
          CALL IFORMgeTinteger (2*11 ,jasal           )        
          CALL IFORMgeTinteger (2*12 ,jatem           )        
          CALL IFORMgeTinteger (2*13 ,jacreep         )        
          CALL IFORMgeTdouble  (2*14 ,epsmaxlev       )  
          CALL IFORMgeTinteger (2*15 ,irov            )        
          CALL IFORMgeTinteger (2*16 ,icorio          )        
          CALL IFORMgeTinteger (2*17 ,jatidep         )        
          CALL IFormgetDouble  (2*18 ,epscg           )
          CALL IFormgetDouble  (2*19 ,epshu           )        
          CALL IFORMgeTinteger (2*20 ,jaexplicitsinks )        
          CALL IFormgetDouble  (2*21 ,Corioadamsbashfordfac)        

          epshs    = 0.2d0*epshu  ! minimum waterdepth for setting cfu
          if (niadvec .ne. iadvec) then
             if (nfxw > 0) then
                call confrm('If Fixedweirs present, please reinitialise the model', ja)
             endif
             iadvec = niadvec
             call iadvecini()
          endif
           
       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE CHANGENUMERICALPARAMETERS

  SUBROUTINE CHANGENUMERICALPARAMETERS2()
   use m_netw
   USE M_FLOW
   use m_flowgeom
   USE m_sferic
   use m_wind
   use unstruc_display
   use m_reduce
   use unstruc_version_module, only : unstruc_company, unstruc_program
   use unstruc_messages
   use m_fixedweirs
   implicit none

   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 19, NUMFLD = 2*NUMPAR)
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   integer :: nlevel
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line
!
   integer :: ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key, ja, niadvec
   integer :: nbut, imp, inp

   NLEVEL     = 4
   OPTION( 1) = 'ITURBULENCEMODEL                     ( )' ; it(2* 1) = 2
   OPTION( 2) = 'JAUSTARINT                           ( )' ; it(2* 2) = 2
   OPTION( 3) = 'jabaroctimeint                          ' ; it(2* 3) = 2
   OPTION( 4) = 'JAVAKEPS                                ' ; it(2* 4) = 2
   OPTION( 5) = 'IDENSFORM                               ' ; it(2* 5) = 2
   OPTION( 6) = 'JARHOXU                                 ' ; it(2* 6) = 2
   OPTION( 7) = 'JAVASAL                                 ' ; it(2* 7) = 2
   OPTION( 8) = 'IFIXEDWEIRSCHEME                        ' ; it(2* 8) = 2
   OPTION( 9) = 'Tsigma                                  ' ; it(2* 9) = 6
   OPTION(10) = 'Local timestepping in transport(1)      ' ; it(2*10) = 2
   OPTION(11) = 'Cffacver                                ' ; it(2*11) = 6
   OPTION(12) = 'Javatem                                 ' ; it(2*12) = 2
   OPTION(13) = 'Javiuplus3D                             ' ; it(2*13) = 2
   OPTION(14) = 'Jaqaisq1                                ' ; it(2*14) = 2
   OPTION(15) = 'Addksources                             ' ; it(2*15) = 6
   OPTION(16) = 'Initialise rho, if not, first barocl = 0' ; it(2*16) = 2
   OPTION(17) = 'jaLogprofatubndin                       ' ; it(2*17) = 2
   OPTION(18) = 'javau                                   ' ; it(2*18) = 2
   OPTION(19) = 'jacomp                                  ' ; it(2*19) = 2
   
!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   HELPM ( 1) = '0=no, 1 = constant, 2 = algebraic, 3 = k-eps, 4 = k-tau     '
   HELPM ( 2) = '0123                                                        '
   HELPM ( 3) = '1 = expl, -2; abashford, -3 = ab3, -5 = adv rho             '
   HELPM ( 4) = '0 = NO, 3 = VERT IMPL, HOR EXPL                             '
   HELPM ( 5) = '0 = no, 1 = eckart                                          '
   HELPM ( 6) = '0 = no, 1 = YES                                             '
   HELPM ( 7) = '0=No, 1=Upwe, 2=Cente, 3=Upwi, 4=Centi, 5=4,3, 6=MCexpl     ' 
   HELPM ( 8) = '0=No, 6=subgrid, 7=rajaratnam, 8=Tabelb, 9=Willemontenotyet ' 
   HELPM ( 9) = 'Sigma adaptation timescale, only for layertype == 4         ' 
   HELPM (10) = '1 = yes, 0 = no                                             ' 
   HELPM (11) = '0=never switch off ho term vertical                         ' 
   HELPM (12) = '0=No, 1=Upwe, 2=Cente, 3=Upwi, 4=Centi, 5=4,3, 6=MCexpl     ' 
   HELPM (13) = '0=no, 1 = yes                                               ' 
   HELPM (14) = '0=no, 1 = yes                                               ' 
   HELPM (15) = '0=no, 1 = yes                                               ' 
   HELPM (16) = '0=no, 1 = yes                                               ' 
   HELPM (17) = 'at ubnd in: 0 = uniform U1, 1 = log U1, 2 = also k-eps      ' 
   HELPM (18) = '0=no, 3 = impli upw, 5 = Quickest                           ' 
   HELPM (19) = '0=standard, 1 = use csu snu in weights, 2 = scalarx,y banf  ' 
  
   
   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      IX(IL) = 13
      IX(IR) = 95
      IY(IL) = 2*I
      IY(IR) = 2*I
      IS(IL) = 82
      IS(IR) = 10
      IT(IL) = 1001
   ENDDO

!  Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW     = NPOS(3)
   IXP    = NPOS(1) + (IWS-IW)/2
   IYP    = NPOS(2)
   IH     = IHS - 9

!  Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program) // ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!  Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!  Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)

   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

!  NUMWNH = InfoWindow(1)
!  CALL IWinSelect(NUMWNH)

!  Define a new form by supplying arrays containing
!  field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

!  Define a help field and define help strings
!  for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFormPutINTEGER (2* 1 ,ITURBULENCEMODEL )            
   CALL IFORMPUTINTEGER (2* 2 ,JAUSTARINT       )            
   CALL IFORMPUTINTEGER (2* 3 ,jabaroctimeint   )    
   CALL IFORMPUTINTEGER (2* 4 ,JAVAKEPS         )            
   CALL IFORMPUTINTEGER (2* 5 ,IDENSFORM        )            
   CALL IFORMPUTINTEGER (2* 6 ,JARHOXU          )        
   CALL IFORMPUTINTEGER (2* 7 ,JAVASAL          )          
   CALL IFORMPUTINTEGER (2* 8 ,ifixedweirscheme )          
   CALL IFORMPUTdouble  (2* 9 ,Tsigma           , '(F7.3)' )          
   CALL IFORMPUTINTEGER (2*10 ,JALTS            )            
   CALL IFORMPUTdouble  (2*11 ,Cffacver         , '(F7.3)' )          
   CALL IFORMPUTINTEGER (2*12 ,JAVATEM          )    
   CALL IFORMputINTEGER (2*13 ,javiuplus3D      )   
   CALL IFORMputINTEGER (2*14 ,jaqaisq1         )   
   CALL IFORMputdouble  (2*15 ,addksources      , '(F7.3)' )      
   CALL IFORMputINTEGER (2*16 ,jainirho         )   
   CALL IFORMputINTEGER (2*17 ,jaLogprofatubndin)   
   CALL IFORMputINTEGER (2*18 ,javau)   
   CALL IFORMputINTEGER (2*19 ,jacomp)   
 
   
   !  Display the form with numeric fields left justified
   !  and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!  check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN

          CALL IFORMGETINTEGER (2* 1 ,ITURBULENCEMODEL )            
          CALL IFORMGETINTEGER (2* 2 ,JAUSTARINT       )            
          CALL IFORMGETINTEGER (2* 3 ,jabaroctimeint   )            
          CALL IFORMGETINTEGER (2* 4 ,JAVAKEPS         )            
          CALL IFORMGETINTEGER (2* 5 ,IDENSFORM        )            
          CALL IFORMGETINTEGER (2* 6 ,JARHOXU          )            
          CALL IFORMGETINTEGER (2* 7 ,JAVASAL          )     
          CALL IFORMGETINTEGER (2* 8 ,IFIXEDWEIRSCHEME )   
          CALL IFORMGETdouble  (2* 9 ,Tsigma           )   
          CALL IFORMGETINTEGER (2*10 ,JALTS            )   
          CALL IFORMGETdouble  (2*11 ,Cffacver         ) 
          CALL IFORMGETINTEGER (2*12 ,JAVATEM          )    
          CALL IFORMGETINTEGER (2*13 ,javiuplus3D      )   
          CALL IFORMGETINTEGER (2*14 ,jaqaisq1         )   
          CALL IFORMGETdouble  (2*15 ,addksources      )   
          CALL IFORMGETINTEGER (2*16 ,jainirho         )   
          CALL IFORMGETINTEGER (2*17 ,jaLogprofatubndin)   
          CALL IFORMGETINTEGER (2*18 ,javau)   
          CALL IFORMGETINTEGER (2*19 ,jacomp)   

          
       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE CHANGENUMERICALPARAMETERS2

   SUBROUTINE CHANGENUMERICALPARAMETERS3()
   use m_netw
   USE M_FLOW
   use m_flowgeom
   USE m_sferic
   use m_wind
   use m_sediment
   use unstruc_display
   use m_reduce
   use unstruc_version_module, only : unstruc_company, unstruc_program
   use unstruc_messages
   use m_fixedweirs
   use m_waves
   implicit none

   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 22, NUMFLD = 2*NUMPAR)
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   integer :: nlevel
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line
!
   integer :: ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key, ja, niadvec
   integer :: nbut, imp, inp

   NLEVEL     = 4
   OPTION( 1) = 'Clveg                                ( )' ; it(2* 1) = 6
   OPTION( 2) = 'Cdveg                                ( )' ; it(2* 2) = 6
   OPTION( 3) = 'Rhoveg                           (kg/m3)' ; it(2* 3) = 6
   OPTION( 4) = 'Cbveg                         (kg.m2/s2)' ; it(2* 4) = 6
   OPTION( 5) = 'Stemheightstd                        ( )' ; it(2* 5) = 6
   OPTION( 6) = 'Hwavuni                              (m)' ; it(2* 6) = 6
   OPTION( 7) = 'Twavuni                              (s)' ; it(2* 7) = 6
   OPTION( 8) = 'Phiwavuni                            ( )' ; it(2* 8) = 6
   OPTION( 9) = 'Wave model nr modind                 ( )' ; it(2* 9) = 2
   OPTION(10) = 'Slotw1D                              (m)' ; it(2*10) = 6
   OPTION(11) = 'Slotw2D                              (m)' ; it(2*11) = 6
   OPTION(12) = 'Epsmaxlev                            (m)' ; it(2*12) = 6
   OPTION(13) = 'Epsmaxlevm                           (m)' ; it(2*13) = 6
   OPTION(14) = 'jawavestreaming terms in D3Dwavemodel( )' ; it(2*14) = 2
   OPTION(15) = 'jawaveStokes 0,1,2                   ( )' ; it(2*15) = 2
   OPTION(16) = 'jawaveRoller                         ( )' ; it(2*16) = 2
   OPTION(17) = 'Maxitforestersal                     ( )' ; it(2*17) = 2
   OPTION(18) = 'Maxitforestertem                     ( )' ; it(2*18) = 2
   OPTION(19) = 'Jajipjan (Noderivedtypes in mdu)     ( )' ; it(2*19) = 2
   OPTION(20) = 'Maxdegree                            ( )' ; it(2*20) = 2
   OPTION(21) = 'Jaevap                               ( )' ; it(2*21) = 2
   OPTION(22) = 'Jaseddenscoupling                    ( )' ; it(2*22) = 2

   
!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   HELPM ( 1) = 'Distance coeff (0.8)                                    ( ) '
   HELPM ( 2) = 'Drag coefficient (0.8)                                  ( ) '
   HELPM ( 3) = 'Vegetation specific density, if > 0, include bending        '
   HELPM ( 4) = 'Bending stiffness coefficient                               '
   HELPM ( 5) = 'Standard deviation of stem height                           '
   HELPM ( 6) = '                                                            '
   HELPM ( 7) = '                                                            '
   HELPM ( 8) = '                                                            '
   HELPM ( 9) = 'wave model nr 1-9                                           '
   HELPM (10) = 'Slotwidth in 1D , default 1d-3                          (m) '
   HELPM (11) = 'Slotwidth in 2D , default 0d-3                          (m) '
   HELPM (12) = 'Max level diff in Newton iterations,      default 1d-8  (m) '
   HELPM (13) = 'Max level diff in outer loop of Nested Newton def 1d-8  (m) '
   HELPM (14) = '>=1 streaming, >= 2 streaming + turb                        '
   HELPM (15) = '0=no, 1 = uniform, 2 = non-uniform, 3=2+vertical visc Stokes'
   HELPM (16) = '0=no, 1 = rol1, 2 = rol2                                    '
   HELPM (17) = 'Max nr of iterations                                        '
   HELPM (18) = 'Max nr of iterations                                        '
   HELPM (19) = '0=use der. types, 1 = less, 2 = lesser, 5 = also deallo der.'
   HELPM (20) = '6 = default, 666 = number of the devil                      '
   HELPM (21) = '1 = evaporation computed bij heatfluxmodel , 0= no evap     '
   HELPM (22) = '0=no, 1 = yes                                               '
   
   
   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      IX(IL) = 13
      IX(IR) = 95
      IY(IL) = 2*I
      IY(IR) = 2*I
      IS(IL) = 82
      IS(IR) = 10
      IT(IL) = 1001
   ENDDO

!  Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW     = NPOS(3)
   IXP    = NPOS(1) + (IWS-IW)/2
   IYP    = NPOS(2)
   IH     = IHS - 9

!  Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program) // ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!  Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!  Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)

   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

!  NUMWNH = InfoWindow(1)
!  CALL IWinSelect(NUMWNH)

!  Define a new form by supplying arrays containing
!  field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

!  Define a help field and define help strings
!  for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFORMputdouble  (2* 1 ,Clveg            , '(F7.3)' )      
   CALL IFORMputdouble  (2* 2 ,Cdveg            , '(F7.3)' )      
   CALL IFORMputdouble  (2* 3 ,Rhoveg           , '(F7.3)' )      
   CALL IFORMputdouble  (2* 4 ,Cbveg            , '(F7.3)' )      
   CALL IFORMputdouble  (2* 5 ,Stemheightstd    , '(F7.3)' )      
   CALL IFORMputdouble  (2* 6 ,hwavuni          , '(F7.3)' )      
   CALL IFORMputdouble  (2* 7 ,twavuni          , '(F7.3)' )      
   CALL IFORMputdouble  (2* 8 ,phiwavuni        , '(F7.3)' )      
   CALL IFORMputinteger (2* 9 ,modind           )       
   CALL IFORMputdouble  (2*10 ,Slotw1D          , '(E8.2)' )       
   CALL IFORMputdouble  (2*11 ,Slotw2D          , '(E8.2)' )       
   CALL IFORMputdouble  (2*12 ,Epsmaxlev        , '(E8.2)' )       
   CALL IFORMputdouble  (2*13 ,Epsmaxlevm       , '(E8.2)' )     
   CALL IFORMputinteger (2*14 ,jawavestreaming             )     
   CALL IFORMputinteger (2*15 ,jawaveStokes                )     
   CALL IFORMputinteger (2*16 ,jawaveRoller                )     
   CALL IFORMputinteger (2*17 ,Maxitverticalforestersal    )     
   CALL IFORMputinteger (2*18 ,Maxitverticalforestertem    )     
   CALL IFORMputinteger (2*19 ,Jajipjan                    )     
   CALL IFORMputinteger (2*20 ,maxdge                      )     
   CALL IFORMputinteger (2*21 ,Jaevap                      )     
   CALL IFORMputinteger (2*22 ,Jaseddenscoupling           )     

   
   !  Display the form with numeric fields left justified
   !  and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!  check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN

          CALL IFORMGETdouble  (2* 1 ,Clveg            )   
          CALL IFORMGETdouble  (2* 2 ,Cdveg            )   
          CALL IFORMGETdouble  (2* 3 ,Rhoveg           )   
          CALL IFORMGETdouble  (2* 4 ,Cbveg            )   
          CALL IFORMGETdouble  (2* 5 ,stemheightstd    )   
          CALL IFORMGETdouble  (2* 6 ,  hwavuni        )   ;  if (  hwavuni > 0d0)   hwav =   hwavuni   
          CALL IFORMGETdouble  (2* 7 ,  twavuni        )   ;  if (  twavuni > 0d0)   twav =   twavuni   
          CALL IFORMGETdouble  (2* 8 ,phiwavuni        )   ;  if (phiwavuni > 0d0) phiwav = phiwavuni 
          CALL IFORMGETinteger (2* 9 ,modind           )  
          CALL IFORMGETdouble  (2*10 ,Slotw1D          )
          CALL IFORMGETdouble  (2*11 ,Slotw2D          )
          CALL IFORMGETdouble  (2*12 ,Epsmaxlev        )
          CALL IFORMGETdouble  (2*13 ,Epsmaxlevm       )
          CALL IFORMGETinteger (2*14 ,jawavestreaming         )  
          CALL IFORMGETinteger (2*15 ,jawaveStokes            )  
          CALL IFORMGETinteger (2*16 ,jawaveRoller            )  
          CALL IFORMGETinteger (2*17 ,Maxitverticalforestersal)     
          CALL IFORMGETinteger (2*18 ,Maxitverticalforestertem)     
          CALL IFORMGETinteger (2*19 ,Jajipjan                )     
          CALL IFORMGETinteger (2*20 ,Maxdge                  )     
          CALL IFORMGETinteger (2*21 ,Jaevap                  )  
          CALL IFORMgetinteger (2*22 ,Jaseddenscoupling            )      
          if (jaevap > 0) then
             if (.not. allocated (evap) ) then 
                allocate (evap(ndx))   
             endif
             jaqin = 1
          endif
       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE CHANGENUMERICALPARAMETERS3
   
   SUBROUTINE CHANGENUMERICALPARAMETERS4()
   USE M_FLOW
   use m_flowgeom
   use unstruc_display
   use unstruc_version_module, only : unstruc_company, unstruc_program
   use unstruc_messages
   implicit none

   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 17, NUMFLD = 2*NUMPAR)
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD), L
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   integer :: nlevel
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line
!
   integer :: ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key, ja, niadvec
   integer :: nbut, imp, inp
   double precision :: h1,h5,h7,w1,w5,w7 

   NLEVEL     = 4
!   OPTION( 1) = 'vicouv_filter                    (m2/s)' ; it(2* 1) = 6
   OPTION( 1) = 'filter                           ( )   ' ; it(2* 1) = 2
   OPTION( 2) = 'filter order                     ( )   ' ; it(2* 2) = 2
   OPTION( 3) = 'hh1DUNI                          (m)   ' ; it(2* 3) = 6
   OPTION( 4) = 'Uniformtyp1D                     (m)   ' ; it(2* 4) = 2
   OPTION( 5) = 'wu1DUNI5                         (m)   ' ; it(2* 5) = 6
   OPTION( 6) = 'hh1DUNI5                         (m)   ' ; it(2* 6) = 6
   OPTION( 7) = 'Uniformtyp1D5                    (m)   ' ; it(2* 7) = 2
   OPTION( 8) = 'wu1DUNI7                         (m)   ' ; it(2* 8) = 6
   OPTION( 9) = 'hh1DUNI7                         (m)   ' ; it(2* 9) = 6
   OPTION(10) = 'Uniformtyp1D7                    (m)   ' ; it(2*10) = 2
   OPTION(11) = 'japiaczek33                      ( )   ' ; it(2*11) = 2
   OPTION(12) = 'Expchistem                       ( )   ' ; it(2*12) = 6
   OPTION(13) = 'Uchistem                         ( )   ' ; it(2*13) = 6
   OPTION(14) = 'Expchileaf                       ( )   ' ; it(2*14) = 6
   OPTION(15) = 'Uchileaf                         ( )   ' ; it(2*15) = 6
   OPTION(16) = 'Cdleaf                           ( )   ' ; it(2*16) = 6
   OPTION(17) = 'Arealeaf                         ( )   ' ; it(2*17) = 6
   
!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   HELPM ( 1) = 'Distance coeff (0.8)                                    ( ) '
   HELPM ( 2) = 'Uniform 1D width                                        ( ) '
   HELPM ( 3) = 'Uniform 1D height                                       ( ) '
   HELPM ( 4) = 'Uniform proftyp kn3=1,6 1=circle, 2=rect A/P, 3=rect K  ( ) '
   HELPM ( 5) = 'Uniform width  of 1D2D connection type 5 streetinlets   ( ) '
   HELPM ( 6) = 'Uniform height of 1D2D connection type 5 streetinlets   ( ) '
   HELPM ( 7) = 'Uniform proftyp kn3=5,  1=circle, 2=rect A/P, 3=rect K  ( ) '
   HELPM ( 8) = 'Uniform width  of 1D2D connection type 7 roofgutterpipes( ) '
   HELPM ( 9) = 'Uniform height of 1D2D connection type 7 roofgutterpipes( ) '
   HELPM (10) = 'Uniform proftyp kn3=7,  1=circle, 2=rect A/P, 3=rect K  ( ) '
   HELPM (11) = '0=no, 1 = yes                                           ( ) '
   HELPM (12) = '                                                        ( ) '
   HELPM (13) = '                                                        ( ) '
   HELPM (14) = '                                                        ( ) '
   HELPM (15) = '                                                        ( ) '
   HELPM (16) = '                                                        ( ) '
   HELPM (17) = '                                                        ( ) '
      
   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      IX(IL) = 13
      IX(IR) = 95
      IY(IL) = 2*I
      IY(IR) = 2*I
      IS(IL) = 82
      IS(IR) = 10
      IT(IL) = 1001
   ENDDO

!  Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW     = NPOS(3)
   IXP    = NPOS(1) + (IWS-IW)/2
   IYP    = NPOS(2)
   IH     = IHS - 9

!  Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program) // ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!  Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!  Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)

   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

!  NUMWNH = InfoWindow(1)
!  CALL IWinSelect(NUMWNH)

!  Define a new form by supplying arrays containing
!  field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

!  Define a help field and define help strings
!  for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

!   CALL IFORMputdouble  (2* 1 ,vicouv_filter, '(F7.3)' ) 
   CALL IFORMputinteger  (2* 1, jafilter)
   write(6,*) filterorder
   CALL IFORMputinteger  (2* 2, filterorder)
   w1 = wu1DUNI  ;  h1 = hh1DUNI
   w5 = wu1DUNI5 ;  h5 = hh1DUNI5
   w7 = wu1DUNI7 ;  h7 = hh1DUNI7
   CALL IFORMputdouble  (2* 3 , hh1DUNI , '(F7.3)' )       
   CALL IFORMputinteger (2* 4 , iproftypuni)       
   CALL IFORMputdouble  (2* 5 , wu1DUNI5, '(F7.3)' )       
   CALL IFORMputdouble  (2* 6 , hh1DUNI5, '(F7.3)' )       
   CALL IFORMputinteger (2* 7 , iproftypuni5)       
   CALL IFORMputdouble  (2* 8 , wu1DUNI7, '(F7.3)' )       
   CALL IFORMputdouble  (2* 9 , hh1DUNI7, '(F7.3)' )       
   CALL IFORMputinteger (2*10 , iproftypuni7)       
   CALL IFORMputinteger (2*11 , japiaczek33 )       
   CALL IFORMputdouble  (2*12 , Expchistem, '(F7.3)' )    
   CALL IFORMputdouble  (2*13 , Uchistem,   '(F7.3)' ) 
   CALL IFORMputdouble  (2*14 , Expchileaf, '(F7.3)' )  
   CALL IFORMputdouble  (2*15 , Uchileaf,   '(F7.3)' )  
   CALL IFORMputdouble  (2*16 , Cdleaf,     '(F7.3)' ) 
   CALL IFORMputdouble  (2*17 , Arealeaf,   '(F7.3)' ) 

   !  Display the form with numeric fields left justified
   !  and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!  check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN

!          CALL IFORMGETdouble  (2* 1 , vicouv_filter)
          CALL IFORMgetinteger (2* 1, jafilter)
          CALL IFORMgetinteger (2* 2 , filterorder  )       
          CALL IFORMgetdouble  (2* 3 , hh1DUNI  )       
          CALL IFORMgetinteger (2* 4 , iproftypuni)        
          CALL IFORMgetdouble  (2* 5 , wu1DUNI5 )       
          CALL IFORMgetdouble  (2* 6 , hh1DUNI5 ) 
          CALL IFORMgetinteger (2* 7 , iproftypuni5)        
          CALL IFORMgetdouble  (2* 8 , wu1DUNI7 )       
          CALL IFORMgetdouble  (2* 9 , hh1DUNI7 ) 
          CALL IFORMgetinteger (2*10 , iproftypuni7)        
          CALL IFORMgetinteger (2*11 , japiaczek33 )    
          CALL IFORMgetdouble  (2*12 , Expchistem )    
          CALL IFORMgetdouble  (2*13 , Uchistem   )
          CALL IFORMgetdouble  (2*14 , Expchileaf )  
          CALL IFORMgetdouble  (2*15 , Uchileaf   ) 
          CALL IFORMgetdouble  (2*16 , Cdleaf     ) 
          CALL IFORMgetdouble  (2*17 , Arealeaf   ) 
          do L = 1,Lnx1D
             if (prof1D(1,L) > 0) then  ! only direct profiles
                if (kcu(L) == 1 ) then 
                   if (wu1DUNI  .ne. w1 ) then  
                      prof1D(1,L) = wu1DUNI
                   else if (hh1DUNI  .ne. h1 ) then 
                      prof1D(2,L) = hh1DUNI
                   endif       
                else if (kcu(L) == 5 ) then 
                   if (wu1DUNI5 .ne. w5 ) then  
                       prof1D(1,L) = wu1DUNI5   
                   else if (hh1DUNI5 .ne. h5 ) then 
                      prof1D(2,L) = hh1DUNI5
                   endif       
                else if (kcu(L) == 7 ) then 
                   if (wu1DUNI7 .ne. w7 ) then  
                      prof1D(1,L) = wu1DUNI7
                   else if (hh1DUNI7 .ne. h7 ) then 
                      prof1D(2,L) = hh1DUNI7
                   endif       
                endif
                wu(L) = prof1D(1,L)
                if (abs(prof1D(3,L)) == 1) then ! circles are round 
                    prof1D(2,L) = prof1D(1,L)
                endif
             endif   
          enddo  
          
       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE CHANGENUMERICALPARAMETERS4
   
 SUBROUTINE CHANGEcolournumbers()
   use m_netw
   USE M_FLOW
   use m_flowgeom
   USE m_sferic
   use m_wind
   use unstruc_display
   use m_reduce
   use unstruc_version_module, only : unstruc_company, unstruc_program
   use unstruc_messages
   use m_fixedweirs
   use m_observations
   implicit none

   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 34 , NUMFLD = 2*NUMPAR)
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   integer :: nlevel
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line
!
   integer :: ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key, ja, niadvec
   integer :: nbut, imp, inp
   INTEGER :: KR , KG  , KB, KL
   
   KR = 0 ; KG = 0 ; KB = 0 ; KL = -1
   
   NLEVEL     = 4
   OPTION( 1) = 'NCOLDG=31          DESIGN GRID          ' ; it(2* 1) = 2
   OPTION( 2) = 'NCOLRG=212         PREVIOUS STATE GRID  ' ; it(2* 2) = 2
   OPTION( 3) = 'NCOLDN=3           DESIGN NET           ' ; it(2* 3) = 2
   OPTION( 4) = 'NCOLRN=211         PREVIOUS STATE NET   ' ; it(2* 4) = 2
   OPTION( 5) = 'NCOLNN=205         NETNODES             ' ; it(2* 5) = 2
   OPTION( 6) = 'NCOLSP=204         SPLINES              ' ; it(2* 6) = 2
   OPTION( 7) = 'NCOLLN=120         LAND BOUNDARY        ' ; it(2* 7) = 2
   OPTION( 8) = 'NCOLTX=210         TEXTLINES            ' ; it(2* 8) = 2
   OPTION( 9) = 'NCOLPL=221         POLYGON              ' ; it(2* 9) = 2
   OPTION(10) = 'NCOLCRS=230        CROSS SECTIONS       ' ; it(2*10) = 2
   OPTION(11) = 'NCOLTHD=231        THIN DAMS            ' ; it(2*11) = 2
   OPTION(12) = 'NCOLFXW=232        FIXED WEIRS          ' ; it(2*12) = 2
   OPTION(13) = 'NCOLHL=31          HIGHLIGHT NODES/LINKS' ; it(2*13) = 2
   OPTION(14) = 'KLVEC=4            VECTORS 110          ' ; it(2*14) = 2
   OPTION(15) = 'KLPROF=222         PROFILES             ' ; it(2*15) = 2
   OPTION(16) = 'KLSCL=221          ISOSCALE LEGEND      ' ; it(2*16) = 2
   OPTION(17) = 'KLTEX=3            NUMBERS              ' ; it(2*17) = 2
   OPTION(18) = 'KLOBS=221          OBSERVATION POINTS   ' ; it(2*18) = 2
   OPTION(19) = '                                        ' ; it(2*19) = 2
   OPTION(20) = 'Change RGB of colour Nr                 ' ; it(2*20) = 2
   OPTION(21) = 'R                                       ' ; it(2*21) = 2
   OPTION(22) = 'G                                       ' ; it(2*22) = 2
   OPTION(23) = 'B                                       ' ; it(2*23) = 2
   OPTION(24) = '                                        ' ; it(2*24) = 2
   OPTION(25) = 'R SCREEN                                ' ; it(2*25) = 2
   OPTION(26) = 'G SCREEN                                ' ; it(2*26) = 2
   OPTION(27) = 'B SCREEN                                ' ; it(2*27) = 2
   OPTION(28) = '                                        ' ; it(2*28) = 2
   OPTION(29) = 'R PLOT                                  ' ; it(2*29) = 2
   OPTION(30) = 'G PLOT                                  ' ; it(2*30) = 2
   OPTION(31) = 'B PLOT                                  ' ; it(2*31) = 2
   OPTION(32) = '                                        ' ; it(2*32) = 2
   OPTION(33) = 'JaFahrenheit                            ' ; it(2*33) = 2
   OPTION(34) = 'KLSRC=233         SORSIN                ' ; it(2*34) = 2
   
   
!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   HELPM ( 1) = '0<= ncol <=255                                              '
   HELPM ( 2) = '0<= ncol <=255                                              '
   HELPM ( 3) = '0<= ncol <=255                                              '
   HELPM ( 4) = '0<= ncol <=255                                              '
   HELPM ( 5) = '0<= ncol <=255                                              '
   HELPM ( 6) = '0<= ncol <=255                                              '
   HELPM ( 7) = '0<= ncol <=255                                              ' 
   HELPM ( 8) = '0<= ncol <=255                                              ' 
   HELPM ( 9) = '0<= ncol <=255                                              ' 
   HELPM (10) = '0<= ncol <=255                                              ' 
   HELPM (11) = '0<= ncol <=255                                              ' 
   HELPM (12) = '0<= ncol <=255                                              ' 
   HELPM (13) = '0<= ncol <=255                                              ' 
   HELPM (14) = '0<= ncol <=255                                              ' 
   HELPM (15) = '0<= ncol <=255                                              ' 
   HELPM (16) = '0<= ncol <=255                                              ' 
   HELPM (17) = '0<= ncol <=255                                              ' 
   HELPM (18) =    '0<= ncol <=255                                              ' 
   HELPM (1:31) =  '0<= ncol <=255                                              ' 
   HELPM (32:33) = '0/1                                                         '   
   HELPM (34) = '0<= ncol <=255                                              ' 
   
   CALL SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      IX(IL) = 13
      IX(IR) = 95
      IY(IL) = I
      IY(IR) = I
      IS(IL) = 82
      IS(IR) = 10
      IT(IL) = 1001
   ENDDO

!  Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW     = NPOS(3)
   IXP    = NPOS(1) + (IWS-IW)/2
   IYP    = NPOS(2)
   IH     = IHS - 9

!  Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program) // ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!  Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!  Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)

   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

!  NUMWNH = InfoWindow(1)
!  CALL IWinSelect(NUMWNH)

!  Define a new form by supplying arrays containing
!  field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

!  Define a help field and define help strings
!  for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFormPutINTEGER (2* 1 ,NCOLDG     )            
   CALL IFORMPUTINTEGER (2* 2 ,NCOLRG     )            
   CALL IFORMPUTINTEGER (2* 3 ,NCOLDN     )    
   CALL IFORMPUTINTEGER (2* 4 ,NCOLRN     )            
   CALL IFORMPUTINTEGER (2* 5 ,NCOLNN     )            
   CALL IFORMPUTINTEGER (2* 6 ,NCOLSP     )        
   CALL IFORMPUTINTEGER (2* 7 ,NCOLLN     )          
   CALL IFORMPUTINTEGER (2* 8 ,NCOLTX     )          
   CALL IFORMPUTINTEGER (2* 9 ,NCOLPL     )          
   CALL IFORMPUTINTEGER (2*10 ,NCOLCRS    )          
   CALL IFORMPUTINTEGER (2*11 ,NCOLTHD    )            
   CALL IFORMPUTINTEGER (2*12 ,NCOLFXW    )          
   CALL IFORMPUTINTEGER (2*13 ,NCOLHL     )    
   CALL IFORMputINTEGER (2*14 ,KLVEC      )   
   CALL IFORMputINTEGER (2*15 ,KLPROF     )   
   CALL IFORMPUTINTEGER (2*16 ,KLSCL      )    
   CALL IFORMputINTEGER (2*17 ,KLTEX      )   
   CALL IFORMputINTEGER (2*18 ,KLOBS      )
                           
   CALL IFORMputINTEGER (2*20 ,KL         )   
   CALL IFORMputINTEGER (2*21 ,KR         )   
   CALL IFORMputINTEGER (2*22 ,KG         )   
   CALL IFORMputINTEGER (2*23 ,KB         )   
                           
   CALL IFORMPUTINTEGER (2*25 ,NREDS      ) 
   CALL IFORMPUTINTEGER (2*26 ,NGREENS    ) 
   CALL IFORMPUTINTEGER (2*27 ,NBLUES     ) 
                           
   CALL IFORMPUTINTEGER (2*29 ,NREDP      ) 
   CALL IFORMPUTINTEGER (2*30 ,NGREENP    ) 
   CALL IFORMPUTINTEGER (2*31 ,NBLUEP     ) 

   CALL IFORMPUTINTEGER (2*33 ,Jafahrenheit) 
   CALL IFORMPUTINTEGER (2*34 ,KLSRC) 
   
   !  Display the form with numeric fields left justified
   !  and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!  check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN

          CALL IFORMGETINTEGER (2* 1 ,NCOLDG     )            
          CALL IFORMGETINTEGER (2* 2 ,NCOLRG     )            
          CALL IFORMGETINTEGER (2* 3 ,NCOLDN     )    
          CALL IFORMGETINTEGER (2* 4 ,NCOLRN     )            
          CALL IFORMGETINTEGER (2* 5 ,NCOLNN     )            
          CALL IFORMGETINTEGER (2* 6 ,NCOLSP     )        
          CALL IFORMGETINTEGER (2* 7 ,NCOLLN     )          
          CALL IFORMGETINTEGER (2* 8 ,NCOLTX     )          
          CALL IFORMGETINTEGER (2* 9 ,NCOLPL     )          
          CALL IFORMGETINTEGER (2*10 ,NCOLCRS    )          
          CALL IFORMGETINTEGER (2*11 ,NCOLTHD    )            
          CALL IFORMGETINTEGER (2*12 ,NCOLFXW    )          
          CALL IFORMGETINTEGER (2*13 ,NCOLHL     )    
          CALL IFORMGEtINTEGER (2*14 ,KLVEC      )   
          CALL IFORMGEtINTEGER (2*15 ,KLPROF     )   
          CALL IFORMGETINTEGER (2*16 ,KLSCL      )    
          CALL IFORMGEtINTEGER (2*17 ,KLTEX      )   
          CALL IFORMGEtINTEGER (2*18 ,KLOBS      )   
          
          CALL IFORMGETINTEGER (2*20 ,KL         )  ; KL   = MIN(255,       KL  ) 
          CALL IFORMGETINTEGER (2*21 ,KR         )  ; KR   = MIN(255, MAX(0,KR  )) 
          CALL IFORMGETINTEGER (2*22 ,KG         )  ; KG   = MIN(255, MAX(0,KG  )) 
          CALL IFORMGETINTEGER (2*23 ,KB         )  ; KB   = MIN(255, MAX(0,KB  )) 
                           
          CALL IFORMGETINTEGER (2*25 ,NREDS      )  ; NREDS   = MIN(255, MAX(0,NREDS  )) 
          CALL IFORMGETINTEGER (2*26 ,NGREENS    )  ; NGREENS = MIN(255, MAX(0,NGREENS)) 
          CALL IFORMGETINTEGER (2*27 ,NBLUES     )  ; NBLUES  = MIN(255, MAX(0,NBLUES ))

          CALL IFORMGETINTEGER (2*29 ,NREDP      )  ; NREDP   = MIN(255, MAX(0,NREDP  )) 
          CALL IFORMGETINTEGER (2*30 ,NGREENP    )  ; NGREENP = MIN(255, MAX(0,NGREENP)) 
          CALL IFORMGETINTEGER (2*31 ,NBLUEP     )  ; NBLUEP  = MIN(255, MAX(0,NBLUEP )) 
          
          CALL IFORMGEtINTEGER (2*33 ,JaFahrenheit  )   
          CALL IFORMGEtINTEGER (2*34 ,KLSRC         )   
          
          IF (KL > 0) THEN 
              CALL IGRPALETTERGB(KL,KR,KG,KB) 
          ENDIF   
          
       ENDIF         
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE CHANGEcolournumbers

   
  SUBROUTINE CHANGETIMEPARAMETERS()
   USE M_FLOWTIMES
   use unstruc_display
   use unstruc_version_module, only : unstruc_company, unstruc_program
   use unstruc_messages
   implicit none
   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 13, NUMFLD = 2*NUMPAR)
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   integer :: nlevel
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line
!
   integer :: ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key
   integer :: nbut, imp, inp

   NLEVEL     = 4
   OPTION( 1) = 'Dt_user                             (s) ' ; it(2*1)  = 6
   OPTION( 2) = 'Dt_max                              (s) ' ; it(2*2)  = 6
   OPTION( 3) = 'Use automatic time step or not (1/0)( ) ' ; it(2*3)  = 2
   OPTION( 4) = 'Tstart_user                         (s) ' ; it(2*4)  = 6
   OPTION( 5) = 'Tstop_user                          (s) ' ; it(2*5)  = 6
   OPTION( 6) = 'HisInterval                         (s) ' ; it(2*6)  = 6
   OPTION( 7) = 'MapInterval                         (s) ' ; it(2*7)  = 6
   OPTION( 8) = 'RstInterval                         (s) ' ; it(2*8)  = 6
   OPTION( 9) = 'WaqInterval                         (s) ' ; it(2*9)  = 6
   OPTION(10) = 'Initial timestep                    (s) ' ; it(2*10) = 6
   OPTION(11) = 'Current time                        (s) ' ; it(2*11) = 6
   OPTION(12) = 'Smoothing time boundaries Tlfsmo    (s) ' ; it(2*12) = 6
   OPTION(13) = 'Dtfacmax                            ( ) ' ; it(2*13) = 6

   
!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   HELPM ( 1) = 'User timestep (rythm of external forcing updates)           '
   HELPM ( 2) = 'Max timestep                                                '
   HELPM ( 3) = 'Use automatic time step (CFL-based) or not (1 or 0)         '
   HELPM ( 4) = '                                                            '
   HELPM ( 5) = '                                                            '
   HELPM ( 6) = '                                                            '
   HELPM ( 7) = '                                                            '
   HELPM ( 8) = '                                                            '
   HELPM ( 9) = '                                                            '
   HELPM (10) = '                                                            '
   HELPM (11) = '                                                            '
   HELPM (12) = '                                                            '
   HELPM (13) = 'dt = min(dtnew, dtfacmax*dtold)                             '
   
   CALL SAVEKEYS()

   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      IX(IL) = 13
      IX(IR) = 95
      IY(IL) = 2*I
      IY(IR) = 2*I
      IS(IL) = 82
      IS(IR) = 10
      IT(IL) = 1001
   ENDDO

!  Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW     = NPOS(3)
   IXP    = NPOS(1) + (IWS-IW)/2
   IYP    = NPOS(2)
   IH     = IHS - 9

!  Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program) // ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!  Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!  Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)

   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

!  NUMWNH = InfoWindow(1)
!  CALL IWinSelect(NUMWNH)

!  Define a new form by supplying arrays containing
!  field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

!  Define a help field and define help strings
!  for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFormPutDouble  (2* 1 ,dt_user          ,'(F10.3)')
   CALL IFormPutDouble  (2* 2 ,dt_max           ,'(F10.3)')
   CALL IFORMPUTINTEGER (2* 3 ,ja_timestep_auto           )
   CALL IFormPutDouble  (2* 4 ,tstart_user      ,'(F10.0)')
   CALL IFormPutDouble  (2* 5 ,tstop_user       ,'(F10.0)')
   CALL IFormPutDouble  (2* 6 ,ti_his           ,'(F10.3)')
   CALL IFormPutDouble  (2* 7 ,ti_map           ,'(F10.3)')
   CALL IFormPutDouble  (2* 8 ,ti_rst           ,'(F10.3)')
   CALL IFormPutDouble  (2* 9 ,ti_waq           ,'(F10.3)')
   CALL IFormPutDouble  (2*10 ,dt_init          ,'(F10.3)')
   CALL IFormPutDouble  (2*11 ,time1            ,'(F10.3)')
   CALL IFormPutDouble  (2*12 ,Tlfsmo           ,'(F10.3)')
   CALL IFormPutDouble  (2*13 ,Dtfacmax         ,'(F10.3)')
   
   ! Display the form with numeric fields left justified
   ! and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!  check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN

           CALL IFormGetDouble  (2* 1 ,dt_user          )
           CALL IFormGetDouble  (2* 2 ,dt_max           )
           CALL IFORMgeTINTEGER (2* 3 ,ja_timestep_auto )
           CALL IFormGetDouble  (2* 4 ,tstart_user      )
           CALL IFormGetDouble  (2* 5 ,tstop_user       )
           CALL IFormGetDouble  (2* 6 ,ti_his           )
           CALL IFormGetDouble  (2* 7 ,ti_map           )
           CALL IFormGetDouble  (2* 8 ,ti_rst           )
           CALL IFormGetDouble  (2* 9 ,ti_waq           )
           CALL IFormGetDouble  (2*10 ,dt_init          )
           CALL IFormGetDouble  (2*12 ,Tlfsmo           )
           CALL IFormGetDouble  (2*13 ,Dtfacmax         )
           
           if (dt_max > dt_user) then
               dt_max = dt_user
               write(msgbuf, '(a,f9.6,a)') 'DtMax should be <= DtUser. It has been reset to: ', dt_max
               call msg_flush()
           end if

       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE CHANGETIMEPARAMETERS


   SUBROUTINE CHANGEPHYSICALPARAMETERS()
   use m_netw
   USE M_FLOW
   use m_flowgeom
   USE M_FLOWTIMES
   USE m_sferic
   use m_wind
   use unstruc_display
   use unstruc_version_module, only : unstruc_company, unstruc_program
   implicit none

   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 15, NUMFLD = 2*NUMPAR)
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   integer :: nlevel
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line
!
   integer :: ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key, ierr
   integer :: nbut, imp, inp
   double precision :: frcuniorg

   NLEVEL     = 4
   OPTION( 1) = 'frcuni                                  ' ; it(2* 1) = 6
   OPTION( 2) = 'ifrctypuni Friction formulation         ' ; it(2* 2) = 2
   OPTION( 3) = 'Windspeed     (m/s)                     ' ; it(2* 3) = 6
   OPTION( 4) = 'Winddirection ( ) 90= to East 0=to North' ; it(2* 4) = 6
   OPTION( 5) = 'vicouv                           (m2/s) ' ; it(2* 5) = 6
   OPTION( 6) = 'Vicoww                           (m2/s) ' ; it(2* 6) = 6  
   OPTION( 7) = 'Dicouv                           ( )    ' ; it(2* 7) = 6
   OPTION( 8) = 'Dicoww                           ( )    ' ; it(2* 8) = 6
   OPTION( 9) = 'Verticall Wall Nikuradse         (m)    ' ; it(2* 9) = 6
   OPTION(10) = 'Smagorinsky                      ( )    ' ; it(2*10) = 6
   OPTION(11) = 'Elder                            ( )    ' ; it(2*11) = 6
   OPTION(12) = 'uniform friction coefficient 1D         ' ; it(2*12) = 6 
   OPTION(13) = 'uniform friction coefficient 1D2D intern' ; it(2*13) = 6       
   OPTION(14) = 'uniform friction coefficient 1D groundly' ; it(2*14) = 6       
   OPTION(15) = 'uniform rainfall              (mm/hr)   ' ; it(2*15) = 6        

   
!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   HELPM ( 1) = 'uniform friction coefficient                                '
   HELPM ( 2) = ' 0=Chz, 1=Mann, 2=White-Col, 3=White-Col-Waqua, 10=Glass    '
   HELPM ( 3) = '                                                            '
   HELPM ( 4) = '                                                            '
   HELPM ( 5) = 'background horizontal viscosity                             '
   HELPM ( 6) = 'background vertical   viscosity (0: no vert. visc. at all)  '
   HELPM ( 7) = 'background horizontal diffusivity                           '
   HELPM ( 8) = 'background vertical   diffusivity (0: no vert. diff. at all)'
   HELPM ( 9) = 'VERTICAL WALL NIKURADSE ROUGHNESS, (wall_z0 = KS/30)     (M)'
   HELPM (10) = 'vicuv = vicuv + ( (Smagorinsky*dx)**2)*Strainrate_S, eg 0.1 '
   HELPM (11) = 'vicuv = vicuv +    Elder*0.009*H*U                   eg 1.0 '
   HELPM (12) = 'uniform friction coefficient 1D                             '
   HELPM (13) = 'uniform friction coefficient 1D2D internal Link             '
   HELPM (14) = 'uniform friction coefficient 1D groundlayer                 '
   HELPM (15) = '(if non-zero overrides ext forcings)                        '
   
   CALL SAVEKEYS()

   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      IX(IL) = 13
      IX(IR) = 95
      IY(IL) = 2*I
      IY(IR) = 2*I
      IS(IL) = 82
      IS(IR) = 10
      IT(IL) = 1001
   ENDDO

!  Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW     = NPOS(3)
   IXP    = NPOS(1) + (IWS-IW)/2
   IYP    = NPOS(2)
   IH     = IHS - 9

!  Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program)// ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!  Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!  Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)

   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

!  NUMWNH = InfoWindow(1)
!  CALL IWinSelect(NUMWNH)

!  Define a new form by supplying arrays containing
!  field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

!  Define a help field and define help strings
!  for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   frcuniorg = frcuni
   CALL IFormPutDouble  (2* 1 , frcuni, '(F8.3)')
   CALL IFORMPUTinteger (2* 2 , ifrctypuni      )
   CALL IFormPutDouble  (2* 3 , windsp ,'(F8.3)')
   CALL IFormPutDouble  (2* 4 , winddir,'(F8.3)')
   CALL IFormPutDouble  (2* 5 , vicouv ,'(e8.3)')
   CALL IFormPutDouble  (2* 6 , vicoww ,'(e8.3)')
   CALL IFORMPUTdouble  (2* 7 , dicouv, '(e8.3)')
   CALL IFORMPUTdouble  (2* 8 , dicoww, '(e8.3)')
   CALL IFormPutDouble  (2* 9 , wall_ks,'(F8.3)')
   CALL IFormPutDouble  (2*10 , Smagorinsky,'(F8.3)')
   CALL IFormPutDouble  (2*11 , Elder,      '(F8.3)')
   CALL IFormPutDouble  (2*12 , frcuni1D,   '(F8.3)')
   CALL IFormPutDouble  (2*13 , frcuni1D2D, '(F8.3)')
   CALL IFormPutDouble  (2*14 , frcuni1Dgrounlay, '(F8.3)')
   CALL IFormPutDouble  (2*15 , rainuni   , '(F8.3)')
 
   !  Display the form with numeric fields left justified
   !  and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!  check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN

           CALL IFormGetDouble  (2* 1 , frcuni )
           CALL IFORMGETinteger (2* 2 , ifrctypuni)
           CALL IFormGetDouble  (2* 3 , windsp )
           CALL IFormGetDouble  (2* 4 , winddir)
           CALL IFormGetDouble  (2* 5 , vicouv )
           CALL IFormGetDouble  (2* 6 , vicoww )
           CALL IFORMGetdouble  (2* 7 , dicouv        )
           CALL IFORMGetdouble  (2* 8 , dicoww        )
           CALL IFormGetDouble  (2* 9 , wall_ks)
           CALL IFormGetDouble  (2*10 , Smagorinsky)
           CALL IFormGetDouble  (2*11 , Elder)
           CALL IFormGetDouble  (2*12 , frcuni1D)
           CALL IFormGetDouble  (2*13 , frcuni1D2D)
           CALL IFormGetDouble  (2*14 , frcuni1Dgrounlay)
           CALL IFormGetDouble  (2*15 , rainuni)
           
           if (allocated (frcu) .and. frcuniorg .ne. frcuni) then 
               frcu = frcuni
           endif
           
           if (rainuni > 0d0) then 
              if (.not. allocated(rain) ) then
                 allocate ( rain(ndx) , stat=ierr) ; rain = 0d0
                 call aerr('rain(ndx)', ierr, ndx)
              endif
              jarain = 1 ; jaqin = 1
           endif   

           wall_z0 = wall_ks / 30d0
           if (windsp .ne. 0d0) then
              call setuniformwind()
           endif

       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE CHANGEPHYSICALPARAMETERS

   SUBROUTINE CHANGEgeometryPARAMETERS()
   use m_netw
   USE M_FLOW
   use m_flowgeom
   USE M_FLOWTIMES
   USE m_sferic
   use m_wind
   use unstruc_display
   use m_fixedweirs
   use unstruc_version_module, only : unstruc_company, unstruc_program
   implicit none

   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 23, NUMFLD = 2*NUMPAR)
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
   integer :: nlevel
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line
!
   integer :: ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key
   integer :: nbut, imp, inp

   NLEVEL    = 4

   OPTION( 1)= 'sini                                (m) ' ; it(2* 1) = 6
   OPTION( 2)= 'zkuni                               (m) ' ; it(2* 2) = 6
   OPTION( 3)= 'numtopsig                           ( ) ' ; it(2* 3) = 2
   OPTION( 4)= 'anglat                            (deg) ' ; it(2* 4) = 6
   OPTION( 5)= 'ibedlevtyp                          ( ) ' ; it(2* 5) = 2
   OPTION( 6)= 'Kmx, nr of Vertical sigma layers    ( ) ' ; it(2* 6) = 2
   OPTION( 7)= 'Jazlayercenterbedvel                ( ) ' ; it(2* 7) = 2
   OPTION( 8)= 'Jasfer3D                            ( ) ' ; it(2* 8) = 2
   OPTION( 9)= 'Jalimnor                            ( ) ' ; it(2* 9) = 2
   OPTION(10)= 'minimum 1D link length,            (m ) ' ; it(2*10) = 6
   OPTION(11)= 'Uniform 1D link width              (m ) ' ; it(2*11) = 6
   OPTION(12)= '1D profile type                    (  ) ' ; it(2*12) = 2
   OPTION(13)= '2D conveyance                      (  ) ' ; it(2*13) = 2
   OPTION(14)= 'non linear continuity 2D           (  ) ' ; it(2*14) = 2
   OPTION(15)= 'non linear continuity 1D           (  ) ' ; it(2*15) = 2
   OPTION(16)= 'sdropstep  when dropping water      (m) ' ; it(2*16) = 6
   OPTION(17)= 'zkdropstep when dropping land       (m) ' ; it(2*17) = 6
   OPTION(18)= 'Ifixedweirscheme                    ( ) ' ; it(2*18) = 2
   OPTION(19)= 'Layertype                           ( ) ' ; it(2*19) = 2
   OPTION(20)= 'Sigmagrowthfactor                   ( ) ' ; it(2*20) = 6
   OPTION(21)= 'Sillheightmin                       ( ) ' ; it(2*21) = 6
   OPTION(22)= 'Mxlayz nr of vertical z-layers      ( ) ' ; it(2*22) = 2
   OPTION(23)= 'Output full time-varying grid data  ( ) ' ; it(2*23) = 2
   

!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   HELPM (1) = 'initial waterlevel                                          '
   HELPM (2) = 'uniform bottom level                                        '
   HELPM (3) = 'number of sigma top layers in Z-model                       '
   HELPM (4) = 'angle of latitude, Delft = 52.0                             '
   HELPM (5) = '1=cell tiledep bl, 2=u-point blu, 3=netw,mean-u 4=netw, maxu'
   HELPM (6) = '0=2D ORIGINAL CODE, 1=2D IN 3D CODE, >1= 3D CODE            '
   HELPM (7) = '1=orig, 2=sigma-like                                        '
  !HELPM (7) = '0=D3D, 0.5dx outside, 1=on net bnd, 2=on polylin (not yet)  '
   HELPM (8) = '0=org, 1=jasfer3D                                           '
   HELPM (9) = 'Jalimnor                                                    '
   HELPM (10)= 'dxmin1D (except for duikers)                                '
   HELPM (11)= 'wu1DUNI                                                     '
   HELPM (12)= '1=circle, 2=rectan, 3=rectan (peri=wid), 4=3,nonlin         '
   HELPM (13)= '0:R=H, 1:R=A/P, 2:K=analytic-1D conv, 3:K=analytic-2D conv  '
   HELPM (14)= 'only for ibedlevtyp==3 and 2D conveyance >=1                '
   HELPM (15)= 'nonlin = max (nonlin1D, nonlin2D)                           '
   HELPM (16)= 'Specify absolute value for dropping water.                  '
   HELPM (17)= 'Specify absolute value for dropping land.                   '
   HELPM (18)= '0=only setbobs, 1=small stencil, 2=full subgrid weir        '
   HELPM (19)= '1=all sigma, 2=all z, 3=left sigma, 4=left z                '
   HELPM (20)= '1d0=uniform, 1.1d0 = increase factor from bottom up         '
   HELPM (21)= 'Only Fixedweirs if both left and right sillheight > Sillmin '
   HELPM (22)= 'max nr of z-layers                                          '
   HELPM (23)= '0=compact, 1=full                                           '


   CALL SAVEKEYS()


   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      IX(IL) = 13
      IX(IR) = 95
      IY(IL) = 2*I
      IY(IR) = 2*I
      IS(IL) = 82
      IS(IR) = 10
      IT(IL) = 1001
   ENDDO

!  Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW     = NPOS(3)
   IXP    = NPOS(1) + (IWS-IW)/2
   IYP    = NPOS(2)
   IH     = IHS - 9

!  Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program)// ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!  Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!  Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)

   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

!  NUMWNH = InfoWindow(1)
!  CALL IWinSelect(NUMWNH)

!  Define a new form by supplying arrays containing
!  field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

!  Define a help field and define help strings
!  for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFormPutDouble  (2*1 ,sini,     '(F8.3)')
   CALL IFormPutDouble  (2*2 ,zkuni,    '(F8.3)')
   CALL IFormPutInteger (2*3 ,numtopsig         )
   CALL IFormPutDouble  (2*4 ,anglat,   '(F8.3)')
   CALL IFORMPUTINTEGER (2*5 ,ibedlevtyp        )
   CALL IFORMPUTINTEGER (2*6 ,kmx               )
   CALL IFORMPUTINTEGER (2*7 ,Jazlayercenterbedvel)
   CALL IFORMPUTINTEGER (2*8 ,jasfer3D          )
   CALL IFORMPUTinteger (2*9 ,jalimnor          )
   CALL IFORMPUTdouble  (2*10,dxmin1D,   '(F8.3)' )
   CALL IFORMPUTdouble  (2*11,wu1DUNI ,   '(F8.3)' )
   CALL IFORMPUTINTEGER (2*12,iproftypuni          )
   CALL IFORMPUTINTEGER (2*13,jaconveyance2D    )
   CALL IFORMPUTINTEGER (2*14,nonlin2D          )
   CALL IFORMPUTINTEGER (2*15,nonlin1D          )
   CALL IFormPutDouble  (2*16,sdropstep,  '(F8.3)')
   CALL IFormPutDouble  (2*17,zkdropstep, '(F8.3)')
   CALL IFORMPUTINTEGER (2*18,ifixedweirscheme     )
   CALL IFORMPUTINTEGER (2*19,Layertype           )
   CALL IFormPutDouble  (2*20,Sigmagrowthfactor, '(F8.3)')
   CALL IFormPutDouble  (2*21,Sillheightmin    , '(F8.3)')
   CALL IFORMPUTINTEGER (2*22,Mxlayz              )
   CALL IFORMPUTINTEGER (2*23,jafullgridoutput    )

   !  Display the form with numeric fields left justified
   !  and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!  check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN
           CALL IFormGetDouble  (2*1 ,sini  )
           CALL IFormGetDouble  (2*2 ,zkuni )
           CALL IFormGetinteger (2*3 ,numtopsig   )
           CALL IFormGetDouble  (2*4 ,anglat      )
           CALL IFORMgeTINTEGER (2*5 ,ibedlevtyp  )
           CALL IFORMgeTINTEGER (2*6 ,kmx         ) 
           CALL IFORMgeTINTEGER (2*7 ,Jazlayercenterbedvel  )
           CALL IFORMgeTINTEGER (2*8 ,jasfer3D    )
           CALL IFORMgetinteger (2*9 ,jalimnor    )
           CALL IFORMgetdouble  (2*10,dxmin1D        )
           CALL IFORMgetdouble  (2*11,wu1DUNI        )
           CALL IFORMgeTINTEGER (2*12,iproftypuni    )
           CALL IFORMgeTINTEGER (2*13,jaconveyance2D )
           CALL IFORMgeTINTEGER (2*14,nonlin2D       )
           CALL IFORMgeTINTEGER (2*15,nonlin1D       )
           CALL IFormGetDouble  (2*16,sdropstep)
           CALL IFormGetDouble  (2*17,zkdropstep)
           CALL IFORMgeTINTEGER (2*18,ifixedweirscheme)
           CALL IFORMgeTINTEGER (2*19,Layertype )
           CALL IFormGetDouble  (2*20,Sigmagrowthfactor)
           CALL IFormGetDouble  (2*21,Sillheightmin)
           CALL IFORMgetINTEGER (2*22,Mxlayz       )
           CALL IFORMgeTINTEGER (2*23,jafullgridoutput)
           if (kmx > 0 .or. mxlayz > 0) then
              if (layertype > 1) then 
                 kmx = max(kmx,mxlayz) ; iadvec = 33
              endif 
           endif

           if (kmx == 0 .and. ja_timestep_auto .ne. 0) ja_timestep_auto = 1
           
           if (ibedlevtyp .ne. 3) then
              jaconveyance2D = -1
              nonlin2D       = 0
           else if (nonlin2d > 0 .and. jaconveyance2D == 0 ) then
              jaconveyance2D = -1
           endif
           nonlin = max(nonlin1D, nonlin2D) 
           call inisferic()
       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE CHANGEgeometryPARAMETERS

      SUBROUTINE CHANGEGRIDPARAMETERS()
      USE M_GRID
      USE M_GRIDSETTINGS
      use m_sferic
      use unstruc_display
      use m_polygon
      use unstruc_version_module, only : unstruc_company, unstruc_program
      implicit none

      integer :: numpar, numfld, numparactual, numfldactual
      PARAMETER  (NUMPAR = 15, NUMFLD = 2*NUMPAR)
      INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
      integer :: nlevel
      CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      integer, external :: infoinput
      external :: highlight_form_line
!
      integer :: ip,ir, il, iw, ixp, iyp, ih, i, iuvfieldorg, ifexit, ifinit, key
      integer :: nbut, imp, inp, k
      double precision :: phi

      NLEVEL    = 3
      OPTION(1) = 'M-REFINEMENT FACTOR                     '
      OPTION(2) = 'N-REFINEMENT FACTOR                     '
      OPTION(3) = 'NR SMOOTHING ITERATIONS                 '
      OPTION(4) = 'SMOOTHING PARAMETER                     '
      OPTION(5) = 'ATTRACTION/REPULSION PARAMETER          '
      OPTION(6) = 'PASSIVE GRID OR GRID FIXED IN PASTE     '
      OPTION(7) = 'GO BACK TO STARTUP DIRECTORY YES/NO     '
      OPTION(8) = 'LINE OR SPLINE REPRESENTATION  (0.0-1.0)'
      OPTION(9) = 'EQUIDISTANT OR SMOOTH INTERPOL (0.0-1.0)'
      OPTION(10)= 'INCREASE FACTOR IN LINE MIRROR  (0.1-10)'
      OPTION(11)= 'Spherical or Cartesian coordinates (1 0)'
      OPTION(12)= 'DRAW STEREOGRAPHIC OR NO PROJECTION(1 0)'
!     pillar grid
      option(13)= 'PILLAR RADIUS (m)                       '
      option(14)= 'PILLAR X-COORDINATE                     '
      option(15)= 'PILLAR Y-COORDINATE                     '
!
!      123456789012345678901234567890123456789012345678901234567890
!               1         2         3         4         5         6
      HELPM (1) = 'INTEGER VALUE <                                             '
      HELPM (2) = 'INTEGER VALUE <                                             '
      HELPM (3) = 'SMOOTHING, EDIT  : (0.0 - 100)  DEFAULT = 20,  INTERMEDIATE '
      HELPM (4) = 'SMOOTHING  EDIT  : (0.0 - 1.0)  DEFAULT = 0.2, INTERMEDIATE '
      HELPM (5) = 'ATTRACT./REPULS. : (0.0 - 0.5)  DEFAULT = 0.1, INTERMEDIATE '
      HELPM (6) = 'GRID PASTE       : (0.0 - 1.0)  0.0: GRID FIXED, 1.0:PASSIVE'
      HELPM (7) = 'ALWAYS BACK TO STARTUP DIRECTORY (1) OR KEEP NEW DIR. (0)   '
      HELPM (8) = 'STRAIGHT LINES REPRESENTATION = 0, CURVED LINES = 1         '
      HELPM (9) = 'SPLINES TO GRID  : (0.0 - 1.0) DEFAULT = 1.0, SMOOTH INTERP.'
      HELPM (10)= 'GRID SIZE INCREASE IN LINE MIRROR, 1.0 = EQUAL SIZE         '
      HELPM (11)= '1 = Spherical, 0 = Cartesian                                '
      HELPM (12)= '1 = STEREOGRAPHIC PROJECTION , 0 = NO PROJECTION            '
      HELPM (13)= 'SET RADIUS TO 0 FOR NO PILLAR                               '
      HELPM (14)= '                                                            '
      HELPM (14)= '                                                            '


      CALL SAVEKEYS()
      IP = 20
      WRITE(HELPM(1)(IP:IP+4),'(I5)') MIN(MMAX-1, 1 + (MMAX-1)/MAX(1,(MC-1)) )
      WRITE(HELPM(2)(IP:IP+4),'(I5)') MIN(NMAX-1, 1 + (NMAX-1)/MAX(1,(NC-1)) )

      IF (JDEMO .EQ. 1) THEN
         NUMPARACTUAL = 6
      ELSE
         NUMPARACTUAL = NUMPAR
      ENDIF
      NUMFLDACTUAL = 2*NUMPARACTUAL

      IR = 0
      DO 10 I = 1,NUMPARACTUAL
         IL = IR + 1
         IR = IL + 1
         IX(IL) = 13
         IX(IR) = 95
         IY(IL) = 2*I
         IY(IR) = 2*I
         IS(IL) = 82
         IS(IR) = 10
         IT(IL) = 1001
         IF (I .LE. 3 .OR. I == 7 .OR. I == 10 .OR. I == 11 .OR. I==12) THEN
            IT(IR) = 2
         ELSE
            IT(IR) = 6
         ENDIF
    10 CONTINUE

!     Initialise
      CALL IWinWordWrap('OFF')
      CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
      CALL INHIGHLIGHT('WHITE','RED')
      IW     = NPOS(3)
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2)
      IH     = IHS - 9

!     Header of filewindow
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP,IW,1)
      CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
      CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program)// ' PARAMETER FORM')
      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!     Explain keyfunctions in bottom window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IHS-1,IW,2)
      CALL IWinOutStringXY (1,1,'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
      CALL IWinOutStringXY (1,2,'right mouse = Enter, click outside window = Esc')

!     Filewindow is middelste window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP+3,IW,IH)

      CALL InControlKey(29,129)
      CALL InControlKey(30,128)

!     NUMWNH = InfoWindow(1)
!     CALL IWinSelect(NUMWNH)

!     Define a new form by supplying arrays containing
!     field positions, sizes and types
      CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

!     Define a help field and define help strings
!     for 2 of the 4 input fields
      CALL IFORMHELP(13,IH,60)

      IR = 0
      DO 20 I = 1,NUMPARACTUAL
         IL = IR + 1
         IR = IL + 1
         CALL IFORMPUTSTRING (IL,OPTION(I))
         CALL IFORMPUTHELP   (IR,HELPM(I))
         CALL IFORMATTRIBUTEN(IR,0,0,7)
    20 CONTINUE

      CALL IFORMPUTINTEGER(2*1,MFAC)
      CALL IFORMPUTINTEGER(2*2,NFAC)
      CALL IFORMPUTINTEGER(2*3,ITSMO)
      CALL IFormPutDouble (2*4,CSMO,'(F5.3)')
      CALL IFormPutDouble (2*5,RFAC,'(F5.3)')
      CALL IFormPutDouble (2*6,BAAS2,'(F5.3)')
      CALL IFORMPUTINTEGER(2*7,KEEPSTARTDIR)
      CALL IFormPutDouble (2*8,SPLFAC,'(F5.3)')
      CALL IFormPutDouble (2*9,SPLFAC2,'(F5.3)')
      CALL IFormPutDouble (2*10,FACMIR,'(F5.3)')
      CALL IFORMPUTINTEGER(2*11,jsferic)
      CALL IFORMPUTINTEGER(2*12,jsferTEK)
      CALL IFormPutDouble (2*13,pil_rad,'(F7.3)')
      CALL IFormPutDouble (2*14,pil_x,  '(F7.3)')
      CALL IFormPutDouble (2*15,pil_y,  '(F7.3)')


!   Diplay the form with numeric fields left justified
!   an set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

    30 CONTINUE
      IFINIT = IFEXIT
      CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!     check for Help, Confirm, Quit
      KEY = INFOINPUT(55)
      IF (KEY .EQ. -2) THEN
          NBUT = INFOINPUT(61)
          IF (NBUT .GE. 1) THEN
             IMP = INFOINPUT(62) + 1
             INP = INFOINPUT(63) + 1
             IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.    &
                 INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
                IF (NBUT .EQ. 1) THEN
                   KEY = 21
                ELSE
                   KEY = 22
                ENDIF
             ELSE
                KEY = 23
             ENDIF
          ENDIF
      ELSE IF (KEY .EQ. -1) THEN
         KEY = INFOINPUT(57)
      ENDIF
      IF (KEY .EQ. 26) THEN
          WRDKEY = OPTION(IFEXIT/2)
          CALL HELP(WRDKEY,NLEVEL)
      ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
          IF (KEY .EQ. 22) THEN
              CALL IFORMGETINTEGER(2*1,MFAC)
              CALL IFORMGETINTEGER(2*2,NFAC)
              CALL IFORMGETINTEGER(2*3,ITSMO)
              CALL IFormGetDouble (2*4,CSMO)
              CALL IFormGetDouble (2*5,RFAC)
              CALL IFormGetDouble (2*6,BAAS2)
              CALL IFORMGETINTEGER(2*7,KEEPSTARTDIR)
              CALL IFormGetDouble (2*8,SPLFAC)
              CALL IFormGetDouble (2*9,SPLFAC2)
              CALL IFormGetDouble (2*10,FACMIR)
              CALL IFORMGETINTEGER(2*11,jsferic)
              CALL IFORMGETINTEGER(2*12,jsferTEK)
              CALL IFormGetDouble (2*13,pil_rad)
              CALL IFormGetDouble (2*14,pil_x)
              CALL IFormGetDouble (2*15,pil_y)

              KEEPSTARTDIR = MAX(0,KEEPSTARTDIR)
              KEEPSTARTDIR = MIN(1,KEEPSTARTDIR)
              !MFAC = MAX(1,MFAC)
              !NFAC = MAX(1,NFAC)
              CSMO = MAX(0d0,CSMO)
              RFAC = MAX(0d0,RFAC)
              BAAS2 = MAX(0d0, MIN(BAAS2,1d0) )
              SPLFAC= MAX(0d0, MIN(SPLFAC,1d0) )
              SPLFAC2=MAX(0d0, MIN(SPLFAC2,1d0) )
              FACMIR=MAX(0.1d0, MIN(FACMIR,10d0) )
              jsferic = max(0,min(jsferic,1)) 

              if ( pil_rad < 0d0 ) then ! cre
                 if (maxpol < mfac+1) then  
                     call increasepol(mfac+1, 0)     
                 endif
                 pil_rad = abs(pil_rad)
                 do k = 1,mfac+1
                    phi = twopi* ( dble(k-1) / dble(mfac) )
                    xpl(k) = pil_x + pil_rad*cos(phi)
                    ypl(k) = pil_y + pil_rad*sin(phi)
                 enddo
                 npl = mfac+1 
             endif  
          ENDIF
          CALL IWinClose(1)
          CALL IWinClose(1)
          CALL IWinClose(1)
          CALL RESTOREKEYS()
          RETURN
      ELSE IF (KEY .EQ. 21) THEN
         IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
             WRDKEY = HELPM(IFEXIT)
             CALL HELP(WRDKEY,NLEVEL)
         ENDIF
      ENDIF
      GOTO 30

      END SUBROUTINE CHANGEGRIDPARAMETERS

   SUBROUTINE CHANGEINTERPOLATIONPARAMETERS()
   use m_ec_interpolationsettings
   use M_SAMPLES, only : mxsam
   use m_arcinfo, only : mca
   use unstruc_display
   use unstruc_version_module, only : unstruc_company, unstruc_program

   implicit none
   integer :: numpar, numfld, numparactual, numfldactual
   PARAMETER  (NUMPAR = 8, NUMFLD = 2*NUMPAR)
   INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
   CHARACTER WRDKEY*40, OPTION(NUMPAR)*45, HELPM(NUMPAR)*60
   integer :: nlevel
   COMMON /HELPNOW/ WRDKEY,NLEVEL
   integer, external :: infoinput
   external :: highlight_form_line
!
   integer :: ir, il, iw, ixp, iyp, ih, i, ifexit, ifinit, key
   integer :: nbut, imp, inp

   NLEVEL     = 4
   OPTION( 1) = 'INTERPOLATIONTYPE (1=TRI,2=AVE,3=CURV. TRI)' ; it(2* 1) = 2
   OPTION( 2) = 'JTEKINTERPOLATIONPROCESS (0/1)             ' ; it(2* 2) = 2
   OPTION( 3) = 'IAV, AVERAGINGTYPE                         ' ; it(2* 3) = 2
   OPTION( 4) = 'NUMMIN, MINUMUM NR OF POINTS IN AV         ' ; it(2* 4) = 2
   OPTION( 5) = 'RCEL, RELATIVE SEARCH CELL SIZE            ' ; it(2* 5) = 6
   OPTION( 6) = 'Interpolate_to , 1=bathy, 2=ZK, 3=S1, 4=ZC ' ; it(2* 6) = 2
   OPTION( 7) = 'Percentileminmax, average min or max Perc %' ; it(2* 7) = 6
   OPTION( 8) = 'Mxsam                                      ' ; it(2* 8) = 2

   
!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

   HELPM ( 1) = '1 = TRIANGULATION 2= CELL AVERAGING, 3=USE CURV.GRID FOR TRI'
   HELPM ( 2) = 'SHOW INTERPOLATION PROCESS 0-No, 1=Yes                      '
   HELPM ( 3) = '1=AVER., 2=CLOSEST POINT, 3=MAX, 4=MIN, 5=INV. DIST. WEIGHT '
   HELPM ( 4) = 'MINIMUM NR OF POINTS NEEDED INSIDE CELL TO HANDLE CELL      '
   HELPM ( 5) = 'DEFAULT 1.0 = ACTUAL CELL SIZE, 2.0 = TWICE AS LARGE        '
   HELPM ( 6) = '1=? , 2=network, 3=waterlevels, 4=curvigrid                 '
   HELPM ( 7) = 'if Perc>0, for iav == 3 or iav==4, av of hih or low percenti'
   HELPM ( 8) = 'if >0, use bilin instead of triinterp                      '

   
   CALL SAVEKEYS()

   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2*NUMPARACTUAL

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      IX(IL) = 13
      IX(IR) = 95
      IY(IL) = 2*I
      IY(IR) = 2*I
      IS(IL) = 82
      IS(IR) = 10
      IT(IL) = 1001
   ENDDO

!  Initialise
   CALL IWinWordWrap('OFF')
   CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
   CALL INHIGHLIGHT('WHITE','RED')
   IW     = NPOS(3)
   IXP    = NPOS(1) + (IWS-IW)/2
   IYP    = NPOS(2)
   IH     = IHS - 9

!  Header of filewindow
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP,IW,1)
   CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
   CALL IWinOutCentre(1,trim(unstruc_company)//'-'//trim(unstruc_program)// ' PARAMETER FORM')
   CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!  Explain keyfunctions in bottom window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IHS-1,IW,2)
   CALL IWinOutStringXY(1,1,'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
   CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!  Filewindow is middelste window
   CALL IWinAction('FPC')
   CALL IWinOpen(IXP,IYP+3,IW,IH)

   CALL InControlKey(29,129)
   CALL InControlKey(30,128)

!  NUMWNH = InfoWindow(1)
!  CALL IWinSelect(NUMWNH)

!  Define a new form by supplying arrays containing
!  field positions, sizes and types
   CALL IFORMDEFINE('W',NUMFLDACTUAL,IX,IY,IS,IT)

!  Define a help field and define help strings
!  for 2 of the 4 input fields
   CALL IFORMHELP(13,IH,60)

   IR = 0
   DO I = 1,NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      CALL IFORMPUTSTRING (IL,OPTION(I))
      CALL IFORMPUTHELP   (IR,HELPM(I))
      CALL IFORMATTRIBUTEN(IR,0,0,7)
   ENDDO

   CALL IFormPutINTEGER (2* 1 , INTERPOLATIONTYPE        )
   CALL IFORMPUTinteger (2* 2 , JTEKINTERPOLATIONPROCESS )
   CALL IFormPutINTEGER (2* 3 , IAV                      )
   CALL IFormPutINTEGER (2* 4 , NUMMIN                   )
   CALL IFormPutDouble  (2* 5 , RCEL            ,'(F8.3)')
   CALL IFormPutINTEGER (2* 6 , Interpolate_to           )
   CALL IFormPutDouble  (2* 7 , percentileminmax,'(F8.3)')
   CALL IFormPutINTEGER (2* 8 , Mxsam                    )

   !  Display the form with numeric fields left justified
   !  and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

30 CONTINUE
   IFINIT = IFEXIT
   CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!  check for Help, Confirm, Quit
   KEY = INFOINPUT(55)
   IF (KEY .EQ. -2) THEN
       NBUT = INFOINPUT(61)
       IF (NBUT .GE. 1) THEN
          IMP = INFOINPUT(62) + 1
          INP = INFOINPUT(63) + 1
          IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.  &
              INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
             IF (NBUT .EQ. 1) THEN
                KEY = 21
             ELSE
                KEY = 22
             ENDIF
          ELSE
             KEY = 23
          ENDIF
       ENDIF
   ELSE IF (KEY .EQ. -1) THEN
      KEY = INFOINPUT(57)
   ENDIF
   IF (KEY .EQ. 26) THEN
       WRDKEY = OPTION(IFEXIT/2)
       CALL HELP(WRDKEY,NLEVEL)
   ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
       IF (KEY .EQ. 22) THEN
          CALL IFormGEtINTEGER (2* 1 , INTERPOLATIONTYPE        )
          CALL IFORMGETinteger (2* 2 , JTEKINTERPOLATIONPROCESS )
          CALL IFormGEtINTEGER (2* 3 , IAV                      )
          CALL IFormGEtINTEGER (2* 4 , NUMMIN                   )
          CALL IFormGEtDouble  (2* 5 , RCEL                     )
          CALL IFormGEtINTEGER (2* 6 , Interpolate_to           )
          CALL IFormGEtDouble  (2* 7 , Percentileminmax         )
          CALL IFormGEtinteger (2* 8 , Mxsam                    )
          if (mxsam == 0) mca = 0

       ENDIF
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL IWinClose(1)
       CALL RESTOREKEYS()
       RETURN
   ELSE IF (KEY .EQ. 21) THEN
      IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
          WRDKEY = HELPM(IFEXIT)
          CALL HELP(WRDKEY,NLEVEL)
      ENDIF
   ENDIF
   GOTO 30

   END SUBROUTINE CHANGEINTERPOLATIONPARAMETERS


      SUBROUTINE TEKVEC(NSC,X,Y,U,V,X1,X2,Y1,Y2,NCOL,TITLE)
      implicit none
      double precision :: dx
      double precision :: dxh
      double precision :: dy
      double precision :: dyh
      integer, save :: ini = 0
      integer :: ncol
      integer :: nsc
      integer :: numsc
      double precision :: psi0
      double precision :: u
      double precision :: v
      double precision :: vfac
      double precision :: x
      double precision :: x1
      double precision :: x1sc
      double precision :: x2
      double precision :: x2sc
      double precision :: y
      double precision :: y1
      double precision :: y1sc
      double precision :: y2
      double precision :: y2sc

      CHARACTER TITLE*(*), TEX*8
      COMMON /GSCREENS/ X1SC(100),Y1SC(100),X2SC(100),Y2SC(100),NUMSC

      INI = INI + 1

      call viewport(real(X1SC(NSC)),real(Y1SC(NSC)),real(X2SC(NSC)),real(Y2SC(NSC) ))
      DX  = (X2-X1)*0.1d0
      DY  = (Y2-Y1)*0.1d0
      DXH = DX/2d0
      DYH = DY/2d0
 !     CALL IGRUNITS( real(X1-DX),real(Y1-DY),real(X2+DX),real(Y2+DY) )
      CALL setwor( X1-DX, Y1-DY, X2+DX, Y2+DY )

      VFAC = 10
      PSI0 = 0
      CALL SETCOL(NCOL)
      CALL ARROWS(X,Y,U,V,PSI0,VFAC)

      RETURN
      END


      SUBROUTINE TEKFN(NSC,NF,JW,X,Y,N,X1,X2,Y1,Y2,NCOL,TITLE,JAUTO,JP,DAG,kp1)
      use m_flow, only : kplotfrombedorsurface
      implicit none
      double precision :: dag
      double precision :: dv
      double precision :: dxh
      double precision :: dyh
      double precision :: f1
      double precision :: f2
      double precision :: fmx
      integer :: i, kp
      integer, save :: ini = 0
      integer :: j
      integer :: jaauto
      integer :: jauto
      integer :: jp
      integer :: jw
      integer :: mx
      integer :: n
      integer :: ncol
      integer :: ncols
      integer :: nf
      integer :: nie
      integer :: nis
      integer :: nsc
      integer :: nv
      integer :: nx, kp1
      double precision :: val, fx1, fx2, fy1, fy2
      double precision :: vmax
      double precision :: vmin
      double precision :: x1
      double precision :: x2
      double precision :: xo
      double precision :: xtx
      double precision :: y1
      double precision :: y2
      double precision :: yo
      double precision :: ytx, rcx, rcy
      PARAMETER (MX= 366, NX=20)
      CHARACTER TITLE*(*), TEX*16
      double precision :: X(N), Y(N), XX(4), YY(4), ZZ(4)
      COMMON /ORGARR/ XO(MX,NX), YO(MX,NX), FMX(NX)
      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
      ! NSC schermnr
      ! NF  functienr
      ! JW  update assen 1 = ja, niet 1 = nee
      ! JP  teken profielen 1 = ja, 2=circ, 3 = teken isolijnen
      ! in dat geval DAG (nr van de dag) toevoegen

      kp = kp1
      
      IF (INI .EQ. 0) THEN
         DO J = 1,NX
            FMX(J) = 0
            DO I = 1,MX
               XO(I,J) = 0
               YO(I,J) = 0
            ENDDO
         ENDDO
         INI = INI + 1
      ENDIF

      IF (N < 2) RETURN

      Fx1 =  1.0D20
      Fx2 = -1.0D20
      DO I = 1,N
         Fx1 = MIN(X(I),Fx1)
         Fx2 = MAX(X(I),Fx2)
      ENDDO

      Fy1 =  1.0D20
      Fy2 = -1.0D20
      DO I = 1,N
         Fy1 = MIN(y(I),Fy1)
         Fy2 = MAX(y(I),Fy2)
      ENDDO

      IF (JAUTO .EQ. 1) THEN
         X1 = Fx1
         X2 = max(Fx2, Fx1 + 1d-4)

         if (fy1 < 2d0*y1-y2) return
         if (fy2 > 2d0*y2-y1) return
         if (fx1 < 2d0*x1-x2) return
         if (fx2 > 2d0*x2-x1) return
      ENDIF
      
      IF (Fx1 .LT. -1.0D6 .OR. Fx2 .GT. 1.0D6) THEN
         CALL KTEXT(TITLE,2,2,60)
         CALL KTEXT('TOO LARGE FOR PLOTTING',3,3,60)
         RETURN
      ENDIF
      IF (Fy1 .LT. -1.0D6 .OR. Fy2 .GT. 1.0D6) THEN
         CALL KTEXT(TITLE,2,2,60)
         CALL KTEXT('TOO LARGE FOR PLOTTING',3,3,60)
         RETURN
      ENDIF

      CALL SETWINDOW(NSC,X1,Y1,X2,Y2,DXH,DYH) ! TEKEN IN WINDOW NR ZOVEEL

      CALL SETCOL(NCOL)

      IF (JW .EQ. 1) THEN
         CALL BOX(X1,Y1,X2,Y2)

         CALL IGRCHARSIZE(2.0,1.0)
         tex = ' '
         write(tex(2:10), '(F8.1)')  fy2 - fy1
         CALL DRAWTEXT(real(X1),real(Y2+DYH),TITLE//tex)

         if (abs(x1) < 1d3) then 
            WRITE (TEX,'(F8.3)') X1
         else
            WRITE (TEX,'(e8.3)') X1
         endif 
         CALL DRAWTEXT(real(X1),real(Y1-DYH),TEX)

         if (abs(x2) < 1d3) then
            WRITE (TEX,'(F8.3)') X2
            CALL DRAWTEXT(real(X2-6*DXH),real(Y1-DYH),TEX)
         endif    
         

         CALL MOVABS((X1+X2)/2, Y1)
         CALL  LNABS((X1+X2)/2, Y1+DYH/2)

      ENDIF

      IF (JP == 1 .OR. JP == 2 ) THEN              ! JA PROFIELEN
         if (JP == 1) then
            CALL DISPF2(X,Y,N,N,NCOL)              ! HUIDIGE PROFIEL TEKENEN
         else
            rcx = 5d-3*(x2-x1)
            rcy = 5d-3*(y2-y1)
            call DISPF2cir(X,Y,N,RCx,rcy,NCOL)
            if (kp > 0 .and. kp <=n) then          ! print layer value
               if (kplotfrombedorsurface .ne. 1) then 
                  kp = n - kp + 1 
               endif   
               call movabs( x(kp), y(kp) )
               call setcol(31)
               call fbox(x(kp)-rcx,y(kp)-rcy,x(kp)+rcx,y(kp)+rcy) 

               WRITE (TEX,'(E13.5)') X(kp)
               xtx = x(kp)
               if ( xtx > (x1+x2)/2 ) then
                  xtx = x2 - (xtx - x1)
               endif
               ytx = Y(kp)
               CALL GTEXT(TEX, xtx, ytx, NCOL)

               WRITE (TEX,'(F13.5)') y(kp) - y1
               ytx = ytx - 0.05d0*(y2-y1)
               CALL GTEXT(TEX, xtx, ytx, NCOL)

            endif
         endif

         IF (JW .EQ. 1) THEN ! ALLEEN BIJ PROFIELEN EN ALS WINDOW GETEKEND WORDT
           ! WRITE (TEX,'(E16.5)') FMX(NF)   ! max profile value
            xtx = X2-10d0*DXH
            ytx = Y2-DYH
           ! CALL GTEXT(TEX, xtx, ytx, 0)
            WRITE (TEX,'(E16.5)') Fx2
            CALL GTEXT(TEX, xtx, ytx, NCOL)
            FMX(NF) = Fx2

            WRITE (TEX,'(E16.5)') Fx1  !  sum(x)/dble(n)    ! ave profile value
            xtx = X1-DXH
            ytx = Y2-DYH
            CALL GTEXT(TEX, xtx, ytx, Ncol)

         ENDIF
      ELSE if (jp > 0) then      ! ISOLIJNEN
         VMAX = 22
         VMIN = 2
         NV   = 10
         DV   = VMAX - VMIN
         DO 40 I = 1,NV
            VAL(I) = VMIN + (I-1)*DV/(NV-1)
   40    CONTINUE
!C        CALL ISOSCALE()
         DO I = 2,N
            XX(1) = DAG-1
            XX(2) = DAG-1
            XX(3) = DAG
            XX(4) = DAG
            YY(1) = YO(I,NF)
            YY(2) = YO(I-1,NF)
            YY(3) = Y (I-1)
            YY(4) = Y (I  )
            ZZ(1) = XO(I,NF)
            ZZ(2) = XO(I-1,NF)
            ZZ(3) = X (I-1)
            ZZ(4) = X (I  )
            CALL ISOFIL(XX,YY,ZZ,4,0)
         ENDDO
      ENDIF
 
      RETURN
      END



      SUBROUTINE SETWINDOW(NSC,X1,Y1,X2,Y2,DXH,DYH)
      implicit none
      double precision :: dx
      double precision :: dxh
      double precision :: dy
      double precision :: dyh
      integer :: nsc
      integer :: numsc
      double precision :: x1
      double precision :: x1sc
      double precision :: x2
      double precision :: x2sc
      double precision :: y1
      double precision :: y1sc
      double precision :: y2
      double precision :: y2sc
      COMMON /GSCREENS/ X1SC(100),Y1SC(100),X2SC(100),Y2SC(100),NUMSC

      CALL viewport ( real(X1SC(NSC)),real(Y1SC(NSC)),real(X2SC(NSC)),real(Y2SC(NSC)) )
      DX  = (X2-X1)*0.1d0
      DY  = (Y2-Y1)*0.1d0
      DXH = DX/2d0
      DYH = DY/2d0
  !    CALL IGRUNITS( real(X1-DX),real(Y1-DY),real(X2+DX),real(Y2+DY) )
      CALL setwor( X1-DX,Y1-DY,X2+DX,Y2+DY )

      RETURN
      END


!----------------------------------------------------------------------
! subroutines from rgfstuff.f90
!----------------------------------------------------------------------
      SUBROUTINE CONVERPARAMETERS(JA)
      USE M_MAPPROPARAMETERS
      use unstruc_display
      use m_sferic
      implicit none
      integer :: i
      integer :: ifexit
      integer :: ifinit
      integer :: ih
      integer :: il
      integer :: imp
      integer :: inp
      integer :: ir
      integer :: iw
      integer :: ixp
      integer :: iyp
      integer :: ja
      integer :: key
      integer :: l
      integer :: nbut
      integer :: nlevel
      integer :: numfld
      integer :: numpar
      PARAMETER  (NUMPAR = 10, NUMFLD = 2*NUMPAR)
      INTEGER  IX(NUMFLD),IY(NUMFLD),IS(NUMFLD),IT(NUMFLD)
      CHARACTER WRDKEY*40, OPTION(NUMPAR)*40, HELPM(NUMPAR)*60, TEX*132
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      integer, external :: infoinput
      external :: highlight_form_line

!
      JA        = 0
      NLEVEL    = 3
      OPTION(1) = 'Type of Map Projection (0,1,2,3,4,-1)   '
      OPTION(2) = 'UTM Zone Nr (1-60)                      '
      OPTION(3) = 'Northern (1) or southern (0) hemisphere '
      OPTION(4) = 'Offset X-Direction                      '
      OPTION(5) = 'Offset Y-Direction                      '
      OPTION(6) = 'Rotation Left (deg)                     '
      OPTION(7) = 'X Scalefactor                           '
      OPTION(8) = 'Y Scalefactor                           '
      OPTION(9) = 'X centrepoint (deg) for stereographic   '
      OPTION(10)= 'Y centrepoint (deg) for stereographic   '


!      123456789012345678901234567890123456789012345678901234567890
!               1         2         3         4         5         6
      HELPM (1) = '0=Trans/Rot,1=UTM,2=Amer,3=RD(Parijs),4=MERC,-1=AFFINE.XYX  '
      HELPM (2) = 'Usually 0, Except When Type = 1 (UTM) and Cartesian         '
      HELPM (3) = 'Only used for UTM->latlon conversion                        '
      HELPM (4) = 'X = X + Offset X-Direction, Real Value (m) (Only for Type=0)'
      HELPM (5) = 'Y = Y + Offset Y-Direction, Real Value (m) (Only for Type=0)'
      HELPM (6) = 'Rotationcenter = Center of Grid            (Only for Type=0)'
      HELPM (7) = 'Dimensionsless ()                          (Only for Type=0)'
      HELPM (8) = 'Dimensionsless ()                          (Only for Type=0)'
      HELPM (9) = 'Degrees                                    (Only for Type=5)'
      HELPM (10)= 'Degrees                                    (Only for Type=5)'


      CALL SAVEKEYS()

      IR = 0
      DO 10 I = 1,NUMPAR
         IL = IR + 1
         IR = IL + 1
         IX(IL) = 13
!         IX(IR) = 53
         IX(IR) = 95
         IY(IL) = 2*I
         IY(IR) = 2*I
         IS(IL) = 82
         IS(IR) = 10
         IS(IR) = 10
         IT(IL) = 1001
         IF (I .LE. 3) THEN
            IT(IR) = 2
         ELSE
            IT(IR) = 6
         ENDIF
   10 CONTINUE

!     Initialise
      CALL IWinWordWrap('OFF')
      CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
      CALL INHIGHLIGHT('WHITE','RED')
      IW     = NPOS(3)
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2)
      IH     = IHS - 9

!     Header of filewindow
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP,IW,1)
      CALL ITEXTCOLOURN(LBLFOR,LBLBCK)

      IF (JSFERIC .EQ. 1) THEN
         TEX = 'Conversion from Spherical (deg) to Cartesian (m) Coordinates'
      ELSE
         TEX = 'Conversion from Cartesian (m) to Spherical (deg) Coordinates'
      ENDIF
      L = len_trim(TEX)
      CALL IWinOutCentre(1,TEX(1:L))

      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!     Explain keyfunctions in bottom window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IHS-1,IW,2)
      CALL IWinOutStringXY(1,1,'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
      CALL IWinOutStringXY(1,2,'right mouse = Enter, click outside window = Esc')

!     Filewindow is middelste window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP+3,IW,IH)

      CALL InControlKey(29,129)
      CALL InControlKey(30,128)

!     NUMWNH = InfoWindow(1)
!     CALL IWinSelect(NUMWNH)

!     Define a new form by supplying arrays containing
!     field positions, sizes and types
      CALL IFORMDEFINE('W',NUMFLD,IX,IY,IS,IT)

!     Define a help field and define help strings
!     for 2 of the 4 input fields
      CALL IFORMHELP(13,IH,60)

      IR = 0
      DO 20 I = 1,NUMPAR
         IL = IR + 1
         IR = IL + 1
         CALL IFORMPUTSTRING (IL,OPTION(I))
         CALL IFORMPUTHELP   (IR,HELPM(I))
         CALL IFORMATTRIBUTEN(IR,0,0,7)
   20 CONTINUE

      CALL IFORMPUTINTEGER( 1*2 ,ITYPE)
      CALL IFORMPUTINTEGER( 2*2 ,IZONE)
      CALL IFORMPUTINTEGER( 3*2 ,IHEM)
      CALL IFormPutDouble ( 4*2,DELTX,'(F10.3)')
      CALL IFormPutDouble ( 5*2,DELTY,'(F10.3)')
      CALL IFormPutDouble ( 6*2,FI,'(F10.3)')
      CALL IFormPutDouble ( 7*2,XF,'(F10.3)')
      CALL IFormPutDouble ( 8*2,YF,'(F10.3)')
      CALL IFormPutDouble ( 9*2,xcstereo,'(F10.3)')
      CALL IFormPutDouble (10*2,ycstereo,'(F10.3)')


!  Display the form with numeric fields left justified
!  and set the initial field to number 2
   CALL IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT-1, 'BU', ' ', ' ')
   CALL IFORMSHOW()

   30 CONTINUE
      IFINIT = IFEXIT
      CALL IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!     check for Help, Confirm, Quit
      KEY = INFOINPUT(55)
      IF (KEY .EQ. -2) THEN
          NBUT = INFOINPUT(61)
          IF (NBUT .GE. 1) THEN
             IMP = INFOINPUT(62) + 1
             INP = INFOINPUT(63) + 1
             IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.    &
                 INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
                IF (NBUT .EQ. 1) THEN
                   KEY = 21
                ELSE
                   KEY = 22
                ENDIF
             ELSE
                KEY = 23
             ENDIF
          ENDIF
      ELSE IF (KEY .EQ. -1) THEN
         KEY = INFOINPUT(57)
      ENDIF
      IF (KEY .EQ. 26) THEN
          WRDKEY = OPTION(IFEXIT/2)
          CALL HELP(WRDKEY,NLEVEL)
      ELSE IF (KEY .EQ. 22 .OR. KEY .EQ. 23) THEN
          IF (KEY .EQ. 22) THEN
              CALL IFORMGETINTEGER( 1*2,ITYPE)
              CALL IFORMGETINTEGER( 2*2,IZONE)
              CALL IFORMGETINTEGER( 3*2,IHEM)
              CALL IFormGetDouble ( 4*2,DELTX)
              CALL IFormGetDouble ( 5*2,DELTY)
              CALL IFormGetDouble ( 6*2,FI)
              CALL IFormGetDouble ( 7*2,XF)
              CALL IFormGetDouble ( 8*2,YF)
              CALL IFormGetDouble ( 9*2,Xcstereo)
              CALL IFormGetDouble (10*2,ycstereo)
              CSE = COS(DG2RD*FI)
              SNE = SIN(DG2RD*FI)
              JA  = 1
          ENDIF
          CALL IWinClose(1)
          CALL IWinClose(1)
          CALL IWinClose(1)
          CALL RESTOREKEYS()
          RETURN
      ELSE IF (KEY .EQ. 21) THEN
         IF (IFEXIT .EQ. 1 .OR. IFEXIT .EQ. 3) THEN
             WRDKEY = HELPM(IFEXIT)
             CALL HELP(WRDKEY,NLEVEL)
         ENDIF
      ENDIF
      GOTO 30

      END SUBROUTINE CONVERPARAMETERS



      subroutine TEKgrid(key)
      use m_grid
      use unstruc_colors
      implicit none
      integer :: key
      integer :: ndraw
      COMMON /DRAWTHIS/ ndraw(50)
      call tekgrd(XC,YC,MMAX,NMAX,1,1,mc,nc,NCOLDG,ndraw(38),key,MC)

      end subroutine TEKgrid
      
      subroutine cleargrid()
      use m_grid
      use unstruc_colors
      implicit none
      integer :: key
      integer :: ndraw
      COMMON /DRAWTHIS/ ndraw(50)
      call tekgrd(XC,YC,MMAX,NMAX,1,1,mc,nc,0,ndraw(38),key,MC)
      end subroutine cleargrid

      SUBROUTINE TEKgrd(XC, YC, MMAX, NMAX, m1,n1,m2,n2,NCOL,MET,key,MC)
      implicit none
      integer :: mmax, nmax, m1, n1, m2, n2, ncol, met, key, mc
      DOUBLE PRECISION :: XC(MMAX,NMAX), YC(MMAX,NMAX), xlist(nmax), ylist(nmax)

      integer :: i, j, kmax, ja


      IF (MET .EQ. 0 .OR. MC == 0) RETURN
      JA = 0

      CALL SETCOL(NCOL)
      IF (MET .EQ. 2 .OR. MET .EQ. 4) CALL IGRLINETYPE(1)

      KMAX = 8
      DO J = N1,N2
        IF (MOD (J,10) .EQ. 0) CALL HALT2(JA)
        IF (JA .EQ. 1) THEN
           IF (MET .EQ. 2 .OR. MET .EQ. 4) CALL IGRLINETYPE(0)
           RETURN
        ENDIF

        CALL JGRLINE8(Xc(M1,J),Yc(M1,J),M2-M1+1)
      ENDDO

      DO I = M1,M2
        IF (MOD (I,10) .EQ. 0) CALL HALT2(JA)
        IF (JA .EQ. 1) THEN
           IF (MET .EQ. 2 .OR. MET .EQ. 4) CALL IGRLINETYPE(0)
           RETURN
        ENDIF

        xlist(1:N2-N1+1) = xc(i,N1:N2)
        ylist(1:N2-N1+1) = yc(i,N1:N2)
        CALL JGRLINE8(xlist,ylist,N2-N1+1)
      ENDDO

      IF (MET .EQ. 2 .OR. MET .EQ. 4) CALL IGRLINETYPE(0)
      IF (MET .EQ. 5) THEN
         CALL TEKnumnetcells(0)
      ENDIF

      END subroutine tekgrd

      subroutine TEKnumnetcells(jatel)
      use m_grid
      use m_netw
      use m_polygon
      use m_missing
      use unstruc_display
      use geometry_module, only: dbpinpol
      use gridoperations

      implicit none
      integer          :: i,j,n,ncol,jatel,in,k,im,jm,mxnum
      double precision :: xx(4),yy(4),x,y,z

      double precision :: vmax, vmin, dv, val
      integer          :: ncols, nv, nis, nie, jaauto
      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO


      call savepol()

      im = 0
      jm = 0
      mxnum = 0
      if (jatel ==1) then
          if (nump == 0) call findcells(0)
          ijyes = 0
      else
          vmax = -9d9
          vmin = -vmax
          do j = 1,nc-1
             do i = 1,mc-1
                if (ijyes(i,j) > 0) then
                   x  = 0.25d0* (xc(i,j) + xc(i+1,j) + xc(i+1,j+1) + xc(i,j+1) )
                   y  = 0.25d0* (yc(i,j) + yc(i+1,j) + yc(i+1,j+1) + yc(i,j+1) )
                   if (inview(x,y) ) then
                      if (ijyes(i,j) > mxnum) then
                         im = i
                         jm = j
                         mxnum = ijyes(i,j)
                      endif
                      z = ijyes(i,j)
                      vmax = max(z,vmax)
                      vmin = min(z,vmin)
                   endif
                endif
             enddo
          enddo
          DV   = VMAX - VMIN
          DO I = 1,NV
             VAL(I) = VMIN + (I-1)*DV/(NV-1)
          ENDDO
      endif


      do j = 1,nc-1
         do i = 1,mc-1
            n = 0
            if (xc(i,j)   .ne. dmiss .and. xc(i+1,j)   .ne. dmiss .and. &
                xc(i,j+1) .ne. dmiss .and. xc(i+1,j+1) .ne. dmiss ) then
                n = n + 1
                xpl(n) = xc(i,j  )
                ypl(n) = yc(i,j)
                n = n + 1
                xpl(n) = xc(i+1,j)
                ypl(n) = yc(i+1,j)
                n = n + 1
                xpl(n) = xc(i+1,j+1)
                ypl(n) = yc(i+1,j+1)
                n = n + 1
                xpl(n) = xc(i,j+1)
                ypl(n) = yc(i,j+1)
                npl = 4
                if (jatel == 1) then
                   in = -1
                   do k = 1,nump
                      call dbpinpol( xzw(k), yzw(k), in,  dmiss, JINS, NPL, xpl, ypl, zpl)
                      ijyes(i,j) = ijyes(i,j) + in
                   enddo
                else
                   z = ijyes(i,j)
                   x = (xpl(1)+xpl(2)+xpl(3)+xpl(4))/4
                   y = (ypl(1)+ypl(2)+ypl(3)+ypl(4))/4
                   call kcir(x,y,z)
                endif
            endif
         enddo
      enddo

      call restorepol()

      if (im > 0 ) then
         i  = im
         j = jm
         z  = ijyes(i,j)
         x  = 0.25d0* (xc(i,j) + xc(i+1,j) + xc(i+1,j+1) + xc(i,j+1) )
         y  = 0.25d0* (yc(i,j) + yc(i+1,j) + yc(i+1,j+1) + yc(i,j+1) )
         CALL SETTEXTSIZEfac(2d0)
         call htext(z, x, y)
         CALL SETTEXTSIZE()
      endif

      end subroutine TEKnumnetcells

      SUBROUTINE TEKGRPT(      X,      Y,     mmax, nmax, MC,     NC,  MP,     NP,   NCOL        )
!     TEKEN GRIDLIJNEN UITKOMEND OP DIT PUNT
      use m_missing
      implicit none
      integer :: mmax, nmax, mc, nc, mp, np, ncol
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)

      double precision :: xp, yp
      integer :: mpu, mpd, npu, npd

      CALL SETCOL(NCOL)
      IF (MP .EQ. 0) RETURN
      XP = X(MP,NP)
      YP = Y(MP,NP)
      IF (XP .EQ. 0) RETURN
      MPU = MP + 1
      MPD = MP - 1
      NPU = NP + 1
      NPD = NP - 1
      IF (MPU .LE. MC) THEN
         IF (X(MPU,NP) .NE. XYMIS) THEN
            CALL MOVABS(X(MPU,NP),Y(MPU,NP))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      IF (MPD .GE. 1) THEN
         IF (X(MPD,NP) .NE. XYMIS) THEN
            CALL MOVABS(X(MPD,NP),Y(MPD,NP))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      IF (NPU .LE. NC) THEN
         IF (X(MP,NPU) .NE. XYMIS) THEN
            CALL MOVABS(X(MP,NPU),Y(MP,NPU))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      IF (NPD .GE. 1) THEN
         IF (X(MP,NPD) .NE. XYMIS) THEN
            CALL MOVABS(X(MP,NPD),Y(MP,NPD))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      RETURN
      END


          ! NOTE: japes is disabled [AvD]
      SUBROUTINE SHWXYZ(X,Y,RD1,mmax, nmax, MC,NC,JAPERS,KEY,M,N)
      use m_missing
      use unstruc_colors
      implicit none

      integer :: mmax, nmax, mc, nc, japers, key, m, n
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX), RD1(MMAX,NMAX)
      CHARACTER  WRDKEY*40, OLDKEY*40
      double precision :: XLC, YLC, XA, YA
      integer :: JMOUSE,JASHOW
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW
      integer :: nlevel
      COMMON /HELPNOW/ WRDKEY,NLEVEL

      integer :: jadraw, jonce, jplus, nlevo
      double precision :: XL, YL, RDOL, FAC

      IF (MC .EQ. 0) RETURN
      OLDKEY = WRDKEY
      NLEVO  = NLEVEL
      WRDKEY = 'TAB = DCURSOR;'
      CALL IMOUSECURSORHIDE()
      CALL SETXOR(1)
      JADRAW = 1
      JONCE  = 0
      JPLUS  = 0

      IF (JAPERS .EQ. 1) THEN
         XL = (X1+X2)/2
         YL = (Y1+Y2)/2
      ELSE
         XL = XLC
         YL = YLC
      ENDIF

      CALL CLOSPT(    X,      Y,     mmax, nmax, MC,     NC, &
                     XL,     YL,      M,      N)
      RDOL = RD1(M,N)

   20 CONTINUE

      CALL DISPOS2(X(M,N),Y(M,N))
      CALL DISDEP(M,N,RD1(M,N))
      IF (JADRAW .EQ. 1) THEN
         CALL TEKGPT(      X,      Y,   mmax, nmax, MMAX,   NMAX, &
                           M,      N, NCOLTX,   RD1)
         JADRAW = 0
      ENDIF
      CALL INKEYEVENT(KEY)
      IF (KEY .NE. 27) JONCE = 0
      IF (KEY .NE. 45 .AND. KEY .NE. 160 .AND. &
          KEY .NE. 43 .AND. KEY .NE. 162) JPLUS = 0

      CALL DISPOS2(X(M,N),Y(M,N))
      CALL DISDEP(M,N,RD1(M,N))
      CALL TEKGPT(      X,      Y,   mmax, nmax, MMAX,   NMAX, &
                        M,      N, NCOLTX,   RD1)
      JADRAW = 1
      IF (KEY .EQ. 131) THEN
         M    = MAX(1,M - 1)
         RDOL = RD1(M,N)
      ELSE IF (KEY .EQ. 130) THEN
         M    = MIN(MC,M + 1)
         RDOL = RD1(M,N)
      ELSE IF (KEY .EQ. 128) THEN
         N    = MIN(NC,N + 1)
         RDOL = RD1(M,N)
      ELSE IF (KEY .EQ. 129) THEN
         N    = MAX(1,N - 1)
         RDOL = RD1(M,N)
      ELSE IF (KEY .EQ. 171) THEN
         CALL HELP(WRDKEY,3)
      ELSE IF (KEY .EQ. 45 .OR. KEY .EQ. 160) THEN
         IF (X(M,N) .NE. XYMIS) THEN
            IF (JPLUS .NE. -1) FAC = 1.0
            IF (RD1(M,N) .EQ. DMISS) RD1(M,N) = 6.9
            RD1(M,N) = RD1(M,N) - .01*FAC
            FAC      = FAC*1.05
            JPLUS    = -1
         ENDIF
      ELSE IF (KEY .EQ. 43 .OR. KEY .EQ. 162) THEN
         IF (X(M,N) .NE. XYMIS) THEN
            IF (JPLUS .NE. 1) FAC = 1.0
            IF (RD1(M,N) .EQ. DMISS) RD1(M,N) = 6.9
            RD1(M,N) = RD1(M,N) + .01*FAC
            FAC      = FAC*1.05
            JPLUS    = 1
         ENDIF
      ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32) THEN
         RD1(M,N) = DMISS
         CALL SETCOL(0)
         CALL MOVABS(X(M,N),Y(M,N))
         CALL CIR(RCIR)
         CALL DISDEP(M,N,RD1(M,N))
      ELSE IF (KEY .EQ. 27) THEN
         JONCE     = JONCE + 1
         IF (JONCE .GE. 2) THEN
            CALL ORGLOCATOR(X(M,N),Y(M,N))
            CALL IMOUSECURSORSHOW()
            CALL SETXOR(0)
            NLEVEL = NLEVO
            WRDKEY = OLDKEY
            RETURN
         ENDIF
         RD1(M,N) = RDOL
         CALL DISDEP(M,N,RD1(M,N))
      ELSE
         CALL ORGLOCATOR(X(M,N),Y(M,N))
         CALL IMOUSECURSORSHOW()
         CALL SETXOR(0)
         NLEVEL = NLEVO
         WRDKEY = OLDKEY
         RETURN
      ENDIF
      GOTO 20
      END subroutine shwxyz



      SUBROUTINE TEKGPT(      X,      Y,     mmax, nmax, MC,     NC, &
                            MP,     NP,   NCOL,   RD1)
!     TEKEN GRIDLIJNEN UITKOMEND OP DIT PUNT
      use m_missing
      use m_wearelt
      implicit none
      integer :: mmax, nmax, mc, nc, mp, np, ncol
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX), RD1(MMAX,NMAX)


      double precision :: XP, YP
      integer :: MPU, MPD, NPU, NPD, ncolcir
      XP = X(MP,NP)
      IF (XP .EQ. XYMIS) RETURN
      YP = Y(MP,NP)
      CALL MOVABS(XP,YP)
      CALL SETCOL(NCOL)
      MPU = MP + 1
      MPD = MP - 1
      NPU = NP + 1
      NPD = NP - 1
      IF (MPU .LE. MC) THEN
         IF (X(MPU,NP) .NE. XYMIS) THEN
            CALL MOVABS(X(MPU,NP),Y(MPU,NP))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      IF (MPD .GE. 1) THEN
         IF (X(MPD,NP) .NE. XYMIS) THEN
            CALL MOVABS(X(MPD,NP),Y(MPD,NP))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      IF (NPU .LE. NC) THEN
         IF (X(MP,NPU) .NE. XYMIS) THEN
            CALL MOVABS(X(MP,NPU),Y(MP,NPU))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      IF (NPD .GE. 1) THEN
         IF (X(MP,NPD) .NE. XYMIS) THEN
            CALL MOVABS(X(MP,NPD),Y(MP,NPD))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      CALL SETXOR(0)
      IF (RD1(MP,NP) .NE. DMISS) THEN
         CALL ISOCOL(RD1(MP,NP),NCOLCIR)
         CALL CIR(RCIR)
         CALL SETCOL(0)
         CALL PTABS(XP,YP)
      ENDIF
      CALL SETXOR(1)
      RETURN
      END subroutine tekgpt



!----------------------------------------------------------------------
! subroutines from unstruc.f90
!----------------------------------------------------------------------
 subroutine tekflowstuff(ja)
 use unstruc_display
 use m_netw
 use m_flowgeom
 use m_flow
 use m_wind
 use m_reduce
 use m_sferic, only: jsferic
 use geometry_module, only: getdx, getdy, getdxdy
 use gridoperations

 use m_observations

 implicit none

 integer :: ndraw
 double precision :: vfac, vfacforce
 integer :: nvec
 common /drawthis/ ndraw(50)
 COMMON /VFAC/ VFAC,VFACFORCE,NVEC

 !locals
 integer          :: k,kk,L,LL,k1,k2,ncol,nn, k3, k4
 integer          :: nodemode, linkmode              ! how  to show on flow nodes and links
 integer          :: nodewhat, linkwhat              ! what to show on flow nodes and links
 double precision :: znod, zlin, zcorn               ! what to show functions
 double precision :: xx1, yy1, Zz1                   ! help only
 double precision :: xx2, yy2, Zz2                   ! help only
 double precision :: x3, y3, x4, y4                  ! help only

 double precision :: xd, yd, zd, dxx, dy, rd, d      ! only
 double precision :: zn, x(4), y(4), z(4), zl
 integer          :: jview = 1                       ! for now fix regular
 integer          :: model24 = 0                     ! colourmodel 0/1
 double precision :: ux, uy                          ! x-y velocity components

 double precision :: rt, rr0, dddx, dddy, uux, uuy 
 integer          :: n, ja, ja2, nsiz

 ! ndraw(28)= show what on nodes   ndraw(19)=how to show on nodes , NDRAW(8) = SHOW WHAT ON NETNODES
 ! ndraw(29)= show what on links   ndraw(11)=how to show on links , NDRAW(7) = SHOW WHAT ON NETLINKS

 if ( ndx == 0 ) return

! nplot = min(ndxi, nplot)

 nplot = max(1,min(Ndx, nplot))

 nodemode = ndraw(19)
 linkmode = ndraw(11)
 nodewhat = ndraw(28)
 ja = 0

 call tekbathy(ja) 
 
 if (nodemode > 1 .AND. nodewhat > 1) then
    IF (NDRAW(8) == 1) call minmxnds()    ! ONLY ADAPT VERTICAL LIMITS FOR FLOW NODES IF NO NET NODES ASKED
 
    call tekflownodes(ja)
    if (ja > 0) then
       return
    end if

    if (nodemode .ge. 6) then

        do k = 1,ndxi
           if (mod(k,200) == 0) then
              call halt2(ja)
              if (ja == 1) return
           endif
           if (nodewhat .ge. 2) then
              ja2 = 1
              if (wetplot > 0d0) then
                 if (hs(k) < wetplot) then
                    ja2 = 0
                 endif
                 if (ja2 == 1) then  ! nodewhat==3: always show bottom
                    if (inview( xz(k), yz(k) ) ) then
                       zn = znod(k)
                       call dhtext( zn, xz(k), yz(k), bl(k) )
                    endif
                 endif
              endif
           end if
        enddo

    endif

    if (jaHighlight == 1) then
       if (ndmax .ne. 0) then
          call gtext( 'NDMAX', xz(ndmax), yz(ndmax), 31  )
       endif
       if (ndmin .ne. 0) then
          call gtext( 'ndmin', xz(ndmin), yz(ndmin), 221 )
       endif
    end if

    if (ndraw(37) >= 1) then 
        call tekprofpoint()
    endif
   if (ndraw(37) >= 2) then 
       do k = 1, nbndz                               ! boundary points tekflowstuff
          k1 = kbndz(1,k)
          k2 = kbndz(2,k)
          LL = kbndz(3,k)
          zn = znod(k1)
          call isocol(zn,ncol)
       
          ja2 = 1
          if (wetplot > 0d0 .and. hu(LL).gt.0d0 ) then
             if (hs(k1) < wetplot) then
                ja2 = 0
             endif
             if (ja2 == 1) then  ! nodewhat==3: always show bottom
                if (inview( xz(k1), yz(k1) ) ) then
                   call dhtext( zn, xz(k1), yz(k1), bl(k1) )
                endif
             endif
          endif
              
          call dmovabs( xz(k1), yz(k1), bl(k1) )
          call  dlnabs( xz(k2), yz(k2), bl(k2) )
       enddo
       
       do k = 1, nbndu                               ! boundary points tekflowstuff
          k1 = kbndu(1,k)
          k2 = kbndu(2,k)
          zn = znod(k1)
          call isocol(zn,ncol)
          call dmovabs( xz(k1), yz(k1), bl(k1) )
          call  dlnabs( xz(k2), yz(k2), bl(k2) )
          call dhtext( zn, xz(k1), yz(k1), bl(k1) )
       enddo
    endif

    if (ndraw(28) == 28) then   ! checkerboard gauss elimin / conj grad
       ncol = 31
       do n=nogauss0+1,nogauss0+nocg0
          k  = noel0(n)
          nn = size( nd(k)%x )
          call PFILLER(nd(k)%x, nd(k)%y, nn,NCOL,NCol)
       enddo

       ncol = 221
       do n=1,nogauss0
          k  = noel0(n)
          nn = size( nd(k)%x )
          call PFILLER(nd(k)%x, nd(k)%y, nn,NCOL,NCol)
       enddo

    endif

 endif

 if (model24 == 1) then
    call igrcolourmodel(8)
 endif

 
 call tekflowlinks()

 if (jaHighlight == 1) then
    if (Lnmax .ne. 0) then
       call gtext( 'Lmax', xu(Lnmax), yu(Lnmax), 31  )
    endif
    if (Lnmin .ne. 0) then
       call gtext( 'Lmin', xu(Lnmin), yu(Lnmin), 221 )
    endif
 endif

 if (ndraw(31) .ge. 2) then                          ! cell corners
    call setcol(221)
    do k = 1, size(ucnx)
       if (inview( xk(k), yk(k) ) ) then
          if (ndraw(31) .le. 4)  then                ! numbers
              zn = zcorn(k)
              call dhtext( zn, xk(k), yk(k), zk(k) )
          else if (ndraw(31) == 5) then              ! vectors
              call arrowsxy( xk(k), yk(k), ucnx(k), ucny(k), VFAC)
          endif
       endif
    enddo
 endif

 if (ndraw(30) .ge. 2) then                          ! show links in white
    call setcol(221)  ! NCOLRG)
    do L = 1,lnx
       k1 = ln(1,L)
       k2 = ln(2,L)
       if (inview( xz(k1), yz(k1) ) .or. inview( xz(k2), yz(k2) ) ) then
          XX1 = XZ(K1)
          YY1 = YZ(K1)
          ZZ1 = Bob(1,L)
          XX2 = XZ(K2)
          YY2 = YZ(K2)
          ZZ2 = Bob(2,L)
          if (ndraw(30) .eq. 2) then
             call dmovabs( Xx1, Yy1, Zz1 )
             call dlnabs ( Xx2, Yy2, Zz2 )
             call dcirr  ( Xx1, Yy1, Zz1, 221 )
             call dcirr  ( Xx2, Yy2, Zz2, 221 )
          else if (ndraw(30) .eq. 3) then
             if (L > lnx1D) then
                k3  = lncn(1,L)
                k4  = lncn(2,L)
                X3  = 0.5d0*(Xk(k3)+Xk(k4))
                Y3  = 0.5d0*(Yk(k3)+Yk(k4))
              else ! Arrows for 1D links
                X3=XX1
                Y3=YY1
              end if
              call arrowrcir(x3, y3, csu(L), snu(L) )
          else if (ndraw(30) .eq. 4) then
              if (hu(L) < epshs) then 
                 if (abs(kcu(L)) == 2) then
                    k3  = lncn(1,L)
                    k4  = lncn(2,L)
                    call movabs(xk(k3), yk(k3)) 
                    call  lnabs(xk(k4), yk(k4)) 
                 else 
                    call movabs(xx1, yy1) 
                    call  lnabs(xx2, yy2) 
                 endif  
              endif   
          endif
       endif
    enddo
  
 endif


! do k = 1,ns
!    call ptabs(xs(k), ys(k))
! enddo

 if (ndraw(13) .ge. 2 .and. ndraw(13) .le. 4) then      ! show vectors centre based
     call setcol(KLVEC)
     do kk = 1,ndx,nvec
        if (mod(kk,200) == 0) then
           call halt2(ja)
           if (ja == 1) return
        endif

        if (inview( xz(kk), yz(kk) ) ) then
           k = kk
           if (kmx > 0) then
              call getktoplot(kk,k)
           endif

           if      ( ndraw(13) == 2) then
              uux = ucx(k)
              uuy = ucy(k)
           else if ( ndraw(13) == 3) then
              uux = ucx(k)
              uuy = ucy(k)
           else if ( ndraw(13) == 4) then
              uux = uqcx(k)
              uuy = uqcy(k)
           endif
!           call arrowsxy( xz(kk), yz(kk), uux, uuy, VFAC, 0)
           call arrows( xz(kk), yz(kk), uux, uuy, 0D0, VFAC)
        endif
     enddo
 else if (ndraw(13) .eq. 5) then                        ! show vectors u based
     call setcol(3)
     do L = 1,lnx
        if (inview( xu(L), yu(L) ) ) then
           uux = wx(L)
           uuy = wy(L)
           call arrowsxy( xu(L), yu(L), uux, uuy, VFAC)
        endif
     enddo
 else if (ndraw(13) .eq. 6) then                        ! show arc wind
     call setcol(221)
     call tekarcuv(vfac,ndraw(13))
 else if (ndraw(13) .eq. 7) then                        ! show arc pressure
     call setcol(31)
     call tekarcuv(vfac,ndraw(13))
 else if (ndraw(13) .eq. 8) then                        ! show arc wind
     call setcol(221)
     call tekspw(vfac,ndraw(13))
 else if (ndraw(13) .eq. 9) then                        ! show primitive velocity u1
     call setcol(2)
     do LL = 1,lnx
        if (inview( xu(LL), yu(LL) ) ) then
            L = Lbot(LL) - 1 + min(kplot,kmxL(LL) )
            uux = u1(L)*csu(LL) 
            uuy = u1(L)*snu(LL) 
            call arrowsxy( xu(LL), yu(LL), uux, uuy, VFAC)
        endif
     enddo
 endif

 if (nodneg .ne. 0) then
    call setcol(221)
    call rcirc( xz(nodneg), yz(nodneg) )
 endif

 ! call tekcflmx()
 
 if ( jased.gt.0 .and. jased.le.3 ) call tekbanfs()

 call tekship()

 call tekpart()

 end subroutine tekflowstuff

 subroutine tekbathy(ja)
 use unstruc_display
 use m_flowgeom
 use m_flow
 use gridoperations
 implicit none
 integer :: nodemode, nodewhat,ndraw
 integer :: k, ja, nn, ncol
 double precision :: znod, zn
 common /drawthis/ ndraw(50)

 if (ndraw(39) == 0) return
 
 nodewhat  = ndraw(28)
 ndraw(28) = 3

 do k = 1,ndxi
    if (mod(k,200) == 0) then
       call halt(ja)
       if (ja == 1) then 
          ndraw(28) = nodewhat   
          return
       endif   
    endif
       
    if (inview( xz(k), yz(k) ) ) then
       zn = znod(k)
       call isocol2(zn,ncol)
       nn = size( nd(k)%x )
       call PFILLER(nd(k)%x, nd(k)%y, nn,NCOL,NCol)
    endif
 enddo
 
 ndraw(28) = nodewhat
 end subroutine tekbathy 



 subroutine tekcflmx()
 use m_flowgeom
 use m_flow
 use m_flowtimes
 implicit none
 if (kkcflmx .ne. 0) then
    call setcol(31)
    call rcirc( xz(kkcflmx), yz(kkcflmx) )
    call HTEXT( dtsc, xz(kkcflmx), yz(kkcflmx) ) 
 endif
 end subroutine tekcflmx

 subroutine tekprofpoint()
 use m_flowgeom
 use m_flow
 use unstruc_display
 implicit none
 integer :: k, nn
 if (klprof > 0 .and. nplot.gt.0 ) then
     call cirr(xz(nplot), yz(nplot), klprof)

     ! k    = nplot
     ! nn   = size( nd(k)%x )
     ! call PFILLER(nd(k)%x, nd(k)%y, nn, klprof, klprof)
 endif
 end subroutine tekprofpoint


 subroutine tekflowlinks()
 use unstruc_display
 use m_netw
 use m_flowgeom
 use m_flow
 use m_sferic
 use m_missing
 use gridoperations
 implicit none
 integer :: nodemode, nodewhat,ndraw(50)
 integer :: k, L, ja, ja2, k1, k2, nn, ncol, linkmode
 double precision :: zlin, zL
 double precision :: xcl, ycl, zcl                   ! help only
 double precision :: xx1, yy1, Zz1                   ! help only
 double precision :: xx2, yy2, Zz2                   ! help only
 double precision :: x3, y3, x4, y4                  ! help only
 double precision :: x(4), y(4), z(4), hw, cs, sn
 real             :: xr(4), yr(4)

 common /drawthis/ ndraw

 linkmode = ndraw(11)
 if (LINKMODE > 1 .AND. ndraw(29) .ge. 2) then                          ! show VALUES AT links
   IF (NDRAW(7) == 1) call minmxlns()    ! ONLY ADAPT VERTICAL LIMITS FOR FLOW links IF NO NET links ASKED

     IF (linkmode == 3 .OR. linkmode == 6) THEN

        call copyzlintornod()
        do k = 1,ndx2d
           if (mod(k,200) == 0) then
              call halt2(ja)
              if (ja == 1) return
           endif
           if (inview( xz(k), yz(k) ) ) then
               call ISOSMOOTHflownode2(k)
           endif
        enddo

     else
        do L = 1,lnx
           if (mod(L,200) == 0) then
              call halt2(ja)
              if (ja == 1) return
           endif

           if ( inview( xu(L), yu(L)  ) ) then
              ZZ1 = 0d0  !Bob(1,L)
              ZZ2 = 0d0  !Bob(2,L)

              xcl = xu(L)
              ycl = yu(L)
              zcl=0.5*(ZZ1+ZZ2)

              zl = zlin(L)
              if ( zL.eq.DMISS ) cycle
              CALL ISOCOL2(zl,NCOL)

              k1 = ln(1,L)
              xX1 = Xz(K1)
              yY1 = Yz(K1)
              k2 = ln(2,L)
              xX2 = Xz(K2)
              yY2 = Yz(K2)

              IF (linkmode .EQ. 3 .OR. linkmode .EQ. 6) THEN
                  CALL DMOVABS(XX1,YY1,ZZ1)
                  CALL DLNABS(XX2,YY2,ZZ2)
              ELSE IF (linkmode .EQ. 4 .OR. linkmode .EQ. 7) THEN
                  if (L > Lnx1D) then ! 2D
                     k1 = lncn(1,L)
                     X3 = Xk(K1)
                     Y3 = Yk(K1)
                     k2 = lncn(2,L)
                     X4 = Xk(K2)
                     Y4 = Yk(K2)

                     CALL DRIETWEE(Xx1, Yy1, ZZ1, X(1), Y(1), Z(1) )
                     CALL DRIETWEE(X3, Y3, ZZ1, X(2), Y(2), Z(2) )
                     CALL DRIETWEE(Xx2, Yy2, ZZ2, X(3), Y(3), Z(3) )
                     CALL DRIETWEE(X4, Y4, ZZ2, X(4), Y(4), Z(4) )
                     xr = x
                     yr = y
                     CALL PFILLERCORE(Xr,Yr,4)
                  else
!                     hw    = 0.25d0*( a1(k1) + a1(k2) )/dx(L)
                     if (hu(L) > 0d0) then
                        hw = 0.5d0 * Au(L) / hu(L)   ! flat bed, half width
                     else 
                        hw = 1d-3
                     endif   
                     if (jsferic == 1) then
                        hw = hw*rd2dg/ra
                     endif

                     sn    = snu(L)
                     cs    = csu(L)
                     x(1)  = xx1 + sn*hw
                     y(1)  = yy1 - cs*hw
                     x(2)  = xx2 + sn*hw
                     y(2)  = yy2 - cs*hw
                     x(3)  = xx2 - sn*hw
                     y(3)  = yy2 + cs*hw
                     x(4)  = xx1 - sn*hw
                     y(4)  = yy1 + cs*hw
                     xr = x
                     yr = y
                     CALL PFILLERCORE(Xr,Yr,4)
                 endif
              ELSE IF (linkmode .EQ. 5 .OR. linkmode .EQ. 8) THEN
                  CALL DRCIRC(XCL,YCL,ZCL)
              ENDIF
              if ( linkmode == 2 .or. linkmode == 6 .or. linkmode == 7 .or. linkmode == 8) then
                 IF (NDRAW(29) .EQ. 12 .or. NDRAW(29) .EQ. 29 .or. NDRAW(29) .EQ. 33 .or. NDRAW(29) .EQ. 35 .or. NDRAW(29) .EQ. 36) THEN
                    CALL DHITEXT( int(zl), xCL, yCL, zCL )
                 else
                    call dhtext( zl, xCL, yCL, zCL )
                 end if
              endif

           endif
        enddo
     endif ! linkmode
 endif ! ndraw(29)
 end subroutine tekflowlinks


 subroutine tekflownodes(ja)
 use unstruc_display
 use m_flowgeom
 use m_flow
 use m_missing
 use m_transport
 use gridoperations
 implicit none
 integer :: nodemode, nodewhat,ndraw(50)
 integer :: k, ja, ja2, nn, ncol
 double precision :: znod, zn, x(8), y(8)
 common /drawthis/ ndraw

 nodemode = ndraw(19)
 nodewhat = ndraw(28)
 ja = 0

 if (nodemode == 3) then               ! interpolate rnod on netnodes based upon znod on flownodes
    call copyznodtornod()
 endif
 do k = 1,ndxi
    if (mod(k,200) == 0) then
       call halt(ja)
       if (ja == 1) then 
          return
       endif   
    endif
    if (nodewhat .ge. 2) then
       ja2 = 1
       if (wetplot > 0d0) then
          if (hs(k) < wetplot) then
             ja2 = 0
          endif
       endif
       if (ja2 == 1 .or. nodewhat == 3) then  ! nodewhat==3: always show bottom
          if (inview( xz(k), yz(k) ) ) then
             zn = znod(k)
             if ( zn.eq.DMISS ) cycle
             if (nodemode .eq. 2) then
                call isocol(zn,ncol)
                call dhtext( zn, xz(k), yz(k), bl(k) )
             else if (nodemode == 3   .or. nodemode == 3 + 3) then    ! isolines within cell
                if (k <= ndx2d) then
                   call ISOSMOOTHflownode(k)
                else
                   call isocol(zn,ncol)
                   nn = size( nd(k)%x )
                   call PFILLER(nd(k)%x, nd(k)%y, nn,NCOL,NCol)
                endif
             else if (nodemode .ge. 4 .or. nodemode == 4 + 3) then  ! isofil= cellfill
                call isocol(zn,ncol)
                if ( nodemode == 5    .or. nodemode == 5 + 3 ) then
                   call drcirc(xz(k), yz(k), zn)
                else
                   nn = size( nd(k)%x )
                   call PFILLER(nd(k)%x, nd(k)%y, nn,NCOL,NCol)
                endif
             endif
          endif
       endif
    endif
 enddo
 end subroutine tekflownodes




 subroutine tekarcuv(vfac,met)
 use M_arcuv
 implicit none
 double precision :: vfac
 integer          :: met

 integer          :: mx, nx, i, j

 mx = size(arcuv,2)
 nx = size(arcuv,3)
 do i = 1,mx
    do j = 1,nx
       call setcol(221)
       if (met == 6) then
          call arrowsxy( arcuv(1,i,j) , arcuv(2,i,j), arcuv(3,i,j) , arcuv(4,i,j), 50*VFAC)
       else
          call htext(arcuv(3,i,j), arcuv(1,i,j) , arcuv(2,i,j) )
       endif
    enddo
 enddo
 end subroutine tekarcuv


 subroutine tekspw(vfac,met)
 use m_flowgeom
 use m_spiderweb
 use m_wind
 implicit none
 double precision :: vfac, shft
 integer          :: met

 integer          :: mx, nx, i, j, L

 shft  = 0d0
 mx    = size(spw,2) 
 nx    = size(spw,3)
 if (sum(xu(:)) .lt. 0) then
     shft       = 1d0
 end if
 if (mx.ne.0 .and. nx.ne.0) then
    do i = 1,mx-1
       do j = 1,nx
          call setcol(221)
          call arrowsxy( spw(1,i,j) - shft*360d0, spw(2,i,j), spw(3,i,j) , spw(4,i,j), 0.05*VFAC)
       enddo
    enddo
 endif 
 if (allocated(wx)) then
    do L  = 1,lnxi
       call setcol(224)
       call arrowsxy( xu(L) , yu(L) , wx(L) , wy(L), 0.05*VFAC)
    enddo
 endif
 
 end subroutine tekspw

 subroutine tekrai(nsiz,ja)

 use unstruc_colors
 use m_netw
 use m_flow
 use m_flowgeom
 use m_flowtimes
 use unstruc_model
 use unstruc_display
 use m_raaitek
 use m_missing
 use m_sediment
 use m_strucs
 use m_flowexternalforcings
 use gridoperations
 
 implicit none

 integer          :: nsiz, ja

 double precision :: xx1, xx2, zz
 integer          :: k1, k2, l1, l2, n1, n2
 double precision :: uu, ww, z1, z2
 double precision :: zfac, zgaten
 integer          :: l, k, kk, j, kplotorg, n, ncol

 double precision :: VMAX,VMIN,DV,VAL(256)
 integer          :: NCOLS(256),NIS,NIE,nv,JAAUTO
 double precision :: vfac, vfacforce, doorh
 integer          :: nvec, ng

 common /depmax/ vmax,vmin,dv,val,ncols,nv,nis,nie,jaauto
 COMMON /VFAC/ VFAC,VFACFORCE,NVEC
 common /drawthis/ ndraw(50)
 integer :: ndraw, kts

 double precision :: xmn, xmx, ymx, zmx, zmx2, bot, top, xx, yy, bup
 double precision :: xp(4), yp(4), zp(4), xxmn, xxmx, zn, dlay, dl, xp1, yp1, qsrck
 integer          :: mx, kb, kt, Lb, Lt, LL, kplotfrombedorsurfacesav
 double precision, external    :: znod, zlin

 double precision, allocatable ::   plotlin2(:)
 integer         , allocatable :: ip(:), ip2(:)

 if (ndx  < 1) return

 kplotfrombedorsurfacesav = kplotfrombedorsurface 
 kplotfrombedorsurface    = 1
 
 if (nsiz > 3) then
    call poiseuille(0)
    return
 endif

 xmn = 1e10
 xmx = -xmn
 ymn = 1e10
 ymx = -ymn
 zmn = 1e4
 zmx = -zmn
 do k = 1,ndx
    xx = xz(k)
    yy = yz(k)
    if ( inview(xx,yy) ) then
       if (xz(k) < xmn ) xmn = xz(k)
       if (xz(k) > xmx ) xmx = xz(k)

       if (yz(k) < ymn ) ymn = yz(k)
       if (yz(k) > ymx ) ymx = yz(k)

       bot = bl(k)

       top = s1(k)

       zmn = min( zmn,min( bot, top ) )
       zmx = max( zmx,max( bot, top ) )

    endif
 enddo

 if (jased > 0 .and. zminrai == dmiss .and. .not.stm_included) then
    dlay = 0d0
    if (jaceneqtr == 1) then
       mx = ndxi
    else
       mx = size(grainlay,2)
    endif
    do k  = 1,mx
       if (jaceneqtr == 1) then
           xx = xz(k)
           yy = yz(k)
       else
           xx = xk(k)
           yy = yk(k)
       endif
       if ( inview(xx,yy) ) then
           dL = 0
           do j = 1,mxgr
              dL = dL + grainlay(j,k)
           enddo
           dlay = max(dlay, dL)
       endif
    enddo

    zmn = zmn - dlay

 endif


 if (zmn == zmx) then
    zmn = zmn -1d-3
    zmx = zmx + 1d-3
 else
    zmn = zmn - 1d-2*(zmx-zmn)
 endif

 if (xmn == xmx) then
    xmn = xmn -1d-3
    xmx = xmx + 1d-3
 endif

 if (nsiz == 1) then
    zmx = zmn + 1.2d0*(zmx-zmn)
 else
    zmx = zmn + 1.5d0*(zmx-zmn)
 endif


 if (zminrai .ne. -999) then
    zmn = zminrai
    zmx=max(zmn+1d-2, zmaxrai)
 endif

 if (md_ident == 'transport1d') then
    zmn = 0
    zmx = 30
 endif

 IF (YFAC > 0) THEN
    if (ymx .ne. ymn) then
       yfac = (zmx - zmn)/(ymx-ymn)
    else
       yfac = 0D-4
    endif
 ENDIF

 zmx2 = zmx + yfac*(ymx-ymn)
 if (zmx2 == zmn) zmx2 = zmn + 1


 if (nsiz == 1) then
    call setwor_rai(0.0,0.77,1.0,0.92, x1, zmn, x2, zmx2 )
 else  if (nsiz == 2) then
    call setwor_rai(0.0,0.56,1.0,0.92, x1, zmn, x2, zmx2 )
 else  if (nsiz == 3) then
    call setwor_rai(0.0,0.15,1.0,0.92, x1, zmn, x2, zmx2 )
 endif


 if (kmx > 0) then
    kplot = max(kplot,1)
    kplot = min(kplot,kmxn(nplot))
    kplotorg = kplot
    if (ndraw(28) > 3) then ! show node values


        if (ndraw(19) == 3) then

            if (.not. allocated(plotlin2) ) then
               allocate( plotlin2(lnkx), ip(lnkx), ip2(lnkx) )
            endif
            plotlin  = 0d0
            plotlin2 = 0d0
            ip       = 0
            ip2      = 0
            do LL = 1,lnx
               k1 = ln(1,LL)
               k2 = ln(2,LL)
               call getLbotLtop(LL,Lb,Lt)
               do L = Lb, Lt
                  kplot         = L - Lb + 1
                  plotlin (L)   = plotlin (L)   + znod(k1)
                  ip      (L)   = ip      (L)   + 1

                  kplot         = MAX( kplot - 1, 1)
                  plotlin (L-1) = plotlin (L-1) + znod(k1)
                  ip      (L-1) = ip      (L-1) + 1

                  plotlin2(L-1) = plotlin2(L-1) + znod(k2)
                  ip2     (L-1) = ip2     (L-1) + 1

                  kplot         = L - Lb + 1
                  plotlin2(L)   = plotlin2(L) + znod(k2)
                  ip2     (L)   = ip2     (L) + 1
               enddo
            enddo

            do LL = 1,lnx

               call getLbotLtop(LL,Lb,Lt)
               do L = Lb-1, Lt
                  if (ip(L) > 0) then
                      plotlin (L)  = plotlin (L) / ip(L)
                  endif
                  if (ip2(L) > 0) then
                      plotlin2(L)  = plotlin2(L) / ip2(L)
                  endif
               enddo
            enddo

            do LL = 1,lnx
               k1 = ln(1,LL)
               k2 = ln(2,LL)
               xp(1) = xz(k1)
               xp(2) = xp(1)
               xp(3) = xz(k2)
               xp(4) = xp(3)
               call getLbotLtop(LL,Lb,Lt)
               do L = Lb, Lt
                  k1 = ln(1,L)
                  k2 = ln(2,L)
                  yp(1) = zws(k1)
                  yp(2) = zws(k1-1)
                  yp(3) = zws(k2-1)
                  yp(4) = zws(k2)

                  zp(1) = plotlin(L)
                  zp(2) = plotlin(L-1)
                  zp(3) = plotlin2(L-1)
                  zp(4) = plotlin2(L)

                  call isofil(xp, yp, zp, 4, 0)
               enddo    
            enddo

        else

           do n = 1,ndxi
              xxmn = minval( nd(n)%x )
              xxmx = maxval( nd(n)%x )
              xp(1) = xxmn
              xp(2) = xxmx
              xp(3) = xxmx
              xp(4) = xxmn
              kb = kbot(n)
              kt = ktop(n)
              do k = kb, kt

                 yp(1) = zws(k-1)
                 yp(2) = yp(1)
                 yp(3) = zws(k)
                 yp(4) = yp(3)

                 kplot = k - kb + 1
                 zn    = znod(n)
                 call isocol(zn, ncol)

                 if (ndraw(19) == 2) then
                    call dhtext( zn, xz(N), 0.5D0*(YP(1)+YP(3)) , 0.5D0*(YP(1)+YP(3)) )
                 else
                    call PFILLER(xp,yp,4,ncol, ncol )
                 endif

              enddo
           enddo

        endif
    endif

    if ( ndraw(29) > 1) then ! show link values
        do LL = 1,Lnx
           n1 = ln(1,LL)
           n2 = ln(2,LL)
           xp(1) = xz(n1)
           xp(4) = xp(1)
           xp(2) = xz(n2)
           xp(3) = xp(2)

           Lb = Lbot(LL)
           do L  = Lb , Ltop(LL)
              k1 = ln(1,L)
              k2 = ln(2,L)

              yp(1) = zws(k1-1)
              yp(2) = zws(k2-1)
              yp(3) = zws(k2)
              yp(4) = zws(k1)
              kplot = L - Lb + 1
              zn    = zlin(LL)

              call isocol2(zn, ncol)

              if (ndraw(11) == 2) then
                 xp1 = 0.25d0*(xp(1) + xp(2) + xp(3) + xp(4) )
                 yp1 = 0.25d0*(yp(1) + yp(2) + yp(3) + yp(4) )
                 call dhtext( zn, xp1, yp1, yp1 )
              else
                 call PFILLER(xp,yp,4,ncol, ncol )
              endif
           enddo
        enddo
    endif

    ncol = 221  ! markerpoint
    n = nplot
    k = kbot(n) + kplotorg - 1
      xxmn  = minval( nd(n)%x )
      xxmx  = maxval( nd(n)%x )
      xp(1) = 0.5d0*(xxmx + xxmn)
      yp(1) = 0.5d0*(zws(k) + zws(k-1)) 
      call cirr(xp(1), yp(1), ncol) 
    ! xp(1) = xxmn ; xp(2) = xxmx ; xp(3) = xxmx ; xp(4) = xxmn
    ! yp(1) = zws(k-1)  ; yp(2) = yp(1)
    ! yp(3) = zws(k)    ; yp(4) = yp(3)
    ! call PFILLER(xp,yp,4,ncol, ncol )

    if ( NDRAW(2) > 0 ) then  ! draw interface lines in white

        do LL  = 1,lnxi
           n1  = ln(1,LL)
           n2  = ln(2,LL)
           xx  = xz(n1)
           xx2 = xz(n2)

           if (hu(LL) > 0) then
              Lb  = Lbot(LL)
              Lt = Ltop(LL)
              do L  = Lb, Lt
                 if (hu(L) > 0) then
                    k1 = ln(1,L)
                    k2 = ln(2,L)
                    call movabs(xx ,zws(k1))
                    call  lnabs(xx2,zws(k2))
                 endif
              enddo
           endif

        enddo

    endif

    if (jaanalytic == 0 ) then

       call setcol(ncolblack)
       CALL LINEWIDTH(2)
       do LL  = 1,lnxi
          if (hu(LL) > 0) then
             n1  = ln(1,LL)
             n2  = ln(2,LL)
             xx  = xz(n1)
             xx2 = xz(n2)
             call movabs(xx ,s1(n1))
             call  lnabs(xx2,s1(n2))
          endif
       enddo
    endif

    if ( NDRAW(2) .ge. 1) then
       call setcol(31)
       do LL  = 1, lnxi
          n1  = ln(1,LL)
          n2  = ln(2,LL)
          xx  = xz(n1)
          xx2 = xz(n2)
          call movabs(xx ,bl(n1))
          call  lnabs(xx2,bl(n2))
       enddo
    endif

    CALL LINEWIDTH(1)

    if ( NDRAW(13) .ge. 2) then
        call setcol(klvec)
        zfac = (zmx2-zmn)/(x2-x1)
        do n = 1,ndxi
           xp(1) = xz(n)
           do k = kbot(n),ktop(n)

              uu    = ucx(k)
              ww    = 0.5d0*( ww1(k) + ww1(k-1))
              yp(1) = 0.5d0*(zws(k)+zws(k-1))
              call arrowsxyzfac( xp(1), yp(1), uu, ww, VFAC, 0, zfac)

           enddo

        enddo

    endif

    kplot = kplotorg
 endif

 if ( NDRAW(2) .ge. 1) then
    call tekrailinesbathy(31,0,1) ! bl
 endif   

 if (jased > 0 .and. jased < 4) then
    do j = 1,mxgr
       call tekrailinesbathy(15,0,1+j) ! grainlay 1,2 etc
    enddo
 endif

 if (jagrw >= 1) then 
    call tekrailines(ncolln,1,4) ! pgrw 
    call tekrailines(ncolln,1,5) ! pgrw 
 endif

 if (md_ident == 'transport1d' .or. jasal == 1 .and. ( md_ident == 'wetbed' .or. md_ident == 'wetbed' ) ) then
    call tekrailines(221,1,3) ! sa1
 endif

 if (kmx == 0) then
    call tekrailines(221,1,1) ! s1
    if (nonlin == 2) then 
       call tekrailines(ncolana,1,6) ! s1m
       ! call tekrailines(2,0,2)       ! bob 
       ! call tekrailines(2,0,7)       ! bbb
    endif   
 endif

 call setcol(ncolblack) ! NCOLANA)
 ! call LINEWIDTH(2)

 if (md_IDENT == 'transport1d') then
     call tektransport1D(time1-tstart_user)
     call setcol(3)
     call movabs(xmn, 0d0)
     call lnabs( xmx, 0d0)
     !call htext( 1d0, xmx, 1d0)
 else if (md_IDENT == 'carrier') then
     call carrier(ndx,time1-tstart_user)
 else if (md_IDENT(1:6) == 'drybed') then
     call drybed(time1-tstart_user)
 else if (md_IDENT(1:6) == 'wetbed') then
     call wetbed(time1-tstart_user)      
 else if (md_IDENT(1:12) == 'coriolistilt') then
     call coriolistilt(time1-tstart_user)
 else if (md_IDENT(1:14) == 'corioliskelvin') then
     call corioliskelvin(time1-tstart_user)
 else if (index(md_ident,'thacker1d') > 0) then
    call thacker1d(0,xz,yz,s1,bl,ndx,time1-tstart_user)
 else if (md_IDENT == 'equator1d') then
     call equatorial(time1-tstart_user)
 else if (md_IDENT(1:8) == 'belanger') then
     call belanger()
 endif
 ! call LINEWIDTH(1)

 do ng = 1,ngatesg  ! loop over gate signals, tekrai
    zgaten = zgate(ng)
    do n  = L1gatesg(ng), L2gatesg(ng)
       L  = kgate(3,n) ; k1 = ln(1,L) ; k2 = ln(2,L) 
       bup = min( bob(1,L), bob(2,L) )
       call fbox(xz(k1),zgaten,xz(k2),zgaten+20d0)
       ! call fbox(xz(k1),bup   ,xz(k2),bup-10d0)
    enddo
 enddo

 do ng = 1,ncgensg  ! loop over gate signals, tekrai
    zgaten = zcgen(3*ng-1)
    do n   = L1cgensg(ng), L2cgensg(ng)
       k1       = kcgen(1,n)
       k2       = kcgen(2,n)
       L        = kcgen(3,n)

       bup = min( bob(1,L), bob(2,L) )
       doorh = 10d0
       if (generalstruc(ng)%gatedoorheight < 1d10 .and. generalstruc(ng)%gatedoorheight < 1d10) then
         if (generalstruc(ng)%gatedoorheight > 0d0) doorh = generalstruc(ng)%gatedoorheight 
         call fbox(xz(k1),zgaten,xz(k2),zgaten+doorh)
       end if

       call fbox(xz(k1),bup   ,xz(k2),zmn) ! bup-10d0
    enddo
 enddo
 
 do ng = 1,ncdamsg  ! loop over gate signals, tekrai
    do n  = L1cdamsg(ng), L2cdamsg(ng)
       L  = kcdam(3,n) ; k1 = ln(1,L) ; k2 = ln(2,L) 
       bup = bob(2,L)  ! min( bob(1,L), bob(2,L) )
       call fbox(xz(k1),bup   ,xz(k2),bup-10d0)
    enddo
 enddo
 
 call setcol(121)
 if (kmx > 0) then 
   do n = 1,numsrc                            ! teksorsin rai
     qsrck  = qsrc(n) 
     kk     = ksrc(1,n)                      ! 2D pressure cell nr from
     if (kk .ne. 0 .and.  ksrc(2,n) > 0 ) then 
        xp(1) = xz(kk) 
        bup   = 0.1d0*sqrt(ba(kk)) 
        do k = ksrc(2,n), ksrc(3,n) 
           yp(1) = 0.5d0*( zws(k) + zws(k-1) ) 
           ! CALL KCIR(XP(1),YP(1),qsrck)
           call fbox(xz(kk)-bup, zws(k-1), xz(kk)+bup , zws(k) )
        enddo   
     endif   
    
     kk     = ksrc(4,n)                      ! 2D pressure cell nr to
     if (kk .ne. 0 .and.  ksrc(5,n) > 0) then 
        xp(1) = xz(kk) 
        bup   = 0.1d0*sqrt(ba(kk)) 
        do k = ksrc(5,n), ksrc(6,n) 
           yp(1) = 0.5d0*( zws(k) + zws(k-1) ) 
           ! CALL KCIR(XP(1),YP(1),qsrck)
           call fbox(xz(kk)-bup, zws(k-1), xz(kk)+bup , zws(k) )
        enddo   
     endif   
   enddo   
 endif
 
 if (javeg > 0 .and. kmx > 0) then 
    call setcol(221)
    do k = 1,ndxi
       if (stemheight(k) > 0d0) then 
          call movabs( xz(k), zws(kbot(k)-1) )
          xx = stemheight(k) * sin(phiv(k)) 
          yy = stemheight(k) * cos(phiv(k))
          call  lnabs( xz(k)+xx, zws(kbot(k)-1)+yy )
       endif   
    enddo   
 endif
 
 
 call viewport(0.0,0.0,1.0,1.0)

 if (nsiz > 1 .and. jtextflow > 0) then
    ! assen in 'gewone' aspect=1 wereld coordinaten, anders wordt de text plat afgedrukt in interacter
 !   CALL IGrUnits (0.0,0.0,1.0,1.0)
    CALL setwor(0d0,0d0,1d0,1d0)

    call setcol(3) ! zwart
    zz = 0.05*(zmx-zmn)/nsiz
    call htext_rai( zmn         , x1+12d0*rcir, zmn-2d0*zz,rcir,zz,1)
    call htext_rai( zmx         , x1+12d0*rcir, zmx       ,rcir,zz,1)
    call htext_rai( x1+10d0*rcir, x1+12d0*rcir, zmn-2d0*zz,rcir,zz,2)
    call htext_rai( x2-10d0*rcir, x2-12d0*rcir, zmn-2d0*zz,rcir,zz,2)
 endif

kplotfrombedorsurface = kplotfrombedorsurfacesav
 call setwor(x1,y1,x2,y2)

 return
 end subroutine tekrai

 subroutine setwor_rai(xs1,ys1,xs2,ys2,xw1,yw1,xw2,yw2)
 use m_raaitek
 implicit none
 real             :: xs1,ys1,xs2,ys2
 double precision :: xw1,yw1,xw2,yw2
 call viewport(xs1,ys1,xs2,ys2)
 call setwor  (xw1,yw1,xw2,yw2)
 xs1m = xs1
 ys1m = ys1
 xs2m = xs2
 ys2m = ys2
 xw1m = xw1
 yw1m = yw1
 xw2m = xw2
 yw2m = yw2
 end subroutine setwor_rai

 subroutine htext_rai(val,x,y,xx,zz,ihv)
 use m_raaitek
 implicit none
 double precision  :: val,x,y,xx,zz
 double precision  :: fx, fy, xa, ya
 integer           :: ihv
 fx = xs2m-xs1m
 fy = ys2m-ys1m
 if (ihv == 1) then
    xa = fx*(x-xx-xw1m)/(xw2m-xw1m) + xs1m
    ya = fy*(y   -yw1m)/(yw2m-yw1m) + ys1m
    call movabs(xa,ya)
    xa = fx*(x+xx-xw1m)/(xw2m-xw1m) + xs1m
    call lnabs (xa,ya)
    xa = fx*(x-11d0*xx-xw1m)/(xw2m-xw1m) + xs1m
 else if (ihv == 2) then
    xa = fx*(x   -xw1m)/(xw2m-xw1m) + xs1m
    ya = fy*(y-zz-yw1m)/(yw2m-yw1m) + ys1m
    call movabs(xa,ya)
    ya = fy*(y+zz-yw1m)/(yw2m-yw1m) + ys1m
    call lnabs (xa,ya)
    xa = fx*(x-5d0*xx-xw1m)/(xw2m-xw1m) + xs1m
    ya = fy*(y-3d0*zz-yw1m)/(yw2m-yw1m) + ys1m
 endif
 call htext(val,xa,ya)
 end subroutine htext_rai

 subroutine tekrailines(ncol,jaall,ITYP)
 use m_flowgeom
 USE M_FLOW
 use m_flowtimes
 use m_sferic
 use m_missing
 use unstruc_display
 implicit none
 integer          :: nx, ncol, jaall, ITYP
 integer          :: r, L, k1,k2
 double precision :: zz1, zz2, xz1, xz2
 integer          :: ja

 call setcol(ncol)
 do L = 1,lnx
    if (mod(L,200) == 0) then
       call halt2(ja)
       if (ja == 1) exit
    endif


    k1 = ln (1,L)
    k2 = ln (2,L)

    if (jaall == 1 .and. wetplot > 0d0) then
       if (hu(L) < wetplot) then !  hs(k1) < wetplot .or. hs(k2) < wetplot) then
           cycle
       endif
    endif

    zz1 = dmiss ; zz2 = dmiss
    if (ityp == 1) then
       zz1 = s1(k1)
       zz2 = s1(k2)
    else if (ityp == 2) then
       zz1 = bl(k1)
       zz2 = bl(k2)
    else if (ityp == 3) then
       zz1 = sa1(k1)
       zz2 = sa1(k2)
    else if (ityp == 4) then
       zz1 = pgrw(k1)
       zz2 = pgrw(k2)   
    else if (ityp == 5) then
       zz1 = sgrw1(k1)
       zz2 = sgrw1(k2)      
    else if (ityp == 6) then
       if (L <= lnx1D) then
          zz1 = dmiss ; zz2 = dmiss
          if (prof1D(3,L) < 0) then 
             if ( s1m(k1) > bl(k1) + prof1D(2,L) .or. s1m(k2) > bl(k2) + prof1D(2,L)) then 
                zz1 = s1m(k1)   
                zz2 = s1m(k2) 
             endif   
          endif   
       endif   
    else if (ityp == 7) then 
       if (L <= lnx1D) then
          if (prof1D(1,L) > 0) then 
             zz1 = bl(k1) + prof1D(2,L)  
             zz2 = bl(k2) + prof1D(2,L)
          else   
             zz1 = bl(k1)    
             zz2 = bl(k2)  
          endif   
       else
          zz1 = bl(k1)    
          zz2 = bl(k2)  
       endif   
    endif

    if (zz1 == dmiss .or. zz2 == dmiss) cycle
    
    if (yfac > 0) then
       zz1 = zz1 + (yz(k1) - ymn)*yfac
       zz2 = zz2 + (yz(k2) - ymn)*yfac
    endif

    if (jsferic == 1) then ! jglobe
       if (abs(xz(k1) - xz(k2)) > 10d0) cycle
    endif

    xz1 = xz(k1)
    xz2 = xz(k2)

    if (abs(zz1) < 1d-6) zz1 = 0d0  ! heh heh, eindelijk. -> #@!
    if (abs(zz2) < 1d-6) zz2 = 0d0 

    call movabs(xz1, zz1 )
    call  lnabs(xz2, zz2 )
    
 enddo

 end subroutine tekrailines

 subroutine tekrailinesBATHY(ncol,jaall,ITYP)
 use m_flowgeom
 USE M_FLOW
 use m_flowtimes
 use m_sferic
 use unstruc_display
 use m_netw, only : xk,yk,zk
 use m_sediment

 implicit none
 integer          :: nx, ncol, jaall, ITYP
 integer          :: r, L, k1,k2, kk,k,n
 double precision :: zz1, zz2, xx1, xx2, yy1, yy2
 integer          :: ja, jg

 call setcol(ncol)
 do L = 1,lnx
    if (mod(L,200) == 0) then
       call halt2(ja)
       if (ja == 1) exit
    endif

    if (ityp == 1) then                                ! bottom layer
       if (ibedlevtyp == 1 .or. ibedlevtyp == 6) then  ! tegelen
          k1  = ln  (1,L)
          k2  = ln  (2,L)
          xx1 = xz(k1)
          xx2 = xz(k2)
          zz1 = bl(k1)
          zz2 = bl(k2)
          if (yfac > 0) then
             yy1 = yz(k1)
             yy2 = yz(k2)
          endif
       else
          k1  = lncn(1,L)
          k2  = lncn(2,L)
          xx1 = xk(k1)
          xx2 = xk(k2)
          zz1 = zk(k1)
          zz2 = zk(k2)
          if (yfac > 0) then
             yy1 = yk(k1)
             yy2 = yk(k2)
          endif
       endif
    else                                             ! non erodable layer
       jg = ityp - 1
       if (jaceneqtr == 1 ) then                     ! combined data not really drawn precise if no tegel
          k1  = ln (1,L)
          k2  = ln  (2,L)
          xx1 = xz(k1)
          xx2 = xz(k2)
          zz1 = bl(k1) - sum(grainlay(1:jg,k1) )
          zz2 = bl(k2) - sum(grainlay(1:jg,k2) )
          if (yfac > 0) then
             yy1 = yz(k1)
             yy2 = yz(k2)
          endif
       else
          k1  = lncn(1,L)
          k2  = lncn(2,L)
          xx1 = xk(k1)
          xx2 = xk(k2)
          zz1 = zk(k1) - sum(grainlay(1:jg,k1) )
          zz2 = zk(k2) - sum(grainlay(1:jg,k2) )
          if (yfac > 0) then
             yy1 = yk(k1)
             yy2 = yk(k2)
          endif
       endif
    endif

    if (yfac > 0) then
       zz1 = zz1 + (yy1 - ymn)*yfac
       zz2 = zz2 + (yy1 - ymn)*yfac
    endif

    if (jsferic == 1) then ! jglobe
       if (abs( xz(ln(1,L)) - xz(ln(2,L) ) ) > 10d0) cycle
    endif

    if (abs(zz1) < 1d-6) zz1 = 0d0  ! heh heh, eindelijk
    if (abs(zz2) < 1d-6) zz2 = 0d0

    call movabs(xx1, zz1 )
    call  lnabs(xx2, zz2 )
 enddo

 if (jaceneqtr == 2 .and. ityp ==4) then
     do kk  = 1,mxban
        n   = nban(1,kk)
        k   = nban(2,kk)
        xx1 = xz(k)
        yy1 = yz(k)
        zz1 = zmn + grainlay(1,kk) ! bl(k) - grainlay(1,kk)
        xx2 = xk(n)
        yy2 = yk(n)
        zz2 = zmn + grainlay(1,kk) ! zk(n) - grainlay(1,kk)

        call movabs(xx1, zz1 )
        call  lnabs(xx2, zz2 )
     enddo
 endif

 end subroutine tekrailinesBATHY


 subroutine tektransport1D(tim)
 use m_sferic
 use m_statistics
 use m_flowgeom
 use m_flow
 implicit none
 double precision :: tim
 double precision :: cwave, period, omeg, wlen, rk, phi, xx, yy, dif
 integer          :: k

 cwave   = 60d0*sqrt(10d0*1d-4)                ! chezy
 period = 90d0*60d0
 omeg = twopi/period     ! s
 wlen   = cwave*period
 rk     = twopi/wlen
 do k   = 1,600
    xx  = -50d0 + (k-1)*100d0
    phi = rk*xx - omeg*tim
    yy  = 15d0 + 10d0*cos(phi)
    if (k == 1) then
       call movabs(xx,yy)
    else
       call  lnabs(xx,yy)
    endif
 enddo

 if (ndxi < 1) return

 avedif = 0d0
 do k = 1,ndxi
    xx  = xz(k)
    phi = rk*xx - omeg*tim
    yy  = 15d0 + 10d0*cos(phi)
    dif = abs(sa1(k) - yy)
    avedif = avedif + dif
 enddo
 avedif = avedif/ndxi

 end subroutine tektransport1D




 subroutine hkcircle(x,y,r) ! plotdevice routine interacter is niet goed, zie file fout.bmp
 implicit none
 double precision :: x, y, r
 double precision :: twopi , phi
 integer :: k
 twopi = 2*acos(-1d0)
 call movabs(x+r,y)
 do k = 1,360
    phi = twopi*dble(k)/360.
    call lnabs( x+r*cos(phi), y+r*sin(phi) )
 enddo
 end subroutine hkcircle


 SUBROUTINE DISND(NN)   ! print node values
 use m_devices
 use m_flowgeom
 implicit none
 integer :: nn

 CHARACTER TEX*23
 DOUBLE PRECISION :: ZNOD

 IF (NN .LE. 0) THEN
    TEX = 'NO FLOW NODE FOUND    '
    CALL KTEXT(TEX,IWS-22,4,15)
 ELSE
    TEX = 'FLOW NODE NR:         '
    WRITE(TEX (14:),'(I10)') NN
    CALL KTEXT(TEX,IWS-22,4,15)
    TEX = 'VAL=                  '
    WRITE(TEX(6:), '(E18.11)') ZNOD(NN)
    CALL KTEXT(TEX,IWS-22,5,15)
    TEX = 'XZ =                  '
    WRITE(TEX(6:), '(E18.11)') XZ(NN)
    CALL KTEXT(TEX,IWS-22,6,15)
    TEX = 'yZ =                  '
    WRITE(TEX(6:), '(E18.11)') YZ(NN)
    CALL KTEXT(TEX,IWS-22,7,15)
 ENDIF

 RETURN
 END SUBROUTINE DISND

 SUBROUTINE DISLN(LL)   ! print link values
 use m_flowgeom
 use m_devices
 use network_data, only:kn
 use unstruc_display
 implicit none

 integer :: LL
 CHARACTER TEX*23
 DOUBLE PRECISION :: ZLIN

 IF (LL .LE. 0) THEN
    TEX = 'NO FLOW LINK FOUND    '
    CALL KTEXT(TEX,IWS-22,4,15)
 ELSE
    TEX = 'FLOW LINK NR:         '
    WRITE(TEX (14:),'(I10)') LL
    CALL KTEXT(TEX,IWS-22,4,15)
    TEX = 'VAL=                  '
    WRITE(TEX(6:), '(E18.11)') ZLIN(LL)
    CALL KTEXT(TEX,IWS-22,5,15)
    TEX = 'Nd1:         '
    WRITE(TEX (6:),'(I10)') LN(1,LL)
    CALL KTEXT(TEX,IWS-22,6,15)
    call gtext(tex, xz(ln(1,LL)), yz(ln(1,LL)), 221)
    TEX = 'Nd2:         '
    WRITE(TEX (6:),'(I10)') LN(2,LL)
    CALL KTEXT(TEX,IWS-22,7,15)
    call gtext(tex, xz(ln(2,LL)), yz(ln(2,LL)), 221)
 ENDIF

 RETURN
 END SUBROUTINE DISLN



 subroutine GETSHIPCONTROL()
    use m_ship
    implicit none

    integer :: ndraw
    COMMON /DRAWTHIS/ ndraw(50)

    integer          :: key, n

    CALL InKeyEventIMM(KEY)

    n = 0
                                 !        pijltjesbeweging
    IF (KEY .EQ. 128) THEN
       fstuw(1) = min( 1d0, fstuw(1) + 0.02)
       n = 1
    ELSE IF (KEY .EQ. 129) THEN
       fstuw(1) = max(-1d0, fstuw(1) - 0.02)
       n = 1
    ELSE IF (KEY .EQ. 130) THEN
       fROER(1) = MIN( 1D0, fROER(1) + 0.02)
       n = 1
    ELSE IF (KEY .EQ. 131) THEN
       fROER(1) = MAX(-1D0, fROER(1) - 0.02)
       n = 1
    ELSE IF (KEY .EQ. 53) THEN
       FSTUW(1) = 0D0
       FROER(1) = 0D0
       n = 1
    ENDIF

    IF (KEY .EQ. 87 .OR. KEY .EQ. 87+32) THEN ! W
       fstuw(2) = min( 1d0, fstuw(2) + 0.02)
       n = 2
    ELSE IF (KEY .EQ. 83 .OR. KEY .EQ.  83+32) THEN ! S
       fstuw(2) = max(-1d0, fstuw(2) - 0.02)
       n = 2
    ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32) THEN
       fROER(2) = MIN( 1D0, fROER(2) + 0.02)
       n = 2
    ELSE IF (KEY .EQ. 65 .OR. KEY .EQ. 65+32) THEN
       fROER(2) = MAX(-1D0, fROER(2) - 0.02)
       n = 2
    else if (KEY .EQ. 81 .OR. KEY .EQ. 81+32) then
       FSTUW(2) = 0D0
       FROER(2) = 0D0
       n = 2
    ENDIF

    if (n > 0) THEN
        ndraw(1) = 0        ! no CLS
        call tekship()
    endif

 end subroutine getshipcontrol



subroutine tekbanfs()
use m_netw
use m_flowgeom
use m_flow, only: kbot
use m_sediment
implicit none
double precision :: x, y, z, v, hsk
integer          :: kk, n, k, kj, ncol, ndraw
COMMON /DRAWTHIS/ ndraw(50)

double precision               :: flx  (mxgr)           !< sed erosion flux (kg/s)                 , dimension = mxgr
double precision               :: seq  (mxgr)           !< sed equilibrium transport rate (kg/m/s) , dimension = mxgr
double precision               :: wse  (mxgr)           !< effective fall velocity (m/s)           , dimension = mxgr, ws*crefa=wse*seq

if (ndraw(34) <= 1 .or. jaceneqtr == 1 .or. jased == 0 ) return
call setcol(3)
do kk = 1,mxban

    call getequilibriumtransportrates(kk, seq, wse, mxgr, hsk)    ! get per netnode and store in small array seq

    n = nban(1,kk)  ! net node
    k = nban(2,kk)  ! flow node

    x = 0.5d0*(xk(n) + xz(k))
    y = 0.5d0*(yk(n) + yz(k))
    v = seq(jgrtek)
    call isocol(v,ncol)

    if (ndraw(34) == 2) then
       CALL dHTEXT(seq(jgrtek),X,Y,Z)
    else if (ndraw(34) == 3) then
       !CALL dHTEXT(seq(jgrtek)-sed(jgrtek,kbot(k)),X,Y,Z)
       CALL dHTEXT(seq(jgrtek)-sed(jgrtek,kbot(k)),X,Y,Z)
    else if (ndraw(34) == 4) then
       z = n
       CALL dHTEXT(z,X,Y,Z)
    else if (ndraw(34) == 5) then
       z = k
       CALL dHTEXT(z,X,Y,Z)
    else if (ndraw(34) == 6) then
       z = kk
       CALL dHTEXT(z,X,Y,Z)
    endif

 enddo

 end subroutine tekbanfs



 subroutine slnabs(n,sx1,sy1)
 implicit none
 integer          :: n
 double precision :: sx1,sx2,sy1,sy2
 call shipcoor(n,sx1,sy1,sx2,sy2)
 call lnabs(sx2,sy2)
 end subroutine slnabs

 subroutine smovabs(n,sx1,sy1)
 implicit none
 integer          :: n
 double precision :: sx1,sx2,sy1,sy2
 call shipcoor(n,sx1,sy1,sx2,sy2)
 call movabs(sx2,sy2)
 end subroutine smovabs

 subroutine shtext(n,snum,sx1,sy1)
 implicit none
 integer          :: n
 double precision :: snum,sx1,sx2,sy1,sy2
 call shipcoor(n,sx1,sy1,sx2,sy2)
 call htext(snum,sx2,sy2)
 end subroutine shtext


 subroutine isosmoothflownode(k) ! smooth isolines in flow cells
 use m_flowgeom
 use m_flow
 use m_netw
 implicit none
 integer :: k

 integer          :: nn4, n
 double precision :: zz(10)

 nn4 = size(nd(k)%nod)
 do n = 1, nn4
    zz(n) = rnod( nd(k)%nod(n) )
 enddo
 nn4 = min(nn4, size(nd(k)%x) )
 call isofil(nd(k)%x, nd(k)%y, zz, nn4, 0)
 !call isocel(nd(k)%x, nd(k)%y, zz, nn4, 0)
 end subroutine isosmoothflownode

subroutine isosmoothflownode2(k) ! smooth isolines in flow cells use depmax2
 use m_flowgeom
 use m_flow
 use m_netw
 implicit none
 integer :: k

 integer          :: nn4, n
 double precision :: zz(10)

 nn4 = size(nd(k)%nod)
 do n = 1, nn4
    zz(n) = rnod( nd(k)%nod(n) )
 enddo
 nn4 = min(nn4, size(nd(k)%x) )
 call isofilb(nd(k)%x, nd(k)%y, zz, nn4, 0)
 end subroutine isosmoothflownode2

 subroutine isosmoothnet(k) ! smooth isolines in net cells
 use m_flowgeom
 use m_flow
 use m_netw
 implicit none
 integer :: k

 integer          :: nn4, n, inode
 double precision :: xx(10), yy(10), zz(10)

 nn4 = size(netcell(k)%nod)
 do n = 1, nn4
    inode = netcell(k)%nod(n)
    xx(n) = xk(inode)
    yy(n) = yk(inode)
    zz(n) = rnod(inode)
 enddo
 call isofil(xx, yy, zz, nn4, 0)
 end subroutine isosmoothnet


 SUBROUTINE TEXTFLOW()
 use m_flowgeom
 !USE M_NETW
 USE M_FLOW
 USE M_FLOWTIMES
 use m_reduce, only : nocg, nogauss, noexpl, nowet
 use M_RAAITEK
 use m_statistics
 USE UNSTRUC_MODEL, only: md_ident
 use unstruc_colors
 use m_transport, only: nsubsteps, numnonglobal
! use m_equatorial, only : ampliforced, amplifreeL, amplitotal, ndxforced, ndxfreeL, ndtforced, ndtfreeL, cflforced, cflfreeL, tforce, tfreeL, amplicomp
 implicit none
 double precision,external :: znod, zlin
 double precision :: cpuperstep, solrest, znn, dtav
 integer :: nn, LL, nl

 CHARACTER TEX*210
 character, save :: TEX1*210 = '@'
 character, save :: TEX2*210 = ''
 character, save :: TEX3*210 = ''
 character(len=4) :: c_nsubsteps
 character(len=7) :: c_numnonglobal
 character(len=15) :: c_lts
 integer, save :: mout = 0
 integer, save :: eeini = 0

 integer :: ndraw
 COMMON /DRAWTHIS/ ndraw(50)

 if (jtextflow < 1) return

 if (ndx < 1) return

! erase previous text
 if ( trim(tex1).ne.'@' ) then
    call setxor(0)
    CALL ITEXTCOLOUR('BWHITE','BBLUE')
    call IClearLine(2)
    call IClearLine(3)
    call IClearLine(4)
    call IClearLine(5)
 end if

! call setxor(1)

 TEX =  ' '
 solrest = 0
 if (cpusteps(3)-cpusol(3) .ne. 0) solrest = cpusol(3)/ (cpusteps(3)-cpusol(3) )
 cpuperstep = max(0d0, min(100d0,  (cpusteps(2) - cpusteps(1)) ) )

 call maketime(tex,time1)

 if (dnt-1 > 0) then
    dtav = (time1-Tstart_user)/ dnt
 else
    dtav = dts
 endif

 WRITE (TEX(18:),'( A4,F8.3, A8,F7.3, A10,F7.3, A5,F8.1, A10,E7.2, A8,E14.8,  A8,E14.8)') &
 'dt: ', dts, ' Avg.dt: ', dtav, &
 ' CPU/step: ', cpuperstep, ' Tot: ', cpusteps(3), ' Sol/Rest: ', solrest , ' Samer: ', samerr, ' Samtot: ', sam1tot ! sam1tot ! samerr
 CALL ICTEXT(TRIM(TEX),13,2,221)
 TEX1=TEX

 nn  = min(nplot,ndx)
 TEX =  ' '
 if (ndraw(29) <2) then
    znn = znod(nn)
    WRITE (TEX,'(A,I3,I6,A,e14.8,A,e14.8,A,e14.8,A,I6,A,I10,A,I10)') &
    'k/nplot: ', KPLOT, nplot, ' znod(nn): ', znn, ' Vol1: ', vol1tot,  ' Vler: ', volerrcum,  &
    ' #setb: ', int(dsetb), ' #dt: ', int(dnt), ' #itsol: ', itsol
 else
    call getlink1(nn,nl) 
    znn = zlin(nl)
    WRITE (TEX,'(A9,I3,1X,I6,1X,A,e14.8,1X,A10,e14.8,1X,A8,e14.8,1X,A7,I6,1X,A5,I10,1X,A8,I5)') &
    'k/nplot: ', KPLOT, nplot, 'zlin(nn): ', znn, 'Vol1: ', vol1tot,  'Vler: ', volerrcum,  &
    '#setb: ', int(dsetb), '#dt: ', int(dnt), '#itsol: ', itsol
 endif
 
 CALL ICTEXT(TRIM(TEX),13,3,221)
 TEX2=TEX

 TEX =  ' '
 
! make string for local time-stepping
 if ( nsubsteps.eq.1 ) then
    write(c_lts, "(15A)") ' '
    if (nonlin == 2) then 
       c_lts = '#s1mit: ' 
       write(c_lts(9:), '(i4)') min(9999, nums1mit)  
    endif   
 else
    write(c_nsubsteps, "(i4)") min(nsubsteps,9999)   ! min: safe text width
    write(c_numnonglobal, "(i7)") min(numnonglobal,9999999)   ! min: safe text width
    c_lts = 'lts:' // trim(adjustl(c_nsubsteps)) // '|' // trim(adjustl(c_numnonglobal))
 end if
     
 if (kmx == 0) then 
    WRITE (TEX,'( A,i8,  A,I8,  A,I4,  A,I8,1  A,I8,  A,I4, A, I2.0, I1, I1, I1, I1, A, A, A15 )')        &
    '#ndx: ' , ndx, ' #lnx: ', lnx, ' #kmx : ', kmx, ' #CG: ', nocg, ' #Gauss: ', nogauss,      &
    ' #s1it: ', min(9999,nums1it), ' iad: ', iadvec, limtypmom, limtypsa, javasal, javau,  ' runid: '//trim(md_ident), ' ', c_lts
 else
    call getlink1(nn,LL) 
    WRITE (TEX,'( A,i8,  A,I8,  A,I4,  A, F8.5, A, F8.5,  A,I4, A, I2.0, I1, I1, I1, I1, A, A, A14)')    &
    '#ndx: ' , ndx, ' #lnx: ', lnx, ' #kmx : ', kmx, ' ustB ', min(ustb(LL),1d2), ' ustW ', ustw(LL),    &
    ' #s1it: ', min(9999,nums1it), ' iad: ', iadvec, limtypmom, limtypsa, javasal, javau,  ' runid: '//trim(md_ident), ' ', c_lts
 endif
 
 CALL ICTEXT(TRIM(TEX),13,4,221)
 TEX3=TEX
 
 call setxor(0)
 
 call textflowspecific()
  
 RETURN
 END SUBROUTINE


  subroutine tekprofs()                         ! and initialise some turb pars
  use m_flow
  use m_flowgeom
  use m_wearelt
  use M_RAAITEK
  use m_observations
  use m_missing
  use m_polygon
  use m_wind
  use m_flowtimes
  use unstruc_model,   only : md_ident
  USE UNSTRUC_DISPLAY
  use m_waves, only : ustokes
  use m_sediment, only : jased, sed
  use m_transport, only : constituents, numconst, itemp, iconst_cur, const_names, NUMCONST, ISED1, ISEDN
  implicit none


  integer          :: ini = 0, kt, mout = 0, jaref
  double precision :: vmin, vmax, ugem, viceld
  integer          :: n, kb, kbn, kbn1, km, km1, k, kk, ku, kd, kku, kkd, Lb0, Lb, Lt, Lm1, L, LL, La
  double precision :: zmin, zmax
  double precision :: h0, b0, z00, zinc, cz, cf, ustbref, ustwref, zint, z1, dz2, zz
  double precision :: tkebot, tkesur, tkewin
  double precision :: epsbot, epssur, epswin, dzkap, sqcf, ulx, sg, drhodz, rhomea

  double precision :: VMAX2,VMIN2,DV2,VAL2
  integer          :: NCOLS2,NV2,NIS2,NIE2,JAAUTO2, is, Ls, LLs, Lbs, Lts
  integer          :: iconst

  integer          :: ndraw
  COMMON /DRAWTHIS/ ndraw(50)
  COMMON /DEPMAX2/ VMAX2,VMIN2,DV2,VAL2(256),NCOLS2(256),NV2,NIS2,NIE2,JAAUTO2


  if (ndx < 1 .or. kmx < 2 .or. ndraw(35) == 0) return

  n  = nplot
  kb = kbot(n)
  kt = ktop(n)
  if (kt - kb + 1 < 2) then 
     return                  ! for less than 2 layers
  endif
     
  uLx = 0d0
  LL  = 0
  do kk = 1, nd(n)%lnx
     L  = nd(n)%ln(kk) 
     La = iabs(L)  
     Lb = Lbot(La)
     is = 1 ; if (L < 0) is = -1
     if ( is*u1(Lb) > uLx ) then ! search link with highest outflow velocity
        LL  = La
        uLx = is*u1(Lb)
     endif
  enddo
  if (LL == 0) then
     LL = La
  endif   
  if (hu(LL) < epshu) then 
     LL = 0
  else
     Lb  = Lbot(LL)
     Lb0 = Lb -1
     Lt  = Ltop(LL)
     Lm1 = Lt - Lb0 + 1
  endif   
     
  L = LL
  
  if (ini == 0) then
     call MAKEPLOTAREAS(2, 4, 1) ! ndraw(35))
     ini = 1
  endif

  b0 = zws(kb-1)
  h0 = zws(kt) - b0
  if (h0 < epshu) return
  ! h0 = 5d0 ! slope

  zmin = 0d0
  zmax = 1.1d0*h0 ! + 1d0

  if (zmaxrai .ne. dmiss .and. zminrai .ne. dmiss) then
      zmax = zmaxrai - zminrai
  endif

  km    = kt - kb + 1
  km1   = km + 1

  ugem  = sum(ucx(kb:kt)) / dble(kt - kb + 1)

  hwref(0) = 0d0
  do k  = kb,kt
     kk = k - kb + 1
     hcref(kk) = 0.5d0* ( zws(k) + zws(k-1) )  - b0
     hwref(kk) = zws(k)                        - b0
  enddo

  jaref = index(md_ident,'slope') 
  if (LL > 0) then
  if (bedslope == 0d0 ) then
     zinc = max(1d-20, ( s1(ln(2,LL)) - s1(ln(1,LL)) )*dxi(LL) )
  else
     zinc = bedslope
  endif

  if (zinc > 0) then
     sg = -1d0
  else
     sg =  1d0
  endif
  zinc  = abs(zinc)
 
  if (frcuni > 0 .and. jaref > 0 )  then
     call getczz0 (h0, frcuni, ifrctypuni, cz, z00)
     ugem  = Cz*sqrt(h0*zinc)
     sqcf  = sag/Cz
     ustbref = sqcf*ugem  ! ustb(LL)
     ustwref = ustw(LL) 

     viceld  = vonkar*ustbref*h0/6d0

     do k   = kb,kt
        kk  = k - kb + 1
        ucxref(kk) = sg*ustbref * log( c9of1 + hcref(kk)/z00) / vonkar
     enddo

     if (iturbulencemodel == 1) then

        vicwref = vicoww

     else if (iturbulencemodel == 2) then


        do k    = 1,km-1
           zint = hwref(k) / h0
           z1   = 1d0 - zint
           zz   = h0*z1*zint
           vicwref(k) = zz * ustbref * vonkar
        enddo
        vicwref (0)  = 0d0
        vicwref(km)  = 0d0

     else if (iturbulencemodel >= 3) then

        tkebot = ustbref**2/sqcmukep
        tkewin = ustwref**2/sqcmukep
        tkesur = max(tkewin,ustbref**2)
        ! tkesur = 0d0
        epsbot = cewall*tkebot**1.5d0
        epssur = cewall*tkesur**1.5d0

        ! TKE and epsilon at layer interfaces:
        do k   = 1,km-1
           zint     = hwref(k) / h0
           z1       =  1d0 - zint
           tkin1ref(k) = tkebot * z1    + tkesur * zint
           teps1ref(k) = (epsbot /zint  + epssur / z1)/h0 
           teps1ref(k) = max(epseps, teps1ref(k) ) 
           vicwref (k) = cmukep*tkin1ref(k)**2/abs(teps1ref(k))
        enddo

        ! TKE, epsilon and mixing coefficients at free surface:
        tkin1ref(km) = tkesur
        teps1ref(km) = epssur / ( hwref(km)-hwref(km-1) )
        ! TKE, epsilon and mixing coefficients at bed:
        tkin1ref(0)  = tkebot
        teps1ref(0)  = epsbot / ( hwref(1)-hwref(0) )!  dzcs(kb)

        dzkap        = vonkar*0.5d0*( hwref(1)-hwref(0) )
        teps1ref(0)  = epsbot  / dzkap
        vicwref (0)  = ustbref * dzkap
        vicwref(km)  = 0d0
    endif
 
    if (dnt == 0) then ! at initialise : copy refprofiles to solution

       do LLs = 1,lnx
          Lbs = Lbot(LLs) ; Lts = Ltop(LLs)
          do Ls = Lbs, Lts
             k  = Ls-Lbs+1
             u1(Ls)      = csu(LLs)*ucxref(k)
             turkin1(Ls) = tkin1ref(k) 
             tureps1(Ls) = teps1ref(k) 
          enddo  
          turkin1(Lbs-1) = tkin1ref(0) 
          tureps1(Lbs-1) = teps1ref(0) 

       enddo

    endif

  endif
  
  
  do kk = 1,km-1
     kku     = kk + 1
     k       = kb + kk - 1
     ku      = k  + 1
     dz2     = ( hcref(kku) - hcref(kk) )**2
     if (jaref > 0) dijdijref(kk) = ( ( ucxref(kku) - ucxref(kk) )**2 )  /  dz2
     dijdij(kk) =  ( ( ucx   (ku)  - ucx   (k)  )**2 )  /  dz2
  enddo
  dijdijref(0) = 0d0 ! ustbref / max(1d-6,vicwref(0) )
  dijdij   (0) = 0d0 !
  endif


  ! TEKFN(NSC,NF,JW,X,Y,N,X1,X2,Y1,Y2,NCOL,TITLE,JAUTO,JP,DAG)
  ! NSC    schermnr
  ! NF     functienr
  ! JW     update assen    1 = ja, niet 1 = nee
  ! JAUTO  zelf schalen    1 = ja, niet 1 = nee
  ! JP     teken profielen 1 = ja, niet 1 = teken isolijnen
  ! in dat geval DAG (nr van de dag) toevoegen


 

  if (ndraw(35) == 1) then ! turbulence profiles etc

  ucm(1:km) = sqrt( ucx(kb:kt)*ucx(kb:kt) + ucy(kb:kt)*ucy(kb:kt) )
  call getvminmax(1,vmin,vmax,ucm, km) 
  vmin = 0d0 ; vmax = 1d0
  if (jaref > 0) then
  call TEKFN(1, 1, 0, ucxref      , hcref   , km, vmin, vmax, zmin, zmax,  31, 'vel. mag.' , 0, 1 , 0d0,0)   ! mid-layers
  endif

  call TEKFN(1, 2, 1, ucm(1:km)  , hcref   , km, vmin, vmax, zmin, zmax, KLPROF, 'vel. mag.' , 0, 2 , 0d0,kplot)

  if (LL > 0) then
  vmin = 0d0
  vmax = 0.0d0
  vmax = max(vmax, maxval(vicwwu(Lb0:Lt)), vmin+1d-5 )
  if (jaref> 0 ) then
  call TEKFN(2, 3, 0, vicwref      , hwref   , km1,vmin, vmax, zmin, zmax,  31, 'vicww'      , 0, 1 , 0d0,0)   ! mid-layers
  endif

  if (LL > 0 ) then 
  call TEKFN(2, 4, 1, vicwwu(Lb0:)  , hwref   , Lm1, vmin, vmax, zmin, zmax, KLPROF, 'vicww'      , 0, 2 , 0d0,kplot+1)
  endif
  endif
  
 ! vmax = 0.1d0  ; vmin = 0d0
 ! if (frcuni > 0 .and. ndraw(35) == 1 ) then
 ! call TEKFN(3, 5, 0, dijdijref(1:), hwref(1:) , km-1, vmin, vmax, zmin, zmax,  31, 'dijdij'    , 0, 1 , 0d0,0)   ! interfaces
 ! endif
 ! call TEKFN(3, 6, 1, dijdij(1:)   , hwref(1:) , km-1, vmin, vmax, zmin, zmax, KLAXS, 'dijdij'    , 0, 2 , 0d0,kplot)

 ! vmin = -0.15d0; vmax = 0.15d0
 ! call TEKFN(3, 6, 1, qw(kb:kt)    , hwref(1:) , km, vmin, vmax, zmin, zmax, KLAXS, 'qw'    , 1, 2 , 0d0,kplot)
 
  vmax = max(minval(ww1(kb:kt)), maxval(ww1(kb:kt)) )
  vmin = -vmax
  call TEKFN(3, 6, 1, ww1(kb:kt)    , hwref(1:) , km, vmin, vmax, zmin, zmax, KLPROF, 'ww1'    , 1, 2 , 0d0,kplot)


  if (iturbulencemodel >= 3 .and. LL > 0) then

     if (frcuni > 0  .and. ndraw(35) == 1 ) then
        vmin = 0d0 ; vmax = 0d0 ; vmax = max(vmax, maxval(turkin1(Lb0:Lt)), vmin+1d-5 )
        if (jaref > 0) call TEKFN(4, 7, 0, tkin1ref    , hwref   , km1, vmin, vmax, zmin, zmax,  31, 'tkin1'      , 0, 1 , 0d0,0)   ! interfaces
        call TEKFN(4, 8, 1, turkin1(Lb0:Lt), hwref  , Lm1, vmin, vmax, zmin, zmax, KLPROF, 'tkin1'      , 0, 2 , 0d0,kplot+1)
     endif

     if (jasal > 0 .and. jatem > 0 .and. idensform > 0)  then 
        call getvminmax(6,vmin,vmax,rho(kb:), kt-kb+1) 
        call TEKFN(5,10, 1, rho(kb:kt)  , hcref  , km, vmin, vmax, zmin, zmax, KLPROF, 'rho' , 1, 2 , 0d0,kplot)
     else
        if (frcuni > 0 .and. ndraw(35) == 1 ) then
           vmin = 0d0 ; vmax = 0.d0 ; vmax = max(vmax, maxval(tureps1(Lb0:Lt)), vmin+1d-5 )
           if (jaref > 0)call TEKFN(5, 9, 0, teps1ref    , hwref   , km1, vmin, vmax, zmin, zmax,  31, 'teps1'      , 0, 1 , 0d0,0)   ! interfaces
           call TEKFN(5,10, 1, tureps1(Lb0:Lt), hwref  , Lm1, vmin, vmax, zmin, zmax, KLPROF, 'teps1'      , 0, 2 , 0d0,kplot+1)
        endif
     endif  

  endif

  if (jasal > 0) then
     
      call getvminmax(6,vmin,vmax,sa1(kb:), kt-kb+1) 
      call TEKFN(6,11, 1, sa1(kb:kt)  , hcref  , km, vmin, vmax, zmin, zmax, KLPROF, 'sal' , 1, 2 , 0d0,kplot)

    ! do k = kb,kt-1
    !    kk     = k-kb+1
    !    drhodz    = ( rho(k+1) - rho(k) ) / (hcref(kk+1) - hcref(kk))
    !    rhomea    = 0.5d0*( rho(k+1) + rho(k) )
    !    bruva(kk) = coefn2*drhodz
    ! enddo

  else if (jatem > 0) then
     
      !call TEKFN(6,11, 1, tem1(kt:kt) , hcref(kt-kb+1)  , 1, 0d0, 86400.d0, -200d0, 600d0, KLPROF, '-200 - 600 WATT' , 0, 2 , 0d0,kplot)
      !CALL TEKHEATS( time1)

     do k = kb,kt-1
        kk     = k-kb+1
        L      = Lb0 + k - kb
        drhodz     = ( rho(k+1) - rho(k) ) / (hcref(kk+1) - hcref(kk))
        rhomea     = 0.5d0*( rho(k+1) + rho(k) )
        dijdij(kk) = tureps0(L)*coefn2*drhodz/rhomea
     enddo
     dijdij(0) = 0d0 ;       dijdij(km) = 0d0
     vmin = minval(dijdij(1:km-1))
     vmax = maxval(dijdij(1:km-1))
     if (abs(vmin) < vmax) vmin = -vmax
     if (vmax < abs(vmin)) vmax = -vmin
     if (abs(vmin-vmax) < 1d-20) then 
        vmax = vmax + 1d-5 ; vmin = vmin - 1d-5
     endif
     call TEKFN(6,11, 1, dijdij(0:km), hwref  , Lm1, vmin, vmax, zmin, zmax, KLPROF, 'Bruva'      , 0, 2 , 0d0,kplot+1)
     
  else if ( iconst_cur.gt.0 .and. iconst_cur.le.NUMCONST ) then
      
      vmin =  1d2
      vmax = -1d2
      vmin = min(vmin, minval(constituents(iconst_cur,kb:kt)) )
      vmax = max(vmax, maxval(constituents(iconst_cur,kb:kt)), vmin+1d-5 )
      
      call TEKFN(6,11, 1, constituents(iconst_cur,kb:kt)  , hcref  , km, vmin, vmax, zmin, zmax, 221, trim(const_names(iconst_cur)) , 1, 2 , 0d0,kplot)
  
      !vmin = 1d2
      !vmax = -1d2
      !do iconst=ISED1,ISEDN
      !   vmin = min(vmin, minval(constituents(iconst,kb:kt)) )
      !   vmax = max(vmax, maxval(constituents(iconst,kb:kt)), vmin+1d-5 )
      !end do
      !
      !do iconst=ISED1,ISEDN
      !   call TEKFN(6,11, 1, constituents(iconst,kb:kt)  , hcref  , km, vmin, vmax, zmin, zmax, 221, 'all seds', 1, 2 , 0d0,kplot)
      !end do
  
 
  else

   if (frcuni > 0 .and. ndraw(35) == 1 .and. LL > 0) then
      ! if (jaref > 0) call TEKFN(5, 9, 0, teps1ref    , hwref   , km1, vmin, vmax, zmin, zmax,  31, 'teps1'      , 0, 1 , 0d0,0)   ! interfaces
      dijdij(1:km-1) = (vicwwu(Lb:Lt-1)+vicoww)*( u1(Lb+1:Lt)-u1(Lb:Lt-1) )*  2d0 /  ( hu(Lb+1:Lt)+hu(Lb:Lt-1) )
      dijdij(0)    = ustb(L)*ustb(L)
      if ( csu(L)*u1(Lb) < 0 ) dijdij(0)  = - dijdij(0)  
      dijdij(km)   = ustw(L)*ustw(L)
      if ( wdsu(L) < 0)        dijdij(km) = -dijdij(km) 
    
      call getvminmax(6,vmin,vmax,dijdij(0:km), km+1) 
      call TEKFN(6,11, 1, dijdij(0:km), hwref  , Lm1, vmin, vmax, zmin, zmax, KLPROF, 'Reyn'      , 0, 2 , 0d0,kplot+1)
   endif
 
  endif

  if (jatem > 0) then 
     if (jafahrenheit > 0) then 
        dijdij(1:km) = 32d0 + constituents(itemp, kb:kt)*1.8d0
        vmin = 70d0 ; vmax = 90d0
        call TEKFN(7, 13, 1, dijdij(1:km) , hcref   , km, vmin, vmax, zmin, zmax, KLPROF, 'Temp (F)' , 0, 2 , 0d0,kplot)
     else   
        dijdij(1:km) = constituents(itemp, kb:kt)    
        call getvminmax(7,vmin,vmax,dijdij(1:km), km) 
        call TEKFN(7, 13, 1, dijdij(1:km) , hcref   , km, vmin, vmax, zmin, zmax, KLPROF, 'Temp (C)' , 0, 2 , 0d0,kplot)
     endif   
  else if (jased > 0 .and. jased < 4) then 
     vmin = 1d2 ; vmax = -1d2  
     vmin = min(vmin, minval(sed(1,kb:kt)) )
     vmax = max(vmax, maxval(sed(1,kb:kt)), vmin+1d-5 )
     call TEKFN(7, 13, 1, sed(1,kb:kt)  , hcref   , km, vmin, vmax, zmin, zmax, KLPROF, 'sed' , 0, 2 , 0d0,kplot)
  else 
     vmin = 1d2 ; vmax = -1d2  
     vmin = min(vmin, minval(ucy(kb:kt)) )
     vmax = max(vmax, maxval(ucy(kb:kt)), vmin+1d-5 )
     call TEKFN(7, 13, 1, ucy(kb:kt)  , hcref   , km, vmin, vmax, zmin, zmax, KLPROF, 'y-velocity' , 0, 2 , 0d0,kplot)
  endif
  
  vmin = minval(ucx(kb:kt)) 
  vmax = maxval(ucx(kb:kt))
  vmax = max(abs(vmin), abs(vmax), 1d-4 ) ; vmin = -vmax
  if (jawaveStokes == 0) then 
     call TEKFN(8, 12, 1, ucx(kb:kt)  , hcref   , km, vmin, vmax, zmin, zmax, KLPROF, 'x-velocity' , 0, 2 , 0d0,kplot)
  else
     vmin = minval( ucx(kb:kt)-ustokes(Lb:Lt) ) 
     vmax = maxval( ucx(kb:kt)-ustokes(Lb:Lt) ) 
     vmax = max(abs(vmin), abs(vmax) ) ; vmin = -vmax
     dijdij(1:km) =  ucx(kb:kt)-ustokes(Lb:Lt)
     call TEKFN(8, 12, 1,  dijdij(1:km)  , hcref   , km, vmin, vmax, zmin, zmax, KLPROF, 'x-velocity' , 0, 2 , 0d0,kplot)
  endif
     
  else if (jasal > 0) then

      if (vmin2 > vmax2) then
         vmin = 0d0
         vmax = 33d0
      else
         vmin = vmin2
         vmax = vmax2
      endif
      if (ndraw(35) == 2) then
         do n  = 1, min(8,numobs)
            kk = kobs(n)
            if (kk.lt. 1) cycle
            call getkbotktop(kk,kb,kt)
            if (kt > kb) then
               call TEKFN(n,2*n-1, 1, sa1(kb:kt)  , hcref  , km, vmin, vmax, zmin, zmax, KLPROF, 'sal' , 0, 2 , 0d0,kplot)
            endif
         enddo
      else if (ndraw(35) == 3) then
         do n  = 1, min(8,npl)
            call in_flowcell(xpl(n), ypl(n), kk)
            if (kk == 0) cycle
            call getkbotktop(kk,kb,kt)
            if (kt > kb) then
               call TEKFN(n,2*n-1, 1, sa1(kb:kt)  , hcref  , km, vmin, vmax, zmin, zmax, KLPROF, 'sal' , 0, 2 , 0d0,kplot)
            endif
         enddo
      endif

  endif

  call FULLSCREEN()
  call setwor(x1,y1,x2,y2) ! reset horizontal world coordinates

  call tekprofpoint()

  end subroutine tekprofs
  
  subroutine getvminmax(num,vmin,vmax,v, n) 
  use unstruc_display
  use m_missing
  implicit none
  integer             :: n
  integer, intent(in) :: num
  double precision    :: vmin, vmax, v(n)
  
  if (profmin(num) == dmiss) then 
      vmin = 1d9
      vmin = min(vmin, minval(v(1:n)) )
  else
      vmin = profmin(num)
  endif

  if (profmax(num) == dmiss) then 
      vmax = -1d9
      vmax = max(vmax, maxval(v(1:n)), vmin+1d-5 )
  else 
      vmax = profmax(num)
  endif   
  end subroutine getvminmax 
  
  SUBROUTINE TEKHEATS( TIMNOW)
  use m_heatfluxes
  implicit none
  double precision       :: TIMNOW, tday
  
  TDAY = modulo (TIMNOW, 1440d0*60d0) 
  CALL GTEXT('SUN',TDAY,QSunav  ,221)
  CALL GTEXT('LWR',TDAY,QLongav ,221)
  CALL GTEXT('CON',TDAY,QEVAav  ,221)
  CALL GTEXT('EVA',TDAY,QCONav  ,221)
  CALL GTEXT('fre',TDAY,Qfreeav ,221)

  RETURN
   END
  
  
!----------------------------------------------------------------------
! subroutines from either net.F90 or rest.F90 that are still needed
!   without the GUI
!----------------------------------------------------------------------

!> Shows a message in a GUI dialog (Interacter only).
!! This routine is supposed to be called from the utility modules,
!! such as gridgeom, as a callback.
!!
!! NOTE: this subroutine is dflowfm's implementation of the MHCallBack::messagebox_iface interface.
subroutine unstruc_guimessage(title, msg, level)
    use unstruc_messages
    implicit none
    character(len=*)    :: title !< Title string
    character(len=*)    :: msg   !< Message string
    integer, intent(in) :: level !< Severity level, use values from the MessageHandling module (e.g., LEVEL_ERROR). Currently not used.

    call qnerror(msg, ' ', ' ')

end subroutine unstruc_guimessage

 !>   write an error-message to the log-file and GUI
      SUBROUTINE QNERROR(W1,W2,W3)
      use unstruc_messages
      use m_devices
      use unstruc_model, only:MD_AUTOSTARTSTOP, md_jaAutoStart
      use unstruc_display, only: jaGUI
      implicit none

      character(len=*), intent(in) :: W1, W2, W3

      integer :: infoattribute
      integer :: key
      integer :: nbck
      integer :: nfor
      integer :: nLEVEL
      character(len=600) :: REC, rec2

      COMMON /HELPNOW/   WRDKEY,NLEVEL
      CHARACTER WRDKEY*40

      REC = trim(W1) // ' ' // trim(W2) // ' ' // trim(W2)

      if (len_trim(W2) == 0) then
         rec2 = msgbuf
      else
         rec2 = ' '
      endif
      msgbuf = REC

      ! No user dialog in batchmode runs:
      if ( jaGUI == 1 .and. md_jaAutoStart /= MD_AUTOSTARTSTOP) then
         call warn_flush()
   !     inquire current colors
         NFOR = InfoAttribute(13)
         NBCK = InfoAttribute(14)
         CALL IWinAction   ('FCP')
   !     set error color
         CALL ITEXTCOLOUR('BWHITE','RED')
         CALL IWinOpen   (1,IHS-2,IWS,3)
         CALL IWINOUTSTRINGXY(IWS-15,3,'press any key')
         CALL OKAY(0)
         CALL ITEXTCOLOUR('BLUE','BWHITE')
         CALL IWINOutCentre   (2,trim(REC))
         
         if (len_trim(rec2) > 0) then 
            CALL IWINOutCentre   (3, trim(rec2) )
         endif

      10 CONTINUE
   !     CALL INFLUSH()
         CALL INKEYEVENT(KEY)
         IF (KEY .EQ. 50  .OR. (KEY .GE. 254 .AND. KEY .LE. 259)) THEN
            GOTO 10
         ELSE
            CALL GETKEY2(KEY)
            IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
               WRDKEY = REC
               NLEVEL = 4
               CALL FKEYS(KEY)
               GOTO 10
            ENDIF
         ENDIF

         CALL IWinClose (1)
   !                            reset colors
         CALL ITEXTCOLOURN(NFOR, NBCK)

      else

         call mess (LEVEL_ERROR, trim(msgbuf) )

      endif

      RETURN
      END

!>    plot a statusbar in the GUI
      SUBROUTINE READYY(TEXT,AF)
      use m_devices
      use unstruc_display, only: jaGUI
      implicit none

      CHARACTER TEXT*(*), BALK*400
      double precision :: af

      integer, save :: ih
      integer, save :: ini = 0
      integer, save :: iw
      integer, save :: ixp
      integer, save :: iyp
      integer :: naf

      if ( jaGUI.ne.1 ) return

      IF (INI .EQ. 0) THEN
         INI    = 1
         IXP    = 10
         IYP    = 10
         IW     = IWS - 10 - 10
         IH     = 2
         CALL ITEXTCOLOUR('BWHITE','BLUE')
         CALL IWinAction('FCP')
         CALL IWinOpenTitle(IXP,IYP,IW,IH,TEXT)
         CALL FILLUP(BALK,' ',IW)
         CALL ITEXTCOLOUR('BLACK','BWHITE')
         CALL IWinOutStringXY(2,2,BALK(1:IW))
      ELSE
         NAF = MAX(AF*IW,1d0)
         CALL FILLUP(BALK,'X',NAF)
         CALL IWinOutStringXY(1,2,BALK(1:NAF))
      ENDIF
      IF (AF .EQ. -1) THEN
         CALL IWinClose(1)
         INI = 0
         RETURN
      ENDIF
      RETURN
      END SUBROUTINE READYY


      SUBROUTINE CONFRM(TEXT,JAZEKR)
      use unstruc_display
      implicit none

      CHARACTER TEXT*(*)
      integer :: jazekr

      integer :: imenutwo
      integer :: infoattribute
      integer :: infoinput
      integer :: iopt
      integer :: iw
      integer :: ixp
      integer :: iyp
      integer :: key
      integer :: nbckgr
      integer :: nforgr
      integer :: nlevel
      CHARACTER WRDKEY*40
      COMMON /HELPNOW/ WRDKEY,NLEVEL

      if ( jaGUI.ne.1 ) then
         if ( jazekr.ne.1 ) then
            jazekr=0
         end if
         return
      end if

      IW     = NPOS(3)
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2)
!     IXP    = INFOCURSOR(1)
!     IYP    = INFOCURSOR(2)
      NFORGR = InfoAttribute(13)
      NBCKGR = InfoAttribute(14)
      CALL INPOPUP('ON')
   20 CONTINUE
      CALL ITEXTCOLOUR('BWHITE','RED')
      CALL INHIGHLIGHT('BLUE','BWHITE')
      CALL TIMLIN()
      if (jazekr.eq.1) then ! SPvdP: if jazekr.eq.1, default to yes
         IOPT = IMenuTwo('NO','YES',IXP,IYP,TEXT,1,2)
      else
         IOPT = IMenuTwo('NO','YES',IXP,IYP,TEXT,1,1)
      end if
      CALL TIMLIN()
      KEY = InfoInput(55)
      CALL INFLUSH()
      IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         NLEVEL = 3
         WRDKEY = TEXT
         CALL FKEYS(KEY)
         IF (KEY .EQ. 3) THEN
            CALL INPOPUP('OFF')
            CALL ITEXTCOLOURN(NFORGR,NBCKGR)
            RETURN
         ENDIF
         GOTO 20
      ELSE IF (KEY .EQ. 21 .OR. KEY .EQ. 22) THEN
         IF (IOPT .EQ. 2) THEN
            JAZEKR = 1
         ELSE
            JAZEKR = 0
         ENDIF
      ELSE IF (KEY .EQ. 23) THEN
            JAZEKR = 0
      ELSE
         GOTO 20
      ENDIF
      CALL INPOPUP('OFF')
      CALL ITEXTCOLOURN(NFORGR,NBCKGR)

      RETURN
      END

!


      SUBROUTINE OKAY(JA)
      use m_devices
      use unstruc_display, only: jaGUI
      implicit none
      integer, intent(in) :: ja

      if ( jaGUI.ne.1 ) return

      CALL ISCREENBELL('ON')
      IF (JA .EQ. 1) then
        CALL ISCREENBELL(' ')
      end if
      CALL ISCREENBELL('OFF')
      RETURN

      END


!     plot stencil for higher-order corrections to screen
      subroutine plotklnup(L)
         use m_flowgeom
         implicit none

         integer, intent(in)   :: L  !< flowlink number

         double precision      :: sln1, sln2, sln3
         integer               :: i, ip, k1, k2, kdum

         integer, dimension(3) :: icolor = (/ 31, 221 , 31 /)

         i = 0
         do ip=0,3,3
            i = i+1
            k1   = klnup(1+ip,L)
            sln1 = slnup(1+ip,L)
            k2   = iabs(klnup(2+ip,L))
            sln2 = slnup(2+ip,L)
            sln3 = slnup(3+ip,L)
            if ( k1.ne.0 ) then
               kdum = iabs(k1)
               call cirr(xz(kdum),yz(kdum),icolor(i))
               CALL dHTEXT(sln1,xz(kdum),yz(kdum),0d0)
            else
               call cirr(xu(L),yu(L),icolor(3))
            end if
            if ( k1.gt.0 ) then
               if ( k2.gt.0 ) then
                  call cirr(xz(k2),yz(k2),icolor(i))
                  CALL dHTEXT(sln2,xz(k2),yz(k2),0d0)
                  CALL dHTEXT(sln3,xu(L),yu(L),0d0)
               end if
            end if
         end do

         return
      end subroutine plotklnup
      
      
!>    move probe:
!>      7 8 9
!>      4 5 6
!>      1 2 3
      subroutine moveprobe(idir, kk, xp, yp)
         use m_flowgeom
         use network_data, only: xzw, yzw
         implicit none
         
         integer,          intent(in)    :: idir   !< direction (see keys on keypad)
         integer,          intent(inout) :: kk     !< probed flownode number
         double precision, intent(inout) :: xp, yp !< probed flownode coordinates
         
         double precision                :: csdir, sndir !< direction vector components
         double precision                :: dum
         double precision                :: dmaxinprod
         double precision                :: cs, sn
         
         integer                         :: i, j, jj, k, k2, L, knext
         
         if ( kk.eq.0 .or. idir.eq.5 ) then
            call in_flowcell(xp, yp, KK)
         else
!           determine direction vector
            csdir = mod(idir-1,3) - 1d0
            sndir = int((idir-1)/3) - 1d0
            dum = sqrt(csdir**2+sndir**2)
            csdir = csdir / dum
            sndir = sndir / dum
            
!!           find next flownode
!            knext = 0
!            dmaxinprod = -huge(0d0)
!            
!            do i=1,size(nd(kk)%nod)
!               k = nd(kk)%nod(i)
!               do j=1,cn(k)%lnx
!                  L = iabs(cn(k)%ln(j))
!                  do jj=1,2
!                     k2 = ln(jj,L)
!                     if ( k2.eq.kk ) cycle
!                     
!                     call getdxdy(xzw(kk),yzw(kk),xzw(k2),yzw(k2),cs,sn)
!                     dum = sqrt(cs**2+sn**2)
!                     cs = cs/dum
!                     sn = sn/dum
!                     
!                     dum = csdir*cs + sndir*sn
!                     
!                     if ( dum.gt.0d0 .and. dum.gt.dmaxinprod) then
!                        knext = k2
!                        dmaxinprod = dum
!                     end if
!                     
!                  end do
!               end do
!            end do
            
!           find next flownode
            knext = 0
            dmaxinprod = -huge(0d0)
            do i=1,nd(kk)%lnx
               L = nd(kk)%ln(i)
               if ( L.lt.0 ) then
                  dum = csdir*csu(-L) + sndir*snu(-L)
               else
                  dum = -(csdir*csu(L) + sndir*snu(L))
               end if
               if ( dum.gt.0d0 .and. dum.gt.dmaxinprod) then
                  knext = ln(1,iabs(L))+ln(2,iabs(L))-kk
                  dmaxinprod = dum
               end if
            end do
            
            if ( knext.ne.0 ) then
               kk = knext
               xp = xzw(kk)
               yp = yzw(kk)
            end if
         end if
         
         return
      end subroutine moveprobe
      
      
!>    plot dots    
      subroutine plotdots()
         use m_plotdots
         use unstruc_colors, only: ncolhl
         use unstruc_display
         implicit none
         integer                    :: i
         
         if ( Ndrawdots.ne.2 ) return
         
         do i=1,numdots
            call cirr(xdots(i), ydots(i), ncolhl)
         end do
         
         return
      end subroutine

subroutine teksorsin()      ! teksrc
use m_flowexternalforcings
use unstruc_display
use m_transport, only: isalt, itemp 
use gridoperations
COMMON /DRAWTHIS/  ndraw(50)
integer           :: n, k, kb, kt, n2
character*40      :: tex
double precision  :: znod, temb, temt, xp, yp

if (ndraw(41) <= 1 .or. numsrc == 0) return

call IGrCharJustify('L')
call settextsizefac(1.0d0)

do n = 1,numsrc ! teksorsin
   k = ksrc(1,n)
   if (k .ne. 0) then 
      n2 = 1 ; xp = xsrc(n,n2) ; yp = ysrc(n,n2) 
      if ( inview(xp,yp) ) then 
          if (qsrc(n) > 0) then 
             ncol = 3
          else
             ncol = 221
          endif   
          call cirr(xp, yp, ncol) 
          if (ndraw(41) == 3) then 
             call gtext(' '//trim(srcname(n)), xp, yp , klsrc)
          else if (ndraw(41) == 4) then 
             write(tex,'(f10.3)') -qsrc(n)
             call gtext(trim(tex)//' (m3/s)', xp, yp , klsrc)
          else if (ndraw(41) == 5.and. isalt > 0) then 
             if (qsrc(n) < 0d0) then 
                write(tex,'(f10.3)') ccsrc(isalt,n)
                call gtext(trim(tex)//' (ppt)', xp, yp , klsrc)
             endif   
          else if (ndraw(41) == 6 .and. itemp > 0) then 
             if (qsrc(n) < 0d0) then 
                write(tex,'(f10.3)') ccsrc(itemp,n)
                call gtext(trim(tex)//' (degC)', xp, yp , klsrc)
             endif   
          endif   
      endif   
   endif   
   k = ksrc(4,n)
   if (k .ne. 0) then 
      n2 = nxsrc(n) ; xp = xsrc(n,n2) ; yp = ysrc(n,n2) 
      if ( inview(xp,yp) ) then
          if (qsrc(n) > 0) then 
             ncol = 221
          else
             ncol = 3
          endif   
          call cirr(xp, yp, ncol) 
          if (ndraw(41) == 3) then 
             call gtext(' '//trim(srcname(n)), xp, yp , klsrc)
          else if (ndraw(41) == 4) then 
             write(tex,'(f10.3)') qsrc(n)
             call gtext(trim(tex)//' (m3/s)', xp, yp , klsrc)
          else if (ndraw(41) == 5 .and. isalt > 0) then 
             if (qsrc(n) > 0d0) then 
                write(tex,'(f10.3)') ccsrc(isalt,n)
                call gtext(trim(tex)//' (ppt)', xp, yp , klsrc)
             endif   
          else if (ndraw(41) == 6 .and. itemp > 0) then 
             if (qsrc(n) > 0d0) then 
                write(tex,'(f10.3)') ccsrc(itemp,n)
                call gtext(trim(tex)//' (degC)', xp, yp , klsrc)
             endif   
          endif   
      endif   
   endif   
enddo

end subroutine teksorsin 

