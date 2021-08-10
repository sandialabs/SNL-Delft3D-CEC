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
   !  $Id: fm_erosed.f90 65778 2020-01-14 14:07:42Z mourits $
   !  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/fm_erosed.f90 $

   !>  pointer data
   module m_fm_erosed
   use precision
   use bedcomposition_module
   use morphology_data_module
   use sediment_basics_module
   use m_sediment, only: stmpar, sedtra, stm_included, mtd
   use m_ini_noderel

   implicit none

   real(fp), dimension(:),                pointer :: rhowat
   real(fp), dimension(:,:),              pointer :: seddif
   real(fp), dimension(:,:),              pointer :: sed
   real(fp), dimension(:),                pointer :: blchg         !< Bed level change (> 0 = sedimentation, < 0 = erosion)
   real(fp), dimension(:),                pointer :: dzbdt         !< Bed level change time rate
   real(fp), dimension(:),                pointer :: uau
   real(fp), dimension(:,:),              pointer :: ws

   real(fp), dimension(:)               , pointer :: ucxq_mor
   real(fp), dimension(:)               , pointer :: ucyq_mor
   real(fp), dimension(:)               , pointer :: hs_mor

   real(fp), dimension(:,:)             , pointer :: q_zeta

   !     stmpar
   integer,                               pointer :: lsed
   integer,                               pointer :: lsedtot

   !     sedpar
   integer                              , pointer :: nmudfrac
   real(fp)         , dimension(:)      , pointer :: rhosol
   real(fp)         , dimension(:)      , pointer :: cdryb
   real(fp)         , dimension(:,:,:)  , pointer :: logseddia
   real(fp)         , dimension(:)      , pointer :: logsedsig
   real(fp)         , dimension(:)      , pointer :: sedd10
   real(fp)         , dimension(:)      , pointer :: sedd50
   real(fp)         , dimension(:)      , pointer :: sedd90
   real(fp)         , dimension(:)      , pointer :: sedd50fld
   real(fp)         , dimension(:)      , pointer :: dstar
   real(fp)         , dimension(:)      , pointer :: taucr
   real(fp)         , dimension(:)      , pointer :: tetacr
   real(fp)         , dimension(:)      , pointer :: mudcnt
   real(fp)         , dimension(:)      , pointer :: pmcrit
   integer          , dimension(:)      , pointer :: nseddia
   integer          , dimension(:)      , pointer :: sedtyp
   logical                              , pointer :: anymud
   real(fp)         , dimension(:)      , pointer :: sedtrcfac
   logical                              , pointer :: bsskin
   real(fp)         , dimension(:)      , pointer :: thcmud
   real(fp)         , dimension(:, :)   , pointer :: dss     !  Description and declaration in esm_alloc_real.f90

   ! morpar
   real(fp)                             , pointer :: thresh
   real(fp)                             , pointer :: sus
   real(fp)                             , pointer :: bed
   real(fp)                             , pointer :: susw
   real(fp)                             , pointer :: sedthr
   real(fp)                             , pointer :: bedw
   integer                              , pointer :: i10
   integer                              , pointer :: i15
   integer                              , pointer :: i50
   integer                              , pointer :: i90
   integer                              , pointer :: nxx
   real(fp)         , dimension(:)      , pointer :: xx
   logical                              , pointer :: multi
   real(fp)                             , pointer :: factcr
   real(fp)                             , pointer :: factsd
   integer                              , pointer :: ihidexp
   real(fp)                             , pointer :: asklhe
   real(fp)                             , pointer :: mwwjhe
   real(fp)                             , pointer :: ffthresh
   real(fp)                             , pointer :: morfac
   logical                              , pointer :: varyingmorfac
   real(hp)                             , pointer :: morft
   real(hp)                             , pointer :: hydrt
   real(fp)                             , pointer :: espir
   logical                              , pointer :: epspar
   real(fp)                             , pointer :: camax
   real(fp)                             , pointer :: aksfac
   real(fp)                             , pointer :: rdc
   integer                              , pointer :: iopkcw
   logical                              , pointer :: oldmudfrac
   real(fp)         , dimension(:,:)    , pointer :: sinkf
   real(fp)         , dimension(:,:)    , pointer :: sourf
   integer                              , pointer :: iflufflyr
   real(fp)         , dimension(:,:)    , pointer :: depfac
   real(fp)         , dimension(:,:)    , pointer :: mfluff
   logical                              , pointer :: bedupd
   real(fp)                             , pointer :: tmor
   integer                              , pointer :: itmor
   integer                              , pointer :: islope
   real(fp)                             , pointer :: dzmax
   real(fp)                             , pointer :: hmaxth
   real(fp)                             , pointer :: thetsd
   logical                              , pointer :: eqmbcsand
   logical                              , pointer :: eqmbcmud
   logical                              , pointer :: eulerisoglm

   ! trapar
   integer          , dimension(:)      , pointer :: iform
   real(fp)         , dimension(:,:)    , pointer :: par
   integer                              , pointer :: max_integers
   integer                              , pointer :: max_reals
   integer                              , pointer :: max_strings
   character(256)   , dimension(:)      , pointer :: dll_function
   integer(pntrsize), dimension(:)      , pointer :: dll_handle
   integer          , dimension(:)      , pointer :: dll_integers
   real(hp)         , dimension(:)      , pointer :: dll_reals
   character(256)   , dimension(:)      , pointer :: dll_strings
   character(256)   , dimension(:)      , pointer :: dll_usrfil

   ! sedtra
   real(fp)         , dimension(:, :)   , pointer :: aks
   real(fp)         , dimension(:)      , pointer :: bc_mor_array
   real(fp)         , dimension(:,:)    , pointer :: dbodsd
   real(fp)         , dimension(:)      , pointer :: dcwwlc
   real(fp)         , dimension(:)      , pointer :: dm
   real(fp)         , dimension(:)      , pointer :: dg
   real(fp)         , dimension(:)      , pointer :: dgsd
   real(fp)         , dimension(:,:)    , pointer :: dxx
   real(fp)         , dimension(:)      , pointer :: e_dzdn
   real(fp)         , dimension(:)      , pointer :: e_dzdt
   real(fp)         , dimension(:)      , pointer :: epsclc
   real(fp)         , dimension(:)      , pointer :: epswlc
   real(fp)         , dimension(:,:)    , pointer :: fixfac
   real(fp)         , dimension(:,:)    , pointer :: frac
   integer          , dimension(:)      , pointer :: kfsed
   integer          , dimension(:,:)    , pointer :: kmxsed
   real(fp)         , dimension(:)      , pointer :: mudfrac
   real(fp)         , dimension(:)      , pointer :: sandfrac
   real(fp)         , dimension(:,:)    , pointer :: hidexp
   real(fp)         , dimension(:)      , pointer :: rsdqlc
   real(fp)         , dimension(:,:)    , pointer :: sbcx
   real(fp)         , dimension(:,:)    , pointer :: sbcy
   real(fp)         , dimension(:,:)    , pointer :: e_sbcn
   real(fp)         , dimension(:,:)    , pointer :: e_sbct
   real(fp)         , dimension(:,:)    , pointer :: sbwx
   real(fp)         , dimension(:,:)    , pointer :: sbwy
   real(fp)         , dimension(:,:)    , pointer :: e_sbwn
   real(fp)         , dimension(:,:)    , pointer :: e_sbwt
   real(fp)         , dimension(:,:)    , pointer :: e_sbt
   real(fp)         , dimension(:,:)    , pointer :: e_sbn
   real(fp)         , dimension(:,:)    , pointer :: e_ssn
   real(fp)         , dimension(:,:)    , pointer :: e_sst
   real(fp)         , dimension(:,:)    , pointer :: e_sbtc
   real(fp)         , dimension(:,:)    , pointer :: e_sbnc
   real(fp)         , dimension(:,:)    , pointer :: e_ssnc
   real(fp)         , dimension(:,:)    , pointer :: e_scrn
   real(fp)         , dimension(:,:)    , pointer :: e_scrt
   real(fp)         , dimension(:)      , pointer :: sddflc
   real(fp)         , dimension(:,:)    , pointer :: sswx
   real(fp)         , dimension(:,:)    , pointer :: sswy
   real(fp)         , dimension(:,:)    , pointer :: sscx
   real(fp)         , dimension(:,:)    , pointer :: sscy
   real(fp)         , dimension(:,:)    , pointer :: e_sswn
   real(fp)         , dimension(:,:)    , pointer :: e_sswt
   real(fp)         , dimension(:,:)    , pointer :: sxtot
   real(fp)         , dimension(:,:)    , pointer :: sytot
   real(fp)         , dimension(:,:)    , pointer :: sbxcum
   real(fp)         , dimension(:,:)    , pointer :: sbycum
   real(fp)         , dimension(:,:)    , pointer :: ssxcum
   real(fp)         , dimension(:,:)    , pointer :: ssycum
   real(fp)         , dimension(:,:)    , pointer :: sinkse
   real(fp)         , dimension(:,:)    , pointer :: sourse
   real(fp)         , dimension(:,:)    , pointer :: sour_im
   real(fp)         , dimension(:,:)    , pointer :: srcmax
   real(fp)         , dimension(:,:)    , pointer :: taurat
   real(fp)         , dimension(:)      , pointer :: ust2
   real(fp)         , dimension(:)      , pointer :: umod
   real(fp)         , dimension(:)      , pointer :: uuu
   real(fp)         , dimension(:)      , pointer :: vvv
   real(fp)         , dimension(:)      , pointer :: wslc
   real(fp)         , dimension(:)      , pointer :: zumod
   real(fp),          dimension(:, :),    pointer :: rsedeq
   real(fp)                         ,     pointer :: alfabs
   real(fp)                         ,     pointer :: alfabn
   real(fp)                         ,     pointer :: wetslope
   real(fp)                         ,     pointer :: avaltime
   real(fp)                         ,     pointer :: dryslope
   logical                          ,     pointer :: duneavalan
   real(fp)                         ,     pointer :: hswitch
   real(fp)                         ,     pointer :: dzmaxdune
   real(fp)                         ,     pointer :: ashld
   real(fp)                         ,     pointer :: bshld
   real(fp)                         ,     pointer :: cshld
   real(fp)                         ,     pointer :: dshld
   real(fp)                         ,     pointer :: alfpa
   real(fp)                         ,     pointer :: thcrpa
   logical                              , pointer :: neglectentrainment
   real(fp)          , dimension(:,:)   , pointer :: rca
   real(fp)          , dimension(:,:)   , pointer :: statqnt

   end module m_fm_erosed

   module m_fm_update_crosssections
   implicit none
   contains

   subroutine fm_update_crosssections(blchg)
   use precision
   use m_flowgeom, only: ndxi, kcs, dx, wu, nd, wu_mor, ba_mor, bai_mor, bl, ndx, acl
   use m_oned_functions, only:gridpoint2cross
   use unstruc_channel_flow, only: network, t_node, nt_LinkNode
   use m_CrossSections, only: t_CSType, CS_TABULATED
   use m_flow, only: s1
   use MessageHandling

   real(fp), dimension(:), intent(inout) :: blchg         !< Bed level change (> 0 = sedimentation, < 0 = erosion)

   integer :: i
   integer :: inod
   integer :: iref
   integer :: j
   integer :: l
   integer :: nm
   integer :: nm2
   integer :: nmm
   integer :: c
   integer :: ctype
   integer :: LL
   double precision :: aref
   double precision :: blmin
   double precision :: da
   double precision :: ds
   double precision :: dvol
   double precision :: fac
   double precision :: href
   double precision :: w_active
   type(t_CSType), pointer :: cdef
   type(t_node), pointer :: pnod
   !
   ! upon entry blchg contains the bed level change averaged over the total cell area
   !
   do nm = 1, ndxi
      if (kcs(nm)==1) then ! only for 1D nodes
         do j = 1, gridpoint2cross(nm)%num_cross_sections
            c = gridpoint2cross(nm)%cross(j)
            if (c == -999) cycle
            cdef => network%crs%cross(c)%tabdef
            ctype = cdef%crosstype
            if (gridpoint2cross(nm)%num_cross_sections == 1) then
               nmm = nm
               !
               ! compute the total cell length
               !
               ds = 0d0
               do i = 1,nd(nm)%lnx
                  LL = nd(nm)%ln(i)
                  L = iabs(LL)
                  if (LL < 0) then
                     ds = ds+dx(L)*acl(L)
                  else
                     ds = ds+dx(L)*(1d0 - acl(L))
                  endif
               enddo
            else
               LL = nd(nm)%ln(j)
               L = iabs(LL)
               if (LL < 0) then
                  ds = dx(L)*acl(L)
               else
                  ds = dx(L)*(1d0 - acl(L))
               endif
            endif
            if (ctype == CS_TABULATED) then
               !
               ! determine the reference height href and the cross sectional area  below that level
               !
               iref = cdef%levelscount
               do i = 2, cdef%levelscount - 1
                  if (cdef%flowWidth(i+1) > cdef%plains(1)) then ! or cdef%height(i)>s1(nm)
                     iref = i
                     exit
                  endif
               enddo
               aref = 0d0
               do i = 2, iref
                  aref = aref + (cdef%flowWidth(i) + cdef%flowWidth(i-1))*(cdef%height(i)-cdef%height(i-1))*0.5d0
               enddo
               href = cdef%height(iref)
               w_active = cdef%flowWidth(iref)
               !
               ! use blchg as bed level change over the total cell area (can be partly dry)
               ! to compute the total volume deposited inside the cell
               !
               dvol = blchg(nm)*ds*cdef%plains(1) ! relative volume (at links)
               !
               ! compute the deposited area per unit length, i.e. the area by which the cross section should be adjusted
               !
               da   = dvol/ds
               !
               ! compute the factor by which the cross sectional area changes
               !
               if (cdef%levelscount == 1) then
                  !
                  ! single level: horizontal bed, shift up/down uniformly
                  !
                  cdef%height(1) = href + (da/w_active)
               elseif (da<aref) then
                  !
                  ! raise/lower proportional to the depth relative to reference level
                  !
                  fac = 1d0 - (da/aref)
                  do i = 1, iref-1
                     cdef%height(i) = href - (href-cdef%height(i))*fac
                  enddo
               else
                  !
                  ! fill uniformly above reference level
                  !
                  do i = iref, cdef%levelscount-1
                     da = da - aref
                     aref = (cdef%flowWidth(i+1) + cdef%flowWidth(i))*(cdef%height(i+1)-cdef%height(i))*0.5d0
                     if (da<aref) then
                        exit
                     else
                        iref = iref+1
                        href = cdef%height(iref)
                     endif
                  enddo
                  !
                  ! remove obsolete levels
                  !
                  do i = iref, cdef%levelscount
                     cdef%flowWidth(i-iref+1) = cdef%flowWidth(i)
                     cdef%height(i-iref+1) = cdef%height(i)
                  enddo
                  cdef%levelscount = cdef%levelscount - iref + 1
                  !
                  ! fill proportional to the depth relative to reference level
                  !
                  fac = 1d0 - (da/aref)
                  do i = 1, iref-1
                     cdef%height(i) = href - (href-cdef%height(i))*fac
                  enddo
               endif
               !
               ! set blchg to bed level change for deepest point
               !
               network%crs%cross(c)%surfaceLevel = cdef%height(cdef%levelscount)
               if (gridpoint2cross(nm)%num_cross_sections == 1) then
                  blchg(nm) = cdef%height(1) - network%crs%cross(c)%bedLevel
               endif
               network%crs%cross(c)%bedLevel     = cdef%height(1) !TODO: check if we need to include network%crs%cross(c)%shift
               network%crs%cross(c)%charheight   = network%crs%cross(c)%surfaceLevel - network%crs%cross(c)%bedLevel
            else
               write(msgbuf,'(a,i5)') 'Bed level updating has not yet implemented for cross section type ',ctype
               call err_flush()
            endif
         enddo
      endif
   enddo
   !
   ! set blchg to bed level change for deepest point of incoming branches
   !
   ! loop over connection nodes
   do inod = 1, network%nds%Count
      pnod => network%nds%node(inod)
      if (pnod%nodeType == nt_LinkNode) then  ! connection node
         nm = pnod%gridnumber
         blmin = 999999d0
         do j = 1, gridpoint2cross(nm)%num_cross_sections
            c = gridpoint2cross(nm)%cross(j)
            if (c == -999) cycle
            cdef => network%crs%cross(c)%tabdef
            !LL = nd(nm)%ln(j)
            !L = iabs(LL)
            !if (LL < 0) then
            !   ds = dx(L)*acl(L)
            !else
            !   ds = dx(L)*(1d0 - acl(L))
            !endif
            blmin = min(blmin, network%crs%cross(c)%bedLevel)
         enddo
         blchg(nm) = blmin - bl(nm)
      endif
   enddo
   !
   ! upon exit blchg contains the bed level change for deepest point
   !
   end subroutine fm_update_crosssections


   subroutine fm_update_mor_width_area()
   use m_flowgeom, only: lnx, lnx1d, lnxi, lnx1Db, wu, wu_mor, LBND1D, bai, ba_mor, bai_mor, ndx, dx, ln, acl, ndx2D, ndx1Db
   use m_cell_geometry, only: ba
   use unstruc_channel_flow, only: network
   use m_CrossSections, only: t_CSType, CS_TABULATED

   type(t_CSType), pointer :: cdef1, cdef2
   integer :: icrs1, icrs2
   integer :: L, LL, k, k1, k2
   double precision :: factor

   ! Set all morphologically active widths to general flow width.
   do L = 1, lnx
      wu_mor(L) = wu(L)
   enddo

   ! Set all morphologically active areas to general flow area and similar for the inverse
   do k = 1, ndx
      ba_mor(k) = ba(k)
      bai_mor(k) = bai(k)
   enddo

   if (network%loaded) then
      ! Replace morphologically active 1d widths by main channel width from cross-section.
      ! This could also be chosen as the minimum of the main channel width and the
      ! flow width (this is apparently how it was implemented in Sobek-RE).
      do L = 1, lnx1d
         factor = network%adm%line2cross(L)%f
         icrs1 = network%adm%line2cross(L)%c1
         icrs2 = network%adm%line2cross(L)%c2
         cdef1 => network%crs%cross(icrs1)%tabdef
         cdef2 => network%crs%cross(icrs2)%tabdef
         if (cdef1%crosstype == CS_TABULATED .and. cdef2%crosstype == CS_TABULATED) then
            wu_mor(L) = (1.0d0 - factor)*cdef1%plains(1) + factor*cdef2%plains(1)
         else
            wu_mor(L) = wu(L)
         endif
      enddo

      ! Overwrite boundary link morphologically active widths by morphologically active width on inside
      do L = lnxi+1, lnx1Db
         LL = LBND1D(L)
         wu_mor(L) = wu_mor(LL)
      enddo

      ! Compute morphologically active areas
      ba_mor(ndx2D+1:ndx1Db) = 0d0

      do L = 1, lnx1d
         k1 = ln(1,L)
         k2 = ln(2,L)
         ba_mor(k1) = ba_mor(k1) + dx(L)*wu_mor(L)*acl(L)
         ba_mor(k2) = ba_mor(k2) + dx(L)*wu_mor(L)*(1d0 - acl(L))
      enddo

      ! Compute morphologically active areas at boundary links
      do L = lnxi+1, lnx1Db
         k1 = ln(1,L)
         k2 = ln(2,L)
         ba_mor(k1) = dx(L)*wu_mor(L) ! area point outside
         ba_mor(k2) = ba_mor(k2) + dx(L)*wu_mor(L)*(1d0 - acl(L)) ! area point inside (added to loop above)
      enddo

      ! Compute inverse of morphologically active areas
      do k = ndx2D+1, ndx1Db
         bai_mor(k) = 1d0/ba_mor(k)
      enddo

   endif

   end subroutine fm_update_mor_width_area

   end module m_fm_update_crosssections
   !
   ! ========================================================================================
   !

   subroutine inipointers_erosed()
   use m_fm_erosed
   use m_flowgeom, only: ndx, lnx
   implicit none
   integer :: ierr

   if (.not.stm_included) return

   ! mtd: Pointer to dummies to fill later
   dzbdt               => mtd%dzbdt
   rhowat              => mtd%rhowat
   seddif              => mtd%seddif
   blchg               => mtd%blchg
   sed                 => mtd%sed
   ws                  => mtd%ws
   uau                 => mtd%uau


   ! stmpar
   lsed                => stmpar%lsedsus
   lsedtot             => stmpar%lsedtot
   ! sedpar
   nmudfrac            => stmpar%sedpar%nmudfrac
   rhosol              => stmpar%sedpar%rhosol
   cdryb               => stmpar%sedpar%cdryb
   logseddia           => stmpar%sedpar%logseddia
   logsedsig           => stmpar%sedpar%logsedsig
   sedd10              => stmpar%sedpar%sedd10
   sedd50              => stmpar%sedpar%sedd50
   sedd90              => stmpar%sedpar%sedd90
   sedd50fld           => stmpar%sedpar%sedd50fld
   dstar               => stmpar%sedpar%dstar
   taucr               => stmpar%sedpar%taucr
   tetacr              => stmpar%sedpar%tetacr
   mudcnt              => stmpar%sedpar%mudcnt
   pmcrit              => stmpar%sedpar%pmcrit
   nseddia             => stmpar%sedpar%nseddia
   sedtyp              => stmpar%sedpar%sedtyp
   anymud              => stmpar%sedpar%anymud
   sedtrcfac           => stmpar%sedpar%sedtrcfac
   bsskin              => stmpar%sedpar%bsskin
   thcmud              => stmpar%sedpar%thcmud
   dss                 => stmpar%sedpar%dss
   ! morpar
   thresh              => stmpar%morpar%thresh
   sus                 => stmpar%morpar%sus
   bed                 => stmpar%morpar%bed
   susw                => stmpar%morpar%susw
   sedthr              => stmpar%morpar%sedthr
   bedw                => stmpar%morpar%bedw
   i10                 => stmpar%morpar%i10
   i15                 => stmpar%morpar%i15
   i50                 => stmpar%morpar%i50
   i90                 => stmpar%morpar%i90
   nxx                 => stmpar%morpar%nxx
   xx                  => stmpar%morpar%xx
   multi               => stmpar%morpar%multi
   eqmbcsand           => stmpar%morpar%eqmbcsand
   eqmbcmud            => stmpar%morpar%eqmbcmud
   factcr              => stmpar%morpar%factcr
   factsd              => stmpar%morpar%factsd
   ihidexp             => stmpar%morpar%ihidexp
   asklhe              => stmpar%morpar%asklhe
   mwwjhe              => stmpar%morpar%mwwjhe
   ffthresh            => stmpar%morpar%thresh
   morfac              => stmpar%morpar%morfac
   varyingmorfac       => stmpar%morpar%varyingmorfac
   morft               => stmpar%morpar%morft
   hydrt               => stmpar%morpar%hydrt
   espir               => stmpar%morpar%espir
   epspar              => stmpar%morpar%epspar
   camax               => stmpar%morpar%camax
   aksfac              => stmpar%morpar%aksfac
   rdc                 => stmpar%morpar%rdc
   iopkcw              => stmpar%morpar%iopkcw
   oldmudfrac          => stmpar%morpar%oldmudfrac
   sinkf               => stmpar%morpar%flufflyr%sinkf
   sourf               => stmpar%morpar%flufflyr%sourf
   iflufflyr           => stmpar%morpar%flufflyr%iflufflyr
   depfac              => stmpar%morpar%flufflyr%depfac
   mfluff              => stmpar%morpar%flufflyr%mfluff
   alfabs              => stmpar%morpar%alfabs
   alfabn              => stmpar%morpar%alfabn
   wetslope            => stmpar%morpar%wetslope
   avaltime            => stmpar%morpar%avaltime
   duneavalan          => stmpar%morpar%duneavalan
   dryslope            => stmpar%morpar%dryslope
   hswitch             => stmpar%morpar%hswitch
   dzmaxdune           => stmpar%morpar%dzmaxdune
   ashld               => stmpar%morpar%ashld
   bshld               => stmpar%morpar%bshld
   cshld               => stmpar%morpar%cshld
   dshld               => stmpar%morpar%dshld
   alfpa               => stmpar%morpar%alfpa
   thcrpa              => stmpar%morpar%thcrpa
   islope              => stmpar%morpar%islope
   tmor                => stmpar%morpar%tmor
   itmor               => stmpar%morpar%itmor
   bedupd              => stmpar%morpar%bedupd
   neglectentrainment  => stmpar%morpar%neglectentrainment
   dzmax               => stmpar%morpar%dzmax
   hmaxth              => stmpar%morpar%hmaxth
   thetsd              => stmpar%morpar%thetsd
   eulerisoglm         => stmpar%morpar%eulerisoglm

   ! trapar
   iform               => stmpar%trapar%iform
   par                 => stmpar%trapar%par
   max_integers        => stmpar%trapar%max_integers
   max_reals           => stmpar%trapar%max_reals
   max_strings         => stmpar%trapar%max_strings
   dll_function        => stmpar%trapar%dll_function
   dll_handle          => stmpar%trapar%dll_handle
   dll_integers        => stmpar%trapar%dll_integers
   dll_reals           => stmpar%trapar%dll_reals
   dll_strings         => stmpar%trapar%dll_strings
   dll_usrfil          => stmpar%trapar%dll_usrfil

   ! sedtra
   aks                 => sedtra%aks
   bc_mor_array        => sedtra%bc_mor_array
   dbodsd              => sedtra%dbodsd
   dcwwlc              => sedtra%dcwwlc
   dm                  => sedtra%dm
   dg                  => sedtra%dg
   dgsd                => sedtra%dgsd
   dxx                 => sedtra%dxx
   e_dzdn              => sedtra%e_dzdn
   e_dzdt              => sedtra%e_dzdt
   epsclc              => sedtra%epsclc
   epswlc              => sedtra%epswlc
   fixfac              => sedtra%fixfac
   frac                => sedtra%frac
   kfsed               => sedtra%kfsed
   kmxsed              => sedtra%kmxsed
   mudfrac             => sedtra%mudfrac
   sandfrac            => sedtra%sandfrac
   hidexp              => sedtra%hidexp
   rsdqlc              => sedtra%rsdqlc
   rsedeq              => sedtra%rsedeq
   sbcx                => sedtra%sbcx
   sbcy                => sedtra%sbcy
   e_sbcn              => sedtra%e_sbcn
   e_sbct              => sedtra%e_sbct
   e_sbn               => sedtra%e_sbn
   e_sbt               => sedtra%e_sbt
   e_ssn               => sedtra%e_ssn
   e_sst               => sedtra%e_sst
   e_sbnc              => sedtra%e_sbnc
   e_sbtc              => sedtra%e_sbtc
   e_ssnc              => sedtra%e_ssnc
   e_scrn              => sedtra%e_scrn
   e_scrt              => sedtra%e_scrt
   sbwx                => sedtra%sbwx
   sbwy                => sedtra%sbwy
   sscx                => sedtra%sscx
   sscy                => sedtra%sscy
   e_sbwn              => sedtra%e_sbwn
   e_sbwt              => sedtra%e_sbwt
   sddflc              => sedtra%sddflc
   sswx                => sedtra%sswx
   sswy                => sedtra%sswy
   e_sswn              => sedtra%e_sswn        ! add correction part later on e_scrn, e_scrt
   e_sswt              => sedtra%e_sswt
   sxtot               => sedtra%sxtot
   sytot               => sedtra%sytot
   sbxcum              => sedtra%sbxcum
   sbycum              => sedtra%sbycum
   ssxcum              => sedtra%ssxcum
   ssycum              => sedtra%ssycum
   sinkse              => sedtra%sinkse
   sourse              => sedtra%sourse
   sour_im             => sedtra%sour_im
   srcmax              => sedtra%srcmax
   taurat              => sedtra%taurat
   ust2                => sedtra%ust2
   umod                => sedtra%umod
   uuu                 => sedtra%uuu
   vvv                 => sedtra%vvv
   wslc                => sedtra%wslc
   zumod               => sedtra%zumod
   rca                 => sedtra%rca
   statqnt             => sedtra%statqnt

   allocate(ucxq_mor(1:ndx), ucyq_mor(1:ndx), hs_mor(1:ndx), stat=ierr)   ! JRE TODO
   ucxq_mor = 0d0; ucyq_mor = 0d0; hs_mor = 0d0
   allocate(q_zeta(2,lnx), stat=ierr)
   q_zeta = 0d0

   end subroutine inipointers_erosed


   subroutine fm_erosed()
   !!--description-----------------------------------------------------------------
   !!
   !!    Function: Computes sediment fluxes at the bed using
   !!              the Partheniades-Krone formulations.
   !!              Arrays SOURSE and SINKSE are filled and added
   !!              to arrays SOUR and SINK
   !!              Computes bed load transport for sand sediment
   !!              Arrays SBUU and SBVV are filled.
   !!              Computes vertical sediment diffusion coefficient
   !!              Array SEDDIF is filled
   !!              Includes wave asymmetry effects on sand bed-load
   !!              transport
   !!              Bed slope effects computed at the U and V velocity
   !!              points
   !! Method used: Attention: pointer ll for 'standard' FLOW
   !!              arrays is shifted with lstart
   !!
   !!
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------
   use precision
   use mathconsts, only: pi
   use bedcomposition_module
   use morphology_data_module
   use sediment_basics_module
   use m_physcoef, only: ag, vonkar, sag, ee, backgroundsalinity, backgroundwatertemperature,dicoww, rhomean
   use m_sediment, only: stmpar, sedtra, stm_included, mtd, vismol, jatranspvel, sbcx_raw,sbcy_raw,sswx_raw,sswy_raw,sbwx_raw,sbwy_raw
   use m_flowgeom, only: ndxi, bl, kfs, lnxi, lnx, ln, dxi, ndx, csu, snu, wcx1, wcx2, wcy1, wcy2, acl, nd, csu, snu, wcl, xz, yz, xu, yu, wu, wu_mor
   use m_flow, only: s0, s1, u1, u0, v, ucx, ucy, kbot, ktop, kmx, kmxn, plotlin, sa1, tem1, zws, hs, ucxq, ucyq, layertype, &
      iturbulencemodel, z0urou, frcu, ifrcutp, hu, spirint, spiratx, spiraty, u_to_umain, q1, frcu_mor
   use m_flowtimes, only: julrefdat, dts, time1
   use unstruc_files, only: mdia
   use unstruc_channel_flow, only: network, t_branch, t_node, nt_LinkNode
   use message_module, only: write_error
   use MessageHandling, only: LEVEL_INFO, LEVEL_FATAL, mess, setmessage
   use m_transport, only: ised1, numconst, constituents
   use dfparall
   use m_alloc
   use m_missing
   use m_physcoef, only: frcuni, ifrctypuni
   use m_turbulence, only: vicwws, turkinepsws
   use m_flowparameters, only: jasal, jatem, jawave, epshs, jasecflow, eps10, jasourcesink
   use m_fm_erosed
   use m_bedform
   use m_xbeach_data
   use m_waves
   use m_xbeach_paramsconst
   use m_tables, only: interpolate
   use m_partitioninfo
   use compbsskin_module, only: compbsskin
   !
   implicit none
   !
   real(fp)                                       :: eps = 1.0e-6_fp
   logical                                        :: scour = .false.
   logical                                        :: ubot_from_com != .true. !! promoted approach, so only option in FM
   logical                                        :: flmd2l = .false.
   logical                                        :: wave

   integer                              , pointer :: iunderlyr
   real(prec)       , dimension(:,:)    , pointer :: bodsed
   type(t_nodefraction)                 , pointer :: pFrac
   type(t_noderelation)                 , pointer :: pNodRel
   type(t_branch)                       , pointer :: pbr
   type(t_node)                         , pointer :: pnod

   !
   ! Local parameters
   !
   integer, parameter :: kmax2d = 20
   !
   ! Global variables
   !
   integer                                        :: ltur
   !
   !
   ! Local variables
   !
   integer                       :: i
   integer                       :: ibr
   integer                       :: iFrac
   integer                       :: iNodeRel
   integer                       :: inod
   integer                       :: istat
   integer                       :: ised
   integer                       :: ierror
   integer                       :: j
   integer                       :: k
   integer                       :: k2d
   integer                       :: kbed
   integer                       :: kmaxsd
   integer                       :: l
   integer                       :: ll
   integer                       :: lstart
   integer                       :: nm
   logical                       :: error
   integer                       :: klc
   integer                       :: kmaxlc
   integer                       :: k1, k2, k3, k4
   logical                       :: suspfrac  ! suspended component sedtyp(l)/=SEDTYP_NONCOHESIVE_TOTALLOAD
   real(fp)                      :: sigdif    ! Prandtl-Schmidt; to do: change to array
   real(fp)                      :: aks_ss3d
   real(fp)                      :: caks
   real(fp)                      :: caks_ss3d
   real(fp)                      :: chezy
   real(fp)                      :: conc2d
   real(fp)                      :: delr
   real(fp)                      :: di50
   real(fp)                      :: difbot
   real(fp)                      :: drho
   real(fp)                      :: dtmor
   real(fp)                      :: fracf
   real(fp)                      :: maxslope
   real(fp)                      :: grkg
   real(fp)                      :: grm2
   real(fp)                      :: grlyrs
   real(fp)                      :: h0
   real(fp)                      :: h1
   real(fp)                      :: rc
   real(fp)                      :: mfltot
   real(fp)                      :: salinity
   real(fp)                      :: sinkfluff
   real(fp)                      :: sinktot
   real(fp)                      :: sourfluff
   real(fp)                      :: spirintnm   ! local variable for spiral flow intensity
   real(fp)                      :: taks
   real(fp)                      :: taks0
   real(fp)                      :: tauadd
   real(fp)                      :: tauc
   real(fp)                      :: tdss      ! temporary variable for dss
   real(fp)                      :: temperature
   real(fp), dimension(max(kmx,1))      :: thicklc
   real(fp), dimension(max(kmx,1))      :: siglc
   real(fp)                      :: thick0
   real(fp)                      :: thick1
   real(fp)                      :: trsedeq   ! temporary variable for rsedeq
   real(fp)                      :: tsd
   real(fp)                      :: tsigmol   ! temporary variable for sigmol
   real(fp)                      :: twsk
   real(fp)                      :: ulocal
   real(fp)                      :: ubed
   real(fp)                      :: umean
   real(fp)                      :: ustarc
   real(fp)                      :: utot
   real(fp)                      :: vbed
   real(fp)                      :: velb
   real(fp)                      :: velm
   real(fp)                      :: vlocal
   real(fp)                      :: vmean
   real(fp)                      :: z0cur
   real(fp)                      :: z0rou
   real(fp)                      :: zvelb
   real(fp)                      :: tsalmax
   real(fp)                      :: poros
   real(fp)                      :: wstau                 ! dummy for erosilt
   real(fp), dimension(:), allocatable :: Evel            ! erosion velocity [m/s]
   real(fp), dimension(0:kmax2d) :: dcww2d
   real(fp), dimension(0:kmax2d) :: sddf2d
   real(fp), dimension(0:kmax2d) :: ws2d
   real(fp), dimension(kmax2d)   :: rsdq2d
   real(fp), dimension(kmax2d), save :: sig2d = &
      (/ -0.0874, -0.2472, -0.3797, -0.4897, -0.5809, -0.6565, -0.7193, &
      & -0.7713, -0.8145, -0.8503, -0.8800, -0.9046, -0.9250, -0.9419, -0.9560,&
      & -0.9676, -0.9773, -0.9854, -0.9920, -0.9975 /)

   real(fp), dimension(kmax2d), save :: thck2d = &
      (/ 0.1747, 0.1449, 0.1202, 0.0997, 0.0827, 0.0686, 0.0569, 0.0472, &
      & 0.0391, 0.0325, 0.0269, 0.0223, 0.0185, 0.0154, 0.0127, 0.0106, 0.0088,&
      & 0.0073, 0.0060, 0.0050 /)

   real(fp), dimension(max(kmx,1))      :: concin3d
   real(fp), dimension(kmax2d)   :: concin2d
   character(256)                :: errmsg
   character(256)                :: msg
   double precision              :: cz_dum, cc, maxdepfrac
   double precision              :: ML, hloc, twothird
   double precision              :: dcfin, dcf
   double precision              :: ubot
   integer                       :: idummy = 0
   integer                       :: ierr, kk, kkk, Lf, kmxvel, kb, kt, snL, csL
   integer                       :: Ldir
   double precision, allocatable :: dzdx(:), dzdy(:), u1ori(:), u0ori(:), vori(:), localpar(:)
   double precision, allocatable :: z0rouk(:), z0curk(:),taub(:), deltas(:), ua(:), va(:)
   double precision              :: dzdn, dzds
   integer                       :: mout
   double precision              :: z0u, czu
   double precision              :: facCheck
   integer                       :: nrd_idx
   double precision              :: expQ
   double precision              :: expW
   double precision              :: facQ
   double precision              :: facW
   double precision              :: qb1d, wb1d, sb1d
   double precision              :: sbrratio, qbrratio, Qbr1, Qbr2
   !
   real(fp), dimension(:), allocatable :: qb_out          !< sum of outgoing discharge at 1d node
   real(fp), dimension(:), allocatable :: width_out       !< sum of outgoing main channel widths
   real(fp), dimension(:,:), allocatable :: sb_in         !< sum of incoming sediment transport at 1d node
   integer, dimension(:,:,:), allocatable :: sb_dir       !< direction of transport at node (nnod, lsedtot, nbr) (-1 = incoming or no transport, +1 = outgoing)
   integer, dimension(:), allocatable :: branInIDLn       !< ID of Incoming Branch (If there is only one) (nnod)
   !
   !! executable statements -------------------------------------------------------
   !
   !   exit the routine immediately if sediment transport (and morphology) is NOT included in the simulation
   !
   error = .false.
   if (.not.stm_included) return
   ubot_from_com = jauorbfromswan>0
   !
   ! Allocate memory
   allocate(dzdx(1:ndx), dzdy(1:ndx), stat=istat)
   if (istat == 0) allocate(localpar (stmpar%trapar%npar), stat = istat)
   if (istat == 0) allocate(ua(1:ndx), va(1:ndx), stat=istat)
   if (istat == 0) allocate(z0rouk(1:ndx), z0curk(1:ndx), taub(1:ndx), deltas(1:ndx), stat=istat)
   if (istat == 0) allocate(qb_out(network%nds%Count), stat = istat)
   if (istat == 0) allocate(width_out(network%nds%Count), stat = istat)
   if (istat == 0) allocate(sb_in(network%nds%Count, lsedtot), stat = istat)
   if (istat == 0) allocate(sb_dir(network%nds%Count, lsedtot, network%nds%maxnumberofconnections), stat = istat)
   if (istat == 0) allocate(branInIDLn(network%nds%Count), stat = istat)

   localpar = 0d0; ua = 0d0; va = 0d0; z0rouk = 0d0; z0curk=0d0
   qb_out = 0d0; width_out = 0d0; sb_in = 0d0; sb_dir = -1
   BranInIDLn = 0

   if ((istat == 0) .and. (.not. allocated(u1ori))) allocate(u1ori(1:lnx), u0ori(1:lnx), vori(1:lnx), stat=ierr)

   if (istat /= 0) then
      error = .true.
      write(errmsg,'(a)') 'fm_erosed::error allocating memory.'
      call write_error(errmsg, unit=mdia)
   endif
   !
   wave = jawave>0
   !
   ! Mass conservation; s1 is updated before entering fm_erosed
   hs = s1 - bl
   !
   if (varyingmorfac) then
      call updmorfac(stmpar%morpar, time1/3600.0_fp, julrefdat)
   endif
   !
   ! Back up velocities before contributions ua, stokes drift
   !
   u1ori = u1; u0ori = u0; vori = v
   !
   ! Reset some arrays before next iteration
   spirintnm = 0.0_fp
   ust2 = 0.0_fp
   !
   ! Use Eulerian velocities if jatranspvel > 0
   !
   u1 = u1*u_to_umain
   u0 = u0*u_to_umain

   if (jatranspvel > 0 .and. jawave > 0) then
      !
      u1 = u1 - ustokes
      u0 = u0 - ustokes
      v  = v  - vstokes
      !
   end if
   !   Calculate cell centre velocities ucxq, ucyq
   call setucxucyucxuucyu()
   call setucxqucyq()
   !
   if (.not. (jawave==4 .or. jawave==3)) then
      ktb=0d0     ! no roller turbulence
   else
      do k=1, ndx
         call rollerturbulence(k)  ! sets ktb values
      end do
   end if
   !
   ! Determine total thickness of the mud layers
   ! to be used in computation of skin friction
   ! (Soulsby&Clarke 2004, EstProc report TR137)
   ! will be used in compbsskin.f90
   !
   if (bsskin) then
      call detthcmud(stmpar%morlyr, thcmud)
   endif
   !
   ! Initialisation:
   ! reset sediment sources and sinks
   !     set default kmxsed layer
   !     set kfsed
   !
   lstart = ised1 - 1
   !
   ! Reset Sourse and Sinkse arrays for all (l,nm)
   !
   do k = 1, ndx
      call getkbotktop(k, kb, kt)
      kmxsed(k,:)  = kb
   end do
   sinkse  = 0.0_fp
   sourse  = 0.0_fp
   sour_im = 0.0_fp
   ! source and sink terms fluff layer
   if (iflufflyr>0) then
      sinkf = 0.0_fp
      sourf = 0.0_fp
   endif
   !
   ! Reset Sediment diffusion arrays for (l,nmk)
   !
   seddif  = 0.0_fp
   rca     = 0.0_fp
   !
   ! Reset Bed Shear Ratio for all nm and l = 1:lsedtot
   !
   taurat = 0.0_fp
   !
   ! Set zero bedload transport for all nm and l = 1:lsedtot
   !
   sbcx   = 0.0_fp
   sbcy   = 0.0_fp
   e_sbcn = 0.0_fp
   e_sbct = 0.0_fp
   sbwx   = 0.0_fp
   sbwy   = 0.0_fp
   e_sbwn = 0.0_fp
   e_sbwt = 0.0_fp
   sswx   = 0.0_fp
   sswy   = 0.0_fp
   e_sswn = 0.0_fp
   e_sswt = 0.0_fp
   sxtot  = 0.0_fp
   sytot  = 0.0_fp
   rsedeq = 0.0_fp

   ! Set ltur
   ltur = 0
   if (kmx>0) then
      select case (iturbulencemodel)
         case (0)
            ltur = 0
         case (1)
            ltur = 0
         case (2)
            ltur = 0
         case (3)
            ltur = 2
         case (4)
            ltur = 2
      end select
   end if

   do nm = 1, ndx
      if ((s1(nm) - bl(nm)) > sedthr) then ! *kfs(nm): always compute sed, also in ghost nodes
         kfsed(nm) = 1
      else
         kfsed(nm) = 0
      endif
   enddo
   !
   ! Determine fractions of all sediments the top layer and
   ! compute the mud fraction.
   !
   if (lsedtot > 1) then
      call getfrac(stmpar%morlyr,frac      ,anymud    ,mudcnt    , &
         & mudfrac      ,1         ,ndx)
   endif

   ! 3D:
   ! Calculate cell centre velocity components and magnitude
   ! based on velocity in the bottom computational layer
   ! Note: uses downwind velocity at any internal point,
   ! uses internal velocity at any open boundary, uses
   ! half of internal velocity in direction of any
   ! closed boundary or dry point.
   !
   do k = 1,ndx                            ! This interpolation is done by considering constant waterdepth per each flow-cell
      h1 = s1(k) - bl(k)                   ! To ensure to get the same results from interpolation based on constant frcu and ifrcutp in the cell centre
                                           ! with considering hs
      z0curk(k) = eps10                    ! safety if nd(k)%lnx==0. Happens sometimes in case of thin dams
      do LL = 1,nd(k)%lnx
         Lf = nd(k)%ln(LL)
         L = abs( Lf )
         if (frcu(L)>0) then
            call getczz0(h1, frcu_mor(L), ifrcutp(L), czu, z0u)
         else
            call getczz0(h1, frcuni, ifrctypuni, czu, z0u)
         end if
         if( Lf < 0 ) then
            z0curk(k) = z0curk(k)+wcl(1,L)*z0u
         else
            z0curk(k) = z0curk(k)+wcl(2,L)*z0u
         endif
      enddo
   enddo
   !
   if (jawave>0) then                          
      z0rouk = 0d0; taub = 0d0
      do L=1,lnx
         k1=ln(1,L); k2=ln(2,L)
         z0rouk(k1) = z0rouk(k1)+wcl(1,L)*z0urou(L)
         z0rouk(k2) = z0rouk(k2)+wcl(2,L)*z0urou(L)
         taub(k1)   = taub(k1)+wcl(1,L)*taubxu(L)
         taub(k2)   = taub(k2)+wcl(2,L)*taubxu(L)
      end do
   endif
   !
   if (kmx > 0) then            ! 3D
      deltas = 0d0
      do L=1,lnx
         k1=ln(1,L); k2=ln(2,L)
         deltas(k1) =  deltas(k1) + wcl(1,L)*wblt(L)
         deltas(k2) =  deltas(k2) + wcl(2,L)*wblt(L)
      end do
      maxdepfrac = 0.05                       !        cases where you want 2D velocity above the wbl
      cc = 0d0

      do kk = 1, ndx
         call getkbotktop(kk,kb,kt)
         do k = kb, kt
            cc  = 0.5d0*(zws(k-1)+zws(k))         ! cell centre position in vertical layer admin, using depth convention
            kmxvel = k
            if (cc>=-maxdepfrac*hs(kk) .or. cc>=(bl(kk)+deltas(kk))) then
               exit
            endif
         enddo

         uuu(kk)   = ucxq(kmxvel)                  ! discharge based cell centre velocities
         vvv(kk)   = ucyq(kmxvel)
         umod(kk)  = sqrt(uuu(kk)*uuu(kk) + vvv(kk)*vvv(kk))
         zumod(kk) = cc-bl(kk)
      end do

      ! If secondary flow, then we consider the bed shear stress magnitude as computed in 3D,
      ! but the direction as computed by the 1DV solution of the secondary flow. Here the
      ! near bed vector is projected back to the original depth averaged direction of the flow.
      if (jasecflow > 0) then
         do kk = 1, ndx
            uuu(kk) = spiratx(kk)*umod(kk)
            vvv(kk) = spiraty(kk)*umod(kk)
         enddo
      end if

   else
      do kk = 1, ndx
         uuu(kk)   = ucxq_mor(kk)
         vvv(kk)   = ucyq_mor(kk)
         umod(kk)  = sqrt(uuu(kk)*uuu(kk) + vvv(kk)*vvv(kk))
         zumod(kk) = hs_mor(kk)/ee
      enddo
   end if
   !
   ! set velocities to zero if not active point for transport
   !
   do nm = 1, ndx
      if (kfsed(nm) == 0) then
         uuu  (nm) = 0.0_fp
         vvv  (nm) = 0.0_fp
         umod (nm) = 0.0_fp
         zumod(nm) = 0.0_fp
         cycle
      endif
   end do
   !
   ! Get the reduction factor if thickness of sediment at bed is less than
   ! user specified threshold. Also get maximum erosion source SRCMAX
   ! (used for cohesive sediments).
   !
   dtmor = dts * morfac
   !
   call getfixfac(stmpar%morlyr, 1        , ndx     , lsedtot, &                  ! Update underlayer bookkeeping system for erosion/sedimentation
   & ndx          , fixfac   , ffthresh  )
   !
   ! Set fixfac to 1.0 for tracer sediments and adjust frac
   !
   istat = bedcomp_getpointer_integer(stmpar%morlyr, 'IUnderLyr', iunderlyr)        ! iunderlayer=1: mixed, 2: layer bookkeeping
   if (ffthresh>0.0_hp .or. iunderlyr/=1) then
      srcmax = 1.0e+10_fp
   elseif (iunderlyr==1) then
      istat = bedcomp_getpointer_realprec(stmpar%morlyr,'bodsed',bodsed)
      do l = 1, lsed
         if (ffthresh<1.0e-10_fp) then
            !
            ! Compute SRCMAX (only used for cohesive sediments)
            !
            do nm = 1, ndx
               !
               ! If user-specified THRESH is <= 0.0, the erosion flux is effectively not limited by FIXFAC since ffthresh is 1e-10
               ! but by the amount of sediment that is available
               !
               !srcmax(nm, l) = bodsed(l, nm)*cdryb(l)/dtmor
               srcmax(nm, l) = bodsed(l, nm)/dtmor
            enddo
         endif
         !
         if (sedtrcfac(l)>0.0_fp) then
            grkg = 1.0_fp / (rhosol(l)*pi*sedd50(l)**3/6.0_fp) ! Number of grains per kg
            grm2 = 0.5_fp / (pi*sedd50(l)**2) ! Number of grains per m^2 -- Not quite correct: maximum area of grain is pi*r^2 not pi*d^2, using porosity factor of 0.5
            do nm = 1, ndx
               fixfac(nm, l) = 1.0_fp
               grlyrs = bodsed(l, nm) * grkg / grm2 ! Number of grain layers
               frac(nm, l) = min(max(0.0_fp, grlyrs), 1.0_fp)*sedtrcfac(l)
            enddo
         endif
      enddo
   endif
   !
   ! in case of multiple (non-mud) fractions, the following quantities
   ! --- that are initialized in INISED --- may be time-dependent and
   ! they must be updated here or after updating the bed levels in
   ! BOTT3D. Since we do it here, these quantities will lag a half time
   ! step behind on the output files. If these statements are moved to
   ! BOTT3D, the GETFRAC call above must be shifted too.
   !
   if (lsedtot-nmudfrac > 1) then    ! for all non-cohesive suspended fractions
      !
      ! calculate arithmetic mean sediment diameter Dm
      ! calculate geometric mean sediment diameter Dg
      ! calculate percentiles Dxx
      !
      call compdiam(frac      ,sedd50    ,sedd50    ,sedtyp    ,lsedtot   , &
         & logsedsig ,nseddia   ,logseddia ,ndx       ,1         , &
         & ndx       ,xx        ,nxx       ,sedd50fld ,dm        , &
         & dg        ,dxx       ,dgsd      )
      !
      ! determine hiding & exposure factors
      !
      call comphidexp(frac      ,dm        ,ndx       ,lsedtot   , &
         & sedd50    ,hidexp    ,ihidexp   ,asklhe    , &
         & mwwjhe    ,1         ,ndx      )
      !
      ! compute sand fraction
      !
      call compsandfrac(frac   ,sedd50       ,ndx       ,lsedtot   , &
         & sedtyp    ,sandfrac     ,sedd50fld , &
         & 1         ,ndx         )
   endif
   !
   ! compute normal component of bed slopes at edges    (e_xxx refers to edges)

   dzdx = 0d0; dzdy = 0d0

   do L = 1, lnx
      ! Get the bottom slope components in the cell centres; keep these, needed later on
      ! Bottom slopes are positive on downsloping parts, cf bedbc2004.f90 and info from Bert Jagers
      ! So bl(k1)-bl(k2) instead of other way round
      k1 = ln(1,L); k2 = ln(2,L)
      dzdx(k1) = dzdx(k1) - wcx1(L)*(bl(k2)-bl(k1))*dxi(L)
      dzdy(k1) = dzdy(k1) - wcy1(L)*(bl(k2)-bl(k1))*dxi(L)
      dzdx(k2) = dzdx(k2) - wcx2(L)*(bl(k2)-bl(k1))*dxi(L)
      dzdy(k2) = dzdy(k2) - wcy2(L)*(bl(k2)-bl(k1))*dxi(L)
   enddo
   !   boundary conditions:
   !      dz/dn = 0
   do L=Lnxi+1,Lnx
      k1 = ln(1,L)
      k2 = ln(2,L)
      !     project in normal and tangential direction
      dzdn =  dzdx(k2) * csu(L) + dzdy(k2) * snu(L)
      dzds = -dzdx(k2) * snu(L) + dzdy(k2) * csu(L)
      !     apply boundary conditions in normal and tangential direction
      dzdn = -dzdn
      !      dzds =  dzds  ! for completeness
      !     project back to Cartesian coordinate directions
      dzdx(k1) = dzdn * csu(L) - dzds * snu(L)
      dzdy(k1) = dzdn * snu(L) + dzds * csu(L)
   end do

   !   Note: at closed boundaries, effectively dzdn=0 is applied

   do L = 1, lnx
      ! Interpolate back to links
      k1 = ln(1,L); k2 = ln(2,L)
      !       e_dzdn(L) = acl(L)*(csu(L)*dzdx(k1) + snu(L)*dzdy(k1)) + (1d0-acl(L))*(csu(L)*dzdx(k2) + snu(L)*dzdy(k2))
      e_dzdn(L) = -dxi(L)*(bl(ln(2,L))-bl(ln(1,L)))                                                              ! more accurate near boundaries
      e_dzdt(L) = acl(L)*(-snu(L)*dzdx(k1) + csu(L)*dzdy(k1))+(1d0-acl(L))*(-snu(L)*dzdx(k2) + csu(L)*dzdy(k2))  ! affected near boundaries due to interpolation
   end do
   !
   !================================================================
   !    Start of sand part
   !================================================================
   !
   ! Start of main loop over sediment fractions for suspended sediment
   ! sources, sinks, equilibrium concentrations and vertical diffusion
   ! coefficients, and bed-load transport vector components at water
   ! level points
   !
   do nm = 1, ndx
      !
      ! do not calculate sediment sources, sinks, and bed load
      ! transport in areas with very shallow water.
      !
      if (hs(nm) < sedthr) cycle
      !
      call getkbotktop(nm, kb, kt)
      if (kfsed(nm) == 0) then
         !
         ! Very shallow water:
         ! set sediment diffusion coefficient
         ! and set zero equilibrium concentrations
         !
         if (kmx>0) then              ! 3D only
            ! at layer interfaces, but not at bed and surface
            do k = kb, kt-1
               do l = 1, lsed
                  seddif(l, k) = max(vicwws(k),dicoww)   ! sigrhoi * vicwws(k) + difsed(l), with sigrhoi = 1d0
               enddo                                     ! consistent with eqtran.f90, line 265
            enddo
            ! in layers
            do k = kb, kt
               do l = 1, lsed
                  rsedeq(k, l) = 0.0_fp
               enddo
            enddo
         endif
         cycle
      endif
      !
      ! kfsed(nm) == 1
      !
      h0   = max(0.01_fp, s0(nm) - bl(nm))
      h1   = max(0.01_fp, s1(nm) - bl(nm))

      kmaxlc = kmx
      if (kmx>0) then
         !
         ! 3D CASE
         !
         kbed    = kb                                   ! okay, this is safe for Z-layers
         thicklc = 0.0_fp
         klc     = 1
         do k = kt,kb,-1                                ! counts from surface to bottom
            thicklc(klc)   = (zws(k)-zws(k-1))/h1       ! depth fraction, this works for z layers. If only sigma: m_flow::laycof can be used
            klc=klc+1
         enddo
         siglc   = 0.0_fp
         kmaxlc = klc-1                                 ! needed for z layers eventually. For sigma, equals kmx
         siglc(1) = -0.5_fp*thicklc(1)
         do klc = 2, kmaxlc
            siglc(klc) = siglc(klc-1) - 0.5_fp*(thicklc(klc) + thicklc(klc-1))
         enddo
      else                       ! 2D
         kbed    = nm            ! okay, kbed index 2D nodes from now on
         kmaxlc  = 1
         thicklc = 1.0_fp
      endif
      !
      ! Compute absolute value maximum slope for erosilt
      ! Slope taken from link, similar to Delft 3D
      !
      maxslope = 0.0_fp
      do Lf = 1, nd(nm)%lnx
         L = abs(nd(nm)%ln(Lf))
         maxslope = max(maxslope, abs(e_dzdn(L)))
      end do
      !
      ! Compute depth-averaged velocity components at cell centre, discharge based cc velocities
      !
      umean = ucxq(nm)      ! ok, calculated in getucxucyandsoon
      vmean = ucyq(nm)
      velm = sqrt(umean**2+vmean**2)
      !
      ubed = ucxq(kbed)
      vbed = ucyq(kbed)
      velb = sqrt(ubed**2 + vbed**2)
      if (kmaxlc>1) then               ! 3D only
         zvelb = 0.5_fp*thicklc(kmaxlc)*h1
      else
         zvelb = h1/ee
      endif
      !
      if (jawave > 0) then
         ubot = uorb(nm)        ! array uitgespaard
      else
         ubot = 0d0
      end if
      !
      ! Calculate total (possibly wave enhanced) roughness
      !
      if (jawave > 0) then
         z0rou = z0rouk(nm)
      else ! currents only
         z0rou = z0curk(nm)       ! currents+potentially trachy
      end if
      !
      chezy = sag * log( 1.0_fp + h1/max(1.0e-8_fp,ee*z0rou) ) / vonkar
      !
      ! bed shear stress as used in flow, or
      ! skin fiction following Soulsby; "Bed shear stress under
      ! combined waves and currents on rough and smooth beds"
      ! Estproc report TR137, 2004
      !
      if (bsskin) then
         !
         ! Compute bed stress resulting from skin friction
         !
         call compbsskin(umean, vmean, h1, wave, uorb(nm), twav(nm), &
                          & phiwav(nm), thcmud(nm), mudfrac(nm), taub(nm), &
                          & rhowat(kbed), vismol, stmpar%sedpar)
      else
         !
         ! use max bed shear stress, rather than mean
         !
         if (jawave>0) then
            ! from tauwave, done in call linkstocenterstwodoubles(taub,taubu)
         else
            ustarc = umod(nm)*vonkar/log(1.0_fp + zumod(nm)/max(z0rou,epshs))
            taub(nm) = ustarc*ustarc*rhowat(kbed)
         endif

      endif
      !
      ustarc = umod(nm)*vonkar/log(1.0_fp + zumod(nm)/max(z0rou,epshs))
      !if (scour) then
      !
      ! Calculate extra stress (tauadd) for point = nm, if so required by
      ! user input. Increment TAUB(MX) and USTARC.
      !
      !!          call shearx(tauadd, nm, gdp)
      !taub = sqrt(taub**2 + tauadd**2)
      !!
      !tauc = rhowat(kbed)*ustarc**2
      !tauc = sqrt(tauc**2 + tauadd**2)
      !ustarc = sqrt(tauc/rhowat(kbed))
      !else
      tauadd = 0.0_fp
      !endif
      !
      ! Compute effective depth averaged velocity
      !
      utot  = ustarc * chezy / sag
      ulocal= utot * uuu(nm) / (umod(nm)+eps)
      vlocal= utot * vvv(nm) / (umod(nm)+eps)
      !
      ! sa0 and tem0 have no real meaning, sa1 and tem1 before transport are at the old time-level,
      ! while after transport they are at the new time-level
      if (jasal > 0) then
         salinity =  sa1(kbed)                            ! r0(nm, kbed, lsal)
      else
         salinity = backgroundsalinity
      endif
      if (jatem > 0) then
         temperature =  tem1(kbed)                        ! r0(nm, kbed, ltem)
      else
         temperature = backgroundwatertemperature
      endif
      !
      taks0 = 0.0_fp
      !
      ! Calculate Van Rijn's reference height
      !
      if (iopkcw==1) then            !  iopkcw: options to calculate curr related roughness height
         rc = 30.0_fp*z0curk(nm)
      else
         rc = rdc
      endif
      taks0 = max(aksfac*rc, 0.01_fp*h1)
      !
      if (jawave>0) then
         if (twav(nm)>0.0_fp) then
            delr  = 0.025_fp
            taks0 = max(0.5_fp*delr, taks0)
         end if
      endif
      !
      ! Limit maximum aks to 20% of water depth
      ! (may be used when water depth becomes very small)
      !
      taks0 = min(taks0, 0.2_fp*h1)
      !
      ! Input parameters are passed via dll_reals/integers/strings-arrays
      !
      if (max_reals < MAX_RP) then
         write(errmsg,'(a)') 'fm_erosed::Insufficient space to pass real values to transport routine.'
         call write_error(errmsg, unit=mdia)
         error = .true.
         return
      endif
      dll_reals(RP_TIME ) = real(time1     ,hp)
      dll_reals(RP_EFUMN) = real(ulocal    ,hp)
      dll_reals(RP_EFVMN) = real(vlocal    ,hp)
      dll_reals(RP_EFVLM) = real(utot      ,hp)
      dll_reals(RP_UCHAR) = real(uuu(nm)   ,hp)
      dll_reals(RP_VCHAR) = real(vvv(nm)   ,hp)
      dll_reals(RP_VELCH) = real(umod(nm)  ,hp)
      dll_reals(RP_ZVLCH) = real(zumod(nm) ,hp)
      dll_reals(RP_DEPTH) = real(h1        ,hp)
      dll_reals(RP_CHEZY) = real(chezy     ,hp)
      if (wave) then
         dll_reals(RP_HRMS ) = real(hwav(nm)     ,hp)
         dll_reals(RP_TPEAK) = real(twav(nm)     ,hp)
         dll_reals(RP_TETA ) = real(phiwav(nm)   ,hp)
         dll_reals(RP_RLAMB) = real(rlabda(nm)   ,hp)
         dll_reals(RP_UORB ) = real(uorb(nm)     ,hp)
      else
         dll_reals(RP_HRMS ) = 0.0_hp
         dll_reals(RP_TPEAK) = 0.0_hp
         dll_reals(RP_TETA ) = 0.0_hp
         dll_reals(RP_RLAMB) = 0.0_hp
         dll_reals(RP_UORB ) = 0.0_hp
      endif
      dll_reals(RP_D10MX) = real(dxx(nm,i10)    ,hp)
      dll_reals(RP_D15MX) = real(dxx(nm,i15)    ,hp)
      dll_reals(RP_D90MX) = real(dxx(nm,i90)    ,hp)
      dll_reals(RP_MUDFR) = real(mudfrac(nm)    ,hp)
      dll_reals(RP_RHOWT) = real(rhowat(kbed)   ,hp) ! Density of water
      dll_reals(RP_SALIN) = real(salinity       ,hp)
      dll_reals(RP_TEMP ) = real(temperature    ,hp)
      dll_reals(RP_GRAV ) = real(ag             ,hp)
      dll_reals(RP_VICML) = real(vismol         ,hp)
      dll_reals(RP_TAUB ) = real(taub(nm)       ,hp) !      taus=taubmx incremented with tauadd
      dll_reals(RP_UBED ) = real(ubed           ,hp)
      dll_reals(RP_VBED ) = real(vbed           ,hp)
      dll_reals(RP_VELBD) = real(velb           ,hp)
      dll_reals(RP_ZVLBD) = real(zvelb          ,hp)
      dll_reals(RP_VNKAR) = real(vonkar         ,hp)
      dll_reals(RP_Z0CUR) = real(z0curk(nm)      ,hp) !   potentially with trachytopes
      dll_reals(RP_Z0ROU) = real(z0rou          ,hp)
      dll_reals(RP_DG   ) = real(dg(nm)         ,hp)
      dll_reals(RP_DM   ) = real(dxx(nm,i50)    ,hp) ! d50 mixture, not dm; following Van Rijn 2007c
      dll_reals(RP_SNDFR) = real(sandfrac(nm)   ,hp)
      dll_reals(RP_DGSD ) = real(dgsd(nm)       ,hp)
      if (iturbulencemodel > 2 .and. kmx>0 ) then
         dll_reals(RP_KTUR ) = real(turkinepsws(1,kb),hp)     ! 1=k, 2=e
      endif
      dll_reals(RP_UMEAN) = real(umean     ,hp)
      dll_reals(RP_VMEAN) = real(vmean     ,hp)
      dll_reals(RP_VELMN) = real(velm      ,hp)
      dll_reals(RP_USTAR) = real(ustarc    ,hp)
      dll_reals(RP_KWTUR) = real(ktb(nm)   ,hp)
      dll_reals(RP_BLCHG) = real(dzbdt(nm) ,hp)   ! for dilatancy
      dll_reals(RP_DZDX)  = real(dzdx(nm)  ,hp)   ! for dilatancy
      dll_reals(RP_DZDY)  = real(dzdy(nm)  ,hp)   ! for dilatancy
      !

      if (max_integers < MAX_IP) then
         write(errmsg,'(a)') 'fm_erosed::Insufficient space to pass integer values to transport routine.'
         call write_error(errmsg, unit=mdia)
         error = .true.
         return
      endif
      dll_integers(IP_NM   ) = nm
      !
      if (max_strings < MAX_SP) then
         write(errmsg,'(a)') 'fm_erosed::Insufficient space to pass strings to transport routine.'
         call write_error(errmsg, unit=mdia)
         error = .true.
         return
      endif
      !
      ! total mass in fluff layer
      !
      mfltot = 0.0_fp
      if (iflufflyr>0) then
         do l = 1, lsedtot
            mfltot = mfltot + max(0.0_fp,mfluff(l,nm))
         enddo
      endif
      !
      do l = 1, lsedtot
         ll = lstart + l
         !
         ! fraction specific quantities
         !
         dll_reals(RP_HIDEX)    = real(hidexp(nm,l) ,hp)
         dll_reals(RP_RHOSL)    = real(rhosol(l) ,hp)
         dll_integers(IP_ISED ) = l
         dll_strings(SP_USRFL)  = dll_usrfil(l)
         !
         do i = 1,stmpar%trapar%npar
            j = stmpar%trapar%iparfld(i,l)
            if (j>0) then
               par(i,l) = stmpar%trapar%parfld(nm,j)
            endif
         enddo
         !
         if (sedtyp(l) == SEDTYP_COHESIVE) then
            !
            ! sediment type COHESIVE
            !
            dll_reals(RP_D50  ) = 0.0_hp
            dll_reals(RP_DSS  ) = 0.0_hp
            dll_reals(RP_DSTAR) = 0.0_hp
            dll_reals(RP_SETVL) = real(ws(kb, l)  ,hp)
            !!             if (flmd2l) then           ! 2 layer fluid mud
            !!                 par(11,l) = entr(nm)
            !!             endif
            !
            if (kmx > 0) then
               klc = 0
               !dcwwlc = 0.0_fp
               wslc   = 0.0_fp
               do kk = kt, kb-1, -1                  ! should follow sigma conventions
                  !dcwwlc(klc) = vicwws(kk) + dicoww  ! to check: why different for Z and sigma in D3D?
                  wslc(klc)   = ws(kk, l)            ! to do: repair for 2d and 3d
                  klc=klc+1
               enddo
            else
               klc = 1
               wslc(klc)   = ws(nm, l)
            end if
            !
            ! Fluff layer parameters
            !
            fracf   = 0.0_fp
            if (iflufflyr>0) then
               if (mfltot>0.0_fp) fracf = max(0.0_fp,mfluff(l,nm))/mfltot
            endif
            !
            kmaxsd        = 1                       ! for mud fractions kmaxsd points to the grid cell at the bottom of the water column
            thick0        = thicklc(kmaxsd) * h0
            thick1        = thicklc(kmaxsd) * h1
            !
            call erosilt(thicklc        ,kmaxlc       , wslc        , mdia          , &
                       & thick1         ,thick1       , fixfac(nm,l), srcmax(nm, l) , &                         ! mass conservation
                       & frac(nm,l)     ,oldmudfrac   , flmd2l      , iform(l)      , &
                       & par(:,l)       ,max_integers , max_reals   , max_strings   , &
                       & dll_function(l),dll_handle(l), dll_integers, dll_reals     , &
                       & dll_strings    ,iflufflyr    , mfltot      , fracf         , &
                       & maxslope       ,wetslope,   &
                       & error          ,wstau        , sinktot     , sourse(nm,l)  , sourfluff)
            if (error) then
               write(errmsg,'(a)') 'fm_erosed::erosilt returned an error. Check your inputs.'
               call write_error(errmsg, unit=mdia)
            end if
            !
            if (iflufflyr>0) then
               if (iflufflyr==2) then
                  sinkf(l,nm)  = sinktot*(1.0_fp - depfac(l,nm))
                  sinkse(nm,l) = sinktot*depfac(l,nm)
               else
                  sinkf(l,nm)  = sinktot
                  sinkse(nm,l) = 0.0_fp
               endif
               !
               sourf(l,nm)  = sourfluff
            else
               sinkse(nm,l) = sinktot
               sourse(nm,l) = sourse(nm,l) + sourfluff
            endif
            !
            if (kmx>0) then
               !
               ! For 3D model set sediment diffusion coefficient
               ! NOTE THAT IF ALGEBRAIC OR K-L TURBULENCE MODEL IS USED THEN WAVES
               ! ONLY AFFECT THE VERTICAL TURBULENT MIXING VIA THE ENHANCED BED
               ! ROUGHNESS
               !
               klc    = 0
               do k = kt, kb-1, -1
                  seddif(l, k) = max(vicwws(k),dicoww)     ! constant value from mdu; JRE to check
                  klc=klc+1
               enddo
            endif
            !
            ! l runs from 1 to lsedtot, kmxsed is defined for 1:lsed
            ! The first lsed fractions are the suspended fractions (including cohesive ones),
            ! so this works
            !
            kmxsed(nm, l) = kb ! to check
            cycle
         endif
         !
         ! sediment type NONCOHESIVE_SUSPENDED or NONCOHESIVE_TOTALLOAD
         !
         suspfrac = sedtyp(l)/=SEDTYP_NONCOHESIVE_TOTALLOAD
         !
         ! Calculation for sand or bedload
         !
         ! Reset Prandtl-Schmidt number for sand fractions
         ! viscosity/diffusivity
         !
         if (suspfrac) then
            sigdif = 1.0_fp      ! has different meaning here, not to confuse with sigdif in m_turbulence
         endif
         tsd  = -999.0_fp
         di50 = sedd50(l)
         if (di50 < 0.0_fp) then
            !  Space varying sedd50 specified in array sedd50fld:
            !  Recalculate dstar, tetacr and taucr for each nm,l - point
            di50     = sedd50fld(nm)
            drho     = (rhosol(l)-rhowat(kbed)) / rhowat(kbed)
            dstar(l) = di50 * (drho*ag/vismol**2)**0.3333_fp
            if (dstar(l) < 1.0_fp) then
               if (iform(l) == -2) then
                  tetacr(l) = 0.115_fp / (dstar(l)**0.5_fp)
               else
                  tetacr(l) = 0.24_fp / dstar(l)
               endif
            elseif (dstar(l) <= 4.0_fp) then
               if (iform(l) == -2) then
                  tetacr(l) = 0.115_fp / (dstar(l)**0.5_fp)
               else
                  tetacr(l) = 0.24_fp / dstar(l)
               endif
            elseif (dstar(l)>4.0_fp .and. dstar(l)<=10.0_fp) then
               tetacr(l) = 0.14_fp  / (dstar(l)**0.64_fp)
            elseif (dstar(l)>10.0_fp .and. dstar(l)<=20.0_fp) then
               tetacr(l) = 0.04_fp  / (dstar(l)**0.1_fp)
            elseif (dstar(l)>20.0_fp .and. dstar(l)<=150.0_fp) then
               tetacr(l) = 0.013_fp * (dstar(l)**0.29_fp)
            else
               tetacr(l) = 0.055_fp
            endif
            taucr(l) = factcr * (rhosol(l)-rhowat(kbed)) * ag * di50 * tetacr(l)
         endif
         !
         if (suspfrac) then
            tsigmol = 1d0            !sigmol(ll)  ! Prandtl number for now == 1d0, to check
            tdss    = dss(nm, l)
            twsk    = ws(kb, l)      ! was kb-1, should be same in 3D (see fallve)
         else
            !
            ! use dummy values for bedload fractions
            !
            tsigmol =  1.0_fp
            tdss    =  di50
            twsk    =  0.0_fp
         endif
         !
         ! NONCOHESIVE fraction specific quantities
         !
         dll_reals(RP_D50  ) = real(di50    ,hp)
         dll_reals(RP_DSS  ) = real(tdss    ,hp)
         dll_reals(RP_DSTAR) = real(dstar(l),hp)
         dll_reals(RP_SETVL) = real(twsk    ,hp) ! Settling velocity near bedlevel
         !
         ! Calculate bed porosity for dilatancy
         !
         poros = 1d0-cdryb(l)/rhosol(l)
         dll_reals(RP_POROS) = real(poros   ,hp)
         !
         par(1,l) = ag
         par(2,l) = rhowat(kbed) ! rhow
         par(3,l) = rhosol(l)
         par(4,l) = (rhosol(l)-rhowat(kbed)) / rhowat(kbed)
         par(5,l) = 1.0E-6     ! laminar viscosity of water
         par(6,l) = di50
         !
         ! SWITCH 2DH/3D SIMULATIONS
         !
         if (kmaxlc > 1) then
            !             
            ! 3D CASE
            !
            concin3d = 0.0_fp
            if (suspfrac) then
               !
               ! Fill local 1dv arrays with settling velocity, diffusivity and concentration.
               !
               klc    = 0
               dcwwlc = 0.0_fp
               wslc   = 0.0_fp
               do kk = kt, kb-1, -1         ! sigma convention
                  dcwwlc(klc) = max(vicwws(kk),dicoww)  ! JRE to check for double counting
                  wslc(klc)   = ws(kk, l)
                  klc=klc+1
               enddo
               !
               klc    = 1
               do kk = kt, kb, -1
                  concin3d(klc) = max(0.0_fp , constituents(ll,kk))
                  klc=klc+1
               enddo
            endif
            taks = 0.0_fp
            !
            ! Solve equilibrium concentration vertical and
            ! integrate over vertical
            !
            if (jasecflow > 0) then                          ! secondary flow in 3D
               spirintnm = spirint(nm)
            endif
            !
            call eqtran(siglc           ,thicklc       ,kmaxlc      ,wslc      ,ltur      , &
                      & frac(nm,l)      ,tsigmol       ,dcwwlc      ,mdia      ,taucr(l)  , &
                      & bfmpar%rksr(nm) ,3             ,jasecflow   ,spirintnm ,suspfrac  , &
                      & tetacr(l)       ,concin3d      , &
                      & dzdx(nm)        ,dzdy(nm)      ,ubot        ,tauadd    ,sus       , &
                      & bed             ,susw          ,bedw        ,espir     ,wave      , &
                      & scour           ,ubot_from_com              ,camax     ,eps       , &
                      & iform(l)        ,par(:,l)      ,max_integers,max_reals ,max_strings, &
                      & dll_function(l) ,dll_handle(l) ,dll_integers,dll_reals ,dll_strings, &
                      & taks            ,caks          ,taurat(nm,l),sddflc    ,rsdqlc    , &
                      & kmaxsd          ,conc2d        ,sbcx(nm,l)  ,sbcy(nm,l),sbwx(nm,l) , &
                      & sbwy(nm,l)      ,sswx(nm,l)    ,sswy(nm,l)  ,tdss      ,caks_ss3d , &
                      & aks_ss3d        ,ust2(nm)      ,tsd         ,error     )
            !
            if (error) then
               write(errmsg,'(a)') 'fm_erosed::eqtran in 3D returned an error. Check your inputs.'
               call write_error(errmsg, unit=mdia)
            end if
            !
            if (suspfrac) then
               aks(nm, l) = taks
               dss(nm, l) = tdss
               !
               ! Copy results into arrays
               !
               kmxsed(nm, l) = kb + kmaxlc-kmaxsd
               !
               klc=0
               do kk = kt, kb-1, -1
                  seddif(l, kk) = sddflc(klc)
                  klc           = klc + 1
               enddo
               rsedeq(nm, l)    = rsdqlc(kmaxsd)
               !
               thick0 = thicklc(kmaxsd) * h0
               thick1 = thicklc(kmaxsd) * h1
               !
               call soursin_3d  (h1                ,thick1         ,thick1             ,              &      ! thick1 iso thick0 mass conservation
                              &  siglc(kmaxsd)     ,thicklc(kmaxsd),constituents(l,kmxsed(nm,l))    , &
                              &  vismol            ,tsigmol        ,seddif(l, kmxsed(nm,l)-1),        &                 
                              &  rhosol(l)         ,caks_ss3d      ,ws(kmxsed(nm,l),l)      ,         &          
                              &  aks_ss3d          ,sourse(nm,l)   ,sour_im(nm,l)      ,              &
                              &  sinkse(nm,l) )
               !
               ! Impose relatively large vertical diffusion
               ! coefficients for sediment in layer interfaces from
               ! bottom of reference cell downwards, to ensure little
               ! gradient in sed. conc. exists in this area.

               difbot = 10.0_fp * ws(kmxsed(nm,l)-1,l) * thick1
               do kk = kb-1, kmxsed(nm,l)-1
                  seddif(l, kk) = difbot
               enddo
            endif ! suspfrac
         else
            !
            ! kmaxlc = 1
            ! 2D CASE (Numerical approximation)
            !
            if (suspfrac) then
               !
               ! Fill local 1dv arrays with fall velocity and
               ! diffusivity
               !
               do k2d = 0, kmax2d
                  ws2d(k2d)   = ws(kb, l)
                  dcww2d(k2d) = 0.0_fp        ! to check
               enddo
            endif
            trsedeq =  0.0_fp
            taks = taks0
            !
            if (jasecflow > 0) then                          ! secondary flow in 2D
               spirintnm = spirint(nm)
            endif
            !
            ! Solve equilibrium concentration vertical and
            ! integrate over vertical; compute bedload
            ! transport excluding slope effects, but including spiral flow
            !
            ltur = 0
            !
            call eqtran(sig2d   ,thck2d        ,kmax2d       ,ws2d      ,ltur      , &
                      & frac(nm,l)     ,tsigmol       ,dcww2d       ,mdia      ,taucr(l)  , &
                      & bfmpar%rksr(nm),2             ,jasecflow    ,spirintnm ,suspfrac  , &
                      & tetacr(l)      ,concin2d      , &
                      & dzdx(nm)       ,dzdy(nm)      ,ubot         ,tauadd    ,sus       , &
                      & bed            ,susw          ,bedw         ,espir     ,wave      , &
                      & scour          ,ubot_from_com ,camax        ,eps       , &
                      & iform(l)       ,par(:,l)      ,max_integers ,max_reals ,max_strings, &
                      & dll_function(l),dll_handle(l) ,dll_integers ,dll_reals ,dll_strings, &
                      & taks           ,caks          ,taurat(nm,l) ,sddf2d    ,rsdq2d    , &
                      & kmaxsd         ,trsedeq       ,sbcx(nm,l)   ,sbcy(nm,l),sbwx(nm,l) , &
                      & sbwy(nm,l)     ,sswx(nm,l)    ,sswy(nm,l)   ,tdss      ,caks_ss3d , &
                      & aks_ss3d       ,ust2(nm)      ,tsd          ,error     )

            if (error) then
               write(errmsg,'(a)') 'fm_erosed::eqtran in 2D returned an error. Check your inputs.'
               call write_error(errmsg, unit=mdia)
            end if

            if (suspfrac) then
               aks   (nm, l)       = taks
               dss   (nm, l)       = tdss
               rsedeq(nm, l)       = trsedeq
               kmxsed(nm, l)       = kbed        ! nm or 1  when sedthr not passed
               !
               ! Galappatti time scale and source and sink terms
               !
               call soursin_2d(umod(nm)      ,ustarc        ,h0            ,h1        , &
                             & ws(kb,l)      ,tsd           ,trsedeq       ,factsd,    &
                             & sourse(nm,l)  ,sour_im(nm,l) ,sinkse(nm,l)  )
            endif ! suspfrac
         endif ! kmaxlc = 1
         if (suspfrac) then
            rca(nm, l) = caks * rhosol(l)
         endif
      enddo ! next sediment fraction
      ua(nm) = real(dll_reals(RP_UAU), fp)
      va(nm) = real(dll_reals(RP_VAU), fp)
   enddo ! next nm point
   !
   ! Distribute velocity asymmetry to links
   !
   do L = 1, lnxi
      k1 = ln(1,L); k2=ln(2,L)
      uau(L) = (acL(L)*ua(k1) + (1d0-acL(L))*ua(k2)) * csu(L) +   &
         (acL(L)*va(k1) + (1d0-acL(L))*va(k2)) * snu(L)
   end do
   !
   do L = lnxi+1, lnx   ! Boundaries: neumann
      k = ln(2,L)
      uau(L) = ua(k)*csu(L) + va(k)*snu(L)
   end do
   !
   ! Reduce the source and sink terms to avoid large bed level changes
   !
   call fm_red_soursin()

   do l = 1,lsedtot                                   ! this is necessary for next calls to upwbed
      if (sedtyp(l)/=SEDTYP_COHESIVE) then
         do nm = 1, ndx
            sxtot(nm, l) = sbcx(nm, l) + sbwx(nm, l) + sswx(nm, l)
            sytot(nm, l) = sbcy(nm, l) + sbwy(nm, l) + sswy(nm, l)
         enddo
      endif
   enddo
   !
   if (stmpar%morpar%moroutput%rawtransports) then
      sbcx_raw = sbcx; sbcy_raw = sbcy;    ! save transports before upwinding and bed slope effects
      sbwx_raw = sbwx; sbwy_raw = sbwy;    ! to compare with analytical solutions
      sswx_raw = sswx; sswy_raw = sswy;
   endif
   !
   ! Upwind scheme for bed load and wave driven transport
   ! Convert sand bed load transport to velocity points using upwind scheme
   !
   if (bed > 0.0_fp) then
      !
      ! Upwind bed load transport
      !
      call fm_upwbed(lsedtot, sbcx, sbcy, sxtot, sytot, e_sbcn, e_sbct)
   endif
   !
   if (bedw>0.0_fp .and. jawave > 0) then
      !
      ! Upwind wave-related bed load load transports
      !
      call fm_upwbed(lsedtot, sbwx, sbwy, sxtot, sytot, e_sbwn, e_sbwt)
   endif
   !
   if (susw>0.0_fp .and. jawave > 0) then
      !
      ! Upwind wave-related suspended load transports
      !
      call fm_upwbed(lsedtot, sswx, sswy, sxtot, sytot, e_sswn, e_sswt)
   endif
   !
   ! Bed-slope and sediment availability effects for
   ! current-related bed load transport
   !
   if (bed > 0.0_fp) then
      call fm_adjust_bedload(e_sbcn, e_sbct, .true.)
   endif
   !
   ! Determine incoming discharge and transport at nodes
   !
   qb_out = 0d0; width_out = 0d0; sb_in = 0d0; sb_dir = 1
   BranInIDLn = 0
   do inod = 1, network%nds%Count
      pnod => network%nds%node(inod)
      if (pnod%numberofconnections > 1) then
         k3 = pnod%gridnumber
         do j=1,nd(k3)%lnx
            L = iabs(nd(k3)%ln(j))
            Ldir = sign(1,nd(k3)%ln(j))
            !
            wb1d = wu_mor(L)
            !
            if (u1(L)*Ldir < 0d0) then
               ! Outgoing discharge
               qb1d = -q1(L)*Ldir  ! replace with junction advection: to do WO
               width_out(inod) = width_out(inod) + wb1d
               qb_out(inod)    = qb_out(inod) + qb1d
               do ised = 1, lsedtot
                  sb_dir(inod, ised, j) = -1           ! set direction to outgoing
               enddo
            else
               ! Incoming discharge
               if (branInIDLn(inod) == 0) then
                  branInIDLn(inod) = L
               else
                  branInIDLn(inod) = -444               ! multiple incoming branches
               endif
            endif
         enddo
      endif
   enddo
   !
   ! Apply nodal relations to transport
   !
   do inod = 1, network%nds%Count
      pnod => network%nds%node(inod)
      if (pnod%numberofconnections == 1) cycle
      if (pnod%nodeType == nt_LinkNode) then  ! connection node
         k1 = pnod%gridnumber
         do j=1,nd(k1)%lnx
            L = iabs(nd(k1)%ln(j))
            Ldir = sign(1,nd(k1)%ln(j))
            !
            wb1d = wu_mor(L)
            do ised = 1, lsedtot
               sb1d = e_sbcn(L, ised) * Ldir  ! first compute all outgoing sed. transport.
               ! this works for one incoming branch TO DO: WO
               if (sb_dir(inod, ised, j) == -1) then
                  sb_in(inod, ised) = sb_in(inod, ised) + max(-wb1d*sb1d, 0.0_fp)  ! outgoing transport is negative
               endif
            enddo
         enddo
      endif
   enddo
   !
   ! Determining sediment redistribution
   !
   ! loop over sediment fractions
   do ised = 1, lsedtot

      ! mor%nrd%nFractions = or 1 (One for All Fractions) or lsedtot (One for Every Fraction)
      iFrac = min(ised, stmpar%nrd%nFractions)

      pFrac => stmpar%nrd%nodefractions(iFrac)

      do inod = 1, network%nds%Count
         pnod => network%nds%node(inod)
         if (pnod%nodeType == nt_LinkNode) then  ! connection node

            facCheck = 0.d0

            if (pnod%numberofconnections == 1) cycle


            ! loop over branches and determine redistribution of incoming sediment
            k3 = pnod%gridnumber
            do j=1,nd(k3)%lnx
               L = iabs(nd(k3)%ln(j))
               Ldir = sign(1,nd(k3)%ln(j))
               qb1d = -Ldir*q1(L)
               wb1d = wu_mor(L)

               ! Get Nodal Point Relation Data
               nrd_idx = get_noderel_idx(inod, pFrac, pnod%gridnumber, branInIDLn(inod), pnod%numberofconnections)

               pNodRel => pFrac%noderelations(nrd_idx)

               if (sb_dir(inod, ised, j) == -1) then ! is outgoing

                  if (qb_out(inod) > 0.0_fp) then

                     if (pNodRel%Method == 'function') then

                        expQ = pNodRel%expQ
                        expW = pNodRel%expW

                        facQ = (qb1d / qb_out(inod))**expQ
                        facW = (wb1d / width_out(inod))**expW

                        facCheck = facCheck + facQ * facW

                        e_sbcn(L,ised) = -Ldir * facQ * facW * sb_in(inod, ised) / wu_mor(L)

                     elseif (pNodRel%Method == 'table') then

                        facCheck = 1.0d0

                        if (L == pNodRel%BranchOut1Ln) then
                           Qbr1 = qb1d
                           Qbr2 = qb_out(inod) - qb1d
                        elseif (L == pNodRel%BranchOut2Ln) then
                           Qbr1 = qb_out(inod) - qb1d
                           Qbr2 = qb1d
                        else
                           call SetMessage(LEVEL_FATAL, 'Unknown Branch Out (This should never happen!)')
                        endif

                        QbrRatio = Qbr1 / Qbr2

                        SbrRatio = interpolate(pNodRel%Table, QbrRatio)

                        if (L == pNodRel%BranchOut1Ln) then
                           e_sbcn(L,ised) = -Ldir * SbrRatio * sb_in(inod, ised) / (1 + SbrRatio) / wu_mor(L)
                           e_sbct(L,ised) = 0.0
                        elseif (L == pNodRel%BranchOut2Ln) then
                           e_sbcn(L,ised) = -Ldir * sb_in(inod, ised) / (1 + SbrRatio) / wu_mor(L)
                           e_sbct(L,ised) = 0.0
                        endif


                     else
                        call SetMessage(LEVEL_FATAL, 'Unknown Nodal Point Relation Method Specified')
                     endif

                  else
                     e_sbcn(L,ised) = 0.0_fp
                     e_sbct(L,ised) = 0.0
                  endif

               endif

            enddo    ! Branches

            ! Correct Total Outflow
            if ((facCheck /= 1.0_fp) .and. (facCheck > 0.0_fp)) then
               ! loop over branches and correct redistribution of incoming sediment
               do j=1,nd(k3)%lnx
                  L = iabs(nd(k3)%ln(j))
                  if (sb_dir(inod, ised, j) == -1) then
                     e_sbcn(L,ised) = e_sbcn(L,ised)/facCheck
                  endif
               enddo    ! Branches
            endif
         endif
      enddo      ! Nodes

   enddo    ! Fractions

   !
   ! Bed-slope and sediment availability effects for
   ! wave-related bed load transport
   !
   if (bedw>0.0_fp .and. jawave > 0) then
      call fm_adjust_bedload(e_sbwn, e_sbwt,.false.)
   endif
   !
   ! Sediment availability effects for
   ! wave-related suspended load transport
   !
   if (susw>0.0_fp .and. jawave > 0) then
      call fm_adjust_bedload(e_sswn, e_sswt, .false.)
   endif
   !
   if (duneavalan) then
      call duneaval(e_sbcn, error)         ! only on current related bed transport
      if (error) then
         write(errmsg,'(a)') 'fm_erosed::duneavalan returned an error. Check your inputs.'
         call write_error(errmsg, unit=mdia)
      end if
   end if
   !
   ! Summation of current-related and wave-related transports on links
   !
   do l = 1,lsedtot
      if (sedtyp(l)/=SEDTYP_COHESIVE) then
         do nm = 1, lnx
            e_sbn(nm, l) = e_sbcn(nm, l) + e_sbwn(nm, l) + e_sswn(nm, l)
            e_sbt(nm, l) = e_sbct(nm, l) + e_sbwt(nm, l) + e_sswt(nm, l)
         enddo
      endif
   enddo
   !
   ! Update sourse fluxes due to sand-mud interaction
   !
   allocate(evel(lsedtot), stat=istat)
   do nm = 1, ndx
      if (pmcrit(nm)<0.0_fp) cycle
      if (mudfrac(nm)<=0.0_fp .or. mudfrac(nm)>=1.0_fp) cycle
      !
      ! compute erosion velocities
      !
      evel = 0.0_fp
      !call getkbotktop(nm, kb, kt)
      do l = 1, lsed
         ll = lstart + l
         kmaxsd = kmxsed(nm,l)              ! meaning of kmaxsd changes here!
         if (frac(nm,l)>0.0_fp)  evel(l) = (sourse(nm,l) - sour_im(nm,l)*constituents(ll,kmaxsd))/(cdryb(l)*frac(nm,l))
      enddo
      !
      ! recompute erosion velocities
      !
      call sand_mud(lsed, evel, frac(nm,:), mudfrac(nm), sedtyp, pmcrit(nm))
      !
      ! recompute erosion fluxes
      ! only explicit part of erosion flux is changed
      ! (is effectively the same as changing the equilibrium concentration)
      !
      do l = 1, lsed
         ll = ised1 - 1 + l
         kmaxsd = kmxsed(nm,l)
         sourse(nm,l) = frac(nm,l)*cdryb(l)*evel(l) + sour_im(nm,l)*constituents(ll,kmaxsd)
      enddo
   enddo
   deallocate(evel, stat=istat)
   !
   ! Add implicit part of source term to sinkse
   !
   do l = 1, lsed
      do nm = 1, ndx
         sinkse(nm, l) = sinkse(nm, l) + sour_im(nm, l)
      enddo
   enddo
   !
   if (jasourcesink==0) then
      sourse=0d0
      sinkse=0d0
   elseif (jasourcesink==1) then
      !
   elseif (jasourcesink==2) then
      sinkse=0d0
   elseif (jasourcesink==3) then
      sourse=0d0
   endif
   !
   if (jatranspvel > 0 .or. jawave>0) then
      u1 = u1ori; u0 = u0ori; v=vori
      call setucxucyucxuucyu()
   end if

   deallocate(dzdx, dzdy, u1ori, u0ori, vori, stat = istat)
   if (istat == 0) deallocate(qb_out, stat = istat)
   if (istat == 0) deallocate(width_out, stat = istat)
   if (istat == 0) deallocate(sb_in, stat = istat)
   if (istat == 0) deallocate(sb_dir, stat = istat)
   if (istat == 0) deallocate(BranInIDLn, stat = istat)

   if (istat /= 0) then
      error = .true.
      write(errmsg,'(a)') 'fm_erosed::error deallocating memory.'
      call write_error(errmsg, unit=mdia)
   endif

   end subroutine fm_erosed


   ! Interpolate flownode-based vector (sx,sy) to edge-based vector (sxx, syy)
   subroutine fm_upwbed(lsedtot, sx, sy, sxtot, sytot, e_sn, e_st)
   use m_flowgeom
   use m_flow
   use unstruc_messages
   use m_sediment, only: stmpar, jabndtreatment  ! debug
   use sediment_basics_module
   implicit none

   integer,                                  intent(in)  :: lsedtot        !< number of sediment fractions
   double precision, dimension(Ndx,lsedtot), intent(in)  :: sx, sy         !< cell (flownode)-based quantity
   double precision, dimension(Ndx,lsedtot), intent(in)  :: sxtot, sytot   !< cell (flownode)-based fluxes
   double precision, dimension(Lnx,lsedtot), intent(out) :: e_sn, e_st     !< edge (flowlink)-based quantity, normal and tangential components

   double precision                                      :: sutot1, sutot2

   integer                                               :: k1, k2, Lf, l, lnxlnxi

   !if ( laterallyaveragedbedload ) then
   !   call mess(LEVEL_ERROR, 'upwbed: laterally averaged bedload not supported')
   !end if

   if (jabndtreatment==0) then
      lnxlnxi = lnx
   else if (jabndtreatment==1) then
      lnxlnxi = lnxi
   end if

   !  internal flowlinks
   do Lf=1,Lnxlnxi
      !     check if flowlink is active and if it connects two active sediment flownodes
      if ( hu(Lf)>epshu ) then
         !        find left and right neighboring flownodes
         k1 = ln(1,Lf)
         k2 = ln(2,Lf)

         do l=1,lsedtot
            if (stmpar%sedpar%sedtyp(l) == SEDTYP_COHESIVE) cycle   ! conform with d3d
            !
            !           project the fluxes in flowlink direction
            sutot1 =  csu(Lf)*sxtot(k1,l) + snu(Lf)*sytot(k1,l)
            sutot2 =  csu(Lf)*sxtot(k2,l) + snu(Lf)*sytot(k2,l)

            !           upwind approximation
            if ( sutot1>0d0 .and. sutot2>0d0 ) then
               e_sn(Lf,l) =  csu(Lf)*sx(k1,l) + snu(Lf)*sy(k1,l)
               !e_st(Lf,l) = -snu(Lf)*sx(k1,l) + csu(Lf)*sy(k1,l)
            else if ( sutot1<0d0 .and. sutot2<0d0 ) then
               e_sn(Lf,l) =  csu(Lf)*sx(k2,l) + snu(Lf)*sy(k2,l)
               !e_st(Lf,l) = -snu(Lf)*sx(k2,l) + csu(Lf)*sy(k2,l)
            else
               e_sn(Lf,l) =  csu(Lf)*0.5d0*(sx(k1,l)+sx(k2,l)) + snu(Lf)*0.5d0*(sy(k1,l)+sy(k2,l))
               !e_st(Lf,l) = -snu(Lf)*0.5d0*(sx(k1,l)+sx(k2,l)) + csu(Lf)*0.5d0*(sy(k1,l)+sy(k2,l))
            end if
            e_st(Lf,l) = -snu(Lf)*0.5d0*(sx(k1,l)+sx(k2,l)) + csu(Lf)*0.5d0*(sy(k1,l)+sy(k2,l))
         end do
      else
         do l=1,lsedtot
            e_sn(Lf,l) = 0d0
            e_st(Lf,l) = 0d0
         end do
      end if
   end do

   if (jabndtreatment==1) then
      ! boundary flowlinks
      do Lf=Lnxi+1,Lnx
         if ( hu(Lf)>epshu .and. u1(Lf)<=0d0) then
            !           find left and right neighboring flownodes
            k1 = ln(1,Lf)  ! boundary node
            k2 = ln(2,Lf)  ! internal node

            do l=1,lsedtot
               e_sn(Lf,l) =  csu(Lf)*sx(k2,l) + snu(Lf)*sy(k2,l)
               e_st(Lf,l) = -snu(Lf)*sx(k2,l) + csu(Lf)*sy(k2,l)
            end do
         end if
      end do
   end if
   end subroutine fm_upwbed


   subroutine fm_bott3d()
   !!--description-----------------------------------------------------------------
   !
   !    Function: Computes suspended sediment transport correction
   !              vector for sand sediment fractions
   !              Computes depth integrated suspended sediment
   !              transport vector for output to map file
   !              Computes change in BODSED based on source and sink
   !              terms calculated in EROSED, and new concentrations.
   !              Calculates new mixing layer thickness based on
   !              change in BODSED values
   !              Calculates new depth values based on changes
   !              in bottom sediment.
   !              Includes erosion of dry points and associated
   !              bathymetry changes
   !!!--declarations----------------------------------------------------------------
   use precision
   use bedcomposition_module
   use sediment_basics_module
   use m_flow     , only: vol0, vol1, s0, s1, hs, u1, kmx, hu, au, zws
   use m_flowgeom , only: bai, ndxi, nd, wu, bl, ba, ln, dx, ndx, lnx, lnxi, acl, wcx1, wcy1, wcx2, wcy2, xz, yz, wu_mor, ba_mor, bai_mor
   use m_flowexternalforcings, only: nbndz, nbndu, nopenbndsect
   use m_flowparameters, only: epshs, epshu, jawave, eps10
   use m_sediment,  only: stmpar, sedtra, mtd, sedtot2sedsus, jaupdates1, m_sediment_sed=>sed
   use m_flowtimes, only: dts, tstart_user, time1, dnt, julrefdat, tfac, ti_sed, ti_seds
   use m_transport, only: fluxhortot, ised1, isedn, constituents, sinksetot, sinkftot
   use unstruc_files, only: mdia, close_all_files
   use m_fm_erosed
   use Messagehandling
   use message_module, only: writemessages, write_error
   use unstruc_netcdf, only: unc_closeall
   use m_dredge
   use m_dad   , only: dad_included
   use table_handles , only:handletype, gettabledata
   use m_partitioninfo
   use m_fm_update_crosssections
   use m_fm_morstatistics, only: morstats, morstatt0
   use precision_basics
   !
   implicit none
   !
   type (handletype)                    , pointer :: bcmfile
   type (bedbndtype)     , dimension(:) , pointer :: morbnd
   logical                              , pointer :: cmpupd
   !!
   !! Local parameters
   !!
   character(len=256)                               :: msg
   !!
   !! Global variables
   !!
   !!
   !! Local variables
   !!
   logical                              :: bedload, error
   integer                              :: ierror
   integer                              :: l, nm, ii, ll, Lx, LLL, Lf, lstart, j, bedchangemesscount, k, k1, k2, knb, nb, kb, ki, mout, kk, ised
   integer                              :: Lb, Lt, ka, kf1, kf2, kt, nto, n1, n2, iL
   double precision                     :: dsdnm, eroflx, sedflx, thick0, thick1, trndiv, flux, sumflux, dtmor
   double precision                     :: dhmax, h1, totdbodsd, totfixfrac, bamin, thet, dv, zktop, cflux, bedflxtot

   integer,          parameter          :: bedchangemessmax = 50
   double precision, parameter          :: dtol = 1d-16

   double precision                     :: tausum2(1)
   double precision                     :: sbsum, taucurc, czc
   double precision, dimension(lsedtot) :: bc_sed_distribution
   double precision, dimension(:), allocatable :: bl0

   integer          :: icond
   integer          :: jb
   integer          :: ib
   integer          :: kvalue
   integer          :: li
   integer          :: lsedbed
   integer          :: nxmx
   integer          :: nmk
   integer          :: lm
   double precision :: aksu
   double precision :: apower
   double precision :: cavg
   double precision :: cavg1
   double precision :: cavg2
   double precision :: ceavg
   double precision :: cumflux
   double precision :: alfa_dist
   double precision :: alfa_mag
   double precision :: dz
   double precision :: dzup
   double precision :: htdif
   double precision :: rate
   double precision :: r1avg
   double precision :: z
   double precision :: timhr
   !!
   !!! executable statements -------------------------------------------------------
   !!
   bcmfile             => stmpar%morpar%bcmfile
   morbnd              => stmpar%morpar%morbnd
   cmpupd              => stmpar%morpar%cmpupd

   if (.not. allocated(bl0)) then
      allocate(bl0(1:ndx),stat=ierror)
      bl0 = 0d0
   endif

   bedload = .false.
   dtmor   = dts*morfac
   lstart  = ised1-1
   error = .false.
   timhr = time1 / 3600.0d0
   nto    = nopenbndsect
   !
   !   Calculate suspended sediment transport correction vector (for SAND)
   !   Note: uses GLM velocities, consistent with DIFU
   !
   !   Correct suspended sediment transport rates by estimating the
   !   quantity of suspended sediment transported in the grid cells below
   !   Van Rijn's reference height (aks) and making a vector of this in the
   !   opposite direction to the suspended sediment transport.
   !
   !   Ensure suspended sediment correction arrays and suspended sediment
   !   vector arrays are blank
   !
   !
   e_scrn = 0d0
   e_scrt = 0d0
   e_ssn  = 0d0
   !
   ! calculate corrections
   !
   if (sus /= 0d0) then
      !
      ! suspension transport correction vector only for 3D
      !
      if (kmx > 0) then
         do l = 1, lsed
            ll = ised1-1 + l
            if (sedtyp(l) == SEDTYP_NONCOHESIVE_SUSPENDED) then
               !
               ! Determine aks
               !
               !                call dfexchg( fluxu(:,:,ll) ,1, kmax, dfloat, nm_pos, gdp)
               !                call dfexchg( fluxv(:,:,ll) ,1, kmax, dfloat, nm_pos, gdp)
               do Lx = 1, lnx
                  call getLbotLtop(Lx, Lb, Lt)
                  !
                  ! try new approach - should be smoother
                  ! don't worry about direction of the flow
                  ! use concentration at velocity point=average of the
                  ! two adjacent concentrations
                  ! use aks height at velocity point = average of the
                  ! two adjacent aks values
                  !
                  ! note correction vector only computed for velocity
                  ! points with active sediment cells on both sides
                  !
                  if (hu(Lx) > epshu) then
                     cumflux = 0.0_fp
                     !
                     ! Determine reference height aks in vel. pt.
                     if (Lx>lnxi) then ! boundary link, take inner point value
                        k2 = ln(2,Lx)
                        aksu = aks(k2,l)
                     else
                        k1 = ln(1,Lx); k2 = ln(2,Lx)
                        !aksu = 0.5*(aks(k1, l) + aks(k2, l))
                        aksu = acL(Lx)*aks(k1, l) + (1d0-acL(Lx))*aks(k2, l)
                     end if
                     !
                     ! work up through layers integrating transport
                     ! below aksu, according to Bert's new implementation
                     !
                     zktop = 0d0
                     ka = 0
                     do Lf = Lb, Lt
                        zktop = hu(Lf)
                        if (Lf==Lt) then
                           dz = hu(Lf)
                        else
                           dz = hu(Lf)-hu(Lf-1)
                        end if
                        !
                        ! if layer containing aksu
                        !
                        if (aksu <= zktop) then     
                           ka = Lf
                           if (Lf/=Lt) then
                              dzup = hu(Lf+1)-hu(Lf)
                           endif
                           ! the contribution of this layer is computed below
                           exit
                        else
                           cumflux = cumflux + fluxhortot(ll,Lf)
                        endif
                     enddo
                     !
                     k = ka
                     if (k==0) then
                        ! aksu larger than water depth, so all done
                     elseif (k==Lt) then
                        ! aksu is located in top layer; use simple flux
                        ! approximation assuming uniform flux
                        cumflux = cumflux + fluxhortot(ll,k)*(aksu - zktop + dz)/dz
                     else
                        ! aksu is located in a layer below the top layer
                        !
                        ! Get reference concentration at aksu
                        !
                        k1 = ln(1,Lx); k2 = ln(2,Lx)  
                        if (Lx>lnxi) then ! boundary link, take inner point value
                           ceavg = rca(k2,l)
                        else
                           ceavg = acL(Lx)*rca(k1, l) + (1d0-acL(Lx))*rca(k2, l)   ! Perot average
                        end if
                        !
                        ! Get concentration in layer above this layer
                        !
                        kf1 = ln(1,k+1); kf2 = ln(2,k+1)
                        r1avg = acL(Lx)*constituents(ll,kf1) + (1d0-acL(Lx))*constituents(ll,kf2)
                        !
                        ! If there is a significant concentration gradient, and significant
                        ! reference concentration
                        !
                        if (ceavg>r1avg*1.1d0 .and. ceavg>0.05d0) then
                           !
                           ! Compute Rouse number based on reference concentration and
                           ! concentration of the layer above it. Make sure that Rouse number
                           ! differs significantly from 1, and that it is not too large.
                           ! Note: APOWER = - Rouse number
                           !
                           ! The Rouse profile equation
                           !
                           !            [ a*(H-z) ]^R
                           ! c(z) = c_a*[ ------- ]
                           !            [ z*(H-a) ]
                           !
                           ! is here approximated by
                           !
                           ! c(z) = c_a*(a/z)^R = c_a*(z/a)^-R
                           !
                           z = zktop + dzup/2.0_fp
                           apower = log(max(r1avg/ceavg,1d-5)) / log(z/aksu)
                           if (apower>-1.05d0 .and. apower<=-1.0d0) then
                              apower = -1.05d0
                           elseif (apower>=-1.0d0 .and. apower<-0.95d0) then
                              apower = -0.95d0
                           else
                           endif
                           apower  = min(max(-10.0d0 , apower), 10.0d0)
                           !
                           ! Compute the average concentration cavg between the reference
                           ! height a and the top of the current layer (bottom of layer above) z.
                           !         /zktop                           zktop                       zktop
                           ! cavg = | c(z) dz = c_a/(-R+1)*(z/a)^(-R+1)*a | = c_a/(-R+1)*a^R*z^(-R+1) |
                           !       /a                                     a                           a
                           !
                           dz     = max(zktop - aksu,eps10)                ! not guaranteed to be >0
                           cavg1   = (ceavg/(apower+1.0d0)) * aksu**(-apower)
                           cavg2   = zktop**(apower+1.0d0) - aksu**(apower+1.0d0)
                           cavg    = cavg1 * cavg2 / dz
                           !
                           ! The corresponding effective suspended load flux is
                           !
                           cflux   = u1(k)*cavg*dz*wu(Lx)
                           !
                           ! Increment the correction by the part of the suspended load flux
                           ! that is in excess of the flux computed above, but never opposite.
                           !
                           if (fluxhortot(ll, k)>0.0d0 .and. cflux>0.0d0) then
                              cumflux = cumflux + max(0.0d0, fluxhortot(ll, k)-cflux)
                           elseif (fluxhortot(ll, k)<0.0d0 .and. cflux<0.0_fp) then
                              cumflux = cumflux + min(fluxhortot(ll, k)-cflux, 0.0d0)
                           else
                              cumflux = cumflux + fluxhortot(ll,k)
                           endif
                        endif
                     endif
                     e_scrn(Lx,l) = -cumflux / wu(Lx)
                     !
                     ! bedload will be reduced in case of sediment transport
                     ! over a non-erodible layer (no sediment in bed) in such
                     ! a case, the suspended sediment transport vector must
                     ! also be reduced.
                     !
                     if (e_scrn(Lx,l) > 0.0d0 .and. Lx<lnxi) then
                        e_scrn(Lx,l) = e_scrn(Lx,l)*fixfac(k1, l)      ! to check
                     else
                        e_scrn(Lx,l) = e_scrn(Lx,l)*fixfac(k2, l)      ! binnenpunt
                     endif
                  else
                     e_scrn(Lx,l) = 0.0d0
                  endif
               enddo ! nm
            endif    ! sedtyp = SEDTYP_NONCOHESIVE_SUSPENDED
         enddo       ! l
      endif          ! kmx>0; end of correction for bed/total load
      !       !
      !       if (lsed > 0) then
      !          call dfexchg( e_scrn,   1, lsed, dfloat, nm_pos, gdp)
      !          call dfexchg( e_scrt,   1, lsed, dfloat, nm_pos, gdp)
      !       endif
   endif           ! sus /= 0.0

   do ll = 1, lsed
      j = lstart + ll   ! constituent index
      do L=1,lnx
         e_ssn(L, ll) = 0d0
         call getLbotLtop(L,Lb,Lt)
         do iL = Lb,Lt
            e_ssn(L, ll)  = e_ssn(L, ll) + fluxhortot(j,iL)/max(wu(L), epshu)             ! timestep transports per layer [kg/s/m]
         enddo
         e_ssn(L, ll)  = e_ssn(L, ll) + e_scrn(L, ll)  ! bottom layer correction
      enddo
   enddo
   !
   ! if morphological computations have started
   !
   if (time1 >= tstart_user + tmor * tfac) then   ! tmor in tunit since start of computations, time1 in seconds since reference date
      !
      ! Increment morphological time
      ! Note: dtmor in seconds, morft in days!
      !
      morft = morft + dtmor/86400.0d0
      if (morfac>0d0) hydrt  = hydrt + dts/86400d0
      if (stmpar%morpar%moroutput%morstats) then
         if (comparereal(time1,ti_seds,eps10)>=0) morstatt0 = morft
      endif
      !
      ! Bed boundary conditions: transport condition
      !
      do jb = 1, nto                                ! no of open bnd sections
         icond = morbnd(jb)%icond
         if (icond == 4 .or. icond == 5) then
            !
            ! Open boundary with transport boundary condition:
            ! Get data from table file
            ! TO DO: flow_getexternalforcing voor morfologie
            !
            call gettabledata(bcmfile  , morbnd(jb)%ibcmt(1) , &
               & morbnd(jb)%ibcmt(2) , morbnd(jb)%ibcmt(3) , &
               & morbnd(jb)%ibcmt(4) , bc_mor_array        , &
               & timhr      ,julrefdat  , msg        )
            !
            ! Prepare loop over boundary points
            !
            tausum2(1) = 0d0
            do ib = 1, morbnd(jb)%npnt
               lm = morbnd(jb)%lm(ib)
               k2 = morbnd(jb)%nxmx(ib)
               if (jampi == 1) then
                  if (.not. (idomain(k2) == my_rank)) cycle    ! internal cells at boundary are in the same domain as the link
               endif
               if( u1(lm) < 0.0d0 ) cycle                     ! to do: 3d
               call gettau(k2,taucurc,czc)
               tausum2(1) = tausum2(1) + taucurc**2            ! sum of the shear stress squared
            enddo                                         ! the distribution of bedload is scaled with square stress
            ! for avoiding instability on BC resulting from uniform bedload
            ! in combination with non-uniform cells.
            li = 0
            do l = 1, lsedtot
               sbsum = 0d0
               !
               ! bed load transport always zero for mud fractions
               !
               if (sedtyp(l) == SEDTYP_COHESIVE) cycle
               li = li + 1
               !
               do ib = 1, morbnd(jb)%npnt
                  lm = morbnd(jb)%lm(ib)
                  k2 = morbnd(jb)%nxmx(ib)
                  if (jampi == 1) then
                     if (.not. (idomain(k2) == my_rank)) cycle
                  endif
                  sbsum = sbsum + bc_mor_array(li) * wu_mor(lm)  ! sum the total bedload flux throughout boundary
               enddo
               bc_sed_distribution(li) = sbsum
            enddo

            ! do MPI reduce step for bc_sed_distribution and tausum2
            if (jampi == 1) then
               call reduce_sum(1, tausum2)
               call reduce_sum(lsedtot, bc_sed_distribution)
            endif

            do ib = 1, morbnd(jb)%npnt
               alfa_dist   = morbnd(jb)%alfa_dist(ib)
               alfa_mag    = morbnd(jb)%alfa_mag(ib)
               !                idir_scalar = morbnd(jb)%idir(ib)
               nm          = morbnd(jb)%nm(ib)
               nxmx        = morbnd(jb)%nxmx(ib)
               lm          = morbnd(jb)%lm(ib)
               !
               ! If the computed transport is directed outward, do not
               ! impose the transport rate (at outflow boundaries the
               ! "free bed level boundary" condition is imposed. This
               ! check is carried out for each individual boundary point.
               !
               ! Detect the case based on the value of nxmx.
               !
               if( u1(lm) < 0.0d0 ) cycle              ! check based on depth averaged velocity value
               !
               ! The velocity/transport points to the left and top are part
               ! of this cell. nxmx contains by default the index of the
               ! neighbouring grid cell, so that has to be corrected. This
               ! correction is only carried out locally since we need the
               ! unchanged nxmx value further down for the bed level updating
               !
               li      = 0
               lsedbed = lsedtot - nmudfrac
               do l = 1, lsedtot
                  !
                  ! bed load transport always zero for mud fractions
                  !
                  if (sedtyp(l) == SEDTYP_COHESIVE) cycle
                  li = li + 1
                  !
                  if (morbnd(jb)%ibcmt(3) == lsedbed) then
                     !rate = bc_mor_array(li)
                     call gettau( ln(2,lm), taucurc, czc )
                     if ( tausum2(1) > 0d0 ) then
                        rate = bc_sed_distribution(li) * taucurc**2 / wu_mor(lm) / tausum2(1)
                     else
                        rate = bc_mor_array(li)
                     endif
                  elseif (morbnd(jb)%ibcmt(3) == 2*lsedbed) then
                     rate = bc_mor_array(li) + &
                        & alfa_dist * (bc_mor_array(li+lsedbed)-bc_mor_array(li))
                  endif
                  rate = alfa_mag * rate
                  !
                  if (icond == 4) then
                     !
                     ! transport including pores
                     !
                     rate = rate*cdryb(l)
                  else
                     !
                     ! transport excluding pores
                     !
                     rate = rate*rhosol(l)
                  endif
                  !
                  ! impose boundary condition
                  !
                  !                   if (idir_scalar == 1) then
                  e_sbn(lm, l) = rate
                  !                   else
                  !                      sbvv(nxmx, l) = rate
                  !                   endif
               enddo ! l (sediment fraction)
            enddo    ! ib (boundary point)
         endif       ! icond = 4 or 5 (boundary with transport condition)
      enddo          ! jb (open boundary)
      !
      ! Update quantity of bottom sediment
      !
      dbodsd = 0d0
      !
      ! compute change in bodsed (dbodsd)
      !
      bedchangemesscount = 0
      do l = 1, lsedtot
         bedload = sedtyp(l)==SEDTYP_NONCOHESIVE_TOTALLOAD
         j = lstart + l   ! constituent index
         !
         ! loop over internal (ndxi) nodes - don't update the boundary nodes
         !
         do nm=1,Ndxi
            trndiv  = 0d0
            sedflx  = 0d0
            eroflx  = 0d0
            if (sus/=0d0 .and. .not. bedload) then
               if (neglectentrainment) then
                  !
                  ! mass balance based on transport fluxes only: entrainment and deposition
                  ! do not lead to erosion/sedimentation.
                  !
                  sumflux = 0d0
                  if (kmx>0) then
                     do ii=1,nd(nm)%lnx
                        LL = nd(nm)%ln(ii)
                        Lf = iabs(LL)
                        call getLbotLtop(Lf,Lb,Lt)
                        flux = 0d0
                        do iL = Lb,Lt
                           flux = flux + fluxhortot(j,iL)
                        enddo
                        ! to check: correct for 3D?
                        if ( LL>0 ) then  ! inward
                           sumflux = sumflux + flux
                        else                 ! outward
                           sumflux = sumflux - flux
                        end if
                     end do
                  else
                     do ii=1,nd(nm)%lnx
                        LL = nd(nm)%ln(ii)
                        Lf = iabs(LL)
                        
                        flux = fluxhortot(j,Lf)

                        if ( LL>0 ) then  ! inward
                           sumflux = sumflux + flux
                        else                 ! outward
                           sumflux = sumflux - flux
                        end if
                     end do
                  endif
                  trndiv = trndiv + sumflux * bai_mor(nm)
               else
                  !
                  ! mass balance includes entrainment and deposition
                  !
                  if (sedtyp(l) == SEDTYP_NONCOHESIVE_SUSPENDED) then
                     !
                     ! l runs from 1 to lsedtot, kmxsed is defined for 1:lsed
                     ! The first lsed fractions are the suspended fractions,
                     ! so this is correct
                     !
                     k = kmxsed(nm, l)
                  else
                     call getkbotktop(nm, kb, kt)
                     k = kb
                  endif
                  !thick0 = vol0(k) * bai_mor(nm)
                  thick1 = vol1(k) * bai_mor(nm)
                  !sedflx = sinkse(nm,l) * m_sediment_sed(l,k) * thick1
                  sedflx = sinksetot(j,nm)*bai_mor(nm)
                  eroflx = sourse(nm,l)                       * thick1         ! mass conservation, different from D3D
                  !
                  ! Update fluff layer
                  !
                  if (iflufflyr>0) then
                     mfluff(l, nm) = mfluff(l, nm) + &
                        & dts*(  sinkftot(j,nm)*bai_mor(nm)   &
                        &      - sourf(l, nm)                  *thick1  )
                  endif
                  !
                  ! add suspended transport correction vector
                  !
                  sumflux = 0d0
                  do ii=1,nd(nm)%lnx
                     LL = nd(nm)%ln(ii)
                     Lf = iabs(LL)
                     flux = e_scrn(Lf,l)*wu(Lf)

                     if ( LL>0 ) then  ! inward
                        sumflux = sumflux + flux
                     else                 ! outward
                        sumflux = sumflux - flux
                     end if
                  end do
                  trndiv = trndiv + sumflux * bai_mor(nm)
               endif
            endif
            if (bed /= 0.0d0) then
               sumflux = 0d0
               do ii=1,nd(nm)%lnx
                  LL = nd(nm)%ln(ii)
                  Lf = iabs(LL)
                  flux = e_sbn(Lf,l)*wu_mor(Lf)

                  if ( LL>0 ) then  ! inward
                     sumflux = sumflux + flux
                  else                 ! outward
                     sumflux = sumflux - flux
                  end if
               end do
               trndiv = trndiv + sumflux * bai_mor(nm)
            endif
            !
            dsdnm = (trndiv+sedflx-eroflx) * dtmor
            !
            ! Warn if bottom changes are very large,
            ! depth change NOT LIMITED
            !
            dhmax = 0.05d0
            h1 = max(0.01d0, s1(nm) - bl(nm))
            if (abs(dsdnm) > dhmax*h1*cdryb(1) .and. bedupd) then
               !
               ! Only write bed change warning when bed updating is true
               ! (otherwise no problem)
               ! Limit the number of messages with bedchangemessmax
               ! (otherwise tri-diag will grow very fast)
               !
               bedchangemesscount = bedchangemesscount + 1
               if (bedchangemesscount <= bedchangemessmax) then
                  write (mdia, '(a,f5.1,a,i0,a,i0,a)') &
                     & '*** WARNING Bed change exceeds ' , dhmax*100.0d0, ' % of waterdepth after ', int(dnt),  &
                     & ' timesteps, flow node = (', nm,')'
               endif
            endif
            !
            ! Update dbodsd value at nm
            !
            dbodsd(l, nm) = dbodsd(l, nm) + dsdnm
         enddo    ! nm
      enddo       ! l
      !
      if (bedchangemesscount > bedchangemessmax) then
         write (mdia,'(12x,a,i0,a)') 'Bed change messages skipped (more than ',bedchangemessmax,')'
         write (mdia,'(12x,2(a,i0))') 'Total number of Bed change messages for timestep ', int(dnt), ' : ',bedchangemesscount
      endif
      !
      call fluff_burial(stmpar%morpar%flufflyr, dbodsd, lsed, lsedtot, 1, ndxi, dts, morfac)
      !
      ! Re-distribute erosion near dry and shallow points to allow erosion
      ! of dry banks
      !
      do nm = 1, ndxi
         !
         ! If this is a cell in which sediment processes are active then ...
         !
         !if (kcs(nm)*kfs(nm)*kfsed(nm) /= 1) cycle
         if (kfsed(nm) /= 1 .or. hs(nm)<epshs) cycle                    ! check whether sufficient as condition
         !
         totdbodsd = 0d0
         do l = 1, lsedtot
            totdbodsd = totdbodsd + real(dbodsd(l, nm), hp)
         enddo
         !
         ! If this is a cell where erosion is occuring (accretion is not
         ! distributed to dry points) then...
         !
         if (totdbodsd < 0d0) then
            !
            ! Note: contrary to the previous implementation, this new
            ! implementation erodes the sediment from nm and
            ! re-distributes the eroded volume based on the composition
            ! of the neighbouring cells, replenishing the sediment volume
            ! at grid point nm with sediment of a different composition
            ! than that what was eroded. This new implementation is mass
            ! conserving per fraction. Furthermore, re-distribution takes
            ! place only in case of net TOTAL erosion, i.e. not of
            ! individual fractions.
            !
            bamin      = ba(nm)
            totfixfrac = 0d0
            !
            do L=1,nd(nm)%lnx
               k1 = ln(1,iabs(nd(nm)%ln(L))); k2 = ln(2,iabs(nd(nm)%ln(L)))
               if (k2 == nm) then
                  knb = k1
               else
                  knb = k2
               end if
               !
               ! evaluate whether dry cell, and calculate totfixfac value for cell
               !
               if (kfsed(knb)==0 .and. bl(knb)>bl(nm)) then
                  bamin = min(bamin, ba(knb))
                  do ll = 1, lsedtot
                     totfixfrac = totfixfrac + fixfac(knb, ll)*frac(knb, ll)
                  end do
               end if
            end do
            !
            !
            ! Re-distribute THET % of erosion in nm to surrounding cells
            ! THETSD is a user-specified maximum value, range 0-1
            !
            if (totfixfrac > 1d-7) then
               !
               ! Compute local re-distribution factor THET
               !
               if (hmaxth > sedthr) then
                  thet = (hs(nm) - sedthr)/(hmaxth - sedthr)*thetsd
                  thet = min(thet, thetsd)
               else
                  thet = thetsd
               end if
               !
               ! Combine some constant factors in variable THET
               ! Note: TOTDBODSD<0.0 and thus THET>0.0 !
               !
               thet = -bamin * totdbodsd * thet / totfixfrac
               !
               do ll = 1, lsedtot
                  !
                  ! update dbodsd values in this cell and surrounding cells
                  ! adjust bedload transport rates to include this erosion
                  ! process.
                  !
                  do L=1,nd(nm)%lnx
                     k1 = ln(1,iabs(nd(nm)%ln(L))); k2 = ln(2,iabs(nd(nm)%ln(L)))
                     Lf = iabs(nd(nm)%ln(L))
                     if (k2 == nm) then
                        knb = k1
                     else
                        knb = k2
                     end if
                     if (kfsed(knb)==0 .and. bl(knb)>bl(nm)) then
                        dv              = thet * fixfac(knb, ll)*frac(knb, ll)
                        dbodsd(ll, knb) = dbodsd(ll, knb) - dv*bai_mor(knb)
                        dbodsd(ll, nm)  = dbodsd(ll, nm)  + dv*bai_mor(nm)
                        e_sbn(Lf,ll)    = e_sbn(Lf,ll)    + dv/(dtmor*wu_mor(Lf)) * sign(1d0,nd(nm)%ln(L)+0d0)
                     end if
                  end do ! L
               enddo ! ll
            endif    ! totfixfrac > 1.0e-7
         endif       ! totdbodsd < 0.0
      enddo          ! nm

      if ( jampi.gt.0 ) then
         call update_ghosts(ITYPE_Sall, lsedtot, Ndx, dbodsd, ierror)
      end if
      !
      call reconstructsedtransports()   ! reconstruct cell centre transports for morstats and cumulative st output
      call collectcumultransports()     ! Always needed, written on last timestep of simulation
      !
      if (stmpar%morpar%moroutput%morstats .and. ti_sed>0d0) then
         call morstats(dbodsd, hs_mor, ucxq_mor, ucyq_mor, sbcx, sbcy, sbwx, sbwy, sscx, sscy, sswx, sswy)
      endif
      !
      ! Apply erosion and sedimentation to bookkeeping system
      !
      if (cmpupd) then
         !
         ! Determine new thickness of transport layer
         !
         call compthick()
         !
         ! Update layers and obtain the depth change
         !
         if (updmorlyr(stmpar%morlyr, dbodsd, blchg, mtd%messages) /= 0) then
            call writemessages(mtd%messages, mdia)
            !            to replace by "nice" exit
            write(errmsg,'(a,a,a)') 'fm_bott3d :: updmorlyr returned an error.'
            call write_error(errmsg, unit=mdia)
            error = .true.
            return
         else
            call writemessages(mtd%messages, mdia)
         endif
         call lyrdiffusion(stmpar%morlyr, dtmor)
         !
         ! Apply composition boundary conditions
         !
         call bndmorlyr( lsedtot, timhr, nto, bc_mor_array, stmpar )
      else
         !
         ! Compute bed level changes without actually updating the bed composition
         !
         blchg = 0d0
         do ll = 1, lsedtot
            do nm = 1, ndx
               blchg(nm) = blchg(nm) + dbodsd(ll, nm)/cdryb(ll)
            enddo
         enddo
      endif
      !
      ! Bed boundary conditions
      !
      nto = size(morbnd,1)
      do jb = 1, nto
         icond = morbnd(jb)%icond
         !
         ! In case of an open boundary with bed level condition
         ! described by time series: get data from table file
         !
         if (icond == 2 .or. icond == 3 .or. icond == 6 .or. icond == 7) then
            call gettabledata(bcmfile  , morbnd(jb)%ibcmt(1)    , &
               & morbnd(jb)%ibcmt(2) , morbnd(jb)%ibcmt(3)    , &
               & morbnd(jb)%ibcmt(4) , bc_mor_array           , &
               & timhr      ,julrefdat  , msg)
            if (msg /= ' ') then
               call setmessage(LEVEL_FATAL, msg)
               return
            endif
         endif
         !
         ! Prepare loop over boundary points
         !
         do ib = 1, morbnd(jb)%npnt
            alfa_dist   = morbnd(jb)%alfa_dist(ib)
            alfa_mag    = morbnd(jb)%alfa_mag(ib)**2   !!!!
            !             idir_scalar = morbnd(jb)%idir(ib)
            nm          = morbnd(jb)%nm(ib)
            nxmx        = morbnd(jb)%nxmx(ib)
            lm          = morbnd(jb)%lm(ib)
            !
            !
            ! Bed change in open boundary point
            ! Any boundary condition is changed into a "free bed level
            ! boundary" if the computed transport is directed outward.
            !
            ! Detect the case based on the value of nxmx. In case of a
            ! diagonal water level boundary, there will be two separate
            ! entries in the morbnd structure. The sum of alfa_mag(ib)**2
            ! will be equal to 1.
            !
            if (u1(lm)<0d0) icond = 0
            !
            select case(icond)
            case (0,4,5)
               !
               ! outflow or free boundary (0)
               ! or prescribed transport with pores (4)
               ! or prescribed transport without pores (5)
               !
               blchg(nm) = blchg(nm) + blchg(nxmx) * alfa_mag
            case (1)
               !
               ! fixed bed level: no update
               !
               ! blchg(nm) = blchg(nm) + 0.0 * alfa_mag
            case (2)
               !
               ! prescribed depth
               ! temporarily store "bed levels" in variable "rate"
               !
               if (morbnd(jb)%ibcmt(3) == 1) then
                  rate = bc_mor_array(1)
               elseif (morbnd(jb)%ibcmt(3) == 2) then
                  rate = bc_mor_array(1) + &
                     & alfa_dist * (bc_mor_array(2)-bc_mor_array(1))
               endif
               !
               blchg(nm) = blchg(nm) + (real(-bl(nm),fp)-rate) * alfa_mag
            case (3)
               !
               ! prescribed depth change rate
               !
               if (morbnd(jb)%ibcmt(3) == 1) then
                  rate = bc_mor_array(1)
               elseif (morbnd(jb)%ibcmt(3) == 2) then
                  rate = bc_mor_array(1) + &
                     & alfa_dist * (bc_mor_array(2)-bc_mor_array(1))
               endif
               !
               blchg(nm) = blchg(nm) - rate * alfa_mag * dtmor
            case (6)
               !
               ! prescribed bed level
               ! temporarily store "bed levels" in variable "rate"
               !
               if (morbnd(jb)%ibcmt(3) == 1) then
                  rate = bc_mor_array(1)
               elseif (morbnd(jb)%ibcmt(3) == 2) then
                  rate = bc_mor_array(1) + &
                     & alfa_dist * (bc_mor_array(2)-bc_mor_array(1))
               endif
               !
               blchg(nm) = blchg(nm) + (real(-bl(nm),fp)+rate) * alfa_mag
            case (7)
               !
               ! prescribed bed level change rate
               !
               if (morbnd(jb)%ibcmt(3) == 1) then
                  rate = bc_mor_array(1)
               elseif (morbnd(jb)%ibcmt(3) == 2) then
                  rate = bc_mor_array(1) + &
                     & alfa_dist * (bc_mor_array(2)-bc_mor_array(1))
               endif
               !
               blchg(nm) = blchg(nm) + rate * alfa_mag * dtmor
            end select
         enddo ! ib (boundary point)
      enddo    ! jb (open boundary)
   else
      !
      ! if morphological computations haven't started yet
      !
      do nm = 1, ndx
         blchg(nm) = 0d0
      enddo
   endif ! nst >= itmor
   !
   ! Update bottom elevations
   !
   if (bedupd) then
      !
      call fm_update_crosssections(blchg)
      !
      bl0=bl
      do nm = 1, Ndx
         !
         bl(nm) = bl(nm) + blchg(nm)
         !
      enddo
      ! AvD: Sander suggestie: call update_geom(2)
      !
      ! Free morpho boundaries get Neumann update
      !
      do jb = 1, nto
         icond = morbnd(jb)%icond
         if (icond .eq. 0) then
            do ib = 1, morbnd(jb)%npnt
               bl(morbnd(jb)%nm(ib))    = bl(morbnd(jb)%nxmx(ib))
               blchg(morbnd(jb)%nm(ib)) = blchg(morbnd(jb)%nxmx(ib))  ! needed below
            end do
         end if
      end do
      !
      ! JRE+BJ: Update concentrations in water column to conserve mass because of bottom update
      ! This needs to happen in work array sed, not constituents, because of copying back and forth later on
      ! To do: 3D
      if (kmx==0) then
         do ll = 1, stmpar%lsedsus
            m_sediment_sed(ll,:) = m_sediment_sed(ll,:) * hs / max(hs - blchg, 1d-10)
         enddo
      else
         do ll = 1, stmpar%lsedsus       ! works for sigma only
            do k=1,ndx
               call getkbotktop(k,kb,kt)
               do kk=kb,kt
                  m_sediment_sed(ll,kk) = m_sediment_sed(ll,kk) * hs(k) / max(hs(k) - blchg(k), 1d-10)
               enddo
            enddo
         enddo
      endif
      !
      ! Placeholders, question is whether we need this here:
      !if (jasal>0) then
      !   ! to do
      !endif
      !!
      !if (jatem>0) then
      !   ! to do
      !end if
      !!
      !if (TRA1>0) then
      !   ! you guessed it...
      !endif

      !
      do nm = 1, ndx
         !
         ! note: if kcs(nm)=0 then blchg(nm)=0.0
         ! should change to following test because blchg may be small
         ! due to truncation errors
         !
         s1(nm) = max(s1(nm), bl(nm))
         s0(nm) = max(s0(nm), bl(nm))
         !
         ! if dry cells are eroded then bring water level down to
         ! bed or maximum water level in surrounding wet cells
         ! (whichever is higher)
         !
         if (hs(nm) < epshs .or. jaupdates1==1) then
            s1(nm) = s1(nm) + blchg(nm)
            s0(nm) = s0(nm) + blchg(nm)
         endif
      enddo
      !
      ! Remember erosion velocity for dilatancy
      if (dtmor>0d0) then
         do nm = 1, ndx
            dzbdt(nm) = blchg(nm)/dtmor
         enddo
      else
         dzbdt=0d0
      endif
      !
      ! Dredging and Dumping
      !
      if (dad_included) then
         call fm_dredge(error)
         if (error) then
            call mess(LEVEL_FATAL, 'Error in fm_bott3d :: fm_dredge returned an error.')
            return
         end if
      endif
   endif

   end subroutine fm_bott3d

   subroutine fm_fallve()
   !!--description-----------------------------------------------------------------
   !
   !    Function: Relation between sediment concentration
   !              and vertical fall velocity. Model for
   !              hindered settling.
   !              Fall velocity at layer interfaces.
   !!--declarations----------------------------------------------------------------
   use precision
   use m_physcoef, only: ee, ag, sag, vonkar, frcuni, backgroundsalinity, backgroundwatertemperature, rhomean
   use m_sediment, only: stmpar, sedtra, stm_included, mtd, vismol
   use m_flowtimes, only: time1
   use m_flowgeom, only: ndx, ln, kfs,bl, wcl, lnx
   use m_flow    , only: ifrctypuni, z0, hs, iturbulencemodel,kbot,ktop,kmx,zws,ucxq,ucyq,sa1,tem1,ucx,ucy,ucz,ndkx,s1,z0urou,ifrcutp,hu,frcu
   use m_flowparameters, only: jasal, jatem, jawave, epshs
   use m_transport, only: constituents, ised1
   use m_turbulence, only:turkinepsws, rho
   use morphology_data_module
   use message_module, only: write_error
   use unstruc_files, only: mdia
   use m_alloc
   use m_fm_erosed, only: ucxq_mor, ucyq_mor
   !
   implicit none
   !
   real(fp), dimension(:)             , pointer :: localpar
   real(fp), dimension(:,:)           , pointer :: ws
   real(fp)                           , pointer :: csoil
   real(fp)           , dimension(:)  , pointer :: rhosol
   real(fp)         , dimension(:,:)  , pointer :: dss
   real(fp)           , dimension(:)  , pointer :: sedd50
   real(fp)           , dimension(:)  , pointer :: sedd50fld

   character(256)     , dimension(:)  , pointer :: dll_usrfil
   character(256)     , dimension(:)  , pointer :: dll_function
   integer(pntrsize)  , dimension(:)  , pointer :: dll_handle
   integer            , dimension(:)  , pointer :: iform_settle
   real(fp)           , dimension(:,:), pointer :: par_settle
   !
   integer                            , pointer :: max_integers
   integer                            , pointer :: max_reals
   integer                            , pointer :: max_strings
   integer            , dimension(:)  , pointer :: dll_integers
   real(hp)           , dimension(:)  , pointer :: dll_reals
   character(256)     , dimension(:)  , pointer :: dll_strings
   !
   ! Global variables
   integer                            , pointer :: lsed   
   double precision, allocatable  , dimension(:):: ctot     
   !                                                              
   ! Local variables
   !
   integer                             :: k, kk, k1, k2, L, ll, nm, i, istat, kb, kt
   logical                             :: error

   double precision                    :: chezy
   double precision                    :: cz
   double precision                    :: cz_dum
   double precision                    :: h0
   double precision                    :: rhoint
   double precision                    :: salint
   double precision                    :: temint
   double precision                    :: tka
   double precision                    :: tkb
   double precision                    :: tkt
   double precision                    :: tur_eps
   double precision                    :: tur_k
   double precision                    :: u
   double precision, allocatable, dimension(:)   :: um
   double precision                    :: v
   double precision, allocatable, dimension(:)   :: vm
   double precision                    :: w
   double precision                    :: wsloc
   double precision                    :: z00
   double precision, dimension(:), allocatable    :: z0rou
   double precision                    :: thick

   character(256)              :: errmsg
   !!
   !!! executable statements -------------------------------------------------------
   !!
   call realloc(ctot, ndkx, keepExisting=.false., fill=0d0)
   call realloc(um, ndx, keepExisting=.false., fill=0d0)
   call realloc(vm, ndx, keepExisting=.false., fill=0d0)
   call realloc(z0rou,ndx, keepExisting=.false., fill=0d0)

   csoil               => stmpar%sedpar%csoil
   rhosol              => stmpar%sedpar%rhosol
   dss                 => stmpar%sedpar%dss
   sedd50              => stmpar%sedpar%sedd50
   sedd50fld           => stmpar%sedpar%sedd50fld

   dll_usrfil          => stmpar%trapar%dll_usrfil_settle
   dll_function        => stmpar%trapar%dll_function_settle
   dll_handle          => stmpar%trapar%dll_handle_settle
   iform_settle        => stmpar%trapar%iform_settle
   par_settle          => stmpar%trapar%par_settle

   max_integers        => stmpar%trapar%max_integers_settle
   max_reals           => stmpar%trapar%max_reals_settle
   max_strings         => stmpar%trapar%max_strings_settle
   dll_integers        => stmpar%trapar%dll_integers_settle
   dll_reals           => stmpar%trapar%dll_reals_settle
   dll_strings         => stmpar%trapar%dll_strings_settle

   lsed                => stmpar%lsedsus
   ws                  => mtd%ws
   !    !
   allocate (localpar (stmpar%trapar%npar), stat = istat)
   !
   !
   do k = 1, ndx
      !
      ! bulk mass concentration from previous timestep
      !
      if (kfs(k) > 0) then
         call getkbotktop(k, kb, kt)
         do kk = kb, kt
            do ll = 1, lsed
               ctot(kk) = ctot(kk) + constituents(ISED1 - 1 +ll, kk)
            end do ! kk
         end do
      end if
   end do

   ! calculate mean velocity in nodes
   call setucxucyucxuucyu()

   if (kmx>0) then                       ! 3D
      um = 0d0; vm = 0d0
      do k = 1, ndx
         h0 = max(s1(k)-bl(k), epshs)
         call getkbotktop(k, kb, kt)
         do kk = kb, kt
            thick = zws(kk) - zws(kk-1)
            um(k) = um(k) + thick/h0*ucxq(kk)     
            vm(k) = vm(k) + thick/h0*ucyq(kk)
         end do
      end do
   else
      um   = ucxq                       ! discharge based velocities
      vm   = ucyq
   end if

   ! Calculate roughness height
   if (jawave<3) then  ! current related, potentially including trachy
      do L=1, lnx
         k1 = ln(1,L); k2 = ln(2,L)
         if (frcu(L)>0d0) then
            call getczz0(hu(L), frcu(L), ifrcutp(L), cz_dum, z00)
         else
            call getczz0(hu(L), frcuni, ifrctypuni, cz_dum, z00)
         end if
         z0rou(k1) = z0rou(k1) + wcl(1,L)*z00
         z0rou(k2) = z0rou(k2) + wcl(2,L)*z00
      end do
      !
   else      !  wave enhanced roughness
      do L=1,lnx
         k1=ln(1,L); k2=ln(2,L)
         z0rou(k1) = z0rou(k1) + wcl(1,L)*z0urou(L)
         z0rou(k2) = z0rou(k2) + wcl(2,L)*z0urou(L)
      end do
   end if

   do ll = 1, lsed
      !
      do i = 1,stmpar%trapar%npar
         localpar(i) = par_settle(i,ll)
      enddo
      !
      do k = 1, ndx
         if (kfs(k) == 0) cycle           ! inactive point
         !
         h0 = max(s1(k)-bl(k), epshs)
         chezy = sag * log( 1.0d0 + h0/max(1d-8,ee*z0rou(k)) ) / vonkar
         !
         ! loop over the interfaces in the vertical
         !
         if (kmx > 0) then                ! 3D
            call getkbotktop(k, kb, kt)
            do kk = kb, kt-1
               !
               ! Input parameters are passed via dll_reals/integers/strings-arrays
               !
               tka = zws(kk+1) - zws(kk) ! thickness above
               tkb = zws(kk) - zws(kk-1)   ! thickness below
               tkt = tka+tkb
               if (jasal > 0) then
                  salint = max(0.0_fp, (tka*sa1(kk+1) + tkb*sa1(kk)  ) / tkt )
               else
                  salint = backgroundsalinity
               endif
               !                !
               if (jatem > 0) then
                  temint = (  tka*tem1(kk+1) + tkb*tem1(kk)  ) / tkt
               else
                  temint = backgroundwatertemperature
               endif
               !
               rhoint = (tka*rho(kk+1) + tkb*rho(kk)) / tkt
               !
               u = (tka*ucx(kk+1)+tkb*ucx(kk)) / tkt   ! x component
               v = (tka*ucy(kk+1)+tkb*ucy(kk)) / tkt   ! y component
               w = (tka*ucz(kk+1)+tkb*ucz(kk)) / tkt   ! z component

               if (iturbulencemodel == 3) then     ! k-eps
                  tur_k = turkinepsws(1,kk)
               else
                  tur_k = -999.0d0
               endif
               if (iturbulencemodel == 3) then
                  tur_eps = turkinepsws(2,kk)
               else
                  tur_eps = -999.0d0
               endif
               !
               ! Pass to DLL, decoupling because of treatment per layer interface
               !
               if (max_reals < WS_MAX_RP) then
                  write(errmsg,'(a,a,a)') 'Insufficient space to pass real values to settling routine.'
                  call write_error(errmsg, unit=mdia)
                  error = .true.
                  return
               endif
               dll_reals(WS_RP_TIME ) = real(time1  ,hp)
               dll_reals(WS_RP_ULOC ) = real(u      ,hp)
               dll_reals(WS_RP_VLOC ) = real(v      ,hp)
               dll_reals(WS_RP_WLOC ) = real(w      ,hp)
               dll_reals(WS_RP_SALIN) = real(salint ,hp)
               dll_reals(WS_RP_TEMP ) = real(temint ,hp)
               dll_reals(WS_RP_RHOWT) = real(rhoint ,hp)
               dll_reals(WS_RP_CFRCB) = real(constituents(ised1-1+ll, kk),hp)
               dll_reals(WS_RP_CTOT ) = real(ctot(kk),hp)
               dll_reals(WS_RP_KTUR ) = real(tur_k  ,hp)
               dll_reals(WS_RP_EPTUR) = real(tur_eps,hp)
               if (sedd50(ll)<0.0_fp) then
                  dll_reals(WS_RP_D50) = real(sedd50fld(k),hp)
               else
                  dll_reals(WS_RP_D50) = real(sedd50(ll),hp)
               endif
               dll_reals(WS_RP_DSS  ) = real(dss(k,ll) ,hp)
               dll_reals(WS_RP_RHOSL) = real(rhosol(ll) ,hp)
               dll_reals(WS_RP_CSOIL) = real(csoil      ,hp)
               dll_reals(WS_RP_GRAV ) = real(ag         ,hp)
               dll_reals(WS_RP_VICML) = real(vismol     ,hp)
               dll_reals(WS_RP_WDEPT) = real(h0         ,hp)
               dll_reals(WS_RP_UMEAN) = real(um(k)      ,hp)
               dll_reals(WS_RP_VMEAN) = real(vm(k)      ,hp)
               dll_reals(WS_RP_CHEZY) = real(chezy      ,hp)
               !
               if (max_integers < WS_MAX_IP) then
                  write(errmsg,'(a,a,a)') 'Insufficient space to pass integer values to settling routine.'
                  call write_error(errmsg, unit=mdia)
                  error = .true.
                  return
               endif
               dll_integers(WS_IP_NM  ) = kk
               dll_integers(WS_IP_ISED) = ll
               !
               if (max_strings < WS_MAX_SP) then
                  write(errmsg,'(a,a,a)') 'Insufficient space to pass strings to settling routine.'
                  call write_error(errmsg, unit=mdia)
                  error = .true.
                  return
               endif
               dll_strings(WS_SP_USRFL) = dll_usrfil(ll)
               !             !
               call eqsettle(dll_function(ll), dll_handle(ll), max_integers, max_reals, max_strings, &
                           & dll_integers, dll_reals, dll_strings, mdia, iform_settle(ll),  &
                           & localpar, stmpar%trapar%npar, wsloc, error)
               if (error) then
                  write(errmsg,'(a,a,a)') 'fm_fallve:: Error from eqsettle.'
                  call write_error(errmsg, unit=mdia)
                  error = .true.
                  return
               endif
               !
               ws(kk, ll) = wsloc
            end do
            if (kmx>1) then
               ws(kb-1,ll) = ws(kb,ll)     ! to check
            endif
         else                           ! 2D
            if (jasal > 0) then
               salint = max(0d0, sa1(k))
            else
               salint = backgroundsalinity
            endif
            !             !
            if (jatem > 0) then
               temint = tem1(k)
            else
               temint = backgroundwatertemperature
            endif
            !
            rhoint = rhomean   ! JRE to do: depends on idensform
            !
            u       = ucx(k)   ! x component
            v       = ucy(k)   ! y component
            w       = -999d0   ! z component
            tur_k   = -999d0
            tur_eps = -999d0
            !
            ! Pass to DLL
            !
            if (max_reals < WS_MAX_RP) then
               write(errmsg,'(a,a,a)') 'Insufficient space to pass real values to settling routine.'
               call write_error(errmsg, unit=mdia)
               error = .true.
               return
            endif
            dll_reals(WS_RP_TIME ) = real(time1  ,hp)
            dll_reals(WS_RP_ULOC ) = real(u      ,hp)
            dll_reals(WS_RP_VLOC ) = real(v      ,hp)
            dll_reals(WS_RP_WLOC ) = real(w      ,hp)
            dll_reals(WS_RP_SALIN) = real(salint ,hp)
            dll_reals(WS_RP_TEMP ) = real(temint ,hp)
            dll_reals(WS_RP_RHOWT) = real(rhoint ,hp)
            dll_reals(WS_RP_CFRCB) = real(constituents(ised1-1+ll, k),hp)
            dll_reals(WS_RP_CTOT ) = real(ctot(k),hp)
            dll_reals(WS_RP_KTUR ) = real(tur_k  ,hp)
            dll_reals(WS_RP_EPTUR) = real(tur_eps,hp)
            if (sedd50(ll)<0.0_fp) then
               dll_reals(WS_RP_D50) = real(sedd50fld(k),hp)
            else
               dll_reals(WS_RP_D50) = real(sedd50(ll),hp)
            endif
            dll_reals(WS_RP_DSS  ) = real(dss(k,ll) ,hp)
            dll_reals(WS_RP_RHOSL) = real(rhosol(ll) ,hp)
            dll_reals(WS_RP_CSOIL) = real(csoil      ,hp)
            dll_reals(WS_RP_GRAV ) = real(ag         ,hp)
            dll_reals(WS_RP_VICML) = real(vismol     ,hp)
            dll_reals(WS_RP_WDEPT) = real(h0         ,hp)
            dll_reals(WS_RP_UMEAN) = real(um(k)      ,hp)
            dll_reals(WS_RP_VMEAN) = real(vm(k)      ,hp)
            dll_reals(WS_RP_CHEZY) = real(chezy      ,hp)
            !
            if (max_integers < WS_MAX_IP) then
               write(errmsg,'(a,a,a)') 'Insufficient space to pass integer values to settling routine.'
               call write_error(errmsg, unit=mdia)
               error = .true.
               return
            endif
            dll_integers(WS_IP_NM  ) = k
            dll_integers(WS_IP_ISED) = ll
            !
            if (max_strings < WS_MAX_SP) then
               write(errmsg,'(a,a,a)') 'Insufficient space to pass strings to settling routine.'
               call write_error(errmsg, unit=mdia)
               error = .true.
               return
            endif
            dll_strings(WS_SP_USRFL) = dll_usrfil(ll)
            !             !
            call eqsettle(dll_function(ll), dll_handle(ll), max_integers, max_reals, max_strings, &
                        & dll_integers, dll_reals, dll_strings, mdia, iform_settle(ll),  &
                        & localpar, stmpar%trapar%npar, wsloc, error)
            if (error) then
               write(errmsg,'(a,a,a)') 'fm_fallve:: Error from eqsettle.'
               call write_error(errmsg, unit=mdia)
               error = .true.
               return
            endif
            !
            ws(k, ll) = wsloc
         end if    ! kmx>0
      enddo        ! k
   enddo           ! ll

   deallocate (localpar, stat = istat)
   end subroutine fm_fallve

   subroutine fm_red_soursin()
   !
   !!--description-----------------------------------------------------------------
   !
   !    Function: Reduces sourse and sink terms to avoid large
   !              bed level changes
   !
   !!--declarations----------------------------------------------------------------
   use precision
   use sediment_basics_module
   use unstruc_files, only: mdia
   use m_flowtimes, only: dts, tstart_user, time1, dnt, tfac
   use m_transport, only: ised1
   use m_flow, only: s0, s1, kmx, zws, zws0
   use m_transport, only: constituents
   use message_module, only: write_warning
   use m_flowgeom
   use m_fm_erosed
   !
   implicit none
   !
   real(fp), dimension(kmx)             :: thick
   !
   ! Local parameters
   !
   integer, parameter  :: reducmessmax = 50
   !
   ! Global variables
   !
   !
   ! Local variables
   !
   integer          :: lstart, reducmesscount, l, ll, nm, kmaxsd, kb, kt
   real(fp)         :: h0, h1, thick0, thick1, dz, reducfac
   character(150)   :: message
   !
   !! executable statements -------------------------------------------------------
   !!
   lstart         = ised1-1
   reducmesscount = 0
   !
   do l = 1, lsed
      ll = lstart + l
      !
      ! Reduction is not applied to mud and not to bedload
      !
      if (sedtyp(l) == SEDTYP_NONCOHESIVE_SUSPENDED) then
         if (kmx > 0) then
            do nm = 1, ndx
               call getkbotktop(nm, kb, kt)
               !
               ! Apply reduction factor to source and sink terms if
               ! bottom is closer than user-specified threshold and
               ! erosive conditions are expected
               !
               kmaxsd = kmxsed (nm,l)
               h0     = max(0.01_fp, s0(nm) - bl(nm))
               h1     = max(0.01_fp, s1(nm) - bl(nm))
               thick0 = zws0(kb)-zws0(kb-1) ! should be safe for z and sigma, was: thick(kmaxsd)*h0
               thick1 = zws(kb)-zws(kb-1)   ! thick(kmaxsd)*h1
               dz     = (dts*morfac/cdryb(l)) &
                  & * (sourse(nm, l)*thick0 - (sinkse(nm, l)+sour_im(nm, l))*thick1*constituents(ll, nm))
               if (abs(dz) > h1*dzmax) then
                  reducfac = (h1*dzmax)/abs(dz)
                  if (reducfac < 0.01 .and. (time1 > tstart_user + tmor * tfac) .and. bedupd) then
                     !                   !
                     !                   ! Only write reduction warning when bed updating is true (and started)
                     !                   ! (otherwise no problem)
                     !                   ! Limit the number of messages with reducmessmax
                     !                   ! (otherwise tri-diag will grow very fast)
                     !                   !
                     reducmesscount = reducmesscount + 1
                     if (reducmesscount <= reducmessmax) then
                        !call nm_to_n_and_m(nm, n, m, gdp)
                        write(message,'(a,i0,a,f12.2,a,2(i0,a))') &
                           & 'Source and sink term sediment ',l,' reduced with factor', &
                           & 1/reducfac,' node number=(',nm,'), after ', int(dnt) , ' timesteps.'
                        call write_warning(message, unit=mdia)
                     endif
                  endif
                  sourse(nm, l)  = sourse(nm, l) *reducfac
                  sour_im(nm, l) = sour_im(nm, l)*reducfac
                  sinkse(nm, l)  = sinkse(nm, l) *reducfac
               endif
               !
               ! Apply reduction factor to source and sink
               ! terms if bottom is closer than user-specified
               ! threshold and erosive conditions are expected
               !
               !
               ! If erosion conditions are expected then apply
               ! reduction factor to sour and sink terms.
               ! estimate sink term based on previous cell
               ! concentration
               !
               if ((sinkse(nm,l)+sour_im(nm,l))*constituents(ll, nm) < sourse(nm,l)) then
                  sinkse(nm, l)  = sinkse(nm, l) * fixfac(nm,l)
                  sourse(nm, l)  = sourse(nm, l) * fixfac(nm,l)
                  sour_im(nm, l) = sour_im(nm, l)* fixfac(nm,l)
                  rsedeq(nm, l)  = rsedeq(nm, l) * fixfac(nm,l)
               endif
            enddo               ! nm
         else
            do nm = 1, ndx
               !
               h0     = max(0.01_fp, s0(nm) - bl(nm))
               h1     = max(0.01_fp, s1(nm) - bl(nm))
               dz     = (dts*morfac/cdryb(l)) &
                  & * (sourse(nm, l)*h0 - (sinkse(nm, l)+sour_im(nm, l))*h1*constituents(ll, nm))
               !
               if (abs(dz) > h1*dzmax) then
                  reducfac = (h1*dzmax)/abs(dz)
                  if (reducfac < 0.01 .and. (time1 > tstart_user + tmor * tfac) .and. bedupd) then
                     reducmesscount = reducmesscount + 1
                     if (reducmesscount <= reducmessmax) then
                        write(message,'(a,i0,a,f12.2,a,2(i0,a))') &
                           & 'Source and sink term sediment ',l,' reduced with factor', &
                           & 1/reducfac,' node number=(',nm,'), after ', int(dnt) , ' timesteps.'
                        call write_warning(message, unit=mdia)
                     endif
                  endif
                  sourse(nm, l)  = sourse(nm, l) *reducfac
                  sour_im(nm, l) = sour_im(nm, l)*reducfac
                  sinkse(nm, l)  = sinkse(nm, l) *reducfac
               endif
               !
               if ((sinkse(nm,l)+sour_im(nm,l))*constituents(ll, nm) < sourse(nm,l)) then
                  sinkse(nm, l)  = sinkse(nm, l) * fixfac(nm,l)
                  sourse(nm, l)  = sourse(nm, l) * fixfac(nm,l)
                  sour_im(nm, l) = sour_im(nm, l)* fixfac(nm,l)
                  rsedeq(nm, l)  = rsedeq(nm, l) * fixfac(nm,l)
               endif
            enddo            ! nm
         endif               ! kmx
      endif                  ! sedtyp
   enddo                     ! lsedtot
   if (reducmesscount > reducmessmax) then
      write (mdia,'(12x,a,i0,a)') 'Reduction messages skipped (more than ',reducmessmax,')'
      write (mdia,'(12x,a,f5.0,a,i0)') 'Total number of reduction messages for timestep ', &
         & dnt,' : ', reducmesscount
   endif
   !
   end subroutine fm_red_soursin




   !> apply sediment boundary conditions
   subroutine apply_sediment_bc()
   use m_flowgeom
   use m_flow, only: q1
   use m_meteo
   use m_transport, only: ised1, numconst, constituents, ifrac2const
   use m_sediment, only: sedtot2sedsus
   use sediment_basics_module
   use m_fm_erosed
   implicit none

   integer :: j, kb, ki, L, ll, iconst, k, kk, Lb, Lt, LLL

   ! New approach: default Neumann, unless time series available
   !
      ! Find sand fractions
      do ll=1,lsed    ! sediment-fraction index
         if (stmpar%sedpar%sedtyp(sedtot2sedsus(ll))==SEDTYP_NONCOHESIVE_SUSPENDED) then
            j=ll+ISED1-1 ! constituent index
            do LLL=Lnxi+1,Lnx
               call getLbotLtop(LLL,Lb,Lt)
               do L=Lb,Lt
                  kb = ln(1,L); ki = ln(2,L)
                  constituents(j,kb) = constituents(j,ki)
               end do
            end do
         end if
      end do
   ! 
      ! From time series bnd, or 0d0
      do ll = 1, numfracs
         iconst = ifrac2const(ll)
         if (iconst==0) cycle
         if (stmpar%sedpar%sedtyp(sedtot2sedsus(iconst-ISED1+1))==SEDTYP_NONCOHESIVE_SUSPENDED) then
            do k=1,nbndsf(ll)
               LLL = bndsf(ll)%k(3,k)
               call getLbotLtop(LLL,Lb,Lt)
               if ( hu(LLL)>0d0 ) then
                  do L=Lb,Lt
                     kb = ln(1,L); ki = ln(2,L)
                     kk = kmxd*(k-1)+L-Lb+1
                     if ( q1(L)>0 ) then  ! inflow
                        constituents(iconst,kb) = bndsf(ll)%z(kk)
                     else                    ! outflow
                        constituents(iconst,kb) = constituents(iconst,ki)
                     end if
                  end do
               else
                  !                 set other values (e.g. dry links)
                  do L=Lb,Lb+kmxL(LLL)-1
                     kb = ln(1,L)
                     constituents(iconst,kb) = 0d0
                  end do
               end if
            end do
         end if
      end do
   !

   !
      ! Find mud fractions
      do ll=1,lsed    ! sediment-fraction index
         if (stmpar%sedpar%sedtyp(sedtot2sedsus(ll))==SEDTYP_COHESIVE) then
            j=ll+ISED1-1 ! constituent index
            do LLL=Lnxi+1,Lnx
               call getLbotLtop(LLL,Lb,Lt)
               do L=Lb,Lt
                  kb = ln(1,L); ki = ln(2,L)
                  constituents(j,kb) = constituents(j,ki)
               end do
            end do
         end if
      end do
   !
      ! From time series bnd, or 0d0
      do ll = 1, numfracs
         iconst = ifrac2const(ll)   ! allow for combo equilibrium/dirichlet bc concentrations
         if (iconst==0) cycle
         if (stmpar%sedpar%sedtyp(sedtot2sedsus(iconst-ISED1+1))==SEDTYP_COHESIVE) then
            do k=1,nbndsf(ll)
               LLL = bndsf(ll)%k(3,k)
               call getLbotLtop(LLL,Lb,Lt)
               do L=Lb,Lt
                  kb = ln(1,L); ki = ln(2,L)
                  kk =  kmxd*(k-1)+L-Lb+1
                  if ( q1(L)>0 ) then     ! inflow
                     constituents(iconst,kb) = bndsf(ll)%z(k)
                  else                    ! outflow
                     constituents(iconst,kb) = constituents(iconst,ki)
                  end if
               end do
            end do
         end if
      end do
   !
   end subroutine apply_sediment_bc

   subroutine fm_adjust_bedload(sbn, sbt, avalan)
   use m_physcoef, only: ag
   use m_sferic, only: pi
   use m_flowparameters, only: epshs, epshu
   use m_flowgeom, only: lnxi, lnx, ln, kcs, ba, bl, Dx, wu, wu_mor
   use m_flow, only: hu, hs
   use m_flowtimes
   use m_turbulence, only: rhou
   use precision
   use m_fm_erosed
   use m_alloc

   implicit none

   !!
   !! Global variables
   !!
   logical                               ,          intent(in)           :: avalan
   real(fp)  , dimension(1:lnx,1:lsedtot),          intent(inout)        :: sbn     !  sbcuu, sbwuu, or sswuu
   real(fp)  , dimension(1:lnx,1:lsedtot),          intent(inout)        :: sbt     !  sbcvv, sbwvv, or sswvv
   real(fp)  , dimension(:)  ,          allocatable                      :: sbncor    ! corrected values
   real(fp)  , dimension(:)  ,          allocatable                      :: sbtcor
   !!
   !! Local variables
   !!
   logical                :: di50spatial
   integer                :: l, Lf, k1, k2, lbot, ltop

   double precision       :: di50, phi, tphi, sbedm, depth, dzdp, dzds, bagnol, alfas
   double precision       :: delta, dmloc, ftheta, hidexploc, shield, sina, cosa, tnorm, frc, fixf
   double precision       :: sbedn, sbedt, tratio, sbedcorr, fnorm, ust2avg, slp, avflux, fac
   double precision       :: eps = 1.0d-6
   !
   !! executable statements -------------------------------------------------------
   !
   ! n: normal to cell face, t: along cell face
   call realloc(sbncor, lnx)    ! corrected values
   call realloc(sbtcor, lnx)

   !
   ! Make assumptions for friction angle
   !
   phi  = 30d0 / 180d0 * pi
   tphi = tan(phi)

   do Lf = 1, Lnx
      if (hu(Lf) > 0d0) then
         k1 = ln(1, Lf)
         k2 = ln(2, Lf)
         call getLbotLtop(Lf, Lbot, Ltop)

         do l = 1, lsedtot
            if (sedtyp(l) /= SEDTYP_COHESIVE) then
               di50 = sedd50(l)
               di50spatial = .false.
               if (di50<0.0_fp .and. lsedtot==1) di50spatial = .true.
               !
               ! Initialize variables
               !
               sbedcorr   = 0d0
               sbtcor(Lf) = 0.0_fp
               sbncor(Lf) = 0.0_fp
               !
               ! calculate bed gradient parallel and perpendicular to BED LOAD
               ! TRANSPORT vector. This exists in the links: e_dzdn, e_dzdt.
               ! Transports also exist in the links: e_sbcn, e_sbct, and so on.
               !
               sbedn    = sbn(Lf, l)      ! e_sxxn
               sbedt    = sbt(Lf, l)      ! e_sxxt
               depth    = hu(Lf)
               sbedm    = sqrt(sbn(Lf, l)**2 + sbt(Lf, l)**2)

               if (sbedm>eps) then
                  dzds =  e_dzdn(Lf)*sbedn/sbedm + e_dzdt(Lf)*sbedt/sbedm ! in direction of transport (not n)
                  dzdp = -e_dzdn(Lf)*sbedt/sbedm + e_dzdt(Lf)*sbedn/sbedm ! perpendicular to transport direction (not t)
                  !
                  ! limit dzdn to 90% of phi
                  !
                  dzds = min(0.9*tphi, dzds)
                  !
                  ! Apply bed slope effect according to
                  !   1: No correction
                  !   2: Bagnold (long. slope) and Ikeda / Van Rijn (transv. slope)
                  !   3: Van Bendegom and Koch & Flokstra
                  !   4: Parker and Andrews
                  !
                  select case (islope)
                  case(1)
                     !
                     ! no correction: default values
                     !
                     sbncor(Lf) = sbedn
                     sbtcor(Lf) = sbedt
                  case(2)
                     !
                     ! adjust bed load for longitudinal bed slope (following Bagnold (1956))
                     ! note alfabs is user-specified scaling parameter
                     !
                     bagnol = tphi / (cos(atan(dzds))*(tphi-dzds))  ! alternative: tphi * sqrt(1d0 + dzdn*dzdn) / (tphi-dzdn)
                     alfas  = 1.0_fp + alfabs*(bagnol-1.0_fp)
                     alfas  = max(0.0_fp , alfas)
                     sbedn  = alfas * sbedn
                     sbedt  = alfas * sbedt
                     !
                     ! adjust bed load for transverse bed slope
                     ! note alfabn is user-specified scaling parameter
                     ! note taurat=(taubcw/taucrb) stored above
                     !
                     tratio = (taurat(k1, l) + taurat(k2, l)) / 2.0_fp

                     if (tratio >= 1.0) then
                        fnorm = alfabn * (1.0/tratio)**0.5 * dzdp
                     else
                        fnorm = alfabn * dzdp
                     endif
                     !
                     ! note adjusted bedload put in temporary array so doesn't influence
                     ! surrounding points
                     !
                     sbncor(Lf) = sbedn - sbedt*fnorm       ! to check
                     sbtcor(Lf) = sbedt + sbedn*fnorm
                  case(3, 4)
                     !                   !
                     ! 3: Formulation according Van Bendegom (1947), Koch & Flokstra (1980)
                     ! as described in Struiksma et al. (1985)
                     !
                     ! 4: Formulation according Parker & Andrews (1985)
                     !
                     ust2avg = (ust2(k1) + ust2(k2)) / 2d0
                     if (di50spatial) then
                        di50 = sqrt(sedd50fld(k1)*sedd50fld(k2))
                     endif
                     delta   = (rhosol(l) - rhou(lbot))/rhou(lbot)
                     shield  = ust2avg/ag/delta/di50
                     !
                     if (shield/=0.0_fp) then
                        if (islope==3) then
                           dmloc = sqrt(dm(k1)*dm(k2))
                           if (comparereal(dmloc,0d0)==0) then
                              if (kcs(k1)>0) then
                                 dmloc = dm(k1)
                              elseif (kcs(k2)>0) then
                                 dmloc = dm(k2)
                              endif
                           endif
                           ftheta  = ashld*(shield**bshld)* &
                              & ((di50/depth)**cshld)*((di50/dmloc)**dshld)
                        else ! islope==4
                           hidexploc = (hidexp(k1, l)+hidexp(k2, l)) / 2.0_fp
                           ftheta    = alfpa * sqrt( shield / &
                              & max(shield*0.1_fp , hidexploc*thcrpa) )
                        endif
                     else
                        ftheta  = 0.0_fp
                     endif
                     !
                     ! deal with exeptional case when ftheta, dzdv and dzdu are exactly
                     ! equal to zero
                     !
                     if (e_dzdt(Lf)/=0.0_fp .or. e_dzdn(Lf)/=0.0_fp) then
                        sina    = ftheta*sbedn/sbedm + e_dzdn(Lf)
                        cosa    = ftheta*sbedt/sbedm + e_dzdt(Lf)
                     else
                        sina    = sbedn/sbedm
                        cosa    = sbedt/sbedm
                     endif
                     tnorm = sqrt(sina**2 + cosa**2)
                     !
                     ! note adjusted bedload put in temporary array so doesn't influence
                     ! surrounding points
                     !
                     sbedm = sbedm * (1.0_fp + alfabs*dzds)
                     sbncor(Lf) = sbedm * (sina/tnorm)
                     sbtcor(Lf) = sbedm * (cosa/tnorm)
                  end select ! islope

                  !               !
                  if (avalan .and. (.not. duneavalan) .and. wetslope<9.99d0) then
                     !
                     ! Avalanching (MvO, 2011-04-06)
                     !
                     ! To be used instead of avalanching routine that is called at the end of BOTT3D.
                     ! Uses a maximum wet slope (keyword WetSlope in the mor file).
                     ! The default for Wetslope is 10.0 (i.e. 10:1, extremely steep, so no avalanching).
                     !
                     ! Sediment flux (avflux) equals volume exchange between two adjacent cells that is required
                     ! to reach maximum allowed slope, divided by avalanching time (1 day). This avalanching time has
                     ! no real physical meaning! The sediment flux due to avalanching is added to the bed load transport.
                     !
                     ! The wet slope should really be a function of sediment characteristics. This has not yet been implemented.
                     !
                     slp = sqrt(e_dzdn(Lf)*e_dzdn(Lf) + e_dzdt(Lf)*e_dzdt(Lf))
                     
                     if (slp>wetslope) then
                        avflux = ba(k1)*ba(k2)/(ba(k1)+ba(k2)) * (bl(k2)-bl(k1) + wetslope*(-e_dzdn(Lf))/slp*Dx(Lf)) / avaltime /morfac
                        sbncor(Lf) = sbncor(Lf) - avflux*rhosol(l)/wu_mor(Lf)
                     end if
                  endif       ! avalan
                  !
                  ! Apply upwind frac and fixfac.
                  !
                  ! At inflow (open, dd, and partition) boundaries the fixfac should not be taken upwind.
                  !
                  if (Lf > lnxi .and. hu(Lf) > epshu) then          ! wet boundary link
                     fixf = fixfac(k2, l)
                     frc  = frac(k2, l)
                  else                                              ! interior link
                     if (sbncor(Lf) >= 0) then
                        fixf = fixfac(k1, l)                        ! outward positive
                        frc  = frac(k1, l)
                     else
                        fixf = fixfac(k2, l)
                        frc  = frac(k2, l)
                     end if
                  end if

                  sbncor(Lf) = sbncor(Lf) * frc * fixf
                  sbtcor(Lf) = sbtcor(Lf) * frc * fixf
                  !
               end if      ! sbedm
            end if         ! SEDTYP
            sbn(Lf, l) = sbncor(Lf)
            sbt(Lf, l) = sbtcor(Lf)
         end do            ! lsedtot
      end if               ! hu
   end do                  ! Lf

   end subroutine fm_adjust_bedload

   !============================================================================================
   ! SEDIMENT CONCENTRATION BOUNDARY SPUL VOOR STM_INCLUDED
   ! Volgt werkwijze tracerranden, met aanpassingen (bv aantal fracties gekend op voorhand)
   !============================================================================================
   subroutine get_sedfracname(qid, sfname, qidname)
   implicit none

   character(len=*), intent(in)  :: qid       !< Original quantityid, e.g., 'sedfracbndsediment1'.
   character(len=*), intent(out) :: sfname    !< The trimmed tracer name, e.g., 'sediment1'.
   character(len=*), intent(inout) :: qidname !< The base quantity name for further use in external forcing, e.g., 'sedfracbnd'.

   sfname = ''

   if ( index(qid,'sedfracbnd') == 1 ) then
      qidname = qid(1:10)
      if ( len_trim(qid)>10 ) then
         sfname = trim(qid(11:))
      else
         sfname = trim('unknown_sediment_fraction')
      end if
   else if (index(qid,'initialsedfrac') == 1 ) then
      qidname = qid(1:14)
      if ( len_trim(qid)>14 ) then
         sfname = trim(qid(15:))
      else
         sfname = trim('unknown_sediment_fraction')
      end if
   else if (index(qid,'initialverticalsedfracprofile') == 1 ) then
      qidname = qid(1:29)
      if ( len_trim(qid)>29 ) then
         sfname = trim(qid(30:))
      else
         sfname = trim('unknown_sediment_fraction')
      end if
   else if (index(qid,'initialverticalsigmasedfracprofile') == 1 ) then
      qidname = qid(1:34)
      if ( len_trim(qid)>34 ) then
         sfname = trim(qid(35:))
      else
         sfname = trim('unknown_sediment_fraction')
      end if   
   end if
   end subroutine get_sedfracname

   subroutine compthick()
   !!--description-----------------------------------------------------------------
   !
   ! Function:   Compute new thickness of transport and exchange layer
   !
   !!--declarations----------------------------------------------------------------
   use precision
   use bedcomposition_module
   use m_flow, only: hs
   use m_flowgeom, only: ndx
   use unstruc_files, only: mdia
   use message_module
   use m_sediment, only: stmpar
   use m_bedform
   !
   implicit none
   !
   ! The following list of pointer parameters is used to point inside the gdp structure
   !
   integer                             , pointer :: ttlform
   integer                             , pointer :: telform
   real(fp)                            , pointer :: ttlalpha
   real(fp)                            , pointer :: ttlmin
   real(fp), dimension(:)              , pointer :: duneheight
   !
   ! Local variables
   !
   integer                                       :: istat
   integer                                       :: nm
   integer                             , pointer :: iunderlyr
   real(fp)         , dimension(:)     , pointer :: thtrlyr
   character(256)                                :: errorstr
   logical                                       :: error
   !
   !! executable statements -------------------------------------------------------
   !
   ttlalpha    => stmpar%morpar%ttlalpha
   ttlmin      => stmpar%morpar%ttlmin
   ttlform     => stmpar%morpar%ttlform
   telform     => stmpar%morpar%telform
   duneheight  => bfmpar%duneheight
   !
   istat = bedcomp_getpointer_integer(stmpar%morlyr,'IUnderLyr',iunderlyr)
   if (istat/=0) then
      errorstr = 'Memory problem in COMPTHICK'
      call write_error(errorstr, unit=mdia)
      error = .true.
      return
   endif
   !
   select case(iunderlyr)
   case(2)
      !
      istat = bedcomp_getpointer_realfp (stmpar%morlyr,'ThTrLyr'  ,thtrlyr  )
      if (istat/=0) then
         errorstr = 'Memory problem in COMPTHICK'
         call write_error(errorstr, unit=mdia)
         error = .true.
         return
      endif
      !
      ! Determine new transport layer thickness
      !
      select case(ttlform)
      case(2)
         !
         ! proportional to water depth
         !
         do nm = 1, ndx
            thtrlyr(nm) = max(ttlalpha*hs(nm),ttlmin)
         enddo
      case(3)
         !
         ! proportional to dune height
         !
         do nm = 1, ndx
            thtrlyr(nm) = max(ttlalpha*duneheight(nm),ttlmin)
         enddo
         case default
         !
         ! nothing to do: constant in time
         !
      endselect
      !
      ! Determine new exchange layer thickness
      !
      select case(telform)
      case(1)
         case default
      endselect
      case default
      !
      ! No active layers: nothing to do
      !
   endselect
   end subroutine compthick

   subroutine reset_sedtra()
   use m_sediment
   use morphology_data_module
   use m_rdstm
   use message_module

   implicit none

   integer            ::   istat

   if (.not. stm_included) return
   istat = clrstm(stmpar)
   call clrsedtra(istat,sedtra)
   if ( associated(mtd%dzbdt)) then
      deallocate(mtd%dzbdt)
      deallocate(mtd%uau)
      deallocate(mtd%rhowat)
      deallocate(mtd%seddif)
      deallocate(mtd%sed)
      deallocate(mtd%ws)
      deallocate(mtd%blchg)

      call clearstack (mtd%messages)
      deallocate(mtd%messages)
   end if
   end subroutine reset_sedtra

   subroutine bndmorlyr( lsedtot, timhr, nto, bc_mor_array, stmpar )
   !!--description-----------------------------------------------------------------
   !
   !    Function: - Apply bed composition boundary conditions
   !
   !!--declarations----------------------------------------------------------------
   use precision
   use bedcomposition_module, only : copybedcomp, setmfrac, setvfrac
   use m_flowtimes,   only: julrefdat
   use table_handles, only: handletype, gettabledata
   use m_fm_erosed,   only: bedbndtype, cmpbndtype
   use m_sediment,    only: stmtype
   !
   implicit none
   !
   ! The following list of pointer parameters is used to point inside the gdp structure
   !
   real(fp)                             , pointer :: bed
   type (handletype)                    , pointer :: bcmfile
   type (bedbndtype), dimension(:)      , pointer :: morbnd
   type (cmpbndtype), dimension(:)      , pointer :: cmpbnd
   character(len=256)                             :: msg
   !
   ! Global variables
   !
   integer                       , intent(in) :: lsedtot
   integer                       , intent(in) :: nto
   real(fp)                                   :: timhr
   real(fp), dimension(2*lsedtot)             :: bc_mor_array
   type(stmtype), intent(in)          :: stmpar
   !
   ! Local variables
   !
   integer  :: icond
   integer  :: ib
   integer  :: jb
   integer  :: l
   integer  :: nm
   integer  :: nxmx
   real(fp) :: alfa_dist
   real(fp) :: bndval
   real(fp) :: sedtot
   real(fp), dimension(lsedtot)         :: frac
   !
   !! executable statements -------------------------------------------------------
   !
   bed                 => stmpar%morpar%bed
   bcmfile             => stmpar%morpar%bcmfile
   morbnd              => stmpar%morpar%morbnd
   cmpbnd              => stmpar%morpar%cmpbnd
   !
   do jb = 1, nto
      icond = cmpbnd(jb)%icond
      !
      ! If composition is fixed, nothing to do. So, we can
      ! continue with next boundary.
      !
      if (icond == 1) cycle
      !
      ! In case of an open boundary with prescribed composition
      ! (either mass or volume fractions): get data from table file
      !
      if (icond == 2 .or. icond == 3) then
         call gettabledata(bcmfile     ,cmpbnd(jb)%ibcmt(1)    , &
            & cmpbnd(jb)%ibcmt(2)    ,cmpbnd(jb)%ibcmt(3)    , &
            & cmpbnd(jb)%ibcmt(4)    ,bc_mor_array           , &
            & timhr      ,julrefdat  ,msg        )
         if (cmpbnd(jb)%ibcmt(3) == lsedtot) then
            do l = 1, lsedtot
               bc_mor_array(lsedtot + l) = bc_mor_array(l)
            enddo
         endif
      endif
      !
      ! Prepare loop over boundary points
      !
      do ib = 1, morbnd(jb)%npnt
         !
         alfa_dist = morbnd(jb)%alfa_dist(ib)
         nm        = morbnd(jb)%nm(ib)
         nxmx      = morbnd(jb)%nxmx(ib)
         !
         if (icond == 0) then
            !
            ! Free composition: copy composition from internal point
            !
            call copybedcomp(stmpar%morlyr, nxmx, nm)
         elseif (icond == 1) then
            !
            ! Fixed composition: no need to update the values
            !
         elseif (icond == 2) then
            !
            ! Prescribed mass fraction; needed volume fraction
            !
            do l = 1, lsedtot
               frac(l) = bc_mor_array(l) + &
                  & alfa_dist * (bc_mor_array(l+lsedtot)-bc_mor_array(l))
            enddo
            call setmfrac(stmpar%morlyr, frac, nm, nm)
         elseif (icond == 3) then
            !
            ! Prescribed volume fraction; needed volume fraction
            !
            do l = 1, lsedtot
               frac(l) = bc_mor_array(l) + &
                  & alfa_dist * (bc_mor_array(l+lsedtot)-bc_mor_array(l))
            enddo
            call setvfrac(stmpar%morlyr, frac, nm, nm)
         endif
      enddo
   enddo
   end subroutine bndmorlyr

   subroutine duneaval(sbn, error)
   use m_fm_erosed
   use m_sediment
   use m_flowgeom
   use m_flow
   use message_module

   implicit none

   logical,                                      intent(out)   :: error
   double precision, dimension(1:lnx,1:lsedtot), intent(inout) :: sbn

   integer                    :: ierr
   integer                    :: k1, k2, L, lsd
   double precision           :: slp, slpmax, avflux, maxflux

   error = .false.

   do lsd = 1, lsedtot
      do L = 1, lnx
         k1 = ln(1,L); k2 = ln(2,L)
         if (hs(k1)>hswitch .or. hs(k2)> hswitch) then
            slpmax = wetslope
         else
            slpmax = dryslope
         end if
         !
         slp = sqrt(e_dzdn(L)*e_dzdn(L)+e_dzdt(L)*e_dzdt(L))
         if (slp>slpmax) then
            avflux = ba(k1)*ba(k2)/(ba(k1)+ba(k2)) * (bl(k2)-bl(k1) + slpmax*e_dzdn(L)/slp*Dx(L)) * (acL(L) * frac(k1,lsd) + (1 - acL(L)) * frac(k2,lsd)) / avaltime / morfac

            maxflux= ba(k1)*ba(k2)/(ba(k1)+ba(k2)) * dzmaxdune / morfac

            if (abs(maxflux) < abs(avflux)) then
               if (avflux > 0 ) then
                  avflux = min(avflux , maxflux)
               else
                  avflux = max(avflux,-maxflux)
               end if
            endif

            sbn(L, lsd) = sbn(L,lsd) - avflux*rhosol(lsd)/wu_mor(L)
         end if
      end do
   end do
   !
   ierr = 0
1234 continue
   end subroutine duneaval

   subroutine reconstructsedtransports()
   ! Reconstructs cell centre transports from link based values for output purposes
   use m_fm_erosed
   use m_flowgeom
   use m_sediment

   implicit none

   integer               :: l, ll, k1, k2, k

   ! init
   sbcx = 0.0_fp
   sbcy = 0.0_fp
   sbwx = 0.0_fp
   sbwy = 0.0_fp
   sscx = 0.0_fp
   sscy = 0.0_fp
   sswx = 0.0_fp
   sswy = 0.0_fp
   sxtot = 0.0_fp
   sytot = 0.0_fp

   do l = 1, lsed
      do ll = 1, lnx
         k1 = ln(1,ll); k2 = ln(2,ll)
         sscx(k1,sedtot2sedsus(l)) = sscx(k1,sedtot2sedsus(l)) + wcx1(ll)*e_ssn(ll,l)
         sscx(k2,sedtot2sedsus(l)) = sscx(k2,sedtot2sedsus(l)) + wcx2(ll)*e_ssn(ll,l)
         sscy(k1,sedtot2sedsus(l)) = sscy(k1,sedtot2sedsus(l)) + wcy1(ll)*e_ssn(ll,l)
         sscy(k2,sedtot2sedsus(l)) = sscy(k2,sedtot2sedsus(l)) + wcy2(ll)*e_ssn(ll,l)
      end do
   end do

   do l = 1, lsedtot
      if (sedtyp(l)/=SEDTYP_COHESIVE) then
         do ll = 1, lnx
            k1 = ln(1,ll); k2 = ln(2,ll)
            ! bed load transports due to currents
            sbcx(k1,l) = sbcx(k1,l) + wcx1(ll)*e_sbcn(ll,l)
            sbcx(k2,l) = sbcx(k2,l) + wcx2(ll)*e_sbcn(ll,l)
            sbcy(k1,l) = sbcy(k1,l) + wcy1(ll)*e_sbcn(ll,l)
            sbcy(k2,l) = sbcy(k2,l) + wcy2(ll)*e_sbcn(ll,l)
            ! bed load transports due to waves
            sbwx(k1,l) = sbwx(k1,l) + wcx1(ll)*e_sbwn(ll,l)
            sbwx(k2,l) = sbwx(k2,l) + wcx2(ll)*e_sbwn(ll,l)
            sbwy(k1,l) = sbwy(k1,l) + wcy1(ll)*e_sbwn(ll,l)
            sbwy(k2,l) = sbwy(k2,l) + wcy2(ll)*e_sbwn(ll,l)
            ! suspended transports due to waves
            sswx(k1,l) = sswx(k1,l) + wcx1(ll)*e_sswn(ll,l)
            sswx(k2,l) = sswx(k2,l) + wcx2(ll)*e_sswn(ll,l)
            sswy(k1,l) = sswy(k1,l) + wcy1(ll)*e_sswn(ll,l)
            sswy(k2,l) = sswy(k2,l) + wcy2(ll)*e_sswn(ll,l)
         end do
      end if

      ! total transports
      do k = 1, ndx
         sxtot(k,l) = sbcx(k,l) + sbwx(k,l) + sswx(k,l) + sscx(k,l)
         sytot(k,l) = sbcy(k,l) + sbwy(k,l) + sswy(k,l) + sscy(k,l)
      enddo
   end do

   end subroutine reconstructsedtransports

   subroutine collectcumultransports()
   use m_flowtimes, only:dts
   use m_flowgeom
   use m_fm_erosed

   implicit none

   integer            :: k, l
   double precision   :: dtmor_

   ! cumulative transports
   dtmor_ = dts*morfac
   do l = 1, lsedtot
      do k = 1,ndx
         sbxcum(k,l) = sbxcum(k,l) + (sbcx(k,l) + sbwx(k,l)) * dtmor_
         sbycum(k,l) = sbycum(k,l) + (sbcy(k,l) + sbwy(k,l)) * dtmor_
         ssxcum(k,l) = ssxcum(k,l) + (sscx(k,l) + sswx(k,l)) * dtmor_
         ssycum(k,l) = ssycum(k,l) + (sscy(k,l) + sswy(k,l)) * dtmor_
      enddo
   enddo

   end subroutine



   subroutine reconstructsedadvvel()
   use m_flowgeom
   use m_transport
   use m_sediment

   implicit none

   integer               :: L, LL, k1, k2, LLL

   ucxsed = 0d0; ucysed=0d0
   qcxsed = 0d0; qcysed=0d0
   xsedflux=0d0; ysedflux=0d0
   do L = 1, lnx
      k1 = ln(1,L); k2 = ln(2,L)
      if (u1sed(L) .ne. 0d0) then
         ucxsed(k1) = ucxsed(k1) + wcx1(L)*u1sed(L)
         ucxsed(k2) = ucxsed(k2) + wcx2(L)*u1sed(L)
         ucysed(k1) = ucysed(k1) + wcy1(L)*u1sed(L)
         ucysed(k2) = ucysed(k2) + wcy2(L)*u1sed(L)
      end if

      if (q1sed(L) .ne. 0d0) then
         qcxsed(k1) = qcxsed(k1) + wcx1(L)*q1sed(L)
         qcxsed(k2) = qcxsed(k2) + wcx2(L)*q1sed(L)
         qcysed(k1) = qcysed(k1) + wcy1(L)*q1sed(L)
         qcysed(k2) = qcysed(k2) + wcy2(L)*q1sed(L)
      end if
      !
      !
      do LL = 1, stmpar%lsedsus
         LLL = ISED1-1+LL
         if (fluxhortot(LLL,L) .ne. 0d0) then
            xsedflux(LLL,k1) = xsedflux(LLL,k1) + wcx1(L)*fluxhortot(LLL,L)
            xsedflux(LLL,k2) = xsedflux(LLL,k2) + wcx2(L)*fluxhortot(LLL,L)
            ysedflux(LLL,k1) = ysedflux(LLL,k1) + wcy1(L)*fluxhortot(LLL,L)
            ysedflux(LLL,k2) = ysedflux(LLL,k2) + wcy2(L)*fluxhortot(LLL,L)
         end if
      end do

   end do

   end subroutine

   ! =================================================================================================
   ! =================================================================================================
   subroutine setucxqucyq()
   use m_fm_erosed, only: ucxq_mor, ucyq_mor, hs_mor
   use m_flowgeom, only: ndx, lnx, lnxi, ln, nd, wcx1, wcx2, wcy1, wcy2, csu, snu, bl
   use m_flow, only: hs, hu, u1, ucxq, ucyq
   use m_flowparameters ,only: jacstbnd, epshs
   use m_sediment, only: stmpar

   implicit none
   integer          :: L, LL, k, k1, k2
   double precision :: wcxu, wcyu, cs, sn, uin
   logical, pointer :: maximumwaterdepth

   maximumwaterdepth => stmpar%morpar%mornum%maximumwaterdepth

   ucxq_mor = 0d0 ; ucyq_mor = 0d0

   if( .not. maximumwaterdepth ) then
      do k = 1,ndx
         ucxq_mor(k) = ucxq(k)
         ucyq_mor(k) = ucyq(k)
         hs_mor(k)   = hs(k)
      enddo
      return
   endif

   do L = 1,lnx
      if (u1(L) == 0d0) cycle
      k1 = ln(1,L) ; k2 = ln(2,L)
      wcxu = wcx1(L)*u1(L)
      ucxq_mor (k1) = ucxq_mor(k1) + wcxu*hu(L)
      wcyu = wcy1(L)*u1(L)
      ucyq_mor (k1) = ucyq_mor(k1) + wcyu*hu(L)
      wcxu = wcx2(L)*u1(L)
      ucxq_mor (k2) = ucxq_mor(k2) + wcxu*hu(L)
      wcyu = wcy2(L)*u1(L)
      ucyq_mor (k2) = ucyq_mor(k2) + wcyu*hu(L)
   enddo

   do L = lnxi+1,lnx
      k1 = ln(1,L) ; k2 = ln(2,L)
      cs = csu(L) ; sn = snu(L)
      if ( jacstbnd == 0 ) then
         uin = ucxq_mor(k2) * cs + ucyq_mor(k2) * sn
         ucxq_mor(k1) = uin * cs
         ucyq_mor(k1) = uin * sn
         bl(k2) = bl(k1)
      else
         ucxq_mor(k1) = ucxq_mor(k2)
         ucyq_mor(k1) = ucyq_mor(k2)
      end if
   enddo

   do k = 1,ndx
      hs_mor(k) = hs(k)
      do L = 1,nd(k)%lnx
         LL = abs( nd(k)%ln(L) )
         hs_mor(k) = max( hs_mor(k), hu(LL) )
      enddo
   enddo

   do k = 1,ndx
      if( hs_mor(k) > epshs) then
         ucxq_mor(k) = ucxq_mor(k) / hs_mor(k)
         ucyq_mor(k) = ucyq_mor(k) / hs_mor(k)
      else
         ucxq_mor(k) = 0d0
         ucyq_mor(k) = 0d0
      endif
   enddo

   end subroutine setucxqucyq

   ! =================================================================================================
   ! =================================================================================================
   subroutine junctionadv()
   use m_flowgeom , only: lnx1d, ln, nd
   use m_flow     , only: q1
   use m_fm_erosed, only: q_zeta
   implicit none
   integer          :: i, L, Li, Lf, La, k
   double precision :: s_l, s_m

   q_zeta = 0d0

   do L = 1,lnx1d                                       ! loop over flow links
      !if (kfu(m)==1) then                                      !.and. kcu(m)==1
      do i = 1,2
         k = ln(i,L)
         do Li = 1,nd(k)%lnx                              ! loop over all flow links for each zeta point
            Lf = nd(k)%ln(Li)
            La = iabs(Lf)
            if (La /= L) then                               ! if (m1 /= current flow link)
               s_l = sign(1d0,Lf+0d0)
               q_zeta(i,L) = q_zeta(i,L) + q1(La) * s_l
            else
               s_m = sign(1d0,Lf+0d0)
            endif
         enddo
         if (nd(k)%lnx == 1) then                           ! if boundary or end node
            q_zeta(i,L)  = q1(L)
         else
            q_zeta(i,L) = ( - s_m * q_zeta(i,L) + ( nd(k)%lnx - 1 ) * q1(L) ) / nd(k)%lnx
         endif
      enddo
   enddo

   end subroutine junctionadv
   
   subroutine fm_mor_maxtimestep()
   use m_flowtimes
   use m_flow
   use m_flowgeom
   use m_sediment
   use m_partitioninfo
   use m_fm_erosed, only: sxtot, sytot, cdryb, morfac, lsedtot

   implicit none

   integer           :: k, k1, k2, kk, L, ised
   double precision  :: dum, sx, sy, sL, dt, dhmax, dtmaxmor

   dtmaxmor = huge(0d0)

   do k = 1, ndx
      do ised = 1, lsedtot
         dum = 0.d0
         do kk = 1, nd(k)%lnx
            L = iabs(nd(k)%ln(kk))
            k1 = ln(1,L)
            k2 = ln(2,L)

            sx = (acL(L)*sxtot(k1,ised) + (1-acL(L))*sxtot(k2,ised))/cdryb(ised)*max(morfac,1d0)
            sy = (acL(L)*sytot(k1,ised) + (1-acL(L))*sytot(k2,ised))/cdryb(ised)*max(morfac,1d0)
            sL = csu(L)*sx + snu(L)*sy

            if (k2 .eq. k) sL = -sL

            if (sL .ge. 0.) then        ! outgoing transport fluxes only
               dum = dum + sL*wu(L)
            end if
         end do
         if (dum > tiny(0d0)) then
            dt = dzbdtmax*ba(k) / max(dum,eps10)   ! safety
            if ( dt.lt.dtmaxmor ) then
               dtmaxmor = dt
            end if
         end if
      end do
   end do

   if ( jampi.eq.1 ) then
      call reduce_double_min(dtmaxmor)
   end if

   if (dtmaxmor > dts) dtmaxmor = dts
   dtmaxmor = dts/ceiling(dts/dtmaxmor)
   !
   dts = dtmaxmor
   dti = 1d0/dts


   end subroutine fm_mor_maxtimestep