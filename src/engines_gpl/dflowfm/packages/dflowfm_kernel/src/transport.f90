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

! $Id: transport.f90 65829 2020-01-21 13:28:31Z kernkam $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/transport.f90 $
!> This subroutine transports an array of scalars.
!> In light of future vectorization, the aim is to:
!>   -use as few module variables as possible,
!>   -do not use if statements within do-loops.

!> updates sedl with
!>     d(h sedl)/dt = -div (q1 sedl) + div (diag(NU) grad sedl) + h ( -sink sedl + source )
!>   solves for each column {k | 1<=k<k=top} an equation of the form
!>     aaj(k) sedj(k-1) + bbj(k) sedj(k) + ccj(k) sedj(k+1) = ddj(k)

subroutine update_constituents(jarhoonly)
   use m_flowgeom,   only: Ndx, Ndxi, Lnxi, Lnx, ln, nd  ! static mesh information
   use m_flow,       only: Ndkx, Lnkx, u1, q1, au, qw, zws, sq, sqi, vol1, kbot, ktop, Lbot, Ltop,  kmxn, kmxL, kmx, viu, vicwws, plotlin, jalts, wsf, jadecaytracers
   use m_flowtimes,  only: dts, ja_timestep_auto
   use m_turbulence, only: sigdifi
   use m_physcoef,   only: dicoww, vicouv, difmolsal
   use m_transport
   use m_flowparameters, only: limtypsa, limtyptm, limtypsed
   use m_alloc
   use m_partitioninfo
   use m_timer
   use unstruc_messages
   use m_sediment,   only: jatranspvel, jased, stmpar, stm_included, mtd
   use m_waves
   use m_fm_wq_processes
   implicit none

   integer :: jarhoonly 
   
   integer :: ierror

   integer                                               :: limtyp  !< limiter type (>0), or first-order upwind (0)
   double precision                                      :: dvoli
   double precision                                      :: dt, dts_store

   integer                                               :: k, LL, L, j, numconst_store,kk,lll,Lb,Lt
   integer                                               :: istep
   integer                                               :: numstepssync
   
   
   if ( NUMCONST.eq.0 ) return  ! nothing to do
   
   ierror = 1
  
   limtyp = max(limtypsa, limtyptm, limtypsed)
   
   if (jarhoonly == 1) then 
      call fill_rho() ; numconst_store = numconst 
   else if (jarhoonly == 2) then 
     ! call fill_ucxucy() ; numconst_store = numconst 
   else
      call fill_constituents(1)
   endif   
   
!  compute areas of horizontal diffusive fluxes divided by Dx
   call comp_dxiAu()
   
!  get maximum transport time step   
   call get_dtmax()
   
   if ( jalts.eq.1 ) then  ! local time-stepping
      call get_ndeltasteps()
   else
      nsubsteps = 1
      ndeltasteps = 1
      numnonglobal = 0
   end if
   
!  store dts
   dts_store = dts

!  set dts to smallest timestep
   dts = dts/nsubsteps
   
   if ( jampi.ne.0 ) then
!     determine at which sub timesteps to update
      if ( limtyp.eq.0 ) then
         numstepssync = max(numlay_cellbased-1,1)  ! one level used in first-order upwind order reconstruction
      else
         numstepssync = max(int(numlay_cellbased/2),1)  ! two levels used in higher-order upwind reconstruction
      end if
   end if
   
   jaupdate = 1
   
   fluxhor    = 0d0  ! not necessary
   sumhorflux = 0d0
   fluxhortot = 0d0
   sinksetot  = 0d0
   sinkftot   = 0d0
   
   do istep=0,nsubsteps-1
      if ( kmx.gt.0 ) then
         fluxver = 0d0
      end if
      
!     BEGIN DEBUG
!      difsedu = 0d0
!      difsedw = 0d0
!     END DEBUG
      
!     BEGIN DEBUG
!      call comp_sq(Ndkx, Lnkx, kbot, ktop, Lbot, Ltop, q1, qw, sq)
!     END DEBUG
      
!     determine which fluxes need to be updated
      if ( nsubsteps.gt.1 ) then
        call get_jaupdatehorflux(nsubsteps, limtyp, jaupdate,jaupdatehorflux)
      end if
      
!     compute horizontal fluxes, explicit part
      if (.not. stm_included) then     ! just do the normal stuff
         call comp_fluxhor3D(NUMCONST, limtyp, Ndkx, Lnkx, u1, q1, au, sqi, vol1, kbot, Lbot, Ltop,  kmxn, kmxL, constituents, difsedu, sigdifi, viu, vicouv, nsubsteps, jaupdate, jaupdatehorflux, ndeltasteps, jaupdateconst,fluxhor, dsedx, dsedy, jalimitdiff, dxiAu)   
      else
         if ( jatranspvel.eq.0 .or. jatranspvel.eq.1 ) then       ! Lagrangian approach
            ! only add velocity asymmetry
            do LL=1,Lnx
               call getLbotLtop(LL,Lb,Lt)                         ! prefer this, as Ltop gets messed around with in hk specials
               do L=Lb,Lt
                  u1sed(L) = u1(L)!+mtd%uau(L)                    ! JRE to do, discuss with Dano
                  q1sed(L) = q1(L)!+mtd%uau(L)*Au(L)
               end do
            end do
         else if (jatranspvel .eq. 2) then                        ! Eulerian approach
!           stokes+asymmetry
            do LL=1,Lnx
               call getLbotLtop(LL,Lb,Lt)
               do L=Lb,Lt
                  u1sed(L) = u1(L)-ustokes(L)  
                  q1sed(L) = q1(L)-ustokes(L)*Au(L)
               end do
            end do
         end if
         call comp_fluxhor3D(NUMCONST, limtyp, Ndkx, Lnkx, u1sed, q1sed, au, sqi, vol1, kbot, Lbot, Ltop,  kmxn, kmxL, constituents, difsedu, sigdifi, viu, vicouv, nsubsteps, jaupdate, jaupdatehorflux, ndeltasteps, noupdateconst, fluxhor, dsedx, dsedy, jalimitdiff, dxiAu)         
!        water advection velocity
         call comp_fluxhor3D(NUMCONST, limtyp, Ndkx, Lnkx, u1,    q1,    au, sqi, vol1, kbot, Lbot, Ltop,  kmxn, kmxL, constituents, difsedu, sigdifi, viu, vicouv, nsubsteps, jaupdate, jaupdatehorflux, ndeltasteps, jaupdateconst, fluxhor, dsedx, dsedy, jalimitdiff, dxiAu)
      end if   
      
      call starttimer(IDEBUG)
      call comp_sumhorflux(NUMCONST, kmx, Lnkx, Ndkx, Lbot, Ltop, fluxhor, sumhorflux)
      call stoptimer(IDEBUG)
      
      if( jased == 4 .and. stmpar%lsedsus > 0 ) then  ! at moment, this function is only required by suspended sediment. Can be extended to other fluxes if necessary
         call comp_horfluxtot()
      endif

      if (jamba > 0) then  ! at moment, this function is only required by waq processes
         call comp_horfluxwaq()
      endif

!     determine which cells need to be updated
      if ( nsubsteps.gt.1 ) then
         call get_jaupdate(istep,nsubsteps,Ndxi,Ndx,ndeltasteps,jaupdate)
      end if

      if ( kmx.lt.1 ) then   ! 2D, call to 3D as well for now
         call solve_2D(NUMCONST, Ndkx, Lnkx, vol1, kbot, ktop, Lbot, Ltop, sumhorflux, fluxver, const_sour, const_sink, nsubsteps, jaupdate, ndeltasteps, constituents, rhs)
      else
         call comp_fluxver( NUMCONST, limtyp, thetavert, Ndkx, kmx, zws, qw, kbot, ktop, constituents, nsubsteps, jaupdate, ndeltasteps, fluxver, wsf)

         call solve_vertical(NUMCONST, ISED1, ISEDN, limtyp, thetavert, Ndkx, Lnkx, kmx,    &
                             zws, qw, vol1, kbot, ktop, Lbot, Ltop,                         &
                             sumhorflux, fluxver, const_sour, const_sink,                   &
                             difsedw, sigdifi, vicwws, nsubsteps, jaupdate, ndeltasteps, constituents, &
                             a, b, c, d, e, sol, rhs)
      end if

      if ( jampi.gt.0 ) then
!        communicate every numstepssync'th or last subtimestep
         if ( mod(istep+1,numstepssync).eq.0 .or. istep+1.eq.nsubsteps ) then
            if ( jatimer.eq.1 ) call starttimer(IUPDSALL)
            if ( kmx.lt.1 ) then ! 2D
               call update_ghosts(ITYPE_Sall, NUMCONST, Ndx, constituents, ierror)
            else                 ! 3D
               call update_ghosts(ITYPE_Sall3D, NUMCONST, Ndkx, constituents, ierror)
            end if
            if ( jatimer.eq.1 ) call stoptimer(IUPDSALL)
         end if
      end if
      
      call comp_sinktot()      
        
   end do
   
   if( jased == 4 .and. stmpar%lsedsus > 0 ) then
      do j = ISED1,ISEDN
         fluxhortot(j,:) = fluxhortot(j,:) / dts_store
         sinksetot(j,:)  = sinksetot(j,:)  / dts_store
         sinkftot(j,:)   = sinkftot(j,:)   / dts_store
      enddo
   endif
   
!!  communicate
!   if ( jampi.gt.0 ) then
!      if ( jatimer.eq.1 ) call starttimer(IUPDSALL)
!      if ( kmx.lt.1 ) then ! 2D
!         call update_ghosts(ITYPE_Sall, NUMCONST, Ndx, constituents, ierror)
!      else                 ! 3D
!         call update_ghosts(ITYPE_Sall3D, NUMCONST, Ndkx, constituents, ierror)
!      end if
!      if ( jatimer.eq.1 ) call stoptimer(IUPDSALL)
!   end if

   if (jarhoonly == 1) then
      call extract_rho() ; numconst = numconst_store 
   else
      if (jadecaytracers > 0) then ! because tracerdecay is normally not done in DFM we do it here so as not to cause overhead elsewhere   
         call decaytracers() 
      endif
      call extract_constituents()
   endif

   ierror = 0
1234 continue

!  restore dts
   dts = dts_store

   return
end subroutine update_constituents

subroutine decaytracers()
use m_transport
use m_flowgeom
use m_flow
use m_flowtimes
double precision :: decaytime
do k = 1,ndkx
   do i=ITRA1,ITRAN
      decaytime = decaytimetracers(i - itra1 + 1)
      if (decaytime > 0) then 
          constituents (i,k) = constituents(i,k) / (1d0 + dts/decaytime)
      endif 
   enddo  
enddo
end subroutine decaytracers 


!> compute horizontal transport fluxes at flowlink
subroutine comp_fluxhor3D(NUMCONST, limtyp, Ndkx, Lnkx, u1, q1, au, sqi, vol1, kbot, Lbot, Ltop, kmxn, kmxL, sed, difsed, sigdifi, viu, vicouv, nsubsteps, jaupdate, jaupdatehorflux, ndeltasteps, jaupdateconst, flux, dsedx, dsedy, jalimitdiff, dxiAu)
   use m_flowgeom,  only: Ndx, Lnx, Lnxi, ln, nd, klnup, slnup, dxi, acl, csu, snu, wcx1, wcx2, wcy1, wcy2, Dx  ! static mesh information
   use m_flowtimes, only: dts, dnt
   use m_flowparameters, only: cflmx
   use m_flow,      only: jadiusp, diusp, dicouv, jacreep, dsalL, dtemL, hu, epshu
   use m_transport, only: ISALT, ITEMP
   use m_missing 
   implicit none

   integer,                                    intent(in)    :: NUMCONST     !< number of transported quantities
   integer,                                    intent(in)    :: limtyp   !< limiter type
   integer,                                    intent(in)    :: Ndkx     !< total number of flownodes (dynamically changing)
   integer,                                    intent(in)    :: Lnkx     !< total number of flowlinks (dynamically changing)
   double precision, dimension(Lnkx),          intent(in)    :: u1       !< flow-field face-normal velocities
   double precision, dimension(Lnkx),          intent(in)    :: q1       !< flow-field discharges
   double precision, dimension(Lnkx),          intent(in)    :: au       !< wet area of flowlinks, note: q1=au*u1
   double precision, dimension(Ndkx),          intent(in)    :: sqi      !< total outward-fluxes at flownodes
   double precision, dimension(Ndkx),          intent(in)    :: vol1     !< volumes
   integer,          dimension(Ndx),           intent(in)    :: kbot     !< flow-node based layer administration
   integer,          dimension(Lnx),           intent(in)    :: Lbot     !< flow-link based layer administration
   integer,          dimension(Lnx),           intent(in)    :: Ltop     !< flow-link based layer administration
   integer,          dimension(Ndx),           intent(in)    :: kmxn     !< flow-link based layer administration
   integer,          dimension(Lnx),           intent(in)    :: kmxL     !< flow-link based layer administration

   double precision, dimension(NUMCONST,Ndkx), intent(in)    :: sed      !< transported quantities
   double precision, dimension(NUMCONST),      intent(in)    :: difsed   !< scalar-specific diffusion coefficent (dicouv)
   real,             dimension(Lnkx),          intent(in)    :: viu      !< spatially varying horizontal eddy viscosity, NOTE: real, not double
   double precision,                           intent(in)    :: vicouv   !< uniform horizontal eddy viscosity
   double precision, dimension(NUMCONST),      intent(in)    :: sigdifi  !< 1/(Prandtl number) for heat, 1/(Schmidt number) for mass
   integer,                                    intent(in)    :: nsubsteps  !< number of substeps
   integer,          dimension(Ndx),           intent(in)    :: jaupdate !< cell updated (1) or not (0)
   integer,          dimension(Lnx),           intent(in)    :: jaupdatehorflux  !< update horizontal flux (1) or not (0)
   integer,          dimension(Ndx),           intent(in)    :: ndeltasteps !< number of substeps between updates
   integer,          dimension(NUMCONST),      intent(in)    :: jaupdateconst   !< update constituent (1) or not (0)
   double precision, dimension(NUMCONST,Lnkx), intent(inout) :: flux     !< adds horizontal advection and diffusion fluxes
   double precision, dimension(NUMCONST,ndkx), intent(inout) :: dsedx    !< grrx 
   double precision, dimension(NUMCONST,ndkx), intent(inout) :: dsedy    !< grry 
   integer,                                    intent(in)    :: jalimitdiff   !< limit diffusion (for time step) (1) or not (0)
   double precision, dimension(Lnkx),          intent(in)    :: dxiAu    !< area of horizontal diffusive flux divided by Dx
  
   
   double precision                                          :: sl1L, sl2L, sl3L, sl1R, sl2R, sl3R
   double precision                                          :: cf, sedkuL, sedkuR, ds1L, ds2L, ds1R, ds2R
                                                             
   double precision                                          :: fluxL, fluxR
   double precision                                          :: sedL, sedR
   double precision                                          :: fluxfac, fluxfacMaxL, fluxfacMaxR
   double precision                                          :: dfac1, dfac2
   double precision                                          :: difcoeff, QL, QR, diuspL, ds1, ds2, dsedn, half
   double precision                                          :: dt_loc
                                                             
   integer                                                   :: j, iswitchL, iswitchR, jahigherL, jahigherR
   integer                                                   :: k1, k2, LL, L, Lb, Lt, laydif, jaL, jaR
   integer                                                   :: kk1L, kk2L, kk1R, kk2R, k1L, k2L, k1R, k2R, is, ku
                                                             
   double precision, external                                :: dlimiter, dlimitercentral
   double precision, external                                :: dlimiter_nonequi

   continue
   
   dt_loc = dts
   
   if (limtyp == 6) then 
     
      dsedx = 0d0; dsedy = 0d0
      do LL = 1,lnx
         Lb = Lbot(LL) ; Lt = Lb - 1 + kmxL(LL)
         do L  = Lb, Lt
            k1 = ln(1,L)
            k2 = ln(2,L)
            do j = 1, Numconst
               dsedn       = dxi(LL)*( sed(j,k2) - sed(j,k1) ) 
               dsedx(j,k1) = dsedx(j,k1) + wcx1(LL)*dsedn 
               dsedy(j,k1) = dsedy(j,k1) + wcy1(LL)*dsedn
               dsedx(j,k2) = dsedx(j,k2) + wcx2(LL)*dsedn 
               dsedy(j,k2) = dsedy(j,k2) + wcy2(LL)*dsedn
           enddo    
        enddo
      enddo
    
    endif    
   
   
!$OMP PARALLEL DO                             &
!$OMP PRIVATE(LL,L,Lb,Lt,kk1L, kk2L, kk1R, kk2R, k1L, k2L, k1R, k2R, iswitchL, iswitchR, sl1L, sl2L, sl3L, sl1R, sl2R, sl3R) &
!$OMP PRIVATE(cf, k1, k2, laydif, j, sedL, sedR, sedkuL, sedkuR, ds1L, ds2L, ds1R, ds2R, jaL, jaR, QL, QR, ds1, ds2, is, ku, dsedn, half ) &
!$OMP FIRSTPRIVATE(dt_loc)
!  advection
   do LL=1,Lnx
   
      if ( nsubsteps.gt.1 ) then
         if ( jaupdatehorflux(LL).eq.0 ) then
            cycle
         else
            dt_loc = dts * min(ndeltasteps(ln(1,LL)),ndeltasteps(ln(2,LL)))
         end if
      else
         dt_loc = dts
      end if
   
      Lb = Lbot(LL)
      Lt = Ltop(LL)   
      
      if (limtyp .ne. 6) then 
!        get the 2D flownodes in the stencil
         
         kk1L = klnup(1,LL)
         iswitchL = 1-min(max(kk1L,0),1)                 ! 1 if kk1L<0, 0 otherwise
         kk2L = (1-iswitchL)*klnup(2,LL) + iswitchL*kk1L ! make kk2L safe for when it is not intented to be used

         kk1R = klnup(4,LL)
         iswitchR = 1-min(max(kk1R,0),1)                 ! 1 if kk1R<0, 0 otherwise
         kk2R = (1-iswitchR)*klnup(5,LL) + iswitchR*kk1R ! make kk2R safe for when it is not intented to be used

         
!        get the weights in the stencil
         sl1L = (dble(1-iswitchL)*slnup(1,LL) + dble(iswitchL)*1d0)
         sl2L = dble(1-iswitchL)*slnup(2,LL)
         sl3L = slnup(3,LL)
         
         sl1R = (dble(1-iswitchR)*slnup(4,LL) + dble(iswitchR)*1d0)
         sl2R = dble(1-iswitchR)*slnup(5,LL)
         sl3R = slnup(6,LL)
         
!        make cell indices safe
!         kk1L = max(iabs(kk1L),1) 
!         kk2L = max(iabs(kk2L),1) 
!         
!         kk1R = max(iabs(kk1R),1) 
!         kk2R = max(iabs(kk2R),1) 

!        make cell indices safe
         kk1L = iabs(kk1L)
         kk2L = iabs(kk2L)
         
         kk1R = iabs(kk1R)
         kk2R = iabs(kk2R)
      endif         

!     loop over vertical flowlinks
      do L=Lb,Lt

!        get left and right neighboring flownodes
         k1 = ln(1,L)
         k2 = ln(2,L)
 
        ! if (limtyp > 0) then 
!           compute Courant number
            cf  =  dt_loc*abs(u1(L))*dxi(LL)   
         
            if ( cf.gt.cflmx ) then
!               write(6,*) cf
               continue
            end if
           
            if (limtyp .ne. 6) then 
               laydif = L-Lb 
            
!              reconstuct from the left and from the right
               jaL = 0
               if ( kk1L.ne.0 ) then
                  k1L = kbot(kk1L) + laydif + kmxn(kk1L) - kmxL(LL)  
                  if ( kk2L.ne.0 ) then
                     k2L = kbot(kk2L) + laydif + kmxn(kk2L) - kmxL(LL)
                     jaL = min(max(k1L-kbot(kk1L)+1,0),1)*min(max(k2L-kbot(kk2L)+1,0),1)*k1L
                  end if
               end if

               jaR = 0
               if ( kk1R.ne.0 ) then
                  k1R = kbot(kk1R) + laydif + kmxn(kk1R) - kmxL(LL)
                  if ( kk2R.ne.0 ) then
                     k2R = kbot(kk2R) + laydif + kmxn(kk2R) - kmxL(LL)
                     jaR = min(max(k1R-kbot(kk1R)+1,0),1)*min(max(k2R-kbot(kk2R)+1,0),1)*k1R
                  end if
               end if
            
            endif 
               
            if ( u1(L) > 0d0 ) then 
               is =  1 ; ku = k1 ; half = acL(LL)
            else   
               is = -1 ; ku = k2 ; half = 1d0 - acl(LL)
            endif   
               
         
!        BEGIN DEBUG
!         if ( dnt.eq.5 .and. ( ( k1.eq.1736 .and. q1(L).gt.0d0 ) .or. ( k2.eq.1736 .and. q1(L).lt.0d0 ) ) ) then
!            continue
!         end if
!        END DEBUG
            
         QL = max(q1(L),0d0)
         QR = min(q1(L),0d0)

         do j=1,NUMCONST
            if ( jaupdateconst(j).ne.1 ) cycle
            
            sedL   = sed(j,k1)
            sedR   = sed(j,k2)
 
            if (Limtyp == 7) then 
                flux(j,L) = q1(L)*0.5d0*(sedR+sedL)    ! central only for cursusdemo
            else if (Limtyp == 6) then
                if ( klnup(1,LL).ne.0 ) then  ! used to detect disabled higher-order
                  ds2 = is*(sedR-sedL)
                  ds1 = is*(dsedx(j,ku)*csu(LL) + dsedy(j,ku)*snu(LL)) * Dx(LL)
                  flux(j,L) = q1(L)* ( sed(j,ku) + half*max(0d0,1d0-cf)*dlimitercentral(ds1, ds2, limtyp) )    
                end if
            else if ( limtyp == 9 ) then  ! MC on non-equidistant mesh
               if ( kk1L.ne.0 .and. q1(L).gt.0d0 .and. jaL.gt.0 ) then
                   sedkuL = sed(j,k1L)*sl1L + sed(j,k2L)*sl2L
                   ds2L =  sed(j,k2) - sed(j,k1)
                   ds1L = (sed(j,k1) - sedkuL)*sl3L
!                   sedL = sedL +      acl(LL) *max(0d0,1d0-cf) * dlimiter_nonequi(ds1L,ds2L,acl(LL),sl3L) * ds2L
                   sedL = sedL +      acl(LL) *max(0d0,1d0-cf) * dlimiter_nonequi(ds1L,ds2L,acl(LL),1d0) * ds2L
               end if
               
                if ( kk1R.ne.0 .and. q1(L).lt.0d0 .and. jaR > 0) then
                   sedkuR = sed(j,k1R)*sl1R + sed(j,k2R)*sl2R
                   ds2R =  sed(j,k1) - sed(j,k2)
                   ds1R = (sed(j,k2) - sedkuR)*sl3R
!                   sedR = sedR + (1d0-acl(LL))*max(0d0,1d0-cf) * dlimiter_nonequi(ds1R,ds2R,1d0-acl(LL),sl3R) * ds2R
                   sedR = sedR + (1d0-acl(LL))*max(0d0,1d0-cf) * dlimiter_nonequi(ds1R,ds2R,1d0-acl(LL),1d0) * ds2R
                end if
               
                flux(j,L) = QL*sedL + QR*sedR
            else  
                
                if ( kk1L.ne.0 .and. q1(L).gt.0d0 .and. jaL > 0) then
                   sedkuL = sed(j,k1L)*sl1L + sed(j,k2L)*sl2L
                   ds2L =  sed(j,k2) - sed(j,k1)
                   ds1L = (sed(j,k1) - sedkuL)*sl3L
                   sedL = sedL +      acl(LL) *max(0d0,1d0-cf) * dlimiter(ds1L,ds2L,limtyp) * ds2L
                end if
               
                if ( kk1R.ne.0 .and. q1(L).lt.0d0 .and. jaR > 0) then
                   sedkuR = sed(j,k1R)*sl1R + sed(j,k2R)*sl2R
                   ds2R =  sed(j,k1) - sed(j,k2)
                   ds1R = (sed(j,k2) - sedkuR)*sl3R
                   sedR = sedR + (1d0-acl(LL))*max(0d0,1d0-cf) * dlimiter(ds1R,ds2R,limtyp) * ds2R
                end if
               
                flux(j,L) = QL*sedL + QR*sedR
               
            endif

            
         end do
      end do

   end do

!$OMP END PARALLEL DO
   
!  BEGIN DEBUG
!   return
!  END DEBUG

!  diffusion
   if (dicouv >= 0d0) then
      
      !$OMP PARALLEL DO                             &
      !$OMP PRIVATE(LL,dfac1,dfac2,Lb,Lt,L,k1,k2,fluxfacMaxL,fluxfacMaxR,j,difcoeff,fluxfac,diuspL) &
      !$OMP FIRSTPRIVATE(dt_loc)
      do LL=1,Lnx
         if ( nsubsteps.gt.1 ) then
            if ( jaupdatehorflux(LL).eq.0 ) then
               cycle
            else
               dt_loc = dts * min(ndeltasteps(ln(1,LL)),ndeltasteps(ln(2,LL)))
            end if
         else
            dt_loc = dts
         end if
      
         if ( jalimitdiff.eq.1 ) then
            !monotinicity criterion, safe for triangles, quad and pentagons, but not for hexahedrons
            !dfac1 = 0.2d0
            !dfac2 = 0.2d0
         
            dfac1 = 1d0/dble(nd(ln(1,LL))%lnx)
            dfac2 = 1d0/dble(nd(ln(2,LL))%lnx)
         end if

         if (jadiusp == 1) then 
             diuspL = diusp(LL)
         else
             diuspL = dicouv
         endif 

         Lb = Lbot(LL)
         Lt = Ltop(LL)
         do L=Lb,Lt
            k1 = ln(1,L)
            k2 = ln(2,L)
            if ( jalimitdiff.eq.1 ) then
               fluxfacMaxL  = dfac1*( vol1(k1)/dt_loc - sqi(k1) )
               fluxfacMaxR  = dfac2*( vol1(k2)/dt_loc - sqi(k2) )
            end if
            do j=1,NUMCONST
               if ( jaupdateconst(j).ne.1 ) cycle
               
               difcoeff  = sigdifi(j)*viu(L) + difsed(j) + diuspL  ! without smagorinsky, viu is 0 , 
                                                                   ! difsed only contains molecular value, 
                                                                   ! so then you only get user specified value  

               fluxfac   = difcoeff*dxiAu(L)
               if ( jalimitdiff.eq.1 ) then
                  fluxfac   = min(fluxfac, fluxfacMaxL, fluxfacMaxR)  ! zie Borsboom sobek note
               end if
               fluxfac   = max(fluxfac, 0d0)
               if (jacreep .ne. 1) then  
                   flux(j,L) = flux(j,L) - fluxfac*(sed(j,k2) - sed(j,k1))
               else
                  if (j == ISALT) then
                     !if (dsalL(L) > 0d0 ) then 
                     !    dsalL(L) =  max(0d0, min(dsalL(L), sed(j,k2) - sed(j,k1) ) ) 
                     !else if (dsalL(L) < 0d0 ) then 
                     !    dsalL(L) =  min(0d0, max(dsalL(L), sed(j,k2) - sed(j,k1) ) ) 
                     !endif
                     flux(j,L) = flux(j,L) - fluxfac*dsalL(L)      
                  else if (j == Itemp) then                 
                     !if (dtemL(L) > 0 ) then 
                     !    dtemL(L) =  max(0d0, min(dtemL(L), sed(j,k2) - sed(j,k1) ) ) 
                     !else if (dtemL(L) < 0 ) then 
                     !    dtemL(L) =  min(0d0, max(dtemL(L), sed(j,k2) - sed(j,k1) ) ) 
                     !endif
                     flux(j,L) = flux(j,L) - fluxfac*dtemL(L)  
                  else  ! SPvdP: I think this was missing
                     flux(j,L) = flux(j,L) - fluxfac*(sed(j,k2) - sed(j,k1))
                  endif
               endif
            end do
         end do
      end do
      !$OMP END PARALLEL DO
   end if
   
   return
end subroutine comp_fluxhor3D


!> compute vertical fluxes
subroutine comp_fluxver(NUMCONST, limtyp, thetavert, Ndkx, kmx, zws, qw, kbot, ktop, sed, nsubsteps, jaupdate, ndeltasteps, flux, wsf)
   use m_flowgeom, only: Ndx, ba, kfs  ! static mesh information
   use m_flowtimes, only: dts
   use m_flowparameters, only: cflmx
   use m_flow, only : hs, epshs, s1, epshsdif, cffacver, a1  ! do not use m_flow, please put this in the argument list
   use m_transport, only : ISED1, ISEDN   ! preferably in argument list
   use m_sediment,  only: mtd
   use unstruc_messages
   use m_sediment, only : jased, ws, sedtra, stmpar, stm_included
   use sediment_basics_module
   implicit none

   integer,                                    intent(in)    :: NUMCONST     !< number of transported quantities
   integer,                                    intent(in)    :: limtyp       !< limiter type
   double precision, dimension(NUMCONST),      intent(in)    :: thetavert    !< compute fluxes (<1) or not (1)
   integer,                                    intent(in)    :: Ndkx         !< total number of flownodes (dynamically changing)
   integer,                                    intent(in)    :: kmx          !< maximum number of layers (dynamically changing)
   double precision, dimension(Ndkx),          intent(in)    :: zws          !< vertical coordinate of layers at interface/center locations
   double precision, dimension(Ndkx),          intent(in)    :: qw           !< flow-field vertical discharges
   integer,          dimension(Ndx),           intent(in)    :: kbot         !< flow-node based layer administration
   integer,          dimension(Ndx),           intent(in)    :: ktop         !< flow-node based layer administration
   double precision, dimension(NUMCONST,Ndkx), intent(in)    :: sed          !< transported quantities
   integer,                                    intent(in)    :: nsubsteps    !< number of substeps
   integer,          dimension(Ndx),           intent(in)    :: jaupdate     !< update cell (1) or not (0)
   integer,          dimension(Ndx),           intent(in)    :: ndeltasteps  !< number of substeps between updates
   double precision, dimension(NUMCONST,Ndkx), intent(inout) :: flux         !< adds vertical advection fluxes
   double precision, dimension(NUMCONST),      intent(in   ) :: wsf          !< vertical fall velocities 

   
   double precision, dimension(2049)                         :: dz
                                                            
   double precision                                          :: sedL, sedR, ds1L, ds2L, ds1R, ds2R, sl3L, sl3R, cf, dum
   double precision                                          :: dt_loc
   double precision                                          :: qw_loc
                                                            
   integer                                                   :: kk, k, kb, kt, kL, kR, kLL, kRR
   integer                                                   :: j, ll
                                                            
   double precision, external                                :: dlimiter
                                                            
   double precision, parameter                               :: DTOL = 1d-8
   
   if ( sum(1d0-thetavert(1:NUMCONST)).lt.DTOL ) goto 1234 ! nothing to do
   
   !if ( limtyp.eq.6 ) then
   !   call message(LEVEL_ERROR, 'transport/comp_fluxver: limtyp==6 not supported')
   !end if
   
   dt_loc = dts

   !$xOMP PARALLEL DO                                                                       &
   !$xOMP PRIVATE(kk,kb,kt,dz,k,cf,kL,kR,j,sedL,sedR,kLL,kRR,sl3L,sl3R,ds1L,ds1R,ds2L,ds2R,qw_loc) &
   !$xOMP FIRSTPRIVATE(dt_loc)
   do kk=1,Ndx
      if (kfs(kk) == 0) cycle
       
      if ( nsubsteps.gt.1 ) then
         if (jaupdate(kk).eq.0 ) then
            cycle
         else
            dt_loc = dts * ndeltasteps(kk)
         end if
      else
         dt_loc = dts
      end if

      kb = kbot(kk)
      kt = ktop(kk)
      dz(1)         = max(dtol, zws(kb)-zws(kb-1) )
      ! dz(2:kt-kb+1) = max(dtol, 0.5d0*(zws(kb+1:kt)-zws(kb-1:kt-1))  ) ! thickness between cell centers org
      dz(2:kt-kb+1) = max(dtol, 0.5d0*(zws(kb+1:kt)-zws(kb-1:kt-2))  ) ! thickness between cell centers

      dz(kt-kb+2)   = max(dtol, zws(kt)-zws(kt-1) )

      ! dz = max(dz,dtol)  !     fix for zero-thickness layer

       do k=kb,kt-1
         ! cf = dt_loc*qw(k)/(ba(kk)*dz(k-kb+2))
      
         kL = k   ! max(k,kb)
         kR = k+1 ! min(k+1,kt)
         
         do j=1,NUMCONST
            qw_loc = qw(k)
            if (jased < 4) then
               qw_loc = qw(k) - wsf(j)*ba(kk)
            else  if ( stm_included .and. j.ge.ISED1 .and. j.le.ISEDN ) then
               ll = j-ISED1+1
               if (k<sedtra%kmxsed(kk,ll)) then
                  qw_loc = qw(k)     ! settling flux zero below kmxsed layer
               else
                  qw_loc = qw(k) - mtd%ws(k,ISED1+j-1)*ba(kk)
               endif
            endif
               
            cf = cffacver*dt_loc*abs(qw_loc)/(ba(kk)*dz(k-kb+2))
            if (cffacver > 0d0) then  
               cf = cffacver*dt_loc*abs(qw_loc)/(ba(kk)*dz(k-kb+2)) ! courant nr
               cf = max(0d0,1d0-cf)                                 ! use high order only for small courant 
            else
               cf = 1d0                                             ! or always use it, is MUSCL = default
            endif 

            !if ( cf.gt.cflmx ) then
            !   continue
            !end if
             
            if ( thetavert(j).eq.1d0 ) cycle

            sedL = sed(j,kL)
            sedR = sed(j,kR)
            
            if ( thetavert(j).gt.0d0 ) then ! semi-explicit, use central scheme
               flux(j,k) = flux(j,k) + qw_loc*0.5d0*(sedL+sedR)
            else     ! fully explicit 
             !  if (limtyp.ne.0  ) then
                  if ( k.gt.kb-1 .and. qw_loc.gt.0d0 ) then
                     kLL  = max(k-1,kb)
                     sL3L = dz(k-kb+2)/dz(k-kb+1)
                     ! if ( abs(sL3L-1d0).gt.1d-4 ) then
                     !    continue
                     ! end if
                   
                     ds2L =  sedR-sedL
                     ds1L = (sedL-sed(j,kLL))*sl3L
                     sedL = sedL + 0.5d0 * cf * dlimiter(ds1L,ds2L,limtyp) * ds2L
                  end if
                  
                  if ( k.lt.kt .and. qw_loc.lt.0d0 .and. s1(kk)-zws(kb-1) > epshsdif ) then
                     kRR = min(k+2,kt)
                     sL3R = dz(k-kb+2)/dz(k-kb+3)
                    ! if ( abs(sL3R-1d0).gt.1d-4 ) then
                    !    continue
                    ! end if
                  
                     ds2R =  sedL-sedR
                     ds1R = (sedR-sed(j,kRR))*sl3R
                     sedR = sedR + 0.5d0 * cf * dlimiter(ds1R,ds2R,limtyp) * ds2R
                  end if
              !  end if
               
               flux(j,k) = flux(j,k) + max(qw_loc,0d0)*sedL + min(qw_loc,0d0)*sedR
            end if
         end do
      end do
   end do
   
   !$xOMP END PARALLEL DO

1234 continue

   return
end subroutine comp_fluxver

!> compose right-hand side
subroutine make_rhs(NUMCONST, thetavert, Ndkx, Lnkx, kmx, vol1, kbot, ktop, Lbot, Ltop, sumhorflux, fluxver, source, sed, nsubsteps, jaupdate, ndeltasteps, rhs)
   use m_flowgeom, only: Ndxi, Ndx, Lnx, Ln, ba  ! static mesh information
   use m_flowtimes, only: dts
   
   implicit none

   integer,                                intent(in)    :: NUMCONST     !< number of transported quantities
   double precision, dimension(NUMCONST),  intent(in)    :: thetavert   !< vertical advection explicit (0) or implicit (1)
   integer,                                intent(in)    :: Ndkx     !< total number of flownodes (dynamically changing)
   integer,                                intent(in)    :: Lnkx     !< total number of flowlinks (dynamically changing)
   integer,                                intent(in)    :: kmx      !< maximum number of layers
!   double precision, dimension(Ndkx),      intent(in)    :: sq       !< flux balance (inward positive)
   double precision, dimension(Ndkx),      intent(in)    :: vol1     !< volumes
   integer,          dimension(Ndx),       intent(in)    :: kbot     !< flow-node based layer administration
   integer,          dimension(Ndx),       intent(in)    :: ktop     !< flow-node based layer administration
   integer,          dimension(Lnx),       intent(in)    :: Lbot     !< flow-link based layer administration
   integer,          dimension(Lnx),       intent(in)    :: Ltop     !< flow-link based layer administration
   double precision, dimension(NUMCONST,Ndkx), intent(inout) :: sumhorflux  !< sum of horizontal fluxes
   double precision, dimension(NUMCONST,Ndkx), intent(in)    :: fluxver  !< vertical fluxes
   double precision, dimension(NUMCONST,Ndkx), intent(in)    :: source   !< sources
   double precision, dimension(NUMCONST,Ndkx), intent(inout) :: sed      !< transported quantities
   integer,                                    intent(in)    :: nsubsteps  !< number of substeps
   integer,          dimension(Ndx),           intent(in)    :: jaupdate !< update cell (1) or not (0)
   integer,          dimension(Ndx),           intent(in)    :: ndeltasteps !< number of substeps between updates
   double precision, dimension(NUMCONST,Ndkx), intent(out)   :: rhs      ! right-hand side, dim(NUMCONST,Ndkx)

   double precision                                      :: dvoli
   double precision                                      :: dt_loc

   integer                                               :: LL, L, Lb, Lt
   integer                                               :: kk, k, kb, kt
   integer                                               :: k1, k2, j

   double precision, parameter                           :: dtol=1d-8
   
   dt_loc = dts
   
!   rhs = 0d0
!   
!!  add horizontal fluxes to right-hand side
!   do LL=1,Lnx
!      Lb = Lbot(LL)
!      Lt = Ltop(LL)
!      do L=Lb,Lt
!!        get neighboring flownodes
!         k1 = ln(1,L)
!         k2 = ln(2,L)
!         do j=1,NUMCONST
!            rhs(j,k1) = rhs(j,k1) - fluxhor(j,L)
!            rhs(j,k2) = rhs(j,k2) + fluxhor(j,L)
!         end do
!      end do
!   end do

   if ( kmx.gt.0 ) then
!     add vertical fluxes, sources, storage term and time derivative to right-hand side

     !$OMP PARALLEL DO                 &
     !$OMP PRIVATE(kk,kb,kt,k,dvoli,j) & 
     !$OMP FIRSTPRIVATE(dt_loc)
      do kk=1,Ndxi
      
         if ( jaupdate(kk).eq.0 ) then
            cycle
         else
            dt_loc = dts * ndeltasteps(kk)
         end if
         
         kb = kbot(kk)
         kt = ktop(kk)
         do k=kb,kt
            dvoli = 1d0/max(vol1(k),dtol)
            
            do j=1,NUMCONST
 !              rhs(j,k) = ((rhs(j,k) - (1d0-thetavert(j))*(fluxver(j,k) - fluxver(j,k-1)) - sed(j,k)*sq(k)) * dvoli + source(j,k))*dts + sed(j,k)

            
               rhs(j,k) = ((sumhorflux(j,k)/ndeltasteps(kk) - (1d0-thetavert(j))*(fluxver(j,k) - fluxver(j,k-1))) * dvoli + source(j,k))*dt_loc + sed(j,k)           
               sumhorflux(j,k) = 0d0
               
               ! BEGIN DEBUG
               ! rhs(j,k) = source(j,k)*dts + sed(j,k)
               ! END DEBUG
            end do
            
         end do
      end do
      !$OMP END PARALLEL DO

   else
!     add time derivative
      if ( nsubsteps.eq.1 ) then
         !$OMP PARALLEL DO       &
         !$OMP PRIVATE(k,j,dvoli ) 
         
         do k=1,Ndxi
            dvoli = 1d0/max(vol1(k),dtol)
            
            do j=1,NUMCONST
                rhs(j,k) = (sumhorflux(j,k) * dvoli + source(j,k)) * dts + sed(j,k)
                sumhorflux(j,k) = 0d0
            end do
         end do
         !$OMP END PARALLEL DO
      else
         
         !$OMP PARALLEL DO         &
         !$OMP PRIVATE(k,j,dvoli ) &
         !$OMP FIRSTPRIVATE(dt_loc)
         do k=1,Ndxi
            if ( jaupdate(k).eq.0 ) then
               cycle
            else
               dt_loc = dts * ndeltasteps(k)
            end if
            
            dvoli = 1d0/max(vol1(k),dtol)
            
            do j=1,NUMCONST
                rhs(j,k) = (sumhorflux(j,k)/ndeltasteps(k) * dvoli + source(j,k)) * dt_loc + sed(j,k)
                sumhorflux(j,k) = 0d0
            end do
         end do
         !$OMP END PARALLEL DO
      end if
   end if
   
end subroutine make_rhs


!> compose right-hand side
subroutine solve_2D(NUMCONST, Ndkx, Lnkx, vol1, kbot, ktop, Lbot, Ltop, sumhorflux, fluxver, source, sink, nsubsteps, jaupdate, ndeltasteps, sed, rhs)
   use m_flowgeom, only: Ndxi, Ndx, Lnx, Ln, ba  ! static mesh information
   use m_flowtimes, only: dts
   
   implicit none

   integer,                                intent(in)    :: NUMCONST     !< number of transported quantities
   integer,                                intent(in)    :: Ndkx     !< total number of flownodes (dynamically changing)
   integer,                                intent(in)    :: Lnkx     !< total number of flowlinks (dynamically changing)
!   double precision, dimension(Ndkx),      intent(in)    :: sq       !< flux balance (inward positive)
   double precision, dimension(Ndkx),      intent(in)    :: vol1     !< volumes
   integer,          dimension(Ndkx),      intent(in)    :: kbot     !< flow-node based layer administration
   integer,          dimension(Ndkx),      intent(in)    :: ktop     !< flow-node based layer administration
   integer,          dimension(Lnkx),      intent(in)    :: Lbot     !< flow-link based layer administration
   integer,          dimension(Lnkx),      intent(in)    :: Ltop     !< flow-link based layer administration
   double precision, dimension(NUMCONST,Ndkx), intent(inout) :: sumhorflux  !< sum of horizontal fluxes
   double precision, dimension(NUMCONST,Ndkx), intent(in)    :: fluxver  !< vertical fluxes
   double precision, dimension(NUMCONST,Ndkx), intent(in)    :: source   !< sources
   double precision, dimension(NUMCONST,Ndkx), intent(in)    :: sink     !< linearized sinks
   integer,                                    intent(in)    :: nsubsteps  !< number of substeps
   integer,          dimension(Ndx),           intent(in)    :: jaupdate !< update cell (1) or not (0)
   integer,          dimension(Ndx),           intent(in)    :: ndeltasteps !< number of substeps between updates
   double precision, dimension(NUMCONST,Ndkx), intent(inout) :: sed      !< transported quantities
   double precision, dimension(NUMCONST,Ndkx)                :: rhs      ! work array: right-hand side, dim(NUMCONST,Ndkx)
   
   double precision, dimension(NUMCONST)                     :: thetavert
   
   double precision                                          :: dt_loc
   
   integer                                                   :: j, k
   
   thetavert = 0d0
   
   dt_loc = dts
   
   call make_rhs(NUMCONST, thetavert, Ndkx, Lnkx, 0, vol1, kbot, ktop, Lbot, Ltop, sumhorflux, fluxver, source, sed, nsubsteps, jaupdate, ndeltasteps, rhs)
   
   !$OMP PARALLEL DO         &
   !$OMP PRIVATE(k,j)        &
   !$OMP FIRSTPRIVATE(dt_loc)
   do k=1,Ndxi
      if ( nsubsteps.gt.1 ) then
         if ( jaupdate(k).eq.0 ) then
            cycle
         else
            dt_loc = dts*ndeltasteps(k)
         end if
      else
         dt_loc = dts
      end if
      
      do j=1,NUMCONST
         sed(j,k) = rhs(j,k) / (1d0 + dt_loc*sink(j,k))
      end do
   end do
   !$OMP END PARALLEL DO

   return
end subroutine solve_2D


!> solve equations implicitly in vertical direction
subroutine solve_vertical(NUMCONST, ISED1, ISEDN, limtyp, thetavert, Ndkx, Lnkx, kmx,    &
                          zws, qw, vol1, kbot, ktop, Lbot, Ltop,    &
                          sumhorflux, fluxver, source, sink,   &
                          difsed, sigdifi, vicwws, &
                          nsubsteps, jaupdate, ndeltasteps, sed,  &
                          a, b, c, d, e, sol, rhs)
   use m_flowgeom,  only: Ndxi, Ndx, Lnx, Ln, ba, kfs, bl  ! static mesh information
   use m_flowtimes, only: dts
   use m_flow,      only: epshsdif, s1, kmxn, xlozmidov, rhomean, rho, ag, a1, wsf  ! do not use m_flow, please put this in the argument list
   use m_sediment,  only: mtd, jased, ws, sedtra, stmpar
   use sediment_basics_module
   
   implicit none

   integer,                                    intent(in)    :: NUMCONST    !< number of transported quantities
   integer,                                    intent(in)    :: ISED1       !< index of first sediment fraction in constituents array
   integer,                                    intent(in)    :: ISEDN       !< index of last  sediment fraction in constituents array
   integer,                                    intent(in)    :: limtyp      !< limiter type
   double precision, dimension(NUMCONST),      intent(in)    :: thetavert   !< vertical advection explicit (0) or implicit (1)
   integer,                                    intent(in)    :: Ndkx        !< total number of flownodes (dynamically changing)
   integer,                                    intent(in)    :: Lnkx        !< total number of flowlinks (dynamically changing)
   integer,                                    intent(in)    :: kmx         !< maximum number of layers
   double precision, dimension(Ndkx),          intent(in)    :: zws         !< vertical coordinate of layers at interface/center locations
   double precision, dimension(Ndkx),          intent(in)    :: qw          !< flow-field vertical discharges
!   double precision, dimension(Ndkx),          intent(in)    :: sq          !< flux balance (inward positive)
   double precision, dimension(Ndkx),          intent(in)    :: vol1        !< volumes
   integer,          dimension(Ndx),           intent(in)    :: kbot        !< flow-node based layer administration
   integer,          dimension(Ndx),           intent(in)    :: ktop        !< flow-node based layer administration
   integer,          dimension(Lnx),           intent(in)    :: Lbot        !< flow-link based layer administration
   integer,          dimension(Lnx),           intent(in)    :: Ltop        !< flow-link based layer administration
   double precision, dimension(NUMCONST,Ndkx), intent(inout) :: sumhorflux  !< sum of horizontal fluxes
   double precision, dimension(NUMCONST,Ndkx), intent(in)    :: fluxver     !< vertical fluxes
   double precision, dimension(NUMCONST,Ndkx), intent(in)    :: source      !< sources
   double precision, dimension(NUMCONST,Ndkx), intent(in)    :: sink        !< sinks
   double precision, dimension(NUMCONST),      intent(in)    :: difsed      !< scalar-specific diffusion coefficent (dicoww+difmod)
   double precision, dimension(Ndkx),          intent(in)    :: vicwws      !< vertical eddy viscosity, NOTE: real, not double
   double precision, dimension(NUMCONST),      intent(in)    :: sigdifi     !< 1/(Prandtl number) for heat, 1/(Schmidt number) for mass
   integer,                                    intent(in)    :: nsubsteps   !< number of substeps
   integer,          dimension(Ndx),           intent(in)    :: jaupdate    !< update cell (1) or not (0)
   integer,          dimension(Ndx),           intent(in)    :: ndeltasteps !< number of substeps
   double precision, dimension(NUMCONST,Ndkx), intent(inout) :: sed         !< transported quantities
   double precision, dimension(kmx,NUMCONST)                 :: a,b,c,d     ! work array: aj(i,j)*sed(j,k-1) + bj(i,j)*sed(j,k) + c(i,j)*sed(j,k+1) = d(i), i=k-kb+1
   double precision, dimension(kmx)                          :: sol, e      ! work array: solution and dummy array in tridag, respectively
   double precision, dimension(NUMCONST,Ndkx)                :: rhs         ! work array: right-hand side, dim(NUMCONST,Ndkx)

   double precision                                          :: dvoli, fluxfac, dvol1i, dvol2i
   double precision                                          :: dum, rhstot, dtbazi, dtba, ozmid, bruns
                                                            
   integer                                                   :: LL, L, Lb, Lt
   integer                                                   :: kk, k, kb, kt, ktx
   integer                                                   :: k1, k2, i, j, n, kkk
   
   double precision                                          :: dt_loc
   double precision                                          :: qw_loc
                                                            
   double precision, parameter                               :: dtol=1d-8
   
   dt_loc = dts
   
   rhs = 0d0
   
   call make_rhs(NUMCONST, thetavert, Ndkx, Lnkx, kmx, vol1, kbot, ktop, Lbot, Ltop, sumhorflux, fluxver, source, sed, nsubsteps, jaupdate, ndeltasteps, rhs)

   ! construct and solve system
   !$OMP PARALLEL DO                                                 &
   !$OMP PRIVATE(kk,kb,ktx,kt,a,b,c,sol,j,d,k,n,dvol1i,dvol2i,fluxfac,e,dtbazi,dtba,ozmid,bruns,qw_loc) &
   !$OMP FIRSTPRIVATE(dt_loc)
   do kk=1,Ndxi
      if ( nsubsteps.gt.1 ) then
         if ( jaupdate(kk).eq.0 ) then
            cycle
         else
            dt_loc = dts * ndeltasteps(kk)
         end if
      else
         dt_loc = dts
      end if
      
      kb  = kbot(kk) 
      kt  = ktop(kk) 
      if (kfs(kk) == 0) cycle

      ktx = kb + kmxn(kk) - 1
      a   = 0d0
      c   = 0d0
      sol = 0d0

!     add linearized sinks to diagonal
      do k=kb,kt
         n = k-kb+1     ! layer number
         do j=1,NUMCONST
            b(n,j) = 1d0 + dt_loc*sink(j,k)
         end do
      end do

      do j=1,NUMCONST
         d(1,j) = rhs(j,kb)
         do k=kb,kt     ! assume zero-fluxes at boundary and top
            n = k-kb+1  ! layer number
            d(n,j) = rhs(j,k)
         end do
      end do

      ! if ( s1(kk)-bl(kk) > epshsdif ) then
      ! if ( s1(kk)-zws(kb-1) > epshsdif ) then
    
      do k=kb,kt-1   ! assume zero-fluxes at boundary and top
         n = k-kb+1  ! layer number
         dvol1i  = 1d0/max(vol1(k),dtol)                            ! dtol: safety
         dvol2i  = 1d0/max(vol1(k+1),dtol)                          ! dtol: safety
         dtba    = dt_loc*ba(kk)
         dtbazi  = dtba / max(1d-4, 0.5d0*(zws(k+1)-zws(k-1)) )     ! another safety check
       
         ozmid   = 0d0
         if (xlozmidov > 0d0) then 
            if (rho(k) < rho(k-1) ) then 
                bruns  = ( rho(k-1) - rho(k) ) / ( 0.5d0*(zws(k+1)-zws(k-1)) )   ! = -drhodz
                bruns  = sqrt( bruns*ag/rhomean )
                ozmid  = 0.2d0*xlozmidov*xlozmidov*bruns 
            endif   
         endif     
         
         do j=1,NUMCONST
            
!           ! diffusion  
            if (jased > 3 .and. j >= ISED1 .and. j <= ISEDN) then  ! sediment d3d
               fluxfac = (mtd%seddif(j-ISED1+1,k))*dtbazi
               ! D3D: vicmol/sigmol(l) + ozmid + seddif(nm, k, ls)/sigdif(l)
            else                     
               fluxfac = (sigdifi(j)*vicwws(k) + difsed(j) + ozmid)*dtbazi
            end if
            
!           BEGIN DEBUG
!            fluxfac = dt_loc * (difsed(j)) *ba(kk) / ( 0.5d0*(zws(k+1) - zws(k-1)) )  ! m3
!           END DEBUG

            b(n,j)   = b(n,j)   + fluxfac*dvol1i
            c(n,j)   = c(n,j)   - fluxfac*dvol1i
            
            b(n+1,j) = b(n+1,j) + fluxfac*dvol2i
            a(n+1,j) = a(n+1,j) - fluxfac*dvol2i
            
!           advection
            if ( thetavert(j).gt.0d0 ) then ! semi-implicit, use central scheme
                ! BEGIN DEBUG
                ! if ( .false. .and. thetavert(j).gt.0d0 ) then ! semi-implicit, use central scheme
                ! END DEBUG
            
                if (jased < 4) then
                   qw_loc = qw(k) - wsf(j)*a1(kk)
                else  if ( j.ge.ISED1 .and. j.le.ISEDN ) then 
                   qw_loc = qw(k) - mtd%ws(k,ISED1+j-1)*a1(kk) 
                endif
               
                fluxfac  = qw_loc*0.5d0*thetavert(j)*dt_loc
                
                a(n+1,j) = a(n+1,j) - fluxfac*dvol2i
                b(n+1,j) = b(n+1,j) - fluxfac*dvol2i
            
                b(n,j)   = b(n,j)   + fluxfac*dvol1i
                c(n,j)   = c(n,j)   + fluxfac*dvol1i
            end if
            
         end do
      end do
      
!     solve system(s)
      do j=1,NUMCONST
!         if ( kk.eq.2 .and. j.eq.1 ) then
!            do k=kb,kt
!               n = k-kb+1
!               write(6,*) n, a(n,j), b(n,j), c(n,j), d(n,j)
!            end do
!         end if
         
         call tridag(a(1,j), b(1,j), c(1,j), d(1,j), e, sol, kt-kb+1)
         
         sed(j,kb:kt) = sol(1:kt-kb+1)
         sed(j,kt+1:ktx) = sed(j,kt)  
         
!        BEGIN DEBUG         
!         do k=kb,kt
!            if ( j.eq.1 .and. ( sed(j,k).gt.30.0001 .or. sed(j,k).lt.-0.0001 ) ) then
!               continue
!               write(6,*) 'kk=', kk, 'lay=', k-kb+1
!               write(6,*) 'rhs=', rhs(j,k)
!               write(6,*) 'sed=', sed(j,k)
!               call qnerror(' ', ' ', ' ')
!            end if
!         end do
!        END DEBUG
         
      end do

   end do

  !$OMP END PARALLEL DO

   return
end subroutine solve_vertical

!> limiter function
double precision function dlimiter(d1,d2,limtyp)
   implicit none
   
   double precision, intent(in) :: d1, d2   !< left and right slopes
   integer         , intent(in) :: limtyp   !< first order upwind (0) or MC (>0)
   
   double precision             :: r
   double precision, parameter  :: dtol=1d-16
   
   double precision, parameter  :: TWO=2.0d0
   
   dlimiter = 0d0
   if (limtyp == 0)     return
   if ( d1*d2.lt.dtol ) return

   r = d1/d2    ! d1/d2
      
!   if ( limtyp.eq.1 ) then
!!     Van Leer
!      dlimiter = dble(min(limtyp,1)) * (r + abs(r) ) / (1 + abs(r) )
!   else
!!     Monotinized Central
      dlimiter = dble(min(limtyp,1)) * max(0d0, min(TWO*r,TWO,0.5d0*(1d0+r)) ) 
!   end if
   
end function dlimiter

!> MC limiter function for non-equidistant grid
double precision function dlimiter_nonequi(d1,d2,alpha,s)
   implicit none
   
   double precision, intent(in) :: d1, d2   !< left and right slopes
   double precision, intent(in) :: alpha    !< interface distance
   double precision, intent(in) :: s        !< mesh width ratio DX2/DX1
   
   double precision             :: r
   double precision, parameter  :: dtol=1d-16
   
   double precision             :: TWO1, TWO2
   
   dlimiter_nonequi = 0d0
   if ( d1*d2.lt.dtol ) return

   r = d1/d2    ! d1/d2
   
   TWO2 = 1d0/max(alpha,dtol)
   TWO1 = TWO2/max(s,dtol)
      
!  Monotinized Central
   dlimiter_nonequi = max(0d0, min(TWO1*r,TWO2,0.5d0*(1d0+r)) )
   
end function dlimiter_nonequi

   
   double precision function dlimitercentral(dc,d2,limtyp)  ! as dlimiter, now for central gradient instead of slope
   implicit none
   
   double precision, intent(in) :: dc, d2   !< central and right slopes
   integer         , intent(in) :: limtyp   !< first order upwind (0) or MC (>0)
   
   double precision             :: r, d1
   double precision, parameter  :: dtol=1d-16
         
   dlimitercentral = 0d0
   if (limtyp == 0)     return
!   if ( d1*d2.lt.dtol ) return
!
!   r = d1/d2    ! d1/d2
!   r = 2d0*r - 1d0 
   
!  compute left slope (assume uniform mesh)
   d1 = 2d0*dc - d2
   
   if ( d1*d2.lt.dtol ) return
   
   r = d1/d2    ! d1/d2
   
   dlimitercentral = d2 * max(0d0, min(2d0*r,0.5d0*(1d0+r),2d0) ) !  Monotonized Central
end function dlimitercentral
   

!> initialize transport, set the enumerators
subroutine ini_transport()
   use m_transport
   use m_flowparameters
   use m_sediment
   use m_physcoef
   use m_flowexternalforcings
   use string_module
   use Messagehandling
   use m_flow, only: kmx

   use m_fm_wq_processes
   use m_alloc
   use unstruc_model, only: md_thetav_waq
    
   implicit none
   
   character(len=8) :: str
   character(len=256) :: msg
   
   integer            :: i, itrace, ised, isf, ifrac, isys, iconst
   integer, external  :: findname

   NUMCONST  = 0
   ISALT = 0
   ITEMP = 0
   ISED1 = 0
   ISEDN = 0
   ISPIR = 0
   ITRA1 = 0
   ITRAN = 0
   
   if ( jasal.ne.0 ) then
      NUMCONST = NUMCONST+1
      ISALT = NUMCONST
   end if
   
   if ( jatem.ne.0 ) then
      NUMCONST = NUMCONST+1
      ITEMP = NUMCONST
   end if
   
   if ( jased.ne.0) then
      if( mxgr.gt.0 ) then
         NUMCONST  = NUMCONST+1
         ISED1 = NUMCONST

         NUMCONST  = NUMCONST+mxgr-1
         ISEDN = NUMCONST
      end if
   end if

   if ( jasecflow > 0 .and. jaequili == 0 .and. kmx == 0 ) then
      NUMCONST  = NUMCONST+1
      ISPIR     = NUMCONST
   endif 
   
   if ( numtracers.gt.0 ) then
      NUMCONST = NUMCONST+1
      ITRA1 = NUMCONST
      NUMCONST = NUMCONST+numtracers-1
      ITRAN = NUMCONST
   end if
   
   select case ( jatransportautotimestepdiff )
   case ( 0 )
!     limitation of diffusion   
      jalimitdiff = 1
      jalimitdtdiff = 0
   case ( 1 )
!     limitation of transport time step due to diffusion 
      jalimitdiff = 0
      jalimitdtdiff = 1
   case ( 2 )
!     no limitation of transport time step due to diffusion, and no limitation of diffusion 
      jalimitdiff = 0
      jalimitdtdiff = 0
   case default   ! as 0
      jalimitdiff = 1
      jalimitdtdiff = 0
   end select
   
   call alloc_transport(.false.)
   
   if ( ISALT.gt.0 ) then
      if ( javasal == 6) then
         thetavert(ISALT) = 0d0    ! Ho explicit
      else
         thetavert(ISALT) = tetav  ! Central implicit
      end if
      const_names(ISALT) = 'salt'
   end if
   
   if ( ITEMP.gt.0 ) then
      if ( javatem == 6) then
         thetavert(ITEMP) = 0d0     ! Ho explicit
      else
         thetavert(ITEMP) = tetav   ! Central implicit  0.55d0
      end if
      const_names(ITEMP) = 'temperature'
   end if
   
   if ( ISED1.gt.0 ) then
      if ( javased == 6 ) then
         thetavert(ISED1:ISEDN) = 0d0
      else
         thetavert(ISED1:ISEDN) = tetav
      end if
      if (.not. stm_included) then   ! Andere naamgeving in flow_sedmorinit, fracties van sed file
         do i=ISED1,ISEDN
            ised = i-ISED1+1
            write(str,"(I0)") ised
            const_names(i) = 'sediment_'//trim(str)
         end do
      else
         !
         ! Map fraction names from sed to constituents (moved from ini_transport)
         !
         do i=ISED1,ISEDN
           ised = i-ISED1+1                                             
           const_names(i) = trim(stmpar%sedpar%NAMSED(sedtot2sedsus(ised)))   ! JRE - netcdf output somehow does not tolerate spaces in varnames?
           !call remove_all_spaces(const_names(i))                             ! see whether this fix works
           !const_names(i) = trim(const_names(i))
         end do
         !
         !   Map sfnames to const_names
         !
         if (numfracs > 0) then
            do isf = 1, stmpar%lsedsus        ! dimension of sed constituents
               ifrac = findname(numfracs,sfnames,trim(const_names(isf+ISED1-1)))
               if ( ifrac.gt.0 ) then
                  ifrac2const(ifrac) = isf+ISED1-1
               !else
                  !call mess(LEVEL_ERROR, 'ini_transport(): fraction '//trim(const_names(isf+ISED1-1))//' does not have a concentration bnd assigned.')
               end if
            end do
         end if    ! numfracs
      end if       ! stm_included
   end if          ! ised
   
   if ( jasecflow > 0 .and. jaequili == 0 .and. kmx == 0 ) then
      const_names(ISPIR) = 'secondary_flow_intensity'
   end if

   if ( ITRA1.gt.0 ) then
      do i=ITRA1,ITRAN
         itrace = i-ITRA1+1
         
!        set name
         if ( trim(trnames(itrace)).ne.'' ) then
            const_names(i) = trim(trnames(itrace))
            const_units(i) = trim(trunits(itrace))
         else
            write(str,"(I0)") itrace
            const_names(i) = 'tracer_'//trim(str)
         end if
         
         itrac2const(itrace) = i
         
      end do
   end if
   
   if ( NUMCONST.gt.0 ) then
      call mess(LEVEL_INFO, 'List of constituents defined in the model')
      do i = 1, NUMCONST
         write(msg, '(I8,X,A)') i, const_names(i)
         call mess(LEVEL_INFO, msg)
      enddo
   endif

   if ( numwqbots.gt.0 ) then
      call mess(LEVEL_INFO, 'List of water quality bottom variables defined in the model')
      do i = 1, numwqbots
         write(msg, '(I8,X,A)') i, wqbotnames(i)
         call mess(LEVEL_INFO, msg)
      enddo
   endif

   if ( jawaqproc > 0 ) then
!     fill administration for WAQ substances with fall velocities, and thetavert
      do isys=1,nosys
         i = itrac2const(isys2trac(isys))
         isys2const(isys) = i
         iconst2sys(i) = isys
         
         thetavert(i) = md_thetav_waq
      end do  
   end if
   
!   iconst_cur = min(NUMCONST,ITRA1)
   iconst_cur = min(NUMCONST,1)
   
   
!  local timestepping
   time_dtmax = -1d0  ! cfl-numbers not evaluated
   nsubsteps = 1
   ndeltasteps = 1
   jaupdatehorflux = 1
   numnonglobal = 0
   
!  sediment advection velocity
   jaupdateconst = 1
   
   if ( stm_included ) then
      if (stmpar%lsedsus>0) then
         noupdateconst = 0
         do i=ISED1,ISEDN
            jaupdateconst(i) = 0
            noupdateconst(i) = 1
         end do
      end if
   end if
   
   return
end subroutine ini_transport


!> allocate transport arrays
subroutine alloc_transport(Keepexisting)
   use m_flowgeom, only: Ndx, Lnx
   use m_flow, only: Lnkx, Ndkx, kmx, sigdifi, wsf
   use m_fm_wq_processes
   use m_transport
   use m_alloc
   use m_meteo, only: numtracers, numfracs
   use m_flowexternalforcings, only: numsrc, qcsrc, vcsrc, wstracers
   use m_sediment, only: stm_included, jasedtranspveldebug
   implicit none
   
   logical, intent(in) :: KeepExisting    !< keep existing data (true) or not (false)
   
!  allocate and initialize fluxes
   
   call realloc(fluxhor, (/ NUMCONST, Lnkx /), keepExisting=KeepExisting, fill=0d0)
   call realloc(fluxver, (/ NUMCONST, Ndkx /), keepExisting=KeepExisting, fill=0d0)
   
   call realloc(fluxhortot, (/ NUMCONST, Lnkx /), keepExisting=KeepExisting, fill=0d0)
   call realloc(sinksetot,  (/ NUMCONST, Ndx /),  keepExisting=KeepExisting, fill=0d0)
   call realloc(sinkftot,   (/ NUMCONST, Ndx /),  keepExisting=KeepExisting, fill=0d0)
   
   call realloc(difsedu, NUMCONST, keepExisting=KeepExisting, fill=0d0)
   call realloc(difsedw, NUMCONST, keepExisting=KeepExisting, fill=0d0)
   call realloc(sigdifi, NUMCONST, keepExisting=KeepExisting, fill=0d0)
   call realloc(wsf, NUMCONST, keepExisting=.true., fill=0d0)
   
   call realloc(constituents, (/ NUMCONST, Ndkx /), keepExisting=KeepExisting, fill=0d0)
   
   call realloc(const_sour  , (/ NUMCONST, Ndkx /), keepExisting=KeepExisting, fill=0d0)
   call realloc(const_sink  , (/ NUMCONST, Ndkx /), keepExisting=KeepExisting, fill=0d0)
   
   call realloc(dsedx       , (/ NUMCONST, Ndkx /), keepExisting=KeepExisting, fill=0d0)
   call realloc(dsedy       , (/ NUMCONST, Ndkx /), keepExisting=KeepExisting, fill=0d0)
   
   call realloc(thetavert, NUMCONST, keepExisting=KeepExisting, fill=0d0)
   !call realloc(wstracers, NUMCONST, keepExisting=KeepExisting, fill=0d0)
   call realloc(wstracers, NUMCONST, keepExisting=.true., fill=0d0)
   
   call realloc(const_names, NUMCONST, keepExisting=KeepExisting, fill='')
   call realloc(const_units, NUMCONST, keepExisting=KeepExisting, fill='')
   
   call realloc(id_const, (/ 2, NUMCONST /), keepExisting=KeepExisting, fill = 0)
   
   call realloc(sumhorflux, (/ NUMCONST, Ndkx /), keepExisting=.false., fill=0d0)
   call realloc(ndeltasteps, Ndx, keepExisting=.false., fill=1)
   call realloc(jaupdate,    Ndx, keepExisting=.false., fill=1)
   call realloc(jaupdatehorflux, Lnx, keepExisting=.false., fill=1)
   call realloc(dtmax,       Ndx, keepExisting=.false., fill=0d0)
   
   if ( jalimitdtdiff.eq.1 ) then
      call realloc(sumdifflim, Ndkx, keepExisting=.false., fill = 0d0)
   end if
   call realloc(dxiAu, Lnkx, keepExisting=.false., fill = 0d0)
   
   call realloc(jaupdateconst, NUMCONST, keepExisting=.false., fill=1)
   if ( stm_included ) then
      call realloc(noupdateconst, NUMCONST, keepExisting=.false., fill=0)
      
      !IF (jasedtranspveldebug>0) then
         ! DEBUG
         call realloc(u1sed, Lnkx, keepExisting=.false., fill=0d0)
         call realloc(q1sed, Lnkx, keepExisting=.false., fill=0d0)
         call realloc(ucxsed, ndx, keepExisting=.false., fill=0d0)
         call realloc(ucysed, ndx, keepExisting=.false., fill=0d0)
         call realloc(qcxsed, ndx, keepExisting=.false., fill=0d0)
         call realloc(qcysed, ndx, keepExisting=.false., fill=0d0)
         call realloc(xsedflux,(/NUMCONST, ndx/), keepExisting=.false., fill=0d0)
         call realloc(ysedflux,(/NUMCONST, ndx/), keepExisting=.false., fill=0d0)
         ! \DEBUG
      !end if
      
   end if
   
!  work arrays
   if ( allocated(rhs) ) deallocate(rhs)
   allocate(rhs(NUMCONST,Ndkx))
   
   if ( kmx.gt.0 ) then ! 3D
      if ( allocated(a) ) deallocate(a,b,c,d,e,sol)
      allocate(a(kmx,NUMCONST),b(kmx,NUMCONST),c(kmx,NUMCONST),d(kmx,NUMCONST),e(kmx),sol(kmx))
      a = 0d0
      b = 0d0
      c = 0d0
      d = 0d0
      e = 0d0
   end if
   
!  tracer boundary condition
   call realloc(itrac2const, numtracers, keepExisting=KeepExisting, fill=0)   
   call realloc(ifrac2const, numfracs, keepExisting=KeepExisting, fill=0)
   
   call realloc(qcsrc, (/   NUMCONST, numsrc /), keepExisting=.false., fill=0d0)
   call realloc(vcsrc, (/ 2*NUMCONST, numsrc /), keepExisting=.false., fill=0d0)
   
   if ( jawaqproc > 0 ) then
!     WAQ
      call realloc(isys2const,  notot, keepExisting=.true., fill=0)
      call realloc(iconst2sys,  NUMCONST, keepExisting=.true., fill=0)
   end if
   return
end subroutine alloc_transport


!> fill constituent array
subroutine fill_constituents(jas) ! if jas == 1 do sources  
   use m_transport
   use m_flowgeom
   use m_flow
   use m_sediment
   use m_fm_wq_processes
   use m_partitioninfo
   use m_sferic, only: jsferic, fcorio
   use m_flowtimes , only : dnt, dts
   use unstruc_messages
   use m_flowparameters, only: janudge
   use m_missing
   implicit none
   
   character(len=128)          :: message
   
   double precision            :: dvoli
   integer, intent(in)         :: jas 
   integer                     :: i, iconst, j, kk, kkk, k, kb, kt, n, kk2, L, s, jamba_src
   double precision, parameter :: dtol=1d-8   
   double precision            :: spir_ce, spir_be, spir_e, alength_a, time_a, alpha, fcoriocof, qsrck, qsrckk, dzss
   
   double precision            :: Trefi
      
   const_sour = 0d0
   const_sink = 0d0
   
   do k=1,Ndkx
      if ( ISALT.ne.0 ) then
         constituents(ISALT,k) = sa1(k)
      end if
   
      !if ( ITEMP.ne.0 ) then
      !    constituents(ITEMP,k) = tem1(k)
      !end if
   
      if( jasecflow > 0 .and. jaequili == 0 .and. kmx == 0 ) then
         constituents(ISPIR,k) = spirint(k)
      endif
   
      if ( ISED1.ne.0 ) then
         do i=1,mxgr
            iconst = ISED1+i-1
            constituents(iconst,k) = sed(i,k)
         end do
      end if
   end do
   
   difsedu = 0d0 ; difsedw = 0d0 ; sigdifi = 0d0
   
!  diffusion coefficients

   if ( ISALT.ne.0 ) then 
      if (dicouv .ge. 0d0) then 
          difsedu(ISALT) =          difmolsal
      endif 
      if (dicoww .ge. 0d0) then 
          difsedw(ISALT) = dicoww + difmolsal
          sigdifi(ISALT) = 1d0/sigsal
      endif
   end if
   
   if ( ITEMP.ne.0 ) then
      if (dicouv .ge. 0d0) then
          difsedu(ITEMP) =          difmoltem
      endif 
      if (dicoww .ge. 0d0) then 
          difsedw(ITEMP) = dicoww + difmoltem
          sigdifi(ITEMP) = 1d0/sigtem
      endif
   end if
   
   if( jasecflow > 0 .and. jaequili == 0 .and. kmx == 0 ) then
      difsedu(ISPIR) = 0d0
      difsedw(ISPIR) = 0d0 !dicoww + difmoltem
      sigdifi(ISPIR) = 0d0 !/sigspi 
   endif 

   if ( ISED1.ne.0) then
      do i=1,mxgr
         iconst = ISED1+i-1
         if (dicouv .ge. 0d0) difsedu(iconst) = 0d0
         if (dicoww .ge. 0d0) then 
            difsedw(iconst) = dicoww 
            ! difsedw(iconst,:) = mtd%seddif(i,:)
            sigdifi(iconst) = 1d0/sigsed
         endif
         if (jased < 4) wsf(iconst) = ws(i)
      end do
   end if
   
   if ( ITRA1.gt.0 ) then
      do i=ITRA1,ITRAN
         difsedu(i)   =          difmoltr
         if (dicoww .ge. 0d0) then 
             difsedw(i) = dicoww + difmoltr 
             sigdifi(i) = 1d0
         endif
         wsf(i) = wstracers(i - itra1 + 1)
      end do
   end if
   
!  sources
   do kk=1,Ndx
   
!     nudging
      Trefi = 0d0
      if ( janudge.eq.1 .and. jas.eq.1 ) then
!        get reference time
         Trefi = nudge_rate(kk)
      end if
            
      call getkbotktop(kk,kb,kt)
      do k=kb,kt
         dvoli = 1d0/max(vol1(k),dtol)

!        temperature
         if (jatem > 1) then
            if (heatsrc(k) > 0d0) then
               const_sour(ITEMP,k) =  heatsrc(k)*dvoli
            else if (heatsrc(k) < 0d0) then  
               const_sink(ITEMP,k) = -heatsrc(k)*dvoli / max(constituents(itemp, k),0.001)
            endif
         endif
         
!        nudging
         if ( Trefi.gt.0d0 ) then
            if ( ITEMP.gt.0 .and. nudge_tem(k).ne.DMISS ) then
               const_sour(ITEMP,k) = const_sour(ITEMP,k) + nudge_tem(k) * Trefi
               const_sink(ITEMP,k) = const_sink(ITEMP,k) + Trefi
            end if
                 
            if ( ISALT.gt.0 .and. nudge_sal(k).ne.DMISS ) then
               const_sour(ISALT,k) = const_sour(ISALT,k) + nudge_sal(k) * Trefi
               const_sink(ISALT,k) = const_sink(ISALT,k) + Trefi
            end if
         end if
         
!        terms due to non-conservative formulation
         do j=1,NUMCONST
            const_sour(j,k) = const_sour(j,k) - constituents(j,k) * sq(k) * dvoli
         end do
      end do
      
!     Note: from now on, only _add_ to sources      
         
!     spiral flow source term
      if ( jasecflow > 0 .and. jaequili == 0 .and. kmx == 0 ) then
         if( spirucm(kk) < 1d-3 .or. hs(kk) < epshu ) then
            ! const_sour(ISPIR,kk) = 0d0
            ! const_sink(ISPIR,kk) = 0d0
         else
            fcoriocof = fcorio ; if( icorio > 0 .and. jsferic == 1 ) fcoriocof = fcoris(kk) 
            alpha = sqrt( ag ) / vonkar / max( czssf(kk), 20.0d0 )
            spir_ce = fcorio * hs(kk) * 0.5d0
            spir_be = hs(kk) * spircrv(kk) * spirucm(kk)
            spir_e  = spir_be - spir_ce
            alength_a = ( 1.0d0 - 2.0d0 * alpha ) * hs(kk) / ( 2.0d0 * vonkar * vonkar * alpha )  !TO DO: this term should be expanded to prevent negative alength_a for alpha > 0.5
            time_a    = alength_a / spirucm(kk)
            !const_sour(ISPIR,kk) =  ( spir_e - spirint(kk) ) / time_a !* dvoli    ! S=(I_eq - I)/Ta
            const_sour(ISPIR,kk) = const_sour(ISPIR,kk) + spir_e / time_a
            const_sink(ISPIR,kk) = const_sink(ISPIR,kk) + 1d0    / time_a
         endif
      end if
      
!     sediment (2D sources, also in 3D)
!          
      if ( stm_included ) then
         if ( ISED1.gt.0 .and. kk.le.Ndxi ) then
            do i=1,mxgr
               kkk = sedtra%kmxsed(kk,i)
               if ( kkk.gt.0 ) then
                  iconst = i+ISED1-1
                  const_sour(iconst,kkk) = const_sour(iconst,kkk)+sedtra%sourse(kk,i)
                  const_sink(iconst,kkk) = const_sink(iconst,kkk)+sedtra%sinkse(kk,i)
                  if (stmpar%morpar%flufflyr%iflufflyr .gt. 0) then
                     const_sour(iconst,kkk) = const_sour(iconst,kkk) + stmpar%morpar%flufflyr%sourf(i,kk)
                     const_sink(iconst,kkk) = const_sink(iconst,kkk) + stmpar%morpar%flufflyr%sinkf(i,kk)
                  end if

                 ! BEGIN DEBUG
                 ! if ( constituents(iconst,kb)+dts*const_sour(iconst,kb).lt.0d0 ) then
                 !    write(message, "('const. source < -const/dt, iconst=', I0, ', kk=', I0)") iconst, kk
                 !    call mess(LEVEL_WARN, trim(message))
                 ! end if
                 ! END DEBUG
               end if
            end do
         end if
      end if
   end do
   
   ! NOTE: apply_tracer_bc has been moved earlier to transport() routine,
   !       but apply_sediment_bc must still be done here, since above the boundary
   !       nodes's constituents(kb,:) = sed(kb,:) has reset it to 0.
   if ( stm_included ) call apply_sediment_bc()
   if (jas == 0) return                    ! no sources from initialise
   
   do n  = 1,numsrc
      kk     = ksrc(1,n)                   ! 2D pressure cell nr FROM
      kk2    = ksrc(4,n)                   ! 2D pressure cell nr TO
      qsrckk = qsrc(n) 
      qsrck  = qsrckk  
      
      jamba_src = jamba
      if (jampi.eq.1) then
         if(kk > 0) then
            if ( idomain(kk) /= my_rank ) jamba_src = 0
         else
            if(kk2 > 0) then
               if ( idomain(kk2) /= my_rank ) jamba_src = 0
            else
               jamba_src = 0
            endif
         endif
      endif

      if (jamba_src > 0) then
         if (qsrck > 0) then
            mbaflowsorsin(2,n) = mbaflowsorsin(2,n) + qsrck*dts
         else if (qsrck < 0) then
            mbaflowsorsin(1,n) = mbaflowsorsin(1,n) - qsrck*dts
         endif
      endif
         
      if (kk > 0) then                     ! FROM Point
         do k = ksrc(2,n) , ksrc(3,n) 
            dvoli  = 1d0/max(vol1(k),dtol)
            if (kmx > 0) then
               dzss  = zws(ksrc(3,n)) - zws(ksrc(2,n)-1) 
               if (dzss > epshs) then 
                  qsrck = qsrckk*( zws(k) - zws(k-1) ) / dzss
               else    
                  qsrck = qsrckk/( ksrc(3,n) - ksrc(2,n) + 1)
               endif
            endif   
            if (qsrck > 0) then              ! FROM k to k2
               do L = 1,numconst
                  const_sour(L,k) = const_sour(L,k) - qsrck*constituents(L,k)*dvoli
                  if (jamba_src > 0) then
                     s = iconst2sys(L)
                     if (s > 0 .and. s <= nosys) then
                       mbafluxsorsin(2,1,s,n) = mbafluxsorsin(2,1,s,n) + qsrck*constituents(L,k)*dts
                     endif
                  endif
               enddo   
            else if  (qsrck  < 0) then       ! FROM k2 to k
               do L = 1,numconst
                  const_sour(L,k) = const_sour(L,k) - qsrck*ccsrc(L,n)*dvoli
                  if (jamba_src > 0) then
                     s = iconst2sys(L)
                     if (s > 0 .and. s <= nosys) then
                        mbafluxsorsin(1,1,s,n) = mbafluxsorsin(1,1,s,n) - qsrck*ccsrc(L,n)*dts
                     endif
                  endif
               enddo   
            endif
         enddo   
      endif

      if (kk2 > 0) then                   ! TO Point
         do k = ksrc(5,n) , ksrc(6,n) 
            dvoli = 1d0/max(vol1(k),dtol)
            if (kmx > 0) then 
               dzss  = zws(ksrc(6,n)) - zws(ksrc(5,n)-1) 
               if (dzss > epshs) then 
                  qsrck = qsrckk*( zws(k) - zws(k-1) ) / dzss
               else    
                  qsrck = qsrckk/( ksrc(6,n) - ksrc(5,n) + 1)
               endif
            endif   
            if (qsrck > 0) then
               do L = 1,numconst
                  const_sour(L,k) = const_sour(L,k) + qsrck*ccsrc(L,n)*dvoli
                  if (jamba_src > 0) then
                     s = iconst2sys(L)
                     if (s > 0 .and. s <= nosys) then
                        mbafluxsorsin(2,2,s,n) = mbafluxsorsin(2,2,s,n) + qsrck*ccsrc(L,n)*dts
                     endif
                  endif
               enddo   
            else if  (qsrck  < 0) then  
               do L = 1,numconst
                  const_sour(L,k) = const_sour(L,k) + qsrck*constituents(L,k)*dvoli
                  if (jamba_src > 0) then
                     s = iconst2sys(L)
                     if (s > 0 .and. s <= nosys) then
                        mbafluxsorsin(1,2,s,n) = mbafluxsorsin(1,2,s,n) -  qsrck*constituents(L,k)*dts
                     endif
                  endif
               enddo   
            endif
         enddo   
      endif  

   enddo

   return
end subroutine fill_constituents

subroutine fill_rho()
   use m_transport
   use m_flowgeom
   use m_flow
   use m_sediment
   use m_transport
   use m_sferic
   use m_flowtimes , only : dnt
   implicit none

   integer          :: kk, k, kb, kt
   double precision :: dvoli, dtol=1d-8
  
   do k=1,Ndkx
      constituents(1,k) = rho(k)
   enddo
   
!  sources
   do kk=1,Ndx
      call getkbotktop(kk,kb,kt)
      do k=kb,kt
         dvoli = 1d0/max(vol1(k),dtol)
         const_sour(1,k) = - rho(k) * sq(k) * dvoli
         const_sink(1,k) = 0d0 
      end do
   enddo    
   
   return
end subroutine fill_rho
   
!> extract constituent array
subroutine extract_constituents()
   use m_transport
   use m_flow
   use m_flowgeom
   use m_sediment
   use m_transport
   use messageHandling
   use m_missing
   use m_plotdots
   implicit none
   
   integer :: i, iconst, k, kk, limmin, limmax
   
   double precision :: dmin
     
   do k=1,Ndkx
      if ( ISALT.ne.0 ) then
         sa1(k) = constituents(ISALT,k)
      end if
   
      !if ( ITEMP.ne.0 ) then
         ! tem1(k) = constituents(ITEMP,k)
      !end if
   
      if( jasecflow > 0 .and. jaequili == 0 .and. kmx == 0 ) then
         spirint(k) = constituents(ISPIR,k)
      endif
   
      if ( ISED1.ne.0 ) then
         do i=1,mxgr
            iconst = ISED1+i-1
            sed(i,k) = constituents(iconst,k)
         end do
      end if
   end do
   
   if (jatem .ne. 0) then  
      if (tempmax .ne. dmiss) then ! tem is now positive
         limmax = 0 
         do k = 1, Ndkx
            if (constituents(itemp,k) > tempmax) then 
                constituents(itemp,k) = tempmax
                limmax   = limmax + 1
            endif     
         enddo
         if (limmax .ne. 0) then 
            write(msgbuf , *) 'Max. temperature limited, number of cells Limmax = ' , limmax  ; call msg_flush()
         endif   
      endif   
      if (tempmin .ne. dmiss) then ! tem is now positive
         limmin = 0 
         do k = 1, Ndkx
            if (constituents(itemp,k) < tempmin) then 
                constituents(itemp,k) = tempmin
                limmin   = limmin + 1
            endif     
         enddo
         if (limmin .ne. 0) then 
            write(msgbuf , *) 'Min. temperature limited, number of cells Limmin = ' , limmin  ; call msg_flush()
         endif   
      endif   

   endif

   if (jasal .ne. 0) then  
      limmax = 0 ; limmin = 0 ; numdots = 0
      dmin = huge(1d0)
      do kk = 1, Ndxi
         if (salimax .ne. dmiss) then  
            do k = kbot(kk),ktop(kk) 
               if (sa1(k) > salimax) then 
                  sa1(k)  = salimax
                  limmax  = limmax + 1
               endif     
            enddo   
         endif
 
         do k = kbot(kk),ktop(kk) 
            if (sa1(k) < salimin) then 
               !if (sa1(k) < -1d-4) then 
               !   call adddot( xz(kk) , yz(kk), sa1(k) )  
               !endif   
               dmin    = min(dmin,sa1(k))
               sa1(k)  = salimin
               limmin  = limmin + 1
            endif   
         enddo   
      enddo   
      
      if (limmax .ne. 0) then 
         write(msgbuf , *) 'Max. salinity limited, number of cells Limmax = ' , limmax  ; call msg_flush()
      endif   
      if (limmin .ne. 0) then 
         write(msgbuf , *) 'Min. salinity limited, number of cells Limmin = ' , limmin  ; call msg_flush()
         write(msgbuf , *) 'Min. salinity limited, min = ' , dmin  ; call msg_flush()
      endif   
  endif
   
  if (jasal > 0 .and. maxitverticalforestersal > 0 .or. jatem > 0 .and. maxitverticalforestertem > 0) then  
     call doforester()
  endif 
     
  return
end subroutine extract_constituents

subroutine extract_rho()
   use m_transport
   use m_flow
   use m_sediment
   use m_transport
   implicit none
   
   integer :: k
   
   do k=1,Ndkx
      rho(k) = constituents(1,k)
      if ( ISALT.ne.0 ) then
         constituents(ISALT,k) = sa1(k)
      endif   
   enddo
   
   return
end subroutine extract_rho

!> add tracer to constituents, or get constituents number if tracer already exists
subroutine add_tracer(tracer_name, iconst)
   use m_transport
   use unstruc_messages
   use m_meteo, only: numtracers, trnames
   implicit none
   
   character(len=*), intent(in)  :: tracer_name  !< tracer name, or '' for default name
   integer,          intent(out) :: iconst       !< constituent number
   
   character(len=8)              :: str
   
   integer                       :: ierror, i, itrac
   
   integer, external             :: findname
   
   ierror = 1
   
!  check if tracer already exists, based on tracer name
   iconst = 0
   if ( ITRA1.gt.0 .and. trim(tracer_name).ne.'') then
      iconst = findname(NUMCONST, const_names, tracer_name)
      
      if ( iconst.ge.ITRA1 .and. iconst.le.ITRAN ) then  ! existing tracer found
         call mess(LEVEL_INFO, 'add_tracer: tracer ' // trim(tracer_name) // ' already exists. No update required.')
         goto 1234
      end if
   end if
   
!  append tracer
   if ( ITRA1.eq.0) then
      NUMCONST = NUMCONST+1
      ITRA1    = NUMCONST
      ITRAN    = NUMCONST
   else
!     check if tracers are at the back
      if ( iTRAN.ne.NUMCONST ) then
         call mess(LEVEL_ERROR, 'add_tracer: tracer(s) not at the back of the constituents array')
         goto 1234
      end if
      
      NUMCONST = NUMCONST+1
      ITRAN    = NUMCONST
   end if
   
!  output tracer number
   iconst = ITRAN
   
!  reallocate arrays
   call alloc_transport(.true.)
   
!  set name
   if ( trim(tracer_name).ne.'' ) then
      const_names(ITRAN) = trim(tracer_name)
   else
      write(str,"(I0)") ITRAN-ITRA1+1
      const_names(ITRAN) = 'tracer_'//trim(str)
   end if
   
   if ( numtracers.gt.0 ) then   ! number of tracers with boundary conditions
!     generate tracer (boundary condition) to constituent
      itrac = findname(numtracers,trnames,trim(tracer_name))
      if ( itrac.gt.0 ) then
         itrac2const(itrac) = iconst
      end if
   end if
   
   ierror = 0
   
1234 continue
   
   return
end subroutine add_tracer


subroutine comp_sq(Ndkx, Lnkx, kbot, ktop, Lbot, Ltop, q1, qw, sq)
   use m_flowgeom, only: Ndx, Lnx, ln
   implicit none
   
   integer,                                intent(in)    :: Ndkx     !< total number of flownodes (dynamically changing)
   integer,                                intent(in)    :: Lnkx     !< total number of flowlinks (dynamically changing)
   integer,          dimension(Ndx),       intent(in)    :: kbot     !< flow-node based layer administration
   integer,          dimension(Ndx),       intent(in)    :: ktop     !< flow-node based layer administration
   integer,          dimension(Lnx),       intent(in)    :: Lbot     !< flow-link based layer administration
   integer,          dimension(Lnx),       intent(in)    :: Ltop     !< flow-link based layer administration
   double precision, dimension(Lnkx),      intent(in)    :: q1       !< flow-field discharges
   double precision, dimension(Ndkx),      intent(in)    :: qw       !< flow-field vertical discharges
   double precision, dimension(Ndkx),      intent(out)   :: sq       !< flux balance (inward positive)
   
   double precision                                      :: dum, sumsq
   
   integer                                               :: k1, k2, k, kk, L, LL
   
   sq = 0d0
   
!   write(6,*) q1(1714)-q1(1735)
!   write(6,*) qw(1736)-qw(1735)

!  vertical
   do kk=1,Ndx
      do k = kbot(kk),ktop(kk)-1
         sq(k)   = sq(k)   - qw(k)
         sq(k+1) = sq(k+1) + qw(k)
      end do
      
      if ( abs(qw(ktop(kk))).gt.1d-8 ) then
         continue
      end if
   
      if ( abs(qw(kbot(kk)-1)).gt.1d-8 ) then
         continue
      end if
   end do
   
!   write(6,"('after vertical: ',E)") sq(1736)
   
!  horizontal
   do LL=1,Lnx
      do L=Lbot(LL),Ltop(LL)
         k1 = ln(1,L)
         k2 = ln(2,L)
         sq(k1) = sq(k1) - q1(L)
         sq(k2) = sq(k2) + q1(L)
      end do    
   end do
   
!   write(6,"('after horizontal: ',E)") sq(1736)
   
   sumsq = 0d0
   do kk=1,Ndx
      do k=kbot(kk),ktop(kk)
         sumsq = sumsq + sq(k)
      end do
   end do
   
!   write(6,*) sumsq
   
   return
end subroutine comp_sq


subroutine droptracer(xp, yp, dval)
   use m_transport
   use m_flowgeom
   use m_flow, only: kmxn, kbot
   use m_polygon
   use m_missing, only: dmiss, JINS
   use geometry_module, only: dbpinpol
   
   implicit none
   
   double precision :: xp, yp   !< point coordinates
   double precision :: dval     !< value
   
   integer, dimension(:), allocatable :: icelllist
   integer                            :: Ncells
   
   integer                            :: i, k, kk, kb, kt
   integer                            :: N, in
   integer                            :: ja
   integer                            :: iconst
   
!  allocate
   allocate(icelllist(Ndx))
   icelllist = 0
   
!  add a tracer if no current tracer is selected (in visualization)  
   if ( ITRA1.eq.0 .or. iconst_cur.eq.0 .or. iconst_cur.lt.ITRA1 ) then ! note: tracers always at the back
      call add_tracer('', iconst)
!     set current tracer (for visualization)
      iconst_cur = iconst
   else
!     select current tracer
      iconst = iconst_cur
   end if
   
!  find active flow nodes
   Ncells = 0
   if ( NPL.le.2 ) then ! no (usable) polygon
      call in_flowcell(xp,yp,kk)
      if ( kk.gt.0 ) then
         Ncells = Ncells + 1
         icelllist(1) = kk
      end if
   else
      in = -1
      do kk=1,Ndx
         N = size(nd(kk)%x)
         call dbpinpol(xz(kk), yz(kk), in, dmiss, JINS, NPL, xpl, ypl, zpl)
         if ( in.eq.1 ) then
            Ncells = Ncells+1
            icelllist(Ncells) = kk
         end if
      end do
   end if
      
!  fill active flow nodes
   do i=1,Ncells
      kk = icelllist(i)
      do k=kbot(kk),kbot(kk) + kmxn(kk) - 1 
         constituents(iconst,k) = constituents(iconst,k) + dval
      end do
   end do
   
!  plot
   call tekflowstuff(ja)
   
!  deallocate
   if ( allocated(icelllist) ) deallocate(icelllist)
     
   return
end subroutine droptracer


!> find index of string in array of strings
integer function findname(N, snames, sname)
   implicit none
   
   integer,                        intent(in) :: N
   character(len=*), dimension(N), intent(in) :: snames
   character(len=*),               intent(in) :: sname
   
   integer :: i
   
   findname = 0

   do i=1,N
      if ( trim(sname).eq.trim(snames(i)) ) then
         findname = i
         return
      end if
   end do
   
   return
end function findname

!> Convert qid (from .ext file) to tracer name (split in generic qidname and specific tracer name).
!! If the input qid is not tracer, then the same qid is returned (and no tracer name)
subroutine get_tracername(qid, trname, qidname)
   use m_transport, only: DEFTRACER
   implicit none
   
   character(len=*), intent(in)  :: qid     !< Original quantityid, e.g., 'tracerbndfluor'.
   character(len=*), intent(out) :: trname  !< The trimmed tracer name, e.g., 'fluor'.
   character(len=*), intent(out) :: qidname !< The base quantity name for further use in external forcing, e.g., 'tracerbnd'.
   
   trname = ''
   qidname = qid
   
   if ( qid(1:9).eq.'tracerbnd' ) then
      qidname = qid(1:9)
      if ( len_trim(qid).gt.9 ) then
         trname = trim(qid(10:))
      else
         trname = trim(DEFTRACER)
      end if
   else if (qid(1:13).eq.'initialtracer' ) then
      qidname = qid(1:13)
      if ( len_trim(qid).gt.13 ) then
         trname = trim(qid(14:))
      else
         trname = trim(DEFTRACER)
      end if
   end if
   
   return
end subroutine get_tracername


!> apply tracer boundary conditions
subroutine apply_tracer_bc()
   use m_transport
   use m_meteo
   use m_flowgeom, only: ln
   use m_flow, only: kmxd, q1, kmxL
   implicit none
   
   character (len=NAMTRACLEN)    :: tracnam
   
   double precision :: valtop
   
   integer :: itrac, iconst
   integer :: k, kk, ki, kb
   integer :: L, LL, Lb, Lt
   
!  loop over the tracer boundary conditions
   do itrac=1,numtracers
      iconst = itrac2const(itrac)
      do k=1,nbndtr(itrac)
         LL = bndtr(itrac)%k(3,k)
         call getLbotLtop(LL,Lb,Lt)
         kb = 0
         do L = Lb,Lt
            kb = ln(1,L)
            ki = ln(2,L)
            if ( q1(L).gt.0 ) then  ! inflow
               kk = kmxd*(k-1)+L-Lb+1
               constituents(iconst,kb) = bndtr(itrac)%z(kk)
            else                    ! outflow
               constituents(iconst,kb) = constituents(iconst,ki)
            end if
         end do
         
         if ( kb.gt.0 ) then
            valtop = constituents(iconst,kb)
       
            do L=Lt+1,Lb+kmxL(LL)-1
               kb = ln(1,L)
               ki = ln(2,L)
               if ( q1(Lt).gt.0d0 ) then
                  constituents(iconst,kb) = valtop
               else
                  constituents(iconst,kb) = constituents(iconst,ki)
               end if
            end do
         end if
      end do
   end do
   
   return
end subroutine apply_tracer_bc 

!> get maximum timestep for water columns (see setdtorg)
subroutine get_dtmax()
   use m_flowgeom, only: Ndx, Ndxi, bl, nd, Dx, ln, lnx
   use m_flow, only: s1, epshu, squ, sqi, vol1, kmx, u1, hu, diusp, viu, Lbot, Ltop
   use m_flowparameters, only: eps10, cflmx, jadiusp
   use m_turbulence, only: sigdifi
   use m_flowtimes, only: time1
   use m_physcoef, only:  dicouv
   use m_timer
   use m_transport
   use m_partitioninfo

   implicit none
   
   double precision                              :: difcoeff
   double precision                              :: diuspL
   
   integer                                       :: kk, k, kb, kt
   integer                                       :: L, LL, Lb, Lt
   integer                                       :: k1, k2
   integer                                       :: j
   integer                                       :: ierror
   
   double precision,                 parameter   :: dtmax_default = 1d4
   
   dtmin_transp = huge(1d0)
   kk_dtmin = 0
   
   if ( jalimitdtdiff.eq.1 ) then
!     determine contribution of diffusion to time-step limitation, mosly copied from "comp_fluxhor3D"
      sumdifflim = 0d0
      do LL=1,Lnx
         if (jadiusp == 1) then 
             diuspL = diusp(LL)
         else
             diuspL = dicouv
         endif 
         
         Lb = Lbot(LL)
         Lt = Ltop(LL)
         
         do L=Lb,Lt
            k1 = ln(1,L)
            k2 = ln(2,L)
            
            difcoeff = 0d0
            
            
!           compute maximum diffusion coefficient     
            do j=1,NUMCONST
!              compute diffusion coefficient (copied from "comp_fluxhor3D")
               difcoeff  = max(difcoeff, sigdifi(j)*viu(L) + difsedu(j) + diuspL)  ! without smagorinsky, viu is 0 , 
                                                                                   ! difsed only contains molecular value, 
                                                                                   ! so then you only get user specified value  
            end do
               
            sumdifflim(k2) = sumdifflim(k2) + difcoeff*dxiAu(L)
            sumdifflim(k1) = sumdifflim(k1) + difcoeff*dxiAu(L)
         end do
      end do
   end if
   
   if ( kmx.eq.0 ) then
   
      do k=1,Ndxi
         dtmax(k) = dtmax_default
         
!         if ( s1(k)-bl(k).gt.epshu ) then
         
            if ( jalimitdtdiff.eq.0 ) then
               if ( squ(k).gt.eps10 ) then
                  dtmax(k) = min(dtmax(k),cflmx*vol1(k)/squ(k))
               end if
            else
               if ( sqi(k)+sumdifflim(k).gt. eps10 ) then
                  dtmax(k) = min(dtmax(k), cflmx*vol1(k)/(sqi(k)+sumdifflim(k)))
!                  dtmax = min(dtmax(k), cflmx*vol1(k)/(squ(k)+sumdifflim(k)))
               end if
            end if
         
! BEGIN DEBUG
!            do LL=1,nd(k)%lnx
!               L = iabs(nd(k)%ln(LL))
!               if ( hu(L).gt.0d0 .and. u1(L).gt.0d0 ) then
!                  dtmax(k) = min(dtmax(k),cflmx*Dx(L)/u1(L))
!               end if
!            end do
! END DEBUG
            
            if ( jampi.eq.1 ) then
!              do not include ghost cells
               if ( idomain(k).ne.my_rank ) cycle
            end if
            
            if ( dtmax(k).lt.dtmin_transp ) then
               dtmin_transp = dtmax(k)
               kk_dtmin = k
            end if
!         end if
      
      end do
   
   else
   
      do kk=1,Ndxi
         dtmax(kk) = dtmax_default
         
         if ( s1(kk)-bl(kk).gt.epshu ) then
            call getkbotktop(kk,kb,kt)
            if ( jalimitdtdiff.eq.0 ) then
               do k=kb,kt
                  if ( squ(k).gt.eps10 .or. sqi(k).gt.eps10 ) then
                     dtmax(kk) = min(dtmax(kk),vol1(k)/max(squ(k),sqi(k)))
                  end if
               end do
            else
               do k=kb,kt
                  if ( sqi(k)+sumdifflim(k).gt.eps10 ) then
                     dtmax(kk) = min(dtmax(kk),vol1(k)/(sqi(k)+sumdifflim(k)))
!                     dtmax(kk) = min(dtmax(kk),vol1(k)/(squ(k)+sumdifflim(k)))
                  end if
               end do
            end if
            dtmax(kk) = cflmx*dtmax(kk)
            
            if ( jampi.eq.1 ) then
!              do not include ghost cells
               if ( idomain(kk).ne.my_rank ) cycle
            end if
            
            if ( dtmax(kk).lt.dtmin_transp ) then
               dtmin_transp = dtmax(kk)
               kk_dtmin = kk
            end if
         end if
      
      end do
   
   end if
   
   time_dtmax = time1
   
   if ( jampi.eq.1 ) then
!     update dtmax
      call update_ghosts(ITYPE_Sall, 1, Ndx, dtmax, ierror)      
!     globally reduce maximum time-step  
      if ( jatimer.eq.1 ) call starttimer(IMPIREDUCE) 
      call reduce_double_min(dtmin_transp)
      if ( jatimer.eq.1 ) call stoptimer(IMPIREDUCE)
   end if
   
   return
end subroutine get_dtmax


!> get number of subtimesteps and delta subtimesteps
subroutine get_ndeltasteps()
   use m_flowgeom, only: Ndxi, Lnxi, Lnx, ln
   use m_flowtimes, only: dts
   use m_transport
   implicit none
   
   double precision                      :: dt, dtmin
   double precision                      :: logtwo
   
   integer                               :: kk, LL
   
   double precision, external            :: get_dt
   
   numnonglobal = 0
   
!  get smallest and largest time steps
   dtmin = dtmin_transp
         
   if ( dtmin.ge.dts ) then
      nsubsteps = 1
      ndeltasteps = 1
   else
      logtwo = log(2d0)
      nsubsteps = max(1,2**int(log(dts/dtmin)/logtwo+0.9999d0))
      dtmin = dts/nsubsteps
      
!     get number of substeps
      do kk=1,Ndxi
         dt = dtmax(kk)
         if ( dt.lt.dts ) then
            ndeltasteps(kk) = min(2**int(log(dt/dtmin)/logtwo),nsubsteps)
            numnonglobal = numnonglobal+1
         else
            ndeltasteps(kk) = nsubsteps
         end if
      end do
      
!     fictitious boundary cells
      do LL=Lnxi+1,Lnx
         ndeltasteps(ln(1,LL)) = ndeltasteps(ln(2,LL))
      end do
      
!      if ( nsubsteps.gt.1 ) then
!         write(6,*) dtmin
!      end if
   
   end if
   
   return  
end subroutine get_ndeltasteps

!> sum horizontal fluxes
subroutine comp_sumhorflux(NUMCONST, kmx, Lnkx, Ndkx, Lbot, Ltop, fluxhor, sumhorflux)
   use m_flowgeom, only: Lnx, Ln, nd, Ndx    ! static mesh information
   implicit none
   
   integer,                                    intent(in)    :: NUMCONST      !< number of transported quantities
   integer,                                    intent(in)    :: kmx           !< number of layers
   integer,                                    intent(in)    :: Ndkx          !< total number of flownodes (dynamically changing)
   integer,                                    intent(in)    :: Lnkx          !< total number of flowlinks (dynamically changing)
   integer,          dimension(Lnx),           intent(in)    :: Lbot          !< flow-link based layer administration
   integer,          dimension(Lnx),           intent(in)    :: Ltop          !< flow-link based layer administration
   double precision, dimension(NUMCONST,Lnkx), intent(in)    :: fluxhor       !< horizontal advection fluxes
   double precision, dimension(NUMCONST,Ndkx), intent(inout) :: sumhorflux    ! sum of horizontal fluxes, dim(NUMCONST,Ndkx)
   
   integer :: LL, L, Lb, Lt
   integer :: j, k1, k2
   integer :: k
   
   if ( kmx.lt.1 ) then
!     add horizontal fluxes to right-hand side
      do L=1,Lnx
!        get neighboring flownodes
         k1 = ln(1,L)
         k2 = ln(2,L)
         do j=1,NUMCONST
            sumhorflux(j,k1) = sumhorflux(j,k1) - fluxhor(j,L)
            sumhorflux(j,k2) = sumhorflux(j,k2) + fluxhor(j,L)
         end do
      end do
   else
!     add horizontal fluxes to right-hand side
      do LL=1,Lnx
         Lb = Lbot(LL)
         Lt = Ltop(LL)
         do L=Lb,Lt
!           get neighboring flownodes
            k1 = ln(1,L)
            k2 = ln(2,L)
            do j=1,NUMCONST
               sumhorflux(j,k1) = sumhorflux(j,k1) - fluxhor(j,L)
               sumhorflux(j,k2) = sumhorflux(j,k2) + fluxhor(j,L)
            end do
         end do
      end do
   end if
   
   return
end subroutine comp_sumhorflux

!> determine if the cells have to be updated (1) or not (0)
subroutine get_jaupdate(istep,nsubsteps,Ndxi,Ndx,ndeltasteps,jaupdate)
   implicit none
   
   integer,                  intent(in)  :: istep        !< substep number
   integer,                  intent(in)  :: nsubsteps    !< number of substeps
   integer,                  intent(in)  :: Ndxi         !< number of cells, excluding virtual boundary cells
   integer,                  intent(in)  :: Ndx          !< number of cells, including virtual boundary cells
   integer, dimension(Ndx),  intent(in)  :: ndeltasteps  !< number of substeps between updates
   integer, dimension(Ndx),  intent(out) :: jaupdate     !< update cell (1) or not (0)
   
   integer                               :: kk
   integer                               :: num
   

   jaupdate = 0
   
   num = 0
   do kk=1,Ndxi
      if ( mod(istep+1, ndeltasteps(kk)).eq.0 ) then
!      if ( int((istep+1)/ndeltasteps(kk))*ndeltasteps(kk).eq.istep+1 ) then
          jaupdate(kk) = 1
          num = num+1
      end if
   end do
   
!  BEGIN DEBUG
!   jaupdate = 1
!  END DEBUG
   
!   if ( istep.lt.nsubsteps ) then
!      write(6,"(I0,':',I0, ' ', $)") istep+1, num
!   end if
   
   return
end subroutine get_jaupdate

!> determine if the horizontal fluxes have to be updated (1) or not (0) from cell-based mask
subroutine get_jaupdatehorflux(nsubsteps, limtyp, jaupdate,jaupdatehorflux)
   use m_flowgeom,  only: Ndx, Lnx, ln, klnup
   implicit none
   
   integer,                  intent(in)  :: nsubsteps       !< number of substeps
   integer,                  intent(in)  :: limtyp          !< limited higher-order upwind (>0) or first-order upwind (0)
   integer, dimension(Ndx),  intent(in)  :: jaupdate        !< cell updated (1) or not (0)
   integer, dimension(Lnx),  intent(out) :: jaupdatehorflux !< update horizontal flux (1) or not (0)
   
   integer                               :: kk, k1, k2, LL
   integer                               :: kk1L, kk2L
   integer                               :: kk1R, kk2R
   
   if ( nsubsteps.eq.1 ) then
      jaupdatehorflux = 1
   else
      jaupdatehorflux = 0
      if ( limtyp.eq.0 ) then
         do LL=1,Lnx
            k1 = ln(1,LL)
            k2 = ln(2,LL)
            if ( jaupdate(k1).eq.1 .or. jaupdate(k2).eq.1 ) then
               jaupdatehorflux(LL) = 1 ! also for diffusion
            end if
         end do
      else
         do LL=1,Lnx
            k1 = ln(1,LL)
            k2 = ln(2,LL)
            if ( jaupdate(k1).eq.1 .or. jaupdate(k2).eq.1 ) then
               jaupdatehorflux(LL) = 1 ! also for diffusion
               cycle
            end if
            
            kk1L = klnup(1,LL)
            if ( kk1L.ne.0 ) then
               if ( jaupdate(iabs(kk1L)).eq.1 ) then
                  jaupdatehorflux(LL) = 1
                  cycle
               end if
               
               if ( kk1L.gt.0 ) then
                  kk2L = klnup(2,LL)
                  if ( jaupdate(iabs(kk2L)).eq.1 ) then
                     jaupdatehorflux(LL) = 1
                     cycle
                  end if
               end if
            end if
            
            kk1R = klnup(4,LL)
            if ( kk1R.ne.0 ) then
               if ( jaupdate(iabs(kk1R)).eq.1 ) then
                  jaupdatehorflux(LL) = 1
                  cycle
               end if
               
               if ( kk1R.gt.0 ) then
                  kk2R = klnup(5,LL)
                  if ( jaupdate(iabs(kk2R)).eq.1 ) then
                     jaupdatehorflux(LL) = 1
                  end if
               end if
            end if
         end do
      end if
   end if
   
   return
end subroutine get_jaupdatehorflux
   
subroutine doforester() 
use m_flow    ,   only : sa1, vol1, ndkx, kbot, ktop, kmxn, ndkx, maxitverticalforestersal, maxitverticalforestertem 
use m_flowgeom,   only : ndx, ndxi  
use m_turbulence, only : kmxx
use m_transport,  only : constituents, numconst, itemp

implicit none

integer          :: kk, km, kb 
double precision :: a(kmxx), d(kmxx)

do kk = 1,ndxi  
   km = ktop(kk) - kbot(kk) + 1
   if (maxitverticalforestersal > 0) then 
      call foresterpoint(sa1(kbot(kk):), vol1(kbot(kk):), a, d, km, kmxn(kk), maxitverticalforestersal, 1) ! foresterpoint
   endif
   if (maxitverticalforestertem > 0) then 
      call foresterpoint2(constituents, numconst, ndkx, itemp, vol1(kb:), a, d, km, kmxn(kk), kb, maxitverticalforestertem, -1)
   endif   
enddo   
   
end subroutine doforester

subroutine comp_horfluxtot()
   use m_flowgeom, only: Lnx
   use m_flow, only: Lbot, Ltop, kmx, Lnkx 
   use m_transport, only: ISED1, ISEDN, fluxhor, fluxhortot, sinksetot, sinkftot
   use m_flowtimes, only: dts
   implicit none
   
  
   integer :: LL, L, Lb, Lt
   integer :: j
   
   if ( kmx<1 ) then
      do L=1,Lnx
         do j=ISED1, ISEDN
            fluxhortot(j,L) = fluxhortot(j,L) + fluxhor(j,L) * dts
         end do
      end do
   else
      do LL=1,Lnx
         Lb = Lbot(LL)
         Lt = Ltop(LL)
         do L=Lb,Lt
            do j=ISED1, ISEDN
               fluxhortot(j,L) = fluxhortot(j,L) + fluxhor(j,L) * dts
            end do
         end do
      end do
   end if

end subroutine comp_horfluxtot

subroutine comp_horfluxwaq()
   use m_flowgeom, only: Lnx
   use m_flow, only: Lbot, Ltop, kmx, Lnkx, q1 
   use m_transport, only: NUMCONST, fluxhor
   use m_flowtimes, only: dts
   use m_fm_wq_processes
   implicit none
   
  
   integer :: LL, L, Lb, Lt, k1, k2, i
   integer :: isys, iconst
   
   do i=1,nombaln
      LL = mbalnlist(i)
      Lb = Lbot(LL)
      Lt = Ltop(LL)
      k1 = mbalnfromto(1,i)
      k2 = mbalnfromto(2,i)
      do L=Lb,Lt
         if (q1(L).gt.0.0) then
            mbaflowhor(2,k1,k2) = mbaflowhor(2,k1,k2) + q1(L) * dts
            mbaflowhor(1,k2,k1) = mbaflowhor(1,k2,k1) + q1(L) * dts
         else
            mbaflowhor(1,k1,k2) = mbaflowhor(1,k1,k2) - q1(L) * dts
            mbaflowhor(2,k2,k1) = mbaflowhor(2,k2,k1) - q1(L) * dts
         endif
      end do
   end do

   do isys=1,nosys
      iconst = isys2const(isys)
      do i=1,nombaln
         LL = mbalnlist(i)
         Lb = Lbot(LL)
         Lt = Ltop(LL)
         k1 = mbalnfromto(1,i)
         k2 = mbalnfromto(2,i)
         do L=Lb,Lt
            if (fluxhor(iconst,L).gt.0.0) then
               mbafluxhor(2,isys,k1,k2) = mbafluxhor(2,isys,k1,k2) + fluxhor(iconst,L) * dts
               mbafluxhor(1,isys,k2,k1) = mbafluxhor(1,isys,k2,k1) + fluxhor(iconst,L) * dts
            else
               mbafluxhor(1,isys,k1,k2) = mbafluxhor(1,isys,k1,k2) - fluxhor(iconst,L) * dts
               mbafluxhor(2,isys,k2,k1) = mbafluxhor(2,isys,k2,k1) - fluxhor(iconst,L) * dts
            endif
         end do
      end do
   end do

end subroutine comp_horfluxwaq

!subroutine update_constituents_RK3
!   use m_flowgeom,   only: Ndx, Ndxi, Lnxi, Lnx, ln, nd  ! static mesh information
!   use m_flow,       only: Ndkx, Lnkx, u1, q1, au, qw, zws, sq, sqi, vol1, kbot, ktop, Lbot, Ltop,  kmxn, kmxL, kmx, viu, vicwws, plotlin, jalts
!   use m_flowtimes,  only: dts, ja_timestep_auto
!   use m_turbulence, only: sigdifi
!   use m_physcoef,   only: dicoww, vicouv, difmolsal
!   use m_transport
!   use m_flowparameters, only: limtypsa, limtyptm, limtypsed, cffachor, cffacver
!   use m_alloc
!   use m_partitioninfo
!   use m_timer
!   use unstruc_messages
!   implicit none
!
!   integer :: jarhoonly 
!   
!   integer :: ierror
!
!   integer                                               :: limtyp  !< limiter type (>0), or first-order upwind (0)
!   double precision                                      :: dvoli
!   double precision                                      :: dts_store
!
!   integer                                               :: k, L, j, numconst_store
!   integer                                               :: istep
!   integer                                               :: numstepssync
!   
!   double precision, dimension(:,:), allocatable         :: constituents0, constituents1, constituents2
!   double precision, dimension(2)                        :: cffac_store
!   
!
!   if ( NUMCONST.eq.0 ) return  ! nothing to do
!
!   ierror = 1
!  
!   limtyp = max(limtypsa, limtyptm, limtypsed)
!
!   call fill_constituents()
!   
!   call get_dtmax()
!   
!   allocate(constituents0(NUMCONST,Ndkx))
!   allocate(constituents1(NUMCONST,Ndkx))
!   allocate(constituents2(NUMCONST,Ndkx))
!   
!!  store
!   dts_store = dts
!   cffac_store = (/ cffachor, cffacver /)
!   cffachor = 0
!   cffacver = 0
!   
!!  no local time-stepping
!   nsubsteps = 1
!   ndeltasteps = 1
!   numnonglobal = 0
!   
!   jaupdate = 1
!   
!   fluxhor = 0d0  ! not necessary
!   
!   if ( kmx.gt.0 ) then
!      fluxver = 0d0
!   end if
!   
!   do istep=1,3
!      sumhorflux = 0d0
!     
!      if ( istep.eq.1 ) then
!         constituents0 = constituents
!         dts = dts_store
!      else if ( istep.eq.2 ) then
!         constituents1 = constituents 
!         dts = 0.25*dts_store
!      else if ( istep.eq.3 ) then
!         constituents2 = constituents
!         dts = 2d0/3d0*dts_store
!      end if
!   
!!     compute horizontal fluxes, explicit part
!      call comp_fluxhor3D(NUMCONST, limtyp, Ndkx, Lnkx, u1, q1, au, sqi, vol1, kbot, Lbot, Ltop,  kmxn, kmxL, constituents, difsedu, sigdifi, viu, vicouv, nsubsteps, jaupdate, jaupdatehorflux, ndeltasteps, fluxhor)
!      
!      call starttimer(IDEBUG)
!      call comp_sumhorflux(NUMCONST, kmx, Lnkx, Ndkx, Lbot, Ltop, fluxhor, sumhorflux)
!      call stoptimer(IDEBUG)
!      
!      if ( kmx.gt.1 ) then
!         call comp_fluxver(  NUMCONST, limtyp, thetavert, Ndkx, kmx, zws, qw, kbot, ktop, constituents, nsubsteps, jaupdate, ndeltasteps, fluxver)
!      end if
!      
!      if ( istep.eq.2 ) then
!         constituents = 0.75*constituents0 + 0.25*constituents1
!      else if ( istep.eq.3 ) then
!         constituents = 1d0/3d0*constituents0 + 2d0/3d0*constituents2
!      end if
!         
!      if ( kmx.le.1 ) then
!         call solve_2D(NUMCONST, Ndkx, Lnkx, vol1, kbot, ktop, Lbot, Ltop, sumhorflux, fluxver, const_sour, const_sink, nsubsteps, jaupdate, ndeltasteps, constituents, rhs)
!      else
!         call solve_vertical(NUMCONST, ISED1, ISEDN, limtyp, thetavert, Ndkx, Lnkx, kmx,    &
!                             zws, qw, vol1, kbot, ktop, Lbot, Ltop,                     &
!                             sumhorflux, fluxver, const_sour, const_sink,                   &
!                             difsedw, sigdifi, vicwws, nsubsteps, jaupdate, ndeltasteps, constituents, &
!                             a, b, c, d, e, sol, rhs)
!      end if
!
!   
!   end do
!
!   if ( jampi.gt.0 ) then
!      if ( jatimer.eq.1 ) call starttimer(IUPDSALL)
!      if ( kmx.lt.1 ) then ! 2D
!         call update_ghosts(ITYPE_Sall, NUMCONST, Ndx, constituents, ierror)
!      else                 ! 3D
!         call update_ghosts(ITYPE_Sall3D, NUMCONST, Ndkx, constituents, ierror)
!      end if
!      if ( jatimer.eq.1 ) call stoptimer(IUPDSALL)
!   end if
!   
!   call extract_constituents()
!
!   ierror = 0
!1234 continue
!
!!  restore
!   dts = dts_store
!   cffachor = cffac_store(1)
!   cffacver = cffac_store(2)
!
!   if ( allocated(constituents0) ) deallocate(constituents0)
!   if ( allocated(constituents1) ) deallocate(constituents1)
!   if ( allocated(constituents2) ) deallocate(constituents2)
!
!   return
!end subroutine update_constituents_RK3

subroutine comp_sinktot()
   use m_transport
   use m_flow, only: vol1, kmx, ndkx
   use m_flowgeom, only: ndx
   use m_flowtimes, only: dts
   use m_sediment

   implicit none

   integer   :: k, j, kb, kt, ll
   !

   if (.not. stm_included) return
   if (mxgr == 0) return
   
   if (kmx<1) then    ! 2D
      do k=1,ndx
         do j=ISED1,ISEDN
            ll = j-ISED1+1
            sinksetot(j,k) = sinksetot(j,k) + vol1(k)*sedtra%sinkse(k,ll)*constituents(j,k)*dts
            if (stmpar%morpar%flufflyr%iflufflyr > 0) then
               sinkftot(j,k)  = sinkftot(j,k) + vol1(k)*stmpar%morpar%flufflyr%sinkf(ll,k)*constituents(j,k)*dts
            endif
         enddo
      enddo
   else               ! 3D
      do k=1,ndx
         call getkbotktop(k,kb,kt)
         do j=ISED1,ISEDN
            ll = j-ISED1+1
            sinksetot(j,k) = sinksetot(j,k) + vol1(sedtra%kmxsed(k,ll))*sedtra%sinkse(k,ll) *constituents(j,sedtra%kmxsed(k,ll))*dts
            if (stmpar%morpar%flufflyr%iflufflyr > 0) then
               sinkftot(j,k)  = sinkftot(j,k) +  vol1(sedtra%kmxsed(k,ll))*stmpar%morpar%flufflyr%sinkf(ll,k)*constituents(j,sedtra%kmxsed(k,ll))*dts
            endif
         enddo
      enddo
   endif

end subroutine comp_sinktot

! compute Au/Dx for diffusive flux
subroutine comp_dxiAu()
   use m_flowgeom, only: ln, Lnx, dxi, wu
   use m_flow, only: hs, zws, kmx, Au
   use m_transport, only : dxiAu, jalimitdtdiff
   implicit none
   
   integer :: k1, k2
   integer :: LL, L, Lb, Lt
   
   if ( jalimitdtdiff.eq.0 ) then
      if ( kmx.eq.0 ) then
         do L=1,Lnx
            dxiAu(L) = dxi(L)*Au(L)
         end do
      else
         do LL=1,Lnx
            call getLbotLtop(LL,Lb,Lt)
            do L=Lb,Lt
               dxiAu(L) = dxi(LL)*Au(L)
            end do
         end do
      end if
   else
      if ( kmx.eq.0 ) then
         do L=1,Lnx
            k1 = ln(1,L)
            k2 = ln(2,L)
            dxiAu(L) = dxi(L)*wu(L) * min(hs(k1), hs(k2))
         end do
      else
         do LL=1,Lnx
            call getLbotLtop(LL,Lb,Lt)
            do L=Lb,Lt
               k1 = ln(1,L)
               k2 = ln(2,L)
               dxiAu(L) = dxi(LL)*wu(LL) * min(zws(k1)-zws(k1-1),zws(k2)-zws(k2-1))
            end do
         end do
      end if
   end if
      
   
   return
end subroutine comp_dxiAu

subroutine sum_const(iter, vol1)
   use m_transport
   use m_flowgeom, only: Ndx
   use m_flow, only: Ndkx
   implicit none
   
   integer,                           intent(in) :: iter
   double precision, dimension(Ndkx), intent(in) :: vol1
   
   double precision, dimension(NUMCONST)         :: sum
                                                 
   integer                                       :: kk, k, kb, kt
   integer                                       :: j
   
   sum = 0d0
   
   do kk=1,Ndx
      call getkbotktop(kk,kb,kt)
      do k=kb,kt
         do j=1,NUMCONST
            sum(j) = sum(j) + vol1(k)*constituents(j,k)
         end do
      end do
   end do
   
   write(6,"(I5, ':', $)") iter
   do j=1,NUMCONST
      write(6,"(E25.15, $)") sum(j)
   end do
   write(6,*)
   
   
   return
end subroutine sum_const
