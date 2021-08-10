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

   ! $Id: waves.f90 65778 2020-01-14 14:07:42Z mourits $
   ! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/waves.f90 $

   subroutine alloc9basicwavearrays()  
   use m_flow
   use m_flowgeom
   use m_waves 
   implicit none
   integer      :: ierr
   call realloc( hwav,    ndx,  stat=ierr, keepExisting = .false., fill = hwavuni)
   call aerr   ('hwav    (ndx)',     ierr, ndx)
   call realloc( twav,    ndx,  stat=ierr, keepExisting = .false., fill = twavuni)
   call aerr   ('twav    (ndx)',     ierr, ndx)
   call realloc( phiwav,  ndx,  stat=ierr, keepExisting = .false., fill = phiwavuni)
   call aerr   ('phiwav  (ndx)',     ierr, ndx)
   call realloc( rlabda,  ndx,  stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr   ('rlabda  (ndx)',     ierr, ndx)
   call realloc( uorb,    ndx,  stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr   ('uorb    (ndx)',     ierr, ndx)
   call realloc( taus,    ndx,  stat=ierr, keepExisting = .false., fill = 0d0)     
   call aerr   ('taus    (ndx)',     ierr, ndx)
   call realloc( ustk,    ndx,  stat=ierr, keepExisting = .false., fill = 0d0)     
   call aerr   ('ustk    (ndx)',     ierr, ndx)
   call realloc( ustokes, lnkx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr   ('ustokes(lnkx)',     ierr, lnkx)
   call realloc( vstokes, lnkx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr   ('vstokes(lnkx)',     ierr, lnkx) 
   end subroutine alloc9basicwavearrays

   subroutine flow_waveinit
   use m_flow
   use m_flowgeom
   use m_waves
   use m_xbeach_data
   use m_xbeach_avgoutput
   use m_xbeach_readkey
   use m_xbeach_filefunctions
   use m_xbeach_errorhandling
   use m_xbeach_paramsconst
   use M_SAMPLES
   use m_missing
   use m_alloc
   use m_sferic, only: jsferic, jasfer3D
   use m_polygon, only: NPL, xpl, ypl, zpl
   use m_ec_basic_interpolation, only: triinterp2
   use m_flowexternalforcings, only: transformcoef
   use dfm_error

   implicit none

   integer      :: ierr
   integer      :: minp0, jdla, nm, ibnd, kb, ki

   ierr = DFM_NOERR
 
   call realloc(uin, nbndw, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('uin  (nbndw)', ierr, nbndw)
   call realloc(vin, nbndw, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('vin  (nbndw)', ierr, nbndw)
   call realloc(u1rm, nbndu, stat=ierr, keepExisting=.false., fill=0d0)   ! remember u1 state
   call aerr('u1rm  (nbndu)', ierr, nbndu)

   call realloc(ypar, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ypar(lnx)', ierr, lnx)
   call realloc(cfwavhi, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('cfwavhi(lnx)', ierr, lnx)
   call realloc(cfhi_vanrijn, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('cfhi_vanrijn(lnx)', ierr, lnx)
   call realloc(taubxu, lnx, stat=ierr, keepExisting = .false., fill = 0d0)   ! Always needs to be allocated, even if jawave == 0, used in gettau()
   call aerr('taubxu(lnx)', ierr, lnx)
   call realloc(ktb, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ktb  (ndx)', ierr, ndx)
   call realloc(taux_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('taux_cc  (ndx)', ierr, ndx)
   call realloc(tauy_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('tauy_cc  (ndx)', ierr, ndx)
   call realloc(ust_mag, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ust_mag  (ndx)', ierr, ndx)
   call realloc(fwav_mag, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fwav_mag  (ndx)', ierr, ndx)
   call realloc(wblt, lnx, stat=ierr, keepExisting = .false., fill = 0d0  )
   call aerr('wblt(lnx)', ierr, lnx)

   if (jawave == 3) then
      call realloc(wavfu, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('wavfu  (lnx)', ierr, lnx)
      call realloc(wavfv, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('wavfv  (lnx)', ierr, lnx)
      call realloc(wavmubnd, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('wavmubnd  (lnx)', ierr, lnx)
      call realloc(sxwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sxwav  (ndx)', ierr, ndx)
      call realloc(sywav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sywav  (ndx)', ierr, ndx)
      call realloc(sbxwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sbxwav  (ndx)', ierr, ndx)
      call realloc(sbywav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sbywav  (ndx)', ierr, ndx)
      call realloc(uorbwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('uorbwav  (ndx)', ierr, ndx)
      call realloc(wlenwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('wlenwav  (ndx)', ierr, ndx)

      call realloc(mxwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('mxwav(ndx)', ierr, ndx)
      call realloc(mywav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('mywav(ndx)', ierr, ndx)

      call realloc(dsurf, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('dsurf(ndx)', ierr, ndx)
      call realloc(dwcap, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('dwcap(ndx)', ierr, ndx)
      call realloc(kdismx, lnx, stat=ierr, keepExisting = .false., fill = 0  )
      call aerr('kdismx(lnx)', ierr, lnx)

   end if
   if  (jawave > 0) then
       call realloc( hwavcom,   ndx, stat=ierr, keepExisting = .false., fill = hwavuni)
      call aerr   ('hwavcom   (ndx)', ierr, ndx)
    endif

   if (jawave .eq. 4) then
      call realloc(ee0, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ee0  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(ee1, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ee1  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(cwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('cwav  (ndx)', ierr, ndx)
      call realloc(cgwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('cgwav  (ndx)', ierr, ndx)
      call realloc(kwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('kwav  (ndx)', ierr, ndx)
      call realloc(km, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('km  (ndx)', ierr, ndx)
      call realloc(umwci, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('umwci  (ndx)', ierr, ndx)
      call realloc(vmwci, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('vmwci  (ndx)', ierr, ndx)
      call realloc(zswci, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('zswci  (ndx)', ierr, ndx)
      call realloc(nwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('nwav  (ndx)', ierr, ndx)
      call realloc(ctheta, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ctheta  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(sigmwav, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sigmwav  (ndx)', ierr, ndx)
      call realloc(sigt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('sigt  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(horadvec, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('horadvec  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(thetaadvec, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('thetaadvec  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(rhs, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rhs  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(rrhoradvec, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rrhoradvec  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(rrthetaadvec, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rrthetaadvec  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(rr, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rr  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(H, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('H  (ndx)', ierr, ndx)
      call realloc(fw, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('fw  (ndx)', ierr, ndx)
      call realloc(E, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('E  (ndx)', ierr, ndx)
      call realloc(DR, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('DR  (ndx)', ierr, ndx)
      call realloc(R, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('R  (ndx)', ierr, ndx)
      call realloc(rr, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('rr  (ntheta,ndx)', ierr, ntheta*ndx)
      call realloc(Sxx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Sxx  (ndx)', ierr, ndx)
      call realloc(Syy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Syy  (ndx)', ierr, ndx)
      call realloc(Sxy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Sxy  (ndx)', ierr, ndx)
      call realloc(Fx, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Fx  (lnx)', ierr, lnx)
      call realloc(Fy, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Fy  (lnx)', ierr, lnx)
      call realloc(Fx_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Fx_cc  (ndx)', ierr, ndx)
      call realloc(Fy_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Fy_cc  (ndx)', ierr, ndx)
      call realloc(urms, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('urms  (lnx)', ierr, lnx)
      call realloc(urms_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('urms_cc  (ndx)', ierr, ndx)
      call realloc(ust, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ust  (lnx)', ierr, lnx)
      call realloc(vst, lnx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('vst  (lnx)', ierr, lnx)

      call realloc(ustx_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ustx_cc  (ndx)', ierr, ndx)
      call realloc(usty_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('usty_cc  (ndx)', ierr, ndx)

      call realloc(dhsdx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('dhsdx  (ndx)', ierr, ndx)
      call realloc(dhsdy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('dhsdy  (ndx)', ierr, ndx)

      call realloc(thetamean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('thetamean  (ndx)', ierr, ndx)
      call realloc(Qb, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Qb  (ndx)', ierr, ndx)
      call realloc(D, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('D  (ndx)', ierr, ndx)
      call realloc(Df, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Df  (ndx)', ierr, ndx)
      call realloc(Dtot, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Dtot  (ndx)', ierr, ndx)
      call realloc(BR, ndx, stat=ierr, keepExisting = .false., fill = beta)
      call aerr('BR  (ndx)', ierr, ndx)
      call realloc(bi, nbndw, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('bi  (nbndw)', ierr, nbndw)
      !call realloc(rolthick, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      !call aerr('rolthick  (ndx)', ierr, ndx)
      !call realloc(kturb, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      !call aerr('kturb  (ndx)', ierr, ndx)
      call realloc(Tbore, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Tbore  (ndx)', ierr, ndx)
      call realloc(xbducxdx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('xbducxdx  (ndx)', ierr, ndx)
      call realloc(xbducydx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('xbducydx  (ndx)', ierr, ndx)
      call realloc(xbducxdy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('xbducxdy  (ndx)', ierr, ndx)
      call realloc(xbducydy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('xbducydy  (ndx)', ierr, ndx)
      call realloc(hdisp, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('hdisp  (ndx)', ierr, ndx)     
      
      if (windmodel .eq. 0) then
      call realloc(L1, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('L1  (ndx)', ierr, ndx)
      call realloc(Ltemp, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('Ltemp  (ndx)', ierr, ndx)
      call realloc(L0, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('L0  (ndx)', ierr, ndx)
         !call realloc(khdisp, ndx, stat=ierr, keepExisting = .false., fill = 0d0)   ML: unused
         !call aerr('khdisp  (ndx)', ierr, ndx)
      endif
      
      if (windmodel .eq. 1) then
         call realloc(tt1, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('tt1  (ntheta,ndx)', ierr, ntheta*ndx)      
         call realloc(cwavt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cwavt  (ntheta,ndx)', ierr, ntheta*ndx)   
         call realloc(cgwavt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cgwavt  (ntheta,ndx)', ierr, ntheta*ndx)     
         call realloc(kwavt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('kwavt  (ntheta,ndx)', ierr, ntheta*ndx)     
         call realloc(nwavt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('nwavt  (ntheta,ndx)', ierr, ntheta*ndx)     
         call realloc(horadvec2, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('horadvec2  (ntheta,ndx)', ierr, ntheta*ndx)     
         call realloc(thetaadvec2, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('thetaadvec2  (ntheta,ndx)', ierr, ntheta*ndx)       
         call realloc(L0t, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('L0t  (ntheta,ndx)', ierr, ntheta*ndx)              
         call realloc(L1t, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('L1t  (ntheta,ndx)', ierr, ntheta*ndx)   
         call realloc(Ltempt, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Ltempt  (ntheta,ndx)', ierr, ntheta*ndx)   
         call realloc(ma, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ma  (ntheta,ndx)', ierr, ntheta*ndx)   
         call realloc(mb, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('mb  (ntheta,ndx)', ierr, ntheta*ndx)    
         call realloc(wmagcc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('wmagcc  (ndx)', ierr, ntheta*ndx)   
         call realloc(windspreadfac, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('windspreadfac (ntheta,ndx)', ierr, ntheta*ndx)   
         call realloc(wsorE, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('wsorE  (ntheta,ndx)', ierr, ntheta*ndx)      
         call realloc(wsorT, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('wsorT  (ntheta,ndx)', ierr, ntheta*ndx)   
         call realloc(egradcg, (/ntheta,ndx/), stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('egradcg  (ntheta,ndx)', ierr, ntheta*ndx)   
         call realloc(ddT, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ddT  (ndx)', ierr, ntheta*ndx)   
         call realloc(SwE, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('SwE  (ndx)', ierr, ntheta*ndx)   
         call realloc(SwT, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('SwT  (ndx)', ierr, ntheta*ndx)   
      endif

      !    for stationary solver
      !call realloc(isweepup, (/2,ntheta*Ndxi/), stat=ierr, keepExisting = .false., fill = 0)
      !call aerr('isweepup (2*ntheta*ndxi)', ierr, 2*ntheta*Ndxi)
      !call realloc(isweepdown, (/2,ntheta*Ndxi/), stat=ierr, keepExisting = .false., fill = 0)
      !call aerr('isweepdown (2*ntheta*ndxi)', ierr, 2*ntheta*Ndxi)

      ! handle wave friction, has to be post-poned until here because of unavailability of ndx
      if (wavefricfile .ne. ' ') then
         call check_file_exist(wavefricfile)   ! if not, program will exit here
         call writelog('lws','(a,a,a)','Warning: wave friction coefficient values from file ''',&
            trim(wavefricfile), &
            ''' will be used in computation')
         call oldfil(minp0, wavefricfile)
         call reasam(minp0, 0)
         !
         jdla = 1
         fw  = dmiss
         CALL triinterp2(xz, yz, fw, ndx,JDLA, &
            XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
         !
         ! mirror boundary cells if undefined if equal to dmiss
         do ibnd = lnxi+1, lnx            ! loop over boundary flow links
            kb = ln(1,ibnd)      ! point outside net
            ki = ln(2,ibnd)      ! point inside net
            if (fw(kb) == dmiss) then
               fw(kb) = fw(ki)
            endif
         enddo
         ! if node value still equal to dmiss (values are not defined on flow nodes) - throw error
         do nm = 1, ndx  ! loop over flow nodes
            if (fw(nm) == dmiss) then
               call xbeach_errorhandler()
            endif
         enddo
         call delsam(-1)
         call doclose(minp0)

      else
         fw = wavefricval
      endif

      !if (jamombal>0) then    ! compute some gradients
      call realloc(xbdsdx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('xbdsdx  (ndx)', ierr, ndx)
      call realloc(xbdsdy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('xbdsdy  (ndx)', ierr, ndx)

      !end if

      if (jaavgwavquant .eq. 1) then            !! arrays for statistical output wave quantities
         call realloc(E_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('E_mean  (ndx)', ierr, ndx)
         call realloc(E_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('E_var  (ndx)', ierr, ndx)
         call realloc(E_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('E_min  (ndx)', ierr, ndx)
         call realloc(E_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('E_max  (ndx)', ierr, ndx)
         call realloc(E_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('E_varcross  (ndx)', ierr, ndx)
         call realloc(E_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('E_varsquare  (ndx)', ierr, ndx)

         call realloc(H_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('H_mean  (ndx)', ierr, ndx)
         call realloc(H_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('H_var  (ndx)', ierr, ndx)
         call realloc(H_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('H_min  (ndx)', ierr, ndx)
         call realloc(H_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('H_max  (ndx)', ierr, ndx)
         call realloc(H_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('H_varcross  (ndx)', ierr, ndx)
         call realloc(H_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('H_varsquare  (ndx)', ierr, ndx)

         call realloc(R_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('R_mean  (ndx)', ierr, ndx)
         call realloc(R_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('R_var  (ndx)', ierr, ndx)
         call realloc(R_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('R_min  (ndx)', ierr, ndx)
         call realloc(R_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('R_max  (ndx)', ierr, ndx)
         call realloc(R_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('R_varcross  (ndx)', ierr, ndx)
         call realloc(R_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('R_varsquare  (ndx)', ierr, ndx)

         call realloc(D_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('D_mean  (ndx)', ierr, ndx)
         call realloc(D_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('D_var  (ndx)', ierr, ndx)
         call realloc(D_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('D_min  (ndx)', ierr, ndx)
         call realloc(D_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('D_max  (ndx)', ierr, ndx)
         call realloc(D_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('D_varcross  (ndx)', ierr, ndx)
         call realloc(D_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('D_varsquare  (ndx)', ierr, ndx)

         call realloc(DR_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('DR_mean  (ndx)', ierr, ndx)
         call realloc(DR_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('DR_var  (ndx)', ierr, ndx)
         call realloc(DR_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('DR_min  (ndx)', ierr, ndx)
         call realloc(DR_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('DR_max  (ndx)', ierr, ndx)
         call realloc(DR_varcross, ndx, stat=ierr, keepExisting = .false., fill = tiny(0d0))
         call aerr('DR_varcross  (ndx)', ierr, ndx)
         call realloc(DR_varsquare, ndx, stat=ierr, keepExisting = .false., fill = tiny(0d0))
         call aerr('DR_varsquare  (ndx)', ierr, ndx)

         call realloc(ust_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ust_mean  (ndx)', ierr, ndx)
         call realloc(ust_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ust_var  (ndx)', ierr, ndx)
         call realloc(ust_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('ust_min  (ndx)', ierr, ndx)
         call realloc(ust_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('ust_max  (ndx)', ierr, ndx)
         call realloc(ust_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ust_varcross  (ndx)', ierr, ndx)
         call realloc(ust_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('ust_varsquare  (ndx)', ierr, ndx)

         call realloc(vst_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('vst_mean  (ndx)', ierr, ndx)
         call realloc(vst_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('vst_var  (ndx)', ierr, ndx)
         call realloc(vst_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('vst_min  (ndx)', ierr, ndx)
         call realloc(vst_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('vst_max  (ndx)', ierr, ndx)
         call realloc(vst_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('vst_varcross  (ndx)', ierr, ndx)
         call realloc(vst_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('vst_varsquare  (ndx)', ierr, ndx)

         call realloc(urms_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('urms_mean  (ndx)', ierr, ndx)
         call realloc(urms_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('urms_var  (ndx)', ierr, ndx)
         call realloc(urms_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('urms_min  (ndx)', ierr, ndx)
         call realloc(urms_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('urms_max  (ndx)', ierr, ndx)
         call realloc(urms_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('urms_varcross  (ndx)', ierr, ndx)
         call realloc(urms_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('urms_varsquare  (ndx)', ierr, ndx)

         call realloc(thetamean_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('thetamean_mean  (ndx)', ierr, ndx)
         call realloc(thetamean_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('thetamean_var  (ndx)', ierr, ndx)
         call realloc(thetamean_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('thetamean_min  (ndx)', ierr, ndx)
         call realloc(thetamean_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('thetamean_max  (ndx)', ierr, ndx)
         call realloc(thetamean_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('thetamean_varcross  (ndx)', ierr, ndx)
         call realloc(thetamean_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('thetamean_varsquare  (ndx)', ierr, ndx)
         call realloc(thetamean_sin, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('thetamean_sin  (ndx)', ierr, ndx)
         call realloc(thetamean_cos, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('thetamean_cos  (ndx)', ierr, ndx)

         call realloc(sigmwav_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('sigmwav_mean  (ndx)', ierr, ndx)
         call realloc(sigmwav_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('sigmwav_var  (ndx)', ierr, ndx)
         call realloc(sigmwav_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('sigmwav_min  (ndx)', ierr, ndx)
         call realloc(sigmwav_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('sigmwav_max  (ndx)', ierr, ndx)
         call realloc(sigmwav_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('sigmwav_varcross  (ndx)', ierr, ndx)
         call realloc(sigmwav_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('sigmwav_varsquare  (ndx)', ierr, ndx)

         call realloc(cwav_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cwav_mean  (ndx)', ierr, ndx)
         call realloc(cwav_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cwav_var  (ndx)', ierr, ndx)
         call realloc(cwav_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('cwav_min  (ndx)', ierr, ndx)
         call realloc(cwav_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('cwav_max  (ndx)', ierr, ndx)
         call realloc(cwav_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cwav_varcross  (ndx)', ierr, ndx)
         call realloc(cwav_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cwav_varsquare  (ndx)', ierr, ndx)

         call realloc(cgwav_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cgwav_mean  (ndx)', ierr, ndx)
         call realloc(cgwav_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cgwav_var  (ndx)', ierr, ndx)
         call realloc(cgwav_min, ndx, stat=ierr, keepExisting = .false., fill =huge(0d0))
         call aerr('cgwav_min  (ndx)', ierr, ndx)
         call realloc(cgwav_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('cgwav_max  (ndx)', ierr, ndx)
         call realloc(cgwav_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cgwav_varcross  (ndx)', ierr, ndx)
         call realloc(cgwav_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('cgwav_varsquare  (ndx)', ierr, ndx)

         call realloc(s1_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('s1_mean  (ndx)', ierr, ndx)
         call realloc(s1_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('s1_var  (ndx)', ierr, ndx)
         call realloc(s1_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('s1_min  (ndx)', ierr, ndx)
         call realloc(s1_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('s1_max  (ndx)', ierr, ndx)
         call realloc(s1_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('s1_varcross  (ndx)', ierr, ndx)
         call realloc(s1_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('s1_varsquare  (ndx)', ierr, ndx)

         call realloc(Fx_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fx_mean  (ndx)', ierr, ndx)
         call realloc(Fx_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fx_var  (ndx)', ierr, ndx)
         call realloc(Fx_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('Fx_min  (ndx)', ierr, ndx)
         call realloc(Fx_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('Fx_max  (ndx)', ierr, ndx)
         call realloc(Fx_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fx_varcross  (ndx)', ierr, ndx)
         call realloc(Fx_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fx_varsquare  (ndx)', ierr, ndx)

         call realloc(Fy_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fy_mean  (ndx)', ierr, ndx)
         call realloc(Fy_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fy_var  (ndx)', ierr, ndx)
         call realloc(Fy_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('Fy_min  (ndx)', ierr, ndx)
         call realloc(Fy_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('Fy_max  (ndx)', ierr, ndx)
         call realloc(Fy_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fy_varcross  (ndx)', ierr, ndx)
         call realloc(Fy_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('Fy_varsquare  (ndx)', ierr, ndx)

         call realloc(u_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('u_mean  (ndx)', ierr, ndx)
         call realloc(u_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('u_var  (ndx)', ierr, ndx)
         call realloc(u_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('u_min  (ndx)', ierr, ndx)
         call realloc(u_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('u_max  (ndx)', ierr, ndx)
         call realloc(u_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('u_varcross  (ndx)', ierr, ndx)
         call realloc(u_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('u_varsquare  (ndx)', ierr, ndx)

         call realloc(v_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('v_mean  (ndx)', ierr, ndx)
         call realloc(v_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('v_var  (ndx)', ierr, ndx)
         call realloc(v_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
         call aerr('v_min  (ndx)', ierr, ndx)
         call realloc(v_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
         call aerr('v_max  (ndx)', ierr, ndx)
         call realloc(v_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('v_varcross  (ndx)', ierr, ndx)
         call realloc(v_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
         call aerr('v_varsquare  (ndx)', ierr, ndx)
      end if
   end if
   end subroutine flow_waveinit

   subroutine tauwave()
   use m_sediment
   use m_sferic
   use m_flowparameters
   use m_flow, only: plotlin, rhog, rhomean, ag, s1, s0, hu, jaconveyance2D, hs, u1, v, taus, frcu, ifrcutp, cfuhi, huvli, z0ucur, z0urou !, wavmu
   use m_flowgeom
   use m_physcoef, only:  rhomean, ee, sag, vonkar
   use m_waves
   use m_bedform, only: bfmpar
   use unstruc_messages

   implicit none

   double precision           :: uorb1, k0, k0h, phigrid, phiwave, phi
   integer                    :: k, k1, k2, n, L, mout
   integer                    :: jatauw = 1
   double precision           :: hk, sh2hk,hksh2,rn,asg,ew,sxx,syy,dtau,shs, h2k, cc, cg, omeg, hminlwi
   double precision           :: dsk2, rk, astar, fw, hss, per, astarc, tauwav, taucur, tauwci, cdrag, z0, uorbu, tpu
   double precision           :: cz, frcn, uuu, vvv, umod, umodsq, cvalue, costu, sintu, abscos, uorbhs, waveps, u2dh
   double precision           :: xpar, ymxpar, lfc, cj, coeffb, coeffp, coeffq, ci,coeffa, coeffm, coeffn, yparL
   double precision           :: hpr, wu2, b21, ai, BL1, BL2, ust, ac1, ac2
   double precision           :: ar, alfaw, wbl, rz, cf, cwall
   double precision           :: a, ks, phivr
   double precision           :: hrmsu, rlabdau, rr,umax,t1,u11,a11,raih,rmax, uon, uoff, uwbih
   double precision           :: rksru, rksmru, gamma, ksc, uratio, ka, ca
   double precision           :: cosk1, cosk2, sink1, sink2
   integer                    :: ifrctyp

   double precision, external         :: tanhsafe, sinhsafe, sinhsafei

   double precision, dimension(8)             :: coeffi     ! Coefficient i in expression for parametrized models
   double precision, dimension(8)             :: coeffj     ! Coefficient j in expression for parametrized models
   double precision, dimension(8, 4)          :: aa         ! Coefficient a(i) in expression for parameter a
   double precision, dimension(8, 4)          :: bb         ! Coefficient b(i) in expression for parameter b
   double precision, dimension(8, 4)          :: mm         ! Coefficient m(i) in expression for parameter n
   double precision, dimension(8, 4)          :: nn         ! Coefficient n(i) in expression for parameter n
   double precision, dimension(8, 4)          :: pp         ! Coefficient p(i) in expression for parameter p
   double precision, dimension(8, 4)          :: qq         ! Coefficient q(i) in expression for parameter q

   waveps = 1d-8
   alfaw  = 20d0
   hminlwi = 1d0/hminlw

   ! parameterized bottom friction models

   do L = 1,lnx
      k1 = ln(1,L); k2 = ln(2,L)
      ac1 = acl(L); ac2 = 1d0-ac1
      !
      ! Use Eulerian velocities
      uuu = u1(L) - ustokes(L)

      if (jaconveyance2D >=3 .or. L <= lnx1D ) then      ! based on subroutine furu
         vvv = 0.0d0
      else
         ! Use Eulerian velocities
         vvv = v(L) - vstokes(L)
      endif

      umodsq    = uuu*uuu + vvv*vvv
      umod      = max(1.0d-4, sqrt(umodsq))
      taubxu(L) = 0.0d0
      ypar(L)   = 0.0d0
      cfwavhi(L)= 0.0d0
      !
      ! TO DO: Replace the following messing with angles by an inproduct, without
      ! the expensive atan2 call -> requires acos call, and quadrant messing, so hardly cheaper
      !
      ! phigrid: angle between "normal direction on link" and "positive x-axis"
      phigrid = atan2(snu(L),csu(L)) * rd2dg
      ! phiwave: angle between "wave propagation direction" and "positive x-axis"
      !          Interpolate from nodes to links
      cosk1 = cos(phiwav(k1)*dg2rd); sink1 = sin(phiwav(k1)*dg2rd)
      cosk2 = cos(phiwav(k2)*dg2rd); sink2 = sin(phiwav(k2)*dg2rd)
      cosk1 = ac1*cosk1+ac2*cosk2; sink1 = ac1*sink1+ac2*sink2 
      !
      phiwave = atan2(sink1, cosk1)*rd2dg
      ! phi: angle between "wave propagation direction" and "normal direction on link"
      phi     = phiwave - phigrid

      ! interpolate uorbu, tpu and wavmu from flownodes to flowlinks
      uorbu = ac1*uorb(k1) + ac2*uorb(k2)
      tpu   = ac1*twav(k1) + ac2*twav(k2)

      ! get current related roughness height
      call getczz0(hu(L),dble(frcu(L)),ifrcutp(L),cz,z0)

      if (modind > 0) then
         if (hu(L) > epshu) then

            costu = dcos(dg2rd*phi)
            sintu = dsin(dg2rd*phi)

            astarc = 30.*pi**2     ! critical value for astar

            abscos = abs(uuu*costu + vvv*sintu)/umod
            !
            ! wave friction factor and drag coefficient
            !
            astar  = tpu*uorbu/z0

            if (astar>astarc) then
               fw = 0.00251d0*exp(14.1d0/(astar**0.19))
            else                                           ! for relative small uorbs or large friction
               fw = 0.3d0
            endif
            !
            ! magnitude of bottom friction due to waves alone
            ! and due to current alone
            !
            tauwav = 0.5d0*rhomean*fw*uorbu**2           ! wave related bed shear stress
            u2dh = umod                                    ! AM: INCLUDE STOKES DRIFT?
            cdrag = ag/(cz**2)
            taucur = rhomean*cdrag*u2dh**2               ! current related bed shear stress
            !cdrag = cfuhi(L)*hu(L)
            !
            ! parameterized models
            !
            call getymxpar(modind,tauwav, taucur, fw, cdrag, abscos, yparL, ymxpar)
            ypar(L) = yparL
            !
            ! bottom friction for combined waves and current
            !
            taubxu(L) = ymxpar*(taucur + tauwav)                       ! maximum shear stress due to waves and currents, eq to taubxu in D3D
            ! ypar*(taucur + tauwav) is mean shear stress
            if (modind < 9) then
               !tauwci = ypar*(taucur + tauwav)
               !!
               !! primary and secondary bottom friction terms
               !!
               !taubpu(L) = tauwci/(umod*rhomean + waveps)             ! D3D style: taubpu = (g*U)/C**2
               !
               ! no waveps needed here: hu>0 and umod=max(umod,waveps)
               cfwavhi(L) = tauwav/ (rhomean*umod**2)*min(huvli(L),hminlwi)   ! tau = cf * rhomean * ||u|| u, and tau/(rho h) appears in (depth-averaged) momentum equation and in D3D taubpu = tau/ (rho ||u||)
            elseif (modind==9) then
               uorbhs   = sqrt(2.0d0)*uorbu
               hrmsu    = ac1*hwav(k1)+ac2*hwav(k2)
               rlabdau  = ac1*rlabda(k1)+ac2*rlabda(k2)
               rr       = -0.4d0 * sqrt(2d0) / hu(L) + 1d0
               umax     = rr * 2d0 * uorbhs
               t1       = tpu  * sqrt(ag/hu(L))
               u11      = umax / sqrt(ag*hu(L))
               a11      = -0.0049_fp*t1**2 - 0.069_fp*t1 + 0.2911_fp
               raih     = max(0.5_fp , -5.25_fp-6.1_fp*tanh(a11*u11-1.76_fp))
               rmax     = max(0.62_fp , min(0.75_fp , -2.5_fp*hu(L)/max(rlabdau,1.0e-20_fp) + 0.85_fp))
               uon      = umax * (0.5_fp + (rmax-0.5_fp)*tanh((raih-0.5_fp)/(rmax-0.5_fp)))
               uoff     = umax - uon
               uon      = max(1.0e-5_fp , uon)
               uoff     = max(1.0e-5_fp , uoff)
               uwbih    = (0.5_fp*uon**3.0_fp + 0.5_fp*uoff**3.0_fp)**(1.0_fp/3.0_fp)
               rksru    = ac1*bfmpar%rksr(k1)+ac2*bfmpar%rksr(k2)                            ! these exist, okay
               rksmru   = ac1*bfmpar%rksmr(k1)+ac2*bfmpar%rksmr(k2)
               !
               ! Van Rijn 2004 formulation
               !
               phivr      = acos((uuu*costu+vvv*sintu) / umod)
               gamma      = 0.8_fp + phivr - 0.3_fp*phivr**2
               ksc        = sqrt(rksru**2 + rksmru**2)
               uratio     = min(uwbih/(u2dh+waveps) , 5.0_fp)
               ka         = ksc * exp(gamma*uratio)
               ka         = min(ka , 10.0_fp*ksc , 0.2_fp*hu(L))
               ca         = 18.0_fp * log10(12.0_fp*hu(L)/max(ka,waveps))
               cfhi_vanrijn(L) = min(huvli(L),hminlwi)*ag / ca**2         ! umod * rhomean * ag * umod / ca**2
            endif

            if (tpu > 1d-1) then
               ks         = z0*33d0
               omega      = 2d0 * pi / tpu
               a          = uorbu / omega
               !
               wbl = 0.09d0 * alfaw * (ks/max(hu(L),epshu)) * (a/ks)**0.82_fp
               wbl = max(alfaw*ee*z0/hu(L) , wbl)
               wblt(L) = min(0.5_fp, wbl)*hu(L)
            else
               wblt(L) = 0d0
            endif
         endif
      endif
      !
      if (modind == 0) then
         if (hu(L)>epshu) then
            z0urou(L) = hu(L)/(ee*(exp(vonkar*cz/sag) - 1d0))
            rz = 1d0 + hu(L)/(ee*z0urou(L))
            cf = log(rz)/vonkar
            cwall         = 1d0/(cf**2)
            taubxu(L)    = rhomean*cwall*umod*umod
         endif
      else
         if (hu(L) > epshu) then
            ! Avoid z0 of zero
            ust  = sqrt(ypar(L)*(taucur + tauwav)/rhomean)
            if (ust > waveps) then
               cf = min(umod/ust,40.0_fp)
               z0urou(L) = hu(L)/((exp(vonkar*cf) - 1d0)*ee)
               z0urou(L) = min(z0urou(L), 10d0)
               !
            endif
            if (modind == 9) then
               z0urou(L) = max(3.33e-5_fp , ka/30.0)
            endif
         endif
      endif
   enddo

   !      In Delft3D this parameter is named 'maximum bottom friction' via the taumax
   !      This is NOT the same as 'bed shear stress' which is defined in Delft3D as rhow*(taubpu*u1 + taubsu)
   !      MIND: taus computed here ~= gettaus!!
   taus(:)   = 0.0d0
   do L=1,LNx
      k1=ln(1,L)
      k2=ln(2,L)
      if (hu(L) > epshu) then
         taus(k1) = taus(k1) + taubxu(L)*wcL(1,L)
         taus(k2) = taus(k2) + taubxu(L)*wcL(2,L)
      end if
   enddo

   end subroutine tauwave

   subroutine wave_uorbrlabda()
   use m_waves, only: uorb, wlenwav, uorbwav, twav, hwav, hwavcom, gammax, rlabda, jauorb, jauorbfromswan
   use m_flow, only: hs
   use m_flowgeom, only: ndx
   use m_physcoef, only: ag
   use m_sferic, only: pi

   implicit none

   integer                            :: k
   integer                            :: uorbwav_from_SWAN=0
   integer                            :: wlenwav_from_SWAN=0

   double precision                   :: hss, per, omeg, k0, k0h, rk, uorb1

   do k = 1,ndx

      hss  = max(0.01, hs(k))
      per = max(0.01, twav(k))                   ! wave period

      hwav(k) = min(hwavcom(k), gammax*hs(k))       ! Prevent unrealistic Hrms in shallow water. Use original comfile value again every time, as hs changes per dts
      omeg       = 2.0*pi/per
      k0         = omeg*omeg/ag
      k0h        = k0*hss
      if (k0h>pi) then                ! if deep water
         rk = k0
      elseif (k0h<0.005) then         ! if very shallow water
         rk = omeg/sqrt(ag*hss)
      else
         call getwavenr(hss,per,rk)
      endif
      if (wlenwav_from_SWAN.eq.1) then
         rlabda(k) = wlenwav(k)
      else
         rlabda(k) = 2.0*pi/rk
      endif
      if (rk*hss<80d0) then            ! if not very deep water
         if (jauorbfromswan.eq.1) then
            Uorb(k)    = uorbwav(k)
         else
            Uorb(k)      = 0.5d0*hwav(k)*omeg/sinh(rk*hss)
            !Uorb(k)    = uorb1*sqrt(pi)/2d0                            ! See note Dano on orbital velocities in D3D, SWAN and XBeach
            if (jauorb==0) then       ! old d3d convention
               uorb(k) = uorb(k)*sqrt(pi)/2d0    ! only on hrms derived value, not on SWAN read uorb
            end if
         endif
      else
         Uorb(k) = 0d0
      endif
   enddo

   end subroutine wave_uorbrlabda


   subroutine setmodind(rouwav, modind)
   implicit none
   integer     :: modind
   character*4 rouwav
   if (rouwav=='FR84') then
      modind = 1
   elseif (rouwav=='MS90') then
      modind = 2
   elseif (rouwav=='HT91') then
      modind = 3
   elseif (rouwav=='GM79') then
      modind = 4
   elseif (rouwav=='DS88') then
      modind = 5
   elseif (rouwav=='BK67') then
      modind = 6
   elseif (rouwav=='CJ85') then
      modind = 7
   elseif (rouwav=='OY88') then
      modind = 8
   elseif (rouwav=='VR04') then
      modind = 9
   elseif (rouwav=='RU03') then
      modind = 10
      !else
      !    modind = 0
   endif
   end subroutine setmodind

   !> subroutine to compute wave forces from SWAN output
   !> originates from Bas Stengs, extended by AM
   subroutine setwavfu()
   use unstruc_messages
   use MessageHandling
   use m_flowparameters
   use m_flowgeom
   use m_flow, only: hu, huvli, wavfu, wavfv, rhomean
   use m_waves
   use m_physcoef, only: sag
   implicit none

   integer          :: mout
   double precision :: wavfx, wavfy, wavfbx, wavfby
   double precision :: wavfu_loc, wavfbu_loc, twavL
   double precision :: wavfv_loc, wavfbv_loc, wavfmag, wavfbmag,wavfang, wavfbang
   double precision :: fmax, ac1, ac2, hminlwi

   integer          :: L, k1, k2

   ! done: compute only once after (re)initialization
   ! facmax = 0.25d0*sag*rhomean*gammax**2
   hminlwi = 1d0/hminlw
   wavfu = 0d0
   !sywav = 0d0

   do L = 1,lnx
      if (hu(L) < epshu) cycle
      if (L > lnx1D) then
         k1 = ln(1,L) ; k2 = ln(2,L)
         ac1 = acl(L)
         ac2 = 1d0-ac1
         ! interpolation from flownodes to flowlinks
         wavfx = ac1*sxwav(k1) + ac2*sxwav(k2)
         wavfy = ac1*sywav(k1) + ac2*sywav(k2)

         wavfbx = ac1*sbxwav(k1) + ac2*sbxwav(k2)
         wavfby = ac1*sbywav(k1) + ac2*sbywav(k2)

         twavL = ac1*twav(k1)   + ac2*twav(k2)

         ! projection in face-normal direction
         wavfu_loc  = wavfx*csu(L)  + wavfy*snu(L)
         wavfv_loc  = -wavfx*snu(L)  + wavfy*csu(L)
         wavfbu_loc = wavfbx*csu(L) + wavfby*snu(L)
         wavfbv_loc = -wavfbx*snu(L) + wavfby*csu(L)

         ! limit forces
         fmax       = facmax*hu(L)**1.5 / max(0.01d0, twavL)

         !wavfu_loc  = min(max(wavfu_loc, -fmax),fmax)
         !wavfbu_loc = min(max(wavfbu_loc,-fmax),fmax)
         
         ! Should be done on the vector norm, nt separate comps
         wavfmag = min(sqrt(wavfu_loc*wavfu_loc + wavfv_loc*wavfv_loc),fmax)
         wavfbmag = min(sqrt(wavfbu_loc*wavfbu_loc + wavfbv_loc*wavfbv_loc),fmax)
         wavfang  = atan2(wavfv_loc,wavfu_loc)
         wavfbang  = atan2(wavfbv_loc,wavfbu_loc)    ! necessary?
         wavfu_loc = wavfmag*cos(wavfang)
         wavfv_loc = wavfmag*sin(wavfang)
         wavfbu_loc = wavfbmag*cos(wavfbang)
         wavfbv_loc = wavfbmag*sin(wavfbang)

         ! for 3D: account for relative top-layer height in wavfu_loc, e.g.
         !         wavfu(L) = wavfu_loc * dz(L)/hu(LL) + wavfbu_loc
         wavfu(L) = wavfu_loc + wavfbu_loc
         wavfv(L) = wavfv_loc + wavfbv_loc
      else
         ! then get data from network points. Turn off for now..
      endif
      !wavfu(L) = wavfu(L)/ (rhomean*hu(L))                          ! Depth is taken into account using facmax
      wavfu(L) = wavfu(L) * min(huvli(L), hminlwi) / rhomean       ! Dimensions [m/s^2]
      wavfv(L) = wavfv(L) * min(huvli(L), hminlwi) / rhomean       ! Dimensions [m/s^2]
   enddo

   end subroutine setwavfu


   subroutine setwavmubnd()
   use m_flowgeom
   use m_flowparameters
   use m_flowexternalforcings
   use m_flow, only: hu, huvli, wavmubnd
   use m_waves
   implicit none

   double precision :: ac1, ac2

   integer          :: kb, ki, L, n
   double precision :: hminlwi

   hminlwi = 1d0/hminlw

   !  wavmubnd is defined on the whole mesh, but has non-zero values at the open boundaries only
   wavmubnd = 0d0

   do n=1,nbndu
      kb = kbndu(1,n)
      ki = kbndu(2,n)
      L  = kbndu(3,n)
      ! interpolate cell-centered mass fluxes to flow links
      if (hu(L) < epshu) cycle
      ac1 = acl(L)
      ac2 = 1d0-ac1
      wavmubnd(L) = (ac1*mxwav(kb) + ac2*mxwav(ki)) * csu(L) + &
         (ac1*mywav(kb) + ac2*mywav(ki)) * snu(L)

      wavmubnd(L) = wavmubnd(L) * min(huvli(L),hminlwi)
   end do

   do n=1,nbndz
      if ( kbndz(4,n).eq.5 ) then   ! riemann boundaries
         kb = kbndz(1,n)
         ki = kbndz(2,n)
         L  = kbndz(3,n)
         if (hu(L) < epshu) cycle
         if ( wavmubnd(L).ne.0d0 ) cycle
         ! interpolate cell-centered mass fluxes to flow links
         ac1 = acl(L)
         ac2 = 1d0-ac1
         wavmubnd(L) = (ac1*mxwav(kb) + ac2*mxwav(ki)) * csu(L) + &
            (ac1*mywav(kb) + ac2*mywav(ki)) * snu(L)

         wavmubnd(L) = wavmubnd(L) * min(huvli(L),hminlwi)
      end if
   end do

   !  normal-velocity boundaries
   do n=1,nbndn
      kb = kbndn(1,n)
      ki = kbndn(2,n)
      L  = kbndn(3,n)
      if (hu(L) < epshu) cycle
      if ( wavmubnd(L).ne.0d0 ) cycle
      ! interpolate cell-centered mass fluxes to flow links
      ac1 = acl(L)
      ac2 = 1d0-ac1
      wavmubnd(L) = (ac1*mxwav(kb) + ac2*mxwav(ki)) * csu(L) + &
         (ac1*mywav(kb) + ac2*mywav(ki)) * snu(L)

      wavmubnd(L) = wavmubnd(L) * min(huvli(L),hminlwi)
   end do

   !  tangential-velocity boundaries: not needed to define mass fluxes

   return
   end subroutine setwavmubnd

   subroutine wave_comp_stokes_velocities()
   use m_flowparameters
   use m_flowgeom
   use m_flow, only: hu, huvli, hs
   use m_physcoef, only: sag
   use m_waves
   implicit none

   double precision :: Mu, Mv, hminlwi, massflux_max, mnorm, mangle          ! link-based and link-oriented wave-induced volume fluxes
   double precision, allocatable :: mx(:), my(:)
   
   integer :: k1, k2, L, k
   integer :: ierror ! error (1) or not (0)
   
   ierror = 1
   
   if (.not.(allocated(mx))) then
      allocate(mx(1:ndx), my(1:ndx), stat=ierror)   
   end if
   
   ! hminlwi = 1d0/hminlw
   ustokes = 0d0
   vstokes = 0d0
   mx      = 0d0
   my      = 0d0

   do k = 1,ndx
      massflux_max = 1d0/8d0*sag*(max(hs(k),0d0)**1.5)*gammax**2
      mnorm  = min(sqrt(mxwav(k)**2+mywav(k)**2), massflux_max)
      mangle = atan2(mywav(k), mxwav(k))
      mx(k)  = mnorm*dcos(mangle)
      my(k)  = mnorm*dsin(mangle)
   end do

   do L=1,Lnxi
      if ( hu(L).gt.epshu ) then
         k1 = ln(1,L); k2 = ln(2,L)
         !massflux_max = 1d0/8d0*sag*(hu(L)**1.5)*gammax**2
         !Mu =    acL(L) *(csu(L)*(Mxwav(k1)) + snu(L)*(Mywav(k1))) + &
         !   (1d0-acL(L))*(csu(L)*(Mxwav(k2)) + snu(L)*(Mywav(k2)))
         Mu =    acL(L) *(csu(L)*(Mx(k1)) + snu(L)*(My(k1))) + &
            (1d0-acL(L))*(csu(L)*(Mx(k2)) + snu(L)*(My(k2)))
         !Mv =    acL(L) *(-snu(L)*(Mxwav(k1)) + csu(L)*(Mywav(k1))) + &
         !   (1d0-acL(L))*(-snu(L)*(Mxwav(k2)) + csu(L)*(Mywav(k2)))
         Mv =    acL(L) *(-snu(L)*(Mx(k1)) + csu(L)*(My(k1))) + &
            (1d0-acL(L))*(-snu(L)*(Mx(k2)) + csu(L)*(My(k2)))
         !Mu = min(max(Mu, -massflux_max),massflux_max)
         !Mv = min(max(Mv, -massflux_max),massflux_max)
         
         !ustokes(L) = Mu * min(huvli(L),hminlwi)                           ! "Corrects" for unphysical longshore drift along waterline
         !vstokes(L) = Mv * min(huvli(L),hminlwi)
         ustokes(L) = Mu * huvli(L)
         vstokes(L) = Mv * huvli(L)
      else
         ustokes(L) = 0d0
         vstokes(L) = 0d0
      end if
   end do

   do L=lnxi+1,lnx                   ! Randen: Neumann
      if (hu(L)>epshu)  then
         k1 = ln(1,L) ! buiten
         k2 = ln(2,L) ! binnen
         Mx(k1) = Mx(k2);  My(k1) = My(k2)
         !Mxwav(k1) = Mxwav(k2);  Mywav(k1) = Mywav(k2)
         !massflux_max = 1d0/8d0*sag*(hu(L)**1.5)*gammax**2
         !Mu =    acL(L) *(csu(L)*(Mxwav(k1)) + snu(L)*(Mywav(k1))) + &
         !   (1d0-acL(L))*(csu(L)*(Mxwav(k2)) + snu(L)*(Mywav(k2)))
         Mu =    acL(L) *(csu(L)*(Mx(k1)) + snu(L)*(My(k1))) + &
            (1d0-acL(L))*(csu(L)*(Mx(k2)) + snu(L)*(My(k2)))
         !Mv =    acL(L) *(-snu(L)*(Mxwav(k1)) + csu(L)*(Mywav(k1))) + &
         !   (1d0-acL(L))*(-snu(L)*(Mxwav(k2)) + csu(L)*(Mywav(k2)))
         Mv =    acL(L) *(-snu(L)*(Mx(k1)) + csu(L)*(My(k1))) + &
            (1d0-acL(L))*(-snu(L)*(Mx(k2)) + csu(L)*(My(k2)))
         !Mu = min(max(Mu, -massflux_max),massflux_max)
         !Mv = min(max(Mv, -massflux_max),massflux_max)
         
         !ustokes(L) = Mu * min(huvli(L),hminlwi)                           ! "Corrects" for unphysical longshore drift along waterline
         !vstokes(L) = Mv * min(huvli(L),hminlwi)
         ustokes(L) = Mu * huvli(L)
         vstokes(L) = Mv * huvli(L)
      else
         ustokes(L) = 0d0
         vstokes(L) = 0d0
      end if
   end do

   ierror = 0 
1234 continue
   return
   end subroutine wave_comp_stokes_velocities

   subroutine wave_makeplotvars
   use m_waves, only: ustokes, vstokes, ust_mag, fwav_mag, taubxu, taux_cc, tauy_cc
   use m_xbeach_data, only: Fx_cc, Fy_cc
   use m_flowparameters, only: jawave
   use m_flow
   use m_flowgeom

   implicit none

   integer           :: ierror
   integer           :: L, LL, Lb, Lt, k1, k2
   double precision :: ust_mag_u

   ust_mag=0d0
   fwav_mag=0d0
   taux_cc=0d0
   tauy_cc=0d0

   do L = 1, lnx   ! safe for 3D
      k1=ln(1,L);k2=ln(2,L)
      taux_cc(k1) = taux_cc(k1)+wcx1(L)*taubxu(L)
      taux_cc(k2) = taux_cc(k2)+wcx2(L)*taubxu(L)
      tauy_cc(k1) = tauy_cc(k1)+wcy1(L)*taubxu(L)
      tauy_cc(k2) = tauy_cc(k2)+wcy2(L)*taubxu(L)
      call getLbotLtop(L, Lb, Lt)
      do LL = Lb,Lt
         k1=ln(1,LL);k2=ln(2,LL)
         ust_mag_u = sqrt(ustokes(LL)*ustokes(LL) + vstokes(LL)*vstokes(LL))
         ust_mag(k1) = ust_mag(k1)+wcl(1,L)*ust_mag_u
         ust_mag(k2) = ust_mag(k2)+wcl(2,L)*ust_mag_u
      end do
   end do

   if (jawave==3) then
      do L=1,lnx
         k1=ln(1,L);k2=ln(2,L)
         fwav_mag(k1) = fwav_mag(k1)+wcl(1,L)*wavfu(L)*rhomean*hu(L)
         fwav_mag(k2) = fwav_mag(k2)+wcl(2,L)*wavfu(L)*rhomean*hu(L)
      enddo
   end if

   if (jawave==4) then
      fwav_mag = sqrt(Fx_cc*Fx_cc + Fy_cc*Fy_cc)
   endif
   ierror = 0
1234 continue
   return

   end subroutine wave_makeplotvars