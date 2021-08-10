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

!> update positions of particles
   
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif 
subroutine update_particles(q,s0,s1,Dt)
!   use m_flow
   use m_particles
   use m_wearelt
!   use m_flowtimes, only: dts, time1
   use m_flowgeom, only: Ndx, Lnx
   use m_partitioninfo
   use m_sferic
   use geometry_module, only: Cart3Dtospher
   use unstruc_messages
   implicit none
   
   double precision, dimension(Lnx), intent(in) :: q  !< fluxes
   double precision, dimension(Ndx), intent(in) :: s0 !< water levels at start of time interval
   double precision, dimension(Ndx), intent(in) :: s1 !< water levels at end of time interval
   double precision,                 intent(in) :: Dt !< time interval
   
   integer, dimension(1) :: numremaining ! number of remaining particles to be updated
   
   double precision :: xx, yy
   
   integer :: i, k
   integer :: iter
   integer :: ierror
   
   integer, parameter :: MAXITER = 1000 ! maximum number of substeps
   
   if ( japart.ne.1 ) return
   
   ierror = 1
      
!  reconstruct velocity field
   call reconst_vel(q, s0, s1, ierror)
   if ( ierror.ne.0 ) goto 1234
   
   if ( Npart.gt.0 ) then
!     set remaining time to time step
      dtremaining = Dt
!      Lpart = 0
      numzero = 0
   end if
   
   do iter=1,MAXITER
!     update particles in cells
      call update_particles_in_cells(numremaining(1), ierror)
      if ( ierror.ne.0 ) goto 1234
      
      if ( jampi.eq.1 ) then
!        reduce numremaining      
         call reduce_int_max(1,numremaining)
      end if
      
!      write(6,*) 'my_rank=', my_rank, 'iter=', iter, 'numremaining=', numremaining(1)
      
!     send/receive particles from other subdomains
      call partition_update_particles()
      
 !     if ( Npart.gt.0 .and. kpart(1).gt.0 ) then
 !        write(6,"('my_rank=', I1, ', kpart(1)=', I5 )") my_rank, kpart(1)
 !     end if
      
!     BEGIN DEBUG
!      i = 1
!      if ( Npart.gt.0 .and. kpart(i).gt.0 ) then
!         write(mfile,"(F8.2, F8.2, I5)") xpart(i), ypart(i), my_rank
!      end if
!     END DEBUG
      
      if ( numremaining(1).eq.0 ) then
!         write(6,*) 'iter=', iter
         exit
      end if
   end do
   
   
!  check for remaining particles
   if ( numremaining(1).gt.0 ) then
 !    plot remaining particles
      do i=1,Npart
         if ( dtremaining(i).gt.0d0 .and. kpart(i).gt.0 ) then
            if ( jsferic.eq.0 ) then
               xx = xpart(i)
               yy = ypart(i)
            else
               call Cart3Dtospher(xpart(i),ypart(i),zpart(i),xx,yy,0d0)
            end if
            call cirr(xx,yy, 211)
            write(6,"(I0, ', ', I0, ':', 2E25.15, ', ', I0)") iglob(i), my_rank, xx, yy, kpart(i)
         end if
      end do
      
!      call qnerror('update_particles: iter>MAXITER', ' ', ' ')
      call mess(LEVEL_WARN, 'update_particles: iter>MAXITER')
      
      goto 1234
   end if
   
   ierror = 0
1234 continue
   
   return
end subroutine update_particles
   
!> update positions of particles within triangles
subroutine update_particles_in_cells(numremaining, ierror)
   use m_particles
   use m_partrecons
   use m_partmesh
   use m_sferic, only: jsferic
   implicit none
   
   integer,        intent(out) :: numremaining !< number of remaining particles to be updated
   integer,        intent(out) :: ierror       !< error (1) or not (0)
   
   integer                     :: ipart
   integer                     :: i, k, k1, k2, L, Lf
   integer                     :: ja
   integer                     :: Lexit
   
   double precision            :: d, d1, un
   double precision            :: t, tex, dt
   double precision            :: ux0, uy0, uz0, cs, sn
   double precision            :: xn, yn, zn, rl
   double precision            :: dvar, dis, dn
   
   double precision, dimension(3) :: ddn
   
   logical                     :: isboundary
   
   double precision, parameter :: DTOL = 1d-4
   double precision, parameter :: DTOLd  = 1d-4
   double precision, parameter :: DTOLun_rel = 1d-4
   double precision, parameter :: DTOLun = 1e-14
   
   integer,          parameter :: MAXNUMZERO = 10
   
   ierror = 1
   
   numremaining = 0
   
   do ipart=1,Npart
!     check if this particle needs to be updated
      if ( dtremaining(ipart).eq.0d0 .or. kpart(ipart).lt.1 ) cycle
!     get cell (flownode) particle in in
      k = kpart(ipart)
      
!     compute exit time <= dtremaining
      tex = dtremaining(ipart)
      
      Lexit = 0   ! exit edge (flowlink)
      
 !    compute velocity at current position     
      ux0 = u0x(k) + alpha(k)*(xpart(ipart)-xzwcell(k))
      uy0 = u0y(k) + alpha(k)*(ypart(ipart)-yzwcell(k))
      if ( jsferic.ne.0 ) then
         uz0 = u0z(k) + alpha(k)*(zpart(ipart)-zzwcell(k))
      end if
      
!     loop over edges (netlinks) of cells
      do i=jcell2edge(k),jcell2edge(k+1)-1
         L = icell2edge(i)   ! edge
         
         k1 = edge2node(1,L)
         k2 = edge2node(2,L)
        
         if ( jsferic.eq.0 ) then
            cs = dnx(1,L)
            sn = dny(1,L)
            if ( edge2cell(2,L).eq.k ) then
               cs = -cs
               sn = -sn
            end if
         else
            if ( edge2cell(1,L).eq.k ) then
               ddn = (/ dnx(1,L), dny(1,L), dnz(1,L) /)
            else
               ddn = (/ dnx(2,L), dny(2,L), dnz(2,L) /)
            end if
         end if
         
!        check for boundary edge
         isboundary = ( edge2cell(1,L).eq.0 .or. edge2cell(2,L).eq.0 )
         
!        compute normal distance to edge
         if ( jsferic.eq.0 ) then
            if ( isboundary ) then ! boundary: add tolerance
               call dlinedis2(xpart(ipart),ypart(ipart),xnode(k1)+cs*DTOLd,ynode(k1)+sn*DTOLd,xnode(k2)+cs*DTOLd,ynode(k2)+sn*DTOLd,ja,d,xn,yn,rl)
            else
               call dlinedis2(xpart(ipart),ypart(ipart),xnode(k1),ynode(k1),xnode(k2),ynode(k2),ja,d,xn,yn,rl)
            end if
            dis = (xn-xpart(ipart))*cs + (yn-ypart(ipart))*sn
         else
            if ( isboundary ) then ! boundary: add tolerance
               call dlinedis3D(xpart(ipart),ypart(ipart),zpart(ipart),xnode(k1)+DTOLd*ddn(1),  &
                                                                      ynode(k1)+DTOLd*ddn(2),  &
                                                                      znode(k1)+DTOLd*ddn(3),  &
                                                                      xnode(k2)+DTOLd*ddn(1),   &
                                                                      ynode(k2)+DTOLd*ddn(2),   &
                                                                      znode(k2)+DTOLd*ddn(3),   &
                                                                      ja,d,xn,yn,zn,rl)
            else
               call dlinedis3D(xpart(ipart),ypart(ipart),zpart(ipart),xnode(k1),ynode(k1),znode(k1),xnode(k2),ynode(k2),znode(k2),ja,d,xn,yn,zn,rl)
            end if
            dis = (xn-xpart(ipart))*ddn(1) + (yn-ypart(ipart))*ddn(2) + (zn-zpart(ipart))*ddn(3)
         end if
         
         
!        BEGIN DEBUG
!         if ( ipart.eq.1 .and. kpart(ipart).eq.5298 ) then
!            write(6,*) i, ':', d, rL, dis
!         end if
!         
!         if ( abs(dis-d).gt.1d-1 ) then
!            write(6,*) i, dis, d
!         end if
!        END DEBUG
         
!        check inside or outside triangle 
!         if ( dis.lt.-DTOLd .and. rL.ge.0d0 .and. rL.le.1d0 .and. .not.isboundary ) then
         if ( dis.lt.-DTOLd .and. .not.isboundary ) then
!           outside triangle
            tex = 0d0
            Lexit = L
            exit
         else
!           inside triangle

   !        compute normal velocity to edge (outward positive)
            if ( jsferic.eq.0 ) then
               un =  ux0*cs + uy0*sn
            else
               un =  ux0*ddn(1) + uy0*ddn(2) + uz0*ddn(3)
            end if
            
!!           BEGIN DEBUG
!!           check normal velocity at closed boundary
!            if ( edge2cell(1,L).eq.0 .or. edge2cell(2,L).eq.0 ) then
!               dvar = (u0x(k) + alpha(k)* (xn-xzwcell(k)))*ddn(1) + (u0y(k) + alpha(k)*(yn-yzwcell(k)))*ddn(2) + (u0z(k) + alpha(k)*(zn-zzwcell(k)))*ddn(3)
!               if ( abs(dvar) .gt. 1d-4 ) then
!                  continue
!               end if
!            end if
!!           END DEBUG
         
            if ( un.gt.max(DTOLun_rel*d,DTOLun) ) then   ! normal velocity does not change sign: sufficient to look at u0.n
   !           compute exit time for this edge: ln(1+ d/un alpha) / alpha
               dvar = alpha(k)*dis/un
               if ( dvar.gt.-1d0) then
                  t = dis/un
                  if ( abs(dvar).ge.DTOL ) then
                     t = t * log(1d0+dvar)/dvar
                  end if
               else
                  t = huge(1d0)
               end if  
            
   !           update exit time/edge (flowlink)      
!               if ( t.le.tex .and. t.ge.0d0 ) then
               if ( t.le.tex ) then

                  tex = t
                  Lexit = L
               end if
            else
               continue
            end if
         
         end if
      end do
         
      if ( dtremaining(ipart).eq.0d0 ) then
         continue
      end if
         
!     compute timestep in cell (flownode)
      dt = min(dtremaining(ipart), tex)
         
!     update particle
      if ( abs(alpha(k)).lt.DTOL ) then
         dvar = dt
      else
         dvar = (exp(alpha(k)*dt)-1d0)/alpha(k)
      end if
      
      xpart(ipart) = xpart(ipart) + dvar * ux0
      ypart(ipart) = ypart(ipart) + dvar * uy0
      
      if ( jsferic.ne.0 ) then
         zpart(ipart) = zpart(ipart) + dvar * uz0
      end if
      
!!     BEGIN DEBUG
!      if ( jsferic.eq.1 ) then
!!        project node on triangle
!         dn = (xpart(ipart) - xzwcell(k)) * dnn(1,k) +  &
!              (ypart(ipart) - yzwcell(k)) * dnn(2,k) +  &
!              (zpart(ipart) - zzwcell(k)) * dnn(3,k)
!         xpart(ipart) = xpart(ipart) - dn * dnn(1,k)
!         ypart(ipart) = ypart(ipart) - dn * dnn(2,k)
!         zpart(ipart) = zpart(ipart) - dn * dnn(3,k)
!      end if
!!     END DEBUG
      
      dtremaining(ipart) = dtremaining(ipart) - dt
!      Lpart(ipart) = Lexit
      
      if ( dt.eq.0d0 ) then
         numzero(ipart) = numzero(ipart) + 1
      end if
      
      if ( numzero(ipart).gt.MAXNUMZERO ) then
!        disable particle that is not moving
         kpart(ipart) = 0
         dtremaining(ipart) = 0d0
         
!     proceed to neighboring cell (if applicable)
      else if ( Lexit.gt.0 ) then
         numremaining = numremaining + 1  ! number of remaining particles for next substep
         if ( edge2cell(1,Lexit).gt.0 .and. edge2cell(2,Lexit).gt.0 ) then   ! internal edge (netlink)
            kpart(ipart) = edge2cell(1,Lexit) + edge2cell(2,Lexit) - k
            
!            if ( kpart(ipart).eq.5298 ) then
!               call qnerror(' ', ' ', ' ')
!            end if
            
            if ( kpart(ipart).eq.0 ) then
               continue
            else
               if ( jsferic.eq.1 ) then
!                 project node on triangle
                  k = kpart(ipart)
                  k1 = edge2node(1,Lexit)
                  k2 = edge2node(2,Lexit)
                  xn = 0.5d0*(xnode(k1)+xnode(k2))
                  yn = 0.5d0*(ynode(k1)+ynode(k2))
                  zn = 0.5d0*(znode(k1)+znode(k2))
                  dn = (xpart(ipart) - xn) * dnn(1,k) +  &
                       (ypart(ipart) - yn) * dnn(2,k) +  &
                       (zpart(ipart) - zn) * dnn(3,k)
                  xpart(ipart) = xpart(ipart) - dn * dnn(1,k)
                  ypart(ipart) = ypart(ipart) - dn * dnn(2,k)
                  zpart(ipart) = zpart(ipart) - dn * dnn(3,k)
               end if
            end if
         else  ! on boundary
            kpart(ipart) = 0
         end if
      else
!        safety check
         if ( dtremaining(ipart).ne.0d0 ) then
            ierror = 1
            call qnerror('update_particles_in_cells: dtremaining <> 0', ' ', ' ')
            goto 1234
         end if
      end if
         
   end do
   
   ierror = 0
1234 continue
   
   return
end subroutine update_particles_in_cells



!> reconstruct velocity in cells
subroutine reconst_vel_coeffs()
   
   use m_flowgeom, only: Ndx, Lnx, bl  !, lne2ln  !, csu, snu, wu  !, xu, yu
   use m_flowparameters, only: epshs
   use m_partrecons
   use m_partmesh
   use m_alloc
   use m_sferic
   use geometry_module, only: dbdistance, gaussj
   
   implicit none
   
   integer,                          parameter   :: N = 4
   
   double precision, dimension(N,N)              :: Amat ! matrix
   double precision, dimension(N)                :: rhs
   
   double precision                              :: cs, sn
   
   integer                                       :: i, icell, j, jj, k, L, NN
   integer                                       :: k1, k2
   integer                                       :: i12, isign
   integer                                       :: ierror
   
   ierror = 1
   
!  allocate startpointers
   call realloc(jreconst, numcells+1, keepExisting=.false., fill=0)
   
!  set startpointers
   jreconst(1) = 1
   do icell=1,numcells
      jreconst(icell+1) = jcell2edge(icell) + jcell2edge(icell+1)-jcell2edge(icell)
   end do
  
!  allocate column indices and matrix entries   
   NN = jreconst(numcells+1)-1
   call realloc(ireconst, NN, keepExisting=.false., fill=0)
   if ( jsferic.eq.0 ) then
      call realloc(Areconst, (/3, NN /), keepExisting=.false., fill=0d0)
   else
      call realloc(Areconst, (/4, NN /), keepExisting=.false., fill=0d0)
   end if
      
!  dummy rhs
   rhs = 0d0
 
!  loop over internal cells
   jj = 0
   do icell=1,numcells
!     check for triangles
!      if ( jcell2edge(k+1)-jcell2edge(k).ne.3 ) then
!         cycle
!      end if
   
!     get flownode number (for s, bl)
      k = iabs(cell2nod(icell))

!     fill system for (ux,uy) = (ux0, uy0) + alpha (x-x0, y-y0)
      
!     loop over edges (netlinks)
      i = 0
      do j=jcell2edge(icell),jcell2edge(icell+1)-1
         i = i+1
         L = icell2edge(j) ! netlink
         
         k1 = edge2node(1,L)
         k2 = edge2node(2,L)
         
         if ( jsferic.eq.0 ) then
            cs = dnx(1,L)
            sn = dny(1,L)
               
!           add to system
            Amat(i,1) = cs
            Amat(i,2) = sn
            Amat(i,3) = (0.5d0*(xnode(k1)+xnode(k2))-xzwcell(icell))*cs + (0.5d0*(ynode(k1)+ynode(k2))-yzwcell(icell))*sn
         else
            i12 = 1
            isign = 1
            if ( edge2cell(2,L).eq.icell ) then
              i12 = 2
              isign = -1d0
            end if
            
            Amat(i,1) = dnx(i12,L)*isign
            Amat(i,2) = dny(i12,L)*isign
            Amat(i,3) = dnz(i12,L)*isign
            Amat(i,4) = ( (0.5d0*(xnode(k1)+xnode(k2))-xzwcell(icell))*dnx(i12,L) + &
                          (0.5d0*(ynode(k1)+ynode(k2))-yzwcell(icell))*dny(i12,L) + &
                          (0.5d0*(znode(k1)+znode(k2))-zzwcell(icell))*dnz(i12,L) ) * isign
         end if
         
         jj = jj+1
         ireconst(jj) = L
      end do
      
      if ( jsferic.eq.0 ) then
!        solve system
         call gaussj(Amat,3,N,rhs,1,1)
         
         do i=1,3
            L = icell2edge(jcell2edge(icell)+i-1)
            Areconst(1:3,jreconst(icell)+i-1) = Amat(1:3,i)
         end do
      else
!        impose zero cell normal velocity
         Amat(4,1:3) = dnn(:,icell)
         Amat(4,4) = 0d0
         
!        solve system
         call gaussj(Amat,4,N,rhs,1,1)
         
         do i=1,3
            L = icell2edge(jcell2edge(icell)+i-1)
            Areconst(1:4,jreconst(icell)+i-1) = Amat(1:4,i)
         end do

      end if
   end do
   
   ierror = 0
!  error handling
1234 continue

   return
end subroutine reconst_vel_coeffs


!> reconstruct velocity in cells
subroutine reconst_vel(q, s0, s1, ierror)
   use m_flowgeom, only: Ndx, Lnx, bl  !, lne2ln  !, csu, snu, wu  !, xu, yu
   use m_flowparameters, only: epshs
   use m_partrecons
   use m_partmesh
   use m_sferic
   use geometry_module, only: dbdistance
   
   implicit none
   
   double precision, dimension(Lnx), intent(in)  :: q    ! flowlink-based discharge (m3/s)
   double precision, dimension(Ndx), intent(in)  :: s0   ! flownode-based water level (m) at begin of interval
   double precision, dimension(Ndx), intent(in)  :: s1   ! flownode-based water level (m) at end of interval
   
   integer,                          intent(out) :: ierror
   
   integer,                          parameter   :: N = 4
   
   double precision, dimension(N,N)              :: Amat ! matrix
   double precision, dimension(N)                :: rhs
   
   integer                                       :: i, icell, j, k, L, Lf
   integer                                       :: k1, k2
   integer                                       :: ja
   integer                                       :: i12, isign
   
   double precision                              :: cs, sn, wwu, h0, h1, h, dfac, un

   double precision, parameter                   :: DTOL=1d-10
   
   ierror = 1
   
!  get fluxes at all edges, including internal
   call set_fluxes(Lnx, q, qe)
   
!  initialize   
   u0x = 0d0
   u0y = 0d0
   if ( jsferic.eq.1 ) then
      u0z = 0d0
   end if
   alpha = 0d0
   
   do icell=1,numcells
!     get flownode number (for s, bl)
      k = iabs(cell2nod(icell))
      
!     get water depth
      h0 = s0(k)-bl(k)
      h1 = s1(k)-bl(k)
      if ( abs(h1-h0).gt.DTOL ) then
         if ( h0.gt.epshs .and. h1.gt.epshs ) then
            h = (h1-h0)/(log(h1)-log(h0))
         else if ( h0.gt.epshs .or. h1.gt.epshs ) then
            h = 0.5d0*(h0+h1)
         else
            h = 0d0
         end if
      else
         h = 0.5d0*(h0+h1)
      end if
      
      if ( h.le.epshs ) cycle
      
      if ( jsferic.eq.0 ) then
         do j=jreconst(icell),jreconst(icell+1)-1
            L = ireconst(j)
            un = qe(L)/(h*w(L))
            u0x(icell)   = u0x(icell)   + Areconst(1,j)*un
            u0y(icell)   = u0y(icell)   + Areconst(2,j)*un
            alpha(icell) = alpha(icell) + Areconst(3,j)*un
         end do
      else
         do j=jreconst(icell),jreconst(icell+1)-1
            L = ireconst(j)
            un = qe(L)/(h*w(L))
            u0x(icell)   = u0x(icell)   + Areconst(1,j)*un
            u0y(icell)   = u0y(icell)   + Areconst(2,j)*un
            u0z(icell)   = u0z(icell)   + Areconst(3,j)*un
            alpha(icell) = alpha(icell) + Areconst(4,j)*un
         end do
      end if
   end do
   
   
   
   
   
   
   

!!  loop over internal cells
!   do icell=1,numcells
!!     check for triangles
!!      if ( jcell2edge(k+1)-jcell2edge(k).ne.3 ) then
!!         cycle
!!      end if
!   
!!     get flownode number (for s, bl)
!      k = iabs(cell2nod(icell))
!
!!     fill system for (ux,uy) = (ux0, uy0) + alpha (x-x0, y-y0)
!      
!!     loop over edges (netlinks)
!      i = 0
!      do j=jcell2edge(icell),jcell2edge(icell+1)-1
!         i = i+1
!         L = icell2edge(j) ! netlink
!         
!         k1 = edge2node(1,L)
!         k2 = edge2node(2,L)
!         
!         wwu = w(L)
!         
!         if ( jsferic.eq.0 ) then
!            cs = dnx(1,L)
!            sn = dny(1,L)
!               
!!           add to system
!            Amat(i,1) = cs
!            Amat(i,2) = sn
!            Amat(i,3) = (0.5d0*(xnode(k1)+xnode(k2))-xzwcell(icell))*cs + (0.5d0*(ynode(k1)+ynode(k2))-yzwcell(icell))*sn
!         else
!            i12 = 1
!            isign = 1
!            if ( edge2cell(2,L).eq.icell ) then
!              i12 = 2
!              isign = -1d0
!            end if
!            
!            Amat(i,1) = dnx(i12,L)*isign
!            Amat(i,2) = dny(i12,L)*isign
!            Amat(i,3) = dnz(i12,L)*isign
!            Amat(i,4) = ( (0.5d0*(xnode(k1)+xnode(k2))-xzwcell(icell))*dnx(i12,L) + &
!                          (0.5d0*(ynode(k1)+ynode(k2))-yzwcell(icell))*dny(i12,L) + &
!                          (0.5d0*(znode(k1)+znode(k2))-zzwcell(icell))*dnz(i12,L) ) * isign
!         end if
!         
!!        u.n = q / ( h wu )
!         h0 = s0(k)-bl(k)
!         h1 = s1(k)-bl(k)
!         if ( abs(h1-h0).gt.DTOL ) then
!            if ( h0.gt.epshs .and. h1.gt.epshs ) then
!               h = (h1-h0)/(log(h1)-log(h0))
!            else if ( h0.gt.epshs .or. h1.gt.epshs ) then
!               h = 0.5d0*(h0+h1)
!            else
!               h = 0d0
!            end if
!         else
!            h = 0.5d0*(h0+h1)
!         end if
!         
!         if ( h.gt.epshs ) then
!            rhs(i) = qe(L) / ( h*wwu )
!         else
!         
!            rhs(i) = 0d0
!         end if
!      end do
!      
!      if ( jsferic.eq.0 ) then
!!        solve system
!         call gaussj(Amat,3,N,rhs,1,1)
!         
!         if ( sqrt( (u0x(icell)-rhs(1))**2 + (u0y(icell)-rhs(2))**2 ).gt.1d-16 ) then
!            continue
!         else if ( abs(alpha(icell)-rhs(3)).gt.1d-16 ) then
!            continue
!         end if
!         
!!         u0x(icell)   = rhs(1)
!!         u0y(icell)   = rhs(2)
!!         alpha(icell) = rhs(3)
!      else
!!        impose zero cell normal velocity
!         Amat(4,1:3) = dnn(:,icell)
!         Amat(4,4) = 0d0
!         rhs(4) = 0d0
!         
!!        solve system
!         call gaussj(Amat,4,N,rhs,1,1)
!         
!!         u0x(icell)   = rhs(1)
!!         u0y(icell)   = rhs(2)
!!         u0z(icell)   = rhs(3)
!!         alpha(icell) = rhs(4)
!         
!         if ( sqrt( (u0x(icell)-rhs(1))**2 + (u0y(icell)-rhs(2))**2 + (u0z(icell)-rhs(3))**2 ).gt.1d-16 ) then
!            continue
!         else if ( abs(alpha(icell)-rhs(4)).gt.1d-16 ) then
!            continue
!         end if
!      end if
!      
!!     BEGIN DEBUG
!!      if ( k.eq.16 ) then
!!         write(6,*) (u0x(k) + alpha(k)*(xu(Lf)-xz(k))) * csu(Lf) +  &
!!            (u0y(k) + alpha(k)*(yu(Lf)-yz(k))) * snu(Lf), q(Lf) / ( (s(k)-bl(k))*wu(Lf) )
!!      end if
!!     END DEBUG
!   end do
   
   ierror = 0
!  error handling
1234 continue

   return
end subroutine reconst_vel


!> draw particles in GUI
subroutine tekpart()
   use m_particles
   use m_wearelt
   use unstruc_display
   use m_sferic, only: jsferic
   use m_missing, only: dmiss
   use geometry_module, only : cart3Dtospher
   implicit none
   
   double precision :: x, y
   
   integer :: i
   
   if ( Npart.lt.1 .or. ndrawpart.eq.1 ) return
   
!  safety check
   if ( jsferic.eq.1 .and. .not.(allocated(zpart)) ) then
      call dealloc_partmesh()
      call dealloc_partfluxes()
      call dealloc_partrecons()
      call dealloc_particles()
      call dealloc_auxfluxes()
      call dealloc_partparallel()
      japart = 0
      return
   end if

   call setcol(31)
   
   if ( jsferic.eq.0 ) then
      do i=1,Npart
         if ( kpart(i).eq.0 ) cycle
         call movabs(xpart(i),ypart(i))
         call cir(rcir)
      end do
   else
       do i=1,Npart
         if ( kpart(i).eq.0 ) cycle
         call Cart3Dtospher(xpart(i),ypart(i),zpart(i),x,y,0d0)
         call movabs(x,y)
         call cir(rcir)
      end do
   end if
   
   return
end subroutine tekpart

!> set pointer
subroutine part_setmesh()
   use network_data, only: kn, xk, yk, xzw, yzw, numk, numL, nump, netcell, lnn, lne
   use m_flowgeom, only: lne2ln, ba
   use m_alloc
   use m_missing
   use m_partmesh
   use m_sferic, only: jsferic, jasfer3D 
   use geometry_module, only: dbdistance, sphertocart3D, normaloutchk, comp_masscenter
   
   implicit none
   
   integer                        :: i, node1, node2, icell, j, k, L, N
   integer                        :: im1, ip1, Lm1, Lp1, L1, L2
   integer                        :: newnode, newedge
   integer                        :: isign, ja, k1, k2, k3, kL, kR
   integer                        :: jaswap
                                  
   integer                        :: numnontris
   integer                        :: numaddedges
   
   double precision, dimension(3) :: xv, yv   
   double precision, dimension(3) :: t, t1, t2    ! edge tangential vector

   
   numnodes = numk
   numedges = numL
   numcells = nump
   
!  count number of non-triangles and additional (internal) edges
   numnontris=0
   numaddedges=0
   do k=1,nump
      N = netcell(k)%N
      if ( N.gt.3 ) then
         numnontris = numnontris+1
         numaddedges = numaddedges+N
      end if
   end do
   
!  compute sizes
   numnodes = numk + numnontris                 ! add centers of non-triangles
   numedges = numL + numaddedges                ! add internal edges of non-triangles
   numcells = nump - numnontris + numaddedges   ! add internal triangles of non-triangles
   
!  (re)allocate   
   call realloc_partmesh()
   
!  nodes
   if ( jsferic.eq.0 ) then
      do k=1,numk
         xnode(k) = xk(k)
         ynode(k) = yk(k)
      end do
   else
      do k=1,numk
         call sphertocart3D(xk(k),yk(k),xnode(k),ynode(k),znode(k))
      end do
   end if

!  edges
   do L=1,numL
      edge2node(1,L) = kn(1,L)
      edge2node(2,L) = kn(2,L)
      if ( lne2ln(L).ge.0 ) then
         edge2link(L) = lne2ln(L)
      else
!        not a flowlink number, see flow_geominit
         continue
      end if
   end do
   
!  set jcell2edge startpointers and add new triangles
   icell=0
   jcell2edge(1) = 1
   do k=1,nump
      N = netcell(k)%N
      if ( N.eq.3 ) then
         icell = icell+1
         jcell2edge(icell+1) = jcell2edge(icell) + 3
!         nod2cell(k) = icell
         cell2nod(icell) = k
      else if ( N.gt.3 ) then
         do j=1,N
            icell = icell+1
            jcell2edge(icell+1) = jcell2edge(icell) + 3
!            if ( j.eq.1 ) nod2cell(k) = -icell
            cell2nod(icell) = -k
         end do
      else  ! should not happen
         continue
      end if
   end do
   
!  allocate jcell2edge
   N = jcell2edge(numcells+1)-1
   call realloc(icell2edge, N, fill=0, keepExisting=.false.)
   
!  copy netcell data into cell2edge and add new triangles (nodes, edges)
   icell = 0
   newnode = numk
   numorigedges = numL
   newedge = numorigedges
   do k=1,nump
      N = netcell(k)%N
      if ( N.eq.3 )then
         icell = icell+1   ! add existing triangle
         i = 0
         do j=jcell2edge(icell),jcell2edge(icell+1)-1
            i = i+1
            L = netcell(k)%lin(i)
            icell2edge(j) = L
         end do
         if ( jsferic.eq.0 ) then
            xzwcell(icell) = xzw(k)
            yzwcell(icell) = yzw(k)
         else
            call sphertocart3D(xzw(k),yzw(k),xzwcell(icell),yzwcell(icell),zzwcell(icell))
         end if
         areacell(icell) = ba(k)
      else
!        add node
         newnode = newnode+1
         if ( jsferic.eq.0 ) then
            xnode(newnode) = xzw(k)
            ynode(newnode) = yzw(k)
         else
            call sphertocart3D(xzw(k),yzw(k),xnode(newnode),ynode(newnode),znode(newnode))
         end if
         
         xv(1) = xzw(k)
         yv(1) = yzw(k)
         
         do i=1,N ! add new edges and triangles
            im1 = i-1; if ( im1.lt.1 ) im1=im1+N
            ip1 = i+1; if ( ip1.gt.N ) ip1=ip1-N
            L = netcell(k)%lin(i)
            call find_common_node(netcell(k)%lin(im1),L,node1)
            call find_common_node(netcell(k)%lin(ip1),L,node2)
            
            icell = icell+1
!           add edges
            newedge = newedge+1
            edge2node(1,newedge) = newnode
            edge2node(2,newedge) = node1
            edge2link(newedge) = -1
            
!           add edges to cell2edge
            j = jcell2edge(icell)
            icell2edge(j) = newedge
            icell2edge(j+1) = L
            icell2edge(j+2) = newedge+1
            if ( i.eq.N ) then
               icell2edge(j+2) = icell2edge(j+2) - N
            end if
            
!           compute mass center
            xv(2) = xk(node1)
            yv(2) = yk(node1)
            xv(3) = xk(node2)
            yv(3) = yk(node2)
            call comp_masscenter(3, xv, yv, xzwcell(icell), yzwcell(icell), areacell(icell), ja, jsferic, jasfer3D, dmiss)
            
            if ( jsferic.eq.1 ) then
               xv(1) = xzwcell(icell)  ! reuse xv
               yv(1) = yzwcell(icell)   ! reuse yv
               call sphertocart3D(xv(1), yv(1),xzwcell(icell),yzwcell(icell),zzwcell(icell))
            end if
            
 !           call cirr(xzwcell(k), yzwcell(k), 31)
         end do
      end if
   end do
   
!  set edge2cell
   edge2cell = 0
   do icell=1,numcells
      do j=jcell2edge(icell),jcell2edge(icell+1)-1
         L = icell2edge(j)
         if ( edge2cell(1,L).eq.0 ) then
            edge2cell(1,L) = icell
         else
            edge2cell(2,L) = icell
         end if
      end do
   end do
   
!  fix orientation of edge2cell
   do L=1,numL ! exclude new edges
      k1 = edge2cell(1,L)
      if ( k1.eq.0 ) cycle
      
      k1 = iabs(cell2nod(k1))
      
      jaswap = 0
      
      if ( lnn(L).eq.1 ) then  !     boundaries: inward normal
         jaswap = 1
      else if ( k1.eq.lne(2,L) .and. lnn(L).gt.1 ) then
         jaswap = 1
      end if
      
      if ( jaswap.eq.1 ) then
         icell = edge2cell(1,L)
         edge2cell(1,L) = edge2cell(2,L)
         edge2cell(2,L) = icell
      end if
   end do
   
   if ( jsferic.ne.0 ) then
!     compute cell normal vectors from first two edges
      do k=1,numcells
         L1 = icell2edge(jcell2edge(k))
         L2 = icell2edge(jcell2edge(k)+1)
         k1 = edge2node(1,L1)
         k2 = edge2node(2,L1)
         k3 = edge2node(1,L2)
         if ( k3.eq.k1 .or. k3.eq.k2 ) then
            k3 = edge2node(2,L2)
         end if
         t1 = (/ xnode(k2)-xnode(k1), ynode(k2)-ynode(k1), znode(k2)-znode(k1)/)
         t2 = (/ xnode(k3)-xnode(k2), ynode(k3)-ynode(k2), znode(k3)-znode(k2)/)
         
         dnn(:,k) = (/ t1(2)*t2(3) - t1(3)*t2(2), t1(3)*t2(1) - t1(1)*t2(3), t1(1)*t2(2) - t1(2)*t2(1) /)
         dnn(:,k) = dnn(:,k) / sqrt( dnn(1,k)**2 + dnn(2,k)**2 + dnn(3,k)**2 )
         
!        fix orientation
         if ( dnn(1,k)*xnode(k1) + dnn(2,k)*ynode(k1) + dnn(3,k)*znode(k1) .lt. 0d0 ) then
            dnn(:,k) = -dnn(:,k)
         end if
      end do
   end if
   
!  nx, ny, w
   do L=1,numedges
      k1 = edge2node(1,L)
      k2 = edge2node(2,L)
      
      kL = edge2cell(1,L)
      kR = edge2cell(2,L)
      
      k = kL   ! outward positive
      
      isign = 1
      if ( k.le.0 ) then
         k = edge2cell(2,L)   ! inward positive
         isign = -1
      end if
      
      if ( k.eq.0 ) cycle  ! isolated edge
      
!     compute normal vector (outward positive)
      if ( jsferic.eq.0 ) then
         call normaloutchk(xnode(k1),ynode(k1),xnode(k2),ynode(k2),xzwcell(k),yzwcell(k),dnx(1,L),dny(1,L),ja, jsferic, jasfer3D, dmiss, dxymis)
         
         if ( isign.eq.-1 ) then
            dnx(1,L) = -dnx(1,L)
            dny(1,L) = -dny(1,L)
         end if
         
         w(L) = dbdistance(xnode(k1),ynode(k1),xnode(k2),ynode(k2),jsferic, jasfer3D, dmiss)
      else
!        compute outward normal with respect to the left cell
         t = (/ xnode(k2)-xnode(k1), ynode(k2)-ynode(k1), znode(k2)-znode(k1) /)
         w(L) = sqrt( t(1)**2 + t(2)**2 + t(3)**2)
         
         t = t/w(L)
         
         if ( kL.gt.0 ) then
!           left cell normal : nn X t
            dnx(1,L) = dnn(2,kL) * t(3) - dnn(3,kL) * t(2)
            dny(1,L) = dnn(3,kL) * t(1) - dnn(1,kL) * t(3)
            dnz(1,L) = dnn(1,kL) * t(2) - dnn(2,kL) * t(1)
            
!           fix orientation
            if ( (xnode(k1)-xzwcell(kL))*dnx(1,L) + (ynode(k1)-yzwcell(kL))*dny(1,L) + (znode(k1)-zzwcell(kL))*dnz(1,L) .lt. 0d0 ) then
               dnx(1,L) = -dnx(1,L)
               dny(1,L) = -dny(1,L)
               dnz(1,L) = -dnz(1,L)
            end if
         end if
         
         if ( kR.gt.0 ) then
!           left cell normal : nn X t
            dnx(2,L) = dnn(2,kR) * t(3) - dnn(3,kR) * t(2)
            dny(2,L) = dnn(3,kR) * t(1) - dnn(1,kR) * t(3)
            dnz(2,L) = dnn(1,kR) * t(2) - dnn(2,kR) * t(1)
            
!           fix orientation
            if ( (xnode(k1)-xzwcell(kR))*dnx(2,L) + (ynode(k1)-yzwcell(kR))*dny(2,L) + (znode(k1)-zzwcell(kR))*dnz(2,L) .lt. 0d0 ) then
               dnx(2,L) = -dnx(2,L)
               dny(2,L) = -dny(2,L)
               dnz(2,L) = -dnz(2,L)
            end if        
         end if
      end if
   end do
   
   
   return
end subroutine part_setmesh

!> set all fluxes, including internal
subroutine set_fluxes(Lnx,q,qe)
   use m_partmesh
   use m_partfluxes
   implicit none
   
   integer,                               intent(in)  :: Lnx
   double precision, dimension(Lnx),      intent(in)  :: q
   
   double precision, dimension(numedges), intent(out) :: qe
   
   integer                                            :: j, L, Lf
   
   do L=1,numedges
      qe(L) = 0d0
      do j=jflux2link(L),jflux2link(L+1)-1
         Lf = iflux2link(j)
         if ( Lf.gt.0 ) then
            qe(L) = qe(L) + Aflux2link(j)*q(Lf)
         end if
      end do
   end do
   
!   double precision, dimension(:),        allocatable :: dum
   
!   integer                                            :: i, j, k, k1, k2
!   integer                                            :: icL, icR, n1, n2
!   integer                                            :: L, L1, L2, L3, Lf, N
   
!   double precision                                   :: sumq, sumarea, circ, dcirc, dlen(MAXSUBCELLS), sumlen, dq
   
!   qe = 0d0
!   
!!  set non-internal fluxes   
!   do L=1,numedges
!      Lf = edge2link(L) ! flow link
!      if ( Lf.gt.0 ) then
!         qe(L) = q(Lf)
!      end if
!   end do
!   
!!  compute flux balances of original non-triangular cells (internal fluxes are still 0d0)
!   allocate(dum(numcells))
!   dum = 0d0
!   do L=1,numedges
!      k1 = edge2cell(1,L)
!      k2 = edge2cell(2,L)
!      
!      if ( k1.gt.0 ) dum(k1) = dum(k1) - qe(L)
!      if ( k2.gt.0 ) dum(k2) = dum(k2) + qe(L)
!   end do
!   
!!  set internal fluxes
!   k = 1 ! cell number
!   do while ( k.le.numcells )
!      
!      if ( cell2nod(k).lt.0 ) then  ! new triangles
!!        count number of sub-triangles
!         N = 0
!         sumq = 0d0
!         sumarea = 0d0
!         do while ( cell2nod(k+N).eq.cell2nod(k) )
!            sumq = sumq + dum(k+N)
!            sumarea = sumarea + areacell(k+N)
!            N = N+1
!            if ( k+N.gt.numcells ) exit
!         end do
!         
!         if ( N.eq.0 ) then
!            k = k+1
!            cycle
!         end if
!         
!         sumq = sumq/sumarea  ! div(q)
!         
!!        loop over all sub-triangles
!         circ = 0d0  ! circulation
!         sumlen = 0d0  ! summed area of triangles
!         j = jcell2edge(k)
!         do i=1,N
!!           get edge numbers     
!            L1 = icell2edge(j+(i-1)*3)   ! "left" internal
!            L2 = icell2edge(j+(i-1)*3+1) ! edge of original non-triangle
!            L3 = icell2edge(j+(i-1)*3+2) ! "right" internal
!            
!            if ( i.eq.1 ) then
!               qe(L1) = 0d0
!            end if
!            
!!           determine "right" internal flux from flux balance with other two fluxes
!            qe(L3) = sumq*areacell(k)  ! outward normal assumed (corrected later)
!            
!            if ( edge2cell(1,L1).eq.k ) then ! outward normal
!               qe(L3) = qe(L3) - qe(L1)
!            else  ! inward normal
!               qe(L3) = qe(L3) + qe(L1)
!            end if
!            
!            if ( edge2cell(1,L2).eq.k ) then ! outward normal
!               qe(L3) = qe(L3) - qe(L2)
!            else  ! inward normal
!               qe(L3) = qe(L3) + qe(L2)
!            end if
!            
!!           account for orientation            
!            if ( edge2cell(2,L3).eq.k ) then ! outward normal
!               qe(L3) = -qe(L3)
!            end if
!            
!!           add to circulation
!            icL = edge2cell(1,L3)
!            icR = edge2cell(2,L3)
!            dlen(i) = 0.25d0*(areacell(icL)+areacell(icR))/w(L3)
!            sumlen = sumlen + dlen(i)/w(L3)
!            if ( icR.eq.k ) then
!               dlen(i) = -dlen(i)
!            end if
!            
!            dcirc = qe(L3)/w(L3)*dlen(i)
!            circ = circ + dcirc
!            
!!           update cell number
!            k = k+1  
!         end do
!         
!!        correct for circulation
!         dq = -circ/sumlen
!         do i=1,N
!            L3 = icell2edge(j+(i-1)*3+2) ! "right" internal
!            if ( dlen(i).gt.0d0 ) then
!               qe(L3) = qe(L3) + dq
!            else
!               qe(L3) = qe(L3) - dq
!            end if
!         end do
!      else
!         k = k+1
!      end if
!   end do
!   
!   if ( allocated(dum) ) deallocate(dum)
   
    return
end subroutine set_fluxes


!> compute mapping from prescribed (at flowlinks) to all fluxes (at all edges, including "internal")
subroutine comp_fluxcoeffs()
   use m_partmesh
   use m_partfluxes
   use m_alloc
   use unstruc_messages
   use geometry_module, only: gaussj
   implicit none
   
   integer                                  :: N         ! number of subtriangles
                     
   integer,          dimension(MAXSUBCELLS) :: L1        ! "internal" edges
   integer,          dimension(MAXSUBCELLS) :: L2        ! original "non-internal" edges
   integer,          dimension(MAXSUBCELLS) :: icell     ! subcells
   integer,          dimension(MAXSUBCELLS) :: isign1    ! orientation of "internal: edges L1, all cw or 
   integer,          dimension(MAXSUBCELLS) :: isign2    ! orientation of "non-internal: edges L2, outward normal
   double precision, dimension(MAXSUBCELLS) :: circ      ! weight of edge L1 in computation of circulation
   
   double precision, dimension(MAXSUBCELLS,MAXSUBCELLS) :: A, B
   
   double precision                         :: areasum, dlen
   
   integer                                  :: i, im1, ip1, j, j2, k, L, L3, Lf
                                     
   integer, external                        :: icommonval
   
!  allocate
   call realloc_partfluxes()
!   call realloc(jflux2link, numedges+1, fill=0, keepExisting=.false.)
   
!  set startpointers
   jflux2link(1) = 1
   L = 1
   do while ( L.le.numedges )
      Lf = edge2link(L)
      if ( Lf.gt.0 ) then
!        original flowlink
         jflux2link(L+1) = jflux2link(L) + 1
         
!        proceed to next edge         
         L = L+1
      else if ( L.gt.numorigedges ) then
!        internal edge: find number of subtriangels
         N = 0
         do while ( icommonval(edge2cell(:,L+N), edge2cell(:,L+N+1)).ne.0 )
            N = N+1
            if ( L+N.ge.numedges ) exit
         end do
         
         if ( N.gt.0 ) then
!           check connection first and last subtriangle
            if ( icommonval(edge2cell(:,L), edge2cell(:,L+N)).ne.0 ) then
               N = N+1
               
               do i=1,N
                  jflux2link(L+1) = jflux2link(L) + N
   !              proceed to next edge  
                  L = L+1
               end do
            else
               call mess(LEVEL_ERROR, 'comp_fluxcoeffs: numbering error')
            end if
         
         else  ! should not happen
            call mess(LEVEL_ERROR, 'comp_fluxcoeffs: numbering error')
         end if
         
         if ( L.ge.numedges ) exit
      else  ! other
         jflux2link(L+1) = jflux2link(L)
!        proceed to next edge  
         L = L+1
      end if
   end do
   
!  reallocate
   call realloc(iflux2link, jflux2link(numedges+1)-1, fill=0, keepExisting=.false.)
   call realloc(Aflux2link, jflux2link(numedges+1)-1, fill=0d0, keepExisting=.false.)
   
!  fill iflux2link (first in edge-numbers) and compute coefficients
   L = 1
   do while ( L.le.numedges )
      j=jflux2link(L)
      N = jflux2link(L+1)-j
      
      if ( N.eq.1 ) then
!        original "non-internal" link      
         iflux2link(j) = L
         Aflux2link(j) = 1d0
!        proceed to next edge  
         L = L+1
      else if ( N.gt.1 ) then
!        "internal" link
         do i=1,N
            ip1 = i+1; if ( ip1.gt.N ) ip1 = ip1-N
            L1(i) = L+i-1
            L3 = L1(i)+ip1
            
!           find subcell            
            icell(i) = icommonval(edge2cell(:,L+i-1), edge2cell(:,L+ip1-1))
            
!           find original edge
            j2 = jcell2edge(icell(i))+1   ! order of cell edges: "left" internal (L1), orginal (L2), "right internal (L3)
            L2(i) = icell2edge(j2)
            
!           get orientation of "internal" edges (i-dir positive)
!           note: will always be (/ -1, 1, 1, ... /), due to generation of edge2cells in part_setmesh
            isign1(i) = 1
            if ( edge2cell(1,L1(i)).eq.icell(i) ) isign1(i) = -1
            
!           get orientation of original "non-internal" edges (outward normal)
            isign2(i) = 1
            if ( edge2cell(2,L2(i)).eq.icell(i) ) isign2(i) = -1
         end do
         
!        compute summed area and circulation weights
         areasum = 0d0
         do i=1,N
            im1 = i-1; if ( im1.lt.1 ) im1=im1+N
            areasum = areasum + areacell(icell(i))
            dlen = 0.25*(areacell(icell(im1))+areacell(icell(i)))/w(L1(i))
            circ(i) = dlen/w(L1(i))
         end do
         
!        build system: A qe = B qlink
         A = 0d0
         B = 0d0
!        continuity equations
         do i=1,N-1
            A(i,i)   = -isign1(i)   ! outward
            A(i,i+1) =  isign1(i+1) ! outward
            B(i,i)   = -isign2(i)   ! outward, rhs
            do k=1,N
               B(i,k) = B(i,k) + areacell(icell(i))*isign2(k)/areasum
            end do
         end do
!        circulation
         do k=1,N
            A(N,k) = circ(k) * isign1(k)
         end do
         
!        invert matrix: qe = inv(A) B qlink
         call gaussj(A,N,MAXSUBCELLS,B,N,MAXSUBCELLS)
         
!        fill data
         do i=1,N
            iflux2link(j:j+N-1) = L2(1:N)
            Aflux2link(j:j+N-1) = B(i,1:N)
            
!           proceed to next edge  
            j = j+N
            L = L+1
         end do
      else
!        closed boundary: proceed to next edge
         L = L+1
      end if
   end do
   
!  convert to link number
   do j=1,jflux2link(numedges+1)-1
      L  = iflux2link(j)
      Lf = edge2link(L)
      if ( Lf.gt.0 ) then
         iflux2link(j) = Lf
      else  ! closed boundary
         iflux2link(j) = 0
      end if
   end do
   
   return
end subroutine comp_fluxcoeffs

!> find common value of two-dimensional arrays i1 and i2
!>   it is assumed there is at most one common value
integer function icommonval(i1, i2)
   implicit none
   
   integer, dimension(2), intent(in)  :: i1
   integer, dimension(2), intent(in)  :: i2
   
   icommonval = 0
   if ( i1(1).eq.i2(1) .or. i1(1).eq.i2(2) ) then
      icommonval = i1(1)
   else if ( i1(2).eq.i2(1) .or. i1(2).eq.i2(2) ) then
      icommonval = i1(2)
   end if
   
   return
end function icommonval

!> plot mesh admin
subroutine tekpartmesh()
   use m_particles
   use m_partmesh
   use m_partrecons
   use m_wearelt
   use unstruc_display
   use m_sferic, only: jsferic
   use geometry_module, only: Cart3Dtospher
   implicit none
   
   character(len=32) :: text
   
   double precision :: xL, yL, xL1, yL1, x, y, dfac
   
   integer          :: i, k, k1, k2, L
   
   if ( japart.ne.1 .or. ndrawpart.eq.1 ) return
   
!  edges   
   do L=1,numedges
      k1 = edge2node(1,L)
      k2 = edge2node(2,L)
      
      if ( jsferic.eq.0 ) then
         xL = 0.5d0*(xnode(k1)+xnode(k2))
         yL = 0.5d0*(ynode(k1)+ynode(k2))
         xL1 = xL + rcir*dnx(1,L)
         yL1 = yL + rcir*dny(1,L)
      else
         dfac = 0.1d0*w(L)
         call Cart3Dtospher(0.5d0*(xnode(k1)+xnode(k2)), 0.5d0*(ynode(k1)+ynode(k2)), 0.5d0*(znode(k1)+znode(k2)), xL, yL, 0d0)
         if( edge2cell(1,L).gt.0 ) then
            call Cart3Dtospher(0.5d0*(xnode(k1)+xnode(k2))+dfac*dnx(1,L), 0.5d0*(ynode(k1)+ynode(k2))+dfac*dny(1,L), 0.5d0*(znode(k1)+znode(k2))+dfac*dnz(1,L), xL1, yL1, 0d0)
         else
            call Cart3Dtospher(0.5d0*(xnode(k1)+xnode(k2))-dfac*dnx(2,L), 0.5d0*(ynode(k1)+ynode(k2))-dfac*dny(2,L), 0.5d0*(znode(k1)+znode(k2))-dfac*dnz(2,L), xL1, yL1, 0d0)
         end if
      end if
      
      if ( jsferic.eq.0 ) then
         call movabs(xnode(k1),ynode(k1))
         call lnabs(xnode(k2),ynode(k2))
      
         call movabs(xL,yL)
         call lnabs(xL1,yl1)
!         call hitext(L,xL1,yL1)
         write(text,"(I0, '(', F0.1, ')')") L, qe(L)
         call drawtext(real(xL1),real(yL1),trim(text))
      else
         call Cart3Dtospher(xnode(k1),ynode(k1), znode(k1), x, y, 0d0)
         call movabs(x,y)
         call Cart3Dtospher(xnode(k2),ynode(k2), znode(k2), x, y, 0d0)
         call lnabs(x,y)
      
         call movabs(xL,yL)
         call lnabs(xL1,yl1)
      end if
   end do
   
!  cells
   if ( jsferic.eq.0 ) then
      do k=1,numcells
         call hitext(k,xzwcell(k),yzwcell(k))
      end do
   else
      do k=1,numcells
         call cart3Dtospher(xzwcell(k),yzwcell(k),zzwcell(k),x,y,0d0)
         call hitext(k,x,y)
      end do
   end if
   
!  particle cell numbers
   do i=1,Npart
!      call hitext(i,xpart(i)+rcir,ypart(i))
!      call hitext(kpart(i),xpart(i)+rcir,ypart(i))
      write(text,"(I0, '(', I0, ')')") iglob(i), kpart(i)
      if ( jsferic.eq.0 ) then
         call drawtext(real(xpart(i)+rcir),real(ypart(i)),trim(text))
      else
         call cart3Dtospher(xpart(i),ypart(i),zpart(i),x,y,0d0)
         call drawtext(real(x+rcir),real(y),trim(text))
      end if
   end do
   
   return
end subroutine tekpartmesh

logical function get_japart()
   use m_particles
   implicit none
   
   get_japart = ( japart.eq.1 )
   
   return
end function get_japart

!> add particles
subroutine add_particles(Nadd, xadd, yadd, jareplace, jadomain)
   use m_particles
   use m_partmesh
   use m_alloc
   use m_wearelt
   use m_sferic, only: jsferic
   use geometry_module, only: sphertocart3D
   use m_partitioninfo

   implicit none
   
   integer,                           intent(in)  :: Nadd       !< number of particles to be added
   double precision, dimension(Nadd), intent(in)  :: xadd       !< x-coordinates of particles to be added
   double precision, dimension(Nadd), intent(in)  :: yadd       !< y-coordinates of particles to be added
   integer,                           intent(in)  :: jareplace  !< replace existing but disabled particles(1) or not (0)
   integer,                           intent(in)  :: jadomain   !< only place particles in own subdomain (1) or not (0)
   
   integer,          dimension(:),    allocatable :: kadd       !< cell numbers
   
   integer                                        :: i, ipoint, Nsize
   integer                                        :: ierror
   integer                                        :: Npartnew
   integer                                        :: Nreplace
   integer                                        :: n, Nloc
   
   double precision                               :: xn, yn, zn, dn
   integer                                        :: k, k1
   
   if ( japart.ne.1 ) return
   
!  get number of existing particles that may be replaced
   if ( jareplace.eq.1 ) then
      Nreplace = 0
      do i=1,Npart
         if ( kpart(i).eq.0 ) then
            Nreplace = Nreplace+1
         end if
      end do
   else
      Nreplace = 0
   end if
   
!  get new particle cell number
   allocate(kadd(Nadd))
   kadd = 0
   call part_findcell(Nadd,xadd,yadd,kadd,ierror)
   
!  count particles to be added   
   Nloc = 0
   do i=1,Nadd
!     get cell number
      k = kadd(i)
      
      if ( k.gt.0 ) then
         if ( jampi.eq.1 .and. jadomain.eq.1 ) then
!           only allow particles in own subdomain
            n = iabs(cell2nod(k))
            if ( idomain(n).ne.my_rank ) then
               kadd(i) = 0
            end if
         end if
      
         if ( kadd(i).gt.0 ) then
            Nloc = Nloc + 1
         end if
      end if
   end do
   
!  new number of particles
   Npartnew = Npart + max(Nloc-Nreplace,0)
   
!  get current array sizes
   if ( allocated(xpart) ) then
      Nsize = size(xpart)
   else
      Nsize = 0
   end if
   
!  realloc
   if ( Npartnew.gt.Nsize ) then
!     compute new array size   
      Nsize = int(1.2*dble(Npartnew)) + 1
      call realloc_particles(Nsize, .true., ierror)
   end if
   
!!  get new particle cell number
!   allocate(kadd(Nadd))
!   kadd = 0
!   call part_findcell(Nadd,xadd,yadd,kadd,ierror)
   
!  fill data
   if ( jareplace.eq.1 ) then
      ipoint = 1
   else
      ipoint = Npart+1
   end if
   
   do i=1,Nadd
      if ( kadd(i).eq.0 ) cycle
      
      if ( ipoint.le.Npart ) then
         do while ( kpart(ipoint).ne.0 )
            ipoint = ipoint+1
         end do
      end if
      
      if ( jsferic.eq.0 ) then
         xpart(ipoint) = xadd(i)
         ypart(ipoint) = yadd(i)
      else
         call sphertocart3D(xadd(i),yadd(i),xpart(ipoint),ypart(ipoint),zpart(ipoint))
         
         
         
         if ( jsferic.eq.1 ) then
!           project particle on triangle
            k = kadd(i)
            if ( k.gt.0 ) then
               k1 = edge2node(1,icell2edge(jcell2edge(k)))
               xn = xnode(k1)
               yn = ynode(k1)
               zn = znode(k1)
               dn = (xpart(ipoint) - xn) * dnn(1,k) +  &
                    (ypart(ipoint) - yn) * dnn(2,k) +  &
                    (zpart(ipoint) - zn) * dnn(3,k)
               xpart(ipoint) = xpart(ipoint) - dn * dnn(1,k)
               ypart(ipoint) = ypart(ipoint) - dn * dnn(2,k)
               zpart(ipoint) = zpart(ipoint) - dn * dnn(3,k)
            end if
         end if
         
      end if
      kpart(ipoint) = kadd(i)
      iglob(ipoint) = Nglob + i
!      write(namepart(ipoint), "('added_particle ', I0)") i
!      Npart = Npart+1
      Npart = max(Npart,ipoint)
      
!     plot      
      call setcol(31)
!      call movabs(xpart(ipoint),ypart(ipoint))
      call movabs(xadd(i), yadd(i))
      call cir(rcir)
      
!     advance pointer
      ipoint = ipoint+1
   end do
   
   Nglob = Nglob + Nadd
   
!  deallocate
   if ( allocated(kadd) ) deallocate(kadd)
   
   return
end subroutine add_particles



!> find in which cells particles are located
subroutine part_findcell(Npart, xxpart, yypart, kpart, ierror)
   use m_partmesh
   use unstruc_messages
   use kdtree2Factory
   use m_sferic, only: jsferic, jasfer3D
   use m_missing, only: jins, dmiss
   use geometry_module, only: pinpok, dbdistance, pinpok3D, cart3Dtospher
   implicit none
   
   integer,                            intent(in)  :: Npart    !< number of particles
   double precision, dimension(Npart), intent(in)  :: xxpart   !< particle x-coordinates, 2D Cartexsion or spherical coordinates (not 3D Cartesian)
   double precision, dimension(Npart), intent(in)  :: yypart   !< particle x-coordinates, 2D Cartexsion or spherical coordinates (not 3D Cartesian)
   integer,          dimension(Npart), intent(out) :: kpart    !< cell numbers
                                                   
   integer                           , intent(out) :: ierror   !< error (1) or not (0)
   
   type(kdtree_instance)                           :: kdtree
   
   double precision, dimension(3)                  :: xv, yv
   
   double precision                                :: dmaxsize
   double precision                                :: R2search
   double precision                                :: xx, yy
   
   integer                                         :: i, ip1, j, k, knode, L, Lp1, N, NN
   integer                                         :: inside
   
   ierror = 1
   
!  build kdtree   
   call build_kdtree(kdtree, Npart, xxpart, yypart, ierror, jsferic, dmiss)
   if ( ierror.ne.0 ) then
      goto 1234
   end if
   
   if ( Npart.lt.1 ) then
      ierror = 0
      goto 1234
   end if
   
   kpart = 0
   
!  loop over cells
   do k=1,numcells
!     check cell size
      N = jcell2edge(k+1)-jcell2edge(k)
      if ( N.ne.3 ) then
         call mess(LEVEL_ERROR, 'part_findcell: non-triangle')
         goto 1234
      end if
      
!     get cell polygon
      i=0
      do j = jcell2edge(k),jcell2edge(k+1)-1
         i = i+1
         L = icell2edge(j)
         ip1 = i+1; if ( ip1.gt.3 ) ip1=ip1-3
         Lp1 = icell2edge(j-i+ip1)
!        find common node of L and Lp1
         if ( edge2node(1,L).eq.edge2node(1,Lp1) .or. edge2node(1,L).eq.edge2node(2,Lp1) ) then
            knode = edge2node(1,L)
         else if ( edge2node(2,L).eq.edge2node(1,Lp1) .or. edge2node(2,L).eq.edge2node(2,Lp1) ) then
            knode = edge2node(2,L)
         else  ! should not happen
            continue
         end if
         if ( jsferic.eq.0 ) then
            xv(i) = xnode(knode)
            yv(i) = ynode(knode)
         else
            call Cart3Dtospher(xnode(knode),ynode(knode),znode(knode),xv(i),yv(i),0d0)
         end if
      end do
      
!     fill query vector
      if ( jsferic.eq.0 ) then
         call make_queryvector_kdtree(kdtree,xzwcell(k),yzwcell(k), jsferic)
      else
         call cart3Dtospher(xzwcell(k),yzwcell(k),zzwcell(k),xx,yy,0d0)
         call make_queryvector_kdtree(kdtree,xx,yy, jsferic)
      end if
      
!     compute maximum flowcell dimension
      dmaxsize = 0d0
      do i=1,N
         ip1=i+1; if ( ip1.gt.N ) ip1=ip1-N
         dmaxsize = max(dmaxsize, dbdistance(xv(i),yv(i),xv(ip1),yv(ip1),jsferic, jasfer3D, dmiss))
      end do
      
!     determine square search radius
      R2search = 1.1d0*dmaxsize**2  ! 1.1d0: safety

!     count number of points in search area
      NN = kdtree2_r_count(kdtree%tree,kdtree%qv,R2search)

      if ( NN.eq.0 ) cycle ! no particles found
      
!     reallocate if necessary
      call realloc_results_kdtree(kdtree,NN)
     
!     find nearest NN samples
      call kdtree2_n_nearest(kdtree%tree,kdtree%qv,NN,kdtree%results)

!     check if samples are in cell
      do i=1,NN
         j = kdtree%results(i)%idx
         if ( jsferic.eq.0 ) then
            call pinpok(xxpart(j), yypart(j), 3, xv, yv, inside, jins, dmiss)
         else
            call pinpok3D(xxpart(j), yypart(j), 3, xv, yv, inside, dmiss, jins, jsferic, jasfer3D)
         end if
         
         if ( inside.eq.1 ) then
            if ( kpart(j).eq.0 ) then
               kpart(j) = k
            end if
         end if
      end do
   end do
   
   ierror = 0

1234 continue
   
!  deallocate
   if ( kdtree%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(kdtree)
   
   return
end subroutine part_findcell

!> copy samples to particles
subroutine copy_sam2part()
   use m_particles
   use m_samples
   implicit none

   character(len=255) :: dum
   
   integer, dimension(1) :: idum
   
   integer :: i
   integer :: ierror
   
   if ( NS.lt.1 ) return
   
   if ( japart.ne.1 ) then
      dum = ' '
      call ini_part(0, dum, 0,0d0,0d0,0)
   end if
   
   call add_particles(Ns, xs, ys, 0, 1)
   
   call delsam(0)
   
end subroutine copy_sam2part


!> (re)allocate
subroutine realloc_particles(Nsize, LkeepExisting, ierror)
   use m_particles
   use m_alloc
   use m_missing
   use m_sferic, only: jsferic
   implicit none
   
   integer, intent(in)  :: Nsize          !< array sizes
   logical, intent(in)  :: LkeepExisting  !< keep existing data (1) or not (0)
   integer, intent(out) :: ierror         !< error (1) or not
   
   ierror = 1
   
!  reallocate   
   call realloc(xpart, Nsize, keepExisting=LkeepExisting, fill=DMISS)
   call realloc(ypart, Nsize, keepExisting=LkeepExisting, fill=DMISS)
   if ( jsferic.eq.1 ) then
      call realloc(zpart, Nsize, keepExisting=LkeepExisting, fill=DMISS)
   end if
   call realloc(dtremaining, Nsize, keepExisting=LkeepExisting, fill=0d0)
   call realloc(kpart, Nsize, keepExisting=LkeepExisting, fill=0)
   call realloc(iglob, Nsize, keepExisting=LkeepExisting, fill=0)
   
   call realloc(numzero, Nsize, keepExisting=LkeepExisting, fill=0)
   numzero = 0
   
   ierror = 0
1234 continue
   
   return
end subroutine realloc_particles

!> deallocate particle data
subroutine dealloc_particles()
   use m_particles
   implicit none
   
   if ( allocated(xpart)       ) deallocate(xpart)
   if ( allocated(ypart)       ) deallocate(ypart)
   if ( allocated(zpart)       ) deallocate(zpart)
   if ( allocated(dtremaining) ) deallocate(dtremaining)
   if ( allocated(kpart)       ) deallocate(kpart)
   if ( allocated(iglob)       ) deallocate(iglob)
   
   if ( allocated(numzero)     ) deallocate(numzero)
   
   Npart = 0
   
   return
end subroutine dealloc_particles


!> (re)allocate partmesh data
subroutine realloc_partmesh()
   use m_partmesh
   use m_alloc
   use m_missing
   use m_sferic, only: jsferic
   implicit none
   
   integer :: N
   
   call realloc(edge2node, (/2, numedges/), fill=0, keepExisting=.false.)
   call realloc(edge2cell, (/2, numedges/), fill=0, keepExisting=.false.)
   call realloc(xnode, numnodes, fill=0d0, keepExisting=.false.)
   call realloc(ynode, numnodes, fill=0d0, keepExisting=.false.)
   if ( jsferic.eq.1 ) then
      call realloc(znode, numnodes, fill=0d0, keepExisting=.false.)
   end if
   
   call realloc(xzwcell, numcells, fill=DMISS, keepExisting=.false.)
   call realloc(yzwcell, numcells, fill=DMISS, keepExisting=.false.)
   if ( jsferic.eq.1 ) then
      call realloc(zzwcell, numcells, fill=DMISS, keepExisting=.false.)
   end if
   call realloc(areacell, numcells, fill=DMISS, keepExisting=.false.)
   
   if ( jsferic.eq.0 ) then
      call realloc(dnx, (/1, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dny, (/1, numedges/), fill=DMISS, keepExisting=.false.)
   else
      call realloc(dnx, (/2, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dny, (/2, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dnz, (/2, numedges/), fill=DMISS, keepExisting=.false.)
      call realloc(dnn, (/3, numcells/), fill=DMISS, keepExisting=.false.)
   end if
   call realloc(w, numedges, fill=DMISS, keepExisting=.false.)
   
   call realloc(edge2link, numedges, fill=0, keepExisting=.false.)
!   call realloc(nod2cell, numcells, fill=0, keepExisting=.false.)
   call realloc(cell2nod, numcells, fill=0, keepExisting=.false.)
   
   call realloc(jcell2edge, numcells+1, fill=1, keepExisting=.false.)
   N = jcell2edge(numcells+1)-1
   call realloc(icell2edge, N, fill=0, keepExisting=.false.)
   
   return
end subroutine realloc_partmesh

!> deallocate particle mesh data
subroutine dealloc_partmesh()
   use m_partmesh
   implicit none
   
   if ( allocated(edge2node ) ) deallocate(edge2node )
   if ( allocated(edge2cell ) ) deallocate(edge2cell )
   if ( allocated(xnode     ) ) deallocate(xnode     )
   if ( allocated(ynode     ) ) deallocate(ynode     )
   if ( allocated(znode     ) ) deallocate(znode     )
   
   if ( allocated(xzwcell   ) ) deallocate(xzwcell   )
   if ( allocated(yzwcell   ) ) deallocate(yzwcell   )
   if ( allocated(zzwcell   ) ) deallocate(zzwcell   )
   if ( allocated(areacell  ) ) deallocate(areacell  )
   
   if ( allocated(dnn       ) ) deallocate(dnn       )
   if ( allocated(dnx       ) ) deallocate(dnx       )
   if ( allocated(dny       ) ) deallocate(dny       )
   if ( allocated(dnz       ) ) deallocate(dnz       )
   if ( allocated(w         ) ) deallocate(w         )
   
   if ( allocated(edge2link ) ) deallocate(edge2link )
!  if ( allocated(nod2cell  ) ) deallocate(nod2cell  )
   if ( allocated(cell2nod  ) ) deallocate(cell2nod  )
   
   if ( allocated(icell2edge) ) deallocate(icell2edge)
   if ( allocated(jcell2edge) ) deallocate(jcell2edge)
   
   
   return
end subroutine dealloc_partmesh


!> (re)allocate flux coefficients
subroutine realloc_partfluxes()
   use m_partmesh
   use m_partfluxes
   use m_alloc
   use m_missing
   implicit none
   
   integer :: N
   
   call realloc(jflux2link, numedges+1, keepExisting=.false., fill=1)
   N = jflux2link(numedges+1)-1
   call realloc(iflux2link, N,  keepExisting=.false., fill=0)
   call realloc(Aflux2link, N,  keepExisting=.false., fill=0d0)
   
   return
end subroutine realloc_partfluxes

!> deallocate flux_coeffs
subroutine dealloc_partfluxes()
   use m_partfluxes
   implicit none
   
   if ( allocated(iflux2link) ) deallocate(iflux2link)
   if ( allocated(jflux2link) ) deallocate(jflux2link)
   if ( allocated(Aflux2link) ) deallocate(Aflux2link)
end subroutine dealloc_partfluxes

!> (re)allocate flux coefficients et cetera
subroutine realloc_partrecons()
   use m_partmesh
   use m_partrecons
   use m_alloc
   use m_missing
   use m_sferic, only: jsferic
   implicit none
   
   call realloc(qe, numedges, keepExisting=.false., fill=DMISS)
   
   call realloc(u0x, numcells, keepExisting=.false., fill=DMISS)
   call realloc(u0y, numcells, keepExisting=.false., fill=DMISS)
   if ( jsferic.eq.1 ) then
      call realloc(u0z, numcells, keepExisting=.false., fill=DMISS)
   end if
   call realloc(alpha, numcells, keepExisting=.false., fill=DMISS)
   
   call realloc(ireconst, numcells+1, keepExisting=.false., fill=0)
   return
end subroutine realloc_partrecons

!> deallocate flux_coeffs
subroutine dealloc_partrecons()
   use m_partrecons
   implicit none
   
   if ( allocated(qe) ) deallocate(qe)
   
   if ( allocated(u0x) ) deallocate(u0x)
   if ( allocated(u0y) ) deallocate(u0y)
   if ( allocated(u0z) ) deallocate(u0z)
   if ( allocated(alpha) ) deallocate(alpha)
   
   if ( allocated(ireconst) ) deallocate(ireconst)
   if ( allocated(jreconst) ) deallocate(jreconst)
   if ( allocated(Areconst) ) deallocate(Areconst)
   
   return
end subroutine dealloc_partrecons

!> initialize particles
subroutine ini_part(japartfile, partfile, jatracer_loc, starttime_loc, timestep_loc, threeDtype_loc)
   use m_particles
   use m_samples
   use m_flow, only: s1, kmx
   use m_transport, only: constituents, numconst
   use m_flowtimes, only: tstart_user
   use m_missing
   use m_partitioninfo
   use unstruc_messages
   implicit none
   
   integer,            intent(in) :: japartfile    !< use particle file (1) or not (0)
   character(len=255), intent(in) :: partfile      !< particle file
   integer,            intent(in) :: jatracer_loc  !< add tracer (1) or not (0)
   double precision,   intent(in) :: starttime_loc !< start time (>0) or not (0)
   double precision,   intent(in) :: timestep_loc  !< time step (>0) or every computational time step (0)
   integer,            intent(in) :: threeDtype_loc    !< depth averaged (0) or free surface (1)
   
   integer, dimension(1) :: idum
   
   integer             :: minp
   logical             :: lexist
   integer             :: iconst
   integer             :: ierror
   integer             :: i
   
!   if ( jampi.eq.0 ) then
!      call newfil(mfile,'part.xyz')
!   else
!      call newfil(mfile,'part_'//sdmn//'.xyz')
!   end if
   
!  deallocate
   call dealloc_partmesh()
   call dealloc_partfluxes()
   call dealloc_partrecons()
   call dealloc_particles()
   call dealloc_auxfluxes()
   call dealloc_partparallel()
   
   Nglob = 0
   
   timenext = 0d0
   timelast = DMISS
   
   jatracer = 0
      
!  add particle tracer (when tracers are initialized)
   if ( jatracer_loc.eq.1 ) then
      jatracer = 1
   end if
   
!  start time
   if ( starttime_loc.gt.0d0 ) then
      starttime = starttime_loc
      timenext = starttime
   end if
   
!  time step
   if ( timestep_loc.gt.0d0 ) then
      timestep = timestep_loc
   end if
   
!  3D type
   if ( kmx.gt.0 ) then
      threeDtype = threeDtype_loc
   else  ! 2D
      threeDtype = 0
   end if
   
   if ( japartfile.eq.1 ) then
      japart = 0
      if ( len_trim(partfile).gt.0 ) then
   !     read initial samples from inputfile  
         inquire(FILE = trim(partfile), exist = lexist)
         if ( lexist ) then
            call oldfil(minp, partfile)
            call savesam()
            call reasam(minp, 0)
            japart = 1
         else
            call mess(LEVEL_ERROR, 'the specified initial particle locations file could not be found: ', trim(partfile))
         end if
      end if
   else  ! initialize only
      japart = 1
   end if
   
   if ( japart.eq.1 ) then
!     set pointers with mesh connectivity etc.
      call part_setmesh()
      
!     set flux coeffecients            
      call comp_fluxcoeffs()
      
      call realloc_partrecons()
      
      call reconst_vel_coeffs()
      
      if ( Ns.gt.0 ) then
         call add_particles(Ns, xs, ys, 0, 1)
         timepart = tstart_user
         
         call delsam(0)
      else
         Npart = 0
      end if
      
      if ( jatracer.eq.1 ) then
!        REMARK: tracer with particle concentration is ALSO updated by transport module (not necessary)      
         call add_tracer(PART_TRACERNAME, part_iconst)
!        compute concentration (overwrite update by transport module)
         call comp_concentration(s1,numconst,part_iconst,constituents)
      end if
   
      call alloc_auxfluxes()
   
      call ini_partparallel()
   end if
   
   return
end subroutine ini_part

!> compute concentrations of particles (parts per unit volume) in flownodes
subroutine comp_concentration(s, nconst, iconst, c)
   use m_particles
   use m_partmesh
   use m_flowgeom, only : Ndx, ba, bl
   use m_flowparameters, only: epshs
   use m_flow, only: Ndkx
   implicit none

   double precision, dimension(Ndx),        intent(in)  :: s      !< water level
   integer,                                 intent(in)  :: nconst !< number of constituents
   integer,                                 intent(in)  :: iconst !< particle tracer constituent number
   double precision, dimension(Nconst,Ndx), intent(out) :: c      !< constituents
   
   integer :: i, k
   
   do i=1,Ndx
      c(iconst,i) = 0d0
   end do
   
!  count number of particles per cell   
   do i=1,Npart
      k = kpart(i)
      if ( k.eq.0 ) cycle
      
      k = iabs(cell2nod(k))
      
      c(iconst,k) = c(iconst,k) + 1
   end do
   
!  compute concentration (parts per unit volume)
   do k=1,Ndx
      if ( s(k)-bl(k) .gt. epshs ) then
         c(iconst,k) = c(iconst,k) / (ba(k)*(s(k)-bl(k)))
      else
         c(iconst,k) = 0d0
      end if
   end do
   
   return
end subroutine comp_concentration

!> allocate auxiliary fluxes
subroutine alloc_auxfluxes()
   use m_particles
   use m_flowgeom, only: Ndx, Lnx
   use m_alloc
   implicit none
   
   if ( timestep.gt.0d0 ) then
      call realloc(sbegin, Ndx, fill=0d0, keepExisting=.false.)
      call realloc(qpart, Lnx, fill=0d0, keepExisting=.false.)
   end if
   
   if ( threeDtype.eq.1 ) then
      call realloc(qfreesurf, Lnx, fill=0d0, keepExisting=.false.)
   end if
   
   return
end subroutine alloc_auxfluxes

!> deallocate auxiliary fluxes
subroutine dealloc_auxfluxes()
   use m_particles
   implicit none
   
   if ( allocated(sbegin) ) deallocate(sbegin)
   if ( allocated(qpart)  ) deallocate(qpart)
   
   if ( allocated(qfreesurf) ) deallocate(qfreesurf)
   
   return
end subroutine dealloc_auxfluxes

subroutine part_sumfluxes(q1,Dts)
   use m_particles
   use m_partmesh
   use m_flowgeom, only: Lnx
   implicit none
   
   double precision, dimension(Lnx), intent(in) :: q1  !< fluxes
   double precision,                 intent(in) :: Dts !< time interval
   
   integer :: L
   
   do L=1,Lnx
      qpart(L) = qpart(L) + q1(L)*dts
   end do
   
   return
end subroutine part_sumfluxes

!> update particles or add to summed fluxes
subroutine update_part()
   use m_particles
   use m_flowtimes
   use m_flowgeom, only: Lnx, wu, bl
   use m_flow
   use m_transport, only: numconst, constituents
   use m_missing
   implicit none
   
   integer                                     :: LL, Lb, Lt
   
   logical                     :: Lsurface
   
   double precision, parameter :: huni=1d0
   
   if ( japart.ne.1 ) return
   
   Lsurface = ( threeDtype.eq.1 )
   
   if ( Lsurface ) then
      do LL=1,Lnx
         call getLbotLtop(LL,Lb,Lt)
         qfreesurf(LL) = u1(Lt)*huni*wu(LL)
      end do
   end if

   if ( time0.ge.starttime ) then
   
      if ( timestep.le.0d0 ) then   ! update particles every computational time step
         if ( .not.Lsurface ) then
            call update_particles(q1,s0,s1,dts)
         else
            call update_particles(qfreesurf,bl+huni,bl+huni,dts)
         end if
         timepart = time1
         
         if ( jatracer.eq.1 ) then
!           udpate particle concentration   
            call comp_concentration(s1,numconst,part_iconst,constituents)
         end if
      else
      
!        check if timestep has been started
         if ( timelast.eq.DMISS ) then
!           start particle timestep
            timelast = time0
            timenext = time0+timestep
            sbegin = s0
            qpart = 0d0
         end if
         
!        sum fluxes of this computational time step
         if ( .not.Lsurface ) then
            call part_sumfluxes(q1,Dts)
         else
            call part_sumfluxes(qfreesurf,Dts)
         end if
         
         if ( time1.ge.timenext ) then
!           finish particle timestep         
            qpart = qpart/(time1-timelast)
            if ( .not.Lsurface ) then
               call update_particles(qpart, sbegin, s1, time1-timelast)
            else
               call update_particles(qfreesurf, bl+huni, bl+huni, time1-timelast)
            end if
            timepart = time1
            
!           start new particle timestep
            timelast = time1
            timenext = time1 + timestep
            sbegin = s1
            qpart = 0d0
   
            if ( jatracer.eq.1 ) then
!              udpate particle concentration   
               call comp_concentration(s1,numconst,part_iconst,constituents)
            end if
            
         end if
      end if
   end if
   
   return
end subroutine update_part

subroutine finalize_part()
   use m_particles
   implicit none
   
   call dealloc_partmesh()
   call dealloc_partfluxes()
   call dealloc_partrecons()
   call dealloc_particles()
   call dealloc_auxfluxes()
   call dealloc_partparallel()
   japart = 0
      
   return
end subroutine finalize_part


! send/receive paticles from other subdomains
subroutine partition_update_particles()
   use m_particles
   use m_partmesh
   use m_partitioninfo
   use m_partparallel
   use m_alloc
   implicit none
   
   integer                                       :: i, icell, idmn, j, k
   integer                                       :: numsend, numrecv, numnew
   integer                                       :: N, Nadd, Nsize
   integer                                       :: ipoint
   integer                                       :: Nreplace
   
   integer                                       :: ierror
   
   if ( jampi.eq.0 ) return
   
!  make sendlist  
   call part_makesendlist(Npart,kpart)
   
   numsend = jsend(ndomains)-1
   
!  copy particles to send array
   call part2send(jsend,isend)
   
!  send/recv data
   call sendrecv_particledata(NDIM,jsend,jrecv)
   
!  deactive sent particles
   do j=1,jsend(ndomains)-1
      i = isend(j)
      kpart(i) = 0
   end do
   
!  add received particles
   call recv2part(jrecv(ndomains)-1,workrecv)
   
   return
end subroutine partition_update_particles


!> copy particle data to send array worksnd
subroutine part2send(jsnd, isnd)
   use m_particles
   use m_partparallel
   use m_partitioninfo, only: ndomains
   use m_alloc
   implicit none
   
   integer, dimension(0:ndomains),        intent(in)  :: jsnd   !< subdomain start pointers in data arrays
   integer, dimension(jsend(ndomains)-1), intent(in)  :: isnd   !< particle numbers
   
   integer                                            :: i, j, kother
   integer                                            :: numsend, numnew
   
   numsend = jsnd(ndomains)-1
   
   if ( numsend.gt.0 ) then
!     realloc
      if ( NDIM.gt.ubound(worksnd,1) .or. numsend.gt.ubound(worksnd,2) ) then
         numnew = 1+int(1.2d0*dble(numsend))
         call realloc(worksnd, (/ NDIM,numnew /), keepExisting=.false.)
      end if
         
!     fill data arrays
      do j=1,numsend
         i=isnd(j)
            
         worksnd(INDX_XPART,j) = xpart(i)
         worksnd(INDX_YPART,j) = ypart(i)
         worksnd(INDX_DTREM,j) = dtremaining(i)
         worksnd(INDX_IGLOB,j) = dble(iglob(i))
         kother = icellother(kpart(i))
         if ( kother.gt.0 ) then ! will be send to other subdomain
            worksnd(INDX_KPART,j) = dble(kother)
         else  ! will not be send to other subdomain, used for backup in own subdomain
            worksnd(INDX_KPART,j) = dble(kpart(i))
         end if
         if ( INDX_ZPART.ne.0 ) then
            worksnd(6,j) = zpart(i)
         end if
      end do
   end if
   
   return
end subroutine part2send


!> add particles from received data
subroutine recv2part(numrecv,work)
   use m_particles
   use m_partparallel
   use m_partitioninfo, only: ndomains
   use unstruc_messages
   use m_missing
   implicit none
   
   integer,                                   intent(in)  :: numrecv  !< number of received particles
   double precision, dimension(NDIM,numrecv), intent(in)  :: work     !< received data
   
   integer,          dimension(:),            allocatable :: iperm
   
   integer                                                :: i, ipoint, j
   integer                                                :: Nreplace
                                                          
   integer                                                :: ierror
     
   if ( numrecv.gt.0 ) then
!     get number of existing particles that may be replaced
      Nreplace = 0
      do i=1,Npart
         if ( kpart(i).eq.0 ) then
            Nreplace = Nreplace+1
         end if
      end do
      
      call realloc_particles(Npart+max(numrecv-Nreplace,0), .true., ierror)
      
      ipoint = 1
      do j=1,numrecv
         if ( ipoint.le.Npart ) then
            do while ( kpart(ipoint).ne.0 )
               ipoint = ipoint+1
            end do
         end if
      
         xpart(ipoint) = work(INDX_XPART,j)
         ypart(ipoint) = work(INDX_YPART,j)
         dtremaining(ipoint) = work(INDX_DTREM,j)
         iglob(ipoint) = int(work(INDX_IGLOB,j))
         kpart(ipoint) = int(work(INDX_KPART,j))
         if ( INDX_ZPART.ne.0 ) then
            zpart(ipoint) = work(INDX_ZPART,j)
         end if
         
         Npart = max(Npart,ipoint)
      end do
   end if
   
   return
end subroutine recv2part


!> initialization for parallel computations
subroutine ini_partparallel()
   use m_particles, only: japart
   use m_partmesh
   use m_partparallel
   use m_partitioninfo, only: jampi, ndomains, DFM_COMM_DFMWORLD, my_rank
   use m_sferic
   use m_alloc
   use geometry_module, only: Cart3Dtospher
   implicit none
   
   double precision, dimension(:),   allocatable :: xrecv, yrecv, zrecv
                                     
   integer,          dimension(:),   allocatable :: icells, irecv
   
   double precision                              :: xref
   
   integer                                       :: i, icell, idmn, k
   integer                                       :: numsend, numrecv
   integer                                       :: N, nrequest
                                               
   integer                                       :: ierror
   
   integer                                       :: itag = 4

   if ( jampi.eq.0 .or. japart.eq.0 ) return
   
   if ( jsferic.eq.0 ) then
      NDIM = 5 ! for updating particles
      INDX_ZPART = 0
      N = 3
   else
      NDIM = 6 ! for updating particles
      INDX_ZPART = NDIM
      N = 4
   end if
   
!  allocate   
   call alloc_partparallel()
   
!  get other subdomain cellnumbers
   allocate(icells(numcells))
   icells = (/ (i, i=1,numcells) /)
   
!  make sendlist
   call part_makesendlist(numcells,icells)
   
!  fill send data
   call realloc(worksnd, (/ N,numcells /), keepExisting=.false., fill=0d0)
   do i=1,jsend(ndomains)-1
      icell = icells(isend(i))
      worksnd(1,i) = dble(icell)
      worksnd(2,i) = xzwcell(icell)
      worksnd(3,i) = yzwcell(icell)
      if ( jsferic.ne.0 ) then
         worksnd(4,i) = zzwcell(icell)
      end if
   end do
   
!  send/receive data
   call sendrecv_particledata(N,jsend,jrecv)
   
!  process received data
   numrecv = jrecv(ndomains)-1
   
   allocate(irecv(numrecv))
   allocate(xrecv(numrecv))
   allocate(yrecv(numrecv))
   if ( jsferic.ne.0 ) then
      allocate(zrecv(numrecv))
   end if
   
   xref = 0d0
   do i=1,jrecv(ndomains)-1
      irecv(i) = int(workrecv(1,i))
      if ( jsferic.eq.0 ) then
         xrecv(i) = workrecv(2,i)
         yrecv(i) = workrecv(3,i)
      else
         call Cart3Dtospher(workrecv(2,i),workrecv(3,i),workrecv(4,i),xrecv(i),yrecv(i),xref)
      end if
   end do
   
!  find which cells correspond to received cell coordinates
   call realloc(icells,numrecv,keepExisting=.false.,fill=0)
   call part_findcell(numrecv, xrecv, yrecv, icells, ierror)
   
!  send found cells back to other subdomains (recv is now send)
   call realloc(worksnd, (/ 1,numrecv /), keepExisting=.false., fill=0d0)
   do i=1,numrecv
      worksnd(1,i) = dble(icells(i))
   end do
   call sendrecv_particledata(1,jrecv,jsend)
   
!  fill other domain cell numbers
   numsend = jsend(ndomains)-1
   call realloc(icellother,numcells,keepExisting=.false.,fill=0)
   do i=1,numsend
      icell = isend(i)
      icellother(icell) = int(workrecv(1,i))
   end do
   
!  deallocate
   if ( allocated(irecv) ) deallocate(irecv)
   if ( allocated(xrecv) ) deallocate(xrecv)
   if ( allocated(yrecv) ) deallocate(yrecv)
   if ( allocated(zrecv) ) deallocate(zrecv)

   japartsaved = 0

   return
end subroutine ini_partparallel

subroutine alloc_partparallel()
   use m_partmesh
   use m_partparallel
   use m_partitioninfo, only: ndomains
   use m_alloc
   use m_sferic
   implicit none
   
   integer :: num
   
   call realloc(icellother, numcells)
   call realloc(jsend,ndomains,lindex=0)
   call realloc(isend,numcells)       ! first bound
   
   num = 10
   
   call realloc(worksnd, (/ NDIM, num /)) ! first bound
   call realloc(workrecv, (/ NDIM, num /)) ! first bound
   
   call realloc(jpoint, ndomains, lindex=0)
                      
   call realloc(irequest,3*ndomains)
   
   call realloc(jrecv,ndomains,lindex=0)
   
   call realloc(numsendarr, ndomains-1, lindex=0)
   call realloc(numrecvarr, ndomains-1, lindex=0)
   
   return
end subroutine alloc_partparallel

subroutine dealloc_partparallel
   use m_partparallel
   implicit none
   
   if ( allocated(icellother) ) deallocate(icellother)
   if ( allocated(jsend) ) deallocate(jsend)
   if ( allocated(isend) ) deallocate(isend)
   
   if ( allocated(worksnd) ) deallocate(worksnd)
   if ( allocated(workrecv) ) deallocate(workrecv)
   if ( allocated(jpoint) ) deallocate(jpoint)
   
   if ( allocated(irequest) ) deallocate(irequest)
   
   if ( allocated(jsend) ) deallocate(jrecv)
   
   if ( allocated(numsendarr) ) deallocate(numsendarr)
   if ( allocated(numrecvarr) ) deallocate(numrecvarr)
   
   return
end subroutine dealloc_partparallel

!> make CRS-formatted send list
subroutine part_makesendlist(N,icells)
   use m_partmesh
   use m_partitioninfo
   use m_partparallel
   use m_alloc
   implicit none
   
   integer                          :: N        !< number of cells to be send
   integer, dimension(N)            :: icells   !< cell numbers
                                    
!   integer, dimension(0:ndomains)   :: jsend    !< startpointers of send list
!   integer, dimension(N)            :: isend    !< send list
   
   integer                          :: i, icell, idmn, j, k
   integer                          :: numnew, numsend
   
!  count number of cells to be sent to other domains
   jsend = 0
   do i=1,N
!     get cell number
      icell = icells(i)
      
      if ( icell.eq.0 ) cycle
      
!     get flownode/netcell number
      k = iabs(cell2nod(icell))
      
!     get domain number
      idmn = idomain(k)
      
      if ( idmn.eq.my_rank ) cycle
      
!     update counters
      jsend(idmn+1) = jsend(idmn+1)+1
   end do
   
!  accumulate
   jsend(0) = 1
   do idmn=0,ndomains-1
      jsend(idmn+1) = jsend(idmn)+jsend(idmn+1)
   end do
   
   numsend = jsend(ndomains)-1
   
   if ( numsend.gt.ubound(isend,1) ) then
!     reallocate
      numnew = 1+int(1.2d0*dble(numsend))
      call realloc(isend,numnew,keepExisting=.false.,fill=0)
   end if
   
!  fill send list
   jpoint = jsend
   do i=1,N
!     get cell number
      icell = icells(i)
      
      if ( icell.eq.0 ) cycle
      
      k = iabs(cell2nod(icell))
      
!     get domain number
      idmn = idomain(k)
      
      if ( idmn.eq.my_rank ) cycle
      
      j = jpoint(idmn)
      
      isend(j) = i
      
      jpoint(idmn) = jpoint(idmn)+1
   end do
   
   return
end subroutine part_makesendlist

!> send/receive data from sendlist to/from worksend/workrecv and update recvlist
subroutine sendrecv_particledata(N,jsend,jrecv)
   use m_alloc
   use m_partparallel, only: worksnd, workrecv, irequest, numsendarr, numrecvarr
   use m_partitioninfo
#ifdef HAVE_MPI   
   use mpi
#endif
   implicit none
   
   integer,                        intent(in)  :: N      !< size of data
   integer, dimension(0:ndomains), intent(in)  :: jsend  !< subdomain startpointers in sent data arrays
   integer, dimension(0:ndomains), intent(out) :: jrecv  !< subdomain startpointers in received data arrays

#ifdef HAVE_MPI
   integer, dimension(MPI_STATUS_SIZE)         :: istat
                                              
   integer                                     :: i, idmn
   integer                                     :: numnew, numrecvtot
   integer                                     :: nrequest
   integer                                     :: ierror
                                              
   integer                                     :: itag1=3
   integer                                     :: itag2=4
   
!  send data
   nrequest = 0
   do idmn=0,ndomains-1
      if ( idmn.eq.my_rank ) cycle
      nrequest = nrequest+1
      numsendarr(idmn) = jsend(idmn+1)-jsend(idmn)
      call mpi_isend(numsendarr(idmn), 1, MPI_INTEGER, idmn, itag1, DFM_COMM_DFMWORLD, irequest(nrequest), ierror)
      
!      write(6,"('send ', I0, ' to ', I0, ': ', I0)") my_rank, idmn, numsendarr(idmn)
      if ( numsendarr(idmn).gt.0 ) then
         nrequest = nrequest+1
         call mpi_isend(worksnd(1,jsend(idmn)), N*numsendarr(idmn), MPI_DOUBLE_PRECISION, idmn, itag2, DFM_COMM_DFMWORLD, irequest(nrequest), ierror)
      end if
   end do
   
!  receive data
   jrecv(0) = 1
   do idmn=0,ndomains-1
      if ( idmn.eq.my_rank ) then
         jrecv(idmn+1) = jrecv(idmn)
         cycle
      end if
         
      call mpi_recv(numrecvarr(idmn),1,MPI_INTEGER,idmn,itag1,DFM_COMM_DFMWORLD,istat,ierror)
!      write(6,"('receive ', I0, ' from ', I0, ': ', I0)") my_rank, idmn, numrecvarr(idmn)
      
      jrecv(idmn+1) = jrecv(idmn)+numrecvarr(idmn)
      
      if ( numrecvarr(idmn).gt.0 ) then
         numrecvtot = jrecv(idmn+1)-1
         if ( N.gt.ubound(workrecv,1) .or. numrecvtot.gt.ubound(workrecv,1) ) then
!           reallocate      
            numnew = 1+int(1.2d0*dble(numrecvtot))
            call realloc(workrecv,(/N,numnew/),keepExisting=.true.)
         end if
         call mpi_recv(workrecv(1,jrecv(idmn)),N*numrecvarr(idmn),MPI_DOUBLE_PRECISION,idmn,itag2,DFM_COMM_DFMWORLD,istat,ierror)
!         nrequest = nrequest+1
!         call mpi_irecv(workrecv(1,jrecv(idmn)),N*numrecvarr(idmn),MPI_DOUBLE_PRECISION,idmn,itag2,DFM_COMM_DFMWORLD,irequest(nrequest),ierror)
         
      end if
   end do
   
!  terminate send (safety)
   do i=1,nrequest
      call mpi_wait(irequest(i),istat,ierror)
   end do
#else
   jrecv = 1
#endif

   return
end subroutine sendrecv_particledata


!> send/receive particles to/at subdomain 0
subroutine reduce_particles()
   use m_particles
   use m_partparallel
   use m_partitioninfo, only: jampi, my_rank, ndomains
   use m_alloc
   implicit none
   
   integer :: i, ipart
   integer :: numsend, numrecv
   
   integer :: ierror
   
!   return
   
   if ( japart.eq.0 .or. jampi.eq.0 ) return
   
!  count number of particles to be sent
   numsend = 0
   do ipart=1,Npart
      if ( kpart(ipart).gt.0 ) then
         numsend = numsend+1
      end if
   end do
   
!  make send list
   jsend(0)=1
   jsend(1:ndomains) = 1+numsend
   call realloc(isend,numsend,keepExisting=.false.,fill=0)
   i=0
   do ipart=1,Npart
      if ( kpart(ipart).gt.0 ) then
         i=i+1
         isend(i) = ipart
      end if
   end do
   
!  fill send array
   call part2send(jsend,isend)
   
!  send/receive data
   call sendrecv_particledata(NDIM,jsend,jrecv)
   
   if ( my_rank.eq.0 ) then
!     add particles (original data stored in worksnd)
      call recv2part(jrecv(ndomains)-1,workrecv)
   end if
   
   japartsaved = 1
   
   return
end subroutine reduce_particles

!> restore particles in subdomain 0 after reduce
subroutine restore_particles()
   use m_particles
   use m_partparallel
   use m_partitioninfo
   implicit none
   
   integer :: i
   
!   return
   
   if ( japart.eq.0 .or. jampi.eq.0 ) return
   
   if ( my_rank.eq.0 .and. japartsaved.eq.1 ) then
!     cleanup large arrays
      call dealloc_particles()
!     restore particles from send array
      call recv2part(jsend(ndomains)-1,worksnd)
   end if
      
   japartsaved = 0
   
   return
end subroutine restore_particles
