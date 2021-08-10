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

! $Id: advec.f90 65778 2020-01-14 14:07:42Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/advec.f90 $

!> initialize matrix for first-order upwind discretization of advection
!>   Cartesian cell-centered vector components are numbered as
!>      ( ucx_1, ucy_1, ucx_2, ucy_2, ..., ...)
   subroutine inisolver_advec(ierror)
      use m_flowgeom
      use m_alloc
      use m_solver
      use m_advec
      implicit none
      
      integer,       intent(inout) :: ierror
      
      integer                                     :: irow, irowother, icolumn, ipoint
      integer                                     :: kk, kkb, kkother, k1, k2, LL
      integer                                     :: i, ishift
      integer                                     :: numnonzeros
      
      ierror = 1
      
!     construct interpolation matrix in CRS format
      numnonzeros = 4*Lnx
      call realloc(iI, Lnx+1,       keepExisting=.false., fill=0)
      call realloc(jI, numnonzeros, keepExisting=.false., fill=0)
      call realloc(aI, numnonzeros, keepExisting=.false., fill=0d0)
      
      ipoint = 0
      irow  = 0
      do LL=1,Lnx
         irow = irow+1
         iI(irow)   = ipoint+1
         
         k1 = ln(1,LL)
         k2 = ln(2,LL)
         
!        x-component first flownode         
         ipoint = ipoint+1
         jI(ipoint) = (k1-1)*2 + 1
         aI(ipoint) = acL(LL)*csu(LL)
         
!        y-component first flownode
         ipoint = ipoint+1
         jI(ipoint) = (k1-1)*2 + 2
         aI(ipoint) = acL(LL)*snu(LL)
         
!        x-component second flownode         
         ipoint = ipoint+1
         jI(ipoint) = (k2-1)*2 + 1
         aI(ipoint) = (1d0-acL(LL))*csu(LL)
         
!        y-component second flownode
         ipoint = ipoint+1
         jI(ipoint) = (k2-1)*2 + 2
         aI(ipoint) = (1d0-acL(LL))*snu(LL)
      end do
      iI(irow+1) = ipoint+1
      
!     construct reconstruction matrix in CRS format
      
!     count number of non-zeros
      numnonzeros = 0
      do kk=1,Ndxi
         numnonzeros = numnonzeros + 2*nd(kk)%lnx
      end do
      do kkb=Ndxi+1,Ndx  ! boundaries: apply Neumann conditions (copy from inside)
         LL = iabs(nd(kkb)%ln(1))   ! only one flowlink connected to boundary flownode
         kk = ln(1,LL) + ln(2,LL) - kkb
         numnonzeros = numnonzeros + 2*nd(kk)%lnx
      end do
      
!     allocate
      call realloc(iR, 2*Ndx+1,     keepExisting=.false., fill=0)
      call realloc(jR, numnonzeros, keepExisting=.false., fill=0)
      call realloc(aR, numnonzeros, keepExisting=.false., fill=0d0)
      
!     fill entries
      ipoint = 0
      irow   = 0
      do kkb=1,Ndx
         if ( kkb.le.Ndxi ) then
            kk = kkb
         else  ! boundaries: apply homogeneous Neumann condition (copy from inside)
            LL = iabs(nd(kkb)%ln(1))   ! only one flowlink connected to boundary flownode
            kk = ln(1,LL) + ln(2,LL) - kkb
         end if
      
!        x-component      
         irow = irow+1
         iR(irow) = ipoint+1
         
!        shift between x-component and y-component
         ishift = nd(kk)%lnx
         
!        y-component (next row)
         iR(irow+1) = iR(irow)+ishift
         
         do i=1,nd(kk)%lnx
            LL = iabs(nd(kk)%ln(i))
            
!           x-component
            ipoint = ipoint+1      
            jR(ipoint) = LL
            
!           y-component (next row)
            jR(ipoint+ishift) = LL
            
            if ( ln(1,LL).eq.kk ) then
               aR(ipoint)        = wcx1(LL)            
               aR(ipoint+ishift) = wcy1(LL)
            else
               aR(ipoint)        = wcx2(LL)
               aR(ipoint+ishift) = wcy2(LL)
            end if
         end do
           
!        shift to y-component
         ipoint = ipoint+ishift
         irow   = irow+1
      end do
      
      iR(irow+1) = ipoint+1
      
!     construct collocated discretization matrix
      
!     count number of non-zeros: use reconstruction matrix
      numnonzeros = 2*Ndx + (iR(2*Ndx+1)-1)
      
!     allocate
      call realloc(iC,        2*Ndx+1,     keepExisting=.false., fill=0)
      call realloc(jC,        numnonzeros, keepExisting=.false., fill=0)
      call realloc(aC,        numnonzeros, keepExisting=.false., fill=0d0)
      call realloc(dfluxfac, (/ 2, Lnx /), keepExisting=.false., fill=0d0)
      
!     fill entries
      ipoint = 0
      irow   = 0
      do kk=1,Ndxi
         irow     = irow+1
         iC(irow) = ipoint+1
         
!        shift between x-component and y-component
         ishift = 1+nd(kk)%lnx
         
!        y-component (next row)
         iC(irow+1) = iC(irow)+ishift
         
!        diagonal entry x-component
         ipoint     = ipoint+1
         icolumn    = irow
         jC(ipoint) = icolumn
         ac(ipoint) = 1d0  ! some non-zero dummy value
         
!        diagonal entry y-component
         jc(ipoint+ishift) = irow+1
         ac(ipoint+ishift) = 2d0  ! some non-zero dummy value
         
!        off-diagonals
         do i=1,nd(kk)%lnx
            ipoint     = ipoint+1
            LL         = iabs(nd(kk)%ln(i))
            kkother    = ln(1,LL)+ln(2,LL)-kk
            icolumn    = (kkother-1)*2 + 1
            
!           x-component            
            jc(ipoint) = icolumn
            ac(ipoint) = 3d0  ! some non-zero dummy value
            
!           y-component
            jc(ipoint+ishift) = icolumn+1
            ac(ipoint+ishift) = 4d0  ! some non-zero dummy value
         end do
         
!        shift to y-component
         ipoint = ipoint+ishift
         irow   = irow+1
      end do
      
!     boundaries: apply homogeneous Neumann condition (copy from inside)
      do kk=Ndxi+1,Ndx
         irow = irow+1
         ic(irow) = ipoint+1
         LL = iabs(nd(kk)%ln(1)) ! only one flowlink attached to boundary flownode
         kkother = ln(1,LL) + ln(2,LL) - kk  ! ln(2,LL)
         
!        x-component
         irowother = (kkother-1)*2+1
         do i=iC(irowother),iC(irowother+1)-1
            ipoint = ipoint+1
            jC(ipoint) = jC(i)
         end do
         
!        y-component
         irow = irow+1
         ic(irow) = ipoint+1
         irowother = (kkother-1)*2+2
         do i=iC(irowother),iC(irowother+1)-1
            ipoint = ipoint+1
            jC(ipoint) = jC(i)
         end do
      end do
      
      ic(irow+1) = ipoint+1
    
!     compute sparsity pattern of work matrix W = C R
      
!     allocate work array
      call realloc(iwork, 2*Ndx, keepExisting=.false., fill=0)
      
!     estimate number of non zeros of W=CR
     ! call amub_countonly(2*Ndx,Lnx,aC,jC,iC,aR,jR,iR,iwork,numnonzeros)
      
!     allocate    
      call realloc(iW, 2*Ndx, keepExisting=.false., fill=0)
      call realloc(jW, numnonzeros, keepExisting=.false., fill=0)
      call realloc(aW, numnonzeros, keepExisting=.false., fill=0d0)
      
!     get sparsity pattern
      call amub (2*Ndx,Lnx,0,aC,jC,iC,aR,jR,iR,aW,jW,iW,numnonzeros,iwork,ierror)
      if ( ierror.ne.0 ) goto 1234
      
      
!     compute sparsity pattern of whole matrix A = I W (= I C R)
      
!     allocate work array
      call realloc(iwork, 2*Ndx, keepExisting=.false., fill=0)
      
!     estimate number of non zeros of W=CR
      ! call amub_countonly(Lnx,2*Ndx,aI,jI,iI,aW,jW,iW,iwork,numnonzeros)
     
!     settings
      call SolverSettings(solver_advec, Lnx, numnonzeros)
      
!     allocate
      ! call allocSolver(solver_advec, ierror)
      
!     get sparsity pattern
      call amub (Lnx,2*Ndx,0,aI,jI,iI,aW,jW,iW,solver_advec%a,solver_advec%ja,solver_advec%ia,solver_advec%numnonzeros,iwork,ierror)
      if ( ierror.ne.0 ) goto 1234
      
      if ( jaoutput.eq.1 ) then
         call writematrix('Imat.m', Lnx,   iI, jI, aI, 'I', 0)
         call writematrix('Rmat.m', 2*Ndx, iR, jR, aR, 'R', 0)
         call writematrix('Cmat.m', 2*Ndx, iC, jC, aC, 'C', 0)
         solver_advec%a = 1d0
         call writematrix('Amat.m', Lnx, solver_advec%ia, solver_advec%ja, solver_advec%a, 'A', 0)
      end if
      
      ierror = 0
 1234 continue
      
      return
   end subroutine inisolver_advec
   
   
!> default solver settings
   subroutine SolverSettings(solver, numrows, numnonzeros)
      use m_solver
      implicit none
      
      type(tsolver), intent(inout) :: solver       !< solver
      integer,       intent(in)    :: numrows      !< number of rows
      integer,       intent(in)    :: numnonzeros  !< number of nonzero elements
      
      solver%numrows            = numrows
      solver%numnonzeros        = numnonzeros
      solver%numnonzerosprecond = 120*numrows
      solver%nwork              = 2*solver%numnonzerosprecond
      
         !!   ipar(1) = 0               ! initialized in "itaux"
      solver%ipar(2) = 1               ! no (0), left (1), right (2), both (3) precond
      solver%ipar(3) = 1               ! stopping criteria
      solver%ipar(4) = solver%nwork    ! number of elems in array 'wk'
      solver%ipar(5) = 10              ! size of Krylov subspace in GMRES and variants
      solver%ipar(6) = 1000            ! max number of mat-vec multiplies
      
      solver%fpar(1) = 0.0D-16         ! relative tolerance ('exact' solve, except
      solver%fpar(2) = 1.0d-14         ! absolute tolerance
      
      solver%lfil  = 3
      solver%alpha = 1d0
      solver%tol   = 0.50D-2
      
      solver%jabcgstab = 1
   end subroutine SolverSettings
   
   
!> construct matrix for first-order upwind discretization of advection
   subroutine fillsystem_advec(ierror)
      use m_flowgeom
      use m_flow, only : vol1, qa, Au
      use m_advec
      use m_alloc
      implicit none
      
      integer,                        intent(inout) :: ierror
      
!      double precision, dimension(:,:), allocatable :: dfluxfac
      
      double precision                              :: dfac
      double precision                              :: ux, uy
      
      integer                                       :: i, irow, irowother, idir, icolumn, ishift
      integer                                       :: ipoint, ipointdiag
      integer                                       :: k1, k2, kk, kkother, LL
      
      ierror = 1
      
!      call realloc(dfluxfac, (/ 2, Lnx /), keepExisting=.false., fill=0d0)
      
      ux = 1d0
      uy = 0.1d0
      
      do LL=1,Lnx
         k1 = ln(1,LL)
         k2 = ln(2,LL)
!         dfac = qa(LL)
         dfac = (csu(LL)*ux + snu(LL)*uy) * Au(LL)
         dfluxfac(1,LL) = max(dfac,0d0)
         dfluxfac(2,LL) = min(dfac,0d0)
      end do
      
!     initialize matrix entries
      aC = 0d0
      
      ipoint = 0
      irow   = 0
      do kk=1,Ndxi
!        x-component      
         irow = irow+1
         
!        shift between x-component and y-component
         ishift = 1+nd(kk)%lnx
         
!        diagonal entry x-component
         ipoint = ipoint+1
         ipointdiag = ipoint
         
            
!        check numbering
         if ( jC(ipointdiag).ne.irow ) then
            call qnerror('fillsystem_advec: numbering error', ' ', ' ')
            goto 1234
         end if
         
         if ( jC(ipointdiag+ishift).ne.irow+1 ) then
            call qnerror('fillsystem_advec: numbering error', ' ', ' ')
            goto 1234
         end if
         
!        off-diagonals         
         do i=1,nd(kk)%lnx
            ipoint = ipoint+1
            LL = nd(kk)%ln(i)
            
            idir = 0 ! to kk
            if ( LL.lt.0 ) then
               idir = 1 ! from kk
               LL = -LL
            end if
            
            
!           check numbering
            if ( jC(ipoint).ne.(ln(1,LL)+ln(2,LL)-kk-1)*2+1 ) then
               call qnerror('fillsystem_advec: numbering error', ' ', ' ')
               goto 1234
            end if
            
            if ( jC(ipoint+ishift).ne.(ln(1,LL)+ln(2,LL)-kk-1)*2+2 ) then
               call qnerror('fillsystem_advec: numbering error', ' ', ' ')
            end if
            
!           x-component
            aC(ipoint)     = aC(ipoint)     + (idir*dfluxfac(2,LL) - (1-idir)*dfluxfac(1,LL))/vol1(kk)
            ac(ipointdiag) = aC(ipointdiag) + (idir*dfluxfac(1,LL) - (1-idir)*dfluxfac(2,LL))/vol1(kk)
            
!           y-component
            aC(ipoint    +ishift) = aC(ipoint    +ishift) + (idir*dfluxfac(2,LL) - (1-idir)*dfluxfac(1,LL))/vol1(kk)
            ac(ipointdiag+ishift) = aC(ipointdiag+ishift) + (idir*dfluxfac(1,LL) - (1-idir)*dfluxfac(2,LL))/vol1(kk)
         end do
         
!        shift to y-component
         ipoint = ipoint+ishift
         irow   = irow+1
      end do
      
!     boundaries: apply homogeneous Neumann condition (copy from inside)
      do kk=Ndxi+1,Ndx
         irow = irow+1
         LL = iabs(nd(kk)%ln(1)) ! only one flowlink attached to boundary flownode
         kkother = ln(1,LL) + ln(2,LL) - kk  ! ln(2,LL)
         
!        x-component
         irowother = (kkother-1)*2+1
         do i=iC(irowother),iC(irowother+1)-1
            ipoint = ipoint+1
            aC(ipoint) = aC(i)
         end do
         
!        y-component
         irow = irow+1
         irowother = (kkother-1)*2+2
         do i=iC(irowother),iC(irowother+1)-1
            ipoint = ipoint+1
            aC(ipoint) = aC(i)
         end do
      end do
      
      if ( jaoutput.eq.1 ) then
         call writematrix('C.m', 2*Ndx, iC, jC, aC, 'A_C', 0)
      end if
      
      ierror = 0
 1234 continue
 
!      if ( allocated(dfluxfac) ) deallocate(dfluxfac)
      
      return
   end subroutine fillsystem_advec
   
   
   !> output matrix in CRS format to file
   subroutine writeMatrix(FNAM, N, ia, ja, a, VARNAM, jaappend)
      implicit none
      
      character(len=*),                       intent(in) :: FNAM         !< filename
                                              
      integer,                                intent(in) :: N            !< number of rows
                                              
      integer,          dimension(N+1),       intent(in) :: ia
      integer,          dimension(ia(N+1)-1), intent(in) :: ja
      double precision, dimension(ia(N+1)-1), intent(in) :: a            !< matrix in CRS format

      character(len=*),                       intent(in) :: VARNAM       !< variable name
      
      integer,                                intent(in) :: jaappend     !< append (1), or not (0)
      
      logical                                            :: Lexist
                                              
      integer                                            :: irow, icol, j
      
      inquire(file=trim(FNAM), exist=Lexist)
      if ( jaappend.eq.0 .or. .not.Lexist ) then
         open(1234,file=trim(FNAM))
      else
         open(1234,file=trim(FNAM), access="append")
      end if
      
      
      write(1234,"('dum = [')")
      do irow=1,N
         do j=ia(irow),ia(irow+1)-1
            icol = ja(j)
            write(1234,"(2I7,E20.10)") irow, icol, a(j)
         end do
      end do
      write(1234,"('];')")
      write(1234,"(A16, '=sparse(dum(:,1), dum(:,2), dum(:,3));')") VARNAM
      
!     write rhs
!      write(1234,"('rhs = [')")
!      do irow=1,solver%numrows
!         write(1234,"(E15.5)") solver%rhs(irow)
!      end do
!      write(1234,"('];')")
      
      close(1234)
   end subroutine writeMatrix