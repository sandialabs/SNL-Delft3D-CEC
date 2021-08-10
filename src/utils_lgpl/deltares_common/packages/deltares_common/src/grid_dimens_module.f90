module grid_dimens_module
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2020.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!  $Id: grid_dimens_module.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_lgpl/deltares_common/packages/deltares_common/src/grid_dimens_module.f90 $
!!--module description----------------------------------------------------------
!
! This module defines the data structure for grid dimensions.
!
!!--module declarations---------------------------------------------------------
use precision
private

!
! public data types
!
public griddimtype

!
! public routines
!
public simplegrid_dimens

!
! collection of grid dimension properties
!
type griddimtype
    !
    !   Local (n,m) indices:                        Local linear nm indices:
    !    ______________________________________      ______________________________________ 
    ! ^ |                             (nub,mub)|    |                                  nmub|
    ! | |______________________________________|    |______________________________________|
    ! M |                                      |    |                                 nmmax|
    !   |     __________________________ _     |    |     __________________________ _     |
    !   |    |/////////////////(nmax,mmax)|    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |__________________________|_|    |    |    |__________________________|_|    |
    !   |    (1,1)                             |    |                                      |
    !   |______________________________________|    |______________________________________|
    !   |                                      |    |1                                     |
    !   |______________________________________|    |______________________________________|
    !   (nlb,mlb)                          N->       nmlb
    !
    !   Global (n,m) indices:                 
    !    ______________________________________________ 
    !   |                               (nmaxgl,mmaxgl)|
    !   |                                              |
    !   |                                              |
    !   |          __________________________          |
    !   |         |/////////////////(nlg,mlg)|         |
    !   |         |//////////////////////////|         |
    !   |         |//////////////////////////|         |
    !   |         |//////////////////////////|         |
    !   |         |//////////////////////////|         |
    !   |         |__________________________|         |
    !   |         (nfg,mfg)                            |
    !   |                                              |
    !   |                                              |
    !   |______________________________________________|
    !   (1,1)
    !
    !
    !   Local (n,m) indices Delft3D-FLOW:           Local linear nm indices Delft3D-FLOW:
    !    ______________________________________      ______________________________________ 
    !   |                 (nmax+ddb,mmax+2+ddb)|    |           (nmax+2*ddb)*(mmax+2+2*ddb)|
    !   |______________________________________|    |______________________________________|
    !   |                   (nmax+ddb,mmax+ddb)|    |             (nmax+2*ddb)*(mmax+2*ddb)|
    !   |     __________________________ _     |    |     __________________________ _     |
    !   |    |/////////////(nmaxus,mmax)|*|    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |//////////////////////////| |    |    |    |//////////////////////////| |    |
    !   |    |__________________________|_|    |    |    |__________________________|_|    |
    !   |    (1,1)                             |    |                                      |
    !   |______________________________________|    |______________________________________|
    !   |(1-ddb,1-ddb)                         |    |1                                     |
    !   |______________________________________|    |______________________________________|
    !   (1-ddb,-1-ddb)           * = (nmax,mmax)    1-2*(nmax+2*ddb)
    !
    !
    ! The hashed area is the area of the grid assigned to the current thread.
    ! In a sequential simulation this corresponds to precisely the grid as
    ! defined by the user; in a parallel simulation it is a part thereof.
    ! The size of this grid area is nmaxus x mmax, where nmaxus and mmax are
    ! either the grid dimensions known to the user (sequential case) or those
    ! corresponding to the partition given to this thread by the master
    ! including any halo cells (parallel run). All hashed areas in local and
    ! global index spaces sketched above correspond.
    !
    ! The local grid dimensions may be bigger than the hashed area e.g. due to
    ! halo cells for domain decomposition (DD boundaries) or for numerical
    ! reasons e.g. the red-black Jacobi implementation requires nmax to be odd.
    !
    ! local m range
    !
    integer :: mlb    ! lower bound on m index in local array dimension
    integer :: mub    ! upper bound on m index in local array dimension
    integer :: mmax   ! active local m index range runs from 1 to mmax
    !
    ! Local n range
    !
    integer :: nlb    ! lower bound on n index in local array dimension
    integer :: nub    ! upper bound on n index in local array dimension
    integer :: nmax   ! active local n index range runs from 1 to nmaxus (nmax=nmaxus+1 if nmax is even)
    !
    ! Local nm linear index range
    !
    integer :: nmlb   ! lower bound on local linear nm index
    integer :: nmub   ! upper bound on local linear nm index
    integer :: nmmax  ! active local nm indices are subset of 1 to nmmax
    !
    ! Global m range
    !
    integer :: mfg    ! global m index corresponding to local m index 1
    integer :: mlg    ! global m index corresponding to local m index mmax
    integer :: mmaxgl ! global maximum m index as known to user
    !
    ! Global n range
    !
    integer :: nfg    ! global n index corresponding to local n index 1
    integer :: nlg    ! global n index corresponding to local n index nmaxus
    integer :: nmaxgl ! global maximum n index as known to user
    !
    integer, dimension(:,:), pointer :: aggrtable => null() ! aggrtable(i,j) = 0 no cell, nm = cell index (>0)
    integer, dimension(:)  , pointer :: celltype => null()  ! 0 = inactive, 1 = active (internal), 2 = boundary, -1 = ghost
    integer, dimension(:,:), pointer :: nmbnd => null()     ! (1,nb) = nm boundary, (2,nb) = nm internal
    !
    real(fp), dimension(:), pointer :: xz => null() ! X-coord. of the water elevation pnt.
    real(fp), dimension(:), pointer :: yz => null() ! Y-coord. of the water elevation pnt.
end type griddimtype

contains

subroutine simplegrid_dimens(griddim,nmax,mmax,aggrtable)
!!--description-----------------------------------------------------------------
!
! Initializes a griddim structure for a simple (1,nmax) and (1,mmax) grid
!
!!--declarations----------------------------------------------------------------
    implicit none
!
! Call variables
!
    type (griddimtype)        :: griddim
    integer                   :: nmax
    integer                   :: mmax
    integer, dimension(:,:), pointer, optional :: aggrtable
!
! Local variables
!
    integer                   :: istat
!
!! executable statements -------------------------------------------------------
!
    griddim%nlb    = 1
    griddim%nub    = nmax
    griddim%nmax   = nmax
    !
    griddim%mlb    = 1
    griddim%mub    = mmax
    griddim%mmax   = mmax
    !
    griddim%nmlb   = 1
    griddim%nmub   = nmax*mmax
    griddim%nmmax  = nmax*mmax
    !
    griddim%nfg    = 1
    griddim%nlg    = nmax
    griddim%nmaxgl = nmax
    !
    griddim%mfg    = 1
    griddim%mlg    = mmax
    griddim%mmaxgl = mmax
    !
    if (present(aggrtable)) then
       griddim%aggrtable => aggrtable
    else
       griddim%aggrtable => null()
    endif
    !
    allocate(griddim%celltype(griddim%nmmax), stat=istat)
    if (istat==0) griddim%celltype = 1
    griddim%nmbnd => null()
end subroutine simplegrid_dimens

end module grid_dimens_module
