subroutine dfupdi_nm_pos2 ( field, ks, ke, gdp )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2015.                                
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
!  $Id: dfupdi_nm_pos2.F90 5518 2015-10-23 14:14:36Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/dfupdi_nm_pos2.F90 $
!!--description-----------------------------------------------------------------
!
!   Updates field array of type integer through exchanging halo values
!   between neighbouring subdomains
!
!!--pseudo code and references--------------------------------------------------
!
!   for all neighbouring subdomains do
!      get subdomain number, pointer and size
!      store data to be sent in array WORK
!      send array WORK
!
!   for all neighbouring subdomains do
!      get subdomain number, pointer and size
!      receive next array and store in WORK
!      store the received data
!
!
!!--declarations----------------------------------------------------------------
    use dfparall
    use globaldata
    !
    implicit none
    !
    type(globdat), target    :: gdp
    include 'fsm.i'
    include 'tri-dyn.igd'
    integer(pntrsize), pointer :: kcs
!
! Global variables
!
    integer, intent(in)                                            :: ke    ! last index in vertical direction
    integer, intent(in)                                            :: ks    ! first index in vertical direction
!
    integer, dimension(ks:ke,gdp%d%nmlb:gdp%d%nmub), intent(inout) :: field ! real array for which halo values must be copied from neighbouring subdomains
!
! Local variables
!
    integer, dimension(:), pointer         :: iblkad
    integer                                :: idom          ! subdomain number
    integer                                :: inb           ! neighbour counter
    integer                                :: istart        ! pointer in array IBLKAD
    integer                                :: itag          ! message tag for sending and receiving
    integer                                :: j             ! loop counter
    integer                                :: k             ! loop counter in vertical direction
    integer                                :: ksiz          ! size in vertical direction (e.g. total number of sigma layers)
    integer                                :: nneigh        ! number of neighbouring subdomains
    integer                                :: novlu         ! number of overlapping unknowns
    integer                                :: worksize
    integer                                :: request(4,2)
    integer, dimension(:,:,:), allocatable :: work          ! work array to store data to be sent to or received from neighbour
!
!! executable statements -------------------------------------------------------
!
    kcs    => gdp%gdr_i_ch%kcs
    iblkad => gdp%gdparall%iblkad
    !
    ksiz = ke - ks + 1
    !
    nneigh = iblkad(1)
    !
    worksize = ksiz*max(ihalom,ihalon)*max(gdp%d%mmax,gdp%d%nmax)
    allocate(work(worksize, 4, 2))
    !
    ! for all neighbouring subdomains do
    !
    itag = 2
    call dfsendi_nm_pos2 ( field, work, worksize, ks, ke, request, itag, gdp )
    call dfwaiti_nm_pos2 ( field, work, worksize, ks, ke, request, itag, i(kcs), gdp )
    !
    deallocate(work)
end subroutine dfupdi_nm_pos2
