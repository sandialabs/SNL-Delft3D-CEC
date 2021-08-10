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

! $Id: partition.F90 65778 2020-01-14 14:07:42Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/partition.F90 $
   
!------------------------------------------------------------------------
!  THOUGHTS:
!    Given that
!      -domain and ghostlevel numbering is in network administration, and
!      -handshaking is in flow administration,
!    the parallelization seems to rely on an identity mapping of internal
!    flownodes to netcells.
!    Note that the boundary flownodes are always in the own subdomain.
!
!------------------------------------------------------------------------

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif 

!> the parameters and enumeraters are taken from
!>   ../third_party_open/metis-<version>/include/metis.h
module m_metis
   integer, parameter :: METIS_NOPTIONS=40
   
   integer, dimension(METIS_NOPTIONS) :: opts
end module

module m_partitioninfo

use m_tpoly
! use dfm_error   ! removed it, the if's are not safe, they rely on 0 or 1

#ifdef HAVE_MPI
   use mpi, only: NAMECLASH_MPI_COMM_WORLD => MPI_COMM_WORLD ! Apparently PETSc causes a name clash, see commit #28532.
#endif

   implicit none

#ifdef HAVE_MPI
   !> The MPI communicator for dflowfm. Default: MPI_COMM_WORLD.
   !! May be changed from outside in a coupled/library run, *before* initialize().
   !! In case of C/C++ calling side, construct an MPI communicator, and call
   !! MPI_Fint MPI_Comm_c2f(MPI_Comm comm) to convert the C comm handle
   !! to a FORTRAN comm handle.
   integer, target :: DFM_COMM_DFMWORLD = NAMECLASH_MPI_COMM_WORLD !< [-] The MPI communicator for dflowfm (FORTRAN handle). {"rank": 0}
#endif
   type tghost
      integer, dimension(:), allocatable   :: list               !< list of ghost nodes or links, in order of their corresponding other domain, dim(1:number of ghost nodes/links)
      integer, dimension(:), allocatable   :: N                  !< cumulative number of ghost nodes/links per domain in the list, starts with fictitious domain 0, dim(0:numdomains)
      integer, dimension(:), allocatable   :: neighdmn           !< list of neighboring domains, dim(1:numneighdmn)
      integer                              :: num                !< number of ghost nodes or links
      integer                              :: numdomains         !< number of domains
      integer                              :: numneighdmn        !< number of neighboring domains
   end type
   
   integer                                 :: ndomains = 0       !< number of domains
   integer                                 :: numranks = 1       !< number of ranks
   integer                                 :: my_rank            !< own rank
   
   character(len=4)                        :: sdmn               !< domain number string

   type(tpoly), allocatable                :: partition_pol(:)   !< polygons that define the partition
   integer                                 :: npartition_pol     !< number of partition polygons

   integer, dimension(:), allocatable      :: idomain            !< cell-based domain number, dim(nump1d2d or ndx)
   integer, dimension(:), allocatable      :: idomain0           !< backup of idomain 
   integer, dimension(:), allocatable      :: numndx             !< number of cells in a domain
   integer, dimension(:), allocatable      :: ighostlev          !< ghost-cell level, combination of node-based and cell-based
   integer, dimension(:), allocatable      :: ighostlev_nodebased !< ghost-cell level, neighboring cells connected through netnodes
   integer, dimension(:), allocatable      :: ighostlev_cellbased !< ghost-cell level, neighboring cells connected through netlinks

   integer, parameter                      :: ITYPE_S      = 0   !< water-level communication identifier, for Poisson equation (water level), first ghost level only
   integer, parameter                      :: ITYPE_U      = 1   !< flow link communication identifier
   integer, parameter                      :: ITYPE_Sall   = 2   !< water-level communcation identifier, all ghost levels
   integer, parameter                      :: ITYPE_S3D    = 3   !< 3D water-level communication identifier, for Poisson equation (water level), first ghost level only
   integer, parameter                      :: ITYPE_U3D    = 4   !< 3D flow link communication identifier
   integer, parameter                      :: ITYPE_Sall3D = 5   !< 3D water-level communcation identifier, all ghost levels
   integer, parameter                      :: ITYPE_SallTheta = 6 !< water-level communcation identifier, all ghost levels, theta-grid (for XBeach waves)
   integer, parameter                      :: ITYPE_Snonoverlap = 7  !< non-overlappling ghost nodes (for solver)
   integer, parameter                      :: ITYPE_U3DW        = 8  !< 3D flow link communication identifier, starting at 0, for interfaces

   
   integer, parameter                      :: IGHOSTTYPE_CELLBASED = 0  !< cell-based ghostlevels
   integer, parameter                      :: IGHOSTTYPE_NODEBASED = 1  !< node-based ghostlevels
   integer, parameter                      :: IGHOSTTYPE_COMBINED  = 2  !< combined ghostlevels
   
!  the non-parameter variables that are initialized with 0 are set in "partition_setghost_params"
   integer                                 :: numlay_cellbased=0           !< number of cell-based ghost-cell layers
   integer                                 :: numlay_nodebased=0           !< number of node-based ghost-cell layers
   
   integer                                 :: minghostlev_s=0    !< minimum ghost-cell layer level of water-level nodes, used for overlap in solver
   integer                                 :: maxghostlev_s=0    !< maximum ghost-cell layer level of water-level nodes, used for overlap in solver
   integer                                 :: ighosttype_s=IGHOSTTYPE_CELLBASED
   
   integer                                 :: minghostlev_u=0    !< minimum ghost-cell layer level of links
   integer                                 :: maxghostlev_u=0    !< maximum ghost-cell layer level of links
   integer, parameter                      :: ighosttype_u=IGHOSTTYPE_COMBINED
   
   integer                                 :: minghostlev_sall=0 !< minimum ghost-cell layer level of water-level nodes, all ghost levels
   integer                                 :: maxghostlev_sall=0 !< maximum ghost-cell layer level of water-level nodes, all ghost levels
   integer, parameter                      :: ighosttype_sall=IGHOSTTYPE_COMBINED
   
   integer, parameter                      :: itag_s=1           !< communication tag
   integer, parameter                      :: itag_u=2           !< communication tag
   integer, parameter                      :: itag_sall=3        !< communication tag
   integer, parameter                      :: itag_snonoverlap=4 !< communication tag
   
   integer                                    :: numghost_s         !< number of water-level ghost nodes
   integer, dimension(:), allocatable, target :: ighostlist_s       !< list of water-level ghost nodes, in order of their corresponding domain
   integer, dimension(:), allocatable, target :: nghostlist_s       !< pointer to last s-node of a certain ghost domain in the ighostlist_s array, first domain first, etc., includes fictitious domain '0' (0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}
   
   integer                                    :: numghost_u         !< number of ghost links
   integer, dimension(:), allocatable, target :: ighostlist_u       !< list of ghost links, in order of their corresponding domain
   integer, dimension(:), allocatable, target :: nghostlist_u       !< pointer to last link of a certain ghost domain in the ighostlist_u array, first domain first, etc., includes fictitious domain '0' (0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}
   
   integer                                    :: numghost_sall      !< number of water-level ghost nodes
   integer, dimension(:), allocatable, target :: ighostlist_sall    !< list of water-level ghost nodes, in order of their corresponding domain
   integer, dimension(:), allocatable, target :: nghostlist_sall    !< pointer to last s-node of a certain ghost domain in the ighostlist_s array, first domain first, etc., includes fictitious domain '0' (0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}
   
   integer                                    :: numghost_snonoverlap      !< number of water-level ghost nodes
   integer, dimension(:), allocatable, target :: ighostlist_snonoverlap    !< list of water-level ghost nodes, in order of their corresponding domain
   integer, dimension(:), allocatable, target :: nghostlist_snonoverlap    !< pointer to last s-node of a certain ghost domain in the ighostlist_s array, first domain first, etc., includes fictitious domain '0' (0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}

   integer                                    :: numsend_s          !< number of water-level send nodes
   integer, dimension(:), allocatable, target :: isendlist_s        !< list of water-level internal nodes to be sent to other domains
   integer, dimension(:), allocatable, target :: nsendlist_s        !< pointer to last s-node of a certain other domain in the isendlist_s array, first domain first, etc., includes fictitious domain '0' (0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}
   
   integer                                    :: numsend_u          !< number of send links
   integer, dimension(:), allocatable, target :: isendlist_u        !< list of internal links to be sent to other domains
   integer, dimension(:), allocatable, target :: nsendlist_u        !< pointer to last link of a certain other domain in the isendlist_u array, first domain first, etc., includes fictitious domain '0' ('0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}
   
   integer                                    :: numsend_sall       !< number of water level send nodes, all ghostlevels.
   integer, dimension(:), allocatable, target :: isendlist_sall     !< list of water level internal nodes to be sent to other domains, all ghostlevels.
   integer, dimension(:), allocatable, target :: nsendlist_sall     !< pointer to last s-node of a certain other domain in the isendlist_sall array, first domain first, etc., includes fictitious domain '0' ('0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}
   
   integer                                    :: numsend_snonoverlap       !< number of water level send nodes, all non-overlapping ghostlevels (for solver).
   integer, dimension(:), allocatable, target :: isendlist_snonoverlap     !< list of water level internal nodes to be sent to other domains, all non-overlapping ghostlevels (for solver).
   integer, dimension(:), allocatable, target :: nsendlist_snonoverlap     !< pointer to last s-node of a certain other domain in the isendlist_snonoverlap array, first domain first, etc., includes fictitious domain '0' ('0, n1, n1+n2, n1+n1+n3, ...) {"shape": ["-1:numdomains-1"]}
   
   integer, dimension(:), allocatable         :: iglobal            !< [-] unique flow node numbers for matrix solver, does not exactly correspond with the original unpartitioned cell/flow node numbers! (duplicate boundary cells cause some addtional cell number increments.)
   integer, dimension(:), allocatable, target :: iglobal_s          !< [-] global flow node numbers to help output aggregation later. Should exactly correspond with the original unpartitioned flow node numbers! (as opposed to iglobal) {"shape": ["ndx"]}
   integer                                    :: Nglobal_s          !< total number of global net cells, equals unpartitioned nump1d2d, Ndxi
   integer, dimension(:), allocatable         :: numcells           !< number of active cells per domain, dim(0:ndomains-1)
   
   double precision, dimension(:), allocatable :: work, workrec  !< work array
   
   double precision, dimension(:,:), allocatable :: workmatbd ! for overlap (solver): matrix (bbr,ddr)
   double precision, dimension(:,:), allocatable :: workmatc  ! for overlap (solver): matrix (ccr)


   integer, dimension(:), allocatable      :: nghostlist_s_3D
   integer, dimension(:), allocatable      :: nsendlist_s_3D
   
   integer, dimension(:), allocatable      :: nghostlist_sall_3D
   integer, dimension(:), allocatable      :: nsendlist_sall_3D
   
   integer, dimension(:), allocatable      :: nghostlist_u_3D
   integer, dimension(:), allocatable      :: nsendlist_u_3D
   
   integer, dimension(:), allocatable      :: nghostlist_u_3Dw
   integer, dimension(:), allocatable      :: nsendlist_u_3Dw
   
   integer, dimension(:), allocatable      :: nghostlist_sall_theta
   integer, dimension(:), allocatable      :: nsendlist_sall_theta
   
   integer                                 :: jaoverlap  !< overlap in solver (1) or not (0)
   !integer                                 :: noverlap   !< number of overlapping nodes (in solver)
   !integer, dimension(:), allocatable      :: ioverlap   !< overlapping nodes (in solver)

#ifdef HAVE_MPI
   integer                                 :: JAMPI = 1          !< use MPI (1) or not (0)
   integer                                 :: ja_mpi_init_by_fm  !< MPI_Init called by FM (1) or not (0). Not/0 in case FM runs in library mode and an outside runner program has taken care of MPI_Init.
#else
   integer                                 :: JAMPI = 0          !< use MPI (1) or not (0)
#endif

   double precision, dimension(:), allocatable :: reducebuf    !< work array for mpi-reduce
   integer                                     :: nreducebuf   !< size of work array 'reducebuf'

!! we need interfaces to getkbotktop and getLbotLtop for the function pointers
! interface
!    subroutine getkbotktop(n,kb,kt)
!      integer :: n, kb, kt
!    end subroutine getkbotktop
! end interface
! 
! interface
!    subroutine getLbotLtop(n,Lb,Lt)
!      integer :: n, Lb, Lt
!    end subroutine getLbotLtop
! end interface
!
!   abstract interface
!!>    get bottom and top layer indices (pointer to getkbotktop or getLbotLtop)
!      subroutine p_getbottop(n,kb,kt)
!         integer :: n   !< flow-node or link
!         integer :: kb  !< index of bottom layer
!         integer :: kt  !< index of top layer
!      end subroutine
!   end interface
   
!   for test solver:  Schwarz method with Robin-Robin coupling
   integer                                       :: nbndint   ! number of interface links
   integer,          dimension(:,:), allocatable :: kbndint   ! interface administration, similar to kbndz, etc., dim(3,nbndint)
   double precision, dimension(:,:), allocatable :: zbndint   ! (1,:): beta value, (2,:): interface value, dim(2,nbndint)
   double precision :: stoptol =1d-4  ! parameter of stopping criteria for subsolver of Schwarz method
   double precision :: sbeta = 10d0 ! beta value in Robin-Robin coupling for Schwarz iterations
   double precision :: prectol = 0.50D-2 ! tolerance for drop of preconditioner
   integer          :: jabicgstab = 1 ! 
   integer          :: Nsubiters  = 1000
   contains

!> generate partition numbering from partition polygons and
!! determine number of partitions.
!!
!! A cell/flow node is assigned to the last partition for which it lies
!! inside that partition's polygon.
!!
!! A single partition may be defined by multiple polygons if they have
!! the same z-value in their first polygon point.
!! Therefore ndomains <= num polygons+1
!!
!! For a single partition, polygons may also overlap (partially or fully),
!! to allow 'holes' inside the outer region or even 'islands' inside 'holes'.
   subroutine partition_pol_to_idomain(janet,jafindcells)
      
      use network_data
      use m_flowgeom, only: Ndx, Ndxi, xz, yz
      use m_missing
      use m_alloc
      use gridoperations

      implicit none
      
      integer, intent(in)                :: janet  !< for network (1) or flow geom (0) or add 1D net (2)
      integer, intent(in), optional      :: jafindcells !< findcells when applicable (1) or not (0)
      
      integer, dimension(:), allocatable :: idomain_prev
      integer, dimension(:), allocatable :: idum
      
      double precision                   :: zval

      integer                            :: i, idmn, ipol, in
      integer                            :: ierror, numcells
      integer                            :: istart, iend
      
      logical                            :: Lfindcells

      ierror = 1
      
!     deallocate module arrays
      if ( allocated(numndx)  ) deallocate(numndx)

      Lfindcells = .true.
      if ( present(jafindcells) ) then
         Lfindcells = jafindcells==1
      end if
      
      if ( Lfindcells ) then
         if ( janet.eq.1 ) then
            call findcells(0)
            call find1dcells()
         else if ( janet.eq.2 ) then   ! add 1D net
            call find1dcells()
         end if
         
         call delete_dry_points_and_areas()
      end if
      
!     determine number of cells
      if ( janet.eq.1 .or. janet.eq.2 ) then
         numcells = nump1d2d  ! netcells
         if ( janet.eq.1 ) then
            istart = 1
         else
            istart = nump+1
         end if
         iend = numcells
      else
         numcells = Ndx !flownodes
         istart   = 1
         iend     = numcells
      end if

!     allocate
      if ( janet.eq.0  .or. janet.eq.1 ) then
         call realloc(idomain, numcells, keepExisting=.false., fill=0) ! default domain number
      else
         call realloc(idomain, numcells, keepExisting=.true., fill=0)
      end if
      
      allocate(idomain_prev(numcells))
      idomain_prev = idomain
      allocate(idum(npartition_pol))

!     assign domain numbers to the polygons (if not assigned already)
!       by setting the first node z-value and determine number of partitions

!     make all domain numbers available
      idum = (/ (i, i=1,npartition_pol) /)
      
!     see wich domain numbers are already used
      do ipol=1,npartition_pol
         if ( partition_pol(ipol)%len.gt.0 ) then ! non-empty polygons only
!           see if this polygon already has a domain number assigned
            zval = partition_pol(ipol)%z(1)
            if ( zval.ne.DMISS ) then
               idmn = int(zval)
               if ( idmn.lt.0 .or. idmn.gt.npartition_pol ) then
                  call qnerror('partition_pol_to_idomain: numbering error', ' ', ' ')
               else
                  idum(idmn) = -1   ! deactivated
               end if
            end if
         end if
      end do
      
!     assign unused (active) domain numbers to remaining polygons and
!       determine number of domains
      ndomains=1
      do ipol=1,npartition_pol
         if ( partition_pol(ipol)%len.gt.0 ) then
            zval = partition_pol(ipol)%z(1)
            if ( zval.eq.DMISS ) then
               idmn = minval(idum, mask=idum.gt.0) ! smallest of active domain numbers
               partition_pol(ipol)%z(1)=idmn
               idum(idmn) = -1   ! deactivated
            else
               idmn = int(zval)
            end if
            ndomains = max(ndomains,idmn+1)
         end if
      end do
      
!     in future, maybe check for unused domain numbers and shift      

!     determine domain number
      do idmn=1,ndomains
         do ipol=1,npartition_pol
            if ( partition_pol(ipol)%len.le.0 ) cycle ! non-zero polygons only
            if ( int(partition_pol(ipol)%z(1)).ne.idmn ) cycle ! polygon belonging to this domain only
            
            in = -1
            do i=istart,iend
               
               if ( i.le.nump1d2d ) then
                  call dbpinpol_tpoly(partition_pol(ipol), xzw(i), yzw(i), in)
!                 SPvdP: (xzw,yzw) are not safe for spherical, periodic coordinates, use (xz, yz) instead
!                  call dbpinpol_tpoly(partition_pol(ipol), xz(i), yz(i), in)
               else  ! fictitious boundary cells: use xz, yz
                  call dbpinpol_tpoly(partition_pol(ipol), xz(i), yz(i), in)
               end if
               if ( in.eq.1 ) then
!                 check if this cell is already inside the domain
                  if ( idomain(i).eq.idmn ) then
!                    outside this domain again
                     idomain(i) = idomain_prev(i)
                  else
!                    inside this domain (again)
                     idomain_prev(i) = idomain(i)
                     idomain(i) = idmn
                  end if
                  
                  
                  !if ( i.eq.115 .and. my_rank.eq.0 ) then
                  !   write(6,*) '-->', i, idomain(i), ipol
                  !end if
                  
               end if
               
            end do
         end do   ! do ipol=1,npartition_pol
      end do   ! do idmn=0,ndomains
      
      if ( janet.eq.0 ) then  ! flow mode
!        get fictitious boundary cells in own domain
         do i=Ndxi+1,Ndx
            idomain(i) = my_rank
         end do
      end if

      ierror = 0
1234  continue
      
!     deallocate
      if ( allocated(idomain_prev) ) deallocate(idomain_prev)
      if ( allocated(idum)         ) deallocate(idum)

      return
   end subroutine partition_pol_to_idomain


!> generate partition domain-numbers from selecting polygons
   subroutine generate_partitioning_from_pol()
      use m_polygon
      use network_data

      implicit none

      integer :: L
      
!     copy polygons to partition polygons
      call pol_to_tpoly(npartition_pol, partition_pol)

!     save/delete selecting polygon (partitioning of whole net)
      call savepol()
      call delpol()

!     generate partition domain-numbers from partition polygons
      call partition_pol_to_idomain(1) ! for net

!     restore selecting polygon
      call restorepol()

      return
   end subroutine generate_partitioning_from_pol
   
!> set ghostlevel parameters   
   subroutine partition_setghost_params(icgsolver)
      implicit none
      
      integer, intent(in) :: icgsolver !< solver type
      
!     set overlap for Schwarz solver, if uninitialized (0)
      if ( icgsolver.eq.9 .or. icgsolver.gt.90 ) then
         numlay_cellbased = 4
         numlay_nodebased = 3
         ighosttype_s = IGHOSTTYPE_NODEBASED
      
         if ( icgsolver.gt.90 ) then
            minghostlev_s = icgsolver-90
            maxghostlev_s = minghostlev_s
            
!           for Robin-Robin interface conditions
            ighosttype_s = IGHOSTTYPE_NODEBASED
            minghostlev_s = max(minghostlev_s - 1,1)
            
!            numlay_cellbased = max(numlay_cellbased,maxghostlev_s)           
            numlay_nodebased = max(numlay_nodebased,maxghostlev_s)
         else
            minghostlev_s = numlay_cellbased
            maxghostlev_s = numlay_cellbased
         end if
         
         minghostlev_sall = 1
         maxghostlev_sall = max(numlay_cellbased,numlay_nodebased)+1
         
         minghostlev_u = 1
         maxghostlev_u = max(numlay_cellbased,numlay_nodebased)+1
      else
         numlay_cellbased = 4
         numlay_nodebased = 3
         
         minghostlev_s = 1
         maxghostlev_s = 1
         
         minghostlev_sall = 1
         maxghostlev_sall = 5
         
         minghostlev_u = 1
         maxghostlev_u = 5
      end if
      
      return
   end subroutine partition_setghost_params


!> initialize partitioning
   subroutine partition_init_1D2D(md_ident, ierror)
!      use unstruc_model, only: md_ident  !<---- circular dependency
      use network_data, only : xzw, yzw, nump1d2d
      use m_flowgeom,   only : xu, yu, Ndx, Ndxi
!      use m_flow,       only : kmx
      use m_flowparameters, only: icgsolver
      use m_polygon
      use m_missing
      use m_alloc
      
      implicit none

      character(len=*),           intent(in)  :: md_ident
      integer,                    intent(out) :: ierror

      integer                                 :: idmn
!      integer                                 ::numlaymax      
      character(len=128)                      :: mesg
      integer                                 :: i
      ierror = 1
      
      call partition_setghost_params(icgsolver)
      
!     the following subroutine will determine the number of domains and generate the domain numbering
     if ( npartition_pol.lt.1 ) then ! If do not use polygon (the domain numbers have been read from the partition nc file)
!        get fictitious boundary cells in own domain
         if (Ndx > nump1d2d) then
            call realloc(idomain, Ndx, keepExisting=.true.)
            do i=Ndxi+1,Ndx
               idomain(i) = my_rank
            end do
         endif
     else
        call partition_pol_to_idomain(0) ! for flow geom.
     endif
     
!     check the number of ranks
      if ( ndomains.ne.numranks .and. jampi.eq.1 ) then
         write(mesg, "('partition_init: numdomains = ', I0, ' <> numranks = ', I0)") ndomains, numranks 
         call qnerror(trim(mesg), ' ', ' ')
         jampi = 0
         goto 1234
      end if

      if ( ndomains.le.1 ) then  ! only one domain
         jampi = 0   ! turn off parallel computing
         ierror = 0
         goto 1234
      end if

      idmn = my_rank
      
!      numlaymax = numlay+1 ! add additional level for enclosed cells and flowlinks

      call partition_set_ghostlevels(idmn, numlay_cellbased+1, numlay_nodebased+1, 1, ierror)
      if ( ierror /= 0 ) goto 1234

!     make 2D ghost- and sendlists
      call partition_make_ghostlists(idmn, ierror)
      if ( ierror /= 0 ) goto 1234
   
      call partition_make_sendlists(idmn, md_ident, ierror)
      if ( ierror /= 0 ) goto 1234
            
!     flow links: check and fix orientation of send list
      call partition_fixorientation_ghostlist(ierror)
      if ( ierror /= 0 ) goto 1234
      
!      if ( kmx.gt.0 ) then
!!        make 3D ghost- and sendlists
!         call partition_make_ghostsendlists_3d(ierror)
!         if ( ierror /= 0 ) goto 1234
!      end if

!      call partition_cleanup(0,ierror)
!      if ( ierror.ne.0 ) goto 1234

!     begin DEBUG
!         call write_ghosts(trim(md_ident)//'_gst.pli')
!     end DEBUG

!     determine overlapping nodes in solver
!      call partition_getoverlappingnodes()
      
!     make non-overlapping ghost- and sendlists (for solver)      
      call partition_fill_ghostsendlist_nonoverlap(ierror)
      if ( ierror.ne.0 ) goto 1234

      call partition_make_globalnumbers(ierror)
      if ( ierror /= 0 ) goto 1234

      ierror = 0
 1234 continue

      return
   end subroutine partition_init_1D2D
   
    
   subroutine partition_init_3D(ierror)
      implicit none
      
      integer, intent(out) :: ierror
      
!     make 3D ghost- and sendlists
      call partition_make_ghostsendlists_3d(ierror)
      if ( ierror /= 0 ) goto 1234
      
 1234 continue
      return
   end subroutine partition_init_3D


!> make a domain, including the ghostcells, by removing the other parts of the network
!>   based on combined ghostlevels
   subroutine partition_make_domain(idmn, numlay_cell, numlay_node, jacells, ierror)
      use MessageHandling
      use dfm_error
      use m_polygon
      use m_missing, only: dmiss, intmiss
      use network_data
      use m_alloc
      use gridoperations

      implicit none

      integer,                 intent(in)  :: idmn   !< domain number
      integer,                 intent(in)  :: numlay_cell !< number of cell-based ghostcell layers
      integer,                 intent(in)  :: numlay_node !< number of node-based ghostcell layers
      integer,                 intent(in)  :: jacells     !< generate partition domain and cell numbers
      integer,                 intent(out) :: ierror

      integer                              :: ic1, ic2, k1, k2, L
      integer                              :: L1, L2, JA, K3, LL, c1, c2, c3, c4, L_old, i, n
      integer, dimension(:,:), allocatable :: lne_org(:,:)
      integer                              :: nLink2Dhang  ! number of hanging 2D links found
      
      
      integer              :: i_old
      
      character(len=128)   :: message

      ierror = DFM_GENERICERROR

!     set the ghostcells level in module variables ighostlev, ighostlev_cellbased and ighostlev_nodebased
      call partition_set_ghostlevels(idmn, numlay_cell, numlay_node, 0, ierror)
      if ( ierror.ne.0 ) goto 1234

!     delete other part of network, by using the node mask (network_data variable kc)
!      if ( allocated(kc) ) deallocate(kc)
!      allocate(kc(numk))
!      kc = 0

!     make link mask
      Lc = 0
      nLink2Dhang = 0
      do L=1,numL
!        check for hanging 2D links (not supported)      
         if ( kn(3,L).eq.2 .and. lnn(L).eq.0 ) then
!            call qnerror('Hanging 2D links not supported', ' ', ' ')
!            call teklink(L,NCOLWARN1)
!            ierror=1
!            goto 1234

             Lc(L) = 0  ! deactive link
             nLink2Dhang = nLink2dhang+1
             cycle
         end if
         
         ic1 = iabs(lne(1,L))
         ic2 = iabs(lne(min(lnn(L),2),L))
         if ( kn(3,L).eq.2 ) then   ! 2D links
            if ( idomain(ic1).eq.idmn .or. ighostlev(ic1).ne.0 .or.  &
                 idomain(ic2).eq.idmn .or. ighostlev(ic2).ne.0 ) then
               !kc(kn(1,L)) = 1
               !kc(kn(2,L)) = 1
               Lc(L) = 1
            end if
         else if ( kn(3,L).ne.0 ) then    ! kn(3,L).eq.1 .or. kn(3,L).eq.3 .or. kn(3,L).eq.4 ) then ! 1D link
!           need to check connected netcells, since the netnode may not be associated with a 1D netcell (1D-2D coupling)
            k1 = kn(1,L)
            k2 = kn(2,L)
            if ( (idomain(ic1).eq.idmn .or. ighostlev(ic1).ne.0 ).and.   &
                 (idomain(ic2).eq.idmn .or. ighostlev(ic2).ne.0 )) then ! active 1D cell
               !kc(k1) = 1
               !kc(k2) = 1
               Lc(L) = 1
            end if
         end if
      end do

!!     deactivate nodes
!      do k=1,numk
!         if ( kc(k).ne.1 ) then
!            call delnode(k)
!         end if
!      end do

!     deactive links
      do L=1,numL
         if ( Lc(L).eq.0 ) then
            kn(1,L) = 0
            kn(2,L) = 0
            kn(3,L) = 0
         end if
      end do
      
      if ( jacells.eq.1 ) then
         Nglobal_s = nump1d2d
         
!        save lne
         allocate(lne_org(2, numL))
         lne_org = 0
         do i = 1, numL
            lne_org(1,i) = abs(lne(1,i))
            lne_org(2,i) = abs(lne(2,i))
         enddo
      endif      

!     physically remove nodes and links from network
      NPL = 0           ! number of polygon points, we set no polygon
      call findcells(100000)  ! output link permutation array "Lperm" (only used if jacells.eq.1)
      call find1dcells()
   
      call delete_dry_points_and_areas()
  
      if (numk == 0 .or. numl == 0) then
         write(message,"('While making partition domain #', I0, ': empty domain (', I0, ' net nodes, ', I0, ' net links).')") idmn, numk, numl
         call mess(LEVEL_WARN, trim(message))
         ierror = DFM_GENERICERROR
         goto 1234
      end if
         
!     output number of hanging 2D links
      if ( idmn.eq.0 ) then
         if ( nLink2Dhang.gt.0 ) then
            write(message,"(I0, ' hanging 2D links disregarded.')") nLink2Dhang
            call mess(LEVEL_WARN, trim(message))
         end if
      end if

      if ( jacells.eq.1 ) then
!        allocate cellmask array (used to flag illegal cells)
         call realloc(cellmask, nump1d2d, keepExisting=.false., fill=0)
         
!        (re)allocate global cell numbers array
         call realloc(iglobal_s, nump1d2d, keepExisting=.false., fill=0)
         
!        find original cell numbers
         call find_original_cell_numbers(Lperm, Lne_org, iglobal_s)
      
      !  generate idomain for the current subdomain so that idomain can be written to the partition file
         do i = 1, nump1d2d 
            i_old = iglobal_s(i)
            
            if ( i_old.gt.0 ) then
               idomain(i) = idomain0(i_old)
               iglobal_s(i) = i_old
            else
!              cell was created by partitioning, "level-five" cell, mark it for removal
               netcell(i)%N = 0
               idomain(i) = -999 ! not necessary, safety
               iglobal_s(i) = -999 ! not necessary, safety
!              mask illegal cell
               cellmask(i) = 1
            end if
         enddo
         
!        remove masked netcells
         call remove_masked_netcells()
      endif
      ierror = DFM_NOERR
 1234 continue

      if ( allocated(lne_org)  ) deallocate(lne_org)

      return
   end subroutine partition_make_domain
   
   
!> find original cell numbers
   subroutine find_original_cell_numbers(L2Lorg, Lne_org, iorg)
      use unstruc_messages
      use network_data, only: nump1d2d, numL, lnn, lne
      implicit none
      
      integer,  intent(in)  :: L2Lorg(:)   !< original Link numbers
      integer,  intent(in)  :: Lne_org(:,:)  !< original Lne
      integer,  intent(out) :: iorg(:)     !< original cell numbers

      integer, dimension(:,:),      allocatable :: icandidate  ! two original cell number candidates, dim(nump1d2d)
      
      integer                                   :: i, k, kother, L, L_org
      integer                                   :: k1, k2
      integer                                   :: ic1, ic2, ic3, ic4
      integer                                   :: nump1d2d_org
     
!     get original number of cells
      nump1d2d_org = 0
      do L=1,numL
         L_org = L2Lorg(L)
         ic3 = iabs(lne_org(1,L_org))
         ic4 = iabs(lne_org(2,L_org))
         nump1d2d_org = max(max( nump1d2d_org, ic3), ic4)
      end do
      
      allocate(icandidate(2,nump1d2d_org))
      
      icandidate = -1
      do L=1,numL
         L_org = L2Lorg(L)
         ic3 = iabs(lne_org(1,L_org))  ! first new candidate
         ic4 = iabs(lne_org(2,L_org))  ! second new candiate, can be 0 if lnn_org==1
         do i=1,2
            k = iabs(lne(i,L))     ! this cell number
            if ( k.eq.0 ) cycle
            ic1 = icandidate(1,k)  ! first stored candidate
            ic2 = icandidate(2,k)  ! second stored candidate
            
            if ( ic1.eq.-1 .and. ic2.eq.-1 ) then  ! initialization: accept both new candidates
               icandidate(1,k) = ic3
               icandidate(2,k) = ic4
            else   ! compare two new candidates (ic3,ic4) with the two stored ones (ic1,ic2)
               if ( ic3.ne.ic1 .and. ic4.ne.ic1 ) then  ! compare with c1
                  ! it's not ic1
                  icandidate(1,k) = 0
               end if
               
               if ( ic3.ne.ic2 .and. ic4.ne.ic2 ) then
                  ! it's not ic2
                  icandidate(2,k) = 0
               end if
            end if
         end do
      end do
      
!     retrieve original cell numbers, 0 if no cell found
      iorg = 0
      do k=1,nump1d2d
         if ( icandidate(1,k).gt.0 .and. icandidate(2,k).gt.0 .and. icandidate(1,k).ne.icandidate(2,k) ) then
!           cell is endnode on a branch
         else if ( icandidate(1,k).gt.0 )then
            iorg(k) = icandidate(1,k)
         else if ( icandidate(2,k).gt.0 ) then
            iorg(k) = icandidate(2,k)
         end if
      end do
      
!     set cells connected through one link/edge      
      do L=1,numL
         do i=1,2
            k = iabs(lne(i,L))     ! this cell number
            kother = iabs(lne(3-i,L))  ! other cell number
            
            if ( k.eq.0 .or. kother.eq.0 ) cycle
            
            if ( iorg(k).eq.0 .and. iorg(kother).gt.0 ) then   ! unassigned original cell number
               ic1 = icandidate(1,k)
               ic2 = icandidate(2,k)
               if ( ic1.eq.iorg(kother) ) then
                  iorg(k) = ic2
               else if ( ic2.eq.iorg(kother) ) then
                  iorg(k) = ic1
               end if
            end if
            
         end do
      end do
      
!!     check if all orginal cell number are assigned
!      do k=1,nump1d2d
!         if ( iorg(k).eq.0 ) then
!            continue
!         end if
!      end do
      
!     deallocate
      if ( allocated(icandidate) ) deallocate(icandidate)
      
      return
   end subroutine find_original_cell_numbers
   
   
!>  combine cell-based ghostlevels with node-based ghostlevels
!>    note: ghostlevel should never be put to 1 by node-based ghostlevel (otherwise "s"-ghostlist becomes unnecessary large)
!>          therefore use max(ighostlev_nodebased(ic),2) if ighostlev_nodebased(ic).gt.0
!>
!>    update: node-based ghostlevel may now be 1 (did not do it yet), since we have introduced ghostlevel types
!             and "s"-type ghosts are cell-based only
   subroutine partition_set_ghostlevels(idmn, numlay_cell, numlay_node, jaboundary, ierror)
      use network_data, only: nump1d2d
      use unstruc_messages
      implicit none
      
      integer, intent(in)  :: idmn          !< domain number
      integer, intent(in)  :: numlay_cell   !< number of cell-based ghost-cell layers
      integer, intent(in)  :: numlay_node   !< number of node-based ghost-cell layers
      integer, intent(in)  :: jaboundary    !< include boundary flownodes (1) or not (0)
      integer, intent(out) :: ierror        !< error (1) or not (0)
      
      integer :: numcells, num, ic
      
      ierror = 1
      
!     determine number of cells from idomain
      numcells = size(idomain)

      if ( allocated(ighostlev) ) deallocate(ighostlev)
      allocate(ighostlev(numcells))  ! make ighostlev same size as idomain
      
!     compute node-based ghostlevels
      call partition_set_ghostlevels_cellbased(idmn, numlay_cell, ierror)
      
      if ( ierror.ne.0 ) goto 1234
      
!     compute node-based ghostlevels
      call partition_set_ghostlevels_nodebased(idmn, numlay_node, ierror)
      if ( ierror.ne.0 ) goto 1234
      
      num = 0
      
      ighostlev = ighostlev_cellbased
      
      do ic=1,nump1d2d
!         if ( ighostlev_nodebased(ic).gt.0 .and. ighostlev_nodebased(ic).le.MAX_NODEBASED_GHOSTLEVEL ) then
         if ( ighostlev_nodebased(ic).gt.0 ) then
            if ( ighostlev_cellbased(ic).eq.0 ) then
               num = num+1
               ighostlev(ic) = max(ighostlev_nodebased(ic),2)
            else
               ighostlev(ic) = min(max(ighostlev_nodebased(ic),2),ighostlev_cellbased(ic))
            end if
         end if
      end do
      
      call mess(LEVEL_INFO, 'added node-based ghostcells:', num)
      
!     set ghostlevels in boundaries (if applicable)
      if ( jaboundary.eq.1 ) then
         call partition_set_ghostlevels_boundaries()
      end if
      
      ierror = 0
 1234 continue
 
      if ( ierror.ne.0 ) then
         call mess(LEVEL_ERROR, 'partition_set_ghostlevels: error')
      end if
      
      return
   end subroutine partition_set_ghostlevels


!> find the ghost cells for a certain domain, given a cell-based domain numbering idomain
!>    it is assumed that module variable idomain has been filled
!>    ghost cells are masked in module variable ighostlev_cellbased
   subroutine partition_set_ghostlevels_cellbased(idmn, numlay_loc, ierror)
      use network_data
      implicit none

      integer, intent(in)  :: idmn         !< domain number
      integer, intent(in)  :: numlay_loc   !< number of ghost-cell layers
      integer, intent(out) :: ierror       !< error (1) or not (0)

      integer :: ilay, ic, icother, L, n, numcells

      ierror = 1
      
!     determine number of cells from idomain
      numcells = size(idomain)

      if ( allocated(ighostlev_cellbased) ) deallocate(ighostlev_cellbased)
      allocate(ighostlev_cellbased(numcells))  ! make ighostlev_cellbased same size as idomain

      ighostlev_cellbased = 0

      do ilay=1,numlay_loc                ! loop over the number of layers
         do L=1,numL                      ! loop over the netlinks
            if ( lnn(L).lt.2 ) cycle      ! 1d-links: relies on proper lnn (set by find1dcells)
            do n=1,2                      ! loop over the cells attached to this link
               ic = iabs(lne(n,L))
               if ( idomain(ic).eq.idmn .or. ( ilay.gt.1 .and. ighostlev_cellbased(ic).eq.ilay-1 ) ) then
!                activate inactive other cell
                 icother = iabs(lne(1,L))+iabs(lne(2,L))-ic
                 if ( idomain(icother).ne.idmn .and. ighostlev_cellbased(icother).eq.0 ) then ! other cell not active
                    ighostlev_cellbased(icother) = ilay   ! set ighostlev_cellbased to the layer number
                 end if
               end if
            end do
         end do   ! do L=1,numL
      end do   ! do ilay=1,numlay_loc
   

      ierror = 0
 1234 continue

      return
   end subroutine partition_set_ghostlevels_cellbased
   
   

!> find the node-based ghost levels (e.g. for horizonal momentum diffusion) for a certain domain, given a cell-based domain numbering idomain
!>    it is assumed that module variable idomain has been filled
!>    ghost cells are masked in module variable ighostlev_nodebased
   subroutine partition_set_ghostlevels_nodebased(idmn, numlay_loc, ierror)
      use network_data
      implicit none
      
      integer, intent(in)  :: idmn         !< domain number
      integer, intent(in)  :: numlay_loc   !< number of ghost-cell layers
      integer, intent(out) :: ierror       !< error (1) or not (0)
      
      integer              :: ilay, ic, icother, i, ip1, kk, k, L, Lp1, numcells
      
      integer, external    :: icommon
      
      ierror = 1
      
!     determine number of cells from idomain
      numcells = size(idomain)

      if ( allocated(ighostlev_nodebased) ) deallocate(ighostlev_nodebased)
      allocate(ighostlev_nodebased(numcells))  ! make ighostlev_nodebased same size as idomain
      
      ighostlev_nodebased = 0
      
      do ilay=1,numlay_loc
         do ic=1,nump
            if ( idomain(ic).eq.idmn .or. ( ilay.gt.1 .and. ighostlev_nodebased(ic).eq.ilay-1 ) ) then
               do kk=1,netcell(ic)%N
                  k = netcell(ic)%nod(kk)
                  do i=1,nmk(k)
                     ip1 = i+1; if ( ip1.gt.nmk(k) ) ip1=ip1-nmk(k)
                     L = nod(k)%lin(i)
                     Lp1 = nod(k)%lin(ip1)
                     icother = icommon(L,Lp1)
                     
                     if ( icother.eq.0 ) cycle  ! boundary links
                     
                     if ( idomain(icother).ne.idmn .and. ighostlev_nodebased(icother).eq.0 ) then ! other cell not active
                       ighostlev_nodebased(icother) = ilay   ! set ighostlev to the layer number
                     end if
                  end do
               end do
            end if
         end do
      end do
      
      ierror = 0
 1234 continue 
 
      return
   end subroutine partition_set_ghostlevels_nodebased
   
   
!> set ghostlevels in boundary flownodes (copy from inner nodes)
   subroutine partition_set_ghostlevels_boundaries()
      use network_data
      use m_flowgeom
      implicit none
      
      integer :: kb, ki, L
      integer :: ierror
      
      ierror = 1
      
!      if ( Ndx.gt.nump ) then
         do L=Lnxi+1,Lnx
            kb = ln(1,L)
            ki = ln(2,L)
            ighostlev_cellbased(kb) = ighostlev_cellbased(ki)
            ighostlev_nodebased(kb) = ighostlev_nodebased(ki)
            ighostlev(kb) = ighostlev(ki)
         end do
!      end if
      
      ierror = 0
 1234 continue 
      
      return
   end subroutine partition_set_ghostlevels_boundaries


!> make the lists of ghost-water-levels and velocities
!>    it is assumed that idomain and ighostlev are filled
   subroutine partition_make_ghostlists(idmn, ierror)
      use m_alloc
      use m_flowgeom

      implicit none

      integer, intent(in)  :: idmn     !< domain number
      integer, intent(out) :: ierror   !< error (1) or not (0)

      type(tghost), dimension(:), allocatable   :: sghost      !< tghost-type ghostlist at flownodes
      type(tghost), dimension(:), allocatable   :: sallghost   !< tghost-type ghostlist at flownodes
      type(tghost), dimension(:), allocatable   :: ughost      !< tghost-type ghostlist at velocties

      integer              :: i, j, num, nums, numu, numsall
      integer              :: iglev

      ierror = 1

!     get ghosts for all ghost levels at both flownodes and links
      call partition_get_ghosts(idmn, ITYPE_S, sghost, ierror)
      call partition_get_ghosts(idmn, ITYPE_Sall, sallghost, ierror)
      call partition_get_ghosts(idmn, ITYPE_U, ughost, ierror)

!     count number of waterlevel ghostnodes
      nums = 0
      do iglev=minghostlev_s,maxghostlev_s
         if ( iglev.lt.lbound(sghost,1) .or. iglev.gt.ubound(sghost,1) ) cycle   ! should not happen
         do i=0,sghost(iglev)%numdomains-1
            do j=sghost(iglev)%N(i-1)+1,sghost(iglev)%N(i)
               nums = nums+1
            end do
         end do
      end do
      
      
!     count number of all waterlevel ghostnodes
      numsall = 0
      do iglev=minghostlev_sall,maxghostlev_sall
         if ( iglev.lt.lbound(sallghost,1) .or. iglev.gt.ubound(sallghost,1) ) cycle   ! should not happen
         do i=0,sallghost(iglev)%numdomains-1
            do j=sallghost(iglev)%N(i-1)+1,sallghost(iglev)%N(i)
               numsall = numsall+1
            end do
         end do
      end do
      
!     count number of velocity ghostlinks
      numu = 0
      do iglev=minghostlev_u,maxghostlev_u
         if ( iglev.lt.lbound(ughost,1) .or. iglev.gt.ubound(ughost,1) ) cycle   ! should not happen
         do i=0,ughost(iglev)%numdomains-1
            do j=ughost(iglev)%N(i-1)+1,ughost(iglev)%N(i)
               numu = numu+1
            end do
         end do
      end do

!     deallocate
      if ( allocated(ighostlist_s)) deallocate(ighostlist_s)
      if ( allocated(nghostlist_s)) deallocate(nghostlist_s)
      
!     allocate
      allocate(ighostlist_s(nums))
      allocate(nghostlist_s(-1:ndomains-1))
      numghost_s   = nums
      ighostlist_s = 0
      nghostlist_s = 0

!     deallocate
      if ( allocated(ighostlist_sall)) deallocate(ighostlist_sall)
      if ( allocated(nghostlist_sall)) deallocate(nghostlist_sall)
      
!     allocate
      allocate(ighostlist_sall(numsall))
      allocate(nghostlist_sall(-1:ndomains-1))
      numghost_sall   = numsall
      ighostlist_sall = 0
      nghostlist_sall = 0
      
!     deallocate
      if ( allocated(ighostlist_u)) deallocate(ighostlist_u)
      if ( allocated(nghostlist_u)) deallocate(nghostlist_u)
      
!     allocate
      allocate(ighostlist_u(numu))
      allocate(nghostlist_u(-1:ndomains-1))
      numghost_u   = numu
      ighostlist_u = 0
      nghostlist_u = 0

!     fill waterlevel ghostlist
      num = 0
      do i=0,ndomains-1
         do iglev=minghostlev_s,maxghostlev_s
            if ( iglev.lt.lbound(sghost,1) .or. iglev.gt.ubound(sghost,1) ) cycle
            if ( i.gt.sghost(iglev)%numdomains-1 ) cycle
            do j=sghost(iglev)%N(i-1)+1,sghost(iglev)%N(i)
               num = num+1
               ighostlist_s(num) = sghost(iglev)%list(j)
               nghostlist_s(i:ndomains-1) = nghostlist_s(i:ndomains-1)+1
            end do
         end do
      end do

!     fill all waterlevel ghostlist
      num = 0
      do i=0,ndomains-1
         do iglev=minghostlev_sall,maxghostlev_sall
            if ( iglev.lt.lbound(sallghost,1) .or. iglev.gt.ubound(sallghost,1) ) cycle
            if ( i.gt.sallghost(iglev)%numdomains-1 ) cycle
            do j=sallghost(iglev)%N(i-1)+1,sallghost(iglev)%N(i)
               num = num+1
               ighostlist_sall(num) = sallghost(iglev)%list(j)
               nghostlist_sall(i:ndomains-1) = nghostlist_sall(i:ndomains-1)+1
            end do
         end do
      end do

!     fill velocity ghostlist
      num = 0
      do i=0,ndomains-1
         do iglev=minghostlev_u,maxghostlev_u
            if ( iglev.lt.lbound(ughost,1) .or. iglev.gt.ubound(ughost,1) ) cycle
            if ( i.gt.ughost(iglev)%numdomains-1 ) cycle
            do j=ughost(iglev)%N(i-1)+1,ughost(iglev)%N(i)
               num = num+1
               ighostlist_u(num) = ughost(iglev)%list(j)
               nghostlist_u(i:ndomains-1) = nghostlist_u(i:ndomains-1)+1
            end do
         end do
      end do

      ierror = 0
 1234 continue

 !    deallocate
      if ( allocated(sghost)    ) call dealloc_tghost(sghost)
      if ( allocated(sallghost) ) call dealloc_tghost(sallghost)
      if ( allocated(ughost)    ) call dealloc_tghost(ughost)
      
      return
   end subroutine partition_make_ghostlists


!> deallocate partitioning arrays
   recursive subroutine partition_cleanup(iwhat, ierror)
      implicit none

      integer, intent(in)  :: iwhat    !< 0: all, 1: cell-based only, 2: polygon only, 3: s-ghostlists, 4: u-ghostlists, 5: s-sendlist, 6: u-sendlist, 7: 3D lists
      integer, intent(out) :: ierror   !< error (1) or not (0)

      integer              :: i, ierror_

      ierror = 0

      if ( iwhat.eq.0 ) then
         ierror_ = 0
         i = 0
         do while ( ierror_.eq.0 )
            i = i+1
            call partition_cleanup(i, ierror_)
         end do
      else if ( iwhat.eq.1 ) then
         if ( allocated(idomain) )   deallocate(idomain)
         if ( allocated(ighostlev) ) deallocate(ighostlev)
         if ( allocated(ighostlev_cellbased) ) deallocate(ighostlev_cellbased)
         if ( allocated(ighostlev_nodebased) ) deallocate(ighostlev_nodebased)
      else if ( iwhat.eq.2 ) then
         if ( allocated(partition_pol) ) then
            call dealloc_tpoly(partition_pol)
            npartition_pol = 0
         end if
      else if ( iwhat.eq.3 ) then
!         call dealloc_tghost(sghost)
         if ( allocated(ighostlist_s) ) deallocate(ighostlist_s)
         if ( allocated(nghostlist_s) ) deallocate(nghostlist_s)
      else if ( iwhat.eq.4 ) then
!         call dealloc_tghost(ughost)
         if ( allocated(ighostlist_u) ) deallocate(ighostlist_u)
         if ( allocated(nghostlist_u) ) deallocate(nghostlist_u)
      else if ( iwhat.eq.5 ) then
!         call dealloc_tghost(sghost)
         if ( allocated(isendlist_s) ) deallocate(isendlist_s)
         if ( allocated(nsendlist_s) ) deallocate(nsendlist_s)
      else if ( iwhat.eq.6 ) then
!         call dealloc_tghost(ughost)
         if ( allocated(isendlist_u) ) deallocate(isendlist_u)
         if ( allocated(nsendlist_u) ) deallocate(nsendlist_u)
      else if ( iwhat.eq.7 ) then
         if ( allocated(nghostlist_s_3d) ) deallocate(nghostlist_s_3d)
         if ( allocated(nsendlist_s_3d)  ) deallocate(nsendlist_s_3d)
         
         if ( allocated(nghostlist_sall_3d) ) deallocate(nghostlist_sall_3d)
         if ( allocated(nsendlist_sall_3d)  ) deallocate(nsendlist_sall_3d)
         
         if ( allocated(nghostlist_u_3d) ) deallocate(nghostlist_u_3d)
         if ( allocated(nsendlist_u_3d)  ) deallocate(nsendlist_u_3d)
         
         if ( allocated(nghostlist_u_3dw) ) deallocate(nghostlist_u_3dw)
         if ( allocated(nsendlist_u_3dw)  ) deallocate(nsendlist_u_3dw)
      else
         ierror = 1
      end if

      if ( iwhat.eq.0 .or. iwhat.eq.2 ) then
      end if

      return
   end subroutine partition_cleanup


!> write ghostcell information to file
   subroutine write_ghosts(FNAM)
      use network_data, only : xzw, yzw
      use m_flowgeom,   only : xu, yu
      use m_alloc
      use m_missing

      implicit none

      character(len=*),  intent(in)                :: FNAM

      character(len=64), dimension(:), allocatable :: nampli

      double precision,  dimension(:), allocatable :: xpl, ypl

      integer,           dimension(:), allocatable :: ipl

      integer                                      :: ndmn, nums, numu, num, numnampli, NPL, MPOL

      integer                                      :: i, j
      integer                                      :: ic, Lf

!     get the number of domains in the water-level ghost list
      ndmn = size(nghostlist_s)-1
!     get the number of water-level ghost nodes
      nums = nghostlist_s(ndmn-1)
!     get the number of domains in the velocity ghost list
      ndmn = size(nghostlist_u)-1
!     get the number of velocity ghost links
      numu = nghostlist_u(ndmn-1)

      num = nums+numu+1

      allocate(xpl(num),ypl(num),ipl(num),nampli(1))

!     fill polygon with ghostcell information

      NPL=0
      numnampli = 0

!     water-level ghost nodes
      numnampli = numnampli+1
      if ( numnampli.gt.size(nampli) ) call realloc(nampli,int(1.2d0*dble(numnampli)+1d0),keepExisting=.true.,fill='')
      nampli(numnampli) = 'water-level'

      do i=0,ubound(nghostlist_s,1)
         do j=nghostlist_s(i-1)+1,nghostlist_s(i)
            NPL = NPL+1
            ic     = ighostlist_s(j)
            xpl(NPL) = xzw(ic)
            ypl(NPL) = yzw(ic)
            ipl(NPL) = i
         end do
      end do

!     add dmiss
      if ( NPL.gt.0 ) then
         NPL = NPL+1
         xpl(NPL) = DMISS
         ypl(NPL) = DMISS
         ipl(NPL) = 0
      end if

!     velocity ghost links
      numnampli = numnampli+1
      if ( numnampli.gt.size(nampli) ) call realloc(nampli,int(1.2d0*dble(numnampli)+1d0),keepExisting=.true.,fill='')
      nampli(numnampli) = 'velocity'

      do i=0,ubound(nghostlist_u,1)
         do j=nghostlist_u(i-1)+1,nghostlist_u(i)
            NPL = NPL+1
            Lf     = iabs(ighostlist_u(j))
            xpl(NPL) = xu(Lf)
            ypl(NPL) = yu(Lf)
            ipl(NPL) = i
         end do
      end do

!     write polygon
      call newfil(MPOL, FNAM)
      call wrildb(MPOL, xpl, ypl, NPL, ipl, NPL, xpl, 0, nampli, 64, numnampli)

!     deallocate
      if ( allocated(xpl) )    deallocate(xpl)
      if ( allocated(ypl) )    deallocate(ypl)
      if ( allocated(ipl) )    deallocate(ipl)
      if ( allocated(nampli) ) deallocate(nampli)

      return
   end subroutine write_ghosts


!  allocate and initialize tghost-type
   subroutine alloc_tghost(ghost, Nmax, Nmin)
      implicit none

      type(tghost), dimension(:), allocatable, intent(inout) :: ghost   !< array of tghost-type
      integer,                                 intent(in)    :: Nmax    !< objective array upper bound
      integer,                       optional, intent(in)    :: Nmin    !< objective array lower bound

      integer                                                :: i, Nmin_

      Nmin_ = 1
      if ( present(Nmin) ) then
         Nmin_ = Nmin
      end if

      if ( allocated(ghost) ) call dealloc_tghost(ghost)

      allocate(ghost(Nmin_:Nmax))

      do i=Nmin_,Nmax
         ghost(i)%num         = 0
         ghost(i)%numdomains  = 0
         ghost(i)%numneighdmn = 0
         allocate(ghost(i)%list(1))
         allocate(ghost(i)%N(-1:0))
         allocate(ghost(i)%neighdmn(1))
         ghost(i)%list     = 0
         ghost(i)%N        = 0
         ghost(i)%neighdmn = 0
      end do

      return
   end subroutine alloc_tghost


!  deallocate tghost-type
   subroutine dealloc_tghost(ghost)
      implicit none

      type(tghost), dimension(:), allocatable, intent(inout) :: ghost   !< array of tghost-type

      integer :: i

      if ( .not.allocated(ghost) ) return

      do i=lbound(ghost,1),ubound(ghost,1)
         if ( allocated(ghost(i)%list)     ) deallocate(ghost(i)%list)
         if ( allocated(ghost(i)%N)        ) deallocate(ghost(i)%N)
         if ( allocated(ghost(i)%neighdmn) ) deallocate(ghost(i)%neighdmn)
      end do

      deallocate(ghost)

      return
   end subroutine dealloc_tghost

!> make the lists of ghost-water-levels and velocities
!>    it is assumed that idomain and ighostlev are filled
   subroutine partition_get_ghosts(idmn, itype, ghost, ierror)
      use m_alloc
      use m_flowgeom

      implicit none

      integer,                                 intent(in)  :: idmn   !< domain number
      integer,                                 intent(in)  :: itype  !< type: 0: flownode, 1: flowlink
      type(tghost), dimension(:), allocatable, intent(out) :: ghost  !< tghost-type ghost list
      integer,                                 intent(out) :: ierror   !< error (1) or not (0)
      
      integer                                              :: minlev !< minimum ghost-level
      integer                                              :: maxlev !< maximum ghost-level

      integer                                              :: i, ic, j, Lf, num, numdomains
      integer                                              :: idmn_ghost, ipoint, iglev, jaghost
      integer                                              :: iglevL, iglevR
      integer                                              :: ighosttype

      ierror = 1

      if ( allocated(ghost) ) call dealloc_tghost(ghost)

      if ( itype.eq.ITYPE_Sall .or. itype.eq.ITYPE_S ) then  ! flownode
         if ( itype.eq.ITYPE_Sall ) then
            minlev = minghostlev_sall
            maxlev = maxghostlev_sall
            ighosttype = ighosttype_sall
         else if ( itype.eq.ITYPE_S ) then
            minlev = minghostlev_s
            maxlev = maxghostlev_s
            ighosttype = ighosttype_s
         end if

!        allocate and initialize
         call alloc_tghost(ghost,maxlev,minlev)
      
   !     get the sorted ghostnode list and count the number of nodes per ghost domain
   !        note: ghost()%N is per domain at first, will be cumulative later
         do ic=1,Ndx ! also include fictitious boundary node
            if ( idomain(ic).ne.idmn ) then
               idmn_ghost = idomain(ic)
               if ( ighosttype.eq.IGHOSTTYPE_CELLBASED ) then
                  iglev      = ighostlev_cellbased(ic)
               else if ( ighosttype.eq.IGHOSTTYPE_NODEBASED ) then
                  iglev      = ighostlev_nodebased(ic)
               else  ! combined
                  iglev      = ighostlev(ic)
               end if
               
               if ( iglev.ge.minlev .and. iglev.le.maxlev ) then
                  num        = ghost(iglev)%num
                  numdomains = ghost(iglev)%numdomains

                  num        = num+1
                  numdomains = max(numdomains, idmn_ghost+1)   ! domain numbers are zero-based
               
   !              reallocate if necessary
                  if ( numdomains-1.gt.ubound(ghost(iglev)%N,1) ) then
                     call realloc(ghost(iglev)%N, numdomains-1, -1, fill=0, keepExisting=.true.)
                  end if
                  if ( num.gt.ubound(ghost(iglev)%list,1) ) then
                     call realloc(ghost(iglev)%list, int(1.2d0*dble(num))+1, fill=0, keepExisting=.true.)
                  end if

   !              administer ghost cell
                  ghost(iglev)%N(idmn_ghost) = ghost(iglev)%N(idmn_ghost)+1
   !              determine the position of the ghost cell in the array
                  ipoint = sum(ghost(iglev)%N(0:idmn_ghost))
   !              insert cell in list
                  do i=num,ipoint+1,-1
                     ghost(iglev)%list(i) = ghost(iglev)%list(i-1)
                  end do
                  ghost(iglev)%list(ipoint)        = ic
               
                  ghost(iglev)%num        = num
                  ghost(iglev)%numdomains = numdomains
               end if
            end if
         end do
      
      else if ( itype.eq.ITYPE_U ) then  ! flowlink
         minlev = minghostlev_u
         maxlev = maxghostlev_u
         ighosttype = ighosttype_u

!        allocate and initialize
         call alloc_tghost(ghost,maxlev,minlev)
         
         num = 0
         do Lf=1,lnx
!           determine if flow link is a ghost link and get domain number and ghost level of link
            if ( ighosttype.eq.IGHOSTTYPE_CELLBASED ) then
               iglevL = ighostlev_cellbased(ln(1,Lf))
               iglevR = ighostlev_cellbased(ln(2,Lf))
            else if ( ighosttype.eq.IGHOSTTYPE_NODEBASED ) then
               iglevL = ighostlev_nodebased(ln(1,Lf))
               iglevR = ighostlev_nodebased(ln(2,Lf))
            else  ! combined
               iglevL = ighostlev(ln(1,Lf))
               iglevR = ighostlev(ln(2,Lf))
            end if
            call link_ghostdata(idmn, idomain(ln(1,Lf)), idomain(ln(2,Lf)), jaghost, idmn_ghost, iglevL, iglevR, iglev)
        
            if ( jaghost.eq.1 ) then
               if ( iglev.ge.minlev .and. iglev.le.maxlev ) then
                  num        = ghost(iglev)%num
                  numdomains = ghost(iglev)%numdomains

                  num        = num+1
                  numdomains = max(numdomains, idmn_ghost+1)   ! domain numbers are zero-based

   !              reallocate if necessary
                  if ( numdomains-1.gt.ubound(ghost(iglev)%N,1) ) then
                     call realloc(ghost(iglev)%N, numdomains-1, -1, fill=0, keepExisting=.true.)
                  end if
                  if ( num.gt.ubound(ghost(iglev)%list,1) ) then
                     call realloc(ghost(iglev)%list, int(1.2d0*dble(num))+1, fill=0, keepExisting=.true.)
                  end if

   !              administer ghost link
                  ghost(iglev)%N(idmn_ghost) = ghost(iglev)%N(idmn_ghost)+1
   !              determine the position of the ghost cell in the array
                  ipoint = sum(ghost(iglev)%N(0:idmn_ghost))
   !              insert cell in list
                  do i=num,ipoint+1,-1
                     ghost(iglev)%list(i) = ghost(iglev)%list(i-1)
                  end do
                  ghost(iglev)%list(ipoint)        = Lf

                  ghost(iglev)%num        = num
                  ghost(iglev)%numdomains = numdomains
               end if
            end if
         end do   ! do Lf=1,lnx
      else  ! unknown type
            call qnerror('partition_get_ghosts: unsupported ghost type', ' ', ' ')
      end if

!     accumulate ghost()%N
      do iglev=minlev,maxlev
         do i=ghost(iglev)%numdomains-1,0,-1
            do j=0,i-1
               ghost(iglev)%N(i) = ghost(iglev)%N(i) + ghost(iglev)%N(j)
            end do
         end do
      end do

!     count the neighboring domains
      do iglev=minlev,maxlev
         num = 0
         do i=0,ghost(iglev)%numdomains-1
            if ( ghost(iglev)%N(i)-ghost(iglev)%N(i-1).gt.0 ) then
               num = num+1
               if ( num.gt.ubound(ghost(iglev)%neighdmn,1) ) call realloc(ghost(iglev)%neighdmn, int(1.2d0*dble(num)+1d0), keepExisting=.true., fill=0)
               ghost(iglev)%neighdmn(num) = i
            end if
         end do
         ghost(iglev)%numneighdmn = num
      end do

      ierror = 0
 1234 continue
      
      return
   end subroutine partition_get_ghosts
  
   
!> determine if flow link is ghost link, flow link domain number and ghost level
!>   
!>   a flow link is owned by the adjacent cell with the smallest domain number
!>   thus, a link is a ghost link if:
!>                it connects two ghost cells, or
!> ACTIVATED ->   it connects only one ghost cell, and the other domain number is smaller than the own domain number, or
!                 it connects connects a cell in the own subdomain with ghostlevel >0 (at the boundary)
   subroutine link_ghostdata(idmn,idmnL,idmnR,jaghost,idmn_link,ighostlevL,ighostlevR,iglev)
      use m_flowgeom
      implicit none
      
      integer, intent(in)            :: idmn        !< domain number based on which the ghost-checking is done (typically my_rank)
      integer, intent(in)            :: idmnL       !< domain number of left neighboring cell
      integer, intent(in)            :: idmnR       !< domain number of right neighboring cell
      integer, intent(out)           :: jaghost     !< flow link is ghost link (1) or not (0)
      integer, intent(out)           :: idmn_link   !< flow link domain number
      integer, intent(in),  optional :: ighostlevL  !< ghost level of left neighboring cell
      integer, intent(in),  optional :: ighostlevR  !< ghost level of right neighboring cell
      integer, intent(out), optional :: iglev       !< flow link ghost level (if ghostlevels specified, otherwise 0)
      
!      integer              :: icL, icR
               
      jaghost    = 0
      idmn_link = idmn
      if ( present(iglev) ) then
         iglev      = 0
      end if
      
!      icL = ln(1,Lf)
!      icR = ln(2,Lf)
      
      if ( ( idmnL.ne.idmn .and. idmnR.ne.idmn ) .or.  &
           ( idmnL.eq.idmn .and. idmnR.lt.idmn ) .or.  &
           ( idmnL.lt.idmn .and. idmnR.eq.idmn )       &
         ) then
         jaghost = 1
      else if ( present(ighostlevL) .and. present(ighostlevR) ) then
         if ( ( idmnL.eq.idmn .and. ighostlevL.gt.0 ) .or.  &
              ( idmnR.eq.idmn .and. ighostlevR.gt.0 ) &
            ) then
            jaghost = 1
         end if
      end if
      
      if ( jaghost.eq.1 ) then
  
         if ( present(ighostlevL) .and. present(ighostlevR) .and. present(iglev) ) then
            idmn_link = min(idmnL,idmnR)  ! a choice
            
!           ghost domain cannot be own domain         
            if ( idmn_link.eq.idmn ) idmn_link = idmnL+idmnR-idmn
            iglev = min(ighostlevL,ighostlevR)
            
!           ghost level may be zero
            if ( iglev.eq.0 ) iglev = max(ighostlevL,ighostlevR)
         end if
      end if
               
      return
   end subroutine link_ghostdata


!> fill sendghostlist
   subroutine partition_fill_sendlist(idmn, idmn_other, itype, N, x, y, ghost, isendlist, nsendlist, ierror)
      
      use m_alloc
      use network_data, only: xzw, yzw
      use m_flowgeom,   only: xu, yu
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none

      integer,                                     intent(in)    :: idmn         !< own domain
      integer,                                     intent(in)    :: idmn_other   !< other domain
      integer,                                     intent(in)    :: itype        !< type: 0: flownode, 1: flowlink
      integer,                                     intent(in)    :: N            !< number of flownodes/links requested by other domain
      double precision, dimension(N),              intent(in)    :: x, y         !< coordinates of requested flownodes/links
      type(tghost),     dimension(:),              intent(in)    :: ghost        !< tghost-type list of ghost flownodes/links
      integer,          dimension(:), allocatable, intent(inout) :: isendlist    !< send list
      integer,          dimension(:), allocatable, intent(inout) :: nsendlist    !< cumulative number of flownodes/links per domain in the list, starts with fictitious domain 0, dim(-1:ndomains-1)
      integer,                                     intent(out)   :: ierror       !< error (1) or not (0)

      integer,          dimension(:), allocatable                :: ilist

      double precision                                           :: xi, yi, dis

      integer                                                    :: i, ii, j, iglev, num, numnew
      integer                                                    :: numdomains, idum
      
      integer                                                    :: jafound
      double precision, parameter                                :: dtol=1d-4

      ierror = 1

!     allocate
      allocate(ilist(N))
      ilist = 0

      num = 0
      do j=1,N
         jafound = 0
  ghstlp:do iglev=lbound(ghost,1),ubound(ghost,1)
            if ( idmn.gt.ghost(iglev)%numdomains-1 .or. idmn.lt.0 ) cycle
            do ii=ghost(iglev)%N(idmn-1)+1,ghost(iglev)%N(idmn)
               i = ghost(iglev)%list(ii)
               if ( itype.eq.ITYPE_S .or. itype.eq.ITYPE_SALL .or. itype.eq.ITYPE_SNONOVERLAP ) then  ! flownodes
                  xi = xzw(i)
                  yi = yzw(i)
               else if ( itype.eq.ITYPE_U ) then   ! flowlinks
                  xi = xu(i)
                  yi = yu(i)
               else  ! unknown type
                  call qnerror('partition_fill_sendlist: unknown ghost type', ' ', ' ')
                  goto 1234
               end if

               dis = dbdistance(x(j),y(j),xi,yi, jsferic, jasfer3D, dmiss)
               if ( dis.lt.dtol ) then ! found
                  num = num+1
                  ilist(num) = i
                  jafound = 1
                  exit ghstlp
               end if
            end do
         end do ghstlp
         
!        check if flownode/link was found
!         if ( jafound.eq.0 ) then
!            num = num+1
!            ilist(num) = 0 ! mark as not found            
!            continue
!         end if
         
!!        safety check
!         if ( j.ne.num ) then
!            call qnerror('partition_fill_sendlist: numbering error', ' ', ' ')
!            goto 1234
!         end if
         
      end do

      !if ( num.eq.0 ) then
      !   goto 1234  ! no flownodes/links found
      !end if

!     (re)allocate
      if ( .not.allocated(nsendlist) ) then
         !numdomains = idmn_other+1
         numdomains = ndomains
         allocate(nsendlist(-1:numdomains-1))
         nsendlist = 0
      else
         numdomains = ubound(nsendlist,1)+1    ! number of domains so far
         if ( idmn_other.gt.numdomains-1 ) then      ! number of domains needs to be increased
            idum = nsendlist(numdomains-1)
            call realloc(nsendlist, idmn_other, -1, fill=idum, keepExisting=.true.)
            numdomains = idmn_other+1
         end if
      end if

      if ( .not.allocated(isendlist) ) then
         allocate(isendlist(num))
      else
         numnew = nsendlist(numdomains-1)+num
         if ( numnew.gt.ubound(isendlist,1) ) then
            call realloc(isendlist, int(1.2d0*dble(numnew)+1d0), fill=0, keepExisting=.true.)
         end if
      end if

!     fill list
      do i=nsendlist(numdomains-1),nsendlist(idmn_other)+1,-1
         isendlist(i+num) = isendlist(i)
      end do
      do i=1,num
         isendlist(nsendlist(idmn_other)+i) = ilist(i)
      end do
      nsendlist(idmn_other:numdomains-1) = nsendlist(idmn_other:numdomains-1) + num

      ierror = 0
 1234 continue

 !    deallocate
      if ( allocated(ilist) ) deallocate(ilist)

      return
   end subroutine partition_fill_sendlist


!>  make the sendlists
!>    include additional layer to detect all flownodes and flowlinks ("enclosed, level-five" cells)
   subroutine partition_make_sendlists(idmn, fnam, ierror)
      use m_polygon
      use m_alloc

      implicit none

      integer,                    intent(in)  :: idmn       !< own domain number
      character(len=*),           intent(in)  :: fnam       !< basename (not used in mpi-mode)

      integer,                    intent(out) :: ierror     !< error (1) or not (0)

      type(tghost), dimension(:), allocatable :: sghost
      type(tghost), dimension(:), allocatable :: sallghost
      type(tghost), dimension(:), allocatable :: ughost
      
!      integer                                 :: numlaymax
      
      integer,                    parameter   :: maxnamelen=255

      integer                                 :: idmn_other, minp
      
      character(len=maxnamelen)               :: filename
      character(len=4)                        :: sdmn       ! domain number string

      ierror = 1
      
!      numlaymax = numlay+1 ! include additional layer to detect all flownodes and flowlinks
   
      if ( allocated(isendlist_s) ) deallocate(isendlist_s)
      if ( allocated(nsendlist_s) ) deallocate(nsendlist_s)
      
      if ( allocated(isendlist_sall) ) deallocate(isendlist_sall)
      if ( allocated(nsendlist_sall) ) deallocate(nsendlist_sall)

      if ( allocated(isendlist_u) ) deallocate(isendlist_u)
      if ( allocated(nsendlist_u) ) deallocate(nsendlist_u)

      if ( jampi.eq.0 ) then
!        save polygon
         call savepol()
         
         do idmn_other=0,ndomains-1
            write(sdmn, '(I4.4)') idmn_other
            filename = trim(trim(fnam)//'_'//sdmn//'_gst.pli')
            call oldfil(minp, filename)
            call reapol(minp, 0)

            if ( NPL.lt.1 ) cycle

!           make ghostcells of other domain
!            call partition_set_ghostlevels(idmn_other, numlaymax, ierror)
            call partition_set_ghostlevels(idmn_other, numlay_cellbased+1, numlay_nodebased+1, 1, ierror)
            call partition_get_ghosts(idmn_other, ITYPE_S, sghost, ierror)
            call partition_get_ghosts(idmn_other, ITYPE_Sall, sallghost, ierror)
!           find the send cells
            call partition_fill_sendlist(idmn, idmn_other, ITYPE_SALL, NPL, XPL, YPL, sallghost, isendlist_sall, nsendlist_sall, ierror)
            call partition_fill_sendlist(idmn, idmn_other, ITYPE_S,    NPL, XPL, YPL, sghost, isendlist_s, nsendlist_s, ierror)

            call partition_get_ghosts(idmn_other, ITYPE_U, ughost, ierror)
            call partition_fill_sendlist(idmn, idmn_other, ITYPE_U, NPL, XPL, YPL, ughost, isendlist_u, nsendlist_u, ierror)
         end do

   !     restore polygon
         call restorepol()
      else
!         call partition_make_sendlist_MPI(ITYPE_S,numlaymax)
!         call partition_make_sendlist_MPI(ITYPE_Sall,numlaymax)
!         call partition_make_sendlist_MPI(ITYPE_u,numlaymax)
         call partition_make_sendlist_MPI(ITYPE_S,   numlay_cellbased+1,numlay_nodebased+1)
         call partition_make_sendlist_MPI(ITYPE_Sall,numlay_cellbased+1,numlay_nodebased+1)
         call partition_make_sendlist_MPI(ITYPE_u,   numlay_cellbased+1,numlay_nodebased+1)
         
!        communicate sendlist back to obtain (possibly) reduced ghostlist in own domain
!        deallocate first
         nghostlist_s = 0
         if ( allocated(ighostlist_s) ) deallocate(ighostlist_s)
         
         nghostlist_sall = 0
         if ( allocated(ighostlist_sall) ) deallocate(ighostlist_sall)
         
         nghostlist_u = 0
         if ( allocated(ighostlist_u) ) deallocate(ighostlist_u)
         
!        fill ghostlists
!         call partition_make_sendlist_MPI(ITYPE_S,numlaymax,ifromto=1)
!         call partition_make_sendlist_MPI(ITYPE_Sall,numlaymax,ifromto=1)
!         call partition_make_sendlist_MPI(ITYPE_u,numlaymax,ifromto=1)
         call partition_make_sendlist_MPI(ITYPE_S,   numlay_cellbased+1,numlay_nodebased+1,ifromto=1)
         call partition_make_sendlist_MPI(ITYPE_Sall,numlay_cellbased+1,numlay_nodebased+1,ifromto=1)
         call partition_make_sendlist_MPI(ITYPE_u,   numlay_cellbased+1,numlay_nodebased+1,ifromto=1)
      end if
      
!     set number of send nodes/links
      numsend_s    = 0
      numsend_sall = 0
      numsend_u    = 0
      if ( allocated(nsendlist_sall) ) then
         if ( allocated(nsendlist_s) )    numsend_s    = nsendlist_s(ndomains-1)
         if ( allocated(nsendlist_sall) ) numsend_sall = nsendlist_sall(ndomains-1)
         if ( allocated(nsendlist_u) )    numsend_u    = nsendlist_u(ndomains-1)
      
!        flow links: check and fix orientation of send list
!         call partition_fixorientation_sendlist(ierror)
      end if
      
!     (re)set number of ghost nodes/links    
      numghost_s    = 0
      numghost_sall = 0
      numghost_u    = 0  
      if ( allocated(nghostlist_sall) ) then
         if ( allocated(nghostlist_s) )    numghost_s    = nghostlist_s(ndomains-1)
         if ( allocated(nghostlist_sall) ) numghost_sall = nghostlist_sall(ndomains-1)
         if ( allocated(nghostlist_u) )    numghost_u    = nghostlist_u(ndomains-1)
      end if
      
!     safety: make dummy empty lists
      call realloc(nghostlist_s,    ndomains-1, -1, keepExisting=.true., fill=0)
      call realloc(nghostlist_sall, ndomains-1, -1, keepExisting=.true., fill=0)
      call realloc(nghostlist_u,    ndomains-1, -1, keepExisting=.true., fill=0)
                                                    
      call realloc(nsendlist_s,     ndomains-1, -1, keepExisting=.true., fill=0)
      call realloc(nsendlist_sall,  ndomains-1, -1, keepExisting=.true., fill=0)
      call realloc(nsendlist_u,     ndomains-1, -1, keepExisting=.true., fill=0)
      
      
      call realloc(ighostlist_s,    max(numghost_s,1),    keepExisting=.true., fill=0)
      call realloc(ighostlist_sall, max(numghost_sall,1), keepExisting=.true., fill=0)
      call realloc(ighostlist_u,    max(numghost_u,1),    keepExisting=.true., fill=0)
      
      call realloc(isendlist_s,     max(numsend_s,1),     keepExisting=.true., fill=0)
      call realloc(isendlist_sall,  max(numsend_sall,1),  keepExisting=.true., fill=0)
      call realloc(isendlist_u,     max(numsend_u,1),     keepExisting=.true., fill=0)

      ierror = 0
 1234 continue

!     deallocate
      if ( allocated(sghost) ) call dealloc_tghost(sghost)
      if ( allocated(sallghost) ) call dealloc_tghost(sallghost)
      if ( allocated(ughost) ) call dealloc_tghost(ughost)

      return
   end subroutine partition_make_sendlists
   
!> communicate ghost cells to other domains
   subroutine partition_make_sendlist_MPI(itype,numlay_cell,numlay_node,ifromto)
      use m_alloc
      use m_missing
      use network_data, only : xzw, yzw
      use m_flowgeom,   only : xu, yu
#ifdef HAVE_MPI
      use mpi
#endif
      
      implicit none
      integer,                                       intent(in)  :: itype      !< type: 0: flownode, 1: flowlink
      
      integer,                                       intent(in)  :: numlay_cell  !< number of cell-based ghost-levels considered (normally numlay_cellbased+1)
      integer,                                       intent(in)  :: numlay_node  !< number of node-based ghost-levels considered (normally numlay_nodebased+1)
      integer,                          optional,    intent(in)  :: ifromto    !< fill sendlists from ghostlists (0, default) or ghostlists from sendlists (1)

#ifdef HAVE_MPI 

      type(tghost),     dimension(:),   allocatable              :: sghost
      type(tghost),     dimension(:),   allocatable              :: sallghost
      type(tghost),     dimension(:),   allocatable              :: ughost
      
      double precision, dimension(:,:), allocatable              :: xysnd        ! send     cell-center coordinates (first x, then y)
      double precision, dimension(:,:), allocatable              :: xyrec        ! recieved cell-center coordinates (first x, then y)
      
      integer,          dimension(:,:), allocatable              :: numrequest   ! number of cells requested from other domains (message size)
      
      integer,          dimension(:),   allocatable              :: ighostlev_bak             ! store of the combined ghost levelslevels
      integer,          dimension(:),   allocatable              :: ighostlev_cellbased_bak   ! store of the cell-based ghost levels
      integer,          dimension(:),   allocatable              :: ighostlev_nodebased_bak   ! store of the node-based ghost levels
      
      integer,          dimension(:),   pointer                  :: nfromlist
      integer,          dimension(:),   pointer                  :: ifromlist
      
      integer,          dimension(MPI_STATUS_SIZE)               :: istat
      
      integer,          dimension(ndomains)                      :: irequest
      
      integer,          dimension(0:ndomains-1)                  :: numrequest_loc
      
      integer                                                    :: nrequest, itag, ierr
      
      integer                                                    :: i, icount, k, num, other_domain
      integer                                                    :: idir
      
      integer, parameter                                         :: NUMRECINI=10
      character(len=1024)                                        :: str
      
      integer,          dimension(:), allocatable, target        :: idumzero
      
      allocate(idumzero(-1:ndomains-1))
      idumzero = 0
      
      if ( present(ifromto) ) then
         idir = ifromto
      else
         idir = 0 ! fill sendlists
      end if

      itag = 1
      
!     store ghost levels of this domain
      allocate(ighostlev_bak(ubound(ighostlev,1)))
      ighostlev_bak = ighostlev
      allocate(ighostlev_cellbased_bak(ubound(ighostlev_cellbased,1)))
      ighostlev_cellbased_bak = ighostlev_cellbased
      allocate(ighostlev_nodebased_bak(ubound(ighostlev_nodebased,1)))
      ighostlev_nodebased_bak = ighostlev_nodebased
      
!     allocate send and recieve arrays
      allocate(xysnd(2,NUMRECINI))
      allocate(xyrec(2,NUMRECINI))
      
      allocate(numrequest(0:ndomains-1,0:ndomains-1))
      numrequest = 0
      
!     fill send coordinates
      if ( itype.eq.ITYPE_S ) then  ! flow nodes
         if ( idir.eq. 0 ) then
            nfromlist => nghostlist_s
            ifromlist => ighostlist_s
         else
            nfromlist => nsendlist_s
            ifromlist => isendlist_s
         end if
         
         if ( .not.associated(nfromlist) ) then
            nfromlist => idumzero
         end if
         
         num = nfromlist(ndomains-1)
         call realloc(xysnd, (/ 2, num /), keepExisting=.false., fill=0d0)
         
         do i=1,num
            k = ifromlist(i)
            xysnd(1,i) = xzw(k)
            xysnd(2,i) = yzw(k)
         end do
      else if ( itype.eq.ITYPE_Sall ) then  ! all flow nodes
         if ( idir.eq. 0 ) then
            nfromlist => nghostlist_sall
            ifromlist => ighostlist_sall
         else
            nfromlist => nsendlist_sall
            ifromlist => isendlist_sall
         end if
         
         if ( .not.associated(nfromlist) ) then
            nfromlist => idumzero
         end if
         
         num = nfromlist(ndomains-1)
         call realloc(xysnd, (/ 2, num /), keepExisting=.false., fill=0d0)
         
         do i=1,num
            k = ifromlist(i)
            xysnd(1,i) = xzw(k)
            xysnd(2,i) = yzw(k)
         end do
      else if ( itype.eq.ITYPE_Snonoverlap ) then  ! all non-overlapping flow nodes (for solver)
         if ( idir.eq. 0 ) then
            nfromlist => nghostlist_snonoverlap
            ifromlist => ighostlist_snonoverlap
         else
            nfromlist => nsendlist_snonoverlap
            ifromlist => isendlist_snonoverlap
         end if
         
         if ( .not.associated(nfromlist) ) then
            nfromlist => idumzero
         end if
         
         num = nfromlist(ndomains-1)
         call realloc(xysnd, (/ 2, num /), keepExisting=.false., fill=0d0)
         
         do i=1,num
            k = ifromlist(i)
            xysnd(1,i) = xzw(k)
            xysnd(2,i) = yzw(k)
         end do
      else if ( itype.eq.ITYPE_U ) then
         if ( idir.eq. 0 ) then
            nfromlist => nghostlist_u
            ifromlist => ighostlist_u
         else
            nfromlist => nsendlist_u
            ifromlist => isendlist_u
         end if
         
         if ( .not.associated(nfromlist) ) then
            nfromlist => idumzero
         end if
         
         num = nfromlist(ndomains-1)
         call realloc(xysnd, (/ 2, num /), keepexisting=.false., fill=0d0)
         
         do i=1,num
            k = ifromlist(i)
            xysnd(1,i) = xu(k)
            xysnd(2,i) = yu(k)
         end do
      else
         call qnerror('partition_make_sendlist_MPI: unknown ghost type', ' ', ' ')
         goto 1234
      end if
      
!     count other domains connected and number of cells to be sent
      numrequest_loc = 0   ! message size, per other domain
      do other_domain=0,ndomains-1
         if ( other_domain.eq.my_rank ) cycle
         num = nfromlist(other_domain)-nfromlist(other_domain-1)
         if ( num.lt.1 ) cycle
         numrequest_loc(other_domain) = num
      end do
      
!     compose other-domain administration
      numrequest = 0 ! safety
      call mpi_allgather(numrequest_loc, ndomains, MPI_INTEGER, numrequest, ndomains, MPI_INTEGER, DFM_COMM_DFMWORLD, ierr)
      
!     send own ghost cells to other domain     
      nrequest = 0   ! number of outgoing requests
      irequest = 0
      do other_domain=0,ndomains-1
         if ( other_domain.eq.my_rank ) cycle
         num = numrequest(other_domain, my_rank)
         if ( num.lt.1 ) cycle
         nrequest = nrequest+1
         call mpi_isend(xysnd(1,nfromlist(other_domain-1)+1), 2*num, mpi_double_precision, other_domain, itag, DFM_COMM_DFMWORLD, irequest(nrequest), ierr)
      end do
      
!     make ghosts if we need to search in own subdomain (only once for all other subdomains)
      if ( idir.eq.1 ) then
         if ( itype.eq.ITYPE_S ) then
            call partition_get_ghosts(my_rank, ITYPE_S, sghost, ierr)
         else if ( itype.eq.ITYPE_SALL ) then
            call partition_get_ghosts(my_rank, ITYPE_SALL, sallghost, ierr)
         else if ( itype.eq.ITYPE_U) then
            call partition_get_ghosts(my_rank, ITYPE_U, ughost, ierr)
         end if
      end if
      
!     recieve requests from other domains
      do other_domain=0,ndomains-1
         num = numrequest(my_rank,other_domain)
         if ( num.lt.1 ) cycle
!        get message length
         call mpi_probe(other_domain,itag,DFM_COMM_DFMWORLD,istat,ierr)
         call mpi_get_count(istat,mpi_double_precision,icount,ierr)
         
!        check message length (safety)
         if ( icount.ne.2*num ) then
            write(str, *) 'partition_make_sendlist_MPI: icount.ne.2*num, domain: ', my_rank, ', other domain: ', other_domain, ' icount: ', icount, ', 2*num: ', 2*num
            call qnerror(str, ' ', ' ')
            call qnerror('partition_communicate_ghosts: message length error', ' ', ' ')
         end if
         
!        check array size
         if ( icount.gt.2*ubound(xyrec,2) ) then   ! reallocate if necessary
            call realloc(xyrec, (/ 2,int(1.2d0*dble(icount/2)+1d0) /), keepExisting=.false., fill=0d0)
!            call writemesg('resizing xyrec')
         end if

         call mpi_recv(xyrec,icount,mpi_double_precision,other_domain,MPI_ANY_TAG,DFM_COMM_DFMWORLD,istat,ierr)
         
!        make ghostcells of other domain in this domain
         if ( idir.eq.0 ) then
!            call partition_set_ghostlevels(other_domain, numlaymax, ierr)
            call partition_set_ghostlevels(other_domain, numlay_cell, numlay_node, 1, ierr)
         end if
         
         if ( itype.eq.ITYPE_S ) then
!           find the send cells
            if ( idir.eq.0 ) then
               call partition_get_ghosts(other_domain, ITYPE_S, sghost, ierr)
               call partition_fill_sendlist(my_rank, other_domain, ITYPE_S, num, xyrec(1,1:num), xyrec(2,1:num), sghost, isendlist_s, nsendlist_s, ierr)
            else
               call partition_fill_sendlist(other_domain, other_domain, ITYPE_S, num, xyrec(1,1:num), xyrec(2,1:num), sghost, ighostlist_s, nghostlist_s, ierr)
            end if
            
            if ( ierr.ne.0 ) goto 1234
         else if ( itype.eq.ITYPE_Sall ) then
!           find the send cells
            if ( idir.eq.0 ) then
               call partition_get_ghosts(other_domain, ITYPE_Sall, sallghost, ierr)
               call partition_fill_sendlist(my_rank, other_domain, ITYPE_Sall, num, xyrec(1,1:num), xyrec(2,1:num), sallghost, isendlist_sall, nsendlist_sall, ierr)
            else
               call partition_fill_sendlist(other_domain, other_domain, ITYPE_Sall, num, xyrec(1,1:num), xyrec(2,1:num), sallghost, ighostlist_sall, nghostlist_sall, ierr)
            end if
            
            if ( ierr.ne.0 ) goto 1234
         else if ( itype.eq.ITYPE_Snonoverlap ) then
!           find the send cells
            if ( idir.eq.0 ) then
               call partition_get_ghosts(other_domain, ITYPE_Sall, sallghost, ierr)
               call partition_fill_sendlist(my_rank, other_domain, ITYPE_Snonoverlap, num, xyrec(1,1:num), xyrec(2,1:num), sallghost, isendlist_snonoverlap, nsendlist_snonoverlap, ierr)
            else
               call partition_fill_sendlist(other_domain, other_domain, ITYPE_Snonoverlap, num, xyrec(1,1:num), xyrec(2,1:num), sallghost, ighostlist_snonoverlap, nghostlist_snonoverlap, ierr)
            end if
            
            if ( ierr.ne.0 ) goto 1234
         else if ( ITYPE.eq.ITYPE_U ) then
!           find the send cells
            if ( idir.eq.0 ) then
               call partition_get_ghosts(other_domain, ITYPE_U, ughost, ierr)
               call partition_fill_sendlist(my_rank, other_domain, ITYPE_U, num, xyrec(1,1:num), xyrec(2,1:num), ughost, isendlist_u, nsendlist_u, ierr)
            else
               call partition_fill_sendlist(other_domain, other_domain, ITYPE_U, num, xyrec(1,1:num), xyrec(2,1:num), ughost, ighostlist_u, nghostlist_u, ierr)
            end if
            
            if ( ierr.ne.0 ) goto 1234
         else
            call qnerror('partition_make_sendlist_MPI: uknown ghost type', ' ', ' ')
         end if
      end do   ! other_domain=0,ndomains-1
      
!     terminate send (safety)
      do i=1,nrequest
         call mpi_wait(irequest(i),istat,ierr)
      end do
      
!     restore ghost levels
      ighostlev = ighostlev_bak
      ighostlev_cellbased = ighostlev_cellbased_bak
      ighostlev_nodebased = ighostlev_nodebased_bak
         
1234  continue          
      
!     deallocate
      if ( allocated(idumzero)   )     deallocate(idumzero)
      if ( allocated(xysnd)      )    deallocate(xysnd)
      if ( allocated(xyrec)      )    deallocate(xyrec)
      if ( allocated(numrequest) )    deallocate(numrequest)
      if ( allocated(sghost)     )    call dealloc_tghost(sghost)
      if ( allocated(sallghost)  )    call dealloc_tghost(sallghost)
      if ( allocated(ughost)     )    call dealloc_tghost(ughost)
      if ( allocated(ighostlev_bak) ) deallocate(ighostlev_bak)
      if ( allocated(ighostlev_nodebased_bak) ) deallocate(ighostlev_nodebased_bak)
         
#endif      
      return
   end subroutine partition_make_sendlist_MPI
   
   
!> fix orientation of send flow-links, opposite orientation will have negative indices
   subroutine partition_fixorientation_ghostlist(ierror)
      use m_flowgeom, only: Lnx, Lnxi, csu, snu, Ln, Ndx
      use unstruc_messages
      implicit none
      
      character(len=128)                                         :: message
      integer,                                     intent(out)   :: ierror       !< error (1) or not (0)
      
      double precision, dimension(:), allocatable                :: csu_loc, snu_loc
      
      integer,          dimension(:), allocatable                :: imask
      
      double precision                                           :: dum
      
      integer                                                    :: i, k, k1, k2, L, num
      
      double precision, parameter                                :: dtol = 1d-8
      
      ierror = 1
      
!     allocate
      allocate(csu_loc(Lnx), snu_loc(Lnx))
      allocate(imask(Ndx))
      
!     fill local copy of cosine and sine of flow links
      csu_loc = csu
      snu_loc = snu
      
!     copy csu and snu from other domains
      call update_ghosts(ITYPE_U, 1, Lnx, csu_loc, ierror)
      call update_ghosts(ITYPE_U, 1, Lnx, snu_loc, ierror)
      
      if ( ierror /= 0 ) goto 1234
      
!     mark the flownodes in the ghostlist
      imask = 0
      do i=1,nghostlist_sall(ndomains-1)
         k = ighostlist_sall(i)
         imask(k) = 1
      end do
      
!     mark the own flownodes      
      do k=1,Ndx
         if ( idomain(k).eq.my_rank ) then
            imask(k) = 1
         end if
      end do
      
!     check and fix orientation
      num = 0
      do i=1,nghostlist_u(ndomains-1)
         L = abs(ighostlist_u(i))
         
!        safety check
         dum = abs(abs(csu_loc(L)*csu(L) + snu_loc(L)*snu(L))-1d0)
         if ( dum.ge.dtol ) then
!           check if this is a valid ghostlink (see also subroutine "disable_invalid_ghostcells_with_wu")
            k1 = ln(1,L)
            k2 = ln(2,L)
            if ( imask(k1).eq.1 .and. imask(k2).eq.1 .and. L.le.Lnxi ) then
               write(message, "('partition_fixorientation_ghostlist: flowlink mismatch, val=', E15.5, ', link=', I0)") dum, L
               call mess(LEVEL_ERROR, trim(message))
            else
               write(message, "('partition_fixorientation_ghostlist: dummy flowlink mismatch, val=', E15.5, ', link=', I0)") dum, L
               call mess(LEVEL_INFO, trim(message))
            end if
         end if
         
         if ( abs(csu_loc(L)*csu(L) + snu_loc(L)*snu(L) + 1d0).lt.dtol ) then
            ighostlist_u(i) = -ighostlist_u(i)
            num = num+1
         end if
      end do

      call mess(LEVEL_INFO, 'partition_fixorientation_ghostlist: number of reversed flowlinks=', num)
      
      ierror = 0
1234  continue
      
!     deallocate
      if ( allocated(csu_loc) ) deallocate(csu_loc)
      if ( allocated(snu_loc) ) deallocate(snu_loc)
      if ( allocated(imask)   ) deallocate(imask)
      
      return
   end subroutine partition_fixorientation_ghostlist

!> copy sendlist to samples
   subroutine copy_sendlist_to_sam()
      use m_samples
      use network_data, only : xzw, yzw
      use m_flowgeom,   only : xu, yu

      implicit none

      integer :: i, j, k, numdomains, num

      call savesam()
      call delsam(0)

      numdomains = ubound(nsendlist_s,1)+1
      num = nsendlist_s(numdomains-1)
      numdomains = ubound(nsendlist_sall,1)+1
      num = num + nsendlist_sall(numdomains-1)
      numdomains = ubound(nsendlist_u,1)+1
      num = num + nsendlist_u(numdomains-1)

      call increasesam(num)

      NS = 0
      do i=0,ubound(nsendlist_s,1)
         do j=nsendlist_s(i-1)+1,nsendlist_s(i)
            NS = NS+1
            k = isendlist_s(j)
            xs(NS) = xzw(k)
            ys(NS) = yzw(k)
            zs(NS) = dble(i)
         end do
      end do
      
      do i=0,ubound(nsendlist_sall,1)
         do j=nsendlist_sall(i-1)+1,nsendlist_sall(i)
            NS = NS+1
            k = isendlist_sall(j)
            xs(NS) = xzw(k)
            ys(NS) = yzw(k)
            zs(NS) = dble(i)
         end do
      end do
      
      do i=0,ubound(nsendlist_u,1)
         do j=nsendlist_u(i-1)+1,nsendlist_u(i)
            NS = NS+1
            k = abs(isendlist_u(j))
            xs(NS) = xu(k)
            ys(NS) = yu(k)
            zs(NS) = -dble(i)
         end do
      end do

   end subroutine copy_sendlist_to_sam
   
   
!> derive 3d ghost- and sendlists from 2d counterparts and 3d layer information
!>    only nghostlist_XXX_3d will be generated, actual ghost flow node/link numbers
!>       can be derived from 2d ighostlist/isendlist and layer information
!>    it is assumed that 3d layer information (kbot, Lbot, kmxn, kmxL) is available
   subroutine partition_make_ghostsendlists_3d(ierror)
      use m_flowgeom, only : Ndx, Lnx
      use m_flow,    only : kmx, kbot, Lbot, kmxn, kmxL
      implicit none
      
      integer, intent(out) :: ierror
      
      integer              :: LL, L
      integer, allocatable :: kmxL1(:)
      
      ierror = 0   ! so far, so good
      
      if ( kmx.eq.0 ) goto 1234
      
      ierror = 1
      
      if ( allocated(nghostlist_s_3d) ) deallocate(nghostlist_s_3d)
      if ( allocated(nsendlist_s_3d) )  deallocate(nsendlist_s_3d)
!      allocate(nghostlist_s_3d(-1:ndomains-1))    ! not used
!      allocate(nsendlist_s_3d(-1:ndomains-1))     ! not used
     
      if ( allocated(nghostlist_sall_3d) ) deallocate(nghostlist_sall_3d)
      if ( allocated(nsendlist_sall_3d) )  deallocate(nsendlist_sall_3d)
      allocate(nghostlist_sall_3d(-1:ndomains-1))
      allocate(nsendlist_sall_3d(-1:ndomains-1))
      
      if ( allocated(nghostlist_u_3d) ) deallocate(nghostlist_u_3d)
      if ( allocated(nsendlist_u_3d) )  deallocate(nsendlist_u_3d)
      allocate(nghostlist_u_3d(-1:ndomains-1))
      allocate(nsendlist_u_3d(-1:ndomains-1))
      
      if ( allocated(nghostlist_u_3dw) ) deallocate(nghostlist_u_3dw)
      if ( allocated(nsendlist_u_3dw) )  deallocate(nsendlist_u_3dw)
      allocate(nghostlist_u_3dw(-1:ndomains-1))
      allocate(nsendlist_u_3dw(-1:ndomains-1))
      
      call partition_fill_ghostsendlist_3d(nghostlist_sall(ndomains-1), ighostlist_sall, nghostlist_sall, Ndx, kbot, kmxn, nghostlist_sall_3d)
      call partition_fill_ghostsendlist_3d(nsendlist_sall(ndomains-1),  isendlist_sall,  nsendlist_sall,  Ndx, kbot, kmxn, nsendlist_sall_3d)
      
      call partition_fill_ghostsendlist_3d(nghostlist_u(ndomains-1), ighostlist_u, nghostlist_u, Lnx, Lbot, kmxL, nghostlist_u_3d)
      call partition_fill_ghostsendlist_3d(nsendlist_u(ndomains-1),  isendlist_u,  nsendlist_u,  Lnx, Lbot, kmxL, nsendlist_u_3d)
      
      if (allocated(kmxL1)) deallocate(kmxL1)  
      allocate(kmxL1(lnx))
      do L = 1,lnx
         kmxL1(L) =  kmxL(L) + 1
      enddo
      call partition_fill_ghostsendlist_3d(nghostlist_u(ndomains-1), ighostlist_u, nghostlist_u, Lnx, Lbot, kmxL1, nghostlist_u_3dw)
      call partition_fill_ghostsendlist_3d(nsendlist_u(ndomains-1),  isendlist_u,  nsendlist_u,  Lnx, Lbot, kmxL1, nsendlist_u_3dw)
      deallocate(kmxL1)      

      ierror = 0
 1234 continue
 
      return
   end subroutine partition_make_ghostsendlists_3d
   
   
!> fill 3d ghost/send flow node/link list   
   subroutine partition_fill_ghostsendlist_3d(Nghost, ilist2d, nlist2d, N, kbot, kmxn, nlist3d)
      implicit none
      
      integer,                           intent(in)  :: Nghost  !< number of ghost/send flow nodes/links
      integer, dimension(Nghost),        intent(in)  :: ilist2d !< 2d ghost/send list
      integer, dimension(-1:ndomains-1), intent(in)  :: nlist2d !< cumulative number of flow nodes/links in 2d ghost/send list
      integer,                           intent(in)  :: N       !< number of flow nodes/links
      integer, dimension(N),             intent(in)  :: kbot    !< bottom layer indices
      integer, dimension(N),             intent(in)  :: kmxn    !< number of layers
      integer, dimension(-1:ndomains-1), intent(out) :: nlist3d !< 3d ghost/send list
      
      integer :: i, idmn, k, num
      
      nlist3d(-1) = 0
      do idmn=0,ndomains-1
!        add number of 3d ghost/send nodes/links from/to this domain
         num = 0
         do i=nlist2d(idmn-1)+1,nlist2d(idmn)
            k = iabs(ilist2d(i))
            num = num + kmxn(k)
         end do
         nlist3d(idmn) = nlist3d(idmn-1) + num
      end do
         
      return
   end subroutine partition_fill_ghostsendlist_3d
   
!> generate non-overlapping ghost/sendlists (for solver)
   subroutine partition_fill_ghostsendlist_nonoverlap(ierror)
      use m_alloc
      use m_flowgeom, only: Ndx
      implicit none
      
      integer,               intent(out) :: ierror   !< error (1) or not (0)
            
      integer                            :: i, idmn, iglev, inum, k, lenold
      
      ierror = 1
      
      if ( allocated(nghostlist_snonoverlap) ) deallocate(nghostlist_snonoverlap)
      if ( allocated(ighostlist_snonoverlap) ) deallocate(ighostlist_snonoverlap)
      
      if ( allocated(nsendlist_snonoverlap) ) deallocate(nsendlist_snonoverlap)
      if ( allocated(isendlist_snonoverlap) ) deallocate(isendlist_snonoverlap)
      
      allocate(nghostlist_snonoverlap(-1:ndomains-1))
      nghostlist_snonoverlap = 0
      allocate(nsendlist_snonoverlap(-1:ndomains-1))
      nsendlist_snonoverlap = 0
     
!     check for overlap (in solver)      
      if ( maxghostlev_s.eq.1 ) then
          jaoverlap = 0
          
          ierror = 0
          goto 1234
      end if
      
!     safety: allocate lists with full overlapping length
      lenold = ubound(ighostlist_sall,1)
      allocate(ighostlist_snonoverlap(lenold))
      ighostlist_snonoverlap = 0
      lenold = ubound(isendlist_sall,1)
      allocate(isendlist_snonoverlap(lenold))
      isendlist_snonoverlap = 0
      
      jaoverlap = 1
      
!     select nodes from ghostlist
      inum = 0
      do idmn=0,ndomains-1
         if ( idmn.eq.my_rank) cycle
         
         do i=nghostlist_sall(idmn-1)+1,nghostlist_sall(idmn)        
            k = ighostlist_sall(i)
            
!           get appropriate ghostlevel
            if ( ighosttype_s.eq.IGHOSTTYPE_CELLBASED ) then
               iglev = ighostlev_cellbased(k)
            else if ( ighosttype_s.eq.IGHOSTTYPE_NODEBASED ) then
               iglev = ighostlev_nodebased(k)
            else  ! combined
               iglev = ighostlev(k)
            end if
            
!!           only add ghost nodes with ghostlevel >= minghostlev_s            
!           only add ghost nodes with ghostlevel >= maxghostlev_s 
!            if ( iglev.ge.minghostlev_s .or. iglev.eq.0 ) then
            if ( iglev.ge.maxghostlev_s .or. iglev.eq.0 ) then
               nghostlist_snonoverlap(idmn) = nghostlist_snonoverlap(idmn) + 1
               inum = inum+1
               ighostlist_snonoverlap(inum) = k
            end if
         end do
      end do
      
!     make nghostlist_snonoverlap cumulative
      nghostlist_snonoverlap(-1) = 0
      do i=0,ndomains-1
         nghostlist_snonoverlap(i) = nghostlist_snonoverlap(i-1) + nghostlist_snonoverlap(i)
      end do
      
!     make sendlist
      call partition_make_sendlist_MPI(ITYPE_Snonoverlap, numlay_cellbased+1,numlay_nodebased+1)

      
!     (re)set number of ghost nodes/links 
      numghost_snonoverlap = nghostlist_snonoverlap(ndomains-1)
      numsend_snonoverlap = nsendlist_snonoverlap(ndomains-1)
      
!     safety: make dummy empty lists
      call realloc(nghostlist_snonoverlap, ndomains-1, -1, keepExisting=.true., fill=0)
      call realloc(nsendlist_snonoverlap,  ndomains-1, -1, keepExisting=.true., fill=0)
      
      call realloc(ighostlist_snonoverlap, max(numghost_snonoverlap,1), keepExisting=.true., fill=0)
      call realloc(isendlist_snonoverlap,  max(numsend_snonoverlap,1),  keepExisting=.true., fill=0)
      
!     check number of non-overlapping ghost nodes
!      if ( numghost_snonoverlap .ne. numghost_sall-noverlap ) then
!         call mess(LEVEL_ERROR, 'partition_fill_ghostsendlist_nonoverlap gave error')
!         goto 1234
!      end if
      
!!     BEGIN DEBUG
!      allocate(idum(Ndx))
!      idum = 0
!      
!      do i=1,noverlap
!         k = ioverlap(i)
!         if ( idum(k).ne.0 ) then
!            continue
!         end if
!         idum(k) = i
!      end do
!      
!      do i=1,numghost_snonoverlap
!         k = ighostlist_snonoverlap(i)
!         if ( idum(k).ne.0 ) then
!            write(6,*) i
!            continue
!         else
!            idum(k) = 1
!         end if
!      end do
!      
!      do i=1,numghost_sall
!         k = ighostlist_sall(i)
!         if ( idum(k).eq.0 ) then
!            continue
!         end if
!         idum(k) = 0
!      end do
!      
!      do k=1,Ndx
!         if ( idum(k).gt.0 ) then
!            write (6,*) idum(k)
!            continue
!         end if
!      end do
!!     END DEBUG

         
      ierror = 0
 1234 continue    
      
      return
    end subroutine partition_fill_ghostsendlist_nonoverlap
   
   
   subroutine update_ghosts(itype, NDIM, N, var, ierror)
#ifdef HAVE_MPI   
      use mpi
#endif      
      use m_flowgeom
      use m_flow, only: kmx, kmxn, kmxL, kbot, Lbot, Ndkx, Lnkx
      use messageHandling

      implicit none

      integer,                                     intent(in)    :: itype        !< type: 0: flownode, 1: flowlink
      integer,                                     intent(in)    :: NDIM         !< number of unknowns per flownode/link
      integer,                                     intent(in)    :: N            !< number of flownodes/links
      double precision, dimension(NDIM*N),         intent(inout) :: var          !< solution
      integer,                                     intent(out)   :: ierror       !< error (1) or not (0)
      

      
      ierror = 1
      
      if ( .not.allocated(isendlist_sall) ) goto 1234   ! safety
      
      if ( itype.eq.ITYPE_S ) then
         if ( N.ne.Ndx) then
            call qnerror('update_ghosts, ITYPE_S: numbering error', ' ', ' ')
            goto 1234
         end if
         call update_ghost_loc(ndomains, NDIM, N, var, nghostlist_s(ndomains-1), ighostlist_s, nghostlist_s, nsendlist_s(ndomains-1), isendlist_s, nsendlist_s, itag_s, ierror)
      else if ( itype.eq.ITYPE_SALL ) then
         if ( N.ne.Ndx) then
            call qnerror('update_ghosts, ITYPE_Sall: numbering error', ' ', ' ')
            goto 1234
         end if
         call update_ghost_loc(ndomains, NDIM, N, var, nghostlist_sall(ndomains-1), ighostlist_sall, nghostlist_sall, nsendlist_sall(ndomains-1), isendlist_sall, nsendlist_sall, itag_sall, ierror)
      else if ( itype.eq.ITYPE_U ) then
         if ( N.ne.Lnx) then
            call qnerror('update_ghosts, ITYPE_U: numbering error', ' ', ' ')
            goto 1234
         end if
         call update_ghost_loc(ndomains, NDIM, N, var, nghostlist_u(ndomains-1), ighostlist_u, nghostlist_u, nsendlist_u(ndomains-1), isendlist_u, nsendlist_u, itag_u, ierror)
!
!     3D-extension         
      else if ( itype.eq.ITYPE_S3D ) then
         if ( N.ne.Ndkx) then
            call qnerror('update_ghosts, ITYPE_S3D: numbering error', ' ', ' ')
            goto 1234
         end if
         call update_ghost_loc(ndomains, NDIM, N, var, nghostlist_s(ndomains-1), ighostlist_s, nghostlist_s, nsendlist_s(ndomains-1), isendlist_s, nsendlist_s, itag_s, ierror, nghostlist_s_3D, nsendlist_s_3D, kmxn, kbot)
      else if ( itype.eq.ITYPE_SALL3D ) then
         if ( N.ne.Ndkx) then
            call qnerror('update_ghosts, ITYPE_Sall3D: numbering error', ' ', ' ')
            goto 1234
         end if
         call update_ghost_loc(ndomains, NDIM, N, var, nghostlist_sall(ndomains-1), ighostlist_sall, nghostlist_sall, nsendlist_sall(ndomains-1), isendlist_sall, nsendlist_sall, itag_sall, ierror, nghostlist_sall_3D, nsendlist_sall_3D, kmxn, kbot)
      else if ( itype.eq.ITYPE_U3D ) then
         if ( N.ne.Lnkx) then
            call qnerror('update_ghosts, ITYPE_U3D: numbering error', ' ', ' ')
            goto 1234
         end if
!         call update_ghost_loc(ndomains, NDIM, N, var, nghostlist_u(ndomains-1), ighostlist_u, nghostlist_u, nsendlist_u(ndomains-1), isendlist_u, nsendlist_u, itag_u, ierror, nghost3d=nghostlist_u_3D, nsend3d=nsendlist_u_3D, kmxnL=kmxL, kbot=Lbot)
         call update_ghost_loc(ndomains, NDIM, N, var, nghostlist_u(ndomains-1), ighostlist_u, nghostlist_u, nsendlist_u(ndomains-1), isendlist_u, nsendlist_u, itag_u, ierror, nghostlist_u_3D, nsendlist_u_3D, kmxL, Lbot)
       else if ( itype.eq.ITYPE_U3DW ) then
         if ( N.ne.Lnkx) then
            call qnerror('update_ghosts, ITYPE_U3DW: numbering error', ' ', ' ')
            goto 1234
         end if
         call update_ghost_loc(ndomains, NDIM, N, var, nghostlist_u(ndomains-1), ighostlist_u, nghostlist_u, nsendlist_u(ndomains-1), isendlist_u, nsendlist_u, itag_u, ierror, nghostlist_u_3Dw, nsendlist_u_3Dw, kmxL+1, Lbot-1)
                 
!     overlap
      else if ( itype.eq.ITYPE_Snonoverlap ) then
         if ( N.ne.Ndx ) then
            call qnerror('update_ghosts, ITYPE_Snonoverlap: numbering error', ' ', ' ')
            goto 1234
         end if
         if ( jaoverlap.eq.1 ) then
            call update_ghost_loc(ndomains, NDIM, N, var, nghostlist_snonoverlap(ndomains-1), ighostlist_snonoverlap, nghostlist_snonoverlap, nsendlist_snonoverlap(ndomains-1), isendlist_snonoverlap, nsendlist_snonoverlap, itag_snonoverlap, ierror)
         else  ! no overlap: use sall
            call update_ghost_loc(ndomains, NDIM, N, var, nghostlist_sall(ndomains-1), ighostlist_sall, nghostlist_sall, nsendlist_sall(ndomains-1), isendlist_sall, nsendlist_sall, itag_sall, ierror)
         end if
      else
         call qnerror('update_ghosts: unknown ghost type', ' ', ' ')
      end if
      
      ierror = 0
 1234 continue
 
      if ( ierror /= 0 ) call mess(LEVEL_ERROR, 'update_ghosts gave error')
      
      return
   end subroutine update_ghosts

     
!> update ghost values
!>   3D extension: we assume that kbot/Lbot and kmxn/kmxL match their counterparts in the other domain(s)
   subroutine update_ghost_loc(ndomains, NDIM, N, s, numghost, ighost, nghost, numsend, isend, nsend, itag, ierror, nghost3d, nsend3d, kmxnL, kbot)
#ifdef HAVE_MPI   
      use mpi
#endif      
      use m_flowgeom
      use m_alloc

      implicit none

      integer,                                     intent(in)     :: ndomains        !< number of subdomains
      integer,                                     intent(in)     :: NDIM            !< number of unknowns per flownode/link
      integer,                                     intent(in)     :: N               !< number of flownodes/links
      double precision, dimension(NDIM*N),         intent(inout)  :: s               !< solution
      integer,                                     intent(in)     :: numghost        !< number of ghost nodes/links
      integer,          dimension(numghost),       intent(in)     :: ighost          !< ghost nodes/links
      integer,          dimension(-1:ndomains-1),  intent(in)     :: nghost          !< ghost list pointers
      integer,                                     intent(in)     :: numsend         !< number of send nodes/links
      integer,          dimension(numsend),        intent(in)     :: isend           !< ghost nodes/links
      integer,          dimension(-1:ndomains-1),  intent(in)     :: nsend           !< ghost list pointers
      integer,                                     intent(in)     :: itag            !< message tag
      integer,                                     intent(out)    :: ierror          !< error (1) or not (0)
!     3D-extension
      integer,          dimension(-1:ndomains-1),  intent(in), optional :: nghost3d  !< number of unknowns to be received per domain
      integer,          dimension(-1:ndomains-1),  intent(in), optional :: nsend3d   !< number of unknowns to be send     per domain
      integer,          dimension(N),              intent(in), optional :: kmxnL     !< number of layers
      integer,          dimension(N),              intent(in), optional :: kbot      !< bottom layer indices

!      double precision, dimension(:), allocatable                :: work         ! work array
#ifdef HAVE_MPI
      integer,          dimension(MPI_STATUS_SIZE)                :: istat

      integer,          dimension(ndomains)                       :: irequest

      integer                                                     :: other_rank, ierr, i, ii, ib, it, nreq
      integer                                                     :: i2d, istart, iend, icount, num
      integer                                                     :: j
      
      integer                                                     :: ja3d   ! 3D (1) or not (0)

      integer,          parameter                                 :: INIWORKSIZE=1000
!      double precision, parameter                                 :: DNOCELL = -1234.5678
      
      character(len=1024)                                         :: str
#endif
      ierror = 1
#ifdef HAVE_MPI

!     check for 3D
      ja3d = 0
      if ( present(nghost3d) .and. present(nsend3d) .and. present(kmxnL) .and. present(kbot) ) then
         ja3d = 1
      end if
         
!     allocate work array (will be reallocated later)
      if ( .not.allocated(work) ) allocate(work(INIWORKSIZE))
      
      if ( ja3d.ne.1 ) then
         num = nsend(ndomains-1)*NDIM
      else
         num = nsend3d(ndomains-1)*NDIM
      end if
      
      if ( ubound(work,1).lt.num ) then
         call realloc(work, int(1.2d0*dble(num)+1d0))
      end if
      
!     fill work array
      if ( ja3d.ne.1 ) then
         if ( NDIM.eq.1 ) then
            do i=1,nsend(ndomains-1)
!               if ( isend(i).gt.0 ) then
                  work(i) = s(isend(i))
!               else  ! if ( isend(i).lt.0 ) then
!                  work(i) = -s(-isend(i))
!!               else  ! no cell was found during the handshake, send missing value
!!                  work(i) = DNOCELL
!               end if
            end do
         else  ! NDIM.ne.1
            do i=1,nsend(ndomains-1)
!               if ( isend(i).gt.0 ) then
                  do j=1,NDIM
                     work(NDIM*(i-1)+j) = s(NDIM*(isend(i)-1)+j)
                  end do
!               else  ! if ( isend(i).lt.0 ) then
!                  do j=1,NDIM
!                     work(NDIM*(i-1)+j) = -s(NDIM*(-isend(i)-1)+j)
!                  end do
!               else  ! no cell was found during the handshake, send missing value
!                  do j=1,NDIM
!                     work(NDIM*(i-1)+j) = DNOCELL
!                  end do
!               end if
            end do
         end if
      else  ! 3D extension
         if ( NDIM.eq.1 ) then
            icount = 0
            do i=1,nsend(ndomains-1)
               i2d = iabs(isend(i))
               ib = kbot(i2d)
               it = ib + kmxnL(i2d) - 1   ! override it
               do ii=ib,it
                  icount = icount+1
!                  if ( isend(i).gt.0 ) then
                     do j=1,NDIM
                        work(icount) = s(ii)
                     end do
!                  else !   if ( isend(i).lt.0 ) then
!                     do j=1,NDIM
!                        work(icount) = -s(-ii)
!                     end do
!                  else    ! no cell was found during the handshake, send missing value
!                     do j=1,NDIM
!                        work(icount) = DNOCELL
!                     end do
!                  end if
               end do   ! do i=ib,it
            end do
         else  ! NDIM.ne.1
            icount = 0
            do i=1,nsend(ndomains-1)
               i2d = iabs(isend(i))
               ib = kbot(i2d)
               it = ib + kmxnL(i2d) - 1   ! override it
               do ii=ib,it
                  icount = icount+1
 !                 if ( isend(i).gt.0 ) then
                     do j=1,NDIM
                        work(NDIM*(icount-1)+j) = s(NDIM*(ii-1)+j)
                     end do
!                  else  ! if ( isend(i).lt.0 ) then
!                     do j=1,NDIM
!                        work(NDIM*(icount-1)+j) = -s(NDIM*(-ii-1)+j)
!                     end do
!                  else    ! no cell was found during the handshake, send missing value
!                     do j=1,NDIM
!                        work(NDIM*(icount-1)+j) = DNOCELL
!                     end do
!                  end if
               end do   ! do i=ib,it
            end do
         end if
         if ( icount.ne.nsend3d(ndomains-1) ) then
            call qnerror('update_ghost_loc: 3d numbering error', ' ', ' ')
         end if
      end if

!     we need to make sure that all processes are at this point right now, since we are using one global work array for sending and receiving
      call mpi_barrier(DFM_COMM_DFMWORLD,ierr)

!     send
      if ( ja3d.eq.0 ) then
         nreq = 0
         do other_rank=0,ndomains-1
            istart = NDIM*nsend(other_rank-1)+1 ! nsend(other_rank-1)+1
            iend   = NDIM*nsend(other_rank)     ! nsend(other_rank)
            num    = iend-istart+1
         
            if ( num.gt.0 ) then
               nreq = nreq+1
               call mpi_isend(work(istart),num,mpi_double_precision,other_rank,itag,DFM_COMM_DFMWORLD,irequest(nreq),ierr)
            end if
         end do
      else
         nreq = 0
         do other_rank=0,ndomains-1
            istart = NDIM*nsend3d(other_rank-1)+1 ! nsend3d(other_rank-1)+1
            iend   = NDIM*nsend3d(other_rank)     ! nsend3d(other_rank)
            num    = iend-istart+1
         
            if ( num.gt.0 ) then
!               write(6, "('update_ghost_loc:    send, domain: ', I3, ', other domain: ', I3, ', num: ', I7)") my_rank, other_rank, num
               nreq = nreq+1
               call mpi_isend(work(istart),num,mpi_double_precision,other_rank,itag,DFM_COMM_DFMWORLD,irequest(nreq),ierr)
            end if
         end do
      end if

!     recieve
!     allocate work array (will be reallocated later)
      if ( .not.allocated(workrec) ) allocate(workrec(INIWORKSIZE))
      
      if ( ja3d.ne.1 ) then
         num = NDIM*nghost(ndomains-1)
      else
         num = NDIM*nghost3d(ndomains-1)
      end if
      
      if ( ubound(workrec,1).lt.num ) then
         call realloc(workrec,int(1.2d0*dble(num)+1d0))
      end if
      
      if ( ja3d.ne.1 ) then
         do other_rank=0,ndomains-1
            istart = NDIM*nghost(other_rank-1)+1 ! nghost(other_rank-1)+1
            iend   = NDIM*nghost(other_rank)     ! nghost(other_rank)
            num    = iend-istart+1
            if ( num.gt.0 ) then
               !call mpi_recv(s(ighost(istart)),num,mpi_double_precision,other_rank,itag,DFM_COMM_DFMWORLD,istat,ierr)
            
   !           ghost cells are NOT ordered
               call mpi_recv(workrec(istart), num, mpi_double_precision, other_rank, itag, DFM_COMM_DFMWORLD, istat, ierr)

   !           check message size
               call mpi_get_count(istat, mpi_double_precision, icount, ierr)
               if ( icount.ne.num ) then
                  write(str, *) 'update_ghost_loc: icount.ne.num, domain: ', my_rank, ', other domain: ', other_rank, ' icount: ', icount, ', num: ', num
                  call qnerror(str, ' ', ' ')
                  goto 1234
               end if
            end if
         end do
      else
         do other_rank=0,ndomains-1
            istart = NDIM*nghost3d(other_rank-1)+1 ! nghost3d(other_rank-1)+1
            iend   = NDIM*nghost3d(other_rank)     ! nghost3d(other_rank)
            num    = iend-istart+1
            
            if ( num.gt.0 ) then
               !call mpi_recv(s(ighost(istart)),num,mpi_double_precision,other_rank,itag,DFM_COMM_DFMWORLD,istat,ierr)
            
!               write(6, "('update_ghost_loc: recieve, domain: ', I3, ', other domain: ', I3, ', num: ', I7)") my_rank, other_rank, num
               
   !           ghost cells are NOT ordered
               call mpi_recv(workrec(istart), num, mpi_double_precision, other_rank, itag, DFM_COMM_DFMWORLD, istat, ierr)

   !           check message size
               call mpi_get_count(istat, mpi_double_precision, icount, ierr)
               if ( icount.ne.num ) then
                  write(str, *) 'update_ghost_loc: icount.ne.num, domain: ', my_rank, ', other domain: ', other_rank, ' icount: ', icount, ', num: ', num
                  call qnerror(str, ' ', ' ')
                  goto 1234
               end if
            end if
         end do
      end if
      
!     copy work array to ghost nodes
      if ( ja3d.ne.1 ) then
         if ( NDIM.eq.1 ) then
            do i=1,nghost(ndomains-1)
!               if ( workrec(i).ne.DNOCELL ) then
            
                if ( ighost(i).gt.0 ) then
                   s(ighost(i)) = workrec(i)
                else if ( ighost(i).lt.0 ) then
                   s(-ighost(i)) = -workrec(i)
                end if
                  
!               else  ! no cell was found during handshake
!!                 do nothing
!                  continue
!               end if
            end do
         else  ! NDIM.ne.1
            do i=1,nghost(ndomains-1)
!               if ( workrec(NDIM*(i-1)+1).ne.DNOCELL ) then ! check first element only
                  if ( ighost(i).gt.0 ) then
                     do j=1,NDIM
                        s(NDIM*(ighost(i)-1)+j) = workrec(NDIM*(i-1)+j)
                     end do
                  else if ( ighost(i).lt.0 ) then
                     do j=1,NDIM
                        s(NDIM*(-ighost(i)-1)+j) = -workrec(NDIM*(i-1)+j)
                     end do
                  end if
!               else  ! no cell was found during handshake
!!                 do nothing
!               end if
            end do
         end if
      else  ! 3D extension, we assume that bot and top matches the ibotsend in the other subdomain(s)
         icount = 0
         if ( NDIM.eq.1 ) then
            do i=1,nghost(ndomains-1)
               if ( ighost(i).gt.0 ) then
                  i2d = ighost(i)
                  ib = kbot(i2d)
                  it = ib + kmxnL(i2d) - 1   ! override it
                  do ii=ib,it
                     icount = icount+1
   !                  if ( workrec(icount).ne.DNOCELL ) then
                        s(ii) = workrec(icount)
   !                  else  ! no cell was found during handshake
   !!                    do nothing
   !                  end if
                  end do
               else if ( ighost(i).lt.0 ) then
                  i2d = -ighost(i)
                  ib = kbot(i2d)
                  it = ib + kmxnL(i2d) - 1   ! override it
                  do ii=ib,it
                     icount = icount+1
   !                  if ( workrec(icount).ne.DNOCELL ) then
                        s(ii) = -workrec(icount)
   !                  else  ! no cell was found during handshake
   !!                    do nothing
   !                  end if
                  end do
               end if
            end do
         else  ! NDIM.ne.1
            do i=1,nghost(ndomains-1)
               if ( ighost(i).gt.0 ) then
                  i2d = ighost(i)
                  ib = kbot(i2d)
                  it = ib + kmxnL(i2d) - 1   ! override it
                  do ii=ib,it
                     icount = icount+1
   !                  if ( workrec(NDIM*(icount-1)+j).ne.DNOCELL ) then  ! check first element only
                        do j=1,NDIM
                           s(NDIM*(ii-1)+j) = workrec(NDIM*(icount-1)+j)
                        end do
   !                  else  ! no cell was found during handshake
   !!                    do nothing
   !                  end if
                  end do
               else if ( ighost(i).lt.0 ) then
                  i2d = -ighost(i)
                  ib = kbot(i2d)
                  it = ib + kmxnL(i2d) - 1   ! override it
                  do ii=ib,it
                     icount = icount+1
   !                  if ( workrec(NDIM*(icount-1)+j).ne.DNOCELL ) then  ! check first element only
                        do j=1,NDIM
                           s(NDIM*(ii-1)+j) = -workrec(NDIM*(icount-1)+j)
                        end do
   !                  else  ! no cell was found during handshake
   !!                    do nothing
   !                  end if
                  end do
               end if
            end do
         end if
         
         if ( icount.ne.nghost3d(ndomains-1) ) then
            call qnerror('update_ghost_loc: 3d numbering error', ' ', ' ')
         end if
      end if

!     terminate send (safety)
      do i=1,nreq
         call mpi_wait(irequest(i),istat,ierr)
      end do

      ierror = 0
1234  continue

!     deallocate
!      if ( allocated(work) ) deallocate(work)
#endif
      return
   end subroutine update_ghost_loc


!> Makes the global flow node numbers for this domain's own flow nodes and links.
!! These numbers for all partitions together will form a continuous list
!! of flow node/link numbers, which could be used for aggregation of
!! partitioned output files.
subroutine partition_make_globalnumbers(ierror)
   use m_flowgeom, only: Ndxi, Ndx, Ln, Lnxi, Lnx
   use m_alloc
   use unstruc_messages
   implicit none

   integer,                           intent(out) :: ierror
   
   integer                                        :: i, istat, k, numlist
   integer                                        :: L, ki
   integer, dimension(:), allocatable             :: inums
   integer, dimension(:), allocatable             :: numcells_s, idum
   
   integer                                        :: Ndxi_glob
   
   ierror = 1

   allocate(inums(Ndx), stat=istat)
   if ( istat.ne.0 ) then
      goto 1234
   end if
   
   allocate(idum(Ndx), stat=istat)
   if ( istat.ne.0 ) then
      goto 1234
   end if
   
!  initialize   
   inums = 0
   idum = 0

! Get global numbers for this domain's own internal flow nodes

   if ( .not.allocated(iglobal_s) ) then
!     set internal numbers (non-boundary nodes)
      numlist = 0
      do k=1,Ndxi
         if ( idomain(k).eq.my_rank ) then
            numlist = numlist+1
            inums(numlist)  = k
         end if
      end do
      
      call get_global_numbers(numlist, inums(1:Ndxi), iglobal_s, numcells_s, 0)
      
!     get the number of global cells (global Ndxi)
      Ndxi_glob = sum(numcells_s(0:ndomains-1))
   else
!     interal (2D) cells already read from file
      Ndxi_glob = Nglobal_s
      
      call realloc(iglobal_s, Ndx, keepExisting=.true., fill=0)
   end if
   
   
!  set boundary numbers
!  first add non-ghost boundary nodes to the list
   numlist = 0
   do L=Lnxi+1,Lnx
!     find boundary node (>Ndxi)
      k  = max(ln(1,L), ln(2,L))
!     find connected internal node (<=Ndxi)
      ki = min(ln(1,L), ln(2,L))
!     only add boundary node if the connected internal node is not a ghost node
      if ( idomain(ki).eq.my_rank ) then
         numlist = numlist + 1
         inums(numlist) = k
      end if
   end do
   
!  get the global numbering of the boundary nodes
   call get_global_numbers(numlist, inums(1:numlist), idum, numcells_s, 0)
   
!  add boundary nodes to global numbering
   do i=1,numlist
      k=inums(i)
      
!     check if this node did not already have a global number assigned
      if ( iglobal_s(k).ne.0 ) then
         call mess(LEVEL_ERROR, 'partition_make_globalnumbers: numbering error')
         goto 1234
      end if
      
      iglobal_s(k) = Ndxi_glob + idum(k)
   end do

   ierror = 0
1234 continue

   if ( allocated(inums) ) deallocate(inums)
   if ( allocated(idum)  ) deallocate(idum)

   return
end subroutine partition_make_globalnumbers


!>  get the global cell numbering for a given list of local flow node numbers (e.g., CG flow nodes only)
!!
!! The iactive list could contain only the CG flow nodes, or for example, only the ndxi internal flow nodes.
!! Results are stored in the iglobnum array, which is typically the 'iglobal' array.
   subroutine get_global_numbers(numactive,iactive, iglobnum, numglobcells, jatime) 
      use m_flowgeom,      only : Ndx
#ifdef HAVE_MPI
      use mpi
#endif
      use m_timer
      use m_flowgeom, only: xz, yz

      implicit none

      integer,                                    intent(in)  :: numactive !< number of active cells
      integer, dimension(numactive),              intent(in)  :: iactive   !< The flow node numbers of the active cells.
      integer, dimension(:),         allocatable, intent(out) :: iglobnum  !< Target array in which the global numbers will be stored. 
      integer, dimension(:),         allocatable, intent(out) :: numglobcells !< number of global cells per subdomain, dim(0:ndomains-1)
      integer,                                    intent(in)  :: jatime    !< time MPI communication (1) or not (0)

#ifdef HAVE_MPI      
      double precision, dimension(:), allocatable :: dum

      integer                                     :: i, n, num
      integer                                     :: ierror

      ierror = 1
      
      if ( allocated(iglobnum) )     deallocate(iglobnum)
      if ( allocated(numglobcells) ) deallocate(numglobcells)

!     allocate
      allocate(iglobnum(Ndx))
      allocate(numglobcells(0:max(ndomains-1,0)))
      allocate(dum(Ndx))
      
!     mark active cells
      iglobnum = 0
      do i=1,numactive
         iglobnum(iactive(i)) = 1
      end do
      
      if ( jampi.eq.1 ) then
         if ( jaoverlap.eq.0 ) then
   !        unmark ghost cells
            do i=1,numghost_sall
               iglobnum(ighostlist_sall(i)) = 0
            end do
         else
   !        unmark non-overlapping ghost cells
            do i=1,numghost_snonoverlap
               iglobnum(ighostlist_snonoverlap(i)) = 0
            end do
         end if
      
!!        unmark ghost cells, alternative based on ghost levels
!         do i=1,Ndx
!            if ( ighostlev(i).ge.minghostlev_s ) then
!               iglobnum(i) = 0
!            end if
!         end do
         
!        compute number of active non-ghost cells
         num = count(iglobnum.eq.1)
!         write(6,*) 'my_rank', my_rank, 'num=', num, 'numghosts_sall=', numghost_sall
         
!        communicate active non-ghost cell numbers
         
         if ( jatime.eq.1 ) call starttimer(IMPICOMM)
         call mpi_allgather(num, 1, MPI_INTEGER, numglobcells, 1, MPI_INTEGER, DFM_COMM_DFMWORLD, ierror)
         if ( jatime.eq.1 ) call stoptimer(IMPICOMM)
         
!        compute global cell numbers for own non-ghost cells
         num = 0
         if ( my_rank.gt.0 ) then
            num = sum(numglobcells(0:my_rank-1))
         end if
      else  ! jampi.eq.0
         numglobcells(0) = numactive
         num = 0
      end if 

      do i=1,numactive
         n = iactive(i)
         if ( iglobnum(n).ne.0 ) then
            num = num+1
            iglobnum(n) = num
         end if
      end do

      if ( jampi.eq.1 ) then
!        update global ghost-cell numbers
         dum = dble(iglobnum)
         if ( jatime.eq.1 ) call starttimer(IMPICOMM)
         !call update_ghost(dum,ierror)
         if ( jampi.eq.1 ) then
            if ( jaoverlap.eq.0 ) then
               call update_ghosts(ITYPE_Sall, 1, Ndx, dum, ierror)
            else
               call update_ghosts(ITYPE_Snonoverlap, 1, Ndx, dum, ierror)
            end if
         end if
         if ( jatime.eq.1 ) call stoptimer(IMPICOMM)
         
         iglobnum = int(dum)
      
!!        fill in global cell number of masked cells      
!         do i=1,Ndx
!            if ( iglobnum(i).eq.0 ) then
!               iglobnum(i) = int(dum(i))
!            end if
!         end do
       end if
      
!      open(6666,file='globalnrs_'//sdmn//'.xyz')
!      do i=1,Ndx
!         write(6666,"(2E15.7, I7)") xz(i), yz(i), iglobnum(i)
!      end do
!      close(6666)

      ierror = 0
 1234 continue

!     deallocate
      if ( allocated(dum) )      deallocate(dum)
      
#endif

      return
   end subroutine get_global_numbers
   
!!> reduce time step
!   subroutine reduce_dt()
!      use m_flowtimes, only: dts, dti
!
!      implicit none
!      
!      call reduce_double_min(dts)    
!
!      return
!   end subroutine reduce_dt
   
!> reduce a double, take global min
   subroutine reduce_double_min(var)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      double precision, intent(inout) :: var !< variable
#ifdef HAVE_MPI
      
      double precision       :: var_all
   
      integer                :: ierror

      call mpi_allreduce(var,var_all,1,mpi_double_precision,mpi_min,DFM_COMM_DFMWORLD,ierror)
      var = var_all
#endif

      return
   end subroutine reduce_double_min
   
!> reduce a double, take global max
   subroutine reduce_double_max(var)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      double precision, intent(inout) :: var !< variable
#ifdef HAVE_MPI
      
      double precision       :: var_all
   
      integer                :: ierror

      call mpi_allreduce(var,var_all,1,mpi_double_precision,mpi_max,DFM_COMM_DFMWORLD,ierror)
      var = var_all
#endif

      return
   end subroutine reduce_double_max
   
!> reduce a double, take global sum
   subroutine reduce_double_sum(N, varin, varout)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      integer,                        intent(in)  :: N      !< array size
      double precision, dimension(N), intent(in)  :: varin  !< variable
      double precision, dimension(N), intent(out) :: varout !< reduced variable
#ifdef HAVE_MPI

      integer                                     :: ierror
      
!      write(6,*) 'my_rank=', my_rank, 'N=', N
      
      call mpi_allreduce(varin,varout,N,mpi_double_precision,mpi_sum,DFM_COMM_DFMWORLD,ierror)
#endif

      return
   end subroutine reduce_double_sum

   
   
!> reduce a double array, take global max
   subroutine reduce_double2_max(var1, var2)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      double precision, intent(inout) :: var1 !< variables
      double precision, intent(inout) :: var2 !< variables
#ifdef HAVE_MPI
      
      double precision, dimension(2)  :: dum, var_all
   
      integer                :: ierror

      dum = (/var1, var2/)
      call mpi_allreduce(dum,var_all,2,mpi_double_precision,mpi_max,DFM_COMM_DFMWORLD,ierror)
      var1 = var_all(1)
      var2 = var_all(2)
#endif

      return
   end subroutine reduce_double2_max
   
   
!> reduce a double array, take global max
   subroutine reduce_double3_max(var1, var2, var3)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      double precision, intent(inout) :: var1 !< variables
      double precision, intent(inout) :: var2 !< variables
      double precision, intent(inout) :: var3 !< variables
#ifdef HAVE_MPI
      
      double precision, dimension(3)  :: dum, var_all
   
      integer                :: ierror

      dum = (/var1, var2, var3/)
      call mpi_allreduce(dum,var_all,3,mpi_double_precision,mpi_max,DFM_COMM_DFMWORLD,ierror)
      var1 = var_all(1)
      var2 = var_all(2)
      var3 = var_all(3)
#endif

      return
   end subroutine reduce_double3_max
   
!> reduce an integer array, take global max
   subroutine reduce_int4_max(var1, var2, var3, var4)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      integer, intent(inout) :: var1 !< variables
      integer, intent(inout) :: var2 !< variables
      integer, intent(inout) :: var3 !< variables
      integer, intent(inout) :: var4 !< variables
#ifdef HAVE_MPI
      
      integer, dimension(4)  :: dum, var_all
   
      integer                :: ierror

      dum = (/var1, var2, var3, var4/)
      call mpi_allreduce(dum,var_all,4,mpi_integer,mpi_max,DFM_COMM_DFMWORLD,ierror)
      var1 = var_all(1)
      var2 = var_all(2)
      var3 = var_all(3)
      var4 = var_all(4)
#endif

      return
   end subroutine reduce_int4_max
   
   !> reduce an integer, take global sum
   subroutine reduce_int_sum(varin,varout)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      integer, intent(in)  :: varin
      integer, intent(out) :: varout
      
#ifdef HAVE_MPI
      integer                :: ierror

      call mpi_allreduce(varin,varout,1,mpi_integer,mpi_sum,DFM_COMM_DFMWORLD,ierror)
#endif

      return
   end subroutine reduce_int_sum
   
!> reduce an array of integers, take global sum
   subroutine reduce_intN_sum(N, varin, varout)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      integer,               intent(in)  :: N      !< array size
      integer, dimension(N), intent(in)  :: varin  !< variable
      integer, dimension(N), intent(out) :: varout !< reduced variable
#ifdef HAVE_MPI

      integer                                     :: ierror
      
!      write(6,*) 'my_rank=', my_rank, 'N=', N
      
      call mpi_allreduce(varin,varout,N,mpi_integer,mpi_sum,DFM_COMM_DFMWORLD,ierror)
#endif

      return
   end subroutine reduce_intN_sum

   !> for an array over integers, take global sum over all subdomains (not over the array itself)
   subroutine reduce_int_array_sum(N, var)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      integer,                        intent(in)    :: N  !< array size
      integer, dimension(N),          intent(inout) :: var !< array with values to be summed over the subdomains (not an array summation)
      
      integer, dimension(N)                         :: dum
      
      integer :: ierror
      
#ifdef HAVE_MPI
      call MPI_allreduce(var,dum,N,mpi_integer,mpi_sum,DFM_COMM_DFMWORLD,ierror)
#endif
      var = dum
      return
   end subroutine reduce_int_array_sum
    
!> reduce key (also used for nonlin in setprofs1D)
!>   take maximum
   subroutine reduce_key(key)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      integer, intent(inout) :: key !< key
#ifdef HAVE_MPI
      
      integer                :: key_all
   
      integer                :: ierror
      if (jampi == 1) then
   !     take maximum key of all domains
         call mpi_allreduce(key,key_all,1,mpi_integer,mpi_max,DFM_COMM_DFMWORLD,ierror)
         key = key_all
      else
         ! No reduction: key is the only key.
         ! NOTE: mpi_allreduce is unpredictable when dflowfm is a library (no exe), when jampi==0 (uninitialized result).
         continue
      end if

#endif

      return
   end subroutine reduce_key
   
!> sum at_all for q-boundaries
   subroutine reduce_at_all()
      use  m_flowexternalforcings
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      integer :: ierror
      
#ifdef HAVE_MPI
      call MPI_allreduce(at_all,at_sum,nqbnd,mpi_double_precision,mpi_sum,DFM_COMM_DFMWORLD,ierror)
      at_all = at_sum
#endif      
      return
   end subroutine reduce_at_all
   
!> sum wwssav_all for q-boundaries
   subroutine reduce_wwssav_all()
      use  m_flowexternalforcings
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      integer :: ierror
      
#ifdef HAVE_MPI
      call MPI_allreduce(wwssav_all,wwssav_sum,2*nqbnd,mpi_double_precision,mpi_sum,DFM_COMM_DFMWORLD,ierror)
      wwssav_all = wwssav_sum
#endif      
      return
   end subroutine reduce_wwssav_all
   
!> sum atqh_all for qh-boundaries
   subroutine reduce_atqh_all()
      use  m_flowexternalforcings
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      integer :: ierror

      if (nqhbnd .eq. 0) then 
         return
      end if
#ifdef HAVE_MPI
      call MPI_allreduce(atqh_all,atqh_sum,nqhbnd,mpi_double_precision,mpi_sum,DFM_COMM_DFMWORLD,ierror)
      atqh_all = atqh_sum
#endif      
      return
   end subroutine reduce_atqh_all
   
   !> sum an array over all subdomains (not over the array itself)
   subroutine reduce_sum(N, var)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      integer,                        intent(in)    :: N  !< array size
      double precision, dimension(N), intent(inout) :: var !< array with values to be summed over the subdomains (not an array summation)
      
      double precision, dimension(N)                :: dum
      
      integer :: ierror
      
#ifdef HAVE_MPI
      call MPI_allreduce(var,dum,N,mpi_double_precision,mpi_sum,DFM_COMM_DFMWORLD,ierror)
      var = dum
#endif      
      return
   end subroutine reduce_sum
   
   !> take maximum integer over all subdomains
   subroutine reduce_int1_max(var)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      integer, intent(inout) :: var !< array with values to be summed over the subdomains (not an array summation)
      
      integer                :: dum
      
      integer                :: ierror
      
#ifdef HAVE_MPI
      call MPI_allreduce(var,dum,1,mpi_integer,mpi_max,DFM_COMM_DFMWORLD,ierror)
      var = dum
#endif      
      return
   end subroutine reduce_int1_max
   
   
   
   !> for an array over integers, take maximum over all subdomains (not over the array itself)
   subroutine reduce_int_max(N, var)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      integer,                        intent(in)    :: N  !< array size
      integer, dimension(N),          intent(inout) :: var !< array with values to be summed over the subdomains (not an array summation)
      
      integer, dimension(N)                         :: dum
      
      integer :: ierror
      
#ifdef HAVE_MPI
      call MPI_allreduce(var,dum,N,mpi_integer,mpi_max,DFM_COMM_DFMWORLD,ierror)
      var = dum
#endif      
      return
   end subroutine reduce_int_max
     

!> reduce an integer, take global min
   subroutine reduce_int_min(var)
#ifdef HAVE_MPI
      use mpi
#endif
      implicit none
      
      integer, intent(inout) :: var !< array with values to be summed over the subdomains (not an array summation)
      
      integer                :: dum
      
      integer                :: ierror
      
#ifdef HAVE_MPI
      call MPI_allreduce(var,dum,1,mpi_integer,mpi_min,DFM_COMM_DFMWORLD,ierror)
      var = dum
#endif      
      return
   end subroutine reduce_int_min
   
   
!> reduce observation stations
!>   in principle only one subdomain may own the observation station
!>   however, for observation stations that are on equal distance from the
!>     two nearest flow nodes, and both flow nodes are on either side
!>     of a subdomain interface, it can happen that different flow nodes
!>     are selected in different subdomains and consequently:
!>       either no or two subdomains are owner of the observation station
!>     in that case: use flow node in subdomain with lowest number
!>
!>   we give the preference to the following order
!>     inside cell in own domain:    dist=0d0
!>     inside cell in other domain:  dist=DPENALTY
!>     outside cell in own domain:   dist=DPENALTY + distance
!>     outside cell in other domain: dist=DREJECT (>DPENALTY) never
!>   reduce: take global cell with lowest dist
   subroutine reduce_kobs(N, kobs, xobs, yobs, jaoutside)
      use m_flowgeom, only: xz, yz, nd
      use unstruc_messages
      use geometry_module, only: pinpok, dbdistance
      use m_missing, only: jins, dmiss
      use m_sferic, only: jsferic, jasfer3D
#ifdef HAVE_MPI
      use mpi
#endif
   
      implicit none
      
      integer,                        intent(in)    :: N          !< number of observation stations
      integer,          dimension(N), intent(inout) :: kobs       !< observation station flow_node numbers, >0: in own subdomain, -1: in other subdomain, 0: not found in any subdomain
      double precision, dimension(N), intent(in)    :: xobs, yobs !< observation station coordinates
      integer,                        intent(in)    :: jaoutside  !< allow outside cells (for 1D) (1) or not (0)
   
#ifdef HAVE_MPI
      double precision, dimension(:),   allocatable :: dist   ! distances from flow nodes to observation stations
      double precision, dimension(:,:), allocatable :: dist_all
      
      
      double precision                              :: xp, yp
      
      integer                                       :: i, other_domain, in, k1, ierror
      
      double precision, parameter                   :: DPENALTY = 1d10  ! should be smaller than DREJECT
      double precision, parameter                   :: DREJECT  = 2d99  ! should be larger than DPENALTY
      
      if ( N.lt.1 ) return
      
      allocate(dist(N))
      dist = DREJECT
      
      allocate(dist_all(N,0:ndomains-1))
      
!     set distances to observation stations
      do i=1,N
         k1 = kobs(i)
         if ( k1.eq.0 ) cycle
         
!        check if the observation station is inside the cell
         call pinpok(xobs(i), yobs(i), size(nd(k1)%x), nd(k1)%x, nd(k1)%y, in, jins, dmiss)
      
!        determine preference
         if ( in.eq.1 ) then
            if ( idomain(k1).eq.my_rank ) then
               dist(i) = 0d0
            else
               dist(i) = DPENALTY
            end if
         else if ( jaoutside.eq.1 ) then
            if ( idomain(k1).eq.my_rank ) then
               xp = xz(k1)
               yp = yz(k1)
               dist(i) = DPENALTY + dbdistance(xobs(i),yobs(i),xp,yp, jsferic, jasfer3D, dmiss)
            else
               dist(i) = DREJECT
            end if
         else
            dist(i) = DREJECT
         end if
      end do
      
!      BEGIN DEBUG
!       call MPI_barrier(DFM_COMM_DFMWORLD,ierror)
!       do idmn=0,ndomains-1
!          if ( idmn.eq.my_rank) then
!             open(unit=666,file='deleteme.txt', access='append')
!             
!             if ( my_rank.eq.0 ) then             
!                write(666,"('reduce_kobs')")
!                write(666,"('before reduce')")
!             end if
!             
!             write(666,"('my_rank=',I0)") idmn
!             do i=1,N
!                write(666,"(I4, I8, E17.4)") i, kobs(i), dist(i)
!             end do
!             
!             close(666)
!          end if
!          
!          call MPI_barrier(DFM_COMM_DFMWORLD,ierror)
!       end do
!      END DEBUG
      

      
!     globally reduce
      call mpi_allgather(dist, N, MPI_DOUBLE_PRECISION, dist_all, N, MPI_DOUBLE_PRECISION, DFM_COMM_DFMWORLD, ierror)
      
!     select domain      
      do i=1,N
         k1 = 0
         dist(i) = DREJECT
         
!        compare distance with distances in other subdomains         
         do other_domain=0,ndomains-1
            if ( dist_all(i,other_domain).eq.DREJECT ) then
               cycle
            else if ( dist_all(i,other_domain).lt.dist(i) .or. dist(i).eq.DREJECT ) then
               if ( other_domain.eq.my_rank ) then
                  k1 = kobs(i)   ! use value in this subdomain
               else
                  k1 = -1        ! in another subdomain
               end if
               dist(i) = dist_all(i,other_domain)
            end if
         end do
         
!        fill reduced value         
         kobs(i) = k1
      end do
      
!     safety: check uniqueness
      dist = 0d0
      do i=1,N
         if ( kobs(i).gt.0 ) then
            dist(i) = 1d0
         end if
      end do
      call mpi_allreduce(dist, dist_all, N, MPI_DOUBLE_PRECISION, MPI_SUM, DFM_COMM_DFMWORLD, ierror) ! re-use (part of) dist_all
      do i=1,N
         if ( dist_all(i,0).gt.1d0 ) then
            call mess(LEVEL_ERROR, 'reduce_kobs: non-unique observation station(s)')
         end if
      end do
      
      
!      BEGIN DEBUG
!       call MPI_barrier(DFM_COMM_DFMWORLD,ierror)
!       do idmn=0,ndomains-1
!          if ( idmn.eq.my_rank) then
!             open(unit=666,file='deleteme.txt', access='append')
!             
!             if ( my_rank.eq.0 ) then   
!                write(666,"('after reduce')")
!             end if
!             
!             write(666,"('my_rank=',I0)") idmn
!             do i=1,N
!                write(666,"(I4, I8, 2E17.4)") i, kobs(i), dist(i), dist_all(i)
!             end do
!             
!             close(666)
!          end if
!          
!          call MPI_barrier(DFM_COMM_DFMWORLD,ierror)
!       end do
!      END DEBUG
      
      if ( allocated(dist)     ) deallocate(dist)
      if ( allocated(dist_all) ) deallocate(dist_all)
      
#endif
      return
   end subroutine reduce_kobs
   
!> reduce outputted values at observation stations
   subroutine reduce_valobs(numvals, numobs, valobs, valobs_all)
      use m_missing
#ifdef HAVE_MPI
      use mpi
#endif
      
      implicit none
      
      integer,                                     intent(in)    :: numvals      !< number of values
      integer,                                     intent(in)    :: numobs       !< number of observation stations
      double precision, dimension(numvals,numobs), intent(inout) :: valobs       !< values at obervations stations to be outputted
      double precision, dimension(numvals,numobs), intent(inout) :: valobs_all   !< work array
      
      
!      double precision, dimension(:,:), allocatable              :: valobs_all
      
      double precision, parameter                                :: dsmall = -huge(1d0)
      
      integer                                                    :: iobs, ival
      
      integer                                                    :: ierror
      
#ifdef HAVE_MPI

      
!     disable oberservation stations with missing values in this domain
      do iobs=1,numobs
         do ival=1,numvals
            if ( valobs(ival,iobs).eq.DMISS ) then
                valobs(ival,iobs) = dsmall
            end if
         end do
      end do
      
      call MPI_allreduce(valobs,valobs_all,numobs*numvals,mpi_double_precision,mpi_max,DFM_COMM_DFMWORLD,ierror)
      valobs = valobs_all
      
!     set values of observation stations that were not found in any subdomain
      do iobs=1,numobs
         do ival=1,numvals
            if ( valobs(ival,iobs).eq.dsmall ) then   ! safety, check all vals until not found (not necessary)
               valobs(ival,iobs) = DMISS
            end if
         end do
      end do
!      write(6,"(I4, ':', E12.5)") my_rank, valobs(1,10)
      
#endif
      return
   end subroutine reduce_valobs
   
   
!> reduce source-sinks
!>   it is assumed that the source-sinks are unique
   subroutine reduce_srsn(numvals, numsrc, srsn)
      use m_missing
#ifdef HAVE_MPI
      use mpi
#endif
      
      implicit none
                                                   
      integer,                                     intent(in)    :: numvals, numsrc                  !< number of sources/sinks
      double precision                                              srsn(numvals, numsrc)   !< values associated with sources/sinks
      
      
      double precision, dimension(NUMVALS,numsrc)                :: srsn_all
      
      integer                                                    :: ierror
      
#ifdef HAVE_MPI

      if ( my_rank.eq.0 ) then
         continue
      end if
      
      if ( my_rank.eq.1 ) then
         continue
      end if
      
      if ( my_rank.eq.2 ) then
         continue
      end if
      
      call MPI_allreduce(srsn,srsn_all,numsrc*NUMVALS,mpi_double_precision,mpi_sum,DFM_COMM_DFMWORLD,ierror)
      srsn = srsn_all
      
      if ( my_rank.eq.0 ) then
         continue
      end if
      
      if ( my_rank.eq.1 ) then
         continue
      end if
      
      if ( my_rank.eq.2 ) then
         continue
      end if
      
#endif
      return
   end subroutine reduce_srsn
   
   
!> reduce XLSAM in setprofs1d
   subroutine reduce_xlsam(Nproflocs, xlsam, distsam, iconnsam)
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      integer,                                     intent(in)    :: Nproflocs   !< number of profile locations (samples)
      double precision, dimension(:),              intent(inout) :: xlsam       !< branch-coordinate of sample points
      double precision, dimension(:),              intent(inout) :: distsam     !< distance from sample to branch
      integer,          dimension(:),              intent(inout) :: iconnsam    !< connected-branch associated with sample

#ifdef HAVE_MPI
      double precision, dimension(:), allocatable                :: dum
      
      integer,          dimension(:), allocatable                :: idum
      
      double precision, parameter                                :: dtol=1d-8
      double precision, parameter                                :: DLARGE=1d99
      double precision, parameter                                :: ILARGE=10000
      
      integer                                                    :: i, ierror
      
      allocate(dum(Nproflocs))
      dum = 0d0
      
      allocate(idum(Nproflocs))
      idum = 0d0
      
      call MPI_allreduce(distsam, dum, Nproflocs, MPI_DOUBLE_PRECISION, MPI_MIN, DFM_COMM_DFMWORLD, ierror)
      if ( ierror.ne.0 ) goto 1234
            
      do i=1,Nproflocs
         if ( distsam(i).gt.dum(i)+dtol ) then
            xlsam(i) = DLARGE
            iconnsam(i) = ILARGE
         end if
      end do
      
      distsam = dum
      
      call MPI_allreduce(xlsam, dum, Nproflocs, MPI_DOUBLE_PRECISION, MPI_MIN, DFM_COMM_DFMWORLD, ierror)
      if ( ierror.ne.0 ) goto 1234
      
      call MPI_allreduce(iconnsam, idum, Nproflocs, MPI_INTEGER, MPI_MIN, DFM_COMM_DFMWORLD, ierror)
      if ( ierror.ne.0 ) goto 1234
      
      do i=1,Nproflocs
         if ( xlsam(i).eq.DLARGE ) then
            xlsam(i) = dum(i)
            iconnsam(i) = idum(i)
         end if
      end do
      
 1234 continue
      deallocate(dum)
      deallocate(idum)   
#endif
      return
   end subroutine reduce_xlsam
     
!> Reduce (sum) the partial sum for each cross section across all MPI partitions.
   subroutine reduce_crs(resu,ncrs,numvals)
#ifdef HAVE_MPI
      use mpi
#endif
      implicit none
      
      integer,                                   intent(in)    :: ncrs                !< number of cross-sections
      integer,                                   intent(in)    :: numvals             !< which values to sum (1=discharge)
      double precision, dimension(numvals,ncrs), intent(inout) :: resu  !< cross-section data, note: ncrs from module m_monitoring_crosssections
      
      double precision, dimension(:,:),          allocatable   :: resu_all
      
      integer                                                  :: ierror
      
#ifdef HAVE_MPI
!     allocate
      allocate(resu_all(numvals,ncrs))
      
      call mpi_allreduce(resu,resu_all,numvals*ncrs,mpi_double_precision,mpi_sum,DFM_COMM_DFMWORLD,ierror)
      
      resu = resu_all
      
!     deallocate
      if ( allocated(resu_all) ) deallocate(resu_all)
#endif
      return
   end subroutine reduce_crs
   
   
!> exclude water-level ghostnodes from solution vector via kfs
   subroutine partition_setkfs()
      use m_flowgeom, only: kfs, Ndx, xz, yz, Ndxi, nd, wu, Lnxi, Lnx, ln
      use m_plotdots
      implicit none
      
      integer :: i
      integer :: k, L, LL, n
      
      if ( jaoverlap.eq.0 ) then 
         do i=1,nghostlist_sall(ndomains-1)
            kfs(ighostlist_sall(i)) = 0
         end do
      else 
         do i=1,nghostlist_snonoverlap(ndomains-1)
            kfs(ighostlist_snonoverlap(i)) = 0
         end do
      end if
      
!     disable disabled ghostnodes
      numdots = 0
  klp:do k=1,Ndxi
         do LL=1,nd(k)%lnx
            L = iabs(nd(k)%ln(LL))
            if ( wu(L).ne.0d0 ) then
               cycle klp
            end if
         end do
         if ( kfs(k).ne.0 ) then
            numdots = numdots+1
            call reallocdots(numdots)
            xdots(numdots) = xz(k)
            ydots(numdots) = yz(k)

            kfs(k) = 0
         end if
      end do klp
      
      return
   end subroutine partition_setkfs
   
   

!> finalize before exit
   subroutine partition_finalize()
      use m_flowparameters, only: Icgsolver
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      integer :: ierr
#ifdef HAVE_PETSC
      call stoppetsc()
#endif

#ifdef HAVE_PARMS
      call deallocparms()
#endif
   
#ifdef HAVE_MPI
      if (ja_mpi_init_by_fm == 1) then
         call mpi_finalize(ierr)
      end if
#endif
      return
   end subroutine partition_finalize
   
!> make global 1D netbranch numbering
!>    affects netbr, NRLB and K1BR
!>    uses:
!>       netbr, mxnetbr from network_data
!>       numranks
!>    note: partition_init not called, i.e. ndomains, ghostlists, sendlists unavailable
   subroutine global_netbranch_numbering()
      use network_data
      use m_missing
      use m_sferic, only: pi
      use geometry_module, only: dbdistance
#ifdef HAVE_MPI
      use mpi
#endif

      implicit none
      
      integer,          dimension(:),   allocatable :: iglobalbranch    ! global branch number, dim(numnetbr)
      integer,          dimension(:),   allocatable :: numbranches      ! number of branches per domain, dim(0:numranks-1)

#ifdef HAVE_MPI
      double precision, dimension(:,:), allocatable :: xyL_all          ! center coordinates and angle of left  link of branch, dim(3,numallnetbr)
      double precision, dimension(:,:), allocatable :: xyR_all          ! center coordinates and angle of right link of branch, dim(3,numallnetbr)
      double precision, dimension(:,:), allocatable :: xyL_loc, xyR_loc
            
      integer,          dimension(:),   allocatable :: ibr_glob_left    ! global number of left  connected branch, dim(numallnetbr)
      integer,          dimension(:),   allocatable :: ibr_glob_right   ! global number of right connected branch, dim(numallnetbr)
      integer,          dimension(:),   allocatable :: Lother_left      ! other link of left  connected branch, dim(numallnetbr)
      integer,          dimension(:),   allocatable :: Lother_right     ! other link of right connected branch, dim(numallnetbr)
      integer,          dimension(:),   allocatable :: idum
      integer,          dimension(:),   allocatable :: inew             ! new global branch number, dim(numallnetbr)
      integer,          dimension(:),   allocatable :: iorient          ! branch orientation, left-right (1), or reverse/right-left (0), dim(numallnetbr)
      integer,          dimension(:),   allocatable :: iordened_branches ! ordened branched, dim(numallnetbr)
      integer,          dimension(:),   allocatable :: ipoint           ! pointer in iordened_branches list, dim(numallnetbr+1)
      
      double precision, dimension(:),   allocatable :: dlL, dlR, dLtot  ! left-part, right-part and total branch length
      double precision, dimension(:),   allocatable :: doffset          ! offset length of branch
      double precision, dimension(:),   allocatable :: ddum
      
      integer                                       :: numnetbr         ! number of branches in this domain
      integer                                       :: numallnetbr      ! number of branches summed over all domains
      integer                                       :: numnew           ! new number of (connected) branches
      
      double precision                              :: xloc, yloc
      double precision                              :: dabsangle
      double precision                              :: dlength, dleft, dconnected
      
      integer                                       :: iglobalbranch_first
      integer                                       :: ibr, ibrr, idmn, i, iglob, k, N, num, L, LL, LR
      integer                                       :: istart, iend, ioff, ibr_other, ibr_glob
      integer                                       :: inext, icount, Lconnect
      
      integer                                       :: ierror           ! error (1) or not (0)
      
      logical                                       :: Lleftfound, Lrightfound
      
      double precision, external                    :: dlinklength
      
      double precision, parameter                   :: dtol=1d-8
      
!     count the number of branches
      numnetbr = mxnetbr
      
!     allocate
      allocate(numbranches(0:numranks-1))
      allocate(iglobalbranch(1:numnetbr))
      allocate(doffset(numnetbr))
      
!     make global branch numbering
      call MPI_allgather(numnetbr, 1, MPI_INTEGER, numbranches, 1, MPI_INTEGER, DFM_COMM_DFMWORLD, ierror)
      if ( ierror.ne.0 ) goto 1234
      
      numallnetbr = sum(numbranches(0:numranks-1))
      
      iglobalbranch_first = 1
      do idmn=0,my_rank-1
         iglobalbranch_first = iglobalbranch_first+numbranches(idmn)
      end do
      
      do ibr=1,numnetbr
         iglobalbranch(ibr) = ibr + iglobalbranch_first - 1
      end do
      
!     make the global left/right coordinate lists
      allocate(xyL_loc(3,numallnetbr), xyR_loc(3,numallnetbr))
      allocate(xyL_all(3,numallnetbr), xyR_all(3,numallnetbr))
      allocate(ibr_glob_left(numallnetbr), ibr_glob_right(numallnetbr))
      allocate(Lother_left(numallnetbr), Lother_right(numallnetbr))
      allocate(dlL(numallnetbr), dlR(numallnetbr), dltot(numallnetbr))
      allocate(idum(numallnetbr))
      allocate(ddum(numallnetbr))
      allocate(inew(numallnetbr))
      allocate(iorient(numallnetbr))
      allocate(iordened_branches(numallnetbr))
      allocate(ipoint(numallnetbr+1))
      
      xyL_loc = 0d0
      xyR_loc = 0d0
      
      do ibr=1,numnetbr
         iglob = ibr+iglobalbranch_first-1
         num = netbr(ibr)%NX
         LL = netbr(ibr)%LN(1)
         LR = netbr(ibr)%LN(num)
         xyL_loc(1,iglob) = 0.5d0*(xk(kn(1,iabs(LL)))+xk(kn(2,iabs(LL))))
         xyL_loc(2,iglob) = 0.5d0*(yk(kn(1,iabs(LL)))+yk(kn(2,iabs(LL))))
         xyR_loc(1,iglob) = 0.5d0*(xk(kn(1,iabs(LR)))+xk(kn(2,iabs(LR))))
         xyR_loc(2,iglob) = 0.5d0*(yk(kn(1,iabs(LR)))+yk(kn(2,iabs(LR))))
         
         xyL_loc(3,iglob) = dLinkangle(LL)
         xyR_loc(3,iglob) = dLinkangle(LR)
      end do
      
!     gather information from all domains
!        note that this can also be achieved by using mpi_allgatherv,
!        but now we do not need to use an offset in the global array
      call MPI_allreduce(xyL_loc,xyL_all,3*numallnetbr,MPI_DOUBLE_PRECISION,MPI_SUM,DFM_COMM_DFMWORLD,ierror)
      if ( ierror.ne.0 ) goto 1234
      call MPI_allreduce(xyR_loc,xyR_all,3*numallnetbr,MPI_DOUBLE_PRECISION,MPI_SUM,DFM_COMM_DFMWORLD,ierror)
      if ( ierror.ne.0 ) goto 1234
      
!     find the global branch connectivity
      call find_branch_conn(ibr_glob_left, ibr_glob_right, Lother_left, Lother_right, ierror)
      if ( ierror /= 0 ) goto 1234
      
!     connect branches and make new global branch numbering
      inew   = 0
      numnew = 0
      dlL = 0d0
      dlR = 0d0
      dltot = 0d0
      iorient = 0
      ipoint(1) = 1
      do ibr=1,numallnetbr
         if ( inew(ibr).ne.0 ) cycle   ! branch has already new global number assigned
         numnew = numnew+1
         idum = 0
         icount = 0
!        walk left
         call connect_branches(ibr,numnew,0,icount)
!        reverse order of branches
         idum(1:icount)    = idum(icount:1:-1)
         iorient(idum(1:icount)) = 1-iorient(idum(1:icount))   ! walking in opposite direction
!        enable this branch again by removing starting (now last) branch
         inew(ibr) = 0
         icount = icount-1
!        walk right
         call connect_branches(ibr,numnew,1,icount)
         
         if ( my_rank.eq.0 ) then
            write(6,"(I4, ':', 100I4)") numnew, (idum(i), i=1,icount)
            write(6,"(I4, ':', 100I4)") numnew, (iorient(iabs(idum(i))), i=1,icount)
         end if
         
!        add to the ordered branch list
         ipoint(numnew+1) = ipoint(numnew)+icount
         iordened_branches(ipoint(numnew):ipoint(numnew+1)-1) = idum(1:icount)

!        measure lengths in the connected branch
         do i=1,icount
            ibr_glob = iabs(idum(i))
            ibrr = ibr_glob-iglobalbranch_first+1
            
            if ( ibrr.lt.1 .or. ibrr.gt.numnetbr ) cycle ! local branches only
            
            N = netbr(ibrr)%NX
!           check orientation of this branch            
            if ( iorient(ibr_glob).ne.1 ) then
!              swap orientation
               call swap_branch(ibrr)
            end if
            
!           find the link that is connected to the start/end of the other link
            if ( i.gt.1 ) then
               Lconnect = Lother_right(iabs(idum(i-1)))  ! we need the right connection of the previous branch, since the orientation is always from left to right
            else
               Lconnect = 0
            end if
            
            dleft = 0d0
            dlength = 0d0
            do k=1,N
               L = netbr(ibrr)%LN(k)
               dlength = dlength + dlinklength(L)
               if ( iabs(L).eq.iabs(Lconnect) ) then  ! offset link
                  dleft = dlength
               end if
            end do
            
            dlL(ibr_glob) = dleft
            dltot(ibr_glob) = dlength
!            dlR(ibr_glob) = dlength-dleft
         end do
      
      end do
      
!     gather information from all domains
      call MPI_allreduce(dlL,ddum,numallnetbr,MPI_DOUBLE_PRECISION,MPI_SUM,DFM_COMM_DFMWORLD,ierror)
      if ( ierror.ne.0 ) goto 1234
      dlL = ddum
      
      call MPI_allreduce(dltot,ddum,numallnetbr,MPI_DOUBLE_PRECISION,MPI_SUM,DFM_COMM_DFMWORLD,ierror)
      if ( ierror.ne.0 ) goto 1234
      dltot = ddum
      
      if ( my_rank.eq.0 ) write(6,*) (dlL(i), i=1,numallnetbr)
      if ( my_rank.eq.0 ) write(6,*) (dltot(i), i=1,numallnetbr)
      
      dlR = dltot - dlL
      
!     compute the offset lengths and fill local branch properties
      do i=1,numnew
         dconnected = 0d0
         do k=ipoint(i),ipoint(i+1)-1
            ibr_glob = iabs(iordened_branches(k))
            ibrr = ibr_glob-iglobalbranch_first+1
      
!           fill local branch properties    
            if ( ibrr.ge.1 .and. ibrr.le.numnetbr ) then
               netbr(ibrr)%iconn = i
               netbr(ibrr)%doff  = dconnected - dlL(ibr_glob)
            end if
      
!           add to length of connected branch
            dconnected = dconnected + dltot(ibr_glob) - dlL(ibr_glob)
         end do
      end do
      
1234  continue 
      
!     deallocate
      if ( allocated(numbranches)       ) deallocate(numbranches)
      if ( allocated(iglobalbranch)     ) deallocate(iglobalbranch)
      if ( allocated(xyL_loc)           ) deallocate(xyL_loc)
      if ( allocated(xyR_loc)           ) deallocate(xyR_loc)
      if ( allocated(xyL_all)           ) deallocate(xyL_all)
      if ( allocated(xyR_all)           ) deallocate(xyR_all)
      if ( allocated(ibr_glob_left)     ) deallocate(ibr_glob_left)
      if ( allocated(ibr_glob_right)    ) deallocate(ibr_glob_right)
      if ( allocated(idum)              ) deallocate(idum)
      if ( allocated(inew)              ) deallocate(inew)
      if ( allocated(Lother_left)       ) deallocate(Lother_left)
      if ( allocated(Lother_right)      ) deallocate(Lother_right)
      if ( allocated(dlL)               ) deallocate(dlL)
      if ( allocated(dlR)               ) deallocate(dlR)
      if ( allocated(dltot)             ) deallocate(dltot)
      if ( allocated(doffset)           ) deallocate(doffset)
      if ( allocated(ddum)              ) deallocate(ddum)
      if ( allocated(iorient)           ) deallocate(iorient)
      if ( allocated(iordened_branches) ) deallocate(iordened_branches)
      if ( allocated(ipoint)            ) deallocate(ipoint)

      return
      
      contains
      
!>    reverse order of branch
!>       affects netbr(ibr), NRLB and K1BR
      subroutine swap_branch(ibr)
         implicit none
         
         integer, intent(in) :: ibr
         
         integer             :: i, ibr_glob, idum, La, Ldum, Lnew, N
         
         ibr_glob = ibr + iglobalbranch_first - 1
         
         N = netbr(ibr)%NX
         netbr(ibr)%LN(1:N) = -netbr(ibr)%LN(N:1:-1)
         
         Ldum                   = Lother_left(ibr_glob)
         Lother_left(ibr_glob)  = Lother_right(ibr_glob)
         Lother_right(ibr_glob) = Ldum
         
         Ldum                     = ibr_glob_left(ibr_glob)
         ibr_glob_left(ibr_glob)  = ibr_glob_right(ibr_glob)
         ibr_glob_right(ibr_glob) = Ldum
         
         iorient(ibr_glob) = 1-iorient(ibr_glob)
         
         do i=1,N/2
            Ldum = iabs(netbr(ibr)%LN(i))
            Lnew = iabs(netbr(ibr)%LN(N-i+1))
            idum = NRLB(Ldum)
            NRLB(Ldum) = NRLB(Lnew)
            NRLB(Lnew) = idum
         end do
         
         do i=1,N
            Ldum = netbr(ibr)%LN(i)
            La = iabs(Ldum)
            if ( Ldum.gt.0 ) then
               K1BR(NRLB(La)) = kn(1,La)
            else
               K1BR(NRLB(La)) = kn(2,La)
            end if
         end do
         
         return
      end subroutine swap_branch
      
!>    make connected branch numbering
!>       sets: inew, idum, iorient
!>       uses: ibr_glob_left, ibr_glob_right
      recursive subroutine connect_branches(ibr,numcur,idir,icount)
         implicit none
         
         integer, intent(in)    :: ibr       !< global branch number, can be <0 to indicate that the orientation has switched
         integer, intent(in)    :: numcur    !< current new branch number
         integer, intent(in)    :: idir      !< walk right (1) or left (0)
         integer, intent(inout) :: icount !< counter of branch in path
         
         integer                :: inext, ibra
         
         ibra = iabs(ibr)
         
         if ( inew(ibra).ne.0 ) return  ! branch has already new global number assigned
         
         inew(ibra) = numcur
         
         icount        = icount+1
         idum(icount)  = ibra
         iorient(ibra) = idir
         
         if ( idir.eq.1 ) then   ! walk right
            inext = ibr_glob_right(ibra)
         else                    ! walk left
            inext = ibr_glob_left(ibra)
         end if
         
         if ( inext.gt.0 ) then
            call connect_branches(inext,numcur,idir,icount)
         else if ( inext.lt.0 ) then   ! swap orientation
            call connect_branches(inext,numcur,1-idir,icount)
         end if
         
         return
      end subroutine connect_branches
      
!>    gives link angle, changes sign when link has negative number      
      double precision function dLinkangle(L)
         use m_sferic, only: jsferic
         use geometry_module, only: getdxdy 
         
         implicit none
         
         integer,          intent(in) :: L  !< link number
         double precision              :: dx, dy       
         integer                       :: k1, k2
         
         
         if ( L.gt.0 ) then
            k1 = kn(1,L)
            k2 = kn(2,L)
         else
            k1 = kn(2,-L)
            k2 = kn(1,-L)
         end if
         
         call getdxdy(xk(k1), yk(k1), xk(k2), yk(k2),dx,dy,jsferic)
         !dx = getdx(xk(k1), yk(k1), xk(k2), yk(k2))
         !dy = getdy(xk(k1), yk(k1), xk(k2), yk(k2))
         
         dLinkangle = atan2(dy,dx)
         
         return
      end function dLinkangle
      

      
!> find the global branch connectivity
!>    sets ibr_glob_left, ibr_glob_right, Lother_left, Lother_right
      subroutine find_branch_conn(ibr_glob_left, ibr_glob_right, Lother_left, Lother_right, ierror)
         
         use m_missing, only: dmiss
         use m_sferic, only: jsferic, jasfer3D
         use geometry_module, only: dbdistance
         
         implicit none
         
         integer, dimension(:), intent(out) :: ibr_glob_left, ibr_glob_right, Lother_left, Lother_right
         integer, intent(out)               :: ierror   !< error (1) or not (0)
         
         integer                            :: La
         integer :: ibr, inext
         
!     see if any of the left/right links of other domain's branches is in a netbranch in this domain
         ibr_glob_left = 0
         ibr_glob_right = 0
         Lother_left  = 0
         Lother_right = 0
         ioff = 0
         do idmn=0,numranks-1
            istart = 1 + ioff                   ! global branch number of first branch in other domain
            iend   = numbranches(idmn) + ioff   ! global branch number of last  branch in other domain
              
            if ( idmn.ne.my_rank ) then
               do ibr_other = istart,iend  ! global branch number
   !              compare start(called left) and end (called right) of branch in other domain with branches in own domain               
                  Lleftfound  = .false.
                  Lrightfound = .false.
               
                  do ibr=1,numnetbr ! local branch number
                     ibr_glob = ibr + iglobalbranch_first - 1  ! global branch number
                     num = netbr(ibr)%NX
                     do i=1,num
                        L = netbr(ibr)%LN(i)
                        La = iabs(L)
                        xloc = 0.5d0*(xk(kn(1,La))+xk(kn(2,La)))
                        yloc = 0.5d0*(yk(kn(1,La))+yk(kn(2,La)))               
                        if ( dbdistance(xloc,yloc,xyL_all(1,ibr_other),xyL_all(2,ibr_other),jsferic, jasfer3D, dmiss).lt.dtol ) then
   !                       left match found
                           Lleftfound = .true.
                           Lother_left(ibr_other) = L
                        
   !                       check orientation
                           dabsangle = abs(dLinkangle(L)-xyL_all(3,ibr_other))
                           if ( dabsangle.lt.dtol ) then
   !                          same orientation
                              ibr_glob_left(ibr_other) = ibr_glob
                           else if ( abs(dabsangle-pi).lt.dtol ) then
   !                          opposite orientation
                              ibr_glob_left(ibr_other) = -ibr_glob
                           else
   !                          orientation error
                              call qnerror('global_netbranch_numbering: orientation error', ' ', ' ')
                              goto 1234
                           end if
                        end if      
                     
                        if ( dbdistance(xloc,yloc,xyR_all(1,ibr_other),xyR_all(2,ibr_other),jsferic, jasfer3D, dmiss).lt.dtol ) then
   !                       right match found
                           Lrightfound = .true.
                           Lother_right(ibr_other) = L
                        
   !                       check orientation
                           dabsangle = abs(dLinkangle(L)-xyR_all(3,ibr_other))
                           if ( dabsangle.lt.dtol ) then
   !                          same orientation
                              ibr_glob_right(ibr_other) = ibr_glob
                           else if ( abs(dabsangle-pi).lt.dtol ) then
   !                          opposite orientation
                              write(6,*) 'ibr_other=', ibr_other
                              write(6,*) 'ibr=', ibr
                              write(6,*) 'angle_other=', xyR_all(3,ibr_other)
                              write(6,*) 'angle=', dLinkangle(L)
                              ibr_glob_right(ibr_other) = -ibr_glob
                           else
   !                          orientation error
                              call qnerror('global_netbranch_numbering: orientation error', ' ', ' ')
                              goto 1234
                           end if
                        end if
                     
                        if ( Lleftfound.and.Lrightfound ) exit
                     end do
                  end do
               end do
            end if
         
            ioff = iend
         end do
      
!        gather information from all domains
         call MPI_allreduce(ibr_glob_left,idum,numallnetbr,MPI_INTEGER,MPI_SUM,DFM_COMM_DFMWORLD,ierror)
         if ( ierror.ne.0 ) goto 1234
         ibr_glob_left = idum
      
         call MPI_allreduce(ibr_glob_right,idum,numallnetbr,MPI_INTEGER,MPI_SUM,DFM_COMM_DFMWORLD,ierror)
         if ( ierror.ne.0 ) goto 1234
         ibr_glob_right = idum
      
         call MPI_allreduce(Lother_left,idum,numallnetbr,MPI_INTEGER,MPI_SUM,DFM_COMM_DFMWORLD,ierror)
         if ( ierror.ne.0 ) goto 1234
         Lother_left = idum
      
         call MPI_allreduce(Lother_right,idum,numallnetbr,MPI_INTEGER,MPI_SUM,DFM_COMM_DFMWORLD,ierror)
         if ( ierror.ne.0 ) goto 1234
         Lother_right = idum

   !     check for mutual connectivity
         do ibr=1,numallnetbr
   !        check right
            inext = ibr_glob_right(ibr)
            if ( inext.gt.0 ) then
   !           same orientation: should connect with next left
               if ( ibr_glob_left(inext).ne.ibr ) then
   !              deactivate connection
                  ibr_glob_right(ibr) = 0
               end if
            else if ( inext.lt.0 ) then
   !           opposite orientation: should connect with next right
               if ( ibr_glob_right(-inext).ne.-ibr ) then
                  ibr_glob_right(ibr) = 0
               end if
            end if
         
   !        check left
            inext = ibr_glob_left(ibr)
            if ( inext.gt.0 ) then
   !           same orientation: should connect with next right
               if ( ibr_glob_right(inext).ne.ibr ) then
   !              deactivate connection
                  ibr_glob_left(ibr) = 0
               end if
            else if ( inext.lt.0 ) then
   !           opposite orientation: should connect with next left
               if ( ibr_glob_left(-inext).ne.-ibr ) then
                  ibr_glob_left(ibr) = 0
               end if
            end if
         end do
         
         ierror = 0
    1234 continue
         
         return
      end subroutine find_branch_conn
      
#endif
   
   end subroutine global_netbranch_numbering
   
   
   
   
   subroutine diff_ghosts(itype, var)
      use m_flowgeom, only: Lnx, Lnxi, ln
      implicit none
      
      integer,                        intent(in)    :: itype
      
      double precision, dimension(:), intent(inout) :: var
      
      double precision, dimension(:), allocatable   :: dum
      
      integer                                       :: i, k, L, N
      integer                                       :: ierr
      
      ierr = 1
      
      write(6,*) 'XXX'
      
      N = ubound(var,1)
      if ( N.lt.1 ) goto 1234
      
      allocate(dum(N))
      dum = var
      
      if ( itype.eq.ITYPE_U ) then
         call update_ghosts(itype,1,N,dum,ierr)
         do L=1,Lnxi
            if ( idomain(ln(1,L)).eq.my_rank .or. idomain(ln(2,L)).eq.my_rank ) then
               if ( abs(dum(L)-var(L)).gt.1d-12 ) then
                  write(6,*) 'XXX: ', my_rank, L, dum(L), var(L), dum(L)-var(L)
               end if
            end if
         end do
      else if ( itype.eq.ITYPE_S .or. itype.eq.ITYPE_Sall ) then
         call update_ghosts(itype,1,N,dum,ierr)
         do i=1,nghostlist_sall(ndomains-1)
            k = ighostlist_sall(i)
            if ( ighostlev_cellbased(k).gt.3 .or. ighostlev_nodebased(k).gt.2 )  cycle
            if ( abs(dum(k)-var(k)).gt.1d-12) then
               write(6,*) 'XXX: ', my_rank, k, dum(k), var(k), dum(k)-var(k)
            end if
         end do
      end if
      
      ierr = 0
      
 1234 continue
      
      if ( allocated(dum) ) deallocate(dum)

      return
   end subroutine


   !> Generic function for weighted average on a quantities defined on cells or links, also applying optional filters
   !> optional firstFilter is defined on a global weights array [1, ncells/nlinks]
   !> optional secondFilter is defined on a local array [1,size(secondFilter)]
   !> works also across multiple MPI ranks
   function getAverageQuantityFromLinks(startLinks, endLinks, weights, indsWeight, quantity, indsQuantity, results, quantityType, &
      firstFilter, firstFilterValue, secondFilter, secondFilterValue ) result(ierr)

   use mpi
   use m_flowexternalforcings
   use m_timer

   !inputs
   integer,intent(in),dimension(:)               :: startLinks             !< start indexes [1,nsegments]
   integer,intent(in),dimension(:)               :: endLinks               !< end   indexes [1,nsegments]
   double precision,intent(in),dimension(:)      :: weights                !< global weights array
   integer,intent(in),dimension(:)               :: indsWeight             !< local indexes on global weights array
   double precision,intent(in),dimension(:)      :: quantity               !< global quantity array
   integer,intent(in),dimension(:)               :: indsQuantity           !< local indexes on global quantity array
   integer,intent(in)                            :: quantityType           !< The type of quantity: 0 scalar, 1 array (edge orientation matters)
   
   double precision,intent(in),dimension(:), optional :: firstFilter       !< filter to apply on the global weights array
   double precision,intent(in), optional              :: firstFilterValue  !< value to activate the first filter (activated if larger than filter value)

   integer,intent(in), dimension(:), optional    :: secondFilter           !< filter to apply on the local weights array
   integer,intent(in), optional                  :: secondFilterValue      !< value to activate the second filter (activated if larger than filter value)
   !locals
   integer                                       :: ns, nsegments, nl, indWeight, indQuantity
   double precision                              :: sumQuantitiesByWeight, sumWeights
   double precision                              :: quantitiesByWeight, weight
   double precision, allocatable                 :: resultsSum(:,:)

   !outputs
   double precision,dimension(:,:),intent(inout) :: results
   integer                                       :: ierr

   ierr = 0
   nsegments = size(startLinks)
   allocate(resultsSum(2,nsegments))
   results = 0.0d0

   do ns = 1, nsegments

      sumQuantitiesByWeight = 0d0
      sumWeights            = 0d0

      do nl  = startLinks(ns), endLinks(ns)

         indWeight    = abs(indsWeight(nl))
         indQuantity  = abs(indsQuantity(nl))
         quantitiesByWeight = 0.0d0
         weight = 0.0d0

         if ( jampi.eq.1 ) then
            ! Exclude ghost nodes
            if ( idomain(indQuantity).ne.my_rank ) then
               cycle
            end if
         end if


         ! weights are always positive
         weight = weights(indWeight)
         if(quantityType == 1) then
            if (indsQuantity(nl)< 0) then
               quantitiesByWeight =   - quantity(indQuantity)*weight
            else
               quantitiesByWeight =   quantity(indQuantity)*weight
            endif
         else
            quantitiesByWeight =  quantity(indQuantity)*weight
         endif

         if ( present(firstFilter).and.present(firstFilterValue)) then
            if ( firstFilter(indWeight) <= firstFilterValue ) then
               quantitiesByWeight =  0.0d0
               weight             =  0.0d0
            endif
         endif

         if ( present(secondFilter).and.present(secondFilterValue)) then
            if ( secondFilter(nl) <= secondFilterValue ) then
               quantitiesByWeight =  0.0d0
               weight             =  0.0d0
            endif
         endif

         sumQuantitiesByWeight   = sumQuantitiesByWeight + quantitiesByWeight
         sumWeights              = sumWeights + weight

      enddo
      results(1,ns) = sumQuantitiesByWeight
      results(2,ns) = sumWeights
   end do

   if ( jampi.eq.1 .and. japartqbnd.eq.1 ) then
      ! Here we reduce the results
      if ( jatimer.eq.1 ) call starttimer(IMPIREDUCE)
#ifdef HAVE_MPI
      call MPI_allreduce(results,resultsSum,2*nsegments,mpi_double_precision,mpi_sum,DFM_COMM_DFMWORLD, ierr)
      results = resultsSum
#endif  
      if ( jatimer.eq.1 ) call stoptimer(IMPIREDUCE)
   end if

   end function getAverageQuantityFromLinks
      
      !> generate partitioning polygons 
      ! Moved to module because of optional argument
      ! Adaptations from original version to fix coupling with SWAN:
      ! - myrank: write only polygon for points that are strictly in domain myrank (no ghostcell polygons)
   subroutine generate_partition_pol_from_idomain(ierror, myrank)
      use network_data
      use m_polygon
      use m_tpoly
      use m_sferic
      use unstruc_messages
      use gridoperations
      implicit none
      
      integer,               intent(out) :: ierror
      integer, optional,     intent(in)  :: myrank
      
      integer, dimension(:), allocatable :: lnn_sav
      
      integer                            :: icL, icR, idmn, L
      
      integer                            :: jabound
      integer                            :: myrank_
      integer                            :: nstart, nstop
      
      ierror = 1
      
      myrank_ = -1
            
      if (present(myrank)) then
         myrank_ = myrank         
      endif
      
      if (myrank_>=0) then
         nstart = myrank_
         nstop  = myrank_ 
      else
         nstart = 1
         nstop  = Ndomains-1
      endif
      
      if ( netstat.eq.NETSTAT_CELLS_DIRTY ) then
         call findcells(0)
         call find1Dcells()
         
         call delete_dry_points_and_areas()
      end if
      
!     check for 1D cells (not supported)
      if ( nump1d2d.gt.nump ) then
         call mess(LEVEL_WARN, 'generate_partition_pol_from_idomain: 1D not supported')
         goto 1234
      end if
      
!     allocate
      allocate(lnn_sav(numL))
      
!     store
      call savepol()
      
      do L=1,numL
         lnn_sav(L) = lnn(L)
      end do
      
!     generate polygons
!        note: domain 0 is the default domain
!     clean up previous tpoly-type partitioning polygon
      call dealloc_tpoly(partition_pol)
      npartition_pol = 0   ! current number
      do idmn=nstart,nstop
         do L=numL1D+1,numL   ! only 2D supported
!           check if this netlink is a domain boundary
            jabound = 0 ! default
            icL = lne(1,L)
            icR = lne(2,L)
            if ( lnn(L).eq.1 ) then
               if ( idomain(icL).eq.idmn ) jabound = 1
            else if ( lnn(L).eq.2 ) then
               if ( (idomain(icL).ne.idomain(icR)) .and.    &
                      ( (idomain(icL).eq.idmn) .or. (idomain(icR).eq.idmn) ) ) then
                     jabound = 1
               end if
            end if
            
!           modify lnn
            if ( jabound.eq.1 ) then
               lnn(L) = 1
            else
               lnn(L) = 2
            end if
         end do
         
!        delete current polygon (if applicable)
         call delpol()
         
!        copy netbounds based on modified lnn to polygon
         call copynetboundstopol(0, 0, 0, 1)
         
!        set polygon nodal value to domain number
         zpl(1:NPL) = dble(idmn)
         
!        add polygon to tpoly-type partitioning polygons
         call pol_to_tpoly(npartition_pol, partition_pol, keepExisting=.true.)
         
!        check number of partitioning polygons
!         if ( npartition_pol.ne.idmn ) then
!            call qnerror('generate_partition_pol_from_idomain: number of polygons and domains do not match', ' ', ' ')
!         end if
         
!         call cls1()
!         rlin(1:numL) = lnn(1:numL)
!         call teknetstuff(key)
!         call tekpolygon()
!         write(message,"('domain ', I)") idmn
!         call qnerror(trim(message), ' ', ' ')
      
!        restore lnn
         do L=1,numL
            lnn(L) = lnn_sav(L)
         end do
      end do   ! do idmn=0,Ndomains-1
      
      
!     delete polygon
      call delpol()
      
!     copy tpoly-type partition polygons to polygon
      call tpoly_to_pol(partition_pol)
      
!     fix polygon for spheric, periodic coordinates
      call fix_global_polygons(0,1)
      
      ierror = 0
      
1234  continue
      
!     deallocate
      if ( allocated(lnn_sav) ) deallocate(lnn_sav)
      
      return
   end subroutine generate_partition_pol_from_idomain
   
!> reduce error level, note that it is assumed that DFM_NOERR=0 and that all error levels >=0 
   subroutine reduce_error(ierror) 
      implicit none 
      integer, intent(inout) :: ierror
      
      integer, dimension(1)  :: idum
      
      idum(1) = ierror
      call reduce_int_max(1, idum)
      ierror = idum(1)

      return 
   end subroutine reduce_error 
   
!> Abort all processes
   subroutine abort_all()
      use dfm_error, only: DFM_GENERICERROR
      implicit none
      integer :: ierr
#ifdef HAVE_MPI      
      call MPI_Abort(DFM_COMM_DFMWORLD, DFM_GENERICERROR, ierr)
#endif
      return
   end subroutine abort_all
   end module m_partitioninfo
   
   
   subroutine pressakey()
#ifdef HAVE_MPI
      use mpi
      use m_partitioninfo

      implicit none

      integer             :: ierr

      call MPI_barrier(DFM_COMM_DFMWORLD,ierr)

      if ( my_rank.eq.0 ) then
         write(6,*) "press a key from rank 0..."
         read(5,*)
      end if

      call MPI_barrier(DFM_COMM_DFMWORLD,ierr)
#else
      write(6,*) "press a key..."
      read(5,*)
#endif

      return
      
   end subroutine pressakey
   
   
!  print timing information to file
   subroutine print_timings(FNAM, dtime)
#ifdef HAVE_MPI
      use mpi
#endif
      use m_timer
      use m_partitioninfo
      
      implicit none
      
      character(len=*),                    intent(in) :: FNAM        !< file name
      double precision,                    intent(in) :: dtime       !< time
      
      integer                                         :: ierr
      integer                                         :: i, j, lenstr

      logical                                         :: Lexist
      
      integer, parameter                              :: ISTRLEN = 20
      integer, parameter                              :: MFILE   = 1234
      
      integer, parameter                              :: Ntvarlist = 13
      integer, dimension(Ntvarlist), parameter        :: itvarlist = (/ 1, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17 /)
      
      double precision, dimension(:,:), allocatable   :: t_max, t_ave, tcpu_max, tcpu_ave
      integer                                         :: itsol_max

      integer                                         :: jadoit
            
      character(len=128)                              :: FORMATSTRING, FORMATSTRINGINT, dum
      
!     allocate local arrays
!      if ( my_rank.eq.0 ) then
         allocate(t_max(3,NUMT), t_ave(3,NUMT), tcpu_max(3,NUMT), tcpu_ave(3,NUMT))
!      end if

      if ( jampi.eq.1 ) then
#ifdef HAVE_MPI
!        reduce timings
         call mpi_reduce(t,t_max,3*NUMT,MPI_DOUBLE_PRECISION,MPI_MAX,0,DFM_COMM_DFMWORLD,ierr)
         call mpi_reduce(t,t_ave,3*NUMT,MPI_DOUBLE_PRECISION,MPI_SUM,0,DFM_COMM_DFMWORLD,ierr)
         t_ave = t_ave/dble(ndomains)
         call mpi_reduce(tcpu,tcpu_max,3*NUMT,MPI_DOUBLE_PRECISION,MPI_MAX,0,DFM_COMM_DFMWORLD,ierr)
         call mpi_reduce(tcpu,tcpu_ave,3*NUMT,MPI_DOUBLE_PRECISION,MPI_SUM,0,DFM_COMM_DFMWORLD,ierr)
         tcpu_ave = tcpu_ave/dble(ndomains)
#endif
      else
         t_ave = t
         tcpu_ave = tcpu
         t_max = t
         tcpu_max = tcpu
      end if
      
!     reduce number of iterations
      if ( jampi.eq.1 ) then
#ifdef HAVE_MPI
         call mpi_reduce(numcgits,itsol_max,1,MPI_INTEGER,MPI_MAX,0,DFM_COMM_DFMWORLD,ierr)
#endif
         jadoit = 0
         if ( my_rank.eq.0 ) jadoit = 1
      else
         itsol_max = numcgits
         jadoit = 1
      end if
      
      if ( jadoit.eq.1 ) then
         inquire(FILE=FNAM,EXIST=Lexist)
         open(unit=MFILE,FILE=trim(FNAM),access='APPEND')
         
         if ( .not.Lexist ) then
!           print header
            lenstr=4
            do j=1,ISTRLEN-lenstr
               write(MFILE,'(" ", $)')
            end do
            write(MFILE,"(A, $)") 'time'
            
            lenstr=16
            do j=1,ISTRLEN-lenstr
               write(MFILE,'(" ", $)')
            end do
            write(MFILE,"(A, $)") 'number_of_tsteps'
            
            do i=1,Ntvarlist
               lenstr=len_trim(tnams(itvarlist(i)))
               do j=1,ISTRLEN-(lenstr+4)
                  write(MFILE,'(" ", $)')
               end do
               write(MFILE,"(A, '_ave', $)") trim(tnams(itvarlist(i)))
               
               do j=1,ISTRLEN-(lenstr+4)
                  write(MFILE,'(" ", $)')
               end do
               write(MFILE,"(A, '_max', $)") trim(tnams(itvarlist(i)))
               
               do j=1,ISTRLEN-(lenstr+8)
                  write(MFILE,'(" ", $)')
               end do
               write(MFILE,"(A, '_CPU_ave', $)") trim(tnams(itvarlist(i)))
               
               do j=1,ISTRLEN-(lenstr+8)
                  write(MFILE,'(" ", $)')
               end do
               write(MFILE,"(A, '_CPU_max', $)") trim(tnams(itvarlist(i)))
            end do
            
            lenstr=8
            do j=1,ISTRLEN-lenstr
               write(MFILE,'(" ", $)')
            end do
            write(MFILE,"(A, $)") 'cg_iters'
            
            write(MFILE,*)
         end if
         
!        make format strings
         write(dum, '(I0)') ISTRLEN
         FORMATSTRING    = '(E'//trim(adjustl(dum))//'.5, $)'
         FORMATSTRINGINT = '(I'//trim(adjustl(dum))//',   $)'
!         write(FORMATSTRING,   '("(E", I0, ".5, $)")') ISTRLEN
!         write(FORMATSTRINGINT,'("(I", I0, ", $)"  )') ISTRLEN
         
!        write time
         write(MFILE,trim(FORMATSTRING)) dtime
         
!        write number of timesteps
         write(MFILE,trim(FORMATSTRINGINT)) numtsteps
         
!        wite timings
         do i=1,Ntvarlist
            write(MFILE,FORMATSTRING) t_ave(3,itvarlist(i))
            write(MFILE,FORMATSTRING) t_max(3,itvarlist(i))
            write(MFILE,FORMATSTRING) tcpu_ave(3,itvarlist(i))
            write(MFILE,FORMATSTRING) tcpu_max(3,itvarlist(i))
         end do
         
!        write number of iterations
         write(MFILE,FORMATSTRINGINT) itsol_max
         
         write(MFILE,*) 
         
         close(MFILE)
      end if

!     deallocate local arrays
!      if ( my_rank.eq.0 ) then
         if ( allocated(t_max)    ) deallocate(t_max)
         if ( allocated(t_ave)    ) deallocate(t_ave)
         if ( allocated(tcpu_max) ) deallocate(tcpu_max)
         if ( allocated(tcpu_ave) ) deallocate(tcpu_ave)
!      end if
      
      return
   end subroutine print_timings
   
!> generate partition numbering with METIS
!!   1D links are supported now
   subroutine partition_METIS_to_idomain(Nparts, jacontiguous, method)
      use network_data
      use m_partitioninfo
      use m_metis
      use m_alloc
      use MessageHandling
      use unstruc_messages
      implicit none
      
      integer,                         intent(in) :: Nparts          !< number of partitions
      integer,                         intent(in) :: method         !< partition method. 1: K-Way, 0,2: Recursive
      integer,                         intent(in) :: jacontiguous    !< enforce contiguous domains (1) or not (0)
      
      integer                                     :: ierror
      
      integer                                     :: Ne              ! number of elements
      integer                                     :: Nn              ! number of nodes
      !integer,          allocatable, dimension(:) :: eptr, eind      ! mesh
      integer,          allocatable, dimension(:) :: vwgt            ! vertex weights, dim(Ne)
      integer,          allocatable, dimension(:) :: vsize           ! communication volume, dim(Ne)
      !integer                                     :: ncommon         ! number of common nodes between elements
      real, allocatable,             dimension(:) :: tpwgts          ! target weight of partitions, dim(Nparts)
      integer                                     :: objval          ! edgecut or total communication volume
      integer,          allocatable, dimension(:) :: npart           ! node    partition number, dim(Nn)                                                 
 !     integer                                     :: ic, k, N, ipoint, icursize
 !     integer,          allocatable, dimension(:) :: xadj, adjncy    ! Adjacency structure of a graph, using compressed storage format (CSR)
      integer,          allocatable, dimension(:) :: ncon            ! number of balancing constrains, at least 1
      real,             allocatable, dimension(:) :: ubvec           ! specify the allowed load imbalance tolerance for each constraint.=1.001 when ncon=1
!      integer,          allocatable, dimension(:) :: adjwgt          ! weights of edges
!      integer                                     :: c_xadj, c_adjncy, nod_j, edgej_k, nlinkj, icellj, cellk, ilink, ncellj ! these variables are used when build dual graph for 1d network
!      integer                                     :: j, kk, i, length, n0, ierr, L, nc1, nc2, k1, k2
      
      integer,          allocatable, dimension(:)  :: iadj_tmp
      integer,          allocatable, dimension(:)  :: iadj, jadj, adjw
      integer                                      :: i, k, k1, k2, L, LL
      
      integer,                       external      :: metisopts
    
      ierror = 1
      
!     check validity of objected number of subdomains
      if ( Nparts.lt.1 ) then
         call qnerror('partition_METIS_to_idomain: number of subdomains < 1', ' ', ' ')
         goto 1234
      end if
      
!     number of nodes shared by two cells on each side of an edge
      !ncommon = 2
      
      !if ( netstat.eq.NETSTAT_CELLS_DIRTY ) then
      !   call findcells(0)
      !   call find1dcells()
      !endif
      
      Ne = nump1d2d
!      if (nump < nump1d2d) Ne = nump1d2d   ! if 1d netlink exists
!      Nn = numk
      
!     deallocate
      if ( allocated(idomain) ) deallocate(idomain)
      
!     allocate
      allocate(idomain(Ne))
      
#ifdef HAVE_METIS
!     allocate
      !allocate(eptr(Ne+1))
      !allocate(eind(4*Ne))
      allocate(vwgt(Ne))
      allocate(vsize(Ne))
      allocate(tpwgts(Nparts))
      allocate(npart(Ne))     
!      allocate(adjwgt(10*Ne))
      allocate(ncon(1))
      allocate(ubvec(1))
      
!     set default options
      call METIS_SetDefaultOptions(opts)
      
      if ( jacontiguous.eq.1 .and. method .eq. 1) then
         ierror = metisopts(opts,"CONTIG",1)   ! enforce contiguous domains, observation: number of cells per domain becomes less homogeneous
         if ( ierror.ne.0 ) goto 1234
      endif
!      i = metisopts(opts,"NCUTS",10)
      ierror = metisopts(opts,"DBGLVL",1)     ! output
      if ( ierror.ne.0 ) goto 1234
      
      ierror = metisopts(opts,"UFACTOR",1)    ! allowed load imbalance TODO, MJ: should be an integer x, and tolerance is (1+x)/1000 according to manual, but 1+x/1000 according to us and "macros.h"
      if ( ierror.ne.0 ) goto 1234
      
      ierror = metisopts(opts,"NITER",100)    ! observation: increasing this number will visually improve the partitioning
      if ( ierror.ne.0 ) goto 1234
      
      vwgt   = 1                         ! weights of vertices
      vsize  = 1                         ! size of vertices for computing the total communication volume
      tpwgts = 1d0/dble(Nparts)          ! desired weight for each partition
!      adjwgt = 1                         ! weight of edges
      ncon = 1                           ! number of balancing constraints
      ubvec = 1.001                      ! allowed load imbalance tolerance

!!     make mesh
!      ipoint   = 1
!      icursize = size(eind)
!      do ic=1,nump
!         eptr(ic) = ipoint
!         N=netcell(ic)%N
!         do k=1,N
!!           reallocate if necessary            
!            if ( ipoint.gt.icursize ) then
!               icursize = int(1.2d0*ipoint) + 1
!               call realloc(eind, icursize, keepExisting=.true.)
!            end if
!            eind(ipoint) = netcell(ic)%nod(k)
!            ipoint = ipoint+1
!         end do
!      end do
!      eptr(nump+1) = ipoint
!      
!!     make mesh arrays zero-based
!      eptr = eptr-1
!      eind = eind-1
      
      
      
      
!     generate adjacency structure in CSR format 
      allocate(iadj(nump1d2d+1))
      allocate(iadj_tmp(nump1d2d+1))
      
!     count number of connection per vertex
      iadj_tmp = 0
      do L=1,numL
         if ( lnn(L).gt.1 ) then
            k1 = iabs(lne(1,L))
            k2 = iabs(lne(2,L))
            iadj_tmp(k1) = iadj_tmp(k1) + 1
            iadj_tmp(k2) = iadj_tmp(k2) + 1
         end if
      end do
      
!     set startpointers
      iadj(1) = 1
      do k=1,nump1d2d
         iadj(k+1) = iadj(k) + iadj_tmp(k)
      end do
      
!     set connections      
      allocate(jadj(iadj(nump1d2d+1)-1))
      
      iadj_tmp = iadj
      do L=1,numL
         if ( lnn(L).gt.1 ) then
            k1 = iabs(lne(1,L))
            k2 = iabs(lne(2,L))
            jadj(iadj_tmp(k1)) = k2
            jadj(iadj_tmp(k2)) = k1
            iadj_tmp(k1) = iadj_tmp(k1)+1
            iadj_tmp(k2) = iadj_tmp(k2)+1
         end if
      end do
      
!     edge weights
      allocate(adjw(iadj(nump1d2d+1)-1))
      adjw = 1
      
!     make CSR arrays zero-based
      iadj = iadj-1
      jadj = jadj-1
      
      netstat = NETSTAT_CELLS_DIRTY
!      
!      !call METIS_PARTMESHDUAL(Ne, Nn, eptr, eind, vwgt, vsize, ncommon, Nparts, tpwgts, opts, objval, idomain, npart)
!      
      if ( method.eq.1 ) then
         call METIS_PartGraphKway(Ne, Ncon, iadj, jadj, vwgt, vsize, adjw, Nparts, tpwgts, ubvec, opts, objval, idomain)
      else
         call METIS_PartGraphRecursive(Ne, Ncon, iadj, jadj, vwgt, vsize, adjw, Nparts, tpwgts, ubvec, opts, objval, idomain)
      endif
      
!     deallocate
      !if ( allocated(eptr)   ) deallocate(eptr)
      !if ( allocated(eind)   ) deallocate(eind)
      if ( allocated(vwgt)   ) deallocate(vwgt)
      if ( allocated(vsize)  ) deallocate(vsize)
      if ( allocated(tpwgts) ) deallocate(tpwgts)
 !     if ( allocated(xadj)   ) deallocate(xadj)
 !     if ( allocated(adjncy) ) deallocate(adjncy)
      if ( allocated(npart) ) deallocate(npart)
      if ( allocated(Ncon)  ) deallocate(ncon)
      if ( allocated(ubvec) ) deallocate(ubvec)
      
      if ( allocated(iadj) )     deallocate(iadj)
      if ( allocated(iadj_tmp) ) deallocate(iadj_tmp)
      if ( allocated(jadj) )     deallocate(jadj)
      if ( allocated(adjw) )     deallocate(adjw)
      
#else
      idomain = 0
      call mess(LEVEL_ERROR, 'This version was built without the METIS mesh partitioner support, '&
                    //'so the option of partitioning a mesh is not available.')
#endif

      ierror=0
 1234 continue

      return
   end subroutine partition_METIS_to_idomain
   
!  set METIS options, returns error (1) or no error (0)   
   integer function metisopts(opts,optionname,optionval)
      implicit none
      
      integer,            intent(inout) :: opts(*)         ! options array
      character(len=*),   intent(in)    :: optionname   ! option name
      integer,            intent(in)    :: optionval       ! option value
#ifdef HAVE_METIS      
      integer           :: i
      
      integer, external :: metisoptions
      
      i = metisoptions(opts,trim(optionname)//char(0),optionval)
      
      metisopts = i
#else
      metisopts = 1
#endif
   end function metisopts
   
   
!> generate partition numbers from polygons, or with METIS of no polygons are present
   subroutine partition_to_idomain()
      
      use m_polygon
      use m_partitioninfo
      use MessageHandling
      use gridoperations
      
      implicit none
   
      integer             :: japolygon
      integer             :: i, jacontiguous, method
      integer             :: NPL_save
      integer             :: ierror
      
      character(len=100)  :: message
      
!     save polygons
      NPL_save = NPL
      
!     disable polygons
      NPL = 0
      
      call findcells(0)
      call find1dcells()
      
!     reenable polygons
      NPL = NPL_save
   
      call delete_dry_points_and_areas()
      
      if ( NPL.gt.1 ) then ! use the polygons
         call generate_partitioning_from_pol()
      else  ! use metis
         Ndomains = 0
         do while ( Ndomains.lt.1 )
            call getint('Number of domains', Ndomains)
         end do
         method = -1
         do while ( method.lt.0 .or. method.gt.2 )
             method = 0
             call getint('Partition method? (0:default, 1: K-Way, 2: Recursive bisection)', method) ! default method is Recursive
         enddo
         jacontiguous = -1
         if ( method.eq.1 ) then ! K-Way enables contiguous
             do while ( jacontiguous.ne.0 .and. jacontiguous.ne.1 )
                jacontiguous = 0
                call getint('Enforce contiguous domains? (0:no, 1:yes)', jacontiguous)
             enddo
         endif
         call partition_METIS_to_idomain(Ndomains, jacontiguous, method)
      
         japolygon = -1
         do while (japolygon.ne.1 .and. japolygon.ne.0)
             japolygon = 0
             call getint('Generate polygon? (0: no, 1: yes)', japolygon)
         enddo
         if ( japolygon.eq.1 ) then
!            generate partitioning polygons
             call generate_partition_pol_from_idomain(ierror)
         
             if ( ierror.eq.0 ) then
!               get 1D domain numbers from polygons
                call partition_pol_to_idomain(2)
             else
                japolygon = 0
             end if
         endif 
      end if   
      
!     deallocate
      if ( allocated(numndx) ) deallocate(numndx)
      
!     allocate numndx
      allocate(numndx(0:ndomains-1))
      
!     count and output number of cells
      do i=0,ndomains-1
         numndx(i) = count(idomain.eq.i)
         write(message, "('domain', I5, ' contains', I7, ' cells.')") i, numndx(i)
         call mess(LEVEL_INFO, message)
      end do
      
!     BEGIN DEBUG
!      other_domain=0
!      call getint('Other domain: ', other_domain)
!      call partition_setghost_params(9)
!      call partition_set_ghostlevels(other_domain,numlay_cellbased+1,numlay_nodebased+1,ierror)
!     END DEBUG
      
      return
   end subroutine partition_to_idomain
   
   
!!> get overlapping nodes (in solver only)
!!>     {k| 1 <= ghostlev_cellbased(k) < minghostlev_s}
!!>   overlapping nodes are put in solution vector
!   subroutine partition_getoverlappingnodes()
!      use m_partitioninfo
!      use m_flowgeom, only: Ndx
!      use m_alloc
!      implicit none
!      
!      integer :: iglev, k, num, nsize
!      
!      nsize = 10  ! array size
!      
!      if ( allocated(ioverlap) ) deallocate(ioverlap)
!      allocate(ioverlap(nsize))
!      ioverlap=0
!       
!      noverlap = 0
!      
!      num = size(idomain)
!      
!      do k=1,num
!         if ( idomain(k).eq.my_rank ) cycle ! ghost cells only
!         
!!        select nodes with 1<=cell-based ghostlevel<minghostlev_s
!         if ( ighosttype_s.eq.IGHOSTTYPE_CELLBASED ) then
!            iglev = ighostlev_cellbased(k)
!         else if ( ighosttype_s.eq.IGHOSTTYPE_NODEBASED ) then
!            iglev = ighostlev_nodebased(k)
!         else  ! combined
!            iglev = ighostlev(k)
!         end if
!         
!         if ( iglev.ge.1 .and. iglev.lt.minghostlev_s ) then
!            noverlap = noverlap+1
!            
!!           reallocate if necessary
!            if ( nsize.lt.noverlap ) then
!               nsize = int(1.2d0*dble(nsize)+1d0)
!               call realloc(ioverlap,nsize,keepExisting=.true.,fill=0)
!            end if
!            
!            ioverlap(noverlap) = k
!         end if
!      end do
!!      
!      return
!   end subroutine partition_getoverlappingnodes

   subroutine writemesg(mesg)
#ifdef HAVE_MPI
      use mpi
#endif
      use m_partitioninfo

      implicit none

      character(len=*), intent(in) :: mesg

      integer :: ierr
         
#ifdef HAVE_MPI
      call mpi_comm_rank(DFM_COMM_DFMWORLD,my_rank,ierr)
#endif
      call flush(6)

      if ( my_rank.eq.0 ) then
         write(6,*) trim(mesg)
      end if

      call flush(6)
#ifdef HAVE_MPI
      call MPI_barrier(DFM_COMM_DFMWORLD,ierr)
#endif
      return
    end subroutine writemesg

    
    !> reduce balences
   subroutine reduce_bal(voltotal,numidx)
#ifdef HAVE_MPI
      use mpi
#endif
      use m_partitioninfo

      implicit none
      
      integer,                                   intent(in)    :: numidx             !< which values to sum (1=discharge)
      double precision, dimension(numidx),       intent(inout) :: voltotal  !< cross-section data, note: ncrs from module m_monitoring_crosssections
      double precision, dimension(:),            allocatable   :: voltot_all
      integer                                                  :: ierror
      
#ifdef HAVE_MPI
!     allocate
      allocate(voltot_all(numidx))
      
      call mpi_allreduce(voltotal,voltot_all,numidx,mpi_double_precision,mpi_sum,DFM_COMM_DFMWORLD,ierror)
      
      voltotal = voltot_all
      
!     deallocate
      if ( allocated(voltot_all) ) deallocate(voltot_all)
#endif
      return
   end subroutine reduce_bal

   
   
!> disable mirror cells that are not mirror cells in the whole model by setting kce=0
!!    note: partition information not available yet, need to perform a manual handshake
   subroutine partition_reduce_mirrorcells(Nx, kce, ke, ierror)
      use m_partitioninfo
      use network_data
      use m_alloc
      use unstruc_messages
      use m_flowgeom, only: ndx, ndxi
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D
      
#ifdef HAVE_MPI
      use mpi
#endif
      implicit none
      
      integer,                           intent(in)    :: Nx      !< number of links
      integer,          dimension(Nx),   intent(inout) :: kce     !< flag
      integer,          dimension(Nx),   intent(in)    :: ke      !< boundary cells
      
      integer,                           intent(out)   :: ierror  !< error (1) or not (0)
      
#ifdef HAVE_MPI 
      double precision, dimension(:,:),  allocatable   :: xysnd        ! send     cell-center coordinates (first x, then y)
      double precision, dimension(:,:),  allocatable   :: xyrec        ! recieved cell-center coordinates (first x, then y)
                                         
      integer,          dimension(:,:),  allocatable   :: numrequest   ! number of cells requested from other domains (message size)
      integer,          dimension(:),    allocatable   :: numrequest_loc
      
      integer,          dimension(:),    allocatable   :: irequest
      integer,          dimension(:),    allocatable   :: kcesnd
      integer,          dimension(:),    allocatable   :: kcerec
      integer,          dimension(:),    allocatable   :: jafound
      
      integer,          dimension(MPI_STATUS_SIZE)     :: istat
      
      character(len=1024)                              :: str
      
      double precision                                 :: xL, yL, dis
      
      double precision                                 :: t0, t1, t2, t3, timefind1, timefind2
      
      integer                                          :: Nbnd    ! number of boundary links
      
      integer                                          :: i, L, k3, k4, num
      integer                                          :: idmn, other_domain
      integer                                          :: nrequest, itag, icount
      integer                                          :: numfound
      integer                                          :: istart
      
      integer                                          :: numdisabled
      
      double precision, parameter                      :: dtol = 1d-4
      
      call klok(t0)
      
      ierror = 1
      
      itag = 2
      
!     get subdomain numbers in netcell  
      if ( npartition_pol.gt.0 ) then
         call partition_pol_to_idomain(1,jafindcells=0)
      endif
      
!     allocate
      allocate(numrequest(0:ndomains-1,0:ndomains-1))
      numrequest = 0
      allocate(numrequest_loc(0:ndomains-1))
      numrequest_loc = 0
      allocate(irequest(0:2*ndomains-1))
!     allocate xysnd sufficiently large
      allocate(xysnd(3,numL))
      xysnd = 0d0
!     allocate kcesnd sufficiently large
      call realloc(kcesnd, numL, keepExisting=.false., fill=0)
      
!     count number of requested boundary links from other subdomains
      Nbnd = 0
      do L=1,numL
         if ( kce(L).eq.1 ) then
            Nbnd = Nbnd+1
            idmn = idomain(ke(L))
            if ( idmn.ne.my_rank .and. idmn.ge.0 .and. idmn.le.ndomains-1 ) then
               numrequest_loc(idmn) = numrequest_loc(idmn)+1
            end if
         end if
      end do
      
!     globally reduce
      call mpi_allgather(numrequest_loc, ndomains, MPI_INTEGER, numrequest, ndomains, MPI_INTEGER, DFM_COMM_DFMWORLD, ierror)
      
!     send own ghost boundary net links to other domain     
      nrequest = 0   ! number of outgoing requests
      istart = 1     ! start index in xysnd
      do other_domain=0,ndomains-1
         if ( other_domain.eq.my_rank ) cycle
         num = numrequest(other_domain, my_rank)
         if ( num.lt.1 ) cycle
         
!        get link center coordinates
         num = 0
         do L=1,numL
            if ( kce(L).eq.1 ) then
               if ( idomain(ke(L)).eq.other_domain ) then
                  k3 = kn(1,L)
                  k4 = kn(2,L)
                  xysnd(1,istart+num) = 0.5d0*(xk(k3)+xk(k4))
                  xysnd(2,istart+num) = 0.5d0*(yk(k3)+yk(k4))
                  xysnd(3,istart+num) = dble(kn(3,L))  ! also send type of netlink
                  num = num+1
               end if
            end if
         end do
         
!        send link center coordinates
         nrequest = nrequest+1
         call mpi_isend(xysnd(1,istart), 3*num, mpi_double_precision, other_domain, itag, DFM_COMM_DFMWORLD, irequest(nrequest), ierror)
         
!        update start index
         istart = istart+num
      end do
      
!     recieve requests from other domains
      timefind1 = 0d0 ! time spent in finding netlinks
      timefind2 = 0d0 ! time spent in finding netlinks
      istart = 1
      do other_domain=0,ndomains-1
         num = numrequest(my_rank,other_domain)
         
         
!        BEGIN DEBUG
!         if ( my_rank.eq.1 .and. other_domain.eq.18 ) then
!            write(6,*) 'my_rank=', my_rank, 'other_domain=', other_domain, 'num=', num, 'jafound=', jafound(i)
!         end if
!        END DEBUG
         
         if ( num.lt.1 ) cycle
!        get message length
         call mpi_probe(other_domain,itag,DFM_COMM_DFMWORLD,istat,ierror)
         call mpi_get_count(istat,mpi_double_precision,icount,ierror)
         
!        check message length (safety)
         if ( icount.ne.3*num ) then
            write(str, *) 'partition_reduce_mirrorcells: icount.ne.3*num, domain: ', my_rank, ', other domain: ', other_domain, ' icount: ', icount, ', 3*num: ', 3*num
            call mess(LEVEL_ERROR, str)
         end if
         
!        realloc
         call realloc(xyrec, (/ 3,num /), keepExisting=.false., fill=0d0)

!        recieve
         call mpi_recv(xyrec,icount,mpi_double_precision,other_domain,MPI_ANY_TAG,DFM_COMM_DFMWORLD,istat,ierror)
         
         
!        realloc
         call realloc(jafound, num, keepExisting=.false., fill=0)
         numfound = 0
         call klok(t2)
   Lloop:do L=1,numL
            if ( kce(L).ne.1 ) cycle  ! boundary links only
            if ( idomain(ke(L)).ne.my_rank ) cycle  ! in own domain only

            do i=1,num
               if ( jafound(i).eq.1 ) cycle
               
               if ( int(xyrec(3,i)).ne.kn(3,L) ) cycle   ! check netlink type
               
!              get netlink coordinates
               k3 = kn(1,L)
               k4 = kn(2,L)
               xL = 0.5d0*(xk(k3)+xk(k4))
               yL = 0.5d0*(yk(k3)+yk(k4))
               
!              measure distance
               dis = dbdistance(xL,yL,xyrec(1,i),xyrec(2,i),jsferic, jasfer3D, dmiss)
               if ( dis.lt.dtol ) then ! found
                  kcesnd(istart-1+i)  = 1
                  jafound(i) = 1
                  numfound = numfound+1
                  if ( numfound.ge.num ) exit Lloop
               end if
            end do
         end do Lloop
         call klok(t3)
         timefind2 = timefind2+t3-t2
         
         
!!        BEGIN DEBUG
!         write(6,*) '-----------DEBUG-----------'
!         do i=1,num
!            if ( xyrec(1,i).ge.-1.87d0 .and. xyrec(1,i).le.-1.47d0 .and. xyrec(2,i).ge.63.9d0 .and. xyrec(2,i).le.64.0d0 ) then
!               write(6,*) 'my_rank=', my_rank, 'other_domain=', other_domain, 'i=', i, xyrec(1,i), xyrec(2,i), xyrec(3,i), jafound(i)
!            end if
!         end do
!         
!         if ( my_rank.eq.1 .and. other_domain.eq.18 ) then
!            open(666,file='temp.xyz', status='replace', action='write')
!            do i=1,num
!               write(666,"(3E15.5)") xyrec(1:3,i)
!            end do
!            close(666)
!            stop
!         end if
!!        END DEBUG
         
!        send kce to other subdomains
         nrequest = nrequest+1
         call mpi_isend(kcesnd(istart), num, mpi_integer, other_domain, itag, DFM_COMM_DFMWORLD, irequest(nrequest), ierror) 
         
         istart = istart+num
      end do   ! other_domain=0,ndomains-1
      
      
      
!      write(str,"('partition_reduce_mirrorcells, time spent in finding netlinks, method 1: ', G15.5, 's.')") timefind1
!      call mess(LEVEL_INFO, trim(str))
!      write(str,"('partition_reduce_mirrorcells, time spent in finding netlinks, method 2: ', G15.5, 's.')") timefind2
!      call mess(LEVEL_INFO, trim(str))
      
      numdisabled = 0
      
!     recieve kcesnd from other domains
      do other_domain=0,ndomains-1
         num = numrequest(other_domain,my_rank)
         if ( num.lt.1 ) cycle
!        get message length
         call mpi_probe(other_domain,itag,DFM_COMM_DFMWORLD,istat,ierror)
         call mpi_get_count(istat,mpi_integer,icount,ierror)
         
!        check message length (safety)
         if ( icount.ne.num ) then
            write(str, *) 'partition_reduce_mirrorcells: icount.ne.num, domain: ', my_rank, ', other domain: ', other_domain, ' icount: ', icount, ', num: ', num
            call mess(LEVEL_ERROR, str)
         end if
         
!        realloc
         call realloc(kcerec, icount, keepExisting=.false., fill=0)

!        recieve 
         call mpi_recv(kcerec,icount,mpi_double_precision,other_domain,MPI_ANY_TAG,DFM_COMM_DFMWORLD,istat,ierror)
         
!        update kce
         num = 0
         do L=1,numL
            if ( kce(L).eq.1 ) then
               if ( idomain(ke(L)).eq.other_domain ) then
                  num = num+1
                  if ( kce(L).ne.kcerec(num) ) then
                     numdisabled = numdisabled+1
                     kce(L) = kcerec(num)
                  end if
               end if
            end if
         end do
      end do
      
!     terminate send (safety)
      do i=1,nrequest
         call mpi_wait(irequest(i),istat,ierror)
      end do
      
      if ( numdisabled.gt.0 ) then
         write(str, "('disabled ', I0, ' boundary links')") numdisabled
         call mess(LEVEL_INFO, trim(str))
      end if
      
      call klok(t1)
      
      write(str,"('partition_reduce_mirrorcells, elapsed time: ', G15.5, 's.')") t1-t0
      call mess(LEVEL_INFO, trim(str))
      
      ierror = 0
1234  continue

!     deallocate
      if ( allocated(irequest)       ) deallocate(irequest)
      if ( allocated(numrequest)     ) deallocate(numrequest)
      if ( allocated(numrequest_loc) ) deallocate(numrequest_loc)
      if ( allocated(xysnd)          ) deallocate(xysnd)
      if ( allocated(xyrec)          ) deallocate(xyrec)
      if ( allocated(kcesnd)         ) deallocate(kcesnd)
      if ( allocated(kcerec)         ) deallocate(kcerec)
      if ( allocated(jafound)        ) deallocate(jafound)
      
#endif
      return
   end subroutine partition_reduce_mirrorcells
   
!> see if a discharge boundary is partitioned and set japartqbnd
   subroutine set_japartqbnd()
      use m_flowexternalforcings
      use m_partitioninfo
#ifdef HAVE_MPI
      use mpi
#endif
      implicit none
      
      integer :: n, nq
      integer :: ki
      integer :: japartqbnd_all
      
      integer :: ierror
      
      japartqbnd = 0
      
#ifdef HAVE_MPI
      
 mnlp:do nq=1,nqbnd
         do n=L1qbnd(nq),L2qbnd(nq)  ! apart
            ki = kbndu(2,n)
            
            if ( idomain(ki).ne.my_rank ) then
               japartqbnd = 1
               exit mnlp
            end if
         end do
      end do mnlp
      
!     not all subdomains may have detected a partitioned q boundary: reduce japartqbnd
      call mpi_allreduce(japartqbnd,japartqbnd_all,1,mpi_integer,mpi_max,DFM_COMM_DFMWORLD,ierror)
      japartqbnd = japartqbnd_all
      
#endif
      
      return
   end subroutine set_japartqbnd
   
   
!> update values in boundaries (these are not in ghost lists), 2D only
   subroutine update_ghostboundvals(itype, NDIM, N, var, jacheck, ierror)
      use m_partitioninfo
      use m_flowgeom
      use m_alloc
      use m_missing
      use unstruc_messages
      implicit none
      
      integer,                                     intent(in)    :: itype        !< type: 0: flownode, 1: flowlink
      integer,                                     intent(in)    :: NDIM         !< number of unknowns per flownode/link
      integer,                                     intent(in)    :: N            !< number of flownodes/links
      double precision, dimension(NDIM*N),         intent(inout) :: var          !< solution
      integer,                                     intent(in)    :: jacheck      !< check if boundary flowlinks are updated (1) or not (0)
      integer,                                     intent(out)   :: ierror       !< error (1) or not (0)
      
      double precision, dimension(:), allocatable                :: dum
      
      integer,          dimension(:), allocatable                :: Lbndmask
      
      integer                                                    :: i, L
      
      ierror = 1
      
      if ( jacheck.eq.1 .and. Lnx.gt.Lnxi ) then
!        check if all ghost boundary flowlinks are being update
         allocate(Lbndmask(1:Lnx-Lnxi+1))
         Lbndmask = 0
         
!        mask updated boundary flowlinks         
         do i=1,nghostlist_u(ndomains-1)
             L = ighostlist_u(i)
             if ( L.gt.Lnxi ) then
                Lbndmask(L-Lnxi+1) = 1
             end if
         end do
         
!        check if all ghost boundary flowlinks are being updated
         do L=Lnxi+1,Lnx   ! boundary links
            if ( idomain(ln(2,L)).ne.my_rank ) then   ! ghost link
               if ( Lbndmask(L-Lnxi+1).ne.1 ) then ! not masked
                  call mess(LEVEL_ERROR, 'update_ghostboundvals: not all ghost boundary flowlinks are being updated')
                  goto 1234
               end if
            end if
         end do
         
!        deallocate         
         if ( allocated(Lbndmask) ) deallocate(Lbndmask)
      end if
      
!     allocate and initialize      
      call realloc(dum, NDIM*Lnx, fill=DMISS)
      
!     fill internal boundary-node values with boundary values
      if ( itype.eq.ITYPE_S .or. itype.eq.ITYPE_Sall ) then
         do L=Lnxi+1,Lnx
            do i=1,NDIM
               dum(NDIM*(L-1)+i)=var(NDIM*(ln(1,L)-1)+i)
            end do
         end do
      else if ( itype.eq.ITYPE_U ) then
         do L=Lnxi+1,Lnx
            do i=1,NDIM
               dum(NDIM*(L-1)+i)=var(NDIM*(L-1)+i)
            end do
         end do
      else
         call qnerror(' update_ghostboundvals: unknown ghost type', ' ', ' ')
         goto 1234
      end if
      
!     update ghost values
      call update_ghosts(ITYPE_U, NDIM, Lnx, dum, ierror)
      if ( ierror.ne.0 ) goto 1234
      
!     copy internal boundary-node values to boundary values   
      if ( itype.eq.ITYPE_S .or. itype.eq.ITYPE_Sall ) then
         do L=Lnxi+1,Lnx
            do i=1,NDIM
               var(NDIM*(ln(1,L)-1)+i)=dum(NDIM*(L-1)+i)
            end do
         end do
      else if ( itype.eq.ITYPE_U ) then
         do L=Lnxi+1,Lnx
            do i=1,NDIM
               var(NDIM*(L-1)+i)=dum(NDIM*(L-1)+i)
            end do
         end do
      else
         call qnerror(' update_ghostboundvals: unknown ghost type', ' ', ' ')
         goto 1234
      end if   
      
      ierror = 0
 1234 continue
      
!     deallocate 
      if ( allocated(dum) ) deallocate(dum)
      
      return
   end subroutine update_ghostboundvals

! =================================================================================================
! =================================================================================================
   subroutine fill_reduce_buffer(vals, nvals)
      use m_partitioninfo
      implicit none
      integer                              :: i
      integer,          intent(in)         :: nvals
      double precision, dimension(1:nvals) :: vals

      if ( jampi == 0 ) then
         return
      end if

      do i = 1,nvals
         reducebuf(nreducebuf + i) = vals(i)
      enddo
      nreducebuf = nreducebuf + nvals

   end subroutine fill_reduce_buffer

! =================================================================================================
! =================================================================================================
   subroutine subsitute_reduce_buffer(vals, nvals)
      use m_partitioninfo
      implicit none
      integer                              :: i
      integer,          intent(in)         :: nvals
      double precision, dimension(1:nvals) :: vals

      nreducebuf = nreducebuf - nvals
      do i = 1,nvals
         vals(i) = reducebuf(nreducebuf + i)
      enddo

   end subroutine subsitute_reduce_buffer