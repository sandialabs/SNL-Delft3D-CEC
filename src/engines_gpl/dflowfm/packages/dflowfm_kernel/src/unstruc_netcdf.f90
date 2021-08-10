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

! $Id: unstruc_netcdf.f90 65915 2020-01-30 15:18:18Z spee $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/unstruc_netcdf.f90 $

! TODO: FB: #define NC_CHECK if(ierr .ne. 0 ) call mess(LEVEL_ERROR, nf90_strerror(ierr))

! TODO: AvD:
! * flowgeom should work now for CFOLD and UGRID. Test this before moving to map writer.
! * map writer: start adding data variables
! * waq writer: migrate to new UGRID?
! * com writer: stay at old CF for now???
! * zk in ugrid flowgeom
! * Solve all TODO's.

!> Reads and writes unstructured net/flow data in netCDF format.
module unstruc_netcdf

use precision
use netcdf
use unstruc_messages
use unstruc_version_module
use io_ugrid
use m_sediment
use string_module

implicit none

integer            :: nerr_
logical            :: err_firsttime_
character(len=255) :: err_firstline_
integer            :: err_level_

!> All NetCDF files should be opened through unc_open or unc_create,
!! such that all opened files are maintained and can be properly closed
!! upon exit of the program by unc_closeall.
integer, parameter :: maxopenfiles = 50
character(len=255) :: open_files_(maxopenfiles)    !< Names of open NetCDF files.
integer            :: open_datasets_(maxopenfiles) !< Dataset IDs of open NetCDF files.
integer            :: nopen_files_ = 0             !< Nr. of NetCDF files currently open.

private :: nerr_, err_firsttime_, err_firstline_, &
           !prepare_error, check_error, &
           open_files_, open_datasets_, nopen_files_

integer, parameter :: UNC_CONV_CFOLD = 1 !< Old CF-only conventions.
integer, parameter :: UNC_CONV_UGRID = 2 !< New CF+UGRID conventions.

integer            :: unc_cmode      = 0 !< Default NetCDF creation mode flag value, used in nf90_create calls (e.g., NF90_NETCDF4).

integer            :: unc_writeopts !< Default write options (currently only: UG_WRITE_LATLON)

! The following location codes generalize for 1D/2D/3D models. See function unc_def_var_map for the details.
integer, parameter :: UNC_LOC_CN  = 1  !< Data location: corner point.
integer, parameter :: UNC_LOC_S   = 2  !< Data location: pressure point.
integer, parameter :: UNC_LOC_U   = 3  !< Data location: horizontal velocity point.
integer, parameter :: UNC_LOC_L   = 13 !< Data location: horizontal net link.
integer, parameter :: UNC_LOC_S3D = 4  !< Data location: pressure point in all layers.
integer, parameter :: UNC_LOC_U3D = 5  !< Data location: horizontal velocity point in all layers.
integer, parameter :: UNC_LOC_W   = 6  !< Data location: vertical velocity point on all layer interfaces.
integer, parameter :: UNC_LOC_WU  = 16 !< Data location: vertical viscosity point on all layer interfaces.

integer, parameter :: MAX_ID_VAR = 4   !< Maximum dimension for id_var arrays

type(t_ug_meta) :: ug_meta_fm  !< Meta information on file.

! This type collects the time and space administration relevant for repeat writes to
! netcdf files in FM
! The original t_unc_mapids now incorporates this type for time and space dims 
!
type t_unc_timespace_id

   type(t_ug_mesh)     :: meshids1d
   type(t_ug_mesh)     :: meshids2d
   type(t_ug_mesh)     :: meshids3d
   type(t_ug_network)  :: network1d
   type(t_ug_contacts) :: meshcontacts

   !
   ! Dimensions
   !
   integer :: id_timedim = -1 !< Time dimension (the only nf90_unlimited in file).
   integer :: id_laydim  = -1 !< Layer (center) dimension. TODO: AvD: to be moved to meshids3d
   integer :: id_wdim    = -1 !< Layer interfaces dimension. TODO: AvD: to be moved to meshids3d.
   !id_flowelemdim, &
   integer :: id_maxfracdim = -1 !< 
   integer :: id_erolaydim  = -1 !< Dimension ID for location of erodable layer thickness.
   integer :: id_sedtotdim  = -1 !< Dimension ID for number of all sediment fractions.
   integer :: id_sedsusdim  = -1 !< Dimension ID for number of suspended sediment fractions. 
   ! arrays to identify 1d mesh and 1d2d contacts
   integer, allocatable :: edgetoln(:)      
   integer, allocatable :: contactstoln(:)
   ! geometry fieldss
   integer :: id_flowelemba(MAX_ID_VAR)     = -1 !< Variable ID for flow node bottom area (on 1D, 2D, 3D, 1D2D grid parts resp.).
   integer :: id_flowelembl(MAX_ID_VAR)     = -1 !< Variable ID for flow node bed level (on 1D, 2D, 3D, 1D2D grid parts resp.).
   integer :: id_flowelemcrsz(MAX_ID_VAR)   = -1 !< Variable ID for cross-section point levels passing through flow node (on 1D).
   integer :: id_flowelemcrsn(MAX_ID_VAR)   = -1 !< Variable ID for cross-section point widths passing through flow node (on 1D).
   integer :: id_jmax                       = -1
   integer :: id_netnodez(MAX_ID_VAR)       = -1 !< Variable ID for net node bed level. TODO: AvD: UNST-1318: consider removing here.
   
   integer :: id_nlyrdim    = -1 !< Dimension ID for number of bed layers in bed stratigraphy
   integer :: id_ntheta     = -1 !< Dimension ID for number of wave directional bins in surfbeat model   
   
   integer :: id_flowelemdomain(MAX_ID_VAR) = -1 ! domain number of flow elem (face)
   integer :: id_flowelemglobalnr(MAX_ID_VAR) = -1 ! global flow element numbering
   
   integer :: idx_curtime  = 0  !< Index of current time (typically of latest snapshot being written).
   
   integer :: id_strlendim = -1 !< string length for e.g. sediment fraction names. To do AvD: should this go here?

end type t_unc_timespace_id

!> This type collects all NetCDF ids that are relevant for repeated file writing.
!! Not only the file pointer, but also all variable ids, dimension ids, etc.
!! Create a separate variable of this type for each map file.
type t_unc_mapids
   !
   ! Toplevel
   !
   integer                  :: ncid = 0 !< NetCDF data set id (typically NetCDF file pointer)
   type(t_unc_timespace_id) :: id_tsp
   !type(t_ug_mesh)     :: meshids1d
   !type(t_ug_mesh)     :: meshids2d
   !type(t_ug_mesh)     :: meshids3d
   !type(t_ug_network)  :: network1d
   !type(t_ug_contacts) :: meshcontacts
   !
   !!
   !! Dimensions
   !!
   !integer :: id_timedim = -1 !< Time dimension (the only nf90_unlimited in file).
   !integer :: id_laydim  = -1 !< Layer (center) dimension. TODO: AvD: to be moved to meshids3d
   !integer :: id_wdim    = -1 !< Layer interfaces dimension. TODO: AvD: to be moved to meshids3d.
   !!id_flowelemdim, &
   !integer :: id_maxfracdim = -1 !< 
   !integer :: id_erolaydim  = -1 !< Dimension ID for location of erodable layer thickness.
   !integer :: id_sedtotdim  = -1 !< Dimension ID for number of all sediment fractions.
   !integer :: id_sedsusdim  = -1 !< Dimension ID for number of suspended sediment fractions. 
   !! arrays to identify 1d mesh and 1d2d contacts
   !integer, allocatable :: edgetoln(:)      
   !integer, allocatable :: contactstoln(:)  
   !
   !integer :: id_nlyrdim    = -1 !< Dimension ID for number of bed layers in bed stratigraphy
   !integer :: id_ntheta     = -1 !< Dimension ID for number of wave directional bins in surfbeat model
   ! TODO: AvD: replace all data var ids below by 1D/2D/3D generalization.
   !
   ! Data variables
   !
   !integer :: id_flowelemba(MAX_ID_VAR)     = -1 !< Variable ID for flow node bottom area (on 1D, 2D, 3D, 1D2D grid parts resp.).
   !integer :: id_flowelembl(MAX_ID_VAR)     = -1 !< Variable ID for flow node bed level (on 1D, 2D, 3D, 1D2D grid parts resp.).
   !integer :: id_flowelemcrsz(MAX_ID_VAR)   = -1 !< Variable ID for cross-section point levels passing through flow node (on 1D).
   !integer :: id_flowelemcrsn(MAX_ID_VAR)   = -1 !< Variable ID for cross-section point widths passing through flow node (on 1D).
   !integer :: id_jmax
   !integer :: id_netnodez(MAX_ID_VAR)       = -1 !< Variable ID for net node bed level. TODO: AvD: UNST-1318: consider removing here.
   integer :: id_time                 = -1 !< Variable ID for 
   integer :: id_timestep             = -1 !< Variable ID for 
   integer :: id_numlimdt(MAX_ID_VAR) = -1 !< Variable ID for 
   integer :: id_s1(MAX_ID_VAR)       = -1 !< Variable ID for water level (on 1D, 2D, 3D grid parts resp.)
   integer :: id_s0(MAX_ID_VAR)       = -1 !< Variable ID for 
   integer :: id_hs(MAX_ID_VAR)       = -1 !< Variable ID for 
   integer :: id_vol1(MAX_ID_VAR)     = -1 !< Variable ID for volume
   integer :: id_au(MAX_ID_VAR)       = -1 !< Variable ID for flow area
   integer :: id_taus(MAX_ID_VAR)     = -1 !< Variable ID for 
   integer :: id_ucx(MAX_ID_VAR)      = -1 !< Variable ID for 
   integer :: id_ucy(MAX_ID_VAR)      = -1 !< Variable ID for 
   integer :: id_ucz(MAX_ID_VAR)      = -1 !< Variable ID for 
   integer :: id_ucmag(MAX_ID_VAR)    = -1 !< Variable ID for 
   integer :: id_ucdir(MAX_ID_VAR)    = -1 !< Variable ID for 
   integer :: id_ucxa(MAX_ID_VAR)     = -1 !< Variable ID for 
   integer :: id_ucya(MAX_ID_VAR)     = -1 !< Variable ID for 
   integer :: id_ucmaga(MAX_ID_VAR)   = -1 !< Variable ID for 
   integer :: id_ucxq(MAX_ID_VAR)     = -1 !< Variable ID for 
   integer :: id_ucyq(MAX_ID_VAR)     = -1 !< Variable ID for
   integer :: id_hu(MAX_ID_VAR)       = -1 !< Variable ID for 
   integer :: id_q1(MAX_ID_VAR)       = -1 !< Variable ID for 
   integer :: id_q1main(MAX_ID_VAR)   = -1 !< Variable ID for main channel discharge (1D quantity)
   integer :: id_u1(MAX_ID_VAR)       = -1 !< Variable ID for 
   integer :: id_u0(MAX_ID_VAR)       = -1 !< Variable ID for 
   integer :: id_viu(MAX_ID_VAR)      = -1 !< Variable ID for horizontal eddy viscosity
   integer :: id_diu(MAX_ID_VAR)      = -1 !< Variable ID for horizontal eddy diffusivity
   integer :: id_ww1(MAX_ID_VAR)      = -1 !< Variable ID for 
   integer :: id_rho(MAX_ID_VAR)      = -1 !< Variable ID for 
   integer :: id_sa1(MAX_ID_VAR)      = -1 !< Variable ID for 
   integer :: id_tem1(MAX_ID_VAR)     = -1 !< Variable ID for 
   integer, dimension(:,:), allocatable :: id_const !< Variable ID for (3, NUM_CONST) constituents (on 1D, 2D, 3D grid parts resp.)
   integer, dimension(:,:), allocatable :: id_wqb !< Variable ID for (3, numwqbots) water quality bottom variables output (on 2D grid only)
   integer, dimension(:,:), allocatable :: id_waq !< Variable ID for (3, noout) waq output (on 1D, 2D, 3D grid parts resp.)
   integer, dimension(:,:), allocatable :: id_wqst !< Variable ID for (3, noout) waq time stat output (on 1D, 2D, 3D grid parts resp.)
   integer, dimension(:,:), allocatable :: id_wqse !< Variable ID for (3, noout) waq end stat output (on 1D, 2D, 3D grid parts resp.)
   integer :: id_mba(MAX_ID_VAR)    = -1 !< Variable ID for mass balance areas 
   integer, dimension(:,:), allocatable :: id_sed !< Variable ID for 
   integer, dimension(:,:), allocatable :: id_ero !< Variable ID for 
   integer :: id_cfcl(MAX_ID_VAR)        = -1 !< Variable ID for netlink data of calibration factor for friction 
   integer :: id_cftrt(MAX_ID_VAR)       = -1 !< Variable ID for netlink data of friction from trachytopes
   integer :: id_czs(MAX_ID_VAR)         = -1 !< Variable ID for flow node data of chezy roughness 
   integer :: id_qsun(MAX_ID_VAR)        = -1 !< Variable ID for 
   integer :: id_qeva(MAX_ID_VAR)        = -1 !< Variable ID for 
   integer :: id_qcon(MAX_ID_VAR)        = -1 !< Variable ID for 
   integer :: id_qlong(MAX_ID_VAR)       = -1 !< Variable ID for 
   integer :: id_qfreva(MAX_ID_VAR)      = -1 !< Variable ID for 
   integer :: id_qfrcon(MAX_ID_VAR)      = -1 !< Variable ID for 
   integer :: id_qtot(MAX_ID_VAR)        = -1 !< Variable ID for 
   integer :: id_rain(MAX_ID_VAR)        = -1 !< Variable ID for 
   integer :: id_wind(MAX_ID_VAR)        = -1 !< Variable ID for 
   integer :: id_patm(MAX_ID_VAR)        = -1 !< Variable ID for 
   integer :: id_tair(MAX_ID_VAR)        = -1 !< Variable ID for 
   integer :: id_rhum(MAX_ID_VAR)        = -1 !< Variable ID for 
   integer :: id_clou(MAX_ID_VAR)        = -1 !< Variable ID for 
   integer :: id_E(MAX_ID_VAR)           = -1 !< Variable ID for 
   integer :: id_R(MAX_ID_VAR)           = -1 !< Variable ID for 
   integer :: id_hwav(MAX_ID_VAR)        = -1 !< Variable ID for 
   integer :: id_D(MAX_ID_VAR)           = -1 !< Variable ID for 
   integer :: id_DR(MAX_ID_VAR)          = -1 !< Variable ID for 
   integer :: id_Df(MAX_ID_VAR)          = -1 !< Variable ID for 
   integer :: id_uorb(MAX_ID_VAR)        = -1 !< Variable ID for 
   integer :: id_thetamean(MAX_ID_VAR)   = -1 !< Variable ID for 
   integer :: id_cwav(MAX_ID_VAR)        = -1 !< Variable ID for 
   integer :: id_cgwav(MAX_ID_VAR)       = -1 !< Variable ID for 
   integer :: id_kwav(MAX_ID_VAR)        = -1 !< Variable ID for
   integer :: id_nwav(MAX_ID_VAR)        = -1
   integer :: id_l1(MAX_ID_VAR)          = -1
   integer :: id_ctheta(MAX_ID_VAR)      = -1
   integer :: id_sigmwav(MAX_ID_VAR)     = -1 !< Variable ID for
   integer :: id_SwE(MAX_ID_VAR)         = -1 !< Variable ID for wind source term on E
   integer :: id_SwT(MAX_ID_VAR)         = -1 !< Variable ID for wind source term on T   
   integer :: id_ustokes(MAX_ID_VAR)     = -1 !< Variable ID for 
   integer :: id_vstokes(MAX_ID_VAR)     = -1 !< Variable ID for 
   integer :: id_Fx(MAX_ID_VAR)          = -1 !< Variable ID for 
   integer :: id_Fy(MAX_ID_VAR)          = -1 !< Variable ID for
   integer :: id_Fxlink(MAX_ID_VAR)      = -1 !< Variable ID for
   integer :: id_Fylink(MAX_ID_VAR)      = -1 !< Variable ID for
   integer :: id_ustokeslink(MAX_ID_VAR) = -1
   integer :: id_vstokeslink(MAX_ID_VAR) = -1
   integer :: id_Sxx(MAX_ID_VAR)         = -1
   integer :: id_Syy(MAX_ID_VAR)         = -1
   integer :: id_Sxy(MAX_ID_VAR)         = -1
   integer :: id_dsdx(MAX_ID_VAR)        = -1
   integer :: id_dsdy(MAX_ID_VAR)        = -1
   integer :: id_ducxdx(MAX_ID_VAR)      = -1
   integer :: id_ducxdy(MAX_ID_VAR)      = -1
   integer :: id_ducydx(MAX_ID_VAR)      = -1
   integer :: id_ducydy(MAX_ID_VAR)      = -1
   integer :: id_windx(MAX_ID_VAR)       = -1 !< Variable ID for wind on cell center, x-component
   integer :: id_windy(MAX_ID_VAR)       = -1 !< Variable ID for wind on cell center, y-component
   integer :: id_windxu(MAX_ID_VAR)      = -1 !< Variable ID for wind on flow links, x-component
   integer :: id_windyu(MAX_ID_VAR)      = -1 !< Variable ID for wind on flow links, y-component
   integer :: id_windstressx(MAX_ID_VAR) = -1  !< Variable ID for wind stress, on cell center, x-component
   integer :: id_windstressy(MAX_ID_VAR) = -1  !< Variable ID for wind stress, on cell center, y-component
   integer :: id_turkin1(MAX_ID_VAR)     = -1 !< Variable ID for 
   integer :: id_vicwwu(MAX_ID_VAR)      = -1 !< Variable ID for 
   integer :: id_tureps1(MAX_ID_VAR)     = -1 !< Variable ID for 
   integer :: id_sbcx(MAX_ID_VAR)        = -1 !< Variable ID for current related bedload sediment transport at cell centre before upwinding, secondary flow and bed slope effect (x-component)
   integer :: id_sbcy(MAX_ID_VAR)        = -1 !< Variable ID for current related bedload sediment transport at cell centre before upwinding, secondary flow and bed slope effect (y-component)
   integer :: id_sbcx_reconstructed(MAX_ID_VAR)     = -1 !< Variable ID for reconstructed bedload sediment transport at cell centre after upwinding, secondary flow and bed slope effect (x-component)
   integer :: id_sbcy_reconstructed(MAX_ID_VAR)     = -1 !< Variable ID for reconstructed bedload sediment transport at cell centre after upwinding, secondary flow and bed slope effect (y-component)
   integer :: id_sbwx(MAX_ID_VAR)        = -1 !< Variable ID for wave related bedload sediment transport at cell centre before upwinding and bed slope effect (x-component)
   integer :: id_sbwy(MAX_ID_VAR)        = -1 !< Variable ID for wave related bedload sediment transport at cell centre before upwinding and bed slope effect (y-component) 
   integer :: id_sbwx_reconstructed(MAX_ID_VAR)     = -1 !< Variable ID for wave related bedload sediment transport at cell centre after upwinding and bed slope effect (x-component)
   integer :: id_sbwy_reconstructed(MAX_ID_VAR)     = -1 !< Variable ID for wave related bedload sediment transport at cell centre after upwinding and bed slope effect (y-component) 
   integer :: id_sswx(MAX_ID_VAR)        = -1 !< Variable ID for wave related suspended sediment transport at cell centre before upwinding and bed slope effect (x-component)
   integer :: id_sswy(MAX_ID_VAR)        = -1 !< Variable ID for wave related suspended sediment transport at cell centre before upwinding and bed slope effect (y-component) 
   integer :: id_sswx_reconstructed(MAX_ID_VAR)     = -1 !< Variable ID for wave related suspended sediment transport at cell centre after upwinding and bed slope effect (x-component)
   integer :: id_sswy_reconstructed(MAX_ID_VAR)     = -1 !< Variable ID for wave related suspended sediment transport at cell centre after upwinding and bed slope effect (y-component) 
   integer :: id_sscx(MAX_ID_VAR)        = -1 !< Variable ID for current related suspended sediment transport at cell centre before upwinding and bed slope effect (x-component)
   integer :: id_sscy(MAX_ID_VAR)        = -1 !< Variable ID for current related suspended sediment transport at cell centre before upwinding and bed slope effect (y-component) 
   integer :: id_sscx_reconstructed(MAX_ID_VAR)     = -1 !< Variable ID for current related suspended sediment transport at cell centre after upwinding and bed slope effect (x-component)
   integer :: id_sscy_reconstructed(MAX_ID_VAR)     = -1 !< Variable ID for current related suspended sediment transport at cell centre after upwinding and bed slope effect (y-component) 
   integer :: id_sbxcum(MAX_ID_VAR)      = -1 !< Variable ID's for time-averaged cell centre transports
   integer :: id_sbycum(MAX_ID_VAR)      = -1
   integer :: id_ssxcum(MAX_ID_VAR)      = -1
   integer :: id_ssycum(MAX_ID_VAR)      = -1
   integer :: id_sbn(MAX_ID_VAR)         = -1 !< Variable ID for 
   integer :: id_sbt(MAX_ID_VAR)         = -1 !< Variable ID for 
   integer :: id_sst(MAX_ID_VAR)         = -1 !< Variable ID for 
   integer :: id_ssn(MAX_ID_VAR)         = -1 !< Variable ID for 
   integer :: id_sourse(MAX_ID_VAR)      = -1 !< Variable ID for 
   integer :: id_sinkse(MAX_ID_VAR)      = -1
   integer :: id_scrn(MAX_ID_VAR)        = -1
   integer :: id_zk(MAX_ID_VAR)          = -1 ! TODO: AvD: HK's timedep zk
   integer :: id_bl(MAX_ID_VAR)          = -1 ! TODO: AvD: HK's timedep bl  
! nudging
   integer :: id_nudge_time(MAX_ID_VAR)  = -1 ! nudging time
   integer :: id_nudge_sal(MAX_ID_VAR)   = -1  ! nudging salinity
   integer :: id_nudge_tem(MAX_ID_VAR)   = -1  ! nudging temperature
   integer :: id_nudge_Dsal(MAX_ID_VAR)  = -1 ! difference of nudging salinity with salinity
   integer :: id_nudge_Dtem(MAX_ID_VAR)  = -1 ! difference of nudging temperature with temperature
! particles
   integer :: id_depth_averaged_particle_concentration(MAX_ID_VAR) = -1  ! depth-averaged particle concentration
! for parallel
   !integer :: id_flowelemdomain(MAX_ID_VAR) = -1 ! domain number of flow elem (face)
   !integer :: id_flowelemglobalnr(MAX_ID_VAR) = -1 ! global flow element numbering
   
   integer :: id_zb(MAX_ID_VAR)     = -1 !< Variable ID for bed elevation
   integer :: id_u1s(MAX_ID_VAR)    = -1 !< sediment advection velocities in trsnaport module; DEBUG
   integer :: id_q1s(MAX_ID_VAR)    = -1
   integer :: id_ucxs(MAX_ID_VAR)   = -1 !< cell centre advection vel
   integer :: id_ucys(MAX_ID_VAR)   = -1
   integer :: id_qcxs(MAX_ID_VAR)   = -1 !< cell centre adv discharge
   integer :: id_qcys(MAX_ID_VAR)   = -1
   integer, dimension(:,:), allocatable :: id_xsedflux
   integer, dimension(:,:), allocatable :: id_ysedflux
   !
   integer :: id_spircrv(MAX_ID_VAR)    = -1 !< Variable ID for flow streamline curvature
   integer :: id_spirint(MAX_ID_VAR)    = -1 !< Variable ID for spiral intensity
   !
   integer :: id_ws(MAX_ID_VAR)         = -1 ! fall velocity
   integer :: id_rsedeq(MAX_ID_VAR)     = -1 ! 
   integer :: id_aks(MAX_ID_VAR)        = -1 ! 
   integer :: id_rca(MAX_ID_VAR)        = -1 ! 
   integer :: id_e_dzdn(MAX_ID_VAR)     = -1 ! 
   integer :: id_e_dzdt(MAX_ID_VAR)     = -1 ! 
   integer :: id_umod(MAX_ID_VAR)       = -1 ! 
   integer :: id_zumod(MAX_ID_VAR)      = -1 ! 
   integer :: id_uuu(MAX_ID_VAR)        = -1 ! 
   integer :: id_vvv(MAX_ID_VAR)        = -1 ! 
   integer :: id_ustar(MAX_ID_VAR)      = -1 ! 
   integer :: id_sxtot(MAX_ID_VAR)      = -1 ! 
   integer :: id_sytot(MAX_ID_VAR)      = -1 ! 
   integer :: id_mor_bl(MAX_ID_VAR)     = -1 ! 
   integer :: id_bodsed(MAX_ID_VAR)     = -1 ! 
   integer :: id_dpsed(MAX_ID_VAR)      = -1 ! 
   integer :: id_msed(MAX_ID_VAR)       = -1 ! 
   integer :: id_lyrfrac(MAX_ID_VAR)    = -1 ! 
   integer :: id_thlyr(MAX_ID_VAR)      = -1 ! 
   integer :: id_poros(MAX_ID_VAR)      = -1 ! 
   integer :: id_duneheight(MAX_ID_VAR) = -1 ! 
   integer :: id_dunelength(MAX_ID_VAR) = -1 ! 
   integer :: id_ksr(MAX_ID_VAR)        = -1 ! 
   integer :: id_ksmr(MAX_ID_VAR)       = -1 ! 
   integer :: id_ksd(MAX_ID_VAR)        = -1 ! 
   integer :: id_ks(MAX_ID_VAR)         = -1 ! 
   integer :: id_taurat(MAX_ID_VAR)     = -1 ! 
   integer :: id_dm(MAX_ID_VAR)         = -1 ! 
   integer :: id_dg(MAX_ID_VAR)         = -1 ! 
   integer :: id_dgsd(MAX_ID_VAR)       = -1 ! 
   integer, allocatable, dimension(:,:) :: id_dxx
   integer :: id_frac(MAX_ID_VAR)       = -1
   integer :: id_mudfrac(MAX_ID_VAR)    = -1
   integer :: id_sandfrac(MAX_ID_VAR)   = -1
   integer :: id_fixfac(MAX_ID_VAR)     = -1
   integer :: id_hidexp(MAX_ID_VAR)     = -1
   integer :: id_mfluff(MAX_ID_VAR)     = -1
   integer :: id_sxwav  (MAX_ID_VAR)    = -1
   integer :: id_sywav  (MAX_ID_VAR)    = -1
   integer :: id_sxbwav (MAX_ID_VAR)    = -1
   integer :: id_sybwav (MAX_ID_VAR)    = -1
   integer :: id_z0c(MAX_ID_VAR)        = -1
   integer :: id_z0r(MAX_ID_VAR)        = -1
   integer :: id_dtcell (MAX_ID_VAR)    = -1
   integer :: id_morft                  = -1
   integer :: id_morfac                 = -1
   integer :: id_sedavgtim              = -1
   integer :: id_frac_name              = -1
   integer :: id_sedfrac(MAX_ID_VAR)    = -1
   integer :: id_kmxsed(MAX_ID_VAR)     = -1
   ! for urban, only for 1d now
   integer :: id_timewetground(MAX_ID_VAR) = -1 !< Variable ID for cumulative time when water is above ground level
   integer :: id_freeboard(MAX_ID_VAR)     = -1 !< Variable ID for freeboard
   integer :: id_hs_on_ground(MAX_ID_VAR)  = -1 !< Variable ID for waterdepth when water is above ground level
   integer :: id_vol_on_ground(MAX_ID_VAR) = -1 !< Variable ID for volume when water is above ground level
   integer :: id_qCur1d2d(MAX_ID_VAR) = -1 !< Variable ID for current total 1d2d inflow (discharge)
   integer :: id_vTot1d2d(MAX_ID_VAR) = -1 !< Variable ID for cumulative total 1d2d inflow (volume)
   integer :: id_qCurLat(MAX_ID_VAR)  = -1 !< Variable ID for current total lateral inflow (discharge)
   integer :: id_vTotLat(MAX_ID_VAR)  = -1 !< Variable ID for cumulative total lateral inflow (volume)
   integer :: id_s1Gradient(MAX_ID_VAR) = -1 !< Variable ID for water level gradient
   !
   ! Other
   !
   !integer :: idx_curtime  = 0  !< Index of current time (typically of latest snapshot being written).
end type t_unc_mapids

type(t_crs), target :: crs !< crs read from net file, to be written to flowgeom. TODO: AvD: temp, move this global CRS into ug_meshgeom (now a bit difficult with old and new file format)

interface unc_put_var_map
   module procedure unc_put_var_map_int
   module procedure unc_put_var_map_real
   module procedure unc_put_var_map_dble
   module procedure unc_put_var_map_dble2
   module procedure unc_put_var_map_dble3
end interface unc_put_var_map

interface unc_put_att
   module procedure unc_put_att_int
   module procedure unc_put_att_dble
   module procedure unc_put_att_char
end interface unc_put_att

contains

!> Initializes some global variables needed for writing NetCDF files during a run.
subroutine init_unstruc_netcdf()
use unstruc_version_module

   ug_meta_fm%institution = trim(unstruc_company)
   ug_meta_fm%source      = trim(unstruc_program)
   ug_meta_fm%references  = trim(unstruc_company_url)
   ug_meta_fm%version     = trim(unstruc_version)
   ug_meta_fm%modelname   = ''
   
   unc_cmode              = NF90_CLOBBER ! Default: 0, use NetCDF library default.
   unc_writeopts          = UG_WRITE_NOOPTS ! Default: 0, no special write options.

end subroutine init_unstruc_netcdf

!> Sets the default NetCDF cmode flag values for all D-Flow FM's created files.
!! Recommended use via calling unc_set_ncformat.
subroutine unc_set_cmode(cmode)
   integer, intent(in) :: cmode !< NetCDF creation mode flags value, intended for use in nf90_create calls.
   unc_cmode = cmode
end subroutine unc_set_cmode


!> Sets the default NetCDF format for all D-Flow FM's created files (NetCDF 3 or 4).
subroutine unc_set_ncformat(iformatnumber)
   use netcdf_utils, only: ncu_format_to_cmode
   integer, intent(in) :: iformatnumber !< The NetCDF format version (3 or 4, colloquially speaking)

   call unc_set_cmode(ncu_format_to_cmode(iformatnumber))
end subroutine unc_set_ncformat


!> Defines a NetCDF variable that has no spatial dimension, also setting the most used attributes.
!! Typically only used for variables without a space dimension.
!! For variables with either his-station-range or map-grid-range in the dimensions:
!! @see unc_def_var_map @see unc_def_var_his
function unc_def_var_nonspatial(ncid, id_var, itype, idims, var_name, standard_name, long_name, unit) result(ierr) 
   use dfm_error
   implicit none
   
   integer,          intent(in)     :: ncid          !< NetCDF file unit
   integer,          intent(inout)  :: id_var        !< Returned variable id.
   integer,          intent(in)     :: itype         !< Variable's data type, one of nf90_double, nf90_int, etc. 
   integer,          intent(in)     :: idims(:)      !< Array with the dimension ids across which this new variable should range. For example (/ id_flowelem, id_time /).
   character(len=*), intent(in)     :: var_name      !< Name for this variable in the file.
   character(len=*), intent(in)     :: standard_name !< Standard name for this variable. May be empty, otherwise it should be CF-compliant.
   character(len=*), intent(in)     :: long_name     !< Description text, used in long_name attribute.
   character(len=*), intent(in)     :: unit          !< Unit for this variable, should be UDUNITS-compliant.
   integer                          :: ierr          !< Result status, DFM_NOERR if successful.

   ierr = DFM_NOERR

   ierr = nf90_def_var(ncid, var_name , itype, idims , id_var)
   if (len_trim(standard_name) > 0) then
      ierr = nf90_put_att(ncid, id_var, 'standard_name', standard_name) 
   end if
   if (len_trim(long_name) > 0) then
      ierr = nf90_put_att(ncid, id_var, 'long_name'    , long_name)
   end if
   ierr = nf90_put_att(ncid, id_var, 'units'        , unit)

end function unc_def_var_nonspatial

!> Defines a NetCDF variable inside a map file, taking care of proper attributes and coordinate references.
!! Produces a UGRID-compliant map file.
!! Typical call: unc_def_var(mapids, mapids%id_s1(:), nf90_double, UNC_LOC_S, 's1', 'sea_surface_height', 'water level', 'm')
!! Space-dependent variables will be multiply defined: on mesh1d and mesh2d-based variables (unless specified otherwise via which_meshdim argument).
function unc_def_var_map(ncid,id_tsp, id_var, itype, iloc, var_name, standard_name, long_name, unit, is_timedep, dimids, cell_method, which_meshdim) result(ierr)
use m_save_ugrid_state, only: network1dname, mesh2dname, mesh1dname, contactname 
use m_flowgeom
use m_flowparameters, only: jamapvol1, jamapau, jamaps1, jamaphu, jamapanc
use network_data, only: numk, numl, numl1d
use dfm_error
use m_missing
use string_module, only: strcmpi
implicit none
integer,            intent(in)  :: ncid
type(t_unc_timespace_id), intent(in)  :: id_tsp        !< Map file and other NetCDF ids.
integer,            intent(out) :: id_var(:)     !< Resulting variable ids, one for each submesh (1d/2d/3d/1d2d-contact if applicable)
integer,            intent(in)  :: itype         !< NetCDF data type (e.g. nf90_double).
integer,            intent(in)  :: iloc          !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W, UNC_LOC_WU).
character(len=*),   intent(in)  :: var_name      !< Variable name for in NetCDF variable, will be prefixed with mesh name.
character(len=*),   intent(in)  :: standard_name !< Standard name (CF-compliant) for 'standard_name' attribute in this variable.
character(len=*),   intent(in)  :: long_name     !< Long name for 'long_name' attribute in this variable (use empty string if not wanted).
character(len=*),   intent(in)  :: unit          !< Unit of this variable (CF-compliant) (use empty string for dimensionless quantities).
integer, optional,  intent(in)  :: is_timedep    !< (Optional) Whether or not (1/0) this variable is time-dependent. (Default: 1)
integer, optional,  intent(in)  :: dimids(:)     !< (Optional) Array with dimension ids, replaces default dimension ordering. Default: ( layerdim, spatialdim, timedim ).
                                                 !! This array may contain special dummy values: -1 will be replaced by time dim, -2 by spatial dim, -3 by layer dim. Example: (/ -2, id_seddim, -1 /).
character(len=*), optional, intent(in) :: cell_method   !< cell_method for this variable (one of 'mean', 'point', see CF for details). Default: mean
integer, optional,  intent(in)  :: which_meshdim !< Selects which (horizontal) mesh dimension(s) need to be defined and written (1: 1D, 2: 2D, 4: 1D2D contacts, 7: all) Default: 7: all.

integer                         :: ierr          !< Result status, DFM_NOERR if successful.
! TODO: AvD: inject vectormax dim here AND timedim!!
character(len=10) :: cell_method_   !< cell_method for this variable (one of 'mean', 'point', see CF for details).
character(len=50) :: cell_measures !< cell_measures for this variable (e.g. 'area: mesh2d_ba', see CF for details).
integer :: ndx1d, numl2d

integer, parameter :: maxrank = 5
integer :: idims(maxrank) !< The (max maxrank) dimensions for this variable, pattern: (id_vectormaxdim, id_spacedim, id_timedim). For time-independent scalar data it is filled as: (<empty>, <empty>, id_spacedim)
integer :: idx_timedim    !< Will point to the position in idims where the time dimension should be injected (typically the slowest index).
integer :: idx_spacedim   !< Will point to the position in idims where the spatial dimension (face/node/edge) should be injected.
integer :: idx_layerdim     !< Will point to the position in idims where the layer dimension (3D) should be injected (only if applicable).
integer :: idx_fastdim    !< Will point to the first used position in idims (i.e. the fastest varying dimension).
integer :: is_timedep_
integer :: is_layerdep_
integer :: ndims, i
integer :: which_meshdim_

   ierr = DFM_NOERR

   idims = 0

   if (present(is_timedep)) then
      is_timedep_ = is_timedep
   else
      is_timedep_ = 1
   end if

   if (present(which_meshdim)) then
      which_meshdim_ = which_meshdim
   else
      which_meshdim_ = 1+2+4 ! 1D and 2D and 1d2d contacts (if applicable)
   end if

   if (iloc == UNC_LOC_S3D .or. iloc == UNC_LOC_U3D .or. iloc == UNC_LOC_W .or. iloc == UNC_LOC_WU) then
      is_layerdep_ = 1
   else
      is_layerdep_ = 0
   end if

   ! Set idx_*dim variables.
   idx_timedim = -1 
   idx_spacedim = -1
   idx_layerdim = -1
   if (present(dimids)) then
      ! Special case: caller supplied its own idims array:
      ndims = size(dimids, 1)
      if (ndims > maxrank) then
         ierr = UG_NOTIMPLEMENTED
         goto 888
      end if

      ! idims will be filled backward, starting from last element 
      idx_fastdim = maxrank-ndims+1
      idims(idx_fastdim:maxrank) = dimids(1:ndims)
   
      ! Loop all given dimension ids and detect any macro ids that need to be replaced later with time/space/layer dimension id.
      do i=idx_fastdim,maxrank
         if (idims(i) == -1 .or. idims(i) == id_tsp%id_timedim) then
            idx_timedim = i
         else if (idims(i) == -2) then
            idx_spacedim = i
         else if (idims(i) == -3 .or. idims(i) == id_tsp%id_laydim .or. idims(i) == id_tsp%id_wdim) then
            idx_layerdim = i
         end if
      end do
   else
      ! Use default order of dimensions.
      if (is_layerdep_ > 0) then
         if (is_timedep_ > 0) then
            idx_timedim = maxrank
            idx_spacedim = maxrank-1
            idx_layerdim = maxrank-2
         else
            idx_spacedim = maxrank
            idx_layerdim = maxrank-1
         end if
         idx_fastdim = idx_layerdim
      else
         if (is_timedep_ > 0) then
            idx_timedim = maxrank
            idx_spacedim = maxrank-1
         else
            idx_spacedim = maxrank
         end if
         idx_fastdim = idx_spacedim
      end if
   end if

   ! TODO: AvD: here vector max handling

   ! Set the time dimension
   if (idx_timedim > 0) then
      idims(idx_timedim) = id_tsp%id_timedim
   end if

   if (present(cell_method)) then
      cell_method_ = cell_method
   else
      cell_method_ = 'mean' !< Default cell average.
   end if
   cell_measures = ''

   select case (iloc)
   case(UNC_LOC_CN) ! Corner point location
      ndx1d = ndxi - ndx2d
      ! Internal 1d netnodes. Horizontal position: nodes in 1d mesh.
      if (iand(which_meshdim_, 1) > 0 .and. ndx1d > 0) then ! If there are 1d flownodes, then there are 1d netnodes.
         ierr = UG_NOTIMPLEMENTED ! Not implemented corner location for 1D grids yet
         goto 888
      end if
      ! Internal 2d netnodes. Horizontal position: nodes in 2d mesh.
      if (iand(which_meshdim_, 2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes, then there are 2d netnodes.
         cell_method_ = 'point' ! NOTE: for now don't allow user-defined cell_method for corners, always point.
         idims(idx_spacedim) = id_tsp%meshids2d%dimids(mdim_node)
         ierr = ug_def_var(ncid, id_var(2), idims(idx_fastdim:maxrank), itype, UG_LOC_NODE, &
                           trim(mesh2dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts)
      end if

   case(UNC_LOC_S) ! Pressure point location
      ndx1d = ndxi - ndx2d
      ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
      if (iand(which_meshdim_, 1) > 0 .and. ndx1d > 0) then
         cell_measures = 'area: '//trim(mesh1dname)//'_flowelem_ba' ! relies on unc_write_flowgeom_ugrid_filepointer
         idims(idx_spacedim) = id_tsp%meshids1d%dimids(mdim_node)
         ierr = ug_def_var(ncid, id_var(1), idims(idx_fastdim:maxrank), itype, UG_LOC_NODE, &
                           trim(mesh1dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts)
      end if
      ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
      if (iand(which_meshdim_, 2) > 0 .and. ndx2d > 0) then
         cell_measures = 'area: '//trim(mesh2dname)//'_flowelem_ba' ! relies on unc_write_flowgeom_ugrid_filepointer
         idims(idx_spacedim) = id_tsp%meshids2d%dimids(mdim_face)
         ierr = ug_def_var(ncid, id_var(2), idims(idx_fastdim:maxrank), itype, UG_LOC_FACE, &
                           trim(mesh2dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts)
      end if
      if (jamapanc > 0 .and. jamaps1 > 0 .and. .not. strcmpi(var_name, 'waterdepth')) then
         ierr = unc_put_att_map_char(ncid, id_tsp, id_var, 'ancillary_variables', 'waterdepth')
      end if

   case(UNC_LOC_U, UNC_LOC_L) ! Horizontal velocity point location, or horizontal net link. Note: defvar for netlinks and flowlinks is the same, putvar not.
      ! Internal 1d flowlinks. Horizontal position: edges in 1d mesh.
      if (iand(which_meshdim_, 1) > 0 .and. numl1d > 0) then
         !1d mesh
         if(size(id_tsp%edgetoln,1).gt.0) then
            !cell_measures = 'area: '//trim(mesh1dname)//'_au' ! TODO: AvD: UNST-1100: au is not yet in map file at all.
            idims(idx_spacedim) = id_tsp%meshids1d%dimids(mdim_edge)
            ierr = ug_def_var(ncid, id_var(1), idims(idx_fastdim:maxrank), itype, UG_LOC_EDGE, &
                              trim(mesh1dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts)
         end if
      end if
      if (iand(which_meshdim_, 4) > 0 .and. numl1d > 0) then
         !1d2d contacts
         if(size(id_tsp%contactstoln,1).gt.0) then  
            idims(idx_spacedim) = id_tsp%meshcontacts%dimids(cdim_ncontacts)
            ierr = ug_def_var(ncid, id_var(4), idims(idx_fastdim:maxrank), itype, UG_LOC_CONTACT, &
                              trim(contactname), var_name, standard_name, long_name, unit, ' ', cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts)
         endif
      end if
      numl2d = numl - numl1d
      ! Internal 2d flowlinks. Horizontal position: edges in 2d mesh.
      if (iand(which_meshdim_, 2) > 0 .and. numl2d > 0) then
         !cell_measures = 'area: '//trim(mesh2dname)//'_au' ! TODO: AvD: UNST-1100: au is not yet in map file at all.
         idims(idx_spacedim) = id_tsp%meshids2d%dimids(mdim_edge)
         ierr = ug_def_var(ncid, id_var(2), idims(idx_fastdim:maxrank), itype, UG_LOC_EDGE, &
                           trim(mesh2dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts)
      end if

      if (jamapanc > 0 .and. jamaphu > 0 .and. .not. strcmpi(var_name, 'hu')) then
         ierr = unc_put_att_map_char(ncid, id_tsp, id_var, 'ancillary_variables', 'hu')
      end if

   case(UNC_LOC_S3D) ! Pressure point location in all layers.
      ndx1d = ndxi - ndx2d
      ! Internal 2dv flownodes. Horizontal position: nodes in 1d mesh. Vertical position: layer centers.
      if (iand(which_meshdim_, 1) > 0 .and. ndx1d > 0) then
         if (jamapvol1 > 0) then
            cell_measures = 'volume: '//trim(mesh1dname)//'_vol1'
         end if

         idims(idx_spacedim) = id_tsp%meshids1d%dimids(mdim_node)
         idims(idx_layerdim) = id_tsp%meshids1d%dimids(mdim_layer)
         ierr = ug_def_var(ncid, id_var(1), idims(idx_fastdim:maxrank), itype, UG_LOC_NODE, &
                           trim(mesh1dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts)
      end if
      ! Internal 3d flownodes. Horizontal position: faces in 2d mesh. Vertical position: layer centers.
      if (iand(which_meshdim_, 2) > 0 .and. ndx2d > 0) then
         if (jamapvol1 > 0) then
            cell_measures = 'volume: '//trim(mesh2dname)//'_vol1'
         end if

         idims(idx_spacedim) = id_tsp%meshids2d%dimids(mdim_face)
         idims(idx_layerdim) = id_tsp%meshids2d%dimids(mdim_layer)
         ierr = ug_def_var(ncid, id_var(2), idims(idx_fastdim:maxrank), itype, UG_LOC_FACE, &
                           trim(mesh2dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts)
      end if

      if (jamapanc > 0 .and. jamaps1 > 0 .and. .not. strcmpi(var_name, 'waterdepth')) then
         ierr = unc_put_att_map_char(ncid, id_tsp, id_var, 'ancillary_variables', 'waterdepth')
      end if

   case(UNC_LOC_U3D) ! Horizontal velocity point location in all layers.
      ! Internal 2dv horizontal flowlinks. Horizontal position: edges in 1d mesh. Vertical position: layer centers.
      if (iand(which_meshdim_, 1) > 0 .and. numl1d > 0) then
         if (jamapau > 0) then
            cell_measures = 'area: '//trim(mesh1dname)//'_au'
         end if
         idims(idx_spacedim) = id_tsp%meshids1d%dimids(mdim_edge)
         idims(idx_layerdim) = id_tsp%meshids1d%dimids(mdim_layer)
         ierr = ug_def_var(ncid, id_var(1), idims(idx_fastdim:maxrank), itype, UG_LOC_EDGE, &
                           trim(mesh1dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts)
      end if
      ! TODO: AvD: 1d2d links as mesh contacts in layered 3D are not handled here yet.

      numl2d = numl - numl1d
      ! Internal 3d horizontal flowlinks. Horizontal position: edges in 2d mesh. Vertical position: layer centers.
      if (iand(which_meshdim_, 2) > 0 .and. numl2d > 0) then
         if (jamapau > 0) then
            cell_measures = 'area: '//trim(mesh2dname)//'_au'
         end if
         idims(idx_spacedim) = id_tsp%meshids2d%dimids(mdim_edge)
         idims(idx_layerdim) = id_tsp%meshids2d%dimids(mdim_layer)
         ierr = ug_def_var(ncid, id_var(2), idims(idx_fastdim:maxrank), itype, UG_LOC_EDGE, &
                           trim(mesh2dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts)
      end if

      if (jamapanc > 0 .and. jamaphu > 0 .and. .not. strcmpi(var_name, 'hu')) then
         ierr = unc_put_att_map_char(ncid, id_tsp, id_var, 'ancillary_variables', 'hu')
      end if

   case(UNC_LOC_W) ! Vertical velocity point location on all layer interfaces.
      ndx1d = ndxi - ndx2d
      ! Internal 2dv vertical flowlinks. Horizontal position: nodes in 1d mesh. Vertical position: layer interfaces.
      if (iand(which_meshdim_, 1) > 0 .and. ndx1d > 0) then ! If there are 1d flownodes and layers, then there are 2dv vertical flowlinks.
         cell_measures = 'area: '//trim(mesh1dname)//'_flowelem_ba' ! relies on unc_write_flowgeom_ugrid_filepointer ! TODO: AvD: UNST-1100: or do we need to use a1 here??
         idims(idx_spacedim) = id_tsp%meshids1d%dimids(mdim_node)
         idims(idx_layerdim) = id_tsp%meshids1d%dimids(mdim_interface)
         ierr = ug_def_var(ncid, id_var(1), idims(idx_fastdim:maxrank), itype, UG_LOC_NODE, &
                           trim(mesh1dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss)
      end if
      ! Internal 3d vertical flowlinks. Horizontal position: faces in 2d mesh. Vertical position: layer interfaces.
      if (iand(which_meshdim_, 2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes and layers, then there are 3d vertical flowlinks.
         cell_measures = 'area: '//trim(mesh2dname)//'_flowelem_ba' ! relies on unc_write_flowgeom_ugrid_filepointer ! TODO: AvD: UNST-1100: or do we need to use a1 here??
         idims(idx_spacedim) = id_tsp%meshids2d%dimids(mdim_face)
         idims(idx_layerdim) = id_tsp%meshids2d%dimids(mdim_interface)
         ierr = ug_def_var(ncid, id_var(2), idims(idx_fastdim:maxrank), itype, UG_LOC_FACE, &
                           trim(mesh2dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts)
      end if

   case(UNC_LOC_WU) ! Vertical viscosity point location on all layer interfaces.
      ! Internal 2dv vertical viscosity points. Horizontal position: edges in 1d mesh. Vertical position: layer interfaces.
      if (iand(which_meshdim_, 1) > 0 .and. numl1d > 0) then
         idims(idx_spacedim) = id_tsp%meshids1d%dimids(mdim_edge)
         idims(idx_layerdim) = id_tsp%meshids1d%dimids(mdim_interface)
         ierr = ug_def_var(ncid, id_var(1), idims(idx_fastdim:maxrank), itype, UG_LOC_EDGE, &
                           trim(mesh1dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts)
      end if
      ! TODO: AvD: 1d2d links as mesh contacts in layered 3D are not handled here yet.

      numl2d = numl - numl1d
      ! Internal 3d vertical viscosity points. Horizontal position: edges in 2d mesh. Vertical position: layer interfaces.
      if (iand(which_meshdim_, 2) > 0 .and. numl2d > 0) then
         idims(idx_spacedim) = id_tsp%meshids2d%dimids(mdim_edge)
         idims(idx_layerdim) = id_tsp%meshids2d%dimids(mdim_interface)
         ierr = ug_def_var(ncid, id_var(2), idims(idx_fastdim:maxrank), itype, UG_LOC_EDGE, &
                           trim(mesh2dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts)
      end if

   case default
      ierr = UG_INVALID_DATALOCATION
      goto 888
   end select

   return ! Successful return.

888 continue
    ! Some error occurred
end function unc_def_var_map

function unc_put_att_dble(ncid, id_var, att_name, att_value) result(ierr) 
use dfm_error
implicit none

integer,          intent(in)     :: ncid          !< NetCDF file unit
integer,          intent(in)     :: id_var(:)     !< Returned variable id.
character(len=*), intent(in)     :: att_name      !< Name of the attribute to be set
double precision, intent(in)     :: att_value     !< (Character) attribute value to be set.
integer                          :: ierr          !< Result status, DFM_NOERR if successful.

integer :: i, numvar

   ierr = DFM_NOERR

   numvar = size(id_var)

   do i=1,numvar
      if (id_var(i) /= -1) then
         ierr = nf90_put_att(ncid, id_var(i), att_name, att_value)
      end if
   end do

end function unc_put_att_dble


function unc_put_att_int(ncid, id_var, att_name, att_value) result(ierr) 
use dfm_error
implicit none

integer,          intent(in)     :: ncid          !< NetCDF file unit
integer,          intent(in)     :: id_var(:)     !< Returned variable id.
character(len=*), intent(in)     :: att_name      !< Name of the attribute to be set
integer,          intent(in)     :: att_value     !< (Character) attribute value to be set.
integer                          :: ierr          !< Result status, DFM_NOERR if successful.

integer :: i, numvar

   ierr = DFM_NOERR

   numvar = size(id_var)

   do i=1,numvar
      if (id_var(i) /= -1) then
         ierr = nf90_put_att(ncid, id_var(i), att_name, att_value)
      end if
   end do

end function unc_put_att_int

!> Puts a normal NetCDF attribute into multiple variable ids, if they are valid.
!! Typically only used for variables that are defined on multiple meshes (e.g. 1d,2d,3d)
!! This routine assumes that unc_def_var_map has already left some id_var(:) values on -1,
!! if that mesh/location is not applicable.
function unc_put_att_char(ncid, id_var, att_name, att_value) result(ierr) 
use dfm_error
implicit none

integer,          intent(in)     :: ncid          !< NetCDF file unit
integer,          intent(in)     :: id_var(:)     !< The variable ids for which to put the attribute.
character(len=*), intent(in)     :: att_name      !< Name of the attribute to be set.
character(len=*), intent(in)     :: att_value     !< (Character) attribute value to be set.
integer                          :: ierr          !< Result status, DFM_NOERR if successful.

integer :: i, numvar

   ierr = DFM_NOERR

   numvar = size(id_var)

   do i=1,numvar
      if (id_var(i) /= -1) then
         ierr = nf90_put_att(ncid, id_var(i), att_name, att_value)
      end if
   end do

end function unc_put_att_char

!> Puts a normal NetCDF attribute into multiple variable ids in a map file, if they are valid.
!! Typically only used for variables that are defined on multiple meshes (e.g. 1d,2d,3d)
!! This function is different from unc_put_att_char as follows: it checks whether the att_value
!! is a variable name that was already defined in the file. If so, it will correctly prefix that
!! variable name with the correct mesh names.
!! This routine assumes that unc_def_var_map has already left some id_var(:) values on -1,
!! if that mesh/location is not applicable.
function unc_put_att_map_char(ncid,id_tsp, id_var, att_name, att_value) result(ierr) 
use dfm_error
implicit none
integer, intent(in)                     :: ncid
type(t_unc_timespace_id),         intent(in)  :: id_tsp        !< Map file and other NetCDF ids.
integer,                    intent(in)  :: id_var(:)     !< Returned variable id.
character(len=*),           intent(in)  :: att_name      !< Name of the attribute to be set.
character(len=*),           intent(in)  :: att_value     !< (Character) attribute value to be set.
integer                                 :: ierr          !< Result status, DFM_NOERR if successful.

character(len=255) :: att_value_ug
integer :: i, numvar, valvarid

   ierr = DFM_NOERR

   ! Check whether attribute value is a reference to another variable name, because then it must be prefixed with the mesh1d/2d3d
   ! 1D ! TODO: AvD: change meshids1d/2d/3d into array(3), such that we can loop 1,3
   att_value_ug = att_value
   ierr = ug_inq_varid(ncid, id_tsp%meshids1d, att_value, valvarid)
   if (ierr == ug_noerr) then
      ierr = nf90_inquire_variable(ncid, valvarid, att_value_ug)
   end if

   if (id_var(1) /= -1) then
      ierr = nf90_put_att(ncid, id_var(1), att_name, att_value_ug)
   end if

   ! 2D ! TODO: AvD: change meshids1d/2d/3d into array(3), such that we can loop 1,3
   att_value_ug = att_value
   ierr = ug_inq_varid(ncid, id_tsp%meshids2d, att_value, valvarid)
   if (ierr == ug_noerr) then
      ierr = nf90_inquire_variable(ncid, valvarid, att_value_ug)
   end if

   if (id_var(2) /= -1) then
      ierr = nf90_put_att(ncid, id_var(2), att_name, att_value_ug)
   end if

   ! 1D ! TODO: AvD: change meshids1d/2d/3d into array(3), such that we can loop 1,3
   att_value_ug = att_value
   ierr = ug_inq_varid(ncid, id_tsp%meshids3d, att_value, valvarid)
   if (ierr == ug_noerr) then
      ierr = nf90_inquire_variable(ncid, valvarid, att_value_ug)
   end if

   if (id_var(3) /= -1) then
      ierr = nf90_put_att(ncid, id_var(3), att_name, att_value_ug)
   end if

end function unc_put_att_map_char

! TODO: AvD: support integer/other data types
! TODO: AvD: support in/exclude boundary points/links

!> Writes a map field of a flow variable to a NetCDF map file, taking care of 1D/2D/3D specifics and s/u/w-point specifics.
!! Only writes data for the current time. Assumes that the mapids%id_tsp%idx_curtime contains the new time index where to write to.
!! Produces a UGRID-compliant map file.
!! If there is a 1d and a 2d mesh, then values are written for both meshes in one call to this function.
!! Typical call: unc_put_var(mapids, mapids%id_s1(:), UNC_LOC_S, s1)


function unc_put_var_map_int(ncid, id_tsp, id_var, iloc, integers, default_value) result(ierr)
implicit none
integer                                 :: ierr
integer, intent(in)                                 :: ncid
type(t_unc_timespace_id),         intent(in)  :: id_tsp        !< Map file and other NetCDF ids.
integer,                    intent(in)  :: id_var(:)     !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable)
integer,                    intent(in)  :: iloc          !< Stagger location for this variable (one of UNC_LOC_S, UNC_LOC_U, UNC_LOC_W).
integer, dimension(:),  intent(in)  :: integers
double precision, optional          :: default_value

double precision, dimension(:), allocatable   :: values

   allocate(values(size(integers)))
   values = integers                   ! casting an array of integers to an array of doubles
   if (present(default_value)) then
      ierr = unc_put_var_map_dble(ncid, id_tsp, id_var, iloc, values, default_value)
   else
      ierr = unc_put_var_map_dble(ncid, id_tsp, id_var, iloc, values)
   end if
   deallocate(values)
end function unc_put_var_map_int

function unc_put_var_map_real(ncid, id_tsp, id_var, iloc, reals, default_value) result(ierr)
implicit none
integer                                 :: ierr
integer, intent(in)                     :: ncid
type(t_unc_timespace_id),         intent(in)  :: id_tsp        !< Map file and other NetCDF ids.
integer,                    intent(in)  :: id_var(:)     !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable)
integer,                    intent(in)  :: iloc          !< Stagger location for this variable (one of UNC_LOC_S, UNC_LOC_U, UNC_LOC_W).
real(kind=4), dimension(:), intent(in)  :: reals
double precision, optional              :: default_value

   double precision, dimension(:), allocatable   :: values

   allocate(values(size(reals)))
   values = reals                   ! casting an array of reals to an array of doubles
   if (present(default_value)) then
      ierr = unc_put_var_map_dble(ncid, id_tsp, id_var, iloc, values, default_value)
   else
      ierr = unc_put_var_map_dble(ncid, id_tsp, id_var, iloc, values)
   end if
   deallocate(values)
end function unc_put_var_map_real

function unc_put_var_map_dble(ncid, id_tsp, id_var, iloc, values, default_value) result(ierr)
use m_flowgeom
use network_data, only: numk, numl, numl1d
use m_flow, only: kmx
use dfm_error
use m_alloc
use m_missing
use m_save_ugrid_state
implicit none

integer, intent(in)                     :: ncid
type(t_unc_timespace_id),         intent(in)  :: id_tsp        !< Map file and other NetCDF ids.
integer,                    intent(in)  :: id_var(:)     !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
integer,                    intent(in)  :: iloc          !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
double precision,           intent(in)  :: values(:)     !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
double precision, optional, intent(in)  :: default_value !< Optional default value, used for writing dummy data on closed edges (i.e. netlinks with no flowlink). NOTE: is not a _FillValue!

integer                         :: ierr          !< Result status, DFM_NOERR if successful.

integer :: ndx1d, lnx2d, lnx2db, numl2d, Lf, L, i, n, k, kb, kt, nlayb, nrlay, LL, Lb, Ltx, nlaybL, nrlayLx
!TODO remove save and deallocate?
double precision, allocatable, save :: workL(:)
double precision, allocatable, save :: workS3D(:,:), workU3D(:,:), workW(:,:), workWU(:,:)
! temporary UGRID fix
double precision, allocatable :: mappedValues(:)

   ierr = DFM_NOERR

   select case (iloc)
   case(UNC_LOC_CN) ! Corner point location
      ndx1d = ndxi - ndx2d
      ! Internal 1d netnodes. Horizontal position: nodes in 1d mesh.
      if (id_var(1) > 0 .and. ndx1d > 0) then ! If there are 1d flownodes, then there are 1d netnodes.
         ierr = UG_NOTIMPLEMENTED ! TODO: AvD putting data on 1D corners not implemented yet.
         goto 888
      end if
      ! Internal 2d netnodes. Horizontal position: nodes in 2d mesh.
      if (id_var(2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes, then there are 2d netnodes.
         ierr = nf90_put_var(ncid, id_var(2), values(1:numk), start = (/ 1, id_tsp%idx_curtime /))
      end if

   case(UNC_LOC_S) ! Pressure point location
      ndx1d = ndxi - ndx2d
      ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
      if (id_var(1) > 0 .and. ndx1d > 0) then
         ! temporary UGRID fix
         !if(numMesh1dBeforeMerging>0) then
         !   if(allocated(mappedValues)) deallocate(mappedValues)
         !   allocate(mappedValues(numMesh1dBeforeMerging))
         !   do i =1, numMesh1dBeforeMerging
         !      !mappedValues(i)=values(ndx2d+mesh1dUnmergedToMerged(i))
         !      mappedValues(i)=values(ndx2d)
         !   enddo  
         !   ierr = nf90_put_var(ncid, id_var(1), mappedValues, start = (/ 1, id_tsp%idx_curtime /))
         !else
            ierr = nf90_put_var(ncid, id_var(1), values(ndx2d+1:ndxi), start = (/ 1, id_tsp%idx_curtime /))
         !end if
      end if
      ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
      if (id_var(2) > 0 .and. ndx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), values(1:ndx2d), start = (/ 1, id_tsp%idx_curtime /))
      end if

   case(UNC_LOC_U) ! Horizontal velocity point location
      ! Internal 1d flowlinks. Horizontal position: edges in 1d mesh.
      if (id_var(1) > 0 .and. lnx1d > 0) then
         ! 1d mesh
         if(size(id_tsp%edgetoln,1).gt.0) then
            ierr = nf90_put_var(ncid, id_var(1), values(id_tsp%edgetoln(:)), start = (/ 1, id_tsp%idx_curtime /))
         endif
      end if

      if (id_var(4) > 0 .and. lnx1d > 0) then
         ! 1d2d contacts
         if(size(id_tsp%contactstoln,1).gt.0) then
            ierr = nf90_put_var(ncid, id_var(4), values(id_tsp%contactstoln(:)), start = (/ 1, id_tsp%idx_curtime /))
         endif
      end if
      
      lnx2d = lnxi - lnx1d
      ! Internal 2d flowlinks. Horizontal position: edges in 2d mesh.
      if (id_var(2) > 0 .and. lnx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), values(lnx1d+1:lnxi), start = (/ 1, id_tsp%idx_curtime /))
      end if
      ! External 2d flowlinks. Horizontal position: edges in 2d mesh.
      lnx2db = lnx - lnx1db
      if (id_var(2) > 0 .and. lnx2db > 0) then
         ierr = nf90_put_var(ncid, id_var(2), values(lnx1db+1:lnx), start = (/ lnx2d+1, id_tsp%idx_curtime /))
      end if
      ! Default value is different from a fill value, use for example for zero velocities on closed edges.
      if (present(default_value)) then
         ! Number of netlinks can be > number of flowlinks, if there are closed edges.
         numl2d = numl - numl1d
         ! Write default_value on all closed edges.
         if (id_var(2) > 0 .and. numl2d - lnx2d - lnx2db > 0) then
            ierr = nf90_put_var(ncid, id_var(2), (/ default_value /), start = (/ lnx2d+lnx2db+1, id_tsp%idx_curtime /), count = (/ numl2d - lnx2d - lnx2db, 1 /), map = (/ 0 /)) ! Use map = 0 to write a single value on multiple edges in file.
         end if
      end if

   case(UNC_LOC_L) ! Horizontal net link location
      ! NOTE: In the ugrid geometry, edges have been order based on flow link order. All non-flowlink net links are at the end of the edge array.
      
      call realloc(workL, numl, keepExisting = .false.)

      ! Permute the input values(:) from netlink ordering to flow link ordering.
      ! TODO: AvD: cache this permutation for all future map writes in a flow() run.
      do Lf=1,lnx1d
         L = abs(ln2lne(Lf))
         workL(Lf) = values(L)
      end do

      ! 1D: write all values on 1D flow links. ! TODO: AvD: for 1D I now assume that all net links are also a flow link. This is not always true (thin dams), so make code below equal to 2D code hereafter.
      if (id_var(1) > 0 .and. lnx1d > 0) then ! TODO: AvD: along with previous TODO, this should become numl1d
         ierr = nf90_put_var(ncid, id_var(1), workL(1:lnx1d), start = (/ 1, id_tsp%idx_curtime /))
      end if

      ! 2D: permute all values on net links such that flow links come first, followed by remaining non-flowlink net links.
      lnx2d = lnxi - lnx1d
      lnx2db = lnx - lnx1db
      i = lnx2d+lnx2db ! last position in permuted array of a written non-flowlink net link (none as a start, i.e., last 2d flow link)
      do L=numl1d+1,numl ! Only 2D net links
         Lf = lne2ln(L) ! If negative, then no flow link

         if (Lf > lnx1db) then ! 2D open boundary flow link
            ! Values on netlinks that are also flowlinks come first.
            workL(Lf - lnx1db + lnx2d) = values(L)
         else if (Lf > lnx1d) then ! 2D internal flow link. This intentionally excludes 2D net links that are 1D2D flow links.
            ! Values on netlinks that are also flowlinks come first.
            workL(Lf - lnx1d) = values(L)
         else
            ! Values on netlinks that are no flowlinks come as a last block (in remaining net link order).
            i = i + 1
            workL(i) = values(L)
         end if
      end do
      if (id_var(2) > 0 .and. numl - numl1d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), workL(1:(numl-numl1d)), start = (/ 1, id_tsp%idx_curtime /))
      end if

   case(UNC_LOC_S3D) ! Pressure point location in all layers.
      ! Fill work array.
      call realloc(workS3D, (/ kmx, ndxi /), keepExisting = .false.)
      ! Loop over horizontal flownodes.
      do n = 1,ndxi
         ! Store missing values for inactive layers (i.e. z layers below bottomlevel or above waterlevel for current horizontal flownode n).
         workS3D(:, n) = dmiss
         ! The current horizontal flownode n has active layers nlayb:nlayb+nrlay-1.
         call getlayerindices(n, nlayb, nrlay)
         ! The current horizontal flownode n has indices kb:kt in values array (one value per active layer).
         call getkbotktop(n, kb, kt)
         ! The range kb:kt can have a different length for each flownode due to inactive layers.
         ! Here kb corresponds to nlayb and kt corresponds to nlayb+nrlay-1
         ! Loop over active layers.
         do k = kb,kt
            workS3D(k - kb + nlayb, n) = values(k)
         end do
      end do

      ! Write work array.
      ndx1d = ndxi - ndx2d
      ! Internal 2dv flownodes. Horizontal position: nodes in 1d mesh. Vertical position: layer centers.
      if (id_var(1) > 0 .and. ndx1d > 0) then
         ierr = nf90_put_var(ncid, id_var(1), workS3D(1:kmx, ndx2d+1:ndxi), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx, ndx1d, 1 /))
      end if
      ! Internal 3d flownodes. Horizontal position: faces in 2d mesh. Vertical position: layer centers.
      if (id_var(2) > 0 .and. ndx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), workS3D(1:kmx, 1:ndx2d), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx, ndx2d, 1 /))
      end if

      ! TODO: AvD: include flow link bug fix (Feb 15, 2017) from 1d/2D above also in U3D and WU code below.
   case(UNC_LOC_U3D) ! Horizontal velocity point location in all layers.
      ! Fill work array.
      call realloc(workU3D, (/ kmx, lnx /), keepExisting = .false.)
      ! Loop over horizontal flowlinks.
      do LL = 1,lnx
         ! Store missing values for inactive layers (i.e. z layers below bottomlevel or above waterlevel for current horizontal flowlink LL).
         workU3D(:, LL) = dmiss
         ! The current horizontal flowlink LL has active layers nlaybL:nlaybL+nrlayLx-1.
         call getlayerindicesLmax(LL, nlaybL, nrlayLx)
         ! The current horizontal flowlink LL has indices Lb:Ltx in values array (one value per active layer).
         call getLbotLtopmax(LL, Lb, Ltx)
         ! The range Lb:Ltx can have a different length for each flowlink due to inactive layers.
         ! Here Lb corresponds to nlaybL and Ltx corresponds to nlaybL+nrlayLx-1
         ! Loop over active layers.
         do L = Lb,Ltx
            workU3D(L - Lb + nlaybL, LL) = values(L)
         end do
      end do

      ! Write work array.
      ! Internal 2dv horizontal flowlinks. Horizontal position: edges in 1d mesh. Vertical position: layer centers.
      if (id_var(1) > 0 .and. lnx1d > 0) then
         ierr = nf90_put_var(ncid, id_var(1), workU3D(1:kmx, 1:lnx1d), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx, lnx1d, 1 /))
      end if
      lnx2d = lnx - lnx1d ! TODO: AvD: now also includes 1D bnds, dont want that.
      ! Internal and external 3d horizontal flowlinks (and 2dv external flowlinks). Horizontal position: edges in 2d mesh. Vertical position: layer centers.
      if (id_var(2) > 0 .and. lnx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), workU3D(1:kmx, lnx1d+1:lnx), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx, lnx2d, 1 /))
      end if
      ! Default value is different from a fill value, use for example for zero velocities on closed edges.
      if (id_var(2) > 0 .and. present(default_value)) then
         ! Number of netlinks can be > number of flowlinks, if there are closed edges.
         numl2d = numl - numl1d
         ! Write default_value on all remaining edges in 2d mesh (i.e. closed edges).
         ierr = nf90_put_var(ncid, id_var(2), (/ default_value /), start = (/ 1, lnx2d+1, id_tsp%idx_curtime /), count = (/ kmx, numl2d - lnx2d, 1 /), map = (/ 0 /)) ! Use map = 0 to write a single value on multiple edges in file.
      end if

   case(UNC_LOC_W) ! Vertical velocity point location on all layer interfaces.
      ! Fill work array.
      call realloc(workW, (/ kmx, ndxi /), lindex=(/ 0, 1 /), keepExisting = .false.)
      ! Loop over horizontal flownodes.
      do n = 1,ndxi
         ! Store missing values for inactive layer interfaces (i.e. z layers below bottomlevel or above waterlevel for current horizontal flownode n).
         workW(:, n) = dmiss
         ! The current horizontal flownode n has active layers nlayb:nlayb+nrlay-1.
         call getlayerindices(n, nlayb, nrlay)
         ! The current horizontal flownode n has indices kb:kt in values array (one value per active layer).
         call getkbotktop(n, kb, kt)
         ! The range kb:kt can have a different length for each flownode due to inactive layers.
         ! Here kb corresponds to nlayb and kt corresponds to nlayb+nrlay-1
         ! Loop over active layer interfaces. First active layer interface has index of first active layer - 1.
         do k = kb-1,kt
            workW(k - kb + nlayb, n) = values(k)
         end do
      end do

      ! Write work array.
      ndx1d = ndxi - ndx2d
      ! Internal 2dv vertical flowlinks. Horizontal position: nodes in 1d mesh. Vertical position: layer interfaces.
      if (id_var(1) > 0 .and. ndx1d > 0) then ! If there are 1d flownodes and layers, then there are 2dv vertical flowlinks.
         ierr = nf90_put_var(ncid, id_var(1), workW(0:kmx, ndx2d+1:ndxi), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx+1, ndx1d, 1 /))
      end if
      ! Internal 3d vertical flowlinks. Horizontal position: faces in 2d mesh. Vertical position: layer interfaces.
      if (id_var(2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes and layers, then there are 3d vertical flowlinks.
         ierr = nf90_put_var(ncid, id_var(2), workW(0:kmx, 1:ndx2d), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx+1, ndx2d, 1 /))
      end if

   case(UNC_LOC_WU) ! Vertical viscosity point location on all layer interfaces.
      ! Fill work array.
      call realloc(workWU, (/ kmx, lnx /), lindex=(/ 0, 1 /), keepExisting = .false.)
      ! Loop over horizontal flowlinks.
      do LL = 1,lnx
         ! Store missing values for inactive layer interfaces (i.e. z layers below bottomlevel or above waterlevel for current horizontal flowlink LL).
         workWU(:, LL) = dmiss
         ! The current horizontal flowlink LL has active layers nlaybL:nlaybL+nrlayLx-1.
         call getlayerindicesLmax(LL, nlaybL, nrlayLx)
         ! The current horizontal flowlink LL has indices Lb:Ltx in values array (one value per active layer).
         call getLbotLtopmax(LL, Lb, Ltx)
         ! The range Lb:Ltx can have a different length for each flowlink due to inactive layers.
         ! Here Lb corresponds to nlaybL and Ltx corresponds to nlaybL+nrlayLx-1
         ! Loop over active layer interfaces. First active layer interface has index of first active layer - 1.
         do L = Lb-1,Ltx
            workWU(L - Lb + nlaybL, LL) = values(L)
         end do
      end do

      ! Write work array.
      ! Internal 2dv vertical viscosity points. Horizontal position: edges in 1d mesh. Vertical position: layer interfaces.
      if (id_var(1) > 0 .and. lnx1d > 0) then
         ierr = nf90_put_var(ncid, id_var(1), workWU(0:kmx, 1:lnx1d), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx+1, lnx1d, 1 /))
      end if
      lnx2d = lnx - lnx1d ! TODO: AvD: now also includes 1D bnds, dont want that.
      ! Internal and external 3d vertical viscosity points (and 2dv external viscosity points). Horizontal position: edges in 2d mesh. Vertical position: layer interfaces.
      if (id_var(2) > 0 .and. lnx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), workWU(0:kmx, lnx1d+1:lnx), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx+1, lnx2d, 1 /))
      end if
      ! Default value is different from a fill value, use for example for zero values on closed edges.
      if (id_var(2) > 0 .and. present(default_value)) then
         ! Number of netlinks can be > number of flowlinks, if there are closed edges.
         numl2d = numl - numl1d
         ! Write default_value on all remaining edges in 2d mesh (i.e. closed edges).
         ierr = nf90_put_var(ncid, id_var(2), (/ default_value /), start = (/ 1, lnx2d+1, id_tsp%idx_curtime /), count = (/ kmx+1, numl2d - lnx2d, 1 /), map = (/ 0 /)) ! Use map = 0 to write a single value on multiple edges in file.
      end if

   case default
      ierr = UG_INVALID_DATALOCATION
      goto 888
   end select

   return ! Successful return.

888 continue
    ! Some error occurred
end function unc_put_var_map_dble

!> copy of unc_put_var_map_dble for writing bytes
!! TODO: use templating
function unc_put_var_map_byte(ncid, id_tsp, id_var, iloc, values, default_value) result(ierr)
use m_flowgeom
use network_data, only: numk, numl, numl1d
use m_flow, only: kmx
use dfm_error
use m_alloc
use m_missing
implicit none
integer, intent(in)                     :: ncid
type(t_unc_timespace_id),         intent(in)  :: id_tsp        !< Map file and other NetCDF ids.
integer,                    intent(in)  :: id_var(:)     !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
integer,                    intent(in)  :: iloc          !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
integer(kind=1),            intent(in)  :: values(:)     !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
integer(kind=1), optional,  intent(in)  :: default_value !< Optional default value, used for writing dummy data on closed edges (i.e. netlinks with no flowlink). NOTE: is not a _FillValue!

integer                         :: ierr          !< Result status, DFM_NOERR if successful.

integer :: ndx1d, lnx2d, lnx2db, numl2d, Lf, L, i, n, k, kb, kt, nlayb, nrlay, LL, Lb, Ltx, nlaybL, nrlayLx
!TODO remove save and deallocate?
double precision, allocatable, save :: workL(:)
double precision, allocatable, save :: workS3D(:,:), workU3D(:,:), workW(:,:), workWU(:,:)

   ierr = DFM_NOERR

   select case (iloc)
   case(UNC_LOC_CN) ! Corner point location
      ndx1d = ndxi - ndx2d
      ! Internal 1d netnodes. Horizontal position: nodes in 1d mesh.
      if (id_var(1) > 0 .and. ndx1d > 0) then ! If there are 1d flownodes, then there are 1d netnodes.
         ierr = UG_NOTIMPLEMENTED ! TODO: AvD putting data on 1D corners not implemented yet.
         goto 888
      end if
      ! Internal 2d netnodes. Horizontal position: nodes in 2d mesh.
      if (id_var(2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes, then there are 2d netnodes.
         ierr = nf90_put_var(ncid, id_var(2), values(1:numk), start = (/ 1, id_tsp%idx_curtime /))
      end if

   case(UNC_LOC_S) ! Pressure point location
      ndx1d = ndxi - ndx2d
      ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
      if (id_var(1) > 0 .and. ndx1d > 0) then
         ierr = nf90_put_var(ncid, id_var(1), values(ndx2d+1:ndxi), start = (/ 1, id_tsp%idx_curtime /))
      end if
      ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
      if (id_var(2) > 0 .and. ndx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), values(1:ndx2d), start = (/ 1, id_tsp%idx_curtime /))
      end if

   case(UNC_LOC_U) ! Horizontal velocity point location
      ! Internal 1d flowlinks. Horizontal position: edges in 1d mesh.
      if (id_var(1) > 0 .and. lnx1d > 0) then
         ! 1d mesh
         if(size(id_tsp%edgetoln,1).gt.0) then
            ierr = nf90_put_var(ncid, id_var(1), values(id_tsp%edgetoln(:)), start = (/ 1, id_tsp%idx_curtime /))
         endif
      end if

      if (id_var(1) > 0 .and. lnx1d > 0) then
         ! 1d2d contacts
         if(size(id_tsp%contactstoln,1).gt.0) then
            ierr = nf90_put_var(ncid, id_var(4), values(id_tsp%contactstoln(:)), start = (/ 1, id_tsp%idx_curtime /))
         endif
      end if

      lnx2d = lnxi - lnx1d
      ! Internal 2d flowlinks. Horizontal position: edges in 2d mesh.
      if (id_var(2) > 0 .and. lnx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), values(lnx1d+1:lnxi), start = (/ 1, id_tsp%idx_curtime /))
      end if
      ! External 2d flowlinks. Horizontal position: edges in 2d mesh.
      lnx2db = lnx - lnx1db
      if (id_var(2) > 0 .and. lnx2db > 0) then
         ierr = nf90_put_var(ncid, id_var(2), values(lnx1db+1:lnx), start = (/ lnx2d+1, id_tsp%idx_curtime /))
      end if
      ! Default value is different from a fill value, use for example for zero velocities on closed edges.
      if (present(default_value)) then
         ! Number of netlinks can be > number of flowlinks, if there are closed edges.
         numl2d = numl - numl1d
         ! Write default_value on all closed edges.
         if (id_var(2) > 0 .and. numl2d - lnx2d - lnx2db > 0) then
            ierr = nf90_put_var(ncid, id_var(2), (/ default_value /), start = (/ lnx2d+lnx2db+1, id_tsp%idx_curtime /), count = (/ numl2d - lnx2d - lnx2db, 1 /), map = (/ 0 /)) ! Use map = 0 to write a single value on multiple edges in file.
         end if
      end if

   case(UNC_LOC_L) ! Horizontal net link location
      ! NOTE: In the ugrid geometry, edges have been order based on flow link order. All non-flowlink net links are at the end of the edge array.

      call realloc(workL, numl, keepExisting = .false.)

      ! Permute the input values(:) from netlink ordering to flow link ordering.
      ! TODO: AvD: cache this permutation for all future map writes in a flow() run.
      do Lf=1,lnx1d
         L = abs(ln2lne(Lf))
         workL(Lf) = values(L)
      end do

      ! 1D: write all values on 1D flow links. ! TODO: AvD: for 1D I now assume that all net links are also a flow link. This is not always true (thin dams), so make code below equal to 2D code hereafter.
      if (id_var(1) > 0 .and. lnx1d > 0) then ! TODO: AvD: along with previous TODO, this should become numl1d
         ierr = nf90_put_var(ncid, id_var(1), workL(1:lnx1d), start = (/ 1, id_tsp%idx_curtime /))
      end if

      ! 2D: permute all values on net links such that flow links come first, followed by remaining non-flowlink net links.
      lnx2d = lnxi - lnx1d
      lnx2db = lnx - lnx1db
      i = lnx2d+lnx2db ! last position in permuted array of a written non-flowlink net link (none as a start, i.e., last 2d flow link)
      do L=numl1d+1,numl ! Only 2D net links
         Lf = lne2ln(L) ! If negative, then no flow link

         if (Lf > lnx1db) then ! 2D open boundary flow link
            ! Values on netlinks that are also flowlinks come first.
            workL(Lf - lnx1db + lnx2d) = values(L)
         else if (Lf > lnx1d) then ! 2D internal flow link. This intentionally excludes 2D net links that are 1D2D flow links.
            ! Values on netlinks that are also flowlinks come first.
            workL(Lf - lnx1d) = values(L)
         else
            ! Values on netlinks that are no flowlinks come as a last block (in remaining net link order).
            i = i + 1
            workL(i) = values(L)
         end if
      end do
      if (id_var(2) > 0 .and. numl - numl1d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), workL(1:(numl-numl1d)), start = (/ 1, id_tsp%idx_curtime /))
      end if

   case(UNC_LOC_S3D) ! Pressure point location in all layers.
      ! Fill work array.
      call realloc(workS3D, (/ kmx, ndxi /), keepExisting = .false.)
      ! Loop over horizontal flownodes.
      do n = 1,ndxi
         ! Store missing values for inactive layers (i.e. z layers below bottomlevel or above waterlevel for current horizontal flownode n).
         workS3D(:, n) = dmiss
         ! The current horizontal flownode n has active layers nlayb:nlayb+nrlay-1.
         call getlayerindices(n, nlayb, nrlay)
         ! The current horizontal flownode n has indices kb:kt in values array (one value per active layer).
         call getkbotktop(n, kb, kt)
         ! The range kb:kt can have a different length for each flownode due to inactive layers.
         ! Here kb corresponds to nlayb and kt corresponds to nlayb+nrlay-1
         ! Loop over active layers.
         do k = kb,kt
            workS3D(k - kb + nlayb, n) = values(k)
         end do
      end do

      ! Write work array.
      ndx1d = ndxi - ndx2d
      ! Internal 2dv flownodes. Horizontal position: nodes in 1d mesh. Vertical position: layer centers.
      if (id_var(1) > 0 .and. ndx1d > 0) then
         ierr = nf90_put_var(ncid, id_var(1), workS3D(1:kmx, ndx2d+1:ndxi), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx, ndx1d, 1 /))
      end if
      ! Internal 3d flownodes. Horizontal position: faces in 2d mesh. Vertical position: layer centers.
      if (id_var(2) > 0 .and. ndx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), workS3D(1:kmx, 1:ndx2d), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx, ndx2d, 1 /))
      end if

      ! TODO: AvD: include flow link bug fix (Feb 15, 2017) from 1d/2D above also in U3D and WU code below.
   case(UNC_LOC_U3D) ! Horizontal velocity point location in all layers.
      ! Fill work array.
      call realloc(workU3D, (/ kmx, lnx /), keepExisting = .false.)
      ! Loop over horizontal flowlinks.
      do LL = 1,lnx
         ! Store missing values for inactive layers (i.e. z layers below bottomlevel or above waterlevel for current horizontal flowlink LL).
         workU3D(:, LL) = dmiss
         ! The current horizontal flowlink LL has active layers nlaybL:nlaybL+nrlayLx-1.
         call getlayerindicesLmax(LL, nlaybL, nrlayLx)
         ! The current horizontal flowlink LL has indices Lb:Ltx in values array (one value per active layer).
         call getLbotLtopmax(LL, Lb, Ltx)
         ! The range Lb:Ltx can have a different length for each flowlink due to inactive layers.
         ! Here Lb corresponds to nlaybL and Ltx corresponds to nlaybL+nrlayLx-1
         ! Loop over active layers.
         do L = Lb,Ltx
            workU3D(L - Lb + nlaybL, LL) = values(L)
         end do
      end do

      ! Write work array.
      ! Internal 2dv horizontal flowlinks. Horizontal position: edges in 1d mesh. Vertical position: layer centers.
      if (id_var(1) > 0 .and. lnx1d > 0) then
         ierr = nf90_put_var(ncid, id_var(1), workU3D(1:kmx, 1:lnx1d), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx, lnx1d, 1 /))
      end if
      lnx2d = lnx - lnx1d ! TODO: AvD: now also includes 1D bnds, dont want that.
      ! Internal and external 3d horizontal flowlinks (and 2dv external flowlinks). Horizontal position: edges in 2d mesh. Vertical position: layer centers.
      if (id_var(2) > 0 .and. lnx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), workU3D(1:kmx, lnx1d+1:lnx), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx, lnx2d, 1 /))
      end if
      ! Default value is different from a fill value, use for example for zero velocities on closed edges.
      if (id_var(2) > 0 .and. present(default_value)) then
         ! Number of netlinks can be > number of flowlinks, if there are closed edges.
         numl2d = numl - numl1d
         ! Write default_value on all remaining edges in 2d mesh (i.e. closed edges).
         ierr = nf90_put_var(ncid, id_var(2), (/ default_value /), start = (/ 1, lnx2d+1, id_tsp%idx_curtime /), count = (/ kmx, numl2d - lnx2d, 1 /), map = (/ 0 /)) ! Use map = 0 to write a single value on multiple edges in file.
      end if

   case(UNC_LOC_W) ! Vertical velocity point location on all layer interfaces.
      ! Fill work array.
      call realloc(workW, (/ kmx, ndxi /), lindex=(/ 0, 1 /), keepExisting = .false.)
      ! Loop over horizontal flownodes.
      do n = 1,ndxi
         ! Store missing values for inactive layer interfaces (i.e. z layers below bottomlevel or above waterlevel for current horizontal flownode n).
         workW(:, n) = dmiss
         ! The current horizontal flownode n has active layers nlayb:nlayb+nrlay-1.
         call getlayerindices(n, nlayb, nrlay)
         ! The current horizontal flownode n has indices kb:kt in values array (one value per active layer).
         call getkbotktop(n, kb, kt)
         ! The range kb:kt can have a different length for each flownode due to inactive layers.
         ! Here kb corresponds to nlayb and kt corresponds to nlayb+nrlay-1
         ! Loop over active layer interfaces. First active layer interface has index of first active layer - 1.
         do k = kb-1,kt
            workW(k - kb + nlayb, n) = values(k)
         end do
      end do

      ! Write work array.
      ndx1d = ndxi - ndx2d
      ! Internal 2dv vertical flowlinks. Horizontal position: nodes in 1d mesh. Vertical position: layer interfaces.
      if (id_var(1) > 0 .and. ndx1d > 0) then ! If there are 1d flownodes and layers, then there are 2dv vertical flowlinks.
         ierr = nf90_put_var(ncid, id_var(1), workW(0:kmx, ndx2d+1:ndxi), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx+1, ndx1d, 1 /))
      end if
      ! Internal 3d vertical flowlinks. Horizontal position: faces in 2d mesh. Vertical position: layer interfaces.
      if (id_var(2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes and layers, then there are 3d vertical flowlinks.
         ierr = nf90_put_var(ncid, id_var(2), workW(0:kmx, 1:ndx2d), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx+1, ndx2d, 1 /))
      end if

   case(UNC_LOC_WU) ! Vertical viscosity point location on all layer interfaces.
      ! Fill work array.
      call realloc(workWU, (/ kmx, lnx /), lindex=(/ 0, 1 /), keepExisting = .false.)
      ! Loop over horizontal flowlinks.
      do LL = 1,lnx
         ! Store missing values for inactive layer interfaces (i.e. z layers below bottomlevel or above waterlevel for current horizontal flowlink LL).
         workWU(:, LL) = dmiss
         ! The current horizontal flowlink LL has active layers nlaybL:nlaybL+nrlayLx-1.
         call getlayerindicesLmax(LL, nlaybL, nrlayLx)
         ! The current horizontal flowlink LL has indices Lb:Ltx in values array (one value per active layer).
         call getLbotLtopmax(LL, Lb, Ltx)
         ! The range Lb:Ltx can have a different length for each flowlink due to inactive layers.
         ! Here Lb corresponds to nlaybL and Ltx corresponds to nlaybL+nrlayLx-1
         ! Loop over active layer interfaces. First active layer interface has index of first active layer - 1.
         do L = Lb-1,Ltx
            workWU(L - Lb + nlaybL, LL) = values(L)
         end do
      end do

      ! Write work array.
      ! Internal 2dv vertical viscosity points. Horizontal position: edges in 1d mesh. Vertical position: layer interfaces.
      if (id_var(1) > 0 .and. lnx1d > 0) then
         ierr = nf90_put_var(ncid, id_var(1), workWU(0:kmx, 1:lnx1d), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx+1, lnx1d, 1 /))
      end if
      lnx2d = lnx - lnx1d ! TODO: AvD: now also includes 1D bnds, dont want that.
      ! Internal and external 3d vertical viscosity points (and 2dv external viscosity points). Horizontal position: edges in 2d mesh. Vertical position: layer interfaces.
      if (id_var(2) > 0 .and. lnx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), workWU(0:kmx, lnx1d+1:lnx), start = (/ 1, 1, id_tsp%idx_curtime /), count = (/ kmx+1, lnx2d, 1 /))
      end if
      ! Default value is different from a fill value, use for example for zero values on closed edges.
      if (id_var(2) > 0 .and. present(default_value)) then
         ! Number of netlinks can be > number of flowlinks, if there are closed edges.
         numl2d = numl - numl1d
         ! Write default_value on all remaining edges in 2d mesh (i.e. closed edges).
         ierr = nf90_put_var(ncid, id_var(2), (/ default_value /), start = (/ 1, lnx2d+1, id_tsp%idx_curtime /), count = (/ kmx+1, numl2d - lnx2d, 1 /), map = (/ 0 /)) ! Use map = 0 to write a single value on multiple edges in file.
      end if

   case default
      ierr = UG_INVALID_DATALOCATION
      goto 888
   end select

   return ! Successful return.

888 continue
    ! Some error occurred
end function unc_put_var_map_byte

!> copy of unc_put_var_map_byte with buffered time
!! TODO: only implemented for UNC_LOC_S
function unc_put_var_map_byte_timebuffer(ncid,id_tsp, id_var, iloc, values, t1, tl) result(ierr)
use m_flowgeom
use network_data, only: numk, numl, numl1d
use m_flow, only: kmx
use dfm_error
use m_alloc
use m_missing
implicit none
integer, intent(in)                     :: ncid
type(t_unc_timespace_id),         intent(in)  :: id_tsp        !< Map file and other NetCDF ids.
integer,                    intent(in)  :: id_var(:)     !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
integer,                    intent(in)  :: iloc          !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
integer(kind=1),            intent(in)  :: values(:,:)   !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
integer,                    intent(in)  :: t1            !< first time in buffer to be written
integer,                    intent(in)  :: tl            !< last time in buffer to be written

integer                         :: ierr          !< Result status, DFM_NOERR if successful.

integer                         :: tstart        !< time index of t1
integer                         :: ndx1d         !< number of 1d node links

   ierr = DFM_NOERR

   select case (iloc)

   case(UNC_LOC_S) ! Pressure point location
      ndx1d = ndxi - ndx2d
      tstart = id_tsp%idx_curtime - tl + t1
      ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
      if (id_var(1) > 0 .and. ndx1d > 0) then
         ierr = nf90_put_var(ncid, id_var(1), values(ndx2d+1:ndxi, t1:tl), start = (/ 1, tstart /))
      end if
      ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
      if (id_var(2) > 0 .and. ndx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), values(1:ndx2d, t1: tl), start = (/ 1, tstart /))
      end if

   case default
      ierr = UG_INVALID_DATALOCATION
      goto 888
   end select

   return ! Successful return.

888 continue
    ! Some error occurred
end function unc_put_var_map_byte_timebuffer

function unc_put_var_map_dble2(ncid,id_tsp, id_var, iloc, values, default_value, locdim) result(ierr)
use m_flowgeom
use network_data, only: numk, numl, numl1d
use m_flow, only: kmx
use dfm_error
use m_alloc
use m_missing
implicit none
integer, intent(in)                     :: ncid
type(t_unc_timespace_id),         intent(in)  :: id_tsp        !< Map file and other NetCDF ids.
integer,                    intent(in)  :: id_var(:)     !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
integer,                    intent(in)  :: iloc          !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
double precision,           intent(in)  :: values(:,:)   !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
double precision, optional, intent(in)  :: default_value !< Optional default value, used for writing dummy data on closed edges (i.e. netlinks with no flowlink). NOTE: is not a _FillValue!
integer, optional,          intent(in)  :: locdim        !< Optional index of the location dimension (default = 1)

integer                         :: ierr          !< Result status, DFM_NOERR if successful.

integer :: ndx1d, lnx2d, lnx2db, numl2d, Lf, L, i, n, k, kb, kt, nlayb, nrlay, LL, Lb, Ltx, nlaybL, nrlayLx
integer               :: ilocdim
integer               :: lndim
integer, dimension(3) :: dimids_var
double precision, allocatable :: work(:,:)

   ierr = DFM_NOERR
   if (present(locdim)) then
      ilocdim = locdim
   else
      ilocdim = 1
   endif

   select case (iloc)
   case(UNC_LOC_S) ! Pressure point location
      ndx1d = ndxi - ndx2d
      ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
      if (id_var(1) > 0 .and. ndx1d > 0) then
         select case (ilocdim)
         case(1)
            allocate(work(ndxi-ndx2d,size(values,2)))
            work = values(ndx2d+1:ndxi,:)
            ierr = nf90_put_var(ncid, id_var(1), work, start = (/ 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         case(2)
            allocate(work(size(values,1),ndxi-ndx2d))
            work = values(:,ndx2d+1:ndxi)
            ierr = nf90_put_var(ncid, id_var(1), work, start = (/ 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         end select
      end if
      ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
      if (id_var(2) > 0 .and. ndx2d > 0) then
         select case (ilocdim)
         case(1)
            allocate(work(ndx2d,size(values,2)))
            work = values(1:ndx2d,:)
            ierr = nf90_put_var(ncid, id_var(2), work, start = (/ 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         case(2)
            allocate(work(size(values,1),ndx2d))
            work = values(:,1:ndx2d)
            ierr = nf90_put_var(ncid, id_var(2), work, start = (/ 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         end select
      end if

   case(UNC_LOC_U) ! Horizontal velocity point location
      ! Internal 1d flowlinks. Horizontal position: edges in 1d mesh.
      if (id_var(1) > 0 .and. lnx1d > 0) then
         select case (ilocdim)
         case(1)
            allocate(work(lnx1d,size(values,2)))
            work = values(1:lnx1d,:)
            ierr = nf90_put_var(ncid, id_var(1), work, start = (/ 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         case(2)
            allocate(work(size(values,1),lnx1d))
            work = values(:,1:lnx1d)
            ierr = nf90_put_var(ncid, id_var(1), work, start = (/ 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         end select
      end if
      lnx2d = lnxi - lnx1d
      ! Internal 2d flowlinks. Horizontal position: edges in 2d mesh.
      if (id_var(2) > 0 .and. lnx2d > 0) then
         select case (ilocdim)
         case(1)
            allocate(work(lnxi-lnx1d,size(values,2)))
            work = values(lnx1d+1:lnxi,:)
            ierr = nf90_put_var(ncid, id_var(2), work, start = (/ 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         case(2)
            allocate(work(size(values,1),lnxi-lnx1d))
            work = values(:,lnx1d+1:lnxi)
            ierr = nf90_put_var(ncid, id_var(2), work, start = (/ 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         end select
      end if
      ! External 2d flowlinks. Horizontal position: edges in 2d mesh.
      lnx2db = lnx - lnx1db
      if (id_var(2) > 0 .and. lnx2db > 0) then
         select case (ilocdim)
         case(1)
            allocate(work(lnx-lnx1db,size(values,2)))
            work = values(lnx1db+1:lnx,:)
            ierr = nf90_put_var(ncid, id_var(2), work, start = (/ lnx2d+1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         case(2)
            allocate(work(size(values,1),lnx-lnx1db))
            work = values(:,lnx1db+1:lnx)
            ierr = nf90_put_var(ncid, id_var(2), work, start = (/ 1, lnx2d+1, id_tsp%idx_curtime /))
            deallocate(work)
         end select
      end if
      ! Default value is different from a fill value, use for example for zero velocities on closed edges.
      if (id_var(2) > 0 .and. present(default_value)) then
         ! Number of netlinks can be > number of flowlinks, if there are closed edges.
         numl2d = numl - numl1d
         ! Write default_value on all closed edges.
         if (numl2d - lnx2d - lnx2db > 0) then
            ierr = nf90_inquire_variable(ncid, id_var(2), dimids = dimids_var)
            ! Use map = 0 to write a single value on multiple edges in file.
            select case (ilocdim)
            case(1)
               ierr = nf90_inquire_dimension(ncid, dimids_var(2), len = lndim)
               ierr = nf90_put_var(ncid, id_var(2), (/ default_value /), start = (/ lnx2d+lnx2db+1, 1, id_tsp%idx_curtime /), count = (/ numl2d - lnx2d - lnx2db, lndim, 1 /), map = (/ 0 /))
            case(2)
               ierr = nf90_inquire_dimension(ncid, dimids_var(1), len = lndim)
               ierr = nf90_put_var(ncid, id_var(2), (/ default_value /), start = (/ 1, lnx2d+lnx2db+1, id_tsp%idx_curtime /), count = (/ lndim, numl2d - lnx2d - lnx2db, 1 /), map = (/ 0 /))
            end select
         end if
      end if

   case default
      ierr = UG_INVALID_DATALOCATION
      goto 888
   end select

   return ! Successful return.

888 continue
    ! Some error occurred
end function unc_put_var_map_dble2

function unc_put_var_map_dble3(ncid,id_tsp, id_var, iloc, values, default_value, locdim) result(ierr)
use m_flowgeom
use network_data, only: numk, numl, numl1d
use m_flow, only: kmx
use dfm_error
use m_alloc
use m_missing
implicit none
integer, intent(in)                     :: ncid
type(t_unc_timespace_id),   intent(in)  :: id_tsp        !< Map file and other NetCDF ids.
integer,                    intent(in)  :: id_var(:)     !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
integer,                    intent(in)  :: iloc          !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
double precision,           intent(in)  :: values(:,:,:) !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
double precision, optional, intent(in)  :: default_value !< Optional default value, used for writing dummy data on closed edges (i.e. netlinks with no flowlink). NOTE: is not a _FillValue!
integer, optional,          intent(in)  :: locdim        !< Optional index of the location dimension (default = 1)

integer                         :: ierr          !< Result status, DFM_NOERR if successful.

integer :: ndx1d, lnx2d, lnx2db, numl2d, Lf, L, i, n, k, kb, kt, nlayb, nrlay, LL, Lb, Ltx, nlaybL, nrlayLx
integer               :: ilocdim
integer               :: lndim1, lndim2
integer, dimension(4) :: dimids_var
double precision, allocatable :: work(:,:,:)

   ierr = DFM_NOERR
   if (present(locdim)) then
      ilocdim = locdim
   else
      ilocdim = 1
   endif

   select case (iloc)
   case(UNC_LOC_S) ! Pressure point location
      ndx1d = ndxi - ndx2d
      ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
      if (id_var(1) > 0 .and. ndx1d > 0) then
         select case (ilocdim)
         case(1)
            allocate(work(ndxi-ndx2d,size(values,2),size(values,3)))
            work = values(ndx2d+1:ndxi,:,:)
            ierr = nf90_put_var(ncid, id_var(1), work, start = (/ 1, 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         case(2)
            allocate(work(size(values,1),ndxi-ndx2d,size(values,3)))
            work = values(:,ndx2d+1:ndxi,:)
            ierr = nf90_put_var(ncid, id_var(1), work, start = (/ 1, 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         case(3)
            allocate(work(size(values,1),size(values,2),ndxi-ndx2d))
            work = values(:,:,ndx2d+1:ndxi)
            ierr = nf90_put_var(ncid, id_var(1), work, start = (/ 1, 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         end select
      end if
      ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
      if (id_var(2) > 0 .and. ndx2d > 0) then
         select case (ilocdim)
         case(1)
            allocate(work(ndx2d,size(values,2),size(values,3)))
            work = values(1:ndx2d,:,:)
            ierr = nf90_put_var(ncid, id_var(2), work, start = (/ 1, 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         case(2)
            allocate(work(size(values,1),ndx2d,size(values,3)))
            work = values(:,1:ndx2d,:)
            ierr = nf90_put_var(ncid, id_var(2), work, start = (/ 1, 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         case(3)
            allocate(work(size(values,1),size(values,2),ndx2d))
            work = values(:,:,1:ndx2d)
            ierr = nf90_put_var(ncid, id_var(2), work, start = (/ 1, 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         end select
      end if

   case(UNC_LOC_U) ! Horizontal velocity point location
      ! Internal 1d flowlinks. Horizontal position: edges in 1d mesh.
      if (id_var(1) > 0 .and. lnx1d > 0) then
         select case (ilocdim)
         case(1)
            allocate(work(lnx1d,size(values,2),size(values,3)))
            work = values(1:lnx1d,:,:)
            ierr = nf90_put_var(ncid, id_var(1), work, start = (/ 1, 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         case(2)
            allocate(work(size(values,1),lnx1d,size(values,3)))
            work = values(:,1:lnx1d,:)
            ierr = nf90_put_var(ncid, id_var(1), work, start = (/ 1, 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         case(3)
            allocate(work(size(values,1),size(values,2),lnx1d))
            work = values(:,:,1:lnx1d)
            ierr = nf90_put_var(ncid, id_var(1), work, start = (/ 1, 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         end select
      end if
      lnx2d = lnxi - lnx1d
      ! Internal 2d flowlinks. Horizontal position: edges in 2d mesh.
      if (id_var(2) > 0 .and. lnx2d > 0) then
         select case (ilocdim)
         case(1)
            allocate(work(lnxi-lnx1d,size(values,2),size(values,3)))
            work = values(lnx1d+1:lnxi,:,:)
            ierr = nf90_put_var(ncid, id_var(2), work, start = (/ 1, 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         case(2)
            allocate(work(size(values,1),lnxi-lnx1d,size(values,3)))
            work = values(:,lnx1d+1:lnxi,:)
            ierr = nf90_put_var(ncid, id_var(2), work, start = (/ 1, 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         case(3)
            allocate(work(size(values,1),size(values,2),lnxi-lnx1d))
            work = values(:,:,lnx1d+1:lnxi)
            ierr = nf90_put_var(ncid, id_var(2), work, start = (/ 1, 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         end select
      end if
      ! External 2d flowlinks. Horizontal position: edges in 2d mesh.
      lnx2db = lnx - lnx1db
      if (id_var(2) > 0 .and. lnx2db > 0) then
         select case (ilocdim)
         case(1)
            allocate(work(lnx-lnx1db,size(values,2),size(values,3)))
            work = values(lnx1db+1:lnx,:,:)
            ierr = nf90_put_var(ncid, id_var(2), work, start = (/ lnx2d+1, 1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         case(2)
            allocate(work(size(values,1),lnx-lnx1db,size(values,3)))
            work = values(:,lnx1db+1:lnx,:)
            ierr = nf90_put_var(ncid, id_var(2), work, start = (/ 1, lnx2d+1, 1, id_tsp%idx_curtime /))
            deallocate(work)
         case(3)
            allocate(work(size(values,1),size(values,2),lnx-lnx1db))
            work = values(:,:,lnx1db+1:lnx)
            ierr = nf90_put_var(ncid, id_var(2), work, start = (/ 1, 1, lnx2d+1, id_tsp%idx_curtime /))
            deallocate(work)
         end select
      end if
      ! Default value is different from a fill value, use for example for zero velocities on closed edges.
      if (present(default_value)) then
         ! Number of netlinks can be > number of flowlinks, if there are closed edges.
         numl2d = numl - numl1d
         ! Write default_value on all closed edges.
         if (id_var(2) > 0 .and. numl2d - lnx2d - lnx2db > 0) then
            ierr = nf90_inquire_variable(ncid, id_var(2), dimids = dimids_var)
            ! Use map = 0 to write a single value on multiple edges in file.
            select case (ilocdim)
            case(1)
               ierr = nf90_inquire_dimension(ncid, dimids_var(2), len = lndim1)
               ierr = nf90_inquire_dimension(ncid, dimids_var(3), len = lndim2)
               ierr = nf90_put_var(ncid, id_var(2), (/ default_value /), start = (/ lnx2d+lnx2db+1, 1, 1, id_tsp%idx_curtime /), count = (/ numl2d - lnx2d - lnx2db, lndim1, lndim2, 1 /), map = (/ 0 /))
            case(2)
               ierr = nf90_inquire_dimension(ncid, dimids_var(1), len = lndim1)
               ierr = nf90_inquire_dimension(ncid, dimids_var(3), len = lndim2)
               ierr = nf90_put_var(ncid, id_var(2), (/ default_value /), start = (/ 1, lnx2d+lnx2db+1, 1, id_tsp%idx_curtime /), count = (/ lndim1, numl2d - lnx2d - lnx2db, lndim2, 1 /), map = (/ 0 /))
            case(3)
               ierr = nf90_inquire_dimension(ncid, dimids_var(1), len = lndim1)
               ierr = nf90_inquire_dimension(ncid, dimids_var(2), len = lndim2)
               ierr = nf90_put_var(ncid, id_var(2), (/ default_value /), start = (/ 1, 1, lnx2d+lnx2db+1, id_tsp%idx_curtime /), count = (/ lndim1, lndim2, numl2d - lnx2d - lnx2db, 1 /), map = (/ 0 /))
            end select
         end if
      end if

   case default
      ierr = UG_INVALID_DATALOCATION
      goto 888
   end select

   return ! Successful return.

888 continue
    ! Some error occurred
end function unc_put_var_map_dble3

!> Puts global attributes in NetCDF data set.
!! This includes: institution, Conventions, etc.
subroutine unc_addglobalatts(ncid)
    !use unstruc_model, only : md_ident
    integer, intent(in) :: ncid

    character*8  :: cdate
    character*10 :: ctime
    character*5  :: czone
    integer :: ierr, jaInDefine
    ierr = nf90_noerr
    jaInDefine = 0

    ierr = nf90_redef(ncid)
    if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
    if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
       write (msgbuf, '(a,i0,a,i0,a,a)') 'Could not put global attributes in NetCDF #', ncid, '. Error code ', ierr, ': ', nf90_strerror(ierr)
       call err_flush()
       return
    end if

    ierr = nf90_put_att(ncid, nf90_global,  'institution', trim(unstruc_company))
    ierr = nf90_put_att(ncid, nf90_global,  'references', trim(unstruc_company_url))
    ierr = nf90_put_att(ncid, nf90_global,  'source', &
            unstruc_version_full//                    &
            ', model ')!''//trim(md_ident)//'''')

    call date_and_time(cdate, ctime, czone)
    ierr = nf90_put_att(ncid, nf90_global,  'history', &
        'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
        ', '//trim(unstruc_program))
    ierr = nf90_put_att(ncid, nf90_global,  'date_created',  cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5))
    ierr = nf90_put_att(ncid, nf90_global,  'date_modified', cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5))

    ierr = nf90_put_att(ncid, nf90_global,  'Conventions', 'CF-1.5 Deltares-0.1')

    ! Leave the dataset in the same mode as we got it.
    if (jaInDefine == 0) then
        ierr = nf90_enddef(ncid)
    end if
end subroutine unc_addglobalatts

! TODO: AvD: add these  (incrementally) to map/his files:
!>       :time_coverage_start = "2010-04-23T00:00:00+01:00" ;
!>       :time_coverage_end = "2010-05-20T00:00:00+01:00" ;
! (can be done outside of definition mode, if att was already created before.)

!> Opens a NetCDF file for reading.
!! The file is maintained in the open-file-list.
function unc_open(filename, cmode, ncid)
    character(len=*), intent(in ) :: filename
    integer,          intent(in ) :: cmode
    integer,          intent(out) :: ncid
    integer                       :: unc_open
    unc_open = nf90_open(trim(filename), cmode, ncid)
    if (unc_open == nf90_noerr) then
        nopen_files_ = nopen_files_ + 1
        open_files_(nopen_files_)    = filename
        open_datasets_(nopen_files_) = ncid
        write (msgbuf, '(a,a,a,i10,a)') 'Opened ''', trim(filename), ''' as #', ncid, '.'
        call dbg_flush()
    else
        call mess(LEVEL_WARN, 'could not open '//trim(filename))
        call dbg_flush()
        call qnerror('Failed to open: '//trim(filename), ' ', ' ')
    end if
end function unc_open


!> Creates or opens a NetCDF file for writing.
!! The file is maintained in the open-file-list.
function unc_create(filename, cmode, ncid)
    character(len=*), intent(in ) :: filename
    integer,          intent(in ) :: cmode
    integer,          intent(out) :: ncid
    integer                       :: unc_create

    unc_create = nf90_create(filename, or(cmode, unc_cmode), ncid)
    if (unc_create == nf90_noerr) then
        nopen_files_ = nopen_files_ + 1
        open_files_(nopen_files_)    = filename
        open_datasets_(nopen_files_) = ncid
        write (msgbuf, '(a,a,a,i0,a)') 'Opened NetCDF file ''', trim(filename), ''' as #', ncid, '.'
        call dbg_flush()

        call unc_addglobalatts(ncid)
    else
        write (msgbuf, '(a,a,a,i0,a,i0,a,a)') 'Cannot open NetCDF file ''', trim(filename), ''' as #', ncid, '. Error code: ', unc_create, ': ', nf90_strerror(unc_create)
        call dbg_flush()
    end if

end function unc_create


!> Closes a NetCDF file.
!! The file is removed from the open-file-list
integer function unc_close(ncid)
    integer, intent(inout) :: ncid
    integer                :: i, j
    logical                :: jafound

    unc_close = 0

    jafound = .false.
    ! Search dataset ID
    do i=nopen_files_,1,-1
        if (open_datasets_(i) == ncid) then
            jafound = .true.
            exit
        end if
    end do
    ! If found, shift all entries behind it one to the left.
    if (jafound) then
        unc_close = nf90_close(ncid)
        write (msgbuf, '(a,a,a)') 'Closed NetCDF file ''', trim(open_files_(nopen_files_)), '.'
        call dbg_flush()
        do j=nopen_files_-1,-1,i
            open_files_(j)    = open_files_(j+1)
            open_datasets_(j) = open_datasets_(j+1)
        end do
        open_files_(nopen_files_)    = ' '
        open_datasets_(nopen_files_) = 0
        nopen_files_ = nopen_files_ - 1
        ncid = 0
    else
        write (msgbuf, '(a,i3,a)') 'Tried to close NetCDF id ', ncid, ', not found.'
        call dbg_flush()
    end if
end function unc_close


!> Closes all NetCDF files that are still open.
subroutine unc_closeall()
    integer :: i, istat
    do i = nopen_files_,1,-1
        istat = unc_close(open_datasets_(i))
    end do
end subroutine unc_closeall


!> Adds coordinate attributes according to CF conventions, based on jsferic.
!! Non-standard attributes (such as long_name) should be set elsewhere.
function unc_addcoordatts(ncid, id_varx, id_vary, jsferic)
    integer, intent(in) :: ncid     !< NetCDF dataset id
    integer, intent(in) :: id_varx  !< NetCDF horizontal variable id
    integer, intent(in) :: id_vary  !< NetCDF vertical variable id
    integer, intent(in) :: jsferic  !< Sferical coords or not (1/0)
    integer             :: unc_addcoordatts !< Result status of NetCDF primitives
    
    integer :: ierr

    if (jsferic == 0) then
        ierr = nf90_put_att(ncid, id_varx, 'units',         'm')
        ierr = nf90_put_att(ncid, id_vary, 'units',         'm')
        ierr = nf90_put_att(ncid, id_varx, 'standard_name', 'projection_x_coordinate')
        ierr = nf90_put_att(ncid, id_vary, 'standard_name', 'projection_y_coordinate')
        ierr = nf90_put_att(ncid, id_varx, 'long_name'    , 'x-coordinate')
        ierr = nf90_put_att(ncid, id_vary, 'long_name'    , 'y-coordinate')
    else
        ierr = nf90_put_att(ncid, id_varx, 'units',         'degrees_east')
        ierr = nf90_put_att(ncid, id_vary, 'units',         'degrees_north')
        ierr = nf90_put_att(ncid, id_varx, 'standard_name', 'longitude')
        ierr = nf90_put_att(ncid, id_vary, 'standard_name', 'latitude')
        ierr = nf90_put_att(ncid, id_varx, 'long_name'    , 'longitude')
        ierr = nf90_put_att(ncid, id_vary, 'long_name'    , 'latitude')
    end if
    unc_addcoordatts = ierr
end function unc_addcoordatts


!> Add longitude and latitude coordinates to a NetCDF dataset.
!!
!! Lon/lat coordinates are required by CF-standards, even if the coordinates
!! used are projected Cartesian. Two new coordinate variables are added
!! to the NetCDF id (e.g. a .nc file), but only if jsferic==0.
!! The names for the new variables are based on varbasename and a postfix.
function unc_add_lonlat_vars(ncid, varnameprefix, varnamepostfix, id_dims, id_varlon, id_varlat, jsferic) result(ierr)
    integer,               intent(in)  :: ncid           !< NetCDF dataset id
    character(len=*),      intent(in)  :: varnameprefix  !< Base text string for new lon lat variable names.
    character(len=*),      intent(in)  :: varnamepostfix !< Text string to be appended for new lon lat variable names.
    integer, dimension(:), intent(in)  :: id_dims        !< Array with NetCDF dimension ids for the coordinate variables.
    integer,               intent(out) :: id_varlon      !< NetCDF horizontal variable id
    integer,               intent(out) :: id_varlat      !< NetCDF vertical variable id
    integer,               intent(in)  :: jsferic        !< Spherical coords or not (1/0). If 1, nothing happens. 
    integer                            :: ierr           !< Result status of NetCDF primitives

    ierr = 0

    ! If current system is already spherical, lon/lat should already be present.
    if (jsferic == 1) then
        return
    end if

    ! Define lon and lat variables
    ierr = nf90_def_var(ncid, trim(varnameprefix)//'_lon'//trim(varnamepostfix), nf90_double, id_dims, id_varlon)
    call check_error(ierr, 'Add longitude variable for '//trim(varnameprefix))
    ierr = nf90_def_var(ncid, trim(varnameprefix)//'_lat'//trim(varnamepostfix), nf90_double, id_dims, id_varlat)
    call check_error(ierr, 'Add latitude variable for '//trim(varnameprefix))

    ! Add standard spherical coordinate attributes 
    ierr = unc_addcoordatts(ncid, id_varlon, id_varlat, 1)

    !ierr = unc_addcoordmapping(ncid, 1)
    !call check_error(ierr, 'Add grid_mapping variable for '//trim(varnameprefix)//'_lon'//trim(varnamepostfix)//'/_lat'//trim(varnamepostfix))

    !ierr = unc_add_gridmapping_att(ncid, (/ id_varlon, id_varlat /), 1)
    !call check_error(ierr, 'Add grid_mapping attributes to '//trim(varnameprefix)//'_lon'//trim(varnamepostfix)//'/_lat'//trim(varnamepostfix))

end function unc_add_lonlat_vars


!> Adds coordinate mapping attributes according to CF conventions, based on jsferic.
!! Attributes are put in a scalar integer variable.
function unc_addcoordmapping(ncid, jsferic)
    use m_sferic, only: ra
    integer,          intent(in) :: ncid     !< NetCDF dataset id
    integer,          intent(in) :: jsferic  !< Spherical coords or not (1/0)
    integer                      :: unc_addcoordmapping !< Result status of NetCDF primitives
    
    integer :: ierr, id_crs
    integer :: epsg
    character(len=11) :: epsgstring
    character(len=30) :: varname  !< Name of the created grid mapping variable.
    epsgstring = ' '

    ! AvD: TODO: Name and params are now hardcoded globally based on a single jsferic=0/1 flag.
    ! generalize this!

    if (jsferic == 1) then
       crs%epsg_code = 4326
    end if

    ierr = ug_add_coordmapping(ncid, crs) ! TODO: AvD: temp, this now uses the global crs instead of in meshgeom/jsferic
    if (ierr /= ug_noerr) then
       ierr = ug_get_message(msgbuf)
       if (len_trim(msgbuf) > 0) then
          call warn_flush()
       end if
    end if

    unc_addcoordmapping = ierr
    return
    !! RETURN !!

    varname = ' '
    if (jsferic == 0) then
        varname = 'projected_coordinate_system'
    else
        varname = 'wgs84'
    end if

    ierr = nf90_inq_varid(ncid, trim(varname), id_crs)
    if (ierr == nf90_noerr) then
        ! A variable with that name already exists. Return.
        unc_addcoordmapping = ierr
        return
    end if

    ierr = nf90_def_var(ncid, trim(varname), nf90_int, id_crs)

    if (jsferic == 0) then
        epsg       = 28992
        epsgstring = 'EPSG:28992'
        ierr = nf90_put_att(ncid, id_crs, 'name',                        'Amersfoort / RD New' ) ! CF
        ierr = nf90_put_att(ncid, id_crs, 'epsg',                        epsg                ) ! CF
        ierr = nf90_put_att(ncid, id_crs, 'grid_mapping_name',           'stereographic' ) ! CF
        ierr = nf90_put_att(ncid, id_crs, 'longitude_of_prime_meridian', 0d0                 ) ! CF
        ierr = nf90_put_att(ncid, id_crs, 'semi_major_axis',             ra                  ) ! CF
        ierr = nf90_put_att(ncid, id_crs, 'semi_minor_axis',             6356752.314245d0    ) ! CF
        ierr = nf90_put_att(ncid, id_crs, 'inverse_flattening',          298.257223563d0     ) ! CF
        ierr = nf90_put_att(ncid, id_crs, 'proj4_params',                '+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs'                 ) ! ADAGUC
        ierr = nf90_put_att(ncid, id_crs, 'EPSG_code',                   trim(epsgstring)    ) ! ADAGUC
        ierr = nf90_put_att(ncid, id_crs, 'projection_name',             'Amersfoort / RD New'                 ) ! ADAGUC
        ierr = nf90_put_att(ncid, id_crs, 'crs_wkt',                     ' '                 ) ! WKT
        ierr = nf90_put_att(ncid, id_crs, 'comment',                     ' '                 )
        ierr = nf90_put_att(ncid, id_crs, 'value',                       'value is equal to EPSG code')
    else
        epsg       = 4326
        epsgstring = 'EPSG:4326'
        ierr = nf90_put_att(ncid, id_crs, 'name',                        'WGS84'             ) ! CF
        ierr = nf90_put_att(ncid, id_crs, 'epsg',                        epsg                ) ! CF
        ierr = nf90_put_att(ncid, id_crs, 'grid_mapping_name',           'latitude_longitude') ! CF
        ierr = nf90_put_att(ncid, id_crs, 'longitude_of_prime_meridian', 0d0                 ) ! CF
        ierr = nf90_put_att(ncid, id_crs, 'semi_major_axis',             ra                  ) ! CF
        ierr = nf90_put_att(ncid, id_crs, 'semi_minor_axis',             6356752.314245d0    ) ! CF
        ierr = nf90_put_att(ncid, id_crs, 'inverse_flattening',          298.257223563d0     ) ! CF
        ierr = nf90_put_att(ncid, id_crs, 'proj4_params',                ' '                 ) ! ADAGUC
        ierr = nf90_put_att(ncid, id_crs, 'EPSG_code',                   trim(epsgstring)    ) ! ADAGUC
        ierr = nf90_put_att(ncid, id_crs, 'projection_name',             ' '                 ) ! ADAGUC
        ierr = nf90_put_att(ncid, id_crs, 'crs_wkt',                     ' '                 ) ! WKT
        ierr = nf90_put_att(ncid, id_crs, 'comment',                     ' '                 )
        ierr = nf90_put_att(ncid, id_crs, 'value',                       'value is equal to EPSG code')
    end if
    unc_addcoordmapping = ierr
end function unc_addcoordmapping


!> Add the grid mapping attribute to one or more NetCDF variables.
!!
!! The specified gridmappingname should be an existing variable in the NetCDF dataset.
function unc_add_gridmapping_att(ncid, id_vars, jsferic) result(ierr)
    integer,               intent(in)  :: ncid        !< NetCDF dataset id
    integer, dimension(:), intent(in)  :: id_vars     !< Array of NetCDF variable ids
    integer,               intent(in)  :: jsferic     !< Spherical coords or not (1/0)
    integer                            :: ierr        !< Result status of NetCDF primitives

    integer :: i, n, ierr_
    character(len=30)  :: gridmappingvar              !< Name of grid mapping variable

    gridmappingvar = ' '
    if (jsferic == 0) then
        gridmappingvar = 'projected_coordinate_system' ! TODO: AvD: this works, but we have parts in ug_add_coord_mapping, and parts here. Unify!
    else
        gridmappingvar = 'wgs84'
    end if

    ierr = nf90_noerr
    n    = size(id_vars)

    do i=1,n
        if (id_vars(i) == nf90_global) then
            cycle ! Sometimes id_vars has value 0 (== unintended nf90_global)
        end if

        ierr_ = nf90_put_att(ncid, id_vars(i), 'grid_mapping', trim(gridmappingvar))
        if (ierr_ /= nf90_noerr) then
            ierr = ierr_
        end if
    end do

end function unc_add_gridmapping_att


!> Defines 3d net data structure for an already opened netCDF dataset.
subroutine unc_append_3dflowgeom_def(imapfile)
    use m_flow            !only kmx, zws, layertype
    use m_flowparameters  !only jafullgridoutput 
    
    integer,           intent(in) :: imapfile

    integer, save :: ierr, &
        id_laydim,id_wdim, &
        id_timedim, &
        id_flowelemdim, &
        id_flowelemzcc, &
        id_flowelemzw, & 
        id_laycoordcc, &
        id_laycoordw    

     !define file structure
     ierr = nf90_def_dim(imapfile, 'laydim', kmx, id_laydim)
     ierr = nf90_def_dim(imapfile, 'wdim', kmx+1, id_wdim)
     !
     if (layertype<3) then  !time-independent sigma layer and z layer    
        ierr = nf90_def_var(imapfile, 'LayCoord_cc', nf90_double, (/id_laydim/), id_laycoordcc)
        ierr = nf90_def_var(imapfile, 'LayCoord_w' , nf90_double, (/id_wdim/), id_laycoordw)
        !
        !define and write compact form output of sigma or z-layer
        if (layertype==1) then  !all sigma layers
           ierr = nf90_put_att(imapfile, id_laycoordcc,  'standard_name', 'ocean_sigma_coordinate')  
           ierr = nf90_put_att(imapfile, id_laycoordcc,  'long_name'    , 'sigma layer coordinate at flow element center')
           ierr = nf90_put_att(imapfile, id_laycoordcc,  'units'        , '')  
           ierr = nf90_put_att(imapfile, id_laycoordcc,  'positive'     , 'up')
           ierr = nf90_put_att(imapfile, id_laycoordcc,  'formula_terms', 'sigma: LayCoord_cc eta: s1 bedlevel: FlowElem_bl')
           !
           ierr = nf90_put_att(imapfile, id_laycoordw ,  'standard_name', 'ocean_sigma_coordinate')  
           ierr = nf90_put_att(imapfile, id_laycoordw ,  'long_name'    , 'sigma layer coordinate at vertical interface')
           ierr = nf90_put_att(imapfile, id_laycoordw ,  'units'        , '')        
           ierr = nf90_put_att(imapfile, id_laycoordw ,  'positive'     , 'up')               
           ierr = nf90_put_att(imapfile, id_laycoordw ,  'formula_terms', 'sigma: LayCoord_w eta: s1 bedlevel: FlowElem_bl')
           !
        elseif (layertype==2) then   !all z layers
           ierr = nf90_put_att(imapfile, id_laycoordcc,  'standard_name', '')  
           ierr = nf90_put_att(imapfile, id_laycoordcc,  'long_name'    , 'z layer coordinate at flow element center')
           ierr = nf90_put_att(imapfile, id_laycoordcc,  'positive'     , 'up')
           ierr = nf90_put_att(imapfile, id_laycoordcc,  'units'        , 'm')  
           !
           ierr = nf90_put_att(imapfile, id_laycoordw ,  'standard_name', '')  
           ierr = nf90_put_att(imapfile, id_laycoordw ,  'long_name'    , 'z layer coordinate at vertical interface')
           ierr = nf90_put_att(imapfile, id_laycoordw ,  'positive'     , 'up')
           ierr = nf90_put_att(imapfile, id_laycoordw ,  'units'        , 'm')   
           !
        endif           
     else
        if (jafullgridoutput==0) then
           call mess(LEVEL_WARN, 'No grid outputdata given - Set "FullGridOutput = 1" in .mdu file to output grid data') 
        endif
     endif
     !
     if ( jafullgridoutput.eq.1 ) then
        ! structured 3d time-dependant output data
        ierr = nf90_inq_dimid(imapfile, 'time', id_timedim)
        ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim)
        if (ierr /= nf90_noerr) then
           ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_flowelemdim)
        end if
        !
        ierr = nf90_def_var(imapfile, 'FlowElem_zcc', nf90_double, (/ id_laydim, id_flowelemdim, id_timedim /) , id_flowelemzcc)
        ierr = nf90_def_var(imapfile, 'FlowElem_zw' , nf90_double, (/ id_wdim, id_flowelemdim, id_timedim /) , id_flowelemzw)
        !
        ierr = nf90_put_att(imapfile, id_flowelemzcc,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
        ierr = nf90_put_att(imapfile, id_flowelemzcc,  'standard_name', '')  
        ierr = nf90_put_att(imapfile, id_flowelemzcc,  'long_name'    , 'flow element center z')
        ierr = nf90_put_att(imapfile, id_flowelemzcc,  'units'        , 'm')  
        !
        ierr = nf90_put_att(imapfile, id_flowelemzw ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
        ierr = nf90_put_att(imapfile, id_flowelemzw ,  'standard_name', '')  
        ierr = nf90_put_att(imapfile, id_flowelemzw ,  'long_name'    , 'flow element z at vertical interface')
        ierr = nf90_put_att(imapfile, id_flowelemzw ,  'units'        , 'm')                   
    end if
    !
end subroutine unc_append_3dflowgeom_def

subroutine unc_append_3dflowgeom_put(imapfile, jaseparate, itim_in)
    use m_flow            !only kmx, zws, layertype
    use m_flowgeom        !only Ndxi
    use m_flowparameters  !only jafullgridoutput 
!    use network_data      !
    
    integer,           intent(in) :: imapfile
    integer,           intent(in) :: jaseparate    
    integer,optional,  intent(in) :: itim_in

    integer :: iid, kk, kb, kt, itim

    integer, save :: ierr
    integer, dimension(2), save :: &
        id_laydim,id_wdim, &
        id_timedim, &
        id_flowelemdim, &
        id_flowelemzcc, &
        id_flowelemzw, & 
        id_laycoordcc, &
        id_laycoordw
    
    logical, dimension(2), save  :: firststep  = .true.

    if (present(itim_in)) then
       itim = itim_in
    else
       itim = 1
    end if
    if (jaseparate == 2) then
       ! comfile, store/use ids number 2
       iid = 2
    else
       ! mapfile, store/use ids number 1
       iid = 1
    endif
 
    !
    ! inquire ids of netcdf file if it is the first step of writing to the file
    !                         or if it is a seperate write file   
    !
    if (firststep(iid) .or. jaseparate>0) then    
       firststep(iid) = .false.
       ierr = nf90_inq_dimid(imapfile, 'laydim', id_laydim(iid))   
       ierr = nf90_inq_dimid(imapfile, 'wdim', id_wdim(iid))
       !
       if (layertype<3) then  
          !time-independent sigma layer and z layer    
          ierr = nf90_inq_varid(imapfile, 'LayCoord_cc', id_laycoordcc(iid))
          ierr = nf90_inq_varid(imapfile, 'LayCoord_w' , id_laycoordw(iid))
          !
          ! write 3d time-independent output data to netcdf file
          !
          if (layertype == 1) then  
             ! structured 3d time-independent output data (sigma-layer)
             ierr = nf90_put_var(imapfile, id_laycoordcc(iid), 0.5d0*(zslay(1:kmx,1)+zslay(0:kmx-1,1)), start=(/ 1 /), count=(/ kmx /))
             ierr = nf90_put_var(imapfile, id_laycoordw (iid), zslay(0:kmx,1), start=(/ 1 /), count=(/ kmx+1 /))
          elseif (layertype == 2) then  
             ! structured 3d time-independent output data (z-layer)
           !  ierr = nf90_put_var(imapfile, id_laycoordcc(iid), 0.5d0*(zslay(1:kmx,1)+zslay(0:kmx-1,1)), start=(/ 1 /), count=(/ kmx /))
           !  ierr = nf90_put_var(imapfile, id_laycoordw(iid) , zslay(0:kmx,1), start=(/ 1 /), count=(/ kmx+1 /))          
          endif          
       endif
       !
       if ( jafullgridoutput.eq.1 ) then
          ! get id's for structured 3d time-dependant output data
          ierr = nf90_inq_dimid(imapfile, 'time', id_timedim(iid))
          ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim(iid))
          if (ierr /= nf90_noerr) then
             ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_flowelemdim(iid))
          end if

          !
          ierr = nf90_inq_varid(imapfile, 'FlowElem_zcc', id_flowelemzcc(iid))
          ierr = nf90_inq_varid(imapfile, 'FlowElem_zw' , id_flowelemzw(iid))
          !
       end if
    endif
    !
    if ( jafullgridoutput.eq.1 ) then
       ! write structured 3d time-dependant output data
       do kk=1,Ndxi
          call getkbotktop(kk,kb,kt)                   
          ierr = nf90_put_var(imapfile, id_flowelemzcc(iid), 0.5d0*(zws(kb:kt)+zws(kb-1:kt-1)),  start=(/ 1, kk, itim /), count=(/ kt-kb+1, 1, 1 /))
          ierr = nf90_put_var(imapfile, id_flowelemzw(iid), zws(kb-1:kt),  start=(/ 1, kk, itim /), count=(/ kt-kb+2, 1, 1 /))      
       end do
    end if    
    !
end subroutine unc_append_3dflowgeom_put




!> Writes the unstructured flow net + flow data to a netCDF file.
!! If file exists, it will be overwritten. Therefore, only use this routine
!! for separate snapshots, the automated rst file should be filled by calling
!! unc_write_rst_filepointer directly.
subroutine unc_write_rst(filename)
    character(len=*), intent(in) :: filename

    integer :: irstfile, ierr

    ierr = unc_create(filename, 0, irstfile)
    if (ierr /= nf90_noerr) then
        call mess(LEVEL_ERROR, 'Could not create rst file '''//trim(filename)//'''.')
        call check_error(ierr)
        return
    end if

    call unc_write_rst_filepointer(irstfile, 0d0)

    ierr = unc_close(irstfile)
end subroutine unc_write_rst



!> Writes rst/flow data to a newly opened netCDF dataset.
!! The netnode and -links have been written already.
subroutine unc_write_rst_filepointer(irstfile, tim)
    use m_flow
    use m_flowtimes
    use m_flowgeom
    use m_sferic
    use network_data
    use m_sediment
    use m_transport, only: NUMCONST, ISALT, ITEMP, ISED1, ISEDN, ITRA1, ITRAN, ITRAN0, constituents, itrac2const, const_names, const_units
    use m_fm_wq_processes, only: numwqbots, wqbotnames, wqbotunits, wqbot
    use m_xbeach_data, only: E, thetamean, sigmwav
    use m_flowexternalforcings, only: numtracers  !, trbndnames
    use m_partitioninfo
    use m_missing
    use m_turbulence
    use m_alloc
    use m_CrossSections 
    use unstruc_channel_flow, only: network
    use m_flowparameters, only: jamd1dfile
    use m_oned_functions, only: gridpoint2cross
    use m_save_ugrid_state, only: mesh1dname
    use m_structures, only: valculvert
   
    integer,           intent(in) :: irstfile
    real(kind=hp),     intent(in) :: tim

    integer, save :: ierr
    integer, save ::  &
        !id_netcelldim, id_netcellmaxnodedim, id_netcellcontourptsdim, &
        id_laydim, id_wdim,   &
        id_flowelemdim,       &
        id_maxfracdim,        &
        id_erolaydim,         &
        id_flowlinkdim,       &
        id_timedim,           &
        id_bndsaldim,         &
        id_bndtemdim,         &
        id_bndseddim,         &
        id_bnddim,            &
        id_culvertdim,        &
        id_time, id_timestep, &
        id_s1, id_taus, id_ucx, id_ucy, id_ucz, id_unorm, id_q1, id_ww1, id_sa1, id_tem1, id_sed, id_ero, id_s0, id_u0, &
        id_q1main, &
        id_cfcl, id_cftrt, id_czs, id_E, id_thetamean, &
        id_sigmwav,  &
        id_tsalbnd, id_zsalbnd, id_ttembnd, id_ztembnd, id_tsedbnd, id_zsedbnd, &
        id_morbl, id_bodsed, id_msed, id_thlyr, id_lyrfrac, id_sedtotdim, id_sedsusdim, id_nlyrdim, &
        id_netelemmaxnodedim, id_netnodedim, id_flowlinkptsdim, id_netelemdim, id_netlinkdim, id_netlinkptsdim, &  
        id_flowelemdomain, id_flowelemglobalnr, id_flowlink, id_netelemnode, id_netlink,&
        id_flowelemxzw, id_flowelemyzw, id_flowlinkxu, id_flowlinkyu,&
        id_flowelemxbnd, id_flowelemybnd, id_bl, id_s0bnd, id_s1bnd, id_blbnd, &
        id_unorma, id_vicwwu, id_tureps1, id_turkin1, id_qw, id_qa, id_hu, id_squ, id_sqi, &
        id_jmax, id_flowelemcrsz, id_flowelemcrsn, id_ncrs, id_morft, &
        id_culvert_openh
    
    integer, allocatable, save :: id_tr1(:), id_rwqb(:), id_bndtradim(:), id_ttrabnd(:), id_ztrabnd(:)
    integer, allocatable, save :: id_sf1(:), id_bndsedfracdim(:), id_tsedfracbnd(:), id_zsedfracbnd(:)

    integer :: i, numContPts, numNodes, itim, k, kb, kt, kk, LL, Lb, Lt, iconst, L, j, nv, nv1, nm, ndxbnd, nlayb, nrlay, LTX, nlaybL, nrlaylx
    double precision              :: vicc, dicc, dens
    double precision, allocatable :: max_threttim(:)
    double precision, dimension(:), allocatable       :: dum
    double precision, dimension(:,:,:), allocatable   :: frac
    integer, allocatable, dimension(:,:) :: netcellnod
    integer, allocatable, dimension(:)   :: kn1write, kn2write
    double precision, allocatable, dimension(:)   :: tmp_x, tmp_y, tmp_s0, tmp_s1, tmp_bl, tmp_sa1, tmp_tem1

    character(len=8) :: numformat
    character(len=2) :: numtrastr, numsedfracstr
    character(len=255) :: tmpstr
    
    type(t_CSType), pointer                       :: pCS
    type(t_CSType), pointer, dimension(:)         :: pCSs
    integer                                       :: n, jmax, ndx1d, nCrs
    double precision, dimension(:,:), allocatable :: work1d_z
    

    ! Grid and flow geometry
    ! hk: not now  call unc_write_net_filepointer(irstfile)      ! Write standard net data as well
    ! please fix   call unc_write_flowgeom_filepointer(irstfile) ! Write time-independent flow geometry data
    ! ierr = nf90_inq_dimid(irstfile, 'nFlowElem', id_flowelemdim)
    ! ierr = nf90_inq_dimid(irstfile, 'nFlowLink', id_flowlinkdim)

    numformat = '(I2.2)'
    if (lnx > 0) then
       ierr = nf90_def_dim(irstfile, 'nFlowLink', lnx, id_flowlinkdim)
       ierr = nf90_def_dim(irstfile, 'nFlowLinkPts',2, id_flowlinkptsdim)
    endif

    if (ndx > 0) then
       ierr = nf90_def_dim(irstfile, 'nFlowElem', ndxi, id_flowelemdim)
       if (jarstbnd > 0) then  ! If write boundary points
          if (jampi == 0) then
             ndxbnd = ndx - ndxi
          else
             ndxbnd = ndxbnd_own! In parallel run, do not write ghost boundary points
          endif
          if (ndxbnd > 0) then ! if boundary points exist    
             call realloc(tmp_x,  ndxbnd, stat=ierr, keepExisting=.false.)
             call realloc(tmp_y,  ndxbnd, stat=ierr, keepExisting=.false.)
             call realloc(tmp_s0, ndxbnd, stat=ierr, keepExisting=.false.)
             call realloc(tmp_s1, ndxbnd, stat=ierr, keepExisting=.false.)
             call realloc(tmp_bl, ndxbnd, stat=ierr, keepExisting=.false.)
             if (kmx == 0) then
                call realloc(tmp_sa1, ndxbnd, stat=ierr, keepExisting=.false.)
                call realloc(tmp_tem1, ndxbnd, stat=ierr, keepExisting=.false.)
             endif
             ierr = nf90_def_dim(irstfile, 'nFlowElemBnd', ndxbnd, id_bnddim)
          endif     
       endif
    endif
    
    ierr = nf90_def_dim(irstfile, 'nNetElem', nump1d2d, id_netelemdim)
    ierr = nf90_def_dim(irstfile, 'nNetNode', numk,     id_netnodedim)
    nv = 0
    do k=1,nump1d2d
        nv = max(nv, netcell(k)%n)
    end do
    ierr = nf90_def_dim(irstfile, 'nNetElemMaxNode', nv,     id_netelemmaxnodedim)
    ierr = nf90_def_dim(irstfile, 'nNetLink',        numl,   id_netlinkdim)
    ierr = nf90_def_dim(irstfile, 'nNetLinkPts',     2,      id_netlinkptsdim)
    ! Definition and attributes of time
    ierr = nf90_def_dim(irstfile, 'time', nf90_unlimited, id_timedim)
    call check_error(ierr, 'def time dim')
    ierr = nf90_def_var(irstfile, 'time', nf90_double, id_timedim,  id_time)
    ierr = nf90_put_att(irstfile, id_time,  'units'        , 'seconds since '//refdat(1:4)//'-'//refdat(5:6)//'-'//refdat(7:8)//' 00:00:00')
    ierr = nf90_put_att(irstfile, id_time,  'standard_name', 'time')

    ! Definition and attributes of 3D geometry    
    if (kmx > 0) then
        ! call unc_append_3dflowgeom_def(irstfile)              ! Append definition of time-independent 3d flow geometry data
        ! ierr = nf90_inq_dimid(irstfile, 'laydim', id_laydim)
        ! ierr = nf90_inq_dimid(irstfile, 'wdim', id_wdim)

        ierr = nf90_def_dim(irstfile, 'laydim', kmx, id_laydim)
        ierr = nf90_def_dim(irstfile, 'wdim', kmx+1, id_wdim)
    end if
                        
    ! Thatcher-Harleman boundary data dimensions
    ! TODO: AvD, GvO: NOTE! I renamed al TH-stuf below consistent with the original meaning of numtracers.
    !       BUT, we should double-check: should we not maintain TH lags for *all* open boundaries, for *all* constituents?
    if(allocated(threttim)) then
      allocate(max_threttim(NUMCONST))
      max_threttim = maxval(threttim,dim=2)
      if(jasal > 0) then
         if(max_threttim(ISALT) > 0d0) then
            ierr = nf90_def_dim(irstfile, 'salbndpt', nbnds, id_bndsaldim)
         endif
      endif
      if(jatem > 0) then
         if(max_threttim(ITEMP) > 0d0) then
            ierr = nf90_def_dim(irstfile, 'tembndpt', nbndtm, id_bndtemdim)
         endif
      endif
      if(jased > 0 .and. .not. stm_included) then
         if(max_threttim(ISED1) > 0d0) then
            ierr = nf90_def_dim(irstfile, 'sedbndpt', nbndsd, id_bndseddim)
         endif
      endif
      if (numfracs > 0) then
         if(.not. allocated(id_bndsedfracdim)) then
            allocate(id_bndsedfracdim(numfracs))
         endif
         do i=1,numfracs
            if(max_threttim(i+ISED1-1) > 0d0) then
               write(numsedfracstr,numformat) i
               ierr = nf90_def_dim(irstfile, 'sfbndpt'//trim(numsedfracstr), nbndsf(i), id_bndsedfracdim(i))
            endif
         enddo
      end if
      if(numtracers > 0) then
         if(.not. allocated(id_bndtradim)) then
            allocate(id_bndtradim(numtracers))
         endif
         do i=1,numtracers
            iconst = itrac2const(i)
            if(max_threttim(iconst) > 0d0) then
               write(numtrastr,numformat) i
               ierr = nf90_def_dim(irstfile, 'trbndpt'//trim(numtrastr), nbndtr(i), id_bndtradim(i))
            endif
         enddo
      endif
    endif
    
    ! Definition and attributes of size of latest timestep
    ierr = nf90_def_var(irstfile, 'timestep', nf90_double, id_timedim,  id_timestep)
    ierr = nf90_put_att(irstfile, id_timestep,  'units'        , 'seconds')
    ierr = nf90_put_att(irstfile, id_timestep,  'standard_name', 'timestep')

    ! Definition and attributes of flow data on centres: water level at latest timestep
    ierr = nf90_def_var(irstfile, 's1',  nf90_double, (/ id_flowelemdim, id_timedim /) , id_s1)
    ierr = nf90_put_att(irstfile, id_s1,   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
    ierr = nf90_put_att(irstfile, id_s1,   'standard_name', 'sea_surface_height') ! sorry for inland water people
    ierr = nf90_put_att(irstfile, id_s1,   'long_name'    , 'water level')
    ierr = nf90_put_att(irstfile, id_s1,   'units'        , 'm')
    
    ! Definition and attributes of flow data on centres: water level timestep before the latest timestep
    ierr = nf90_def_var(irstfile, 's0',  nf90_double, (/ id_flowelemdim, id_timedim /) , id_s0)
    ierr = nf90_put_att(irstfile, id_s0,   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
    ierr = nf90_put_att(irstfile, id_s0,   'standard_name', 'sea_surface_height') ! sorry for inland water people
    ierr = nf90_put_att(irstfile, id_s0,   'long_name'    , 'water level at previous timestep')
    ierr = nf90_put_att(irstfile, id_s0,   'units'        , 'm')
    
    ! Definition and attributes of flow data on centres: shear stress
    ierr = nf90_def_var(irstfile, 'taus' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_taus)
    ierr = nf90_put_att(irstfile, id_taus,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
    ierr = nf90_put_att(irstfile, id_taus,  'standard_name', 'taucurrent')
    ierr = nf90_put_att(irstfile, id_taus,  'long_name'    , 'taucurrent in flow element center')
    ierr = nf90_put_att(irstfile, id_taus,  'units'        , 'N m-2')
     
    ! Definition and attributes of flow data on centres: chezy roughness
    ierr = nf90_def_var(irstfile, 'czs' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_czs)
    ierr = nf90_put_att(irstfile, id_czs,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
    ierr = nf90_put_att(irstfile, id_czs,  'long_name'    , 'Chezy roughness in flow element center')
    ierr = nf90_put_att(irstfile, id_czs,  'units'        , 'm0.5s-1')
    
    ! Definition and attributes of flow data on centres: bed level
    ierr = nf90_def_var(irstfile, 'FlowElem_bl',  nf90_double, (/ id_flowelemdim, id_timedim /) , id_bl)
    ierr = nf90_put_att(irstfile, id_bl,   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
    ierr = nf90_put_att(irstfile, id_bl,   'long_name'    , 'bed level at flow element circumcenter')
    ierr = nf90_put_att(irstfile, id_bl,   'units'        , 'm')

    ! Definition of 3D data
    if (kmx > 0) then    
        
       ! Definition and attributes of flow data on edges: velocity magnitude at latest timestep
       ierr = nf90_def_var(irstfile, 'unorm' , nf90_double, (/ id_laydim, id_flowlinkdim, id_timedim /) , id_unorm)
       ierr = nf90_put_att(irstfile, id_unorm,'long_name', 'normal component of sea_water_speed')
       ierr = nf90_put_att(irstfile, id_unorm,'units'        , 'm s-1')
       ierr = nf90_put_att(irstfile, id_unorm,'coordinates'  , 'FlowLink_xu FlowLink_yu')
    
       ! Definition and attributes of flow data on edges: velocity magnitude at previous timestep
       ierr = nf90_def_var(irstfile, 'u0'    , nf90_double, (/ id_laydim, id_flowlinkdim, id_timedim /) , id_u0)
       ierr = nf90_put_att(irstfile, id_u0   ,'long_name',     'normal component of sea_water_speed at previous time t0')
       ierr = nf90_put_att(irstfile, id_u0   ,'units'        , 'm s-1')
       ierr = nf90_put_att(irstfile, id_u0   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')
        
       ! Definition and attributes of flow data on edges: discharge
       ierr = nf90_def_var(irstfile, 'q1'    , nf90_double, (/ id_laydim, id_flowlinkdim, id_timedim /) , id_q1)
       ierr = nf90_put_att(irstfile, id_q1   ,'standard_name', 'discharge')
       ierr = nf90_put_att(irstfile, id_q1   ,'long_name',     'discharge through flow link at current time')
       ierr = nf90_put_att(irstfile, id_q1   ,'units'        , 'm3 s-1')
       ierr = nf90_put_att(irstfile, id_q1   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')
        
       ! Definition and attributes of flow data on centres: x-component of the velocity
       ierr = nf90_def_var(irstfile, 'ucx', nf90_double, (/ id_laydim, id_flowelemdim, id_timedim /) , id_ucx)
       ierr = nf90_put_att(irstfile, id_ucx,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
       if (jsferic == 0) then
          ierr = nf90_put_att(irstfile, id_ucx, 'standard_name', 'sea_water_x_velocity')
       else
          ierr = nf90_put_att(irstfile, id_ucx, 'standard_name', 'eastward_sea_water_velocity')
       end if
       ierr = nf90_put_att(irstfile, id_ucx,  'long_name'    , 'flow element center velocity vector, x-component')
       ierr = nf90_put_att(irstfile, id_ucx,  'units'        , 'm s-1')

       ! Definition and attributes of flow data on centres: y-component of the velocity
       ierr = nf90_def_var(irstfile, 'ucy', nf90_double, (/ id_laydim, id_flowelemdim, id_timedim /) , id_ucy)
       ierr = nf90_put_att(irstfile, id_ucy,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
       if (jsferic == 0) then
          ierr = nf90_put_att(irstfile, id_ucy, 'standard_name', 'sea_water_y_velocity')
       else
          ierr = nf90_put_att(irstfile, id_ucy, 'standard_name', 'northward_sea_water_velocity')
       end if
       ierr = nf90_put_att(irstfile, id_ucy,  'long_name'    , 'flow element center velocity vector, y-component')
       ierr = nf90_put_att(irstfile, id_ucy,  'units'        , 'm s-1')
        
       ! Definition and attributes of flow data on centres: z-component of the velocity
       ierr = nf90_def_var(irstfile, 'ucz', nf90_double, (/ id_laydim, id_flowelemdim, id_timedim /) , id_ucz)
       ierr = nf90_put_att(irstfile, id_ucz,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
       ierr = nf90_put_att(irstfile, id_ucz,  'standard_name', 'upward_sea_water_velocity')
       ierr = nf90_put_att(irstfile, id_ucz,  'long_name'    , 'upward velocity on flow element center')
       ierr = nf90_put_att(irstfile, id_ucz,  'units'        , 'm s-1')
       
       ! Definition and attributes of flow data on centres: z-component of the velocity on vertical interface
       ierr = nf90_def_var(irstfile, 'ww1', nf90_double, (/ id_wdim, id_flowelemdim, id_timedim /) , id_ww1)
       ierr = nf90_put_att(irstfile, id_ww1,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
       ierr = nf90_put_att(irstfile, id_ww1,  'standard_name', 'upward_sea_water_velocity')              ! same standard name allowed?
       ierr = nf90_put_att(irstfile, id_ww1,  'long_name'    , 'upward velocity on vertical interface')  ! (upward normal or upward)?
       ierr = nf90_put_att(irstfile, id_ww1,  'units'        , 'm s-1')
        
       ! Definition and attributes of depth averaged velocity u1(1:lnx)
       ierr = nf90_def_var(irstfile, 'unorm_averaged', nf90_double, (/ id_flowlinkdim, id_timedim /) , id_unorma)
       ierr = nf90_put_att(irstfile, id_unorma   ,'long_name',     'depth averaged normal component of sea_water_speed')
       ierr = nf90_put_att(irstfile, id_unorma   ,'units'        , 'm3 s-1')
       ierr = nf90_put_att(irstfile, id_unorma   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')
       
       ! Definition and attributes of vertical flux through interface qw
       ierr = nf90_def_var(irstfile, 'qw', nf90_double, (/ id_wdim, id_flowelemdim, id_timedim /) , id_qw)
       ierr = nf90_put_att(irstfile, id_qw,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
       ierr = nf90_put_att(irstfile, id_qw,  'long_name'    , 'vertical flux through interface')
       ierr = nf90_put_att(irstfile, id_qw,  'units'        , 'm3 s-1')
       
       ! Definition and attributes of discharge used in advection qa
       ierr = nf90_def_var(irstfile, 'qa', nf90_double,   (/ id_laydim, id_flowlinkdim, id_timedim /)  , id_qa)
       ierr = nf90_put_att(irstfile, id_qa,  'coordinates'  , 'FlowLink_xu FlowLink_yu')
       ierr = nf90_put_att(irstfile, id_qa,  'long_name'    , 'discharge used in advection')
       ierr = nf90_put_att(irstfile, id_qa,  'units'        , 'm3 s-1')
       
       ! Definition and attributes of cell center incoming flux
       ierr = nf90_def_var(irstfile, 'sqi', nf90_double,   (/ id_wdim, id_flowelemdim, id_timedim /)  , id_sqi)
       ierr = nf90_put_att(irstfile, id_sqi,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
       ierr = nf90_put_att(irstfile, id_sqi,  'long_name'    , 'cell center incoming flux')
       ierr = nf90_put_att(irstfile, id_sqi,  'units'        , 'm3 s-1')
       
       ! Definition and attributes of cell center outcoming flux
       ierr = nf90_def_var(irstfile, 'squ', nf90_double,   (/ id_wdim, id_flowelemdim, id_timedim /)  , id_squ)
       ierr = nf90_put_att(irstfile, id_squ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
       ierr = nf90_put_att(irstfile, id_squ,  'long_name'    , 'cell center outcoming flux')
       ierr = nf90_put_att(irstfile, id_squ,  'units'        , 'm3 s-1')
       
       if ( iturbulencemodel >= 3 ) then
          ! Definition and attributes of vertical eddy viscosity vicwwu
          ierr = nf90_def_var(irstfile, 'vicwwu' , nf90_double, (/ id_wdim, id_flowlinkdim, id_timedim /) , id_vicwwu)
          ierr = nf90_put_att(irstfile, id_vicwwu,  'coordinates'  , 'FlowLink_xu FlowLink_yu')
          ierr = nf90_put_att(irstfile, id_vicwwu,  'long_name'    , 'turbulent vertical eddy viscosity')
          ierr = nf90_put_att(irstfile, id_vicwwu,  'units'        , 'm2 s-1')
          ierr = nf90_put_att(irstfile, id_vicwwu,  '_FillValue'   , dmiss)
          
          ! Definition and attributes of kinetic energy
          ierr = nf90_def_var(irstfile, 'turkin1' , nf90_double, (/ id_wdim, id_flowlinkdim, id_timedim /) , id_turkin1)
          ierr = nf90_put_att(irstfile, id_turkin1,  'coordinates'  , 'FlowLink_xu FlowLink_yu')
          ierr = nf90_put_att(irstfile, id_turkin1,  'standard_name', 'specific_turbulent_kinetic_energy_of_sea_water')
          ierr = nf90_put_att(irstfile, id_turkin1,  'long_name'    , 'turbulent kinetic energy')
          ierr = nf90_put_att(irstfile, id_turkin1,  'units'        , 'm2 s-2')
          ierr = nf90_put_att(irstfile, id_turkin1,  '_FillValue'   , dmiss)
          
          ! Definition and attributes of energy_dissipation or turbulence_time_scale
          ierr = nf90_def_var(irstfile, 'tureps1', nf90_double,   (/ id_wdim, id_flowlinkdim, id_timedim /)  , id_tureps1)
          ierr = nf90_put_att(irstfile, id_tureps1,  'coordinates'  , 'FlowLink_xu FlowLink_yu')
          ierr = nf90_put_att(irstfile, id_tureps1,  '_FillValue'   , dmiss)
          if( iturbulencemodel == 3 ) then
             ierr = nf90_put_att(irstfile, id_tureps1,  'standard_name', 'specific_turbulent_kinetic_energy_dissipation_in_sea_water')
             ierr = nf90_put_att(irstfile, id_tureps1,  'long_name'    , 'turbulent energy dissipation')
             ierr = nf90_put_att(irstfile, id_tureps1,  'units'        , 'm2 s-3')
          else if( iturbulencemodel == 4 ) then
             !ierr = nf90_put_att(irstfile, id_tureps1,  'standard_name', '')
             ierr = nf90_put_att(irstfile, id_tureps1,  'long_name'    , 'turbulent time scale')
             ierr = nf90_put_att(irstfile, id_tureps1,  'units'        , 's-1')
          endif
       endif
    else
       ! Definition and attributes of flow data on centres: x-component of the velocity
       ierr = nf90_def_var(irstfile, 'ucx', nf90_double, (/ id_flowelemdim, id_timedim /) , id_ucx)
       ierr = nf90_put_att(irstfile, id_ucx,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
       if (jsferic == 0) then
          ierr = nf90_put_att(irstfile, id_ucx, 'standard_name', 'sea_water_x_velocity')
       else
          ierr = nf90_put_att(irstfile, id_ucx, 'standard_name', 'eastward_sea_water_velocity')
       end if
       ierr = nf90_put_att(irstfile, id_ucx,  'long_name'    , 'velocity on flow element center, x-component')
       ierr = nf90_put_att(irstfile, id_ucx,  'units'        , 'm s-1')

       ! Definition and attributes of flow data on centres: y-component of the velocity
       ierr = nf90_def_var(irstfile, 'ucy', nf90_double, (/ id_flowelemdim, id_timedim /) , id_ucy)
       ierr = nf90_put_att(irstfile, id_ucy,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
       if (jsferic == 0) then
          ierr = nf90_put_att(irstfile, id_ucy, 'standard_name', 'sea_water_y_velocity')
       else
          ierr = nf90_put_att(irstfile, id_ucy, 'standard_name', 'northward_sea_water_velocity')
       end if
       ierr = nf90_put_att(irstfile, id_ucy,  'long_name'    , 'velocity on flow element center, y-component')
       ierr = nf90_put_att(irstfile, id_ucy,  'units'        , 'm s-1')
       
       ! Definition and attributes of flow data on edges: velocity magnitude at latest timestep
       ierr = nf90_def_var(irstfile, 'unorm' , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_unorm)
       ierr = nf90_put_att(irstfile, id_unorm,'long_name', 'normal component of sea_water_speed')
       ierr = nf90_put_att(irstfile, id_unorm,'units'        , 'm s-1')
       ierr = nf90_put_att(irstfile, id_unorm,'coordinates'  , 'FlowLink_xu FlowLink_yu')
    
       ! Definition and attributes of flow data on edges: velocity magnitude at previous timestep
       ierr = nf90_def_var(irstfile, 'u0'    , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_u0)
       ierr = nf90_put_att(irstfile, id_u0   ,'long_name',     'normal component of velocity through flow link at previous time t0')
       ierr = nf90_put_att(irstfile, id_u0   ,'units'        , 'm s-1')
       ierr = nf90_put_att(irstfile, id_u0   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')

       ! Definition and attributes of flow data on edges: velocity magnitude at previous timestep
       ierr = nf90_def_var(irstfile, 'q1'    , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_q1)
       ierr = nf90_put_att(irstfile, id_q1   ,'standard_name', 'discharge')
       ierr = nf90_put_att(irstfile, id_q1   ,'long_name',     'discharge through flow link at current time')
       ierr = nf90_put_att(irstfile, id_q1   ,'units'        , 'm3 s-1')
       ierr = nf90_put_att(irstfile, id_q1   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')
       
       ! Definition and attributes of flow data on edges: velocity magnitude at previous timestep
       ierr = nf90_def_var(irstfile, 'qa'    , nf90_double, (/ id_flowlinkdim, id_timedim /) , id_qa)
       ierr = nf90_put_att(irstfile, id_qa   ,'long_name',     'discharge used in advection')
       ierr = nf90_put_att(irstfile, id_qa   ,'units'        , 'm3 s-1')
       ierr = nf90_put_att(irstfile, id_qa   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')
       
       ! Definition and attributes of cell center outcoming flux
       ierr = nf90_def_var(irstfile, 'squ', nf90_double,   (/ id_flowelemdim, id_timedim /)  , id_squ)
       ierr = nf90_put_att(irstfile, id_squ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
       ierr = nf90_put_att(irstfile, id_squ,  'long_name'    , 'cell center outcoming flux')
       ierr = nf90_put_att(irstfile, id_squ,  'units'        , 'm3 s-1')
    end if

    ! Definition and attributes of flow data on centres: salinity
    if (jasal > 0) then 
       if (kmx > 0) then
          ierr = nf90_def_var(irstfile, 'sa1', nf90_double, (/ id_laydim, id_flowelemdim , id_timedim /), id_sa1)
       else
          ierr = nf90_def_var(irstfile, 'sa1', nf90_double, (/ id_flowelemdim , id_timedim /), id_sa1)
       endif
       ierr = nf90_put_att(irstfile, id_sa1,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
       ierr = nf90_put_att(irstfile, id_sa1,  'standard_name', 'sea_water_salinity')
       ierr = nf90_put_att(irstfile, id_sa1,  'long_name'    , 'salinity')
       ierr = nf90_put_att(irstfile, id_sa1,  'units'        , '1e-3')
    endif
    
    ! Definition and attributes of flow data on centres: temperature
    if (jatem > 0) then 
       if (kmx > 0) then
          ierr = nf90_def_var(irstfile, 'tem1', nf90_double, (/ id_laydim, id_flowelemdim , id_timedim /), id_tem1)
       else
          ierr = nf90_def_var(irstfile, 'tem1', nf90_double, (/ id_flowelemdim , id_timedim /), id_tem1)
       endif
       ierr = nf90_put_att(irstfile, id_tem1,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
       ierr = nf90_put_att(irstfile, id_tem1,  'standard_name', 'sea_water_temperature')
       ierr = nf90_put_att(irstfile, id_tem1,  'long_name'    , 'temperature in flow element')
       ierr = nf90_put_att(irstfile, id_tem1,  'units'        , 'degC')
    endif
    
    ! Tracer fields
    if(ITRA1 > 0) then
       if((.not.allocated(id_tr1)) .or. (ITRAN0 .ne. ITRAN)) then ! If id_tri is not allocated, or if last  tracer changes
          call realloc(id_tr1, ITRAN-ITRA1+1)
       endif
       do i=ITRA1,ITRAN
          j = i-ITRA1+1
          tmpstr = const_names(i)
          ! Forbidden chars in NetCDF names: space, /, and more.
          call replace_char(tmpstr,32,95) 
          call replace_char(tmpstr,47,95) 
          if(kmx > 0) then
             ierr = nf90_def_var(irstfile, trim(tmpstr), nf90_double, (/ id_laydim, id_flowelemdim , id_timedim /), id_tr1(j))
          else
             ierr = nf90_def_var(irstfile, trim(tmpstr), nf90_double, (/ id_flowelemdim , id_timedim /), id_tr1(j))
          endif
          ierr = nf90_put_att(irstfile, id_tr1(j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
          ierr = nf90_put_att(irstfile, id_tr1(j),  'standard_name', trim(tmpstr))
          ierr = nf90_put_att(irstfile, id_tr1(j),  'long_name'    , trim(tmpstr))
          if (const_units(j).ne.' ') then
             tmpstr = const_units(j)
          else
             tmpstr = '1e-3'
          endif
          ierr = nf90_put_att(irstfile, id_tr1(j),  'units'        , tmpstr)
       enddo
       ITRAN0 = ITRAN
    endif
    
    ! water quality bottom variables
    if(numwqbots > 0) then
       call realloc(id_rwqb, numwqbots, keepExisting = .false., fill = 0)
       do j=1,numwqbots
          tmpstr = wqbotnames(j)
          ! Forbidden chars in NetCDF names: space, /, and more.
          call replace_char(tmpstr,32,95) 
          call replace_char(tmpstr,47,95) 
          ierr = nf90_def_var(irstfile, trim(tmpstr), nf90_double, (/ id_flowelemdim , id_timedim /), id_rwqb(j))
          ierr = nf90_put_att(irstfile, id_rwqb(j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
          ierr = nf90_put_att(irstfile, id_rwqb(j),  'standard_name', trim(tmpstr))
          ierr = nf90_put_att(irstfile, id_rwqb(j),  'long_name'    , trim(tmpstr))
          tmpstr = wqbotunits(j)
          ierr = nf90_put_att(irstfile, id_rwqb(j),  'units'        , trim(tmpstr))
       enddo
       ITRAN0 = ITRAN
    endif

    if (jawave .eq. 4) then
      ierr = nf90_def_var(irstfile, 'E',  nf90_double, (/ id_flowelemdim, id_timedim /) , id_E)
      ierr = nf90_put_att(irstfile, id_E,   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
      ierr = nf90_put_att(irstfile, id_E,   'standard_name', 'sea_surface_bulk_wave_energy')                          ! not CF
      ierr = nf90_put_att(irstfile, id_E,   'long_name'    , 'wave energy per square meter')
      ierr = nf90_put_att(irstfile, id_E,   'units'        , 'J m-2')

      ierr = nf90_def_var(irstfile, 'thetamean',  nf90_double, (/ id_flowelemdim, id_timedim /) , id_thetamean)
      ierr = nf90_put_att(irstfile, id_thetamean,   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
      ierr = nf90_put_att(irstfile, id_thetamean,   'standard_name', 'sea_surface_wave_from_direction')                          ! not CF
      ierr = nf90_put_att(irstfile, id_thetamean,   'long_name'    , 'mean wave angle')
      ierr = nf90_put_att(irstfile, id_thetamean,   'units'        , 'rad')

      ierr = nf90_def_var(irstfile, 'sigmwav',  nf90_double, (/ id_flowelemdim, id_timedim /) , id_sigmwav)
      ierr = nf90_put_att(irstfile, id_sigmwav,   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
      ierr = nf90_put_att(irstfile, id_sigmwav,   'standard_name', 'sea_surface_wave_mean_frequency')                          ! not CF
      ierr = nf90_put_att(irstfile, id_sigmwav,   'long_name'    , 'mean wave frequency')
      ierr = nf90_put_att(irstfile, id_sigmwav,   'units'        , 'rad s-1')
    end if

    ndx1d = ndxi - ndx2d
    if (jased > 0 .and. stm_included) then
       ierr = nf90_def_dim(irstfile, 'nSedTot', stmpar%lsedtot, id_sedtotdim)
       ierr = nf90_def_dim(irstfile, 'nSedSus', stmpar%lsedsus, id_sedsusdim)
       ierr = nf90_def_dim(irstfile, 'nBedLayers', stmpar%morlyr%settings%nlyr, id_nlyrdim)
       !      
       if (stmpar%lsedsus .gt. 0) then
          if(.not.allocated(id_sf1)) then
             allocate(id_sf1(stmpar%lsedsus))
          endif
          do i=ISED1,ISEDN
             j = i-ISED1+1
             tmpstr = const_names(i)
             ! Forbidden chars in NetCDF names: space, /, and more.
             call replace_char(tmpstr,32,95) 
             call replace_char(tmpstr,47,95) 
             if(kmx > 0) then
                ierr = nf90_def_var(irstfile, trim(tmpstr), nf90_double, (/ id_laydim, id_flowelemdim , id_timedim /), id_sf1(j))
             else
                ierr = nf90_def_var(irstfile, trim(tmpstr), nf90_double, (/ id_flowelemdim , id_timedim /), id_sf1(j))
             endif
             ierr = nf90_put_att(irstfile, id_sf1(j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(irstfile, id_sf1(j),  'standard_name', trim(tmpstr)//' mass concentration')
             ierr = nf90_put_att(irstfile, id_sf1(j),  'long_name'    , trim(tmpstr)//' mass concentration')
             ierr = nf90_put_att(irstfile, id_sf1(j),  'units'        , 'kg m-3')
          end do 
      end if
      !
      ierr = nf90_def_var(irstfile, 'mor_bl',  nf90_double, (/ id_flowelemdim , id_timedim /) , id_morbl)
      ierr = nf90_put_att(irstfile, id_morbl,   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')                          ! not CF
      ierr = nf90_put_att(irstfile, id_morbl,   'long_name'    , 'Time-varying bottom level in flow cell center')          
      ierr = nf90_put_att(irstfile, id_morbl,   'units'        , 'm')
      !
      ierr = nf90_def_var(irstfile, 'morft', nf90_double, id_timedim,  id_morft)
      ierr = nf90_put_att(irstfile, id_morft,  'units'        , 'morphological days since '//refdat(1:4)//'-'//refdat(5:6)//'-'//refdat(7:8)//' 00:00:00')
      ierr = nf90_put_att(irstfile, id_morft,  'standard_name', 'morphological time')
      ierr = nf90_put_att(irstfile, id_morbl,   'units'       , 'days')      
      !
      if (ndx1d > 0 .and. stm_included) then
         nCrs = 0
         do i = 1,size(network%crs%cross)
            if (network%crs%cross(i)%crossindx ==	0) exit 
            nCrs = nCrs + 1
         enddo
         pCSs => network%CSDefinitions%CS
         jmax = 0
         do i = 1,size(pCSs)
            if (pCSs(i)%levelscount == 0) exit
            jmax = max(jmax,pCSs(i)%levelscount)
         enddo
         ierr = nf90_def_dim(irstfile, trim(mesh1dname)//'_crs_maxdim', jmax,    id_jmax)
         ierr = nf90_def_dim(irstfile, trim(mesh1dname)//'_ncrs',      nCrs,   id_ncrs)
         ierr = nf90_def_var(irstfile, 'flowelem_crs_z', nf90_double, (/ id_jmax, id_ncrs, id_timedim /), id_flowelemcrsz)
         ierr = nf90_put_att(irstfile, id_flowelemcrsz, 'long_name','cross-section points level')
         ierr = nf90_put_att(irstfile, id_flowelemcrsz, 'unit', 'm')
      endif
      !
      select case (stmpar%morlyr%settings%iunderlyr)
      case (1)
         ierr = nf90_def_var(irstfile, 'bodsed' , nf90_double, (/ id_sedtotdim , id_flowelemdim , id_timedim /) , id_bodsed)
         ierr = nf90_put_att(irstfile, id_bodsed ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_bodsed ,  'long_name'    , 'Available sediment mass in the bed in flow cell center')
         ierr = nf90_put_att(irstfile, id_bodsed ,  'units'        , 'kg m-2')
         !
      case (2)
         ierr = nf90_def_var(irstfile, 'msed' , nf90_double, (/ id_sedtotdim , id_nlyrdim , id_flowelemdim , id_timedim /) , id_msed)
         ierr = nf90_put_att(irstfile, id_msed ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_msed ,  'long_name'    , 'Available sediment mass in a layer of the bed in flow cell center')
         ierr = nf90_put_att(irstfile, id_msed ,  'units'        , 'kg m-2')

         ierr = nf90_def_var(irstfile, 'lyrfrac' , nf90_double, (/ id_sedtotdim , id_nlyrdim , id_flowelemdim , id_timedim /) , id_lyrfrac)
         ierr = nf90_put_att(irstfile, id_lyrfrac ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_lyrfrac ,  'long_name'    , 'Volume fraction in a layer of the bed in flow cell center')
         ierr = nf90_put_att(irstfile, id_lyrfrac ,  'units'        , '-')

         ierr = nf90_def_var(irstfile, 'thlyr' , nf90_double, (/ id_nlyrdim , id_flowelemdim , id_timedim /) , id_thlyr)
         ierr = nf90_put_att(irstfile, id_thlyr ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_thlyr ,  'long_name'    , 'Thickness of a layer of the bed in flow cell center')
         ierr = nf90_put_att(irstfile, id_thlyr ,  'units'        , 'm')
       end select
    end if
    
    ! Old morphology
    ! Definition and attributes of flow data on centres: sediment concentation and erodable layer thickness
    if (jased > 0 .and. .not.stm_included) then 
       ! Sediment concentration
       ierr = nf90_def_dim(irstfile, 'nFrac', mxgr, id_maxfracdim)  
       if (jaceneqtr == 1) then 
          ierr = nf90_inq_dimid(irstfile, 'nFlowElem', id_erolaydim) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
       else
          ierr = nf90_inq_dimid(irstfile, 'nNetNode' , id_erolaydim) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
       end if
       ierr = nf90_def_var(irstfile, 'sed'  , nf90_double, (/ id_maxfracdim  , id_flowelemdim, id_timedim /) , id_sed)
       ierr = nf90_put_att(irstfile, id_sed ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
       ierr = nf90_put_att(irstfile, id_sed ,  'standard_name', 'sediment_concentration')
       ierr = nf90_put_att(irstfile, id_sed ,  'long_name'    , 'sediment concentration at flow element centres')
       ierr = nf90_put_att(irstfile, id_sed ,  'units'        , 'kg m-3')
       
       ! Erodable thickness
       ierr = nf90_def_var(irstfile, 'ero' , nf90_double, (/ id_maxfracdim  , id_erolaydim, id_timedim /) , id_ero)
       if (jaceneqtr == 1) then 
          ierr = nf90_put_att(irstfile, id_ero,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
          ierr = nf90_put_att(irstfile, id_ero,  'long_name', 'erodable layer thickness per size fraction in flow element centers')
       else 
          ierr = nf90_put_att(irstfile, id_ero,  'coordinates'  , 'NetNode_x NetNode_y')
          ierr = nf90_put_att(irstfile, id_ero,  'long_name', 'erodable layer thickness per size fraction at flow element corners')
       endif
       !ierr = nf90_put_att(irstfile, id_ero,  'standard_name'    , 'Erodable layer thickness') ! Not CF
       ierr = nf90_put_att(irstfile, id_ero,  'units'        , 'm')
    endif
    
    ! Thatcher-Harleman boundary data
    if(allocated(threttim)) then
      if(jasal > 0) then
         if(max_threttim(ISALT) > 0d0) then
            ierr = nf90_def_var(irstfile, 'tsalbnd', nf90_double, (/ id_bndsaldim, id_timedim /), id_tsalbnd)
            ierr = nf90_put_att(irstfile, id_tsalbnd, 'long_name', 'Thatcher-Harlem time interval for salinity')
            ierr = nf90_put_att(irstfile, id_tsalbnd, 'units', 's')
            ierr = nf90_def_var(irstfile, 'zsalbnd', nf90_double, (/ id_bndsaldim, id_timedim /), id_zsalbnd)
            ierr = nf90_put_att(irstfile, id_zsalbnd, 'long_name', 'Thatcher-Harleman salinity')
            ierr = nf90_put_att(irstfile, id_zsalbnd, 'units', '1e-3')
         endif
      endif
      if(jatem > 0) then
         if(max_threttim(ITEMP) > 0d0) then
            ierr = nf90_def_var(irstfile, 'ttembnd', nf90_double, (/ id_bndtemdim, id_timedim /), id_ttembnd)
            ierr = nf90_put_att(irstfile, id_ttembnd, 'long_name', 'Thatcher-Harleman time interval for temperature')
            ierr = nf90_put_att(irstfile, id_ttembnd, 'units', 's')
            ierr = nf90_def_var(irstfile, 'ztembnd', nf90_double, (/ id_bndtemdim, id_timedim /), id_ztembnd)
            ierr = nf90_put_att(irstfile, id_ztembnd, 'long_name', 'Thatcher-Harleman temperature')
            ierr = nf90_put_att(irstfile, id_ztembnd, 'units', 'degrees celsius')
         endif
      endif
      if(jased > 0 .and. .not. stm_included) then
         if(max_threttim(ISED1) > 0d0) then
            ierr = nf90_def_var(irstfile, 'tsedbnd', nf90_double, (/ id_bndseddim, id_timedim /), id_tsedbnd)
            ierr = nf90_put_att(irstfile, id_tsedbnd, 'long_name', 'Thatcher-Harleman time interval for sediment')
            ierr = nf90_put_att(irstfile, id_tsedbnd, 'units', 's')
            ierr = nf90_def_var(irstfile, 'zsedbnd', nf90_double, (/ id_bndseddim, id_timedim /), id_zsedbnd)
            ierr = nf90_put_att(irstfile, id_zsedbnd, 'long_name', 'Thatcher-Harleman sediment concentration')
            ierr = nf90_put_att(irstfile, id_zsedbnd, 'units', 'kg m-3')
         endif
      endif
      if (numfracs > 0) then      ! TO DO
         if(.not. allocated(id_tsedfracbnd)) then
            allocate(id_tsedfracbnd(numfracs))
         endif
         if(.not. allocated(id_zsedfracbnd)) then
            allocate(id_zsedfracbnd(numfracs))
         endif
         do i=1,numfracs
            if(max_threttim(i+ISED1-1) > 0d0) then
               write(numsedfracstr,numformat) i
               ierr = nf90_def_var(irstfile, 'tsedfracbnd'//numsedfracstr, nf90_double, (/ id_bndsedfracdim(i), id_timedim /), id_tsedfracbnd(i))
               ierr = nf90_put_att(irstfile, id_tsedfracbnd(i), 'long_name', 'TH time interval '//numsedfracstr)
               ierr = nf90_put_att(irstfile, id_tsedfracbnd(i), 'units', 's')
               ierr = nf90_def_var(irstfile, 'zsedfracbnd'//numtrastr, nf90_double, (/ id_bndsedfracdim(i), id_timedim /), id_zsedfracbnd(i))
               ierr = nf90_put_att(irstfile, id_zsedfracbnd(i), 'long_name', 'TH sediment fraction '//numsedfracstr)
               ierr = nf90_put_att(irstfile, id_zsedfracbnd(i), 'units', 'kg m-3')
            endif
         enddo
      end if
      if(numtracers > 0) then
         if(.not. allocated(id_ttrabnd)) then
            allocate(id_ttrabnd(numtracers))
         endif
         if(.not. allocated(id_ztrabnd)) then
            allocate(id_ztrabnd(numtracers))
         endif
         do i=1,numtracers
            iconst = itrac2const(i)
            if(max_threttim(iconst) > 0d0) then
               write(numtrastr,numformat) i
               ierr = nf90_def_var(irstfile, 'ttrabnd'//numtrastr, nf90_double, (/ id_bndtradim(i), id_timedim /), id_ttrabnd(i))
               ierr = nf90_put_att(irstfile, id_ttrabnd(i), 'long_name', 'Thatcher-Harleman time interval for tracer '//numtrastr)
               ierr = nf90_put_att(irstfile, id_ttrabnd(i), 'units', 's')
               ierr = nf90_def_var(irstfile, 'ztrabnd'//numtrastr, nf90_double, (/ id_bndtradim(i), id_timedim /), id_ztrabnd(i))
               ierr = nf90_put_att(irstfile, id_ztrabnd(i), 'long_name', 'Thatcher-Harleman concentration of tracer '//numtrastr)
               ierr = nf90_put_att(irstfile, id_ztrabnd(i), 'units', '1e-3')
            endif
         enddo
      endif
    endif

    ! Gridmapping
    ierr = unc_add_gridmapping_att(irstfile, (/ id_s1, id_taus, id_ucx, id_ucy, id_unorm, id_sa1, id_sed /), jsferic)   ! add id_ucz?

    ! Flow cells
    ierr = nf90_def_var(irstfile, 'FlowElem_xzw', nf90_double, id_flowelemdim, id_flowelemxzw) ! For later cell-matching based on center of mass.
    ierr = nf90_def_var(irstfile, 'FlowElem_yzw', nf90_double, id_flowelemdim, id_flowelemyzw)
    ierr = unc_addcoordatts(irstfile, id_flowelemxzw, id_flowelemyzw, jsferic)
    ierr = nf90_put_att(irstfile, id_flowelemxzw, 'long_name'    , 'x-coordinate of flow element center of mass')
    ierr = nf90_put_att(irstfile, id_flowelemyzw, 'long_name'    , 'y-coordinate of flow element center of mass')
   
    if (lnx > 0) then
       ierr = nf90_def_var(irstfile, 'FlowLink_xu',     nf90_double, (/ id_flowlinkdim /) ,   id_flowlinkxu)
       ierr = nf90_def_var(irstfile, 'FlowLink_yu',     nf90_double, (/ id_flowlinkdim /) ,   id_flowlinkyu)
       ierr = unc_addcoordatts(irstfile, id_flowlinkxu, id_flowlinkyu, jsferic)
       ierr = nf90_put_att(irstfile, id_flowlinkxu, 'long_name'    , 'x-coordinate of flow link center (velocity point)')
       ierr = nf90_put_att(irstfile, id_flowlinkyu, 'long_name'    , 'y-coordinate of flow link center (velocity point)')
    endif


    ! The following variables will be used to merge the rst files, therefore, they are written only in parallel run
    if ( jampi.eq.1 ) then
       !   domain numbers and global node
       ierr = nf90_def_var(irstfile, 'FlowElemDomain', nf90_int, id_flowelemdim, id_flowelemdomain)
       ierr = nf90_put_att(irstfile, id_flowelemdomain, 'long_name'    ,   'domain number of flow element')
       ierr = nf90_def_var(irstfile, 'FlowElemGlobalNr', nf90_int, id_flowelemdim, id_flowelemglobalnr)
       ierr = nf90_put_att(irstfile, id_flowelemglobalnr, 'long_name'    ,   'global flow element numbering')

       if (lnx > 0) then
          ierr = nf90_def_var(irstfile, 'FlowLink',     nf90_int, (/ id_flowlinkptsdim, id_flowlinkdim /) ,   id_flowlink)
          ierr = nf90_put_att(irstfile, id_flowlink    , 'long_name'    , 'link/interface between two flow elements')
       endif
       if (nump1d2d > 0) then
          ierr = nf90_def_var(irstfile, 'NetElemNode', nf90_int, (/ id_netelemmaxnodedim, id_netelemdim /) , id_netelemnode)
          ierr = nf90_put_att(irstfile, id_netelemnode, 'long_name', 'mapping from net cell to its net nodes (counterclockwise)')
          ierr = nf90_put_att(irstfile, id_netelemnode, 'cf_role',   'face_node_connectivity')
          ierr = nf90_put_att(irstfile, id_netelemnode, 'start_index', 1)
          ierr = nf90_put_att(irstfile, id_netelemnode, '_FillValue', intmiss)
       endif
       if (numl > 0) then
          ! Netlinks
          ierr = nf90_def_var(irstfile, 'NetLink', nf90_int, (/ id_netlinkptsdim, id_netlinkdim /) , id_netlink)
          ierr = nf90_put_att(irstfile, id_netlink, 'standard_name', 'netlink')
          ierr = nf90_put_att(irstfile, id_netlink, 'long_name',     'link between two netnodes')
          ierr = nf90_put_att(irstfile, id_netlink, 'cf_role',       'edge_node_connectivity')
          ierr = nf90_put_att(irstfile, id_netlink, 'start_index', 1)
       endif    
    end if
    if (jarstbnd > 0 .and. ndxbnd > 0) then
       ierr = nf90_def_var(irstfile, 'FlowElem_xbnd', nf90_double, (/ id_bnddim/), id_flowelemxbnd)
       ierr = nf90_def_var(irstfile, 'FlowElem_ybnd', nf90_double, (/ id_bnddim/), id_flowelemybnd)
       if (jsferic == 0) then
          ierr = nf90_put_att(irstfile, id_flowelemxbnd, 'units',         'm')
          ierr = nf90_put_att(irstfile, id_flowelemybnd, 'units',         'm')
          ierr = nf90_put_att(irstfile, id_flowelemxbnd, 'long_name'    , 'x-coordinate of boundary points')
          ierr = nf90_put_att(irstfile, id_flowelemybnd, 'long_name'    , 'y-coordinate of boundary points')
       else
          ierr = nf90_put_att(irstfile, id_flowelemxbnd, 'units',         'degrees_east')
          ierr = nf90_put_att(irstfile, id_flowelemybnd, 'units',         'degrees_north')
          ierr = nf90_put_att(irstfile, id_flowelemxbnd, 'long_name'    , 'longitude for boundary points')
          ierr = nf90_put_att(irstfile, id_flowelemybnd, 'long_name'    , 'latitude for boundary points')
       endif    
       
       ierr = nf90_def_var(irstfile, 's0_bnd', nf90_double, (/ id_bnddim, id_timedim/) , id_s0bnd)
       ierr = nf90_put_att(irstfile, id_s0bnd,   'coordinates'  , 'FlowElem_xbnd FlowElem_ybnd')
       ierr = nf90_put_att(irstfile, id_s0bnd,   'long_name'    , 'water level at boundaries of previous timestep')
       ierr = nf90_put_att(irstfile, id_s0bnd,   'units'        , 'm')
       
       ierr = nf90_def_var(irstfile, 's1_bnd', nf90_double, (/ id_bnddim, id_timedim/) , id_s1bnd)
       ierr = nf90_put_att(irstfile, id_s1bnd,   'coordinates'  , 'FlowElem_xbnd FlowElem_ybnd')
       ierr = nf90_put_att(irstfile, id_s1bnd,   'long_name'    , 'water level at boundaries')
       ierr = nf90_put_att(irstfile, id_s1bnd,   'units'        , 'm')
       
       ierr = nf90_def_var(irstfile, 'bl_bnd', nf90_double, (/ id_bnddim, id_timedim/) , id_blbnd)
       ierr = nf90_put_att(irstfile, id_blbnd,   'coordinates'  , 'FlowElem_xbnd FlowElem_ybnd')
       ierr = nf90_put_att(irstfile, id_blbnd,   'long_name'    , 'bed level at boundaries')
       ierr = nf90_put_att(irstfile, id_blbnd,   'units'        , 'm')
    endif
    
    ! Write structure info.
    if (network%sts%numCulverts > 0) then ! write culvert info.
       ierr = nf90_def_dim(irstfile, 'nCulvert', network%sts%numculverts, id_culvertdim)
       ierr = nf90_def_var(irstfile, 'culvert_valve_opening_height', nf90_double, (/ id_culvertdim, id_timedim /), id_culvert_openh)
       ierr = nf90_put_att(irstfile, id_culvert_openh, 'long_name', 'Valve opening height of culvert')
       ierr = nf90_put_att(irstfile, id_culvert_openh, 'units', 'm')
    end if
    ierr = nf90_enddef(irstfile)

    ! Inquire var-id's
    ! NOTE: alle inq_varids below are not needed, since they have just been def_var'd above in this subroutine. Cleanup later together with rst/map cleanup. [AvD]
    if ( kmx>0 ) then
       ierr = nf90_inq_dimid(irstfile, 'laydim', id_laydim)
       ierr = nf90_inq_dimid(irstfile, 'wdim', id_wdim)
    end if        
    ierr = nf90_inq_varid(irstfile, 'timestep', id_timestep)
    ierr = nf90_inq_varid(irstfile, 's1', id_s1)
    ierr = nf90_inq_varid(irstfile, 's0', id_s0)
    ierr = nf90_inq_varid(irstfile, 'unorm'   , id_unorm   )
    ierr = nf90_inq_varid(irstfile, 'u0'      , id_u0      )
    ierr = nf90_inq_varid(irstfile, 'q1'      , id_q1      )
    ierr = nf90_inq_varid(irstfile, 'ucx'     , id_ucx     )
    ierr = nf90_inq_varid(irstfile, 'ucy'     , id_ucy     )
    ierr = nf90_inq_varid(irstfile, 'taus'    , id_taus)
    ierr = nf90_inq_varid(irstfile, 'czs'     , id_czs)
    ierr = nf90_inq_varid(irstfile, 'qa'      , id_qa)
    ierr = nf90_inq_varid(irstfile, 'squ'     , id_squ)
    
    if ( kmx>0 ) then
       ierr = nf90_inq_varid(irstfile, 'ucz', id_ucz)
       ierr = nf90_inq_varid(irstfile, 'ww1', id_ww1)
    end if
    if (jasal > 0) then 
       ierr = nf90_inq_varid(irstfile, 'sa1', id_sa1)
    endif

     ! JRE
     if (jawave .eq. 4) then 
        ierr = nf90_inq_varid(irstfile, 'E'        , id_E)
        ierr = nf90_inq_varid(irstfile, 'thetamean', id_thetamean)
        ierr = nf90_inq_varid(irstfile, 'sigmwav'  , id_sigmwav)
     endif

        !
    if (jatem > 0) then 
       ierr = nf90_inq_varid(irstfile, 'tem1', id_tem1)
    endif
    !
    if (jased > 0 .and. .not.stm_included) then 
       ierr = nf90_inq_dimid(irstfile, 'nFrac', id_maxfracdim)  
       if (jaceneqtr == 1) then 
          ierr = nf90_inq_dimid(irstfile, 'nFlowElem', id_erolaydim) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
       else
          ierr = nf90_inq_dimid(irstfile, 'nNetNode', id_erolaydim) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
       end if
       ierr = nf90_inq_varid(irstfile, 'sed', id_sed)
       ierr = nf90_inq_varid(irstfile, 'ero', id_ero)
    endif
    !
    if (network%sts%numCulverts > 0) then
       ierr = nf90_inq_varid(irstfile, 'culvert_valve_opening_height', id_culvert_openh)
    end if
                
    ! -- Start data writing (flow data) ------------------------
    itim = 1
    
    ! Write the data: time
    ierr = nf90_put_var(irstfile, id_time    , tim, (/ itim /))
    ierr = nf90_put_var(irstfile, id_timestep, dts, (/ itim /))

    ! Write the data: water level (new and old)
    ierr = nf90_put_var(irstfile, id_s1,  s1,   (/ 1, itim /), (/ ndxi, 1 /))
    ierr = nf90_put_var(irstfile, id_s0,  s0,   (/ 1, itim /), (/ ndxi, 1 /))
    
    ! Write the data: bed level
    ierr = nf90_put_var(irstfile, id_bl,  bl,   (/ 1, itim /), (/ ndxi, 1 /))

    ! Write the data: tau current 
    if (jawave < 3) then   ! If jawave > 2, then taus is obtained from subroutine tauwave (taus = taucur + tauwave).
        call gettaus(1)
    elseif (jamapchezy > 0) then
        call gettaus(2)
    endif
    if(jamaptaucurrent > 0) then
        ierr = nf90_put_var(irstfile, id_taus, taus,  (/ 1, itim /), (/ ndxi, 1 /))
    endif  
    if(jamapchezy > 0) then
        ierr = nf90_put_var(irstfile, id_czs, czs,  (/ 1, itim /), (/ ndxi, 1 /))
    endif
    
    ! Write the data: velocities (components and magnitudes)
    if (kmx > 0) then
!      3D
       call reconstructucz(0)
       call unc_append_3dflowgeom_put(irstfile, 1, itim) 
       !do kk=1,Ndxi
       !   call getkbotktop(kk,kb,kt)
       !   ierr = nf90_put_var(irstfile, id_ucx  , ucx(kb:kt),  start=(/ 1, kk, itim /), count=(/ kt-kb+1, 1, 1 /))
       !   ierr = nf90_put_var(irstfile, id_ucy  , ucy(kb:kt),  start=(/ 1, kk, itim /), count=(/ kt-kb+1, 1, 1 /))
       !   ierr = nf90_put_var(irstfile, id_ucz  , ucz(kb:kt),  start=(/ 1, kk, itim /), count=(/ kt-kb+1, 1, 1 /))
       !   ierr = nf90_put_var(irstfile, id_ww1  , ww1(kb-1:kt),start=(/ 1, kk, itim /), count=(/ kt-kb+2, 1, 1 /))
       !end do
       !do LL=1,lnx
       !   call getLbotLtopmax(LL,Lb,Lt)
       !   ierr = nf90_put_var(irstfile, id_unorm, u1(Lb:Lt),  start=(/ 1, LL, itim /), count=(/ Lt-Lb+1, 1, 1 /))
       !   ierr = nf90_put_var(irstfile, id_u0   , u0(Lb:Lt),  start=(/ 1, LL, itim /), count=(/ Lt-Lb+1, 1, 1 /))
       !   ierr = nf90_put_var(irstfile, id_q1   , q1(Lb:Lt),  start=(/ 1, LL, itim /), count=(/ Lt-Lb+1, 1, 1 /))
       !end do

       work1 = dmiss
       do kk=1,ndxi
          call getkbotktop(kk,kb,kt)
          call getlayerindices(kk, nlayb, nrlay)
          do k = kb,kt
             work1(k-kb+nlayb,kk) = ucx(k)
          enddo
       enddo
       ierr = nf90_put_var(irstfile, id_ucx, work1(1:kmx,1:ndxi), start=(/ 1, 1, itim /), count=(/ kmx, ndxi, 1 /))
       
       work1 = dmiss
       do kk=1,ndxi
          call getkbotktop(kk,kb,kt)
          call getlayerindices(kk, nlayb, nrlay)
          do k = kb,kt
             work1(k-kb+nlayb,kk) = ucy(k)
          enddo
       enddo
       ierr = nf90_put_var(irstfile, id_ucy, work1(1:kmx,1:ndxi), start=(/ 1, 1, itim /), count=(/ kmx, ndxi, 1 /))
       
       work1 = dmiss
       do kk=1,ndxi
          call getkbotktop(kk,kb,kt)
          call getlayerindices(kk, nlayb, nrlay)
          do k = kb,kt
             work1(k-kb+nlayb,kk) = ucz(k)
          enddo
       enddo
       ierr = nf90_put_var(irstfile, id_ucz, work1(1:kmx,1:ndxi), start=(/ 1, 1, itim /), count=(/ kmx, ndxi, 1 /))
       
       work0 = dmiss
       do kk=1,ndxi
         call getkbotktop(kk,kb,kt)
         call getlayerindices(kk, nlayb, nrlay)
         do k = kb-1,kt
             work0(k-kb+nlayb,kk) = ww1(k)
         enddo
       enddo
       ierr = nf90_put_var(irstfile, id_ww1, work0(0:kmx,1:ndxi), start=(/ 1, 1, itim /), count=(/ kmx+1, ndxi, 1 /))
            
       work1 = dmiss
       do LL=1,lnx
          call getLbotLtopmax(LL,Lb,Ltx)
          call getlayerindicesLmax(LL, nlaybL, nrlayLx)
          do L = Lb,Ltx
             work1(L-Lb+nlaybL,LL) = u1(L)
          enddo
       enddo
       ierr = nf90_put_var(irstfile, id_unorm, work1(1:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx, lnx, 1 /))
       
       work1 = dmiss
       do LL=1,lnx
          call getLbotLtopmax(LL,Lb,Ltx)
          call getlayerindicesLmax(LL, nlaybL, nrlayLx)
          do L = Lb,Ltx
             work1(L-Lb+nlaybL,LL) = u0(L)
          enddo
       enddo
       ierr = nf90_put_var(irstfile, id_u0   , work1(1:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx, lnx, 1 /))
       
       work1 = dmiss
       do LL=1,lnx
          call getLbotLtopmax(LL,Lb,Ltx)
          call getlayerindicesLmax(LL, nlaybL, nrlayLx)
          do L = Lb,Ltx
             work1(L-Lb+nlaybL,LL) = q1(L)
          enddo
       enddo
       ierr = nf90_put_var(irstfile, id_q1   , work1(1:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx, lnx, 1 /))
       
       ! write averaged u1
       ierr = nf90_put_var(irstfile, id_unorma, u1(1:lnx), start=(/ 1, itim /), count=(/ lnx, 1 /))
       
       if (iturbulencemodel >= 3) then
         ! write vertical eddy viscosity vicwwu
          work0 = dmiss
          do LL=1,lnx    
             call getLbotLtopmax(LL,Lb,Ltx)
             call getlayerindicesLmax(LL, nlaybL, nrlayLx)
             do L = Lb-1,Ltx
                work0(L-Lb+nlaybL,LL) = vicwwu(L)
             enddo
             enddo
          ierr = nf90_put_var(irstfile, id_vicwwu, work0(0:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx+1, lnx, 1 /))

      
           ! write tureps1
           work0 = dmiss
           do LL=1,lnx
              call getLbotLtopmax(LL,Lb,Ltx)
              call getlayerindicesLmax(LL, nlaybL, nrlayLx)
              do L = Lb-1,Ltx
                 work0(L-Lb+nlaybL,LL) = tureps1(L)
              enddo
           enddo
           ierr = nf90_put_var(irstfile, id_tureps1, work0(0:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx+1, lnx, 1 /))
           
           ! write turkin1
           work0 = dmiss
           do LL=1,lnx
              call getLbotLtopmax(LL,Lb,Ltx)
              call getlayerindicesLmax(LL, nlaybL, nrlayLx)
              do L = Lb-1,Ltx
                 work0(L-Lb+nlaybL,LL) = turkin1(L)
              enddo
           enddo
           ierr = nf90_put_var(irstfile, id_turkin1, work0(0:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx+1, lnx, 1 /))
        end if
    
        ! qw
        work0 = dmiss
        do kk=1,ndxi
           call getkbotktop(kk,kb,kt)
           call getlayerindices(kk, nlayb, nrlay)
           do k = kb-1,kt
              work0(k-kb+nlayb,kk) = qw(k)
           enddo
        enddo
        ierr = nf90_put_var(irstfile, id_qw, work0(0:kmx,1:ndxi), start=(/ 1, 1, itim /), count=(/ kmx+1, ndxi, 1 /))
        
        ! qa
        work1 = dmiss
        do LL=1,lnx
           call getLbotLtopmax(LL,Lb,Ltx)
           call getlayerindicesLmax(LL, nlaybL, nrlayLx)
           do L = Lb,Ltx
              work1(L-Lb+nlaybL,LL) = qa(L)
           enddo
        enddo
        ierr = nf90_put_var(irstfile, id_qa, work1(1:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx, lnx, 1 /))
        
        ! sqi
        work0 = dmiss
        do kk=1,ndxi
           call getkbotktop(kk,kb,kt)
           call getlayerindices(kk, nlayb, nrlay)
           do k = kb-1,kt
              work0(k-kb+nlayb,kk) = sqi(k)
           enddo
        enddo
        ierr = nf90_put_var(irstfile, id_sqi, work0(0:kmx,1:ndxi), start=(/ 1, 1, itim /), count=(/ kmx+1, ndxi, 1 /))
        
        ! squ
        work0 = dmiss
        do kk=1,ndxi
           call getkbotktop(kk,kb,kt)
           call getlayerindices(kk, nlayb, nrlay)
           do k = kb-1,kt
              work0(k-kb+nlayb,kk) = squ(k)
           enddo
        enddo
        ierr = nf90_put_var(irstfile, id_squ, work0(0:kmx,1:ndxi), start=(/ 1, 1, itim /), count=(/ kmx+1, ndxi, 1 /))
       
    else
       ierr = nf90_put_var(irstfile, id_ucx  , ucx,  (/ 1, itim /), (/ ndxi, 1 /))
       ierr = nf90_put_var(irstfile, id_ucy  , ucy,  (/ 1, itim /), (/ ndxi, 1 /))
       ierr = nf90_put_var(irstfile, id_unorm, u1 ,  (/ 1, itim /), (/ lnx , 1 /))
       ierr = nf90_put_var(irstfile, id_u0   , u0 ,  (/ 1, itim /), (/ lnx , 1 /))
       ierr = nf90_put_var(irstfile, id_q1   , q1 ,  (/ 1, itim /), (/ lnx , 1 /))
       ierr = nf90_put_var(irstfile, id_qa   , qa ,  (/ 1, itim /), (/ lnx , 1 /))
       ierr = nf90_put_var(irstfile, id_squ  , squ,  (/ 1, itim /), (/ ndxi, 1 /))
    end if
   
    if (jasal > 0) then  ! Write the data: salinity
       if (kmx > 0) then
          !do kk=1,Ndxi
          !   call getkbotktop(kk,kb,kt)
          !   ierr = nf90_put_var(irstfile, id_sa1, sa1(kb:kt), (/ 1, kk, itim /), (/ kt-kb+1, 1, 1 /))
          !end do
          work1 = dmiss
          do kk = 1,ndxi
             call getkbotktop(kk,kb,kt)
             call getlayerindices(kk, nlayb, nrlay)  
             do k = kb,kt
                work1(k-kb+nlayb,kk) = sa1(k)
             enddo
          end do
          ierr = nf90_put_var(irstfile, id_sa1, work1(1:kmx,1:ndxi), (/ 1, 1, itim /), (/ kmx, ndxi, 1 /))
       else
          ierr = nf90_put_var(irstfile, id_sa1, sa1, (/ 1, itim /), (/ ndxi, 1 /))
       end if
    endif
    
    if (jatem > 0) then ! Write the data: temperature
       if ( kmx>0 ) then
          !do kk=1,Ndxi
          !   call getkbotktop(kk,kb,kt)
          !   ierr = nf90_put_var(irstfile, id_tem1, tem1(kb:kt), (/ 1, kk, itim /), (/ kt-kb+1, 1, 1 /))
          !end do
          work1 = dmiss
          do kk = 1,ndxi
             call getkbotktop(kk,kb,kt)
             call getlayerindices(kk, nlayb, nrlay)  
             do k = kb,kt
                work1(k-kb+nlayb,kk) = constituents(itemp, k)
             enddo
          end do
          ierr = nf90_put_var(irstfile, id_tem1, work1(1:kmx,1:ndxi), (/ 1, 1, itim /), (/ kmx, ndxi, 1 /))
       else
          do k = 1,ndxi
             tem1(k) = constituents(itemp, k)
          enddo   
          ierr = nf90_put_var(irstfile, id_tem1, tem1, (/ 1, itim /), (/ ndxi, 1 /))
       end if
    endif
    
    if (jamapconst > 0 .and. ITRA1 > 0) then
       allocate(dum(ndxi))
       do j=ITRA1,ITRAN
          if (kmx > 0) then
!           3D
            work1 = dmiss
            do kk=1,ndxi
               call getkbotktop(kk,kb,kt)
               call getlayerindices(kk, nlayb, nrlay) 
               do k = kb,kt
                  work1(k-kb+nlayb,kk) = constituents(j,k)
               enddo
            enddo
            ierr = nf90_put_var(irstfile, id_tr1(j-ITRA1+1), work1(1:kmx,1:ndxi), (/ 1, 1, itim /), (/ kmx, ndxi, 1 /))
            !   if ( ierr.ne.0 ) exit  ! probably newly added tracer in the GUI
          else
             do kk=1,ndxi
                dum(kk) = constituents(j,kk)
             enddo
             ierr = nf90_put_var(irstfile, id_tr1(j-ITRA1+1), dum, (/ 1, itim /), (/ ndxi, 1 /) )
          endif
       enddo
       if (allocated(dum)) deallocate(dum)
    end if
    
    ! water quality bottom variables
    if (numwqbots > 0) then
       allocate(dum(ndxi))
       do j=1,numwqbots
          do kk=1,ndxi
             dum(kk) = wqbot(j,kk)
          enddo
          ierr = nf90_put_var(irstfile, id_rwqb(j), dum(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /) )
       enddo
       if (allocated(dum)) deallocate(dum)
    end if

    ! JRE: review what is really necessary
    if (jawave .eq. 4) then
       ierr = nf90_put_var(irstfile, id_E, E, (/ 1, itim /), (/ ndxi, 1 /))
       ierr = nf90_put_var(irstfile, id_thetamean, thetamean, (/ 1, itim /), (/ ndxi, 1 /))
       ierr = nf90_put_var(irstfile, id_sigmwav, sigmwav, (/ 1, itim /), (/ ndxi, 1 /))
    end if
    
    if (jased > 0 .and. stm_included) then
       ! concentrations
       if (stmpar%lsedsus .gt. 0) then
          allocate(dum(ndxi))
          do j=ISED1,ISEDN
             if (kmx > 0) then
                do kk=1,ndxi
                   call getkbotktop(kk,kb,kt)
                   call getlayerindices(kk, nlayb, nrlay)
                   do k = kb,kt
                      work1(k-kb+nlayb,kk) = constituents(j,k)
                   enddo
                enddo
                ierr = nf90_put_var(irstfile, id_sf1(j-ISED1+1), work1(1:kmx,1:ndxi), (/ 1, 1, itim /), (/ kmx, ndxi, 1 /))
             else
                do kk=1,ndxi
                   dum(kk) = constituents(j,kk)
                enddo
                ierr = nf90_put_var(irstfile, id_sf1(j-ISED1+1), dum, (/ 1, itim /), (/ ndxi, 1 /) )
             endif
          enddo
          if (allocated(dum)) deallocate(dum)
       end if
       ! morbl
       ierr = nf90_put_var(irstfile, id_morbl, bl, (/1 , itim/),(/ndxi, 1/))
       ierr = nf90_put_var(irstfile, id_morft, stmpar%morpar%morft, (/ itim /))
       
       select case (stmpar%morlyr%settings%iunderlyr)
       case (1)
          ! bodsed
          ierr = nf90_put_var(irstfile, id_bodsed, stmpar%morlyr%state%bodsed(:, 1:ndxi), (/ 1, 1, itim /), (/ stmpar%lsedtot, ndxi, 1 /))
       case (2)
          ! msed
          ierr = nf90_put_var(irstfile, id_msed, stmpar%morlyr%state%msed(:,:,1:ndxi), (/ 1, 1, 1, itim /), (/ stmpar%lsedtot, stmpar%morlyr%settings%nlyr, ndxi, 1 /))
          ! lyrfrac
          if (.not. allocated(frac) ) allocate( frac(1:ndx, 1:stmpar%morlyr%settings%nlyr, stmpar%lsedtot) )
          frac = -999d0
          do l = 1, stmpar%lsedtot
             if (stmpar%morlyr%settings%iporosity==0) then
                dens = stmpar%sedpar%cdryb(l)
             else
                dens = stmpar%sedpar%rhosol(l)
             endif
             do k = 1, stmpar%morlyr%settings%nlyr
                do nm = 1, ndxi
                   if (stmpar%morlyr%state%thlyr(k,nm)>0.0_fp) then
                        frac(nm, k, l) = stmpar%morlyr%state%msed(l, k, nm)/(dens*stmpar%morlyr%state%svfrac(k, nm) * &
                                         stmpar%morlyr%state%thlyr(k, nm))
                   else
                        frac(nm, k, l) = 0d0
                   endif
                enddo
             enddo
          enddo
          ! thlyr
          ierr = nf90_put_var(irstfile, id_thlyr, stmpar%morlyr%state%thlyr(:,1:ndxi), (/ 1, 1, itim /), (/ stmpar%morlyr%settings%nlyr, ndxi, 1 /))
          ierr = nf90_put_var(irstfile, id_lyrfrac, frac(1:ndxi, :, :), (/ 1, 1, 1, itim /), (/ ndxi, stmpar%morlyr%settings%nlyr, stmpar%lsedtot, 1 /))
       end select   
    end if
    
    ! Write the data: sediment Herman
    if (jased > 0 .and. jased < 4) then ! Write the data: sediment
       ierr = nf90_put_var(irstfile, id_sed, sed, (/ 1, 1, itim /), (/ mxgr, ndxi, 1 /))
       ierr = nf90_put_var(irstfile, id_ero, grainlay, (/ 1, 1, itim /), (/ mxgr, size(grainlay,2) , 1 /))
       ! TODO: AvD: size(grainlay,2) is always correct (mxn), but we have a problem if jaceneqtr==2 and mxn/=numk,
       ! because then the dimension for ero is set to nNetNode, and coordinate attribute refers to NetNode_x
       ! (both length numk), whereas ero itself is shorter than numk.
    endif
    
    ! Write the data: TH boundaries
    if(allocated(threttim)) then
      if(jasal > 0) then
         if(max_threttim(ISALT) > 0d0) then
            ierr = nf90_put_var(irstfile, id_tsalbnd, thtbnds, (/1, itim/), (/nbnds, 1/))
            ierr = nf90_put_var(irstfile, id_zsalbnd, thzbnds, (/1, itim/), (/nbnds*kmxd, 1/))
         endif
      endif
      if(jatem > 0) then
         if(max_threttim(ITEMP) > 0d0) then
            ierr = nf90_put_var(irstfile, id_ttembnd, thtbndtm, (/1, itim/), (/nbndtm, 1/))
            ierr = nf90_put_var(irstfile, id_ztembnd, thzbndtm, (/1, itim/), (/nbndtm*kmxd, 1/))
         endif
      endif
      if(jased > 0 .and. .not. stm_included) then
         if(max_threttim(ISED1) > 0d0) then
            ierr = nf90_put_var(irstfile, id_tsedbnd, thtbndsd, (/1, itim/), (/nbndsd, 1/))
            ierr = nf90_put_var(irstfile, id_zsedbnd, thzbndsd, (/1, itim/), (/nbndsd*kmxd, 1/))
         endif
      endif
      if (numfracs > 0) then !JRE sedfrac
         do i = 1, numfracs
            if(max_threttim(i+ISED1-1) > 0d0) then
               ierr = nf90_put_var(irstfile, id_tsedfracbnd(i), bndsf(i)%tht, (/1, itim/), (/nbndsf(i), 1/))
               ierr = nf90_put_var(irstfile, id_zsedfracbnd(i), bndsf(i)%thz, (/1, itim/), (/nbndsf(i)*kmxd, 1/))
            endif
         end do
      end if
      if(numtracers > 0) then
         do i=1,numtracers
            iconst = itrac2const(i)
            if(max_threttim(iconst) > 0d0) then
               ierr = nf90_put_var(irstfile, id_ttrabnd(i), bndtr(i)%tht, (/1, itim/), (/nbndtr(i), 1/))
               ierr = nf90_put_var(irstfile, id_ztrabnd(i), bndtr(i)%thz, (/1, itim/), (/nbndtr(i)*kmxd, 1/))
            endif
         enddo
      endif
    endif
    
    ! Write 1D cross sections
    if (ndx1d > 0 .and. stm_included) then
       if (.not.allocated(work1d_z)) then
          allocate( work1d_z(jmax,nCrs) )
       endif
       work1d_z = dmiss
       do i = 1,nCrs
          pCS => network%crs%cross(i)%tabdef
          !if (pCS%crosstype == CS_TABULATED) then
             do j = 1,pCS%levelscount
                work1d_z(j,i) = pCS%height(j)
             enddo
          !endif
       enddo
       ierr = nf90_put_var(irstfile, id_flowelemcrsz, work1d_z(1:jmax,1:nCrs), start=(/ 1, 1, itim /), count=(/ jmax, nCrs, 1 /) )
       deallocate( work1d_z )
    endif
    
   
   ! Flow cell cc coordinates (only 1D + internal 2D)
    ierr = nf90_put_var(irstfile, id_flowelemxzw, xzw(1:ndxi))
    ierr = nf90_put_var(irstfile, id_flowelemyzw, yzw(1:ndxi))
    if (lnx > 0) then
       ! Flow links velocity points
       ierr = nf90_put_var(irstfile, id_flowlinkxu, xu(1:lnx))
       ierr = nf90_put_var(irstfile, id_flowlinkyu, yu(1:lnx))
    end if
    
    ! Write structure info.
    if (network%sts%numculverts > 0) then
       ierr = nf90_put_var(irstfile, id_culvert_openh, valculvert(11, 1:network%sts%numculverts), (/1, itim/), (/network%sts%numculverts, 1/))
    end if


    if ( jampi.eq.1 ) then  
       ! flow cell domain numbers    
       ierr = nf90_put_var(irstfile, id_flowelemdomain, idomain(1:ndxi))
       ierr = nf90_put_var(irstfile, id_flowelemglobalnr, iglobal_s(1:ndxi))
       
       ! flow link
       ierr = nf90_put_var(irstfile, id_flowlink,   ln(:,1:lnx))
       
       ! netelemnode
       if (nump1d2d > 0) then
          ierr = nf90_inquire_dimension(irstfile, id_netelemmaxnodedim, len = nv)
          allocate(netcellnod(nv, nump1d2d))
          netcellnod = intmiss
          do k = 1, nump1d2d
             nv1 = netcell(k)%n
             netcellnod(1:nv1,k) = netcell(k)%nod(1:nv1)
          enddo
          ierr = nf90_put_var(irstfile, id_netelemnode, netcellnod)
          call check_error(ierr, 'Write netcell nodes')
          deallocate(netcellnod)
       end if
       if (numl > 0) then
           allocate(kn1write(numL))
           allocate(kn2write(numL))
           do L=1,numL
              kn1write(L)=kn(1,L)
              kn2write(L)=kn(2,L)
           end do
           ierr = nf90_put_var(irstfile, id_netlink,     kn1write, count=(/ 1, numl /), start=(/1,1/))
           ierr = nf90_put_var(irstfile, id_netlink,     kn2write, count=(/ 1, numl /), start=(/2,1/))
           deallocate(kn1write)
           deallocate(kn2write)
       endif
    end if
    if (jarstbnd > 0 .and. ndxbnd > 0) then
       ! boundary points coordinates, and correspondng waterlevel and bedlevel
       if (jampi == 0) then
           ierr = nf90_put_var(irstfile, id_flowelemxbnd, xz(ndxi+1:ndx))
           ierr = nf90_put_var(irstfile, id_flowelemybnd, yz(ndxi+1:ndx))
           
           do i = 1, ndxbnd
              j = ln(1,lnxi+i)
              tmp_s0(i) = s0(j)
              tmp_s1(i) = s1(j)
              tmp_bl(i) = bl(j)
           enddo
       else
          do i = 1, ndxbnd
             k = ibnd_own(i)             
             j = ln(1,lnxi+k)
             tmp_x(i)  = xz(j)
             tmp_y(i)  = yz(j)
             tmp_s0(i) = s0(j)
             tmp_s1(i) = s1(j)
             tmp_bl(i) = bl(j)
          enddo
          ierr = nf90_put_var(irstfile, id_flowelemxbnd, tmp_x)
          ierr = nf90_put_var(irstfile, id_flowelemybnd, tmp_y)
       endif
       ierr = nf90_put_var(irstfile, id_s0bnd, tmp_s0, (/ 1, itim /), (/ ndxbnd, 1 /))
       ierr = nf90_put_var(irstfile, id_s1bnd, tmp_s1, (/ 1, itim /), (/ ndxbnd, 1 /))
       ierr = nf90_put_var(irstfile, id_blbnd, tmp_bl, (/ 1, itim /), (/ ndxbnd, 1 /))
    endif
end subroutine unc_write_rst_filepointer



!> Writes a single snapshot of the unstructured flow net + flow data to a netCDF file.
!! If file exists, it will be overwritten. Therefore, only use this routine
!! for separate snapshots, the automated map file should be filled by calling
!! unc_write_map_filepointer directly instead!
subroutine unc_write_map(filename, iconventions)
    implicit none

    character(len=*),  intent(in) :: filename
    integer, optional, intent(in) :: iconventions !< Unstructured NetCDF conventions (either UNC_CONV_CFOLD or UNC_CONV_UGRID)

    type(t_unc_mapids) :: mapids
    integer :: ierr, iconv

    if (.not. present(iconventions)) then
       iconv = UNC_CONV_CFOLD
    else
       iconv = iconventions
    end if

    ierr = unc_create(filename, 0, mapids%ncid)
    if (ierr /= nf90_noerr) then
        call mess(LEVEL_ERROR, 'Could not create map file '''//trim(filename)//'''.')
        call check_error(ierr)
        return
    end if

    if (iconv == UNC_CONV_UGRID) then
       call unc_write_map_filepointer_ugrid(mapids, 0d0)
    else
       call unc_write_map_filepointer(mapids%ncid, 0d0, 1)
    end if

    ierr = unc_close(mapids%ncid)
end subroutine unc_write_map


!> Writes map/flow data to an already opened netCDF dataset. NEW version according to UGRID conventions + much cleanup.
!! The netnode and -links have been written already.
subroutine unc_write_map_filepointer_ugrid(mapids, tim, jabndnd) ! wrimap
   use m_flow
   use m_flowtimes
   use m_flowgeom
   use m_heatfluxes
   use m_sferic
   use network_data
   use m_sediment
   use m_bedform
   use m_wind
   use m_flowparameters, only: jatrt, jamd1dfile
   use m_fm_wq_processes
   use m_xbeach_data
   use m_transport, only: NUMCONST, itemp, ITRA1, ITRAN, ISED1, ISEDN, constituents, const_names, const_units, id_const, u1sed, q1sed, ucxsed, ucysed, qcxsed, qcysed, xsedflux, ysedflux
   use m_particles, only: japart, jatracer, part_iconst
   use m_alloc
   use m_waves
   use m_missing
   use m_CrossSections 
   use unstruc_channel_flow, only: network
   use m_oned_functions, only: gridpoint2cross
   use string_module, only: replace_multiple_spaces_by_single_spaces
   use m_save_ugrid_state, only: mesh1dname

   implicit none

   type(t_unc_mapids), intent(inout) :: mapids   !< Set of file and variable ids for this map-type file.
   real(kind=hp),      intent(in)    :: tim
   integer, optional,  intent(in)    :: jabndnd !< Whether to include boundary nodes (1) or not (0). Default: no.

   integer                       :: jabndnd_

   integer                       :: idims(2)

   integer, save                 :: ierr, ndim

   double precision, allocatable                       :: ust_x(:), ust_y(:), wavout(:), wavout2(:)
   character(len=255)                                  :: tmpstr
   integer                                             :: nm
   character(16)                                       :: dxname
   character(64)                                       :: dxdescr
   character(15)                                       :: transpunit
   double precision                                    :: rhol, dens, mortime, wavfac
   double precision                                    :: moravg, morstarthyd, dmorft, dmorfs, rhodt
   double precision, dimension(:,:), allocatable       :: poros, toutputx, toutputy
   double precision, dimension(:,:,:), allocatable     :: frac
   integer, dimension(:), allocatable                  :: flag_val
   character(len=10000)                                :: flag_mean
   
   double precision, dimension(:), allocatable         :: numlimdtdbl
   integer, dimension(:),   allocatable                :: idum
   double precision, dimension(:), allocatable         :: work1d
   double precision                                    :: vicc, dicc, ddum

!    Secondary Flow 
!        id_rsi, id_rsiexact, id_dudx, id_dudy, id_dvdx, id_dvdy, id_dsdx, id_dsdy

   integer :: iid, i, j, jj, numContPts, numNodes, itim, n, LL, L, Lb, Lt, LLL, k, k1, k2, k3
   integer :: kk, kb, kt, kkk, found
   integer :: nlayb, nrlay
   integer :: ndxndxi ! Either ndx or ndxi, depending on whether boundary nodes also need to be written.
   integer :: iLocS ! Either UNC_LOC_S or UNC_LOC_S3D, depending on whether layers are present.
   integer :: iLocU ! Either UNC_LOC_U or UNC_LOC_U3D, depending on whether layers are present.
   double precision, dimension(:), allocatable :: windx, windy, toutput, rks, tetatemp, wa
   character( len = 4 ) :: str
   
   type(t_CSType), pointer                       :: pCS
   type(t_CSType), pointer, dimension(:)         :: pCSs
   integer                                       :: jmax, ndx1d
   double precision, dimension(:,:), allocatable :: work1d_z, work1d_n
   double precision, dimension(:,:,:), allocatable :: work3d

   pCSs => network%CSDefinitions%CS
   ndx1d = ndxi - ndx2d

   if (ndxi <= 0) then
      call mess(LEVEL_WARN, 'No flow elements in model, will not write flow geometry.')
      return
   end if

   if (present(jabndnd)) then
      jabndnd_ = jabndnd
   else
      jabndnd_ = 0
   endif

   ! Include boundary cells in output (ndx) or not (ndxi)
   if (jabndnd_ == 1) then
      ndxndxi   = ndx
   else
      ndxndxi   = ndxi
   end if

   ! Prepare the U/S location for either 2D or 3D for subsequent def_var and put_var sequences.
   if (kmx > 0) then ! If layers present.
      iLocS = UNC_LOC_S3D
      iLocU = UNC_LOC_U3D
   else
      iLocS = UNC_LOC_S
      iLocU = UNC_LOC_U
   end if


   call realloc(mapids%id_const, (/ MAX_ID_VAR, NUMCONST/), keepExisting=.false.)
   
   ! DEBUG
   if (jasedtranspveldebug>0) then
      call realloc(mapids%id_xsedflux, (/ MAX_ID_VAR, NUMCONST/), keepExisting=.false., fill = -1)
      call realloc(mapids%id_ysedflux, (/ MAX_ID_VAR, NUMCONST/), keepExisting=.false., fill = -1)
   end if
   !\ DEBUG
   
   ! Use nr of dimensions in netCDF file a quick check whether vardefs were written
   ! before in previous calls.
   ndim = 0
   ierr = nf90_inquire(mapids%ncid, nDimensions=ndim)

   ! Only write net and flow geometry data the first time, or for a separate map file.
   if (ndim == 0) then

      ierr = ug_addglobalatts(mapids%ncid, ug_meta_fm)
      call unc_write_flowgeom_filepointer_ugrid(mapids%ncid, mapids%id_tsp, jabndnd_)

      ! Current time t1
      ierr = nf90_def_dim(mapids%ncid, 'time', nf90_unlimited, mapids%id_tsp%id_timedim)
      call check_error(ierr, 'def time dim')
      tmpstr = 'seconds since '//refdat(1:4)//'-'//refdat(5:6)//'-'//refdat(7:8)//' 00:00:00'
      ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_time, nf90_double, (/ mapids%id_tsp%id_timedim /), 'time', 'time', '', trim(tmpstr))
      
      ! Size of latest timestep
      ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_timestep, nf90_double, (/ mapids%id_tsp%id_timedim /), 'timestep', '',     'Latest computational timestep size in each output interval', 's')

      if (jamapnumlimdt > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_numlimdt   , nf90_double, UNC_LOC_S, 'Numlimdt'  , '', 'Number of times flow element was Courant limiting', '1', cell_method = 'point')
      endif

      ! Water levels
      if (jamaps1 > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hs, nf90_double, UNC_LOC_S, 'waterdepth', 'sea_floor_depth_below_sea_surface', 'Water depth at pressure points', 'm')
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s1, nf90_double, UNC_LOC_S, 's1',         'sea_surface_height',                'Water level', 'm')
      end if
      if (jamaps0 > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s0, nf90_double, UNC_LOC_S, 's0', 'sea_surface_height', 'Water level on previous timestep', 'm')
      end if

      ! Volumes
      if (jamapvol1 > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vol1, nf90_double, iLocS, 'vol1',         '',                'volume of water in grid cell', 'm3')
      end if

      ! Calculated time step per cell based on CFL number
      if(jamapdtcell > 0 ) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dtcell, nf90_double, iLocS, 'dtcell', '', 'Time step per cell based on CFL', 's')
      endif
      
      ! Velocities
      if (jamaphu > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hu, nf90_double, UNC_LOC_U, 'hu', 'sea_floor_depth_below_sea_surface', 'water depth at velocity points', 'm')
      end if

      if (jamapau > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_au, nf90_double, iLocU, 'au',         '',                'normal flow area between two neighbouring grid cells', 'm2')
      end if

      if(jamapu1 > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_u1, nf90_double, iLocU, 'u1', '', 'Velocity at velocity point, n-component', 'm s-1')
         ierr = unc_put_att(mapids%ncid, mapids%id_u1, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
      end if
      if(jamapu0 > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_u0, nf90_double, iLocU, 'u0', '', 'Velocity at velocity point at previous time step, n-component', 'm s-1')
         ierr = unc_put_att(mapids%ncid, mapids%id_u0, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
      end if
      if(jamapucvec > 0) then
         if (jaeulervel==1 .and. jawave>0) then ! TODO: AvD:refactor such that yes<->no Eulerian velocities are in parameters below:
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucx, nf90_double, iLocS, 'ucx', 'sea_water_x_eulerian_velocity',      'Flow element center eulerian velocity vector, x-component', 'm s-1')
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucy, nf90_double, iLocS, 'ucy', 'sea_water_y_eulerian_velocity',      'Flow element center eulerian velocity vector, y-component', 'm s-1')
         else
            if (jsferic == 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucx, nf90_double, iLocS, 'ucx', 'sea_water_x_velocity',      'Flow element center velocity vector, x-component', 'm s-1')
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucy, nf90_double, iLocS, 'ucy', 'sea_water_y_velocity',      'Flow element center velocity vector, y-component', 'm s-1')
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucx, nf90_double, iLocS, 'ucx', 'eastward_sea_water_velocity',      'Flow element center velocity vector, x-component', 'm s-1')
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucy, nf90_double, iLocS, 'ucy', 'northward_sea_water_velocity',      'Flow element center velocity vector, y-component', 'm s-1')
            end if
         end if
         if (kmx > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucz, nf90_double, UNC_LOC_S3D, 'ucz', 'upward_sea_water_velocity', 'Flow element center velocity vector, z-component', 'm s-1')
            ! Depth-averaged cell-center velocities in 3D:
            if (jsferic == 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxa, nf90_double, UNC_LOC_S, 'ucxa', 'sea_water_x_velocity', 'Flow element center depth-averaged velocity, x-component', 'm s-1')
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucya, nf90_double, UNC_LOC_S, 'ucya', 'sea_water_y_velocity', 'Flow element center depth-averaged velocity, y-component', 'm s-1')
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxa, nf90_double, UNC_LOC_S, 'ucxa', 'eastward_sea_water_velocity', 'Flow element center depth-averaged velocity, x-component', 'm s-1')
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucya, nf90_double, UNC_LOC_S, 'ucya', 'northward_sea_water_velocity', 'Flow element center depth-averaged velocity, y-component', 'm s-1')
            end if
         end if
      end if
      if(jamapucmag > 0) then
         if (jaeulervel==1 .and. jawave>0) then ! TODO: AvD:refactor such that yes<->no Eulerian velocities are in parameters below:
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmag, nf90_double, iLocS, 'ucmag', 'sea_water_eulerian_speed', 'Flow element center eulerian velocity magnitude', 'm s-1')
         else
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmag, nf90_double, iLocS, 'ucmag', 'sea_water_speed', 'Flow element center velocity magnitude', 'm s-1')
         end if
         if (kmx > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmaga, nf90_double, UNC_LOC_S, 'ucmaga', 'sea_water_speed', 'Flow element center depth-averaged velocity magnitude', 'm s-1')
         end if
      end if
      if(jamapucqvec > 0) then
         if (jaeulervel==1 .and. jawave>0) then ! TODO: AvD:refactor such that yes<->no Eulerian velocities are in parameters below:
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxq, nf90_double, iLocS, 'ucxq', 'ucxq_eulerian_velocity', 'Flow element center eulerian velocity vector, ucxq-component', 'm s-1')
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucyq, nf90_double, iLocS, 'ucyq', 'ucyq_eulerian_velocity', 'Flow element center eulerian velocity vector, ucyq-component', 'm s-1')
         else
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxq, nf90_double, iLocS, 'ucxq', 'ucxq_velocity', 'Flow element center velocity vector based discharge, x-component', 'm s-1')
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucyq, nf90_double, iLocS, 'ucyq', 'ucyq_velocity', 'Flow element center velocity vector based discharge, y-component', 'm s-1')
         end if
      end if
      if (kmx > 0) then
         if(jamapww1 > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ww1, nf90_double, UNC_LOC_W, 'ww1', 'upward_sea_water_velocity', 'Upward velocity on vertical interface, n-component', 'm s-1')
         end if
         if(jamaprho > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rho, nf90_double, UNC_LOC_S3D, 'rho', 'sea_water_density', 'Flow element center mass density', 'kg m-3')
         end if
      end if

      if(jamapq1 > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1, nf90_double, iLocU, 'q1', 'discharge', 'Discharge through flow link at current time', 'm3 s-1', cell_method = 'sum')
         ierr = unc_put_att(mapids%ncid, mapids%id_q1, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
      end if

      if(jamapq1main > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1main, nf90_double, iLocU, 'q1_main', '', 'Main channel discharge through flow link at current time', 'm3 s-1', cell_method = 'sum')
         ierr = unc_put_att(mapids%ncid, mapids%id_q1main, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
      end if

      if (jamapviu > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_viu, nf90_double, iLocU, 'viu', '', 'Horizontal eddy viscosity', 'm2 s-1')
      end if
      if (jamapdiu > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_diu, nf90_double, iLocU, 'diu', '', 'Horizontal eddy diffusivity', 'm2 s-1')
      end if

      ! Bed shear stress
      if (jamaptaucurrent > 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_taus   , nf90_double, UNC_LOC_S, 'taus'  , '', 'Total bed shear stress', 'N m-2')
      endif

      ! Chezy data on flow-nodes
      if (jamapchezy > 0) then 
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_czs , nf90_double, UNC_LOC_S, 'czs'  , '', 'Chezy roughness', 'm0.5s-1')
            ! WO: m0.5s-1 does not follow standard ? (which accepts only integral powers?)
      endif 


      ! Constituents
      if (jamapsal > 0 .and. jasal > 0) then 
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sa1, nf90_double, iLocS, 'sa1', 'sea_water_salinity', 'Salinity in flow element', '1e-3')
      end if

      if (jamaptem > 0 .and. jatem > 0) then 
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tem1, nf90_double, iLocS, 'tem1', 'sea_water_temperature', 'Temperature in flow element', 'degC')
      endif
      
      if (jamapspir > 0 .and. jasecflow > 0) then
         if (kmx < 1) then 
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_spircrv, nf90_double, UNC_LOC_S, 'spircrv', 'streamline_curvature', 'Flow streamline curvature'  , '1/m' )
         endif
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_spirint, nf90_double, UNC_LOC_S, 'spirint', 'spiral_intensity'    , 'Spiral flow intensity'       , 'm/s')
      endif

      ! Tracers
      if (jamapconst > 0 .and. ITRA1 > 0) then
         call realloc(mapids%id_const, (/ MAX_ID_VAR, NUMCONST /), keepExisting=.false., fill = 0)
         do j=ITRA1,ITRAN
            tmpstr = const_names(j)
            ! Forbidden chars in NetCDF names: space, /, and more.
            call replace_char(tmpstr,32,95) 
            call replace_char(tmpstr,47,95) 
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_const(:,j), nf90_double, iLocS, trim(tmpstr), &
                                   '', trim(const_names(j)) // ' in flow element', const_units(j))
         end do
      endif
      ! Discharges
      ! TODO: AVD...
      ! TIDAL TURBINES: Insert equivalent of addturbine_cnst and addturbine_time here
      
    ! water quality bottom variables
      if (numwqbots > 0) then
         call realloc(mapids%id_wqb, (/ 3, numwqbots /), keepExisting=.false., fill = 0)
         do j=1,numwqbots
            tmpstr = wqbotnames(j)
            ! Forbidden chars in NetCDF names: space, /, and more.
            call replace_char(tmpstr,32,95) 
            call replace_char(tmpstr,47,95) 
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqb(:,j), nf90_double, UNC_LOC_S, trim(tmpstr), &
                                   '', trim(wqbotnames(j)) // ' in flow element', wqbotunits(j))
         end do
      endif

      ! WAQ extra outputs
      if (jawaqproc > 0) then
         if (noout_map > 0) then
            call realloc(mapids%id_waq, (/ 3, noout_map /), keepExisting=.false., fill = 0)
            do j=1,noout_map
               tmpstr = ' '
               write (tmpstr, "('water_quality_output_',I0)") j
               ierr = unc_def_var_map(mapids%ncid,  mapids%id_tsp, mapids%id_waq(:,j), nf90_double, iLocS, tmpstr, &
                                      '', outputs%names(j), outputs%units(j))
               tmpstr = trim(outputs%names(j))//' - '//trim(outputs%descrs(j))//' in flow element'
               call replace_multiple_spaces_by_single_spaces(tmpstr)
               ierr = nf90_put_att(mapids%ncid, mapids%id_waq(2,j),  'description'  , tmpstr)
            end do
         endif
         if (noout_statt > 0) then
            call realloc(mapids%id_wqst, (/ 3, noout_statt /), keepExisting=.false., fill = 0)
            do j=1,noout_statt
               jj = noout_user + j
               tmpstr = ' '
               write (tmpstr, "('water_quality_stat_',I0)") j
               ierr = unc_def_var_map(mapids%ncid,  mapids%id_tsp, mapids%id_wqst(:,j), nf90_double, iLocS, tmpstr, &
                                      '', outputs%names(jj), outputs%units(jj))
               tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%descrs(jj))//' in flow element'
               call replace_multiple_spaces_by_single_spaces(tmpstr)
               ierr = nf90_put_att(mapids%ncid, mapids%id_wqst(2,j),  'description'  , tmpstr)
            end do
         endif
         if (noout_state > 0) then
            call realloc(mapids%id_wqse, (/ 3, noout_state /), keepExisting=.false., fill = 0)
            do j=1,noout_state
               jj = noout_user + noout_statt + j
               tmpstr = ' '
               write (tmpstr, "('water_quality_stat_',I0)") noout_statt + j
               ierr = unc_def_var_map(mapids%ncid,  mapids%id_tsp, mapids%id_wqse(:,j), nf90_double, iLocS, tmpstr, &
                                      '', outputs%names(jj), outputs%units(jj), 0)
               tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%descrs(jj))//' in flow element'
               call replace_multiple_spaces_by_single_spaces(tmpstr)
               ierr = nf90_put_att(mapids%ncid, mapids%id_wqse(2,j),  'description'  , tmpstr)
            end do
         endif
      endif

      ! water quality mass balance areas
      if (nomba > 0) then
         ierr = unc_def_var_map(mapids%ncid,  mapids%id_tsp, mapids%id_mba(:), nf90_int, UNC_LOC_S, 'water_quality_mba', '', 'Water quality mass balance areas', '', is_timedep=0)
         call realloc(flag_val, nomba, keepExisting = .false., fill = 0)
         flag_mean = ' '
         do j=nomba,1,-1
            flag_val(j) = j
            flag_mean = trim(mbaname(j))//' '//flag_mean
         enddo
         ierr = nf90_put_att(mapids%ncid, mapids%id_mba(2), 'flag_values', flag_val)
         ierr = nf90_put_att(mapids%ncid, mapids%id_mba(2), 'flag_meanings', flag_mean)
      endif
      
      ! Meteo forcings
      if (jamaprain > 0 .and. jarain /= 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rain,  nf90_double, UNC_LOC_S, 'rainfall_rate',  'rainfall_rate', 'Rainfall rate', 'm s-1')
      end if

      if (jamapwind > 0 .and. japatm /= 0) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_patm,  nf90_double, UNC_LOC_S, 'Patm',  'surface_air_pressure', 'Atmospheric pressure near surface', 'N m-2')
      end if

      if ((jamapwind > 0 .or. jamapwindstress > 0) .and. jawind /= 0) then
         if (jawindstressgiven == 0 .and. jamapwind > 0) then
            if (jsferic == 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windx,  nf90_double, UNC_LOC_S, 'windx',  'x_wind', 'velocity of air on flow element center, x-component', 'm s-1')
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windy,  nf90_double, UNC_LOC_S, 'windy',  'y_wind', 'velocity of air on flow element center, y-component', 'm s-1')
               ! Also wind on flow links
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windxu, nf90_double, UNC_LOC_U, 'windxu', 'x_wind', 'velocity of air on flow links, x-component', 'm s-1')
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windyu, nf90_double, UNC_LOC_U, 'windyu', 'y_wind', 'velocity of air on flow links, y-component', 'm s-1')
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windx,  nf90_double, UNC_LOC_S, 'windx',  'eastward_wind',  'velocity of air on flow element center, x-component', 'm s-1')
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windy,  nf90_double, UNC_LOC_S, 'windy',  'northward_wind', 'velocity of air on flow element center, y-component', 'm s-1')
               ! Also wind on flow links
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windxu, nf90_double, UNC_LOC_U, 'windxu', 'eastward_wind', 'velocity of air on flow links, x-component', 'm s-1')
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windyu, nf90_double, UNC_LOC_U, 'windyu', 'northward_wind', 'velocity of air on flow links, y-component', 'm s-1')
            end if
         endif
         if (jamapwindstress > 0) then
            if (jsferic == 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressx, nf90_double, UNC_LOC_S, 'windstressx',  &
                  'surface_downward_x_stress', 'wind stress on flow element center, x-component', 'N m-2')
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressy, nf90_double, UNC_LOC_S, 'windstressy',  &
                  'surface_downward_y_stress', 'wind stress on flow element center, y-component', 'N m-2')
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressx, nf90_double, UNC_LOC_S, 'windstressx',  &
                  'surface_downward_eastward_stress',  'wind stress on flow element center, x-component', 'N m-2')
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressy, nf90_double, UNC_LOC_S, 'windstressy',  &
                  'surface_downward_northward_stress', 'wind stress on flow element center, y-component', 'N m-2')
            end if
         endif
      endif

      ! Heat fluxes
      if (jamapheatflux > 0 .and. jatem > 1) then ! here less verbose

         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_tair   , nf90_double, UNC_LOC_S, 'Tair' , 'surface_temperature'      , 'Air temperature near surface'     , 'degC')
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_tair   , nf90_double, UNC_LOC_S, 'Rhum' , 'surface_specific_humidity', 'Relative humidity near surface'    , '')
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_tair   , nf90_double, UNC_LOC_S, 'Clou' , 'cloud_area_fraction'      , 'Cloudiness'                       , '1')

         if (jatem == 5) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qsun  , nf90_double, UNC_LOC_S, 'Qsun'  , 'surface_net_downward_shortwave_flux'                     , 'Solar influx'                         , 'W m-2')
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qeva  , nf90_double, UNC_LOC_S, 'Qeva'  , 'surface_downward_latent_heat_flux'                       , 'Evaporative heat flux'                , 'W m-2')   
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qcon  , nf90_double, UNC_LOC_S, 'Qcon'  , 'surface_downward_sensible_heat_flux'                     , 'Sensible heat flux'                   , 'W m-2')   
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qlong , nf90_double, UNC_LOC_S, 'Qlong' , 'surface_net_downward_longwave_flux'                      , 'Long wave back radiation'             , 'W m-2')   
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qfreva, nf90_double, UNC_LOC_S, 'Qfreva', 'downward_latent_heat_flux_in_sea_water_due_to_convection', 'Free convection evaporative heat flux', 'W m-2')   
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qfrcon, nf90_double, UNC_LOC_S, 'Qfrcon', 'surface_downward_sensible_heat_flux_due_to_convection'   , 'Free convection sensible heat flux'   , 'W m-2')   
         endif

         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_Qtot   , nf90_double, UNC_LOC_S, 'Qtot'  , 'surface_downward_heat_flux_in_sea_water'                 , 'Total heat flux'                      , 'W m-2')
         
      endif

      ! Turbulence.
      if (jamaptur > 0 .and. kmx > 0) then
         if (iturbulencemodel >= 3) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_turkin1, nf90_double, UNC_LOC_WU, 'turkin1', 'specific_turbulent_kinetic_energy_of_sea_water', 'turbulent kinetic energy',          'm2 s-2')
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vicwwu,  nf90_double, UNC_LOC_WU, 'vicwwu',  'eddy_viscosity', 'turbulent vertical eddy viscosity', 'm2 s-1')
            if (iturbulencemodel == 3) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tureps1, nf90_double, UNC_LOC_WU, 'tureps1', 'specific_turbulent_kinetic_energy_dissipation_in_sea_water',    'turbulent energy dissipation', 'm2 s-3')
            else if (iturbulencemodel == 4) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tureps1, nf90_double, UNC_LOC_WU, 'tureps1', '', 'turbulent time scale',         's-1')
            end if
         end if
      end if

      ! Sediment transport (via morphology module)
      if (jamapsed > 0 .and. jased > 0 .and. stm_included) then
         ierr = nf90_def_dim(mapids%ncid, 'nSedTot', stmpar%lsedtot, mapids%id_tsp%id_sedtotdim)
         ierr = nf90_def_dim(mapids%ncid, 'nSedSus', stmpar%lsedsus, mapids%id_tsp%id_sedsusdim)
         ierr = nf90_def_dim(mapids%ncid, 'nBedLayers', stmpar%morlyr%settings%nlyr, mapids%id_tsp%id_nlyrdim)
         ierr = nf90_def_dim(mapids%ncid, 'nStringlen', 100, mapids%id_tsp%id_strlendim)
         !
         if (.not. stmpar%morpar%moroutput%cumavg) then   ! only one average transport value at end of model run
            ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_sedavgtim, nf90_double, (/  1  /), 'sedAvgTim', '', 'Time interval over which cumulative transports are calculated', 's')
         endif
         !
         call realloc(mapids%id_dxx, (/stmpar%morpar%nxx, 3 /), keepExisting=.false.)
         !
         ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_morfac, nf90_double, (/ mapids%id_tsp%id_timedim /), 'morfac', '', 'Average morphological factor over elapsed morphological time', '-')
         ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_morft, nf90_double,  (/ mapids%id_tsp%id_timedim /), 'morft',  '', 'Current morphological time', 's')
         !
         ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_frac_name, nf90_char,  (/ mapids%id_tsp%id_strlendim, mapids%id_tsp%id_sedtotdim /), 'sedfrac_name', '', 'Sediment fraction name', '-')
         !
         select case(stmpar%morpar%moroutput%transptype)
            case (0)
               transpunit = 'kg s-1 m-1'
            case (1)
               transpunit = 'm3 s-1 m-1'
            case (2)
               transpunit = 'm3 s-1 m-1'
         end select
         !
         ! Suspended transport related quantities
         !
         if (stmpar%lsedsus .gt. 0) then
            !
            if ( kmx > 0 ) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_kmxsed, nf90_int, UNC_LOC_S, 'kmxsed', '', 'Bottom layer for sed calculations', '-', dimids = (/  -2, mapids%id_tsp%id_sedsusdim, -1 /))
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ws, nf90_double, UNC_LOC_W, 'ws', '', 'Sediment settling velocity', 'm s-1', dimids = (/ -3, -2, mapids%id_tsp%id_sedsusdim, -1 /))
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ws, nf90_double, UNC_LOC_S, 'ws', '', 'Sediment settling velocity', 'm s-1', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
            end if
            !
            if (kmx == 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rsedeq, nf90_double, UNC_LOC_S, 'rsedeq', '', 'Equilibrium sediment concentration', 'kg m-3', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
            end if
            !
            if (stmpar%morpar%moroutput%aks) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_aks, nf90_double, UNC_LOC_S, 'aks', '', 'Near-bed reference concentration height', 'm', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rca, nf90_double, UNC_LOC_S, 'rca', '', 'Near-bed reference concentration', 'kg m-3', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
            end if
            !
            if (stmpar%morpar%moroutput%sourcesink) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sourse , nf90_double, UNC_LOC_S, 'sourse'  , '', 'Source term suspended sediment fractions', 'kg m-3 s-1', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sinkse , nf90_double, UNC_LOC_S, 'sinkse'  , '', 'Sink term suspended sediment fractions', 's-1', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
            endif
            !
            if ( kmx > 0 ) then
               if (stmpar%morpar%moroutput%suvcor) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_scrn , nf90_double, UNC_LOC_U, 'e_scrn'  , '', 'Near-bed transport correction in face-normal direction', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
               end if
            endif
            !
            if (kmx > 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedfrac, nf90_double, UNC_LOC_S3D, 'sedfrac_concentration', '', 'Sediment concentration in flow cell', 'kg m-3',dimids = (/ -3, -2, mapids%id_tsp%id_sedsusdim, -1 /))
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedfrac, nf90_double, UNC_LOC_S, 'sedfrac_concentration', '', 'Sediment concentration in flow cell', 'kg m-3',dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
            end if
            !
         endif
         
         ! roughness heights for current and current and wave related roughness
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_z0c   , nf90_double, UNC_LOC_U, 'z0ucur'  , '', 'Current related roughness'        , 'm', dimids = (/ -2,  -1 /))
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_z0r   , nf90_double, UNC_LOC_U, 'z0urou'  , '', 'Current-wave related roughness'   , 'm', dimids = (/ -2,  -1 /))
         
         ! default sediment transport output (suspended and bedload) on flow links 
         if (stmpar%lsedsus > 0) then 
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ssn   , nf90_double, UNC_LOC_U, 'ssn'  , '', 'Suspended load transport, n-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sst   , nf90_double, UNC_LOC_U, 'sst'  , '', 'Suspended load transport, t-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
            
            ! DEBUG: get transport velocities and discharges from transport module on output file
            if (jasedtranspveldebug>0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_u1s   , nf90_double, UNC_LOC_U, 'u1sed', '', 'Sed adv velocity,  n-component'          , 'm s-1',    dimids = (/ -2, -1 /))
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_q1s   , nf90_double, UNC_LOC_U, 'q1sed', '', 'Sed adv discharge, n-component'          , 'm3 s-1',   dimids = (/ -2, -1 /))
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ucxs  , nf90_double, UNC_LOC_S, 'ucxs' , 'sed_adv_x_velocity',  'Flow element center velocity vector for sed adv, x-component', 'm s-1',    dimids = (/ -2, -1 /))
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ucys  , nf90_double, UNC_LOC_S, 'ucys' , 'sed_adv_y_velocity',  'Flow element center velocity vector for sed adv, y-component', 'm s-1',    dimids = (/ -2, -1 /))
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_qcxs  , nf90_double, UNC_LOC_S, 'qcxs' , 'sed_adv_x_discharge',  'Flow element center discharge vector for sed adv, x-component', 'm3 s-1',    dimids = (/ -2, -1 /))
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_qcys  , nf90_double, UNC_LOC_S, 'qcys' , 'sed_adv_y_discharge',  'Flow element center discharge vector for sed adv, y-component', 'm3 s-1',    dimids = (/ -2, -1 /))
               if (stmpar%lsedsus .gt. 0) then
                  do j = ISED1, ISEDN
                     tmpstr = const_names(j)
                     ! Forbidden chars in NetCDF names: space, /, and more.
                     call replace_char(tmpstr,32,95) 
                     call replace_char(tmpstr,47,95) 
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_xsedflux(:,j)  , nf90_double, UNC_LOC_S, trim(tmpstr)//'xsedflux', '', trim(tmpstr) // ' sed flux, x-component', '', dimids = (/ -2, -1 /))
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ysedflux(:,j)  , nf90_double, UNC_LOC_S, trim(tmpstr)//'ysedflux', '', trim(tmpstr) // ' sed flux, y-component', '', dimids = (/ -2, -1 /))
                  end do
               end if
            !\ DEBUG
            end if  
         endif 
         
         if (stmpar%lsedtot > 0) then 
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbn   , nf90_double, UNC_LOC_U, 'sbn'  , '', 'Bed load transport, n-component'         , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbt   , nf90_double, UNC_LOC_U, 'sbt'  , '', 'Bed load transport, t-component'         , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
         endif

         if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_e_dzdn , nf90_double, UNC_LOC_U, 'e_dzdn'  , '', 'Bed slope parallel to flow link', '-', dimids = (/ -2, -1 /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_e_dzdt , nf90_double, UNC_LOC_U, 'e_dzdt'  , '', 'Bed slope normal to flow link', '-', dimids = (/ -2, -1 /))
         end if
         
         if (stmpar%morpar%moroutput%uuuvvv) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_uuu , nf90_double, UNC_LOC_S, 'uuu'  , '', 'Characteristic velocity in cell centre, x-component', 'm s-1', dimids = (/ -2, -1 /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_vvv , nf90_double, UNC_LOC_S, 'vvv'  , '', 'Characteristic velocity in cell centre, y-component', 'm s-1', dimids = (/ -2, -1 /))   
         end if
      
         if (stmpar%morpar%moroutput%umod) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_umod , nf90_double, UNC_LOC_S, 'umod'  , '', 'Characteristic velocity magnitude in cell centre', 'm s-1', dimids = (/ -2, -1 /))
         end if
         
         if (stmpar%morpar%moroutput%zumod) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_zumod , nf90_double, UNC_LOC_S, 'zumod'  , '', 'Height above bed for characteristic velocity in cell centre', 'm', dimids = (/ -2, -1 /))
         end if

         if (stmpar%morpar%moroutput%ustar) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ustar , nf90_double, UNC_LOC_S, 'ustar'  , '', 'Bed shear velocity in cell centre', 'm s-1', dimids = (/ -2, -1 /))
         end if

         if (stmpar%morpar%moroutput%sbcuv) then
            if (stmpar%morpar%moroutput%rawtransports) then    ! if either of these is true, the reconstruction is done outside this subroutine, invalidating Willem's approach to have 'unspoiled' transports
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcx   , nf90_double, UNC_LOC_S, 'sbcx'  , '', 'Bed load transport due to currents, x-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcy   , nf90_double, UNC_LOC_S, 'sbcy'  , '', 'Bed load transport due to currents, y-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
            end if
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcx_reconstructed   , nf90_double, UNC_LOC_S, 'sbcx_reconstructed'  , '', 'Bed load transport due to currents (reconstructed), x-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcy_reconstructed   , nf90_double, UNC_LOC_S, 'sbcy_reconstructed'  , '', 'Bed load transport due to currents (reconstructed), y-component'   , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
         endif
         
         if (stmpar%morpar%moroutput%sbwuv) then
            if (stmpar%morpar%moroutput%rawtransports) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwx   , nf90_double, UNC_LOC_S, 'sbwx'  , '', 'Bed load transport due to waves, x-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwy   , nf90_double, UNC_LOC_S, 'sbwy'  , '', 'Bed load transport due to waves, y-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
            endif
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwx_reconstructed   , nf90_double, UNC_LOC_S, 'sbwx_reconstructed'  , '', 'Bed load transport due to waves (reconstructed), x-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwy_reconstructed   , nf90_double, UNC_LOC_S, 'sbwy_reconstructed'  , '', 'Bed load transport due to waves (reconstructed), y-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
         endif
         
         if (stmpar%morpar%moroutput%sscuv) then    ! This differs from Delft3D 4
            if (stmpar%morpar%moroutput%rawtransports) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscx   , nf90_double, UNC_LOC_S, 'sscx'  , '', 'Suspended load transport due to currents, x-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscy   , nf90_double, UNC_LOC_S, 'sscy'  , '', 'Suspended load transport due to currents, y-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
            endif
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscx_reconstructed   , nf90_double, UNC_LOC_S, 'sscx_reconstructed'  , '', 'Suspended load transport due to currents (reconstructed), x-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscy_reconstructed   , nf90_double, UNC_LOC_S, 'sscy_reconstructed'  , '', 'Suspended load transport due to currents (reconstructed), y-component'      , transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
         endif

         if (stmpar%morpar%moroutput%sswuv) then
            if (stmpar%morpar%moroutput%rawtransports) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswx   , nf90_double, UNC_LOC_S, 'sswx'  , '', 'Suspended load transport due to waves, x-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswy   , nf90_double, UNC_LOC_S, 'sswy'  , '', 'Suspended load transport due to waves, y-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
            endif
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswx_reconstructed   , nf90_double, UNC_LOC_S, 'sswx_reconstructed'  , '', 'Suspended load transport due to waves (reconstructed), x-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswy_reconstructed   , nf90_double, UNC_LOC_S, 'sswy_reconstructed'  , '', 'Suspended load transport due to waves (reconstructed), y-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
         endif

         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sxtot   , nf90_double, UNC_LOC_S, 'sxtot'  , '', 'Total sediment transport in flow cell center (reconstructed), x-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sytot   , nf90_double, UNC_LOC_S, 'sytot'  , '', 'Total sediment transport in flow cell center (reconstructed), y-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
         
         ! Time averaged sediment transport values
         if (stmpar%morpar%moroutput%cumavg) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbxcum   , nf90_double, UNC_LOC_S, 'sbxcum'  , '', 'Time-averaged bed load transport, x-component',       transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbycum   , nf90_double, UNC_LOC_S, 'sbycum'  , '', 'Time-averaged bed load transport, y-component',       transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ssxcum   , nf90_double, UNC_LOC_S, 'ssxcum'  , '', 'Time-averaged suspended load transport, x-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ssycum   , nf90_double, UNC_LOC_S, 'ssycum'  , '', 'Time-averaged suspended load transport, y-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
         else
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbxcum   , nf90_double, UNC_LOC_S, 'sbxcum'  , '', 'Time-averaged bed load transport, x-component',       transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbycum   , nf90_double, UNC_LOC_S, 'sbycum'  , '', 'Time-averaged bed load transport, y-component',       transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ssxcum   , nf90_double, UNC_LOC_S, 'ssxcum'  , '', 'Time-averaged suspended load transport, x-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ssycum   , nf90_double, UNC_LOC_S, 'ssycum'  , '', 'Time-averaged suspended load transport, y-component', transpunit, dimids = (/ -2, mapids%id_tsp%id_sedtotdim /))
         endif
         
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_mor_bl   , nf90_double, UNC_LOC_S, 'mor_bl'  , '', 'Time-varying bottom level in flow cell center', 'm', dimids = (/ -2, -1 /))

         select case (stmpar%morlyr%settings%iunderlyr)
            case (1)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_bodsed  , nf90_double, UNC_LOC_S, 'bodsed'  , '', 'Available sediment mass in the bed in flow cell center', 'kg m-2', dimids = (/ mapids%id_tsp%id_sedtotdim, -2, -1 /))
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dpsed   , nf90_double, UNC_LOC_S, 'dpsed'  , '', 'Sediment thickness in the bed in flow cell center', 'm', dimids = (/ -2, -1 /))
            case (2)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_msed   , nf90_double, UNC_LOC_S, 'msed'  , '', 'Available sediment mass in a layer of the bed in flow cell center', 'kg m-2', dimids = (/ mapids%id_tsp%id_sedtotdim, mapids%id_tsp%id_nlyrdim, -2, -1 /))
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_thlyr  , nf90_double, UNC_LOC_S, 'thlyr'  , '', 'Thickness of a layer of the bed in flow cell center', 'm', dimids = (/ mapids%id_tsp%id_nlyrdim, -2, -1 /))
               !
               if (stmpar%morlyr%settings%iporosity>0) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_poros  , nf90_double, UNC_LOC_S, 'poros'  , '', 'Porosity of a layer of the bed in flow cell center', '-', dimids = (/ mapids%id_tsp%id_nlyrdim, -2, -1 /))
               endif
               !
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_lyrfrac  , nf90_double, UNC_LOC_S, 'lyrfrac'  , '', 'Volume fraction in a layer of the bed in flow cell center', '-', dimids = (/ -2, mapids%id_tsp%id_nlyrdim, mapids%id_tsp%id_sedtotdim, -1 /))
         end select
         !
         if (stmpar%morpar%moroutput%taurat) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_taurat  , nf90_double, UNC_LOC_S, 'taurat'  , '', 'Excess bed shear ratio', '-', dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
         endif
         if (stmpar%morpar%moroutput%dm) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dm  , nf90_double, UNC_LOC_S, 'dm'  , '', 'Arithmetic mean sediment diameter', 'm', dimids = (/ -2, -1 /))
         endif
         if (stmpar%morpar%moroutput%dg) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dg  , nf90_double, UNC_LOC_S, 'dg'  , '', 'Geometric mean sediment diameter', 'm', dimids = (/ -2, -1 /))
         endif
         if (stmpar%morpar%moroutput%dgsd) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dgsd  , nf90_double, UNC_LOC_S, 'dgsd'  , '', 'Geometric standard deviation of particle size mix', 'm', dimids = (/ -2, -1 /))
         endif
         if (stmpar%morpar%moroutput%percentiles) then
            do l = 1, stmpar%morpar%nxx
               write(dxname,'(A,I2.2)') 'DXX',l
               write(dxdescr,'(A,F4.1,A)') 'Sediment diameter percentile '    , stmpar%morpar%xx(l)*100d0,' %'
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dxx(l,:)  , nf90_double, UNC_LOC_S, dxname  , '', dxdescr, 'm', dimids = (/ -2, -1 /))
            enddo
         endif
         if (stmpar%morpar%moroutput%frac) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_frac  , nf90_double, UNC_LOC_S, 'frac'  , '', 'Availability fraction in top layer', '-', dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
         endif
         if (stmpar%morpar%moroutput%mudfrac) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_mudfrac  , nf90_double, UNC_LOC_S, 'mudfrac'  , '', 'Mud fraction in top layer', '-', dimids = (/ -2, -1 /))
         endif
         if (stmpar%morpar%moroutput%sandfrac) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sandfrac  , nf90_double, UNC_LOC_S, 'sandfrac'  , '', 'Sand fraction in top layer', '-', dimids = (/ -2, -1 /))
         endif
         if (stmpar%morpar%moroutput%fixfac) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_fixfac  , nf90_double, UNC_LOC_S, 'fixfac'  , '', 'Reduction factor due to limited sediment thickness', '-', dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
         endif
         if (stmpar%morpar%moroutput%hidexp) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_hidexp  , nf90_double, UNC_LOC_S, 'hidexp'  , '', 'Hiding and exposure factor', '-', dimids = (/ -2, mapids%id_tsp%id_sedtotdim, -1 /))
         endif
         !
         if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0) then   
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_mfluff  , nf90_double, UNC_LOC_S, 'mfluff'  , '', 'Sediment mass in fluff layer', 'kg m-2', dimids = (/ -2, mapids%id_tsp%id_sedsusdim, -1 /))
         end if
         !
         if (ndx1d > 0) then
            !ierr = nf90_def_dim(mapids%ncid, 'nmesh1d_crs_maxdim', jmax,    mapids%id_jmax)
            ierr = nf90_def_var(mapids%ncid, trim(mesh1dname)//'_mor_crs_z', nf90_double, (/ mapids%id_tsp%id_jmax, mapids%id_tsp%meshids1d%dimids(mdim_node), mapids%id_tsp%id_timedim /), mapids%id_tsp%id_flowelemcrsz(1))
            ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_flowelemcrsz(1), 'long_name','time-varying cross-section points level')
            ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_flowelemcrsz(1), 'unit', 'm')
            ierr = nf90_def_var(mapids%ncid, trim(mesh1dname)//'_mor_crs_n', nf90_double, (/ mapids%id_tsp%id_jmax, mapids%id_tsp%meshids1d%dimids(mdim_node), mapids%id_tsp%id_timedim /), mapids%id_tsp%id_flowelemcrsn(1))
            ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_flowelemcrsn(1), 'long_name','time-varying cross-section points half width')
            ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_flowelemcrsn(1), 'unit', 'm')
         endif
      endif
      !
      ! BEDFORMS
      !
      if (bfmpar%lfbedfrmout) then
         if (bfmpar%lfbedfrm) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_duneheight, nf90_double, UNC_LOC_S, 'duneheight'  , '', 'Time-varying dune height in flow cell center', 'm', dimids = (/ -2, -1 /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dunelength, nf90_double, UNC_LOC_S, 'dunelength'  , '', 'Time-varying dune length in flow cell center', 'm', dimids = (/ -2, -1 /))
         end if
         !
         if (bfmpar%lfbedfrmrou) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ksr,  nf90_double, UNC_LOC_S, 'ksr'  , '', 'Ripple roughness height in flow cell center', 'm', dimids = (/ -2, -1 /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ksmr, nf90_double, UNC_LOC_S, 'ksmr'  , '', 'Megaripple roughness height in flow cell center', 'm', dimids = (/ -2, -1 /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ksd,  nf90_double, UNC_LOC_S, 'ksd'  , '', 'Dune roughness height in flow cell center', 'm', dimids = (/ -2, -1 /))
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ks,   nf90_double, UNC_LOC_S, 'ks'  , '', 'Bedform roughness height in flow cell center', 'm', dimids = (/ -2, -1 /))
         end if
      end if

      ! Sediment transport (via own built-in sed)
      if (jamapsed > 0 .and. jased > 0 .and. .not. stm_included) then
         ierr = nf90_def_dim(mapids%ncid, 'nFrac', mxgr, mapids%id_tsp%id_maxfracdim)
         if( .not. allocated(mapids%id_sed) ) then
            allocate( mapids%id_sed(MAX_ID_VAR,mxgr), mapids%id_ero(MAX_ID_VAR,mxgr) )
            mapids%id_sed = -1
            mapids%id_ero = -1
         endif
         do j = 1,mxgr
            write(str,"(I4)") j
            str = adjustl( str )
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sed(:,j), nf90_double, UNC_LOC_S, 'sed'//trim(str), 'sediment_concentration'      , 'Sediment concentration'   , 'kg m-3') !, dimids = (/ mapids%id_maxfracdim, -2, -1 /))
         enddo
         if (jaceneqtr == 1) then ! Bed level in cell center
            do j = 1,mxgr
               write(str,"(I4)") j
               str = adjustl( str )
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ero(:,j), nf90_double, UNC_LOC_S, 'ero'//trim(str), 'layer_thickness_per_fraction', 'Erodable layer thickness per size fraction in flow element centers'   , 'm')
            enddo
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bl,  nf90_double, UNC_LOC_S, 'flowelem_bedlevel_bl', ''   , 'Flow element center bedlevel (bl)'                             , 'm')
         else                     ! Bed level at cell corner
            do j = 1,mxgr
               write(str,"(I4)") j
               str = adjustl( str )
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ero(:,j), nf90_double, UNC_LOC_CN, 'ero'//trim(str), 'layer_thickness_per_fraction', 'Erodable layer thickness per size fraction in flow element corners'   , 'm')
            enddo
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_zk , nf90_double, UNC_LOC_CN,'netnode_bedlevel_zk', ''      , 'Flow element corner bedlevel (zk)'                          , 'm')
         end if
      end if

      if (jamapwav>0) then     
      ! JRE waves
      if (jawave .eq. 4) then
         ierr = nf90_def_dim(mapids%ncid, 'ntheta', ntheta, mapids%id_tsp%id_ntheta)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_E        , nf90_double, UNC_LOC_S, 'E'        , 'sea_surface_bulk_wave_energy'         , 'Wave energy per square meter'                     , 'J m-2') ! not CF
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_R        , nf90_double, UNC_LOC_S, 'R'        , 'sea_surface_bulk_roller_energy'       , 'Roller energy per square meter'                   , 'J m-2') ! not CF
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_DR       , nf90_double, UNC_LOC_S, 'DR'       , 'sea_surface_bulk_roller_dissipation'  , 'Roller energy dissipation per square meter'       , 'W m-2') ! not CF
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_D        , nf90_double, UNC_LOC_S, 'D'        , 'sea_surface_wave_breaking_dissipation', 'Wave breaking energy dissipation per square meter', 'W m-2') ! not CF
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Df        , nf90_double, UNC_LOC_S, 'Df'        , 'sea_surface_wave_bottom_dissipation', 'Wave bottom energy dissipation per square meter', 'W m-2') ! not CF

         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Sxx      , nf90_double, UNC_LOC_S, 'Sxx'      , ''         , 'Radiation stress, x-component'          , 'N m-2') ! not CF
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Syy      , nf90_double, UNC_LOC_S, 'Syy'      , ''        , 'Radiation stress, y-component'          , 'N m-2') ! not CF
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Sxy      , nf90_double, UNC_LOC_S, 'Sxy'      , 'sea_surface_wave_radiation_stress_NE'         , 'Radiation stress, xy-component'           , 'N m-2') ! not CF
         
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cwav     , nf90_double, UNC_LOC_S, 'cwav'     , 'sea_surface_wave_phase_celerity'      , 'Sea_surface_wave_phase_celerity'                  , 'm s-1') ! not CF
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cgwav    , nf90_double, UNC_LOC_S, 'cgwav'    , 'sea_surface_wave_group_celerity'      , 'Sea_surface_wave_group_celerity'                  , 'm s-1') ! not CF
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sigmwav  , nf90_double, UNC_LOC_S, 'sigmwav'  , 'sea_surface_wave_mean_frequency'      , 'Sea_surface_wave_mean_frequency'                  , 'rad s-1') ! not CF
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_kwav     , nf90_double, UNC_LOC_S, 'kwav'     , 'sea_surface_wave_wavenumber'          , 'Sea_surface_wave_wavenumber'                      , 'rad m-1') ! not CF
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nwav     , nf90_double, UNC_LOC_S, 'nwav'     , 'sea_surface_wave_cg_over_c'           , 'Sea_surface_wave_ratio_group_phase_speed'         , '-') ! not CF
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ctheta   , nf90_double, UNC_LOC_S, 'ctheta'   , 'sea_surface_wave_refraction_celerity' , 'Sea_surface_wave_refraction_celerity'             , 'rad s-1', dimids = (/ mapids%id_tsp%id_ntheta, -2,  -1 /)) ! not CF
         if (windmodel.eq.0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_l1       , nf90_double, UNC_LOC_S, 'L1'       , 'sea_surface_wave_wavelength'          , 'Sea_surface_wave_wavelength'                      , 'm'      ) ! not CF
         elseif ( (windmodel .eq. 1) .and. (jawsource .eq. 1) ) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_SwE  , nf90_double, UNC_LOC_S, 'SwE'  , 'source_term_wind_on_E'      , 'wind source term on wave energy'                  , 'J m-2 s-1') ! not CF        
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_SwT  , nf90_double, UNC_LOC_S, 'SwT'  , 'source_term_wind_on_T'      , 'wind source term on wave period'                  , 's s-1') ! not CF        
         endif
         if (jamombal==1) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dsdx      , nf90_double, UNC_LOC_S, 'dsdx'      , ' ' , 'Water level gradient, x-component'          , 'm m-1') 
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dsdy      , nf90_double, UNC_LOC_S, 'dsdy'      , ' ' , 'Water level gradient, y-component'          , 'm m-1') 
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ducxdx    , nf90_double, UNC_LOC_S, 'ducxdx'    , ' ' , 'Gradient of X component CC velocity, x-component'          , 'm s-1 m-1') 
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ducydx    , nf90_double, UNC_LOC_S, 'ducydx'    , ' ' , 'Gradient of Y component CC velocity, x-component'          , 'm s-1 m-1') 
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ducxdy    , nf90_double, UNC_LOC_S, 'ducxdy'    , ' ' , 'Gradient of X component CC velocity, y-component'          , 'm s-1 m-1') 
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ducydy    , nf90_double, UNC_LOC_S, 'ducydy'    , ' ' , 'Gradient of Y component CC velocity, y-component'          , 'm s-1 m-1') 
         end if
      end if
      
      if (jawave==3) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxwav       , nf90_double, UNC_LOC_S, 'sxwav' , 'sea_surface_x_wave_force_surface', 'Surface layer wave forcing term, x-component'   , 'N m-2') ! not CF    
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sywav       , nf90_double, UNC_LOC_S, 'sywav' , 'sea_surface_y_wave_force_surface', 'Surface layer wave forcing term, y-component'   , 'N m-2') ! not CF    
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxbwav      , nf90_double, UNC_LOC_S, 'sxbwav', 'sea_surface_x_wave_force_bottom' , 'Bottom layer wave forcing term, x-component'    , 'N m-2') ! not CF    
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sybwav      , nf90_double, UNC_LOC_S, 'sybwav', 'sea_surface_y_wave_force_bottom' , 'Bottom layer wave forcing term, y-component'    , 'N m-2') ! not CF    
      end if
      
      if (jawave .gt. 0) then
         if (jamapsigwav==0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav        , nf90_double, UNC_LOC_S, 'hwav'         , 'sea_surface_wave_rms_height'          , 'RMS wave height'          , 'm'    ) ! not CF
         else
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav        , nf90_double, UNC_LOC_S, 'hwav'         , 'sea_surface_wave_significant_wave_height'          , 'Significant wave height'          , 'm'    )
         endif
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uorb     , nf90_double, UNC_LOC_S, 'uorb'            , 'sea_surface_wave_orbital_velocity'    , 'Wave orbital velocity'    , 'm s-1') ! not CF
      endif
      if (jawave .gt. 2) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustokes      , nf90_double, iLocS, 'ust_cc'     , 'sea_surface_x_stokes_drift'        , 'Stokes drift, x-component'   , 'm s-1') ! not CF
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vstokes      , nf90_double, iLocS, 'vst_cc'     , 'sea_surface_y_stokes_drift'       , 'Stokes drift, y-component'    , 'm s-1') ! not CF
         if (jawave/=5) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fx       , nf90_double, UNC_LOC_S, 'Fx'              , 'sea_surface_x_wave_force'          , 'Wave force, x-component'     , 'N') ! not CF
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fy       , nf90_double, UNC_LOC_S, 'Fy'              , 'sea_surface_y_wave_force'         , 'Wave force, y-component'      , 'N') ! not CF 
            !debug
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fxlink, nf90_double, UNC_LOC_U, 'wavfu', '', 'Wave force at velocity point, n-component', 'N')! not CF 
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fylink, nf90_double, UNC_LOC_U, 'wavfv', '', 'Wave force at velocity point, t-component', 'N')! not CF
         endif
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustokeslink      , nf90_double, iLocU, 'ustokes'     , ''        , 'Stokes drift, n-component'   , 'm s-1') ! not CF
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vstokeslink      , nf90_double, iLocU, 'vstokes'     , ''        , 'Stokes drift, t-component'   , 'm s-1') ! not CF
         
         !\debug
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_thetamean, nf90_double, UNC_LOC_S, 'thetamean'       , 'sea_surface_wave_from_direction'      , 'Wave from direction'      , 'deg from N') ! not CF
      end if
 
      end if
      !
      ! Trachytope roughnesses on NET links
      if (jamaptrachy > 0 .and. jatrt == 1) then

         if (ifrctypuni == 0) then 
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nf90_double, UNC_LOC_L, 'cftrt',   '', 'Chezy roughness from trachytopes', '')
            ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', 'm0.5s-1')
         else if (ifrctypuni == 1) then 
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nf90_double, UNC_LOC_L, 'cftrt',   '', 'Manning roughness from trachytopes', '')
            ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', 'sm-0.333')
         else if ((ifrctypuni == 2) .or. (ifrctypuni == 3)) then 
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nf90_double, UNC_LOC_L, 'cftrt',   '', 'White-Colebrook roughness from trachytopes', '')
            ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', 'm')
         else
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nf90_double, UNC_LOC_L, 'cftrt',   '', 'Roughness from trachytopes', '')
            ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', ' ')
         end if
      end if 

      if (jamapcali > 0 .and. jacali == 1) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cfcl, nf90_double, UNC_LOC_L, 'cfcl',   '', 'Calibration factor for roughness', '')
         ierr = unc_put_att(mapids%ncid, mapids%id_cfcl, 'non_si_units', 'm0.5s-1')
      endif 
      
      ! Secondary Flow ! TODO: AvD: add secondary flow
           !if (jasecflow == 1) then
           !    ierr = nf90_def_var(imapfile, 'rsi' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_rsi)
           !    ierr = nf90_put_att(imapfile, id_rsi,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_rsi,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_rsi,  'long_name'    , 'inverse streamline curvature in flow element center')
           !    ierr = nf90_put_att(imapfile, id_rsi,  'units'        , 'm-1')
           !    ierr = nf90_def_var(imapfile, 'rsiexact' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_rsiexact)
           !    ierr = nf90_put_att(imapfile, id_rsiexact,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_rsiexact,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_rsiexact,  'long_name'    , 'inverse streamline curvature in flow element center')
           !    ierr = nf90_put_att(imapfile, id_rsiexact,  'units'        , 'm-1')
           !    ierr = nf90_def_var(imapfile, 'dsdx' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dsdx)
           !    ierr = nf90_put_att(imapfile, id_dsdx,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_dsdx,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_dsdx,  'long_name'    , 'water level gradient in x direction')
           !    ierr = nf90_put_att(imapfile, id_dsdx,  'units'        , 's-1')
           !    ierr = nf90_def_var(imapfile, 'dsdy' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dsdy)
           !    ierr = nf90_put_att(imapfile, id_dsdy,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_dsdy,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_dsdy,  'long_name'    , 'water level gradient in y direction')
           !    ierr = nf90_put_att(imapfile, id_dsdy,  'units'        , 's-1')        
           !    ierr = nf90_def_var(imapfile, 'dudx' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dudx)
           !    ierr = nf90_put_att(imapfile, id_dudx,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_dudx,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_dudx,  'long_name'    , 'x-velocity gradient in x direction')
           !    ierr = nf90_put_att(imapfile, id_dudx,  'units'        , 's-1')
           !    ierr = nf90_def_var(imapfile, 'dudy' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dudy)
           !    ierr = nf90_put_att(imapfile, id_dudy,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_dudy,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_dudy,  'long_name'    , 'x-velocity gradient in y direction')
           !    ierr = nf90_put_att(imapfile, id_dudy,  'units'        , 's-1')        
           !    ierr = nf90_def_var(imapfile, 'dvdx' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dvdx)
           !    ierr = nf90_put_att(imapfile, id_dvdx,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_dvdx,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_dvdx,  'long_name'    , 'y-velocity gradient in x direction')
           !    ierr = nf90_put_att(imapfile, id_dvdx,  'units'        , 's-1')
           !    ierr = nf90_def_var(imapfile, 'dvdy' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dvdy)
           !    ierr = nf90_put_att(imapfile, id_dvdy,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           !    ierr = nf90_put_att(imapfile, id_dvdy,  'standard_name', '')
           !    ierr = nf90_put_att(imapfile, id_dvdy,  'long_name'    , 'y-velocity gradient in y direction')
           !    ierr = nf90_put_att(imapfile, id_dvdy,  'units'        , 's-1')        
           !end if
      
      if ( janudge.gt.0 .and. jamapNudge.gt.0 ) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_time, nf90_double, UNC_LOC_S, 'Tnudge', 'nudging_time', 'Nudging relaxing time', 's', is_timedep=0)
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_tem, nf90_double, UNC_LOC_S3D, 'nudge_tem', 'nudging_tem', 'Nudging temperature', 'degC')
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_sal, nf90_double, UNC_LOC_S3D, 'nudge_sal', 'nudging_sal', 'Nudging salinity', '1e-3')
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_Dtem, nf90_double, UNC_LOC_S3D, 'nudge_Dtem', 'nudging_Dtem', 'Difference of nudging temperature with temperature', 'degC')
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_Dsal, nf90_double, UNC_LOC_S3D, 'nudge_Dsal', 'nudging_Dsal', 'Difference of nudging salinity with salinity', '1e-3')
         
      end if
      
      if ( japart.eq.1 .and. jatracer.eq.1 .and. kmx.gt.0 ) then
         ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_depth_averaged_particle_concentration, nf90_double, UNC_LOC_S, 'depth_averaged_particle_concentration', 'depth_averaged_particle_concentration', 'depth-averaged particle concentration', 'm-3')
      end if

      ! for 1D only, urban
      if (ndxi-ndx2d>0) then
         if (jamapTimeWetOnGround > 0) then ! cumulative time when water is above ground level
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_timewetground, nf90_double, UNC_LOC_S, 'time_water_on_ground', '', 'Cumulative time water above ground level', 's', which_meshdim = 1)
         end if
         if (jamapFreeboard > 0) then ! freeboard
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_freeboard, nf90_double, UNC_LOC_S, 'freeboard', '', 'Freeboard', 'm', which_meshdim = 1)
         end if
         if (jamapDepthOnGround > 0) then ! waterdpth that is above ground level
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hs_on_ground, nf90_double, UNC_LOC_S, 'waterdepth_on_ground', '', 'Waterdepth above ground level', 'm', which_meshdim = 1)
         end if
         if (jamapVolOnGround > 0) then ! volume that is above ground level
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vol_on_ground, nf90_double, UNC_LOC_S, 'volume_on_ground', '', 'Volume above ground level', 'm3', which_meshdim = 1)
         end if
         if (jamapTotalInflow1d2d > 0) then ! total 1d2d net inflow
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qCur1d2d, nf90_double, UNC_LOC_S, 'current_total_net_inflow_1d2d', '', 'Current total net inflow via all connected 1d2d links at each 1D node', 'm3 s-1', which_meshdim = 1)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vTot1d2d, nf90_double, UNC_LOC_S, 'cumulative_total_net_inflow_1d2d', '', 'Cumulative total net inflow via all connected 1d2d links at each 1D node', 'm3', which_meshdim = 1)
         end if
         if (jamapTotalInflowLat > 0) then ! total lateral net inflow
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qCurLat, nf90_double, UNC_LOC_S, 'current_total_net_inflow_lateral', '', 'Current total net inflow via all laterals at each 1D node', 'm3 s-1', which_meshdim = 1)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vTotLat, nf90_double, UNC_LOC_S, 'cumulative_total_net_inflow_lateral', '', 'Cumulative total net inflow via all laterals at each 1D node', 'm3', which_meshdim = 1)
         end if
      end if
      if (lnx1d > 0) then
         if (jamapS1Gradient > 0) then ! water level gradient
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s1Gradient, nf90_double, UNC_LOC_U, 'water_level_gradient', '', 'Water level gradient at each 1D flow link', '1', which_meshdim = 1)
         end if
      end if
      ierr = nf90_enddef(mapids%ncid)
      
      if ( janudge.gt.0 .and. jamapnudge.gt.0 ) then
!        output static nudging time
         workx = 0d0
         do k=1,Ndx
            if ( nudge_rate(k).gt.0d0 ) then
               workx(k) = 1d0/nudge_rate(k)
            end if
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_time, UNC_LOC_S, workx)
      end if

      if (nomba > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mba(:), UNC_LOC_S, mbadef)
      end if
      
      if (jased==4 .and. stm_included) then
         do j=1,stmpar%lsedtot
            ierr = nf90_put_var(mapids%ncid,mapids%id_frac_name,trim(stmpar%sedpar%namsed(j)),(/ 1, j /),(/ len(trim(stmpar%sedpar%namsed(j))), 1 /))  ! only write once
         enddo
      endif

   endif
   ! End of writing time-independent flow geometry data.
   
   ! -- Start data writing (flow data) ------------------------

   mapids%id_tsp%idx_curtime = mapids%id_tsp%idx_curtime+1      ! Increment time dimension index  
   itim               = mapids%id_tsp%idx_curtime

   ! Time
   ierr = nf90_put_var(mapids%ncid, mapids%id_time    , tim, (/ itim /))
   ierr = nf90_put_var(mapids%ncid, mapids%id_timestep, dts, (/ itim /))

   if (jamapnumlimdt > 0) then
      ! ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_numlimdt, UNC_LOC_S, numlimdt) ! TODO: AvD: integer version of this routine
      call realloc(numlimdtdbl, ndxndxi, keepExisting=.false.)
      numlimdtdbl = dble(numlimdt) ! To prevent stack overflow. TODO: remove once integer version is available.
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_numlimdt, UNC_LOC_S, numlimdtdbl)
      deallocate(numlimdtdbl)
   end if

   ! Water level
   if (jamaps1 == 1) then
      !ierr = nf90_inq_varid(mapids%ncid, 'mesh2d'//'_s1', mapids%id_s1(2))
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s1, UNC_LOC_S, s1) 
      !ierr = nf90_inq_varid(mapids%ncid, 'mesh2d'//'_waterdepth', mapids%id_hs(2))
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hs, UNC_LOC_S, hs)
   end if
   
   if (jamaps0 == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s0, UNC_LOC_S, s0)
   end if

   ! Volumes
   if (jamapvol1 == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vol1, ilocS, vol1) 
   end if

   ! Flow areas
   if (jamapau == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_au, iLocU, au) 
   end if

   ! Velocities
   if (jamapu1 == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_u1, iLocU, u1, 0d0)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hu, UNC_LOC_U, hu)
   end if
   if (jamapu0 == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_u0, iLocU, u0, 0d0)
   end if
   if (jamapdtcell == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dtcell, UNC_LOC_S, dtcell)
   endif

   ! TODO: AvD below: workx/y needs to be reset with miss/0 values before using.
   if (jamapucvec == 1 .or. jamapucmag == 1 .or. jamapucqvec == 1) then
      call getucxucyeulmag(ndkx, workx, worky, ucmag, jaeulervel, jamapucmag)

      if (jamapucvec == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucx, iLocS, workx)     ! JRE langrangian or eulerian, see write_map_filepointer
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucy, iLocS, worky)
      end if

      if (jamapucmag == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmag, iLocS, ucmag)
      end if

      if (kmx > 0) then
         call reconstructucz(0)
         if (jamapucvec == 1) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucz, UNC_LOC_S3D, ucz)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxa, UNC_LOC_S, ucxq)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucya, UNC_LOC_S, ucyq)
         end if

         if (jamapucmag == 1) then
            call realloc(work1d, ndkx, keepExisting = .false.)
            do k=1,size(ucxq)
               work1d(k) = sqrt(ucxq(k)**2 + ucyq(k)**2) ! TODO: this does not include vertical/w-component now.
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmaga, UNC_LOC_S, work1d)
         end if
      end if

      if (jamapucqvec == 1) then
         ! TODO: AvD/MN: consider removing entire loop and simply unc_put_var_map( ..., ucqx,..)
         if (kmx > 0) then
            do kk = 1,ndx
                call getkbotktop(kk,kb,kt)
                do k = kb,kt
                    workx(k) = ucxq(k)
                    worky(k) = ucyq(k) 
                enddo
            enddo
         else
            do kk = 1,ndx
                workx(kk) = ucxq(kk)
                worky(kk) = ucyq(kk)
            enddo
         endif
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxq, iLocS, workx)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucyq, iLocS, worky)
      end if

   end if
   if (kmx > 0) then
      if(jamapww1 > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ww1, UNC_LOC_W, ww1)
      end if
      if(jamaprho > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rho, UNC_LOC_S3D, rho)
      end if
   end if

   if (jamapq1 == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1, iLocU, q1, 0d0)
   end if

   if (jamapq1main == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1main, iLocU, q1_main, 0d0)
   end if
   
   ! TIDAL TURBINES: Insert equivalent of wrturbine_cnst and wrturbine_time here

   if (kmx > 0) then
      if (jamapviu > 0) then
         ! For all flowlinks and layers add user defined part (viusp(LL) or vicouv) to modeled part (viu(LL)).
         ! Values for inactive layers are set to missing in function unc_put_var_map.
         call realloc(work1d, lnkx, keepExisting = .false.)
         do LL = 1,lnx
            if (javiusp == 1) then ! If horizontal eddy viscosity is spatially varying.
               vicc = viusp(LL)
            else
               vicc = vicouv
            end if
            call getLbotLtopmax(LL, Lb, Lt)
            do L = Lb,Lt
               work1d(L) = viu(L) + vicc
            end do
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_viu, iLocU, work1d)
      end if

      if (jamapdiu > 0) then
         ! For all flowlinks and layers add user defined part (diusp(LL) or dicouv) to modeled part (0.7*viu(LL)).
         ! Values for inactive layers are set to missing in function unc_put_var_map.
         call realloc(work1d, lnkx, keepExisting = .false.)
         do LL = 1,lnx
            if (jadiusp == 1) then ! If horizontal eddy viscosity is spatially varying.
               dicc = diusp(LL)
            else
               dicc = dicouv
            end if
            call getLbotLtopmax(LL, Lb, Lt)
            do L = Lb,Lt
               work1d(L) = viu(L) * 0.7 + dicc
            end do
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_diu, iLocU, work1d)
      end if
   endif

   if (kmx == 0) then
      if (jamapviu > 0) then
         ! For all flowlinks add user defined part (viusp(LL) or vicouv) to modeled part (viu(LL)).
         call realloc(work1d, lnx, keepExisting = .false.)
         do LL = 1,lnx
            if (javiusp == 1) then ! If horizontal eddy viscosity is spatially varying.
               vicc = viusp(LL)
            else
               vicc = vicouv
            end if
            work1d(LL) = viu(LL) + vicc
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_viu, iLocU, work1d)
      end if

      if (jamapdiu > 0) then
         ! For all flowlinks add user defined part (diusp(LL) or dicouv) to modeled part (0.7*viu(LL)).
         call realloc(work1d, lnx, keepExisting = .false.)
         do LL = 1,lnx
            if (jadiusp == 1) then ! If horizontal eddy viscosity is spatially varying.
               dicc = diusp(LL)
            else
               dicc = dicouv
            end if
            work1d(LL) = viu(LL) * 0.7 + dicc
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_diu, iLocU, work1d)
      end if
   endif

   if (allocated(work1d)) deallocate(work1d)

   ! Tau current and Chezy
   if (jamaptaucurrent > 0 .or. jamapchezy > 0) then
      if (jawave < 3) then   ! Else, get taus from subroutine tauwave (taus = taucur + tauwave). Bas; Mind for jawind!
         call gettaus(1)       ! Update taus and czs 
      else if (jamapchezy > 0) then    
         call gettaus(2)       ! Only update czs 
      end if
   end if
   if (jamaptaucurrent > 0) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_taus, UNC_LOC_S, taus)
   end if    
   if (jamapchezy > 0) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_czs , UNC_LOC_S, czs)
   end if

   ! Salinity
   if (jasal > 0 .and. jamapsal > 0) then 
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sa1, iLocS, sa1)
   end if

   ! Temperature
   if (jatem > 0 .and. jamaptem > 0) then 
      do k = 1,ndkx
         tem1(k) = constituents(itemp, k )
      enddo   
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tem1, iLocS, tem1)
   endif
   
   if (jasecflow > 0 .and. jamapspir > 0) then
      if (kmx == 0) then 
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_spircrv, UNC_LOC_S, spircrv)
      endif
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_spirint, UNC_LOC_S, spirint)
   endif

   ! Constituents
   
!   The following is not stack-safe:
!   if (jamapconst > 0 .and. ITRA1 > 0) then
!      do j=ITRA1,ITRAN
!         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_const(:,j), iLocS, constituents(j,:))
!      end do
!   end if
   
!   The following is (almost) copied from unc_wite_map_filepointer
    if (jamapconst > 0 .and. ITRA1 > 0) then
    
       do j=ITRA1,ITRAN
          workx = DMISS ! For proper fill values in z-model runs.
          if ( kmx>0 ) then
!            3D
             do kk=1,ndxndxi
                call getkbotktop(kk,kb,kt)
                do k = kb,kt
                   workx(k) = constituents(j,k)
                enddo
             end do
!             ierr = nf90_put_var(imapfile, mapids%id_const(:,j), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
             ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_const(:,j), UNC_LOC_S3D, workx)
             !   if ( ierr.ne.0 ) exit  ! probably newly added tracer in the GUI
          else
             do kk=1,NdxNdxi
                workx(kk) = constituents(j,kk)
             end do
!             ierr = nf90_put_var(imapfile, id_const(iid,j), dum, (/ 1, itim /), (/ NdxNdxi, 1 /) )
             ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_const(:,j), UNC_LOC_S, workx)
          end if
       end do
    end if

    ! water quality bottom variables
    if (numwqbots > 0) then
       do j=1,numwqbots
          do k=1,Ndxi
             workx(k) = wqbot(j,k)
          end do
          ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqb(:,j), UNC_LOC_S, workx(1:ndxi))
       end do
    end if

    ! WAQ output
    if (jawaqproc > 0) then
       do j=1,noout_map
          if (outvar(j)>0)then
             workx = DMISS ! For proper fill values in z-model runs.
             if ( kmx>0 ) then
!               3D
                do kk=1,ndxndxi
                   call getkbotktop(kk,kb,kt)
                   do k = kb,kt
                      workx(k) = waqoutputs(j,k-kbx+1)
                   enddo
                end do
                ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_waq(:,j), UNC_LOC_S3D, workx)
             else
!               2D                
                do kk=1,NdxNdxi
                   workx(kk) = waqoutputs(j,kk)
                end do
                ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_waq(:,j), UNC_LOC_S, workx)
             end if
          end if
       end do
       do j=1,noout_statt
          jj = noout_user + j
          if (outvar(jj)>0)then
             workx = DMISS ! For proper fill values in z-model runs.
             if ( kmx>0 ) then
!               3D
                do kk=1,ndxndxi
                   call getkbotktop(kk,kb,kt)
                   do k = kb,kt
                      workx(k) = waqoutputs(jj,k-kbx+1)
                   enddo
                end do
                ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqst(:,j), UNC_LOC_S3D, workx)
             else
!               2D                
                do kk=1,NdxNdxi
                   workx(kk) = waqoutputs(jj,kk)
                end do
                ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqst(:,j), UNC_LOC_S, workx)
             end if
          end if
       end do
       if (comparereal(tim, ti_mape, eps10) == 0) then
          do j=1,noout_state
             jj = noout_user + noout_statt + j
             if (outvar(jj)>0)then
                workx = DMISS ! For proper fill values in z-model runs.
                if ( kmx>0 ) then
!                  3D
                   do kk=1,ndxndxi
                      call getkbotktop(kk,kb,kt)
                      do k = kb,kt
                         workx(k) = waqoutputs(jj,k-kbx+1)
                      enddo
                   end do
                   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqse(:,j), UNC_LOC_S3D, workx)
                else
!                  2D                
                   do kk=1,NdxNdxi
                      workx(kk) = waqoutputs(jj,kk)
                   end do
                   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqse(:,j), UNC_LOC_S, workx)
                end if
             end if
          end do
      end if
    end if

   ! Turbulence.
   if (jamaptur > 0 .and. kmx > 0) then
      if (iturbulencemodel >= 3) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_turkin1, UNC_LOC_WU, turkin1)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vicwwu,  UNC_LOC_WU, vicwwu)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tureps1, UNC_LOC_WU, tureps1)
      end if
   end if

   !
   ! Sediment transport (via morphology module)
if (jamapsed > 0 .and. jased > 0 .and. stm_included) then
   !
   ! TODO: AvD: support kmax in put routine
   do L=1, lnx
      if (frcu(L)>0) then
         call getczz0(hu(L), frcu(L), ifrcutp(L), ddum, z0ucur(L))
      else
         call getczz0(hu(L), frcuni, ifrctypuni, ddum, z0ucur(L))
      end if
   enddo
   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_z0c, UNC_LOC_U, z0ucur,0d0)
   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_z0r, UNC_LOC_U, z0urou,0d0)    ! already available

   if (stmpar%lsedsus > 0) then
      if (kmx>0) then
         call realloc(toutputx, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
         !
         ! convert kmxsed to kmx administration
         if (itim==1) then
            toutputx = 1      ! set to bottom layer on first time step
         else
            do l=1,stmpar%lsedsus
               do k=1,ndx
                  kk = sedtra%kmxsed(k,l)
                  call getkbotktop(k,kb,kt)
                  found=0
                  do kkk=kb,kt
                     found=found+1
                     if (kkk==kk) exit   ! meh...
                  enddo
                  toutputx(k,l) = found
               enddo
            enddo
         endif
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_kmxsed, UNC_LOC_S, toutputx)
      endif
      !
      call realloc(toutputx, (/lnx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
      call realloc(toutputy, (/lnx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
      do l = 1, stmpar%lsedsus
         select case(stmpar%morpar%moroutput%transptype)
         case (0)
            rhol = 1d0
         case (1)
            rhol = stmpar%sedpar%cdryb(sedtot2sedsus(l))
         case (2)
            rhol = stmpar%sedpar%rhosol(sedtot2sedsus(l))
         end select
         toutputx(:,l) = sedtra%e_ssn(:,l)/rhol
         toutputy(:,l) = sedtra%e_sst(:,l)/rhol
      end do
       ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ssn  , UNC_LOC_U, toutputx)
       ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sst  , UNC_LOC_U, toutputy)

       ! DEBUG
       if (jasedtranspveldebug>0) then
          ierr = nf90_put_var(mapids%ncid, mapids%id_u1s(2), u1sed(lnx1d+1:lnx), start = (/ 1, itim /), count = (/ lnx-lnx1d, 1 /))
          ierr = nf90_put_var(mapids%ncid, mapids%id_q1s(2), q1sed(lnx1d+1:lnx), start = (/ 1, itim /), count = (/ lnx-lnx1d, 1 /))
          call reconstructsedadvvel()
          ierr = nf90_put_var(mapids%ncid, mapids%id_ucxs(2), ucxsed(1:ndxi), start = (/ 1, itim /), count = (/ ndxi, 1 /))
          ierr = nf90_put_var(mapids%ncid, mapids%id_ucys(2), ucysed(1:ndxi), start = (/ 1, itim /), count = (/ ndxi, 1 /))
          ierr = nf90_put_var(mapids%ncid, mapids%id_qcxs(2), qcxsed(1:ndxi), start = (/ 1, itim /), count = (/ ndxi, 1 /))
          ierr = nf90_put_var(mapids%ncid, mapids%id_qcys(2), qcysed(1:ndxi), start = (/ 1, itim /), count = (/ ndxi, 1 /))
          if (stmpar%lsedsus .gt. 0) then
             do j = ISED1, ISEDN
                do kk=1,NdxNdxi
                   workx(kk) = xsedflux(j,kk)
                end do
                ierr = nf90_put_var(mapids%ncid, mapids%id_xsedflux(2,j), workx(1:NdxNdxi), start = (/ 1, itim /), count = (/ NdxNdxi, 1 /))
                do kk=1,NdxNdxi
                   workx(kk) = ysedflux(j,kk)
                end do
                ierr = nf90_put_var(mapids%ncid, mapids%id_ysedflux(2,j), workx(1:NdxNdxi), start = (/ 1, itim /), count = (/ NdxNdxi, 1 /))
             end do
          end if
          ! /DEBUG
       end if
   endif 
   if (stmpar%lsedtot > 0) then
      call realloc(toutputx, (/lnx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      call realloc(toutputy, (/lnx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      do l = 1, stmpar%lsedtot
         select case(stmpar%morpar%moroutput%transptype)
         case (0)
            rhol = 1d0
         case (1)
            rhol = stmpar%sedpar%cdryb(l)
         case (2)
            rhol = stmpar%sedpar%rhosol(l)
         end select
         toutputx(:,l) = sedtra%e_sbn(:,l)/rhol
         toutputy(:,l) = sedtra%e_sbt(:,l)/rhol
      end do
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbn  , UNC_LOC_U, toutputx)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbt  , UNC_LOC_U, toutputy)
   endif 
   !      
   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mor_bl, UNC_LOC_S, bl) 
   !
   if (stmpar%morpar%moroutput%uuuvvv) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uuu, UNC_LOC_S, sedtra%uuu) 
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vvv, UNC_LOC_S, sedtra%vvv) 
   end if
   !
   if (stmpar%lsedsus .gt. 0) then
      
      call realloc(work3d, (/kmx, ndxndxi, stmpar%lsedsus /), keepExisting=.false., fill = dmiss)
      if (kmx>0) then
         ! Concentrations
         do kk = 1, ndxndxi
            call getkbotktop(kk,kb,kt)
            call getlayerindices(kk, nlayb,nrlay)
            do k = kb, kt
               work3d(k-kb+nlayb,kk,:) = constituents(ISED1:ISEDN,k)
            enddo
         enddo
         ierr = nf90_put_var(mapids%ncid,mapids%id_sedfrac(2),work3d(1:kmx,1:ndxndxi,1:stmpar%lsedsus),start=(/1,1,1,itim/), count=(/kmx,ndxndxi,stmpar%lsedsus,1/))
         work3d = dmiss
         ! Settling velocity
         do kk = 1, ndxndxi
            call getkbotktop(kk,kb,kt)
            call getlayerindices(kk,nlayb,nrlay)
            do k = kb, kt
               work3d(k-kb+nlayb,kk,:) = mtd%ws(k,1:stmpar%lsedsus)
            enddo
         enddo
         ierr = nf90_put_var(mapids%ncid,mapids%id_ws(2),work3d(1:kmx,1:ndxndxi,1:stmpar%lsedsus),start=(/1,1,1,itim/), count=(/kmx,ndxndxi,stmpar%lsedsus,1/))
      else
         call realloc(work1d_z,(/ndxndxi, stmpar%lsedsus /), keepExisting=.false., fill = dmiss)
         work1d_z = transpose(constituents(ISED1:ISEDN,:))  ! avoid array slice on stack
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedfrac, UNC_LOC_S, work1d_z)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ws, UNC_LOC_S, mtd%ws)
      endif
!
      if (kmx == 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rsedeq, UNC_LOC_S, sedtra%rsedeq)
      end if
!
      if (stmpar%morpar%moroutput%aks) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_aks, UNC_LOC_S, sedtra%aks)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rca, UNC_LOC_S, sedtra%rca)
      end if
!
      if (stmpar%morpar%moroutput%sourcesink) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sourse, UNC_LOC_S, sedtra%sourse)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sinkse, UNC_LOC_S, sedtra%sinkse)
      endif

      if (stmpar%morpar%moroutput%suvcor) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_scrn ,UNC_LOC_U, sedtra%e_scrn)
      end if
   endif
   !
   if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_e_dzdn ,UNC_LOC_U, sedtra%e_dzdn)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_e_dzdt ,UNC_LOC_U, sedtra%e_dzdt)
   end if
!
   if (stmpar%morpar%moroutput%umod) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_umod , UNC_LOC_S, sedtra%umod)
   end if
!   
   if (stmpar%morpar%moroutput%zumod) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_zumod , UNC_LOC_S, sedtra%zumod)
   end if
!
   if (stmpar%morpar%moroutput%ustar) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ustar , UNC_LOC_S, sqrt(sedtra%ust2))
   end if
!
   if (stmpar%morpar%moroutput%rawtransports) then
      if (stmpar%morpar%moroutput%sbcuv) then
         call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         do l = 1, stmpar%lsedtot
            select case(stmpar%morpar%moroutput%transptype)
            case (0)
               rhol = 1d0
            case (1)
               rhol = stmpar%sedpar%cdryb(l)
            case (2)
               rhol = stmpar%sedpar%rhosol(l)
            end select
            toutputx(:,l) = sbcx_raw(:,l)/rhol
            toutputy(:,l) = sbcy_raw(:,l)/rhol
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcx   , UNC_LOC_S, toutputx)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcy   , UNC_LOC_S, toutputy)
      endif
      !
      if (stmpar%morpar%moroutput%sbwuv) then
         call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         do l = 1, stmpar%lsedtot
            select case(stmpar%morpar%moroutput%transptype)
            case (0)
               rhol = 1d0
            case (1)
               rhol = stmpar%sedpar%cdryb(l)
            case (2)
               rhol = stmpar%sedpar%rhosol(l)
            end select
            toutputx(:,l) = sbwx_raw(:,l)/rhol
            toutputy(:,l) = sbwy_raw(:,l)/rhol
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwx   , UNC_LOC_S, toutputx)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwy   , UNC_LOC_S, toutputy)
      endif
      !
      if (stmpar%morpar%moroutput%sswuv) then
         call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         do l = 1, stmpar%lsedtot
            select case(stmpar%morpar%moroutput%transptype)
            case (0)
               rhol = 1d0
            case (1)
               rhol = stmpar%sedpar%cdryb(l)
            case (2)
               rhol = stmpar%sedpar%rhosol(l)
            end select
            toutputx(:,l) = sswx_raw(:,l)/rhol
            toutputy(:,l) = sswy_raw(:,l)/rhol
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswx   , UNC_LOC_S, toutputx)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswy   , UNC_LOC_S, toutputy)
      endif
   endif
   !
   ! Get cell centre transport values, removed from fm_erosed and fm_bott3d, and calculated here and stored in sscx, sscy, sbcx, sbcy, sbwx, sbwy, sswx, sswy
   call reconstructsedtransports()

   if (stmpar%morpar%moroutput%rawtransports) then
      if (stmpar%morpar%moroutput%sscuv) then
         call realloc(toutputx, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
         call realloc(toutputy, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
         do l = 1, stmpar%lsedsus
            select case(stmpar%morpar%moroutput%transptype)
            case (0)
               rhol = 1d0
            case (1)
               rhol = stmpar%sedpar%cdryb(sedtot2sedsus(l))
            case (2)
               rhol = stmpar%sedpar%rhosol(sedtot2sedsus(l))
            end select
            toutputx(:,l) = sedtra%sscx(:,sedtot2sedsus(l))/rhol
            toutputy(:,l) = sedtra%sscy(:,sedtot2sedsus(l))/rhol
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscx   , UNC_LOC_S, toutputx)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscy   , UNC_LOC_S, toutputy)
      endif
   endif
   
   if (stmpar%morpar%moroutput%sbcuv) then
      call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      do l = 1, stmpar%lsedtot
         select case(stmpar%morpar%moroutput%transptype)
         case (0)
            rhol = 1d0
         case (1)
            rhol = stmpar%sedpar%cdryb(l)
         case (2)
            rhol = stmpar%sedpar%rhosol(l)
         end select
         toutputx(:,l) = sedtra%sbcx(:,l)/rhol
         toutputy(:,l) = sedtra%sbcy(:,l)/rhol
      end do
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcx_reconstructed   , UNC_LOC_S, toutputx)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbcy_reconstructed   , UNC_LOC_S, toutputy)
   endif
!
   if (stmpar%morpar%moroutput%sbwuv) then
      call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      do l = 1, stmpar%lsedtot
         select case(stmpar%morpar%moroutput%transptype)
         case (0)
            rhol = 1d0
         case (1)
            rhol = stmpar%sedpar%cdryb(l)
         case (2)
            rhol = stmpar%sedpar%rhosol(l)
         end select
         toutputx(:,l) = sedtra%sbwx(:,l)/rhol
         toutputy(:,l) = sedtra%sbwy(:,l)/rhol
      end do
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwx_reconstructed   , UNC_LOC_S, toutputx)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sbwy_reconstructed   , UNC_LOC_S, toutputy)
   endif
!
   if (stmpar%morpar%moroutput%sswuv) then
      call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      do l = 1, stmpar%lsedtot
         select case(stmpar%morpar%moroutput%transptype)
         case (0)
            rhol = 1d0
         case (1)
            rhol = stmpar%sedpar%cdryb(l)
         case (2)
            rhol = stmpar%sedpar%rhosol(l)
         end select
         toutputx(:,l) = sedtra%sswx(:,l)/rhol
         toutputy(:,l) = sedtra%sswy(:,l)/rhol
      end do
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswx_reconstructed   , UNC_LOC_S, toutputx)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sswy_reconstructed   , UNC_LOC_S, toutputy)
   endif
   !
   if (stmpar%morpar%moroutput%sscuv) then
      call realloc(toutputx, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
      call realloc(toutputy, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
      do l = 1, stmpar%lsedsus
         select case(stmpar%morpar%moroutput%transptype)
         case (0)
            rhol = 1d0
         case (1)
            rhol = stmpar%sedpar%cdryb(sedtot2sedsus(l))
         case (2)
            rhol = stmpar%sedpar%rhosol(sedtot2sedsus(l))
         end select
         toutputx(:,l) = sedtra%sscx(:,sedtot2sedsus(l))/rhol
         toutputy(:,l) = sedtra%sscy(:,sedtot2sedsus(l))/rhol
      end do
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscx_reconstructed   , UNC_LOC_S, toutputx)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sscy_reconstructed   , UNC_LOC_S, toutputy)
   endif
   !
   call realloc(toutputx, (/ndxi, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
   call realloc(toutputy, (/ndxi, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
   do l = 1, stmpar%lsedtot
      select case(stmpar%morpar%moroutput%transptype)
      case (0)
         rhol = 1d0
      case (1)
         rhol = stmpar%sedpar%cdryb(l)
      case (2)
         rhol = stmpar%sedpar%rhosol(l)
      end select
      toutputx(1:ndxi,l) = (sedtra%sxtot(1:ndxi,l))/rhol
      toutputy(1:ndxi,l) = (sedtra%sytot(1:ndxi,l))/rhol
   end do
   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sxtot   , UNC_LOC_S, toutputx)
   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sytot   , UNC_LOC_S, toutputy)
   !
   ! Time averaged transports, could probably be more concise...
   !morstarthyd = tstart_user + stmpar%morpar%tmor*tfac        ! seconds
   dmorft      = stmpar%morpar%morft - stmpar%morpar%morft0    ! days since morstart
   dmorfs      = dmorft*86400.0d0                              ! seconds
   mortime     = stmpar%morpar%morft*86400d0                   ! seconds*morfac since tstart_user
   if (stmpar%morpar%hydrt > stmpar%morpar%hydrt0) then       
      moravg = dmorft/(stmpar%morpar%hydrt - stmpar%morpar%hydrt0)
   else
      moravg = 0d0
   endif
   !
   ierr = nf90_put_var(mapids%ncid, mapids%id_morfac  , moravg,(/ itim /))
   ierr = nf90_put_var(mapids%ncid, mapids%id_morft   , mortime,(/ itim /))
   !
   if (stmpar%morpar%moroutput%cumavg) then
      ! Bedload components
      call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
      if ( dmorft > 0d0 ) then
         do l = 1, stmpar%lsedtot
            select case(stmpar%morpar%moroutput%transptype)
            case (0)
               rhodt = dmorfs
            case (1)
               rhodt = stmpar%sedpar%cdryb(l)*dmorfs
            case (2)
               rhodt = stmpar%sedpar%rhosol(l)*dmorfs
            end select
            toutputx(:,l) = sedtra%sbxcum(:,l)/rhodt
            toutputy(:,l) = sedtra%sbycum(:,l)/rhodt
         enddo
      else
          toutputx = 0d0
          toutputy = 0d0
      endif      
      ierr = nf90_put_var(mapids%ncid, mapids%id_sbxcum(2), toutputx(1:ndxi,:), start = (/ 1, 1, itim /), count = (/ ndxi, stmpar%lsedtot, 1 /))
      ierr = nf90_put_var(mapids%ncid, mapids%id_sbycum(2), toutputy(1:ndxi,:), start = (/ 1, 1, itim /), count = (/ ndxi, stmpar%lsedtot, 1 /))       
      !
      ! Suspended load
      if ( dmorft > 0d0 ) then
         do l = 1, stmpar%lsedtot
            select case(stmpar%morpar%moroutput%transptype)
            case (0)
               rhodt = dmorfs
            case (1)
               rhodt = stmpar%sedpar%cdryb(l)*dmorfs
            case (2)
               rhodt = stmpar%sedpar%rhosol(l)*dmorfs
            end select
            toutputx(:,l) = sedtra%ssxcum(:,l)/rhodt
            toutputy(:,l) = sedtra%ssycum(:,l)/rhodt
         enddo
      else
          toutputx = 0d0
          toutputy = 0d0
      endif      
      ierr = nf90_put_var(mapids%ncid, mapids%id_ssxcum(2), toutputx(1:ndxi,:), start = (/ 1, 1, itim /), count = (/ ndxi, stmpar%lsedtot, 1 /))
      ierr = nf90_put_var(mapids%ncid, mapids%id_ssycum(2), toutputy(1:ndxi,:), start = (/ 1, 1, itim /), count = (/ ndxi, stmpar%lsedtot, 1 /))       
   else
      if (time_map >= ti_mape) then   ! to check, last timestep?
         ierr = nf90_put_var(mapids%ncid, mapids%id_sedavgtim    , mortime, (/ 1 /))  
         ! Bedload components
         call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
         if ( dmorft > 0d0 ) then
            do l = 1, stmpar%lsedtot
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhodt = dmorfs
               case (1)
                  rhodt = stmpar%sedpar%cdryb(l)*dmorfs
               case (2)
                  rhodt = stmpar%sedpar%rhosol(l)*dmorfs
               end select
               toutputx(:,l) = sedtra%sbxcum(:,l)/rhodt
               toutputy(:,l) = sedtra%sbycum(:,l)/rhodt
            enddo
         else
             toutputx = 0d0
             toutputy = 0d0
         endif      
         ierr = nf90_put_var(mapids%ncid, mapids%id_sbxcum(2), toutputx(1:ndxi,:), start = (/ 1, 1 /), count = (/ ndxi, stmpar%lsedtot /))
         ierr = nf90_put_var(mapids%ncid, mapids%id_sbycum(2), toutputy(1:ndxi,:), start = (/ 1, 1 /), count = (/ ndxi, stmpar%lsedtot /))
         !
         ! Suspended load
         if ( dmorft > 0d0 ) then
            do l = 1, stmpar%lsedtot
               select case(stmpar%morpar%moroutput%transptype)
               case (0)
                  rhodt = dmorfs
               case (1)
                  rhodt = stmpar%sedpar%cdryb(l)*dmorfs
               case (2)
                  rhodt = stmpar%sedpar%rhosol(l)*dmorfs
               end select
               toutputx(:,l) = sedtra%ssxcum(:,l)/rhodt
               toutputy(:,l) = sedtra%ssycum(:,l)/rhodt
            enddo
         else
             toutputx = 0d0
             toutputy = 0d0
         endif      
         ierr = nf90_put_var(mapids%ncid, mapids%id_ssxcum(2), toutputx(1:ndxi,:), start = (/ 1, 1 /), count = (/ ndxi, stmpar%lsedtot /))
         ierr = nf90_put_var(mapids%ncid, mapids%id_ssycum(2), toutputy(1:ndxi,:), start = (/ 1, 1 /), count = (/ ndxi, stmpar%lsedtot /))         
      end if
   end if
!
   select case (stmpar%morlyr%settings%iunderlyr)
      case (1)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_bodsed  , UNC_LOC_S, stmpar%morlyr%state%bodsed, locdim=2)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dpsed   , UNC_LOC_S, stmpar%morlyr%state%dpsed)
      case (2)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_msed  , UNC_LOC_S, stmpar%morlyr%state%msed , locdim=3)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_thlyr , UNC_LOC_S, stmpar%morlyr%state%thlyr, locdim=2)
         !
         if (.not. allocated(frac) ) allocate( frac(1:ndx, 1:stmpar%morlyr%settings%nlyr, stmpar%lsedtot) )
         frac = -999d0
         do l = 1, stmpar%lsedtot
            if (stmpar%morlyr%settings%iporosity==0) then
               dens = stmpar%sedpar%cdryb(l)
            else
               dens = stmpar%sedpar%rhosol(l)
            endif
            do k = 1, stmpar%morlyr%settings%nlyr
               do nm = 1, ndxi
                  if (stmpar%morlyr%state%thlyr(k,nm)>0.0_fp) then
                       frac(nm, k, l) = stmpar%morlyr%state%msed(l, k, nm)/(dens*stmpar%morlyr%state%svfrac(k, nm) * &
                                        stmpar%morlyr%state%thlyr(k, nm))
                  else
                       frac(nm, k, l) = 0d0
                  endif
               enddo
            enddo
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_lyrfrac  , UNC_LOC_S, frac)
         !
         if (stmpar%morlyr%settings%iporosity>0) then
            if (.not. allocated(poros) ) allocate( poros(1:stmpar%morlyr%settings%nlyr, 1:ndx ) )
            poros = 1d0-stmpar%morlyr%state%svfrac
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_poros , UNC_LOC_S, poros, locdim=2)
         endif
         !
      case default
         ! do nothing
      end select
               
      if (stmpar%morpar%moroutput%taurat) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_taurat, UNC_LOC_S, sedtra%taurat)
      endif
      if (stmpar%morpar%moroutput%dm) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dm    , UNC_LOC_S, sedtra%dm)
      endif
      if (stmpar%morpar%moroutput%dg) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dg    , UNC_LOC_S, sedtra%dg)
      endif
      if (stmpar%morpar%moroutput%dgsd) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dgsd  , UNC_LOC_S, sedtra%dgsd)
      endif
      if (stmpar%morpar%moroutput%percentiles) then
         do l = 1, stmpar%morpar%nxx
            call realloc(toutput, ndx, keepExisting=.false., fill = -999d0)
            toutput = sedtra%dxx(1:ndx, l)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dxx(l,:)   , UNC_LOC_S, toutput)
         enddo
      endif
      if (stmpar%morpar%moroutput%frac) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_frac , UNC_LOC_S, sedtra%frac)
      endif
      if (stmpar%morpar%moroutput%mudfrac) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_mudfrac , UNC_LOC_S, sedtra%mudfrac)
      endif
      if (stmpar%morpar%moroutput%sandfrac) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_sandfrac , UNC_LOC_S, sedtra%sandfrac)
      endif
      if (stmpar%morpar%moroutput%fixfac) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_fixfac , UNC_LOC_S, sedtra%fixfac)
      endif
      if (stmpar%morpar%moroutput%hidexp) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_hidexp , UNC_LOC_S, sedtra%hidexp)
      endif
      !
      if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0) then
         do l = 1, stmpar%lsedsus
            call realloc(toutput, ndx, keepExisting=.false., fill = -999d0)
            toutput = stmpar%morpar%flufflyr%mfluff(l,1:ndx)
            ! ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_mfluff , UNC_LOC_S, stmpar%morpar%flufflyr%mfluff)
            ierr = nf90_put_var(mapids%ncid, mapids%id_mfluff(2)   , toutput(1:ndxi) , start = (/ 1, l, itim /), count = (/ ndxi, 1, 1 /))
         end do
      end if
      !
      if (ndx1d > 0) then
         j = 0
         do i = 1,size(pCSs)
            if (pCSs(i)%levelscount == 0) exit
            j = max(j,pCSs(i)%levelscount)
         enddo
         jmax = j
         call realloc(work1d_z, [jmax, ndx1d], fill = dmiss, stat=ierr)
         if (ierr == 0) call realloc(work1d_n, [jmax, ndx1d], fill = dmiss, stat=ierr)
         if (ierr /= 0) then
            call mess(LEVEL_ERROR, 'Allocation error in unc_write_map_filepointer_ugrid.')
            return
         endif
         do i = 1,ndx1d
            k = ndx2d + i
            if (gridpoint2cross(k)%num_cross_sections==1) then
               n = gridpoint2cross(k)%cross(1)
               if (n==-999) cycle
               pCS => network%crs%cross(n)%tabdef
               if (pCS%crosstype == CS_TABULATED) then
                  do j = 1,pCS%levelscount
                     work1d_z(j,i) = pCS%height(j)
                     work1d_n(j,i) = pCS%flowWidth(j) * 0.5d0
                  enddo
               endif
            endif
         enddo
         ierr = nf90_put_var(mapids%ncid, mapids%id_tsp%id_flowelemcrsz(1), work1d_z, start=(/ 1, 1, mapids%id_tsp%idx_curtime /), count=(/ jmax, ndx1d, 1 /) )
         ierr = nf90_put_var(mapids%ncid, mapids%id_tsp%id_flowelemcrsn(1), work1d_n, start=(/ 1, 1, mapids%id_tsp%idx_curtime /), count=(/ jmax, ndx1d, 1 /) )
      endif
   endif
   !
   ! BEDFORMS
   !
   if (bfmpar%lfbedfrmout) then
      if (bfmpar%lfbedfrm) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_duneheight, UNC_LOC_S, bfmpar%duneheight(1:ndxi))
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dunelength, UNC_LOC_S, bfmpar%dunelength(1:ndxi))
      end if
      !
      if (bfmpar%lfbedfrmrou) then
         if (.not. allocated(rks)) then 
            allocate(rks(1:ndx))
            rks = 0d0
         end if
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ksr,  UNC_LOC_S, bfmpar%rksr(1:ndxi))
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ksmr, UNC_LOC_S, bfmpar%rksmr(1:ndxi))
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ksd,  UNC_LOC_S, bfmpar%rksd(1:ndxi))
         
         do nm = 1,ndxi
            rks(nm) = sqrt(bfmpar%rksr(nm)**2 + bfmpar%rksmr(nm)**2 + bfmpar%rksd(nm)**2)
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_ks,   UNC_LOC_S, rks(1:ndxi))
      end if
   end if

   ! Sediment transport (via own built-in sed)
   if (jamapsed > 0 .and. jased > 0 .and. .not.stm_included) then 
      do j = 1,mxgr
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sed(:,j), UNC_LOC_S, sed(j,:)) ! ,  (/ 1, 1, itim /), (/ mxgr, ndxndxi, 1 /))
      enddo
      if (jaceneqtr .eq. 1) then
         do j = 1,mxgr
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ero(:,j), UNC_LOC_S, grainlay(j,:))
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bl , UNC_LOC_S, bl)
      else
         do j = 1,mxgr
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ero(:,j), UNC_LOC_CN, grainlay(j,:))
         enddo
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bl , UNC_LOC_CN, zk)
      end if

      ! TODO: AvD: size(grainlay,2) is always correct (mxn), but we have a problem if jaceneqtr==2 and mxn/=numk,
      ! because then the dimension for ero is set to nNetNode, and coordinate attribute refers to NetNode_x
      ! (both length numk), whereas ero itself is shorter than numk.
   end if

   ! Meteo forcings
   if (jamapwind > 0 .and. jawind /= 0 .and. jawindstressgiven == 0) then
      allocate (windx(ndxndxi), windy(ndxndxi), stat=ierr)
      if (ierr /= 0) call aerr( 'windx/windy', ierr, ndxndxi)

      !windx/y is set to 0.0 for flownodes without links
      windx = 0.0d0
      windy = 0.0d0
      do n = 1,ndxndxi
         do LL=1,nd(n)%lnx
            LLL = abs(nd(n)%ln(LL))
            k1 = ln(1,LLL) ; k2 = ln(2,LLL)
            k3 = 1 ; if( nd(n)%ln(LL) > 0 ) k3 = 2
            windx(n) = windx(n) + wx(LLL) * wcL(k3,LLL)
            windy(n) = windy(n) + wy(LLL) * wcL(k3,LLL)
         end do
      end do
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windx , UNC_LOC_S, windx)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windy , UNC_LOC_S, windy)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windxu, UNC_LOC_U, wx)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windyu, UNC_LOC_U, wy)
      deallocate (windx, windy, stat=ierr)
   end if

   if (jamaprain > 0 .and. jarain /= 0) then
      call realloc(workx, ndx, keepExisting = .false., fill = dmiss)
      do n=1,ndxndxi
         workx(n) = rain(n)*bare(n)/ba(n)*1d-3/(24d0*3600d0) ! mm/day->(m3/s / m2) Average actual rainfall rate on grid cell area (maybe zero bare).
      end do
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rain  , UNC_LOC_S, workx)
   endif

   if (jamapwind > 0 .and. japatm > 0) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_patm  , UNC_LOC_S, patm)
   endif

   if (jawind /= 0 .and. jamapwindstress > 0) then
      allocate (windx(ndxndxi), windy(ndxndxi), stat=ierr)
      if (ierr /= 0) call aerr( 'windx/windy', ierr, ndxndxi)

      windx = 0.0d0
      windy = 0.0d0
      do n = 1,ndxndxi
         do LL=1,nd(n)%lnx
            LLL = abs(nd(n)%ln(LL))
            k1 = ln(1,LLL) ; k2 = ln(2,LLL)
            k3 = 1 ; if( nd(n)%ln(LL) > 0 ) k3 = 2
            windx(n) = windx(n) + wdsu_x(LLL) * wcL(k3,LLL) * rhomean
            windy(n) = windy(n) + wdsu_y(LLL) * wcL(k3,LLL) * rhomean
         end do
      end do
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressx, UNC_LOC_S, windx)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressy, UNC_LOC_S, windy)
      deallocate(windx, windy, stat=ierr)
   endif

   ! Heat flux models
   if (jamapheatflux > 0 .and. jatem > 1) then ! here less verbose

      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_tair  , UNC_LOC_S, Tair)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_rhum  , UNC_LOC_S, Rhum)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_clou  , UNC_LOC_S, Clou)

       
      if (jatem == 5) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qsun  , UNC_LOC_S, Qsunmap)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qeva  , UNC_LOC_S, Qevamap)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qcon  , UNC_LOC_S, Qconmap)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qlong , UNC_LOC_S, Qlongmap)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qfreva, UNC_LOC_S, Qfrevamap)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qfrcon, UNC_LOC_S, Qfrconmap)
      end if
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp   , mapids%id_qtot  , UNC_LOC_S, Qtotmap)
   end if

   if (jamapwav>0) then
   ! JRE - XBeach
   if (jawave .eq. 4) then 
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_E        , UNC_LOC_S, E)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_R        , UNC_LOC_S, R)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_D        , UNC_LOC_S, D)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_DR       , UNC_LOC_S, DR)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Df       , UNC_LOC_S, Df)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Sxx      , UNC_LOC_S, Sxx)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Syy      , UNC_LOC_S, Syy)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Sxy      , UNC_LOC_S, Sxy)
     
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sigmwav  , UNC_LOC_S, sigmwav)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cwav     , UNC_LOC_S, cwav)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cgwav    , UNC_LOC_S, cgwav)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_kwav     , UNC_LOC_S, kwav)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nwav     , UNC_LOC_S, nwav)
      if (windmodel.eq.0) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_l1       , UNC_LOC_S, L1)
      elseif ( (windmodel.eq.1) .and. (jawsource.eq.1 ) ) then
          ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_SwE      , UNC_LOC_S, SwE)      
          ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_SwT      , UNC_LOC_S, SwT) 
      endif
      
      if (jamombal==1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dsdx         , UNC_LOC_S, xbdsdx)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dsdy         , UNC_LOC_S, xbdsdy)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ducxdx       , UNC_LOC_S, xbducxdx)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ducydx       , UNC_LOC_S, xbducydx)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ducxdy       , UNC_LOC_S, xbducxdy)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ducydy       , UNC_LOC_S, xbducydy)
      end if

      ierr = nf90_put_var(mapids%ncid, mapids%id_ctheta(2)   , ctheta(:,1:ndxi) , start = (/ 1, 1, itim /), count = (/ ntheta, ndxi, 1 /))
   endif
   
   if (jawave == 3) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxwav     , UNC_LOC_S, sxwav)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sywav     , UNC_LOC_S, sywav)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxbwav     , UNC_LOC_S, sbxwav)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sybwav     , UNC_LOC_S, sbywav)
   end if
   
   if (jawave .gt. 0) then
      if (jamapsigwav==0) then
         wavfac = 1d0
      else
         wavfac = sqrt(2d0)
      endif
      if (allocated(wa)) deallocate(wa, stat = ierr)
      allocate(wa(1:ndx), stat=ierr)
      wa = wavfac*hwav
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav        , UNC_LOC_S, wa)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uorb        , UNC_LOC_S, uorb, 0d0)
   endif

   if (jawave .gt. 2) then
           
      wa = modulo(270d0 - phiwav, 360d0)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_thetamean, UNC_LOC_S, wa) 
      deallocate(wa)
      
      ! ustokes, vstokes
      if (.not. allocated(ust_x)) allocate(ust_x(ndkx), ust_y(ndkx), wavout(lnx),wavout2(lnx))
      ust_x = 0d0; ust_y = 0d0
      if (kmx==0) then
         do L=1,lnx
            k1 = ln(1,L); k2 = ln(2,L)
            ust_x(k1) = ust_x(k1) + wcx1(L)*ustokes(L)
            ust_x(k2) = ust_x(k2) + wcx2(L)*ustokes(L)
            ust_y(k1) = ust_y(k1) + wcy1(L)*ustokes(L)
            ust_y(k2) = ust_y(k2) + wcy2(L)*ustokes(L)
         enddo
      else
         do LL = 1,lnx
            call getLbotLtopmax(LL, Lb, Lt)
            do L = Lb,Lt
               k1 = ln(1,L); k2 = ln(2,L)
               ust_x(k1) = ust_x(k1) + wcx1(LL)*ustokes(L)
               ust_x(k2) = ust_x(k2) + wcx2(LL)*ustokes(L)
               ust_y(k1) = ust_y(k1) + wcy1(LL)*ustokes(L)
               ust_y(k2) = ust_y(k2) + wcy2(LL)*ustokes(L)
            end do  
         end do
      endif

      ! then write:
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustokes      , iLocS, ust_x)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vstokes      , iLocS, ust_y)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustokeslink      , iLocU, ustokes)
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vstokeslink      , iLocU, vstokes)
       
      ! Wave forces
      if (jawave == 3) then
         ust_x = 0d0; ust_y = 0d0; wavout=0d0; wavout2=0d0
         do L= 1, lnx
            k1 = ln(1,L); k2=ln(2,L)
            ust_x(k1) = ust_x(k1) + wcx1(L)*wavfu(L)*hu(L)*rhomean
            ust_x(k2) = ust_x(k2) + wcx2(L)*wavfu(L)*hu(L)*rhomean
            ust_y(k1) = ust_y(k1) + wcy1(L)*wavfu(L)*hu(L)*rhomean
            ust_y(k2) = ust_y(k2) + wcy2(L)*wavfu(L)*hu(L)*rhomean  
            wavout(L) = wavfu(L)*hu(L)*rhomean   ! stack
            wavout2(L) = wavfv(L)*hu(L)*rhomean
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fx       , UNC_LOC_S, ust_x, 0d0)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fy       , UNC_LOC_S, ust_y, 0d0)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fxlink, UNC_LOC_U, wavout)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fylink, UNC_LOC_U, wavout2)
         deallocate(ust_x, ust_y, wavout, wavout2)
      end if
      if (jawave == 4) then ! cell centre values available
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fx       , UNC_LOC_S, Fx_cc, 0d0)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fy       , UNC_LOC_S, Fy_cc, 0d0)
      end if
   end if

   end if
   
   ! Roughness from trachytopes
   if (jamaptrachy > 0 .and. jatrt == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, UNC_LOC_L, cftrt(:,2))
   end if

   ! Calibration factor for roughness from trachytopes
   if (jamapcali > 0 .and. jacali == 1) then
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cfcl, UNC_LOC_L, cfclval)
   end if
   
   if ( janudge.gt.0 .and. jamapnudge.gt.0 ) then
!    nudging
     ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_tem, UNC_LOC_S3D, nudge_tem)
     ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_sal, UNC_LOC_S3D, nudge_sal)
     
     workx = DMISS
     do k=1,ndkx
        if ( nudge_tem(k).ne.DMISS ) then
           workx(k) = nudge_tem(k)-constituents(itemp, k)
        end if
     end do
     ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_Dtem, UNC_LOC_S3D, workx)
     
     workx = DMISS
     do k=1,ndkx
        if ( nudge_tem(k).ne.DMISS ) then
           workx(k) = nudge_sal(k)-sa1(k)
        end if
     end do
     ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_Dsal, UNC_LOC_S3D, workx)
   end if
   
   if ( japart.eq.1 .and. jatracer.eq.1 .and. kmx.gt.0 ) then
!     depth-averaged particle concentration     
      do k=1,Ndx
         workx(k) = constituents(part_iconst,k)
      end do
      ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_depth_averaged_particle_concentration, UNC_LOC_S, workx)
   end if
  
   if (ndxi-ndx2d>0) then
      if (jamapTimeWetOnGround > 0) then ! Cumulative time water above ground level
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_timewetground, UNC_LOC_S, time_wetground) 
      end if
      if (jamapFreeboard > 0) then ! freeboard
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_freeboard, UNC_LOC_S, freeboard) 
      end if
      if (jamapDepthOnGround > 0) then ! waterdepth that is above ground level
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hs_on_ground, UNC_LOC_S, hsOnGround)
      end if
      if (jamapVolOnGround > 0) then ! volume that is above ground level
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vol_on_ground, UNC_LOC_S, volOnGround)
      end if
      if (jamapTotalInflow1d2d > 0) then ! total 1d2d inflow
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qCur1d2d, UNC_LOC_S, qCur1d2d)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vTot1d2d, UNC_LOC_S, vTot1d2d)
      end if
      if (jamapTotalInflowLat > 0) then ! total lateral inflow
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qCurLat, UNC_LOC_S, qCurLat)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vTotLat, UNC_LOC_S, vTotLat)
      end if
   end if
   if (lnx1d > 0) then
      if (jamapS1Gradient > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s1Gradient, UNC_LOC_U, s1Gradient)
      end if
   end if
end subroutine unc_write_map_filepointer_ugrid


!> Writes map/flow data to an already opened netCDF dataset.
!! The netnode and -links have been written already.
subroutine unc_write_map_filepointer(imapfile, tim, jaseparate) ! wrimap
    use m_flow
    use m_flowtimes
    use m_flowgeom
    use m_sobekdfm
    use m_heatfluxes
    use m_sferic
    use network_data
    use m_sediment
    use m_bedform
    use m_wind
    use m_flowparameters, only: jatrt, jacali
    use m_fm_wq_processes
    use m_xbeach_data
    use m_transport, only: NUMCONST, itemp, ITRA1, ITRAN, ISED1, ISEDN, constituents, const_names, const_units, id_const
    use bedcomposition_module, only: bedcomp_getpointer_integer
    use m_alloc
    use m_missing
    use m_partitioninfo, only: jampi
    use string_module, only: replace_multiple_spaces_by_single_spaces
  
    implicit none

    integer,           intent(in) :: imapfile
    real(kind=hp),     intent(in) :: tim
    integer, optional, intent(in) :: jaseparate   !< Whether this save is manual/by user (not part of the standard map write series)

    integer                       :: jaseparate_, idims(2)

    logical, dimension(2), save   :: firststep = .true.

    integer, save                 :: ierr, ndim
    integer, dimension(2), save   :: &
    !id_netcelldim, id_netcellmaxnodedim, id_netcellcontourptsdim, &
    id_laydim, id_wdim, &
        id_flowelemdim, &
    id_maxfracdim,  &
    id_erolaydim,   &
    id_flowlinkdim, &
    id_netlinkdim,  &
    id_1d2ddim,     &
    id_timedim,     &
    id_time, id_timestep, &
    id_sbcx, id_sbcy, id_sbcx_reconstructed, id_sbcy_reconstructed, &
    id_sbwx, id_sbwy, id_sbwx_reconstructed, id_sbwy_reconstructed, &
    id_sswx, id_sswy, id_sswx_reconstructed, id_sswy_reconstructed, &
    id_sourse, id_sinkse, id_ws, &
    id_sxtot, id_sytot, id_rsedeq, id_umod, id_zumod, id_ustar, id_dzdn, id_dzdt, id_morbl, id_diag, id_aks, id_rca, &
    id_bodsed, id_dpsed, id_msed, id_lyrfrac, id_thlyr, id_poros, id_nlyrdim, &
    id_sedtotdim, id_sedsusdim, id_rho, id_viu, id_diu, id_q1, id_spircrv, id_spirint, &
    id_q1main, &
    id_s1, id_taus, id_ucx, id_ucy, id_ucz, id_ucxa, id_ucya, id_unorm, id_ww1, id_sa1, id_tem1, id_sed, id_ero, id_s0, id_u0, id_cfcl, id_cftrt, id_czs, & 
    id_qsun, id_qeva, id_qcon, id_qlong, id_qfreva, id_qfrcon, id_qtot, &
    id_wind, id_patm, id_tair, id_rhum, id_clou, id_E, id_R, id_H, id_D, id_DR, id_urms, id_thetamean, &
    id_cwav, id_cgwav, id_sigmwav, id_SwE, id_SwT, &
    id_ust, id_Fx, id_Fy, id_vst, id_windx, id_windy, id_windxu, id_windyu, id_numlimdt, id_hs, id_bl, id_zk, &
    id_1d2d_edges, id_1d2d_zeta1d, id_1d2d_crest_level, id_1d2d_b_2di, id_1d2d_b_2dv, id_1d2d_d_2dv, id_1d2d_q_zeta, id_1d2d_q_lat, &
    id_1d2d_cfl, id_1d2d_flow_cond, id_1d2d_sb, id_1d2d_s1_2d, id_1d2d_s0_2d, id_tidep, id_salp, id_inttidesdiss, &
    id_rsi, id_dudx, id_dudy, id_dvdx, id_dvdy, & 
    id_duneheight, id_dunelength, id_ksd, id_ksr, id_ksmr, id_ks, &
    id_taurat, id_dm, id_dg, id_dgsd, id_frac, id_mudfrac, id_sandfrac, id_fixfac, id_hidexp, id_mfluff, id_scrn, id_urmscc, id_Fxcc, id_Fycc, &
    id_ducxdx, id_ducydy, id_ducxdy, id_ducydx, id_dsdx, id_dsdy, & 
    id_sscx, id_sscy, id_sscx_reconstructed, id_sscy_reconstructed, &
    id_turkin1, id_tureps1, id_vicwwu, id_swanbl

    integer,          dimension(:,:),   allocatable, save :: id_dxx                     ! fractions
    double precision, dimension(:),     allocatable       :: dum, ust_rot,vst_rot
    double precision, dimension(:,:),   allocatable       :: poros
    double precision, dimension(:,:,:), allocatable       :: frac
    double precision, dimension(:),     allocatable       :: toutput
    double precision, dimension(:,:),   allocatable       :: toutputx, toutputy
    double precision, dimension(:),     allocatable       :: rks
    
    integer,          dimension(:), allocatable :: idum

    integer :: iid, i, j, jj, numContPts, numNodes, itim, k, kb, kt, kk, n, LL, Ltx, Lb, L, nm, nlayb,nrlay, nlaybL, nrlayLx
    integer :: ndxndxi ! Either ndx or ndxi, depending on whether boundary nodes also need to be written.
    double precision, dimension(:), allocatable :: windx, windy, windang
    double precision, dimension(:), allocatable :: numlimdtdbl ! TODO: WO/AvD: remove this once integer version of unc_def_map_var is available
    double precision :: vicc, dicc, dens
    integer :: jaeulerloc
    
    double precision   :: rhol
    character(16)      :: dxname
    character(64)      :: dxdescr
    character(10)      :: transpunit
    character(len=255) :: tmpstr

    integer, dimension(:), allocatable :: flag_val
    character(len=10000)               :: flag_mean
    
    if (.not. allocated(id_dxx) .and. stm_included) allocate(id_dxx(1:stmpar%morpar%nxx,1:2))
    
    ! If jaseparate_==1 or this map file was just opened for the first time:
    ! only write net+vardefs first time, and write subsequent flow snapshots in later calls.
    ! jaseparate_==2: write com file
    if (present(jaseparate)) then
        jaseparate_ = jaseparate
    else
        jaseparate_ = 0
    end if
    
    if (jaseparate_ == 0 .or. jaseparate_ == 1) then
       ! mapfile, store/use ids number 1
       iid = 1
       ndxndxi = ndxi
    elseif (jaseparate_ == 2) then
       ! comfile, store/use ids number 2
       iid = 2
       ndxndxi = ndx ! Com file, include boundary nodes
    else
       ! error
       iid = 0
    endif
    
    ! Use nr of dimensions in netCDF file a quick check whether vardefs were written
    ! before in previous calls.
    ndim = 0
    ierr = nf90_inquire(imapfile, nDimensions=ndim)

    ! Only write net and flow geometry data the first time, or for a separate map file.
    if (ndim == 0) then

        call unc_write_net_filepointer(imapfile)      ! Write standard net data as well
    
        if (jaseparate_ == 2) then
           call unc_write_flowgeom_filepointer(imapfile, jabndnd = 1) ! Write time-independent flow geometry data, with boundary nodes
           ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_flowelemdim(iid))
        else
           call unc_write_flowgeom_filepointer(imapfile) ! Write time-independent flow geometry data
           ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim(iid))
        end if

        ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim(iid))
        ierr = nf90_inq_dimid(imapfile, 'nNetLink' , id_netlinkdim(iid))

        if (nbnd1d2d > 0) then
           ierr = nf90_def_dim(imapfile, 'nBnd1d2d', nbnd1d2d, id_1d2ddim(iid))
        end if

        ! Time
        ierr = nf90_def_dim(imapfile, 'time', nf90_unlimited, id_timedim(iid))
        call check_error(ierr, 'def time dim')
        ierr = nf90_def_var(imapfile, 'time', nf90_double, id_timedim(iid),  id_time(iid))
        ierr = nf90_put_att(imapfile, id_time(iid),  'units'        , 'seconds since '//refdat(1:4)//'-'//refdat(5:6)//'-'//refdat(7:8)//' 00:00:00')
        ierr = nf90_put_att(imapfile, id_time(iid),  'standard_name', 'time')

        ! 3D    
        if ( kmx > 0 ) then
           call unc_append_3dflowgeom_def(imapfile)              ! Append definition of time-independent 3d flow geometry data
           ierr = nf90_inq_dimid(imapfile, 'laydim', id_laydim(iid))
           ierr = nf90_inq_dimid(imapfile, 'wdim', id_wdim(iid))
        end if        

        ! Size of latest timestep
        ierr = nf90_def_var(imapfile, 'timestep', nf90_double, id_timedim(iid),  id_timestep(iid))
        ierr = nf90_put_att(imapfile, id_timestep(iid),  'units'        , 'seconds')
        ierr = nf90_put_att(imapfile, id_timestep(iid),  'standard_name', 'timestep')

        if(jamaps1 > 0) then
            ! Flow data on centres: water level at latest timestep
            ierr = nf90_def_var(imapfile, 's1',  nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_s1(iid))
            ierr = nf90_put_att(imapfile, id_s1(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_s1(iid),   'standard_name', 'sea_surface_height') ! sorry for inland water people
            ierr = nf90_put_att(imapfile, id_s1(iid),   'long_name'    , 'water level')
            ierr = nf90_put_att(imapfile, id_s1(iid),   'units'        , 'm')
            ierr = unc_add_gridmapping_att(imapfile, (/ id_s1(iid) /), jsferic)
        endif

        if (jaseparate_ == 0 .or. jaseparate_ == 1) then ! to mapfile
            ! Flow data on centres: water level timestep before the latest timestep
            
            if(jamaps0 > 0) then
                ierr = nf90_def_var(imapfile, 's0',  nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_s0(iid))
                ierr = nf90_put_att(imapfile, id_s0(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_s0(iid),   'standard_name', 'sea_surface_height') ! sorry for inland water people
                ierr = nf90_put_att(imapfile, id_s0(iid),   'long_name'    , 'water level at previous timestep')
                ierr = nf90_put_att(imapfile, id_s0(iid),   'units'        , 'm')
                ierr = unc_add_gridmapping_att(imapfile, (/ id_s0(iid) /), jsferic)
            endif
            
            idims(1) = id_flowelemdim(iid)
            idims(2) = id_timedim(iid)
            
            if(jamaps1 > 0) then
                call definencvar(imapfile,id_hs(iid)   ,nf90_double,idims,2, 'waterdepth'  , 'water depth', 'm', 'FlowElem_xcc FlowElem_ycc')   
            endif

            if (jamapheatflux > 0 .and. jatem > 1) then ! Heat modelling only
               call definencvar(imapfile,id_tair(iid)   ,nf90_double,idims,2, 'Tair'  , 'air temperature', 'degC', 'FlowElem_xcc FlowElem_ycc')   
               call definencvar(imapfile,id_rhum(iid)   ,nf90_double,idims,2, 'rhum'  , 'Relative humidity', ' ','FlowElem_xcc FlowElem_ycc')   
               call definencvar(imapfile,id_clou(iid)   ,nf90_double,idims,2, 'clou'  , 'cloudiness', ' ', 'FlowElem_xcc FlowElem_ycc')   

               if (jatem == 5) then 
                  call definencvar(imapfile,id_qsun(iid)   ,nf90_double,idims,2, 'Qsun'  , 'solar influx', 'W m-2', 'FlowElem_xcc FlowElem_ycc')   
                  call definencvar(imapfile,id_Qeva(iid)   ,nf90_double,idims,2, 'Qeva'  , 'evaporative heat flux', 'W m-2', 'FlowElem_xcc FlowElem_ycc')   
                  call definencvar(imapfile,id_Qcon(iid)   ,nf90_double,idims,2, 'Qcon'  , 'sensible heat flux', 'W m-2', 'FlowElem_xcc FlowElem_ycc')   
                  call definencvar(imapfile,id_Qlong(iid)  ,nf90_double,idims,2, 'Qlong' , 'long wave back radiation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')   
                  call definencvar(imapfile,id_Qfreva(iid) ,nf90_double,idims,2, 'Qfreva', 'free convection evaporative heat flux', 'W m-2', 'FlowElem_xcc FlowElem_ycc')   
                  call definencvar(imapfile,id_Qfrcon(iid) ,nf90_double,idims,2, 'Qfrcon', 'free convection sensible heat flux', 'W m-2', 'FlowElem_xcc FlowElem_ycc')   
               endif
               
               call definencvar(imapfile,id_Qtot(iid)   ,nf90_double,idims,2, 'Qtot'  , 'total heat flux', 'W m-2', 'FlowElem_xcc FlowElem_ycc')   
            endif

            if (jamapnumlimdt > 0) then
                call definencvar(imapfile,id_numlimdt(iid)  ,nf90_double,idims,2, 'numlimdt' , 'number of times flow element was Courant limiting', '1', 'FlowElem_xcc FlowElem_ycc') 
            endif

            if(jamaptaucurrent > 0) then
                ! Flow data on centres
                ierr = nf90_def_var(imapfile, 'taus' ,  nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_taus(iid))
                ierr = nf90_put_att(imapfile, id_taus(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_taus(iid),  'standard_name', 'taucurrent')
                ierr = nf90_put_att(imapfile, id_taus(iid),  'long_name'    , 'taucurrent in flow element')
                ierr = nf90_put_att(imapfile, id_taus(iid),  'units'        , 'N m-2')
            endif
            
            if (jamaptidep >0 .and. jatidep >0) then
               if ( jaselfal.eq.0 ) then
                  ierr = nf90_def_var(imapfile, 'TidalPotential', nf90_double, (/ id_flowelemdim(iid), id_timedim(iid)/), id_tidep(iid))
                  ierr = nf90_put_att(imapfile, id_tidep(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_tidep(iid),  'standard_name', 'TidalPotential')
                  ierr = nf90_put_att(imapfile, id_tidep(iid),  'long_name'    , 'TidalPotential in flow element center')
                  ierr = nf90_put_att(imapfile, id_tidep(iid),  'units'        , 'm2 s-2')
               else
                  ierr = nf90_def_var(imapfile, 'TidalPotential_without_SAL', nf90_double, (/ id_flowelemdim(iid), id_timedim(iid)/), id_tidep(iid))
                  ierr = nf90_put_att(imapfile, id_tidep(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_tidep(iid),  'standard_name', 'TidalPotential without SAL')
                  ierr = nf90_put_att(imapfile, id_tidep(iid),  'long_name'    , 'TidalPotential without SAL in flow element center')
                  ierr = nf90_put_att(imapfile, id_tidep(iid),  'units'        , 'm2 s-2')
               end if
            end if
            if (jamapselfal > 0) then
               if ( jaselfal.gt.0 ) then
                  ierr = nf90_def_var(imapfile, 'SALPotential', nf90_double, (/ id_flowelemdim(iid), id_timedim(iid)/), id_salp(iid))
                  ierr = nf90_put_att(imapfile, id_salp(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_salp(iid),  'standard_name', 'SALPotential')
                  ierr = nf90_put_att(imapfile, id_salp(iid),  'long_name'    , 'SALPotential in flow element center')
                  ierr = nf90_put_att(imapfile, id_salp(iid),  'units'        , 'm2 s-2')
               end if
            end if
            
            if (jaFrcInternalTides2D >0 .and. jamapIntTidesDiss >0) then
               ierr = nf90_def_var(imapfile, 'internal_tides_dissipation', nf90_double, (/ id_flowelemdim(iid), id_timedim(iid)/), id_IntTidesDiss(iid))
               ierr = nf90_put_att(imapfile, id_inttidesdiss(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_inttidesdiss(iid),  'standard_name', 'internal_tides_dissipation')
               ierr = nf90_put_att(imapfile, id_inttidesdiss(iid),  'long_name'    , 'internal tides dissipation in flow element center')
               ierr = nf90_put_att(imapfile, id_inttidesdiss(iid),  'units'        , 'J s-1 m-2')
            end if

            if (kmx > 0) then 
                !     3D
                if(jamapu1 > 0) then
                    ierr = nf90_def_var(imapfile, 'unorm', nf90_double, (/ id_laydim(iid), id_flowlinkdim(iid), id_timedim (iid)/) , id_unorm(iid))
                endif
                if(jamapu0 > 0) then
                    ierr = nf90_def_var(imapfile, 'u0'   , nf90_double, (/ id_laydim(iid), id_flowlinkdim(iid), id_timedim (iid)/) , id_u0(iid)   )
                endif
                if(jamapq1 > 0) then
                    ierr = nf90_def_var(imapfile, 'q1'   , nf90_double, (/ id_laydim(iid), id_flowlinkdim(iid), id_timedim (iid)/) , id_q1(iid)   )
                endif
                if(jamapq1main > 0) then
                    ierr = nf90_def_var(imapfile, 'q1main', nf90_double, (/ id_laydim(iid), id_flowlinkdim(iid), id_timedim (iid)/) , id_q1(iid)   )
                endif
                if(jamapviu > 0) then
                    ierr = nf90_def_var(imapfile, 'viu'   , nf90_double, (/ id_laydim(iid), id_flowlinkdim(iid), id_timedim (iid)/) , id_viu(iid)   )
                endif
                if(jamapdiu > 0) then
                    ierr = nf90_def_var(imapfile, 'diu'   , nf90_double, (/ id_laydim(iid), id_flowlinkdim(iid), id_timedim (iid)/) , id_diu(iid)   )
                endif

                if(jamapucvec > 0) then    
                   ! JRE Velocity vector needs to be written, irrespective of kmx, also for com file. Statements moved down outside if-clause
                   !    ierr = nf90_def_var(imapfile, 'ucx'   , nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_ucx(iid)  )
                   !    ierr = nf90_def_var(imapfile, 'ucy'   , nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_ucy(iid)  )
                    ierr = nf90_def_var(imapfile, 'ucz'  , nf90_double, (/ id_laydim(iid), id_flowelemdim(iid), id_timedim (iid)/) , id_ucz(iid)  )

                    ! Depth-averaged cell-center velocities in 3D:
                    ierr = nf90_def_var(imapfile, 'ucxa' , nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_ucxa(iid)  )
                    ierr = nf90_def_var(imapfile, 'ucya' , nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_ucya(iid)  )

                endif
                if(jamapww1 > 0) then
                    ierr = nf90_def_var(imapfile, 'ww1'  , nf90_double, (/ id_wdim(iid), id_flowelemdim(iid), id_timedim (iid)/) , id_ww1(iid))
                endif
                if(jamaprho > 0) then
                    ierr = nf90_def_var(imapfile, 'rho'  , nf90_double, (/ id_laydim(iid), id_flowelemdim(iid), id_timedim (iid)/) , id_rho(iid))
                endif
              !
                if(jamapucvec > 0) then
                  ierr = nf90_put_att(imapfile, id_ucz(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_ucz(iid),  'standard_name', 'upward_sea_water_velocity')
                  ierr = nf90_put_att(imapfile, id_ucz(iid),  'long_name'    , 'upward velocity on flow element center')
                  ierr = nf90_put_att(imapfile, id_ucz(iid),  'units'        , 'm s-1')
                  ierr = nf90_put_att(imapfile, id_ucz(iid),  '_FillValue'   , dmiss)

                  ierr = nf90_put_att(imapfile, id_ucxa(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                  if (jsferic == 0) then
                     ierr = nf90_put_att(imapfile, id_ucxa(iid),  'standard_name', 'sea_water_x_velocity')
                  else
                     ierr = nf90_put_att(imapfile, id_ucxa(iid),  'standard_name', 'eastward_sea_water_velocity')
                  end if

                  ierr = nf90_put_att(imapfile, id_ucxa(iid),  'long_name'    , 'depth-averaged velocity on flow element center, x-component')
                  ierr = nf90_put_att(imapfile, id_ucxa(iid),  'units'        , 'm s-1')

                  ierr = nf90_put_att(imapfile, id_ucya(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                  if (jsferic == 0) then
                     ierr = nf90_put_att(imapfile, id_ucya(iid),  'standard_name', 'sea_water_y_velocity')
                  else
                     ierr = nf90_put_att(imapfile, id_ucya(iid),  'standard_name', 'northward_sea_water_velocity')
                  end if
                  ierr = nf90_put_att(imapfile, id_ucya(iid),  'long_name'    , 'depth-averaged velocity on flow element center, y-component')
                  ierr = nf90_put_att(imapfile, id_ucya(iid),  'units'        , 'm s-1')
                endif
                if(jamapww1 > 0) then
                  ierr = nf90_put_att(imapfile, id_ww1(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_ww1(iid),  'standard_name', 'upward_sea_water_velocity')              ! same standard name allowed?
                  ierr = nf90_put_att(imapfile, id_ww1(iid),  'long_name'    , 'upward velocity on vertical interface')  ! (upward normal or upward)?
                  ierr = nf90_put_att(imapfile, id_ww1(iid),  'units'        , 'm s-1')
                  ierr = nf90_put_att(imapfile, id_ww1(iid),  '_FillValue'   , dmiss)
                  !?elevation
                endif
                if(jamaprho > 0) then
                  ierr = nf90_put_att(imapfile, id_rho(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_rho(iid),  'standard_name', 'sea_water_density')
                  ierr = nf90_put_att(imapfile, id_rho(iid),  'long_name'    , 'flow mass density')
                  ierr = nf90_put_att(imapfile, id_rho(iid),  'units'        , 'kg m-3')
                  ierr = nf90_put_att(imapfile, id_rho(iid),  '_FillValue'   , dmiss)
                endif
            endif  ! kmx>0

            if (kmx == 0) then
               if(jamapu1 > 0) then
                  ierr = nf90_def_var(imapfile, 'unorm' , nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_unorm(iid))
               endif
               if(jamapu0 > 0) then
                  ierr = nf90_def_var(imapfile, 'u0'    , nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_u0(iid)   )
               endif
               if(jamapq1 > 0) then
                  ierr = nf90_def_var(imapfile, 'q1'    , nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_q1(iid)   )
               endif
               if(jamapq1main > 0) then
                  ierr = nf90_def_var(imapfile, 'q1main', nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_q1main(iid)   )
               endif
               if(jamapviu > 0) then
                  ierr = nf90_def_var(imapfile, 'viu'    , nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_viu(iid)   )
               endif
               if(jamapdiu > 0) then
                  ierr = nf90_def_var(imapfile, 'diu'    , nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_diu(iid)   )
               endif
            endif

            if(jamapu1 > 0) then
               ierr = nf90_put_att(imapfile, id_unorm(iid),'coordinates'  , 'FlowLink_xu FlowLink_yu')
               ierr = nf90_put_att(imapfile, id_unorm(iid),'long_name', 'normal component of sea_water_speed')
               ierr = nf90_put_att(imapfile, id_unorm(iid),'units'        , 'm s-1')
               ierr = nf90_put_att(imapfile, id_unorm(iid),'_FillValue'   , dmiss)
            endif

            if(jamapu0 > 0) then
               ierr = nf90_put_att(imapfile, id_u0(iid)   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')
               ierr = nf90_put_att(imapfile, id_u0(iid)   ,'long_name',     'normal component of sea_water_speed at previous timestep')
               ierr = nf90_put_att(imapfile, id_u0(iid)   ,'units'        , 'm s-1')
               ierr = nf90_put_att(imapfile, id_u0(iid)   ,'_FillValue'   , dmiss)
            endif
            if(jamapq1 > 0) then
               ierr = nf90_put_att(imapfile, id_q1(iid)   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')
               !ierr = nf90_put_att(imapfile, id_q1(iid)   ,'standard_name', 'discharge') ! not CF
               ierr = nf90_put_att(imapfile, id_q1(iid)   ,'long_name'    , 'flow flux')
               ierr = nf90_put_att(imapfile, id_q1(iid)   ,'units'        , 'm3 s-1')
               ierr = nf90_put_att(imapfile, id_q1(iid)   ,'_FillValue'   , dmiss)
            endif
            if(jamapq1main > 0) then
               ierr = nf90_put_att(imapfile, id_q1(iid)   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')
               !ierr = nf90_put_att(imapfile, id_q1(iid)   ,'standard_name', 'discharge') ! not CF
               ierr = nf90_put_att(imapfile, id_q1(iid)   ,'long_name'    , 'flow flux in main channel')
               ierr = nf90_put_att(imapfile, id_q1(iid)   ,'units'        , 'm3 s-1')
               ierr = nf90_put_att(imapfile, id_q1(iid)   ,'_FillValue'   , dmiss)
            endif

            if(jamapviu > 0) then
               ierr = nf90_put_att(imapfile, id_viu(iid)   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')
               ierr = nf90_put_att(imapfile, id_viu(iid)   ,'long_name',     'horizontal viscosity')
               ierr = nf90_put_att(imapfile, id_viu(iid)   ,'units'        , 'm2 s-1')
               ierr = nf90_put_att(imapfile, id_viu(iid)   ,'_FillValue'   , dmiss)
            endif
            if(jamapdiu > 0) then
               ierr = nf90_put_att(imapfile, id_diu(iid)   ,'coordinates'  , 'FlowLink_xu FlowLink_yu')
               ierr = nf90_put_att(imapfile, id_diu(iid)   ,'long_name',     'horizontal diffusivity')
               ierr = nf90_put_att(imapfile, id_diu(iid)   ,'units'        , 'm2 s-1')
               ierr = nf90_put_att(imapfile, id_diu(iid)   ,'_FillValue'   , dmiss)
            endif
        endif   ! jaseparate =/ 2
        !
        if (kmx==0) then
           if(jamapucvec > 0) then
               ierr = nf90_def_var(imapfile, 'ucx'   , nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_ucx(iid)  )
               ierr = nf90_def_var(imapfile, 'ucy'   , nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_ucy(iid)  )
           endif
        else
           if (jamapucvec > 0) then
              ierr = nf90_def_var(imapfile, 'ucx'  , nf90_double, (/ id_laydim(iid), id_flowelemdim(iid), id_timedim (iid)/) , id_ucx(iid)  )
              ierr = nf90_def_var(imapfile, 'ucy'  , nf90_double, (/ id_laydim(iid), id_flowelemdim(iid), id_timedim (iid)/) , id_ucy(iid)  )
           endif
        endif

        if(jamapucvec > 0) then
            ierr = nf90_put_att(imapfile, id_ucx(iid)  ,'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            if (jsferic == 0) then
               ierr = nf90_put_att(imapfile, id_ucx(iid)  ,'standard_name', 'sea_water_x_velocity')
            else
               ierr = nf90_put_att(imapfile, id_ucx(iid)  ,'standard_name', 'eastward_sea_water_velocity')
            end if

            if (jaeulervel==0 .or. jaseparate_==2) then
               ierr = nf90_put_att(imapfile, id_ucx(iid)  ,'long_name'    , 'velocity on flow element center, x-component')
            else
               ierr = nf90_put_att(imapfile, id_ucx(iid)  ,'long_name'    , 'Eulerian velocity on flow element center, x-component')
            endif
            ierr = nf90_put_att(imapfile, id_ucx(iid)  ,'units'        , 'm s-1')
            ierr = nf90_put_att(imapfile, id_ucx(iid)  ,'_FillValue'   , dmiss)

            ierr = nf90_put_att(imapfile, id_ucy(iid)  ,'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
            if (jsferic == 0) then
               ierr = nf90_put_att(imapfile, id_ucy(iid)  ,'standard_name', 'sea_water_y_velocity')
            else
               ierr = nf90_put_att(imapfile, id_ucy(iid)  ,'standard_name', 'northward_sea_water_velocity')
            end if
               
            if (jaeulervel==0 .or. jaseparate_==2) then
               ierr = nf90_put_att(imapfile, id_ucy(iid)  ,'long_name'    , 'velocity on flow element center, y-component')
            else
               ierr = nf90_put_att(imapfile, id_ucy(iid)  ,'long_name'    , 'Eulerian velocity on flow element center, y-component')
            endif
            ierr = nf90_put_att(imapfile, id_ucy(iid)  ,'units'        , 'm s-1')
            ierr = nf90_put_att(imapfile, id_ucy(iid)  ,'_FillValue'   , dmiss)
        endif
        
        if (jaseparate_ /= 2) then
           if (jamapsal > 0 .and. jasal > 0) then 
              if ( kmx > 0 ) then  !        3D
                 ierr = nf90_def_var(imapfile, 'sa1' , nf90_double, (/ id_laydim(iid), id_flowelemdim (iid), id_timedim (iid)/) , id_sa1(iid))
              else
                 ierr = nf90_def_var(imapfile, 'sa1' , nf90_double, (/ id_flowelemdim (iid), id_timedim (iid)/) , id_sa1(iid))
              end if
              ierr = nf90_put_att(imapfile, id_sa1(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
              ierr = nf90_put_att(imapfile, id_sa1(iid),  'standard_name', 'sea_water_salinity')
              ierr = nf90_put_att(imapfile, id_sa1(iid),  'long_name'    , 'salinity')
              ierr = nf90_put_att(imapfile, id_sa1(iid),  'units'        , '1e-3')
              ierr = nf90_put_att(imapfile, id_sa1(iid),  '_FillValue'   , dmiss)
           endif
 
           if (jamaptem > 0 .and. jatem > 0) then 
              if ( kmx > 0 ) then !        3D
                ierr = nf90_def_var(imapfile, 'tem1' , nf90_double, (/ id_laydim(iid), id_flowelemdim(iid) , id_timedim(iid) /) , id_tem1(iid))
              else
                ierr = nf90_def_var(imapfile, 'tem1' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_tem1(iid))
              end if
              ierr = nf90_put_att(imapfile, id_tem1(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
              ierr = nf90_put_att(imapfile, id_tem1(iid),  'standard_name', 'sea_water_temperature')
              ierr = nf90_put_att(imapfile, id_tem1(iid),  'long_name'    , 'temperature')
              ierr = nf90_put_att(imapfile, id_tem1(iid),  'units'        , 'degC')
              ierr = nf90_put_att(imapfile, id_tem1(iid),  '_FillValue'   , dmiss)
           endif

!          tracers
           if (jamapconst > 0 .and. ITRA1 > 0) then
              do j=ITRA1,ITRAN
                 tmpstr = const_names(j)
                 ! Forbidden chars in NetCDF names: space, /, and more.
                 call replace_char(tmpstr,32,95) 
                 call replace_char(tmpstr,47,95) 
                 if ( kmx > 0 ) then  !        3D
                    ierr = nf90_def_var(imapfile, trim(tmpstr), nf90_double, (/ id_laydim(iid), id_flowelemdim (iid), id_timedim (iid)/) , id_const(iid,j))
                 else
                    ierr = nf90_def_var(imapfile, trim(tmpstr), nf90_double, (/ id_flowelemdim (iid), id_timedim (iid)/) , id_const(iid,j))
                 end if
                 ierr = nf90_put_att(imapfile, id_const(iid,j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_const(iid,j),  'standard_name', trim(tmpstr))
                 ierr = nf90_put_att(imapfile, id_const(iid,j),  'long_name'    , trim(tmpstr))
                 if (const_units(j).ne.' ') then
                    tmpstr = const_units(j)
                 else
                    tmpstr = '1e-3'
                 endif
                 ierr = nf90_put_att(imapfile, id_const(iid,j),  'units'        , tmpstr)
                 ierr = nf90_put_att(imapfile, id_const(iid,j),  '_FillValue'   , dmiss)
              end do
           endif

!          water quality bottom variables
           if (numwqbots > 0) then
              call realloc(id_wqb, (/ 3, numwqbots /), keepExisting=.false., fill = 0)
              do j=1,numwqbots
                 tmpstr = wqbotnames(j)
                 ! Forbidden chars in NetCDF names: space, /, and more.
                 call replace_char(tmpstr,32,95) 
                 call replace_char(tmpstr,47,95) 
                 ierr = nf90_def_var(imapfile, trim(tmpstr), nf90_double, (/ id_flowelemdim (iid), id_timedim (iid)/) , id_wqb(iid,j))
                 ierr = nf90_put_att(imapfile, id_wqb(iid,j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_wqb(iid,j),  'standard_name', trim(tmpstr))
                 ierr = nf90_put_att(imapfile, id_wqb(iid,j),  'long_name'    , trim(tmpstr))
                 tmpstr = wqbotunits(j)
                 ierr = nf90_put_att(imapfile, id_wqb(iid,j),  'units'        , tmpstr)
                 ierr = nf90_put_att(imapfile, id_wqb(iid,j),  '_FillValue'   , dmiss)
              end do
           endif

!          waq output
           if(jawaqproc > 0) then
              if (noout_map > 0) then
                 call realloc(id_waq, (/ 3, noout_map /), keepExisting=.false., fill = 0)
                 do j=1,noout_map
                    tmpstr = ' '
                    write (tmpstr, "('water_quality_output_',I0)") j
                    if ( kmx > 0 ) then  !        3D
                       ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/ id_laydim(iid), id_flowelemdim (iid), id_timedim (iid)/) , id_waq(iid,j))
                    else
                       ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/ id_flowelemdim (iid), id_timedim (iid)/) , id_waq(iid,j))
                    end if
                    tmpstr = trim(outputs%names(j))//' - '//trim(outputs%descrs(j))//' in flow element'
                    call replace_multiple_spaces_by_single_spaces(tmpstr)
                    ierr = nf90_put_att(imapfile, id_waq(iid,j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                    ierr = nf90_put_att(imapfile, id_waq(iid,j),  'long_name'    , trim(outputs%names(j)))
                    ierr = nf90_put_att(imapfile, id_waq(iid,j),  'units'        , trim(outputs%units(j)))
                    ierr = nf90_put_att(imapfile, id_waq(iid,j),  'description'  , tmpstr)
                    ierr = nf90_put_att(imapfile, id_waq(iid,j),  '_FillValue'   , dmiss)
                 end do
              endif
              if (noout_statt > 0) then
                 call realloc(id_wqst, (/ 3, noout_statt /), keepExisting=.false., fill = 0)
                 do j=1,noout_statt
                    jj = noout_user + j
                    tmpstr = ' '
                    write (tmpstr, "('water_quality_stat_',I0)") j
                    if ( kmx > 0 ) then  !        3D
                       ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/ id_laydim(iid), id_flowelemdim (iid), id_timedim (iid)/) , id_wqst(iid,j))
                    else
                       ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/ id_flowelemdim (iid), id_timedim (iid)/) , id_wqst(iid,j))
                    end if
                    tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%descrs(jj))//' in flow element'
                    call replace_multiple_spaces_by_single_spaces(tmpstr)
                    ierr = nf90_put_att(imapfile, id_wqst(iid,j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                    ierr = nf90_put_att(imapfile, id_wqst(iid,j),  'long_name'    , trim(outputs%names(jj)))
                    ierr = nf90_put_att(imapfile, id_wqst(iid,j),  'units'        , trim(outputs%units(jj)))
                    ierr = nf90_put_att(imapfile, id_wqst(iid,j),  'description'  , tmpstr)
                    ierr = nf90_put_att(imapfile, id_wqst(iid,j),  '_FillValue'   , dmiss)
                 end do
              endif
              if (noout_state > 0) then
                 call realloc(id_wqse, (/ 3, noout_state /), keepExisting=.false., fill = 0)
                 do j=1,noout_state
                    jj = noout_user + noout_statt + j
                    tmpstr = ' '
                    write (tmpstr, "('water_quality_stat_',I0)") noout_statt + j
                    if ( kmx > 0 ) then  !        3D
                       ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/ id_laydim(iid), id_flowelemdim (iid)/) , id_wqse(iid,j))
                    else
                       ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/ id_flowelemdim (iid)/) , id_wqse(iid,j))
                    end if
                    tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%descrs(jj))//' in flow element'
                    call replace_multiple_spaces_by_single_spaces(tmpstr)
                    ierr = nf90_put_att(imapfile, id_wqse(iid,j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                    ierr = nf90_put_att(imapfile, id_wqse(iid,j),  'long_name'    , trim(outputs%names(jj)))
                    ierr = nf90_put_att(imapfile, id_wqse(iid,j),  'units'        , trim(outputs%units(jj)))
                    ierr = nf90_put_att(imapfile, id_wqse(iid,j),  'description'  , tmpstr)
                    ierr = nf90_put_att(imapfile, id_wqse(iid,j),  '_FillValue'   , dmiss)
                 end do
              endif
           endif 

           ! water quality mass balance areas
           if (nomba > 0) then
              ierr = nf90_def_var(imapfile,  'water_quality_mba', nf90_int, (/ id_flowelemdim (iid) /) , id_mba(iid))
              ierr = nf90_put_att(imapfile, id_mba(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
              ierr = nf90_put_att(imapfile, id_mba(iid),  'long_name'    , 'Water quality mass balance areas')
              ierr = unc_add_gridmapping_att(imapfile, (/ id_mba(iid) /), jsferic)
              call realloc(flag_val, nomba, keepExisting = .false., fill = 0)
              flag_mean = ' '
              do j=nomba,1,-1
                 flag_val(j) = j
                 flag_mean = trim(mbaname(j))//' '//flag_mean
              enddo
              ierr = nf90_put_att(imapfile, id_mba(iid), 'flag_values', flag_val)
              ierr = nf90_put_att(imapfile, id_mba(iid), 'flag_meanings', flag_mean)
           endif

           if ( jasecflow > 0 .and. jamapspir > 0) then
              if (kmx < 2) then
                 ierr = nf90_def_var(imapfile, 'spircrv' , nf90_double, (/ id_flowelemdim (iid), id_timedim (iid) /) , id_spircrv(iid))
                 ierr = nf90_put_att(imapfile, id_spircrv(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_spircrv(iid),  'long_name'    , 'streamline curvature')
                 ierr = nf90_put_att(imapfile, id_spircrv(iid),  'units'        , 'm-1')
              endif
              ierr = nf90_def_var(imapfile, 'spirint' , nf90_double, (/ id_flowelemdim (iid), id_timedim (iid) /) , id_spirint(iid))
              ierr = nf90_put_att(imapfile, id_spirint(iid)  ,'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
              ierr = nf90_put_att(imapfile, id_spirint(iid)  ,'long_name'    , 'Spiral flow intensity')
              ierr = nf90_put_att(imapfile, id_spirint(iid)  ,'units'        , 'm/s')
           endif


           if (jamaptur > 0 .and. kmx > 0) then
              if ( iturbulencemodel >= 3 ) then
                 ierr = nf90_def_var(imapfile, 'turkin1' , nf90_double, (/ id_wdim(iid), id_flowlinkdim(iid) , id_timedim(iid) /) , id_turkin1(iid))
                 ierr = nf90_put_att(imapfile, id_turkin1(iid),  'coordinates'  , 'FlowLink_xu FlowLink_yu')
                 ierr = nf90_put_att(imapfile, id_turkin1(iid),  'standard_name', 'specific_turbulent_kinetic_energy_of_sea_water')
                 ierr = nf90_put_att(imapfile, id_turkin1(iid),  'long_name'    , 'turbulent kinetic energy')
                 ierr = nf90_put_att(imapfile, id_turkin1(iid),  'units'        , 'm2 s-2')
                 ierr = nf90_put_att(imapfile, id_turkin1(iid),  '_FillValue'   , dmiss)
                 
                 ierr = nf90_def_var(imapfile, 'vicwwu' , nf90_double, (/ id_wdim(iid), id_flowlinkdim(iid) , id_timedim(iid) /) , id_vicwwu(iid))
                 ierr = nf90_put_att(imapfile, id_vicwwu(iid),  'coordinates'  , 'FlowLink_xu FlowLink_yu')
                 ierr = nf90_put_att(imapfile, id_vicwwu(iid),  'long_name'    , 'turbulent vertical eddy viscosity')
                 ierr = nf90_put_att(imapfile, id_vicwwu(iid),  'units'        , 'm2 s-1')
                 ierr = nf90_put_att(imapfile, id_vicwwu(iid),  '_FillValue'   , dmiss)
                 
                 ierr = nf90_def_var(imapfile, 'tureps1' , nf90_double, (/ id_wdim(iid), id_flowlinkdim(iid) , id_timedim(iid) /) , id_tureps1(iid))
                 ierr = nf90_put_att(imapfile, id_tureps1(iid),  'coordinates'  , 'FlowLink_xu FlowLink_yu')
                 ierr = nf90_put_att(imapfile, id_tureps1(iid),  '_FillValue'   , dmiss)
                 
                 if( iturbulencemodel == 3 ) then
                    ierr = nf90_put_att(imapfile, id_tureps1(iid),  'standard_name', 'specific_turbulent_kinetic_energy_dissipation_in_sea_water')
                    ierr = nf90_put_att(imapfile, id_tureps1(iid),  'long_name'    , 'turbulent energy dissipation')
                    ierr = nf90_put_att(imapfile, id_tureps1(iid),  'units'        , 'm2 s-3')
                 else if( iturbulencemodel == 4 ) then
                    ierr = nf90_put_att(imapfile, id_tureps1(iid),  'long_name'    , 'turbulent time scale')
                    ierr = nf90_put_att(imapfile, id_tureps1(iid),  'units'        , 's-1')
                 endif
              endif
           endif

           if (jamapsed > 0 .and. stm_included) then
              ierr = nf90_def_dim(imapfile, 'nSedTot', stmpar%lsedtot, id_sedtotdim(iid))
              ierr = nf90_def_dim(imapfile, 'nSedSus', stmpar%lsedsus, id_sedsusdim(iid))
              ierr = nf90_def_dim(imapfile, 'nBedLayers', stmpar%morlyr%settings%nlyr, id_nlyrdim(iid))
              !
              select case(stmpar%morpar%moroutput%transptype)
                 case (0)
                    transpunit = 'kg/(s m)'
                 case (1)
                    transpunit = 'm3/(s m)'
                 case (2)
                    transpunit = 'm3/(s m)'
              end select
              !
              ! fall velocity
              if (stmpar%lsedsus > 0) then
                 if (kmx > 0) then
                    ierr = nf90_def_var(imapfile, 'ws', nf90_double, (/ id_laydim(iid), id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /), id_ws(iid))
                 else ! '2D' fall velocity, ref fm_erosed(), to check...
                    ierr = nf90_def_var(imapfile, 'ws', nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /), id_ws(iid))
                 end if
                 ierr = nf90_put_att(imapfile, id_ws(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_ws(iid) ,  'long_name'    , 'Sediment settling velocity')
                 ierr = nf90_put_att(imapfile, id_ws(iid) ,  'units'        , 'm s-1')
                 !
                 ! equilibrium concentration, 2D only
                 if (kmx == 0) then
                    ierr = nf90_def_var(imapfile, 'rsedeq', nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /), id_rsedeq(iid))
                    ierr = nf90_put_att(imapfile, id_rsedeq(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                    ierr = nf90_put_att(imapfile, id_rsedeq(iid) ,  'long_name'    , 'Equilibrium sediment concentration')
                    ierr = nf90_put_att(imapfile, id_rsedeq(iid) ,  'units'        , 'kg m-3')
                 end if
                 !
                 ! reference height
                 if (stmpar%morpar%moroutput%aks) then
                    ierr = nf90_def_var(imapfile, 'aks', nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /), id_aks(iid))
                    ierr = nf90_put_att(imapfile, id_aks(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                    ierr = nf90_put_att(imapfile, id_aks(iid) ,  'long_name'    , 'Near-bed reference concentration height')
                    ierr = nf90_put_att(imapfile, id_aks(iid) ,  'units'        , 'm')
                    
                    ierr = nf90_def_var(imapfile, 'rca', nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /), id_rca(iid))
                    ierr = nf90_put_att(imapfile, id_rca(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                    ierr = nf90_put_att(imapfile, id_rca(iid) ,  'long_name'    , 'Near-bed reference concentration')
                    ierr = nf90_put_att(imapfile, id_rca(iid) ,  'units'        , 'kg m-3')
                 end if
                 
                 if (stmpar%morpar%moroutput%sourcesink) then
                    ierr = nf90_def_var(imapfile, 'sourse' , nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_sourse(iid))
                    ierr = nf90_put_att(imapfile, id_sourse(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                    ierr = nf90_put_att(imapfile, id_sourse(iid) ,  'long_name'    , 'Source term suspended sediment fractions')
                    ierr = nf90_put_att(imapfile, id_sourse(iid) ,  'units'        , 'kg/(m3 s)')
                    
                    ierr = nf90_def_var(imapfile, 'sinkse' , nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_sinkse(iid))
                    ierr = nf90_put_att(imapfile, id_sinkse(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                    ierr = nf90_put_att(imapfile, id_sinkse(iid) ,  'long_name'    , 'Sink term suspended sediment fractions')
                    ierr = nf90_put_att(imapfile, id_sinkse(iid) ,  'units'        , 's-1')
                 endif
                 
                 if (stmpar%morpar%moroutput%suvcor) then
                    ierr = nf90_def_var(imapfile, 'e_scrn' , nf90_double, (/ id_flowlinkdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_scrn(iid))
                    ierr = nf90_put_att(imapfile, id_scrn(iid) ,  'coordinates'  , 'FlowLink_xu FlowLink_yu')
                    ierr = nf90_put_att(imapfile, id_scrn(iid) ,  'long_name'    , 'Near-bed transport correction in face-normal direction')
                    ierr = nf90_put_att(imapfile, id_scrn(iid) ,  'units'        , transpunit)
                    
                    !ierr = nf90_def_var(imapfile, 'e_scrt' , nf90_double, (/ id_flowlinkdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_scrt(iid))
                    !ierr = nf90_put_att(imapfile, id_scrt(iid) ,  'coordinates'  , 'FlowLink_xu FlowLink_yu')
                    !ierr = nf90_put_att(imapfile, id_scrt(iid) ,  'long_name'    , 'Near-bed transport correction face-tangential direction')
                    !ierr = nf90_put_att(imapfile, id_scrt(iid) ,  'units'        , transpunit)
                 end if
                 !
                 ! Suspended fractions
                 !
                 do j=ISED1,ISEDN
                    tmpstr = const_names(j)
                    ! Forbidden chars in NetCDF names: space, /, and more.
                    call replace_char(tmpstr,32,95) 
                    call replace_char(tmpstr,47,95) 
                    if ( kmx > 0 ) then  !        3D
                       ierr = nf90_def_var(imapfile, trim(tmpstr), nf90_double, (/ id_laydim(iid), id_flowelemdim (iid), id_timedim (iid)/) , id_const(iid,j))
                    else
                       ierr = nf90_def_var(imapfile, trim(tmpstr), nf90_double, (/ id_flowelemdim (iid), id_timedim (iid)/) , id_const(iid,j))
                    end if
                    ierr = nf90_put_att(imapfile, id_const(iid,j),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                    ierr = nf90_put_att(imapfile, id_const(iid,j),  'standard_name', trim(tmpstr)//' concentration')
                    ierr = nf90_put_att(imapfile, id_const(iid,j),  'long_name'    , trim(tmpstr)//' concentration')
                    ierr = nf90_put_att(imapfile, id_const(iid,j),  'units'        , 'kg m-3')
                 end do  
              end if
              
              if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
                 ierr = nf90_def_var(imapfile, 'e_dzdn', nf90_double, (/ id_flowlinkdim(iid) , id_timedim(iid) /), id_dzdn(iid))
                 ierr = nf90_put_att(imapfile, id_dzdn(iid) ,  'coordinates'  , 'FlowLink_xu FlowLink_yu')
                 ierr = nf90_put_att(imapfile, id_dzdn(iid) ,  'long_name'    , 'Bed slope parallel to flow link')
                 ierr = nf90_put_att(imapfile, id_dzdn(iid) ,  'units'        , '-')
                 
                 ierr = nf90_def_var(imapfile, 'e_dzdt', nf90_double, (/ id_flowlinkdim(iid) , id_timedim(iid) /), id_dzdt(iid))
                 ierr = nf90_put_att(imapfile, id_dzdt(iid) ,  'coordinates'  , 'FlowLink_xu FlowLink_yu')
                 ierr = nf90_put_att(imapfile, id_dzdt(iid) ,  'long_name'    , 'Bed slope normal to flow link')
                 ierr = nf90_put_att(imapfile, id_dzdt(iid) ,  'units'        , '-')
              end if
              
              if (stmpar%morpar%moroutput%umod) then
                 ierr = nf90_def_var(imapfile, 'umod', nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /), id_umod(iid))
                 ierr = nf90_put_att(imapfile, id_umod(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_umod(iid) ,  'long_name'    , 'Characteristic velocity magnitude in cell centre')
                 ierr = nf90_put_att(imapfile, id_umod(iid) ,  'units'        , 'm s-1')
              end if
              
              if (stmpar%morpar%moroutput%zumod) then
                 ierr = nf90_def_var(imapfile, 'zumod', nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /), id_zumod(iid))
                 ierr = nf90_put_att(imapfile, id_zumod(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_zumod(iid) ,  'long_name'    , 'Height above bed for characteristic velocity in cell centre')
                 ierr = nf90_put_att(imapfile, id_zumod(iid) ,  'units'        , 'm')
              end if
              
              if (stmpar%morpar%moroutput%ustar) then
                 ierr = nf90_def_var(imapfile, 'ustar', nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /), id_ustar(iid))
                 ierr = nf90_put_att(imapfile, id_ustar(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_ustar(iid) ,  'long_name'    , 'Bed shear velocity u* in cell centre')
                 ierr = nf90_put_att(imapfile, id_ustar(iid) ,  'units'        , 'm s-1')
              end if

              if (stmpar%morpar%moroutput%sbcuv) then
                 ierr = nf90_def_var(imapfile, 'sbcx' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sbcx(iid))
                 ierr = nf90_put_att(imapfile, id_sbcx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sbcx(iid) ,  'long_name'    , 'bed load transport due to currents, x-component')
                 ierr = nf90_put_att(imapfile, id_sbcx(iid) ,  'units'        , transpunit)

                 ierr = nf90_def_var(imapfile, 'sbcy' , nf90_double, (/ id_flowelemdim (iid), id_sedtotdim(iid), id_timedim (iid)/) , id_sbcy(iid))
                 ierr = nf90_put_att(imapfile, id_sbcy (iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sbcy (iid),  'long_name'    , 'bed load transport due to currents, y-component')
                 ierr = nf90_put_att(imapfile, id_sbcy (iid),  'units'        , transpunit)

                 ierr = nf90_def_var(imapfile, 'sbcx_reconstructed' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sbcx_reconstructed(iid))
                 ierr = nf90_put_att(imapfile, id_sbcx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sbcx(iid) ,  'long_name'    , 'bed load transport due to currents (reconstructed), x-component')
                 ierr = nf90_put_att(imapfile, id_sbcx(iid) ,  'units'        , transpunit)

                 ierr = nf90_def_var(imapfile, 'sbcy_reconstructed' , nf90_double, (/ id_flowelemdim (iid), id_sedtotdim(iid), id_timedim (iid)/) , id_sbcy_reconstructed(iid))
                 ierr = nf90_put_att(imapfile, id_sbcy (iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sbcy (iid),  'long_name'    , 'bed load transport due to currents (reconstructed), y-component')
                 ierr = nf90_put_att(imapfile, id_sbcy (iid),  'units'        , transpunit)
              endif

              if (stmpar%morpar%moroutput%sbwuv) then
                 ierr = nf90_def_var(imapfile, 'sbwx' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sbwx(iid))
                 ierr = nf90_put_att(imapfile, id_sbwx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sbwx(iid) ,  'long_name'    , 'bed load transport due to waves, x-component')
                 ierr = nf90_put_att(imapfile, id_sbwx(iid) ,  'units'        , transpunit)
       
                 ierr = nf90_def_var(imapfile, 'sbwy' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sbwy(iid))
                 ierr = nf90_put_att(imapfile, id_sbwy(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sbwy(iid) ,  'long_name'    , 'bed load transport due to waves, y-component')
                 ierr = nf90_put_att(imapfile, id_sbwy(iid) ,  'units'        , transpunit)

                 ierr = nf90_def_var(imapfile, 'sbwx_reconstructed' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sbwx_reconstructed(iid))
                 ierr = nf90_put_att(imapfile, id_sbwx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sbwx(iid) ,  'long_name'    , 'bed load transport due to waves (reconstructed), x-component')
                 ierr = nf90_put_att(imapfile, id_sbwx(iid) ,  'units'        , transpunit)
       
                 ierr = nf90_def_var(imapfile, 'sbwy_reconstructed' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sbwy_reconstructed(iid))
                 ierr = nf90_put_att(imapfile, id_sbwy(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sbwy(iid) ,  'long_name'    , 'bed load transport due to waves (reconstructed), y-component')
                 ierr = nf90_put_att(imapfile, id_sbwy(iid) ,  'units'        , transpunit)
              endif

              if (stmpar%morpar%moroutput%sswuv) then
                 ierr = nf90_def_var(imapfile, 'sswx' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sswx(iid))
                 ierr = nf90_put_att(imapfile, id_sswx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sswx(iid) ,  'long_name'    , 'suspended load transport due to waves, x-component')
                 ierr = nf90_put_att(imapfile, id_sswx(iid) ,  'units'        , transpunit)
          
                 ierr = nf90_def_var(imapfile, 'sswy' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sswy(iid))
                 ierr = nf90_put_att(imapfile, id_sswy(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sswy(iid) ,  'long_name'    , 'suspended load transport due to waves, y-component')
                 ierr = nf90_put_att(imapfile, id_sswy(iid) ,  'units'        , transpunit)

                 ierr = nf90_def_var(imapfile, 'sswx_reconstructed' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sswx_reconstructed(iid))
                 ierr = nf90_put_att(imapfile, id_sswx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sswx(iid) ,  'long_name'    , 'suspended load transport due to waves (reconstructed), x-component')
                 ierr = nf90_put_att(imapfile, id_sswx(iid) ,  'units'        , transpunit)
          
                 ierr = nf90_def_var(imapfile, 'sswy_reconstructed' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sswy_reconstructed(iid))
                 ierr = nf90_put_att(imapfile, id_sswy(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sswy(iid) ,  'long_name'    , 'suspended load transport due to waves (reconstructed), y-component')
                 ierr = nf90_put_att(imapfile, id_sswy(iid) ,  'units'        , transpunit)
              endif
              
              if (stmpar%morpar%moroutput%sscuv) then
                 ierr = nf90_def_var(imapfile, 'sscx' , nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_sscx(iid))
                 ierr = nf90_put_att(imapfile, id_sscx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sscx(iid) ,  'long_name'    , 'suspended load transport due to currents, x-component')
                 ierr = nf90_put_att(imapfile, id_sscx(iid) ,  'units'        , transpunit)
          
                 ierr = nf90_def_var(imapfile, 'sscy' , nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_sscy(iid))
                 ierr = nf90_put_att(imapfile, id_sscy(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sscy(iid) ,  'long_name'    , 'suspended load transport due to currents, y-component')
                 ierr = nf90_put_att(imapfile, id_sscy(iid) ,  'units'        , transpunit)

                 ierr = nf90_def_var(imapfile, 'sscx_reconstructed' , nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_sscx_reconstructed(iid))
                 ierr = nf90_put_att(imapfile, id_sscx(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sscx(iid) ,  'long_name'    , 'suspended load transport due to currents (reconstructed), x-component')
                 ierr = nf90_put_att(imapfile, id_sscx(iid) ,  'units'        , transpunit)
          
                 ierr = nf90_def_var(imapfile, 'sscy_reconstructed' , nf90_double, (/ id_flowelemdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_sscy_reconstructed(iid))
                 ierr = nf90_put_att(imapfile, id_sscy(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sscy(iid) ,  'long_name'    , 'suspended load transport due to currents (reconstructed), y-component')
                 ierr = nf90_put_att(imapfile, id_sscy(iid) ,  'units'        , transpunit)
              endif
              
              ierr = nf90_def_var(imapfile, 'sxtot' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sxtot(iid))
              ierr = nf90_put_att(imapfile, id_sxtot(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
              ierr = nf90_put_att(imapfile, id_sxtot(iid) ,  'long_name'    , 'total sediment transport in flow cell center, x-component')
              ierr = nf90_put_att(imapfile, id_sxtot(iid) ,  'units'        , transpunit)
              
              ierr = nf90_def_var(imapfile, 'sytot' , nf90_double, (/ id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_sytot(iid))
              ierr = nf90_put_att(imapfile, id_sytot(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
              ierr = nf90_put_att(imapfile, id_sytot(iid) ,  'long_name'    , 'total sediment transport in flow cell center, y-component')
              ierr = nf90_put_att(imapfile, id_sytot(iid) ,  'units'        , transpunit)
              
              ierr = nf90_def_var(imapfile, 'mor_bl' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_morbl(iid))
              ierr = nf90_put_att(imapfile, id_morbl(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
              ierr = nf90_put_att(imapfile, id_morbl(iid) ,  'long_name'    , 'Time-varying bottom level in flow cell center')
              ierr = nf90_put_att(imapfile, id_morbl(iid) ,  'units'        , 'm')

              
              select case (stmpar%morlyr%settings%iunderlyr)
              case (1)
                 ierr = nf90_def_var(imapfile, 'bodsed' , nf90_double, (/ id_sedtotdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /) , id_bodsed(iid))
                 ierr = nf90_put_att(imapfile, id_bodsed(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_bodsed(iid) ,  'long_name'    , 'available sediment in the bed in flow cell center')
                 ierr = nf90_put_att(imapfile, id_bodsed(iid) ,  'units'        , 'kg m-2')

                 ierr = nf90_def_var(imapfile, 'dpsed' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_dpsed(iid))
                 ierr = nf90_put_att(imapfile, id_dpsed(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_dpsed(iid) ,  'long_name'    , 'sediment thickness in the bed in flow cell center')
                 ierr = nf90_put_att(imapfile, id_dpsed(iid) ,  'units'        , 'm')
              case (2)
                 ierr = nf90_def_var(imapfile, 'msed' , nf90_double, (/ id_sedtotdim(iid) , id_nlyrdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /) , id_msed(iid))
                 ierr = nf90_put_att(imapfile, id_msed(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_msed(iid) ,  'long_name'    , 'available sediment in a layer of the bed in flow cell center')
                 ierr = nf90_put_att(imapfile, id_msed(iid) ,  'units'        , 'kg m-2')

                 ierr = nf90_def_var(imapfile, 'lyrfrac' , nf90_double, (/ id_sedtotdim(iid) , id_nlyrdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /) , id_lyrfrac(iid))
                 ierr = nf90_put_att(imapfile, id_lyrfrac(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_lyrfrac(iid) ,  'long_name'    , 'volume fraction in a layer of the bed in flow cell center')
                 ierr = nf90_put_att(imapfile, id_lyrfrac(iid) ,  'units'        , '-')

                 ierr = nf90_def_var(imapfile, 'thlyr' , nf90_double, (/ id_nlyrdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /) , id_thlyr(iid))
                 ierr = nf90_put_att(imapfile, id_thlyr(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_thlyr(iid) ,  'long_name'    , 'thickness of a layer of the bed in flow cell center')
                 ierr = nf90_put_att(imapfile, id_thlyr(iid) ,  'units'        , 'm')

                 if (stmpar%morlyr%settings%iporosity>0) then
                    ierr = nf90_def_var(imapfile, 'poros' , nf90_double, (/ id_nlyrdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /) , id_poros(iid))
                    ierr = nf90_put_att(imapfile, id_poros(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                    ierr = nf90_put_att(imapfile, id_poros(iid) ,  'long_name'    , 'porosity of a layer of the bed in flow cell center')
                    ierr = nf90_put_att(imapfile, id_poros(iid) ,  'units'        , '-')
                 endif
              end select

              if (stmpar%morpar%moroutput%taurat) then
                 ierr = nf90_def_var(imapfile, 'taurat' , nf90_double, (/id_flowelemdim(iid) , id_sedtotdim(iid) ,id_timedim(iid) /) , id_taurat(iid))
                 ierr = nf90_put_att(imapfile, id_taurat(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_taurat(iid) ,  'long_name'    , 'Excess bed shear ratio')
                 ierr = nf90_put_att(imapfile, id_taurat(iid) ,  'units'        , '-')
              endif
              if (stmpar%morpar%moroutput%dm) then
                 ierr = nf90_def_var(imapfile, 'dm' , nf90_double, (/id_flowelemdim(iid) , id_timedim(iid) /) , id_dm(iid))
                 ierr = nf90_put_att(imapfile, id_dm(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_dm(iid) ,  'long_name'    , 'Arithmetic mean sediment diameter')
                 ierr = nf90_put_att(imapfile, id_dm(iid) ,  'units'        , 'm')
              endif
              if (stmpar%morpar%moroutput%dg) then
                 ierr = nf90_def_var(imapfile, 'dg' , nf90_double, (/id_flowelemdim(iid) , id_timedim(iid) /) , id_dg(iid))
                 ierr = nf90_put_att(imapfile, id_dg(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_dg(iid) ,  'long_name'    , 'Geometric mean sediment diameter')
                 ierr = nf90_put_att(imapfile, id_dg(iid) ,  'units'        , 'm')
              endif
              if (stmpar%morpar%moroutput%dgsd) then
                 ierr = nf90_def_var(imapfile, 'dgsd' , nf90_double, (/id_flowelemdim(iid) , id_timedim(iid) /) , id_dgsd(iid))
                 ierr = nf90_put_att(imapfile, id_dgsd(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_dgsd(iid) ,  'long_name'    , 'Geometric standard deviation of particle size mix')
                 ierr = nf90_put_att(imapfile, id_dgsd(iid) ,  'units'        , 'm')
              endif
              if (stmpar%morpar%moroutput%percentiles) then
                 do l = 1, stmpar%morpar%nxx
                    write(dxname,'(A,I2.2)') 'DXX',l
                    write(dxdescr,'(A,F4.1,A)') 'Sediment diameter percentile '    , stmpar%morpar%xx(l)*100d0,' %'
                    ierr = nf90_def_var(imapfile, dxname , nf90_double, (/id_flowelemdim(iid) , id_timedim(iid) /) , id_dxx(l,iid))
                    ierr = nf90_put_att(imapfile, id_dxx(l,iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                    ierr = nf90_put_att(imapfile, id_dxx(l,iid) ,  'long_name'    , dxdescr)
                    ierr = nf90_put_att(imapfile, id_dxx(l,iid) ,  'units'        , 'm')
                 enddo
              endif
              if (stmpar%morpar%moroutput%frac) then
                 ierr = nf90_def_var(imapfile, 'frac' , nf90_double, (/id_flowelemdim(iid) , id_sedtotdim(iid) , id_timedim(iid) /) , id_frac(iid))
                 ierr = nf90_put_att(imapfile, id_frac(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_frac(iid) ,  'long_name'    , 'Availability fraction in top layer')
                 ierr = nf90_put_att(imapfile, id_frac(iid) ,  'units'        , '-')
              endif
              if (stmpar%morpar%moroutput%mudfrac) then
                 ierr = nf90_def_var(imapfile, 'mudfrac' , nf90_double, (/id_flowelemdim(iid) , id_timedim(iid) /) , id_mudfrac(iid))
                 ierr = nf90_put_att(imapfile, id_mudfrac(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_mudfrac(iid) ,  'long_name'    , 'Mud fraction in top layer')
                 ierr = nf90_put_att(imapfile, id_mudfrac(iid) ,  'units'        , '-')
              endif
              if (stmpar%morpar%moroutput%sandfrac) then
                 ierr = nf90_def_var(imapfile, 'sandfrac' , nf90_double, (/id_flowelemdim(iid) , id_timedim(iid) /) , id_sandfrac(iid))
                 ierr = nf90_put_att(imapfile, id_sandfrac(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_sandfrac(iid) ,  'long_name'    , 'Sand fraction in top layer')
                 ierr = nf90_put_att(imapfile, id_sandfrac(iid) ,  'units'        , '-')
              endif
              if (stmpar%morpar%moroutput%fixfac) then
                 ierr = nf90_def_var(imapfile, 'fixfac' , nf90_double, (/id_flowelemdim(iid) , id_sedtotdim(iid), id_timedim(iid) /) , id_fixfac(iid))
                 ierr = nf90_put_att(imapfile, id_fixfac(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_fixfac(iid) ,  'long_name'    , 'Reduction factor due to limited sediment thickness')
                 ierr = nf90_put_att(imapfile, id_fixfac(iid) ,  'units'        , '-')
              endif
              if (stmpar%morpar%moroutput%hidexp) then
                 ierr = nf90_def_var(imapfile, 'hidexp' , nf90_double, (/id_flowelemdim(iid) , id_sedtotdim(iid), id_timedim(iid) /) , id_hidexp(iid))
                 ierr = nf90_put_att(imapfile, id_hidexp(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_hidexp(iid) ,  'long_name'    , 'Hiding and exposure factor')
                 ierr = nf90_put_att(imapfile, id_hidexp(iid) ,  'units'        , '-')
              endif
              ! Fluff layers
              if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0) then
                 ierr = nf90_def_var(imapfile, 'mfluff' , nf90_double, (/id_flowelemdim(iid) , id_sedsusdim(iid), id_timedim(iid) /) , id_mfluff(iid))
                 ierr = nf90_put_att(imapfile, id_mfluff(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_mfluff(iid) ,  'long_name'    , 'Sediment mass in fluff layer')
                 ierr = nf90_put_att(imapfile, id_mfluff(iid) ,  'units'        , 'kg m-2 ')              
              end if
           endif
           
           if (bfmpar%lfbedfrmout) then
              if (bfmpar%lfbedfrm) then
                 ! DUNEHEIGHT
                 ierr = nf90_def_var(imapfile, 'duneheight' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_duneheight(iid))
                 ierr = nf90_put_att(imapfile, id_duneheight(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_duneheight(iid) ,  'long_name'    , 'Time-varying dune height in flow cell centers')
                 ierr = nf90_put_att(imapfile, id_duneheight(iid) ,  'units'        , 'm')
                 ! DUNELENGTH
                 ierr = nf90_def_var(imapfile, 'dunelength' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_dunelength(iid))
                 ierr = nf90_put_att(imapfile, id_dunelength(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_dunelength(iid) ,  'long_name'    , 'Time-varying dune length in flow cell centers')
                 ierr = nf90_put_att(imapfile, id_dunelength(iid) ,  'units'        , 'm')
              end if
              if (bfmpar%lfbedfrmrou) then
                 call realloc(rks,ndx, keepExisting=.false.,fill=0d0)
                 ! KSR
                 ierr = nf90_def_var(imapfile, 'ksr' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_ksr(iid))
                 ierr = nf90_put_att(imapfile, id_ksr(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_ksr(iid) ,  'long_name'    , 'Ripple roughness height in flow cell center')
                 ierr = nf90_put_att(imapfile, id_ksr(iid) ,  'units'        , 'm')
                 ! KSMR
                 ierr = nf90_def_var(imapfile, 'ksmr' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_ksmr(iid))
                 ierr = nf90_put_att(imapfile, id_ksmr(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_ksmr(iid) ,  'long_name'    , 'Mega-ripple roughness height in flow cell center')
                 ierr = nf90_put_att(imapfile, id_ksmr(iid) ,  'units'        , 'm')
                 ! KSD
                 ierr = nf90_def_var(imapfile, 'ksd' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_ksd(iid))
                 ierr = nf90_put_att(imapfile, id_ksd(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_ksd(iid) ,  'long_name'    , 'Dune roughness height in flow cell center')
                 ierr = nf90_put_att(imapfile, id_ksd(iid) ,  'units'        , 'm')
                 ! KS
                 ierr = nf90_def_var(imapfile, 'ks' , nf90_double, (/ id_flowelemdim(iid) , id_timedim(iid) /) , id_ks(iid))
                 ierr = nf90_put_att(imapfile, id_ks(iid) ,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                 ierr = nf90_put_att(imapfile, id_ks(iid) ,  'long_name'    , 'Bedform roughness height in flow cell center')
                 ierr = nf90_put_att(imapfile, id_ks(iid) ,  'units'        , 'm')
              end if
           end if
           if (jased > 0 .and. .not.stm_included) then
              ierr = nf90_def_dim(imapfile, 'nFrac', mxgr, id_maxfracdim(iid))  

              if (jaceneqtr == 1) then 
                  ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_erolaydim(iid)) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
                  if (ierr /= nf90_noerr) then
                     ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_erolaydim(iid))
                  end if
              else
                  ierr = nf90_inq_dimid(imapfile, 'nNetNode' , id_erolaydim(iid)) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
              endif

              ierr = nf90_def_var(imapfile, 'sed'  , nf90_double, (/ id_maxfracdim  (iid), id_flowelemdim(iid), id_timedim (iid)/) , id_sed(iid))
              ierr = nf90_put_att(imapfile, id_sed(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
              ierr = nf90_put_att(imapfile, id_sed(iid),  'long_name'    , 'sediment concentration')
              ierr = nf90_put_att(imapfile, id_sed(iid),  'units'        , 'kg m-3')
              ierr = nf90_def_var(imapfile, 'ero' , nf90_double, (/ id_maxfracdim  (iid), id_erolaydim(iid), id_timedim (iid)/) , id_ero(iid))
              if (jaceneqtr == 1) then 
                  ierr = nf90_put_att(imapfile, id_ero(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_ero(iid),  'long_name', 'erodable layer thickness per size fraction in flow element center')
              else 
                  ierr = nf90_put_att(imapfile, id_ero(iid),  'coordinates'  , 'NetNode_x NetNode_y')
                  ierr = nf90_put_att(imapfile, id_ero(iid),  'long_name', 'erodable layer thickness per size fraction at flow element corners')
              endif
              ierr = nf90_put_att(imapfile, id_ero(iid),  'standard_name'    , 'Erodable layer thickness') ! Not CF
              ierr = nf90_put_att(imapfile, id_ero(iid),  'units'        , 'm')

              if (jaceneqtr .ne. 1) then
                 idims(1) = id_erolaydim(iid)
                 call definencvar(imapfile,id_zk(iid)   ,nf90_double,idims,2, 'netnode_bedlevel_zk'  , 'Flow element corner bedlevel (zk)', 'm', 'NetNode_x NetNode_y')
              endif
              idims(1) = id_flowelemdim(iid)
              call definencvar(imapfile,id_bl(iid)   ,nf90_double,idims,2, 'flowelem_bedlevel_bl'  , 'Flow element center bedlevel (bl)', 'm', 'FlowElem_xcc FlowElem_ycc')

           endif

           ! JRE waves
           if (jawave .eq. 4) then
             ierr = nf90_def_var(imapfile, 'E',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_E(iid))
             ierr = nf90_put_att(imapfile, id_E(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_E(iid),   'standard_name', 'sea_surface_bulk_wave_energy')                          ! not CF
             ierr = nf90_put_att(imapfile, id_E(iid),   'long_name'    , 'wave energy per square meter')          
             ierr = nf90_put_att(imapfile, id_E(iid),   'units'        , 'J m-2')
           
             ierr = nf90_def_var(imapfile, 'R',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_R(iid))
             ierr = nf90_put_att(imapfile, id_R(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_R(iid),   'standard_name', 'sea_surface_bulk_roller_energy')                          ! not CF
             ierr = nf90_put_att(imapfile, id_R(iid),   'long_name'    , 'roller energy per square meter')          
             ierr = nf90_put_att(imapfile, id_R(iid),   'units'        , 'J m-2')
           
             ierr = nf90_def_var(imapfile, 'DR',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_DR(iid))
             ierr = nf90_put_att(imapfile, id_DR(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_DR(iid),   'standard_name', 'sea_surface_bulk_roller_dissipation')                          ! not CF
             ierr = nf90_put_att(imapfile, id_DR(iid),   'long_name'    , 'roller energy dissipation per square meter')          
             ierr = nf90_put_att(imapfile, id_DR(iid),   'units'        , 'W m-2')
           
             ierr = nf90_def_var(imapfile, 'D',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_D(iid))
             ierr = nf90_put_att(imapfile, id_D(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_D(iid),   'standard_name', 'sea_surface_wave_breaking_dissipation')                          ! not CF
             ierr = nf90_put_att(imapfile, id_D(iid),   'long_name'    , 'wave breaking energy dissipation per square meter')          
             ierr = nf90_put_att(imapfile, id_D(iid),   'units'        , 'W m-2')
             ! JRE TO DO: change definition in unc file to correct one
             ierr = nf90_def_var(imapfile, 'H',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_H(iid))
             ierr = nf90_put_att(imapfile, id_H(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_H(iid),   'standard_name', 'sea_surface_wave_rms_height')   
             ierr = nf90_put_att(imapfile, id_H(iid),   'long_name'    , 'Root mean square wave height based on wave energy')          
             ierr = nf90_put_att(imapfile, id_H(iid),   'units'        , 'm')
             
             ierr = nf90_def_var(imapfile, 'urms_cc',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_urmscc(iid))
             ierr = nf90_put_att(imapfile, id_urmscc(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_urmscc(iid),   'standard_name', 'sea_surface_wave_orbital_velocity')   
             ierr = nf90_put_att(imapfile, id_urmscc(iid),   'long_name'    , 'Root mean square orbital velocity on flow centers')          
             ierr = nf90_put_att(imapfile, id_urmscc(iid),   'units'        , 'm')
             
             ierr = nf90_def_var(imapfile, 'Fx_cc',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_Fxcc(iid))
             ierr = nf90_put_att(imapfile, id_Fxcc(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_Fxcc(iid),   'standard_name', 'sea_surface_wave_force_east')   
             ierr = nf90_put_att(imapfile, id_Fxcc(iid),   'long_name'    , 'Wave induced flow forcing in cell centre, east component')          
             ierr = nf90_put_att(imapfile, id_Fxcc(iid),   'units'        , 'kg m s-2')
             
             ierr = nf90_def_var(imapfile, 'Fy_cc',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_Fycc(iid))
             ierr = nf90_put_att(imapfile, id_Fycc(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_Fycc(iid),   'standard_name', 'sea_surface_wave_force_north')   
             ierr = nf90_put_att(imapfile, id_Fycc(iid),   'long_name'    , 'Wave induced flow forcing in cell centre, north component')          
             ierr = nf90_put_att(imapfile, id_Fycc(iid),   'units'        , 'kg m s-2')
             
             !if (jamombal>0) then
             
                ierr = nf90_def_var(imapfile, 'ducxdx',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_ducxdx(iid))
                ierr = nf90_put_att(imapfile, id_ducxdx(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_ducxdx(iid),   'standard_name', 'E velocity gradient in E direction')          
                ierr = nf90_put_att(imapfile, id_ducxdx(iid),   'units'        , 's-1')
                
                ierr = nf90_def_var(imapfile, 'ducxdy',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_ducxdy(iid))
                ierr = nf90_put_att(imapfile, id_ducxdy(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_ducxdy(iid),   'standard_name', 'E velocity gradient in N direction')          
                ierr = nf90_put_att(imapfile, id_ducxdy(iid),   'units'        , 's-1')
                
                ierr = nf90_def_var(imapfile, 'ducydx',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_ducydx(iid))
                ierr = nf90_put_att(imapfile, id_ducydx(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_ducydx(iid),   'standard_name', 'N velocity gradient in E direction')          
                ierr = nf90_put_att(imapfile, id_ducydx(iid),   'units'        , 's-1')
                
                ierr = nf90_def_var(imapfile, 'ducydy',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_ducydy(iid))
                ierr = nf90_put_att(imapfile, id_ducydy(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_ducydy(iid),   'standard_name', 'N velocity gradient in N direction')          
                ierr = nf90_put_att(imapfile, id_ducydy(iid),   'units'        , 's-1')
             
                ierr = nf90_def_var(imapfile, 'dsdx',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_dsdx(iid))
                ierr = nf90_put_att(imapfile, id_dsdx(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_dsdx(iid),   'standard_name', 'water level gradient in E direction')         
                ierr = nf90_put_att(imapfile, id_dsdx(iid),   'units'        , '-')
             
                ierr = nf90_def_var(imapfile, 'dsdy',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_dsdy(iid))
                ierr = nf90_put_att(imapfile, id_dsdy(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')   
                ierr = nf90_put_att(imapfile, id_dsdy(iid),   'long_name'    , 'water level gradient in N direction')          
                ierr = nf90_put_att(imapfile, id_dsdy(iid),   'units'        , '-')
             
             !end if
           
             ierr = nf90_def_var(imapfile, 'urms',  nf90_double, (/ id_flowlinkdim(iid), id_timedim(iid)/) , id_urms(iid))
             ierr = nf90_put_att(imapfile, id_urms(iid),'standard_name', 'sea_surface_wave_orbital_velocity')
             ierr = nf90_put_att(imapfile, id_urms(iid),'units'        , 'm s-1')
             ierr = nf90_put_att(imapfile, id_urms(iid),'coordinates'  , 'FlowLink_xu FlowLink_yu')
           
             ierr = nf90_def_var(imapfile, 'ust' , nf90_double, (/ id_flowlinkdim(iid), id_timedim(iid) /) , id_ust(iid))
             ierr = nf90_put_att(imapfile, id_ust(iid),'standard_name', 'sea_surface_Stokes_drift_east')
             ierr = nf90_put_att(imapfile, id_ust(iid),'units'        , 'm s-1')
             ierr = nf90_put_att(imapfile, id_ust(iid),'coordinates'  , 'FlowLink_xu FlowLink_yu')
           
             ierr = nf90_def_var(imapfile, 'vst' , nf90_double, (/ id_flowlinkdim(iid), id_timedim(iid)/) , id_vst(iid))
             ierr = nf90_put_att(imapfile, id_vst(iid),'standard_name', 'sea_surface_Stokes_drift_north')
             ierr = nf90_put_att(imapfile, id_vst(iid),'units'        , 'm s-1')
             ierr = nf90_put_att(imapfile, id_vst(iid),'coordinates'  , 'FlowLink_xu FlowLink_yu')
             
             !ierr = nf90_def_var(imapfile, 'Fx' , nf90_double, (/ id_flowlinkdim(iid), id_timedim(iid) /) , id_Fx(iid))
             !ierr = nf90_put_att(imapfile, id_Fx(iid),'standard_name', 'sea_surface_wave_force_east')
             !ierr = nf90_put_att(imapfile, id_Fx(iid),'units'        , 'N m-2')
             !ierr = nf90_put_att(imapfile, id_Fx(iid),'coordinates'  , 'FlowLink_xu FlowLink_yu')
             !
             !ierr = nf90_def_var(imapfile, 'Fy' , nf90_double, (/ id_flowlinkdim(iid), id_timedim(iid) /) , id_Fy(iid))
             !ierr = nf90_put_att(imapfile, id_Fy(iid),'standard_name', 'sea_surface_wave_force_north')
             !ierr = nf90_put_att(imapfile, id_Fy(iid),'units'        , 'N m-2')
             !ierr = nf90_put_att(imapfile, id_Fy(iid),'coordinates'  , 'FlowLink_xu FlowLink_yu')
           
             ierr = nf90_def_var(imapfile, 'thetamean',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid)/) , id_thetamean(iid))
             ierr = nf90_put_att(imapfile, id_thetamean(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_thetamean(iid),   'standard_name', 'sea_surface_wave_from_direction')                          ! not CF
             ierr = nf90_put_att(imapfile, id_thetamean(iid),   'long_name'    , 'mean wave angle')          
             ierr = nf90_put_att(imapfile, id_thetamean(iid),   'units'        , 'deg')
           
             ierr = nf90_def_var(imapfile, 'cwav',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_cwav(iid))
             ierr = nf90_put_att(imapfile, id_cwav(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_cwav(iid),   'standard_name', 'sea_surface_wave_phase_celerity')                          ! not CF
             ierr = nf90_put_att(imapfile, id_cwav(iid),   'long_name'    , 'phase celerity')          
             ierr = nf90_put_att(imapfile, id_cwav(iid),   'units'        , 'm s-1')
           
             ierr = nf90_def_var(imapfile, 'cgwav',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_cgwav(iid))
             ierr = nf90_put_att(imapfile, id_cgwav(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_cgwav(iid),   'standard_name', 'sea_surface_wave_group_celerity')                          ! not CF
             ierr = nf90_put_att(imapfile, id_cgwav(iid),   'long_name'    , 'group celerity')          
             ierr = nf90_put_att(imapfile, id_cgwav(iid),   'units'        , 'm s-1')
           
             ierr = nf90_def_var(imapfile, 'sigmwav',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_sigmwav(iid))
             ierr = nf90_put_att(imapfile, id_sigmwav(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
             ierr = nf90_put_att(imapfile, id_sigmwav(iid),   'standard_name', 'sea_surface_wave_mean_frequency')                          ! not CF
             ierr = nf90_put_att(imapfile, id_sigmwav(iid),   'long_name'    , 'mean wave frequency')          
             ierr = nf90_put_att(imapfile, id_sigmwav(iid),   'units'        , 'rad s-1')
             
             if ( (windmodel.eq.1) .and. (jawsource.eq.1) ) then
             
                ierr = nf90_def_var(imapfile, 'SwE',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_SwE(iid))
                ierr = nf90_put_att(imapfile, id_SwE(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_SwE(iid),   'standard_name', 'source_term_wind_on_E')                          ! not CF
                ierr = nf90_put_att(imapfile, id_SwE(iid),   'long_name'    , 'source term wind on wave energy')          
                ierr = nf90_put_att(imapfile, id_SwE(iid),   'units'        , 'J m-2 s-1')             

                ierr = nf90_def_var(imapfile, 'SwT',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_SwT(iid))
                ierr = nf90_put_att(imapfile, id_SwT(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
                ierr = nf90_put_att(imapfile, id_SwT(iid),   'standard_name', 'source_term_wind_on_T')                          ! not CF
                ierr = nf90_put_att(imapfile, id_SwT(iid),   'long_name'    , 'source term wind on wave period')          
                ierr = nf90_put_att(imapfile, id_SwT(iid),   'units'        , 's s-1')      
                
             endif
           endif
           
           if ( NUMCONST.eq.0 ) then
              ierr = unc_add_gridmapping_att(imapfile, (/ id_s1(iid), id_taus(iid), id_ucx(iid), id_ucy(iid), id_unorm(iid), id_sa1(iid), id_sed(iid) /), jsferic)   ! add id_ucz(iid)?
           else
              if (allocated(idum)) deallocate(idum)
              allocate(idum(7+NUMCONST))
              idum(1:7) = (/ id_s1(iid), id_taus(iid), id_ucx(iid), id_ucy(iid), id_unorm(iid), id_sa1(iid), id_sed(iid) /)
              do j=1,NUMCONST
                 idum(7+j) = id_const(iid,j)
              end do
              ierr = unc_add_gridmapping_att(imapfile, idum, jsferic)
           endif   
           if (kmx > 0) then
              ierr = unc_add_gridmapping_att(imapfile, (/ id_ucz(iid), id_ucxa(iid), id_ucya(iid), id_ww1(iid), id_rho(iid) /), jsferic)
           end if

           if (jamaptrachy > 0 .and. jatrt == 1) then
               ! Roughness data on net-links
               ierr = nf90_def_var(imapfile, 'cftrt' , nf90_double, (/ id_netlinkdim(iid), id_timedim(iid) /) , id_cftrt(iid))
               if (ifrctypuni == 0) then 
                   ierr = nf90_put_att(imapfile, id_cftrt(iid),'long_name'    , 'Chezy roughness from trachytopes')
                   ierr = nf90_put_att(imapfile, id_cftrt(iid),'units'        , 'm0.5s-1')                ! WO: does not follow standard ? (which accepts only integral powers?)
               elseif (ifrctypuni == 1) then 
                   ierr = nf90_put_att(imapfile, id_cftrt(iid),'long_name'    , 'Manning roughness from trachytopes')
                   ierr = nf90_put_att(imapfile, id_cftrt(iid),'units'        , 'sm-0.333')               ! WO: does not follow standard ? (which accepts only integral powers?)
               elseif ((ifrctypuni == 2) .or. (ifrctypuni == 3)) then 
                   ierr = nf90_put_att(imapfile, id_cftrt(iid),'long_name'    , 'White-Colebrook roughness from trachytopes')
                   ierr = nf90_put_att(imapfile, id_cftrt(iid),'units'        , 'm')  
               else
                   ierr = nf90_put_att(imapfile, id_cftrt(iid),'long_name'    , 'roughness from trachytopes')
                   ierr = nf90_put_att(imapfile, id_cftrt(iid),'units'        , ' ')       
               endif
           endif 

           if (jamapcali > 0 .and. jacali == 1) then
               ! Calibration factor for roughness data on net-links
               ierr = nf90_def_var(imapfile, 'cfcl' , nf90_double, (/ id_netlinkdim(iid), id_timedim(iid) /) , id_cfcl(iid))
               ierr = nf90_put_att(imapfile, id_cfcl(iid),'long_name'    , 'Calibration factor for roughness')
               ierr = nf90_put_att(imapfile, id_cfcl(iid),'units'        , ' ')             
           endif 
           
           if (jamapchezy > 0) then 
               ! Chezy data on flow-nodes
               ierr = nf90_def_var(imapfile, 'czs' , nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_czs(iid))
               ierr = nf90_put_att(imapfile, id_czs(iid),'long_name'    , 'Chezy roughness')
               ierr = nf90_put_att(imapfile, id_czs(iid),'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_czs(iid),'units'        , 'm0.5s-1')                ! WO: does not follow standard ? (which accepts only integral powers?)
           endif

           ! 1D2D boundaries
           if (nbnd1d2d > 0) then
               ierr = nf90_def_var(imapfile, '1d2d_flowlinknrs' , nf90_int, (/ id_1d2ddim(iid) /) , id_1d2d_edges(iid))
               ierr = nf90_put_att(imapfile, id_czs(iid),'long_name'    , 'flow link numbers of the open 1D2D boundary links')

               ierr = nf90_def_var(imapfile, '1d2d_zeta' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_zeta1d(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_zeta1d(iid),'standard_name', 'sea_surface_height_above_geoid')
               ierr = nf90_put_att(imapfile, id_1d2d_zeta1d(iid),'long_name'    , '1D water level next to each 1d2d boundary link')
               ierr = nf90_put_att(imapfile, id_1d2d_zeta1d(iid),'units'        , 'm')

               ierr = nf90_def_var(imapfile, '1d2d_crest_level' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_crest_level(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_crest_level(iid),'standard_name', 'sea_surface_height_above_geoid')
               ierr = nf90_put_att(imapfile, id_1d2d_crest_level(iid),'long_name'    , 'crest level of 1d2d boundary link')
               ierr = nf90_put_att(imapfile, id_1d2d_crest_level(iid),'units'        , 'm')

               ierr = nf90_def_var(imapfile, '1d2d_b_2di' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_b_2di(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_b_2di(iid),'standard_name', 'b_2di')
               ierr = nf90_put_att(imapfile, id_1d2d_b_2di(iid),'long_name'    , 'coefficient for 1d2d interface b_2di')
               ierr = nf90_put_att(imapfile, id_1d2d_b_2di(iid),'units'        , '-')

               ierr = nf90_def_var(imapfile, '1d2d_b_2dv' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_b_2dv(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_b_2dv(iid),'standard_name', 'b_2dv')
               ierr = nf90_put_att(imapfile, id_1d2d_b_2dv(iid),'long_name'    , 'coefficient for 1d2d interface b_2di')
               ierr = nf90_put_att(imapfile, id_1d2d_b_2dv(iid),'units'        , '-')

               ierr = nf90_def_var(imapfile, '1d2d_d_2dv' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_d_2dv(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_d_2dv(iid),'standard_name', 'd_2dv')
               ierr = nf90_put_att(imapfile, id_1d2d_d_2dv(iid),'long_name'    , 'coefficient for 1d2d interface d_2dv')
               ierr = nf90_put_att(imapfile, id_1d2d_d_2dv(iid),'units'        , '-')

               ierr = nf90_def_var(imapfile, '1d2d_qzeta' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_q_zeta(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_q_zeta(iid),'standard_name', 'q_zeta_1d2d')
               ierr = nf90_put_att(imapfile, id_1d2d_q_zeta(iid),'long_name'    , 'q_zeta_1d2d')
               ierr = nf90_put_att(imapfile, id_1d2d_q_zeta(iid),'units'        , 'm2 s-1')

               ierr = nf90_def_var(imapfile, '1d2d_q_lat' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_q_lat(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_q_lat(iid),'standard_name', 'q_lat')
               ierr = nf90_put_att(imapfile, id_1d2d_q_lat(iid),'long_name'    , 'q_lat')
               ierr = nf90_put_att(imapfile, id_1d2d_q_lat(iid),'units'        , 'm3 s-1')

               ierr = nf90_def_var(imapfile, '1d2d_cfl' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_cfl(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_cfl(iid),'standard_name', 'cfl')
               ierr = nf90_put_att(imapfile, id_1d2d_cfl(iid),'long_name'    , 'wave flow courant')
               ierr = nf90_put_att(imapfile, id_1d2d_cfl(iid),'units'        , '-')

               ierr = nf90_def_var(imapfile, '1d2d_sb' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_sb(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_sb(iid),'standard_name', '1d2d_sb')
               ierr = nf90_put_att(imapfile, id_1d2d_sb(iid),'long_name'    , 'water levels in boundary points')
               ierr = nf90_put_att(imapfile, id_1d2d_sb(iid),'units'        , 'm')

               ierr = nf90_def_var(imapfile, '1d2d_s0_2d' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_s0_2d(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_s0_2d(iid),'standard_name', '1d2d_s0_2d')                       
               ierr = nf90_put_att(imapfile, id_1d2d_s0_2d(iid),'long_name'    , 'water levels on interface at previous time step')
               ierr = nf90_put_att(imapfile, id_1d2d_s0_2d(iid),'units'        , 'm')

               ierr = nf90_def_var(imapfile, '1d2d_s1_2d' , nf90_double, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_s1_2d(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_s1_2d(iid),'standard_name', '1d2d_s1_2d')
               ierr = nf90_put_att(imapfile, id_1d2d_s1_2d(iid),'long_name'    , 'water levels on interface at current time step')
               ierr = nf90_put_att(imapfile, id_1d2d_s1_2d(iid),'units'        , 'm')

               ierr = nf90_def_var(imapfile, '1d2d_flow_cond' , nf90_int, (/ id_1d2ddim(iid), id_timedim(iid) /) , id_1d2d_flow_cond(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_flow_cond(iid),'standard_name', 'flow_condition')
               ierr = nf90_put_att(imapfile, id_1d2d_flow_cond(iid),'long_name'    , 'flow Condition 0: closed, 1: free 1d to 2d, 2: free 2d to 1d, 3: submerged')
               ierr = nf90_put_att(imapfile, id_1d2d_flow_cond(iid),'units'        , '-')

           end if
        endif

        if (jamapwind > 0 .and. japatm > 0) then
            call definencvar(imapfile,id_patm(iid)   ,nf90_double,idims,2, 'Patm'  , 'Atmospheric Pressure', 'N m-2', 'FlowElem_xcc FlowElem_ycc')
        endif

        if ((jamapwind > 0 .or. jamapwindstress > 0) .and. jawind /= 0) then
           if (jawindstressgiven == 0) then
              ierr = nf90_def_var(imapfile, 'windx', nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_windx(iid))
              ierr = nf90_def_var(imapfile, 'windy', nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_windy(iid))
           else
              ierr = nf90_def_var(imapfile, 'windstressx', nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_windx(iid))
              ierr = nf90_def_var(imapfile, 'windstressy', nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_windy(iid))
           endif

           ierr = nf90_put_att(imapfile, id_windx(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           if (jawindstressgiven == 0) then
              if (jsferic == 0 ) then
                 ierr = nf90_put_att(imapfile, id_windx(iid),  'standard_name', 'x_wind')
                 ierr = nf90_put_att(imapfile, id_windx(iid),  'long_name'    , 'velocity of air on flow element center, x-component')
              else
                 ierr = nf90_put_att(imapfile, id_windx(iid),  'standard_name', 'eastward_wind')
                 ierr = nf90_put_att(imapfile, id_windx(iid),  'long_name'    , 'eastward air velocity on flow element center, x-component')
              endif
              ierr = nf90_put_att(imapfile, id_windx(iid),  'units'        , 'm s-1')
           else
              if (jsferic == 0 ) then
                 ierr = nf90_put_att(imapfile, id_windx(iid),  'standard_name', 'x_windstress')
                 ierr = nf90_put_att(imapfile, id_windx(iid),  'long_name'    , 'windstress on flow element center, x-component')
              else
                 ierr = nf90_put_att(imapfile, id_windx(iid),  'standard_name', 'eastward_windstress')
                 ierr = nf90_put_att(imapfile, id_windx(iid),  'long_name'    , 'eastward windstress on flow element center, x-component')
              endif
              ierr = nf90_put_att(imapfile, id_windx(iid),  'units'        , 'N m-2')
           endif

           ierr = nf90_put_att(imapfile, id_windy(iid),  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
           if (jawindstressgiven == 0) then
              if (jsferic == 0 ) then
                 ierr = nf90_put_att(imapfile, id_windy(iid),  'standard_name', 'y_wind')
                 ierr = nf90_put_att(imapfile, id_windy(iid),  'long_name'    , 'velocity of air on flow element center, y-component')
              else
                 ierr = nf90_put_att(imapfile, id_windy(iid),  'standard_name', 'northward_wind')
                 ierr = nf90_put_att(imapfile, id_windy(iid),  'long_name'    , 'northward air velocity on flow element center, y-component')
              endif
              ierr = nf90_put_att(imapfile, id_windy(iid),  'units'        , 'm s-1')
           else
              if (jsferic == 0 ) then
                 ierr = nf90_put_att(imapfile, id_windy(iid),  'standard_name', 'y_windstress')
                 ierr = nf90_put_att(imapfile, id_windy(iid),  'long_name'    , 'windstress air on flow element center, y-component')
              else
                 ierr = nf90_put_att(imapfile, id_windy(iid),  'standard_name', 'northward_windstress')
                 ierr = nf90_put_att(imapfile, id_windy(iid),  'long_name'    , 'northward windstress on flow element center, y-component')
              endif
              ierr = nf90_put_att(imapfile, id_windy(iid),  'units'        , 'N m-2')
           endif
        endif

        if (jamapwind > 0 .and. jawind /= 0 .and. jawindstressgiven == 0) then
           ! Also wind on flow links
           ierr = nf90_def_var(imapfile, 'windxu', nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_windxu(iid))
           ierr = nf90_def_var(imapfile, 'windyu', nf90_double, (/ id_flowlinkdim(iid), id_timedim (iid)/) , id_windyu(iid))
      
           ierr = nf90_put_att(imapfile, id_windxu(iid),  'coordinates'  , 'FlowLink_xu FlowLink_yu')
           if (jsferic == 0) then
              ierr = nf90_put_att(imapfile, id_windxu(iid),  'long_name'    , 'velocity of air on flow links, x-component')
              ierr = nf90_put_att(imapfile, id_windxu(iid),  'standard_name', 'x_velocity_wind')
           else
              ierr = nf90_put_att(imapfile, id_windxu(iid),  'long_name'    , 'eastward air velocity on flow links, x-component')
              ierr = nf90_put_att(imapfile, id_windxu(iid),  'standard_name', 'eastward_wind')
           endif
           ierr = nf90_put_att(imapfile, id_windxu(iid),  'units'        , 'm s-1')

           ierr = nf90_put_att(imapfile, id_windyu(iid),  'coordinates'  , 'FlowLink_xu FlowLink_yu')
           if (jsferic == 0) then
              ierr = nf90_put_att(imapfile, id_windyu(iid),  'long_name'    , 'velocity of air on flow links, y-component')
              ierr = nf90_put_att(imapfile, id_windyu(iid),  'standard_name', 'y_velocity_wind')
           else
              ierr = nf90_put_att(imapfile, id_windyu(iid),  'long_name'    , 'northward air velocity on flow links, y-component')
              ierr = nf90_put_att(imapfile, id_windyu(iid),  'standard_name', 'northward_wind')
           endif
           ierr = nf90_put_att(imapfile, id_windyu(iid),  'units'        , 'm s-1')
        endif
        !
        ierr = unc_add_gridmapping_att(imapfile, (/ id_windx(iid), id_windy(iid), id_windxu(iid), id_windyu(iid),  nf90_global /), jsferic)

        ierr = nf90_enddef(imapfile)

        ! 1D2D boundaries
        if (nbnd1d2d > 0 .and. jaseparate_ /= 2) then
           if (allocated(idum)) deallocate(idum)
           allocate(idum(nbnd1d2d))
           do i=1,nbnd1d2d
              idum(i) = kbnd1d2d(3, i) ! Flow link nrs
           end do
           ierr = nf90_put_var(imapfile, id_1d2d_edges(iid), idum)
           deallocate(idum)
        end if

        if (nomba > 0) then
           ierr = nf90_put_var(imapfile, id_mba(iid), mbadef(1:NdxNdxi))
        end if

        firststep(iid) = .false.
        
    endif   

    ! End of writing time-independent flow geometry data.
    ! -- Inquire id's belonging to map file ------------------------
    if (firststep(iid) .and. ndim>0) then ! TODO: AvD: UNST-530
       ! 
       ! 
       ! this step is necessary because if a snapshot_map.nc file is written
       ! in between two map file outputs the saved id's may have changed
       !
       firststep(iid) = .false. 
       !
       ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim(iid))
       if (ierr /= nf90_noerr) then
          ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_flowelemdim(iid))
       endif

       ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim(iid))
       !
       ! Time
       ierr = nf90_inq_dimid(imapfile, 'time', id_timedim(iid))
       ierr = nf90_inq_varid(imapfile, 'time', id_time(iid))
       !
       if ( kmx>0 ) then
          ierr = nf90_inq_dimid(imapfile, 'laydim', id_laydim(iid))
          ierr = nf90_inq_dimid(imapfile, 'wdim', id_wdim(iid))
       endif        
       !    
       ! Size of latest timestep
       
       ! Why ask for id_*, they are in a save statement no? 
       
       ierr = nf90_inq_varid(imapfile, 'timestep', id_timestep(iid))
       ierr = nf90_inq_varid(imapfile, 'taus' ,  id_taus(iid))
       !
       if ( kmx>0 ) then     !  3D
          ierr = nf90_inq_varid(imapfile, 'ucx', id_ucx(iid))
          ierr = nf90_inq_varid(imapfile, 'ucy', id_ucy(iid))
          ierr = nf90_inq_varid(imapfile, 'ucz', id_ucz(iid))
          ierr = nf90_inq_varid(imapfile, 'ucxa', id_ucxa(iid))
          ierr = nf90_inq_varid(imapfile, 'ucya', id_ucya(iid))
          ierr = nf90_inq_varid(imapfile, 'ww1', id_ww1(iid))
          ierr = nf90_inq_varid(imapfile, 'rho', id_rho(iid))
          if( iturbulencemodel >= 3 ) then
             ierr = nf90_inq_varid(imapfile, 'turkin1', id_turkin1(iid))
             ierr = nf90_inq_varid(imapfile, 'tureps1', id_tureps1(iid))
             ierr = nf90_inq_varid(imapfile, 'vicwwu' , id_vicwwu(iid) )
          endif
        else
          ierr = nf90_inq_varid(imapfile, 'ucx', id_ucx(iid))
          ierr = nf90_inq_varid(imapfile, 'ucy', id_ucy(iid))
          !ierr = nf90_inq_varid(imapfile, 'rho', id_rho(iid))
          ierr = nf90_inq_varid(imapfile, 'spircrv', id_spircrv(iid))
          ierr = nf90_inq_varid(imapfile, 'spirint', id_spirint(iid))
        endif
        !
        if (jasal > 0) then 
           ierr = nf90_inq_varid(imapfile, 'sa1', id_sa1(iid))
        endif
    
        if (jatem > 0) then 
           ierr = nf90_inq_varid(imapfile, 'tem1', id_tem1(iid))
        endif
        
        if (ITRA1 > 0) then
           do j=ITRA1,ITRAN
              tmpstr = const_names(j)
              ! Forbidden chars in NetCDF names: space, /, and more.
              call replace_char(tmpstr,32,95) 
              call replace_char(tmpstr,47,95) 
              ierr = nf90_inq_varid(imapfile, trim(tmpstr), id_const(iid,j))
           end do
        endif
       
        !
        if (stm_included) then
           ierr = nf90_inq_varid(imapfile, 'nSedTot', id_sedtotdim(iid))
           ierr = nf90_inq_varid(imapfile, 'nSedSus', id_sedsusdim(iid))
           ierr = nf90_inq_varid(imapfile, 'nBedLayers', id_nlyrdim(iid))
           
           if (stmpar%lsedsus > 0) then
              ierr = nf90_inq_varid(imapfile, 'ws', id_ws(iid))
              !
              ! equilibrium concentration, 2D only
              if (kmx == 0) then
                 ierr = nf90_inq_varid(imapfile, 'rsedeq', id_rsedeq(iid))
              end if
              
              if (stmpar%morpar%moroutput%sourcesink) then
                 ierr = nf90_inq_varid(imapfile, 'sourse', id_sourse(iid))
                 ierr = nf90_inq_varid(imapfile, 'sinkse', id_sinkse(iid))
              endif
              
              if (stmpar%morpar%moroutput%suvcor) then
                 ierr = nf90_inq_varid(imapfile, 'e_scrn', id_scrn(iid))
                 !ierr = nf90_inq_varid(imapfile, 'e_scrt', id_scrt(iid))
              end if
              
              if (stmpar%morpar%moroutput%aks) then
                 ierr = nf90_inq_varid(imapfile, 'aks', id_aks(iid))
                 ierr = nf90_inq_varid(imapfile, 'rca', id_rca(iid))
              end if
              !
              ! Suspended fractions
              if (stmpar%lsedsus .gt. 0) then
                 do j=ISED1,ISEDN
                    tmpstr = const_names(j)
                    ! Forbidden chars in NetCDF names: space, /, and more.
                    call replace_char(tmpstr,32,95) 
                    call replace_char(tmpstr,47,95) 
                    ierr = nf90_inq_varid(imapfile, trim(tmpstr), id_const(iid,j))
                 end do
              endif
           end if
              
           if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
              ierr = nf90_inq_varid(imapfile, 'e_dzdn', id_dzdn(iid))
              ierr = nf90_inq_varid(imapfile, 'e_dzdt', id_dzdt(iid))
           end if
           
           if (stmpar%morpar%moroutput%umod) then
              ierr = nf90_inq_varid(imapfile, 'umod', id_umod(iid))
           end if
           
           if (stmpar%morpar%moroutput%zumod) then
              ierr = nf90_inq_varid(imapfile, 'zumod', id_zumod(iid))
           end if
           
           if (stmpar%morpar%moroutput%ustar) then
              ierr = nf90_inq_varid(imapfile, 'ustar', id_ustar(iid))
           end if
            
           if (stmpar%morpar%moroutput%sbcuv) then
              ierr = nf90_inq_varid(imapfile, 'sbcx', id_sbcx(iid))
              ierr = nf90_inq_varid(imapfile, 'sbcy', id_sbcy(iid))
              ierr = nf90_inq_varid(imapfile, 'sbcx_reconstructed', id_sbcx_reconstructed(iid))
              ierr = nf90_inq_varid(imapfile, 'sbcy_reconstructed', id_sbcy_reconstructed(iid))
           endif

           if (stmpar%morpar%moroutput%sbwuv) then
              ierr = nf90_inq_varid(imapfile, 'sbwx', id_sbwx(iid))
              ierr = nf90_inq_varid(imapfile, 'sbwy', id_sbwy(iid))
              ierr = nf90_inq_varid(imapfile, 'sbwx_reconstructed', id_sbwx_reconstructed(iid))
              ierr = nf90_inq_varid(imapfile, 'sbwy_reconstructed', id_sbwy_reconstructed(iid))
           endif

           if (stmpar%morpar%moroutput%sswuv) then
              ierr = nf90_inq_varid(imapfile, 'sswx', id_sswx(iid))
              ierr = nf90_inq_varid(imapfile, 'sswy', id_sswy(iid))
              ierr = nf90_inq_varid(imapfile, 'sswx_reconstructed', id_sswx_reconstructed(iid))
              ierr = nf90_inq_varid(imapfile, 'sswy_reconstructed', id_sswy_reconstructed(iid))
           endif
            
           if (stmpar%morpar%moroutput%sscuv) then
              ierr = nf90_inq_varid(imapfile, 'sscx', id_sscx(iid))
              ierr = nf90_inq_varid(imapfile, 'sscy', id_sscy(iid))
              ierr = nf90_inq_varid(imapfile, 'sscx_reconstructed', id_sscx_reconstructed(iid))
              ierr = nf90_inq_varid(imapfile, 'sscy_reconstructed', id_sscy_reconstructed(iid))
           endif
           
           ierr = nf90_inq_varid(imapfile, 'sxtot', id_sxtot(iid))           
           ierr = nf90_inq_varid(imapfile, 'sytot', id_sytot(iid))
           
           ierr = nf90_inq_varid(imapfile, 'mor_bl', id_morbl(iid))
           
           select case (stmpar%morlyr%settings%iunderlyr)
           case (1)
              ierr = nf90_inq_varid(imapfile, 'bodsed', id_bodsed(iid))           
              ierr = nf90_inq_varid(imapfile, 'dpsed', id_dpsed(iid))
           case (2)
              ierr = nf90_inq_varid(imapfile, 'msed', id_msed(iid))           
              ierr = nf90_inq_varid(imapfile, 'lyrfrac', id_lyrfrac(iid))
              ierr = nf90_inq_varid(imapfile, 'thlyr', id_thlyr(iid))
              if (stmpar%morlyr%settings%iporosity>0) then
                 ierr = nf90_inq_varid(imapfile, 'poros', id_poros(iid))
              endif
           end select
        !
           if (stmpar%morpar%moroutput%taurat) then
              ierr = nf90_inq_varid(imapfile, 'taurat' ,id_taurat(iid))
           endif
           if (stmpar%morpar%moroutput%dm) then
              ierr = nf90_inq_varid(imapfile, 'dm' ,id_dm(iid))
           endif
           if (stmpar%morpar%moroutput%dg) then
              ierr = nf90_inq_varid(imapfile, 'dg' ,id_dg(iid))
           endif
           if (stmpar%morpar%moroutput%dgsd) then
              ierr = nf90_inq_varid(imapfile, 'dgsd' ,id_dgsd(iid))
           endif
           if (stmpar%morpar%moroutput%percentiles) then
              do l = 1, stmpar%morpar%nxx
                 write(dxname,'(A,I2.2)') 'DXX',l
                 ierr = nf90_inq_varid(imapfile, dxname ,id_dxx(l,iid))
              enddo
           endif
           if (stmpar%morpar%moroutput%frac) then
              ierr = nf90_inq_varid(imapfile, 'frac' ,id_frac(iid))
           endif
           if (stmpar%morpar%moroutput%mudfrac) then
              ierr = nf90_inq_varid(imapfile, 'mudfrac' ,id_mudfrac(iid))
           endif
           if (stmpar%morpar%moroutput%sandfrac) then
              ierr = nf90_inq_varid(imapfile, 'sandfrac' ,id_sandfrac(iid))
           endif
           if (stmpar%morpar%moroutput%fixfac) then
              ierr = nf90_inq_varid(imapfile, 'fixfac' ,id_fixfac(iid))
           endif
           if (stmpar%morpar%moroutput%hidexp) then
              ierr = nf90_inq_varid(imapfile, 'hidexp' ,id_hidexp(iid))
           endif
           ! Fluff layers
           if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0) then
              ierr = nf90_inq_varid(imapfile, 'mfluff' ,id_mfluff(iid))            
           end if
        endif
        
        if (bfmpar%lfbedfrmout) then
           if (bfmpar%lfbedfrm) then
              ierr = nf90_inq_varid(imapfile, 'duneheight' ,id_duneheight(iid))
              ierr = nf90_inq_varid(imapfile, 'dunelength' ,id_dunelength(iid))
           end if
           if (bfmpar%lfbedfrmrou) then
              ierr = nf90_inq_varid(imapfile, 'ksr' ,id_ksr(iid))
              ierr = nf90_inq_varid(imapfile, 'ksmr' ,id_ksmr(iid))
              ierr = nf90_inq_varid(imapfile, 'ksd' ,id_ksd(iid))
              ierr = nf90_inq_varid(imapfile, 'ks' ,id_ks(iid))
           end if
        end if
        !
        if (jased > 0 .and. .not.stm_included) then 
           ierr = nf90_inq_dimid(imapfile, 'nFrac', id_maxfracdim(iid))  
           if (jaceneqtr == 1) then 
              ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_erolaydim(iid)) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
              if (ierr /= nf90_noerr) then
                 ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_erolaydim(iid))
              end if
           else
              ierr = nf90_inq_dimid(imapfile, 'nNetNode', id_erolaydim(iid)) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
           end if
           !
           ierr = nf90_inq_varid(imapfile, 'sed', id_sed(iid))
           !
           ierr = nf90_inq_varid(imapfile, 'ero', id_ero(iid))
        endif
        ! 
        ! JRE - XBeach
        if (jawave .eq. 4) then
           ierr = nf90_inq_varid(imapfile, 'E'        , id_E(iid))
           ierr = nf90_inq_varid(imapfile, 'R'        , id_R(iid))
           ierr = nf90_inq_varid(imapfile, 'H'        , id_H(iid))
           ierr = nf90_inq_varid(imapfile, 'D'        , id_D(iid))
           ierr = nf90_inq_varid(imapfile, 'DR'       , id_DR(iid))
           ierr = nf90_inq_varid(imapfile, 'urms'     , id_urms(iid))
           ierr = nf90_inq_varid(imapfile, 'urms_cc'  , id_urmscc(iid))
           ierr = nf90_inq_varid(imapfile, 'ust'      , id_ust(iid))
           ierr = nf90_inq_varid(imapfile, 'vst'      , id_vst(iid))
           !ierr = nf90_inq_varid(imapfile, 'Fx'       , id_Fx(iid))
           !ierr = nf90_inq_varid(imapfile, 'Fy'       , id_Fy(iid))
           ierr = nf90_inq_varid(imapfile, 'Fx_cc'    , id_Fxcc(iid))
           ierr = nf90_inq_varid(imapfile, 'Fy_cc'    , id_Fycc(iid))
           !if (jamombal>0) then
              ierr = nf90_inq_varid(imapfile, 'ducxdx'    , id_ducxdx(iid))
              ierr = nf90_inq_varid(imapfile, 'ducydx'    , id_ducydx(iid))
              ierr = nf90_inq_varid(imapfile, 'ducxdy'    , id_ducxdy(iid))
              ierr = nf90_inq_varid(imapfile, 'ducydy'    , id_ducydy(iid))
              ierr = nf90_inq_varid(imapfile, 'dsdx'    , id_dsdx(iid))
              ierr = nf90_inq_varid(imapfile, 'dsdy'    , id_dsdy(iid))
           !end if
           ierr = nf90_inq_varid(imapfile, 'thetamean', id_thetamean(iid))
           ierr = nf90_inq_varid(imapfile, 'cwav'     , id_cwav(iid))
           ierr = nf90_inq_varid(imapfile, 'cgwav'    , id_cgwav(iid))
           ierr = nf90_inq_varid(imapfile, 'sigmwav'  , id_sigmwav(iid))
           
           if ( (windmodel .eq. 1) .and. (jawsource .eq. 1) ) then
              ierr = nf90_inq_varid(imapfile, 'SwE'  , id_SwE(iid))           
              ierr = nf90_inq_varid(imapfile, 'SwT'  , id_SwT(iid))   
           endif
           
        endif

        ! 1D2D boundaries
        if (nbnd1d2d > 0) then
           ierr = nf90_inq_varid(imapfile, '1d2d_flowlinknrs' , id_1d2d_edges(iid))
           ierr = nf90_inq_varid(imapfile, '1d2d_zeta'        , id_1d2d_zeta1d(iid))
           ierr = nf90_inq_varid(imapfile, '1d2d_crest_level' , id_1d2d_crest_level(iid))
           ierr = nf90_inq_varid(imapfile, '1d2d_b_2di'       , id_1d2d_b_2di(iid)) 
           ierr = nf90_inq_varid(imapfile, '1d2d_b_2dv'       , id_1d2d_b_2dv(iid)) 
           ierr = nf90_inq_varid(imapfile, '1d2d_d_2dv'       , id_1d2d_d_2dv(iid)) 
           ierr = nf90_inq_varid(imapfile, '1d2d_q_zeta'      , id_1d2d_q_zeta(iid))
           ierr = nf90_inq_varid(imapfile, '1d2d_q_lat'       , id_1d2d_q_lat(iid))
           ierr = nf90_inq_varid(imapfile, '1d2d_cfl'         , id_1d2d_cfl(iid))
           ierr = nf90_inq_varid(imapfile, '1d2d_sb'          , id_1d2d_sb(iid))
           ierr = nf90_inq_varid(imapfile, 'id_1d2d_s0_2d'    , id_1d2d_s0_2d(iid))
           ierr = nf90_inq_varid(imapfile, 'id_1d2d_s1_2d'    , id_1d2d_s1_2d(iid))
           ierr = nf90_inq_varid(imapfile, '1d2d_flow_cond'   , id_1d2d_flow_cond(iid))
        end if
        
        if ( jamaptidep.eq.1 .and. jatidep.gt.0 ) then
           if ( jaselfal.eq.0 ) then
              ierr = nf90_inq_varid(imapfile, 'TidalPotential', id_tidep(iid))
           else
              ierr = nf90_inq_varid(imapfile, 'TidalPotential_no_SAL', id_tidep(iid))
           end if
           if ( jaselfal.gt.0 ) then
              ierr = nf90_inq_varid(imapfile, 'SALPotential', id_salp(iid))
           end if
        end if
        
        if ( jamapIntTidesDiss.eq.1 .and. jaFrcInternalTides2D.gt.0 ) then
           ierr = nf90_inq_varid(imapfile, 'internal_tides_dissipation', id_inttidesdiss(iid))
        end if

        !
        ! Flow data on edges
        ierr = nf90_inq_varid(imapfile, 'unorm' , id_unorm(iid))
        ! 
        ! Flow data on edges
        ierr = nf90_inq_varid(imapfile, 'u0'    , id_u0(iid))
        ierr = nf90_inq_varid(imapfile, 'q1'    , id_q1(iid))
        ierr = nf90_inq_varid(imapfile, 'viu'   , id_viu(iid))
        ierr = nf90_inq_varid(imapfile, 'diu'   , id_diu(iid))
        !
        if (jawind/=0) then
            ierr = nf90_inq_varid(imapfile, 'windx', id_windx(iid))
            ierr = nf90_inq_varid(imapfile, 'windy', id_windy(iid))
        endif    
    endif    
    
    ! -- Start data writing (flow data) ------------------------
    if (jaseparate_ == 1) then
        itim = 1
        firststep(iid) = .true. 
    elseif (jaseparate_ == 2) then
        itim = 1
    else
        it_map   = it_map+1
        itim     = it_map ! Increment time dimension index  
    end if
    
    ! Time
    ierr = nf90_put_var(imapfile, id_time    (iid), tim, (/ itim /))
    ierr = nf90_put_var(imapfile, id_timestep(iid), dts, (/ itim /))

    !
    ! Transform uxy/ucy into Eulerian velocities,
    ! only when the user asks for it and only if we are not writing to com-file
    !
    jaeulerloc = 0
    if (jaeulervel==1 .and. jaseparate_/=2 .and. jawave.gt.0) then
       jaeulerloc = 1
    endif
    !
    call getucxucyeulmag(ndkx, workx, worky, ucmag, jaeulerloc, 0)
    !
    !  Hack to pass time varying bottom levels to SWAN
    !  Also needed for morphostatic runs in 3D
    !
    if (jaseparate_==2) then
       ! JRE: was _zcc, but this has laydim included as dimension, which does not work in 3D
       ierr = nf90_inq_varid(imapfile, 'FlowElem_bl', id_swanbl(iid))     
       ierr = nf90_put_var(imapfile, id_swanbl(iid),  -bl,   (/ 1, itim /), (/ ndxndxi, 1 /))
    end if
    !    
    ! Water level
    ierr = nf90_put_var(imapfile, id_s1(iid),  s1,   (/ 1, itim /), (/ ndxndxi, 1 /))
   
    if ( kmx==0 ) then
       ierr = nf90_put_var(imapfile, id_ucx  (iid), workx,  (/ 1, itim /), (/ ndxndxi, 1 /))
       ierr = nf90_put_var(imapfile, id_ucy  (iid), worky,  (/ 1, itim /), (/ ndxndxi, 1 /))
    endif
    
    if ( kmx>0 ) then
       call unc_append_3dflowgeom_put(imapfile, jaseparate_, itim) ! needed for 3D wave coupling on comfile: Flowelem_zw

       do kk=1,ndxndxi
          work1(:, kk) = dmiss ! For proper fill values in z-model runs.
          call getkbotktop(kk,kb,kt)
          call getlayerindices(kk, nlayb, nrlay)
          do k = kb,kt
             work1(k-kb+nlayb,kk) = workx(k)
          enddo
       enddo
       ierr = nf90_put_var(imapfile, id_ucx(iid), work1(1:kmx,1:ndxndxi), start=(/ 1, 1, itim /), count=(/ kmx, ndxndxi, 1 /))

       do kk=1,ndxndxi
          work1(:, kk) = dmiss ! For proper fill values in z-model runs.
          call getkbotktop(kk,kb,kt)
          call getlayerindices(kk, nlayb, nrlay)
          do k = kb,kt
             work1(k-kb+nlayb,kk) = worky(k)
          enddo
       enddo
       ierr = nf90_put_var(imapfile, id_ucy(iid), work1(1:kmx,1:ndxndxi), start=(/ 1, 1, itim /), count=(/ kmx, ndxndxi, 1 /))
    endif
   
    if (jaseparate_ /= 2) then
        ierr = nf90_put_var(imapfile, id_s0(iid),  s0,   (/ 1, itim /), (/ ndxndxi, 1 /))

        ierr = nf90_put_var(imapfile, id_hs(iid),  hs,   (/ 1, itim /), (/ ndxndxi, 1 /))

       ! Tau current 
       if (jawave .ne. 3) then   ! Else, get taus from subroutine tauwave (taus = taucur + tauwave). Bas; Mind for jawind!
           call gettaus(1)       ! Update taus and czs 
       elseif (jamapchezy > 0) then    
           call gettaus(2)       ! Only update czs 
       endif
       if (jamaptaucurrent > 0) then
           ierr = nf90_put_var(imapfile, id_taus(iid), taus,  (/ 1, itim /), (/ ndxndxi, 1 /))
       endif    
       if (jamapchezy > 0) then
           ierr = nf90_put_var(imapfile, id_czs(iid), czs,  (/ 1, itim /), (/ ndxndxi, 1 /))
       endif

       ! Velocities
       if ( kmx>0 ) then
!         3D
          call reconstructucz(0)
          !
          do kk=1,ndxndxi
             work1(:, kk) = dmiss ! For proper fill values in z-model runs.
             call getkbotktop(kk,kb,kt)
             call getlayerindices(kk, nlayb, nrlay)
             do k = kb,kt
                work1(k-kb+nlayb,kk) = ucz(k)
             enddo
          enddo
          ierr = nf90_put_var(imapfile, id_ucz(iid), work1(1:kmx,1:ndxndxi), start=(/ 1, 1, itim /), count=(/ kmx, ndxndxi, 1 /))

          ierr = nf90_put_var(imapfile, id_ucxa(iid), ucxq(1:ndxndxi), start=(/ 1, itim /), count=(/ ndxndxi, 1 /))
          ierr = nf90_put_var(imapfile, id_ucya(iid), ucyq(1:ndxndxi), start=(/ 1, itim /), count=(/ ndxndxi, 1 /))

          do kk=1,ndxndxi
             work0(:, kk) = dmiss ! For proper fill values in z-model runs.
             call getkbotktop(kk,kb,kt)
             call getlayerindices(kk, nlayb, nrlay)
             do k = kb-1,kt
                work0(k-kb+nlayb,kk) = ww1(k)
             enddo
          enddo
          ierr = nf90_put_var(imapfile, id_ww1(iid), work0(0:kmx,1:ndxndxi), start=(/ 1, 1, itim /), count=(/ kmx+1, ndxndxi, 1 /))

          do LL=1,lnx
             work1(:, LL) = dmiss ! For proper fill values in z-model runs.
             call getLbotLtopmax(LL,Lb,Ltx)
             call getlayerindicesLmax(LL, nlaybL, nrlayLx)
             do L = Lb,Ltx
                 work1(L-Lb+nlaybL,LL) = u1(L)
             enddo
          enddo
          ierr = nf90_put_var(imapfile, id_unorm(iid), work1(1:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx, lnx, 1 /))
          
          do LL=1,lnx
             work1(:, LL) = dmiss ! For proper fill values in z-model runs.
             call getLbotLtopmax(LL,Lb,Ltx)
             call getlayerindicesLmax(LL, nlaybL, nrlayLx)
             do L = Lb,Ltx
                 work1(L-Lb+nlaybL,LL) = u0(L)
             enddo
          enddo
          ierr = nf90_put_var(imapfile, id_u0(iid)   , work1(1:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx, lnx, 1 /))

          do LL=1,lnx    
             work1(:, LL) = dmiss ! For proper fill values in z-model runs.
             call getLbotLtopmax(LL,Lb,Ltx)
             call getlayerindicesLmax(LL, nlaybL, nrlayLx)
             do L = Lb,Ltx
                 work1(L-Lb+nlaybL,LL) = q1(L)
             enddo
          enddo
          ierr = nf90_put_var(imapfile, id_q1(iid)   , work1(1:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx, lnx, 1 /))

          do LL=1,lnx
             work1(:, LL) = dmiss ! For proper fill values in z-model runs.
             call getLbotLtopmax(LL,Lb,Ltx)
             call getlayerindicesLmax(LL, nlaybL, nrlayLx)
             if (javiusp == 1) then       ! user specified part              
                 vicc = viusp(LL)
             else
                 vicc = vicouv 
             endif
             do L = Lb,Ltx
                 work1(L-Lb+nlaybL,LL) = viu(L) + vicc
             enddo
          enddo
          ierr = nf90_put_var(imapfile, id_viu(iid)   , work1(1:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx, lnx, 1 /))
          
          do LL=1,lnx
             work1(:, LL) = dmiss ! For proper fill values in z-model runs.
             call getLbotLtopmax(LL,Lb,Ltx)
             call getlayerindicesLmax(LL, nlaybL, nrlayLx)
             if (jadiusp == 1) then           
                 dicc = diusp(LL)
             else
                 dicc = dicouv 
             endif
             do L = Lb,Ltx
                 work1(L-Lb+nlaybL,LL) = viu(L) * 0.7 + dicc
             enddo
          enddo
          ierr = nf90_put_var(imapfile, id_diu(iid)   , work1(1:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx, lnx, 1 /))

          do kk=1,ndxndxi
             work1(:, kk) = dmiss ! For proper fill values in z-model runs.
             call getkbotktop(kk,kb,kt)
             call getlayerindices(kk, nlayb, nrlay)  
             do k = kb,kt
                work1(k-kb+nlayb, kk) = rho(k)
             enddo
          enddo
          ierr = nf90_put_var(imapfile, id_rho(iid), work1(1:kmx,1:ndxndxi), start=(/ 1, 1, itim /), count=(/ kmx, ndxndxi, 1 /))
          
          if (jamaptur > 0 .and. iturbulencemodel >= 3) then
             do LL=1,lnx    
                work0(:, LL) = dmiss ! For proper fill values in z-model runs.
                call getLbotLtopmax(LL,Lb,Ltx)
                call getlayerindicesLmax(LL, nlaybL, nrlayLx)
                do L = Lb-1,Ltx
                   work0(L-Lb+nlaybL,LL) = turkin1(L)
                enddo
             enddo
             ierr = nf90_put_var(imapfile, id_turkin1(iid)   , work0(0:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx+1, lnx, 1 /))
             do LL=1,lnx    
                work0(:, LL) = dmiss ! For proper fill values in z-model runs.
                call getLbotLtopmax(LL,Lb,Ltx)
                call getlayerindicesLmax(LL, nlaybL, nrlayLx)
                do L = Lb-1,Ltx
                   work0(L-Lb+nlaybL,LL) = tureps1(L)
                enddo
             enddo
             ierr = nf90_put_var(imapfile, id_tureps1(iid)   , work0(0:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx+1, lnx, 1 /))
             do LL=1,lnx    
                work0(:, LL) = dmiss ! For proper fill values in z-model runs.
                call getLbotLtopmax(LL,Lb,Ltx)
                call getlayerindicesLmax(LL, nlaybL, nrlayLx)
                do L = Lb-1,Ltx
                   work0(L-Lb+nlaybL,LL) = vicwwu(L)
                enddo
             enddo
             ierr = nf90_put_var(imapfile, id_vicwwu(iid)   , work0(0:kmx,1:lnx), start=(/ 1, 1, itim /), count=(/ kmx+1, lnx, 1 /))
          endif
          
       end if

       if( jasecflow > 0 .and. jamapspir > 0) then
          ierr = nf90_put_var(imapfile, id_spirint(iid), spirint, (/ 1, itim /), (/ ndxndxi, 1 /))
          if ( kmx == 0 ) then 
             ierr = nf90_put_var(imapfile, id_spircrv(iid), spircrv, (/ 1, itim /), (/ ndxndxi, 1 /))
          endif
       endif

       if ( kmx == 0 ) then
           ierr = nf90_put_var(imapfile, id_unorm(iid), u1 ,  (/ 1, itim /), (/ lnx , 1 /))
           ierr = nf90_put_var(imapfile, id_u0   (iid), u0 ,  (/ 1, itim /), (/ lnx , 1 /))
           ierr = nf90_put_var(imapfile, id_q1 (iid)    , q1     , (/ 1, itim /), (/ lnx    , 1 /))
           
           do LL=1,lnx
              work1(:,LL) = dmiss
              if (javiusp == 1) then       ! user specified part              
                 vicc = viusp(LL)
              else
                 vicc = vicouv 
              endif
              work1(1,LL) = viu(LL) + vicc
           enddo
           ierr = nf90_put_var(imapfile, id_viu (iid), work1(1:1,1:lnx) ,  (/ 1, itim /), (/ lnx , 1 /))

           do LL=1,lnx
              work1(:,LL) = dmiss
              if (jadiusp == 1) then           
                 dicc = diusp(LL)
              else
                 dicc = dicouv 
              endif
              work1(1,LL) = viu(LL) * 0.7 + dicc
          enddo
           ierr = nf90_put_var(imapfile, id_diu (iid), work1(1:1,1:lnx) ,  (/ 1, itim /), (/ lnx , 1 /))
       end if

    end if
    
    if (jaseparate_ /= 2) then
        
       
       ! Salinity
       if (jamapsal > 0 .and. jasal > 0) then
          if ( kmx>0 ) then
!            3D
             !do kk=1,ndxndxi
             !   call getkbotktop(kk,kb,kt)
             !   ierr = nf90_put_var(imapfile, id_sa1(iid), sa1(kb:kt), (/ 1, kk, itim /), (/ kt-kb+1, 1, 1 /))
             !end do
             do kk=1,ndxndxi
                 work1(:,kk) = dmiss ! For proper fill values in z-model runs.
                call getkbotktop(kk,kb,kt)
                call getlayerindices(kk, nlayb, nrlay)  
                do k = kb,kt
                   work1(k-kb+nlayb, kk) = sa1(k)
                enddo
             end do
             ierr = nf90_put_var(imapfile, id_sa1(iid), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
          else
             ierr = nf90_put_var(imapfile, id_sa1(iid), sa1, (/ 1, itim /), (/ ndxndxi, 1 /))
          end if
       endif
    
       if (jamaptem > 0 .and. jatem > 0) then
          if ( kmx>0 ) then ! 3D
             !do kk=1,ndxndxi
             !   call getkbotktop(kk,kb,kt)
             !   ierr = nf90_put_var(imapfile, id_tem1(iid), tem1(kb:kt), (/ 1, kk, itim /), (/ kt-kb+1, 1, 1 /))
             !end do
             do kk=1,ndxndxi
                work1(:,kk) = dmiss ! For proper fill values in z-model runs.
                call getkbotktop(kk,kb,kt)
                call getlayerindices(kk, nlayb, nrlay)  
                do k = kb,kt
                   work1(k-kb+nlayb, kk) = constituents(itemp,k)
                enddo
             end do
             ierr = nf90_put_var(imapfile, id_tem1(iid), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
          else
             do k = 1, ndxndxi
                tem1(k) = constituents(itemp, k)
             enddo   
             ierr = nf90_put_var(imapfile, id_tem1(iid), tem1, (/ 1, itim /), (/ ndxndxi, 1 /))
          end if
       endif
       
!      tracers
       if (jamapconst > 0 .and. ITRA1 > 0) then ! Note: numtracers is only counting tracer boundaries. SPvdP: now also includes tracers with initial conditions only
          allocate(dum(NdxNdxi))
          
          do j=ITRA1,ITRAN
             if ( kmx>0 ) then
!               3D
                do kk=1,ndxndxi
                   work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                   call getkbotktop(kk,kb,kt)
                   call getlayerindices(kk, nlayb, nrlay)  
                   do k = kb,kt
                      work1(k-kb+nlayb, kk) = constituents(j,k)
                   enddo
                end do
                ierr = nf90_put_var(imapfile, id_const(iid,j), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
                !   if ( ierr.ne.0 ) exit  ! probably newly added tracer in the GUI
             else
                do kk=1,NdxNdxi
                   dum(kk) = constituents(j,kk)
                end do
                ierr = nf90_put_var(imapfile, id_const(iid,j), dum, (/ 1, itim /), (/ NdxNdxi, 1 /) )
             end if
          end do
          
          if ( allocated(dum) ) deallocate(dum)
       end if
    
       ! water quality bottom variables outputs
       if (numwqbots > 0) then
          allocate(dum(NdxNdxi))
          do j=1,numwqbots
             do kk=1,NdxNdxi
                dum(kk) = wqbot(j,kk)
             end do
             ierr = nf90_put_var(imapfile, id_wqb(iid,j), dum, (/ 1, itim /), (/ NdxNdxi, 1 /) )
          end do
          if ( allocated(dum) ) deallocate(dum)
       end if

       ! WAQ extra outputs
       if (jawaqproc > 0) then
          do j=1,noout_map
             if (outvar(j)>0)then
                work1 = DMISS ! For proper fill values in z-model runs.
                if ( kmx>0 ) then
!                  3D
                   do kk=1,ndxndxi
                      work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                      call getkbotktop(kk,kb,kt)
                      call getlayerindices(kk, nlayb, nrlay)  
                      do k = kb,kt
                         work1(k-kb+nlayb, kk) = waqoutputs(j,k-kbx+1)
                      enddo
                   end do
                   ierr = nf90_put_var(imapfile, id_waq(iid,j), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
                else
                   call realloc(dum,NdxNdxi, keepExisting=.false.)
                   do kk=1,NdxNdxi
                      dum(kk) = waqoutputs(j,kk)
                   end do
                   ierr = nf90_put_var(imapfile, id_waq(iid,j), dum, (/ 1, itim /), (/ NdxNdxi, 1 /) )
                   if (allocated(dum)) deallocate(dum)
                end if
             end if
          end do
          do j=1,noout_statt
             jj = noout_user + j
             if (outvar(jj)>0)then
                work1 = DMISS ! For proper fill values in z-model runs.
                if ( kmx>0 ) then
!                  3D
                   do kk=1,ndxndxi
                      work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                      call getkbotktop(kk,kb,kt)
                      call getlayerindices(kk, nlayb, nrlay)  
                      do k = kb,kt
                         work1(k-kb+nlayb, kk) = waqoutputs(jj,k-kbx+1)
                      enddo
                   end do
                   ierr = nf90_put_var(imapfile, id_wqst(iid,j), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
                else
                   call realloc(dum,NdxNdxi, keepExisting=.false.)
                   do kk=1,NdxNdxi
                      dum(kk) = waqoutputs(jj,kk)
                   end do
                   ierr = nf90_put_var(imapfile, id_wqst(iid,j), dum, (/ 1, itim /), (/ NdxNdxi, 1 /) )
                   if (allocated(dum)) deallocate(dum)
                end if
             end if
          end do
          if (comparereal(tim, ti_mape, eps10) == 0) then
             do j=1,noout_state
                jj = noout_user + noout_statt + j
                if (outvar(jj)>0)then
                   work1 = DMISS ! For proper fill values in z-model runs.
                   if ( kmx>0 ) then
!                     3D
                      do kk=1,ndxndxi
                         work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                         call getkbotktop(kk,kb,kt)
                         call getlayerindices(kk, nlayb, nrlay)  
                         do k = kb,kt
                            work1(k-kb+nlayb, kk) = waqoutputs(jj,k-kbx+1)
                         enddo
                      end do
                      ierr = nf90_put_var(imapfile, id_wqse(iid,j), work1(1:kmx,1:ndxndxi), (/ 1, 1 /), (/ kmx, ndxndxi, 1 /))
                   else
                      call realloc(dum,NdxNdxi, keepExisting=.false.)
                      do kk=1,NdxNdxi
                         dum(kk) = waqoutputs(jj,kk)
                      end do
                      ierr = nf90_put_var(imapfile, id_wqse(iid,j), dum, (/ 1 /), (/ NdxNdxi, 1 /) )
                      if (allocated(dum)) deallocate(dum)
                   end if
                end if
             end do
          end if
       end if

       if (jased>0 .and. stm_included) then
          if (stmpar%lsedsus > 0) then
             if (kmx > 0) then
                do kk = 1, ndxndxi
                   call getkbotktop(kk, kb, kt)
                   ierr = nf90_put_var(imapfile, id_ws(iid), mtd%ws(kb:kt,:), (/ 1, kk , 1 , itim /), (/ kt-kb+1, 1 , stmpar%lsedsus , 1 /))
                end do
             else 
                ierr = nf90_put_var(imapfile, id_ws(iid),mtd%ws, (/ 1 , 1 , itim /), (/ ndxndxi , stmpar%lsedsus , 1 /))
             end if
             !
             ! equilibrium concentration, 2D only
             if (kmx == 0) then
                ierr = nf90_put_var(imapfile, id_rsedeq(iid), sedtra%rsedeq(1:ndxi, :), (/ 1 , 1 , itim /), (/ ndxi , stmpar%lsedsus , 1 /))
             end if
                                    
             if (stmpar%morpar%moroutput%sourcesink) then
                ierr = nf90_put_var(imapfile, id_sourse(iid) , sedtra%sourse(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedsus, 1 /))
                ierr = nf90_put_var(imapfile, id_sinkse(iid) , sedtra%sinkse(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedsus, 1 /))
             endif
             
             if (stmpar%morpar%moroutput%suvcor) then
                ierr = nf90_put_var(imapfile, id_scrn(iid) , sedtra%e_scrn(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedsus, 1 /))
                !ierr = nf90_put_var(imapfile, id_scrt(iid) , sedtra%e_scrt(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedsus, 1 /))
             endif
                                    
             if (stmpar%morpar%moroutput%aks) then
                ierr = nf90_put_var(imapfile, id_aks(iid) , sedtra%aks(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedsus, 1 /))
                ierr = nf90_put_var(imapfile, id_rca(iid) , sedtra%rca(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedsus, 1 /))
             endif
             !
             ! Suspended fractions
             call realloc(dum,NdxNdxi, keepExisting=.false.)
             do j=ISED1,ISEDN
                if ( kmx>0 ) then
      !            3D
                   do kk=1,ndxndxi
                      call getkbotktop(kk,kb,kt)
                      do k = kb,kt
                         ! TODO: UNST-976, incorrect for Z-layers:
                         work1(k-kb+1,kk) = constituents(j,k)
                      enddo
                   end do
                   ierr = nf90_put_var(imapfile, id_const(iid,j), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
                else
                   do kk=1,NdxNdxi
                      dum(kk) = constituents(j,kk)
                   end do
                   ierr = nf90_put_var(imapfile, id_const(iid,j), dum, (/ 1, itim /), (/ NdxNdxi, 1 /) )
                end if
             end do
             if ( allocated(dum) ) deallocate(dum)
          end if
             
          if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
             ierr = nf90_put_var(imapfile, id_dzdn(iid), sedtra%e_dzdn, (/ 1, itim /), (/ lnxi , 1 /))
             ierr = nf90_put_var(imapfile, id_dzdt(iid), sedtra%e_dzdt, (/ 1, itim /), (/ lnxi , 1 /))
          end if
          
          if (stmpar%morpar%moroutput%umod) then
             ierr = nf90_put_var(imapfile, id_umod(iid), sedtra%umod, (/ 1, itim /), (/ ndxi, 1 /))
          end if
          
          if (stmpar%morpar%moroutput%zumod) then
             ierr = nf90_put_var(imapfile, id_zumod(iid), sedtra%zumod, (/ 1, itim /), (/ ndxi, 1 /))
          end if
          
          if (stmpar%morpar%moroutput%ustar) then
             ierr = nf90_put_var(imapfile, id_ustar(iid), sqrt(sedtra%ust2), (/ 1, itim /), (/ ndxi, 1 /))
          end if
    
          if (stmpar%morpar%moroutput%sbcuv) then
             do l = 1, stmpar%lsedtot
                select case(stmpar%morpar%moroutput%transptype)
                case (0)
                   rhol = 1d0
                case (1)
                   rhol = stmpar%sedpar%cdryb(l)
                case (2)
                   rhol = stmpar%sedpar%rhosol(l)
                end select
                sedtra%sbcx(:,l) = sedtra%sbcx(:,l)/rhol
                sedtra%sbcy(:,l) = sedtra%sbcy(:,l)/rhol
             end do
             ierr = nf90_put_var(imapfile, id_sbcx(iid) , sedtra%sbcx(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
             ierr = nf90_put_var(imapfile, id_sbcy(iid) , sedtra%sbcy(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
          endif
          
          if (stmpar%morpar%moroutput%sbwuv) then
             do l = 1, stmpar%lsedtot
                select case(stmpar%morpar%moroutput%transptype)
                case (0)
                   rhol = 1d0
                case (1)
                   rhol = stmpar%sedpar%cdryb(l)
                case (2)
                   rhol = stmpar%sedpar%rhosol(l)
                end select
                sedtra%sbwx(:,l) = sedtra%sbwx(:,l)/rhol
                sedtra%sbwy(:,l) = sedtra%sbwy(:,l)/rhol
             end do
             ierr = nf90_put_var(imapfile, id_sbwx(iid) , sedtra%sbwx(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
             ierr = nf90_put_var(imapfile, id_sbwy(iid) , sedtra%sbwy(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
          endif
          
          if (stmpar%morpar%moroutput%sswuv) then
             do l = 1, stmpar%lsedtot
                select case(stmpar%morpar%moroutput%transptype)
                case (0)
                   rhol = 1d0
                case (1)
                   rhol = stmpar%sedpar%cdryb(l)
                case (2)
                   rhol = stmpar%sedpar%rhosol(l)
                end select
                sedtra%sswx(:,l) = sedtra%sswx(:,l)/rhol
                sedtra%sswy(:,l) = sedtra%sswy(:,l)/rhol
             end do
             ierr = nf90_put_var(imapfile, id_sswx(iid) , sedtra%sswx(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
             ierr = nf90_put_var(imapfile, id_sswy(iid) , sedtra%sswy(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
          endif
          
          if (stmpar%morpar%moroutput%sscuv) then
             call realloc(toutputx, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
             call realloc(toutputy, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
             do l = 1, stmpar%lsedsus
                select case(stmpar%morpar%moroutput%transptype)
                case (0)
                   rhol = 1d0
                case (1)
                   rhol = stmpar%sedpar%cdryb(sedtot2sedsus(sedtot2sedsus(l)))
                case (2)
                   rhol = stmpar%sedpar%rhosol(sedtot2sedsus(sedtot2sedsus(l)))
                end select
                toutputx(:,l) = sedtra%sscx(:,sedtot2sedsus(l))/rhol         ! mapping necessary because dim(sscx)=lsedtot
                toutputy(:,l) = sedtra%sscy(:,sedtot2sedsus(l))/rhol
             end do
             ierr = nf90_put_var(imapfile, id_sscx(iid) , toutputx(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedsus, 1 /))
             ierr = nf90_put_var(imapfile, id_sscy(iid) , toutputy(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedsus, 1 /))
          endif

          ! Get cell centre transport values, removed from fm_erosed and fm_bott3d, and calculated here and stored in sscx, sscy, sbcx, sbcy, sbwx, sbwy, sswx, sswy
          call reconstructsedtransports()
          
          if (stmpar%morpar%moroutput%sbcuv) then
             do l = 1, stmpar%lsedtot
                select case(stmpar%morpar%moroutput%transptype)
                case (0)
                   rhol = 1d0
                case (1)
                   rhol = stmpar%sedpar%cdryb(l)
                case (2)
                   rhol = stmpar%sedpar%rhosol(l)
                end select
                sedtra%sbcx(:,l) = sedtra%sbcx(:,l)/rhol
                sedtra%sbcy(:,l) = sedtra%sbcy(:,l)/rhol
             end do
             ierr = nf90_put_var(imapfile, id_sbcx_reconstructed(iid) , sedtra%sbcx(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
             ierr = nf90_put_var(imapfile, id_sbcy_reconstructed(iid) , sedtra%sbcy(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
          endif
          
          if (stmpar%morpar%moroutput%sbwuv) then
             do l = 1, stmpar%lsedtot
                select case(stmpar%morpar%moroutput%transptype)
                case (0)
                   rhol = 1d0
                case (1)
                   rhol = stmpar%sedpar%cdryb(l)
                case (2)
                   rhol = stmpar%sedpar%rhosol(l)
                end select
                sedtra%sbwx(:,l) = sedtra%sbwx(:,l)/rhol
                sedtra%sbwy(:,l) = sedtra%sbwy(:,l)/rhol
             end do
             ierr = nf90_put_var(imapfile, id_sbwx_reconstructed(iid) , sedtra%sbwx(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
             ierr = nf90_put_var(imapfile, id_sbwy_reconstructed(iid) , sedtra%sbwy(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
          endif
          
          if (stmpar%morpar%moroutput%sswuv) then
             do l = 1, stmpar%lsedtot
                select case(stmpar%morpar%moroutput%transptype)
                case (0)
                   rhol = 1d0
                case (1)
                   rhol = stmpar%sedpar%cdryb(l)
                case (2)
                   rhol = stmpar%sedpar%rhosol(l)
                end select
                sedtra%sswx(:,l) = sedtra%sswx(:,l)/rhol
                sedtra%sswy(:,l) = sedtra%sswy(:,l)/rhol
             end do
             ierr = nf90_put_var(imapfile, id_sswx_reconstructed(iid) , sedtra%sswx(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
             ierr = nf90_put_var(imapfile, id_sswy_reconstructed(iid) , sedtra%sswy(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
          endif
          
          if (stmpar%morpar%moroutput%sscuv) then
             call realloc(toutputx, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
             call realloc(toutputy, (/ndx, stmpar%lsedsus /), keepExisting=.false., fill = -999d0)
             do l = 1, stmpar%lsedsus
                select case(stmpar%morpar%moroutput%transptype)
                case (0)
                   rhol = 1d0
                case (1)
                   rhol = stmpar%sedpar%cdryb(sedtot2sedsus(sedtot2sedsus(l)))
                case (2)
                   rhol = stmpar%sedpar%rhosol(sedtot2sedsus(sedtot2sedsus(l)))
                end select
                toutputx(:,l) = sedtra%sscx(:,sedtot2sedsus(l))/rhol         ! mapping necessary because dim(sscx)=lsedtot
                toutputy(:,l) = sedtra%sscy(:,sedtot2sedsus(l))/rhol
             end do
             ierr = nf90_put_var(imapfile, id_sscx_reconstructed(iid) , toutputx(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedsus, 1 /))
             ierr = nf90_put_var(imapfile, id_sscy_reconstructed(iid) , toutputy(1:ndxi, :),  (/ 1, 1, itim /), (/ ndxi, stmpar%lsedsus, 1 /))
          endif
          
          do l = 1, stmpar%lsedtot
             call realloc(toutputx, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
             call realloc(toutputy, (/ndx, stmpar%lsedtot /), keepExisting=.false., fill = -999d0)
             select case(stmpar%morpar%moroutput%transptype)
             case (0)
                rhol = 1d0
             case (1)
                rhol = stmpar%sedpar%cdryb(l)
             case (2)
                rhol = stmpar%sedpar%rhosol(l)
             end select
             toutputx(:,l) = sedtra%sxtot(:,l)/rhol
             toutputy(:,l) = sedtra%sytot(:,l)/rhol
          end do
          ierr = nf90_put_var(imapfile, id_sxtot(iid), toutputx(1:ndxi, :), (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))           
          ierr = nf90_put_var(imapfile, id_sytot(iid), toutputy(1:ndxi, :), (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
          
          if (stmpar%morpar%bedupd) then
             ierr = nf90_put_var(imapfile, id_morbl(iid), bl(1:ndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
          end if
                 
          select case (stmpar%morlyr%settings%iunderlyr)
          case (1)
             ierr = nf90_put_var(imapfile, id_bodsed(iid), stmpar%morlyr%state%bodsed(:, 1:ndxi), (/ 1, 1, itim /), (/ stmpar%lsedtot, ndxi, 1 /))
             ierr = nf90_put_var(imapfile, id_dpsed(iid), stmpar%morlyr%state%dpsed(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
          case (2)
             !
             ! Calculate values for lyrfrac and porosity
             !
             ! lyrfrac
             if (.not. allocated(frac) ) allocate( frac(stmpar%lsedtot,1:stmpar%morlyr%settings%nlyr,1:ndx  ) )
             frac = -999d0
             do l = 1, stmpar%lsedtot
                if (stmpar%morlyr%settings%iporosity==0) then
                   dens = stmpar%sedpar%cdryb(l)
                else
                   dens = stmpar%sedpar%rhosol(l)
                endif
                do k = 1, stmpar%morlyr%settings%nlyr
                   do nm = 1, ndxi
                      if (stmpar%morlyr%state%thlyr(k,nm)>0.0_fp) then
                           frac(l, k, nm) = stmpar%morlyr%state%msed(l, k, nm)/(dens*stmpar%morlyr%state%svfrac(k, nm) * &
                                            stmpar%morlyr%state%thlyr(k, nm))
                      else
                           frac(l, k, nm) = 0d0
                      endif
                   enddo
                enddo
             enddo
             !
             if (stmpar%morlyr%settings%iporosity>0) then
                if (.not. allocated(poros) ) allocate( poros(1:stmpar%morlyr%settings%nlyr, 1:ndx ) )
                poros = 1d0-stmpar%morlyr%state%svfrac
             endif
             !
             ! Avoid stack overflow issues with large models
             do l = 1, stmpar%lsedtot
                ierr = nf90_put_var(imapfile, id_msed(iid), stmpar%morlyr%state%msed(l,:,1:ndxi), (/ l, 1, 1, itim /), (/ 1, stmpar%morlyr%settings%nlyr, ndxi, 1 /))
                ierr = nf90_put_var(imapfile, id_lyrfrac(iid), frac(l,:,1:ndxi), (/ l, 1, 1, itim /), (/ 1, stmpar%morlyr%settings%nlyr, ndxi, 1 /))
             end do
             ierr = nf90_put_var(imapfile, id_thlyr(iid), stmpar%morlyr%state%thlyr(:,1:ndxi), (/ 1, 1, itim /), (/ stmpar%morlyr%settings%nlyr, ndxi, 1 /))
             if (stmpar%morlyr%settings%iporosity>0) then
                ierr = nf90_put_var(imapfile, id_poros(iid), poros(:,1:ndxi), (/ 1, 1, itim /), (/ stmpar%morlyr%settings%nlyr, ndxi, 1 /))
             endif
          end select

          if (stmpar%morpar%moroutput%taurat) then
             ierr = nf90_put_var(imapfile, id_taurat(iid), sedtra%taurat(1:ndxi,:), (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
          endif
          if (stmpar%morpar%moroutput%dm) then
             ierr = nf90_put_var(imapfile, id_dm(iid), sedtra%dm(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
             
          endif
          if (stmpar%morpar%moroutput%dg) then
             ierr = nf90_put_var(imapfile, id_dg(iid), sedtra%dg(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
          endif
          if (stmpar%morpar%moroutput%dgsd) then
             ierr = nf90_put_var(imapfile, id_dgsd(iid), sedtra%dgsd(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
          endif
          if (stmpar%morpar%moroutput%percentiles) then    ! JRE to do: check with Arthur
             call realloc(dum,ndxndxi, keepExisting=.false.)
             do l = 1, stmpar%morpar%nxx
                do kk=1,NdxNdxi
                   dum(kk) = sedtra%dxx(kk, l)
                end do
                ierr = nf90_put_var(imapfile, id_dxx(l,iid), dum, (/ 1, itim /), (/ ndxi, 1 /))
             enddo
          endif
          if (stmpar%morpar%moroutput%frac) then
             ierr = nf90_put_var(imapfile, id_frac(iid), sedtra%frac(1:ndxi, :), (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
          endif
          if (stmpar%morpar%moroutput%mudfrac) then
             ierr = nf90_put_var(imapfile, id_mudfrac(iid), sedtra%mudfrac(1:ndxi), (/ 1, itim /), (/ ndxi,  1 /))
          endif
          if (stmpar%morpar%moroutput%sandfrac) then
             ierr = nf90_put_var(imapfile, id_sandfrac(iid), sedtra%sandfrac(1:ndxi), (/ 1, itim /), (/ ndxi,  1 /))
          endif
          if (stmpar%morpar%moroutput%fixfac) then
             ierr = nf90_put_var(imapfile, id_fixfac(iid), sedtra%fixfac(1:ndxi,:), (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
          endif
          if (stmpar%morpar%moroutput%hidexp) then
             ierr = nf90_put_var(imapfile, id_hidexp(iid), sedtra%hidexp(1:ndxi,:), (/ 1, 1, itim /), (/ ndxi, stmpar%lsedtot, 1 /))
          endif
          ! Fluff layers
          if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0) then
             do l = 1, stmpar%lsedsus
                call realloc(toutput, ndx, keepExisting=.false., fill=-999d0)
                toutput = stmpar%morpar%flufflyr%mfluff(l,1:ndx)
                ierr = nf90_put_var(imapfile, id_mfluff(iid), toutput(1:ndxi), (/ 1, l, itim /), (/ ndxi, 1, 1 /))
             end do
          end if
       endif ! stm
       
       ! Bedform pars
       if (bfmpar%lfbedfrmout) then
          if (bfmpar%lfbedfrm) then
             ierr = nf90_put_var(imapfile, id_duneheight(iid), bfmpar%duneheight(1:ndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
             ierr = nf90_put_var(imapfile, id_dunelength(iid), bfmpar%dunelength(1:ndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
          end if
          if (bfmpar%lfbedfrmrou) then
             ierr = nf90_put_var(imapfile, id_ksr(iid), bfmpar%rksr(1:ndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
             ierr = nf90_put_var(imapfile, id_ksmr(iid), bfmpar%rksmr(1:ndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
             ierr = nf90_put_var(imapfile, id_ksd(iid), bfmpar%rksd(1:ndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
             
             do k = 1,ndxi
                rks(k) = sqrt(bfmpar%rksr(k)**2 + bfmpar%rksmr(k)**2 + bfmpar%rksd(k)**2)
             enddo
             ierr = nf90_put_var(imapfile, id_ks(iid), rks(1:ndxi), (/ 1, itim /), (/ ndxndxi, 1 /))
          end if
       end if
       ! Sediment Herman
       if (jased > 0 .and. .not.stm_included) then 
          ierr = nf90_put_var(imapfile, id_sed(iid), sed, (/ 1, 1, itim /), (/ mxgr, ndxndxi, 1 /))
          ierr = nf90_put_var(imapfile, id_ero(iid), grainlay, (/ 1, 1, itim /), (/ mxgr, size(grainlay,2) , 1 /))

          ierr = nf90_put_var(imapfile, id_bl(iid), bl, (/ 1, itim /), (/ ndxndxi , 1 /))
          if (jaceneqtr .ne. 1) then
              ierr = nf90_put_var(imapfile, id_zk(iid), zk, (/ 1, itim /), (/ numk , 1 /))
          endif


       ! TODO: AvD: size(grainlay,2) is always correct (mxn), but we have a problem if jaceneqtr==2 and mxn/=numk,
       ! because then the dimension for ero is set to nNetNode, and coordinate attribute refers to NetNode_x
       ! (both length numk), whereas ero itself is shorter than numk.
       endif

       ! 1D2D boundaries
       if (nbnd1d2d > 0) then
          ierr = nf90_put_var(imapfile, id_1d2d_zeta1d(iid),      zbnd1d2d1,  (/ 1, itim /), (/ nbnd1d2d, 1 /))
          ierr = nf90_put_var(imapfile, id_1d2d_crest_level(iid), zcrest1d2d, (/ 1, itim /), (/ nbnd1d2d, 1 /))
          ierr = nf90_put_var(imapfile, id_1d2d_b_2di(iid),       b_2di,      (/ 1, itim /), (/ nbnd1d2d, 1 /))
          ierr = nf90_put_var(imapfile, id_1d2d_b_2dv(iid),       b_2dv,      (/ 1, itim /), (/ nbnd1d2d, 1 /))
          ierr = nf90_put_var(imapfile, id_1d2d_d_2dv(iid),       d_2dv,      (/ 1, itim /), (/ nbnd1d2d, 1 /)) 
          ierr = nf90_put_var(imapfile, id_1d2d_q_zeta(iid),      qzeta_1d2d, (/ 1, itim /), (/ nbnd1d2d, 1 /))
          ierr = nf90_put_var(imapfile, id_1d2d_q_lat(iid),       qlat_1d2d,  (/ 1, itim /), (/ nbnd1d2d, 1 /))
          ierr = nf90_put_var(imapfile, id_1d2d_cfl(iid),         cfl,        (/ 1, itim /), (/ nbnd1d2d, 1 /))
          ierr = nf90_put_var(imapfile, id_1d2d_sb(iid),          sb_1d2d,    (/ 1, itim /), (/ nbnd1d2d, 1 /))
          ierr = nf90_put_var(imapfile, id_1d2d_s1_2d(iid),       s1_2d,      (/ 1, itim /), (/ nbnd1d2d, 1 /))
          ierr = nf90_put_var(imapfile, id_1d2d_s0_2d(iid),       s0_2d,      (/ 1, itim /), (/ nbnd1d2d, 1 /))
          ierr = nf90_put_var(imapfile, id_1d2d_flow_cond(iid),   FlowCond,   (/ 1, itim /), (/ nbnd1d2d, 1 /))
       end if
       
       if ( jatidep.gt.0 .and. jamaptidep.eq.1 ) then
          if ( jaselfal.eq.0 ) then
             do k=1,Ndx
                workx(k) = tidep(1,k)
             end do
             ierr = nf90_put_var(imapfile, id_tidep(iid), workx,  (/ 1, itim /), (/ ndxndxi, 1 /))
          else ! write potential without SAL and SAL potential
             do k=1,Ndx
                workx(k) = tidep(1,k) - tidep(2,k)
!                worky(k) = tidep(2,k)
             end do
             ierr = nf90_put_var(imapfile, id_tidep(iid), workx,  (/ 1, itim /), (/ ndxndxi, 1 /))
!             ierr = nf90_put_var(imapfile, id_salp(iid),  worky,  (/ 1, itim /), (/ ndxndxi, 1 /))
          end if
       end if
       if ( jaselfal.gt.0 .and. jamapselfal.eq.1 ) then
          do k=1,Ndx
             worky(k) = tidep(2,k)
          end do
          ierr = nf90_put_var(imapfile, id_salp(iid),  worky,  (/ 1, itim /), (/ ndxndxi, 1 /))
       end if
       
       if ( jaFrcInternalTides2D.gt.0 .and. jamapIntTidesDiss.eq.1 ) then
          ierr = nf90_put_var(imapfile, id_inttidesdiss(iid), DissInternalTidesPerArea,  (/ 1, itim /), (/ ndxndxi, 1 /))
       end if
    endif

    if (jawind > 0 .and. jamapwind > 0 .and. jawindstressgiven == 0) then
       allocate (windx(ndxndxi), windy(ndxndxi), stat=ierr)
       if (ierr /= 0) call aerr( 'windx/windy', ierr, ndxndxi)
       !windx/y is not set to 0.0 for flownodes without links
       windx = 0.0d0
       windy = 0.0d0
       do n = 1,ndxndxi
          !
          ! Currently, wx/y is defined on the links
          ! TO DO: EC-module should not be asked for wind components on the links but on the cells
          !
          if (nd(n)%lnx > 0) then
             do i = 1,nd(n)%lnx
                windx(n) = windx(n) + wx(iabs(nd(n)%ln(i)))
                windy(n) = windy(n) + wy(iabs(nd(n)%ln(i)))
             end do
             windx(n) = windx(n) / nd(n)%lnx
             windy(n) = windy(n) / nd(n)%lnx
          else
             j=1
          endif
       end do
       ierr = nf90_put_var(imapfile, id_windx  (iid), windx,  (/ 1, itim /), (/ ndxndxi, 1 /))
       ierr = nf90_put_var(imapfile, id_windy  (iid), windy,  (/ 1, itim /), (/ ndxndxi, 1 /))
       deallocate (windx, stat=ierr)
       ierr = nf90_put_var(imapfile, id_windxu  (iid), wx,  (/ 1, itim /), (/ lnx, 1 /))
       ierr = nf90_put_var(imapfile, id_windyu  (iid), wy,  (/ 1, itim /), (/ lnx, 1 /))
    endif

    if (jamapwind > 0 .and. japatm > 0) then
       ierr = nf90_put_var(imapfile, id_patm(iid)  , Patm, (/ 1, itim /), (/ ndxndxi, 1 /))
    endif

    if (jamapheatflux > 0 .and. jatem > 1) then    ! Heat modelling only
       ierr = nf90_put_var(imapfile, id_tair(iid)  , Tair, (/ 1, itim /), (/ ndxndxi, 1 /))
       ierr = nf90_put_var(imapfile, id_rhum(iid)  , Rhum, (/ 1, itim /), (/ ndxndxi, 1 /))
       ierr = nf90_put_var(imapfile, id_clou(iid)  , Clou, (/ 1, itim /), (/ ndxndxi, 1 /))

        if (jatem == 5) then
           ierr = nf90_put_var(imapfile, id_qsun(iid)  , Qsunmap  , (/ 1, itim /), (/ ndxndxi, 1 /))
           ierr = nf90_put_var(imapfile, id_qeva(iid)  , Qevamap  , (/ 1, itim /), (/ ndxndxi, 1 /))
           ierr = nf90_put_var(imapfile, id_qcon(iid)  , Qconmap  , (/ 1, itim /), (/ ndxndxi, 1 /))
           ierr = nf90_put_var(imapfile, id_qlong(iid) , Qlongmap , (/ 1, itim /), (/ ndxndxi, 1 /))
           ierr = nf90_put_var(imapfile, id_qfreva(iid), Qfrevamap, (/ 1, itim /), (/ ndxndxi, 1 /))
           ierr = nf90_put_var(imapfile, id_qfrcon(iid), Qfrconmap, (/ 1, itim /), (/ ndxndxi, 1 /))
        endif
        ierr = nf90_put_var(imapfile, id_qtot(iid)  , Qtotmap  , (/ 1, itim /), (/ ndxndxi, 1 /))
    endif     
    call realloc(numlimdtdbl, ndxndxi, keepExisting=.false.)
    numlimdtdbl = dble(numlimdt) ! To prevent stack overflow. TODO: remove once integer version is available.
    ierr = nf90_put_var(imapfile, id_numlimdt(iid)  , numlimdtdbl, (/ 1, itim /), (/ ndxndxi, 1 /))
    deallocate(numlimdtdbl)

    ! Roughness from trachytopes
    if (jatrt == 1) then       
       ierr = nf90_put_var(imapfile, id_cftrt(iid),  cftrt(:,2),   (/ 1, itim /), (/ numl, 1 /))
    end if

    ! Roughness calibration factors
    if (jacali == 1) then       
       ierr = nf90_put_var(imapfile, id_cfcl(iid),  cfclval,   (/ 1, itim /), (/ numl, 1 /))
    end if
    
    ! JRE - XBeach
    if (jawave .eq. 4) then 
       ierr = nf90_put_var(imapfile, id_E(iid), E, (/ 1, itim /), (/ ndxndxi, 1 /)) ! direction integrated 
       ierr = nf90_put_var(imapfile, id_R(iid), R, (/ 1, itim /), (/ ndxndxi, 1 /)) 
       ierr = nf90_put_var(imapfile, id_H(iid), H, (/ 1, itim /), (/ ndxndxi, 1 /))
       ierr = nf90_put_var(imapfile, id_urmscc(iid), urms_cc, (/ 1, itim /), (/ ndxndxi, 1 /))
       ierr = nf90_put_var(imapfile, id_Fxcc(iid), Fx_cc, (/ 1, itim /), (/ ndxndxi, 1 /))
       ierr = nf90_put_var(imapfile, id_Fycc(iid), Fy_cc, (/ 1, itim /), (/ ndxndxi, 1 /))
       ierr = nf90_put_var(imapfile, id_D(iid), D, (/ 1, itim /), (/ ndxndxi, 1 /))
       ierr = nf90_put_var(imapfile, id_DR(iid), DR, (/ 1, itim /), (/ ndxndxi, 1 /))
       !if (jamombal>0) then
          ierr = nf90_put_var(imapfile, id_dsdx(iid), xbdsdx, (/ 1, itim /), (/ ndxndxi, 1 /))
          ierr = nf90_put_var(imapfile, id_dsdy(iid), xbdsdy, (/ 1, itim /), (/ ndxndxi, 1 /))
          ierr = nf90_put_var(imapfile, id_ducxdx(iid), xbducxdx, (/ 1, itim /), (/ ndxndxi, 1 /))
          ierr = nf90_put_var(imapfile, id_ducxdy(iid), xbducxdy, (/ 1, itim /), (/ ndxndxi, 1 /))
          ierr = nf90_put_var(imapfile, id_ducydx(iid), xbducydx, (/ 1, itim /), (/ ndxndxi, 1 /))
          ierr = nf90_put_var(imapfile, id_ducydy(iid), xbducydy, (/ 1, itim /), (/ ndxndxi, 1 /))
       !end if
       !ierr = nf90_put_var(imapfile, id_Fx(iid), Fx, (/ 1, itim /), (/ lnx, 1 /))
       !ierr = nf90_put_var(imapfile, id_Fy(iid), Fy, (/ 1, itim /), (/ lnx, 1 /))
!      Orient ust, vst in correct (E, N) direction...
       if (.not. allocated(ust_rot)) allocate(ust_rot(lnx), vst_rot(lnx))
       ust_rot = ust*csu - vst*snu
       vst_rot = ust*snu + vst*csu   
!      then write:
       ierr = nf90_put_var(imapfile, id_ust(iid), ust_rot, (/ 1, itim /), (/ lnx, 1 /))
       ierr = nf90_put_var(imapfile, id_vst(iid), vst_rot, (/ 1, itim /), (/ lnx, 1 /))
       deallocate(ust_rot, vst_rot)
       ierr = nf90_put_var(imapfile, id_urms(iid), urms, (/ 1, itim /), (/ lnx, 1 /))
       ierr = nf90_put_var(imapfile, id_sigmwav(iid), sigmwav, (/ 1, itim /), (/ ndxndxi, 1 /))
       ierr = nf90_put_var(imapfile, id_cwav(iid), cwav, (/ 1, itim /), (/ ndxndxi, 1 /))
       ierr = nf90_put_var(imapfile, id_cgwav(iid), cgwav, (/ 1, itim /), (/ ndxndxi, 1 /))
       ierr = nf90_put_var(imapfile, id_thetamean(iid), 270d0 - thetamean*180d0/pi, (/ 1, itim /), (/ ndxndxi, 1 /))
       if ( (windmodel .eq. 1) .and. (jawsource .eq. 1) ) then
          ierr = nf90_put_var(imapfile, id_SwE(iid), SwE, (/ 1, itim /), (/ ndxndxi, 1 /))       
          ierr = nf90_put_var(imapfile, id_SwT(iid), SwT, (/ 1, itim /), (/ ndxndxi, 1 /))  
       endif
    endif
   
    
!   deallocate
    if ( NUMCONST.gt.0 ) then
       if ( allocated(idum)     ) deallocate(idum)
    end if
  
end subroutine unc_write_map_filepointer


!> Writes the unstructured net to a netCDF file.
!! If file exists, it will be overwritten.
subroutine unc_write_net(filename, janetcell, janetbnd, jaidomain, jaiglobal_s, iconventions)
    character(len=*), intent(in) :: filename

    integer, optional, intent(in) :: janetcell  !< write additional network cell information (1) or not (0). Default: 0.
    integer, optional, intent(in) :: janetbnd   !< write additional network boundary information (1) or not (0). Default: 0.
    integer, optional, intent(in) :: jaidomain  !< write subdomain numbers (1) or not (0, default)
    integer, optional, intent(in) :: jaiglobal_s !< write global netcell number (1) or not (0, default)
    integer, optional, intent(in) :: iconventions
    type(t_unc_mapids)            :: mapids
    type(t_ug_meta)               :: meta 

    integer :: inetfile, ierr, janetcell_loc, janetbnd_loc, jaidomain_loc, jaiglobal_s_loc, iconv

    janetcell_loc = 0
    janetbnd_loc  = 0
    jaidomain_loc = 0
    jaiglobal_s_loc = 0
    
    if (.not. present(iconventions)) then
       iconv = UNC_CONV_CFOLD
    else
       iconv = iconventions
    end if
   
    if ( present(janetcell) ) then
      janetcell_loc = janetcell
    end if
    if ( present(janetbnd) ) then
      janetbnd_loc = janetbnd
    end if
    if ( present(jaidomain) ) then
      jaidomain_loc = jaidomain
    end if
    if ( present(jaiglobal_s) ) then
      jaiglobal_s_loc = jaiglobal_s
    end if
    
    ierr = unc_create(filename, 0, inetfile)
    if (ierr /= nf90_noerr) then
        call mess(LEVEL_ERROR, 'Could not create net file '''//trim(filename)//'''.')
        call check_error(ierr)
        return
    end if

    if (iconv == UNC_CONV_UGRID) then
       mapids%ncid = inetfile
       ierr = ug_addglobalatts(mapids%ncid, meta) !Add UGRID convention
       call unc_write_net_ugrid2(mapids%ncid, mapids%id_tsp, janetcell=janetcell_loc)   
    else
       call unc_write_net_filepointer(inetfile, janetcell=janetcell_loc, janetbnd=janetbnd_loc, jaidomain=jaidomain_loc, jaiglobal_s=jaiglobal_s_loc)
    end if

    ierr = unc_close(inetfile)
end subroutine unc_write_net


!> Writes the unstructured net in UGRID format to an already opened netCDF dataset.
subroutine unc_write_net_filepointer(inetfile, janetcell, janetbnd, jaidomain, jaiglobal_s)
    use network_data
    use m_flowgeom, only: xz, yz
    use m_alloc
    use m_polygon
    use m_sferic
    use m_missing
    use m_partitioninfo
    use geometry_module, only: get_startend, normaloutchk
    use gridoperations
    use m_flowparameters, only: jawave
    
    integer, intent(in) :: inetfile

    integer, optional, intent(in) :: janetcell  !< write additional network cell information (1) or not (0). Default: 1.
    integer, optional, intent(in) :: janetbnd   !< write additional network boundary information (1) or not (0). Default: 1.
    integer, optional, intent(in) :: jaidomain   !< write subdomain numbers (1) or not (0, default)
    integer, optional, intent(in) :: jaiglobal_s !< write global netcell numbers (1) or not (0, default)
    integer                       :: janetcell_
    integer                       :: janetbnd_
    integer                       :: jaidomain_
    integer                       :: jaiglobal_s_
    integer, allocatable :: kn3(:), ibndlink(:)

    integer :: ierr
    integer :: id_netnodedim, id_netlinkdim, id_netlinkptsdim, &   !< Dimensions
               id_bndlinkdim, &
               id_encinstdim, id_encptsdim, id_encpartdim, &
               id_netelemdim, id_netelemmaxnodedim, &
               id_netelemlinkdim, id_netelemlinkptsdim, id_netlinkcontourptsdim, &
               id_netnodex, id_netnodey, id_netnodez, &            !< Node variables
               id_netnodelon, id_netnodelat, &                     !< Mandatory lon/lat coords for net nodes
               id_netlink, id_netlinktype, &                       !< Link variables
               id_netlinkxu, id_netlinkyu, &
               id_netlinkcontourx, id_netlinkcontoury, &
               id_bndlink, id_bndlinktype, &                       !< Boundary variables
               id_enc_container, id_encx, id_ency, &               !< Grid enclosure variables
               id_enc_nodecount, id_enc_partnodecount, &
               id_enc_interiorring, &
               id_netelemnode, id_netelemlink, &
               id_idomain, id_iglobal_s                            !< Netelem variables   
    integer, allocatable, dimension(:,:) :: netcellnod, netcelllin
    integer :: id_mesh2d
    integer :: jaInDefine
    integer :: k, L, nv, numbnd, maxbnd, numencparts, numencpts, ja, k1, k2, kt, n1, nv1
    double precision :: xzn,yzn,x3, y3, x4, y4,DIS,XP,YP,rl, xn, yn, t0, t1
    double precision, allocatable :: xtt(:,:), ytt(:,:), xut(:), yut(:), polc(:)
    integer, dimension(:), allocatable :: kn1write
    integer, dimension(:), allocatable :: kn2write
    integer :: istart, iend, ipoint, ipoly, numpoints, iorient, iinterior
    integer :: netstat_store
    
    call readyy('Writing net data',0d0)

    ! Defaults for extended information:
    janetcell_ = 1
    janetbnd_  = 1
    jaidomain_ = 0
    jaiglobal_s_ = 0
  

    if ( present(janetcell) ) then
      janetcell_ = janetcell
    end if
    if ( present(janetbnd) ) then
      janetbnd_ = janetbnd
    end if
    if ( present(jaidomain)) then
     jaidomain_ = jaidomain
    end if
    if (present(jaiglobal_s)) then
     jaiglobal_s_ = jaiglobal_s
    end if
    
   ! hk: this should not be done here anymore 
   ! if (janetcell_ /= 0) then 
   !    if (size(lnn) < numl .or. netstat == NETSTAT_CELLS_DIRTY ) then
   !       call setnodadm(0)
   !       call findcells(0)
   !    end if
   ! endif

    if (janetbnd_ /= 0) then
       numbnd = 0
       maxbnd = ceiling(sqrt(real(numl))) ! First estimate of numbnd
       allocate(ibndlink(maxbnd))
       do L=1,numl
           if (lnn(L) < 2 .and. kn(3, L) == 2) then
               numbnd = numbnd+1
               if (numbnd > maxbnd) then
                   maxbnd = MAX(NUMBND, NINT(1.2*maxbnd))
                   call realloc(ibndlink, maxbnd)
               end if
               ibndlink(numbnd) = L
           end if
       enddo

       ! Start detecting grid enclosure
       call savepol()
       if (jampi>0) then ! .and. jawave == 3) then
          netstat_store = netstat
          netstat = NETSTAT_OK
          call generate_partition_pol_from_idomain(ierr, myrank=my_rank)    ! UNST-1937: strictly domain, no ghostcells
          netstat = netstat_store
       else
          call copynetboundstopol(0, 0, 1, 0)
       endif
       call realloc(iistart, maxpoly, keepExisting=.false.)
       call realloc(iiend, maxpoly, keepExisting=.false.)
       ipoint = 1
       ipoly = 0
       numpoints = 0
       do while ( ipoint <= NPL)
          ipoly = ipoly+1
          if (ipoly > maxpoly) then
             maxpoly = ceiling(maxpoly*1.1)
             call realloc(iistart, maxpoly, keepExisting=.true.)
             call realloc(iiend, maxpoly, keepExisting=.true.)
          end if

         !        get polygon start and end pointer respectively
         call get_startend(NPL-ipoint+1,xpl(ipoint:NPL),ypl(ipoint:NPL), istart, iend, dmiss)
         istart = istart+ipoint-1
         iend   = iend  +ipoint-1

         if ( istart.ge.iend .or. iend.gt.NPL ) exit ! done
         
         iistart(ipoly) = istart
         iiend(ipoly)   = iend
         numpoints = numpoints + (iend-istart+1)

!           advance pointer
         ipoint = iend+2
       end do
       npoly = ipoly
    end if

    ! Put dataset in define mode (possibly again) to add dimensions and variables.
    jaInDefine = 0
    ierr = nf90_redef(inetfile)
    if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
    if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
        call mess(LEVEL_ERROR, 'Could not put header in net file.')
        call check_error(ierr)
        return
    end if

    if ( janetcell_ /= 0 ) then
       ! Determine max nr. of vertices in NetElems (netcells)
       nv = 0
       do k=1,nump1d2d
           nv = max(nv, netcell(k)%n)
       end do
    end if
        
    ! Dimensions
    ierr = nf90_def_dim(inetfile, 'nNetNode',        numk,   id_netnodedim)
    ierr = nf90_def_dim(inetfile, 'nNetLink',        numl,   id_netlinkdim)
    ierr = nf90_def_dim(inetfile, 'nNetLinkPts',     2,      id_netlinkptsdim)
    ierr = nf90_def_dim(inetfile, 'nNetElem',        nump1d2d,   id_netelemdim)
    if ( janetbnd_ /= 0 .and. numbnd > 0) then
       ierr = nf90_def_dim(inetfile, 'nBndLink',        numbnd, id_bndlinkdim)

       numencpts = numpoints
       numencparts = npoly
       ! Define the enclosure variables for each mesh
       ierr = nf90_def_dim(inetfile, 'nmesh2d_EnclosureInstance', 1,           id_encinstdim) ! Just one enclosure
       ierr = nf90_def_dim(inetfile, 'nmesh2d_EnclosurePoints',   numencpts,   id_encptsdim)
       ierr = nf90_def_dim(inetfile, 'nmesh2d_EnclosureParts',    numencparts, id_encpartdim)
   
       ierr = nf90_def_var(inetfile, 'mesh2d_enc_x', nf90_double, id_encptsdim, id_encx)
       ierr = nf90_def_var(inetfile, 'mesh2d_enc_y', nf90_double, id_encptsdim, id_ency)
       ierr = unc_addcoordatts(inetfile, id_encx, id_ency, jsferic)
       ierr = nf90_put_att(inetfile, id_encx,     'cf_role',    'geometry_x_node')
       ierr = nf90_put_att(inetfile, id_ency,     'cf_role',    'geometry_y_node')
   
       ierr = nf90_def_var(inetfile, 'mesh2d_enc_node_count', nf90_int, id_encinstdim, id_enc_nodecount)
       ierr = nf90_put_att(inetfile, id_enc_nodecount,     'long_name',    'count of coordinates in each instance geometry')
   
       ierr = nf90_def_var(inetfile, 'mesh2d_enc_part_node_count', nf90_int, id_encpartdim, id_enc_partnodecount)
       ierr = nf90_put_att(inetfile, id_enc_partnodecount, 'long_name',    'count of nodes in each geometry part')
   
       ierr = nf90_def_var(inetfile, 'mesh2d_enc_interior_ring', nf90_int,    id_encpartdim, id_enc_interiorring)
       ierr = nf90_put_att(inetfile, id_enc_interiorring,  'long_name',    'type of each geometry part')
   
       ierr = nf90_def_var(inetfile, 'mesh2d_enclosure_container', nf90_float, id_enc_container)
       ierr = nf90_put_att(inetfile, id_enc_container,     'geometry_type',    'multipolygon')
       ierr = nf90_put_att(inetfile, id_enc_container,     'node_count',       'mesh2d_enc_node_count')
       ierr = nf90_put_att(inetfile, id_enc_container,     'node_coordinates', 'mesh2d_enc_x mesh2d_enc_y')
       !ierr = nf90_put_att(inetfile, id_enc_container,     'crs',              'crs')
       ierr = nf90_put_att(inetfile, id_enc_container,     'part_node_count',  'mesh2d_enc_part_node_count')
       ierr = nf90_put_att(inetfile, id_enc_container,     'interior_ring',    'mesh2d_enc_interior_ring')
    end if

    if (janetcell_ /= 0 .and. nump1d2d > 0) then
       ierr = nf90_def_dim(inetfile, 'nNetElemMaxNode', nv,     id_netelemmaxnodedim)
       ierr = nf90_def_dim(inetfile, 'nNetLinkContourPts', 4,   id_netlinkcontourptsdim) ! Momentum control volume a la Perot: rectangle around xu/yu
    end if

!    ierr = nf90_def_dim(inetfile, 'nNetElemLink',    numl,   id_netelemlinkdim)
!    ierr = nf90_def_dim(inetfile, 'nNetElemLinkPts', 2,      id_netelemlinkptsdim)

    ierr = nf90_def_var(inetfile, 'Mesh2D', nf90_int, id_mesh2d)
    ierr = nf90_put_att(inetfile, id_mesh2d, 'cf_role', 'mesh_topology')
    ierr = nf90_put_att(inetfile, id_mesh2d, 'node_coordinates', 'NetNode_x NetNode_y')
    ierr = nf90_put_att(inetfile, id_mesh2d, 'node_dimension', 'nNetNode')
    ierr = nf90_put_att(inetfile, id_mesh2d, 'edge_node_connectivity', 'NetLink')
    ierr = nf90_put_att(inetfile, id_mesh2d, 'edge_dimension', 'nNetLink')
    ierr = nf90_put_att(inetfile, NF90_GLOBAL, 'Conventions', 'UGRID-0.9')

    ! Coordinates
    ierr = nf90_def_var(inetfile, 'NetNode_x', nf90_double, id_netnodedim, id_netnodex)
    ierr = nf90_def_var(inetfile, 'NetNode_y', nf90_double, id_netnodedim, id_netnodey)
    ierr = unc_addcoordatts(inetfile, id_netnodex, id_netnodey, jsferic)

    ierr = unc_addcoordmapping(inetfile, jsferic)

    !! Add mandatory lon/lat coords too (only if jsferic==0)
    !ierr = unc_add_lonlat_vars(inetfile, 'NetNode', '', (/ id_netnodedim /), id_netnodelon, id_netnodelat, jsferic)

    ierr = nf90_def_var(inetfile, 'NetNode_z', nf90_double, id_netnodedim, id_netnodez)
    ierr = nf90_put_att(inetfile, id_netnodez, 'units',         'm')
    ierr = nf90_put_att(inetfile, id_netnodez, 'positive',      'up')
    ierr = nf90_put_att(inetfile, id_netnodez, 'standard_name', 'sea_floor_depth')
    ierr = nf90_put_att(inetfile, id_netnodez, 'long_name',     'bed level at net nodes (flow element corners)') !!  at flow element''s corner  / net node
    ierr = nf90_put_att(inetfile, id_netnodez, 'coordinates',   'NetNode_x NetNode_y')
    ierr = nf90_put_att(inetfile, id_netnodez, 'mesh',          'Mesh2D')
    ierr = nf90_put_att(inetfile, id_netnodez, 'location',      'node')

!    ierr = unc_add_gridmapping_att(inetfile, (/ id_netnodex, id_netnodey, id_netnodez /), jsferic)

    ! Netlinks
    ierr = nf90_def_var(inetfile, 'NetLink', nf90_int, (/ id_netlinkptsdim, id_netlinkdim /) , id_netlink)
    ierr = nf90_put_att(inetfile, id_netlink, 'standard_name', 'netlink')
    ierr = nf90_put_att(inetfile, id_netlink, 'long_name',     'link between two netnodes')
    ierr = nf90_put_att(inetfile, id_netlink, 'start_index', 1)

    ierr = nf90_def_var(inetfile, 'NetLinkType', nf90_int, id_netlinkdim, id_netlinktype)
    ierr = nf90_put_att(inetfile, id_netlinktype, 'long_name',     'type of netlink')
    ierr = nf90_put_att(inetfile, id_netlinktype, 'valid_range',   (/ 0, 4 /))
    ierr = nf90_put_att(inetfile, id_netlinktype, 'flag_values',   (/ 0, 1, 2, 3, 4 /))
    ierr = nf90_put_att(inetfile, id_netlinktype, 'flag_meanings', 'closed_link_between_2D_nodes link_between_1D_nodes link_between_2D_nodes embedded_1D2D_link 1D2D_link')

    if (janetcell_ /= 0 .and. nump1d2d > 0) then
       ierr = nf90_put_att(inetfile, id_mesh2d, 'topology_dimension', 2)
       ierr = nf90_put_att(inetfile, id_mesh2d, 'face_node_connectivity', 'NetElemNode')
       ierr = nf90_put_att(inetfile, id_mesh2d, 'face_dimension', 'nNetElem')
       !
       ! Netcells
       ! Netcell-to-netnode mapping    
       ierr = nf90_def_var(inetfile, 'NetElemNode', nf90_int, (/ id_netelemmaxnodedim, id_netelemdim /) , id_netelemnode)
       ierr = nf90_put_att(inetfile, id_netelemnode, 'long_name', 'mapping from net cell to net nodes (counterclockwise)')
       ierr = nf90_put_att(inetfile, id_netelemnode, 'start_index', 1)
       ierr = nf90_put_att(inetfile, id_netelemnode, '_FillValue', intmiss)

       ierr = nf90_def_var(inetfile, 'NetElemLink', nf90_int, (/ id_netelemmaxnodedim, id_netelemdim /) , id_netelemlink)! netcell()%Lin
       ierr = nf90_put_att(inetfile, id_netelemlink, 'long_name', 'mapping from net cell to its net links (counterclockwise)')
       ierr = nf90_put_att(inetfile, id_netelemlink, 'short_name', 'netcell()%LIN')
       
       !ierr = nf90_def_var(inetfile, 'ElemCenter_x', nf90_double, (/ id_netelemdim /), id_elemcenx)
       !ierr = nf90_put_att(inetfile, id_elemcenx, 'long_name', 'x coordinate of cell center')
       !ierr = nf90_put_att(inetfile, id_elemcenx, 'short_name', 'xz')
       !
       !ierr = nf90_def_var(inetfile, 'ElemCenter_y', nf90_double, (/ id_netelemdim /), id_elemceny)
       !ierr = nf90_put_att(inetfile, id_elemceny, 'long_name', 'y coordinate of cell center')
       !ierr = nf90_put_att(inetfile, id_elemceny, 'short_name', 'yz')
    
       ierr = nf90_def_var(inetfile, 'NetLinkContour_x',     nf90_double, (/ id_netlinkcontourptsdim, id_netlinkdim /) ,   id_netlinkcontourx)
       ierr = nf90_def_var(inetfile, 'NetLinkContour_y',     nf90_double, (/ id_netlinkcontourptsdim, id_netlinkdim /) ,   id_netlinkcontoury)
       ierr = unc_addcoordatts(inetfile, id_netlinkcontourx, id_netlinkcontoury, jsferic)
       ierr = nf90_put_att(inetfile, id_netlinkcontourx, 'long_name'    , 'list of x-contour points of momentum control volume surrounding each net/flow link')
       ierr = nf90_put_att(inetfile, id_netlinkcontoury, 'long_name'    , 'list of y-contour points of momentum control volume surrounding each net/flow link')
       ierr = nf90_put_att(inetfile, id_netlinkcontourx, '_FillValue', dmiss)
       ierr = nf90_put_att(inetfile, id_netlinkcontoury, '_FillValue', dmiss)
       
       ierr = nf90_def_var(inetfile, 'NetLink_xu',     nf90_double, (/ id_netlinkdim /) ,   id_netlinkxu)
       ierr = nf90_def_var(inetfile, 'NetLink_yu',     nf90_double, (/ id_netlinkdim /) ,   id_netlinkyu)
       ierr = unc_addcoordatts(inetfile, id_netlinkxu, id_netlinkyu, jsferic)
       ierr = nf90_put_att(inetfile, id_netlinkxu, 'long_name'    , 'x-coordinate of net link center (velocity point)')
       ierr = nf90_put_att(inetfile, id_netlinkyu, 'long_name'    , 'y-coordinate of net link center (velocity point)')

       ! Add grid_mapping reference to all original coordinate and data variables
!       ierr = unc_add_gridmapping_att(inetfile, &
!          (/ id_netlinkxu, id_netlinkyu, id_netlinkcontourx, id_netlinkcontoury /), jsferic)
    else
       ierr = nf90_put_att(inetfile, id_mesh2d, 'topology_dimension', 1)
    end if

    if ( janetbnd_ /= 0 .and. numbnd > 0) then
       ! List of boundary netlinks
       ierr = nf90_def_var(inetfile, 'BndLink', nf90_int, id_bndlinkdim, id_bndlink)
       ierr = nf90_put_att(inetfile, id_bndlink, 'long_name',     'netlinks that compose the net boundary')
    end if

   if (jaidomain_ /= 0) then
       ierr = nf90_def_var(inetfile, 'idomain', nf90_int, (/id_netelemdim /), id_idomain)
       ierr = nf90_put_att(inetfile, id_idomain, 'long_name', 'partition subdomain numbers')
       ierr = nf90_put_att(inetfile, id_idomain, 'short_name', 'idomain')
       ierr = nf90_put_att(inetfile, id_idomain, 'valid_max', ndomains)  ! the total number of subdomains
       ierr = nf90_put_att(inetfile, id_idomain, 'mesh', 'Mesh2D')
       ierr = nf90_put_att(inetfile, id_idomain, 'location', 'face')
   end if
   
   if (jaiglobal_s_ /= 0) then
       ierr = nf90_def_var(inetfile, 'iglobal_s', nf90_int, (/id_netelemdim /), id_iglobal_s)
       ierr = nf90_put_att(inetfile, id_iglobal_s, 'long_name', 'global netcell numbers')
       ierr = nf90_put_att(inetfile, id_iglobal_s, 'short_name', 'iglobal_s')
       ierr = nf90_put_att(inetfile, id_iglobal_s, 'valid_max', Nglobal_s)
       ierr = nf90_put_att(inetfile, id_iglobal_s, 'mesh', 'Mesh2D')
       ierr = nf90_put_att(inetfile, id_iglobal_s, 'location', 'face')
    endif
    
    ierr = nf90_enddef(inetfile)
    call readyy('Writing net data',.05d0)

    ! Write the actual data
    ierr = nf90_put_var(inetfile, id_netnodex,    xk(1:numk))
    call readyy('Writing net data',.25d0)
    ierr = nf90_put_var(inetfile, id_netnodey,    yk(1:numk))
    call readyy('Writing net data',.45d0)
    ierr = nf90_put_var(inetfile, id_netnodez,    zk(1:numk))
    call readyy('Writing net data',.65d0)
    
 !   ierr = nf90_put_var(inetfile, id_netlink,     kn, count=(/ 2, numl /), map=(/ 1, 3 /))
    allocate(kn1write(numL))
    allocate(kn2write(numL))
    do L=1,numL
       kn1write(L)=kn(1,L)
       kn2write(L)=kn(2,L)
    end do
    ierr = nf90_put_var(inetfile, id_netlink,     kn1write, count=(/ 1, numl /), start=(/1,1/))
    ierr = nf90_put_var(inetfile, id_netlink,     kn2write, count=(/ 1, numl /), start=(/2,1/))
    deallocate(kn1write)
    deallocate(kn2write)
    call readyy('Writing net data',.85d0)
    
    ! AvD: TODO: if jsferic==0, then use proj.4 to convert x/y and write lon/lat for netnodes too.


    ! An array slice cannot be passed to netcdf C-library (risk of stack overflow), so use copy.
    allocate(kn3(numl))
    do L = 1,numl
       if (kn(3,L) >= 1 .and. kn(3,L) <= 7) then
          kn3(L) = kn(3,L) ! TODO: UNST-715: in rare cases, this will incorrectly change 1D net links into 2D net links.
       else
          kn3(L) = 2       ! e.g. thind dams, so thin dams become 2D again   
       end if
    end do

    ierr = nf90_put_var(inetfile, id_netlinktype, kn3)
    deallocate(kn3)
    call readyy('Writing net data',1d0)

    if ( janetbnd_ /= 0 .and. numbnd > 0) then
       ! Write boundary links
       ierr = nf90_put_var(inetfile, id_bndlink, ibndlink, count = (/ numbnd /))

       ! Write grid enclosure
       ipoint = 1
       do ipoly=1,npoly
          ! Points
          istart = iistart(ipoly)
          iend   = iiend(ipoly)
          nv     = iend - istart + 1
          allocate(polc(nv))          ! temp variable necessary here to get actual values in file
          polc = xpl(istart:iend)
          ierr = nf90_put_var(inetfile, id_encx, polc, start = (/ ipoint /), count = (/ nv /))
          polc = ypl(istart:iend)
          ierr = nf90_put_var(inetfile, id_ency, polc, start = (/ ipoint /), count = (/nv /))
          deallocate(polc)
          ipoint = ipoint + nv

          ! Current part
          ierr = nf90_put_var(inetfile, id_enc_partnodecount, (/ nv /), start = (/ ipoly /), count = (/ 1 /))

          ! Re-determine the orientation of this polygon again (currently no nice way to get it back from copynetboundstopol())
          call polorientation(xpl(istart:iend), ypl(istart:iend), iend-istart+1, iend-istart+1, iorient)
          ! NCSG requires 1/0 var to denote interior/outer ring respectively. Additionally, we always write outer as CCW, and inner as CW.
          if (iorient == 1) then
             iinterior = 0
          else
             iinterior = 1
          end if
          ierr = nf90_put_var(inetfile, id_enc_interiorring, (/ iinterior /), start = (/ ipoly /), count = (/ 1 /))
       end do
       ierr = nf90_put_var(inetfile, id_enc_nodecount, numencpts)
       ierr = nf90_sync(inetfile)
       call restorepol()

    end if
    !
    if ( jaidomain_ /= 0) then
       ierr = nf90_put_var(inetfile, id_idomain,   idomain,   count = (/ nump1d2d /)) !!!!!!!!!!!!!!
    endif
    
    if ( jaiglobal_s_ /= 0) then
       ierr = nf90_put_var(inetfile, id_iglobal_s, iglobal_s, count = (/ nump1d2d /))
    end if
    
    if ( janetcell_ /= 0 .and. nump1d2d > 0) then
       ! Write net cells 
        ierr = nf90_inquire_dimension(inetfile, id_netelemmaxnodedim, len = nv)
       allocate(netcellnod(nv, nump1d2d))
       allocate(netcelllin(nv, nump1d2d))
       netcellnod = intmiss
       netcelllin = intmiss
       do k = 1, nump1d2d
          nv1 = netcell(k)%n
          netcellnod(1:nv1,k) = netcell(k)%nod
          netcelllin(1:nv1,k) = netcell(k)%lin
       enddo
       ierr = nf90_put_var(inetfile, id_netelemnode, netcellnod)
       call check_error(ierr, 'Write netcell elem.')
       ierr = nf90_put_var(inetfile, id_netelemlink, netcelllin)
       call check_error(ierr, 'Write netcell links')
       deallocate(netcellnod)
       deallocate(netcelllin)
  
     call readyy('Writing net data',.65d0)
!       call klok(t0)
       allocate(xtt(4, numl), ytt(4, numl), xut(numl), yut(numl))
       xtt = dmiss
       ytt = dmiss
       do L=1,numl1d
          xut(L) = .5d0*(xk(kn(1,L)) + xk(kn(2,L)))
          yut(L) = .5d0*(yk(kn(1,L)) + yk(kn(2,L)))
       end do

       do L=numl1d+1,numl
          xut(L) = .5d0*(xk(kn(1,L)) + xk(kn(2,L)))
          yut(L) = .5d0*(yk(kn(1,L)) + yk(kn(2,L)))
          xtt(:,L) = 0d0
          ytt(:,L) = 0d0
          if (LNN(L) >= 1) then
             K1 = kn(1,L)
             k2 = kn(2,L)

             ! 'left' half
             n1 = LNE(1,L)
             if (n1 < 0) n1 = -n1
             x3 = xk(k1)
             y3 = yk(k1)
             x4 = xk(k2)
             y4 = yk(k2)
             xzn = xz(n1)
             yzn = yz(n1)

             ! Normal distance from circumcenter to net link:
             call dLINEDIS2(xzn,yzn,x3, y3, x4, y4,JA,DIS,XP,YP,rl)

             ! Note: we're only in net-mode, not yet in flow-mode, so we can NOT assume that net node 3->4 have a 'rightward' flow node orientation 1->2
             ! Instead, compute outward normal, and swap points 3 and 4 if necessary, such that we locally achieve the familiar k3->k4 + n1->n2 orientation.
             ! Outward normal vector of net link (cell 1 considered 'inward'):
             call normaloutchk(x3, y3, x4, y4, xzw(n1), yzw(n1), xn, yn, ja, jsferic, jasfer3D, dmiss, dxymis)
             if (ja == 1) then ! normal was flipped, so swap net point 3 and 4 locally, to construct counterclockwise cell hereafter.
                kt = k1
                k1 = k2
                k2 = kt
                xp = x3
                yp = y3
                x3 = x4
                y3 = y4
                x4 = xp
                y4 = yp
             end if

             xtt(1,L) = x4 - DIS*xn
             ytt(1,L) = y4 - DIS*yn
             xtt(2,L) = x3 - DIS*xn
             ytt(2,L) = y3 - DIS*yn
             
             if (LNN(L) == 2) then
                ! second half
                n1 = LNE(2,L)
                if (n1 < 0) n1 = -n1
                xzn = xz(n1)
                yzn = yz(n1)
                call dLINEDIS2(xzn,yzn,x3, y3, x4, y4,JA,DIS,XP,YP,rl)
                xtt(3, L) = x3 + DIS*xn
                ytt(3, L) = y3 + DIS*yn
                xtt(4, L) = x4 + DIS*xn
                ytt(4, L) = y4 + DIS*yn
             else
                ! closed net boundary, no second half cell, just close off with netlink
                xtt(3, L) = x3
                ytt(3, L) = y3
                xtt(4, L) = x4
                ytt(4, L) = y4
             end if
          else
             ! No surrounding cells: leave missing fill values in file.
             xtt(:,L) = dmiss
             ytt(:,L) = dmiss
          end if
       enddo
       ierr = nf90_put_var(inetfile, id_netlinkcontourx, xtt, (/ 1, 1 /), (/ 4, numl /) )
       ierr = nf90_put_var(inetfile, id_netlinkcontoury, ytt, (/ 1, 1 /), (/ 4, numl /) )
       ierr = nf90_put_var(inetfile, id_netlinkxu, xut)
       ierr = nf90_put_var(inetfile, id_netlinkyu, yut)
!      call klok(t1)
!      write(msgbuf,"('writing netlinkcontours at once, elapsed time: ', G15.5, 's.')") t1-t0
!      call msg_flush()
    end if

    ! Leave the dataset in the same mode as we got it.
    if (jaInDefine == 1) then
        ierr = nf90_redef(inetfile)
    end if

    if (allocated(ibndlink)) deallocate(ibndlink)
    if (allocated(kn3))      deallocate(kn3)
    if (allocated(xtt))      deallocate(xtt)
    if (allocated(ytt))      deallocate(ytt)
    if (allocated(xut))      deallocate(xut)
    if (allocated(yut))      deallocate(yut)

    call readyy('Writing net data',-1d0)
end subroutine unc_write_net_filepointer


! NOTE: AvD: this routine below is a temporary working function that should replace unc_write_ugrid soon.
! It should contain two things:
! * io_ugrid-based writing of all basic net data (nodes/edges/faces)
! * AND NetLinkContour-related variables (see the original unc_write_net_filepointer routine)
!> Writes the unstructured network in UGRID format to an already opened netCDF dataset.
subroutine unc_write_net_ugrid2(ncid,id_tsp, janetcell)
   use m_flowgeom, only: xz, yz
   use network_data, xe_no=>xe, ye_no=>ye
   use m_sferic
   use m_missing
   use netcdf
   use m_alloc
   use dfm_error
   use geometry_module
   use m_save_ugrid_state
   use gridoperations

   implicit none

   integer                          :: ncid
   type(t_unc_timespace_id), intent(inout):: id_tsp
   integer, optional, intent(in)    :: janetcell
   integer                          :: janetcell_
   
   
   integer :: nn
   integer, allocatable :: edge_nodes(:,:), face_nodes(:,:), edge_type(:), contacts(:,:) 
   integer :: layer_count, layer_type
   real(kind=dp), dimension(:), pointer :: layer_zs=>null(), interface_zs =>null()
!   type(t_crs) :: pj

   integer :: ierr
   integer :: i, k, k1, k2, numContPts, n, numl2d, numk1d, numk2d, L, Lnew, nv, n1, n2, ja, kt
   double precision :: xzn,yzn,x3, y3, x4, y4,DIS,XP,YP,rl,xperp,yperp
   double precision, allocatable :: xtt(:,:), ytt(:,:)
   integer :: id_flowelemcontourptsdim, id_flowelemcontourx, id_flowelemcontoury, &
              id_netelemmaxnodedim, id_netlinkcontourptsdim, &
              id_netlinkcontourx, id_netlinkcontoury
   integer :: jaInDefine

   double precision :: xx, yy
   double precision, dimension(:), allocatable :: zz
   double precision, allocatable :: xn(:), yn(:), zn(:), xe(:), ye(:)
   double precision, allocatable :: work2(:,:)

   integer                       :: n1dedges, n1d2dcontacts, start_index
   integer,allocatable           :: contacttype(:) 

   jaInDefine = 0
   n1d2dcontacts = 0
   start_index   = 1

   if (present(janetcell)) then
      janetcell_   = janetcell
   else
      janetcell_   = 0
   end if
   
   !We need the cells for the face_nodes
   if (janetcell_ /= 0) then 
   !    if (size(lnn) < numl .or. netstat == NETSTAT_CELLS_DIRTY ) then
          call setnodadm(0)
          call findcells(0)
          call find1dcells()
   !    end if
   endif

   if (NUMK <= 0) then
      call mess(LEVEL_WARN, 'No grid points in model, will not write net geometry.')
      return
   end if

   ! Put dataset in define mode (possibly again) to add dimensions and variables.
   ierr = nf90_redef(ncid)
   if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
   if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
      call mess(LEVEL_ERROR, 'Could not put header in net geometry file.')
      call check_error(ierr)
      return
   end if

    if (jsferic == 1) then
       crs%epsg_code = 4326
    end if

   ! 1D network geometry
   if (numl1d > 0) then

      ! count 1d mesh nodes, edges and 1d2d contacts 
      n1dedges = 0 
      n1d2dcontacts = 0
      KC(:) = 0
      NUMK1D = 0
      do L=1,numl1d
         if (janetcell_ == 0 .or. (kn(3,L) == 1 .or. kn(3,L) == 6)) then
            ! Regular 1D net link, or: when no cells, all 1D2D-type net links will also be included with both start and end node.
            n1dedges = n1dedges + 1

            K1 = KN(1,L)
            K2 = KN(2,L)
            if (KC(K1) == 0) then
               NUMK1D = NUMK1D+1
               KC(K1) = 1
            end if
            if (KC(K2) == 0) then
               NUMK1D = NUMK1D+1
               KC(K2) = 1
            end if
         else
            ! 1D2D-type net links, with cell info available.
            n1d2dcontacts = n1d2dcontacts + 1

            N1 = abs(lne(1,L))
            N2 = abs(lne(2,L))
            if (N1 > nump) then  ! First point of 1D link is 1D cell
               K1 = netcell(N1)%nod(1)
               if (KC(K1) == 0) then
                  NUMK1D = NUMK1D+1
                  KC(K1) = 1
               end if
            end if
            if (N2 > nump) then  ! Second point of 1D link is 1D cell
               K2 = netcell(N2)%nod(1)
               if (KC(K2) == 0) then
                  NUMK1D = NUMK1D+1
                  KC(K2) = 1
               end if
            end if
         end if
      enddo

      ! Allocate  nodes
      call realloc(xn, NUMK1D)
      call realloc(yn, NUMK1D)
      call realloc(zn, NUMK1D)

      ! Allocate edges
      call realloc(edge_nodes, (/ 2, n1dedges /), fill = -999)
      call realloc(edge_type, n1dedges, fill = -999, keepExisting = .false.)
      call realloc(xe, n1dedges, fill = dmiss, keepExisting = .false.)
      call realloc(ye, n1dedges, fill = dmiss, keepExisting = .false.)

      ! Allocate contacts
      call realloc(contacts, (/ 2, n1d2dcontacts /), fill = -999) 
      call realloc(contacttype, n1d2dcontacts, keepExisting = .false., fill = 0)

      ! Assign values to 1D mesh nodes and edges, and 1d2d contacts 
      n1dedges = 0 
      n1d2dcontacts = 0
      NUMK1D = 0
      KC(:) = 0
      do L=1,NUML1D
         if (janetcell_ == 0 .or. (kn(3,L) == 1 .or. kn(3,L) == 6)) then
            n1dedges = n1dedges + 1

            K1 = KN(1,L)
            K2 = KN(2,L)
            if (KC(K1) == 0) then
               NUMK1D = NUMK1D+1
               xn(NUMK1D) = xk(K1)
               yn(NUMK1D) = yk(K1)
               zn(NUMK1D) = zk(K1)
               KC(K1) = -NUMK1D ! Remember new node number
            end if
            if (KC(K2) == 0) then
               NUMK1D = NUMK1D+1
               xn(NUMK1D) = xk(K2)
               yn(NUMK1D) = yk(K2)
               zn(NUMK1D) = zk(K2)
               KC(K2) = -NUMK1D ! Remember new node number
            end if

            edge_nodes(1,n1dedges) = abs(KC(KN(1,L)))
            edge_nodes(2,n1dedges) = abs(KC(KN(2,L)))
            edge_type(n1dedges)    = KN(3,L)

            xe(n1dedges) = .5d0*(xk(K1) + xk(K2)) ! TODO: AvD: make this sferic+3D-safe
            ye(n1dedges) = .5d0*(yk(K1) + yk(K2)) ! TODO: AvD: make this sferic+3D-safe

         else if (kn(3,L) == 3 .or. kn(3,L) == 4 .or. kn(3,L) == 5 .or. kn(3,L) == 7) then  ! 1d2d, lateralLinks, streetinlet, roofgutterpipe
            ! 1D2D-type net links, with cell info available.
            n1d2dcontacts = n1d2dcontacts + 1

            N1 = abs(lne(1,L))
            N2 = abs(lne(2,L))

            if (N1 > nump) then  ! First point of 1D link is 1D cell
               K1 = netcell(N1)%nod(1)
               if (KC(K1) == 0) then
                  NUMK1D = NUMK1D+1
                  xn(NUMK1D) = xk(K1)
                  yn(NUMK1D) = yk(K1)
                  zn(NUMK1D) = zk(K1)
                  KC(K1) = -NUMK1D ! Remember new node number
               end if
               
               contacts(1,n1d2dcontacts) = abs(KC(netcell(N1)%nod(1))) ! cell -> orig node -> new node
               contacts(2,n1d2dcontacts) = N2   ! 2D cell number in network_data is the same in UGRID mesh2d numbering (see below).
            end if

            if (N2 > nump) then  ! First point of 1D link is 1D cell
               K2 = netcell(N2)%nod(1)
               if (KC(K2) == 0) then
                  NUMK1D = NUMK1D+1
                  xn(NUMK1D) = xk(K2)
                  yn(NUMK1D) = yk(K2)
                  zn(NUMK1D) = zk(K2)
                  KC(K2) = -NUMK1D ! Remember new node number
               end if
               contacts(1,n1d2dcontacts) = abs(KC(netcell(N2)%nod(1))) ! cell -> orig node -> new node
               contacts(2,n1d2dcontacts) = N1   ! 2D cell number in network_data is the same in UGRID mesh2d numbering (see below).
            end if

            contacttype(n1d2dcontacts) = kn(3,L)
         endif
      enddo



      if (associated(meshgeom1d%ngeopointx)) then
         if (meshgeom1d%numnode .ge. 0) then ! TODO: LC:  check the number of mesh nodes has not changed 
         ierr = ug_write_mesh_arrays(ncid, id_tsp%meshids1d, mesh1dname, 1, UG_LOC_NODE + UG_LOC_EDGE, numk1d, n1dedges, 0, 0, &
                                    edge_nodes, face_nodes, null(), null(), null(), xn, yn, xe, ye, xzw(1:1), yzw(1:1), &
                                    crs, -999, dmiss, start_index, -999, -999, null(), null(), & ! Indexing is 1 based
                                    id_tsp%network1d, network1dname, meshgeom1d%nnodex, meshgeom1d%nnodey, nnodeids, nnodelongnames, &
                                    meshgeom1d%nedge_nodes(1,:), meshgeom1d%nedge_nodes(2,:), nbranchids, nbranchlongnames, meshgeom1d%nbranchlengths, meshgeom1d%nbranchgeometrynodes, meshgeom1d%nbranches, & 
                                    meshgeom1d%ngeopointx, meshgeom1d%ngeopointy, meshgeom1d%ngeometry, &
                                    meshgeom1d%nbranchorder, &
                                    nodeids = nodeids, nodelongnames = nodelongnames, nodebranchidx = meshgeom1d%nodebranchidx, nodeoffsets = meshgeom1d%nodeoffsets, edgebranchidx = meshgeom1d%edgebranchidx, edgeoffsets = meshgeom1d%edgeoffsets)
         else
               call mess(LEVEL_ERROR, 'Could not put header in net geometry file.')
               return
         endif
      else
         ierr = ug_write_mesh_arrays(ncid, id_tsp%meshids1d, mesh1dname, 1, UG_LOC_NODE + UG_LOC_EDGE, numk1d, n1dedges, 0, 0, &
                                    edge_nodes, face_nodes, null(), null(), null(), xn, yn, xe, ye, xzw(1:1), yzw(1:1), &
                                    crs, -999, dmiss, start_index)
      endif

      !! TODO: AvD: hier verder
      !! Determine max nr of vertices and contour points
      !
      !! NOTE: numk2d = numk - numk1d does not necessarily hold, if input grid illegally connected a 2D net link and 1D netlink to one and the same net node.
      !! Count 2D net nodes
      !numNodes   = ndx1d
      !numContPts = 0
      !do i=1,ndx1d
      !   numNodes   = max(numNodes,   size(netcell(ndx2d + i)%NOD))
      !   numContPts = max(numContPts, size(netcell(ndx2d + i)%NOD))
      !end do
      !
      !if( allocated(work2) ) deallocate( work2 )
      !allocate( work2(numContPts,ndx1d) ) ; work2 = dmiss
      !
      !ierr = nf90_def_dim(mapids%ncid, 'nmesh1d_FlowElemContourPts', numContPts,    id_flowelemcontourptsdim)
      !
      !! Flow elem contours (plot help)
      !! Todo: generalize x/y's to 2/3-D coords everywhere else [Avd]
      !ierr = nf90_def_var(mapids%ncid, 'mesh1d_FlowElemContour_x', nf90_double, (/ id_flowelemcontourptsdim, mapids%id_tsp%meshids1d%dimids(mdim_node) /), id_flowelemcontourx)
      !ierr = nf90_def_var(mapids%ncid, 'mesh1d_FlowElemContour_y', nf90_double, (/ id_flowelemcontourptsdim, mapids%id_tsp%meshids1d%dimids(mdim_node) /), id_flowelemcontoury)
      !ierr = unc_addcoordatts(mapids%ncid, id_flowelemcontourx, id_flowelemcontoury, jsferic)
      !ierr = nf90_put_att(mapids%ncid, id_flowelemcontourx, 'long_name',     'list of x-coordinates forming flow element')
      !ierr = nf90_put_att(mapids%ncid, id_flowelemcontoury, 'long_name',     'list of y-coordinafltes forming flow element')
      !ierr = nf90_put_att(mapids%ncid, id_flowelemcontourx, '_FillValue', dmiss)
      !ierr = nf90_put_att(mapids%ncid, id_flowelemcontoury, '_FillValue', dmiss)
      !
      !ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%meshids1d%varids(mid_nodex), 'bounds', 'mesh1d_FlowElemContour_x')
      !ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%meshids1d%varids(mid_nodey), 'bounds', 'mesh1d_FlowElemContour_y')
      !
      !ierr = nf90_enddef(mapids%ncid)
      !
      !do i=1,ndx1d
      !   nn = size(nd(ndx2d + i)%x)
      !   do n = 1,nn
      !      work2(n,i)=nd(ndx2d + i)%x(n)
      !   enddo
      !enddo
      !ierr = nf90_put_var(mapids%ncid, id_flowelemcontourx, work2(1:numContPts,1:ndx1d), (/ 1, 1 /), (/ numContPts, ndx1d /) )
      !
      !do i=1,ndx1d
      !   nn = size(nd(ndx2d + i)%x)
      !   do n = 1,nn
      !      work2(n,i)=nd(ndx2d + i)%y(n)
      !   enddo
      !enddo
      !ierr = nf90_put_var(mapids%ncid, id_flowelemcontoury, work2(1:numContPts,1:ndx1d), (/ 1, 1 /), (/ numContPts, ndx1d /) )
      !ierr = nf90_redef(mapids%ncid)
      !
      !deallocate( work2 )
      !
      ! Add edge type variable (edge-flowlink relation)
      call write_edge_type_variable(ncid, id_tsp%meshids1d, mesh1dname, edge_type)  

      if (numk1d > 0) then
         ierr = ug_inq_varid(ncid, id_tsp%meshids1d, 'node_z', id_tsp%id_netnodez(1)) ! TODO: AvD: keep this here as long as ug itself does not WRITE the zk data.
         ! TODO: AvD: move cell_measure  'point' to io_ugrid, or not? Check comm with G.Lang.

         ierr = nf90_enddef(ncid)
         ierr = nf90_put_var(ncid, id_tsp%id_netnodez(1), zn)
         ierr = nf90_redef(ncid) ! TODO: AvD: I know that all this redef is slow. Split definition and writing soon.
      end if

      deallocate(xn)
      deallocate(yn)
      deallocate(edge_nodes)
      deallocate(edge_type)
   end if ! 1D network geometry

   numl2d = numl-numl1d
   if (numl2d > 0) then ! 2D net geometry
      call realloc(edge_nodes, (/ 2, numl2d /), fill = -999, keepExisting = .false.)
      call realloc(edge_type, numl2d, fill = -999, keepExisting = .false.)
      call realloc(xe, numl2d, fill = dmiss, keepExisting = .false.)
      call realloc(ye, numl2d, fill = dmiss, keepExisting = .false.)
      
      ! All 2D net links
      ! Count first:
      KC(:) = 0
      NUMK2D = 0
      do L=NUML1D+1,NUML
         K1 = KN(1,L)
         K2 = KN(2,L)
         if (KC(K1) == 0) then
            NUMK2D = NUMK2D+1
            KC(K1) = 1
         end if
         if (KC(K2) == 0) then
            NUMK2D = NUMK2D+1
            KC(K2) = 1
         end if
      enddo
      
      ! Nodes Allocate 
      call realloc(xn, NUMK2D)
      call realloc(yn, NUMK2D)
      call realloc(zn, NUMK2D)

      k = 0
      KC(:) = 0
      do L = NUML1D+1,NUML
         LNEW = L - NUML1D
         K1 = KN(1,L)
         K2 = KN(2,L)
         if (KC(K1) == 0) then
            k = k+1
            xn(k) = xk(K1)
            yn(k) = yk(K1)
            zn(k) = zk(K1)
            KC(K1) = -k ! Remember new node number
         end if
         if (KC(K2) == 0) then
            k = k+1
            xn(k) = xk(K2)
            yn(k) = yk(K2)
            zn(k) = zk(K2)
            KC(K2) = -k ! Remember new node number
         end if

         edge_nodes(1,Lnew) = abs(KC(KN(1,L)))
         edge_nodes(2,Lnew) = abs(KC(KN(2,L)))
         edge_type(Lnew)    = KN(3,L) ! TODO: AvD: later, restore UG_EDGE_TYPE params, these have gotten lost.

         xe(Lnew) = .5d0*(xk(K1) + xk(K2)) ! TODO: AvD: make this sferic+3D-safe
         ye(Lnew) = .5d0*(yk(K1) + yk(K2)) ! TODO: AvD: make this sferic+3D-safe
      enddo
      
      ! Determine max nr of vertices and contour points
      nv =  0
      do i=1,NUMP ! 2D cells
         nv = max(nv, netcell(i)%n)
      end do

      ! Note: AvD: numk may be larger than nr of cell corners. Will cause problems when writing output data on corners (mismatch in dimensions), not crucial now.
      call realloc(face_nodes, (/ nv, NUMP /), fill = -999)
      do i=1,NUMP
         nn  = size(netcell(i)%NOD)
         do k=1,nn
            face_nodes(k,i) = abs(KC(netcell(i)%NOD(k))) ! Use permuted 2D node numbers
         end do
      end do
      ! TODO: AvD: lnx1d+1:lnx includes open bnd links, which may *also* be 1D boundaries (don't want that in mesh2d)
      ierr = ug_write_mesh_arrays(ncid, id_tsp%meshids2d, mesh2dname, 2, UG_LOC_EDGE + UG_LOC_FACE, numk2d, numl2d, nump, nv, &
                                    edge_nodes, face_nodes, null(), null(), null(), xn, yn, xe, ye, xzw(1:nump), yzw(1:nump), &
                                    crs, -999, dmiss, start_index)
  
      ! Add edge type variable (edge-flowlink relation)
      call write_edge_type_variable(ncid, id_tsp%meshids2d, mesh2dname, edge_type)  

      if (numk2d > 0) then
         ierr = ug_inq_varid(ncid, id_tsp%meshids2d, 'node_z', id_tsp%id_netnodez(2)) ! TODO: AvD: keep this here as long as ug itself does not WRITE the zk data.
         ierr = nf90_enddef(ncid)
         ierr = nf90_put_var(ncid, id_tsp%id_netnodez(2), zn)
         ierr = nf90_redef(ncid) ! TODO: AvD: I know that all this redef is slow. Split definition and writing soon.
      end if

      !define 1d2dcontacts only after mesh2d is completly defined  
      if (n1d2dcontacts > 0) then
         ierr = ug_def_mesh_contact(ncid, id_tsp%meshcontacts, trim(contactname), n1d2dcontacts, id_tsp%meshids2d, id_tsp%meshids1d, UG_LOC_NODE, UG_LOC_FACE, start_index)
         ierr = nf90_enddef(ncid)
         ! Put the contacts
         ierr = ug_put_mesh_contact(ncid, id_tsp%meshcontacts, contacts(1,:), contacts(2,:), contacttype) 
         ierr = nf90_redef(ncid) ! TODO: AvD: I know that all this redef is slow. Split definition and writing soon.
      endif


      if (janetcell_ /= 0 .and. nump > 0) then
         !ierr = nf90_def_dim(mapids%ncid, 'nmesh2d_NetElemMaxNode', nv,     id_netelemmaxnodedim)
         ierr = nf90_def_dim(ncid, 'n'//trim(mesh2dname)//'_NetLinkContourPts', 4,   id_netlinkcontourptsdim) ! Momentum control volume a la Perot: rectangle around xu/yu
      end if

      ierr = nf90_def_var(ncid, trim(mesh2dname)//'_NetLinkContour_x',     nf90_double, (/ id_netlinkcontourptsdim, id_tsp%meshids2d%dimids(mdim_edge) /) ,   id_netlinkcontourx)
      ierr = nf90_def_var(ncid, trim(mesh2dname)//'_NetLinkContour_y',     nf90_double, (/ id_netlinkcontourptsdim, id_tsp%meshids2d%dimids(mdim_edge) /) ,   id_netlinkcontoury)
      ierr = unc_addcoordatts(ncid, id_netlinkcontourx, id_netlinkcontoury, jsferic)
      ierr = nf90_put_att(ncid, id_netlinkcontourx, 'long_name'    , 'list of x-contour points of momentum control volume surrounding each net/flow link')
      ierr = nf90_put_att(ncid, id_netlinkcontoury, 'long_name'    , 'list of y-contour points of momentum control volume surrounding each net/flow link')
      ierr = nf90_put_att(ncid, id_netlinkcontourx, '_FillValue', dmiss)
      ierr = nf90_put_att(ncid, id_netlinkcontoury, '_FillValue', dmiss)
      !ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%meshids2d%varids(mid_edgex), 'bounds', 'mesh1d_FlowElemContour_x') ! TODO: AvD: this would conflict with io_ugrid bounds for edge: 2 endpoints, instead of momentum cell
      !ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%meshids2d%varids(mid_edgey), 'bounds', 'mesh1d_FlowElemContour_y')

      deallocate(xn)
      deallocate(yn)
      deallocate(zn)
      deallocate(face_nodes)
      deallocate(edge_nodes)
      deallocate(edge_type)
   end if

   ierr = nf90_enddef(ncid)

   ! -- Start data writing (time-independent data) ------------
    if ( janetcell_ /= 0 .and. nump1d2d > 0) then
       !! Write net cells 
       !ierr = nf90_inquire_dimension(inetfile, id_netelemmaxnodedim, len = nv)
       !allocate(netcellnod(nv, nump1d2d))
       !allocate(netcelllin(nv, nump1d2d))
       !netcellnod = intmiss
       !netcelllin = intmiss
       !do k = 1, nump1d2d
       !   nv1 = netcell(k)%n
       !   netcellnod(1:nv1,k) = netcell(k)%nod
       !   netcelllin(1:nv1,k) = netcell(k)%lin
       !enddo
       !ierr = nf90_put_var(inetfile, id_netelemnode, netcellnod)
       !call check_error(ierr, 'Write netcell elem.')
       !ierr = nf90_put_var(inetfile, id_netelemlink, netcelllin)
       !call check_error(ierr, 'Write netcell links')
       !deallocate(netcellnod)
       !deallocate(netcelllin)
  
       !call readyy('Writing net data',.65d0)
       allocate(xtt(4, numl), ytt(4, numl))
       xtt = dmiss
       ytt = dmiss
       !do L=1,numl1d
       !   xut(L) = .5d0*(xk(kn(1,L)) + xk(kn(2,L)))
       !   yut(L) = .5d0*(yk(kn(1,L)) + yk(kn(2,L)))
       !end do

       do L=numl1d+1,numl
          xtt(:,L) = 0d0
          ytt(:,L) = 0d0
          if (LNN(L) >= 1) then
             K1 = kn(1,L)
             k2 = kn(2,L)

             ! 'left' half
             n1 = LNE(1,L)
             if (n1 < 0) n1 = -n1
             x3 = xk(k1)
             y3 = yk(k1)
             x4 = xk(k2)
             y4 = yk(k2)
             xzn = xz(n1)
             yzn = yz(n1)

             ! Normal distance from circumcenter to net link:
             call dLINEDIS2(xzn,yzn,x3, y3, x4, y4,JA,DIS,XP,YP,rl)

             ! Note: we're only in net-mode, not yet in flow-mode, so we can NOT assume that net node 3->4 have a 'rightward' flow node orientation 1->2
             ! Instead, compute outward normal, and swap points 3 and 4 if necessary, such that we locally achieve the familiar k3->k4 + n1->n2 orientation.
             ! Outward normal vector of net link (cell 1 considered 'inward'):
             call normaloutchk(x3, y3, x4, y4, xzw(n1), yzw(n1), xperp, yperp, ja, jsferic, jasfer3D, dmiss, dxymis)
             if (ja == 1) then ! normal was flipped, so swap net point 3 and 4 locally, to construct counterclockwise cell hereafter.
                kt = k1
                k1 = k2
                k2 = kt
                xp = x3
                yp = y3
                x3 = x4
                y3 = y4
                x4 = xp
                y4 = yp
             end if

             xtt(1,L) = x4 - DIS*xperp
             ytt(1,L) = y4 - DIS*yperp
             xtt(2,L) = x3 - DIS*xperp
             ytt(2,L) = y3 - DIS*yperp
             
             if (LNN(L) == 2) then
                ! second half
                n1 = LNE(2,L)
                if (n1 < 0) n1 = -n1
                xzn = xz(n1)
                yzn = yz(n1)
                call dLINEDIS2(xzn,yzn,x3, y3, x4, y4,JA,DIS,XP,YP,rl)
                xtt(3, L) = x3 + DIS*xperp
                ytt(3, L) = y3 + DIS*yperp
                xtt(4, L) = x4 + DIS*xperp
                ytt(4, L) = y4 + DIS*yperp
             else
                ! closed net boundary, no second half cell, just close off with netlink
                xtt(3, L) = x3
                ytt(3, L) = y3
                xtt(4, L) = x4
                ytt(4, L) = y4
             end if
          else
             ! No surrounding cells: leave missing fill values in file.
             xtt(:,L) = dmiss
             ytt(:,L) = dmiss
          end if
       enddo
       ierr = nf90_put_var(ncid, id_netlinkcontourx, xtt, (/ 1, 1 /), (/ 4, numl2d /) )
       ierr = nf90_put_var(ncid, id_netlinkcontoury, ytt, (/ 1, 1 /), (/ 4, numl2d /) )
    end if

   ! TODO: AvD:
   ! * in WAVE: handle the obsolete 'nFlowElemWithBnd'/'nFlowElem' difference
   ! * for WAVE: add FlowElem_zcc back in com file.
   ! * for parallel: add 'FlowElemDomain', 'FlowLinkDomain', 'FlowElemGlobalNr'

   ! Leave the dataset in the same mode as we got it.
   if (jaInDefine == 1) then
      ierr = nf90_redef(ncid)
   end if

   !call readyy('Writing flow geometry data',-1d0)
   return

888 continue
   ! Possible error.

end subroutine unc_write_net_ugrid2


!> Reads the net data from a NetCDF file.
!! Processing is done elsewhere.
subroutine unc_read_net_ugrid(filename, numk_keep, numl_keep, numk_read, numl_read, ierr)
   use network_data
   use m_save_ugrid_state
   use io_netcdf
   use odugrid
   use netcdf
   use m_sferic
   use m_missing
   use unstruc_messages
   use dfm_error
   use m_alloc
   use gridoperations

   use unstruc_channel_flow
   use m_cross_helper
   use m_1d_networkreader
   use m_flow1d_reader
   use m_profiles

   character(len=*), intent(in)    :: filename           !< Name of NetCDF file.
   integer,          intent(inout) :: numk_keep          !< Number of netnodes to keep in existing net (0 to replace all).
   integer,          intent(inout) :: numl_keep          !< Number of netlinks to keep in existing net (0 to replace all).
   integer,          intent(out)   :: numk_read          !< Number of new netnodes read from file.
   integer,          intent(out)   :: numl_read          !< Number of new netlinks read from file.
   integer,          intent(out)   :: ierr               !< Return status (NetCDF operations)

   integer :: ioncid, iconvtype, start_index, networkIndex
   integer :: im, nmesh, idim, i, iv, L, numk_last, numl_last
   integer :: ncid, id_netnodez, id_netlinktype
   integer, allocatable :: kn12(:,:), kn3(:) ! Placeholder arrays for the edge_nodes and edge_types
   double precision :: convversion, zk_fillvalue, altsign
   type(t_ug_meshgeom) :: meshgeom
   
   logical           :: dflowfm_1d
   
   type(t_branch), pointer      :: pbr
   type(t_node), pointer        :: pnod
   integer                      :: istat, minp, ifil, inod, ngrd, k, k1, k2
   type (t_structure), pointer  :: pstru
   integer                      :: nstru
   
   ! 1d2d links
   integer                                   :: ncontacts, ncontactmeshes, koffset1dmesh
   integer                                   :: begin_face, end_face 
   integer, allocatable                      :: mesh1indexes(:),mesh2indexes(:), contacttype(:)
   character(len=40), allocatable            :: contactsids(:)
   character(len=80), allocatable            :: contactslongnames(:)
   character(len=40)                         :: currentNodeId
   logical                                   :: includeArrays
   logical                                   :: do_edgelengths, need_edgelengths
   double precision, allocatable             :: xface(:), yface(:)
   integer, allocatable                      :: branchStartNode(:), branchEndNode(:)
   integer                                   :: nodesOnBranchVertices
   character(len=255)                        :: tmpstring
   integer :: n1, n2, ibr_n1, ibr_n2, ibr
   double precision :: off1, off2

   numk_read = 0
   numl_read = 0
   start_index = 1
   numk_last = numk_keep
   numl_last = numl_keep
   includeArrays = .true.
   do_edgelengths = .false.
   networkIndex = 0
   koffset1dmesh = 0

   ierr = ionc_open(filename, NF90_NOWRITE, ioncid, iconvtype, convversion)

   if (ierr /= ionc_noerr .or. iconvtype /= IONC_CONV_UGRID .or. convversion < 1.0) then ! NOTE: no check on conventions version number (yet?)
      ! No valid UGRID, not a problem, call site will fall back to trying old format.
      call mess(LEVEL_DEBUG,  'unc_read_net_ugrid: net file '''//trim(filename)//''' is not UGRID. No problem, will fall back to old format reader.')
      ierr = DFM_EFILEFORMAT
      goto 999
   end if
   
   if (numk_keep == 0 .and. numl_keep == 0) then
      ! This is to allow more than one call to loadNetwork/unc_read_net_ugrid. Remove any previously read network state.
      call default_save_ugrid_state()
      call dealloc(network)
      network%loaded = .false.
      network%initialized = .false.
   else
      continue ! TODO: I don't think we support reading and appending a new 1D ugrid network to the currently loaded one.
   end if

   ! Old convention, with overlapping points
   if (allocated(mesh1dNodeIds)) deallocate(mesh1dNodeIds)
   if (allocated(mesh1dUnmergedToMerged)) deallocate(mesh1dUnmergedToMerged)
   !if (allocated(mesh1dMergedToUnMerged)) deallocate(mesh1dMergedToUnMerged)
   
   ! UNST-2510: Based on _net.nc version either read with or without duplicatie points on connection nodes.
   nodesOnBranchVertices = 1
   ierr = ionc_get_ncid(ioncid, ncid)
   tmpstring = ''
   ierr = nf90_get_att(ncid, nf90_global, 'Conventions', tmpstring)
   if (ierr == NF90_ENOTATT) then
      nodesOnBranchVertices = 0 ! New format.
      call mess(LEVEL_DEBUG,  'No NetCDF Conventions found. Defaulting to current format (>= "CF-1.8 UGRID-1.0 Deltares-0.10") for '''//trim(filename)//'''.')
   elseif (ierr == nf90_noerr) then
      i = index(tmpstring, 'Deltares')
      if (i > 0 .and. tmpstring(i+9:i+11) == '0.8') then
         nodesOnBranchVertices = 1 ! Old format
         call mess(LEVEL_DEBUG,  'Detected old format for 1D ("'//trim(tmpstring)//'") in '''//trim(filename)//'''.')
   else
         nodesOnBranchVertices = 0 ! New format
         call mess(LEVEL_DEBUG,  'Detected new format for 1D ("'//trim(tmpstring)//'") in '''//trim(filename)//'''.')
      end if
   end if
   
      
   ! Construct network with with old files (nodesOnBranchVertices). In this function 1d edge nodes (kn array) are also set
   if (nodesOnBranchVertices==1) then
      ierr = read_1d_mesh_convention_one(ioncid, numk_keep, numl_keep, numk_last, numl_last)
      ! TODO: AvD: do we need the two lines below here?
      numk_read = numk_last
      numl_read = numl_last
   endif

   ierr = ionc_get_coordinate_reference_system(ioncid, crs)
   ! ierr = ionc_get_crs(ioncid, crs) ! TODO: make this API routine.
   ! TODO: also get the %crs item from the ionc dataset, store it in unstruc, AND, use that one in unc_write_flowgeom_ugrid.
   if (ierr /= ionc_noerr) then
   call mess(LEVEL_WARN,  'ionc_get_coordinate_system: No epsg_code found in UGRID net file '''//trim(filename)//'''.')
   goto 999
   end if
   select case (crs%epsg_code)
   case (4326) ! WGS84
      jsferic = 1
      jsfertek = 0  ! TOT NADER ORDER UITGESTELD
   case default
      jsferic = 0
      jsfertek = 0
   end select

   !
   ! Prepare for multiple (partial) meshes
   !
   ierr = ionc_get_mesh_count(ioncid, nmesh)
   if (ierr /= ionc_noerr) then
      call mess(LEVEL_WARN,  'unc_read_net_ugrid: No grids found in UGRID net file '''//trim(filename)//'''.')
      goto 999
   end if
   
   !------------------------------------------------------------!
   ! meshes
   !------------------------------------------------------------!
   do im = 1, nmesh
      
      ierr = ionc_get_meshgeom(ioncid, im, networkIndex, meshgeom)
      
      if (meshgeom%dim == 1) then
         !Save meshgeom for later writing of the 1d network names
         ierr = ionc_get_meshgeom(ioncid, im, networkIndex, meshgeom, start_index, includeArrays, nbranchids, nbranchlongnames, nnodeids, nnodelongnames, & 
                                  nodeids, nodelongnames, network1dname, mesh1dname) 
         meshgeom1d = meshgeom 
         mesh1dname = meshgeom1d%meshname
         if (nodesOnBranchVertices==1) then 
             !1d edge nodes (kn array) set above
             cycle
         endif
      elseif (meshgeom%dim == 2) then
         !Else 2d/3d mesh
         if (meshgeom%numnode < 0 .or. meshgeom%numface < 0) then
            cycle
         end if
         ierr = ionc_get_meshgeom(ioncid, im, networkIndex, meshgeom, start_index, includeArrays) 
         mesh2dname = meshgeom%meshname
         !Variable to store the coordinates of face centres
         allocate(xface(meshgeom%numface)) ! TODO: LC: this is only used when there are mesh contacts. Also: have these not already been read into meshgeom%facex/y?
         allocate(yface(meshgeom%numface))
         ierr = ionc_get_face_coordinates(ioncid, im, xface, yface)
      else
         ! Only support 1D network and 2D grid
         write(msgbuf, '(a,i0,a,i0,a)') 'unc_read_net_ugrid: unsupported topology dimension ', meshgeom%dim, &
            ' in file '''//trim(filename)//' for mesh #', im, '.'
         call warn_flush()
         cycle
      end if
      
      do_edgelengths = (meshgeom%dim == 1 .and. networkIndex > 0)
      need_edgelengths = allocated(dxe) .or. do_edgelengths ! Either from a previous meshgeom, or now for the first time.
      
      !increasenetw 
      call increasenetw(numk_last + meshgeom%numnode, numl_last + meshgeom%numedge, also_dxe=need_edgelengths) ! increases XK, YK, KN, optionally dxe
      if (meshgeom%dim.ne.1) then
      ! not 1d
         ierr = ionc_get_node_coordinates(ioncid, im, XK(numk_last+1:numk_last + meshgeom%numnode), YK(numk_last+1:numk_last + meshgeom%numnode)) ! TODO: LC: this duplicates with the above get_meshgeom with includearrays=.true.
      else
         ! 1d part
         koffset1dmesh = numk_last
         ierr = odu_get_xy_coordinates(meshgeom%nodebranchidx, meshgeom%nodeoffsets, meshgeom%ngeopointx, meshgeom%ngeopointy, meshgeom%nbranchgeometrynodes, meshgeom%nbranchlengths, jsferic, meshgeom%nodeX, meshgeom%nodeY)
         XK(numk_last+1:numk_last + meshgeom%numnode) = meshgeom%nodeX
         YK(numk_last+1:numk_last + meshgeom%numnode) = meshgeom%nodeY
         network%numk = meshgeom%numnode
         ! construct network and administrate
         ierr = construct_network_from_meshgeom(network, meshgeom, nbranchids, nbranchlongnames, nnodeids, nnodelongnames, nodeids, nodelongnames, network1dname, mesh1dname, nodesOnBranchVertices)
         ! get the edge nodes, usually not available (needs to be generated)
         if (meshgeom%numedge.eq.-1) then
            ierr = ggeo_count_or_create_edge_nodes(meshgeom%nodebranchidx, meshgeom%nodeoffsets, meshgeom%nedge_nodes(1,:), meshgeom%nedge_nodes(2,:), meshgeom%nbranchlengths, start_index, meshgeom%numedge)
            call reallocP(meshgeom%edge_nodes,(/ 2, meshgeom%numedge /), keepExisting = .false.)
            meshgeom%edge_nodes = 0
            ierr = ggeo_count_or_create_edge_nodes(meshgeom%nodebranchidx, meshgeom%nodeoffsets, meshgeom%nedge_nodes(1,:), meshgeom%nedge_nodes(2,:), meshgeom%nbranchlengths, start_index, meshgeom%numedge, meshgeom%edge_nodes)
         endif
         network%numl = meshgeom%numedge
      endif

      if (ierr /= ionc_noerr) then
         write (msgbuf, '(a,i0,a)') 'unc_read_net_ugrid: Could not read x/y node coordinates from mesh #', im, ' in UGRID net file '''//trim(filename)//'''.'
         call warn_flush()
         goto 999
      end if

      ierr = ionc_get_ncid(ioncid, ncid)
      if (ierr /= ionc_noerr) then
         write (msgbuf, '(a,i0,a)') 'unc_read_net_ugrid: Could not get direct access to UGRID NetCDF net file '''//trim(filename)//'''.'
         call warn_flush()
         goto 999
      end if

      ! zk values on nodes
      ierr = ionc_inq_varid_by_standard_name(ioncid, im, UG_LOC_NODE, 'sea_floor_depth_below_geoid', id_netnodez)
      if (ierr == ionc_noerr) then
         altsign = -1d0 ! altitude as depths
      else
         ierr = ionc_inq_varid_by_standard_name(ioncid, im, UG_LOC_NODE, 'altitude', id_netnodez)
         if (ierr == ionc_noerr) then
            altsign = 1d0 ! altitude as altitudes
         else
            ! NOTE: AvD: As long as there's no proper standard_name, try some possible variable names for reading in net node z values:
            altsign = 1d0 ! altitude as altitudes
            ierr = ionc_inq_varid(ioncid, im, 'NetNode_z', id_netnodez)
            if (ierr /= ionc_noerr) then
               ierr = ionc_inq_varid(ioncid, im, 'node_z', id_netnodez)
            end if
         end if
      end if

      if (ierr == nf90_noerr) then
         ierr = nf90_get_var(ncid, id_netnodez, ZK(numk_last+1:numk_last+meshgeom%numnode))
         call check_error(ierr, 'z values')
         if (ierr == nf90_noerr) then
            ZK(numk_last+1:numk_last+meshgeom%numnode) = altsign*ZK(numk_last+1:numk_last+meshgeom%numnode)
         end if

         ! Replace the missing/fill values read from file by the kernel's dmiss missing value.
         ierr = nf90_get_att(ncid, id_netnodez, '_FillValue', zk_fillvalue)
         ! TODO: LC: should we not check for nf90 constant default double-fill-value here as a fallback?
         if (ierr == nf90_noerr) then
            if (zk_fillvalue .ne. dmiss) then
               where (ZK(numk_last+1:numk_last+meshgeom%numnode) == altsign*zk_fillvalue) ZK(numk_last+1:numk_last+meshgeom%numnode) = dmiss
            endif
         else
            ierr = ionc_noerr
         endif
      else
         ZK(numk_last+1:numk_last+meshgeom%numnode) = dmiss
      end if

      !
      ! 3. Net links. Just append the edges from the mesh(es) as netlinks, later setnodadm() at call site will group them by 1D and 2D.
      !
      if (allocated(kn12)) deallocate(kn12)
      allocate(kn12(2, meshgeom%numedge))

      if (allocated(kn3))  deallocate(kn3)
      allocate(kn3(meshgeom%numedge))

      if (meshgeom%dim.ne.1) then 
         ! TODO: LC: these have already been read into meshgeom%edge_nodes, so maybe just copy it here?
         ierr = ionc_get_edge_nodes(ioncid, im, kn12, 1) !unstruct requires 1 based indexes
      else
         kn12 = meshgeom%edge_nodes
         ierr = ionc_noerr
      endif

      if (ierr /= ionc_noerr) then
         write (msgbuf, '(a,i0,a)') 'unc_read_net_ugrid: Could not read edge-node connectivity from mesh #', im, ' in UGRID net file '''//trim(filename)//'''.'
         call warn_flush()
         goto 999
      end if

      ! TODO: AvD: replace by read-in edge_type 
      ! NOTE: AvD: even meshgeom%dim is not entirely suitable, because if a net file was saved without cell info, then we currently write topology_dimension=1, whereas we actually intend to have kn(3,:)=2.
      kn3(:) = meshgeom%dim ! was 2, Needs to be read from file at some point

      ! Backwards compatibility
      !ierr = nf90_inq_varid(ncid, 'NetLinkType', id_netlinktype)
      !if (ierr == nf90_noerr) then
      !   ierr = nf90_get_var(ncid, id_netlinktype, kn3, count = (/ meshgeom%numedge /))
      !end if

      ! ierr = ionc_inq_varid(ioncid, im, 'kn3', iv)
      !ierr = ionc_inq_varid(ioncid, im, 'kn3', iv)
      !ierr = nf90_get_var(..., iv, kn3, count=meshgeom%numedge)

      do L=1,meshgeom%numedge
         ! Append the netlink table, and also increment netnode numbers in netlink array to ensure unique ids.
         kn(1:2,numl_last+L) = numk_last + kn12(:,L)
         kn(3,  numl_last+L) = kn3(L)

         ! Determine edge (==netlink) lengths, IF present in the file.
         if (do_edgelengths) then
            ibr = meshgeom%edgebranchidx(L)
            if (ibr <= 0 .or. ibr > meshgeom%nbranches) then
               cycle
            end if
            n1 = meshgeom%edge_nodes(1,L)
            n2 = meshgeom%edge_nodes(2,L)
            if (n1 <= 0 .or. n1 > meshgeom%numnode) then
               write (msgbuf, '(a,a,a,a,a,i0,a,i0,a)') 'Error while reading ''', trim(filename), ''', mesh ''', trim(meshgeom%meshname), &
                  ''' : edge_node table for edge #', L, ' contains invalid node nr ', n1, '.'
               call warn_flush()
               ierr = DFM_WRONGINPUT
               goto 999
            end if
            if (n2 <= 0 .or. n2 > meshgeom%numnode) then
               write (msgbuf, '(a,a,a,a,a,i0,a,i0,a)') 'Error while reading ''', trim(filename), ''', mesh ''', trim(meshgeom%meshname), &
                  ''' : edge_node table for edge #', L, ' contains invalid node nr ', n2, '.'
               call warn_flush()
               ierr = DFM_WRONGINPUT
               goto 999
            end if

            ibr_n1 = meshgeom%nodebranchidx(n1)
            ibr_n2 = meshgeom%nodebranchidx(n2)
            if (ibr_n1 /= ibr .and. ibr_n2 /= ibr) then
               off1 = 0d0                             ! Start of branch
               off2 = meshgeom%nbranchlengths(ibr)    ! End of branch
            else if (ibr_n1 == ibr .and. ibr_n2 /= ibr) then
               off1 = meshgeom%nodeoffsets(n1)
               if (meshgeom%nodeoffsets(n1) < meshgeom%edgeoffsets(L)) then
                  off2 = meshgeom%nbranchlengths(ibr) ! End of branch
               else
                  off2 = 0d0                          ! Start of branch
               end if
            else if (ibr_n1 /= ibr .and. ibr_n2 == ibr) then
               off2 = meshgeom%nodeoffsets(n2)
               if (meshgeom%nodeoffsets(n2) < meshgeom%edgeoffsets(L)) then
                  off1 = meshgeom%nbranchlengths(ibr) ! End of branch
               else
                  off1 = 0d0                          ! Start of branch
               end if
            else if (ibr_n1 == ibr .and. ibr_n2 == ibr) then
               off1 = meshgeom%nodeoffsets(n1)
               off2 = meshgeom%nodeoffsets(n2)
            end if
            dxe(numl_last+L) = abs(off2 - off1)
         end if
      end do
      
      numk_read = numk_read + meshgeom%numnode 
      numk_last = numk_last + meshgeom%numnode  

      numl_read = numl_read + meshgeom%numedge
      numl_last = numl_last + meshgeom%numedge

   end do

   !------------------------------------------------------------!
   ! 1d2d contacts
   !------------------------------------------------------------!
   ierr = ionc_get_contact_topo_count(ioncid, ncontactmeshes)
   if (ierr /= ionc_noerr) then
      call mess(LEVEL_WARN,  'ug_get_contact_topo_count: No mesh contacts found in UGRID net file '''//trim(filename)//'''.')
      goto 999
   end if 
   do im = 1, ncontactmeshes
      
      ierr = ionc_get_contacts_count_ugrid(ioncid, im, ncontacts)
      
      allocate(mesh1indexes(ncontacts))
      allocate(mesh2indexes(ncontacts))
      allocate(contactsids(ncontacts))
      allocate(contactslongnames(ncontacts))
      allocate(contacttype(ncontacts))
    
      ierr = ionc_get_mesh_contact_ugrid(ioncid, im, mesh1indexes, mesh2indexes, contactsids, contactslongnames, contacttype, 1 )
      ierr = ionc_get_contact_name(ioncid, im, contactname)
      
      call increasenetw(numk_last + ncontacts, numl_last + ncontacts)      
      do l = 1, ncontacts
         XK(numk_last+l) = xface(mesh2indexes(l))
         YK(numk_last+l) = yface(mesh2indexes(l))
         currentNodeId   = nodeids(mesh1indexes(l))
         ! For 1d use mapping 
         !do i = 1, numMesh1dBeforeMerging
         !   if (mesh1dNodeIds(i) == currentNodeId) then
         !      kn(1,numl_last+l) = mesh1dUnmergedToMerged(i)
         !      exit
         !   endif
         !enddo         
         if (nodesOnBranchVertices == 1) then
            kn(1,numl_last+l) = mesh1dUnmergedToMerged(mesh1indexes(l))
         else
            kn(1,numl_last+l) = mesh1indexes(l)
         endif
         kn(2,numl_last+l) = numk_last+l
         kn(3,numl_last+l) = contacttype(l)
      enddo
      ! Set the ZK to dmiss 
      ZK(numk_last+1:numk_last+ncontacts) = dmiss
      
      numk_read = numk_read + ncontacts
      numk_last = numk_last + ncontacts  
      
      numl_read = numl_read + ncontacts
      numl_last = numl_last + ncontacts
   enddo

   ! Success
888 continue    
   ierr = ionc_close(ioncid)
   ierr = dfm_noerr
   return

999 continue
   ! Some error occurred (error code previously set)
   ! Try to close+cleanup the data set anyway.
   i = ionc_close(ioncid) ! Don't overwrite actual ierr.

end subroutine unc_read_net_ugrid


!> Reads the net data from a NetCDF file.
!! Processing is done elsewhere.
subroutine unc_read_net(filename, numk_keep, numl_keep, numk_read, numl_read, ierr)
    
    use network_data
    use m_sferic
    use m_missing
    use dfm_error
    use gridoperations
    
    character(len=*), intent(in)     :: filename  !< Name of NetCDF file.
    integer,          intent(inout)  :: numk_keep !< Number of netnodes to keep in existing net.
    integer,          intent(inout)  :: numl_keep !< Number of netlinks to keep in existing net.
    integer,          intent(out)    :: numk_read !< Number of new netnodes read from file.
    integer,          intent(out)    :: numl_read !< Number of new netlinks read from file.
    integer,          intent(out)    :: ierr      !< Return status (NetCDF operations)

    logical :: stringsequalinsens
    
    character(len=32) :: coordsyscheck
    integer, dimension(:),   allocatable :: kn3read
    integer, dimension(:),   allocatable :: kn1read
    integer, dimension(:),   allocatable :: kn2read
    
    
    integer :: inetfile, &
               id_netnodedim, id_netlinkdim, &             !< Dimensions
               id_netnodex, id_netnodey, id_netnodez, &    ! Node variables
               id_netlink, id_netlinktype, &                !< Link variables
               id_crsvar

    integer :: ja, L
    double precision :: zk_fillvalue

    call readyy('Reading net data',0d0)

    call prepare_error('Could not read NetCDF file '''//trim(filename)//'''. Details follow:')

    nerr_ = 0

    !
    ! Try and read as new UGRID NetCDF format
    !
    call unc_read_net_ugrid(filename, numk_keep, numl_keep, numk_read, numl_read, ierr)
    if (ierr == dfm_noerr) then
       ! UGRID successfully read, we're done.
       return
    else
       ! No UGRID, but just try to use the 'old' format now.
       continue
    end if

    ierr = unc_open(filename, nf90_nowrite, inetfile)
    call check_error(ierr, 'file '''//trim(filename)//'''')
    if (nerr_ > 0) return

    ! Get nr of nodes and edges
    ierr = nf90_inq_dimid(inetfile, 'nNetNode', id_netnodedim)
    call check_error(ierr, 'nNetNode')
    ierr = nf90_inq_dimid(inetfile, 'nNetLink', id_netlinkdim)
    call check_error(ierr, 'nNetLink')
    if (nerr_ > 0) return

    ierr = nf90_inquire_dimension(inetfile, id_netnodedim, len=numk_read)
    call check_error(ierr, 'node count')
    ierr = nf90_inquire_dimension(inetfile, id_netlinkdim, len=numl_read)
    call check_error(ierr, 'link count')
    if (nerr_ > 0) return

    call readyy('Reading net data',.05d0)

    ierr = nf90_inq_varid(inetfile, 'projected_coordinate_system', id_crsvar)
    if (ierr /= nf90_noerr) then
       ierr = nf90_inq_varid(inetfile, 'wgs84', id_crsvar)
    end if
    if (ierr == nf90_noerr) then
        ierr = nf90_inquire_variable(inetfile, id_crsvar, name = crs%varname)
        ierr = nf90_get_var(inetfile, id_crsvar, crs%epsg_code)
        if (crs%epsg_code == nf90_fill_int) then 
           ierr = nf90_get_att(inetfile, id_crsvar, 'epsg', crs%epsg_code)
           !if (ierr /= nf90_noerr) then
           !   ierr = nf90_get_att(datasets(ioncid)%ncid, id_crsvar, 'epsg_code', tmpstring)
           !   read(tmpstring, '(a,i0)') dummy, datasets(ioncid)%crs%epsg_code
           !end if
        end if
        ierr = ug_get_var_attset(inetfile, id_crsvar, crs%attset)
    end if

! Prepare net vars for new data and fill with values from file
    call increasenetw(numk_keep+numk_read, numl_keep+numl_read)
    call readyy('Reading net data',.1d0)

    ierr = nf90_inq_varid(inetfile, 'NetNode_x', id_netnodex)
    call check_error(ierr, 'x coordinates')

    ierr = nf90_inq_varid(inetfile, 'NetNode_y', id_netnodey)
    call check_error(ierr, 'y coordinates')

    ierr = nf90_inq_varid(inetfile, 'NetLink'    , id_netlink    )
    call check_error(ierr, 'netlinks')
    ierr = nf90_inq_varid(inetfile, 'NetLinkType', id_netlinktype)
    call check_error(ierr, 'netlinktypes')
    if (nerr_ > 0) return

    ierr = nf90_get_var(inetfile, id_netnodex,    XK(numk_keep+1:numk_keep+numk_read))
    call check_error(ierr, 'x values')
    call readyy('Reading net data',.3d0)
    ierr = nf90_get_var(inetfile, id_netnodey,    YK(numk_keep+1:numk_keep+numk_read))
    call check_error(ierr, 'y values')
    call readyy('Reading net data',.5d0)

    ierr = nf90_inq_varid(inetfile, 'NetNode_z', id_netnodez)
    if (ierr == nf90_noerr) then
        ierr = nf90_get_var(inetfile, id_netnodez,    ZK(numk_keep+1:numk_keep+numk_read))
        call check_error(ierr, 'z values')
        
        ierr = nf90_get_att(inetfile, id_netnodez, '_FillValue', zk_fillvalue)
        if (ierr == nf90_noerr) then
           if (zk_fillvalue .ne. dmiss) then
              where (ZK(numk_keep+1:numk_keep+numk_read) == zk_fillvalue) ZK(numk_keep+1:numk_keep+numk_read) = dmiss
           endif
        else
           ierr = nf90_noerr
        endif
    else
        ZK(numk_keep+1:numk_keep+numk_read) = dmiss
    end if
    call readyy('Reading net data',.7d0)

    coordsyscheck = ' '
    ierr = nf90_get_att(inetfile, id_netnodex, 'standard_name', coordsyscheck)
    if (stringsequalinsens(coordsyscheck, 'longitude')) then
        jsferic = 1
        jsfertek = 0  ! TOT NADER ORDER UITGESTELD
    else
        jsferic = 0
        jsfertek = 0
    endif
    if (jsferic == 1) then
       crs%epsg_code = 4326
    end if

    ! An array slice cannot be passed to netcdf C-library (risk of stack overflow), so use placeholder.
    allocate(kn3read(numl_read))
    allocate(kn2read(numl_read))
    allocate(kn1read(numl_read))

!    ierr = nf90_get_var(inetfile, id_netlink,     kn(:,numl_keep+1:numl_keep+numl_read), count = (/ 2, numl_read /), map=(/ 1, 3 /))
    ierr = nf90_get_var(inetfile, id_netlink,     kn1read, count = (/ 1,numl_read /))
    ierr = nf90_get_var(inetfile, id_netlink,     kn2read, count = (/ 1,numl_read /), start= (/ 2, 1 /))
    call check_error(ierr, 'netlink nodes')
    do L=numL_keep+1,numL_keep+numL_read
       kn(1,L) = kn1read(L-numL_keep)
       kn(2,L) = kn2read(L-numL_keep)
    end do
       
    ierr = nf90_get_var(inetfile, id_netlinktype, kn3read, count = (/ numl_read /))
    call check_error(ierr, 'netlink type')

    kn(3,numl_keep+1:numl_keep+numl_read) = kn3read
    ! Repair invalid kn3 codes (e.g. 0, always set to default 2==2D, i.e., don't read in thin dam codes)
    do L=numl_keep+1,numl_keep+numl_read
       if (kn(3,L) < 1) then
          kn(3,L) = 2
       end if
    end do

    deallocate(kn3read)
    deallocate(kn1read)
    deallocate(kn2read)
    call readyy('Reading net data',.95d0)

    ! Increment netnode numbers in netlink array to ensure unique ids.
    KN(1:2,numl_keep+1:numl_keep+numl_read) = KN(1:2,numl_keep+1:numl_keep+numl_read) + numk_keep
    call readyy('Reading net data',1d0)

    ierr = unc_close(inetfile)
    call readyy('Reading net data',-1d0)

end subroutine unc_read_net


!> Reads a single array variable from a map file, and optionally,
!! if it was a merged map file from parallel run, reshift the read
!! values to the actual own 1:ndxi / 1:lnx numbering.
!!
!! Details for merged-map file as single restart file for parallel models:
!! In the current parallel model, 1:ndxi contains mainly own nodes, but also several ghost nodes.
!! A merged-map file contains only unique nodes, in long blocks per partition, concatenated in one long
!! domain-global array for each quantity. The nf90_var_get will only read the block for the current rank,
!! and that will yield only 'own' nodes, not ghostnodes. All these values need to be 'spread' into the current
!! s1/u1, etc. arrays, with some empty ghost values in between here and there.
!! The calling routine should later call update_ghosts, such that ghost locations are filled as well.
function get_var_and_shift(ncid, varname, targetarr, tmparr, loctype, kmx, locstart, loccount, it_read, jamergedmap, iloc_own, iloc_merge) result(ierr)
use dfm_error
use m_flow, only: layertype
   integer, intent(in)             :: ncid !< Open NetCDF data set
   character(len=*), intent(in)    :: varname !< Variable name in file.
   double precision, intent(inout) :: targetarr(:)  !< Data will be stored in this array.
   double precision, intent(inout) :: tmparr(:)     !< Temporary work array where file data will be first read before shifting.
   integer,          intent(in)    :: loctype       !< Loc type (UNC_LOC_S, etc.)
   integer,          intent(in)    :: kmx           !< Number of layers (0 if 2D)
   integer,          intent(in)    :: locstart      !< Spatial index in file where to start reading (e.g. kstart)
   integer,          intent(in)    :: loccount      !< Spatial count in file to read (e.g. ndxi_own)
   integer,          intent(in)    :: it_read       !< Time index in file to read
   integer,          intent(in)    :: jamergedmap   !< Whether input is from a merged map file (i.e. needs shifting or not) (1/0)
   integer,          intent(in)    :: iloc_own(:)   !< Mapping array from the unique own (i.e. non-ghost) nodes/links to the actual ndxi/lnx numbering. Should be filled from index 1:loccount (e.g. 1:ndxi_own).
   integer,          intent(in)    :: iloc_merge(:) !< Mapping array from the unique own (i.e. non-ghost) nodes/links to the global/merged ndxi/lnx numbering. Should be filled from index 1:loccount (e.g. 1:ndxi_own).
   integer                         :: ierr         !< Result, DFM_NOERR if successful
   integer                         :: id_var
   integer                         :: i, ib, it, is, imap, numDims, d1, d2,nlayb, nrlay, jawarn = 0
   double precision                :: tmpval
   double precision, allocatable   :: tmparray1D(:), tmparray2D(:,:)
   integer, dimension(nf90_max_var_dims):: rhdims, tmpdims
   integer :: jamerged_dif
   
   ierr = DFM_NOERR

   if (size(iloc_merge) ==1 .and. iloc_merge(1) == -999) then
      jamerged_dif = 0 !the partition is the same, iloc_merge does not function
   else
      jamerged_dif = 1
   endif
   
   ierr = nf90_inq_varid(ncid, varname, id_var)
   if (ierr /=0) goto 999
   if (kmx == 0 .or. loctype == UNC_LOC_S .or. loctype == UNC_LOC_U) then
      if (jamergedmap /= 1) then
         ierr = nf90_get_var(ncid, id_var, targetarr(1:loccount), start = (/ locstart, it_read/), count = (/ loccount, 1 /))
      else
         if (jamerged_dif == 1) then
            ! Firstly read all the data from the file to tmparray1D, this avoinds calling nf90 subroutine in the loop
            ierr = nf90_inquire_variable(ncid, id_var, ndims=numDims, dimids=rhdims)
            do i = 1, numDims-1
               ierr = nf90_inquire_dimension(ncid, rhdims(i), len = tmpdims(i))
               if (ierr /= nf90_noerr) goto 999
            enddo
            if (numDims==2 .or. (numDims==1 .and. it_read==1)) then
                d1 = tmpdims(1)
                if(allocated(tmparray1D)) deallocate(tmparray1D)
                allocate(tmparray1D(d1))
                ierr = nf90_get_var(ncid, id_var, tmparray1D, start = (/1, it_read/), count = (/d1, 1/))
                if (ierr /= nf90_noerr) goto 999
            else
               call mess(LEVEL_WARN, 'get_var_and_shift: rank of the array  '''//trim(varname)//''' can only be 2 for 2D models (time+space).')
               goto 999
            endif
            ! Then assign the data based on the mapping
            do i = 1, loccount
               imap  = iloc_merge(i)
               targetarr(iloc_own(i)) = tmparray1D(imap)
            enddo
         else
            ierr = nf90_get_var(ncid, id_var, tmparr(1:loccount), start = (/ locstart, it_read/), count = (/ loccount, 1 /))
            if (ierr /= nf90_noerr) goto 999
            do i=1,loccount
               targetarr(iloc_own(i)) = tmparr(i)
            end do
         endif         
      end if
   else
      ! 3D array, firstly read the whole array from the nc file, and then assign the data.
      ierr = nf90_inquire_variable(ncid, id_var, ndims=numDims, dimids=rhdims)
      do i = 1, numDims-1
         ierr = nf90_inquire_dimension(ncid, rhdims(i), len = tmpdims(i))
         if (ierr /= nf90_noerr) goto 999
      enddo
      if (numDims==3) then
          d1 = tmpdims(1); d2 =tmpdims(2)
          if (allocated(tmparray2D)) deallocate(tmparray2D)
          allocate(tmparray2D(d1,d2))
          ierr = nf90_get_var(ncid, id_var, tmparray2D, start = (/1, 1, it_read/), count = (/d1, d2, 1/))
          ! TODO: consider using loccount etc.: ierr = nf90_get_var(ncid, id_var, tmparray2D, start = (/1, locstart, it_read/), count = (/d1, loccount, 1/))

          if (ierr /= nf90_noerr) goto 999
      else
         call mess(LEVEL_WARN, 'get_var_and_shift: rank of the array  '''//trim(varname)//''' can only be 3 for 3D models (time+2D space+layers).')
         goto 999
      endif
      
      do i=1,loccount
         if (jamergedmap /= 1) then
            is = i
         else 
            if (jamerged_dif == 1) then
               is = iloc_own(i)
               imap=iloc_merge(i)
            else
               is = iloc_own(i)
            endif
         end if

         if (loctype == UNC_LOC_S3D .or. loctype == UNC_LOC_W) then
            call getkbotktop(is, ib, it)  ! TODO: AvD: double check whether this original 3D restart reading was working at all with kb, kt! (no kbotktopmax here?? lbotltopmax)
            call getlayerindices(is, nlayb, nrlay)
         else if (loctype == UNC_LOC_U3D .or. loctype == UNC_LOC_WU) then
            call getLbotLtopmax(is, ib, it)
            call getlayerindicesLmax(is, nlayb, nrlay)
            !call getlayerindices(is, nlayb, nrlay)
            ! UNST-976: TODO: does NOT work for links yet. We need some setlbotltop call up in read_map, similar to sethu behavior.
            !if (layertype .ne. 1 .and. jawarn < 100)  then
            !    call mess(LEVEL_WARN, 'get_var_and_shift: reading 3D flow link data from '''//trim(varname)//''' is badly supported for z-layer models.')
            !    jawarn = jawarn + 1 
            !endif
            !nlayb = 1
            !nrlay = it-ib+1 ! For now, sigma defaults
            !goto 999
         end if
!         call getLbotLtopmax(LL,Lb,Lt)
          if (loctype == UNC_LOC_WU .or. loctype == UNC_LOC_W) then
             ib = ib -1
             nrlay = nrlay + 1
          endif
          
        if (jamerged_dif == 1) then
           targetarr(ib:it) = tmparray2D(nlayb:nlayb+nrlay-1,imap)
        else
           targetarr(ib:it) = tmparray2D(nlayb:nlayb+nrlay-1, locstart-1+i)
        endif    
      end do
   end if

   if(allocated(tmparray1D)) deallocate(tmparray1D)
   if(allocated(tmparray2D)) deallocate(tmparray2D)
999 continue

end function get_var_and_shift

   
!> Reads the flow data from a map or a rst file.
!! Processing is done elsewhere.
!subroutine unc_read_map(filename, numk_keep, numl_keep, numk_read, numl_read, ierr)
!TODO:JZ modify the name of this subroutine, since it also reads rst files. 
subroutine unc_read_map(filename, tim, ierr)
    use m_flow
    use m_flowtimes
    use m_transport, only: NUMCONST, ISALT, ITEMP, ISED1, ISEDN, ITRA1, ITRAN, constituents, itrac2const, const_names
    use m_fm_wq_processes
    use m_flowexternalforcings, only: numtracers, trnames, kbndz
    use m_sediment
    use bedcomposition_module
    use m_flowgeom
    use dfm_error
    use m_partitioninfo
    use m_alloc
    use m_timer
    use m_turbulence
    use m_samples
    use m_CrossSections
    use m_save_ugrid_state,   only: mesh1dname
    use unstruc_channel_flow, only: network
    use m_oned_functions,     only: gridpoint2cross

    character(len=*),  intent(in)       :: filename   !< Name of NetCDF file.
    real(kind=hp),     intent(in)       :: tim        !< Desired time (snapshot) to be read from map file.
    integer,           intent(out)      :: ierr       !< Return status (NetCDF operations)
                                        
    character(len=33)                   :: refdat_map !< Date time string read from map file.
    real(kind=hp)                       :: trefdat_map, trefdat_rst, trefdat_mdu
    character(len=100)             :: convformat
    
    real(fp), dimension(:,:,:), pointer :: msed
    real(fp), dimension(:,:),   pointer :: thlyr
    real(fp), dimension(:,:),   pointer :: svfrac
    
    integer :: imapfile,                        &                   
               id_flowelemdim,                  &
               id_flowlinkdim,                  &
               id_laydim,                       &
               id_wdim,                         &
               id_timedim,                      &
               id_bndsaldim,                    &
               id_bndtemdim,                    &
               id_bndseddim,                    &
               id_bnddim,                       &
               id_time,                         &
               id_timestep,                     &
               id_s1,                           & 
               id_s0,                           &
               id_u1,                           &
               id_u0,                           &
               id_q1,                           &
               id_ww1,                          &
               id_sa1,                          &
               id_tem1,                         &
               id_tsalbnd,                      &
               id_zsalbnd,                      &
               id_ttembnd,                      &
               id_ztembnd,                      &
               id_tsedbnd,                      &
               id_zsedbnd,                      &
               id_sedtotdim,                    &
               id_sedsusdim,                    &
               id_nlyrdim,                      &
               id_msed,                         &
               id_mfluff,                       &
               id_thlyr,                        &
               id_lyrfrac,                      &
               id_bodsed,                       &
               id_xzw, id_yzw, id_xu, id_yu,    &
               id_bl, id_blbnd, id_s0bnd, id_s1bnd, id_xbnd, id_ybnd, &
               id_unorma, id_vicwwu, id_tureps1, id_turkin1, id_qw, id_qa, id_hu, id_flowlink, &
               id_morft, id_taus, id_czs,                       &
               id_jmax, id_ncrs, id_flowelemcrsz, &
               id_culvertdim, id_culvert_openh


    double precision, allocatable :: xmc(:), ymc(:), xuu(:), yuu(:), xbnd_read(:), ybnd_read(:)
    integer :: id_tmp
    integer :: layerfrac
    integer, allocatable :: id_tr1(:), id_bndtradim(:), id_ttrabnd(:), id_ztrabnd(:)
    integer, allocatable :: id_sf1(:), id_bndsedfracdim(:), id_tsedfracbnd(:), id_zsedfracbnd(:)
    integer, allocatable :: id_rwqb(:)

    integer :: it_read, nt_read, ndxi_read, lnx_read, mapref, L, tok1, tok2, tok3, nbnd_read
    integer :: sedtot_read, sedsus_read, nlyr_read
    integer :: kloc,kk, kb, kt, LL, Lb, Lt, laydim, wdim, itmp, i, iconst, iwqbot, nm, Lf, j, k, nlayb, nrlay
    integer :: iostat
    logical :: fname_has_date, mdu_has_date
    integer :: titleLength, strlen, nlen, istru, jaCulvDim
    integer, allocatable :: maptimes(:)
    logical :: file_exists
    double precision, allocatable        :: max_threttim(:)
    double precision, allocatable        :: tmpvar(:,:)
    double precision, allocatable        :: tmpvar1(:)
    double precision, allocatable        :: tmpvar2(:,:,:)
    double precision, allocatable        :: tmp_s1(:), tmp_bl(:), tmp_s0(:), tmp_sa1(:), tmp_tem1(:)
    double precision, allocatable        :: tmpvar_stru(:)
    double precision, allocatable        :: rst_bodsed(:,:), rst_mfluff(:,:), rst_thlyr(:,:)
    double precision, allocatable        :: rst_msed(:,:,:)
    real(fp)                             :: mfracsum, poros, sedthick
    real(fp), dimension(stmpar%lsedtot)  :: mfrac
    integer, allocatable :: inode_own(:), ilink_own(:), inode_ghost(:), ilink_ghost(:) !< Mapping from unique flow nodes/links that are a domain's own to the actual index in 1:ndxi and 1:lnx
    integer, allocatable :: inode_merge(:), ilink_merge(:), ibnd_merge(:), inodeghost_merge(:), ilinkghost_merge(:) !< Mapping from subdomain flow nodes/links to merged map files
    integer, allocatable :: inode_merge2own(:) !< Mapping flownodes from merged file to subdomain own index
    integer, allocatable :: inode_owninv(:)    !< Mapping from actual node index to its own index
    integer, allocatable :: ln_read(:,:)       !< ln array read from the merged file
    integer, allocatable :: itmp2D(:,:)
    integer :: ndxi_own, lnx_own, ndxi_ghost, lnx_ghost !< number of nodes/links that are a domain's own (if jampi==0, ndxi_own===ndxi, lnx_own===lnx)
    integer :: ndxi_all, lnx_all !< total numbers of nodes/links among all partitions 
    integer :: ndxi_merge, lnx_merge, ndxbnd_merge
    integer :: numpart, jamergedmap, jaghost, idmn_ghost, jamergedmap_same, lmerge = 0, lugrid = 0, jafillghost
    integer :: kstart, lstart, kstart_bnd
    integer :: MSAM
    
    character(len=8)::numformat
    character(len=2)::numtrastr, numsedfracstr
    character(len=255) :: tmpstr
    
    type(t_CSType), pointer                       :: pCS
    type(t_CSType), pointer, dimension(:)         :: pCSs
    integer                                       :: n, jmax, ndx1d, nCrs
    double precision, dimension(:,:), allocatable :: work1d_z
    
    ierr = DFM_GENERICERROR
    
    numformat = '(I2.2)'

    ! Identify the type of restart file: *_rst.nc or *_map.nc
    tok1 = index( filename, '_rst.nc', success )
    tok2 = index( filename, '_map.nc', success )
    
    ! Convert the refdat from the mdu to seconds w.r.t. an absolute t0
    call maketimeinverse(refdat//'000000',trefdat_mdu, iostat)
    
    call readyy('Reading map data',0d0)
    
    call prepare_error('Could not read NetCDF restart file '''//trim(filename)//'''. Details follow:')
    nerr_ = 0
    inquire(file=filename,exist=file_exists)
    if ( .not. file_exists ) then
        call mess(LEVEL_FATAL, 'The specified file for the restart has not been found. Check your .mdu file.')
        call readyy('Reading map data',-1d0)
        return
    endif
    ierr  = unc_open(filename, nf90_nowrite, imapfile)
    call check_error(ierr, 'file '''//trim(filename)//'''')
    if (nerr_ > 0) goto 999

    !-- Sequential model, or parallel? If parallel: merged-map file, or separate partition-map files?
    ! First check whether the restart NetCDF file contains a fully merged model, or is just a partition.
    jamergedmap = 0
    jamergedmap_same = 1
    
    ! do not support a rst file of UGRID format
    ierr = nf90_get_att(imapfile, nf90_global, 'Conventions', convformat)
    lugrid = index(convformat, 'UGRID-1')
    if (lugrid > 0) then
       call mess(LEVEL_ERROR, 'The specified restart file is of UGRID format, which is not supported.')
       call readyy('Reading map data',-1d0)
       go to 999
    endif
    
    lmerge = index(filename, '_merged')
    if ( lmerge > 0) then
       jamergedmap = 1
    endif 
    
    if (jampi == 1) then
       ierr = nf90_get_att(imapfile, nf90_global, 'NumPartitionsInFile', numpart)
       if (ierr .ne. nf90_noerr  .and. jamergedmap .eq. 1) then
          call mess(LEVEL_ERROR, 'The merged restart file is not correct.')
          call readyy('Reading map data',-1d0)
          go to 999
       endif
       if (ierr == nf90_noerr) then
          if (numpart /= numranks) then
             write (msgbuf, '(a,i0,a,a,a,i0,a)') 'Partitions are different in model (', numranks, ') and in restart file `', &
                    trim(filename), ''' (', numpart, ').'
             call warn_flush() ! Error handled on call site.
             jamergedmap_same = 0
          endif
          if (jamergedmap_same == 1) then ! If rst file is a merged file, and the partitions do not change
             nerr_ = 0
             
             ierr = nf90_inq_varid(imapfile, 'partitions_face_count', id_tmp)
             call check_error(ierr, 'inquiring partitions_face_count')
             ierr = nf90_get_var(imapfile, id_tmp, ndxi_read, start=(/ my_rank+1 /))
             call check_error(ierr, 'getting partitions_face_count')
             
             ierr = nf90_inq_varid(imapfile, 'partitions_edge_count', id_tmp)
             call check_error(ierr, 'inquiring partitions_edge_count')
             ierr = nf90_get_var(imapfile, id_tmp, lnx_read, start=(/ my_rank+1 /))
             call check_error(ierr, 'getting partitions_edge_count')
             
             ierr = nf90_inq_varid(imapfile, 'partitions_face_start', id_tmp)
             call check_error(ierr, 'getting partitions_face_start')
             ierr = nf90_get_var(imapfile, id_tmp, kstart, start=(/ my_rank+1 /))
             call check_error(ierr, 'getting partitions_face_start')
             
             ierr = nf90_inq_varid(imapfile, 'partitions_edge_start', id_tmp)
             call check_error(ierr, 'getting partitions_edge_start')
             ierr = nf90_get_var(imapfile, id_tmp, lstart, start=(/ my_rank+1 /))
             call check_error(ierr, 'getting partitions_edge_start')
                          
             ! Ask file for the dimension of its own boundary points
             ierr = nf90_inq_dimid(imapfile, 'nFlowElemBnd', id_bnddim)
             if (ierr == 0) then   
                ierr = nf90_inq_varid(imapfile, 'partitions_facebnd_start', id_tmp)
                call check_error(ierr, 'getting partitions_facebnd_start')
                ierr = nf90_get_var(imapfile, id_tmp, kstart_bnd, start=(/ my_rank+1 /))
                call check_error(ierr, 'getting partitions_facebnd_start')
             
                ierr = nf90_inq_varid(imapfile, 'partitions_facebnd_count', id_tmp)
                call check_error(ierr, 'getting partitions_facebnd_count')
                ierr = nf90_get_var(imapfile, id_tmp, nbnd_read, start=(/ my_rank+1 /))
                call check_error(ierr, 'getting partitions_facebnd_count')
                
                jaoldrstfile = 0                
             else
                call mess(LEVEL_INFO, 'The restart file does not contain info. on boundaries.')
                ierr = 0
                nbnd_read = 0
                jaoldrstfile = 1
             endif
             
             if (nerr_ > 0) then
                write (msgbuf, '(a,a,a)') 'Could not read partition start/count info from file `', trim(filename), '''.'
                call warn_flush() ! Error handled on call site.
                ierr = DFM_WRONGINPUT
                goto 999
             end if
          endif
       else ! No merged-map file: no problem, we'll assume that each rank got its own unique restart file, &
            ! so just read data from start.
          kstart = 1
          lstart = 1
          kstart_bnd = 1
       end if
    else    ! Sequential model: just read all data from restart file.
       kstart = 1
       lstart = 1
       kstart_bnd = 1
    end if
    if (jampi == 0 .and. jamergedmap == 1) then 
       call mess(LEVEL_INFO, 'Restart a sequential run with a merged rst file.')
       jamergedmap_same = 0
    endif
    
    if (jampi>0 .and. kmx>0 .and. jamergedmap==1) then
       ! In the case of 3D parallel restarting with a merged rst file, 
       ! some variables on ghost cells are not communicated but filled using kd-tree
       jafillghost = 1
    else
       jafillghost = 0
    endif
    
    ! allocate inode_merge and ilink_merge in all restart situations. When the partition is the same, these two arrays do not function, 
    ! but they help to simplify the codes when calling "get_var_and_shift".
    call realloc(inode_merge,  1, keepExisting=.false., fill = -999)
    call realloc(ilink_merge,  1, keepExisting=.false., fill = -999)
    
    if (jamergedmap == 1) then
       ! If rst file is a merged-map or merged-rst file, read only a domain's own flow nodes and links.
       if (jamergedmap_same == 1) then  ! If the partitions of the model are the same with the rst file
          ndxi_own = 0
          lnx_own  = 0
          call realloc(inode_own, ndxi, keepExisting=.false.)
          call realloc(ilink_own, lnx, keepExisting=.false.)
          call realloc(tmpvar1, max(ndxi,lnx), keepExisting=.false.) ! Only necessary for buffered reading from merged map.
          
          if (nbnd_read > 0 .and. jaoldrstfile == 0) then
              do kk = 1, nbnd_read
                 ibnd_own(kk) = kk
              enddo
          endif
          
          if (jafillghost == 0) then
             do kk=1,ndxi
                if (idomain(kk) == my_rank) then
                   ndxi_own = ndxi_own + 1
                   inode_own(ndxi_own) = kk
                end if
             end do
            
             do LL=1,lnx
                call link_ghostdata(my_rank,idomain(ln(1,LL)), idomain(ln(2,LL)), jaghost, idmn_ghost, &
                                    ighostlev(ln(1,LL)), ighostlev(ln(2,LL)))
                if ( jaghost /= 1 ) then
                   lnx_own = lnx_own + 1
                   ilink_own(lnx_own) = LL
                end if
             end do
          else ! Using merged rst file in 3D problems need to fill in some variables on ghost cells
             ndxi_ghost=0
             lnx_ghost= 0
             call realloc(inode_ghost, ndxi, keepExisting=.false.)
             call realloc(inodeghost_merge, ndxi, keepExisting=.false.)
             call realloc(ilink_ghost, lnx, keepExisting=.false.)
             call realloc(ilinkghost_merge, lnx, keepExisting=.false.)
             
             do kk=1,ndxi
                if (idomain(kk) == my_rank) then
                   ndxi_own = ndxi_own + 1
                   inode_own(ndxi_own) = kk
                else
                   ndxi_ghost = ndxi_ghost + 1
                   inode_ghost(ndxi_ghost) = kk
                endif
             enddo
             
             do LL=1,lnx
                call link_ghostdata(my_rank,idomain(ln(1,LL)), idomain(ln(2,LL)), jaghost, idmn_ghost, &
                                    ighostlev(ln(1,LL)), ighostlev(ln(2,LL)))
                if ( jaghost /= 1 ) then
                   lnx_own = lnx_own + 1
                   ilink_own(lnx_own) = LL
                else
                   lnx_ghost = lnx_ghost + 1
                   ilink_ghost(lnx_ghost) = LL
                end if
             end do        

             ! prepare for kd-tree search
             ! Read coordinates of flow elem circumcenters from merged map file
             ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim) 
             call check_error(ierr, 'nFlowElem')
             ierr = nf90_inquire_dimension(imapfile, id_flowelemdim, len=ndxi_merge)
             call check_error(ierr, 'Flow Elem count')
             ierr = nf90_inq_varid(imapfile, 'FlowElem_xzw', id_xzw)
             call check_error(ierr, 'center of mass x-coordinate')
             ierr = nf90_inq_varid(imapfile, 'FlowElem_yzw', id_yzw)
             call check_error(ierr, 'center of mass y-coordinate')
             
             allocate(xmc(ndxi_merge))
             allocate(ymc(ndxi_merge))
             ierr = nf90_get_var(imapfile, id_xzw, xmc)
             ierr = nf90_get_var(imapfile, id_yzw, ymc)
             
             Ns = 0
             call find_flownodesorlinks_merge(ndxi_merge, xmc, ymc, ndxi, ndxi_ghost, inode_ghost, inodeghost_merge, 1, 1)
          
            ! read coordinates of flow links center
            ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim)
            call check_error(ierr, 'nFlowLink')
            ierr = nf90_inquire_dimension(imapfile, id_flowlinkdim, len=lnx_merge )
            call check_error(ierr, 'link count')
            ierr = nf90_inq_varid(imapfile, 'FlowLink_xu', id_xu)
            ierr = nf90_inq_varid(imapfile, 'FlowLink_yu', id_yu)
            if (nerr_ > 0) goto 999
            
            allocate(xuu(lnx_merge))
            allocate(yuu(lnx_merge))
            ierr = nf90_get_var(imapfile, id_xu, xuu)
            ierr = nf90_get_var(imapfile, id_yu, yuu)
            
            !call find_flownodesorlinks_merge(lnx_merge, xuu, yuu, lnx, lnx_own, ilink_own, ilink_merge, 0)
            call find_flownodesorlinks_merge(lnx_merge, xuu, yuu, lnx, lnx_ghost, ilink_ghost, ilinkghost_merge, 0, 1)
          endif
       else     ! If the partitions in the model are different comparing with the rst file
          ndxi_own = 0
          lnx_own  = 0
          call realloc(inode_own,   ndxi, keepExisting=.false.)
          call realloc(inode_merge, ndxi, keepExisting=.false.)
          call realloc(ilink_own,    lnx, keepExisting=.false.)
          call realloc(ilink_merge,  lnx, keepExisting=.false.)
          call realloc(inode_owninv,ndxi, keepExisting=.false., fill = -999)
          
          if (jampi == 0) then ! Restart a sequential run with a merged rst file
             ndxbnd_own = ndx - ndxi
             if (ndxbnd_own>0 .and. jaoldrstfile == 0) then
                call realloc(ibnd_own, ndxbnd_own, keepExisting=.false.)
             endif
          endif
          if (ndxbnd_own > 0 .and. jaoldrstfile == 0) then
             call realloc(ibnd_merge, ndxbnd_own, keepExisting=.false.)
          endif             
         
          !!! Set inode_own, ilink_own, ect
          ! when sequential run
          if (jampi==0) then
             ! node
             ndxi_own = ndxi
             ndxi_all = ndxi_own
             do kk=1,ndxi
                inode_own(kk) = kk
             enddo
             ! bnd
             nbnd_read = ndx - ndxi    ! Sequential restart, nbnd_read is assumed to be ndx-ndxi
             if (nbnd_read > 0 .and. jaoldrstfile == 0) then
                do kk = 1, nbnd_read
                   ibnd_own(kk) = kk
                enddo
             endif
             ! link
             lnx_own = lnx
             lnx_all = lnx_own
             do LL=1,lnx
                ilink_own(LL) = LL
             enddo
          else
            ! parallel
            if (jafillghost==0) then
               ! only consider non-ghost flow nodes
              do kk=1,ndxi
                 if (idomain(kk) == my_rank) then
                    ndxi_own = ndxi_own + 1
                    inode_own(ndxi_own) = kk
                    inode_owninv(kk) = ndxi_own
                 end if
              end do
              ! link
              do LL=1,lnx
                 call link_ghostdata(my_rank,idomain(ln(1,LL)), idomain(ln(2,LL)), jaghost, idmn_ghost, &
                                     ighostlev(ln(1,LL)), ighostlev(ln(2,LL)))
                 if ( jaghost /= 1 ) then
                    lnx_own = lnx_own + 1
                    ilink_own(lnx_own) = LL
                 end if
              end do
            else ! need to fill ghosts
               ndxi_ghost=0
               lnx_ghost= 0
               call realloc(inode_ghost, ndxi, keepExisting=.false.)
               call realloc(inodeghost_merge, ndxi, keepExisting=.false.)
               call realloc(ilink_ghost, lnx, keepExisting=.false.)
               call realloc(ilinkghost_merge, lnx, keepExisting=.false.)
               
               do kk=1,ndxi
                  if (idomain(kk) == my_rank) then
                     ndxi_own = ndxi_own + 1
                     inode_own(ndxi_own) = kk
                     inode_owninv(kk) = ndxi_own
                  else
                     ndxi_ghost = ndxi_ghost + 1
                     inode_ghost(ndxi_ghost) = kk
                  endif
               enddo
               
               do LL=1,lnx
                  call link_ghostdata(my_rank,idomain(ln(1,LL)), idomain(ln(2,LL)), jaghost, idmn_ghost, &
                                      ighostlev(ln(1,LL)), ighostlev(ln(2,LL)))
                  if ( jaghost /= 1 ) then
                     lnx_own = lnx_own + 1
                     ilink_own(lnx_own) = LL
                  else
                     lnx_ghost = lnx_ghost + 1
                     ilink_ghost(lnx_ghost) = LL
                  end if
               end do
            endif
            ! compute global number of nodes/lnx of all subdomains
            call reduce_int_sum(ndxi_own, ndxi_all)
            call reduce_int_sum(lnx_own,  lnx_all )
          endif
          
          
          !! read and prepare for inode_merge and ilink_merge,etc   
          ! Read coordinates of flow elem circumcenters from merged map file
          ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim) 
          call check_error(ierr, 'nFlowElem')
          ierr = nf90_inquire_dimension(imapfile, id_flowelemdim, len=ndxi_merge)
          call check_error(ierr, 'Flow Elem count')

          ! Check if global number of nodes in the merged rst file is equal to that in the model
          if (ndxi_all .ne. ndxi_merge) then
             write (msgbuf, '(a,i0,a,i0,a)') 'Global number of nodes among all partitions: in the merged restart file ', ndxi_merge, ',  in model: ', ndxi_all, '.'
             call warn_flush()
             call qnerror('Global number of nodes read from the merged restart file unequal to global number of nodes in model,' &
                           //' therefore some nodes may not be found',' ',' ')
          end if

          ierr = nf90_inq_varid(imapfile, 'FlowElem_xzw', id_xzw)
          call check_error(ierr, 'center of mass x-coordinate')
          ierr = nf90_inq_varid(imapfile, 'FlowElem_yzw', id_yzw)
          call check_error(ierr, 'center of mass y-coordinate')
          if (ndxbnd_own >0) then
             ierr = nf90_inq_dimid(imapfile, 'nFlowElemBnd', id_bnddim)
             if (ierr == 0) then
                 jaoldrstfile = 0
             call check_error(ierr, 'nFlowElemBnd')
             ierr = nf90_inquire_dimension(imapfile, id_bnddim, len=ndxbnd_merge)
             call check_error(ierr, 'Flow Elem bnd count')
             ierr = nf90_inq_varid(imapfile, 'FlowElem_xbnd', id_xbnd)
             call check_error(ierr, 'x-coordinate of boundary points')
             ierr = nf90_inq_varid(imapfile, 'FlowElem_ybnd', id_ybnd)
             call check_error(ierr, 'y-coordinate of boundary points')
             else
                jaoldrstfile = 1
                ierr = 0
             endif
          endif

          if (ierr == nf90_noerr) then
             ! centers of mass were stored in rst/map file, so directly read them
             allocate(xmc(ndxi_merge))
             allocate(ymc(ndxi_merge))
             ierr = nf90_get_var(imapfile, id_xzw, xmc)
             ierr = nf90_get_var(imapfile, id_yzw, ymc)
             if (ndxbnd_own >0 .and. jaoldrstfile == 0) then
                allocate(xbnd_read(ndxbnd_merge))
                allocate(ybnd_read(ndxbnd_merge))
                ierr = nf90_get_var(imapfile, id_xbnd, xbnd_read)
                ierr = nf90_get_var(imapfile, id_ybnd, ybnd_read)
             endif  
          end if

          if (nerr_ > 0) goto 999

          
          call realloc(inode_merge2own, ndxi_merge, keepExisting=.false., fill=-999)
          call find_flownodesorlinks_merge(ndxi_merge, xmc, ymc, ndxi, ndxi_own, inode_own, inode_merge, 1, 1, inode_merge2own)
         
          if (ndxbnd_own>0 .and. jaoldrstfile == 0) then 
             ! For parallel run, 'ibnd_own' and 'ndxbnd_own' has been determined in function 'flow_initexternalforcings'
             call find_flownodesorlinks_merge(ndxbnd_merge, xbnd_read, ybnd_read, ndx-ndxi, ndxbnd_own, ibnd_own, ibnd_merge, 2, 1)
          endif
          ! read coordinates of flow links center
          ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim)
          call check_error(ierr, 'nFlowLink')
          ierr = nf90_inquire_dimension(imapfile, id_flowlinkdim, len=lnx_merge )
          call check_error(ierr, 'link count')
          ! ! Check if global number of links in the merged rst file is equal to that in the model
          if (lnx_all .ne. lnx_merge) then
             write (msgbuf, '(a,i0,a,i0,a)') 'Global number of links among all partitions: in the merged restart file ', lnx_merge, ',  in model: ', lnx_all, '.'
             call warn_flush()
             call qnerror('Global number of links read from the merged restart file unequal to global number of links in model, '&
                           //' therefore some links may not be found',' ',' ')
          end if
          ierr = nf90_inq_varid(imapfile, 'FlowLink_xu', id_xu)
          ierr = nf90_inq_varid(imapfile, 'FlowLink_yu', id_yu)
          if (nerr_ > 0) goto 999
          
          allocate(xuu(lnx_merge))
          allocate(yuu(lnx_merge))
          ierr = nf90_get_var(imapfile, id_xu, xuu)
          ierr = nf90_get_var(imapfile, id_yu, yuu)
       
          call find_flownodesorlinks_merge(lnx_merge, xuu, yuu, lnx, lnx_own, ilink_own, ilink_merge, 0, 1)
          
          if (jafillghost==1) then
             call find_flownodesorlinks_merge(ndxi_merge, xmc, ymc, ndxi, ndxi_ghost, inode_ghost, inodeghost_merge, 1, 1)
             call find_flownodesorlinks_merge(lnx_merge, xuu, yuu, lnx, lnx_ghost, ilink_ghost, ilinkghost_merge, 0, 1)
          endif
          
          if ( jampi.eq.0 ) then
             if ( NS.gt.0 ) then
                call newfil(MSAM, 'rst_error.xyz')
                call wrisam(MSAM)
   !            delete samples
                NS = 0
                call mess(LEVEL_ERROR, 'restart error, unfound nodes/links are written to sample files rst_error.xyz')
             end if
          else
             if ( NS.gt.0 ) then
                call newfil(MSAM, 'rst_error_'// sdmn // '.xyz')
                call wrisam(MSAM)
                call mess(LEVEL_WARN, 'restart error, unfound nodes/links are written to sample files rst_error_'// sdmn // '.xyz')
             end if
             
!            get maximum number of flownodes with error over all subdomains             
             call reduce_key(Ns)
             
             if ( Ns.gt.0 ) then
                call mess(LEVEL_ERROR, 'restart error, please check sample files rst_error_NNNN.xyz')
             end if
             
   !         delete samples
             NS = 0
          end if
          
          deallocate(xmc, ymc, xuu, yuu)
          if(ndxbnd_own>0 .and. jaoldrstfile == 0)  deallocate(xbnd_read, ybnd_read)
       endif
    else ! If rst file is a non-merged rst file
       ! NOTE: intentional: if jampi==1, but rst file is a normal separate rst file
       !       *per* partition, just read all ndxi/lnx, including ghost nodes/links (as before)
       ndxi_own = ndxi
       lnx_own  = lnx

       nerr_ = 0

       ! Ask file for dimension id of nodes and edges
       ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim) ! Intentional: read a map/rst is *without* boundary nodes. 
                                                                    ! (so don't read nFlowElemWithBnd)
       call check_error(ierr, 'nFlowElem')
       ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim)
       call check_error(ierr, 'nFlowLink')
       if (nerr_ > 0) goto 999

       ! Ask for dimensions of nodes and edges, ergo: the number of netnodes and netlinks
       ierr = nf90_inquire_dimension(imapfile, id_flowelemdim, len=ndxi_read)
       call check_error(ierr, 'elem count')
       ierr = nf90_inquire_dimension(imapfile, id_flowlinkdim, len=lnx_read )
       call check_error(ierr, 'link count')
       if (nerr_ > 0) goto 999
       
       ! Ask file for the dimension of its own boundary points
       ierr = nf90_inq_dimid(imapfile, 'nFlowElemBnd', id_bnddim)
       if (ierr == 0) then
          ierr = nf90_inquire_dimension(imapfile, id_bnddim, len=nbnd_read)
          call check_error(ierr, 'FlowElem_bnd count')
          jaoldrstfile = 0
       else
          call mess(LEVEL_INFO, 'The restart file does not contain waterlevel info. on boundaries')
          ierr = 0
          nbnd_read = 0
          jaoldrstfile = 1
       endif
       
       ! Ask for dimension of structures
       jaCulvDim = 0
       if (network%sts%numCulverts > 0) then
          ierr = nf90_inq_dimid(imapfile, 'nCulvert', id_culvertdim)
          if (ierr /= 0) then
             call mess(LEVEL_WARN, 'read_rst: cannot read a dimension of culvert in restart file '''//trim(filename)//'''. The simulation will continue but the results may not be reliable.')
          else
             ierr = nf90_inquire_dimension(imapfile, id_culvertdim, len = nlen)
             call check_error(ierr, 'Culvert dimension')
             if (nlen /= network%sts%numCulverts) then
                call qnerror('Error reading '''//trim(filename)//''': Number of culverts read unequal to number of culverts in model',' ',' ')
                ierr = DFM_GENERICERROR
                call readyy('Reading map data',-1d0)
                goto 999
             else
                jaCulvDim = 1 ! dimsion of culvert exists and equal to the number of culverts in model
             end if
          end if
       end if
    end if

   ! check if restarting a model with Riemann boundary conditions
    if (allocated(kbndz)) then
       if( any(kbndz(4,:) .eq. 5) .and. jaoldrstfile == 1) then
          call mess(LEVEL_WARN, 'When restarting a model with Riemann boundary conditions, the restart file is suggested to be '&
                    //'a *_rst file which contians waterlevel info. on boundaries. Otherwise FM still runs but the resutls are '&
                    //'not accurate.') 
       endif
    endif
    
    if (jamergedmap_same == 1) then
      if (ndxi_read /= ndxi_own .or. lnx_read /= lnx_own) then
         tmpstr = ''
         if (jampi == 1) then
            write (tmpstr, '(a,i0,a)') 'my_rank=', my_rank, ': '
         end if
         write (msgbuf, '(a,i0,a,i0,a)') trim(tmpstr)//'#nodes in file: ', ndxi_read, ', #nodes in model: ', ndxi_own, '.'
         call warn_flush()
         write (msgbuf, '(a,i0,a,i0,a)') trim(tmpstr)//'#links in file: ', lnx_read, ', #links in model: ', lnx_own, '.'
         call warn_flush()
         call qnerror('Error reading '''//trim(filename)//''': Number of nodes/links read unequal to nodes/links in model',' ',' ')
         ierr = DFM_GENERICERROR
         call readyy('Reading map data',-1d0)
         goto 999
      end if
      if (nbnd_read > 0 .and. jaoldrstfile == 0) then
         if ((jampi==0 .and. nbnd_read .ne. ndx-ndxi) .or. (jampi>0 .and. nbnd_read .ne. ndxbnd_own)) then
            tmpstr = ''
            write (msgbuf, '(a,i0,a,i0,a)') trim(tmpstr)//'#boundary points in file: ', nbnd_read, ', #boundary points in model: ', &
                   ndx-ndxi, '.'
            call warn_flush()
            call qnerror('Number of boundary points unequal to those in model',' ',' ')
            ierr = DFM_GENERICERROR
            call readyy('Reading map data',-1d0)
            goto 999
            endif
      endif
      
      if (jamergedmap == 0) then
      !! check if the flownodes/flowlinks numbering index is the same
      ! only check when sequential restart and parallel restart with its own rst file.
      ! TODO: check also for other restart situations
         ! Read coordinates of flownodes 
         call realloc(xmc, ndxi_read, keepExisting=.false.)
         call realloc(ymc, ndxi_read, keepExisting=.false.)
         ierr = nf90_inq_varid(imapfile, 'FlowElem_xzw', id_xzw)
         if (ierr == nf90_noerr) call check_error(ierr, 'center of mass x-coordinate')
         if (ierr == nf90_noerr) ierr = nf90_inq_varid(imapfile, 'FlowElem_yzw', id_yzw)
         if (ierr == nf90_noerr) call check_error(ierr, 'center of mass y-coordinate')
         
         if (ierr == nf90_noerr) ierr = nf90_get_var(imapfile, id_xzw, xmc)
         if (ierr == nf90_noerr) ierr = nf90_get_var(imapfile, id_yzw, ymc)
         
         if (ierr == nf90_noerr) then
            ! check flownodes numbering with rst file
            call check_flownodesorlinks_numbering_rst(ndxi, 1, xmc, ymc, ierr)
            if (ierr .ne. nf90_noerr) then
               goto 999
            end if
         else
            call mess(LEVEL_WARN, 'Skip checking flownodes numbering when restart, '&
                       //'because flownodes coordinates are missing in rst file.')
         end if      
         
         ! Read coordinates of flowlinks
         call realloc(xuu, lnx_read, keepExisting=.false.)
         call realloc(yuu, lnx_read, keepExisting=.false.)
         ierr = nf90_inq_varid(imapfile, 'FlowLink_xu', id_xu)
         call check_error(ierr, 'velocity point x-coordinate')
         ierr = nf90_inq_varid(imapfile, 'FlowLink_yu', id_yu)
         call check_error(ierr, 'velocity point y-coordinate')
         ierr = nf90_get_var(imapfile, id_xu, xuu)
         ierr = nf90_get_var(imapfile, id_yu, yuu)
         
         if (ierr == nf90_noerr) then
            ! Check flowlinks numbering with rst file
            call check_flownodesorlinks_numbering_rst(lnx, 0, xuu, yuu, ierr)
            if (ierr .ne. nf90_noerr) then
               goto 999
            end if
         else
            call mess(LEVEL_WARN, 'Skip checking flowlinks numbering when restart, '&
                       //'because flowlinks coordinates are missing in rst file.')
         end if
      end if
    endif
    call readyy('Reading map data',0.05d0)
    
    ! Choose latest timestep
    ierr = nf90_inq_dimid        (imapfile, 'time'    , id_timedim )
    call check_error(ierr, 'time')
    ierr = nf90_inquire_dimension(imapfile, id_timedim, len=nt_read)
    call check_error(ierr, 'time')
    if (nt_read.eq.0) then
        call qnerror('There do not exist any time data in file ',trim(filename),' ')
        call readyy('Reading map data',-1d0)
        return
    end if
    
    call readyy('Reading map data',0.10d0)
   
    iostat = 0
    call maketimeinverse(restartdatetime(1:14),trefdat_rst,iostat)    ! result: refdatnew in seconds  w.r.t. absolute MDU refdat
    mdu_has_date = (iostat==0) 
    
    ! Restart from *yyyymmdd_hhmmss_rst.nc
    !              15    0 8  5   1^tok1      
    if (tok1 .gt. 0) then       
           
       ! Derive time from restart file name (first: check if the string length is larger than 15 characters at all!)
       it_read     = 1

       fname_has_date = .false.
       if (tok1 .gt. 15) then
          tmpstr  = filename(tok1-15:tok1-8)//filename(tok1-6:tok1-1)
          call maketimeinverse(tmpstr(1:14), trefdat_rst, iostat)
          fname_has_date = (iostat==0)    
          tok3    = index( filename(tok1-15:tok1-1), '_', success )
          fname_has_date = fname_has_date .and. success                   ! require connecting underscore between date and time 
       endif 
            
       if (.not.fname_has_date) then
          if (.not.mdu_has_date) then
             call mess(LEVEL_WARN, 'No valid date-time-string in either the MDU-file or *yyyymmdd_hhmmss_rst.nc filename: '''// &
                       trim(filename)//'''.')
             ierr = DFM_WRONGINPUT
             goto 999
          else 
             call mess(LEVEL_INFO, 'No valid date-time-string in *yyyymmdd_hhmmss_rst.nc filename: '''//trim(filename)  &
                             //'''. MDU RestartDateTime of '//restartdatetime(1:14)//' will be used.')
          endif 
       endif 

       ! Check if restart time is within specified simulation time window       
       ! NOTE: UNST-1094, intentional behavior keep the original Tstart 
       if (trefdat_rst /= tstart_user) then
           call mess(LEVEL_INFO, 'Datetime for restart state differs from model start date/time. Will use it anyway, and keep '&
                     //'TStart the same.')
           tmpstr = ''
           call maketime(tmpstr, trefdat_rst)
           msgbuf = 'Datetime for rst: '//trim(tmpstr)
           
           call maketime(tmpstr, tstart_user)
           msgbuf = trim(msgbuf)//', start datetime for model: '//trim(tmpstr)
           call msg_flush()
       end if
    end if         

    ! Restart from *_map.nc
    if (tok2 .gt. 0) then
        if (.not. mdu_has_date) then
           call mess(LEVEL_WARN, 'Missing RestartDateTime in MDU file. Will not read from map file '''//trim(filename)//'''.')
           ierr = DFM_WRONGINPUT
           goto 999
        end if

        allocate(maptimes(nt_read),STAT=ierr)
        ! Read reference time of the underlying computation
        ! Seconds since yyyy-dd-mm HH:MM:SS
        ! 123456789012345678901234567890123
        ierr = nf90_inq_varid(imapfile, 'time', id_time)
        ierr = nf90_inquire_attribute(imapfile, id_time, "units", len = titleLength)
        ierr = nf90_get_att(imapfile, id_time, "units", refdat_map)
        tmpstr = ' '
        tmpstr  = refdat_map(15:18)//refdat_map(20:21)//refdat_map(23:24)//refdat_map(26:27)//refdat_map(29:30)//refdat_map(32:33) 
        call maketimeinverse(trim(tmpstr),trefdat_map,iostat)             ! result: refdatold in seconds  w.r.t. absolute t0
    
        ! Read map times
        ierr = nf90_inq_varid(imapfile, 'time', id_time)
        ierr = nf90_get_var(imapfile, id_time, maptimes)
        call check_error(ierr, 'time')  
        call readyy('Reading map data',0.20d0)
    
        ! Find last map time <= restartdatetime
        it_read = 0
        do L = nt_read,1,-1
            if (maptimes(L) + trefdat_map <= trefdat_rst) then
                it_read = L
                exit
            end if
        end do

        ! If no map time was found <= restartdatetime, issue warning
        if (it_read == 0) then
            ! TODO: warning
            ! And stop, because no suitable restart time found.
            call mess(LEVEL_WARN, 'No suitable restart time found in '''//trim(filename)//''', using '//trim(restartdatetime)//'.')
            ierr = DFM_WRONGINPUT
            goto 999
        end if
        if (maptimes(it_read) + trefdat_map /= trefdat_rst) then
            call maketime(tmpstr, maptimes(it_read) + trefdat_map)
            call mess(LEVEL_WARN, 'Could not find exact restart datetime in '''//trim(filename)// &
                                  ''', now selected: '//tmpstr)
            ! And proceed, because this is still a good restart time.
        end if
                   
        ! NOTE: UNST-1094, intentional behavior keep the original Tstart
        if (trefdat_map /= tstart_user) then
           call mess(LEVEL_INFO, 'Datetime for restart state differs from model start date/time. Will use it anyway, and keep '&
                     //'TStart the same.')
           tmpstr = ''
           call maketime(tmpstr, trefdat_map)
           msgbuf = 'Datetime for rst: '//trim(tmpstr)
           
           call maketime(tmpstr, tstart_user)
           msgbuf = trim(msgbuf)//', start datetime for model: '//trim(tmpstr)
           call msg_flush()
        end if            
    end if
    
    ! Read size of latest timestep
    ierr = nf90_inq_varid(imapfile, 'timestep', id_timestep)
    ierr = nf90_get_var(imapfile, id_timestep,  dt_init, start = (/   it_read/))
    call check_error(ierr, 'timestep')
    dts = dt_init
    
    ! Read following variables no matter if it is the same or different partitions
    ! Read waterlevels (flow elem)
    ierr = get_var_and_shift(imapfile, 's1', s1, tmpvar1, UNC_LOC_S, kmx, kstart, ndxi_own, it_read, jamergedmap, inode_own, &
                             inode_merge)
    call check_error(ierr, 'waterlevels')    
    call readyy('Reading map data',0.30d0)
    
    ! Read waterlevels old (flow elem)
    ierr = get_var_and_shift(imapfile, 's0', s0, tmpvar1, UNC_LOC_S, kmx, kstart, ndxi_own, it_read, jamergedmap, inode_own, &
                             inode_merge)
    
    call check_error(ierr, 'waterlevels old')    
    call readyy('Reading map data',0.35d0)
    
    ! Read bedlevels (flow elem)
    if (jaoldrstfile == 1) then
       call mess(LEVEL_INFO, 'The restart file is of an old version, therefore no bedlevel info is read')
    else if (jased > 0) then 
       ierr = get_var_and_shift(imapfile, 'FlowElem_bl', bl, tmpvar1, UNC_LOC_S, kmx, kstart, ndxi_own, 1, jamergedmap, &
                                inode_own, inode_merge)
       call check_error(ierr, 'FlowElem_bl')
    endif
    ! Read normal velocities (flow link)
    ierr = get_var_and_shift(imapfile, 'unorm', u1, tmpvar1, UNC_LOC_U3D, kmx, Lstart, lnx_own, it_read, jamergedmap, &
                             ilink_own, ilink_merge)
    call check_error(ierr, 'normal velocities')    
    call readyy('Reading map data',0.40d0)
    
    ! Read normal velocities old (flow link)
    ierr = get_var_and_shift(imapfile, 'u0', u0, tmpvar1, UNC_LOC_U3D, kmx, Lstart, lnx_own, it_read, jamergedmap, &
                             ilink_own, ilink_merge)
    call check_error(ierr, 'normal velocities old')    
    call readyy('Reading map data',0.45d0)
    
    ! Read discharges (flow link)
    ierr = get_var_and_shift(imapfile, 'q1', q1, tmpvar1, UNC_LOC_U3D, kmx, Lstart, lnx_own, it_read, jamergedmap, &
                             ilink_own, ilink_merge)
    call check_error(ierr, 'discharges')    
    call readyy('Reading map data',0.50d0)
    
    if (jamergedmap_same == 1) then
       ! Read info. on waterlevel boundaries
       if (nbnd_read > 0 .and. jaoldrstfile == 0) then
          call realloc(tmp_s1, nbnd_read, stat=ierr, keepExisting=.false.)
          call realloc(tmp_s0, nbnd_read, stat=ierr, keepExisting=.false.)
          call realloc(tmp_bl, nbnd_read, stat=ierr, keepExisting=.false.)
          
          ierr = nf90_inq_varid(imapfile, 's0_bnd', id_s0bnd)
          if (ierr/=0) goto 999
          ierr = nf90_inq_varid(imapfile, 's1_bnd', id_s1bnd)
          if (ierr/=0) goto 999
          ierr = nf90_inq_varid(imapfile, 'bl_bnd', id_blbnd)
          if (ierr/=0) goto 999
          
          ierr = nf90_get_var(imapfile, id_s0bnd, tmp_s0, start=(/ kstart_bnd, it_read/), count = (/ nbnd_read, 1 /))
          call check_error(ierr, 's0_bnd')
          ierr = nf90_get_var(imapfile, id_s1bnd, tmp_s1, start=(/ kstart_bnd, it_read/), count = (/ nbnd_read, 1 /))
          call check_error(ierr, 's1_bnd')
          ierr = nf90_get_var(imapfile, id_blbnd, tmp_bl, start=(/ kstart_bnd, it_read/), count = (/ nbnd_read, 1 /))
          call check_error(ierr, 'bl_bnd')
          if (nerr_/=0) goto 999
          
          if (jampi==0) then
             do i = 1, nbnd_read
                kk = ln(1, lnxi+i)
                s0(kk) = tmp_s0(i)
                s1(kk) = tmp_s1(i)
                bl(kk) = tmp_bl(i)
             enddo
          else
             do i = 1, nbnd_read ! u and z bnd
                Lf = lnxi+ibnd_own(i) ! boundary flow link
                kk = ln(1, Lf) ! boundary flow node (the external one)
                s0(kk) = tmp_s0(i)
                s1(kk) = tmp_s1(i)
                bl(kk) = tmp_bl(i)
             enddo
          endif
       endif
       call readyy('Reading map data',0.60d0)
       
    else  ! restart with different partitions
       ! Read info. on waterlevel boundaries
       if (ndxbnd_own > 0 .and. jaoldrstfile == 0) then
          call realloc(tmp_s1, ndx-ndxi, stat=ierr, keepExisting=.false.)
          call realloc(tmp_s0, ndx-ndxi, stat=ierr, keepExisting=.false.)
          call realloc(tmp_bl, ndx-ndxi, stat=ierr, keepExisting=.false.)
          
          ierr = get_var_and_shift(imapfile, 's0_bnd', tmp_s0, tmpvar1, UNC_LOC_S, kmx, kstart, ndxbnd_own, it_read, &
                                   jamergedmap, ibnd_own, ibnd_merge)
          call check_error(ierr, 's0_bnd')
          ierr = get_var_and_shift(imapfile, 's1_bnd', tmp_s1, tmpvar1, UNC_LOC_S, kmx, kstart, ndxbnd_own, it_read, &
                                   jamergedmap, ibnd_own, ibnd_merge)
          call check_error(ierr, 's1_bnd')
          ierr = get_var_and_shift(imapfile, 'bl_bnd', tmp_bl, tmpvar1, UNC_LOC_S, kmx, kstart, ndxbnd_own, it_read, &
                                   jamergedmap, ibnd_own, ibnd_merge)
          call check_error(ierr, 'bl_bnd')
          
          do i=1,ndxbnd_own
             j=ibnd_own(i)
             Lf=lnxi+j
             kk=ln(1,Lf)
             s0(kk) = tmp_s0(j)
             s1(kk) = tmp_s1(j)
             bl(kk) = tmp_bl(j)
          enddo
       endif      
    endif
    
    ! For 3D model
    if (kmx > 0) then
       ierr =get_var_and_shift(imapfile, 'ww1', ww1, tmpvar1, UNC_LOC_W,   kmx, kstart, ndxi_own, it_read, jamergedmap, &
                               inode_own, inode_merge)
       call check_error(ierr, 'ww1') 
       ! qa
       ierr = get_var_and_shift(imapfile, 'qa', qa,  tmpvar1, UNC_LOC_U3D, kmx, Lstart, lnx_own,  it_read, jamergedmap, &
                                ilink_own, ilink_merge)
       call check_error(ierr, 'qa')
       ! qw
       ierr = get_var_and_shift(imapfile, 'qw', qw,  tmpvar1, UNC_LOC_W,   kmx, kstart, ndxi_own, it_read, jamergedmap, &
                                inode_own, inode_merge)
        call check_error(ierr, 'qw')
       ! unorm_averaged
       ierr = get_var_and_shift(imapfile, 'unorm_averaged', u1(1:lnx), tmpvar1, UNC_LOC_U, 1, Lstart, lnx_own,   it_read, &
                                jamergedmap, ilink_own,   ilink_merge)
       call check_error(ierr, 'unorm_averaged')
       ! sqi
       ierr = get_var_and_shift(imapfile, 'sqi', sqi,  tmpvar1, UNC_LOC_W,   kmx, kstart, ndxi_own, it_read, jamergedmap, &
                                inode_own, inode_merge)
       ! squ
       ierr = get_var_and_shift(imapfile, 'squ', squ,  tmpvar1, UNC_LOC_W,   kmx, kstart, ndxi_own, it_read, jamergedmap, &
                                inode_own, inode_merge)
      
       ! ghost
       if (jafillghost==1) then
          ierr = get_var_and_shift(imapfile, 'qw',  qw,  tmpvar1, UNC_LOC_W, kmx, kstart, ndxi_ghost, it_read, jamergedmap, &
                                   inode_ghost, inodeghost_merge)
          call check_error(ierr, 'qw_ghost_cells')
          ierr = get_var_and_shift(imapfile, 'unorm_averaged', u1(1:lnx), tmpvar1, UNC_LOC_U, 1, Lstart, lnx_ghost, it_read, &
                                   jamergedmap, ilink_ghost, ilinkghost_merge)
          call check_error(ierr, 'unorm_averaged_ghost_cells')
       endif
       call readyy('Reading map data',0.75d0)

       ! turbulence variables
       if ( iturbulencemodel >= 3 ) then 
          ! vicwwu
          ierr = get_var_and_shift(imapfile, 'vicwwu',  vicwwu, tmpvar1, UNC_LOC_WU, kmx, Lstart, lnx_own, it_read, &
                                   jamergedmap, ilink_own, ilink_merge)
          call check_error(ierr, 'vicwwu')    
          call readyy('Reading map data',0.76d0)
        
          ! tureps1
          ierr = get_var_and_shift(imapfile, 'tureps1', tureps1, tmpvar1, UNC_LOC_WU, kmx, Lstart, lnx_own, it_read, &
                                   jamergedmap, ilink_own, ilink_merge)
          call check_error(ierr, 'tureps1')    
          call readyy('Reading map data',0.77d0)
          
          ! turkin1   
          ierr = get_var_and_shift(imapfile, 'turkin1', turkin1, tmpvar1, UNC_LOC_WU, kmx, Lstart, lnx_own, it_read, &
                                   jamergedmap, ilink_own, ilink_merge)
          call check_error(ierr, 'turkin1')    
          call readyy('Reading map data',0.78d0)
          
          ! ghost
          if (jafillghost==1) then
            ierr = get_var_and_shift(imapfile, 'tureps1', tureps1, tmpvar1, UNC_LOC_WU, kmx, Lstart, lnx_ghost, it_read, &
                                     jamergedmap, ilink_ghost, ilinkghost_merge)
            call check_error(ierr, 'tureps1_ghost_cells')
            ierr = get_var_and_shift(imapfile, 'turkin1', turkin1, tmpvar1, UNC_LOC_WU, kmx, Lstart, lnx_ghost, it_read, &
                                     jamergedmap, ilink_ghost, ilinkghost_merge)
            call check_error(ierr, 'turkin1_ghost_cells')
          endif
       endif
    else
       ! squ
       ierr = get_var_and_shift(imapfile, 'squ', squ,  tmpvar1, UNC_LOC_W,   kmx, kstart, ndxi_own, it_read, jamergedmap, &
                                inode_own, inode_merge)
       call check_error(ierr, 'squ')
    endif
    call readyy('Reading map data', 0.80d0)

    
    ! Read the salinity (flow elem)
    if (jasal > 0) then 
        ierr = get_var_and_shift(imapfile, 'sa1', sa1, tmpvar1, UNC_LOC_S3D, kmx, kstart, ndxi_own, it_read, jamergedmap, &
                                 inode_own, inode_merge)
        call check_error(ierr, 'salinity')
    endif
    call readyy('Reading map data',0.90d0)
    ! Read the temperature (flow elem)
    if (jatem > 0) then
        
        ierr = get_var_and_shift(imapfile, 'tem1', tem1, tmpvar1, UNC_LOC_S3D, kmx, kstart, ndxi_own, it_read, jamergedmap, &
                                 inode_own, inode_merge)
        call check_error(ierr, 'temperature')
        ! TODO: [TRUNKMERGE]: HK: fill this tem1 into constituents (because below the updateghost will not work now for tem)
    endif
    
    
    ! Read the tracers
    if(ITRA1 > 0) then
       if(.not.allocated(id_tr1)) then
          allocate(id_tr1(ITRAN-ITRA1+1))
       endif
       if (allocated(tmpvar)) deallocate(tmpvar)
       allocate(tmpvar(max(1,kmx), ndxi))
       do iconst = ITRA1,ITRAN
          i = iconst - ITRA1 + 1
          tmpstr = const_names(iconst)
          ! Forbidden chars in NetCDF names: space, /, and more.
          call replace_char(tmpstr,32,95) 
          call replace_char(tmpstr,47,95) 
          ierr = nf90_inq_varid(imapfile, trim(tmpstr), id_tr1(i))
          if ( ierr.eq.NF90_NOERR ) then
!            tracer exists in restart file             
             if(kmx > 0) then
                ierr = nf90_get_var(imapfile, id_tr1(i), tmpvar(1:kmx,1:ndxi_own), start=(/ 1, kstart, it_read /), &
                                    count=(/ kmx, ndxi_own, 1 /))
                do kk = 1, ndxi_own
                   if (jamergedmap == 1) then
                      kloc = inode_own(kk)
                   else
                      kloc = kk
                   end if

                   call getkbotktop(kloc, kb, kt)
                   ! TODO: UNST-976, incorrect for Z-layers:
                   constituents(iconst,kb:kt) = tmpvar(1:kt-kb+1,kk)
                enddo
             else
                ierr = nf90_get_var(imapfile, id_tr1(i), tmpvar(1,1:ndxi_own), start = (/ kstart, it_read/), count = (/ndxi,1/))
                do kk = 1, ndxi
                   if (jamergedmap == 1) then
                      kloc = inode_own(kk)
                   else
                      kloc = kk
                   end if
                   constituents(iconst, kloc) = tmpvar(1,kk)
                end do
             end if
          endif
          call check_error(ierr, const_names(iconst))
       enddo
    endif

!   Read the water quality bottom variables
    if(numwqbots > 0) then
       call realloc(id_rwqb, numwqbots, keepExisting = .false., fill = 0)
       call realloc(tmpvar, [1, ndxi], keepExisting = .false., fill = 0.0d0)
       do iwqbot = 1, numwqbots
          tmpstr = wqbotnames(iwqbot)
          ! Forbidden chars in NetCDF names: space, /, and more.
          call replace_char(tmpstr,32,95) 
          call replace_char(tmpstr,47,95) 
          ierr = nf90_inq_varid(imapfile, trim(tmpstr), id_rwqb(iwqbot))
          if ( ierr.eq.NF90_NOERR ) then
!            water quality bottom variable exists in restart file
             ierr = nf90_get_var(imapfile, id_rwqb(iwqbot), tmpvar(1,1:ndxi_own), start = (/ kstart, it_read/), count = (/ndxi,1/))
             do kk = 1, ndxi
                if (jamergedmap == 1) then
                   kloc = inode_own(kk)
                else
                   kloc = kk
                end if
                wqbot(iwqbot, kloc) = tmpvar(1,kk)
             end do
          endif
          call check_error(ierr, wqbotnames(iwqbot))
       enddo
    endif

    ! JRE to do
    if (jased > 0 .and. stm_included) then
       if (jamergedmap == 1) then
          call mess(LEVEL_WARN, 'read_map: Morphology data not present in merged map file. Ignoring for now.')
       end if
       msed  => stmpar%morlyr%state%msed
       thlyr => stmpar%morlyr%state%thlyr
       svfrac => stmpar%morlyr%state%svfrac
       layerfrac = 0
       !
       ! Check dimensions for consistency
       !
       ierr = nf90_inq_dimid(imapfile, 'nSedTot', id_sedtotdim) ! Accept any errors, we may have a hydrodynamic restart only. Is allowed.
       ierr = nf90_inq_dimid(imapfile, 'nSedSus', id_sedsusdim)
       ierr = nf90_inq_dimid(imapfile, 'nBedLayers', id_nlyrdim)

       ierr = nf90_inquire_dimension(imapfile, id_sedtotdim, len=sedtot_read)
       if (ierr /= nf90_noerr) then
          sedtot_read = 0    ! Set the total number of sediment fractions to zero if it is not found in the _rst.nc file
          if (stmpar%lsedtot > 0) then
             write (msgbuf, '(a)') 'Restart file '''//trim(filename)//''' contains no sediment fractions, but model does. Continuing anyway.'
             call msg_flush()
          end if
       end if

       ierr = nf90_inquire_dimension(imapfile, id_sedsusdim, len=sedsus_read)
       if (ierr /= nf90_noerr) then
          sedsus_read = 0    ! Set the suspended sediment fraction to zero if it is not found in the _rst.nc file
          if (stmpar%lsedsus > 0) then
             write (msgbuf, '(a)') 'Restart file '''//trim(filename)//''' contains no suspended sediment fractions, but model does. Continuing anyway.'
             call msg_flush()
          end if
       end if

       ierr = nf90_inquire_dimension(imapfile, id_nlyrdim,   len=nlyr_read)
       if (ierr /= nf90_noerr) then
          nlyr_read = 0    ! Set the bed layer count to zero if it is not found in the _rst.nc file
          if (stmpar%morlyr%settings%nlyr > 0) then
             write (msgbuf, '(a)') 'Restart file '''//trim(filename)//''' contains no bed composition layers, but model does. Continuing anyway.'
             call msg_flush()
          end if
       end if
       !
       if ((sedtot_read .ne. stmpar%lsedtot .and. sedtot_read > 0 .and. stmpar%lsedtot > 0) .or.       &
           (sedsus_read .ne. stmpar%lsedsus .and. sedsus_read > 0 .and. stmpar%lsedsus > 0))        then
          write (msgbuf, '(a)') 'Mismatch in number of sediment fractions in morphology restart file. Not reading restart data at all.'
          call warn_flush()
          ierr = DFM_WRONGINPUT
          goto 999
       end if
       !
       ! Read morphology data:
       ! fraction concentrations
       if (stmpar%lsedsus .gt. 0 .and. sedsus_read == stmpar%lsedsus) then
          if(.not.allocated(id_sf1)) then
             allocate(id_sf1(ISEDN-ISED1+1))
          endif
          if (allocated(tmpvar)) deallocate(tmpvar)
          allocate(tmpvar(max(1,kmx), ndxi))
          do iconst = ISED1,ISEDN
             i = iconst - ISED1 + 1
             tmpstr = const_names(iconst)
             ! Forbidden chars in NetCDF names: space, /, and more.
             call replace_char(tmpstr,32,95) 
             call replace_char(tmpstr,47,95) 
             ierr = nf90_inq_varid(imapfile, trim(tmpstr), id_sf1(i))
             if (kmx > 0) then
                ierr = nf90_get_var(imapfile, id_sf1(i), tmpvar(1:kmx,1:ndxi_own), start=(/ 1, kstart, it_read /), count=(/ kmx, ndxi_own, 1 /))
                do kk = 1, ndxi_own
                   if (jamergedmap == 1) then
                      kloc = inode_own(kk)
                   else
                      kloc = kk
                   end if
                   call getkbotktop(kloc, kb, kt)
                   ! TODO: UNST-976, incorrect for Z-layers:
                   !constituents(iconst,kb:kt) = tmpvar(1:kt-kb+1,kk)
                   sed(i,kb:kt) = tmpvar(1:kt-kb+1,kk)
                enddo
             else
                ierr = nf90_get_var(imapfile, id_sf1(i), tmpvar(1,1:ndxi_own), start = (/ kstart, it_read/), count = (/ndxi,1/))
                do kk = 1, ndxi
                   if (jamergedmap == 1) then
                      kloc = inode_own(kk)
                   else
                      kloc = kk
                   end if
                   !constituents(iconst, kloc) = tmpvar(1,kk)
                   sed(i, kloc) = tmpvar(1,kk)
                end do
             endif
             call check_error(ierr, const_names(iconst))
          enddo
       end if
       
       ! morbl
       ierr = get_var_and_shift(imapfile, 'mor_bl', bl, tmpvar1, UNC_LOC_S, kmx, kstart, ndxi_own, it_read, jamergedmap, &
                                inode_own, inode_merge)
       
       ! morphological time
       ierr = nf90_inq_varid(imapfile, 'morft', id_morft)
       ierr = nf90_get_var(imapfile, id_morft,  stmpar%morpar%morft0, start = (/it_read/)) 
       stmpar%morpar%morft = stmpar%morpar%morft0
       
       ! mfluff
       if (stmpar%morpar%flufflyr%iflufflyr>0 .and. stmpar%lsedsus>0 .and. sedsus_read == stmpar%lsedsus) then
          if (allocated(tmpvar))     deallocate(tmpvar)
          if (allocated(rst_mfluff)) deallocate(rst_mfluff)
          allocate(tmpvar(sedsus_read, ndxi))
          allocate(rst_mfluff(stmpar%lsedsus, ndxi))
          ierr = nf90_inq_varid(imapfile, 'mfluff', id_mfluff)
          ierr = nf90_get_var(imapfile, id_mfluff, tmpvar(1:sedsus_read, 1:ndxi_own), start = (/ 1, kstart, it_read/), count = (/sedsus_read, ndxi,1/))
          do kk = 1, ndxi
             if (jamergedmap == 1) then
                kloc = inode_own(kk)
             else
                kloc = kk
             end if
             rst_mfluff(:, kloc) = tmpvar(:,kk)
          end do
          call check_error(ierr, 'mfluff')
       end if
       
       ! Bed composition
       if(stmpar%morlyr%settings%iunderlyr > 0 .and. nlyr_read == stmpar%morlyr%settings%iunderlyr ) then
       select case(stmpar%morlyr%settings%iunderlyr)
       case (1)
          ! bodsed
          if (allocated(tmpvar)) deallocate(tmpvar)
          if (allocated(rst_bodsed)) deallocate(rst_bodsed)
          allocate(tmpvar(sedtot_read, ndxi))
          allocate(rst_bodsed(sedtot_read, ndxi))
          do l = 1, stmpar%lsedtot
             ierr = nf90_inq_varid(imapfile, 'bodsed', id_bodsed)
             ierr = nf90_get_var(imapfile, id_bodsed, tmpvar(1:sedtot_read, 1:ndxi_own), start = (/ 1, kstart, it_read/), count = (/sedtot_read, ndxi,1/))
             do kk = 1, ndxi
                if (jamergedmap == 1) then
                   kloc = inode_own(kk)
                else
                   kloc = kk
                end if
                rst_bodsed(:, kloc) = tmpvar(:, kk)
             end do
             call check_error(ierr, 'bodsed')
          end do
          stmpar%morlyr%state%bodsed = rst_bodsed
          call bedcomp_use_bodsed(stmpar%morlyr)
       case (2)
          ! msed (/ id_sedtotdim(iid) , id_nlyrdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /)
          if (allocated(tmpvar2)) deallocate(tmpvar2)
          if (allocated(rst_msed)) deallocate(rst_msed)
          call realloc(tmpvar2,(/sedtot_read, nlyr_read, ndxi/) ,keepExisting = .false.)
          call realloc(rst_msed,(/sedtot_read, nlyr_read, ndxi/),keepExisting = .false.)
          !
          ierr = nf90_inq_varid(imapfile, 'msed', id_msed)
          do l = 1, sedtot_read
             ierr = nf90_get_var(imapfile, id_msed, tmpvar2(l, 1:nlyr_read,1:ndxi_own), start = (/ l, 1, kstart, it_read/), count = (/1, nlyr_read, ndxi, 1/))
          end do
          !
          do kk = 1, ndxi
             if (jamergedmap == 1) then
                kloc = inode_own(kk)
             else
                kloc = kk
             end if
             rst_msed(:, :, kloc) = tmpvar2(:, :, kk)
          end do
          !call check_error(ierr, 'msed')
          if (ierr .ne. 0) then
             !
             ! lyrfrac (/ id_sedtotdim(iid) , id_nlyrdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /)
             if (allocated(tmpvar2)) deallocate(tmpvar2)
             call realloc(tmpvar2,(/sedtot_read, nlyr_read, ndxi/) ,keepExisting = .false.)
             !
             ierr = nf90_inq_varid(imapfile, 'lyrfrac', id_lyrfrac)
             do l = 1, sedtot_read
                ierr = nf90_get_var(imapfile, id_lyrfrac, tmpvar2(l, 1:nlyr_read,1:ndxi_own), start = (/ l, 1, kstart, it_read/), count = (/1, nlyr_read, ndxi, 1/))
             end do
             do kk = 1, ndxi
                if (jamergedmap == 1) then
                   kloc = inode_own(kk)
                else
                   kloc = kk
                end if
                rst_msed(:, :, kloc) = tmpvar2(:, :, kk)     ! no typo, see restart_lyrs.f90
             end do
             call check_error(ierr, 'lyrfrac')
             layerfrac = 1
          end if
          !
          ! thlyr (/ id_nlyrdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /)
          if (allocated(tmpvar)) deallocate(tmpvar)
          if (allocated(rst_thlyr)) deallocate(rst_thlyr)
          call realloc(tmpvar,(/nlyr_read, ndxi/) ,keepExisting = .false.)
          call realloc(rst_thlyr,(/nlyr_read, ndxi/),keepExisting = .false.)
          !
          ierr = nf90_inq_varid(imapfile, 'thlyr', id_thlyr)
          ierr = nf90_get_var(imapfile, id_thlyr, tmpvar(1:nlyr_read,1:ndxi_own), start = (/ 1, kstart, it_read/), count = (/nlyr_read, ndxi, 1/))
          do kk = 1, ndxi
             if (jamergedmap == 1) then
                kloc = inode_own(kk)
             else
                kloc = kk
             end if
             rst_thlyr(:, kloc) = tmpvar(:, kk)
          end do
          call check_error(ierr, 'thlyr')
          !
          ! Organize layer administration
          if (stmpar%morlyr%settings%nlyr >= nlyr_read) then
             ! copy first layer
             thlyr(1, 1:ndxi)                   = rst_thlyr(1, 1:ndxi)
             msed(1:stmpar%lsedtot,1,1:ndxi)    = rst_msed(1:stmpar%lsedtot,1,1:ndxi)
             !
             do k = 2,1+stmpar%morlyr%settings%nlyr-nlyr_read
                thlyr(k,1:ndxi)                     = 0.0_fp
                msed(1:stmpar%lsedtot,k,1:ndxi)     = 0.0_fp
             enddo
             !
             ! copy remaining layers
             !
             thlyr(stmpar%morlyr%settings%nlyr-nlyr_read+2:stmpar%morlyr%settings%nlyr,1:ndxi)             = rst_thlyr(2:nlyr_read,1:ndxi)
             msed(1:stmpar%lsedtot,stmpar%morlyr%settings%nlyr-nlyr_read+2:stmpar%morlyr%settings%nlyr,1:ndxi) = rst_msed(1:stmpar%lsedtot,2:nlyr_read, 1:ndxi)
          else
             !
             ! more layers in restart file than in simulation
             !
             ! copy the first nlyr layers
             !
             thlyr(1:stmpar%morlyr%settings%nlyr, 1:ndxi)                    = rst_thlyr(1:stmpar%morlyr%settings%nlyr,1:ndxi)
             msed(1:stmpar%lsedtot, 1:stmpar%morlyr%settings%nlyr, 1:ndxi)   = rst_msed(1:stmpar%lsedtot,1:stmpar%morlyr%settings%nlyr,1:ndxi)
             !
             !
             ! add contents of other layers to last layer
             !
             do k = stmpar%morlyr%settings%nlyr+1, nlyr_read
                thlyr(stmpar%morlyr%settings%nlyr,1:ndxi)        = thlyr(stmpar%morlyr%settings%nlyr,1:ndxi) &
                                                                 & + rst_thlyr(k, 1:ndxi)
                do l = 1, stmpar%lsedtot
                   msed(l, stmpar%morlyr%settings%nlyr, 1:ndxi) = msed(l, stmpar%morlyr%settings%nlyr,1:ndxi) &
                                                                & + rst_msed(l,k,1:ndxi) 
                enddo
             enddo
          end if
          !
          if (layerfrac==1) then
             !
             ! msed contains volume fractions
             if (stmpar%morlyr%settings%iporosity==0) then
                do l = 1,stmpar%lsedtot
                   do k = 1, stmpar%morlyr%settings%nlyr
                      do nm = 1, ndxi
                         msed(l,k,nm) = msed(l,k,nm)*thlyr(k,nm)*stmpar%sedpar%cdryb(l)
                      enddo
                   enddo
                enddo
             else
                do k = 1, stmpar%morlyr%settings%nlyr
                   do nm = 1, ndxi
                      !
                      ! determine mass fractions
                      mfracsum = 0.0_fp
                      do l = 1, stmpar%lsedtot
                         mfrac(l) = msed(l,k,nm)*stmpar%sedpar%rhosol(l)
                         mfracsum = mfracsum + mfrac(l)
                      enddo
                      if (mfracsum>0.0_fp) then
                         do l = 1, stmpar%lsedtot
                            mfrac(l) = mfrac(l)/mfracsum
                         enddo
                         !
                         ! obtain porosity and sediment thickness without pores
                         !
                         call getporosity(stmpar%morlyr, mfrac, poros)
                         sedthick = thlyr(k,nm)*(1.0_fp-poros)
                      else
                         sedthick = 0.0_fp
                         poros = 0.0_fp
                      endif
                      !
                      ! convert volume fractions to sediment mass
                      !
                      do l = 1, stmpar%lsedtot
                         msed(l,k,nm) = msed(l,k,nm)*sedthick*stmpar%sedpar%rhosol(l)
                      enddo
                      svfrac(k,nm) = 1.0_fp-poros
                   enddo
                enddo
             endif
          else
             if (stmpar%morlyr%settings%iporosity>0) then
                do nm = 1, ndxi
                   sedthick = 0.0_fp
                   do l = 1, stmpar%lsedtot
                      sedthick = sedthick + msed(l,k,nm)/stmpar%sedpar%rhosol(l)
                   enddo
                   svfrac(k,nm) = sedthick/thlyr(k,nm)
                enddo
             endif
          endif
       end select
       endif
       
       ! Read 1D cross sections
       ndx1d = ndxi - ndx2d
       if (ndx1d > 0 .and. stm_included) then
          ierr = nf90_inq_dimid (imapfile, trim(mesh1dname)//'_crs_maxdim', id_jmax)
          if (ierr == 0) ierr = nf90_inquire_dimension(imapfile, id_jmax, len =jmax)
          ierr = nf90_inq_dimid (imapfile, trim(mesh1dname)//'_ncrs', id_ncrs)
          if (ierr == 0) ierr = nf90_inquire_dimension(imapfile, id_ncrs, len =nCrs)
          if (allocated(work1d_z)) deallocate(work1d_z)
          allocate(work1d_z(1:jmax,1:nCrs))
          ierr = nf90_inq_varid(imapfile, 'flowelem_crs_z', id_flowelemcrsz)
          ierr = nf90_get_var(imapfile, id_flowelemcrsz, work1d_z(1:jmax,1:nCrs), start = (/ 1, 1/), count = (/jmax, nCrs/))
          do i = 1,nCrs
            do j = 1,network%crs%cross(i)%tabdef%levelscount
              network%crs%cross(i)%tabdef%height(j) = work1d_z(j,i)
            enddo
            network%crs%cross(i)%bedlevel = work1d_z(1,i)
          enddo
       endif
    end if

    ! Read Thatcher-Harleman boundary data
    ! TODO: AvD: UNST-994: no TH data in merged files yet. Replace the 1 indices below by a prop kstart later.
    if(allocated(threttim)) then
       if (jamergedmap == 1) then
          call mess(LEVEL_WARN, 'read_map: Thatcher-Harlemann data not present in merged map file. Ignoring for now.')
       end if
      allocate(max_threttim(NUMCONST))
      max_threttim = maxval(threttim,dim=2)
      if(jasal > 0) then
         if(max_threttim(ISALT) > 0d0) then
            ierr = nf90_inq_varid(imapfile, 'tsalbnd', id_tsalbnd)
            ierr = nf90_get_var(imapfile, id_tsalbnd, thtbnds(1:nbnds), start=(/1, it_read/), count=(/nbnds, 1/))
            ierr = nf90_inq_varid(imapfile, 'zsalbnd', id_zsalbnd)
            ierr = nf90_get_var(imapfile, id_zsalbnd, thzbnds(1:nbnds*kmxd), start=(/1, it_read/), count=(/nbnds*kmxd, 1/))
         endif
      endif
      if(jatem > 0) then
         if(max_threttim(ITEMP) > 0d0) then
            ierr = nf90_inq_varid(imapfile, 'ttembnd', id_ttembnd)
            ierr = nf90_get_var(imapfile, id_ttembnd, thtbndtm(1:nbndtm), start=(/1, it_read/), count=(/nbndtm, 1/))
            ierr = nf90_inq_varid(imapfile, 'ztembnd', id_ztembnd)
            ierr = nf90_get_var(imapfile, id_ztembnd, thzbndtm(1:nbndtm*kmxd), start=(/1, it_read/), count=(/nbndtm*kmxd, 1/))
         endif
      endif
      if(jased > 0 .and. .not. stm_included) then
         if(max_threttim(ISED1) > 0d0) then
            ierr = nf90_inq_varid(imapfile, 'tsedbnd', id_tsedbnd)
            ierr = nf90_get_var(imapfile, id_tsedbnd, thtbndsd(1:nbndsd), start=(/1, it_read/), count=(/nbndsd, 1/))
            ierr = nf90_inq_varid(imapfile, 'zsedbnd', id_zsedbnd)
            ierr = nf90_get_var(imapfile, id_zsedbnd, thzbndsd(1:nbndsd*kmxd), start=(/1, it_read/), count=(/nbndsd*kmxd, 1/))            
         endif
      endif
      if (numfracs > 0) then     ! sediment fractions stm model
         if(.not. allocated(id_tsedfracbnd)) allocate(id_tsedfracbnd(numfracs))
         if(.not. allocated(id_zsedfracbnd)) allocate(id_zsedfracbnd(numfracs))
         do i=1,numfracs
            if(max_threttim(ISED1+i-1) > 0d0) then
               write(numsedfracstr,numformat) i
               ierr = nf90_inq_varid(imapfile, 'tsedfracbnd'//numsedfracstr, id_tsedfracbnd(i))
               ierr = nf90_get_var(imapfile, id_tsedfracbnd(i), bndsf(i)%tht(1:nbndsf(i)), start=(/1, it_read/), count=(/nbndsf(i), 1/))
               ierr = nf90_inq_varid(imapfile, 'zsedfracbnd'//numsedfracstr, id_zsedfracbnd(i))
               ierr = nf90_get_var(imapfile, id_zsedfracbnd(i), bndsf(i)%thz(1:nbndsf(i)*kmxd), start=(/1, it_read/), count=(/nbndsf(i)*kmxd, 1/))            
            endif
         enddo
      end if
      if(numtracers > 0) then
         if(.not. allocated(id_ttrabnd)) allocate(id_ttrabnd(numtracers))
         if(.not. allocated(id_ztrabnd)) allocate(id_ztrabnd(numtracers))
         do i=1,numtracers
            iconst = itrac2const(i)
            if(max_threttim(iconst) > 0d0) then
               write(numtrastr,numformat) i
               ierr = nf90_inq_varid(imapfile, 'ttrabnd'//numtrastr, id_ttrabnd(i))
               ierr = nf90_get_var(imapfile, id_ttrabnd(i), bndtr(i)%tht(1:nbndtr(i)), start=(/1, it_read/), &
                                   count=(/nbndtr(i), 1/))
               ierr = nf90_inq_varid(imapfile, 'ztrabnd'//numtrastr, id_ztrabnd(i))
               ierr = nf90_get_var(imapfile, id_ztrabnd(i), bndtr(i)%thz(1:nbndtr(i)*kmxd), start=(/1, it_read/), &
                                   count=(/nbndtr(i)*kmxd, 1/))            
            endif
         enddo
      endif
      call check_error(ierr, 'Thatcher-Harleman boundaries')
    endif
    
    ! Read structure info.
    if (jaCulvDim > 0 .and. network%sts%numCulverts > 0) then
       ierr = nf90_inq_varid(imapfile, 'culvert_valve_opening_height', id_culvert_openh)
       if (ierr /= 0) then
          call mess(LEVEL_WARN, 'read_rst: cannot read valve opening height of culverts in restart file'''//trim(filename)//'''. The simulation will continue but the results may not be reliable.')
       else
          call realloc(tmpvar_stru, network%sts%numCulverts, stat=ierr, keepExisting=.false.)
          ierr = nf90_get_var(imapfile, id_culvert_openh, tmpvar_stru, start=(/1, it_read/), count=(/network%sts%numCulverts, 1/))
          call check_error(ierr, 'culvert_valve_opening_height')
          
          do i = 1, network%sts%numCulverts
             istru = network%sts%culvertIndices(i)
             network%sts%struct(istru)%culvert%valveOpening = tmpvar_stru(i)
          end do
       end if
    end if
       
    call readyy('Reading map data',0.95d0)    
   
   ! Check if the orientation of each flowlink in the current model is the same with the link in the rst file
   ! If not, reverse the velocity that is read from rst file
   ! Check only when parallel restart with different partitions. ToDo: check for all the restart scenarios
   !if (jamergedmap_same == 0 .and. jampi == 1 ) then
   !   ! Read link/interface between two flow elements (flow link) from the merged file
   !   allocate(ln_read(2,lnx_own))
   !   allocate(itmp2D(2,lnx_merge))
   !   ierr = nf90_inq_varid(imapfile, 'FlowLink', id_flowlink)
   !   ierr = nf90_get_var(imapfile, id_flowlink, itmp2D)
   !   do L = 1, lnx_own
   !      LL = ilink_merge(L)
   !      ln_read(:,L) = itmp2D(:,LL)
   !   end do
   !   call check_error(ierr, 'FlowLink')
   !   
   !   ! check orientation
   !   call check_flowlink_orientation(lnx_own, ln_read, ilink_own, lnx_merge, ndxi_merge, ndxi, inode_owninv, inode_merge2own)
   !end if
   
   !-- Synchronisation to other domains, only for merged-map input
   if (jampi == 1 .and. jamergedmap == 1) then
      !-- S/S3D --
      if ( jatimer.eq.1 ) call starttimer(IUPDSALL)

      call update_ghosts(ITYPE_SALL, 1, Ndx, s1, ierr)
      call update_ghosts(ITYPE_SALL, 1, Ndx, s0, ierr)

      if (kmx == 0) then
      ! 2D
         if (jasal > 0) then
            call update_ghosts(ITYPE_Sall, 1, Ndx, sa1, ierr)
         endif
         if (jatem > 0) then
            call update_ghosts(ITYPE_Sall, 1, Ndx, tem1, ierr)
         end if
         if (ITRA1 > 0) then
            ! NOTE: This update sends too much (sa1/tem), but ok.
            call update_ghosts(ITYPE_Sall, NUMCONST, Ndx, constituents, ierr)
         end if

      else
      ! 3D
         if (jasal > 0) then
            call update_ghosts(ITYPE_Sall3D, 1, Ndkx, sa1, ierr)
         endif
         !if (jatem > 0) then
         !   call update_ghosts(ITYPE_Sall3D, 1, Ndkx, tem1, ierr)
         !endif
         !if (ITRA1 > 0 .or. jatem > 0) then
         if (ITRA1 > 0 .or. jatem > 0 .or. ISED1 > 0) then
            ! NOTE: This update sends too much (sa1/tem), but ok.
            ! Sed concentrations automagically included
            call update_ghosts(ITYPE_Sall3D, NUMCONST, Ndkx, constituents, ierr)
         end if
      end if
      !
      ! Sediment
      !call update_ghosts()

      if ( jatimer.eq.1 ) call stoptimer(IUPDSALL)

      !-- U/U3D --
      if ( jatimer.eq.1 ) call starttimer(IUPDU)
      if (kmx == 0) then
      ! 2D
         call update_ghosts(ITYPE_U, 1, Lnx, u1, ierr)
         call update_ghosts(ITYPE_U, 1, Lnx, u0, ierr)
         call update_ghosts(ITYPE_U, 1, Lnx, q1, ierr)
      else
      ! 3D
         call update_ghosts(ITYPE_U3D, 1, Lnkx, u1, ierr)
         call update_ghosts(ITYPE_U3D, 1, Lnkx, u0, ierr)
         call update_ghosts(ITYPE_U3D, 1, Lnkx, q1, ierr)
         call update_ghosts(ITYPE_U3D, 1, Lnkx, qa, ierr)
      end if

      if ( jatimer.eq.1 ) call stoptimer(IUPDU)

      if (ierr /= 0) then
         ierr = DFM_MODELNOTINITIALIZED
         goto 999
      end if

   endif ! jampi .and. jamapmerged

    call readyy('Reading map data',1.00d0)    

    ierr = DFM_NOERR
    ! Close the netcdf-file _map.nc
999 continue    
    itmp = unc_close(imapfile)
    call readyy('Reading map data',-1d0)
    if(allocated(maptimes)) deallocate(maptimes)
    if(allocated(max_threttim)) deallocate(max_threttim)
    if(allocated(id_ttrabnd)) deallocate(id_ttrabnd)
    if(allocated(id_ztrabnd)) deallocate(id_ztrabnd)
    if(allocated(id_tsedfracbnd)) deallocate(id_tsedfracbnd)
    if(allocated(id_zsedfracbnd)) deallocate(id_zsedfracbnd)
    if(allocated(tmpvar)) deallocate(tmpvar)
    if(allocated(tmpvar1)) deallocate(tmpvar1)
    if(allocated(tmpvar2)) deallocate(tmpvar2)
    
end subroutine unc_read_map


!> Writes the unstructured flow geometry to a netCDF file.
!! If file exists, it will be overwritten.
subroutine unc_write_flowgeom(filename)
    character(len=*), intent(in) :: filename

    integer :: igeomfile, ierr

    ierr = unc_create(filename, 0, igeomfile)
    if (ierr /= nf90_noerr) then
        call mess(LEVEL_ERROR, 'Could not create flow geometry file '''//trim(filename)//'''.')
        call check_error(ierr)
        return
    end if

    call unc_write_flowgeom_filepointer(igeomfile) ! UNC_CONV_CFOLD

    ierr = unc_close(igeomfile)
end subroutine unc_write_flowgeom


!> Writes the unstructured network and flow geometry to a netCDF file.
!! If file exists, it will be overwritten.
subroutine unc_write_net_flowgeom(filename)
    character(len=*), intent(in) :: filename

    integer :: igeomfile, ierr

    ierr = unc_create(filename, 0, igeomfile)
    if (ierr /= nf90_noerr) then
        call mess(LEVEL_ERROR, 'Could not create flow geometry file '''//trim(filename)//'''.')
        call check_error(ierr)
        return
    end if

    call unc_write_net_filepointer(igeomfile)      ! Write standard net data as well
    call unc_write_flowgeom_filepointer(igeomfile) ! UNC_CONV_CFOLD

    ierr = unc_close(igeomfile)
end subroutine unc_write_net_flowgeom

!> Writes the unstructured network and flow geometry to a netCDF file.
!! If file exists, it will be overwritten.
! TODO: consider removing/replacing by flowgeom_ugrid, IF network data writing is not necessary anymore as a separate call. AvD
subroutine unc_write_net_flowgeom_ugrid(filename)
    implicit none

    character(len=*), intent(in) :: filename

    integer :: ierr
    type(t_unc_mapids) :: geomids

    ierr = unc_create(filename, 0, geomids%ncid)
    if (ierr /= nf90_noerr) then
        call mess(LEVEL_ERROR, 'Could not create flow geometry file '''//trim(filename)//'''.')
        call check_error(ierr)
        return
    end if

    call unc_write_flowgeom_filepointer_ugrid(geomids%ncid, geomids%id_tsp) ! UNC_CONV_UGRID

    ierr = unc_close(geomids%ncid)
end subroutine unc_write_net_flowgeom_ugrid

!> Fills the given arrays for all edges in the 2D mesh, ordered as follows: first internal flow links, then boundary flow links, then closed net links.
subroutine get_2d_edge_data(edge_nodes, edge_faces, edge_type, xue, yue, edge_mapping_table, reverse_edge_mapping_table)
   use network_data
   use m_flowgeom

   implicit none

   integer, intent(out)                          :: edge_nodes(:,:) !< Edge node connectivity array to be filled.
   integer, pointer, intent(in)                  :: edge_faces(:,:) !< Edge face type to be filled
   integer, intent(out)                          :: edge_type(:)    !< Edge type array to be filled.
   real(kind=dp), intent(out)                    :: xue(:)          !< Edge x coordinate array to be filled.
   real(kind=dp), intent(out)                    :: yue(:)          !< Edge y coordinate array to be filled.
   integer, optional, intent(out)                :: edge_mapping_table(:) !< Mapping from original edges to ordered edges (first flow links, then closed edges). To be filled if present.
   integer, optional, intent(out)                :: reverse_edge_mapping_table(:) !< Mapping from ordered edges (first flow links, then closed edges) to original edges. To be filled if present.

   integer :: i, L, Lf !< Counters.

   ! Write all edges that are 2D internal flow links.
   i = 0
   ! Lf is flow link number.
   do Lf = lnx1d+1,lnxi
      ! i is edge number.
      i = i + 1

      edge_nodes(1:2, i) = lncn(1:2, Lf)
      if (associated(edge_faces)) edge_faces(1:2,i) = ln(1:2, Lf)
      
      edge_type(i) = UG_EDGETYPE_INTERNAL
      xue(i) = xu(Lf)
      yue(i) = yu(Lf)

      ! L is net link number.
      L = ln2lne(Lf)
      if (present(edge_mapping_table)) edge_mapping_table(L) = i
      if (present(reverse_edge_mapping_table)) reverse_edge_mapping_table(i) = L
   end do

   ! Write all edges that are 2D boundary flow links.
   ! Lf is flow link number.
   do Lf = lnx1Db+1,lnx
      ! i is edge number.
      i = i + 1
      
      edge_nodes(1:2, i) = lncn(1:2, Lf)
      if (associated(edge_faces)) then
         edge_faces(1:2,i) = (/ 0, ln(2, Lf) /)
      endif
       
      edge_type(i) = UG_EDGETYPE_BND
      xue(i) = xu(Lf)
      yue(i) = yu(Lf)

      ! L is net link number.
      L = ln2lne(Lf)
      if (present(edge_mapping_table)) edge_mapping_table(L) = i
      if (present(reverse_edge_mapping_table)) reverse_edge_mapping_table(i) = L
   end do

   ! Write all remaining edges, which are closed.
   ! Loop over all 2D net links, which includes both 2D flow links and closed 2D net links.
   ! L is net link number.
   if (allocated(lne2ln)) then
      do L = NUML1D+1,NUML
         ! Lf is flow link number.
         Lf =  lne2ln(L)
         if (Lf <= 0) then ! If this net link does not have a flow link (i.e. closed net link).
            ! i is edge number.
            i = i + 1

            edge_nodes(1:2, i) = KN(1:2, L)
            if (lnn(L) < 2) then
               edge_type(i) = UG_EDGETYPE_BND_CLOSED
            else if (kn(3,L) == 0) then
               edge_type(i) = UG_EDGETYPE_INTERNAL_CLOSED
            end if
            
            if (associated(edge_faces)) edge_faces(1:2, i) = lne(1:2, L)
            ! Edge coordinate is in the middle of the net link.
            xue(i) = .5d0*(xk(kn(1,L)) + xk(kn(2,L)))
            yue(i) = .5d0*(yk(kn(1,L)) + yk(kn(2,L)))

            if (present(edge_mapping_table)) edge_mapping_table(L) = i
            if (present(reverse_edge_mapping_table)) reverse_edge_mapping_table(i) = L
         end if
      end do
   end if
end subroutine get_2d_edge_data

!> Sets layer info in the given variables. Only call this if layers present.
subroutine get_layer_data_ugrid(layer_count, layer_type, layer_zs, interface_zs)
   use m_alloc
   use m_missing
   use m_flow, only: laytyp, LAYTP_SIGMA, LAYTP_Z, zslay
   use io_ugrid, only: LAYERTYPE_OCEANSIGMA, LAYERTYPE_Z

   implicit none

   integer,                     intent(in)  :: layer_count  !< Number of layers.
   integer,                     intent(out) :: layer_type   !< UGRID layer type (sigma or z) to be determined.
   real(kind=dp), dimension(:), intent(out) :: layer_zs     !< Vertical layer center coordinates array to be filled.
   real(kind=dp), dimension(:), intent(out) :: interface_zs !< Vertical layer interface coordinates array to be filled.

   character(len=255) :: message !< Temporary variable for writing log messages.

   ! Create rank 1 array with vertical layer coordinates (not per flow node).
   select case(laytyp(1))
   case (LAYTP_SIGMA)
      ! Transform from dfm sigma (positive upwards, bedlevel=0, eta=1) to ocean sigma (positive upwards, bedlevel=-1, eta=0) coordinates.
      interface_zs(1:layer_count + 1) = zslay(0:layer_count, 1) - 1d0
      layer_type = LAYERTYPE_OCEANSIGMA
   case (LAYTP_Z)
      ! Fixed z coordinates.
      interface_zs(1:layer_count + 1) = zslay(0:layer_count, 1)
      layer_type = LAYERTYPE_Z
   case default
      write(message, *) 'Unsupported layer type: ', laytyp(1), '. Layer coordinate variables will not be written.'
      call mess(LEVEL_WARN, trim(message))
      layer_type = -1
      return
   end select

   ! Layer center coordinates.
   layer_zs(1:layer_count) = .5d0*(interface_zs(1:layer_count) + interface_zs(2:layer_count + 1))
end subroutine get_layer_data_ugrid

!> Writes the unstructured flow geometry in UGRID format to an already opened netCDF dataset.
subroutine unc_write_flowgeom_filepointer_ugrid(ncid,id_tsp, jabndnd)
   
   use m_flowgeom
   use network_data
   use m_sferic
   use m_missing
   use netcdf
   use m_partitioninfo
   use m_flow, only: kmx, mxlaydefs, laymx
   use m_alloc
   use dfm_error
   use m_save_ugrid_state !stores the contactname and other saved ugrid names
   use m_CrossSections 
   use unstruc_channel_flow, only: network
   use m_flowparameters, only: jamd1dfile
   use m_oned_functions, only: gridpoint2cross

   implicit none

   integer, intent(in)                     :: ncid
   type(t_unc_timespace_id), intent(inout) :: id_tsp   !< Set of time and space related variable id's
   integer, optional, intent(in) :: jabndnd !< Whether to include boundary nodes (1) or not (0). Default: no.

   integer                       :: jabndnd_

   integer :: nn
   integer, allocatable :: edge_nodes(:,:), face_nodes(:,:), edge_type(:), contacts(:,:) 
   integer, dimension(:,:), pointer :: edge_faces => null()
   integer :: layer_count, layer_type
   real(kind=dp), dimension(:), pointer :: layer_zs => null(), interface_zs => null()
!   type(t_crs) :: pj

   integer :: ierr
   integer :: i, numContPts, numNodes, n, ndxndxi, ndx1d, numl2d, L
   integer :: id_flowelemcontourptsdim, id_flowelemcontourx, id_flowelemcontoury
   integer :: jaInDefine

   double precision :: xx, yy
   double precision, dimension(:), allocatable :: zz
   double precision, allocatable :: work2(:,:)

   integer                       :: n1dedges, n1d2dcontacts, numk2d, start_index
   integer,allocatable           :: contacttype(:) 
   
   type(t_CSType), pointer                       :: pCS
   type(t_CSType), pointer, dimension(:)         :: pCSs
   integer                                       :: j, jmax
   double precision, dimension(:,:), allocatable :: work1d_z, work1d_n

   ! re-mapping of 1d mesh coordinates for UGrid
   double precision, allocatable                 :: x1dn(:), y1dn(:), xue(:), yue(:)
   ! re-mapping of 2d mesh coordinates for UGrid
   double precision, allocatable                 :: x2dn(:), y2dn(:), z2dn(:)
   integer                                       :: netNodeReMappedIndex, nnSize

   jaInDefine    = 0
   n1d2dcontacts = 0
   n1dedges      = 0
   start_index   = 1
   
   if (ndxi <= 0) then
      call mess(LEVEL_WARN, 'No flow elements in model, will not write flow geometry.')
      return
   end if

   if (present(jabndnd)) then
      jabndnd_ = jabndnd
   else
      jabndnd_ = 0
   endif

   ! Include boundary cells in output (ndx) or not (ndxi)
   if (jabndnd_ == 1) then
      ndxndxi   = ndx
   else
      ndxndxi   = ndxi
   end if

   ! Put dataset in define mode (possibly again) to add dimensions and variables.
   ierr = nf90_redef(ncid)
   if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
   if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
      call mess(LEVEL_ERROR, 'Could not put header in flow geometry file.')
      call check_error(ierr)
      return
   end if

   if (jsferic == 1) then
      crs%epsg_code = 4326
   end if

   ! Get layer info.
   if (kmx <= 0) then ! If no layers present.
      layer_count = 0
      layer_type = -1
      ! Leave layer_zs and interface_zs unallocated, since they will not be used in this case.
   else ! If layers present.
      if (mxlaydefs > 1) then
         call mess(LEVEL_WARN, 'Multiple layer definitions cannot be handled for layer variables. Layer variables will not be written.')
         ierr = DFM_NOTIMPLEMENTED
         goto 888
      else
         layer_count = laymx(1)
         call reallocP(layer_zs, layer_count, fill=dmiss, keepExisting=.false.)
         call reallocP(interface_zs, layer_count + 1, fill=dmiss, keepExisting=.false.)
         call get_layer_data_ugrid(layer_count, layer_type, layer_zs, interface_zs)
      end if
   end if

   ! Temporary UGRID fix
   !if(numMesh1dBeforeMerging>0) then
   !   ndx1d=numMesh1dBeforeMerging
   !else
      ! 1D flow grid geometry + 1D2D connections
      ndx1d = ndxi - ndx2d
   !endif
   
   n1d2dcontacts = 0
   if (ndx1d > 0) then
      
      ! First store pure 1D nodes (in flow node order), start counting at 1.call realloc(x1dn, ndx1d)
      call realloc(x1dn, ndx1d)
      call realloc(y1dn, ndx1d)
      ! Temporary UGRID fix
      !if(numMesh1dBeforeMerging>0) then
      !   do n =1,ndx1d
      !      !x1dn(n)=xz(ndx2d+mesh1dUnmergedToMerged(n))
      !      !y1dn(n)=xz(ndx2d+mesh1dUnmergedToMerged(n))
      !      x1dn(n)=xz(ndx2d+n)
      !      y1dn(n)=xz(ndx2d+n)
      !   enddo  
      !else
         do n=1,ndx1d
            x1dn(n) = xz(ndx2d+n)
            y1dn(n) = yz(ndx2d+n)
         enddo
      !endif
      
      !count 1d mesh edges and 1d2d contacts 
      n1dedges = 0 
      n1d2dcontacts = 0
      do L=1,lnx1d
         if (kcu(L) == 1) then
            n1dedges = n1dedges + 1
         else if (kcu(L) == 3 .or. kcu(L) == 4 .or. kcu(L) == 5 .or. kcu(L) == 7) then  ! 1d2d, lateralLinks, streetinlet, roofgutterpipe
            n1d2dcontacts = n1d2dcontacts + 1
         else
            continue
         endif
      enddo
      
      !allocate mesh edges and 1d2d contacts  
      call realloc(edge_nodes, (/ 2, n1dedges /), fill = -999) 
      call realloc(contacts, (/ 2, n1d2dcontacts /), fill = -999) 
      call realloc(id_tsp%edgetoln, n1dedges, keepExisting = .false., fill = 0)
      call realloc(id_tsp%contactstoln, n1d2dcontacts, keepExisting = .false., fill = 0)
      call realloc(contacttype, n1d2dcontacts, keepExisting = .false., fill = 0)
      
      !assign values to mesh edges 1d2d contacts 
      n1dedges = 0 
      n1d2dcontacts = 0
      do L=1,lnx1d
         if (kcu(L) == 1) then
            n1dedges = n1dedges + 1
            !if (allocated(mesh1dMergedToUnMerged)) then 
            !    edge_nodes(1:2,n1dedges) = mesh1dMergedToUnMerged(ln(1:2,L) - ndx2d) !only 1d edge nodes
            !else
                edge_nodes(1:2,n1dedges) = ln(1:2,L) - ndx2d !only 1d edge nodes
            !endif
            !mappings
            id_tsp%edgetoln(n1dedges) = L
         else if (kcu(L) == 3 .or. kcu(L) == 4 .or. kcu(L) == 5 .or. kcu(L) == 7) then  ! 1d2d, lateralLinks, streetinlet, roofgutterpipe
            ! 1D2D link, find the 2D flow node and store its cell center as '1D' node coordinates
            n1d2dcontacts = n1d2dcontacts + 1
            id_tsp%contactstoln(n1d2dcontacts) = L
            contacttype(n1d2dcontacts) = kcu(L)
            if (ln(1,L) > ndx2d) then  ! First point of 1D link is 1D cell
               !if (allocated(mesh1dMergedToUnMerged)) then ! mesh1dMergedToUnMerged is allocated only for UGrid file format (where merging might be necessary)
               !   contacts(1,n1d2dcontacts) = mesh1dMergedToUnMerged(ln(1,L) - ndx2d)
               !else
                  contacts(1,n1d2dcontacts) = ln(1,L) - ndx2d
               !endif
               contacts(2,n1d2dcontacts) = ln(2,L)   ! In m_flowgeom: 1D nodenr = ndx2d+n, in UGrid 1D flowgeom: local 1D nodenr = n.
            else                       ! Second point of 1D link is 1D cell
               !if (allocated(mesh1dMergedToUnMerged)) then 
               !   contacts(1,n1d2dcontacts) = mesh1dMergedToUnMerged(ln(2,L) - ndx2d)
               !else
                  contacts(1,n1d2dcontacts) = ln(2,L) - ndx2d
               !endif
               contacts(2,n1d2dcontacts) = ln(1,L)         !2d
            end if
         else
            continue
         endif
      enddo
      
      !define 1dmesh      
      if (n1dedges.gt.0) then
         if (associated(meshgeom1d%ngeopointx)) then    
         ierr = ug_write_mesh_arrays(ncid, id_tsp%meshids1d, mesh1dname, 1, UG_LOC_NODE + UG_LOC_EDGE, ndx1d, n1dedges, 0, 0, &
                                       edge_nodes, face_nodes, null(), null(), null(), x1dn, y1dn, xu(id_tsp%edgetoln(:)), yu(id_tsp%edgetoln(:)), xz(1:1), yz(1:1), &
                                       crs, -999, dmiss, start_index, layer_count, layer_type, layer_zs, interface_zs, &
                                       id_tsp%network1d, network1dname, meshgeom1d%nnodex, meshgeom1d%nnodey, nnodeids, nnodelongnames, &
                                       meshgeom1d%nedge_nodes(1,:), meshgeom1d%nedge_nodes(2,:), nbranchids, nbranchlongnames, meshgeom1d%nbranchlengths, meshgeom1d%nbranchgeometrynodes, meshgeom1d%nbranches, & 
                                       meshgeom1d%ngeopointx, meshgeom1d%ngeopointy, meshgeom1d%ngeometry, &
                                       meshgeom1d%nbranchorder, &
                                       nodeids, nodelongnames, meshgeom1d%nodebranchidx, meshgeom1d%nodeoffsets, meshgeom1d%edgebranchidx, meshgeom1d%edgeoffsets,&
                                       writeopts=unc_writeopts)
         else
         ierr = ug_write_mesh_arrays(ncid, id_tsp%meshids1d, mesh1dname, 1, UG_LOC_NODE + UG_LOC_EDGE, ndx1d, n1dedges, 0, 0, &
                                     edge_nodes, face_nodes, null(), null(), null(), x1dn, y1dn, xu(id_tsp%edgetoln(:)), yu(id_tsp%edgetoln(:)), xz(1:1), yz(1:1), &
                                     crs, -999, dmiss, start_index, layer_count, layer_type, layer_zs, interface_zs, writeopts=unc_writeopts)
         endif         
      endif

      ! Determine max nr of vertices and contour points
      ndx1d = ndxi - ndx2d
      numNodes   = ndx1d
      numContPts = 0
      do i=1,ndx1d
         numNodes   = max(numNodes,   size(nd(ndx2d + i)%nod))
         numContPts = max(numContPts, size(nd(ndx2d + i)%x))
      end do
    
      if( allocated(work2) ) deallocate( work2 )
      allocate( work2(numContPts,ndx1d) ) ; work2 = dmiss

      ierr = nf90_def_dim(ncid, 'n'//trim(mesh1dname)//'_FlowElemContourPts', numContPts,    id_flowelemcontourptsdim)

      ! Flow elem contours (plot help)
      ! Todo: generalize x/y's to 2/3-D coords everywhere else [Avd]
      ierr = nf90_def_var(ncid, trim(mesh1dname)//'_FlowElemContour_x', nf90_double, (/ id_flowelemcontourptsdim, id_tsp%meshids1d%dimids(mdim_node) /), id_flowelemcontourx)
      ierr = nf90_def_var(ncid, trim(mesh1dname)//'_FlowElemContour_y', nf90_double, (/ id_flowelemcontourptsdim, id_tsp%meshids1d%dimids(mdim_node) /), id_flowelemcontoury)
      ierr = unc_addcoordatts(ncid, id_flowelemcontourx, id_flowelemcontoury, jsferic)
      ierr = nf90_put_att(ncid, id_flowelemcontourx, 'long_name',     'list of x-coordinates forming flow element')
      ierr = nf90_put_att(ncid, id_flowelemcontoury, 'long_name',     'list of y-coordinates forming flow element')
      ierr = nf90_put_att(ncid, id_flowelemcontourx, '_FillValue', dmiss)
      ierr = nf90_put_att(ncid, id_flowelemcontoury, '_FillValue', dmiss)

      ierr = nf90_put_att(ncid, id_tsp%meshids1d%varids(mid_nodex), 'bounds', trim(mesh1dname)//'_FlowElemContour_x')
      ierr = nf90_put_att(ncid, id_tsp%meshids1d%varids(mid_nodey), 'bounds', trim(mesh1dname)//'_FlowElemContour_y')

      ierr = nf90_enddef(ncid)

      do i=1,ndx1d
         nn = size(nd(ndx2d + i)%x)
         do n = 1,nn
            work2(n,i)=nd(ndx2d + i)%x(n)
         enddo
      enddo
      ierr = nf90_put_var(ncid, id_flowelemcontourx, work2(1:numContPts,1:ndx1d), (/ 1, 1 /), (/ numContPts, ndx1d /) )
    
      do i=1,ndx1d
         nn = size(nd(ndx2d + i)%x)
         do n = 1,nn
            work2(n,i)=nd(ndx2d + i)%y(n)
         enddo
      enddo
      ierr = nf90_put_var(ncid, id_flowelemcontoury, work2(1:numContPts,1:ndx1d), (/ 1, 1 /), (/ numContPts, ndx1d /) )
      ierr = nf90_redef(ncid)

      deallocate( work2 )
      !
      if (ndx1d > 0 .and. stm_included) then
         if( stmpar%morpar%bedupd ) then
            pCSs => network%CSDefinitions%CS
            j = 1
            do i = 1,size(pCSs)
               j = max(j,pCSs(i)%levelscount)
            enddo
            jmax = j
            ierr = nf90_def_dim(ncid, 'n'//trim(mesh1dname)//'_crs_maxdim', jmax,    id_tsp%id_jmax)
            ierr = nf90_def_var(ncid, 'flowelem_crs_z', nf90_double, (/ id_tsp%id_jmax, id_tsp%meshids1d%dimids(mdim_node) /), id_tsp%id_flowelemcrsz(1))
            if (jased > 0) then
                ierr = nf90_put_att(ncid, id_tsp%id_flowelemcrsz(1), 'long_name','initial cross-section points level')
            else
                ierr = nf90_put_att(ncid, id_tsp%id_flowelemcrsz(1), 'long_name','cross-section points level')
            endif
            ierr = nf90_put_att(ncid, id_tsp%id_flowelemcrsz(1), 'unit', 'm')
            ierr = nf90_def_var(ncid, 'flowelem_crs_n', nf90_double, (/ id_tsp%id_jmax, id_tsp%meshids1d%dimids(mdim_node) /), id_tsp%id_flowelemcrsn(1))
            if (jased > 0) then
                ierr = nf90_put_att(ncid, id_tsp%id_flowelemcrsn(1), 'long_name','initial cross-section points half width')
            else
                ierr = nf90_put_att(ncid, id_tsp%id_flowelemcrsn(1), 'long_name','cross-section points half width')
            endif
            ierr = nf90_put_att(ncid, id_tsp%id_flowelemcrsn(1), 'unit', 'm')
         endif
      endif

      deallocate(x1dn)
      deallocate(y1dn)
      deallocate(edge_nodes)
   end if ! 1D flow grid geometry

   numk2d = 0
   ndx1d = ndxi - ndx2d
   if (ndx2d > 0) then ! 2D flow geometry
      numl2d = numl-numl1d
      numk2d = (numk-n1d2dcontacts) - ndx1d
      call realloc(edge_nodes, (/ 2, numl2d /), fill = -999 , keepExisting = .false.)
      call reallocP(edge_faces, (/ 2, numl2d /), fill = -999) 
      call realloc(edge_type, numl2d, fill = -999, keepExisting = .false.)
      call realloc(xue, numl2d, fill = dmiss, keepExisting = .false.)
      call realloc(yue, numl2d, fill = dmiss, keepExisting = .false.)
      call realloc(x2dn,numk2d, fill = dmiss, keepExisting = .false.)
      call realloc(y2dn,numk2d, fill = dmiss, keepExisting = .false.)
      call realloc(z2dn,numk2d, fill = dmiss, keepExisting = .false.)
      call get_2d_edge_data(edge_nodes, edge_faces, edge_type, xue, yue)

      ! Determine max nr of vertices and contour points
      numNodes   = 0
      numContPts = 0 ! TODO: AvD: contour points equals nodes here, remove, OR move to 1D
      do i=1,ndxndxi
         numNodes   = max(numNodes,   size(nd(i)%nod))
         numContPts = max(numContPts, size(nd(i)%x))
      end do

      ! Note: AvD: for cell corners, we write *all* net nodes (numk). This may also be '1D' nodes, but that is not problematic: they will simply not be referenced in face_nodes/edge_nodes.
      ! Note: AvD: numk may be larger than nr of cell corners. Will cause problems when writing output data on corners (mismatch in dimensions), not crucial now.
      call realloc(face_nodes, (/ numNodes, ndx2d /), fill = -999)
      
      ! re-mapping by edge nodes is needed, use kc as table
      kc = 0
      netNodeReMappedIndex = 0
      do l=1,numl2d
         nn = edge_nodes(1,l)
         if (nn > 0) then
            if ( kc(nn)==0 ) then
               netNodeReMappedIndex = netNodeReMappedIndex + 1
               x2dn(netNodeReMappedIndex) = xk(nn)
               y2dn(netNodeReMappedIndex) = yk(nn)
               z2dn(netNodeReMappedIndex) = zk(nn)
               kc(nn)=netNodeReMappedIndex
            endif
         endif
         nn = edge_nodes(2,l)
         if (nn > 0) then
            if ( kc(nn)==0 ) then
               netNodeReMappedIndex = netNodeReMappedIndex + 1
               x2dn(netNodeReMappedIndex) = xk(nn)
               y2dn(netNodeReMappedIndex) = yk(nn)
               z2dn(netNodeReMappedIndex) = zk(nn)
               kc(nn)=netNodeReMappedIndex
            endif
         endif
      enddo

      !remapped edge_nodes
      do l=1,numl2d
         edge_nodes(1,l) = kc(edge_nodes(1,l))
         edge_nodes(2,l) = kc(edge_nodes(2,l))
      enddo

      !remapped face_nodes
      do n=1,ndx2d
         nnSize  = size(nd(n)%nod)
         do i=1,nnSize
            nn = nd(n)%nod(i)
            if(nn>0) then
               face_nodes(i,n) = kc(nn)
            endif
         enddo
      enddo
      ! face_nodes does not need to be re-mapped: 2d cells come first
      ! TODO: AvD: lnx1d+1:lnx includes open bnd links, which may *also* be 1D boundaries (don't want that in mesh2d)
      ! note edge_faces does not need re-indexing, cell number are flow variables and 2d comes first
      ierr = ug_write_mesh_arrays(ncid, id_tsp%meshids2d, mesh2dname, 2, UG_LOC_EDGE + UG_LOC_FACE, numk2d, numl2d, ndx2d, numNodes, &
                                    edge_nodes, face_nodes, edge_faces, null(), null(),x2dn, y2dn, xue, yue, xz(1:ndx2d), yz(1:ndx2d), &
                                    crs, -999, dmiss, start_index, layer_count, layer_type, layer_zs, interface_zs, writeopts=unc_writeopts)

      ! Add edge type variable (edge-flowlink relation)
      call write_edge_type_variable(ncid, id_tsp%meshids2d, mesh2dname, edge_type)

      deallocate(edge_nodes)
      deallocate(face_nodes)
      deallocate(edge_faces)
      deallocate(x2dn)
      deallocate(y2dn)
   end if

   ! NOTE: UNST-1318: backwards compatibility: we write zk values in flowgeom/map file since DELFT3DFM still needs it.
   !       The def_var is inside io_ugrid (needs to be removed), but the put_var is only here.
   ! TODO: below would be better than def_var inside io_ugrid:
   ! TODO: ierr = unc_def_var_map(mapids, mapids%id_netnodez(:),   nf90_double, UNC_LOC_CN, 'node_z', '', 'Bed level at grid nodes', 'm', 0)
   ! ierr = ug_inq_varid(mapids%ncid, mapids%id_tsp%meshids1d, 'node_z', mapids%id_netnodez(1)) ! TODO: AvD: 1D UGRID not entirely yet.
   ierr = ug_inq_varid(ncid, id_tsp%meshids2d, 'node_z', id_tsp%id_netnodez(2))
   ! ierr = ug_inq_varid(mapids%ncid, mapids%id_tsp%meshids3d, 'node_z', mapids%id_netnodez(3)) ! TODO: AvD: 3D UGRID not yet
   
!   ierr = unc_def_var_map(mapids, mapids%id_flowelemcontourx(:), nf90_double, UNC_LOC_S, 'FlowElemContour_x', '', '', 'm', (/ id_flowelemcontourptsdim, id_seddim, -1 /).

   ierr = unc_def_var_map(ncid, id_tsp, id_tsp%id_flowelemba(:), nf90_double, UNC_LOC_S, 'flowelem_ba', 'cell_area', '', 'm2', 0)
   ierr = unc_def_var_map(ncid, id_tsp, id_tsp%id_flowelembl(:), nf90_double, UNC_LOC_S, 'flowelem_bl', 'altitude', 'flow element center bedlevel (bl)', 'm', 0)
   ! ierr = nf90_put_att(igeomfile, id_flowelembl, 'positive',      'up') ! Not allowed for non-coordinate variables


   !define 1d2dcontacts only after mesh2d is completly defined  
   if (n1d2dcontacts.gt.0) then
      ierr = ug_def_mesh_contact(ncid, id_tsp%meshcontacts, trim(contactname), n1d2dcontacts, id_tsp%meshids2d, id_tsp%meshids1d, UG_LOC_NODE, UG_LOC_FACE, start_index)
   endif

   ! Define domain numbers when it is a parallel run
   if (jampi .eq. 1) then
      ierr = unc_def_var_map(ncid, id_tsp, id_tsp%id_flowelemdomain(:), nf90_int, UNC_LOC_S, 'flowelem_domain', 'cell_domain_number', 'domain number of flow element', '', 0)
      ierr = unc_def_var_map(ncid, id_tsp, id_tsp%id_flowelemglobalnr(:), nf90_int, UNC_LOC_S, 'flowelem_globalnr', 'cell_global_number', 'global flow element numbering', '', 0)
   endif
   ierr = nf90_enddef(ncid)

   ! -- Start data writing (time-independent data) ------------
   ! Flow cell cc coordinates (only 1D + internal 2D)
   if (ndx1d > 0) then
      ierr = nf90_put_var(ncid, id_tsp%id_flowelemba(1), ba(ndx2d+1:ndx2d+ndx1d)) ! TODO: AvD: handle 1D/2D boundaries
      ierr = nf90_put_var(ncid, id_tsp%id_flowelembl(1), bl(ndx2d+1:ndx2d+ndx1d)) ! TODO: AvD: handle 1D/2D boundaries
      ! TODO: AvD: UNST-1318: handle 1d zk as well
   end if
   if (ndx2d > 0) then
      ierr = nf90_put_var(ncid, id_tsp%id_flowelemba(2), ba(1:ndx2d)) ! TODO: AvD: handle 1D/2D boundaries
      ierr = nf90_put_var(ncid, id_tsp%id_flowelembl(2), bl(1:ndx2d)) ! TODO: AvD: handle 1D/2D boundaries
      ierr = nf90_put_var(ncid, id_tsp%id_netnodez(2),   zk(1:numk))  ! NOTE: UNST-1318: backwards compatibility, UNST-2207: remapped depth array must be used (ierr = nf90_put_var(ncid, id_tsp%id_netnodez(2), z2dn(1:numk))), 
                                                                      ! but is not part of ugrid coverage
   endif
   !
   if (ndx1d > 0 .and. stm_included) then
      if (stmpar%morpar%bedupd) then
         ndx1d = ndxi - ndx2d
         allocate( work1d_z(jmax,ndx1d), work1d_n(jmax,ndx1d) )
         work1d_z = dmiss
         work1d_n = dmiss
         do i = 1,ndx1d
            if (gridpoint2cross(i)%num_cross_sections==1) then
               n = gridpoint2cross(i)%cross(1)
               if (n==-999) cycle
               pCS => network%crs%cross(n)%tabdef
               if (pCS%crosstype == CS_TABULATED) then
                  do j = 1,pCS%levelscount
                     work1d_z(j,i) = pCS%height(j)
                     work1d_n(j,i) = pCS%flowWidth(j) * 0.5d0
                  enddo
               endif
            endif
         enddo
         ierr = nf90_put_var(ncid, id_tsp%id_flowelemcrsz(1), work1d_z(1:jmax,1:ndx1d), start=(/ 1, 1 /), count=(/ jmax, ndx1d /) )
         ierr = nf90_put_var(ncid, id_tsp%id_flowelemcrsn(1), work1d_n(1:jmax,1:ndx1d), start=(/ 1, 1 /), count=(/ jmax, ndx1d /) )
         deallocate( work1d_z, work1d_n )
      endif
   endif

   ! Put the contacts
   if (n1d2dcontacts.gt.0) then
      ierr = ug_put_mesh_contact(ncid, id_tsp%meshcontacts, contacts(1,:), contacts(2,:), contacttype) 
   endif
      
   if (allocated(edge_type)) deallocate(edge_type)
   ! TODO: AvD: also edge_type for 1D
   if (associated(layer_zs)) deallocate(layer_zs)
   if (associated(interface_zs)) deallocate(interface_zs)
   if (allocated(contacts)) deallocate(contacts)
   if (allocated(contacttype)) deallocate(contacttype)
   if (allocated(edge_nodes)) deallocate(edge_nodes)

   ! TODO: AvD:
   ! * in WAVE: handle the obsolete 'nFlowElemWithBnd'/'nFlowElem' difference
   ! * for WAVE: add FlowElem_zcc back in com file.
   ! * for parallel: add 'FlowElemDomain', 'FlowLinkDomain', 'FlowElemGlobalNr'
   ! domain numbers
   if ( jampi.eq.1 ) then  
      ! FlowElemDomain
      if (ndx2d > 0) then
         ierr = nf90_put_var(ncid, id_tsp%id_flowelemdomain(2), idomain(1:ndx2d))
      endif
      ! FlowElemGlobalNr
      if (ndx2d > 0) then
         ierr = nf90_put_var(ncid, id_tsp%id_flowelemglobalnr(2), iglobal_s(1:ndx2d))
      endif
   end if
   ! Leave the dataset in the same mode as we got it.
   if (jaInDefine == 1) then
      ierr = nf90_redef(ncid)
   end if

   !call readyy('Writing flow geometry data',-1d0)
   return

888 continue
   ! Possible error.

end subroutine unc_write_flowgeom_filepointer_ugrid


!> Writes the unstructured flow geometry to an already opened netCDF dataset.
subroutine unc_write_flowgeom_filepointer(igeomfile, jabndnd)
    use m_flowgeom
    use network_data
    use m_sferic
    use m_missing
    use netcdf
    use m_partitioninfo
    use m_flow, only: kmx
    integer, intent(in) :: igeomfile
    integer, optional, intent(in) :: jabndnd !< Whether to include boundary nodes (1) or not (0). Default: no.

    integer                       :: jabndnd_

    integer, allocatable :: kn3(:), ibndlink(:)

    integer :: ierr
    integer :: &
        id_laydim, id_netlinkdim, id_netlinkptsdim, &
        id_flowelemdim, id_flowelemmaxnodedim, id_flowelemcontourptsdim, &
        id_flowlinkdim, id_flowlinkptsdim, id_erolaydim, &
        id_flowelemxcc, id_flowelemycc, id_flowelemzcc, &
        id_flowelemxzw, id_flowelemyzw, &
        id_flowelemloncc, id_flowelemlatcc, &
        id_flowelemcontourx, id_flowelemcontoury, id_flowelemba, &
        id_flowelemcontourlon, id_flowelemcontourlat, &
        id_flowelembl, id_elemlink, &
        id_flowlink, id_flowlinktype, &
        id_flowlinkxu, id_flowlinkyu, &
        id_flowlinklonu, id_flowlinklatu, &
        id_flowelemdomain, id_flowlinkdomain, &
        id_flowelemglobalnr

    integer :: i, numContPts, numNodes, n, ndxndxi, nn, L
    integer :: jaInDefine
    integer :: jaghost, idmn
    integer, dimension(:), allocatable :: lne1write
    integer, dimension(:), allocatable :: lne2write

    double precision :: xx, yy
    double precision, dimension(:), allocatable :: zz
    double precision, dimension(:,:), allocatable :: work2

    jaInDefine = 0

    if (ndxi <= 0) then
        call mess(LEVEL_WARN, 'No flow elements in model, will not write flow geometry.')
        return
    end if

    if (present(jabndnd)) then
        jabndnd_ = jabndnd
    else
        jabndnd_ = 0
    endif

    ! Include boundary cells in output (ndx) or not (ndxi)
    if (jabndnd_ == 1) then
       ndxndxi   = ndx
    else
       ndxndxi   = ndxi
    end if

    ! Determine max nr of vertices and contour points
    numNodes   = 0
    numContPts = 0
    do i=1,ndxndxi
        numNodes   = max(numNodes,   size(nd(i)%nod))
        numContPts = max(numContPts, size(nd(i)%x))
    end do
    
    if( allocated(work2) ) deallocate( work2 )
    allocate( work2(numContPts,ndxndxi) ) ; work2 = dmiss

    ! Put dataset in define mode (possibly again) to add dimensions and variables.
    ierr = nf90_redef(igeomfile)
    if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
    if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
        call mess(LEVEL_ERROR, 'Could not put header in flow geometry file.')
        call check_error(ierr)
        return
    end if

    if (jabndnd_ == 1) then
       ierr = nf90_def_dim(igeomfile, 'nFlowElemWithBnd',    ndxndxi,       id_flowelemdim) ! Different name to easily show boundary nodes are included, rest of code below is generic ndx/ndxi.
    else
       ierr = nf90_def_dim(igeomfile, 'nFlowElem',           ndxndxi,       id_flowelemdim)
    end if

    ierr = nf90_inq_dimid(igeomfile, 'nNetLinkPts', id_netlinkptsdim)
    if(ierr.ne.0) then
       ierr = nf90_def_dim(igeomfile, 'nNetLinkPts',     2,      id_netlinkptsdim)
    endif

    ierr = nf90_inq_dimid(igeomfile, 'nNetLink', id_netlinkdim)
    if(ierr.ne.0) then
       ierr = nf90_def_dim(igeomfile, 'nNetLink',     numl,      id_netlinkdim)
    endif
   
    if (numNodes > 0) then
       ierr = nf90_def_dim(igeomfile, 'nFlowElemMaxNode',    numNodes,   id_flowelemmaxnodedim)
    end if

    ierr = nf90_def_dim(igeomfile, 'nFlowElemContourPts', numContPts,    id_flowelemcontourptsdim)

    if (lnx > 0) then
       ierr = nf90_def_dim(igeomfile, 'nFlowLink',           lnx ,       id_flowlinkdim)
       ierr = nf90_def_dim(igeomfile, 'nFlowLinkPts',        2,          id_flowlinkptsdim)
    end if

    ! Flow cells
    ierr = nf90_def_var(igeomfile, 'FlowElem_xcc', nf90_double, id_flowelemdim, id_flowelemxcc)
    ierr = nf90_def_var(igeomfile, 'FlowElem_ycc', nf90_double, id_flowelemdim, id_flowelemycc)
    ierr = nf90_def_var(igeomfile, 'FlowElem_zcc', nf90_double, id_flowelemdim, id_flowelemzcc)
    ierr = nf90_def_var(igeomfile, 'FlowElem_bac', nf90_double, id_flowelemdim, id_flowelemba)
    
    ierr = unc_addcoordatts(igeomfile, id_flowelemxcc, id_flowelemycc, jsferic)
    ierr = nf90_put_att(igeomfile, id_flowelemxcc, 'long_name'    , 'x-coordinate of flow element circumcenter')
    ierr = nf90_put_att(igeomfile, id_flowelemycc, 'long_name'    , 'y-coordinate of flow element circumcenter')
    ierr = nf90_put_att(igeomfile, id_flowelemzcc, 'standard_name', 'bed_level')
    ierr = nf90_put_att(igeomfile, id_flowelemzcc, 'long_name'    , 'bed level of flow element')
    !ierr = nf90_put_att(igeomfile, id_flowelemzcc, 'positive '    , 'down') ! For WAVE ! ONLY allowed for true coordinate-vars
    ierr = nf90_put_att(igeomfile, id_flowelemxcc, 'bounds'       , 'FlowElemContour_x')
    ierr = nf90_put_att(igeomfile, id_flowelemycc, 'bounds'       , 'FlowElemContour_y')

    ierr = nf90_put_att(igeomfile, id_flowelemba, 'long_name'    , 'flow element area')
    ierr = nf90_put_att(igeomfile, id_flowelemba, 'units',         'm2')
    ierr = nf90_put_att(igeomfile, id_flowelemba, 'standard_name', 'cell_area')

    ! Flow element mass centers
    ierr = nf90_def_var(igeomfile, 'FlowElem_xzw', nf90_double, id_flowelemdim, id_flowelemxzw)
    ierr = nf90_def_var(igeomfile, 'FlowElem_yzw', nf90_double, id_flowelemdim, id_flowelemyzw)
    ierr = unc_addcoordatts(igeomfile, id_flowelemxzw, id_flowelemyzw, jsferic)
    ierr = nf90_put_att(igeomfile, id_flowelemxzw, 'long_name'    , 'x-coordinate of flow element center of mass')
    ierr = nf90_put_att(igeomfile, id_flowelemyzw, 'long_name'    , 'y-coordinate of flow element center of mass')
    ierr = nf90_put_att(igeomfile, id_flowelemxzw, 'bounds'       , 'FlowElemContour_x')
    ierr = nf90_put_att(igeomfile, id_flowelemyzw, 'bounds'       , 'FlowElemContour_y')

    ! Flow elem contours (plot help)
    ! Todo: generalize x/y's to 2/3-D coords everywhere else [Avd]
    ierr = nf90_def_var(igeomfile, 'FlowElemContour_x', nf90_double, (/ id_flowelemcontourptsdim, id_flowelemdim /), id_flowelemcontourx)
    ierr = nf90_def_var(igeomfile, 'FlowElemContour_y', nf90_double, (/ id_flowelemcontourptsdim, id_flowelemdim /), id_flowelemcontoury)
    ierr = unc_addcoordatts(igeomfile, id_flowelemcontourx, id_flowelemcontoury, jsferic)
    ierr = nf90_put_att(igeomfile, id_flowelemcontourx, 'long_name',     'list of x-coordinates forming flow element')
    ierr = nf90_put_att(igeomfile, id_flowelemcontoury, 'long_name',     'list of y-coordinates forming flow element')
    ierr = nf90_put_att(igeomfile, id_flowelemcontourx, '_FillValue', dmiss)
    ierr = nf90_put_att(igeomfile, id_flowelemcontoury, '_FillValue', dmiss)

    ! Flow elems bottom levels
    ierr = nf90_def_var(igeomfile, 'FlowElem_bl', nf90_double, id_flowelemdim, id_flowelembl)
    ierr = nf90_put_att(igeomfile, id_flowelembl, 'units',         'm')
    !ierr = nf90_put_att(igeomfile, id_flowelembl, 'positive',      'up') ! Only allowed for true CF vertical coordinate
    !ierr = nf90_put_att(igeomfile, id_flowelembl, 'standard_name', 'sea_floor_depth') ! CF
    ierr = nf90_put_att(igeomfile, id_flowelembl, 'long_name',     'Initial bed level at flow element circumcenter')

    ierr = nf90_def_var(igeomfile, 'ElemLink', nf90_int, (/ id_netlinkptsdim, id_netlinkdim /) , id_elemlink)
    ierr = nf90_put_att(igeomfile, id_elemlink, 'standard_name', 'elemlink')
    ierr = nf90_put_att(igeomfile, id_elemlink, 'long_name',     'flow nodes between/next to which link between two netnodes lies')
    ierr = nf90_put_att(igeomfile, id_elemlink, 'start_index', 1)

    if (lnx > 0) then
       ierr = nf90_def_var(igeomfile, 'FlowLink',     nf90_int, (/ id_flowlinkptsdim, id_flowlinkdim /) ,   id_flowlink)
       ierr = nf90_put_att(igeomfile, id_flowlink    , 'long_name'    , 'link/interface between two flow elements')

       ierr = nf90_def_var(igeomfile, 'FlowLinkType', nf90_int, (/ id_flowlinkdim /) ,   id_flowlinktype)
       ierr = nf90_put_att(igeomfile, id_flowlinktype, 'long_name'    ,   'type of flowlink')
       ierr = nf90_put_att(igeomfile, id_flowlinktype, 'valid_range'  ,   (/ 1, 4 /))
       ierr = nf90_put_att(igeomfile, id_flowlinktype, 'flag_values'  ,   (/ 1, 2, 3, 4 /))
       ierr = nf90_put_att(igeomfile, id_flowlinktype, 'flag_meanings', 'link_between_1D_flow_elements link_between_2D_flow_elements embedded_1D2D_link 1D2D_link')

       ierr = nf90_def_var(igeomfile, 'FlowLink_xu',     nf90_double, (/ id_flowlinkdim /) ,   id_flowlinkxu)
       ierr = nf90_def_var(igeomfile, 'FlowLink_yu',     nf90_double, (/ id_flowlinkdim /) ,   id_flowlinkyu)
       ierr = unc_addcoordatts(igeomfile, id_flowlinkxu, id_flowlinkyu, jsferic)
       ierr = nf90_put_att(igeomfile, id_flowlinkxu, 'long_name'    , 'x-coordinate of flow link center (velocity point)')
       ierr = nf90_put_att(igeomfile, id_flowlinkyu, 'long_name'    , 'y-coordinate of flow link center (velocity point)')
    end if

    ! Coordinate/grid mapping
    ierr = unc_addcoordmapping(igeomfile, jsferic)

    ! Add mandatory lon/lat coords too (only if jsferic==0)
    ! BJ: following two lines commented out since QuickPlot will select longitude and latitude based on preference; however, these arrays don't actually contain data yet!
    !ierr = unc_add_lonlat_vars(igeomfile, 'FlowElem',        'cc', (/ id_flowelemdim /),                           id_flowelemloncc,      id_flowelemlatcc,      jsferic)
    !ierr = unc_add_lonlat_vars(igeomfile, 'FlowElemContour', ''  , (/ id_flowelemcontourptsdim, id_flowelemdim /), id_flowelemcontourlon, id_flowelemcontourlat, jsferic)

    ! Add grid_mapping reference to all original coordinate and data variables
    ierr = unc_add_gridmapping_att(igeomfile, &
       (/ id_flowelembl /), jsferic)
!       (/ id_flowelemxcc, id_flowelemycc, id_flowelemcontourx, id_flowelemcontoury, &

    if (lnx > 0) then
       ierr = unc_add_lonlat_vars(igeomfile, 'FlowLink',        'u' , (/ id_flowlinkdim /),                           id_flowlinklonu,       id_flowlinklatu,       jsferic)

       ! Add grid_mapping reference to all original coordinate and data variables
       !ierr = unc_add_gridmapping_att(igeomfile, &
       !   (/ id_flowlinkxu, id_flowlinkyu /), jsferic)
    end if


    !   domain numbers and global node/link numbers
    if ( jampi.eq.1 ) then
       ierr = nf90_def_var(igeomfile, 'FlowElemDomain', nf90_int, id_flowelemdim, id_flowelemdomain)
       ierr = nf90_put_att(igeomfile, id_flowelemdomain, 'long_name'    ,   'domain number of flow element')
       ierr = nf90_def_var(igeomfile, 'FlowLinkDomain', nf90_int, id_flowlinkdim, id_flowlinkdomain)
       ierr = nf90_put_att(igeomfile, id_flowlinkdomain, 'long_name'    ,   'domain number of flow link')
       ierr = nf90_def_var(igeomfile, 'FlowElemGlobalNr', nf90_int, id_flowelemdim, id_flowelemglobalnr)
       ierr = nf90_put_att(igeomfile, id_flowelemglobalnr, 'long_name'    ,   'global flow element numbering')
    end if

    ierr = nf90_enddef(igeomfile)
    ! End of writing time-independent flow net data.
    ! call readyy('Writing flow geometry data',.05d0)

    ! -- Start data writing (time-independent data) ------------
    ! Flow cell cc coordinates (only 1D + internal 2D)
    ierr = nf90_put_var(igeomfile, id_flowelemxcc, xz(1:ndxndxi))
    ierr = nf90_put_var(igeomfile, id_flowelemycc, yz(1:ndxndxi))
    ierr = nf90_put_var(igeomfile, id_flowelemba, ba(1:ndxndxi))     
    allocate (zz(ndxndxi), stat=ierr)
    !
    ! DFlowFM: z-positive is upwards
    ! WAVE: z-positive is downwards
    ! Don't change DMISS
    do n = 1,ndxndxi
       if (bl(n).eq.DMISS) then
          zz(n) =  bl(n)
       else
          zz(n) = -bl(n)
       end if
    end do
    ierr = nf90_put_var(igeomfile, id_flowelemzcc, zz(1:ndxndxi))
    if (allocated(zz)) deallocate (zz, stat=ierr)

    ! Flow cell center of mass coordinates (only 1D + internal 2D)
    ierr = nf90_put_var(igeomfile, id_flowelemxzw, xzw(1:ndxndxi))
    ierr = nf90_put_var(igeomfile, id_flowelemyzw, yzw(1:ndxndxi))

    !call readyy('Writing flow geometry data',.15d0)

    ! Flow cell contours
    !!!do i=1,ndxndxi
    !!!   numContPts = size(nd(i)%x)
    !!!   ierr = nf90_put_var(igeomfile, id_flowelemcontourx, nd(i)%x, (/ 1, i /), (/ numContPts, 1 /) )
    !!!   ierr = nf90_put_var(igeomfile, id_flowelemcontoury, nd(i)%y, (/ 1, i /), (/ numContPts, 1 /) )
    !!!enddo
    !call readyy('Writing flow geometry data',.45d0)

    do i=1,ndxndxi
       nn = size(nd(i)%x)
       do n = 1,nn
          work2(n,i)=nd(i)%x(n)
       enddo
    enddo
    ierr = nf90_put_var(igeomfile, id_flowelemcontourx, work2(1:numContPts,1:ndxndxi), (/ 1, 1 /), (/ numContPts, ndxndxi /) )
    
    do i=1,ndxndxi
       nn = size(nd(i)%x)
       do n = 1,nn
          work2(n,i)=nd(i)%y(n)
       enddo
    enddo
    ierr = nf90_put_var(igeomfile, id_flowelemcontoury, work2(1:numContPts,1:ndxndxi), (/ 1, 1 /), (/ numContPts, ndxndxi /) )

    deallocate( work2 )

    ! flowcells bottom levels
    ierr = nf90_put_var(igeomfile, id_flowelembl, bl(1:ndxndxi))
    !call readyy('Writing flow geometry data',.55d0)

    allocate(lne1write(numL))
    allocate(lne2write(numL))
    do L=1,numL
       lne1write(L)=lne(1,L)
       lne2write(L)=lne(2,L)
    end do
    ierr = nf90_put_var(igeomfile, id_elemlink,     lne1write, count=(/ 1, numl /), start=(/1,1/))
    ierr = nf90_put_var(igeomfile, id_elemlink,     lne2write, count=(/ 1, numl /), start=(/2,1/))
    deallocate(lne1write)
    deallocate(lne2write)

    ! Flow links
    ierr = nf90_put_var(igeomfile, id_flowlink,   ln(:,1:lnx))
    do i=1,lnx1D
       ierr = nf90_put_var(igeomfile, id_flowlinktype, (/ 1 /), start = (/ i /))
    end do 
    do i=lnx1D+1,lnx
       ierr = nf90_put_var(igeomfile, id_flowlinktype, (/ 2 /), start = (/ i /))
    end do
    !call readyy('Writing flow geometry data',.90d0)
  
    if (lnx > 0) then
       ! Flow links velocity points
       ierr = nf90_put_var(igeomfile, id_flowlinkxu, xu(1:lnx))
       ierr = nf90_put_var(igeomfile, id_flowlinkyu, yu(1:lnx))
    end if
    
    ! domain numbers
    if ( jampi.eq.1 ) then  
       ! flow cell domain numbers    
       ierr = nf90_put_var(igeomfile, id_flowelemdomain, idomain(1:ndxi) ) ! TODO: ndxndxi
       ! flow link domain numbers
       do i=1,Lnx 
          ! determine if flow link is a ghost link and get domain number and ghost level of link
          call link_ghostdata(my_rank, idomain(ln(1,i)), idomain(ln(2,i)), jaghost, idmn)
          ierr = nf90_put_var(igeomfile, id_flowlinkdomain, (/ idmn /), start=(/ i /) )   ! corresponds with partition_get_ghosts
       end do
       ierr = nf90_put_var(igeomfile, id_flowelemglobalnr, iglobal_s(1:ndxi)) ! TODO: ndxndxi
    end if
    !call readyy('Writing flow geometry data',1d0)

    ! Leave the dataset in the same mode as we got it.
    if (jaInDefine == 1) then
        ierr = nf90_redef(igeomfile)
    end if

    !call readyy('Writing flow geometry data',-1d0)
end subroutine unc_write_flowgeom_filepointer

! -- PRIVATE ROUTINES ---------------------------
!> Resets current error status and sets informative message for subsequent
!! errors. Generally called at start of any routine that wants to use
!! routine check_error. The informative message is only shown/used when
!! later check_error's indeed detect an error.
subroutine prepare_error(firstline, level)
    character(len=*), intent(in) :: firstline !< Informative message for screen/log.
    integer,          intent(in), optional :: level !< Error level (one from LEVEL_(FATAL|ERROR|WARN|INFO|DEBUG), default: LEVEL_WARN.)

    err_firstline_ = firstline
    err_firsttime_ = .true.
    nerr_          = 0
    if (present(level)) then
       err_level_ = level
    else
       err_level_ = LEVEL_WARN
    end if

end subroutine prepare_error

!> Check a NetCDF error status and print a message when it is not nf90_noerr.
subroutine check_error(ierr, info, level)
    integer, intent(in)        :: ierr
    character(len=*), intent(in), optional :: info  !< Caller's information string. Will be printed as prefix to the NetCDF error string.
    integer,          intent(in), optional :: level !< Error level (one from LEVEL_(FATAL|ERROR|WARN|INFO|DEBUG), default: the level set by prepare_error.)

    character(len=255)         :: infostring
    integer :: local_level

    if (ierr /= nf90_noerr) then
        nerr_ = nerr_ + 1

        ! Optional informative message (appended to NetCDF error string)
        if (present(info)) then
            infostring = '('//trim(info)//')'
        else
            infostring = ' '
        endif

        ! First error line
        if (err_firsttime_) then
            call mess(LEVEL_WARN, err_firstline_)
            err_firsttime_ = .false.
        endif

        if (present(level)) then
           local_level = level
        else
           local_level = err_level_ ! from prepare_error()
        end if

        ! Actual error line
        call mess(local_level, 'NetCDF error: ', nf90_strerror(ierr), trim(infostring))
    endif
end subroutine check_error

!function unc_is_netfile(filename)
!    character(len=*), intent(in) :: filename
!    logical :: unc_is_netfile
!
!    unc_is_netfile = .true.
!
!end function unc_is_netfile
!
!    ierr = nf90_def_dim(inetfile, 'nElem', nump, id_elemdim)
!    ierr = nf90_def_dim(inetfile, 'nNode', numk, id_nodedim)
!    ierr = nf90_def_dim(inetfile, 'nConnect', 7, id_connectdim)
!!    ierr = nf90_def_dim(inetfile, 'id_len', 40, id_strlendim)
!!    ierr = nf90_def_dim(inetfile, 'time', nf90_unlimited, id_timedim)
!
!    ierr = nf90_def_var(inetfile, 'grid1', nf90_int, (/ id_connectdim, id_elemdim /), id_grid1topo)
!    ierr = nf90_put_att(inetfile, id_grid1topo, 'standard_name', 'net_topology')
!    ierr = nf90_put_att(inetfile, id_grid1topo, 'spatial_dimension', 2)
!    ierr = nf90_put_att(inetfile, id_grid1topo, 'topological_dimension', 2)
!    ierr = nf90_put_att(inetfile, id_grid1topo, 'cell_type', 'nc_mixed')
!    ierr = nf90_put_att(inetfile, id_grid1topo, 'index_start', 1)
!    ierr = nf90_put_att(inetfile, id_grid1topo, 'x_nodal_coordinate', 'x')
!    ierr = nf90_put_att(inetfile, id_grid1topo, 'y_nodal_coordinate', 'y')
!    
!    ierr = nf90_def_var(inetfile, 'x', nf90_double, id_nodedim, id_nodex)
!    ierr = nf90_put_att(inetfile, id_nodex, 'units', 'm')
!    ierr = nf90_put_att(inetfile, id_nodex, 'long_name', 'nodal x-coordinate')
!    ierr = nf90_def_var(inetfile, 'y', nf90_double, id_nodedim, id_nodey)
!    ierr = nf90_put_att(inetfile, id_nodey, 'units', 'm')
!    ierr = nf90_put_att(inetfile, id_nodey, 'long_name', 'nodal y-coordinate')
!
!    ierr = nf90_enddef(inetfile)
!
!    ierr = nf90_put_var(inetfile, id_nodex, xk(1:numk))
!    ierr = nf90_put_var(inetfile, id_nodey, yk(1:numk))
!    do i=1,nump
!        ierr = nf90_put_var(inetfile, id_grid1topo, (/ netcell(i)%n, ( netcell(i)%nod(k), k=1,netcell(i)%n) /), (/ 1, i /) )
!    enddo
!
!    ierr = nf90_close(inetfile)



!> Reads the flow data from a map file for one single variable, specified by the user.
!! Processing is done elsewhere.
subroutine read_flowsamples_from_netcdf(fileName, quantityName, ierr)
    use m_samples
    
    implicit none

    ! I/O variables
    character(len=256),             intent(in)  :: fileName       !< Name of the NetCDF file.
    character(len=256),             intent(in)  :: quantityName   !< Name of the variable (i.e. the QUANTITY in the ext-file, i.e. 'qid').
    integer,                        intent(out) :: ierr           !< Return status (NetCDF operations).
    
    ! Local variables
    integer :: iNetcdfFile,                     &
               id_flowelemdim,                  &
               id_flowlinkdim,                  &
               id_timedim
    integer :: id_varXcoord,                    &
               id_varYcoord,                    &
               id_varData
    integer :: ndxi_read,                       & 
               lnx_read,                        &
               nt_read
    logical :: file_exists
       
    ! Safety check: does the file exist at all?
    inquire(file=trim(fileName), exist=file_exists)
    if ( .not. file_exists ) then
        call mess(LEVEL_FATAL, 'The specified file for the initial conditions sample set has not been found. Check your .ext file.')
        return
    endif

    ! Reset error status:
    call prepare_error('Could not read flow samples from NetCDF file '''//trim(filename)//'''. Details follow:')

    ierr  = unc_open(trim(fileName), nf90_nowrite, iNetcdfFile)
    call check_error(ierr, 'file '''//trim(fileName)//'''')
    if (nerr_ > 0) goto 999

    ! Ask for dimension id of nodes and edges
    ierr = nf90_inq_dimid(iNetcdfFile, 'nFlowElem', id_flowelemdim)
    ierr = nf90_inq_dimid(iNetcdfFile, 'nFlowLink', id_flowlinkdim)
    ierr = nf90_inq_dimid(iNetcdfFile, 'time',      id_timedim)

    ! Ask for dimensions of nodes and edges, ergo: the number of netnodes and netlinks
    ierr = nf90_inquire_dimension(iNetcdfFile, id_flowelemdim, len=ndxi_read)
    ierr = nf90_inquire_dimension(iNetcdfFile, id_flowlinkdim, len=lnx_read )
    ierr = nf90_inquire_dimension(iNetcdfFile, id_timedim,     len=nt_read )
    
    ! Read the output data
    if     (quantityName == 'initialwaterlevel') then
        
        ! Allocate the output variable to be read, based on the location of the variable
        ns   = ndxi_read
    
        ! Allocate the output variables to be read
        if (allocated (xs) ) deallocate (xs,ys,zs)
        allocate (xs(ns), ys(ns), zs(ns), stat=ierr)
    
        ! Read the xcoord data
        ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_xcc', id_varXcoord)
        ierr = nf90_get_var(iNetcdfFile, id_varXcoord, xs(1:ns))
    
        ! Read the xcoord data
        ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_ycc', id_varYcoord)
        ierr = nf90_get_var(iNetcdfFile, id_varYcoord, ys(1:ns))
        
        ! Read the actual water levels
        ierr = nf90_inq_varid(iNetcdfFile, 's1', id_varData)
        if (ierr .lt. 0) then
            ierr = unc_close(iNetcdfFile)
            call mess(LEVEL_FATAL, 'No waterlevel data found in the specified NetCDF file.')
            return
        else
            ierr = nf90_get_var(iNetcdfFile, id_varData, zs(1:ns), start = (/ 1, nt_read /))
        endif
        
    elseif (quantityName == 'initialsalinity') then
        
        ! Allocate the output variable to be read, based on the location of the variable
        ns   = ndxi_read
    
        ! Allocate the output variables to be read
        if (allocated (xs) ) deallocate (xs,ys,zs)
        allocate (xs(ns), ys(ns), zs(ns), stat=ierr)
    
        ! Read the xcoord data
        ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_xcc', id_varXcoord)
        ierr = nf90_get_var(iNetcdfFile, id_varXcoord, xs(1:ns))
    
        ! Read the xcoord data
        ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_ycc', id_varYcoord)
        ierr = nf90_get_var(iNetcdfFile, id_varYcoord, ys(1:ns))
        
        ! Read the actual water levels
        ierr = nf90_inq_varid(iNetcdfFile, 'sa1', id_varData)
        if (ierr .lt. 0) then
            ierr = unc_close(iNetcdfFile)
            call mess(LEVEL_FATAL, 'No salinity data found in the specified NetCDF file.')
            return
        else
            ierr = nf90_get_var(iNetcdfFile, id_varData, zs(1:ns), start = (/ 1, nt_read /))
        endif
        
    elseif (quantityName == 'initialvelocityx') then
        
        ! Allocate the output variable to be read, based on the location of the variable
        ns   = ndxi_read
    
        ! Allocate the output variables to be read
        if (allocated (xs) ) deallocate (xs,ys,zs)
        allocate (xs(ns), ys(ns), zs(ns), stat=ierr)
    
        ! Read the xcoord data
        ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_xcc', id_varXcoord)
        ierr = nf90_get_var(iNetcdfFile, id_varXcoord, xs(1:ns))
    
        ! Read the xcoord data
        ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_ycc', id_varYcoord)
        ierr = nf90_get_var(iNetcdfFile, id_varYcoord, ys(1:ns))
        
        ! Read the actual water levels
        ierr = nf90_inq_varid(iNetcdfFile, 'ucx', id_varData)
        if (ierr .lt. 0) then
            ierr = unc_close(iNetcdfFile)
            call mess(LEVEL_FATAL, 'No velocityx data found in the specified NetCDF file.')
            return
        else
            ierr = nf90_get_var(iNetcdfFile, id_varData, zs(1:ns), start = (/ 1, nt_read /))
        endif
        
    elseif (quantityName == 'initialvelocityy') then
        
        ! Allocate the output variable to be read, based on the location of the variable
        ns   = ndxi_read
    
        ! Allocate the output variables to be read
        if (allocated (xs) ) deallocate (xs,ys,zs)
        allocate (xs(ns), ys(ns), zs(ns), stat=ierr)
    
        ! Read the xcoord data
        ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_xcc', id_varXcoord)
        ierr = nf90_get_var(iNetcdfFile, id_varXcoord, xs(1:ns))
    
        ! Read the xcoord data
        ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_ycc', id_varYcoord)
        ierr = nf90_get_var(iNetcdfFile, id_varYcoord, ys(1:ns))
        
        ! Read the actual water levels
        ierr = nf90_inq_varid(iNetcdfFile, 'ucy', id_varData)
        if (ierr .lt. 0) then
            ierr = unc_close(iNetcdfFile)
            call mess(LEVEL_FATAL, 'No velocity data found in the specified NetCDF file.')
            return
        else
            ierr = nf90_get_var(iNetcdfFile, id_varData, zs(1:ns), start = (/ 1, nt_read /))
        endif
        
    else
        
        call mess(LEVEL_FATAL, 'Initial field specification of this quantity not supported through NetCDF file.')
        return
        
    endif
    
    ! Close the netcdf-file
999 continue    
    ierr = unc_close(iNetcdfFile)
    
end subroutine read_flowsamples_from_netcdf
    
! Read cell info. in order to bypass findcells
subroutine readcells(filename, ierr, jaidomain, jaiglobal_s, jareinitialize)
    
    use network_data
    use m_flowgeom
    use m_alloc
    use m_partitioninfo, only: idomain, ndomains, iglobal_s, Nglobal_s
    use dfm_error
    use gridoperations
    
    character(len=*), intent(in)  :: filename  !< Name of NetCDF file.
    integer,          intent(out) :: ierr      !< Return status (NetCDF operations)
    integer, optional, intent(in) :: jaidomain !<if read idomain
    integer, optional, intent(in) :: jaiglobal_s !< read global cell numbers (1) or not (0)
    integer, optional, intent(in) :: jareinitialize!< if is re-initialize
    
    
    integer                       :: inetfile, & 
                                     id_netnodedim, id_netlinkdim, id_netelemdim, & !< Dimensions
                                     id_netelemmaxnodedim, id_nodmaxlinkdim,  id_netlinkptsdim, &  
                                     id_netnodex, id_netnodey,  &    !< Node variables
                                     id_netlink, id_netlinktype, &   !< Link variables
                                     id_netelemnode, id_netelemlink, &    !< Netcell variables
                                     id_idomain, id_iglobal_s
               
    integer                       :: ja_oldformatread, L, nv, k, s, jaidomain_, jaiglobal_s_, fillvalue, jareinitialize_
    integer                       :: nerr_store
    integer, allocatable          :: netcellnod(:,:), netcelllin(:,:), kn_tmp(:,:), ltype_tmp(:)
    integer                       :: numl_read, numk_read
    ierr = DFM_NOERR
    
    if ( len_trim(filename)<1 ) then
       ierr = DFM_GENERICERROR
       return
    end if

    jaidomain_ = 0
    if(present(jaidomain)) jaidomain_ = jaidomain
    
    jaiglobal_s_ = 0
    if ( present(jaiglobal_s) ) then
       jaiglobal_s_ = jaiglobal_s
    end if
    
    jareinitialize_ = 0
    if(present(jareinitialize)) jareinitialize_ = jareinitialize

    call readyy('Reading net data',0d0)
    ja_oldformatread = 0

    call prepare_error('Could not read net cells from NetCDF file '''//trim(filename)//''' (is not critical). Details follow:', LEVEL_DEBUG)

    nerr_ = 0

    ierr = unc_open(filename, nf90_nowrite, inetfile)
    call check_error(ierr, 'file '''//trim(filename)//'''')
    if (nerr_ > 0) goto 888

    ! Get nr of cells
    ierr = nf90_inq_dimid(inetfile, 'nNetElem', id_netelemdim)
    call check_error(ierr, 'nNetElem')
    if (nerr_ > 0) goto 888

    ierr = nf90_inquire_dimension(inetfile, id_netelemdim, len=nump1d2d)
    call check_error(ierr, 'Elem count')
    if (nerr_ > 0) goto 888

    ! check number of netlinks in the network file
    ierr = nf90_inq_dimid(inetfile, 'nNetLink', id_netlinkdim)
    call check_error(ierr, 'nNetLink')
    ierr = nf90_inquire_dimension(inetfile, id_netlinkdim, len=numl_read)
    call check_error(ierr, 'link count')
    if (numl_read .ne. numl) then
       goto 888
    end if

    ! check number of netnodes in the network file
    ierr = nf90_inq_dimid(inetfile, 'nNetNode', id_netnodedim)
    call check_error(ierr, 'nNetNode')
    ierr = nf90_inquire_dimension(inetfile, id_netnodedim, len=numk_read)
    call check_error(ierr, 'Node count')
    if (numk_read .ne. numk) then
       goto 888
    end if

    call readyy('Reading net data',.1d0)


    
    ! Read Netcell and cell center 
    ierr = nf90_inq_varid(inetfile, 'NetElemNode',  id_netelemnode)
!    call check_error(ierr, 'NetElemNode')
    if ( ierr /= NF90_NOERR ) goto 888
    ierr = nf90_inq_varid(inetfile, 'NetElemLink',  id_netelemlink)
!    call check_error(ierr, 'NetElemLink')
    if ( ierr /= NF90_NOERR ) goto 888
    
    ierr = nf90_inq_dimid(inetfile, 'nNetElemMaxNode', id_netelemmaxnodedim)
!    call check_error(ierr, 'nNetElemMaxNode')
    if ( ierr /= NF90_NOERR ) goto 888
    ierr = nf90_inquire_dimension(inetfile, id_netelemmaxnodedim, len = nv)
    if ( ierr /= NF90_NOERR ) goto 888

!    if (nerr_ > 0) goto 888
    
    
    !ierr = nf90_inq_varid(inetfile, 'ElemCenter_x', id_elemcenx)
    !call check_error(ierr, 'x coordinate of cell center')
    !ierr = nf90_inq_varid(inetfile, 'ElemCenter_y', id_elemceny)
    !call check_error(ierr, 'y coordinate of cell center')
   
    call readyy('Reading net data',.3d0)
            
    call increasenetcells(nump1d2d, 1.0, .false.)
    ierr = nf90_get_att(inetfile, id_netelemnode, '_FillValue', fillvalue)
    
    allocate(netcellnod(nv, nump1d2d))
    allocate(netcelllin(nv, nump1d2d))
    ierr = nf90_get_var(inetfile, id_netelemnode, netcellnod)
    call check_error(ierr, 'cell elem.') 
    ierr = nf90_get_var(inetfile, id_netelemlink, netcelllin)
    call check_error(ierr, 'cell link') 
    nump = 0  
    do L = 1, nump1d2d     
       if (netcelllin(1 ,L) == 0) then ! this is a 1d cell
          call realloc(netcell(L)%nod, 1, keepExisting=.false.)
          call realloc(netcell(L)%lin, 1, keepExisting=.false.)
          netcell(L)%nod = netcellnod(1, L)
          netcell(L)%lin = 0
          netcell(L)%n   = 1
       else
          nump = nump + 1              ! update Nr of 2D cells
          s = nv                       ! s will be computed to for Nr of nodes of this cell
          do k = nv,1,-1
             if (netcellnod(k, L) /= fillvalue) then
                s = k
                exit
             end if
          enddo
          call realloc(netcell(L)%nod, s, keepExisting=.false.)
          call realloc(netcell(L)%lin, s, keepExisting=.false.)
          netcell(L)%nod = netcellnod(1:s, L)
          netcell(L)%lin = netcelllin(1:s, L)
          netcell(L)%n   = s
       endif
    enddo
    
    call readyy('Reading net data',.8d0)
    call check_error(ierr, 'net elem')  
    
    ! read idomain
    if (jaidomain_ .ne. 0) then
      ierr = nf90_inq_varid(inetfile, 'idomain', id_idomain)
      call check_error(ierr, 'idomain')
      if ( ierr.eq.nf90_noerr ) then
         call realloc(idomain, nump1d2d, stat = ierr, keepExisting = .false.)
         ierr = nf90_get_var(inetfile, id_idomain, idomain, count = (/ nump1d2d /))
         ierr = nf90_get_att(inetfile, id_idomain, 'valid_max', ndomains)
      else  ! no subdomain numbers in netfile
         ndomains = 0
         if ( allocated(idomain) ) deallocate(idomain)
      end if
    endif
    
    if ( jaiglobal_s_.eq.1 ) then
!      store nerr_
       nerr_store = nerr_
       ierr = nf90_inq_varid(inetfile, 'iglobal_s', id_iglobal_s)
       call check_error(ierr, 'iglobal_s')
       if ( ierr.eq.nf90_noerr ) then
          call realloc(iglobal_s, nump1d2d, stat = ierr, keepExisting = .false.)
          ierr = nf90_get_var(inetfile, id_iglobal_s, iglobal_s, count = (/ nump1d2d /))
          ierr = nf90_get_att(inetfile, id_iglobal_s, 'valid_max', Nglobal_s)
       else  ! no global cell numbers in netfile (not a problem)
          Nglobal_s = 0
          if ( allocated(iglobal_s) ) deallocate(iglobal_s)
!         restore nerr_
          nerr_ = nerr_store
       end if
    end if
    
    if (jareinitialize_ .ne. 0) then ! when re-initialize in GUI, need to read KN, since KN has been changed in renumberflownode
      ierr = nf90_inq_dimid(inetfile, 'nNetLink', id_netlinkdim)
      call check_error(ierr, 'nNetLink')
      ierr = nf90_inquire_dimension(inetfile, id_netlinkdim, len=numl)
      call check_error(ierr, 'link count')
      if (nerr_ > 0) goto 888
      
      allocate(kn_tmp(2, numl))
      allocate(ltype_tmp(numl))
      ierr = nf90_inq_varid(inetfile, 'NetLink', id_netlink)
      ierr = nf90_get_var(inetfile, id_netlink, kn_tmp)
      call check_error(ierr, 'NetLink')
      kn(1:2,1:numl) = kn_tmp(1:2, 1:numl)
      
      ierr = nf90_inq_varid(inetfile, 'NetLinkType', id_netlinktype)
      ierr = nf90_get_var(inetfile, id_netlinktype, ltype_tmp)
      call check_error(ierr, 'NetLinkType')
      kn(3,1:numl) = ltype_tmp(1:numl)
      deallocate(kn_tmp,ltype_tmp)
      call check_error(ierr, 'NetLink')
    endif
    
    call readyy('Reading net data',1d0) 
    ierr = unc_close(inetfile)
    if (nerr_ > 0) goto 888

    call readyy('Reading net data',-1d0)   
    return ! Return with success

888 continue
    ! Some error occurred
    ierr  = 1
    nerr_ = 0
end subroutine readcells

!!> Check the orientations of flowlinks in the current model, comparing with flowlinks that are read from the rst file.
!!> If the orientations are different, then reverse the orientation of the velociy that is read from rst file.
subroutine check_flowlink_orientation(lnx_own, ln_read, ilink_own, lnx_merge, ndxi_merge, ndxi, inode_owninv, inode_merge2own)
   use m_flowgeom,      only: ln
   use m_flow,          only: u1, u0
   use m_partitioninfo, only: my_rank, jampi
   use unstruc_messages
   implicit none

   
   integer,                        intent(in) :: lnx_own      ! Number of flowlinks (excluding ghosts) on the current model
   integer, dimension(2,lnx_own),  intent(in) :: ln_read      ! FlowLink read from rst file
   integer, dimension(lnx_own),    intent(in) :: ilink_own(:) ! index mapping from its own partition (excluding ghosts) to this partition(with ghosts) 
   integer, optional,              intent(in) :: lnx_merge    ! Number of Flowlinks in the merged file
   integer,                        intent(in) :: ndxi_merge   ! Number of Flownodes in the merged file
   integer,                        intent(in) :: ndxi         ! Number of Flownodes (including ghosts) on the current model
   integer, dimension(ndxi),       intent(in) :: inode_owninv ! Mapping from actual index (including ghosts) to the own index (excluding ghosts)
   integer, dimension(ndxi_merge), intent(in) :: inode_merge2own ! Mapping from merged file to the own index
   integer                                    :: L, LL, k1, k2, k1_read, k2_read, kk1, kk2, kk1_read, kk2_read, num=0, ierr=0
   character(len=128)                         :: message
    
   
   call mess(LEVEL_INFO, 'Check flowlink orientation when restart:')
    
   ! check orientation for flowlinks of its own
   do L = 1, lnx_own
      LL = ilink_own(L) ! L link is LL link when including ghost links
      
      ! on the curret model
      k1 = ln(1,LL)     ! in the current model, ln array contains ghosts
      k2 = ln(2,LL)
      if (k1>ndxi) then
         kk1 = 0
      else
         kk1 = max(inode_owninv(k1), 0) ! node index of its own
      end if
      if (k2>ndxi) then
         kk1 = 0
      else
         kk2 = max(inode_owninv(k2), 0)
      end if
      
      ! from rst file
      k1_read = ln_read(1,L) ! node index in the merged file
      k2_read = ln_read(2,L)
      if (k1_read > 0) then
         kk1_read = max(inode_merge2own(k1_read), 0 ) ! node index of its own 
      else
         ! the fill_value in ln_read from the merged file denotes a bnd flownodes. In this situation, the orientation of 
         ! the flowlink is always pointing to the interior of the comp. domain. So we do not need to compare the coordinates of the 
         ! bnd flownodes
         kk1_read = 0
      end if
      
      if (k2_read > 0) then
         kk2_read = max(inode_merge2own(k2_read), 0)
      else
         kk2_read = 0
      end if

      if (kk1 .ne. kk1_read) then
         if (kk1 .eq. kk2_read .and. kk2 .eq. kk1_read) then ! For the same link L, the orientation is differet
            if (jampi==0) then
               write(message, "('orientation mismatches for link=', I0, ', has been fixed.')") L
               call mess(LEVEL_INFO, trim(message))
            else
               write(message, "('my_rank=', I0,': orientation mismatches for link=', I0, ', has been fixed.')") my_rank, L
               call mess(LEVEL_INFO, trim(message))
            end if         
            
            u1(LL) = -u1(LL)
            u0(LL) = -u0(LL)
            num   = num + 1
         else ! It is not the same link
            if (jampi==0) then
               write(message, "('flowlink mismatches: link=', I0, '.')") L
               call mess(LEVEL_ERROR, trim(message))
            else
               write(message, "('my_rank=', I0, ': flowlink mismatches: link=', I0,'.')") my_rank, L
               call mess(LEVEL_ERROR, trim(message))
            end if
         end if
      end if
   end do
         
   if (num == 0) then
      if (jampi == 0) then
         call mess(LEVEL_INFO, 'No mismatched flowlink is found. Finish checking.')
      else
         write(message, "('my_rank=', I0,': no mismatched flowlink is found. Finish checking')") my_rank
         call mess(LEVEL_INFO, trim(message))
      end if
   end if

end subroutine check_flowlink_orientation

subroutine find_flownodesorlinks_merge(n, x, y, n_loc, n_own, iloc_own, iloc_merge, janode, jaerror2sam, inode_merge2loc)
   use kdtree2Factory
   use unstruc_messages
   use m_flowgeom
   use network_data
   use m_missing, only: dmiss
   use m_sferic, only: jsferic
   use m_samples
   use m_alloc
   
   implicit none
   type(kdtree_instance)                           :: treeinst
   integer,                          intent(in)    :: n               !< number of flownodes in merged map file
   double precision, dimension(n),   intent(in)    :: x, y            !< coordinates of flownode circumcenters or flowlink centers in merged map file
   integer,                          intent(in)    :: n_loc           !< number of flownodes or flowlinks of the current subdomain (including ghosts)
   integer,                          intent(in)    :: n_own           !< number of flownodes or flowlinks of the current subdomain (excluding ghosts)
   integer,                          intent(in)    :: janode          !< if janode==1, find flow nodes, otherwise find flow links
   integer, dimension(n_loc),        intent(inout) :: iloc_own        !< mapping to the actual index on the current subdomain 
   integer, dimension(n_loc),        intent(inout) :: iloc_merge      !< mapping to the index in the merged map file    
   integer, dimension(n), optional,  intent(inout) :: inode_merge2loc !< mapping from the index in the merged map file
   integer,                          intent(in)    :: jaerror2sam  !< add unfound nodes to samples (1) or not (0)
   integer                                         :: ierror = 1
   integer                                         :: k, nn, i, jj, kk, jamerge2own
   double precision                                :: R2search = 1d-8 !< Search radius
   double precision                                :: t0, t1
   character(len=128)                              :: mesg
   double precision, allocatable                   :: x_tmp(:), y_tmp(:)

   call klok(t0)
   if (present(inode_merge2loc)) then
      jamerge2own = 1
   else
      jamerge2own = 0
   end if
   
   allocate(x_tmp(n_loc))
   allocate(y_tmp(n_loc))
   if(janode == 1) then
      x_tmp = xzw
      y_tmp = yzw
   else if (janode == 2) then ! boundary waterlevel points
      x_tmp = xz(ndxi+1:ndx)
      y_tmp = yz(ndxi+1:ndx)
   else
      x_tmp = xu
      y_tmp = yu
   endif
   
   !build kdtree
   call build_kdtree(treeinst, n, x, y, ierror, jsferic, dmiss)
   if ( ierror.ne.0 ) then
      goto 1234
   end if
   
   call mess(LEVEL_INFO, 'Restart parallel run: Finding flow nodes/ flow links...')
   
!  clear samples
   do k = 1, n_own
        !  fill query vector
        kk = iloc_own(k)
        call make_queryvector_kdtree(treeinst, x_tmp(kk), y_tmp(kk), jsferic)
        !  count number of points in search area
        NN = kdtree2_r_count(treeinst%tree, treeinst%qv, R2search)
        if ( NN.eq.0 ) then
!           call mess(LEVEL_INFO, 'No flownode/flowlink is found')
           
           if ( jaerror2sam.eq.1 ) then
   !          add to samples
              call mess(LEVEL_INFO, 'copying unfound flownodes/links to samples')
              call INCREASESAM(NS+1)
              NS=NS+1
              xs(Ns) = x_tmp(kk)
              ys(NS) = y_tmp(kk)
              zs(NS) = dble(NN)
           end if
           
           cycle ! no points found
        else
           !  reallocate if necessary
            call realloc_results_kdtree(treeinst, NN)
            
           !  find nearest NN samples
            if (NN > 1) then ! If we found more candidates within small search radius, then we should consider falling back to inside-polygon check of cell contour
!               if (janode == 1) then
!                  write (msgbuf, '(a,i0,a,i0,a)') 'Multiple flow nodes in merged restart file can be matched with current model''s node #', kk, '. Nr of candidates: ', NN, '. Picking last.'
!               else
!                  write (msgbuf, '(a,i0,a,i0,a)') 'Multiple flow links in merged restart file can be matched with current model''s link #', kk, '. Nr of candidates: ', NN, '. Picking last.'
!               end if
!               call err_flush()
!               ! TODO: AvD: return error code from this routine
               
               
              if ( jaerror2sam.eq.1 ) then
      !          add to samples
!                 call mess(LEVEL_INFO, 'copying double or more found flownodes/links to samples')
                 call INCREASESAM(NS+1)
                 NS=NS+1
                 xs(Ns) = x_tmp(kk)
                 ys(NS) = y_tmp(kk)
                 zs(NS) = dble(NN)
              end if
           
            end if

            call kdtree2_n_nearest(treeinst%tree, treeinst%qv, NN, treeinst%results)
            do i=1,NN
               jj = treeinst%results(i)%idx
               iloc_merge(k) = jj 
               if (jamerge2own > 0) then
                  inode_merge2loc(jj) = k
               end if
            end do
        endif
   end do

   call klok(t1)

   write(mesg, "('done in ', F12.5, ' sec.')") t1-t0
   call mess(LEVEL_INFO, trim(mesg))
   ierror = 0
1234 continue

!    deallocate  
   if ( treeinst%itreestat.ne.ITREE_EMPTY ) then 
      call delete_kdtree2(treeinst)
   endif

   return  
end subroutine find_flownodesorlinks_merge

!! check if the flownodes or flowlinks in the current model have the same numbering with in the rst file
subroutine check_flownodesorlinks_numbering_rst(n, janode, x_rst, y_rst, ierror)
   use network_data, only: xzw, yzw
   use m_flowgeom, only: xu, yu
   use unstruc_messages
   use m_missing, only: dmiss
   use geometry_module, only: dbdistance
   use m_sferic, only: jsferic, jasfer3D
   use m_partitioninfo, only: jampi, my_rank
   implicit none
   
   integer,                        intent(in) :: n            ! Number of flownodes/flowlinks to be checked
   integer,                        intent(in) :: janode       !if janode==1, find flow nodes, otherwise find flow links
   double precision, dimension(:), intent(in) :: x_rst, y_rst ! Coordinates read from rst file
   integer,                        intent(out):: ierror
    
   double precision, allocatable              :: x_tmp(:), y_tmp(:)
   integer                                    :: i
   double precision                           :: dist, dtol = 1d-8
   character(len=128)                         :: message
 
   ierror = 0
   
   allocate(x_tmp(n))
   allocate(y_tmp(n))
   if (janode == 1) then
      call mess(LEVEL_INFO, 'Check flownodes numbering when restart:')
      x_tmp = xzw
      y_tmp = yzw
   else
      call mess(LEVEL_INFO, 'Check flowlinks numbering when restart:')
      x_tmp = xu
      y_tmp = yu
   endif

   do i = 1, n
      dist = dbdistance(x_tmp(i),y_tmp(i),x_rst(i),y_rst(i), jsferic, jasfer3D, dmiss)
      if (dist > dtol) then
         ierror = 1
         if (janode == 1) then
            if (jampi > 0) then
               write(message, "('my_rank=', I0, ': flownode mismatches: node=', I0,'.')") my_rank, i
               call mess(LEVEL_ERROR, trim(message))
            else
               write(message, "('flownode mismatches: node=', I0,'.')") i
               call mess(LEVEL_ERROR, trim(message))
            end if
         else
            if (jampi > 0) then
               write(message, "('my_rank=', I0, ': flowlink mismatches: link=', I0,'.')") my_rank, i
               call mess(LEVEL_ERROR, trim(message))
            else
               write(message, "('flowlink mismatches: link=', I0,'.')") i
               call mess(LEVEL_ERROR, trim(message))
            end if
         end if
         
         exit
      end if
   end do
   return
end subroutine check_flownodesorlinks_numbering_rst

!> Reads the 1d mesh assuming at least two nodes per branch (old file version) 
integer function read_1d_mesh_convention_one(ioncid, numk_keep, numl_keep, numk_last, numl_last) result (ierr)

   use network_data
   use unstruc_channel_flow
   use m_1d_networkreader
   use m_flow1d_reader
   use m_profiles
   use gridoperations
   use m_save_ugrid_state
   use m_missing

   integer, intent(in)          :: ioncid
   integer, intent(inout)       :: numk_keep
   integer, intent(inout)       :: numl_keep
   integer, intent(inout)       :: numk_last 
   integer, intent(inout)       :: numl_last

   !locals
   type(t_branch), pointer      :: pbr
   type(t_node), pointer        :: pnod
   integer                      :: inod, ibr, ngrd, k
   logical                      :: dflowfm_1d
   
   
   !reduce the scope, do deallocation here
   if (allocated(mesh1dNodeIds)) deallocate(mesh1dNodeIds)
   if (allocated(mesh1dUnmergedToMerged)) deallocate(mesh1dUnmergedToMerged)
   !if (allocated(mesh1dMergedToUnMerged)) deallocate(mesh1dMergedToUnMerged)

   allocate(mesh1dNodeIds(size(xk)))
   allocate(mesh1dUnmergedToMerged(size(xk)))
   !allocate(mesh1dMergedToUnMerged(size(xk)))

   numMesh1dBeforeMerging = 0
   ierr = 0
   if (.not. network%loaded) then
      dflowfm_1d = .true.
!      call read_1d_ugrid(network, ioncid, dflowfm_1d, nodesOnBranchVertices = 1)
      call read_1d_ugrid(network, ioncid, dflowfm_1d)
      if (network%loaded) then
         call admin_network(network, numk, numl)

         !! TODO: Start temporary fix, to be removed when 1d ugrid file is correct (at this point branches are not connected)
         do inod = 1, network%nds%Count
            pnod => network%nds%node(inod)
            pnod%gridNumber = 0
         enddo

         numk = 0
         numl = 0
         do ibr = 1, network%brs%Count
            pbr => network%brs%branch(ibr)
            ! first step add coordinates and bed levels to nodes
            if ( pbr%FromNode%gridNumber == 0 ) then
               numk = numk + 1
               pbr%FromNode%gridNumber = numk
               xk(numk) = pbr%Xs(1)
               yk(numk) = pbr%Ys(1)
               zk(numk) = dmiss
            endif
            pbr%grd(1) = pbr%FromNode%gridNumber
            ! id-mesh node mapping
            numMesh1dBeforeMerging = numMesh1dBeforeMerging + 1
            mesh1dNodeIds(numMesh1dBeforeMerging) = pbr%gridPointIDs(1)
            mesh1dUnmergedToMerged(numMesh1dBeforeMerging) = pbr%grd(1)
            !mesh1dMergedToUnMerged(pbr%grd(1)) = numMesh1dBeforeMerging
            ngrd = pbr%gridPointsCount

            do k = 2, ngrd-1
               ! i do not want to renumber the nodes, otherwise link info gets invalidated
               numk = numk + 1
               pbr%grd(k) = numk
               xk(numk) = pbr%Xs(k)
               yk(numk) = pbr%Ys(k)
               zk(numk) = dmiss
               ! id-mesh node mapping
               numMesh1dBeforeMerging = numMesh1dBeforeMerging + 1
               mesh1dNodeIds(numMesh1dBeforeMerging) = pbr%gridpointids(k)
               mesh1dUnmergedToMerged(numMesh1dBeforeMerging) = pbr%grd(k)
               !mesh1dMergedToUnMerged(pbr%grd(k)) = numMesh1dBeforeMerging
            enddo
            if ( pbr%toNode%gridNumber == 0 ) then
               numk = numk + 1
               pbr%toNode%gridNumber = numk
               xk(numk) = pbr%Xs(ngrd)
               yk(numk) = pbr%Ys(ngrd)
               zk(numk) = dmiss
            endif
            pbr%grd(ngrd) = pbr%toNode%gridNumber
            ! id-mesh node mapping
            numMesh1dBeforeMerging = numMesh1dBeforeMerging + 1
            mesh1dNodeIds(numMesh1dBeforeMerging) = pbr%gridPointIDs(ngrd)
            mesh1dUnmergedToMerged(numMesh1dBeforeMerging) = pbr%grd(ngrd)
            !mesh1dMergedToUnMerged(pbr%grd(ngrd)) = numMesh1dBeforeMerging

            ! second step create links
            do k = 1, ngrd-1
               numl = numl+1
               kn(1,numl) = pbr%grd(k)
               kn(2,numl) = pbr%grd(k+1)
               kn(3,numl) = 1
            enddo

         enddo

         network%numk = numk
         network%numl = numl

         numk_keep = numk
         numl_keep = numl

         numk_last = numk
         numl_last = numl
         ! End temporary fix
         ! TODO: Once dflowfm's own 1D and the flow1d code are aligned, the following switch should probably disappear.

         jainterpolatezk1D = 0
      else
         network%numk = 0
         network%numl = 0
      endif
   endif
   
   ! re-allocate mesh1dNodeIds and mesh1dUnmergedToMerged
   call realloc(mesh1dNodeIds, numMesh1dBeforeMerging, keepExisting=.true.)
   call realloc(mesh1dUnmergedToMerged, numMesh1dBeforeMerging, keepExisting=.true.)

end function read_1d_mesh_convention_one

end module unstruc_netcdf
