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

! $Id: dfm_merge.F90 65865 2020-01-24 18:02:36Z zhao $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/tools_gpl/dfmoutput/src/dfm_merge.F90 $

module dfm_merge
use netcdf
use netcdf_utils
use dfm_params
implicit none

! TODO: re-use the definitions below from original source: unstruc_netcdf
! The following location codes generalize for 1D/2D/3D models.
integer, parameter :: UNC_LOC_CN = 1  !< Data location: corner point.
integer, parameter :: UNC_LOC_S  = 2  !< Data location: pressure point.
integer, parameter :: UNC_LOC_SN = 102  !< TEMP: Data location: netelem (not always identical to flowelem). (UNST-1256)
integer, parameter :: UNC_LOC_U  = 3  !< Data location: horizontal velocity point.
integer, parameter :: UNC_LOC_L  = 13  !< Data location: net link. TODO: AvD: phase out
integer, parameter :: UNC_LOC_W  = 6  !< Data location: vertical velocity point.
integer, parameter :: UNC_LOC_SBND=7  !< Data location: boundary waterlevel points

contains

!> Merges multiple D-Flow FM map files into a single file.
!! Typically this routine should be used on output of a parallel run,
!! i.e., on files <modelname>_000x_map.nc
function dfm_merge_mapfiles(infiles, nfiles, outfile, force) result(ierr)
   use m_alloc
   use string_module
   use io_netcdf
   use io_ugrid, only: mdim_face, mdim_node, mdim_edge, mdim_maxfacenodes
   implicit none

   character(len=MAXNAMELEN), intent(inout) :: infiles(:) !< Input files names, will be sorted if not sorted already.
   integer,                   intent(in)    :: nfiles     !< Number of input files.
   character(len=MAXNAMELEN), intent(inout) :: outfile    !< Output file name. When empty, the name is derived from the input file names.
   logical,                   intent(in)    :: force      !< Whether to disallow interactive user prompting (for file overwrite)
   integer                                  :: ierr       !< Result status (0 = success)

   integer, parameter :: int8 = 1     ! also local storage compact in 1 byte
   integer, parameter :: mapclass_time_buffer_size =   1

   integer, dimension(nfiles+1) :: ncids, id_timedim, id_facedim, id_edgedim, id_laydim, id_wdim, id_nodedim, id_sedtotdim, id_sedsusdim, &
                                   id_netedgedim, id_netfacedim, id_netfacemaxnodesdim, id_time, id_timestep, id_bnddim !< dim and var ids, maintained for all input files + 1 output file.
   double precision :: convversion
   integer :: jaugrid, iconvtype, formatCode, new_ndx
   integer, dimension(nfiles) :: jaugridi, ioncids
   logical :: isNetCDF4
   integer, allocatable :: dimids(:,:) !< (nfiles+1:NF90_MAX_DIMS) Used for storing any remaining vectormax dimension IDs
   integer, dimension(nfiles+1), target :: ndx, lnx, ndxg, lnxg, kmx, numk, numl, nump, numkg, numlg, netfacemaxnodes, nt, ndxbnd !< counters, maintained for all input files + 1 output file.
   integer, dimension(:), pointer :: item_counts !< Generalized count pointer, will point to ndx, lnx, numl, or numk during var data reading + writing.
   integer:: noutfile !< array index/position of output file ids, by default the last, i.e., nfiles + 1.
   integer, allocatable, target  :: face_domain(:), facebnd_domain(:), edge_domain(:), node_domain(:), netedge_domain(:) !< Global face/edge/node numbers and their domain number.
   integer,              pointer :: item_domain(:)
   integer, allocatable :: ln(:,:) !< Flow links
   integer, allocatable :: edgenodes(:,:) !< Net links
   integer, allocatable :: netedgefaces(:,:)  !< ElemLinks
   integer, allocatable :: netfaceedges(:,:)  !< NetElemLinks
   integer, allocatable :: netfacenodes(:,:) !< Net cell - to - net node mapping
   integer, allocatable :: netfacenodesl(:,:) !< Net cell - to - net node mapping for local usage (per cell)
   integer, allocatable :: face_c2g(:), node_c2g(:), netedge_c2g(:), edge_c2g(:) !< Concatenated index - to - global index mapping.
   integer, allocatable :: node_g2c(:), edge_g2c(:), face_g2c(:), netedge_g2c(:) !< Global index - to - concatenated index mapping.
   integer, allocatable :: node_faces(:,:) ! faces that surrounds the node
   integer, allocatable :: nfaceedges(:)   ! total number of edges that surround a face
   double precision, allocatable :: node_x(:), node_y(:), edge_x(:), edge_y(:) !< coordinates
   double precision              :: xx, yy
   integer :: im,nm,topodim, ikk, ic, iii, k1c, g1, g2, nfaces, iedge, iface, tmpvarDim, jafound
   integer :: ifacefile, ifacein, ifaceout, ifacec
   integer :: id_nodex, id_nodey, id_edgex, id_edgey
   integer :: intmiss = -2147483647 ! integer fillvalue
   double precision :: dmiss = -999d0, intfillv
!netface_g2c(:)
   integer :: id_facedomain, id_faceglobnr, id_edgefaces, id_netfacenodes, id_edgenodes, id_netedgefaces, id_netfaceedges
   integer :: ierri
   integer :: maxlen, nlen, plen, mlen, ii, id, iv, it, itm, ip, ik, is, ie, nvarsel, ntsel, nvars, ndims, nvardims, vartype
   integer :: netfacemaxnodesg, netnodemaxface=10, ndxc, lnxc, numkc, numlc,ndx_bndc
   integer :: nfaceglob,    nfaceglob0,    nfacecount, ifaceglob
   integer :: nedgeglob,    nedgeglob0,    nedgecount, iedgeglob
   integer :: nnodeglob,    nnodeglob0,    nnodecount
   integer :: nbndglob,     nbndcount
   integer :: netedgecount, inetedgeglob
!   integer :: nnetfaceglob, nnetfaceglob0, nnetfacecount, numpg
   integer :: nnetedgeglob, nnetedgeglob0, nnetedgecount
   integer :: nitemglob, nitemglob0, nitemcount, maxitems
   integer :: nkmxglob

   integer :: idom, n1, n2, n3, k1, k2
   integer :: tmpdimids(NF90_MAX_VAR_DIMS)
   ! TODO: Consider to change the type of the following i-variables from double precision to integer.
   double precision, allocatable, target  :: itmpvar1D(:) !< array buffer for a single global variable slice, size: (kmx1, max(ndx(noutfile),lnx(noutfile)))
   integer,          allocatable, target  :: itmpvar1D_tmp(:)
   double precision, allocatable, target  :: itmpvar2D(:,:) !< array buffer for a single global variable slice, size: (kmx1, max(ndx(noutfile),lnx(noutfile)))
   double precision, allocatable, target  :: itmpvar2D_tmp(:,:)
   double precision, allocatable, target  :: itmpvar2D_tmpmax(:,:)
   double precision,              pointer :: itmpvarptr(:,:,:)
   ! for class map: store in 1 byte:
   integer(kind=int8),allocatable,target  :: btmpvar1D(:,:) !< array buffer for a single global variable slice, size: (kmx1, max(ndx(noutfile),lnx(noutfile)))
   integer(kind=int8),allocatable,target  :: btmpvar1D_tmp(:,:)
   integer(kind=int8),            pointer :: btmpvarptr(:,:,:,:)
   ! for others: double precision:
   double precision, allocatable, target  :: tmpvar1D(:) !< array buffer for a single global variable slice, size: (kmx1, max(ndx(noutfile),lnx(noutfile)))
   double precision, allocatable, target  :: tmpvar1D_tmp(:)
   double precision, allocatable, target  :: tmpvar2D(:,:) !< array buffer for a single global variable slice, size: (kmx1, max(ndx(noutfile),lnx(noutfile)))
   double precision, allocatable, target  :: tmpvar2D_tmp(:,:)
   double precision, allocatable, target  :: tmpvar2D_tmpmax(:,:)
   double precision, allocatable, target  :: tmpvar3D(:,:,:) !< array buffer for a single global variable slice, size: (kmx1, max(ndx(noutfile),lnx(noutfile)))
   double precision,              pointer :: tmpvarptr(:,:,:)
   character(len=4) :: fmtstr
   character(len=4096) :: tmpstr1, tmpstr2
   integer,                      allocatable :: varids(:,:)        !< Variable IDs for the selected variables in the input files. Support having different variables in differnt input files.
   integer,                      allocatable :: varids_out(:)      !< Variable IDs for the selected variables in the output file. Will typically not be equal to the var IDs in the input files, as we may not be copying *all* vars from input files.
   character(len=NF90_MAX_NAME), allocatable :: var_names(:)       !< Names of the selected variables.
   integer,                      allocatable :: var_types(:)       !< Data types of the selected variables.
   integer,                      allocatable :: var_dimids(:,:)    !< Dimension ids for selected variables, should be filled later, starting at the end.
   integer,                      allocatable :: var_timdimpos(:)   !< Position in var_dimids(1:4,iv) of time dimension (-1 if not timedep)
   integer,                      allocatable :: var_spacedimpos(:) !< Position in var_dimids(1:4,iv) of space dimension (-1 if not timedep)
   integer,                      allocatable :: var_laydimpos(:)   !< Position in var_dimids(1:4,iv) of layer dimension (-1 if not timedep)
   integer,                      allocatable :: var_kxdimpos(:)    !< Position in var_dimids(1:4,iv) of vectormax dimension (-1 if not timedep)
   integer,                      allocatable :: var_seddimpos(:)   !< Position in var_dimids(1:4,iv) of sediment dimension (-1 if not timedep)
   integer,                      allocatable :: var_ndims(:)       !< Actual number of dimensions.
   integer,                      allocatable :: var_loctype(:)     !< Spatial location type for each var (face/node/etc.)
   integer,                      allocatable :: var_wdimpos(:)     !< Position in var_dimids(1:4,iv) of layer interface dimension (-1 if not timedep)
   integer,                      allocatable :: file_ndims(:)      !< Nr. dimensions in every input file
   integer,                      allocatable :: dimids_uses(:)     !< Nr. vectormax-like dimensions that are used
   integer :: ivarcandidate, ifirstdim, ilastdim
   integer, parameter :: MAX_VAR_DIMS = 4 !< Max nr of dimensions for a single var. Support: (vectormax, layers, space, time).
   integer, dimension(MAX_VAR_DIMS) :: start_idx   !< Start index array for calling nf90_get_var(..., start=...)
   integer, dimension(MAX_VAR_DIMS) :: count_read  !< Data size array for calling nf90_get_var(..., count=...)
   integer, dimension(MAX_VAR_DIMS) :: count_write !< Data size array for calling nf90_put_var(..., count=...)
   integer :: nofill, ifill_value !< For inquiring fill values

   integer :: id_npartdim !< Dimension ID for the partitions counter
   integer :: id_part_face_start, id_part_edge_start, id_part_node_start, id_part_facebnd_start !< Var IDs for storing start index for each partition in the merged global arrays.
   integer :: id_part_face_count, id_part_edge_count, id_part_node_count, id_part_facebnd_count !< Var IDs for storing count of unique items for each partition in the merged global arrays
   integer :: Lrst_m, ifile, max_nvars
   integer :: isBndLink = 0, id_infile
   integer :: jamerge_cntv = 1 ! merge topology connectivity variables
   integer :: jaread_sep = 0   ! read the variable seperately

   character(len=NF90_MAX_NAME) :: varname, dimname
   character(len=NF90_MAX_NAME) :: facedimname            ! UGRID face / FM flow node
   character(len=NF90_MAX_NAME) :: nodedimname            ! UGRID node / FM net node
   character(len=NF90_MAX_NAME) :: netedgedimname         ! UGRID edge / FM net link
   character(len=NF90_MAX_NAME) :: netfacemaxnodesdimname ! UGRID max face-nodes / FM max net cell nods
   character(len=NF90_MAX_NAME) :: edgedimname            ! (no UGRID) / FM flow link
   character(len=NF90_MAX_NAME) :: netfacedimname         ! (no UGRID) / FM net cell
   character(len=NF90_MAX_NAME) :: timedimname            ! time
   character(len=NF90_MAX_NAME) :: laydimname             ! layer (mids)
   character(len=NF90_MAX_NAME) :: wdimname               ! layer interfaces
   integer,                      allocatable :: itimsel(:)
   double precision,             allocatable :: times(:)
   double precision,             allocatable :: timestep(:)
   logical :: isfound, needshift, exist
   integer :: size_btmp
   character(len=1) :: answer
   character*8  :: cdate
   character*10 :: ctime
   character*5  :: czone

   if (nfiles <= 1) then
      write (*,'(a)') 'Error: mapmerge: At least two input files required.'
      ierr = 12
      goto 888
   else
      if (verbose_mode) then
         write (*,'(a,i0,a)') 'Info: mapmerge: Starting merge of ', nfiles, ' files...'
      end if
   end if

   noutfile = nfiles+1
   ifile      = 1    ! Initially use the first input file as the reference file for merging
   nvars      = 0
   max_nvars  = 0
   ncids      = -1
   id_timedim = -1
   id_facedim = -1
   id_edgedim = -1
   id_time    = -1
   id_timestep= -1
   id_laydim  = -1
   id_wdim    = -1
   id_bnddim  = -1
   id_sedtotdim = -1
   id_sedsusdim = -1
   ndx        =  0
   lnx        =  0
   kmx        =  0
   nt         =  0
   ndxbnd     =  0
   netfacemaxnodes = 0

   !! 0a. Open input files
   call dfm_order_by_partition(infiles, nfiles)

   ierr = nf90_noerr
   isNetCDF4 = .false.
   do ii=1,nfiles
      ierri = ionc_open(infiles(ii), NF90_NOWRITE, ioncids(ii), iconvtype, convversion)
      if (ierri /= nf90_noerr) then
         write (*,'(a)') 'Error: mapmerge: could not open file `'//trim(infiles(ii))//'''.'
         ierr = ierri
         ncids(ii) = -1
      else
         if (iconvtype == IONC_CONV_UGRID .and. convversion >= 1.0) then
            jaugridi(ii) = 1
         else
            jaugridi(ii) = 0
         endif
      end if
      ierri = ionc_get_ncid(ioncids(ii), ncids(ii))
      ierri = nf90_inquire(ncids(ii), formatNum=formatCode)
      isNetCDF4 = (isNetCDF4 .or. formatCode == nf90_format_netcdf4 .or. formatCode == nf90_format_netcdf4_classic)
   end do

   ! check if all the map files are the same format
   jaugrid = jaugridi(1)
   do ii=2,nfiles
      if (jaugridi(ii) .ne. jaugrid) then
         write (*,'(a)') 'Error: mapmerge: map files are not the same format.'
         ierr = 1
         ncids(ii) = -1
         exit
      endif
   enddo
   if (ierr /= nf90_noerr .and. .not. verbose_mode) then
      goto 888
   else
      if (jaugrid == 1) then
         write (*,'(a,i0,a)') 'Info: mapmerge: all map files are of UGRID format.'
      else
         write (*,'(a,i0,a)') 'Info: mapmerge: all map files are of old format.'
   end if

      end if

   do ii = 1, nfiles
      ierr = ionc_get_ncid(ioncids(ii), ncids(ii))
      if (ierr /= nf90_noerr) then
         write (*,'(a)') 'Error: mapmerge: could not get ncID for file: `'//trim(infiles(ii))//'''.'
         if (.not. verbose_mode) goto 888
   end if
   enddo



   !! 0b. Open output file
   if (len_trim(outfile) == 0) then
      nlen = len_trim(infiles(1))
      n3 = index(infiles(1)(1:nlen), '.', .true.) - 1  ! pos of '.nc'
      if (n3 < 0) then
         n3 = nlen
      end if
      Lrst_m = index(infiles(1), '_rst.nc')
      if (Lrst_m > 0) then ! If they are _rst files
         n1 = index(infiles(1)(1:Lrst_m), '_0000_', .true.)
         outfile = infiles(1)(1:n1) //'merged_'// infiles(1)(n1+6:Lrst_m)//'rst.nc'
         jamerge_cntv = 0
         write (*,'(a)') 'Info: mapmerge: for *_rst.nc files, topology connectivity variables (except for "FlowLink") are not merged.'
      else
      n2  = index(infiles(1)(1:n3), '_', .true.) - 1  ! pos of '_map'
      n1  = index(infiles(1)(1:n2), '_', .true.) - 1  ! pos of '_0000'
      if (n1 >= 0) then      ! mdident_0000_typ.nc --> mdident_merged_typ.nc
         outfile = infiles(1)(1:n1) // '_merged_'//infiles(1)(n2+2:n3)//'.nc'
      else if (n2 >= 0) then ! mdident_typ.nc      --> mdident_merged_typ.nc
         outfile = infiles(1)(1:n2) // '_merged_'//infiles(1)(n2+2:n3)//'.nc'
      else                   ! mdident.nc          --> mdident_merged_map.nc
         outfile = infiles(1)(1:n3) // '_merged_map.nc'
      end if
      endif
   end if
   ! Check for possible overwrite of outfile
   inquire(file=trim(outfile), exist=exist)
   if (exist .and. .not. force) then
      write (*, '(a)', advance='no') 'mapmerge: overwrite `'//trim(outfile)//'''? (Y/N) '
      answer = ''
      read (*,'(a)') answer
      if (.not. strcmpi(answer, 'Y')) then
         goto 888
      end if
   end if

   if (isNetCDF4) then
      ierr = nf90_create(outfile, NF90_HDF5, ncids(noutfile))
   else
      ierr = nf90_create(outfile, ior(NF90_CLOBBER, NF90_64BIT_OFFSET), ncids(noutfile))
   endif
   if (ierr /= nf90_noerr) then
      write (*,'(a)') 'Error: mapmerge: could not open file for writing: `'//trim(outfile)//'''.'
      if (.not. verbose_mode) goto 888
   end if


   maxlen = 16
   if (nfiles > 0) then
      maxlen = min(maxlen, maxval(len_trim(infiles(1:nfiles))))
   end if

   !! 1a. Scan for flownode (face), flow link (edge) and time dimensions in input files
   ! Find the input file 'ifile', which has the most variables. This file will be the reference file where later we scan variables.
   allocate(file_ndims(nfiles))
   do ii = 1, nfiles
      ierr = nf90_inquire(ncids(ii), nVariables = nvars )
      if ( ierr /= nf90_noerr ) then
          write (*,'(a)') 'Error: mapmerge: no variables found in file `'//trim(infiles(ii))//'''.'
          if (.not. verbose_mode) goto 888
      else
          ierr = nf90_inquire(ncids(ii), nDimensions = file_ndims(ii) )
          if ( ierr /= nf90_noerr ) then
              write (*,'(a)') 'Error: mapmerge: no dimension found in file `'//trim(infiles(ii))//'''.'
              if (.not. verbose_mode) goto 888
          endif
      endif
      if (nvars > max_nvars) then
         max_nvars = nvars
         ifile = ii   ! Set 'ifile' the one that has the most variables
      endif
   enddo
   nDims = file_ndims(ifile) ! nDims is equal to Nr. dimensions in ifile

   allocate(dimids(nDims, nfiles+1))
   dimids = -999 ! missing
   do ii=1,nfiles
      if (ncids(ii) <= 0) then
         if (verbose_mode) then
            write (*,'(a)') 'Warning: mapmerge: Skipping scan of file `'//trim(infiles(ii))//'''.'
         end if
         cycle
      end if

      if (jaugrid == 1) then ! UGRID format
         ierr = ionc_get_mesh_count(ioncids(ii), nm) 
         im = 1 ! only 2D mesh now, TODO: 1D mesh
         do im=1,nm
            ierr = ionc_get_topology_dimension(ioncids(ii), im, topodim)
            if (topodim == 2) then
               exit ! We found the correct mesh in #im, no further searching
            end if
         end do

         ! find the mesh topology variables
         ! face -netelem
         ierr = ionc_get_dimid(ioncids(ii), im, mdim_face, id)
         ierr = nf90_inquire_dimension(ncids(ii), id, name = dimname, len = nlen)
         id_facedim(ii) = id
         facedimname    = dimname
         ndx(ii)        = nlen
         nump(ii)       = nlen
         ! net node
         ierr = ionc_get_dimid(ioncids(ii), im, mdim_node, id)
         ierr = nf90_inquire_dimension(ncids(ii), id, name = dimname, len = nlen)
         id_nodedim(ii) = id
         nodedimname    = dimname
         numk(ii)       = nlen
         ! edge
         ierr = ionc_get_dimid(ioncids(ii), im, mdim_edge, id)
         ierr = nf90_inquire_dimension(ncids(ii), id, name = dimname, len = nlen)
         id_netedgedim(ii) = id
         netedgedimname    = dimname ! note: ugrid has no flow link, netlink only (==edge)
         numl(ii)          = nlen
         ! max face node
         ierr = ionc_get_dimid(ioncids(ii), im, mdim_maxfacenodes, id)
         ierr = nf90_inquire_dimension(ncids(ii), id, name = dimname, len = nlen)
         if (id > 0) then
            dimids(id, ii) = id
         end if

         id_netfacemaxnodesdim(ii) = id
         netfacemaxnodesdimname    = dimname
         netfacemaxnodes(ii)       = nlen

         ! other dimensions
         do id = 1, nDims
            ierr = nf90_inquire_dimension(ncids(ii), id, name = dimname, len = nlen)
            if (ierr /= nf90_noerr) then
               write (*,'(a,i0,a)') 'Error: mapmerge: unable to read dimension information from file `'//trim(infiles(ii))//''' for #', id,'.'
               if (.not. verbose_mode) goto 888
            end if
            if (id /= id_facedim(ii) .and. id /= id_nodedim(ii) .and. id /= id_netedgedim(ii) .and. id /= id_netfacemaxnodesdim(ii)) then
               if (trim(dimname) == 'time') then
                  !! Time dimension
                  id_timedim(ii) = id
                  timedimname    = dimname
                  nt(ii)         = nlen
               else if (strcmpi(dimname, 'nmesh2d_layer') .or. strcmpi(dimname, 'mesh2d_nLayers')) then
                     id_laydim(ii) = id
                     laydimname    = dimname
                     kmx(ii)       = nlen
               else if (strcmpi(dimname, 'nmesh2d_interface') .or. strcmpi(dimname, 'mesh2d_nInterfaces')) then
                     id_wdim(ii) = id
                     wdimname    = dimname
               else
                  ! No special dimension, so probably just some vectormax-type dimension that
                  ! we may need later for some variables, so store it.
                  dimids(id, ii) = id ! Only stored to filter on non-missing values in def_dim loop later
                  
                  ! check if it is a dimension for sediment variables
                  if (strcmpi(dimname, 'nSedTot')) then
                     id_sedtotdim(ii) = id
                  else if (strcmpi(dimname, 'nSedSus')) then
                     id_sedsusdim(ii) = id
                  end if
               endif
            endif
         enddo
      else ! old format

      do id=1,file_ndims(ii)
         ierr = nf90_inquire_dimension(ncids(ii), id, name = dimname, len = nlen) ! NetCDF-F90 allows us to assume that the dim IDs are 1:ndims
         if (ierr /= nf90_noerr) then
            write (*,'(a,i0,a)') 'Error: mapmerge: unable to read dimension information from file `'//trim(infiles(ii))//''' for #', id,'.'
            if (.not. verbose_mode) goto 888
         end if

         if (trim(dimname) == 'nFlowElem') then
         !! Flow nodes (face) dimension
            id_facedim(ii) = id
            facedimname    = dimname
            ndx(ii)        = nlen

         else if (trim(dimname) == 'nFlowLink') then
         !! Flow links (edge) dimension
            id_edgedim(ii) = id
            edgedimname    = dimname
            lnx(ii)        = nlen

         else if (trim(dimname) == 'laydim') then ! TODO: AvD: also wdim?
            id_laydim(ii) = id
            laydimname    = dimname
            kmx(ii)       = nlen
         else if (trim(dimname) == 'wdim') then
            id_wdim(ii) = id
            wdimname    = dimname

      !! Now some Net* related dimensions (in addition to Flow*).

         else if (trim(dimname) == 'nNetElem') then
         !! Net cells (again face) dimension
            id_netfacedim(ii) = id
            netfacedimname    = dimname
            nump(ii)          = nlen


         else if (trim(dimname) == 'nNetElemMaxNode') then ! TODO: AvD: now we detect nNetElemMaxNode, but should be not change to nFlowElemMaxNode, now that facedim is the overall counter and netfacedim is hardly used anymore?
            dimids(id, ii) = id ! Store this now, because later it is just a vectormax dim, so should be available in dim filter
            id_netfacemaxnodesdim(ii) = id
            netfacemaxnodesdimname    = dimname
            netfacemaxnodes(ii)       = nlen

         else if (trim(dimname) == 'nNetNode') then
         !! Net nodes (node) dimension
            id_nodedim(ii) = id
            nodedimname    = dimname
            numk(ii)       = nlen

         else if (trim(dimname) == 'nNetLink') then
         !! Net links (again edge) dimension
            id_netedgedim(ii) = id
            netedgedimname    = dimname
            numl(ii)          = nlen

         else if (trim(dimname) == 'time') then
         !! Time dimension
            id_timedim(ii) = id
            timedimname    = dimname
            nt(ii)         = nlen
         else if (trim(dimname) == 'nFlowElemBnd') then
         !! Flow nodes (face) boundary points dimension
            id_bnddim(ii) = id
            ndxbnd(ii)   = nlen
            ! TODO: dimname needed?
            if (verbose_mode) then
               write (*,'(a)') 'Info: mapmerge: find dimension of boundary waterlevel points in file `'//trim(infiles(ii))//'''.'
            endif
         else
            ! No special dimension, so probably just some vectormax-type dimension that
            ! we may need later for some variables, so store it.
            dimids(id, ii) = id ! Only stored to filter on non-missing values in def_dim loop later
            
            ! check if it is a dimension for sediment variables
            if (strcmpi(dimname, 'nSedTot')) then
               id_sedtotdim(ii) = id
            else if (strcmpi(dimname, 'nSedSus')) then
               id_sedsusdim(ii) = id
            end if
         end if
      end do ! id
      end if
   end do ! ii


   !! 1b. Scan for variables in the file which has the most dimension (and variables).
   if (verbose_mode) then
      write (*,'(a)') 'Info: mapmerge: Scan for variables in file `'//trim(infiles(ifile))//'''.'
   endif
   ierr = nf90_inquire(ncids(ifile), nVariables = nvars )
   if ( ierr /= nf90_noerr ) then
      write (*,'(a)') 'Error: mapmerge: no variables found in file `'//trim(infiles(ifile))//'''.'
      if (.not. verbose_mode) goto 888
   endif

   if (verbose_mode) then
      write (*,'(a)') '## Selecting variables to include in merge:'
   end if

   allocate(varids(nfiles, nvars));
   allocate(varids_out(nvars))
   allocate(var_names(nvars))
   allocate(var_types(nvars))
   allocate(var_dimids(MAX_VAR_DIMS,nvars))
   allocate(var_timdimpos(nvars));   var_timdimpos   = -1
   allocate(var_spacedimpos(nvars)); var_spacedimpos = -1
   allocate(var_laydimpos(nvars));   var_laydimpos   = -1
   allocate(var_kxdimpos(nvars));    var_kxdimpos    = -1
   allocate(var_wdimpos(nvars));     var_wdimpos     = -1
   allocate(var_seddimpos(nvars));   var_seddimpos   = -1
   allocate(var_ndims(nvars));       var_ndims       =  0
   allocate(var_loctype(nvars));     var_loctype     =  0
   allocate(dimids_uses(nDims));     dimids_uses     =  0
   nvarsel = 0
   do iv = 1,nvars
      ierr = nf90_inquire_variable(ncids(ifile), iv, name=varname, xtype=vartype, ndims=nvardims, dimids=tmpdimids)

      if (nvardims == 1) then
         if (tmpdimids(1) == id_timedim(ifile) .and. trim(varname) == 'time') then
            id_time(ifile) = iv ! TODO: AvD: do this for all ii files, not only for 'ifile'
            if (verbose_mode) then
               write (*,'(a)') 'Found time variable in file `'//trim(infiles(ifile))//''': `'//trim(varname)//'''.'
            end if

            cycle ! This was time, continue searching for remaining data variables.
         elseif (tmpdimids(1) == id_timedim(ifile) .and. trim(varname) == 'timestep') then
            id_timestep(ifile) = iv ! TODO: AvD: do this for all ii files, not only for 'ifile'
            cycle
         end if
      end if


      ! It was not time, see if this is a geometry or data variable on flow nodes/links/net nodes:
      ivarcandidate = nvarsel+1
      isfound = .true.
      ilastdim = MAX_VAR_DIMS
      ifirstdim = ilastdim+1 ! dummy start: no dims detected yet.
      do id=nvardims,1,-1
         if (tmpdimids(id) == id_timedim(ifile)) then
            ifirstdim = ifirstdim-1
            var_timdimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_facedim(ifile)) then
            tmpdimids(id) = id_facedim(ifile) ! replace netfacedim by (flow)facedim
            ifirstdim = ifirstdim-1
            var_loctype(ivarcandidate) = UNC_LOC_S
            var_spacedimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_netfacedim(ifile)) then
            tmpdimids(id) = id_netfacedim(ifile)
            ifirstdim = ifirstdim-1
            var_loctype(ivarcandidate) = UNC_LOC_SN       ! UNST-1256: original UNST_LOC_S caused NetElemNode variable in merged file to be on nFlowElem dimension, whereas input is on nNetElem dimension.
            var_spacedimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_edgedim(ifile)) then
            ifirstdim = ifirstdim-1
            var_loctype(ivarcandidate) = UNC_LOC_U
            var_spacedimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_nodedim(ifile)) then
            ifirstdim = ifirstdim-1
            var_loctype(ivarcandidate) = UNC_LOC_CN
            var_spacedimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_netedgedim(ifile)) then
            ifirstdim = ifirstdim-1
            var_loctype(ivarcandidate) = UNC_LOC_L
            var_spacedimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_laydim(ifile)) then
            ! TODO: do we really not need to set the S3D/W3D here?
            ifirstdim = ifirstdim-1
          !  var_loctype(ivarcandidate) = UNC_LOC_S3D
            var_laydimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_wdim(ifile)) then
            ifirstdim = ifirstdim-1
            !var_loctype(ivarcandidate) = UNC_LOC_W
            var_wdimpos(ivarcandidate) = ifirstdim
         else if (tmpdimids(id) == id_bnddim(ifile)) then
            tmpdimids(id) = id_bnddim(ifile)
            ifirstdim = ifirstdim-1
            var_loctype(ivarcandidate) = UNC_LOC_SBND
            var_spacedimpos(ivarcandidate) = ifirstdim
         else
            if (var_kxdimpos(ivarcandidate) == -1) then
               ifirstdim = ifirstdim-1
               var_kxdimpos(ivarcandidate) = ifirstdim
               ! count how many times this dimension is used
               id_infile = tmpdimids(id)
               dimids_uses(id_infile) = dimids_uses(id_infile) + 1
               if (tmpdimids(id) == id_sedtotdim(ifile) .or. tmpdimids(id) == id_sedsusdim(ifile)) then
                  var_seddimpos(ivarcandidate) = ifirstdim
               end if
            else
               if (verbose_mode) then
                  write (*,'(a)')           'Error: mapmerge: detected more than one vectormax dimension for `'//trim(varname)//''':'
                  write (*,'(a,i0,a,i0,a)') '       current: ', id, ', other: ', var_kxdimpos(ivarcandidate), '. Skipping this variable.'
               end if
               isfound = .false.
               exit ! Stop scanning any remaining dimensions for this var.
            end if
         end if
         var_dimids(ifirstdim,ivarcandidate) = tmpdimids(id) ! for debugging only: temp store the dimids from file #1
      end do ! id

      ! We can only merge a variable across multiple domains *if* it is defined on space locations (face/edge/node)
      ! *or* if it is a special time-related variable. All others impossible to merge.
      if (var_spacedimpos(ivarcandidate) <= 0 .and. var_timdimpos(ivarcandidate) <= 0 .and. nvardims .ne. 0) then
         if (nvardims == 1 .and. (var_laydimpos(ivarcandidate) > 0 .or. var_wdimpos(ivarcandidate) > 0) .and. verbose_mode) then
            write (*,'(a)')'Info: mapmerge: Variable `'//trim(varname)//''' will be copied from one of the input files to the merged file.'
         else
            if (verbose_mode) then
               write (*,'(a)') 'Error: mapmerge: detected that variable `'//trim(varname)//''': is not defined on space' &
               //'locations, and is not a special time-related variable. Skipping this variable.'
            end if
            ! Make decrement -1 to all dimensions of the skipped variable, i.e. they are used one time less
            ! dimids_uses(id_infile) needs to be decremented -1 here for the kx dim
            do id=nvardims,1,-1
               id_infile = tmpdimids(id)
               dimids_uses(id_infile) = dimids_uses(id_infile) - 1
            enddo
            var_timdimpos(ivarcandidate)   = -1
            var_spacedimpos(ivarcandidate) = -1
            var_kxdimpos(ivarcandidate)    = -1
            var_laydimpos(ivarcandidate)   = -1
            var_wdimpos(ivarcandidate)     = -1
            isfound = .false.
         end if
      end if

      if (isfound) then
         nvarsel = nvarsel + 1 ! === ivarcandidate
         ! NOTE: Use variable ID from file #1 and assume that all input files contain the same variables, and as such the same consecutive variable IDs.
         varids(ifile, nvarsel) = iv
         var_names(nvarsel) = varname
         var_types(nvarsel) = vartype
         ! NOTE: all dimension positions were already stored, actual dimension ids not,
         ! because those are only needed for output file, will be done later.
         var_ndims(nvarsel) = ilastdim-ifirstdim+1
         if (verbose_mode) then
            tmpstr1 = ''
            nlen=0
            do id=ifirstdim,ilastdim
               ierr = nf90_inquire_dimension(ncids(ifile), var_dimids(id,nvarsel), name=tmpstr2)
               mlen = len_trim(tmpstr2)
               tmpstr1 = tmpstr1(1:nlen)//', '//tmpstr2(1:mlen)
               nlen = nlen + 2 + mlen
            end do
            write (*,'(a,i3,a,a)') ' * ', nvarsel, ': ', trim(varname)//'('//tmpstr1(3:nlen)//')'
         end if
         ! Set IDs of current variable to other input files, if those files does not have this variable, then set ID to -1
         do ii = 1, nfiles
            if (ii .ne. ifile) then
               ierr = nf90_inq_varid(ncids(ii), varname, varids(ii, nvarsel))
               if (ierr .ne. nf90_noerr) then
                  if (verbose_mode) then
                     write (*,'(a)') 'Info: mapmerge: file `'//trim(infiles(ii))//'''does not have variable `'//trim(varname)//'''.'
                  endif
                  ierr = 0
                  varids(ii,nvarsel) = -1
               endif
            endif
         enddo
      end if
   end do ! iv

   if (verbose_mode) then
      if (nvarsel == 0) then
         write (*,'(a)') 'Error: mapmerge: no variables found in file `'//infiles(ifile)//'''.'
         if (.not. verbose_mode) goto 888
      end if
   end if

   !! 2a. Write top level attributes to file as a copy from input file.
   ierr = ncu_copy_atts(ncids(ifile), ncids(noutfile), nf90_global, nf90_global)

   ! We're making history here
   tmpstr1 = ''
   ierr = nf90_get_att(ncids(ifile), nf90_global, 'history', tmpstr1)
   if (ierr /= nf90_noerr) then
      mlen = 0
   else
      mlen = min(len(tmpstr1), len_trim(tmpstr1)+1)
      tmpstr1(mlen:mlen) = char(10) ! Prepare for our extra history line below by adding newline now already.
   end if

   call get_command (tmpstr2, nlen, ierr)
   if (ierr < 0) then ! command did not fit into string, abbreviate it.
      tmpstr2(len(tmpstr2)-2:len(tmpstr2)) = '...'
   end if

   call date_and_time(cdate, ctime, czone)
   ierr = nf90_put_att(ncids(noutfile), nf90_global, 'history', &
      tmpstr1(1:mlen)// &
      cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)//': '// &
      tmpstr2(1:nlen))

   ! Don't set 'date_created', hopefully it was in the original input files, and then we copy it and leave it at that source value.
   ierr = nf90_put_att(ncids(noutfile), nf90_global,  'date_modified', cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5))


   !! 2b. Define time dim&var with attributes in outputfile as a copy from input file.
   ierr = nf90_def_dim(ncids(noutfile), trim(timedimname), nf90_unlimited, id_timedim(noutfile))
   ierr = nf90_def_var(ncids(noutfile), 'time', nf90_double,    (/ id_timedim(noutfile) /), id_time   (noutfile))
   ierr = ncu_copy_atts(ncids(ifile), ncids(noutfile), id_time(ifile), id_time(noutfile))
   if (isNetCDF4) then
      ierr = ncu_copy_chunking_deflate(ncids(ifile), ncids(noutfile), id_time(ifile), id_time(noutfile))
   endif

   ierr = nf90_def_var(ncids(noutfile), 'timestep', nf90_double,    (/ id_timedim(noutfile) /), id_timestep(noutfile))
   ierr = ncu_copy_atts(ncids(ifile), ncids(noutfile), id_timestep(ifile), id_timestep(noutfile))
   if (isNetCDF4) then
      ierr = ncu_copy_chunking_deflate(ncids(ifile), ncids(noutfile), id_timestep(ifile), id_timestep(noutfile))
      if (id_timestep(ifile) < 0) then
         ! avoid very large chuncksize on Linux for timestep
         ierr = nf90_def_var_chunking(ncids(noutfile), id_timestep(noutfile), nf90_chunked, [512])
         if (ierr /= 0) write(*,*) 'nf90_def_var_chunking failed for var timestep'
      endif
   endif

   !! 3. Construct merged flow geometry (using proper cellmasks, global numbers, etc.
   !! 3a. Count dimensions (removing partition overlap) for merged flow nodes (faces) and flow links (edges).
   nfacecount    = 0 !< running total of ndx(ii) while iterating the files
   nedgecount    = 0 !< running total of lnx(ii) while iterating the files
   nnodecount    = 0 !< running total of numk(ii) while iterating the files
   !nnetfacecount = 0 !< running total of nump(ii) while iterating the files
   nnetedgecount = 0 !< running total of numl(ii) while iterating the files
   nfaceglob     = 0 !< total number of flow nodes (faces) without duplicates
   nedgeglob     = 0 !< total number of flow links (edges) without duplicates
   nnodeglob     = 0 !< total number of net nodes (nodes) without duplicates
   !nnetfaceglob  = 0 !< total number of net cells (faces) without duplicates
   nnetedgeglob  = 0 !< total number of net links (edges) without duplicates
   nbndglob      = 0 !< total number of boundary waterlevel points without duplicates
   nbndcount     = 0 !< total number of boundary waterlevel points

   ndxc = sum(ndx(1:nfiles))
   call realloc(face_domain, ndxc, keepExisting=.false., fill=-1)
   call realloc(face_c2g,    ndxc, keepExisting=.false.)
   call realloc(face_g2c,    ndxc, keepExisting=.false.)

   lnxc = sum(lnx(1:nfiles))
   call realloc(edge_domain, lnxc, keepExisting=.false., fill=-1)
   call realloc(ln, (/ 2, lnxc /), keepExisting=.false.)
   call realloc(edge_g2c,    lnxc, keepExisting=.false.)
   call realloc(edge_c2g,    lnxc, keepExisting=.false., fill=-1)

   netfacemaxnodesg = maxval(netfacemaxnodes(1:nfiles))
   call realloc(netfacenodes, (/ netfacemaxnodesg, sum(nump(1:nfiles)) /), keepExisting=.false., fill=-1)
   call realloc(netfaceedges, (/ netfacemaxnodesg, sum(nump(1:nfiles)) /), keepExisting=.false., fill=-1)
   call realloc(nfaceedges, sum(nump(1:nfiles)), keepExisting=.false., fill=0)

   numkc = sum(numk(1:nfiles))
   call realloc(node_domain, numkc, keepExisting=.false., fill=huge(1))
   call realloc(node_g2c,    numkc, keepExisting=.false.)
   call realloc(node_c2g,    numkc, keepExisting=.false., fill=-1)
   call realloc(node_x, numkc, keepExisting=.false.)
   call realloc(node_y, numkc, keepExisting=.false.)
   call realloc(node_faces, (/ netnodemaxface+1, numkc/), keepExisting=.false., fill= -1)
   ! Prepare node_faces array: the last value for each nodes is a counter of how many faces are surrounding this node
   node_faces(netnodemaxface+1, 1: numkc) = 0

   numlc = sum(numl(1:nfiles))
   call realloc(netedge_domain, numlc, keepExisting=.false., fill=-1)
   call realloc(edgenodes, (/ 2, numlc /), keepExisting=.false.)
   call realloc(netedge_g2c,    numlc, keepExisting=.false.)

   ndx_bndc = sum(ndxbnd(1:nfiles))
   call realloc(facebnd_domain, ndx_bndc, keepExisting=.false., fill =-1)
   call realloc(netedge_c2g,    numlc, keepExisting=.false., fill=-1)
   call realloc(netedgefaces, (/2, numlc/), keepExisting=.false., fill=-1)
   call realloc(edge_x, numlc, keepExisting=.false.)
   call realloc(edge_y, numlc, keepExisting=.false.)

   if (verbose_mode) then
      write (*,'(a)') '## Scanning input files for dimensions...'

      write (fmtstr, '(a,i0)') 'a', maxlen
      write (*,'('//trim(fmtstr)//',a3,4(a13),a8)') 'File', &
         ' : ', '   flow nodes', ' | flow links', ' |  net nodes', ' |  net links', ' | times'
   end if
   do ii=1,nfiles
      !! 3a.1: handle flow nodes (faces)
      nfaceglob0 = nfaceglob
      face_domain(nfacecount+1:nfacecount+ndx(ii)) = ii-1 ! Just set default domain if FlowElemDomain not present.
      if (jaugrid == 0) then
         ierr = nf90_inq_varid(ncids(ii), 'FlowElemDomain', id_facedomain)
      else
         ierr = nf90_inq_varid(ncids(ii), 'mesh2d_flowelem_domain', id_facedomain)
      endif
      if (ierr == nf90_noerr) then
         ierr = nf90_get_var(ncids(ii), id_facedomain, face_domain(nfacecount+1:nfacecount+ndx(ii)), count=(/ ndx(ii) /))
         if (ierr /= nf90_noerr) then
            write (*,'(a)') 'Error: mapmerge: could not retrieve FlowElemDomain from `'//trim(infiles(ii))//'''. '
            if (.not. verbose_mode) goto 888
         end if
      else
         ! no problem if FlowElemDomain is missing: just include all elems (default ii-1 was set above).
      end if

      if (ierr == nf90_noerr) then
         if (jaugrid == 0) then
            ierr = nf90_inq_varid(ncids(ii), 'FlowElemGlobalNr', id_faceglobnr)
         else
            ierr = nf90_inq_varid(ncids(ii), 'mesh2d_flowelem_globalnr', id_faceglobnr)
            ! TODO: UNST-1794: generalize to multiple meshes in the UGRID file, a bit like the following:
            ! ierr = ionc_inq_varid(ncids(ii), meshid, 'flowelem_globalnr', id_faceglobnr)
         end if
      end if
      if (ierr == nf90_noerr) then
         ierr = nf90_get_var(ncids(ii), id_faceglobnr, face_c2g(nfacecount+1:nfacecount+ndx(ii)), count=(/ ndx(ii) /))
         if (ierr /= nf90_noerr) then
            write (*,'(a)') 'Error: mapmerge: could not retrieve FlowElemGlobalNr from `'//trim(infiles(ii))//'''. '
            if (.not. verbose_mode) goto 888
         end if
      else
         ! no problem if FlowElemGlobalNr is missing: just include all elems: global nr is equal to 'concat-index'.
         do ip=1,ndx(ii)
            face_c2g(nfacecount+ip) = nfacecount+ip
         end do
      end if

      ! Count the actual unique flow nodes (to get rid of partition overlap)
      do ip=1,ndx(ii)
         if (face_domain(nfacecount+ip) == ii-1) then
            nfaceglob = nfaceglob+1
            ifaceglob = face_c2g(nfacecount+ip)
            face_g2c(ifaceglob) = nfacecount + ip
         end if
      end do
      ndxg(ii)   = nfaceglob-nfaceglob0

      !! 3a.2: handle flow links (edges)
      if (jaugrid==0) then ! TODO: zhao: why is this if different from the one for faces and nodes?
      nedgeglob0 = nedgeglob
      ierr = nf90_inq_varid(ncids(ii), 'FlowLink', id_edgefaces)
      if (ierr == nf90_noerr) then
         ierr = nf90_get_var(ncids(ii), id_edgefaces, ln(:,nedgecount+1:nedgecount+lnx(ii)), count=(/ 2,lnx(ii) /))
      else
         write (*,'(a)') 'Warning: mapmerge: could not retrieve FlowLink from `'//trim(infiles(ii))//'''. '
         if (.not. verbose_mode) goto 888
      end if

      ! Count the actual unique flow links (to get rid of partition overlap, and also of duplicate boundary links)
      do ip=1,lnx(ii)
         ! For each link, take 2nd point (such that boundary links will be uniquely
         ! owned by the domain who owns the internal flow node of that link.)
         n1 = ln(1,nedgecount+ip) ! Could be a boundary point
         n2 = ln(2,nedgecount+ip)
         if (n1 > ndx(ii)) then
            idom = ii - 1 ! Boundary mirrornodes are always owned by the domain itself.
            isBndLink = 1 ! Mark the boundary link
         else
            idom = face_domain(nfacecount+n1)
         end if

         idom = min(idom, face_domain(nfacecount+n2))

         if (isBndLink == 1 .and. face_domain(nfacecount+n2) .ne. ii-1) then
         ! If this boundary link connects an interior flownode which does not belong to the current subdomain
            idom = - 999
         endif
         edge_domain(nedgecount+ip) = idom
         if (idom == ii-1) then
            nedgeglob = nedgeglob+1
            edge_g2c(nedgeglob) = nedgecount + ip
               edge_c2g(nedgecount+ip) = nedgeglob
         end if
         isBndLink = 0
      end do
      lnxg(ii)   = nedgeglob-nedgeglob0
      end if

      !! 3a.3: handle net nodes (nodes)
      nnodeglob0 = nnodeglob
      if (jaugrid==0) then
         ierr = nf90_inq_varid(ncids(ii), 'NetElemNode', id_netfacenodes)
      else
         ierr = nf90_inq_varid(ncids(ii), 'mesh2d_face_nodes', id_netfacenodes)
      endif
      if (ierr == nf90_noerr) then
         ierr = ncu_inq_var_fill(ncids(ii), id_netfacenodes, nofill, ifill_value)
         call realloc (netfacenodesl, (/ netfacemaxnodes(ii), nump(ii) /), keepExisting = .false.)
         ierr = nf90_get_var(ncids(ii), id_netfacenodes, netfacenodesl(:,:), count=(/ netfacemaxnodes(ii), nump(ii) /))
         do ip=1,nump(ii)
           netfacenodes(1:netfacemaxnodes(ii),nfacecount+ip) = netfacenodesl(1:netfacemaxnodes(ii),ip)
           ! generate node_faces: the faces that surround a node
           do ik =1, netfacemaxnodes(ii) ! for its every node
               k1 = netfacenodesl(ik,ip)
               if (k1 .ne. -1 .and. k1 .ne. ifill_value) then
                   nfaces = node_faces(netnodemaxface+1, nnodecount+k1) + 1
                   node_faces(netnodemaxface+1, nnodecount+k1) = nfaces ! update the counter
                   node_faces(nfaces,nnodecount+k1) = nfacecount+ip
               end if
           end do
         end do

         ! read coordinates of net nodes
         if (jaugrid==0) then
            ierr = nf90_inq_varid(ncids(ii), 'NetNode_x', id_nodex)
            ierr = nf90_inq_varid(ncids(ii), 'NetNode_y', id_nodey)
            ierr = nf90_get_var(ncids(ii), id_nodex, node_x(nnodecount+1:nnodecount+numk(ii)))
            ierr = nf90_get_var(ncids(ii), id_nodey, node_y(nnodecount+1:nnodecount+numk(ii)))
         else
            ierr = nf90_inq_varid(ncids(ii), 'mesh2d_node_x', id_nodex)
            ierr = nf90_inq_varid(ncids(ii), 'mesh2d_node_y', id_nodey)
            ierr = nf90_get_var(ncids(ii), id_nodex, node_x(nnodecount+1:nnodecount+numk(ii)))
            ierr = nf90_get_var(ncids(ii), id_nodey, node_y(nnodecount+1:nnodecount+numk(ii)))
      end if
      if (ierr /= nf90_noerr) then
            jamerge_cntv = 0
            write (*,'(a)') 'Warning: mapmerge: could not retrieve coordinates of net nodes from `'//trim(infiles(ii))//'''. '
         end if
      else
         if (jaugrid==0) then
            write (*,'(a)') 'Warning: mapmerge: could not retrieve NetElemNode from `'//trim(infiles(ii))//'''. '
         else
            write (*,'(a)') 'Warning: mapmerge: could not retrieve mesh2d_face_nodes from `'//trim(infiles(ii))//'''. '
         end if
         if (.not. verbose_mode) goto 888
      end if

      ! Identify the node domain based on the already processed net cells
      do ip=1,nump(ii)
         do ik=1,netfacemaxnodes(ii)
            k1 = netfacenodes(ik, nfacecount+ip)
            if (k1 == -1 .or. k1 == ifill_value) then
               exit
            end if
            ! Owner of the node will be the lowest domain number of any of the cells surrounding that node.
            node_domain(nnodecount+k1) = min(node_domain(nnodecount+k1), face_domain(nfacecount+ip))
         end do
      end do
      !numpg(ii)   = nnetfaceglob-nnetfaceglob0

      ! Count the actual unique nodes (to get rid of partition overlap)
      do ip=1,numk(ii)
         idom = node_domain(nnodecount+ip)

         if (idom == ii-1) then ! Second check is to identify orphan nodes (in case no 1D netcells were available on file)
            nnodeglob = nnodeglob+1 ! TODO: orphan nodes are not handled correctly yet for 1D channel strings (duplicates?)
            node_g2c(nnodeglob) = nnodecount + ip
            node_c2g(nnodecount + ip) = nnodeglob
         end if
      end do
      numkg(ii)   = nnodeglob-nnodeglob0

      !! 3a.4: handle net edges (also edges)
      nnetedgeglob0 = nnetedgeglob
      if (jaugrid==0) then
         ierr = nf90_inq_varid(ncids(ii), 'NetLink', id_edgenodes)
      else
         ierr = nf90_inq_varid(ncids(ii), 'mesh2d_edge_nodes', id_edgenodes)
      endif

      if (ierr == nf90_noerr) then
         ierr = nf90_get_var(ncids(ii), id_edgenodes, edgenodes(:,nnetedgecount+1:nnetedgecount+numl(ii)), count=(/ 2, numl(ii) /))
      else
         if (jaugrid==0) then
            write (*,'(a)') 'Warning: mapmerge: could not retrieve NetLink from `'//trim(infiles(ii))//'''. '
         else
            write (*,'(a)') 'Warning: mapmerge: could not retrieve mesh2d_edge_nodes from `'//trim(infiles(ii))//'''. '
         end if
         if (.not. verbose_mode) goto 888
      end if

      if (jaugrid==0) then
         ierr = nf90_inq_varid(ncids(ii), 'ElemLink', id_netedgefaces)
      else
         ierr = nf90_inq_varid(ncids(ii), 'mesh2d_edge_faces', id_netedgefaces)
      end if
      if (ierr == nf90_noerr) then
         ierr = nf90_get_var(ncids(ii), id_netedgefaces, netedgefaces(:,nnetedgecount+1:nnetedgecount+numl(ii)), count=(/ 2, numl(ii) /))
      else
         if (jaugrid==0) then
            write (*,'(a)') 'Warning: mapmerge: could not retrieve ElemLink from `'//trim(infiles(ii))//'''. '
         else
            write (*,'(a)') 'Warning: mapmerge: could not retrieve mesh2d_edge_faces from `'//trim(infiles(ii))//'''. '
         end if
         if (.not. verbose_mode) goto 888
      end if

      if (jaugrid==0) then
         ierr = nf90_inq_varid(ncids(ii), 'NetElemLink', id_netfaceedges)
         if (ierr == nf90_noerr) then
            ierr = nf90_get_var(ncids(ii), id_netfaceedges, netfaceedges(1:netfacemaxnodes(ii),nfacecount+1:nfacecount+nump(ii)), count=(/ netfacemaxnodes(ii), nump(ii) /))
            if (ierr /= nf90_noerr) then
               jamerge_cntv = 0
               write (*,'(a)') 'Warning: mapmerge: could not retrieve NetElemLink from `'//trim(infiles(ii))//'''. '
            end if
         else
            write (*,'(a)') 'Warning: mapmerge: could not retrieve NetElemLink from `'//trim(infiles(ii))//'''. '
            if (.not. verbose_mode) goto 888
         end if
      else ! UGRID map file does not contain _face_edges, build it here: netfaceedges
         do iedge = 1, numl(ii)
            do ikk = 1, 2
               ic = netedgefaces(ikk,iedge+nnetedgecount)
               if (ic > 0) then
                  iface = ic + nfacecount
                  nfaceedges(iface) = nfaceedges(iface) +1
                  netfaceedges(nfaceedges(iface), iface) = iedge
               end if
            end do
         end do
      end if

      if (jaugrid ==0) then
         ! read coordinates of NetLinks
         ierr = nf90_inq_varid(ncids(ii), 'NetLink_xu', id_edgex)
         ierr = nf90_inq_varid(ncids(ii), 'NetLink_yu', id_edgey)
         ierr = nf90_get_var(ncids(ii), id_edgex, edge_x(nnetedgecount+1:nnetedgecount+numl(ii)))
         ierr = nf90_get_var(ncids(ii), id_edgey, edge_y(nnetedgecount+1:nnetedgecount+numl(ii)))
      else
         ierr = nf90_inq_varid(ncids(ii), 'mesh2d_edge_x', id_edgex)
         ierr = nf90_inq_varid(ncids(ii), 'mesh2d_edge_y', id_edgey)
         ierr = nf90_get_var(ncids(ii), id_edgex, edge_x(nnetedgecount+1:nnetedgecount+numl(ii)))
         ierr = nf90_get_var(ncids(ii), id_edgey, edge_y(nnetedgecount+1:nnetedgecount+numl(ii)))
      end if
      if (ierr /= nf90_noerr) then
         jamerge_cntv = 0
         write (*,'(a)') 'Warning: mapmerge: could not retrieve coordinates of net edges from `'//trim(infiles(ii))//'''. '
      end if

      ! Identify the net link node domain based on the already processed net nodes
      do ip=1,numl(ii)
         k1 = edgenodes(1,nnetedgecount+ip)
         k2 = edgenodes(2,nnetedgecount+ip)
         ! NOTE: AvD: by taking the MAX below, I believe this also guarantees that one and
         !            the same domain owns both the net link and the associated flow link.
         idom = max(node_domain(nnodecount+k1), node_domain(nnodecount+k2))

         netedge_domain(nnetedgecount+ip) = idom

         if (idom == ii-1) then
            nnetedgeglob = nnetedgeglob+1
            netedge_g2c(nnetedgeglob) = nnetedgecount + ip
            netedge_c2g(nnetedgecount + ip) = nnetedgeglob
         end if
      end do
      numlg(ii)   = nnetedgeglob-nnetedgeglob0

      !! 3a.5: handle boundary waterlevel points
      if (ndxbnd(ii) > 0) then
         facebnd_domain(nbndcount+1:nbndcount+ndxbnd(ii)) = ii-1
         nbndglob = nbndglob + ndxbnd(ii)
      else if (verbose_mode) then
         write (*,'(a)') 'Info: mapmerge: no waterlevel boundary in `'//trim(infiles(ii))//'''. '
      endif

      ! Intentional: all of these need to be done at very last instant:
      nedgecount    = nedgecount    + lnx(ii)
      nfacecount    = nfacecount    + ndx(ii)
      nnodecount    = nnodecount    + numk(ii)
      nnetedgecount = nnetedgecount + numl(ii)
      !nnetfacecount = nnetfacecount + nump(ii)
      nbndcount     = nbndcount     + ndxbnd(ii)

      if (verbose_mode) then
         nlen = len_trim(infiles(ii))
         plen = 3*max(0,sign(1,nlen-maxlen-1))

         write (fmtstr, '(a,i0)') 'a', maxlen
         write (*,'('//trim(fmtstr)//',a3,i7,3(i13),i8)') repeat('.', plen)//infiles(ii)(max(1,nlen-maxlen+1)+plen:nlen)//repeat(' ', max(0,maxlen-nlen-plen)), &
            ' : ', ndx(ii), lnx(ii), numk(ii), numl(ii), nt(ii)
         write (*,'('//trim(fmtstr)//',a3,4(i13))')    repeat(' ', maxlen-7)//'Removed', &
            ' : ', (ndxg(ii)-ndx(ii)), (lnxg(ii)-lnx(ii)), (numkg(ii)-numk(ii)), (numlg(ii)-numl(ii))
      end if
   end do ! ii

   if (jamerge_cntv == 1) then
   !! fulfill node_c2g, because in domain that is larger than 0000, there are nodes which are in this domain but
   !  their domain numbers are another domain.
    do ii = 2, nfiles
        nfacecount = sum(nump(1:ii-1))
        nnodecount = sum(numk(1:ii-1))
        do ik=1,numk(ii)
            if (node_c2g(nnodecount+ik)==-1) then
                jafound = 0
                do ikk = 1, node_faces(netnodemaxface+1, nnodecount+ik)
                    ic = node_faces(ikk,nnodecount+ik) ! index of one surrounding cell
                    if (ic .ne. -1) then
                       if (face_domain(ic) .ne. ii-1 .and. face_domain(ic) == node_domain(nnodecount+ik)) then ! cell ic is not in current domain, and is in the same domain with node ik
                           ifaceglob = face_c2g(ic)
                           ifacec = face_g2c(ifaceglob) ! and this is now from the OTHER domain.
                           ! in which domain (iii) does iface belong
                           do iii = 1, nfiles
                               if (ifacec > sum(nump(1:iii-1)) .and. ifacec <= sum(nump(1:iii))) then
                                   ifacefile = iii
                                   exit
                               end if
                           end do
                           ! Loop on all the nodes of face ifacec, to find the node which has the same coordinates with node ik from file ii
                           do im=1,netfacemaxnodes(ifacefile)
                               k1 = netfacenodes(im, ifacec)
                               if (k1 .ne. -1 .and. k1 .ne. ifill_value) then
                                   k1c = k1 + sum(numk(1:ifacefile-1)) ! concatinated index of nodes
                                   xx = node_x(k1c)
                                   yy = node_y(k1c)
                                   if (abs(node_x(ik+nnodecount)-xx)<1d-10 .and. abs(node_y(ik+nnodecount)-yy)<1d-10) then
                                       node_c2g(ik+nnodecount) = node_c2g(k1c)
                                       jafound = 1
                                       exit
                                   end if
                               end if
                           end do
                       end if
                    end if
                end do
                if (jafound == 0 .and. verbose_mode) then
                    write (*,'(a,i0,a)') 'Warning: mapmerge: node_c2g: could not find global number for node # ', ik ,' of file `'//trim(infiles(ii))//'''. '
                end if
            end if
        end do
    end do

    !! fulfill netedge_c2g, because in domain larger than 0000, there are netedges which are in this domain
    !  but their domain numbers are another domain.
    do ii = 2, nfiles
        netedgecount = sum(numl(1:ii-1))
        nfacecount = sum(nump(1:ii-1))
        do ip = 1, numl(ii)
            k1 = netedgefaces(1, netedgecount+ip)  ! flowelem that edge L connects
            k2 = netedgefaces(2, netedgecount+ip)
            if (k1.ne.0 .and. k2.ne. 0) then       ! When edge L is not on the boundary
                g1 = face_domain(nfacecount+k1)    ! domain number of this flowelem
                g2 = face_domain(nfacecount+k2)
                if (g1 .ne. g2) then               ! if edge L lies on the boundary of two different domains
                    if (g1==ii-1 .and. g1>g2) then ! decide which flowelem is in the current domain, which is not
                        ifacein = k1
                        ifaceout= k2
                    else if (g2==ii-1 .and. g2>g1) then
                        ifacein = k2
                        ifaceout= k1
                    else
                        cycle
                    end if

                    ifaceglob = face_c2g(ifaceout+nfacecount) ! for the flowelem which is in another domain with smaller domain number
                    ifacec = face_g2c(ifaceglob)              ! and this is now from the OTHER domain.
                    ! in which domain (iii) does iface belong
                    do iii = 1, nfiles
                        if (ifacec>sum(nump(1:iii-1)) .and. ifacec < sum(nump(1:iii))) then
                            ifacefile = iii
                            exit
                        end if
                    end do
                    ! Loop on all netedges of face ifacec
                    do im = 1, netfacemaxnodes(ifacefile)
                        k1 = netfaceedges(im, ifacec)
                        if (k1 .ne. -1 .and. k1 .ne. ifill_value) then
                           if (k1 .ne. -1) then
                               k1c= k1 + sum(numl(1:ifacefile-1))! concatinated index of the edge
                               xx = edge_x(k1c)
                               yy = edge_y(k1c)
                               if (abs(edge_x(ip+netedgecount)-xx)<1d-10 .and. abs(edge_y(ip+netedgecount)-yy)<1d-10) then
                                   netedge_c2g(ip+netedgecount) = netedge_c2g(k1c)
                                   exit
                               end if
                           end if
                        end if
                    end do
                end if
            end if
        end do
    end do
   end if
   nkmxglob = kmx(1)
   do ii = 2,nfiles
      if (kmx(ii) .ne. nkmxglob) then
         write (*,'(a,i0,a,i0,a)') 'Error: mapmerge: Numbers of layers are different in files: ', kmx(ii), 'layers in file`'//trim(infiles(ii))//''' and', &
                                    nkmxglob, 'layers in other files.'
         if (.not. verbose_mode) goto 888
      endif
   enddo

   ndx (noutfile) = nfaceglob
   lnx (noutfile) = nedgeglob
   numk(noutfile) = nnodeglob
   numl(noutfile) = nnetedgeglob
   kmx (noutfile) = nkmxglob
   ndxbnd(noutfile) = nbndglob

   if (verbose_mode) then
      nlen = len_trim(outfile)
      plen = 3*max(0,sign(1,nlen-maxlen-1))

      write (fmtstr, '(a,i0)') 'a', maxlen
      write (*,'('//trim(fmtstr)//',a3,4(i13))')    repeat(' ', maxlen-7)//'Net sum', &
         ' : ', sum(ndxg(1:nfiles)), sum(lnxg(1:nfiles)), sum(numkg(1:nfiles)), sum(numlg(1:nfiles))
      write (*,'('//trim(fmtstr)//',a3,4(i13))')    repeat(' ', maxlen-11)//'Check count', &
         ' : ', ndx(noutfile), lnx(noutfile), numk(noutfile), numl(noutfile)
   end if

   !! 3b. dimensions for merged flow nodes (faces) and flow links (edges), same for net nodes + links.
   if (jaugrid==0) then
      ierr = nf90_def_dim(ncids(noutfile), trim(netfacedimname), ndx(noutfile), id_netfacedim(noutfile)) ! UNST-1256: temp fix, pending proper UGRID support in UNST-1134.
      ierr = nf90_def_dim(ncids(noutfile), trim(facedimname), ndx(noutfile), id_facedim(noutfile))
      ierr = nf90_def_dim(ncids(noutfile), trim(edgedimname), lnx(noutfile), id_edgedim(noutfile))
      if (kmx(noutfile) > 0) then
         ierr = nf90_def_dim(ncids(noutfile), trim(laydimname), kmx(noutfile), id_laydim(noutfile))
         ierr = nf90_def_dim(ncids(noutfile), trim(wdimname), kmx(noutfile)+1, id_wdim(noutfile))
      end if
      ierr = nf90_def_dim(ncids(noutfile), trim(nodedimname), numk(noutfile), id_nodedim(noutfile))
      ierr = nf90_def_dim(ncids(noutfile), trim(netedgedimname), numl(noutfile), id_netedgedim(noutfile))
      ierr = nf90_def_dim(ncids(noutfile), 'nFlowElemBnd', ndxbnd(noutfile), id_bnddim(noutfile))! TODO: Add if ndxbnd > 0
   else
      ierr = nf90_def_dim(ncids(noutfile), trim(facedimname), ndx(noutfile),  id_facedim(noutfile))
      ierr = nf90_def_dim(ncids(noutfile), trim(netedgedimname), numl(noutfile), id_netedgedim(noutfile))
      ierr = nf90_def_dim(ncids(noutfile), trim(nodedimname), numk(noutfile), id_nodedim(noutfile))
      if (kmx(noutfile) > 0) then
         ierr = nf90_def_dim(ncids(noutfile), trim(laydimname), kmx(noutfile), id_laydim(noutfile))
         ierr = nf90_def_dim(ncids(noutfile), trim(wdimname), kmx(noutfile)+1, id_wdim(noutfile))
      end if
   endif

   !! 4. Define all variables (grid + data), including any remaining dimensions
   !! 4a. Simply copy all remaining dimensions (probably vectormax-like) to output file.
   do id=1,ndims
      if (dimids(id,ifile) > 0) then ! For now, just copy the vectormax dimids (if any) from file #1 to output file. Assume same length in all files.
         ierr = nf90_inquire_dimension(ncids(ifile), dimids(id,ifile), name = dimname, len = nlen)
         if (dimids_uses(id) <= 0) then
            write (*,'(a)') 'Info: mapmerge: Dimension `'//trim(dimname)//''' is not merged because no merged variable uses it. '
            cycle
         endif
         ! When one file has only triangular mesh and one file has only rectangular mesh, then a variable, e.g. 'NetElemNode'
         ! has dimension 3 and 4, respectively. Then this variable in the target merged map should have dimension nlen=4,
         ! which is the maximum (UNST-1842).
         if (dimname == 'nNetElemMaxNode' .or. dimname == 'max_nmesh2d_face_nodes'  .or. dimname == 'mesh2d_nMax_face_nodes' .or. dimname=='nFlowElemContourPts') then
            nlen = maxval(netfacemaxnodes)
         end if

         if (ierr == nf90_noerr) then
            ierr = nf90_def_dim(ncids(noutfile), trim(dimname), nlen, dimids(id, noutfile))
         end if
         if (ierr /= nf90_noerr) then
            write (*,'(a,i0,a,i0)') 'Error: mapmerge: Could not copy dimension #', dimids(id,ifile), &
                                    ' from `'//trim(infiles(ifile))//''' into output file `'//trim(outfile)//'''. Error: ', ierr
            if (.not. verbose_mode) goto 888
         end if
      end if
   end do

   !! 4b. Define all variables, based on previously detected dimension information.
   ! var_dimids = -1 ! NOTE: can't do this, as we still need the vectormax dimensions that are stored in here.
   do iv=1,nvarsel
!      ierr = nf90_inquire_var(ncids(1), varids(iv)
!      ndims = 2
      !dimids(1) = id_facedim(noutfile)
      !dimids(2) = id_timedim(noutfile)

      ip = var_timdimpos(iv)
      if (ip /= -1) then
         var_dimids(ip,iv) = id_timedim(noutfile)
      end if
      ip = var_spacedimpos(iv)
      if (ip /= -1) then
         if (var_loctype(iv) == UNC_LOC_S) then
            var_dimids(ip,iv) = id_facedim(noutfile)
         else if (var_loctype(iv) == UNC_LOC_SN) then
            var_dimids(ip,iv) = id_netfacedim(noutfile)
         else if (var_loctype(iv) == UNC_LOC_U) then
            var_dimids(ip,iv) = id_edgedim(noutfile)
         else if (var_loctype(iv) == UNC_LOC_CN) then
            var_dimids(ip,iv) = id_nodedim(noutfile)
         else if (var_loctype(iv) == UNC_LOC_L) then
            var_dimids(ip,iv) = id_netedgedim(noutfile)
         else if (var_loctype(iv) == UNC_LOC_SBND) then
            var_dimids(ip,iv) = id_bnddim(noutfile)
         else
            write (*,'(a,i0,a)') 'Error: mapmerge: Unknown location type ', var_loctype(iv), ' for `'//trim(var_names(iv))//'''.'
         end if
      end if
      ip = var_laydimpos(iv)
      if (ip /= -1) then
         var_dimids(ip,iv) = id_laydim(noutfile)
      end if
      ip = var_wdimpos(iv)
      if (ip /= -1) then
         var_dimids(ip,iv) = id_wdim(noutfile)
      end if
      ip = var_kxdimpos(iv)
      if (ip /= -1) then
         var_dimids(ip,iv) = dimids(var_dimids(ip,iv), noutfile) ! this is necessary because in outfile dim IDs will be in different order, even if *all* dim ids have been copied
         ! Dim ID for this var in outfile === based on *pos* in dimids(:) for *this* noutfile.
      end if

      ierr = nf90_def_var(ncids(noutfile), var_names(iv), var_types(iv), var_dimids(4-var_ndims(iv)+1:4,iv), varids_out(iv))

      if (ierr /= nf90_noerr) then
         write (*,'(a)') 'Error: mapmerge: could not create output variable `'//trim(var_names(iv))//'''. '
         write (*,'(a)')        'Details : '//trim(nf90_strerror(ierr))
         if (.not. verbose_mode) goto 888
         varids_out(iv) = -1
         cycle
      end if

      ierr = ncu_copy_atts(ncids(ifile), ncids(noutfile), varids(ifile, iv), varids_out(iv))
      if (isNetCDF4) then
         new_ndx = min(ndx(noutfile), 2000)  ! ! must be equal to mapclass_chunksize_ndx
         ierr = ncu_copy_chunking_deflate(ncids(ifile), ncids(noutfile), varids(ifile, iv), varids_out(iv), new_ndx)
      endif

      ! For Variable 'FlowLink', the flowelem might be outside the domain, and there might be no info. about such flowelem
      ! in mapfiles, so they are denoted by _FillValue.
      if (var_names(iv) .eq. 'FlowLink') then
          ierr = nf90_put_att(ncids(noutfile), varids_out(iv), '_FillValue', intmiss)
      end if
   end do


   ierr = nf90_put_att(ncids(noutfile), nf90_global, 'NumPartitionsInFile', nfiles)
   ierr = nf90_def_dim(ncids(noutfile), 'nPartitions', nfiles, id_npartdim)
   ierr = nf90_def_var(ncids(noutfile), 'partitions_face_start', nf90_int, (/ id_npartdim /), id_part_face_start)
   ierr = nf90_def_var(ncids(noutfile), 'partitions_edge_start', nf90_int, (/ id_npartdim /), id_part_edge_start)
   ierr = nf90_def_var(ncids(noutfile), 'partitions_node_start', nf90_int, (/ id_npartdim /), id_part_node_start)
   ierr = nf90_put_att(ncids(noutfile), id_part_face_start, 'long_name', 'start index in global data arrays for face data for all partititions')
   ierr = nf90_put_att(ncids(noutfile), id_part_edge_start, 'long_name', 'start index in global data arrays for edge data for all partititions')
   ierr = nf90_put_att(ncids(noutfile), id_part_node_start, 'long_name', 'start index in global data arrays for node data for all partititions')
   ! NOTE: AvD: intentionally not adding netedge here, as I want to phase it out.
   ierr = nf90_def_var(ncids(noutfile), 'partitions_face_count', nf90_int, (/ id_npartdim /), id_part_face_count)
   ierr = nf90_def_var(ncids(noutfile), 'partitions_edge_count', nf90_int, (/ id_npartdim /), id_part_edge_count)
   ierr = nf90_def_var(ncids(noutfile), 'partitions_node_count', nf90_int, (/ id_npartdim /), id_part_node_count)
   ierr = nf90_put_att(ncids(noutfile), id_part_face_count, 'long_name', 'per-partition count in global data arrays for face data')
   ierr = nf90_put_att(ncids(noutfile), id_part_edge_count, 'long_name', 'per-partition count in global data arrays for edge data')
   ierr = nf90_put_att(ncids(noutfile), id_part_node_count, 'long_name', 'per-partition count in global data arrays for node data')
   if (nbndglob>0) then
      ierr = nf90_def_var(ncids(noutfile), 'partitions_facebnd_count', nf90_int, (/ id_npartdim /), id_part_facebnd_count)
      ierr = nf90_put_att(ncids(noutfile), id_part_facebnd_count, 'long_name', 'per-partition count in global data arrays for boundary points data')
      ierr = nf90_def_var(ncids(noutfile), 'partitions_facebnd_start', nf90_int, (/ id_npartdim /), id_part_facebnd_start)
      ierr = nf90_put_att(ncids(noutfile), id_part_facebnd_start, 'long_name', 'start index in global data arrays for boundary points for all partititions')
   endif
   ! 4. Write new flow geom to output file
   ! 5. Write all timedep fields from all files into output file

   ierr = nf90_enddef(ncids(noutfile))

   ! For now: do all times.
   ntsel = nt(ifile)
   nt(noutfile) = ntsel
   allocate(itimsel(ntsel))
   itimsel = (/ (it, it=1,ntsel) /)
   allocate(times(ntsel), timestep(ntsel))
   do it=1,ntsel
      ierr = nf90_get_var(ncids(ifile), id_time(ifile), times(it), start = (/ itimsel(it) /)) ! count=1
      ierr = nf90_get_var(ncids(ifile), id_timestep(ifile), timestep(it), start = (/ itimsel(it) /)) ! count=1
   end do

   ierr = nf90_put_var(ncids(noutfile), id_time(noutfile), times, count = (/ ntsel /))
   ierr = nf90_put_var(ncids(noutfile), id_timestep(noutfile), timestep, count = (/ ntsel /))

   if (verbose_mode) then
      write (*,'(a)') '## Writing merged variables to output file...'
   end if

   ! 1D tmp array: take largest of all topological position counts:
   maxitems = max(nedgecount, nfacecount, nnodecount, nnetedgecount, nbndcount, kmx(noutfile)+1)
   call realloc( tmpvar1D, maxitems, keepExisting=.false.)
   call realloc(itmpvar1D, maxitems, keepExisting=.false.)
   call realloc(itmpvar1D_tmp,maxitems, keepExisting=.false.)
   size_btmp = max(ndx(size(ndx)), sum(ndx(1:size(ndx)-1)))
   call realloc(btmpvar1D, [size_btmp, mapclass_time_buffer_size], keepExisting=.false.)
   call realloc(btmpvar1D_tmp, [ndx(size(ndx)), mapclass_time_buffer_size], keepExisting=.false.)
   call realloc(tmpvar1D_tmp, maxitems, keepExisting=.false.)
   ! 2D/3D done in loop below

   do iv=1,nvarsel
      if (verbose_mode) then
         write (tmpstr1, '(a,i0,a,i0,a)') 'Var #', iv, ' of ', nvarsel, ': '
      end if

      ! Skip merging the connectivity variables when coordinates of netnodes or netedges are not read from the files
      if (jamerge_cntv == 0 .and. (var_names(iv) .eq. 'NetLink' .or. var_names(iv) .eq. 'NetElemNode' .or. &
          var_names(iv) .eq. 'NetElemLink' .or. var_names(iv) .eq. 'ElemLink' .or. &
          var_names(iv) .eq. 'mesh2d_edge_nodes' .or. var_names(iv) .eq. 'mesh2d_face_nodes' .or. var_names(iv) .eq. 'mesh2d_edge_faces')) then
          write (*,'(a)') 'Warning: mapmerge: Skipping merging topology variable: `'//trim(var_names(iv))//''', because coordinates of netnodes or netedges can not be read from the file.'
          cycle
      end if

      if (var_ndims(iv) == 0) then  ! For instance, 'Mesh2D'
         cycle
      else if (var_spacedimpos(iv) == -1 .and. var_timdimpos(iv) == -1 .and. var_kxdimpos(iv) /= -1) then
         ! Some unknown non-space and non-time dimension: impossible to merge in a generic way. Skip it.
         write (*,'(a)') 'Warning: mapmerge: cannot merge vars with non-space/time dimensions: `'//trim(var_names(iv))//'''. Skipping.'
         cycle
         ! TODO: AvD: read + write timestep variable and similar here.
      end if

      is = MAX_VAR_DIMS-var_ndims(iv)+1
      ie = MAX_VAR_DIMS
      start_idx  (is:ie) = 1 ! For all relevant dimensions for this var: start indices to be set below
      count_read (is:ie) = 1 ! For all relevant dimensions for this var: read  counts to be set below
      count_write(is:ie) = 1 ! For all relevant dimensions for this var: write counts to be set below

      ! 5a.1 Optional kx/vectormax is the same for all files, for all times:
      if (var_kxdimpos(iv) /= -1) then
         id = var_dimids(var_kxdimpos(iv), iv) ! Dim ID for this kx dim in outfile
         ierr = nf90_inquire_dimension(ncids(noutfile), id, name = dimname, len = nlen)
         if (ierr /= nf90_noerr) then
            write (*,'(a,i0,a,a,a)') 'Error: mapmerge: Could not inquire vectormax dimension #', id, ' for variable `', trim(var_names(iv)), '''.'
            cycle ! iv
         end if
         count_read (var_kxdimpos(iv)) = nlen
         count_write(var_kxdimpos(iv)) = nlen
      end if

      ! 5a.2 Optional kmx/layer dim is assumed to be the same for all files, for all times:
      if (var_laydimpos(iv) /= -1) then
         count_read (var_laydimpos(iv)) = kmx(noutfile)
         count_write(var_laydimpos(iv)) = kmx(noutfile)
         ! TODO: AvD: UNST-993: support w-position quantities: difference between kmx and kmx1 !!
         ! TODO: AvD: future work: what if kmx varies between input files?
      end if
      if (var_wdimpos(iv) /= -1) then
         count_read (var_wdimpos(iv)) = kmx(noutfile) +1
         count_write(var_wdimpos(iv)) = kmx(noutfile) +1
      end if

      ! 5a.3 Prepare for space dimension, by pointers to proper face/edge/node/netedge variables
      if ( var_spacedimpos(iv) /= -1) then
      select case (var_loctype(iv))
      case (UNC_LOC_S)
         item_counts => ndx
         item_domain => face_domain
      case (UNC_LOC_SN)
         item_counts => ndx
         item_domain => face_domain
      case (UNC_LOC_U)
         item_counts => lnx
         item_domain => edge_domain
      case (UNC_LOC_CN)
         item_counts => numk
         item_domain => node_domain
      case (UNC_LOC_L)
         item_counts => numl
         item_domain => netedge_domain
      case (UNC_LOC_SBND)
         item_counts => ndxbnd
         item_domain => facebnd_domain
      case default
         ! TODO: Handle non-space dependent vars
         if (var_ndims(iv) > 0) then
            write (*,'(a)') 'Warning: mapmerge: cannot write space-independent vars: `'//trim(var_names(iv))//'''. Skipping.'
         else
            if (verbose_mode) then
               call progress(tmpstr1, 100) ! generate the progress bar.
            end if
         end if

         cycle
      end select
      count_write(var_spacedimpos(iv)) = item_counts(noutfile)
      endif

      ! NOTE: AvD: below we assume that order is kx, kmx, ndx, nt, so not as generic anymore as the var_*dimpos analysis would allow.
      ! Allocate the proper memory space for nf90_get_var without risk of stack overflows in the netcdf lib

      if (var_types(iv) /= nf90_double .and. var_types(iv) /= nf90_int .and. var_types(iv) /= nf90_short .and. var_types(iv) /= nf90_byte) then
         write (*,'(a,i0,a)') 'Error: mapmerge: encountered unsupported data type ', var_types(iv), ' for variable `'//trim(var_names(iv))//'''.'
         if (.not. verbose_mode) goto 888
      end if

      intfillv = dble(intmiss)
      if ((var_kxdimpos(iv) == -1 .and. var_laydimpos(iv) == -1  .and. var_wdimpos(iv) == -1) & ! 1D array with no layers and no vectormax (possibly time-dep)
           .or. (var_ndims(iv) == 1 .and. (var_laydimpos(iv) > 0 .or. var_wdimpos(iv) > 0)) )then ! 1D array of vertical coordinates
         ! Already allocated at max(lnx, ndx, numk, numl), no risk of stack overflow
         if (var_types(iv) == nf90_double) then
            tmpvarptr(1:1,1:1,1:maxitems)  =>  tmpvar1D(:)
         else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
            itmpvarptr(1:1,1:1,1:maxitems) => itmpvar1D(:)
         else if (var_types(iv) == nf90_byte) then
            btmpvarptr(1:1,1:1,1:maxitems,1:mapclass_time_buffer_size) => btmpvar1D(:,1:mapclass_time_buffer_size)
         end if
         tmpvarDim = 1

      else if (var_kxdimpos(iv) /= -1) then
         if (var_laydimpos(iv) /= -1) then   ! Both a vectormax AND a laydim
            call realloc(tmpvar3D, (/  count_read(var_kxdimpos(iv)), count_read(var_laydimpos(iv)), maxitems /), keepExisting=.false.)
            ! use maxitems instead of items_count(noutfile) to try and have as few reallocs as possible.
            tmpvarptr => tmpvar3D
            tmpvarDim = 3
         else                                ! Only a vectormax dim
            if (var_types(iv) == nf90_double) then
               call realloc( tmpvar2D, (/  count_read(var_kxdimpos(iv)), maxitems /), keepExisting=.false., fill=dmiss)
               tmpvarptr(1:count_read(var_kxdimpos(iv)),1:1,1:maxitems)  =>  tmpvar2D(:,:)
               call realloc( tmpvar2D_tmp, (/  count_read(var_kxdimpos(iv)), maxitems /), keepExisting=.false., fill=dmiss)
            else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
               call realloc(itmpvar2D, (/  count_read(var_kxdimpos(iv)), maxitems /), keepExisting=.false., fill=intfillv)
               itmpvarptr(1:count_read(var_kxdimpos(iv)),1:1,1:maxitems) => itmpvar2D(:,:)
               call realloc(itmpvar2D_tmp, (/  count_read(var_kxdimpos(iv)), maxitems /), keepExisting=.false., fill=intfillv)
            end if
            tmpvarDim = 2
         end if
      else if (var_laydimpos(iv) /= -1) then ! Only a laydim
         if (var_types(iv) == nf90_double) then
            call    realloc(tmpvar2D, (/  kmx(noutfile), maxitems /), keepExisting=.false., fill=dmiss)
            tmpvarptr(1:1,1:kmx(noutfile),1:maxitems)  =>  tmpvar2D(:,:)
            call    realloc(tmpvar2D_tmp, (/  kmx(noutfile), maxitems /), keepExisting=.false., fill=dmiss)
         else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
            call    realloc(itmpvar2D, (/  kmx(noutfile), maxitems /), keepExisting=.false., fill=intfillv)
            itmpvarptr(1:1,1:kmx(noutfile),1:maxitems) => itmpvar2D(:,:)
            call    realloc(itmpvar2D_tmp, (/  kmx(noutfile), maxitems /), keepExisting=.false., fill=intfillv)
         end if
         tmpvarDim = 2
      else if (var_wdimpos(iv) /= -1) then
         if (var_types(iv) == nf90_double) then
            call    realloc(tmpvar2D, (/  kmx(noutfile)+1, maxitems /), keepExisting=.false., fill=dmiss)
            tmpvarptr(1:1,1:kmx(noutfile)+1,1:maxitems)  =>  tmpvar2D(:,:)
            call    realloc(tmpvar2D_tmp, (/  kmx(noutfile)+1, maxitems /), keepExisting=.false., fill=dmiss)
         else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
            call    realloc(itmpvar2D, (/  kmx(noutfile)+1, maxitems /), keepExisting=.false., fill=intfillv)
            itmpvarptr(1:1,1:kmx(noutfile)+1,1:maxitems) => itmpvar2D(:,:)
            call    realloc(itmpvar2D_tmp, (/  kmx(noutfile)+1, maxitems /), keepExisting=.false., fill=intfillv)
         end if
         tmpvarDim = 2
      end if
      
      !! 1D array of vertical coordinates are COPIED from file "ifile" to the merged file
      if (var_ndims(iv) == 1 .and. (var_laydimpos(iv) > 0 .or. var_wdimpos(iv) > 0)) then 
         nlen = count_read(ie)
         if (var_types(iv) == nf90_double) then
            ierr = nf90_get_var(ncids(ifile), varids(ifile,iv), tmpvar1D(1:nlen), count=count_read(is:ie))
            if (ierr /= nf90_noerr) then
               write (*,'(a,i0,a)') 'Error: mapmerge: cannot read variable ', var_names(iv), ' from file `'//trim(infiles(ifile))//'''.'
               if (.not. verbose_mode) goto 888
            end if

            ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar1D, count = count_write(is:ie), start = start_idx(is:ie))
            if (ierr /= nf90_noerr) then
               write (*,'(a,i0,a)') 'Error: mapmerge: cannot write variable ', var_names(iv), ' to the merged file.'
               if (.not. verbose_mode) goto 888
            end if
         else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
            ierr = nf90_get_var(ncids(ifile), varids(ifile,iv), itmpvar1D(1:nlen), count=count_read(is:ie))
            if (ierr /= nf90_noerr) then
               write (*,'(a,i0,a)') 'Error: mapmerge: cannot read variable ', var_names(iv), ' from file `'//trim(infiles(ifile))//'''.'
               if (.not. verbose_mode) goto 888
            end if

            ierr = nf90_put_var(ncids(noutfile), varids_out(iv), itmpvar1D, count = count_write(is:ie), start = start_idx(is:ie))
            if (ierr /= nf90_noerr) then
               write (*,'(a,i0,a)') 'Error: mapmerge: cannot write variable ', var_names(iv), ' to the merged file.'
               if (.not. verbose_mode) goto 888
            end if
         end if
      else ! the following is merging
         do it=1,ntsel
            itm = mod(it-1, mapclass_time_buffer_size)+1
            if (verbose_mode) then
               call progress(tmpstr1, ceiling((it-1)*100.0/ntsel)) ! generate the progress bar.
            end if
         
            ! 5a.4 Time dimension: Which timestep to read from input files
            if (var_timdimpos(iv) /= -1) then
               start_idx(var_timdimpos(iv)) = itimsel(it)
            end if
         
            nitemglob  = 0
            nitemcount = 0
            do ii=1,nfiles
               nitemglob0 = nitemglob
         
               if (var_spacedimpos(iv) /= -1) then
                  if (item_counts(ii) == 0) then
                     cycle
                  else
                     count_read(var_spacedimpos(iv)) = item_counts(ii) ! How many flow face/edges/nodes to read from file #ii
                  endif
               end if
         
               ! Do the actual reading
         
               ! When one file has only triangular mesh and one file has only rectangular mesh, then a variable, e.g. 'NetElemNode'
               ! in the target merged map has vectormax dimension nlen=4. To read such a variable from each file, the vectormax dimension
               ! should be consistent in the current file. If this dimension is smaller than the maximal nlen, then a seperate array
               ! "itmpvar2D_tmpmax" will be defined by the current vectormax dimension. We first read values into this new array and then
               ! put them into array "itmpvar2D" (UNST-1842).
               if (var_kxdimpos(iv) /= -1 .and. (dimname == 'nNetElemMaxNode' .or. dimname == 'max_nmesh2d_face_nodes' .or. dimname == 'mesh2d_nMax_face_nodes' .or. dimname=='nFlowElemContourPts')) then
                  count_read(is) = netfacemaxnodes(ii)
                  if (netfacemaxnodes(ii) < nlen) then
                     jaread_sep = 1
                  end if
               end if
         
               if (var_kxdimpos(iv) == -1 .and. var_laydimpos(iv) == -1  .and. var_wdimpos(iv) == -1) then ! 1D array with no layers and no vectormax (possibly time-dep)
                  if (var_types(iv) == nf90_double) then
                     ierr = nf90_get_var(ncids(ii), varids(ii,iv), tmpvar1D(    nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                  else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
                     ierr = nf90_get_var(ncids(ii), varids(ii,iv), itmpvar1D(    nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                  else if (var_types(iv) == nf90_byte) then
                     ierr = nf90_get_var(ncids(ii), varids(ii,iv), btmpvar1D(    nitemglob0+1:,itm:itm), count=count_read(is:ie), start=start_idx(is:ie))
                  end if
               else if (var_kxdimpos(iv) /= -1 .neqv. var_laydimpos(iv) /= -1) then ! Either a vectormax OR a laydim
                  if (var_types(iv) == nf90_double) then
                     if (jaread_sep == 1) then
                        call realloc(tmpvar2D_tmpmax, (/  count_read(is), count_read(ie) /), keepExisting=.false., fill=dmiss)
                        ierr = nf90_get_var(ncids(ii), varids(ii,iv), tmpvar2D_tmpmax, count=count_read(is:ie), start=start_idx(is:ie))
                        tmpvar2D(1:netfacemaxnodes(ii),nitemglob0+1:nitemglob0+count_read(ie)) = tmpvar2D_tmpmax(1:count_read(is),1:count_read(ie))
                        jaread_sep = 0
                     else
                        if (var_seddimpos(iv) /= -1) then 
                           ! Reading a sediment variable needs to specify the "map" argument in nf90_get_var, because its dimensions are in a different order than other vectormax variables
                           ierr = nf90_get_var(ncids(ii), varids(ii,iv),  tmpvar2D(  :,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie), map = (/ count_read (var_kxdimpos(iv)), 1, count_read (var_kxdimpos(iv))*item_counts(ii) /))
                        else
                           ierr = nf90_get_var(ncids(ii), varids(ii,iv),  tmpvar2D(  :,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                        end if
                     end if
                  else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
                     if (jaread_sep == 1) then
                        call realloc(itmpvar2D_tmpmax, (/  count_read(is), count_read(ie) /), keepExisting=.false., fill=intfillv)
                        ierr = nf90_get_var(ncids(ii), varids(ii,iv), itmpvar2D_tmpmax, count=count_read(is:ie), start=start_idx(is:ie))
                        itmpvar2D(1:netfacemaxnodes(ii),nitemglob0+1:nitemglob0+count_read(ie)) = itmpvar2D_tmpmax(1:count_read(is),1:count_read(ie))
                        jaread_sep = 0
                     else
                        ierr = nf90_get_var(ncids(ii), varids(ii,iv), itmpvar2D(  :,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                     end if
                  end if
                else if (var_kxdimpos(iv) /= -1 .neqv. var_wdimpos(iv) /= -1) then ! Either a vectormax OR a wdim
                  if (var_types(iv) == nf90_double) then
                     ierr = nf90_get_var(ncids(ii), varids(ii,iv),  tmpvar2D(  :,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                  else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
                     ierr = nf90_get_var(ncids(ii), varids(ii,iv), itmpvar2D(  :,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                  end if
               else ! Both a vectormax AND a laydim
                  if (var_types(iv) == nf90_double) then
                     ierr = nf90_get_var(ncids(ii), varids(ii,iv), tmpvar3D(:,:,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                  end if
               end if
               if (ierr /= nf90_noerr) then
                  write (*,'(a,i0,a)') 'Error: mapmerge: could not read `'//trim(var_names(iv))//''' from file `'//trim(infiles(ii))//''' (it=', itimsel(it), ').'
                  if (.not. verbose_mode) goto 888
               end if
         
               ! Now shift all items (in space) that really belong to *current* domain ii to the left,
               ! such that global item (edge/node) nrs form one increasing range in tmpvar.
               ! Faces related variables in the merged file are numbered by 'FlowElemGlobalNr'
               ! Conectivity arrays are taken care seperately.
               if (var_names(iv) .eq. 'NetLink' .or. var_names(iv) .eq. 'mesh2d_edge_nodes') then
                   nnetedgecount = sum(numl(1:ii-1))
                   nnodecount = sum(numk(1:ii-1))
                   do ip=1,item_counts(ii)
                       if (item_domain(nnetedgecount+ip) == ii-1) then ! only for the NetLink which belongs to the current domain
                           inetedgeglob = netedge_c2g(nnetedgecount+ip)
                           if (inetedgeglob > 0) then
                               nitemglob = nitemglob + 1               ! for later checking nitemglob
                               itmpvar2D(:,nitemglob) = itmpvar2D(:,nitemglob0+ip)
                               do ikk=1,2
                                   g1 = itmpvar2D(ikk, nitemglob)
                                   if (g1 > 0) then
                                       g1 = g1 +nnodecount
                                       itmpvar2D(ikk,nitemglob) = node_c2g(g1) ! mapping the nodes to global nodes
                                   end if
                               end do
                           end if
                       end if
                   end do
               else if (var_names(iv) .eq. 'NetElemNode' .or. var_names(iv) .eq. 'mesh2d_face_nodes') then
                   nfacecount = sum(nump(1:ii-1))
                   nnodecount = sum(numk(1:ii-1))
                   do ip=1, item_counts(ii)
                       if (item_domain(nfacecount+ip) == ii-1) then
                           ifaceglob = face_c2g(nfacecount+ip)
                           if (ifaceglob > 0) then
                               nitemglob = nitemglob + 1
                               itmpvar2D_tmp(:,nitemglob) = itmpvar2D(:,nitemglob0+ip)
                               do ikk = 1, netfacemaxnodes(ii)
                                   g1 = itmpvar2D_tmp(ikk,nitemglob)
                                   if (g1 > 0) then
                                       g1 = g1 + nnodecount
                                       itmpvar2D_tmp(ikk,nitemglob) = node_c2g(g1)
                                   end if
                               end do
                           end if
                       end if
                   end do
                   if (ii==nfiles) then ! When finished, copy to itmpvar2D for writting
                       itmpvar2D(:,1:nitemglob) = itmpvar2D_tmp(:,1:nitemglob)
                   end if
               else if (var_names(iv) .eq. 'NetElemLink') then
                   nfacecount = sum(nump(1:ii-1))
                   nnetedgecount = sum(numl(1:ii-1))
                   do ip =1, item_counts(ii)
                       if (item_domain(nfacecount+ip) == ii-1) then
                           ifaceglob = face_c2g(nfacecount+ip)
                           if (ifaceglob > 0) then
                               nitemglob = nitemglob + 1
                               itmpvar2D_tmp(:,nitemglob) = itmpvar2D(:,nitemglob0+ip)
                               do ikk = 1, netfacemaxnodes(ii)
                                   g1 = itmpvar2D_tmp(ikk,nitemglob)
                                   if (g1 > 0) then
                                       g1 = g1 + nnetedgecount
                                       itmpvar2D_tmp(ikk,nitemglob) = netedge_c2g(g1)
                                   end if
                               end do
                           end if
                       end if
                   end do
                   if (ii==nfiles) then
                       itmpvar2D(:,1:nitemglob) = itmpvar2D_tmp(:,1:nitemglob)
                   end if
               else if (var_names(iv) .eq. 'ElemLink' .or. var_names(iv) .eq. 'mesh2d_edge_faces') then
                   nnetedgecount = sum(numl(1:ii-1))
                   nfacecount = sum(nump(1:ii-1))
                   do ip=1,item_counts(ii)
                       if (item_domain(nnetedgecount+ip) == ii-1) then
                           inetedgeglob = netedge_c2g(nnetedgecount+ip)
                           if (inetedgeglob > 0) then
                               nitemglob = nitemglob + 1
                               itmpvar2D(:,nitemglob) = itmpvar2D(:,nitemglob0+ip)
                               do ikk=1,2
                                   g1 = itmpvar2D(ikk, nitemglob)
                                   if (g1 > 0) then
                                       g1 = g1 +nfacecount
                                       itmpvar2D(ikk,nitemglob) = face_c2g(g1)
                                   else if (g1 == 0) then
                                       itmpvar2D(ikk,nitemglob) = 0
                                   end if
                               end do
                           end if
                       end if
                   end do
               else if (var_names(iv) .eq. 'FlowLink') then
                   nedgecount = sum(lnx(1:ii-1))
                   nfacecount = sum(nump(1:ii-1))
                   do ip=1,item_counts(ii)
                       if (item_domain(nedgecount+ip) == ii-1) then
                           iedgeglob = edge_c2g(nedgecount+ip)
                           if (iedgeglob > 0) then
                               nitemglob = nitemglob + 1
                               itmpvar2D_tmp(:,nitemglob) = itmpvar2D(:,nitemglob0+ip)
                               do ikk=1,2
                                   g1 = itmpvar2D_tmp(ikk, nitemglob)
                                   if (g1 > 0 .and. g1 <= nump(ii)) then
                                       g1 = g1 +nfacecount
                                       itmpvar2D_tmp(ikk,nitemglob) = face_c2g(g1)
                                   else if (g1 > nump(ii)) then
                                       itmpvar2D_tmp(ikk,nitemglob) = intmiss
                                   end if
                               end do
                           end if
                       end if
                   end do
                   if (ii==nfiles) then
                       itmpvar2D(:,1:nitemglob) = itmpvar2D_tmp(:,1:nitemglob)
                   end if
               !else if (var_loctype(iv) == UNC_LOC_S) then ! variables that locate on faces, temporaly disabled
               !    if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
               !      nfacecount = sum(nump(1:ii-1))
               !      do ip=1, item_counts(ii)
               !         if (item_domain(nfacecount+ip) == ii-1) then
               !            ifaceglob = face_c2g(nfacecount+ip)
               !            if (ifaceglob > 0) then
               !               nitemglob = nitemglob + 1
               !               itmpvar1D_tmp(ifaceglob) = itmpvar1D(nitemglob0+ip)
               !            end if
               !         end if
               !      end do
               !      if (ii==nfiles) then
               !          itmpvar1D(1:nitemglob) = itmpvar1D_tmp(1:nitemglob)
               !      end if
               !   else if (var_types(iv) == nf90_byte) then
               !      nfacecount = sum(nump(1:ii-1))
               !      do ip=1, item_counts(ii)
               !         if (item_domain(nfacecount+ip) == ii-1) then
               !            ifaceglob = face_c2g(nfacecount+ip)
               !            if (ifaceglob > 0) then
               !               nitemglob = nitemglob + 1
               !               btmpvar1D_tmp(ifaceglob,itm:itm) = btmpvar1D(nitemglob0+ip,itm:itm)
               !            end if
               !         end if
               !      end do
               !      if (ii==nfiles) then
               !          btmpvar1D(1:nitemglob,itm:itm) = btmpvar1D_tmp(1:nitemglob,itm:itm)
               !      end if
               !   else
               !      nfacecount = sum(nump(1:ii-1))
               !      if (tmpvarDim == 1) then
               !         do ip=1, item_counts(ii)
               !            if (item_domain(nfacecount+ip) == ii-1) then
               !               ifaceglob = face_c2g(nfacecount+ip)
               !               if (ifaceglob > 0) then
               !                  nitemglob = nitemglob + 1
               !                  tmpvar1D_tmp(ifaceglob) = tmpvar1D(nitemglob0+ip)
               !               end if
               !            end if
               !         end do
               !         if (ii==nfiles) then
               !             tmpvar1D(1:nitemglob) = tmpvar1D_tmp(1:nitemglob)
               !         end if
               !      else if (tmpvarDim == 2) then
               !         do ip=1, item_counts(ii)
               !            if (item_domain(nfacecount+ip) == ii-1) then
               !               ifaceglob = face_c2g(nfacecount+ip)
               !               if (ifaceglob > 0) then
               !                  nitemglob = nitemglob + 1
               !                  tmpvar2D_tmp(:,ifaceglob)=tmpvar2D(:,nitemglob0+ip)
               !               end if
               !            end if
               !         end do
               !         if (ii==nfiles) then
               !            tmpvar2D(:,1:nitemglob) = tmpvar2D_tmp(:,1:nitemglob)
               !         end if
               !      end if
               !   end if
               else
               needshift = .false. ! The get_var above started at the right place, so no shifting needed yet.
               if (var_types(iv) == nf90_double) then ! TODO: AvD: try to remove this ugly code-copy for just different types
                  do ip=1,item_counts(ii)
                     if (item_domain(nitemcount+ip) == ii-1) then
                        nitemglob = nitemglob+1
                        if (needshift) then
                           tmpvarptr(:,:,nitemglob) = tmpvarptr(:,:,nitemglob0+ip)
                        end if
                     else
                        needshift = .true. ! From now on, all points from this var/file need one or more shifts to the left.
                     end if
                  end do
               else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
                  do ip=1,item_counts(ii)
                     if (item_domain(nitemcount+ip) == ii-1) then
                        nitemglob = nitemglob+1
                        if (needshift) then
                           itmpvarptr(:,:,nitemglob) = itmpvarptr(:,:,nitemglob0+ip)
                        end if
                     else
                        needshift = .true. ! From now on, all points from this var/file need one or more shifts to the left.
                     end if
                  end do
               else if (var_types(iv) == nf90_byte) then
                  do ip=1,item_counts(ii)
                     if (item_domain(nitemcount+ip) == ii-1) then
                        nitemglob = nitemglob+1
                        if (needshift) then
                           btmpvarptr(:,:,nitemglob,itm:itm) = btmpvarptr(:,:,nitemglob0+ip,itm:itm)
                        end if
                     else
                        needshift = .true. ! From now on, all points from this var/file need one or more shifts to the left.
                     end if
                  end do
               end if
               nitemcount = nitemcount + item_counts(ii)
              end if
            end do ! ii
         
            if (item_counts(noutfile) /= nitemglob) then
               write (*,'(a,i0,a,i0,a)') 'Error: mapmerge: accumulated ', nitemglob, ' items, but expected ', item_counts(noutfile), ', for `'//var_names(iv)//'''.'
               if (.not. verbose_mode) goto 888
            end if
            !! tmpvar is now filled with 1 var, 1 time, across all domains, without overlap, so write it now:
            if (var_kxdimpos(iv) == -1 .and. var_laydimpos(iv) == -1 .and. var_wdimpos(iv) == -1 .and. var_seddimpos(iv) == -1) then ! 1D array with no layers and no vectormax (possibly time-dep)
               if (var_types(iv) == nf90_double) then
                  ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar1D, count = count_write(is:ie), start = start_idx(is:ie))
               else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
                  ierr = nf90_put_var(ncids(noutfile), varids_out(iv), itmpvar1D, count = count_write(is:ie), start = start_idx(is:ie))
               else if (var_types(iv) == nf90_byte) then
                  if (itm == mapclass_time_buffer_size) then
                     ierr = nf90_put_var(ncids(noutfile), varids_out(iv), btmpvar1D, count = [count_write(is), mapclass_time_buffer_size*count_write(ie)], start = [start_idx(is), start_idx(ie) + 1 - mapclass_time_buffer_size])
                  endif
               end if
            else if (var_kxdimpos(iv) /= -1 .neqv. var_laydimpos(iv) /= -1) then ! Either a vectormax OR a laydim
               if (var_types(iv) == nf90_double) then
                  if (var_seddimpos(iv) /= -1) then ! if it is a sediment variable
                     ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar2D, count = count_write(is:ie), start = start_idx(is:ie), map = (/ count_write (var_kxdimpos(iv)), 1, count_write (var_kxdimpos(iv))*item_counts(ii) /))
                  else
                     ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar2D, count = count_write(is:ie), start = start_idx(is:ie))
                  end if
               else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
                  ierr = nf90_put_var(ncids(noutfile), varids_out(iv), itmpvar2D, count = count_write(is:ie), start = start_idx(is:ie))
               end if
            else if (var_kxdimpos(iv) /= -1 .neqv. var_wdimpos(iv) /= -1) then ! Either a vectormax OR a laydim
               if (var_types(iv) == nf90_double) then
                  ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar2D, count = count_write(is:ie), start = start_idx(is:ie))
               else if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
                  ierr = nf90_put_var(ncids(noutfile), varids_out(iv), itmpvar2D, count = count_write(is:ie), start = start_idx(is:ie))
               end if
            else ! Both a vectormax AND a laydim
               if (var_types(iv) == nf90_double) then
                  ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar3D, count = count_write(is:ie), start = start_idx(is:ie))
               end if
            end if
            !if (kmx(noutfile) > 0) then
            !   ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar, count = (/ kmx(noutfile), ndx(noutfile), 1 /), start = (/ 1, 1, it /))
            !else
            !   ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar, count = (/ ndx(noutfile), 1 /), start = (/ 1, it /))
            !end if
            !if (ierr /= nf90_noerr) then
            !   write (*,'(a,i0,a)') 'Error: mapmerge: could not write merged variable `'//trim(var_names(iv))//''' into output file `'//trim(outfile)//''' (itime=', it, ').'
            !   if (.not. verbose_mode) goto 888
            !end if
         
            ! Check: if this was time-independent variable, first iteration-step was enough for reading+writing.
            if (var_timdimpos(iv) == -1) then
               exit ! it
            end if
         
         end do ! it
      end if

      ! if writing with time buffer, write remaining time steps:
      if (var_types(iv) == nf90_byte) then
         if (itm /= mapclass_time_buffer_size) then
            ierr = nf90_put_var(ncids(noutfile), varids_out(iv), btmpvar1D(:,1:itm), count = [count_write(is), itm*count_write(ie)], start = [start_idx(is), start_idx(ie) + 1 - itm])
         endif
      endif

      if (verbose_mode) then
         call progress(tmpstr1, 100) ! generate the progress bar.
      end if

   end do ! iv


   ! 6. Write some useful meta info on all merged domains into the output file

   nfacecount = 0
   nedgecount = 0
   nnodecount = 0

   do ii=1,nfiles
      ierr = nf90_put_var(ncids(noutfile), id_part_face_start, nfacecount+1, start=(/ ii /))
      ierr = nf90_put_var(ncids(noutfile), id_part_edge_start, nedgecount+1, start=(/ ii /))
      ierr = nf90_put_var(ncids(noutfile), id_part_node_start, nnodecount+1, start=(/ ii /))

      nfacecount = nfacecount + ndxg (ii)
      nedgecount = nedgecount + lnxg (ii)
      nnodecount = nnodecount + numkg(ii)
   end do
   ierr = nf90_put_var(ncids(noutfile), id_part_face_count, ndxg (1:nfiles))
   ierr = nf90_put_var(ncids(noutfile), id_part_edge_count, lnxg (1:nfiles))
   ierr = nf90_put_var(ncids(noutfile), id_part_node_count, numkg(1:nfiles))

   if (nbndglob>0) then
      nbndcount  = 0
      do ii=1,nfiles
         ierr = nf90_put_var(ncids(noutfile), id_part_facebnd_start, nbndcount+1, start=(/ ii /))
         nbndcount = nbndcount  + ndxbnd(ii)
      enddo
      ierr = nf90_put_var(ncids(noutfile), id_part_facebnd_count, ndxbnd(1:nfiles))
   endif


   ! Success:
   ierr = 0

   !! Cleanup:
888 continue
   do ii=1,nfiles
      ierr = nf90_close(ncids(ii))
   end do
   ierr = nf90_close(ncids(noutfile))

   if (allocated(varids))          deallocate(varids)
   if (allocated(varids_out))      deallocate(varids_out)
   if (allocated(var_names))       deallocate(var_names)
   if (allocated(var_types))       deallocate(var_types)
   if (allocated(var_dimids))      deallocate(var_dimids)
   if (allocated(var_timdimpos))   deallocate(var_timdimpos)
   if (allocated(var_spacedimpos)) deallocate(var_spacedimpos)
   if (allocated(var_laydimpos))   deallocate(var_laydimpos)
   if (allocated(var_kxdimpos))    deallocate(var_kxdimpos)
   if (allocated(var_ndims))       deallocate(var_ndims)
   if (allocated(var_loctype))     deallocate(var_loctype)
   if (allocated(itmpvar1D))       deallocate(itmpvar1D)
   if (allocated(itmpvar2D))       deallocate(itmpvar2D)
   if (allocated(itmpvar2D_tmp))   deallocate(itmpvar2D_tmp)
   if (allocated(itmpvar2D_tmpmax))deallocate(itmpvar2D_tmpmax)
   if (allocated(tmpvar1D))        deallocate(tmpvar1D)
   if (allocated(tmpvar1D_tmp))    deallocate(tmpvar1D_tmp)
   if (allocated(tmpvar2D))        deallocate(tmpvar2D)
   if (allocated(tmpvar2D_tmp))    deallocate(tmpvar2D_tmp)
   if (allocated(tmpvar2D_tmpmax)) deallocate(tmpvar2D_tmpmax)
   if (allocated(tmpvar3D))        deallocate(tmpvar3D)
   if (allocated(btmpvar1D))       deallocate(btmpvar1D)
   if (allocated(btmpvar1D_tmp))   deallocate(btmpvar1D_tmp)

end function dfm_merge_mapfiles

!> Orders a filename list by increasing partition number.
!! Sorting is done in place.
subroutine dfm_order_by_partition(files, nfiles)
   use m_alloc
   implicit none

   character(len=MAXNAMELEN), intent(inout) :: files(:)   !< List files names, will be replaced by the sorted list.
   integer,                   intent(in)    :: nfiles     !< Number of input files.

   integer,                   allocatable :: idom(:)
   character(len=MAXNAMELEN), allocatable :: filesorig(:)
   integer :: ii, ierr, nlen

   allocate(idom(nfiles))
   allocate(filesorig(nfiles))

   do ii=1,nfiles
      filesorig(ii) = files(ii)
      nlen = len_trim(files(ii))
      ! modelname_xxxx_map.nc'
      !           0  76  3  0
      if (files(ii)(max(1,nlen-6):nlen) == '_map.nc') then
         read (files(ii)(max(1,nlen-10):nlen-7), '(i4.4)', iostat=ierr) idom(ii)
      else
         ierr = 1
      end if
      if (ierr /= 0) then
         idom(ii) = ii-1
      end if
   end do

   do ii=1,nfiles
      if (idom(ii)+1 > nfiles) then
         write (*,'(a,i0,a,i0,a)') 'Error: mapmerge: found domain number ', idom(ii), ', which exceeds the number of files: ', nfiles, '.'
         exit ! We can't really recover from this.
      end if

      files(idom(ii)+1) = filesorig(ii)
   end do

   deallocate(idom, filesorig)

end subroutine dfm_order_by_partition


subroutine progress(prefix, j)
  implicit none
  character(len=*),intent(in)  :: prefix
  integer(kind=4),intent(in)   :: j
  integer(kind=4)              :: k
  character(len=27)            ::bar

  bar = "???% |                    |"
  write(unit=bar(1:3),fmt="(i3)") j
  do k=1, ceiling(j/5.0)
    bar(6+k:6+k)="*"
  enddo
  ! print the progress bar.
  if (j==100) then
     write(unit=6,fmt="(a1,a16,a27)") char(13), prefix, bar
  else
     write(unit=6,fmt="(a1,a16,a27)", advance='no') char(13), prefix, bar
  end if

  flush(6)
  return
end subroutine progress
end module dfm_merge
