!!  Copyright (C)  Stichting Deltares, 2012-2020.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine outmnc ( ncidmap, mncnam , ugridf , timeid, bndtimeid, mncrec , itime  , moname ,
     &                    noseg  , notot1 , conc1  , synam1 , sysnm1, syuni1, sydsc1, wqid1  , notot2 ,
     &                    conc2  , synam2 , sysnm2, syuni2, sydsc2, wqid2  , volume, iknmrk , lunut)

!     Deltares Software Centre

!     Function            : Writes map output to NetCDF

      use timers
      use dlwq_netcdf  !   read/write grid in netcdf
      use output, only: ncopt
      implicit none

!     Parameters          :

!     kind           function         name                    description

      integer   (4), intent(inout) :: ncidmap              ! NetCDF id of output map file
      character(255), intent(in   ) :: mncnam               ! name NetCDF output map file
      character(255), intent(in   ) :: ugridf               ! name of NetCDF ugrid file
      integer   (4), intent(inout) :: timeid
      integer   (4), intent(inout) :: bndtimeid
      integer   (4), intent(in   ) :: mncrec               ! present record in NetCDF file
      integer   (4), intent(in   ) :: itime                ! present time in clock units
      character(40), intent(in   ) :: moname(4)            ! model identification
      integer   (4), intent(in   ) :: noseg                ! number of computational volumes
      integer   (4), intent(in   ) :: notot1               ! number of variables in conc1
      real      (4), intent(in   ) :: conc1 (notot1,noseg) ! values
      character(20), intent(in   ) :: synam1(notot1)       ! names of variables in conc1
      character(100), intent(in   ) :: sysnm1(notot1)       ! standard names of variables in conc1
      character(40), intent(in   ) :: syuni1(notot1)       ! units of variables in conc1
      character(60), intent(in   ) :: sydsc1(notot1)       ! decriptions of variables in conc1
      integer   (4), intent(inout) :: wqid1(notot1,2)      ! NetCDF ids of variables in conc1
      integer   (4), intent(in   ) :: notot2               ! number of variables in conc2
      real      (4), intent(in   ) :: conc2 (notot2,noseg) ! values
      character(20), intent(in   ) :: synam2(notot2)       ! names of variables in conc2
      character(100), intent(in   ) :: sysnm2(notot2)       ! standard names of variables in conc1
      character(40), intent(in   ) :: syuni2(notot2)       ! units of variables in conc1
      character(60), intent(in   ) :: sydsc2(notot2)       ! decriptions of variables in conc1
      integer   (4), intent(inout) :: wqid2(notot2,2)      ! NetCDF ids of variables in conc1
      real      (4), intent(in   ) :: volume(noseg)        ! values
      integer   (4), intent(in   ) :: iknmrk(noseg)        ! Feature array. Bit zero set means active.
      integer   (4), intent(in   ) :: lunut                ! unit number monitoring file

      integer(4) iseg                   ! loop counter for segments
      integer(4) k                      ! loop counter for substances
      real   (4) amiss   /-999.0/       ! missing value indicator

      integer :: ncid
      integer :: varid, varidout, meshid, meshidout, ntimeid, wqid, noseglmesh, nolaymesh
      integer :: inc_error, ierr, nolay, iout
      integer                               :: xtype
      integer                               :: ndims
      logical, allocatable                  :: sumconc1(:), sumconc2(:)
      integer, dimension(nf90_max_var_dims) :: dimids
      integer, dimension(nf90_max_dims)     :: dimsizes
!      integer                               :: naggr
      integer, dimension(:,:), allocatable  :: aggr

      integer           :: values(8)
      character(len=40) :: timestamp
      character(len=40) :: t0string
      character(len=40) :: uuid

      character(len=nf90_max_name) :: mesh_name
      character(len=nf90_max_name) :: dimname

      integer :: i, j
      integer :: type_ugrid
      integer :: noseglid, nolayid
      integer :: nosegl, isegl, ilay
      integer :: wqidvolume, wqidvolume_2d
      real, dimension(:), allocatable :: laythickness
      real, dimension(:,:), allocatable :: dlwq_values
      real, dimension(:), allocatable :: dlwq_values_2d, dlwq_volume_2d

      save ncid, noseglmesh, varid, aggr, wqidvolume, wqidvolume_2d, sumconc1, sumconc2 ! TODO auto object can not be saved! allocate!: sumconc1, sumconc2


      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "outmnc", ithandl )

      call dhnolay(nolay)
      nosegl = noseg/nolay  ! How to deal with layer sediment output? Separate file!

!     Initialize file
      if ( ncidmap .lt. 0 ) then

         ! Turn on debug info from dlwaqnc
         inc_error = dlwqnc_debug_status(.true.)

         ! Prepare a Delwaq-NetCDF output-file for map data from the UGRID-file
         ! To do: we should check if everything went right, if not, NetCDF output is not possible...

         ! Write the version of the netcdf library
         write ( lunut , 2520 ) trim(nf90_inq_libvers())

         ! Open the ugrid-file file
         inc_error = nf90_open(ugridf, nf90_nowrite, ncid )
         if (inc_error /= nf90_noerr ) then
            write ( lunut , 2530 ) trim(ugridf)
            goto 800
         end if

         inc_error = dlwqnc_read_dims( ncid, dimsizes )
         if (inc_error /= nf90_noerr ) then
            write ( lunut , 2531 ) trim(ugridf)
            goto 800
         end if

         ! Copy the global attributes to a new file
#ifdef NetCDF4
         if ( ncopt(1) == 4 ) then
             inc_error = nf90_create( mncnam, ior(nf90_clobber,nf90_netcdf4), ncidmap )
         else
             inc_error = nf90_create( mncnam, ior(nf90_clobber,nf90_format_classic), ncidmap )
         endif
#else
         inc_error = nf90_create( mncnam, nf90_clobber, ncidmap )
#endif
         if ( inc_error /= nf90_noerr ) then
             write ( lunut , 2560 ) trim(mncnam)
             goto 800
         endif

         ! Find the variable with the attribute "delwaq_role"
         inc_error = dlwqnc_find_var_with_att( ncid, "delwaq_role", varid )
         if ( inc_error == nf90_noerr ) then
            ! Get the dimensions
            inc_error = nf90_inquire_variable( ncid, varid, xtype = xtype, ndims = ndims, dimids = dimids )
            if ( inc_error /= nf90_noerr ) then
               write ( lunut , 2541 )
               goto 800
            endif

            noseglid   = dimids(1)
            nolayid    = dimids(2)
            noseglmesh = dimsizes(noseglid)
            nolaymesh  = dimsizes(nolayid)
            type_ugrid = type_ugrid_face_crds

            ! Read aggregation table
            allocate (aggr(noseglmesh, nolaymesh))
            inc_error = nf90_get_var( ncid, varid, aggr )

            ! Determine the mesh variable from that
            mesh_name = ' '
            inc_error = nf90_get_att( ncid, varid, "mesh", mesh_name )
            if ( inc_error /= nf90_noerr ) then
                write ( lunut , 2555 )
                goto 800
            endif
         else
            ! TODO: make sure there is only one mesh available

            ! Try to find a suitable mesh variable
            ! We do not have an aggregation table: construct one
            inc_error = dlwqnc_find_var_with_att( ncid, "cf_role", varid, "mesh_topology" )
            if ( inc_error /= nf90_noerr ) then
               write ( lunut , 2540 )
               goto 800
            endif

            inc_error  = nf90_inquire_variable( ncid, varid, mesh_name )
            type_ugrid = type_ugrid_node_crds

            allocate (aggr(nosegl, nolay))
            do j = 1,nolay
               do i = 1,nosegl
                  aggr(i,j) = i + (j-1) * nosegl
               enddo
            enddo
         endif
         write ( lunut , 2550 ) trim(mesh_name)

         ! Get the meshid
         inc_error = nf90_inq_varid( ncid, mesh_name, meshid )
         if ( inc_error /= nf90_noerr ) then
             write ( lunut , 2556 ) trim(mesh_name)
             goto 800
         endif

         inc_error = dlwqnc_copy_var_atts( ncid, ncidmap, nf90_global, nf90_global )
         if ( inc_error /= nf90_noerr ) then
             write ( lunut , 2570 )
             goto 800
         endif

         ! Generate the UUID and store it as an attibute
         call getuuid( uuid )
         inc_error = nf90_put_att( ncidmap, nf90_global, "uuid", uuid )
         if ( inc_error /= nf90_noerr ) then
             write ( lunut , 2571 )
             goto 800
         endif

         ! For now we can simply copy the mesh data
         inc_error = dlwqnc_copy_mesh( ncid, ncidmap, meshid, mesh_name, type_ugrid )
         if ( inc_error /= nf90_noerr ) then
             write ( lunut , 2572 )
             goto 800
         endif

         ! Add a "layer" dimension for DELWAQ and update the IDs
         ! (Must happen after copying the mesh - otherwise the dimension IDs do not match)
         if ( type_ugrid == type_ugrid_node_crds ) then
            inc_error = dlwqnc_create_delwaq_dims( ncidmap, nosegl, nolay, dimids, dimsizes )
            if ( inc_error /= nf90_noerr ) then
               write ( lunut , 2570 )
               goto 800
            endif

            if ( type_ugrid == type_ugrid_face_crds ) then
                noseglid   = dimids(1)
            else
                ! Use the number of faces
                inc_error = nf90_get_att( ncid, meshid, 'face_dimension', dimname )
                if ( inc_error /= nf90_noerr ) then
                    dimname = 'nNetElem' ! Fallback option
                endif
                inc_error = nf90_inq_dimid( ncid, dimname, noseglid )
                if ( inc_error /= nf90_noerr ) then
                    write ( lunut , 2594 ) trim(dimname)
                    goto 800
                endif
            endif

            noseglmesh = dimsizes(noseglid)
            nolaymesh  = nolay
         endif

         ! Update the timestamp
         call date_and_time( values = values )
         write( timestamp, '(i4.4,a,i2.2,a,i2.2, a,i2.2,a,i2.2,a,i2.2,a,f5.3,a,i2.2,a,i2.2)' )
     &        values(1), '-', values(2), '-', values(3), 'T',
     &        values(5), ':', values(6), ':', values(7), ':', values(8)/1000.0,
     &        merge('+','-',values(4)>=0), values(4)/60, ':', mod(values(4),60)

         inc_error = nf90_redef( ncidmap )
         if ( inc_error /= nf90_noerr ) then
             write ( lunut , 2565 )
             goto 800
         endif

         inc_error = nf90_put_att( ncidmap, nf90_global, 'date_created', trim(timestamp) )
         if ( inc_error /= nf90_noerr ) then
             write ( lunut , 2573 )
             goto 800
         endif
         inc_error = nf90_put_att( ncidmap, nf90_global, 'date_modified', trim(timestamp) )
         if ( inc_error /= nf90_noerr ) then
             write ( lunut , 2574 )
             goto 800
         endif

         inc_error = nf90_enddef( ncidmap )
         if ( inc_error /= nf90_noerr ) then
             write ( lunut , 2566 )
             goto 800
         endif

         allocate(laythickness(nolay))
         laythickness = 1.0/real(nolay)  ! Uniform distribution for now
         inc_error = dlwqnc_create_layer_dim( ncidmap, mesh_name, nolay, laythickness, nolayid )
         if ( inc_error /= nf90_noerr ) then
             write ( lunut , 2580 )
             goto 800
         endif

         t0string = moname(4)
         inc_error = dlwqnc_create_wqtime( ncidmap, mesh_name, t0string, timeid, bndtimeid, ntimeid )
         if ( inc_error /= nf90_noerr ) then
             write( lunut , 2581)
             goto 800
         endif


         ! Write output variables and proces library info to NetCDF-file
         ! long name and unit will follow later, they are in the ouput.wrk-file!
         !
         ! Create 3D and only when nolay > 1 also 2D variables. This is controlled by the NetCDF ID for the layer dimension
         !
         allocate(sumconc1(notot1), sumconc2(notot2))
         do iout = 1, notot1
            inc_error = dlwqnc_create_wqvariable( ncidmap, mesh_name, synam1(iout), sydsc1(iout),
     *                           sysnm1(iout), syuni1(iout), ntimeid, noseglid, nolayid, wqid1(iout,1) )
            if ( inc_error /= nf90_noerr ) then
                write( lunut , 2582)
                goto 800
            endif

            if ( nolay > 1 ) then
               inc_error = dlwqnc_create_wqvariable( ncidmap, mesh_name, synam1(iout), sydsc1(iout),
     *                              sysnm1(iout), syuni1(iout), ntimeid, noseglid, dlwqnc_type2d, wqid1(iout,2) )
               if ( inc_error /= nf90_noerr ) then
                  write( lunut , 2582)
                  goto 800
               endif
            else
               wqid1(iout,2) = -1
            endif
            if ( index(syuni1(iout), 'm-2') > 0 ) then
               sumconc1(iout) = .true.
            else
               sumconc1(iout) = .false.
            endif
         enddo
         do iout = 1, notot2
            inc_error = dlwqnc_create_wqvariable( ncidmap, mesh_name, synam2(iout), sydsc2(iout),
     *                           sysnm2(iout), syuni2(iout), ntimeid, noseglid, nolayid, wqid2(iout,1) )
            if ( inc_error /= nf90_noerr ) then
                write( lunut , 2582)
                goto 800
            endif
            if ( nolay > 1 ) then
               inc_error = dlwqnc_create_wqvariable( ncidmap, mesh_name, synam2(iout), sydsc2(iout),
     *                              sysnm2(iout), syuni2(iout), ntimeid, noseglid, dlwqnc_type2d, wqid2(iout,2) )
               if ( inc_error /= nf90_noerr ) then
                  write( lunut , 2582)
                  goto 800
               endif
            endif
            if ( index(syuni2(iout), 'm-2') > 0 ) then
               sumconc2(iout) = .true.
            else
               sumconc2(iout) = .false.
            endif
         enddo

!        Always add volume
!        Note: the standard name for "volume" is fixed for the moment, but the NetCDF standard is
!        far from complete.
         inc_error = dlwqnc_create_wqvariable( ncidmap, mesh_name, 'volume', 'volume (m3)',
     *                        'sea_water_volume', 'm3', ntimeid, noseglid, nolayid, wqidvolume )
         if ( inc_error /= nf90_noerr ) then
            write( lunut , 2583)
            goto 800
         endif
         if ( nolay > 1 ) then
            inc_error = dlwqnc_create_wqvariable( ncidmap, mesh_name, 'volume', 'volume (m3)',
     *                     'sea_water_volume', 'm3', ntimeid, noseglid, dlwqnc_type2d, wqidvolume_2d )
            if ( inc_error /= nf90_noerr ) then
               write( lunut , 2583)
               goto 800
            endif
         endif


         ! Flush after first stage of preparing NetCDF file
         inc_error = nf90_sync(ncidmap)
         if ( inc_error /= nf90_noerr ) then
             write( lunut , 2591)
             goto 800
         endif
      endif

!     Perform output
!     New time record
      inc_error = dlwqnc_write_wqtime( ncidmap, timeid, bndtimeid, mncrec, itime )
      if ( inc_error /= nf90_noerr ) then
         if ( inc_error /= nf90_noerr ) then
             write( lunut , 2590)
             goto 800
         endif
      endif

!     Output arrays
      allocate( dlwq_values(noseglmesh,nolay), dlwq_values_2d(noseglmesh), dlwq_volume_2d(noseglmesh) )

!     Total volume in 2D
      dlwq_volume_2d = 0.0

      !     Write volumes
      do ilay = 1, nolay
         do isegl = 1, noseglmesh
            if (aggr(isegl, ilay).gt.0) then
               dlwq_values(isegl,ilay) = volume(aggr(isegl, ilay))
               if ( nolay .gt. 1 ) then
                  dlwq_volume_2d(isegl) = dlwq_volume_2d(isegl) + volume(aggr(isegl, ilay))
               endif
            else
               dlwq_values(isegl,ilay) = -999.0
               if ( nolay .gt. 1 ) then
                  dlwq_volume_2d(isegl)   = -999.0
               endif
            endif
         enddo
      enddo

      inc_error = dlwqnc_write_wqvariable( ncidmap, wqidvolume, mncrec, dlwq_values)
      if ( inc_error /= nf90_noerr ) then
          write( lunut , 2591)
          goto 800
      endif

      if ( nolay > 1 ) then
         inc_error = dlwqnc_write_wqvariable( ncidmap, wqidvolume_2d, mncrec, dlwq_volume_2d)
         if ( inc_error /= nf90_noerr ) then
            write( lunut , 2591)
            goto 800
         endif
      endif

!     First set of output
      do iout = 1, notot1
         if ( nolay .gt. 1 ) then
            dlwq_values_2d = 0.0
         endif
         do ilay = 1, nolay
            do isegl = 1, noseglmesh
               if (aggr(isegl, ilay).gt.0) then
                  dlwq_values(isegl,ilay) = conc1(iout, aggr(isegl, ilay))
                  if ( nolay .gt. 1 ) then
                     if ( .not. sumconc1(iout) ) then
                        dlwq_values_2d(isegl)   = dlwq_values_2d(isegl) +
     &                                            conc1(iout, aggr(isegl, ilay)) *
     &                                            volume(aggr(isegl, ilay))
                     else
                        dlwq_values_2d(isegl)   = dlwq_values_2d(isegl) +
     &                                            conc1(iout, aggr(isegl, ilay))
                     endif
                  endif
               else
                  dlwq_values(isegl,ilay) = -999.0
                  if ( nolay .gt. 1 ) then
                     dlwq_values_2d(isegl)   = -999.0
                  endif
               endif
            enddo
         enddo

         inc_error = dlwqnc_write_wqvariable( ncidmap, wqid1(iout,1), mncrec, dlwq_values)
         if ( inc_error /= nf90_noerr ) then
             write( lunut , 2591)
             goto 800
         endif

!        Add 2D output in 3D models
         if ( nolay .gt. 1 ) then
            if ( .not. sumconc1(iout) ) then
               do isegl = 1, noseglmesh
                  if(dlwq_volume_2d(isegl).ne.-999.0) then
                     dlwq_values_2d(isegl) = dlwq_values_2d(isegl) / dlwq_volume_2d(isegl)
                  endif
               enddo
            endif

            inc_error = dlwqnc_write_wqvariable( ncidmap, wqid1(iout,2), mncrec, dlwq_values_2d)
            if ( inc_error /= nf90_noerr ) then
               write( lunut , 2591)
               goto 800
            endif
         endif
      enddo

!     Second set of output
      do iout = 1, notot2
         if ( nolay .gt. 1 ) then
            dlwq_values_2d = 0.0
         endif

         do ilay = 1, nolay
            do isegl = 1, noseglmesh
               if (aggr(isegl, ilay).gt.0) then
                  dlwq_values(isegl,ilay) = conc2(iout, aggr(isegl, ilay))
                  if ( nolay .gt. 1 ) then
                     if ( .not. sumconc2(iout) ) then
                        dlwq_values_2d(isegl)   = dlwq_values_2d(isegl) +
     &                                            conc2(iout, aggr(isegl, ilay)) *
     &                                            volume(aggr(isegl, ilay))
                     else
                        dlwq_values_2d(isegl)   = dlwq_values_2d(isegl) +
     &                                            conc2(iout, aggr(isegl, ilay))
                     endif
                  endif
               else
                  dlwq_values(isegl,ilay) = -999.0
                  if ( nolay .gt. 1 ) then
                     dlwq_values_2d(isegl)   = -999.0
                  endif
               endif
            enddo
         enddo

         inc_error = dlwqnc_write_wqvariable( ncidmap, wqid2(iout,1), mncrec, dlwq_values)
         if ( inc_error /= nf90_noerr ) then
             write( lunut , 2591)
             goto 800
         endif

!        Add 2D output in 3D models
         if ( nolay .gt. 1) then
            if (.not. sumconc2(iout) ) then
               do isegl = 1, noseglmesh
                  if(dlwq_volume_2d(isegl).ne.-999.0) then
                     dlwq_values_2d(isegl) = dlwq_values_2d(isegl) / dlwq_volume_2d(isegl)
                  endif
               enddo
            endif

            inc_error = dlwqnc_write_wqvariable( ncidmap, wqid2(iout,2), mncrec, dlwq_values_2d)
            if ( inc_error /= nf90_noerr ) then
               write( lunut , 2591)
               goto 800
            endif
         endif
      enddo

      deallocate( dlwq_values )
      deallocate( dlwq_values_2d )
      deallocate( dlwq_volume_2d )

! Flush after each map write
      inc_error = nf90_sync(ncidmap)
      if ( inc_error /= nf90_noerr ) then
          write( lunut , 2591)
          goto 800
      endif
      goto 900

 800  continue
! There were errors!
      write ( lunut , 2600 ) inc_error
      write ( lunut , 2610 ) trim(nf90_strerror(inc_error))
      call srstop (1)

 900  continue
      if ( timon ) call timstop ( ithandl )
      return

 2510 format ( / ' File containing the grid: ', A )
 2520 format ( / ' NetCDF version: ', A )
 2530 format ( / ' ERROR, opening NetCDF file. Filename: ',A )
 2531 format ( / ' ERROR, reading dimensions from NetCDF file.' )
 2540 format ( / ' ERROR, no variable found with requested attribute "delwaq_role" or "cf_type"' )
 2541 format ( / ' ERROR, reading dimensions from "delwaq_role"/"cf_type" in NetCDF file.' )
 2550 format ( / ' Mesh used for Delwaq output: ', A )
 2555 format ( / ' ERROR, Getting the mesh name failed' )
 2556 format ( / ' Getting the mesh ID failed - variable: ', A )
 2560 format ( / ' Creating the output file failed. Filename:', A )
 2565 format ( / ' ERROR: Reopening NetCDF definition failed' )
 2566 format ( / ' ERROR: Closing NetCDF definition failed' )
 2570 format ( / ' Copying the attributes/dimensions failed' )
 2571 format ( / ' Writing the UUID failed' )
 2572 format ( / ' Copying the mesh data failed' )
 2573 format ( / ' Writing date_created failed' )
 2574 format ( / ' Writing date_modified failed' )
 2575 format ( / ' Creating number of segments per layer (nosegl) variable failed' )
 2576 format ( / ' Writing number of segments per layer (nosegl) failed' )
 2580 format ( / ' Creating layer dimension failed' )
 2581 format ( / ' Creating time dimension failed' )
 2582 format ( / ' Creating variable failed' )
 2583 format ( / ' Creating volume variable failed' )
 2584 format ( / ' Ending definition phase failed' )
 2590 format ( / ' Writing new NetCDF map time failed' )
 2591 format ( / ' Writing new NetCDF map output data failed' )
 2592 format ( / ' Sync on NetCDF file failed' )
 2593 format ( / ' Attribute "face_dimension" in mesh not found' )
 2594 format ( / ' Dimension "',A,'" not found' )
 2600 format ( / ' NetCDF error number: ', I6 )
 2610 format ( / ' NetCDF error message: ', A )
      end
