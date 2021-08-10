!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2019.!
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

! $Id: caching.f90 65914 2020-01-30 12:49:20Z markus $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/caching.f90 $

! NOTES:
! - Observation points can be "moving" - does that have influence on the data that need to be cached?
! - How about the global index to these items?
!


!> Manages the caching file - store and retrieve the grid-based information.
module unstruc_caching
    use precision
    use m_observations, only: numobs, xobs, yobs, locTpObs, kobs
    use m_monitoring_crosssections, only: crs, tcrs
    !use m_crspath, only: tcrspath
    use md5_checksum

    implicit none

    logical, private :: cache_success

    character(len=20), dimension(10), private :: section = ['OBSERVATIONS        ', &
                                                            'FIXED WEIRS         ', &
                                                            'CROSS_SECTIONS      ', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890', &
                                                            '12345678901234567890']
    integer, parameter, private :: key_obs = 1
    integer, parameter, private :: key_fixed_weirs = 2
    integer, parameter, private :: key_cross_sections = 3

    double precision, dimension(:), allocatable, private :: cache_xobs
    double precision, dimension(:), allocatable, private :: cache_yobs
    double precision, dimension(:), allocatable, private :: cache_xpl_fixed
    double precision, dimension(:), allocatable, private :: cache_ypl_fixed
    double precision, dimension(:), allocatable, private :: cache_dsl_fixed
    integer, dimension(:), allocatable, private          :: cache_locTpObs
    integer, dimension(:), allocatable, private          :: cache_kobs
    integer, dimension(:), allocatable, private          :: cache_ilink_fixed
    integer, dimension(:), allocatable, private          :: cache_ipol_fixed
    integer, dimension(:), allocatable, private          :: cache_linklist
    integer, dimension(:), allocatable, private          :: cache_ipol

    type(tcrs), dimension(:), allocatable               :: cache_cross_sections


    character(len=30), private :: version_string = "D-Flow FM, cache file, 1.0"
    character(len=14), private :: md5current



contains
!> Check that the caching file contained compatible information
logical function cacheRetrieved()
    cacheRetrieved = cache_success
end function cacheRetrieved

!> Load the information from the caching file - if any
subroutine loadCachingFile( filename, netfile, usecaching )
    character(len=*), intent(in   ) :: filename      !< Name of MDU file (used to construct the name of the caching file)
    character(len=*), intent(in   ) :: netfile       !< Full name of the network file
    integer,          intent(in   ) :: usecaching    !< Use the cache file if possible (1) or not (0)

    integer :: lun
    integer :: ierr
    integer :: number, number_links, number_sections
    character(len=30) :: version_file
    character(len=20) :: key
    character(len=14) :: md5checksum
    logical :: okay
    logical :: success

    cache_success = .false.

    if ( usecaching /= 1 ) then
        return
    endif

    !
    ! Allocate the arrays to zero length
    !
    if (.not. allocated(cache_xobs)) then
       allocate( cache_xobs(0), cache_yobs(0), cache_xpl_fixed(0), cache_ypl_fixed(0), cache_dsl_fixed(0), &
                 cache_locTpObs(0), cache_kobs(0), cache_ilink_fixed(0), cache_ipol_fixed(0) )
    endif

    open( newunit = lun, file = trim(filename) // ".cache", status = "old", access = "stream", iostat = ierr )

    !
    ! Apparently there is no caching file, so return without further processing
    ! But for writing the caching file later, determine the checksum now
    !
    if ( ierr /= 0 ) then
        call md5file( netfile, md5current, success )
        return
    endif

    !
    ! Load the version number and the MD5 checksum - useable at all?
    !
    read( lun, iostat = ierr ) version_file, md5checksum

    if ( ierr /= 0 ) then
        return
    endif

    if ( version_file /= version_string ) then
        !
        ! As there is no history of versions yet, the version in the file
        ! should match exactly
        !
        return
    endif

    !
    ! Determine the MD5 checksum for the network file - it must match the
    ! checksum in the cache file
    !
    call md5file( netfile, md5current, success )

    if ( md5checksum /= md5current .or. .not. success ) then
        close( lun )
        return
    endif

    !
    ! Load the observation points:
    ! Copy the node numbers when successful
    !
    okay = .true.

    read( lun, iostat = ierr ) key, number

    if ( ierr /= 0 .or. key /= section(key_obs) ) then
        close( lun )
        return
    endif

    deallocate( cache_xobs, cache_yobs, cache_locTpObs, cache_kobs )

    allocate( cache_xobs(number), cache_yobs(number), cache_kobs(number), cache_locTpObs(number) )

    if ( number > 0 ) then
        read( lun, iostat = ierr ) cache_xobs      ; okay = ierr == 0
        read( lun, iostat = ierr ) cache_yobs      ; okay = okay .and. ierr == 0
        read( lun, iostat = ierr ) cache_locTpObs  ; okay = okay .and. ierr == 0
        read( lun, iostat = ierr ) cache_kobs      ; okay = okay .and. ierr == 0
    endif

    if ( .not. okay ) then
        close( lun )
        return
    endif

    !
    ! Load the information on the fixed weirs:
    ! Copy the node numbers when successful
    !
    read( lun, iostat = ierr ) key, number, number_links

    if ( ierr /= 0 .or. key /= section(key_fixed_weirs) ) then
        close( lun )
        return
    endif

    deallocate( cache_xpl_fixed, cache_ypl_fixed, cache_ilink_fixed, cache_ipol_fixed, cache_dsl_fixed )

    allocate( cache_xpl_fixed(number), cache_ypl_fixed(number) )
    allocate( cache_ilink_fixed(number_links), cache_ipol_fixed(number_links), cache_dsl_fixed(number_links) )

    if ( number > 0 ) then
        read( lun, iostat = ierr ) cache_xpl_fixed   ; okay = ierr == 0
        read( lun, iostat = ierr ) cache_ypl_fixed   ; okay = okay .and. ierr == 0
        read( lun, iostat = ierr ) cache_ilink_fixed ; okay = okay .and. ierr == 0
        read( lun, iostat = ierr ) cache_ipol_fixed  ; okay = okay .and. ierr == 0
        read( lun, iostat = ierr ) cache_dsl_fixed   ; okay = okay .and. ierr == 0
    endif

    if ( .not. okay ) then
        close( lun )
        return
    endif

    !
    ! Load the information on the cross-sections:
    ! Copy all information when successful
    !
    read( lun, iostat = ierr ) key, number, number_links

    if ( ierr /= 0 .or. key /= section(key_cross_sections) ) then
        close( lun )
        return
    endif

    allocate( cache_cross_sections(number) )
    allocate( cache_linklist(number_links) )
    allocate( cache_ipol(number_links) )
    call loadCachedSections( lun, cache_linklist, cache_ipol, cache_cross_sections, ierr )
    if ( ierr /= 0 ) then
        close( lun )
        return
    endif

    !
    ! All cached values were loaded, so all is well
    !
    close( lun )
    cache_success = .true.

end subroutine loadCachingFile

!> Load cached cross sections from a caching file
subroutine loadCachedSections( lun, linklist, ipol, sections, ierr )
    integer,                  intent(in   ) :: lun       !< LU-number of the caching file
    integer, dimension(:),    intent(  out) :: linklist  !< Cached list of crossed flow links
    integer, dimension(:),    intent(  out) :: ipol      !< Cached polygon administration
    type(tcrs), dimension(:), intent(  out) :: sections  !< Array of cross-sections to be filled
    integer,                  intent(  out) :: ierr      !< Error code

    integer                                 :: i, np, nlink
    logical                                 :: okay

    read( lun, iostat = ierr ) linklist
    if ( ierr /= 0 ) then
        return
    endif

    read( lun, iostat = ierr ) ipol
    if ( ierr /= 0 ) then
        return
    endif

    do i = 1,size(sections)
        read( lun, iostat = ierr ) np, nlink

        sections(i)%path%np  = np
        sections(i)%path%lnx = nlink
        allocate( sections(i)%path%xp(np), sections(i)%path%yp(np),  &
                  sections(i)%path%zp(np), sections(i)%path%indexp(nlink), &
                  sections(i)%path%xk(2,nlink), sections(i)%path%yk(2,nlink), &
                  sections(i)%path%wfp(nlink), &
                  sections(i)%path%iperm(nlink), sections(i)%path%wfk1k2(nlink), &
                  sections(i)%path%sp(nlink), sections(i)%path%ln(nlink) )

        if ( nlink > 0 ) then
            read( lun, iostat = ierr ) sections(i)%path%xp, sections(i)%path%yp,  &
                                       sections(i)%path%zp, sections(i)%path%indexp, &
                                       sections(i)%path%xk, sections(i)%path%yk, &
                                       sections(i)%path%wfp, &
                                       sections(i)%path%iperm, sections(i)%path%wfk1k2, &
                                       sections(i)%path%sp, sections(i)%path%ln
        else
            if ( np > 0 ) then
                read( lun, iostat = ierr ) sections(i)%path%xp, sections(i)%path%yp,  &
                                           sections(i)%path%zp
            endif
        endif
        if ( ierr /= 0 ) then
            exit
        endif
    enddo
end subroutine loadCachedSections

!> Save the link list of crossed flow links for later storage in the caching file
subroutine saveLinklist( length, linklist, ipol )
    integer,                  intent(in   ) :: length    !< Length of the list of crossed flow links
    integer, dimension(:),    intent(in   ) :: linklist  !< List of crossed flow links to be saved
    integer, dimension(:),    intent(in   ) :: ipol      !< Polygon administration

    cache_linklist = linklist(1:length)
    cache_ipol     = ipol(1:length)
end subroutine saveLinklist

!> Store the grid-based information in the caching file
subroutine storeCachingFile( filename, usecaching )
    character(len=*), intent(in   ) :: filename            !< Name of the MDU file (to construct the name of the caching file)
    integer,          intent(in   ) :: usecaching          !< Write the caching file (1) or not (0) - in accordance with the user setting

    integer :: lun
    integer :: ierr

    cache_success = .false.

    !
    ! If no caching should be used, dispense with writing the caching file
    !
    if ( usecaching /= 1 ) then
        return
    endif

    open( newunit = lun, file = trim(filename) // ".cache", access = "stream", status = "old", action = 'read',  iostat = ierr )

    if ( ierr == 0 ) then
        close( lun, status = "delete" )
    endif
    open( newunit = lun, file = trim(filename) // ".cache", access = "stream" )

    !
    ! Store version string and checksum (already determined at start-up)
    !
    write( lun ) version_string, md5current

    !
    ! Store the observation points
    !
    write( lun ) section(key_obs), numobs
    if ( numobs > 0 ) then
        write( lun ) xobs(1:numobs), yobs(1:numobs), locTpObs(1:numobs), kobs(1:numobs)
    endif

    !
    ! Store the fixed weirs data
    !
    write( lun ) section(key_fixed_weirs), size(cache_xpl_fixed), size(cache_ilink_fixed)

    if ( size(cache_xpl_fixed) > 0 ) then
        write( lun ) cache_xpl_fixed, cache_ypl_fixed, cache_ilink_fixed, cache_ipol_fixed, cache_dsl_fixed
    endif

    !
    ! Store the data for the cross-sections
    !
    write( lun ) section(key_cross_sections), size(crs)
    call storeSections( lun, crs, cache_linklist, cache_ipol )

    !
    ! We are done, so close the file
    !
    close( lun )

end subroutine storeCachingFile

!> Store cross sections to a caching file
subroutine storeSections( lun, sections, linklist, ipol )
    integer, intent(in)                  :: lun       !< LU-number of the caching file
    type(tcrs), dimension(:), intent(in) :: sections  !< Array of cross-sections to be filled
    integer, dimension(:), intent(in)    :: linklist  !< List of crossed flow links
    integer, dimension(:), intent(in)    :: ipol      !< Polygon administration

    integer                              :: i, np, nlink

    write( lun ) size(linklist)
    write( lun ) linklist
    write( lun ) ipol

    do i = 1,size(sections)
        np    = sections(i)%path%np
        nlink = sections(i)%path%lnx
        write( lun ) sections(i)%path%np, sections(i)%path%lnx

        if ( nlink > 0 ) then
            write( lun ) sections(i)%path%xp(1:np), sections(i)%path%yp(1:np),  &
                         sections(i)%path%zp(1:np), sections(i)%path%indexp(1:nlink), &
                         sections(i)%path%xk(:,1:nlink), sections(i)%path%yk(:,1:nlink), &
                         sections(i)%path%wfp(1:nlink), &
                         sections(i)%path%iperm(1:nlink), sections(i)%path%wfk1k2(1:nlink), &
                         sections(i)%path%sp(1:nlink), sections(i)%path%ln(1:nlink)
        else
            if ( np > 0 ) then
                write( lun ) sections(i)%path%xp(1:np), sections(i)%path%yp(1:np),  &
                             sections(i)%path%zp(1:np)
            endif
        endif
    enddo
end subroutine storeSections

!> Copy the cached network information for observation points
subroutine copyCachedObservations( success )
    logical, intent(  out) :: success             !< The cached information was compatible if true

    success = .false.
    if ( cache_success ) then
        !
        ! Check the number of observations
        !
        if ( numobs /= size(cache_xobs) ) then
            return
        endif
        !
        ! Check that the coordinates and the type are identical to the cached values
        !
        if ( all( cache_xobs == xobs(1:numobs) ) .and. all( cache_yobs == yobs(1:numobs) ) .and. &
             all( cache_locTpObs == locTpObs(1:numobs) ) ) then
            success        = .true.
            kobs(1:numobs) = cache_kobs
        endif
    endif
end subroutine copyCachedObservations

!> Copy the cached network information for cross-sections
subroutine copyCachedCrossSections( linklist, ipol, success )
    integer, dimension(:), allocatable, intent(  out) :: linklist            !< Cached list of crossed flow links
    integer, dimension(:), allocatable, intent(  out) :: ipol                !< Polygon administration
    logical,                            intent(  out) :: success             !< The cached information was compatible if true

    integer                :: i, np

    success = .false.

    if ( cache_success ) then
        !
        ! Check the number of observations
        !
        if ( size(crs) /= size(cache_cross_sections) ) then
            return
        endif
        !
        ! Check that the coordinates and the type are identical to the cached values
        ! Note: no check on zp, it seems filled with arbitrary data (at least in 2D?)
        !
        success = .true.
        do i = 1,size(cache_cross_sections)
            np = cache_cross_sections(i)%path%np
            if ( np /= crs(i)%path%np ) then
                success        = .false.
                exit
            endif
            if ( any( cache_cross_sections(i)%path%xp(1:np) /= crs(i)%path%xp(1:np) ) .or. &
                 any( cache_cross_sections(i)%path%yp(1:np) /= crs(i)%path%yp(1:np) ) ) then
                success        = .false.
                exit
            endif
        enddo

        if ( success ) then
            linklist = cache_linklist
            ipol     = cache_ipol

            do i = 1,size(cache_cross_sections)
                ! Rely on automatic (re)allocation)
                crs(i)%path%np     = cache_cross_sections(i)%path%np
                crs(i)%path%lnx    = cache_cross_sections(i)%path%lnx
                crs(i)%path%indexp = cache_cross_sections(i)%path%indexp
                crs(i)%path%xk     = cache_cross_sections(i)%path%xk
                crs(i)%path%yk     = cache_cross_sections(i)%path%yk
                crs(i)%path%wfp    = cache_cross_sections(i)%path%wfp
                crs(i)%path%iperm  = cache_cross_sections(i)%path%iperm
                crs(i)%path%wfk1k2 = cache_cross_sections(i)%path%wfk1k2
                crs(i)%path%sp     = cache_cross_sections(i)%path%sp
                crs(i)%path%ln     = cache_cross_sections(i)%path%ln
            enddo
        endif
    endif
end subroutine copyCachedCrossSections

!> Copy the cached information on fixed weirs
subroutine copyCachedFixedWeirs( npl, xpl, ypl, number_links, iLink, iPol, dSL, success )
    integer, intent(in   )                         :: npl                !< Number of vertices in the polygons making up the weirs
    double precision, dimension(:), intent(in   )  :: xpl                !< X-coordinates of the vertices for the weirs
    double precision, dimension(:), intent(in   )  :: ypl                !< Y-coordinates of the vertices for the weirs

    integer, intent(  out)                         :: number_links       !< Number of links that was cached
    double precision, dimension(:), intent(  out)  :: dSL                !< Distances along the links that were cached
    integer, dimension(:), intent(  out)           :: iLink              !< Cached lInkage information
    integer, dimension(:), intent(  out)           :: iPol               !< Cached polygon information
    logical, intent(  out)                         :: success            !< The cached information was compatible if true

    success = .false.
    if ( cache_success ) then
        !
        ! Check the number of coordinate pairs
        !
        if ( npl /= size(cache_xpl_fixed) ) then
            return
        endif
        !
        ! Check that the coordinates are identical to the cached values
        !
        if ( all( cache_xpl_fixed == xpl(1:npl) ) .and. all( cache_ypl_fixed == ypl(1:npl) ) ) then
            success      = .true.
            number_links = size(cache_iLink_fixed)
            iLink(1:number_links) = cache_iLink_fixed
            iPol(1:number_links)  = cache_iPol_fixed
            dSL(1:number_links)   = cache_dSL_fixed
        endif
    endif
end subroutine copyCachedFixedWeirs

!> cacheFixedWeirs:
!>     The arrays for fixed weirs are partly local - they do not reside in a
!>     module, so explicitly store them when we have the actual data
!
subroutine cacheFixedWeirs( npl, xpl, ypl, number_links, iLink, iPol, dSL )
    integer, intent(in   )                        :: npl             !< Number of vertices in the polygons making up the weirs
    integer, intent(in   )                        :: number_links    !< Number of links that is to be cached
    double precision, dimension(:), intent(in   ) :: xpl             !< X-coordinates of the vertices for the weirs
    double precision, dimension(:), intent(in   ) :: ypl             !< Y-coordinates of the vertices for the weirs
    double precision, dimension(:), intent(in   ) :: dSL             !< Distances along the links that are to be cached
    integer, dimension(:), intent(in   )          :: iLink           !< LInkage information to be cached
    integer, dimension(:), intent(in   )          :: iPol            !< Polygon information to be cached

    cache_xpl_fixed   = xpl(1:npl)
    cache_ypl_fixed   = ypl(1:npl)
    cache_iLink_fixed = iLink(1:number_links)
    cache_iPol_fixed  = iPol(1:number_links)
    cache_dSL_fixed   = dSL(1:number_links)
end subroutine cacheFixedWeirs

end module unstruc_caching
