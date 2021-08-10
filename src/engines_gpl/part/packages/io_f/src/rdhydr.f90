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

module rdhydr_mod
!
!  module declarations
!
!
!  data definition module(s)
!
      use precision_part      ! single and double precision
      use timers
!
!  module procedure(s)
!
      use openfl_mod     ! explicit interface
      use rd_token       ! tokenized reading like in DELWAQ
      use partmem
      use alloc_mod
!
      implicit none      ! force explicit typing
!
   contains
      subroutine rdhydr ( nmax   , mmax   , mnmaxk , nflow  , noseg  ,  &
                          noq    , itime  , itstrt , idelt  , volume ,  &
                          vdiff  , area   , flow   , vol1   , vol2   ,  &
                          flow1  , vdiff1 , update , cellpnt, flowpnt,  &
                          tau    , tau1   , caltau , salin  , salin1 ,  &
                          temper , temper1, nfiles , lunit  , fname  ,  &
                          ftype  , rhowatc)
!
!     READING HYDRODYNAMICS FILE (*.hyd)
!              (initially)
!
!     system administration : r. j. vos
!
!
!     created               : february 1990, by l. postma
!
!     note                  : 3d version may 1996
!                             see the -2 option coupling in loop 35
!                             was not present in vs3.00, but in v2.3 ok
!
!
!     logical unit numbers  : lun    - array with lu-numbers
!
!
!     subroutines called    : stop_exit
!                             parttd.
!                             dlwqbl.
!
!     functions   called    : none.
!
      save
!
!     parameters            :

      integer  (ip), intent(in   ) :: nmax             !< size of first index in the cube
      integer  (ip), intent(in   ) :: mmax             !< size of second index in the cube
      integer  (ip), intent(in   ) :: mnmaxk           !< number of cells in the cube
      integer  (ip), intent(in   ) :: nflow            !< size of 3*3d flow array
      integer  (ip), intent(in   ) :: noseg            !< nr of computational volumes
      integer  (ip), intent(in   ) :: noq              !< total number of exchanges
      integer  (ip), intent(in   ) :: itime            !< current time
      integer  (ip), intent(  out) :: itstrt           !< start of the hydrodynamic files
      integer  (ip), intent(  out) :: idelt            !< time step of hydrodynamic files
      real     (sp), intent(  out) :: volume (mnmaxk)  !< a grid with volumes
      real     (sp), intent(  out) :: vdiff  (mnmaxk)  !< a grid with vertical diffusions
      real     (sp), intent(in   ) :: area   (nflow )  !< a grid with horizontal surfaces
      real     (sp), intent(  out) :: flow   (nflow )  !< a grid with flows
      real     (sp), intent(  out) :: vol1   (noseg )  !< first volume record
      real     (sp), intent(  out) :: vol2   (noseg )  !< second volume record
      real     (sp), intent(  out) :: flow1  ( noq  )  !< flow record in file
      real     (sp), intent(  out) :: vdiff1 (noseg )  !< vertical diffusion record in file
      logical      , intent(  out) :: update           !< values have been updated
      integer  (ip), intent(in   ) :: cellpnt(noseg )  !< backpointering from volumes to grid
      integer  (ip), intent(in   ) :: flowpnt(noq,2 )  !< backpointering from flows to grid
      real     (sp), intent(  out) :: tau    (mnmaxk)  !< tau record on the grid
      real     (sp), intent(  out) :: tau1   (noseg )  !< tau record in file
      logical      , intent(  out) :: caltau           !< should the tau be calculated ?
      real     (sp), intent(  out) :: salin  (mnmaxk)  !< salinity record on the grid
      real     (sp), intent(  out) :: salin1 (noseg )  !< salinity record in file
      real     (sp), intent(  out) :: temper (mnmaxk)  !< temperature record on the grid
      real     (sp), intent(  out) :: temper1(noseg )  !< temperature record in file
      real     (sp), intent(  out) :: rhowatc(noseg )  !< density water calculated from temperature and salinity on the active grid
      integer  (ip), intent(in   ) :: nfiles           !< nr. of files
      integer  (ip), intent(inout) :: lunit(nfiles)    !< unit nrs of all files
      character(* ), intent(inout) :: fname(nfiles)    !< file names of all files
      character(20), intent(inout) :: ftype(2)         !< 'binary'

!     locals

      logical           :: updatv, updatf, updatd, lblock
!
      logical :: first  = .true.
      integer(ip) :: i     , i2    , idelt1 , ifflag , iocond , isflag, kmax
      integer(ip) :: it1   , it2   , max    , mod    , lunut
      integer(ip) :: mnmax                        ! number of cells per layer in the cube
      integer(ip) :: idtimv , itimv1 , itimv2     ! timings of the volumes file
      integer(ip) :: idtimf , itimf1 , itimf2     ! timings of the flow file
      integer(ip) :: idtimd , itimd1 , itimd2     ! timings of the vertical diffusion file
      integer(ip) :: idtimt , itimt1 , itimt2     ! timings of the tau file
      real   (sp) :: depmin
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      real(sp) :: densty  ! AddedDana
      if ( timon ) call timstrt( "rdhydr", ithndl )
!
      lunut = lunit(2)
      depmin = 0.05*nmax*mmax/mnmaxk
      depmin = max(depmin,0.001)
      kmax   = mnmaxk/nmax/mmax
      if ( idelt == -999 ) then
!

         write ( lunut, * ) ' Opening the volume file:', fname(6)(1:len_trim(fname(6)))
         call openfl ( lunit(6), fname(6), ftype(2), 0 )
!        if (iocond  /=  0) goto 100
         write ( lunut, * ) ' Opening the flow   file:', fname(7)(1:len_trim(fname(7)))
         call openfl ( lunit(7), fname(7), ftype(2), 0 )

         if ( lunit(20) .gt. 0 .and. fname(20) .ne. 'none' ) then
            write ( lunut, * ) ' Opening the vdf    file:', fname(20)(1:len_trim(fname(20)))
            call openfl ( lunit(20), fname(20), ftype(2), 3 )
            if ( lunit(20) .eq. 0 ) write ( lunut, * ) ' Warning the vdf file does not exist !'
         else
            lunit(20) = 0
         endif
         if ( lunit(20) .eq. 0 ) vdiff = 0.0

         if ( lunit(21) .gt. 0 .and. fname(21)(1:4) .ne. 'none' ) then
            write ( lunut, * ) ' Opening the tau    file:', fname(21)(1:len_trim(fname(21)))
            call openfl ( lunit(21), fname(21), ftype(2), 3 )
            if ( lunit(21) .eq. 0 ) write ( lunut, * ) ' Warning the tau file does not exist !'
         else
            lunit(21) = 0
         endif
         caltau = .false.
         if ( lunit(21) .eq. 0 ) caltau = .true.

         if ( lunit(22) .gt. 0 .and. fname(22)(1:4) .ne. 'none' ) then
            write ( lunut, * ) ' Opening the salinity file:', fname(22)(1:len_trim(fname(22)))
            call openfl ( lunit(22), fname(22), ftype(2), 3 )
            if ( lunit(22) .eq. 0 ) write ( lunut, * ) ' Warning the salinity file does not exist !'
         else
            lunit(22) = 0
         endif

         if ( lunit(23) .gt. 0 .and. fname(23)(1:4) .ne. 'none' ) then
            write ( lunut, * ) ' Opening the temperature file:', fname(23)(1:len_trim(fname(23)))
            call openfl ( lunit(23), fname(23), ftype(2), 3 )
            if ( lunit(23) .eq. 0 ) write ( lunut, * ) ' Warning the temperature file does not exist !'
         else
            lunit(23) = 0
         endif
!
         read ( lunit(6), iostat = iocond ) it1, vol1
         if (iocond  /=  0) goto 120
         read ( lunit(6), iostat = iocond ) it2, vol2
         if (iocond  /=  0) goto 130
         idelt = it2 - it1
!
         read (lunit(7), iostat = iocond) it1, flow1
         if (iocond  /=  0    ) goto 140
!        read (lunit(7), iostat = iocond) it2, flow1
!        if (iocond  /=  0    ) goto 150
!        idelt = it2 - it1
!        if (idelt   /=  idelt1) goto 160
         itstrt = it1
         rewind (lunit(6))
         rewind (lunit(7))
         volume(cellpnt(:)) = vol1 (:)
         flow = 0.0
         do i = 1, noq
            if ( flowpnt(i,1) .gt. 0 ) flow(flowpnt(i,1)) = flow(flowpnt(i,1)) + flow1(i)
            if ( flowpnt(i,2) .gt. 0 ) flow(flowpnt(i,2)) = flow(flowpnt(i,2)) + flow1(i)
         enddo
      else
!
         if ( first ) then
             ifflag = 1
         else
             ifflag = 0
         endif
         isflag = 1

!.. volumes

         lblock = .false.
         call parttd ( lunit(6), lunut   , itime   , idtimv  , itimv1  ,   &
                       itimv2  , noseg   , mnmaxk  , vol1    , vol2    ,   &
                       volume  , cellpnt , lblock  , fname(6), isflag  ,   &
                       ifflag  , updatv  )

!.. flows
!.. note that flows must be block since no space is reserved for flow-interpolation
         call dlwqfl ( lunit(7), lunut   , itime   , idtimf  , itimf1  ,   &
                       itimf2  , idelt   , noq     , nflow   , flow1   ,   &
                       flow    , flowpnt , fname(7), isflag  , ifflag  ,   &
                       updatf  )

!.. vertical diffusions

         if ( lunit(20) .gt. 0 ) then
            call dlwqbl ( lunit(20), lunut   , itime    , idtimd  , itimd1  ,   &
                          itimd2   , idelt   , noseg    , mnmaxk  , vdiff1  ,   &
                          vdiff    , cellpnt , fname(20), isflag  , ifflag  ,   &
                          updatd  )
            if ( kmax .gt. 1 ) then                                    ! fill the zero last layer with the
               mnmax = nmax*mmax
               do i = mnmaxk-mnmax+1,mnmaxk
                  vdiff(i) = vdiff(i-mnmax)                        ! values above
               end do
            endif
         endif

!.. tau

         if ( .not. caltau ) then
            call dlwqbl ( lunit(21), lunut   , itime    , idtimt , itimt1 ,   &
                          itimt2   , idelt   , noseg    , mnmaxk , tau1   ,   &
                          tau      , cellpnt , fname(21), isflag , ifflag ,   &
                          updatd   )
         endif

!.. salinity

         if ( lunit(22) .gt. 0 ) then
            call dlwqbl ( lunit(22), lunut   , itime    , idtimt , itimt1 ,   &
                          itimt2   , idelt   , noseg    , mnmaxk , salin1 ,   &
                          salin    , cellpnt , fname(22), isflag , ifflag ,   &
                          updatd   )
         endif

!.. temperature

         if ( lunit(23) .gt. 0 ) then
            call dlwqbl ( lunit(23), lunut   , itime    , idtimt , itimt1 ,   &
                          itimt2   , idelt   , noseg    , mnmaxk , temper1,   &
                          temper   , cellpnt , fname(23), isflag , ifflag ,   &
                          updatd   )
         endif

         first  = .false.
         if (itimv1  /=  itimf1) goto 170
!
!.. selfort does not understand this..  think it is Salford after Salford University lp
!
         if ( (updatv.and..not.updatf) .or.  &
              (.not.updatv.and.updatf)     ) goto 180
         update = updatv
!
      endif
!
      do 60 i = 1, mnmaxk

!       limit volume to 5cm

        i2 = mod(i,nmax*mmax)
        if(i2==0) i2 = nmax*mmax
        volume(i) = max(volume(i), area(i2) * depmin)

!       apply scaling to vertical diffusion
!       the .vdf file at the moment contains the D3D-FLOW dicww array in m2/s.
!       typically: doffset = vicmol/sigmol(substance)
!                  dscale  = 1.0   /sigdif(substance)
!                  dminim  = gdp%gdphysco%dicoww       if I interpreted D3D-FLOW correctly (lp)
!       off course only do this if the vertical diffusion was updated with the file-values

   60 continue
!
!     end of routine
!
      if ( lunit(22) .gt. 0 .and. lunit(23) .gt. 0 ) then
         do i = 1, noseg
            rhowatc(i) = densty(max(0.0e0,salin1(i)), temper1(i))
         enddo
      else
         do i = 1, noseg
            rhowatc(i) = rhow
         enddo
      endif
      
      if ( timon ) call timstop ( ithndl )
      return
!
!     exit with formats
!
  120 write (lunut, *) ' Error 4403. Reading the volume file:', fname(6)
      call stop_exit(1)
  130 write (lunut, *) ' Error 4404. Reading the volume file:', fname(6)
      call stop_exit(1)
  140 write (lunut, *) 'Error 4405. Reading the flow file  :', fname(7)(:12)
      call stop_exit(1)
  150 write (lunut, *) 'Error 4406. Reading the flow file  :', fname(7)(:12)
      call stop_exit(1)
  160 write (lunut, *) ' Error 4409. Time steps do not match!'
      write (lunut, *) '             Time step in volume file:', idelt1
      write (lunut, *) '             Time step in flow   file:', idelt
      write (lunut, *) '             Time step in depth  file:', idelt
      call stop_exit(1)
  170 write (lunut, *) ' Error 4410. Times in files do not match!'
      write (lunut, *) '             Volumes:',itimv1,' flows:',itimf1
      call stop_exit(1)
  180 write (lunut, *) ' Error 4411. Files have not both been updated  '
      write (lunut, *) ' Volumes-update:',updatv,' flows-update:',updatf
      call stop_exit(1)
      end subroutine
end module
