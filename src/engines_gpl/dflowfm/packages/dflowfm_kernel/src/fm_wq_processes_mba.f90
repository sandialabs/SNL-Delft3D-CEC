!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2018-2020.!
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

! $Id: fm_wq_processes_mba.f90 65778 2020-01-14 14:07:42Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/fm_wq_processes_mba.f90 $
   
   subroutine mba_init()
   
   use m_alloc
   use m_fm_wq_processes
   use m_partitioninfo
   use m_flowtimes, only: tstart_user
   use m_flowgeom, only: Ndxi, Lnxi, ln, lne2ln
   use unstruc_model, only: md_ident, md_ident_sequential, getoutputdir
   use m_flowexternalforcings
   use unstruc_files

   implicit none
   
   integer :: isys, imba, i, j, istart, ibnd, isrc, L, LL, Lf, kk1, kk2, ba1, ba2, to, from
   logical :: writebalance
   character(len=64)  :: ident !< Identifier of the model, used as suggested basename for some files. (runid)

   jamba = 1
   timembastart = tstart_user ! when DFM doesn't start at t=0.0??
   timembastarttot = timembastart
   itimembastart = nint(tstart_user)
   itimembastarttot = itimembastart
   
   flxdmp = 0.0
   flxdmptot = 0.0

!  Allocate the massbalance, flux and derivative arrays
   nombabnd = nomba + nopenbndsect

   call realloc(mbaarea, nomba, keepExisting=.false., fill=0d0)

   call realloc(mbavolumebegin   , nomba, keepExisting=.false., fill=0d0)
   call realloc(mbavolumebegintot, nomba, keepExisting=.false., fill=0d0)
   call realloc(mbavolumeend     , nomba, keepExisting=.false., fill=0d0)
      
   call realloc(mbaflowhor, [2, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
   call realloc(mbaflowhortot, [2, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
   call realloc(mbaflowsorsin, [2, numsrc], keepExisting=.false., fill=0d0)
   call realloc(mbaflowsorsintot, [2, numsrc], keepExisting=.false., fill=0d0)

   call realloc(mbamassbegin   , [notot, nomba], keepExisting=.false., fill=0d0)
   call realloc(mbamassbegintot, [notot, nomba], keepExisting=.false., fill=0d0)
   call realloc(mbamassend     , [notot, nomba], keepExisting=.false., fill=0d0)
      
   call realloc(mbafluxhor, [2, nosys, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
   call realloc(mbafluxhortot, [2, nosys, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
   call realloc(mbafluxsorsin, [2, 2, notot, numsrc], keepExisting=.false., fill=0d0)
   call realloc(mbafluxsorsintot, [2, 2, notot, numsrc], keepExisting=.false., fill=0d0)

   if ( jampi.eq.1 ) then
      call realloc(mbavolumereduce  , nomba, keepExisting=.false., fill=0d0)
      call realloc(mbaflowhorreduce , [2, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
      call realloc(mbaflowsorsinreduce, [2, numsrc], keepExisting=.false., fill=0d0)
      call realloc(mbamassreduce    , [notot, nomba], keepExisting=.false., fill=0d0)
      call realloc(mbafluxhorreduce , [2, nosys, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
      call realloc(mbafluxsorsinreduce, [2, 2, notot, numsrc], keepExisting=.false., fill=0d0)
   end if

   ibflag = 1

!  Determine 2D pointers fo links (from balance area to balance area)
   nombaln = 0 
   do LL=1,Lnxi
      kk1=ln(1,LL)
      kk2=ln(2,LL)
      ba1=mbadef(kk1)
      ba2=mbadef(kk2)
      ! check on ghosts!
      if ( jampi.eq.1 ) then
!        if neither is in my domain, don't use it
         if ( idomain(kk1).ne.my_rank .and. idomain(kk2).ne.my_rank ) cycle
         if ( idomain(kk1).lt.my_rank .or. idomain(kk2).lt.my_rank ) cycle
      end if
      if (ba1.ne.ba2) then
         nombaln = nombaln + 1
         call realloc(mbalnlist, nombaln, keepExisting=.true., fill=LL)
         call realloc(mbalnfromto, [2, nombaln], keepExisting=.true., fill=0)
         mbalnfromto(1, nombaln) = ba1
         mbalnfromto(2, nombaln) = ba2
      endif
   enddo
      
   if (nopenbndsect.gt.0) then
      istart = 1
      call realloc(mbaname, nombabnd, keepExisting=.true., fill=' ')
      do ibnd=1,nopenbndsect
         mbaname(nomba + ibnd) = 'bnd_'//openbndname(ibnd)
         do LL = istart, nopenbndlin(ibnd)
            L  = openbndlin(LL)
            Lf = lne2ln(L)
            ! check on ghosts!
            if ( jampi.eq.1 ) then
               if ( idomain(ln(2,Lf)).ne.my_rank) cycle
            end if
            nombaln = nombaln + 1
            call realloc(mbalnlist, nombaln, keepExisting=.true., fill=Lf)
            call realloc(mbalnfromto, [2, nombaln], keepExisting=.true., fill=0)
            mbalnfromto(1, nombaln) = nomba + ibnd
            mbalnfromto(2, nombaln) = mbadef(ln(2,Lf))
         enddo
         istart = nopenbndlin(ibnd) + 1
      enddo
   endif
      
   call realloc(mbalnused, [nomba, nombabnd], keepExisting=.true., fill=0)
   do imba = 1, nombaln
      to = mbalnfromto(1, imba)
      from = mbalnfromto(2, imba)
      if ( to <= nomba) then
         mbalnused(to,from) = mbalnused(to,from) + 1
      endif
      if ( from <= nomba) then
         mbalnused(from,to) = mbalnused(from,to) + 1
      endif
   enddo

   if (jampi.eq.1) then
      call reduce_int_array_sum(nomba * nombabnd, mbalnused)
   endif
      
   call realloc(mbasorsin, [2, numsrc], keepExisting=.true., fill=0)
   call realloc(mbasorsinout, [2, numsrc], keepExisting=.true., fill=0)
   do isrc = 1, numsrc
      kk1    = ksrc(1,isrc)                   ! 2D pressure cell nr FROM
      kk2    = ksrc(4,isrc)                   ! 2D pressure cell nr TO
      if(kk1 > 0) then
         mbasorsin(1,isrc) = mbadef(kk1)
         if ( jampi.eq.1 ) then
            if ( idomain(kk1) /= my_rank ) mbasorsin(1,isrc) = 0
         endif
      endif
      if(kk2 > 0) then
         mbasorsin(2,isrc) = mbadef(kk2)
         if ( jampi.eq.1 ) then
            if ( idomain(kk2) /= my_rank ) mbasorsin(2,isrc) = 0
         endif
      endif
      mbasorsinout(1,isrc) = mbasorsin(1,isrc)
      mbasorsinout(2,isrc) = mbasorsin(2,isrc)
   enddo

   if (jampi.eq.1) then
      call reduce_int_array_sum(2 * numsrc, mbasorsinout)
   endif

   call realloc(flxdmp, [2,nflux, nomba], keepExisting=.false., fill=0.0d0 )       !< Fluxes at dump segments
   call realloc(flxdmpreduce, [2,nflux, nomba], keepExisting=.false., fill=0.0d0 )       !< Fluxes at dump segments
   call realloc(flxdmptot, [2,nflux, nomba], keepExisting=.false., fill=0.0d0 )       !< Fluxes at dump segments

   call mba_sum_area(nomba, mbaarea)
   call mba_sum(nomba, notot, mbavolumebegin, mbamassbegin)
   if ( jampi.eq.1 ) then
      call reduce_double_sum(nomba, mbaarea, mbavolumereduce)
      do imba =1, nomba
         mbaarea(imba) = mbavolumereduce(imba)
      enddo
      call reduce_double_sum(nomba, mbavolumebegin, mbavolumereduce)
      do imba =1, nomba
         mbavolumebegin(imba) = mbavolumereduce(imba)
      enddo
      call reduce_double_sum(notot * nomba, mbamassbegin, mbamassreduce)
      do imba =1, nomba
         do isys = 1, notot
            mbamassbegin(isys, imba) = mbamassreduce(isys, imba)
         enddo
      enddo
   endif
   
   do imba = 1, nomba
      mbavolumebegintot(imba) = mbavolumebegin(imba)
   end do

   do imba = 1, nomba
      do isys=1,notot
         mbamassbegintot(isys,imba) = mbamassbegin(isys,imba)
      end do
   end do

!  write mbahis to a his files (for now) and ascii bal-files
   writebalance = .true.
   if ( jampi.eq.1 ) then
!     in MPI mode      
      if (my_rank.ne.0) then
!        this is not the main node that writes the full balance over all domains, switch of writing
         writebalance = .false.
      else
!        use the original sequential ident without domain number
         ident = md_ident_sequential
      endif
   else
!     not in MPI mode, we can use ident
      ident = md_ident
   endif

   if (writebalance) then
!      open(newunit=lunmbahis,file=trim(getoutputdir())//trim(ident)//'_mba.his', &
!           form='unformatted', access='stream', status='replace')
!      call mba_write_his_header(lunmbahis)
      open(newunit=lunmbabal,file=defaultfilename('wq_bal'))
      call mba_write_bal_header(lunmbabal, nosys, notot, nomba, mbaname, syname_sub, nflux, &
                                totfluxsys, stochi, fluxname, fluxprocname, nfluxsys, fluxsys)

!      open(newunit=lunmbatothis,file=trim(getoutputdir())//trim(ident)//'_mbatot.his', &
!           form='unformatted', access='stream', status='replace')
!      call mba_write_his_header(lunmbatothis)
   endif
   
   end subroutine mba_init

   subroutine mba_update(time)
   use m_fm_wq_processes
   use m_partitioninfo
   use m_flowexternalforcings, only: numsrc, srcname

   implicit none

   double precision, intent(in) :: time !< time     for waq in seconds

   integer :: isys, imba, jmba, iflx, isrc, j
   logical :: writebalance

   itimembaend = int(time)
   timembaend = time

!  New total volumes and masses
   call mba_sum(nomba, notot, mbavolumeend, mbamassend)

!  If in parallel mode, reduce arrays
   writebalance = .true.
   if ( jampi.eq.1 ) then
      if (my_rank.ne.0) writebalance = .false.

      call reduce_double_sum(nomba, mbavolumeend, mbavolumereduce)
      do imba =1, nomba
         mbavolumeend(imba) = mbavolumereduce(imba)
      enddo
      call reduce_double_sum(2 * nombabnd * nombabnd, mbaflowhor, mbaflowhorreduce)
      do imba = 1, nombabnd
         do jmba = 1, nombabnd
            mbaflowhor(1:2, imba, jmba) = mbaflowhorreduce(1:2, imba, jmba)
         enddo
      enddo
      
      call reduce_double_sum(notot * nomba, mbamassend, mbamassreduce)
      do imba =1, nomba
         do isys = 1, notot
            mbamassend(isys, imba) = mbamassreduce(isys, imba)
         enddo
      enddo

      call reduce_double_sum(2 * nosys * nombabnd * nombabnd, mbafluxhor, mbafluxhorreduce)
      do imba = 1, nombabnd
         do jmba = 1, nombabnd
            do isys = 1, nosys
               mbafluxhor(1:2, isys, imba, jmba) = mbafluxhorreduce(1:2, isys, imba, jmba)
            enddo
         enddo
      enddo

      call reduce_double_sum(2 * numsrc, mbaflowsorsin, mbaflowsorsinreduce)
      do isrc = 1, numsrc
         mbaflowsorsin(1:2,isrc) = mbaflowsorsinreduce(1:2,isrc)
      enddo

      call reduce_double_sum(2 * 2 * notot * numsrc, mbafluxsorsin, mbafluxsorsinreduce)
      do isrc = 1, numsrc
         do isys = 1, nosys
            mbafluxsorsin(1:2,1:2,isys,isrc) = mbafluxsorsinreduce(1:2,1:2,isys,isrc)
         enddo
      enddo

      call reduce_double_sum(2 * nflux * nomba, flxdmp, flxdmpreduce)
      do imba = 1, nomba
         do iflx = 1, nflux
            flxdmp(1:2, iflx, imba) = flxdmpreduce(1:2, iflx, imba)
         enddo
      enddo
   endif

   if (writebalance) then
!      call mba_write_his_time_step(lunmbahis, itimembastart, nosys, notot, nomba, nombabnd, nflux, totfluxsys, &
!                                   mbavolumebegin, mbavolumeend, mbaflowhor, &
!                                   mbamassbegin, mbamassend, mbafluxhor, &
!                                   flxdmp, stochi, nfluxsys, fluxsys)

      call mba_write_bal_time_step(lunmbabal, timembastart, timembaend, nosys, notot, nomba, nombabnd, nflux, totfluxsys, &
                                   mbaname, syname_sub, mbalnused, numsrc, srcname, mbasorsinout, &
                                   mbaarea, mbavolumebegin, mbavolumeend, mbaflowhor, mbaflowsorsin, &
                                   mbamassbegin, mbamassend, mbafluxhor, mbafluxsorsin, &
                                   flxdmp, stochi, fluxname, nfluxsys, fluxsys)
   endif

   ! Store end volumes and masses as begin volumes and masses for the next balance output step
   itimembastart = itimembaend
   timembastart = timembaend
   do imba = 1, nomba
      mbavolumebegin(imba) = mbavolumeend(imba)
   end do

   do imba = 1, nomba
      do isys=1,notot
         mbamassbegin(isys,imba) = mbamassend(isys,imba)
      end do
   end do

   ! add fluxes to the full calculation period fluxes
   do imba = 1, nombabnd
      do jmba = 1, nombabnd
         mbaflowhortot(1:2, imba, jmba) = mbaflowhortot(1:2, imba, jmba) + mbaflowhor(1:2, imba, jmba)
      enddo
   enddo

   do isrc = 1, numsrc
      mbaflowsorsintot(1:2,isrc) = mbaflowsorsintot(1:2,isrc) + mbaflowsorsin(1:2,isrc)
   enddo

   do imba = 1, nombabnd
      do jmba = 1, nombabnd
         do isys=1,nosys
            mbafluxhortot(1:2, isys, imba, jmba) = mbafluxhortot(1:2, isys, imba, jmba) + mbafluxhor(1:2, isys, imba, jmba)
         enddo
      enddo
   enddo

   do isrc = 1, numsrc
      do isys=1,nosys
         mbafluxsorsintot(1:2,1:2,isys,isrc) = mbafluxsorsintot(1:2,1:2,isys,isrc) + mbafluxsorsin(1:2,1:2,isys,isrc)
      enddo
   enddo
!   double precision, allocatable            :: mbaflowsorsin(:,:,:)   ! periodical flow from source sinks
!   double precision, allocatable            :: mbaflowsorsintot(:,:,:)! total flow from source sinks
!   double precision, allocatable            :: mbafluxsorsin(:,:,:,:)    ! periodical fluxes from source sinks
!   double precision, allocatable            :: mbafluxsorsintot(:,:,:,:) ! total fluxes from source sinks

   do imba = 1, nomba
      do iflx = 1, nflux
         flxdmptot(1:2, iflx, imba) = flxdmptot(1:2, iflx, imba) + flxdmp(1:2, iflx, imba)
      enddo
   enddo

   ! reset flux accumulators
   mbaflowhor = 0.0d0
   mbaflowsorsin = 0.0d0
   mbafluxhor = 0.0d0
   mbafluxsorsin = 0.0d0
   flxdmp = 0.0


   end subroutine mba_update

   subroutine mba_final(time)
   use m_fm_wq_processes
   use m_partitioninfo
   use m_flowexternalforcings, only: numsrc, srcname

   implicit none

   double precision, intent(in) :: time !< time     for waq in seconds

   integer :: isys, imba, jmba, j
   logical :: writebalance

   itimembaend = int(time)

   writebalance = .true.
   if ( jampi.eq.1 ) then
      if (my_rank.ne.0) writebalance = .false.
   endif

   if (writebalance) then
!      call mba_write_his_time_step(lunmbatothis, itimembastarttot, nosys, notot, nomba, nombabnd, nflux, totfluxsys, &
!                                   mbavolumebegintot, mbavolumeend, mbaflowhortot, &
!                                   mbamassbegintot, mbamassend, mbafluxhortot, &
!                                   flxdmptot, stochi, nfluxsys, fluxsys)
      
      write(lunmbabal,1000)
      call mba_write_bal_time_step(lunmbabal, timembastarttot, timembaend, nosys, notot, nomba, nombabnd, nflux, totfluxsys, &
                                   mbaname, syname_sub, mbalnused, numsrc, srcname, mbasorsinout, &
                                   mbaarea, mbavolumebegintot, mbavolumeend, mbaflowhortot,mbaflowsorsintot,  &
                                   mbamassbegintot, mbamassend, mbafluxhortot, mbafluxsorsintot, &
                                   flxdmptot, stochi, fluxname, nfluxsys, fluxsys)
   endif

   1000 format (///'============================================================='&
                  /'Mass balances for whole calculation period'                   &
                  /'=============================================================')

   end subroutine mba_final

   subroutine mba_sum(nmba, ntot, mbavolume, mbamass)
   
   use m_fm_wq_processes
   use m_partitioninfo
   use m_flowgeom
   use m_flow
   use m_transport
   
   implicit none

   integer :: nmba, ntot
   double precision :: mbavolume(nmba)      ! volumes
   double precision :: mbamass(ntot, nmba)  ! masses

   integer :: k, kk, kb, kt, isys, iconst, iwqbot, imba

   mbavolume = 0.0d0
   mbamass = 0.0d0

   do kk = 1,ndxi
      if ( jampi.eq.1 ) then
!        do not include ghost cells
         if ( idomain(kk).ne.my_rank ) cycle
      end if
      imba = mbadef(kk)
      call getkbotktop(kk,kb,kt)
      do k = kb,kt
         mbavolume(imba) = mbavolume(imba) + vol1(k)
         do isys=1,nosys
            iconst = isys2const(isys)
            mbamass(isys,imba) = mbamass(isys,imba) + constituents(iconst,k)*vol1(k)
         end do
      end do
      do isys=nosys+1,notot
         iwqbot = isys2wqbot(isys)
         mbamass(isys,imba) = mbamass(isys,imba) + wqbot(iwqbot,kk)*ba(kk)
      end do
   end do 
   end subroutine mba_sum

   subroutine mba_sum_area(nmba, mbaba)
   
   use m_fm_wq_processes
   use m_partitioninfo
   use m_flowgeom
   
   implicit none

   integer :: nmba
   double precision :: mbaba(nmba)      ! areas

   integer :: kk, imba

   mbaba = 0.0d0

   do kk = 1,ndxi
      if ( jampi.eq.1 ) then
!        do not include ghost cells
         if ( idomain(kk).ne.my_rank ) cycle
      end if
      imba = mbadef(kk)
      mbaba(imba) = mbaba(imba) + ba(kk)
   end do 
   end subroutine mba_sum_area
   
   subroutine mba_write_his_header(lunhis)
   
   use m_alloc
   use m_fm_wq_processes
   use unstruc_model, only: md_ident
   use unstruc_files

   implicit none
   
   integer :: lunhis, isys, imba, i, j, jflux

   character(20), allocatable :: balterms(:)
   character(20), allocatable :: mbanamehis(:)
   character(40)              :: moname(4)
   
   integer :: nobalterms, ifluxsys

!  write mbahis to a his file (for now
   moname=' '
   moname(1)='Mass balance information'
   moname(2)=trim(md_ident(1:40))
   nobalterms = (4 + 2 * (nombabnd) + nosys * 2 * (2 + nombabnd) + (notot-nosys) * 2 * 2 + 2 * sum(nfluxsys))
   call realloc(balterms, nobalterms, keepexisting=.false., fill =' ')
   balterms(1) = 'Begin water'
   balterms(2) = 'End   water'
   balterms(3) = 'Water in  storage'
   balterms(4) = 'Water out storage'
   j = 4
   do i = 1, nombabnd
      j = j + 1
      balterms(j) = 'Water in '//mbaname(i)
      j = j + 1
      balterms(j) = 'Water out '//mbaname(i)
   enddo
   ifluxsys = 0
   do isys = 1, notot
      j = j + 1
      balterms(j) = trim(syname_sub(isys))//' begin mass'
      j = j + 1
      balterms(j) = trim(syname_sub(isys))//' end   mass'
      j = j + 1
      balterms(j) = trim(syname_sub(isys))//' in  storage'
      j = j + 1
      balterms(j) = trim(syname_sub(isys))//' out storage'
      if (isys.le.nosys) then
         do i = 1, nombabnd
            j = j + 1
            balterms(j) = trim(syname_sub(isys))//' in  '//trim(mbaname(i))
            j = j + 1
            balterms(j) = trim(syname_sub(isys))//' out '//trim(mbaname(i))
         enddo
      endif
      if (nfluxsys(isys).gt.0) then
         do i = ifluxsys + 1, ifluxsys + nfluxsys(isys)
            jflux = fluxsys(i)
            j = j + 1
            balterms(j) = trim(syname_sub(isys))//' in  '//fluxname(jflux)
            j = j + 1
            balterms(j) = trim(syname_sub(isys))//' out '//fluxname(jflux)
         enddo
         ifluxsys = ifluxsys + nfluxsys(isys)
      endif
   end do
   call realloc(mbanamehis, nomba + 1, keepexisting=.false., fill=' ')
   do i = 1, nomba
      mbanamehis(i) = trim(mbaname(i)(1:20))
   end do
   mbanamehis(nomba+1) = 'Total mass balance'

   write(lunhis) moname
   write(lunhis) nobalterms
   write(lunhis) nomba + 1
   write(lunhis) balterms
   write(lunhis) (i, mbanamehis(i), i = 1, nomba+1)
   
   end subroutine mba_write_his_header

   subroutine mba_write_his_time_step(lunmbahis, itime, nosys, notot, nomba, nombabnd, nflux, totfluxsys, &
                                      mbavolumebegin, mbavolumeend, mbaflowhor, &
                                      mbamassbegin, mbamassend, mbafluxhor, &
                                      flxdmp, stochi, nfluxsys, fluxsys)
   
   use precision
   
   implicit none
   
   integer                     :: lunmbahis                 ! logical unit
   
   integer                     :: itime                     ! start time of balance period
   integer                     :: notot                     ! Number of systems
   integer                     :: nosys                     ! Number of active systems
   integer                     :: nomba                     ! Number of balance areas
   integer                     :: nombabnd                  ! Number of balance areas and boundaries
   integer                     :: nflux                     ! number of fluxes
   integer                     :: totfluxsys                ! total number of fluxes for all sustances

   double precision            :: mbavolumebegin(nomba)     ! begin volume in mass balance area
   double precision            :: mbavolumeend(nomba)       ! end volume in mass balance area
   double precision            :: mbaflowhor(2,nombabnd,nombabnd) ! periodical flows between balance areas and between boundaries and balance areas

   double precision            :: mbamassbegin(notot,nomba) ! begin volume in mass balance area
   double precision            :: mbamassend(notot,nomba)   ! end volume in mass balance area
   double precision            :: mbafluxhor(2,nosys,nombabnd,nombabnd) ! periodical fluxes between balance areas and between boundaries and balance areas

   double precision            :: flxdmp(2,nflux, nomba)
   real                        :: stochi(notot,nflux)
      
   integer                     :: nfluxsys(notot)
   integer                     :: fluxsys(totfluxsys)

   double precision            :: summbavolumebegin
   double precision            :: summbavolumeend
   double precision            :: summbamassbegin
   double precision            :: summbamassend
   double precision            :: flux(2)
   integer :: imba, jmba, isys, iflux, jflux, ifluxsys
   real :: zero = 0.0

   write (lunmbahis) itime

   do imba = 1, nomba
      write (lunmbahis) real(mbavolumebegin(imba),kind=sp)
      write (lunmbahis) real(mbavolumeend(imba),kind=sp)
      if (mbavolumebegin(imba).gt.mbavolumeend(imba)) then
         write (lunmbahis) real(mbavolumebegin(imba)-mbavolumeend(imba),kind=sp), zero
      else
         write (lunmbahis) zero, real(mbavolumeend(imba)-mbavolumebegin(imba),kind=sp)
      endif
      do jmba = 1, nombabnd
         write (lunmbahis) real(mbaflowhor(1:2, imba, jmba),kind=sp)
      end do
      ifluxsys = 0
      do isys = 1, notot
         write (lunmbahis) real(mbamassbegin(isys, imba),kind=sp)
         write (lunmbahis) real(mbamassend(isys, imba),kind=sp)
         if (mbamassbegin(isys, imba).gt.mbamassend(isys, imba)) then
            write (lunmbahis) real(mbamassbegin(isys, imba) - mbamassend(isys, imba),kind=sp), zero
         else
            write (lunmbahis) zero, real(mbamassend(isys, imba) - mbamassbegin(isys, imba),kind=sp)
         endif
         if (isys.le.nosys) then
            do jmba = 1, nombabnd
               write (lunmbahis) real(mbafluxhor(1:2, isys, imba, jmba),kind=sp)
            end do
         endif
         if (nfluxsys(isys).gt.0) then
            do iflux = ifluxsys + 1, ifluxsys + nfluxsys(isys)
               jflux = fluxsys(iflux)
               if(stochi(isys,jflux).ge.0.0) then
                  flux(1) =  dble(stochi(isys,jflux)) * flxdmp(1,jflux, imba)
                  flux(2) =  dble(stochi(isys,jflux)) * flxdmp(2,jflux, imba)
               else
                  flux(1) =  -dble(stochi(isys,jflux)) * flxdmp(2,jflux, imba)
                  flux(2) =  -dble(stochi(isys,jflux)) * flxdmp(1,jflux, imba)
               endif
               write (lunmbahis) real(flux(1:2),kind=sp)
            enddo
            ifluxsys = ifluxsys + nfluxsys(isys)
         endif
      end do
   end do

   summbavolumebegin = sum(mbavolumebegin)
   summbavolumeend = sum(mbavolumeend)
   write (lunmbahis) real(summbavolumebegin,kind=sp)
   write (lunmbahis) real(summbavolumeend,kind=sp)
   if (summbavolumebegin.gt.summbavolumeend) then
      write (lunmbahis) real(summbavolumebegin-summbavolumeend,kind=sp), zero
   else
      write (lunmbahis) zero, real(summbavolumeend-summbavolumebegin,kind=sp)
   endif
   do jmba = 1, nombabnd
      write (lunmbahis) real(sum(mbaflowhor(1, :, jmba))), real(sum(mbaflowhor(2, :, jmba)),kind=sp)
   end do
   ifluxsys = 0
   do isys = 1, notot
      summbamassbegin = sum(mbamassbegin(isys, :))
      summbamassend = sum(mbamassend(isys, :))
      write (lunmbahis) real(summbamassbegin,kind=sp)
      write (lunmbahis) real(summbamassend,kind=sp)
      if (summbamassbegin.gt.summbamassend) then
         write (lunmbahis) real(summbamassbegin - summbamassend,kind=sp), zero
      else
         write (lunmbahis) zero, real(summbamassend - summbamassbegin,kind=sp)
      endif
      if (isys.le.nosys) then
         do jmba = 1, nombabnd
            write (lunmbahis) real(sum(mbafluxhor(1, isys, :, jmba)),kind=sp), &
                              real(sum(mbafluxhor(2, isys, :, jmba)),kind=sp)
         end do
      endif
      if (nfluxsys(isys).gt.0) then
         do iflux = ifluxsys + 1, ifluxsys + nfluxsys(isys)
            jflux = fluxsys(iflux)
            if(stochi(isys,jflux).ge.0.0) then
               flux(1) =  dble(stochi(isys,jflux)) * sum(flxdmp(1,jflux, :))
               flux(2) =  dble(stochi(isys,jflux)) * sum(flxdmp(2,jflux, :))
            else
               flux(1) =  -dble(stochi(isys,jflux)) * sum(flxdmp(2,jflux, :))
               flux(2) =  -dble(stochi(isys,jflux)) * sum(flxdmp(1,jflux, :))
            endif
            write (lunmbahis) real(flux(1:2),kind=sp)
         enddo
         ifluxsys = ifluxsys + nfluxsys(isys)
      endif
   end do
   return
   end subroutine mba_write_his_time_step

   subroutine mba_write_bal_header(lunbal, nosys, notot, nomba, mbaname, syname, nflux, &
                                   totfluxsys, stochi, fluxname, fluxprocname, nfluxsys, fluxsys)
   
   use unstruc_version_module, only: unstruc_version_full, get_unstruc_source

   implicit none
   
   integer                     :: lunbal                    ! logical unit

   integer                     :: nosys                     ! Number of active systems
   integer                     :: notot                     ! Number of systems
   integer                     :: nomba                     ! Number of balance areas

   character(*)                :: mbaname(nomba)            ! balance names
   character(20)               :: syname(notot)             ! sunstance names
   
   integer                     :: nflux                     ! number of fluxes
   integer                     :: totfluxsys                ! total number of fluxes for all sustances

   real                        :: stochi(notot,nflux)
   character(10)               :: fluxname(nflux)
   character(10)               :: fluxprocname(nflux)
      
   integer                     :: nfluxsys(notot)
   integer                     :: fluxsys(totfluxsys)

   character(255)              :: tex
   character(20)               :: rundat
   integer                     :: imba
   integer                     :: isys
   integer                     :: iflux
   integer                     :: jflux
   integer                     :: ifluxsys

   write (lunbal, '("=============================================================")')
   write(lunbal,'(A)') trim(unstruc_version_full)
   call get_unstruc_source(tex)
   write(lunbal,'(A)') 'Source: '//trim(tex)
   call datum(rundat)
   write(lunbal,'(A)') 'File creation date: '//rundat
   write (lunbal, '(/"Balances for all mass balance areas")')
   write (lunbal, '("=============================================================")')

   write (lunbal, '(/"Overview of mass balance areas")')
   write (lunbal, '( "-------------------------------------------------------------")')
   write (lunbal, '( "Number of mass balance areas                    :",I8)') nomba
   do imba = 1, nomba
      write (lunbal, '(I8,2X,A40)') imba, mbaname(imba)
   enddo

   write (lunbal, '(/"Overview of substances")')
   write (lunbal, '( "-------------------------------------------------------------")')
   write (lunbal, '( "Total number of substances                      :",I8)') notot
   write (lunbal, '( "Number of active (transported) substances       :",I8)') nosys
   do isys = 1, nosys
      write (lunbal, '(I8,2X,A20)') isys, syname(isys)
   enddo
   write (lunbal, '(/"Number of inactive (not transported) substances :",I8)') notot - nosys
   do isys = nosys + 1, notot
      write (lunbal, '(I8,2X,A20)') isys, syname(isys)
   enddo

   write (lunbal, '(/"Overview of fluxes")')
   write (lunbal, '( "-------------------------------------------------------------")')
   write (lunbal, '( "total number of substances fluxes               :",I8)') totfluxsys
   write (lunbal, '(/"Substance      Process        Flux        Stochiometry factor")')
   write (lunbal, '( "-------------------------------------------------------------")')
   ifluxsys = 0
   do isys = 1, notot
      if (nfluxsys(isys).gt.0) then
         do iflux = ifluxsys + 1, ifluxsys + nfluxsys(isys)
            jflux = fluxsys(iflux)
            write (lunbal, '(A10,5X,A10,5X,A10,ES20.6)') syname(isys), fluxprocname(jflux), fluxname(jflux), stochi(isys,jflux)
         enddo
         ifluxsys = ifluxsys + nfluxsys(isys)
      endif
   enddo

   return
   end subroutine mba_write_bal_header
   
   subroutine mba_write_bal_time_step(lunbal, timestart, timeend, nosys, notot, nomba, nombabnd, nflux, totfluxsys, &
                                      mbaname, syname, mbalnused, numsrc, srcname, mbasorsinout, &
                                      mbaarea, mbavolumebegin, mbavolumeend, mbaflowhor, mbaflowsorsin, &
                                      mbamassbegin, mbamassend, mbafluxhor, mbafluxsorsin, &
                                      flxdmp, stochi, fluxname, nfluxsys, fluxsys)
   implicit none
   
   integer                     :: lunbal                    ! logical unit
   
   double precision            :: timestart                 ! start time of balance period
   double precision            :: timeend                   ! end time of balance period
   integer                     :: notot                     ! Number of systems
   integer                     :: nosys                     ! Number of active systems
   integer                     :: nomba                     ! Number of balance areas
   integer                     :: nombabnd                  ! Number of balance areas and boundaries
   integer                     :: nflux                     ! number of fluxes
   integer                     :: totfluxsys                ! total number of fluxes for all sustances

   character(*)                :: mbaname(nombabnd)         ! balance names
   character(20)               :: syname(notot)             ! sunstance names
   
   integer                     :: mbalnused(nomba,nombabnd) ! number of links between mda and mbabnd that are actually active

   integer                     :: numsrc                    ! nr of point sources/sinks
   character(len=255)          :: srcname(numsrc)           ! sources/sinks name (numsrc)
   integer                     :: mbasorsinout(2,numsrc)    ! (reduced) mba for each side of a source sink
   
   double precision            :: mbaarea(nomba)            ! surface area of mass balance area

   double precision            :: mbavolumebegin(nomba)     ! begin volume in mass balance area
   double precision            :: mbavolumeend(nomba)       ! end volume in mass balance area
   double precision            :: mbaflowhor(2,nombabnd,nombabnd) ! periodical flows between balance areas and between boundaries and balance areas
   double precision            :: mbaflowsorsin(2,numsrc)   ! periodical flow from source sinks

   double precision            :: mbamassbegin(notot,nomba) ! begin volume in mass balance area
   double precision            :: mbamassend(notot,nomba)   ! end volume in mass balance area
   double precision            :: mbafluxhor(2,nosys,nombabnd,nombabnd) ! periodical fluxes between balance areas and between boundaries and balance areas
   double precision            :: mbafluxsorsin(2,2,notot,numsrc) ! periodical fluxes from source sinks

   double precision            :: flxdmp(2,nflux, nomba)
   real                        :: stochi(notot,nflux)
   character(10)               :: fluxname(nflux)
      
   integer                     :: nfluxsys(notot)
   integer                     :: fluxsys(totfluxsys)

   integer, parameter :: long = SELECTED_INT_KIND(16)
   character(len=20), external :: seconds_to_dhms
   integer :: imba, jmba, isrc, isys, iflux, jflux, ifluxsys
   double precision            :: totals(2)                 ! totals for both columns
   double precision            :: concbegin
   double precision            :: concend
   double precision            :: summbaarea
   double precision            :: summbavolumebegin
   double precision            :: summbavolumeend
   double precision            :: summbamassbegin
   double precision            :: summbamassend
   double precision            :: flux(2)
   double precision            :: reference                 ! reference for relative error
   double precision            :: relative_error            ! relative error
   double precision, parameter :: zero = 0.0
   double precision, parameter :: tiny = 1.0d-10

    do imba = 1, nomba
      totals = zero
      ifluxsys = 0
      write (lunbal, 1000) mbaname(imba)
      write (lunbal, 1001) seconds_to_dhms(nint(timestart, long)), seconds_to_dhms(nint(timeend, long)), mbaarea(imba)
      write (lunbal, 2000) mbavolumebegin(imba), mbavolumeend(imba)
      write (lunbal, 1002)
      if (mbaarea(imba).gt.0.0) then
         write (lunbal, 2000) mbavolumebegin(imba)/mbaarea(imba), mbavolumeend(imba)/mbaarea(imba)
      else
         write (lunbal, 2005)
      endif
      write (lunbal, 1003)
      if (mbavolumebegin(imba).gt.mbavolumeend(imba)) then
         totals(1) = mbavolumebegin(imba) - mbavolumeend(imba)
      else
         totals(2) = mbavolumeend(imba) - mbavolumebegin(imba)
      endif
      write (lunbal, 2002) totals
      do jmba = 1, nombabnd
         if (mbalnused(imba,jmba).gt.0) then
            totals = totals + mbaflowhor(1:2, imba, jmba)
            write (lunbal, 2001) mbaname(jmba), mbaflowhor(1:2, imba, jmba)
         endif
      end do
      do isrc = 1, numsrc
         if (mbasorsinout(1,isrc).eq.imba) then
            totals = totals + mbaflowsorsin(1:2, isrc)
            write (lunbal, 2001) 'src_'//srcname(isrc), mbaflowsorsin(1:2, isrc)
         endif
         if (mbasorsinout(2,isrc).eq.imba) then
            totals = totals + mbaflowsorsin(2:1:-1, isrc)
            write (lunbal, 2001) 'src_'//srcname(isrc), mbaflowsorsin(2:1:-1, isrc)
         endif
      end do
      write (lunbal, 1004)
      write (lunbal, 2003) totals
      write (lunbal, 2010) totals(2)-totals(1)
      reference = max(abs(mbavolumebegin(imba)),abs(mbavolumeend(imba)),totals(1),totals(2))
      if (reference .gt. tiny) then
         relative_error = 1.0d2*abs(totals(2)-totals(1))/reference
         write (lunbal, 2011) relative_error
      else
         write (lunbal, 2012)
      endif
      do isys = 1, notot
         totals = zero
         write (lunbal, 1010) seconds_to_dhms(nint(timestart, long)), seconds_to_dhms(nint(timeend, long)), mbaname(imba), syname(isys)
         write (lunbal, 2000) mbamassbegin(isys, imba), mbamassend(isys, imba)
         if (isys.le.nosys) then
            write (lunbal, 1011)
            if (mbavolumebegin(imba).gt.0.0) then
               concbegin = mbamassbegin(isys, imba) / mbavolumebegin(imba)
            else
               concbegin = 0.0
            endif
            if (mbavolumeend(imba).gt.0.0) then
               concend = mbamassend(isys, imba) / mbavolumeend(imba)
            else
               concend = 0.0
            endif
         else
            write (lunbal, 1012)
            if (mbaarea(imba).gt.0.0) then
               concbegin = mbamassbegin(isys, imba) / mbaarea(imba)
               concend = mbamassend(isys, imba) / mbaarea(imba)
            else
               concbegin = 0.0
               concend = 0.0
            endif
         endif
         write (lunbal, 2000) concbegin, concend
         write (lunbal, 1013) syname(isys)
         if (mbamassbegin(isys, imba).gt.mbamassend(isys, imba)) then
            totals(1) = mbamassbegin(isys, imba) - mbamassend(isys, imba)
         else
            totals(2) = mbamassend(isys, imba) - mbamassbegin(isys, imba)
         endif
         write (lunbal, 2002) totals
         if (isys.le.nosys) then
            do jmba = 1, nombabnd
               if (mbalnused(imba,jmba).gt.0) then
                  totals = totals + mbafluxhor(1:2, isys, imba, jmba)
                  write (lunbal, 2001) mbaname(jmba), mbafluxhor(1:2, isys, imba, jmba)
               endif
            end do
            do isrc = 1, numsrc
               if (mbasorsinout(1,isrc).eq.imba) then
                  totals = totals + mbafluxsorsin(1:2, 1, isys, isrc)
                  write (lunbal, 2001) 'src_'//srcname(isrc), mbafluxsorsin(1:2, 1, isys, isrc)
               endif
               if (mbasorsinout(2,isrc).eq.imba) then
                  totals = totals + mbafluxsorsin(2:1:-1, 2, isys, isrc)
                  write (lunbal, 2001) 'src_'//srcname(isrc), mbafluxsorsin(2:1:-1, 2, isys, isrc)
               endif
            end do
         endif
         if (nfluxsys(isys).gt.0) then
            do iflux = ifluxsys + 1, ifluxsys + nfluxsys(isys)
               jflux = fluxsys(iflux)
               if(stochi(isys,jflux).ge.0.0) then
                  flux(1) =  dble(stochi(isys,jflux)) * flxdmp(1,jflux, imba)
                  flux(2) =  dble(stochi(isys,jflux)) * flxdmp(2,jflux, imba)
               else
                  flux(1) =  -dble(stochi(isys,jflux)) * flxdmp(2,jflux, imba)
                  flux(2) =  -dble(stochi(isys,jflux)) * flxdmp(1,jflux, imba)
               endif
               write (lunbal, 2004) fluxname(jflux), flux(1:2)
               totals(1:2) = totals(1:2) + flux(1:2)
            enddo
            ifluxsys = ifluxsys + nfluxsys(isys)
         endif
         write (lunbal, 1004)
         write (lunbal, 2003) totals
         write (lunbal, 2020) syname(isys), totals(2)-totals(1)
         reference = max(abs(mbamassbegin(isys,imba)),abs(mbamassend(isys,imba)),totals(1),totals(2))
         if (reference .gt. tiny) then
            relative_error = 1.0d2*abs(totals(2)-totals(1))/reference
            write (lunbal, 2021) syname(isys), relative_error
         else
            write (lunbal, 2022) syname(isys)
         end if
      end do
   end do
   totals = zero
   ifluxsys = 0
   summbaarea = sum(mbaarea)
   summbavolumebegin = sum(mbavolumebegin)
   summbavolumeend = sum(mbavolumeend)
   write (lunbal, 1000) 'Whole model'
   write (lunbal, 1001) seconds_to_dhms(nint(timestart, long)), seconds_to_dhms(nint(timeend, long)), summbaarea
   write (lunbal, 2000) summbavolumebegin, summbavolumeend
   write (lunbal, 1002) 
   if (summbaarea.gt.0.0) then
      write (lunbal, 2000) summbavolumebegin/summbaarea, summbavolumeend/summbaarea
   else
      write (lunbal, 2005)
   endif
   write (lunbal, 1003) 
   if (summbavolumebegin.gt.summbavolumeend) then
      totals(1) = summbavolumebegin - summbavolumeend
   else
      totals(2) = summbavolumeend - summbavolumebegin
   endif
   write (lunbal, 2002) totals
   do jmba = nomba + 1, nombabnd
      totals(1) = totals(1) + sum(mbaflowhor(1, :, jmba))
      totals(2) = totals(2) + sum(mbaflowhor(2, :, jmba))
      write (lunbal, 2001) mbaname(jmba), sum(mbaflowhor(1, :, jmba)), sum(mbaflowhor(2, :, jmba))
   end do
   do isrc = 1, numsrc
      if (mbasorsinout(1,isrc).gt.0) then
         totals = totals + mbaflowsorsin(1:2, isrc)
         write (lunbal, 2001) 'src_'//srcname(isrc), mbaflowsorsin(1:2, isrc)
      endif
      if (mbasorsinout(2,isrc).gt.0) then
         totals = totals + mbaflowsorsin(2:1:-1, isrc)
         write (lunbal, 2001) 'src_'//srcname(isrc), mbaflowsorsin(2:1:-1, isrc)
      endif
   end do
   write (lunbal, 1004)
   write (lunbal, 2003) totals
   write (lunbal, 2010) totals(2)-totals(1)
   reference = max(abs(summbavolumebegin),abs(summbavolumeend),totals(1),totals(2))
   if (reference .gt. tiny) then
      relative_error = 1.0d2*abs(totals(2)-totals(1))/reference
      write (lunbal, 2011) relative_error
   else
      write (lunbal, 2012)
   endif
   do isys = 1, notot
      totals = zero
      write (lunbal, 1010) seconds_to_dhms(nint(timestart, long)), seconds_to_dhms(nint(timeend, long)), 'Whole model', syname(isys)
      summbamassbegin = sum(mbamassbegin(isys, :))
      summbamassend = sum(mbamassend(isys, :))
      write (lunbal, 2000) summbamassbegin, summbamassend
      if(isys.le.nosys) then
         write (lunbal, 1011)
         if (summbavolumebegin.gt.0.0) then
            concbegin = summbamassbegin / summbavolumebegin
         else
            concbegin = 0.0
         endif
         if (summbavolumeend.gt.0.0) then
            concend = summbamassend / summbavolumeend
         else
            concend = 0.0
         endif
      else
         write (lunbal, 1012)
         if (summbaarea.gt.0.0) then
            concbegin = summbamassbegin / summbaarea
            concend = summbamassend / summbaarea
         else
            concbegin = 0.0
            concend = 0.0
         endif
      endif
      write (lunbal, 2000) concbegin, concend
      write (lunbal, 1013) syname(isys)
      if (summbamassbegin.gt.summbamassend) then
         totals(1) = summbamassbegin - summbamassend
      else
         totals(2) = summbamassend - summbamassbegin
      endif
      write (lunbal, 2002) totals
      if (isys.le.nosys) then
         do jmba = nomba + 1, nombabnd
            totals(1) = totals(1) + sum(mbafluxhor(1, isys, :, jmba))
            totals(2) = totals(2) + sum(mbafluxhor(2, isys, :, jmba))
            write (lunbal, 2001) mbaname(jmba), sum(mbafluxhor(1, isys, :, jmba)), sum(mbafluxhor(2, isys, :, jmba))
         end do
         do isrc = 1, numsrc
            if (mbasorsinout(1,isrc).gt.0) then
               totals = totals + mbafluxsorsin(1:2, 1, isys, isrc)
               write (lunbal, 2001) 'src_'//srcname(isrc), mbafluxsorsin(1:2, 1, isys, isrc)
            endif
            if (mbasorsinout(2,isrc).gt.0) then
               totals = totals + mbafluxsorsin(2:1:-1, 2, isys, isrc)
               write (lunbal, 2001) 'src_'//srcname(isrc), mbafluxsorsin(2:1:-1, 2, isys, isrc)
            endif
         end do
      endif
      if (nfluxsys(isys).gt.0) then
         do iflux = ifluxsys + 1, ifluxsys + nfluxsys(isys)
            jflux = fluxsys(iflux)
            if(stochi(isys,jflux).ge.0.0) then
               flux(1) =  dble(stochi(isys,jflux)) * sum(flxdmp(1,jflux, :))
               flux(2) =  dble(stochi(isys,jflux)) * sum(flxdmp(2,jflux, :))
            else
               flux(1) =  -dble(stochi(isys,jflux)) * sum(flxdmp(2,jflux, :))
               flux(2) =  -dble(stochi(isys,jflux)) * sum(flxdmp(1,jflux, :))
            endif
            write (lunbal, 2004) fluxname(jflux), flux(1:2)
            totals(1:2) = totals(1:2) + flux(1:2)
         enddo
         ifluxsys = ifluxsys + nfluxsys(isys)
      endif
      write (lunbal, 1004)
      write (lunbal, 2003) totals
      write (lunbal, 2020) syname(isys), totals(2)-totals(1)
      reference = max(abs(summbamassbegin),abs(summbamassend),totals(1),totals(2))
      if (reference .gt. tiny) then
         relative_error = 1.0d2*abs(totals(2)-totals(1))/reference
         write (lunbal, 2021) syname(isys), relative_error
      else
         write (lunbal, 2022) syname(isys)
      end if
   end do

   return
   
   1000 format (///'============================================================='&
                  /'Mass balances for ',a                                         &
                  /'=============================================================')
   1001 format (  /'Mass balance period start time: ',a                           &
                  /'Mass balance period end time  : ',a                           &
                 //'Surface area (m2)             : ',ES15.6E3                    &
                 //'Water (m3)                              Begin            End '&
                  /'-------------------------------------------------------------')
   1002 format (  /'Average depth (m)                       Begin            End '&
                  /'-------------------------------------------------------------')
   1003 format (  /'Water (m3)                    Sources/Inflows Sinks/Outflows '&
                  /'-------------------------------------------------------------')

   1004 format (   '-------------------------------------------------------------')

   1010 format ( //'-------------------------------------------------------------'&
                  /'Mass balance period start time: ',a                           &
                  /'Mass balance period end time  : ',a                           &
                 //'Mass balance area             : ',a                           &
                 //'Mass substance ',A20,'     Begin            End '             &
                  /'-------------------------------------------------------------')
   1011 format (  /'Average concentration (mass/m3)         Begin            End '&
                  /'-------------------------------------------------------------')
   1012 format (  /'Average concentration (mass/m2)         Begin            End '&
                  /'-------------------------------------------------------------')
   1013 format (  /'Substance ',A20,'Sources/Inflows Sinks/Outflows '             &
                  /'-------------------------------------------------------------')

   2000 format (30X,2ES15.6E3)
   2001 format (A30,2ES15.6E3)
   2002 format (   'Taken from/added to storage   ',2ES15.6E3)
   2003 format (   'Sum of all terms              ',2ES15.6E3)
   2004 format (   'Process flux ',A10,7X,2ES15.6E3)
   2005 format (45X,'no surface area')

   2010 format (  /'Water balance error (m3)                     ',ES15.6E3)
   2011 format (   'Water balance error                          ',F15.6,'%')
   2012 format (   'Water balance error                                       - %')
 
   2020 format (  /A20,' Mass balance error      ',ES15.6E3)
   2021 format (   A20,' Mass balance error      ',F15.6,'%')
   2022 format (   A20,' Mass balance error                   - %')

   end subroutine mba_write_bal_time_step
