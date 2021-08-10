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

      subroutine dlwq41 ( lun    , itime  , itimel , harmat , array  ,
     &                    iharm  , nrharm , nrftot , noseg  , volume ,
     &                    ipoint , luntxt , ftype  , isflag , ivflag ,
     &                    updatv , inwspc , anwspc , inwtyp , iwork  ,
     &                    lstrec , lrewin , vollst , mypart , dlwqd  )

!     Deltares Software Centre

!>/file
!>              Makes values at ITIME for volumes only
!>
!>              Routine is a stripped version of dlwqt0 to read volumes
!>              at the end of the time step only. Remainder of the time
!>              varying info is updated after the time step has finished.
!>              Ratio is that the end-volume of a time step is often needed
!>              to determine drying and flooding and for a number of
!>              numerical schemes.

!     Created             : april- 8-1988 by Leo Postma

!     Logical unitnumbers : LUN(..) -

!     Subroutines called  : DLWQT1, makes one time function

      use timers                     ! WAQ performance timers
      use m_couplib                  ! some first steps on paralellism
      use delwaq2_data

      implicit none

!     Arguments           :

!     Kind        Function         Name                  Description

      integer(4), intent(in   ) :: lun   (*)           !< Array with unit numbers
      integer(4), intent(in   ) :: itime               !< The model timer
      integer(4), intent(in   ) :: itimel              !< The model timer last step
      real   (4), intent(inout) :: harmat(*)           !< Matrices harmonic components
      real   (4), intent(inout) :: array (*)           !< Set of double file buffers
      integer(4), intent(in   ) :: iharm (*)           !< Harmonic time space
      integer(4), intent(in   ) :: nrharm(*)           !< Set of nrs of harmonic records
      integer(4), intent(in   ) :: nrftot(*)           !< Set of record lengthes
      integer(4), intent(in   ) :: noseg               !< Nr of computational volumes
      real   (4), intent(  out) :: volume(noseg)       !< Array of volumes per gridcell
      integer(4), intent(in   ) :: ipoint(*)           !< Set of pointers to destination
      character*(*), intent(in) :: luntxt(*)           !< Text with the unit numbers
      integer(4), intent(in   ) :: ftype (*)           !< Type of file to read
      integer(4), intent(in   ) :: isflag              !< = 1 then 'ddhhmmss' format
      integer(4), intent(in   ) :: ivflag              !< = 1 then computed volumes
      logical   , intent(  out) :: updatv              !< set to T if volume is updated
      integer(4), intent(inout) :: inwspc(*)           !< Integer space new time functions
      real   (4), intent(inout) :: anwspc(*)           !< Real space new time functions
      integer(4), intent(in   ) :: inwtyp(*)           !< Types of items
      integer(4), intent(inout) :: iwork (*)           !< Integer workspace
      logical   , intent(in   ) :: lstrec              !< Switch last record on rewind wanted
      logical   , intent(  out) :: lrewin              !< If T then rewindtook place
      real   (4), intent(  out) :: vollst(noseg)       !< Last volume record before rewind
      integer(4), intent(in   ) :: mypart              !< number of current part/subdomain
      type(delwaq_data), intent(inout) :: dlwqd        !< derived type for persistent storage

!     Common  /  syst   /   system time function flags

      include 'syst.inc'

!     Local

      integer(4)  iph, ipf, ipa, ipi      ! pointers in the arrays
      integer(4)  ifflag                  ! if 1, then it was the first invoke
      logical     update, ldum(2)         ! logicals on rewind
      integer(4)  ierr                    ! error indicator

      integer(4)  ithandl /0/
      if ( timon ) call timstrt ( "dlwq41", ithandl )

!         initialisation

      iph    = 1
      ipf    = 1
      ipa    = 1
      ipi    = 1
      ifflag = 0
      updatv = .false.
      lrewin = .false.

!         integration step size IDT

      if ( nrftot( 1) .gt. 0 ) then
         ipa = ipa + 2
         ipi = ipi + 4
      endif

!         volumes

      if ( nrharm( 2) .ge. 0 ) then
         if (mypart.eq.1) then
            call dlwqt1 ( lun       , itime      , itimel, iharm(ipf), harmat(iph),
     &                    array(ipa), ipoint(ipi), volume, 1         , nrharm( 2) ,
     &                    noseg     , nrftot( 2) , ipa   , iph       , ipf        ,
     &                    ipi       , luntxt     , 7     , isflag    , ifflag     ,
     &                    update    , .false.    , 0     , iwork     , lstrec     ,
     &                    lrewin    , vollst     , ftype , dlwqd     )
!           ldum(1) = update
!           ldum(2) = lrewin
         endif

!        call distribute_data(mypart, ldum, 2, ierr)
!        update = ldum(1)
!        lrewin = ldum(2)
         if ( update ) then
            updatv = .true.
!           call distribute_data( mypart, volume, 'noseg', 'distrib_itf', ierr )
         endif
!        if ( lrewin ) call distribute_data( mypart, vollst, 'noseg', 'distrib_itf', ierr )
      endif

      if ( timon ) call timstop ( ithandl )
      return
      end
