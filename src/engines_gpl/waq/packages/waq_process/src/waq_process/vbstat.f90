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

      subroutine VBSTAT     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint( 14) ! I  Array of pointers in pmsa to get and store the data
      integer increm( 14) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 14)   !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop
      real(4) DELT        ! I  timestep for processes                             (d)
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      real(4) SwEmersion  ! I  switch indicating submersion(0) or emersion(1)     (-)
      integer VBType      ! I  code of vegetation type for error and warnings     (-)
      real(4) nsfVB     ! I  nr successive emersion(flood) VB01                 (d)
      real(4) CrnsfVB01   ! I  critical number successive flood days VB01         (d)
      real(4) SwNutVB01   ! I  switch indicating nutrient limitation (0=no,1=yes) (-)
      real(4) Initnsf     ! I  initial nr of flood days at start of simulation    (d)
      real(4) nsnlVB01    ! I  number of successive days nutrient lim. VB01       (d)
      real(4) CrnsnlVB01  ! I  critical number of successive nut. lim VB01        (d)
      real(4) SwVB01Gro   ! O  vegetation biomass growth allowed (0=no,1=yes)     (-)
      real(4) SwVB01Mrt   ! O  vegetation biomass dead (0=no,1=yes)               (-)
      integer, save       :: ifirst(1:18) = 0     !    for initialisation
      integer             :: ikmrk2         ! second feature
      integer             :: ikmrk3         ! third feature
      integer             :: ip             ! base output location for bottom segement pointer output
      integer             :: inc            ! increment in output location for bottom segement pointer output
      integer             :: iq             ! counter for pointer loop
      integer             :: ifrom          ! from location
      integer             :: ito            ! to location
      integer             :: ibotseg        ! bottom segement for current segement
      integer ilumon
!
!*******************************************************************************
!
      ipnt        = ipoint
!
      CALL GETMLU(ILUMON)
      VBType     = NINT(pmsa( ipnt(  2) ))

! initialise bottom segment pointer

      if (ifirst(VBType).eq.0) then
         ip = ipoint( 14)
         inc = increm( 14)
         
         do iseg = 1,noseg
            pmsa(ip + inc * (iseg - 1)) = real(-1,4)
         end do

         ! set botseg equal to iseg for the segments which have a bottom

         do iseg = 1,noseg
            call dhkmrk(3,iknmrk(iseg),ikmrk3)
            if (ikmrk3.eq.1) then
               call dhkmrk(2,iknmrk(iseg),ikmrk2)
               if ((ikmrk2.eq.0).or.(ikmrk2.eq.3)) then
                  pmsa(ip + inc * (iseg - 1)) = real(iseg,4)
               endif
            endif
         enddo

         ! loop to find bottom segment in water columns

         do iq = noq1+noq2+noq3, noq1 + noq2 +1, -1
            ifrom   = iexpnt(1,iq)
            ito     = iexpnt(2,iq)
            if ( ifrom .gt. 0 .and. ito .gt. 0 ) then
               ibotseg = pmsa(ip + inc * (ito - 1))
               if ( ibotseg .gt. 0 ) then
                  pmsa(ip + inc * (ifrom - 1)) = real(ibotseg,4)
               endif
            endif
         enddo

         ! do the same for the delwaq-g bottom

         do iq = noq1+noq2+noq3+1, noq1+noq2+noq3+noq4
            ifrom   = iexpnt(1,iq)
            ito     = iexpnt(2,iq)
            if ( ifrom .gt. 0 .and. ito .gt. 0 ) then
               ibotseg = pmsa(ip + inc * (ifrom - 1))
               if ( ibotseg .gt. 0 ) then
                  pmsa(ip + inc * (ito - 1)) = real(ibotseg,4)
               endif
            endif
         enddo
      endif

      do 9000 iseg = 1 , noseg
!
!        lowest water and 2d segments only
         call dhkmrk(2,iknmrk(iseg),ikmrk2)
         call dhkmrk(3,iknmrk(iseg),ikmrk3)
         if (ikmrk3.eq.1 .and. (ikmrk2.eq.0).or.(ikmrk2.eq.3)) then

            SwEmersion = pmsa( ipnt(  1) )
            VBType     = NINT(pmsa( ipnt(  2) ))
            nsfVB      = pmsa( ipnt(  3) )
            CrnsfVB01  = pmsa( ipnt(  4) )
            Initnsf    = pmsa( ipnt(  5) )
            SwNutVB01  = pmsa( ipnt(  6) )
            nsnlVB01   = pmsa( ipnt(  7) )
            CrnsnlVB01 = pmsa( ipnt(  8) )
            DELT       = pmsa( ipnt(  9) )

!           initialise growth
            SWVB01Gro = 1.0
            SwVB01Mrt = 0.0

            if (ifirst(VBType) .eq. 0) then
               nsfVB = Initnsf
!              WRITE (ILUMON, *) 'ifirst, iseg, nsf', ifirst, iseg, nsfvb
            endif

            if ( NINT(SwEmersion) .eq. 0 ) then
               nsfVB = nsfVB + DELT
               SWVB01Gro = 0.0
            else
               nsfVB = 0
            endif

            if ( NINT(SWNutVB01) .eq. 1 ) then
               nsnlVB01 = nsnlVB01 + DELT
               SWVB01Gro = 0.0
            else
               nsnlVB01 = 0
            endif

            if ( (nsfVB .gt. CrnsfVB01) .or. (nsnlVB01 .gt. CrnsnlVB01) ) then
               SwVB01Mrt = 1.0
            endif

            pmsa( ipnt( 10)   ) = SwVB01Gro
            pmsa( ipnt( 11)   ) = SwVB01Mrt
            pmsa( ipnt( 12)   ) = nsfVB
            pmsa( ipnt( 13)   ) = nsnlVB01

         endif
!
         ipnt        = ipnt        + increm
!
 9000 continue
      ifirst(VBType) = 1
!
      return
      end
