!!  Copyright (C)  Stichting Deltares, 2012-2015.
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

      subroutine veg3du     ( pmsa   , fl     , ipoint , increm, noseg ,
     +                        noflux , iexpnt , iknmrk , noq1  , noq2  ,
     +                        noq3   , noq4   )

      ! function : vegetation module uptake of nutrients

      implicit none

      ! arguments          i/o description

      real(4) pmsa(*)     !i/o process manager system array, window of routine to process library
      real(4) fl(*)       ! o  array of fluxes made by this process in mass/volume/time
      integer ipoint(*)   ! i  array of pointers in pmsa to get and store the data
      integer increm(*)   ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! i  number of computational elements in the whole model schematisation
      integer noflux      ! i  number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
      integer noq1        ! i  nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      integer noq2        ! i  nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)

      ! from pmsa array

      real(4) depth       ! i  depth of segment                               (m)
      real(4) totaldepth  ! i  total depth water column                       (m)
      real(4) localdepth  ! i  depth from water surface to bottom of segment  (m)
      real(4) hmax        ! i  maxmimum length roots                          (m)
      real(4) delt        ! i  delt                                           (d)
      real(4) nh4         ! i  nh4                                         (g/m3)
      real(4) aap         ! i  aap                                         (g/m3)
      real(4) so4         ! i  so4                                         (g/m3)
      real(4) no3         ! i  no3                                         (g/m3)
      real(4) po4         ! i  po4                                         (g/m3)
      real(4) sud         ! i  sud                                         (g/m3)
      real(4) inicovvbxx  ! i  percentage coverage                            (%)
      real(4) vbxxnavail  ! i  available nitrogen                          (g/m2)
      real(4) vbxxpavail  ! i  available phosphorus                        (g/m2)
      real(4) vbxxsavail  ! i  available sulfur                            (g/m2)
      real(4) fnvbxxup    ! i  2d uptake flux                            (g/m2/d)
      real(4) fpvbxxup    ! i  2d uptake flux                            (g/m2/d)
      real(4) fsvbxxup    ! i  2d uptake flux                            (g/m2/d)
      real(4) fn1vbxxupy  ! o  3d uptake flux                            (g/m2/d)
      real(4) fn2vbxxupy  ! o  3d uptake flux                            (g/m2/d)
      real(4) fp1vbxxupy  ! o  3d uptake flux                            (g/m2/d)
      real(4) fp2vbxxupy  ! o  3d uptake flux                            (g/m2/d)
      real(4) fs1vbxxupy  ! o  3d uptake flux                            (g/m2/d)
      real(4) fs2vbxxupy  ! o  3d uptake flux                            (g/m2/d)
      real(4) fl_fn1vbxxupy  ! o  3d uptake flux N pool 1                (g/m3/d)
      real(4) fl_fn2vbxxupy  ! o  3d uptake flux N pool 2                (g/m3/d)
      real(4) fl_fp1vbxxupy  ! o  3d uptake flux P pool 1                (g/m3/d)
      real(4) fl_fp2vbxxupy  ! o  3d uptake flux P pool 2                (g/m3/d)
      real(4) fl_fs1vbxxupy  ! o  3d uptake flux S pool 1                (g/m3/d)
      real(4) fl_fs2vbxxupy  ! o  3d uptake flux S pool 2                (g/m3/d)

      ! local declarations

      integer iseg        !    local loop counter for computational element loop
      real(4) z2          !    height bottom segment from bottom              (m)
      real(4) z1          !    height top segment from bottom                 (m)
      integer ikmrk1
      integer ikmrk2
      real(4) zm          !    watersurface to top macropyte                  (-)
      real(4) frlay       !    fraction witin layer                           (-)
      integer iq          !    loop counter
      integer ifrom       !    from segment
      integer ito         !    from segment
      integer iflux       !    index in the fl array
      real(4) pnvbxxup   !    2d uptake percentage n pool 1 and 2             (-)
      real(4) ppvbxxup   !    2d uptake percentage p pool 1 and 2             (-)
      real(4) psvbxxup   !    2d uptake percentage s pool 1 and 2             (-)

      integer, parameter           :: npnt = 25           ! number of pointers
      integer                      :: ipnt(npnt)          ! local work array for the pointering
      integer                      :: ibotseg             ! bottom segment for macrophyte

      ! accumulate mass in the rooting zone in the pool of the bottom segment

      ipnt  = ipoint(1:npnt)
      iflux = 0
      do iseg = 1 , noseg

         fn1vbxxupy = 0.0
         fn2vbxxupy = 0.0
         fp1vbxxupy = 0.0
         fp2vbxxupy = 0.0
         fs1vbxxupy = 0.0
         fs2vbxxupy = 0.0
         fl_fn1vbxxupy =  0.0
         fl_fn2vbxxupy =  0.0
         fl_fp1vbxxupy =  0.0
         fl_fp2vbxxupy =  0.0
         fl_fs1vbxxupy =  0.0
         fl_fs2vbxxupy =  0.0

         call dhkmrk(1,iknmrk(iseg),ikmrk1)
!         if (ikmrk1.eq.1 .or. ikmrk1 .eq. 3) then !=> NO TESTING ON IKMRK1, PROCESS IS ALLWAYS ON!

            ibotseg     = NINT(pmsa(ipnt(4)))
            inicovvbxx  = pmsa(ipoint( 13)+(ibotseg-1)*increm( 13)) / 100.

            if (inicovvbxx .gt. 0.001) then

               depth       = pmsa(ipnt(1))
               totaldepth  = pmsa(ipnt(2))
               localdepth  = pmsa(ipnt(3))
               hmax        = pmsa(ipnt(5))
               delt        = pmsa(ipnt(6))
               nh4         = pmsa(ipnt(7))
               no3         = pmsa(ipnt(8))
               aap         = pmsa(ipnt(9))
               po4         = pmsa(ipnt(10))
               so4         = pmsa(ipnt(11))
               sud         = pmsa(ipnt(12))

               vbxxnavail  = pmsa(ipoint(14)+(ibotseg-1)*increm(14))
               vbxxpavail  = pmsa(ipoint(15)+(ibotseg-1)*increm(15))
               vbxxsavail  = pmsa(ipoint(16)+(ibotseg-1)*increm(16))
               fnvbxxup    = pmsa(ipoint(17)+(ibotseg-1)*increm(17))
               fpvbxxup    = pmsa(ipoint(18)+(ibotseg-1)*increm(18))
               fsvbxxup    = pmsa(ipoint(19)+(ibotseg-1)*increm(19))


               ! percentage uptake = uptake/available/percentage coverage

               if ( vbxxnavail .gt. 1e-20 ) then
                  pNvbxxup=fNvbxxup*delt/vbxxNavail
!                 pn5vbxxup=fn5vbxxup*delt/vbxxnavail
               else
                  pNvbxxup=0.0
!                 pn5vbxxup=0.0
               endif
               if ( vbxxpavail .gt. 1e-20 ) then
                  pPvbxxup=fPvbxxup*delt/vbxxPavail
!                 pp5vbxxup=fp5vbxxup*delt/vbxxpavail
               else
                  pPvbxxup=0.0
!                 pp5vbxxup=0.0
               endif
               if ( vbxxsavail .gt. 1e-20 ) then
                  pSvbxxup=fSvbxxup*delt/vbxxSavail
!                 ps5vbxxup=fs5vbxxup*delt/vbxxsavail
               else
                  pSvbxxup=0.0
!                 ps5vbxxup=0.0
               endif

               if ( ikmrk1.eq.1 .and. hmax .gt. 0.0 ) then

                  ! active water segment

                  hmax = min(hmax,totaldepth)
                  zm = totaldepth - hmax
                  z1 = localdepth - depth
                  z2 = localdepth

                  if (zm .gt. z2) then
                     ! not in segment:
                  elseif (zm . lt. z1 ) then
                     ! partialy in segment:
                     frlay = (z2-zm)/depth
                     fN1vbxxupy = pNvbxxup*nh4*frlay*depth/delt
                     fN2vbxxupy = pNvbxxup*no3*frlay*depth/delt
                     fP1vbxxupy = pPvbxxup*aap*frlay*depth/delt
                     fP2vbxxupy = pPvbxxup*po4*frlay*depth/delt
                     fS1vbxxupy = pSvbxxup*so4*frlay*depth/delt
                     fS2vbxxupy = pSvbxxup*sud*frlay*depth/delt
                  else
                     ! completely in segment:
                     fN1vbxxupy = pNvbxxup*nh4*depth/delt
                     fN2vbxxupy = pNvbxxup*no3*depth/delt
                     fP1vbxxupy = pPvbxxup*aap*depth/delt
                     fP2vbxxupy = pPvbxxup*po4*depth/delt
                     fS1vbxxupy = pSvbxxup*so4*depth/delt
                     fS2vbxxupy = pSvbxxup*sud*depth/delt
                  endif

               elseif (ikmrk1.eq.3 .and. hmax .lt. 0.0) then

                  ! delwaq-g segment, distribution over the bottom segments

                  hmax = -hmax
                  hmax = min(hmax,totaldepth)
                  z1 = localdepth - depth

                  if (hmax .gt. localdepth) then
                     ! completely in segment:
                     fN1vbxxupy = pNvbxxup*nh4*depth/delt
                     fN2vbxxupy = pNvbxxup*no3*depth/delt
                     fP1vbxxupy = pPvbxxup*aap*depth/delt
                     fP2vbxxupy = pPvbxxup*po4*depth/delt
                     fS1vbxxupy = pSvbxxup*so4*depth/delt
                     fS2vbxxupy = pSvbxxup*sud*depth/delt
                  elseif (hmax .gt. z1 ) then
                     ! partialy in segment:
                     frlay = (hmax-z1)/depth
                     fN1vbxxupy = pNvbxxup*nh4*frlay*depth/delt
                     fN2vbxxupy = pNvbxxup*no3*frlay*depth/delt
                     fP1vbxxupy = pPvbxxup*aap*frlay*depth/delt
                     fP2vbxxupy = pPvbxxup*po4*frlay*depth/delt
                     fS1vbxxupy = pSvbxxup*so4*frlay*depth/delt
                     fS2vbxxupy = pSvbxxup*sud*frlay*depth/delt
                  else
                     ! not in segment:
                  endif

               endif

               fl_fn1vbxxupy =  fn1vbxxupy/depth
               fl_fn2vbxxupy =  fn2vbxxupy/depth
               fl_fp1vbxxupy =  fp1vbxxupy/depth
               fl_fp2vbxxupy =  fp2vbxxupy/depth
               fl_fs1vbxxupy =  fs1vbxxupy/depth
               fl_fs2vbxxupy =  fs2vbxxupy/depth

            endif

!!         endif

         pmsa(ipnt(20)) =  fn1vbxxupy
         pmsa(ipnt(21)) =  fn2vbxxupy
         pmsa(ipnt(22)) =  fp1vbxxupy
         pmsa(ipnt(23)) =  fp2vbxxupy
         pmsa(ipnt(24)) =  fs1vbxxupy
         pmsa(ipnt(25)) =  fs2vbxxupy
         fl(iflux+1) =  fl_fn1vbxxupy
         fl(iflux+2) =  fl_fn2vbxxupy
         fl(iflux+3) =  fl_fp1vbxxupy
         fl(iflux+4) =  fl_fp2vbxxupy
         fl(iflux+5) =  fl_fs1vbxxupy
         fl(iflux+6) =  fl_fs2vbxxupy

         ipnt  = ipnt  + increm(1:npnt)
         iflux = iflux + noflux

      enddo

      return
      end
