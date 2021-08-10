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

      subroutine part03 ( lgrid  , volume , flow   , dx     , dy     ,   &
                          nmax   , mmax   , mnmaxk , lgrid2 , velo   ,   &
                          layt   , area   , depth  , dps    , locdep ,   &
                          zlevel , tcktot , ltrack)
!
!
!                   Deltares (former: Deltares)
!
!                        d e l p a r    v3.10
!
!
!     system administration : r.j. vos
!
!
!     created               : july 1991, by l.postma
!
!
!     function              : computes velocities
!
!
!     note                  : none.
!
!
!     logical unit numbers  : none.
!
!
!     functions   called    : none.
!
!     i0      integer     1       local   grid help variable
!     i1      integer     1       local   grid help variable
!     i2      integer     1       local   grid help variable
!     i3      integer     1       local   grid help variable
!     i4      integer     1       local   grid help variable
!     sum     real        1       local   summed help variable
!     vx      real        1       local   velocity in x direction
!     vy      real        1       local   velocity in y direction
!
      use precision_part    ! single/double precision
      use timers
      implicit none    ! force explicit typing

!     parameters

!     kind         function         name                     Descriptipon

      integer(ip), intent(in   ) :: nmax                   !< first dimension lgrid
      integer(ip), intent(in   ) :: mmax                   !< second dimension lgrid
      integer(ip), intent(in   ) :: mnmaxk                 !< total size of 3D matrix
      integer(ip), intent(in   ) :: layt                   !< number of layers
      integer(ip), intent(in   ) :: lgrid (nmax,mmax)      !< active grid indices matrix
      integer(ip), intent(in   ) :: lgrid2(nmax,mmax)      !< total grid indices matrix
      real   (rp), intent(in   ) :: dx    (nmax*mmax)      !< x distance of grid cell
      real   (rp), intent(in   ) :: dy    (nmax*mmax)      !< y distance of grid cell
      real   (rp), intent(in   ) :: volume(mnmaxk)         !< volumes
      real   (rp), intent(in   ) :: flow  (*)              !< flows
      real   (rp), intent(in   ) :: area  (nmax*mmax)      !< horizontal surface area
      real   (rp), intent(  out) :: depth (nmax*mmax)      !< water depth
      real   (rp), intent(  out) :: velo  (mnmaxk)         !< velocities in 3D
      real   (rp), intent(in   ) :: dps   (nmax*mmax)      !< bed depth
      real   (rp), intent(  out) :: locdep(nmax*mmax,layt) !< depth per layer
      real   (rp), intent(  out) :: zlevel(nmax*mmax)
      real   (rp), intent(in   ) :: tcktot(layt)
      logical    , intent(in   ) :: ltrack

      real (sp) ::  default = 999.999

!     local scalars

      integer(ip)  i0       !    grid help variable
      integer(ip)  i1       !    grid help variable
      integer(ip)  i2       !    grid help variable
      integer(ip)  i3       !    grid help variable
      integer(ip)  i4       !    grid help variable
      real   (rp)  sum      !    summed help variable
      real   (rp)  vx       !    velocity in x direction
      real   (rp)  vy       !    velocity in y direction
      integer(ip)  i03d , i33d, i43d
      integer(ip)  ilay
      real   (dp)  dplay

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "part03", ithndl )
!
!     initialisation
!
      velo   = 0.0 ! whole array assignment
      depth  = 0.0 ! whole array assignment
      locdep = 0.0 ! whole array assignment
!
!     loop over the segments
!
      do 300 ilay = 1, layt
         do 200 i2 = 2, mmax
           do 150 i1 = 2, nmax
!
!            active?
!
             i0 = lgrid(i1, i2)
             if (i0  >  0) then
               i03d = i0 + (ilay-1)*nmax*mmax
!
!              magnitude of the velocities
!
               vy  = flow(i03d        ) / volume(i03d) * dy(i0)
               vx  = flow(i03d+ mnmaxk) / volume(i03d) * dx(i0)
!
!              calculate sum; value >= 0
!
               sum = vx**2 + vy**2
!
               i3 = lgrid2(i1 - 1, i2    )
               i33d = i3 + (ilay-1)*nmax*mmax
               if (i3  >  0) then
                 vy  = flow(i33d        ) / volume(i03d) * dy(i0)
                 sum = sum + vy**2
               endif
!
               i4 = lgrid2(i1    , i2 - 1)
               i43d = i4 + (ilay-1)*nmax*mmax
               if (i4  >  0) then
                 vx  = flow(i43d+mnmaxk) / volume(i03d) * dx(i0)
                 sum = sum + vx**2
               endif
!
               velo(i03d) = sqrt(sum / 2.0)
               depth(i0)  = depth(i0) + volume(i03d)
             endif
!
!            end of loop
!
  150      continue
  200    continue
  300 continue
!
      do 400 i2 = 2, mmax
         do 350 i1 = 2, nmax
!
!            active?
!
             i0 = lgrid(i1, i2)
             if (i0  >  0) then
                if(area(i0)==(0.0)) then
                   write (*,*) ' Area zero in active segment'
                   call stop_exit(1)
                endif
                depth(i0) = depth(i0)/area(i0)
             endif
  350    continue
  400 continue
!
!     determine local depth per layer
!     local depth = distance free water surface till (bottom of) layer
!
      do i2 = 1, mmax
         do i1 = 1, nmax
            i0 = lgrid(i1, i2)
            dplay = 0.0
            do ilay = 1, layt
               if (i0  >  0) then
                  i03d         = i0    + (ilay-1)*nmax*mmax
                  dplay        = dplay + depth(i0)*tcktot(ilay)
                  locdep(i0,ilay) = dplay
               endif
            enddo
         enddo
      enddo
!
!     determine z coordinate free water surface
!     totdep = total depth (free water surface - bottom)
!     dps    = bathymetry (w.r.t. reference level) in water level points
!
!     note that zlevel is only used for calculating the vertical
!     position(za) of particles in case of storing the particle tracks
!     (itrack=1).
!
      if (ltrack) then
         do i2 = 1, mmax
         do i1 = 1, nmax
            i0 = lgrid(i1, i2)
            if (i0  >  0) then
               i03d       = i0  + (layt-1)*nmax*mmax
!              zlevel(i0) = locdep(i03d) - dps(i0)
               zlevel(i0) = locdep(i0,layt) - dps(i0)
            endif
         enddo
         enddo
      else
         zlevel = default  ! whole array assignment
      endif
!
!     end of subroutine
!
      if ( timon ) call timstop ( ithndl )
      return
!
      end subroutine
