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

module p10cor_mod
!
!  module declarations
!
!  data definition module(s)
!
use precision_part               ! single/double precision
      use timers
!
!  module procedure(s)
!
use openfl_mod              ! explicit interface
use grid_search_mod         ! explicit interface
use stop_exit_mod           ! explicit interface
!
implicit none               ! force explicit typing
!
contains
      subroutine  p10cor( tmin1, tmin0 , xp   , yp    , zp    , xpold , &
                          ypold, zpold , chi0 , chi1  , dtstop, rj0   , &
                          vx   , vy    , vz   , vvx   , vvy   , vvz   , &
                          c2   , c3    , ipc  , vx0   , vy0   , vz0   , &
                          ipcgo, accrjv)
!
!
!                   Deltares (former: Deltares)
!
!                        d e l p a r    v3.22
!
!
!     system administration : r.j. vos
!
!
!     created               : january 1990, by l. postma
!
!     modified              : 20-11-96 by vos and van kester, v322
!
!     function              : computes corrected displacements,
!                             and/or corrected time step
!                             corrected displacement is only calculated
!                             for corrected time > dtstop
!                             otherwise only the corrected time is needed
!                             for the predicted position (=corrected position)
!
!     notes                 : option ipc=3 from dunsbergen is implemented
!                             interpolation routine (uvwint) is revised
!
!     logical unit numbers  : none.
!
!     subroutines called    : none.
!
!     functions   called    : none.
!
!     parameters            :
!
!     name    kind     length     funct.  description
!     ====    ====     ======     ======  ===========
!     c2      real        1       input   depth curve of the wind component
!     c3      real        1       input   depth curve vertical velocities
!     chi0    real        1       input   ds/dt at old position
!     chi1    real        1       input   ds/dt at new position
!     dtstop  real        1       input   time step size
!     depth   real      mnmaxk    input   depth of segment  (in m)
!     dx      real      mnmaxk    input   delta x for the segments
!     dy      real      mnmaxk    input   delta y for the segments
!     flow    real    2*mnmaxk    input   flows in two directions
!     ipc     integer     1       in/out  option numerical scheme
!     ipcgo   integer     1       in/out  when 0 time step ends for particle
!                                         due to corrector (1 on input)
!     mp      integer     1       in/out  1th grid index particles
!     np      integer     1       in/out  2th grid index particles
!     kp      integer     1       in/out  3th grid index particles
!     rj0     real        1       input   volume (jacobian of this cel)
!     tmin1   integer     1       in/out  input/output for time step
!     tmin0   integer     1       input   input for time step
!     vvx     real        1       input   particle velocity in x dir.
!     vvy     real        1       input   particle velocity in y dir.
!     vvz     real        1       input   particle velocity in z dir.
!     vx      real        1       input   particle velocity in x dir. normalized
!     vy      real        1       input   particle velocity in y dir. normalized
!     vz      real        1       input   particle velocity in z dir. normalized
!     xp      real        1       input   predicted x-coordinate
!     yp      real        1       input   predicted y-coordinate
!     zp      real        1       input   predicted z-coordinate
!     xpold   real        1       input   initial   x-coordinate
!     ypold   real        1       input   initial   y-coordinate
!     zpold   real        1       input   initial   z-coordinate
! ---------------------------------------------------------------
!     acc     real        1       local   accuracy parameter
!     accura  real        1       local   accuracy parameter
!
      real (sp), parameter :: acc = 1.0e-15, accura = 1.0e-5
!
!     local scalars
!
      integer(ip) :: ipc, ipcgo
!
!     local scalars
!
      real   (sp) :: abs  , accrjv , c2     , c3   , chi0  , chi1   , dtstop
      real   (sp) :: exp  , rj0    , tcor   , tmin0, tmin1 , tmingo , vvx
      real   (sp) :: vvy  , vvz    , vx     , vx0  , vy    , vy0    , vz
      real   (sp) :: vz0  , xp     , xpold  , yp   , ypold , zp     , zpold
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "p10cor", ithndl )
!
!     compute the time needed (tcor) to reach predicted position
!     according to the corrector step
!
!.. compute the derivatives ds/dt at old and new positions
!
      if(ipc==3) then
         if(abs(chi0) > accura) then
            chi0 = rj0/chi0
         else
!rjv        chi0 = 1.0
!*jvk
            chi0 = rj0
         endif
         if(abs(chi1) > accura) then
            chi1 = rj0/chi1
         else
!rjv        chi1 = 1.0
!*jvk
            chi1 = rj0
         endif
         if (abs(chi1-chi0) > acc) then
           tcor = tmin0*(chi1-chi0)/(rj0*log(1.0+(chi1-chi0)/chi0))
         else
           tcor = tmin0*chi0/rj0
         endif
      elseif(ipc==5) then
           tcor = 2.0*tmin0*chi0/(chi0+chi1)
!jvk:
!
!          tcor = 2.0*tmin0     /(chi0+chi1)
      else
           write(*,*) ' Numerical option ipc = ',ipc
           write(*,*) ' Error: this option for ipc is not implemented '
           call stop_exit(1)
      endif
!
!     determine whether predicted particle position must be
!     corrected and whether time must be corrected
!
      if (( tcor > accura ) .and.      &
          abs(tcor-tmin0) > accura            ) then
!
!     introduce the time correction as time step
!
          tmin1  = tcor
!
!     only determine a new position when tmin1 > dtstop
!     otherwise time correction is sufficient (pred.= corr. position)
!
          if (tmin1 >= dtstop) then
             tmin1  = dtstop
!jvk  !!!!!!
             ipcgo  = 0
!
!     corrected s(t) required for travelling tmin1
!
             if(ipc==3) then
                if (chi1-chi0 > acc) then
                   tmingo = tcor*rj0                       &
                            *log(1.0+tmin1*(chi1-chi0)/(tcor*chi0))  &
                            /(chi1-chi0)
                else
                   tmingo = tmin1*rj0/chi0
                endif
             elseif(ipc==5) then
                tmingo = 0.5*(chi0+chi1)*tmin1/chi0
!jvk:
!
!               tmingo = 0.5*(chi0+chi1)*tmin1
             else
                stop ' This option for ipc is not implemented '
             endif
!
!     corrected position differs from predicted one and becomes:
!
             xp = xpold
             yp = ypold
             zp = zpold
!
             if (abs(vvx)  >  accrjv ) then
                xp = (vx / vvx) * exp(c2 * vvx * tmingo) - vx0 / vvx
             else
                xp = xp + vx * tmingo * c2
             endif
!
             if (abs(vvy)  >  accrjv ) then
                yp = (vy / vvy) * exp(c2 * vvy * tmingo) - vy0 / vvy
             else
                yp = yp + vy * tmingo * c2
             endif
!
             if (abs(vvz)  >  accrjv ) then
                zp = (vz / vvz) * exp(c3 * vvz * tmingo) - vz0 / vvz
             else
                zp = zp + vz * tmingo * c3
             endif
          endif
!
      endif
!
      if ( timon ) call timstop ( ithndl )
      return
      end subroutine
end module
