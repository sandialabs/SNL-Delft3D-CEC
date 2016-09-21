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

module extout_mod
!
!  module declarations
!
!  data definition module(s)
!
use precision_part    ! single/double precision
      use timers
!
implicit none    ! force explicit typing
!
contains
      subroutine extout ( lgrid  , lgrid2 ,                  &
                          nmax   , mmax   , conc   ,         &
                          volume , layt   , vdiff  ,         &
                          itime  , idelt  , icwsta ,         &
                          icwsto , icwste , lun2   ,         &
                          modtyp , nosubs , nolay  ,         &
                          mnmax2 , isfile , nosubc ,         &
                          depth  , nosube , chezy  ,         &
                          rhow   , flow   , dx     ,         &
                          dy     )
!
!
!    Deltares (former: Deltares)
!
!
!    d e l p a r    v3.43
!
!
!     system administration : r.j. vos
!
!
!     created               : july 1998 by r.j. vos
!
!
!     function              : generates extra output variables
!
!     note                  : only properties that are independent of particles
!                             positions, but depent on grid coordinates
!                             names are set in rdparm
!                             (from ak - v3.06.05)
!                             results from this routine maybe highly unreliable. to get rid of
!                             some undefined variables some values were assigned.
!                             all (4) extra output parameters are now assigned the default
!                             value (-999.99) to notify the user that this output is unreleastic.
!
!     logical unit numbers  :
!                             lun2 - output log file
!
!
!     subroutines called    : none.
!
!
!     functions   called    : none.
!
!
!     parameters            :
!
!     name    kind     length     funct.  description
!     ====    ====     ======     ======  ===========
!     conc    real  nosubc*mnmaxk output  concentration work-array
!     depth   real  nosubc*mnmaxk output  depth array
!     icwsta  integer     1       input   start time map-file
!     icwste  integer     1       input   time step map-file
!     icwsto  integer     1       input   stop  time map-file
!     idelt   integer     1       input   model time step
!     isfile  integer   nosubs    input   when 1 a concentration is not updated
!                                         but follows from an exteranl file
!     itime   integer     1       input   model time
!     layt    integer     1       input   number of layers hydrodynamics
!     lgrid   integer  nmax*mmax  input   active grid layout of the area
!     lgrid2  integer  nmax*mmax  input   total grid layout of the area
!     lsettl  logical     1       input   if true there is an extra bed layer
!     lun2    integer     1       input   unit number 2 (logging)
!     mapsub  integer   nosubs    input   index array for substances (reserved ones)
!     mmax    integer     1       input   second dimension of lgrid
!     mnmaxv  integer     1       input   dimension of volume
!     mnmaxk  integer     1       input   dimension of conc-array (+1 layer for sedimentation)
!     mnmax2  integer     1       input   dimension of one layer
!     modtyp  integer     1       input   model-run-type
!     nmax    integer     1       input   leading dimension of lgrid
!     nolay   integer     1       input   actual number of layers
!     nosubc  integer     1       input   leading dimension conc-array
!     nosubs  integer     1       input   actual number of substances
!     subst   char*20   nosubs    input   substance names with layer extension
!     volume  real      mnmaxk    input   volumes of the grid cells
!     ----    ----     ------     ------  -----------
!     mapfil  logical     1       local   .true. if mapfil output
!     subnam  char.* 6    1       local   name of this program/file
!
!
!     save values between invocations: specified precisely those needed!!
!
!     declarations
!
      logical           :: mapfil
!
!     parameters
!
      character(len=6), parameter :: subnam = 'extout'
!
      real(sp), parameter  :: grav = 9.81
!
!     declare putget help var's
!
!     dimensioning
!
      real   (sp) , dimension(:)     :: volume,  depth, vdiff,  flow
      real   (sp) , dimension(:)     :: dx    , dy
      real   (sp) , dimension(:,:)   :: conc
      real   (sp) , dimension(:,:)   :: lgrid , lgrid2
      integer(ip) , dimension(:)     :: isfile

      real   (sp) ::  default = 999.999
!
!     local scalars
!
      integer(ip) ::  i1    , i2    , ic    , ipos  , lun2   , icwsta, icwste, icwsto
      integer(ip) ::  idelt , ilay  , iseg  , isub  , itime  , layt  , m     , mmax
      integer(ip) ::  mnmaxk, mnmax2, modtyp, nmax  , nolay  , nosubc, n0    , n1
      integer(ip) ::  n2    , n     , nosube, nosubs
      real   (sp) ::  chezy , gc2   , vol   , vy0   , rhow   , sqrt  , vvx   , vvy
      real   (sp) ::  vx    , vx0   , vx1   , vxr   , vy     , vy1   , vyr   , xp
      real   (sp) ::  yp
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "extout", ithndl )
!
!     determine if there is extra output
!
      if(nosube == 0) goto 9999
!
!     determine if map file must be produced
!
      mapfil = .true.
      if (icwste                     < 1     ) mapfil = .false.
      if (itime                      < icwsta) mapfil = .false.
      if (itime - idelt              >=  icwsto) mapfil = .false.
      if (mod(itime-icwsta, icwste)  >=  idelt ) mapfil = .false.
!
      if(.not.mapfil) goto 9999
!
!.. zero the conc array for extra substances
!
      isfile(nosubs+1) = 0
      isfile(nosubs+2) = 0
      isfile(nosubs+3) = 0
      isfile(nosubs+4) = 0
!
      iseg = 0
      do 10  ilay = 1, layt
      do 20  m = 1, mmax
      do 30  n = 1, nmax
         iseg = iseg + 1
         do 40  isub = nosubs+1, nosubc
            if(isfile(isub) /= 1) then
               conc(isub,iseg) = 0.0
            else
!
!.. clean up external file outside the domain of water
!
               if(lgrid(n,m) <= 0) then
                  conc(isub,iseg) = 0.0
               endif
            endif
40       continue
30    continue
20    continue
10    continue
!
!     insert values
!
      iseg = 0
      if(chezy > 1.0) then
         gc2 = grav / chezy / chezy
      else
         gc2 = grav / 2500.0
      endif
!
!.. layt is the real number of layers, the bed is not included
!
!     mnmaxk undefined - maybe this is the right value ?
!
      mnmaxk = mmax*nmax*layt
      do 50  ilay = 1, layt
      do 60  m = 1, mmax
      do 70  n = 1, nmax
         iseg = iseg + 1
         if(isfile(isub) /= 1.and. lgrid(n,m) > 0) then
!
!.. velocities
!
               isub   = nosubs + 1
               n0     = lgrid2(n    , m    )
               n1     = lgrid2(n - 1, m    )
               n2     = lgrid2(n    , m - 1)
               vol    = volume(iseg)
               vy0    = flow  (n1 + ilay  ) / vol
               vy1    = flow  (n0 + ilay  ) / vol
               vx0    = flow  (n2 + ilay + mnmaxk) / vol
               vx1    = flow  (n0 + ilay + mnmaxk) / vol
               vvx    = vx1 - vx0
               vvy    = vy1 - vy0
!              xp and yp undefined - this is certainly not a right value ???
               xp     = 0.0
               yp     = 0.0
               vx     = vx0 + xp * vvx
               vy     = vy0 + yp * vvy
               vxr    = vx  * dx(n0)
               vyr    = vy  * dy(n0)
               conc(isub,iseg) = sqrt(vxr*vxr + vyr*vyr)
               conc(isub,iseg) = default
!
!.. tau bottom
!
               isub   = isub   + 1
               conc(isub,iseg) = rhow*gc2*(vxr*vxr + vyr*vyr)
               conc(isub,iseg) = default
!
!.. depth
!
               isub   = isub   + 1
               conc(isub,iseg) = depth(n0)
               conc(isub,iseg) = default
!
!.. vertical dispersion
!
               isub   = isub   + 1
               conc(isub,iseg) = vdiff(iseg)
               conc(isub,iseg) = default
!
         endif
70    continue
60    continue
50    continue
!
!     insert missing values
!
      do 500 i1 = 1, nmax
        do 450 i2 = 1, mmax
          ic = lgrid2(i1, i2)
          if (ic  >  1) then
            if (lgrid(i1, i2)  < 1) then
              do 395 ilay = 1, nolay
                 iseg = (ilay-1)*mnmax2 + ic
                 do 400 isub = nosubs + 1, nosubc
                    if(isfile(isub) /= 1) then
                       if(modtyp /= 2) then
                          conc(isub, iseg) = -999.0
                       else
                          ipos = ilay + (isub-1)*nolay
                          conc(ipos, ic  ) = -999.0
                       endif
                    endif
  400            continue
  395         continue
            endif
          endif
  450   continue
  500 continue
!
!.. protect for part12
!
      isfile(nosubs+1) = 1
      isfile(nosubs+2) = 1
      isfile(nosubs+3) = 1
      isfile(nosubs+4) = 1
!
      write (lun2, 99001)subnam,itime /86400,mod(itime ,86400)        &
                         /3600,mod(itime ,3600)/60, mod(itime ,60)
      write (*   , 99001)subnam,itime /86400,mod(itime ,86400)        &
                         /3600,mod(itime ,3600)/60, mod(itime ,60)
!
!     end of subroutine
!
 9999 if ( timon ) call timstop ( ithndl )
      return
!
!     formats
!
99001 format( ' EXTRA output to mapfile in ', a6,             &
               ' at simulation time :'         &
               ,i3,'d ',i2,'h ',i2,'m ',i2,'s !')
!
      end subroutine
end module
