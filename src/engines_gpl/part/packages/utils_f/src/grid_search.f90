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

module grid_search_mod
!
!  module procedure for grid searching routines, including
!
!  1. routine find
!  2. routine findcell
!  3. routine part07
!
!  (for description, see routine header)
!
!
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
use stop_exit_mod           ! explicit interface
!
implicit none               ! force explicit typing
!
contains
! -------------------------------------------------------------------------------------
!     Routine find
! -------------------------------------------------------------------------------------
      subroutine find   ( x    , y  , radius , np     , mp , &
                          lgrid, dx , dy     , lcircl )
!
!                        Deltares (former: Deltares)
!
!                        d e l p a r    v3.10
!
!
!     system administration : m. zeeuw
!
!
!     created               : july     1991, by l. postma
!
!
!     function              : finds a suitable place for a particle
!
!     modified              : june 1996 by rj vos: option on circle (lcircl)
!
!     logical unit numbers  : none.
!
!
!     subroutines called    : none.
!
!     functions   called    : rnd.
!
!
!     parameters            :
!
!     name    kind     length     funct.  description
!     ====    ====     ======     ======  ===========
!     dx      real      mnmaxk    input   dx of the cells
!     dy      real      mnmaxk    input   dy of the cells
!     lcircl  locigal     1       input   if true particles on circle
!     lgrid   integer  nmax*mmax  input   active grid table
!     mp      integer     1       in/out  m-values of particle
!     nmax    integer     1       input   dimension of lgrid
!     np      integer     1       in/out  n-values of particle
!     radius  real        1       input   radius of the load
!     x       real        1       in/out  x-value of particle
!     y       real        1       in/out  y-value of particle
!     ----    ----     ------     ------  -----------
!     angle   real        1       local   randomized angle
!     cflag   logical     1       local   .false. when ready
!     movx    logical     1       local   .true. when move in x dir.
!     movy    logical     1       local   .true. when move in y dir.
!     n0      integer     1       local   lgrid value
!     radiuh  real        1       local   randomized radius
!     rseed   real        1       local   seed for randomizer
!
!
!     save values between invocations
!
      save
!
!     declarations
!
      real(dp)     ::  rseed =  0.5d+00
      logical      ::  cflag , movx  , movy
      logical      ::  lcircl
!
!     parameters
!
      real(sp), parameter   :: twopi  = 6.2831853
!
!     dimensioning
!
      real(sp)   , dimension(:)    :: dx   , dy
      integer(sp), dimension(:,:)  :: lgrid
!
!     local scalars
!
      integer (ip) ::   mp   , n0     , np
      real    (sp) ::   angle, radiuh , radius , x   ,    y
      real    (sp) ::   rnd  , sqrt

!
!     note:
!       random function rnd() must be declared external, as it is an
!       intrinsic function for the lahey fortran95 compiler under linux
!
      external rnd
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "find", ithndl )
!
      if (radius  >=  0.1) then
!
!       test for correct x- and y-values
!
        n0      = lgrid(np, mp)
        if(.not.lcircl) then
           radiuh = radius * sqrt(rnd(rseed))
        else
           radiuh = radius
        endif
        angle  = rnd(rseed) * twopi
        x = x + radiuh * sin(angle) / dx(n0)
        y = y + radiuh * cos(angle) / dy(n0)
!
  100   cflag = .true.
        movx  = .false.
        movy  = .false.
!
        if     (x  >  1.0) then
!
          cflag = .false.
          movx  = .true.
          if (lgrid(np    , mp + 1)  < 1) then
            x  = 2.0 - x
          else
            x  = x - 1.0
            mp = mp + 1
          endif
          x = x * dx(n0)
!
        elseif (x  < 0.0) then
!
          cflag = .false.
          movx  = .true.
          if (lgrid(np    , mp - 1)  < 1) then
            x  = -x * dx(n0)
          else
            x  =  x * dx(n0)
            mp =  mp - 1
            n0  =  lgrid(np, mp)
            x  =  x + dx(n0)
          endif
!
        endif
!
        if     (y  >  1.0) then
!
          cflag = .false.
          movy  = .true.
          if (lgrid(np + 1, mp    )  < 1) then
            y  = 2.0 - y
          else
            y  = y - 1.0
            np = np + 1
          endif
          y = y * dy(n0)
!
        elseif (y  < 0.0) then
!
          cflag = .false.
          movy  = .true.
          if (lgrid(np - 1, mp    )  < 1) then
            y  = -y * dy(n0)
          else
            y  =  y * dy(n0)
            np =  np - 1
            n0  =  lgrid(np, mp)
            y  =  y + dy(n0)
          endif
!
        endif
!
        if (.not. cflag) then
          n0 = lgrid(np, mp)
          if (movx) then
            x = x / dx(n0)
          endif
          if (movy) then
            y = y / dy(n0)
          endif
          goto 100
        endif
      endif
!
!     end of subroutine
!
      if ( timon ) call timstop ( ithndl )
      return
!
      end subroutine find


! -------------------------------------------------------------------------------------
!     Routine findcell
! -------------------------------------------------------------------------------------

      subroutine findcell ( nmax   , mmax   , xnloc  , ynloc  , lgrid  ,   &
                            lgrid2 , xb     , yb     , nmloc  )

!     Deltares Software Centre

      use pinpok_mod
      implicit none

!     Arguments

!     kind            function         name                description

      integer  ( ip), intent(in   ) :: nmax              !< 1st dimension of the flow grid
      integer  ( ip), intent(in   ) :: mmax              !< 2nd dimension of the flow grid
      real     ( rp), intent(in   ) :: xnloc             !< x-value of the point
      real     ( rp), intent(in   ) :: ynloc             !< y-value of the point
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax) !< active grid matrix
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax) !< total grid matrix
      real     ( rp), intent(in   ) :: xb    (nmax*mmax) !< x-values of the grid
      real     ( rp), intent(in   ) :: yb    (nmax*mmax) !< y-values of the grid
      integer  ( ip), intent(  out) :: nmloc             !< linear index of grid cell

!     Locals

      real     ( rp) amin                !  used to find the minimum
      integer  ( ip) ns, ms              !  location of the minimum
      integer  ( ip) n , m               !  loop variables for the grid
      real     ( rp) distpow2            !  distance ** 2
      integer  ( ip) i , j               !  loop variables for the grid
      real     ( rp) x(4), y(4)          !  x,y of cell corners
      integer  ( ip) nm, nmd, ndm, ndmd  !  help variables indices in the grid
      integer  ( ip) inside              !  1 if point is inside the grid-cell
      integer  ( ip) ierror              !  0 if successful, 1 if on error

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "findcell", ithndl )

      ierror = 1
      amin   = 1.0e+30
      ns   = 0
      ms   = 0
      do m = 2, mmax
         do n = 2, nmax
            nm   = lgrid2( n  , m   )
            ndmd = lgrid2( n-1, m-1 )
            ndm  = lgrid2( n-1, m   )
            nmd  = lgrid2( n  , m-1 )

            ! active bed level point

            if ( ndmd > 1 .or. ndm > 1 .or. nm > 1 .or. nmd > 1 ) then
               distpow2 = (xb(ndmd)-xnloc)**2 + (yb(ndmd)-ynloc)**2
               if ( distpow2 .lt. amin ) then
                  amin   = distpow2
                  ms     = m-1
                  ns     = n-1
               endif
            endif
         enddo
      enddo
!
outer1:do j = 0, 1
         do i = 0, 1

            n = ns + i
            m = ms + j
            if ( lgrid( n, m ) .le. 0 ) cycle
            nm   = lgrid2( n  , m   )
            ndmd = lgrid2( n-1, m-1 )
            ndm  = lgrid2( n-1, m   )
            nmd  = lgrid2( n  , m-1 )
            x(1) = xb(ndmd)
            x(2) = xb(ndm )
            x(3) = xb(nm  )
            x(4) = xb(nmd )
            y(1) = yb(ndmd)
            y(2) = yb(ndm )
            y(3) = yb(nm  )
            y(4) = yb(nmd)
            call pinpok( xnloc, ynloc, 4, x, y, inside )
            if ( inside .eq. 1 ) then
               ierror = 0
               nmloc  = nm
               exit outer1
            endif

         enddo
      enddo  outer1

      if ( ierror .ne. 0 ) then ! the method above is faster, but doesn't always work with DD. When the cell was not found use, pinpok for all cells until found
outer2:  do m = 2, mmax
            do n = 2, nmax
               if ( lgrid( n, m ) .le. 0 ) cycle
               nm   = lgrid2( n  , m   )
               ndmd = lgrid2( n-1, m-1 )
               ndm  = lgrid2( n-1, m   )
               nmd  = lgrid2( n  , m-1 )

               x(1) = xb(ndmd)
               x(2) = xb(ndm )
               x(3) = xb(nm  )
               x(4) = xb(nmd )
               y(1) = yb(ndmd)
               y(2) = yb(ndm )
               y(3) = yb(nm  )
               y(4) = yb(nmd)
               call pinpok( xnloc, ynloc, 4, x, y, inside )
               if ( inside .eq. 1 ) then
                  ierror = 0
                  nmloc  = nm
                  exit outer2
               endif
            enddo
         enddo outer2
      end if

      if ( ierror .ne. 0 ) nmloc = 1           ! still not found, so probably outside the grid?!?

      if ( timon ) call timstop ( ithndl )
      return
      end subroutine findcell

! -------------------------------------------------------------------------------------
!     Routine part07
! -------------------------------------------------------------------------------------

      subroutine part07 ( lgrid  , lgrid2 , nmax   , mmax   , xb     ,  &
                          yb     , xnloc  , ynloc  , nmloc  , mmloc  ,  &
                          xmloc  , ymloc  , ierror )

!       Deltares Software Centre

!>\file
!>            Determines the grid cell and relative coordinates of a give point in global coordinates

!     System administration : Antoon Koster

!     created               : July 1994, by Robert Vos

!     logical unit numbers  : none.

!     subroutines called    : bilin5 - bilinear coefficients within the grid

!     functions   called    : none.

!     Arguments

!     kind            function         name                   description

      integer  ( ip), intent(in   ) :: nmax                 !< first index hydrodynamic grid
      integer  ( ip), intent(in   ) :: mmax                 !< second index hydrodynamic grid
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax)    !< active grid matrix
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax)    !< total grid matrix
      real     ( rp), intent(in   ) :: xb    (nmax*mmax)    !< x of grid corner points
      real     ( rp), intent(in   ) :: yb    (nmax*mmax)    !< y of grid corner points
      real     ( rp), intent(in   ) :: xnloc                !< x of the point to locate
      real     ( rp), intent(in   ) :: ynloc                !< y of the point to locate
      integer  ( ip), intent(  out) :: nmloc                !< first grid index of the point
      integer  ( ip), intent(  out) :: mmloc                !< second grid index of the point
      real     ( rp), intent(  out) :: xmloc                !< local x in the grid of the point
      real     ( rp), intent(  out) :: ymloc                !< local y in the grid of the point
      integer  ( ip), intent(  out) :: ierror               !< if 1 if no valid location was found

!     locals

      real   (rp) :: x(4), y(4)      !   x,y of the corner points of the found cell
      real   (rp) :: w(6)            !   work array of bilin5
      integer(ip) :: n0, n1, n2, n3  !   4 linear grid indices of the cornerpoints of the cell
      integer(ip) :: ier             !   error variable of bilin5

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "part07", ithndl )

      x = 0
      y = 0
      w = 0
      ierror = 0

!     find the grid cel n0 in (nmloc,mmloc) in which (xnloc,ynloc) is located

      call findcell ( nmax   , mmax   , xnloc  , ynloc  , lgrid  ,  &
                      lgrid2 , xb     , yb     , n0     )

!     point in active grid cel n0 = lgrid2(nmloc,mmloc)

      if ( n0 .eq. 1 ) then          ! ( nloc  , mloc   )
         ierror = 1
         goto 9999
      endif
      n1 = n0 - 1                    ! ( nloc-1, mloc   )
      n2 = n0 - nmax                 ! ( nloc  , mloc-1 )
      n3 = n0 - nmax - 1             ! ( nloc-1, mloc-1 )
      nmloc = mod(n0-1, nmax) + 1
      mmloc =    (n0-1)/nmax  + 1

      x(1) = xb(n3)
      x(2) = xb(n1)
      x(3) = xb(n0)
      x(4) = xb(n2)
      y(1) = yb(n3)
      y(2) = yb(n1)
      y(3) = yb(n0)
      y(4) = yb(n2)
      call bilin5 ( x, y, xnloc, ynloc, w, ier )
      xmloc = w(5)                   ! local weight factors m-direction
      ymloc = w(6)                   ! local weight factors n-direction

 9999 if ( timon ) call timstop ( ithndl )
      return

      end subroutine part07

! -------------------------------------------------------------------------------------
!     Routine part07nm
! -------------------------------------------------------------------------------------

      subroutine part07nm ( lgrid  , lgrid2 , nmax   , mmax   , xb     ,  &
                            yb     , xnloc  , ynloc  , nmloc  , mmloc  ,  &
                            xmloc  , ymloc  , ierror )

!       Deltares Software Centre

!>\file
!>            Determines the relative coordinates to a given grid cell of a given point in global coordinates

!     System administration : Antoon Koster

!     created               : November 2013, by Michel Jeuken

!     logical unit numbers  : none.

!     subroutines called    : bilin5 - bilinear coefficients within the grid

!     functions   called    : none.

!     Arguments

!     kind            function         name                   description

      integer  ( ip), intent(in   ) :: nmax                 !< first index hydrodynamic grid
      integer  ( ip), intent(in   ) :: mmax                 !< second index hydrodynamic grid
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax)    !< active grid matrix
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax)    !< total grid matrix
      real     ( rp), intent(in   ) :: xb    (nmax*mmax)    !< x of grid corner points
      real     ( rp), intent(in   ) :: yb    (nmax*mmax)    !< y of grid corner points
      real     ( rp), intent(in   ) :: xnloc                !< x of the point to locate
      real     ( rp), intent(in   ) :: ynloc                !< y of the point to locate
      integer  ( ip), intent(in   ) :: nmloc                !< first grid index of the point
      integer  ( ip), intent(in   ) :: mmloc                !< second grid index of the point
      real     ( rp), intent(  out) :: xmloc                !< local x in the grid of the point
      real     ( rp), intent(  out) :: ymloc                !< local y in the grid of the point
      integer  ( ip), intent(  out) :: ierror               !< if 1 if no valid location was found

!     locals

      real   (rp) :: x(4), y(4)      !   x,y of the corner points of the found cell
      real   (rp) :: w(6)            !   work array of bilin5
      integer(ip) :: n0, n1, n2, n3  !   4 linear grid indices of the cornerpoints of the cell
      integer(ip) :: ier             !   error variable of bilin5

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "part07nm", ithndl )

      x = 0
      y = 0
      w = 0
      ierror = 0

!     point in active grid cel n0 = lgrid2(nmloc,mmloc)
      n0   = lgrid2( nmloc  , mmloc   )
      if ( n0 .eq. 1 ) then          ! ( nloc  , mloc   )
         ierror = 1
         goto 9999
      endif
      n1 = n0 - 1                    ! ( nloc-1, mloc   )
      n2 = n0 - nmax                 ! ( nloc  , mloc-1 )
      n3 = n0 - nmax - 1             ! ( nloc-1, mloc-1 )

      x(1) = xb(n3)
      x(2) = xb(n1)
      x(3) = xb(n0)
      x(4) = xb(n2)
      y(1) = yb(n3)
      y(2) = yb(n1)
      y(3) = yb(n0)
      y(4) = yb(n2)
      call bilin5 ( x, y, xnloc, ynloc, w, ier )
      xmloc = w(5)                   ! local weight factors m-direction
      ymloc = w(6)                   ! local weight factors n-direction

 9999 if ( timon ) call timstop ( ithndl )
      return

      end subroutine part07nm

! -------------------------------------------------------------------------------------
!     Aux. routine intl2v
! -------------------------------------------------------------------------------------
      subroutine intl2v (x , y , x0 , y0 , xl , yl,  memory )
!
!
!                   Deltares (former: Deltares)
!
!                        d e l p a r    v2.01
!
!
!     system administration : m. zeeuw
!
!     created      : november 1993, by r.j. vos
!
!     modified     : july  1994, by r.j. vos
!                    switching of x- and y- omitted
!                    (also part07 was therefore adapted !!!)
!
!     function     : determines the natural coordinates in a
!                    bilinear quadrilateral element when the
!                    corner coordinates (x,y) are given.
!
!     note           natural coordinates    parent domain
!
!                                  x 3
!                    4 x
!                                           4 .        . 3
!                         0(x0,y0)
!                               x 2
!                     1 x----x----->        1 .---yl-->. 2
!
!                    figure:
!                    local coordinate system according to hughes
!                    ' the finite element method ',section 3.2
!                    in parent domain:
!                    -1 < ex < 1  <=>  ex//xl     0 < xl < 1
!                    -1 < ey < 1  <=>  ey//yl     0 < yl < 1
!                     (note: n//y, m//x)
!
!
!     logical unit numbers  : output to screen (6)
!
!     subroutines called    : none
!
!     functions   called    : none.
!
!
!     parameters            :
!
!     name    kind     length     funct.  description
!     ====    ====     ======     ======  ===========
!     x       real     memory     input   x of corners of cell(national grid)
!     y       real     memory     input   y of corners of cell(national grid)
!     x0      real      1         input   x of location (national grid)
!     y0      real      1         input   y of location (national grid)
!     xl      real      1        output   x of location in parent domain
!     yl      real      1        output   y of location in parent domain
!     memory  int       1         input   array length (=4)
!
!
      implicit none           !   force explicit typing
      integer :: memory, i
      real    :: x0, y0, xl, yl
      integer, parameter           :: dp = kind(1.0d0)  ! double precision
      character(len=6), parameter  :: subnam = 'intl2v'
!
      real(sp), dimension(:)       :: x, y
!
      real(dp)                     :: da0, da1, da2, da3, db0, db1, db2, db3
      real(dp)                     :: a, b, c, d, discr, tweea, wd
      real(dp)                     :: ex, ey

      real(dp), parameter          ::  d1 = 1.0d0, d2 = 2.0d0, d4 = 4.0d0
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "int12v", ithndl )
!
      da0 = (  x(1) + x(2) + x(3) + x(4) ) / d4
      db0 = (  y(1) + y(2) + y(3) + y(4) ) / d4
      da1 = ( -x(1) + x(2) + x(3) - x(4) ) / d4
      db1 = ( -y(1) + y(2) + y(3) - y(4) ) / d4
      da2 = ( -x(1) - x(2) + x(3) + x(4) ) / d4
      db2 = ( -y(1) - y(2) + y(3) + y(4) ) / d4
      da3 = (  x(1) - x(2) + x(3) - x(4) ) / d4
      db3 = (  y(1) - y(2) + y(3) - y(4) ) / d4
!
      c =  da1*( db0 - y0 )  +  db1*( x0 - da0 )
      b =  da3*( db0 - y0 )  +  db3*( x0 - da0 ) + (db2*da1 - db1*da2)
      a =  db2*da3 - db3*da2
!
      if ( abs(a)  < 1.0d-6) then
!..
         ey=-c/b
!
      else
!
         discr = b*b-d4*a*c
         tweea = d2*a
         wd    = sqrt(discr)
         ey    = (-b+wd)/tweea
         if ( ey  < (-d1 -1.0d-5) .or. ey  >  1.00005d0 )   &
              ey = (-b-wd)/tweea
!
      endif
!
      d  =  da1 + da3*ey
      if ( abs(d)  < 1.0d-6) then
!
!..  this should not happen, otherwise you have a different coordinate system
!
         write (6,991) subnam
         call stop_exit(1)
      else
         ex = (x0 - da0 - da2*ey) / d
      endif
      if ( ex  < (-d1 -1.0d-5) .or. ex  >  1.00005d0 )  then
         write (6,992)  -d1 -1.0d-5 , ex , 1.00005d0,    &
                        x0,y0,(x(i),y(i),i=1,4)
         call stop_exit(1)
      endif
!
!rjv  xl = (ey + d1)/d2
!rjv  yl = (ex + d1)/d2
      xl = real((ex + d1)/d2)
      yl = real((ey + d1)/d2)
!
991   format(/,2x,' Error 4301: programming error in routine ',a6,/       &
                  '             x and y-axis have been switched ? ')
992   format(/,2x,' Error 4302: point outside parent domain of cell '     &
             /,2x,'             Check [lower < ex < upper]: ',3es15.7,     &
             /,2x,'             Check  point  : (x,y)     : ',2es15.7      &
             /,2x,'             Corner point 1: (x,y)     : ',2es15.7,     &
             /,2x,'             Corner point 2: (x,y)     : ',2es15.7,     &
             /,2x,'             Corner point 3: (x,y)     : ',2es15.7,     &
             /,2x,'             Corner point 4: (x,y)     : ',2es15.7)
!
      if ( timon ) call timstop ( ithndl )
      return
      end subroutine intl2v

subroutine bilin5(xa        ,ya        ,x0        ,y0        ,w         , &
                & ier       )
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2012-2015.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!  $Id: grid_search.f90 4891 2015-03-31 15:35:26Z jeuke_ml $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/engines_gpl/part/packages/utils_f/src/grid_search.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision_part
    implicit none
!
! Global variables
!
    integer, intent(out)           :: ier
    real(sp), intent(in)               :: x0
    real(sp), intent(in)               :: y0
    real(sp), dimension(6), intent(out) :: w
    real(sp), dimension(4), intent(in) :: xa
    real(sp), dimension(4), intent(in) :: ya
!
!
! Local variables
!
    real(sp)                           :: a
    real(sp)                           :: a21
    real(sp)                           :: a22
    real(sp)                           :: a31
    real(sp)                           :: a32
    real(sp)                           :: a41
    real(sp)                           :: a42
    real(sp)                           :: b
    real(sp)                           :: c
    real(sp)                           :: det
    real(sp)                           :: discr
    real(sp)                           :: eta
    real(sp)                           :: x
    real(sp)                           :: x1
    real(sp)                           :: x2
    real(sp)                           :: x3
    real(sp)                           :: x3t
    real(sp)                           :: x4
    real(sp)                           :: xi
    real(sp)                           :: xt
    real(sp)                           :: y
    real(sp)                           :: y1
    real(sp)                           :: y2
    real(sp)                           :: y3
    real(sp)                           :: y3t
    real(sp)                           :: y4
    real(sp)                           :: yt
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    ! Author: H. Petit
    !
    !
    !     read(12,*)x1,y1,f1
    x1 = xa(1)
    y1 = ya(1)
    !     read(12,*)x2,y2,f2
    x2 = xa(2)
    y2 = ya(2)
    !     read(12,*)x3,y3,f3
    x3 = xa(3)
    y3 = ya(3)
    !     read(12,*)x4,y4,f4
    x4 = xa(4)
    y4 = ya(4)
    x = x0
    y = y0
    ! The bilinear interpolation problem is first transformed
    ! to the quadrangle with nodes
    ! (0,0),(1,0),(x3t,y3t),(0,1)
    ! and required location (xt,yt)
    a21 = x2 - x1
    a22 = y2 - y1
    a31 = x3 - x1
    a32 = y3 - y1
    a41 = x4 - x1
    a42 = y4 - y1
    det = a21*a42 - a22*a41
    if (abs(det)<1D-20) then
       ! write (*, *) 'surface is zero'
       ier = 1
       goto 99999
    endif
    x3t = (a42*a31 - a41*a32)/det
    y3t = ( - a22*a31 + a21*a32)/det
    xt = (a42*(x - x1) - a41*(y - y1))/det
    yt = ( - a22*(x - x1) + a21*(y - y1))/det
    if ((x3t<0.0) .or. (y3t<0.0)) then
       ! write (*, *) 'distorted quadrangle'
       ier = 1
       goto 99999
    endif
    if (abs(x3t - 1.0)<1.0D-7) then
       xi = xt
       if (abs(y3t - 1.0)<1.0D-7) then
          eta = yt
       elseif (abs(1.0 + (y3t - 1.0)*xt)<1.0D-6) then
          ! write (*, *) 'extrapolation over too large a distance'
          ier = 1
          goto 99999
       else
          eta = yt/(1.0 + (y3t - 1.0)*xt)
       endif
    elseif (abs(y3t - 1.0)<1.0D-6) then
       eta = yt
       if (abs(1.0 + (x3t - 1.0)*yt)<1.D-6) then
          ! write (*, *) 'extrapolation over too large a distance'
          ier = 1
          goto 99999
       else
          xi = xt/(1.0 + (x3t - 1.0)*yt)
       endif
    else
       a = y3t - 1.0
       b = 1.0 + (x3t - 1.0)*yt - (y3t - 1.0)*xt
       c = -xt
       discr = b*b - 4.0*a*c
       if (discr<1.0D-6) then
          ! write (*, *) 'extrapolation over too large a distance'
          ier = 1
          goto 99999
       endif
       xi = ( - b + sqrt(discr))/(2.0*a)
       eta = ((y3t - 1.0)*(xi - xt) + (x3t - 1.0)*yt)/(x3t - 1.0)
    endif
    w(1) = (1.0 - xi)*(1.0 - eta)
    w(2) = xi*(1.0 - eta)
    w(3) = xi*eta
    w(4) = eta*(1.0 - xi)
    w(5) = xi  ! local weight factor in direction from point 1 to 2
    w(6) = eta ! local weight factor in direction from point 1 to 4
    return
99999 continue
end subroutine bilin5


end module grid_search_mod
