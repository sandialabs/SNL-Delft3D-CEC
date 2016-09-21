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

      subroutine boombounce( xold, yold, xnew, ynew, nboom, xboom, &
                                    yboom, xcatch, ycatch, catch, xbounce, ybounce, bounce )
!
!     Does a boom catch a particle in it's path from xold, yold to xnew, ynew?
!     And if it does, does it bounce without crossing another part of the boom?

      use precision_part      ! single/double precision
      use timers

      implicit none

      real    ( sp ) :: xold, yold, xnew, ynew
      integer ( 4  ) :: nboom
      real    ( sp ) :: xboom(nboom), yboom(nboom)
      real    ( sp ) :: xcatch, ycatch
      logical        :: catch
      real    ( sp ) :: xbounce, ybounce
      logical        :: bounce

!     local variables
      real    ( dp ) :: xoldd, yoldd, xnewd, ynewd
      real    ( dp ) :: xboomd(nboom), yboomd(nboom)
      real    ( dp ) :: a1, b1, c1, rl1, rlnear
      real    ( dp ) :: a2(nboom), b2(nboom), c2(nboom), rl2(nboom)
      real    ( dp ) :: xcross(nboom), ycross(nboom)
      real    ( dp ) :: rl, xn, yn, xb, yb
      integer ( 4  ) :: iboom
      integer ( 4  ) :: inear
      real    ( dp ) :: det
      logical        :: boomgap(nboom)

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "lines_cross", ithndl )

      catch = .false.
      xcatch = 999.999e0
      ycatch = 999.999e0
      bounce = .false.
      xbounce = 999.999e0
      ybounce = 999.999e0
      inear = 0
      rlnear = 1.1D0
      boomgap = .false.

      xoldd = dble(xold)
      yoldd = dble(yold)
      xnewd = dble(xnew)
      ynewd = dble(ynew)
      xboomd = dble(xboom)
      yboomd = dble(yboom)
         
      if ( xold .ne. xnew .or. yold .ne. ynew ) then
         a1 = ynewd - yoldd
         b1 = xoldd - xnewd
         c1 = xnewd * yoldd - xoldd * ynewd

         do iboom = 1, (nboom - 1)
            boomgap(iboom) = (xboom(iboom) .eq. 999.999e0 .and. xboom(iboom) .eq. 999.999e0) .or. &
                          (xboom(iboom + 1) .eq. 999.999e0 .and. xboom(iboom + 1) .eq. 999.999e0)
            if (.not. boomgap(iboom)) then
               if (xboomd(iboom) .ne. xboomd(iboom+1) .or. yboomd(iboom) .ne. yboomd(iboom+1) ) then
                  a2(iboom) = yboomd(iboom+1) - yboomd(iboom)
                  b2(iboom) = xboomd(iboom) - xboomd(iboom+1)
                  c2(iboom) = xboomd(iboom+1) * yboomd(iboom) - xboomd(iboom) * yboomd(iboom+1)

                  det =  a1 * b2(iboom) - b1 * a2(iboom)

                  if ( det .ne. 0.0D+00 ) then
                     xcross(iboom) = (b1 / det) * c2(iboom) - (b2(iboom) / det) * c1
                     ycross(iboom) = (a2(iboom) / det) * c1 - (a1 / det) * c2(iboom)

                     if (xnew .ne. xold) then 
                        rl1 = (xcross(iboom) - xoldd) / (xnewd - xoldd)
                     else
                        rl1 = (ycross(iboom) - yoldd) / (ynewd - yoldd)
                     end if
                     if (xboom(iboom+1) .ne. xboom(iboom)) then
                        rl2(iboom) = (xcross(iboom) - xboom(iboom)) / (xboom(iboom+1) - xboom(iboom))
                     else
                        rl2(iboom) = (ycross(iboom) - yboom(iboom)) / (yboom(iboom+1) - yboom(iboom))
                     end if
                     if ( (rl1 .ge. 0.0D0 .and. rl1 .le. 1.0D0) .and. (rl2(iboom) .ge. 0.0D0 .and. rl2(iboom) .le. 1.0D0)) then
                        if (rl2(iboom) .lt. rlnear) then
                           catch = .true.
                           rlnear = rl2(iboom)
                           inear = iboom
                        end if
                     end if
                  end if
               end if
            end if
         end do
         if (catch) then
            xcatch = sngl(xcross(inear))
            ycatch = sngl(ycross(inear))
!           bounce without crossing boom?
            rl  = ((xboomd(inear) - xnewd) * b2(inear) + (ynewd - yboomd(inear)) * a2(inear)) / (b2(inear)**2 + a2(inear)**2)
            xn = xboomd(inear) - rl * b2(inear)
            yn = yboomd(inear) + rl * a2(inear)
            xb = xn - (xnewd - xn)
            yb = yn - (ynewd - yn)
            bounce = .true.

            a1 = yb - yoldd
            b1 = xoldd - xb
            c1 = xb * yoldd - xoldd * yb

            do iboom = 1, (nboom - 1)
               if (bounce .and. (.not.boomgap(iboom))) then
                  det =  a1 * b2(iboom) - b1 * a2(iboom)
                  if ( det .ne. 0.0D+00 ) then
                     xcross(iboom) = (b1 / det) * c2(iboom) - (b2(iboom) / det) * c1
                     ycross(iboom) = (a2(iboom) / det) * c1 - (a1 / det) * c2(iboom)

                     if (xb .ne. xold) then 
                        rl1 = (xcross(iboom) - xoldd) / (xb - xoldd)
                     else
                        rl1 = (ycross(iboom) - yoldd) / (yb - yoldd)
                     end if
                     if (xboom(iboom+1) .ne. xboom(iboom)) then
                        rl2(iboom) = (xcross(iboom) - xboom(iboom)) / (xboom(iboom+1) - xboom(iboom))
                     else
                        rl2(iboom) = (ycross(iboom) - yboom(iboom)) / (yboom(iboom+1) - yboom(iboom))
                     end if
                     if ( (rl1 .ge. 0.0D0 .and. rl1 .le. 1.0D0) .and. (rl2(iboom) .ge. 0.0D0 .and. rl2(iboom) .le. 1.0D0)) then
                        bounce = .false.
                     end if
                  end if
               end if
            end do
            xbounce = sngl(xb)
            ybounce = sngl(yb)
         end if
      end if
99999 if ( timon ) call timstop ( ithndl )
      return
      end subroutine boombounce      
	  