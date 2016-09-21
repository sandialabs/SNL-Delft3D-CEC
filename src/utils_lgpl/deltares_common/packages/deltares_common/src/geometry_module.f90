module geometry_module
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2015.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.            
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!  $Id: geometry_module.f90 4612 2015-01-21 08:48:09Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/deltares_common/packages/deltares_common/src/geometry_module.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: - Various geometrical routines
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------

   implicit none

   private

   !
   ! functions and subroutines
   !
   public :: clockwise

   interface clockwise
      module procedure clockwise_sp
      module procedure clockwise_hp
   end interface clockwise

   contains

      !> Checks orientation of a polygon in single precision.
      function clockwise_sp(x,y) result(cw)
          use precision
      
          implicit none
          
          real(sp), dimension(:), intent(in) :: x   !< x-coordinates
          real(sp), dimension(:), intent(in) :: y   !< y-coordinates
          logical                            :: cw  !< true if clockwise, false if not
          
          integer                             :: n        !< number of points in polygon
          real(hp), dimension(:), allocatable :: xhp      !< temporary double precision x-coordinates
          real(hp), dimension(:), allocatable :: yhp      !< temporary double precision y-coordinates
          
          n = size(x)
          allocate(xhp(n),yhp(n))
          xhp = real(x,hp)
          yhp = real(y,hp)
          cw = clockwise_hp(xhp,yhp)
          deallocate(xhp,yhp)
      end function clockwise_sp

      !> Checks orientation of a polygon in high precision.
      function clockwise_hp(x,y) result(cw)
          use precision
      
          implicit none
          
          real(hp), dimension(:), intent(in) :: x   !< x-coordinates
          real(hp), dimension(:), intent(in) :: y   !< y-coordinates
          logical                            :: cw  !< true if clockwise, false if not
          
          integer :: i        !< loop variable
          integer :: i0       !< index of lowest left most point
          integer :: in       !< index of next point (not equal to x0,y0)
          integer :: ip       !< index of previous point (not equal to x0,y0)
          integer :: n        !< number of points in polygon
          real(hp) :: an      !< angle of next point compared to horizontal x-axis
          real(hp) :: ap      !< angle of previous point compared to horizontal x-axis
          real(hp) :: x0      !< x-coordinate of point i0
          real(hp) :: y0      !< y-coordinate of point i0
          
          !
          ! select lowest left most point
          !
          n = size(x)
          x0 = x(1)
          y0 = y(1)
          i0 = 1
          do i = 2, n
              if ( x(i)<x0 ) then
                  x0 = x(i)
                  y0 = y(i)
                  i0 = i
              elseif (x(i)==x0 .and. y(i)<y0) then
                  y0 = y(i)
                  i0 = i
              endif
          enddo
          !
          ! find point before
          ! note that this will give an infinite loop if n==1 or more generally if all(x==x0 and y==y0)
          !
          ip = i0
          do while (x(ip)==x0 .and. y(ip)==y0)
              if (ip==1) then
                  ip = n
              else
                  ip = ip-1
              endif
          enddo
          !
          ! find point after
          !
          in = i0
          do while (x(in)==x0 .and. y(in)==y0)
              if (in==n) then
                  in = 1
              else
                  in = in+1
              endif
          enddo
          !
          ! if "point after" lies above "point before" the orientation is clockwise ...
          !
          an = atan2(y(in)-y0,x(in)-x0)
          ap = atan2(y(ip)-y0,x(ip)-x0)
          cw = an>ap
      end function clockwise_hp
end module geometry_module
