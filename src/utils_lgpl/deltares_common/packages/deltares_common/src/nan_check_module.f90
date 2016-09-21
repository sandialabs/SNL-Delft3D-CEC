module nan_check_module
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
!  $Id: nan_check_module.f90 4612 2015-01-21 08:48:09Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/deltares_common/packages/deltares_common/src/nan_check_module.f90 $
!!--module description----------------------------------------------------------
!
! This module provides a subroutine nan_check for 1D, 2D, 3D and 4D,
! in single, double and flexible precision.
!
!!--module declarations---------------------------------------------------------
implicit none
interface nan_check
  module procedure nan_check_1D_sp
  module procedure nan_check_2D_sp
  module procedure nan_check_3D_sp
  module procedure nan_check_4D_sp
  module procedure nan_check_1D_dp
  module procedure nan_check_2D_dp
  module procedure nan_check_3D_dp
  module procedure nan_check_4D_dp
end interface

contains

logical function nan_check_1D_sp(field, fieldnam, lundia) result (retval)
!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 1D single precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Call variables
    !
    real(kind=4), dimension(:), intent(in) :: field
    character*(*), intent(in)              :: fieldnam
    integer, intent(in)                    :: lundia
    !
    ! Local variables
    !
    integer                                :: i
    character*(256)                        :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    do i = lbound(field,1), ubound(field,1)
       if (isnan(field(i))) then
          write(message,*) 'NaN found in ', fieldnam, &
               ' at (i) = (', i, ')'
          write(lundia,*) 'U021', trim(message)
          retval = .false.
          return
       endif
    enddo
end function nan_check_1D_sp

logical function nan_check_2D_sp(field, fieldnam, lundia) result (retval)
!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 2D single precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Call variables
    !
    real(kind=4), dimension(:,:), intent(in) :: field
    character*(*), intent(in)                :: fieldnam
    integer, intent(in)                      :: lundia
    !
    ! Local variables
    !
    integer                                  :: m, n
    character*(256)                          :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    do m = lbound(field,2), ubound(field,2)
       do n = lbound(field,1), ubound(field,1)
          if (isnan(field(n,m))) then
             write(message,*) 'NaN found in ', fieldnam, &
              ' at (n,m) = (', n, ',' , m, ')'
             write(lundia,*) 'U021', trim(message)
             retval = .false.
             return
          endif
       enddo
    enddo
end function nan_check_2D_sp

logical function nan_check_3D_sp(field, fieldnam, lundia) result (retval)
!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 3D single precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Call variables
    !
    real(kind=4), dimension(:,:,:), intent(in) :: field
    character*(*), intent(in)                  :: fieldnam
    integer, intent(in)                        :: lundia
    !
    ! Local variables
    !
    integer                                    :: k, m, n
    character*(256)                            :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    do k = lbound(field,3), ubound(field,3)
       do m = lbound(field,2), ubound(field,2)
          do n = lbound(field,1), ubound(field,1)
            if (isnan(field(n,m,k))) then
              write(message,*) 'NaN found in ', fieldnam, &
                  ' at (n,m,k) = (', n, ',', m, ',', k, ')'
              write(lundia,*) 'U021', trim(message)
              retval = .false.
              return
            endif
          enddo
        enddo
     enddo
    end function nan_check_3D_sp

logical function nan_check_4D_sp(field, fieldnam, lundia) result (retval)
!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 4D single precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Call variables
    !
    real(kind=4), dimension(:,:,:,:), intent(in) :: field
    character*(*), intent(in)                    :: fieldnam
    integer, intent(in)                          :: lundia
    !
    ! Local variables
    !
    integer                                      :: k, l, m, n
    character*(256)                              :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    do l = lbound(field,4), ubound(field,4)
       do k = lbound(field,3), ubound(field,3)
          do m = lbound(field,2), ubound(field,2)
             do n = lbound(field,1), ubound(field,1)
                if (isnan(field(n,m,k,l))) then
                   write(message,*) 'NaN found in ', fieldnam, &
                      ' at (n,m,k,l) = (', n, ',', m, ',', k, ',', l, ')'
                   write(lundia,*) 'U021', trim(message)
                   retval = .false.
                   return
                endif
             enddo
          enddo
       enddo
    enddo
    end function nan_check_4D_sp

logical function nan_check_1D_dp(field, fieldnam, lundia) result (retval)
!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 1D double precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Call variables
    !
    real(kind=8), dimension(:), intent(in) :: field
    character*(*), intent(in)              :: fieldnam
    integer, intent(in)                    :: lundia
    !
    ! Local variables
    !
    integer                                :: i
    character*(256)                        :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    do i = lbound(field,1), ubound(field,1)
       if (isnan(field(i))) then
          write(message,*) 'NaN found in ', fieldnam, &
               ' at (i) = (', i, ')'
          write(lundia,*) 'U021', trim(message)
          retval = .false.
          return
       endif
    enddo
end function nan_check_1D_dp

logical function nan_check_2D_dp(field, fieldnam, lundia) result (retval)
!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 2D double precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Call variables
    !
    real(kind=8), dimension(:,:), intent(in) :: field
    character*(*), intent(in)                :: fieldnam
    integer, intent(in)                      :: lundia
    !
    ! Local variables
    !
    integer                                  :: m, n
    character*(256)                          :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    do m = lbound(field,2), ubound(field,2)
       do n = lbound(field,1), ubound(field,1)
          if (isnan(field(n,m))) then
             write(message,*) 'NaN found in ', fieldnam, &
              ' at (n,m) = (', n, ',' , m, ')'
             write(lundia,*) 'U021', trim(message)
             retval = .false.
             return
          endif
       enddo
    enddo
end function nan_check_2D_dp

logical function nan_check_3D_dp(field, fieldnam, lundia) result (retval)
!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 3D double precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Call variables
    !
    real(kind=8), dimension(:,:,:), intent(in) :: field
    character*(*), intent(in)                  :: fieldnam
    integer, intent(in)                        :: lundia
    !
    ! Local variables
    !
    integer                                    :: k, m, n
    character*(256)                            :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    do k = lbound(field,3), ubound(field,3)
       do m = lbound(field,2), ubound(field,2)
          do n = lbound(field,1), ubound(field,1)
            if (isnan(field(n,m,k))) then
              write(message,*) 'NaN found in ', fieldnam, &
                  ' at (n,m,k) = (', n, ',', m, ',', k, ')'
              write(lundia,*) 'U021', trim(message)
              retval = .false.
              return
            endif
          enddo
        enddo
     enddo
    end function nan_check_3D_dp

logical function nan_check_4D_dp(field, fieldnam, lundia) result (retval)
!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 4D double precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Call variables
    !
    real(kind=8), dimension(:,:,:,:), intent(in) :: field
    character*(*), intent(in)                    :: fieldnam
    integer, intent(in)                          :: lundia
    !
    ! Local variables
    !
    integer                                      :: k, l, m, n
    character*(256)                              :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    do l = lbound(field,4), ubound(field,4)
       do k = lbound(field,3), ubound(field,3)
          do m = lbound(field,2), ubound(field,2)
             do n = lbound(field,1), ubound(field,1)
                if (isnan(field(n,m,k,l))) then
                   write(message,*) 'NaN found in ', fieldnam, &
                      ' at (n,m,k,l) = (', n, ',', m, ',', k, ',', l, ')'
                   write(lundia,*) 'U021', trim(message)
                   retval = .false.
                   return
                endif
             enddo
          enddo
       enddo
    enddo
    end function nan_check_4D_dp

end module nan_check_module
