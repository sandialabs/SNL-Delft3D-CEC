module precision_basics
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2020.                                
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
!  $Id: precision_basics.F90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_lgpl/deltares_common/packages/deltares_common/src/precision_basics.F90 $
!!--description-----------------------------------------------------------------
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
implicit none
!
! parameters, used in conversions: sp=single precision, hp=high (double) precision
!
integer, parameter :: sp=kind(1.0e00)
integer, parameter :: hp=kind(1.0d00)
!
! double precision integers:
!
integer, parameter :: long = SELECTED_INT_KIND(16)
!
! interfaces
!
  interface comparereal
     module procedure comparerealdouble
     module procedure comparerealsingle
  end interface
! 

contains

function comparerealdouble(val1, val2, eps)
!!--description-----------------------------------------------------------------
!
! Compares two double precision numbers
! Allow users to define the value of eps. If not, eps equals to the default machine eps
!
! Return value: -1 if val1 < val2
!                0 if val1 = val2
!               +1 if val1 > val2
!
!!--pseudo code and references--------------------------------------------------
!
! The functionality in this subroutine is copied from subroutine Ifdbl,
! written by Jaap Zeekant.
!
! eps must be machine precision dependent.
! eps may not be given by the user! See what happens when
! val1 = -666.0, val2 = -999.0, eps = 0.5
!
!!--declarations----------------------------------------------------------------
    implicit none
!
! Return value
!
integer :: comparerealdouble
!
! Global variables
!
real(hp), intent(in)           :: val1
real(hp), intent(in)           :: val2
real(hp), optional, intent(in) :: eps
!
! Local variables
!
real(hp) :: eps0
real(hp) :: value
!
!! executable statements -------------------------------------------------------
!
if (present(eps)) then
    eps0 = eps
else 
    eps0 = 2.0_hp * epsilon(val1)
endif
!
if (abs(val1)<1.0_hp .or. abs(val2)<1.0_hp) then
   value = val1 - val2
else
   value = val1/val2 - 1.0_hp
endif
!
if (abs(value)<eps0) then
   comparerealdouble = 0
elseif (val1<val2) then
   comparerealdouble = -1
else
   comparerealdouble = 1
endif
end function comparerealdouble



function comparerealsingle(val1, val2,eps)
!!--description-----------------------------------------------------------------
!
! REMARK: THE NAME OF THIS FUNCTION IS WRONG!
!         The name should be comparefp
!
! Compares two real numbers of type fp
! Allow users to define the value of eps. If not, eps equals to the default machine eps
!
! Return value: -1 if val1 < val2
!                0 if val1 = val2
!               +1 if val1 > val2
!
!!--pseudo code and references--------------------------------------------------
!
! The functionality in this subroutine is copied from subroutine Ifflt,
! written by Jaap Zeekant.
!
! eps must be machine precision dependent.
! eps may not be given by the user! See what happens when
! val1 = -666.0, val2 = -999.0, eps = 0.5
!
!!--declarations----------------------------------------------------------------
implicit none
!
! Return value
!
integer :: comparerealsingle
!
! Global variables
!
real(sp), intent(in)           :: val1
real(sp), intent(in)           :: val2
real(sp), optional, intent(in) :: eps
!
! Local variables
!
real(sp) :: eps0
real(sp) :: value
!
!! executable statements -------------------------------------------------------
!
!  
if (present(eps)) then
    eps0 = eps
else
    eps0 = 2.0_sp * epsilon(val1)
endif
!
if (abs(val1)<1.0_sp .or. abs(val2)<1.0_sp) then
   value = val1 - val2
else
   value = val1/val2 - 1.0_sp
endif
!
if (abs(value)<eps0) then
   comparerealsingle = 0
elseif (val1<val2) then
   comparerealsingle = -1
else
   comparerealsingle = 1
endif
end function comparerealsingle


end module precision_basics
