subroutine nm_to_n_and_m(nm, n, m, gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2020.                                
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
!  $Id: nm_to_n_and_m.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/flow2d3d/packages/kernel/src/general/nm_to_n_and_m.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global parameters
!
    integer, intent(in)  :: nm
    integer, intent(out) :: m
    integer, intent(out) :: n
!
! Local parameters
!
    integer :: ddb
    integer :: nmaxddb
!
!! executable statements -------------------------------------------------------
!
    ddb     = gdp%d%ddbound
    nmaxddb = gdp%d%nmax + 2*gdp%d%ddbound
    !
    ! Calculation of m:
    ! This used to be:
    ! m = int (nm / nmaxddb ) + 1 - ddb
    ! But that goes wrong for negative nm and nm is a multiple of nmaxddb
    !
    m = floor(real(nm-1)/real(nmaxddb)) + 1 - ddb
    n = nm - nmaxddb * (m - 1 + ddb ) - ddb
end subroutine nm_to_n_and_m
