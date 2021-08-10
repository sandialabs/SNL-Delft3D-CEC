subroutine nm_to_nmglob(nm, nmglob, gdp)
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
!  $Id: nm_to_nmglob.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/flow2d3d/packages/kernel/src/general/nm_to_nmglob.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global parameters
!
    integer, intent(in)  :: nm
    integer, intent(out) :: nmglob
!
! Local parameters
!
    integer :: ddb
    integer :: nmaxddb
    integer :: m
    integer :: mglob
    integer :: n
    integer :: nglob
!
!! executable statements -------------------------------------------------------
!
    if (parll) then
        ! this code is based on ddb = gdp%d%ddbound = 1 for parallel simulations
        call nm_to_n_and_m(nm, n, m, gdp)
        nglob = gdp%griddim%nfg + n - 1
        mglob = gdp%griddim%mfg + m - 1
        nmglob = nglob + (gdp%griddim%nmaxgl + 2) * mglob + 1
    else
        nmglob = nm
    endif
end subroutine nm_to_nmglob
