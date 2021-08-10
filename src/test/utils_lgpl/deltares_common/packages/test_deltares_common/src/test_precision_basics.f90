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

module test_precision_basics
    use precision_basics
    use ftnunit

    implicit none

    private
    public :: tests_precision_basics

contains
subroutine tests_precision_basics
    call test( test_precision_sp, 'Compare single-precision reals' )
    call test( test_precision_dp, 'Compare double-precision reals' )
end subroutine tests_precision_basics

subroutine test_precision_sp
    integer, parameter :: wp = kind(1.0)

include "test_precision_body.f90"
end subroutine test_precision_sp

subroutine test_precision_dp
    integer, parameter :: wp = kind(1.0d0)

include "test_precision_body.f90"
end subroutine test_precision_dp

end module test_precision_basics
