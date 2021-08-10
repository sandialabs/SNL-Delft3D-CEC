module time_module_tests
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
   !  $Id: time_module_tests.f90 65778 2020-01-14 14:07:42Z mourits $
   !  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_lgpl/deltares_common/packages/deltares_common/src/time_module_tests.f90 $
   !!--description-----------------------------------------------------------------
   !
   !    Function: - Tests for various time processing routines
   !
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------
   use precision_basics, only : hp, comparereal
   use time_module
   implicit none

   private

   public :: testconversion1, testconversion2

   contains

      subroutine testConversion1(success, errMessage)
         logical,          intent(out) :: success
         character(len=*), intent(out) :: errMessage

         integer :: jdn1, jdn2, jdn3, jdn4, yyyymmdd, yyyymmdd2, istat
         real(kind=hp) :: mjd, mjd2

         success = .false.

         jdn1 = CalendarYearMonthDayToJulianDateNumber(1, 1, 1)
         jdn2 = CalendarYearMonthDayToJulianDateNumber(1582, 10, 4)
         jdn3 = CalendarYearMonthDayToJulianDateNumber(1582, 10, 15)
         jdn4 = CalendarYearMonthDayToJulianDateNumber(2001, 1, 1)

         if (jdn1 /= 1721424) then
            errMessage = 'error for 1-1-1'
            return
         endif
         if (jdn2 /= 2299160) then
            errMessage = 'error for 4-10-1582'
            return
         endif
         if (jdn3 /= 2299161) then
            errMessage = 'error for 15-10-1582'
            return
         endif
         if (jdn4 /= 2451911) then
            errMessage = 'error for 1-1-2001'
            return
         endif

         mjd = 55833.5
         istat = mjd2date(mjd, yyyymmdd)
         mjd2 = date2mjd(yyyymmdd)
         if (mjd /= mjd2) then
            errMessage = 'error for mjd=55833.5'
            return
         endif

         yyyymmdd = 15821015
         mjd = date2mjd(yyyymmdd)
         istat = mjd2date(mjd, yyyymmdd2)
         if (yyyymmdd /= yyyymmdd2) then
            errMessage = 'error for yyyymmdd = 15821015'
            return
         endif

         yyyymmdd = 15821004
         mjd2 = date2mjd(yyyymmdd)
         istat = mjd2date(mjd2, yyyymmdd2)
         if (istat == 0) then
            errMessage = 'mjd2date failed for yyyymmdd = 15821004'
            return
         endif
         if (yyyymmdd /= yyyymmdd2) then
            errMessage = 'error for yyyymmdd = 15821004'
            return
         endif
         if (abs(mjd - mjd2 - 1d0) > 1d-4) then
            errMessage = 'error for jump 15821004 -> 15821015'
            return
         endif

         errMessage = ' '
         success = .true.

      end subroutine testConversion1

      subroutine testConversion2(success, errMessage)
         logical,          intent(out) :: success
         character(len=*), intent(out) :: errMessage

         integer :: yyyymmdd, yyyymmdd2, istat
         integer :: iyear , imonth, iday, ihour, imin, isec
         real(kind=hp) :: jdn1, jdn2, jdn3, jdn4, djul, dsec
         real(kind=hp), parameter :: tol = 1d-9

         success = .false.

         jdn1 = julian(00010101, 0)
         jdn2 = julian(15821004, 0)
         jdn3 = julian(15821015, 0)
         jdn4 = julian(20010101, 0)

         if (comparereal(jdn1, -678577.0_hp, tol) /= 0) then
            errMessage = 'error for 1-1-1'
            return
         endif
         if (comparereal(jdn2, -100841.0_hp, tol) /= 0) then
            errMessage = 'error for 4-10-1582'
            return
         endif
         if (comparereal(jdn3, -100840.0_hp, tol) /= 0) then
            errMessage = 'error for 15-10-1582'
            return
         endif
         if (comparereal(jdn4, 51910.0_hp, tol) /= 0) then
            errMessage = 'error for 1-1-2001'
            return
         endif

         success = .true.

      end subroutine testConversion2

end module time_module_tests
