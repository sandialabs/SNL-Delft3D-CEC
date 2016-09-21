module time_module
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
   !  $Id: time_module.f90 5327 2015-08-10 06:26:50Z dam_ar $
   !  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/deltares_common/packages/deltares_common/src/time_module.f90 $
   !!--description-----------------------------------------------------------------
   !
   !    Function: - Various time processing routines
   !
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------

   implicit none

   private

   public :: time_module_info
   public :: datetime2sec
   public :: sec2ddhhmmss
   public :: ymd2jul
   public :: jul2ymd
   public :: datetime_to_string

   interface ymd2jul
      module procedure GregorianDateToJulianDateNumber
      module procedure GregorianYearMonthDayToJulianDateNumber
   end interface ymd2jul

   interface jul2ymd
      module procedure JulianDateNumberToGregorianDate
      module procedure JulianDateNumberToGregorianYearMonthDay
   end interface jul2ymd

   contains
      
      ! ------------------------------------------------------------------------------
      !   Subroutine: time_module_info
      !   Purpose:    Add info about this time module to the messages stack
      !   Summary:    Add id string and URL
      !   Arguments:
      !   messages    Stack of messages to add the info to
      ! ------------------------------------------------------------------------------
      subroutine time_module_info(messages)
          use message_module
          !
          ! Call variables
          !
          type(message_stack), pointer :: messages
          !
          !! executable statements ---------------------------------------------------
          !
          call addmessage(messages,'$Id: time_module.f90 5327 2015-08-10 06:26:50Z dam_ar $')
          call addmessage(messages,'$URL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/deltares_common/packages/deltares_common/src/time_module.f90 $')
      end subroutine time_module_info
      
      
      
      ! ------------------------------------------------------------------------------
      !   Subroutine: datetime2sec
      !   Purpose:    Convert a 6 integer datetime vector to a number of seconds
      !   Arguments:
      !   datetime    Integer array of length 6 containing date yr,mo,dy and time hr,mn,sc
      !   refdatetime Optional integer array of length 6 containing reference date yr,mo,dy and time hr,mn,sc
      !   sec         If refdatetime is specified: time difference in seconds
      !               Otherwise: time in seconds since julday = 0 (only to be used for time steps)
      ! ------------------------------------------------------------------------------
      function datetime2sec(datetime, refdatetime) result (sec)
          !
          ! Call variables
          !
          integer, dimension(6)           , intent(in)    :: datetime
          integer, dimension(6) , optional, intent(in)    :: refdatetime
          integer                                         :: sec
          !
          ! Local variables
          !
          integer :: jd    ! Julian date
          integer :: refjd ! Julian refernce date
          !
          !! executable statements ---------------------------------------------------
          !
          jd    = ymd2jul(datetime(1), datetime(2), datetime(3))
          !
          if (present(refdatetime)) then
             refjd = ymd2jul(refdatetime(1), refdatetime(2), refdatetime(3))
             ! difference in days
             sec   = jd - refjd
             ! difference in hours
             sec   = sec*24 + datetime(4) - refdatetime(4)
             ! difference in minutes
             sec   = sec*60 + datetime(5) - refdatetime(5)
             ! difference in seconds
             sec   = sec*60 + datetime(6) - refdatetime(6)
          else
             ! time in hours
             sec   = jd*24 + datetime(4)
             ! time in minutes
             sec   = sec*60 + datetime(5)
             ! time in seconds
             sec   = sec*60 + datetime(6)
          endif
      end function datetime2sec
      
      
      
      ! ------------------------------------------------------------------------------
      !   Subroutine: sec2ddhhmmss
      !   Purpose:    Convert a number of seconds to ddhhmmss integer
      !   Arguments:
      !   sec         Number of seconds
      !   ddhhmmss    Integer with days, hours, minutes and seconds formatted as ddhhmmss 
      ! ------------------------------------------------------------------------------
      function sec2ddhhmmss(sec) result (ddhhmmss)
          !
          ! Call variables
          !
          integer                         , intent(in)    :: sec
          integer                                         :: ddhhmmss
          !
          ! Local variables
          !
          integer :: dd ! days
          integer :: hh ! hours
          integer :: mm ! minutes
          integer :: ss ! seconds
          !
          !! executable statements ---------------------------------------------------
          !
          ! time in seconds
          dd = sec
          ss = mod(dd,60)
          ! time in minutes
          dd = (dd - ss)/60
          mm = mod(dd,60)
          ! time in hours
          dd = (dd - mm)/60
          hh = mod(dd,24)
          ! time in days
          dd = (dd - hh)/24
          !
          ddhhmmss = ss + 100*mm + 10000*hh + 1000000*dd
      end function sec2ddhhmmss

      ! =======================================================================
      
      !> Calculates the Julian Date Number from a Gregorian calender date.
      !! Returns 0 in case of failure.
      function GregorianDateToJulianDateNumber(yyyymmdd) result(jdn)
         integer             :: jdn      !< calculated Julian Date Number
         integer, intent(in) :: yyyymmdd !< Gregorian calender date
         !
         integer :: year  !< helper variable
         integer :: month !< helper variable
         integer :: day   !< helper variable
         !
         year = yyyymmdd/10000
         month = yyyymmdd/100 - year*100
         day = yyyymmdd - month*100 - year*10000
         jdn = GregorianYearMonthDayToJulianDateNumber(year, month, day)
      end function GregorianDateToJulianDateNumber
      
      ! =======================================================================
      
      !> Calculates the Julian Date Number from a Gregorian year, month and day.
      !! Returns 0 in case of failure.
      function GregorianYearMonthDayToJulianDateNumber(year, month, day) result(jdn)
         integer :: jdn               !< calculated Julian Date Number
         integer, intent(in) :: year  !< Gregorian year
         integer, intent(in) :: month !< Gregorian month
         integer, intent(in) :: day   !< Gregorian day
         !
         integer :: month1 !< helper variable
         integer :: y      !< helper variable
         integer :: m      !< helper variable
         integer :: d      !< helper variable
         !
         ! Calculate Julian day assuming the given month is correct
         !
         month1 = (month - 14)/12
         jdn = day - 32075 + 1461*(year + 4800 + month1)/4 &
                           + 367*(month - 2 - month1*12)/12 &
                           - 3*((year + 4900 + month1)/100)/4
         !
         ! Calculate backwards to test if the assumption is correct
         !
         call jul2ymd(jdn, y, m, d)
         !
         ! Test if calculation is correct
         !
         if ((y /= year) .or. (m /= month) .or. (d /= day)) then
            jdn = 0
         endif
      end function GregorianYearMonthDayToJulianDateNumber
      
      ! =======================================================================
      
      !> Calculates the Gregorian calender date from a Julian Date Number.
      subroutine JulianDateNumberToGregorianDate(jdn, yyyymmdd)
         integer, intent(in) :: jdn       !< Julian Date Number
         integer, intent(out) :: yyyymmdd !< calculated Gregorian calender date
         !
         integer :: year  !< helper variable
         integer :: month !< helper variable
         integer :: day   !< helper variable
         !
         call JulianDateNumberToGregorianYearMonthDay(jdn, year, month, day)
         yyyymmdd = year*10000 + month*100 + day
      end subroutine JulianDateNumberToGregorianDate
      
      ! =======================================================================
      
      !> Calculates the Gregorian year, month, day triplet from a Julian Date Number.
      subroutine JulianDateNumberToGregorianYearMonthDay(jdn, year, month, day)
         integer, intent(in)  :: jdn   !< Julian Date Number
         integer, intent(out) :: year  !< calculated Gregorian year
         integer, intent(out) :: month !< calculated Gregorian month
         integer, intent(out) :: day   !< calculated Gregorian day
         !
         integer :: j !< helper variable
         integer :: l !< helper variable
         integer :: m !< helper variable
         integer :: n !< helper variable
         !
         l      = jdn + 68569
         n      = 4 * l / 146097
         l      = l - (146097*n + 3)/4
         j      = 4000 * (l + 1) / 1461001
         l      = l - 1461*j/4 + 31
         m      = 80 * l / 2447
         day    = l - 2447*m/80
         l      = m / 11
         month  = m + 2 - 12*l
         year   = 100*(n-49) + j + l
      end subroutine JulianDateNumberToGregorianYearMonthDay

      ! =======================================================================
      
      !> Parses an UDUnit-conventions datetime unit string.
      !! TODO: replace this by calling C-API from UDUnits(-2).
      function parse_ud_timeunit(timeunitstr, iunit, iyear, imonth, iday, ihour, imin, isec) result(ierr)
         character(len=*), intent(in)  :: timeunitstr !< Time unit by UDUnits conventions, e.g. 'seconds since 2012-01-01 00:00:00.0 +0000'.
         integer,          intent(out) :: iunit       !< Unit in seconds, i.e. 'hours since..' has iunit=3600.
         integer,          intent(out) :: iyear       !< Year in reference datetime.
         integer,          intent(out) :: imonth      !< Month in reference datetime.
         integer,          intent(out) :: iday        !< Day in reference datetime.
         integer,          intent(out) :: ihour       !< Hour in reference datetime.
         integer,          intent(out) :: imin        !< Minute in reference datetime.
         integer,          intent(out) :: isec        !< Seconds in reference datetime.
         integer                       :: ierr        !< Error status, only 0 when successful.
         !
         integer          :: i
         integer          :: n
         integer          :: ifound
         integer          :: iostat
         character(len=7) :: unitstr
         !
         ierr    = 0
         unitstr = ' '
         !
         n = len_trim(timeunitstr)
         ifound = 0
         do i = 1,n
            if (timeunitstr(i:i) == ' ') then ! First space found
               if (timeunitstr(i+1:min(n, i+5)) == 'since') then
                  unitstr = timeunitstr(1:i-1)
                  ifound = 1
               else
                  ierr = 1
               end if
               exit ! Found or error, look no further.
            end if
         end do
         !
         if (ifound == 1) then
            select case(trim(unitstr))
            case('seconds')
               iunit = 1
            case('minutes')
               iunit = 60
            case('hours')
               iunit = 3600
            case('days')
               iunit = 86400
            case('weeks')
               iunit = 604800
            case default
               iunit = -1
            end select
            !
            read (timeunitstr(i+7:n), '(I4,1H,I2,1H,I2,1H,I2,1H,I2,1H,I2)', iostat = iostat) iyear, imonth, iday, ihour, imin, isec
         end if
      end function parse_ud_timeunit

      !> Creates a string representation of a date time, in the ISO 8601 format.
      !! Example: 2015-08-07T18:30:27Z
      !! 2015-08-07T18:30:27+00:00
      !! Performs no check on validity of input numbers!
      function datetime_to_string(iyear, imonth, iday, ihour, imin, isec, ierr) result(datetimestr)
         integer,           intent(in)  :: iyear, imonth, iday
         integer, optional, intent(in)  :: ihour, imin, isec !< Time is optional, will be printed as 00:00:00 if omitted.
         integer, optional, intent(out) :: ierr !< Error status, 0 if success, nonzero in case of format error.

         character(len=20) :: datetimestr !< The resulting date time string. Considering using trim() on it.

         integer :: ihour_, imin_, isec_, ierr_
         if (.not. present(ihour)) then
            ihour_ = 0
         else
            ihour_ = ihour
         end if
         if (.not. present(imin)) then
            imin_ = 0
         else
            imin_ = imin
         end if
         if (.not. present(isec)) then
            isec_ = 0
         else
            isec_ = isec
         end if

         write (datetimestr, '(i4,"-",i2.2,"-",i2.2,"T",i2.2,":",i2.2,":",i2.2,"Z")', iostat=ierr_) &
                               iyear, imonth, iday, ihour_, imin_, isec_

         if (present(ierr)) then
            ierr = ierr_
         end if
      end function datetime_to_string

end module time_module
