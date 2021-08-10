module time_module
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
   !  $Id: time_module.f90 65778 2020-01-14 14:07:42Z mourits $
   !  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_lgpl/deltares_common/packages/deltares_common/src/time_module.f90 $
   !!--description-----------------------------------------------------------------
   !
   !    Function: - Various time processing routines
   !
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------
   use precision_basics, only : hp
   implicit none

   private

   public :: time_module_info
   public :: datetime2sec
   public :: sec2ddhhmmss
   public :: ymd2jul, ymd2reduced_jul
   public :: mjd2jul
   public :: jul2mjd
   public :: date2mjd   ! obsolete, use ymd2reduced_jul
   public :: mjd2date
   public :: datetime_to_string
   public :: parse_ud_timeunit
   public :: split_date_time
   public :: CalendarYearMonthDayToJulianDateNumber
   public :: julian, gregor, offset_reduced_jd

   interface ymd2jul
      ! obsolete, use ymd2reduced_jul
      module procedure CalendarDateToJulianDateNumber
      module procedure CalendarYearMonthDayToJulianDateNumber
   end interface ymd2jul

   interface date2mjd
      ! obsolete, use ymd2reduced_jul
      module procedure ymd2mjd
      module procedure datetime2mjd
      module procedure ymdhms2mjd
   end interface date2mjd

   interface ymd2reduced_jul
      module procedure ymd2reduced_jul_string
      module procedure ymd2reduced_jul_int
      module procedure ymd2reduced_jul_int3
   end interface ymd2reduced_jul

   interface mjd2date
      module procedure mjd2datetime
      module procedure mjd2ymd
      module procedure mjd2ymdhms
   end interface mjd2date

   interface datetime_to_string
      module procedure datetime2string
      module procedure jul_frac2string
      module procedure mjd2string
   end interface datetime_to_string

   real(kind=hp), parameter :: offset_reduced_jd   = 2400000.5_hp
   integer      , parameter :: firstGregorianDayNr = 2299161
   integer      , parameter :: justBeforeFirstGregorian(3) = [1582, 10, 14]
   integer      , parameter :: justAfterLastJulian(3)      = [1582, 10,  5]

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
          call addmessage(messages,'$Id: time_module.f90 65778 2020-01-14 14:07:42Z mourits $')
          call addmessage(messages,'$URL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_lgpl/deltares_common/packages/deltares_common/src/time_module.f90 $')
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

!---------------------------------------------------------------------------------------------
! implements interface ymd2reduced_jul
!---------------------------------------------------------------------------------------------
      !> calculates reduced Julian Date base on a string 'yyyyddmm' with or without separators
      function ymd2reduced_jul_string(date, reduced_jul_date) result (success)
         character(len=*), intent(in) :: date             !< date as string 'yyyyddmm' or 'yyyy dd mm' or 'yyyy d m'
         real(kind=hp), intent(out)   :: reduced_jul_date !< returned date as reduced modified julian
         logical                      :: success          !< function result

         integer :: year, month, day, ierr
         character :: separator
         logical :: has_separators
         character(len=20) :: fmt

         separator = date(5:5)
         has_separators = (separator < '0' .or. separator > '9')

         if (len_trim(date) >= 10) then
            fmt = '(i4,x,i2,x,i2)'     ! yyyy*dd*mm
         else if (has_separators) then
            fmt = '(i4,x,i1,x,i1)'     ! yyyy*d*m
         else
            fmt = '(i4,i2,i2)'         ! yyyymmdd
         endif

         read(date, fmt, iostat=ierr) year, month, day

         success = (ierr == 0)
         if (success) then
            reduced_jul_date = julian(year*10000 + month * 100 + day, 0)
         endif

      end function ymd2reduced_jul_string

      !> calculates reduced Julian Date base on a integer yyyyddmm
      function ymd2reduced_jul_int(yyyymmdd, reduced_jul_date) result(success)
         integer,       intent(in)  :: yyyymmdd          !< date as integer yyyymmdd
         real(kind=hp), intent(out) :: reduced_jul_date  !< output reduced Julian Date number
         logical                    :: success           !< function result

         integer :: year, month, day

         call splitDate(yyyymmdd, year, month, day)

         success = ymd2reduced_jul_int3(year, month, day, reduced_jul_date)

      end function ymd2reduced_jul_int

      !> calculates reduced Julian Date base on integers year, month and day
      function ymd2reduced_jul_int3(year, month, day, reduced_jul_date) result(success)
         integer      , intent(in)  :: year              !< year
         integer      , intent(in)  :: month             !< month
         integer      , intent(in)  :: day               !< day
         real(kind=hp), intent(out) :: reduced_jul_date  !< output reduced Julian Date number
         logical                    :: success           !< function result

         integer :: jdn

         jdn = CalendarYearMonthDayToJulianDateNumber(year, month, day)

         if (jdn == 0) then
            reduced_jul_date = 0.0_hp
            success = .false.
         else
            reduced_jul_date = real(jdn, hp) - offset_reduced_jd
            success = .true.
         endif

      end function ymd2reduced_jul_int3

!---------------------------------------------------------------------------------------------
! implements interface ymd2jul
!---------------------------------------------------------------------------------------------
      !> Calculates the Julian Date Number from a calender date.
      !! Returns 0 in case of failure.
      function CalendarDateToJulianDateNumber(yyyymmdd) result(jdn)
         integer             :: jdn      !< calculated Julian Date Number
         integer, intent(in) :: yyyymmdd !< Gregorian calender date
         !
         integer :: year  !< helper variable
         integer :: month !< helper variable
         integer :: day   !< helper variable
         !
         call splitDate(yyyymmdd, year, month, day)
         jdn = CalendarYearMonthDayToJulianDateNumber(year, month, day)
      end function CalendarDateToJulianDateNumber

      !> helper function to split integer yyyyddmm in 3 integers year, month and day
      subroutine splitDate(yyyymmdd, year, month, day)
         integer, intent(in)  :: yyyymmdd !< calender date
         integer, intent(out) :: year     !< year
         integer, intent(out) :: month    !< month
         integer, intent(out) :: day      !< day

         year = yyyymmdd/10000
         month = yyyymmdd/100 - year*100
         day = yyyymmdd - month*100 - year*10000
      end subroutine splitDate

      !> Calculates the Julian Date Number from a year, month and day.
      !! Returns 0 in case of failure.
      function CalendarYearMonthDayToJulianDateNumber(year, month, day) result(jdn)
         integer :: jdn               !< calculated Julian Date Number
         integer, intent(in) :: year  !< year
         integer, intent(in) :: month !< month
         integer, intent(in) :: day   !< day

         if (compareDates([year, month, day], justBeforeFirstGregorian) == 1) then
            jdn = GregorianYearMonthDayToJulianDateNumber(year, month, day)
         else if (compareDates([year, month, day], justAfterLastJulian) == -1) then
            jdn = JulianYearMonthDayToJulianDateNumber(year, month, day)
         else
            jdn = 0
         endif
      end function CalendarYearMonthDayToJulianDateNumber

      !> helper function to compare dates
      !> return 0 if equal, -1 if x is earlier than y, +1 otherwise
      integer function compareDates(x, y)
         integer, intent(in) :: x(3)  !< date 1 : [year, month, day]
         integer, intent(in) :: y(3)  !< date 2 (idem)

         integer :: i

         compareDates = 0
         do i = 1, 3
            if (x(i) < y(i)) then
               compareDates = -1
               exit
            else if (x(i) > y(i)) then
               compareDates = 1
               exit
            endif
         enddo
      end function compareDates

      !> calculates Julian Date Number based on date before oktober 1582
      function JulianYearMonthDayToJulianDateNumber(year, month, day) result(jdn)
      integer, intent(in) :: year  !< year
      integer, intent(in) :: month !< month
      integer, intent(in) :: day   !< day
      integer             :: jdn   !< function result: Juliun day number

      integer :: y, m, d
      real(kind=hp) :: jd

      jd = JulianYearMonthDayToJulianDateRealNumber(year, month, day) + 0.5_hp
      jdn = nint(jd)

      !
      ! Calculate backwards to test if the assumption is correct
      !
      call JulianDateNumberToCalendarYearMonthDay(jdn, y, m, d)
      !
      ! Test if calculation is correct
      !
      if (CompareDates([y, m, d], [year, month, day]) /= 0) then
         jdn = 0
      endif

      end function JulianYearMonthDayToJulianDateNumber

      !> from https://quasar.as.utexas.edu/BillInfo/JulianDateCalc.html
      function JulianYearMonthDayToJulianDateRealNumber(year, month, day) result(jdn)
      integer, intent(in) :: year  !< year
      integer, intent(in) :: month !< month
      integer, intent(in) :: day   !< day
      real(kind=hp)       :: jdn   !< function result: Julian day number

      integer       :: y, m
      real(kind=hp) :: e, f

      y = year
      m = month
      if (m < 3) then
         y = y-1
         m = m + 12
      endif
      e = floor(365.25_hp  * real(y + 4716, hp))
      f = floor(30.6001_hp * real(m + 1, hp))

      jdn = day + e + f - 1524.5_hp
      end function JulianYearMonthDayToJulianDateRealNumber

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
         call splitDate(yyyymmdd, year, month, day)
         !
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
         call JulianDateNumberToCalendarYearMonthDay(jdn, y, m, d)
         !
         ! Test if calculation is correct
         !
         if (CompareDates([y, m, d], [year, month, day]) /= 0) then
            jdn = 0
         endif
      end function GregorianYearMonthDayToJulianDateNumber

      ! =======================================================================

      subroutine JulianDateNumberToCalendarDate(jdn, yyyymmdd)
         integer, intent(in) :: jdn       !< Julian Date Number
         integer, intent(out) :: yyyymmdd !< calculated calender date

         integer :: year  !< helper variable
         integer :: month !< helper variable
         integer :: day   !< helper variable

         if (jdn >= firstGregorianDayNr) then
            call JulianDateNumberToGregorianDate(jdn, yyyymmdd)
         else
            call JulianDateNumberToJulianYearMonthDay(jdn, year, month, day)
            yyyymmdd = year*10000 + month*100 + day
         endif
      end subroutine JulianDateNumberToCalendarDate

      subroutine JulianDateNumberToCalendarYearMonthDay(jdn, year, month, day)
         integer, intent(in)  :: jdn   !< Julian Date Number
         integer, intent(out) :: year  !< calculated year
         integer, intent(out) :: month !< calculated month
         integer, intent(out) :: day   !< calculated day

         if (jdn >= firstGregorianDayNr) then
            call JulianDateNumberToGregorianYearMonthDay(jdn, year, month, day)
         else
            call JulianDateNumberToJulianYearMonthDay(jdn, year, month, day)
         endif
      end subroutine JulianDateNumberToCalendarYearMonthDay

      !> calculates (year, month, day) based on Julian date number before oktober 1582
      subroutine JulianDateNumberToJulianYearMonthDay(jdn, year, month, day)
         integer, intent(in)  :: jdn   !< Julian Date Number
         integer, intent(out) :: year  !< calculated year
         integer, intent(out) :: month !< calculated month
         integer, intent(out) :: day   !< calculated day

         real(kind=hp) :: jd

         jd = jdn-0.5_hp

         call JulianDateRealNumberToJulianYearMonthDay(jd, year, month, day)
      end subroutine JulianDateNumberToJulianYearMonthDay

      !> from https://quasar.as.utexas.edu/BillInfo/JulianDateCalc.html
      subroutine JulianDateRealNumberToJulianYearMonthDay(jdn, year, month, day)
         real(kind=hp), intent(in)  :: jdn   !< Julian Date Number
         integer, intent(out)       :: year  !< calculated year
         integer, intent(out)       :: month !< calculated month
         integer, intent(out)       :: day   !< calculated day

         real(kind=hp) :: a, b, z, f
         integer       :: c, d, e

         z = jdn + 0.5_hp
         f = z - floor(z)
         a = z
         b = a + 1524
         c = floor((b - 122.1_hp)/365.25_hp)
         d = floor(365.25_hp*c)
         e = floor((b - d)/30.6001_hp)

         month = merge(e-13, e-1, e > 13)
         day   = b - d - floor(30.6001_hp * real(e, hp)) + f
         year  = merge(c-4715, c-4716, month < 3)

      end subroutine JulianDateRealNumberToJulianYearMonthDay

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

!---------------------------------------------------------------------------------------------
! implements interface datetime_to_string
!---------------------------------------------------------------------------------------------
      !> Creates a string representation of a date time, in the ISO 8601 format.
      !! Example: 2015-08-07T18:30:27Z
      !! 2015-08-07T18:30:27+00:00
      !! Performs no check on validity of input numbers!
      function datetime2string(iyear, imonth, iday, ihour, imin, isec, ierr) result(datetimestr)
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
      end function datetime2string

      function jul_frac2string(jul, dayfrac, ierr) result(datetimestr)
         implicit none
         integer                , intent(in)  :: jul
         real(kind=hp), optional, intent(in)  :: dayfrac
         integer      , optional, intent(out) :: ierr        !< Error status, 0 if success, nonzero in case of format error.
         character(len=20)                    :: datetimestr !< The resulting date time string. Considering using trim() on it.

         real(kind=hp) :: days
         real(kind=hp) :: dayfrac_
         integer       :: ierr_

         if (present(dayfrac)) then
             dayfrac_ = dayfrac
         else
             dayfrac_ = 0.0_hp
         end if
         datetimestr = mjd2string(jul2mjd(jul,dayfrac_), ierr_)
         if (present(ierr)) then
            ierr = ierr_
         end if
      end function jul_frac2string

      function mjd2string(days, ierr) result(datetimestr)
         implicit none
         real(kind=hp)     , intent(in)  :: days
         integer , optional, intent(out) :: ierr        !< Error status, 0 if success, nonzero in case of format error.
         character(len=20)               :: datetimestr !< The resulting date time string. Considering using trim() on it.

         integer       :: iyear, imonth, iday, ihour, imin, isec
         real(kind=hp) :: second
         integer       :: ierr_

         ierr_ = -1
         if (mjd2datetime(days,iyear,imonth,iday,ihour,imin,second)/=0) then
            isec = nint(second) ! unfortunately rounding instead of truncating requires all of the following checks
            if (isec == 60) then
               imin = imin+1
               isec = 0
            endif
            if (imin == 60) then
               ihour = ihour+1
               imin = 0
            endif
            if (ihour == 24) then
               iday = iday+1
               ihour = 0
            endif
            select case(imonth)
            case (1,3,5,7,8,10) ! 31 days
               if (iday == 32) then
                  imonth = imonth+1
                  iday = 1
               endif
            case (12) ! 31 days
               if (iday == 32) then
                  iyear = iyear+1
                  imonth = 1
                  iday = 1
               endif
            case (4,6,9,11) ! 30 days
               if (iday == 31) then
                  imonth = imonth+1
                  iday = 1
               endif
            case default ! February
                if (leapYear(iyear)) then
                   if (iday == 30) then
                      imonth = 3
                      iday = 1
                   endif
                else ! 28 days
                   if (iday == 29) then
                      imonth = 3
                      iday = 1
                   endif
                endif
            end select
            datetimestr = datetime2string(iyear, imonth, iday, ihour, imin, isec, ierr_)
         else
            ierr_ = -1
            datetimestr = ' '
         endif
         if (present(ierr)) then
            ierr = ierr_
         end if
      end function mjd2string

      !> helper function to find out if a year is a leap year or not
      logical function leapYear(iyear)
         integer, intent(in) :: iyear

         integer :: jdn

         if (mod(iyear, 4) /= 0) then
            ! basic check: if year is not a multiple of 4 it is certainly NOT a leap year
            leapYear = .false.
         else
            ! if it is a multiple of 4, it is quite complex,
            ! so check if 29 February is a valid date in that year:
            jdn = CalendarYearMonthDayToJulianDateNumber(iyear, 2, 29)
            leapYear = (jdn /= 0)
         endif
      end function leapYear

      ! implements (obsolete!) interface date2mjd
      function ymd2mjd(ymd) result(days)
         implicit none
         integer, intent(in)       :: ymd
         real(kind=hp)             :: days

         integer       :: year, month, day

         call splitDate(ymd, year, month, day)
         days = datetime2mjd(year,month,day,0,0,0.d0)
      end function ymd2mjd

      function ymdhms2mjd(ymd,hms) result(days)
         implicit none
         integer, intent(in)       :: ymd
         real(kind=hp), intent(in) :: hms
         real(kind=hp)             :: days
         integer       :: year, month, day, hour, minute
         real(kind=hp) :: second
         year   = int(ymd/10000)
         month  = int(mod(ymd,10000)/100)
         day    = mod(ymd,100)
         hour   = int(hms/10000)
         minute = int(mod(hms,10000.d0)/100)
         second = mod(hms,100.d0)
         days = datetime2mjd(year,month,day,hour,minute,second)
      end function ymdhms2mjd

      function datetime2mjd(year,month,day,hour,minute,second) result(days)
         implicit none
         integer, intent(in)   :: year, month, day
         integer, intent(in)   :: hour, minute
         real(kind=hp):: second
         real(kind=hp) :: days
         real(kind=hp) :: dayfrac
         dayfrac = (hour*3600+minute*60+second)/(24*3600)
         days = jul2mjd(CalendarYearMonthDayToJulianDateNumber(year,month,day),dayfrac)
      end function datetime2mjd

!---------------------------------------------------------------------------------------------
      function mjd2jul(days,frac) result(jul)
         implicit none
         real(kind=hp)          , intent(in)  :: days
         real(kind=hp), optional, intent(out) :: frac
         integer                              :: jul

         jul = int(days+offset_reduced_jd)
         if (present(frac)) then
             frac = mod(days,0.5_hp)
         endif
      end function mjd2jul

      function jul2mjd(jul,frac) result(days)
         implicit none
         integer                , intent(in)  :: jul
         real(kind=hp), optional, intent(in)  :: frac
         real(kind=hp)                        :: days

         days = real(jul,hp)-offset_reduced_jd
         if (present(frac)) then
             days = days + frac
         endif
      end function jul2mjd

!---------------------------------------------------------------------------------------------
! implements interface mjd2date
!---------------------------------------------------------------------------------------------
      function mjd2ymd(days,ymd) result(success)
         implicit none
         real(kind=hp), intent(in)  :: days
         integer, intent(out)       :: ymd
         integer       :: year, month, day, hour, minute
         real(kind=hp) :: second
         integer       :: success

         success = 0
         if (mjd2datetime(days,year,month,day,hour,minute,second)==0) return
         ymd = year*10000 + month*100 + day
         success = 1
      end function mjd2ymd

      function mjd2ymdhms(days,ymd,hms) result(success)
         implicit none
         real(kind=hp), intent(in)       :: days
         integer, intent(out)            :: ymd
         real(kind=hp), intent(out)      :: hms
         integer       :: year, month, day, hour, minute
         real(kind=hp) :: second
         integer       :: success

         success = 0
         if (mjd2datetime(days,year,month,day,hour,minute,second)==0) return
         ymd = year*10000 + month*100 + day
         hms = hour*10000 + minute*100 + second
         success = 1
      end function mjd2ymdhms

      function mjd2datetime(days,year,month,day,hour,minute,second) result(success)
         implicit none
         real(kind=hp), intent(in)  :: days
         integer,  intent(out)      :: year, month, day
         integer,  intent(out)      :: hour, minute
         real(kind=hp), intent(out) :: second
         real(kind=hp) :: dayfrac
         integer       :: jul
         integer       :: success

         success = 0
         jul = mjd2jul(days,dayfrac)
         call JulianDateNumberToCalendarYearMonthDay(jul,year,month,day)
         hour = int(dayfrac*24)
         minute = int(mod(dayfrac*24*60,60.d0))
         second = mod(dayfrac*24*60*60,60.d0)
         success = 1
      end function mjd2datetime

      !> split a string in date and time part
      subroutine split_date_time(string, date, time)
         character(len=*), intent(in)  :: string  !< input string like 1950-01-01 00:00:00; with or without time
         character(len=*), intent(out) :: date    !< output date, in this case 1950-01-01
         character(len=*), intent(out) :: time    !< output time, in this case 00:00:00

         character(len=:), allocatable :: date_time
         integer                       :: ipos

         date_time = trim(adjustl(string))
         ipos      = index(date_time, ' ')

         if (ipos > 0) then
            date = date_time(1:ipos-1)
            time = adjustl(date_time(ipos+1:))
         else
            date = date_time
            time = ' '
         endif
      end subroutine split_date_time

      DOUBLE PRECISION FUNCTION JULIAN ( IDATE , ITIME )
!***********************************************************************
!
!     Description of module :
!
!        This functions returns the so called Julian day of a date, or
!        the value -1.0 if an error occurred.
!
!        The Julian day of a date is the number of days that has passed
!        since January 1, 4712 BC at 12h00 ( Gregorian). It is usefull
!        to compute differences between dates. ( See SUBROUTINE GREGOR
!        for the reverse proces ).
!
!         If idate is before 15821005 Julian calendar is assumed,
!         otherwise Gregorian.
!
!***********************************************************************
!
!     Arguments :
!
!     Name   Type     In/Out Size            Description
!     ------ -----    ------ -------         ---------------------------
!     IDATE  integer  in     -               Date as YYYYMMDD
!     ITIME  integer  in     -               Time as HHMMSS
!
!     Local variables :
!
!     Name   Type     Size   Description
!     ------ -----    ------ ------------------------
!     TEMP1  real*8   -      Temporary variable
!     TEMP2  real*8   -      Temporary variable
!     IYEAR  integer  -      Year   ( -4713-.. )
!     IMONTH integer  -      Month  ( 1-12 )
!     IDAY   integer  -      Day    ( 1-28,29,30 or 31 )
!     IHOUR  integer  -      Hour   ( 0-23 )
!     IMIN   integer  -      Minute ( 0-59 )
!     ISEC   integer  -      Second ( 0-59 )
!     MONLEN integer  12     Length of month in days
!
!     Calls to : none
!
!***********************************************************************
!
!     Variables :
!
      IMPLICIT NONE !!!

      INTEGER          IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISEC, IDATE, ITIME, MONLEN(12)
      DOUBLE PRECISION TEMP1, TEMP2
!
!***********************************************************************
!
!     Initialize lenghts of months :
!
      DATA MONLEN / 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
!
!***********************************************************************
!
!
!
      IYEAR  = IDATE/10000
      IMONTH = IDATE/100 - IYEAR*100
      IDAY   = IDATE - IYEAR*10000 - IMONTH*100
      IHOUR  = ITIME/10000
      IMIN   = ITIME/100 - IHOUR*100
      ISEC   = ITIME - IHOUR*10000 - IMIN*100
      
      IF (( IYEAR  .LT. -4713 ) .OR. ( IMONTH .LT.  1 ) .OR. &
          ( IMONTH .GT.    12 ) .OR. ( IDAY   .LT.  1 ) .OR. &
          ( IDAY   .GT. MONLEN(IMONTH) ) .OR. &
          ( IHOUR  .LT.     0 ) .OR. ( IHOUR  .GT. 23 ) .OR. &
          ( IMIN   .LT.     0 ) .OR. ( IMIN   .GT. 59 ) .OR. &
          ( ISEC   .LT.     0 ) .OR. ( ISEC   .GT. 60 )) THEN
!!!GP original check was op 59 seconden, gaf afrondingsproblemen; daarom nu op 60 gezet
!!!  5    ( ISEC   .LT.     0 ) .OR. ( ISEC   .GT. 59 )) THEN
         JULIAN = -1.0
         GOTO 999
      ELSE if (compareDates([iyear, imonth, iday], justAfterLastJulian) == -1) then
         TEMP2 = JulianYearMonthDayToJulianDateNumber(iyear, imonth, iday)
         TEMP1  = FLOAT ( IHOUR ) * 3600.0 + &
                  FLOAT ( IMIN  ) *   60.0 + &
                  FLOAT ( ISEC  ) - 43200.0
         JULIAN = TEMP2 + ( TEMP1 / 86400.0 ) - offset_reduced_jd
      ELSE
         TEMP1  = INT (( IMONTH-14.0) / 12.0 )
         TEMP2  = IDAY - 32075.0 + &
                INT ( 1461.0 * ( IYEAR + 4800.0 + TEMP1 ) / 4.0 ) + &
                INT ( 367.0 * ( IMONTH - 2.0 - TEMP1 * 12.0 ) / 12.0 ) - &
                INT ( 3.0 * INT ( ( IYEAR + 4900.0 + TEMP1 ) / 100.0 ) / &
                4.0 )
         TEMP1  = FLOAT ( IHOUR ) * 3600.0 + &
                  FLOAT ( IMIN  ) *   60.0 + &
                  FLOAT ( ISEC  ) - 43200.0
         JULIAN = TEMP2 + ( TEMP1 / 86400.0 ) - offset_reduced_jd
      ENDIF
  999 RETURN
      END FUNCTION JULIAN

      SUBROUTINE GREGOR ( JULIAN, IYEAR , IMONTH, IDAY  , IHOUR , IMIN  , ISEC  , DSEC)
!***********************************************************************
!
!     Description of module :
!
!        This functions returns the Gregorian date and the time of a so
!        called Julian day, or iyear -9999 if an error occurred.
!
!        The Julian day of a date is the number of days that has passed
!        since January 1, 4712 BC at 12h00 ( Gregorian). It is usefull
!        to compute differces between dates. ( See DOUBLE PRECISION
!        FUNCTION JULIAN for the reverse proces ).
!
!***********************************************************************
!
!     Arguments :
!
!     Name   Type     In/Out Size            Description
!     ------ -----    ------ -------         ---------------------------
!     JULIAN real*8   in     -               Julian day
!     IYEAR  integer  out    -               Year   ( -4713-.. )
!     IMONTH integer  out    -               Month  ( 1-12 )
!     IDAY   integer  out    -               Day    ( 1-28,29,30 or 31 )
!     IHOUR  integer  out    -               Hour   ( 0-23 )
!     IMIN   integer  out    -               Minute ( 0-59 )
!     ISEC   integer  out    -               Second ( 0-59 )
!     DSEC   real*8   out    -               Second as double
!
!     Local variables :
!
!     Name   Type     Size   Description
!     ------ -----    ------ ------------------------
!     TEMP1  real*8   -      Temporary variable
!     TEMP2  real*8   -      Temporary variable
!     TEMP3  real*8   -      Temporary variable
!     TEMP4  real*8   -      Temporary variable, JULIAN
!     TEMP5  real*8   -      Temporary variable, fractional part JULIAN
!
!     Calls to : none
!
!***********************************************************************
!
!     Variables :
!
      INTEGER           :: IYEAR , IMONTH, IDAY  , IHOUR , IMIN  , ISEC
      DOUBLE PRECISION  :: JULIAN, TEMP1 , TEMP2 , TEMP3 , TEMP4 , TEMP5
      DOUBLE PRECISION  :: DSEC
      DOUBLE PRECISION  :: myJULIAN, delta
      integer           :: nTry
!
!***********************************************************************
!
      delta = 0.0D+00
 
      IF ( JULIAN .LT. 0.0 ) THEN
         IYEAR = -9999
      ELSE IF ( JULIAN < real(firstGregorianDayNr, hp)) then
         IYEAR = -9999 ! not yet implemented
      ELSE
         nTry = 1
         DO WHILE ( nTry <= 2 )
             myJULIAN= JULIAN + delta
             TEMP4 = myJULIAN
             TEMP5 = DMOD ( myJULIAN, 1.0D0 )
             IF ( TEMP5 .LT. 0.5 ) THEN
                TEMP3  = 0.5 + TEMP5
                TEMP4  = DINT ( TEMP4 )
             ELSE
                TEMP3  = TEMP5 - 0.5
                TEMP4  = DINT ( TEMP4 ) + 1.0
             ENDIF
             TEMP1  = TEMP4 + 68569.0
             TEMP2  = DINT  ( 4.0 * TEMP1 / 146097.0 )
             TEMP1  = TEMP1 - DINT ( ( 146097.0 * TEMP2 + 3.0 ) / 4.0 )
             IYEAR  = INT   ( 4000.0 * ( TEMP1 + 1.0 ) / 1461001.0 )
             TEMP1  = TEMP1 - DINT ( (1461.0D0 * IYEAR) / 4.0 ) + 31.0
             IMONTH = INT   ( 80.0 * TEMP1 / 2447.0 )
             IDAY   = INT   ( TEMP1 - AINT ( 2447.0 * IMONTH / 80.0 ) )
             TEMP1  = DINT  ( dble(IMONTH / 11.0D0) )
             IMONTH = INT   ( IMONTH + 2.0 - 12.0 * TEMP1 )
             IYEAR  = INT   ( 100.0 * ( TEMP2 - 49.0 ) + IYEAR + TEMP1 )
             IHOUR  = INT   ( TEMP3 * 24.0 )
             IMIN   = INT   ( TEMP3 * 1440.0 - 60.0 * IHOUR )
             DSEC   =         TEMP3 * 86400.0 - 3600.0 * IHOUR - 60.0*IMIN
             ISEC   = NINT  ( DSEC )

             if ( isec >= 60 ) then
                 if ( nTry < 2 ) then
                     delta = 0.49999D+00 / 86400.0D+00
                     nTry = nTry + 1
                 else
                     IYEAR = -9999
                     exit
                 endif
             else
                 exit
             endif
         ENDDO

      ENDIF

      END SUBROUTINE GREGOR

end module time_module
