!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2020.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

! $Id: dfm_max25_getdata.f90 65778 2020-01-14 14:07:42Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/tools_gpl/dfmoutput/src/dfm_max25_getdata.f90 $

!> DFM_MAX25_GETDATA - Subroutine to provide max25 filters to dfmoutput.
!!
module dfm_max25_getdata
   use netcdf
   use write_extremes_his
   use read_nc_histories
   implicit none
   private
   public :: fmgetdata

   contains

!> main routine to write max25 output to file
subroutine fmgetdata(filename, filename_out, field_name, minmaxlst)
   implicit none
   character(len=*) , intent(in) :: filename      !< input filename (NetCDF)
   character(len=*) , intent(in) :: field_name    !< input field name (e.g. 'waterlevel')
   character(len=*) , intent(in) :: filename_out  !< output filename (ascii)
   character(len=*),  intent(in) :: minmaxlst     !< list with filter lengths (e.g. '13,25')

   integer :: ierr, i, nStations
   real, allocatable :: hisdata(:,:)
   character(len=64), allocatable :: stations(:)
   integer, allocatable :: stats_index(:), list(:)

                           ierr = read_meta_data(filename, nStations)
   if (ierr == nf90_noerr) ierr = read_station_names(stations, 'station_name')
   if (ierr == nf90_noerr) ierr = read_data(hisdata, field_name)
   if (ierr == nf90_noerr) ierr = close_nc_his_file()

   if (ierr /= nf90_noerr) then
      write(*,*) trim(nf90_strerror(ierr))
   else
      allocate(stats_index(nStations))
      do i = 1, nStations
         stats_index(i) = i
      enddo
      call parse_min_max_list(minmaxlst, list)
      call write_extremes_stat( hisdata, 1, size(stations), filename_out, stations, stats_index, list)
   endif

end subroutine fmgetdata

!> convert comma seperated values in a string to an array of integers
subroutine parse_min_max_list(list_string, list)
   character(len=*),     intent(in)  :: list_string  !< string with filter lengths (e.g. '13,25')
   integer, allocatable, intent(out) :: list(:)      !< list with filter lengths

   integer :: size, i

   size = 1
   do i = 1, len(list_string)
      if (list_string(i:i) == ',') size = size + 1
   enddo
   allocate(list(size))
   read(list_string,*) list
end subroutine parse_min_max_list

end module dfm_max25_getdata
