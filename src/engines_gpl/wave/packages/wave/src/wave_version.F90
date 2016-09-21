!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2013.
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
!  $Id: wave_version.F90.svn 2392 2013-03-28 14:27:50Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/engines_gpl/wave/packages/wave/src/wave_version.F90.svn $
!
! Do not use a module here; that will cause a serious dependency problem
!
!
!
!===============================================================================
subroutine getfullversionstring_wave(stringout)
   character(*), intent(out) :: stringout
   integer                   :: length

   character(*), parameter :: wave_major        = '3'
   character(*), parameter :: wave_minor        = '05'
   character(*), parameter :: wave_revision     = '01'
   character(*), parameter :: wave_build_number = '000000'

   character(*), parameter :: wave_company      = 'Deltares'
   character(*), parameter :: wave_company_url  = 'http://www.deltares.nl'
   character(*), parameter :: wave_program      = 'Delft3D-WAVE'

   character(*), parameter :: wave_version      = wave_major//'.'//wave_minor//'.'//wave_revision//'.'//wave_build_number
   character(*), parameter :: wave_version_full = 'Deltares, '//wave_program//' Version '//wave_version//', '//__DATE__//', '//__TIME__
   character(*), parameter :: wave_version_id   = '@(#)'//wave_version_full



   !
   ! body
   length    = min(len_trim(wave_version_full),len(stringout))
   stringout = wave_version_id(5:5+length-1)
end subroutine getfullversionstring_wave
!
!
!===============================================================================
subroutine getshortversionstring_wave(stringout)
   character(*), intent(out) :: stringout
   integer                   :: length

   character(*), parameter :: wave_major         = '3'
   character(*), parameter :: wave_minor         = '05'

   character(*), parameter :: wave_short_version = wave_major//'.'//wave_minor
   !
   ! body
   length    = min(len_trim(wave_short_version),len(stringout))
   stringout = wave_short_version(1:length)
end subroutine getshortversionstring_wave
!
!
!===============================================================================
subroutine getcomfileversionstring_wave(stringout)
   character(*), intent(out) :: stringout
   integer                   :: length

   character(*), parameter :: com_file_version      = '3.54.11'   ! Needed for the file header

   !
   ! body
   length    = min(len_trim(com_file_version),len(stringout))
   stringout = com_file_version(1:length)
end subroutine getcomfileversionstring_wave
!
!
!===============================================================================
subroutine getmapfileversionstring_wave(stringout)
   character(*), intent(out) :: stringout
   integer                   :: length

   character(*), parameter :: map_file_version      = '3.54.28'   ! Needed for the file header

   !
   ! body
   length    = min(len_trim(map_file_version),len(stringout))
   stringout = map_file_version(1:length)
end subroutine getmapfileversionstring_wave
!
!
!===============================================================================
subroutine gethisfileversionstring_wave(stringout)
   character(*), intent(out) :: stringout
   integer                   :: length

   character(*), parameter :: his_file_version      = '3.52.09'   ! Needed for the file header

   !
   ! body
   length    = min(len_trim(his_file_version),len(stringout))
   stringout = his_file_version(1:length)
end subroutine gethisfileversionstring_wave
!
!
!===============================================================================
subroutine getdrofileversionstring_wave(stringout)
   character(*), intent(out) :: stringout
   integer                   :: length

   character(*), parameter :: dro_file_version      = '3.20.01'   ! Needed for the file header

   !
   ! body
   length    = min(len_trim(dro_file_version),len(stringout))
   stringout = dro_file_version(1:length)
end subroutine getdrofileversionstring_wave

