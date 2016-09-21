!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                
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

!  $Id: ec_filereader_read.F90 5640 2015-12-10 09:24:34Z hummel $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/ec_module/packages/ec_module/src/ec_filereader_read.F90 $

!> This module contains the read methods for the meteo files.
!! @author stef.hummel@deltares.nl
!! @author herman.kernkamp@deltares.nl
!! @author adri.mourits@deltares.nl
!! @author edwin.bos@deltares.nl
module m_ec_filereader_read
   use precision
   use string_module
   use m_ec_typedefs
   use m_ec_support
   use m_ec_message
   use m_ec_bcreader
   use mathconsts
   use time_module
   use string_module
   use m_alloc
   
   implicit none
   
   private
   
   public :: ecUniReadFirstLine
   public :: ecUniReadTimeSteps
   public :: ecUniReadBlock
   public :: ecBCReadBlock
   public :: ecFourierReadAll
   public :: ecFindInFile
   public :: ecSpiderwebAndCurviFindInFile
   public :: ecSpiderAndCurviAndArcinfoReadToBody
   public :: ecSpiderwebReadBlock
   public :: ecArcinfoAndT3dReadBlock
   public :: ecCurviReadBlock
   public :: ecNetcdfReadBlock
   public :: ecQhtableReadAll
   public :: ect3DFindInFile
   public :: ecNetcdfReadNextBlock
   public :: ecApplyCorrectionToCmp
   public :: ecSampleReadAll
   public :: ecParseARCinfoMask
   public :: asc
   
contains
      
      ! =======================================================================
      
      !> Read the first line from a uni* file.
      function ecUniReadFirstLine(fileReaderPtr) result(rec)
         character(len=maxRecordLen)  :: rec           !< content of a line
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(in)
         !
         integer :: istat !< status of read operation
         !
         rewind(unit=fileReaderPtr%fileHandle)
         ! continue reading lines untill a data line is encountered
         do
            read(fileReaderPtr%fileHandle, '(a)', IOSTAT = istat) rec
            if (istat == 0) then
               call strip_comment(rec)
               if (len_trim(rec)>0) then 
                  exit
               end if
            else
               call setECMessage("INFO: ec_filereader_read::ecUniReadFirstLine: File end has been reached of: "//trim(fileReaderPtr%fileName))
               exit
            end if
         end do
      end function ecUniReadFirstLine
      
      ! =======================================================================
      
      !> Read the number of time steps of the next record from a uni* file.
      !! meteo1: readseries
      function ecUniReadTimeSteps(fileReaderPtr, time_steps) result(success)
         logical                                  :: success       !< function status
         type(tEcFileReader), pointer             :: fileReaderPtr !< intent(in)
         real(hp),                    intent(out) :: time_steps    !< number of time steps of length tEcTimeFrame%time_unit
         !
         character(132) :: rec   !< content of a line
         integer        :: istat !< status of read operation
         !
         success = .false.
         ! continue reading lines untill a data line is encountered
         do
            read(fileReaderPtr%fileHandle, '(a)', IOSTAT = istat) rec
            if (istat == 0) then
               if (.not. (rec(1:1) == '*' .or. rec(1:1) == '#' .or. len_trim(rec) == 0)) then
                  read(rec, *, IOSTAT = istat) time_steps
                  if (istat == 0) then
                     ! Convert from minutes to seconds.
                     time_steps = time_steps * 60.0_hp
                     success = .true.
                  else
                     call setECMessage("ERROR: ec_filereader_read::ecUniReadTimeSteps: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                     return
                  end if
               end if
            else
               call setECMessage("INFO: ec_filereader_read::ecUniReadTimeSteps: File end has been reached of: "//trim(fileReaderPtr%fileName))
               exit
            end if
         end do
      end function ecUniReadTimeSteps
      
      ! =======================================================================
      
      !> Read the next record from a uni* file.
      !! meteo1: readseries
      function ecUniReadBlock(fileReaderPtr, time_steps, values) result(success)
         logical                               :: success       !< function status
         type(tEcFileReader),    pointer       :: fileReaderPtr !< intent(in)
         real(hp),               intent(inout) :: time_steps    !< number of time steps of duration: seconds
         real(hp), dimension(:), intent(inout) :: values        !< read values
         !
         integer        :: n_values !< number of quantities in the file
         character(132) :: rec      !< content of a line
         integer        :: istat    !< status of read operation
         integer        :: i        !< loop counter
         !
         success = .false.
         n_values = size(values)
         ! continue reading lines untill a data line is encountered
         do
            read(fileReaderPtr%fileHandle, '(a)', IOSTAT = istat) rec
            if (istat == 0) then
               call strip_comment(rec)
               if (len_trim(rec)>0) then 
                  read(rec, *, IOSTAT = istat) time_steps, ( values(i), i=1,n_values )
                  if (istat == 0) then
                     ! Convert from minutes to seconds.
                     time_steps = time_steps * 60.0_hp
                     success = .true.
                  else
                     call setECMessage("Read failure before end of file: "//trim(fileReaderPtr%fileName))
                     call setECMessage("     line = "//trim(rec))
                     return
                  end if
                  exit
               end if
            else
               call setECMessage("File end has been reached of: "//trim(fileReaderPtr%fileName))
               exit
            end if
         end do
      end function ecUniReadBlock
      
      ! =======================================================================

      !> Read the next record from a *.bc file.
      !> Requests a line from the EcBC object's stringbuffer block, advancing its pointer in the block
      function ecBCReadBlock(fileReaderPtr, time_steps, values) result(success)
         implicit none
         logical                               :: success       !< function status
         type(tEcFileReader),    pointer       :: fileReaderPtr !< intent(in)
         real(hp),               intent(inout) :: time_steps    !< number of time steps of duration: seconds
         real(hp), dimension(:), intent(inout) :: values        !< read values
         type(tEcItem),          pointer       :: item
         success = ecBCreadline(fileReaderPtr, values = values, time_steps = time_steps)
      end function ecBCReadBlock

      ! =======================================================================
      
      !> Read the next record from a Curvi file.
      !! meteo1: readarcinfoblock
      function ecCurviReadBlock(fileReaderPtr, handle, t0t1) result(success)
         use string_module
         !
         logical                         :: success       !< function status
         type(tEcFileReader), pointer    :: fileReaderPtr !< intent(in)
         integer,             intent(in) :: handle        !< file handle
         integer,             intent(in) :: t0t1          !< read into Field T0 or T1 (0,1)
         !
         integer                   :: n_cols     !< number of columns
         integer                   :: n_rows     !< number of rows
         type(tEcItem),    pointer :: item       !< Item containing the first Quantity
         character(len=maxNameLen) :: rec        !< helper variable
         real(hp)                  :: time_steps !< number of time steps from reference time of current data block
         integer                   :: i, j, k    !< loop counters
         character(len=maxNameLen) :: keyword    !< helper variable
         real(hp)                  :: timesteps  !< helper variable
         integer                   :: istat      !< status of read operation
         !
         success = .false.
         !
         n_cols = fileReaderPtr%items(1)%ptr%elementSetPtr%n_cols
         n_rows = fileReaderPtr%items(1)%ptr%elementSetPtr%n_rows
         ! Find the time specification line of the next block.
         keyword = 'TIME'
         rec = ecFindInFile(handle, keyword)
         if (len(trim(rec)) == 0) then
            call setECMessage("ERROR: ec_filereader_read::ecCurviReadBlock: Reached end of file: "//trim(fileReaderPtr%fileName))
            return
         end if
         ! Read and convert the timesteps to seconds.
         if (.not. ecGetTimesteps(rec, time_steps, .false.)) then
            return
         end if
         !
         ! ===== T0 =====
         if (t0t1 == 0) then
            ! Set the new time.
            timesteps = ecSupportThisTimeToTimesteps(fileReaderPtr%tframe, time_steps)
            do k=1, fileReaderPtr%nItems
               item => fileReaderPtr%items(k)%ptr
               item%sourceT0FieldPtr%timesteps = timesteps
            end do
            ! Set the new data.
            do k=1, fileReaderPtr%nItems
               item => fileReaderPtr%items(k)%ptr
               do i=n_rows, 1, -1
                  read(handle, *, IOSTAT = istat) (item%sourceT0FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
                  if(istat /= 0) then
                     call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                     call setECMessage("     line = "//trim(rec))
                     return
                  end if
               end do
            end do
         ! ===== T1 =====
         else if (t0t1 == 1) then
            ! Set the new time.
            timesteps = ecSupportThisTimeToTimesteps(fileReaderPtr%tframe, time_steps)
            do k=1, fileReaderPtr%nItems
               item => fileReaderPtr%items(k)%ptr
               item%sourceT1FieldPtr%timesteps = timesteps
            end do
            ! Set the new data.
            do k=1, fileReaderPtr%nItems
               item => fileReaderPtr%items(k)%ptr
               do i=n_rows, 1, -1
                  read(handle, *, IOSTAT = istat) (item%sourceT1FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
                  if(istat /= 0) then
                     call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                     call setECMessage("     line = "//trim(rec))
                     return
                  end if
               end do
            end do
         else
            call setECMessage("ERROR: ec_filereader_read::ecCurviReadBlock: Invalid Field specified.")
            return
         end if
         success = .true.
      end function ecCurviReadBlock

      ! =======================================================================
      
      !> Read the next record from a Arcinfo or Curvi file.
      !! meteo1: readarcinfoblock
      function ecArcinfoAndT3dReadBlock(fileReaderPtr, handle, t0t1, n_cols, n_rows, item1, item2, item3) result(success)
         use string_module
         logical                             :: success       !< function status
         type(tEcFileReader),     pointer    :: fileReaderPtr !< intent(in)
         integer,                 intent(in) :: handle        !< file handle
         integer,                 intent(in) :: t0t1          !< read into Field T0 or T1 (0,1)
         integer,                 intent(in) :: n_cols
         integer,                 intent(in) :: n_rows
         type(tEcItem),           pointer    :: item1         !< Item containing quantity1, intent(inout)
         type(tEcItem), optional, pointer    :: item2         !< Item containing quantity2, intent(inout)
         type(tEcItem), optional, pointer    :: item3         !< Item containing quantity3, intent(inout)
         !
         character(len=maxNameLen) :: rec        !< helper variable
         real(hp)                  :: time_steps !< number of time steps from reference time of current data block
         integer                   :: i, j       !< loop counter
         character(len=maxNameLen) :: keyword    !< helper variable
         integer                   :: istat      !< status of read operation
         
         !
         success = .false.
         keyword = 'TIME'
         ! Find the time specification line of the next block.
         rec = ecFindInFile(handle, keyword)
         if (len(trim(rec)) == 0) then
            call setECMessage("ERROR: ec_filereader_read::ecArcinfoAndT3dReadBlock: Reached end of file: "//trim(fileReaderPtr%fileName))
            return
         end if
         ! Read and convert the timesteps to seconds.
         if (.not. ecGetTimesteps(rec, time_steps, .false.)) then
            return
         else
            success = .true.
         end if
         !
         ! ===== T0 =====
         if (t0t1 == 0) then
            ! Set the new time.
            if (success) then
               item1%sourceT0FieldPtr%timesteps = ecSupportThisTimeToTimesteps(fileReaderPtr%tframe, time_steps)
               if (present(item2)) then
                  item2%sourceT0FieldPtr%timesteps = item1%sourceT0FieldPtr%timesteps
               end if
               if (present(item3)) then
                  item3%sourceT0FieldPtr%timesteps = item1%sourceT0FieldPtr%timesteps
               end if
            end if
            ! Set the new data.
            do i=n_rows, 1, -1
               read(handle, *, IOSTAT = istat) (item1%sourceT0FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
                  return
               end if
            end do
            if (present(item2)) then
               do i=n_rows, 1, -1
                  read(handle, *, IOSTAT = istat) (item2%sourceT0FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
                  if(istat /= 0) then
                     call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                     call setECMessage("     line = "//trim(rec))
                     success = .false.
                     return
                  end if
               end do
            end if
            if (present(item3)) then
               do i=n_rows, 1, -1
                  read(handle, *, IOSTAT = istat) (item3%sourceT0FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
                  if(istat /= 0) then
                     call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                     call setECMessage("     line = "//trim(rec))
                     success = .false.
                     return
                  end if
               end do
            end if
         ! ===== T1 =====
         else if(t0t1 == 1) then
            ! Set the new time.
            if (success) then
               item1%sourceT1FieldPtr%timesteps = ecSupportThisTimeToTimesteps(fileReaderPtr%tframe, time_steps)
               if (present(item2)) then
                  item2%sourceT1FieldPtr%timesteps = item1%sourceT1FieldPtr%timesteps
               end if
               if (present(item3)) then
                  item3%sourceT1FieldPtr%timesteps = item1%sourceT1FieldPtr%timesteps
               end if
            end if
            ! Set the new data.
            do i=n_rows, 1, -1
               read(handle, *, IOSTAT = istat) (item1%sourceT1FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
               return
               end if
            end do
            if (present(item2)) then
               do i=n_rows, 1, -1
                  read(handle, *, IOSTAT = istat) (item2%sourceT1FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
               return
               end if
               end do
            end if
            if (present(item3)) then
               do i=n_rows, 1, -1
                  read(handle, *, IOSTAT = istat) (item3%sourceT1FieldPtr%arr1dPtr((i-1)*n_cols+j), j=1, n_cols)
                  if(istat /= 0) then
                     call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                     call setECMessage("     line = "//trim(rec))
                     success = .false.
                     return
                  end if
               end do
            end if
         else
            call setECMessage("ERROR: ec_filereader_read::ecArcinfoAndT3dReadBlock: Invalid Field specified.")
            success = .false.
         end if
      end function ecArcinfoAndT3dReadBlock

      ! =======================================================================
      ! TODO: we should really switch to newer spiderweb reader in 
      ! ec_module\packages\ec_module\src\meteo\meteo_read.f90

      !> Read the next record from a spiderweb file.
      !! meteo1: reaspwtim
      function ecSpiderwebReadBlock(fileReaderPtr, item1, item2, item3, t0t1, n_cols, n_rows) result(success)
         logical                         :: success       !< function status
         type(tEcFileReader), pointer    :: fileReaderPtr !< intent(in)
         type(tEcItem),       pointer    :: item1         !< Item containing quantity1, intent(inout)
         type(tEcItem),       pointer    :: item2         !< Item containing quantity2, intent(inout)
         type(tEcItem),       pointer    :: item3         !< Item containing quantity3, intent(inout)
         integer,             intent(in) :: t0t1          !< read into Field T0 or T1 (0,1)
         integer,             intent(in) :: n_cols
         integer,             intent(in) :: n_rows
         !
         character(len=maxNameLen) :: rec        !< helper variable
         real(hp)                  :: time_steps !< number of time steps from reference time of current data block
         integer                   :: i, j       !< loop counter
         real(hp)                  :: x_spw_eye
         real(hp)                  :: y_spw_eye
         real(hp)                  :: p_drop_spw_eye
         character(len=maxNameLen) :: keyword
         integer                   :: istat      ! Reader status code
         !
         success = .true.                        ! Returns success, even when the file end is reached (see UNST-708)
         keyword = 'TIME'
         !
         rec = ecFindInFile(fileReaderPtr%fileHandle, 'TIME')
         if (len(trim(rec)) == 0) then
            call setECMessage("INFO: ec_filereader_read::ecSpiderwebReadBlock: Reached end of file: "//trim(fileReaderPtr%fileName))
            return
         end if
         ! Read and convert the timesteps to seconds.
         if (.not. ecGetTimesteps(rec, time_steps, .false.)) then
            success = .false.
            return
         endif
         !
         ! ===== T0 =====
         if (t0t1 == 0) then
            item1%sourceT0FieldPtr%timesteps = ecSupportThisTimeToTimesteps(fileReaderPtr%tframe, time_steps)
            item2%sourceT0FieldPtr%timesteps = ecSupportThisTimeToTimesteps(fileReaderPtr%tframe, time_steps)
            item3%sourceT0FieldPtr%timesteps = ecSupportThisTimeToTimesteps(fileReaderPtr%tframe, time_steps)
            rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'x_spw_eye', .false.)         
            if (len_trim(rec) == 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Failed to find keyword", "x_spw_eye")
               success = .false.               
               return
            end if
            
            read(rec, *, IOSTAT = istat) x_spw_eye
            if(istat /= 0) then
               call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
               call setECMessage("     line = "//trim(rec))
               success = .false.
               return
            end if
            item1%sourceT0FieldPtr%x_spw_eye = x_spw_eye
            rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'y_spw_eye', .false.)  
            if (len_trim(rec) == 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Failed to find keyword", "y_spw_eye")
               success = .false.
               return
            end if
            
            read(rec, *, IOSTAT = istat) y_spw_eye
            if(istat /= 0) then
               call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
               call setECMessage("     line = "//trim(rec))
               success = .false.
               return
            end if
            item1%sourceT0FieldPtr%y_spw_eye = y_spw_eye
            rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'pdrop_spw_eye', .false.) 
            if (len_trim(rec) == 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Failed to find keyword", "pdrop_spw_eye")
               success = .false.
               return
            end if

            read(rec, *, IOSTAT = istat) p_drop_spw_eye
            if(istat /= 0) then
               call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
               call setECMessage("     line = "//trim(rec))
               success = .false.
               return
            end if
            do i=1, n_rows-1
               read(fileReaderPtr%fileHandle, *, IOSTAT = istat) (item1%sourceT0FieldPtr%arr1dPtr(i*n_cols+j), j=1, n_cols-1)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
                  return
               end if
            end do
            do i=1, n_rows-1
               read(fileReaderPtr%fileHandle, *, IOSTAT = istat) (item2%sourceT0FieldPtr%arr1dPtr(i*n_cols+j), j=1, n_cols-1)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
                  return
               end if
            end do
            do i=1, n_rows-1
               read(fileReaderPtr%fileHandle, *, IOSTAT = istat) (item3%sourceT0FieldPtr%arr1dPtr(i*n_cols+j), j=1, n_cols-1)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
                  return
               end if
            end do
            ! Fill the central point. (the first row)
            item1%sourceT0FieldPtr%arr1dPtr(1:n_cols) = 0.0_hp
            item2%sourceT0FieldPtr%arr1dPtr(1:n_cols) = item2%sourceT0FieldPtr%arr1dPtr(n_cols+1:n_cols+n_cols)
            item3%sourceT0FieldPtr%arr1dPtr(1:n_cols) = p_drop_spw_eye
            ! Fill 360 degrees with values of 0 degrees. (copy first column into last column)
            do i=1, n_rows
               item1%sourceT0FieldPtr%arr1dPtr(i*n_cols) = item1%sourceT0FieldPtr%arr1dPtr(1+(i-1)*n_cols)
               item2%sourceT0FieldPtr%arr1dPtr(i*n_cols) = item2%sourceT0FieldPtr%arr1dPtr(1+(i-1)*n_cols)
               item3%sourceT0FieldPtr%arr1dPtr(i*n_cols) = item3%sourceT0FieldPtr%arr1dPtr(1+(i-1)*n_cols)
            end do
            ! Compensate for unit of pressure (mbar versus Pa)
            if (trim(item3%quantityPtr%units) == 'mbar') then
               do i=1, size(item3%sourceT0FieldPtr%arr1dPtr)
                  item3%sourceT0FieldPtr%arr1dPtr(i) = item3%sourceT0FieldPtr%arr1dPtr(i)*100.0_hp
               end do
            end if
         ! ===== T1 =====
         else if(t0t1 == 1) then
            item1%sourceT1FieldPtr%timesteps = ecSupportThisTimeToTimesteps(fileReaderPtr%tframe, time_steps)
            item2%sourceT1FieldPtr%timesteps = ecSupportThisTimeToTimesteps(fileReaderPtr%tframe, time_steps)
            item3%sourceT1FieldPtr%timesteps = ecSupportThisTimeToTimesteps(fileReaderPtr%tframe, time_steps)
            rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'x_spw_eye', .false.) 
            
            read(rec, *, IOSTAT = istat) x_spw_eye
            if(istat /= 0) then
               call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
               call setECMessage("     line = "//trim(rec))
               success = .false.
               return
            end if

            item1%sourceT1FieldPtr%x_spw_eye = x_spw_eye
            rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'y_spw_eye', .false.) 
            
            read(rec, *, IOSTAT = istat) y_spw_eye
            if(istat /= 0) then
               call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
               call setECMessage("     line = "//trim(rec))
               success = .false.
               return
            end if
            
            item1%sourceT1FieldPtr%y_spw_eye = y_spw_eye
            rec = ecSpiderwebAndCurviFindInFile(fileReaderPtr%fileHandle, 'pdrop_spw_eye', .false.)  
            
            read(rec, *, IOSTAT = istat) p_drop_spw_eye
            if(istat /= 0) then
               call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
               call setECMessage("     line = "//trim(rec))
               success = .false.
               return
            end if
            
            do i=1, n_rows-1
               read(fileReaderPtr%fileHandle, *, IOSTAT = istat) (item1%sourceT1FieldPtr%arr1dPtr(i*n_cols+j), j=1, n_cols-1)
            if(istat /= 0) then
               call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
               call setECMessage("     line = "//trim(rec))
               success = .false.
               return
            end if
            end do
            do i=1, n_rows-1
               read(fileReaderPtr%fileHandle, *, IOSTAT = istat) (item2%sourceT1FieldPtr%arr1dPtr(i*n_cols+j), j=1, n_cols-1)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
                  return
               end if
            end do
            do i=1, n_rows-1
               read(fileReaderPtr%fileHandle, *, IOSTAT = istat) (item3%sourceT1FieldPtr%arr1dPtr(i*n_cols+j), j=1, n_cols-1)
               if(istat /= 0) then
                  call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                  call setECMessage("     line = "//trim(rec))
                  success = .false.
                  return
               end if
            end do
            ! Fill the central point.
            item1%sourceT1FieldPtr%arr1dPtr(1:n_cols) = 0.0_hp
            item2%sourceT1FieldPtr%arr1dPtr(1:n_cols) = item2%sourceT1FieldPtr%arr1dPtr(n_cols+1:n_cols+n_cols)
            item3%sourceT1FieldPtr%arr1dPtr(1:n_cols) = p_drop_spw_eye
            ! Fill 360 degrees with values of 0 degrees.
            do i=1, n_rows
               item1%sourceT1FieldPtr%arr1dPtr(i*n_cols) = item1%sourceT1FieldPtr%arr1dPtr(1+(i-1)*n_cols)
               item2%sourceT1FieldPtr%arr1dPtr(i*n_cols) = item2%sourceT1FieldPtr%arr1dPtr(1+(i-1)*n_cols)
               item3%sourceT1FieldPtr%arr1dPtr(i*n_cols) = item3%sourceT1FieldPtr%arr1dPtr(1+(i-1)*n_cols)
            end do
            ! Compensate for unit of pressure (mbar versus Pa)
            if (trim(item3%quantityPtr%units) == 'mbar') then
               do i=1, size(item3%sourceT1FieldPtr%arr1dPtr)
                  item3%sourceT1FieldPtr%arr1dPtr(i) = item3%sourceT1FieldPtr%arr1dPtr(i)*100.0_hp
               end do
            end if
         else
            call setECMessage("ERROR: ec_filereader_read::ecSpiderwebReadBlock: Invalid Field specified.")
            success = .false.
         end if
      end function ecSpiderwebReadBlock
      
      ! =======================================================================
      
      !> Read the next record from a NetCDF file.
      function ecNetcdfReadNextBlock(fileReaderPtr, item, t0t1) result(success)
         use netcdf
         !
         logical                      :: success       !< function status
         type(tEcFileReader), pointer :: fileReaderPtr !< intent(in)
         type(tEcItem)                :: item          !< Item containing quantity1, intent(inout)
         integer                      :: t0t1          !< read into Field T0 or T1 (0,1).
         !
         type(tEcField), pointer                 :: fieldPtr         !< Field to update
         integer                                 :: ierror           !< return status of NetCDF method call
         integer                                 :: varid            !< NetCDF id of NetCDF variable
         integer                                 :: ndims            !< NetCDF variable's number of dimensions
         integer,  dimension(3)                  :: dimids           !< NetCDF variable's dimension ids
         integer                                 :: length           !< size of a NetCDF variable's dimension
         integer                                 :: times_index      !< Index in tEcTimeFrame's times array
         real(hp)                                :: netcdf_timesteps !< seconds since k_refdate
         integer                                 :: i, j             !< loop counters
         real(hp), dimension(:,:,:), allocatable :: data_block       !< 2D slice of NetCDF variable's data
         integer                                 :: istat            !< allocation status
         real(hp)                                :: time_window      !< time window between times(i) and times(i+1)
         real(hp)                                :: dmiss_nc         !< local netcdf missing
         !
         success = .false.
         fieldPtr => null()

!        ierror = nf90_inq_varid(fileReaderPtr%fileHandle, trim(item%quantityPtr%name), varid)           ! inquire a varid, given the variable name in the quantityname 

         ! With the the quantity name interpreted as a standard name, inquire from the filereader instance the varid 
         do varid=1,size(fileReaderPtr%standard_names)
            if (fileReaderPtr%standard_names(varid)==item%quantityPtr%name) then 
               exit 
            endif 
         enddo 
         if (varid>size(fileReaderPtr%standard_names)) then 
            ! ERROR: standard name not found in this filereader, TODO: handle exception 
            return 
         endif 
         
         ierror = nf90_get_att(fileReaderPtr%fileHandle, varid, "_FillValue", dmiss_nc)
         times_index = ec_undef_int
         !
         ! =============
         ! sanity checks
         ! =============
         !
         ! - 1 - Source T0 or T1 Field specified
         if (t0t1 == 0) then
            fieldPtr => item%sourceT0FieldPtr
         else if (t0t1 == 1) then
            fieldPtr => item%sourceT1FieldPtr
         else
            call setECMessage("Invalid source Field specified in ecNetcdfReadNextBlock.")
            return
         end if
         !
         ! - 2 - There is no convention in dimension order, so only support scalar, (/x, y, time/) and (/latitude, longitude, time/)
         !       TODO: This check on dimension length is not infallable. Can be replaced by check on standard_name.
         if (item%elementSetPtr%nCoordinates > 0) then
            ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, varid, ndims=ndims)
            if (ndims == 3) then
               ierror = nf90_inquire_variable(fileReaderPtr%fileHandle, varid, dimids=dimids)
               ! x or latitude
               ierror = nf90_inquire_dimension(fileReaderPtr%fileHandle, dimids(1), len=length)
               if (length /= item%elementSetPtr%n_cols) then
                  call setECMessage("NetCDF variable with unsupported dimension ordering in "//trim(fileReaderPtr%filename)//".")
                  return
               end if
               ! y or longitude
               ierror = nf90_inquire_dimension(fileReaderPtr%fileHandle, dimids(2), len=length)
               if (length /= item%elementSetPtr%n_rows) then
                  call setECMessage("NetCDF variable with unsupported dimension ordering in "//trim(fileReaderPtr%filename)//".")
                  return
               end if
               ! time
               ierror = nf90_inquire_dimension(fileReaderPtr%fileHandle, dimids(3), len=length)
               if (length /= fileReaderPtr%tframe%nr_timesteps) then
                  call setECMessage("NetCDF variable with unsupported dimension ordering in "//trim(fileReaderPtr%filename)//".")
                  return
               end if
            else
               call setECMessage("NetCDF variable with unsupported number of dimensions in "//trim(fileReaderPtr%filename)//".")
               return
            end if
         end if
         !
         ! - 3 - Check for the presence of times, indicating the presence of further data blocks.
         if (fileReaderPtr%tframe%nr_timesteps == ec_undef_int .or. fileReaderPtr%tframe%nr_timesteps <= 0.0_hp) then
            call setECMessage("Empty NetCDF time dimension in "//trim(fileReaderPtr%filename)//".")
            return
         end if
         !
         ! ===================
         ! update source Field
         ! ===================
         !
         ! - 1 - Determine the relevant time entry from the times array and its index.
         if (fieldPtr%timesteps == ec_undef_hp) then
            times_index = 1
         else
            do i=1, int(fileReaderPtr%tframe%nr_timesteps)
               ! Convert times(i) * ec_timestep_unit since ec_refdate to seconds since k_refdate.
               netcdf_timesteps = ecSupportTimeToTimesteps(fileReaderPtr%tframe, i)
               ! field timesteps < NetCDf timesteps => read this block
               if (comparereal(fieldPtr%timesteps, netcdf_timesteps) == -1) then
                  times_index = i
                  exit
               end if
            end do
         end if
         !
         ! - 2 - Update the source Field's timesteps variable.
         if (times_index /= ec_undef_int) then
            fieldPtr%timesteps = ecSupportTimeToTimesteps(fileReaderPtr%tframe, times_index)
         else
            call setECMessage("Data block requested outside valid time window in "//trim(fileReaderPtr%filename)//".")
            return
         end if
         !
         ! - 3 - Read a scalar data block.
         if (item%elementSetPtr%nCoordinates == 0) then
            ierror = nf90_get_var(fileReaderPtr%fileHandle, varid, fieldPtr%arr1dPtr, start=(/times_index/), count=(/1/))
         end if
         !
         ! - 4 - Read a grid data block.
         if (item%elementSetPtr%nCoordinates > 0) then
            allocate(data_block( item%elementSetPtr%n_cols, item%elementSetPtr%n_rows, 1 ), stat = istat)
            ierror = nf90_get_var(fileReaderPtr%fileHandle, varid, data_block, start=(/1, 1, times_index/), count=(/item%elementSetPtr%n_cols, item%elementSetPtr%n_rows, 1/))
            
            ! copy data to source Field's 1D array, store (X1Y1, X1Y2, ..., X1Yn_rows, X2Y1, XYy2, ..., Xn_colsY1, ...)
            if (trim(item%quantityPtr%name) == 'Rainfall' .or. trim(item%quantityPtr%name) == 'rainfall' .or. trim(item%quantityPtr%name)=='precipitation') then                          ! str_upper(item%quantityPtr%name)=='RAINFALL' 
               ! Data must be converted here to rainfall per day for FM.
               time_window = 1.d0 
               select case (item%quantityPtr%units)
               case ('MM')
                  if (comparereal(1.0_hp*times_index, fileReaderPtr%tframe%nr_timesteps) == 0) then
                     time_window = fileReaderPtr%tframe%times(times_index) - fileReaderPtr%tframe%times(times_index - 1)
                  else
                     time_window = fileReaderPtr%tframe%times(times_index + 1) - fileReaderPtr%tframe%times(times_index)
                  end if
                  if (fileReaderPtr%tframe%ec_timestep_unit == ec_second) then
                     time_window = time_window / (60.0_hp * 60.0_hp * 24.0_hp)
                  else if (fileReaderPtr%tframe%ec_timestep_unit == ec_minute) then
                     time_window = time_window / (60.0_hp * 24.0_hp)
                  else if (fileReaderPtr%tframe%ec_timestep_unit == ec_hour) then
                     time_window = time_window / 24.0_hp
                  else
                     call setECMessage("Unknown time unit encountered in "//trim(fileReaderPtr%filename)//".")
                     return
                  end if
                  ! In future, this is the location for conversion from a variety of rainfall units to the only
                  ! accepted unit for rainfal intensity mm/day  
               end select 
               if (comparereal(time_window, 0.0_hp) == 0) then
                  call setECMessage("Empty time window leads to zero division error in "//trim(fileReaderPtr%filename)//".")
                  return
               end if
               do i=1, item%elementSetPtr%n_rows
                  do j=1, item%elementSetPtr%n_cols
                     if (data_block(j,i,1) == dmiss_nc) then 
                        fieldPtr%arr1dPtr( (i-1)*item%elementSetPtr%n_cols + j ) = 0d0
                     else                     
                        fieldPtr%arr1dPtr( (i-1)*item%elementSetPtr%n_cols + j ) = data_block(j,i,1) / time_window
                     endif
                  end do
               end do
            else
               do i=1, item%elementSetPtr%n_rows
                  do j=1, item%elementSetPtr%n_cols
                     if (data_block(j,i,1) == dmiss_nc) then 
                        fieldPtr%arr1dPtr( (i-1)*item%elementSetPtr%n_cols + j ) = 0d0
                     else                     
                        fieldPtr%arr1dPtr( (i-1)*item%elementSetPtr%n_cols + j ) = data_block(j,i,1)
                     endif
                  end do
               end do
            end if
            if (allocated(data_block)) deallocate(data_block, stat = istat)
         end if
         !
         success = .true.
      end function ecNetcdfReadNextBlock
      
      ! =======================================================================
      
      !> Read the next record from a NetCDF file.
      !! meteo1: readnetcdfblock
      ! TODO: cleanup: lastReadTime, TimeFrame usage, time conversions, remove hardcoded asumption of file content and structure
      function ecNetcdfReadBlock(fileReaderPtr, item1, t0t1, n) result(success)
         use netcdf
         !
         logical                         :: success       !< function status
         type(tEcFileReader), pointer    :: fileReaderPtr !< intent(in)
         type(tEcItem),       pointer    :: item1         !< Item containing quantity1, intent(inout)
         integer                         :: t0t1          !< read into Field T0 or T1 (0,1). -1: choose yourself and return where you put it.
         integer,             intent(in) :: n             !< dimension of quantity to read
         !
         integer                             :: i             !< loop counter
         integer                             :: j             !< loop counter
         integer                             :: ierror        !< return value of function calls
         integer                             :: iddim_time    !< id as obtained from NetCDF
         integer                             :: idvar_time    !< id as obtained from NetCDF
         integer                             :: idvar_q       !< id as obtained from NetCDF
         integer                             :: ntimes        !< number of times on the NetCDF file
         integer                             :: read_index    !< index of field to read
         logical                             :: local_success !< when the return flag should not be influenced
         real(hp), dimension(:), allocatable :: times         !< time array read from NetCDF
         character(len=maxNameLen)           :: rec           !< helper variable
         character(NF90_MAX_NAME)            :: string        !< to catch NetCDF messages
         !
         success = .false.
         !
         ierror = nf90_sync(fileReaderPtr%fileHandle)
         ierror = nf90_inq_dimid(fileReaderPtr%fileHandle, 'time', iddim_time); success = ecSupportNetcdfCheckError(ierror, "inq_dimid time"    , fileReaderPtr%fileName)
         ierror = nf90_inquire_dimension(fileReaderPtr%fileHandle, iddim_time, string, ntimes); success = ecSupportNetcdfCheckError(ierror, "inq_dim time", fileReaderPtr%fileName)
         ierror = nf90_inq_varid(fileReaderPtr%fileHandle, 'time', idvar_time); success = ecSupportNetcdfCheckError(ierror, "inq_varid time", fileReaderPtr%fileName)
         !
         !
         ! varid: First compare name with standard_names
         idvar_q = -1
         do i=1,size(fileReaderPtr%standard_names)
            if (fileReaderPtr%standard_names(i)==item1%quantityPtr%name) then
               idvar_q = i
               exit 
            endif 
         enddo
         !
         ! varid not found: compare name with variable_names
         if (idvar_q == -1) then
            do i=1,size(fileReaderPtr%variable_names)
               if (fileReaderPtr%variable_names(i)==item1%quantityPtr%name) then
                  idvar_q = i
                  exit 
               endif 
            enddo
         endif
         !
         ! varid not found: get it via nf90_inq_varid
         if (idvar_q == -1) then
            ierror = nf90_inq_varid(fileReaderPtr%fileHandle, item1%quantityPtr%name, idvar_q); success = ecSupportNetcdfCheckError(ierror, "inq_varid "//item1%quantityPtr%name, fileReaderPtr%fileName)
         endif
         !
         ! TODO: replace times by filereaderPtr%tframe%times
         allocate (times(ntimes), stat=ierror)
         if (ierror /= 0) then
            call setECMessage("Allocation error in ec_filereader_read::ecNetcdfReadBlock.")
            return
         endif
         ierror = nf90_get_var(fileReaderPtr%fileHandle, idvar_time, times, start=(/1/), count=(/ntimes/)); success = ecSupportNetcdfCheckError(ierror, "get_var time", fileReaderPtr%fileName)
         !
         ! Search in times the first time bigger than lastReadTime
         !
         read_index = -1
         if (comparereal(fileReaderPtr%lastReadTime,ec_undef_hp) == 0) then
            !
            ! No data read at all. Force reading for the first time
            read_index = 1
         else
            do i=1, ntimes
               if (comparereal(times(i),fileReaderPtr%lastReadTime) == 1) then
                  read_index = i
                  exit
               endif
            enddo
         endif
         if (read_index > 0) then
            if (t0t1 < 0) then
               if (comparereal(item1%sourceT0FieldPtr%timesteps,0.0_hp) == -1) then
                  t0t1 = 0
               elseif (comparereal(item1%sourceT1FieldPtr%timesteps,0.0_hp) == -1) then
                  t0t1 = 1
               elseif (comparereal(item1%sourceT0FieldPtr%timesteps,item1%sourceT1FieldPtr%timesteps) /= 1) then
                  t0t1 = 0
               else
                  t0t1 = 1
               endif
            endif
            !
            ! T0
            if (t0t1==0) then
               item1%sourceT0FieldPtr%timesteps = times(read_index)
               ierror = nf90_get_var(fileReaderPtr%fileHandle, idvar_q, item1%sourceT0FieldPtr%arr1dPtr, start=(/ 1, read_index /), count = (/ n, 1 /)) 
               success = ecSupportNetcdfCheckError(ierror, "get_var "//item1%quantityPtr%name, fileReaderPtr%fileName)
            ! ===== T1 =====
            else if(t0t1==1) then
               item1%sourceT1FieldPtr%timesteps = times(read_index)
               ierror = nf90_get_var(fileReaderPtr%fileHandle, idvar_q, item1%sourceT1FieldPtr%arr1dPtr, start=(/ 1, read_index /), count = (/ n, 1 /))
               success = ecSupportNetcdfCheckError(ierror, "get_var "//item1%quantityPtr%name, fileReaderPtr%fileName)
            else
               call setECMessage("ecNetcdfReadBlock: Invalid Field specified.")
            endif
            fileReaderPtr%lastReadTime = times(read_index)
         else
            success = .false.
         endif
         deallocate (times, stat=ierror)
      end function ecNetcdfReadBlock
      
      ! =======================================================================
      
      !> Read the Qhtable file.
      function ecQhtableReadAll(fileReaderPtr, discharges, waterlevels, nr_rows) result(success)
         logical                                                     :: success       !< function status
         type(tEcFileReader),                            pointer     :: fileReaderPtr !< intent(in)
         real(hp),            dimension(:), allocatable, intent(out) :: discharges    !< 
         real(hp),            dimension(:), allocatable, intent(out) :: waterlevels   !< 
         integer,                                        intent(out) :: nr_rows       !< 
         !
         integer        :: istat !< status of operation
         character(132) :: rec   !< content of a line
         !
         success = .true.
         nr_rows = 0
         !
         allocate(discharges(10), waterlevels(10), STAT = istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_filereader_read::ecQhtableReadAll: Unable to allocate additional memory.")
            return
         end if
         !
         if (fileReaderPtr%ofType == provFile_qhtable) then 
            rewind(unit=fileReaderPtr%fileHandle)
         endif
         !
         do
            if (fileReaderPtr%ofType == provFile_bc) then 
               if (.not.ecBCReadLine(fileReaderPtr, recout=rec)) then 
                  ! TODO (RL): insert real message handling/reporting here (deltarescommon message)
                  if (fileReaderPtr%end_of_data) then           ! legitimate way to exit, data simply ended  
                     istat = 0
                  else                    ! reading failed but not eof! something wrong
                     istat = -666      
                     success = .false. 
                  endif 
                  return 
               endif 
            endif 
            if (fileReaderPtr%ofType == provFile_qhtable) then 
               read (fileReaderPtr%fileHandle,'(a)', IOSTAT = istat) rec
            endif 
            if (istat == 0) then
               if (.not. (rec(1:1) == '*' .or. len_trim(rec) == 0)) then
                  if (nr_rows == size(discharges)) then
                     call realloc(discharges, nr_rows + 10, STAT = istat, keepExisting = .true.)
                     if (istat /= 0) then
                        call setECMessage("ERROR: ec_filereader_read::ecQhtableReadAll: Unable to allocate additional memory.")
                        success = .false.
                        exit
                     end if
                     call realloc(waterlevels, nr_rows + 10, STAT = istat, keepExisting = .true.)
                     if (istat /= 0) then
                        call setECMessage("ERROR: ec_filereader_read::ecQhtableReadAll: Unable to allocate additional memory.")
                        success = .false.
                        exit
                     end if
                  end if
                  nr_rows = nr_rows + 1
                  read (rec, *, iostat = istat) discharges(nr_rows), waterlevels(nr_rows)
                  if (istat /= 0) then 
                      success = .false.
                      call setECMessage("ERROR: ec_filereader_read::ecQhtableReadAll: Cannot find two numbers in line: "//trim(rec)//" in file: "//	trim(fileReaderPtr%FILENAME))
                      exit 
                  end if     
               end if
            else
               exit
            end if
         end do
      end function ecQhtableReadAll
      
      ! =======================================================================
      
      !> Read the Fourier file, transforming components into periods.
      !! meteo1: readfourierdims, readfouriercompstim
      function ecFourierReadAll(fileReaderPtr, periods, components, magnitudes, phases, nPeriods) result(success)
         logical                                                     :: success       !< function status
         type(tEcFileReader),                            pointer     :: fileReaderPtr !< intent(in)
         real(hp),            dimension(:), allocatable, intent(out) :: periods       !< periods in minutes, directly or converted from components
         character(len=8),    dimension(:), allocatable, intent(out) :: components    !< astro component names
         real(hp),            dimension(:), allocatable, intent(out) :: magnitudes    !< seed values for the magnitudes of the Fourier components
         real(hp),            dimension(:), allocatable, intent(out) :: phases        !< seed values for the phases of the Fourier components (in deg, output in rad)
         integer,                                        intent(out) :: nPeriods      !< number of periods
         
         !
         integer                   :: istat     !< status of allocation operation
         character(132)            :: rec       !< content of a line
         integer                   :: reclen    !< record length minus comment 
         integer                   :: i1        !< start index of first word
         integer                   :: i2        !< stop index of first word
         character(len=maxNameLen) :: component !< helper variable, when converting from component to period
         real(hp)                  :: dummy_amplitude, dummy_phase
         logical                   :: eof       !< true if the end of file was reached 
         logical                   :: is_corr   !< true if the fourier data is an astronomic/harmonic correction
         logical                   :: is_astro  !< true if an astronomical component has been parsed

         real(hp)                  :: magdum, phasedum, period, ampl, shift
         integer                   :: MAXCMP = 100 
         !
         success = .true.
         nPeriods = 0
         !
         allocate(periods(10), components(10), magnitudes(10), phases(10), STAT = istat)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_filereader_read::ecFourierReadAll: Unable to allocate additional memory.")
            return
         end if

         if (fileReaderPtr%ofType == provFile_fourier) then 
            rewind(unit=fileReaderPtr%fileHandle)
         endif
         !

         if (fileReaderPtr%ofType == provFile_bc) then 
            if (allocated(fileReaderPtr%bc%quantity%astro_component)) deallocate (fileReaderPtr%bc%quantity%astro_component)
            if (allocated(fileReaderPtr%bc%quantity%astro_amplitude)) deallocate (fileReaderPtr%bc%quantity%astro_amplitude)
            if (allocated(fileReaderPtr%bc%quantity%astro_phase)) deallocate (fileReaderPtr%bc%quantity%astro_phase)
            allocate (fileReaderPtr%bc%quantity%astro_component(MAXCMP))
            allocate (fileReaderPtr%bc%quantity%astro_amplitude(MAXCMP))
            allocate (fileReaderPtr%bc%quantity%astro_phase(MAXCMP))
         endif 

         is_astro=.false.
         do
            if (fileReaderPtr%ofType == provFile_bc) then 
               if (.not.ecBCReadLine(fileReaderPtr, recout=rec)) then 
                  ! TODO (RL): insert real message handling/reporting here (deltarescommon message)
                  istat = -666   
                  success = fileReaderPtr%end_of_data        ! if reading failed, allow only if at end of data
                  exit
               else
                  istat = 0
               endif 
            endif 
            if (fileReaderPtr%ofType == provFile_fourier) then 
               read (fileReaderPtr%fileHandle,'(a)', IOSTAT = istat) rec
            endif 
            if (istat == 0) then
               if (.not. (rec(1:1) == '*' .or. len_trim(rec) == 0)) then
                  if (nPeriods == size(periods)) then
                     call realloc(periods, nPeriods + 10, STAT = istat, keepExisting = .true.)
                     if (istat == 0) call realloc(components, nPeriods + 10, STAT = istat, keepExisting = .true.)
                     if (istat == 0) call realloc(magnitudes, nPeriods + 10, STAT = istat, keepExisting = .true.)
                     if (istat == 0) call realloc(phases, nPeriods + 10, STAT = istat, keepExisting = .true.)
                     if (istat /= 0) then
                        call setECMessage("ERROR: ec_filereader_read::ecFourierReadAll: Unable to allocate additional memory.")
                        success = .false.
                        return
                     end if
                  end if
                  nPeriods = nPeriods + 1
                  call remove_leading_spaces(rec) ! Prevents scientific notation number to be identified as a component name.
                  call find_first_word(rec, i1, i2)
                  if ((i1 == 0 .and. i2 == 0) .or. (i1 > 3)) then
                     ! period found
                     read (rec, *, IOSTAT = istat) periods(nPeriods), magnitudes(nPeriods), phases(nPeriods)
                     if(istat /= 0) then
                        call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                        call setECMessage("     line = "//trim(rec))
                        success = .false.
                        return
                     end if
                     ! 
                     if(is_astro) then
                        call setECMessage("ERROR: mixed astro-components/harmonic components encountered.")
                        success = .false.
                        return
                     end if
                     ! Perform transformations, which are handled by subroutine asc for components.
                     if (.not. (comparereal(periods(nPeriods), 0.0_hp) == 0)) then
                        ! if a bc-structure is associated to the filereader (i.e. we are reading from a BC-file),
                        ! inspect the 'timeunit' in which info was stored on the contents of the 'component'-column 
                        if (associated(fileReaderPtr%bc)) then  
                           select case (fileReaderPtr%bc%timeunit)
                              case ('SECOND')
                                 periods(nPeriods) = 2.0_hp * pi_hp / (periods(nPeriods)/60.0)
                              case ('MINUTTE')
                                 periods(nPeriods) = 2.0_hp * pi_hp / (periods(nPeriods))
                              case ('HOUR')
                                 periods(nPeriods) = 2.0_hp * pi_hp / (periods(nPeriods)*60.0)
                              case ('PERSECOND')
                                 periods(nPeriods) = 2.0_hp * pi_hp * (periods(nPeriods)/60.0)
                              case ('PERMINUTE')
                                 periods(nPeriods) = 2.0_hp * pi_hp * (periods(nPeriods))
                              case ('PERHOUR')
                                 periods(nPeriods) = 2.0_hp * pi_hp * (periods(nPeriods)*60.0)
                              case ('RADPERSECOND')
                                 periods(nPeriods) = periods(nPeriods)/60.0
                              case ('RADPERMINUTE')
                                 periods(nPeriods) = periods(nPeriods)
                              case ('RADPERHOUR')
                                 periods(nPeriods) = periods(nPeriods)*60.0
                              case default                                       ! old setting 
                                 periods(nPeriods) = 2.0_hp * pi_hp / periods(nPeriods)
                           end select 
                        else 
                           periods(nPeriods) = 2.0_hp * pi_hp / periods(nPeriods)
                        end if
                     end if
                     phases(nPeriods) = phases(nPeriods)*degrad_hp
                     is_astro = .false.
                  else
                     ! component found
                     read (rec, *, IOSTAT = istat) component, magnitudes(nPeriods), phases(nPeriods)
                     if(istat /= 0) then
                        call setECMessage("ec_filereader_read::ecUniReadBlock: Read failure before end of file: "//trim(fileReaderPtr%fileName))
                        call setECMessage("     line = "//trim(rec))
                        success = .false.
                        return
                     end if
                     !
                     components(nPeriods) = trim(component)
                     phases(nPeriods) = phases(nPeriods)*degrad_hp
                     ! store the original component parameters, read from file, into a bc%quantity  
                     if (fileReaderPtr%ofType == provFile_bc) then 
                        fileReaderPtr%bc%quantity%astro_component(nPeriods) = components(nPeriods)
                        fileReaderPtr%bc%quantity%astro_amplitude(nPeriods) = magnitudes(nPeriods)                        
                        fileReaderPtr%bc%quantity%astro_phase(nPeriods) = phases(nPeriods)
                     endif 
                     is_astro = .true.
                  end if
               end if
            else
               exit
            end if
         end do
         
         ! truncate the period amplitude and phase arrays to the actual sizes (processed components)
         if(is_astro) then
            deallocate(periods)
            call realloc(components, nPeriods, STAT = istat, keepExisting = .true.)
         else
            deallocate(components)
            call realloc(periods, nPeriods, STAT = istat, keepExisting = .true.)
         end if
         if (istat == 0) call realloc(magnitudes, nPeriods, STAT = istat, keepExisting = .true.)
         if (istat == 0) call realloc(phases, nPeriods, STAT = istat, keepExisting = .true.)
         if (istat /= 0) then
            call setECMessage("ERROR: ec_filereader_read::ecFourierReadAll: Unable to allocate actual memory (components).")
            success = .false.
            return
         end if

         ! truncate the period amplitude and phase arrays to the actual sizes (original components in bc-instance)
         if (fileReaderPtr%ofType == provFile_bc) then 
            call realloc(fileReaderPtr%bc%quantity%astro_component, nPeriods, STAT = istat, keepExisting = .true.)
            call realloc(fileReaderPtr%bc%quantity%astro_amplitude, nPeriods, STAT = istat, keepExisting = .true.)
            call realloc(fileReaderPtr%bc%quantity%astro_phase, nPeriods, STAT = istat, keepExisting = .true.)
            if (istat /= 0) then
               call setECMessage("ERROR: ec_filereader_read::ecFourierReadAll: Unable to allocate actual memory (original components).")
               success = .false.
               return
            endif
         endif 

      end function ecFourierReadAll
      
      ! =======================================================================
      
      !> Read the file from the current line untill a line containing the keyword is found and read.
      !! meteo1.f90: reaspwheader
      function ecFindInFile(minp, keyword) result(rec)
         character(maxNameLen)                 :: rec
         integer                  , intent(in) :: minp    !< IO unit number
         character(*)             , intent(in) :: keyword !< keyword to find
         !
         ! locals
         integer               :: istat !< status of read operation
         integer               :: indx  !< helper index variable
         character(maxNameLen) :: rec_small
         character(maxNameLen) :: keyword_small
         !
         ! body
         rec = ' '
         keyword_small = keyword
         call small(keyword_small, len(keyword_small))
         do
            ! Infinite read loop until keyword found or EOF
            read(minp, '(a)', IOSTAT = istat) rec
            rec_small = rec
            call small(rec_small, len(rec_small))
            if (istat == 0) then
               !if (.not. (rec_small(1:1) == '*' .or. rec_small(1:1) == '#' .or. len_trim(rec_small) == 0)) then
               if (.not. (rec_small(1:1) == '*'                            .or. len_trim(rec_small) == 0)) then
                  if (index(rec_small, trim(keyword_small)) /= 0) then
                     exit ! Jump out of do-loop
                  end if
               end if
            else
               call setECMessage("INFO: ec_filereader_read::ecFindInFile: File end has been reached.")
               rec = ' '
               exit ! Jump out of do-loop
            end if
         enddo
      end function ecFindInFile
      
      ! =======================================================================
      
      !> In a spiderweb or curvi file, find the value curresponding to the specified keyword.
      !! meteo1.f90: reaspwheader
      function ecSpiderwebAndCurviFindInFile(minp, keyword, do_rewind) result(answer)
         character(len=20)                     :: answer
         integer,                   intent(in) :: minp      !< IO unit number
         character(len=*),          intent(in) :: keyword   !< keyword to find
         logical, optional,         intent(in) :: do_rewind !< rewind file before search        
         !
         character(len=maxNameLen) :: word
         character(len=maxNameLen) :: rec     !< content of read line
         integer                   :: istat   !< status of read operation
         integer                   :: indx    !< helper index variable
         !
         answer = ' '
         word = keyword
         if (present(do_rewind)) then
            if (do_rewind) then
               rewind(unit = minp)
            end if
         else
            rewind(unit = minp)
         end if
         !
         rec = ecFindInFile(minp, word)
         indx = index(rec, '=')
         if (indx /= 0) then
            read(rec(indx+1:indx+20),"(A20)", iostat = istat) answer
            if (istat /= 0) then
               call setECMessage("ERROR: ec_filereader_read::ecSpiderwebAndCurviFindInFile: Failed to read an existing line.")
               answer = '                    '
            end if
         end if
      end function ecSpiderwebAndCurviFindInFile
      
      ! =======================================================================
      
      !> In a t3D file, find the list of values following the specified keyword.
      function ect3DFindInFile(minp, keyword, do_rewind) result(answer)
         character(len=1000)        :: answer
         integer,                   intent(in) :: minp      !< IO unit number
         character(len=*),          intent(in) :: keyword   !< keyword to find
         logical, optional,         intent(in) :: do_rewind !< rewind file before search        
         !
         character(len=maxNameLen) :: word
         character(len=maxNameLen) :: rec     !< content of read line
         integer                   :: istat   !< status of read operation
         integer                   :: indx    !< helper index variable
         !
         answer = ''
         word = keyword
         if (present(do_rewind)) then
            if (do_rewind) then
               rewind(unit = minp)
            end if
         else
            rewind(unit = minp)
         end if
         !
         rec = ecFindInFile(minp, word)
         indx = index(rec, '=')
         answer = rec(indx+1:)
      end function ect3DFindInFile
         
      ! =======================================================================
      
      !> Rewind and then read past the header of a spiderweb file, putting it is a state suitable for calling ecSpiderwebReadBlock.
      function ecSpiderAndCurviAndArcinfoReadToBody(minp) result(success)
         logical             :: success
         integer, intent(in) :: minp !< IO unit number
         !
         character(len=20) :: answer
         !
         success = .false.
         rewind(unit=minp)
         answer = ecFindInFile(minp, "TIME")
         if (len_trim(answer) > 0) then
            success = .true.
            backspace(minp) ! We wanted to read to the end of the header.
         end if
      end function ecSpiderAndCurviAndArcinfoReadToBody
      
      ! =======================================================================

      function ecSampleReadAll(fileReaderPtr, xs, ys, zs, nSamples, kx) result(success)
         use m_alloc
         logical                                                       :: success       !< function status
         type(tEcFileReader),                              pointer     :: fileReaderPtr !< intent(in)
         real(hp),            dimension(:),   allocatable, intent(out) :: xs            !< list of x-coordinates of all samples
         real(hp),            dimension(:),   allocatable, intent(out) :: ys            !< list of y-coordinates of all samples
         real(hp),            dimension(:,:), allocatable, intent(out) :: zs            !< list of z-values of all samples
         integer,                                          intent(out) :: nSamples      !< number of samples
         integer,                                          intent(out) :: kx            !< number of vector components in each sample value (1 for scalars)

         integer :: msam
         double precision :: xx, yy, zz
         double precision :: dmiss_dflt = -999d0   ! Use default missing value for this 'old' sample file type
         double precision :: xymis_dflt = -999d0   !
         character(len=132) :: rec
         character(len=maxMessageLen) :: tex

         success = .true.

         msam = fileReaderPtr%fileHandle
         nSamples = 0

         rewind(msam)
11       read (msam,*, end = 31)
            nSamples = nSamples + 1
         goto 11
31       continue
         kx = 1 ! TODO: AvD: scan and support vector_max > 1
         call realloc(xs,        nSamples,    keepExisting = .false.)
         call realloc(ys,        nSamples,    keepExisting = .false.)
         call realloc(zs, (/ kx, nSamples /), keepExisting = .false.)

         rewind(msam)

         nSamples = 0
10       continue
         read (msam,'(a)',end = 30) rec
         read (rec,*,end = 40, err = 40) xx,yy,zz

         if (  xx .ne. xymis_dflt .and. yy .ne. xymis_dflt .and. &
               zz .ne. dmiss_dflt .and. zz .ne. 999.999d0 .and. &
               .not.(isnan(xx) .or. isnan(yy) .or. isnan(zz)) ) then
            nSamples = nSamples + 1
            xs(nSamples)    = xx
            ys(nSamples)    = yy
            zs(kx,nSamples) = zz
         endif
         goto 10

40       continue
         success = .false.
         write(tex,'(a,a,a,i0,a)') "ERROR: ec_filereader_read::ecSampleReadAll: read error in file '", trim(fileReaderPtr%filename), "' on line ", nSamples+1, "."
         call setECMessage(trim(tex))
         return

30       continue
!        ! TODO: Sample cleaning below not necessary in EC module?
!        write(tex,'(i10)') ns
!        call readyy('sorting '//trim(tex)//' samples points',0d0)
!        if (ns .gt. 1) then
!           call tidysamples(xs,ys,zs,ipsam,ns,mxsam,mysam)
!           call get_samples_boundingbox()
!           ipstat = ipstat_ok
!        end if
         return
      end function ecSampleReadAll

      ! =======================================================================

      
      !> Determination of FR and V0+U.
      !! 'stripped' VERSION OF MAIN (ASCON)
      !! meteo1 : asc
      subroutine asc(omeg, ampl, phas, component, idate, itime, ierrs)
         real(hp),         intent(out)   :: omeg      !< period [minute]
         real(hp),         intent(inout) :: ampl      !< amplitude [m]
         real(hp),         intent(inout) :: phas      !< phase [degree]
         character(len=8), intent(in)    :: component !< component name
         integer,          intent(in)    :: idate     !< date integer yyyymmdd
         integer,          intent(in)    :: itime     !< time integer hhmmss
         integer,          intent(out)   :: ierrs     !< number of errors
         !
         ! local
         !
         integer, parameter    :: kcmp = 1   !< 
         integer, parameter    :: mxkc = 234 !< 
         integer, dimension(6) :: jdatum     !< Date and time

         real(hp), dimension(kcmp) :: fr  !< Amplitude factors for the referenced components
         real(hp), dimension(kcmp) :: v0u !< Astronomical arguments of the referenced components [rad]
         real(hp), dimension(kcmp) :: w   !< Angular velocity of the referenced components [rad/hr]

         integer                               :: i      !< Help var.
         integer                               :: ik     !< Help var.
         integer                               :: il     !< Help var.
         integer                               :: j      !< Help var.
         integer                               :: jaar   !< Present year
         integer,           dimension(16*mxkc) :: jnaam  !< Help var.
         character(len=8),  dimension(mxkc)    :: knaam  !< Array with the names of all components
         character(len=80), dimension(mxkc)    :: kombes !< Array with tidal components
         real(hp)                              :: t      !< Time in hours referred to January 1, 00:00 of the year 'JAAR'
         real(hp),          dimension(15)      :: v      !< Help var. to calculate V0U()
         real(hp),          dimension(25)      :: f      !< Help var. to calculate FR()
         
         !
         !! executable statements -------------------------------------------------------
         !
         
         if (index(component, 'A0') /= 0) then
            omeg = 0.0_hp
            phas = 0.0_hp
            ierrs = 0
            return
         end if
         
         jdatum(1) = idate/10000
         jdatum(2) = idate/100 - 100*(idate/10000)
         jdatum(3) = idate - 100*(idate/100)
         jdatum(4) = itime/10000
         jdatum(5) = itime/100 - 100*(itime/10000)
         jdatum(6) = itime - 100*(itime/100)
         
         call kompbs(kombes)
         !
         ik = -15
         do i = 1, mxkc
            ik = ik + 16
            il = ik + 15
            read (kombes(i), '(a8,10i3,3(i1,i2))') knaam(i), (jnaam(j), j = ik, il)
         enddo
         !
         jaar = jdatum(1)
         !
         call datumi(jaar, jdatum, t)
         call hulpgr(jaar      ,t         ,v         ,f         )
         call bewvuf(ierrs     ,kcmp      ,mxkc      ,component ,knaam     , &
                   & jnaam     ,w         ,v0u       ,fr        ,v         , &
                   & f         )
    
!         omeg = (2.0_hp*pi*60.0_hp)/w(1) ! [minute]
         omeg = w(1)/60.0_hp
!         ampl = ampl * fr(1)             ! [m]
         ampl = ampl * fr(1)
!         phas = phas - v0u(1)*raddeg     ! [degree]
         phas = phas - v0u(1)
      end subroutine asc
      
      ! =======================================================================
      
      !> simulation of external kompbes-file
      !! meteo1 : kompbs
      subroutine kompbs(l)
         character(80), dimension(234), intent(out) :: l !< Array with tidal components
         !
         l(1)   = 'SA                 1                            '
         l(2)   = 'SSA                2                            '
         l(3)   = 'MSM          1  1 -2                  1 1       '
         l(4)   = 'MM           1 -1                     1 1       '
         l(5)   = 'MSF          2    -2                  1 1       '
         l(6)   = 'MS0          2    -2    -2  2         1 6       '
         l(7)   = 'MF           2          -2            1 2       '
         l(8)   = 'KO0          2          -2  1 -2-10   1 3119    '
         l(9)   = 'MK0          2          -2  2   -11   120       '
         l(10)  = 'SNU          3  1 -4    -2  2         1 6       '
         l(11)  = 'SN           3 -1 -2    -2  2         1 6       '
         l(12)  = 'MSTM         3  1 -2    -2            1 2       '
         l(13)  = 'MFM          3 -1       -2            1 2       '
         l(14)  = '2SM          4    -4    -4  4         2 6       '
         l(15)  = 'MSQM         4    -2    -2            1 2       '
         l(16)  = 'MQM          4 -2       -2            1 2       '
         l(17)  = '2SMN         5 -1 -4    -4  4         2 6       '
         l(18)  = '2OK1      1 -4     1     4 -2 -1+10   2 3119    '
         l(19)  = '2Q1       1 -4  2  1     2 -1  1      1 3       '
         l(20)  = 'NJ1       1 -4  2  1     2 -1  1      1 41 6    '
         l(21)  = 'SIGMA1    1 -4     3     2 -1  1      1 3       '
         l(22)  = 'MUK1      1 -4     3     2 -2   +10   1 6119    '
         l(23)  = 'NUJ1      1 -4     3     2 -1  1      1 41 6    '
         l(24)  = 'Q1        1 -3  1  1     2 -1  1      1 3       '
         l(25)  = 'NK1       1 -3  1  1     2 -2  1+10   1 6119    '
         l(26)  = 'RO1       1 -3 -1  3     2 -1  1      1 3       '
         l(27)  = 'NUK1      1 -3 -1  3     2 -2 +1+10   1 6119    '
         l(28)  = 'O1        1 -2     1     2 -1  1      1 3       '
         l(29)  = 'TAU1      1 -2     3       -1 -1      1 4       '
         l(30)  = 'MP1       1 -2     3     2 -2 -1      1 6       '
         l(31)  = 'M1B       1 -1 -1  1     2 -1 -1      1 3       '
         l(32)  = 'M1C       1 -1     1     1 -1         112       '
         l(33)  = 'M1A       1 -1  1  1       -1 -1      1 4       '
         l(34)  = 'M1        1 -1  1  1       -1 -1-12   121       '
         l(35)  = 'NO1       1 -1  1  1       -1 -1      1 31 6    '
         l(36)  = 'CHI1      1 -1 -1 +3       -1 -1      1 4       '
         l(37)  = 'LP1       1 -1 -1  3     2 -2  1-13   122       '
         l(38)  = 'PI1       1       -2 +1        1                '
         l(39)  = 'TK1       1       -2  1        1+10   119       '
         l(40)  = 'P1        1       -1           1                '
         l(41)  = 'SK1       1       -1           1+10   119       '
         l(42)  = 'S1        1                                     '
         l(43)  = 'K1        1        1          -1-10   119       '
         l(44)  = 'MO1       1        1       -1 -1      1 31 6    '
         l(45)  = 'SP1       1        1          -1                '
         l(46)  = 'PSI1      1        2 -1       -1                '
         l(47)  = 'RP1       1        2 -1        1                '
         l(48)  = 'FI1       1        3          -1                '
         l(49)  = 'KP1       1        3          -1-11   120       '
         l(50)  = 'THETA1    1  1  1 -1       -1 -1      1 4       '
         l(51)  = 'LABDAO1   1  1  1 -1       -1  1      1 31 6    '
         l(52)  = 'J1        1  1 -1  1       -1 -1      1 4       '
         l(53)  = 'MQ1       1  1 -1  1       -1 -1      1 31 6    '
         l(54)  = '2PO1      1  2    -3    -2  1  1      1 3       '
         l(55)  = 'SO1       1  2    -1    -2  1 -1      1 3       '
         l(56)  = 'OO1       1  2     1    -2 -1 -1      1 5       '
         l(57)  = '2KO1      1  2     1    -2  1  1-10-101 3219    '
         l(58)  = 'UPSILON1  1  3 -1  1    -2 -1  1      1 5       '
         l(59)  = 'KQ1       1  3 -1  1    -2  1 -1-11   1 3120    '
         l(60)  = '2MN2S2    2 -7  1  6     6 -6         3 6       '
         l(61)  = '3MKS2     2 -6     4     6 -6   +11   3 6120    '
         l(62)  = '2NS2      2 -6  2  4     4 -4         2 6       '
         l(63)  = '3MS2      2 -6     6     6 -6         3 6       '
         l(64)  = 'OQ2       2 -5  1  2     4 -2  2      2 3       '
         l(65)  = 'MNK2      2 -5  1  2     4 -4   +11   2 6120    '
         l(66)  = 'EPSILON2  2 -5  1  4     2 -2         1 6       '
         l(67)  = 'MNS2      2 -5  1  4     4 -4         2 6       '
         l(68)  = '2ML2S2    2 -5 -1  6     6 -6  2-13   2 6122    '
         l(69)  = 'MNUS2     2 -5 -1  6     4 -4         2 6       '
         l(70)  = 'MNK2S2    2 -5  1  6     4 -4  0-11   2 6120    '
         l(71)  = '2MS2K2    2 -4           4 -4   +11+112 6220    '
         l(72)  = 'O2        2 -4     2     4 -2  2      2 3       '
         l(73)  = 'NLK2      2 -4     2     4 -4  2+11-131 6120122 '
         l(74)  = '2MK2      2 -4     2     4 -4   +11   1 6120    '
         l(75)  = '2N2       2 -4  2  2     2 -2         1 6       '
         l(76)  = 'MU2       2 -4     4     2 -2         1 6       '
         l(77)  = '2MS2      2 -4     4     4 -4         2 6       '
         l(78)  = 'SNK2      2 -3  1        2 -2   +11   1 6120    '
         l(79)  = 'NA2       2 -3  1  1  1                         '
         l(80)  = 'N2        2 -3  1  2     2 -2         1 6       '
         l(81)  = 'KQ2       2 -3  1  2     2 -1   -10   1 3119    '
         l(82)  = 'NB2       2 -3  1  3 -1                         '
         l(83)  = 'NU2       2 -3 -1  4     2 -2         1 6       '
         l(84)  = '3MSN2     2 -3  1  6     4 -4         4 6       '
         l(85)  = '2KN2S2    2 -3  1  6     2 -2   -11-111 6220    '
         l(86)  = 'OP2       2 -2           2 -1  2      1 3       '
         l(87)  = 'MSK2      2 -2           2 -2   +11   1 6120    '
         l(88)  = 'GAMMA2    2 -2  2        2 -2  2      1 6       '
         l(89)  = 'ALFA2     2 -2     1     2 -2  2      1 6       '
         l(90)  = 'MPS2      2 -2     1     2 -2  1      1 6       '
         l(91)  = 'MA2       2 -2     1                            '
         l(92)  = 'M2        2 -2     2     2 -2         1 6       '
         l(93)  = 'KO2       2 -2     2     2 -1   -10   1 3119    '
         l(94)  = 'MSP2      2 -2     3     2 -2 -1      1 6       '
         l(95)  = 'MB2       2 -2     3                            '
         l(96)  = 'DELTA2    2 -2     4       -2  0      1 7       '
         l(97)  = 'MKS2      2 -2     4     2 -2   -11   1 6120    '
         l(98)  = 'M2(KS)2   2 -2     6     2 -2   -11-111 6220    '
         l(99)  = '2SN(MK)2  2 -1  1 -2            +11   2 6120    '
         l(100) = 'LABDA2    2 -1  1        2 -2  2      1 6       '
         l(101) = 'SNM2      2 -1  1                     2 6       '
         l(102) = '2MN2      2 -1 -1  2     2 -2         3 6       '
         l(103) = 'L2        2 -1 -1  2     2 -2  2-13   122       '
         l(104) = 'L2A       2 -1 -1  2     2 -2  2      1 6       '
         l(105) = 'L2B       2 -1  1  2       -2         1 7       '
         l(106) = '2SK2      2       -2            +11   120       '
         l(107) = 'T2        2       -1  1                         '
         l(108) = 'S2        2                                     '
         l(109) = 'KP2       2                     -10   119       '
         l(110) = 'R2        2        1 -1        2                '
         l(111) = 'K2        2        2            -11   120       '
         l(112) = 'MSNU2     2  1  1 -2                            '
         l(113) = 'MSN2      2  1 -1                     2 6       '
         l(114) = 'ZETA2     2  1  1          -2         1 7       '
         l(115) = 'ETA2      2  1 -1  2       -2         1 7       '
         l(116) = 'KJ2       2  1 -1  2       -1 -2-10   1 4119    '
         l(117) = 'MKN2      2  1 -1  2            -11   2 6120    '
         l(118) = '2KM(SN)2  2  1 -1  4            -11-112 6220    '
         l(119) = '2SM2      2  2    -2    -2  2         1 6       '
         l(120) = 'SKM2      2  2          -2  2   -11   1 6120    '
         l(121) = '2MS2N2    2  2 -2                     2 6       '
         l(122) = '2SNU2     2  3  1 -4    -2  2         1 6       '
         l(123) = '2SN2      2  3 -1 -2    -2  2         1 6       '
         l(124) = 'SKN2      2  3 -1       -2  2   -11   1 6120    '
         l(125) = 'MQ3       3 -5  1  3     4 -3  1      1 31 6    '
         l(126) = 'NO3       3 -5  1  3     4 -3  1      1 31 6    '
         l(127) = 'MO3       3 -4     3     4 -3  1      1 31 6    '
         l(128) = '2MK3      3 -4     3     4 -4  1+10   2 6119    '
         l(129) = '2MP3      3 -4     5     4 -4 -1      2 6       '
         l(130) = 'M3        3 -3     3     3 -3         117       '
         l(131) = 'NK3       3 -3  1  3     2 -2 -1-10   1 6119    '
         l(132) = 'SO3       3 -2     1     2 -1  1      1 3       '
         l(133) = 'MP3       3 -2     1     2 -2  1      1 6119    '
         l(134) = 'MK3       3 -2     3     2 -2 -1-10   1 6119    '
         l(135) = 'SP3       3       -1           1                '
         l(136) = '2MQ3      3 -1 -1  3     2 -3 -1      1 32 6    '
         l(137) = 'SK3       3        1          -1-10   119       '
         l(138) = '2SO3      3  2    -1    -2  1 -1      1 3       '
         l(139) = 'K3        3        3          -1-10-11119120    '
         l(140) = '4MS4      4 -8     8     8 -8         4 6       '
         l(141) = '2MNS4     4 -7  1  6     6 -6         3 6       '
         l(142) = '3MK4      4 -6     4     6 -6   +11   3 6120    '
         l(143) = 'MNLK4     4 -6     4     6 -6  2+11-132 6120122 '
         l(144) = '3MS4      4 -6     6     6 -6         3 6       '
         l(145) = 'MSNK4     4 -5  1  2     4 -4   +11   2 6120    '
         l(146) = 'MN4       4 -5  1  4     4 -4         2 6       '
         l(147) = 'MNU4      4 -5 -1  6     4 -4         2 6       '
         l(148) = '2MLS4     4 -5 -1  6     6 -6  2-13   2 6122    '
         l(149) = '2MSK4     4 -4     2     4 -4   +11   2 6120    '
         l(150) = 'M4        4 -4     4     4 -4         2 6       '
         l(151) = '2MKS4     4 -4     6     4 -4   -11   2 6120    '
         l(152) = 'SN4       4 -3  1  2     2 -2         1 6       '
         l(153) = '3MN4      4 -3 -1  4     4 -4         4 6       '
         l(154) = '2SMK4     4 -2           2 -2   +11   1 6120    '
         l(155) = 'MS4       4 -2     2     2 -2         1 6       '
         l(156) = 'MK4       4 -2     4     2 -2   -11   1 6120    '
         l(157) = '2SNM4     4 -1  1                     2 6       '
         l(158) = '2MSN4     4 -1 -1  2     2 -2         3 6       '
         l(159) = 'SL4       4 -1 -1  2     2 -2  2-13   122       '
         l(160) = 'S4        4                                     '
         l(161) = 'SK4       4        2            -11   120       '
         l(162) = '2SMN4     4  1 -1                     2 6       '
         l(163) = '3SM4      4  2    -2    -2  2         1 6       '
         l(164) = '2SKM4     4  2          -2  2   -11   1 6120    '
         l(165) = 'MNO5      5 -7  1  5     6 -5  1      1 32 6    '
         l(166) = '3MK5      5 -6     5     6 -6  1+10   3 6119    '
         l(167) = '3MP5      5 -6     7     6 -6 -1      3 6       '
         l(168) = 'M5        5 -5  1  5     4 -5 -1-12   2 6121    '
         l(169) = 'MNK5      5 -5  1  5     4 -4 -1-10   2 6119    '
         l(170) = '2MP5      5 -4     3     4 -4  1      2 6       '
         l(171) = 'MSO5      5 -4     3     4 -3         1 31 6    '
         l(172) = '3MO5      5 -4     5     4 -5 -1      1 33 6    '
         l(173) = 'MSK5      5 -2     3     2 -2 -1-10   1 6119    '
         l(174) = '3KM5      5 -2     5     2 -2 -3-14   1 6319    '
         l(175) = '2(MN)S6   6-10  2  8     8 -8         4 6       '
         l(176) = '3MNS6     6 -9  1  8     8 -8         4 6       '
         l(177) = '4MK6      6 -8     6     8 -8   +11   4 6120    '
         l(178) = '2NM6      6 -8  2  6     6 -6         3 6       '
         l(179) = '4MS6      6 -8     8     8 -8         4 6       '
         l(180) = '2MSNK6    6 -7  1  4     6 -6   +11   3 6120    '
         l(181) = '2MN6      6 -7  1  6     6 -6         3 6       '
         l(182) = '2MNU6     6 -7 -1  8     6 -6         3 6       '
         l(183) = '3MSK6     6 -6     4     6 -6   +11   3 6120    '
         l(184) = 'M6        6 -6     6     6 -6         3 6       '
         l(185) = 'MSN6      6 -5  1  4     4 -4         2 6       '
         l(186) = 'MNK6      6 -5  1  6     4 -4   -11   2 6120    '
         l(187) = '4MN6      6 -5 -1  6     6 -6         5 6       '
         l(188) = 'MKNU6     6 -5 -1  8     4 -4   -11   2 6120    '
         l(189) = '2(MS)K6   6 -4     2     4 -4   +11   2 6120    '
         l(190) = '2MS6      6 -4     4     4 -4         2 6       '
         l(191) = '2MK6      6 -4     6     4 -4   -11   2 6120    '
         l(192) = '2SN6      6 -3  1  2     2 -2         1 6       '
         l(193) = '3MSN6     6 -3 -1  4     4 -4         4 6       '
         l(194) = 'MKL6      6 -3 -1  6     4 -4  2-11-131 6120122 '
         l(195) = '2SM6      6 -2     2     2 -2         1 6       '
         l(196) = 'MSK6      6 -2     4     2 -2   -11   1 6120    '
         l(197) = 'S6        6                                     '
         l(198) = '2MNO7     7 -9  1  7     8 -7  1      1 33 6    '
         l(199) = '2NMK7     7 -8  2  7     6 -6 -1-10   3 6119    '
         l(200) = 'M7        7 -7  1  7     6 -7 -1-12   3 6121    '
         l(201) = '2MSO7     7 -6     5     6 -5  1      1 32 6    '
         l(202) = 'MSKO7     7 -4     5     4 -3  1-11   1 31 6120 '
         l(203) = '2(MN)8    8-10  2  8     8 -8         4 6       '
         l(204) = '3MN8      8 -9  1  8     8 -8         4 6       '
         l(205) = '3MNKS8    8 -9  1 10     8 -8   -11   4 6120    '
         l(206) = 'M8        8 -8     8     8 -8         4 6       '
         l(207) = '2MSN8     8 -7  1  6     6 -6         3 6       '
         l(208) = '2MNK8     8 -7  1  8     6 -6   -11   3 6120    '
         l(209) = '3MS8      8 -6     6     6 -6         3 6       '
         l(210) = '3MK8      8 -6     8     6 -6   -11   3 6120    '
         l(211) = '2SNM8     8 -5  1  4     4 -4         2 6       '
         l(212) = 'MSNK8     8 -5  1  6     4 -4   -11   2 6120    '
         l(213) = '2(MS)8    8 -4     4     4 -4         2 6       '
         l(214) = '2MSK8     8 -4     6     4 -4   -11   2 6120    '
         l(215) = '3SM8      8 -2     2     2 -2         1 6       '
         l(216) = '2SMK8     8 -2     4     2 -2   -11   1 6120    '
         l(217) = 'S8        8                                     '
         l(218) = '2(MN)K9   9-10  2  9     8 -8 -1-10   4 6119    '
         l(219) = '3MNK9     9 -9  1  9     8 -8 -1-10   4 6119    '
         l(220) = '4MK9      9 -8     9     8 -8 -1-10   4 6119    '
         l(221) = '3MSK9     9 -6     7     6 -6 -1-10   3 6119    '
         l(222) = '4MN10    10-11  1 10    10-10         5 6       '
         l(223) = 'M10      10-10    10    10-10         5 6       '
         l(224) = '3MSN10   10 -9  1  8     8 -8         4 6       '
         l(225) = '4MS10    10 -8     8     8 -8         4 6       '
         l(226) = '2(MS)N10 10 -7  1  6     6 -6         3 6       '
         l(227) = '2MNSK10  10 -7  1  8     6 -6   -11   3 6120    '
         l(228) = '3M2S10   10 -6     6     6 -6         3 6       '
         l(229) = '4MSK11   11 -8     9     8 -8 -1-10   4 6119    '
         l(230) = 'M12      12-12    12    12-12         6 6       '
         l(231) = '4MSN12   12-11  1 10    10-10         5 6       '
         l(232) = '5MS12    12-10    10    10-10         5 6       '
         l(233) = '3MNKS12  12 -9  1 10     8 -8   -11   4 6120    '
         l(234) = '4M2S12   12 -8     8     8 -8         4 6       '
      end subroutine kompbs
      
      ! =======================================================================
      
      !> Calculates the number of hours referred to January 1, 00:00 of the year 'JAAR' from a given date/time.
      !! meteo1 : datumi
      subroutine datumi(jaar, jdatum, t)
         integer              , intent(in)  :: jaar   !< Year
         integer, dimension(6), intent(in)  :: jdatum !< Date and time
         real(hp),              intent(out) :: t      !< Time in hours referred to January 1, 00:00 of the year 'JAAR'
         !
         integer                 :: i     !< loop counter
         integer                 :: jhulp !< Help var.
         integer                 :: mnd   !< Help var. for the month
         real(hp)                :: rlen  !< Length of a year in hours
         real(hp), dimension(12) :: rmd   !< The number of days of the cumulated counted months
         !
         rmd(1)  =   0.0_hp
         rmd(2)  =  31.0_hp
         rmd(3)  =  59.0_hp
         rmd(4)  =  90.0_hp
         rmd(5)  = 120.0_hp
         rmd(6)  = 151.0_hp
         rmd(7)  = 181.0_hp
         rmd(8)  = 212.0_hp
         rmd(9)  = 243.0_hp
         rmd(10) = 273.0_hp
         rmd(11) = 304.0_hp
         rmd(12) = 334.0_hp
         !
         jhulp = jdatum(1)
         !
         ! Calculate month definitions for leap-years:
         ! year divisible by 4 minus centuries which are not divisible by 4 
         if (mod(jhulp, 4) == 0) then
            if (mod(jhulp, 100)/=0 .or. mod(jhulp, 400)==0) then
               do i = 3, 12
                  rmd(i) = rmd(i) + 1d0
               enddo
            endif
         endif
         !
         mnd = jdatum(2)
         t = rmd(mnd)*24.0_hp + real(jdatum(3) - 1, hp)*24.0_hp + real(jdatum(4), hp) &
                              + real(jdatum(5), hp)/60.0_hp     + real(jdatum(6), hp)/3600.0_hp
         !
         ! hypothetical case (jhulp = jdatum(1) and jaar = jdatum(1))
         !
         if (jhulp /= jaar) then
            rlen = 8760.0_hp
            if (jhulp <= jaar) then
               if (mod(jhulp, 4) == 0) rlen = 8784.0_hp
               t = t - rlen
            else
               if (mod(jaar, 4) == 0) rlen = 8784.0_hp
               t = t + rlen
            endif
         endif
      end subroutine datumi
      
      ! =======================================================================
      
      !> Calulates helper variables V and F.
      !! meteo1 : hulpgr
      subroutine hulpgr(jaar, tm1, v, f)
         integer                , intent(in)  :: jaar !< Present year
         real(hp)               , intent(in)  :: tm1  !< Given time in hours referred to January 1, 00:00:00
         real(hp), dimension(15), intent(out) :: v    !< Help var. to calculate V0U()
         real(hp), dimension(25), intent(out) :: f    !< Help var. to calculate FR()
         !
         integer  :: ischrk  !< Number of leap-years since 1900
         integer  :: j
         real(hp) :: ci
         real(hp) :: ci4
         real(hp) :: cri
         real(hp) :: dhalf   !< Value for 0.5 in SIGN function
         real(hp) :: p
         real(hp) :: pix2    !< PI*2.
         real(hp) :: q
         real(hp) :: ri
         real(hp) :: rjaar   !< Real value of JAAR - 1900
         real(hp) :: rk
         real(hp) :: rn1
         real(hp) :: s2ri
         real(hp) :: si
         real(hp) :: si4
         real(hp) :: sri
         real(hp) :: sri3
         real(hp) :: tm3     !< ISCHRK + TM1/24.0, i.e. the number of correction-days since January 1, 1900 00:00 hour, after the length of a year is set to 365 days in the first instance
         real(hp) :: z
         !
         ! Calculate tm3 assuming tm1 plus the number of additional leap-years 
         ! since 1900. Centuries which are indivisible by 4 are not leap-years.)
         !
         pix2   = pi * 2.0_hp
         dhalf  = 0.5_hp
         rjaar  = real(jaar - 1900, fp)
         ischrk = int((rjaar - 0.99_hp)/4.0_hp) - int((rjaar - 0.99_hp)/100.0_hp) &
                                                + int((rjaar + 300.0_hp - 0.99_hp)/400.0_hp)
         tm3    = real(ischrk, fp) + real(tm1/24.0_hp, fp)
         !
         v(1) = (180.000_hp + 360.0000000_hp*tm3)*degrad
         v(2) = (277.026_hp + 129.3848200_hp*rjaar + 13.176396800000_hp*tm3)*degrad
         v(3) = (334.384_hp + 40.6624700_hp *rjaar +  0.111404000000_hp*tm3)*degrad
         v(4) = (280.190_hp - 0.2387136_hp  *rjaar +  0.985647360000_hp*tm3)*degrad
         v(5) = (281.221_hp + 0.0171800_hp  *rjaar +  0.000047064943_hp*tm3)*degrad
         v(8) = (259.156_hp + 340.6718100_hp*rjaar -  0.052953945000_hp*tm3)*degrad
         !
         z = 0.009415_hp
         p = atan(z*sin(v(8))/(1.0_hp + z*(1.0_hp - cos(v(8)))))
         z = -0.17794_hp
         q = atan(z*sin(v(8))/(1.0_hp + z*(1.0_hp - cos(v(8)))))
         !
         v(6) = -p - q
         v(7) = p - q
         !
         rk = 0.9137_hp - 0.03569_hp*cos(v(8))
         ri = atan(sqrt(1.0_hp - rk*rk)/rk)
         !
         v(9) = ri
         !
         p   = mod(v(3), pix2) - pix2*(sign(dhalf, v(3)) - dhalf)
         rk  = v(6)
         rn1 = v(7)
         !
         ! Initialization of common arguments
         !
         s2ri = sin(2.0_hp*ri)
         sri  = sin(ri)
         si   = sin(0.5_hp*ri)
         cri  = cos(ri)
         ci   = cos(0.5_hp*ri)
         !
         v(10) = atan(s2ri*sin(rn1)/(s2ri*cos(rn1) + 0.3347_hp))
         v(11) = atan(sri*sri*sin(2.0_hp*rn1)/(sri*sri*cos(2.0_hp*rn1) + 0.0727_hp))
         v(12) = atan(sin(2.0_hp*(p - rk))/(3.0_hp*cri/(ci*ci) + cos(2.0_hp*(p - rk))))
         v(13) = atan(sin(2.0_hp*(p - rk))/(ci*ci/(si*si*6.0_hp) - cos(2.0_hp*(p - rk))))
         v(14) = 3.0_hp*v(10)
         v(15) = 0.0_hp
         !
         ! Reduce all angles to range 0 - 2*pi radials
         !
         do j = 1, 15
            v(j) = mod(v(j), pix2) - pix2*(sign(dhalf, v(j)) - dhalf)
         enddo
         !
         ci4  = ci*ci*ci*ci
         si4  = si*si*si*si
         sri3 = sri*sri*sri
         !
         f(1)  = (2.0_hp/3.0_hp - sri*sri)/0.5021_hp
         f(2)  = sri*sri/0.1578_hp
         f(3)  = sri*ci*ci/0.38_hp
         f(4)  = s2ri/0.7214_hp
         f(5)  = sri*si*si/0.0164_hp
         f(6)  = ci4/0.9154_hp
         f(7)  = sri*sri/0.1565_hp
         f(8)  = si4/0.0017_hp
         f(9)  = (sri - 1.25_hp*sri3)/0.3192_hp
         f(10) = sri3/0.063_hp
         f(11) = sri*sri*ci*ci/0.1518_hp
         f(12) = (1d0 - 10.0_hp*si*si + 15.0_hp*si4)*ci*ci/0.5873_hp
         f(13) = (1d0 - 10.0_hp*ci*ci + 15.0_hp*ci4)*si*si/0.2147_hp
         f(14) = sri*ci4/0.3658_hp
         f(15) = (ci*ci - 2.0_hp/3.0_hp)*sri*ci*ci/0.1114_hp
         f(16) = (ci*ci - 1.0_hp/3.0_hp)*sri*si*si/0.0103_hp
         f(17) = ci4*ci*ci/0.8758_hp
         f(18) = ci4*si*si/0.038_hp
         f(19) = sqrt(0.8965_hp*s2ri*s2ri + 0.6001_hp*s2ri*cos(rn1) + 0.1006_hp)
         f(20) = sqrt(19.0444_hp*sri3*sri + 2.7702_hp*sri*sri*cos(2.0_hp*rn1) + 0.0981_hp)
         f(21) = 6.0_hp*cri*cos(2.0_hp*(p - rk))/(ci*ci) + 9.0_hp*cri*cri/(ci4)
         f(21) = 2.6316_hp*sri*ci*ci*0.5_hp*sqrt(1.0_hp + f(21))
         f(22) = 36.0_hp*si4/(ci4) - 12.0_hp*si*si/(ci*ci)*cos(2.0_hp*(p - rk))
         f(22) = 1.0924_hp*ci4*sqrt(1.0_hp + f(22))
      end subroutine hulpgr
      
      ! =======================================================================
      
      !> calculates V0U() and FR()
      !! meteo1 : bewvuf
      subroutine bewvuf(ierrs     ,kcmp      ,mxkc      ,inaam     ,knaam     , &
                      & jnaam     ,w         ,v0u       ,fr        ,v         , f)
         integer,                          intent(out) :: ierrs !<  Number of error messages
         integer,                          intent(in)  :: kcmp  !< 
         integer,                          intent(in)  :: mxkc  !< 
         character(8),                     intent(in)  :: inaam !< Name of the referenced components
         character(8), dimension(mxkc),    intent(in)  :: knaam !< Names of all components
         integer,      dimension(mxkc*16), intent(in)  :: jnaam !< Help var.
         real(hp),     dimension(kcmp)                 :: w     !< Angular velocity of the referenced components
         real(hp),     dimension(kcmp)                 :: v0u   !< Astronomical arguments of the  referenced components [rad]
         real(hp),     dimension(kcmp)                 :: fr    !< Amplitude factors for the referenced components
         real(hp),     dimension(15),      intent(in)  :: v     !< Help var. to calculate V0U()
         real(hp),     dimension(25),      intent(in)  :: f     !< Help var. to calculate FR()
         !
         integer  :: ia1   !< 
         integer  :: ia2   !< 
         integer  :: iar   !< 
         integer  :: ie1   !< 
         integer  :: ie2   !< 
         integer  :: iex   !< 
         integer  :: ikomp !< 
         integer  :: j     !< 
         integer  :: kw    !< 
         integer  :: kx    !< 
         integer  :: mh    !< 
         integer  :: mp    !< 
         integer  :: mp1   !< 
         integer  :: ms    !< 
         integer  :: mt    !< 
         real(hp) :: dhalf !< Value for 0.5 in SIGN function
         real(hp) :: pix2  !< 
         real(hp) :: s1    !< 
         real(hp) :: s2    !< 
         !
         pix2 = pi * 2.0_hp
         dhalf = 0.5_hp
         ! loop over given components
         do ikomp = 1, kcmp
            ! loop over the elements of kompbes
            do j = 1, mxkc
               ! test on name of present component
               if (inaam == knaam(j)) then
                  ! compute angular velocity
                  mt = jnaam(16*j - 15)
                  ms = jnaam(16*j - 14)
                  mp = jnaam(16*j - 13)
                  mh = jnaam(16*j - 12)
                  mp1 = jnaam(16*j - 11)
                  w(ikomp) = mt*15.0_hp + ms*0.54901653_hp + mp*0.0046418333_hp &
                                      & + mh*0.04106864_hp + mp1*0.0000019610393_hp
                  w(ikomp) = (w(ikomp)*pix2)/360.0_hp
                  ! compute v0+u
                  v0u(ikomp) = (jnaam(16*j - 8)*pix2)/4.0_hp
                  do kw = 1, 7
                     kx = 16*j - 16 + kw
                     v0u(ikomp) = v0u(ikomp) + v(kw)*jnaam(kx)
                  enddo
                  ie1 = jnaam(16*j - 7)
                  if (ie1 /= 0) then
                     ia1 = abs(ie1)
                     s1 = real(ie1/ia1, fp)
                     v0u(ikomp) = v0u(ikomp) + s1*v(ia1)
                     ie2 = jnaam(16*j - 6)
                     if (ie2 /= 0) then
                        ia2 = abs(ie2)
                        s2 = real(ie2/ia2, fp)
                        v0u(ikomp) = v0u(ikomp) + s2*v(ia2)
                     endif
                  endif
                  v0u(ikomp) = mod(v0u(ikomp), pix2) - pix2*(sign(dhalf, v0u(ikomp)) - dhalf)
                  ! compute f
                  fr(ikomp) = 1.0_hp
                  iex = jnaam(16*j - 5)
                  if (iex /= 0) then
                     iar = jnaam(16*j - 4)
                     fr(ikomp) = (f(iar))**iex
                     iex = jnaam(16*j - 3)
                     if (iex /= 0) then
                        iar = jnaam(16*j - 2)
                        fr(ikomp) = fr(ikomp)*(f(iar))**iex
                        iex = jnaam(16*j - 1)
                        if (iex /= 0) then
                           iar = jnaam(16*j)
                           fr(ikomp) = fr(ikomp)*(f(iar))**iex
                        endif
                     endif
                  endif
                  exit
               endif
               if (j >= mxkc) then
                  ierrs = ierrs + 1
                  call setECMessage("unknown component '"//trim(inaam)//"' ")
                  exit
               endif
            enddo
         enddo
      end subroutine bewvuf
      
      ! =======================================================================
      
!!==============================================================================
!
!function read_spv_block(unitnr, p_conv, xwind, ywind, press, nmax, mmax, tread, ipart) result(success)
!   !
!   ! Read block in meteo_on_flow_grid file
!   !
!   ! result
!   logical                    :: success
!   !
!   ! arguments
!   !
!   integer, intent(in)        :: unitnr
!   integer, intent(in)        :: nmax
!   integer, intent(in)        :: mmax
!   real(hp), dimension(:,:), intent(out) :: xwind, ywind, press
!   real(hp), intent(in)       :: p_conv
!   real(hp), intent(out)      :: tread
!   integer, intent(in)        :: ipart ! request 'uvp' for all components,
!                                       ! 'u', 'v', or 'p' for one component
!   !
!   ! locals
!   !
!   integer                    :: i
!   integer                    :: ierr
!   integer                    :: j
!   integer                    :: loc_is
!   character(132)             :: rec
!   !
!   ! read line with 'TIME = ... minutes ' or a Headerline
!   !
!   read (unitnr, '(a)') rec
!   if (index(rec, 'HEADER') .gt. 0) then
!      ! skip header-lines
!      do
!        read (unitnr, '(a)', iostat=ierr) rec
!        if (ierr /= 0) then
!           call setECMessage("ERROR: error reading header of meteo-file")
!           success = .false.
!           return
!        endif
!        i = index(rec, 'END OF HEADER')
!        if (i .gt. 0) then
!           !
!           ! extra read so rec is always 'TIME = ... minutes ' after this if-do-if-block
!           !
!           read (unitnr, '(a)') rec
!           exit
!        endif
!      enddo
!   endif
!   !
!   loc_is  = index(rec, '=')
!   if (loc_is .gt. 0) then
!      read (rec(loc_is+1:), *) tread
!   else
!      call setECMessage('Could not find time in meteo-file')
!      success = .false.
!      return
!   endif
!   !
!   ! Loop over the first dimension in flow
!   !
!   if (ipart == 1 .or. ipart == -1) then
!      if ( size(xwind,1) .ne. nmax .or. size(xwind,2) .ne. mmax) then
!         call setECMessage('READ_SPV_BLOCK: wrong sizes xwind')
!         success = .false.
!         return
!      endif
!      do j = 1,nmax
!         read(unitnr,*,end = 100, err=101) ( xwind(j,i), i = 1,mmax )
!      enddo
!   endif
!   if (ipart == 2 .or. ipart == -1) then
!      if ( size(ywind,1) .ne. nmax .or. size(ywind,2) .ne. mmax ) then
!         call setECMessage('READ_SPV_BLOCK: wrong sizes ywind')
!         success = .false.
!         return
!      endif
!      do j = 1,nmax
!         read(unitnr,*,end = 100, err=102) ( ywind(j,i), i = 1,mmax )
!      enddo
!   endif
!   if (ipart == 3 .or. ipart == -1) then
!      if ( size(press,1) .ne. nmax .or. size(press,2) .ne. mmax ) then
!         call setECMessage('READ_SPV_BLOCK: wrong sizes press')
!         success = .false.
!         return
!      endif
!      do j = 1,nmax
!         read(unitnr,*,end = 100, err=103) ( press(j,i), i = 1,mmax )
!      enddo
!      !
!      ! Conversion of pressure to Pa (N/m2). If already Pa, p_conv = 1.0_hp
!      !
!      if (p_conv .ne. 1d0) then
!         press(:,:) = press(:,:) * p_conv
!      endif
!   endif
!   !
!   success = .true.
!   return
!100 continue
!   call setECMessage('Unexpected end of file in meteo_on_flow_grid file')
!   success = .false.
!   return
!101 continue
!   call setECMessage('Error reading wind u-field')
!   success = .false.
!   return
!102 continue
!   call setECMessage('Error reading wind v-field')
!   success = .false.
!   return
!103 continue
!   call setECMessage('Error reading pressure field')
!   success = .false.
!   return
!end function read_spv_block
!!
!function ec_grib_open(unitnr, filename) result(success)
!   !
!   ! wrapper around pbopen
!   !
!   ! result
!   logical                    :: success
!   !
!   ! arguments
!   !
!   integer,      intent(out) :: unitnr
!   character(*), intent(in)  :: filename
!   !
!   ! locals
!   !
!   integer                   :: ierr
!   character(maxFileNameLen) :: filename2
!   !
!   if (index(filename,'?') > 0) then
!      success = expand_wildcard(1, filename, filename2)
!   else
!      filename2 = filename
!   endif
!   !
!#  if defined GRIB
!      call pbopen(unitnr, filename2, 'r', ierr)
!#  else
!      unitnr = -1
!      ierr   = -4
!#  endif
!   select case (ierr)
!   case (0)
!      success = .true.
!   case (-1)
!      call setECMessage("ERROR: grib open: could not open file")
!   case (-2)
!      call setECMessage("ERROR: grib open: invalid filename")
!   case (-3)
!      call setECMessage("ERROR: grib open: invalid open mode")
!   case (-4)
!      call setECMessage("ERROR: grib lib not linked")
!   case default
!      call setECMessage("ERROR: grib open: unknown error")
!   end select
!   !
!end function ec_grib_open
!!
!function read_grib(unitnr, filename, filenr, xwind, ywind, press, nmax, mmax, tread, meta) result(success)
!   !
!   ! Read wind and pressure from grib file
!   !
!   ! result
!   logical                            :: success
!   !
!   ! arguments
!   !
!   integer                            :: unitnr
!   character(maxFileNameLen)          :: filename
!   integer                            :: filenr
!   integer                            :: nmax       ! out at initialization, otherwise in
!   integer                            :: mmax       ! out at initialization, otherwise in
!   type(tGrib_data), pointer, optional :: meta
!   real(hp), dimension(:,:)           :: xwind, ywind, press
!   real(hp)                           :: txwind, tywind, tpress, tread
!   !
!   ! locals
!   !
!   character(maxFileNameLen)          :: filename2
!   logical, parameter                 :: debug = .false.
!   logical                            :: initialize ! use for initialization
!   integer                            :: iyear, imonth, iday, ihour, imin
!   integer                            :: bufsz1, bufsz2
!   integer                            :: ifldtp   ! Type of the field values acording the WMO table 2,
!!    used are:  001   Pressure Pa
!!               033   u-component of wind m/s
!!               034   v-component of wind m/s
!!               124   Momentum flux (stress) u component N/m^2 (not yet implemented)
!!               125   Momentum flux (stress) v component N/m^2 (not yet implemented)
!   integer                             :: ierr
!   integer                             :: iword       ! number of elements from inbuff that contain coded data (return from gribex not used)
!   integer                             :: lenout
!   integer                             :: i, n, m
!   integer                             :: wildcard
!   integer , dimension(:), allocatable :: buffer
!   real(sp), dimension(:), allocatable :: zbuf4
!   integer                             :: isec0(2)    ! integer info from section 0 of GRIB file (identification)
!   integer                             :: isec1(1024) ! array holding section 1 of GRIB data (time/date)
!   integer                             :: isec2(1024) ! array holding section 2 of GRIB data (grid definition)
!   integer                             :: isec3(2)    ! info from section 3 of GRIB file (bitmap section)
!   integer                             :: isec4(512)  ! info from section 4 of GRIB file (field values)
!   real(sp)                            :: zsec2(512)  ! data from section 2 of the GRIB file
!   real(sp)                            :: zsec3(2)    ! data from section 3 of the GRIB file
!   logical                             :: found_u, found_v, found_p
!   character(1)                        :: request     ! 'J' only decodes identification, 'D' decodes all
!   !
!   ! body
!   !
!   success = .false.
!   initialize = (present(meta))
!
!   txwind  = dble(EC_MISSING_VALUE)
!   tywind  = dble(EC_MISSING_VALUE)
!   tpress  = dble(EC_MISSING_VALUE)
!   !
!   if (unitnr < 0) return ! work-around
!   !
!   if (initialize) then
!      request  = 'J'
!      bufsz1 = 4 * 999 * 999
!      bufsz2 = 1
!   else
!      request  = 'D'
!      bufsz1 = 4 * mmax * nmax
!      bufsz2 = bufsz1
!   endif
!   allocate(buffer(bufsz1), zbuf4(bufsz2), stat=ierr)
!   if (ierr /= 0) then
!      call setECMessage("allocate problem in read_grib")
!      return
!   endif
!   !
!   found_u = .false.
!   found_v = .false.
!   found_p = .false.
!   outer: do while (.not. (found_u .and. found_v .and. found_p))
!      !
!      ! read from file and check error status
!      !
!#     if defined GRIB
!         call pbgrib(unitnr, buffer, bufsz1, lenout, ierr)
!#     else
!         success = .false.
!         call setECMessage("ERROR: grib lib not linked")
!         exit outer
!#     endif
!      select case (ierr)
!      case (0)
!         success = .true.
!      case (-1)
!         if (filenr == -1) then
!            call setECMessage("end of file")
!            exit
!         else
!#           if defined GRIB
!               call pbclose(unitnr, ierr)
!#           endif
!            unitnr = -1
!            inner: do i = filenr+1, 9
!               wildcard = index(filename, '?')
!               filename2 = filename
!               write(filename2(wildcard:wildcard), '(i1)') i
!               inquire(file=trim(filename2), exist=success)
!               if (success) then
!                  success = ec_grib_open(unitnr, filename2)
!                  if (.not. success) exit outer
!#                 if defined GRIB
!                     call pbgrib(unitnr, buffer, bufsz1, lenout, ierr)
!#                 endif
!                  if (ierr == 0) then
!                     exit inner
!                  else
!                     call setECMessage("too much trouble with wildcards in gribfiles")
!                     exit outer
!                  endif
!               endif
!            enddo inner
!            if (.not. success) then
!               call setECMessage("too much trouble with wildcards in gribfiles")
!               exit outer
!            endif
!         endif
!      case (-2)
!         call setECMessage("error in file handling")
!         exit
!      case (-3)
!         call setECMessage("size of buffer array too small to hold product")
!         exit
!      case default
!         call setECMessage("unknown error in grib_grid")
!         exit
!      end select
!      !
!      ! decoded grib data
!      !
!#     if defined GRIB
!         if (debug) call grsdbg(1)
!#     endif
!      ierr = 0
!      call gribex(isec0, isec1, isec2, zsec2, isec3, zsec3, isec4, zbuf4, bufsz2, buffer, lenout, iword, request, ierr)
!      select case (ierr)
!      case (-4)
!         if (debug) write (*,*) "grib decode warning: Bit-map encountered"
!      case (-6)
!         if (debug) write (*,*) "grib decode warning: ECMWF pseudo-grib data encountered."
!      case (0)
!         success = .true.
!      case (1:999)
!         call setECMessage("decoding error in read_grib", ierr)
!         success = .false.
!         exit
!      case default
!         if (debug) write (*,*) "grib decode warning number", ierr
!      end select
!      !
!      ! fill metadata
!      !
!      if (initialize) then
!          call fill_grib_metadata(nmax, mmax, meta, isec2)
!          exit
!      endif
!      !
!      ! file time tread
!      !
!      iyear  = (isec1(21)-1)*100+isec1(10)
!      imonth = isec1(11)
!      iday   = isec1(12)
!      ihour  = isec1(13) + isec1(16)
!      imin   = isec1(14)
!      tread  = real(1440 * (iday -1) + 60 * ihour + imin, hp)
!      !
!      ! fill wind or pressure
!      !
!      ifldtp = isec1(6)
!      select case (ifldtp)
!      case (1)
!         if (debug) write (*,*) 'found pressure'
!         do m = 1, mmax
!             do n = 1, nmax
!                press(n, m) = zbuf4(m+(n-1)*mmax)
!             enddo
!         enddo
!         tpress = tread
!         found_p = .true.
!      case (33)
!         if (debug) write (*,*) 'found u-wind'
!         do m = 1, mmax
!             do n = 1, nmax
!                xwind(n,m) = zbuf4(m+(n-1)*mmax)
!             enddo
!         enddo
!         txwind = tread
!         found_u = .true.
!      case (34)
!         if (debug) write (*,*) 'found v-wind'
!         do m = 1, mmax
!             do n = 1, nmax
!                ywind(n,m) = zbuf4(m+(n-1)*mmax)
!             enddo
!         enddo
!         tywind = tread
!         found_v = .true.
!      case (124:125)
!         if (debug) write (*,*) 'found wind stress'
!      case default
!         if (debug) write (*,*) 'unknown entry: ', ifldtp
!      end select
!   enddo outer
!   !
!   if ((tpress /= txwind .or. txwind /= tywind) .and. success .and. .not. initialize) then
!      write (*,*) 'different times for pressure and x, y wind: ', tpress, txwind, tywind
!      success = .false.
!   endif
!   !
!   deallocate(buffer, zbuf4)
!end function read_grib
!!
!subroutine fill_grib_metadata(nmax, mmax, meta, isec2)
!   !
!   ! arguments
!   !
!   integer                  :: nmax, mmax
!   type(tGrib_data)         :: meta
!   integer, dimension(1024) :: isec2
!   !
!   ! locals
!   !
!   integer                  :: iang1y, iangy, iang1x, iangx
!   integer                  :: igdtp
!   !
!   ! body
!   !
!   nmax = isec2(3)
!   mmax = isec2(2)
!   !
!   if ( isec2(7) .gt. isec2(4) ) then
!      iang1y = isec2(4)
!      iangy  = isec2(7)
!   else
!      iang1y = isec2(7)
!      iangy  = isec2(4)
!   endif
!   if (isec2(5) .gt. 180000) isec2(5) = isec2(5) - 360000
!   if (isec2(8) .gt. 180000) isec2(8) = isec2(8) - 360000
!   if ( isec2(8) .gt. isec2(5) ) then
!      iang1x = isec2(5)
!      iangx  = isec2(8)
!   else
!      iang1x = isec2(8)
!      iangx  = isec2(5)
!   endif
!   meta%dx = (real (iangx - iang1x, hp) * 1D-3) / real (mmax - 1, hp)
!   meta%dy = (real (iangy - iang1y, hp) * 1D-3) / real (nmax - 1, hp)
!   meta%x0 = real (iang1x, hp) * 1D-3
!   meta%y0 = real (iang1y, hp) * 1D-3
!   !
!   igdtp  = isec2(1)
!   if (igdtp .eq. 10) then
!      meta%latsp = real(isec2(13), hp) * 1D-3
!      meta%lonsp = real(isec2(14), hp) * 1D-3
!   else
!      meta%latsp = 0D0
!      meta%lonsp = 0D0
!   endif
!   !
!end subroutine fill_grib_metadata

                      
      ! =======================================================================
      
      !> Add corr. to astro/harmonic components
      function ecApplyCorrectionToCmp(instancePtr, corFileReaderPtr) result(success)
      use m_ec_support
         logical                      :: success           !< function status
         type(tEcInstance),   pointer :: instancePtr       !< intent(in)
         type(tEcFileReader), pointer :: corFileReaderPtr  !< intent(inout)
         !
         integer :: i            !< loop counter
         integer :: quantityId   !< helper variable 
         integer :: elementSetId !< helper variable 
         integer :: field0Id     !< helper variable 
         integer :: field1Id     !< helper variable 
         integer :: itemId       !< helper variable 
         !
         integer                             :: nPeriods          !< number of periods
         real(hp), dimension(:), allocatable :: periods           !< Fourier components transformed into periods
         character(len=8), dimension(:), allocatable :: components
         real(hp), dimension(:), allocatable :: magnitudes        !< seed values for the magnitudes of the Fourier components
         real(hp), dimension(:), allocatable :: phases            !< seed values for the phases of the Fourier components
         type(tEcFileReader) , pointer     :: cmpFileReaderPtr    !< related file reader (with components)

         character(len=8), pointer         :: cmpcomponent(:), corcomponent(:)             !< raw data from input, stored in the bc%quantity 
         real(hp), pointer                 :: cmpamplitude(:), coramplitude(:)
         real(hp), pointer                 :: cmpphase(:), corphase(:)

         real(hp), pointer                 :: cmpamplitude_result_T0(:)
         real(hp), pointer                 :: cmpphase_result_T0(:)
         real(hp), pointer                 :: cmpamplitude_result_T1(:)
         real(hp), pointer                 :: cmpphase_result_T1(:)
         
         real(hp)                          :: omega               !< dummy variable for astro component period

         integer           ::  icmp, ncmp, icor, ncor, iitem, istat  
         logical           ::  cmpfound 
         !
         success = .true.
            
         if (corFileReaderPtr%bc%func==BC_FUNC_HARMOCORR) then 
            cmpFileReaderPtr => ecSupportFindRelatedBCBlock(instancePtr, corFileReaderPtr, corFileReaderPtr % bc % qname, corFileReaderPtr % bc % bcname, BC_FUNC_HARMONIC)
         elseif (corFileReaderPtr%bc%func==BC_FUNC_ASTROCORR) then 
            cmpFileReaderPtr => ecSupportFindRelatedBCBlock(instancePtr, corFileReaderPtr, corFileReaderPtr % bc % qname, corFileReaderPtr % bc % bcname, BC_FUNC_ASTRO)
         endif    

         if (.not. associated(cmpFileReaderPtr)) then
            ! TODO: message: no related component
            success = .false.
            return
         endif
         
         if (ecFourierReadAll(corFileReaderPtr, periods, components, magnitudes, phases, nPeriods)) then

            do iitem = 1, cmpFileReaderPtr%nItems
               if (cmpFileReaderPtr%items(iitem)%ptr%role == itemType_source) then           ! source items 
                  select case (cmpFileReaderPtr%items(iitem)%ptr%quantityptr%name)
                     case('magnitude')
                        cmpamplitude_result_T0 => cmpFileReaderPtr%items(iitem)%ptr%sourceT0FieldPtr%arr1d
                        cmpamplitude_result_T1 => cmpFileReaderPtr%items(iitem)%ptr%sourceT1FieldPtr%arr1d
                     case('phase')
                        cmpphase_result_T0 => cmpFileReaderPtr%items(iitem)%ptr%sourceT0FieldPtr%arr1d
                        cmpphase_result_T1 => cmpFileReaderPtr%items(iitem)%ptr%sourceT1FieldPtr%arr1d
                  end select 
               endif
            enddo 

            cmpcomponent => cmpFileReaderPtr%bc%quantity%astro_component
            cmpamplitude => cmpFileReaderPtr%bc%quantity%astro_amplitude
            cmpphase => cmpFileReaderPtr%bc%quantity%astro_phase
            corcomponent => corFileReaderPtr%bc%quantity%astro_component
            coramplitude => corFileReaderPtr%bc%quantity%astro_amplitude
            corphase => corFileReaderPtr%bc%quantity%astro_phase

            ncmp = size(cmpcomponent)                       ! number of components 
            ncor = size(corcomponent)                       ! number of corrections 
            do icor = 1,  ncor 
               cmpfound = .false. 
               do icmp = 1,  ncmp
                  if (trim(corcomponent(icor))==trim(cmpcomponent(icmp))) then 
                     cmpamplitude_result_T0(icmp)=cmpamplitude(icmp)*coramplitude(icor)
                     cmpphase_result_T0(icmp)=cmpphase(icmp)+corphase(icor)
                     cmpamplitude_result_T1(icmp)=cmpamplitude(icmp)*coramplitude(icor)
                     cmpphase_result_T1(icmp)=cmpphase(icmp)+corphase(icor)
                     cmpfound = .true.
                     cycle 
                  endif 
               enddo                ! components 
               if(.not.cmpfound) then 
                  ! TODO: think of a meaningfull error message if correcting a non-existing component 
                  success = .false. 
               endif 
            enddo                   ! corrections 
         else
            ! TODO: message
            success = .false.
         end if
      end function ecApplyCorrectionToCmp

      function ecParseARCinfoMask(maskfilname, mask, fileReaderPtr) result(success)
         use m_ec_typedefs
         implicit none
         logical                         :: success
         character(len=256), intent(in)  :: maskfilname
         type(tEcMask),      intent(out) :: mask
         type(tEcFileReader),pointer     :: fileReaderPtr
   
         integer             :: fmask            
         integer             :: iostat 
         character(len=999)  :: rec 
         logical             :: jamaskinit
         integer             :: i, j
         logical             :: exists
   
         success = .false. 
         
         if (.not.ecSupportOpenExistingFile(fmask, maskfilname)) then
            call setECMessage('Cannot open maskfile '//trim(maskfilname))
            return
         endif 
   
         mask%mrange = fileReaderPtr%items(1)%ptr%elementSetPtr%n_cols     ! NB. implicitly assume that all items on this filereader are either based on 
         mask%nrange = fileReaderPtr%items(1)%ptr%elementSetPtr%n_rows     !     the same elementset, or elementsets with the same number of rows and cols 
         mask%mmin   = 1 
         mask%nmin   = 1 
   
         jamaskinit = .false.  
         iostat = 0
         i = 0 
         do while(iostat==0) 
            read(fmask,'(a256)',iostat=iostat) rec 
            if (len_trim(rec)==0) cycle
            if (iostat/=0) cycle
            rec = adjustl(rec)
            call str_upper(rec)
            if (index('%*!#',rec(1:1))+index('//',rec(1:2)) > 0 ) cycle
            if (index(rec,'=')>0) then 
               if (index(rec,'N_COLS')>0) then 
                   read(rec(index(rec,'=')+1:len_trim(rec)),*,iostat=iostat) mask%mrange   
               elseif (index(rec,'N_ROWS')>0) then 
                   read(rec(index(rec,'=')+1:len_trim(rec)),*,iostat=iostat) mask%nrange
               elseif (index(rec,'XLL')>0) then 
                   read(rec(index(rec,'=')+1:len_trim(rec)),*,iostat=iostat) mask%mmin
               elseif (index(rec,'YLL')>0) then 
                   read(rec(index(rec,'=')+1:len_trim(rec)),*,iostat=iostat) mask%nmin
               endif
            else                 ! Line of values expected  
                if (.not.jamaskinit) then 
                   if ((mask%mrange>0) .and. (mask%nrange>0)) then 
                      if (allocated(mask%msk)) deallocate (mask%msk)
                      allocate(mask%msk(mask%mrange*mask%nrange))
                      jamaskinit = .true.
                   else 
                      call setECMessage('At least one of the mask dimensions in '//trim(maskfilname)//' is smaller than 1.')
                      return
                   endif 
                endif  
                i = i + 1 
                ! NB. Mask is stored in a 1D-array (n_rows*n_cols) row-by-row from the last row to the first,
                !     identically to the curvi and arcinfo data, so that data 1d-array matches the mask array elementwise
                read(rec,*,iostat=iostat) mask%msk((mask%nrange-i)*mask%mrange+1:(mask%nrange-i+1)*mask%mrange)
            endif 
         enddo          ! reading maskfile 
         mask%mmax = mask%mmin + mask%mrange - 1 
         mask%nmax = mask%nmin + mask%nrange - 1 
         success = .true. 
         call doclose(fmask)
      end function ecParseARCinfoMask
      
       subroutine strip_comment(rec)
          implicit none
          character(len=*), intent(inout) :: rec 
          integer                         :: reclen, commentpos 
          reclen = len_trim(rec)                                  ! deal with various comment delimiters 
          commentpos = index(rec,'//')
          if (commentpos>0) reclen = min(reclen,commentpos-1)
          commentpos = index(rec,'%')
          if (commentpos>0) reclen = min(reclen,commentpos-1)
          commentpos = index(rec,'#')
          if (commentpos>0) reclen = min(reclen,commentpos-1)
          commentpos = index(rec,'*')
          if (commentpos>0) reclen = min(reclen,commentpos-1)
          commentpos = index(rec,'!')
          if (commentpos>0) reclen = min(reclen,commentpos-1)
          rec=rec(1:reclen)
       end subroutine strip_comment
   
   end module m_ec_filereader_read

   

   


