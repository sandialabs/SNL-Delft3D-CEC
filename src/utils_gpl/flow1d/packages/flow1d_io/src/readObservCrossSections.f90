module m_readObservCrossSections
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2020.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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
!  $Id: readObservCrossSections.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_io/src/readObservCrossSections.f90 $
!-------------------------------------------------------------------------------

   use m_CrossSections
   use MessageHandling
   use properties
   use m_network
   use m_GlobalParameters
   use m_hash_search
   use m_hash_list
   
   
   implicit none
   private
   
   public readObservCrossSections
   public read_observ_cross_section_cache

   !> The file version number of the observation cross section file format: d.dd, [config_major].[config_minor], e.g., 1.03
   !!
   !! Note: read config_minor as a 2 digit-number, i.e., 1.1 > 1.02 (since .1 === .10 > .02).
   !! Convention for format version changes:
   !! * if a new format is backwards compatible with old files, only
   !!   the minor version number is incremented.
   !! * if a new format is not backwards compatible (i.e., old files
   !!   need to be converted/updated by user), then the major version number
   !!   is incremented.
   
   ! Observation cross section file current version: 2.00
   integer, parameter :: ObservCrsFileMajorVersion = 2
   integer, parameter :: ObservCrsFileMinorVersion = 0
   
   ! History observation cross section file versions:

   ! 1.01 (2019-03-12): First version of *.ini type observation cross section file.
   ! 2.00 (2019-07-05): renamed numValues -> numCoordinates.

   contains
   
   !> Reads observation cross sections from a *.ini file
   subroutine readObservCrossSections(network, CrossSectionFile)
      use m_missing, only: dmiss
      use m_alloc
      implicit none
      type(t_network), intent(inout)        :: network
      character(len=*), intent(in)          :: CrossSectionFile
       
      type(t_observCrossSection), pointer   :: pCrs
      logical                               :: success
      type(tree_data), pointer              :: md_ptr
      integer                               :: istat
      integer                               :: numstr
      integer                               :: i
                                            
      character(len=IdLen)                  :: observcrsName
      character(len=IdLen)                  :: branchID
      double precision                      :: Chainage
      integer                               :: numv
      double precision, allocatable         :: xx(:), yy(:)
   
      integer                               :: branchIdx
      type(t_ObservCrossSection), pointer   :: pobservcrs
      integer                               :: pos
      integer                               :: ibin = 0
      character(len=Charln)                 :: binfile
      logical                               :: file_exist
      integer                               :: formatbr       ! =1: use branchid and chainage, =0: use xy coordinate and numCoordinates
      integer                               :: major, minor, ierr
      
      Chainage   = dmiss
      numv       = 0
      branchIdx  = 0
      
      branchID   = ''
      observcrsName = ''

      pos = index(CrossSectionFile, '.', back = .true.)
      binfile = CrossSectionFile(1:pos)//'cache'
      inquire(file=binfile, exist=file_exist)
      if (doReadCache .and. file_exist) then
         open(newunit=ibin, file=binfile, status='old', form='unformatted', access='stream', action='read', iostat=istat)
         if (istat /= 0) then
            call setmessage(LEVEL_FATAL, 'Error opening Cross-Section file')
            ibin = 0
         endif
         call read_observ_cross_section_cache(ibin, network)
         close(ibin)
         return
      endif
      
      call tree_create(trim(CrossSectionfile), md_ptr, maxlenpar)
      call prop_file('ini',trim(CrossSectionfile),md_ptr,istat)

      ! check FileVersion
      ierr = 0
      major = 0
      minor = 0
      call prop_get_version_number(md_ptr, major = major, minor = minor, success = success)
      if (.not. success .or. major < ObservCrsFileMajorVersion) then
         write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of observation cross section file detected in '''//trim(CrossSectionFile)//''': v', major, minor, '. Current format: v',ObservCrsFileMajorVersion,ObservCrsFileMinorVersion,'. Ignoring this file.'
         call warn_flush()
         ierr = 1
      end if

      if (ierr /= 0) then
         goto 999
      end if

      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if
   
      success = .true.
      do i = 1, numstr
        
         if (tree_get_name(md_ptr%child_nodes(i)%node_ptr) == 'observationcrosssection') then
            ! Read Data
            call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'observationcrosssection', 'name', observcrsName, success)
            if (success) then
               call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'observationcrosssection', 'branchID', branchID, success)
               if (success) then ! the crs is defined by branchid and chainage
                  formatbr = 1
                  call prop_get_double(md_ptr%child_nodes(i)%node_ptr, 'observationcrosssection', 'chainage', chainage, success)
               else ! the crs is defined by x, y coordinate
                  formatbr = 0
                  call prop_get_integer(md_ptr%child_nodes(i)%node_ptr, 'observationcrosssection', 'numCoordinates', numv, success) ! UNST-2390: new consistent keyword
                  if (success) then
                     if (numv < 2) then
                        call SetMessage(LEVEL_ERROR, 'Observation cross section '''//trim(observcrsName)//''' should have more than 1 point (numCoordinates > 1).')     
                        cycle
                     else if (numv > 0) then
                        call realloc(xx, numv)
                        call prop_get_doubles(md_ptr%child_nodes(i)%node_ptr, 'observationcrosssection', 'xCoordinates', xx, numv, success)
                        if (success) then
                           call realloc(yy, numv)
                           call prop_get_doubles(md_ptr%child_nodes(i)%node_ptr, 'observationcrosssection', 'yCoordinates', yy, numv, success)
                        end if
                     end if
                  end if
               end if
               
               if (.not. success) then
                  call SetMessage(LEVEL_ERROR, 'Error Reading observation cross section '''//trim(observcrsName)//'''')     
                  cycle
               end if
               
            else
               call SetMessage(LEVEL_ERROR, 'Error Reading the name of observation cross section. ')     
               cycle
            end if
                  
   
            network%observcrs%count = network%observcrs%count + 1
            if (network%observcrs%count > network%observcrs%size) then
               call realloc(network%observcrs)
            end if
            
            pCrs => network%observcrs%observcross(network%observcrs%count)
            
            PCrs%name = observcrsName
            if (formatbr == 1) then
               pCrs%branchid = branchID
               pCrs%chainage = chainage
               pCrs%branchIdx= hashsearch(network%brs%hashlist, branchid)
               pCrs%numValues = 1
               call realloc(pCrs%x,pCrs%numValues)
               call realloc(pCrs%y,pCrs%numValues)
            else
               pCrs%numValues= numv                  
               call realloc(pCrs%x,numv)
               call realloc(pCrs%y,numv)
               pCrs%x(1:numv) = xx(1:numv)
               pCrs%y(1:numv) = yy(1:numv)
               pCrs%branchIdx= 0
            end if
         end if
      end do
      
      write(msgbuf,'(i10,2a)') network%observcrs%Count , ' (1d network) observation cross sections have been read from file ', trim(CrossSectionFile)
      call msg_flush() 
      
999   continue
      call tree_destroy(md_ptr)
      
      if (allocated(xx)) deallocate(xx)
      if (allocated(yy)) deallocate(yy)
         
   end subroutine readObservCrossSections
   
   
   subroutine read_observ_cross_section_cache(ibin, network)
   implicit none
   type(t_network), intent(inout)     :: network
   integer        , intent(in   )     :: ibin
   
   integer                            :: i
   type(t_ObservCrossSection), pointer:: pobservcrs
   
   read(ibin) network%observcrs%count
   network%observcrs%growsby = network%observcrs%count + 2
   call realloc(network%observcrs)
   
   do i = 1, network%observcrs%count
      
      pobservcrs => network%observcrs%Observcross(i)
      
      read(ibin) pobservcrs%name
      read(ibin) pobservcrs%branchid 
      read(ibin) pobservcrs%chainage
      read(ibin) pobservcrs%branchIdx
      read(ibin) pobservcrs%numValues          
      read(ibin) pobservcrs%x
      read(ibin) pobservcrs%y
      
   enddo
   
   call read_hash_list_cache(ibin, network%observcrs%hashlist)
   
   end subroutine read_observ_cross_section_cache
   
   end module m_readObservCrossSections

   
