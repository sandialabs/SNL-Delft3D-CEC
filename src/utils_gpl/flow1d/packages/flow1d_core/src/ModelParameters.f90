module ModelParameters
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
!  $Id: ModelParameters.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/ModelParameters.f90 $
!-------------------------------------------------------------------------------

   use MessageHandling
   
   implicit none
   
   private
   
   public realloc
   public dealloc
   public AddOrReplaceParameter
   public GetParameterValue
   public readini
   public getBoolFromIni   
   public getIntFromIni   
   public getDoubleFromIni 
   public getStringFromIni
   public LogAllParameters
   public ReadSobekSimIni
   
   !> a parameter consists of a name and a value (character)
   type, public :: t_parameter
      character(len=Charln)                     :: Name
      character(len=Charln)                     :: Value
   end type
   
   type, public :: t_category
      character(len=Charln)                     :: Name
      integer                                   :: Size    = 0    !< actual size of the array Par
      integer                                   :: GrowsBy = 2000  !< increment when a reallocation is necessary
      integer                                   :: Count   = 0    !< actual number of parameters in list
      type(t_parameter), pointer, dimension(:)  :: Par
   end type
   
   !> Set containing parameters for SOBEKSIM. \n
   !! The parameters are grouped by categories. Parameterset\%categories(:) \n
   !! The actual parameters can be found in Parameterset\%categories(:)\%Par(:). \n
   !! The array sizes are dynamically updated. 
   type, public :: t_ParameterSet
      integer                                   :: Size    = 0    !< actual size of the array category
      integer                                   :: GrowsBy = 2000  !< increment when a reallocation is necessary
      integer                                   :: Count   = 0    !< actual number of categories in list
      type(t_category), pointer, dimension(:)   :: Category       
   end type
   
   interface realloc
      module procedure reallocCategory
      module procedure reallocParameter
   end interface

   interface dealloc
      module procedure deallocCategory
      module procedure deallocParameter
   end interface dealloc

   type(t_parameterSet), public :: ParameterSet

contains

   !> Subroutine for adding non existing combinations of category and par values. When the 
   !! combination exists the value of this parameter is set to the new value
   subroutine AddOrReplaceParameter(chapter, Keyword, KeyValue, replace)
   
      character(len=*), intent(in)         :: chapter
      character(len=*), intent(in)         :: KeyWord
      character(len=*), intent(in)         :: KeyValue
      logical, intent(in)                  :: replace
      
      character(len=Charln)                :: category
      character(len=Charln)                :: par
      character(len=Charln)                :: value
      integer                   :: i
      integer                   :: j
      logical                   :: foundCategory
      logical                   :: foundParameter
      type(t_category), pointer :: pCat
      
      foundCategory  = .false.
      foundParameter = .false.
      
      category = chapter
      call small(category, len(category))
      par = KeyWord
      call small(par, len(par))
      value = KeyValue
      call small(value, len(value))

      do i = 1, ParameterSet%count
         if (trim(ParameterSet%category(i)%Name) == trim(category)) then
            foundCategory = .true.
            pCat => ParameterSet%category(i)
            do j = 1, pCat%Count
               if (trim(pCat%par(j)%Name) == trim(par)) then
                  foundParameter = .true.
                  if (replace) pCat%par(j)%value = value
               endif
            enddo
            
            if (.not. foundParameter) then
               pCat%Count = pCat%Count + 1
               if (pCat%Count > pCat%Size) then
                  call realloc(pCat)
               endif
               pCat%Par(pCat%Count)%Name  = Par
               pCat%Par(pCat%Count)%Value = Value
            endif
         endif
      enddo
      
      if (.not. foundCategory) then
         ParameterSet%Count = ParameterSet%Count + 1
         if (parameterSet%Count > parameterSet%Size) then
            call realloc(ParameterSet)
         endif
         Parameterset%Category(Parameterset%Count)%Name = category
         pCat => Parameterset%Category(parameterSet%Count)
         call realloc(pCat)
         pCat => ParameterSet%category(ParameterSet%Count)
         pCat%Count = pCat%Count+1
         pCat%par(1)%Name  = Par 
         pCat%par(1)%Value = Value
      endif
      
   end subroutine AddOrReplaceParameter
   
   !> Gets the value of the combination CATEGORY, PARAMETER from the ParameterSet
   subroutine GetParameterValue(category, par, value)
      character(len=*), intent(in)      :: category
      character(len=*), intent(in)      :: par
      character(len=*), intent(inout)   :: value
      
      logical                   :: found
      integer                   :: i, j
      type(t_category), pointer :: pCat
      
      call small(category, len(category))
      call small(par, len(par))
      found = .false.
      value = ''
      do i = 1, ParameterSet%count
         if (trim(ParameterSet%category(i)%Name) == trim(category)) then
            pCat => ParameterSet%category(i)
            do j = 1, pCat%Count
               if (trim(pCat%par(j)%Name) == trim(par)) then
                  found = .true.
                  value = pCat%par(j)%value 
                  call setmessage(LEVEL_DEBUG, trim(category)//': '//trim(par)//' = '//trim(value) )
               endif
            enddo
         endif
      enddo

   end subroutine GetParameterValue
   
   subroutine reallocCategory(ParameterSet)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_parameterSet) :: ParameterSet
      
      ! Local variables
      type(t_category), pointer, dimension(:)         :: oldPar
      
      ! Program code
      
      if (ParameterSet%Size > 0) then
         oldPar=>ParameterSet%category
      endif
      
      if (ParameterSet%growsBy <=0) then
         ParameterSet%growsBy = 200
      endif
      allocate(ParameterSet%category(ParameterSet%Size+ParameterSet%growsBy))
      
      if (ParameterSet%Size > 0) then
         ParameterSet%category(1:ParameterSet%Size) = oldPar(1:ParameterSet%Size)
         deallocate(oldPar)
      endif
      ParameterSet%Size = ParameterSet%Size+ParameterSet%growsBy
   end subroutine reallocCategory
   
   subroutine reallocParameter(Category)
      ! Modules
      
      implicit none
      
      ! Input/output parameters
      type(t_category), pointer  :: Category 

      ! Local variables
      type(t_Parameter), pointer, dimension(:)  :: oldPar
      
      ! Program code
      
      if (Category%Size > 0) then
         oldPar=>Category%Par
      endif
      
      if (Category%growsBy <=0) then
         Category%growsBy = 200
      endif
      allocate(Category%Par(Category%Size+Category%growsBy))
      
      if (Category%Size > 0) then
         Category%Par(1:Category%Size) = oldPar(1:Category%Size)
         deallocate(oldPar)
      endif
      Category%Size = Category%Size+Category%growsBy
   end subroutine reallocParameter
   
   subroutine deallocCategory(ParameterSet)
      type(t_parameterSet) :: ParameterSet
      
      integer           :: i
      type(t_Category), pointer :: pCat
      
      do i = 1, ParameterSet%Count
         pCat=>ParameterSet%category(i)
         if (associated(pCat%Par)) then
            call dealloc(pCat)
         endif
      enddo
      if (associated(parameterSet%Category) ) then
         deallocate(parameterSet%Category)
      endif
      parameterSet%Category => null()
      ParameterSet%Count     = 0
      ParameterSet%size      = 0
   end subroutine deallocCategory
   
   subroutine deallocParameter(category)
      type(t_category), pointer  :: Category 

      deallocate(Category%par)
      Category%par  => null()
      Category%count = 0
      Category%size  = 0
   end subroutine deallocParameter

subroutine readini(stringhead, stringleft, stringright, lenstrright)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    !=======================================================================
    !                       Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         P.J. van Overloop
    !
    ! Module:             READINI (READ windows INItialization)
    !
    ! Module description: Information from sobeksim.ini file is read.
    !
    !
    !     update information
    !     person                    date
    !
    !
    !
    !     lenstrright              Length of string with information found
    !                              in ini-file
    !     stringhead               String with requested header (chapter)
    !                              from ini-file
    !     stringleft               String with requested information from
    !                              ini-file
    !     stringright              String with information found in
    !                              ini-file
    !
    !
    !
    !     Include filenames and language file
    !
    !
    !     Arguments
    !

    use MessageHandling
    implicit none
   !
   ! Global variables
   !
    integer, intent(out)           :: lenstrright
    character(*)  :: stringhead
    character(*)  :: stringleft
    character(*)  :: stringright

    if (len(stringhead) > CharLn) then
       call SetMessage(LEVEL_FATAL, "INTERNAL ERROR: length of Stringhead incorrect")
    endif
    if (len(stringleft) > CharLn) then
       call SetMessage(LEVEL_FATAL, "INTERNAL ERROR: length of stringleft incorrect")
    endif
    if (len(stringright) > CharLn) then
       call SetMessage(LEVEL_FATAL, "INTERNAL ERROR: length of stringright incorrect")
    endif
    call getParameterValue(stringhead, stringleft, stringright)
    lenstrright = len_trim(stringright)

end subroutine readini

!> function returns true or false, depending on the given value in the ini file (or XML).
!! When no value is found the DEFAULT value is returned

logical function getBoolFromIni(stringhead, stringleft, default)
   use MessageHandling
   implicit none
   !
   ! Local parameters
   !
   character(Charln)  :: stringright
   !
   ! Global variables
   !
   character(*)       :: stringhead
   character(*)       :: stringleft
   logical            :: default

   integer            :: lenstrright
   
   if (len(stringhead) > CharLn) then
      call SetMessage(LEVEL_FATAL, "INTERNAL ERROR: length of stringhead incorrect")
   endif
   if (len(stringleft) > CharLn) then
      call SetMessage(LEVEL_FATAL, "INTERNAL ERROR: length of stringleft incorrect")
   endif
    
   ! Initialize
   stringright = ' '
   getBoolFromIni = default

   call getParameterValue(stringhead, stringleft, stringright)
   call small(stringright, len(stringright))
   lenstrright = len_trim(stringright)
    
   if (lenstrright==0) then
      getBoolFromIni = default
      ! Add missing/unspecified to be shown in SOBEK.LOG
      if (getBoolFromIni) then
         call AddOrReplaceParameter(stringhead, stringleft, 'true', .true.)
      else
         call AddOrReplaceParameter(stringhead, stringleft, 'false', .true.)
      endif
   elseif (stringright(1:lenstrright) == 'none') then
      getBoolFromIni = .false.
   elseif (stringright(1:lenstrright) == 'current' .or.  & 
           stringright(1:lenstrright) == 'minimum' .or.  & 
           stringright(1:lenstrright) == 'maximum' .or.  & 
           stringright(1:lenstrright) == 'average') then
      getBoolFromIni = .true.
   elseif (stringright(1:lenstrright) == 'true') then
      getBoolFromIni = .true.
   elseif (stringright(1:lenstrright) == 'false') then
      getBoolFromIni = .false.
   elseif (stringright(1:lenstrright)/='0') then
      getBoolFromIni = .true.
   else
      getBoolFromIni = .false.
   endif
    
end function getBoolFromIni

!> function returns true or false, depending on the given value in the ini file (or XML).
!! When no value is found the DEFAULT value is returned
!logical function getBoolFromIni(stringhead, stringleft, default)
!end function getBoolFromIni

!> function returns int value, depending on the given value in the ini file (or XML).
!! When no value is found the DEFAULT value is returned

integer function getIntFromIni(strhead, strleft, default)
   use MessageHandling
   implicit none
   !
   ! Local parameters
   !
   character(Charln)  :: stringright, stringhead, stringleft
   !
   ! Global variables
   !
   character(*)       :: strhead
   character(*)       :: strleft
   integer            :: default

   integer            :: lenstrright

   stringhead = strhead
   stringleft = strleft
   if (len(stringhead) > CharLn) then
      call SetMessage(LEVEL_FATAL, "INTERNAL ERROR: length of Stringhead incorrect")
   endif
   if (len(stringleft) > CharLn) then
      call SetMessage(LEVEL_FATAL, "INTERNAL ERROR: length of stringleft incorrect")
   endif
    
   ! Initialize
   stringright = ' '

   call getParameterValue(stringhead, stringleft, stringright)
   call small(stringright, len(stringright))
   lenstrright = len_trim(stringright)
    
   if (lenstrright==0) then
      getIntFromIni = default
   else
      read (stringright, *) getIntFromIni
   endif

end function getIntFromIni

!> function returns int value, depending on the given value in the ini file (or XML).
!! When no value is found the DEFAULT value is returned

double precision function getDoubleFromIni(strhead, strleft, default)
   use MessageHandling
   implicit none
   !
   ! Global variables
   !
   character(*)       :: strhead
   character(*)       :: strleft
   double precision   :: default
   !
   ! Local parameters
   !
   character(Charln)  :: stringright, stringhead, stringleft

   integer            :: lenstrright

   stringhead = strhead
   stringleft = strleft
   if (len(stringhead) > CharLn) then
      call SetMessage(LEVEL_FATAL, "INTERNAL ERROR: length of Stringhead incorrect")
   endif
   if (len(stringleft) > CharLn) then
      call SetMessage(LEVEL_FATAL, "INTERNAL ERROR: length of stringleft incorrect")
   endif
    
   ! Initialize
   stringright = ' '

   call getParameterValue(stringhead, stringleft, stringright)
   call small(stringright, len(stringright))
   lenstrright = len_trim(stringright)
    
   if (lenstrright==0) then
      getDoubleFromIni = default
   else
      read (stringright, *) getDoubleFromIni
   endif

   end function getDoubleFromIni


   character(len=IdLen) function getStringFromIni(strhead, strleft, default)
   use MessageHandling

   implicit none
   !
   ! Local parameters
   !
   character(Charln)  :: stringright, stringhead, stringleft
   !
   ! Global variables
   !
   character(*)       :: strhead
   character(*)       :: strleft
   character(*)       :: default

   integer            :: lenstrright

   stringhead = strhead
   stringleft = strleft
    
   if (len(stringhead) > CharLn) then
      call SetMessage(LEVEL_FATAL, "INTERNAL ERROR: length of Stringhead incorrect")
   endif
   if (len(stringleft) > CharLn) then
      call SetMessage(LEVEL_FATAL, "INTERNAL ERROR: length of stringleft incorrect")
   endif
    
   ! Initialize
   stringright = ' '

   call getParameterValue(stringhead, stringleft, stringright)
   call small(stringright, len(stringright))
   lenstrright = len_trim(stringright)
    
   if (lenstrright==0) then
      getStringFromIni = default
   else
      read (stringright, *) getStringFromIni
   endif

end function getStringFromIni

subroutine LogAllParameters()

   use MessageHandling
   use string_module

   implicit none

   ! Local parameters
   character(len=Charln)                     :: Category
   character(len=Charln)                     :: KeyWord
   character(len=Charln)                     :: KeyValue
   
   integer                                   :: i
   integer                                   :: j
   type(t_category), pointer                 :: pCat

   msgbuf = 'Sobek Parameters'

   call SetMessage(LEVEL_INFO, ' ')
   call SetMessage(LEVEL_INFO, 'Sobek Parameters')
   call SetMessage(LEVEL_INFO, ' ')
    
   do i = 1, ParameterSet%count
      
   Category = ParameterSet%category(i)%Name
   call str_upper(Category)
   msgbuf = '['//trim(Category)//']'
   call SetMessage(LEVEL_INFO, msgbuf)
      
   pCat => ParameterSet%category(i)
   do j = 1, pCat%Count
      KeyWord = pCat%par(j)%Name
      KeyValue = pCat%par(j)%value
      msgbuf = trim(Keyword)//' = '//trim(KeyValue)
      call SetMessage(LEVEL_INFO, msgbuf)
   enddo

   call SetMessage(LEVEL_INFO, ' ')

   enddo
    
end subroutine LogAllParameters

! The SOBEKSIM.INI file is read and the parameters are put in the ParameterSet
   subroutine ReadSobekSimIni(file)
   
      character(len=*) :: file
      
      integer                 :: ioStat
      logical                 :: file_exist
      integer                 :: i
      integer                 :: errStat
      integer                 :: isPos
      integer                 :: help
      integer                 :: posEnd
      character(len=Charln)   :: line
      character(len=Charln)   :: category = ''
      character(len=Charln)   :: par
      character(len=Charln)   :: value
      
      integer                 :: ifileunit
      
      if (file == ' ') return
      
      inquire(file=file, exist=file_exist)
      if (.not. file_exist) return
    
      open (newunit=ifileunit, file = file,iostat=errStat)
      if (errStat /= 0) then
         call setMessage(LEVEL_FATAL, 'Error in opening SOBEKSIM.INI')
      endif
      
      read (ifileunit, '(a)', iostat=ioStat) line
      
      do while (iostat == 0)
         if(len_trim(line) > 0 ) then
            i = verify(line, ' ')
            line = line(i:)
            call small(line, len(line))
            isPos = scan(line,'[')
            if (isPos /= 0) then
               ! category found
               posEnd = scan(line,']')
               if (posEnd /= 0) then
                  category = line(isPos+1:posEnd-1)
               else
                  call setMessage(LEVEL_ERROR, 'Incorrect data item found in SOBEKSIM.INI in line: '//trim(line))
               endif
            else
               isPos = scan(line,'=')
               par = line(1:isPos-1)
               ! ignore spaces
               isPos = isPos+1
               help  = verify(line(isPos:), ' ')
               if (help == 0) help=1
               value = trim(line(isPos+help-1:))
               
               call AddOrReplaceParameter(category, par, value, .false. )
            endif
         endif   
         read (ifileunit, '(a)', iostat=ioStat) line
      enddo

      close(ifileunit)
      
   end subroutine ReadSobekSimIni
   
end module ModelParameters
