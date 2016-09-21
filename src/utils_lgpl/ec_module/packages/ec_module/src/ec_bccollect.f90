module m_ec_bccollect
    use m_ec_parameters
    use m_ec_typedefs
    use m_ec_provider
    use m_ec_bcreader
    use m_ec_support
    use m_ec_message
    use m_ec_instance
    use m_alloc 
    use multi_file_io
    use string_module
    use time_module
    implicit none 
          
    private 

    public   ::  ecCollectBCBlocks
    public   ::  collectbc_all

   interface ecCollectBCBlocks
      module procedure collectbc
      module procedure collectbc_all
   end interface ecCollectBCBlocks

   contains 

!============================================================================================================================
! Collectloop: Scan a bc-file, being given QUANTITY and LOCATION.
!              For each match, create a file reader, add it to the list of file readers in the instance
    integer function collectbc(instancePtr, fname, quantity, location, iostat) result (count)
    use m_ec_bcreader, only: checkhdr
    implicit none            
    type (tEcInstance),     pointer,   intent(in)      :: instancePtr     !< EC Instance, overall structure for the EC-module 
    character(len=*),                  intent(in)      :: fname           !< file name (bc-format)
    character(len=*),                  intent(in)      :: quantity        !< quantity name 
    character(len=*),                  intent(in)      :: location        !< location       ! quantity+location = search key 
    integer,                           intent(out)     :: iostat

    integer (kind=8)    ::  fhandle
    character*(255)     ::  rec          
    integer             ::  reclen 
    integer             ::  commentpos
    character*(1000)    ::  keyvaluestr                                    ! all key-value pairs in one header 
    integer             ::  posfs
    integer             ::  nfld
    integer             ::  nq
    logical             ::  jablock, jaheader
    integer             ::  reallocstat
    integer             ::  lineno 
    integer (kind=8)    ::  savepos 
    integer             ::  iostatloc
    type (tEcBCBlock),    pointer :: bcBlockPtr
    type (tEcFileReader), pointer :: fileReaderPtr
    integer             :: bcBlockId, fileReaderId

    iostat = EC_UNKNOWN_ERROR
    lineno = 0 
    savepos = 0

    if (.not.ecSupportOpenExistingFileGnu(fhandle, fname)) then
       iostat = EC_DATA_NOTFOUND
       return
    end if

    count = 0
    do while (.not.mf_eof(fhandle))
       call mf_read(fhandle,rec,savepos)
       iostatloc = 0 ! mf_read always ok?
       if (iostatloc /= 0) then 
          iostat = iostatloc
          return ! beter break?
       endif  
       lineno = lineno + 1 
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

       if (len_trim(rec(1:reclen))>0) then                     ! skip empty lines 
          if (index(rec,'[forcing]'         )>0 .or. &
              index(rec,'[Boundary]'        )>0 .or. &
              index(rec,'[LateralDischarge]')>0) then          ! new boundary chapter       
             jaheader = .true.                                 ! switching to header mode 
             keyvaluestr = ','
             jablock=.false.
             nfld = 0                                          ! count the number of fields in this header block 
             nq = 0                                            ! count the (maximum) number of quantities in this block 
          else
             if (jaheader) then 
                posfs = index(rec(1:reclen),'=')               ! key value pair ?  
                if (posfs>0) then 
                   call replace_char(rec,9,32)                 ! replace tabs by spaces, header key-value pairs only 
                   nfld = nfld + 1                             ! count the number of lines in the header file 
                                                               ! Create a lengthy string of ',key,value,key,value.....' 
                   call str_upper(rec(1:posfs-1))              ! all keywords uppercase , not case sensitive 
                   if (index(rec(1:posfs-1),'QUANTITY')>0) then 
                      nq = nq + 1 
                   endif 

                   keyvaluestr = trim(keyvaluestr)//''''// (trim(adjustl(rec(1:posfs-1))))//''',''' &
                                                    //(trim(adjustl(rec(posfs+1:reclen))))//''',' 
                endif 
             else                                                    ! switch to datamode 
                call str_upper(keyvaluestr,len(trim(keyvaluestr)))   ! case insensitive format 
                if (jakeyvalue(keyvaluestr,'NAME',trim(location))) then  
                   if (jakeyvalue(keyvaluestr,'FUNCTION','ASTRONOMIC').or.jakeyvalue(keyvaluestr,'FUNCTION','ASTROCOR')) then 
                      ! Check for harmonic or astronomic components
                      jablock = .true.
                      jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY','ASTRONOMIC COMPONENT') 
                      jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY',trim(quantity)//' '//'AMPLITUDE')
                      jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY',trim(quantity)//' '//'PHASE')
                   elseif (jakeyvalue(keyvaluestr,'FUNCTION','HARMONIC').or.jakeyvalue(keyvaluestr,'FUNCTION','HARMCOR')) then 
                      ! Check for harmonic or harmonic components
                      jablock = .true.
                      jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY','HARMONIC COMPONENT')
                      jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY',trim(quantity)//' '//'AMPLITUDE')
                      jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY',trim(quantity)//' '//'PHASE')
                   elseif (jakeyvalue(keyvaluestr,'FUNCTION','QH')) then  
                      ! Check for qh
                      jablock = .true.
                      jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY',trim(quantity)//' '//'WATERLEVEL')
                      jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY',trim(quantity)//' '//'DISCHARGE')
                   elseif (jakeyvalue(keyvaluestr,'FUNCTION','T3D')) then  
                      ! Check for timeseries on sigma- or z-levels 
                      jablock = jakeyvalue(keyvaluestr,'QUANTITY',quantity)
                   elseif (jakeyvalue(keyvaluestr,'FUNCTION','TIMESERIES')) then  
                      ! Check for timeseries
                      jablock = jakeyvalue(keyvaluestr,'QUANTITY',quantity)
                   endif 
                   if (jablock) then                           ! block confirmed, Create a filereader and initialize a bc-Block 
                      bcBlockId = ecInstanceCreateBCBlock(InstancePtr)
                      bcBlockPtr=>ecSupportFindBCBlock(instancePtr, bcBlockId)
                      fileReaderId = ecInstanceCreateFileReader(InstancePtr)
                      fileReaderPtr = ecSupportFindFileReader(instancePtr, fileReaderID)
                      fileReaderPtr%bc => bcBlockPtr
                      if (.not.processhdr_all_quantities(bcBlockPtr,nfld,nq,keyvaluestr)) return   ! dumb translation of bc-object metadata  
                      if (.not.checkhdr(bcBlockPtr)) return                                        ! check on the contents of the bc-object 
                      bcBlockPtr%fname = fname 
                      if (ecSupportOpenExistingFileGnu(bcBlockPtr%fhandle, fname)) then
                         call mf_backspace(bcBlockPtr%fhandle, savepos)           ! set newly opened file to the appropriate position 
                         count = count + 1 
                         iostat = EC_NOERR
                      else 
                         call mf_close(bcBlockPtr%fhandle)
                         iostat = EC_DATA_NOTFOUND
                      end if
                   else 
                      ! location was found, but not all required meta data was present
                      iostat = EC_METADATA_INVALID
                   endif                                    ! Right quantity
                else
                   ! location was found, but data was missing/invalid
                   iostat = EC_DATA_NOTFOUND
                endif                                       ! Right label
                jaheader = .false.                          ! No, we are NOT reading a header 
                if (jablock) then                           ! Yes, we are in the bc-block of interest
                endif                                       ! found matching block 
             endif          ! in header mode (data lines are ignored) 
          endif             ! not a new '[forcing]' item  
       endif                ! non-empty string 
    enddo                   ! read/scan loop, ended when we reached end-of-file
    iostat = EC_EOF      
    end function collectbc

!============================================================================================================================
! Collectloop: Scan a bc-file
!              For each bc-block, create a file reader, add it to the list of file readers in the instance
    integer function collectbc_all(instancePtr, fname, iostat, k_refdate, k_timezone, k_timestep_unit, dtnodal) result (count)
    implicit none            
    type (tEcInstance),     pointer,   intent(in)      :: instancePtr     !< EC Instance, overall structure for the EC-module 
    character(len=*),                  intent(in)      :: fname           !< file name (bc-format)
    integer,                           intent(out)     :: iostat
    integer,                optional,  intent(in)      :: k_refdate       !< Kernel timeframe refdate
    real(hp),               optional,  intent(in)      :: k_timezone      !< Kernel timeframe timezone
    integer,                optional,  intent(in)      :: k_timestep_unit !< Kernel timeframe timestep unit 
    real(kind=hp),          optional,  intent(in)      :: dtnodal         !< Nodal factors update interval

    integer (kind=8)    ::  fhandle
    character*(255)     ::  rec          
    integer             ::  reclen 
    integer             ::  commentpos
    character*(1000)    ::  keyvaluestr                                    ! all key-value pairs in one header 
    integer             ::  posfs
    integer             ::  nfld
    integer             ::  nq
    logical             ::  jablock, jaheader
    integer             ::  reallocstat
    integer             ::  lineno 
    integer (kind=8)    ::  savepos 
    integer             ::  iostatloc
    type (tEcBCBlock),    pointer :: bcBlockPtr
    type (tEcFileReader), pointer :: fileReaderPtr
    integer             :: bcBlockId, fileReaderId
    integer             :: ifr 
    logical             :: success
    integer             :: unit

    real(hp)            :: ref_date

    iostat = EC_UNKNOWN_ERROR
    lineno = 0 
    savepos = 0
    jaheader = .false.

    if (.not.ecSupportOpenExistingFileGnu(fhandle, fname)) then
       iostat = EC_DATA_NOTFOUND
       return
    end if

    count = 0
    do while (.not.mf_eof(fhandle))
       call mf_read(fhandle,rec,savepos)
       iostatloc = 0 ! mf_read always ok?
       if (iostatloc /= 0) then 
          iostat = iostatloc
          return ! beter break?
       endif  
       lineno = lineno + 1 
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
       
       if (reclen < 3) cycle

       if (len_trim(rec(1:reclen))>0) then                     ! skip empty lines 
          if (index(rec,'[forcing]'         )>0 .or. &
              index(rec,'[Boundary]'        )>0 .or. &
              index(rec,'[LateralDischarge]')>0) then          ! new boundary chapter       
             jaheader = .true.                                 ! switching to header mode 
             keyvaluestr = ','
             jablock=.false.
             nfld = 0                                          ! count the number of fields in this header block 
             nq = 0                                            ! count the (maximum) number of quantities in this block 
          else
             if (jaheader) then
                posfs = index(rec(1:reclen),'=')               ! key value pair ?  
                if (posfs>0) then 
                   call replace_char(rec,9,32)                 ! replace tabs by spaces, header key-value pairs only 
                   nfld = nfld + 1                             ! count the number of lines in the header file 
                                                               ! Create a lengthy string of ',key,value,key,value.....' 
                   call str_upper(rec(1:posfs-1))              ! all keywords uppercase , not case sensitive 
                   if (index(rec(1:posfs-1),'QUANTITY')>0) then 
                      nq = nq + 1 
                   endif 
                   keyvaluestr = trim(keyvaluestr)//''''// (trim(adjustl(rec(1:posfs-1))))//''',''' &
                                                    //(trim(adjustl(rec(posfs+1:reclen))))//''',' 
                else                                                    ! switch to datamode 
                   call str_upper(keyvaluestr,len(trim(keyvaluestr)))   ! case insensitive format 
                   bcBlockId = ecInstanceCreateBCBlock(InstancePtr)
                   bcBlockPtr=>ecSupportFindBCBlock(instancePtr, bcBlockId)
                   fileReaderId = ecInstanceCreateFileReader(InstancePtr)
                   fileReaderPtr => ecSupportFindFileReader(instancePtr, fileReaderID)
                   fileReaderPtr%bc => bcBlockPtr
                   fileReaderPtr%ofType = provFile_bc
                   if (.not.(processhdr_all_quantities(bcBlockPtr,nfld,nq,keyvaluestr))) then
                     return  ! dumb translation of bc-object metadata  
                   endif
!                  if (.not.(checkhdr(bcBlockPtr))) return    ! skip check                       ! check on the contents of the bc-object       
                   bcBlockPtr%fname = fname 
                   bcBlockPtr%ftype=BC_FTYPE_ASCII                                               ! set BC-Block filetype to ASCII

                   !if (bcBlockPtr%func==BC_FUNC_TSERIES) then 
                   !   if (.not.ecSupportTimestringToUnitAndRefdate(bcBlockPtr%timeunit, unit, ref_date)) then 
                   !      return
                   !   endif           ! Parsing the time string failed 
                   !endif              ! Timeseries function 

                   if (present(k_refdate) .and. present(k_timezone) .and. present(k_timestep_unit)) then 
                      if (present(dtnodal)) then
                         if (.not.ecProviderInitializeTimeFrame(fileReaderPtr, k_refdate, k_timezone, k_timestep_unit, dtnodal)) return
                      else
                         if (.not.ecProviderInitializeTimeFrame(fileReaderPtr, k_refdate, k_timezone, k_timestep_unit)) return
                      endif
                   else
                      if (.not.ecProviderInitializeTimeFrame(fileReaderPtr, -1, 0.d0, ec_second)) return
                   endif

                   if (ecSupportOpenExistingFileGnu(bcBlockPtr%fhandle, fname)) then
                      call mf_backspace(bcBlockPtr%fhandle, savepos)           ! set newly opened file to the appropriate position 
                      count = count + 1 
                      iostat = EC_NOERR
                   else 
                      call mf_close(bcBlockPtr%fhandle)
                      iostat = EC_DATA_NOTFOUND
                   end if
                   jaheader = .false.
                endif 
             endif          ! in header mode (data lines are ignored) 
          endif             ! not a new '[forcing]' item  
       endif                ! non-empty string 
    enddo                   ! read/scan loop, ended when we reached end-of-file
    ! Create the items 
    do ifr = instancePtr%nFileReaders-count+1,instancePtr%nFileReaders                   ! The latest set of filereaders = latest set of bcBlocks       
       success = items_from_bc_quantities(instancePtr,instancePtr%ecFileReadersPtr(ifr)%ptr)
    enddo

    end function collectbc_all
!============================================================================================================================
    
    !> Given a character string of key-value pairs gathered from a header block,
    !> extract all relevant fields (and find the block of requested quantity)
    !> and fill a bc-object

    
    !> Under construction: one bc-quantity instance for each quantity, but with a vector dimensionality of 1 and number of layers 1. 
    !> For now use the existing processhdr, with only 1 quantity that can be a user-defined vector (actually containing multiple quantities)
    function processhdr_all_quantities(bc,nfld,nq,keyvaluestr) result (success) 
    implicit none 
    logical                                 ::      success 
    type (tEcBCBlock),  intent(inout)       ::      bc              !< boundary condition instance   
    integer,            intent(in)          ::      nfld            !< number of fields in the header 
    integer,            intent(in)          ::      nq              !< number of quantities (quantity block) 
    character(len=*),   intent(in)          ::      keyvaluestr     !< string containing header fields as key-value pairs            

    integer                          ::     ifld 
    integer                          ::     i, jv
    integer                          ::     iostat
    character(len=60),  allocatable  ::     hdrkeys(:)     !< All keys from header
    character(len=60),  allocatable  ::     hdrvals(:)     !< All values from header 
    integer, allocatable             ::     iv(:), il(:), perm_vpos(:)
    character(len=60)                ::     dumstr 

    integer                          ::     ipos, npos, posfs  
    integer                          ::     iq, iq_sel, idim
    integer, parameter               ::     MAXDIM = 10    !< max number of vector quantities in one vector 
    character(len=10)                ::     vectorquantities(MAXDIM) 
    character(len=60)                ::     vectordefinition, vectorstr   

    success = .False.
    if (allocated(hdrkeys)) then 
       deallocate(hdrkeys)
    endif 
    if (allocated(hdrvals)) then 
       deallocate(hdrvals)
    endif 
    if (allocated(bc%quantities)) then 
       deallocate(bc%quantities)
    endif 
    allocate(hdrkeys(nfld),hdrvals(nfld))
    allocate(bc%quantities(nq))                    ! individual quantity objects 

    ! RL666: TODO: support vectors and layers, grouping of columns into quantities 
    do iq = 1,nq 
       allocate(bc%quantities(iq)%jacolumn(nq))
       allocate(bc%quantities(iq)%col2elm(nq))     ! RL66 check if this one is still needed
       bc%quantities(iq)%col2elm = -1 
       bc%quantities(iq)%col2elm(iq) = 1 
       bc%quantities(iq)%jacolumn = .false.        
       bc%quantities(iq)%jacolumn(iq) = .true.    
    enddo

    hdrvals=''
    hdrkeys=''
    read(keyvaluestr,*,iostat=iostat) dumstr,(hdrkeys(ifld),hdrvals(ifld),ifld=1,nfld)
    iq = 0                              
    iq_sel = 0
    do ifld=1,nfld
       call replace_char(hdrkeys(ifld),ichar('-'),ichar(' '))
       call replace_char(hdrkeys(ifld),ichar('_'),ichar(' '))
       call replace_char(hdrkeys(ifld),ichar('.'),ichar(' '))
       select case (trim(adjustl(hdrkeys(ifld))))
          case ('NAME')
               bc%bcname = trim(hdrvals(ifld))
          case ('QUANTITY')
               iq = iq + 1                                 ! count quantities, corresponds with column numbers [iq]
               bc%quantities(iq)%name = trim(hdrvals(ifld)) 
               select case (bc%func)                       ! some quantities have a distinct meaning, require interpretation later
               case (BC_FUNC_TSERIES, BC_FUNC_TIM3D)
                  if (trim(hdrvals(ifld))=='TIME') then    ! special check on the time field 
                     bc%timecolumn = iq 
                  endif 
               case (BC_FUNC_CONSTANT)
               case default
                  call setECMessage("Collecting items from BC file currently only supports time series")
                  return
               end select                                          
          case ('UNIT')
               bc%quantities(iq)%unit = trim(hdrvals(ifld))
               if (iq==bc%timecolumn) then                 ! Is this the unit of time ? 
                  bc%timeunit = trim(hdrvals(ifld))        ! store timeunit string in this bc instance 
               endif 
          case ('FUNCTION')
               if (iq>0) cycle 
               select case (trim(adjustl(hdrvals(ifld))))
                  case ('TIMESERIES')
                     bc%func = BC_FUNC_TSERIES
                  case ('CONSTANT')                        ! Constant is a special version of time-series (Sobek3)
                     bc%func = BC_FUNC_CONSTANT
                  case ('T3D')
                     bc%func = BC_FUNC_TIM3D
               end select 
          case ('OFFSET')                           
               if (iq>0) cycle 
               read(hdrvals(ifld),*) bc%quantities(iq)%offset
          case ('FACTOR')
               if (iq>0) cycle 
               read(hdrvals(ifld),*) bc%quantities(iq)%factor
          case ('MISSING VALUE DEFINITION')
               read(hdrvals(ifld),*) bc%missing
          case ('TIME INTERPOLATION')
               select case (trim(adjustl(hdrvals(ifld))))
                  case ('LINEAR')
                     bc%timeint = BC_TIMEINT_LIN
                  case ('BLOCK-TO')
                     bc%timeint = BC_TIMEINT_BTO
                  case ('BLOCK-FROM')
                     bc%timeint = BC_TIMEINT_BFROM
               end select  
       end select 
    enddo 

    bc%numcols = iq
    ! Fill bc%quantity%col2elm(nq) which holds the mapping of columns in the file to vector positions
    bc%numcols = iq
    do iq = 1, nq 
       bc%quantities(iq)%col2elm = -1 
       bc%quantities(iq)%col2elm(iq) = 1
    enddo 
    
    deallocate(hdrkeys)
    deallocate(hdrvals)
    success = .True.
    end function processhdr_all_quantities

end module m_ec_bccollect
