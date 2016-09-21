module m_ec_bcreader
  use precision
  use m_ec_parameters
  use m_ec_stringbuffer
  use m_ec_support
  use m_ec_message
  use m_ec_typedefs
  use m_alloc
  use multi_file_io
  use string_module
  implicit none

  private

  public   ::  ecBCInit
  public   ::  ecBCReadLine
  public   ::  ecBCBlockCreate
  public   ::  ecBCBlockFree
  public   ::  jakeyvalue
  public   ::  jakeyvaluelist
  public   ::  processhdr
  public   ::  checkhdr
  public   ::  sortndx

contains

  ! =======================================================================

  !> Initialize BC instance
  function ecBCInit (instancePtr, fname, quantityName, plilabel, bc, iostat) result (success)
    use m_ec_netcdf_timeseries
    implicit none
    !   Open a .bc file with external forcings
    !   Scrolls the headers until it finds quantity, location and .... are matched
    !   Advances the filepointer upto the first line of data, setting all necesary fields of bc-object
    !   Leaves the file open and return the filehandle fhandle (in a bc-object) to caller
    logical                                        :: success
    type(tEcInstance),             pointer         :: instancePtr  !< intent(in)
    character(len=*),              intent(in)      :: fname
    character(len=*),              intent(in)      :: quantityName
    character(len=*),              intent(in)      :: plilabel
    type (tEcBCBlock),             intent(inout)   :: bc
    integer, optional,             intent(out)     :: iostat

    integer :: netCDFId, iNetCDF
    integer :: vectormax
    type(tEcQuantity), pointer :: qptr

    success = .false.
    bc%quantity => null()
    bc%qname = quantityName
    bc%bcname = plilabel
    bc%fname = fname                                  ! store original filename for later referece (?)
    call str_upper(bc%qname,len(trim(bc%qname)))
    call str_upper(bc%bcname,len(trim(bc%bcname)))
    !
    select case (bc%ftype)
    case (BC_FTYPE_ASCII)
       if (.not.ecSupportOpenExistingFileGnu(bc%fhandle, bc%fname)) then
          return
       end if
       if (scanbc_ascii(bc, bc%fhandle, iostat)) then          ! parsing the open bc-file
          allocate(bc%columns(bc%numcols))
       else
          call mf_close(bc%fhandle)
          return                                               ! quantityName-plilabel combination not found
       endif
    case (BC_FTYPE_NETCDF)
       bc%func = BC_FUNC_TSERIES                               ! For the time being we only support (1d, vectormax=1) timeseries
       ! in combination with netcdf
       if (.not.ecNetCDFscan(bc%ncptr, quantityName, plilabel, bc%ncvarndx, bc%nclocndx)) then
          return                                               ! quantityName-plilabel combination not found
       endif
       ! TODO:
       ! Harvest the netCDF and the selected variable for metadata, using ecNetCDFGetAttrib
       ! parse them and store in the BC instance, analogous to processhdr for the ASCII BC-files
       if (ecNetCDFGetVectormax (bc%ncptr, bc%ncvarndx, vectormax)) then
          bc%quantity%vectormax = vectormax
       endif
       bc%quantity%vectormax  = 1                              ! To be removed later, when we enable vectormax>1 for netcdf files
       !if (ecNetCDFGetAttrib (bc%ncptr, q_id, attribname, attribvalue)) then
       !endif

       bc%timeunit = bc%ncptr%timeunit
    end select
    success=.true.
  end function ecBCInit


  ! =======================================================================

  !> Find quantity-pli point combination, preparing a BC-block using an ASCII-file as input
  function scanbc_ascii(bc, fhandle, iostat) result (success)
    implicit none
    logical                                 :: success
    type (tEcBCBlock),      intent(inout)   :: bc
    integer (kind=8),       intent(in)      :: fhandle
    integer,                intent(out)     :: iostat

    character*(255)     ::  rec
    integer             ::  reclen
    integer             ::  commentpos
    character(len=:),   &
         allocatable   ::  keyvaluestr                         ! all key-value pairs in one header
    integer             ::  posfs
    integer             ::  nfld
    integer             ::  nq
    logical             ::  jablock, jaheader
    integer             ::  reallocstat
    integer             ::  lineno
    integer (kind=8)    ::  savepos
    integer             ::  iostatloc

    success = .false.
    iostat = EC_UNKNOWN_ERROR
    lineno = 0
    savepos = 0

    do
       if (mf_eof(fhandle)) then
          ! print *,'End of BC FILE'                            ! reached the end of the bc-file, but combination was not found
          iostat = EC_EOF
          return
       endif
       !      read(fhandle,'(a200)', iostat=iostat) rec
       call mf_read(fhandle,rec,savepos)
       iostatloc = 0 ! mf_read always ok?
       if (iostatloc /= 0) then
          iostat = iostatloc
          return ! beter break?
       endif
       lineno = lineno + 1
       reclen = len_trim(rec)                                  ! duplicate with strip_comment in ec_filereader_read, but otherwise circular dependency
       commentpos = index(rec(1:reclen),'//')
       if (commentpos>0) reclen = min(reclen,commentpos-1)
       commentpos = index(rec(1:reclen),'%')
       if (commentpos>0) reclen = min(reclen,commentpos-1)
       commentpos = index(rec(1:reclen),'#')
       if (commentpos>0) reclen = min(reclen,commentpos-1)
       commentpos = index(rec(1:reclen),'*')
       if (commentpos>0) reclen = min(reclen,commentpos-1)
       commentpos = index(rec(1:reclen),'!')
       if (commentpos>0) reclen = min(reclen,commentpos-1)

       if (len_trim(rec(1:reclen))>0) then                     ! skip empty lines
          if (index(rec,'[forcing]')>0) then                   ! new boundary chapter
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

                   !                  keyvaluestr = trim(keyvaluestr)//(trim(adjustl(rec(1:posfs-1))))//',' &
                   !                                                 //(trim(adjustl(rec(posfs+1:reclen))))//','
                   keyvaluestr = trim(keyvaluestr)//''''// (trim(adjustl(rec(1:posfs-1))))//''',''' &
                        //(trim(adjustl(rec(posfs+1:reclen))))//''','
                else                                                    ! switch to datamode
                   call str_upper(keyvaluestr,len(trim(keyvaluestr)))   ! case insensitive format
                   if (jakeyvalue(keyvaluestr,'NAME',trim(bc%bcname))) then
                      if (jakeyvalue(keyvaluestr,'FUNCTION','ASTRONOMIC').or.jakeyvalue(keyvaluestr,'FUNCTION','ASTRONOMIC-CORRECTION')) then
                         ! Check for harmonic or astronomic components
                         jablock = .true.
                         jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY','ASTRONOMIC COMPONENT')
                         jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY',trim(bc%qname)//' '//'AMPLITUDE')
                         jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY',trim(bc%qname)//' '//'PHASE')
                      elseif (jakeyvalue(keyvaluestr,'FUNCTION','HARMONIC').or.jakeyvalue(keyvaluestr,'FUNCTION','HARMONIC-CORRECTION')) then
                         ! Check for harmonic or harmonic components
                         jablock = .true.
                         jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY','HARMONIC COMPONENT')
                         jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY',trim(bc%qname)//' '//'AMPLITUDE')
                         jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY',trim(bc%qname)//' '//'PHASE')
                      elseif (jakeyvalue(keyvaluestr,'FUNCTION','QHTABLE')) then
                         ! Check for qh
                         jablock = .true.
                         jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY',trim(bc%qname)//' '//'WATERLEVEL')
                         jablock = jablock .and. jakeyvalue(keyvaluestr,'QUANTITY',trim(bc%qname)//' '//'DISCHARGE')
                      elseif (jakeyvalue(keyvaluestr,'FUNCTION','T3D')) then
                         ! Check for timeseries on sigma- or z-levels
                         jablock = jakeyvalue(keyvaluestr,'QUANTITY',bc%qname) .or. jakeyvaluelist(keyvaluestr,'VECTOR',bc%qname)
                      elseif (jakeyvalue(keyvaluestr,'FUNCTION','TIMESERIES')) then
                         ! Check for timeseries
                         jablock = jakeyvalue(keyvaluestr,'QUANTITY',bc%qname) .or. jakeyvaluelist(keyvaluestr,'VECTOR',bc%qname)
                      endif
                      if (jablock) then                                        ! block confirmed
                         if (.not.processhdr(bc,nfld,nq,keyvaluestr)) return   ! dumb translation of bc-object metadata
                         if (.not.checkhdr(bc)) return                         ! check on the contents of the bc-object
                         call mf_backspace(fhandle, savepos)      ! Rewind the first line with data
                         success = .true.
                         iostat = EC_NOERR
                         return
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
                endif                                          ! switch to datamode
             endif          ! in header mode (data lines are ignored)
          endif             ! not a new '[forcing]' item
       endif                ! non-empty string
    enddo                   ! read/scan loop
  end function scanbc_ascii

  function jakeyvaluelist(keyvaluestr,key,value) result (jafound)
    implicit none
    logical                          :: jafound
    character(len=*),    intent(in)  :: keyvaluestr
    character(len=*),    intent(in)  :: key
    character(len=*),    intent(in)  :: value
    jafound = (index(keyvaluestr,','''//trim(key)//''','''//trim(value)//':')>0)            ! like 'keyword1','value1 : one,two,three','keyword',..... etc
  end function jakeyvaluelist

  function jakeyvalue(keyvaluestr,key,value) result (jafound)
    implicit none
    logical                          :: jafound
    character(len=*),    intent(in)  :: keyvaluestr
    character(len=*),    intent(in)  :: key
    character(len=*),    intent(in)  :: value
    jafound = (index(keyvaluestr,','''//trim(key)//''','''//trim(value)//''',')>0)            ! like 'keyword1','value1','keyword',.....
  end function jakeyvalue

  !> Given a character string of key-value pairs gathered from a header block,
  !> extract all relevant fields (and find the block of requested quantity)
  !> and fill a bc-object
  function processhdr(bc,nfld,nq,keyvaluestr) result (success)
    implicit none
    logical                                 ::      success
    type (tEcBCBlock),  intent(inout)       ::      bc              !< boundary condition instance
    integer,            intent(in)          ::      nfld            !< number of fields in the header
    integer,            intent(in)          ::      nq              !< number of quantities (quantity block)
    character(len=*),   intent(in)          ::      keyvaluestr     !< string containing header fields as key-value pairs

    integer                          ::     ifld
    integer                          ::     i, jv
    integer                          ::     iostat
    character(len=maxNameLen),  allocatable   ::     hdrkeys(:)     !< All keys from header
    character(len=maxRecordLen),  allocatable ::     hdrvals(:)     !< All values from header
    integer, allocatable             ::     iv(:), il(:), perm_vpos(:)
    character(len=maxRecordLen)      ::     dumstr

    integer                          ::     ipos, npos, posfs
    integer                          ::     iq, iq_sel, idim
    integer, parameter               ::     MAXDIM = 10    !< max number of vector quantities in one vector
    character(len=maxNameLen)        ::     vectorquantities(MAXDIM)
    character(len=maxNameLen)        ::     vectordefinition, vectorstr

    success = .False.
    if (allocated(hdrkeys)) then
       deallocate(hdrkeys)
    endif
    if (allocated(hdrvals)) then
       deallocate(hdrvals)
    endif

    if (associated(bc%quantity)) then
       if (allocated(bc%quantity%jacolumn)) then
          deallocate(bc%quantity%jacolumn)
       endif
       if (allocated(bc%quantity%col2elm)) then
          deallocate(bc%quantity%col2elm)
       endif
       deallocate(bc%quantity)
    endif
    allocate(bc%quantity)
    allocate(hdrkeys(nfld),hdrvals(nfld))
    allocate(bc%quantity%jacolumn(nq))
    allocate(bc%quantity%col2elm(nq))
    allocate(iv(nq),il(nq))
    iv = -1
    il = -1
    bc%quantity%col2elm = -1
    hdrvals=''
    hdrkeys=''
    vectordefinition = ''
    read(keyvaluestr,*,iostat=iostat) dumstr,(hdrkeys(ifld),hdrvals(ifld),ifld=1,nfld)
    iq = 0
    iq_sel = 0
    do ifld=1,nfld
       call replace_char(hdrkeys(ifld),ichar('-'),ichar(' '))
       call replace_char(hdrkeys(ifld),ichar('_'),ichar(' '))
       call replace_char(hdrkeys(ifld),ichar('.'),ichar(' '))
       select case (trim(adjustl(hdrkeys(ifld))))
       case ('QUANTITY')
          iq = iq + 1                                     ! count quantities, corresponds with column numbers [iq]
          if (trim(hdrvals(ifld))==bc%qname) then         ! detected quantity of interest
             bc%quantity%name=bc%qname
             bc%quantity%jacolumn(iq)=.true.
             iq_sel = iq_sel + 1
             il(iq) = 1                                  ! layer this column belongs to, default 1
             iv(iq) = 1                                  ! iv is the number of the element in the vector
             bc%quantity%col2elm(iq_sel) = iq
          else if (index(vectordefinition,'|'//trim(hdrvals(ifld))//'|')>0) then         ! quantity is part of requested vector
             posfs = index(vectordefinition,'|'//trim(hdrvals(ifld))//'|')
             bc%quantity%jacolumn(iq)=.true.
             iq_sel = iq_sel + 1
             il(iq) = 1                                  ! layer this column belongs to, default 1
             iv(iq) = 0                                  ! iv is the number of the element in the vector
             do jv=1,posfs
                if (vectordefinition(jv:jv)=='|') iv(iq) = iv(iq) + 1                    ! count number of | preceding the quantitie.
             enddo
          else
             bc%quantity%jacolumn(iq)=.false.
             select case (bc%func)
             case (BC_FUNC_TSERIES, BC_FUNC_TIM3D)
                if (trim(hdrvals(ifld))=='TIME') then    ! special check on the time field
                   bc%timecolumn = iq
                endif
             case (BC_FUNC_HARMONIC, BC_FUNC_ASTRO, BC_FUNC_HARMOCORR, BC_FUNC_ASTROCORR, BC_FUNC_CMP3D)
                if (trim(hdrvals(ifld))=='HARMONIC COMPONENT') then          ! harmonic component
                   bc%quantity%astro_component_column = iq
                endif
                if (trim(hdrvals(ifld))=='ASTRONOMIC COMPONENT') then        ! astronomic component label
                   bc%quantity%astro_component_column = iq
                endif
                if (trim(hdrvals(ifld))==trim(bc%qname)//' AMPLITUDE') then  ! amplitude field for astronomic/harmonic components
                   bc%quantity%astro_amplitude_column = iq
                endif
                if (trim(hdrvals(ifld))==trim(bc%qname)//' PHASE') then      ! phase field for astronomic/harmonic components
                   bc%quantity%astro_phase_column = iq
                endif
             case (BC_FUNC_QHTABLE)
                if (trim(hdrvals(ifld))==trim(bc%qname)//' WATERLEVEL') then ! waterlevel field for qh-boundary
                   bc%quantity%qh_waterlevel_column = iq
                endif
                if (trim(hdrvals(ifld))==trim(bc%qname)//' DISCHARGE') then  ! discharge field for qh-boundary
                   bc%quantity%qh_discharge_column = iq
                endif
             end select
          endif
       case ('VECTOR')
          vectorstr = trim(hdrvals(ifld))
          posfs = index(vectorstr,':')
          if (posfs>0) then
             if (trim(vectorstr(1:posfs-1))==trim(bc%qname)) then           ! this vector defines the requested 'quantity'
                vectorquantities = ''
                read (vectorstr(posfs+1:len_trim(vectorstr)),*,iostat=iostat)  (vectorquantities(idim),idim=1,MAXDIM)
                vectordefinition = '|'
                bc%quantity%vectormax=1
                do idim=1,MAXDIM
                   vectordefinition=trim(vectordefinition)//trim(vectorquantities(idim))//'|'
                   if (len_trim(vectorquantities(idim))>0) bc%quantity%vectormax=idim
                enddo
             endif
          endif
       case ('UNIT')
          if (bc%quantity%jacolumn(iq)) then
             bc%quantity%unit = trim(hdrvals(ifld))
          endif
          if (iq==bc%timecolumn) then                     ! Is this the unit of time ?
             bc%timeunit = trim(hdrvals(ifld))            ! store timeunit string in this bc instance
          endif
          if (iq == bc%quantity%astro_component_column) then
             bc%timeunit = trim(hdrvals(ifld))            ! store period/feq unit in time unit
          endif
       case ('FUNCTION')
          if (iq>0) cycle
          select case (trim(adjustl(hdrvals(ifld))))
          case ('TIMESERIES')
             bc%func = BC_FUNC_TSERIES
          case ('HARMONIC')
             bc%func = BC_FUNC_HARMONIC
          case ('ASTRONOMIC')
             bc%func = BC_FUNC_ASTRO
          case ('HARMONIC-CORRECTION')
             bc%func = BC_FUNC_HARMOCORR
          case ('ASTRONOMIC-CORRECTION')
             bc%func = BC_FUNC_ASTROCORR
          case ('T3D')
             bc%func = BC_FUNC_TIM3D
          case ('C3D')
             bc%func = BC_FUNC_CMP3D
          case ('QHTABLE')
             bc%func = BC_FUNC_QHTABLE
          end select
       case ('OFFSET')
          if (iq>0) cycle
          read(hdrvals(ifld),*) bc%quantity%offset
       case ('FACTOR')
          if (iq>0) cycle
          read(hdrvals(ifld),*) bc%quantity%factor
       case ('VERTICAL POSITION')
          read(hdrvals(ifld),*) il(iq)
          bc%quantity%vertndx = il(iq)                          ! layer this column belongs to, default 1
       case ('VERTICAL POSITION SPECIFICATION')
          npos=0
          if (len_trim(hdrvals(ifld))>0) then
             npos = count([(verify(hdrvals(ifld)(i:i),', ')>0   &
                  .and.verify(hdrvals(ifld)(i-1:i-1),', ')==0, i=2,len_trim(hdrvals(ifld)))]) + 1
          endif
          allocate(bc%vp(npos))
          bc%numlay = npos
          read(hdrvals(ifld),*) (bc%vp(ipos),ipos=1,npos)       ! globally store ALL vertical positions
          allocate(perm_vpos(npos))
          call sortndx(bc%vp,perm_vpos,npos)                    ! produce the permutation that sorts the vertical positions perm_vpos
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
       case ('VERTICAL INTERPOLATION')
          select case (trim(adjustl(hdrvals(ifld))))
          case ('LINEAR')
             bc%zInterpolationType = zinterpolate_linear
          case ('LOG')
             bc%zInterpolationType = zinterpolate_log
          case ('BLOCK')
             bc%zInterpolationType = zinterpolate_block
          case default
             bc%zInterpolationType = zinterpolate_unknown
          end select
       case ('VERTICAL POSITION TYPE')
          IF (index(hdrvals(ifld),'PERCEN')+index(hdrvals(ifld),'BED')>0) then
             hdrvals(ifld)='PERCBED'
          endif
          select case (trim(adjustl(hdrvals(ifld))))
          case ('PERCBED')
             bc%vptyp = BC_VPTYP_PERCBED
          case ('ZDATUM')
             bc%vptyp = BC_VPTYP_ZDATUM
          case ('BEDSURF')
             bc%vptyp = BC_VPTYP_BEDSURF
          case ('PERCSURF')
             bc%vptyp = BC_VPTYP_PERCSURF
          case ('ZBED')
             bc%vptyp = BC_VPTYP_ZBED
          case ('ZSURF')
             bc%vptyp = BC_VPTYP_ZSURF
          end select
       end select
    enddo

    ! Fill bc%quantity%col2elm(nq) which holds the mapping of columns in the file to vector positions
    bc%quantity%col2elm(iq) = -1
    bc%numcols = iq
    do iq = 1, nq
       if (iv(iq)>0) then
          bc%quantity%col2elm(iq) = (il(iq)-1)*bc%quantity%vectormax + iv(iq)
          if (allocated(bc%vp)) then
             bc%quantity%col2elm(iq) = (perm_vpos(il(iq))-1)*bc%quantity%vectormax + iv(iq)
          else
             bc%quantity%col2elm(iq) = iv(iq)
          endif
       else
          bc%quantity%col2elm(iq) = -1
       endif
    enddo

    deallocate(hdrkeys)
    deallocate(hdrvals)
    deallocate(iv,il)
    success = .True.
  end function processhdr


  !> Given a filled bc-object, scrutinize its content for completeness, validity and consistency
  function checkhdr(bc) result (success)
    implicit none
    logical                                     ::      success
    type (tEcBCBlock),         intent(inout)    ::      bc
    !
    success = .False.
    ! If function is tseries (not intended for the 3rd dimension, but numlay>1, indicating a specification of
    ! vertical position, assume that the function label is a mistake and change it into T3D
    if (bc%numlay>1 .and. bc%func==BC_FUNC_TSERIES) then
       bc%func=BC_FUNC_TIM3D
    end if

    ! Check vectors
    if (bc%func==BC_FUNC_TSERIES .or. bc%func==BC_FUNC_TIM3D) then        ! in case of timeseries-like signal  ...
       if (bc%numcols-1/=bc%numlay*bc%quantity%vectormax) then           ! ... the number of columns minus 1 (time column) should equal ...
          print *, 'vectormax = ', bc%quantity%vectormax
          print *, 'numlay = ', bc%numlay
          call setECMessage("Number of selected column mismatch.")
          return                                                          ! ... the vectordimensionality * number of layers
       end if
    endif

    success = .True.
  end function checkhdr

  !> Read the next record from a *.bc file.
  !> Requests a line from the EcBC object's stringbuffer block, advancing its pointer in the block
  function ecBCReadLine(fileReaderPtr, values, time_steps, recout) result(success)
    use m_ec_netcdf_timeseries
    implicit none
    logical                                                 :: success       !< function status
    type(tEcFileReader),    pointer                         :: fileReaderPtr
    real(hp), optional,                       intent(inout) :: time_steps    !< number of time steps of duration: seconds
    real(hp), dimension(:), optional,         intent(inout) :: values        !< vector of values for a BC_FUNC_TSERIES
    character(len=*), optional,               intent(out)   :: recout        !< line prepared from input for caller

    !
    type(tEcBCBlock),           pointer                     :: bcPtr
    integer        :: n_col      !< number of columns in the file, inferred from the number of quantity blocks in the header
    integer        :: n_col_time !< position of the dedicated time-column in the line, deduced from the header
    character(256) :: rec        !< content of a line
    character(30)  :: ncolstr
    integer        :: istat      !< status of read operation
    integer        :: i, j       !< loop counters
    integer        :: reclen
    integer        :: commentpos
    integer(kind=8):: savepos    !< saved position in file, for mf_read to enabled rewinding
    real(kind=hp), dimension(1:1)  :: ec_timesteps ! to read in source time from file block
    real(kind=hp), dimension(1:1)  :: time ! to read in source time from file block
    real(kind=hp)  :: amplitude

    bcPtr => fileReaderPtr%bc
    
    success = .false.

    select case (bcPtr%ftype)
    case (BC_FTYPE_ASCII)
       n_col = bcPtr%numcols             ! Number of quantities = columns specified in the header
       n_col_time = bcPtr%timecolumn     ! Rank number of the colunm presumably holding the time (not necessarily the first)

       if(present(recout)) then
          recout = ''                       ! initialize return string to empty
       endif
       rec = ''
       reclen = len_trim(rec)
       do while(reclen==0)
          if (mf_eof(bcPtr%fhandle)) then
             select case (BCPtr%func)
             case (BC_FUNC_TSERIES, BC_FUNC_TIM3D, BC_FUNC_CONSTANT)
                call setECMessage("   File: "//trim(bcPtr%fname)//", Location: "//trim(bcPtr%fname)//", Quantity: "//trim(bcPtr%qname))
                call setECMessage("Datablock end (eof) has been reached.")
                fileReaderPtr%end_of_data = .true.
             end select
             return
          endif

          call mf_read(bcPtr%fhandle,rec,savepos)
          reclen = len_trim(rec)

          ! deal with various comment delimiters, RL: skip it if performance is affected !
          commentpos = index(rec,'//')                                      ! Scan for various types of comments
          if (commentpos>0) reclen = min(reclen,commentpos-1)
          commentpos = index(rec,'%')
          if (commentpos>0) reclen = min(reclen,commentpos-1)
          commentpos = index(rec,'#')
          if (commentpos>0) reclen = min(reclen,commentpos-1)
          commentpos = index(rec,'*')
          if (commentpos>0) reclen = min(reclen,commentpos-1)
          commentpos = index(rec,'!')
          if (commentpos>0) reclen = min(reclen,commentpos-1)
          reclen = len_trim(rec(1:reclen))                                 ! Finally remove trailing spaces
          if (index(rec,'[forcing]'         )>0 .or. &
              index(rec,'[Boundary]'        )>0 .or. &
              index(rec,'[LateralDischarge]')>0) then ! new boundary chapter       
             select case (BCPtr%func)
             case (BC_FUNC_TSERIES, BC_FUNC_TIM3D)
                call setECMessage("   File: "//trim(bcPtr%fname)//", Location: "//trim(bcPtr%fname)//", Quantity: "//trim(bcPtr%qname))
                call setECMessage("Datablock end (new [forcing] block) has been prematurely reached.")
                fileReaderPtr%end_of_data = .true.
             end select
             return
          endif
       enddo

       !     read(rec(1:reclen), *, IOSTAT = istat) (values(i), i=1,n_col_time-1), time_steps, (values(i), i=n_col_time, n_col-1)
       BCPtr%columns(1:n_col)=''
       read(rec(1:reclen), *, IOSTAT = istat) BCPtr%columns(1:n_col)
       if (istat /= 0) then
          ! error handling, report column number i, field content columns(i) and record rec  ....
          ! TODO: hookup MessageHandlign and print rec and column stats here directly
          call setECMessage("   File: "//trim(bcPtr%fname)//", Location: "//trim(bcPtr%fname)//", Quantity: "//trim(bcPtr%qname))
          call setECMessage("ec_bcreader::ecBCReadBlock: Read failure.")
          write (ncolstr,'(a,i0,a,i0,a)') '(expecting ',n_col,' columns)'
          call setECMessage("   ''"//rec(1:reclen)//"'' "//trim(ncolstr))
          success = .false.
          return
       endif

       select case (BCPtr%func)
       case (BC_FUNC_CONSTANT)
          ec_timesteps(1) = 0.0d+0
          read (BCPtr%columns(1), *)  values(1)
       case (BC_FUNC_TSERIES, BC_FUNC_TIM3D)
          read (BCPtr%columns(n_col_time), *) ec_timesteps(1)
          ! Convert source time to kernel time:
          time_steps = ecSupportThisTimeToTimesteps(fileReaderPtr%tframe, ec_timesteps(1))
          j=0
          if (count(BCPtr%quantity%col2elm>0)==0) then                ! col2elm is not used to map the read colums, added to the vector in reading order
             do i=1,n_col
                if (BCPtr%quantity%jacolumn(i)) then
                   j=j+1
                   read(BCPtr%columns(i),*,iostat=istat) values(j)
                   values(j)=values(j)*BCPtr%quantity%factor+BCPtr%quantity%offset      ! apply using given offset and factor to this quantity
                   if (istat/=0) then
                      ! error handling, report column number i, field content columns(i) and record rec  ....
                      call setECMessage("Read failure in file: "//trim(bcPtr%fname))
                      return
                   endif
                endif
             enddo
          else
             do i=1,n_col                                                                     ! loop over columns
                if (BCPtr%quantity%col2elm(i)>0) then                                         ! if the i-th column was assigned to a vector position
                   read(BCPtr%columns(i),*,iostat=istat) values(BCPtr%quantity%col2elm(i))    ! read the i-th column into the col2vectpos(i)-th element
                   values(BCPtr%quantity%col2elm(i)) = values(BCPtr%quantity%col2elm(i))        &
                        &                                                   * BCPtr%quantity%factor                    &
                        &                                                   + BCPtr%quantity%offset
                   if (istat>0) then
                      call setECMessage("   line = "//trim(rec))
                      call setECMessage("Read failure in file: "//trim(bcPtr%fname))
                      return
                   endif
                endif
             enddo
          endif
       case (BC_FUNC_ASTRO,BC_FUNC_HARMONIC,BC_FUNC_ASTROCORR,BC_FUNC_HARMOCORR)
          ! Produce a record of component, amplitude, phase extracted from rec
          ! Apply the factor to the amplitude column only
          read(BCPtr%columns(BCPtr%quantity%astro_amplitude_column),*,iostat=istat) amplitude
          if (istat==0) then
             amplitude = amplitude * BCPtr%quantity%factor
             write(BCPtr%columns(BCPtr%quantity%astro_amplitude_column),*) amplitude
          endif

          ! construct a new record
          recout =                     trim(BCPtr%columns(BCPtr%quantity%astro_component_column))
          recout = trim(recout)//'  '//trim(BCPtr%columns(BCPtr%quantity%astro_amplitude_column))
          recout = trim(recout)//'  '//trim(BCPtr%columns(BCPtr%quantity%astro_phase_column))
       case (BC_FUNC_QHTABLE)
          ! Produce a record of waterlevel, discharge extracted from rec
          recout =                     trim(BCPtr%columns(BCPtr%quantity%qh_discharge_column))
          recout = trim(recout)//'  '//trim(BCPtr%columns(BCPtr%quantity%qh_waterlevel_column))
       end select

    case (BC_FTYPE_NETCDF)
       if (BCPtr%nctimndx>BCPtr%ncptr%dimlen(BCPtr%ncptr%timedimid)) then
          call setECMessage("Datablock end (eof) has been reached in file: "//trim(bcPtr%fname))
          return
       endif

       if (.not.ecNetCDFGetTimeseriesValue (BCPtr%ncptr,BCPtr%ncvarndx,BCPtr%nclocndx,BCPtr%nctimndx,ec_timesteps,values)) then
          call setECMessage("Read failure in file: "//trim(bcPtr%fname))
          return
       else
          BCPtr%nctimndx = BCPtr%nctimndx + 1
          time_steps = ecSupportThisTimeToTimesteps(fileReaderPtr%tframe, ec_timesteps(1))
       endif
    case default
       call setECMessage("Invalid filetype set for file: "//trim(bcPtr%fname)//' (internal EC-error)')
       return
    end select

    success = .true.
  end function ecBCReadLine

  ! =======================================================================

  ! Realloc sub and oldfil function to be removed lateron,
  ! only included for standalone-testing
  ! realloc can be found in m_alloc (deltares common)
  ! oldfil to be replaced by ! ecSupportOpenExistingFile(fhandle, fname)
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  integer function oldfil(fname, maxunit, iostat) result(unit)
    implicit none
    character(len=*),       intent(in)      :: fname
    integer,                intent(in)      :: maxunit
    integer,                intent(out)     :: iostat

    logical     :: opened

    iostat = 0                ! ran out of free file handles
    do unit=1, maxunit
       inquire(unit,opened=opened)
       if (.not.opened) then
          exit
       endif
    enddo
    if (unit<=maxunit) then
       open(unit, file=trim(fname), status='old', iostat=iostat)
    else
       iostat = -66           ! ran out of free file handles
    endif
    ! .....
    return
  end function oldfil

  ! =======================================================================

  !> Construct a new BCBlock instance with the specified id.
  !! Failure is indicated by returning a null pointer.
  function ecBCBlockCreate(bcBlockId) result(bcBlockPtr)
    type(tEcBCBlock), pointer            :: bcBlockPtr !< the new BCBlock, intent(out)
    integer,                  intent(in) :: bcBlockId  !< unique BCBlock id
    !
    integer :: istat   !< allocate() status
    integer :: i       !< loop counter
    logical :: success !< helper variable
    !
    success = .false.
    !
    ! allocation
    allocate(bcBlockPtr, stat = istat)
    if (istat == 0) then
       ! see what's allocatable in this object and add it here
       ! ......
    end if
    if (istat /= 0) then
       call setECMessage("ec_bcreader::ecBCBlockCreate: Unable to allocate additional memory.")
       bcBlockPtr => null()
       return
    end if
    ! initialization
    bcBlockPtr%id = bcBlockId
  end function ecBCBlockCreate

  ! BCBlock destructor
  function ecBCBlockFree(bcblock) result(success)
    implicit none
    logical                         :: success !< function status
    type(tEcBCBlock), intent(inout) :: bcblock !< intent(inout)

    success = .False.
    if (associated(bcblock%ncptr)) then
       bcblock%ncptr => null()
       ! TODO: also clean up netcdf instance somewhere (or is this not necessary)
    endif

    if (bcblock%fhandle>0) then
       call mf_close(bcblock%fhandle)
    endif

    if (associated(bcblock%ncptr)) then
       bcblock%ncptr => null()
    endif
    if (allocated(bcblock%columns)) then
       deallocate(bcblock%columns)
    endif
    if (allocated(bcblock%vp)) then
       deallocate(bcblock%vp)
    endif

    if (.not.ecBCQuantityFree(bcblock%quantity)) then
       return                                        ! TODO: issue a warning
    endif

    success = .True.
  end function ecBCBlockFree

  ! BCQuantity destructor
  function ecBCQuantityFree(bcquantity) result(success)
    implicit none
    logical                            :: success    !< function status
    type(tEcBCQuantity), intent(inout) :: bcquantity !< intent(inout)
    success = .False.
    if (allocated(bcquantity%jacolumn)) then
       deallocate(bcquantity%jacolumn)
    endif
    if (allocated(bcquantity%col2elm)) then
       deallocate(bcquantity%col2elm)
    endif
    if (allocated(bcquantity%astro_component)) then
       deallocate(bcquantity%astro_component)
    endif
    if (allocated(bcquantity%astro_amplitude)) then
       deallocate(bcquantity%astro_amplitude)
    endif
    if (allocated(bcquantity%astro_phase)) then
       deallocate(bcquantity%astro_phase)
    endif
    success = .True.
  end function ecBCQuantityFree


  ! =======================================================================

  subroutine sortndx(x,c,n)
    implicit none
    double precision, intent(in)    :: x(:)
    integer, intent(out)            :: c(:)
    integer, intent(in)             :: n

    integer                         :: a(n), b(n)
    integer                         :: i,j,bs
    double precision                :: as

    a(1:n) = x(1:n)
    ! n = size(a,dim=1)
    do i=1,n
       b(i) = i
       c(i) = i
    enddo
    do i=2,n
       as = a(i)
       bs = b(i)
       do j = i,2,-1
          if (a(j-1)>as) then
             a(j)=a(j-1)
             b(j)=b(j-1)
             c(b(j))=c(b(j))+1
          else
             a(j)=as
             b(j)=bs
             c(i)=j
             exit
          endif
       enddo
       if (j==1) then
          a(j)=as
          b(j)=bs
          c(i)=j
       endif
    enddo
  end subroutine sortndx

end module m_ec_bcreader
