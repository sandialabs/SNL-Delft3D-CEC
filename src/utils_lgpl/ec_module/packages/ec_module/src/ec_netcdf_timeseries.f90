module m_ec_netcdf_timeseries
    use precision 
    use m_ec_parameters
    use m_ec_stringbuffer
    use m_ec_support
    use m_ec_message
    use m_ec_typedefs
    use netcdf
    use m_alloc 
    use multi_file_io
    use string_module
    implicit none 
          
    private 

    public   :: ecNetCDFCreate
    public   :: ecNetCDFInit
    public   :: ecNetCDFFree
    public   :: ecNetCDFFree1dArray
    public   :: ecNetCDFScan
    public   :: ecNetCDFGetTimeseriesValue
    public   :: ecNetCDFGetAttrib
    public   :: ecNetCDFGetVectormax

   contains 

    !> Construct a new netCDF instance with the specified id.
    !! Failure is indicated by returning a null pointer.
    function ecNetCDFCreate(netCDFId) result(netCDFPtr)
       type(tEcNetCDF), pointer            :: netCDFPtr !< the new netCDF instance, intent(out)
       integer,                 intent(in) :: netCDFId  !< unique netCDF instance id
       !
       integer :: istat   !< allocate() status
       integer :: i       !< loop counter
       logical :: success !< helper variable
       !
       success = .false.
       !
       ! allocation
       allocate(netCDFPtr, stat = istat)
       if (istat == 0) then
          ! see what's allocatable in this object and add it here 
          ! ...... 
       end if
       if (istat /= 0) then
          call setECMessage("ec_bcreader::ecNetCDFCreate: Unable to allocate additional memory.")
          netCDFPtr => null()
          return
       end if
       ! initialization
       netCDFPtr%id = netCDFId
    end function ecNetCDFCreate

    ! NetCDF (for timeseries) destructor 
    function ecNetCDFFree(netcdf) result(success)
       implicit none
       logical                         :: success !< function status
       type(tEcNetCDF), intent(inout)  :: netcdf  !< intent(inout)
       integer                         :: ierr 
    
       success = .False.
       ierr = nf90_close(netcdf%id)
       if (ierr/=NF90_NOERR) then 
          continue
          ! TODO: issue a warning here 
       endif 
       if (allocated(netcdf%standard_names)) then      
          deallocate(netcdf%standard_names)
       endif 
       if (allocated(netcdf%tsid)) then      
          deallocate(netcdf%tsid)
       endif 
       if (allocated(netcdf%tsid)) then      
          deallocate(netcdf%tsid)
       endif 

       success = .True.
    end function ecNetCDFFree
    
    ! =======================================================================
    
    !> Frees a 1D array of tEcFileReaderPtrs, after which the ptr is deallocated.
    function ecNetCDFFree1dArray(ptr, nNetCDFs) result (success)
       logical                                       :: success      !< function status
       type(tEcNetCDFPtr), dimension(:), pointer     :: ptr          !< intent(inout)
       integer, intent(inout)                        :: nNetCDFs     !< number of NetCDF objects
       !
       integer :: i      !< loop counter
       integer :: istat  !< deallocate() status
       !
       success = .true.
       !
       if (associated(ptr)) then
          ! Free and deallocate all tEcFileReaderPtrs in the 1d array.
          do i=1, nNetCDFs
             if (ecNetCDFFree(ptr(i)%ptr)) then
                deallocate(ptr(i)%ptr, stat = istat)
                if (istat /= 0) success = .false.
             else
                success = .false.
             end if
          end do
          ! Finally deallocate the tEcNetCDFPtr(:) pointer.
          if (success) then
             deallocate(ptr, stat = istat)
             if (istat /= 0) success = .false.
          end if
       end if
       nNetCDFs = 0
    end function ecNetCDFFree1dArray
    
    
    !> Initialize NetCDF instance 
    function ecNetCDFInit (ncname, ncptr, iostat) result (success)   
    use string_module
    implicit none 
!   Open a netCDF file, store ncid, standard names ....
!   Open only if this file is not already opened, so check the list of nc-objects first and return a pointer ....
    logical                                        :: success
    character(len=*),              intent(in)      :: ncname       
!   type (tEcNetCDF),              intent(inout)   :: nc              
    type (tEcNetCDF),              pointer         :: ncptr              
    integer, optional,             intent(out)     :: iostat
    character(len=50)                              :: name, cf_role , positive, units

    integer    :: iDims, nDims, iVars, iTims, nVars, nTims, nGlobalAtts, unlimdimid, ierr 
    integer    :: nts, tslen
    integer    :: dimids_tsid(2)
    integer, allocatable :: var_dimids(:,:)
    integer, allocatable :: var_ndims(:)

    
    success = .false.

    ierr = nf90_open(trim(ncname), NF90_NOWRITE, ncptr%ncid)
    ierr = nf90_inquire(ncptr%ncid, nDims, nVars, nGlobalAtts, unlimdimid)
    ncptr%nDIms = nDims
    ncptr%nVars = nVars
    allocate (ncptr%dimlen(nDims))

    ncptr%ncname =  ncname 
    iostat = 0                                                                              ! not yet used, placeholder 
    do iDims = 1, nDims
       ierr = nf90_inquire_dimension(ncptr%ncid, iDims, name, ncptr%dimlen(iDims))
    enddo 
    allocate (ncptr%standard_names(nVars))
    ncptr%standard_names = ''
    allocate(var_dimids(nDims, nVars)) ! NOTE: nDims is only an upper bound here!
    allocate(var_ndims(nVars))
    var_ndims = 0
    do iVars = 1, nVars                                                                     ! Inventorize variables 
       ierr = nf90_inquire_variable(ncptr%ncid,iVars,name=ncptr%standard_names(iVars))      ! Variable name as fallback 
       ierr = nf90_get_att(ncptr%ncid,iVars,'standard_name',ncptr%standard_names(iVars))    ! Standard name if available 
       ierr = nf90_inquire_variable(ncptr%ncid,iVars,ndims=var_ndims(iVars),dimids=var_dimids(:,iVars))

       ! Check for important var: was it the stations?
       cf_role = ''
       ierr = nf90_get_att(ncptr%ncid,iVars,'cf_role',cf_role)
       if (cf_role == 'timeseries_id') then 
             nDims = 0                
             ierr = nf90_inquire_variable(ncptr%ncid, iVars, ndims = nDims)
             if (nDims==2) then                                                             ! If cf Role 'timeseries_id' found, compose an index timeseries id's 
                ierr = nf90_inquire_variable(ncptr%ncid, iVars, dimids = dimids_tsid)
                tslen = ncptr%dimlen(dimids_tsid(1))                                        ! timeseries ID length 
                nTims = ncptr%dimlen(dimids_tsid(2))                                        ! number of timeseries IDs  
                ncptr%nTims = nTims
                allocate (ncptr%tsid(nTims))
                tslen = min(tslen,len(ncptr%tsid(1)))
                ncptr%tsid = ''
                do iTims=1,nTims
                   ierr = nf90_get_var(ncptr%ncid, iVars, ncptr%tsid(iTims), (/1,iTims/),(/tslen,1/)) 
                   call replace_char(ncptr%tsid(iTims), 0, 32) ! Replace NULL char by whitespace: iachar(' ') == 32
                end do
                ncptr%tsidid  = iVars                                                       ! For convenience also store the Station ID explicitly 
                ncptr%tsiddimid = dimids_tsid(2)                                            ! For convenience also store the Station's dimension ID explicitly 
             else 
                ! timeseries_id has the wrong dimensionality
                return  
             endif 
       end if

       ! Check for important var: was it time?
       if (ncptr%standard_names(iVars) == 'time') then
          ierr = nf90_get_att(ncptr%ncid,iVars,'units',ncptr%timeunit)                     ! Store the unit string of the time variable 
          ncptr%timeid    = iVars                                                          ! For convenience also store the ID explicitly 
          ncptr%timedimid = var_dimids(1,iVars)
       endif 

       ! Check for important var: was it layering?
       positive = ''
       ierr = nf90_get_att(ncptr%ncid,iVars,'positive',positive)
       if (len_trim(positive) > 0) then ! Identified a layercoord variable, by its positive:up/down attribute
          ! NOTE: officially, a vertical coord var may also be identified by a unit of pressure, but we don't support that here.
          ncptr%layerid    = iVars
          ncptr%layerdimid = var_dimids(1,iVars)                                          ! For convenience also store the dimension ID explicitly 
       end if
       
    enddo 

    !VECTORMAX SUPPORT WILL BE IMPLEMENTED LATER 
    !vectormax = 1 ! default
    !do iVars = 1, nVars                                                                     ! Inventorize variables 
    !   ierr = nf90_inquire_variable(ncptr%ncid,iVars,ndims=var_ndims, dimids=var_dimids)
    !   n = var_ndims(iVar)
    !   if (n <= 1) then
    !      cycle ! Timeseries should at least have the station *and* time dimension
    !   end if
    !
    !   if (var_dimids(n,iVars) == ncptr%ts_dimid) then        ! Check 1: This var is defined on the stations
    !      if (var_dimids(n-1,iVars) == ncptr%time_dimid) then ! Check 2: this var is also defined in the time dimension
    !         if (n >= 3) then
    !            if (var_dimids(n-2,iVars) == ncptr%layer_dimid) then ! Check 3: This vasr is also defined in the layering dimension
    !               idimvectcandidate = n-3
    !            else
    !               idimvectcandidate = n-2
    !            end if
    !            
    !            if (idimvectcandidate >= 1) then
    !               ! This remaining dimension is then by default interpreted as a vectormax dimension for this var.
    !               var_vectormax(iVar) = ncptr%dimlen(idimvectcandidate) ! This var has a third dimension, must be the vectormax
    !            end if
    !            ! TODO: warning if var contains even more dimensions (var_ndims>4)
    !         end if ! n>=3 dim
    !      end if ! time dim
    !   end if ! station dim
    !enddo 

    success = .True.
    end function ecNetCDFInit

    !> Scan netcdf instance for a specific quantity name and location label 
    function ecNetCDFScan (ncptr, quantity, location, q_id, l_id) result (success)  
    use string_module
    implicit none
    logical                          :: success
    type (tEcNetCDF),   pointer      :: ncptr              
    character(len=*),   intent(in)   :: quantity
    character(len=*),   intent(in)   :: location
    integer, intent(out)             :: q_id
    integer, intent(out)             :: l_id
    integer    ::    ivar, itim, ltl  , i

    success = .False. 
    do ivar=1,ncptr%nVars
       ltl = len_trim(quantity)
       if (ncptr%standard_names(ivar)(1:ltl)==quantity(1:ltl)) exit 
    enddo 
    if (ivar<=ncptr%nVars) then 
       q_id = ivar
    else 
       q_id = -1 
    endif 
    do itim=1,ncptr%nTims
       ltl = len_trim(location)

       if (strcmpi(ncptr%tsid(itim), location)) exit ! Found
    enddo 
    if (itim<=ncptr%nTims) then 
       l_id = itim
    else 
       l_id = -1 
    endif 
    if (l_id<=0 .or. q_id<=0) then 
       return                 ! l_id<0 means : location not found, q_id<0 means quantity not found 
    endif  
    !  TODO: Retrieve relevant variable attributes and store them in the bc-instance this netcdf is connected to  
    success = .True. 
    end function ecNetCDFScan
   
    ! Reader of timeseries to be implemented here .... 
    function ecNetCDFGetTimeseriesValue (ncptr, q_id, l_id, timelevel, nctime, ncvalue) result (success)   
    logical                          :: success
    type (tEcNetCDF),   pointer      :: ncptr              
    integer, intent(in)              :: q_id
    integer, intent(in)              :: l_id
    integer, intent(in)              :: timelevel 
    integer                          :: ierr           
    double precision, intent(out), dimension (:)   :: nctime 
    double precision, intent(out), dimension (:)   :: ncvalue 
    
    success = .False.
    ierr = nf90_get_var(ncptr%ncid,q_id,ncvalue(1:1),(/l_id,timelevel/),(/1,1/))          ! get time 
    if (ierr/=NF90_NOERR) return 
    ierr = nf90_get_var(ncptr%ncid,ncptr%timeid,nctime(1:1),(/timelevel/),(/1,1/))         ! get one single data value 
    if (ierr/=NF90_NOERR) return 
    success = .True.
    end function ecNetCDFGetTimeseriesValue

    !> Acquire the vectormax, vector dimensionality, from the variable's metadata (attribute:vectormax)
    function ecNetCDFGetVectormax (ncptr, q_id, vectormax) result (success)   
    implicit none
    logical                          :: success 
    type (tEcNetCDF),   pointer      :: ncptr              
    integer, intent(in)              :: q_id
    integer, intent(out)             :: vectormax 
    integer                          :: ierr 
    
    character(len=10)   :: str
    success = .False. 
    ierr = nf90_get_att(ncptr%ncid,q_id,'vectormax',str)
    if (ierr/=NF90_NOERR) return 
    read(str,*,iostat=ierr) vectormax
    if (ierr/=0) return 
    success = .True. 
    end function ecNetCDFGetVectormax

    !> Scan netcdf instance for a specific quantity name and location label 
    !> Retrieve an arbitrary attrib from the variable's metadata (returns attribute string)
    function ecNetCDFGetAttrib (ncptr, q_id, attribname, attribvalue) result (success)   
    implicit none
    logical                          :: success 
    type (tEcNetCDF),   pointer      :: ncptr              
    integer, intent(in)              :: q_id
    character(len=*), intent(in)     :: attribname
    character(len=*), intent(out)    :: attribvalue 
    integer                          :: ierr 
    
    success = .False. 
    ierr = nf90_get_att(ncptr%ncid,q_id,trim(attribname),attribvalue)
    if (ierr/=0) return 
    success = .True. 
    end function ecNetCDFGetAttrib
    
end module m_ec_netcdf_timeseries