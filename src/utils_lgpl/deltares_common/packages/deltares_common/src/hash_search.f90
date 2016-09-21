module m_hash_search
   
   use m_alloc
   use messageHandling
   
   implicit none
   
   public hashfill
   public hashsearch
   public dealloc
   public hashsearch_or_add
   public hashfill_init
   
   interface dealloc
      module procedure deallochashtable
   end interface

   type, public :: t_hashlist
      integer :: hashcon = 1009
      integer :: id_count = 0
      character(len=idLen), allocatable, dimension(:) :: id_list
      integer, allocatable, dimension(:) :: hashfirst
      integer, allocatable, dimension(:) :: hashnext
   end type
   
   contains   
   
   subroutine deallochashtable(hashlist)
      type(t_hashlist) :: hashlist
      
      if (allocated(hashlist%hashfirst)) then
         deallocate(hashlist%hashfirst)
         deallocate(hashlist%hashnext)
         if (allocated(hashlist%id_list)) then
            deallocate(hashlist%id_list)
         endif
      endif
      hashlist%id_count= 0
   end subroutine deallochashtable
   
   
   integer function hashfun(string, hashcon)

      ! Hashing function
      ! Original by: Geert Prinsen
      ! Module description: Modified version of the hashing system used
      !                     in SOBEK_LITE/PLUVIUS. 

      character(*), intent(in)               :: string
      integer, intent(in)                    :: hashcon
      
      integer                                :: ires
      integer                                :: length
      integer                                :: i
      ires = 0
      length = len_trim(string)

      do i = 1, length
         ires = ishft(ires,4) + ires+ iachar(string(i:i))
      enddo
      ires = iabs(ires)
      ires = mod (ires, hashcon)
      
      if (ires == 0) ires = hashcon
      
      hashfun = ires

   end function hashfun

   subroutine hashfill(hashlist)
 
      ! Module description: Fill hashing arrays
      use string_module
 
      ! Global Variables
      type(t_hashlist), intent(inout) :: hashlist

      ! Local Variables
      integer                                  :: icount
      integer                                  :: hashcode
      integer                                  :: inr
      integer                                  :: next
      integer                                  :: ierr
      character(len=idLen)                     :: locid
      integer ires
      
      call realloc(hashlist%hashfirst, hashlist%hashcon, lindex = 0, stat = ierr)
      call aerr('hashfirst(0:hashcon - 1)', ierr, hashlist%hashcon)
    
      call realloc(hashlist%hashnext, hashlist%id_count, stat = ierr)
      call aerr('hashnext(id_count)', ierr, hashlist%id_count)
      
      hashlist%hashfirst = 0
      hashlist%hashnext  = 0

      do icount = 1, hashlist%id_count
      
         locid = hashlist%id_list(icount)
         if (len_trim(locid) == 0) then
            cycle
         endif
         
         call str_upper(locid)
         
         hashcode = hashfun(locid, hashlist%hashcon)
  
         !      write(*,*) ' Hashfill ', id,' ', hashcode
  
         if (hashlist%hashfirst(hashcode) .eq. 0) then
            
            hashlist%hashfirst(hashcode) = icount
            hashlist%hashnext(icount)    = 0
           
         else
            
            inr  = hashlist%hashfirst(hashcode)
            next = hashlist%hashnext(inr)
           
            do while (next .ne. 0)
               inr  = next
               next = hashlist%hashnext (inr)
            enddo
           
            hashlist%hashnext(inr) = icount
            
         endif
 
      end do
      
   end subroutine hashfill
 
   subroutine hashfill_init(hashlist, count)
   
      type(t_hashlist), intent(inout) :: hashlist
      integer, intent(in) :: count
      
      integer                                  :: ierr
      
      call realloc(hashlist%hashfirst, hashlist%hashcon, lindex = 0, stat = ierr)
      call aerr('hashfirst(0:hashcon - 1)', ierr, hashlist%hashcon)
    
      call realloc(hashlist%hashnext, count, stat = ierr)
      call aerr('hashnext(id_count)', ierr, count)
      
      hashlist%hashfirst = 0
      hashlist%hashnext  = 0
      hashlist%id_count  = 0
   end subroutine hashfill_init
   
   subroutine hashfill_inc(hashlist, ind)
 
      ! Module description: Fill hashing arrays
      use string_module
 
      ! Global Variables
      type(t_hashlist), intent(inout) :: hashlist
      integer, intent(in) :: ind

      ! Local Variables
      integer                                  :: icount
      integer                                  :: hashcode
      integer                                  :: inr
      integer                                  :: next
      integer                                  :: ierr
      character(len=idLen)                     :: locid
      integer ires
      

      locid = hashlist%id_list(ind)
      call str_upper(locid)
      
      hashcode = hashfun(locid, hashlist%hashcon)
  
      !      write(*,*) ' Hashfill ', id,' ', hashcode
  
      if (hashlist%hashfirst(hashcode) .eq. 0) then
         
         hashlist%hashfirst(hashcode) = icount
         hashlist%hashnext(icount)    = 0
        
      else
         
         inr  = hashlist%hashfirst(hashcode)
         next = hashlist%hashnext(inr)
        
         do while (next .ne. 0)
            inr  = next
            next = hashlist%hashnext (inr)
         enddo
        
         hashlist%hashnext(inr) = icount
         
      endif

   end subroutine hashfill_inc

 !  inode = hashsearch(node_local_id, nodefirst, nodenext, node_id)
   
   integer function hashsearch(hashlist, id)
 
      ! Module description: Search in hashing arrays
      use string_module
      ! Global Variables
      character(len=*), intent(in)           :: id
      type(t_hashlist), intent(in)           :: hashlist
      
      ! Local Variables
      character(len=idLen)                             :: locid
      character(len=idLen)                             :: idtest
      integer                                         :: hashcode
      integer                                         :: inr
      integer                                         :: next
      integer                                         :: ifound
 
      ifound = -1
      locid  = id
      call str_upper(locid)
      
      hashcode = hashfun(locid, hashlist%hashcon)
 
      !   write(*,*) ' Hashsearch', id, hashcode
 
      if (hashlist%hashfirst(hashcode) > 0) then
 
        inr  = hashlist%hashfirst(hashcode)
        next = inr
        
        do while (next .ne. 0)
           
          idtest = hashlist%id_list(next)
          call str_upper (idtest)
          
          if (locid .ne. idtest) then
            inr  = next
            next = hashlist%hashnext (inr)
          else
            ifound = next
            exit
          endif
          
        enddo
        
      else
         ! 'Hash search failed'
      endif

      hashsearch = ifound
 
   end function hashsearch
   
   integer function hashsearch_or_add(hashlist, id)
 
      ! Module description: Search in hashing arrays
      use string_module
      ! Global Variables
      character(len=*), intent(in)           :: id
      type(t_hashlist), intent(inout)        :: hashlist
      
      ! Local Variables
      character(len=idLen)                             :: locid
      character(len=idLen)                             :: idtest
      integer                                         :: hashcode
      integer                                         :: inr
      integer                                         :: next
      integer                                         :: ifound
 
      ifound = -1
      locid  = id
      call str_upper(locid)
      
      hashcode = hashfun(locid, hashlist%hashcon)
 
      !   write(*,*) ' Hashsearch', id, hashcode
 
      if (hashlist%hashfirst(hashcode) > 0) then
 
        inr  = hashlist%hashfirst(hashcode)
        next = inr
        
        do while (next .ne. 0)
           
          idtest = hashlist%id_list(next)
          call str_upper (idtest)
          
          if (locid .ne. idtest) then
            inr  = next
            next = hashlist%hashnext (inr)
          else
            ifound = next
            exit
          endif
          
        enddo
        
      else
         ! 'Hash search failed'
      endif
      
         if (ifound<0) then
         hashlist%id_count = hashlist%id_count+1
         hashlist%id_list(hashlist%id_count) = id
         ifound = hashlist%id_count 
         
         if (hashlist%hashfirst(hashcode) .eq. 0) then
         
            hashlist%hashfirst(hashcode) = ifound
            hashlist%hashnext(ifound)    = 0
        
         else
         
            inr  = hashlist%hashfirst(hashcode)
            next = hashlist%hashnext(inr)
        
            do while (next .ne. 0)
               inr  = next
               next = hashlist%hashnext (inr)
            enddo
        
            hashlist%hashnext(inr) = ifound
         
         endif
      endif         
      
      hashsearch_or_add = ifound
 
   end function hashsearch_or_add

end module m_hash_search
