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

! $Id: removedcode.f90 65778 2020-01-14 14:07:42Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/removedcode.f90 $
!module unused_code_from_meteo1
!   function reaspwtim(minp,d,mx,nx,tread,x0r,y0r) result(success)
!   !
!   ! Read spiderweb time and field
!   !
!   use m_wind
!   
!   implicit none
!   !
!   integer                    :: i
!   integer                    :: j
!   integer                    :: minp
!   integer                    :: mx
!   integer                    :: nx
!   double precision           :: pdrop
!   double precision           :: tread, tspwref
!   double precision           :: x0r
!   double precision           :: y0r
!   double precision           :: dmiss
!   double precision, dimension(:,:,:) :: d
!   character(132)             :: rec
!   character(4)               :: tspwyear
!   character(2)               :: tspwmonth, tspwday, tspwhour, tspwminute, tspwsec
!   integer                    :: tok, tok1, tok2 
!   double precision           :: tokf
!   logical                    :: success
!   !
!   success = .true.
!   if ( size(d,1) .ne. mx .or. size(d,2) .ne. nx ) then
!      errormessage = 'REASPWTIM: wrong sizes'
!      success = .false.
!      return
!   endif
!
!   ! Read the time
!   read (minp,'(a)',end=100) rec
!   tok1 = index( rec, '='      , success )
!   tok2 = index( rec, 'minutes', success )
!   tokf = 1d0
!   if (tok2.eq.0) then
!       tok2 = index( rec, 'hours', success )
!       tokf = 60d0
!   endif
!   if (tok1.eq.0 .or. tok2.eq.0) then
!       ! WvB, TODO: give appropriate error message
!       success = .false.
!       return
!   endif
!   read( rec(tok1+1:tok2-1), * ) tread
!   tread      = tread*tokf
!   
!   ! Read the reference time
!   tok  = index( rec, 'since' , success )
!   read( rec(tok+ 6:tok+ 9), * ) tspwyear
!   read( rec(tok+11:tok+12), * ) tspwmonth
!   read( rec(tok+14:tok+15), * ) tspwday
!   read( rec(tok+17:tok+18), * ) tspwhour
!   read( rec(tok+20:tok+21), * ) tspwminute
!   read( rec(tok+23:tok+24), * ) tspwsec
!   call maketimeinverse(tspwyear//tspwmonth//tspwday//tspwhour//tspwminute//tspwsec, tspwref)
!   tread      = tread + tspwref/60d0
!
!   ! Read the x-coordinate of the cyclone eye
!   read (minp,'(a)',end=100) rec
!   tok  = index( rec, '='      , success )
!   read( rec(tok+1:), * ) x0r
!   
!   ! Read the y-coordinate of the cyclone eye
!   read (minp,'(a)',end=100) rec
!   tok  = index( rec, '='      , success )
!   read( rec(tok+1:), * ) y0r
!   
!   ! Read the pressure drop of the cyclone eye
!   read (minp,'(a)',end=100) rec
!   tok  = index( rec, '='      , success )
!   read( rec(tok+1:), * ) pdrop
!   
!   ! Read the actual data
!   do j = 2,nx
!      read(minp,*,end = 100, err=201) ( d(i,j,1), i = 1,mx-1 )
!   enddo
!   do j = 2,nx
!      read(minp,*,end = 100, err=202) ( d(i,j,2), i = 1,mx-1 )
!   enddo
!   do j = 2,nx
!      read(minp,*,end = 100, err=203) ( d(i,j,3), i = 1,mx-1 )
!   enddo
!   
!   ! Fill central point
!   d( 1:mx-1, 1, 1)  = 0d0                        ! magnitude
!   d( 1:mx-1, 1, 2)  = d( 1:mx-1, 2, 2)           ! direction
!   d( 1:mx-1, 1, 3)  = pdrop                      ! pressure drop
!   
!   ! Fill 360 degrees
!   d(     mx, :, :)  = d( 1, :, :)
!    
!   ! Compensate for unit of pressure (mbar versus Pa, through patmfac)
!   d( :, :, 3)       = d( :, :, 3)*patmfac
!   
!   success = .true.
!   return
!100 continue
!   errormessage = 'Unexpected end of file in spiderweb wind file'
!   success = .false.
!   return
!101 continue
!   write(errormessage,'(2a)') 'Error reading time in spiderweb wind file : ', trim(rec)
!   success = .false.
!   return
!102 continue
!   write(errormessage,'(2a)') 'Error reading x0, y0, pdrop of cyclone eye : ', trim(rec)
!   success = .false.
!   return
!201 continue
!   errormessage = 'Error reading cyclone wind-u field'
!   success = .false.
!   return
!202 continue
!   errormessage = 'Error reading cyclone wind-v field'
!   success = .false.
!   return
!203 continue
!   errormessage = 'Error reading cyclone pressure drop field'
!   success = .false.
!   return
!end function reaspwtim
!
!function readarcinfoheader(minp      ,mmax      ,nmax      ,x0        ,y0        , &
!                         & dxa       ,dya       ,dmiss      ) result(success)
!!!--description-----------------------------------------------------------------
!! NONE
!!!--pseudo code and references--------------------------------------------------
!! NONE
!!!--declarations----------------------------------------------------------------
!
!    use precision
!    !
!    implicit none
!!
!! Global variables
!!
!    integer               :: minp
!    integer , intent(out) :: mmax
!    integer , intent(out) :: nmax
!    double precision, intent(out) :: dxa
!    double precision, intent(out) :: dya
!    double precision, intent(out) :: dmiss
!    double precision, intent(out) :: x0
!    double precision, intent(out) :: y0
!    logical               :: success
!!
!! Local variables
!!
!    integer        :: jacornerx
!    integer        :: jacornery
!    integer        :: k
!    integer        :: l
!!    integer        :: numbersonline
!    character(132) :: rec
!!
!!! executable statements -------------------------------------------------------
!!
!    jacornerx = 0
!    jacornery = 0
!  
!   10 continue
!    read (minp, '(A)', end = 100) rec
!   
!   
!    if (index(rec,'### START OF HEADER') > 0) then  ! new d3dflow header
!   
!        20 continue
!        read (minp, '(A)', end = 100) rec
!        
!        if (index(rec, 'NODATA_value') > 0) then
!           L = index(rec,'=') + 1
!           read (rec(L:), *, err = 101) dmiss
!        endif
!        
!        if (index(rec, 'n_cols') > 0) then
!           L = index(rec,'=') + 1
!           read (rec(L:), *, err = 101) mmax
!        endif
!
!        if (index(rec, 'n_rows') > 0) then
!           L = index(rec,'=') + 1
!           read (rec(L:), *, err = 101) nmax
!        endif
!
!        if (index(rec, 'x_llcenter') > 0) then
!           L = index(rec,'=') + 1
!           read (rec(L:), *, err = 103) x0
!        endif
!
!        if (index(rec, 'x_llcorner') > 0) then
!           L = index(rec,'=') + 1
!           read (rec(L:), *, err = 103) x0
!           jacornerx = 1
!        endif
!
!        if (index(rec, 'y_llcenter') > 0) then
!           L = index(rec,'=') + 1
!           read (rec(L:), *, err = 104) y0
!        endif
!
!        if (index(rec, 'y_llcorner') > 0) then
!           L = index(rec,'=') + 1
!           read (rec(L:), *, err = 104) y0
!           jacornery = 1 
!        endif
!
!       if (index(rec, 'dx') > 0) then
!           L = index(rec,'=') + 1
!           read (rec(L:), *, err = 105) dxa
!        endif
!
!       if (index(rec, 'dy') > 0) then
!           L = index(rec,'=') + 1
!           read (rec(L:), *, err = 106) dya
!        endif
!
!
!        if (index(rec,'### END OF HEADER') == 0) then  ! new d3dflow header
!           goto 20 
!        else 
!           dxa = dxa + 1 - 1
!        endif
!       
!         !
!       
!     
!   
!    else 
!    
!        if (rec(1:1)=='*' .or. rec(2:2)=='*') goto 10
!        read (rec(13:), *, err = 101) mmax
!        read (minp, '(A)', end = 100) rec
!        read (rec(13:), *, err = 102) nmax
!        !
!        read (minp, '(A)', end = 100) rec
!        read (rec(13:), *, err = 103) x0
!        jacornerx = 0
!        if (index(rec, 'corner')/=0) jacornerx = 1
!        !
!        read (minp, '(A)', end = 100) rec
!        read (rec(13:), *, err = 104) y0
!        jacornery = 0
!        if (index(rec, 'corner')/=0) jacornery = 1
!        !
!        read (minp, '(A)', end = 100) rec
!        l = index(rec, 'cellsize') + 8
!        k = numbersonline(rec(l:))
!        if (k==1) then
!           read (rec(13:), *, err = 105) dxa
!           dya = dxa
!           jacornery = jacornerx
!        else
!           read (rec(13:), *, err = 105) dxa, dya
!        endif
!        read (minp, '(A)', end = 100) rec
!        read (rec(13:), *, err = 106) dmiss
!        !
!        ! Data in an arcinfo grid file is always defined in the cell centres.
!        ! Data is assumed to be given at the points x0+i*dx,y0+j*dy
!        ! If the x0/y0 line contains the word corner, the corner coordinates
!        ! have been specified in the file. Therefore, shift x0 and y0 by half
!        ! a grid cell to the cell centres.
!        !
!    
!    endif
!    
!    if (jacornerx .eq. 1) x0 = x0 + dxa/2
!    if (jacornery .eq. 1) y0 = y0 + dya/2
!    
!    !
!    success = .true.
!    return
!    !
!    ! error handling
!    !
!  100 continue
!   errormessage = 'Unexpected end of file while reading header of arcinfo wind file'
!   goto 999
!  101 continue
!   write(errormessage,'(2a)') 'Looking for ncols (arc-info), but getting',trim(rec)
!   goto 999
!  102 continue
!   write(errormessage,'(2a)') 'Looking for nrows (arc-info), but getting',trim(rec)
!   goto 999
!  103 continue
!   write(errormessage,'(2a)') 'Looking for xll (arc-info), but getting',trim(rec)
!   goto 999
!  104 continue
!   write(errormessage,'(2a)') 'Looking for yll (arc-info), but getting',trim(rec)
!   goto 999
!  105 continue
!   write(errormessage,'(2a)') 'Looking for cellsize (dx, dy) (arc-info), but getting',trim(rec)
!   goto 999
!  106 continue
!   write(errormessage,'(2a)') 'Looking for missing value (arc-info), but getting',trim(rec)
!   goto 999
!  999 continue
!   success = .false.
!   return
!end function readarcinfoheader
!
! 
! 
!function reaspwheader(minp      ,mx        ,nx        ,dxa        ,dya        , &
!                      & mncoor, dmisval    ) result(success)
!!!--description-----------------------------------------------------------------
!! NONE
!!!--pseudo code and references--------------------------------------------------
!! NONE
!!!--declarations----------------------------------------------------------------
!
!    use m_wind
!    use precision
!    implicit none
!!
!! Global variables
!!
!    integer               :: minp
!    integer, intent(out)  :: mncoor
!    integer               :: mx
!    integer               :: nx
!    double precision, intent(out) :: dxa
!    double precision, intent(out) :: dya
!    double precision, intent(out) :: dmisval
!    logical               :: success
!!
!! Local variables
!!
!    integer                        :: k, tok, isdegr, jsuccess
!    double precision               :: radius, pi
!    double precision               :: earthrad
!    logical                        :: sferic
!    character(132)                 :: rec,test
!    character(20)                  :: ism
!
!!
!!
!!! executable statements -------------------------------------------------------
!!
!    mncoor   = 0
!    sferic   = .false.
!    success  = .true.
!    pi       = acos(-1d0)
!    earthrad = 6378137d0
!    jsuccess = 0
!    
!    ! Read the missing data value
!    call zoekja(minp, rec, 'NODATA_value', jsuccess)
!    tok = index( rec, '=', success )
!    read( rec(tok+1:tok+21), * ) dmisval
!    
!    ! Read the number of columns (i.e. number of winddirections, specified in spw-file)
!    call zoekja(minp, rec, 'n_cols', jsuccess)
!    tok = index( rec, '=', success )
!    read( rec(tok+1:tok+21), * ) mx
!    
!    ! Read the number of rows (i.e. number of intervals along the radius, specified in spw-file)
!    call zoekja(minp, rec, 'n_rows', jsuccess)
!    tok = index( rec, '=', success )
!    read( rec(tok+1:tok+21), * ) nx
!    
!    ! Read the spiderweb radius
!    call zoekja(minp, rec, 'spw_radius', jsuccess)
!    tok = index( rec, '=', success )
!    read( rec(tok+1:tok+21), * ) radius
!    
!    ! Read the spiderweb radius unit (meters or degrees)
!    call zoekja(minp, rec, 'spw_rad_unit', jsuccess)
!    tok = index( rec, '=', success )
!    ism = rec(tok+1:tok+21)
!    tok = index( ism, 'm', success )
!    !if (success) radius = radius/(2d0*pi*earthrad)*360d0     ! in degrees
!    
!    ! Read the pressure unit
!    call zoekja(minp, rec, 'unit3', jsuccess)
!    tok = index( rec, '=', success )
!    ism = rec(tok+1:tok+21)
!    tok = index( ism, 'mbar', success )
!    if (tok .gt. 0) then
!        patmfac  = 100d0
!    else
!        patmfac  =   1d0
!    endif
!    
!    ! Find end of header (sometimes denoted by '### END OF HEADER', sometimes not: then it just ends with 'unit3'
!    call zoekja(minp, rec, '### END OF HEADER', jsuccess)
!    if (jsuccess /= 1) then
!        rewind(minp)
!        call zoekja(minp, rec, 'unit3', jsuccess)
!    endif
!
!    ! Set sferic true
!    sferic = .true.
!
!!    if (sferic) then !hk: This has already been set by the flow grid. Here do no more than check consistency
!!       call meteosetsferic()
!!    endif
!
!    !
!    mx  = mx + 1              ! 1 KOLOM EXTRA VOOR 360 GRADEN = 0 GRADEN IVM INTERPOLATIE
!    dxa = 360d0/dble(mx - 1)  ! MX-1 INTERVALLEN IN HOEK
!    nx  = nx + 1              ! 1 RIJ EXTRA VOOR DE PUNTEN OP STRAAL = 0
!    dya = radius/dble(nx - 1) ! NX-1 INTERVALLEN IN DE STRAAL
!    success = .true.
!    return
!    !
!    ! error handling
!    !
!  100 continue
!   errormessage = 'Unexpected end of file while reading header of spiderweb wind file'
!   goto 999
!  101 continue
!   write(errormessage,'(2a)') 'Looking for web dimensions (spiderweb wind file), but getting ',trim(rec)
!   goto 999
!  102 continue
!   write(errormessage,'(2a)') 'Looking for web radius (m) (spiderweb wind file), but getting ',trim(rec)
!   goto 999
!  999 continue
!   success = .false.
!   return
!end function reaspwheader
!
!function readwindseriesfiles(minp) result(success)
!   implicit none
!   logical                  :: success
!   integer                  :: minp
!   integer, external :: numuni
!
!   success = .false.
!   minp = numuni()
!
!end function readwindseriesfiles
!
!
!function reaarc_curv_tim(minp,xq,yq,zq,indxn,wfn,kx,mxq,jdla,dmiss,tread) result(success)  ! read curv wind file and interpolate quantity in provider  
!
!   !
!   ! read arcinfo curvilinear time and field
!   !
!   implicit none
!   !
!   integer                             :: minp, kx, mxq, jdla 
!   integer, save                       :: mx, nx    ! dims of curvi data   ! todo, save is not safe
!   double precision                    :: tread, dmiss, xp, yp, wf(4), dum 
!   double precision, dimension(:)      :: xq,yq     ! dim = (mxq)      
!   double precision                    :: zq(kx*mxq)    ! dim = (kx=1,mxq)      
!   double precision, dimension(:,:)    :: wfn       ! dim = (4, mxq)   
!   integer,          dimension(:,:)    :: indxn     ! dim = (2, mxq)   
!   integer                             :: ielsetq, k, L 
!   
!   double precision, allocatable       :: vc(:,:,:)   
!   double precision, allocatable       :: xc(:,:), yc(:,:)   
!
!   character(132)                      :: rec, gridfilnam
!   character(4)                        :: keywrd
!   logical                             :: success
!   integer                             :: mgrd, mp, np, i, in, jn, inside
!   
!   
!   
!   if (jdla == 1) then  ! set up weightfactors once
!   
!      gridfilnam = ' '
!  
!   10 continue
!      read (minp, '(A)', end = 100) rec
!      if (index(rec,'END OF HEADER') == 0) then
!         if (index(rec, 'grid_file')/=0) then
!           l = index(rec, '=') + 1
!           read(rec(l:),'(a)', err = 101) gridfilnam
!         endif
!         goto 10
!      endif
!   
!      dmiss = -9999d0
!      
!      L = index( gridfilnam, '#') - 1
!      if (L == -1) L = len_trim(gridfilnam)
!      call oldfil(mgrd, trim(gridfilnam(1:L) ) )
!          
!   20 read(mgrd,'(a)') rec
!      if (index (rec,'=') == 0) goto 20
!      read(mgrd,*) mx, nx
!      read(mgrd,*) 
!
!      allocate( xc(mx,nx), yc(mx,nx))
!      CALL ECRREA(Xc,mx,nx,mx,nx,MGRD,-2d0)
!      CALL ECRREA(Yc,mx,nx,mx,nx,MGRD,-2d0)
!      call doclose(mgrd)
!      
!      do i = 1, mxq
!     
!         CALL FINDNM( xq(i), yq(i), xc, yc, mx, nx, mx, nx, INSIDE, MP, NP, IN, JN, wf)
!       
!         if (inside == 1) then
!
!            wfn(1,i)   = wf(1)    
!            wfn(2,i)   = wf(2)    
!            wfn(3,i)   = wf(3)    
!            wfn(4,i)   = wf(4)    
!
!            indxn(1,i) = mp
!            indxn(2,i) = np
!            
!         endif
!         
!      enddo 
!      
!      deallocate (xc, yc)
!      jdla = 0
!   endif
!   
!   
!   keywrd = 'TIME'
!   do
!      read (minp, '(a)', end = 999) rec
!      if (index(rec, keywrd)/=0) exit
!      cycle
!      999 continue
!      errormessage = 'Keyword TIME not found in curvi file'
!      success = .false.
!      return
!   enddo
!   L = index(rec, ')') 
!   if (L > 0) then 
!      L = L + 1
!   else 
!      L = index(rec, '=') + 1
!   endif
!   
!   read(rec(l:),*,err=998) tread
!   if (index(rec, 'HRS') .gt. 0 .or. index(rec, 'hrs') .gt. 0 .or. index(rec, 'hours') .gt. 0) then
!      tread = tread*60d0
!   endif
!   allocate ( vc(kx,mx,nx) )
!   
!   do k = 1,kx
!      success = readarcinfoblock(minp,vc(k,:,:),mx,nx,dmiss)
!   enddo
!   
!   
!   do i = 1,mxq
!   
!      mp = indxn(1,i)
!      np = indxn(2,i) 
!      if (mp > 0 .and. np > 0) then 
!         do k = 1,kx
!   
!            zq( (k-1)*mxq+i) =  wfn(1,i)*vc(k, mp  ,np)    +     & 
!                                wfn(2,i)*vc(k, mp+1,np)    +     &
!                                wfn(3,i)*vc(k, mp+1,np+1)  +     & 
!                                wfn(4,i)*vc(k, mp  ,np+1) 
!         enddo   
!      else 
!         zq( (1-1)*mxq+i) = 100000d0 
!         zq( (2-1)*mxq+i) = 0d0 
!         zq( (3-1)*mxq+i) = 0d0 
!      endif  
!         
!   enddo
!   deallocate ( vc ) 
!   
!   
!   return
!
!100 call qnerror('error reading meteo file', rec, ' ')  
!
!101 call qnerror('error reading meteo file', rec, ' ')  
!   
!   
!   
!998 call qnerror('error reading meteo file', rec, ' ')  
!
!end function reaarc_curv_tim
!
!function reaarctim(minp,d,mx,nx,tread,dmiss) result(success)
!   !
!   ! read arcinfo time and field
!   !
!   implicit none
!   !
!   integer                  :: minp
!   integer                  :: mx
!   integer                  :: nx
!   integer                  :: l
!   double precision         :: tread
!   double precision         :: dmiss
!   double precision, dimension(:,:):: d
!   character(132)           :: rec
!   character(4)             :: keywrd
!   logical                  :: success
!   !
!   if ( size(d,1) .ne. mx .or. size(d,2) .ne. nx ) then
!        errormessage = 'REAARCTIM: wrong sizes'
!        success = .false.
!        return
!   endif
!   keywrd = 'TIME'
!   do
!      read (minp, '(a)', end = 999) rec
!      if (index(rec, keywrd)/=0) exit
!      cycle
!      999 continue
!      errormessage = 'Keyword TIME not found in arcinfo file'
!      success = .false.
!      return
!   enddo
!   L = index(rec, ')') 
!   if (L > 0) then 
!      L = L + 1
!   else 
!      L = index(rec, '=') + 1
!   endif
!   
!   read(rec(l:),*,err=998) tread
!   if (index(rec, 'HRS') .gt. 0 .or. index(rec, 'hrs') .gt. 0 .or. index(rec, 'hours') .gt. 0) then
!      tread = tread*60d0
!   endif
!   success = readarcinfoblock(minp,d,mx,nx,dmiss)
!   return
!
! 998  call qnerror('error reading meteo file', rec, ' ')  
!
!end function reaarctim
!
!
!function readarcinfoblock(minp,d,mx,nx,dmiss) result(success)
!   !
!   ! read arcinfoveld
!   !
!   implicit none
!   !
!   integer                  :: minp
!   integer                  :: mx
!   integer                  :: nx
!   integer                  :: j
!   integer                  :: i
!   double precision         :: dmiss
!   double precision, dimension(:,:):: d
!   logical                  :: success
!   character(16)            :: tex
!   character(8000)          :: rec
!   !
!   if ( size(d,1) .ne. mx .or. size(d,2) .ne. nx ) then
!        errormessage = 'READARCINFOBLOCK: wrong sizes'
!        success = .false.
!        return
!   endif
!   do j = nx,1,-1
!    !  read(minp,'(a)',end=100) rec
!    !  read(rec,*,err=101) (d(i,j),i = 1,mx)
!       read(minp,*,err=101,end=100) (d(i,j),i = 1,mx)
!    enddo
!   do i = 1,mx
!      do j = 1,nx
!         if (d(i,j) .eq. dmiss) d(i,j) = dmiss_default
!      enddo
!   enddo
!   success = .true.
!   return
!   ! error handling
!100 continue
!   errormessage = 'Unexpected end of file in arcinfo file'
!   success = .false.
!   return
!101 continue
!   write(tex,'(2i8)') i,j
!   write(errormessage,'(2a)') 'Error reading arc-info block in colnr, rownr :', trim(tex)
!   success = .false.
!   return
!end function readarcinfoblock
!
!
!function readseries(minp,d,kx,tread) result(success)
!   !
!   !  Read uniform time serie
!   !  number of columns is number of dimensions
!   !
!   implicit none
!   !
!   integer                :: minp
!   integer                :: kx
!   integer                :: k
!   double precision       :: tread
!   double precision, dimension(:):: d
!   logical                :: success
!   character(132)         :: rec
!   !
!   if ( size(d,1) .lt. kx ) then
!      errormessage = 'READSERIES: wrong sizes'
!      success = .false.
!      return
!   endif
!10 read (minp,'(a)',end = 100) rec
!   if (rec(1:1) .eq. '*' .or. rec(1:1) .eq. '#' ) goto 10
!   read(rec,*,err = 101) tread, ( d(k), k = 1,kx )
!
!   
!   if (tread > 189912312359.0d0 ) then ! we estimate this is a datetimefiletype, 100 years in minutes = 52560000 minutes 
!      call maketimeinverse( rec(1:14), tread )     ! tread here in seconds
!      tread = tread/60d0                           ! tread here in minutes
!   endif
!
!   success = .true.
!   return
!100 continue
!   errormessage = 'Unexpected end of file in uniform time serie file'
!   success = .false.
!   return
!101 continue
!   write(errormessage,'(2a)') 'Error reading timeseries : ', trim(rec)
!   success = .false.
!   return
!end function readseries
!
!  ! type definitions
!  type telementset                              ! algemene roosterdefinitie, zowel voor vraag als aanbod
!     double precision, allocatable :: x(:)      !  => null() ! als size = 2, dan x0 en dx in x(1) en x(2)
!     double precision, allocatable :: y(:)      !  => null() ! als size = 2, dan y0 en dy in y(1) en y(2)
!     integer , allocatable         :: kcs(:)    !=> null() ! value = 0 : invalid point   value /= 0 : valid point
!     integer                       :: mnx       ! size of ! bedoeld only for debug now, may be removed later
!     logical                       :: sferic    ! true  = sferical coordinates
!     double precision, allocatable :: xyen(:,:) !=> null() ! optional, 'end'point outside of cells, defining search range for providers.
!  end type telementset
!
!  type tfield                                 ! definitie plaats- tijd- en veldinformatie aanbodkant
!     double precision                 :: time         ! tijd
!     integer                          :: ielset       ! pointer naar elementset bijhorend bij datagrid (plaats)
!     double precision, pointer        :: arr1d(:)     => null() ! 1-dim array veld
!     double precision, pointer        :: arr2d(:,:)   => null() ! 2-dim array veld
!     double precision, pointer        :: arr3d(:,:,:) => null() ! 3-dim array veld
!  end type tfield
!
!  type tdataprovider                          ! definitie aanbodkant
!     character(maxnamelen)    :: qid          ! id of this quantity
!     character(maxnamelen)    :: filename     ! file containing data
!     integer                  :: filetype     ! type of file
!     integer                  :: minp         ! handle to file of omi handle
!     integer                  :: it0          !
!     integer                  :: it1          ! index oude of nieuwe velden
!     integer                  :: mx           ! size of meteo fields  m
!     integer                  :: nx           !                       n
!     integer                  :: kx           !                       k
!     double precision                 :: dmiss        ! missing value
!
!
!
!
!     type(tfield), pointer    :: field(:)    => null()   ! oude en nieuwe velden, dimensie = 2
!
!     integer                  :: method       ! initially, store the desired spacetime method
!                                              ! later, these methods are collected and put in relations
!     character(len=1)         :: operand      ! operand
!     integer                  :: ielsetq      ! pointer to elementset on which this provider should give a quantity
!                                              ! only relevant for method >= 2
!     double precision, pointer :: wfn(:,:)   => null() ! weightfactors, bv (3,mnx) voor triang, (2,mnx) voor poly
!     integer,          pointer :: indxn(:,:) => null() ! indices      , bv (3,nmx) voor triang, (2,mnx) voor poly
!     integer                   :: refresh = 1 !< Whether or not weightfactors need to be updated.
!     double precision          :: julrefdatetime = 0d0 !< Julian reference date (+ time as decimal value < 1) from source data (file).
!  end type tdataprovider
!
!  type tquantity                              ! definitie vraagkant
!     character(maxnamelen)    :: qid          ! name that you give to a dataset/quantity at initialisation
!     integer                  :: ielset       ! pointer naar elementset van het bijhorend modelgrid
!     integer                  :: kx           ! vectormax. quantity kan meerdere grootheden bevatten
!                                              ! die allen op hetzelfde tijd/ruimteframe zitten
!     integer, pointer         :: providernrs(:) => null() ! welke aanbodnrs zijn allemaal additief voor deze grootheid
!  end type tquantity
!
!  type tsubdom                                ! en dit is alleen nodig voor de threadsaveheid
!     type(telementset),   pointer  :: elementsets(:)   => null() ! bevat roosters zowel van vraagkant als aanbodkant
!     type(tdataprovider), pointer  :: dataproviders(:) => null() ! bevat het aanbod
!     type(tquantity),     pointer  :: quantities(:)    => null() ! bevat de vraag
!     integer                       :: ini = 0
!  end type tsubdom
!
!!  type(tsubdom), pointer, save   :: subdoms(:)       ! as many subdoms as there are threads
!  type(tsubdom), allocatable,save :: subdoms(:)       ! as many subdoms as there are threads
!
!  integer                         :: idom             ! current threadnr = domnr
!
!
!!  type tindexweight
!!  end type tindexweight
!
!!  type(tindexweight), pointer, save :: indexweight(:) !
!
!function addelementset(elementsets, x, y, kcs, ielset, xyen) result(success)
!  implicit none
!  logical :: success
!
!  ! arguments
!  type(telementset), pointer   :: elementsets(:)
!  integer  :: n       ! dimension of x,y,kcs
!  integer  :: ielset  ! pointer to elementset nr
!  double precision :: x(:)
!  double precision :: y(:)
!  integer  :: kcs(:)
!
!  double precision, optional :: xyen(:,:)
!  double precision           :: foo
!
!  ! locals
!  integer  :: k, kk, nn, i, ja
!
!  success = .true.
!  foo = timelast
!
!  n  = size(x)
!  kk = size(elementsets)
!
!  if (n .gt. 2) then ! op stack kijken alleen als elset niet constant (dus dim > 2)
!                     ! constante elsets gaan altijd op de stack, kost weinig en is nodig voor spiderweb
!     do k = 1,kk                          ! check if elementset already present
!        nn = size(elementsets(k)%x)
!        if (nn == n) then
!           ja   = 1
!           do i = 1,nn
!              if (x(i)   .ne. elementsets(k)%x(i)   ) ja = 0
!              if (y(i)   .ne. elementsets(k)%y(i)   ) ja = 0
!              if (kcs(i) .ne. elementsets(k)%kcs(i) ) ja = 0
!           enddo
!           if (ja .eq. 1) then
!              ielset = k                  ! if equal, elementset already present, just pointer to it
!              return
!           endif
!        endif
!     enddo
!  endif
!
!  success = increaseelsetssize(kk,elementsets)  ! otherwise, allocate a new one
!
!  k = kk + 1
!  allocate (elementsets(k)%x(n) )
!  allocate (elementsets(k)%y(n) )
!  allocate (elementsets(k)%kcs(n) )
!  elementsets(k)%x      = x                  ! and fill with the given arrays
!  elementsets(k)%y      = y
!  elementsets(k)%kcs    = kcs
!  elementsets(k)%mnx    = n
!  elementsets(k)%sferic = .false.            
!  ielset = k
!  if (present(xyen)) then
!     allocate (elementsets(k)%xyen(2,n) )
!     elementsets(k)%xyen = xyen
!     ! No check whether size(xyen,1) == 2...
!  endif
!
!  return
!end function addelementset
!
!function destroyelementsets( elementsets ) result(success)
!  implicit none
!  logical :: success
!
!  ! arguments
!  type(telementset), pointer   :: elementsets(:)
!
!  ! locals
!  integer  :: k, kk
!
!  success = .true.
!
!  kk = size(elementsets)
!  do k = 1,kk
!     if (allocated(elementsets(k)%x)   ) deallocate (elementsets(k)%x )
!     if (allocated(elementsets(k)%y)   ) deallocate (elementsets(k)%y )
!     if (allocated(elementsets(k)%kcs) ) deallocate (elementsets(k)%kcs )
!     if (allocated(elementsets(k)%xyen)) deallocate (elementsets(k)%xyen )
!  enddo
!
!end function destroyelementsets
!
!function increaseelsetssize(k,elementsets) result (success)
!  implicit none
!  logical :: success
!  type(telementset), pointer   :: elementsets(:)
!  integer                      :: k
!
!  ! locals
!  type(telementset), pointer   :: helementsets(:)
!
!
!  success = .true.
!
!  if (k .ge. 1) then
!
!     ! GD: MEMORY LEAK HERE!
!     !if(associated(helementsets)) deallocate(helementsets)
!     allocate (helementsets(k))
!     success = copyelementsets(k,elementsets,helementsets)
!     deallocate (elementsets)
!  endif
!
!  ! GD: MEMORY LEAK HERE!
!  !if(associated(elementsets)) deallocate(elementsets)
!  allocate (elementsets(k+1))
!
!  if (k .ge. 1) then
!     success = copyelementsets(k,helementsets,elementsets)
!     success = destroyelementsets(helementsets)
!  endif
!
!end function increaseelsetssize
!
!
!function copyelementsets(kk,elementsets,helementsets) result(success)
!  implicit none
!  logical :: success
!  type(telementset), pointer :: elementsets(:), helementsets(:)
!  integer                    :: k,kk,nn,nh, n
!
!  success = .true.
!
!  do k = 1,kk
!     helementsets(k)%sferic  = elementsets(k)%sferic
!     helementsets(k)%mnx     = elementsets(k)%mnx
!     nn = size(  elementsets(k)%x)
!!    nh = size( helementsets(k)%x)
!!    if ( nh .eq. 0 ) then
!!    if ( .not. associated( helementsets(k)%x ) ) then
!        allocate (helementsets(k)%x(nn) )
!        helementsets(k)%x      = elementsets(k)%x   
!        allocate (helementsets(k)%y(nn) )
!        helementsets(k)%y      = elementsets(k)%y   
!        allocate (helementsets(k)%kcs(nn) )
!        helementsets(k)%kcs    = elementsets(k)%kcs   
!
!        if (allocated(elementsets(k)%xyen)) then
!            ! Officially, we also need to check on allocated(%xyen or size(%xyen) > 0
!            ! but %xyen is never a pointer to some non-matrix array.
!            allocate (helementsets(k)%xyen(2,nn) )
!            helementsets(k)%xyen = elementsets(k)%xyen
!        end if
!!    endif
!  enddo
!end function copyelementsets
!
!
!function addquantity(idom, qid, kx, x, y, kcs, numq, ielsetq, xyen ) result(success)
!  implicit none
!  logical :: success
!
!
!  ! arguments
!  integer                      :: idom          ! threadnr
!  character(len= *)            :: qid           ! name that you give to a quantity at initialisation
!  integer                      :: kx            ! vectormax of this quantity
!  double precision             :: x(:)          ! als size = 2, dan x0 en dx in x(1) en x(2)
!  double precision             :: y(:)          ! als size = 2, dan y0 en dy in y(1) en y(2)
!  integer                      :: kcs(:)        ! value = 0 : invalid point   value /= 0 : valid point
!  integer, intent (out)        :: ielsetq       ! for method >= 2 then
!
!  integer, intent (out)        :: numq          ! this is quantity nr ...
!
!  double precision, intent(in), optional :: xyen(:,:) ! cellsize of associated eleset points
!
!  ! locals
!  integer  :: k, kk
!
!  type(telementset),   pointer :: elementsets(:)   ! bevat roosters zowel van vraagkant als aanbodkant
!  type(tquantity),     pointer :: quantities(:)    ! bevat de vraag
!
!  type(tquantity), pointer     :: hquantities(:)
!
!  elementsets   => subdoms(idom)%elementsets
!  quantities    => subdoms(idom)%quantities
!
!  success = .true.
!
!  kk  = size(quantities)
!  do k = 1,kk                                 ! check if this qid is new
!     if (trim(quantities(k)%qid) .eq. trim(qid) ) then
!        if (quantities(k)%kx .ne. kx) then
!           errormessage = 'addquantity: quantity already defined with another vectormax'//trim(qid)
!           success       = .false.
!       else
!           numq    = k                           ! quantity already present, just pointer to it
!           ielsetq = quantities(k)%ielset
!           return
!        endif
!     endif
!  enddo
!                                              ! otherwise, increase size
!
!  if (kk .ge. 1) then                         ! copy if necessary
!     allocate(hquantities(kk))
!     call copyquantities(kk,quantities, hquantities)
!     deallocate (quantities)
!  endif
!
!  numq = kk + 1
!  allocate( quantities(numq) )
!
!  if (kk .ge. 1) then
!     call copyquantities(kk,hquantities, quantities) ! restore if necessary
!     deallocate( hquantities )
!  endif
!
!  quantities(numq)%qid    = qid
!  quantities(numq)%kx     = kx
!
!  if (present(xyen) ) then
!     success = addelementset(elementsets, x, y, kcs, ielsetq, xyen)
!  else
!     success = addelementset(elementsets, x, y, kcs, ielsetq)
!  endif
!
!  quantities(numq)%ielset = ielsetq
!
!
!
!  subdoms(idom)%elementsets   => elementsets
!  subdoms(idom)%quantities    => quantities
!
!end function addquantity
!
!subroutine copyquantities(kk,quantities,hquantities)
!implicit none
!type(tquantity), pointer   ::  quantities(:)
!type(tquantity), pointer   :: hquantities(:)
!integer :: kk
!hquantities(1:kk) = quantities(1:kk)
!end subroutine copyquantities
!
!function addfield( field, m, n, k, time, dmiss, ielset ) result(success)
!  implicit none
!  logical :: success
!  type(tfield), pointer     :: field
!  integer  :: m
!  integer  :: n
!  integer  :: k
!  integer  :: ielset
!  double precision :: dmiss
!  double precision :: time
!
!  success = .true.
!  field%time    = time
!  field%ielset  = ielset
!  allocate( field % arr3d(1:m,1:n,1:k) )
!  field%arr3d(1:m,1:n,1:k)   = dmiss ! 0
!  field%arr2d   => field%arr3d(:, :, 1)
!  field%arr1d   => field%arr3d(1, 1, :)
!end function addfield
!
!function destroyfield( field ) result(success)
!  implicit none
!  logical :: success
!  type(tfield)     :: field
!  success = .true.
!  if(associated(field % arr3d)) deallocate( field % arr3d )
!end function destroyfield
!
!
!
!function addprovider(idom, qid, kx, filename, filetype, method, operand, nump, ielsetq) result(success)
!  use netcdf
!  use time_module
!  implicit none
!  logical                      :: success
!
!  ! this subroutine adds a provider in domainnr=threadnr idom (use 1 of no threads)
!  ! for a requested quantity with id=qid
!
!  ! arguments
!  integer,      intent(in)     :: idom       ! threadnr = domainnr
!  character(*), intent(in)     :: qid        ! unique quantity identification
!
!  integer,      intent(in)     :: kx         ! vectormax
!
!  character(*), intent(in)     :: filename   ! file name for meteo data file
!  integer     , intent(in)     :: filetype   ! spw, arcinfo, uniuvp etc
!  integer     , intent(in)     :: method     ! time/space interpolation method
!  character(1), intent(in)     :: operand    ! operand
!  integer     , intent(in)     :: ielsetq    ! pointer to elementset on which this provider should give a quantity
!                                             ! only relevant for method >= 2
!  integer     , intent(out)    :: nump       ! index number of this provider
!
!
!  ! locals
!  integer                      :: mx
!  integer                      :: nx
!  integer                      :: minp, mncoor
!  double precision, allocatable        :: xe(:)      ! lokale elementset waarop de provider is gedefinieerd
!  double precision, allocatable        :: ye(:)      ! wordt gedimensioneerd in de leesroutine
!  integer , allocatable                :: kcse(:)    ! en daarna doorgeschoven naar addelementset
!  double precision                     :: dmiss
!  double precision                     :: x0,y0,dxa,dya
!
!
!
!  ! and extra for the triangulation/polytim :
!  double precision, allocatable        :: xe0(:)      ! dummy
!  double precision, allocatable        :: ye0(:)      ! dummy
!  integer , allocatable        :: kcse0(:)    ! indexen van aangeboden providers (meteostations)
!  character(len=maxnamelen)    :: filename0   ! file name for meteo data file
!  character(len=maxnamelen)    :: qidc        ! child qid bij multiple_uni, (gedeelte achtyer de underscore )  
!  integer                      :: filetype0   ! spw, arcinfo, uniuvp etc
!  integer                      :: num0        ! number of single meteostations in this triangulation
!  integer                      :: num02       ! 2*num0
!  integer                      :: mnx0 = 1000 ! max number of single meteostations in this triangulation
!  integer                      :: minp0       ! lun
!  integer                      :: ielsetq0    !
!  integer                      :: method0     ! if justupdate this provider can be part of another one that intp
!  character(1)                 :: operand0    ! operand
!  character(4)                 :: tex         ! stringetje
!  integer                      :: ja, k, L, mx0, nx0
!  integer                      :: num00 = 0, mgrd
!  logical                      :: jawel
!  integer                      :: ierr, kk
!  
!  integer, external            :: julday
!
!  ! and for pointering to the timespaceworld
!  type(telementset),   pointer :: elementsets(:)   ! bevat roosters zowel van vraagkant als aanbodkant
!  type(tdataprovider), pointer :: dataproviders(:) ! bevat het aanbod
!  type(tquantity),     pointer :: quantities(:)    ! bevat de vraag
!
!  success = .false.
!
!  if (mdia < 0) call newfil(mdia, 'timespace.dia')
!
!  dataproviders => subdoms(idom)%dataproviders
!  elementsets   => subdoms(idom)%elementsets
!  quantities    => subdoms(idom)%quantities
!
!  if ( filetype <= 0 .or. filetype > max_file_types ) then
!     errormessage = 'In addprovider: file '//filename//' filetype unknown'
!     success = .false.
!     return
!  endif
!
!  mx = 1  ! default dimension of provided field
!  nx = 1  !
!  dmiss = dmiss_default
!  allocate(xe(2))
!  allocate(ye(2))
!  allocate(kcse(2))! default dimensionering kale elementset
!  xe = dmiss
!  ye = dmiss
!  kcse = 1
!
!  if (filetype /= multiple_uni .and. filetype /= ncgrid) then
!     call oldfil(minp,filename)
!  endif
!  success = .true.
!
!  select case (filetype)
!  
!  case ( multiple_uni ) 
!
!     L    = len_trim(qid)
!     qidc = qid(14:L)
!     kk   = 0
!
!     num0 = elementsets(ielsetq)%mnx
!
!     deallocate( xe,ye,kcse)
!     num02 = 2*num0  ! hierdoor wordt de kcs 2 maal groter en kan je later bijhouden of stationnetjes aan of uitgaan
!     allocate(xe(num02))
!     allocate(ye(num02))
!     allocate(kcse(num02)) ! dimensionering op totaal aantal stationnetjes
!
!     kcse(1:num0)       = 0                                ! The child provider numbers will later appear in the first half of the new kcse array.
!     kcse(num0+1:num02) = elementsets(ielsetq)%kcs(1:num0) ! The original mask 0/1's for the stations are in the second half of the new kcse array.
!     xe = dmiss
!     ye = dmiss
!
!     k = 0 ! Start search for providers that deliver the child qidc.
!     do kk=1,num0
!        if (elementsets(ielsetq)%kcs(kk) == 0) then
!           ! Child provider on this station is not requested, so has never been created even, don't search for it.
!           kcse(kk) = 0
!           cycle
!        end if
!
!        ! We ARE looking for a provider on station #i:
!        do ! Look until found (start searching where we left off last time at #k.)
!           k = k + 1
!           if (k > size(dataproviders)) then
!             ! Error: we ran out of providers, while we were still searching.
!             kcse(kk) = 0
!             exit
!           end if
!
!           if (trim(qidc) == trim(dataproviders(k)%qid) ) then 
!              ! Found.
!              kcse(kk) = k
!              exit
!           endif
!        end do ! providerset loop
!     enddo ! child station loop
!     mx = size(elementsets(ielsetq)%kcs ) ! de grootte van het providersfield gehaald uit de elementset v/d quantity
!     elementsets(ielsetq)%kcs(1:num0) = kcse(1:num0)
!      
!  case ( uniform )
!
!  case ( unimagdir)
!
!  case ( svwp   )
!
!  case ( arcinfo )
!
!     success = readarcinfoheader(minp,mx,nx,x0,y0,dxa,dya,dmiss)
!     xe(1) = x0
!     ye(1) = y0
!     xe(2) = dxa
!     ye(2) = dya
!     
!  case ( curvi )
!  
!     mx = size(elementsets(ielsetq)%kcs ) ! de grootte van het providersfield gehaald uit de elementset v/d quantity
!  
!  case ( ncgrid )
!  
!     ierr = nf90_open(filename, NF90_NOWRITE, minp)
!     if (ierr /= nf90_noerr) then
!        errormessage = 'Could not open file ''' // filename // ''' for reading.'
!        success = .false.
!        return
!     end if
!     success = read_nc_header(minp, mx, nx, x0, y0, dxa, dya) !TODO: AvD:, refdatetime, tunit)
!     xe(1) = x0
!     ye(1) = y0
!     xe(2) = dxa
!     ye(2) = dya   
!  
!  case ( triangulationmagdir )
!
!     ja = 1
!     num0 = 0
!     allocate(xe0(mnx0))
!     allocate(ye0(mnx0))
!     allocate(kcse0(mnx0))
!     xe0 = dmiss
!     ye0 = dmiss
!     kcse0 = 0
!     do while (ja .eq. 1)
!        success = connectsinglestationfile(minp,x0,y0,filename0,filetype0,method0,operand0, minp0,ja)
!        if (ja .eq. 1) then
!           xe(1)   = x0
!           ye(1) = y0
!           num0 = num0 + 1
!           method0 = justupdate  ! this provider just updates
!           success = adddataprovider(elementsets, dataproviders, qid, filename0, filetype0, minp0, &
!                                     mx, nx, kx, dmiss, xe, ye, kcse, method0, operand0, nump, ielsetq0 )
!           kcse0 (num0) = nump
!           xe0(num0) = x0
!           ye0(num0) = y0
!        endif
!     enddo
!
!     if (num0 .lt. 3) then
!        success = .false.
!        errormessage = 'minimum nr. of required stations for triangulation = 3'
!        return
!     endif
!
!     success = .true.
!     deallocate( xe,ye,kcse)
!     num02 = 2*num0  ! hierdoor wordt de kcs 2 maal groter en kan je later bijhouden of stationnetjes aan of uitgaan
!     allocate(xe(num02))
!     allocate(ye(num02))
!     allocate(kcse(num02)) ! dimensionering op totaal aantal stationnetjes
!     kcse(1:num02) = kcse0(1:num02)
!     xe(1:num02) = xe0(1:num02)
!     ye(1:num02) = ye0(1:num02)
!     deallocate( xe0, ye0, kcse0 )
!
!     mx = size(elementsets(ielsetq)%kcs ) ! de grootte van het providersfield gehaald uit de elementset v/d quantity
!
!  case ( poly_tim )
!
!     method0   = justupdate
!     operand0  = 'O'
!
!     allocate(xe0(mnx0))
!     allocate(ye0(mnx0))
!     allocate(kcse0(mnx0))
!     xe0 = dmiss
!     ye0 = dmiss
!     kcse0 = 0
!
!     call read1polylin(minp,xe0,ye0,num0)
!     if (num0 .lt. 2) then
!        success = .false.
!        errormessage = 'minimum nr. of required support points for polyline boundary = 2'
!        return
!     endif
!
!!     L = index(filename,'.') - 1
!
!! SPvdP: search for last dot (filename may contain ../ etc.)
!     L = index(filename,'.', back=.true.) - 1
!
!     ielsetq0 = ielsetq
!     num00 = 0
!     do k = 1,num0
!        filetype0 = -999                      ! unknown type
!        write(tex,'(i4.4)') k
!
!
!        filename0 = filename(1:L)//'_'//tex//'.tim'
!        inquire (file = trim(filename0), exist = jawel)
!        if (jawel) then
!           filetype0 = uniform            ! uniform=single time series vectormax = ..
!           call oldfil (minp0, filename0) ! hier file aansluiten en openlatenstaan voor lezen tijdseries
!           mx0 = 1
!           nx0 = 1
!        endif
!
!        if (filetype0 == -999) then
!           filename0 = filename(1:L)//'_'//tex//'.cmp'
!           inquire (file = trim(filename0), exist = jawel)
!           if (jawel) then
!              filetype0 = fourier            ! fourier components, watch this....
!              call oldfil (minp0, filename0) ! file aansluiten
!              success   = readfourierdims(minp0, mx0, nx0)  ! lezen en rewind voor later lezen componenten
!!           else
!!              errormessage = 'In adddataprovider: file '//trim(filename0)//' was not found'
!!              success = .false.
!!              return 
!           endif
!        endif
!        
!        if ((filetype0 == -999) .and. (k == 1)) then
!           filename0 = filename(1:L)//'.qh'
!           inquire (file = trim(filename0), exist = jawel)
!           if (jawel) then
!              filetype0 = qhtable             ! qhbnd-file, watch this....
!              call oldfil (minp0, filename0) ! file aansluiten
!              !success = .true.               ! inlezen gebeurt elders
!              success   = readtabledims(minp0, mx0, nx0)  ! lezen en rewind voor later lezen componenten
!!           else
!!              errormessage = 'In adddataprovider: file '//trim(filename0)//' was not found'
!!              success = .false.
!!              return 
!           endif
!        endif        
!
!        ! if (filetype0 .eq. qhtable)
!        
!        ! else
!        if (filetype0 .ne. -999) then
!            ! qh table does not use children+parent provider: no provider for pli,
!            ! directly an single provider for the qh table, so reuse original method and operand from .ext file.
!            if (filetype0 .eq. qhtable) then 
!               method0 = method
!               operand0 = operand
!               ! ielsetq0??
!               if (trim(qid) == trim('waterlevelbnd')) then 
!                  errormessage = 'In adddataprovider: file '//trim(filename)//' expected QUANTITY=qhbnd but received QUANTITY='
!                  success = .false.
!                  return                   
!               end if
!               if (operand /= 'O') then 
!                  errormessage = 'In adddataprovider: file '//trim(filename)//' only OPERAND=O is supported'
!                  success = .false.
!                  return                   
!               end if
!           end if
!
!           num00   = num00 + 1
!           xe(1)   = xe0(k)
!           ye(1) = ye0(k)
!           success = adddataprovider(elementsets, dataproviders, qid, filename0, filetype0, minp0, &
!                                     mx0, nx0, kx, dmiss, xe, ye, kcse, method0, operand0, nump, ielsetq0 )
!
!           if (filetype0 .eq. qhtable) then 
!               exit                    ! only 1 qhtable read per poly_tim
!           end if
!           if (success) then
!                kcse0 (k) = nump       ! provider nump is attached to this support point
!           else
!                kcse0 (k) = 0
!           end if
!        else
!           kcse0 (k) = 0          ! this polyline support point has no provider attached
!        endif
!
!     enddo
!
!     if (num00 == 0) then
!         errormessage = 'In addprovider: file '//trim(filename)//' has 0 point files (.cmp/.tim/.qh) associated.'
!         success = .false.
!     else
!         success = .true.
!     end if
!     deallocate( xe,ye,kcse)
!     num02 = 2*num0  ! hierdoor wordt de kcs 2 maal groter en kan je later bijhouden of stationnetjes aan of uitgaan
!     allocate(xe(num02))
!     allocate(ye(num02))
!     allocate(kcse(num02)) ! dimensionering op totaal aantal stationnetjes
!     kcse(1:num02) = kcse0(1:num02)
!     xe(1:num02) = xe0(1:num02)
!     ye(1:num02) = ye0(1:num02)
!     deallocate( xe0, ye0, kcse0 )
!
!     mx = size(elementsets(ielsetq)%kcs ) ! de grootte van het providersfield gehaald uit de elementset v/d quantity
!
!  case ( spiderweb )
!
!     success = reaspwheader(minp,mx,nx,dxa,dya,mncoor,dmiss)
!     xe(1) = 0d0
!     ye(1) = 0d0
!     xe(2) = dxa
!     ye(2) = dya
!!    call meteosetmncoord(mncoor) ! zet iets in private module meteo, moet hier eigenlijk uit
!
!  case ( fourier )
!
!     success = readfourierdims(minp, mx, nx)
!
!  case ( qhtable )
!
!     success = readtabledims(minp, mx, nx)     
!     
!  end select
!
!  if (.not. success) return
!
!  !if (filetype0 /= qhtable) then               !! WvB: commented, since this line triggered a breakpoint (don't know the background of the statement)
!      success = adddataprovider(elementsets, dataproviders, qid, filename, filetype, minp,      &
!                                mx, nx, kx, dmiss, xe, ye, kcse, method, operand, nump, ielsetq )
!
!  ! endif
!
!  deallocate (xe, ye, kcse)
!
!  subdoms(idom)%dataproviders => dataproviders
!  subdoms(idom)%elementsets   => elementsets
!  subdoms(idom)%quantities    => quantities
!
!end function addprovider
!
!function connectsinglestationfile(minp,x0,y0,filename0,filetype0,method0,operand0,minp0,ja) result(success)
!  implicit none
!
!  ! globals
!  logical :: success
!
!  character(*)                 :: filename0    ! single station filename
!  double precision                     :: x0, y0       ! single station coordinates
!  integer                      :: filetype0    ! type of file
!  integer                      :: method0      ! type of file
!  integer                      :: minp0        ! lun of single station file
!  integer                      :: minp         ! lun of file that lists single stations
!  integer                      :: ja           ! ja of nee, 1 of 0
!  character (len=1)            :: operand0     ! + or =
!  ! locals
!  integer                      :: l1           ! indexpos
!  character (len=maxnamelen)   :: rec, keywrd, qid0
!  double precision             :: transformcoef0(0)
!
!  success = .false.
!
!  call readprovider(minp,qid0,filename0,filetype0, method0, operand0, transformcoef0, ja)
!  if (ja == 1) then
!      call oldfil(minp0,filename0)
!      ! backspace(minp) ! tja....
!      keywrd = 'XCOORDIN='
!      call zoekja(minp, rec, keywrd, ja)
!      l1 = index(rec, '=') + 1
!      read(rec(l1:),*) x0
!
!      keywrd = 'YCOORDIN='
!      call zoekja(minp, rec, keywrd, ja)
!      l1 = index(rec, '=') + 1
!      read(rec(l1:),*) y0
!      success = .true.
!  endif
!
!end function connectsinglestationfile
!
!
!function adddataprovider(elementsets, dataproviders, qid, filename, filetype, minp, &
!                         my, ny, ky, dmiss, x, y, kcs, method, operand, nump, ielsetq ) result(success)
!  implicit none
!  ! globals
!  logical :: success
!
!  type(telementset),   pointer :: elementsets(:)   ! bevat roosters zowel van vraagkant als aanbodkant
!  type(tdataprovider), pointer :: dataproviders(:) ! bevat het aanbod
!
!  character(len=*)             :: qid          ! id of this quantity
!  character(len=*)             :: filename     ! file containing data
!  character(1)                 :: operand      ! + or =
!  integer                      :: filetype     ! type of file
!  integer                      :: minp         ! unit nr
!  integer                      :: my           ! field dim 1
!  integer                      :: ny           ! field dim 2
!  integer                      :: ky           ! field dim 3
!  double precision             :: dmiss        ! misssing value
!  double precision             :: x(:)         ! elset, als size = 2, dan x0 en dx in x(1) en x(2)
!  double precision             :: y(:)         ! elset, als size = 2, dan y0 en dy in y(1) en y(2)
!  integer                      :: kcs(:)       ! elset, value = 0 : invalid point   value /= 0 : valid point
!  integer                      :: method       ! time/space interpolation method
!  integer     , intent(out)    :: nump         ! index number of this provider
!  integer     , intent(in)     :: ielsetq      ! pointer to elementset on which this provider should give a quantity
!                                               ! only relevant for method >= 2
!
!
!
!  ! locals
!  type(tdataprovider), pointer :: hdataproviders(:)  ! h for help
!  type(tfield),        pointer :: hfield
!  double precision             :: time           ! starttijd
!  integer                      :: ielset       ! pointer naar elementset bijhorend bij datagrid
!  integer                      :: mx           ! size of meteo fields  m
!  integer                      :: nx           !                       n
!  integer                      :: kx           !                       k
!
!  integer                      :: kk, i, k, mnx
!
!  success = .true.
!
!
!  kk = size(dataproviders)
!  if (kk .ge. 1) then
!     allocate(hdataproviders(kk))
!
!     do k = 1,kk
!        hdataproviders(k) = dataproviders(k)
!        allocate ( hdataproviders(k)%field(0:1) )
!        mx    = dataproviders(k)%mx
!        nx    = dataproviders(k)%nx
!        kx    = dataproviders(k)%kx
!        do i  = 0,1
!           time   =   dataproviders(k)%field(i)%time
!           ielset =   dataproviders(k)%field(i)%ielset
!           hfield => hdataproviders(k)%field(i)
!           success = addfield( hfield, mx, nx, kx, time, dmiss, ielset )
!           hdataproviders(k)%field(i) = hfield
!           hdataproviders(k)%field(i)%arr3d  = dataproviders(k)%field(i)%arr3d
!        enddo
!        if (associated(dataproviders(k)%indxn)) then
!            hdataproviders(k)%indxn => dataproviders(k)%indxn
!            hdataproviders(k)%wfn   => dataproviders(k)%wfn
!            hdataproviders(k)%refresh= dataproviders(k)%refresh
!        end if
!     enddo
!     deallocate (dataproviders)
!  endif
!
!  allocate   (dataproviders(kk+1))
!
!  if (kk .ge. 1) then
!     do k = 1,kk
!        dataproviders(k) = hdataproviders(k)
!! GD: memory leak
!!        if ( associated(dataproviders(k)%field) ) deallocate(dataproviders(k)%field)
!        allocate( dataproviders(k)%field(0:1) )
!        mx    = hdataproviders(k)%mx
!        nx    = hdataproviders(k)%nx
!        kx    = hdataproviders(k)%kx
!        do i  = 0, 1
!           time   =  hdataproviders(k)%field(i)%time
!           ielset =  hdataproviders(k)%field(i)%ielset
!           hfield => hdataproviders(k)%field(i)
!           success = addfield( hfield, mx, nx, kx, time, dmiss, ielset )
!           dataproviders(k)%field(i) = hfield
!           dataproviders(k)%field(i)%arr3d  = hdataproviders(k)%field(i)%arr3d
!        enddo
!     enddo
!     deallocate (hdataproviders)
!  endif
!
!  k = kk + 1
!  dataproviders(k)%qid      = qid
!  dataproviders(k)%filename = filename
!  dataproviders(k)%filetype = filetype
!  dataproviders(k)%minp     = minp
!  dataproviders(k)%dmiss    = dmiss
!  dataproviders(k)%method   = method
!  dataproviders(k)%operand  = operand
!  dataproviders(k)%ielsetq  = ielsetq
!  dataproviders(k)%it0      = 0
!  dataproviders(k)%it1      = 1
!  dataproviders(k)%mx       = my
!  dataproviders(k)%nx       = ny
!  dataproviders(k)%kx       = ky
!  nullify(dataproviders(k)%indxn)
!  nullify(dataproviders(k)%wfn)
!
!  allocate (dataproviders(k)%field(0:1))
!
!  do i = 0, 1                                                 ! en voeg de velden toe
!     success = addelementset(elementsets, x, y, kcs, ielset)  ! each field is defined on its own elementset
!     hfield  => dataproviders(k)%field(i)
!     time    = t01ini
!     success = addfield( hfield, my, ny, ky, time, dmiss, ielset )
!  enddo
!
!  if (method .eq. weightfactors) then
!     mnx     = size( elementsets(ielsetq)%kcs )
!     success = addindexweights(dataproviders(k)%indxn, dataproviders(k)%wfn, mnx, filetype)
!     dataproviders(k)%refresh = 1
!  endif
!  
!  nump = k
!
!end function adddataprovider
!
!function addindexweights(indxn, wfn, mnx, filetype)  result(success)
!  implicit none
!  ! globals
!  logical                   :: success
!  integer, pointer          :: indxn(:,:)
!  double precision, pointer :: wfn(:,:)
!  integer                   :: mnx, filetype
!
!  ! locals
!  integer                   :: ierr
!
!  success = .false.
!  if (associated(indxn)) then
!    deallocate(indxn)
!    deallocate(wfn)
!  end if
!  if (filetype == curvi) then  ! curvi goes bilin, 1,2 = m,n
!     allocate (indxn(2,mnx),stat=ierr)
!     allocate (wfn(4,mnx),stat=ierr)
!  else                     ! rest goes triang
!     allocate (indxn(3,mnx),stat=ierr)
!     allocate (wfn(3,mnx),stat=ierr)
!  endif
!  indxn = 0
!  wfn = 0d0
!  success = .true.
!
!end function addindexweights
!
!function readfouriercompstim(minp,d0,d1,mx,nx,kx,tim,tread) result(success)
!use m_itdate
!   use string_module, only: find_first_word
!   !
!   ! Read fourier components initially, next generate
!   !
!   implicit none
!   !
!   integer,  intent (inout)                       :: minp   !< IO unit number
!   integer,  intent (in )                         :: mx     !< size of meteo fields  m
!   integer,  intent (in )                         :: nx     !< size of meteo fields  n
!   integer,  intent (in )                         :: kx     !< size of meteo fields  k, at the moment kx == 1 allways
!   double precision, intent (in )                 :: tim    !< time we want to update to, must be between t0 and t1 after reading
!   double precision, intent (inout)               :: tread  !< time of next read record
!   double precision, dimension(:,:), intent (out) :: d0, d1 !< arr2d !!! change to 1D arrays
!   logical                                        :: success
!
!   ! locals
!   character(len=maxnamelen)                  :: rec !< next read string (header or data)
!   character(len=8)                           :: compname !< component name of read data string (may not be present)
!   integer                                    :: k, L, ierrs, i1, i2
!   integer                                    :: n !< the Nth data line (not header nor blank lines)
!   double precision                           :: fff, ampl, phas, omegt, phast
!   double precision                           :: omeg !< the frequency[rad/min]
!
!   success = .false.
!   ierrs   = 0 
!
!   if (tread == t01ini) then
!
!      n = 0
!10    read(minp,'(a)',end = 999) rec
!      if  (rec(1:1) == '*' .or. len_trim(rec) == 0 ) goto 10 !< Skip header lines.
!
!      n = n + 1 ! new data line found
!
!      call find_first_word(rec,i1,i2) ! start and stop index of the first word (will be the component name)
!      if ( i1 .ne. 0 )  then ! probably m2, s2 etc instead of period in minutes
!
!         read(rec(i1:i2) ,'(a)' ,err = 889)   compname ! if not present, then it is the period in minutes
!         read(rec(i2+1:),   *   ,err = 889)   ( d0(2+(k-1)*kx , n ) , d0(3+(k-1)*kx , n ) , k = 1,kx)
!
!         if (index(compname,'A0') .ne. 0) then
!            d0(1,n) = 0
!            d0(3,:) = 0 ! zero periodicity? And no phase. So only magnitude...
!         else
!            call asc(d0(1,n), d0(2,n), d0(3,n), compname, itdate, ierrs) ! itdate is reference date as yyyymmdd integer
!         endif
!
!      else ! component name is missing => period in minutes
!
!         read(rec ,*    ,err = 888) d0(1,n) , ( d0(2+(k-1)*kx,n), d0(3+(k-1)*kx,n) , k = 1,kx)
!         if (d0(1,n) .ne. 0) then
!            d0(1,n) = 2d0*pi/d0(1,n)                     ! period specified in minutes => d0(1,*) (rad/min)
!         endif
!
!         do k = 1,kx
!            d0(3+(k-1)*kx,n) = d0(3+(k-1)*kx,n)*d2r ! pi/180d0 ! on input, fases in degrees, now converted to radians
!         enddo
!
!      endif
!
!
!      goto 10
!   endif
!
!999 continue
!   if (tread == t01ini) then
!      call doclose(minp)
!   endif
!   tread = tim           ! trick into being up to date, this will always be update
!
!   do k = 1, kx          ! for all kx quantities
!      fff = 0d0
!      do n = 1,nx        ! for all nx components
!         omeg    = d0(1,n)
!         ampl    = d0(2+(k-1)*kx,n)
!         phas    = d0(3+(k-1)*kx,n)
!
!         omegt   = d0(1,n)*tim
!         phast   = omegt - phas
!
!         fff     = fff + ampl*cos(phast)
!      enddo
!      d1(1,k) = fff
!   enddo
!   success = .true.
!
!   return
!
!
!888 call readerror('reading omeg, ampl, phas, but getting ', rec, minp)
!    return
!
!889 call readerror('reading component name, ampl, phas, but getting ', rec, minp)
!    return
!
!end function readfouriercompstim
!
!
!function readtableentries(minp,d0,d1,mx,nx,kx,tim,tread) result(success)
!use m_itdate
!   use string_module, only: find_first_word
!   !
!   ! Read two column table from file
!   !
!   implicit none
!   !
!   integer,  intent (inout)                   :: minp
!   integer,  intent (in )                     :: mx
!   integer,  intent (in )                     :: nx
!   integer,  intent (in )                     :: kx
!   double precision, intent (in )                     :: tim
!   double precision, intent (inout)                   :: tread
!   double precision, dimension(:,:), intent (out)     :: d0
!   double precision, dimension(:,:), intent (out)     :: d1
!   logical                                    :: success
!
!   ! locals
!   character(len=maxnamelen)                  :: rec
!   character(len=8)                           :: compname
!   integer                                    :: k, n, L, ierrs, i1, i2
!   integer                                    :: n1 ! first row containing data  (not a comment)
!   integer                                    :: n2 ! temporary variable for previous row
!
!   double precision                           :: dzdx
!   double precision                           :: z0
!   
!   success = .false.
!   n1 = 2147483647                              ! large value (replace by system settings?)
!   if (tread == t01ini) then
!
!      n = 0
!10    read(minp,'(a)',end = 999) rec
!      if  (rec(1:1) == '*' .or. len_trim(rec) == 0 ) goto 10
!
!      n = n + 1
!
!      call find_first_word(rec,i1,i2)
!      if ( i1 .ne. 0 )  then ! probably m2, s2 etc instead of period in minutes
!
!         ! give error - text not supported  
!         goto 888
!         
!      else
!         n1 = min(n,n1) 
!         read(rec ,*    ,err = 888) d1(1,n) , d1(2,n) ! discharge, waterlevel
!         n2   = max(n1,n-1)
!         if (n .gt. n2) then 
!            if (d1(1,n) .le. d1(1,n2)) then
!               goto 887
!            end if
!            dzdx = (d1(2,n)-d1(2,n2))/(d1(1,n)-d1(1,n2))  ! slope 
!            z0   = d1(2,n2) - dzdx*d1(1,n2)               ! x0 crossing
!            d0(1,n2) = z0
!            d0(2,n2) = dzdx
!         end if 
!      endif
!
!
!      goto 10
!   endif
!
!999 continue
!   if (tread == t01ini) then
!      call doclose(minp)
!   endif
!   tread = tim          ! trick into being up to date, this will always be update
!
!   ! compute discharge flowing through the open boundary
!   
!   !do k = 1, kx          ! for all kx quantities
!   !   fff = 0d0
!   !   do n = 1,nx        ! for all nx components
!   !      omeg    = d0(1,n)
!   !      ampl    = d0(2+(k-1)*kx,n)
!   !      phas    = d0(3+(k-1)*kx,n)
!
!   !      omegt   = d0(1,n)*tim
!   !      phast   = omegt - phas
!
!   !      fff     = fff + ampl*cos(phast)
!   !   enddo
!   !   d1(1,k) = 3.0
!   !enddo
!   success = .true.
!
!   return
!
!887 call readerror('first column should be an increasing sequence', rec, minp)
!    return
!
!888 call readerror('expecting two column table, but received text ', rec, minp)
!    return
!
!889 call readerror('expecting two column table, but getting ', rec, minp)
!    return
!
!end function readtableentries
!
!subroutine dlauny(x         ,y         ,ns        )
!
!
!   ! implicit none
!!
!! Local parameters
!!
!    integer, parameter :: nh = 1
!    ! COMMON variables
!    !
!!
!! Global variables
!!
!    integer         :: ns
!    double precision, dimension(ns + 4) :: x
!    double precision, dimension(ns + 4) :: y
!!
!!
!! Local variables
!!
!    integer                        :: i
!    integer                        :: i1
!    integer                        :: i2
!    integer                        :: i3
!    integer                        :: ie
!    integer                        :: ierr
!    integer                        :: in
!    integer                        :: inew
!    integer                        :: inewe
!    integer                        :: interval
!    integer                        :: j
!    integer                        :: je
!    integer                        :: k
!    integer                        :: l
!    integer                        :: match
!    integer                        :: maxtri
!    integer                        :: nart
!    integer                        :: ndel
!    integer                        :: newel
!    integer                        :: nn
!    integer                        :: nsm
!    double precision                   :: cx
!    double precision                 :: cy
!    double precision               :: den
!    double precision               :: dx
!    double precision               :: dxy
!    double precision               :: dy
!    double precision               :: r2
!    double precision               :: rn2
!    double precision               :: x2
!    double precision               :: x3
!    double precision               :: xl
!    double precision               :: xmax
!    double precision               :: xmin
!    double precision               :: xn1
!    double precision               :: xr
!    double precision               :: y2
!    double precision               :: y3
!    double precision               :: yl
!    double precision               :: ymax
!    double precision               :: ymin
!    double precision               :: yr
!    double precision               :: z
!    double precision               :: zero
!!
!!
!!! executable statements -------------------------------------------------------
!!
!    !
!    !
!    !     ******************************************************************
!    !     *                                                                *
!    !     * PERFORMS A DELAUNAY TRIANGULARISATION OF A REGION GIVEN A SET  *
!    !     * OF MESH POINTS.                                                *
!    !     *   X,Y    :- 1D ARRAYS HOLDING COORDINATES OF MESH POINTS.      *
!    !     *             DIMENS2ONED AT LEAST NS+4.                      *
!    !     *   NS  :- NUMBER OF MESH POINTS.                             *
!    !     *   INDX :- INTEGER ARRAY, DIMENS2ONED 3*nsmax,  WHICH ON EXIT*
!    !     *             CONTAINS THE INDEX OF GLOBAL NS ASSOCIATED WITH *
!    !     *             EACH ELEMENT.                                      *
!    !     *   X,YCENT :- ARRAY GIVING CO-ORDINATES OF ELEMENT CIRCUMCENTRES
!    !
!    !     *   NUMTRI :- ON EXIT CONTAINS THE NUMBER OF ELEMENTS IN THE     *
!    !     *             TRIANGULARISATION.                                 *
!    !     *                                                                *
!    !     *   N.B.  A NON-DELAUNAY TRIANGULATION MAY OCCUR IF ROUNDING     *
!    !     *         ERRORS CAUSE A WRONG DECISION IN THE TEST FOR NEW      *
!    !     *         POINT BEING INS2DE CIRCUMCIRCLE                        *
!    !     *                                                                *
!    !     *                                   P.K.SWEBY                    *
!    !     *                                   ADAPTED FOR AMDAHL BY        *
!    !     *                                   J.J.BARLEY (17/5/88)         *
!    !     ******************************************************************
!    !
!
!    if (ns<3) then
!       return
!    endif
!    maxtri = 0
!    numtri = 0
!    nsm = 3*ns + 20
!    !
!    if (size(xcent) .lt. nsm) then
!        if (allocated (xcent) ) deallocate (xcent, ycent, indx)
!        allocate ( xcent(nsm), ycent(nsm), indx(3, nsm), stat = ierr)
!    endif
!   !
!    zero = 0.01d0
!
!    !
!    !     CALCULATE ARTIFICIAL NS NS+I I=1,2,3,4 AND CONSTRUCT FIRST
!    !     TWO (ARTIFICIAL) ELEMENTS.
!    !
!    xmin = x(1)
!    xmax = x(1)
!    ymin = y(1)
!    ymax = y(1)
!    do i = 2, ns
!       xmin = min(xmin, x(i))
!       xmax = max(xmax, x(i))
!       ymin = min(ymin, y(i))
!       ymax = max(ymax, y(i))
!    enddo
!    !
!    dx = xmax - xmin
!    dy = ymax - ymin
!    dxy = 0.1d0*max(dx, dy)
!    zero = 10*dxy*1.0D-9
!    xl = xmin - 4d0*dx - dxy
!    xr = xmax + 4d0*dx + dxy
!    yl = ymin - 4d0*dy - dxy
!    yr = ymax + 4d0*dy + dxy
!    x(ns + 1) = xl
!    y(ns + 1) = yl
!    x(ns + 2) = xl
!    y(ns + 2) = yr
!    x(ns + 3) = xr
!    y(ns + 3) = yr
!    x(ns + 4) = xr
!    y(ns + 4) = yl
!    indx(1, 1) = ns + 1
!    indx(2, 1) = ns + 2
!    indx(3, 1) = ns + 3
!    indx(1, 2) = ns + 3
!    indx(2, 2) = ns + 4
!    indx(3, 2) = ns + 1
!    numtri = 2
!    !
!    do ie = 1, 2
!       i1 = indx(1, ie)
!       i2 = indx(2, ie)
!       i3 = indx(3, ie)
!       x2 = x(i2) - x(i1)
!       x3 = x(i3) - x(i1)
!       y2 = y(i2) - y(i1)
!       y3 = y(i3) - y(i1)
!       den = y2*x3 - y3*x2
!       if (den/=0) then
!          z = (x2*(x2 - x3) + y2*(y2 - y3))/den
!       else
!          ! call qnerror('COINCIDING POINTS'  ,' '       ,' '       )
!       endif
!       xcent(ie) = 0.5D0*(x3 - z*y3)
!       ycent(ie) = 0.5D0*(y3 + z*x3)
!    enddo
!    !
!    !     CALL READYY('CREATING TRIANGLE NETWORK',0.0)
!    interval = max(1, ns/100)
!    do in = 1, ns
!
!       !
!       !     ADD ONE MESH POINT AT A TIME AND REMESH LOCALLY IF NECESSARY
!       !
!       ndel = 0
!       newel = 0
!       do ie = 1, numtri
!          !
!          !     IS POINT IN INS2DED CIRCUMCIRCLE OF ELEMENT IE ?
!          !
!          i1 = indx(1, ie)
!          i2 = indx(2, ie)
!          i3 = indx(3, ie)
!          cx = xcent(ie)
!          cy = ycent(ie)
!          r2 = cx**2 + cy**2
!          xn1 = x(in) - x(i1)
!          rn2 = (xn1 - cx)**2 + (y(in) - y(i1) - cy)**2
!          !
!          if (rn2>r2) then
!             cycle
!          endif
!          !
!          !     YES IT IS INS2DE,CREATE NEW ELEMENTS AND MARK OLD FOR DELETION.
!          !
!          !
!          do j = 1, 3
!             do k = 1, 3
!                indx(k, numtri + newel + j) = indx(k, ie)
!             enddo
!             maxtri = max(maxtri, numtri + newel + 3)
!             if (maxtri>nsm) then
!                write(*,*)'maxtri>nsm'
!                ! call qnerror('MAXIMUM NUMBER OF TRIANGLES EXCEEDED'     ,'REDUCE NUMBER OF SAMPLES IN'   ,'TRIANGULATION'      )
!                return
!             endif
!             indx(j, numtri + newel + j) = in
!          enddo
!          do inew = 1, 3
!             inewe = numtri + newel + inew
!             maxtri = max(maxtri, inewe)
!             if (maxtri>nsm) then
!                ! call qnerror('MAXIMUM NUMBER OF TRIANGLES EXCEEDED'     ,'REDUCE NUMBER OF SAMPLES IN'   ,'TRIANGULATION'      )
!                write(*,*)'maxtri>nsm'
!                return
!             endif
!             i1 = indx(1, inewe)
!             i2 = indx(2, inewe)
!             i3 = indx(3, inewe)
!             x2 = x(i2) - x(i1)
!             x3 = x(i3) - x(i1)
!             y2 = y(i2) - y(i1)
!             y3 = y(i3) - y(i1)
!             if (abs(y2*x3 - y3*x2)>zero) then
!                z = (x2*(x2 - x3) + y2*(y2 - y3))/(y2*x3 - y3*x2)
!                cx = 0.5D0*(x3 - z*y3)
!                cy = 0.5D0*(y3 + z*x3)
!             else
!                cx = 0.5D0*(x3 - x2)
!                cy = 0.5D0*(y3 - y2)
!             endif
!             xcent(inewe) = cx
!             ycent(inewe) = cy
!          enddo
!          newel = newel + 3
!          indx(1, ie) = 0
!          ndel = ndel + 1
!       !
!       enddo
!       !
!       !     IF IN WAS INS2DE CIRCUMCIRCLE OF MORE THAN 1 ELEMENT THEN WILL
!       !     HAVE CREATED 2 IDENTICAL NEW ELEMENTS: DELETE THEM BOTH.
!       !
!       if (ndel>1) then
!          do ie = numtri + 1, numtri + newel - 1
!             do je = ie + 1, numtri + newel
!                match = 0
!                do k = 1, 3
!                   do l = 1, 3
!                      if (indx(k, ie)==indx(l, je)) match = match + 1
!                   enddo
!                enddo
!                if (match==3) then
!                   indx(1, ie) = 0
!                   indx(1, je) = 0
!                   ndel = ndel + 2
!                endif
!             enddo
!          enddo
!       endif
!       !
!       !     DELETE ANY ELEMENTS
!       !
!       !
!       nn = numtri + newel
!       ie = 1
!   70  continue
!       if (indx(1, ie)==0) then
!          do j = ie, nn - 1
!             xcent(j) = xcent(j + 1)
!             ycent(j) = ycent(j + 1)
!             do k = 1, 3
!                indx(k, j) = indx(k, j + 1)
!             enddo
!          enddo
!          nn = nn - 1
!          ie = ie - 1
!       endif
!       ie = ie + 1
!       if (ie<=nn) goto 70
!       numtri = nn
!    !
!    enddo
!    !
!    !
!    !     FINALLY REMOVE ELEMENTS CONTAINING ARTIFICIAL NS
!    !
!    !
!    !
!    ie = 1
!  100 continue
!    nart = 0
!    do l = 1, 3
!       if (indx(l, ie)>ns) nart = nart + 1
!    enddo
!    if (nart>0) then
!       do j = ie, nn - 1
!          xcent(j) = xcent(j + 1)
!          ycent(j) = ycent(j + 1)
!          do k = 1, 3
!             indx(k, j) = indx(k, j + 1)
!          enddo
!       enddo
!       numtri = numtri - 1
!       ie = ie - 1
!    endif
!    ie = ie + 1
!    if (ie<=numtri) goto 100
!    !
!!    deallocate (xcent, ycent)
!    !
!    !     CALL READYY('CREATING TRIANGLE NETWORK',-1d0)
!!    write (mdia, *) numtri, maxtri
!end subroutine dlauny
!
!function addtimespacerelation(idom, qid, kx, x, y, kcs, filename, filetype, method, operand, xyen)  result(success)
!  implicit none
!  logical                      :: success
!
!  ! this subroutine adds a relation in domainnr=threadnr idom (use 1 of no threads)
!  ! between a requested quantity with id=qid, defined on elementset x,y,kcs
!
!  ! arguments
!  integer,      intent(in)     :: idom       ! threadnr = domainnr
!  character(*), intent(in)     :: qid        ! unique quantity identification
!  integer,      intent(in)     :: kx         ! vectormax
!
!  double precision,     intent(in)     :: x(:)       ! x of elset
!  double precision,     intent(in)     :: y(:)       ! y of elset
!  integer ,     intent(in)     :: kcs(:)     ! kcs of elset
!
!  character(*), intent(in)     :: filename   ! file name for meteo data file
!  integer     , intent(in)     :: filetype   ! spw, arcinfo, uniuvp etc
!  integer     , intent(in)     :: method     ! time/space interpolation method
!  character(1), intent(in)     :: operand    ! file name for meteo data file
!
!  double precision, intent(in), optional :: xyen(:,:)     ! distance tolerance / cellsize of elset
!
!  ! locals
!  integer                      :: mx
!  integer                      :: numq       ! index number of this quantity
!  integer                      :: nump       ! index number of this provider
!  integer                      :: ielsetq    ! number of elset x,y,kcs of quantity
!
!  type(tsubdom),       pointer :: hsubdoms(:)
!
!  if (size(kcs) < 1) return
!
!if (.not. allocated (subdoms) ) then
!     mx = 0
!  else
!     mx = size(subdoms)
!  end if
!
!  if (idom .gt. mx) then                     ! todo: deepcopy maken voor echt gebruik
!     if (mx > 0) then
!        allocate ( hsubdoms(mx) )
!        hsubdoms = subdoms
!        deallocate (subdoms)
!     endif
!     allocate( subdoms(idom) )
!     allocate( subdoms(idom)%quantities(0))
!     allocate( subdoms(idom)%dataproviders(0))
!     allocate( subdoms(idom)%elementsets(0))
!     if (mx > 0) then
!        subdoms(1:mx) = hsubdoms(1:mx)
!        deallocate(hsubdoms)
!     endif
!     pi  = acos(-1.d0)
!     d2r = pi/180.d0
!     r2d = 180.d0/pi
!  endif
!
!  if (present(xyen) ) then
!     success = addquantity( idom, qid, kx, x, y, kcs, numq, ielsetq, xyen )
!  else
!     success = addquantity( idom, qid, kx, x, y, kcs, numq, ielsetq)
!  endif
!
!  success = addprovider( idom, qid, kx, filename, filetype, method, operand, nump, ielsetq )
!
!end function addtimespacerelation
!
!subroutine magdir2uv(u0,u1,a0,a1,uv)
!  implicit none
!  double precision                    :: a1
!  double precision                    :: a0
!  double precision                    :: u0(2)
!  double precision                    :: u1(2)
!  double precision                    :: uv(2)
!
!
!  double precision                    :: wmag
!  double precision                    :: wdir
!  double precision                    :: wdir0
!  double precision                    :: wdir1
!
!  wdir0 = u0(2)
!  wdir1 = u1(2)
!  call regdir(wdir0, wdir1)
!  wmag  = a0*u0(1) + a1*u1(1)
!  wdir  = a0*wdir0 + a1*wdir1      ! WvB, TODO: cyclic interpolation required
!  wdir  = (270e0_fp - wdir)*d2r    ! nautical convention
!  uv(1)  = wmag * cos(wdir)
!  uv(2)  = wmag * sin(wdir)
!
!end subroutine magdir2uv
!
!subroutine distance2(sferic    ,x1        ,y1        ,x2        ,y2        , &
!                   & d12       )
!!!--description-----------------------------------------------------------------
!!
!!    Function: Calculates distance between two points on earth
!! Method used: Circular distance when sferic is true,
!!              Euclidic distance when sferic is false
!!
!!!--pseudo code and references--------------------------------------------------
!!
!! This subroutine is identical to subroutine distance in Delft3D-FLOW, except
!! that this version is independent of GDP
!!
!!!--declarations----------------------------------------------------------------
!    use precision
!    implicit none
!!
!! Global variables
!!
!    logical, intent(in)   :: sferic !  true: spherical, false: cartesian coordinate system
!    double precision, intent(out) :: d12    !!  Calculated distance from 1 to 2
!    double precision, intent(in)  :: x1     !!  X coordinate of point 1 (deg or m)
!    double precision, intent(in)  :: x2     !!  X coordinate of point 2 (deg or m)
!    double precision, intent(in)  :: y1     !!  Y coordinate of point 1 (deg or m)
!    double precision, intent(in)  :: y2     !!  Y coordinate of point 2 (deg or m)
!!
!! Local variables
!!
!    double precision :: ddegrad
!    double precision :: dearthrad
!    double precision :: d128      ! Double precision d12
!    double precision :: phi       ! Angle
!    double precision :: x1rad     ! X1 in radials
!    double precision :: x2rad     ! X2 in radials
!    double precision :: y1rad     ! Y1 in radials
!    double precision :: y2rad     ! Y2 in radials
!!
!!! executable statements -------------------------------------------------------
!!
!    if (x1==x2 .and. y1==y2) then
!       d12 = 0.0_fp
!       return
!    endif
!    dearthrad = 6378137.0_hp
!    ddegrad   = acos( - 1.0_hp)/180.0_hp
!    if (sferic) then
!       x1rad = x1*ddegrad
!       x2rad = x2*ddegrad
!       y1rad = y1*ddegrad
!       y2rad = y2*ddegrad
!       phi = cos(y1rad)*cos(y2rad)*cos(x1rad - x2rad) + sin(y1rad)*sin(y2rad)
!       d128 = dearthrad*acos(phi)
!    else
!       d128 = sqrt((x2 - x1)**2 + (y2 - y1)**2)
!    endif
!    d12 = d128
!end subroutine distance2
!
!            !
!            !call savepol()
!            !call newfil(mpol, 'dualcells.pol')
!            !call wripol(mpol)
!            !call doclose(mpol)
!            !call delpol()
!
!   !if (filetype .ne. ncflow) then
!   !   if (minp0 > 0) then
!   !      call doclose(minp0)
!   !   end if
!   !end if
!
!subroutine read1pliz(minp,xs,ys,zs,ns)
!use m_alloc
!
!   implicit none
!
!   double precision, allocatable, dimension(:)  :: xs, ys, zs
!
!   integer                       :: ns
!   integer                       :: minp
!
!   ! locals
!   character (len=maxnamelen)    :: rec
!   integer                       :: k, nr, nx
!
!
!   ns = 0
!
!10 read(minp,'(a)',end = 999) rec
!   if  (rec(1:1) == '*' ) goto 10
!
!   read(minp,'(a)',end = 999) rec
!   read(rec ,*    ,err = 888) nr
!
!   if (nr > size(xs) ) then 
!      nx = 1.2*nr
!      call realloc( xs, nx, 1 )  
!      call realloc( ys, nx, 1 )  
!      call realloc( zs, nx, 1 )  
!   endif
!   
!   do k = 1,nr
!
!      read(minp,'(a)',end = 999) rec
!      read(rec ,*    ,err = 777) xs(k), ys(k), zs(k)
!      ns = k
!
!   enddo
!
!   return
!
!999 call doclose(minp)
!    return
!
!888 call readerror('reading nrows but getting ', rec, minp)
!    return
!
!777 call readerror('reading x, y  but getting ', rec, minp)
!    return
!
!end subroutine read1pliz
!
!
!
!
!
!
!function readfourierdims(minp,mx,nx) result(success)
!   use string_module, only: find_first_word
!   implicit none
!
!   integer                      :: minp
!   integer                      :: mx      ! standard = 3, for omeg, ampl, phas
!   integer                      :: nx      ! nr of fourier components
!   logical                      :: success
!
!
!   ! locals
!   character (len=maxnamelen)   :: rec
!   double precision             :: omeg, ampl, phas
!   integer                      :: L, i1, i2
!   character(len=10)            :: compname
!
!   success = .false.
!   mx = 3
!   nx = 0
!
!10 read(minp,'(a)',end = 999) rec
!   if  (rec(1:1) == '*' .or. len_trim(rec) == 0) goto 10
!
!   call find_first_word(rec,i1,i2)
!   if ( i1 .ne. 0 )  then ! probably m2, s2 etc instead of period in minutes
!
!      read(rec(i1:i2) ,'(a)' ,err = 889)   compname
!
!   else
!
!      read(rec ,*    ,err = 888) omeg, ampl, phas   ! todo als hier meer ampl, phas cols staan, increase mx todo
!
!   endif
!   nx = nx + 1
!   goto 10
!
!
!999 rewind (minp)
!    success = .true.
!    return
!
!888 call readerror('reading omeg, ampl, phas, but getting ', rec, minp)
!    return
!
!889 call readerror('reading component name, ampl, phas, but getting ', rec, minp)
!    return
!
!
!end function readfourierdims
!
!function readtabledims(minp,mx,nx) result(success)
!   ! copied from readfourierdims (could be combined to a general reading function? )
!   use string_module, only: find_first_word
!   implicit none
!
!   integer                      :: minp
!   integer                      :: mx      ! standard = 2, for q, h
!   integer                      :: nx      ! nr of fourier components
!   logical                      :: success
!
!
!   ! locals
!   character (len=maxnamelen)   :: rec
!   double precision             :: q, h
!   integer                      :: L, i1, i2
!   character(len=10)            :: compname
!
!   success = .false.
!   mx = 2
!   nx = 0
!
!10 read(minp,'(a)',end = 999) rec     ! ignore comment lines
!   if  (rec(1:1) == '*' .or. len_trim(rec) == 0) goto 10
!
!   call find_first_word(rec,i1,i2)
!   if ( i1 .ne. 0 )  then     
!
!      goto 888
!
!   else
!
!      read(rec ,*    ,err = 888) q, h
!
!   endif
!   nx = nx + 1
!   goto 10
!
!
!999 rewind (minp)
!    success = .true.
!    return
!
!888 call readerror('expecting numeric two column table, but getting ', rec, minp)
!    return
!
!end function readtabledims
!
!function dataInterpolation (var1,var2,weight1,weight2,iscyclic)
!
!    double precision, intent(in) :: var1                !< First input argument for in interpolation functions using a scalar weight value
!    double precision, intent(in) :: var2                !< Second input argument for in interpolation functions using a scalar weight value
!    double precision, intent(in) :: weight1             !< Value for weighing two variables: 'weight1' holds for var1
!    double precision, intent(in) :: weight2             !< Value for weighing two variables: 'weight2' holds for var2
!    integer                      :: iscyclic            !< If the input is of cyclic character, then iscyclic = 1 (for instance: wave direction)
!    double precision             :: dataInterpolation   !< Result value after linear interpolation between var1 and var2 using weightvalue weight
!    double precision             :: minangle,maxangle,delta,weightfac
!    
!    ! Direct linear interpolation of two scalars
!    if (iscyclic.eq.0) then
!       dataInterpolation = var1*weight1 + var2*weight2
!    end if
!    
!    ! Cyclic interpolation of two scalars, based on periodicity of 360 (degrees)
!    if (iscyclic.eq.1) then
!       ! Sort data in monotonically increasing order and rotate over smallest angle
!       minangle      = var1
!       maxangle      = var2
!       weightfac     = weight1
!       if (var2.lt.var1) then
!           minangle     = var2
!           maxangle     = var1
!           weightfac    = 1d0-weightfac
!       end if
!       delta         = maxangle - minangle
!       
!       ! Carry out the interpolation
!       if (delta <= 180d0) then
!           dataInterpolation   = (1d0-weightfac)*delta
!       else
!           dataInterpolation   = (1d0-weightfac)*delta + weightfac*360d0
!       end if
!       
!       ! Rotate backwards over the smallest angle
!       dataInterpolation = dataInterpolation + minangle
!       dataInterpolation = modulo(dataInterpolation,360d0)
!       
!    end if
!
!end function
!
!
!
!
!
!
!subroutine asc(omeg, ampl, phas, inaam, itdate, ierrs)
!!!--description-----------------------------------------------------------------
!!
!!    Function: Determination of FR and V0+U
!!              'stripped' VERSION OF MAIN (ASCON)
!! Method used:
!!
!!!--pseudo code and references--------------------------------------------------
!! NONE
!!!--declarations----------------------------------------------------------------
!    use precision
!    implicit none
!!
!! Global variables
!!
!    double precision, intent(out)   :: omeg   !< period
!    double precision, intent(inout) :: ampl   !< amplitude
!    double precision, intent(inout) :: phas   !< phase
!    integer,          intent(in)    :: itdate !< Gregorian yyyymmdd integer
!    integer,          intent(out)   :: ierrs  !< number of errors
!    character(len=8), intent(in)    :: inaam  !< component name as read from Fourier components file
!    !
!    ! local
!    !
!    integer, parameter    :: kcmp = 1   !< 
!    integer, parameter    :: mxkc = 234 !< 
!    integer, dimension(6) :: jdatum     !< Date and time
!
!    double precision, dimension(kcmp) :: fr  !< Amplitude factors for the referenced components
!    double precision, dimension(kcmp) :: v0u !< Astronomical arguments of the referenced components [rad]
!    double precision, dimension(kcmp) :: w   !< Angular velocity of the referenced components [rad/hr]
!
!    integer                               :: i      !< Help var.
!    integer                               :: ik     !< Help var.
!    integer                               :: il     !< Help var.
!    integer                               :: j      !< Help var.
!    integer                               :: jaar   !< Present year
!    integer,           dimension(16*mxkc) :: jnaam  !< Help var.
!    character(len=8),  dimension(mxkc)    :: knaam  !< Array with the names of all components
!    character(len=80), dimension(mxkc)    :: kombes !< Array with tidal components
!    double precision                      :: t      !< Time in hours referred to January 1, 00:00 of the year 'JAAR'
!    double precision,  dimension(15)      :: v      !< Help var. to calculate V0U()
!    double precision,  dimension(25)      :: f      !< Help var. to calculate FR()
!!
!!! executable statements -------------------------------------------------------
!!
!    !     **************************
!    !     ** LEES C0021KOMPBES IN **
!    !     **************************
!    !
!    !     transform itdate into jdatum (integer)
!    !     example, if datum = 28 07 1991 then
!    !     itdate = 19910728
!    !        jdatum (1) = 1991
!    !        jdatum (2) =    7
!    !        jdatum (3) =   28
!    !        jdatum (4) =    0
!    !        jdatum (5) =    0
!    !        jdatum (6) =    0
!    !
!    !
!    jdatum(3) =  itdate - 100*(itdate/100)
!    jdatum(2) = (itdate - 10000*(itdate/10000))/100
!    jdatum(1) =  itdate/10000
!    jdatum(4) = 0
!    jdatum(5) = 0
!    jdatum(6) = 0
!    call kompbs(kombes    )
!    !
!    ik = -15
!    do i = 1, mxkc
!       ik = ik + 16
!       il = ik + 15
!       read (kombes(i), '(a8,10i3,3(i1,i2))') knaam(i), (jnaam(j), j = ik, il)
!    enddo
!    !
!    !     ***************************************
!    !     ** LUS OVER DE OPGEGEVEN TIJDSTIPPEN **
!    !     ***************************************
!    !
!    jaar = jdatum(1)
!    !
!    call datumi(jaar      ,jdatum    ,t         )
!    call hulpgr(jaar      ,t         ,v         ,f         )
!    call bewvuf(ierrs     ,kcmp      ,mxkc      ,inaam     ,knaam     , &
!              & jnaam     ,w         ,v0u       ,fr        ,v         , &
!              & f         )
!    
!    omeg = w(1)/60d0
!    ampl = ampl * fr(1)
!    phas = phas*d2r - v0u(1)
!end subroutine asc
!
!subroutine datumi(jaar      ,jdatum    ,t         )
!!!--description-----------------------------------------------------------------
!!
!!    Function: Calculates the number of hours referred to
!!              January 1, 00:00 of the year 'JAAR' from a given
!!              date/time
!! Method used:
!!
!!!--pseudo code and references--------------------------------------------------
!! NONE
!!!--declarations----------------------------------------------------------------
!    use precision
!    !
!    implicit none
!!
!! Global variables
!!
!    integer              , intent(in)  :: jaar   !!  Year
!    integer, dimension(6), intent(in)  :: jdatum !!  Date and time
!    double precision                   :: t      !!  Time in hours referred to January 1,
!                                                 !!  00:00 of the year 'JAAR'
!!
!! Local variables
!!
!    integer                 :: i     ! Help var.
!    integer                 :: jhulp ! Help var.
!    integer                 :: mnd   ! Help var. for the month
!    double precision                :: rlen  ! Length of a year in hours
!    double precision, dimension(12) :: rmd   ! The number of days of the cumulated counted months
!!
!!! executable statements -------------------------------------------------------
!!
!    rmd(1) = 0d0
!    rmd(2) = 31d0
!    rmd(3) = 59d0
!    rmd(4) = 90d0
!    rmd(5) = 120d0
!    rmd(6) = 151d0
!    rmd(7) = 181d0
!    rmd(8) = 212d0
!    rmd(9) = 243d0
!    rmd(10) = 273d0
!    rmd(11) = 304d0
!    rmd(12) = 334d0
!    !
!    jhulp = jdatum(1)
!    !
!    ! bereken maand definities voor schrikkeljaren
!    ! jaar deelbaar door 4 minus eeuwen welke niet deelbaar zijn door 4
!    !
!    if (mod(jhulp, 4) == 0) then
!       if (mod(jhulp, 100)/=0 .or. mod(jhulp, 400)==0) then
!          do i = 3, 12
!             rmd(i) = rmd(i) + 1d0
!          enddo
!       endif
!    endif
!    !
!    mnd = jdatum(2)
!    t = rmd(mnd)*24d0 + real(jdatum(3) - 1, hp)*24d0 + real(jdatum(4), hp)          &
!      & + real(jdatum(5), hp)/60d0 + real(jdatum(6), hp)/3600d0
!    !
!    ! hypothetisch geval (jhulp = jdatum(1) en jaar = jdatum(1))
!    !
!    if (jhulp /= jaar) then
!       rlen = 8760d0
!       if (jhulp <= jaar) then
!          if (mod(jhulp, 4) == 0) rlen = 8784d0
!          t = t - rlen
!       else
!          if (mod(jaar, 4) == 0) rlen = 8784d0
!          t = t + rlen
!       endif
!    endif
!end subroutine datumi
!
!subroutine hulpgr(jaar      ,tm1       ,v         ,f         )
!!!--description-----------------------------------------------------------------
!!
!!    Function: Calulates help var. V and F
!! Method used:
!!
!!!--pseudo code and references--------------------------------------------------
!! NONE
!!!--declarations----------------------------------------------------------------
!    use precision
!    !
!    implicit none
!!
!! Global variables
!!
!    integer                , intent(in)   :: jaar !!  Present year
!    double precision               , intent(in)   :: tm1  !!  Given time in hours referred to
!                                                  !!  January 1, 00:00:00
!    double precision, dimension(15)               :: v    !!  Help var. to calculate V0U()
!    double precision, dimension(25)               :: f    !!  Help var. to calculate FR()
!!
!! Local variables
!!
!    integer  :: ischrk  ! Number of leap-years since 1900
!    integer  :: j
!    double precision :: ci
!    double precision :: ci4
!    double precision :: cri
!    double precision :: dhalf   ! Value for 0.5 in SIGN function
!    double precision :: p
!    double precision :: pix2    ! PI*2.
!    double precision :: q
!    double precision :: rad     ! PI/180.
!    double precision :: ri
!    double precision :: rjaar   ! Real value of JAAR - 1900
!    double precision :: rk
!    double precision :: rn1
!    double precision :: s2ri
!    double precision :: si
!    double precision :: si4
!    double precision :: sri
!    double precision :: sri3
!    double precision :: tm3     ! ISCHRK + TM1/24.0, i.e. the number of correction-days since January 1, 1900 00:00 hour, after the length of a year is set to 365 days in the first instance
!    double precision :: z
!!
!!! executable statements -------------------------------------------------------
!!
!    ! bereken tm3 uitgaande van tm1 plus aantal schrikkeldagen extra
!    ! sinds 1900. Niet door 4 deelbare eeuwen zijn geen schrikkeldagen)
!    !
!    pix2   = 8d0*atan(1d0)
!    dhalf  = 0.5d0
!    rad    = pix2/360d0
!    rjaar  = real(jaar - 1900, hp)
!    ischrk = int((rjaar - 0.99d0)/4d0) - int((rjaar - 0.99d0)/100d0)        &
!           & + int((rjaar + 300d0 - 0.99d0)/400d0)
!    tm3    = real(ischrk, hp) + tm1/24d0
!    !
!    v(1) = (180.000d0 + 360.0000000d0*tm3)*rad
!    v(2) = (277.026d0 + 129.3848200d0*rjaar + 13.176396800000d0*tm3)*rad
!    v(3) = (334.384d0 + 40.6624700d0*rjaar + 0.111404000000d0*tm3)*rad
!    v(4) = (280.190d0 - 0.2387136d0*rjaar + 0.985647360000d0*tm3)*rad
!    v(5) = (281.221d0 + 0.0171800d0*rjaar + 0.000047064943d0*tm3)*rad
!    v(8) = (259.156d0 + 340.6718100d0*rjaar - 0.052953945000d0*tm3)*rad
!    !
!    z = 0.009415d0
!    p = atan(z*sin(v(8))/(1d0 + z*(1d0 - cos(v(8)))))
!    z = -0.17794d0
!    q = atan(z*sin(v(8))/(1d0 + z*(1d0 - cos(v(8)))))
!    !
!    v(6) = -p - q
!    v(7) = p - q
!    !
!    rk = 0.9137d0 - 0.03569d0*cos(v(8))
!    ri = atan(sqrt(1d0 - rk*rk)/rk)
!    !
!    v(9) = ri
!    !
!    p   = mod(v(3), pix2) - pix2*(sign(dhalf, v(3)) - dhalf)
!    rk  = v(6)
!    rn1 = v(7)
!    !
!    ! Initialisatie van regelmatig voorkomende argumenten
!    !
!    s2ri = sin(2d0*ri)
!    sri  = sin(ri)
!    si   = sin(0.5d0*ri)
!    cri  = cos(ri)
!    ci   = cos(0.5d0*ri)
!    !
!    v(10) = atan(s2ri*sin(rn1)/(s2ri*cos(rn1) + 0.3347d0))
!    v(11) = atan(sri*sri*sin(2d0*rn1)/(sri*sri*cos(2d0*rn1) + 0.0727d0))
!    v(12) = atan(sin(2d0*(p - rk))/(3d0*cri/(ci*ci) + cos(2d0*(p - rk))))
!    v(13) = atan(sin(2d0*(p - rk))/(ci*ci/(si*si*6d0) - cos(2d0*(p - rk)))&
!          & )
!    v(14) = 3d0*v(10)
!    v(15) = 0d0
!    !
!    ! Alle hoeken terugbrengen tot het interval 0 - 2*pi radialen
!    !
!    do j = 1, 15
!       v(j) = mod(v(j), pix2) - pix2*(sign(dhalf, v(j)) - dhalf)
!    enddo
!    !
!    ci4  = ci*ci*ci*ci
!    si4  = si*si*si*si
!    sri3 = sri*sri*sri
!    !
!    f(1)  = (2d0/3d0 - sri*sri)/0.5021d0
!    f(2)  = sri*sri/0.1578d0
!    f(3)  = sri*ci*ci/0.38d0
!    f(4)  = s2ri/0.7214d0
!    f(5)  = sri*si*si/0.0164d0
!    f(6)  = ci4/0.9154d0
!    f(7)  = sri*sri/0.1565d0
!    f(8)  = si4/0.0017d0
!    f(9)  = (sri - 1.25d0*sri3)/0.3192d0
!    f(10) = sri3/0.063d0
!    f(11) = sri*sri*ci*ci/0.1518d0
!    f(12) = (1d0 - 10d0*si*si + 15d0*si4)*ci*ci/0.5873d0
!    f(13) = (1d0 - 10d0*ci*ci + 15d0*ci4)*si*si/0.2147d0
!    f(14) = sri*ci4/0.3658d0
!    f(15) = (ci*ci - 2d0/3d0)*sri*ci*ci/0.1114d0
!    f(16) = (ci*ci - 1d0/3d0)*sri*si*si/0.0103d0
!    f(17) = ci4*ci*ci/0.8758d0
!    f(18) = ci4*si*si/0.038d0
!    f(19) = sqrt(0.8965d0*s2ri*s2ri + 0.6001d0*s2ri*cos(rn1) + 0.1006d0)
!    f(20) = sqrt(19.0444d0*sri3*sri + 2.7702d0*sri*sri*cos(2d0*rn1)           &
!          & + 0.0981d0)
!    f(21) = 6d0*cri*cos(2d0*(p - rk))/(ci*ci) + 9d0*cri*cri/(ci4)
!    f(21) = 2.6316d0*sri*ci*ci*0.5d0*sqrt(1d0 + f(21))
!    f(22) = 36d0*si4/(ci4) - 12d0*si*si/(ci*ci)*cos(2d0*(p - rk))
!    f(22) = 1.0924d0*ci4*sqrt(1d0 + f(22))
!end subroutine hulpgr
!
!function cmpnum(num       )
!!!--description-----------------------------------------------------------------
!!
!!    Function: Finds the name of the tidal component
!! Method used:
!!
!!!--pseudo code and references--------------------------------------------------
!! NONE
!!!--declarations----------------------------------------------------------------
!    use precision
!    implicit none
!!
!! Local parameters
!!
!    integer, parameter :: mxcmp = 234 !  Description and declaration in tfzeta.igs
!!
!! Global variables
!!
!    integer, intent(in)            :: num
!                                   !!  Pointer for the tidal components
!    character(8)    :: cmpnum
!                                   !!  Name of the chosen tidal component
!!
!!
!! Local variables
!!
!    character(8), dimension(0:mxcmp) :: l ! Array with the names of the tidal components
!!
!!
!!! executable statements -------------------------------------------------------
!!
!    !
!    !
!    !
!    !
!    l(0) = 'A0      '
!    l(1) = 'SA      '
!    l(2) = 'SSA     '
!    l(3) = 'MSM     '
!    l(4) = 'MM      '
!    l(5) = 'MSF     '
!    l(6) = 'MS0     '
!    l(7) = 'MF      '
!    l(8) = 'KO0     '
!    l(9) = 'MK0     '
!    l(10) = 'SNU     '
!    l(11) = 'SN      '
!    l(12) = 'MSTM    '
!    l(13) = 'MFM     '
!    l(14) = '2SM     '
!    l(15) = 'MSQM    '
!    l(16) = 'MQM     '
!    l(17) = '2SMN    '
!    l(18) = '2OK1    '
!    l(19) = '2Q1     '
!    l(20) = 'NJ1     '
!    l(21) = 'SIGMA1  '
!    l(22) = 'MUK1    '
!    l(23) = 'NUJ1    '
!    l(24) = 'Q1      '
!    l(25) = 'NK1     '
!    l(26) = 'RO1     '
!    l(27) = 'NUK1    '
!    l(28) = 'O1      '
!    l(29) = 'TAU1    '
!    l(30) = 'MP1     '
!    l(31) = 'M1B     '
!    l(32) = 'M1C     '
!    l(33) = 'M1A     '
!    l(34) = 'M1      '
!    l(35) = 'NO1     '
!    l(36) = 'CHI1    '
!    l(37) = 'LP1     '
!    l(38) = 'PI1     '
!    l(39) = 'TK1     '
!    l(40) = 'P1      '
!    l(41) = 'SK1     '
!    l(42) = 'S1      '
!    l(43) = 'K1      '
!    l(44) = 'MO1     '
!    l(45) = 'SP1     '
!    l(46) = 'PSI1    '
!    l(47) = 'RP1     '
!    l(48) = 'FI1     '
!    l(49) = 'KP1     '
!    l(50) = 'THETA1  '
!    l(51) = 'LABDAO1 '
!    l(52) = 'J1      '
!    l(53) = 'MQ1     '
!    l(54) = '2PO1    '
!    l(55) = 'SO1     '
!    l(56) = 'OO1     '
!    l(57) = '2KO1    '
!    l(58) = 'UPSILON1'
!    l(59) = 'KQ1     '
!    l(60) = '2MN2S2  '
!    l(61) = '3MKS2   '
!    l(62) = '2NS2    '
!    l(63) = '3MS2    '
!    l(64) = 'OQ2     '
!    l(65) = 'MNK2    '
!    l(66) = 'EPSILON2'
!    l(67) = 'MNS2    '
!    l(68) = '2ML2S2  '
!    l(69) = 'MNUS2   '
!    l(70) = 'MNK2S2  '
!    l(71) = '2MS2K2  '
!    l(72) = 'O2      '
!    l(73) = 'NLK2    '
!    l(74) = '2MK2    '
!    l(75) = '2N2     '
!    l(76) = 'MU2     '
!    l(77) = '2MS2    '
!    l(78) = 'SNK2    '
!    l(79) = 'NA2     '
!    l(80) = 'N2      '
!    l(81) = 'KQ2     '
!    l(82) = 'NB2     '
!    l(83) = 'NU2     '
!    l(84) = '3MSN2   '
!    l(85) = '2KN2S2  '
!    l(86) = 'OP2     '
!    l(87) = 'MSK2    '
!    l(88) = 'GAMMA2  '
!    l(89) = 'ALFA2   '
!    l(90) = 'MPS2    '
!    l(91) = 'MA2     '
!    l(92) = 'M2      '
!    l(93) = 'KO2     '
!    l(94) = 'MSP2    '
!    l(95) = 'MB2     '
!    l(96) = 'DELTA2  '
!    l(97) = 'MKS2    '
!    l(98) = 'M2(KS)2 '
!    l(99) = '2SN(MK)2'
!    l(100) = 'LABDA2  '
!    l(101) = 'SNM2    '
!    l(102) = '2MN2    '
!    l(103) = 'L2      '
!    l(104) = 'L2A     '
!    l(105) = 'L2B     '
!    l(106) = '2SK2    '
!    l(107) = 'T2      '
!    l(108) = 'S2      '
!    l(109) = 'KP2     '
!    l(110) = 'R2      '
!    l(111) = 'K2      '
!    l(112) = 'MSNU2   '
!    l(113) = 'MSN2    '
!    l(114) = 'ZETA2   '
!    l(115) = 'ETA2    '
!    l(116) = 'KJ2     '
!    l(117) = 'MKN2    '
!    l(118) = '2KM(SN)2'
!    l(119) = '2SM2    '
!    l(120) = 'SKM2    '
!    l(121) = '2MS2N2  '
!    l(122) = '2SNU2   '
!    l(123) = '2SN2    '
!    l(124) = 'SKN2    '
!    l(125) = 'MQ3     '
!    l(126) = 'NO3     '
!    l(127) = 'MO3     '
!    l(128) = '2MK3    '
!    l(129) = '2MP3    '
!    l(130) = 'M3      '
!    l(131) = 'NK3     '
!    l(132) = 'SO3     '
!    l(133) = 'MP3     '
!    l(134) = 'MK3     '
!    l(135) = 'SP3     '
!    l(136) = '2MQ3    '
!    l(137) = 'SK3     '
!    l(138) = '2SO3    '
!    l(139) = 'K3      '
!    l(140) = '4MS4    '
!    l(141) = '2MNS4   '
!    l(142) = '3MK4    '
!    l(143) = 'MNLK4   '
!    l(144) = '3MS4    '
!    l(145) = 'MSNK4   '
!    l(146) = 'MN4     '
!    l(147) = 'MNU4    '
!    l(148) = '2MLS4   '
!    l(149) = '2MSK4   '
!    l(150) = 'M4      '
!    l(151) = '2MKS4   '
!    l(152) = 'SN4     '
!    l(153) = '3MN4    '
!    l(154) = '2SMK4   '
!    l(155) = 'MS4     '
!    l(156) = 'MK4     '
!    l(157) = '2SNM4   '
!    l(158) = '2MSN4   '
!    l(159) = 'SL4     '
!    l(160) = 'S4      '
!    l(161) = 'SK4     '
!    l(162) = '2SMN4   '
!    l(163) = '3SM4    '
!    l(164) = '2SKM4   '
!    l(165) = 'MNO5    '
!    l(166) = '3MK5    '
!    l(167) = '3MP5    '
!    l(168) = 'M5      '
!    l(169) = 'MNK5    '
!    l(170) = '2MP5    '
!    l(171) = 'MSO5    '
!    l(172) = '3MO5    '
!    l(173) = 'MSK5    '
!    l(174) = '3KM5    '
!    l(175) = '2(MN)S6 '
!    l(176) = '3MNS6   '
!    l(177) = '4MK6    '
!    l(178) = '2NM6    '
!    l(179) = '4MS6    '
!    l(180) = '2MSNK6  '
!    l(181) = '2MN6    '
!    l(182) = '2MNU6   '
!    l(183) = '3MSK6   '
!    l(184) = 'M6      '
!    l(185) = 'MSN6    '
!    l(186) = 'MNK6    '
!    l(187) = '4MN6    '
!    l(188) = 'MKNU6   '
!    l(189) = '2(MS)K6 '
!    l(190) = '2MS6    '
!    l(191) = '2MK6    '
!    l(192) = '2SN6    '
!    l(193) = '3MSN6   '
!    l(194) = 'MKL6    '
!    l(195) = '2SM6    '
!    l(196) = 'MSK6    '
!    l(197) = 'S6      '
!    l(198) = '2MNO7   '
!    l(199) = '2NMK7   '
!    l(200) = 'M7      '
!    l(201) = '2MSO7   '
!    l(202) = 'MSKO7   '
!    l(203) = '2(MN)8  '
!    l(204) = '3MN8    '
!    l(205) = '3MNKS8  '
!    l(206) = 'M8      '
!    l(207) = '2MSN8   '
!    l(208) = '2MNK8   '
!    l(209) = '3MS8    '
!    l(210) = '3MK8    '
!    l(211) = '2SNM8   '
!    l(212) = 'MSNK8   '
!    l(213) = '2(MS)8  '
!    l(214) = '2MSK8   '
!    l(215) = '3SM8    '
!    l(216) = '2SMK8   '
!    l(217) = 'S8      '
!    l(218) = '2(MN)K9 '
!    l(219) = '3MNK9   '
!    l(220) = '4MK9    '
!    l(221) = '3MSK9   '
!    l(222) = '4MN10   '
!    l(223) = 'M10     '
!    l(224) = '3MSN10  '
!    l(225) = '4MS10   '
!    l(226) = '2(MS)N10'
!    l(227) = '2MNSK10 '
!    l(228) = '3M2S10  '
!    l(229) = '4MSK11  '
!    l(230) = 'M12     '
!    l(231) = '4MSN12  '
!    l(232) = '5MS12   '
!    l(233) = '3MNKS12 '
!    l(234) = '4M2S12  '
!    !
!    cmpnum = 'ERROR   '
!    if (num>=0 .and. num<=mxcmp) then
!       cmpnum = l(num)
!    endif
!end function cmpnum
!subroutine kompbs(l         )
!!!--description-----------------------------------------------------------------
!!
!!    Function: SIMULATION OF EXTERNAL KOMPBES-FILE
!! Method used:
!!
!!!--pseudo code and references--------------------------------------------------
!! NONE
!!!--declarations----------------------------------------------------------------
!    use precision
!    implicit none
!!
!! Global variables
!!
!    character(80), dimension(234), intent(out) :: l
!                                   !!  Array with tidal components
!!
!!
!!! executable statements -------------------------------------------------------
!!
!    !
!    !
!    !
!    l(1)   = 'SA                 1                            '
!    l(2)   = 'SSA                2                            '
!    l(3)   = 'MSM          1  1 -2                  1 1       '
!    l(4)   = 'MM           1 -1                     1 1       '
!    l(5)   = 'MSF          2    -2                  1 1       '
!    l(6)   = 'MS0          2    -2    -2  2         1 6       '
!    l(7)   = 'MF           2          -2            1 2       '
!    l(8)   = 'KO0          2          -2  1 -2-10   1 3119    '
!    l(9)   = 'MK0          2          -2  2   -11   120       '
!    l(10)  = 'SNU          3  1 -4    -2  2         1 6       '
!    l(11)  = 'SN           3 -1 -2    -2  2         1 6       '
!    l(12)  = 'MSTM         3  1 -2    -2            1 2       '
!    l(13)  = 'MFM          3 -1       -2            1 2       '
!    l(14)  = '2SM          4    -4    -4  4         2 6       '
!    l(15)  = 'MSQM         4    -2    -2            1 2       '
!    l(16)  = 'MQM          4 -2       -2            1 2       '
!    l(17)  = '2SMN         5 -1 -4    -4  4         2 6       '
!    l(18)  = '2OK1      1 -4     1     4 -2 -1+10   2 3119    '
!    l(19)  = '2Q1       1 -4  2  1     2 -1  1      1 3       '
!    l(20)  = 'NJ1       1 -4  2  1     2 -1  1      1 41 6    '
!    l(21)  = 'SIGMA1    1 -4     3     2 -1  1      1 3       '
!    l(22)  = 'MUK1      1 -4     3     2 -2   +10   1 6119    '
!    l(23)  = 'NUJ1      1 -4     3     2 -1  1      1 41 6    '
!    l(24)  = 'Q1        1 -3  1  1     2 -1  1      1 3       '
!    l(25)  = 'NK1       1 -3  1  1     2 -2  1+10   1 6119    '
!    l(26)  = 'RO1       1 -3 -1  3     2 -1  1      1 3       '
!    l(27)  = 'NUK1      1 -3 -1  3     2 -2 +1+10   1 6119    '
!    l(28)  = 'O1        1 -2     1     2 -1  1      1 3       '
!    l(29)  = 'TAU1      1 -2     3       -1 -1      1 4       '
!    l(30)  = 'MP1       1 -2     3     2 -2 -1      1 6       '
!    l(31)  = 'M1B       1 -1 -1  1     2 -1 -1      1 3       '
!    l(32)  = 'M1C       1 -1     1     1 -1         112       '
!    l(33)  = 'M1A       1 -1  1  1       -1 -1      1 4       '
!    l(34)  = 'M1        1 -1  1  1       -1 -1-12   121       '
!    l(35)  = 'NO1       1 -1  1  1       -1 -1      1 31 6    '
!    l(36)  = 'CHI1      1 -1 -1 +3       -1 -1      1 4       '
!    l(37)  = 'LP1       1 -1 -1  3     2 -2  1-13   122       '
!    l(38)  = 'PI1       1       -2 +1        1                '
!    l(39)  = 'TK1       1       -2  1        1+10   119       '
!    l(40)  = 'P1        1       -1           1                '
!    l(41)  = 'SK1       1       -1           1+10   119       '
!    l(42)  = 'S1        1                                     '
!    l(43)  = 'K1        1        1          -1-10   119       '
!    l(44)  = 'MO1       1        1       -1 -1      1 31 6    '
!    l(45)  = 'SP1       1        1          -1                '
!    l(46)  = 'PSI1      1        2 -1       -1                '
!    l(47)  = 'RP1       1        2 -1        1                '
!    l(48)  = 'FI1       1        3          -1                '
!    l(49)  = 'KP1       1        3          -1-11   120       '
!    l(50)  = 'THETA1    1  1  1 -1       -1 -1      1 4       '
!    l(51)  = 'LABDAO1   1  1  1 -1       -1  1      1 31 6    '
!    l(52)  = 'J1        1  1 -1  1       -1 -1      1 4       '
!    l(53)  = 'MQ1       1  1 -1  1       -1 -1      1 31 6    '
!    l(54)  = '2PO1      1  2    -3    -2  1  1      1 3       '
!    l(55)  = 'SO1       1  2    -1    -2  1 -1      1 3       '
!    l(56)  = 'OO1       1  2     1    -2 -1 -1      1 5       '
!    l(57)  = '2KO1      1  2     1    -2  1  1-10-101 3219    '
!    l(58)  = 'UPSILON1  1  3 -1  1    -2 -1  1      1 5       '
!    l(59)  = 'KQ1       1  3 -1  1    -2  1 -1-11   1 3120    '
!    l(60)  = '2MN2S2    2 -7  1  6     6 -6         3 6       '
!    l(61)  = '3MKS2     2 -6     4     6 -6   +11   3 6120    '
!    l(62)  = '2NS2      2 -6  2  4     4 -4         2 6       '
!    l(63)  = '3MS2      2 -6     6     6 -6         3 6       '
!    l(64)  = 'OQ2       2 -5  1  2     4 -2  2      2 3       '
!    l(65)  = 'MNK2      2 -5  1  2     4 -4   +11   2 6120    '
!    l(66)  = 'EPSILON2  2 -5  1  4     2 -2         1 6       '
!    l(67)  = 'MNS2      2 -5  1  4     4 -4         2 6       '
!    l(68)  = '2ML2S2    2 -5 -1  6     6 -6  2-13   2 6122    '
!    l(69)  = 'MNUS2     2 -5 -1  6     4 -4         2 6       '
!    l(70)  = 'MNK2S2    2 -5  1  6     4 -4  0-11   2 6120    '
!    l(71)  = '2MS2K2    2 -4           4 -4   +11+112 6220    '
!    l(72)  = 'O2        2 -4     2     4 -2  2      2 3       '
!    l(73)  = 'NLK2      2 -4     2     4 -4  2+11-131 6120122 '
!    l(74)  = '2MK2      2 -4     2     4 -4   +11   1 6120    '
!    l(75)  = '2N2       2 -4  2  2     2 -2         1 6       '
!    l(76)  = 'MU2       2 -4     4     2 -2         1 6       '
!    l(77)  = '2MS2      2 -4     4     4 -4         2 6       '
!    l(78)  = 'SNK2      2 -3  1        2 -2   +11   1 6120    '
!    l(79)  = 'NA2       2 -3  1  1  1                         '
!    l(80)  = 'N2        2 -3  1  2     2 -2         1 6       '
!    l(81)  = 'KQ2       2 -3  1  2     2 -1   -10   1 3119    '
!    l(82)  = 'NB2       2 -3  1  3 -1                         '
!    l(83)  = 'NU2       2 -3 -1  4     2 -2         1 6       '
!    l(84)  = '3MSN2     2 -3  1  6     4 -4         4 6       '
!    l(85)  = '2KN2S2    2 -3  1  6     2 -2   -11-111 6220    '
!    l(86)  = 'OP2       2 -2           2 -1  2      1 3       '
!    l(87)  = 'MSK2      2 -2           2 -2   +11   1 6120    '
!    l(88)  = 'GAMMA2    2 -2  2        2 -2  2      1 6       '
!    l(89)  = 'ALFA2     2 -2     1     2 -2  2      1 6       '
!    l(90)  = 'MPS2      2 -2     1     2 -2  1      1 6       '
!    l(91)  = 'MA2       2 -2     1                            '
!    l(92)  = 'M2        2 -2     2     2 -2         1 6       '
!    l(93)  = 'KO2       2 -2     2     2 -1   -10   1 3119    '
!    l(94)  = 'MSP2      2 -2     3     2 -2 -1      1 6       '
!    l(95)  = 'MB2       2 -2     3                            '
!    l(96)  = 'DELTA2    2 -2     4       -2  0      1 7       '
!    l(97)  = 'MKS2      2 -2     4     2 -2   -11   1 6120    '
!    l(98)  = 'M2(KS)2   2 -2     6     2 -2   -11-111 6220    '
!    l(99)  = '2SN(MK)2  2 -1  1 -2            +11   2 6120    '
!    l(100) = 'LABDA2    2 -1  1        2 -2  2      1 6       '
!    l(101) = 'SNM2      2 -1  1                     2 6       '
!    l(102) = '2MN2      2 -1 -1  2     2 -2         3 6       '
!    l(103) = 'L2        2 -1 -1  2     2 -2  2-13   122       '
!    l(104) = 'L2A       2 -1 -1  2     2 -2  2      1 6       '
!    l(105) = 'L2B       2 -1  1  2       -2         1 7       '
!    l(106) = '2SK2      2       -2            +11   120       '
!    l(107) = 'T2        2       -1  1                         '
!    l(108) = 'S2        2                                     '
!    l(109) = 'KP2       2                     -10   119       '
!    l(110) = 'R2        2        1 -1        2                '
!    l(111) = 'K2        2        2            -11   120       '
!    l(112) = 'MSNU2     2  1  1 -2                            '
!    l(113) = 'MSN2      2  1 -1                     2 6       '
!    l(114) = 'ZETA2     2  1  1          -2         1 7       '
!    l(115) = 'ETA2      2  1 -1  2       -2         1 7       '
!    l(116) = 'KJ2       2  1 -1  2       -1 -2-10   1 4119    '
!    l(117) = 'MKN2      2  1 -1  2            -11   2 6120    '
!    l(118) = '2KM(SN)2  2  1 -1  4            -11-112 6220    '
!    l(119) = '2SM2      2  2    -2    -2  2         1 6       '
!    l(120) = 'SKM2      2  2          -2  2   -11   1 6120    '
!    l(121) = '2MS2N2    2  2 -2                     2 6       '
!    l(122) = '2SNU2     2  3  1 -4    -2  2         1 6       '
!    l(123) = '2SN2      2  3 -1 -2    -2  2         1 6       '
!    l(124) = 'SKN2      2  3 -1       -2  2   -11   1 6120    '
!    l(125) = 'MQ3       3 -5  1  3     4 -3  1      1 31 6    '
!    l(126) = 'NO3       3 -5  1  3     4 -3  1      1 31 6    '
!    l(127) = 'MO3       3 -4     3     4 -3  1      1 31 6    '
!    l(128) = '2MK3      3 -4     3     4 -4  1+10   2 6119    '
!    l(129) = '2MP3      3 -4     5     4 -4 -1      2 6       '
!    l(130) = 'M3        3 -3     3     3 -3         117       '
!    l(131) = 'NK3       3 -3  1  3     2 -2 -1-10   1 6119    '
!    l(132) = 'SO3       3 -2     1     2 -1  1      1 3       '
!    l(133) = 'MP3       3 -2     1     2 -2  1      1 6119    '
!    l(134) = 'MK3       3 -2     3     2 -2 -1-10   1 6119    '
!    l(135) = 'SP3       3       -1           1                '
!    l(136) = '2MQ3      3 -1 -1  3     2 -3 -1      1 32 6    '
!    l(137) = 'SK3       3        1          -1-10   119       '
!    l(138) = '2SO3      3  2    -1    -2  1 -1      1 3       '
!    l(139) = 'K3        3        3          -1-10-11119120    '
!    l(140) = '4MS4      4 -8     8     8 -8         4 6       '
!    l(141) = '2MNS4     4 -7  1  6     6 -6         3 6       '
!    l(142) = '3MK4      4 -6     4     6 -6   +11   3 6120    '
!    l(143) = 'MNLK4     4 -6     4     6 -6  2+11-132 6120122 '
!    l(144) = '3MS4      4 -6     6     6 -6         3 6       '
!    l(145) = 'MSNK4     4 -5  1  2     4 -4   +11   2 6120    '
!    l(146) = 'MN4       4 -5  1  4     4 -4         2 6       '
!    l(147) = 'MNU4      4 -5 -1  6     4 -4         2 6       '
!    l(148) = '2MLS4     4 -5 -1  6     6 -6  2-13   2 6122    '
!    l(149) = '2MSK4     4 -4     2     4 -4   +11   2 6120    '
!    l(150) = 'M4        4 -4     4     4 -4         2 6       '
!    l(151) = '2MKS4     4 -4     6     4 -4   -11   2 6120    '
!    l(152) = 'SN4       4 -3  1  2     2 -2         1 6       '
!    l(153) = '3MN4      4 -3 -1  4     4 -4         4 6       '
!    l(154) = '2SMK4     4 -2           2 -2   +11   1 6120    '
!    l(155) = 'MS4       4 -2     2     2 -2         1 6       '
!    l(156) = 'MK4       4 -2     4     2 -2   -11   1 6120    '
!    l(157) = '2SNM4     4 -1  1                     2 6       '
!    l(158) = '2MSN4     4 -1 -1  2     2 -2         3 6       '
!    l(159) = 'SL4       4 -1 -1  2     2 -2  2-13   122       '
!    l(160) = 'S4        4                                     '
!    l(161) = 'SK4       4        2            -11   120       '
!    l(162) = '2SMN4     4  1 -1                     2 6       '
!    l(163) = '3SM4      4  2    -2    -2  2         1 6       '
!    l(164) = '2SKM4     4  2          -2  2   -11   1 6120    '
!    l(165) = 'MNO5      5 -7  1  5     6 -5  1      1 32 6    '
!    l(166) = '3MK5      5 -6     5     6 -6  1+10   3 6119    '
!    l(167) = '3MP5      5 -6     7     6 -6 -1      3 6       '
!    l(168) = 'M5        5 -5  1  5     4 -5 -1-12   2 6121    '
!    l(169) = 'MNK5      5 -5  1  5     4 -4 -1-10   2 6119    '
!    l(170) = '2MP5      5 -4     3     4 -4  1      2 6       '
!    l(171) = 'MSO5      5 -4     3     4 -3         1 31 6    '
!    l(172) = '3MO5      5 -4     5     4 -5 -1      1 33 6    '
!    l(173) = 'MSK5      5 -2     3     2 -2 -1-10   1 6119    '
!    l(174) = '3KM5      5 -2     5     2 -2 -3-14   1 6319    '
!    l(175) = '2(MN)S6   6-10  2  8     8 -8         4 6       '
!    l(176) = '3MNS6     6 -9  1  8     8 -8         4 6       '
!    l(177) = '4MK6      6 -8     6     8 -8   +11   4 6120    '
!    l(178) = '2NM6      6 -8  2  6     6 -6         3 6       '
!    l(179) = '4MS6      6 -8     8     8 -8         4 6       '
!    l(180) = '2MSNK6    6 -7  1  4     6 -6   +11   3 6120    '
!    l(181) = '2MN6      6 -7  1  6     6 -6         3 6       '
!    l(182) = '2MNU6     6 -7 -1  8     6 -6         3 6       '
!    l(183) = '3MSK6     6 -6     4     6 -6   +11   3 6120    '
!    l(184) = 'M6        6 -6     6     6 -6         3 6       '
!    l(185) = 'MSN6      6 -5  1  4     4 -4         2 6       '
!    l(186) = 'MNK6      6 -5  1  6     4 -4   -11   2 6120    '
!    l(187) = '4MN6      6 -5 -1  6     6 -6         5 6       '
!    l(188) = 'MKNU6     6 -5 -1  8     4 -4   -11   2 6120    '
!    l(189) = '2(MS)K6   6 -4     2     4 -4   +11   2 6120    '
!    l(190) = '2MS6      6 -4     4     4 -4         2 6       '
!    l(191) = '2MK6      6 -4     6     4 -4   -11   2 6120    '
!    l(192) = '2SN6      6 -3  1  2     2 -2         1 6       '
!    l(193) = '3MSN6     6 -3 -1  4     4 -4         4 6       '
!    l(194) = 'MKL6      6 -3 -1  6     4 -4  2-11-131 6120122 '
!    l(195) = '2SM6      6 -2     2     2 -2         1 6       '
!    l(196) = 'MSK6      6 -2     4     2 -2   -11   1 6120    '
!    l(197) = 'S6        6                                     '
!    l(198) = '2MNO7     7 -9  1  7     8 -7  1      1 33 6    '
!    l(199) = '2NMK7     7 -8  2  7     6 -6 -1-10   3 6119    '
!    l(200) = 'M7        7 -7  1  7     6 -7 -1-12   3 6121    '
!    l(201) = '2MSO7     7 -6     5     6 -5  1      1 32 6    '
!    l(202) = 'MSKO7     7 -4     5     4 -3  1-11   1 31 6120 '
!    l(203) = '2(MN)8    8-10  2  8     8 -8         4 6       '
!    l(204) = '3MN8      8 -9  1  8     8 -8         4 6       '
!    l(205) = '3MNKS8    8 -9  1 10     8 -8   -11   4 6120    '
!    l(206) = 'M8        8 -8     8     8 -8         4 6       '
!    l(207) = '2MSN8     8 -7  1  6     6 -6         3 6       '
!    l(208) = '2MNK8     8 -7  1  8     6 -6   -11   3 6120    '
!    l(209) = '3MS8      8 -6     6     6 -6         3 6       '
!    l(210) = '3MK8      8 -6     8     6 -6   -11   3 6120    '
!    l(211) = '2SNM8     8 -5  1  4     4 -4         2 6       '
!    l(212) = 'MSNK8     8 -5  1  6     4 -4   -11   2 6120    '
!    l(213) = '2(MS)8    8 -4     4     4 -4         2 6       '
!    l(214) = '2MSK8     8 -4     6     4 -4   -11   2 6120    '
!    l(215) = '3SM8      8 -2     2     2 -2         1 6       '
!    l(216) = '2SMK8     8 -2     4     2 -2   -11   1 6120    '
!    l(217) = 'S8        8                                     '
!    l(218) = '2(MN)K9   9-10  2  9     8 -8 -1-10   4 6119    '
!    l(219) = '3MNK9     9 -9  1  9     8 -8 -1-10   4 6119    '
!    l(220) = '4MK9      9 -8     9     8 -8 -1-10   4 6119    '
!    l(221) = '3MSK9     9 -6     7     6 -6 -1-10   3 6119    '
!    l(222) = '4MN10    10-11  1 10    10-10         5 6       '
!    l(223) = 'M10      10-10    10    10-10         5 6       '
!    l(224) = '3MSN10   10 -9  1  8     8 -8         4 6       '
!    l(225) = '4MS10    10 -8     8     8 -8         4 6       '
!    l(226) = '2(MS)N10 10 -7  1  6     6 -6         3 6       '
!    l(227) = '2MNSK10  10 -7  1  8     6 -6   -11   3 6120    '
!    l(228) = '3M2S10   10 -6     6     6 -6         3 6       '
!    l(229) = '4MSK11   11 -8     9     8 -8 -1-10   4 6119    '
!    l(230) = 'M12      12-12    12    12-12         6 6       '
!    l(231) = '4MSN12   12-11  1 10    10-10         5 6       '
!    l(232) = '5MS12    12-10    10    10-10         5 6       '
!    l(233) = '3MNKS12  12 -9  1 10     8 -8   -11   4 6120    '
!    l(234) = '4M2S12   12 -8     8     8 -8         4 6       '
!end subroutine kompbs
!
!subroutine bewvuf(ierrs     ,kcmp      ,mxkc      ,inaam     ,knaam     , &
!                & jnaam     ,w         ,v0u       ,fr        ,v         , &
!                & f         )
!!!--description-----------------------------------------------------------------
!!
!!    Function: calculates V0U() and FR()
!! Method used:
!!
!!!--pseudo code and references--------------------------------------------------
!! NONE
!!!--declarations----------------------------------------------------------------
!    use precision
!    use unstruc_messages
!    !
!    implicit none
!!
!! Global variables
!!
!    integer                                       :: ierrs !!  Number of error messages
!    integer                         , intent(in)  :: kcmp
!    integer                         , intent(in)  :: mxkc
!    integer     , dimension(mxkc*16), intent(in)  :: jnaam !!  Help var.
!    character(8),                     intent(in)  :: inaam !!  Name of the referenced components
!    character(8), dimension(mxkc)   , intent(in)  :: knaam !!  Names of all components
!    double precision    , dimension(15)     , intent(in)  :: v     !!  Help var. to calculate V0U()
!    double precision    , dimension(25)     , intent(in)  :: f     !!  Help var. to calculate FR()
!    double precision    , dimension(kcmp)                 :: fr    !!  Amplitude factors for the referenced
!                                                           !!  components
!    double precision    , dimension(kcmp)                 :: v0u   !!  Astronomical arguments of the
!                                                           !!  referenced components
!    double precision    , dimension(kcmp)                 :: w     !!  Angular velocity of the referenced
!                                                           !!  components
!!
!! Local variables
!!
!    integer  :: ia1
!    integer  :: ia2
!    integer  :: iar
!    integer  :: ie1
!    integer  :: ie2
!    integer  :: iex
!    integer  :: ikomp
!    integer  :: j
!    integer  :: kw
!    integer  :: kx
!    integer  :: mh
!    integer  :: mp
!    integer  :: mp1
!    integer  :: ms
!    integer  :: mt
!    double precision :: dhalf   ! Value for 0.5 in SIGN function
!    double precision :: pix2
!    double precision :: s1
!    double precision :: s2
!    
!    character(len=255) :: message
!!
!!! executable statements -------------------------------------------------------
!!
!    pix2 = 8d0*atan(1d0)
!    dhalf = 0.5d0
!    !
!    ! loop over given components
!    !
!    do ikomp = 1, kcmp
!       !
!       ! loop over the elements of kompbes
!       !
!       do j = 1, mxkc
!          !
!          ! test on name of present component
!          !
!          if (inaam==knaam(j)) then
!             !
!             ! compute angular velocity
!             !
!             mt = jnaam(16*j - 15)
!             ms = jnaam(16*j - 14)
!             mp = jnaam(16*j - 13)
!             mh = jnaam(16*j - 12)
!             mp1 = jnaam(16*j - 11)
!             w(ikomp) = mt*15d0 + ms*0.54901653d0 + mp*0.0046418333d0 +       &
!                      & mh*0.04106864d0 + mp1*0.0000019610393d0
!             w(ikomp) = (w(ikomp)*pix2)/360d0
!             !
!             ! compute v0+u
!             !
!             v0u(ikomp) = (jnaam(16*j - 8)*pix2)/4d0
!             do kw = 1, 7
!                kx = 16*j - 16 + kw
!                v0u(ikomp) = v0u(ikomp) + v(kw)*jnaam(kx)
!             enddo
!             ie1 = jnaam(16*j - 7)
!             if (ie1/=0) then
!                ia1 = abs(ie1)
!                s1 = real(ie1/ia1, hp)
!                v0u(ikomp) = v0u(ikomp) + s1*v(ia1)
!                ie2 = jnaam(16*j - 6)
!                if (ie2/=0) then
!                   ia2 = abs(ie2)
!                   s2 = real(ie2/ia2, hp)
!                   v0u(ikomp) = v0u(ikomp) + s2*v(ia2)
!                endif
!             endif
!             v0u(ikomp) = mod(v0u(ikomp), pix2)                                 &
!                        & - pix2*(sign(dhalf, v0u(ikomp)) - dhalf)
!             !
!             ! compute f
!             !
!             fr(ikomp) = 1d0
!             iex = jnaam(16*j - 5)
!             if (iex/=0) then
!                iar = jnaam(16*j - 4)
!                fr(ikomp) = (f(iar))**iex
!                iex = jnaam(16*j - 3)
!                if (iex/=0) then
!                   iar = jnaam(16*j - 2)
!                   fr(ikomp) = fr(ikomp)*(f(iar))**iex
!                   iex = jnaam(16*j - 1)
!                   if (iex/=0) then
!                      iar = jnaam(16*j)
!                      fr(ikomp) = fr(ikomp)*(f(iar))**iex
!                   endif
!                endif
!             endif
!             exit
!          endif
!          if (j>=mxkc) then
!             ierrs = ierrs + 1
!             message = ''
!             write (message, '(a,a,a)') '*** ERROR Component ', inaam ,         &
!                                 & ' not in internal component base'
!             call mess(LEVEL_WARN,trim(message))
!             exit
!          endif
!       enddo
!    enddo
!       
!end subroutine bewvuf
!
!!subroutine polyindexweight( xe, ye, xs, ys, kcs, ns, xyen, k1, rl)    ! interpolate in a polyline like way
!!
!! ! Global variables
!! integer ,                intent(in)     :: ns       ! Dimension of polygon OR LINE BOUNDARY
!! double precision, dimension(:),  intent(in) :: xs       ! polygon
!! double precision, dimension(:),  intent(in) :: ys
!! integer, dimension(:),  intent(in)      :: kcs      ! polygon mask
!! double precision                        :: xyen(:)
!! double precision                        :: xe, ye, rl
!!
!!
!! integer :: ja1, ja2, k, km, k1, k2
!! double precision:: x1,x2,y1,y2,dis,xn,yn,dx,dy
!! double precision:: dism, dis1, dis2, rl1, rl2, dbdistance
!!
!!
!! dism = 1e30
!! do k = 1, ns
!!    dis  = DbdISTANCE( Xe,Ye,XS(K),YS(K) )
!!    if (dis < dism) then
!!       dism = dis
!!       km   = k
!!    endif
!! enddo
!!
!! k1 = 0
!!
!! if (km == 1) then
!!    x1 = xs(km  ); y1 = ys(km  )
!!    x2 = xs(km+1); y2 = ys(km+1)
!!    call LINEDISQ(Xe,Ye,X1,Y1,X2,Y2,JA1,DIS1,XN,YN,RL)
!!    if (ja1 == 1) then
!!       if (dis1 < rdis) k1 = km
!!    endif
!! else if (km == ns) then
!!    x1 = xs(km-1); y1 = ys(km-1)
!!    x2 = xs(km  ); y2 = ys(km  )
!!    call LINEDISQ(Xe,Ye,X1,Y1,X2,Y2,JA1,DIS1,XN,YN,RL)
!!    if (ja1 == 1) then
!!       if (dis1 < rdis) k1 = km-1
!!    endif
!! else
!!    x1 = xs(km-1); y1 = ys(km-1)
!!    x2 = xs(km)  ; y2 = ys(km)
!!    call LINEDISQ(Xe,Ye,X1,Y1,X2,Y2,JA1,DIS1,XN,YN,RL1)
!!    x1 = xs(km)  ; y1 = ys(km)
!!    x2 = xs(km+1); y2 = ys(km+1)
!!    call LINEDISQ(Xe,Ye,X1,Y1,X2,Y2,JA2,DIS2,XN,YN,RL2)
!!    if      (ja1 == 1) then ! if on line 1
!!        if (dis1 < rdis) then
!!           k1 = km-1 ; rl = rl1
!!        endif
!!    else if (ja2 == 1) then
!!        if (dis2 < rdis) then
!!           k1 = km ; rl = rl2
!!        endif
!!    else ! niet op een van beiden, maar wel in de buurt, uitwerken. Nu dus alleen convexe randen
!!    endif
!! endif
!!
!!end subroutine polyindexweight
!
!function updatetimespaceproviders(idom, qid, time) result(success) ! nb, doet enkel qid, niet allen
!  implicit none
!  logical success
!
!
!  ! Globals
!  double precision, intent(in) :: time
!  integer, intent(in)          :: idom
!  character(len=*)             :: qid
!
!  ! Locals
!  integer                      :: i, j, k
!  integer                      :: numproviders
!
!  type(tdataprovider), pointer :: dataproviders(:) ! bevat het aanbod
!  type(telementset),   pointer :: elementsets(:)   ! bevat elementsets van zowel vraag als aanbod
!  type(tquantity),     pointer :: quantities(:)    ! bevat de vraag
!  type(tdataprovider), pointer :: dataprovider     ! bevat een aanbieder
!
!
!  !if (time .le. timelast) return
!  !timelast = time
!  
!  success = .false.
!
!  dataproviders => subdoms(idom)%dataproviders
!  elementsets   => subdoms(idom)%elementsets
!
!  if (subdoms(idom)%ini == 0) then  ! once for each domain once, assemble list of providers for *each* quantity
!      subdoms(idom)%ini = 1
!      quantities => subdoms(idom)%quantities
!      do i = 1,size(quantities)
!         do k = 1,2
!            numproviders = 0
!            do j = 1,size(dataproviders)
!               if (quantities(i)%qid .eq. dataproviders(j)%qid .and. dataproviders(j)%method .ne. justupdate) then
!                  numproviders = numproviders + 1
!                  if (k == 2) quantities(i)%providernrs(numproviders) = j
!               endif
!            enddo
!            if (k == 1) allocate( quantities(i)%providernrs(numproviders) )
!         enddo
!      enddo
!  endif
!
!  do i = 1, size(subdoms(idom)%dataproviders)
!     dataprovider => subdoms(idom)%dataproviders(i)
!     if (qid == dataprovider%qid) then  
!        success = updateprovider(dataprovider ,time, elementsets)
!        if (.not. success) return
!     endif
!  enddo
!  
!  success = .true.
!end function updatetimespaceproviders
!
!
!function updateprovider(dataprovider, tim, elementsets) result(success)
!  use m_itdate
!
!  implicit none
!
!  logical :: success
!
!  ! Update information of an item in the meteo module.
!  ! Bij Uniuvp, ook direct transformatie
!
!
!
!  ! globals
!  double precision, intent(in)        :: tim       ! flow time to update to
!  type(tdataprovider), pointer        :: dataprovider
!  type(telementset),   pointer        :: elementsets(:)
!
!  ! locals
!  integer                             :: mx
!  integer                             :: nx
!  integer                             :: kx
!  integer, pointer                    :: minp
!  integer                             :: it1
!  integer                             :: filetype
!  integer                             :: ielset
!  double precision                    :: dmiss
!  double precision                    :: tread
!  double precision                    :: x0r
!  double precision                    :: y0r
!  double precision, dimension(:),     pointer:: uz     ! 1-dim array
!  double precision, dimension(:,:),   pointer:: vz     ! 2-dim array
!  double precision, dimension(:,:,:), pointer:: wz     ! 3-dim array
!
!  ! only for triangulation
!  integer                             :: m, mm, i0, i1, k, ns, ielsetq, jdla, jpoly
!  double precision                    :: a0, a1, t0, t1, t1min, t0max, treadlast
!  double precision, pointer           :: u0(:)     ! 2-dim array
!  double precision, pointer           :: u1(:)
!  double precision, pointer           :: xs(:),ys(:) ! pointers naar elementset provider
!  double precision, allocatable       :: zs(:)       ! maar zs en kcss blijven lokaal
!  integer , pointer                   :: kcss(:)
!
!  double precision, pointer           :: x(:),y(:)   ! pointers naar elementset v/d quantity
!  integer , pointer                   :: kcs(:)      !
!  double precision, pointer           :: xyen(:,:)   ! pointer naar cell tolerance, aleen voor polyint
!  double precision, pointer           :: z(:,:,:)    ! pointer naar provider field, met dimension v/d quantity
!  double precision                    :: timdiff
!  ! end only
!
!
!  ! only for triangulation:
!  integer, save :: ncheckprev   ! jdla 1 if ncheckprev .ne. ncheck todo: store this in provider
!  integer       :: ncheck, mxq
!
!  it1   = dataprovider%it1
!
!  do while (tim > dataprovider%field(it1)%time)
!
!     tread    = 0
!     minp     =>dataprovider%minp ! pointer, so closed minp (=0) is set for provider directly.
!     mx       = dataprovider%mx
!     nx       = dataprovider%nx
!     kx       = dataprovider%kx
!     dmiss    = dataprovider%dmiss
!     filetype = dataprovider%filetype
!     ielset   = dataprovider%field(it1)%ielset
!
!     if ( tim > dataprovider%field(it1)%time) then
!        dataprovider%it0 =     dataprovider%it1
!        dataprovider%it1 = 1 - dataprovider%it1
!        it1              =     dataprovider%it1
!        select case (filetype)
!
!           case ( uniform, unimagdir )
!
!              uz     => dataprovider%field(it1)%arr1d
!           10 treadlast = tread
!              success = readseries(minp,uz,kx,tread)
!              if (.not. success) return
!
!              if (uz(1) .eq. dmiss .and. dataprovider%field(1-it1)%arr1d(1) .eq. dmiss) then
!                 goto 10  ! hooguit 1 missing in geheugen
!              endif
!
!           case (fourier)
!
!              tread   = dataprovider%field(1)%time  ! groot negatief bij aanvang
!              success = readfouriercompstim(minp,dataprovider%field(0)%arr2d, dataprovider%field(1)%arr2d,  &
!                                            mx,nx,kx,tim,tread)     ! field 0 holds comps, 1 holds result
!
!              dataprovider%field(0)%time  = tread                   ! trick fourier into being update
!              dataprovider%field(1)%time  = tread
!
!              if (.not. success) return
!              
!           case ( qhtable )
!
!              tread   = dataprovider%field(1)%time  ! groot negatief bij aanvang
!              success = readtableentries(minp,dataprovider%field(0)%arr2d,dataprovider%field(1)%arr2d,  &
!                                            mx,nx,kx,tim,tread)      ! field 1 holds table
!                                                                     ! field 0 holds slope and x=0 crossing
!
!              !dataprovider%field(0)%time  = tread                   ! trick qhtable into being update (not needed, read just once)
!              !dataprovider%field(1)%time  = tread
!
!              if (.not. success) return              
!
!           case ( svwp  )
!
!              wz     => dataprovider%field(it1)%arr3d
!              success = reaspv(minp,wz,mx,nx,kx,tread)
!              if (.not. success) return
!
!           case ( arcinfo )
!
!              vz      => dataprovider%field(it1)%arr2d
!              success = reaarctim(minp,vz,mx,nx,tread,dmiss)
!              
!              if (.not. success) return
!
!           case ( ncgrid )
!
!              vz     => dataprovider%field(it1)%arr2d ! TODO: AvD: nc precipitation files usually have constant intervals of rain, i.e. no linear interpolation. Later: make this dependant on time interpolation method.
!              success = read_nc_field(minp, 'rainfall', vz,mx,nx,tread,dmiss) ! hk: tread aanpassen aan onze tijd
!
!              if (.not. success) then 
!                 return
!              else
!                 continue 
!              endif
!
!           case ( curvi )
!
!              ielsetq = dataprovider%ielsetq 
!              mxq     = size(elementsets(ielsetq)%x)         ! mxq = size of quantity that is kept in memory twice
!              x       => elementsets(ielsetq)%x
!              y       => elementsets(ielsetq)%y
!              wz      => dataprovider%field(it1)%arr3d
!            
!              success = reaarc_curv_tim(minp,x,y,wz,dataprovider%indxn,dataprovider%wfn,kx,mxq,dataprovider%refresh,dmiss,tread)
!
!              if (.not. success) return              
! 
!           case ( spiderweb )
!
!              wz     => dataprovider%field(it1)%arr3d
!              success = reaspwtim(minp,wz,mx,nx,tread,x0r,y0r)
!              if (.not. success) return
!              dataprovider%field(it1)%arr1d(1) = x0r
!              dataprovider%field(it1)%arr1d(2) = y0r
!              elementsets(ielset)%x(1) = x0r
!              elementsets(ielset)%y(1) = y0r
!
!           case ( multiple_uni )                            ! several uniform timeseries without interpolation 
!              
!              
!              tread   = tim
!              ielsetq = dataprovider%ielsetq    
!              ns      = size(elementsets(ielsetq)%x)         ! ielset vrager is de paraplu
!          
!              wz    => dataprovider%field(it1)%arr3d
!              wz = dmiss
!  
!              do m  = 1,ns                                   ! in deze set zitten pointers naar punt providers
!                 mm    =      elementsets(ielsetq)%kcs(m)
!                 if (mm == 0) then
!                    cycle
!                 end if
!
!                 i0    =  subdoms(idom)%dataproviders(mm)%it0
!                 i1    =  subdoms(idom)%dataproviders(mm)%it1
!                 dmiss =  subdoms(idom)%dataproviders(mm)%dmiss
!                 u1    => subdoms(idom)%dataproviders(mm)%field(i1)%arr1d
!                 u0    => subdoms(idom)%dataproviders(mm)%field(i0)%arr1d
!                 if (u1(1) .ne. dmiss) then                           ! als rechts er is
!                     t1 = subdoms(idom)%dataproviders(mm)%field(i1)%time
!                     t0 = subdoms(idom)%dataproviders(mm)%field(i0)%time
!                     if (t1 == t0) then
!                         a1 = 1d0
!                     else
!                         a1 = (tread - t0)/ (t1-t0)
!                     endif
!                     a0 = 1d0 - a1
!                     do k = 1,kx
!                        wz(m,1,k) = a0*u0(k) + a1*u1(k)
!                     enddo
!                  endif     
!              enddo
!              
!              a0 = 2d0
!                 
!           case ( triangulationmagdir, poly_tim )
!
!              jpoly = 0
!              if (filetype == poly_tim) jpoly = 1           ! polyline interpolation for boundary conditions
!
!              ns   = size(elementsets(ielset)%x)/2          ! de data staat op dit providersfield, windstationnetjes of polyline
!              xs   => elementsets(ielset)%x
!              ys   => elementsets(ielset)%y
!              kcss => elementsets(ielset)%kcs               ! in deze set zitten pointers naar de punt providers
!
!              call realloc( zs,kx*ns )
!
!
!              t1min = 1d30
!              t0max = -1d30                  ! zoeken naar meest nabije punt in toekomst, t1min
!              do m  = 1,ns
!                 mm    =  kcss(m)                           ! in deze set zitten pointers naar de punt providers
!
!                 ! If current child provider does not exist, cycle to next.
!                 if (mm <= 0 .or. mm > size(subdoms(idom)%dataproviders)) then
!                     cycle
!                 end if
!
!                 i0    =  subdoms(idom)%dataproviders(mm)%it0
!                 i1    =  subdoms(idom)%dataproviders(mm)%it1
!                 ! dmiss =  subdoms(idom)%dataproviders(mm)%dmiss
!                 ! u1    => subdoms(idom)%dataproviders(mm)%field(i1)%arr1d
!                 ! u0    => subdoms(idom)%dataproviders(mm)%field(i0)%arr1d
!                 t1    =  subdoms(idom)%dataproviders(mm)%field(i1)%time
!                 if (t1 .lt. t1min) then
!                     t1min  = t1                            ! meest nabije tijdstip in toekomst
!                 endif
!
!                 if (dataprovider%field(1-it1)%time == t01ini) then     ! only at first step also look for max of t0 times
!                    t0 = subdoms(idom)%dataproviders(mm)%field(i0)%time ! dus meest nabije punt verleden
!                    if (t0 .gt. t0max) then
!                        t0max = t0
!                    endif
!                 endif
!              enddo
!              
!              tread   = t1min
!              
!
!
!              
!              
!              ielsetq = dataprovider%ielsetq                    ! we interpoleren voor dit quantityfield
!              x       => elementsets(ielsetq)%x
!              y       => elementsets(ielsetq)%y
!              kcs     => elementsets(ielsetq)%kcs
!
!
!              ! Now t1min is known, so interpolate in time for all separate child providers.
!              zs = 0
!              kcss(ns+1:) = 0
!              ncheck = 0
!              do m  = 1,ns                                      ! in deze set zitten pointers naar punt providers
!                 mm    =      elementsets(ielset)%kcs(m)
!
!                 ! If current child provider does not exist, cycle to next.
!                 if (mm <= 0 .or. mm > size(subdoms(idom)%dataproviders)) then
!                    cycle
!                 end if
!
!                 if (subdoms(idom)%dataproviders(mm)%filetype == fourier) then ! no time interpolation for fourier,
!                  !  u1 => subdoms(idom)%dataproviders(mm)%field(1 )%arr1d      ! just set zs, contained in field(1)
!                    do k = 1,kx
!                       zs(kx*(m-1)+k) = subdoms(idom)%dataproviders(mm)%field(1 )%arr1d(k)   ! just set zs, contained in field(1)
!                    enddo
!                    kcss(ns+m) = 1                                             ! fourier never gaps
!                 else
!                    i0    =  subdoms(idom)%dataproviders(mm)%it0
!                    i1    =  subdoms(idom)%dataproviders(mm)%it1
!                    dmiss =  subdoms(idom)%dataproviders(mm)%dmiss
!                    u1    => subdoms(idom)%dataproviders(mm)%field(i1)%arr1d
!                    u0    => subdoms(idom)%dataproviders(mm)%field(i0)%arr1d
!                    if (u1(1) .ne. dmiss) then                           ! als rechts er is
!                       t1 = subdoms(idom)%dataproviders(mm)%field(i1)%time
!                       if (u0(1) .ne. dmiss .or. t1min == t1 ) then      ! en links is er ook of de tijd is rechts
!                          t0    = subdoms(idom)%dataproviders(mm)%field(i0)%time
!                          if (t1 == t0) then
!                             a1 = 1d0
!                          else
!                             a1 = (t1min - t0)/ (t1-t0)
!                          endif
!                          a0 = 1d0 - a1
!                          if (jpoly == 0) then                          ! special case of mag/dir interpolation
!                             k  = 1
!                             call magdir2uv( u0,u1,a0,a1,zs(kx*(m-1)+k) )
!                             do k = 3,kx
!                                zs(kx*(m-1)+k) = a0*u0(k) + a1*u1(k)
!                             enddo
!                          else                                          ! the rest is standard
!                             do k = 1,kx
!                                zs(kx*(m-1)+k) = a0*u0(k) + a1*u1(k)
!                             enddo
!                          endif
!                          kcss(ns+m) = 1
!                          ncheck = ncheck + m*m                         ! check only for time series that may contain defaults
!                       endif                                            ! check difference may cause re-interpolation
!                    endif
!                 endif
!              enddo
!
!
!              ! Now zs contains the time-interpolated values for all child providers.
!              ! Do the spatial interpolation now.
!              z    => dataprovider%field(it1)%arr3d
!              z = dmiss
!              if (jpoly == 0) then
!                 jdla = 0
!                 if (ncheck .ne. ncheckprev) then
!                    jdla = 1 ! triangulate only when
!                 end if
!
!                 ncheckprev = ncheck
!
!                 call triint  (xs, ys, zs , kcss(ns+1:), ns,      &
!                               x , y , z  , kcs , kx, mx*nx, jdla )
!
!                 if (mdia > 0) then
!                    write(mdia,*) tim, tread, ncheck, jdla
!                    write(mdia,*) (zs(2*m-1), m = 1,ns)
!                 endif
!
!              else
!                 xyen => elementsets(ielsetq)%xyen
!                 if (dataprovider%method == weightfactors) then
!                    jdla = dataprovider%refresh
!                    call polyint (xs, ys, zs , kcss(ns+1:), ns,      &
!                                  x , y , z  , kcs , kx, mx, jdla, xyen, &
!                                  dataprovider%indxn, dataprovider%wfn)
!                                    !hier niet mx*nx, mx = elsetq
!                    dataprovider%refresh = 0
!                 else
!                    jdla = 1
!                    call polyint (xs, ys, zs , kcss(ns+1:), ns,      &
!                                  x , y , z  , kcs , kx, mx, jdla, xyen)
!                                    !hier niet mx*nx, mx = elsetq
!                 end if
!
!                 if (mdia > 0) then
!                    write(mdia,*) tim, tread, ns, ncheck, jpoly
!                    write(mdia,'(300F6.2)') (zs(m), m = 1,ns)
!                 endif
!
!              endif
!
!
!              ! onderstaande code om het veld ook eenmalig voor t = t0 in te vullen.
!              ! lijkt veel op hierboven, zou in lus kunnen, waarschijnlijk minder overzichtelijk => zo laten
!              ! index 1-it1 ipv it1, t0max ipv t1min
!            !  if ( dataprovider%field(1-it1)%time /= t0max) then !
!               if ( dataprovider%field(1-it1)%time == t01ini) then ! TODO: nog als .or. bij regel hierboven
!                  ! AvD en HK : eenmalig doen
!                   
!                   
!                 zs = 0
!                 kcss(ns+1:) = 0
!                 do m  = 1,ns                                      ! in deze set zitten pointers naar punt providers
!                    mm = elementsets(ielset)%kcs(m)
!
!                    ! If current child provider does not exist, cycle to next.
!                    if (mm <= 0 .or. mm > size(subdoms(idom)%dataproviders)) then
!                       cycle
!                    end if
!
!                    if (subdoms(idom)%dataproviders(mm)%filetype == fourier) then ! no time interpolation for fourier,
!               !       u1 => subdoms(idom)%dataproviders(mm)%field(1 )%arr1d      ! just set zs
!                       do k = 1,kx                                                ! @melding dangling in salfor
!               !          zs(kx*(m-1)+k) = u1(k)
!                          zs(kx*(m-1)+k) = subdoms(idom)%dataproviders(mm)%field(1 )%arr1d(k)
!                       enddo
!                       kcss(ns+m) = 1
!                    else
!                       i0    =  subdoms(idom)%dataproviders(mm)%it0
!                       i1    =  subdoms(idom)%dataproviders(mm)%it1
!                       dmiss =  subdoms(idom)%dataproviders(mm)%dmiss
!                       u1    => subdoms(idom)%dataproviders(mm)%field(i1)%arr1d
!                       u0    => subdoms(idom)%dataproviders(mm)%field(i0)%arr1d
!                       if (u0(1) .ne. dmiss) then                              ! als links er is
!                          t0 = subdoms(idom)%dataproviders(mm)%field(i0)%time
!!                          if (u1(1) .ne. dmiss .and. u1(2) .ne. dmiss .or. &  ! en rechts is er ook of de tijd is links
!!                             t0max == t0                             ) then
!                          if (u1(1) .ne. dmiss .or. t0max == t0 ) then
!
!                             t1    = subdoms(idom)%dataproviders(mm)%field(i1)%time
!                             if (t1 == t0) then
!                                a1 = 0d0 ! anders dan boven
!                             else
!                                a1 = (t0max - t0)/ (t1-t0)
!                             endif
!                             a0 = 1d0 - a1
!                             if (jpoly == 0) then                            ! special case of mag/dir interpolation
!                                k  = 1
!                                call magdir2uv( u0,u1,a0,a1,zs(kx*(m-1)+k) )
!                                do k = 3,kx
!                                   zs(kx*(m-1)+k) = a0*u0(k) + a1*u1(k)
!                                enddo
!                             else                                            ! the rest is standard
!                                do k = 1,kx
!                                   zs(kx*(m-1)+k) = a0*u0(k) + a1*u1(k)
!                                enddo
!                             endif
!                             kcss(ns+m) = 1
!                          endif
!                       endif
!                    endif
!                 enddo
!
!                 z       => dataprovider%field(1-it1)%arr3d
!                 z = dmiss
!                 if (jpoly == 0) then
!                    jdla = 1   ! always triangulate
!                    call triint  (xs, ys, zs , kcss(ns+1:), ns,             &
!                                  x , y , z  , kcs , kx, mx*nx, jdla)
!                 else
!                    xyen => elementsets(ielsetq)%xyen
!                    if (dataprovider%method == weightfactors) then
!                        jdla = dataprovider%refresh
!                        call polyint (xs, ys, zs , kcss(ns+1:), ns,      &
!                                     x , y , z  , kcs , kx, mx, jdla, xyen, &
!                                     dataprovider%indxn, dataprovider%wfn)
!                                       !hier niet mx*nx, mx = elsetq
!                        dataprovider%refresh = 0
!                    else
!                       jdla = 1
!                       call polyint (xs, ys, zs , kcss(ns+1:), ns,      &
!                                     x , y , z  , kcs , kx, mx, jdla, xyen)
!                                       !hier niet mx*nx, mx = elsetq
!                    end if
!
!!                    call polyint (xs, ys, zs , kcss(ns+1:), ns,             &
!!                                  x , y , z  , kcs , kx, mx, jpoly, xyen)
!                 endif
!
!                 dataprovider%field(1-it1)%time = t0max
!              endif
!
!
!        end select
!
!
!        dataprovider%field(it1)%time  = tread
!     endif
!
!  enddo
!  success = .true.
!end function updateprovider
!
!
!function gettimespacevalue(idom, qid, time, z) result(success)
!use m_arcuv
!use m_spiderweb
!use m_wind
!use precision_basics
!implicit none
!
!  logical :: success
!
!  ! Global variables
!  integer                ,intent(in)  :: idom    ! domnr
!  character(*)           ,intent(in)  :: qid     !
!  double precision       ,intent(in)  :: time    !
!  double precision       ,intent(out) :: z(:)     ! resultvector for this quantity
!
!  !Local variables
!  integer                             :: i
!  integer                             :: k
!
!  integer                             :: kx, nmx, mx, nx, method
!  integer                             :: it1
!  integer                             :: it0
!  integer                             :: ierr, ielset
!  integer                             :: m, n, num
!  integer                             :: i1
!  integer                             :: j1, j, iop 
!  integer                             :: filetype
!  integer                             :: upperindex(3),lowerindex(3)
!
!  character(len=1)                    :: operand
!
!  double precision                    :: t1
!  double precision                    :: t0
!  double precision                    :: a1
!  double precision                    :: a0
!
!  double precision                    :: dmiss
!
!  double precision                    :: x01
!  double precision                    :: y01
!  double precision                    :: dx1
!  double precision                    :: dy1
!  double precision                    :: x1
!  double precision                    :: y1
!  double precision                    :: di1
!  double precision                    :: dj1
!  double precision                    :: vv0
!  double precision                    :: vv1
!  double precision                    :: rr
!  double precision, dimension(4)              :: f
!  double precision, dimension(4)              :: u
!  double precision, dimension(4)              :: v
!  double precision, dimension(40)             :: uv     ! 40=u+v plus 38 possible other quantities on 1 meteo station
!
!  double precision, dimension(:)    , pointer :: u0     ! 1-dim array
!  double precision, dimension(:)    , pointer :: u1
!  double precision, dimension(:,:)  , pointer :: v0     ! 2-dim array
!  double precision, dimension(:,:)  , pointer :: v1
!  double precision, dimension(:,:,:), pointer :: w0     ! 3-dim array
!  double precision, dimension(:,:,:), pointer :: w1
!  
!  double precision                    :: xeye0, xeye1, xeye, xc
!  double precision                    :: yeye0, yeye1, yeye, yc
!  double precision                    :: spwr, spwd, spwp
!  double precision                    :: spwdrad  , spwdphi  , spwdradm
!  double precision                    :: spwrad   , spwphi
!  double precision                    :: spwradhat, spwphihat
!  double precision                    :: spwr1, spwr2, spwr3, spwr4, spwrA, spwrB
!  double precision                    :: spwd1, spwd2, spwd3, spwd4, spwdA, spwdB
!  double precision                    :: spwp1, spwp2, spwp3, spwp4, spwpA, spwpB
!  double precision                    :: rintp, dintp, uintp, vintp, pintp
!  double precision                    :: wrad , wphi
!  double precision                    :: spwlon, spwlat, dlat, dlon, h1, h2, fa, fi
!  double precision                    :: earthrad
!  integer                             :: mf, nf
!
!  type(telementset),   pointer :: elementsets(:)   ! bevat roosters zowel van vraagkant als aanbodkant
!  type(tdataprovider), pointer :: dataproviders(:) ! bevat het aanbod
!  type(tquantity),     pointer :: quantities(:)    ! bevat de vraag
!
!                                                   ! en meer specifiek:
!  type(telementset),   pointer :: elsetp           ! elementset provider / aanbodkant
!  type(telementset),   pointer :: elsetq           ! elementset quantity / vraagkant
!  type(tquantity),     pointer :: quantity
!  type(tdataprovider), pointer :: dataprovider
!
!  
!  
!  k = index(qid,'multiple_uni_')
!  if (k > 0) then 
!     success = updatetimespaceproviders(idom, qid(14:), time)
!  endif
!   
!  success = updatetimespaceproviders(idom, qid, time)
!  
!  dataproviders => subdoms(idom)%dataproviders
!  elementsets   => subdoms(idom)%elementsets
!  quantities    => subdoms(idom)%quantities
!
!  success = .false.
!  num = 0
!  do k = 1,size(quantities)
!     if (trim(qid) .eq. trim(quantities(k)%qid) ) num = k
!  enddo
!  if (num == 0) then
!     errormessage = 'first initialise quantity using addrelation(idom, qid, kx, x, y, kcs, filename, filetype, method) '
!     success = .false.
!     return
!  endif
!
!  quantity => quantities(num)
!  kx       = quantity%kx
!  ielset   = quantity%ielset
!  elsetq   => elementsets(ielset)
!  nmx      = size(elsetq%x)
!
!
!  ierr     = 0
!
!
!  do i = 1, size(quantity%providernrs)            ! loop over all relevant providers
!     j = quantity%providernrs(i)
!     dataprovider => dataproviders(j)
!     mx       = dataprovider%mx
!     nx       = dataprovider%nx
!     kx       = dataprovider%kx
!     it0      = dataprovider%it0
!     it1      = dataprovider%it1
!     t0       = dataprovider%field(it0)%time
!     t1       = dataprovider%field(it1)%time
!     dmiss    = dataprovider%dmiss
!     method   = dataprovider%method
!     operand  = dataprovider%operand
!     filetype = dataprovider%filetype
!     ielset   = dataprovider%field(it0)%ielset
!     elsetp   => elementsets(ielset)
!     
!     if (method == spaceandtimeint) then 
!        a0 = 0d0
!        a1 = 1d0
!     else
!        if (t1 == t0) then
!           a1 = 1d0
!        else
!           a1 = (time - t0)/ (t1-t0)
!        endif
!        a0 = 1d0 - a1
!     endif
!
!
!     if (method == spacefirst .or. method == weightfactors) then
!
!        select case (filetype)
!        
!        case ( multiple_uni) ! alleen voor polytims die uitsluitend cmpsets bevatten, op alle steunpunten
!        
!          w1  => dataprovider%field(it1)%arr3d
!          do n = 1,nmx
!             if (elsetq%kcs(n) /= 0 ) then
!                do k  = 1,kx
!                   rr = w1(n,1,k)
!                   if (rr .ne. dmiss) then 
!                      call operate(z(kx*(n-1)+k), rr , operand)
!                   endif
!                enddo
!             endif
!          enddo
!        
!        case ( arcinfo )
!
!            v1  => dataprovider%field(it1)%arr2d
!            v0  => dataprovider%field(it0)%arr2d
!
!            ! spatial coordinates
!            x01 =  elsetp%x(1)
!            y01 =  elsetp%y(1)
!            dx1 =  elsetp%x(2)
!            dy1 =  elsetp%y(2)
!
!           if (.not. allocated (arcuv)) then
!                upperindex = (/5,mx,nx/)
!                lowerindex = (/1,1,1/)
!                call realloc(arcuv,upperindex,lowerindex) !4,mx,nx)
!                arcuv = 0
!            endif
!
!            k   = 1  ! up to now, arcinfo only available with vectormax = 1
!            do n = 1,nmx
!
!               if (elsetq%kcs(n) /= 0) then
!
!
!                  x1 = (elsetq%x(n) - x01)/dx1
!                  if (x1 < -0.5d0 .or. x1 .gt. mx - 0.5d0) cycle
!
!                  y1 = (elsetq%y(n) - y01)/dy1
!                  if (y1 < -0.5d0 .or. y1 .gt. nx - 0.5d0) cycle
!
!                  i1  = int(x1 + 1)
!                  i1  = min(mx - 1,max(1,i1))
!                  di1 = x1 + 1 - i1
!
!                  j1  = int(y1 + 1)
!                  j1  = min(nx - 1,max(1,j1))
!                  dj1 = y1 + 1 - j1
!
!                  ! spatial weight factors
!
!                  f(1) = (1-di1)*(1-dj1)
!                  f(2) = (  di1)*(1-dj1)
!                  f(3) = (  di1)*(  dj1)
!                  f(4) = (1-di1)*(  dj1)
!
!                  if (method .ne. spaceandtimeint) then 
!                     u(1) = v0(i1  ,j1  )
!                     u(2) = v0(i1+1,j1  )
!                     u(3) = v0(i1+1,j1+1)
!                     u(4) = v0(i1  ,j1+1)
!                     vv0  = u(1)*f(1) + u(2)*f(2) + u(3)*f(3) + u(4)*f(4)
!                  else
!                     vv0 = 0d0                    
!                  endif                     
!                  v(1) = v1(i1  ,j1  )
!                  v(2) = v1(i1+1,j1  )
!                  v(3) = v1(i1+1,j1+1)
!                  v(4) = v1(i1  ,j1+1)
!                  vv1  = v(1)*f(1) + v(2)*f(2) + v(3)*f(3) + v(4)*f(4)
!                  rr   = a0*vv0 + a1*vv1
!                  call operate(z(kx*(n-1)+k), rr , operand)
!               endif
!            enddo
!
!            do i1 = 1,mx
!               do j1 = 1,nx
!
!                  arcuv(1,i1,j1) = x01 + (i1-1)*dx1
!                  arcuv(2,i1,j1) = y01 + (j1-1)*dy1
!                  if      (qid == 'windx') then
!                      arcuv(3,i1,j1) = a0*v0(i1,j1) + a1*v1(i1,j1)
!                  else if (qid == 'windy') then
!                      arcuv(4,i1,j1) = a0*v0(i1,j1) + a1*v1(i1,j1)
!                  else if (qid == 'atmospheric_pressure') then
!                      arcuv(5,i1,j1) = a0*v0(i1,j1) + a1*v1(i1,j1)
!                  else if (qid == 'rainfall') then
!                      arcuv(3,i1,j1) = a0*v0(i1,j1) + a1*v1(i1,j1)
!                  endif
!
!               enddo
!            enddo
!            
!        case default 
!        
!          w1  => dataprovider%field(it1)%arr3d
!          w0  => dataprovider%field(it0)%arr3d
!
!          do n = 1,nmx
!             if (elsetq%kcs(n) /= 0) then
!                do k = 1,kx
!                   if ((w0(n,1,k) .ne. dmiss .or. a0.eq.0d0 ) .and. (w1(n,1,k) .ne. dmiss .or. a1.eq.0d0) ) then
!                      rr = a0*w0(n,1,k) + a1*w1(n,1,k)
!                      call operate(z(kx*(n-1)+k), rr , operand)
!                   endif
!                enddo
!             endif
!          enddo
!        
!        end select  
!
!     else if (method == spaceandtime .or. method == spaceandtimeint) then
!
!        select case (filetype)
!
!        case ( uniform )
!
!           u1  => dataprovider%field(it1)%arr1d
!           u0  => dataprovider%field(it0)%arr1d
!
!           do n = 1,nmx
!              if (elsetq%kcs(n) /= 0) then
!                 do k = 1,kx
!                    rr = a0*u0(k) + a1*u1(k)
!                    call operate(z(kx*(n-1)+k), rr , operand)
!                 enddo
!              endif
!           enddo
!
!        case ( unimagdir )
!
!            ! arr1d contains wind magnitude and direction in index 1 and 2
!            ! first interpolate in time, then convert to u/v
!
!            u1  => dataprovider%field(it1)%arr1d
!            u0  => dataprovider%field(it0)%arr1d
!
!            call magdir2uv(u0,u1,a0,a1,uv)
!
!            do k = 3,kx   ! if there are other parameters after mag, dir, < 38
!               uv(k) = a0*u0(k) + a1*u1(k)
!            enddo
!
!            do n = 1,nmx
!               if (elsetq%kcs(n) /= 0) then
!                  do k = 1,kx
!                     call operate(z(kx*(n-1)+k), uv(k) , operand)
!                  enddo
!               endif
!            enddo
!
!        case ( ncgrid )
!
!            v1  => dataprovider%field(it1)%arr2d
!            v0  => dataprovider%field(it0)%arr2d
!               
!            ! spatial coordinates
!            x01 =  elsetp%x(1)
!            y01 =  elsetp%y(1)
!            dx1 =  elsetp%x(2)
!            dy1 =  elsetp%y(2)
!
!           if (.not. allocated (arcuv)) then
!                upperindex = (/5,mx,nx/)
!                lowerindex = (/1,1,1/)
!                call realloc(arcuv,upperindex,lowerindex) !4,mx,nx)
!                arcuv = 0
!            endif
!
!            k   = 1  ! up to now, arcinfo only available with vectormax = 1
!            do n = 1,nmx
!
!               if (elsetq%kcs(n) /= 0) then
!
!
!                  x1 = (elsetq%x(n) - x01)/dx1
!                  if (x1 < -0.5d0 .or. x1 .gt. mx - 0.5d0) cycle
!
!                  y1 = (elsetq%y(n) - y01)/dy1
!                  if (y1 < -0.5d0 .or. y1 .gt. nx - 0.5d0) cycle
!
!                  i1  = int(x1 + 1)
!                  i1  = min(mx - 1,max(1,i1))
!                  di1 = x1 + 1 - i1
!
!
!                  j1  = int(y1 + 1)
!                  j1  = min(nx - 1,max(1,j1))
!                  dj1 = y1 + 1 - j1
!
!
!                  ! spatial weight factors
!
!                  f(1) = (1-di1)*(1-dj1)
!                  f(2) = (  di1)*(1-dj1)
!                  f(3) = (  di1)*(  dj1)
!                  f(4) = (1-di1)*(  dj1)
!
!                  if (method == spaceandtimeint) then 
!                     vv0 = 0d0                    
!                  else
!                     u(1) = v0(i1  ,j1  )
!                     u(2) = v0(i1+1,j1  )
!                     u(3) = v0(i1+1,j1+1)
!                     u(4) = v0(i1  ,j1+1)
!                     vv0  = u(1)*f(1) + u(2)*f(2) + u(3)*f(3) + u(4)*f(4)
!                  endif                     
!                  v(1) = v1(i1  ,j1  )
!                  v(2) = v1(i1+1,j1  )
!                  v(3) = v1(i1+1,j1+1)
!                  v(4) = v1(i1  ,j1+1)
!                  vv1  = v(1)*f(1) + v(2)*f(2) + v(3)*f(3) + v(4)*f(4)
!                  rr   = a0*vv0 + a1*vv1
!                  call operate(z(kx*(n-1)+k), rr , operand)
!               endif
!            enddo
!
!            do i1 = 1,mx
!               do j1 = 1,nx
!
!                  arcuv(1,i1,j1) = x01 + (i1-1)*dx1
!                  arcuv(2,i1,j1) = y01 + (j1-1)*dy1
!                  if      (qid == 'windx') then
!                      arcuv(3,i1,j1) = a0*v0(i1,j1) + a1*v1(i1,j1)
!                  else if (qid == 'windy') then
!                      arcuv(4,i1,j1) = a0*v0(i1,j1) + a1*v1(i1,j1)
!                  else if (qid == 'atmospheric_pressure') then
!                      arcuv(5,i1,j1) = a0*v0(i1,j1) + a1*v1(i1,j1)
!                  else if (qid == 'rainfall') then
!                      arcuv(3,i1,j1) = a1*v1(i1,j1)
!                  endif
!
!               enddo
!            enddo
!            
!        case ( spiderweb )
!            
!            earthrad = 6378137d0
!            fa       =    pi / 180d0
!            fi       = 180d0 /    pi
!            
!            ! Read the 3d arrays from the dataprovider
!            w0  => dataprovider%field(it0)%arr3d
!            w1  => dataprovider%field(it1)%arr3d
!            
!            ! Read the basic spiderweb grid settings
!            spwdphi     = elsetp%x(2)                                               ! angular increment
!            spwdrad     = elsetp%y(2)                                               ! radial  increment
!            
!            ! Reallocate
!            if (.not. allocated (spw)) then
!
!                upperindex = (/5,mx,nx/)
!                lowerindex = (/1,1,1/)
!                call realloc(spw,upperindex,lowerindex) 
!                spw = 0
!            endif
!            
!            ! Interpolate the data at the eye of the cyclone
!            xeye0       = dataprovider%field(it0)%arr1d(1)
!            yeye0       = dataprovider%field(it0)%arr1d(2)
!            xeye1       = dataprovider%field(it1)%arr1d(1)
!            yeye1       = dataprovider%field(it1)%arr1d(2)
!            xeye        = dataInterpolation(xeye0,xeye1,a0,a1,1)                    ! cyclic interpolation (spheric coord)
!            yeye        = dataInterpolation(yeye0,yeye1,a0,a1,1)                    ! cyclic inteprolation (spheric coord)
!           
!            ! Span the spiderweb grid 
!            ! Note: array 'spw' is only used for drawing purposes
!            if (allocated(spw)) deallocate(spw)
!            allocate(spw(5,mx,nx), stat=ierr)
!            spw(1,:,:)  = xeye
!            spw(2,:,:)  = yeye
!            spw(3,:,:)  = 0d0
!            spw(4,:,:)  = 0d0
!            spw(5,:,:)  = paver
!            do m=1,mx-1
!               do n=2,nx
!                   spwphi        =  (m-1)*spwdphi                                   ! span angle 
!                   spwrad        =  (n-1)*spwdrad                                   ! span radius 
!                   spwlat        =  asin(  sin(yeye*fa)*cos(spwrad/earthrad)                                   + & 
!                                           cos(yeye*fa)*sin(spwrad/earthrad)*cos(spwphi*fa)   )
!                   spwlon        =  atan( (sin(spwphi*fa)*sin(spwrad/earthrad)*cos(yeye*fa))                   / &  
!                                          (cos(spwrad/earthrad)-sin(yeye*fa)*sin(spwlat)    ) ) + xeye*fa
!                   spw(1,m,n)    =  spwlon*fi                                       ! return to degrees
!                   spw(2,m,n)    =  spwlat*fi                                       ! return to degrees
!               enddo
!            enddo
!            spw(1:2,mx,:)        =  spw(1:2,1,:)                                    ! define 360 degree direction
!            
!            ! Interpolate the data between two timesteps and determine wind speed components
!            ! Note: array 'spw' is only used for drawing purposes
!            do m=1,mx-1
!               do n=2,nx
!                   spwr          =  dataInterpolation(w0(m,n,1),w1(m,n,1),a0,a1,0)  ! linear interpolation
!                   spwd          =  dataInterpolation(w0(m,n,2),w1(m,n,2),a0,a1,1)  ! cyclic interpolation
!                   spwp          =  dataInterpolation(w0(m,n,3),w1(m,n,3),a0,a1,0)  ! linear interpolation
!                   spwd          =  90d0 - spwd                                     ! convert from nautical convention to regular
!                   spw(3,m,n)    = -spwr*cos(spwd*fa)                               ! minus sign: wind from N points to S
!                   spw(4,m,n)    = -spwr*sin(spwd*fa)                               ! minus sign: wind from N points to S
!                   spw(5,m,n)    =  paver - spwp                                    ! computes actual pressure (including presdrop)
!               enddo
!            enddo
!            spw(3:5,mx,:)        =  spw(3:5,1,:)                                    ! define 360 degree direction
!            
!            ! Project the spiderweb data on the actual computational grid
!            k  =  1                                                                 ! spiderweb for 1 field (only velocities now!)
!            do n = 1,nmx
!               if (elsetq%kcs(n) /= 0) then
!                  
!                  ! Compute distance to eye and the initial bearing
!                  xc             =  elsetq%x(n)
!                  yc             =  elsetq%y(n)
!                  dlat           =  modulo(yc,360d0) - yeye    
!                  dlon           =  modulo(xc,360d0) - xeye    
!                  h1             =  (sin(dlat/2d0*fa))**2 + cos(yeye*fa)*cos(yc*fa)*(sin(dlon/2d0*fa))**2
!                  h2             =  2d0*atan(sqrt(h1)/sqrt(1d0-h1))
!                  spwradhat      =  earthrad*h2
!                  spwphihat      =  cos(yeye*fa)*sin(yc*fa) - sin(yeye*fa)*cos(yc*fa)*cos(dlon*fa)
!                  if (.not. comparereal(spwphihat, 0d0) == 0) then
!                     spwphihat = atan( sin(dlon*fa)*cos(yc*fa) / (spwphihat) )*fi
!                  else
!                     spwphihat = 0d0
!                  end if
!                  if ((dlat.lt.0d0 .and. dlon.le.0d0 .and. spwphihat.ge.0d0) .or.                       &
!                      (dlat.lt.0d0 .and. dlon.ge.0d0 .and. spwphihat.le.0d0)      )     then
!                      spwphihat  =  spwphihat + 180d0
!                  endif
!                  spwphihat      =  modulo(spwphihat,360d0)
!                  
!                  ! Find the four nearest points in the spiderweb
!                  mf             =  floor(spwphihat/spwdphi) + 1                    ! find orientation in windrose
!                  nf             =  floor(spwradhat/spwdrad) + 1                    ! find orientation on radius
!                  
!                  ! If outside spiderweb or exactly in eye, then set windspeed to zero, else find weightfactors and interpolate
!                  if (nf.ge.nx .or. (dlat.eq.0d0 .and. dlon.eq.0d0)) then
!                      uintp      =  0d0
!                      vintp      =  0d0
!                      pintp      =  paver
!                  else
!                      ! Get data from stencil (mf (+1), nf (+1))
!                      spwr1      =  dataInterpolation(w0(mf  ,nf  ,1),w1(mf  ,nf  ,1),a0,a1,0)   ! linear time interp of magnitude
!                      spwd1      =  dataInterpolation(w0(mf  ,nf  ,2),w1(mf  ,nf  ,2),a0,a1,1)   ! cyclic time interp of direction
!                      spwp1      =  dataInterpolation(w0(mf  ,nf  ,3),w1(mf  ,nf  ,3),a0,a1,0)   ! linear time interp of pressure
!                      spwr2      =  dataInterpolation(w0(mf+1,nf  ,1),w1(mf+1,nf  ,1),a0,a1,0)   ! linear time interp of magnitude
!                      spwd2      =  dataInterpolation(w0(mf+1,nf  ,2),w1(mf+1,nf  ,2),a0,a1,1)   ! cyclic time interp of direction
!                      spwp2      =  dataInterpolation(w0(mf+1,nf  ,3),w1(mf+1,nf  ,3),a0,a1,0)   ! linear time interp of pressure
!                      spwr3      =  dataInterpolation(w0(mf  ,nf+1,1),w1(mf  ,nf+1,1),a0,a1,0)   ! linear time interp of magnitude
!                      spwd3      =  dataInterpolation(w0(mf  ,nf+1,2),w1(mf  ,nf+1,2),a0,a1,1)   ! cyclic time interp of direction
!                      spwp3      =  dataInterpolation(w0(mf  ,nf+1,3),w1(mf  ,nf+1,3),a0,a1,0)   ! linear time interp of pressure
!                      spwr4      =  dataInterpolation(w0(mf+1,nf+1,1),w1(mf+1,nf+1,1),a0,a1,0)   ! linear time interp of magnitude
!                      spwd4      =  dataInterpolation(w0(mf+1,nf+1,2),w1(mf+1,nf+1,2),a0,a1,1)   ! cyclic time interp of direction
!                      spwp4      =  dataInterpolation(w0(mf+1,nf+1,3),w1(mf+1,nf+1,3),a0,a1,0)   ! linear time interp of pressure
!                      
!                      ! Safety at center
!                      if (nf.eq.1) then
!                          spwr1  =  0d0
!                          spwd1  =  spwd3
!                      endif
!                      
!                      ! Interpolate over wind direction
!                      wphi       =  1d0 - (spwphihat - (mf-1)*spwdphi)/(spwdphi)                 ! weightfactor for the direction
!                      wrad       =  1d0 - (spwradhat - (nf-1)*spwdrad)/(spwdrad)                 ! weightfactor for the radius
!                      spwrA      =  dataInterpolation(spwr1,spwr2,wphi,1d0-wphi,0)               ! space interp magnitude (direction)
!                      spwrB      =  dataInterpolation(spwr3,spwr4,wphi,1d0-wphi,0)               ! space interp magnitude (direction)
!                      spwdA      =  dataInterpolation(spwd1,spwd2,wphi,1d0-wphi,1)               ! space interp direction (direction)
!                      spwdB      =  dataInterpolation(spwd3,spwd4,wphi,1d0-wphi,1)               ! space interp direction (direction)
!                      spwpA      =  dataInterpolation(spwp1,spwp2,wphi,1d0-wphi,0)               ! space interp pressure (direction)
!                      spwpB      =  dataInterpolation(spwp3,spwp4,wphi,1d0-wphi,0)               ! space interp pressure (direction)
!                      
!                      ! Interpolate over radial direction                     
!                      rintp      =  dataInterpolation(spwrA,spwrB,wrad,1d0-wrad,0)               ! space interp (radius)
!                      dintp      =  dataInterpolation(spwdA,spwdB,wrad,1d0-wrad,1)               ! space interp (radius)
!                      pintp      =  dataInterpolation(spwpA,spwpB,wrad,1d0-wrad,0)               ! space interp (radius)
!                      
!                      ! Final treatment of the data
!                      dintp      =  90d0 - dintp                                                 ! revert from nautical conventions
!                      dintp      =  modulo(dintp,360d0)                                          ! for debug purposes                                               ! turn to radians
!                      uintp      = -rintp*cos(dintp*fa)                                          ! minus sign: wind from N points to S
!                      vintp      = -rintp*sin(dintp*fa)                                          ! minus sign: wind from N points to S
!                      pintp      =  paver - pintp                                                ! relate pressure drop to actual pressure
!                  endif
!                  
!                  ! Put in common array
!                  call operate(z(kx*(n-1)+k+0), uintp , operand)                                 ! store uintp, later becomes wx
!                  call operate(z(kx*(n-1)+k+1), vintp , operand)                                 ! store vintp, later becomes wy
!                  call operate(z(kx*(n-1)+k+2), pintp , operand)                                 ! store pintp, later becomes patm
!               endif
!            enddo
!        
!        end select
!     endif
!  enddo
!
!  success = .true.
!end function gettimespacevalue
!
!
!
!function gettablelookupvalues(idom, qid, x, z) result(success)
!implicit none
!
!  logical :: success
!
!  ! Global variables 
!  integer                ,intent(in)  :: idom     ! domnr
!  character(*)           ,intent(in)  :: qid      !
!  double precision       ,intent(in)  :: x(:)     ! input vector for this quantity, length should be exactly equal to number of providers for this quantity
!  double precision       ,intent(out) :: z(:)     ! result vector for this quantity
!!  integer                ,intent(inout) :: idx(:) ! result vector for this quantity
!
!  !Local variables
!  integer                             :: k
!  integer                             :: i
!  !
!  integer                             :: kx, nmx, mx, nx, method
!  integer                             :: it1
!  integer                             :: it0
!  integer                             :: ierr, ielset
!  integer                              :: num
!  integer                             :: j
!  integer                             :: n
!  
!  character(len=1)                    :: operand
!  
!  double precision                    :: xt
!  double precision                    :: zt
!  !
!  type(telementset),   pointer :: elementsets(:)   ! bevat roosters zowel van vraagkant als aanbodkant
!  type(tdataprovider), pointer :: dataproviders(:) ! bevat het aanbod
!  type(tquantity),     pointer :: quantities(:)    ! bevat de vraag
!  
!                                                   ! en meer specifiek:
!  type(telementset),   pointer :: elsetp           ! elementset provider / aanbodkant
!  type(telementset),   pointer :: elsetq           ! elementset quantity / vraagkant
!  type(tquantity),     pointer :: quantity
!  type(tdataprovider), pointer :: dataprovider
!  
!  double precision, dimension(:,:), pointer :: table ! 2-dim array
!  double precision, dimension(:,:), pointer :: tablez0dzdx ! 2-dim array bevat z0 waarde en de helling dzdx
!  
!  k = index(qid,'multiple_uni_')
!  if (k > 0) then 
!     success = updatetimespaceproviders(idom, qid(14:), t01ini)
!  endif
!   
!  success = updatetimespaceproviders(idom, qid, 0d0)
!  !
!  dataproviders => subdoms(idom)%dataproviders
!  elementsets   => subdoms(idom)%elementsets
!  quantities    => subdoms(idom)%quantities
!  !
!  success = .false.
!  num = 0
!  do k = 1,size(quantities)
!     if (trim(qid) .eq. trim(quantities(k)%qid) ) num = k
!  enddo
!  if (num == 0) then
!     errormessage = 'first initialise quantity using addrelation(idom, qid, kx, x, y, kcs, filename, filetype, method) '
!     success = .false.
!     return
!  endif
!  !
!  quantity => quantities(num)
!  !
!  do i = 1, size(quantity%providernrs)            ! loop over all relevant providers
!     ! z(i) = lookup in provider j==quantity%providernrs(i), based on x(i)
!     ! implicitly the same order of the providernrs as in L1qhbnd is assumed (i.e. same as reading order)
!     j = quantity%providernrs(i)
!     dataprovider => dataproviders(j)
!     mx       = dataprovider%mx
!     nx       = dataprovider%nx
!     kx       = dataprovider%kx
!     it0      = dataprovider%it0
!     it1      = dataprovider%it1
!!     dmiss    = dataprovider%dmiss
!     method   = dataprovider%method
!     operand  = dataprovider%operand
!     
!     table => dataprovider%field(1)%arr2d ! discharge, waterlevel
!     tablez0dzdx => dataprovider%field(0)%arr2d ! crossing, slope
!     xt = x(i) 
!     
!     ! possibly faster search algorithm can be used for large tables 
!     ! or initial search position can be remembered from the previous timestep. WO
!     !      
!     do n = 1,nx
!        if (xt .lt. table(1,n)) then 
!           exit
!        end if
!     end do
!     if (n .gt. nx) then 
!        z(i) = table(2,nx)         ! issue warning when xt falls outside the range of x(i)?  WO
!     elseif (n .eq. 1) then
!        z(i) = table(2,1)          ! issue warning when xt falls outside the range of x(i)?  WO
!     else
!        z(i) = tablez0dzdx(2,n-1)*xt + tablez0dzdx(1,n-1)
!     end if
!     ! max of i is 1, next one would be the parent, so exit
!     exit
!  enddo
!  !
!  success = .true.
!end function gettablelookupvalues
!
!
!function allocsubdoms(ndoms)  result(success) ! to be called at level that knows the nr of threads ndoms
!! use m_alloc
!  implicit none
!  logical                      :: success
!
!  ! arguments
!  integer,      intent(in)     :: ndoms      ! nr of threads
!
!  ! locals
!  integer                      :: ierr
!
!  success = .false.
!
!  ! call realloc(subdoms, ndoms, stat= ierr )
!  allocate (subdoms(ndoms), stat = ierr)
!
!  if (ierr == 0) then
!     success = .true.
!  else
!     errormessage = 'allocation error timespace subdoms'
!  endif
!
!end function allocsubdoms
!
!
!function deallocsubdoms() result(success)
!  implicit none
!  logical success
!
!  ! Locals
!  integer                      :: i, idom
!
!  type(tdataprovider), pointer :: dataproviders(:)         ! bevat het aanbod
!
!  success = .true. ! prachtig, altijd goed !
!  if (.not. allocated (subdoms) ) then 
!     return 
!  elseif (size(subdoms) == 0) then 
!     return
!  else 
!     do idom = 1,size(subdoms)
!        dataproviders => subdoms(idom)%dataproviders
!
!        do i = 1, size(dataproviders)
!           if (dataproviders(i)%minp > 0) then 
!              call doclose(dataproviders(i)%minp)
!           endif
!        enddo
!     enddo
!
!     deallocate(subdoms)
!!    nullify   (subdoms)
!
!  endif
!
!end function deallocsubdoms
!
!subroutine regulate(w0,w1,a0,a1,w)
!   !
!   ! angular interpolation
!   !
!   implicit none
!   double precision              , intent(in)  :: a0
!   double precision              , intent(in)  :: a1
!   double precision, dimension(4), intent(in)  :: w0
!   double precision, dimension(4), intent(in)  :: w1
!   double precision, dimension(4), intent(out) :: w
!   !
!   ! local
!   !
!   integer :: k
!   !
!   ! body
!   !
!   ! Time interpolation
!   !
!   do k = 1,4
!     call regdir(w0(k),w1(k))
!     w(k) = a0*w0(k) + a1*w1(k)
!   enddo
!   !
!   ! The four surrounding points
!   !
!   call regdir(w(4),w(3))
!   call regdir(w(1),w(2))
!   call regdir(w(2),w(3))
!   call regdir(w(1),w(4))
!   call regdir(w(2),w(4))
!   call regdir(w(1),w(3))
!end subroutine regulate
!
!subroutine regdir(w0,w1)
!   !
!   ! angle regularisation
!   !
!   implicit none
!   double precision :: w0
!   double precision :: w1
!   if      ( (w1 - w0) > 180d0) then
!      w0 = w0 + 360d0
!   else if ( (w0 - w1) > 180d0) then
!      w1 = w1 + 360d0
!   endif
!
!!    if (w0>270. .and. w1<90.) then
!!       w1 = w1 + 360.
!!    elseif (w1>270. .and. w0<90.) then
!!       w0 = w0 + 360.
!!    endif
!
!end subroutine regdir
!
!subroutine polyint( xs, ys, zs ,kcs, ns,            &   ! interpolate in a polyline like way
!                    x , y  ,z  ,kc , kx , mnx, jintp, xyen, indxn, wfn)
!
!    implicit none
!
!
!    ! Global variables
!    integer, intent(in)                               :: ns       !< Dimension of polygon OR LINE BOUNDARY
!    double precision, dimension(:),      intent(in)   :: xs       !< polyline point coordinates
!    double precision, dimension(:),      intent(in)   :: ys
!    double precision, dimension(:),      intent(in)   :: zs       !< Values at all points. Dimension: ns*kx
!    integer, dimension(:),               intent(in)   :: kcs      !< polyline mask
!
!    integer,                              intent(in)  :: mnx      !< Dimension of target points
!    integer,                              intent(in)  :: kx       !< #values at each point (vectormax)
!    double precision, dimension(:),       intent(in)  :: x        !< Grid points (where to interpolate to)
!    double precision, dimension(:),       intent(in)  :: y
!    double precision, dimension(kx*mnx),  intent(out) :: z        !< Output array for interpolated values. Dimension: mnx*kx
!    integer , dimension(:),               intent(in)  :: kc       !< Target (grid) points mask
!    integer,                              intent(in)  :: jintp    !< (Re-)interpolate if 1 (otherwise use index weights)
!
!    double precision, dimension(:,:),     intent(in)  :: xyen     !< cellsize / tol
!    integer,          dimension(:,:),     intent(inout), optional :: indxn   !< pli segment is identified by its first node nr.
!    double precision, dimension(:,:),     intent(inout), optional :: wfn     !< If present, get weight index and factor
!
!    ! locals
!
!    double precision:: wL, wR
!    integer :: m, k, kL, kR, jgetw
!
!    jgetw = 0                                            ! niets met gewichten, doe interpolatie
!    if ( present(indxn) .and. jintp .eq. 1) jgetw = 1    ! haal gewichten       doe interpolatie , gebruik gewichten
!    if ( present(indxn) .and. jintp .eq. 0) jgetw = 2    !                      doe interpolatie , gebruik gewichten
!
!    do m = 1, mnx
!
!        if (jgetw .le. 1) then
!            !call polyindexweight( x(m), y(m), xs, ys, kcs, ns, xyen(:,m), k1, rl)    ! interpolate in a polyline like way
!            call polyindexweight( x(m), y(m), xyen(1,m), xyen(2,m), xs, ys, kcs, ns, kL, wL, kR, wR)    ! interpolate in a polyline like way
!            !call findtri_indices_weights (x(n),y( n), xs, ys, ns, zp, indxp)     ! zoeken bij 0 en 1
!            if (jgetw .eq. 1) then                                              ! zetten bij 1
!                indxn(1,m) = kL
!                wfn(1,m)   = wL
!                indxn(2,m) = kR
!                wfn(2,m)   = wR
!            endif
!        elseif (jgetw .eq. 2) then                                              ! halen bij 2, je hoeft niet te zoeken
!            kL = indxn(1,m)
!            wL = wfn(1,m)
!            kR = indxn(2,m)
!            wR = wfn(2,m)
!        endif
!
!        ! Now do the actual interpolation of data zs -> z
!        if (kL > 0) then
!            if (kR > 0) then
!                do k = 1,kx
!                    z(kx*(m-1)+k) = wL*zs(kx*(kL-1)+k) + wR*zs(kx*(kR-1)+k)
!                end do
!            else ! Just left point
!                do k = 1,kx
!                    z(kx*(m-1)+k) = wL*zs(kx*(kL-1)+k)
!                end do
!            end if
!        else if (kR > 0) then
!            do k = 1,kx
!                z(kx*(m-1)+k) =                          wR*zs(kx*(kR-1)+k)
!            end do
!        endif
!    enddo
!
!                    end subroutine polyint
!                    
! SUBROUTINE LINEDISq(X3,Y3,X1,Y1,X2,Y2,JA,DIS,XN,YN,rl) ! = dlinesdis2
! integer          :: ja
! DOUBLE PRECISION :: X1,Y1,X2,Y2,X3,Y3,DIS,XN,YN
! DOUBLE PRECISION :: R2,RL,X21,Y21,X31,Y31,getdx,getdy,dbdistance
! ! korste afstand tot lijnelement tussen eindpunten
! JA  = 0
! !X21 = getdx(x1,y1,x2,y2)
! !Y21 = getdy(x1,y1,x2,y2)
! call getdxdy(x1,y1,x2,y2,x21,y21)  
! !X31 = getdx(x1,y1,x3,y3)
! !Y31 = getdy(x1,y1,x3,y3)
! call getdxdy(x1,y1,x3,y3,x31,y31)
! R2  = dbdistance(x2,y2,x1,y1)
! R2  = R2*R2
! IF (R2 .NE. 0) THEN
!    RL  = (X31*X21 + Y31*Y21) / R2
!    IF (0d0 .LE. RL .AND. RL .LE. 1d0) then
!       JA = 1
!    endif
!    XN  = X1 + RL*(x2-x1)
!    YN  = Y1 + RL*(y2-y1)
!    DIS = dbdistance(x3,y3,xn,yn)
! ENDIF
! RETURN
! END subroutine LINEDISq
! 
! function numbersonline(rec)
!!!--description-----------------------------------------------------------------
!! NONE
!!!--pseudo code and references--------------------------------------------------
!! NONE
!!!--declarations----------------------------------------------------------------
!    implicit none
!!
!! Global variables
!!
!    integer         :: numbersonline
!    character(*)    :: rec
!!
!!
!! Local variables
!!
!    integer                        :: i
!    integer                        :: istarti
!    integer                        :: leeg
!    integer                        :: lend
!!
!!
!!! executable statements -------------------------------------------------------
!!
!    !
!    numbersonline = 0
!    leeg = 1
!    lend = len_trim(rec)
!    do i = 1, lend
!       if (index(rec(i:i), ' ')==0) then
!          !           hier staat iets
!          if (leeg==1) then
!             leeg = 0
!             istarti = i
!             numbersonline = numbersonline + 1
!          endif
!       else
!          leeg = 1
!       endif
!    enddo
! end function numbersonline
! 
! !> Read Space Varying Wind
!function reaspv(minp,d,mx,nx,kx,tread) result(success)
!   integer                    :: minp
!   integer                    :: mx
!   integer                    :: nx
!   integer                    :: kx
!   integer                    :: i
!   integer                    :: j
!   double precision           :: tread
!   double precision, dimension(:,:,:):: d
!   logical                    :: success
!   character(132)             :: rec
!   !
!   if ( size(d,1) .ne. mx .or. size(d,2) .ne. nx .or. size(d,3) .ne. kx ) then
!      errormessage = 'REASPV: wrong sizes'
!      success = .false.
!      return
!   endif
!   read (minp,'(a)',end=100) rec
!   read(rec,*,err=101) tread
!   !
!   ! Loop over the first dimension in flow
!   !
!   do j = 1,mx
!      read(minp,*,end = 100, err=102) ( d(j,i,1), i = 1,nx )
!   enddo
!   do j = 1,mx
!      read(minp,*,end = 100, err=103) ( d(j,i,2), i = 1,nx )
!   enddo
!   do j = 1,mx
!      read(minp,*,end = 100, err=104) ( d(j,i,3), i = 1,nx )
!   enddo
!   success = .true.
!   return
!100 continue
!   errormessage = 'Unexpected end of file in space varying wind file'
!   success = .false.
!   return
!101 continue
!   write(errormessage,'(2a)') 'Error reading time in space varying wind file : ', trim(rec)
!   success = .false.
!   return
!102 continue
!   errormessage = 'Error reading wind u-field'
!   success = .false.
!   return
!103 continue
!   errormessage = 'Error reading wind v-field'
!   success = .false.
!   return
!104 continue
!   errormessage = 'Error reading atmospheric pressure field'
!   success = .false.
!   return
!end function reaspv
!
!!> Reads header data for a NetCDF grid.
!!! NOTE: currently assumes uniform rectilinear grids.
!function read_nc_header(ncid, mx        ,nx        , x0, y0, dxa        ,dya) result(success)
!    use netcdf
!    use unstruc_messages
!!
!    integer, intent(in)   :: ncid
!    integer               :: mx
!    integer               :: nx
!    double precision, intent(out) :: x0
!    double precision, intent(out) :: y0
!    double precision, intent(out) :: dxa
!    double precision, intent(out) :: dya
!    logical               :: success
!    
!    integer                        :: ierr, id_varx, id_vary, ndims
!    integer, dimension(1)          :: idimtmp
!    double precision, dimension(2) :: xytmp
!    
!    success = .true.
!
!    !! x variable ! TODO: AvD: replace this by standar_name checking, and coordinates attribute from data variable.
!    ierr = nf90_inq_varid(ncid, 'x', id_varx)
!    if (ierr /= nf90_noerr) then
!        success = .false.
!        write (msgbuf, *) 'read_nc_header: could not find x variable in file #', ncid, '. Code: ', ierr
!        call warn_flush()
!        return
!    end if
!    
!    ierr = nf90_inquire_variable(ncid, id_varx, ndims = ndims)
!    if (ierr /= nf90_noerr .or. ndims /= 1) then
!        success = .false.
!        call mess(LEVEL_WARN, 'read_nc_header: Variable x should be of rank 1. Code: ', ierr)
!        return
!    else
!        ierr = nf90_inquire_variable(ncid, id_varx, dimids = idimtmp)
!    end if
!
!    ierr = nf90_inquire_dimension(ncid, idimtmp(1), len = mx)
!
!    !! y variable
!    ierr = nf90_inq_varid(ncid, 'y', id_vary)
!
!    ierr = nf90_inquire_variable(ncid, id_vary, ndims = ndims)
!    if (ierr /= nf90_noerr .or. ndims /= 1) then
!        success = .false.
!        call mess(LEVEL_WARN, 'read_nc_header: Variable y should be of rank 1. Code: ', ierr)
!        return
!    else
!        ierr = nf90_inquire_variable(ncid, id_vary, dimids = idimtmp)
!    end if
!
!    ierr = nf90_inquire_dimension(ncid, idimtmp(1), len = nx)
!
!    ! Get delta x: assume uniform rectilinear grid.
!    ierr = nf90_get_var(ncid, id_varx, xytmp, start = (/ 1 /), count = (/ 2 /))
!    if (ierr /= nf90_noerr) then
!        success = .false.
!        call mess(LEVEL_WARN, 'read_nc_header: Could not read coordinate data from x variable. Code: ', ierr)
!    else
!        x0 = xytmp(1)
!        dxa = xytmp(2) - xytmp(1)
!    end if
!
!    ! Get delta y: assume uniform rectilinear grid.
!    ierr = nf90_get_var(ncid, id_vary, xytmp, start = (/ 1 /), count = (/ 2 /))
!    if (ierr /= nf90_noerr) then
!        success = .false.
!        call mess(LEVEL_WARN, 'read_nc_header: Could not read coordinate data from y variable. Code: ', ierr)
!    else
!        y0 = xytmp(1)
!        dya = xytmp(2) - xytmp(1)
!    end if
!
!end function read_nc_header
!
!!> Reads field data for a NetCDF grid.
!!! NOTE: currently assumes uniform rectilinear grids.
!function read_nc_field(ncid, variable, d,mx,nx,tread, dmiss) result(success)
!   use netcdf
!   use m_itdate
!   !
!   integer,          intent(in)   :: ncid
!   character(len=*), intent(in)   :: variable
!   integer,          intent(in)   :: mx
!   integer,          intent(in)   :: nx
!   double precision, intent(out)  :: tread
!   double precision, dimension(:,:), intent(out) :: d
!   real, allocatable              :: rd(:,:) 
!   double precision, intent(out)  :: dmiss
!   logical                        :: success
!   character(len=132)             :: rec
!   integer, save                  :: it_last = 0
!   integer                        :: ierr, id_time, id_fld
!   integer                        :: iyear, imonth, iday, ihour, imin, isec, julprovider
!   double precision, dimension(1) :: tmp
!   double precision               :: tfact, tnext, timdiff
!   integer                        :: iunit, idum, i, j
!   integer                        :: kx
!   
!   integer, external              :: julday
!   
!   allocate ( rd(mx,nx) )
!   
!   !
!   ! TODO: AvD: what to do with rewind/reset of it_last? AND!!! multiple nc files
!    success = .true.
! !if ( size(d,1) .ne. mx .or. size(d,2) .ne. nx .or. size(d,3) .ne. kx ) then
!
!   if ( size(d,1) .ne. mx .or. size(d,2) .ne. nx) then
!      errormessage = 'read_nc_field: wrong sizes'
!      success = .false.
!      return
!   endif
!
!   it_last = it_last+1
!   ierr = nf90_inq_varid(ncid, 'time', id_time)
!   ierr = nf90_get_var(ncid, id_time, tmp, start = (/ it_last /), count = (/ 1 /))
!   if (ierr /= nf90_noerr) then
!      it_last = it_last-1
!
!      errormessage = 'nc_read_field: Could not read next time from file.'
!      success = .false.
!      return
!   else
!      tread = tmp(1)
!      ierr = nf90_get_var(ncid, id_time, tmp, start = (/ it_last+1 /), count = (/ 1 /))
!      if (ierr /= nf90_noerr) then
!         errormessage = 'nc_read_field: Could not read end time for current time window.'
!         success = .false.
!         return
!      else
!         tnext = tmp(1)
!      end if
!   end if
!
!   ierr = nf90_inq_varid(ncid, 'Rainfall', id_fld) ! TODO: AvD: handle standard names and use requested input variable.
!   if (ierr /= nf90_noerr) then
!      write(errormessage,*) 'nc_read_field: Could not find variable for ', variable, ' in file. Code: ', ierr
!      success = .false.
!   end if
!   
!   ierr = nf90_get_var(ncid, id_fld, rd, start = (/ 1, 1, it_last /))
!   if (ierr /= nf90_noerr) then
!      write(errormessage,*) 'nc_read_field: Could not read field data from file for next time #', it_last, '. Code: ', ierr
!      success = .false.
!   ELSE 
!      IERR = 0
!   end if
!
!   ierr = nf90_get_att(ncid, id_time, 'units', rec)
!   if (ierr /= nf90_noerr) then
!      errormessage = 'nc_read_field: Could not read ''units'' attribute for time variable.'
!      success = .false.
!      return
!   else
!      ierr        = parse_ud_timeunit(trim(rec), iunit,  iyear, imonth, iday, ihour, imin, isec)
!      julprovider = julday(imonth,iday,iyear)
!      !refdatetime
!   end if
!
!   ! Convert to amount per minute
!   tfact = (dble(iunit)/(tnext - tread))/60d0
!   do j=1,nx
!      do i=1,mx
!         d(i,j) = tfact*rd(i,j)
!      end do
!   end do
!
!   ierr = nf90_get_att(ncid, id_fld, '_FillValue', dmiss)
!   if (ierr /= nf90_noerr) then
!      dmiss = -999d0
!   end if
!   
!   tread   = (dble(iunit)/60d0)*tread   ! Convert to minutes (since refdatetime)
!
!   timdiff = 60*24*(jul0 - julprovider)  ! time difference in minutes between flow refdat and meteo provider refdattime
!
!   tread   = tread - timdiff + Tzone*60
!   
!   ! TODO: AvD: setting of refdatetime and tunit should move to adddataprovider.
!   ! if (filetype == ncgrid) then
!   ! dataproviders(nump)%julrefdatetime = dble(ymd2jul(1970, 1, 1)) ! temp. hardcoded for FEWS
!   ! dataproviders(nump)%tunit = tunit
!   ! end if
!   
!   deallocate (rd)
!end function read_nc_field
!end module unused_code_from_meteo1
!
