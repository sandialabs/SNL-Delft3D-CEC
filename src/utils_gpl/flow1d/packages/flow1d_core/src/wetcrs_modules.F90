module M_newcross                                                ! new type conveyance table crossections
                                                                 ! all data is attached to pluv u nr
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
!  $Id: wetcrs_modules.F90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/wetcrs_modules.F90 $
!-------------------------------------------------------------------------------

implicit none

private

public CalcConveyanceTables
public ConveyanceTables
public generateConvtab
public regulatehlv
public realloc
public dealloc
public write_conv_tab

 integer, parameter                             :: ncx = 144
 integer, parameter, public                             :: CS_LUMPED    = 0 !< Lumped option for yz-type conveyance calculation.
 integer, parameter, public                             :: CS_VERT_SEGM = 1 !< Vertically segmented option for yz-type conveyance calculation.
 integer, public     :: lcnvmax
 
interface dealloc
   module procedure deallocCru
   module procedure deallocCruArr
end interface dealloc


interface realloc
   module procedure reallocCru
end interface

type, public :: t_crsu
   integer                                         :: jopen         !< open/closed profile, 1/0
   integer                                         :: msec          !< number of friction sections (horreur only for postpro)
   integer                                         :: nru           !< number of levels in u tables
   integer                                         :: iolu   = 1    !< latest level found, initialise at bottom
   integer                                         :: negcon = 0    !< different conveyance for negative flow? 0/1
   integer                                         :: conveyType = CS_VERT_SEGM    !< calculation type for conveyance (lumped or vertically segmented)
   double precision                                :: a_pos_extr    !< Extrapolation Factor for YZ-Profiles
   double precision                                :: a_neg_extr    !< Extrapolation Factor for YZ-Profiles
   double precision                                :: b_pos_extr    !< Extrapolation Factor for YZ-Profiles
   double precision                                :: b_neg_extr    !< Extrapolation Factor for YZ-Profiles

                                                                  !< first index: mxhu, second index: over msec + 1
   double precision, allocatable                   :: hu (:)      !< heights for next (u) tables:
   double precision, allocatable                   :: af (:)      !< flow area  ft of h, for all sections
   double precision, allocatable                   :: wf (:)      !< flow width (not double precisionly needed)
   double precision, allocatable                   :: pf (:)      !< wet perimeter (only postpro)
   double precision, allocatable                   :: co1(:)      !< conveyance positive flow direction
   double precision, allocatable                   :: co2(:)      !< conveyance negative flow direction
   double precision, allocatable                   :: cz1(:)      !< discharge dependent chezy posflow
   double precision, allocatable                   :: cz2(:)      !< discharge dependent chezy negflow

   double precision                                :: chezy_act   !< Actual Chezy
                                                                  !< here stored for output
                                                                  !< used in function ChezyFromConveyance
   
                                                                    !< last index: 1 or 2 for left/right waterlevel point
   integer                                         :: nrhh(2)       !< number of levels in h table l/r  1,2
   integer                                         :: iolh(2)       !< latest level found    left/right 1,2
   double precision                                :: bob (2)       !< left and right bobs              1,2 (positive down)
                                                                    !< first index: mxhh
   double precision, allocatable                   :: hh(:,:)       !< heights for tables    left/right
   double precision, allocatable                   :: at(:,:)       !< total area            left/right
   double precision, allocatable                   :: wt(:,:)       !< total width           left/right
end type t_crsu

double precision, allocatable, public::    &
         ihlev (:,:),        &
         iwft  (:,:,:),      &
         iaft  (:,:,:),      &
         ipft  (:,:,:),      &
         ikpt  (:,:,:),      &
         iknt  (:,:,:),      &
         ifcp  (:,:,:),      &
         ifcn  (:,:,:),      &
         iwtt  (:,:),        &
         iatt  (:,:),        &
         crsn_lc(:)
integer, allocatable, save, public :: cvip_cp(:,:)

integer, public                                        :: nupt
integer, public                                        :: msect

contains

subroutine reallocCru(arr, newLength, stat)
   ! Modules

   implicit none
   ! Input/output parameters
   type(t_crsu), allocatable, intent(inout)     :: arr(:)
   integer, intent(in)                          :: newLength
   integer, optional                            :: stat
   
   ! Local variables
   integer                                      :: length
   integer                                      :: localErr = 0

   ! Program code
   
   if (allocated(arr)) then
      length = size(arr)
      if (length /= newLength) then
         call dealloc(arr)
         allocate(arr(newLength), stat = localErr)
      endif
   else
      allocate(arr(newLength), stat = localErr)
   endif
   if (present(stat)) stat = localErr
end subroutine

subroutine deallocCruArr(arr)
   ! Modules
   
   implicit none
   
   ! Input/output parameters
   type(t_crsu), allocatable, intent(inout)     :: arr(:)
   
   ! Local variables
   integer length, i

   ! Program code
   
   if (allocated(arr)) then
      length = size(arr)
      do i = 1, length
         if (allocated(arr(i)%af )) deallocate(arr(i)%af )
         if (allocated(arr(i)%wf )) deallocate(arr(i)%wf )
         if (allocated(arr(i)%pf )) deallocate(arr(i)%pf )
         if (allocated(arr(i)%co1)) deallocate(arr(i)%co1)
         if (allocated(arr(i)%co2)) deallocate(arr(i)%co2)
         if (allocated(arr(i)%cz1)) deallocate(arr(i)%cz1)
         if (allocated(arr(i)%cz2)) deallocate(arr(i)%cz2)
         if (allocated(arr(i)%hh )) deallocate(arr(i)%hh )
         if (allocated(arr(i)%hu )) deallocate(arr(i)%hu )
         if (allocated(arr(i)%at )) deallocate(arr(i)%at )
         if (allocated(arr(i)%wt )) deallocate(arr(i)%wt )
      enddo
      deallocate(arr)
   endif
end subroutine deallocCruArr

subroutine deallocCru(cru)
   ! Modules
   
   implicit none
   
   ! Input/output parameters
   type(t_crsu), pointer, intent(inout)     :: cru
   
   ! Local variables

   ! Program code
   if (associated(cru)) then
      if (allocated(cru%af )) deallocate(cru%af )
      if (allocated(cru%wf )) deallocate(cru%wf )
      if (allocated(cru%pf )) deallocate(cru%pf )
      if (allocated(cru%co1)) deallocate(cru%co1)
      if (allocated(cru%co2)) deallocate(cru%co2)
      if (allocated(cru%cz1)) deallocate(cru%cz1)
      if (allocated(cru%cz2)) deallocate(cru%cz2)
      if (allocated(cru%hh )) deallocate(cru%hu )
      if (allocated(cru%hu )) deallocate(cru%hh )
      if (allocated(cru%at )) deallocate(cru%at )
      if (allocated(cru%wt )) deallocate(cru%wt )
      deallocate(cru)
   endif
      
end subroutine deallocCru

subroutine generateConvtab(convtab, levelsCount, href, grndlyr, typ, &
                           nc, nbo, branchid, bedFrictionType, groundFriction, &
                           yin, z, frictionSectionFrom, frictionSectionTo, frictionTypePos, &
                           frictionValuePos, frictionTypeNeg, frictionValueNeg)

   implicit none
   
   type (t_crsu), pointer, intent(inout) :: convtab
   integer, intent(in)        :: levelsCount
   double precision           :: href
   double precision           :: grndlyr
   double precision           :: yin(:)
   double precision           :: z(:)
   double precision           :: groundfriction
   integer                    :: typ
   integer                    :: nbo
   integer                    :: nc
   integer                    :: branchid
   integer                    :: bedfrictiontype
   double precision           :: frictionSectionFrom(:)
   double precision           :: frictionSectionTo(:)
   integer                    :: frictionTypePos(:)
   double precision           :: frictionValuePos(:)
   integer                    :: frictionTypeNeg(:)
   double precision           :: frictionValueNeg(:)
   
   integer      ncolmn, msect1
   parameter   (ncolmn=3, msect1 = 1)

   double precision, allocatable :: vf(:), vg(:)
   double precision, allocatable :: y(:), d(:), hlv(:)
   integer, allocatable          :: jf(:), jg(:)
   integer, allocatable          :: indx  (:)
   integer                       :: n12, jgetlevels, nnlev, ierr

   ! Variables for calculating extrapolation coefficients:
   integer              :: i1 ! one but last index
   integer              :: i2 ! last index
   double precision     :: h_1
   double precision     :: h_2
   double precision     :: K_1
   double precision     :: K_2

   double precision ::  zminprof(3) ! absolute z level lowest profile point  
!
   if (.not. associated(convtab)) then
      allocate(convtab)
   endif

   lcnvmax = max(400, levelsCount * 4)
  
   allocate (ihlev(ncolmn,lcnvmax) ,      &
             iwft(ncolmn,lcnvmax,msect1) ,   &
             iaft(ncolmn,lcnvmax,msect1) ,   &
             ipft(ncolmn,lcnvmax,msect1) ,   &
             ifcp(ncolmn,lcnvmax,msect1) ,   &
             ifcn(ncolmn,lcnvmax,msect1) ,   &
             ikpt(ncolmn,lcnvmax,msect1) ,   &
             iknt(ncolmn,lcnvmax,msect1) ,   &
             iwtt(ncolmn,lcnvmax) ,    &
             iatt(ncolmn,lcnvmax) ,    &
             indx(lcnvmax) , stat=ierr )
   
   allocate (vf(lcnvmax),  &
             vg(lcnvmax),  &
             y(lcnvmax),   &
             d(lcnvmax*2), &
             jf(lcnvmax),  &
             jg(lcnvmax),  &
             hlv(lcnvmax*2), &
             stat=ierr )

   iwft = 0 !-9
   iaft = 0 !-9
   ipft = 0 !-9
   ifcp = 0 !-9
   ifcn = 0 !-9
   ikpt = 0 !-9
   iknt = 0 !-9
   iwtt = 0 !-9
   iatt = 0 !-9
   indx = 9 !-9
   zminprof = 0

   n12 = 1
   nnlev = 2
   jgetlevels = 1

   call ConveyanceTables(href, grndlyr, typ, yin, z, nbo, branchid, bedFrictionType,   &
                         groundFriction, frictionSectionFrom,                          &
                         frictionSectionTo, frictionTypePos, frictionValuePos,         &
                         frictionTypeNeg, frictionValueNeg, jf, vf, y, d, hlv,         &
                         nc, n12, jgetlevels, zminprof(n12), nnlev)

   convTab%jopen  = 1
   convTab%msec   = 1 
   convTab%nru    = nnlev   
   convTab%iolu   = 1
   convTab%negcon = 0
   
   
   allocate(convTab%hu (nnlev)    )
   allocate(convTab%af (nnlev)  )
   allocate(convTab%wf (nnlev)  )
   allocate(convTab%pf (nnlev)  )
   allocate(convTab%co1(nnlev)  )
   allocate(convTab%co2(nnlev)  )
   allocate(convTab%cz1(nnlev)  )
   allocate(convTab%cz2(nnlev)  )
   allocate(convTab%hh(nnlev,2)   )
   allocate(convTab%at(nnlev,2)   )
   allocate(convTab%wt(nnlev,2)   )
   
   convTab%hu   = hlv(1:nnlev)
   convTab%af   = iaft(1,1:nnlev,1)
   convTab%wf   = iwft(1,1:nnlev,1)
   convTab%pf   = ipft(1,1:nnlev,1)
   convTab%co1  = ikpt(1,1:nnlev,1)
   convTab%co2  = iknt(1,1:nnlev,1)
   convTab%cz1  = ifcp(1,1:nnlev,1)
   convTab%cz2  = ifcn(1,1:nnlev,1)
   convTab%nrhh = nnlev
   convTab%iolh = 1  
   convTab%bob  = -hlv(1) - zminprof(1)
   convTab%hh(:,1)   = hlv(1:nnlev)
   convTab%hh(:,2)   = hlv(1:nnlev)
   convTab%at(:,1)   = iatt(1,1:nnlev)
   convTab%at(:,2)   = iatt(1,1:nnlev)
   convTab%wt(:,1)   = iwtt(1,1:nnlev)
   convTab%wt(:,2)   = iwtt(1,1:nnlev)
   
   ! calculate the coefficients for extrapolation of conveyance above specified profile
  !(*)   !  document SOBEK-21942: Change of roughness formulations in "Y-Z" and
   ! "Asymetrical Trapezium" profiles, Author:     Thieu van Mierlo
   !                                   Programmer: Daniel Abel
   i1  = convTab%nru - 1     ! so i1, i2 always inside table
   i2  = i1 + 1
   !
   h_1 = convTab%hu(i1)
   h_2 = convTab%hu(i2)
   !
   K_1 = convTab%co1(i1)
   K_2 = convTab%co1(i2)
   !
   ! dlog (h1/h2) is almost zero, however, h1 and h2 
   ! always differ enough (h2-h1 ~> 1e-5) s.t. the operation is stable and
   ! accurate
   !
   convTab%b_pos_extr = dlog(K_1/K_2) / (dlog(h_1/h_2))
   convTab%a_pos_extr = K_1*(h_1**(-convTab%b_pos_extr))
   !
   if (convTab%negcon .eq. 1) then
       K_1 = convTab%co2(i1)
       K_2 = convTab%co2(i2)
       !
       convTab%b_neg_extr = dlog(K_1/K_2) / (dlog(h_1/h_2))
       convTab%a_neg_extr = K_1*(h_1**(-convTab%b_neg_extr))
   else
       convTab%b_neg_extr = convTab%b_pos_extr
       convTab%a_neg_extr = convTab%a_pos_extr
   endif
   
   
   deallocate (ihlev ,     &
               iwft  ,     &
               iaft  ,     &
               ipft  ,     &
               ikpt  ,     &
               iknt  ,     &
               ifcp  ,     &
               ifcn  ,     &
               iatt  ,     &
               iwtt  , stat=ierr )
   deallocate( vf, vg, y, d, jf, jg, hlv)

end subroutine generateConvtab

subroutine CalcConveyanceTables(jf, vf, y, d, hlv, nc, n12, jgetlevels, zminpr,                        &
                                grndlyr, nbo, frictionSectionFrom, frictionSectionTo,                  &
                                frictionTypePos, frictionValuePos, frictionTypeNeg, frictionValueNeg,  &
                                cg, nhmax)
    use MessageHandling
    
    implicit none
    
    ! Input output
    integer          jf(lcnvmax), nh, n12, jgetlevels, lu_dump
    integer          nc
    integer          nbo
    
    double precision           ::  vf(lcnvmax)
    double precision           ::  cg(2)
    double precision           :: hlv(lcnvmax*2), y(lcnvmax), d(lcnvmax*2), zminpr
    double precision           :: grndlyr
    double precision           :: frictionSectionFrom(nbo)
    double precision           :: frictionSectionTo(nbo)
    integer                    :: frictionTypePos(nbo)
    double precision           :: frictionValuePos(nbo)
    integer                    :: frictionTypeNeg(nbo)
    double precision           :: frictionValueNeg(nbo)

    ! Local variables

    integer numt, nump, nhi, nhmax
    integer ierr, k, j, ncc, i, io, num
    integer ja, k2, numpunt, k1
    integer nbn
    double precision               :: yya, yyb
    double precision               :: cp(2), cn(2)
    double precision               :: zmin, zh, zl, zr, dy, dz, zground, yy, f
    double precision               :: area, ar1, ar2, a, dda
    double precision               :: w, p, co, at, wt, conv, accuracy, width
    double precision               :: dif1, dif2, hlvz
    double precision               :: sl
    integer, allocatable           :: ik1(:)
    double precision,  allocatable :: yh(:), dh(:)
    ! A small profile might cause an overflow in calculating the extrapolation parameters for the conveyance table
    ! in that case raise the conveyance table in order to create a larger profile
    double precision, parameter :: EXTRA_HEIGHT = 0.5d0
    double precision, parameter :: MINCONV = 1e-3

    data numt /0/, nump /0/

    allocate ( ik1(lcnvmax*2), &
               yh(lcnvmax), &
               dh(lcnvmax*2), stat=ierr )
    if ( ierr .ne. 0 ) goto 9000
    nh=nhmax
    ik1 = 0
    yh = 0.0
    dh = 0.0

    sl = 0.

! compute conveyance (y/z profiles)
    nbo = max(nbo,1)       ! please check

    !hk: nb, d bevindt  zich nu op zijn absolute niveau, is makkelijk, want dan hoeven we daar
    !    later geen rekening meer mee te houden.

    ja  = 1                                ! eerst dubbele punten weghalen
    do while (ja .eq. 1)
      ja = 0
      do k = 2,nc
        if (k .le. nc .and. y(k) .eq. y(k-1) .and. d(k) .eq. d(k-1) ) then
          ja  = 1
          nc  = nc - 1
          do j = k,nc
            y(j) = y(j+1)
            d(j) = d(j+1)
          enddo
        endif
      enddo
    enddo

    ja  = 1                                ! dan middelste y-punten weghalen uit verticale stukken
    do while (ja .eq. 1)
      ja = 0
      do k = 2,nc-1
        if (k .le. nc-1 .and. y(k) .eq. y(k-1) .and. y(k) .eq. y(k+1) ) then
          ja  = 1
          nc  = nc - 1
          do j = k,nc
            y(j) = y(j+1)
            d(j) = d(j+1)
          enddo
        endif
      enddo
    enddo

    ja  = 1                                ! dan middelste d-punten weghalen uit horizontale stukken
    do while (ja .eq. 1)
      ja = 0
      do k = 2,nc-1
        if (k .le. nc-1 .and.  d(k) .eq. d(k-1) .and. d(k) .eq. d(k+1) ) then
          ja  = 1
          nc  = nc - 1
          do j = k,nc
            y(j) = y(j+1)
            d(j) = d(j+1)
          enddo
        endif
      enddo
    enddo

    !hk: hier moet de groundlayer erin gezet worden. moet redelijk netjes gebeuren,
    !    dus niet alleen min/max functies zoals eerst, maar insnijden aan zijkant

    zmin = 1e30
    do k = 1,nc
      zmin = min(zmin,d(k))
    enddo
    if (grndlyr .gt. 0.0d0) then
      zground = zmin + grndlyr !hk: is this o.k. ?
    else
      zground = -1e30
    endif

    zh  = zground
    ncc = nc

    do k = ncc-1,1,-1                   ! hk: eerst punten bijzetten
      zL = d(k)
      zr = d(k+1)
      dz = d(k+1) - d(k)
      dy = y(k+1) - y(k)
      if ( (zl .gt. zh .and. zr .lt. zh) .or. &
           (zl .lt. zh .and. zr .gt. zh) ) then
        f  = abs(zh-zl)/abs(dz)
        yy = y(k) + f*dy
        nc = nc + 1
        do j = nc,k+2,-1
          y(j) = y(j-1)
          d(j) = d(j-1)
        enddo
        y(k+1) = yy
        d(k+1) = zh
      endif
    enddo

    j = 0                               !hk: dan weghalen wat onder zground zit

    do k = 1,nc
      if (d(k) .ge. zh) then
        j = j + 1
        yh(j) = y(k)
        dh(j) = d(k)   !hk: copie
      endif
    enddo

    nc = j

    do k = 1,nc
      y(k) = yh(k)
      d(k) = dh(k)      ! en terugzetten
    enddo

    ! na inzetten grondlaag het level van het laagste profielpunt

    zmin = 1e30

    do k = 1,nc
      zmin = min(zmin,d(k))
    enddo

    ! einde ground layer aanpassingen

    ! start friction secties invoegen op overgangen moeten (eventueel) extra 
    ! steunpunten worden aangemaakt

    zminpr = zmin

    do i = 1, nbo                          !hk: do over the friction sections
      yya   = frictionSectionFrom(i)
      yyb   = frictionSectionTo(i)
      cp(1) = frictionTypePos(i)
      cp(2) = frictionValuePos(i)
      cn(1) = frictionTypeNeg(i)
      cn(2) = frictionValueNeg(i)
      k1 = 0
      do k = 1,nc                            !hk: zoek punt links van yya grens
        if (y(k) .lt. yya) k1 = k
      enddo
      if (k1 .ne. 0 .and. k1 .ne. nc) then   !hk: als nodig bepaal tussenpunt
        a   = (y(k1+1) - yya)/(y(k1+1) - y(k1))
        dda = d(k1)*a + d(k1+1)*(1-a)
        nc  = nc + 1
        do k = nc, k1+2,-1                  !hk: dan opschuiven
          y(k) = y(k-1)
          d(k) = d(k-1)
        enddo                               ! en tussenpunt zetten
        y(k1+1) = yya
        d(k1+1) = dda
      endif
      ik1(i) = k1 + 1                        ! sla startpunt vd sectie op, (index lf,vf gelijk aan index linker segmentpunt))
      do k = k1+1,nc
        ! zet frictietypes
        if (d(k) .eq. zground .and. d(k+1) .eq. zground) then ! grondlaag kan alleen horizontaal zijn
          jf(k) = cg(1)
          vf(k) = cg(2)
        else
          jf(k) = cp(1)
          vf(k) = cp(2)
        endif
      enddo
    enddo

    ! Einde aanpassingen tbv frictie secties

    if (jgetlevels .eq. 1) then ! hier wordt eerste hoogtetabel opgebouwd
      do k = 1,nc
        hlv(k) = d(k) - zminpr     ! hiermee wordt hlv een hoogte tov laagste profielpunt
      enddo                        ! dit spaart een aantal correctieslagen.
                                   ! zminpr is nog steeds abs level laagste profielpunt
      do k = 2,nc
        if ( d(k) .eq. d(k-1) ) then  ! floodplane, til verticaal tabelpunt iets op
          hlv(k) = hlv(k) + 1d-4      ! voor scherper zien van breekpunt in tabel
        endif
      enddo
      nh = nc
      call regulatehlv(hlv,nh)
    endif

    nhi = nh

    iwft(n12,:,:) = 0
    iaft(n12,:,:) = 0
    ipft(n12,:,:) = 0
    ikpt(n12,:,:) = 0
    iknt(n12,:,:) = 0
    iatt(n12,:)   = 0
    iwtt(n12,:)   = 0

    num = 0
    j   = 0
    io  = 0

    ! for the number of levels
    do while (j .lt. nh)
      j  = j + 1
      zh = hlv(j) + zminpr            ! dit zit op absoluut niveau, dus zh ook
      do i = 1, nbo                   !hk: do over the friction sections
        k1 = ik1(i)
        if (i .eq. 1) k1 = 1           !hk: if friction section def starts too late
        if (i .lt. nbo) then
          k2 = min(nc,ik1(i+1))        !hk: never beyond nc
        else
          k2 = nc
        endif
        numpunt = k2 - k1 + 1
        if (numpunt .ge. 2 .and. numpunt .le. nc) then
          if(io.eq.1) write(lu_dump,'(a,2i3,2f10.4)') 'SECTION LOOP: prof,level,nbo,zh ',j,i, zh
          call ConveyYZ(numpunt, y(k1), d(k1), jf(k1), vf(k1), zh, a, at, w, wt, p, co)
          iwft (n12,j,1)   = iwft (n12,j,1) + w
          iaft (n12,j,1)   = iaft (n12,j,1) + a
          ipft (n12,j,1)   = ipft (n12,j,1) + p
          ikpt (n12,j,1)   = ikpt (n12,j,1) + co
          iknt (n12,j,1)   = iknt (n12,j,1) + co
        endif
      enddo

      if (zh > d(1)) then
         ipft(n12,j,1) = ipft(n12,j,1) + zh-d(1)
      endif
      if (zh > d(nc)) then
         ipft(n12,j,1) = ipft(n12,j,1) + zh-d(nc)
      endif

      if(io.eq.1) write(lu_dump,*) 'END SECTION LOOP, co_tot: ', ikpt (n12,j,1)
      io = 0
      call ConveyYZ(nc,y,d,jf,vf,zh,a,at,w,wt,p,co)

      if (abs(co - ikpt (n12,j,1) ) .gt. 0.001*co) then
        co = co
      endif

      iwtt(n12,j) = wt
      iatt(n12,j) = at
      if (j .ge. 2 .and. jgetlevels .eq. 1) then   ! hier kan de hoogtetabel
                                                   ! uitgebreid worden om conveyance interpolatie
                                                   ! voldoende nauwkeurig te krijgen

        ! hk: check if interpolation half-way is sufficiently accurate
        ! by comparing true value and interpolation
        ! (interpolation in the same way as in flow)

        dz = hlv(j) - hlv(j-1)

        if (dz .gt. 3.0d-3) then

          hlvz = ( hlv(j)+hlv(j-1) )*0.5d0
          zh   = hlvz + zminpr

          call ConveyYZ(nc, y, d, jf, vf, zh, a, at, w, wt, p, co)

          width = 0.5d0*iwft (n12,j,1) + 0.5d0*iwft (n12,j-1,1)

          dif1  = dabs(width - w)/(width + w)
          if (dif1 .gt. 0.0001) then
             dif1 = dif1
          endif

          ar1   = 0.25d0*( iwft (n12,j-1,1) + width ) * dz
          ar2   = 0.25d0*( iwft (n12,j,1)   + width ) * dz
          area  = 0.5d0*( iaft (n12,j-1,1) + ar1) + 0.5d0*( iaft (n12,j,1) - ar2)

          dif2  = abs(area - a)/(area+a)
          if (dif2 .gt. 0.0001) then
            dif2 = dif2
          endif

          conv = 0.5d0*ikpt(n12,j,1) + 0.5d0*ikpt(n12,j-1,1)
          co = max(co,1d-6)
          dif2 = abs(conv - co)/(co)
          accuracy = 0.01d0

          if ( dif2 .gt. accuracy ) then  ! hk: en zolang (nh .lt. size(hlv) )

            num = num + 1

            if (nh .gt. 400) then
              nh = nh
            endif


            do k = nh+1, j+1,-1
              hlv(k) = hlv(k-1)
            enddo

            hlv(j) = hlvz
            nh = nh + 1

            nbn = 0

            iwft (n12,j,1:nbn+1) = 0
            iaft (n12,j,1:nbn+1) = 0
            ipft (n12,j,1:nbn+1) = 0
            ikpt (n12,j,1:nbn+1) = 0
            iknt (n12,j,1:nbn+1) = 0

            j  = j - 1

          endif

        endif

      endif
      
      if (j==nh) then
         ! check if previous conveyance is large enough
         
         if (j==1) then
            hlv(j+1) = hlv(j)+EXTRA_HEIGHT
            nh = nh+1
         elseif (ikpt(n12,j-1,1) < MINCONV) then
            hlv(j+1) = hlv(j)+EXTRA_HEIGHT
            nh = nh+1
         endif
         
      endif
      
    enddo  ! end do while

    if (jgetlevels .eq. 1) then
      numt = numt + num
      nump = nump + 1
    endif


    nhmax = max(nh,nhmax)

    if (nh .gt. lcnvmax) then

      call SetMessage( LEVEL_ERROR, &
          'Conveyance: Dimension error: total number of levels exceeded' )

    endif



999   continue

   !hk : dus na conveyancetables staan de totalen op plek 1, moet in code vlak hierboven
   !     waarschijnlijk nog veranderd worden. voordeel is dat we die optelling nu maar 1 keer hoeven te doen

  deallocate ( ik1, &
               yh, &
               dh, stat=ierr )
  if ( ierr .ne. 0 ) goto 9010


    return


 9000 continue
      call SetMessage( LEVEL_ERROR, &
          'ConCnv: Error allocating array space')

 9010 continue
      call SetMessage( LEVEL_ERROR, &
          'ConCnv: Error deallocating array space')

end subroutine CalcConveyanceTables

subroutine ConveyanceTables(href, grndlyr, typ, yin, z, nbo, branchid, bedFrictionType,  &
                            groundFriction, frictionSectionFrom,                         &
                            frictionSectionTo, frictionTypePos, frictionValuePos,        &
                            frictionTypeNeg, frictionValueNeg, jf, vf, y, d, hlv,        &
                            nc, n12, jgetlevels, zminpr, nhmax)

   use MessageHandling
   implicit none
   
   double precision :: href
   double precision :: grndlyr
   double precision :: yin(:)
   double precision :: z(:)
   integer          :: bedFrictionType
   integer          :: nhmax
   integer          :: nbo
   integer          :: branchid
   double precision groundfriction
   
   double precision           :: frictionSectionFrom(:)
   double precision           :: frictionSectionTo(:)
   integer                    :: frictionTypePos(:)
   double precision           :: frictionValuePos(:)
   integer                    :: frictionTypeNeg(:)
   double precision           :: frictionValueNeg(:)

   integer nc, ind, typ
   integer jf(:), icrds
   integer j, jgetlevels, n12
   double precision              :: vf(:)
   double precision              :: cg(2)
   double precision              :: hlv(:), d(:), y(:), zminpr
   logical                       :: prtout
   character*10                  :: id
   
   double precision, parameter         :: eps = 1d-4

   icrds    = 0
   
   do j = 1, nc                         !hk: get coordinates for whole profile, met nc punten
      d(j) = z(j) + href       ! nc = actueel aantal punten, wordt aangepast
      if (j>1) then
         if (d(j) == d(j-1)) then
            d(j) = d(j) + 0.0011    ! Prevent horizontal to prevent devision by zero
         endif
      endif
      y(j) = yin(j)
   enddo                                ! door opschonen, groundlayer, frictiesecties, etc

   if (nbo .gt. 0) ind = 1
   nbo = max(nbo,1)  ! please check
   
   IF (NBO .LE. 0) THEN
      write(id, '(i0)') branchid
      call setMessage (LEVEL_ERROR, 'Zero friction value in branch '// trim(id))
      return
   ENDIF
   
   cg(1) = 0.
   cg(2) = 0.
   
   if (grndlyr .gt. 0.5) then    ! ground layer coeffs
      cg(1) = bedFrictionType
      cg(2) = groundFriction
   endif
     
   if (typ.ge.10) then
      prtout = .false.
      call calcConveyanceTables(jf, vf, y, d, hlv, nc, n12, jgetlevels, zminpr, grndlyr, nbo,               &
                                frictionSectionFrom, frictionSectionTo, frictionTypePos, frictionValuePos,  &
                                frictionTypeNeg, frictionValueNeg, cg, nhmax)

   endif

   return

   end subroutine
                 
     
subroutine regulatehlv(hlv, nh)  ! sorteren en dubbele entries weghalen

   use qsort

   integer nh, ja, k, j

   double precision hlv(nh)

   call d_qsort(hlv, nh)

   ja  = 1                                ! en dubbele entries weghalen
   do while (ja .eq. 1)
      ja = 0
      do k = 2,nh
      if (k .le. nh .and. abs(hlv(k) - hlv(k-1)) < 1e-8 ) then
         ja   = 1
         nh   = nh - 1
         do j = k,nh
            hlv(j) = hlv(j+1)
         enddo
      endif
      enddo
   enddo

   return

end subroutine regulatehlv
    
subroutine ConveyYZ(n,y,z,jf,cf,hw,af,at,w,wt,p,co) ! conveyance computation for a YZ profile
! in
integer          :: n                          ! number of profile points
double precision :: y(n), z(n)                 ! original YZ profile description (Z positive up),
                                               ! also breakpointed at friction segment points. no more.......
integer          :: jf(n)                      ! friction types for segments (1, n-1)
double precision :: cf(n)                      ! friction coefficients for segments (1, n-1)
double precision :: hw                         ! water level for which a,w,p,co must be calculated

! out
double precision :: af, at                     ! area flow, total
double precision :: w, wt                      ! flow width, total
double precision :: p                          ! perimeter
double precision :: co                         ! conveyance pos dir

!local
integer          :: k, ct
double precision :: cfv                      ! friction coefficients for segments (1, n-1)
double precision :: z0,z1,d0,d1,dz,y0,y1,bb,bt
double precision :: aa,ww,pp,cc

af = 0 
at = 0 
w = 0
wt = 0
p = 0
co  = 0

do k = 1,n-1

   z0 = z(k)
   z1 = z(k+1)
   d0 = max(hw - z0,1.0d-6)                                ! depth left
   d1 = max(hw - z1,1.0d-6)                                ! depth right
   dz = dabs(z1 - z0)

   y0 = y(k)   - y(k)
   y1 = y(k+1) - y(k)
   bb = dabs(y1 - y0)         ! breedte segment
   bt = 0.0d0
   if(bb.ne.0.d0) bt = (z1-z0)/(y1-y0) ! beta = tan(phi)

   ct = jf(k) ! type
   cfv = max(cf(k),1.0e-10)
   if(.NOT. (d0.le.1.0d-6.and.d1.le.1.0d-6)) then
     call ConveySeg (y0,y1,d0,d1,bt,bb,dz,ct,cfv,aa,ww,pp,cc) ! segment routine
     at = at + aa
    wt = wt + ww
     if (cfv .ne. 0.0) then
         w = w + ww
       af = af + aa
       p = p + pp
       co = co + cc   ! totals
     endif
   endif
enddo

end subroutine ConveyYz


subroutine ConveySeg(y0,y1,d0,d1,bt,bb,dz,ct,cf,a,w,p,co)  ! conveyance computation for a segment
! in
integer          :: ct
double precision :: y0, y1                     ! left and right y values   (m)
double precision :: d0, d1                     ! left and right waterdepth (m), always either hl > 0 or hr > 0
double precision :: bt                         ! beta = tan(phi)
double precision :: bb                         ! y1 - y0 (m)
double precision :: dz                         ! |z1-z0|
double precision :: cf                         ! roughness coefficient

! out
double precision :: a                          ! area       (m2)
double precision :: w                          ! width      (m)
double precision :: p                          ! perimeter  (m)
double precision :: co                         ! conveyance (m2/s)

! locals

double precision, parameter :: sixth = -1d0/6d0  ! for power law
double precision, parameter :: s83   = 8d0/3d0   ! for power law
double precision, parameter :: s52   = 5d0/2d0   ! for power law
double precision, parameter :: s53   = 5d0/3d0   ! for power law
double precision, parameter :: s32   = 3d0/2d0   ! for power law
double precision, parameter :: s14   = 1d0/4d0   ! for power law
double precision, parameter :: s25   = 2d0/5d0   ! for power law
double precision            :: c1, c2, dcf, f1, f2    

co = 0.0d0
dcf = cf
if(ct.eq.3) then
   c1 = (1.0d0+bt**2)**s14*dlog(10.0d0)
   c2 = dcf/12.0d0
endif
!
if(d0.lt.dz.or.d1.lt.dz) then   ! beta#0
   if(bt.lt.-0.01d0) then
      select case (ct)
      case (0)                   ! Chezy
         co = 2.0d0*dcf/(5.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d1**s52
      case (1)                   ! Manning (n)
         co = 3.0d0/(8.0d0*dcf*dabs(bt)*(1.0d0+bt**2)**s14)*d1**s83
      case (7)                   ! Strickler (kn)
         co = 75.0d0*dcf**sixth/(8.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d1**s83
      case (8)                   ! Strickler (ks)
         co = 3.0d0*dcf/(8.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d1**s83
      case (3)                   ! White-Colebrook (kn)
         if(d1/c2.le.1.495d0) then 
            f1 = 2.13d-3
         else
            f1 = dlog(d1/c2)-s25
         endif
         co = 36.0d0/(5.0d0*dabs(bt)*c1)*d1**s52*f1
      case (9)                   ! Bos&Bijkerk 
         co = dcf/(3.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d1**3
      end select
   else if(bt.ge.-0.01.and.bt.lt.0) then
      select case (ct)
      case (0)                   ! Chezy
         co = dcf/(((1.0d0+bt**2)**s14))*(d1/2.0d0)**s32*(-d1/bt)
      case (1)                   ! Manning (n)
         co = 1/(dcf*(1.0d0+bt**2)**s14)*(d1/2.0d0)**s53*(-d1/bt)
      case (7)                   ! Strickler (kn)
         co = 25.0d0*dcf**sixth/((1.0d0+bt**2)**s14)*(d1/2.0d0)**s53*(-d1/bt)
      case (8)                   ! Strickler (ks)
         co = dcf/((1.0d0+bt**2)**s14)*(d1/2.0d0)**s53*(-d1/bt)
      case (3)                   ! White-Colebrook (kn)
         if(6.0d0*d1/dcf.le.1.0005d0) then
            f1 = 2.2d-4
         else
            f1 = dlog10(6.0d0*d1/dcf)
         endif
         co = 18.0d0/((1.0d0+bt**2)**s14)*f1*(-d1/bt)*(d1/2.0d0)**s32
      case (9)                   ! Bos&Bijkerk 
         co = dcf/((1.0d0+bt**2)**s14)*(d1/2.0d0)**2*(-d1/bt)
      end select
  else if(bt.le.0.01.and.bt.gt.0) then
      select case (ct)
      case (0)                   ! Chezy
         co = dcf/((1.0d0+bt**2)**s14)*(d0/2.0d0)**s32*(d0/bt)
      case (1)                   ! Manning (n)
         co = 1/(dcf*(1.0d0+bt**2)**s14)*(d0/2.0d0)**s53*(d0/bt)
      case (7)                   ! Strickler (kn)
         co = 25.0d0*dcf**sixth/((1.0d0+bt**2)**s14)*(d0/2.0d0)**s53*(d0/bt)
      case (8)                   ! Strickler (ks)
         co = dcf/((1.0d0+bt**2)**s14)*(d0/2.0d0)**s53*(d0/bt)
      case (3)                   ! White-Colebrook (kn)
         if(6.0d0*d0/dcf.le.1.0005d0) then
            f1 = 2.2d-4
         else
            f1 = dlog10(6.0d0*d0/dcf)
         endif
         co = 18.0d0/((1.0d0+bt**2)**s14)*f1*(d0/bt)*(d0/2.0d0)**s32
      case (9)                   ! Bos&Bijkerk 
         co = dcf/((1.0d0+bt**2)**s14)*(d0/2.0d0)**2*(d0/bt)
      end select
  else if(bt.gt.0.01) then
      select case (ct)
      case (0)                   ! Chezy
         co = 2.0d0*dcf/(5.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d0**s52
      case (1)                   ! Manning (n)
         co = 3.0d0/(8.0d0*dcf*dabs(bt)*(1.0d0+bt**2)**s14)*d0**s83
      case (7)                   ! Strickler (kn)
         co = 75.0d0*dcf**sixth/(8.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d0**s83
      case (8)                   ! Strickler (ks)
         co = 3.0d0*dcf/(8.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d0**s83
      case (3)                   ! White-Colebrook (kn)
         if(d0/c2.le.1.495d0) then 
            f1 = 2.13d-3
         else
            f1 = dlog(d0/c2)-s25
         endif
         co = 36.0d0/(5.0d0*dabs(bt)*c1)*d0**s52*f1
      case (9)                   ! Bos&Bijkerk 
         co = dcf/(3.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*d0**3
      end select
  endif
endif
if(d0.ge.dz.or.d1.ge.dz) then   
  if(bt.ge.-0.01d0.and.bt.le.0.01d0) then
      select case (ct)
      case (0)                   ! Chezy
         co = dcf/((1.0d0+bt**2)**s14)*((d0+d1)/2.0d0)**s32*bb
      case (1)                   ! Manning (n)
         co = 1.0d0/(dcf*(1.0d0+bt**2)**s14)*((d0+d1)/2.0d0)**s53*bb
      case (7)                   ! Strickler (kn)
         co = 25.0d0*dcf**sixth/((1.0d0+bt**2)**s14)*((d0+d1)/2.0d0)**s53*bb
      case (8)                   ! Strickler (ks)
         co = dcf/((1.0d0+bt**2)**s14)*((d0+d1)/2.0d0)**s53*bb
      case (3)                   ! White-Colebrook (kn)
         if(6.0d0*(d0+d1)/dcf.le.1.0005d0) then
            f1 = 2.2d-4
         else
            f1 = dlog10(6.0d0*(d0+d1)/dcf)
         endif
         co = 18.0d0/((1.0d0+bt**2)**s14)*f1*bb*((d0+d1)/2.0d0)**s32
      case (9)                   ! Bos&Bijkerk 
         co = dcf/((1.0d0+bt**2)**s14)*((d0+d1)/2.0d0)**2*bb
      end select
  elseif (dabs(bt) .gt. 0.01d0) then
      select case (ct)
      case (0)                   ! Chezy
         co = 2.0d0*dcf/(5.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*dabs(d0**s52-d1**s52)
      case (1)                   ! Manning (n)
         co = 3.0d0/(8.0d0*dcf*dabs(bt)*(1.0d0+bt**2)**s14)*dabs(d0**s83-d1**s83)
      case (7)                   ! Strickler (kn)
         co = 75.0d0*dcf**sixth/(8.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*dabs(d0**s83-d1**s83)
      case (8)                   ! Strickler (ks)
         co = 3.0d0*dcf/(8.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*dabs(d0**s83-d1**s83)
      case (3)                   ! White-Colebrook (kn)
         if(d0/c2.le.1.495d0) then 
            f1 = 2.13d-3
         else
            f1 = dlog(d0/c2)-s25
         endif
         if(d1/c2.le.1.495d0) then 
            f2 = 2.13d-3
         else
            f2= dlog(d1/c2)-s25
         endif
         co = 36.0d0/(5.0d0*dabs(bt)*c1)*dabs(d0**s52*f1-(d1**s52*f2))
      case (9)                   ! Bos&Bijkerk 
         co = dcf/(3.0d0*dabs(bt)*(1.0d0+bt**2)**s14)*dabs(d0**3-d1**3)
      end select
  endif
endif
call compwap(y0,y1,d0,d1,dz,w,a,p)
end subroutine ConveySeg

subroutine compwap(yL,yr,hL,hr,dz,w,a,p)
double precision :: hL,hr,dz,yL,yR,f,w,a,p
double precision :: h1, h2                     ! min, max depth
double precision :: dh                         ! depth dif

   if (max(hL,hr) .gt. 1.0d-6) then               ! both wet or one wet
      if (min(hL,hr) .gt. 1.0d-6) then            ! both wet
!
      else if (hL .gt. 1.0d-6) then               ! only left wet
         f  = hl/dz
         yr = f*yr
       hr = 0d0
      else if (hr .gt. 1.0d-6) then               ! only rigth wet
         f  = hr/dz 
         yr = f*yr
       hl = 0d0
      endif
   endif

   if (hL .gt. hr) then
     h1 = hL
    h2 = hr
   else                                       
     h2 = hL
    h1 = hr
   endif

   dh = h1 - h2

   w  = yr - yL
   a  = 0.5d0*( h1 + h2 )*w
   p  = dsqrt ( w*w  + dh*dh )
end subroutine compwap

subroutine write_conv_tab(convtab)
   use messagehandling
   
   type(t_crsu), intent(in)      :: convtab
   
   integer :: nlevels
   integer :: i
   
   write(msgbuf, '(''Number of levels in Conveyance table = '', i5)') convtab%nru
   call msg_flush()
   
   write(msgbuf,'(''Extrapolation factor a (positive direction)'', g14.6)') convtab%a_pos_extr
   call msg_flush()
   write(msgbuf,'(''Extrapolation factor a (negative direction)'', g14.6)') convtab%a_neg_extr
   call msg_flush()
   write(msgbuf,'(''Extrapolation factor b (positive direction)'', g14.6)') convtab%b_pos_extr
   call msg_flush()
   write(msgbuf,'(''Extrapolation factor b (negative direction)'', g14.6)') convtab%b_neg_extr
   call msg_flush()
   write(msgbuf,'(11a17)') 'Water_depth', 'Total_width', 'Flow_width', 'Total_Area', 'Flow_Area', 'Conv_pos_dir', &
               'Conv_neg_dir', 'Perimeter'
   call msg_flush()

   nlevels = convtab%nru
   do i = 1, nlevels
      write(msgbuf, '(11g17.6)') convtab%hu (i) , convtab%wt(i,1), convtab%wf (i), convtab%at(i,1), convtab%af (i), &
                                 convtab%co1(i), convtab%co2(i), convtab%pf (i)
      call msg_flush()
   enddo 
   
end subroutine write_conv_tab

end module M_newcross           ! new type conveyance table crossections



