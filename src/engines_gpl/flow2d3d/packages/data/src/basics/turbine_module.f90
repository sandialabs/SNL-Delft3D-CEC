module turbine_module
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
!  $Id: turbine_module.f90 5747 2016-01-20 10:00:59Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/engines_gpl/flow2d3d/packages/data/src/basics/turbine_module.f90 $
!!--declarations----------------------------------------------------------------
    use precision
    use table_handles, only: handletype, MAXTABLECLENGTH, cleartable
    !
    implicit none

type structure_turbine
    integer, dimension(2)             :: thrustcrvnr    ! index of turbine thurst curve
    integer, dimension(2)             :: powercrvnr     ! index of turbine power curve
    integer                           :: vertpos        ! vertical positioning type of turbine: 0 = fixed z level, 1 = turbine axis at fixed depth relative to water surface
    integer                           :: turbtype       ! BJ 20150326: turbine type: 0 = circular, 1 = rectangular
    real(fp)                          :: angle          ! orientation of turbine indicated by angle between turbine axis and positive x/lon direction
    real(fp)                          :: diam           ! diameter of turbine (turbtype=0)
    real(fp)                          :: width          ! BJ 20150326: width of turbine (turbtype=1)
    real(fp)                          :: height         ! BJ 20150326: height of turbine (turbtype=1)
    real(fp)                          :: turbarea       ! turbine area (0.25*pi*D**2)
    real(fp), dimension(3)            :: xyz            ! (relative) position of turbine
    character(256)                    :: name           ! name of turbine
    character(MAXTABLECLENGTH)        :: thrustcrvname  ! name of turbine thrust curve
    character(MAXTABLECLENGTH)        :: powercrvname   ! name of turbine power curve
    !
    integer                           :: turbinemodel   ! turbine model used 0: default deltares model, 1: SNL model
    integer                           :: turbulencemodel! turbulence model used at turbine 0: none 1: Rethore implementation 2: Rados implementation
    real(fp)                          :: beta_p         ! Beta_p coefficient
    real(fp)                          :: beta_d         ! Beta_d coefficient
    real(fp)                          :: cep4           ! Cepsilon4 coefficient
    real(fp)                          :: cep5           ! Cepsilon5 coefficient
    real(fp)                          :: friccoef       ! latest friction coefficient
    real(fp)                          :: powercoef      ! latest power coefficient
    real(fp)                          :: thrustcoef     ! latest thrust coefficient
    !
    real(fp)                          :: current_uref       ! latest reference velocity
    real(fp)                          :: current_zlevel     ! latest vertical position
    real(fp)                          :: current_sim_thrust ! latest simulated thrust
    real(fp)                          :: cumul_sim_thrust   ! integrated simulated thrust
    real(fp)                          :: current_thrust     ! latest analytical thrust
    real(fp)                          :: cumul_thrust       ! integrated analytical thrust
    real(fp)                          :: current_power      ! latest analytical power
    real(fp)                          :: cumul_power        ! integrated analytical power
    !
    integer                           :: cellnr         ! index of cell in which turbine is located
    real(fp), dimension(2)            :: relhpos        ! relative location within cell
    !
    real(fp)                          :: ndiamu         ! distance of velocity measurement point "up- & downstream"
    integer, dimension(2)             :: cellu          ! indices of "up- & downstream" cells for velocity
    !
    character(1)                      :: edgetype       ! type of edge/link on which structure is located
    integer , dimension(:)  , pointer :: edgelist       ! indices of edges on which structure is located
    real(fp), dimension(:)  , pointer :: reldist        ! distance along edges measured relative to location of turbine
    real(fp), dimension(:,:), pointer :: zlevel         ! work array of right size to store levels of layer interfaces per cell
    real(fp), dimension(:,:), pointer :: area           ! work array to store full    exchange area          per layer and cell
    real(fp), dimension(:,:), pointer :: blockfrac      ! work array to store blocked exchange area fraction per layer and cell
end type structure_turbine

type structure_turbines
    type(structure_turbine)   , dimension(:)    , pointer :: nr         ! pointer to individual turbines
    !
    type(handletype)                                      :: curves     ! thrust and power curve tables
end type structure_turbines

contains

subroutine init_turbines(turbines)
!!--description-----------------------------------------------------------------
!
! Initialize turbine data records
!
!!--declarations----------------------------------------------------------------
    implicit none
!
! Call variables
!
    type(structure_turbines)     , intent(out) :: turbines
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    nullify(turbines%nr)
end subroutine init_turbines


subroutine allocate_turbines(turbines,nturbines,lundia,error)
!!--description-----------------------------------------------------------------
!
! Allocate turbine data records
!
!!--declarations----------------------------------------------------------------
    use message_module, only: write_error
    implicit none
!
! Call variables
!
    integer                     , intent(in)    :: lundia
    integer                     , intent(in)    :: nturbines
    logical                     , intent(out)   :: error
    type(structure_turbines)    , intent(inout) :: turbines
!
! Local variables
!
    integer :: i
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
    error = .false.
    allocate(turbines%nr  (nturbines)  , stat=istat)
    if (istat/=0) then
       call write_error('Memory allocation error in ALLOCATE_TURBINES.',unit=lundia)
       error = .true.
       return
    endif
    !
    do i = 1, nturbines
       turbines%nr(i)%thrustcrvnr        = -999
       turbines%nr(i)%powercrvnr         = -999
       turbines%nr(i)%vertpos            = 0
       turbines%nr(i)%turbtype           = 0       ! BJ 20150326
       turbines%nr(i)%angle              = 0.0_fp
       turbines%nr(i)%width              = 0.0_fp  ! BJ 20150326
       turbines%nr(i)%height             = 0.0_fp  ! BJ 20150326
       turbines%nr(i)%diam               = 0.0_fp
       turbines%nr(i)%turbarea           = 0.0_fp
       turbines%nr(i)%xyz                = 0.0_fp
       turbines%nr(i)%name               = ' '
       turbines%nr(i)%thrustcrvname      = ' '
       turbines%nr(i)%powercrvname       = ' '
       !
       turbines%nr(i)%cellnr             = -999
       turbines%nr(i)%relhpos            = 0.0_fp
       !
       turbines%nr(i)%ndiamu             = 5.0_fp
       turbines%nr(i)%cellu              = 0
       !
       turbines%nr(i)%turbinemodel       = 0
       turbines%nr(i)%turbulencemodel    = 0
       turbines%nr(i)%beta_p             = -999.0_fp
       turbines%nr(i)%beta_d             = -999.0_fp
       turbines%nr(i)%cep4               = -999.0_fp
       turbines%nr(i)%cep5               = -999.0_fp
       turbines%nr(i)%friccoef           = -999.0_fp
       turbines%nr(i)%powercoef          = -999.0_fp
       turbines%nr(i)%thrustcoef         = -999.0_fp
       !
       turbines%nr(i)%current_uref       = -999.0_fp
       turbines%nr(i)%current_sim_thrust = -999.0_fp
       turbines%nr(i)%cumul_sim_thrust   =    0.0_fp
       turbines%nr(i)%current_thrust     = -999.0_fp
       turbines%nr(i)%cumul_thrust       =    0.0_fp
       turbines%nr(i)%current_power      = -999.0_fp
       turbines%nr(i)%cumul_power        =    0.0_fp
       !
       turbines%nr(i)%edgetype           = ' '
       nullify(turbines%nr(i)%edgelist)
       nullify(turbines%nr(i)%reldist)
       nullify(turbines%nr(i)%zlevel)
       nullify(turbines%nr(i)%area)
       nullify(turbines%nr(i)%blockfrac)
    enddo
end subroutine allocate_turbines


subroutine deallocate_turbines(turbines)
!!--description-----------------------------------------------------------------
!
! Deallocate turbine data records
!
!!--declarations----------------------------------------------------------------
    implicit none
!
! Call variables
!
    type(structure_turbines)    , intent(inout) :: turbines
!
! Local variables
!
    integer :: i
    integer :: istat
!
!! executable statements -------------------------------------------------------
!
    if (associated(turbines%nr)) then
       do i = 1,size(turbines%nr)
          if (associated(turbines%nr(i)%edgelist))  deallocate(turbines%nr(i)%edgelist, stat=istat)
          if (associated(turbines%nr(i)%reldist))   deallocate(turbines%nr(i)%reldist, stat=istat)
          if (associated(turbines%nr(i)%zlevel))    deallocate(turbines%nr(i)%zlevel, stat=istat)
          if (associated(turbines%nr(i)%area))      deallocate(turbines%nr(i)%area, stat=istat)
          if (associated(turbines%nr(i)%blockfrac)) deallocate(turbines%nr(i)%blockfrac, stat=istat)
       enddo
       deallocate(turbines%nr  , stat=istat)
    endif
    !
    call cleartable(turbines%curves)
end subroutine deallocate_turbines


subroutine intersect_turbine(reldist,zlevel,z0,diam,width,height,turbtype,area,blockfrac)
    use precision
    implicit none
!
! Call variables
!
    real(fp), dimension(:)   , intent(in)  :: reldist   ! horizontal distance relative to turbine [nmax+1]
    real(fp), dimension(:,0:), intent(in)  :: zlevel    ! vertical position of layer interfaces [nmax,kmax+1]
    real(fp)                 , intent(in)  :: z0        ! z coordinate of turbine axis
    real(fp)                 , intent(in)  :: diam      ! turbine diameter
    real(fp)                 , intent(in)  :: width     ! turbine width
    real(fp)                 , intent(in)  :: height    ! turbine height
    integer                  , intent(in)  :: turbtype  ! turbine type
    real(fp), dimension(:,:) , intent(out) :: area      ! full    exchange area          [nmax,kmax]
    real(fp), dimension(:,:) , intent(out) :: blockfrac ! blocked exchange area fraction [nmax,kmax]
!
! Local variables
!
    integer  :: k
    integer  :: n
    real(fp) :: rad
!
!! executable statements -------------------------------------------------------
!
    rad = diam/2.0_fp
    !
    do n = 1, size(blockfrac, dim=1)
        do k = 1, size(blockfrac, dim=2)
            area(n,k)      = abs(zlevel(n,k-1) - zlevel(n,k)) * abs(reldist(n) - reldist(n+1))
            if (turbtype == 0) then   ! BJ 20150326, existing case of circular turbine
               blockfrac(n,k) = intersect_circle_rect(reldist(n),reldist(n+1),zlevel(n,k-1),zlevel(n,k),rad,0.0_fp,z0) / &
                           & area(n,k)
            elseif (turbtype == 1) then   ! BJ 20150326, new case of rectangular turbine
               blockfrac(n,k) = abs(min(max(reldist(n),-width/2.0_fp),width/2.0_fp) - min(max(reldist(n+1),-width/2.0_fp),width/2.0_fp)) * &
                              & abs(min(max(zlevel(n,k)-z0,-height/2.0_fp),height/2.0_fp) - min(max(zlevel(n,k-1),-height/2.0_fp),height/2.0_fp)) / &
                              & area(n,k)
            endif   ! BJ 20150326
        enddo
    enddo
end subroutine intersect_turbine


function intersect_circle_rect(x1,x2,z1,z2,rad,x0,z0) result (area)
    use precision
    implicit none
!
! Call variables
!
    real(fp), intent(in)  :: x1    ! x coordinate of corner 1
    real(fp), intent(in)  :: x2    ! x coordinate of corner 2
    real(fp), intent(in)  :: z1    ! z coordinate of corner 1
    real(fp), intent(in)  :: z2    ! z coordinate of corner 2
    real(fp), intent(in)  :: rad   ! radius of circle
    real(fp), intent(in)  :: x0    ! x coordinate of circle centre
    real(fp), intent(in)  :: z0    ! z coordinate of circle centre
    real(fp)              :: area  ! intersection area
!
! Local variables
!
    real(fp)              :: zero = 0.0_fp ! as the name says: zero
    !
    real(fp)              :: dxmin ! minimum of x coordinates
    real(fp)              :: dxmax ! maximum of x coordinates
    real(fp)              :: dzmin ! minimum of z coordinates
    real(fp)              :: dzmax ! maximum of z coordinates
    real(fp)              :: area1 ! area of intersection with circle part 1 (  0 -  90 deg)
    real(fp)              :: area2 ! area of intersection with circle part 1 ( 90 - 180 deg)
    real(fp)              :: area3 ! area of intersection with circle part 1 (180 - 270 deg)
    real(fp)              :: area4 ! area of intersection with circle part 1 (270 - 360 deg)
!
!! executable statements -------------------------------------------------------
!
    !
    ! convert to coordinates relative to centre of circle
    !
    dxmin = min(x1,x2) - x0
    dxmax = max(x1,x2) - x0
    dzmin = min(z1,z2) - z0
    dzmax = max(z1,z2) - z0
    !
    ! The total area of the intersection of the rectangle with the circle
    ! equals the sum of the areas of the intersections of each quarter of the
    ! circle with the rectangle.
    !
    ! In each case we only have to consider part of the rectangle in the same
    ! quadrant (i.e. the associated sign of x and z). We implement only the
    ! first quarter and map the other quarters to the first one.
    !
    area1 = intersect_circle_rect4( max(zero,dxmin), max(zero,dxmax), max(zero,dzmin), max(zero,dzmax),rad) ! upper right (Q1)
    area2 = intersect_circle_rect4(-min(zero,dxmax),-min(zero,dxmin), max(zero,dzmin), max(zero,dzmax),rad) ! upper left  (Q2) - flip x
    area3 = intersect_circle_rect4(-min(zero,dxmax),-min(zero,dxmin),-min(zero,dzmax),-min(zero,dzmin),rad) ! lower left  (Q3) - flip x - flip z
    area4 = intersect_circle_rect4( max(zero,dxmin), max(zero,dxmax),-min(zero,dzmax),-min(zero,dzmin),rad) ! lower right (Q4) - flip z
    area  = area1 + area2 + area3 + area4
end function intersect_circle_rect


function intersect_circle_rect4(dxmin,dxmax,dzmin,dzmax,rad) result (area)
    use precision
    implicit none
!
! Call variables
!
    real(fp), intent(in)  :: dxmin ! minimum relative x coordinate (x=0 at circle centre)
    real(fp), intent(in)  :: dxmax ! maximum relative x coordinate (x=0 at circle centre)
    real(fp), intent(in)  :: dzmin ! minimum relative z coordinate (z=0 at circle centre)
    real(fp), intent(in)  :: dzmax ! maximum relative z coordinate (z=0 at circle centre)
    real(fp), intent(in)  :: rad   ! radius of circle
    real(fp)              :: area  ! intersection area
!
! Local variables
!
    real(fp)              :: zero = 0.0_fp ! as the name says: zero
    !
    real(fp)              :: rad2  ! squared radius
    real(fp)              :: a1    ! angle 1 (radian) associated with first intersection of circle and box
    real(fp)              :: a2    ! angle 2 (radian) associated with second intersection of circle and box
    real(fp)              :: dx1   ! x coordinate of intersection point 1
    real(fp)              :: dx2   ! x coordinate of intersection point 2
    real(fp)              :: dz1   ! z coordinate of intersection point 1
    real(fp)              :: dz2   ! z coordinate of intersection point 2
!
!! executable statements -------------------------------------------------------
!
    rad2 = rad*rad
    !
    if ((dxmin*dxmin + dzmin*dzmin) > rad2) then
        ! rectangle completely outside circle
        area = zero
    elseif ((dxmax*dxmax + dzmax*dzmax) < rad2) then
        ! rectangle completely inside circle
        area = (dxmax-dxmin)*(dzmax-dzmin)
    else
        ! rectangle partly inside circle
        if ((dxmax*dxmax + dzmin*dzmin) > rad2) then
            ! edge (dxmin,dzmin) - (dxmax,dzmin) intersects the circle
            a1 = asin(dzmin/rad)
            dx1 = sqrt(rad2-dzmin*dzmin)
            dz1 = dzmin
        else
            ! edge (dxmax,dzmin) - (dxmax,dzmax) intersects the circle
            a1 = acos(dxmax/rad)
            dx1 = dxmax
            dz1 = sqrt(rad2-dxmax*dxmax)
        endif
        if ((dxmin*dxmin + dzmax*dzmax) > rad2) then
            ! edge (dxmin,dzmin) - (dxmin,dzmax) intersects the circle
            a2 = acos(dxmin/rad)
            dx2 = dxmin
            dz2 = sqrt(rad2-dxmin*dxmin)
        else
            ! edge (dxmin,dzmax) - (dxmax,dzmax) intersects the circle
            a2 = asin(dzmax/rad)
            dx2 = sqrt(rad2-dzmax*dzmax)
            dz2 = dzmax
        endif
        area = 0.5_fp*(a2-a1)*rad2 &
             & + 0.5_fp*dx1*dz1 &
             & + 0.5_fp*dx2*dz2 &
             & - dxmin*dz2 &
             & - (dx1-dxmin)*dzmin
    endif
end function intersect_circle_rect4

function turbinecurve(curves, curvenr, u, errorstring) result(thrustcoef)
    use precision
    use table_handles, only: gettabledata
    implicit none
!
! Call variables
!
    integer, dimension(2), intent(in) :: curvenr     ! index of turbine thrust curve
    type(handletype)     , intent(in) :: curves      ! thrust and power curve tables
    real(fp)             , intent(in) :: u           ! velocity u
    real(fp)                          :: thrustcoef  ! resulting thrust coefficient
    character(256)       ,intent(out) :: errorstring ! error string
!
! Local variables
!
    real(fp), dimension(1)            :: thrustvar   ! resulting thrust coefficient
!
!! executable statements -------------------------------------------------------
!
    call gettabledata(curves, curvenr(1), curvenr(2), 1, 1, &
                    & thrustvar, u, 0, errorstring)
    thrustcoef = thrustvar(1)
end function turbinecurve

end module turbine_module