module m_rdturbine
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
!  $Id: rdturbine.f90 5748 2016-01-20 13:00:50Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/engines_gpl/flow2d3d/packages/data/src/basics/rdturbine.f90 $
!!--declarations----------------------------------------------------------------
    implicit none
    
contains

subroutine rdturbine(filtrb, lundia, turbines, error)
!!--description-----------------------------------------------------------------
!
! Read turbine data from input file
!
!!--declarations----------------------------------------------------------------
    use precision
    use properties ! includes tree_structures
    !use grid_dimens_module, only: griddimtype
    use turbine_module, only: structure_turbines, allocate_turbines
    use message_module, only: write_error, write_warning, FILE_NOT_FOUND, FILE_READ_ERROR, PREMATURE_EOF
    use table_handles, only: readtable, gettable, GETTABLE_NAME
    use mathconsts, only: pi
    !
    implicit none
!
! Call variables
!
!    type (griddimtype)          , pointer     :: griddim
    character(*)                , intent(in)  :: filtrb
    integer                     , intent(in)  :: lundia
    type(structure_turbines)    , intent(out) :: turbines
    logical                     , intent(out) :: error
!
! Local variables
!
    integer                     , external    :: newunit
    !
    integer                                   :: istat
    integer                                   :: j
    integer                                   :: itrb
    integer                                   :: nval
    real(fp), dimension(2)                    :: xyloc
    character(10)                             :: tempstring
    character(20)                             :: parname
    character(256)                            :: curvename
    character(256)                            :: trbthrustfil
    character(1024)                           :: message
    type (tree_data)            , pointer     :: aturbine
    type (tree_data)            , pointer     :: turbine_tree
!
!! executable statements -------------------------------------------------------
!
    error = .false.
    call tree_create  ( "Turbine input", turbine_tree )
    !
    if (filtrb == ' ') then
       call write_error('Empty turbine file name specified.',unit=lundia)
       error = .true.
       return
    endif     
    !
    ! Read turbine-file
    !
    call prop_file('ini', trim(filtrb), turbine_tree, istat)
    if (istat /= 0) then
       select case (istat)
       case(1)
          call write_error(FILE_NOT_FOUND//trim(filtrb), unit=lundia)
       case(3)
          call write_error(PREMATURE_EOF//trim(filtrb), unit=lundia)
       case default
          call write_error(FILE_READ_ERROR//trim(filtrb), unit=lundia)
       endselect
       error = .true.
       return
    endif
    !
    call prop_get_string(turbine_tree, 'TurbineFileInformation', 'FileVersion', tempstring)
    if (trim(tempstring) /= '01.00') then
       call write_error('FileVersion should match 01.00 for turbine file.',unit=lundia)
       error = .true.
       return
    endif
    !
    trbthrustfil = ' '
    call prop_get_string(turbine_tree,'General','CurvesFil',trbthrustfil)
    if (trbthrustfil == ' ') then
       call write_error('Missing Turbine Loss Coefficients file.',unit=lundia)
       error = .true.
       return
    else
       message = ' '
       call readtable(turbines%curves, trbthrustfil, 0, message)
       if (message /= ' ') then
          call write_error(message,unit=lundia)
          error = .true.
          return
       endif
    endif
    !
    itrb = 0
    do j = 1, size(turbine_tree%child_nodes)
       !
       ! Does turbine_tree contain any child with name 'turbine' (converted to lower case)?
       !
       aturbine => turbine_tree%child_nodes(j)%node_ptr
       parname = tree_get_name( aturbine )
       if (parname == 'turbine') then
          itrb = itrb+1
       endif
    enddo
    !
    call allocate_turbines(turbines,itrb,lundia,error)
    if (error) return
    !
    itrb = 0
    do j = 1, size(turbine_tree%child_nodes)
       !
       ! Does turbine_tree contain any child with name 'turbine' (converted to lower case)?
       !
       aturbine => turbine_tree%child_nodes(j)%node_ptr
       parname = tree_get_name( aturbine )
       if (parname == 'turbine') then
          itrb = itrb+1
          call prop_get_string(aturbine, '*', 'Name', turbines%nr(itrb)%name)
          if (turbines%nr(itrb)%name == ' ') write(turbines%nr(itrb)%name,'(A,I0)') 'Turbine ',itrb
          !
          call prop_get(aturbine, '*', 'TurbType', turbines%nr(itrb)%turbtype) ! BJ 20150326
          call prop_get(aturbine, '*', 'Diameter', turbines%nr(itrb)%diam)     ! BJ 20150326 Need to keep this for NDiaDist4Vel
          if (turbines%nr(itrb)%turbtype == 0) then ! BJ 20150326
           turbines%nr(itrb)%turbarea = 0.25_fp*pi*turbines%nr(itrb)%diam**2
          elseif (turbines%nr(itrb)%turbtype == 1) then
             call prop_get(aturbine, '*', 'Width', turbines%nr(itrb)%width)
             call prop_get(aturbine, '*', 'Height', turbines%nr(itrb)%height)
             turbines%nr(itrb)%turbarea = turbines%nr(itrb)%width*turbines%nr(itrb)%height
          endif
          call prop_get(aturbine, '*', 'XYLoc', xyloc, 2)
          turbines%nr(itrb)%xyz(1:2) = xyloc
          call prop_get(aturbine, '*', 'Orientation', turbines%nr(itrb)%angle)
          call prop_get(aturbine, '*', 'NDiaDist4Vel', turbines%nr(itrb)%ndiamu)
          call prop_get(aturbine, '*', 'TurbineModel', turbines%nr(itrb)%turbinemodel)
          call prop_get(aturbine, '*', 'TurbulenceModel', turbines%nr(itrb)%turbulencemodel)
          call prop_get(aturbine, '*', 'Beta_p', turbines%nr(itrb)%beta_p)
          call prop_get(aturbine, '*', 'Beta_d', turbines%nr(itrb)%beta_d)
          call prop_get(aturbine, '*', 'Cep4', turbines%nr(itrb)%cep4)
          call prop_get(aturbine, '*', 'Cep5', turbines%nr(itrb)%cep5)
          !
          tempstring = ' '
          call prop_get_string(aturbine, '*', 'VertPos', tempstring)
          call small(tempstring,10)
          if (tempstring=='fixed' .or. tempstring==' ') then
              turbines%nr(itrb)%vertpos = 0
              call prop_get(aturbine, '*', 'AxisLevel', turbines%nr(itrb)%xyz(3))
          elseif (tempstring=='floating') then
              turbines%nr(itrb)%vertpos = 1
              call prop_get(aturbine, '*', 'AxisDepth', turbines%nr(itrb)%xyz(3))
          else
              write(message,'(4A)') 'Invalid vertical position "',trim(tempstring),'" for ',turbines%nr(itrb)%name
              call write_error(message,unit=lundia)
              error = .true.
              return
          endif
          !
          call prop_get_string(aturbine, '*', 'ThrustCurve', turbines%nr(itrb)%thrustcrvname)
          call gettable(turbines%curves, turbines%nr(itrb)%thrustcrvname, 'thrust coefficient', &
                & turbines%nr(itrb)%thrustcrvnr(1), turbines%nr(itrb)%thrustcrvnr(2), &
                & nval, 1, message, GETTABLE_NAME)
          if (nval/=1) then
              write(message,'(3A)') 'Unable to find table for thrust curve "',trim(turbines%nr(itrb)%thrustcrvname),'"'
              call write_error(message,unit=lundia)
              error = .true.
              return
          endif
          !
          call prop_get_string(aturbine, '*', 'PowerCurve', turbines%nr(itrb)%powercrvname)
          if (turbines%nr(itrb)%powercrvname /= ' ') then
              call gettable(turbines%curves, turbines%nr(itrb)%powercrvname, 'power coefficient', &
                    & turbines%nr(itrb)%powercrvnr(1), turbines%nr(itrb)%powercrvnr(2), &
                    & nval, 1, message, GETTABLE_NAME)
              if (nval/=1) then
                  write(message,'(3A)') 'Unable to find table for power curve "',trim(turbines%nr(itrb)%powercrvname),'"'
                  call write_error(message,unit=lundia)
                  error = .true.
                  return
              endif
          endif
       endif
    enddo
    !
    call tree_destroy(turbine_tree)
end subroutine rdturbine


subroutine echoturbine(turbines, lundia)
!!--description-----------------------------------------------------------------
!
! Display turbine data
!
!!--declarations----------------------------------------------------------------
    use precision
    use turbine_module, only: structure_turbines
    !
    implicit none
!
! Call variables
!
    integer                     , intent(in)  :: lundia
    type(structure_turbines)    , intent(in)  :: turbines
!
! Local variables
!
    integer                                   :: j
    character(30)                             :: txtput1
    character(50)                             :: txtput2
!
!! executable statements -------------------------------------------------------
!
    if (associated(turbines%nr)) then
       write (lundia, '(a)')   '*** Start  of turbine input'
       do j = 1, size(turbines%nr)
          txtput1 = 'Turbine'
          write (lundia, '(3a)') txtput1, ': ', trim(turbines%nr(j)%name)
          if (turbines%nr(j)%turbtype==0) then ! BJ 20150326
             txtput1 = '  Diameter'
             write (lundia, '(2a,f12.3)') txtput1, ': ', turbines%nr(j)%diam
          else
             txtput1 = '  Width'
             write (lundia, '(2a,f12.3)') txtput1, ': ', turbines%nr(j)%width
             txtput1 = '  Height'
             write (lundia, '(2a,f12.3)') txtput1, ': ', turbines%nr(j)%height
          endif
          !
          txtput1 = '  X coordinate'
          write (lundia, '(2a,e12.4)') txtput1, ': ', turbines%nr(j)%xyz(1)
          txtput1 = '  Y coordinate'
          write (lundia, '(2a,e12.4)') txtput1, ': ', turbines%nr(j)%xyz(2)
          txtput1 = '  Orientation of turbine'
          write (lundia, '(2a,f12.3)') txtput1, ': ', turbines%nr(j)%angle
          !
          txtput1 = '  Z position of turbine axis'
          select case(turbines%nr(j)%vertpos)
             case (0)
                write(txtput2,'(a,e12.4)') 'fixed at level =',turbines%nr(j)%xyz(3)
             case (1)
                write(txtput2,'(a,e12.4)') 'floating at depth =',turbines%nr(j)%xyz(3)
          end select
          write (lundia, '(3a)') txtput1, ': ', trim(txtput2)
          !
          txtput1 = '  Loss Curve Name'
          write (lundia, '(3a)') txtput1, ': ', trim(turbines%nr(j)%thrustcrvname)
          !
          txtput1 = '  Power Curve Name'
          if (turbines%nr(j)%powercrvname /= ' ') then
              write (lundia, '(3a)') txtput1, ': ', trim(turbines%nr(j)%powercrvname)
          else
              write (lundia, '(3a)') txtput1, ': ', 'N/A'
          endif
       enddo
       write (lundia, '(a)')   '*** End    of turbine input'
    endif
end subroutine echoturbine

subroutine mapturbine(turbines, xcor, ycor, guu, gvv, kcu, kcv, error, gdp)
!!--description-----------------------------------------------------------------
!
! Determine locations of turbines on grid
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use turbine_module, only: structure_turbines, structure_turbine
    use mathconsts
    !
    implicit none
!
! Call variables
!
    type(globdat)                                                                   , target        :: gdp
    type(structure_turbines)                                                        , intent(inout) :: turbines
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)    :: xcor
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)    :: ycor
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)    :: guu
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)    :: gvv
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)    :: kcu
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)    :: kcv
    logical                                                                         , intent(out)   :: error
!
! Local variables
!
    logical                                            :: inside
    integer                                            :: dn
    integer                                            :: dm
    integer                                            :: i
    integer                                            :: istat
    integer                                            :: j
    integer                                            :: n
    integer                                            :: nedges
    integer                                            :: nf
    integer                                            :: nt
    integer                                            :: m
    integer                                            :: mf
    integer                                            :: mt
    real(fp)                                           :: ang_nline
    real(fp)                                           :: ang_mline
    real(fp)                                           :: ang_ax
    real(fp)                                           :: df
    real(fp)                                           :: dt
    real(fp)                                           :: radius
    real(fp)                                           :: rn
    real(fp)                                           :: rm
    real(fp)                                           :: xu
    real(fp)                                           :: yu
    character(1)                                       :: normalflowdir
    character(10)                                      :: angn_str
    character(10)                                      :: angm_str
    character(10)                                      :: angx_str
    type(structure_turbine)                  , pointer :: turbine
    !
    integer, pointer                                   :: kmax
    integer, pointer                                   :: lundia
    logical, pointer                                   :: spheric
!
!! executable statements -------------------------------------------------------
!
    error = .false.
    if (associated(turbines%nr)) then
        kmax    => gdp%d%kmax
        lundia  => gdp%gdinout%lundia
        spheric => gdp%gdtricom%sferic
        do j = 1, size(turbines%nr)
            turbine => turbines%nr(j)
            !
            ! vvv This doesn't work for spheric models!  vvv
            !
            if (spheric) then
                call write_error('Determining location of turbine velocity points not yet implemented for spherical models.',unit=lundia)
                error = .true.
                return
            endif
            inside = .false.
            xu = turbine%xyz(1) + cos(turbine%angle*degrad) * turbine%ndiamu * turbine%diam
            yu = turbine%xyz(2) + sin(turbine%angle*degrad) * turbine%ndiamu * turbine%diam
            call findnm_kcs_flowwrapper(xu, yu, m, n, rm, rn, inside, spheric, gdp)
            if (.not.inside) then
                call write_error('Velocity reference point primary side outside domain for turbine: '//trim(turbine%name),unit=lundia)
                error = .true.
                return
            endif
            call n_and_m_to_nm(n, m, turbine%cellu(1), gdp)
            !
            inside = .false.
            xu = turbine%xyz(1) - cos(turbine%angle*degrad) * turbine%ndiamu * turbine%diam
            yu = turbine%xyz(2) - sin(turbine%angle*degrad) * turbine%ndiamu * turbine%diam
            call findnm_kcs_flowwrapper(xu, yu, m, n, rm, rn, inside, spheric, gdp)
            if (.not.inside) then
                call write_error('Velocity reference point reverse side outside domain for turbine: '//trim(turbine%name),unit=lundia)
                error = .true.
                return
            endif
            call n_and_m_to_nm(n, m, turbine%cellu(2), gdp)
            !
            ! ^^^ This doesn't work for spheric models!  ^^^
            !
            inside = .false.
            call findnm_kcs_flowwrapper(turbine%xyz(1), turbine%xyz(2), m, n, rm, rn, inside, spheric, gdp)
            call n_and_m_to_nm(n, m, turbine%cellnr, gdp)
            turbine%relhpos(1) = rn
            turbine%relhpos(2) = rm
            !
            if (.not.inside) then
                call write_error('Turbine '//trim(turbine%name)//' located outside the grid',unit=lundia)
                error = .true.
            else
                if (rn>0.5_fp) then
                    call angle(spheric, xcor(n,m-1), ycor(n,m-1), xcor(n,m), ycor(n,m), ang_nline, gdp)
                    dn = 0
                else
                    call angle(spheric, xcor(n-1,m-1), ycor(n-1,m-1), xcor(n-1,m), ycor(n-1,m), ang_nline, gdp)
                    dn = 1
                endif
                ang_nline = ang_nline * raddeg
                !
                if (rm>0.5_fp) then
                    call angle(spheric, xcor(n-1,m), ycor(n-1,m), xcor(n,m), ycor(n,m), ang_mline, gdp)
                    dm = 0
                else
                    call angle(spheric, xcor(n-1,m-1), ycor(n-1,m-1), xcor(n,m-1), ycor(n,m-1), ang_mline, gdp)
                    dm = 1
                endif
                ang_mline = ang_mline * raddeg
                !
                ang_ax = turbine%angle
                if (abs(ang_nline-ang_ax)<10.0_fp .or. &
                  & abs(ang_nline-ang_ax-180.0_fp)<10.0_fp .or. &
                  & abs(ang_nline-ang_ax+180.0_fp)<10.0_fp) then
                    ! turbine axis aligned with n-line, so turbine along m-line
                    normalflowdir = 'U'
                    m = m-dm
                elseif (abs(ang_mline-ang_ax)<10.0_fp .or. &
                      & abs(ang_mline-ang_ax-180.0_fp)<10.0_fp .or. &
                      & abs(ang_mline-ang_ax+180.0_fp)<10.0_fp) then
                    ! turbine axis aligned with m-line, so turbine along n-line
                    normalflowdir = 'V'
                    n = n-dn
                else
                    ! turbine axis not approximately aligned with either one of the two grid-lines
                    write(angn_str,'(f5.1)') ang_nline
                    write(angm_str,'(f5.1)') ang_mline
                    write(angx_str,'(f5.1)') ang_ax
                    call write_error('Axis of turbine '//trim(turbine%name)//' ('//trim(angx_str)//') is not aligned ' // &
                                   & 'with either M ('//trim(angm_str)//') or N ('//trim(angn_str)//') grid line',unit=lundia)
                    error = .true.
                    continue ! let's check other turbines as well
                endif
                !
                radius = turbine%diam/2.0_fp
                nf = n
                mf = m
                nt = n
                mt = m
                !
                turbine%edgetype = normalflowdir
                if (normalflowdir=='U') then
                    df = guu(n,m)*rn
                    dt = guu(n,m)*(1.0_fp-rn)
                    !
                    do while (df<radius .and. kcv(nf-1,mf)==1)
                        nf = nf-1
                        df = df+guu(nf,mf)
                    enddo
                    !
                    do while (dt<radius .and. kcv(nt+1,mt)==1)
                        nt = nt+1
                        dt = dt+guu(nt,mt)
                    enddo
                    !
                    nedges = nt-nf+1
                elseif (normalflowdir=='V') then
                    df = gvv(n,m)*rm
                    dt = gvv(n,m)*(1.0_fp-rm)
                    !
                    do while (df<radius .and. kcv(nf,mf-1)==1)
                        mf = mf-1
                        df = df+gvv(nf,mf)
                    enddo
                    !
                    do while (dt<radius .and. kcv(nt,mt+1)==1)
                        mt = mt+1
                        dt = dt+gvv(nt,mt)
                    enddo
                    !
                    nedges = mt-mf+1
                endif
                !
                if (df<radius .or. dt<radius) then
                   ! error: grid not wide enough for turbine
                   call write_error('Turbine '//trim(turbine%name)//' too large for model',unit=lundia)
                   error = .true.
                   continue ! let's check other turbines as well
                endif
                !
                              allocate(turbine%edgelist (nedges)       , stat=istat)
                if (istat==0) allocate(turbine%reldist  (nedges+1)     , stat=istat)
                if (istat==0) allocate(turbine%zlevel   (nedges,0:kmax), stat=istat)
                if (istat==0) allocate(turbine%area     (nedges,kmax)  , stat=istat)
                if (istat==0) allocate(turbine%blockfrac(nedges,kmax)  , stat=istat)
                if (istat/=0) then
                   ! error: memory allocation
                   call write_error('Memory allocation error in MAPTURBINE',unit=lundia)
                   error = .true.
                   return
                endif
                !
                df = -df
                if (normalflowdir=='U') then
                    do i = 1, nedges
                        if (i==1) then
                            turbine%reldist(1) = df
                        endif
                        call n_and_m_to_nm(nf+i-1, mf, turbine%edgelist(i), gdp)
                        df = df+guu(nf+i-1,mf)
                        turbine%reldist(i+1) = df
                    enddo
                elseif (normalflowdir=='V') then
                    do i = 1, nedges
                        if (i==1) then
                            turbine%reldist(1) = df
                        endif
                        call n_and_m_to_nm(nf, mf+i-1, turbine%edgelist(i), gdp)
                        df = df+gvv(nf,mf+i-1)
                        turbine%reldist(i+1) = df
                    enddo
                endif
            endif
        enddo
    endif
end subroutine mapturbine


subroutine updturbine(turbines, dzu, dzv, dpu, dpv, hu, hv, zw, thick, &
                    & u, v, alfas, hdt, nmaxddb, gdp)
!!--description-----------------------------------------------------------------
!
! Apply turbines
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use mathconsts
    use turbine_module, only: structure_turbines, structure_turbine, turbinecurve, intersect_turbine
    !
    implicit none
!
! Call variables
!
    type(globdat)                                              , target        :: gdp
    type(structure_turbines)                                   , intent(inout) :: turbines
    real(fp)                                                   , intent(in)    :: hdt
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: alfas
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: dzu
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: dzv
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: dpu
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: dpv
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: hu
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: hv
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: zw
    real(fp)     , dimension(gdp%d%kmax)                       , intent(in)    :: thick
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: u
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: v
    integer                                                    , intent(in)    :: nmaxddb
!
! Local variables
!
    integer                                            :: j
    integer                                            :: k
    integer                                            :: n
    integer                                            :: nm
    real(fp)                                           :: rhow
    real(fp)                                           :: unm1
    real(fp)                                           :: unm2
    real(fp)                                           :: uref
    real(fp)                                           :: z0
    character(256)                                     :: errorstring
    type(structure_turbine)                  , pointer :: turbine
    !
    integer, pointer                                   :: kmax
    integer, pointer                                   :: lundia
    logical, pointer                                   :: zmodel
!
!! executable statements -------------------------------------------------------
!
    if (associated(turbines%nr)) then
        kmax   => gdp%d%kmax
        lundia => gdp%gdinout%lundia
        zmodel => gdp%gdprocs%zmodel
        do j = 1, size(turbines%nr)
            turbine => turbines%nr(j)
            !
            ! determine blockage
            !
            select case(turbine%edgetype)
            case ('U')
                call compute_zlevel(turbine, dzu, dpu, hu, thick, gdp)
            case ('V')
                call compute_zlevel(turbine, dzv, dpv, hv, thick, gdp)
            end select
            !
            if (turbine%vertpos==1) then
                ! floating at fixed depth
                z0 = zw(turbine%cellnr) - turbine%xyz(3)
            else !if (turbine%vertpos==0) then
                ! fixed z level
                z0 = turbine%xyz(3)
            endif
            call intersect_turbine(turbine%reldist,turbine%zlevel,z0,turbine%diam,turbine%width,turbine%height,turbine%turbtype,turbine%area,turbine%blockfrac)
            !
            ! determine characteristic velocity
            !
            call compute_unorm(dzu, dzv, dpu, dpv, hu, hv, alfas, thick, u, v, turbine%cellu(1), z0, turbine%angle, unm1, gdp)
            call compute_unorm(dzu, dzv, dpu, dpv, hu, hv, alfas, thick, u, v, turbine%cellu(2), z0, turbine%angle, unm2, gdp)
            if (unm1>0.0_fp .and. unm2>0.0_fp) then ! flow from primary side
               uref = unm1
            elseif (unm1<0.0_fp .and. unm2<0.0_fp) then ! flow from reverse side
               uref = unm2
            else ! transitional flow
               uref = 0.5_fp * (unm1 + unm2)
            endif
            !
            ! determine thrust and power coefficients
            !
            rhow = 1000.0_fp
            turbine%current_zlevel = z0
            turbine%current_uref   = uref
            !
            turbine%thrustcoef = turbinecurve(turbines%curves, turbine%thrustcrvnr, uref, errorstring)
            !
            ! analytical thrust
            !
            turbine%current_thrust = 0.5_fp * turbine%thrustcoef * rhow * turbine%turbarea * uref**2
            turbine%cumul_thrust   = turbine%cumul_thrust + hdt*turbine%current_thrust
            !
            ! analytical power
            !
            if (turbine%powercrvnr(1)>0 .and. errorstring == '  ') then
                turbine%powercoef      = turbinecurve(turbines%curves, turbine%powercrvnr, uref, errorstring)
                turbine%current_power  = 0.5_fp * turbine%powercoef * rhow * turbine%turbarea * abs(uref**3)
                turbine%cumul_power    = turbine%cumul_power + hdt*turbine%current_power
            else
                turbine%powercoef      = -999.0_fp
                turbine%current_power  = -999.0_fp
                turbine%cumul_power    = -999.0_fp
            endif
            if (errorstring /= ' ') then
                write(lundia,'(A)') trim(errorstring)
                call d3stop(1, gdp)
            endif
            !
            ! friction coefficient set by add_loss_due_to_turbines calls below
        enddo
        call add_loss_due_to_turbines(turbines, v, u, 1, nmaxddb, 4, gdp)
        call add_loss_due_to_turbines(turbines, u, v, nmaxddb, 1, 4, gdp)
    endif
end subroutine updturbine


subroutine updturbinethrust(turbines, u0, u1, v0, v1, gvu, guv, bbk, nmaxddb, hdt, gdp)
!!--description-----------------------------------------------------------------
!
! Update curent and cumulative simulated thrust values
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use turbine_module, only: structure_turbines
    !
    implicit none
!
! Call variables
!
    type(globdat)                                              , target        :: gdp
    type(structure_turbines)                                   , intent(inout) :: turbines
    integer                                                    , intent(in)    :: nmaxddb
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(inout) :: bbk
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: u0
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: u1
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: v0
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: v1
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: gvu
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: guv
    real(fp)                                                   , intent(in)    :: hdt
!
! Local variables
!
!   NONE
!
!! executable statements -------------------------------------------------------
!
    call add_loss_due_to_turbines(turbines, v0, u0, 1, nmaxddb, 3, gdp, gvu=guv, u1=v1, hdt=hdt)
    call add_loss_due_to_turbines(turbines, u0, v0, nmaxddb, 1, 3, gdp, gvu=gvu, u1=u1, hdt=hdt)
end subroutine updturbinethrust


subroutine applyturbines(turbines, u0, v, gvu, icx, icy, mom_output, bbk, u1, gdp)
!!--description-----------------------------------------------------------------
!
! Add flow resistance / energy extraction term due to turbines (to bbk or to momentum output)
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use turbine_module, only: structure_turbines, structure_turbine
    !
    implicit none
!
! Call variables
!
    type(globdat)                                              , target        :: gdp
    type(structure_turbines)                                   , intent(inout) :: turbines
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: u0
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: u1
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: v
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: gvu
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(inout) :: bbk
    integer                                                    , intent(in)    :: icx
    integer                                                    , intent(in)    :: icy
    logical                                                    , intent(in)    :: mom_output
!
! Local variables
!
!   NONE
!
!! executable statements -------------------------------------------------------
!
    if (mom_output) then
        call add_loss_due_to_turbines(gdp%turbines, u0, v, icx, icy, 2, gdp, gvu=gvu, u1=u1)
    else
        call add_loss_due_to_turbines(gdp%turbines, u0, v, icx, icy, 1, gdp, gvu=gvu, bbk=bbk)
    endif
end subroutine applyturbines


subroutine add_loss_due_to_turbines(turbines, u0, v, icx, icy, option, gdp, gvu, bbk, u1, hdt)
!!--description-----------------------------------------------------------------
!
! Add or compute flow resistance / energy extraction term due to turbines
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use turbine_module, only: structure_turbines, structure_turbine
    !
    implicit none
!
! Call variables
!
    type(globdat)                                              , target        :: gdp
    type(structure_turbines)                                   , intent(inout) :: turbines
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: u0
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: v
    integer                                                    , intent(in)    :: icx
    integer                                                    , intent(in)    :: icy
    integer                                                    , intent(in)    :: option
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)   , optional :: u1
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)   , optional :: gvu
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(inout), optional :: bbk
    real(fp)                                                   , intent(in)   , optional :: hdt
!
! Local variables
!
    integer                                            :: j
    integer                                            :: k
    integer                                            :: n
    integer                                            :: ndm
    integer                                            :: ndmu
    integer                                            :: nm
    integer                                            :: nmu
    real(fp)                                           :: friccoef
    real(fp)                                           :: rhow
    real(fp)                                           :: sum_au2
    real(fp)                                           :: uuu
    real(fp)                                           :: vvv
    type(structure_turbine)                  , pointer :: turbine
    !
    integer                                  , pointer :: kmax
    integer                                  , pointer :: lundia
    logical                                  , pointer :: zmodel
    real(fp), dimension(:,:)                 , pointer :: mom_m_struct        ! structure momentum term
!
!! executable statements -------------------------------------------------------
!
    if (associated(turbines%nr)) then
        kmax   => gdp%d%kmax
        rhow   = 1000.0_fp
        !
        ! perform some basic code consistency testing
        !
        select case (option)
        case (1)
            if (.not.present(gvu) .or. .not.present(bbk)) then
                write(*,*) 'add_loss_due_to_turbines error 1'
                stop
            endif
        case (2)
            if (.not.present(gvu) .or. .not.present(u1)) then
                write(*,*) 'add_loss_due_to_turbines error 2'
                stop
            endif
            if (icx==1) then ! solve V/N component
                mom_m_struct     => gdp%gdflwpar%mom_n_struct
            else ! solve U/M component
                mom_m_struct     => gdp%gdflwpar%mom_m_struct
            endif
        case (3)
            if (.not.present(hdt) .or. .not.present(u1)) then
                write(*,*) 'add_loss_due_to_turbines error 3'
                stop
            endif
        case (4)
            ! NONE of the optional arguments needed
        case default
            write(*,*) 'add_loss_due_to_turbines error 5'
            stop
        end select
        !
        do j = 1, size(turbines%nr)
            turbine => turbines%nr(j)
            !
            ! add terms ... ubrlsu*|u| added to bbk
            !
            if ((icx/=1 .and. turbine%edgetype=='U') .or. (icx==1 .and. turbine%edgetype=='V')) then
                friccoef = turbine%friccoef
                if (turbine%turbinemodel==1) then
                  ! ccc - modify the definition of friccoef here -- this is the Roc model
                  friccoef = 2.0_fp*friccoef  ! remove the 1/2 from friccoef
                  friccoef = 4.0_fp*(1.0_fp - sqrt( 1.0_fp - friccoef) ) / (1.0_fp + sqrt( 1.0_fp - friccoef) )
                  friccoef = 0.5_fp*friccoef  ! replace the 1/2 in friccoef
                endif

                if (option==3) then
                    turbine%current_sim_thrust = 0.0_fp
                elseif (option==4) then
                    sum_au2 = 0.0_fp
                endif
                !
                do n = 1,size(turbine%edgelist)
                    nm = turbine%edgelist(n)
                    !
                    ndm  = nm - icy
                    nmu  = nm + icx
                    ndmu = nm - icy + icx
                    do k = 1, kmax
                        !vvv = .25*(v(ndm, k) + v(ndmu, k) + v(nm, k) + v(nmu, k))
                        !uuu = sqrt(u0(nm, k)**2 + vvv**2)
                        uuu = abs(u0(nm, k))
                        select case (option)
                        case (1)
                            bbk(nm, k) = bbk(nm, k) + uuu*friccoef*turbine%blockfrac(n, k)/gvu(nm)
                        case (2)
                            mom_m_struct(nm, k) = mom_m_struct(nm, k) - uuu*u1(nm, k)*friccoef*turbine%blockfrac(n, k)/gvu(nm)
                        case (3)
                            turbine%current_sim_thrust = turbine%current_sim_thrust + uuu*abs(u1(nm, k))*friccoef*turbine%blockfrac(n, k)*turbine%area(n, k)*rhow
                        case (4)
                            sum_au2 = sum_au2 + uuu*abs(u0(nm, k))*turbine%area(n, k)*turbine%blockfrac(n, k)
                        end select
                    enddo
                enddo
                !
                if (option==3) then
                    turbine%cumul_sim_thrust   = turbine%cumul_sim_thrust + hdt*turbine%current_sim_thrust
                elseif (option==4) then
                    turbine%friccoef = 0.5_fp * turbine%thrustcoef
                    if(turbine%turbinemodel==0) then
                      ! ccc only use this for deltares model (not SNL model)
                      if (sum_au2>0.0_fp) turbine%friccoef = turbine%friccoef * min((turbine%turbarea * turbine%current_uref**2) / sum_au2, 1000.0_fp)
                    endif
                endif
            endif
        enddo
    endif
end subroutine add_loss_due_to_turbines


subroutine compute_zlevel(turbine, dzu, dpu, hu, thick, gdp)
!!--description-----------------------------------------------------------------
!
! Compute Z levels at turbine location
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use turbine_module, only: structure_turbine
    !
    implicit none
!
! Call variables
!
    type(globdat)                                              , target        :: gdp
    type(structure_turbine)                                    , intent(inout) :: turbine
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: dzu
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: dpu
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: hu
    real(fp)     , dimension(gdp%d%kmax)                       , intent(in)    :: thick
!
! Local variables
!
    integer                                            :: k
    integer                                            :: n
    integer                                            :: nm
    !
    integer, pointer                                   :: kmax
!
!! executable statements -------------------------------------------------------
!
    kmax => gdp%d%kmax
    if (gdp%gdprocs%zmodel) then
        do n = 1,size(turbine%edgelist)
            nm = turbine%edgelist(n)
            turbine%zlevel(n,0) = -dpu(nm)
            do k = 1, kmax
                turbine%zlevel(n,k) = turbine%zlevel(n,k-1) + dzu(nm,k)
            enddo
        enddo
    else
        do n = 1,size(turbine%edgelist)
            nm = turbine%edgelist(n)
            turbine%zlevel(n,kmax) = -dpu(nm)
            do k = kmax, 1, -1
                turbine%zlevel(n,k-1) = turbine%zlevel(n,k) + hu(nm)*thick(k)
            enddo
        enddo
    endif
end subroutine compute_zlevel


subroutine compute_unorm(dzu, dzv, dpu, dpv, hu, hv, alfas, thick, u, v, &
                     & nm, z0, turang, unorm, gdp)
!!--description-----------------------------------------------------------------
!
! Compute velocity at specific location (hor=nm, vert=z0)
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use mathconsts
    use turbine_module, only: structure_turbine
    !
    implicit none
!
! Call variables
!
    type(globdat)                                              , target        :: gdp
    integer                                                    , intent(in)    :: nm
    real(fp)                                                   , intent(in)    :: turang
    real(fp)                                                   , intent(in)    :: z0
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: alfas
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: dzu
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: dzv
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: dpu
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: dpv
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: hu
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: hv
    real(fp)     , dimension(gdp%d%kmax)                       , intent(in)    :: thick
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: u
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: v
    real(fp)                                                   , intent(out)   :: unorm
!
! Local variables
!
    integer                                            :: nmd
    integer                                            :: ndm
    real(fp)                                           :: rads
    real(fp)                                           :: radt
    real(fp)                                           :: u1
    real(fp)                                           :: u2
    real(fp)                                           :: v1
    real(fp)                                           :: v2
    !
    integer, pointer                                   :: kmax
!
!! executable statements -------------------------------------------------------
!
    kmax => gdp%d%kmax
    !
    ! M and N components at velocity points
    !
    call compute_velnm(dzu, dpu, hu, thick, u, nm, z0, u1, gdp)
    call compute_velnm(dzv, dpv, hv, thick, v, nm, z0, v1, gdp)
    nmd = nm - gdp%d%nmax - 2*gdp%d%ddbound
    call compute_velnm(dzu, dpu, hu, thick, u, nmd, z0, u2, gdp)
    ndm = nm - 1
    call compute_velnm(dzv, dpv, hv, thick, v, ndm, z0, v2, gdp)
    !
    ! M and N components at cell centre
    !
    u1 = 0.5_fp * (u1+u2)
    v1 = 0.5_fp * (v1+v2)
    !
    ! X and Y components at cell centre
    !
    rads = alfas(nm)*degrad
    radt = turang*degrad
    u2 = u1*cos(rads) - v1*sin(rads)
    v2 = u1*sin(rads) + v1*cos(rads)
    !
    ! component normal to turbine
    !
    unorm = - u2*cos(radt) - v2*sin(radt)
end subroutine compute_unorm


subroutine compute_velnm(dzu, dpu, hu, thick, u, nm, z0, unm, gdp)
!!--description-----------------------------------------------------------------
!
! Compute velocity at specific u (or v) point and specific elevation
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use turbine_module, only: structure_turbine
    !
    implicit none
!
! Call variables
!
    type(globdat)                                              , target        :: gdp
    integer                                                    , intent(in)    :: nm
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: dzu
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: dpu
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)    :: hu
    real(fp)     , dimension(gdp%d%kmax)                       , intent(in)    :: thick
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, gdp%d%kmax), intent(in)    :: u
    real(fp)                                                   , intent(in)    :: z0
    real(fp)                                                   , intent(out)   :: unm
!
! Local variables
!
    integer                                            :: k
    real(fp)                                           :: zlow
    real(fp)                                           :: zup
    !
    integer, pointer                                   :: kmax
!
!! executable statements -------------------------------------------------------
!
    kmax => gdp%d%kmax
    !
    zup = -dpu(nm)
    if (gdp%gdprocs%zmodel) then
       do k = 1, kmax
          zlow = zup
          zup  = zlow + dzu(nm,k)
          if (z0<=zup) then
              unm = u(nm,k)
              exit
          endif
       enddo
    else
       do k = kmax, 1, -1
          zlow = zup
          zup  = zlow + hu(nm)*thick(k)
          if (z0<=zup) then
              unm = u(nm,k)
              exit
          endif
       enddo
    endif
end subroutine compute_velnm

end module m_rdturbine
