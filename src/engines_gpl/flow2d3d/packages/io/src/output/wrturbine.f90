module m_wrturbine
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
!  $Id: wrturbine.f90 5750 2016-01-20 17:22:01Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/engines_gpl/flow2d3d/packages/io/src/output/wrturbine.f90 $
!!--declarations----------------------------------------------------------------
    implicit none
    
contains

subroutine addturbine_cnst(gdp, lundia, grnam2)
!!--description-----------------------------------------------------------------
!
! Define elements of constants group
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use datagroups, only: getfiletype, adddim, addelm
    !
    implicit none
!
! Call variables
!
    type(globdat)                                              , target        :: gdp
    integer                                                    , intent(in)    :: lundia
    character(16)                                              , intent(in)    :: grnam2      ! Data-group name defined for the NEFIS-files 
!
! Local variables
!
    integer                                           :: filetype
    integer                                           :: iddim_2
    integer                                           :: iddim_nturb
    integer                                           :: nturb
!
!! executable statements -------------------------------------------------------
!
    nturb = size(gdp%turbines%nr)
    if (nturb==0) return
    !
    filetype = getfiletype(gdp, FILOUT_HIS)
    iddim_nturb    = adddim(gdp, lundia, FILOUT_HIS, 'NTURBINES'         , nturb   )
    iddim_2        = adddim(gdp, lundia, FILOUT_HIS, 'length_2'          , 2       )
    if (filetype == FTYPE_NEFIS) then ! for NEFIS only
        call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'NTURBINES', ' ', IO_INT4 , 0, longname='number of turbines' )
    endif
    call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'NAMTURBINES', ' ', 256       , 1, dimids=(/iddim_nturb/), longname='turbine name' )
    call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'ANGTURBINES', ' ', IO_REAL4  , 1, dimids=(/iddim_nturb/), longname='orientation of turbine axis', unit='arc_degrees' )
    call addelm(gdp, lundia, FILOUT_HIS, grnam2, 'XYTURBINES', ' ', IO_REAL4   , 2, dimids=(/iddim_nturb, iddim_2/), longname='horizontal turbine location', unit='m' )
end subroutine addturbine_cnst


function wrturbine_cnst(gdp, lundia, grnam2, fds, filename) result (ierror)
!!--description-----------------------------------------------------------------
!
! Write NEFIS elements of constants group
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use datagroups, only: getfiletype
    use wrtarray, only: wrtvar
    !
    implicit none
!
! Call variables
!
    type(globdat)                                              , target        :: gdp
    integer                                                    , intent(in)    :: lundia
    character(16)                                              , intent(in)    :: grnam2      ! Data-group name defined for the NEFIS-files 
    character(*)                                               , intent(in)    :: filename    ! File name
    integer                                                    , intent(in)    :: fds         ! File handle of output NEFIS/NetCDF file
    integer                                                                    :: ierror
!
! Local variables
!
    integer                                           :: filetype
    integer                                           :: i
    integer                                           :: nturb
    real(fp), dimension(:), allocatable               :: rbuff1
    real(fp), dimension(:,:), allocatable             :: rbuff2
    character(256), dimension(:), allocatable         :: cbuff
!
!! executable statements -------------------------------------------------------
!
    nturb = size(gdp%turbines%nr)
    ierror = 0
    if (nturb==0) return
    !
    filetype = getfiletype(gdp, FILOUT_HIS)
    allocate(cbuff(nturb),rbuff1(nturb),rbuff2(nturb,2))
    !
    do i = 1,nturb
        cbuff(i)    = gdp%turbines%nr(i)%name
        rbuff1(1)   = gdp%turbines%nr(i)%angle
        rbuff2(i,1) = gdp%turbines%nr(i)%xyz(1)
        rbuff2(i,2) = gdp%turbines%nr(i)%xyz(2)
    enddo
    !
    if (filetype == FTYPE_NEFIS) then ! for NEFIS only
        call wrtvar(fds, filename, filetype, grnam2, 1, &
                  & gdp, ierror, lundia, nturb, 'NTURBINES')
        if (ierror/=0) goto 999
    endif
    !
    call wrtvar(fds, filename, filetype, grnam2, 1, &
              & gdp, ierror, lundia, cbuff, 'NAMTURBINES')
    if (ierror/=0) goto 999
    !
    call wrtvar(fds, filename, filetype, grnam2, 1, &
              & gdp, ierror, lundia, rbuff1, 'ANGTURBINES')
    if (ierror/=0) goto 999
    !
    call wrtvar(fds, filename, filetype, grnam2, 1, &
              & gdp, ierror, lundia, rbuff2, 'XYTURBINES')
    if (ierror/=0) goto 999
    !
999 continue
    deallocate(cbuff, rbuff1, rbuff2)
end function wrturbine_cnst


subroutine addturbine_time(gdp, lundia, grnam3)
!!--description-----------------------------------------------------------------
!
! Define NEFIS elements of time-varying group
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use datagroups, only: getfiletype, adddim, addelm
    !
    implicit none
!
! Call variables
!
    type(globdat)                                              , target        :: gdp
    integer                                                    , intent(in)    :: lundia
    character(16)                                              , intent(in)    :: grnam3      ! Data-group name defined for the NEFIS-files 
!
! Local variables
!
    integer                                           :: filetype
    integer                                           :: iddim_nturb
    integer                                           :: nturb
!
!! executable statements -------------------------------------------------------
!
    nturb = size(gdp%turbines%nr)
    if (nturb==0) return
    !
    filetype = getfiletype(gdp, FILOUT_HIS)
    iddim_nturb    = adddim(gdp, lundia, FILOUT_HIS, 'NTURBINES'         , nturb   )
    call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'ZTURBINES', ' ', IO_REAL4      , 1, dimids=(/iddim_nturb/), longname='vertical turbine location', unit='m' )
    call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'UTURBINES', ' ', IO_REAL4      , 1, dimids=(/iddim_nturb/), longname='turbine reference velocity', unit='m/s' )
    call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'POWERCOEF', ' ', IO_REAL4      , 1, dimids=(/iddim_nturb/), longname='power coefficient' )
    call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'THRUSTCOEF', ' ', IO_REAL4     , 1, dimids=(/iddim_nturb/), longname='thrust coefficient' )
    call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'LOSSCOEF', ' ', IO_REAL4       , 1, dimids=(/iddim_nturb/), longname='loss coefficient' )
    call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'INST_SIMTHRUST', ' ', IO_REAL4 , 1, dimids=(/iddim_nturb/), longname='instantaneous simulated thrust of turbine', unit='N' )
    call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'CUM_SIMTHRUST', ' ', IO_REAL4  , 1, dimids=(/iddim_nturb/), longname='cumulative simulated thrust of turbine', unit='N s' )
    call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'INST_THRUST', ' ', IO_REAL4    , 1, dimids=(/iddim_nturb/), longname='instantaneous analytical thrust of turbine', unit='N' )
    call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'CUM_THRUST', ' ', IO_REAL4     , 1, dimids=(/iddim_nturb/), longname='cumulative analytical thrust of turbine', unit='N s' )
    call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'INST_POWER', ' ', IO_REAL4     , 1, dimids=(/iddim_nturb/), longname='instantaneous analytical power of turbine', unit='W' )
    call addelm(gdp, lundia, FILOUT_HIS, grnam3, 'CUM_POWER', ' ', IO_REAL4      , 1, dimids=(/iddim_nturb/), longname='cumulative analytical power of turbine', unit='W s' )
end subroutine addturbine_time


function wrturbine_time(gdp, lundia, grnam3, fds, filename, celidt) result (ierror)
!!--description-----------------------------------------------------------------
!
! Write NEFIS elements of time-varying group
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use datagroups, only: getfiletype
    use wrtarray, only: wrtvar
    !
    implicit none
!
! Call variables
!
    type(globdat)                                              , target        :: gdp
    integer                                                    , intent(in)    :: celidt
    integer                                                    , intent(in)    :: lundia
    character(16)                                              , intent(in)    :: grnam3      ! Data-group name defined for the NEFIS-files 
    character(*)                                               , intent(in)    :: filename    ! File name
    integer                                                    , intent(in)    :: fds         ! File handle of output NEFIS/NetCDF file
    integer                                                                    :: ierror
!
! Local variables
!
    integer                                           :: filetype
    integer                                           :: i
    integer                                           :: nturb
    real(fp), dimension(:), allocatable               :: rbuff1
!
!! executable statements -------------------------------------------------------
!
    nturb = size(gdp%turbines%nr)
    ierror = 0
    if (nturb==0) return
    !
    allocate(rbuff1(nturb))
    filetype = getfiletype(gdp, FILOUT_HIS)
    !
    do i = 1,nturb
        rbuff1(i) = gdp%turbines%nr(i)%current_zlevel
        write(lundia,*) 'Writing zlevel[',i,'] = ',rbuff1(i)
    enddo    
    call wrtvar(fds, filename, filetype, grnam3, celidt, &
              & gdp, ierror, lundia, rbuff1, 'ZTURBINES')
    if (ierror/=0) goto 999
    !
    do i = 1,nturb
        rbuff1(i) = gdp%turbines%nr(i)%current_uref
        write(lundia,*) 'Writing uref[',i,'] = ',rbuff1(i)
    enddo    
    call wrtvar(fds, filename, filetype, grnam3, celidt, &
              & gdp, ierror, lundia, rbuff1, 'UTURBINES')
    if (ierror/=0) goto 999
    !
    do i = 1,nturb
        rbuff1(i) = gdp%turbines%nr(i)%powercoef
    enddo    
    call wrtvar(fds, filename, filetype, grnam3, celidt, &
              & gdp, ierror, lundia, rbuff1, 'POWERCOEF')
    if (ierror/=0) goto 999
    !
    do i = 1,nturb
        rbuff1(i) = gdp%turbines%nr(i)%thrustcoef
    enddo    
    call wrtvar(fds, filename, filetype, grnam3, celidt, &
              & gdp, ierror, lundia, rbuff1, 'THRUSTCOEF')
    if (ierror/=0) goto 999
    !
    do i = 1,nturb
        rbuff1(i) = gdp%turbines%nr(i)%friccoef
    enddo    
    call wrtvar(fds, filename, filetype, grnam3, celidt, &
              & gdp, ierror, lundia, rbuff1, 'LOSSCOEF')
    if (ierror/=0) goto 999
    !
    do i = 1,nturb
        rbuff1(i) = gdp%turbines%nr(i)%current_sim_thrust
    enddo    
    call wrtvar(fds, filename, filetype, grnam3, celidt, &
              & gdp, ierror, lundia, rbuff1, 'INST_SIMTHRUST')
    if (ierror/=0) goto 999
    !
    do i = 1,nturb
        rbuff1(i) = gdp%turbines%nr(i)%cumul_sim_thrust
    enddo    
    call wrtvar(fds, filename, filetype, grnam3, celidt, &
              & gdp, ierror, lundia, rbuff1, 'CUM_SIMTHRUST')
    if (ierror/=0) goto 999
    !
    do i = 1,nturb
        rbuff1(i) = gdp%turbines%nr(i)%current_thrust
    enddo    
    call wrtvar(fds, filename, filetype, grnam3, celidt, &
              & gdp, ierror, lundia, rbuff1, 'INST_THRUST')
    if (ierror/=0) goto 999
    !
    do i = 1,nturb
        rbuff1(i) = gdp%turbines%nr(i)%cumul_thrust
    enddo    
    call wrtvar(fds, filename, filetype, grnam3, celidt, &
              & gdp, ierror, lundia, rbuff1, 'CUM_THRUST')
    if (ierror/=0) goto 999
    !
    do i = 1,nturb
        rbuff1(i) = gdp%turbines%nr(i)%current_power
    enddo    
    call wrtvar(fds, filename, filetype, grnam3, celidt, &
              & gdp, ierror, lundia, rbuff1, 'INST_POWER')
    if (ierror/=0) goto 999
    !
    do i = 1,nturb
        rbuff1(i) = gdp%turbines%nr(i)%cumul_power
    enddo    
    call wrtvar(fds, filename, filetype, grnam3, celidt, &
              & gdp, ierror, lundia, rbuff1, 'CUM_POWER')
    if (ierror/=0) goto 999
    !
999 continue
    deallocate(rbuff1)
end function wrturbine_time

end module m_wrturbine