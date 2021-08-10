module m_cross_helper
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
!  $Id: cross_helper.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/cross_helper.f90 $
!-------------------------------------------------------------------------------

   use m_network
   use M_newcross
   use m_CrossSections
   use m_tables
   
   implicit none
   
   public getBobs
   public getConveyance
   public getCrossDischarge

   integer, public, parameter :: CSH_DEPTH = 0
   integer, public, parameter :: CSH_LEVEL  = 1
   
   
   private
   
contains

   function getbobs(network, ilink) result(res)
      type(t_network), intent(in) :: network
      integer, intent(in) :: ilink
      double precision, dimension(2) ::res
      
      type (t_CrossSection), pointer     :: cross1
      type (t_CrossSection), pointer     :: cross2 
      double precision :: dx
      double precision :: dxlocal
      double precision :: distancelocal
      double precision :: factor
      double precision :: linkpos

      if (network%adm%line2cross(ilink)%c1 < 0) then
         ! no cross section on this branch
         res = huge(1d0)
         return
      endif

      cross1 => network%crs%cross(network%adm%line2cross(ilink)%c1)
      cross2 => network%crs%cross(network%adm%line2cross(ilink)%c2)
      
      if (network%adm%line2cross(ilink)%c1 == network%adm%line2cross(ilink)%c2) then 
          res(1) = getBob(cross1)
          res(2) = res(1)
      else
          dxlocal = 0.5d0*getdeltax(network, ilink) 
          distancelocal = cross2%chainage - cross1%chainage
          dx = dxlocal/distancelocal
          linkpos = network%adm%line2cross(ilink)%f
          factor = linkpos - dx
          res(1) = getBob(cross1, cross2, factor)      
          factor = linkpos + dx
          res(2) = getBob(cross1, cross2, factor)      
      endif    
   end function getbobs   
   
   double precision function getdeltax(network, ilink)
      type(t_network), intent(in) :: network
      integer, intent(in) :: ilink
      
      integer :: ibr, ll
      
      ibr = network%adm%lin2ibr(ilink)
      ll  = network%adm%lin2local(ilink)
      getdeltax = network%brs%branch(ibr)%dx(ll)
   end function getdeltax
! =================================================================================================
! =================================================================================================
   subroutine getConveyance(network, dpt, u1L, q1L, s1L, L, perim_sub, flowarea_sub, conv, cz_sub, cz, flowArea, wetPerimeter)
      use m_CrossSections     , only: t_CSType, CS_TABULATED, CS_YZ_PROF
      
      implicit none
      type(t_network), intent(in)    :: network
      double precision               :: dpt, u1L, q1L, s1L
      double precision, intent(out)  :: conv
      integer                        :: i , L, n
      double precision, dimension(3), intent(in) :: flowarea_sub, perim_sub
      double precision, dimension(3), intent(out) :: cz_sub
      double precision, intent(out)  :: cz
      double precision, intent(in)   :: flowArea
      double precision, intent(in)   :: wetPerimeter

      double precision, parameter    :: eps = 1d-3               !< accuracy parameter for determining wetperimeter == 0d0
      double precision               :: r, cz1, cz2
      double precision               :: f
      integer                        :: igrid
      integer                        :: ibranch
      logical                        :: YZ_conveyance 
      double precision               :: chainage
      type(t_CSType), pointer        :: cross
      
      n = network%adm%line2cross(L)%c1
      if ( n <= 0) then
         ! no cross section defined on L
         conv = 45d0* flowarea_sub(1) * sqrt(flowarea_sub(1) / perim_sub(1))
         cz = 45d0
         return
      else
         ! for YZ profiles CalcCSParsFlow computes the conveyance
         yz_conveyance = network%crs%cross(n)%crosstype == CS_YZ_PROF
      endif
      
      if (yz_conveyance) then
         call getYZConveyance(network%adm%line2cross(L), network%crs%cross, dpt, u1L, cz, conv)

      else
         igrid   = network%adm%lin2grid(L)
         ibranch = network%adm%lin2ibr(L)
         chainage = network%brs%branch(ibranch)%uPointsChainages(network%adm%lin2local(L))
         conv = 0d0
         if (perim_sub(1)> 0.0d0) then
            do i = 1, 3
               if (perim_sub(i) > eps .and. flowarea_sub(i) > 0.0d0) then
                  r = flowarea_sub(i)/perim_sub(i)
                  cross => network%crs%cross(network%adm%line2cross(L)%c1)%tabdef
                  cz1 = getFrictionValue(network%rgs, network%spdata, cross, ibranch, i, igrid, s1L, q1L, u1L, r, dpt, chainage)
                  cross => network%crs%cross(network%adm%line2cross(L)%c2)%tabdef
                  cz2 = getFrictionValue(network%rgs, network%spdata, cross, ibranch, i, igrid, s1L, q1L, u1L, r, dpt, chainage)
                  ! Compute weighting of left and right cross section on this grid point.
                  ! Note: friction coefficient was already interpolated onto this grid point inside getFrictionValue.
                  f = network%adm%line2cross(L)%f
                  cz_sub(i) = (1.0d0 - f) * cz1     + f * cz2
                  conv = conv + cz_sub(i) * flowarea_sub(i) * sqrt(flowarea_sub(i) / perim_sub(i))
               else
                  cz_sub(i) = 0
               endif
            enddo
         endif
         ! compute average chezy 
         cz = conv/(flowArea*sqrt(flowArea/wetPerimeter))

      endif
      !        criteria to satisfy the criteria  in normup i.e cz(m)*cz(m)*wet
      if (cz * cz * flowArea < 1.0d0) then
         conv = sqrt(flowArea * flowArea / wetPerimeter)
      endif

   end subroutine getConveyance

   
   subroutine getCrossDischarge(perim_sub, flowarea_sub, cz_sub, q1_local, q_sub)
      ! Get discharges per reachsubsegment - based on plqsec() by J.Kuipers 
      implicit none
      double precision, dimension(3), intent(in) :: flowarea_sub, cz_sub, perim_sub
      double precision,               intent(in) :: q1_local
      double precision, dimension(3), intent(out) :: q_sub  
      double precision, dimension(3)             :: r_sub  
      integer                                    :: isec
      double precision                           :: acrtot
      double precision                           :: qacrtot
      double precision, parameter                :: eps_perim = 1d-3               ! accuracy wetted perimeter == 0d0
      double precision, parameter                :: eps_acrtot = 1d-10            ! accuracy weighted area parameter == 0d0
      double precision, parameter                :: eps_area = 1d-6               ! accuracy area == 0d0
      
      acrtot = 0.d0
      do isec = 1,3
          if (perim_sub(isec) > eps_perim) then
              r_sub(isec) = flowarea_sub(isec)/perim_sub(isec)
              acrtot = acrtot+flowarea_sub(isec)*cz_sub(isec) * sqrt(r_sub(isec))
          endif
      enddo
      if (acrtot > eps_acrtot) then
         qacrtot = q1_local/acrtot
         q_sub = 0.d0
         do isec = 1, 3
            if (flowarea_sub(isec) > eps_area) then
               q_sub(isec) = flowarea_sub(isec)*cz_sub(isec) *sqrt(r_sub(isec))*qacrtot
            endif
         enddo
      endif      
   end subroutine getCrossDischarge
   
   
end module m_cross_helper
