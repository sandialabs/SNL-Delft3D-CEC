module m_temperature
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
!  $Id: temperature.f90 65778 2020-01-14 14:07:42Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/utils_gpl/flow1d/packages/flow1d_core/src/temperature.f90 $
!-------------------------------------------------------------------------------

   use messagehandling 

   implicit none
   private
   
   public heat_exchange
   public default_heatfluxes
   public set_par_temperature
   
   integer, parameter, public :: HEAT_TRANSPORT = 1
   integer, parameter, public :: HEAT_EXCESS    = 3
   integer, parameter, public :: HEAT_COMPOSITE = 5
   
   type :: t_temperature_pars
      integer          :: heat_model      
      ! variables for heat model
      double precision :: alfa_albedo                !< reflection coefficient of water () at average incidence angle of 60 deg,
                                                !< (albedo is .025 at angle 0 deg, 0.13 at angle 70 deg)
      double precision :: S_solar               !< Specific heat air   [J/kg/K]
      double precision :: eps_emissivity        !< Emissivity ()
      double precision :: sigma_stefan          !< Stefan's constant =5.67e-8 [W/m^2/K^4]
      double precision :: R_dry                 !< gas constant for dry air
      double precision :: R_vap                 !< gas constant for water vapour
      double precision :: c_pw                  !< specific heat capacity of sea water
      double precision :: c_pa                  !< specific heat capacity of air
      double precision :: p_atm                 !< atmospheric pressure
      double precision :: c_e_dalton            !< dalton number
      double precision :: c_h_stanton           !< stanton number
      double precision :: s_area                !< exposed water surface area
      double precision :: F_cloud               !< Cloud coverage factor [-]
      double precision :: T_air                 !< background temperature
      double precision :: r_hum                 !< air humidity
      double precision :: c_frconv              !< coefficient of free convection
      double precision :: nu_air                !< air viscosity
      double precision :: sigma_prandtl         !< prandtl number
      double precision :: tkelvn                !< T(Kelvin) = T(Celsius) + tkelvn
      double precision :: rhoair                !< rho air
   end type
   
   type(t_temperature_pars), target, public :: tempPars
   
   ! Temperature Output
   double precision, allocatable, public, target, dimension(:) :: q_tot       !< Total heat flux
   double precision, allocatable, public, target, dimension(:) :: q_sc        !< Radiation flux for clear sky condition
   double precision, allocatable, public, target, dimension(:) :: q_co        !< Heat loss due to convection
   double precision, allocatable, public, target, dimension(:) :: q_sn        !< Net incident solar radiation
   double precision, allocatable, public, target, dimension(:) :: q_eb        !< Effective back radiation
   double precision, allocatable, public, target, dimension(:) :: q_ev        !< Heat loss due to evaporation
   double precision, allocatable, public, target, dimension(:) :: q_evforced  !< Forced heat loss due to evaporation
   double precision, allocatable, public, target, dimension(:) :: q_evfree    !< Free heat loss due to evaporation
   double precision, allocatable, public, target, dimension(:) :: q_coforced  !< Heat loss due to forced convection
   double precision, allocatable, public, target, dimension(:) :: q_co_free   !< Heat loss due to free convection
   
   contains
   
   subroutine set_par_temperature(name, value)
      character(len=*), intent(in) :: name
      double precision, intent(in)     :: value
      
      select case(trim(name))
      case('S_solar')           
         tempPars%S_solar        = value
      case('eps_emissivity')    
         tempPars%eps_emissivity = value
      case('sigma_stefan')      
         tempPars%sigma_stefan   = value
      case('R_dry')             
         tempPars%R_dry          = value
      case('R_vap')             
         tempPars%R_vap          = value
      case('c_p')               
         tempPars%c_pw           = value
      case('p_atm')             
         tempPars%p_atm          = value/100d0
      case('c_e_dalton')        
         tempPars%c_e_dalton     = value
      case('c_h_stanton')       
         tempPars%c_h_stanton    = value
      case('s_area')           
         tempPars%s_area         = value
      case('cloudiness')           
         tempPars%F_cloud        = value/100d0
      case('air_temperature')             
         tempPars%T_air          = value
      case('humidity')             
         tempPars%r_hum          = value/100d0
      end select
         
   end subroutine set_par_temperature
      
   subroutine heat_exchange(temp, load, time, rhow, s1, dp, wind_speed, nod)
   
      use m_tables
      use m_GlobalParameters
      integer, intent(in) :: nod
      double precision, intent(in)            :: temp
      double precision, intent(inout)         :: load
      double precision, intent(in)            :: time
      double precision, intent(in)            :: rhow
      double precision, intent(in)            :: s1
      double precision, intent(in)            :: dp
      double precision, intent(in)            :: wind_speed
      
      double precision :: f_U10
      double precision :: g_U10
      double precision :: labda
      double precision :: f_Fc
      double precision :: e_a
      double precision :: e_s
      double precision :: rho_a10
      double precision :: rho_a0
      double precision :: k_s
      double precision :: alfa_diff
      double precision :: rho_avg
      double precision :: L_v 
      double precision :: Q_Ts
      double precision :: Q_Ta
      double precision :: rhomean
      
      type(t_temperature_pars), pointer :: tp
      tp => tempPars

      rhomean = rhow
      
      select case (tp%heat_model)
      case (HEAT_TRANSPORT)
         ! nothing to do
         q_tot(nod) = 0d0
      case (HEAT_EXCESS)
         f_U10 = (3.5d0 + 2d0*wind_speed) *(5d6/tp%s_area)**0.05d0
      
         labda = 4.48d0 + 0.049d0 * temp + f_U10 * (1.12d0 + 0.018d0*temp + 0.00158d0*temp**2)
         Q_tot(nod) = - labda * (temp - tp%T_air)
      case (HEAT_COMPOSITE) 
         
         ! calculate Q_sn: net incident solar radiation (short wave)
         f_Fc = 1d0 - 0.4d0 * tp%F_cloud - 0.38d0 * tp%F_cloud**2
         q_sc(nod) = qsun_nominal(time) 
         q_sn(nod) = (1d0-tp%alfa_albedo) * q_sc(nod) * f_fc
         
         ! calculate Q_eb effective back radiation
         e_a = tp%r_hum * get_e_temp(tp%T_air)
         e_s = get_e_temp(temp)
         q_eb(nod) = tp%eps_emissivity * tp%sigma_stefan * (temp+tp%tkelvn)**4 * (0.39d0 - 0.05d0 * sqrt(e_a)) * (1d0 - 0.6d0 * tp%F_cloud**2)
         
         ! calculate Q_ev evaporative heat flux
         rho_a10     = get_rho_air(tp%T_air, e_a)
         rho_a0      = get_rho_air(temp, e_s)
         alfa_diff   = tp%nu_air/tp%sigma_prandtl
         L_v         = 2.5d6 - 2.3d3 * temp
         Q_Ts        = get_q_t(e_s)
         Q_Ta        = get_q_t(e_a)
         f_U10       = tp%c_e_dalton * wind_speed
         rho_avg     = 0.5d0 * (rho_a10 + rho_a0)
         if (rho_a10 - rho_a0 <= 0d0) then
            k_s = 0d0
         else
            k_s = tp%c_frconv * (gravity * alfa_diff**2 / (tp%nu_air * rho_avg) * (rho_a10 - rho_a0) )**(1d0/3d0)
         endif
         q_evforced(nod)  = L_v * tp%rhoair * f_U10 * (q_Ts - q_Ta)
         q_evfree(nod)    = k_s * L_v * rho_avg * (q_Ts - q_Ta)
         q_ev(nod)        = q_evforced(nod) + q_evfree(nod)
         
         ! calculate q_co
         g_U10 = tp%c_h_stanton * wind_speed
         q_coforced(nod)  = tp%rhoair * tp%c_pa * g_U10 * (temp - tp%T_air)
         q_co_free(nod)   = k_s * rho_avg * tp%c_pa * (temp - tp%T_air)
         q_co(nod) = q_coforced(nod) + q_co_free(nod)
         
         q_tot(nod) = q_sn(nod) - q_eb(nod) - q_ev(nod) - q_co(nod)
      case default
         q_tot(nod) = 0d0
      end select
      load = q_tot(nod) /( rhomean * tp%c_pw * (s1+dp) )        !TODO TEMP: navragen bij Erik: Moet hier geen rekening worden gehouden met het dwarsprofiel? formule 9.214 Delft3d UM.
   end subroutine heat_exchange
   
   double precision function get_q_t(e_t)
      double precision, intent(in)  :: e_t
      
      get_q_t = 0.62d0 * e_t / (tempPars%p_atm - 0.38d0 * e_t)
   end function get_q_t
   
   double precision function get_rho_air(temp, e_t)
      double precision, intent(in) :: temp
      double precision, intent(in)  :: e_t
      
      get_rho_air = 100d0 * (tempPars%p_atm - e_t)/tempPars%R_dry + 100d0 * e_t/tempPars%R_vap
      get_rho_air = get_rho_air  / (temp+tempPars%tkelvn)
      
   end function get_rho_air
   
   double precision function get_e_temp(temp)
      double precision, intent(in) :: temp
   
      get_e_temp = 10d0**((0.7859d0+0.03477d0*temp)/(1d0+0.00412d0*temp))
   end function get_e_temp
   
double precision function qsun_nominal(time) 
   use m_globalParameters

   implicit none

   double precision, intent(in) :: time
   
   double precision :: decln, w0, w1, d, e, tm , snh

   ! Calculate sine of the angle of the sun above the horizon: SNH
   ! d is the declination angle
   ! June 21st is the 171st day after TM=0

   tm = time + 24.0d0*longitude/360.0d0 - time_zone
   w0     = 2d0*pi / (365.24d0*24d0)
   w1     = 2d0*pi / (24d0)
   decln  = 23.5d0*pi/180d0
   d      = decln * cos(w0*tm - 2.950d0)
   e      = latitude*pi/180d0
   snh    = -cos(e) * cos(d) * cos(w1*tm) + sin(e) * sin(d)
   snh    = max(0d0,min(1d0,snh))
   qsun_nominal     = tempPars%S_solar * snh *  0.76d0
end function qsun_nominal


   subroutine default_heatfluxes()
      tempPars%rhoair         = 1.205
      tempPars%alfa_albedo    = 0.06d0
      tempPars%S_solar        = 1368d0
      tempPars%eps_emissivity = 0.985d0
      tempPars%sigma_stefan   = 5.67d-8
      tempPars%R_dry          = 287.05d0
      tempPars%R_vap          = 461.495d0
      tempPars%c_pw           = 3930d0
      tempPars%c_pa           = 1004d0
      tempPars%p_atm          = 1013.25
      tempPars%c_e_dalton     = 0.0013d0
      tempPars%c_h_stanton    = 0.0013d0
      tempPars%s_area         = 1d6
      tempPars%F_cloud        = 0d0
      tempPars%T_air          = 15d0
      tempPars%r_hum          = 0.3d0
      tempPars%c_frconv       = 0.14d0
      tempPars%nu_air         = 16.0d-6
      tempPars%sigma_prandtl  = 0.7d0
      tempPars%tkelvn         = 273.15d0
   end subroutine default_heatfluxes
   

end module m_temperature
   