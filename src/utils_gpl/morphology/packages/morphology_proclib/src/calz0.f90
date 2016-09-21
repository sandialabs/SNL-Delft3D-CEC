      subroutine calz0 ( pmsa   , fl     , ipoint , increm , noseg  , &
     &                   noflux , iexpnt , iknmrk , noq1   , noq2   , &
     &                   noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'CALZ0' :: CALZ0
!>\file
!>       Process: Z0 - Compute z0 coefficient from Chezy

!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2015.                                
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
!------------------------------------------------------------------------------
      IMPLICIT NONE
!
!     Type                      Name          I/O Description
!
      real(4), dimension(*)  :: pmsa        !<I/O Process Manager System Array, window of routine to process library
      real(4), dimension(*)  :: fl          !< O  Array of fluxes made by this process in mass/volume/time
      integer, dimension(6)  :: ipoint      !< I  Array of pointers in pmsa to get and store the data
      integer, dimension(6)  :: increm      !< I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer                :: noseg       !< I  Number of computational elements in the whole model schematisation
      integer                :: noflux      !< I  Number of fluxes, increment in the fl array
      integer, dimension(4,*):: iexpnt      !< I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer, dimension(*)  :: iknmrk      !< I  Active-Inactive, Surface-water-bottom, see manual for use
      integer                :: noq1        !< I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer                :: noq2        !< I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer                :: noq3        !< I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer                :: noq4        !< I  Nr of exchanges in the bottom (bottom layers, specialist use only)
!
!------------------------------------------------------------------------------
!
!     Type                                  Name          I/O Description                                        Unit
!
      real(4)                            :: CHEZY       !< I  Chezy coefficient                                  (m0.5/s)
      real(4)                            :: TotalDepth  !< I  total depth water column                           (m)
      real(4)                            :: VonKarman   !< I  Von Karman constant                                (-)
      real(4)                            :: GRAV        !< I  Gravitational acceleration                         (m/s2)
      real(4)                            :: z0cur       !< O  current related bed roughness z_0 height           (m)
      real(4)                            :: z0rou       !< O  wave enhanced bed roughness z_0 height             (m)
!
      integer                            :: iflux       !<    Local index for pointering the fluxes
      integer, dimension(6)              :: ipnt        !<    Local work array for the pointering
      integer                            :: iseg        !<    Local loop counter for computational element loop
!
      real(4)                            :: ee          !<    Euler's number e (computed using exp(1.0))
      real(4)                            :: sqrtGRAV    !<    Sqrt of gravitational acceleration
!
!------------------------------------------------------------------------------
!
!     Initialise pointers
!
      ipnt        = ipoint
      iflux       = 0
!
      ee          = exp(1.0)
      GRAV        = pmsa( ipnt(  4) )
      sqrtGRAV    = sqrt(GRAV)
!
!     Loop over all segments
!
      do iseg = 1, noseg
!
!        Get input values from arrays
!
         CHEZY       = pmsa( ipnt(  1) )
         TotalDepth  = pmsa( ipnt(  2) )
         VonKarman   = pmsa( ipnt(  3) )
!
!   *****     Insert your code here  *****
!
         z0cur = TotalDepth/(ee*(exp(VonKarman*CHEZY/sqrtGRAV) - 1.0))
         z0rou = z0cur ! for the time being both numbers are assumed equal (no waves)
!
!   *****     End of your code       *****
!
!        Put output and flux values into arrays
!
         pmsa( ipnt(  5)   ) = z0cur
         pmsa( ipnt(  6)   ) = z0rou
!
!        Increment pointers
!
         iflux       = iflux       + noflux
         ipnt        = ipnt        + increm
!
      enddo
!
      end subroutine calz0
