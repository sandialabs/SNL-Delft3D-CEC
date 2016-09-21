      subroutine kfsed ( pmsa   , fl     , ipoint , increm , noseg  , &
     &                   noflux , iexpnt , iknmrk , noq1   , noq2   , &
     &                   noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'KFSED' :: KFSED
!>\file
!>       Process: KfSed - Morphologically active segments

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
      integer, dimension(3)  :: ipoint      !< I  Array of pointers in pmsa to get and store the data
      integer, dimension(3)  :: increm      !< I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
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
      real(4)                            :: TotalDepth  !< I  total depth water column                           (m)
      real(4)                            :: SedThr      !< I  threshold depth for morphological activity         (m)
      real(4)                            :: KfSedVal    !< O  Morphological active segments                      (-)
      real(4)                            :: dum_dZB4    !< F  dummy flux to access KfSed                         (-)
!
      integer                            :: iflux       !<    Local index for pointering the fluxes
      integer, dimension(3)              :: ipnt        !<    Local work array for the pointering
      integer                            :: iseg        !<    Local loop counter for computational element loop
!
!------------------------------------------------------------------------------
!
!     Initialise pointers
!
      ipnt        = ipoint
      iflux       = 0
!
!     Loop over all segments
!
      do iseg = 1, noseg
!
!        Get input values from arrays
!
         TotalDepth  = pmsa( ipnt(  1) )
         SedThr      = pmsa( ipnt(  2) )
!
!   *****     Insert your code here  *****
!
         if (TotalDepth>SedThr) then
            KfSedVal = 1
         else
            KfSedVal = 0
         endif
         dum_dZB4    = 0.0
!
!   *****     End of your code       *****
!
!        Put output and flux values into arrays
!
         fl  ( iflux +  1  ) = dum_dZB4
         pmsa( ipnt(  3)   ) = KfSedVal
!
!        Increment pointers
!
         iflux       = iflux       + noflux
         ipnt        = ipnt        + increm
!
      enddo
!
      end subroutine kfsed
