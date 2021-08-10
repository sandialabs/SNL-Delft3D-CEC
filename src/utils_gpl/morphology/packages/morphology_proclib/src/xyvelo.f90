      subroutine xyvelo ( pmsa   , fl     , ipoint , increm , noseg  , &
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   , &
     &                    noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'XYVELO' :: XYVELO
!>\file
!>       Process: XYVeloc - horizontal flow velocity

!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2020.                                
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
      use precision
      !
      IMPLICIT NONE
!
!     Type                      Name          I/O Description
!
      real(4), dimension(*)  :: pmsa        !<I/O Process Manager System Array, window of routine to process library
      real(4), dimension(*)  :: fl          !< O  Array of fluxes made by this process in mass/volume/time
      integer, dimension(8)  :: ipoint      !< I  Array of pointers in pmsa to get and store the data
      integer, dimension(8)  :: increm      !< I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
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
      real(fp)                            :: XArea       !< I  exchange area                                      (m2)
      real(fp)                            :: Flow        !< I  flow rate                                          (m3/s)
      real(fp)                            :: nx_from     !< I  x-component of unit normal for FROM segment        (-)
      real(fp)                            :: ny_from     !< I  y-component of unit normal for FROM segment        (-)
      real(fp)                            :: nx_to       !< I  x-component of unit normal for TO segment          (-)
      real(fp)                            :: ny_to       !< I  y-component of unit normal for TO segment          (-)
      real(fp)                            :: Veloc1      !< O  horizontal flow velocity first direction           (m/s)
      real(fp)                            :: Veloc2      !< O  horizontal flow velocity second direction          (m/s)
!
      real(fp), dimension(:), allocatable :: a_nx_nx     !<    Temporary array to accumulate Area*nx^2 term
      real(fp), dimension(:), allocatable :: a_nx_ny     !<    Temporary array to accumulate Area*nx*ny term
      real(fp), dimension(:), allocatable :: a_ny_ny     !<    Temporary array to accumulate Area*ny^2 term
      real(fp), dimension(:), allocatable :: f_nx        !<    Temporary array to accumulate Flow*nx term
      real(fp), dimension(:), allocatable :: f_ny        !<    Temporary array to accumulate Flow*ny term
      real(fp), dimension(:), allocatable :: cuma        !<    Temporary array to accumulate Area term
      real(fp)                            :: Omega       !<    Help variable
      integer                             :: iflux       !<    Local index for pointering the fluxes
      integer, dimension(8)               :: ipnt        !<    Local work array for the pointering
      integer                             :: iq          !<    Local loop counter for exchanges loop 
      integer                             :: iseg        !<    Local loop counter for computational element loop
      integer                             :: ifrom       !<    Index of from segment
      integer                             :: ito         !<    Index of to segment
      integer                             :: dfrom       !<    PMSA offset of from segment
      integer                             :: dto         !<    PMSA offset of to segment 
!
!------------------------------------------------------------------------------
!
!     Initialise pointers
!
      ipnt        = ipoint
      iflux       = 0
!
!     Assert that both Veloc1 and Veloc2 are spatial
!
      if (increm(7) /= increm(8)) then
         stop
      endif
!
      allocate(a_nx_nx(noseg), a_nx_ny(noseg), a_ny_ny(noseg), f_nx(noseg), f_ny(noseg), cuma(noseg))
      a_nx_nx = 0.0_fp
      a_nx_ny = 0.0_fp
      a_ny_ny = 0.0_fp
      f_nx = 0.0_fp
      f_ny = 0.0_fp
      cuma = 0.0_fp
!
!     Loop over all horizontal exchanges
!
      do iq = 1, noq1+noq2
         XArea       = real(pmsa( ipnt(  1) ),fp)
         Flow        = real(pmsa( ipnt(  2) ),fp)
         nx_from     = real(pmsa( ipnt(  3) ),fp)
         ny_from     = real(pmsa( ipnt(  4) ),fp)
         nx_to       = real(pmsa( ipnt(  5) ),fp)
         ny_to       = real(pmsa( ipnt(  6) ),fp)
!
!        Get from and to segments
!
         ifrom = iexpnt(1,iq)
         ito   = iexpnt(2,iq)
!
         if (ifrom>0) then
            a_nx_nx(ifrom) = a_nx_nx(ifrom) + nx_from * nx_from * XArea
            a_ny_ny(ifrom) = a_ny_ny(ifrom) + ny_from * ny_from * XArea
            a_nx_ny(ifrom) = a_nx_ny(ifrom) + nx_from * ny_from * XArea
            f_nx(ifrom)    = f_nx(ifrom) + nx_from * Flow
            f_ny(ifrom)    = f_ny(ifrom) + ny_from * Flow
            cuma(ifrom)    = cuma(ifrom) + XArea
         endif
!
         if (ito>0) then
            a_nx_nx(ito)   = a_nx_nx(ito) + nx_to * nx_to * XArea
            a_ny_ny(ito)   = a_ny_ny(ito) + ny_to * ny_to * XArea
            a_nx_ny(ito)   = a_nx_ny(ito) + nx_to * ny_to * XArea
            f_nx(ito)      = f_nx(ito) + nx_to * Flow
            f_ny(ito)      = f_ny(ito) + ny_to * Flow
            cuma(ito)      = cuma(ito) + XArea
         endif
!
!        Increment pointers
!
         ipnt        = ipnt        + increm
!
      enddo
!
!     Loop over all segments
!
      ipnt        = ipoint
!
      do iseg = 1, noseg
!
!        Get from and to segments
!
         Omega  = a_nx_nx(iseg) * a_ny_ny(iseg) - a_nx_ny(iseg)**2
         if (cuma(iseg)==0.0_fp) then
            Veloc1 = 0.0_fp
            Veloc2 = 0.0_fp
         elseif (Omega<1e-6) then
            Veloc1 = f_nx(iseg)/cuma(iseg)
            Veloc2 = f_ny(iseg)/cuma(iseg)
         else
            Veloc1 = (a_ny_ny(iseg) * f_nx(iseg) - a_nx_ny(iseg) * f_ny(iseg))/Omega
            Veloc2 = (a_nx_nx(iseg) * f_ny(iseg) - a_nx_ny(iseg) * f_nx(iseg))/Omega
         endif
!
!        Put output and flux values into arrays
!
!         fl  ( iflux +  1  ) = 0.0
         pmsa( ipnt(  7)   ) = real(Veloc1,4)
         pmsa( ipnt(  8)   ) = real(Veloc2,4)
!
!        Increment pointers
!
         ipnt        = ipnt        + increm
!
      enddo
!
      deallocate(a_nx_nx, a_nx_ny, a_ny_ny, f_nx, f_ny, cuma)
!
      end subroutine xyvelo
