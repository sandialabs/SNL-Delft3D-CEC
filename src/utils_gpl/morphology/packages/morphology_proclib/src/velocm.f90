      subroutine velocm ( pmsa   , fl     , ipoint , increm , noseg  , &
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   , &
     &                    noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'VELOCM' :: VELOCM
!>\file
!>       Process: VelocM - Flow characteristics for morphology

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
      integer, dimension(24) :: ipoint      !< I  Array of pointers in pmsa to get and store the data
      integer, dimension(24) :: increm      !< I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
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
      real(4)                            :: Veloc1      !< I  horizontal flow velocity first direction           (m/s)
      real(4)                            :: Veloc2      !< I  horizontal flow velocity second direction          (m/s)
      real(4)                            :: Depth       !< I  depth of segment                                   (m)
      real(4)                            :: TotalDepth  !< I  total depth water column                           (m)
      real(4)                            :: KfSed       !< I  Morphological active segments                      (-)
      real(4)                            :: VonKarman   !< I  Von Karman constant                                (-)
      real(4)                            :: GRAV        !< I  Gravitational acceleration                         (m/s2)
      real(4)                            :: CHEZY       !< I  Chezy coefficient                                  (m0.5/s)
      real(4)                            :: z0rou       !< I  wave enhanced bed roughness z_0 height             (m)
      real(4)                            :: UxMor       !< O  x-component of velocity for morphology             (m/s)
      real(4)                            :: UyMor       !< O  y-component of velocity for morphology             (m/s)
      real(4)                            :: UMor        !< O  velocity magnitude for morphology                  (m/s)
      real(4)                            :: zUMor       !< O  height above bed for morphology velocity           (m)
      real(4)                            :: UxBed       !< O  x-component of velocity in lowest layer            (m/s)
      real(4)                            :: UyBed       !< O  y-component of velocity in lowest layer            (m/s)
      real(4)                            :: UBed        !< O  velocity in lowest layer                           (m/s)
      real(4)                            :: zUBed       !< O  height above bed of velocity of lowest layer       (m)
      real(4)                            :: UxEff2D     !< O  x-component of effective depth averaged velocity   (m/s)
      real(4)                            :: UyEff2D     !< O  y-component of effective depth averaged velocity   (m/s)
      real(4)                            :: UEff2D      !< O  effective depth averaged velocity magnitude        (m/s)
      real(4)                            :: UxDepAvg    !< O  x-component of depth averaged velocity             (m/s)
      real(4)                            :: UyDepAvg    !< O  y-component of depth averaged velocity             (m/s)
      real(4)                            :: UDepAvg     !< O  depth averaged velocity                            (m/s)
      real(4)                            :: UStar       !< O  shear velocity u_star                              (m/s)
      real(4)                            :: dum_dZB3    !< F  dummy flux to access VelocM                        (-)
!
      integer                            :: iflux       !<    Local index for pointering the fluxes
      integer, dimension(24)             :: ipnt        !<    Local work array for the pointering
      integer                            :: iseg        !<    Local loop counter for computational element loop
!
      integer                            :: isegtyp     !<    Segment type (location in water column)
      real(4)                            :: sqrtGRAV    !<    Sqrt of gravity
!
      integer                            :: iseg2D
      integer                            :: noseg2D
      real(4), dimension(:), allocatable :: u1
      real(4), dimension(:), allocatable :: v1
      integer                            :: isegMor
      real(4)                            :: ztmp
      real(4)                            :: dz
!
!------------------------------------------------------------------------------
!
!     Initialise pointers
!
      ipnt        = ipoint
      iflux       = 0
!
!     Allocate temporary array space
!
      noseg2D = noseg - noq3
      allocate(u1(noseg2D), v1(noseg2D))
      u1 = 0.0
      v1 = 0.0
!
!     Get constants
!
      VonKarman   = pmsa( ipnt(  6) )
      GRAV        = pmsa( ipnt(  7) )
      sqrtGRAV    = sqrt(GRAV)
!
!     Loop over all segments
!
      iseg2D = 0
      do iseg = 1, noseg
         iseg2D = iseg2D+1
         if (iseg2D>noseg2D) iseg2D = iseg2D-noseg2D
!
!        Get input values from arrays
!
         Veloc1      = pmsa( ipnt(  1) )
         Veloc2      = pmsa( ipnt(  2) )
         Depth       = pmsa( ipnt(  3) )
         TotalDepth  = pmsa( ipnt(  4) )
         KfSed       = pmsa( ipnt(  5) )
         CHEZY       = pmsa( ipnt(  8) )
         z0rou       = pmsa( ipnt(  9) )
!
!
!   *****     Insert your code here  *****
!
         call dhkmrk ( 2 , iknmrk(iseg) , isegtyp )
         if (KfSed == 0 .or. isegtyp == 1 .or. isegtyp == 2) then
            UxBed    = 0.0
            UyBed    = 0.0
            UBed     = 0.0
            !
            UxMor    = 0.0
            UyMor    = 0.0
            UMor     = 0.0
            !
            UxDepAvg = 0.0
            UyDepAvg = 0.0
            UDepAvg  = 0.0
            !
            UxEff2D  = 0.0
            UyEff2D  = 0.0
            UEff2D   = 0.0
            !
            if (KfSed == 0) then
               zUBed    = TotalDepth*0.368
               zUMor    = TotalDepth*0.368
            else
               u1(iseg2D) = u1(iseg2D) + Veloc1*Depth/TotalDepth
               v1(iseg2D) = v1(iseg2D) + Veloc2*Depth/TotalDepth
               zUBed     = 0.0
               zUMor     = 0.0
            endif
         else
            if (isegtyp == 0) then ! 1D or 2D
               UxBed    = Veloc1
               UyBed    = Veloc2
               zUBed    = TotalDepth*0.368
               !
               UxMor    = Veloc1
               UyMor    = Veloc2
               zUMor    = TotalDepth*0.368
               !
               UxDepAvg = Veloc1
               UyDepAvg = Veloc2
            else ! 3D
               u1(iseg2D) = u1(iseg2D) + Veloc1*Depth/TotalDepth
               v1(iseg2D) = v1(iseg2D) + Veloc2*Depth/TotalDepth
               !
               UxBed    = Veloc1
               UyBed    = Veloc2
               zUBed    = Depth*0.5
               !
               ztmp     = 0.0
               isegMor  = iseg
               dz       = pmsa( ipoint(  3) + (isegMor-1)*increm(3) ) ! Depth(k)
               do while (ztmp+dz/2 < min(0.05*TotalDepth,0.05))
                  ztmp    = ztmp + dz
                  isegMor = isegMor - noseg2D
                  dz      = pmsa( ipoint(  3) + (isegMor-1)*increm(3) ) ! Depth(k)
               enddo
               UxMor    = pmsa( ipoint(  1) + (isegMor-1)*increm(1) ) ! Veloc1(k)
               UyMor    = pmsa( ipoint(  2) + (isegMor-1)*increm(2) ) ! Veloc2(k)
               zUMor    = ztmp + dz/2
               !
               UxDepAvg = u1(iseg2D)
               UyDepAvg = v1(iseg2D)
            endif
            !
            UBed    = sqrt(UxBed*UxBed + UyBed*UyBed)
            UMor    = sqrt(UxMor*UxMor + UyMor*UyMor)
            UDepAvg = sqrt(UxDepAvg*UxDepAvg + UyDepAvg*UyDepAvg)
            UStar   = UMor * VonKarman/log(1.0 + zUMor/z0rou)
            if (UMor > 1.0e-6) then
               UEff2D  = UStar * CHEZY / sqrtGRAV
               UxEff2D = UEff2D * (UxMor / UMor)
               UyEff2D = UEff2D * (UyMor / UMor)
            else
               UEff2D  = 0.0
               UxEff2D = 0.0
               UyEff2D = 0.0
            endif
         endif
!
         dum_dZB3 = 0.0
!
!   *****     End of your code       *****
!
!        Put output and flux values into arrays
!
         fl  ( iflux +  1  ) = dum_dZB3
         pmsa( ipnt( 10)   ) = UxMor
         pmsa( ipnt( 11)   ) = UyMor
         pmsa( ipnt( 12)   ) = UMor
         pmsa( ipnt( 13)   ) = zUMor
         pmsa( ipnt( 14)   ) = UxBed
         pmsa( ipnt( 15)   ) = UyBed
         pmsa( ipnt( 16)   ) = UBed
         pmsa( ipnt( 17)   ) = zUBed
         pmsa( ipnt( 18)   ) = UxEff2D
         pmsa( ipnt( 19)   ) = UyEff2D
         pmsa( ipnt( 20)   ) = UEff2D
         pmsa( ipnt( 21)   ) = UxDepAvg
         pmsa( ipnt( 22)   ) = UyDepAvg
         pmsa( ipnt( 23)   ) = UDepAvg
         pmsa( ipnt( 24)   ) = UStar
!
!        Increment pointers
!
         iflux       = iflux       + noflux
         ipnt        = ipnt        + increm
!
      enddo
!
      deallocate(u1,v1)
!
      end subroutine velocm
