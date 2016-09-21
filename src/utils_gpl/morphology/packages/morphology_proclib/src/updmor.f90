      subroutine updmor ( pmsa   , fl     , ipoint , increm , noseg  , &
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   , &
     &                    noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'UPDMOR' :: UPDMOR
!>\file
!>       Process: BedUpdate - Update bed levels

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
      use bedcomposition_module, only:updmorlyr
!      use sediment_basics_module
      use message_module, only:writemessages
      use bed_ensemble
      use precision
!
      IMPLICIT NONE
!
!     Type                      Name          I/O Description
!
      real(4), dimension(*)  :: pmsa        !<I/O Process Manager System Array, window of routine to process library
      real(4), dimension(*)  :: fl          !< O  Array of fluxes made by this process in mass/volume/time
      integer, dimension(*)  :: ipoint      !< I  Array of pointers in pmsa to get and store the data
      integer, dimension(*)  :: increm      !< I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
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
      integer                               :: numISS      !< I  number of ISS fractions                            (-)
      integer                               :: numIBS      !< I  number of IBS fractions                            (-)
      real(4)                               :: SBTnrm      !< I  total normal transport comp for IBS frac           (kg/m)
      real(4)                               :: MorFac      !< I  morphological acceleration factor                  (-)
      real(4)                               :: Surf        !< I  horizontal surface area of a DELWAQ segment        (m2)
      real(4)                               :: XWidth      !< I  exchange width                                     (m)
      real(4)                               :: dVB         !< O  segment integrated bed level change                (m3/d)
      real(4)                               :: dZB         !< F  bed level change                                   (m/d)
!
      integer                               :: iflux       !<    Local index for pointering the fluxes
      integer, dimension(:), allocatable    :: ipnt        !<    Local work array for the pointering
      integer                               :: iseg        !<    Local loop counter for computational element loop
      integer                               :: iq          !<    Local loop counter for exchanges loop
      integer                               :: ifrom       !<    Index of from segment
      integer                               :: ito         !<    Index of to segment
      integer                               :: i           !<    Local loop counter for sediment fraction
!
      integer                               :: iSBTnrm     !<    Offset of SBTnrm data in ipnt/increm arrays
      integer                               :: iMorFac     !<    Offset of MorFac data in ipnt/increm arrays
      integer                               :: nItem       !<    Total number of input and output items
!
      integer                               :: pSurf       !<    Offset of Surf data in pmsa array
      integer                               :: numIS       !<    Total number of fractions 
      real(fp), dimension(:,:), allocatable :: dbodsd      !<    Bed composition change
      real(fp), dimension(:), allocatable   :: depchg      !<    Bed level change
      real(fp)                              :: S           !<    Total transport flux at exchange
      type(bed_data)                        :: bed         !<    Bed composition data 
!
!------------------------------------------------------------------------------
!
      numISS      = int(pmsa( ipoint(  1) ))
      numIBS      = int(pmsa( ipoint(  2) ))
      numIS       = numISS+numIBS
!
      iSBTnrm      = 2
      iMorFac      = iSBTnrm      + numIS
      nItem        = iMorFac      + 4
!
      call failconst(increm(iMorFac+1),'Morphological factor')
      MorFac         = pmsa( ipoint(iMorFac     +  1) )
      allocate( dbodsd(numIS,noseg) )
      dbodsd = 0.0_fp
      allocate( depchg(noseg) )
      depchg = 0.0_fp
!
!     Initialise pointers
!
      allocate(ipnt(nItem))
      ipnt        = ipoint(1:nItem)
!
!     Loop over all exchanges
!
      do iq = 1, noq1+noq2
!
!        Get from and to segments
!
         ifrom = iexpnt(1,iq)
         ito   = iexpnt(2,iq)
!
!        Get input values from arrays
!
         do i = 1, numIS
            SBTnrm = pmsa( ipnt(iSBTnrm+i) )
            XWidth = pmsa( ipnt(iMorFac+3) )
            S = SBTnrm * XWidth * MorFac
            !
            if (ifrom>0) dbodsd(i,ifrom) = dbodsd(i,ifrom) - S
            if (ito>0)   dbodsd(i,ito)   = dbodsd(i,ito)   + S
         enddo
!
!        Increment pointers
!
         ipnt        = ipnt        + increm(1:nItem)
!
      enddo
!
!     Reset pointers
!
      ipnt        = ipoint(1:nItem)
      iflux       = 0
!
!     Convert sediment total mass into sediment per m2
!
      pSurf = ipnt(iMorFac+2)
      do iseg = 1, noseg
         Surf  = pmsa( pSurf )
         do i = 1, numIS
            dbodsd(i,iseg) = dbodsd(i,iseg) / Surf
         enddo
         pSurf = pSurf + increm(iMorFac+2)
      enddo
!
!     Update bedcomposition
!
      call getbedcomp(bed,1,1) 
      if (updmorlyr(bed%comp, dbodsd, depchg, bed%messages) /= 0) then
          call writemessages(bed%messages, 6)
          stop
      endif
!
!     Reset pointers
!
      ipnt        = ipoint(1:nItem)
      iflux       = 0
!
!     Loop over all segments
!
      do iseg = 1, noseg
         Surf  = pmsa( ipnt(iMorFac+2) )
!
         dZB = depchg(iseg)
         dVB = depchg(iseg)*Surf
!
!        Put output and flux values into arrays
!
         fl  ( iflux+1 ) = dZB
         pmsa( ipnt(iMorFac+4) ) = dVB
!
!        Increment pointers
!
         iflux       = iflux       + noflux
         ipnt        = ipnt        + increm(1:nItem)
!
      enddo
!
      deallocate(ipnt)
      deallocate(dbodsd)
      deallocate(depchg)
!
      end subroutine updmor
