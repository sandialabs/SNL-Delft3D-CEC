      subroutine beddiam ( pmsa   , fl     , ipoint , increm , noseg  , &
     &                     noflux , iexpnt , iknmrk , noq1   , noq2   , &
     &                     noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'BEDDIA' :: BEDDIAM
!>\file
!>       Process: BedDiam - Bed grain size properties

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
!  $Id: beddiam.f90 4612 2015-01-21 08:48:09Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_gpl/morphology/packages/morphology_proclib/src/beddiam.f90 $
!------------------------------------------------------------------------------
      use precision
      use sediment_basics_module
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
      integer                             :: numISS      !< I  number of ISS fractions                            (-)
      integer                             :: numIBS      !< I  number of IBS fractions                            (-)
      real(fp), dimension(:), allocatable :: frac        !< I  mass fraction of ISS fraction in bed               (-)
                                                         !< I  mass fraction of IBS fraction in bed               (-)
      real(fp), dimension(:), allocatable :: d50         !< I  D50 of ISS fraction                                (m)
                                                         !< I  D50 of IBS fraction                                (m)
      real(fp), dimension(:), allocatable :: d90         !< I  D90 of ISS fraction                                (m)
                                                         !< I  D90 of IBS fraction                                (m)
      real(fp), dimension(:), allocatable :: dmin        !< I  Dmin of ISS fraction                               (m)
                                                         !< I  Dmin of IBS fraction                               (m)
      real(fp), dimension(:), allocatable :: dmax        !< I  Dmax of ISS fraction                               (m)
                                                         !< I  Dmax of IBS fraction                               (m)
      real(fp), dimension(:), allocatable :: logstd      !< I  std of log diameter of ISS fraction                (-)
                                                         !< I  std of log diameter of IBS fraction                (-)
      integer, dimension(:), allocatable  :: sedtyp      !< I  type of ISS fraction (1=non-coh, 2=coh)            (-)
                                                         !<    type of IBS fraction = SEDTYP_NONCOHESIVE_TOTALLOAD
      real(fp), dimension(1)              :: DM_mix      !< O  arithm mean diameter of sediment mixture           (m)
      real(fp), dimension(1)              :: DG_mix      !< O  algebr mean diameter of sediment mixture           (m)
      real(fp), dimension(1)              :: DGSD_mix    !< O  algebr mean diameter of sediment mixture           (m)
      real(fp), dimension(3)              :: DXX_mix     !< O  D10, D50 and D90 of sediment mixture               (m)
      real(fp), dimension(:), allocatable :: hidexp      !< O  hiding and exposure factor for ISS fractions       (-)
                                                         !< O  hiding and exposure factor for IBS fractions       (-)
!
      real(fp), dimension(:,:,:), allocatable :: logseddia !<    percentile and log-diameter per fraction
      real(fp), dimension(3)              :: xx          !<    percentile: the xx of Dxx, i.e. 0.5 for D50
      integer                             :: numIS       !<    total number of fractions
      integer                             :: nItem       !<    total number of input and output items
      integer                             :: ifrac       !<    offset of fraction data in ipnt/increm arrays
      integer                             :: id50        !<    offset of d50 data in ipnt/increm arrays
      integer                             :: id90        !<    offset of d90 data in ipnt/increm arrays
      integer                             :: idmin       !<    offset of dmin data in ipnt/increm arrays
      integer                             :: idmax       !<    offset of dmax data in ipnt/increm arrays
      integer                             :: ilogstd     !<    offset of logstd data in ipnt/increm arrays
      integer                             :: isedtyp     !<    offset of sedtyp data in ipnt/increm arrays
      integer                             :: ioutput     !<    offset of output data in ipnt/increm arrays
      integer                             :: i           !<    loop index
      integer                             :: iflux       !<    Local index for pointering the fluxes
      integer, dimension(:), allocatable  :: ipnt        !<    Local work array for the pointering
      integer, dimension(:), allocatable  :: nseddia     !<    number of sediment diameters per fraction
      integer                             :: iseg        !<    Local loop counter for computational element loop
      logical                             :: varying     !<    Are sediment properties spatially varying?
!
!------------------------------------------------------------------------------
!
!     Initialise pointers
!
      numISS      = pmsa( ipoint(  1) )
      numIBS      = pmsa( ipoint(  2) )
      numIS       = numISS+numIBS
!
      ifrac   = 2
      id50    = ifrac  +numIS
      id90    = id50   +numIS
      idmin   = id90   +numIS
      idmax   = idmin  +numIS
      ilogstd = idmax  +numIS
      isedtyp = ilogstd+numIS
      ioutput = isedtyp+numISS
      nItem   = ioutput+5+numIS
!
      allocate(ipnt(nItem))
      ipnt        = ipoint(1:nItem)
      iflux       = 0
!
      allocate(frac  (numIS))
      allocate(d50   (numIS))
      allocate(d90   (numIS))
      allocate(dmin  (numIS))
      allocate(dmax  (numIS))
      allocate(logstd(numIS))
      allocate(sedtyp(numIS))
!
      allocate(hidexp(numIS))
!
      allocate(nseddia(numIS))
      allocate(logseddia(2,101,numIS))
!
      xx(1) = 10
      xx(2) = 50
      xx(3) = 90
!
!     Constant coefficients
!
      do i = 1,numIS
         if (i<=numISS) then
            call failconst(increm(isedtyp+i),'Sediment type')
            sedtyp(i) = pmsa( ipnt(isedtyp+i) )
         else
            sedtyp(i) = SEDTYP_NONCOHESIVE_TOTALLOAD
         endif
      enddo
!
!     The following coefficients will also be constant most of the time
!
      varying = .false.
      do i = 1,5*numIS
         varying = varying .or. increm(id50+i) /= 0
      enddo
!
      if (.not.varying) then
         do i = 1,numIS
            d50(i)    = pmsa( ipnt(id50   +i) )
            d90(i)    = pmsa( ipnt(id90   +i) )
            dmin(i)   = pmsa( ipnt(idmin  +i) )
            dmax(i)   = pmsa( ipnt(idmax  +i) )
            logstd(i) = pmsa( ipnt(ilogstd+i) )
            !
            nseddia(i) = 4
            logseddia(1,1,i) = 0.0
            logseddia(2,1,i) = log(dmin(i))
            !logseddia(1,2,i) = 10.0
            !logseddia(2,2,i) = log(d10(i))
            logseddia(1,2,i) = 50.0
            logseddia(2,2,i) = log(d50(i))
            logseddia(1,3,i) = 90.0
            logseddia(2,3,i) = log(d90(i))
            logseddia(1,4,i) = 100.0
            logseddia(2,4,i) = log(dmax(i))
         enddo
      endif
!
!     Loop over all segments
!
      do iseg = 1, noseg
!
!        Get input values from arrays
!
         do i = 1,numIS
            frac(i) = pmsa( ipnt(ifrac  +i) )
         enddo
         if (varying) then
            do i = 1,numIS
               d50(i)    = pmsa( ipnt(id50   +i) )
               d90(i)    = pmsa( ipnt(id90   +i) )
               dmin(i)   = pmsa( ipnt(idmin  +i) )
               dmax(i)   = pmsa( ipnt(idmax  +i) )
               logstd(i) = pmsa( ipnt(ilogstd+i) )
               !
               nseddia(i) = 4
               logseddia(1,1,i) = 0.0
               logseddia(2,1,i) = log(dmin(i))
               !logseddia(1,2,i) = 10.0
               !logseddia(2,2,i) = log(d10(i))
               logseddia(1,2,i) = 50.0
               logseddia(2,2,i) = log(d50(i))
               logseddia(1,3,i) = 90.0
               logseddia(2,3,i) = log(d90(i))
               logseddia(1,4,i) = 100.0
               logseddia(2,4,i) = log(dmax(i))
            enddo
         endif
!
!   *****     Insert your code here  *****
!
         call compdiam(frac      ,d50       ,d50       ,sedtyp    ,numIS     , &
                     & logstd    ,nseddia   ,logseddia ,1         ,1         , &
                     & 1         ,xx        ,3         ,d50       ,DM_mix    , &
                     & DG_mix    ,DXX_mix   ,DGSD_mix  )
!
         call comphidexp(frac      ,DM_mix    ,1         ,numIS     , &
                       & d50       ,hidexp    ,1         ,1.0_fp    , &
                       & 1.0_fp    ,1         ,1         )
!         call comphidexp(frac      ,DM_mix    ,1         ,numIS     , &
!                       & d50       ,hidexp    ,ihidexp   ,asklhe    , &
!                       & mwwjhe    ,1         ,1         )
!
!   *****     End of your code       *****
!
!        Put output and flux values into arrays
!
         fl  ( iflux +  1  ) = 0.0 ! dum_dZB2
         pmsa( ipnt( ioutput+1)   ) = DM_mix(1)
         pmsa( ipnt( ioutput+2)   ) = DG_mix(1)
         pmsa( ipnt( ioutput+3)   ) = DXX_mix(1)
         pmsa( ipnt( ioutput+4)   ) = DXX_mix(2)
         pmsa( ipnt( ioutput+5)   ) = DXX_mix(3)
         do i = 1,numIS
            pmsa( ipnt( ioutput+5+i)   ) = hidexp(i)
         enddo
!
!        Increment pointers
!
         iflux       = iflux       + noflux
         ipnt        = ipnt        + increm(1:nItem)
!
      enddo
!
      deallocate(ipnt)
      deallocate(frac  ,d50   ,d90   ,dmin  ,dmax  ,logstd,sedtyp)
      deallocate(hidexp)
      deallocate(nseddia,logseddia)
!
      end subroutine beddiam
