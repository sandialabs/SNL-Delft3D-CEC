      subroutine exchtra ( pmsa   , fl     , ipoint , increm , noseg  , &
     &                     noflux , iexpnt , iknmrk , noq1   , noq2   , &
     &                     noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'EXCHTR' :: EXCHTRA
!>\file
!>       Process: ExchTra - Transport at exchanges for sediment fractions

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
      IMPLICIT NONE
!
!     Type                      Name          I/O Description
!
      real(4), dimension(*)  :: pmsa        !<I/O Process Manager System Array, window of routine to process library
      real(4), dimension(*)  :: fl          !< O  Array of fluxes made by this process in mass/volume/time
      integer, dimension(22) :: ipoint      !< I  Array of pointers in pmsa to get and store the data
      integer, dimension(22) :: increm      !< I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
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
      integer                            :: iSBCx       !< I  pmsa index for x-comp of bed-load due to currents for IBS frac    (kg/m)
      integer                            :: iSBCy       !< I  pmsa index for y-comp of bed-load due to currents for IBS frac    (kg/m)
      integer                            :: iSBWx       !< I  pmsa index for x-comp of bed-load due to waves for IBS frac       (kg/m)
      integer                            :: iSBWy       !< I  pmsa index for y-comp of bed-load due to waves for IBS frac       (kg/m)
      integer                            :: iSSWx       !< I  pmsa index for x-comp of susp load due to waves for IBS frac      (kg/m)
      integer                            :: iSSWy       !< I  pmsa index for y-comp of susp load due to waves for IBS frac      (kg/m)
      integer                            :: ifrac       !< I  pmsa index for mass fraction of IBS fraction in bed               (-)
      integer                            :: ifixfac     !< I  pmsa index for non-erodible layer factor                          (-)
      integer                            :: TypeSed     !< I  type of sediment fraction (0=bedl, 1=noncoh, 2=coh)(-)
      real(4)                            :: SlopeNorm   !< I  slope normal to interface                          (m/m)
      real(4)                            :: SlopeTang   !< I  slope tangential to interface                      (m/m)
      real(4)                            :: nx_from     !< I  x-component of unit normal for FROM segment        (-)
      real(4)                            :: ny_from     !< I  y-component of unit normal for FROM segment        (-)
      real(4)                            :: nx_to       !< I  x-component of unit normal for TO segment          (-)
      real(4)                            :: ny_to       !< I  y-component of unit normal for TO segment          (-)
      real(4)                            :: SBCnrm      !< O  normal comp of SBC for IBS frac                    (kg/m)
      real(4)                            :: SBCtng      !< O  tangential comp of SBC for IBS frac                (kg/m)
      real(4)                            :: SBWnrm      !< O  normal comp of SBW for IBS frac                    (kg/m)
      real(4)                            :: SBWtng      !< O  tangential comp of SBW for IBS frac                (kg/m)
      real(4)                            :: SSWnrm      !< O  normal comp of SSW for IBS frac                    (kg/m)
      real(4)                            :: SSWtng      !< O  tangential comp of SSW for IBS frac                (kg/m)
      real(4)                            :: SBTnrm      !< O  total normal transport comp for IBS frac           (kg/m)
      real(4)                            :: dum2        !< F  dummy flux to access ExchTraIBS                    (-)
!
      real(4)                            :: SBCn_from   !<
      real(4)                            :: SBCt_from   !<
      real(4)                            :: SBCn_to     !<
      real(4)                            :: SBCt_to     !<
      real(4)                            :: SBWn_from   !<
      real(4)                            :: SBWt_from   !<
      real(4)                            :: SBWn_to     !<
      real(4)                            :: SBWt_to     !<
      real(4)                            :: SSWn_from   !<
      real(4)                            :: SSWt_from   !<
      real(4)                            :: SSWn_to     !<
      real(4)                            :: SSWt_to     !<
      real(4)                            :: STn_from    !<
      real(4)                            :: STn_to      !<
      real(4)                            :: frac_from   !<
      real(4)                            :: frac_to     !<
      real(4)                            :: fxfc_from   !<
      real(4)                            :: fxfc_to     !<
!
      integer                            :: iflux       !<    Local index for pointering the fluxes
      integer, dimension(22)             :: ipnt        !<    Local work array for the pointering
      integer                            :: iq          !<    Local loop counter for exchanges loop
      integer                            :: iseg        !<    Local loop counter for segments
      integer                            :: ifrom       !<    Index of from segment
      integer                            :: ito         !<    Index of to segment
      integer                            :: dfrom       !<    PMSA offset of from segment
      integer                            :: dto         !<    PMSA offset of to segment
!
      logical                            :: avalan = .false.
      integer                            :: islope = 1
      integer                            :: ibedload = 1
!
!------------------------------------------------------------------------------
!
!     Initialise pointers
!
      ipnt        = ipoint
      iflux       = 0
!
      do iseg = 1, noseg
         fl  ( iflux +  1  ) = 0.0
         iflux = iflux + noflux
      enddo
      !
      call failconst(increm(9),'Sediment type')
      TypeSed     = int(pmsa( ipnt(  9) ))
      if (TypeSed == 2) then ! SEDTYP_COHESIVE
         do iq = 1, noq1+noq2
!
!           Set all output to zero
!
            pmsa( ipnt( 16)   ) = 0.0
            pmsa( ipnt( 17)   ) = 0.0
            pmsa( ipnt( 18)   ) = 0.0
            pmsa( ipnt( 19)   ) = 0.0
            pmsa( ipnt( 20)   ) = 0.0
            pmsa( ipnt( 21)   ) = 0.0
            pmsa( ipnt( 22)   ) = 0.0
!
!           Increment pointers
!
            ipnt        = ipnt        + increm
!
         enddo

         return
      endif
!
      iSBCx   = ipnt(  1)
      iSBCy   = ipnt(  2)
      iSBWx   = ipnt(  3)
      iSBWy   = ipnt(  4)
      iSSWx   = ipnt(  5)
      iSSWy   = ipnt(  6)
      ifrac   = ipnt(  7)
      ifixfac = ipnt(  8)
!
!     Loop over all horizontal exchanges
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
         SlopeNorm   = pmsa( ipnt( 10) )
         SlopeTang   = pmsa( ipnt( 11) )
         nx_from     = pmsa( ipnt( 12) )
         if (nx_from<-1.0) then
            ! vector components in grid directions
            ! note: we may need different 
            if (iq<=noq1) then
               ! first grid direction
               nx_from     = 1.0
               ny_from     = 0.0
               nx_to       = 1.0
               ny_to       = 0.0
            else
               ! second grid direction
               nx_from     = 0.0
               ny_from     = 1.0
               nx_to       = 0.0
               ny_to       = 1.0
            endif
         else
            ! vector components in user specified (geospatial) direction
            ny_from     = pmsa( ipnt( 13) )
            nx_to       = pmsa( ipnt( 14) )
            ny_to       = pmsa( ipnt( 15) )
         endif
!
!   *****     Insert your code here  *****
!
         dfrom     =  increm(1)*(ifrom-1)
         dto       =  increm(1)*(ito-1)
!
         if (ifrom>0) then
            SBCn_from =  pmsa(iSBCx+dfrom)*nx_from + pmsa(iSBCy+dfrom)*ny_from
            SBCt_from = -pmsa(iSBCx+dfrom)*ny_from + pmsa(iSBCy+dfrom)*nx_from
            SBWn_from =  pmsa(iSBWx+dfrom)*nx_from + pmsa(iSBWy+dfrom)*ny_from
            SBWt_from = -pmsa(iSBWx+dfrom)*ny_from + pmsa(iSBWy+dfrom)*nx_from
            SSWn_from =  pmsa(iSSWx+dfrom)*nx_from + pmsa(iSSWy+dfrom)*ny_from
            SSWt_from = -pmsa(iSSWx+dfrom)*ny_from + pmsa(iSSWy+dfrom)*nx_from
            frac_from =  pmsa(ifrac+dfrom)
            fxfc_from =  pmsa(ifixfac+dfrom)
            STn_from  = SBCn_from + SBWn_from + SSWn_from
         endif
         !
         if (ito>0) then
            SBCn_to   =  pmsa(iSBCx+dto)  *nx_to   + pmsa(iSBCy+dto)  *ny_to
            SBCt_to   = -pmsa(iSBCx+dto)  *ny_to   + pmsa(iSBCy+dto)  *nx_to
            SBWn_to   =  pmsa(iSBWx+dto)  *nx_to   + pmsa(iSBWy+dto)  *ny_to
            SBWt_to   = -pmsa(iSBWx+dto)  *ny_to   + pmsa(iSBWy+dto)  *nx_to
            SSWn_to   =  pmsa(iSSWx+dto)  *nx_to   + pmsa(iSSWy+dto)  *ny_to
            SSWt_to   = -pmsa(iSSWx+dto)  *ny_to   + pmsa(iSSWy+dto)  *nx_to
            frac_to   =  pmsa(ifrac+dto)
            fxfc_to   =  pmsa(ifixfac+dto)
            STn_to    = SBCn_to   + SBWn_to   + SSWn_to
         endif
         !
         if (ifrom<=0) then
            if (ito>0) then
               SBCnrm = SBCn_to * frac_to * fxfc_to
               SBWnrm = SBWn_to * frac_to * fxfc_to
               SSWnrm = SSWn_to * frac_to * fxfc_to
            else
               SBCnrm = 0.0
               SBWnrm = 0.0
               SSWnrm = 0.0
            endif
         elseif (ito<=0) then
            SBCnrm = SBCn_from * frac_from * fxfc_from
            SBWnrm = SBWn_from * frac_from * fxfc_from
            SSWnrm = SSWn_from * frac_from * fxfc_from
         else
            !
            select case (ibedload)
            case (1)
                if (STn_from>0.0 .and. STn_to>0.0) then
                    SBCnrm = SBCn_from
                    SBWnrm = SBWn_from
                    SSWnrm = SSWn_from
                elseif (STn_from<0.0 .and. STn_to<0.0) then
                    SBCnrm = SBCn_to
                    SBWnrm = SBWn_to
                    SSWnrm = SSWn_to
                else
                    SBCnrm = (SBCn_from + SBCn_to)/2.0
                    SBWnrm = (SBWn_from + SBWn_to)/2.0
                    SSWnrm = (SSWn_from + SSWn_to)/2.0
                endif
            case (2)
                SBCnrm = (SBCn_from + SBCn_to)/2.0
                SBWnrm = (SBWn_from + SBWn_to)/2.0
                SSWnrm = (SSWn_from + SSWn_to)/2.0
            endselect
            SBCtng = (SBCt_from + SBCt_to)/2.0
            SBWtng = (SBWt_from + SBWt_to)/2.0
            SSWtng = (SSWt_from + SSWt_to)/2.0
             !
            select case (islope)
            case(1)
               !
               ! no correction: default values
               !
            endselect
            !
            if (avalan) then
               ! Copy avalanching
            endif
            !
            SBTnrm = SBCnrm + SBWnrm + SSWnrm
            !
            if (SBTnrm>0.0) then
               SBTnrm = SBTnrm * frac_from * fxfc_from
            else
               SBTnrm = SBTnrm * frac_to * fxfc_to
            endif
            !
         endif
!
!   *****     End of your code       *****
!
!        Put output values into arrays
!
         pmsa( ipnt( 16)   ) = SBCnrm
         pmsa( ipnt( 17)   ) = SBCtng
         pmsa( ipnt( 18)   ) = SBWnrm
         pmsa( ipnt( 19)   ) = SBWtng
         pmsa( ipnt( 20)   ) = SSWnrm
         pmsa( ipnt( 21)   ) = SSWtng
         pmsa( ipnt( 22)   ) = SBTnrm
!
!        Increment pointers
!
         ipnt        = ipnt        + increm
!
      enddo
!
      end subroutine exchtra
