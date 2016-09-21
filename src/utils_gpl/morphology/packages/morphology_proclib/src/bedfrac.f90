      subroutine bedfrac ( pmsa   , fl     , ipoint , increm , noseg  , &
     &                     noflux , iexpnt , iknmrk , noq1   , noq2   , &
     &                     noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'BEDFRA' :: BEDFRAC
!>\file
!>       Process: BedFrac - Bed composition properties

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
!  $Id: bedfrac.f90 4612 2015-01-21 08:48:09Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_gpl/morphology/packages/morphology_proclib/src/bedfrac.f90 $
!------------------------------------------------------------------------------
      use bedcomposition_module
      use sediment_basics_module
      use message_module, only:initstack
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
      integer                             :: numISS      !< I  number of ISS fractions                            (-)
      integer                             :: numIBS      !< I  number of IBS fractions                            (-)
      real(fp), dimension(:), allocatable :: IniThk      !< I  initial sediment thickness of ISS fraction         (m)
                                                         !< I  initial sediment thickness of IBS fraction         (m)
      real(fp), dimension(:), allocatable :: d50         !< I  D50 of ISS fraction                                (m)
                                                         !< I  D50 of IBS fraction                                (m)
      real(fp), dimension(:), allocatable :: logstd      !< I  std of log diameter of ISS fraction                (-)
                                                         !< I  std of log diameter of IBS fraction                (-)
      real(fp), dimension(:), allocatable :: rhosol      !< I  density of ISS fraction                            (kg/m3)
                                                         !< I  density of IBS fraction                            (kg/m3)
      integer, dimension(:), allocatable  :: sedtyp      !< I  type of ISS fraction (1=non-coh, 2=coh)            (-)
                                                         !<    type of IBS fraction = SEDTYP_NONCOHESIVE_TOTALLOAD
      real(fp), dimension(1)              :: mudcnt      !< I  bed mud content                                    (-)
      real(fp)                            :: ThrNonErod  !< I  threshold for non-erodible layer effect            (m)
      integer                             :: swUnderlyr  !< I  Switch of underlayer concept                       (-)
      integer                             :: swPorosity  !< I  Switch of porosity concept                         (-)
      real(fp), dimension(:), allocatable :: frac        !< O  mass fraction of ISS fraction in bed               (-)
                                                         !< O  mass fraction of IBS fraction in bed               (-)
      real(fp), dimension(1)              :: fracMud     !< O  mud fraction in bed                                (-)
      real(fp), dimension(1)              :: fixfac      !< O  non-erodible layer factor                          (-)
!
      integer                             :: iflux       !<    Local index for pointering the fluxes
      integer, dimension(:), allocatable  :: ipnt        !<    Local work array for the pointering
      integer                             :: iseg        !<    Local loop counter for computational element loop
      integer                             :: iIniThk     !<    Offset of IniThk data in ipnt/increm arrays
      integer                             :: id50        !<    Offset of d50 data in ipnt/increm arrays
      integer                             :: ilogstd     !<    Offset of logstd data in ipnt/increm arrays
      integer                             :: irhosol     !<    Offset of solid sediment density data in ipnt/increm arrays
      integer                             :: irhobed     !<    Offset of dry bed density data in ipnt/increm arrays
      integer                             :: isedtyp     !<    Offset of sedtyp data in ipnt/increm arrays
      integer                             :: imudcnt     !<    Offset of mudcnt data in ipnt/increm arrays
      integer                             :: ifrac       !<    Offset of frac data in ipnt/increm arrays
      integer                             :: ifracMud    !<    Offset of fracMud data in ipnt/increm arrays
      integer                             :: i           !<    Loop index
      integer                             :: nItem       !<    Total number of input and output items
      integer                             :: numIS       !<    Total number of fractions
      integer                             :: istat       !<    Error status
      logical                             :: anymud      !<    Any cohesive ISS fraction included?
      logical                             :: varying     !<    Are sediment properties spatially varying?
!
      type(bed_data)                      :: bed         !<    Bed composition data
      integer, external                   :: inibedcomp  !<    Initialization function for bed composition
!
!------------------------------------------------------------------------------
!
!     Initialise pointers
!
      numISS      = pmsa( ipoint(  1) )
      numIBS      = pmsa( ipoint(  2) )
      numIS       = numISS+numIBS
!
      iIniThk     = 2
      id50        = iIniThk+numIS
      ilogstd     = id50+numIS
      irhosol     = ilogstd+numIS
      irhobed     = irhosol+numIS
      isedtyp     = irhobed+numIS
      imudcnt     = isedtyp+numISS
      ifrac       = imudcnt+4
      ifracMud    = ifrac+numIS
      nItem       = ifracMud+2
!
      allocate(ipnt(nItem))
      ipnt        = ipoint(1:nItem)
      iflux       = 0
!
      allocate(sedtyp(numIS))
!
      allocate(frac  (numIS))
!
!     Constant coefficients
!
      anymud = .false.
      do i = 1,numIS
         if (i<=numISS) then
            call failconst(increm(isedtyp+i),'Sediment type')
            sedtyp(i) = int(pmsa( ipnt(isedtyp+i) ))
            if (sedtyp(i)==SEDTYP_COHESIVE) then
               anymud = .true.
            endif
         else
            sedtyp(i) = SEDTYP_NONCOHESIVE_TOTALLOAD
         endif
      enddo
      !
      call failconst(increm(imudcnt+2),'Threshold non-erodible layer')
      ThrNonErod = pmsa( ipnt(imudcnt+2) )
!
!     The following coefficients will also be constant most of the time
!
      varying = increm(imudcnt+1) /= 0
!
      if (.not.varying) then
         mudcnt(1) = pmsa( ipnt(imudcnt+1) )
      endif
!
      call getbedcomp(bed,1,1)
      if (.not.bed%isInitialized) then
         call failconst(increm(imudcnt+3),'Underlayer type')
         swUnderLyr = int(pmsa( ipnt(imudcnt+3) ))
         !
         call failconst(increm(imudcnt+4),'Porosity type')
         swPorosity = int(pmsa( ipnt(imudcnt+4) ))
         if (swUnderLyr==1) swPorosity = 0
         !
         allocate(d50(numIS))
         allocate(logstd(numIS))
         allocate(rhosol(numIS))
         do i = 1,numIS
            if (swPorosity==0) then
               call failconst(increm(irhobed+i),'rhobed')
               d50(i)    = 999.0 ! value not relevant
               logstd(i) = 999.0 ! value not relevant
               rhosol(i) = pmsa( ipnt(irhobed+i) )
            else
               call failconst(increm(id50+i),'D50')
               call failconst(increm(ilogstd+i),'logstd')
               call failconst(increm(irhosol+i),'rhosol')
               d50(i)    = pmsa( ipnt(id50+i) )
               logstd(i) = pmsa( ipnt(ilogstd+i) )
               rhosol(i) = pmsa( ipnt(irhosol+i) )
            endif
         enddo
         allocate(bed%comp)
         allocate(bed%messages)
         call initstack(bed%messages)
         istat = inibedcomp(bed%comp, noseg, numIS, sedtyp, d50, logstd, rhosol, swUnderLyr, swPorosity)
         if (istat /= 0) then
            write(*,'(A)') 'An error occurred in BEDFRAC. Program stops.'
            stop
         endif
         deallocate(d50,logstd,rhosol)
         !
         do iseg = 1, noseg
            do i = 1, numIS
               bed%comp%state%bodsed(i,iseg) = pmsa( ipnt(iIniThk+i) )
            enddo
            !
            ipnt        = ipnt        + increm(1:nItem)
         enddo
         ipnt        = ipoint(1:nItem)
         !
         bed%isInitialized = .true.
      endif
!
!     Loop over all segments
!
      do iseg = 1, noseg
!
!        Get input values from arrays
!
         if (varying) then
            mudcnt(1) = pmsa( ipnt(imudcnt+1) )
         endif
!
!   *****     Insert your code here  *****
!
         call getfrac(bed%comp ,frac      ,anymud   , &
                    & mudcnt   ,fracMud   ,iseg      ,iseg     )
!
         call getfixfac(bed%comp  ,iseg      ,iseg      ,1         , &
                      & noseg     ,fixfac    ,ThrNonErod)
!
!   *****     End of your code       *****
!
!        Put output and flux values into arrays
!
         fl  ( iflux +  1  ) = 0.0 ! dum_dZB1
         do i = 1,numIS
            pmsa( ipnt( ifrac+i)     ) = frac(i)
         enddo
         pmsa( ipnt( ifracMud+1)  ) = fracMud(1)
         pmsa( ipnt( ifracMud+2)  ) = fixfac(1)
!
!        Increment pointers
!
         iflux       = iflux       + noflux
         ipnt        = ipnt        + increm(1:nItem)
!
      enddo
!
      call setbedcomp(bed,1)
!
      deallocate(ipnt)
      deallocate(sedtyp)
      deallocate(frac)
!
      end subroutine bedfrac


      integer function inibedcomp(bedcomp, noseg, numIS, sedtyp, d50, logstd, rhosol, swUnderLyr, swPorosity)
      use bedcomposition_module
      use precision
!
      IMPLICIT NONE
!
      type(bedcomp_data)                     :: bedcomp     !<    Bed composition data
      integer                   , intent(in) :: noseg       !<    Number of segments
      integer                   , intent(in) :: numIS       !<    Number of sediment fractions
      integer , dimension(numIS), intent(in) :: sedtyp      !<    sediment type
      real(fp), dimension(numIS), intent(in) :: d50         !<    d50 of sediment fractions
      real(fp), dimension(numIS), intent(in) :: logstd      !<    logstd of sediment fractions
      real(fp), dimension(numIS), intent(in) :: rhosol      !<    rhosol of sediment fractions
      integer                   , intent(in) :: swUnderLyr  !<    underlayer type
      integer                   , intent(in) :: swPorosity  !<    porosity type
!
      integer                                :: istat       !<    Error status
      integer, pointer                       :: iunderlyr
      logical, pointer                       :: exchlyr
      integer, pointer                       :: nfrac
      integer, pointer                       :: nmlb
      integer, pointer                       :: nmub
      integer, pointer                       :: nlalyr
      integer, pointer                       :: neulyr
      real(fp), pointer                      :: theulyr
      real(fp), pointer                      :: thlalyr
      integer, pointer                       :: updbaselyr
      real(fp), pointer                      :: minmass
      integer, pointer                       :: maxwarn
      integer, pointer                       :: iporosity
!
      istat = initmorlyr(bedcomp)
!
      if (istat == 0) istat = bedcomp_getpointer_integer(bedcomp, 'nfrac'               , nfrac)
      if (istat == 0) istat = bedcomp_getpointer_integer(bedcomp, 'nmlb'                , nmlb)
      if (istat == 0) istat = bedcomp_getpointer_integer(bedcomp, 'nmub'                , nmub)
!
      if (istat == 0) istat = bedcomp_getpointer_integer(bedcomp, 'IUnderLyr'           , iunderlyr)
      if (istat == 0) istat = bedcomp_getpointer_logical(bedcomp, 'ExchLyr'             , exchlyr)
      if (istat == 0) istat = bedcomp_getpointer_integer(bedcomp, 'NLaLyr'              , nlalyr)
      if (istat == 0) istat = bedcomp_getpointer_integer(bedcomp, 'NEuLyr'              , neulyr)
      if (istat == 0) istat = bedcomp_getpointer_realfp (bedcomp, 'ThEuLyr'             , theulyr)
      if (istat == 0) istat = bedcomp_getpointer_realfp (bedcomp, 'ThLaLyr'             , thlalyr)
      if (istat == 0) istat = bedcomp_getpointer_integer(bedcomp, 'UpdBaseLyr'          , updbaselyr)
      if (istat == 0) istat = bedcomp_getpointer_realfp (bedcomp, 'MinMassShortWarning' , minmass)
      if (istat == 0) istat = bedcomp_getpointer_integer(bedcomp, 'MaxNumShortWarning'  , maxwarn)
      if (istat == 0) istat = bedcomp_getpointer_integer(bedcomp, 'IPorosity'           , iporosity)
      if (istat == 0) then
         nmlb = 1
         nmub = noseg
         nfrac = numIS
         iunderlyr = swUnderLyr
         exchlyr = .false.
         nlalyr = 0
         neulyr = 0
         theulyr = 0.1
         thlalyr = 0.1
         updbaselyr = 1
         minmass = 0.0
         maxwarn = 100
         iporosity = swPorosity
      endif
!
      if (istat == 0) istat = allocmorlyr(bedcomp)
!
!     initialize bed composition
!
      if (istat == 0) call setbedfracprop(bedcomp, sedtyp, d50, logstd, rhosol)
!
      inibedcomp = istat
      end function inibedcomp

!subroutine mytest(j)
!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: '_MYTEST' :: _MYTEST
!    use bedcomposition_module
!    integer :: i
!    integer :: j
!    i = j+1
!    write(*,*) j,i
!end subroutine mytest
