!!  Copyright (C)  Stichting Deltares, 2012-2020.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      SUBROUTINE PRONRS ( PRONAM, IMODUL)
!>\file
!>       Returns number of the process routine

!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : jan -1994 by Jan van Beek
!
!     FUNCTION            : Returns module number
!                           NOTE the numbers in this subroutine must have
!                           an 1 to 1 relation with the labels in the
!                           subroutine PROCEZ.
!
!     SUBROUTINES CALLED  : -
!
!     FILES               : -
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     PRONAM  CHA*(*)       1     INPUT   Name of called module
!     IMODUL  INTEGER       1     OUTPUT  Module number proces
!
!     Declaration of arguments
!
      INTEGER       IMODUL
      CHARACTER*(*) PRONAM
!
!     Local declarations
!
      PARAMETER   ( NOMODU = 154)
      CHARACTER*6   MODNAM(NOMODU)
      SAVE          MODNAM
      DATA MODNAM /
     +   'DDEPTH',
     +   'DSURF',
     +   'TOTDEP',
     +   'EMERSI',
     +   'METEO',
     +   'HEATFL',
     +   'DAYRAD',
     +   'TEMPER',
     +   'VARSAL',
     +   'VELOC',
     +   'RESTIM',
     +   'STOX3D',
     +   'HDISP',
     +   'HDISPV',
     +   'WATAGE',
     +   'INTPOL',
     +   'CALCHZ',
     +   'CALWAV',
     +   'CALTAU',
     +   'SIMPH',
     +   'SPCARB',
     +   'EXTINA',
     +   'EXTINC',
     +   'CLCRAD',
     +   'DAYL',
     +   'DEPAVE',
     +   'VTRANS',
     +   'D40BLO',
     +   'PHCOMB',
     +   'MAKPOC',
     +   'PHCOMP',
     +   'SEDCOM',
     +   'WKCOMP',
     +   'DMVOL',
     +   'BACMRT',
     +   'SATCO2',
     +   'REAR',
     +   'ADSPO4',
     +   'DENSED',
     +   'DENWAT',
     +   'NITRIF',
     +   'SATOXY',
     +   'VAROXY',
     +   'BOTMIN',
     +   'BODCOD',
     +   'DECBOD',
     +   'DECPC5',
     +   'VIVIAN',
     +   'DISSI',
     +   'SEDOX',
     +   'TFALG',
     +   'DLALG',
     +   'NLALG',
     +   'RADALG',
     +   'RDBALG',
     +   'PRIPRO',
     +   'SDPPRO',
     +   'PPRLIM',
     +   'NUTUPT',
     +   'NUTREL',
     +   'NRALGS',
     +   'OXYMIN',
     +   'CSELAC',
     +   'EBUCH4',
     +   'SATCH4',
     +   'SULFID',
     +   'SULFOX',
     +   'SULFPR',
     +   'METHOX',
     +   'SPECFE',
     +   'IRONOX',
     +   'SULPHO',
     +   'IRONRE',
     +   'PRIRON',
     +   'CALSED',
     +   'SEDCAR',
     +   'SEDNUT',
     +   'SEDSOD',
     +   'SSEDPH',
     +   'SOMSED',
     +   'SEDAAP',
     +   'RESDM',
     +   'BURIAL',
     +   'DIGGIN',
     +   'ADVTRA',
     +   'DSPTRA',
     +   'RFPART',
     +   'PARTMP',
     +   'TRASE2',
     +   'ULFIX',
     +   'CONSBL',
     +   'SWOXY',
     +   'TRCOEF',
     +   'VERVLU',
     +   'DEGMP',
     +   'SEDHM',
     +   'SEDOMV',
     +   'ATMDEP',
     +   'NH3FRE',
     +   'POSOXY',
     +   'SECCHI',
     +   'PTEWOR',
     +   'STREAR',
     +   'TRSOXY',
     +   'APATIT',
     +   'HARVES',
     +   'VEG2DN',
     +   'VBSTAT',
     +   'VBGRO',
     +   'VBMRT',
     +   'VEG3DX',
     +   'VBUPT',
     +   'VEG3DU',
     +   'SALCHL',
     +   'DECDET',
     +   'S12TRA',
     +   'RESANT',
     +   'STADAY',
     +   'STADPT',
     +   'STADSC',
     +   'STAGEO',
     +   'STAPRC',
     +   'STAQTL',
     +   'SUMFRC',
     +   'FLXFRC',
     +   'PHCARB',
     +   'HDISPA',
     +   'MAXMAC',
     +   'COVMAC',
     +   'MACDIS',
     +   'RADMAC',
     +   'MACNUT',
     +   'MACROP',
     +   'MAC3DU',
     +   'GRZMAC',
     +   'NPPS12',
     +   'DEBGRZ',
     +   'FLOCEQ',
     +   'DREDGE',
     +   'RESPUP',
     +   'SEDIM ',
     +   'S12TIM',
     +   'REFL  ',
     +   'ATTOUT',
     +   'CASCAD',
     +   'EFFBLO',
     +   'EFFAVE',
     +   'DECTRA',
     +   'ESPACE',
     +   'CALTEM',
     +   'PLASTC',
     +   'WLCWOC',
     +   'HDISS' ,
     +   'TMODE'
     +   /
!
!     Set module number
!
      IMODUL = 0
      DO 10 J = 1,NOMODU
         IF (PRONAM(1:6).EQ.MODNAM(J)) IMODUL = J
   10 CONTINUE
!
      RETURN
      END
