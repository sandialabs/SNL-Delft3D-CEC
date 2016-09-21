!!  Copyright (C)  Stichting Deltares, 2012-2015.
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

      SUBROUTINE WBPDEF ( LUPDEF, LUREP , NBPR  , NBPRM , BPRNAM,
     +                    BPRTXT, MODNAM, NSVAI , VAINAM, VAITXT,
     +                    VAIDEF, NSVAO , VAONAM, VAOTXT, NBFL  ,
     +                    BFLNAM, BFLTXT, NBST  , GENBST, FLXBST,
     +                    STOBST, VERSIO, SERIAL, IPVAI , IPVAO ,
     +                    IPBFL , IPBST , NSVXI , IPVXI , VXINAM,
     +                    VXITXT, VXIDEF, NSVXO , IPVXO , VXONAM,
     +                    VXOTXT, NDST  , IPDST , GENDST, OUTDST,
     +                    STODST, NVST  , IPVST , GENVST, OUTVST,
     +                    STOVST, ISWITR)
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: nov -1992 by Jan van Beek
!
!     FUNCTION            : Writes the binary proces definition file
!
!     LOGICAL UNITNUMBERS : LUPDEF  - proces definition file
!                         : LUREP   - report file
!
!     SUBROUTINES CALLED  : SRSTOP, stops execution
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     LUPDEF  INTEGER       1     INPUT   Porces definition file
!     LUREP   INTEGER       1     INPUT   Report file
!     NBPR    INTEGER       1     INPUT   Number of processes in def file
!     NBPRM   INTEGER       1     INPUT   Max number of processes
!     BPRNAM  CHARACTER*(*) *     INPUT   Name of processes
!     BPRTXT  CHARACTER*(*) *     INPUT   Text of processes
!     MODNAM  CHARACTER*(*) *     INPUT   Name of module of processes
!     NSVAI   INTEGER       *     INPUT   No of input vars per proces
!     VAINAM  CHARACTER*(*) *     INPUT   Name of input variable
!     VAITXT  CHARACTER*(*) *     INPUT   Text of input variable
!     VAIDEF  REAL          *,*   INPUT   Default values input variables
!     NSVAO   INTEGER       *     INPUT   No of output vars per proces
!     VAONAM  CHARACTER*(*) *     INPUT   Name of input variable
!     VAOTXT  CHARACTER*(*) *     INPUT   Text of output variable
!     NBFL    INTEGER       *     INPUT   No of basic fluxes per proces
!     BFLNAM  CHARACTER*(*) *     INPUT   Name of basix fluxe
!     BFLTXT  CHARACTER*(*) *     INPUT   Text of basix fluxe
!     NBST    INTEGER       *     INPUT   No of basic stochis per proces
!     GENBST  CHARACTER*(*) *,*   INPUT   Name of substance in stochi
!     FLXBST  CHARACTER*(*) *,*   INPUT   Name of flux in stochi
!     STOBST  REAL          *,*   INPUT   Stochimetric factor
!     IPVAI   INTEGER       *     INPUT   Pointers for arrays on VAI
!     IPVAO   INTEGER       *     INPUT   Pointers for arrays on VAO
!     IPBFL   INTEGER       *     INPUT   Pointers for arrays on BFL
!     IPBST   INTEGER       *     INPUT   Pointers for arrays on BST
!     NSVXI   INTEGER       *     INPUT   No of input vars X per proces
!     IPVXI   INTEGER       *     INPUT   Pointers for arrays on VXI
!     VXINAM  CHARACTER*(*) *     INPUT   Name of input variable X
!     VXITXT  CHARACTER*(*) *     INPUT   Text of input variable X
!     VXIDEF  REAL          *     INPUT   Default values input X variables
!     NSVXO   INTEGER       *     INPUT   No of output vars X per proces
!     IPVXO   INTEGER       *     INPUT   Pointers for arrays on VXO
!     VXONAM  CHARACTER*(*) *     INPUT   Name of output variable X
!     VXOTXT  CHARACTER*(*) *     INPUT   Text of output variable X
!     NSDST   INTEGER       *     INPUT   No of dispersion rules p.proces
!     IPDST   INTEGER       *     INPUT   Pointers for arrays on DST
!     GENDST  CHARACTER*(*) *     INPUT   Name of substance in disp rule
!     OUTDST  CHARACTER*(*) *     INPUT   Name of output item in disp rule
!     STODST  REAL          *     INPUT   factor in dispersion rule
!     NSVST   INTEGER       *     INPUT   No of velocity rules p.proces
!     IPVST   INTEGER       *     INPUT   Pointers for arrays on VST
!     GENVST  CHARACTER*(*) *     INPUT   Name of substance in velo rule
!     OUTVST  CHARACTER*(*) *     INPUT   Name of output item in velo rule
!     STOVST  REAL          *     INPUT   factor in velocity rule
!     ISWITR  INTEGER       *     INPUT   Target dimension indicator
!
!     Declaration of arguments
!
      INTEGER        LUPDEF          , LUREP           ,
     +               NBPR            , NBPRM           ,
     +               SERIAL
      INTEGER        NSVAI(*)        , NSVAO(*)        ,
     +               NBFL(*)         , NBST(*)         ,
     +               IPVAI(*)        , IPVAO(*)        ,
     +               IPBFL(*)        , IPBST(*)        ,
     +               NSVXI(*)        , IPVXI(*)        ,
     +               NSVXO(*)        , IPVXO(*)        ,
     +               NDST(*)         , IPDST(*)        ,
     +               NVST(*)         , IPVST(*)        ,
     +               ISWITR(*)
      REAL           VERSIO
      REAL           VAIDEF(*)       , STOBST(*)       ,
     +               VXIDEF(*)       , STODST(*)       ,
     +               STOVST(*)
      CHARACTER*(*)  BPRNAM(*)       , MODNAM(*)       ,
     +               VAINAM(*)       , VAONAM(*)       ,
     +               BFLNAM(*)       , GENBST(*)       ,
     +               FLXBST(*)       , VXINAM(*)       ,
     +               VXONAM(*)       , GENDST(*)       ,
     +               OUTDST(*)       , GENVST(*)       ,
     +               OUTVST(*)
      CHARACTER*(*)  BPRTXT(*)       , VAITXT(*)       ,
     +               VAOTXT(*)       , BFLTXT(*)       ,
     +               VXITXT(*)       , VXOTXT(*)
!
!     Local
!
      INTEGER        NSV   , NFL   , NST
!
!     Write version number and serial number
!
      WRITE ( LUPDEF ) VERSIO, SERIAL
!
!     Write NBPR  number of proces modules
!
      WRITE ( LUPDEF ) NBPR
!
!     Write name , input vars, output vars and fluxes
!
      DO 700 IP = 1 , NBPR
!
!        Write proces name , module name
!
         WRITE ( LUPDEF )
     +         BPRNAM(IP),BPRTXT(IP)
         WRITE ( LUPDEF ) MODNAM(IP)
!
!        Write the xD target dimension indicator
!
         WRITE ( LUPDEF ) ISWITR(IP)
!
!        Write input variables VAI with defaults
!
         NSV = NSVAI(IP)
         WRITE ( LUPDEF ) NSV
         DO 100 IV = 1 , NSV
            IPV = IPVAI(IP) + IV - 1
            WRITE ( LUPDEF )
     +           VAINAM(IPV), VAIDEF(IPV),VAITXT(IPV)
  100    CONTINUE
!
!        Write input variables VXI with defaults
!
         NSV = NSVXI(IP)
         WRITE ( LUPDEF ) NSV
         DO 150 IV = 1 , NSV
            IPV = IPVXI(IP) + IV - 1
            WRITE ( LUPDEF )
     +           VXINAM(IPV), VXIDEF(IPV),VXITXT(IPV)
  150    CONTINUE
!
!        Write output variables VAO
!
         NSV = NSVAO(IP)
         WRITE ( LUPDEF ) NSV
         DO 200 IVAO = 1 , NSV
            IPV = IPVAO(IP) + IVAO - 1
            WRITE ( LUPDEF )
     +           VAONAM(IPV),VAOTXT(IPV)
  200    CONTINUE
!
!        Write output variables VXO
!
         NSV = NSVXO(IP)
         WRITE ( LUPDEF ) NSV
         DO 250 IVAO = 1 , NSV
            IPV = IPVXO(IP) + IVAO - 1
            WRITE ( LUPDEF )
     +           VXONAM(IPV),VXOTXT(IPV)
  250    CONTINUE
!
!        Write basis fluxes  BFL
!
         NFL = NBFL(IP)
         WRITE ( LUPDEF ) NFL
         DO 300 IFLX = 1 , NFL
            IPV = IPBFL(IP) + IFLX - 1
            WRITE ( LUPDEF )
     +             BFLNAM(IPV),BFLTXT(IPV)
  300    CONTINUE
!
!        Write basis stochiometry  BST
!
         NST = NBST(IP)
         WRITE ( LUPDEF ) NST
         DO 400 IST = 1 , NST
            IPV = IPBST(IP) + IST - 1
            WRITE ( LUPDEF ) GENBST(IPV), FLXBST(IPV),
     +                       STOBST(IPV)
  400    CONTINUE
!
!        Write dispersion rules DST
!
         NST = NDST(IP)
         WRITE ( LUPDEF ) NST
         DO 500 IST = 1 , NST
            IPV = IPDST(IP) + IST - 1
            WRITE ( LUPDEF ) GENDST(IPV), OUTDST(IPV),
     +                       STODST(IPV)
  500    CONTINUE
!
!        Write velocity rules VST
!
         NST = NVST(IP)
         WRITE ( LUPDEF ) NST
         DO 600 IST = 1 , NST
            IPV = IPVST(IP) + IST - 1
            WRITE ( LUPDEF ) GENVST(IPV), OUTVST(IPV),
     +                       STOVST(IPV)
  600    CONTINUE
!
  700 CONTINUE
!
      RETURN
      END
