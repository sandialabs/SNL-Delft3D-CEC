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

      subroutine d40blo ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       BLOOM II algae module

!
!     Description of the module :
!
! Name    T   L I/O   Description                                   Units
! ----    --- -  -    -------------------                            ----

!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      USE      DATA_3DL
      USE      DATA_VTRANS

      IMPLICIT NONE

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , IQ, IFROM, ITO, NOQ1, NOQ2, NOQ3, NOQ4
!
!     Local (species groups arrays are now dimensioned as species/types arrays)
!
!     Name    Type  Length   I/O  Description

!     ALGDM   R     1             Dry matter in algae (gDM/m3)
!     ALGTYP  R     0:20,NTYP     Algae type properties
!     AMMONI  R     1             Ammonium concentration (gN/m3)
!     BIOMAS  R     NTYP          Species biomass (gC/m3)
!     BLDEP   R     1             Bloomdepth (DEPTH averaged over BLSTEP)
!     BLSTEP  R*4   1             Time step Bloom (days)
!     CHLORO  R     1             Total chlorophyl in algae (mg/m3)
!     CGROUP  R     NGRO          Algae species group biomass (gC/m3)
!     CL      R     1             Chlorinity (gCl/m3)
!     DEPTHW  R     1             Depth (m)
!     DAYLEN  R     1             Day length (h)
!     DELTAT  R     1             Time step DELWAQ (d)
!     DEAT4   R*4   1             ??$Necessity to transfer?$
!     EXTALG  R     1             Extinction by algae (1/m)
!     EXTTOT  R     1             Total extinction (1/m)
!     FAUT    R     NTYP          Fraction autolysis per species (-)
!     FDET    R     NTYP          Fraction detritus per species (-)
!     FL(IFPROD)    NTYP_A        Primary production per type (g/m3/d)
!     FL(IFMORT)    NTYP_A        Mortality per type (g/m3/d)
!     FL(IFAUTO)    4             Autolysis fluxes per nutrient (g/m3/d)
!                                 (C, N, P, Si)
!     FL(IFDETR)    4             Detritus production per nutrient
!                                 (C, N, P, Si)  (g/m3/d)
!     FL(IFOOXP)    4             OOx production
!                                 (C, N, P, Si)  (g/m3/d)
!     FL(IFUPTA)    5             Uptake of nutrients
!                                 (CO2, NH4, NO3, PO4, SiOx)  (g/m3/d)
!     FRAMMO  R     1             Fraction of NH4 in N-Uptake (-)
!     FBOD5   R     1             BOD5/BODinf in algae (-)
!     HISTOR  L     1             Indicates call for history element at
!                                 an history output timestep
!     ID      I     1             Week number (-)
!     IFIX    I     NTYP          Flag indicating fixed (attached, immobile algae)
!     ISWVTR  I     1             Switch if 3DL is to be used
!     LIMFAC  R     6             Limiting factors (-)
!     LPRINO  I     1             Saves original value of LPRINT
!     LCOUPL  I     1             Flag for BLOOM II
!     LDUMPO  I     1             Saves original value of IDUMP
!     MRTM1   I     NTYP          Mortality parameter
!     MRTM2   I     NTYP          Mortality parameter
!     MRTB1   I     NTYP          Mortality parameter
!     MRTB2   I     NTYP          Mortality parameter
!     NTYP_A  I     1             Actual number of types
!     NTYP_M  I     1             Limit number of types
!     NGRO_A  I     1             Actual number of groups
!     NGRO_M  I     1             Limit number of groups
!     NSET    I     1             Counter for subroutine SETABC of BLOOM II
!     NITRAT  R     1             Nitrate (gN/m3)
!     NUPTAK  R     1             N-Uptake (gN/m3/d)
!     PHOSPH  R     1             Phosphate (gP/m3)
!     RATGRO  R     NGRO          Effective growth rate per group (1/d)
!     RATMOR  R     NGRO          Effective mortality per group (1/d)
!     RUNNAM  C*12  1             Filename consisting of runid without
!     RADIAT  R     1             Irradiation (W/m2)
!     SILICA  R     1             Silicate (gSi/m3)
!     SWCLIM  I     1             Carbon limitation switch (0 inactive, 1 active)
!     TIMMUL  R     1             Time step multiplyer Bloom call (-)
!     TEMPER  R     1             Temperature (degrees C)
!     TOTNUT  R     4             C, N, P, Si in algae (gX/m3)
!     NUTCON  I*4   8             Nutrients involved in active nutrient constraints
!     FLXCON  I*4   8             Uptake fluxes involved in active nutrient constra
      INTEGER  NTYP_M, NIPFIX, NIPVAR, NOUTLIM, NUNUCOM, NOPFIX
      PARAMETER ( NTYP_M = 30 )
!     NIPFIX      Nr of input items independent of BLOOM types, preceding BLOOM types input
!     NIPVAR      Nr of input items for BLOOM types
      PARAMETER ( NIPFIX = 28 , NIPVAR= 26 )
      PARAMETER ( NOPFIX = 29 )
      PARAMETER (NUNUCOM = 8)
      PARAMETER (NOUTLIM = NUNUCOM + 2 + 2*NTYP_M)
      REAL     BIOMAS(NTYP_M), FAUT  (NTYP_M), FDET  (NTYP_M),
     1         ALGTYP(0:20,NTYP_M), MRTM1(NTYP_M), MRTM2(NTYP_M),
     2         MRTB1(NTYP_M), MRTB2(NTYP_M), CGROUP(NTYP_M)
      INTEGER  IFIX(NTYP_M)
      REAL     RATGRO(NTYP_M), RATMOR(NTYP_M)
      CHARACTER*12    RUNNAM
      LOGICAL  HISTOR, THIS, LMIXO,LFIXN,LCARB
      INTEGER  NTYP_A, NGRO_A,
     J         NSET  , LCOUPL, LPRINO, LDUMPO, ID
      REAL     TIMMUL, TEMPER, RADIAT, DEPTHW, DEPTH,  DAYLEN,
     J         AMMONI, NITRAT, PHOSPH, SILICA, DELTAT, BLSTEP,
     J         EXTTOT, DEAT4 , NUPTAK, FRAMMO, FBOD5 , EXTALG,
     J         CHLORO, TOTNUT(4)     ,                 ALGDM ,
     J         THRNH4, THRNO3, THRPO4, THRSI , RCRESP, TCRESP,
     M         BLDEP , CL    , TIC   , CO2   ,         CO2LIM,
     j         PPMCO2, DETN  , DETP  , RDCNT , SDMIXN, VOLUME
      REAL  :: LIMFAC(6)
      INTEGER  IP1 , IP2 , IP3 , IP4 , IP5 , IP6 , IP7 , IP8 , IP9 ,
     J         IP10, IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18,
     J         IP19, IP20, IP21, IP22, IP23, IP24, IP25, IP26, IP27,
     J         IP28
      INTEGER  IO(NOPFIX)
      INTEGER  NOSEGW, NOLAY, NOSEGL, IKMRK1, IKMRK2
      integer  ipo17, ipo18, ipo19
      integer  ino17, ino18, ino19
      INTEGER  INIT , IFLUX, ISEG, IALG, IOFF, IP, IGRO
      INTEGER  IFAUTO, IFDETR, IFOOXP, IFUPTA, IFPROD, IFMORT
      INTEGER  ISWVTR
      INTEGER  SWBLOOMOUT
      INTEGER  SWCLIM
      INTEGER  LUNREP
      CHARACTER CDUMMY
      REAL*8 ORG_AVAILN
      INTEGER NUTCON(NUNUCOM), FLXCON(NUNUCOM), CON2OUT(NUNUCOM)
      REAL    OUTLIM(NOUTLIM)
!
!     JVB much more variables needs to be saved, for the time being all
!
!     SAVE     INIT,RDCNT,ID
      SAVE
!
      DATA     INIT   / 1 /
      DATA     NSET   / 0 /
      DATA     LCOUPL / 1 /
!
      IF ( INIT .EQ. 1 ) THEN
         INIT = 0
         TIMMUL = PMSA(IPOINT(1))
         DELTAT = PMSA(IPOINT(19))
         BLSTEP = TIMMUL * DELTAT
         RDCNT  = - BLSTEP
         ID = 0
         SWCLIM = NINT(PMSA(IPOINT(28)))
         IF (INCREM(28).NE.0) CALL BLSTOP('SWCLIM',ID)
         LCARB = .FALSE.
         IF (SWCLIM.GT.0) LCARB = .TRUE.

!     Set logical numbers and open autonomous I/O files Bloom
         RUNNAM = 'bloominp.XXX'
         CALL BLFILE (RUNNAM)

!        Copy algae type properties for input
         DO 40 IALG=1,NTYP_M
!          BLOOMALG
           IP = NIPFIX + IALG
           IP = IPOINT(IP)
!
!          Hier ook voor ulva van (g) naar (g/m3) lijkt me niet wordt
!          hier alleen naar negatieve waarde gekeken
!
           ALGTYP(0,IALG) = PMSA(IP)
!          SPECALG
           IP = NIPFIX + 1*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('SpecAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(1,IALG) = NINT(PMSA(IP))
!          FAUTALG
           IP = NIPFIX + 2*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('FrAutAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(15,IALG) = PMSA(IP)
!          EXTVLALG
           IP = NIPFIX + 4*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('ExtVlAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(2,IALG) = PMSA(IP)
!          DMCFALG
           IP = NIPFIX + 5*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('DMCFAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(3,IALG) = PMSA(IP)
!          NCRALG
           IP = NIPFIX + 6*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('NCRAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(4,IALG) = PMSA(IP)
!          PCRALG
           IP = NIPFIX + 7*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('PCRAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(5,IALG) = PMSA(IP)
!          SCRALG
           IP = NIPFIX + 8*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('SCRAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(6,IALG) = PMSA(IP)
!          XNCRALG
           IP = NIPFIX + 9*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('XNCRAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(16,IALG) = PMSA(IP)
!          XPCRALG
           IP = NIPFIX +10*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('XPCRAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(17,IALG) = PMSA(IP)
!          FNCRALG
           IP = NIPFIX +11*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('FNCRAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(18,IALG) = PMSA(IP)
!          CHLACALG
           IP = NIPFIX +12*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('ChlaCAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(7,IALG) = PMSA(IP)
!          PPMAXALG
           IP = NIPFIX + 13*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('PPMaxAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(8,IALG) = PMSA(IP)
!          TCPMXALG
           IP = NIPFIX + 14*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('TcPMxAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(9,IALG) = PMSA(IP)
!          TFPMXALG
           IP = NIPFIX + 15*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('TFPMxAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(10,IALG) = PMSA(IP)
!          MORT0ALG
           IP = NIPFIX + 16*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('Mort0Alg',IALG)
           IP = IPOINT(IP)
           ALGTYP(11,IALG) = PMSA(IP)
!          TCMRTALG
           IP = NIPFIX + 17*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('TcMrtAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(12,IALG) = PMSA(IP)
!          MRESPALG
           IP = NIPFIX + 18*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('MRespAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(13,IALG) = PMSA(IP)
!          TCRSPALG
           IP = NIPFIX + 19*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('TcRspAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(14,IALG) = PMSA(IP)
!          SDMIXALG
           IP = NIPFIX + 20*NTYP_M + IALG
!jvb       set SDMIX for all types, time/space dependent
!jvb       IF (INCREM(IP).NE.0) CALL BLSTOP('SDMixAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(19,IALG) = PMSA(IP)
!          MRTEXALG
           IP = NIPFIX + 21*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('MrtExAlg',IALG)
           IP = IPOINT(IP)
           ALGTYP(20,IALG) = PMSA(IP)
!          FIXALG
           IP = NIPFIX + 25*NTYP_M + IALG
           IF (INCREM(IP).NE.0) CALL BLSTOP('FixAlg',IALG)
           IP = IPOINT(IP)
           IFIX(IALG) = NINT(PMSA(IP))
   40    CONTINUE

!     Read BLOOM-input and set some parameters
!     JvG 11102013 set NUNUCO dependent of LCARB

         CALL BLINPU (NTYP_M, NTYP_A, NGRO_A, ALGTYP, LMIXO , LFIXN ,
     J                LCARB , NUNUCOM, NUTCON, FLXCON, CON2OUT)
         IF (NTYP_A.GT.NTYP_M) GOTO 901

!     set common CBLBAL communication with balance routines

         CALL IBLBAL ( NTYP_M, NTYP_A, ALGTYP, IPOINT(NIPFIX+1))

!     Initialize BLOOM (Unit conversions and filling of A-matrix)

         CALL BLINIT (LPRINO,LDUMPO)

!     initialise 3DLight data

         IF ( NOQ3 .GT. 0 ) THEN
            CALL DHNOSEG(NOSEGW)
            CALL DHNOLAY(NOLAY)
            NOSEGL = NOSEGW/NOLAY
            IF ( NOSEGL*NOLAY .NE. NOSEGW ) THEN
               CALL GETMLU(LUNREP)
               WRITE(LUNREP,*) ' WARNING unstructured 3D application'
               WRITE(LUNREP,*) ' BLOOM 3D light approach not possible'
               ACTIVE_3DL = .FALSE.
               NOLAY = 1
            ELSE
               ACTIVE_3DL = .TRUE.
               IF ( .NOT. ACTIVE_3DL ) THEN ! to trick something in the debugger, (don't) use the variables here
                  NOSEG_3DL  = 0                ! number of segments, copy of NOSEG
                  NOSEGL_3DL = 0                ! number of segments per layer
                  NOLAY_3DL  = 0                ! number of layers
                  NGRO_3DL   = 0                ! number of BLOOM algae groups, copy of NGRO_A
                  ISEG_3DL   = 0                ! actual segment for which bloom is called
                  ILAY_3DL   = 0                ! actual layer for which bloom is called
                  ACTIVE_3DL = .FALSE.          ! switch indicating if 3DL functionality is active
                  EFFIC_3DL  = 0.0
               ENDIF
            ENDIF
         ELSE
            NOSEGW     = NOSEG
            NOSEGL     = NOSEG
            ACTIVE_3DL = .FALSE.
            NOLAY = 1
         ENDIF
         CALL INIT_3DL( NOSEG, NOSEGW, NOSEGL, NOLAY, NGRO_A, NTYP_A )
         ALLOCATE(IFIX_3DL(NTYP_A))
         IFIX_3DL=IFIX

!     Return after initialization

         RETURN

      ENDIF

      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
      IP8  = IPOINT( 8)
      IP9  = IPOINT( 9)
      IP10 = IPOINT(10)
      IP11 = IPOINT(11)
      IP12 = IPOINT(12)
      IP13 = IPOINT(13)
      IP14 = IPOINT(14)
      IP15 = IPOINT(15)
      IP16 = IPOINT(16)
      IP17 = IPOINT(17)
      IP18 = IPOINT(18)
      IP19 = IPOINT(19)
      IP20 = IPOINT(20)
      IP21 = IPOINT(21)
      IP22 = IPOINT(22)
      IP23 = IPOINT(23)
      IP24 = IPOINT(24)
      IP25 = IPOINT(25)
      IP26 = IPOINT(26)
      IP27 = IPOINT(27)
      IP28 = IPOINT(28)

      DO IP = 1,NOPFIX
          IO(IP) = IPOINT(NIPFIX+NIPVAR*NTYP_M+IP)
      ENDDO
!
      ISWVTR = NINT(PMSA(IPOINT(24)))
      IF ( ACTIVE_3DL .AND. ISWVTR .EQ. 0 ) THEN
         CALL GETMLU(LUNREP)
         WRITE(LUNREP,*) ' WARNING vertical distribution not active'
         WRITE(LUNREP,*) ' BLOOM 3D light approach not possible'
         ACTIVE_3DL = .FALSE.
      ENDIF

      TIMMUL = PMSA(IP1 )
      DELTAT = PMSA(IP19)
      BLSTEP = TIMMUL * DELTAT
      RDCNT  = RDCNT + BLSTEP
      IF ((AINT(RDCNT / 7.) + 1).NE.ID) THEN
        THIS   = .TRUE.
        ID = AINT(RDCNT / 7.) + 1
        IF (ID.GT.52) THEN
          ID = ID - 52
          RDCNT = RDCNT - 52. * 7.
        ENDIF
      ELSE
        THIS   = .FALSE.
      ENDIF

!     First segment loop set efficiencies

      DO ISEG = 1 , NOSEG
         CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!       IF (IKMRK1.EQ.1) THEN
         IF (BTEST(IKNMRK(ISEG),0)) THEN
            CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
            ISEG_3DL = ISEG
            ILAY_3DL = (ISEG-1)/NOSEGL_3DL+1
            EXTTOT = PMSA(IP2 )
            EXTALG = PMSA(IP3 )
            TEMPER = PMSA(IP4 )
            RADIAT = PMSA(IP5 ) * 60.48
            IF ( IKMRK1 .EQ. 3 ) THEN
               RADIAT = 0.0                    ! WAQ-G bodem geen groei
               CALL BL_NO_AUTOLYSE(ORG_AVAILN) ! WAQ-G bodem geen autolyse
            ENDIF
            DEPTH  = PMSA(IP6 )
            BLDEP  = PMSA(IP7 )
            DAYLEN = PMSA(IP8 ) * 24.
            DEPTHW = DEPTH
            IF (BLDEP.GT.0.) DEPTHW = BLDEP
            CL     = PMSA(IP22)

            DO IALG = 1,NTYP_A

!jvb           set SDMIX for all types, time/space dependent
!              SDMIXALG
               IOFF = NIPFIX + 20*NTYP_M + IALG
               IP = IPOINT(IOFF) + (ISEG-1)*INCREM(IOFF)
               SDMIXN = PMSA(IP)
               CALL BLSSDM(IALG,SDMIXN)
!jvb
               IF (IFIX(IALG).LT.0) THEN
!
!                 No PP for fixed ulva in non bottom segment, unless sdmix is set positive for this segment
!
                  IF ( SDMIXN .LT. -1.E-10 ) THEN
                     IF ((IKMRK2.EQ.1).OR.(IKMRK2.EQ.2)) THEN
                        CALL BLSPPM(IALG,0.0)
                     ENDIF
                  ELSEIF ( SDMIXN .LT. 1.E-10 ) THEN
                     CALL BLSPPM(IALG,0.0)
                  ENDIF
               ENDIF

               IOFF = NIPFIX + NTYP_M*22
               IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
               MRTM2 (IALG) = PMSA(IP)
               IOFF = NIPFIX + NTYP_M*23
               IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
               MRTB1 (IALG) = PMSA(IP)
               IOFF = NIPFIX + NTYP_M*24
               IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
               MRTB2 (IALG) = PMSA(IP)
            ENDDO
            CALL BLCLST (MRTM1,MRTM2,MRTB1,MRTB2,NTYP_A,CL)

            CALL SET_EFFI( TEMPER, RADIAT, EXTTOT, DEPTHW, DAYLEN,
     +                     ID    )

            IF ( IKMRK1 .EQ. 3 ) THEN
               CALL BL_RESTORE_AUTOLYSE(ORG_AVAILN) ! WAQ-G restore autolyse
            ENDIF
            CALL BLCLRS (MRTM1,NTYP_A)
!nt2        reset PPMAX anyhow
            DO IALG = 1,NTYP_A
               CALL BLSPPM(IALG,ALGTYP(8,IALG))
            ENDDO
         ENDIF
         IP2  = IP2  + INCREM( 2)
         IP3  = IP3  + INCREM( 3)
         IP4  = IP4  + INCREM( 4)
         IP5  = IP5  + INCREM( 5)
         IP6  = IP6  + INCREM( 6)
         IP7  = IP7  + INCREM( 7)
         IP8  = IP8  + INCREM( 8)
         IP22 = IP22 + INCREM(22)
         IP25 = IP25 + INCREM(25)
         IP26 = IP26 + INCREM(26)
         IP27 = IP27 + INCREM(27)
         IP28 = IP28 + INCREM(28)
      ENDDO
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
      IP8  = IPOINT( 8)
      IP22 = IPOINT(22)
      IP25 = IPOINT(25)
      IP26 = IPOINT(26)
      IP27 = IPOINT(27)
      IP28 = IPOINT(28)

!     Second segment loop, actual bloom call

      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
      CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
      IF (IKMRK1.EQ.1 .OR. IKMRK1.EQ.3) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
!
      ISEG_3DL = ISEG
      ILAY_3DL = (ISEG-1)/NOSEGL_3DL+1
!
      TIMMUL = PMSA(IP1 )
      EXTTOT = PMSA(IP2 )
      EXTALG = PMSA(IP3 )
      TEMPER = PMSA(IP4 )

!     Conversion from standard Delwaq 4.0 [W/m2] to [J/cm2/week] for Bloom

      RADIAT = PMSA(IP5 ) * 60.48
      IF ( IKMRK1 .EQ. 3 ) THEN
         RADIAT = 0.0                    ! WAQ-G bodem geen groei
         CALL BL_NO_AUTOLYSE(ORG_AVAILN) ! WAQ-G bodem no autolyse
      ENDIF
      DEPTH  = PMSA(IP6 )
      BLDEP  = PMSA(IP7 )
!     Replace DEPTHW with BLDEP if BLDEP > 0.0
      DEPTHW = DEPTH
      IF (BLDEP.GT.0.) DEPTHW = BLDEP

!     Conversion from standard Delwaq 4.0 [d] to [h] for Bloom

      DAYLEN = PMSA(IP8 ) * 24.
      IF (DAYLEN.GT.24.) GOTO 903
      AMMONI = PMSA(IP9 )
      NITRAT = PMSA(IP10)
      PHOSPH = PMSA(IP11)
      SILICA = PMSA(IP12)
      THRNH4 = PMSA(IP13)
      THRNO3 = PMSA(IP14)
      THRPO4 = PMSA(IP15)
      THRSI  = PMSA(IP16)
      DETN   = PMSA(IP17)
      DETP   = PMSA(IP18)
      DELTAT = PMSA(IP19)
      SWBLOOMOUT = NINT(PMSA(IP20))
      IF ((SWBLOOMOUT.NE.0).AND.(THIS)) THEN
        HISTOR = .TRUE.
      ELSE
        HISTOR = .FALSE.
      ENDIF
      CL     = PMSA(IP22)
      VOLUME = PMSA(IP23)
      TIC      = MAX(0.0,PMSA(IP25))
      CO2      = MAX(0.0,PMSA(IP26))

!     SUBTRACT THRESHOLDS FROM DISSOLVED CONCENTRATION, NOT BELOW ZERO,
!     BUT BELOW ZERO IF ORIGINAL CONCENTRATION BELOW ZERO
      AMMONI = MAX(MIN(AMMONI,0.0),AMMONI - THRNH4)
      NITRAT = MAX(MIN(NITRAT,0.0),NITRAT - THRNO3)
      PHOSPH = MAX(MIN(PHOSPH,0.0),PHOSPH - THRPO4)
      SILICA = MAX(MIN(SILICA,0.0),SILICA - THRSI )

      DO 20 IALG = 1,NTYP_A

!jvb     set SDMIX for all types, time/space dependent
!        SDMIXALG
         IOFF = NIPFIX + 20*NTYP_M + IALG
         IP = IPOINT(IOFF) + (ISEG-1)*INCREM(IOFF)
         SDMIXN = PMSA(IP)
         CALL BLSSDM(IALG,SDMIXN)
!jvb
!         scale ulva from (g/m2) to (g/m3)
!
          IOFF = NIPFIX
          IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
          IF (IFIX(IALG).LT.0) THEN
             BIOMAS(IALG) = PMSA(IP)/DEPTH
!
!            No PP for fixed ulva in non bottom segment, unless sdmix is set positive for this segment
!
             IF ( SDMIXN .LT. -1.E-10 ) THEN
                IF ((IKMRK2.EQ.1).OR.(IKMRK2.EQ.2)) THEN
                   CALL BLSPPM(IALG,0.0)
                ENDIF
             ELSEIF ( SDMIXN .LT. 1.E-10 ) THEN
                CALL BLSPPM(IALG,0.0)
             ENDIF
          ELSE
             BIOMAS(IALG) = PMSA(IP)
          ENDIF
!jvb
          IOFF = NIPFIX + NTYP_M*2
          IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
          FAUT  (IALG) = PMSA(IP)
          IOFF = NIPFIX + NTYP_M*3
          IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
          FDET  (IALG) = PMSA(IP)
          IOFF = NIPFIX + NTYP_M*22
          IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
          MRTM2 (IALG) = PMSA(IP)
          IOFF = NIPFIX + NTYP_M*23
          IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
          MRTB1 (IALG) = PMSA(IP)
          IOFF = NIPFIX + NTYP_M*24
          IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
          MRTB2 (IALG) = PMSA(IP)

          IF ( IKMRK1 .EQ. 3 ) THEN ! WAQG bodem no autolyses
             FDET(IALG) = FDET(IALG)+FAUT(IALG)
             FAUT(IALG) = 0.0
          ENDIF

   20 CONTINUE

      IFAUTO = IFLUX + 1
      IFDETR = IFLUX + 5
      IFOOXP = IFLUX + 9
      IFUPTA = IFLUX + 13
      IFPROD = IFLUX + 23
      IFMORT = IFLUX + 23 + NTYP_M
!
!     Set output control variables
!     $ How can we couple this to the DELWAQ history flag?
!     HISTOR should be true for history elements at history times
!     Present .true. gives independent output of Bloom
!     .false. prohibits independent output of Bloom


!JVB  tijdelijk altijd om de weekcyclus te negeren
!jvb  HISTOR = .TRUE.
      CALL BLOUTC (HISTOR,LPRINO,LDUMPO)

!     Salinity dependend mortality
!     Adapt mortality rates

      CALL BLCLST (MRTM1,MRTM2,MRTB1,MRTB2,NTYP_A,CL)

!     Compute mortality

      CALL BLMORT ( BIOMAS        , TEMPER        , FAUT          ,
     J              FDET          , FL(IFAUTO)    , FL(IFDETR)    ,
     J              FL(IFOOXP)    , FL(IFMORT)    , DEAT4         ,
     J              BLSTEP        , LMIXO         , LFIXN         ,
     J              LCARB         , NUTCON        , FLXCON        )

!     Compute primary production and nutrient uptake

      CALL BLPRIM ( BIOMAS        , AMMONI        , NITRAT        ,
     J              PHOSPH        , SILICA        , DETN          ,
     M              DETP          ,                 CO2           , 
     J              TIC           , FL(IFMORT)    ,
     J              FL(IFDETR)    , BLSTEP        , EXTTOT        ,
     J              EXTALG        , TEMPER        , RADIAT        ,
     J              DEPTHW        , DAYLEN        , ID            ,
     J              LCOUPL        , NSET          , DEAT4         ,
     J              TOTNUT        , CHLORO        , FL(IFPROD)    ,
     J              FL(IFUPTA)    , LIMFAC        , NUPTAK        ,
     J              FRAMMO        , FBOD5         , RATGRO        ,
     J              RATMOR        , ALGDM         , ISEG          ,
     J              CGROUP        , LMIXO         , LFIXN         ,
     J              LCARB         , NUTCON        , FLXCON        , 
     J              NOUTLIM       , OUTLIM        , NUNUCOM       , 
     J              NTYP_M        , CON2OUT                       )

!     Copy C-uptake flux to seperate flux for Oxygen

      IF ( NINT(PMSA(IP21)).EQ.0 )
     JFL(IFUPTA+9) = FL(IFUPTA)

!     Salinity dependend mortality
!     Reset mortality rates

      CALL BLCLRS (MRTM1,NTYP_A)

!nt2  reset PPMAX anyhow
      DO IALG = 1,NTYP_A
         CALL BLSPPM(IALG,ALGTYP(8,IALG))
      ENDDO

      IF ( IKMRK1 .EQ. 3 ) THEN
         CALL BL_RESTORE_AUTOLYSE(ORG_AVAILN) ! WAQ-G restore autolyse
      ENDIF

      PMSA(IO(1 )) = NUPTAK
      PMSA(IO(2 )) = FRAMMO
      PMSA(IO(3 )) = TOTNUT(1)
      PMSA(IO(4 )) = TOTNUT(2)
      PMSA(IO(5 )) = TOTNUT(3)
      PMSA(IO(6 )) = TOTNUT(4)
      PMSA(IO(7 )) = ALGDM
      PMSA(IO(8 )) = FBOD5
      PMSA(IO(9 )) = CHLORO
      PMSA(IO(10)) = CHLORO
      PMSA(IO(11)) = LIMFAC(1)
      PMSA(IO(12)) = LIMFAC(2)
      PMSA(IO(13)) = LIMFAC(3)
      PMSA(IO(14)) = LIMFAC(4)
      PMSA(IO(15)) = LIMFAC(5)
      PMSA(IO(16)) = LIMFAC(6)
      PMSA(IO(17)) = FL(IFUPTA)*DEPTHW

!     RECONSTRUCT RESPIRATION FLUXES
      PMSA(IO(18)) = 0.0
      DO IGRO = 1,NTYP_A
          RCRESP = ALGTYP(13,IGRO)
          TCRESP = ALGTYP(14,IGRO)
          PMSA(IO(18)) = PMSA(IO(18)) 
     J                 + RCRESP*TCRESP**TEMPER*BIOMAS(IGRO)
      ENDDO
      PMSA(IO(18)) = PMSA(IO(18))*DEPTHW
      PMSA(IO(19)) = FL(IFUPTA+7)*DEPTHW
      
!     New limitation factors (nutrients + light)
      DO IP = 1,NUNUCOM+2
          PMSA(IO(19+IP)) = OUTLIM(IP)
      ENDDO

!     Growth rate of all groups
      IOFF = NIPFIX + NIPVAR*NTYP_M + NOPFIX
      DO IGRO = 1,NTYP_A
          IP = IPOINT(IOFF+IGRO) + (ISEG-1)*INCREM(IOFF+IGRO)
          PMSA(IP) = RATGRO(IGRO)
      ENDDO

!     Mortality rate of all groups
      IOFF = NIPFIX + NIPVAR*NTYP_M + NOPFIX + NTYP_M
      DO IGRO = 1,NTYP_A
          IP = IPOINT(IOFF+IGRO) + (ISEG-1)*INCREM(IOFF+IGRO)
          PMSA(IP) = RATMOR(IGRO)
      ENDDO
      
!     Biomass of all groups
      IOFF = NIPFIX + NIPVAR*NTYP_M + NOPFIX + 2*NTYP_M
      DO IGRO = 1,NTYP_A
          IP = IPOINT(IOFF+IGRO) + (ISEG-1)*INCREM(IOFF+IGRO)
          PMSA(IP) = CGROUP(IGRO)
      ENDDO

!     Growth limitation of all groups
      IOFF = NIPFIX + NIPVAR*NTYP_M + NOPFIX + 3*NTYP_M
      DO IGRO = 1,NTYP_A
          IP = IPOINT(IOFF+IGRO) + (ISEG-1)*INCREM(IOFF+IGRO)
          PMSA(IP) = OUTLIM(NUNUCOM+2+IGRO)
      ENDDO

!     Mort limitation of all groups
      IOFF = NIPFIX + NIPVAR*NTYP_M + NOPFIX + 4*NTYP_M
      DO IGRO = 1,NTYP_A
          IP = IPOINT(IOFF+IGRO) + (ISEG-1)*INCREM(IOFF+IGRO)
          PMSA(IP) = OUTLIM(NUNUCOM+2+NTYP_M+IGRO)
      ENDDO

      ENDIF
!
      IFLUX = IFLUX + NOFLUX
!
      IP1  = IP1  + INCREM( 1)
      IP2  = IP2  + INCREM( 2)
      IP3  = IP3  + INCREM( 3)
      IP4  = IP4  + INCREM( 4)
      IP5  = IP5  + INCREM( 5)
      IP6  = IP6  + INCREM( 6)
      IP7  = IP7  + INCREM( 7)
      IP8  = IP8  + INCREM( 8)
      IP9  = IP9  + INCREM( 9)
      IP10 = IP10 + INCREM(10)
      IP11 = IP11 + INCREM(11)
      IP12 = IP12 + INCREM(12)
      IP13 = IP13 + INCREM(13)
      IP14 = IP14 + INCREM(14)
      IP15 = IP15 + INCREM(15)
      IP16 = IP16 + INCREM(16)
      IP17 = IP17 + INCREM(17)
      IP18 = IP18 + INCREM(18)
      IP19 = IP19 + INCREM(19)
      IP20 = IP20 + INCREM(20)
      IP21 = IP21 + INCREM(21)
      IP22 = IP22 + INCREM(22)
      IP23 = IP23 + INCREM(23)
      IP24 = IP24 + INCREM(24)
      IP25 = IP25 + INCREM(25)
      IP26 = IP26 + INCREM(26)
      IP27 = IP27 + INCREM(27)
      IP28 = IP28 + INCREM(28)

      DO IP = 1,NOPFIX
          IO(IP) = IO(IP) + INCREM(NIPFIX+NIPVAR*NTYP_M+IP)
      ENDDO
!
 9000 CONTINUE

      ! cummulate output per square metre over the depth towards lowest layer

      ipo17 = ipoint(nipfix+nipvar*ntyp_m+17)
      ino17 = increm(nipfix+nipvar*ntyp_m+17)
      ipo18 = ipoint(nipfix+nipvar*ntyp_m+18)
      ino18 = increm(nipfix+nipvar*ntyp_m+18)
      ipo19 = ipoint(nipfix+nipvar*ntyp_m+19)
      ino19 = increm(nipfix+nipvar*ntyp_m+19)

      do iq = noq1+noq2+1 , noq1+noq2+noq3
         ifrom = iexpnt(1,iq)
         ito   = iexpnt(2,iq)
         if ( ifrom.gt.0 .and. ito.gt.0 ) then
            pmsa(ipo17+(ito-1)*ino17) = pmsa(ipo17+(ifrom-1)*ino17) + pmsa(ipo17+(ito-1)*ino17)
            pmsa(ipo18+(ito-1)*ino18) = pmsa(ipo18+(ifrom-1)*ino18) + pmsa(ipo18+(ito-1)*ino18)
            pmsa(ipo19+(ito-1)*ino19) = pmsa(ipo19+(ifrom-1)*ino19) + pmsa(ipo19+(ito-1)*ino19)
         endif
      enddo

      ! set the accumulated value for every layer

      do iq = noq1+noq2+noq3, noq1+noq2+1,-1
         ifrom = iexpnt(1,iq)
         ito   = iexpnt(2,iq)
         if ( ifrom.gt.0 .and. ito.gt.0 ) then
            pmsa(ipo17+(ifrom-1)*ino17) = pmsa(ipo17+(ito-1)*ino17)
            pmsa(ipo18+(ifrom-1)*ino18) = pmsa(ipo18+(ito-1)*ino18)
            pmsa(ipo19+(ifrom-1)*ino19) = pmsa(ipo19+(ito-1)*ino19)
         endif
      enddo

      RETURN
!
  901 STOP 'ERROR D40BLO: DIMENSION NTYP_M TOO SMALL'
! 902 STOP 'ERROR D40BLO: DIMENSION NGRO_M TOO SMALL'
  903 STOP 'ERROR D40BLO: DAYLEN > 1.0 DAY'
      END

      SUBROUTINE BLSTOP(MES,I)

      CHARACTER*12 MES

      WRITE(*,*) 'ERROR IN BLOOM: '
      WRITE(*,*) 'CHARACTERISTIC ',MES,' FOR ALGAE TYPE ',I,
     1           ' MUST BE A CONSTANT!'

      CALL SRSTOP(1)

      RETURN

      END
      SUBROUTINE BL_NO_AUTOLYSE(ORG_AVAILN)
      REAL*8 ORG_AVAILN
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      ORG_AVAILN = AVAILN
      AVAILN     = 0.0
      END
      SUBROUTINE BL_RESTORE_AUTOLYSE(ORG_AVAILN)
      REAL*8 ORG_AVAILN
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      AVAILN     = ORG_AVAILN
      END
