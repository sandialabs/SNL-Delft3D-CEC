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

!
!  *********************************************************************
!  *         SUBROUTINE TO CALCULATE GRAZING RATE CONSTANTS            *
!  *                  MULTIPLE SPECIES                                 *
!  *********************************************************************
!
      SUBROUTINE CONSB2(X,GRAMOR,GRADET,ZOOD,ZOOC,ITNUM,LGRAZ,T,
     1                  PERIOD,GRAMX,LCOUPL,ZMAX)

!**** Name   Type Size   I/O Description
!**** ------ ---- ------ --- -------------------------------------------------
!**** AA     R*8  NUNUCO*I   BLOOM algae stoichiometry matrix excluding
!****             NUSPEC     carbon
!**** ALGFLX R*8  NUSPEC Total algal flux (mg C/l.period) [POSITIVE = DECREASE ]
!**** ALGSTC R*8  NTONUT*    Algae stoichiometry matrix including carbon
!****             NUSPEC
!**** ALTFLX R*8  NTONUT Total algal flux (mg/l.d) [POSITIVE = ALGAE DECREASE ]
!**** BOTFLX R*8  NTONUT Total bottom detritus flux (mg/l.period)
!**** CDETR  R*8  NTONUT     detritus concentration for the nutrients
!****                        including carbon (g/m3)
!**** CTODRY R*8  NUSPEC I   dry weigth to carbon ratio of the algae species
!**** DETRIT R*8  NUNUCO I   BLOOM detritus concentration for the
!****                        nutrients excluding carbon (mg/m3)
!**** DETFLX R*8  NTONUT     Flux of nutrients from detritus
!**** DISFLX R*8  NTONUT Total dissolved nutrients flux (mg/l.period)
!**** GDETPR R*8  NUGRAZ I   Preference for detritus
!**** GDETFF R*8  NUGRAZ I   Feacal fraction for detritus
!**** GDETO  R*8  NUNUCO     First order decay rate of detritus due to grazing
!****                        for the previous iteration step
!**** GFECFR R*8  NUSPEC*I   Feacal fraction for algae types
!****             NUGRAZ
!**** GMORO  R*8  NUSPEC     First order decay rate of algae due to grazing
!****                        for the previous iteration step
!**** GRADET R*8  NUNUCO O   First order decay rate of detritus due to grazing
!**** GRAMOR R*8  NUSPEC O   First order decay rate of algae due to grazing
!**** GRAMX  R*8  NUSPEC I   Maximum sustainable grazing rate
!**** GRZBRU R*8         Bruto growth rate grazer (mg C/l.d)
!**** GRZFIL R*8             Realized filtration rate
!**** GRZFOO R*8         Available food (mg C/l)
!**** GRZFM  R*8  NUGRAZ I   Maximum clearance rate grazer (l/mg C.d)
!**** GRZGM  R*8  NUGRAZ I   Maximum relative growth grazer (1/d)
!**** GRZGRZ R*8         Grazing rate (1/d)
!**** GRZMET R*8         Respiration grazer (1/d)
!**** GRZMM  R*8  NUGRAZ I   Maximum relative mortality grazer (1/d)
!**** GRZMO  R*8  NUGRAZ I   Monod term filtr.r. in rel. to foodconc. (mg C/l)
!**** GRZNEW R*8  NUGRAZ     biomass of the grazers provided for the current
!****                        timestep (gC/m3) - can be adapted
!**** GRZOLD R*8  NUGRAZ     biomass of the grazers during the previous
!****                        timestep (gC/m3)
!**** GRZPMX R*8         Max. sustainable bruto growth rate (mg C/l.d)
!**** GRZRM  R*8  NUGRAZ I   Maximum daily ration grazer (mg C/mg C.d)
!**** GRZRAT R*8             Realized feeding rate
!**** GRZRE  R*8  NUGRAZ     Routine respiration coefficient (fraction)
!**** GRZSE  R*8  NUGRAZ I   Standard respiration coefficient (1/d)
!**** GRZST  R*8  NTONUT*    Grazer stoichiometry matrix including carbon
!****             NUGRAZ
!**** GRZUPT R*8  NTONUT Total intake per element (mg/l.d)
!**** GTMPFM R*8  NUGRAZ I   Temperature coefficients filtration rate
!**** GTMPGM R*8  NUGRAZ I   Temperature coefficients maximum growth rate
!**** GTMPMM R*8  NUGRAZ I   Temperature coefficients maximum mortality rate
!**** GTMPRE R*8  NUGRAZ I   Temperature coefficients routine metabolism
!**** GTMPRM R*8  NUGRAZ I   Temperature coefficients feeding rate
!**** GTMPSE R*8  NUGRAZ I   Temperature coefficients standard metabolism
!**** GTODET R*8  NUGRAZ I   Fraction of excretion of grazers to the water colum
!**** IFILSP I*4             Counter in the loop over the grazer types
!**** IPERM  I*4         I   Maximum number of steps in grazing iteration loop
!**** ITNUM  I*4         I   Step number in grazing iteration loop
!**** LCOUPL I*4         I   Switch for stand alone (0) or coupled (1)
!**** LGRAZ  I*4         IO  Switch for start of next iteration step (1)
!**** LZERO  I*4             Switch for zero biomass of food
!**** NTONUT I*4             Number of nutrients for CONSBL, NUNUCO + 1
!**** NUGRAZ I*4         I   Number of grazers
!**** NUNUCO I*4         I   Number of nutrients for BLOOM, excluding carbon
!**** NUSPEC I*4         I   Total number of algae species
!**** PERIOD R*8         I   Duration of period (d)
!**** T      R*8         I   Temperature (degrees Celsius)
!**** TOTGRA R*8             Total grazing rate (1/d)
!**** X      R*8  MX+1   I   A.o. algae biomass (mgC/m3)
!**** ZOOC   R*8  NUGRAZ I   biomass of the grazers during the previous
!****                        timestep (gC/m3)
!**** ZOOD   R*8  NUGRAZ I   biomass of the grazers provided for the current
!****                        timestep (gC/m3)
!**** ZOONUT R*8  NUNUCO*I   BLOOM grazer stoichiometry matrix excluding
!****             NUGRAZ     carbon
!**** ZOOPR  R*8  NUSPEC*I   Preference for algae types
!****             NUGRAZ
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'graas.inc'
      INCLUDE 'ioblck.inc'
      DIMENSION X(MX),GRAMOR(MT),ZOOD(0:MG),ZOOC(MG),GRZOLD(MG),
     1          GRZNEW(MG),DISFLX(MN+1),DETFLX(MN+1),BOTFLX(MN+1),
     2          ALGFLX(MT),GRZUPT(MN+1),ALGSTC(MN+1,MT),GRZST(MN+1,MG),
     3          DFLUX(MN+1),ALTFLX(MN+1),GRZFLX(MN+1),TOTFLX(MN+1),
     4          CDETR(MN+1),GRADET(MN),GMORO(MT),GDETO(MN)
      SAVE GMORO,GDETO,LZERO

!****
!*  1 Make a copy of the algal and grazers stochiometry
!****
      ZMAX = 1.D7
      IF (ITNUM.LE.1) THEN
        LZERO = 0
      ELSEIF (LZERO.EQ.1) THEN
        LZERO = 0
        LGRAZ = 0
        RETURN
      ENDIF

      NTONUT = NUNUCO + 1
      DO 280 I=1,NUNUCO
        GRADET(I) = 0.0D0
  280 CONTINUE

      IF ((ITNUM.EQ.1).AND.(ITNUM.LT.IPERM)) THEN
        LGRAZ = 1
      ELSE
        LGRAZ = 0
      ENDIF
      DO 210 J = 1, NUSPEC
          GRAMOR(J) = 0.0D0
          ALGSTC(1,J)=1.0
          DO 220 I = 2, NTONUT
              ALGSTC(I,J) = AA(I-1, J) * CTODRY ( J)
  220     CONTINUE
  210 CONTINUE

      DO 230 J = 1, NUGRAZ
          GRZST(1,J)=1.0
          DO 240 I = 2, NTONUT
              GRZST(I,J) = ZOONUT(I-1, J)
  240     CONTINUE
  230 CONTINUE

!     The amount of carbon in detritus is not known in BLOOM
!     Here we estimate the amount from the amount of nitrogen
!     in detritus. The ratio is taken is the average ratio in the
!     current algae species composition

!     Calcuate the average C/N ratio in the algae
      K = NUROWS
      ALGC = 0.0D0
      ALGN = 0.0D0
      DO 260 J=1,NUSPEC
        K = K + 1
        ALGC = ALGC + X(K)
        ALGN = ALGN + X(K) * ALGSTC(2,J)
  260 CONTINUE

!     Convert DET-N to DET-C and fill the rest of the CDETR array
      IF (ALGN.GT.1.0D-6) THEN
        CDETR(1) = 1.D-3 * DETRIT(1) * ALGC / ALGN
      ELSEIF (CDETR(2).GT.1.0D-6) THEN
        CDETR(1) = 1.D-3 * DETRIT(1) * CDETR(1) / CDETR(2)
      ELSEIF (DETRIT(1).LT.1.0D-6) THEN
        CDETR(1) = 0.0
      ELSE
        CDETR(1) = 10.0D-3 * DETRIT(1)
        WRITE(*,*) 'CONSBL: Could not establish C/N ratio detritus!',
     1             ' Used 10.'
      ENDIF
      DO 270 I=2,NTONUT
        CDETR(I) = 1.D-3 * DETRIT(I-1)
  270 CONTINUE

!
!     The routine does not produce if biomass in GRZNE1 (segfun)
!     evers becomes 0.0, so safeguard added 940524 AH
!
      DO 10 J=1,NUGRAZ
        IF (ZOOC(J).LT.1.0D-20) THEN
          GRZOLD(J) = 1.0D-20
        ELSE
          GRZOLD(J) = ZOOC(J)
        ENDIF
        IF (ZOOD(J).LT.1.0D-20) THEN
          GRZNEW(J) = 1.0D-20
        ELSE
          GRZNEW(J) = ZOOD(J)
        ENDIF
   10 CONTINUE

!     IF ( DEBUG ) THEN
!        WRITE ( IDBG, 9000) ( I, GRZOLD(I), GRZNEW(I), I=1,NUGRAZ)
!     ENDIF

!****
!*  2 Loop over the grazers
!****
      DO 200 IFILSP = 1, NUGRAZ

!****
!*  3 Initialize output variables
!****

          DO 40 I = 1, NTONUT
              DISFLX(I) = 0.0D0
              DETFLX(I) = 0.0D0
              BOTFLX(I) = 0.0D0
   40     CONTINUE
          DO 50 I = 1, NUSPEC
              ALGFLX(I) = 0.0D0
   50     CONTINUE

!****
!*  4 Check if grazer growth or mortality exceeds constraints
!****

          IF ((GRZOLD(IFILSP) .GT. 0.0).AND.(LCOUPL.NE.0)) THEN
              IF ((GRZNEW(IFILSP) - GRZOLD(IFILSP)) .GE. 0.0) THEN
                  IF (GRZNEW(IFILSP) .GT. GRZOLD(IFILSP) *
     &            (1.0 + GRZGM(IFILSP) *
     &            EXP(GTMPGM(IFILSP) *
     &            (T - 20.0)) * PERIOD)) THEN
                      GRZNEW(IFILSP) = GRZOLD(IFILSP) *
     &                (1.0 + GRZGM(IFILSP) *
     &                EXP(GTMPGM(IFILSP) *
     &                (T - 20.0)) * PERIOD)
                  ENDIF
              ELSE
                  IF (GRZNEW(IFILSP) .LT. GRZOLD(IFILSP) *
     &            (1.0 - GRZMM(IFILSP) *
     &            EXP(GTMPMM(IFILSP) *
     &            (T - 20.0)) * PERIOD)) THEN
                      GRZNEW(IFILSP) = GRZOLD(IFILSP) *
     &                (1.0 - GRZMM(IFILSP) *
     &                EXP(GTMPMM(IFILSP) *
     &                (T - 20.0)) * PERIOD)
                  ENDIF
              ENDIF
          ENDIF

!****
!*  5 Calculate total available amount of food (mg C/l)
!****
          GRZFOO = CDETR(1) * GDETPR(IFILSP)
          K = NUROWS
          DO 60 I = 1, NUSPEC
              K = K + 1
              GRZFOO = GRZFOO + X(K) * ZOOPR(I,IFILSP) * 1.D-3
   60     CONTINUE

!****
!*  6 Calculate grazing (1/d) rate and optionally maximum grazing rate
!****
          IF (GRZFOO .GT. 0.0) THEN
              IF (GRZFOO .LT. ((EXP(GTMPRM(IFILSP) *
     &        (T - 20.0)) * GRZRM(IFILSP)) /
     &        (EXP(GTMPFM(IFILSP) * (T - 20.0)) *
     &        GRZFM(IFILSP) *
     &        (GRZFOO / (GRZFOO + GRZMO(IFILSP)))))) THEN
                  GRZGRZ = ((GRZOLD(IFILSP) *
     &            EXP(GTMPFM(IFILSP) * (T - 20.0)) *
     &            GRZFM(IFILSP)) *
     &            (GRZFOO / (GRZFOO + GRZMO(IFILSP))))
              ELSE
                  GRZGRZ = ((GRZOLD(IFILSP) *
     &            EXP(GTMPRM(IFILSP) * (T - 20.0)) *
     &            GRZRM(IFILSP)) / GRZFOO)
              ENDIF
          ELSE
              GRZGRZ = 0.0
              LZERO = 2
          ENDIF
!         IF (GRZGRZ.GT.GRAMX) THEN
!           GRZGRZ=GRAMX
!           LZERO = 1
!         ENDIF

!****
!*  7 Calculate realized feeding (mg C/mg C.d) and filtration rate (l/mg C.d)
!*    GRZRAT and GRZFIL are output variables, not used at the moment
!****
          IF (GRZOLD(IFILSP) .GT. 0.0) THEN
              GRZRAT = GRZFOO * GRZGRZ / GRZOLD(IFILSP)
              GRZFIL = GRZGRZ / GRZOLD(IFILSP)
          ELSE
              GRZRAT = 0.0
              GRZFIL = 0.0
          ENDIF

!****
!*  8 Calculate flux and biomass for detritus and phytoplankton groups
!****
          DO 70 I = 1, NTONUT
              DFLUX(I) = -(CDETR(I) * GRZGRZ * GDETPR(IFILSP))
   70     CONTINUE
          K = NUROWS
          DO 80 I = 1, NUSPEC
              K = K + 1
              ALGFLX(I) = -(X(K) * GRZGRZ * ZOOPR(I,IFILSP)) * 1.D-3
              GRAMOR(I) = GRAMOR(I) + GRZGRZ * ZOOPR(I,IFILSP)
   80     CONTINUE

!****
!*  9 Calculate total intake per element (mg/d)
!****
          DO 100 J = 1, NTONUT
              GRZUPT(J) = -(DFLUX(J) * (1.0 - GDETFF(IFILSP)))
              DETFLX(J) = DFLUX(J) - DFLUX(J) *
     &            GDETFF(IFILSP) * GTODET(IFILSP)
              BOTFLX(J) = BOTFLX(J) - DFLUX(J) *
     &            GDETFF(IFILSP) * (1.0-GTODET(IFILSP))
              DO 90 I = 1, NUSPEC
                  GRZUPT(J) = GRZUPT(J) - ALGFLX(I) * ALGSTC(J,I) *
     1            (1.0 - GFECFR(I,IFILSP))
                  DETFLX(J) = DETFLX(J) - ALGFLX(I) * ALGSTC(J,I) *
     1                GFECFR(I,IFILSP) * GTODET(IFILSP)
                  BOTFLX(J) = BOTFLX(J) - ALGFLX(I) * ALGSTC(J,I) *
     1                GFECFR(I,IFILSP) * (1.0 - GTODET(IFILSP))
   90         CONTINUE
  100     CONTINUE

!****
!* 10 Calculate limiting element for growth
!****
          GRZPMX = 10.0E20
          DO 110 I = 1, NTONUT
              IF (GRZST(I,IFILSP) .GT. 0.0) THEN
                  GRZPRT = GRZUPT(I) /
     &            GRZST(I,IFILSP)
                  IF (GRZPRT .LT. GRZPMX) THEN
                      GRZPMX = GRZPRT
                  ENDIF
              ENDIF
  110     CONTINUE

!****
!* 11 Calculate routine respiration (mgC/l.d)
!****
          DO 120 I = 1, NTONUT
              DISFLX(I) = (GRZPMX * EXP(GTMPRE(IFILSP) *
     &        (T - 20.0)) * GRZRE(IFILSP) *
     &        GRZST(I,IFILSP))
              GRZUPT(I) = GRZUPT(I) - DISFLX(I)
  120     CONTINUE
          GRZPMX = GRZPMX * (1.0 - EXP(GTMPRE(IFILSP) *
     &    (T - 20.0)) * GRZRE(IFILSP))

!****
!* 12 Calculate the standard respiration (1/d)
!****
          GRZMET = EXP(GTMPSE(IFILSP) * (T - 20.0)) *
     &             GRZSE(IFILSP)

!****
!* 13 Correct for length of period (d)
!****
          DO 130 I = 1, NUSPEC
              ALGFLX(I) = ALGFLX(I) * PERIOD
  130     CONTINUE
          DO 140 I = 1, NTONUT
              GRZUPT(I) = GRZUPT(I) * PERIOD
              DETFLX(I) = DETFLX(I) * PERIOD
              BOTFLX(I) = BOTFLX(I) * PERIOD
              DISFLX(I) = DISFLX(I) * PERIOD
  140     CONTINUE
          GRZPMX = GRZPMX * PERIOD
          GRZMET = GRZMET * PERIOD

!****
!* 14 Calculate bruto growth (mg C/period)
!****
          GRZBRU = GRZNEW(IFILSP) - GRZOLD(IFILSP) * (1.0 - GRZMET)

!****
!* 15 Correct bruto growth if intake can not sustain respiration and growth
!****
          IF ((GRZBRU .GT. GRZPMX).AND.(LCOUPL.NE.0)) THEN
              GRZBRU = GRZPMX
              GRZNEW(IFILSP) = GRZOLD(IFILSP) * (1.0 - GRZMET) + GRZPMX
          ENDIF

!****
!* 16 Add respiration to dissolved nutrients
!****
          DO 150 I = 1, NTONUT
              DISFLX(I) = DISFLX(I) + GRZOLD(IFILSP) * GRZMET *
     &        GRZST(I,IFILSP)

!****
!* 17 If there is bruto growth, subtract nutrients from the intake
!****
              IF (GRZBRU .GE. 0.0) THEN
                  GRZUPT(I) = GRZUPT(I) - GRZBRU *
     &            GRZST(I,IFILSP)
!****
!* 18 If their is mortality, add the nutrients to the detritus pool
!****
              ELSE
                  DETFLX(I) = DETFLX(I) - GRZBRU *
     &                GRZST(I,IFILSP) * GTODET(IFILSP)
                  BOTFLX(I) = BOTFLX(I) - GRZBRU *
     &                GRZST(I,IFILSP) * (1.0 - GTODET(IFILSP))
              ENDIF

!****
!* 19 Convert the intake not used for bruto growth to the detritus pool
!****
              DETFLX(I) = DETFLX(I) + GRZUPT(I) * GTODET(IFILSP)
              BOTFLX(I) = BOTFLX(I) + GRZUPT(I) * (1.0 - GTODET(IFILSP))
  150     CONTINUE

!****
!* 20 Check massbalans
!****
          DO 170 J=1,NTONUT
              ALTFLX(J) = 0.0E0
              DO 160 I=1,NUSPEC
                  ALTFLX(J) = ALTFLX(J) + ALGFLX(I) * ALGSTC(J,I)
  160         CONTINUE
              GRZFLX(J) = (GRZNEW(IFILSP) - GRZOLD(IFILSP)) *
     &        GRZST(J,IFILSP)
              TOTFLX(J) = DISFLX(J) + DETFLX(J) + BOTFLX(J) +
     &        ALTFLX(J) + GRZFLX(J)
!             IF ( DEBUG ) WRITE ( IDBG), 9030) J, TOTFLX(J)
!
  170     CONTINUE

!****
!* 21 Update total fluxes detritus
!****
          DO 290 I=1,NUNUCO
            IF (CDETR(1).GT.1.D-12) THEN
!             GRADET will be used for calculation of the change in
!             detritus concentration according to
!             DETR = DETR * EXP(-GRADET*DT)
!             uit de flux  DD=(DETFLX + BOTFLX) kan GRADET afgeleid worden
!             GRADET = - LOG(1+DD/DETR) / DT
              IF ((DETFLX(I+1)+BOTFLX(I+1))/CDETR(I+1).LE.-1.D0) THEN
                WRITE(*,*) 'CONSBL: Grazing of detritus becomes ',
     1                     'unstable!'
                WRITE(*,*) '        This is caused by an unrealistic ',
     1                     'high grazing pressure.'
                WRITE(*,*) '        Please adapt the grazing pressure ',
     1                     'in the input.'
                STOP 1
              ENDIF
              GRADET(I) = GRADET(I) - DLOG(1.0D0 + (DETFLX(I+1)+
     1                    BOTFLX(I+1))/(CDETR(I+1))) / PERIOD
!           ELSEIF (ABS(DETFLX(1)+BOTFLX(1)).GT.1.D-12) THEN
!             WRITE(*,*) 'CONSBL: No detritus to add feacal fraction ',
!    1                   'grazers to!'
!             WRITE(*,*) '        Nutients will become available ',
!    1                   'immediately as dissolved.'
            ENDIF
  290     CONTINUE
!         WRITE(*,*) 'CDETR  = ',CDETR(1)
!         WRITE(*,*) 'DFLUX  = ',DFLUX(1)
!         WRITE(*,*) 'DETFLX = ',DETFLX(1)
!         WRITE(*,*) 'BOTFLX = ',BOTFLX(1)
!         WRITE(*,*) 'ALGFLX = ',ALGFLX(1)

!****
!* 22 End loop over filter feeders
!****
  200 CONTINUE

!****
!* 23 Save biomass grazers for next time step
!****
      TOTGRA = 0.0
      DO 250 J=1,NUGRAZ
          TOTGRA=TOTGRA+GRAMOR(J)
  250 CONTINUE

      IF ((ITNUM.GT.1).AND.(ITNUM.LT.IPERM).AND.(LGRAZ.NE.2)) THEN
        DO 300 I=1,NUNUCO
          IF (ABS(GRADET(I)-GDETO(I)).GT.1.0D-06) LGRAZ=1
  300   CONTINUE
        IF (LGRAZ.EQ.0) THEN
          DO 310 J=1,NUSPEC
            IF (ABS(GRAMOR(J)-GMORO(J)).GT.1.0D-06) LGRAZ=1
  310     CONTINUE
        ENDIF
      ENDIF

      IF ((ITNUM.LT.IPERM).AND.(LGRAZ.EQ.1)) THEN
        DO 320 I=1,NUNUCO
          GDETO(I)=GRADET(I)
  320   CONTINUE
        DO 330 J=1,NUSPEC
          GMORO(J)=GRAMOR(J)
  330   CONTINUE
      ENDIF

      IF (LZERO.EQ.2) THEN
        LZERO = 0
        LGRAZ = 0
      ENDIF


!     WRITE(244,'(I2,5(1PE12.4))') ITNUM,GRZOLD(1),GRZNEW(1),GRZFOO,
!    1                             TOTGRA,GRAMX

      RETURN

 9000 FORMAT ( ' CONSBL: ' / ( ' Biomass grazer ', I2, ' old ', E12.5,
     1         '   wanted ', E12.5))
 9010 FORMAT ( '   FLUX nut ',I2, ' dis, det, bot   ', 3E12.5)
 9020 FORMAT ( '   FLUX alg ',I2, '    ', E12.5)
 9030 FORMAT ( '   TOTFLX   ',I2, '    ', E12.5)
 9040 FORMAT ( '   TOT alg  ',I2, '    ', E12.5)

      END
