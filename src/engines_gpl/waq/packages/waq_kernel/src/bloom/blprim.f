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

!    23-12-97: Store computed biomass in BIOMAS array for use in D40BLO
!    Version 0.2 22 July 1994
!    Version 0.1 7 Januari 1994
!    Program:    BLPRIM.FOR
!    Programmer: Jos van Gils
!
!    Compute primary production and associated fluxes
!
!    Called by: BLOOMC
!    Calls    : BVECT , DYNRUN

!     22062006 Bugfixes Uptake fluxes, streamlining of NUNUCO based loops

      SUBROUTINE BLPRIM (BIOMAS, CNH4  , CNO3  , CPO4  , CSIO  , 
     J                   CDETN , CDETP ,         CCO2  , CTIC  ,
     J                   FLMORA, FLDETN, TSTEPI, EXTOT , EXALG , TEMP  ,
     J                   RAD   , DEPTH , DAYL  , ID    , LCOUPL, NSET  ,
     J                   DEAT4 , TOTNUT, CHLTOT, FLPRPA, FLUPTN, FACLIM,
     J                   UPTNIT, FRACAM, FBOD5 , RATGRO, RATMOR, ALGDM ,
     J                   ISEG  , CGROUP, LMIXO , LFIXN , LCARB , NUTCON,
     J                   FLXCON, NOUTLIM, OUTLIM, NUNUCOM, NUECOGM, 
     j                   CON2OUT)

      IMPLICIT NONE

!  
!     Arguments
!
!     Name    Type  Length   I/O  Description
!
!     BIOMAS  R*4   NUSPEC   I    Biomass (gC/m3)
!     CNH4    R*4   1        I    Concentration NH4 (gN/m3)
!     CNO3    R*4   1        I    Concentration NO3 (gN/m3)
!     CPO4    R*4   1        I    Concentration PO4 (gP/m3)
!     CSIO    R*4   1        I    Concentration SIO (gSi/m3)
!     CDETN   R*4   1        I    Concentration DetN (gN/m3)
!     CDETP   R*4   1        I    Concentration DetP (gP/m3) 
!     CCO2    R*4   1        I    Concentration CO2 (g/m3)
!     CTIC    R*4   1        I    Concentration TIC (gC/m3)
!     FLMORA  R*4   NUSPEC   I/O  Mortality fluxes (gC/m3/d)
!     FLDETN  R*4   4        I/O  Detritus production (g/m3/d)
!     TSTEPI  R*4   1        I    Time step (d)
!     EXTOT   R*4   1        I    Total extinction (1/m)
!     EXALG   R*4   1        I    Extinction from living algae (1/m)
!     TEMP    R*4   1        I    Temperature (deg.C)
!     RAD     R*4   1        I    Radiation (J/cm2/week)
!     DEPTH   R*4   1        I    Depth (m)
!     DAYL    R*4   1        I    Length of light period (h)
!     ID      I     1        I    Week number (1 to 52)
!     LCOUPL  I     1        I    Coupling flag
!     NSET    I     1        I    Counter
!     DEAT4   R*4   1        I    ??
!     TOTNUT  R*4   4        O    Total C,N,P,Si in algae (g/m3)
!     CHLTOT  R*4   1        O    Total chlorophyl in algae (mgChl/m3)
!     FLPRPA  R*4   NUSPEC   O    Primary production fluxes (gC/m3/d)
!     FLUPTN  R*4   5        O    Uptake fluxes (g/m3/d)
!     FACLIM  R*4   6        O    Limiting factors (-)
!     UPTNIT  R*4   1        O    Nitrogen uptake per day
!     FRACAM  R*4   1        O    Fraction NH4 of N uptake
!     FBOD5   R*4   1        O    BOD5/BODinf in algae
!     RATGRO  R*4   NUECOG   O    Effective growth rate per group (1/day)
!     RATMOR  R*4   NUECOG   O    Effective mortality per group (1/day)
!     ALGDM   R*4   1        O    Dry matter in algae (gDM/m3)
!     ISEG    I     1        I    Segment number
!     CGROUP  r*4   NUECOG   O    Group biomass
!     LMIXO   L     1        I    FLAG mixotrophy
!     LFIXN   L     1        I    FLAG N fixation
!     LCARB   L     1        I    FLAG C limitation
!     NOUTLIM I     1        I    dimension of OUTLIM
!     OUTLIM  R*4   NOUTLIM  O    limiting factors (extended)
!     NUNUCOM I     1        I    max nr of nutrient constraints in DELWAQ output
!     NUECOGM I     1        I    max nr of algae groups in DELWAQ in/out
!     CON2OUT I     1        I    mapping of actual nutrient constraints to DELWAQ output
!
      REAL            BIOMAS(*), CNH4, CNO3, CPO4, CSIO, RATGRO(*),
     J                CCO2  , CTIC  , CDETN , CDETP,
     J                FLMORA(*), FLDETN(*), TSTEPI, EXTOT, EXALG, TEMP,
     J                RAD, DEPTH, DAYL, DEAT4, TOTNUT(*), CHLTOT, FBOD5,
     J                FLPRPA(*), FLUPTN(*), FACLIM(*), UPTNIT, FRACAM,
     J                RATMOR(*), ALGDM, CGROUP(*)
      INTEGER         LCOUPL, NSET, ID, ISEG, NOUTLIM, NUNUCOM, NUECOGM
      INTEGER         CON2OUT(NUNUCOM), NUTCON(NUNUCOM), FLXCON(NUNUCOM)
      REAL            OUTLIM(NOUTLIM)
      LOGICAL         LMIXO , LFIXN , LCARB
      
!     Common block variables used
!
!     Name    Type  Length   I/O  Inc-file  Description
!
!     NUSPEC  I     1        I    phyt2     Number of types
!     NUECOG  I     1        I    phyt2     Number of groups
!     NUROWS  I     1        I    phyt2
!     AA      R*8   MN,MT    I    phyt1     Stoichiometry matrix (g/gDW)
!     CTODRY  R*8   MT       I    size      Conversion (gDW/gC)
!     NREP    I     1        I/O  phyt2     Counter
!     IT2     I     MS,2     I    phyt2     Administration of groups/types
!     BIOBAS  R*8   1        I    size      Base level per group (gDW/m3)
!     TOPLEV  R*8   1        I    size      Base level per type (gDW/m3)
!     CONCEN  R*8   3        I/O  phyt1     Available nutrients (g/m3)
!     IOU     I     99       I    ioblck    Logical unit numbers
!     ISDPER  I     2        I    sumout    First and last week selective dump
!     XDEF    R*8   MX+1     I    xvect     System vector of Bloom
!     LIMIT   C*18  1        I    sumout    Limiting factors
!     XINIT   R*8   MS       O    xvect     Initial biomass per group
!     TSTEP   R*8   1        O    dynam     Time step
!     RMORT   R*8   MT       I    size      Mortality rate (1/day)
!
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'size.inc'
      INCLUDE 'ioblck.inc'
      INCLUDE 'sumout.inc'
      INCLUDE 'xvect.inc'
      INCLUDE 'dynam.inc'
!
!     Local variables
!
!     Name    Type  Length   I/O  Description
!
!     CMORT   R*4   1             Additional mortality flux (gDW/m3/d)
!     X       R*8   MT            Remaining after mortality (gDW/m3)
!     XJ      R*8   1             Biomass per group (gDW/m3)
!     J       I     1
!     IHULP   I     1
!     IERROR  I     1             Present number of mass errors
!     MERROR  I     1             Maximum number of mass errors
!     EXTOT8  R*8   1             Real*8 version of input parameter
!     EXBAC8  R*8   1             ..
!     TEMP8   R*8   1             ..
!     RAD8    R*8   1             ..
!     DEPTH8  R*8   1             ..
!     DAYL8   R*8   1             ..
!     CHLOR8  R*8   1             ..
!     EXTLIM  R*8   1             ??
!     DEAT    R*8   1             ??
!     TOTCHL  R*8   1             Real*8 version of output parameter
!     TOTDRY  R*8   1             Real*8 version of output parameter
!     TOTCAR  R*8   1             Real*8 version of output parameter
!     UPTAKE  R*4   1             Nitrogen uptake (gN/m3/d)
!     I       I     1
!     FRMIXX  R*4   1             Fraction of mixotrophy in production
!     NUTCON  I*4   8             Nutrients involved in active nutrient constraints
!     FLXCON  I*4   8             Uptake fluxes involved in active nutrient constraints

      REAL*8          EXTOT8, EXBAC8, TEMP8, RAD8, DAYL8, CHLOR8, XJ,
     J                DEPTH8, EXTLIM, DEAT, TOTCHL, TOTDRY, TOTCAR,
     J                X(MT), AUTO(3), FIXINF, AUTNUT
      REAL            CMORT , UPTAKE, FRMIXN, FRMIXP, FRMIX
      INTEGER         IERROR, MERROR, K, J, I, IHULP, I2, INUCO
      PARAMETER      (MERROR = 100,FIXINF=1.0D+03)
      SAVE IERROR
      DATA IERROR /0/

!
! Increase BLOOM II indicator for the number of runs.
      NREP = NREP + 1
!
!  Transfer actual time step to Bloom (through common variable TSTEP)
      TSTEP = DBLE(TSTEPI)

!
!  Compute totals per species group (XINIT), limit to BIOBAS
      DO J=1,NUECOG
        XJ = 0D0
        DO I = IT2(J,1), IT2(J,2)
          XJ = XJ + DBLE(BIOMAS(I)) * CTODRY(I)
        ENDDO
        XINIT(J) = MAX(XJ,BIOBAS)
      ENDDO
!
!  Compute available nutrients (CONCEN) start with nutrients outside algae pool (dissolved , detritus)
!
      CONCEN (1) = DBLE(CNO3 + CNH4)
      CONCEN (2) = DBLE(CPO4)
      CONCEN (3) = DBLE(CSIO)
      INUCO = 3
      IF (LCARB) THEN 
        CONCEN (INUCO+1) = DBLE(CTIC)          ! DECIDE IF THIS SHOULD BE TIC
        INUCO = INUCO + 1
      ENDIF
!     MIXOTROPHY
      IF (LMIXO) THEN
        CONCEN(INUCO+1) = DBLE(CDETN)
        CONCEN(INUCO+2) = DBLE(CDETP)
        INUCO = INUCO + 2
      ENDIF
!     N-FIXATION
      IF (LFIXN) THEN
        CONCEN(INUCO+1) = FIXINF
        INUCO = INUCO + 1
      ENDIF
      AUTO = 0.D0
!
! Check whether biomass minus mortality is reasonably large.
! If the value has dropped below TOPLEV, then set the biomass to
! zero and correct the detritus fluxes.
! Otherwise add nutrients in live phytoplankton to CONCEN.
!
! Loop over algae species

      DO J=1,NUSPEC

! Compute remaining biomass after mortality

         IF (BIOMAS (J) .LT. 0.0) THEN
            X (J) = BIOMAS (J)
         ELSE
            X(J) = DBLE( BIOMAS(J) - TSTEPI * FLMORA(J) ) * CTODRY(J)
!
!  Add nutrients in autolyses to CONCEN and AUTO
!
            DO K=1,NUNUCO
               I = NUTCON(K)
               AUTNUT  = DBLE( TSTEPI * FLMORA(J)) * CTODRY(J) *
     1                   (1.D0 - AVAILN) * AA(K,J)
               IF (I.LE.3) THEN
                 AUTO(I) = AUTO(I) + AUTNUT                   ! ONLY FOR N,P,Si
                 CONCEN(I) = CONCEN(I) + AUTNUT
               ENDIF
            ENDDO
!
! Negative biomass after mortality? Message!
!
            IF (X(J) .LT. -1.0D-2) THEN
               IERROR = IERROR + 1
               WRITE (IOU(61), 1050) IERROR,J,BIOMAS(J), ISEG, X(J)
               IF (IERROR .EQ. MERROR) GOTO 901
            END IF
 1050    FORMAT ( ' Integration error number ',I3,/,
     &            ' Current biomass of type ',I3,' = ',E15.5,/,
     &            ' in segment',I8,/,
     &            ' Remaining after mortality = ',E15.5,/,
     &            ' Serious error: time step is too big to model',
     &            ' mortality')

            IF (X(J) .LT. TOPLEV) THEN
!
!  Set small biomasses to zero, putting mass into the detritus pools,
!  by increasing the mortality and detritus production fluxes
!
               CMORT = SNGL(X(J))/TSTEPI
               FLMORA(J) = FLMORA(J) + CMORT/SNGL(CTODRY(J))
               FLDETN(1) = FLDETN(1) + CMORT/SNGL(CTODRY(J))
               DO K=1,NUNUCO
                   I = NUTCON(K)
                   IF (I.LE.3)                         ! ONLY N,P,Si
     J             FLDETN(I+1) = FLDETN(I+1) + CMORT*SNGL(AA(K,J))
               ENDDO
!  Biomass set to zero, for BLOOM to make proper mortality constraint
!  and for correct calculation of production fluxes afterwards
!  (JvG, June 2006)

               X(J) = 0.0D0
            ENDIF
!  End of code for positive biomass
         ENDIF
!
!  Add nutrients in live phytoplankton to CONCEN.
!  Contrary to earlier versions ALL cases pass this
!  code, because X(J) is used afterwards to calculate 
!  production flux, the same value needs to be given to 
!  BLOOM as available nutrients in CONCEN
!  (JvG, June 2006)
!
         DO K=1,NUNUCO
             I = NUTCON(K)
             CONCEN(I) = CONCEN(I) + AA(K,J) * X(J)
         ENDDO

!  End of loop over algae species
      ENDDO
!
! Call BVECT to set the mortality constraints.
!
      CALL BVECT (X, XDEF)
!
!  Call DYNRUN. This routine calls the actual BLOOM II module
!
      EXTOT8 = DBLE(EXTOT)
      EXBAC8 = DBLE(EXTOT-EXALG)
      TEMP8  = DBLE(TEMP)
      RAD8   = DBLE(RAD)
      DEPTH8 = DBLE(DEPTH)
      DAYL8  = DBLE(DAYL)
      CHLOR8 = -1D0
      EXTLIM = 0D0
      DEAT   = DBLE(DEAT4)

!      write (1961,*   ) 'BLOOM   ',NREP
!      write (1961,1001) 'XINIT   ',(XINIT(J), J=1,NUECOG)
!      write (1961,1001) 'CONCEN  ',(CONCEN(J), J=1,NUNUCO)
!      write (1961,1001) 'X       ',(X(J), J=1,NUSPEC)

      CALL DYNRUN (EXTOT8, EXBAC8, TEMP8 , RAD8  , DEPTH8, DAYL8 ,
     J             CHLOR8, ID    , ISEG  , LCOUPL, NSET  , EXTLIM,
     J             DEAT  , TOTCHL, TOTDRY, TOTCAR)

!      write (1961,1001) 'XDEFA   ',(XDEF(J), J=1,NUNUCO)
!      write (1961,1001) 'XDEFB   ',(XDEF(NUROWS+J), J=1,NUSPEC)

!
! Store total carbon and chlorophyll concentration
!
      TOTNUT(1) = SNGL(TOTCAR)
      CHLTOT    = SNGL(TOTCHL)
      ALGDM     = SNGL(TOTDRY)

      DO I=2,4
         TOTNUT(I) = 0.0
      ENDDO
      DO I=1,9
         FLUPTN(I) = 0.0
      ENDDO
      FBOD5     = 0.0

! Compute gross production: computed biomass by Bloom is in XDEF(NUROWS+J)
! Loop over algae species, compute production per species and total pr.pr.
! Added: computation of total nutrients
! Added: Calculate uptake fluxes (JvG, June 2006)

      DO J = 1, NUSPEC
         FLPRPA(J) = SNGL( (XDEF(J+NUROWS)-X(J)) / CTODRY(J) ) / TSTEPI
         IF (.NOT.LCARB) FLUPTN(1) = FLUPTN(1) + FLPRPA(J)
         FBOD5 = FBOD5 + SNGL( XDEF(J+NUROWS)/CTODRY(J) )
     J             * ( 1. - EXP(-5.*RMORT(J)) )
         DO K = 1,NUNUCO
             I = NUTCON(K)
             I2 = FLXCON(K)
             IF (I.LE.3)
     J       TOTNUT(I+1) = TOTNUT(I+1) + SNGL( XDEF(J+NUROWS)*AA(K,J) )
             FLUPTN(I2)  = FLUPTN(I2) + FLPRPA(J)*AA(K,J)*CTODRY(J)
         ENDDO

! Determine carbon uptake mixotrophy

         IF (LMIXO) THEN
!           FRACTION MIXOTROPHY IN PRODUCTION
           IF (LCARB) THEN
            FRMIXN = AA(5,J) / (AA(1,J) + AA(5,J))
            FRMIXP = AA(6,J) / (AA(2,J) + AA(6,J))
           ELSE
            FRMIXN = AA(4,J) / (AA(1,J) + AA(4,J))
            FRMIXP = AA(5,J) / (AA(2,J) + AA(5,J))
           ENDIF
           FRMIX  = MAX(FRMIXN,FRMIXP)
           FLUPTN(9) = FLUPTN(9) + FRMIX*FLPRPA(J)
!          Uptake of DetC should be subtracted from total C-uptake 
           FLUPTN(1) = FLUPTN(1) - FRMIX*FLPRPA(J)
         ENDIF
      ENDDO   

      IF (TOTNUT(1) .GT. 1E-30) THEN
          FBOD5 = FBOD5/TOTNUT(1)
      ELSE
          FBOD5 = 1.0
      ENDIF

!  Correct for depletion of NH4 assume that
!  phytoplankton first depletes ammonia (completely)

      IF (XDEF(1) .LE. CNO3) THEN
         UPTAKE = FLUPTN(2)
         FLUPTN(2) = (CNH4 + AUTO(1)) / TSTEPI
         FLUPTN(3) = UPTAKE - FLUPTN(2)
      ENDIF
      UPTNIT = FLUPTN(2) + FLUPTN(3)
      IF (UPTNIT .GT. 1E-30) THEN
          FRACAM = FLUPTN(2)/UPTNIT
      ELSE
          FRACAM = 1.0
      ENDIF

! Find limiting factors, this algorithm just takes first 3 (inorganic N,P,Si) and last 3 (e,gro,mor)
! regardless of additional nutrient constraints. This cnnects to traditional 6 limiting factors output
! For extended output (extra nutrient contstraints, grow/mort per group) see below

      I = 0
      DO J = 1,NUNUCO + 3
          READ ( LIMIT((2*J-1):(2*J)),'(I2)') IHULP
          IF ((J.LE.3).OR.(J.GT.NUNUCO)) THEN
            I = I + 1
            FACLIM(I) = REAL(IHULP)
          ENDIF
      ENDDO
!      write (1963,*) 'stap ',nrep
!      write (1963,*) 'faclim ',(faclim(i),i=1,6)
      
!     Extended output for limiting factors

      CALL BL_ISPLIM(NOUTLIM,OUTLIM,NUNUCOM,NUECOGM,CON2OUT)
!
! Loop to compute effective growth and mortality rates
!
      DO  J=1,NUECOG
          RATGRO(J) = 0.0
          RATMOR(J) = 0.0
          CGROUP(J) = 0.0
          DO I = IT2(J,1), IT2(J,2)
              RATGRO(J) = RATGRO(J)
     J              + SNGL( XDEF(I+NUROWS)-X(I) )
              RATMOR(J) = RATMOR(J)
     J              + SNGL(CTODRY(I))*BIOMAS(I) - SNGL(X(I))
              BIOMAS(I) = SNGL(XDEF(I+NUROWS)/CTODRY(I))
              CGROUP(J) = CGROUP(J) + BIOMAS(I)
          ENDDO
          RATGRO(J) = RATGRO(J) / SNGL(XINIT(J)) / TSTEPI
          RATMOR(J) = RATMOR(J) / SNGL(XINIT(J)) / TSTEPI
      ENDDO

!      write (1961,1001) 'FLMORA  ',(FLMORA(J), J=1,NUSPEC)
!      write (1961,1001) 'FLDETN  ',(FLDETN(J), J=1,4)
!      write (1961,1001) 'FLUPTN  ',(FLUPTN(J), J=1,9)
!
! Exit
!
      RETURN

  901 WRITE (*,*) 'Fatal ERROR in Bloom module: time step too big'
      CALL SRSTOP(1)
 1001 FORMAT (A,1X,99E15.5)
 1002 FORMAT (A,1X,i5,1x,99E15.5)

      END
      SUBROUTINE BL_ISPLIM(NOUTLIM,OUTLIM,NUNUCOM,NUECOGM,CON2OUT)
      INTEGER NOUTLIM,NUNUCOM,NUECOGM
      INTEGER CON2OUT(NUNUCOM)
      REAL OUTLIM(NOUTLIM)
      INCLUDE 'blmdim.inc'
      INCLUDE 'matri.inc'
      INCLUDE 'phyt2.inc'
      INTEGER II,ICON,ICONOUT
!
!     ISPLIM  holds a list of actually limiting constraint numbers.
!             The constraint nrs are dependent of actual NUNUCO (nr of nutrient constraints)
!             and NUECOG (nr of algae groups). A common example for NUNUCO = 3 and NUECOG = 6:
!       Legend for limiting factors: 
!       Number   Constraint name
!         1      NITROGEN        
!         2      PHOSPHOR        
!         3      SILICON         
!         4      KMIN         (light limitation)      
!         5      KMAX         (photo-inhibition)
!         6                   (to be neglected)
!         7      Growth-DIATOMS  
!               ......
!        12      Growth-OSCILAT  
!        13      Mortal-DIATOMS  
!               ......
!        18      Mortal-OSCILAT  
!             This needs to be converted to the DELWAQ fixed dimension output structure:
!        1 to NUNUCOM for nutrient limitations
!        NUNUCOM+1 and +2 for light
!       NUNUCOM + 3 to NUNUCOM + 2 + NUECOGM for Growth limitation
!       NUNUCOM + 2 + NUECOGM + 1 to NUNUCOM + 2 + 2*NUECOGM for mortality limitation
!
!      WRITE (*,*) 'in'
!      write (1963,*) 'isplim ',(isplim(i),i=1,12)
      OUTLIM = 0.0
!                  write (1963,*) 'NUECOG ',NUECOG
!                  write (1963,*) 'NUNUCO ',NUNUCO
      DO II = 1,MT   ! MT is dimension of ISPLIM according to blmdim.inc
          ICON = ISPLIM(II)
          IF (ICON.GT.0) THEN
              IF (ICON.LE.NUNUCO) THEN
                  ICONOUT = CON2OUT(ICON) ! this array determines the mapping of the actual BLOOM constraints to the fixed array of DELWAQ, filled in BLINPU
              ELSEIF (ICON.LE.NUNUCO+2) THEN
                  ICONOUT = NUNUCOM+(ICON-NUNUCO)
              ELSEIF (ICON.LE.NUNUCO+3) THEN
!                 this is the constraint NUNUCO + 3 that we can neglect
              ELSEIF (ICON.LE.NUNUCO+3+NUECOG) THEN
                  ICONOUT = NUNUCOM+2+ICON-(NUNUCO+3) 
              ELSE
                  ICONOUT = NUNUCOM+2+NUECOGM+ICON-(NUNUCO+3+NUECOG) 
              ENDIF
              OUTLIM(ICONOUT) = 1.0
          ENDIF
!          write (1963,*) 'loop ',ii,icon,iconout
      ENDDO
!      WRITE (*,*) 'UIT'
!      write (1963,*) 'outlim ',(outlim(i),i=1,10)
!      write (1963,*) 'grow  ',(outlim(i),i=11,13)
!      write (1963,*) 'mort  ',(outlim(i),i=41,43)

      RETURN
      END

