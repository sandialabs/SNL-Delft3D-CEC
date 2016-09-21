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

!    Date:       30 Dec 1999
!    Time:       09:12
!    Program:    PRINSU.FOR
!    Version:    1.32
!    Programmer: Hans Los
!    Previous version(s):
!    1.3 -- 4 Jan 1994 -- 19:45 -- Operating System: DOS
!    1.2 -- 19 Feb 1992 -- 14:04 -- Operating System: DOS
!    1.1 -- 11 Oct 1990 -- 13:10 -- Operating System: DOS
!    1.0 -- 14 Nov 1989 -- 11:24 -- Operating System: DOS
!    0.0 -- 7 Jul 1989 --  9:34 -- Operating System: DOS
!
!  Update 1.32: Changed format at 160 for zooplankton output
!  Update 1.31: No output for bottom algae
!  Update 1.3: Extended registration of limiting factors.
!              Array ISPLIM is filled with ALL limiting factors for
!              each type in the optimum solution. ISPLIM is printed
!              by PRINUN and postprocessed by POSTBL.
!  Update 1.2:
!  Use 1.0D-4 rather than 1.0D-6 to determine limiting factors:
!  due to round-off errors the previous values sometimes suggested
!  that a factor was not limiting when in fact it was.
!
!  *********************************************************************
!  *         SUBROUTINE TO PRINT SUMMARIZED SOLUTIONS                  *
!  *********************************************************************
!
!  0895 MvdV dimension added to ZOOD
!            output of extra items for more grazer types
!
      SUBROUTINE PRINSU(X,XECO,BIO2,TOTAL,ZOOD,COUT,OUT,NTSTOT,
     1                  ITNUM,NTAPE)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'graas.inc'
      INCLUDE 'size.inc'
      INCLUDE 'sumout.inc'
      INCLUDE 'matri.inc'
      INCLUDE 'postbl.inc'
      REAL*8 X(*),OUT(*),XECO(*),ZOOD(0:MG)
      CHARACTER*8 WORDS(14)
      CHARACTER*4 COUT(*)
      LOGICAL LCON
      DATA WORDS  /'Date    ','Limiting','Factors ','Iter    ',
     1             'Zood    ','Total   ','CHL-pred','        ',
     2             'Plank.  ','Diss.   ','CHL-obs ','Tot Ext.',
     3             'Growth  ','Mortalit'/

!     write(*,*) 'Arjen 1: nugraz:',nugraz
!
!  Call subroutines to print headings for output on tape 10,14 and ntape
!  if this is the first time through the subroutine.
!  Set print array indices.
!
      IF (LPRINT .EQ. 2) NPRINT=NPRINT+1
      IF (NPRINT .GT. 1) GO TO 40
      CALL HEADIN (NTAPE,WORDS)
!     0895 MvdV add output for CONSBL grazers
      IF (NUGRAZ.GT.0) THEN
        NTSEX = NUGRAZ -1
      ELSE
        NTSEX = 0
      ENDIF
      NTS14 = NTS14 + NTSEX
      NTSTOT=NTS14+1
!
!  Update 1.3. Construct constraint names and store them in CNAMES.
!  This array is printed by PRINUN.
!
!  Abiotic constraints
!
      DO 10 I = 1, NUABCO
         CNAMES (I) = CSTRA (I)
   10 CONTINUE
!
!  Blank for exclusion row
!
!     write(*,*) 'Arjen 2: nugraz:',nugraz
!     write(*,*) 'Arjen 2: nuexro:',nuexro
!     write(*,*) 'Arjen 2: phyt2:'
!     write(*,'(10i5)')
!    1             IT2,NSPF,NSF,NREP,NUSPEC,NUECOG,NUNUCO,
!    1             NUCOLS,NUFILI,NUABCO,NUEXRO,NUROWS,NUSPE1,IDUMP

      CNAMES (NUEXRO) = WORDS (8)
!     write(*,*) 'Arjen 2b: nugraz:',nugraz
!
!  Growth and mortality constraints: name + group name.
!
      I1 = NUEXRO
      I2 = NUEXRO + NUECOG
      DO 20 I = 1, NUECOG
         I1 = I1 + 1
         I2 = I2 + 1
         WRITE(CNAMES (I1), 30) WORDS (13), GRNAME(I)
         WRITE(CNAMES (I2), 30) WORDS (14), GRNAME(I)
!     write(*,*) 'Arjen 2c: nugraz:',nugraz, i
   20 CONTINUE
   30 FORMAT (A6,'-',A8)
!
!  Start writing the output into print-arrays.
!
   40 CONTINUE
!     write(*,*) 'Arjen 2 - 40: nugraz:',nugraz
!
!  Calculate totals for species, the total chlorophyll concentration
!  and record in OUT.
!
      TOTAL=0.
      DO 60 K=1,NUECOG
         TOT2=0.
         DO 50 J=IT2(K,1),IT2(K,2)
            XBIO = X(J+NUROWS)
            TOT2=TOT2+XBIO
            IF (SDMIX (K) .LT. 0.0) GO TO 50
            TOTAL=TOTAL+XBIO/CHLR(J)
   50    CONTINUE
         XECO(K)=TOT2
   60 OUT(K+NTS7+NTSEX)=TOT2
      OUT(NTS14)=BIO2
      IF (BIO2 .LT. 0.0) OUT(NTS14) = 0.0
      OUT(NTSTOT)=TOTAL
!
!  Determine limiting factors and record their names in COUT.
!  Record in LIMIT in 1,0 notation.
!
!     write(*,*) 'Arjen 2 - 70: nugraz:',nugraz
      WRITE (LIMIT,70) ('0',K=1,NUABCO+1)
   70 FORMAT (9(1X,A1))
      DO 80 K=2,NTS6
         COUT(K) = WORDS(8) (1:4)
   80 CONTINUE
!
!  Update 1.3
!  Initiate ISPLIM at 0
!
!     write(*,*) 'Arjen 3: nugraz:',nugraz
      DO 90 I = 1, NUSPEC
         ISPLIM(I) = 0
   90 CONTINUE
!
!  Update 1.3: modified section.
!  Two changes with respect to previous versions:
!  1.  Register detailed constraint numbers in ISPLIM
!  2.  To this purpose do not leave DO loops for growth and mortality
!      constraints when the first limitation is found: there may be more
!      types growth respectively mortality limited.
!      These duplicate limitations should NOT be written to LIMIT!
!
! 1. nutrient constraints.
!
      K1=1
      NUMLIM = 0
      NCON = 0
      DO 100 K=1,NUNUCO
         NCON = NCON + 1
         IF (X(K) .GT. 1.D-4 ) GO TO 100
         K1=K1+1
         NUMLIM = NUMLIM + 1
         ISPLIM (NUMLIM) = NCON
         COUT(K1) = CSTRA(K) (1:4)
         LIMIT (2*K:2*K) = '1'
  100 CONTINUE
!
! 2. energy constraints.
!
      K2 = 2 + 2 * NUNUCO
      DO 110 K=NUNUCO+1,NUABCO
         NCON = NCON + 1
         IF (X(K) .GT. 1.D-4 ) GO TO 110
         NUMLIM = NUMLIM + 1
         ISPLIM (NUMLIM) = NCON
         K1=K1+1
         COUT(K1) = CSTRA(K) (1:4)
         LIMIT (K2:K2) = '1'
  110 CONTINUE
!
!  Increment NCON by 1 to skip exclusion row!
!
      NCON = NCON + 1
!     write(*,*) 'Arjen 4: nugraz:',nugraz
!     stop
!
! 3. Growth constraints.
!
!  Print slacks for (optional) growth constraints.
!  Note: if both the growth and mortality slack of a phytoplankton
!  are 0.0, assume that the mortality constraint is the actual
!  limitation: do not write "GRO" to output files.
!
      LCON = .FALSE.
      IF (LGROCH .EQ. 0) GO TO 150
      K2 = 2 * (NUABCO -1) + 2
      DO 120 I=1,NUECOG
         NCON = NCON + 1
         IF (X(I+NUEXRO) .GT. 1.D-4) GO TO 120
         IF (X(I+NUEXRO+NUECOG) .LT. 1.D-4 .AND. LMORCH .EQ. 1) GOTO 120
         NUMLIM = NUMLIM + 1
         ISPLIM (NUMLIM) = NCON
         IF ( .NOT. LCON) THEN
            K1=K1+1
            COUT(K1) = WORDS(13) (1:4)
            LIMIT (K2:K2) = '1'
            LCON = .TRUE.
         END IF
  120 CONTINUE
!
! 4. Mortality constraints.
!  Print slacks for (optional) mortality constraints.
!
  130 CONTINUE
!     write(*,*) 'Arjen 4: nugraz:',nugraz
      LCON = .FALSE.
      IF (LMORCH .EQ. 0) GO TO 150
      K2 = K2 + 2
      DO 140 I=1,NUECOG
         NCON = NCON + 1
         IF (X(I+NUEXRO+NUECOG) .GT. 1.D-4) GO TO 140
         IF (XECO(I) .LT. 1.D-4) GO TO 140
         NUMLIM = NUMLIM + 1
         ISPLIM (NUMLIM) = NCON
         IF ( .NOT. LCON) THEN
            K1=K1+1
            COUT(K1) = WORDS(14) (1:4)
            LIMIT (K2:K2) = '1'
            LCON = .TRUE.
         END IF
  140 CONTINUE
  150 CONTINUE
!     0895 MvdV add output for CONSBL grazers
      IF (NUGRAZ.GT.0) THEN
!       write(*,*) 'nts6:',nts6
!       write(*,*) 'nugraz:',nugraz
        DO 200 IG=1,NUGRAZ
          OUT(NTS6+IG)=ZOOD(IG)*1000.*GCTDRY(IG)
  200   CONTINUE
      ELSE
        OUT(NTS7) = ZOOD(0)
      ENDIF
!
! Exit if LPRINT <= 1: nothing more to be done here.
!
      IF (LPRINT .LE. 1) RETURN
!
!  Print solution for iteration ITNUM on tape NTAPE
!  if zooplankton iterations are possible this run.
!
      IF (IPERM .LE. 1) GO TO 170
      WRITE(NTAPE,160) (COUT(K),K=1,NTS6),ITNUM,(OUT(K),K=NTS7,NTSTOT)
  160 FORMAT(2X,A4,2X,6(A3,1X),A3,I2,1X,33(F8.2,1X))
  170 CONTINUE
!
!  Calculate particulate and dissolved concentrations for nutrients,
!  record predicted and observed chlorophyll.
!
      J=0
      DO 180 K=1,NUNUCO
         J=J+1
         PARDIS(J)=CONCEN(K)-X(K)
         J=J+1
         PARDIS(J)=X(K)
  180 CONTINUE
      RETURN
      END
