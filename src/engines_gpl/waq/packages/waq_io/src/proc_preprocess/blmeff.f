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

      SUBROUTINE BLMEFF (LUNREP, LUNBLM, LUNFRM, GRNAME, NUECOG)
!
      use timers       !   performance timers

      IMPLICIT NONE
      INTEGER       LUNREP, LUNBLM, LUNFRM, NUECOG
      CHARACTER*10  GRNAME(NUECOG)
!
      INTEGER, PARAMETER :: MAXLIN=255
      INTEGER, PARAMETER :: MAXSPE=30
      INTEGER, PARAMETER :: MAXTOK=8
      INTEGER, PARAMETER :: MAXNZ=51
      INTEGER IFND (MAXSPE)
      REAL*8 FUN(51,MAXSPE), DER(51,MAXSPE), ZVEC(51),
     &       DAYMUL(24,MAXSPE), DL(24)
      CHARACTER*8 TOKEN,                  SPNAM2 (MAXSPE)
      INTEGER GETS, POSIT, MATCH, UPRCAS, STOS, LENSTR, WIPE
      INTEGER NUMTYP, LENTOK, IRC, I, J, LENSPE, NFND, NZ, TEFCUR
      CHARACTER*255 LINE
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "blmeff", ithndl )
!
! read efficiency database
! Read the first record. This contains the names of
! all species for whome information is available.
! Note: this should be consistent with the process coefficient data base
! but this is not checked!
!
   20 FORMAT (A255)
      READ (LUNBLM, 20, END=360) LINE
      POSIT = 1
      NUMTYP = 0
  260 CONTINUE
      IF (GETS (LINE, POSIT, MAXLIN, MAXTOK, TOKEN, LENTOK) .NE. 0)
     &    GO TO 270
      NUMTYP = NUMTYP + 1
      IRC = UPRCAS (TOKEN, SPNAM2(NUMTYP), LENTOK)
      GO TO 260
!
! Match the selected group names (GRNAME) with those stored in the date
! base (SPNAM2). If a match is found, store the matching number in IFND.
!
  270 CONTINUE
      DO 280 I = 1, NUECOG
         LENSPE = LENSTR(GRNAME(I), 8)
         IF (MATCH(SPNAM2,MAXSPE,MAXTOK,GRNAME(I),LENSPE,0,NFND) .GE. 1)
     &      IFND (I) = NFND
  280 CONTINUE
!
! Sort the record pointers to get them in the apprpriate order for the
! output! This is necessary as the user may use a random input order
! for the species names in BLOING.DAT.
!
      CALL INSORT (IFND, NUECOG)
!
!  Read the entire efficiency data base file using the same statements
!  as in INPUT2 of BLOOM II
!
      READ (LUNBLM,290) NZ,TEFCUR
  290 FORMAT (I5,5X,F10.2)
      READ (LUNBLM,300) (ZVEC(I),I=1,NZ)
  300 FORMAT (10(D15.8,3X))
  301 FORMAT (30(D15.8,3X))
      READ (LUNBLM,290) NZ
      DO 310 I=1,NZ
         READ (LUNBLM,301) (FUN(I,J),J=1,NUMTYP)
         READ (LUNBLM,301) (DER(I,J),J=1,NUMTYP)
  310 CONTINUE
      DO 320 I=1,24
         READ (LUNBLM,330) DL(I),(DAYMUL(I,J),J=1,NUMTYP)
  320 CONTINUE
  330 FORMAT (31F5.2)
!
! Write the efficiency data for those species that were selected.
!
      WRITE (LUNFRM,290) NZ,TEFCUR
      WRITE (LUNFRM,300) (ZVEC(I),I=1,NZ)
      WRITE (LUNFRM,290) NZ
      DO 340 I=1,NZ
         WRITE (LUNFRM,301) (FUN(I,IFND(J)),J=1,NUECOG)
         WRITE (LUNFRM,301) (DER(I,IFND(J)),J=1,NUECOG)
  340 CONTINUE
      DO 350 I=1,24
         WRITE (LUNFRM,330) DL(I),(DAYMUL(I,IFND(J)),J=1,NUECOG)
  350 CONTINUE
  360 CONTINUE
      if (timon) call timstop( ithndl )
      RETURN
      END
!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
! INSORT subroutine.                                                   ³
! Purpose: sort an integer array.                                      ³
!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      SUBROUTINE INSORT (INARR, LENARR)
      INTEGER INARR (*), LENARR
      LOGICAL READY
!
10    CONTINUE
      READY = .TRUE.
      DO 20 I = 1, LENARR - 1
         IF (INARR(I) .GT. INARR(I+1)) THEN
            READY = .FALSE.
            IHELP = INARR(I)
            INARR(I) = INARR(I+1)
            INARR(I+1) = IHELP
         END IF
20    CONTINUE
      IF ( .NOT. READY) GO TO 10
      RETURN
      END
