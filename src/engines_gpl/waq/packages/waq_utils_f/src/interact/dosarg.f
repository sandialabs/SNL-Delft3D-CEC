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

! Integer function to parse the argument string of the last dos
! command. This function returns
! 1. The name of the dos program (CMD).
! 2. The argument list (ARGSTR).
! 3. The length of the argument list (LENS).
! 4. Each individual argument (ARGS).
! 5. The length of each argument (LENARG).
! 6. The number of actual arguments (INDEX).
!
      INTEGER FUNCTION DOSARG (MAXLA, MAXLS, CMD, ARGSTR, LENS, ARGS,
     *                         LENARG, INDEX)
      INTEGER LENARG(*), MAXLA, MAXLS, INDEX, LENSTR, STOS, WIPE
      integer dhcarg
      CHARACTER*1 ARGS(*)
      CHARACTER*(*) ARGSTR, CMD
      CHARACTER*80 ARGV, STRTMP, STRTM2
!
! Initiate DOSARG at 0. Get value of INDEX from function IARGC. Write
! the 0-argument to CMD.
!
      DOSARG = 0
cjvb
!     INDEX = IARGC() - 1
!     CALL DHCARG(INDEX)
      index = dhcarg()
      INDEX = INDEX - 1
!     WRITE(STRTMP,10) ARGV(0)
      CALL DHGARG(0,STRTMP)
cjvb
  10  FORMAT(A80)
      LENS = LENSTR (STRTMP,80)
      IRC = STOS (STRTMP, 1, LENS, CMD, LENCMD)
      IRC = WIPE (STRTMP, 1, LENS)
!
! Loop through the argument list. Copy each individual argument to ARGS.
! Store its length in LENARG. Append STRTMP, which hold a temporary copy
! of ARGSTRING.
!
      LENS = 1
      K = 0
cjvb  DO 30 I=1,IARGC()-1
      DO 30 I=1,INDEX
         K = K + 1
cjvb     WRITE(STRTM2,20) ARGV(I)
         CALL DHGARG(I,STRTM2)
  20     FORMAT(A80)
         LENI = LENSTR(STRTM2,80)
         IF (LENI .GT. MAXLA) GO TO 200
         LENARG(K) = LENI
         IRC = STOS (STRTM2, 1, LENI, ARGS((K-1)*MAXLA+1), LENK)
         STRTMP (LENS:LENS+LENI) = STRTM2 (1:LENI)
         LENS = LENS + LENI + 1
  30  CONTINUE
!
! Exit. Set lens (=length argstring) and copy STRTMP to ARGSTR.
!
      LENS = LENS - 2
      IF (LENS .GT. MAXLS) GO TO 300
      IRC = STOS (STRTMP, 1, LENS, ARGSTR, LENTOT)
      GO TO 1000
!
! Argument to long.
!
200   CONTINUE
      DOSARG = 1
      GO TO 1000
!
! Argstring to long.
!
300   CONTINUE
      DOSARG = 2
      GO TO 1000
1000  CONTINUE
      RETURN
      END
