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

      SUBROUTINE DLWQ12 ( IOUT   , IDUMP  , CONC   , ITIME  , IDT    ,
     *                    IHSTRT , IHSTOP , IHSTEP , DNAME  , SNAME  ,
     *                    MNAME  , NODUMP , NOTOT  , IHFLAG )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : june, 1988 by L.Postma
!
!     FUNCTION            : Writes history results to IOUT in
!                                          for postprocessing.
!
!     LOGICAL UNITNUMBERS : IOUT = number of history output file
!
!     SUBROUTINES CALLED  : none
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     IOUT    INTEGER     1       INPUT   unit number output file
!     IDUMP   INTEGER  NODUMP     INPUT   segment numbers for dump
!     CONC    REAL     NOTOT*?    INPUT   concentration values
!     ITIME   INTEGER     1       INPUT   present time in clock units
!     IDT     INTEGER     1       INPUT   time step of simulation
!     IHSTRT  INTEGER     1       INPUT   start time of history
!     IHSTOP  INTEGER     1       INPUT   stop time of history
!     IHSTEP  INTEGER     1       INPUT   time step of history
!     DNAME   CHAR*20   NODUMP    INPUT   names of monitoring stations
!     SNAME   CHAR*20   NOTOT     INPUT   names of substances
!     MNAME   CHAR*40     4       INPUT   model identification
!     NODUMP  INTEGER     1       INPUT   amount of dump segments
!     NOTOT   INTEGER     1       INPUT   total number of systems
!     IHFLAG  LOGICAL     1       OUTPUT  TRUE if history took place
!
      use timers
      DIMENSION    IDUMP(*) , CONC(NOTOT,*)
      CHARACTER*20 DNAME(*) , SNAME(*)
      CHARACTER*40 MNAME(*)
      LOGICAL      IHFLAG
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq12", ithandl )

      IHFLAG = .FALSE.
      IF ( IHSTEP                   .LE. 0      ) goto 9999
      IF ( ITIME                    .LT. IHSTRT ) goto 9999
      IF ( ITIME-IDT                .GE. IHSTOP ) goto 9999
      IF ( MOD(ITIME-IHSTRT,IHSTEP) .GE. IDT    ) goto 9999
!
!         initialise the history file
!
      IF ( ITIME-IHSTRT .LT. IDT ) THEN
           WRITE ( IOUT ) ( MNAME(K) , K = 1,4 )
           WRITE ( IOUT )   NOTOT    , NODUMP
           WRITE ( IOUT ) ( SNAME(K) , K = 1,NOTOT )
           WRITE ( IOUT ) ( IDUMP(K),DNAME(K), K = 1,NODUMP )
      ENDIF
      IHFLAG = .TRUE.
!
!         dump a history
!
      WRITE (IOUT) ITIME , ((CONC(K,IDUMP(J)),K=1,NOTOT),J=1,NODUMP)
!
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
      END
