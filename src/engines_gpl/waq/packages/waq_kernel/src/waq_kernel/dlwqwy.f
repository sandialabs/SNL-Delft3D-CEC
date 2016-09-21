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

      SUBROUTINE DLWQWY ( NOTOT  , NOSYS  , NOSEG  , NOPA   , NOSFUN ,
     *                    VOLUME , CONC   , CONS   , PARAM  , FUNC   ,
     *                    SEGFUN , DERIV  , ITIME  , IDT    , ASMASS ,
     *                    IBFLAG , SYNAME , NOCONS , NOFUN  , CONAME ,
     *                    PANAME , FUNAME , SFNAME , NODUMP , IDUMP  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:    march 1988 by L.Postma
!
!     FUNCTION            : WATERQUALITY subroutine
!
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NOTOT   INTEGER       1     INPUT   Total number of substances
!     NOSYS   INTEGER       1     INPUT   Number of active substances
!     NOSEG   INTEGER       1     INPUT   Nr. of computational elements
!     NOPA    INTEGER       1     INPUT   Number of parameters
!     NOSFUN  INTEGER       1     INPUT   Number of segment functions
!     VOLUME  REAL      NOSEG     INPUT   Segment volumes
!     CONC    REAL   NOTOT*NOSEG  INPUT   Volumes for the active
!                                         substances. Concentrations can
!                                         be obtained by dividing DERIV
!                                         with CONC.
!     CONS    REAL          *     IN/OUT  Model constants
!     PARAM   REAL    NOPA*NOSEG  IN/OUT  Model parameters
!     FUNC    REAL          *     IN/OUT  Model functions at ITIME
!     SEGFUN  REAL   NOSEG*NOSFUN IN/OUT  Segment functions at ITIME
!     DERIV   REAL   NOTOT*NOSEG  IN/OUT  MASS for active substances
!                                         derivitive for inactive substances
!     ITIME   INTEGER       1     INPUT   Time in system clock units
!     IDT     INTEGER       1     INPUT   Time step system clock units
!     ASMASS  REAL  NOTOT*NOSEG*? IN/OUT  mass balance per comp. elems.
!                                         if IBFLAG = 1
!     IBFLAG  INTEGER    1        INPUT   if 1 then mass balance p.c.e.
!     SYNAME  CHAR*20    NOTOT    INPUT   names of systems
!     NOCONS  INTEGER       1     INPUT   Number of constants used
!     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
!     CONAME  CHAR*20   NOCONS    INPUT   Constant names
!     PANAME  CHAR*20   NOPA      INPUT   Parameter names
!     FUNAME  CHAR*20   NOFUN     INPUT   Function names
!     SFNAME  CHAR*20   NOSFUN    INPUT   Segment function names
!     NODUMP  INTEGER       1     INPUT   Number of monitor points
!     IDUMP   INTEGER   NODUMP    INPUT   Segment numbers monitor points
!
!     ==================================================================
!
!          DIMENSION  PARAM (NOPA,NOSEG) , SEGFUN(NOSEG,NOSFUN)
!
!     NOTE that previous dimension statement is only correct if
!          PARAM and NOSFUN are larger than zero, which is not
!          always the case. The user may insert 2-D dimensioning
!          for applications with non-zero PARAM and/or NOSFUN.
!
      CHARACTER*20 SYNAME (NOTOT), CONAME (*), PANAME (*),
     &             FUNAME (*)    , SFNAME (*)
      DIMENSION    VOLUME(NOSEG) , CONC  (NOTOT,NOSEG) , CONS(*) ,
     &             FUNC  (  *  ) , DERIV (NOTOT,NOSEG) ,
     &             PARAM (NOPA,NOSEG) , SEGFUN(NOSEG,NOSFUN),
     &             ASMASS(NOTOT,NOSEG,*), IDUMP(*)
!
!     NOTE:
!     - The concentration is not available as such. It must be determined by tak
!       corresponding substance (in this location now the mass is stored at entr
!       CONC of the substance (in this location the volume of the segment is sto
!     - Never change the CONC and/or the DERIV array at locations of active subs
!       to NOSYS) only change the DERIV of the INactive substances (numbered NOS
!       if needed (and optionally the CONS or PARAM array locations).
!
!
!
!
      RETURN
      END
