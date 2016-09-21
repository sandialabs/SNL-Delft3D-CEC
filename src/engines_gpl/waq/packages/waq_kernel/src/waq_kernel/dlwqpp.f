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

      SUBROUTINE DLWQPP ( NOTOT  , NOSYS  , NOSEG  , NOPA   , NOSFUN ,
     *                    ITIME  , IMFLAG , IDFLAG , IHFLAG , MONAME ,
     *                    SYNAME , DUNAME , WANAME , IDUMP  , NODUMP ,
     *                    IWASTE , NOWAST , CONC   , CONS   , PARAM  ,
     *                    FUNC   , SEGFUN , VOLUME , WASTE  , BOUND  ,
     *                    NOBND  , ITSTRT , ITSTOP , NX     , NY     ,
     *                    LGRID  , NODISP , NOVELO , NOQ    , NOQ1   ,
     *                    NOQ2   , NOQ3   , DISPER , VELO   , ASMASS ,
     *                    IBFLAG , NOCONS , NOFUN  , CONAME , PANAME ,
     *                    FUNAME , SFNAME , BONAME )
!
!
!     Deltares      SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : june 1988 by L. Postma
!
!     FUNCTION            : Parameter list and header for user supplied
!                           subroutine for POSTPROCESSING.
!
!     LOGICAL UNITS       : none explicitly, the user may use any unit
!                           number if NOT between 10 and 35 !!!!!!!!!!
!
!     SUBROUTINES CALLED  : none explicitly
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
!     ITIME   INTEGER       1     INPUT   Time in system clock units
!     IMFLAG  LOGICAL       1     INPUT   .TRUE. if DELWAQ sets a
!                                                     monitoring step
!     IDFLAG  LOGICAL       1     INPUT   as IMFLAG for dump actions
!     IHFLAG  LOGICAL       1     INPUT   as IMFLAG for history actions
!     MONAME  CHAR*40       4     INPUT   Model and run names
!     SYNAME  CHAR*20    NOTOT    INPUT   names of systems
!     DUNAME  CHAR*20    NODUMP   INPUT   names of dump locations
!     WANAME  CHAR*20    NOWAST   INPUT   names of waste locations
!     IDUMP   INTEGER    NODUMP   INPUT   dump segment numbers
!     NODUMP  INTEGER       1     INPUT   number of dump locations
!     IWASTE  INTEGER    NOWAST   INPUT   waste segment numbers
!     NOWAST  INTEGER       1     INPUT   number of waste locations
!     CONC    REAL   NOTOT,NOSEG  INPUT   Model concentrations
!     CONS    REAL          *     IN/OUT  Model constants
!     PARAM   REAL    NOPA,NOSEG  IN/OUT  Model parameters
!     FUNC    REAL          *     IN/OUT  Model functions at ITIME
!     SEGFUN  REAL   NOSEG,NOSFUN IN/OUT  Segment functions at ITIME
!     VOLUME  REAL      NOSEG     INPUT   Segment volumes
!     WASTE   REAL NOTOT+1,NOWAST INPUT   waste loads
!     BOUND   REAL   NOSYS,NOBND  INPUT   boundary concentrations
!     NOBND   INTEGER     1       INPUT   number of boundary conditions
!     ITSTRT  INTEGER     1       INPUT   model start time in units
!     ITSTOP  INTEGER     1       INPUT   model stop  time in units
!     NX      INTEGER     1       INPUT   width of grid
!     NY      INTEGER     1       INPUT   depth of grid
!     LGRID   INTEGER     NX*NY   INPUT   grid-layout
!     NOQ     INTEGER       1     INPUT   Total number of exchanges
!     NOQ1    INTEGER       1     INPUT   number of exchanges 1st direction
!     NOQ2    INTEGER       1     INPUT   number of exchanges 2nd direction
!     NOQ3    INTEGER       1     INPUT   number of exchanges 3rd direction
!     NOVELO  INTEGER       1     INPUT   Number of user-flows
!     NODISP  INTEGER       1     INPUT   Number of user-dispersions
!     DISPER  REAL   NODISP*NOQ   OUTPUT  User defined dispersion
!     VELO    REAL   NOVELO*NOQ   OUTPUT  User defined flows
!     ASMASS  REAL  NOTOT*NOSEG*? IN/OUT  mass balance per comp. elems.
!                                         if IBFLAG = 1
!     IBFLAG  INTEGER    1        INPUT   if 1 then mass balance p.c.e.
!     NOCONS  INTEGER       1     INPUT   Number of constants used
!     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
!     CONAME  CHAR*20   NOCONS    INPUT   Constant names
!     PANAME  CHAR*20   NOPA      INPUT   Parameter names
!     FUNAME  CHAR*20   NOFUN     INPUT   Function names
!     SFNAME  CHAR*20   NOSFUN    INPUT   Segment function names
!     BONAME  CHAR*20   NOBND     INPUT   Boundary names
!
!     ==================================================================
!
!
!
      CHARACTER*40 MONAME(4)
      CHARACTER*20 SYNAME (NOTOT), DUNAME (NODUMP), WANAME (NOWAST),
     &             CONAME (*)    , PANAME (*)     , FUNAME (*)     ,
     &             SFNAME (*)    , BONAME (*)
      DIMENSION    IDUMP (NODUMP), IWASTE (NOWAST)
      DIMENSION    VOLUME (NOSEG), CONS        (*),FUNC        (*),
     &             CONC   (NOTOT,NOSEG)           ,PARAM   (NOPA,NOSEG),
     &             WASTE (0:NOTOT,NOWAST)         ,SEGFUN(NOSEG,NOSFUN),
     &             BOUND ( NOSYS, NOBND )           ,LGRID (NX    ,NY ),
     &             DISPER( NODISP, NOQ  )           ,VELO  (NOVELO,NOQ),
     &             ASMASS( NOTOT,NOSEG,*)
      LOGICAL IMFLAG, IDFLAG, IHFLAG
!
!
      RETURN
      END
