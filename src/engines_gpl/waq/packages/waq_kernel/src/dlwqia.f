!!  Copyright (C)  Stichting Deltares, 2012-2020.
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

      SUBROUTINE DLWQIA ( LUN    , LUNUT  , A      , J      , MODE   ,
     *                    CONST  , PARAM  , NOSEG  , NOPA   , IISP   ,
     *                    IRSP   , ISFLAG , IFFLAG , ITIME  , LTXT   ,
     *                                                        IERR   )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: april 1996 by L. Postma
!
!     FUNCTION            : Initialises the complete boundary subsystem
!
!     LOGICAL UNITNUMBERS : LUN   - binary boundary system file
!                           LUNUT - monitoring file
!
!     SUBROUTINES CALLED  : none
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     LUN     INTEGER    1        INPUT   unit number input file
!     A       REAL       ?        IN/OUT  Real      boundary workspace
!     J       INTEGER    ?        IN/OUT  Integer   boundary workspace
!     MODE    INTEGER    1        INPUT   File number involved
!     CONST   REAL       *        OUTPUT  Constants read
!     PARAM   REAL       *        OUTPUT  Parameters read
!     NOSEG   INTEGER    1        INPUT   Nr of segments
!     IISP    INTEGER    1        IN/OUT  Integer array space pointer
!     IRSP    INTEGER    1        IN/OUT  Real array space pointer
!     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
!     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
!     ITIME   INTEGER    1        INPUT   system timer
!     LTXT  CHAR*(*)   *        INPUT   array with filenames binary fils
!     IERR    INTEGER    1        IN/OUT  error count
!
!     Declaration of arguments
!
      use timers

      DIMENSION       A(*)   , J(*) , CONST(*) , PARAM(*)
      CHARACTER*(*)   LTXT(*)
      CHARACTER*80    C80
      LOGICAL         LOPEN
!
!     Local declarations
!
      LOGICAL         LDUMMY, LDUMM2
      REAL            RDUMMY(1)
      INTEGER         IDUMMY(1)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqia", ithandl )
!
!         initialise the system
!
      IA = 1
      IJ = 1
      ITEL = 0
      IDUMMY(1) = 1
!
!       Read the system dimensions
!
!     WRITE ( LUNUT , * ) MODE
!
!       Read the pointers for this block of data
!
!     IOPT =  0 means that input is not time dependent
!     NDIM =  0 means a block of constants comes
!     NDIM = -1 means that defaults for parameters come
!     NDIM >  0 means that parameter+segment nr comes from the block
!     IOPT = 1,2,3 or 4 means that input is time dependent
!     IORDER = 1 means that NPNT is the number of items and
!                           NDIM is the number of infor per item
!     IORDER = 2 means the reverse
!     IPRO between 800 and 899 is the unit nr of a binary file
!
   10 READ ( LUN , END=40 , ERR=110 )  J(IJ),
     *                NPNT, ( J(IJ+K              ) , K = 2,NPNT+1 ) ,
     *                NDIM, ( J(IJ+K+MAX(NPNT,0)+2) , K = 1,NDIM   ) ,
     *                IOPT, IPRO
      IORDER = J(IJ)
      J( IJ               + 1 ) = NPNT
      J( IJ + MAX(NPNT,0) + 2 ) = NDIM
      IJS= IJ + 1
      IKS= IJ + MAX(NPNT,0) + 2
      IAS= IA - 1
      IJ = IJ + MAX(NPNT,0) + MAX(NDIM,0) + 5
      J(IJ-2) = IOPT
      J(IJ-1) = IPRO
!        default for all items
      NTOT = NPNT*NDIM
      IF ( NPNT .LE. 0 ) NTOT = NDIM
      IF ( NDIM .LE. 0 ) NTOT = NPNT
      IF ( IPRO .GT. 800 .AND. IPRO .LT. 900 ) THEN
         ILT = IPRO - 800
         IF ( IOPT .EQ. 0 ) THEN
            IF ( NDIM .EQ. 0 ) THEN
!              constants
               CALL DLWQT5( IPRO  , LUNUT    ,ITIME   , A(IA), CONST,
     *                      IDUMMY, 1        ,J(IJS+1), NPNT , 1    ,
     *                      NTOT  , LTXT(ILT),ISFLAG )
            ELSE
!              parameters
               CALL DLWQT5( IPRO    , LUNUT    , ITIME   , A(IA), PARAM,
     *                      J(IJS+1), NPNT     , J(IKS+1), NDIM , NOPA ,
     *                      NTOT    , LTXT(ILT), ISFLAG  )
            ENDIF
         ELSEIF ( IOPT .EQ. 1 ) THEN
            DO I = 1 , NPNT
               J(IJS+I) = -J(IJS+I)
            ENDDO
         ENDIF
!  NOSUB  is first complete dimension of the result
!  IPOINT points into the second dimension of the result
         IJ = IJ + 3
         IA = IA + MAX(1,NDIM)*MAX(1,NPNT)*3
         GOTO 10
      ENDIF
!
!       Nr of breakpoints or harmonics
!
      READ ( LUN , END=100 , ERR=110 ) NOBRK
      J(IJ) = NOBRK
      IJ = IJ + 1
!
!       3 and 4 are harmonics, then an additional real comes in
!
      NTAL = 0
      IF ( IOPT .EQ. 3 .OR. IOPT .EQ. 4 ) NTAL  = 1
!
      DO 20 I=1,NOBRK
      IF ( IOPT .EQ. 0 ) THEN
!                 read the non - time functions
         IF ( NDIM .EQ. 0 ) THEN
!                 read the constants
            READ ( LUN , END=100 , ERR=110 ) J(IJ) ,
     *                                   ( CONST(J(IJS+K)) , K=1,NPNT )
         ENDIF
!                 read the parameters
         IF ( IORDER .EQ. 1 ) THEN
            IF ( NDIM .GT.  0 )
     *         READ ( LUN , END=100 , ERR=110 ) J(IJ) ,
     *                 (( PARAM( (J(IKS+K1)-1)*NOPA + J(IJS+K2) ),
     *                          K1=1,NDIM ) , K2 = 1,NPNT )
            IF ( NDIM .EQ. -1 )
     *         READ ( LUN , END=100 , ERR=110 ) J(IJ) ,
     *                 (  PARAM(                      J(IJS+K2) ),
     *                                        K2 = 1,NPNT )
         ELSE
               READ ( LUN , END=100 , ERR=110 ) J(IJ) ,
     *                 (( PARAM( (J(IJS+K2)-1)*NOPA + J(IKS+K1) ),
     *                          K1=1,NDIM ) , K2 = 1,NPNT )
         ENDIF
      ELSE
!                 read the time functions
         READ ( LUN , END=100 , ERR=110 ) J(IJ) ,
     *                                ( A(IA+K) , K=0,NTOT-1+NTAL )
         IJ = IJ + 1
         IA = IA + NTOT + NTAL
      ENDIF
   20 CONTINUE
      IF ( IOPT .EQ. 0 ) THEN
         IF ( NDIM .EQ. -1 ) THEN
            DO 33 I = 1,NPNT
               ISP = J(IJS+I)
               AAA = PARAM(ISP)
               DO 32 K = 1 , NOSEG
                  PARAM(ISP) = AAA
                  ISP = ISP + NOPA
   32          CONTINUE
   33       CONTINUE
         ENDIF
         IJ = IJS - 1
         IA = IAS + 1
      ENDIF
      ITEL = ITEL + 1
!
!       Return until finished
!
      GOTO 10
!
!       Update linear pointers in array
!
   40 IISP = IISP + IJ-1
      IRSP = IRSP + IA-1
      goto 9999    !  RETURN
!
  100 WRITE ( LUNUT , '(A,I3)' ) ' END-OF-FILE mode:',MODE
      IERR = IERR + 1
      goto 9999    !  RETURN
  110 WRITE ( LUNUT , '(A,I3)' ) ' ERROR-ON-FILE mode:',MODE
      IERR = IERR + 1
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
!
      END
