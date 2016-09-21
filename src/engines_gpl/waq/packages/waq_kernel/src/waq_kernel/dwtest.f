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

      SUBROUTINE DWTEST ( AREA   , FLOW   , VELO   , CONC   , BOUND  ,
     *                    IMAT   , NOSYS  , NOTOT  , NOSEG  ,
     *                    NOQ1   , NOQ2   , NOQ3   , NOQ    , NOBND  ,
     *                    IVPNT  , IDPNT  , NOVELO , BPTOR  ,
     *                    DERIV  , IDT    , INTSRT ,
!
     *                    DISP   , DISPER , ALENG  , NODISP ,
     *                    DVOL0  , DVOL1  , ILFLAG , IOPT   ,
! trisula arrays:
     *                    qxk    , qyk    , qzk   ,
     *                    kcs    , kfs    , kfu   , kfv     ,
     *                    gvu    , guv    , gzz   ,
     *                    vol0   , vol1   ,
     *                    difx   , dify   , difz  ,
     *                    r1     ,
     *                    aakl   ,bbkl    ,cckl   ,ddkl     ,
     *                    lundia
     *                 )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : August 1996 by E. de Goede
!
!     FUNCTION            : Conversion of arrays for
!                           routine DLDIFU
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ALENG   REAL      2*NOQ     INPUT   from- and to lengthes
!     AREA    REAL       NOQ      INPUT   exchange surfaces
!     BOUND   REAL   NOSYS,NOBND  INPUT   boundary concentrations
!     BPTOR   REAL     NOBND      INPUT   pointers to original cel numbers
!     CONC    REAL   NOTOT*NOSEG  INPUT   concentrations
!     AAKL    REAL                OUTPUT  contains additional vel. + disp.
!                                         for the vertical direction
!     BBKL    REAL                OUTPUT  contains additional vel. + disp.
!                                         for the vertical direction
!     CCKL    REAL                OUTPUT  contains additional vel. + disp.
!                                         for the vertical direction
!     DDKL    REAL                OUTPUT  contains processes, waste loads
!     DERIV   REAL   NOTOT,NOSEG  INPUT   derivatives
!     DISP    REAL        3       INPUT   dispersion in 3 directions
!     DISPER  REAL   NODISP*NOQ   INPUT   additional dispersion array
!     DVOL0   REAL      NOSEG     INPUT   DELWAQ volumes at old time level
!     DVOL1   REAL      NOSEG     INPUT   DELWAQ volumes at new time level
!     FLOW    REAL      NOQ       INPUT   flows accross exchange surfs
!     GUV     REAL                OUTPUT  Grid distance in the eta-/y-direction
!                                         at v-velocity point
!     GVU     REAL                OUTPUT  Grid distance in the ksi-/x-direction
!                                         at u-velocity point
!     GZZ     REAL                OUTPUT  Grid distance in the z-direction
!                                         at w-velocity point
!     IDPNT   INTEGER   NOSYS     INPUT   pointer systems to dispersions
!     ILFLAG  INTEGER     1       INPUT   if 0 then 3 length values
!     INTSRT  INTEGER     1       INPUT   integration option number
!     IOPT    INTEGER     1       INPUT   = 0 or 2 DISP at zero flow
!                                         = 1 or 3 no DISP at zero flow
!                                         = 0 or 1 DISP over boundary
!                                         = 2 or 3 no DISP over boundary
!     IVPNT   INTEGER   NOSYS     INPUT   pointer systems to velocities
!     IMAT    INT    NOQ1,NOQ2    INPUT   grid layout matrix
!     NOBND   INTEGER     1       INPUT   number of boundary cells
!     NODISP  INTEGER     1       INPUT   number  of additional dispers.
!     NOQ     INTEGER     1       INPUT   nr of segments for all 3 directions
!     NOQ1    INTEGER     1       INPUT   nr of grid points in y-direction
!       remark: N0Q1 = nmax in TRISULA
!     NOQ2    INTEGER     1       INPUT   nr of grid points in x-direction
!       remark: N0Q2 = mmax in TRISULA
!     NOQ3    INTEGER     1       INPUT   nr of grid points in z-direction
!       remark: N0Q3 = kmax in TRISULA
!     NOSEG   INTEGER     1       INPUT   number of segments
!     NOSYS   INTEGER     1       INPUT   number of active substances
!     NOTOT   INTEGER     1       INPUT   number of total substances
!     NOVELO  INTEGER     1       INPUT   number of velocity arrays
!     r1      real                OUTPUT  concentration array
!     VELO    REAL   NOVELO*NOQ   INPUT   additional velocity array
!     VOL0    REAL                OUTPUT  TRISULA volumes at old time level
!     VOL1    REAL                OUTPUT  TRISULA volumes at new time level
!
      use timers
      DIMENSION  FLOW (*) , VELO (*) , CONC (NOTOT,NOSEG) ,
     *           BOUND (NOSYS,NOBND) ,
     *           IVPNT(*) , DERIV(NOTOT,NOSEG) , AREA (*) ,
     *           DVOL0(*) , DVOL1(*) ,
     *           IMAT ( noq1 , noq2 ),
     *           DISP  (  3) , DISPER(*) ,  ALENG (*) , IDPNT(*)
!
      INTEGER    BPTOR (NOBND)
!
      INTEGER    kfu    (noq1, -1:noq2+2),
     *           kfv    (noq1, -1:noq2+2),
     *           kfs    (noq1, -1:noq2+2),
     *           kcs    (noq1, -1:noq2+2)
!
      DIMENSION  qxk    (noq1, -1:noq2+2, noq3),
     *           qyk    (noq1, -1:noq2+2, noq3),
     *           qzk    (noq1, -1:noq2+2, 0:noq3),
     *           difx   (noq1, -1:noq2+2, noq3),
     *           dify   (noq1, -1:noq2+2, noq3),
     *           difz   (noq1, -1:noq2+2, 0:noq3),
     *           r1     (noq1, -1:noq2+2, noq3, notot),
     *           aakl   (noq1, -1:noq2+2, noq3, nosys),
     *           bbkl   (noq1, -1:noq2+2, noq3, nosys),
     *           cckl   (noq1, -1:noq2+2, noq3, nosys),
     *           ddkl   (noq1, -1:noq2+2, noq3, nosys),
     *           vol0   (noq1, -1:noq2+2, noq3),
     *           vol1   (noq1, -1:noq2+2, noq3),
     *           guv    (noq1, -1:noq2+2),
     *           gvu    (noq1, -1:noq2+2),
     *           gzz    (noq1, -1:noq2+2, 0:noq3)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dwtest", ithandl )
!
!
      write(lundia, * ) nosys,notot,noseg,noq1,noq2,noq3
      write(lundia, * ) noq,nobnd,novelo,idt,ilflag,iopt
      write(lundia,'(A)') 'ivpnt'
      write(lundia,'(10i10)') (ivpnt(i),i=1,nosys)
      write(lundia,'(A)') 'imat'
      write(lundia,'(10i10)') ((imat(i,j),i=1,noq1),j=1,noq2)
      write(lundia,'(A)') 'idpnt'
      write(lundia,'(10i10)') (idpnt(i),i=1,nosys)
      write(lundia,'(A)') 'bptor'
      write(lundia,'(10i10)') (bptor(i),i=1,nobnd)


      write(lundia,'(A)') 'flow'
      write(lundia,'(10e13.6)') (flow(i),i=1,noq)
      write(lundia,'(A)') 'velo'
      write(lundia,'(10e13.6)') (velo(i),i=1,noq*novelo)
      write(lundia,'(A)') 'conc'
      write(lundia,'(10e13.6)') ((conc(i,j),i=1,notot),j=1,noseg)
      write(lundia,'(A)') 'bound'
      write(lundia,'(10e13.6)') ((bound(i,j),i=1,nosys),j=1,nobnd)
      write(lundia,'(A)') 'deriv'
      write(lundia,'(10e13.6)') ((deriv(i,j),i=1,notot),j=1,noseg)
      write(lundia,'(A)') 'area'
      write(lundia,'(10e13.6)') (area(i),i=1,noq)
      write(lundia,'(A)') 'dvol0'
      write(lundia,'(10e13.6)') (dvol0(i),i=1,noseg)
      write(lundia,'(A)') 'dvol1'
      write(lundia,'(10e13.6)') (dvol1(i),i=1,noseg)
      write(lundia,'(A)') 'disp'
      write(lundia,'(10e13.6)') (disp(i),i=1,3)
      write(lundia,'(A)') 'disper'
      write(lundia,'(10e13.6)') (disper(i),i=1,noq*nodisp)
      IF ( ILFLAG .EQ. 0 ) THEN
         write(lundia,'(A)') 'aleng'
         write(lundia,'(10e13.6)') (aleng(i),i=1,3)
      ELSE
         write(lundia,'(A)') 'aleng'
         write(lundia,'(10e13.6)') (aleng(i),i=1,2*noq)
      ENDIF
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
