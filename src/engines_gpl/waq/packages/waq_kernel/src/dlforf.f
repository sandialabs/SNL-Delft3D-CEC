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

      SUBROUTINE DLFORF(
! trisula parameters:
     *                  lundia    ,
     *                  icx       ,icy       ,jstart    ,nmmaxj    ,
     *                  nmmax     ,kmax      ,
     *                  kcs       ,kfs       ,kfu       ,kfv       ,
     *                  r1        ,rwork     ,value     ,
! delwaq parameters:
     *                  nosys     ,notot     ,volum1               )
!
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : August 1996 by E. de Goede
!
!     FUNCTION            : TRISULA transport discretization + solver
!                           implemented in DELWAQ code
!
!     PARAMETERS          :
!
!     NAME    KIND      FUNCT.  DESCRIPTION
!     ----    -----     ------  -----------
!     ICX     INTEGER   INPUT   Increment in the X-dir., if ICX= NMAX
!                               then computation proceeds in the X-
!                               dir. If icx=1 then computation pro-
!                               ceeds in the Y-dir.
!     ICY     INTEGER   INPUT   Increment in the Y-dir. (see ICX)
!     JSTART  INTEGER   INPUT   start pointer   (jstart=1-2*nmax)
!       remark: in TRISULA j instead of jstart
!     KCS     INTEGER   INPUT   Mask array for the zeta points
!                                      (time independent)
!                                      =0 inactive point
!                                      =1 active   point
!                                      =2 open boundary point
!     KFS     INTEGER   INPUT   Mask array for the zeta points
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
!     KFU     INTEGER   INPUT   Mask array for the u-velocity point
!                                      (time dependent)
!                                      =0 dry      point
!                                      =1 active   point
!     KFV     INTEGER   INPUT   Mask array for the v-velocity point
!                                      (time dependent)
!                                      =0 dry      point
!                                      =1 active   point
!     KMAX    INTEGER   INPUT   nr of layers in third  dimension (=noq3)
!     LUNDIA  INTEGER   INPUT   integer number for monitoring file
!     NMMAX   INTEGER   INPUT   nmax * mmax (= noq2 * noq1)
!     NMMAXJ  INTEGER   INPUT   end pointer   (nmmaxj=(mmax+2)*nmax)
!     NOSYS   INTEGER   INPUT   number of active substances
!     NOTOT   INTEGER   INPUT   number of total substances
!     R1      REAL      IN/OUT  concentration array
!     RMNEG   INTEGER   INPUT   Criterion for conc. above which filter
!                               procedure is applied
!     RWORK   REAL      INPUT   real work array
!     VALUE   REAL      INPUT   real work array
!     VOLUM1  REAL      INPUT   volumes at new time level
!
!

      use timers

      INTEGER   kfu    (jstart:nmmaxj),
     *          kfv    (jstart:nmmaxj),
     *          kfs    (jstart:nmmaxj),
     *          kcs    (jstart:nmmaxj)
!
      DIMENSION r1     (jstart:nmmaxj,kmax, notot),
     *          rwork  (jstart:nmmaxj,kmax),
     *          volum1 (jstart:nmmaxj,kmax),
     *          value  (jstart:nmmaxj),
     *          rmneg  (100)

      save            ifirst, rmneg

      data            maxfil /   10/
      data            ifirst / 1 /
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlforf", ithandl )


      if (ifirst .eq. 1) then
         ifirst = 0

         if (nosys .gt. 100) then
            write (lundia,*)  ' INCREASE DIMENSION OF ARRAY RMNEG'
            write (lundia,*)  ' ASK FOR A NEW VERSION'
            call srstop(1)
         endif
!
!-----------------------------------------------------------------------
!-----define value for 'smoothing', for turbulence model this must be 0.
!     for all other constituents are -1.e-2 this small enough
!-----------------------------------------------------------------------
         do 10 l=1,nosys
           rmneg(l) = -1.0e-2
  10     continue
!
! ******************************************
! ******************************************
! ad hoc oplossing:
!        open (unit=77, file='filter.bnd')
!        read (77,*) nofil
!        do 20 l=1,nofil
!          read (77,*) i,rmneg(i)
! 20     continue
!        close (unit=77)
! ******************************************
! ******************************************
      endif
!
!-----------------------------------------------------------------------
!-----loop over the constituents
!-----------------------------------------------------------------------
!$DIR SCALAR
      do 3000 l=1,nosys
!-----------------------------------------------------------------------
!-------iteration loop over computational grid
!-----------------------------------------------------------------------
!$DIR SCALAR
        do 2000 k=1,kmax
!-----------------------------------------------------------------------
!---------Forester filter initialisation
!-----------------------------------------------------------------------
          do 900 nm=1,nmmax
            value(nm)=0.0
  900     continue
!-----------------------------------------------------------------------
!---------Forester filter number of iterations
!-----------------------------------------------------------------------
!$DIR SCALAR
          do 1200 itfil=1,maxfil
            ifil=0
!
            do 1000 nm=1,nmmax
              rwork(nm,k)=r1(nm,k,l)
              if (rwork(nm ,k).lt.rmneg(l).and.
     *            kcs(nm)*kfs(nm).eq.1) then
                value(nm )=1.0
                ifil      =1
              endif
 1000       continue
!
            if (ifil.eq.0) goto 1299
            nmd=-icx
            ndm=-icy
            nmu= icx
            num= icy
            do 1150 nm=1,nmmax
              nmd=nmd+1
              ndm=ndm+1
              nmu=nmu+1
              num=num+1
              if (kcs(nm)*kfs(nm).eq.1) then
                volnmu = MIN (1.0,volum1(nmu,k)/volum1(nm ,k))
                cofnmu = 0.125*(value(nmu)+value(nm))*kfu(nm)  *
     *                             volnmu
                volnmd = MIN (1.0,volum1(nmd,k)/volum1(nm ,k))
                cofnmd = 0.125*(value(nmd)+value(nm))*kfu(nmd) *
     *                             volnmd
                volnum = MIN (1.0,volum1(num,k)/volum1(nm ,k))
                cofnum = 0.125*(value(num)+value(nm))*kfv(num) *
     *                             volnum
                volndm = MIN (1.0,volum1(ndm,k)/volum1(nm ,k))
                cofndm = 0.125*(value(ndm)+value(nm))*kfv(ndm) *
     *                             volndm
                r1(nm,k,l)=rwork(nm ,k) *
     *                     (1 - cofnmu - cofnmd - cofndm - cofnum) +
     *                     rwork(nmu,k) * cofnmu +
     *                     rwork(nmd,k) * cofnmd +
     *                     rwork(num,k) * cofnum +
     *                     rwork(ndm,k) * cofndm
              endif
 1150       continue
!-----------------------------------------------------------------------
!-----------test if number of iteration steps for filtering is exceeded
!-----------------------------------------------------------------------
            if (itfil.eq.maxfil) then
               write (lundia,*)
     *         'FORESTER FILTER: MAXIMUM NUMBER OF ITERATIONS EXCEEDED'
            endif
 1200     continue
!
 1299     continue
 2000   continue
!       if (itfil.gt.1)
!    *  write (lundia,*) 'FOR CONST. NO. ',l,
!    *                   'THE NUMBER OF FORESTER ITERATIONS=: ',itfil
!-----------------------------------------------------------------------
 3000 continue
6      if ( timon ) call timstop ( ithandl )
      return
      end

