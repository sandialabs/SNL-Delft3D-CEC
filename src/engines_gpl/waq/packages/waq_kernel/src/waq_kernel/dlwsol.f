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

      subroutine dlwsol(lundia    ,noq1      ,noq2      ,noq3      ,
     *                  r1        ,kcs       ,nosys     , notot   )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : August 1996 by E. de Goede
!
!     FUNCTION            : Print concentrations at end of simulation
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NOQ1    INTEGER     1       INPUT   nr of grid points in y-direction
!       remark: N0Q1 = nmax in TRISULA
!     NOQ2    INTEGER     1       INPUT   nr of grid points in x-direction
!       remark: N0Q2 = mmax in TRISULA
!     NOQ3    INTEGER     1       INPUT   nr of grid points in z-direction
!       remark: N0Q3 = kmax in TRISULA
!     NOSYS   INTEGER     1       INPUT   number of active substances
!     NOTOT   INTEGER     1       INPUT   number of total substances
!
      use timers
      DIMENSION  r1  (noq1, -1:noq2+2, noq3, notot)
      INTEGER    kcs (noq1, -1:noq2+2)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwsol", ithandl )
!
!
      write (lundia,*) '=================================== '
      write (lundia,*) '=================================== '
      write (lundia,*) 'eindwaarden'
      write (lundia,*) noq1,noq2,noq3,nosys,notot
!
!     print transport
!
      if ( nosys  .gt. 0 ) then
         write (lundia,*) '   '
         write (lundia,*) ' Concentrations'
         do 420 l=1,nosys
            write (lundia,*) ' Constituent no. ',l
            do 410 m=1,noq2
            do 410 n=1,noq1
               if (kcs(n,m).ne.0)
     *             write (lundia,'(2i4,100f10.4)')
     *                        n,m,(r1(n,m,k,l),k=1,noq3)
 410        continue
            write (lundia,'(''  0  0'',100f10.4)')
     *                            (r1(1,1,k,l),k=1,noq3)
 420     continue
      endif
!
      write (lundia,*) '=================================== '
      write (lundia,*) '=================================== '
      if ( timon ) call timstop ( ithandl )
      end
