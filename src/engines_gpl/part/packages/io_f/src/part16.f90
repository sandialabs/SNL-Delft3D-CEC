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

module part16_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use precision_part              ! single and double precision
      use timers
use fileinfo               ! file information for all input/output files
!
!  module procedure(s)
!
implicit none              ! force explicit typing
!
contains
      subroutine part16(lun2  , lgrid , conc  , mnmaxk, npart ,    &
                        mpart , wpart , nopart, itime , iptime,    &
                        npwndw, atotal, itstrt, iddtim, itstop,    &
                        nodye , nocont, idelt , kpart , npwndn,    &
                        nosubs, nolay , mnmax2, isfile, nosubc,    &
                        modtyp                        )
!
!                   Deltares (former: Deltares)
!
!                        d e l p a r    v3.12
!
!
!     system administration : r.j. vos
!
!
!     created               : april 1990, by m. zeeuw
!
!
!     modified              : may 1990, by a. markus
!
!
!                           : april 1991, by a. markus
!
!                           : july 1992 by r.j. vos, for 8 substances
!                             with identical transport but different
!                             decay
!
!                           : july 1996 by r.j. vos, for 3d version
!                             and conc-array like delwaq standard
!
!     function              : generates a delwaq - loads file
!
!
!     note                  : not tested yet for user-defined releases
!
!
!     logical unit numbers  : lun1 - unit nr delwaq - map-file
!                             lun2 - output log file
!
!
!     subroutines called    : srstop
!
!
!     functions   called    : none.
!
!
!     parameters            :
!
!     name    kind     length     funct.  description
!     ====    ====     ======     ======  ===========
!     atotal  real   nolay*nosubs output  total mass per subst/per layer
!     conc    real  nosubs*mnmaxk output  concentration work-array
!     iddtim  integer     1       input   delwaq-delay-time
!     idelt   integer     1       input   time step of calculation
!     iptime  integer     1       input   age of particles
!     isfile  integer  nosubs     input   if (0) then a modelled substance,
!                                         else external
!     itime   integer     1       input   model time
!     itstop  integer     1       output  simulation stop
!     itstrt  integer     1       in/out  simulation start
!     kpart   integer   nopart    input   layer number of the particles
!     lgrid   integer  nmax*mmax  input   grid numbering active gridcells
!     mmax    integer     1       input   nr. grid cells in secnd dir.
!     mnmaxk  integer     1       input   dimension of volume
!     mpart   integer   nopart    output  m-values particles
!     modtyp  integer     1       input   modeltype
!     nmax    integer     1       input   nr. grid cells in first dir.
!     nocont  integer     1       in/out  max nr of cont. release points
!     nodye  integer     1       in/out  max nr of dye   release points
!     nolay   integer     1       input   actual number of layers
!     nopart  integer     1       input   number of active particles
!     nosubc  integer     1       input   leading dimension conc.array
!     nosubs  integer     1       input   actual number of substances
!     npart   integer   nopart    output  n-values particles
!     npwndn  integer     1       output  new start of active nopart number - 1
!     npwndw  integer     1       input   start of active nopart number
!     wpart   real  nosubs*nopart input   weight of the particles
!     ----    ----     ------     ------  -----------
!     i       integer     1       local   index pointer
!     ic      integer     1       local   igrid cell value
!     ihelp   integer     1       local   help variable
!     ilay    integer     1       local   layer pointer
!     ipos    integer     1       local   help pointer
!     isub    integer     1       local   substance pointer
!     j       integer     1       local   index pointer
!     lname   char.*40    1       local   name of particle work file
!     lun2    integer     1       local   unit number 2
!     nosubt  integer     1       local   nolay * nosubs
!     rhelp   real        1       local   help variable
!     un10pn  logical     1       local   .true. if unit 10 open
!
!     save values between invocations
!
      save
!
!     parameters
!
      integer(ip), parameter  :: lun1   =    31
!
!     dimensioning
!
      integer(ip), dimension(:)        :: iptime
      integer(ip), dimension(:)        :: isfile
      integer(ip), dimension(:)        :: npart , mpart , kpart
      integer(ip), dimension(:,:)      :: lgrid
!
      real   (sp), dimension(:)        :: atotal
      real   (sp), dimension(:,:)      :: conc
      real   (sp), dimension(:,:)      :: wpart
!
!     character(len=20) ::  ftype
!     common /wnmnef4/ ftype(2)
!
!     data
!
      character (len=256) :: lname  = 'partderv.wrk'
      logical             :: un1opn = .false.
!
!     local scalars
!
      integer(ip) :: i , ic , iddtim , idelt  , ihelp  , ilay   , ipos   , iseg
      integer(ip) :: isub   , itime  , itstop , itstrt , j      , lun2   , mnmax2
      integer(ip) :: mnmaxk , mod    , modtyp , nocont , nodye  , nolay
      integer(ip) :: nopart , nosubc , nosubs , nosubt , npwndn , npwndw
      real   (sp) :: rhelp
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "part16", ithndl )
!
!     set sliding window pointer anyway; part16 checks always for this variable,
!     so it must be set to zero eventhough delwaq overtake may not be activated
!
      npwndn = 0
!
      nosubt = nolay * nosubs
!
      if (iddtim  >  0) then
!
!       delwaq overtake
!
        if (itime - itstrt  >=  iddtim) then
!
!         actual time greater than delwaq-delay-time
!
!         determine if this is the first time
!
          if ( .not. un1opn) then
            un1opn = .true.
            call openfl ( lun1, lname, ftype(2), 1 )
            if (nodye  >  0 .and. nocont  < 1) then
!
!             run for dye(s)
!
              ihelp = 1
            elseif (nodye  < 1 .and. nocont  >  0) then
!
!             run for continuous load(s)
!
              ihelp = 0
            elseif(nodye > 0.and.nocont > 0) then
!
!             run for dye(s) and continuous load(s)
!
              ihelp = 3
            else
!
!             run with solely user-defined releases
!
              ihelp = 4
            endif
            write (lun1) ihelp, itstrt, iddtim, itstop, mnmaxk, idelt
          endif
!
!         zero the work array for all the substances
!
          do 10 i = 1, mnmaxk
             do 20 isub = 1, nosubc
                if(isfile(isub)==0) then
                   conc(isub,iseg) = 0.0
                endif
 20          continue
 10       continue
!
!         add particles of certain age
!         sum masses and make concentrations
!
          atotal = 0.0
!
          do 60, i = npwndw, nopart
            if (iptime(i)  >=  iddtim) then
              ic = lgrid(npart(i), mpart(i))
              if (ic  >  0) then
!
!               remainder for last removed particles
!
                npwndn = i
!
                ilay = kpart(i)
!
!               check on programming error
!
                if (ilay   >  nolay) then
                  write (lun2, 99002)
                  call srstop(1)
                endif
!
                do 50, isub = 1, nosubs
                   if(isfile(isub)==0) then
                      ipos             = ilay + (isub - 1) * nolay
                      iseg             = (ilay-1)*mnmax2 + ic
                      rhelp            = wpart (isub, i)
                      atotal(ipos)     = atotal(ipos)      + rhelp
                      if(modtyp /= 2) then
                         conc  (isub,iseg)= conc  (isub,iseg) + rhelp
                      else
                         conc  (ipos,ic  )= conc  (ipos,ic  ) + rhelp
                      endif
                   endif
   50           continue
              endif
            endif
   60     continue
!
!         produce a map record
!
          if (npwndn  >  0 .or. nocont  >  0) then
            write (lun1) itime,      &
                         ((conc(i, j), i = 1, nosubc), j = 1, mnmaxk)
            write (lun2, 99001) itime/86400, mod(itime, 86400)/3600,  &
                                mod(itime, 3600)/60, mod(itime, 60),  &
                                (atotal(ipos),ipos=1,nosubt)
!
          endif
        endif
      endif
!
!     end of subroutine
!
      if ( timon ) call timstop ( ithndl )
      return
!
!     formats
!
99001 format(/'  Total removed masses for DELWAQ at time :',  &
                 i3,'d-',i2.2,'h-',i2.2,'m-',i2.2,'s = ',/,   &
                 8(e13.6,2x),/,8(e13.6,2x) )
99002 format(/' Error: programming error PART16: number of substances!')
!
      end subroutine
end module

