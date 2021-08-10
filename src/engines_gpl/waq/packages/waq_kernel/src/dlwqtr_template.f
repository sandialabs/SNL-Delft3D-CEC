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

      subroutine dlwqtr ( notot  , nosys  , noseg  , noq    , noq1   ,
     &                    noq2   , noq3   , nopa   , nosfun , nodisp ,
     &                    novelo , ipoint , volume , area   , flow   ,
     &                    aleng  , conc   , disp   , cons   , param  ,
     &                    func   , segfun , disper , velo   , itime  ,
     &                    idt    , syname , nocons , nofun  , coname ,
     &                    paname , funame , sfname , updatr , ilflag ,
     &                    npartp )

!     Deltares Software Centre

!>\File Before 1995 used to allow users to define their specific (additional) transport processes
!>
!>    To use this routine you may get the index for any constant, parameter or (segment) function
!>    by getting its sequence number e.g. through a call to:\\
!>         call zoek20 ( 'My_name   ', nocons, coname, 10, index )\\
!>    and then use cons(index) (or the parameter, function or segment function value) in your
!>    formula to define a specific additional velo(myvelo,*) or disper(mydisp,*).\\
!>    Look in the description of the Delwaq input file group 4 how to define additional velocity
!>    and dispersion arrays and initialize them with a (dummy) value. They will enter into the
!>    advection diffusion solver as additional velocity (e.g. for settling) or mixing (e.g. for
!>    the effect of additional stirring).\\
!>    This functionality is replaced by the processes library who also adds additional velocities
!>    and dispersions on top of those defined by the user. If you specify 1 additional velocity, but
!>    also switch 'on' processes that create 3 additional velocities, then you might notice that this
!>    routine is invoked with novelo set to 4, 1 defined by you and 3 added by the processes library.

!     Created:    march 1988 by L.Postma

      use timers
      implicit none

!     Parameters          :

!     kind           function         name                    description

      integer  ( 4), intent(in   ) :: notot                 !< Total number of substances
      integer  ( 4), intent(in   ) :: nosys                 !< Number of transported substances
      integer  ( 4), intent(in   ) :: noseg                 !< Number of computational cells
      integer  ( 4), intent(in   ) :: noq                   !< Total number of exchanges
      integer  ( 4), intent(in   ) :: noq1                  !< Number of exchanges direction 1
      integer  ( 4), intent(in   ) :: noq2                  !< Number of exchanges direction 2
      integer  ( 4), intent(in   ) :: noq3                  !< Number of exchanges vertical
      integer  ( 4), intent(in   ) :: nocons                !< Number of constants
      integer  ( 4), intent(in   ) :: nopa                  !< Number of parameters
      integer  ( 4), intent(in   ) :: nofun                 !< Number of functions
      integer  ( 4), intent(in   ) :: nosfun                !< Number of segment functions
      integer  ( 4), intent(in   ) :: nodisp                !< Number of user-dispersions
      integer  ( 4), intent(in   ) :: novelo                !< Number of user-flows
      integer  ( 4), intent(in   ) :: ipoint(4,noq )        !< 'From'-'to' pointer table
      real     ( 4), intent(in   ) :: volume(noseg )        !< Segment volumes
      real     ( 4), intent(in   ) :: area  (noq   )        !< Exchange surfaces
      real     ( 4), intent(in   ) :: flow  (noq   )        !< Exchange surfaces
      real     ( 4), intent(in   ) :: aleng (2,noq )        !< "From" and "To" lengthes
      real     ( 4), intent(in   ) :: conc  (notot ,noseg ) !< Model concentrations
      real     ( 4), intent(in   ) :: disp  (3)             !< Dispersion in 3 directions
      real     ( 4), intent(in   ) :: cons  (nocons)        !< Model constants
      real     ( 4), intent(in   ) :: param (nopa  ,noseg ) !< Model parameters
      real     ( 4), intent(in   ) :: func  (nofun )        !< Model functions at ITIME
      real     ( 4), intent(in   ) :: segfun(noseg ,nosfun) !< Segment functions at ITIME
      real     ( 4), intent(in   ) :: disper(nodisp,noq   ) !< User defined dispersion
      real     ( 4), intent(in   ) :: velo  (novelo,noq   ) !< User defined flows
      integer  ( 4), intent(in   ) :: itime                 !< Time in system clock units
      integer  ( 4), intent(in   ) :: idt                   !< Time step system clock units
      character(20), intent(in   ) :: syname(notot )        !< Names of substances
      character(20), intent(in   ) :: coname(nocons)        !< Names of Constant names
      character(20), intent(in   ) :: paname(nopa  )        !< Names of Parameter names
      character(20), intent(in   ) :: funame(nofun )        !< Names of Function names
      character(20), intent(in   ) :: sfname(nosfun)        !< Names of Segment function names
      logical      , intent(inout) :: updatr                !< Set to .true. if transport is changed
      integer      , intent(in   ) :: ilflag                !< If 0 then only 3 constant lengthes
      integer      , intent(in   ) :: npartp                !< Number of subdomains for parallelism

!     Local declarations

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqtr", ithandl )

!          check usage w.r.t. parallel computing
!          activate this check when your routine is not parallellized.

!     if ( npartp .gt. 1 ) then
!        write(lunrep,1000) npartp
!        call srstop
!     endif

      if ( timon ) call timstop ( ithandl )
      return

!     Output formats

 1000 format (' ERROR: User-supplied transport processes (DLWQTR) may not be used',/,
     &        '        in parallel runs (NPART=',i3,').')
      end
