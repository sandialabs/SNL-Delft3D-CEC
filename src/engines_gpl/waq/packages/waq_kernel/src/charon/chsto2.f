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

      subroutine chsto2 ( lurep , naij2 , procesdef)

      ! Manipulates stoichiometric coefficients of Delwaq 4.0 for use with Charon

      use processet
      implicit none

      ! declaration of arguments

      integer                   :: lurep           ! Unit number report file
      integer                   :: naij2           ! naij2
      type(procespropcoll)      :: procesdef       ! all processes
!
!     Local Declarations
!
      INTEGER      LUIN  , LUOUT , I     , ICOMP , ISPEC , IS    ,
     J                     NSTOC , NSTOCN, NSTOCM, ISTOC , NSKIP,
     J             INDEX
      PARAMETER   (NSTOCM = 100)
      CHARACTER*10 C10
      CHARACTER*12 ACTSUB, ACTFLU, SUBARR(NSTOCM), FLUARR(NSTOCM)
      CHARACTER*80 REGEL
      REAL         ACTSTO, RMSPEC, RMCOMP, STOCHI, STOARR(NSTOCM)
      CHARACTER*10 NH4   , NH4PLU, PO4   , PO4MIN, CARBTO,
     +             CO2
      LOGICAL      L_NH4 , L_PO4 , L_CO2

      integer                   :: nproc           ! number of processes
      integer                   :: iproc           ! loop counter processes
      type(procesprop), pointer :: proc            ! process description
      type(stochiprop)          :: astochiprop     ! one stochi
      type(stochipropcoll)      :: fluxstochi      ! one collection of stochis
      integer                   :: indx            ! index ioitem
      integer                   :: mlevel          ! monitoring level
      integer                   :: iret            ! return value

      INCLUDE 'charon.inc'

      DATA LUIN  /21/
      DATA LUOUT /22/
!
!     get monitoring level
!
      CALL GETMMO(MLEVEL)
!
!     If NH4 is not being modelled and NH4+ is, then set the NH4 fluxes to NH4+
!
      L_NH4  = .FALSE.
      NH4    = 'NH4'
      CALL ZOEK (NH4,NTRANS,VARNAM,10,INDX)
      IF ( INDX .LE. 0 ) THEN
         NH4PLU = 'NH4+'
         CALL ZOEK (NH4PLU,NTRANS,VARNAM,10,INDX)
         IF ( INDX .GT. 0 ) THEN
            L_NH4 = .TRUE.
         ENDIF
      ENDIF
!
!     If PO4 is not being modelled and PO4--- is, then set the PO4 fluxes to PO4---
!
      L_PO4  = .FALSE.
      PO4    = 'PO4'
      CALL ZOEK (PO4,NTRANS,VARNAM,10,INDX)
      IF ( INDX .LE. 0 ) THEN
         PO4MIN = 'PO4---'
         CALL ZOEK (PO4MIN,NTRANS,VARNAM,10,INDX)
         IF ( INDX .GT. 0 ) THEN
            L_PO4 = .TRUE.
         ENDIF
      ENDIF
!
!     If CARBTOT is not being modelled and CO2 is, then set the CARBTOT fluxes to CO2
!
      L_CO2  = .FALSE.
      CARBTO = 'CARBTOT'
      CALL ZOEK (CARBTO,NTRANS,VARNAM,10,INDX)
      IF ( INDX .LE. 0 ) THEN
         CO2 = 'CO2'
         CALL ZOEK (CO2,NTRANS,VARNAM,10,INDX)
         IF ( INDX .GT. 0 ) THEN
            L_CO2 = .TRUE.
         ENDIF
      ENDIF

      ! Loop over the processes

      nproc = procesdef%cursize
      do iproc = 1, nproc
         proc => procesdef%procesprops(iproc)

         if (mlevel.ge.10) write(lurep,*) 'Modifying stochimetrics for:',proc%routine
         nstoc  = proc%no_fluxstochi
         nstocn = 0

         ! loop over stoichiometry lines

         do is = 1, nstoc

            actsub = proc%fluxstochi(is)%substance
            actflu = proc%fluxstochi(is)%ioitem
            actsto = proc%fluxstochi(is)%scale

!           solve problem with double meaning of po4 and nh4 and co2

!           if ( actsub .eq. 'nh4         ' ) actsub = 'nh4+        '
!           if ( actsub .eq. 'po4         ' ) actsub = 'po4---      '
            if ( l_nh4 ) then
               call zoek (nh4,1,actsub,10,indx)
               if ( indx .gt. 0 ) then
                  actsub = nh4plu
               endif
            endif
            if ( l_po4 ) then
               call zoek (po4,1,actsub,10,indx)
               if ( indx .gt. 0 ) then
                  actsub = po4min
               endif
            endif
            if ( l_co2 ) then
               call zoek (carbto,1,actsub,10,indx)
               if ( indx .gt. 0 ) then
                  actsub = co2
               endif
            endif

!           is this a charon species?? is it transported??

            call zoek (actsub,n,kn,6,ispec)
            write ( c10 , '(a6,''    '')' ) actsub
            call zoek (c10,ntrans,varnam,10,index)
!           write(lurep,*) actsub,c10,index,varnam(6)
            if (ispec .gt. 0 .and. index .le. 0 ) then

!              charon-species, which is not transported

               if (mlevel.ge.10)
     +            write (lurep,'(''Remove :'',2a12,f10.3)')
     j         actsub, actflu, actsto

!              find molar mass to make flux in moles

               rmspec = gfw(ispec)

!              loop over non-zero matrix-entries charon

               do 20 i = 1,naij
                  if ( jcol(i) .eq. ispec ) then

!                     this entry concerns the right species!!

                      icomp = irow(i)

!                     find molar mass of component

                      rmcomp = commas(icomp)

!                     find component in transported vector

                      if ( i .le. naij2 ) then
                          write (c10,'(a6,''_dis'')') nr(icomp,1)
                      else
                          write (c10,'(a6,''_par'')') nr(icomp,1)
                      endif
                      call zoek ( c10,ntrans,varnam,10,index)
                      if ( index .le. 0 ) then
                          write (c10,'(a6,''_tot'')') nr(icomp,1)
                          call zoek ( c10,ntrans,varnam,10,index)
                      endif
                      if ( index .le. 0 ) stop 'CHSTOC: 001'

!                     component has been found: index

                      write (actsub,'(a10,''  '')') varnam(index)

!                     compute new stochi coefficient

                      stochi = actsto * aij(i) * rmcomp / rmspec

!                     new stoichiometry line found
!                     check if the affected substance is a new one
!                     if not, increase stoch. coefficient
!                             and skip creating new line

                      do 15 istoc = 1,nstocn
                      if ( subarr(istoc) .eq. actsub .and.
     j                     fluarr(istoc) .eq. actflu ) then
                          stoarr(istoc) = stoarr(istoc) + stochi
                          goto 18
                      endif
   15                 continue

!                     copy to block of new lines

                      nstocn = nstocn + 1
                      if ( nstocn .gt. nstocm ) goto 900
                      subarr(nstocn) = actsub
                      fluarr(nstocn) = actflu
                      stoarr(nstocn) = stochi

   18                 continue


!                 end block for right component

                  endif

!                 end loop non-zero matrix-entries charon

   20          continue

!              end block of not-transported affected charon-species

            else

!              stoichiometry line does not concern charon species
!              copy to block of new lines and give message

               if (mlevel.ge.10)
     +            write (lurep,'(''Accept :'',2a12,f10.3)')
     j         actsub, actflu, actsto
               nstocn = nstocn + 1
               if ( nstocn .gt. nstocm ) goto 900
               subarr(nstocn) = actsub
               fluarr(nstocn) = actflu
               stoarr(nstocn) = actsto
            endif

!           end loop over old stochi lines

         enddo

!        check stochi lines, skip if coefficient is low

         nskip = 0
         do i = 1,nstocn
            if ( abs(stoarr(i)) .lt. 0.005 ) nskip = nskip + 1
         enddo

!        store new block of stochi lines, skip if coefficient is low

         fluxstochi%cursize = 0
         fluxstochi%maxsize = 0
         is = 0
         do i = 1,nstocn
            if ( abs(stoarr(i)) .gt. 0.005 ) then
!
               if (mlevel.ge.10)
     +            write(lurep,'(''Final  :'',2a12,f10.3)')
     j         subarr(i), fluarr(i), stoarr(i)
!
               astochiprop%type      = STOCHITYPE_FLUX
               astochiprop%ioitem    = fluarr(i)
               astochiprop%substance = subarr(i)
               astochiprop%subindx   = 0
               astochiprop%scale     = stoarr(i)
               iret = stochipropcolladd( fluxstochi , astochiprop )
!
            else
               if (mlevel.ge.10)
     +            write(lurep,'(''Neglect:'',2a12,f10.3)')
     j         subarr(i), fluarr(i), stoarr(i)
            endif
         enddo

         if ( proc%no_fluxstochi .gt. 0 ) then
            deallocate ( proc%fluxstochi )
         endif
         proc%no_fluxstochi= fluxstochi%cursize
         proc%fluxstochi   =>fluxstochi%stochiprops

!        end loop processes

      enddo

!     close file

      return
  900 stop 'Dimension error in CHSTO2'
      end
