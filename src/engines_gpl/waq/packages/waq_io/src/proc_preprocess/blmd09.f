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

      subroutine blmd09 (lunrep, lund09)
!
      use timers       !   performance timers

      implicit none
      integer       lunrep, lund09
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "blmd09", ithndl )
!
! write a default d09 file
      write (lund09,'(a)')'1985   ECOLUMN - BLOOM II model'
      write (lund09,'(a)')' Based on coefficients of ECOLUMN - BLOOM, but modified'
      write (lund09,'(a)')' Diatom-E and Flag-E as in 2 species calculation! C/CHL E-types lowerd'
      write (lund09,'(a)')'TYPES          SPECIES        NUTRIENT    3  ADDITION    1'
      write (lund09,'(a)')'FIRST       1  LAST       52  INCREM      1'
      write (lund09,'(a)')'NITROGEN       TEMPDEP  0.006 NOMINAL        MULTIPLI  1.0  INCREM  0.0'
      write (lund09,'(a)')'PHOSPHOR       TEMPDEP  0.006 NOMINAL        MULTIPLI  1.0  INCREM  0.0'
      write (lund09,'(a)')'SILICON        TEMPDEP  0.003 NOMINAL        MULTIPLI  1.0  INCREM  0.0'
      write (lund09,'(a)')'BACKGROUND EXT                NOMINAL        MULTIPLI  1.0  INCREM  0.0'
      write (lund09,'(a)')'TEMPERATURE                   NOMINAL        MULTIPLI  1.0  INCREM  0.0'
      write (lund09,'(a)')'SOLAR INTENS   TOTALRAD       NOMINAL        MULTIPLI  1.0  INCREM  0.0'
      write (lund09,'(a)')'MIXING DEPTH                  NOMINAL        MULTIPLI  1.0  INCREM  0.0'
      write (lund09,'(a)')'ORGANIC MINERALIZATION  0.005'
      write (lund09,'(a)')'KMIN           KMAX           TEMPMULT.02956 TEMPCON  1.8971'
      write (lund09,'(a)')'MORTALITY      EXPONENT          FLUSH   0.0'
      write (lund09,'(a)')'ZOOPLANKTON    NITROGEN .0833 PHOSPHOR.00675 SILICON   .0'
      write (lund09,'(a)')'ZOOK    2000.  GRAZRATE 1.0   ESCAPE   250.  GRAZINIT  0.0  MAXITER    0'
      write (lund09,'(a)')'AUTOLYSE       SEDIMENT 0.0'
      write (lund09,'(a)')'PARAM'
      write (lund09,'(a)')'PRINT'
      write (lund09,'(a)')'GRO PRINT END'
      write (lund09,'(a)')'STO PRINT END'
      write (lund09,'(a)')'OPTION'
      write (lund09,'(a)')'OBJECTIVE GROWTH'
      write (lund09,'(a)')'TEMP  2.5 0.01'
      write (lund09,'(a)')'GR C 50'
      write (lund09,'(a)')'MO    2.50'
      write (lund09,'(a)')'DOMINANCE'
      write (lund09,'(a)')'RUN'
      write (lund09,'(a)')'STOP'

      if (timon) call timstop( ithndl )
      return
      end
