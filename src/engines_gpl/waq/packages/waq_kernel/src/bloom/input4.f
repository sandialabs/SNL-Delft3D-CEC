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

!
!  *********************************************************************
!  *     SUBROUTINE INPUT4 TO READ GRAZER COEFFICIENTS                 *
!  *********************************************************************
!
!  0895 MvdV New subroutine to read the grazing coefficients for the
!            subroutine CONSBL. This subroutine is called from BLOOMPC if
!            NUGRAZ > 0 and reads the input file with the extension
!            .D23.
!
      SUBROUTINE INPUT4 (INPU)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'graas.inc'
!
!  Read grazer cofficients from unit 23:
!     fecal fractions per grazer per algae type
!     preferences per grazer per algae type
!     grazer coeffients per grazer
!

!     Fecal fractions
      DO 10 I=1,NUSPEC
   10 READ(INPU,99999) (GFECFR(I,J),J=1,NUGRAZ)

!     Preferences
      DO 20 I=1,NUSPEC
   20 READ(INPU,99999) (ZOOPR(I,J),J=1,NUGRAZ)

!     Grazing coefficients
!     Feacal fraction for detritus
      READ(INPU,99999,END=40) (GDETFF(J),J=1,NUGRAZ)

!     Preference for detritus
      READ(INPU,99999,END=40) (GDETPR(J),J=1,NUGRAZ)

!     Maximum filtration rate
      READ(INPU,99999,END=40) (GRZFM (J),J=1,NUGRAZ)

!     Maximum growth rate
      READ(INPU,99999,END=40) (GRZGM (J),J=1,NUGRAZ)

!     Maximum mortality rate
      READ(INPU,99999,END=40) (GRZMM (J),J=1,NUGRAZ)

!     Monod half-saturation value for filtration rate
      READ(INPU,99999,END=40) (GRZMO (J),J=1,NUGRAZ)

!     Routine respiration
      READ(INPU,99999,END=40) (GRZRE (J),J=1,NUGRAZ)

!     Maximum uptake
      READ(INPU,99999,END=40) (GRZRM (J),J=1,NUGRAZ)

!     Standard respiration
      READ(INPU,99999,END=40) (GRZSE (J),J=1,NUGRAZ)

!     Stochiometry in nutrient content per gram carbon
      DO 30 I=1,NUNUCO
   30 READ(INPU,99999,END=40) (ZOONUT(I,J),J=1,NUGRAZ)

!     Temp. coef. filtration
      READ(INPU,99999,END=40) (GTMPFM(J),J=1,NUGRAZ)

!     Temp. coef. maximum growth
      READ(INPU,99999,END=40) (GTMPGM(J),J=1,NUGRAZ)

!     Temp. coef. maximum mortality
      READ(INPU,99999,END=40) (GTMPMM(J),J=1,NUGRAZ)

!     Temp. coef. routine respiration
      READ(INPU,99999,END=40) (GTMPRE(J),J=1,NUGRAZ)

!     Temp. coef. maximum uptake
      READ(INPU,99999,END=40) (GTMPRM(J),J=1,NUGRAZ)

!     Temp. coef. standard respiration
      READ(INPU,99999,END=40) (GTMPSE(J),J=1,NUGRAZ)

!     Fraction excretion to the water column
      READ(INPU,99999,END=40) (GTODET(J),J=1,NUGRAZ)

!     Carbon to dry weight ratio
      READ(INPU,99999,END=40) (GCTDRY(J),J=1,NUGRAZ)

      RETURN

   40 WRITE(*,*) 'Hit end of file at unit: ',INPU
      STOP

99999 FORMAT (40F10.0)
      END
