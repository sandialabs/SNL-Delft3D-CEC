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

      SUBROUTINE DHSTOP ( IEXIT )
!
!
!     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED            : June  '91  BY  Jan van Beek
!
!     FUNCTION           : stops execution if possible with return value
!
!*********************************************************************
!     System dependent routine
!     configuration
!
!     UNIX systems
!*********************************************************************
!
!     LOGICAL UNITS      : -
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     IEXIT   INTEGER    1         INPUT   return value
!     ---------------------------------------------------------
!
      INTEGER   IEXIT
!
!     Local
!
!     INTEGER*2 IEXI2
!
!     Close all files
!
      DO 10 ILUN = 10 , 99
         CLOSE ( ILUN )
 10   CONTINUE
!
!     Call C routine
!
      IF ( IEXIT .NE. 0 ) THEN
         WRITE(*,*) ' EXIT LEVEL: ',IEXIT
      ENDIF
      IEXI2 = IEXIT
      CALL STOP ( IEXI2 )
!
      END
