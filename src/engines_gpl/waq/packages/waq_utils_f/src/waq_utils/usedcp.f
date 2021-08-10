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

      function usedcp()
!-----------------------------------------------------------------------
!             Module:   FUNCTION USEDCP
!           Function: Determines the consumed cpu system time or the
!                     elapsed time in seconds from 0.0 hour (HARDWARE
!                     dependent routine)
!        Method used:
!               Date: 08-06-1995
!         Programmer: P. Koole, Heleen Leepel
!         CVS header
!            $Author: Beek_j $
!              $Date: 7-09-98 10:30 $
!            $Source: /u/trisula/cvsroot/trisula/alg/usedcp.f,v $
!          $Revision: 3 $
!-----------------------------------------------------------------------
!   Calling routines:              SYSINI
!                                  TRICOM
!                                  TRIEND
!                                  TRISOL
!-----------------------------------------------------------------------
!   Called  routines:              CGETCP (c-routine)
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! USEDCP   O  R*4                  Function name for the HARDWARE DEPEN-
!                                  DENT routine. It gets the system time
!                                  in order to derive the CPU'S
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! CPU         R*8                  Cur. time to be used for the compu-
!                                  tation of CPU's
!-----------------------------------------------------------------------
!
! declarations and specifications
!
      usedcp = 0.0
!
      end
