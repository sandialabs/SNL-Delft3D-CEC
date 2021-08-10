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

      subroutine write_version(lun)

      use part_version_module
      use timers
!
      implicit none    ! force explicit typing
!
!
!                     Deltares (former: Deltares)
!
!                        d e l p a r    v3.60
!
!                          particle program
!
!
!     system administration : r.j. vos (since 1 january 1994, before m. zeeuw)
!
!
!     created               : january 1990, by l. postma
!
!
!     modified              : cleared may , 1996 by r.j. vos
!
!..       17 november 1993: by rj vos (part07.f)
!..       14 april    1994: by rjvos (part10.f,delpar.f,part14.f,partwq.f)
!..       25 may      1994: by rjvos (rdparm.f,delpar.f)
!..       15 july     1994: by rjvos (part07.f, parths.f) => v3.00
!..       4 april     1996: includes psf and his but on pos. 13 and 14 in filename.dat
!..       6 june      1996: the 3d version is implemented (v3.10)
!..       31 july     1996: 3d version, with algae model and conc. like delwaq
!..       9 september 1996: some errors adapted (selfort/partplot)
!..       30 september1996: interpolation option in delpar, some arrays set larger
!..       1  oktober  1996: better partfl in delpar
!..       11 oktober  1996: essential corr. 2-layer model (delpar.f/partfl.f/part18.f)
!..       25 november 1996: adaptions in part10, p10corr and umagi for dunsbergen schemes
!..                         and errors in parths(serious) and rdhydr(not serious).
!..       29 january  1997: version v323 with general use of user def. releases
!..       21 july     1997: version v330 for delft3d; user def. release from delwaq
!..       27 august   1997: version v330 for delft3d; also coordinates file option
!..       1  september1997: version v330 for delft3d; names releases introduced
!..       2  september1997: version v330 for delft3d; names hyd-file delft3d introduced
!..       3  september1997: version v330 for delft3d; corrected mapfile names in rdparm
!..       4  september1997: version v330 for delft3d; added check to rdparm
!..                         and set lnam to 255 char in part12 and part13
!..       11 september1997: version v330 for delft3d; added char*20 subst2 to
!..                         and a komma in a write in rdlgri (req. for nt version)
!..       11 november 1997: version v340 for delft3d; oil module (modtyp=4) included
!..       21 november 1997: version v340 for delft3d; putget_chars included, putget adapted, openfl included
!..       27 november 1997: version v340 for delft3d; oildsp adapted, rdlgri corrected for nt
!..       4  december 1997: version v340 for delft3d; rdccol adapted for nt/rdparm format 2338 added
!..       12 march    1998: version 3.40 for delft3d; rdparm, lnam1 must be * (*) long and not *40 !!
!..       15 april    1998: version 3.40 for delft3d; comment stat. before goto 4 omitted
!..       23 april    1998: version 3.41 for delft3d; putget and putget_chars adapted
!..        4 june     1998: version 3.42 for delft3d; cstop implemented from trisula
!..       10 july     1998: version 3.43 for delft3d; sedimentation-erosion implemented
!                           and option for extra output in mapfile
!                           changes in: delpar, rdparm, part10, part12, part13, parths and version
!                           extra routine for extra output is extout
!         8 september 1998: correction for zero variance in psfzoom
!        17 september 1998: this is the testversion for the new release for
!                           susbstances that can stick to land.
!                           also corrected an error in the 3d-bottom reflection algorithm
!                           sticking substances can no longer evaporate/disperse
!                           sticking substances are expresses in mass/m2
!                           sticking is a phase change betwen two susbstances
!                           and can be caused by horizontal diffusion or settling
!                           changes in: delpar, rdparm, part10, oildsp and version
!                                       part12, part13, parths
!         7 april     1999: test version 3.60; test version for 3d temperature modelling
!                           and constant vertical dispersion option
!                           and 3-dimensional point spread function
!                           and errore messages in rdparm
!                           changes in: delpar, rdparm, part12, partwq, part10,
!                                       pfzoom, version
!         22 april    1999: final version for delft3d-release of 1 june
!                           definitons of sticky subst. altered for ui
!                           changes in: delpar, rdparm, part10, part12, part13, parths,
!                                       pfzoom, version
!          9 june     1999: adaptations in delpar, partwr, part12, part13, psfzoom
!                           and rdparm  from arno noltes acceptance tests
!
!
!     function              : echoes a header to screen
!
!     parameters            : none.
!
      character(len= 32) :: version
      character(len= 22) :: firm
      character(len= 80) :: cident
      character(len= 30) :: verdat
!
!     local scalars
!
      integer(kind=4) ::   icom,   iend,   il,  ir,   iver,   iwl,   lun

      character*120 idstr
      character*3   os
      integer (4)   i, j

      character*75  opkom(7)
      
      data   opkom  / &
        '+-----------------------------------------------------------------------+', &
        '|                        D e l f t 3 D - P A R T                        |', &
        '|                                                                       |', &
        '| D-Particle Tracking     Water quality simulation in 2D/3D models      |', &
        '|                                                                       |', &
        '| Version xx.xxxx  xx-xx-xxxx                                           |', &
        '+-----------------------------------------------------------------------+'/
      
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "write_version", ithndl )
!
!     Get version string from file version_number.h.svn
!     (see also version_number project)
!
      call getfullversionstring_PART(cident)
!
      if (lun==0) then
!        scherm uitvoer
         do i = 1 , size(opkom)
            if ( opkom(i)(3:15) .eq. 'Version xx.xx' ) then
               write(opkom(i)(3:72),'(a)') cident(1:70)
            end if
            write( * , * ) opkom(i)
         enddo
      else
!        print uitvoer
         write(lun,'(//13x,a)')   'PART - Particle tracking'
         write(lun,'(   6x,a)')   ' Water quality simulation in 2D/3D models      '
         write(lun,'(    a//)')   trim(cident)
      end if
!
!     end of routine
!
      if ( timon ) call timstop ( ithndl )
      return
      end subroutine
