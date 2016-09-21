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

module delpar_mod
!
!  module declarations
!
use precision_part                  ! single/double precision
use timers
use fileinfo  , lun=> lunit    ! logical unit numbers for files
use filtyp_mod                 ! explicit interface
use spec_feat_par
use normal_mod
!
!  module procedure(s)
!
use delete_file_mod            ! explicit interface
use oildsp_mod                 ! explicit interface
use part09_mod                 ! explicit interface
use part10_mod                 ! explicit interface
use grid_search_mod            ! explicit interface
use part12_mod                 ! explicit interface
use part13_mod                 ! explicit interface
use part14_mod                 ! explicit interface
use part18_mod                 ! explicit interface
use part21_mod                 ! explicit interface
use parths_mod                 ! explicit interface
use partur_mod                 ! explicit interface
use partwq_mod                 ! explicit interface
use grid_search_mod            ! explicit interface
use rdhydr_mod                 ! explicit interface
use stop_exit_mod              ! explicit interface
use writrk_mod                 ! explicit interface
use partmem
use alloc_mod
!use normal_mod

#ifdef HAVE_CONFIG_H
#else
use ifcore
#endif
!
implicit none                  ! force explicit typing
!
save
contains
      subroutine delpar(ifnam)

!
!
!                          Deltares (former: Deltares)
!
!                        d e l p a r    v3.60
!
!                          particle program
!
!
!     system administration : r.j. vos (since 1 january 1994, before m. zeeuw)
!
!
!     programmer            : april '90, l. postma ('static version')
!                             dec.  '00, a. koster ('dynamic version')
!
!
!     function              : main steering module for the 3d discrete particle
!                             model.
!
!                             note:
!                             in delpar versions earlier then v3.60.05 this was the
!                             main program. for the implementation of dynamic memory
!                             allocation (based on fmm) this main program was converted
!                             to a subroutine.
!     modifications         : including pc dunsbergen and 3d facilities
!                             v3.40: oil module included (modtyp=4), process routine oildsp called;
!                                    oil has two fractions: one at the surface and one dispersed
!                                    psfzoom is only used for plotting 2d and for floating oil
!                                    add openfl for opening standard formats
!                                    changed putget, added putget_chars
!                             v3.43: sedimentation and erosion at the bed is allowed
!                                    option to create extra output
!                                    corrected an error in psfzoom on 8-9
!                             v3.50: extra possibility for particle to stop travelling
!                                    particle goes into a persistent phase, i.e. stays
!                                    at land. this is not a particle at the bed (bottom)!
!                                    sticking oil can not evaporate or disperse (when floating)
!                                    sticking oil s expressed in mass per m2 (part12, part13)
!                                    adapt calls to part10 and oildsp, and part12,part13 and parths
!                             v3.60: partwq can also be used for 3d temmperature modelling
!                                    modtyp = 2: 2d-1-layer or 2-layer temperature model (2d hydrodynamics)
!                                    modtyp = 5: 3d-temperature model (3d hydrodynamics)
!                                                some updates of rdparm are made
!                                    also updated the psf-function to 3d with simple 3d approach
!                                    (by generalisation of the 2d formulae)
!                                    allows also for constant vertical dispersion
!                             v3.60: license file implemented (see module licens)
!                             v3.60: errors from acceptance test
!                                    delpar: nolay => noslay in
!                                        calls to partwr,oildsp
!                                    rdparm: adapted formats in report
!                                    partwr: delete 3 useless lines
!                                    part12, part13, pfzoom: not correct for jsub=2,or settling dispersed oil
!                                    pfzoom: xmach mus be 0.0 for land-mask
!
!                                    4/8/2000: oildsp has been modified for higher numerical accuracy of oil
!                                              dispersion (rj vos)
!                           v3.60.05:transformed to subroutine for dynamic version of delpar
!
!     note                  : two layer model must be temp. model !!!
!
!     logical unit numbers  : 97     - user input file
!                             lun1   - stripped user input file
!                             lun2   - output log file
!
!
!     subroutines called    : write_version - echo header to screen
!                             dlpr12 - write the history files
!                             dlwqtd - does time inteprolation (like delwaq)
!                             oildsp - oil dispersion (modtyp=4)
!                             part01 - calculate distances and angles in grid
!                             part06 - calculate dump-sites in the grids
!                             part08 - calculate total released mass and
!                                      mass/particle
!                             part09 - add dye release
!                             part10 - calculate actual displacement
!                             part12 - output map-file for concentration
!                             part13 - make plot grids corrected for recovery
!                                      rate
!                             part14 - add continuous release
!                             part15 - adapt wind and direction for actual time
!                             part16 - output dump-file towards delwaq overtake
!                                      (output map-file for eliminating
!                                      particles of certain age)
!                             part17 - calculate actual decaycoefficient
!                             parths - make history plots
!                             partur - adds user-defined releases from file to system
!                             partvs - make settling velocities
!                             partwq - two-layer or 3d temperature model (modtyp=2 or 5)
!                             pfzoom - make plots for zoom window using psf's
!                             rdccol - read curved grid
!                             rdfnam - read in/out files
!                             rdhydr - determine ihdel, starttime, check if
!                                      hydr exists etc.
!                                    - read hydrodynamic water-flow
!                                    - interpolate/rewind if needed
!                             rdlgri - read grids
!                             rdparm - read user steering file
!                             stop_exit - stop routine
!
!
!     subroutines influenced by stratification:
!                             part09 - vertical position particles
!                             part10 - two layers: horizontal displacement
!                             part12 - two substances/layers in map file
!                             part13 - two substances in plot file
!                             part14 - vertical position particles
!                             part16 - two substances in deriv file
!                             part18 - added
!                             rdparm - two names, vertical position
!                                      waste loads
!
!
!     parameters            :
!
!     name    kind     length                           description
!     ====    ====     ======                           ======================================
!     abuoy   real     nopart                           dispersion parameter buoyancy
!     accrjv  real        1                             accuracy limit taylor exp. tracks
!     acomp   logical     1                             use an analytical function for umagi
!     acf     real     ncmax                            disp.coeff. buoyancy per cont.load
!     aconc   real     nosubs*nwmax .                   mass per particle/per load/per subst dye+cont rel
!     aconud  real     nosubs*numax                     mass per part/per load/per subst/ud releases
!     adepth  real     noslx                            depth for peak conc per subst
!     alpha   real        1                             scale factor algebraic vertical diffusivity model
!     amap    real     noslx*nmap *mmap                 plot grid to be dumped
!     amapsett real    irfac*irfac*amap                 plot grid for c-depend. settling
!     amassc  real     nosubs*nocont*ictmax             masses continuous at breakpnts/per substance
!     amassd  real     ndmax*nosubs                     masses of dye releases/ per subst
!     angle   real     mmax*nmax                        angle of the grid cells
!     apeak   real     noslx                            peak conc per substance
!     area    real     mmax*nmax                        surface area of grid cells
!     atotal  real     noslx                            total mass per substance
!     atrack  real     nolayers*nmap*mmap               tracks to be dumped
!     chezy   real     1                                chezy coefficient
!     chismp  real     noslx*nostmx                     time history for total grid
!     chispl  real     noslx*nostmx                     time history for plot grid
!     conc    real     nosubs*mnmax                     concentration in two layers
!     const   real     noconsp                          user-defined constants
!                                                       for oil: const(nocons) = deflection angle
!     decay   real     nosubs*idmax                     decay value at the breaks
!     decays  real     nosubs                           actual decay; linearly interpolated between decay-times
!     defang  real        1                             deflection angle for 3d oil simulations
!     depth   real     mmax*nmax                        total depth of the cells from top to bottom
!     dfact   real     nosubs                           actual decay-factor
!     drand   real     norand                           random step parameters
!     dx      real     mmax*nmax                        dx of the cells
!     dy      real     mmax*nmax                        dy of the cells
!     finud   char*256 numax                            filenames of ud delwaq files
!     flow    real     2*mnmax+nolayers-1)*nmax*mmax    current flows in 3 directions
!     flres   real     nosubs*nmax                      closure error concentration
!     fname    char*256 nfiles                           array of file names
!     fstick  real     nosubs                           fraction of mass that sticks to land
!     ftime   real     icmax*nocont                     time-series for cont. loads
!     ictime  integer  icmax*nocont                     array of breakpoint times
!     ictmax  integer     1                             adapted nr of breakpoints
!     icwsta  integer     1                             start time map-file
!     icwste  integer     1                             time step map-file
!     icwsto  integer     1                             stop  time map-file
!     iddtim  integer     1                             delwaq time delay
!     idelt   integer     1                             main do-loop time -step variable
!     idtime  integer  idmax                            breakpoint times, decays
!     idtmax  integer     1                             adapted maximum nr of breaks decays
!     idummy  integer     1                             dummy variable
!     ifnam   char*(*)    1                             name of master file (file with file names, previously "filename.dat")
!     iftime  integer  numax                            ud rel. reading times from files
!     ifopt   integer  numax                            file option ud release
!     ihdel   integer     1                             time step in hydrdyn. database
!     ihplot  integer  nostmx                           if 1 then history from plot window
!     ihstep  integer     1                             time-step on history file
!     ihstop  integer     1                             stop time on history file
!     ihstrt  integer     1                             start time on history file
!     imask   integer     1                             mask for psf function
!     imonth  integer     1                             offset to real timings; month
!     iofset  integer     1                             offset to real timings
!     ipnt    integer  mnmax                            help pointer
!     ipoil   integer     1                             oil-indicator mass/m^2 or mass/m^3
!     ipc     integer     1                             option for predictor corrector
!     ipset   integer  ipmax                            times for a plot file
!     iptime  integer  npmax                            particle age
!     iptmax  integer     1                             nr of plot grids
!     isfile  integer  nosubs                           when 1 conc. follows from external file
!                                                       then do not overwrite it in part12
!     isfud   integer  nosubs                           index array for subst. from files ud rel.
!     isub    integer  nosubs                           index array for substances for user defined relea
!     itime   integer     1                             main do-loop time       variable
!     itstop  integer     1                             main do-loop stop -time variable
!     itstrt  integer     1                             main do-loop start-time variable
!     iutime  integer  noudef                           ud release times for delpar
!     itrack  integer     1                             particle track to be followed
!     ivtime  integer  ndmax                            time of settling velocities
!     ivtmax  integer     1                             adapted nr of breakpoints set.vel.
!     iwndtm  integer  nowinx                           time on of wind changes
!     iwtime  integer  ndmax                            time of dye release
!     iyear   integer    1                              offset to real timings; year
!     kpart   integer  npmax                            3th grid index particles
!     kwaste  integer     1                             k-values waste loads in model
!     layt    integer     1                             number of layers hydr.database
!     lcorr   logical     1                             switch pred. corrector scheme
!     ldiffh  logical     1                             exchange horizont diffusion on/off
!     ldiffz  logical     1                             exchange vertical diffusion on/off
!     lgrid   integer  nmax *mmax                        active grid
!     lgrid2  integer  nmax *mmax                        total  grid
!     lgrid3  integer  nmax *mmax                        original active grid matrix
!     linear  integer  nocont                           0 = block interpolated loads
!                                                       1= linear interpolated loads
!     lsettl  logical     1                             when true settling occurs in extra bed layer
!                                                       then there is one extra layer(noslay) on output
!     lun     integer  nfiles                           array with unit numbers
!     kwaste  integer  nwmax                            k-values wasteloads in model
!     mapsub  integer  nosubs*2                         relation substances /numbers for wq routines
!     mmap    integer     1                             adapted dimension of plot grid
!     mmax    integer     1                             adapted nr. grid cells in second dir.
!     mnmaxk  integer     1                             active cells (except bed when settling is active)
!     mnmax2  integer     1                             nr. of active gridcells for one layer
!     modtyp  integer     1                             model-run-type
!     mpart   integer  npmax                            second grid index particles
!     mplsta  integer     1                             sec   dir. plot history station
!     mstat   integer     1                             sec   dir. map history station
!     mstick  integer  nosubs                           tells which substances are sticking and from whom they come
!     mwaste  integer  nwmax                            m-values wasteloads in model
!     nbin    integer  layx*nmap*mmap                   number of particles in zoom grid
!     ncheck  integer  ncmax                            check no. particles per load
!     ndprt   integer  nwmax+numax                      no. of particles per load
!     nfract  integer     1                             no .of oil fractions
!     nfracmx integer     1                             max. no. of foil fractions
!     nmap    integer     1                             adapted dimension of plot grid
!     nmax    integer     1                             adapted nr. grid cells in first dir.
!     nmdyer  char*20  ndmax                            names of dye releases
!     nmconr  char*20  ncmax                            names of dye releases
!     nmstat  char*20  nostmx                           names of mon. stations
!     nocont  integer     1                             adapted total number of continuous
!                                                       releases
!     noconsp integer     1                             adapted (max.) number of constants
!     nodye  integer     1                             adapted total number of dye releases
!     nolay   integer     1                             number of layers computation
!     nolays  integer     1                             number of layers output
!     nopam   integer     1                             adapted total number of parameters
!     nopart  integer     1                             adapted total number of particles
!     noslay  real        1                             number of layers for water phase and bed
!                                                       for lsettl = .true. noslay = nolay + 1
!     nosubs  integer     1                             total number of substances
!     nosubc  integer     1                             leading dimension conc. array
!     nosud   integer  numax                            number of subst. on file for ud release
!                                                       only required when ifopt=1
!     notrak  integer     1                             number of partciel tracks
!     noudef  integer     1                             adapted total number of u.d.releases
!     nowind  integer     1                             adapted total number of wind
!                                                       variabilities
!     npart   integer  npmax                            first grid index particles
!     nplay   integer  nolayers                         number of particles per layer
!     nplot   integer  notrak                           numbers of particles for track (zoom window)
!     nplsta  integer     1                             first dir. plot history station
!     npwndn  integer     1                             new start of active nopart number - 1
!     npwndw  integer     1                             start of active nopart number
!     nstick  integer  nosubs                           number of sticking substances
!     ntrack  integer     1                             no. particles to be followed for this sub
!     nwaste  integer  nwmax                            n-values wasteloads in model
!     oiltyp  char.20    1                              sort of oil
!     param   real     nopamx*mnmax                     user-defined parameters
!     pblay   real        1                             relative thickness lower layer
!     nstat   integer     1                             first dir. map history station
!     ptlay   real        1                             relative thickness upper layer
!     radius  real     ncmax + ndmax                    radius of the load
!     recovr  real     ipmax                            recovery for the plots
!     rem     real     ncmax                            remainder of mass to be released
!     rhow    real        1                             density of water
!     rough   real        1                             roughness length
!     stoch   real     nosubs*nocont                    stoechiometric matrix for loads
!     subst   char.*20 nosubs                           substance names plo with layers
!     substi  char.*20 nosubs                           input substance name
!     surf    real        1                             surface of a plot grid cell
!     tcktot  real     nolayers                         number of layers
!     t0buoy  real     nopart                           t0 parameter buoyancy
!     t0cf    real     ncmax                            t0 coeff. buoyancy per cont.load
!     title   char.*40    4                             model- and run titles
!     tmass   real     nosubs                           total mass dye releases per subst
!     tmassc  real     nosubs*ncmax                     total mass cont releases per subst
!     tmassu  real     ncmax                            total unit mass cont releases
!     tmasud  real     numax                            total unit mass u.d. releases
!     uscal   real     numax                            scale factor u.d. releases
!     update  logical     1                             true: new rec. form hydr. is taken
!     vdiff   real     mmax*nmax                        vertical diffusion
!     velo    real     mmax*nmax                        absolute velocity in cells
!     volume  real     mmax*nmax                        segment volumes
!     vol1    real     mmax*nmax                        segment volumes at old time      ?
!     vol2    real     mmax*nmax                        segment volumes at new time
!     vsfour  real     4*nosubs*ivtmax                  fourier series settling vel.
!     vsfact  real     4*nosubs                         actual settling velocities
!     wdir    real        1                             wind direction from north
!     wdira   real     nowinx                           wind direction degree from north
!     window  real        4                             plot grid window
!     wparm   real     nwmax+numax                      decay weighing parameter
!     wpart   real     npmax                            weights of the particles
!     wsettl  real     npmax                            settling velocity of a particle
!     wvelo   real       1                              wind velocity
!     wveloa  real     nowinx                           wind velocity  m/s
!     xa      real     npmax                            particles in the area
!     xb      real     mmax*nmax                        x-values bottom points
!     xpart   real     npmax                            x of particles in the gridcell
!     xstat   real     nostmx                           x values of mon. stations
!     xwaste  real     nwmax                            x-value of waste locations
!     ya      real     npmax                            y of particles in the area
!     yb      real     mmax*nmax                        y-values bottom points
!     ypart   real     npmax                            y of particles in the gridcell
!     ystat   real     nostmx                           y values of mon. stations
!     ywaste  real     nwmax                            y-value of waste locations
!     zpart   real     npmax                            z of particles in the gridcell
!     zwaste  real     nwmax                            relative z values of the points
!     ----    ----     -----                            ------  -----------
!
!     local variables
!     ===============
!     mmax    integer     1                             nr. grid cells in second dir.
!     nmap    integer     1                             dimension of plot grid
!     ncmax   integer     1                             total number of continuous releases
!     ndmax   integer     1                             total number of dye releases
!     nfiles  integer     1                             total number of readable files
!     nmax    integer     1                             nr. grid cells in first dir.
!     nolayx  integer     1                             total number of layers
!     nopamx  integer     1                             total number of parameters
!     norand  integer     1                             total number of random step parameters
!     noslx   integer     1                             nolayx * nosubs
!     nosta   integer     1                             actual no. monitoring stations
!     nosubs  integer     1                             total number of substances
!     npmax   integer     1                             total number of particles
!     numax   integer     1                             max. number of user defined releases
!     nwmax   integer                                   total sum of dye- and con-
!                                                       tinuous releases
!     nuwmax  integer                                   total sum of dye- and continuous and user defined releases
!
!     local scalars
!
      include "omp_lib.h"

      integer(ip)         :: ierror  , itime   , lunpr
      integer(ip)         :: nosubud , noth
      integer(ip)         :: ilp, isp, iext, nores, noras
      real(sp)            :: dtstep
      logical             :: update
      character(len=*)    :: ifnam

      real     ( dp)              :: rseed = 0.5d0
      real     ( sp)              :: rnorm
      
      integer(4) ithndl              ! handle to time this subroutine
      data ithndl / 0 /
      call timini ( )
      timon = .true.
      if ( timon ) call timstrt( "delpar", ithndl )

!     initialize normal distribution generator

      call norm_init()

!     set file types (binary for pc ; unformatted for unix)

      call filtyp()
      alone = .true.

!     read unit-numbers and file-names

      call rdfnam ( lun     , ifnam   , fname   , nfiles  , 2       ,    &
                    1       , alone   )
      lunpr = lun(2)

      call report_date_time ( lunpr   )

      noth = OMP_GET_MAX_THREADS()

      write ( lunpr  , 2020 ) noth
      write (    *   , 2030 ) noth

!     rdlgri also calculates tcktot ! Data is put in the partmem module

      call rdlgri ( nfiles , lun    , fname  , ftype )

!     read curved grid

      call rdccol ( nmaxp   , mmaxp   , lun(5)  , fname(5) , ftype  ,    &
                    lgrid2  , xb      , yb      , lun(2)   )

!     calculate distances and angles, and depth in the grid

      call part01 ( lgrid   , lgrid2  , xb      , yb      , dx      ,    &
                    dy      , area    , angle   , nmaxp   , mmaxp   )

!     from nmax, mmax get dimension of flow array

      nolayp = layt

!     initialize hydrodynamics reading

      ihdel = -999
      itime = -999
      call rdhydr ( nmaxp    , mmaxp    , mnmaxk   , nflow    , nosegp   ,    &
                    noqp     , itime    , itstrtp  , ihdel    , volumep  ,    &
                    vdiff    , area     , flow     , vol1     , vol2     ,    &
                    flow1    , vdiff1   , update   , cellpntp , flowpntp ,    &
                    tau      , tau1     , caltau   , salin    , salin1   ,    &
                    temper   , temper1  , nfiles   , lun      , fname    ,    &
                    ftype    , rhowatc)

!     Read the whole input file ! Data is put in the partmem module !

      call rdpart ( lun(1)   , lun(2)   , fname(1) )

!     calculate mapping function between plot grid and curvilinear grid

      write ( *, '(/a)',advance='no' ) '  Preparing initial stage ...'
      call plotgrp( npgrid   , pg       , nmaxp    , mmaxp    , lgrid    ,    &
                    lgrid2   , xb       , yb       )

!        part08 - calculates total released mass and mass/particle

      call part08 ( lun(2)   , nodye    , nocont   , ictmax   , amassd   ,    &
                    ictime   , amassc   , aconc    , tmass    , tmassc   ,    &
                    nosubs   , ndprt    , tmassu   , ftime    , linear   ,    &
                    substi   , nmdyer   , nmconr   )


!     calculate dump-sites in the grids

      call part06 ( lun(2)   , lgrid    , lgrid2   , nmaxp    , mmaxp    ,    &
                    xb       , yb       , nodye    , nocont   , xwaste   ,    &
                    ywaste   , nwaste   , mwaste   )

!     initialize number of active particles and other essentials

      nopart   = 0
      npwndw   = 1
      npwndn   = 0

!     some fixed intializations..

      acomp  = .false.
      accrjv = 1.0e-9_sp
      ltrack = notrak  /=  0
      oil    = modtyp == 4
      oil2dh = oil .and. layt == 1
      oil3d  = oil .and. layt  > 1


!     deflection angle for coriolis effect.(3d oil module only)

      if (oil3d) then
         defang   = const(noconsp)
      endif
!3d
!3d.. modtyp = 2 is still a 2 layer option, it assumes 2 layers
!3d.. nolay etc. specifies the total number of layers
!3d.. layt specifies the number of layers for the hydrodynamics
!3d
!3d.. pblay specifies the thickness where the particles will be flipped
!3d
!oil  if (modtyp == 1.or.modtyp==3) then
      if (modtyp == 1.or.modtyp >= 3) then
        pblay = 0.0
      elseif(modtyp==2) then
!..
!.. set pblay equal to some value that differs from zero
!.. the true value will be set by the user later
!..
        pblay = 0.7
!3d
!3d
      else
          write(*,*) 'This model type has not been implemented yet '
          call stop_exit(1)
      endif
      ptlay  = 1.0 - pblay
!
!     check maximal number of time steps for the psf's

      nstep = 1 + (itstopp - itstrtp)/idelt

!     for particle tracking:
!     ittrkc : current time step for writing to track file
!     itraki : time step increment for writing to track file

!     in this version, tracks will be written from begin to end
!     time and each time step. all particles will be written.
!     particles not yet released will be written as default (999.999)

      itrakc = 0
      itraki = 1

!     get bathymetry depths (w.r.t. reference level)

      call getdps ( lunpr   , lun(17) ,fname(17), nmaxp   , mmaxp   ,    &
                    noseglp , dpsp    , cellpntp, ltrack  )

!     compute velocities and depth

      call part03 ( lgrid   , volumep , flow    , dx      , dy      ,    &
                    nmaxp   , mmaxp   , mnmaxk  , lgrid2  , velo    ,    &
                    layt    , area    , depth   , dpsp    , locdep  ,    &
                    zlevel  , tcktot  , ltrack)

!      write particle tracks (initial state)

      if (ltrack) then
!
         call part11 ( lgrid   , xb      , yb      , nmaxp   , npart   ,    &
                       mpart   , xpart   , ypart   , xa      , ya      ,    &
                       nopart  , npwndw  , lgrid2  , kpart   , zpart   ,    &
                       za      , locdep  , dpsp    , layt    , mmaxp   ,    &
                       tcktot  )

!          write actual particle tracks (file #16)

         call wrttrk ( lun(2)  , fout    ,fname(16), itrakc  , nopart  ,    &
                       npmax   , xa      , ya      , za      , xyztrk  )
         itrakc = itrakc + itraki
!
      endif
      write(*,'(a//)') '  Ready'

!     set initial conditions of particles (only oil module)

      if ( ini_opt .eq. 1 .and. oil ) then
         call inipart( lgrid   , lgrid2  , nmaxp   , mmaxp   , xb      ,    &
                       yb      , nopart  , nosubs  , substi  , ini_file,    &
                       xpol    , ypol    , npolmax , wpart   , xpart   ,    &
                       ypart   , zpart   , npart   , mpart   , kpart   ,    &
                       iptime  , npmax   , nrowsmax, lunpr   )
      elseif ( ini_opt .eq. 2 .and. oil ) then
          call inipart_asc( lgrid   , lgrid2  , nmaxp   , mmaxp   , xb  ,    &
                       yb      , nopart  , nosubs  , substi  , ini_file,    &
                       xpol    , ypol    , wpart   , xpart   , conc2 ,       &
                       ypart   , zpart   , npart   , mpart   , kpart   ,    &
                       iptime  , npmax   , nrowsmax, lunpr   )
      endif
      if ( idp_file .ne. ' ' ) then
         if (modtyp .ne. 6) then
            write ( lunpr, * ) ' Opening initial particles file:', idp_file(1:len_trim(idp_file))
            call openfl ( 50, idp_file, ftype(2), 0 )
            read ( 50 ) ilp, nopart, nosubs
            do ilp = 1, nopart
               read( 50 ) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), wpart(1:nosubs,ilp), iptime(ilp)
            enddo
            close ( 50 )
         else
            write ( lunpr, * ) ' Opening initial particles file:', idp_file(1:len_trim(idp_file))
            call openfl ( 50, idp_file, ftype(2), 0 )
            read ( 50 ) ilp, nopart, nosubs
            do ilp = 1, nopart
               read( 50 ) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), wpart(1:nosubs,ilp), &
                          spart(1:nosubs,ilp), iptime(ilp)
            enddo
            do ilp = 1, nopart
               do isp = 1, nosubs
                  if (modtyp .eq. 6) then
                     rhopart(isp, ilp) = pldensity(isp)
                  endif                 
               enddo
            enddo
            close ( 50 )
         end if
      endif

!     Draw random log normal distributed particle sizes for non-restart particles
      if (modtyp .eq. 6) then
         do ilp = 1, npmax
            rnorm = normal(rseed)
            if (ilp .gt. nopart_res) then
               do isp = 1, nosubs
                   spart(isp,ilp) = exp(plmusize(isp) + plsigmasize(isp) * rnorm)
               enddo
            endif
         enddo
         if (pldebug) then
            size_file = fname(1)
            iext = len_trim(size_file) - 3
            size_file(iext+1:iext+5) = 'size'    !dump file for drawn plastic sizes
            open  (50, file = size_file, form = 'formatted')
            write(50 , '(A10,100A20)') 'particle', (trim(substi(isp)), isp=1,nosubs)
            do ilp = 1, npmax
               write(50 , '(I10,100E20.7)') ilp, spart(1:nosubs,ilp)
            enddo
            close(50)
         endif
      end if

!     echo start- and stop-time to screen

      write ( *, 1010 ) itstrtp/86400, mod(itstrtp, 86400)/3600, mod(itstrtp, 3600)/60, mod(itstrtp, 60),  &
                        itstopp/86400, mod(itstopp, 86400)/3600, mod(itstopp, 3600)/60, mod(itstopp, 60)

!     begin of main time-cycle loop

      do itime = itstrtp, itstopp, idelt

!        Echo actual time to screen

         write ( *, 1020) itime  /86400, mod(itime  , 86400)/3600, mod(itime  , 3600)/60, mod(itime  , 60),  &
                          itstopp/86400, mod(itstopp, 86400)/3600, mod(itstopp, 3600)/60, mod(itstopp, 60),  &
                          nopart - npwndw + 1, npmax
         write (lun(2), '(/a)')                                   &
          '----------------------------------------------------------------------------------'
         write (lun(2), 1020 ) itime  /86400, mod(itime  , 86400)/3600, mod(itime  , 3600)/60, mod(itime  , 60),  &
                               itstopp/86400, mod(itstopp, 86400)/3600, mod(itstopp, 3600)/60, mod(itstopp, 60),  &
                               nopart - npwndw + 1, npmax

!        Part15 adapts wind and direction for actual time
         call part15 ( lun(2)   , itime    , spawnd   , mnmax2   , nowind   ,    &
                       iwndtm   , wveloa   , wdira    , wvelo    , wdir     )

!        Rdhydr reads hydrodynamic water-flow

         call rdhydr ( nmaxp    , mmaxp    , mnmaxk   , nflow    , nosegp   ,    &
                       noqp     , itime    , itstrtp  , ihdel    , volumep  ,    &
                       vdiff    , area     , flow     , vol1     , vol2     ,    &
                       flow1    , vdiff1   , update   , cellpntp , flowpntp ,    &
                       tau      , tau1     , caltau   , salin    , salin1   ,    &
                       temper   , temper1  , nfiles   , lun      , fname    ,    &
                       ftype    , rhowatc)

!        Part12 makes .map files, binary and Nefis versions

         call part12 ( lun(8)   , fname(8) , lun(2)   , title    , subst2   ,    &
                       lgrid    , lgrid2   , lgrid3   , nmaxp    , mmaxp    ,    &
                       concp    , volumep  , npart    , mpart    , wpart    ,    &
                       nopart   , itime    , idelt    , icwsta   , icwsto   ,    &
                       icwste   , atotal   , npwndw   , kpart    , pblay    ,    &
                       iptime   , npwndn   , modtyp   , nosubs   , noslay   ,    &
                       iyear    , imonth   , iofset   , pg(1)    , rbuffr   ,    &
                       nosta    , mnmax2   , noseglp  , isfile   , mapsub   ,    &
                       layt     , area     , nfract   , lsettl   , mstick   ,    &
                       elt_names, elt_types, elt_dims , elt_bytes, locdep   ,    &
                       nosub_max, bufsize  )

!        Part13 makes 3d detail plot grids corrected for recovery rate

         call part13 ( lun(9)   , fname(9) , lun(2)   , title    , subst2   ,    &
                       lgrid2   , nmaxp    , volumep  , area     , npart    ,    &
                       mpart    , xpart    , ypart    , wpart    , nopart   ,    &
                       itime    , idelt    , ipset    , iptset   , xa       ,    &
                       ya       , xb       , yb       , pg(1)    , recovr   ,    &
                       atotal   , iyear    , imonth   , iofset   , npwndw   ,    &
                       lgrid    , pblay    , modtyp   , apeak    , adepth   ,    &
                       noslay   , nosubs   , rbuffr   , kpart    , itrack   ,    &
                       nplot    , mapsub   , ntrack   , isfile   , mmaxp    ,    &
                       nfract   , lsettl   , mstick   , elt_names, elt_types,    &
                       elt_dims , elt_bytes, locdep   , zpart    , za       ,    &
                       dpsp     , tcktot   , nosub_max, bufsize  )

!        Parths makes 2D averaged time histories every ihstep

         call parths ( lun(13)  , lun(2)   , title    , subst    , mmaxp    ,    &
                       lgrid2   , nmaxp    , volumep  , area     , npart    ,    &
                       mpart    , xpart    , ypart    , wpart    , nopart   ,    &
                       itime    , idelt    , xa       , npwndw   , lgrid    ,    &
                       ya       , xb       , yb       , pg(1)    , pblay    ,    &
                       modtyp   , noslay   , nosubs   , concp    , chismp   ,    &
                       chispl   , nosta    , nmstat   , xstat    , ystat    ,    &
                       nstat    , mstat    , nplsta   , mplsta   , ihstrtp  ,    &
                       ihstopp  , ihstepp  , ihplot   , fname(13), kpart    ,    &
                       mnmax2   , noseglp  , nfract   , lsettl   , mstick   ,    &
                       elt_names, elt_types, elt_dims , elt_bytes, rbuffr   ,    &
                       zpart    , za       , locdep   , dpsp     , tcktot   ,    &
                       lgrid3   )

         if ( itime .ge. itstopp ) exit    ! <=== here the simulation loop ends

!        Part03 computes velocities and depth

         call part03 ( lgrid    , volumep  , flow     , dx       , dy       ,    &
                       nmaxp    , mmaxp    , mnmaxk   , lgrid2   , velo     ,    &
                       layt     , area     , depth    , dpsp     ,               &
                       locdep   , zlevel   , tcktot   , ltrack)

!        This section does water quality processes

         select case ( modtyp )

            case ( 1 )     ! = conservative tracer model

            case ( 2, 5 )  ! = temperature model
               call partwq ( lgrid    , nmaxp    , concp    , volumep  , area     ,    &
                             npart    , mpart    , wpart    , radius   , nodye    ,    &
                             npwndw   , nopart   , idelt    , velo     , wvelo    ,    &
                             const    , noconsp  , ptlay    , lun(2)   , nosubs   ,    &
                             nolayp   , lgrid2   , mmaxp    , xb       , yb       ,    &
                             t0cf     , acf      , nwaste   , mwaste   , kpart    ,    &
                             mapsub   , layt     , mnmaxk   )

            case ( 3 )     ! = obsolete

            case ( 4 )     ! = oil model
               call oildsp ( lgrid    , nmaxp    , concp    , volumep  , area     ,    &
                             npart    , mpart    , wpart    , radius   , nodye    ,    &
                             npwndw   , nopart   , itime    , idelt    , wvelo    ,    &
                             const    , lun(2)   , nosubs   , noslay   , lgrid2   ,    &
                             lgrid3   ,                                                &
                             mmaxp    , xb       , yb       , kpart    , mapsub   ,    &
                             isfile   , nfract   , mstick   , nstick   , fstick   ,    &
                             xa       , ya       , pg(1)    , lsettl   , xpart    ,    &
                             ypart    , zpart    , za       , locdep   , dpsp     ,    &
                             tcktot   , substi   ,            npmax    , rhow     ,    &
                             amassd   , ioptrad  , ndisapp  , idisset  , tydisp   ,    &
                             efdisp   , xpoldis  , ypoldis  , nrowsdis , wpartini ,    &
                             iptime)

         end select

!     two-layer system with stratification

         if ( modtyp .eq. 2 )          &
         call part18 ( lgrid    , velo     , concp    , flres    , volumep  ,    &
                       area     , mnmaxk   , npart    , mpart    , wpart    ,    &
                       zpart    , nopart   , idelt    , nolayp   , npwndw   ,    &
                       vdiff    , pblay    , ptlay    , const    , noconsp  ,    &
                       lun(2)   , nosubs   , layt     , kpart    , mapsub(1),    &
                       wvelo    , alpha    , nosubc   , mapsub(2) )

!      add dye release

         if ( nodye .gt. 0 )           &
         call part09 ( lun(2)   , itime    , nodye    , nwaste   , mwaste   ,    &
                       xwaste   , ywaste   , iwtime   , amassd   , aconc    ,    &
                       npart    , mpart    , xpart    , ypart    , zpart    ,    &
                       wpart    , iptime   , nopart   , radius   , lgrid    ,    &
                       dx       , dy       , ndprt    , nosubs   , kpart    ,    &
                       layt     , tcktot   , nplay    , kwaste   , nolayp   ,    &
                       modtyp   , zwaste   , track    , nmdyer   , substi   ,    &
                       rhopart)

!      add continuous release

         if ( nocont .gt. 0 )          &
         call part14 ( itime    , idelt    , nodye    , nocont   , ictime   ,    &
                       ictmax   , nwaste   , mwaste   , xwaste   , ywaste   ,    &
                       zwaste   , aconc    , rem      , npart    , ndprt    ,    &
                       mpart    , xpart    , ypart    , zpart    , wpart    ,    &
                       iptime   , nopart   , pblay    , radius   , lgrid    ,    &
                       dx       , dy       , ftime    , tmassu   , nosubs   ,    &
                       ncheck   , t0buoy   , modtyp   , abuoy    , t0cf     ,    &
                       acf      , lun(2)   , kpart    , layt     , tcktot   ,    &
                       nplay    , kwaste   , nolayp   , linear   , track    ,    &
                       nmconr   , spart    , rhopart  , noconsp  , const)

!        write particle tracks

         if (ltrack) then            ! get the absolute x,y,z's of the particles
            call part11 ( lgrid    , xb       , yb       , nmaxp    , npart    ,    &
                          mpart    , xpart    , ypart    , xa       , ya       ,    &
                          nopart   , npwndw   , lgrid2   , kpart    , zpart    ,    &
                          za       , locdep   , dpsp     , nolayp   , mmaxp    ,    &
                          tcktot   )
                                     ! write actual particle tracks (file #16)
            call wrttrk ( lun(2)   , fout     , fname(16), itrakc   , nopart  ,    &
                          npmax    , xa       , ya       , za       , xyztrk  )
            itrakc = itrakc + itraki
         endif

         if ( noudef .gt. 0 )  then

!          add release in a way defined by the user
!          array isub contains references to substances

            call partur ( itime    , noudef   , iutime   , mpart    , npart    ,    &
                          kpart    , xpart    , ypart    , zpart    , wpart    ,    &
                          iptime   , nopart   , lgrid    , nmaxp    , mmaxp    ,    &
                          tmasud   , ipntp    , substi   , nosubs   , nolayp   ,    &
                          nocont   , ndprt    , nodye    , lun(2)   , rbuffr   ,    &
                          volumep  , aconud   , uscal    , isub     , finud    ,    &
                          iftime   , ifopt    , nosyss   , isfud    , nosubud  ,    &
                          subsud   )

         endif

!        calculate the settling velocities on a refined grid
!                           (NB: this routine is NOT part of any test in the testbench)
         if ( anfac .ne. 0.0 ) then
            call part21 ( lun(2)   , lgrid    , lgrid2   , xb       , yb       ,    &
                          area     , volumep  , nmaxp    , mmaxp    , noslay   ,    &
                          nosubs   , nopart   , npart    , mpart    , kpart    ,    &
                          xpart    , ypart    , zpart    , wpart    , npwndw   ,    &
                          pg(1)    , amapsett , xa       , ya       , za       ,    &
                          atotal   , apeak    , adepth   , imap     , nplay    ,    &
                          wsettl   , irfac    , anfac    , lsettl   , locdep   ,    &
                          tcktot   , dpsp     )
         else
            wsettl = 1.0  ! whole array assignment
         endif
         call partvs ( lun(2)   , itime    , nosubs   , nopart   , ivtset   ,    &
                       ivtime   , vsfour   , vsfact   , wpart    , wsettl   ,    &
                       modtyp   , nmaxp    , mmaxp    , lgrid3   , noslay   ,    &
                       npart    , mpart    , kpart    , nosegp   , noseglp  ,    &
                       rhopart  , rhowatc  , spart    , iptime)


!         calculate actual decaycoefficient

         if ( idtset .gt. 0 )                                                    &
         call part17 ( itime    , nosubs   , idtset   , idtime   , decay    ,    &
                       decays   )

!         calculate actual displacement  3d version
!         this routine must be called with the number of hydrodynamic layers

         call part10 ( lgrid    , volumep  , flow     , dx       , dy       ,    &
                       area     , angle    , nmaxp    , mnmaxk   , idelt    ,    &
                       nopart   , npart    , mpart    , xpart    , ypart    ,    &
                       zpart    , iptime   , rough    , drand    , lgrid2   ,    &
                       wvelo    , wdir     , decays   , wpart    , pblay    ,    &
                       npwndw   , vdiff    , nosubs   , dfact    , modtyp   ,    &
                       t0buoy   , abuoy    , kpart    , mmaxp    , layt     ,    &
                       wsettl   , depth    , ldiffz   , ldiffh   , lcorr    ,    &
                       acomp    , ipc      , accrjv   , xb       , yb       ,    &
                       tcktot   , lun(2)   , alpha    , mapsub   , nfract   ,    &
                       taucs    , tauce    , chezy    , rhow     , lsettl   ,    &
                       mstick   , nstick   , ioptdv   , cdisp    , dminim   ,    &
                       fstick   , defang   , floil    , xpart0   , ypart0   ,    &
                       xa0      , ya0      , xa       , ya       , npart0   ,    &
                       mpart0   , za       , locdep   , dpsp     , nolayp   ,    &
                       vrtdsp   , stickdf  , subst    , nbmax    , nconn    ,    &
                       conn     , tau      , caltau   , nboomint , iboomset ,    &
                       tyboom   , efboom   , xpolboom , ypolboom , nrowsboom ,    &
                       itime)

!         print test data for checking purposes (< 100 particles)
!          if (nopart  >=  1 .and. nopart .le. 100) then   &
!             call report (lunpr  ,nopart, modtyp, floil  ,stoil ,  &
!     *                    mpart  ,npart , kpart , xpart  ,ypart ,  &
!     *                    zpart  ,vrtdsp ,wsettl )
!          endif

      enddo

!     write initial information to track file

      if (ltrack) then
         dtstep = float(idelt)
         call writrk ( lun(2)   , fout     , fname(16), nopart   , title(4) ,    &
                       dtstep   , nstep    , ibuff    , rbuff    , cbuff    ,    &
                       track    , npmax    )
      endif
      call exit_alloc ( nstep )

      call delete_file ( "particle.wrk", ierror )

      if (write_restart_file) then
         ! first to calculate the number of particles in the restart files
         nores = 0
         noras = 0
         do ilp = 1, nopart
            if (npart(ilp)>1.and.mpart(ilp)>1) then          !only for the active particles
               if (lgrid( npart(ilp), mpart(ilp)).ge.1) then
                  nores = nores + 1          ! only for the active particles
                  if (max_restart_age .gt. 0 .and. iptime(ilp) .lt. max_restart_age) then
                     noras = noras + 1       ! if max_restart_age is a positve and the particles' age is less then max_restart_age
                  end if
               end if
            end if
         enddo

         res_file = fname(1)
         iext = len_trim(res_file) - 3
         if (max_restart_age .lt. 0) then
!           Write the restart file with all active paritcles
            if (modtyp.eq.6)then
               res_file(iext+1:iext+4) = 'ses'    !limited number of particles (for 'plastics' modeltype 6 restart, as 'ras' but including settling values)
               write ( lunpr, * ) ' Including particle dependent settling velocity'
            else
               res_file(iext+1:iext+4) = 'res'     !all results, except those that are inactive (outside model)
            end if
            write ( lunpr, * ) ' Opening restart particles file:', idp_file(1:len_trim(res_file))
            call openfl ( 50, res_file, ftype(2), 1 )
            write ( 50 ) 0, nores, nosubs

            do ilp = 1, nopart
               if (npart(ilp)>1.and.mpart(ilp)>1) then
                  if (lgrid( npart(ilp), mpart(ilp)).ge.1) then  !only for the active particles
                     if (modtyp.ne.6) then
                        write ( 50 ) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), &
                                     wpart(1:nosubs,ilp), iptime(ilp)
                     else
                        write ( 50 ) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), &
                                     wpart(1:nosubs,ilp), spart(1:nosubs,ilp), iptime(ilp)
                     end if
                  end if
               end if
            enddo
            write (lunpr,*) ' Number of active particles in the restart file: ',nores
            close ( 50 )
         else
!        Write the restart file with all active paritcles below a certain age
            if (modtyp.eq.6)then
               res_file(iext+1:iext+4) = 'sas'    !limited number of particles (for 'plastics' modeltype 6 restart, as 'ras' but including settling values)
               write ( lunpr, * ) ' Including particle dependent settling velocity'
            else
               res_file(iext+1:iext+4) = 'ras'    !limited number of particles (remove particles older than a certain age or inactive)
            end if
            write ( lunpr, * ) ' Opening restart particles file:', idp_file(1:len_trim(res_file))
            write ( lunpr, * ) ' Particles older than ',max_restart_age,' seconds are removed'
            call openfl ( 50, res_file, ftype(2), 1 )
            write ( 50 ) 0, noras, nosubs

            do ilp = 1, nopart
               if (npart(ilp)>1.and.mpart(ilp)>1) then
                  if (lgrid( npart(ilp), mpart(ilp)).ge.1 .and. (iptime(ilp).lt.max_restart_age)) then   !only when the particles' age less than max_restart_age, time in seconds
                     if (modtyp.ne.6) then
                        write ( 50 ) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), &
                                     wpart(1:nosubs,ilp),iptime(ilp)
                     else
                        write ( 50 ) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), &
                                     wpart(1:nosubs,ilp), spart(1:nosubs,ilp), iptime(ilp)
                     end if
                  end if
               end if
            enddo
            write (lunpr,*) ' Number of active particles in the restart file below maximum age: ',noras
            close ( 50 )
         end if
      end if

      call report_date_time(lunpr)
      write ( *    , '(//a)') ' Normal end of PART simulation'
      write ( lunpr, '(//a)') ' Normal end of PART simulation'

      if ( timon ) then
         call timstop ( ithndl )
         call timdump ( fname(1)(1:index(fname(1),".",.true.)-1)//'-timers.out' )
      endif
      return

!     formats

 1010 format( '  Start  time :', i6.4 ,'D-', i2.2 ,'H-', i2.2 , 'M-', i2.2 ,'S.'/          &
              '  Stop   time :', i6.4 ,'D-', i2.2 ,'H-', i2.2 , 'M-', i2.2 ,'S.'// )
 1020 format( '  Time ', i6.4 ,'D-', i2.2 ,'H-', i2.2 ,'M-', i2.2 ,'S.',' Stop time ',     &
                i6.4 ,'D-', i2.2 ,'H-', i2.2 ,'M-', i2.2 ,'S.', i11,' part. (of',i11,')')

 2020 format (/'  Parallel processing with ',i3,' processor(s)'/)
 2030 format (/'  Parallel processing with ',i3,' processor(s)'/)

      end subroutine delpar
end module

