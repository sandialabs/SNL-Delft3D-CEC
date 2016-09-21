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

      SUBROUTINE DLWQIV ( LUREP , NOCONS, NOPA  , NOFUN , NOSFUN,
     +                    NOSYS , NOTOT , NODISP, NOVELO, NODEF ,
     +                    NOLOC , NDSPX , NVELX , NLOCX , NFLUX ,
     +                    NOPRED, NOVAR , VARARR, VARIDX, VARTDA,
     +                    VARDAG, VARTAG, VARAGG, NOGRID, VGRSET)
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            : Jan van Beek
!
!     FUNCTION            : Initialisation of Variables structure
!
!     SUBROUTINES CALLED  :
!
!     FILES               :
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!
!     Declaration of arguments
!

      use timers

      INTEGER             LUREP , NOCONS, NOPA  , NOFUN , NOSFUN,
     +                    NOSYS , NOTOT , NODISP, NOVELO, NODEF ,
     +                    NOLOC , NDSPX , NVELX , NLOCX , NFLUX ,
     +                    NOPRED, NOVAR
      INTEGER             VARARR(NOVAR) , VARIDX(NOVAR) ,
     +                    VARTDA(NOVAR) , VARDAG(NOVAR) ,
     +                    VARTAG(NOVAR) , VARAGG(NOVAR)
      INTEGER             VGRSET(NOVAR,NOGRID)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqiv", ithandl )
!
!     Just take the used array's in the right order
!
      IIVOL  =  1
      IIAREA =  2
      IIFLOW =  3
      IILENG =  4
      IIDISP =  5
      IICONC =  6
      IIMASS =  7
      IIDERV =  8
      IIBOUN =  9
      IIBSET = 10
      IIBSAV = 11
      IIWSTE = 12
      IICONS = 13
      IIPARM = 14
      IIFUNC = 15
      IISFUN = 16
      IIDNEW = 17
      IIDIFF = 18
      IIVNEW = 19
      IIVELO = 20
      IIHARM = 21
      IIFARR = 22
      IIMAS2 = 23
      IITIMR = 24
      IIVOL2 = 25
      IITRAC = 26
      IIGWRK = 27
      IIGHES = 28
      IIGSOL = 29
      IIGDIA = 30
      IIGTRI = 31
      IISMAS = 32
      IIPLOC = 33
      IIDEFA = 34
      IIFLUX = 35
      IISTOC = 36
      IIFLXD = 37
      IIFLXI = 38
      IIRIOB = 39
      IIDSPX = 40
      IIVELX = 41
      IILOCX = 42
      IIDSTO = 43
      IIVSTO = 44
      IIDMPQ = 45
      IIDMPS = 46
      IITRRA = 47
      IINRSP = 48
      IIVOLL = 49
      IIVOL3 = 50
      IIR1   = 51
      IIQXK  = 52
      IIQYK  = 53
      IIQZK  = 54
      IIDIFX = 55
      IIDIFY = 56
      IIDIFZ = 57
      IIVOLA = 58
      IIVOLB = 59
      IIGUV  = 60
      IIGVU  = 61
      IIGZZ  = 62
      IIAAK  = 63
      IIBBK  = 64
      IICCK  = 65
      IIBD3X = 66
      IIBDDX = 67
      IIBDX  = 68
      IIBU3X = 69
      IIBUUX = 70
      IIBUX  = 71
      IIWRK1 = 72
      IIWRK2 = 73
      IIAAKL = 74
      IIBBKL = 75
      IICCKL = 76
      IIDDKL = 77
!
      IVVOL = 1
      IVARE = IVVOL + 1
      IVFLO = IVARE + 1
      IVLEN = IVFLO + 1
      IVCNS = IVLEN + 2
      IVPAR = IVCNS + NOCONS
      IVFUN = IVPAR + NOPA
      IVSFU = IVFUN + NOFUN
      IVCNC = IVSFU + NOSFUN
      IVMAS = IVCNC + NOTOT
      IVDER = IVMAS + NOTOT
      IVDSP = IVDER + NOTOT
      IVVEL = IVDSP + NODISP
      IVDEF = IVVEL + NOVELO
      IVLOC = IVDEF + NODEF
      IVDSX = IVLOC + NOLOC
      IVVLX = IVDSX + NDSPX
      IVLCX = IVVLX + NVELX
      IVFLX = IVLCX + NLOCX
!
!
!
      CALL DHZERI(VGRSET,NOVAR*NOGRID)
!
!     Volume
!
      IVAR = 1
!     VARARR(IVAR) = IIVOL
!     VARIDX(IVAR) = 1
!     VARTDA(IVAR) = 0
!     VARDAG(IVAR) = 0
!     VARTAG(IVAR) = 1
!     VARAGG(IVAR) = 0
      VGRSET(IVAR,1) = 1
!
!     Area
!
      IVAR = IVAR + 1
!     VARARR(IVAR) = IIAREA
!     VARIDX(IVAR) = 1
!     VARTDA(IVAR) = 0
!     VARDAG(IVAR) = 0
!     VARTAG(IVAR) = 0
!     VARAGG(IVAR) = 0
      VGRSET(IVAR,1) = 1
!
!     Flow
!
      IVAR = IVAR + 1
!     VARARR(IVAR) = IIFLOW
!     VARIDX(IVAR) = 1
!     VARTDA(IVAR) = 0
!     VARDAG(IVAR) = 0
!     VARTAG(IVAR) = 0
!     VARAGG(IVAR) = 0
      VGRSET(IVAR,1) = 1
!
!     Length , two length
!
      IVAR = IVAR + 1
!     VARARR(IVAR) = IILENG
!     VARIDX(IVAR) = 1
!     VARTDA(IVAR) = 0
!     VARDAG(IVAR) = 0
!     VARTAG(IVAR) = 0
!     VARAGG(IVAR) = 0
      VGRSET(IVAR,1) = 1
      IVAR = IVAR + 1
!     VARARR(IVAR) = IILENG
!     VARIDX(IVAR) = 2
!     VARTDA(IVAR) = 0
!     VARDAG(IVAR) = 0
!     VARTAG(IVAR) = 0
!     VARAGG(IVAR) = 0
      VGRSET(IVAR,1) = 1
!
!     Cons
!
      DO ICONS = 1 , NOCONS
         IVAR = IVAR + 1
!        VARARR(IVAR) = IICONS
!        VARIDX(IVAR) = ICONS
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Param
!
      DO IPA = 1 , NOPA
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIPARM
!        VARIDX(IVAR) = IPA
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 3
!        VARAGG(IVAR) = 1
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Func
!
      DO IFUN = 1 , NOFUN
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIFUNC
!        VARIDX(IVAR) = IFUN
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Seg Func
!
      DO ISFUN = 1 , NOSFUN
         IVAR = IVAR + 1
!        VARARR(IVAR) = IISFUN
!        VARIDX(IVAR) = ISFUN
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 3
!        VARAGG(IVAR) = 1
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Conc
!
      DO ISYS = 1 , NOSYS
         IVAR = IVAR + 1
!        VARARR(IVAR) = IICONC
!        VARIDX(IVAR) = ISYS
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 3
!        VARAGG(IVAR) = 1
         VGRSET(IVAR,1) = 1
      ENDDO
      DO ISYS = NOSYS + 1 , NOTOT
         IVAR = IVAR + 1
!        VARARR(IVAR) = IICONC
!        VARIDX(IVAR) = ISYS
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 1
!        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Mass
!
      DO ISYS = 1 , NOTOT
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIMASS
!        VARIDX(IVAR) = ISYS
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 1
!        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Deriv
!
      DO ISYS = 1 , NOTOT
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIDERV
!        VARIDX(IVAR) = ISYS
!        VARTDA(IVAR) = 2
!        VARDAG(IVAR) = IVMAS + ISYS - 1
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
      ENDDO
!
!     Disp
!
      DO IDSP = 1 , NODISP
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIDISP
!        VARIDX(IVAR) = IDSP
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Velo
!
      DO IVEL = 1 , NOVELO
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIVELO
!        VARIDX(IVAR) = IVEL
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Default
!
      DO IDEF = 1 , NODEF
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIDEFA
!        VARIDX(IVAR) = IDEF
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
         VGRSET(IVAR,1) = 1
      ENDDO
!
!     Local
!
      DO ILOC = 1 , NOLOC
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIPLOC
!        VARIDX(IVAR) = ILOC
!        VARTDA(IVAR) = 1
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 3
!        VARAGG(IVAR) = 1
      ENDDO
!
!     DSPX
!
      DO IDSX = 1 , NDSPX
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIDSPX
!        VARIDX(IVAR) = IDSX
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
      ENDDO
!
!     VELX
!
      DO IVLX = 1 , NVELX
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIVELX
!        VARIDX(IVAR) = IVLX
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
      ENDDO
!
!     LOCX
!
      DO ILCX = 1 , NLOCX
         IVAR = IVAR + 1
!        VARARR(IVAR) = IILOCX
!        VARIDX(IVAR) = ILCX
!        VARTDA(IVAR) = 0
!        VARDAG(IVAR) = 0
!        VARTAG(IVAR) = 0
!        VARAGG(IVAR) = 0
      ENDDO
!
!     FLUX
!
      DO IFLX = 1 , NFLUX
         IVAR = IVAR + 1
!        VARARR(IVAR) = IIFLUX
!        VARIDX(IVAR) = IFLX
!        VARTDA(IVAR) = 2
!        VARDAG(IVAR) = IVVOL
!        VARTAG(IVAR) = 1
!        VARAGG(IVAR) = 0
      ENDDO
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
