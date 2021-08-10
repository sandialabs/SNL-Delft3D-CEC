!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2020.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id: rest.F90 65778 2020-01-14 14:07:42Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/rest.F90 $

      SUBROUTINE dCROSS(X1,Y1,X2,Y2,X3,Y3,X4,Y4,JACROS,SL,SM,XCR,YCR,CRP) ! liggen 3 en 4 aan weerszijden van lijn 12
      use m_sferic
      use geometry_module, only: getdxdy, sphertoCart3D, Cart3Dtospher, crossinbox
      use m_missing, only: dmiss
      implicit none
      double precision           :: det
      double precision           :: eps
      DOUBLE PRECISION           :: X1,Y1,X2,Y2,X3,Y3,X4,Y4,SL,SM,XCR,YCR,CRP
      INTEGER                    :: JACROS
                                 
      DOUBLE PRECISION           :: X21, Y21, X43, Y43, X31, Y31
      double precision           :: xx1, yy1, zz1
      double precision           :: xx2, yy2, zz2
      double precision           :: xx3, yy3, zz3
      double precision           :: xx4, yy4, zz4
      double precision           :: xx21, yy21, zz21
      double precision           :: xx43, yy43, zz43
      double precision           :: xx31, yy31, zz31
      double precision           :: xxn, yyn, zzn
      double precision           :: det2
      double precision           :: xxcr, yycr, zzcr
      

      JACROS = 0
      EPS    = 0.00001d0
!     SL     = LABDA TUSSEN 0 EN 1 OP EERSTE PAAR
!     Sm     = LABDA TUSSEN 0 EN 1 OP TWEEDE PAAR
      
      if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
         call sphertoCart3D(x1,y1,xx1,yy1,zz1)
         call sphertoCart3D(x2,y2,xx2,yy2,zz2)
         call sphertoCart3D(x3,y3,xx3,yy3,zz3)
         call sphertoCart3D(x4,y4,xx4,yy4,zz4)
         
         xx21 = xx2 - xx1
         yy21 = yy2 - yy1
         zz21 = zz2 - zz1
         
         xx43 = xx4 - xx3
         yy43 = yy4 - yy3
         zz43 = zz4 - zz3
         
         xx31 = xx3 - xx1
         yy31 = yy3 - yy1
         zz31 = zz3 - zz1
         
         xxn = yy43*zz21 - zz43*yy21
         yyn = zz43*xx21 - xx43*zz21
         zzn = xx43*yy21 - yy43*xx21
         
         det2 = xxn**2 + yyn**2 + zzn**2
         det  = sqrt(det2)
         
         if ( det.lt.eps ) then
            return
         else
            SL = ( xxn*(yy43*zz31-zz43*yy31) + yyn*(zz43*xx31-xx43*zz31) + zzn*(xx43*yy31-yy43*xx31) ) / det2
            SM = ( xxn*(yy21*zz31-zz21*yy31) + yyn*(zz21*xx31-xx21*zz31) + zzn*(xx21*yy31-yy21*xx31) ) / det2
            
            xxcr = 0.5d0*(xx1 + SL*xx21 + xx3 + SM*xx43)
            yycr = 0.5d0*(yy1 + SL*yy21 + yy3 + SM*yy43)
            zzcr = 0.5d0*(zz1 + SL*zz21 + zz3 + SM*zz43)
            
            call Cart3Dtospher(xxcr,yycr,zzcr,xcr,ycr,maxval((/x1,x2,x3,x4/)))
!            CRP = -DET
            crp = -(xxn*xxcr + yyn*yycr + zzn*zzcr) / sqrt( xxcr**2 + yycr**2 + zzcr**2)
            IF (SM >= 0d0 .AND. SM <= 1d0) then
               JACROS = 1
            ENDIF
         end if
         
      else
         call getdxdy(x1,y1,x2,y2,x21,y21,jsferic)
         call getdxdy(x3,y3,x4,y4,x43,y43,jsferic)
         call getdxdy(x1,y1,x3,y3,x31,y31,jsferic)
         
         !X21 =  getdx(x1,y1,x2,y2)
         !Y21 =  getdy(x1,y1,x2,y2)
         !X43 =  getdx(x3,y3,x4,y4)
         !Y43 =  getdy(x3,y3,x4,y4)
         !X31 =  getdx(x1,y1,x3,y3)
         !Y31 =  getdy(x1,y1,x3,y3)
         DET    = X43*Y21 - Y43*X21
         IF (ABS(DET) .LT. EPS) THEN
            RETURN
         ELSE
            SM = (Y31*X21 - X31*Y21) / DET
            IF (ABS(X21) .GT. EPS) THEN
               SL = (SM*X43 + X31) / X21
            ELSE IF (ABS(Y21) .GT. EPS) THEN
               SL = (SM*Y43 + Y31) / Y21
            ELSE
               SL   = 0d0
            ENDIF
            XCR = X1 + SL*(X2-X1)
            YCR = Y1 + SL*(Y2-Y1)
            CRP = -DET
            IF (SM >= 0d0 .AND. SM <= 1d0) then
               JACROS = 1
            ENDIF
         ENDIF
      end if
      
      RETURN
      END subroutine dcross

      double precision FUNCTION DISLIN(X,Y,N,XX,YY,TV)
      
!     AFSTAND VAN PUNT XX,YY TOT LIJN MET PARM TV
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D
      
      implicit none
      integer :: n
      double precision :: tv
      double precision :: xv
      double precision :: xx
      double precision :: yv
      double precision :: yy

      double precision :: X(N), Y(N)
      TV   = MAX(0d0,MIN(TV,N-1d0))
      CALL LINT(X,Y,N,TV,XV,YV)
      dislin = dbdistance(XV,YV,XX,YY,jsferic, jasfer3D, dmiss)
      RETURN
      END function dislin

      SUBROUTINE LINT(X,Y,N,TV,XV,YV)
      implicit none
      integer :: n
      integer :: n1
      integer :: n2
      integer :: ntv
      double precision :: t
      double precision :: tv
      double precision :: xv
      double precision :: yv
!     Lineaire interpolatie op TV in lijn
      double precision :: X(N), Y(N)
      NTV  = INT(TV)
      T    = TV  - NTV
      N1   = NTV + 1
      N2   = N1  + 1
      XV   = (1-T)*X(N1) + T*X(N2)
      YV   = (1-T)*Y(N1) + T*Y(N2)
      RETURN
      END subroutine LINT

      SUBROUTINE GOLDLN(AX,BX,CX,TOL,XMIN,P,Q,N,XX,YY,DIS)
      implicit none
      double precision :: ax
      double precision :: bx
      double precision :: c
      double precision :: cx
      double precision :: dis
      double precision :: f0
      double precision :: f1
      double precision :: f2
      double precision :: f3
      integer :: n
      double precision :: r
      double precision :: tol
      double precision :: x0
      double precision :: x1
      double precision :: x2
      double precision :: x3
      double precision :: xmin
      double precision :: xx
      double precision :: yy
      PARAMETER (R=.61803399,C=.38196602)
!     EENDIMENSIONAAL ZOEKEN VAN 'GEBRACKED' MINIMUM
      double precision :: P(N), Q(N)
      double precision :: dislin
      X0=AX
      X3=CX
      IF(ABS(CX-BX).GT.ABS(BX-AX))THEN
        X1=BX
        X2=BX+C*(CX-BX)
      ELSE
        X2=BX
        X1=BX-C*(BX-AX)
      ENDIF
!     F1=F(X1)
!     F1=DIST(P,P2,Q,Q2,XX,YY,X1,N)
      F1=DISLIN(P,Q,N,XX,YY,X1)
!     F2=F(X2)
!     F2=DIST(P,P2,Q,Q2,XX,YY,X2,N)
      F2=DISLIN(P,Q,N,XX,YY,X2)
1     IF(ABS(X3-X0).GT.TOL*(ABS(X1)+ABS(X2)))THEN
        IF(F2.LT.F1)THEN
          X0=X1
          X1=X2
          X2=R*X1+C*X3
          F0=F1
          F1=F2
!         F2=DIST(P,P2,Q,Q2,XX,YY,X2,N)
          F2=DISLIN(P,Q,N,XX,YY,X2)
!         F2=F(X2)
        ELSE
          X3=X2
          X2=X1
          X1=R*X2+C*X0
          F3=F2
          F2=F1
!         F1=F(X1)
          F1=DISLIN(P,Q,N,XX,YY,X1)
!         F1=DIST(P,P2,Q,Q2,XX,YY,X1,N)
        ENDIF
      GOTO 1
      ENDIF
      IF(F1.LT.F2)THEN
        DIS =F1
        XMIN=X1
      ELSE
        DIS =F2
        XMIN=X2
      ENDIF
      RETURN
      END


    SUBROUTINE REALAN( MLAN, ANTOT)
      use m_polygon
      use M_landboundary
      USE M_MISSING
      implicit none
      integer, intent(inout)                ::  mlan
      integer, intent(inout), optional      ::  antot

      integer :: i
      integer :: ncl
      integer :: newlin
      integer :: nkol
      integer :: nrow
      integer :: ntot, n, k, kd, ku
      double precision :: xlr

      CHARACTER CHARMC*5, MATR*4, REC*132
      DOUBLE PRECISION :: XL, YL, ZL

      if (present(antot)) then
         NTOT   = antot
      else
         NTOT   = 0
      endif
      
      if (ntot == 0) then
         call increaselan(10000)
      endif

      CALL READYY('READING land boundary',0d0)
   10 CONTINUE
      READ(MLAN,'(A)',END=777,ERR=887) MATR
      IF (MATR(1:1) .EQ. '*') GOTO 10

      READ(MLAN,'(A)',END = 777) REC
      READ(REC,*,ERR = 666) NROW, NKOL

      NEWLIN = 0
      DO 20 I = 1,NROW
         IF (NTOT .GE. MAXLAN-1) THEN
            call increaselan(NTOT+1)
         ENDIF
         READ(MLAN,'(A)',END = 999) REC
         NCL = 0
         ZL  = 0
         if (nkol == 2) then
            READ (REC,*,ERR=881) XL,YL
         else if (nkol == 3) then
            READ (REC,*,ERR=881) XL,YL,NCL
         else if (nkol == 4) then
            READ (REC,*,ERR=881) XL,YL,ZL,NCL
         endif

         XLR = XL

   881   IF (XL .EQ. 999.999d0 .OR. XLR == 999.999d0) THEN
            XL  = dmiss
            YL  = dmiss
            ZL  = dmiss
            NCL = 0
         ENDIF
         IF (NTOT == 0) THEN  
            NTOT  = NTOT + 1
            MXLAN = NTOT
            XLAN(NTOT)    = XL
            YLAN(NTOT)    = YL
            ZLAN(NTOT)    = ZL
            NCLAN(NTOT)   = NCL
         ELSE IF (XL .ne. XLAN(NTOT) .or. YL .ne. YLAN(NTOT) )  THEN 
            NTOT  = NTOT + 1
            MXLAN = NTOT
            XLAN(NTOT)    = XL
            YLAN(NTOT)    = YL
            ZLAN(NTOT)    = ZL
            NCLAN(NTOT)   = NCL
         ENDIF 
         IF (MOD(I,1000) .EQ. 0) THEN
            CALL READYY(' ',MIN( 1d0,dble(I)/MAXLAN ) )
         ENDIF
   20 CONTINUE
      NTOT  = NTOT + 1
      MXLAN = NTOT
      XLAN(NTOT)  = dmiss
      YLAN(NTOT)  = dmiss
      ZLAN(NTOT)  = dmiss

      GOTO 10

  777 CONTINUE
      MXLAN = NTOT
      CALL READYY(' ', 1d0)
      CALL READYY(' ',-1d0)
      call doclose (MLAN)
     
      if (present(antot)) then
         antot = NTOT
      endif

      return
      
      n = 1                                    ! remove double points in lineseg oriented files  
      xpl(n) = xlan(1) ; ypl(n) = ylan(1)
      do k  = 2,mxlan-1
         kd = k - 1; ku = k + 1 
         if (xlan(k) == dmiss .and. xlan(kd) == xlan(ku) .and. ylan(kd) == ylan(ku) ) then 
             
         else
            n = n + 1
            xpl(n) = xlan(k) ; ypl(n) = ylan(k)
         endif
      enddo   
      n = n + 1  
      xpl(n) = xlan(mxlan) ; ypl(n) = ylan(mxlan)
            
      npl = n
      
      RETURN

  666 CALL QNREADERROR('SEARCHING NROWS,NCOLS, BUT GETTING', REC, MLAN)
      MXLAN = NTOT
      CALL READYY(' ', 1d0)
      CALL READYY(' ',-1d0)
      call doclose (MLAN)
      RETURN

  888 CALL QNREADERROR('SEARCHING COORDINATES, BUT GETTING', REC, MLAN)
      MXLAN = NTOT
      CALL READYY(' ', 1d0)
      CALL READYY(' ',-1d0)
      call doclose (MLAN)
      RETURN

  887 CALL QNREADERROR('EXPECTING 4 CHAR, BUT GETTING', MATR, MLAN)
      MXLAN = NTOT
      CALL READYY(' ', 1d0)
      CALL READYY(' ',-1d0)
      call doclose (MLAN)
      RETURN

  999 CALL QNEOFERROR(MLAN)
      MXLAN = NTOT
      CALL READYY(' ', 1d0)
      CALL READYY(' ',-1d0)
      call doclose (MLAN)
      RETURN

      END


!> Read land boundary from world vector shoreline files (NetCDF format)
!! Global Self-consistent Hierarchical High-resolution Shorelines (GSHHS)
!! http://opendap.deltares.nl/thredds/catalog/opendap/noaa/gshhs/catalog.html
!! Directly stored in m_landboundary module variables.
subroutine read_land_boundary_netcdf(filename)    
    use M_landboundary
    USE M_MISSING
    use netcdf
    
    implicit none

    character(len=*), intent(in)  :: fileName

    double precision, dimension(:), allocatable :: x_lan, y_lan
    integer, dimension(:), allocatable          :: k
    integer :: i

    integer :: status, istart, istop
    integer :: id_nc, id_lon, id_lat,id_npts, id_sep, id_k
    integer :: npts, nsep
    integer :: max_vertex, n_vertex
    logical :: succes

!    write(msgtxt,'("Reading file: ",a)') trim(get_basename( get_filename(filename) ) )

    ! Read the land boundary geometry from netcdf-file.

    succes = .false.

    status = nf90_open(trim(filename), NF90_NOWRITE, ncid=id_nc)
!    if (status .ne. nf90_noerr) msgtxt = nf90_strerror(status)

    status = nf90_inq_dimid(id_nc, "npoints", id_npts)
    status = nf90_inquire_dimension(id_nc, id_npts, len=npts)
    status = nf90_inq_dimid(id_nc, "segment_separators", id_sep)
    status = nf90_inquire_dimension(id_nc, id_sep, len=nsep)

    !call setTotalSteps(pbar, 5+nsep)
    
    allocate(k(nsep))
    status = nf90_inq_varid(id_nc, "k", id_k)
    status = nf90_get_var(id_nc, id_k, k, count=(/ nsep /))
    status = nf90_inq_varid(id_nc, "lon", id_lon)
!    if (status .ne. nf90_noerr) msgtxt = nf90_strerror(status)

    status = nf90_inq_varid(id_nc, "lat", id_lat)
!    if (status .ne. nf90_noerr) msgtxt = nf90_strerror(status)

    ! Determine largest landboundary segment
    max_vertex = 0
    do i = 1, nsep-1
        max_vertex = max(max_vertex, k(i+1)-k(i)-1 )
    enddo
    !allocate(x_lan(npts))
    !allocate(y_lan(npts))
    call increaselan(npts)
    !call setProgress(pbar, 5)
    
    status = nf90_get_var(id_nc, id_lon, xlan, count=(/ npts /))
    status = nf90_get_var(id_nc, id_lat, ylan, count=(/ npts /))

    !call multiFeatureSetCapacity(polylines, nsep-1+10)
    do i = 1, nsep
       ! if (mod(i,100)==0) call setProgress(pbar, 5+i)
        !istart   = k(i  ) + 1
        !istop    = k(i+1) - 1
        !n_vertex = istop -istart + 1

        ! Replace NetCDF NaNs by our dmiss vals at segment separator positions, that's all.
        xlan(k(i)) = dmiss
        ylan(k(i)) = dmiss

        !
        !polyline  = newpolyline()
        !call addPointArray( polyline, n_vertex, x_lan(istart:istop), y_lan(istart:istop))
        !        
        !call multifeatureaddfeature(polylines, polyline)
    enddo        
    MXLAN = npts
    status = nf90_close(id_nc)

    !call setProgress(pbar, nsep)
    !call free(pbar)
    
    deallocate(k)
    !deallocate(x_lan)
    !deallocate(y_lan)

    if (status==0) then
        succes = .true.
    endif
end subroutine read_land_boundary_netcdf


      subroutine reapol(mpol, jadoorladen)
      implicit none
      integer :: mpol
      integer, intent(in)           :: jadoorladen !< Append to existing polygons (intended to read multiple crs files)
      integer                       :: ipli
      ipli = 0
      call reapol_nampli(mpol, jadoorladen, 0, ipli)    
      end subroutine reapol
    

      !> Read polygon file (or cross section/pli file) and store in global polygon.
      !! File should contain Tekal block(s) with two or three columns.
      !! The block names may be used for cross sections.
      !! A dmiss line starts a new polyline without a name. Multiple dmiss lines are skipped.
      SUBROUTINE REAPOL_NAMPLI(MPOL, jadoorladen, janampl, ipli)
      USE M_POLYGON
      use network_data, only: netstat, NETSTAT_CELLS_DIRTY
      USE M_MISSING
      use m_alloc
      use unstruc_messages
      use unstruc_files
      use m_flowparameters, only: ifixedweirscheme
 
      implicit none
      integer :: mpol
      integer, intent(in)           :: jadoorladen !< Append to existing polygons (intended to read multiple crs files)
      integer, intent(in)           :: janampl     !< Store the pli-name as crosssection name
      integer, intent(inout)        :: ipli
     
      integer :: i
      integer :: nkol
      integer :: nrow
      integer :: nmiss
      integer :: ierr
      double precision :: xx, yy, zz, dz1, dz2
      double precision :: zcrest,sillup, silldown, crestl,taludl, taludr, veg
      character(len=1) :: weirtype
      
      CHARACTER(len=5) :: CHARMC
      character(len=64) :: MATR
      character(len=256) :: REC
      
      if (jadoorladen /= 1) then
        if (.not. allocated(XPL)) allocate(XPL(1), YPL(1), ZPL(1))
        XPL = XYMIS
        YPL = XYMIS
        ZPL = XYMIS
        NPL = 0
        call realloc(nampli,20, keepExisting = .false., fill = ' ')
      end if


      CALL READYY('READING POLYGON / land boundary / CRS-FILE',0d0)
   10 CONTINUE
      READ(MPOL,'(A)',END=999,ERR=888) MATR
      IF (MATR(1:1) .EQ. '*' .or. len_trim(matr) == 0) GOTO 10
      READ(MPOL,'(A)',END = 999) REC
      READ(REC,*,iostat=ierr) NROW, NKOL
      if (ierr /= 0) goto 888
      jaKol45 = 0 
      if (nkol < 2) then
        CALL QNERROR('File should contain at least 2 or 3 columns, but got:', ' ', ' ') ! nkol)
        goto 999
      else if (nkol > 3 .and. nkol .le. 8) then 
         jaKol45 = 1
      else if (nkol .ge. 9) then 
         jaKol45 = 2
      end if
      CALL INCREASEPOL(NPL + NROW + 1, 1) ! previous pols (if any) + 1 dmiss + new polyline

11    ipli = ipli + 1         ! Start reading a new polyline
     
      if (janampl>0) then
         if (len_trim(matr) > 0) then 
            if (ipli>size(nampli)) then
               call realloc(nampli, int(1.2*ipli) + 1, keepexisting = .True.)
            endif
            nampli(ipli) = matr  ! Temporarily store cross section name with polyline
         endif
      endif
     
      if (npl > 0 .and. nrow > 0) then
          ! Separator element for subsequent named polylines
        npl = npl + 1
        xpl(npl) = dmiss
        ypl(npl) = dmiss
        zpl(npl) = dmiss
      end if

      I = 0
 row: DO
        I = I+1
        if (I > NROW) exit

            nmiss = 0
            do ! Read a single line, or multiple until a NON-dmiss line is found
                READ(MPOL,'(A)',END = 999) REC
                ZZ = DMISS ; dz1 = dmiss; dz2 = dmiss
                if (nkol == 10) then 
                    READ(REC,*,iostat=ierr) XX,YY,zcrest, sillup, silldown, crestl, taludl, taludr, veg, weirtype  ! read weir data from Baseline format plus weirtype 
                    if (ierr /= 0) goto 777
                    ZZ = zcrest ! dummy value for zz to guarantee that ZPL will be filled
                else if (nkol == 9) then 
                    READ(REC,*,iostat=ierr) XX,YY,zcrest, sillup, silldown, crestl, taludl, taludr, veg  ! read weir data from Baseline format 
                    if (ierr /= 0) goto 777
                    ZZ = zcrest ! dummy value for zz to guarantee that ZPL will be filled
                else if (nkol == 5) then 
                    READ(REC,*,iostat=ierr) XX,YY,ZZ,dz1,dz2
                    if (ierr /= 0) goto 777
                else if (nkol == 4) then 
                    READ(REC,*,iostat=ierr) XX,YY,ZZ,dz1 
                    if (ierr /= 0) goto 777
                else if (nkol == 3) then
                    READ(REC,*,iostat=ierr) XX,YY,ZZ
                    if (ierr /= 0) goto 777
                else
                    READ(REC,*,iostat=ierr) XX,YY
                    if (ierr /= 0) goto 777
                end if
                IF (XX .NE. dmiss .AND. XX .NE. 999.999d0) exit
                nmiss = nmiss + 1
                I = I+1
                if (I > NROW) exit row ! Last row was also dmiss, now exit outer 'row' loop.
            end do
            if (nmiss > 0) then
                backspace(MPOL) ! Last one was NON-dmiss, preceded by one or more dmiss lines
                NROW = NROW-I+1 ! so reread this in the next polyline loop
                MATR = ' '      ! Fake new Tekal block by decrementing NROW and dummy name in MATR
                goto 11
            else
                NPL = NPL + 1
                XPL(NPL) = XX
                YPL(NPL) = YY
                ZPL(NPL) = ZZ
                if (jakol45 == 1) then
                   IF (.NOT. ALLOCATED(DZL) ) THEN 
                      ALLOCATE ( DZL(MAXPOL), DZR(MAXPOL) ) ; DZL = DMISS ; DZR = DMISS
                   ENDIF     
                   DZL(NPL) = DZ1
                   DZR(NPL) = DZ2  
                else if (jakol45 == 2) then
                   IF (.NOT. ALLOCATED(IWEIRT) ) THEN 
                      if( allocated( DZL ) ) deallocate( DZL )
                      if( allocated( DZR ) ) deallocate( DZR )
                      ALLOCATE ( DZL(MAXPOL), DZR(MAXPOL), DCREST(MAXPOL), DTL(MAXPOL), DTR(MAXPOL), DVEG(MAXPOL), IWEIRT(MAXPOL) )
                      IWEIRT = dmiss
                   ENDIF     
                   DZL(NPL) = sillup  
                   DZR(NPL) = silldown  
                   DCREST(NPL) = crestl  
                   DTL(NPL) = taludl
                   DTR(NPL) = taludr  
                   DVEG(NPL) = veg 
                   IWEIRT(NPL) = -999  ! if no weirtype has been specified
                   if (nkol .eq. 10) then
                      if (weirtype .eq. 't' .or. weirtype .eq. 'T') then
                          IWEIRT(NPL) = 1  
                      elseif (weirtype .eq. 'v' .or. weirtype .eq. 'V')  then
                          IWEIRT(NPL) = 2  
                      endif    
                   else if (nkol == 9) then
                      if (ifixedweirscheme == 8) then
                         IWEIRT(NPL) = 1
                      elseif (ifixedweirscheme == 9) then
                         IWEIRT(NPL) = 2
                      endif
                   endif    
                endif
            
            end if
            IF (MOD(NPL,100) .EQ. 0) THEN
               CALL READYY(' ',MIN( 1d0,dble(I)/MAXPOL ) )
            ENDIF

      end do row
      GOTO 10


  999 CONTINUE
      ! If last polyline in file had only dmisses, remove last separator element in xpl.
      if (npl > 0) then
         if (xpl(npl) == dmiss) then
            npl = npl-1
         end if
      end if
      CALL READYY(' ',-1d0)
      call doclose (MPOL)

      RETURN

  888 CALL QNREADERROR('SEARCHING NROWS,NCOLS, BUT GETTING', REC, MPOL)
      CALL READYY(' ',-1d0)
      call doclose (MPOL)
      RETURN

  777 CALL QNREADERROR('READING COORDINATES BUT GETTING',REC,MPOL)
      CALL READYY(' ',-1d0)
      call doclose (MPOL)
      RETURN

      END SUBROUTINE REAPOL_NAMPLI

      SUBROUTINE WRIPOL(MPOL)
      USE M_POLYGON
      use m_missing
      implicit none
      integer :: mpol, numnampli
      integer :: NCLAN(0)
      double precision :: ZSH(0)

      if (NPL<=0) return
      numnampli = size(nampli)
      if (zpl(1) == dmiss) then ! No third column for z-values
        CALL WRILDB(MPOL, XPL, YPL, NPL, NCLAN, 0, ZSH, 0, nampli, 64, numnampli)
      else
        CALL WRILDB(MPOL, XPL, YPL, NPL, NCLAN, 0, ZPL, NPL, nampli, 64, numnampli)
      end if
      END subroutine wripol

      SUBROUTINE WRILAN(MPOL)
      USE M_LANDBOUNDARY
      implicit none
      integer :: mpol
      integer :: mx
      double precision, ALLOCATABLE :: XL(:), YL(:)
      double precision :: ZL(0)    ! no z-values
      character(len=1) :: names(1) ! no names

      MX = MAXLAN
      ALLOCATE ( XL(MX), YL(MX))
      XL (1:MXLAN)  = XLAN(1:MXLAN)
      YL (1:MXLAN)  = YLAN(1:MXLAN)
      names = ' '

      CALL WRILDB(MPOL, XL, YL, MXLAN, nclan, MXLAN, ZL, 0, names, 1, 1)
      DEALLOCATE (XL, YL)

      END

      !> Writes active cross sections to a polyline file.
      subroutine wricrs(mpol)
      use m_crosssections
      use m_polygon
      use m_missing
      implicit none
      integer :: mpol,i

      call savepol()
      call copycrosssectionstopol()
!      npl = 0 ! Write traced polygons instead of original plis
!      do i=1,ncrs
!        xpl(npl+1:npl+crs(i)%len+1)=crs(i)%xk(1:crs(i)%len+1)
!        ypl(npl+1:npl+crs(i)%len+1)=crs(i)%yk(1:crs(i)%len+1)
!        npl = npl+crs(i)%len+2
!        xpl(npl) = dmiss
!        ypl(npl) = dmiss
!      end do
!      if (ncrs>0) npl = npl - 1 ! remove last separator
      call wripol(mpol)
      call restorepol()

      end subroutine wricrs


      !> Writes a polygon/land boundary/cross section file.
      !! The polyline(s) are written as a sequence of Tekal blocks.
      !! The name for each Tekal block can be specified, or is auto-generated
      !! as 'L00x' otherwise.
      SUBROUTINE WRILDB(MPOL, XSH, YSH, NSH, NCLAN, nnclan, ZSH, nzsh, names, namlen, nnam)
      USE M_MISSING
      use m_polygon, only : zpl, DZL, DZR, jakol45
      implicit none
      integer,       intent(inout) :: mpol !< Open file pointer where to write to.
      double precision, intent(in) :: XSH(NSH), YSH(NSH) !< Coordinates, polylines can be separated by dmiss value.
      integer,          intent(in) :: nsh  !< Number of points in polyline.
      integer,          intent(in) :: namlen   !< string length of names.
      character(len=namlen), intent(in) :: names(nnam) !< Names of all polylines, header of each Tekal Block.
      integer,          intent(in) :: nnam     !< Number of polyline names.
      integer,          intent(in) :: NCLAN(*) !< Third integer value for each point in XSH, optional: use nnclan=0 to ignore
      integer,          intent(in) :: nnclan   !< Size of NCLAN, use 0 to ignore.
      double precision, intent(in) :: ZSH(*)   !< Third double  value for each point in XSH, optional: use nzsh=0 to ignore
      integer,          intent(in) :: nzsh     !< Size of ZSH, use 0 to ignore.

      integer :: L

      integer :: i, ipli, npli, mbna, ncol
      integer, allocatable :: istart(:), iend(:)
      character(len=max(namlen,10)) :: name
      character(len=1)  :: cdigits
      character(len=40) :: rec
      logical :: jaNCLAN, jaZSH
      
      ! Only include third column when size is equal to XSH array (or larger).
      jaNCLAN = nNCLAN >= NSH
      jaZSH   = nZSH   >= NSH

      CALL READYY('Writing Polygon / Land Boundary FILE',0d0)

      
      MBNA = 0
      IF (MBNA > 0) call newfil(mbna, 'bna.bna')

      if (NSH <= 0) goto 11
      allocate(istart(nsh), iend(nsh))

      ! First, find starts and ends of all polylines (separated by dmiss line(s))
      ! such that each can be written as a named Tekal block later.
      ipli = 0
      i    = 0
 pli: do
        i = i+1
        if (i > nsh) exit pli
        if (xsh(i) == dmiss) cycle pli

        ! Start of a new polyline found
        ipli= ipli + 1
        istart(ipli) = i
   pts: do
            i = i+1
            if (i > nsh) exit pts
            if (xsh(i) == dmiss) exit pts
        end do pts
        iend(ipli) = i-1
      end do pli

      npli = ipli


      ! Start writing the set of polyline(s).
      KMOD = MAX(1,NSH/100)

      write(cdigits, '(i1)') int(floor(log10(dble(npli))+1)) ! nr of digits in npli

      if (jaNCLAN .or. jaZSH) then
          ncol = 3
          if (jakol45 == 1) then
              ncol = 5
          endif    
      else
          ncol = 2
      end if
      do ipli=1,npli
        if (ipli <= nnam) then
            name = names(ipli)
        else
            name = ' '
        end if
        ! Generate 'L00x' name if empty
        if (len_trim(name) <= 0) then
            write(name, '(A1,I'//cdigits//'.'//cdigits//')') 'L', ipli
            
            IF (MBNA > 0) THEN 
               rec = ' '
               write(rec , '(A1,I'//cdigits//'.'//cdigits//')') '"', ipli
               L = len_trim(rec)
               write(rec(L+1:) , '(A)') '","",'
               L = len_trim(rec)
               write(rec(L+1:) , '(i10)') -(iend(ipli)-istart(ipli)+1)
               write(mbna,'(a)') rec
            ENDIF
     
        endif

        WRITE(MPOL,'(A)') trim(name)
        WRITE(MPOL,'(I6,I6)') iend(ipli)-istart(ipli)+1, ncol
        
!        rec = '"324","",-2
        DO I = istart(ipli),iend(ipli)
            IF (jaNCLAN) THEN
               WRITE(MPOL,'(2F15.6,I5)') XSH(I), YSH(I), NCLAN(I)
            elseif (jaZSH) then
               if (jakol45 == 1) then  
                  WRITE(MPOL,'(5F15.6)') XSH(I), YSH(I), zpl(i), DZL(i), DZR(i) 
               else
               WRITE(MPOL,'(3F15.6)') XSH(I), YSH(I), ZSH(I)
               endif   
               IF (MBNA > 0) WRITE(Mbna,'(2F15.6)') XSH(I), YSH(I)
            else    
                WRITE(MPOL,'(2F15.6)') XSH(I), YSH(I)
            ENDIF

            IF (MOD(I,KMOD) .EQ. 0) THEN
                CALL READYY(' ',MIN( 1d0,dble(I)/MAX(1,NSH) ) )
            ENDIF
        END DO ! pts of one polyline
      end do ! all polylines

      deallocate(istart, iend)
   11 CALL READYY(' ',-1d0)
      call doclose (MPOL)

      IF (MBNA > 0) call doclose (MBNA)

      
      RETURN
      END



      SUBROUTINE PUTAR  (XR,X,MMAX)
      implicit none
      integer :: i
      integer :: mmax
      double precision :: x
      double precision :: xr
!     DE EERSTE IN DE TWEEDE
      DIMENSION XR(MMAX) , X(MMAX)
      DO 10 I = 1,MMAX
         X(I) = XR(I)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE DPUTAR (XR,X,MMAX)
      implicit none
      integer :: i
      integer :: mmax
!     DE EERSTE IN DE TWEEDE
      DOUBLE PRECISION XR(MMAX) , X(MMAX)
      DO 10 I = 1,MMAX
         X(I) = XR(I)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE IPUTAR  (IXR,IX,MMAX)
      implicit none
      integer :: i
      integer :: ix
      integer :: ixr
      integer :: mmax
!     DE EERSTE IN DE TWEEDE
      DIMENSION IXR(MMAX) , IX(MMAX)
      DO 10 I = 1,MMAX
         IX(I) = IXR(I)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE PUTARR (XR,X,MMAX,NMAX)
      implicit none
      integer :: i
      integer :: j
      integer :: mmax
      integer :: nmax
      double precision :: x
      double precision :: xr
!     DE EERSTE IN DE TWEEDE
      DIMENSION XR(MMAX,NMAX), X(MMAX,NMAX)
      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            X(I,J) = XR(I,J)
   10 CONTINUE
      RETURN
      END


      SUBROUTINE IPUTARR(XR,X,MMAX,NMAX)
      implicit none
      integer :: i
      integer :: j
      integer :: mmax
      integer :: nmax
!     DE EERSTE IN DE TWEEDE
      INTEGER XR(MMAX,NMAX), X(MMAX,NMAX)
      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            X(I,J) = XR(I,J)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE MINMAX(      X, MXLAN,   XMIN,   XMAX, MAXLAN)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: maxlan
      integer :: mxlan
      double precision :: xmax
      double precision :: xmin
      double precision :: xx
!     BEPAAL MINIMUM EN MAXIMUM VAN EEN EENDIMENSIONALE ARRAY
      double precision :: X(MAXLAN)

      IF (MXLAN .EQ. 0) THEN
         XMIN = 0
         XMAX = 0
         RETURN
      ENDIF

      XMIN =  10D20
      XMAX = -10D20
      DO 10 I = 1,MXLAN
         XX   = X(I)
         IF (XX .NE. dmiss) THEN
            XMIN = MIN(XMIN,XX)
            XMAX = MAX(XMAX,XX)
         ENDIF
   10 CONTINUE
      IF (XMIN .EQ. 10D20) XMIN = 0
      IF (XMAX .EQ.-10D20) XMAX = 0
      RETURN
      END

      SUBROUTINE DMINMAX(      X, MXLAN,   XMIN,   XMAX, MAXLAN)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: maxlan
      integer :: mxlan
      double precision :: xmax
      double precision :: xmin
      double precision :: xx
!     BEPAAL MINIMUM EN MAXIMUM VAN EEN EENDIMENSIONALE ARRAY
      DOUBLE PRECISION, intent(inout)  ::  X(MAXLAN)

      IF (MXLAN .EQ. 0) THEN
         XMIN = 0
         XMAX = 0
         RETURN
      ENDIF

      XMIN =  10D20
      XMAX = -10D20
      DO 10 I = 1,MXLAN
         XX   = X(I)
         IF (XX .NE. dmiss) THEN
            XMIN = MIN(XMIN,XX)
            XMAX = MAX(XMAX,XX)
         ENDIF
   10 CONTINUE
      IF (XMIN .EQ. 10D20) XMIN = 0
      IF (XMAX .EQ.-10D20) XMAX = 0
      RETURN
      END

      SUBROUTINE DMINMX2(      X,   XMIN,   XMAX,     MC,NC,MMAX,NMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: j
      integer :: mc
      integer :: mmax
      integer :: nc
      integer :: nmax
      double precision :: xmax
      double precision :: xmin
      double precision :: xx
!     BEPAAL MINIMUM EN MAXIMUM VAN EEN TWEEDIMENSIONALE ARRAY
      DOUBLE PRECISION :: X(MMAX,NMAX)
      IF (MC .EQ. 0 .OR. NC .EQ. 0) THEN
         XMIN = 0
         XMAX = 0
         RETURN
      ENDIF
      XMIN =  10D20
      XMAX = -10D20
      DO 10 I = 1,MC
         DO 10 J = 1,NC
            XX   = X(I,J)
            IF (XX .NE. DXYMIS) THEN
               XMIN = MIN(XX,XMIN)
               XMAX = MAX(XX,XMAX)
            ENDIF
   10 CONTINUE
      IF (XMIN .EQ. 10D20) XMIN = 0
      IF (XMAX .EQ.-10D20) XMAX = 0
      RETURN
      END



      SUBROUTINE MINMXI(IS,MMAX,MINI,MAXI)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: maxi
      integer :: mini
      integer :: mmax
      INTEGER IS(MMAX)

      MAXI = -999999
      MINI =  999999
      DO 10 I = 1,MMAX
         IF (IS(I) .NE. dmiss) THEN
            MAXI = MAX(MAXI,IS(I))
            MINI = MIN(MINI,IS(I))
         ENDIF
   10 CONTINUE
      RETURN
      END

      SUBROUTINE QNREADERROR(W1,W2,MINP)
      use unstruc_files
      implicit none
      integer :: minp
      CHARACTER W1*(*),W2*(*)

      CALL QNERROR(W1,W2,' IN FILE '//FILENAMES(MINP))
      END

      SUBROUTINE QNEOFERROR(MINP)
      USE unstruc_files
      implicit none
      integer :: minp
      CALL QNERROR('UNEXPECTED END OF FILE IN ',FILENAMES(MINP),' ')
      END



      SUBROUTINE ININUMBERS()
      USE M_MISSING
      implicit none
      COMMON /NUMBERS/ PI, DG2RD, RD2DG, RA

      double precision :: pi, dg2rd, rd2dg, ra
      RA    = 6370000d0
!     RA    = dble(6378000.0)   DIT IN MEESTE ANDERE LITERATUUR
      PI    = acos(-1d0)
      DG2RD = PI/180d0
      RD2DG = 180d0/PI
      RETURN
      END

      SUBROUTINE ZEROLAN( KEY)
      use m_landboundary
      use m_polygon
      use m_missing
      use geometry_module, only: dbpinpol
      implicit none
      integer :: i
      integer :: inhul
      integer :: istart
      integer :: ja
      integer :: k
      integer :: key
      integer :: mxol
      integer :: ntot
      KEY = 3
      IF (NPL .LE. 2) THEN
         CALL CONFRM('NO POLYON, SO DELETE all BOUNDARY POINTS ? ',JA)
         IF (JA .EQ. 0) THEN
            KEY = 0
            RETURN
         ENDIF
!        CALL SAVESAM()
         DO 5 I = 1,MXLAN
            XLAN(I)  = dmiss
            YLAN(I)  = dmiss
            ZLAN(I)  = dmiss
            NCLAN(I) = 0
    5    CONTINUE
         MXLAN = 0
         RETURN
      ENDIF
!     CALL SAVESAM()
      INHUL = -1
      DO 10 I = 1,MXLAN
            CALL DBPINPOL( XLAN(I), YLAN(I), INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
            IF (INHUL .EQ. 1) THEN
               XLAN(I)  = dmiss
               YLAN(I)  = dmiss
               ZLAN(I)  = dmiss
               NCLAN(I) = 0
            ENDIF
   10 CONTINUE

      K = 0
      MXOL   = MXLAN
      ISTART = 0
      NTOT   = 0
      DO 20 I = 1,MXLAN
         IF (XLAN(I) .NE. dmiss) THEN
            ISTART   = 1
            K        = K + 1
            XLAN(K)  = XLAN(I)
            YLAN(K)  = YLAN(I)
            ZLAN(K)  = ZLAN(I)
            NCLAN(K) = NCLAN(I)
         ELSE IF (ISTART .EQ. 1) THEN
            K       = K + 1
            XLAN(K)  = dmiss
            YLAN(K)  = dmiss
            ZLAN(K)  = dmiss
            NCLAN(K) = 0
            ISTART   = 0
         ENDIF
   20 CONTINUE
      MXLAN = K

      DO 30 I = MXLAN+1,MXOL
         XLAN(I)  = dmiss
         YLAN(I)  = dmiss
         ZLAN(I)  = dmiss
         NCLAN(I) = 0
   30 CONTINUE

      RETURN
      END

      subroutine checkdislin()
      use m_polygon
      use m_sferic
      use geometry_module, only: dlinedis
      use m_missing, only: dmiss

      implicit none
      integer :: ja
      integer :: jashow
      integer :: jmouse
      double precision :: xa
      double precision :: xlc
      double precision :: ya
      double precision :: ylc
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW
      double precision :: dis, xn, yn

      if (npl >= 2) then
         call DLINEDIS(xlc, ylc ,Xpl(1),ypl(1), xpl(2), ypl(2) ,JA,DIS,XN,YN,jsferic, jasfer3D, dmiss)
      endif

      call DLINEDIS(1d0,0d0,0d0,0d0,1d0,1d0, JA, DIS,XN,YN,jsferic, jasfer3D, dmiss)

      dis = 0.5d0*sqrt(2d0)

      call DLINEDIS(1d0,0.5d0,0d0,0d0,1d0,1d0, JA, DIS,XN,YN,jsferic, jasfer3D, dmiss)

      dis = 0.25d0*sqrt(2d0)

      jsferic = 1
      call DLINEDIS(4d0,60d0,3d0,60d0,4d0,61d0, JA, DIS,XN,YN,jsferic, jasfer3D, dmiss)

      call DLINEDIS(4d0,60.8d0,3d0,60d0,4d0,61d0, JA, DIS,XN,YN,jsferic, jasfer3D, dmiss)
      
      call rcirc( xn, yn )
      end subroutine checkdislin

      subroutine DISPFORM (value,fmt)
      implicit none
      integer :: n1
      integer :: n2
      double precision :: value
      character fmt*(*)

      fmt='(f9.3)'

      if (value .eq. 0.0) then
         fmt='(f9.5)'
         return
      endif

      n1 = int(log10(abs(value)))

      if (n1 .le. 6 .and. n1 .gt. 0) then
         n2 = min(9,n1 + 3)
         write (fmt(5:5),'(i1)') 9 - n2
      else if (n1 .ge. -5 .and. n1 .lt. 0) then
         write (fmt(5:5),'(i1)') 6
      else if ( n1 .eq. 0) then
         write (fmt(5:5),'(i1)') 6
      else
         fmt ='(e9.3)'
      endif

      return
      end

      subroutine DISPFORMscale0(value,fmt)
      implicit none
      integer :: n1
      integer :: n2
      integer :: ndec
      double precision :: scalesize
      double precision :: value
      double precision :: xsc
      double precision :: ysc
      character fmt*(*)
      COMMON /SCALEPOS/ XSC,YSC,SCALESIZE,NDEC

      fmt='(f9.3)'

      if (value .eq. 0.0) then
         fmt='(f3.1)'
         return
      endif

      n1 = int(log10(abs(value)))

      if (n1 .le. 6 .and. n1 .gt. 0) then
         n2 = min(9,n1 + 3)
         write (fmt(5:5),'(i1)') 9 - n2
      else if (n1 .ge. -5 .and. n1 .lt. 0) then
         write (fmt(5:5),'(i1)') 6
      else if ( n1 .eq. 0) then
         write (fmt(5:5),'(i1)') 6
      else
         fmt ='(e9.3)'
      endif
      IF (NDEC .GT. 0) write (fmt(5:5),'(i1)') NDEC
      return
      end

      SUBROUTINE STOPINT()
      use unstruc_files
      use unstruc_netcdf, only: unc_closeall
      use m_partitioninfo
      implicit none
      CALL ISCREENCLOSE()
      call unc_closeall()
      call close_all_files()
      
      if ( jampi.eq.1 ) then
!        finalize before exit
         call partition_finalize()
      end if
      
    
!     SPvdP: close dia-file
      if ( mdia.gt.0 .and. mdia.lt.maxnum ) then
         close(mdia)
         mdia = 0
      end if
      
      STOP
      END

      SUBROUTINE START()
      use M_dimens
      USE M_DEVICES
      use unstruc_files
      use unstruc_startup
      use unstruc_version_module, only : unstruc_basename
      use unstruc_display, only : jaGUI
      use unstruc_messages

      implicit none

      integer :: infofile
      integer :: infohardware
      integer :: infoopsystem
      integer :: ja
      integer :: jmouse
      integer :: jscreen
      integer :: key
      integer :: nlevel
      integer :: num
      integer :: numclargs
      integer :: nwhat
      COMMON /HELPNOW/   WRDKEY, NLEVEL
      COMMON /KERN3D/    INFOFILE,NAMEGRID,NAMEFIELDI,NAMEFIELDO,GRIDAT
      COMMON /MESSAGETOSCREEN/ JSCREEN
      CHARACTER NAMEGRID*80,NAMEFIELDI*80,NAMEFIELDO*80,GRIDAT*1
      CHARACTER WRDKEY*40
      CHARACTER(len=8192) :: cmd
      integer :: cmdlen

!
      WRDKEY  = 'PROGRAM PURPOSE'
      NLEVEL  = 1
      JSCREEN = 0
      INFOFILE = 0
      
      CALL INIDIA(unstruc_basename)

      CALL FIRSTLIN(MDIA)
      CALL FIRSTLIN(6)
      
      CALL get_command(cmd, cmdlen)
      write (msgbuf, '(a,a)') 'Command: ', cmd(1:cmdlen); call msg_flush()

      if ( jaGUI.ne.1 ) return

!     initialisatiefiles
      CALL initProgram()
      
      if ( jaGUI.ne.1 ) return

! SPvdP: disabled mouse-check for mouseless buildserver      
!      JMOUSE = INFOHARDWARE(13)
!      IF (JMOUSE .EQ. 1) THEN
!         CALL QNERROR ('NO MOUSE FOUND',' ',' ')
!         NLEVEL = 2
!         WRDKEY = 'MOUSE INSTALLATION'
!         CALL HELP(WRDKEY,NLEVEL)
!         CALL STOPINT()
!      ENDIF

      WRITE(msgbuf,*) 'MAXIMUM NUMBER OF LINKS         : ', LMAX      ; call msg_flush()
      WRITE(msgbuf,*) 'MAXIMUM NUMBER OF NODES         : ', KMAX      ; call msg_flush()
      WRITE(msgbuf,*) 'RESOLUTION GRAPHICS SCREEN      : ', NPX,  NPY ; call msg_flush()
      WRITE(msgbuf,*) 'RESOLUTION TEXT     SCREEN      : ', IWS,  IHS ; call msg_flush()
      WRITE(msgbuf,*) 'NUMBER OF COLOURS AVAILABLE     : ', NCOLR     ; call msg_flush()

   15 CONTINUE
      NUMCLARGS = INFOOPSYSTEM(2)
      IF (NUMCLARGS .GT. 0 .OR. INFOFILE .EQ. 1) RETURN
      KEY  = 0
      JA   = 2

      CALL MENUH(JA,NUM,NWHAT)
      CALL BOTLIN(JA,0,KEY)

      IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         CALL FKEYS(KEY)
         GOTO 15
      ENDIF

      RETURN
      END

  
      SUBROUTINE REACLASSES(MINP,CLASS,NUMQ,NUMCLASS)
      USE M_MISSING
      implicit none
      integer :: ja
      integer :: k
      integer :: l
      integer :: minp
      integer :: numc
      integer :: numclass
      integer :: numq
      CHARACTER REC*132
      double precision :: CLASS(NUMQ,NUMCLASS)
      LOGICAL EMPTY


      CALL MISARR(CLASS,NUMQ,NUMCLASS)
      CALL ZOEKAL(MINp,REC,'TEKAL BLOCK INDICATORS',JA)
      if (ja .ne. 1) return

      L = 1
   10 CONTINUE
         READ(MINP,'(A)',END = 999) REC
         IF (REC(1:1) .NE. ' ' .OR. EMPTY(REC) ) THEN
            NUMC = L - 1
            RETURN
         ENDIF
         READ(REC,*,ERR = 888) (CLASS(K,L),K=1,NUMQ)
         L = L + 1
      GOTO 10

  888 CALL READERROR('READING 5 REALS BUT GETTING',REC,MINP)

  999 CALL EOFERROR(MINP)
      END

      SUBROUTINE READARCINFOHEADER(MINP,MMAX,NMAX,X0,Y0,DX,DY,RMIS)
      implicit none
      double precision :: dx, dy
      integer :: jacornerx
      integer :: jacornery
      integer :: minp
      integer :: mmax
      integer :: nmax
      double precision :: rmis
      double precision :: x0
      double precision :: y0
      double precision :: DumX, DumY
      CHARACTER REC*132
      
      DumY = -1d10
      
   10 CONTINUE
      read(minp,*, end=100) rec, mmax
      call ilowercase(rec)
      IF (INDEX(REC,'ncol') .LT. 1) goto 101   ! wrong format
      read(minp,*, end=100,err=102) rec, nmax
      read(minp,*, end=100,err=103) rec, x0
      call ilowercase(rec)
      JACORNERX = 0
      IF (INDEX(REC,'cor') .GE. 1) JACORNERX = 1
      read(minp,*, end=100,err=104) rec, y0
      call ilowercase(rec)
      JACORNERY= 0
      IF (INDEX(REC,'cor') .GE. 1) JACORNERY = 1
      read(minp,'(A)', end=100) rec
      READ(REC(10:),*,ERR = 105) DX
      DY = DX
      READ(REC(10:),*,END = 107) DumX, DumY
      if (DumY>0) DY = DumY

  107 continue
      
      !READ(MINP,'(A)',END = 100) REC
      !READ(REC(13:),*,ERR = 106) RMIS
      read(minp,*,end=100,err=106) rec,rmis
      IF (JACORNERX .EQ. 1) X0 = X0 + DX/2
      IF (JACORNERy .EQ. 1) Y0 = Y0 + DX/2
      RETURN
  100 CONTINUE
      CALL EOFERROR(MINP)

  101 CALL READERROR('LOOKING FOR NCOLS (ARC-INFO), BUT GETTING',REC,MINP)
  102 CALL READERROR('LOOKING FOR NROWS (ARC-INFO), BUT GETTING',REC,MINP)
  103 CALL READERROR('LOOKING FOR XLLCORNER (ARC-INFO), BUT GETTING',REC,MINP)
  104 CALL READERROR('LOOKING FOR YLLCORNER (ARCINFO), BUT GETTING',REC,MINP)
  105 CALL READERROR('LOOKING FOR CELLSIZE (ARCINFO), BUT GETTING',REC,MINP)
  106 CALL READERROR('LOOKING FOR MISSING VALUE (ARCINFO), BUT GETTING',REC,MINP)
      END

      SUBROUTINE READARCINFOBLOCK(MINP,D,MC,NC,RMIS)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: j
      integer :: mc
      integer :: minp
      integer :: nc
      double precision :: rmis
      double precision :: D(MC,NC)
      double precision, dimension(MC) :: dline
      CHARACTER TEX*16

      DO 10 J = NC,1,-1
         READ(MINP,*,ERR=101,END=100) (D(I,J),I = 1,MC)
   10 CONTINUE
      DO 20 I = 1,MC
         DO 20 J = 1,NC
            IF (D(I,J) .EQ. RMIS) D(I,J) = dmiss
   20 CONTINUE
      call doclose (MINP)
      RETURN

  100 CONTINUE
      CALL EOFERROR(MINP)
  101 CONTINUE
      WRITE(TEX,'(2I8)') I,J
      CALL READERROR('ERROR READING ARC-INFO BLOCK IN COLNR, ROWNR :',TEX,MINP)
      RETURN
      END


!>    read Arcinfo data and average it into a smaller array
      subroutine ReadLargeArcInfoBlock(MINP, Mfile, Nfile, istart, iend, jstart, jend, Marray, Narray, RMIS, istep, jstep, D)
      use m_missing
      implicit none

      integer,                                     intent(in)    :: Mfile,  Nfile  !< arcinfo dimensions
      integer,                                     intent(in)    :: istart, iend, jstart, jend  !< block to be read in file-index numbering
      integer,                                     intent(in)    :: Marray, Narray !< sample data array dimensions
      integer,                                     intent(inout) :: MINP           !< input file unit number
      double precision,                            intent(in)    :: RMIS           !< missing value
      integer,                                     intent(out)   :: istep, jstep   !< subblock sizes
      double precision, dimension(Marray, Narray), intent(inout) :: D              !< sample data array
      
      double precision, dimension(:), allocatable                :: dline

      integer                                                    :: iarray, jarray, ifile, jfile, ja3
      integer                                                    :: isub, jsub
      integer,          dimension(:), allocatable                :: num

      double precision                                           :: af, dum

      integer                                                    :: ierror
      CHARACTER TEX*16

      ierror = 1

!     compute subblock sizes
!      istep = max(Mfile/Marray,1)
!      jstep = max(Nfile/Narray,1)
      istep = max((iend-istart+1)/Marray,1)
      jstep = max((jend-jstart+1)/Narray,1)

!     allocate
      allocate(dline(Mfile))
      allocate(num(Marray))

      D = 0d0

      call readyy(' ', -1d0)
      call readyy('Reading Arcinfo file (press right mouse button to cancel)', 0d0)

!     read last lines outside block
      ifile = 1
      do jfile=Nfile,jstart+Narray*jstep,-1
         read(MINP,*,ERR=101,END=100) ! ( dum, ifile=1,Mfile )
         
!        check for right mouse button
         call halt3(ja3)
         if ( ja3.eq.3 ) then
!           fill remaining array elements with DMISS
            D = DMISS
            goto 1234
         end if
         
         af = dble(jfile-Nfile)/dble(jstart-Nfile)
         call readyy('Reading Arcinfo file (press right mouse button to cancel)', af)
         WRITE(6,'(1H+"Reading Arcinfo file: ", F7.2, "% done")') 1d2*af

      end do
      
      do jarray=Narray,1,-1
!        read jstep lines and average
         dline = RMIS
         num   = 0
         do jsub=jstep,1,-1
            jfile = jstart-1 + jsub + jstep*(jarray-1)
!            if ( jfile.gt.Nfile ) cycle
            if ( jfile.gt.jend ) cycle
            if ( jfile.lt.jstart ) cycle

            dline=-1
            read(MINP,*,ERR=101,END=100) ( dline(ifile), ifile=1,Mfile )

!           sum the sample values in a subcell
            do iarray=1,Marray
                do isub=1,istep
                   ifile = istart-1 + isub + istep*(iarray-1)
!                   if ( ifile.gt.Mfile ) cycle
                   if ( ifile.gt.iend ) cycle

                   if ( dline(ifile).ne.RMIS ) then
                      num(iarray) = num(iarray)+1
                      D(iarray,jarray) = D(iarray,jarray) + dline(ifile)
                   end if
                end do
            end do
         end do
         
         af = dble(jfile-Nfile)/dble(jstart-Nfile)
         call readyy('Reading Arcinfo file (press right mouse button to cancel)', af)
         WRITE(6,'(1H+"Reading Arcinfo file: ", F7.2, "% done")') 1d2*af

!        divide by the number of samples in a subcell
         do iarray=1,Marray
            if (num(iarray).gt.0 ) then
               D(iarray,jarray) = D(iarray,jarray) / dble(num(iarray))
            else
               D(iarray,jarray) = DMISS
            end if
         end do

!        check for right mouse button
         call halt3(ja3)
         if ( ja3.eq.3 ) then
!           fill remaining array elements with DMISS
            D(1:Marray,jarray-1:1:-1) = DMISS
            goto 1234
         end if

      end do   ! do jarray=Narray,1,-1

      call readyy(' ', -1d0)

      call doclose (MINP)

      ierror = 0

      goto 1234

  100 CONTINUE
      CALL EOFERROR(MINP)
  101 CONTINUE
      WRITE(TEX,'(2I8)') ifile, jfile
      CALL READERROR('ERROR READING ARC-INFO BLOCK IN COLNR, ROWNR :',TEX,MINP)
      
!     error handling
 1234 continue

!     deallocate
      deallocate(dline)
      deallocate(num)

      return
      end subroutine ReadLargeArcInfoBlock


      SUBROUTINE REAARC(MINP,japrompt)
      USE M_ARCINFO
      use m_polygon
      use m_missing
      use m_alloc

      implicit none

      integer :: ierr
      integer :: minp

      integer, intent(in) :: japrompt  !< prompt for step size (1) or not (0)

      integer :: istep, jstep, MCfile, NCfile
      integer :: istart, iend, jstart, jend  !< block to be read in file-index numbering

      double precision :: distep, djstep, dsqrtnumcur

      logical :: LdirectReadBlock = .false.

      integer, parameter :: MAXSAMSIZE = 1000000

      CALL READARCINFOHEADER(MINP,MCa,NCa,X0,Y0,DXa,DYa,RMIS)

      IF ( ALLOCATED(D) ) THEN
            DEALLOCATE(D)
      ENDIF
      
      if (japrompt == -1 ) LdirectReadBlock = .true.

      if ( LdirectReadBlock ) then
         ALLOCATE ( D(MCa,NCa),STAT=IERR)
         CALL AERR('D(MCa,NCa)',IERR,MCa*NCa)

         CALL READARCINFOBLOCK (MINP,D,MCa,NCa,RMIS)
      else
         istep = 1
         jstep = 1
         ierr  = 1

         MCfile = MCa
         NCfile = NCa

!         do while ( ierr.ne.0 )
         

         if ( NPL.le.0 ) then
            istart = 1
            iend   = MCa
            jstart = 1
            jend   = NCa
         else  ! use selecting polygon for dimensions of block to be read
            istart = max(1+int( (minval(xpl(1:NPL), xpl(1:NPL).ne.DMISS)-X0)/DXa ), 1)
            iend   = min(1+int( (maxval(xpl(1:NPL), xpl(1:NPL).ne.DMISS)-X0)/DXa ), MCa)
            
            jstart = max(1+int( (minval(ypl(1:NPL), ypl(1:NPL).ne.DMISS)-Y0)/DYa ), 1)
            jend   = min(1+int( (maxval(ypl(1:NPL), ypl(1:NPL).ne.DMISS)-Y0)/DYa ), NCa)
         end if

         if ( japrompt.eq.1 ) then
!           automatic istep, jstep
            dsqrtnumcur = sqrt(dble(iend-istart+1))*sqrt(dble(jend-jstart+1))
            distep = dsqrtnumcur/sqrt(dble(MAXSAMSIZE))
            distep = dble(int(distep+0.5d0))
            djstep = distep

            if ( distep.gt.1d0 ) then  ! only if necessary
               call getreal("istep = ", distep)
               call getreal("jstep = ", djstep)
            end if

            istep = max(int(distep),1)
            jstep = max(int(djstep),1)
         end if

            MCa = (iend-istart+1)/istep
            NCa = (jend-jstart+1)/jstep

            ALLOCATE ( D(MCa,NCa),STAT=IERR)
!            CALL AERR('D(MCa,NCa)',IERR,MCa*NCa)

   !        check for allocation error
            if ( IERR.ne.0 ) then
               call qnerror('Sample file too large: increase istep and/or jstep', ' ', ' ')
               MCA = 0
               NCA = 0
   !           we cannot deallocate erroneously allocated arrays directly and need to reallocate it correctly first
               allocate(D(1,1))
               deallocate(D)
               goto 1234
            end if
!         end do   ! do while ( ierr.ne.0 )

         call ReadLargeArcInfoBlock(MINP, MCfile, NCfile, istart, iend, jstart, jend, MCa, NCa, RMIS, istep, jstep, D)

!        modife arcinfo module data
!         X0 = X0 + dble(istep-1)*0.5d0*DXa
!         Y0 = Y0 + dble(jstep-1)*0.5d0*DYa
         X0 = X0 + (istart-1)*Dxa + dble(istep-1)*0.5d0*DXa
         Y0 = Y0 + (jstart-1)*Dya + dble(jstep-1)*0.5d0*DYa
         DXa = dble(istep)*DXa
         DYa = dble(jstep)*DYa

      end if   ! if ( LdirectReadBlock )

!     error handling
 1234 continue

      RETURN
      END


      SUBROUTINE WRIARCsam(MARC,DP,MMAX,NMAX,MC,NC,X0,Y0,DX,DY,dmiss)
      implicit none
      double precision :: dmiss
      double precision :: dp
      double precision :: dx, dy
      integer :: i
      integer :: j
      integer :: marc
      integer :: mc
      integer :: mmax
      integer :: nc
      integer :: nmax
      double precision :: x0
      double precision :: y0
!      DIMENSION DP  (MMAX,NMAX)
      DIMENSION DP  (NMAX,MMAX)   ! SPvdP: j-index is fastest running in sample arrays

      CALL WRITEARCINFOHEADER(MARC,MC,NC,X0,Y0,DX,DY,dmiss)

!      DO 10 J = NC,1,-1      ! SPvdP: j-index is already reverse-order in sample arrays
      DO 10 J = 1,NC
!         WRITE(MARC,'(5000F10.2)') ( DP(I,J),I = 1,MC)
         WRITE(MARC,'(5000F10.2)') ( DP(J,I),I = 1,MC)   ! SPvdP: j-index is fastest running in sample arrays
   10 CONTINUE

      RETURN
      END

      SUBROUTINE WRIARC(MARC,DP,MMAX,NMAX,MC,NC,X0,Y0,DX,DY,dmiss)
      implicit none
      double precision :: dmiss
      double precision :: dp
      double precision :: dx, dy
      integer :: i
      integer :: j
      integer :: marc
      integer :: mc
      integer :: mmax
      integer :: nc
      integer :: nmax
      double precision :: x0
      double precision :: y0
      DIMENSION DP  (MMAX,NMAX)

      CALL WRITEARCINFOHEADER(MARC,MC,NC,X0,Y0,DX,DY,dmiss)

      DO 10 J = NC,1,-1
         WRITE(MARC,'(5000F10.2)') ( DP(I,J),I = 1,MC)
   10 CONTINUE
      RETURN
      END


      SUBROUTINE WRITEARCINFOHEADER(MARC,MC,NC,X0,Y0,DX,DY,dmiss)
      implicit none
      double precision :: dmiss
      double precision :: dx, dy
      integer :: marc
      integer :: mc
      integer :: nc
      double precision :: x0
      double precision :: y0
      WRITE(MARC,'(A,I8)')    'ncols         ', MC
      WRITE(MARC,'(A,I8)')    'nrows         ', NC
      WRITE(MARC,'(A,F13.3)') 'xllcorner     ', X0 - DX/2
      WRITE(MARC,'(A,F13.3)') 'yllcorner     ', Y0 - DY/2
      WRITE(MARC,'(A,2F13.3)') 'cellsize      ', DX, DY
      WRITE(MARC,'(A,F13.3)') 'NODATA_value  ', dmiss
      RETURN
      END

      SUBROUTINE MISARRR(H,NUMQ,MMAX,NMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: j
      integer :: k
      integer :: mmax
      integer :: nmax
      integer :: numq
      double precision :: H(NUMQ,MMAX,NMAX)

      DO 10 K = 1,NUMQ
         DO 10 I = 1,MMAX
            DO 10 J = 1,NMAX
               H(K,I,J) = dmiss
   10 CONTINUE
      RETURN
      END

      SUBROUTINE MISARR(H,MMAX,NMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: j
      integer :: mmax
      integer :: nmax
      double precision :: H(MMAX,NMAX)

      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            H(I,J) = dmiss
   10 CONTINUE
      RETURN
      END

      SUBROUTINE MISAR(H,MMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: mmax
      double precision :: H(MMAX)

      DO 10 I = 1,MMAX
         H(I) = dmiss
   10 CONTINUE
      RETURN
      END

      SUBROUTINE IMISARRR(IH,NUMQ,MMAX,NMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: j
      integer :: k
      integer :: mmax
      integer :: nmax
      integer :: numq
      INTEGER IH(NUMQ,MMAX,NMAX)

      DO 10 K = 1,NUMQ
         DO 10 I = 1,MMAX
            DO 10 J = 1,NMAX
               IH(K,I,J) = dmiss
   10 CONTINUE
      RETURN
      END

      SUBROUTINE IMISARR(IH,MMAX,NMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: j
      integer :: mmax
      integer :: nmax
      INTEGER IH(MMAX,NMAX)

      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            IH(I,J) = dmiss
   10 CONTINUE
      RETURN
      END

      SUBROUTINE IMISAR(IH,MMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: mmax

      INTEGER IH(MMAX)
      DO 10 I = 1,MMAX
         IH(I) = dmiss
   10 CONTINUE
      RETURN
      END

      SUBROUTINE IMISAR2(IH,MMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: mmax
      INTEGER*2 IH(MMAX)

      DO 10 I = 1,MMAX
         IH(I) = INT(dmiss)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE TMISARR(H,MMAX,NMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: j
      integer :: mmax
      integer :: nmax

      double precision :: H(NMAX,-1:MMAX+1)
      DO 10 I = -1,MMAX+1
         DO 10 J = 1,NMAX
            H(J,I) = dmiss
   10 CONTINUE
      RETURN
      END

!     PLUS QNRGF
      SUBROUTINE NUL2(N1,NSMAX)
      implicit none
      integer :: i
      integer :: nsmax
      INTEGER*2 N1(NSMAX)
      DO 30 I = 1,NSMAX
         N1(I) = 0
   30 CONTINUE
      RETURN
      END

      SUBROUTINE NULAR (X, MMAX)
      implicit none
      integer :: i
      integer :: mmax
      double precision :: x
      DIMENSION X(MMAX)
      DO 10 I = 1,MMAX
         X(I) = 0d0
   10 CONTINUE
      RETURN
      END

      SUBROUTINE DNULAR (X, MMAX)
      implicit none
      integer :: i
      integer :: mmax
      DOUBLE PRECISION X(MMAX)
      DO 10 I = 1,MMAX
         X(I) = 0d0
   10 CONTINUE
      RETURN
      END

      SUBROUTINE DNULARR(X,   MMAX,   NMAX)
      implicit none
      integer :: i
      integer :: j
      integer :: mmax
      integer :: nmax
      DOUBLE PRECISION X(MMAX,NMAX)
      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            X(I,J) = 0d0
   10 CONTINUE
      RETURN
      END

      SUBROUTINE NULARR(X,   MMAX,   NMAX)
      implicit none
      integer :: i
      integer :: j
      integer :: mmax
      integer :: nmax
      double precision :: x
      DIMENSION X(MMAX,NMAX)
      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            X(I,J) = 0d0
   10 CONTINUE
      RETURN
      END

      SUBROUTINE NULARRR(X,   MMAX,   NMAX,   LMAX)
      implicit none
      integer :: i
      integer :: j
      integer :: l
      integer :: lmax
      integer :: mmax
      integer :: nmax
      double precision :: x
      DIMENSION X(MMAX,NMAX,LMAX)
      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            DO 10 L = 1,LMAX
               X(I,J,L) = 0d0
   10 CONTINUE
      RETURN
      END

      SUBROUTINE INULAR (X, MMAX)
      implicit none
      integer :: i
      integer :: mmax
      INTEGER X(MMAX)
      DO 10 I = 1,MMAX
         X(I) = 0d0
   10 CONTINUE
      RETURN
      END

      SUBROUTINE INULARR(X,   MMAX,   NMAX)
      implicit none
      integer :: i
      integer :: j
      integer :: mmax
      integer :: nmax
      INTEGER X(MMAX,NMAX)
      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            X(I,J)    = 0d0
   10 CONTINUE
      RETURN
      END

      SUBROUTINE INULARRR(X,   MMAX,   NMAX,   LMAX)
      implicit none
      integer :: i
      integer :: j
      integer :: l
      integer :: lmax
      integer :: mmax
      integer :: nmax
      INTEGER X(MMAX,NMAX,LMAX)
      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            DO 10 L = 1,LMAX
               X(I,J,L) = 0
   10 CONTINUE
      RETURN
      END

      SUBROUTINE INULAR2(X, MMAX)
      implicit none
      integer :: i
      integer :: mmax
      INTEGER*2 X(MMAX)
      DO 10 I = 1,MMAX
         X(I) = 0d0
   10 CONTINUE
      RETURN
      END

      SUBROUTINE INULARR2(X,   MMAX,   NMAX)
      implicit none
      integer :: i
      integer :: j
      integer :: mmax
      integer :: nmax
      INTEGER*2 X(MMAX,NMAX)
      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            X(I,J)    = 0d0
   10 CONTINUE
      RETURN
      END

      SUBROUTINE INULARRR2(X,   MMAX,   NMAX,   LMAX)
      implicit none
      integer :: i
      integer :: j
      integer :: l
      integer :: lmax
      integer :: mmax
      integer :: nmax
      INTEGER*2 X(MMAX,NMAX,LMAX)
      DO 10 I = 1,MMAX
         DO 10 J = 1,NMAX
            DO 10 L = 1,LMAX
               X(I,J,L)    = 0d0
   10 CONTINUE
      RETURN
      END

      SUBROUTINE RE0RCINFODIMENSIONS(MINP,MMAX,NMAX,DX,X0,Y0)
      implicit none
      double precision :: dx
      integer :: ja
      integer :: larc
      integer :: marc
      integer :: mc
      integer :: minp
      integer :: mmax
      integer :: nc
      integer :: nmax
      double precision :: rmis
      double precision :: x0
      double precision :: y0
      CHARACTER REC*132, FILENAME*80

      REWIND(MINP)
      CALL ZOEKJA(MINP,REC,'ARC-INFO',JA)
      IF (JA .EQ. 1) THEN
         LARC = INDEX(REC,'ARC')
         READ(REC(LARC+9:),'(A)',ERR = 888) FILENAME
         CALL OLDFIL(MARC,FILENAME)
         CALL READARCINFOHEADER(MARC,MC,NC,X0,Y0,DX,dx,RMIS)
         call doclose(MARC)
         MMAX = MC
         NMAX = NC
         CALL MESSAGE('MAIN DIMENSIONS PLUS GRID PARAMETERS HAVE',       &
                      'BEEN READ FROM ARC-INFO FILE:', FILENAME)
      ELSE
         CALL ERROR('NEITHER MAIN DIMENSIONS NOR ARC-INFO FILE FOUND,',  &
                    'SPECIFY MAIN DIMENSIONS BY KEYWORD:',               &
                    'MAIN DIMENSIONS OR BY ARC-INFO FILE')
      ENDIF
      RETURN

  888 CALL ERROR('LOOKING FOR ARC-INFO FILENAME, BUT GETTING:',REC,' ')
      END


      subroutine dateandtimenow(iyear, month, iday, ihour, minute, isecnd)
      implicit none
      integer,            intent(out)              :: iyear, month, iday, ihour, minute, isecnd
!     integer,            intent(out), optional    :: imsec
!     character(len=5),   intent(out), optional    :: zone

      character(len=8 ) ::       dat
      character(len=10) ::       tim
      character(len=5)  ::       zone
      integer           ::       imsec
      integer           ::       values(8)

      call date_and_time(dat,tim,zone,values)
      iyear  = values(1)
      month  = values(2)
      iday   = values(3)
      ihour  = values(5)
      minute = values(6)
      isecnd = values(7)
      imsec  = values(8)
      end subroutine dateandtimenow
      

      SUBROUTINE DATUM(DATE)
      implicit none
      integer :: iyear, month, iday, ihour, minute, isecnd
      CHARACTER DATE*20
!              1  4  7   11 14 17
      DATE = 'hh:mm:ss, dd-mm-yyyy'
     
      call dateandtimenow(iyear, month, iday, ihour, minute, isecnd)
      
      WRITE(DATE( 1:2 ),'(I2.2)') IHOUR
      WRITE(DATE( 4:5 ),'(I2.2)') MINUTE
      WRITE(DATE( 7:8 ),'(I2.2)') ISECND
      WRITE(DATE(11:12),'(I2.2)') IDAY
      WRITE(DATE(14:15),'(I2.2)') MONTH
      WRITE(DATE(17:20),'(I4)') IYEAR
      RETURN
      END

      SUBROUTINE DATUM2(DATE)
      use unstruc_display, only : jadatetime 
      implicit none
      integer :: iyear, month, iday, ihour, minute, isecnd
      CHARACTER DATE*20

      character(len=1), external :: get_dirsep
!              1  4  7   11 14 17
      
      if (jadatetime == 0) then
         DATE = get_dirsep()
      else
         DATE = '_yymmddhhmmss'//get_dirsep()
         
         call dateandtimenow(iyear, month, iday, ihour, minute, isecnd)

         WRITE(DATE(2:3),'(I2.2)') IYEAR - 2000 
         WRITE(DATE(4:5),'(I2.2)') month 
         WRITE(DATE(6:7),'(I2.2)') iday  
         WRITE(DATE(8:9),'(I2.2)') Ihour  
         WRITE(DATE(10:11),'(I2.2)') minute 
         WRITE(DATE(12:13),'(I2.2)') isecnd 
      endif
      RETURN
      END

      
      integer FUNCTION USECP()
      implicit none
      integer :: ihour
      integer :: isecnd
      integer :: minute
      integer :: iy,im,id
      call dateandtimenow(iy,im,id,IHOUR,MINUTE,ISECND)
      USECP = 3600*IHOUR+60*MINUTE+ISECND
      RETURN
      END


      SUBROUTINE READXYMIS(MINP)
      USE M_MISSING
      implicit none
      integer :: ja
      integer :: l
      integer :: minp
!     snelheidsdrempel
      CHARACTER REC*132, TEX*8
      CALL ZOEKAL(MINP,REC,'MISSING VALUE XY',JA)
      XYMIS = 0d0
      IF (JA .EQ. 1) THEN
         L = INDEX(REC,'XY') + 4
         READ(REC(L:),*,ERR = 888) XYMIS
         WRITE(TEX,'(F8.3)') XYMIS
         CALL MESSAGE('MISSING VALUE XY = ', TEX,' ')
      ENDIF
      RETURN
  888 CALL READERROR('READING MISSING VALUE XY, BUT GETTING', REC, MINP)
      END

      SUBROUTINE INDEKSI(N,NARRIN,INDX)
      implicit none
      integer :: i
      integer :: indx
      integer :: indxt
      integer :: ir
      integer :: j
      integer :: l
      integer :: n
      integer :: narrin
      double precision :: q
      DIMENSION NARRIN(N),INDX(N)
      DO 11 J=1,N
        INDX(J)=J
11    CONTINUE
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          INDXT=INDX(L)
          Q=NARRIN(INDXT)
        ELSE
          INDXT=INDX(IR)
          Q=NARRIN(INDXT)
          INDX(IR)=INDX(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            INDX(1)=INDXT
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(NARRIN(INDX(J)).LT.NARRIN(INDX(J+1)))J=J+1
          ENDIF
          IF(Q.LT.NARRIN(INDX(J)))THEN
            INDX(I)=INDX(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        INDX(I)=INDXT
      GO TO 10
      END
!
      SUBROUTINE INDEKS(N,ARRIN,INDX)
      implicit none
      double precision :: arrin
      integer :: i
      integer :: indx
      integer :: indxt
      integer :: ir
      integer :: j
      integer :: l
      integer :: n
      double precision :: q
      DIMENSION ARRIN(N),INDX(N)
      DO 11 J=1,N
        INDX(J)=J
11    CONTINUE
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          INDXT=INDX(L)
          Q=ARRIN(INDXT)
        ELSE
          INDXT=INDX(IR)
          Q=ARRIN(INDXT)
          INDX(IR)=INDX(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            INDX(1)=INDXT
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
          ENDIF
          IF(Q.LT.ARRIN(INDX(J)))THEN
            INDX(I)=INDX(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        INDX(I)=INDXT
      GO TO 10
      END

      SUBROUTINE BILIN(X, Y, Z, XP, YP, ZP)
      implicit none
      double precision :: r1
      double precision :: r2
      double precision :: x1
      double precision :: x2
      double precision :: xa
      double precision :: xp
      double precision :: xr
      double precision :: xrm
      double precision :: y1
      double precision :: y2
      double precision :: ya
      double precision :: yp
      double precision :: yr
      double precision :: yrm
      double precision :: zp
      double precision :: X(4), Y(4), Z(4)
!     Bepaal relatieve ligging in cel.
!     Twee coordinaten (xr,yr) van 0 tot 1.
!     Bilineaire interpolatie
      X1 = X(2) - X(1)
      Y1 = Y(2) - Y(1)
      R1 = X1*X1 + Y1*Y1
      X2 = X(4) - X(1)
      Y2 = Y(4) - Y(1)
      R2 = X2*X2 + Y2*Y2
      XA = XP - X(1)
      YA = YP - Y(1)
      XR = (X1*XA + Y1*YA)/R1
      YR = (X2*XA + Y2*YA)/R2

      XRM = 1 - XR
      YRM = 1 - YR
      ZP  = XRM*YRM*Z(1) + XR*YRM*Z(2) + XR*YR*Z(3) + XRM*YR*Z(4)
      RETURN
      END


      SUBROUTINE GETDIMENSIONS(MXD,NXD,MXLN,NSX)
      implicit none
      integer :: mout
      integer :: mxd
      integer :: mxln
      integer :: nsx
      integer :: nxd
      CHARACTER GETAL*100
      LOGICAL THISISANUMBER, JAWEL

      MXD  = 500       ! ROOSTERS EN SPLINES M-RICHTING
      NXD  = 500       ! ROOSTERS EN SPLINES N-RICHTING
      MXLN = 100000    ! land boundary
      NSX  = 100000    ! SAMPLES

      GETAL = ' '
      CALL get_command_argument(1,GETAL)
      IF (THISISANUMBER(GETAL)) READ(GETAL,*) MXD

      GETAL = ' '
      CALL get_command_argument(2,GETAL)
      IF (THISISANUMBER(GETAL)) READ(GETAL,*) NXD

      GETAL = ' '
      CALL get_command_argument(3,GETAL)
      IF (THISISANUMBER(GETAL)) READ(GETAL,*) MXLN

      GETAL = ' '
      CALL get_command_argument(4,GETAL)
      IF (THISISANUMBER(GETAL)) READ(GETAL,*) NSX

      INQUIRE (FILE = 'rgfdim', EXIST = JAWEL)
      IF (JAWEL) THEN
         MOUT  = 10
         call oldfil(MOUT,'rgfdim')
         READ  (MOUT,*,ERR=999) MXD,NXD,MXLN,NSX
         call doclose(MOUT)
      ENDIF
  999 CONTINUE
      RETURN
      END


      SUBROUTINE BILINXY(X, Y, XZ, YZ, XP, YP, XP2, YP2, INI)
      implicit none
      double precision :: c
      integer :: i
      integer :: ini
      integer :: japarallel
      double precision, SAVE    :: A(4,4), BX(4), BY(4)
      double precision :: X(4), Y(4), XZ(4), YZ(4), XP, YP, XP2, YP2
      INTEGER, SAVE :: INX(4)
      ! (Zi = AXi + BYi + CXiYi + Di ,i=1,4)
      ! Coefficienten in A, rechterlid in B, opl met LU-decompositie
      IF (INI .EQ. 1) THEN
         DO I = 1,4
            A(I,1) =  X(I)-X(1)
            A(I,2) =  Y(I)-Y(1)
            A(I,3) = (Y(I)-Y(1))*(X(I)-X(1))
            A(I,4) =  1
            BX(I)  =  XZ(I)
            BY(I)  =  YZ(I)
         ENDDO
         CALL LUDCMP(A,4,4,INX,C,JAPARALLEL)
         IF (JAPARALLEL .EQ. 1) THEN
            CALL qnerror('Problem in Ludcmp',' ',' ')
            INI = -1
            RETURN
         ENDIF
         CALL LUBKSB(A,4,4,INX,BX)
         CALL LUBKSB(A,4,4,INX,BY)
      ENDIF
      XP2 = (XP-X(1))*BX(1) + (YP-Y(1))*BX(2) + (XP-X(1))*(YP-Y(1))*BX(3) + BX(4)
      YP2 = (XP-X(1))*BY(1) + (YP-Y(1))*BY(2) + (XP-X(1))*(YP-Y(1))*BY(3) + BY(4)
      RETURN
      END

      SUBROUTINE LUDCMP(A,N,NP,INDX,D,JAPARALLEL)
      implicit none
      double precision :: a
      double precision :: aamax
      double precision :: d
      double precision :: dum
      integer :: i
      integer :: imax
      integer :: indx
      integer :: j
      integer :: japarallel
      integer :: k
      integer :: n
      integer :: np
      integer :: nx
      double precision :: sum
      double precision :: tiny
      double precision :: vv
      PARAMETER (NX=4,TINY=1d-20)
      DIMENSION A(NP,NP),INDX(N),VV(NX)
      JAPARALLEL = 0
      D=1.
      DO 12 I=1,N
        AAMAX=0.
        DO 11 J=1,N
          IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
11      CONTINUE
        IF (AAMAX .EQ. 0) THEN
           JAPARALLEL = 1
           RETURN
        ENDIF
        VV(I)=1./AAMAX
12    CONTINUE
      DO 19 J=1,N
        IF (J.GT.1) THEN
          DO 14 I=1,J-1
            SUM=A(I,J)
            IF (I.GT.1)THEN
              DO 13 K=1,I-1
                SUM=SUM-A(I,K)*A(K,J)
13            CONTINUE
              A(I,J)=SUM
            ENDIF
14        CONTINUE
        ENDIF
        AAMAX=0.
        DO 16 I=J,N
          SUM=A(I,J)
          IF (J.GT.1)THEN
            DO 15 K=1,J-1
              SUM=SUM-A(I,K)*A(K,J)
15          CONTINUE
            A(I,J)=SUM
          ENDIF
          DUM=VV(I)*ABS(SUM)
          IF (DUM.GE.AAMAX) THEN
            IMAX=I
            AAMAX=DUM
          ENDIF
16      CONTINUE
        IF (J.NE.IMAX)THEN
          DO 17 K=1,N
            DUM=A(IMAX,K)
            A(IMAX,K)=A(J,K)
            A(J,K)=DUM
17        CONTINUE
          D=-D
          VV(IMAX)=VV(J)
        ENDIF
        INDX(J)=IMAX
        IF(J.NE.N)THEN
          IF(A(J,J).EQ.0d0)A(J,J)=TINY
          DUM=1./A(J,J)
          DO 18 I=J+1,N
            A(I,J)=A(I,J)*DUM
18        CONTINUE
        ENDIF
19    CONTINUE
      IF(A(N,N).EQ.0d0)A(N,N)=TINY
      RETURN
      END

      SUBROUTINE LUBKSB(A,N,NP,INDX,B)
      implicit none
      double precision :: a
      double precision :: b
      integer :: i
      integer :: ii
      integer :: indx
      integer :: j
      integer :: ll
      integer :: n
      integer :: np
      double precision :: sum
      DIMENSION A(NP,NP),INDX(N),B(N)
      II=0
      DO 12 I=1,N
        LL=INDX(I)
        SUM=B(LL)
        B(LL)=B(I)
        IF (II.NE.0)THEN
          DO 11 J=II,I-1
            SUM=SUM-A(I,J)*B(J)
11        CONTINUE
        ELSE IF (SUM.NE.0d0) THEN
          II=I
        ENDIF
        B(I)=SUM
12    CONTINUE
      DO 14 I=N,1,-1
        SUM=B(I)
        IF(I.LT.N)THEN
          DO 13 J=I+1,N
            SUM=SUM-A(I,J)*B(J)
13        CONTINUE
        ENDIF
        B(I)=SUM/A(I,I)
14    CONTINUE
      RETURN
      END

    SUBROUTINE SETCLASSFILE()
    implicit none
    integer :: minp
    CHARACTER FILNAM*86
    FILNAM = '*.cls'
    MINP   = 0
    CALL FILEMENU(MINP,FILNAM)
    IF (MINP .GT. 0) THEN
       CALL REACLS(MINP)
    ENDIF
    RETURN
    END

  SUBROUTINE SYSORLOCALFIL(LUNID,FILNAM,MUSTBE)
  use string_module, only: find_first_char
  use unstruc_messages
  use unstruc_files
  implicit none
  integer :: istart
  integer :: k1
  integer :: k2
  integer :: lunid
  integer :: mustbe
  integer :: istat
  CHARACTER FILNAM*76,FULNAM*180
  LOGICAL JA

  LUNID = 0
  INQUIRE(FILE = FILNAM,EXIST = JA)
  IF (JA) THEN
     CALL OLDFIL(LUNID,FILNAM)
     WRITE(msgbuf,'(2A)') 'Using Local File', FILNAM; call msg_flush()
  ELSE

     FULNAM = PATHDI
     ISTART = len_trim(PATHDI) + 1
     WRITE(FULNAM(ISTART:),'(A)') FILNAM
     K1 = find_first_char(FULNAM)
     K2 = len_trim(FULNAM)
     INQUIRE(FILE= FULNAM(K1:K2),EXIST=JA)
     IF (JA) THEN
        CALL OLDFIL(LUNID,FULNAM)
        call mess(LEVEL_INFO, 'Using Program File ', FULNAM(K1:K2))
     ELSE IF (MUSTBE .EQ. 1) THEN
        call mess(LEVEL_ERROR, 'Program File ',FULNAM(K1:K2),' Not Found')
     ENDIF

  ENDIF

  RETURN
  END

      SUBROUTINE REACLS (MLAN)      ! DV
      implicit none
      double precision :: dv
      integer :: i
      integer :: jaauto
      integer :: mlan
      integer :: ncols
      integer :: nie
      integer :: nis
      integer :: nrow
      integer :: nv
      double precision :: val
      double precision :: vmax
      double precision :: vmin
      double precision :: x
!     ------------------------------------------------------------------
!     LEZEN FILE MET CLASSES
!    ------------------------------------------------------------------
      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

      CHARACTER MATR*4

      CALL READYY ('READING CLS-FILE',0d0)

!     ------------------------------------------------------------------
!     EERST LEZEN ALS DUMMY WAARDEN OM TE TESTEN OF ER FOUTEN OPTREDEN
!     ------------------------------------------------------------------
   10 READ(MLAN,'(A)',END=999) MATR
      IF (MATR(1:1) .EQ. '*') GOTO 10

      READ (MLAN,*,ERR=999) NROW

      NROW = MIN(NROW,30)

      DO 20 I = 1,NROW
         READ (MLAN,*,ERR=999,END=999) X
   20 CONTINUE

!     -------------------------------------
!     NO ERRORS, SO READ THE VALUES
!     -------------------------------------
      REWIND (MLAN)

  110 READ(MLAN,'(A)',END=999) MATR
      IF (MATR(1:1) .EQ. '*') GOTO 110

      READ (MLAN,*,ERR=999) NROW

      NROW = MIN(NROW,30)

      DO 120 I = 1,NROW
         READ (MLAN,*) VAL(I)
         VMIN = MIN(VMIN,VAL(I))
         VMAX = MAX(VMAX,VAL(I))
  120 CONTINUE

      JAAUTO = 2
      NV = NROW

  999 CONTINUE

      CALL READYY(' ', 1d0)
      CALL READYY(' ',-1d0)
      call doclose (MLAN)
      RETURN
      END

      subroutine read_samples_from_dem(filnam, jadoorladen)
    use dem
    use m_missing
    use m_samples
    implicit none
    character(len=*), intent(in) :: filnam
    integer, intent(in) :: jadoorladen
    integer :: ndraw
    COMMON /DRAWTHIS/ ndraw(50)
  
    integer :: i, j, is, istep
    type(DEMInfo) :: dem_info
    integer, allocatable :: arr(:,:)
    double precision, allocatable :: xarr(:,:), yarr(:,:)
    character(len=10) :: TEX


    call savesam()
    if (jadoorladen == 0) then
        ns = 0
    end if

    call read_dem_file(trim(filnam),dem_info, xarr, yarr, arr)
    if (dem_info%rows <= 0 .or. dem_info%cols <= 0) then
        call message('No samples read from file ', filnam, ' ')
        return
    end if

    call increasesam(ns + dem_info%rows*dem_info%cols)

    WRITE(TEX,'(I10)') dem_info%rows*dem_info%cols
    CALL READYY('Filtering '//TRIM(TEX)//' Samples Points',0d0)

    istep = int(dem_info%rows/100d0)
    do i=1,dem_info%rows
        do j=1,dem_info%cols
            if (arr(i,j) == NODATA) then
                continue
            else
                ns = ns + 1
                xs(ns) = xarr(i,j)
                ys(ns) = yarr(i,j)
                zs(ns) = dble(arr(i,j))
            end if
        end do
        IF (MOD(i,istep) .EQ. 0) THEN
            CALL READYY(' ',MIN( 1d0,dble(i)/dem_info%rows) )
        ENDIF
    end do
    deallocate(xarr, yarr, arr)
    CALL READYY(' ',-1d0)

    IF (NS .GT. 100000) NDRAW(32) = 7 ! Squares (faster than circles)
    IF (NS .GT. 500000) NDRAW(32) = 3 ! Small dots (fastest)

    WRITE(TEX,'(I10)') NS
    CALL READYY('Sorting '//TRIM(TEX)//' Samples Points',0d0)
    IF (NS .GT. 1) THEN
      CALL TIDYSAMPLES(XS,YS,ZS,IPSAM,NS,MXSAM,MYSAM)
      call get_samples_boundingbox()
      IPSTAT = IPSTAT_OK
    END IF
    CALL READYY(' ',-1d0)
end subroutine read_samples_from_dem

subroutine read_samples_from_arcinfo(filnam, jadoorladen)  ! reaasc
    use m_missing
    use m_samples
    use m_samples_refine, only: iHesstat, iHesstat_DIRTY
    use m_arcinfo
    use unstruc_display, only: jagui
    implicit none
    character(len=*), intent(in) :: filnam
    integer, intent(in) :: jadoorladen
    integer :: i, j, istep, marc
    character(len=10) :: TEX

    integer :: ndraw
    COMMON /DRAWTHIS/ ndraw(50)

    CALL READYY('Reading arcinfo file',0d0)
    call oldfil(marc, filnam)
    call reaarc (marc,jagui)
    CALL DOCLOSE(marc)
    CALL READYY('Reading arcinfo file',1d0)

    if (mca <= 0 .or. nca <= 0) then
        call message('No samples read from file ', filnam, ' ')
        return
    end if

    call savesam()
    if (jadoorladen == 0) then
        ns = 0
    end if
    CALL INCREASEsam(ns+mca*nca)

    WRITE(TEX,'(I10)') mca*nca
    CALL READYY('Filtering '//TRIM(TEX)//' Samples Points',0d0)
    istep = max(int(mca/100d0+.5d0),1)
! SPvdP: j needs to be fastest running index
    do i = 1,mca
        IF (MOD(i,istep) .EQ. 0) THEN
            CALL READYY('Filtering '//TRIM(TEX)//' Samples Points',min( 1d0,dble(i)/mca))
        ENDIF

        do j = nca,1,-1 ! SPvdP: first line needs to be nca'th row
!            if (d(I,J) .ne. dmiss) then  ! SPvdP: we need to maintain structured data
                ns = ns+1
                xs(ns) =  x0 + dxa*(i-1)
                ys(ns) =  y0 + dya*(j-1)
                zs(ns) =  d(i,j)
!            endif
        enddo
    enddo
    CALL READYY(' ',-1d0)
    
!   mark samples as structured, and in supply block sizes
    MXSAM = nca   ! j is fastest running index
    MYSAM = mca
    IPSTAT = IPSTAT_NOTOK

!   new sample set: no Hessians computed yet
    iHesstat = iHesstat_DIRTY

    deallocate(d) ! Save memory, arcinfo block is no longer needed.

    IF (NS .GT. 100000) NDRAW(32) = 7 ! Squares (faster than circles)
    IF (NS .GT. 500000) NDRAW(32) = 3 ! Small dots (fastest)

    ! No TIDYSAMPLES required: arcinfo grid was already loaded in correctly sorted order.
    do i=1,NS
      IPSAM(i) = i
    end do
    call get_samples_boundingbox()
    IPSTAT = IPSTAT_OK
end subroutine read_samples_from_arcinfo

      SUBROUTINE REASAM(MSAM, JADOORLADEN)
      USE M_MISSING
      USE M_SAMPLES
      use m_alloc
      implicit none
      integer, intent(inout) :: msam        !< already opened file pointer to sample file
      integer, intent(in)    :: jadoorladen !< whether to append to global set (1) or start empty (0)
      integer :: ierr
      integer :: jflow
      integer :: jqn
      integer :: mcs
      integer :: ncs
      integer :: ndraw
      integer :: nkol
      integer :: nrow
      integer :: ns1
      integer :: nsm
      integer :: num
      integer :: K, K0
      double precision :: x, y, z 
      double precision :: XX, YY, ZZ, ZZ2
     

      COMMON /PHAROSFLOW/  JFLOW
      COMMON /PHAROSLINE/  REC1
      COMMON /SAMPLESADM/  MCS,NCS,NS1
      COMMON /QNRGF/ JQN
      COMMON /DRAWTHIS/ ndraw(50)

      CHARACTER REC*132, TEX*10, REC1*132
      LOGICAL THISISANUMBER

      CALL SAVESAM()
      NSM = 0
      MXSAM = 0
      MYSAM = 0
      IPSTAT = IPSTAT_NOTOK
      nkol = 0
      CALL READYY('Counting nr. of Samples ',0d0)
   11 READ (MSAM,'()',END = 31)
         NSM = NSM + 1
      GOTO 11
   31 NSMAX = 1.2d0*(NSM + JADOORLADEN*NS)
      IF (NSMAX .GT. 100000) NDRAW(32) = 7
      IF (NSMAX .GT. 500000) NDRAW(32) = 3
      IF (ALLOCATED (XS) ) DEALLOCATE (XS,YS,ZS)
      ALLOCATE (XS(NSMAX),YS(NSMAX),ZS(NSMAX),STAT=IERR)
      CALL AERR ('XS(NSMAX),YS(NSMAX),ZS(NSMAX)',IERR,NSMAX)
      if ( allocated(ipsam) ) deallocate(ipsam)
      allocate(ipsam(NSMAX),stat=ierr)
      call aerr('ipsam(NSMAX)',ierr,NSMAX)
      CALL READYY(' ',-1d0)

      REWIND(MSAM)

      WRITE(TEX,'(I10)') NSM
      CALL READYY('Reading '//TRIM(TEX)//' Sample Points',0d0)
      IF (JADOORLADEN .EQ. 0) THEN
         CALL XMISAR(XS,NSMAX)
         CALL XMISAR(YS,NSMAX)
         CALL  MISAR(ZS,NSMAX)
         K = 0
      ELSE
         CALL RESTORESAM()
         K   = NS
         NS1 = NS
      ENDIF
      K0 = K

!    check of dit een PHAROS file is
      JFLOW = 1
   14 READ (MSAM,'(A)',END = 30) REC1
      if (rec1(1:1) == '*') goto 14 

      IF ( .NOT. (THISISANUMBER(REC1)) ) THEN
         READ (MSAM,'(A)',END = 30) REC
         IF ( THISISANUMBER(REC) ) THEN
            READ (REC,*,ERR = 16) NROW,NKOL
            GOTO 15
   16       CONTINUE
            READ (MSAM,'(A)',END = 30) REC
            READ (REC,*,ERR = 15) NUM,X,Y,Z
            JFLOW = 3
         ENDIF
      ENDIF
   15 CONTINUE

      REWIND (MSAM)


      KMOD = max(1, NSM/100)
   10 CONTINUE
      READ (MSAM,'(A)',END = 30) REC
      IF (REC(1:1) .EQ. '*') GOTO 10
      IF ( .NOT. (THISISANUMBER(REC)) ) THEN
!        we nemen aan dat er net een blokcode is gelezen
!        en we lezen meteen de nrow ncol regel, maar checken die regel niet
         READ  (MSAM,'(A)',END = 30) REC
      ELSE

         IF (JFLOW .EQ. 3) THEN
            READ (REC,*,ERR = 40) NUM,XX,YY,ZZ
         ELSE IF (NKOL == 4) THEN  
            READ (REC,*,ERR = 40) XX,YY, ZZ, ZZ2
            if (zz .ne. -999d0) then 
               zz = sqrt(zz*zz + zz2*zz2)
            endif 
         ELSE 
            READ (REC,*,end = 40) XX,YY,ZZ
            READ (REC,*,ERR = 40) XX,YY,ZZ
         ENDIF

         IF (K  .LE. NSMAX-1 .AND. XX .NE. XYMIS .AND.   &
             ZZ .NE. dmiss .AND. ZZ .NE. 999.999d0 .and. &
             .not.(isnan(XX) .or. isnan(YY) .or. isnan(ZZ)) ) THEN
            K     = K + 1
            NS    = K
            XS(K) = XX
            YS(K) = YY
            ZS(K) = ZZ
         ENDIF
         IF (MOD(K-K0,KMOD) .EQ. 0) THEN
            CALL READYY(' ',MIN( 1d0,dble(K)/NSM) )
         ENDIF
      ENDIF
      GOTO 10

   40 CONTINUE
      WRITE(TEX,'(I10)') K
      CALL QNERROR('ERROR READING SAMPLES FILE LINE NR ',TEX,REC)

   30 CONTINUE
      IF (K .GT. NSMAX) THEN
         WRITE(TEX,'(I8)') NSMAX
         CALL QNERROR('ONLY',TEX,'SAMPLE POINTS CAN BE LOADED')
         WRITE(TEX,'(I8)') K
         CALL QNERROR('YOU TRIED TO LOAD',TEX,'SAMPLE POINTS')
      ENDIF
      CALL READYY(' ',-1d0)
      WRITE(TEX,'(I10)') NS
      CALL READYY('Sorting '//TRIM(TEX)//' Samples Points',0d0)
      IF (NS .GT. 1) THEN
         CALL TIDYSAMPLES(XS,YS,ZS,IPSAM,NS,MXSAM,MYSAM)
         call get_samples_boundingbox()
         IPSTAT = IPSTAT_OK
      END IF
      CALL READYY(' ',-1d0)
      call doclose(MSAM)
      RETURN
      END


      SUBROUTINE XMISAR (X, MMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: mmax
      double precision :: x
      DIMENSION X(MMAX)
      DO 10 I = 1,MMAX
         X(I) = XYMIS
   10 CONTINUE
      RETURN
      END

      SUBROUTINE TIDYSAMPLES(XS,YS,ZS,IPSAM,NS,MXSAM,MYSAM)
      use sorting_algorithms, only: indexx
      implicit none
      integer :: ns
      double precision :: XS(NS), YS(NS), ZS(NS)   !< sample coordinates
      integer, dimension(NS), intent(out) :: IPSAM !< permutation array (increasing x-coordinate)
      integer,                intent(in)  :: MXSAM, MYSAM   !< structured sample data dimensions (>0) or unstructured (0)
!      IF (NS .GT. 1) CALL RSORT3(XS,YS,ZS,NS)

      if ( NS.gt.1 ) then
         call indexx(Ns,xs,IPSAM)
      end if
   
!     remove double/missing samples (non-structured sample data only)
      if ( MXSAM*MYSAM.ne.NS ) then
         CALL READYY(' ',0.3d0)
         IF (NS .GT. 1) CALL RMDOUBLE(XS,YS,ZS,IPSAM,NS)
      end if
      
      CALL READYY(' ',1d0)
      
      RETURN
      END


!>    determine sample bounding box
      subroutine get_samples_boundingbox()
         use m_samples
         use m_missing
         implicit none

         integer :: i

         xsammin =  huge(1d0)
         xsammax = -huge(1d0)
         ysammin =  huge(1d0)
         ysammax = -huge(1d0)

         do i=1,NS
            if ( xs(i).ne.DMISS .and. ys(i).ne.DMISS .and. zs(i).ne.DMISS ) then
               xsammin = min(xsammin,xs(i))
               xsammax = max(xsammax,xs(i))
               ysammin = min(ysammin,ys(i))
               ysammax = max(ysammax,ys(i))
            end if
         end do

         return
      end subroutine get_samples_boundingbox
      
      SUBROUTINE RSORT3new (X, Y, Z, N) ! 1 !!  second faster than   
      use sorting_algorithms, only: indexx
      implicit none
      double precision              :: X(N), Y(N), Z(N)
      integer, allocatable          :: ind(:)
      double precision, allocatable :: h(:)
      integer :: k, n
      
      allocate(ind(n), h(n))
      
      call indexx(n,x,ind)
      
      h = x
      do k = 1,n
         x(k) = h(ind(k))
      enddo   
          
      h = y
      do k = 1,n
         y(k) = h(ind(k))
      enddo   

      h = z
      do k = 1,n
         z(k) = h(ind(k))
      enddo   
      
      deallocate(ind,h)
      
      end SUBROUTINE RSORT3new
      
      SUBROUTINE RSORT3(X, Y, Z, N)
      implicit none
      integer :: j
      integer :: j1
      integer :: k0
      integer :: kk
      integer :: lk
      integer :: n
      integer :: nm
      double precision :: temp

      double precision :: X(N), Y(N), Z(N)
      IF (N .EQ. 0) RETURN
      LK = N / 2
      K0 = LK
      KK = K0

20    J  = 2 * KK
      J1 = J + 1

30    IF (   J1 .LE. N ) THEN

         IF ( X(J) .LT. X(J1) )  J  = J1

      ENDIF

      IF ( X(KK) .LT. X(J) ) THEN

         TEMP  = X(J)
         X(J)  = X(KK)
         X(KK)  = TEMP

         TEMP  = Y(J)
         Y(J)  = Y(KK)
         Y(KK)  = TEMP

         TEMP  = Z(J)
         Z(J)  = Z(KK)
         Z(KK)  = TEMP

         IF ( J .LE. LK ) THEN
            KK = J
            GOTO 20
         ENDIF

      ENDIF

      K0 = K0 - 1

      IF ( K0 .NE. 0 ) THEN
         KK  = K0
         J  = 2 * KK
         J1 = J + 1
         GOTO 30
      ENDIF

      NM = N

65    TEMP  = X(1)
      X(1)  = X(NM)
      X(NM) = TEMP

      TEMP  = Y(1)
      Y(1)  = Y(NM)
      Y(NM) = TEMP

      TEMP  = Z(1)
      Z(1)  = Z(NM)
      Z(NM) = TEMP

      NM = NM - 1
      IF ( NM .EQ. 1 ) RETURN

      KK  = 1

70    J  = 2 * KK
      J1 = J + 1

      IF (    J .GT. NM ) GOTO 65

      IF ( J1 .LE. NM .AND.  X(J) .LT. X(J1) )  J = J1

      IF ( X(KK) .GE. X(J) ) GOTO 65

      TEMP  = X(J)
      X(J)  = X(KK)
      X(KK)  = TEMP

      TEMP  = Y(J)
      Y(J)  = Y(KK)
      Y(KK)  = TEMP

      TEMP  = Z(J)
      Z(J)  = Z(KK)
      Z(KK)  = TEMP

      KK = J

      GOTO 70

      END

      SUBROUTINE RMDOUBLE(XS,YS,ZS,IPSAM,NS)
      use m_missing
      use m_sferic
      use unstruc_messages
      use kdtree2Factory
      
      implicit none

      integer :: i
      integer :: j
      integer :: jadouble
      integer :: k
      integer :: ns
      integer :: nsorg
      integer :: numweg
      integer :: isam, jsam, ksam
      double precision,       intent(inout) :: XS(NS), YS(NS), ZS(NS)
      integer, dimension(NS), intent(inout) :: IPSAM  !< permutation array (increasing x-coordinate)
      integer, dimension(:), allocatable    :: newnode

      double precision, dimension(:), allocatable :: xx, yy  ! non-missing sample coordinates

      integer,          dimension(:), allocatable :: iperm   ! permutation array

      double precision                            :: t0, t1, t2, t3, t4
      integer                                     :: ii, jj, NN, num, nummerged, ierror
      integer                                     :: jakdtree=1

      integer                                     :: jsferic_store

      character(len=128)                          :: txt
      
      double precision, parameter                 :: dtol2 = 1d-8 ! sample-on-top of each other tolerance, squared

      CHARACTER OUD*8, NIEUW*8
      NSORG = NS
      
      allocate(newnode(NS))

!     store jsferic
      jsferic_store = jsferic

!     safety
      !if ( NS.lt.100 ) then
      !   jakdtree=0
      !end if

      call klok(t0)

      if ( jakdtree.eq.1 ) then
!        force Cartesian coordinates
         jsferic = 0
!        get non-missing sample coordinates
         allocate(xx(NS))
         xx = 0d0
         allocate(yy(NS))
         yy = 0d0
         allocate(iperm(NS))
         iperm = 0

!        get non-missing sample coordinates
         num = 0
         do i=1,NS
            if ( xs(i).ne.DMISS .and. ys(i).ne.DMISS ) then
            
               if ( num.gt.1 .and. janeedfix.eq.1 ) then
!                 fix for Wim: already check samples with latest (not understood kdtree error until June 2017)
                  if( xs(i).eq.xs(num) .and. ys(i).eq.ys(num) ) cycle
               end if
               
               num = num+1
               xx(num) = xs(i)
               yy(num) = ys(i)
               iperm(num) = i
            end if
         end do

!        initialize kdtree
         call build_kdtree(treeglob,num,xx,yy,ierror, jsferic, dmiss)

!        deallocate arrays with non-missing node coordinates
         deallocate(xx)
         deallocate(yy)

         if ( ierror.ne.0 ) then
!           deallocate permutation array
            if ( allocated(iperm) ) deallocate(iperm)

!           deallocate kdtree
            if ( treeglob%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(treeglob)

!           disable kdtree
            jakdtree = 0
         end if
      end if

      nummerged=0

!     fill double samples with DMISS
    5 CONTINUE
      JADOUBLE = 0

      if ( jakdtree.eq.1 ) then
!        find samples on top of each other
         do ii=1,num
            i=iperm(ii)
            
            if ( i.eq.0 ) cycle   ! already merged

!           fill query vector
            call make_queryvector_kdtree(treeglob,xs(i),ys(i), jsferic)

!           count number of points in search area
            NN = kdtree2_r_count(treeglob%tree,treeglob%qv,dtol2)

            if ( NN.gt.1 ) then ! at least two samples need to be merged
!              resize results array if necessary
               call realloc_results_kdtree(treeglob,NN)

!              find other nodes
               call kdtree2_n_nearest(treeglob%tree,treeglob%qv,NN,treeglob%results)

!              merge with other nodes
               do k=1,NN
                  jj = treeglob%results(k)%idx
                  j  = iperm(jj)
!                 exclude own sample and samples already deleted
                  if ( j.ne.i .and. j.gt.0 ) then
                     if ( xs(i).eq.xs(j) .and. ys(i).eq.ys(j) ) then ! NOT SPHERICAL-PROOF
                        iperm(jj) = 0
                        xs(j) = DMISS
                        jadouble = 1
                        nummerged = nummerged+1
                     end if
                  end if
               end do
            end if
         end do
      else  !  non kdtree
      DO 10 I = 1,NS-1
         ISAM = IPSAM(I)
         IF (XS(ISAM) .NE. XYMIS .and. ZS(ISAM) .NE. DMISS) THEN
            J = I
   15       CONTINUE
            IF (J .LT. NS) THEN
               J = J + 1
               JSAM = IPSAM(J)
               IF (XS(ISAM) .EQ. XS(JSAM)) THEN
                  IF (YS(ISAM) .EQ. YS(JSAM)) THEN
                      XS(JSAM)    = XYMIS
                      JADOUBLE = 1
                  ENDIF
                  GOTO 15
               ENDIF
            ENDIF
         ENDIF
   10 CONTINUE

      end if

      call klok(t1)

!     remove double samples
      K = 0
      newnode = 0
      DO 20 I = 1,NS
         IF (XS(I) .NE. XYMIS .and. ZS(I) .NE. DMISS) THEN
            K = K + 1
            XS(K) = XS(I)
            YS(K) = YS(I)
            ZS(K) = ZS(I)
            newnode(i) = k   ! old-to-new sample number
         ENDIF
   20 CONTINUE
      
      call klok(t2)
      
!     update permutation array
      k = 0
      do i=1,NS
         j = IPSAM(i)   ! old node number
         if ( newnode(j).gt.0 ) then
            k=k+1
            IPSAM(k) = newnode(j)
         end if
      end do
      
      call klok(t3)
      
!     set new number of samples      
      NS = K

      IF (JADOUBLE .EQ. 1) GOTO 5

      NUMWEG = NSORG - K
      IF (NUMWEG .GE. 1) THEN
         WRITE(OUD,'(I8)') NUMWEG
         ! CALL QNERROR('NUMBER OF DOUBLE POINTS REMOVED',OUD,' ')
      ENDIF
      
      call klok(t4)

!     output message
      if ( jakdtree.eq.1 ) then
         txt = ''
         write(txt, "('merged ', I0, ' samples in ', F0.2, ' seconds.')") nummerged, t4-t0
         call mess(LEVEL_INFO, trim(txt))
      end if
      
 1234 continue

!     deallocate
      if ( allocated(newnode) ) deallocate(newnode)

      if ( jakdtree.eq.1 ) then
!         deallocate permutation array
          if ( allocated(iperm) ) deallocate(iperm)

!         deallocate kdtree
          if ( treeglob%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(treeglob)
      end if

!     restore jsferic
      jsferic = jsferic_store

      RETURN
      END


      SUBROUTINE MAKEPLOTAREAS(NUMROW,NUMCOL,nsize)
      implicit none
      double precision :: dx
      double precision :: dy
      integer :: i, nsize
      integer :: j
      integer :: nsc
      integer :: numcol
      integer :: numrow
      integer :: numsc
      double precision :: x1sc
      double precision :: x2sc
      double precision :: xb
      double precision :: xm
      double precision :: xz
      double precision :: y1sc
      double precision :: y2sc
      double precision :: yb
      double precision :: ym
      double precision :: yz
      COMMON /GSCREENS/ X1SC(100),Y1SC(100),X2SC(100),Y2SC(100),NUMSC
      NSC  = 0
   
      if (numrow == 1) then  
         yz = 0.4d0 ; yb = 0.8d0*(1d0-yz)
      else   
         yz = 0.7d0 ; yb = 0.8d0*(1d0-yz)
      endif
   
      if (numcol < 3) then 
         xz = 0.7d0 ; xb = 0.5d0*(1d0-xz)
      else  
         xz = 0.9d0 ; xb = 0.001d0 ! 05d0*(1d0-xz) 
      endif
   
      if (nsize == 2) then 
         yz = 0.45d0 ; yb = 0.8d0*(1d0-yz)
      endif
      DY = yz / NUMROW
      DX = xz / NUMCOL
      XM = DX / 40
      YM = DY / 40
      DO 10 J = 1,NUMROW
         DO 10 I = 1,NUMCOL
            NSC = NSC + 1
            X1SC(NSC) = xb + (I-1)*DX + XM
            X2SC(NSC) = xb + (I  )*DX - XM
            Y1SC(NSC) = 1d0-yb - (J  )*DY + YM
            Y2SC(NSC) = 1d0-yb - (J-1)*DY - YM
   10 CONTINUE
      NUMSC = NSC
      RETURN
      END

!>    bilineair interpolation between four nodes
      subroutine bilin6(x, y, z, xp, yp, zp)
      use m_missing
      
      implicit none
      
      double precision, dimension(2,2), intent(in)  :: x, y   !< node coordinates
      double precision, dimension(2,2), intent(in)  :: z      !< node values
      double precision,                 intent(in)  :: xp, yp !< interpolant coordinates
      double precision,                 intent(out) :: zp     !< interpolant value
      
      integer                                       :: ierror
      
      double precision                              :: A, B, C, D
      double precision                              :: E, F, G, H
      double precision                              :: P, Q, R, S
      double precision                              :: xi1, eta1, xp1, yp1
      double precision                              :: xi2, eta2, xp2, yp2
      
      double precision                              :: aa, bb, cc, dis
      
      double precision, parameter                   :: dtol = 1d-9
      
      ierror = 1
      
      zp = DMISS
      
!     the interpolant has the form:
!        xp(xi,eta) = A + B xi + C eta + D xi eta
!        yp(xi,eta) = E + F xi + G eta + H xi eta
!        zp(xi,eta) = P + Q xi + R eta + S xi eta

      A = x(1,1)
      B = x(2,1) - x(1,1)                       ! "xi-derivative"
      C = x(1,2) - x(1,1)                       ! "eta-derivative"
      D = (x(2,2) - x(2,1)) - (x(1,2)-x(1,1))   ! "xi-eta-derivative"
      
      E = y(1,1)
      F = y(2,1) - y(1,1)                       ! "xi-derivative"
      G = y(1,2) - y(1,1)                       ! "eta-derivative"
      H = (y(2,2) - y(2,1)) - (y(1,2)-y(1,1))   ! "xi-eta-derivative"
      
      P = z(1,1)
      Q = z(2,1) - z(1,1)                       ! "xi-derivative"
      R = z(1,2) - z(1,1)                       ! "eta-derivative"
      S = (z(2,2) - z(2,1)) - (z(1,2)-z(1,1))   ! "xi-eta-derivative"
      
!     determine xi and eta from xp(xi,eta) = xp and yp(xi,eta)=yp
!        aa xi^2 + bb xi + cc = 0
      aa = D*F - B*H
      bb = H*(xp-A) - B*G - D*(yp-E) + C*F
      cc = G*(xp-A) - C*(yp-E)
      
      dis = bb*bb - 4.0d0*aa*cc
      
      if ( dis.lt.0d0 ) goto 1234
      
      if ( abs(aa).gt.dtol ) then
         dis = sqrt(dis)
         aa  = 0.5d0 / aa
         xi1 = (-bb - dis) * aa
         xi2 = (-bb + dis) * aa
      else if ( abs(bb).gt.1d-9) then ! bb*xi + cc = 0
         xi1 = -cc/bb
         xi2 = xi1
      else  ! no or infinitely many solutions
         goto 1234
      end if
      
      eta1 = C+D*xi1
      if ( abs(eta1).gt.dtol ) then
         eta1 = (xp-(A+B*xi1))/eta1
      else
         eta1 = G + H*xi2
         if ( abs(eta1).gt.dtol ) then
            eta1 = (yp-E-F*xi1)/ eta1
         else
            eta1 = DMISS
         end if
      end if
      
      eta2 = C+D*xi2
      if ( abs(eta2).gt.dtol ) then
         eta2 = (xp-A-B*xi2)/eta2
      else
         eta2 = G + H*xi2
         if ( abs(eta2).gt.dtol ) then
            eta2 = (yp-E-F*xi2)/ eta2
         else
            eta2 = DMISS
         end if
      end if
      
!     determine zp
      do
!        try first (xi,eta)      
         if ( xi1.ne.DMISS .and. eta1.ne.DMISS ) then
            xp1 = A + B*xi1 + C*eta1 + D*xi1*eta1
            yp1 = E + F*xi1 + G*eta1 + H*xi1*eta1
            if ( abs(xp1-xp).lt.dtol .and. abs(yp1-yp).lt.dtol ) then
               zp = P + Q*xi1 + R*eta1 + S*xi1*eta1
               exit
            end if
         end if
         
!        try second (xi,eta)      
         if ( xi2.ne.DMISS .and. eta2.ne.DMISS ) then
            xp2 = A + B*xi2 + C*eta2 + D*xi2*eta2
            yp2 = E + F*xi2 + G*eta2 + H*xi2*eta2
            if ( abs(xp1-xp).lt.dtol .and. abs(yp1-yp).lt.dtol ) then
               zp = P + Q*xi2 + R*eta2 + S*xi2*eta2
               exit
            end if
         end if
         
!        no solution found         
         goto 1234
      end do
      
      ierror = 0
      
!     error handling
 1234 continue
      
      return
      end


 
 
 
 double precision function dprodin(x1,y1,x2,y2,x3,y3,x4,y4)    ! inner product of two segments
 use m_missing
 use m_sferic
 use geometry_module, only: getdx, getdy, sphertoCart3D
 implicit none
 double precision :: x1,y1,x2,y2,x3,y3,x4,y4
 double precision :: dx1,dy1,dx2,dy2
 
 double precision, dimension(4) :: xx, yy, zz
 double precision               :: dz1, dz2
 
 if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
    call sphertocart3D(x1, y1, xx(1), yy(1), zz(1))
    call sphertocart3D(x2, y2, xx(2), yy(2), zz(2))
    call sphertocart3D(x3, y3, xx(3), yy(3), zz(3))
    call sphertocart3D(x4, y4, xx(4), yy(4), zz(4))
    
    dx1 = xx(2)-xx(1)
    dy1 = yy(2)-yy(1)
    dz1 = zz(2)-zz(1)
    
    dx2 = xx(4)-xx(3)
    dy2 = yy(4)-yy(3)
    dz2 = zz(4)-zz(3)
    
    dprodin = dx1*dx2 + dy1*dy2 + dz1*dz2
 else

    dx1 = getdx(x1,y1,x2,y2,jsferic)
    dx2 = getdx(x3,y3,x4,y4,jsferic)
    
    dy1 = getdy(x1,y1,x2,y2,jsferic)
    dy2 = getdy(x3,y3,x4,y4,jsferic)
    
    dprodin = (dx1*dx2 + dy1*dy2)
 end if

 return
 end function dprodin

 subroutine sincosdis(x1,y1,x2,y2,s,c,d)    ! get sin, cos, length of a line segment
 use m_missing
 use m_sferic, only: jsferic
 use geometry_module, only: getdx, getdy
 implicit none
 double precision :: x1,y1,x2,y2,s,c,d
 double precision :: dx1,dy1,dx2,dy2

 dx1 = getdx(x1,y1,x2,y2,jsferic)
 dy1 = getdy(x1,y1,x2,y2,jsferic)
 d   = sqrt(dx1*dx1 + dy1*dy1)
 if (d > 0d0) then 
    s  = dy1/d
    c  = dx1/d
 else 
    s  = 0d0
    c  = 0d0
 endif
 end subroutine sincosdis 


      SUBROUTINE dLINEDIS2(X3,Y3,X1,Y1,X2,Y2,JA,DIS,XN,YN,rl)
      use m_sferic
      use geometry_module, only: getdx, getdy, dbdistance, sphertoCart3D, cart3Dtospher
      use m_missing, only: dmiss
      
      implicit none
      integer          :: ja
      DOUBLE PRECISION :: X1,Y1,X2,Y2,X3,Y3,DIS,XN,YN,ZN, d2
      DOUBLE PRECISION :: R2,RL,X21,Y21,Z21,X31,Y31,Z31
      DOUBLE PRECISION :: xx1,xx2,xx3,yy1,yy2,yy3,zz1,zz2,zz3,xxn,yyn,zzn
      !     korste afstand tot lijnelement

      JA  = 0 
      
      if (jsferic == 0 .or. jasfer3D == 0) then 
         
         X21 = getdx(x1,y1,x2,y2,jsferic)
         Y21 = getdy(x1,y1,x2,y2,jsferic)
         X31 = getdx(x1,y1,x3,y3,jsferic)
         Y31 = getdy(x1,y1,x3,y3,jsferic)
         R2  = dbdistance(x2,y2,x1,y1,jsferic, jasfer3D, dmiss)
         R2  = R2*R2
         IF (R2 .NE. 0) THEN
            RL  = (X31*X21 + Y31*Y21) / R2
            IF (0d0 .LE. RL .AND. RL .LE. 1d0) then
               JA = 1
            endif
            XN  = X1 + RL*(x2-x1)
            YN  = Y1 + RL*(y2-y1)
            DIS = dbdistance(x3,y3,xn,yn,jsferic, jasfer3D, dmiss)
         else 
            DIS = dbdistance(x3,y3,x1,y1,jsferic, jasfer3D, dmiss)
         ENDIF
      
      else 
         
         call sphertocart3D(x1,y1,xx1,yy1,zz1)
         call sphertocart3D(x2,y2,xx2,yy2,zz2)
         call sphertocart3D(x3,y3,xx3,yy3,zz3)
         
         x21 = xx2-xx1
         y21 = yy2-yy1
         z21 = zz2-zz1
         x31 = xx3-xx1
         y31 = yy3-yy1
         z31 = zz3-zz1
         
         r2  = x21*x21 + y21*y21 + z21*z21      
         if (r2 .ne. 0d0) then 
            RL = (X31*X21 + Y31*Y21 + Z31*Z21) / R2
            IF (0d0 .LE. RL .AND. RL .LE. 1d0) then
               JA = 1
            endif
            XXN  = xx1 + RL*x21 
            YYN  = yy1 + RL*y21
            ZZN  = zz1 + RL*z21
            x31 = xxn-xx3
            y31 = yyn-yy3
            z31 = zzn-zz3
            DIS = sqrt(x31*x31 + y31*y31 + z31*z31)
            
            call Cart3Dtospher(xxn,yyn,zzn,xn,yn,maxval((/x1,x2,x3/)))
         else   
            DIS = dbdistance(x3,y3,x1,y1, jsferic, jasfer3D, dmiss)
         endif   
         
      endif   
      
      RETURN
      END subroutine DLINEDIS2
      
      subroutine dlinedis3D(xx3,yy3,zz3,xx1,yy1,zz1,xx2,yy2,zz2,JA,DIS,xxn,yyn,zzn,rl)
      implicit none
      integer          :: ja
      DOUBLE PRECISION :: DIS,XN,YN,ZN, d2
      DOUBLE PRECISION :: R2,RL,X21,Y21,Z21,X31,Y31,Z31
      DOUBLE PRECISION :: xx1,xx2,xx3,yy1,yy2,yy3,zz1,zz2,zz3,xxn,yyn,zzn

      !     korste afstand tot lijnelement

      JA  = 0 
         
      x21 = xx2-xx1
      y21 = yy2-yy1
      z21 = zz2-zz1
      x31 = xx3-xx1
      y31 = yy3-yy1
      z31 = zz3-zz1
      
      r2  = x21*x21 + y21*y21 + z21*z21      
      if (r2 .ne. 0d0) then 
         RL = (X31*X21 + Y31*Y21 + Z31*Z21) / R2
         IF (0d0 .LE. RL .AND. RL .LE. 1d0) then
            JA = 1
         endif
         XXN  = xx1 + RL*x21 
         YYN  = yy1 + RL*y21
         ZZN  = zz1 + RL*z21
         x31 = xxn-xx3
         y31 = yyn-yy3
         z31 = zzn-zz3
         DIS = sqrt(x31*x31 + y31*y31 + z31*z31)
      else   
         DIS = 0d0
      endif   
      
      RETURN
      
      end subroutine dlinedis3D

      
!>    transform global spherical coordinates (xglob,yglob) to local coordinates (xloc,yloc) around reference point (xref,yref)
      subroutine spher2loc(xref,yref,N,xglob,yglob,xloc,yloc)
         use m_sferic
         use m_missing, only: dmiss
         use geometry_module, only: sphertocart3D, cart3Dtospher
         implicit none
         
         double precision,               intent(in)  :: xref,  yref    !< global coordinates of reference point (longitude, latitude)
         integer,                        intent(in)  :: N              !< number of global coordinates
         double precision, dimension(N), intent(in)  :: xglob, yglob   !< global coordinates, (longitude, latitude)
         double precision, dimension(N), intent(out) :: xloc,  yloc    !< local coordinates
         
         double precision, dimension(3)              :: exxp, eyyp, ezzp   ! base vectors of rotated 3D Cartesian reference frame
         
         double precision                            :: xx, yy, zz     !  3D Cartesian coordinates
         double precision                            :: xxp, yyp, zzp  !  3D Cartesian coordinates in rotated frame
         
         double precision                            :: phi0, lambda0
         
         integer                                     :: i
         
         if ( jsferic.eq.0 .or. jasfer3D.eq.0 ) then
            do i=1,N
               xloc(i) = xglob(i)-xref
               yloc(i) = yglob(i)-yref
            end do
            
         else
            phi0 = yref*dg2rd
            lambda0 = xref*dg2rd
            
!           compute base vectors
            exxp = (/  cos(phi0) * cos(lambda0),  cos(phi0) * sin(lambda0), sin(phi0) /)
            eyyp = (/             -sin(lambda0),              cos(lambda0), 0d0       /)
            ezzp = (/ -sin(phi0) * cos(lambda0), -sin(phi0) * sin(lambda0), cos(phi0) /)
            
            do i=1,N
!              get 3D-coordinates
               call sphertocart3D(xglob(i),yglob(i),xx,yy,zz)
               
!              project to rotated frame
               xxp = exxp(1) * xx + exxp(2) * yy + exxp(3) * zz
               yyp = eyyp(1) * xx + eyyp(2) * yy + eyyp(3) * zz
               zzp = ezzp(1) * xx + ezzp(2) * yy + ezzp(3) * zz
               
!              tranform to local spherical coordinates
               call Cart3Dtospher(xxp,yyp,zzp,xloc(i),yloc(i),0d0)   ! local reference longitude
            end do
         
         end if
      
         return
      end subroutine spher2loc
      
      
!>    transform local spherical coordinates (xloc,yloc) around reference point (xref,yref) to global spherical coordinates (xglob,yglob)
      subroutine loc2spher(xref,yref,N,xloc,yloc,xglob,yglob)
         use m_sferic
         use m_missing, only: dmiss
         use geometry_module, only: sphertocart3D, cart3Dtospher
         implicit none
         
         double precision,               intent(in)  :: xref,  yref    !< global coordinates of reference point (longitude, latitude)
         integer,                        intent(in)  :: N              !< number of global coordinates
         double precision, dimension(N), intent(in)  :: xloc,  yloc    !< local coordinates
         double precision, dimension(N), intent(out) :: xglob, yglob   !< global coordinates, (longitude, latitude)
         
         double precision, dimension(3)              :: exxp, eyyp, ezzp   ! base vectors of rotated 3D Cartesian reference frame
         
         double precision                            :: xx, yy, zz     !  3D Cartesian coordinates
         double precision                            :: xxp, yyp, zzp  !  3D Cartesian coordinates in rotated frame
         
         double precision                            :: phi0, lambda0
         
         integer                                     :: i
         
         if ( jsferic.eq.0 .or. jasfer3D.eq.0 ) then
            do i=1,N
               xglob(i) = xloc(i)+xref
               yglob(i) = yloc(i)+yref
            end do
            
         else
            phi0 = yref*dg2rd
            lambda0 = xref*dg2rd
            
!           compute base vectors
            exxp = (/  cos(phi0) * cos(lambda0),  cos(phi0) * sin(lambda0), sin(phi0) /)
            eyyp = (/             -sin(lambda0),              cos(lambda0), 0d0       /)
            ezzp = (/ -sin(phi0) * cos(lambda0), -sin(phi0) * sin(lambda0), cos(phi0) /)
            
            do i=1,N
!              get 3D-coordinates in rotated frame
               call sphertocart3D(xloc(i),yloc(i),xxp,yyp,zzp)
               
!              project to fixed frame
!               xxp = exxp(1) * xx + exxp(2) * yy + exxp(3) * zz
!               yyp = eyyp(1) * xx + eyyp(2) * yy + eyyp(3) * zz
!               zzp = ezzp(1) * xx + ezzp(2) * yy + ezzp(3) * zz
               
               xx = exxp(1) * xxp + eyyp(1) * yyp + ezzp(1) * zzp
               yy = exxp(2) * xxp + eyyp(2) * yyp + ezzp(2) * zzp
               zz = exxp(3) * xxp + eyyp(3) * yyp + ezzp(3) * zzp
               
!              tranform to spherical coordinates
               call Cart3Dtospher(xx,yy,zz,xglob(i),yglob(i),xref)
            end do
         
         end if
      
         return
      end subroutine loc2spher
      
   
      
!>    return x-component in link coordinate frame of vector in node coordinate frame
      double precision function nod2linx(L,i12,ux,uy)
         use m_flowgeom, only: csb, snb
         use m_sferic
         implicit none
         
         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: i12 !< left (1) or right (2) neighboring cell
         double precision, intent(in) :: ux, uy !< vector components in flowlnode coordinate frame
         
         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            nod2linx = ux
         else
            nod2linx =  csb(i12,L) * ux + snb(i12,L) * uy
         end if
         
         return
      end function nod2linx
      
!>    return y-component in link coordinate frame of a vector in node coordinate frame
      double precision function nod2liny(L,i12,ux,uy)
         use m_flowgeom, only: csb, snb
         use m_sferic
         implicit none
         
         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: i12 !< left (1) or right (2) neighboring cell
         double precision, intent(in) :: ux, uy !< vector components in flownode coordinate frame

         
         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            nod2liny = uy
         else
            nod2liny =  -snb(i12,L) * ux + csb(i12,L) * uy
         end if
         
         return
      end function nod2liny
      
      
!>    return x-component in node coordinate frame of a vector in link coordinate frame
      double precision function lin2nodx(L,i12,ux,uy)
         use m_flowgeom, only: csb, snb
         use m_sferic
         implicit none
         
         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: i12 !< left (1) or right (2) neighboring cell
         double precision, intent(in) :: ux, uy !< vector components in flowlink coordinate frame
         
         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            lin2nodx = ux
         else
            lin2nodx =  csb(i12,L) * ux - snb(i12,L) * uy
         end if
         
         return
      end function lin2nodx
      
!>    return y-component in node coordinate frame of a vector in link coordinate frame
      double precision function lin2nody(L,i12,ux,uy)
         use m_flowgeom, only: csb, snb
         use m_sferic
         implicit none
         
         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: i12 !< left (1) or right (2) neighboring cell
         double precision, intent(in) :: ux, uy !< vector components in flowlink coordinate frame
         
         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            lin2nody = uy
         else
            lin2nody =  snb(i12,L) * ux + csb(i12,L) * uy
         end if
         
         return
      end function lin2nody
      
      
!>    return x-component in link coordinate frame of vector in "klnup"-node coordinate frame
      double precision function nodup2linx(L,ib,ux,uy)
         use m_flowgeom, only: csbup, snbup
         use m_sferic
         implicit none
         
         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: ib !< stencil index  (1 (iup=1), 2 (iup=2), 3 (iup=4), or 4 (iup=5))
         double precision, intent(in) :: ux, uy !< vector components in flownode coordinate frame

         
         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            nodup2linx = ux
         else      
            nodup2linx =  csbup(ib,L) * ux + snbup(ib,L) * uy
         end if
         
         return
      end function nodup2linx    


!>    return y-component in link coordinate frame of vector in "klnup"-node coordinate frame
      double precision function nodup2liny(L,ib,ux,uy)
         use m_flowgeom, only: csbup, snbup
         use m_sferic
         implicit none
         
         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: ib !< stencil index (1 (iup=1), 2 (iup=2), 3 (iup=4), or 4 (iup=5))
         double precision, intent(in) :: ux, uy !< vector components in flownode coordinate frame

         
         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            nodup2liny = uy
         else
            nodup2liny = -snbup(ib,L) * ux + csbup(ib,L) * uy
         end if
         
         return
      end function nodup2liny
      
!>    return x-component in corner (netnode) coordinate frame of a vector in link coordinate frame
      double precision function lin2corx(L,i12,ux,uy)
         use m_flowgeom, only: csbn, snbn
         use m_sferic
         implicit none
         
         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: i12 !< left (1) or right (2) corner (netnode)
         double precision, intent(in) :: ux, uy !< vector components in flowlink coordinate frame
         
         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            lin2corx = ux
         else
            lin2corx =  csbn(i12,L) * ux - snbn(i12,L) * uy
         end if
         
         return
      end function lin2corx
      
!>    return x-component in corner (netnode) coordinate frame of a vector in link coordinate frame
      double precision function lin2cory(L,i12,ux,uy)
         use m_flowgeom, only: csbn, snbn
         use m_sferic
         implicit none
         
         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: i12 !< left (1) or right (2) corner (netnode)
         double precision, intent(in) :: ux, uy !< vector components in flowlink coordinate frame
         
         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            lin2cory = uy
         else
            lin2cory =  snbn(i12,L) * ux + csbn(i12,L) * uy
         end if
         
         return
      end function lin2cory
      
!>    return x-component in link coordinate frame of vector in corner (netnode) coordinate frame
      double precision function cor2linx(L,i12,ux,uy)
         use m_flowgeom, only: csbn, snbn
         use m_sferic
         implicit none
         
         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: i12 !< left (1) or right (2) neighboring corner (netnode)
         double precision, intent(in) :: ux, uy !< vector components in corner coordinate frame

         
         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            cor2linx = ux
         else
            cor2linx =  csbn(i12,L) * ux + snbn(i12,L) * uy
         end if
         
         return
      end function cor2linx
      
!>    return y-component in link coordinate frame of a vector in corner (netnode) coordinate frame
      double precision function cor2liny(L,i12,ux,uy)
         use m_flowgeom, only: csbn, snbn
         use m_sferic
         implicit none
         
         integer,          intent(in) :: L   !< flowlink number
         integer,          intent(in) :: i12 !< left (1) or right (2) neighboring corner (netnode)
         double precision, intent(in) :: ux, uy !< vector components in corner coordinate frame
         
         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            cor2liny = uy
         else
            cor2liny = -snbn(i12,L) * ux + csbn(i12,L) * uy
         end if
         
         return
      end function cor2liny
      
!>    return x-component in link coordinate frame of vector in wall coordinate frame
      double precision function wall2linx(nw,i12,ux,uy)
         use m_flowgeom, only: csbw, snbw
         use m_sferic
         implicit none
         
         integer,          intent(in) :: nw  !< wall element number
         integer,          intent(in) :: i12 !< left (1) or right (2) attached flowlink
         double precision, intent(in) :: ux, uy !< vector components in wall coordinate frame
 
         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            wall2linx = ux
         else
            wall2linx =  csbw(i12,nw) * ux - snbw(i12,nw) * uy
         end if
         
         return
      end function wall2linx   
      
!>    return y-component in link coordinate frame of vector in wall coordinate frame
      double precision function wall2liny(nw,i12,ux,uy)
         use m_flowgeom, only: csbw, snbw
         use m_sferic
         implicit none
         
         integer,          intent(in) :: nw  !< wall element number
         integer,          intent(in) :: i12 !< left (1) or right (2) attached flowlink
         double precision, intent(in) :: ux, uy !< vector components in wall coordinate frame
        
         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            wall2liny = uy
         else
            wall2liny =  snbw(i12,nw) * ux + csbw(i12,nw) * uy
         end if 
         
         return
      end function wall2liny
      
      
!>    return x-component in link coordinate frame of vector in wall coordinate frame
      double precision function nod2wallx(nw,ux,uy)
         use m_flowgeom, only: csbwn, snbwn
         use m_sferic
         implicit none
         
         integer,          intent(in) :: nw  !< wall element number
         double precision, intent(in) :: ux, uy !< vector components in wall coordinate frame

         integer                      :: L
         
         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            nod2wallx = ux
         else
            nod2wallx =  csbwn(nw) * ux + snbwn(nw) * uy
         end if
         
         return 
      end function nod2wallx
      
      
!>    return x-component in link coordinate frame of vector in wall coordinate frame
      double precision function nod2wally(nw,ux,uy)
         use m_flowgeom, only: csbwn, snbwn
         use m_sferic
         implicit none
         
         integer,          intent(in) :: nw  !< wall element number
         double precision, intent(in) :: ux, uy !< vector components in wall coordinate frame
         
         if ( jsferic.ne.1 .or. jasfer3D.ne.1 ) then
            nod2wally = uy
         else
            nod2wally = -snbwn(nw) * ux + csbwn(nw) * uy
         end if
         
         return
      end function nod2wally
   
      SUBROUTINE dLINEDIS3(X3,Y3,X1,Y1,X2,Y2,JA,DIS,XN,YN, RLOUT)  ! 3: SORRY
      use geometry_module, only: getdx, getdy, dbdistance, sphertocart3D, Cart3Dtospher
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      integer          :: ja
      DOUBLE PRECISION :: X1,Y1,X2,Y2,X3,Y3,DIS,XN,YN
      DOUBLE PRECISION :: xx1,xx2,xx3,yy1,yy2,yy3,zz1,zz2,zz3,xxn,yyn,zzn
      DOUBLE PRECISION :: R2,RL,X21,Y21,Z21,X31,Y31,Z31

      double precision :: RLout  ! needed in orthogonalisenet/projection of boundary nodes
                                 ! korste afstand tot lijnelement tussen eindpunten
      JA  = 0
      
      if ( jsferic.eq.0 .or. jasfer3D.eq.0 ) then
         X21 = getdx(x1,y1,x2,y2,jsferic)
         Y21 = getdy(x1,y1,x2,y2,jsferic)
         X31 = getdx(x1,y1,x3,y3,jsferic)
         Y31 = getdy(x1,y1,x3,y3,jsferic)
         R2  = dbdistance(x2,y2,x1,y1,jsferic, jasfer3D, dmiss)
         R2  = R2*R2
         RLout = 0d0
         IF (R2 .NE. 0) THEN
            RL  = (X31*X21 + Y31*Y21) / R2
            RLout = RL
            RL  = MAX( MIN(1d0,RL) , 0d0)
            JA  = 1
            XN  = X1 + RL*(x2-x1)
            YN  = Y1 + RL*(y2-y1)
            DIS = dbdistance(x3,y3,xn,yn,jsferic, jasfer3D, dmiss)
         ENDIF
      else
         
         call sphertocart3D(x1,y1,xx1,yy1,zz1)
         call sphertocart3D(x2,y2,xx2,yy2,zz2)
         call sphertocart3D(x3,y3,xx3,yy3,zz3)
         
         x21 = xx2-xx1
         y21 = yy2-yy1
         z21 = zz2-zz1
         x31 = xx3-xx1
         y31 = yy3-yy1
         z31 = zz3-zz1
         
         r2  = x21*x21 + y21*y21 + z21*z21    
         RLout = 0d0  
         if (r2 .ne. 0d0) then 
            RL = (X31*X21 + Y31*Y21 + Z31*Z21) / R2
            RLout = RL
            RL  = MAX( MIN(1d0,RL) , 0d0)
            JA = 1
            XXN  = xx1 + RL*x21 
            YYN  = yy1 + RL*y21
            ZZN  = zz1 + RL*z21
            x31 = xxn-xx3
            y31 = yyn-yy3
            z31 = zzn-zz3
            DIS = sqrt(x31*x31 + y31*y31 + z31*z31)
            
            call Cart3Dtospher(xxn,yyn,zzn,xn,yn,maxval((/x1,x2,x3/)))
         endif   
      
      end if
      RETURN
      END subroutine DLINEDIS3


 !> Computes the enclosed area and length of a polygon.
 !!
 !! Only the first polygon is considered; whenever a missing value
 !! is encountered, the polygon is 'closed'.
 SUBROUTINE dAREAN( XX, YY, N, DAREA, DLENGTH, DLENMX )
 USE m_missing
 use m_sferic
 use geometry_module, only: dbdistance, get_startend, comp_masscenter
 
 implicit none
 double precision, intent(in)  :: XX(N), YY(N) !< Polygon points.
 double precision, intent(out) :: DAREA   !< Area enclosed within polygon.
 double precision, intent(out) :: DLENGTH !< Length of polygon contour.
 double precision, intent(out) :: DLENMX  !< Length of longest segment in polygon contour.
 integer,          intent(in)  :: n       !< Nr. of polygon points.

 integer :: i, iu, nend, jstart, jend
 double precision :: DX, DY, Y0, DLE, Y
 double precision :: xcg, ycg
 integer :: jacounterclockwise
   DAREA   = 0d0
   DLENGTH = 0D0
   Y0      = 1d30
   NEND    = 0
   DLENMX  = 0.D0

   call get_startend(N,XX,YY,jstart,jend,dmiss)
   
   if ( jend.le.jstart ) return
   
   call comp_masscenter(jend-jstart+1, xx(jstart), yy(jstart), xcg, ycg, darea, jacounterclockwise, jsferic, jasfer3D, dmiss)

   !DO I  = jstart,jend
   !   IF (XX(I) .NE.  dXYMIS) THEN
   !      Y0   = MIN(Y0,YY(I))
   !      NEND = I
   !   ELSE
   !      ! dmiss encountered: end of first polygon.
   !      ! Only compute area for first polygon.
   !      EXIT
   !   ENDIF
   !ENDDO
   !
   DO I = jstart,jend
      IU = I + 1
      
      if ( iu.gt.jend ) iu=jstart
      
 !     IF (IU .GT. NEND) IU = 1
 !     IF (JSFERIC .EQ. 0) THEN
 !        DX    = ( XX(IU) - XX(I) )
 !        Y     = 0.5d0*(YY(IU)-Y0) + 0.5d0*(YY(I) -Y0)
 !     ELSE
 !        DX    = ( XX(IU) - XX(I) )*DG2RD*RA*COS( DG2RD*( YY(IU)+YY(I) )/2 )
 !        Y     = RA*DG2RD*(0.5d0*( YY(IU) + YY(I) )-Y0)
 !     ENDIF
 !     DAREA    = DAREA - DX*Y
      DLE      = DBDISTANCE( XX(I), YY(I), XX(IU), YY(IU), jsferic, jasfer3D, dmiss)
      DLENGTH  = DLENGTH + DLE
      DLENMX   = MAX (DLENMX, DLE)
   
   ENDDO
   !
   !DAREA = ABS(DAREA)
 RETURN
 END SUBROUTINE dAREAN


   
!> Write tecplot output in already opened file
!>   note: file is closed when tim==tstop
   subroutine tecplot_out(mtecfil, tim, Lwriheader)

   use m_flowgeom
   use m_flow
   use m_flowtimes
   use m_netw
   implicit none

   integer                      :: mtecfil
   double precision, intent(in) :: tim
   logical,          intent(in) :: Lwriheader

   integer               :: i, j, k, icell
   integer, dimension(4) :: inode
   double precision, dimension(:), allocatable :: s1k, a1k, hk
   character(len=256)    :: zone

! Write header
   if ( Lwriheader ) then
      write(mtecfil, '(a)') 'Title = "output"'
      write(mtecfil, '(a)') 'Variables = x y z e h u v'
   end if
   
   ! Write cell centred data

!   write (mtecfil, *) 'Zone f=point, t="', trim(zone),'"', &
!        &      ', i=', ndx
!   do i = 1, ndx
!      write (mtecfil, '(3f18.7)') xz(i), yz(i), s1(i) !, u1(inod)
!   enddo

   ! Write net node data

   ! Interpolate s1 to net nodes

   allocate(s1k(1:numk))
   allocate(a1k(1:numk))
   allocate(hk (1:numk))
   s1k = 0.0d0
   a1k = 0.0d0
   hk  = 0.0d0
   
   ! Loop over flow nodes (flow cells/elements) and over all its net nodes (corners) = expensive!
   ! Weighted interpolation based on cell area

   do i = 1, ndxi
      do j = 1, netcell(i)%N
         k = netcell(i)%nod(j)
         s1k(k) = s1k(k) + a1(i)*s1(i)
         a1k(k) = a1k(k) + a1(i)
      enddo
   enddo
   
   ! Scale the interpolated water levels
   
   s1k = s1k / a1k

   ! Compute total depth in net nodes

   do i = 1, numk
      hk(i) = max(0.0d0, s1k(i) - zk(i))
   enddo
   
   ! Compute velocities to make sure the velocities in net nodes (corner points) have been computed

   call setvelocityfield()

   !
   ! Write zone information
   !
   write (zone  , '(f16.4)'      ) tim
   write (mtecfil, '(3a)'         ) 'Zone T ="', trim(zone),'"' !, numk
   write (mtecfil, '(a,i0,a,i0,a)') 'N=', numk, ', E=', ndxi,', F=FEPOINT ET=QUADRILATERAL'

   do i = 1, numk
      write (mtecfil, '(7f18.7)') xk(i), yk(i), zk(i), s1k(i), hk(i), ucnx(i), ucny(i) !, u1k(i)
   enddo

   ! Write connectivity table to file

   do icell = 1, ndxi
      do j=1,3
         inode(j) = netcell(icell)%nod(j)
      enddo
      if (netcell(icell)%N == 3) then
         inode(4) = netcell(icell)%nod(1)
      else
         inode(4) = netcell(icell)%nod(4)
      endif
      write (mtecfil, '(4(i0,3x))') inode(1), inode(2), inode(3), inode(4)
   enddo
   
end subroutine tecplot_out

subroutine timdat(julday, timsec, idatum, itijd)
!!--description-----------------------------------------------------------------
!
!    Function:  returns date and time according actual time
!               in minutes and itdate (julian day)
!               in the form yyyymmdd  hhmmss
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Global variables
!
    integer               :: idatum !  Absolute date related to ITDATE and TIMSEC
    integer , intent(out) :: itijd  !  Absolute time related to ITDATE and TIMSEC
    integer , intent(in)  :: julday !  Description and declaration in inttim.igs
    real(fp), intent(in)  :: timsec !  Description and declaration in inttim.igs
!
! Local variables
!
    integer(long) :: icurtm
    integer       :: iday
    integer       :: ihou
    integer       :: imin
    integer       :: imo
    integer       :: isec
    integer       :: iy
    integer       :: l
    integer       :: n
!
!! executable statements -------------------------------------------------------
!
    ! Define number of days, hours, minutes and seconds from TIMSEC
    !
    icurtm = nint(timsec,long)
    iday   = int(icurtm/86400_long)
    icurtm = icurtm - int(iday,long)*86400_long
    ihou   = int(icurtm)/3600
    icurtm = icurtm - int(ihou,long)*3600_long
    imin   = int(icurtm)/60
    icurtm = icurtm - int(imin,long)*60_long
    isec   = int(icurtm)
    !
    ! Convert julian day-number
    !
    l    = julday + iday + 68569
    n    = 4*l/146097
    l    = l - (146097*n + 3)/4
    iy   = 4000*(l + 1)/1461001
    l    = l - 1461*iy/4 + 31
    imo  = 80*l/2447
    iday = l - 2447*imo/80
    l    = imo/11
    imo  = imo + 2 - 12*l
    iy   = 100*(n - 49) + iy + l
    !
    ! Define integer values for time and date
    !
    itijd  = ihou*10000 + imin*100 + isec
    idatum = abs(iy)*10000 + imo*100 + iday
    idatum = sign(idatum, iy)
end subroutine timdat

  !> Parses a manually preprocessed SVG file into 1D network 'drawing'.
    !!
    !! Format: each line should have one SVG command (m/M/c/l/z) with coordi nates.
    subroutine parsekerst(filename)
    use m_polygon
    use m_missing
    use unstruc_messages
    use network_data
    use gridoperations
    use unstruc_display, only:minmxns
    use m_wearelt, only: rcir
   
    implicit none
   
    character(len=*), intent(in) :: filename
    integer :: mfil, i, maxpts, it, maxt, KP, K1, LNU
    character(len=2000) :: line
    real, allocatable :: pts(:)
    real :: startx, starty, curx, cury, x, y, t
    double precision :: zp
    
    maxpts = 200
    allocate(pts(maxpts))

    curx = 0.0
    cury = 0.0
    open(mfil, file=trim(filename))
    
    do
        read(mfil,'(a)',end=999) line
        select case (line(1:1))
        case ('c')
            pts = dmiss
            read(line(3:), *, end=97) pts
97          continue

            do i = 1,maxpts-5,6
                if (pts(i) == dmiss .or. pts(i+1) == dmiss) exit

                !curx = curx + pts(i+4) ! Just take endpoint of bezier curve
                !cury = cury + pts(i+5)
                !npl = npl+1
                !call increasepol(npl, 1)
                !xpl(npl) = curx
                !ypl(npl) = cury
                maxt=5
                do it=1,maxt
                    t = real(it)/real(maxt)
                    x =            (1.0-t)**3 *        (curx) &
                             + 3.0*(1.0-t)**2 * t    * (curx+pts(i)) &
                             + 3.0*(1.0-t)    * t**2 * (curx+pts(i+2)) &
                             +                  t**3 * (curx+pts(i+4))
                    y =            (1.0-t)**3 *        (cury) &
                             + 3.0*(1.0-t)**2 * t    * (cury+pts(i+1)) &
                             + 3.0*(1.0-t)    * t**2 * (cury+pts(i+3)) &
                             +                  t**3 * (cury+pts(i+5))

                    npl = npl+1
                    call increasepol(npl, 1)
                    xpl(npl) = x
                    ypl(npl) = y
                end do
                curx = x
                cury = y
            end do

        case ('m','M') ! Move to new location
            pts = dmiss
            read(line(3:), *, end=98) pts
98          continue
            if (line(1:1) == 'M') then
                curx = pts(1)
                cury = pts(2)
            else
                curx = curx + pts(1)
                cury = cury + pts(2)
            end if
            startx = curx
            starty = cury
            npl = npl+1
            call increasepol(npl+1, 1)
            xpl(npl) = dmiss
            ypl(npl) = dmiss
            npl = npl+1
            xpl(npl) = startx
            ypl(npl) = starty
            ! If more than one point was given, treat as implicit subsequent lineto commands
            do i = 3,maxpts-1,2
                if (pts(i) == dmiss .or. pts(i+1) == dmiss) exit

                npl = npl+1
                call increasepol(npl, 1)
                if (line(1:1) == 'M') then
                    curx = pts(i)
                    cury = pts(i+1)
                else
                    curx = curx + pts(i)
                    cury = cury + pts(i+1)
                end if
                xpl(npl) = curx
                ypl(npl) = cury
            end do
        case ('z') ! Close current subpath, return to subpath start
            npl = npl+1
            call increasepol(npl, 1)
            xpl(npl) = startx
            ypl(npl) = starty
            curx = startx
            cury = starty
        case ('l')
            pts = dmiss
            read(line(3:), *, end=99) pts
99          continue

            do i = 1,maxpts-1,2
                if (pts(i) == dmiss .or. pts(i+1) == dmiss) exit

                npl = npl+1
                call increasepol(npl, 1)
                curx = curx + pts(i)
                cury = cury + pts(i+1)
                xpl(npl) = curx
                ypl(npl) = cury
            end do
        case default
!            call mess('Unrecognized SVG command: '//trim(line))
        end select    
    end do
999 continue

    do i=1,npl
        ypl(i) = -ypl(i)  
    end do
    
    call zeronet()
    KN3TYP=1
    K1 = 0
    rcir = 1d-10 ! isnode only for 'exact' hits
    do i=1,npl
        if (XPL(i) == dmiss) then
            K1 = 0
            cycle
        end if
        zp = 0d0
        CALL ISNODE(KP, XPL(i), YPL(i), zp)
        IF (KP .EQ. 0) THEN
            CALL SETNEWPOINT(XPL(i), YPL(i), zp,KP)
        ENDIF
        IF (K1 .NE. 0) THEN
            CALL CONNECTDBN(K1,KP,LNU)
        END IF
        K1=KP
    end do
    KN3TYP=2
    call savepol() ! Store pol for optional restore
    call delpol()  ! Delete/hide the pol read from SVG file
    CALL MINMXNS() ! New world scaling
    !call wearel()
    !CALL MERGENODESINPOLYGON() ! Merge 1D nodes that are too close.
    
   deallocate(pts)
   
   end subroutine parsekerst
   
   
   
 subroutine getmdia(mdi) ! thanks herman
 use unstruc_files
 implicit none
 integer :: mdi
 
 mdi = mdia 
 
 end subroutine getmdia  
 
 
 subroutine setmdia(mdi) ! thanks herman, again
 use unstruc_files
 implicit none
 integer :: mdi
 
 mdia = mdi 
 
 end subroutine setmdia


!> make directory (also for linux)
 subroutine makedir(dirname)
#ifdef __INTEL_COMPILER
   use ifport
#endif
    implicit none
    character(len=*), intent(in) :: dirname
    
    character(len=256)           :: command
    character(len=1), external   :: get_dirsep
    integer                      :: istat
    logical                      :: l_exist
    integer                      :: i
    character(len=256)           :: dirnamewin

!    write(6,"('Creating directory ', A128)") trim(dirname)
    
#ifdef __INTEL_COMPILER
    inquire(directory = trim(dirname), exist = l_exist)
#else
    ! GNU
    inquire(file = trim(dirname)//get_dirsep()//".", exist = l_exist)
#endif
    if (l_exist) then
       return
    end if

    if ( get_dirsep().eq.'/' ) then
!     linux
      command = "mkdir -p "//trim(dirname)
    else
!     windows
       dirnamewin = trim(dirname)
       do i = 1,len(dirnamewin)
          if( dirnamewin(i:i) == '/' ) dirnamewin(i:i) = '\'
       enddo
       command = "mkdir "//trim(dirnamewin)
       ! call iosDirMAKE(dirname)
    end if

    istat = system(command)
    ! Fortran2008, not available before Intel 15:
    ! call execute_command_line(command)
    
    return
 end subroutine
 

      SUBROUTINE TIMLIN0()
      implicit none
      RETURN
      END
 
      SUBROUTINE FIRSTLIN(MRGF)
      use unstruc_version_module, only: unstruc_version_full, get_unstruc_source
      implicit none
      integer :: mrgf

      CHARACTER TEX*255, RUNDAT*20
      CALL DATUM(RUNDAT)
      WRITE(MRGF,'(A)') '* '//trim(unstruc_version_full)
      call get_unstruc_source(TEX)
      WRITE(MRGF,'(A)') '* Source: '//trim(TEX)
      TEX = '* File creation date: ' //RUNDAT
      WRITE(MRGF,'(A)') trim(TEX)

      RETURN
      END



!---------------------------------------------------------------
! the following subroutines use kdtree2
!---------------------------------------------------------------
   
!> find links crossed by polyline with kdtree2
   subroutine find_crossed_links_kdtree2(treeinst,NPL,xpl,ypl,itype,nLinks,jaboundarylinks,numcrossedLinks, iLink, iPol, dSL, ierror)
      
      !use m_polygon
      use network_data, only: nump, numL, kn, xk, yk, lnn,lne
      use m_flowgeom
      use kdtree2Factory
      use m_sferic
      use unstruc_messages
      use m_missing
      use m_alloc
      use geometry_module, only: dbdistance, crossinbox
      
      implicit none
      
      type(kdtree_instance),               intent(inout) :: treeinst
      integer,                             intent(in)    :: NPL                !< polyline length
      double precision, dimension(NPL),    intent(in)    :: xpl, ypl           !< polyline node coordinates
      integer,                             intent(in)    :: itype              !< netlinks (1: cross with dual link, 3: cross with netlink itself) or flowlinks(2)
      integer,                             intent(in)    :: nLinks             !< number of links ( Lnx for flowlinks, numL for netlinks)
      integer,                             intent(in)    :: jaboundarylinks    !< include boundary links (1) or not (0), flowlinks only
      integer,                             intent(out)   :: numcrossedLinks    !< number of crossed flowlinks
      integer,          dimension(nLinks), intent(inout) :: iLink              !< crossed flowlinks
      integer,          dimension(nLinks), intent(inout) :: iPol               !< polygon section
      double precision, dimension(nLinks), intent(inout) :: dSL                !< polygon section cross location
      integer,                             intent(out)   :: ierror             !< ierror (1) or not (0)

      double precision, dimension(:),       allocatable  :: x, y      
   
      integer,          dimension(:),       allocatable  :: ipolsection
      double precision                                   :: dmaxpollen, dmaxLinlen, dlinlen, R2search
      integer                                            :: num
      integer,                              parameter    :: jakdtree=1
      integer,                              parameter    :: MAXFIND=100
      integer,                              parameter    :: MINTREESIZE=0
         
      double precision                                   :: SL, SM, XCR, YCR, CRP
      double precision                                   :: xa, ya, xb, yb, af, d
      integer                                            :: i, k, L, N1, N2, NN, numnew
      integer                                            :: jacros, kint
      integer                                            :: LnxiORLnx
      integer                                            :: isactive      
      double precision                                   :: dtol

      if ( janeedfix.eq.1 ) then
         dtol = 1d-8
      else
         dtol = 0d0
      end if
      
      ierror          = 1

      numcrossedLinks = 0
      
      if ( NPL.lt.1 ) goto 1234  ! nothing to do

      LnxiORLnx = 0

      if ( itype.eq.1 .or. itype.eq.3 ) then  ! netlinks
         LnxiORLnx = numL
      else if ( itype.eq.2 ) then   ! flowlinks
         if ( jaboundarylinks.eq.1 ) then
            LnxiORLnx = Lnx
         else
            LnxiORLnx = Lnxi
         end if
      end if

!     allocate
      allocate(ipolsection(NPL-1))
      allocate(treeinst%qv(NTREEDIM))
    
!     determine maximum polygon section length, and administer polygon sections
      dmaxpollen = 0d0
      num = 0
      do i=1,NPL-1
         if ( xpl(i).ne.DMISS .and. xpl(i+1).ne.DMISS ) then
            num = num+1
            ipolsection(num) = i
            dmaxpollen = max(dmaxpollen, dbdistance(xpl(i),ypl(i),xpl(i+1),ypl(i+1),jsferic, jasfer3D, dmiss))
         end if
      end do

!     check tree size and exit if the tree is too small
      if ( num.lt.MINTREESIZE ) then
         goto 1234
      end if
   
!     build kdtree
      allocate(treeinst%sample_coords(NTREEDIM,NPL-1))
   
      NN = min(MAXFIND,num)
   
!     allocate
      allocate(x(num), y(num))
   
!     fill coordinates
      if ( janeedfix.eq.1 ) then
         do k=1,num
            i=ipolsection(k)
            call random_number(d)
            x(k) = xpl(i) + dtol*d
            call random_number(d)
            y(k) = ypl(i) + dtol*d
         end do
      else
         do k=1,num
            i=ipolsection(k)
            x(k) = xpl(i)
            y(k) = ypl(i)
         end do
      end if

      call build_kdtree(treeinst,num, x, y, ierror, jsferic, dmiss)
      if ( ierror.ne.0 ) then
         goto 1234
      end if
         
!     find crossed flowlinks
      call mess(LEVEL_INFO, 'Finding crossed flowlinks...')   
      
      kint = max(LnxiORLnx/1000,1)
         
      do L=1,LnxiORLnx
         if (mod(L,kint) == 0) then
            af = dble(L)/dble(LnxiORLnx)
            call readyy('Finding crossed links', af)
!            write(6,"(F4.1, ' %')") af*100d0
         endif
         
            
         if ( itype.eq.1 ) then   ! netlinks, cross with dual links
            call get_link_neighboringcellcoords(L,isactive,xa,ya,xb,yb)
            if ( isactive.ne.1 ) then
               cycle
            end if
         else if ( itype.eq.2 ) then ! flowlinks
            n1 = ln(1,L) ; n2 = ln(2,L)
            xa = xz(n1)  ; ya = yz(n1)
            xb = xz(n2)  ; yb = yz(n2)
         else if ( itype.eq.3 ) then   ! netlinks, cross with netlinks
            n1 = kn(1,L)
            n2 = kn(2,L)
            xa = xk(n1)
            ya = yk(n1)
            xb = xk(n2)
            yb = yk(n2)
         end if
         
!        fill query vector
         call make_queryvector_kdtree(treeinst,xa,ya, jsferic)
         
!        compute flowlink length
         dlinlen = dbdistance(xa,ya,xb,yb, jsferic, jasfer3D, dmiss)
         
!        determine square search radius
         if ( jsferic.eq.0 ) then
            R2search = 1.1d0*(dlinlen+dmaxpollen+2d0*dtol)**2  ! 1.1d0: safety
         else
            R2search = 1.1d0*(dlinlen+dmaxpollen+2d0*dtol*Ra)**2  ! 1.1d0: safety
         end if
        
!        count number of points in search area
         NN = kdtree2_r_count(treeinst%tree,treeinst%qv,R2search)
        
         if ( NN.eq.0 ) cycle ! no links found
        
!        reallocate if necessary
         call realloc_results_kdtree(treeinst,NN)
     
!        find nearest NN points
         call kdtree2_n_nearest(treeinst%tree,treeinst%qv,NN,treeinst%results)
         
         jacros = 0
         do i=1,NN
            k = ipolsection(treeinst%results(i)%idx)
            CALL crossinbox (XPL(k), YPL(k), XPL(k+1), YPL(k+1), Xa, Ya, Xb, Yb, jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
               
            if ( jacros.eq.1 ) then
               numcrossedLinks = numcrossedLinks + 1
                  
               if ( numcrossedLinks.gt.ubound(iLink,1) ) then
                  call mess(LEVEL_ERROR, 'find_crossed_links_kdtree2: array size too small')
               end if
                  
               iLink(numcrossedLinks) = L
               iPol(numcrossedLinks)  = k
               dSL(numcrossedLinks)   = SL
            end if
         end do
      end do

      call readyy(' ', -1d0 )
         
      call mess(LEVEL_INFO, 'done')
         
      ierror = 0
 1234 continue

!     deallocate
      if ( treeinst%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(treeinst)
      if ( allocated(ipolsection) )   deallocate(ipolsection)
      if ( allocated(x) )             deallocate(x)
      if ( allocated(y) )             deallocate(y)
         
      return
   end subroutine find_crossed_links_kdtree2


!> find flow cells with kdtree2
  subroutine find_flowcells_kdtree(treeinst,Ns,xs,ys,inod,jaoutside,iLocTp, ierror)

     use m_missing
     use m_flowgeom
     use m_GlobalParameters, only: INDTP_1D, INDTP_2D, INDTP_ALL
     use kdtree2Factory
     use m_sferic
     use unstruc_messages
     use gridoperations
     use geometry_module, only: dbdistance, pinpok

     implicit none
     
     type(kdtree_instance),           intent(inout) :: treeinst
     integer,                         intent(in)    :: Ns      !< number of samples
     double precision, dimension(Ns), intent(in)    :: xs, ys  !< observation coordinates
     double precision, dimension(:),  allocatable   :: xx, yy  !< unique station coordinates
     integer,          dimension(:),  allocatable   :: iperm   !< permutation array
     integer,          dimension(:),  allocatable   :: invperm !< inverse array 
     integer,          dimension(Ns), intent(out)   :: inod    !< flow nodes
     integer,                         intent(in)    :: jaoutside  !< allow outside cells (for 1D) (1) or not (0)
     integer,                         intent(in)    :: iLocTp !< (0) not for obs, or obs with locationtype==0, (1) for obs with locationtype==1, (2) for obs with locationtype==2
     integer,                         intent(out)   :: ierror  !< error (>0), or not (0)
     
     character(len=128)                           :: mesg, FNAM
     
     integer,          parameter                  :: Msize=10
     
     double precision, dimension(Msize)           :: xloc, yloc
     integer,          dimension(Msize)           :: Lorg
     integer,          dimension(Msize)           :: LnnL

     double precision                             :: dmaxsize, R2search, t0, t1, zz
     
     integer                                      :: i, ip1, isam, in, k, N, NN 
     integer                                      :: inum, num, jj
     integer                                      :: in3D, j, fid
     integer                                      :: nstart, nend
     logical                                      :: jadouble
     double precision                             :: dist_old, dist_new    

     ierror = 1

     inod = 0

     call klok(t0)
     
     if ( janeedfix.eq.1 ) then
     
!       reduce double stations ( see "fix for Wim" in subroutine rmdouble)
        allocate(iperm(Ns), invperm(Ns))
        iperm = 0
        invperm = 0
        allocate(xx(Ns),yy(Ns))
        xx = 0d0
        yy = 0d0
        num = 0
        do i=1,Ns
           if ( xs(i).ne.DMISS .and. ys(i).ne.DMISS ) then
              jadouble = .false.
              do inum=1,num
                 if ( xs(i).eq.xx(inum) .and. ys(i).eq.yy(inum) )  then
                   jadouble = .true.
                   exit
                 end if
              end do
              if ( jadouble ) then
                 invperm(i) = -iperm(inum)      ! store unique station index
                 cycle
              end if
              
!             new unique observation station
              num        = num+1
              xx(num)    = xs(i)
              yy(num)    = ys(i)
              iperm(num) = i
              invperm(i) = num   ! really the inverse permutation
           end if
        end do
           
!       build kdtree
        call build_kdtree(treeinst, num, xx, yy, ierror, jsferic, dmiss)
     else
        call build_kdtree(treeinst, Ns, xs, ys, ierror, jsferic, dmiss)
     end if
     
     if ( ierror.ne.0 ) then
        goto 1234
     end if

     ! define the searching range, this is especially for the purpose of snapping obs to 1D, 2D or 1D+2D flownodes. 
     ! For other purpose it should stay as before
     select case(iLocTp)
     case (INDTP_ALL)
        nstart = 1
        nend   = ndx
     case(INDTP_1D) ! 1d flownodes coordinates
        nstart = ndx2D+1
        nend   = ndx
     case(INDTP_2D) ! 2d flownodes coordinates
        nstart = 1
        nend   = ndx2D 
     end select
     
     call mess(LEVEL_INFO, 'Finding flow nodes...')   

!    loop over flownodes
     do k = nstart, nend
!       fill query vector
        call make_queryvector_kdtree(treeinst,xz(k),yz(k), jsferic)
        
!       compute maximum flowcell dimension
        dmaxsize = 0d0
        N = size(nd(k)%x)
        do i=1,N
           ip1=i+1; if ( ip1.gt.N ) ip1=ip1-N
           dmaxsize = max(dmaxsize, dbdistance(nd(k)%x(i),nd(k)%y(i),nd(k)%x(ip1),nd(k)%y(ip1), jsferic, jasfer3D, dmiss))
        end do
        
!       determine square search radius
        R2search = 1.1d0*dmaxsize**2  ! 1.1d0: safety
        
!       get the cell polygon that is safe for periodic, spherical coordinates, inluding poles         
        call get_cellpolygon(k,Msize,N,1d0,xloc,yloc,LnnL,Lorg,zz)
        
        if ( N.lt.1 ) then
           if ( k.le.Ndxi ) then
              continue
           end if
           cycle
        end if
        
!       count number of points in search area
        NN = kdtree2_r_count(treeinst%tree,treeinst%qv,R2search)

        if ( NN.eq.0 ) cycle ! no links found
        
!       reallocate if necessary
        call realloc_results_kdtree(treeinst,NN)
     
!       find nearest NN samples
        call kdtree2_n_nearest(treeinst%tree,treeinst%qv,NN,treeinst%results)

!       check if samples are in cell
        do i=1,NN
           if ( janeedfix.eq.1 ) then
              jj = treeinst%results(i)%idx
!             find samples in original numbering (excluding the doubles)
              isam = iperm(jj)
           else
              isam = treeinst%results(i)%idx
           end if
           
           if ( k>ndx2D .and. k<ndxi+1 .and. jaoutside.eq.1 ) then  ! For 1D nodes, skip point-in-cell check
              in = 1                           ! These are always accepted if closest. 
           else
              call pinpok(xs(isam), ys(isam), N, xloc, yloc, in, jins, dmiss)
              
!!             BEGIN DEBUG              
!              if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
!                 call pinpok3D(xs(isam),ys(isam),N,xloc,yloc,in3D)
!                 if ( in3D.ne.in ) then
!                    write(FNAM, "('obs_', I0, '_cell_', I0, '.m')") isam, k
!                    call newfil(fid, trim(FNAM))
!                    write(fid,"('x=[', $)")
!                    do j = 1,N
!                       if ( j.gt.1 ) write(6,*)
!                       write(fid,"(E15.5, $)") xloc(j)
!                    end do
!                    write(fid,"('];')")
!                    
!                    write(fid,"('y=[', $)")
!                    do j = 1,N
!                       if ( j.gt.1 ) write(6,*)
!                       write(fid,"(E15.5, $)") yloc(j)
!                    end do
!                    write(fid,"('];')")
!                    
!                    write(fid,"('xp=', E15.5, ';')") xs(isam)
!                    write(fid,"('yp=', E15.5, ';')") ys(isam)
!                    call doclose(fid)
!                    
!                    continue
!                 end if
!              end if
!!             END DEBUG
              
           endif 
           if ( in.eq.1 ) then
              if ( inod(isam).ne.0 ) then            ! should not happen, but it can: for example in case of overlapping 1D branches
                 write(mesg, "('find_flowcells_kdtree: sample/point ', I0, ' in cells ', I0, ' and ', I0)") isam, inod(isam), k
                 call mess(LEVEL_INFO, mesg  )
!                goto 1234
                 if ( k>ndx2D .and. k<ndxi+1 .and. jaoutside.eq.1 ) then  ! ONLY in case of a 1D node, consider replacing, if the 1D node is closer
                    dist_old = dbdistance(xs(isam),ys(isam),xz(inod(isam)), yz(inod(isam)),jsferic, jasfer3D, dmiss)
                    dist_new = dbdistance(xs(isam),ys(isam),xz(k), yz(k),jsferic, jasfer3D, dmiss)            
                    if (dist_new<dist_old) then            ! if the new candidate is nearer to the observation station  ... 
                       inod(isam) = k                      ! ... adopt the new candidate as primary candidate 
                    endif 
                    write(mesg, "('   selected : ',I0,' based on distance comparison.')")  inod(isam)
                    call mess(LEVEL_INFO, mesg  )
                 end if
              else
                 inod(isam) = k
              end if
           end if
        end do
     end do
     
     if ( janeedfix.eq.1 ) then
!       fill double stations (by copy from respective unique  one)
        do isam=1,Ns
           i=invperm(isam) ! the unique number
           if ( i.lt.0 ) then
              inod(isam) = inod(-i)
           end if
        end do
     end if

     call klok(t1)

     write(mesg, "('done in ', F12.5, ' sec.')") t1-t0
     call mess(LEVEL_INFO, trim(mesg))

     ierror = 0
1234 continue

!    deallocate
     if ( treeinst%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(treeinst)
     if ( janeedfix.eq.1 ) then
        if ( allocated(iperm)   ) deallocate(iperm)
        if ( allocated(invperm) ) deallocate(invperm)
        if ( allocated(xx)      ) deallocate(xx)
        if ( allocated(yy)      ) deallocate(yy)
     end if

     return
  end subroutine find_flowcells_kdtree


!---------------------------------------------------------------
module m_snappol  ! intentionally a module (for assumed size)
use kdtree2Factory
implicit none
   contains   
   
!> snap polygon to mesh
  subroutine snappol(Nin, Xin, Yin, dsep, itype, Nout, Xout, Yout, ipoLout, ierror)
     use m_polygon
     use m_missing
     use m_alloc
     use m_flowgeom
     use network_data, only: xk, yk, kn
     use sorting_algorithms, only: indexxi
     implicit none

     integer,                                     intent(in)  :: Nin          !< thin-dyke polyline size
     double precision, dimension(Nin),            intent(in)  :: Xin, Yin     !< dsep-separated thin-dyke polyline coordinates
     double precision,                            intent(in)  :: dsep         !< separator
     integer,                                     intent(in)  :: itype        !< netlinks (1: cross with dual link, 3: cross with netlink itself) or flowlinks(2)


     integer,                                     intent(out) :: Nout         !< output polygon size
     double precision, dimension(:), allocatable, intent(out) :: Xout, Yout   !< output polygon coordinates, dim(Nout)
     integer,          dimension(:), allocatable, intent(out) :: ipoLout      !< reference to input polyline (>0), seperator w.r.t. input polyline (0), dim(Nout)
     integer,                                     intent(out) :: ierror       !< error (1) or not (0)

     integer                                                  :: NumLinks, NDIM

     double precision, dimension(:), allocatable              :: dSL
     integer,          dimension(:), allocatable              :: iLink, ipol
     integer,          dimension(:), allocatable              :: ipolnr, indx

     integer                                                  :: i, ii, iL, ipL, ipolsec, k1, k2, L, numpols

     ierror = 1

     Nout = 0

!    save polygon
     call savepol()

!    allocate
     allocate(iLink(Lnx))
     iLink = 0
     allocate(ipol(Lnx))
     ipol = 0
     allocate(dSL(Lnx))
     dSL = 0d0
     allocate(ipolnr(Nin))
     ipolnr = 999

!    number the input polyline segments
     numpols = 0   ! polyline segment counter
     i       = 1   ! pointer in input array
     do while ( i.lt.Nin )
!       advance pointer to start of segment
        do while ( (xin(i).eq.dsep .or. yin(i).eq.dsep) )
           ipolnr(i) = 0
           i=i+1
           if ( i.gt.Nin ) exit
        end do

!       check for end of array
        if ( i.gt.Nin ) exit

!       mark this segment
        numpols = numpols+1
        do while ( xin(i).ne.dsep .and. yin(i).ne.dsep )
           ipolnr(i) = numpols
           i=i+1
           if ( i.gt.Nin ) exit
        end do

!       check for end of array
        if ( i.gt.Nin ) exit
     end do

!!    BEGIN DEBUG
!     do i=1,NPL
!        zpl(i) = dble(ipolnr(i))
!     end do
!     goto 1234
!!    END DEBUG

!    temporarily use output arrays to replace missing value
     call realloc(xout, Nin, keepExisting=.false.)
     call realloc(yout, Nin, keepExisting=.false.)

!    replace missing values
     do i=1,Nin
        xout(i) = xin(i)
        yout(i) = yin(i)
        if ( xin(i).eq.dsep .or. yin(i).eq.dsep ) then
           xout(i) = DMISS
           yout(i) = DMISS
        end if
     end do

!    snap polygon (note: xout and yout are temporary arrays)
     call find_crossed_links_kdtree2(treeglob,Nin,xout,yout,itype,Lnx,1,NumLinks,iLink, iPol, dSL, ierror)
     if ( ierror.ne.0 .or. NumLinks.eq.0 ) goto 1234

!    sort crossed flowlinks in increasing polyline order
     allocate (indx(numLinks))
     call indexxi(numLinks,iPol,indx)

!    increase polygon array
     call increasepol(3*NumLinks,0)

     ii=1   ! pointer in indx array, sorted in increasing polygon number (iPol)
     
     do ipL=1,numpols
!       fill polygon with sections
        i = 0

!       advance pointer
        do while ( ipolnr(iPol(indx(ii))).lt.ipL )
           ii=ii+1
        end do

        if ( ii.gt.Numlinks ) then  ! done
           exit
        end if

        do while ( ipolnr(iPol(indx(ii))).eq.ipL )
        !do iL=1,NumLinks
           iL = indx(ii)

           L = iLink(iL)
!          check for matching polygon section
           ipolsec = iPol(iL)
           if ( ipolsec.lt.1 .or. ipolsec.ge.Nin ) then  ! should not happen
              continue
              exit
           end if
           if ( ipolnr(ipolsec).eq.ipL .or. ipolnr(ipolsec+1).eq.ipL ) then
               !             find the netnodes
              if (itype == 1 .or. itype == 3) then
                  k1 = kn(1,L)
                  k2 = kn(2,L)
              else ! itype == 2: flowlinks
                 k1 = lncn(1,L)
                 k2 = lncn(2,L)
              end if

!             fill the polyline array
              i=i+1
              xpl(i) = xk(k1)
              ypl(i) = yk(k1)
              i=i+1
              xpl(i) = xk(k2)
              ypl(i) = yk(k2)
              i=i+1
              xpl(i) = DMISS
              ypl(i) = DMISS
           else   ! should not happen
              continue
           end if

           ii = ii+1

           if ( ii.gt.Numlinks ) exit  ! done
        end do
        NPL = i

!       merge polyline section parts
        call merge_polylines()

        if ( NPL.lt.2 ) cycle  ! no polyline section found

!       copy to output
        NDIM = Nout + NPL + 1 ! add one for seperator
        call realloc(Xout, NDIM, keepExisting=.true.)
        call realloc(Yout, NDIM, keepExisting=.true.)
        call realloc(ipoLout, NDIM, keepExisting=.true.)
        do i=1,NPL
           Xout(Nout+i) = xpl(i)
           Yout(Nout+i) = ypl(i)
           if ( xpl(i).eq.DMISS .or. xpl(i).eq.DMISS ) then
              Xout(Nout+i) = dsep
              Yout(Nout+i) = dsep
           end if
           ipoLout(Nout+i) = ipL
        end do
        Nout = Nout + NPL + 1 ! add one for seperator
        Xout(Nout) = dsep
        Yout(Nout) = dsep
        ipoLout(Nout)  = 0

        if ( ii.gt.NumLinks ) exit  ! done
     end do

     ierror = 0
1234 continue

     if ( allocated(ipolnr) ) deallocate(ipolnr)
     if ( allocated(iLink)  ) deallocate(iLink)
     if ( allocated(iPol)   ) deallocate(iPol)
     if ( allocated(dSL)    ) deallocate(dSL)

     call restorepol()
 
     return
  end subroutine snappol


!> snap point to flow node
  subroutine snappnt(Nin, xin, yin, dsep, Nout, xout, yout, ipoLout, ierror)
     use m_alloc
     use m_flowgeom, only: xz, yz
     use m_GlobalParameters, only: INDTP_ALL
     implicit none

     integer,                                     intent(in)  :: Nin          !< thin-dyke polyline size
     double precision, dimension(Nin)                         :: Xin, Yin     !< dsep-separated thin-dyke polyline coordinates
  
     double precision,                            intent(in)  :: dsep         !< missing value

     integer,                                     intent(out) :: Nout         !< output polygon size
     double precision, dimension(:), allocatable, intent(out) :: Xout, Yout   !< output polygon coordinates, dim(Nout)
     integer,          dimension(:), allocatable, intent(out) :: ipoLout      !< reference to input points (>0), no flownode found (0), dim(Nout)
     integer,                                     intent(out) :: ierror       !< error (1) or not (0)

     character(len=40), dimension(:), allocatable             :: namobs

     integer,          dimension(:), allocatable              :: kobs

     integer                                                  :: i, k
     integer                                                  :: jakdtree = 0

     ierror = 1
     Nout   = 0

     if ( Nin.lt.1 ) goto 1234

!    allocate
     allocate(namobs(Nin))
     allocate(kobs(Nin))

     do i=1,Nin
        namobs(i) = ''
        kobs(i)   = 0
     end do

     call find_flownode(Nin, xin, yin, namobs, kobs, jakdtree, 1, INDTP_ALL)

!    copy to output
     Nout = Nin
     call realloc(xout, Nout, keepExisting=.false., fill=dsep)
     call realloc(yout, Nout, keepExisting=.false., fill=dsep)
     call realloc(ipoLout, Nout, keepExisting=.false., fill=0)

     do i=1,Nout
        k = kobs(i)
        if ( k.gt.0 ) then
           xout(i) = xz(k)
           yout(i) = yz(k)
           ipoLout(i) = i
        else
           xout(i) = dsep
           yout(i) = dsep
           ipoLout(i) = 0
        end if
     end do

     ierror = 0
1234 continue

!    deallocate
     if ( allocated(namobs) ) deallocate(namobs)
     if ( allocated(kobs)   ) deallocate(kobs)

     return
  end subroutine snappnt


!> snap polyline to mesh boundary
!>   2D only
  subroutine snapbnd(bndtype, Nin, Xin, Yin, dsep, Nout, Xout, Yout, ipoLout, ierror)
     use timespace_triangle
     use m_polygon
     use m_missing
     use network_data, only: kn, xk, yk, NumL, lne
     use m_flowparameters, only: izbndpos
     implicit none

     character(len=*),                              intent(in)  :: bndtype    !< boundary condition type

     integer,                                       intent(in)  :: Nin          !< polyline size
     double precision, dimension(Nin),              intent(in)  :: Xin, Yin     !< dsep-separated polyline coordinates
     double precision,                              intent(in)  :: dsep         !< separator

     integer,                                       intent(out) :: Nout         !< output polygon size
     double precision, dimension(:),   allocatable, intent(out) :: Xout, Yout   !< output polygon coordinates, dim(Nout)
     integer,          dimension(:),   allocatable, intent(out) :: ipoLout      !< reference to input polyline (>0), seperator w.r.t. input polyline (0), dim(Nout)
     integer,                                       intent(out) :: ierror       !< error (1) or not (0)

     double precision, dimension(:),   allocatable              :: xe, ye
     double precision, dimension(:,:), allocatable              :: xyen
     double precision, dimension(:),   allocatable              :: xdum, ydum

     integer,          dimension(:),   allocatable              :: kce, ke, ki, kcs
     integer,          dimension(:),   allocatable              :: idx

     double precision                                           :: wL, wR
     double precision                                           :: xm, ym, crpm, distanceStartPolygon

     double precision, dimension(4)                             :: xx, yy
     double precision                                           :: xzz, yzz, xci, yci, xce2, yce2

     integer                                                    :: ierr, mx1Dend, Nx, numpols, jamiss
     integer                                                    :: i, iend, j, k1, k2, k3, k4, kL, kR, L, m, num, NDIM
     integer                                                    :: ja, isec, numsec, k, Lf

     integer            :: ioutput

     integer, parameter :: INETLINKS  = 0
     integer, parameter :: IFLOWNODES = 1
     integer, parameter :: IFLOWLINKS = 2

     select case(trim(bndtype))
        case( 'boundary' )
           ioutput = INETLINKS
        case( 'velocitybnd', 'dischargebnd', '1d2dbnd')
           ioutput = IFLOWLINKS
        case DEFAULT
           ioutput = IFLOWNODES
     end select

     ierror = 1
     Nout   = 0

!    save polygon
     call savepol()

     
!    count number of 2D links and 1D endpoints
     call count_links(mx1Dend, Nx)

!    allocate
     if(allocated(xe)) deallocate(xe, stat=ierror) 
     if(allocated(ye)) deallocate(ye, stat=ierror) 
     if(allocated(xyen)) deallocate(xyen, stat=ierror) 	 
     if(allocated(kce)) deallocate(kce, stat=ierror) 
     if(allocated(ke)) deallocate(ke, stat=ierror) 
     if(allocated(ki)) deallocate(ki, stat=ierror) 
     if(allocated(kcs)) deallocate(kcs, stat=ierror) 
     if(allocated(xdum)) deallocate(xdum, stat=ierror) 
     if(allocated(ydum)) deallocate(ydum, stat=ierror) 

     allocate(xe(Nx), stat=ierror)
     allocate(ye(Nx), stat=ierror)
     allocate(xyen(2,Nx), stat=ierror)
     allocate(kce(Nx), stat=ierror)
     allocate(ke(Nx), stat=ierror)
     allocate(ki(Nx), stat=ierror)

     allocate(kcs(Nin), stat=ierror)
     allocate(xdum(Nin), stat=ierror)
     allocate(ydum(Nin), stat=ierror)
     
     kce = 0
     ke  = 0

!    replace missing values
     do i=1,Nin
        xdum(i) = xin(i)
        ydum(i) = yin(i)
        if ( xin(i).eq.dsep .or. yin(i).eq.dsep ) then
           xdum(i) = DMISS
           ydum(i) = DMISS
        end if
     end do

!    make mirror cells (will set kce and ke)
     call make_mirrorcells(Nx, xe, ye, xyen, kce, ke, ierror) 

!    set polyline mask
     kcs = 1

!    loop over the input polylines
     numpols = 0   ! polyline counter
     i       = 1   ! pointer in input array
     do while ( i.lt.Nin )
!       advance pointer to start of segment
        do while ( (xin(i).eq.dsep .or. yin(i).eq.dsep) )
           i=i+1
           if ( i.gt.Nin ) exit
        end do

!       check for end of array
        if ( i.gt.Nin ) exit

!       find end pointer of this polyline
        numpols = numpols+1
        iend = i
        do while ( xin(iend).ne.dsep .and. yin(iend).ne.dsep )
           iend=iend+1
           if ( iend.gt.Nin ) exit
        end do
        iend = iend-1

!-------------------------------------------------------------------------------------------------------------------------
!       the core of this subroutine (mostly copied from selectelset)
!-------------------------------------------------------------------------------------------------------------------------
!       find boundary links
        num = 0
        do m = 1,Nx
           if (iabs(kce(m)) == 1) then     ! point is a possible candidate for a line boundary
              call polyindexweight(xe(m), ye(m),  xyen(1,m), xyen(2,m), Xdum(i:iend), Ydum(i:iend), kcs(i:iend), iend-i+1, kL, wL, kR, wR)
              ! if k1 > 0 this point can be dataprovided by this polyline
              if (kL > 0 .or. kR > 0) then
                 if ( kce(m) .eq. -1 ) then
                    !errormessage = 'Boundary location already claimed; Overlap with other bnds?'
                    !return
                    continue
                    cycle
                 else
                    num     =  num + 1
                    ki(num) =  m
                    kce(m)   = -1                ! this tells you this point is already claimed by some bnd
                 endif
              endif
           endif
        enddo
!-------------------------------------------------------------------------------------------------------------------------

!       copy found net links to polygon segments
        call increasepol(3*num, 0)
        NPL = 0
        do m=1,num
           L = ki(m)
           k1 = kn(1,L)
           k2 = kn(2,L)

           NPL = NPL+1
           XPL(NPL) = xk(k1)
           YPL(NPL) = yk(k1)

           NPL = NPL+1
           XPL(NPL) = xk(k2)
           YPL(NPL) = yk(k2)

           NPL = NPL+1
           XPL(NPL) = DMISS
           YPL(NPL) = DMISS
        end do

   !    merge polyline section parts
        call merge_polylines()

        if ( ioutput.eq.IFLOWNODES .or. ioutput.eq.IFLOWLINKS ) then
   !       find flownodes or flowlinks for output
           call realloc(idx, NPL, keepExisting=.false., fill=0)
           numsec = 0
           do i=1,num
              m = ki(i)
   !          find polyline section (again)
              call CROSSPOLY(xe(m),ye(m),xyen(1,m),xyen(2,m),XPL,YPL,NPL,xm,ym,crpm,ja,isec,distanceStartPolygon)
           
   !          remember which polyline segment points to this link
              if ( isec.gt.0 ) then
                 if ( idx(isec).eq.0 ) then
                    numsec = numsec+1
                    idx(isec) = m
                 else   ! should not happen
                    continue
                 end if
              else   ! should not happen
                 continue
              end if
           end do
        end if


!-------------------------------------------------------------------------------------------------------------------------
   !    copy to output
        NDIM = Nout + NPL + 1 ! add one for seperator
        call realloc(Xout, NDIM, keepExisting=.true., fill = DMISS)
        call realloc(Yout, NDIM, keepExisting=.true., fill = DMISS)
        call realloc(ipoLout, NDIM, keepExisting=.true., fill = 0)

        if ( ioutput.eq.INETLINKS ) then
        do m=1,NPL
           Xout(Nout+m) = xpl(m)
           Yout(Nout+m) = ypl(m)
           ipoLout(Nout+m) = numpols
        end do
        else if ( ioutput.eq.IFLOWLINKS ) then
           num = 0   ! polyline size
           jamiss = 0
           do i=1,NPL
              Xout(Nout+i) = DMISS
              Yout(Nout+i) = DMISS
              ipoLout(Nout+i) = numpols

              L = idx(i)
              if ( L.gt.0 .and. L.le.numL ) then
                 jamiss = 1
                 k1 = kn(1,L)
                 k2 = kn(2,L)
                 if ( k1.gt.0 .and. k2.gt.0 ) then
                    num = num+1
                    Xout(Nout+num) = 0.5d0*(xk(k1)+xk(k2))
                    Yout(Nout+num) = 0.5d0*(yk(k1)+yk(k2))
                    ipoLout(Nout+num) = numpols
                 end if
              else
                 if ( jamiss.eq.1 ) then ! add seperator
                    num = num+1
                    Xout(Nout+num) = DMISS
                    Yout(Nout+num) = DMISS
                    ipolout(Nout+num) = numpols
                    jamiss = 0
                 end if
              end if
           end do
           NPL = num
        else if ( ioutput.eq.IFLOWNODES ) then
           num = 0
           jamiss = 0
           do i=1,NPL
              m = idx(i)
              if ( m.gt.0 ) then
!-------------------------------------------------------------------------------------------------------------------------
! mostly copied from addexternalboundarypoints
!-------------------------------------------------------------------------------------------------------------------------
                 !if (kn(3,L) .ne. 1) then  ! in 2D mirror cell
                 if ( m.le.numL ) then
                    jamiss = 1
                    L  = m
                    k2 = iabs(lne(1,L))
                    k3 = kn(1,L); k4 = kn(2,L)

                    call mirrorcell(k2, xk(k3), yk(k3), xk(k4), yk(k4), xci, yci, xzz, yzz, xce2, yce2, xx, yy)
                    if (izbndpos == 0) then                    ! as in D3DFLOW

                    else if (izbndpos == 1) then               ! on network boundary
                       xzz  = 0.5d0*( xk(k3) + xk(k4 ) )
                       yzz  = 0.5d0*( yk(k3) + yk(k4 ) )
                    else if (izbndpos == 2) then               ! on specified boundary polyline

                    end if
                    num = num+1
                    Xout(Nout+num) = xzz
                    Yout(Nout+num) = yzz
                    ipoLout(Nout+num) = numpols
                 endif
!-------------------------------------------------------------------------------------------------------------------------
              else
                 if ( jamiss.eq.1 ) then
                    num = num+1
                    Xout(Nout+num) = DMISS
                    Yout(Nout+num) = DMISS
                    ipoLout(Nout+num) = numpols
                    jamiss = 0
                 end if
              end if
           end do
           NPL = num
        end if

!       remove trailing missing values
        if ( NPL.gt.0 ) then
           do while ( Xout(Nout+NPL).eq.DMISS .and. YOUT(Nout+NPL).eq.DMISS .and. NPL.gt.0)
              NPL = NPL-1
              if ( NPL.lt.1 ) exit
           end do
        end if

        if ( NPL.gt.0 ) then
        Nout = Nout + NPL + 1 ! add one for seperator
           Xout(Nout) = DMISS
           Yout(Nout) = DMISS
        ipoLout(Nout)  = 0
        end if

!       advance pointer
        i = iend+1

!       check for end of array
        if ( i.gt.Nin ) exit
     end do

!    replace DMISS with seperator, if necessary
     if ( dsep.ne.DMISS ) then
        do i=1,Nout
           if ( Xout(i).eq.DMISS .or. Yout(m).eq.DMISS ) then
              Xout(i) = dsep
              Yout(i) = dsep
           end if
        end do
     end if

!    trim output arrays to actual size
     call realloc(Xout,    Nout, keepExisting=.true.)
     call realloc(Yout,    Nout, keepExisting=.true.)
     call realloc(ipolout, Nout, keepExisting=.true.)

     ierror = 0
1234 continue

!    deallocate
     if ( allocated(xe)   ) deallocate(xe)
     if ( allocated(ye)   ) deallocate(ye)
     if ( allocated(xyen) ) deallocate(xyen)
     if ( allocated(kce)  ) deallocate(kce)
     if ( allocated(ke)   ) deallocate(ke)
     if ( allocated(ki)   ) deallocate(ki)
     if ( allocated(kcs)  ) deallocate(kcs)
     if ( allocated(xdum) ) deallocate(xdum)
     if ( allocated(ydum) ) deallocate(ydum)
     if ( allocated(idx)  ) deallocate(idx)

     call restorepol()

     return
  end subroutine snapbnd
end module m_snappol



!< get jaopengl module variable
integer function iget_jaopengl()
   use unstruc_opengl, only: jaopengl
   implicit none
   
   iget_jaopengl = jaopengl
   
   return
end function iget_jaopengl


!< set jaopengl module variable
subroutine iset_jaopengl(jaopengl_loc)
   use unstruc_opengl, only: jaopengl
   use unstruc_model,  only: md_jaopengl
   implicit none
   
   integer, intent(in) :: jaopengl_loc  !< value to be set to jaopengl
   
   if ( md_jaopenGL.eq.-1 ) then
      jaopengl = jaopengl_loc
   else
      jaopengl = md_jaopengl
   end if
   
   return
end subroutine iset_jaopengl


!> prepares a matrix for solver test (as in "mpitest")
subroutine make_matrix(CFL, s1)
      use m_reduce
      use m_flowgeom

      implicit none

      double precision,                   intent(in)  :: CFL     !< CFL-number
      double precision, dimension(Ndx),   intent(in)  :: s1      !< exact solution

      double precision                                :: aufu

      integer                                         :: k1, k2, n, L

      bbr = 1/CFL**2
      ccr = 0d0
      do L = 1,lnx
         aufu        = 1d0
         k1 = ln(1,L)
         k2 = ln(2,L)
         bbr(k1)     = bbr(k1)     + aufu
         bbr(k2)     = bbr(k2)     + aufu
         ccr(Lv2(L)) = ccr(Lv2(L)) - aufu
      enddo

!      dd = 0d0
!      do n=1,Ndx
!         dd(n) = dd(n) + bb(n)*s1(n)
!      end do

      ddr = bbr*s1
      do L=1,Lnx
         k1 = ln(1,L)
         k2 = ln(2,L)
         ddr(k1) = ddr(k1) + ccr(Lv2(L))*s1(k2)
         ddr(k2) = ddr(k2) + ccr(Lv2(L))*s1(k1)
      enddo

      return
   end subroutine make_matrix
   
   !> test iterative solver (as "mpitest")
   subroutine soltest(iCFL,icgsolver_loc,maxsubmatvecs,iepsdiff,iepscg)
      use m_partitioninfo
      use m_timer
      use unstruc_messages
      use m_flowgeom
      use network_data, only: xzw
      use m_flowparameters
      use m_reduce
      use m_flow
      use m_alloc
      use unstruc_model, only: md_findcells
      implicit none
      
      integer,                          intent(in)  :: iCFL            !< wave-based Courant number
      integer,                          intent(in)  :: icgsolver_loc   ! icgsolver (if > 0)
      integer,                          intent(in)  :: maxsubmatvecs   ! maximum number of subiterations in Schwarz solver (if > 0)
      integer,                          intent(in)  :: iepsdiff        ! -10log(tolerance in Schwarz iterations) (if > 0)
      integer,                          intent(in)  :: iepscg          ! -10log(tolerance in inner iterations) (if > 0)
      
      double precision, dimension(:),   allocatable :: sex   ! exact solution at cell centers
      double precision, dimension(:),   allocatable :: dmask ! used for masking ghost cells that are not being updated
      
      double precision                              :: CFL
      double precision                              :: diffmax
      
      integer                                       :: NRUNS
      integer                                       :: i, ii, irun
      integer                                       :: ierror
      
      integer, external :: flow_modelinit
      
      jarenumber = 0
      CFL = 10d0
!      maxdge = 0d0
!      icgsolver = 4
!      ipre = 0
      Nruns = 1
      
      if ( iCFL.gt.0d0 ) then
         CFL = dble(iCFL)
      end if
      
!     settings from command line      
      if ( icgsolver_loc.gt.0 ) then
         icgsolver = icgsolver_loc
      end if
      
      if ( iepsdiff.gt.0 ) then
         epsdiff = 10d0**(-iepsdiff)
      end if
      
      if ( iepscg.gt.0 ) then
         epscg = 10d0**(-iepscg)
      end if
      
      if ( maxsubmatvecs.gt.0 ) then
         maxmatvecs = maxsubmatvecs
      end if
      
      ierror = flow_modelinit()
      
!!      call initimer()
!      
!      call resetflow()
!      
!      if ( jampi.eq.0 ) then
!         call flow_geominit(0)
!      
!         if (Ndx == 0) then
!           call mess(LEVEL_INFO,'no network')
!           goto 1234
!         end if
!         
!      else
!         call flow_geominit(1)   ! first phase only
!      
!         if ( Ndx.gt.0 ) then
!            if ( jatimer.eq.1 ) call starttimer(IPARTINIT)
!            call partition_init_1D2D('dummy', md_genpolygon, ierror)   ! both for 1D & 2D (hence the name, thanks to Herman for pointing this out)
!            if ( jatimer.eq.1 ) call stoptimer(IPARTINIT)
!            
!            if ( ierror.ne.0 ) then
!              call mess(LEVEL_WARN,'Error in 2D partitioning initialization.')
!              goto 1234
!            end if
!            
!            call update_geom(1)              ! update geometry in ghost area 
!            
!            call flow_geominit(2)            ! second phase
!            call update_geom(2)              ! update geometry in ghost area 
!            
!            call disable_invalid_ghostcells_with_wu() ! disable ghost cells that are not being synchronised by setting wu's to zero
!         else 
!            call mess(LEVEL_INFO,'no network')
!            goto 1234
!         end if
!      
!      end if
      
!      call flow_allocflow()

!     allocate solution and mask
      allocate(sex(Ndx))
      allocate(dmask(Ndx))
      
!     activate all cells
      hu = epshu+1d0
      
      
!     set exact solution
      sex = xzw
      
      if ( jatimer.eq.1 ) call starttimer(ITOTAL)
      
!!     prepare matrix
!      if ( jatimer.eq.1 ) call starttimer(IREDUCE)
!      call reducept(Ndx, Ndxi, Lnx)
!      if ( jatimer.eq.1 ) call stoptimer(IREDUCE)

!     construct matrix and rhs
      call make_matrix(CFL, sex)
      
!     update overlapping ghost-parts of matrix
      if ( jampi.eq.1 .and. jaoverlap.eq.1 ) then
         call update_matrix(ierror)
      end if

!     pack matrix
      call pack_matrix()
         
      call realloc(ccrsav, ubound(ccr,1), lbound(ccr,1), keepExisting=.false., fill=0d0)
      ccrsav = ccr

!     solve system
      do irun=1,Nruns
         s1 = 0d0
         ccr = ccrsav

!         if (icgsolver.eq.6) call setPETSCmatrixEntries()
!         call createPETSCPreconditioner(iprecond)

         call solve_matrix(s1,Ndx,itsol)
         
      end do
      if ( jatimer.eq.1 ) call stoptimer(ITOTAL)
      
!     unmask all cells       
      dmask = 0d0      

      if ( jampi.eq.1 ) then
         call update_ghosts(ITYPE_SALL,1,Ndx,s1,ierror)
         
!        mask all ghost cells  
         do i=1,Ndx
            if ( idomain(i).ne.my_rank ) then
               dmask(i) = 1d0
            end if
         end do
         
!        unmask ghost cells with updated values         
         call update_ghosts(ITYPE_SALL,1,Ndx,dmask,ierror)
      end if

      diffmax = 0d0
      do i=1,Ndxi
         if ( nd(i)%lnx.gt.0 .and. dmask(i).eq.0d0 ) then
            if ( abs(s1(i)-sex(i)).gt.1d-10 ) then
               continue
            end if
            diffmax = max(diffmax, abs(s1(i)-sex(i)))
         end if
      end do
      
      do ii=1,nghostlist_sall(ndomains-1)
         i = ighostlist_sall(ii)
         if ( abs(s1(i)-sex(i)).gt.1d-10 ) then
            continue
         end if
      end do

      write(6,'("rank", I2, ", number of iterations: ", I4, ", max diff: ", E7.2)') my_rank, itsol, diffmax
      
      if ( my_rank.eq.0 ) then
         write(6,'(a,E8.2,a,E8.2)') ' WC-time solver   [s]: ' , gettimer(1,ITOTALSOL), ' CPU-time solver   [s]: ' , gettimer(0,ITOTALSOL)
         write(6,'(a,E8.2,a,E8.2)') ' WC-time MPI comm [s]: ' , gettimer(1,IMPICOMM),  ' CPU-time MPI comm [s]: ' , gettimer(0,IMPICOMM)
      end if
!         call mpi_barrier(DFM_COMM_DFMWORLD,ierr)
         
!      call writemesg('Wallclock times')
!      call printall(numt, t(3,:), tnams)
!      call writemesg('CPU times')
!      call printall(numt, tcpu(3,:), tnams)
      
 1234 continue
 
      if ( allocated(sex)   ) deallocate(sex)
      if ( allocated(dmask) ) deallocate(dmask)
      
      return
   end subroutine soltest
   
   
!> output tide potential as samples
subroutine writidep(time)
   use m_flowgeom
   use m_flow
   use m_partitioninfo
   use m_samples
   implicit none
   
   double precision, intent(in) :: time
   
   character(len=256)           :: dateandtime
                                
   integer                      :: k
   integer                      :: jaRestoreSam
   integer                      :: itid
   
   dateandtime = '_'
   call maketime(dateandtime(2:), time)
   
   if ( jampi.eq.0 ) then
      call newfil(itid, 'tide_potential' // trim(dateandtime) // '.xyz')
   else
      call newfil(itid, 'tide_potential' // trim(dateandtime) // '_' // trim(sdmn) // '.xyz')
   end if
   
   jaRestoreSam = 0
   if ( Ns.gt.0 ) then
      call savesam()
      jaRestoreSam = 1
   end if
   
   call increasesam(Ndx)
   NS = Ndx
   
   do k=1,Ndx
      xs(k) = xz(k)
      ys(k) = yz(k)
      zs(k) = tidep(1,k)
   end do
   
   call wrisam(itid)
   
   call doclose(itid)
   
   if ( jaRestoreSam.eq.1 ) then
      call restoresam()
   else
      call delsam(0)
   end if
   
   return
end subroutine writidep

subroutine savecells() !! save netcell, lne, lnn, idomain
   use network_data
   use m_partitioninfo, only: idomain, idomain0
   use m_flowgeom, only: xz, xz0, yz, yz0, ba, ba0
   use m_alloc
   implicit none
   
   integer    :: ierr
   integer    :: k, N
   
   nump0 = nump
   nump1d2d0 = nump1d2d
   
   if(allocated(netcell0)) then
      do k=1,ubound(netcell0,1)
         if ( allocated(netcell0(k)%nod) ) deallocate(netcell0(k)%nod)
         if ( allocated(netcell0(k)%lin) ) deallocate(netcell0(k)%lin)
      end do
      deallocate(netcell0)
   end if
   
   allocate(netcell0(nump1d2d), stat = ierr) 
   do k=1,nump1d2d
      N = netcell(k)%N
      netcell0(k)%N = N
      
      allocate(netcell0(k)%nod(N))
      netcell0(k)%nod = netcell(k)%nod(1:N)
      
      allocate(netcell0(k)%lin(N))
      netcell0(k)%lin = netcell(k)%lin(1:N)
   end do
   
!   netcell0(1: nump1d2d) = netcell(1: nump1d2d)
   
   call realloc(lne0, (/2, numl/), stat=ierr, keepExisting=.false.)
   call realloc(lnn0, numl, stat=ierr, keepExisting=.false.)
   lne0 = lne
   lnn0 = lnn
   
   call realloc(xz0, nump1d2d, stat=ierr, keepExisting=.false.)
   call realloc(yz0, nump1d2d, stat=ierr, keepExisting=.false.)
   xz0 = xz
   yz0 = yz
   
   call realloc(xzw0, nump1d2d, stat=ierr, keepExisting=.false.)
   call realloc(yzw0, nump1d2d, stat=ierr, keepExisting=.false.)
   xzw0 = xzw
   yzw0 = yzw
   
   call realloc(ba0, nump1d2d, stat=ierr, keepExisting=.false.)
   ba0 = ba
   
   if ( allocated(idomain) ) then
      call realloc(idomain0, nump1d2d, stat=ierr, keepExisting=.false.)
      idomain0 = idomain
   end if

   end subroutine savecells
   
subroutine restorecells()
   use network_data
   use m_partitioninfo, only: idomain, idomain0
   use m_flowgeom, only: xz, xz0, yz, yz0, ba, ba0
   use m_alloc
   implicit none
   integer  :: ierr
   
   integer  :: k, N
   
!   nump1d2d = size(netcell)
   nump1d2d = nump1d2d0
   nump     = nump0
   
   if(allocated(netcell)) then
      do k=1,ubound(netcell0,1)
         if ( allocated(netcell(k)%nod) ) deallocate(netcell(k)%nod)
         if ( allocated(netcell(k)%lin) ) deallocate(netcell(k)%lin)
      end do
      deallocate(netcell)
   end if
   
   allocate(netcell(nump1d2d), stat = ierr) 
   do k=1,nump1d2d
      N = netcell0(k)%N
      netcell(k)%N = N
      
      allocate(netcell(k)%nod(N))
      netcell(k)%nod = netcell0(k)%nod(1:N)
      
      allocate(netcell(k)%lin(N))
      netcell(k)%lin = netcell0(k)%lin(1:N)
   end do
   
!   if(allocated(netcell))  deallocate(netcell)
!   allocate(netcell(nump1d2d), stat = ierr)
!   netcell(1: nump1d2d) = netcell0(1: nump1d2d)
   
   call realloc(lne, (/2, numl/), stat=ierr, keepExisting=.false.)
   call realloc(lnn, numl , stat=ierr, keepExisting=.false.)
   lne = lne0
   lnn = lnn0
   
   call realloc(xz, nump1d2d, stat=ierr, keepExisting=.false.)
   call realloc(yz, nump1d2d, stat=ierr, keepExisting=.false.)
   xz = xz0
   yz = yz0
   
   call realloc(xzw, nump1d2d, stat=ierr, keepExisting=.false.)
   call realloc(yzw, nump1d2d, stat=ierr, keepExisting=.false.)
   xzw = xzw0
   yzw = yzw0
   
   call realloc(ba, nump1d2d, stat=ierr, keepExisting=.false.)
   ba = ba0
   
   if ( allocated(idomain0) ) then
      call realloc(idomain, nump1d2d, stat=ierr, keepExisting=.false.)
      idomain = idomain0
   end if

end subroutine restorecells


subroutine updateValuesOnSourceSinks(tim1)
use m_flowexternalforcings, only: qsrc, qsrcavg, vsrccum, vsrccum_pre, numsrc
use m_missing
use m_flowtimes, only: ti_his, time_his
use precision
use m_flowparameters, only: eps10
implicit none
   double precision, intent(in) :: tim1 !< Current (new) time

   double precision,                 save        :: timprev = -1d0 ! TODO: save is unsafe, replace by using time1 and time0, also two other occurrences
   double precision                              :: timstep
   integer                                       :: i

   if (timprev < 0d0) then      
      allocate(qsrcavg(numsrc))
      allocate(vsrccum(numsrc))
      allocate(vsrccum_pre(numsrc))
      vsrccum = 0d0
      vsrccum_pre = 0d0
      qsrcavg = 0d0
   else
      timstep = tim1 - timprev
      ! cumulative volume from Tstart
      do i = 1, numsrc
         vsrccum(i) =vsrccum(i) + timstep*qsrc(i)
      enddo
      
      if (comparereal(tim1, time_his, eps10)== 0) then
         do i = 1, numsrc
            qsrcavg(i) = (vsrccum(i) - vsrccum_pre(i)) / ti_his ! average discharge in the past His-interval
            vsrccum_pre(i) = vsrccum(i)
         enddo
      endif
   end if
   
   timprev = tim1
end subroutine updateValuesOnSourceSinks

! update m_wind::vincum(:) with the realized inflow from m_wind::qinextreal(:)
subroutine updateCumulativeInflow(deltat) 
    use m_wind
    use m_flowgeom, only : ndx
    
    integer :: k  
    double precision, intent(in) :: deltat ! dt of current timestep
    
    if (jaQinext == 0) return

    do k = 1, ndx
        vincum(k) = vincum(k) + qinextreal(k)*deltat
    enddo
    
end subroutine updateCumulativeInflow
    
    
subroutine updateBalance()
   use m_flow
   use m_partitioninfo
   implicit none

   if (jampi == 1) then
      call reduce_bal(cumvolcur, MAX_IDX)
!     only need to reduce the first two entries of volcur, but we do the reduce for the whole array here 
      call reduce_bal(volcur,    MAX_IDX)
   endif
   voltot(IDX_STOR)    = volcur(IDX_STOR) - vol1ini
   voltot(IDX_VOLTOT)  = volcur(IDX_VOLTOT)
   voltot(IDX_VOLERR)  = voltot(IDX_VOLERR)  + cumvolcur(IDX_VOLERR)
   voltot(IDX_BNDIN )  = voltot(IDX_BNDIN )  + cumvolcur(IDX_BNDIN ) 
   voltot(IDX_BNDOUT)  = voltot(IDX_BNDOUT)  + cumvolcur(IDX_BNDOUT) 
   voltot(IDX_BNDTOT)  = voltot(IDX_BNDTOT)  + cumvolcur(IDX_BNDTOT) 
   voltot(IDX_EXCHIN)  = voltot(IDX_EXCHIN)  + cumvolcur(IDX_EXCHIN) 
   voltot(IDX_EXCHOUT) = voltot(IDX_EXCHOUT) + cumvolcur(IDX_EXCHOUT)
   voltot(IDX_EXCHTOT) = voltot(IDX_EXCHTOT) + cumvolcur(IDX_EXCHTOT)
   voltot(IDX_PRECIP)  = voltot(IDX_PRECIP)  + cumvolcur(IDX_PRECIP) 
   voltot(IDX_SOUR)    = voltot(IDX_SOUR)    + cumvolcur(IDX_SOUR)
   voltot(IDX_InternalTidesDissipation) = voltot(IDX_InternalTidesDIssipation) + cumvolcur(IDX_InternalTidesDIssipation)
   voltot(IDX_GravInput) = voltot(IDX_GravInput) + cumvolcur(IDX_GravInput)
   voltot(IDX_SALInput)  = voltot(IDX_SALInput)  + cumvolcur(IDX_SALInput)
   voltot(IDX_SALInput2) = voltot(IDX_SALInput2) + cumvolcur(IDX_SALInput2)
   voltot(IDX_GRWIN )  = voltot(IDX_GRWIN )  + cumvolcur(IDX_GRWIN ) 
   voltot(IDX_GRWOUT)  = voltot(IDX_GRWOUT)  + cumvolcur(IDX_GRWOUT) 
   voltot(IDX_GRWTOT)  = voltot(IDX_GRWTOT)  + cumvolcur(IDX_GRWTOT) 
   voltot(IDX_LATIN )  = voltot(IDX_LATIN )  + cumvolcur(IDX_LATIN ) 
   voltot(IDX_LATOUT)  = voltot(IDX_LATOUT)  + cumvolcur(IDX_LATOUT) 
   voltot(IDX_LATTOT)  = voltot(IDX_LATTOT)  + cumvolcur(IDX_LATTOT) 
   voltot(IDX_EVAP)    = voltot(IDX_EVAP)    + cumvolcur(IDX_EVAP)

   cumvolcur = 0d0
end subroutine updateBalance
   

subroutine generatePartitionMDUFile(filename, filename_new)
   use unstruc_model
   use unstruc_messages
   use m_partitioninfo
   use string_module
   implicit none
   character(len=*), intent(in)  :: filename, filename_new
   integer                       :: k1, k2, k3, k4, k5, k6, k7, n
   character(len=500)            :: string, string_c, string_tmp, string_v
   integer                       :: ja_innumerics, ja_icgsolverset

   open(261, file = filename, status ="old", action="read", err=999)
   open(262, file = filename_new, status = "replace", action="write",err=999)
  
   k1 = 0; k2 = 0; k3 = 0; k4 = 0; k5 = 0; k6 = 0; k7 = 0; ja_innumerics = 0; ja_icgsolverset = 0
   do while (.true.)
      read(261, "(a)", err=999, end=1212) string
      n = index(string, '=')

      !> In case icgsolver was not present in input MDU, find-and-replace impossible, so make sure to add it, because it's required.
      if (strcmpi(string, '[numerics]', 10)) then
         ja_innumerics = 1
      elseif (string(1:1) == '[' .and. ja_innumerics == 1) then ! About to close [numerics]
         if (ja_icgsolverset == 0) then
            write(string_v, "(I5)") md_icgsolver
            string_tmp = "Icgsolver = "//trim(adjustl(string_v))//"          # Solver type , 1 = sobekGS_OMP, 2 = sobekGS_OMPthreadsafe, 3 = sobekGS, 4 = sobekGS + Saadilud, 5 = parallel/global Saad, 6 = parallel/Petsc, 7 = parallel/GS"
            write(262, "(a)") trim(string_tmp)
         end if
         ja_innumerics = 0
      end if

      string_c = string(1:n)
      call str_lower(string_c)
      k1 = index(string_c, 'netfile')
      k2 = index(string_c, 'icgsolver')
      if (len_trim(md_restartfile) > 0) then
         k3 = index(string_c, 'restartfile')
      endif
      if (len_trim(md_mapfile) > 0) then
         k4 = index(string_c, 'mapfile')
      endif
      if (md_genpolygon .eq. 1) then
         k5 = index(string_c, 'partitionfile')
      endif
      if (len_trim(md_flowgeomfile) > 0) then
         k6 = index(string_c, 'flowgeomfile')
      endif
      if (len_trim(md_classmap_file) > 0) then
         k7 = index(string_c, 'classmapfile')
      endif

      if(k1==0 .and. k2==0 .and. k3==0 .and. k4==0 .and. k5==0 .and. k6==0 .and. k7==0) then ! Copy the whole row
         write(262, "(a)") trim(string)
      else 
         if (k1 .ne. 0) then      ! modify NetFile
           string_tmp = trim(string_c)//" "//trim(md_netfile)//"        # *_net.nc"
           write(262, "(a)") trim(string_tmp)
         else if (k2 /= 0) then ! Modify icgsolver
            write(string_v, "(I5)") md_icgsolver
            string_tmp = trim(string_c)//" "//trim(adjustl(string_v))//"          # Solver type , 1 = sobekGS_OMP, 2 = sobekGS_OMPthreadsafe, 3 = sobekGS, 4 = sobekGS + Saadilud, 5 = parallel/global Saad, 6 = parallel/Petsc, 7 = parallel/GS"
            write(262, "(a)") trim(string_tmp)
            ja_icgsolverset = 1
         else if (k3 /= 0) then ! Modify restart file name
            string_tmp = trim(string_c)//" "//trim(md_restartfile)//"       # Restart file, only from netcdf-file, hence: either *_rst.nc or *_map.nc"
            write(262, "(a)") trim(string_tmp)
         else if (k7 /= 0) then ! Modify ClassMapFile. Must be before mapfile as we don't check on whole words
            string_tmp = trim(string_c)//" "//trim(md_classmap_file)//"       # ClassMapFile name *.nc"
            write(262, "(a)") trim(string_tmp)
         else if (k4 /= 0) then ! Modify mapfile
            string_tmp = trim(string_c)//" "//trim(md_mapfile)//"       # MapFile name *_map.nc"
            write(262, "(a)") trim(string_tmp)
         else if (k5 /= 0) then ! Modify Partitionfile
            string_tmp = trim(string_c)//" "//trim(md_partitionfile)//"          # *_part.pol, polyline(s) x,y"
            write(262, "(a)") trim(string_tmp)      
         else if (k6 /= 0) then ! Modify FlowGeomFile
            string_tmp = trim(string_c)//" "//trim(md_flowgeomfile)//"       # FlowGeomFile name *.nc"
            write(262, "(a)") trim(string_tmp)
         endif
      endif
   enddo

1212 continue
   close(261)
   close(262)
   return
999 call mess(LEVEL_ERROR, 'Error occurs when generating partition MDU files') 
   close(261)
   close(262)
   return
end subroutine generatePartitionMDUFile


!> create samples in triangle
subroutine create_samples_in_triangle()
   
   use m_polygon
   use m_samples
   use network_data, only: cornercos
   use m_missing
   use m_sferic, only: jsferic, jasfer3D
   use geometry_module, only: dbdistance, dcosphi

   implicit none
   
   double precision, dimension(:,:), allocatable :: xx
   double precision, dimension(:,:), allocatable :: yy
   
   double precision                              :: dcos
   double precision                              :: xi, eta
   double precision                              :: dfac, dfacL, dfacR, RL, RR
                              
   integer                                       :: n1, n2, n3
   integer                                       :: M, N, Nxi, Msize
   integer                                       :: i, j
   integer                                       :: jL, jR
   
   
   if ( NPL.lt.2 ) return
   
!  delete samples
   call delsam(-1)
   
!  find startpoint of triangle in polygon
   n1 = 1
   do while ( xpl(n1).eq.DMISS )
      if ( n1.eq.NPL ) exit
      n1 = n1+1
   end do
   if ( xpl(n1).eq.DMISS ) goto 1234
   
!  find first corner
   n2 = n1+1
   if ( n2.ge.NPL ) goto 1234
   
   dcos = dcosphi(xpl(n2-1),ypl(n2-1),xpl(n2),ypl(n2),xpl(n2),ypl(n2),xpl(n2+1),ypl(n2+1), jsferic, jasfer3D, dxymis)
   do while ( dcos.gt.cornercos )
      n2 = n2+1
      if ( n2.eq.NPL ) goto 1234
      dcos = dcosphi(xpl(n2-1),ypl(n2-1),xpl(n2),ypl(n2),xpl(n2),ypl(n2),xpl(n2+1),ypl(n2+1), jsferic, jasfer3D, dxymis)
   end do
   if ( dcos.gt.cornercos ) goto 1234
   
!  find third corner
   n3 = n2+1
   if ( n3.ge.NPL ) goto 1234
   
   dcos = dcosphi(xpl(n3-1),ypl(n3-1),xpl(n3),ypl(n3),xpl(n3),ypl(n3),xpl(n3+1),ypl(n3+1), jsferic, jasfer3D, dxymis)
   do while ( dcos.gt.cornercos )
      n3 = n3+1
      if ( n3.eq.NPL ) exit
      dcos =  dcosphi(xpl(n3-1),ypl(n3-1),xpl(n3),ypl(n3),xpl(n3),ypl(n3),xpl(n3+1),ypl(n3+1), jsferic, jasfer3D, dxymis)
   end do
   
!  determine dimensions
   M = n2-n1+1
   N = n3-n2+1
   
   if ( n3+M-1.gt.NPL ) goto 1234
   
   Msize = max(M,N)
   allocate(xx(Msize,3))
   xx = 0d0
   allocate(yy(Msize,3))
   yy = 0d0
   do i=1,M
      xx(i,1) = xpl(n1+i-1)-xpl(n1)
      yy(i,1) = ypl(n1+i-1)-ypl(n1)
      xx(i,3) = xpl(n3+M-i)-xpl(n1)
      yy(i,3) = ypl(n3+M-i)-ypl(n1)
   end do
   
   do j=1,N
      xx(j,2) = xpl(n2+j-1)-xpl(n1)
      yy(j,2) = ypl(n2+j-1)-ypl(n1)
   end do
   
   call increasesam(M*N)
   
   Ns=Ns+1
   xs(1) = xpl(n1)
   ys(1) = ypl(n1)
   
   RL = dbdistance(xpl(n1),ypl(n1),xpl(n2),ypl(n2),jsferic, jasfer3D, dmiss)
   RR = dbdistance(xpl(n1),ypl(n1),xpl(n3),ypl(n3),jsferic, jasfer3D, dmiss)
   
   do i=2,M-1
      
      xi = dble(i-1)/dble(M-1)
      Nxi = floor(xi*(N-1)+1)
         
      dfacL = dbdistance(xpl(n1),ypl(n1),xpl(n1)+xx(i,1),ypl(n1)+yy(i,1),jsferic, jasfer3D, dmiss)/RL
      dfacR = dbdistance(xpl(n1),ypl(n1),xpl(n1)+xx(i,3),ypl(n1)+yy(i,3),jsferic, jasfer3D, dmiss)/RR

      do j=2,Nxi-1
         eta = dble(j-1)/dble(Nxi-1)
         
         jL = 1 + floor(eta*(N-1))
         if ( jL.ge.N ) jL=N-1
         jR = jL+1
         
         dfac = 1d0+eta*(N-1)-jL
         
         Ns = Ns+1
!         xs(Ns) = (1d0-xi)*xpl(n1) + xi*( (1-eta)*xpl(n2) + eta*xpl(n3) )
!         ys(Ns) = (1d0-xi)*ypl(n1) + xi*( (1-eta)*ypl(n2) + eta*ypl(n3) )
!         xs(Ns) = xpl(n1) + xi*((1-dfac)*xx(jL,2) + dfac*xx(jR,2))
!         ys(Ns) = ypl(n1) + xi*((1-dfac)*yy(jL,2) + dfac*yy(jR,2))
         
!         xs(Ns) = (1-dfac) * xpl(n1) + dfac*xs(Ns)
!         ys(Ns) = (1-dfac) * ypl(n1) + dfac*ys(Ns)
         
         xs(Ns) = xpl(n1) + (1-dfac)*xx(jL,2) + dfac*xx(jR,2)
         ys(Ns) = ypl(n1) + (1-dfac)*yy(jL,2) + dfac*yy(jR,2)
         
         dfac = (1-eta)*dfacL + eta*dfacR
         
         xs(Ns) = (1-dfac) * xpl(n1) + dfac * xs(Ns)
         ys(Ns) = (1-dfac) * ypl(n1) + dfac * ys(Ns)
      end do
      Ns = Ns+1
      xs(Ns) = xpl(n1)+xx(i,1)
      ys(Ns) = ypl(n1)+yy(i,1)
      Ns = Ns+1
      xs(Ns) = xpl(n1)+xx(i,3)
      ys(Ns) = ypl(n1)+yy(i,3)
   end do
   
   do j=1,N
      Ns = Ns+1
      xs(Ns) = xpl(n1)+xx(j,2)
      ys(Ns) = ypl(n1)+yy(j,2)
   end do
   
   do i=1,Ns
      zs = 0d0
   end do
   
1234 continue

!  deallocate
   if ( allocated(xx) ) deallocate(xx)
   if ( allocated(yy) ) deallocate(yy)
   
   return
end subroutine create_samples_in_triangle

subroutine smooth_samples_from_GUI()
   use m_samples
   implicit none
   
   integer :: N
   
!  check if samples are structured
   if ( MXSAM*MYSAM.ne.NS ) then
      call qnerror('Samples or not structured', ' ', ' ')
      goto 1234
   end if
   
   N = 0
   call getint('Number of smoothing iterations', N)
   
   call savesam()
   call smooth_samples(MXSAM, MYSAM, NS, 1, N, zs, zs)

1234 continue
   
   return
end subroutine smooth_samples_from_GUI


!> debugging subroutine
subroutine inipole(japole)
   use unstruc_model
   use m_flow
   use m_flowgeom
   use m_sferic
   implicit none
   
   integer, intent(in) :: japole !< pole (1) or equator (0)
   
   double precision :: lambda, phi, u
   
   integer :: L
   
   integer :: ierror
   
   integer, external :: flow_modelinit

!  set velocity field
   do L=1,Lnx
      lambda = xu(L)*dg2rd
      phi = yu(L)*dg2rd
      
      if ( japole.eq.1 ) then
         u1(L) = (  sin(phi) * cos(lambda) ) * csu(L) +   &
                 ( -sin(lambda)            ) * snu(L)
                 
         u1(L) = u1(L) / sqrt( sin(phi)**2 * cos(lambda)**2 + sin(lambda)**2 )
      else
         u1(L) = csu(L)
      end if
   end do
   
   return
end subroutine inipole


! compute coordinates (xu, yu) from (x1,y1) and (x2,y2) with
!    weights alpha1 and alpha2
subroutine a1x1a2x2(x1,y1,x2,y2,alpha1,alpha2,xu,yu)
   use m_sferic
   use geometry_module, only: sphertocart3D, Cart3Dtospher
   implicit none
   
   double precision, intent(in)  :: x1, y1
   double precision, intent(in)  :: x2, y2
   double precision, intent(in)  :: alpha1
   double precision, intent(in)  :: alpha2
   double precision, intent(out) :: xu, yu
   
   double precision              :: xx1, yy1, zz1, xx2, yy2, zz2
   double precision              :: xxu, yyu, zzu
   
   if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
      call sphertoCart3D(x1,y1,xx1,yy1,zz1)
      call sphertoCart3D(x2,y2,xx2,yy2,zz2)
      xxu = alpha1*xx1 + alpha2*xx2
      yyu = alpha1*yy1 + alpha2*yy2
      zzu = alpha1*zz1 + alpha2*zz2
      call Cart3Dtospher(xxu,yyu,zzu,xu,yu,max(x1,x2))
   else
      xu = alpha1*x1 + alpha2*x2
      yu = alpha1*y1 + alpha2*y2
   end if
   
   return
end subroutine a1x1a2x2


!> generate curvilinear mesh in polygon, based on three polygon nodes that define two sides 1-2 and 2-3
!>    the third side 3-4 is defined by the polygon nodes by matching the number of nodes with side 1-2
!>    ja4=1: the fourth side is also taken from the polygon, starting from 1 and matching the number of nodes with side 2-3, the end-node will not necessarily be 4
!>    ja4=0: the fourth side is linearly interpolated between nodes 1 and 4, polygon nodes beyond 4 not used
!>
!>   1 x - - - - - x 4
!>     |           |
!>     |           |
!>     |           |
!>   2 x-----------x 3
subroutine pol2curvi(i1, i2, i3,ja4)
   use m_grid
   use m_gridsettings
   use m_alloc
   use m_missing
   use m_polygon
   implicit none
   
   integer,                          intent(in)  :: i1, i2, i3  !< first, second and third corner point in polygon, respectively
   integer,                          intent(in)  :: ja4         !< use polygon for fourth side (1) or not (0)
   
   double precision, dimension(:,:), allocatable :: xh, yh  ! mesh boundary coordinates
   
   double precision                              :: xi
                                                
   integer                                       :: i4, Nh
   integer                                       :: istart, iend
   integer                                       :: i, j, mcL, mcR, ncL, ncR
   
   integer                                       :: idum, idir, ipoint, num, numsubpol
                                                
   integer                                       :: ierror       ! error (1) or not (0)
   
   ierror = 1
   
   if ( NPL.le.4 ) goto 1234
   
!  get start and end pointers in polygon
   call get_polstartend(NPL, XPL, YPL, i1, istart, iend)
   numsubpol = iend-istart+1
   
!  get grid size and orientation
   mcR = i2-i1; if ( mcR.lt.0 ) mcR = mcR+numsubpol
   mcL = i1-i2; if ( mcL.lt.0 ) mcL = mcL+numsubpol
   
   if ( mcR.le.mcL ) then
      idir = 1
      mc = mcR+1
   else
      idir = -1
      mc = mcL+1
   end if
   
   ncR = i3-i2; if ( ncR.lt.0 ) ncR = ncR+numsubpol
   ncL = i2-i3; if ( ncL.lt.0 ) ncL = ncL+numsubpol
   
   if ( idir.eq.1 ) then
      nc = ncR+1
   else
      nc = ncL+1
   end if
   
!  get fourth corner index
   i4 = i3+idir*(mc-1)
   if ( i4.lt.istart ) i4 = i4 + numsubpol
   if ( i4.gt.iend )   i4 = i4 - numsubpol
   
!  check if polygon suffices
   if ( ja4.eq.1 ) then
      num = 2*(mc-1) + 2*(nc-1)
   else
      num = 1+2*(mc-1)+(nc-1)
   end if
   
   if ( num.gt.numsubpol ) then
!     polygon not large enough
      call qnerror('polygon is not large enough', ' ', ' ')
      goto 1234
   end if
   
!  allocate array with boundary coordinates
   Nh = max(mc,nc)
   allocate(xh(Nh,4))
   allocate(yh(Nh,4))
   
!  fill boundary coordinates
   if ( ja4.eq.1 ) then
!     fourth side from polygon
      ipoint = i1
      do j=1,nc
         xh(j,1) = xpl(ipoint)
         yh(j,1) = ypl(ipoint)
         ipoint = ipoint-idir
         if ( ipoint.lt.istart ) ipoint = ipoint + numsubpol
         if ( ipoint.gt.iend )   ipoint = ipoint - numsubpol
      end do
   else
!     interpolate fourth side
      do i=1,nc
         xi = dble(i-1)/dble(nc-1)
         xh(i,1) = (1d0-xi)*xpl(i1) + xi*xpl(i4)
         yh(i,1) = (1d0-xi)*ypl(i1) + xi*ypl(i4)
      end do
   end if
   
   ipoint = i2
   do j=1,nc
      xh(j,2) = xpl(ipoint)
      yh(j,2) = ypl(ipoint)
      ipoint = ipoint+idir
      if ( ipoint.lt.istart ) ipoint = ipoint + numsubpol
      if ( ipoint.gt.iend )   ipoint = ipoint - numsubpol
   end do
   
   ipoint = i1
   do i=1,mc
      xh(i,3) = xpl(ipoint)
      yh(i,3) = ypl(ipoint)
      ipoint = ipoint+idir
      if ( ipoint.lt.istart ) ipoint = ipoint + numsubpol
      if ( ipoint.gt.iend )   ipoint = ipoint - numsubpol
   end do
   
   ipoint = i4
   do i=1,mc
      xh(i,4) = xpl(ipoint)
      yh(i,4) = ypl(ipoint)
      ipoint = ipoint-idir
      if ( ipoint.lt.istart ) ipoint = ipoint + numsubpol
      if ( ipoint.gt.iend )   ipoint = ipoint - numsubpol
   end do
   
!  increase grid
   call increasegrid(mc,nc)   
   
   MFAC = mc-1
   NFAC = nc-1
   
!  make grid
   CALL TRANFN2( xh(1,1), xh(1,2), xh(1,3), xh(1,4),            &  ! . 3 .       . 4 .
                 yh(1,1), yh(1,2), yh(1,3), yh(1,4),            &  ! 4   2       1   2
                 Nh, MMAX, NMAX, XC, YC)                           ! . 1 .       . 3 .
   
   ierror = 0
1234 continue

   if ( ierror.ne.0 ) then
      mc = 0
      nc = 0
   end if

!  deallocate
   if ( allocated(xh) ) deallocate(xh)
   if ( allocated(yh) ) deallocate(yh)
   
   return
end subroutine pol2curvi


!> construct triangle with three blocks of curvilinear grids
subroutine pol2curvi_tri(i1, i2_, i3_)
   use m_grid
   use m_gridsettings
   use m_alloc
   use m_missing
   use m_polygon
   implicit none
   
   integer, intent(in)   :: i1, i2_, i3_  !< first, second and third corner point in polygon, respectively
   
   double precision, dimension(:,:), allocatable  :: xh, yh, xg, yg
   
   double precision                               :: xm, ym
   double precision                               :: xia, xib, xic
   
   integer,                          dimension(3) :: M, N, i0, ileft, iright
   
   integer                                        :: i2, i3                                 
   integer                                        :: istart, iend
   integer                                        :: numsubpol
   integer                                        :: Na, Nb, Ncc ! length of triangle sides
   integer                                        :: N1, N2, N3
   integer                                        :: isum, itri, Nh
   integer                                        :: ia, ib, ic, i, j
   integer                                        :: ipoint, idir
   integer                                        :: key
                                                 
   integer                                        :: ierror
   
   ierror = 1
   
   if ( NPL.le.4 ) goto 1234
   
!  get start and end pointers in polygon
   call get_polstartend(NPL, XPL, YPL, i1, istart, iend)
   numsubpol = iend-istart+1
   
!  check if number of points is even
   if ( mod(numsubpol,2).eq.1 ) then
      call qnerror('Number of points needs to be even', ' ', ' ')
   end if
   
   idir = 1
   i2 = i2_
   i3 = i3_
   
!  get grid size and orientation
   Na  = i2-i1; if ( Na.lt.1 ) Na  = Na+numsubpol
   Nb  = i3-i2; if ( Nb.lt.1 ) Nb  = Nb+numsubpol
   Ncc = numsubpol-(Na+Nb)
   
   if ( Ncc.lt.1 ) then
      i2 = i3_
      i3 = i2_
      Na  = i2-i1; if ( Na.lt.1 ) Na  = Na+numsubpol
      Nb  = i3-i2; if ( Nb.lt.1 ) Nb  = Nb+numsubpol
      Ncc = numsubpol-(Na+Nb)
   end if
   
!  get block sizes (N1 x N3), (N2 x N3), (N1 X N2)
   isum = (Na+Nb+Ncc)/2
   N1 = isum - Ncc
   N2 = isum - Nb
   N3 = isum - Na
   
   if ( N1.lt.1 .or. N2.lt.1 .or. N3.lt.1 ) then
      call qnerror('unable to get block dimensions', ' ', ' ')
      goto 1234
   end if
   
!  compute midpoint
   ia = i1 + N1; if ( ia.gt.iend ) ia=ia-numsubpol
   ib = i2 + N3; if ( ib.gt.iend ) ib=ib-numsubpol
   ic = i3 + N2; if ( ic.gt.iend ) ic=ic-numsubpol
   
   
!  set dimensions of blocks   
   M = (/ N1, N3, N2 /)
   N = (/ N3, N2, N1 /)
   
!  set pointers of block corners
!      ileft ------------------
!           |                  |
!           |                  |
!           |                  |
!           |------------------|
!          0                   iright
   
   i0     = (/ i1, i2, i3 /)
   ileft  = (/ ic, ia, ib /)
   iright = (/ ia, ib, ic /)
   
!   xia = dbdistance(XPL(i1),YPL(i1),XPL(ia),YPL(ia)) / dbdistance(XPL(i1),YPL(i1),XPL(i2),YPL(i2))
!   xib = dbdistance(XPL(i2),YPL(i2),XPL(ib),YPL(ib)) / dbdistance(XPL(i2),YPL(i2),XPL(i3),YPL(i3))
!   xic = dbdistance(XPL(i3),YPL(i3),XPL(ic),YPL(ic)) / dbdistance(XPL(i3),YPL(i3),XPL(i1),YPL(i1))
   
   xia = dble(N1)/dble(Na)
   xib = dble(N3)/dble(Nb)
   xic = dble(N2)/dble(Ncc)
   
   xm = ( ((1d0-xia)*XPL(i1) + xia*XPL(i2)) * xic + (1d0-xic)*XPL(i3) +    &
          ((1d0-xib)*XPL(i2) + xib*XPL(i3)) * xia + (1d0-xia)*XPL(i1) +    &
          ((1d0-xic)*XPL(i3) + xic*XPL(i1)) * xib + (1d0-xib)*XPL(i2) ) / 3d0
          
   ym = ( ((1d0-xia)*YPL(i1) + xia*YPL(i2)) * xic + (1d0-xic)*YPL(i3) +    &
          ((1d0-xib)*YPL(i2) + xib*YPL(i3)) * xia + (1d0-xia)*YPL(i1) +    &
          ((1d0-xic)*YPL(i3) + xic*YPL(i1)) * xib + (1d0-xib)*YPL(i2) ) / 3d0
   
!  allocate arrays with boundary coordinates
   Nh = max(maxval(M),maxval(N))+1
   allocate(xh(Nh,4))
   allocate(yh(Nh,4))
   
!  prepare grid  
   MC = N1+N3+1
   NC = N2+N3+1
      
!  increase grid
   call increasegrid(MC,NC)
   xc = DMISS
   yc = DMISS
   
!  fill coordinates of blocks
   do itri=1,3
   
      xh = DMISS
      yh = DMISS
   
      ipoint = i0(itri)
      do i=1,N(itri)+1
         xh(i,1) = XPL(ipoint)
         yh(i,1) = YPL(ipoint)
         ipoint = ipoint-idir
         if ( ipoint.lt.istart ) ipoint = ipoint + numsubpol
         if ( ipoint.gt.iend )   ipoint = ipoint - numsubpol
      end do
      
      ipoint = i0(itri)
      do i=1,M(itri)+1
         xh(i,3) = XPL(ipoint)
         yh(i,3) = YPL(ipoint)
         ipoint = ipoint+idir
         if ( ipoint.lt.istart ) ipoint = ipoint + numsubpol
         if ( ipoint.gt.iend )   ipoint = ipoint - numsubpol
      end do
      
      do i=1,M(itri)+1
         xia = dble(i-1)/dble(M(itri))
         xh(i,4) = (1d0-xia) * XPL(ileft(itri)) + xia * xm
         yh(i,4) = (1d0-xia) * YPL(ileft(itri)) + xia * ym
      end do
      
      do i=1,N(itri)+1
         xia = dble(i-1)/dble(N(itri))
         xh(i,2) = (1d0-xia) * XPL(iright(itri)) + xia * xm
         yh(i,2) = (1d0-xia) * YPL(iright(itri)) + xia * ym
      end do
   
!     allocate arrays with grid coordinates
      call realloc(xg, (/M(itri)+1,N(itri)+1/), keepExisting=.false., fill=DMISS)
      call realloc(yg, (/M(itri)+1,N(itri)+1/), keepExisting=.false., fill=DMISS)
      
      MFAC = M(itri)
      NFAC = N(itri)
      
!     make block coordinates
      CALL TRANFN2( xh(1,1), xh(1,2), xh(1,3), xh(1,4),            &  ! . 3 .       . 4 .
                    yh(1,1), yh(1,2), yh(1,3), yh(1,4),            &  ! 4   2       1   2
                    Nh, M(itri)+1, N(itri)+1, xg, yg)
       
!     add to grid
      select case(itri)
         case(1)
            do j=1,N3+1
               do i=1,N1+1
                  xc(i,j) = xg(i,j)
                  yc(i,j) = yg(i,j)
               end do
            end do
         case(2)
            do j=1,N2+1
               do i=1,N3+1
                  xc(N1+N3+2-i,N2+N3+2-j) = xg(i,j)
                  yc(N1+N3+2-i,N2+N3+2-j) = yg(i,j)
               end do
            end do
         case(3)
            do j=1,N1+1
               do i=1,N2+1
                  xc(j,N2+N3+2-i) = xg(i,j)
                  yc(j,N2+N3+2-i) = yg(i,j)
               end do
            end do
      end select
      
!      key = 1
!      call tekgrid(key)
!      call qnerror(' ', ' ', ' ')
      
   end do
   
   ierror = 0
1234 continue

   if ( allocated(xh) ) deallocate(xh)
   if ( allocated(yh) ) deallocate(yh)
   if ( allocated(xg) ) deallocate(xg)
   if ( allocated(yg) ) deallocate(yg)
   
   return
end subroutine pol2curvi_tri


!> linear interpolation of z-values in polylines
subroutine interpolate_zpl_in_polylines()
   use m_polygon
   use m_missing
   use geometry_module, only: dbdistance, get_startend
   use m_sferic, only: jsferic, jasfer3D

   implicit none
   
   double precision, dimension(:), allocatable :: wfromLeft       ! arc length from left
   
   integer,          dimension(:), allocatable :: iLeft, iRight   ! left and right node for interpolation, respectively
   
   double precision                            :: wL, wR, w
   
   integer                                     :: jstart, jend
   integer                                     :: jpoint
   integer                                     :: i, iL, iR
   
   integer                                     :: ierror
   double precision, parameter                 :: dtol = 1d-8
   
   ierror = 1
   
   if ( NPL.lt.2 ) goto 1234
   
!  allocate
   allocate(wfromleft(NPL))
   allocate(ileft(NPL))
   allocate(iRight(NPL))
   
   jpoint=1
   
   do while ( jpoint.lt.NPL )
   
!     get subpolyline 
      call get_startend(NPL-jpoint+1,xpl(jpoint:NPL),ypl(jpoint:NPL),jstart,jend, dmiss)

      jstart = jstart+jpoint-1
      jend   = jend+jpoint-1
      
!     compute arc lengths from left
      wfromLeft(jstart) = 0d0
      do i=jstart, jend-1
         wfromLeft(i+1) = wfromLeft(i) + dbdistance(xpl(i),ypl(i),xpl(i+1),ypl(i+1), jsferic, jasfer3D, dmiss)
      end do
      
!     get left nodes for interpolation
      iL = 0
      do i=jstart, jend
         if ( zpl(i).ne.DMISS ) then
            iL = i
         end if
         iLeft(i) = iL
      end do
      
!     get right nodes for interpolation
      iR = 0
      do i=jend,jstart,-1
         if ( zpl(i).ne.DMISS ) then
            iR = i
         end if
         iRight(i) = iR
      end do
      
!     interpolate values
      do i=jstart,jend
         iL = iLeft(i)
         iR = iRight(i)
         wL=0d0; if ( iL.gt.0 ) wL = wfromLeft(iL)
         wR=0d0; if ( iR.gt.0 ) wR = wfromLeft(iR)
         
         if ( iL.eq.iR .and. iL.ne.0 ) then
!           value prescibed
            if ( i.ne.iL ) then
               call qnerror('interpolate_zpl_in_polylines: error', ' ', ' ')
            end if
         else if ( iL.gt.0 .and. iR.gt.0 ) then
!           two-sided interpolation            
            if ( abs(wR-wL).gt.dtol ) then
               w = (wfromLeft(i)-wL) / (wR-wL)
               zpl(i) = (1d0-w)*zpl(iL) + w*zpl(iR)
            else
!              left and right node on top
               zpl(i) = 0.5d0*(zpl(iL)+zpl(iR))
            end if
         else if ( iL.gt.0 ) then
!           one-sided interpolation
            zpl(i) = zpl(iL)
         else if ( iR.gt.0 ) then
!           one-sided interpolation
            zpl(i) = zpl(iR)
         else
!           nothing to interpolate
         end if
      end do
      
!     proceed to next polyline, if available         
      jpoint = jend+1
   end do
   
   ierror = 0
1234 continue
   
!  deallocate
   if ( allocated(wfromLeft) ) deallocate(wfromLeft)
   if ( allocated(iLeft)     ) deallocate(iLeft)
   if ( allocated(iRight)    ) deallocate(iRight)
   
   return
end subroutine interpolate_zpl_in_polylines
