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

! $Id: rgfstuff.f90 65778 2020-01-14 14:07:42Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/rgfstuff.f90 $
     MODULE M_MAPPROPARAMETERS
     implicit none
      double precision :: XCE,YCE,DELTX,DELTY,XF,YF,FI, XCSTEREO, YCSTEREO
      double precision :: CSE=1D0,SNE=0D0
      INTEGER          :: ITYPE=0, IZONE=0, NZONE=0, IHEM=1
      END MODULE M_MAPPROPARAMETERS
      
     
      SUBROUTINE ANDERSOM(X,N)
      
      use m_alloc
      
      implicit none
      integer :: n
      double precision              :: X(N)
      integer :: i, ierr
      double precision, ALLOCATABLE :: XH(:)
      ALLOCATE ( XH(N), stat=ierr )
      call aerr('XH(N)', ierr, N)
      XH = X
      DO I= 1,N
         X(I) = XH(N-I+1)
      ENDDO

      DEALLOCATE (XH)
      call aerr('XH', ierr, -N)
      END SUBROUTINE ANDERSOM

      SUBROUTINE TRANFN2(    X1,     X2,     X3,     X4,             &  ! WAS B
                             Y1,     Y2,     Y3,     Y4,             &
                            IMX, MX, NX,    XRH,  YRH)

      USE M_GRIDSETTINGS
      use m_orthosettings, only: ITIN
      implicit none
      double precision :: db
      double precision :: dl
      double precision :: don
      double precision :: dr
      double precision :: ds
      double precision :: dx
      double precision :: dx1
      double precision :: dx2
      double precision :: dx3
      double precision :: dx4
      double precision :: dy
      double precision :: dy1
      double precision :: dy2
      double precision :: dy3
      double precision :: dy4
      double precision :: f1
      double precision :: f2
      double precision :: f3
      double precision :: f4
      integer :: i
      integer :: ierr
      integer :: imx
      integer :: j
      integer :: k
      integer :: key
      integer :: mixelliptic
      integer :: mm
      integer :: mnx
      integer :: mx
      integer :: nn
      integer :: nx
      double precision :: rb
      double precision :: rl
      double precision :: ro
      double precision :: rr
      double precision :: w
      double precision :: w12
      double precision :: w34
      double precision :: wa
      double precision :: xh
      double precision :: xy
      double precision :: yh

      double precision , DIMENSION(:,:), ALLOCATABLE  :: X1V,Y1V,X2V,Y2V,     &
                                                          X3V,Y3V,X4V,Y4V,     &
                                                          SI ,SJ, W1, W2, W3, W4
      double precision , DIMENSION (:), ALLOCATABLE   :: D1 ,D2, D3, D4, TI, TJ

      double precision :: XRH(MX,NX),YRH(MX,NX),                    &
                          X1(IMX), X2(IMX), X3(IMX), X4(IMX),       &
                          Y1(IMX), Y2(IMX), Y3(IMX), Y4(IMX)
      DOUBLE PRECISION :: DDX, DDY, RI, RJ, T1, T2, T3, T4
!      double precision , DIMENSION(:,:), ALLOCATABLE  :: X1V,Y1V,X2V,Y2V,     &
!                                                          X3V,Y3V,X4V,Y4V,     &
!                                                          SI ,SJ, W1, W2, W3, W4
!      double precision , DIMENSION (:), ALLOCATABLE   :: D1 ,D2, D3, D4, TI, TJ
!
!      double precision :: XRH(MX,NX),YRH(MX,NX),                    &
!                          X1(IMX), X2(IMX), X3(IMX), X4(IMX),       &
!                          Y1(IMX), Y2(IMX), Y3(IMX), Y4(IMX)
!      DOUBLE PRECISION :: DDX, DDY, RI, RJ, T1, T2, T3, T4


       CHARACTER TEX*4
!     1,2    VERTICALEN
!     3,4    HORIZONTALEN
!     D1234  REL. LIJN COORDINAAT 0-1
!     SI,SJ  REL. VELD COORDINAAT 0-1
!     TI,TJ  SCHATTING KOORDELENGTES
!     W1234  VELD WEEGFACTOR
!     X1V    SCHATTING VELDCOORDINAAT VANUIT VERTICAAL 1

       NN  = NFAC + 1
       MM  = MFAC + 1
       MNX  = MAX(MM, NN)
       ALLOCATE(X1V(MM,NN),STAT=IERR)
       ALLOCATE(Y1V(MM,NN),STAT=IERR)
       ALLOCATE(X2V(MM,NN),STAT=IERR)
       ALLOCATE(Y2V(MM,NN),STAT=IERR)
       ALLOCATE(X3V(MM,NN),STAT=IERR)
       ALLOCATE(Y3V(MM,NN),STAT=IERR)
       ALLOCATE(X4V(MM,NN),STAT=IERR)
       ALLOCATE(Y4V(MM,NN),STAT=IERR)
       ALLOCATE(si (MM,NN),STAT=IERR)
       ALLOCATE(sj (MM,NN),STAT=IERR)
       ALLOCATE(W1 (MM,NN),STAT=IERR)
       ALLOCATE(W2 (MM,NN),STAT=IERR)
       ALLOCATE(W3 (MM,NN),STAT=IERR)
       ALLOCATE(W4 (MM,NN),STAT=IERR)

       ALLOCATE(D1 (MNX),STAT=IERR)
       ALLOCATE(D2 (MNX),STAT=IERR)
       ALLOCATE(D3 (MNX),STAT=IERR)
       ALLOCATE(D4 (MNX),STAT=IERR)
       ALLOCATE(TI (MNX),STAT=IERR)
       ALLOCATE(TJ (MNX),STAT=IERR)



       CALL ABREL2(X1,Y1,D1,NN,T1)
       CALL ABREL2(X2,Y2,D2,NN,T2)
       CALL ABREL2(X3,Y3,D3,MM,T3)
       CALL ABREL2(X4,Y4,D4,MM,T4)

       DO I = 1,MM
          DO J = 1,NN
             RI   = DBLE(I-1)/DBLE(MFAC)                       ! INDEXWEGING I
             RJ   = DBLE(J-1)/DBLE(NFAC)                       ! INDEXWEGING J

             SI(I,J) = (1D0-RJ)*D3(I) + RJ*D4(I)               ! AFSTANDSWEGING I
             SJ(I,J) = (1D0-RI)*D1(J) + RI*D2(J)               ! AFSTANDSWEGING J

          ENDDO
       ENDDO



       DO I = 1,MM
          DO J  = 1,NN
             W1(I,J) = (1D0-SJ(I,J))*T3 + SJ(I,J)*T4          ! AFSTANDSGEWOGEN TOTALE KOORDELENGTE I-RICHTING
             W2(I,J) = (1D0-SI(I,J))*T1 + SI(I,J)*T2          ! AFSTANDSGEWOGEN TOTALE KOORDELENGTE J-RICHTING
             W3(I,J) = W2(I,J) / W1(I,J)                      ! ATPF
             W4(I,J) = W1(I,J) / W2(I,J)
             WA      = 1D0 / ( W3(I,J) + W4(I,J) )
             W1(I,J) = WA*W3(I,J)
             W2(I,J) = WA*W4(I,J)
          ENDDO
       ENDDO

       DO I = 1,MM                                             ! randen
         XRH(I,1)  = X3(I)
         XRH(I,NN) = X4(I)
         YRH(I,1)  = Y3(I)
         YRH(I,NN) = Y4(I)
       ENDDO

       DO J = 1,NN
         XRH(1,J)  = X1(J)
         XRH(MM,J) = X2(J)
         YRH(1,J)  = Y1(J)
         YRH(MM,J) = Y2(J)
       ENDDO

       DO I = 2,MM-1                                           ! BINNENGEBIED 1E SCHATTING MET RANDPUNTEN
          DO J = 2,NN-1

             XRH(I,J) = ( (1D0-SI(I,J))*X1(J) + SI(I,J)*X2(J) ) * W1(I,J) +   &
                        ( (1D0-SJ(I,J))*X3(I) + SJ(I,J)*X4(I) ) * W2(I,J)
             YRH(I,J) = ( (1D0-SI(I,J))*Y1(J) + SI(I,J)*Y2(J) ) * W1(I,J) +   &
                        ( (1D0-SJ(I,J))*Y3(I) + SJ(I,J)*Y4(I) ) * W2(I,J)
          ENDDO
       ENDDO

       ! CALL TEKGRD(XRH,YRH,MM,NN,1,1,MM,NN,31,2,KEY,MM)
       ! CALL WAITESC()


       DO I = 1,MM                                             ! EVEN TERUGGEZET
          DO J  = 1,NN
             W1(I,J) = (1D0-SJ(I,J))*D3(I)*T3 + SJ(I,J)*D4(I)*T4     ! AFSTANDSGEWOGEN KOORDELENGTE I-RICHTING
             W2(I,J) = (1D0-SI(I,J))*D1(J)*T1 + SI(I,J)*D2(J)*T2     ! AFSTANDSGEWOGEN KOORDELENGTE J-RICHTING
          ENDDO
       ENDDO

       DO I = 2,MM                                             ! DI TUSSEN NETNODES
          DO J  = 1,NN
             W3(I,J) = W1(I,J) - W1(I-1,J)
          ENDDO
       ENDDO

       DO I = 1,MM                                             ! DJ TUSSEN NETNODES
          DO J  = 2,NN
             W4(I,J) = W2(I,J) - W2(I,J-1)
          ENDDO
       ENDDO

       DO I = 2,MM                                             ! atpI over cellen
          DO J  = 2,NN-1
             W1(I,J) = 0.25D0*( W4(I,J) + W4(I,J+1) + W4(I-1,J) + W4(I-1,J+1) ) / W3(I,J)
          ENDDO
       ENDDO


       DO I = 2,MM-1                                           ! atpJ over cellen
          DO J  = 2,NN
             W2(I,J) = 0.25D0*( W3(I,J) + W3(I,J-1) + W3(I+1,J) + W3(I+1,J-1) ) / W4(I,J)
          ENDDO
       ENDDO





!        W1 = W3; W2 = W4

       DO K  = 1, ITIN

          W3 = XRH
          W4 = YRH
          DO I = 2,MM-1                                        ! BINNEN
             DO J = 2,NN-1

                WA       = 1D0 / ( W1(I,J) + W1(I+1,J) + W2(I,J) +  W2(I  ,J+1) )

                XRH(I,J) = WA*   ( W3(I-1,J  )*W1(I,J) +  W3(I+1,J  )*W1(I+1,J  ) +  &
                                   W3(I  ,J-1)*W2(I,J) +  W3(I  ,J+1)*W2(I  ,J+1) )

                YRH(I,J) = WA*   ( W4(I-1,J  )*W1(I,J) +  W4(I+1,J  )*W1(I+1,J  ) +  &
                                   W4(I  ,J-1)*W2(I,J) +  W4(I  ,J+1)*W2(I  ,J+1) )

             ENDDO
          ENDDO

          ! hier nog een bnd lus zeker weten goed!

          ! CALL TEKGRD(XRH,YRH,MM,NN,1,1,MM,NN,31,2,KEY,MM)
          ! CALL WAITESC()

       ENDDO


       DEALLOCATE(X1V,Y1V,X2V,Y2V,X3V,Y3V,X4V,Y4V,si,sj,W1,W2,W3,W4)

       DEALLOCATE ( D1, D2, D3, D4, TI, TJ )

       RETURN
       END subroutine tranfn2


      SUBROUTINE ABREL2(X,Y,D,NN,T)
      
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D
      
      implicit none
      integer :: j
      integer :: nn
      double precision :: X(NN), Y(NN), D(NN)
      DOUBLE PRECISION :: T
      D(1) = 0
      DO 10 J = 2,NN
         D(J) = D(J-1) + DBDISTANCE( X(J-1),Y(J-1), X(J), Y(J), jsferic, jasfer3D, dmiss)
    10 CONTINUE
      T = D(NN)

      DO 20 J = 1,NN
         D(J) = D(J)/T
    20 CONTINUE
      RETURN
      END SUBROUTINE ABREL2

      SUBROUTINE ORTHOGRID(M1,N1,M2,N2)
      use unstruc_colors
      USE M_GRID
      USE M_SFERIC
      USE M_GRIDSETTINGS
      use m_orthosettings
      implicit none
      integer :: in
      integer :: it
      integer :: jdla
      integer :: ma1
      integer :: ma2
      integer :: mcr
      integer :: mx
      integer :: na1
      integer :: na2
      integer :: ncr
      integer :: ndraw
      integer :: num
      integer :: nx
      double precision :: rjac

      DOUBLE PRECISION , DIMENSION(:,:), ALLOCATABLE :: XR,YR,XI2,XJ2,YI2,YJ2,  &
                                                        A,B,C,D,E,ATP,XO,YO

      INTEGER :: M1, N1, M2, N2

      COMMON /DRAWTHIS/ ndraw(50)

      CHARACTER*76 FILNAM

      IF (MC .EQ. 0) THEN
         CALL QNERROR('First Create or Load a Grid',' ',' ')
         NUM = 0
         RETURN
      ENDIF

      CALL SAVEgrd()

      MX = MMAX ; NX = NMAX
      ALLOCATE (XR(MX,NX),YR(MX,NX),XI2(MX,NX),XJ2(MX,NX),YI2(MX,NX), YJ2(MX,NX),   &
                 A(MX,NX),B(MX,NX),C(MX,NX),D(MX,NX),E(MX,NX),                      &
                 ATP(MX,NX), XO(MX,NX), YO(MX,NX)                                   )

      IN  = 1
      PI  = ACOS(-1d0)
      MCR = MC
      NCR = NC

      IF (NDRAW(8) .EQ. 0) CALL READYY('ORTHOGONALISATION',0d0)
      CALL ISITU ( )
      IF (NDRAW(8) .EQ. 0) CALL READYY(' ',0.05d0)

      IF (JSFERIC .EQ. 1)  CALL MAKEF(XC,YC,MMAX,NMAX)

      CALL GETSPL2(     XC,    XI2,    XJ2,     MC,     NC, MMAX,NMAX)
      IF (NDRAW(8) .EQ. 0) CALL READYY(' ',0.10d0)

      CALL GETSPL2(     YC,    YI2,    YJ2,     MC,     NC, MMAX,NMAX)
      IF (NDRAW(8) .EQ. 0) CALL READYY(' ',0.15d0)

      XR = XC
      YR = YC

      RJAC  = 0.9d0
!     RJAC1 = (COS(PI/MCR) * (XM**2)*COS(PI/NCR)) / (1 + XM**2)
!     RJAC2 = 2*(COS(PI/MCR)/XM + COS(PI/NCR)) / (1 + 1/XM)
!     VUL DE COEFFICIENTEN-MATRICES
      DO 10 IT = 1,ITATP
         JDLA   = 0
         IF (IT .EQ. 1) JDLA = 1
         MA1  = MAX(1,M1-1)
         NA1  = MAX(1,N1-1)
         MA2  = MIN(MC-1,M2)
         NA2  = MIN(NC-1,N2)

         CALL ATPPAR(XR,YR,MA1,NA1,MA2,NA2,ATP,A,B,C,D,E,JDLA)

!        JAMMER IN DEZE LOOP, IJC WORDT EERST VERKLOOT IN SOMDIST
!        CALL SETINTERNALBOUNDARIES(IJC)
         CALL FIXDDBOUNDARIES()
         IF (NDRAW(8) .EQ. 0) CALL READYY('ORTHOGONALISATION',0.20d0)

         CALL ORTSOR(XR,YR,A,B,C,D,E,ATP,M1,N1,M2,N2,     &
                     XI2,YI2,XJ2,YJ2,XO,YO,               &
                     RJAC)
    10 CONTINUE

      IF (NDRAW(8) .EQ. 0) CALL READYY('ORTHOGONALISATION',-1d0)

      XC = XR ; YC = YR


      IF (JSFERIC .EQ. 1) CALL MAKEY(XC,YC,MMAX,NMAX)
!     CALL TEKSHOW(X, Y, MA2, NA2, ATP, 2,'FINAL ATP')

      DEALLOCATE (XR,YR,XI2,XJ2,YI2,YJ2,A,B,C,D,E,ATP,XO,YO)

      RETURN
      END SUBROUTINE ORTHOGRID

      SUBROUTINE ORTSOR(XR,YR,A,B,C,D,E,ATP,M1,N1,M2,N2,     &
                        XI2,YI2,XJ2,YJ2,XO,YO,             &
                        RJAC)
      use unstruc_colors
      use m_sferic
      use m_grid
      use m_gridsettings
      use m_orthosettings
      implicit none
      integer :: i
      integer :: key
      integer :: m1
      integer :: m2
      integer :: n1
      integer :: n2
      integer :: ndraw
      double precision :: rjac

      DOUBLE PRECISION :: XR(MMAX,NMAX), YR(MMAX,NMAX),   &
                         XI2(MMAX,NMAX),XJ2(MMAX,NMAX),                                &
                         YI2(MMAX,NMAX),YJ2(MMAX,NMAX),                                &
                          XO(MMAX,NMAX), YO(MMAX,NMAX),                                &
                           A(MMAX,NMAX),  B(MMAX,NMAX), C(MMAX,NMAX),                  &
                           D(MMAX,NMAX),  E(MMAX,NMAX),ATP(MMAX,NMAX)
      COMMON /DRAWTHIS/ ndraw(50)

      DO 10 I = 1,ITBND

         IF (NDRAW(8) .EQ. 0) CALL READYY(' ',0.25d0 + 0.75d0*( dble(I-1 + 0.20d0)/dble(ITBND) ))
         CALL SOR(A,B,C,D,E,XR,RJAC,M1,N1,M2,N2)

         IF (NDRAW(8) .EQ. 0) CALL READYY(' ',0.25d0 + 0.75d0*( dble(I-1 + 0.60d0)/dble(ITBND) ))
         CALL SOR(A,B,C,D,E,YR,RJAC,M1,N1,M2,N2)

         IF (NDRAW(8) .EQ. 0) CALL READYY(' ',0.25d0 + 0.75d0*( dble(I-1 + 1.00d0)/dble(ITBND) ))

         CALL BNDSMT(XR,YR,XI2,YI2,XJ2,YJ2,ATP,M1,N1,M2,N2)

         IF (NDRAW(8) .EQ. 1 .AND. MDESIGN .NE. 5) THEN
            IF (JSFERIC .EQ. 1) THEN
               CALL MAKEY2(XR,YR,XO,YO,MMAX,NMAX)
               CALL TEKGRD(XO,YO,MMAX,NMAX,M1,N1,M2,N2,NCOLDG,NDRAW(38),KEY,MC)
            ELSE
               CALL TEKGRD(XR,YR,MMAX,NMAX,M1,N1,M2,N2,NCOLDG,NDRAW(38),KEY,MC)
            ENDIF
         ENDIF
    10 CONTINUE
      RETURN
      END SUBROUTINE ORTSOR

      SUBROUTINE MAKEF(XR,YR,MMAX,NMAX) ! naar rekenvlak SUBROUTINE MAKEF
      USE M_SFERIC
      USE M_MISSING
      implicit none
      integer :: mmax,nmax
      DOUBLE PRECISION :: XR(MMAX,NMAX), YR(MMAX,NMAX), FI2
      integer :: i,j
      DO I = 1,MMAX
         DO J = 1,NMAX
            IF (XR(I,J) .NE. DXYMIS) THEN
               FI2     = DG2RD*YR(I,J)
               YR(I,J) = ( 1D0 + SIN(FI2) ) / COS(FI2)
               YR(I,J) = LOG(YR(I,J))
               XR(I,J) = DG2RD*XR(I,J)
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END SUBROUTINE MAKEF

      SUBROUTINE MAKEF1D(XR,YR,MNMAX) ! naar rekenvlak SUBROUTINE MAKEF1D
      USE M_SFERIC
      USE M_MISSING
      implicit none
      integer :: mnmax
      DOUBLE PRECISION :: XR(MNMAX), YR(MNMAX), FI2
      integer :: i

      DO I = 1,MNMAX
         IF (XR(I) .NE. DXYMIS) THEN
            FI2     = DG2RD*YR(I)
            YR(I) = ( 1D0 + SIN(FI2) ) / COS(FI2)
            YR(I) = LOG(YR(I))
            XR(I) = DG2RD*XR(I)
         ENDIF
      ENDDO
      RETURN
      END SUBROUTINE MAKEF1D


      SUBROUTINE MAKEY(XR,YR,MMAX,NMAX)  ! terug naar graden SUBROUTINE MAKEY
      USE M_SFERIC
      USE M_MISSING
      implicit none
      integer :: mmax,nmax

      DOUBLE PRECISION :: XR(MMAX,NMAX), YR(MMAX,NMAX), FI2
      integer :: i,j

      DO I = 1,MMAX
         DO J = 1,NMAX
            IF (XR(I,J) .NE. DXYMIS) THEN
               FI2     = ATAN(SINH(YR(I,J) ) )
               YR(I,J) = RD2DG*FI2
               XR(I,J) = RD2DG*XR(I,J)
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END SUBROUTINE MAKEY

      SUBROUTINE MAKEY1D(XR,YR,MNMAX)  ! terug naar graden SUBROUTINE MAKEY1D
      USE M_SFERIC
      USE M_MISSING
      implicit none
      integer :: mnmax
      DOUBLE PRECISION :: XR(MNMAX), YR(MNMAX), FI2
      integer :: i

      DO I = 1,MNMAX
         IF (XR(I) .NE. DXYMIS) THEN
            FI2   = ATAN(SINH(YR(I) ) )
            YR(I) = RD2DG*FI2
            XR(I) = RD2DG*XR(I)
         ENDIF
      ENDDO
      RETURN
      END SUBROUTINE MAKEY1D


      SUBROUTINE MAKEY2(XR,YR,XO,YO,MMAX,NMAX)  ! Voor tekenen bij JSFERIC SUBROUTINE MAKEY2
      USE M_SFERIC
      USE M_MISSING
      implicit none
      DOUBLE PRECISION :: XR(MMAX,NMAX), YR(MMAX,NMAX), &
                          XO(MMAX,NMAX), YO(MMAX,NMAX), FI2
      integer :: mmax,nmax
      integer :: i,j



      DO I = 1,MMAX
         DO J = 1,NMAX
            IF (XR(I,J) .NE. XYMIS) THEN
               FI2     = ATAN(SINH(YR(I,J) ) )
               YO(I,J) = RD2DG*FI2
               XO(I,J) = RD2DG*XR(I,J)
            ELSE
               XO(I,J) = XYMIS
               YO(I,J) = XYMIS
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END SUBROUTINE MAKEY2

      SUBROUTINE SOR(A,B,C,D,E,U,RJAC,M1,N1,M2,N2)
      use m_grid
      use m_gridsettings
      use m_orthosettings, only: ITIN
      implicit none
      double precision :: anorm
      double precision :: anormf
      double precision :: half
      integer :: j
      integer :: l
      integer :: m1
      integer :: m2
      integer :: maxits
      integer :: n
      integer :: n1
      integer :: n2
      double precision :: one
      double precision :: qtr
      double precision :: rjac
      double precision :: zero
!     IMPLICIT double precision ::(A-H,O-Z)
      DOUBLE PRECISION :: A(MMAX,NMAX),B(MMAX,NMAX),C(MMAX,NMAX), D(MMAX,NMAX),E(MMAX,NMAX),U(MMAX,NMAX)

      PARAMETER(ZERO=0D0,HALF=.5D0,QTR=.25D0,ONE=1D0)
      DOUBLE PRECISION :: RESID, OMEGA
!     WRITE (MDIA,*) 'MEGS AVAILABLE SOR ', N4*4.096*0.001,
!      (N1+N2)*4.096*0.001d0
      MAXITS=ITIN
      ANORMF=ZERO
      OMEGA =ONE

      DO 15 N=1,MAXITS
        ANORM=ZERO
        DO 14 J=MAX(2,M1),MIN(M2,MC-1)
          DO 13 L=MAX(2,N1),MIN(N2,NC-1)
            IF (IJC (J,L) .EQ. 10) THEN
!              IF(MOD(J+L,2).EQ.MOD(N,2))THEN
                 RESID=A(J,L)*U(J+1,L)+B(J,L)*U(J-1,L)+    &
                     C(J,L)*U(J,L+1)+D(J,L)*U(J,L-1)+ E(J,L)*U(J,L)
                 U(J,L)=U(J,L)-OMEGA*RESID/E(J,L)
!              ENDIF
            ENDIF
13        CONTINUE
14      CONTINUE
        IF(N.EQ.1) THEN
          OMEGA=ONE/(ONE-HALF*RJAC**2)
        ELSE
          OMEGA=ONE/(ONE-QTR*RJAC**2*OMEGA)
        ENDIF
!       write(mdia,*) omega, rjac

15    CONTINUE
      RETURN
      END SUBROUTINE SOR

!*******************  BOUNDARY TREATMENT *****************************

      SUBROUTINE BNDSMT(XR,YR,XI2,YI2,XJ2,YJ2,ATP,M1,N1,M2,N2)
      use m_grid
      use m_gridsettings
      implicit none
      double precision :: bfe
      integer :: i
      integer :: iff
      integer :: ifr
      integer :: il
      integer :: ilr
      integer :: in
      integer :: int
      integer :: ir
      integer :: irr
      integer :: j
      integer :: jf
      integer :: jfr
      integer :: jl
      integer :: jlr
      integer :: jr
      integer :: jrr
      integer :: kc
      integer :: m1
      integer :: m2
      integer :: n1
      integer :: n2
      integer :: num
      double precision :: qb
      double precision :: qbc
      double precision :: qc
      double precision :: rn
      double precision :: x0
      double precision :: x1
      double precision :: x2
      double precision :: x3
      double precision :: y0
      double precision :: y1
      double precision :: y2
      double precision :: y3
!     RANDPUNTEN OF INTERNE PUNTEN
!     TERUGZETTEN OP DE SPLINE TUSSEN OUDE POSITIE OP RAND (BFAC = 0)
!     EN PROJECTIE OP SPLINE VAN NABIJ PUNT (BFAC = 1)
!     BIJ NCODE IS 5, INT(ERNAL) HORIZONTAAL, 6 = VERTICAAL
      DOUBLE PRECISION :: XR(MMAX,NMAX), YR(MMAX,NMAX),   &
                          XI2(MMAX,NMAX),XJ2(MMAX,NMAX),ATP(MMAX,NMAX),               &
                          YI2(MMAX,NMAX),YJ2(MMAX,NMAX)

      DOUBLE PRECISION, ALLOCATABLE :: XH(:), YH(:), XH2(:), YH2(:)
      DOUBLE PRECISION              :: XX1, XX2, YY1, YY2, TV, XV, YV, XV2, YV2, DIS

      ALLOCATE ( XH(MNMAX), YH(MNMAX), XH2(MNMAX), YH2(MNMAX) )


      IF (BFAC .EQ. 0) RETURN
      BFE = 1 - BFAC

!     DE HORIZONTALEN
      DO 10 JR = 1,NC
         IN  = 0
         INT = 0
         J   = JR
         DO 10 IR = 1,MC
            KC = ABS(IJC(IR,JR))
            IF (KC .EQ. 11 .OR. KC .EQ. 14) THEN
               IFR = IR + 1
               IFF = IR
            ELSE IF (KC .EQ. 1) THEN
               IN  = 1
            ELSE IF (KC .EQ. 3) THEN
               IN  = -1
            ELSE IF (KC .EQ. 5) THEN
               IN  = 1
               INT = 1
            ELSE IF (KC .EQ. 12 .OR. KC .EQ. 13 .AND. IN .NE. 0) THEN
               ILR = IR - 1
               IL  = IR
               NUM = IL - IFF + 1
               CALL GETIJ(XC,   XH,MMAX,NMAX,MNMAX,IFF,IL, J, J)
               CALL GETIJ(XJ2,XH2,MMAX,NMAX,MNMAX,IFF,IL, J, J)
               CALL GETIJ(YC,   YH,MMAX,NMAX,MNMAX,IFF,IL, J, J)
               CALL GETIJ(YJ2,YH2,MMAX,NMAX,MNMAX,IFF,IL, J, J)

               DO 20 IRR = IFR,ILR
                  IF (IRR .GE. M1 .AND. IRR .LE. M2 .AND.   &
                       JR .GE. N1 .AND.  JR .LE. N2 .AND.   &
                       IJC(IRR,JR) .GT. 0) THEN
                     XX1 = XR(IRR,JR+IN)
                     YY1 = YR(IRR,JR+IN)
                     X0  = XR(IRR,JR)
                     Y0  = YR(IRR,JR)
                     XX1 = XX1*BFAC + X0*BFE
                     YY1 = YY1*BFAC + Y0*BFE
                     TV  = IRR - IFF
                     IF (IN .EQ. 1) THEN
!                       onder
                        X1  = XR(IRR-1,JR)
                        X3  = XR(IRR+1,JR)
                        X2  = XR(IRR,JR+1)
                        Y1  = YR(IRR-1,JR)
                        Y3  = YR(IRR+1,JR)
                        Y2  = YR(IRR,JR+1)
                        QB  = ATP(IRR-1,JR)
                        QC  = ATP(IRR,JR)
                        QBC = 1d0/QB + 1d0/QC
                        RN  = QB + QC + QBC
                        XX1 = (QB*X1 + QBC*X2 + QC*X3 + Y3 - Y1) / RN
                        YY1 = (QB*Y1 + QBC*Y2 + QC*Y3 + X1 - X3) / RN
                     ELSE IF (IN .EQ. -1) THEN
!                       boven
                        X1  = XR(IRR-1,JR)
                        X3  = XR(IRR+1,JR)
                        X2  = XR(IRR,JR-1)
                        Y1  = YR(IRR-1,JR)
                        Y3  = YR(IRR+1,JR)
                        Y2  = YR(IRR,JR-1)
                        QB  = ATP(IRR-1,JR-1)
                        QC  = ATP(IRR,JR-1)
                        QBC = 1d0/QB + 1d0/QC
                        RN  = QB + QC + QBC
                        XX1 = (QB*X1 + QBC*X2 + QC*X3 + Y1 - Y3) / RN
                        YY1 = (QB*Y1 + QBC*Y2 + QC*Y3 + X3 - X1) / RN
                     ENDIF
!                    CALL RCIRC(XX1,YY1)
                     CALL DISMIN(XH,XH2,YH,YH2,XX1,YY1,NUM,DIS,TV,XV,YV)
                     IF (INT .EQ. 1) THEN
!                       for internal boundary points
                        XX2 = XR(IRR,JR-IN)
                        YY2 = YR(IRR,JR-IN)
                        XX2 = XX2*BFAC + X0*BFE
                        YY2 = YY2*BFAC + Y0*BFE
                        CALL DISMIN(XH,XH2,YH,YH2,XX2,YY2,NUM,DIS,TV,XV2,YV2)
                        XV  = (XV + XV2)/2
                        YV  = (YV + YV2)/2
                     ENDIF
                     XR(IRR,JR) = XV
                     YR(IRR,JR) = YV
                  ENDIF
    20         CONTINUE
               IN  = 0
               INT = 0
            ENDIF
    10 CONTINUE

!     CALL WAITESC()

!     DE VERTICALEN
      DO 30 IR = 1,MC
         IN  = 0
         INT = 0
         I   = IR
         DO 30 JR = 1,NC
            KC = ABS(IJC(IR,JR))
            IF (KC .EQ. 11 .OR. KC .EQ. 12) THEN
               JFR = JR + 1
               JF  = JR
            ELSE IF (KC .EQ. 4) THEN
               IN = 1
            ELSE IF (KC .EQ. 2) THEN
               IN = -1
            ELSE IF (KC .EQ. 6) THEN
               IN  = 1
               INT = 1
            ELSE IF (KC .EQ. 14 .OR. KC .EQ. 13 .AND. IN .NE. 0) THEN
               JLR = JR - 1
               JL  = JR
               NUM = JL - JF + 1
               CALL GETIJ(XC,   XH,MMAX,NMAX,MNMAX, I, I,JF,JL)
               CALL GETIJ(XI2,XH2,MMAX,NMAX,MNMAX, I, I,JF,JL)
               CALL GETIJ(YC,   YH,MMAX,NMAX,MNMAX, I, I,JF,JL)
               CALL GETIJ(YI2,YH2,MMAX,NMAX,MNMAX, I, I,JF,JL)

               DO 40 JRR = JFR,JLR
                  IF (JRR .GE. N1 .AND. JRR .LE. N2 .AND.    &
                       IR .GE. M1 .AND.  IR .LE. M2 .AND.    &
                       IJC(IR,JRR) .GT. 0) THEN
                     XX1 = XR(IR+IN,JRR)
                     YY1 = YR(IR+IN,JRR)
                     X0  = XR(IR,JRR)
                     Y0  = YR(IR,JRR)
                     XX1 = XX1*BFAC + X0*BFE
                     YY1 = YY1*BFAC + Y0*BFE
                     TV  = JRR - JF
                     IF (IN .EQ. 1) THEN
!                       links
                        X1  = XR(IR,JRR-1)
                        X3  = XR(IR,JRR+1)
                        X2  = XR(IR+1,JRR)
                        Y1  = YR(IR,JRR-1)
                        Y3  = YR(IR,JRR+1)
                        Y2  = YR(IR+1,JRR)
                        QC  = 1d0/ATP(IR,JRR)
                        QB  = 1d0/ATP(IR,JRR-1)
                        QBC = 1d0/QB + 1d0/QC
                        RN  = QB + QC + QBC
                        XX1 = (QB*X1 + QBC*X2 + QC*X3 + Y1 - Y3) / RN
                        YY1 = (QB*Y1 + QBC*Y2 + QC*Y3 + X3 - X1) / RN
                     ELSE IF (IN .EQ. -1) THEN
!                       rechts
                        X1  = XR(IR,JRR-1)
                        X3  = XR(IR,JRR+1)
                        X2  = XR(IR-1,JRR)
                        Y1  = YR(IR,JRR-1)
                        Y3  = YR(IR,JRR+1)
                        Y2  = YR(IR-1,JRR)
                        QC  = 1d0/ATP(IR-1,JRR)
                        QB  = 1d0/ATP(IR-1,JRR-1)
                        QBC = 1d0/QB + 1d0/QC
                        RN  = QB + QC + QBC
                        XX1 = (QB*X1 + QBC*X2 + QC*X3 + Y3 - Y1) / RN
                        YY1 = (QB*Y1 + QBC*Y2 + QC*Y3 + X1 - X3) / RN
                     ENDIF
!                    CALL RCIRC(XX1,YY1)
                     CALL DISMIN(XH,XH2,YH,YH2,XX1,YY1,NUM,DIS,TV,XV,YV)
                     IF (INT .EQ. 1) THEN
!                       for internal boundary points
                        XX2 = XR(IR-IN,JRR)
                        YY2 = YR(IR-IN,JRR)
                        XX2 = XX2*BFAC + X0*BFE
                        YY2 = YY2*BFAC + Y0*BFE
                        CALL DISMIN(XH,XH2,YH,YH2,XX2,YY2,NUM,DIS,TV,XV2,YV2)
                        XV = (XV + XV2)/2
                        YV = (YV + YV2)/2
                     ENDIF
                     XR(IR,JRR) = XV
                     YR(IR,JRR) = YV
                  ENDIF
    40         CONTINUE
               IN  = 0
               INT = 0
            ENDIF
    30 CONTINUE

      DEALLOCATE ( XH, YH, XH2, YH2 )


      RETURN
      END SUBROUTINE BNDSMT

      SUBROUTINE DISMIN(X,X2,Y,Y2,XX,YY,N,DIS,TV,XV,YV)
      implicit none
      integer :: n
      double precision :: rn
!     ZOEK MEEST NABIJE PUNT OP SPLINE
!     START ZOEKEN ROND TV, ZOEK MET GULDEN SNEDE ROUTINE
!     N IS MAXIMUM INDEX ZOEKGEBIED
      DOUBLE PRECISION :: X(N), X2(N), Y(N), Y2(N), XV, YV, XX, YY, TV
      DOUBLE PRECISION :: AX, BX, CX, TOL, DIS

!     RLEN = SQRT((X(1)-X(2))**2+(Y(1)-Y(2))**2)
      TOL  = 0.000001d0
!     TOL  = 0.000005*RLEN
      RN  = dble(N)
      AX  = 0d0
      BX  = TV
      CX  = RN
      CALL GOLD(AX,BX,CX,TOL,TV,X,X2,Y,Y2,XX,YY,N,DIS)

      CALL SPLINT(X,X2,N,TV,XV)
      CALL SPLINT(Y,Y2,N,TV,YV)

      RETURN
      END SUBROUTINE DISMIN

      SUBROUTINE GOLD(AX,BX,CX,TOL,XMIN,P,P2,Q,Q2,XX,YY,N,DIS)
      implicit none
      double precision :: c
      double precision :: f0
      double precision :: f1
      double precision :: f2
      double precision :: f3
      integer :: n
      double precision :: r
      double precision :: x0
      double precision :: x3
      double precision :: spldist
      PARAMETER (R=.61803399,C=.38196602)

      DOUBLE PRECISION :: AX, BX, CX, TOL, DIS, XMIN, XX, YY, X1, X2

!     EENDIMENSIONAAL ZOEKEN VAN 'GEBRACKED' MINIMUM
      DOUBLE PRECISION :: P(N), P2(N), Q(N), Q2(N)
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
      F1=SPLDIST(P,P2,Q,Q2,XX,YY,X1,N)
!     F2=F(X2)
      F2=SPLDIST(P,P2,Q,Q2,XX,YY,X2,N)
1     IF(ABS(X3-X0).GT.TOL*(ABS(X1)+ABS(X2)))THEN
!     IF(ABS(X3-X0).GT.TOL) THEN
        IF(F2.LT.F1)THEN
          X0=X1
          X1=X2
          X2=R*X1+C*X3
          F0=F1
          F1=F2
          F2=SPLDIST(P,P2,Q,Q2,XX,YY,X2,N)
!         F2=F(X2)
        ELSE
          X3=X2
          X2=X1
          X1=R*X2+C*X0
          F3=F2
          F2=F1
!         F1=F(X1)
          F1=SPLDIST(P,P2,Q,Q2,XX,YY,X1,N)
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
      END SUBROUTINE GOLD

      DOUBLE PRECISION FUNCTION SPLDIST(X,X2,Y,Y2,XX,YY,TV,N)
      implicit none
      integer :: n
!     AFSTAND VAN PUNT XX,YY TOT SPLINEPUNT MET PARM TV

      DOUBLE PRECISION :: X(N), X2(N), Y(N), Y2(N), TV, XX, YY, XV, YV
      TV = MAX(0d0,MIN(TV,N-1d0))
      CALL SPLINT(X,X2,N,TV,XV)
      CALL SPLINT(Y,Y2,N,TV,YV)
!     CALL DISTANCE(XV,YV,XX,YY,DIST)
      CALL PLANEDISTANCE(XV,YV,XX,YY,SPLDIST)
      RETURN
      END FUNCTION SPLDIST



      SUBROUTINE GETSPL2(X,XI2,XJ2,MC,NC,MMAX,NMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: iff
      integer :: il
      integer :: in
      integer :: jalin
      integer :: jn
      integer :: k
      integer :: mc
      integer :: mmax
      integer :: mnmax
      integer :: nc
      integer :: nmax
!     VUL DE ARRAY MET TWEEDE AFGELEIDES IN I EN J RICHTING
!     HAAL TELKENS EEN LIJNTJE, DOE SPLINE EN ZET TERUG

      DOUBLE PRECISION              :: X(MMAX,NMAX), XI2(MMAX,NMAX), XJ2(MMAX,NMAX)
      DOUBLE PRECISION, ALLOCATABLE :: XH1(:), XH21(:), XHH(:)

      MNMAX = MAX(MMAX,NMAX)

      ALLOCATE ( XH1(MNMAX), XH21(MNMAX), XHH(MNMAX) )


      XI2 = DXYMIS
      XJ2 = DXYMIS

      DO 20 JN = 1,NC
         CALL GETIJ(X,XH1,MMAX,NMAX,MNMAX,1,MC,JN,JN)
         JALIN = 0
         K     = 0
         DO 30 I = 1,MC
            IF (XH1(I) .NE. DXYMIS) THEN
               IF (JALIN .EQ. 0) THEN
!                 BEGIN LIJN BIJ I
                  IFF    = I
                  JALIN  = 1
               ENDIF
               K      = K + 1
               XHH(K) = XH1(I)
               IF (JALIN .EQ. 1 .AND. I .EQ. MC) THEN
!                 EINDE LIJN BIJ MC DUS SPLINE VRAGEN
                  CALL SPLINE(XHH,K,XH21)
                  CALL PUTIJ(XJ2,XH21,MMAX,NMAX,MNMAX,IFF,MC,JN,JN)
               ENDIF
            ELSE IF (JALIN .EQ. 1) THEN
!              EINDE LIJN BIJ I - 1 DOORDAT I NUL IS, SPLINE VRAGEN
               JALIN = 0
               IL    = I - 1
               CALL SPLINE(XHH,K,XH21)
               CALL PUTIJ(XJ2,XH21,MMAX,NMAX,MNMAX,IFF,IL,JN,JN)
               K     = 0
            ENDIF
    30   CONTINUE
    20 CONTINUE

      DO 40 IN = 1,MC
         CALL GETIJ(X,XH1,MMAX,NMAX,MNMAX,IN,IN,1,NC)
         JALIN = 0
         K     = 0
         DO 50 I = 1,NC
            IF (XH1(I) .NE. DXYMIS) THEN
               IF (JALIN .EQ. 0) THEN
!                 BEGIN LIJN BIJ I
                  IFF    = I
                  JALIN  = 1
               ENDIF
               K      = K + 1
               XHH(K) = XH1(I)
               IF (JALIN .EQ. 1 .AND. I .EQ. NC) THEN
!                 EINDE LIJN BIJ MC DUS SPLINE VRAGEN
                  CALL SPLINE(XHH,K,XH21)
                  CALL PUTIJ(XI2,XH21,MMAX,NMAX,MNMAX,IN,IN,IFF,NC)
               ENDIF
            ELSE IF (JALIN .EQ. 1) THEN
!              EINDE LIJN BIJ I - 1 DOORDAT I NUL IS, SPLINE VRAGEN
               JALIN = 0
               IL    = I - 1
               CALL SPLINE(XHH,K,XH21)
               CALL PUTIJ(XI2,XH21,MMAX,NMAX,MNMAX,IN,IN,IFF,IL)
               K     = 0
            ENDIF
    50   CONTINUE
    40 CONTINUE

      DEALLOCATE ( XH1, XH21, XHH )

      RETURN
      END SUBROUTINE GETSPL2


      SUBROUTINE ATPPAR(X,Y,M1,N1,M2,N2,          &
                        ATP,A,B,C,D,E,JDLA)
      use m_grid, not1=>xc, not2=>yc
      USE M_GRIDSETTINGS
      use m_orthosettings
      USE M_MISSING
      implicit none
      double precision :: af
      double precision :: cy
      double precision :: dg2rd
      integer :: i
      integer :: j
      integer :: jdla
      integer :: m1
      integer :: m2
      integer :: n1
      integer :: n2
      integer :: ndraw
      integer :: ndraw8
      double precision :: ym
!     STUURPARAMETERS (1,MC-1)
!     4 3             (1,NC-1)
!     1 2       D1: (12+43)/2   D2:(14 + 23)/2
!     En vul ATP in celmiddens

      DOUBLE PRECISION ::    X(MMAX,NMAX),  Y(MMAX,NMAX), ATP(MMAX,NMAX),    &
              A(MMAX,NMAX),  B(MMAX,NMAX),   C(MMAX,NMAX),    &
              D(MMAX,NMAX),  E(MMAX,NMAX), XC(4), YC(4)

      DOUBLE PRECISION :: X1,Y1,X2,Y2,D12, X3,Y3,X4,Y4,D34, D14, D23


      COMMON /DRAWTHIS/ ndraw(50)

      LOGICAL JAWEL
      SAVE NDRAW8

      A = DXYMIS; B = DXYMIS; C = DXYMIS ; D = DXYMIS ; E = DXYMIS ; ATP = DXYMIS

      DG2RD   = (ACOS(-1d0))/180d0
!     A,B = METRISCH EN SOM, ATP,E = STUUR, C,D = SOM ATP EN E
!     A,ATP EN C IN M-RICHTING
      DO 10 I = M1,M2
         DO 10 J = N1,N2
            IF (IJYES(I,J) .EQ. 1) THEN
               X1 = X(I,J)
               X2 = X(I+1,J)
               X3 = X(I+1,J+1)
               X4 = X(I,J+1)
               Y1 = Y(I,J)
               Y2 = Y(I+1,J)
               Y3 = Y(I+1,J+1)
               Y4 = Y(I,J+1)
               YM = (Y1 + Y2 + Y3 + Y4)/4
               CY = COS(YM*DG2RD)
               CALL PLANEDISTANCE(X1,Y1,X2,Y2,D12)
               CALL PLANEDISTANCE(X3,Y3,X4,Y4,D34)
               CALL PLANEDISTANCE(X1,Y1,X4,Y4,D14)
               CALL PLANEDISTANCE(X2,Y2,X3,Y3,D23)
               A(I,J)   = (D12 + D34) / 2
               B(I,J)   = (D14 + D23) / 2
!              B(I,J)   = (C12 + C34) / 2
!              A(I,J)   = (C14 + C23) / 2
               ATP(I,J) = A(I,J)
               E(I,J)   = B(I,J)
            ENDIF
    10 CONTINUE

      IF (MDESIGN .GE. 2) THEN
!        andere stuurparameters, in celmiddens
         NDRAW8   = NDRAW(8)
         NDRAW(8) = 0
        ! CALL INTPATP(ATP,E,C,D,X,Y,JDLA,M1,N1,M2,N2)
         NDRAW(8) = NDRAW8
      ENDIF

      DO 30 I = M1,M2
         DO 30 J = N1,N2
            C(I,J) = ATP(I,J)
            D(I,J) =   E(I,J)
    30 CONTINUE

!     sommmen
      CALL SOMDIST(X,Y,A,B,C,D,M1,N1,M2,N2)

!     normeren

      AF = 1 - ATPF
      DO 60 I = M1,M2
         DO 60 J = N1,N2
            IF (IJYES(I,J) .EQ. 1) THEN
               ATP(I,J) = ATP(I,J)*A(I,J)/C(I,J)
               ATP(I,J) = ATPF*ATP(I,J) + AF*A(I,J)
               E(I,J)   = E(I,J)*B(I,J)/D(I,J)
               E(I,J)   = ATPF*E(I,J) + AF*B(I,J)

               A(I,J)   = ATP(I,J)
               B(I,J)   = E(I,J)
            ENDIF
    60 CONTINUE

      DO 90 I = M1,M2
         DO 90 J = N1,N2
            IF (IJYES(I,J) .EQ. 1) THEN
               ATP(I,J) = B(I,J) / A(I,J)
            ELSE
               ATP(I,J) = dmiss
            ENDIF
   90 CONTINUE
!     CALL TEKSHOW(X, Y, M2, N2, ATP, 2,'FINAL ATP')

      A = 0D0 ; B = 0D0 ; C = 0D0 ; D = 0D0 ; E = 0D0
      DO 100 I = M1+1,M2
         DO 100 J = N1+1,N2
            IF (IJC(I,J) .EQ. 10) THEN

!              A(I,J) = ( ATP(I,J-1) + ATP(I,J) )*0.5
!              B(I,J) = ( ATP(I-1,J-1) + ATP(I-1,J) )*0.5
!              C(I,J) = 1.0 / ( ( ATP(I-1,J) + ATP(I,J) )*0.5 )
!              D(I,J) = 1.0 / ( ( ATP(I-1,J-1) + ATP(I,J-1) )*0.5 )

!              A(I,J) = ( ATP(I,J-1) + ATP(I,J) )
!              B(I,J) = ( ATP(I-1,J-1) + ATP(I-1,J) )
!              C(I,J) = 4.0 / ( ATP(I-1,J)   + ATP(I,J)   )
!              D(I,J) = 4.0 / ( ATP(I-1,J-1) + ATP(I,J-1) )

               A(I,J) = ( ATP(I,J-1) + ATP(I,J) )
               B(I,J) = ( ATP(I-1,J-1) + ATP(I-1,J) )
               C(I,J) = ( 1d0/ATP(I-1,J)   + 1d0/ATP(I,J) )
               D(I,J) = ( 1d0/ATP(I-1,J-1) + 1d0/ATP(I,J-1) )

               E(I,J) = -( A(I,J) + B(I,J) + C(I,J) + D(I,J) )
            ENDIF
   100 CONTINUE

      RETURN
      END SUBROUTINE ATPPAR

      SUBROUTINE PLANEDISTANCE(X1,Y1,X2,Y2,D12)
      implicit none
      DOUBLE PRECISION :: X1,Y1,X2,Y2,D12, DX,DY
!     D12 IS DISTANCE BETWEEN 1 AND 2 IN PLANE COORDINATES
      DX  = X1 - X2
      DY  = Y1 - Y2
      D12 = SQRT(DX*DX + DY*DY)
      RETURN
      END SUBROUTINE PLANEDISTANCE

      SUBROUTINE FIXDDBOUNDARIES()
      use m_grid
      implicit none
      integer :: i
      integer :: m
      integer :: m1
      integer :: m2
      integer :: mb
      integer :: mb2
      integer :: md
      integer :: n
      integer :: n1
      integer :: n2
      integer :: nb
      integer :: nb2
      integer :: nd
      integer :: npt
      integer :: npt2
      integer :: nputo
      COMMON /DOMBND/ MB(80),NB(80),MB2(80),NB2(80), NPT,NPT2,NPUTO
      DO 10 I = 1,NPT-1,2
         M1 = MB(I)
         N1 = NB(I)
         M2 = MB(I+1)
         N2 = NB(I+1)
         IF (M1 .GT. M2 .OR. N1 .GT. N2) THEN
            MB(I)   = M2
            NB(I)   = N2
            MB(I+1) = M1
            NB(I+1) = N1
         ENDIF
         M1 = MB(I)
         N1 = NB(I)
         M2 = MB(I+1)
         N2 = NB(I+1)
         MD = M2-M1
         ND = N2-N1
         IF (MD .GT. 0) THEN
            N = N1
            DO 20 M = M1,M2
               IJC(M,N) = -IJC(M,N)
    20      CONTINUE
         ELSE IF (ND .GT. 0) THEN
            M = M1
            DO 30 N = N1,N2
               IJC(M,N) = -IJC(M,N)
    30      CONTINUE
         ENDIF
    10 CONTINUE
      RETURN
      END SUBROUTINE FIXDDBOUNDARIES

      SUBROUTINE GETIJ(X,XH,MMAX,NMAX,MNMAX,I1,I2,J1,J2)
      implicit none
      integer :: i
      integer :: i1
      integer :: i2
      integer :: j
      integer :: j1
      integer :: j2
      integer :: k
      integer :: mmax
      integer :: mnmax
      integer :: nmax
!     HAAL EEN LIJN (XH) UIT EEN ARRAY (X)
      DOUBLE PRECISION :: X(MMAX,NMAX),XH(MNMAX)
      K   = 0
      DO 10 J  = J1,J2
         DO 10 I  = I1,I2
            K     = K + 1
            XH(K) = X(I,J)
    10 CONTINUE
      RETURN
      END SUBROUTINE GETIJ

      SUBROUTINE PUTIJ(X,XH,MMAX,NMAX,MNMAX,I1,I2,J1,J2)
      implicit none
      integer :: i
      integer :: i1
      integer :: i2
      integer :: j
      integer :: j1
      integer :: j2
      integer :: k
      integer :: mmax
      integer :: mnmax
      integer :: nmax
!     EN ZET HEM WEER TERUG

      DOUBLE PRECISION :: X(MMAX,NMAX),XH(MNMAX)
      K = 0
      DO 10 J  = J1,J2
         DO 10 I  = I1,I2
            K      = K + 1
            X(I,J) = XH(K)
    10 CONTINUE
      RETURN
      END SUBROUTINE PUTIJ


      SUBROUTINE SOMDIST(X,Y,A,B,C,D,M1,N1,M2,N2)
      use m_grid
      use m_missing
      implicit none
      integer :: i
      integer :: i2
      integer :: ii
      integer :: j
      integer :: j2
      integer :: jj
      integer :: k
      integer :: l
      integer :: m1
      integer :: m2
      integer :: n1
      integer :: n2
      DOUBLE PRECISION ::   X(MMAX,NMAX),  Y(MMAX,NMAX),        &
                            A(MMAX,NMAX),  B(MMAX,NMAX),        &
                            C(MMAX,NMAX),  D(MMAX,NMAX)
!
      DO 10 I = M1+1,M2
         DO 10 J = N1+1,N2
            IF (IJC(I,J) .EQ. 11) THEN
               II = -1
            ELSE IF (IJC(I,J) .EQ. 12) THEN
               II =  1
            ELSE IF (IJC(I,J) .EQ. 13) THEN
               II =  1
            ELSE IF (IJC(I,J) .EQ. 14) THEN
               II = -1
            ENDIF
            IF (IJC(I,J) .GE. 11 .AND. IJC(I,J) .LE. 14) THEN
               K = I
    20         CONTINUE
               K  = K + II
               I2 = K
               IF (IJC(K,J) .EQ. 10) GOTO 20
               DO 30 K = I,I2,II
                  IJC(K,J) = 21
    30         CONTINUE
            ENDIF
    10 CONTINUE

!     DO 40 I = 1,MC
!        DO 40 J = 1,NC
!           IF (IJC(I,J) .NE. 0) THEN
!              R = IJC(I,J)
!              CALL HTEXT(R, X(I,J), Y(I,J))
!           ENDIF
!   40 CONTINUE
!     CALL WAITESC()
      CALL INULARR(IJYES,MMAX,NMAX)
      DO 50 I = M1,M2
         DO 50 J = N1+1,N2
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.          &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               IF (B(I,J) .NE. dmiss .AND. B(I,J-1) .NE. dmiss .AND.   &
                   IJC(I,J) .NE. 21) THEN
                  B(I,J) = B(I,J) + B(I,J-1)
                  D(I,J) = D(I,J) + D(I,J-1)
                  IJYES(I,J) = IJYES(I,J-1) + 1
               ENDIF
            ENDIF
    50 CONTINUE

      DO 55 I = M1,M2
         DO 55 J = N2-1,N1,-1
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.         &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               IF (B(I,J) .NE. dmiss .AND. B(I,J+1) .NE. dmiss .AND.  &
                   IJC(I,J+1) .NE. 21) THEN
                  B(I,J) = B(I,J+1)
                  D(I,J) = D(I,J+1)
                  IJYES(I,J) = IJYES(I,J+1)
               ENDIF
            ENDIF
    55 CONTINUE

      DO 56 I = M1,M2
         DO 56 J = N1,N2
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.         &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               B(I,J) = B(I,J) / (IJYES(I,J) + 1)
               D(I,J) = D(I,J) / (IJYES(I,J) + 1)
            ENDIF
    56 CONTINUE

!     DO 60 I = 1,MC-1
!        DO 60 J = 1,NC-1
!           IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.
!               IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
!              R = B(I,J)
!              R = IJYES(I,J) + 1
!              XX = ( X(I,J) + X(I,J+1) + X(I+1,J+1) + X(I+1,J) ) / 4
!              YY = ( Y(I,J) + Y(I,J+1) + Y(I+1,J+1) + Y(I+1,J) ) / 4
!              CALL HTEXT(R, XX, YY)
!           ENDIF
!   60 CONTINUE
!     CALL WAITESC()

      CALL ISITU ( )
      CALL INULARR(IJYES,MMAX,NMAX)

      DO 110 I = M1+1,M2
         DO 110 J = N1+1,N2
            IF (IJC(I,J) .EQ. 11) THEN
               JJ = -1
            ELSE IF (IJC(I,J) .EQ. 12) THEN
               JJ = -1
            ELSE IF (IJC(I,J) .EQ. 13) THEN
               JJ =  1
            ELSE IF (IJC(I,J) .EQ. 14) THEN
               JJ =  1
            ENDIF
            IF (IJC(I,J) .GE. 11 .AND. IJC(I,J) .LE. 14) THEN
               L = J
   120         CONTINUE
               L  = L + JJ
               J2 = L
               IF (IJC(I,L) .EQ. 10) GOTO 120
               DO 130 L = J,J2,JJ
                  IJC(I,L) = 22
   130         CONTINUE
            ENDIF
   110 CONTINUE

      DO 150 J = N1,N2
         DO 150 I = M1+1,M2
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.        &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               IF (A(I,J) .NE. dmiss .AND. A(I-1,J) .NE. dmiss .AND. &
                   IJC(I,J) .NE. 22) THEN
                  A(I,J) = A(I,J) + A(I-1,J)
                  C(I,J) = C(I,J) + C(I-1,J)
                  IJYES(I,J) = IJYES(I-1,J) + 1
               ENDIF
            ENDIF
   150 CONTINUE

      DO 155 J = N1,N2
         DO 155 I = M2-1,M1,-1
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.         &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               IF (A(I,J) .NE. dmiss .AND. A(I+1,J) .NE. dmiss .AND.  &
                   IJC(I+1,J) .NE. 22) THEN
                  A(I,J) = A(I+1,J)
                  C(I,J) = C(I+1,J)
                  IJYES(I,J) = IJYES(I+1,J)
               ENDIF
            ENDIF
   155 CONTINUE

      DO 156 J = N1,N2
         DO 156 I = M1,M2
            IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.        &
                IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
               A(I,J) = A(I,J) / (IJYES(I,J) + 1)
               C(I,J) = C(I,J) / (IJYES(I,J) + 1)
            ENDIF
   156 CONTINUE

!     CALL SETCOL(66)
!     DO 160 I = 1,MC-1
!        DO 160 J = 1,NC-1
!           IF (IJC(I,J) .NE. 0 .AND. IJC(I+1,J) .NE. 0 .AND.
!               IJC(I,J+1) .NE. 0 .AND. IJC(I+1,J+1) .NE. 0 ) THEN
!              R = A(I,J)
!              R = IJYES(I,J) + 1
!              XX = ( X(I,J) + X(I,J+1) + X(I+1,J+1) + X(I+1,J) ) / 4
!              YY = ( Y(I,J) + Y(I,J+1) + Y(I+1,J+1) + Y(I+1,J) ) / 4
!              CALL HTEXT(R, XX, YY)
!           ENDIF
!  160 CONTINUE
!     CALL WAITESC()

!     Herstellen
      CALL ISITU (     )

      RETURN
      END SUBROUTINE SOMDIST


      !> Write a curvilinear grid to (ascii) grd-file.
      !! NOTE: 'new' format (RGFGrid 4.x)
      !!
      !! Format:
      !! Start with at least one comment line, prefixed by '*', with optional keyword Spherical (old RGFGRID 3.x style)
      !! Next, zero or more key=value lines (all optional):
      !!   Coordinate System=Spherical
      !!   Missing Value=...
      !! First subsequent line without a '=' should be '0 0 0' (backwards compatibility)
      !! Next line should be mmax, nmax
      !! That ends the header, start reading coordinates in the usual fashion.
      SUBROUTINE WRIRGF(MRGF,FILNAM)
      use m_sferic
      use m_grid
      use m_missing
      use m_arcinfo
      use m_polygon

      implicit none
      double precision :: half
      integer :: ipnt, n, i,j, nfirst
      integer :: mrgf, mdep



      CHARACTER NAME2*76, FILNAM*(*)

      IPNT  = INDEX(FILNAM,'.')
      NAME2 = FILNAM
      WRITE(NAME2(IPNT+1:),'(A)') 'enc'

      CALL FIRSTLIN(MRGF)
      IF (JSFERIC .EQ. 1) THEN
         WRITE(MRGF,'(A)') 'Coordinate System=Spherical'
      ENDIF
      WRITE(MRGF,'(A,F14.3)') 'Missing Value=',XYMIS

      WRITE(MRGF,'(2I8)') MC,NC
      WRITE(MRGF,'(3I8)') 0, 0, 0 ! Backwards compatibility
     ! CALL CORRGF(Xc,Yc,MMAX,NMAX)
     ! CALL ISITU()
     ! CALL WRIENC(NAME2, Xc, MC, NC, IJC, IJYES,mmax,nmax)
      HALF = 0
      CALL READYY('Writing Grid File',HALF)
      CALL ECRTAB(Xc,MC,NC,MRGF,HALF,mmax,nmax)
      HALF = 0.5d0
      CALL ECRTAB(Yc,MC,NC,MRGF,HALF,mmax,nmax)

      CALL READYY(' ',-1d0)
      CALL DOCLOSE (MRGF)

      WRITE(NAME2(IPNT+1:),'(A)') 'asc'
      call newfil(MDEP,NAME2)
      call wriarc(MDEP,ZC,MMAX,NMAX,MC,NC,X0,Y0,DXA,DYA,DMISS)
      call doclose(MDEP)
      
      WRITE(NAME2(IPNT+1:),'(A)') 'dep'
      call newfil(MDEP,NAME2)
      call  WRIDEP(MDEP,Zc,1,1,mmax,nmax,mmax,nmax) 
      call doclose(MDEP)

      if (mc*nc < -1000) then ! save grd to polygon for partitioning
         call savepol()
         n = 0 ; nfirst = 0
         do i = 1, mc-1
            do j = 1,nc-1
               if (xc(i,j)   .ne. dmiss .and. xc(i+1,j)   .ne. dmiss .and. & 
                   xc(i,j+1) .ne. dmiss .and. xc(i+1,j+1) .ne. dmiss ) then 
                   if (nfirst .ne. 0) then 
                      n = n + 1 ; xpl(n) = xc(i,j  )   ; ypl(n) = yc(i,j)
                      n = n + 1 ; xpl(n) = xc(i+1,j)   ; ypl(n) = yc(i+1,j)
                      n = n + 1 ; xpl(n) = xc(i+1,j+1) ; ypl(n) = yc(i+1,j+1)
                      n = n + 1 ; xpl(n) = xc(i,j+1)   ; ypl(n) = yc(i,j+1)
                      n = n + 1 ; xpl(n) = dmiss       ; ypl(n) = dmiss
                   else 
                      nfirst = 1
                   endif   
                endif   
            enddo
         enddo
         npl = n
         WRITE(NAME2(IPNT:),'(A)') '_part.pol'
         call newfil(MDEP,NAME2)
         call wripol(mdep) 
         call restorepol()
      endif   
      
      END SUBROUTINE WRIRGF
      
      SUBROUTINE WRIDEP(MMDD,ZC,M1,N1,MC,NC,mmax,nmax)
      implicit none
      integer          :: MMDD,M1,N1,MC,NC,mmax,nmax,n,m
      double precision :: ZC(mmax,nmax)
      double precision :: AF

      CALL READYY('Writing Depth File ',0d0)
      DO 10 N = N1, NC
         AF = dble(N) / dble(NC)
         CALL READYY('Writing Dept File',AF)
         WRITE(MMDD,'(12(1PE13.5))') (ZC(M,N),M = M1,MC)
   10 CONTINUE
      CALL READYY('writing Dept File',-1d0)
      CALL DOCLOSE (MMDD)
      RETURN
      END


      SUBROUTINE ECRTAB(X,MC,NC,MRGF,HALF,mmax,nmax)
      implicit none
      double precision :: af
      double precision :: half
      integer :: i
      integer :: i4
      integer :: j
      integer :: mc
      integer :: mmax
      integer :: mrgf
      integer :: nc
      integer :: nmax
!     SCHRIJFROUTINE RGF-FORMAT
      double precision :: X(MMAX,NMAX)
      DO 1 J=1,NC
         AF = HALF + 0.5d0*dble(J)/dble(NC)
         CALL READYY(' ',AF)
         WRITE(MRGF,888) J,(X(I,J),I=1,MC)
  1   CONTINUE
  888 FORMAT(' ETA= ',I4,5ES26.18:/(10X,5ES26.18))
      RETURN
      END SUBROUTINE ECRTAB

      SUBROUTINE MAPPROJECTIONS(IT,JA)
      USE M_MAPPROPARAMETERS
      USE M_SFERIC
      USE M_MISSING
      use m_netw
      USE M_GRID
      USE M_LANDBOUNDARY
      USE M_POLYGON
      USE M_XYTEXTS
      USE M_SAMPLES
      USE M_SPLINES
      implicit none
      integer :: i
      integer :: ini
      integer :: it
      integer :: j
      integer :: ja
      integer :: k
      DOUBLE PRECISION :: XG, YG

      INI    = 1
      DELTX  = 0d0
      DELTY  = 0d0
      FI     = 0d0
      XF     = 1d0
      YF     = 1d0
      ! IZONE  =  UTMZONE DIE JE WIL, NZONE = ADVIESZONE
      ! ITYPE  = 1  ! 0 = ROTATIE/TRANSLATIE, 1 = UTM, 2=RD, 3 = PARIJS, 5 = AFFINE

   10 IF (IT .EQ. -1) THEN
       CALL CONVERPARAMETERS(JA)
      ELSE
       JA    = 1
      ENDIF
      IF (JA .EQ. 1) THEN
         IF (ITYPE .EQ. 0 .AND. JSFERIC .EQ. 1) THEN
            CALL QNERROR('Spherical Coordinates Should Not Be', 'Scaled, Translated or Rotated',' ')
            RETURN
         ENDIF
         IF (ITYPE .EQ. 1 .AND. JSFERIC .EQ. 0 .AND. IZONE .EQ. 0) THEN
            CALL QNERROR('Please Specify a Valid ZONE Nr', 'in the range 1-60',' '          )
            GOTO 10
         ENDIF


         DO K = 1,NUMK
            IF (XK(K) .NE. DXYMIS) THEN
               CALL MAPPRO (XK(K),YK(K),XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INI)
               IF (XG .NE. DXYMIS) THEN
                  XK(K) = XG
                  YK(K) = YG
               ELSE
                  XK(K) = DXYMIS
                  YK(K) = DXYMIS
               ENDIF
             ENDIF
         ENDDO


         DO I = 1,MC
            DO J = 1,NC
               IF (XC(I,J) .NE. DXYMIS) THEN
                  CALL MAPPRO (XC(I,J),YC(I,J),XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INI)
                  IF (XG .NE. DXYMIS) THEN
                     XC(I,J) = XG
                     YC(I,J) = YG
                  ELSE
                     XC(I,J) = DXYMIS
                     YC(I,J) = DXYMIS
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         DO K = 1,MXLAN
            IF (XLAN(K) .NE. DXYMIS) THEN
               CALL MAPPRO (XLAN(K),YLAN(K),XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INI)
               IF (XG .NE. DXYMIS) THEN
                  XLAN(K) = XG
                  YLAN(K) = YG
               ELSE
                  XLAN(K) = DXYMIS
                  YLAN(K) = DXYMIS
               ENDIF
            ENDIF
         ENDDO

         DO K = 1,NPL
            IF (XPL(K) .NE. XYMIS) THEN
               CALL MAPPRO (XPL(K),YPL(K),XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INI)
               IF (XG .NE. DXYMIS) THEN
                  XPL(K) = XG
                  YPL(K) = YG
               ELSE
                  XPL(K) = XYMIS
                  YPL(K) = XYMIS
               ENDIF
            ENDIF
         ENDDO

         DO K = 1,NTXT
            IF (XTXT(K) .NE. XYMIS) THEN
               CALL MAPPRO (XTXT(K),YTXT(K),XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INI)
               IF (XG .NE. DXYMIS) THEN
                  XTXT(K) = XG
                  YTXT(K) = YG
               ELSE
                  XTXT(K) = XYMIS
                  YTXT(K) = XYMIS
               ENDIF
            ENDIF
         ENDDO

         DO K = 1,NS
            IF (XS(K) .NE. XYMIS) THEN
               CALL MAPPRO (XS(K),YS(K),XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INI)
               IF (XG .NE. DXYMIS) THEN
                  XS(K) = XG
                  YS(K) = YG
               ELSE
                  XS(K) = XYMIS
                  YS(K) = XYMIS
               ENDIF
            ENDIF
         ENDDO

         DO J = 1,maxsplen
             DO I = 1,MCs
                IF (XSP(I,J) .NE. DXYMIS) THEN
                   CALL MAPPRO (XSp(I,J),YSp(I,J),XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INI)
                   IF (XG .NE. DXYMIS) THEN
                       XSP(I,J) = XG
                       YSP(I,J) = YG
                    ELSE
                       XSP(I,J) = XYMIS
                       YSP(I,J) = XYMIS
                    ENDIF
                endif
             ENDDO
         ENDDO    
    

!        IF (ITYPE .EQ. 1) THEN
!           IF (IZONE .EQ. 0) IZONE = NZONE   ! if initialised as zero
!        ENDIF

         IF (ITYPE .GE. 1) THEN
            JSFERIC  = 1 - JSFERIC
            JSFERTEK = JSFERIC
         ENDIF
      ENDIF

      RETURN
      END SUBROUTINE MAPPROJECTIONS

      SUBROUTINE MAPPRO(XX,YY,XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INIA)
      USE M_MISSING
      !use proj4
      implicit none
      integer :: ierr
      integer :: ini
      integer :: inia
      integer :: itype
      integer :: izone
      integer :: jsferic
      integer :: nzone
      integer :: ihem
!      type(pjf90_proj), save :: proj_latlon, proj_magsirwest
!      double precision, save,pointer :: xp(:), yp(:) ! AvD: temp
      double precision :: A,E
      DOUBLE PRECISION :: XX,YY,XG,YG

      SAVE A,E
      DATA  INI /0/
      IF (INI .EQ. 0) THEN
         CALL SETELLIPS(3) ! WGS84
         INI = 1
!         ierr = pjf90_init_plus(proj_latlon, &
!            '+proj=latlong +datum=WGS84')
!         ierr = pjf90_init_plus(proj_magsirwest, &
!            '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-77.0775079166666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ')
!         ierr = pjf90_init_plus(proj_utm31,
!     +   '+proj=utm +zone=31 +datum=WGS84')
!         ierr = pjf90_init_plus(proj_rdnew,
!     +  '+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889'
!     +//' +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m'
!     +//' +no_defs')
!         allocate(xp(1),yp(1))
      ENDIF

      XG = DXYMIS
      YG = DXYMIS
      IF (JSFERIC .EQ. 0) THEN        ! Cartesisch => Spherisch
         IF (ITYPE .EQ. 0) THEN  ! except for itype = 0
            CALL TRAROT(XX,YY,XG,YG)
         ELSE IF (ITYPE .EQ. 1) THEN  ! UTM
!            xp(1) = xx
!            yp(1) = yy
!            ierr = pjf90_transform(proj_magsirwest,proj_latlon, 1, 1, xp, yp, null())
!            xg = xp(1)*RAD_TO_DEG
!            yg = yp(1)*RAD_TO_DEG
            !CALL UTMGEO(XX,YY,XG,YG,IZONE,IERR) ! IZONE = input !TMP disable
            CALL UTMGEO2(XX,YY,XG,YG,IZONE,IHEM,IERR) ! IZONE = input !TMP disable
         ELSE IF (ITYPE .EQ. 2) THEN  ! Amersfoorts
            CALL RDGEO(XX,YY,XG,YG,0)
         ELSE IF (ITYPE .EQ. 3) THEN  ! RD (Ofwel Parijs)
            CALL RDGEO(XX,YY,XG,YG,1)
         ELSE IF (ITYPE .EQ. 4) THEN  ! MERCATOR
            CALL MERCGEO(XX,YY,XG,YG)
         ELSE IF (ITYPE .EQ. -1) THEN  ! AFFINE
            CALL AFFINE(XX,YY,XG,YG,INIA)
         ENDIF
      ELSE IF (JSFERIC .EQ. 1) THEN   ! Spherisch  => Cartesisch
         IF (ITYPE .EQ. 1) THEN       ! UTM
            CALL GEOUTM (XX,YY,XG,YG,IZONE,NZONE,IERR) ! IZONE = output
         ELSE IF (ITYPE .EQ. 2) THEN  ! Amersfoorts
            CALL GEORD(XX,YY,XG,YG,0)
         ELSE IF (ITYPE .EQ. 3) THEN  ! RD (Ofwel Parijs)
            CALL GEORD(XX,YY,XG,YG,1)
         ELSE IF (ITYPE .EQ. 4) THEN  ! MERCATOR
            CALL GEOMERC(XX,YY,XG,YG)
         ELSE IF (ITYPE .EQ. -1) THEN  ! AFFINE
            CALL AFFINE(XX,YY,XG,YG,INIA)
         ENDIF
      ENDIF
      RETURN
      END SUBROUTINE MAPPRO



      subroutine GEOUTM (xgeo,ygeo,xutm,yutm,Izone,nzone,IERR)
      implicit none
      integer :: nzone
! ----------------------------------------------------------------------
!
!     conversion of geographical (lat, lon) --> UTM coordinates (x, y, zone)
!     geographical coordinates (lat, lon) expressed in decimal degrees.
!
! ----------------------------------------------------------------------
!     arguments:
!     xgeo    i    double precision ::    longitude (geographical coordinate)
!     ygeo    i    double precision ::    lattitude (geographical coordinate)
!     a       i    double precision ::    semi-major axis of ellipsoid
!     e       i    double precision ::    excentricity of ellipsoid
!     xutm    o    double precision ::    easting (UTM)
!     yutm    o    double precision ::    northing (UTM)
!     zone    o    integer   zone (UTM)
!     ierr    o    integer   error code (zero for no error)
!
      double precision :: xgeo,ygeo,a,e,xutm,yutm
      integer      Izone,ierr
!
!     local variables:
!     pi           double precision ::    3.14....
!     fn           double precision ::    false northing
!     fe           double precision ::    false easting
!     fi           double precision ::    geographic lattitude (equivalent to lat)
!     dl           double precision ::    longitude within zone
!     dl2          double precision ::    dl*dl
!     s            double precision ::    sin(fi)
!     ss           double precision ::    s*s
!     sc           double precision ::    sin(fi)*cos(fi)
!     c            double precision ::    cos(fi)
!     cc           double precision ::    c*c
!     cccc         double precision ::    c*c*c*c
!     f1           double precision ::    coefficient in function dm(fi)
!     f2           double precision ::    coefficient in function dm(fi)
!     f3           double precision ::    coefficient in function dm(fi)
!     f4           double precision ::    coefficient in function dm(fi)
!     e2           double precision ::    e*e
!     e4           double precision ::    e2*e2
!     e6           double precision ::    e2*e4
!     n            double precision ::    e*e/(1-e*e)
!     nn           double precision ::    n*n
!     x            double precision ::    UTM easting (similar to xutm)
!     y            double precision ::    UTM northing (similar to yutm)
!     rp           double precision ::    function rp(fi)
!     dm           double precision ::    function dm(fi)
!     gx           double precision ::    function gx(fi,dl)
!     gy           double precision ::    function gy(fi,dl)
!
      double precision :: pi,fn,fe
      double precision :: fi,dl,dl2,s,ss,sc,c,cc,cccc,f1,f2,f3,f4,e2,e4,e6
      double precision :: n,nn,x,y,rp,dm,gx,gy
      COMMON /ELLIPS/ A,E
!
! -----------------------------------------------------------------------------
!     t.j.zitman                                  last update: 10 december 1990
! -----------------------------------------------------------------------------
!
!     initialize constants
!
      pi       = 4d0*ATAN(1d0)
!
      e2     = e**2
      e4     = e2**2
      e6     = e2*e4
      n      = e2/(1d0-e2)
      nn     = n**2
      f1     = 1d0 - (1d0/4d0)*e2 - (3d0/64d0)*e4 -  ( 5d0/256d0)*e6
      f2     =       (3d0/8d0)*e2 + (3d0/32d0)*e4 + (45d0/1024d0)*e6
      f3     =                    (15d0/256d0)*e4 + (45d0/1024d0)*e6
      f4     =                                      (35d0/3072d0)*e6
!
!     set false northing and false easting
!
   !  IF (ygeo.LT.0.0) then
   !    ygeo = -ygeo
   !    fn   = 1.0E+07
   !    fn   = Y_offset
   !  else
   !    fn   = 0.0d0
   !  endif

      fn = 0D0
      fe = 5d+05
!
!     determine zone
!
      Nzone = INT( (xgeo+180)/6 ) + 1
      if (IZONE .EQ. 0) then
         IZONE = NZONE
      endif
!
!     set fi and dl
!
      fi = ygeo*pi/180d0
      dl = (xgeo + 177d0 - 6d0*FLOAT(Izone-1))*pi/180d0
!
!     constants, related to fi
!
      s      = SIN(fi)
      ss     = s**2
      c      = COS(fi)
      cc     = c**2
      cccc   = cc**2
      sc     = s*c
!
!     values of sub-functions
!
      rp     = a/SQRT(1d0-e2*ss)
      dm     = a*( f1*fi - f2*SIN(2d0*fi)  + f3*SIN(4d0*fi) - f4*SIN(6d0*fi) )
      dl2    = dl**2
      gx     = dl2*(2d0*cc - 1d0 + nn*cccc)/6d0
      gy     = dl2*(6d0*cc - 1d0 + 9d0*nn*cccc)/12d0
!
!     function values x and y
!
      x      = rp*dl*c*(1d0+gx)
      y      = dm + rp*0.5d0*dl2*sc*(1d0+gy)
!
!     set UTM x- and y-coordinates
!
      xutm   = 0.9996d0*x + fe
      yutm   = 0.9996d0*y + fn
!
!     set no error
!
      ierr   = 0
!
      continue
      return
      end subroutine geoutm

      subroutine UTMGeo(xutm,yutm,xgeo,ygeo,IZONE,ierr)
      implicit none
!
! -----------------------------------------------------------------------------
!
!     conversion of UTM coordinates (x, y, zone) into geographical
!     coordinates (lat, lon), expressed in decimal degrees.
!
! -----------------------------------------------------------------------------
!
!     arguments:
!     xutm    i    double precision ::    easting (UTM)
!     yutm    i    double precision ::    northing (UTM)
!     Izone    i    integer   Izone (UTM)
!     a       i    double precision ::    semi-major axis of ellipsoid
!     e       i    double precision ::    excentricity of ellipsoid
!     xgeo    o    double precision ::    longitude (geographical coordinate)
!     ygeo    o    double precision ::    lattitude (geographical coordinate)
!     ierr    o    integer   error code (zero for no error)
!
      double precision :: xutm,yutm,a,e,ygeo,xgeo
      integer      Izone,ierr
!
!     local variables:
!     pi           double precision ::    3.14....
!     eps          double precision ::    stopping criterion (limit in change)
!     fn           double precision ::    false northing
!     fe           double precision ::    false easting
!     cxutm        double precision ::    xutm, corrected for false eastin
!     cyutm        double precision ::    yutm, corrected for false northing
!     fi           double precision ::    geographic lattitude (equivalent to lat)
!     dl           double precision ::    longitude within zone
!     dl2          double precision ::    dl*dl
!     s            double precision ::    sin(fi)
!     ss           double precision ::    s*s
!     sc           double precision ::    sin(fi)*cos(fi)
!     c            double precision ::    cos(fi)
!     cc           double precision ::    c*c
!     cccc         double precision ::    c*c*c*c
!     f1           double precision ::    coefficient in function dm(fi)
!     f2           double precision ::    coefficient in function dm(fi)
!     f3           double precision ::    coefficient in function dm(fi)
!     f4           double precision ::    coefficient in function dm(fi)
!     e2           double precision ::    e*e
!     e4           double precision ::    e2*e2
!     e6           double precision ::    e2*e4
!     r            double precision ::    1-e2*ss
!     n            double precision ::    e*e/(1-e*e)
!     nn           double precision ::    n*n
!     x            double precision ::    UTM easting (similar to xutm)
!     dxdfi        double precision ::    partial derivative of x wrt. fi
!     dxddl        double precision ::    partial derivative of x wrt. dl
!     y            double precision ::    UTM northing (similar to yutm)
!     dydfi        double precision ::    partial derivative of y wrt. fi
!     dyddl        double precision ::    partial derivative of y wrt. dl
!     rp           double precision ::    function rp(fi)
!     drpdfi       double precision ::    derivative of rp wrt. fi
!     dm           double precision ::    function dm(fi)
!     ddmdfi       double precision ::    derivative of dm wrt. fi
!     gx           double precision ::    function gx
!     dgxdfi       double precision ::    partial derivative of gx wrt. fi
!     dgxddl       double precision ::    partial derivative of gx wrt. dl
!     gy           double precision ::    function gy
!     dgydfi       double precision ::    partial derivative of gy wrt. fi
!     dgyddl       double precision ::    partial derivative of gy wrt. dl
!     det          double precision ::    determinant
!     chanfi       double precision ::    change in fi (NR-iteration)
!     chandl       double precision ::    change in dl (NR-iteration)
!
      double precision :: pi,eps,fn,fe,cxutm,cyutm
      double precision :: fi,dl,dl2,s,ss,sc,c,cc,cccc,f1,f2,f3,f4,e2,e4,e6,r
      double precision :: n,nn,x,dxdfi,dxddl,y,dydfi,dyddl,rp,drpdfi,dm,ddmdfi
      double precision :: gx,dgxdfi,gy,dgydfi,det,chanfi,chandl
      COMMON /ELLIPS/ A,E
!
!c -----------------------------------------------------------------------------
!     t.j.zitman                                   last update: 5 december 1990
!c -----------------------------------------------------------------------------
!
!     initialize constants
!
      pi     = acos(-1.d0)  ! 4.0d0*daTAN(1.0d0)
      eps    = 1.0d-05
      fe     = 5.0d+05
!     fn     = 1.0E+07
      fn     = 0.D0
!
      e2     = e**2
      e4     = e2**2
      e6     = e2*e4
      n      = e2/(1d0-e2)
      nn     = n**2
      f1     = 1d0 - (1d0/4d0)*e2 - (3d0/64d0)*e4 -  ( 5d0/256d0)*e6
      f2     =       (3d0/8d0)*e2 + (3d0/32d0)*e4 + (45d0/1024d0)*e6
      f3     =                    (15d0/256d0)*e4 + (45d0/1024d0)*e6
      f4     =                                      (35d0/3072d0)*e6
!
!     correct input for false easting and false northing
!
      cxutm = (xutm - fe)/0.9996d0
      if (yutm .ge. fn) then
        cyutm = (yutm - fn)/0.9996d0
      else
        cyutm = yutm/0.9996d0
      endif
!
!     first estimates of dl and fi
!
      dl     = xutm/a
      fi     = yutm/a

!     dl     = 0.0d0
!     fi     = pi/6.0d0

!
!     Newton Raphson iteration
!
100   continue
!
!     constants, related to fi
!
      s      = SIN(fi)
      ss     = s**2
      c      = COS(fi)
      cc     = c**2
      cccc   = cc**2
      sc     = s*c
!
!     values of sub-functions and derivatives
!
      r      = 1d0-e2*ss
      rp     = a/SQRT(r)
      drpdfi = a*e2*sc/(r**1.5d0)
      dm     = a*( f1*fi - f2*SIN(2d0*fi)  + f3*SIN(4d0*fi) - f4*SIN(6d0*fi) )
      ddmdfi = a*( f1 - 2d0*f2*COS(2d0*fi) + 4d0*f3*COS(4d0*fi) - 6d0*f4*COS(6d0*fi) )
      dl2    = dl**2
      gx     = dl2*(2d0*cc - 1d0 + nn*cccc)/6d0
      dgxdfi = -2d0*dl2*sc*(1d0+nn*cc)/3d0
      gy     = dl2*(6d0*cc - 1d0 + 9d0*nn*cccc)/12d0
      dgydfi = -dl2*sc*(1d0-3d0*nn*cc)
!
!     function values x, y and derivatives
!
      x      = rp*dl*c*(1d0+gx) - cxutm
      dxdfi  = dl*( (drpdfi*c-rp*s)*(1d0+gx) + rp*c*dgxdfi )
      dxddl  = rp*c*(1d0+3d0*gx)
      y      = dm + rp*0.5d0*dl2*sc*(1d0+gy) - cyutm
      dydfi  = ddmdfi + 0.5d0*dl2*( sc*(drpdfi*(1d0+gy) + rp*dgydfi) + rp*(cc-ss)*(1d0+gy) )
      dyddl  = rp*dl*sc*(1d0+2d0*gy)
!
!     changes in the estimates dl and fi
!
      det    = dxddl*dydfi - dxdfi*dyddl
      if (det.eq.0d0) then
        ierr = 1
        goto 900
      endif
      chanfi = -(-x*dyddl + y*dxddl)/det
      chandl = -( x*dydfi - y*dxdfi)/det
!
!     check stopping criterion
!
      if ( ABS(chanfi).GT.ABS(eps*fi)  .and. ABS(chandl).GT.ABS(eps*dl)  )then
         fi   = fi + chanfi
         dl   = dl + chandl
         goto 100
      endif
!
!     set final values
!
      ygeo   = fi*180d0/pi
      xgeo   = dl*180d0/pi + 6d0*FLOAT(Izone-1) - 177d0
      ierr   = 0
!
900   continue
      return
      end subroutine UTMGeo

!> converts UTM coords to lat/long.  Equations from USGS Bulletin 1532 
!! East Longitudes are positive, West longitudes are negative. 
!! North latitudes are positive, South latitudes are negative
!! Lat and Long are in decimal degrees. 
!! Written by Chuck Gantz- chuck.gantz@globalstar.com
!! BY: Chuck Gantz, http://www.gpsy.com/gpsinfo/geotoutm/gantz/LatLong-UTMconversion.cpp
    subroutine utmgeo2(xutm,yutm,xgeo,ygeo,IZONE,ihem, ierr)
    use m_sferic
    implicit none
!     xutm    i    double precision ::    easting (UTM)
!     yutm    i    double precision ::    northing (UTM)
!     Izone   i    integer   Izone (UTM)
!     Ihem    i    integer hemisphere (0=north, 1 = south)
!     a       i    double precision ::    semi-major axis of ellipsoid
!     e       i    double precision ::    excentricity of ellipsoid
!     xgeo    o    double precision ::    longitude (geographical coordinate)
!     ygeo    o    double precision ::    lattitude (geographical coordinate)
!     ierr    o    integer   error code (zero for no error)
!
    double precision :: xutm,yutm,a,e,ygeo,xgeo
    integer      Izone,ihem,ierr
    COMMON /ELLIPS/ A,E

    double precision :: k0 = 0.9996
    double precision :: eccSquared
	double precision :: eccPrimeSquared;
	double precision :: e1
	double precision :: N1, T1, C1, R1, D, M
	double precision :: LongOrigin
	double precision :: mu, phi1, phi1Rad
	double precision :: x, y
	integer ::  ZoneNumber
	integer :: NorthernHemisphere !1 for northern hemispher, 0 for southern

    eccSquared = e*e
    e1 = (1-sqrt(1-eccSquared))/(1+sqrt(1-eccSquared))
    ZoneNumber = izone
	NorthernHemisphere = ihem
	x = xutm - 500000.0  !remove 500,000 meter offset for longitude
	y = yutm

    if (ihem == 0) then
        y = y - 10000000.0 !remove 10,000,000 meter offset used for southern hemisphere
	end if

	LongOrigin = (ZoneNumber - 1)*6 - 180 + 3 !  //+3 puts origin in middle of zone

	eccPrimeSquared = (eccSquared)/(1-eccSquared)

	M = y / k0
	mu = M/(a*(1-eccSquared/4-3*eccSquared*eccSquared/64-5*eccSquared*eccSquared*eccSquared/256))

	phi1Rad = mu	+ (3*e1/2-27*e1*e1*e1/32)*sin(2*mu) &
				+ (21*e1*e1/16-55*e1*e1*e1*e1/32)*sin(4*mu) &
				+(151*e1*e1*e1/96)*sin(6*mu)
	phi1 = phi1Rad*rd2dg

	N1 = a/sqrt(1-eccSquared*sin(phi1Rad)*sin(phi1Rad))
	T1 = tan(phi1Rad)*tan(phi1Rad)
	C1 = eccPrimeSquared*cos(phi1Rad)*cos(phi1Rad)
	R1 = a*(1-eccSquared)/(1-eccSquared*sin(phi1Rad)*sin(phi1Rad))**1.5d0
	D = x/(N1*k0)

	ygeo = phi1Rad - (N1*tan(phi1Rad)/R1)*(D*D/2-(5+3*T1+10*C1-4*C1*C1-9*eccPrimeSquared)*D*D*D*D/24 &
					+(61+90*T1+298*C1+45*T1*T1-252*eccPrimeSquared-3*C1*C1)*D*D*D*D*D*D/720)
	ygeo = ygeo * rd2dg

	xgeo = (D-(1+2*T1+C1)*D*D*D/6+(5-2*C1+28*T1-3*C1*C1+8*eccPrimeSquared+24*T1*T1) &
					*D*D*D*D*D/120)/cos(phi1Rad)
	xgeo = LongOrigin + xgeo * rd2dg

    end subroutine utmgeo2

    subroutine RDGEO(xrd,yrd,xgeo,ygeo,JAPARIJS)
      use m_sferic
      implicit none
      integer :: japarijs
!
! -----------------------------------------------------------------------------
!
!     Conversion of RD-coordinates into Geographical coordinates (Bessel)
!
! -----------------------------------------------------------------------------
!     t.j.zitman                                   last update: 29 january 1991
! -----------------------------------------------------------------------------
!
!     arguments:
!     xrd    [ I ]   east-coordinate in RD system
!     yrd    [ I ]   north-coordinate in RD system
!     xgeo   [ O ]   geographical east-coordinate (degrees; decimal)
!     ygeo   [ O ]   geographical north-coordinate (degrees; decimal)
!
      double precision :: xrd,yrd
      double precision :: xgeo,ygeo
!
!     local variables:
!     urd    : linearly transformed xrd
!     vrd    : linearly transformed yrd
!     ugeo   : linearly transformed xgeo
!     vgeo   : linearly transformed ygeo
!
      double precision :: urd,vrd
      double precision :: ugeo,vgeo
      
      double precision :: a01, a20, a02, a21, a03, a22, a40, a23, a41, a04, a42, a24
      double precision :: b10, b11, b12, b30, b31, b13, b32, b14, b50, b33, b51, b15      
      double precision :: dx, dx2, dx3, dx4, dx5, xd, x0
      double precision :: dy, dy2, dy3, dy4, dy5, yd, y0
      
      double precision :: a, e, ya, xa, b0, dl0, gn, gm, rr, dk, r, sa, ca, psi, spsi
      double precision :: cb, sb, b, sdl, dl, rl, w, q, psia, dq, phi 
      
      integer          :: k, jazitman = 1
      
      x0  = 155000d0
      y0  = 463000d0
      
      ya  = 52.156160556d0
      xa  = 5.387638889d0
      
      IF (JAPARIJS .EQ. 1) THEN
         XRD = XRD - x0
         YRD = YRD - y0
      ENDIF
      urd = 0.00001d0*xrd
      vrd = 0.00001d0*yrd
    
      if (jazitman == 1) then 
         vgeo = 187762.178d0 + 3236.033d0*vrd - 32.592d0*(urd**2) -               &
                0.247d0*(vrd**2) - 0.850d0*vrd*(urd**2) - 0.065d0*(vrd**3) +      &
                0.005d0*(urd**4) - 0.017d0*(urd**2)*(vrd**2)
         ugeo = 19395.500d0 + 5261.305d0*urd + 105.979d0*urd*vrd +                &
                2.458d0*urd*(vrd**2) - 0.819d0*(urd**3) +                         &
                0.056d0*urd*(vrd**3) - 0.056d0*vrd*(urd**3)
      !xgeo = ugeo/3600d0
      !ygeo = vgeo/3600d0
         call bessel2wgs84(vgeo/3600d0,ugeo/3600d0,ygeo,xgeo)
      else if (jazitman == 2) then 
         a01 = 3236.0331637d0   
         a20 =  -32.5915821d0  
         a02 =   -0.2472814d0
         a21 =   -0.8501341d0
         a03 =   -0.0655238d0
         a22 =   -0.0171137d0
         a40 =    0.0052771d0
         a23 =   -0.0003859d0
         a41 =    0.0003314d0
         a04 =    0.0000371d0
         a42 =    0.0000143d0
         a24 =   -0.0000090d0

         b10 = 5261.3028966d0
         b11 =  105.9780241d0        
         b12 =    2.4576469d0
         b30 =   -0.8192156d0
         b31 =   -0.0560092d0
         b13 =    0.0560089d0
         b32 =   -0.0025614d0
         b14 =    0.0012770d0
         b50 =    0.0002574d0
         b33 =   -0.0000973d0
         b51 =    0.0000293d0
         b15 =    0.0000291d0
          
         dx  = urd ; dx2 = dx*dx ; dx3 = dx*dx2 ;  dx4 = dx*dx3; dx5 = dx*dx4
         dy  = vrd ; dy2 = dy*dy ; dy3 = dy*dy2 ;  dy4 = dy*dy3; dy5 = dy*dy4
         
         yd  = a01*dy      + a20*dx2     + a02*dy2     + a21*dx2*dy + a03*dy3 +  &
               a40*dx4     + a22*dx2*dy2 + a04*dy4     + a41*dx4*dy           +  &         
               a23*dx2*dy3 + a42*dx4*dy2 + a24*dx2*dy4
         
         xd  = b10*dx      + b11*dx*dy   + b30*dx3     + b12*dx*dy2           +  &
               b31*dx3*dy  + b13*dx*dy3  + b50*dx5     + b32*dx3*dy2          +  & 
               b14*dx*dy4  + b51*dx5*dy  + b33*dx3*dy3 + b15*dx*dy5
         
         xgeo = xa + xd/3600d0      
         ygeo = ya + yd/3600d0     
      
      else  ! SPvdP: may not be accurate
          
         a   = 6377397.155d0
         e   = 0.081696831222d0
         b0  = 52.121097249d0
         dl0 = xa         
         gn  = 1.00047585668d0
         gm  = 0.003773953832d0
         rr  = 6382644.571d0
         dk  = 0.999079d0
         
         r    = sqrt(xrd*xrd + yrd*yrd) 
         sa   = xrd / r
         ca   = yrd / r
         psi  = 2d0*atan2(r, 2d0*dk*rr)
         spsi = sin(psi) 
         cb   = cos(dg2rd*b0)
         sb   = ca*cb*spsi + sin(dg2rd*b0)*cos(psi)
         b    = asin(sb)
         
         sdl  = sa*spsi / cb
         dl   = asin(sdl)
         rl   = rd2dg*dl/gn + xa
         w    = atanh(sb) 
         do k = 1,4
            q    = (w - gm) / gn
            psia = 2d0*atan(exp(q)) - 0.5d0*pi
            dq   = e*atanh(e*sin(psia))
            phi  = asin(tanh(q+dq))  
         enddo 
         
         ygeo = phi*rd2dg
         xgeo = rl  
         
      endif    
         
!

      continue
      return
      end subroutine RDGEO

      subroutine GEORD(xgeo,ygeo,xrd,yrd,JAPARIJS)
      implicit none
      integer :: japarijs

! -----------------------------------------------------------------------------
!     Conversion of Geographical coordinates (Bessel) into RD-coordinates
! -----------------------------------------------------------------------------
!     t.j.zitman                                   last update: 29 january 1991
! -----------------------------------------------------------------------------
!
!     arguments:
!     xgeo   [ I ]   geographical east-coordinate (degrees; decimal)
!     ygeo   [ I ]   geographical north-coordinate (degrees; decimal)
!     xrd    [ O ]   east-coordinate in RD system
!     yrd    [ O ]   north-coordinate in RD system
!
      double precision :: xgeo,ygeo
      double precision :: xrd,yrd
      double precision :: xx,yy
!
!     local variables:
!     ugeo   : linearly transformed xgeo
!     vgeo   : linearly transformed ygeo
!
      double precision :: ugeo,vgeo
!
!     externals:
!     none
!
! -----------------------------------------------------------------------------
!
!     compute linear tramsformation of Geographical coordinates

      call wgs842bessel(ygeo,xgeo,yy,xx)
!!
      ugeo = 0.3600d0*xx -  1.9395500d0
      vgeo = 0.3600d0*yy - 18.7762178d0

!
!     perform conversion
!
      xrd  = 190066.91d0*ugeo - 11831d0*ugeo*vgeo -         &
             114.2d0*ugeo*(vgeo**2) - 32.39d0*(ugeo**3) -     &
             2.33d0*ugeo*(vgeo**3) - 0.61d0*vgeo*(ugeo**3)
      yrd  = 309020.34d0*vgeo + 3638.36d0*(ugeo**2) +         &
             72.92d0*(vgeo**2) - 157.97d0*vgeo*(ugeo**2) +    &
             59.77d0*(vgeo**3) +0.09d0*(ugeo**4) -            &
             6.45d0*(vgeo**2)*(ugeo**2) + 0.07d0*(vgeo**4)
!

      IF (JAPARIJS .EQ. 1) THEN
         XRD = XRD + 155000.
         YRD = YRD + 463000.
      ENDIF

      return
      end subroutine GEORD
!
      SUBROUTINE SETELLIPS(IELL)
      implicit none
      integer :: iell
      COMMON /ELLIPS/ A,E
      double precision :: A,E

      A = 6378137d0
      E = 0.081819d0

      IF (IELL .EQ. 1) THEN      ! Hayford
         A = 6378388d0
         E = 0.081992d0
      ELSEIF (IELL .EQ. 2) THEN  ! Bessel
         A = 6377397d0
         E = 0.081690d0
      ELSEIF (IELL .EQ. 3) THEN  ! WGS 84
         A = 6378137d0
         E = 0.081819d0
      ELSEIF (IELL .EQ. 4) THEN  ! Clarke 1880
         A = 6378249d0
         E = 0.082478d0
      ELSEIF (IELL .EQ. 5) THEN  ! India 1830
         A = 6377276.345d0
         E = 0.081473d0
      ENDIF
      RETURN
      END SUBROUTINE SETELLIPS

      SUBROUTINE MERCGEO(XX,YY,XG,YG)
      USE M_SFERIC
      implicit none
      double precision :: XX,YY,XG,YG,FI2
      XG  = RD2DG*XX/RA
      FI2 = ATAN(SINH(YY/RA))
      YG  = RD2DG*FI2
      RETURN
      END SUBROUTINE MERCGEO

      SUBROUTINE GEOMERC(XG,YG,XX,YY)
      USE M_SFERIC
      implicit none
      double precision :: XX,YY,XG,YG,FI2,YC,CY,F,E
      double precision :: a
      double precision :: sf
      XX  = XG*DG2RD*RA

      FI2 = DG2RD*YG
      YY  = ( 1D0 + SIN(FI2) ) / COS(FI2)
      YY  = RA*LOG(YY)

      A    = 6378140
      XX   = XG*DG2RD*RA
      YC   = DG2RD*(90-YG)
      CY   = COS(YC)
      F    = 298.257223d0
      E    = SQRT(2/F - 1/F**2)

      YY   = -A*log( ABS(tan(YC/2)) *((1+e*CY) / (1-e*CY))**(e/2) )
      SF   = sin(YC) / SQRT( 1 - E*E*CY )

      RETURN
      END SUBROUTINE GEOMERC


      SUBROUTINE TRAROT(XX,YY,XG,YG)
      USE M_MAPPROPARAMETERS
      implicit none
      
      double precision :: XX,YY,XG,YG
      XX = (XX - XCE)*XF
      YY = (YY - YCE)*YF
      XG = DELTX + XX*CSE - YY*SNE + XCE
      YG = DELTY + XX*SNE + YY*CSE + YCE
      RETURN
      END SUBROUTINE TRAROT

      SUBROUTINE AFFINE(XX,YY,XG,YG,INI)
      USE M_BITMAP
      use string_module, only: find_first_letter
      implicit none
      integer :: ini
      logical :: jawel
      integer :: k
      integer :: minp
      integer :: numbersonline
      double precision :: xg4
      double precision :: xx4
      double precision :: yg4
      double precision :: yy4
      CHARACTER REC*132
	  DOUBLE PRECISION :: XX,YY,XG,YG
      XX4 = XX ; YY4 = YY

      IF (INI .EQ. 1) THEN

         INQUIRE(FILE = 'AFFINE'//'.xyx', EXIST = JAWEL)

         IF (JAWEL) THEN
            CALL OLDFIL(MINP, 'AFFINE'//'.xyx')
            READ(MINP,'(A)') REC
            IF (find_first_letter(REC) .EQ. 1) THEN
               READ(MINP,'(A)') REC
               DO K = 1,4
                  READ(MINP,'(A)') REC
                  IF (NUMBERSONLINE(REC) .EQ. 2) THEN
                     READ(REC,*) XP(K),YP(K)
                  ELSE IF (NUMBERSONLINE(REC) .EQ. 4) THEN
                     READ(REC,*) XP(K),YP(K),XB(K),YB(K)

                  ENDIF
               ENDDO
            ELSE
               CALL QNERROR('Cannot Read AFFINE.XYX File',' ',' ')
            ENDIF
            CALL DOCLOSE(MINP)
            CALL BILINXY(XP, YP, XB, YB, XX4, YY4, XG4, YG4, INI)
            INI = 0
         ELSE
            CALL QNERROR ('NO AFFINE.XYX FILE FOUND', ' ',' ')
         ENDIF
      ENDIF

      CALL BILINXY(XP, YP, XB, YB, XX4, YY4, XG4, YG4, INI)
      XG = XG4
	  YG = YG4

      RETURN
	  END
!----------------- splrgfr stuff below


!      SUBROUTINE SPLRGFR (XSP, YSP, MCS, NCS,  X,  Y,  &
!                          MC, NC, MERR, MFAC, NFAC,    &
!                          IJC,IJYES,TIJ,XH,YH,         &
!                          X1,Y1,X2,Y2,X3,Y3,X4,Y4)
    SUBROUTINE SPLRGFR ()
!      USE DIMENS
    USE M_SPLINES
    use m_missing
    use m_grid
    use M_GRIDSETTINGS
    use m_netw, only: zkUNI
    use m_alloc
    implicit none

    double precision, allocatable, dimension(:,:) :: xspc, yspc, xh, yh, tij
    double precision, allocatable, dimension(:)   :: x1, x2, x3, x4, y1, y2, y3, y4
    integer,          allocatable, dimension(:,:) :: mn12
    double precision, allocatable, dimension(:)   :: xi1, yi1, ti1
    integer,          allocatable, dimension(:)   :: ntyp
    double precision, allocatable, dimension(:)   :: tt
! x, y -> xc, yc (m_grid)
! ijc, ijyes in m_grid
    CHARACTER TEX*6
    integer :: ierr, mspl
    integer :: imax, i1, ni1, nti1, l1max, jj, ii1, ii2, k, ii, i, j, L, ki, LJ, no, &
               numspl, numpx, numi, &
               ms, ns, &
               mcr, ncr, &
               mfa, nfa

    IF (MFAC > 1000) THEN 
        CALL QNERROR('Please reduce MFAC and NFAC to about < 50 or so', ' ', ' ')
        return
    endif    
         
    call increasegrid(mfac*mcs, nfac*mcs)

    imax = max( max(mfac,nfac)*mcs, maxsplen )  ! mnmax

    allocate ( xspc(mcs, maxsplen), yspc(mcs, maxsplen), TIJ(mcs, maxsplen), stat=ierr)
    call aerr('xspc(mcs, maxsplen), yspc(mcs, maxsplen), TIJ(mcs,maxsplen)', ierr, 3*mcs*maxsplen)
    allocate ( xh(mmax, nmax), yh(mmax, nmax), stat = ierr)
    call aerr('xh(mmax, nmax), yh(mmax, nmax)', ierr, 2*mmax*nmax)
    allocate(XI1(IMAX),      YI1(IMAX), TI1(IMAX),       &
              X1(IMAX),      Y1(IMAX),                  &
              X2(IMAX),      Y2(IMAX),                  &
              X3(IMAX),      Y3(IMAX),                  &
              X4(IMAX),      Y4(IMAX), TT(IMAX),        &
              stat=ierr)
    call aerr('XI1(imax),YI1(imax),TI1(imax), X/Y1..4(imax), TT(imax)', ierr, 11*imax)

    allocate ( NTYP(IMAX), MN12(IMAX,3), stat=ierr)
    call aerr('NTYP(IMAX), MN12(IMAX,3)', ierr, 2*imax)
    mn12 = 0
    ntyp = 0

    xspc = xymis
    yspc = xymis
    xspc(1:mcs,1:maxsplen) = xsp(1:mcs,1:maxsplen)
    yspc(1:mcs,1:maxsplen) = ysp(1:mcs,1:maxsplen)
    numspl = mcs
    !maxsplen = 2*maxsplen


    CALL READYY('TRANSFORMING SPLINES INTO DESIGN-GRID',0d0)
    ierr = 0

    CALL NEWFIL(mspl,'asave.spl')
    CALL writeSplines(mspl)

    CALL READYY(' ',0.05d0) 

    CALL SECTR(    XSPc,    YSPc,    TIJ, mcs, maxsplen, imax, &
                   ierr,   NUMI,                               &
                 NUMSPL,  NUMPX,   NTYP, MN12,                 &
                  X1,Y1,X2,Y2)


      IF (ierr .LT. 1) THEN
         MS = 0
         NS = 0
 
        !  grof rooster aanmaken uit TIJ of nieuwe interpolatie
         CALL savegrd()
         Xc = dxymis
         Yc = dxymis
         zc = zkuni
!        vul voor alle splines de lijninterpolaties in
         DO 10 I1 = 1, NUMSPL
!           Alle horizontaaltjes
            CALL GETIJ(XSPc, XI1, mcs, maxsplen, imax,I1, I1, 1, NUMPX)
            CALL GETIJ(YSPc, YI1, mcs, maxsplen, imax,I1, I1, 1, NUMPX)
            CALL NUMPold (XSPc,  mcs, maxsplen, I1,NI1)
            CALL PAKTIJ(TIJ,mcs, maxsplen, TI1,imax,I1,I1,1,NUMSPL,NTI1)
            IF (I1 .LE. NUMI) THEN
               CALL MAKESPL(TI1,XI1,YI1,imax, NI1,NTI1,MFAC,X1,Y1,L1MAX,TT,0d0)
               JJ  = (MN12(I1,1) - 1)*NFAC + 1
               II1 = (MN12(I1,2) - 1)*MFAC + 1
               II2 = (MN12(I1,3) - 1)*MFAC + 1
            ELSE
               CALL MAKESPL(TI1,XI1,YI1,imax, NI1,NTI1,NFAC,X1,Y1,L1MAX,TT,0d0)
               JJ  = (MN12(I1,1) - 1)*MFAC + 1
               II1 = (MN12(I1,2) - 1)*NFAC + 1
               II2 = (MN12(I1,3) - 1)*NFAC + 1
            ENDIF
            K   = 0
            DO 20 II = II1,II2
               K        = K + 1
               IF (K .LE. L1MAX) THEN
                  IF (I1 .LE. NUMI) THEN
                     Xc(II,JJ) = X1(K)
                     Yc(II,JJ) = Y1(K)
                  ELSE
                     Xc(JJ,II) = X1(K)
                     Yc(JJ,II) = Y1(K)
                  ENDIF
               ENDIF
!              CALL RCIRC( X1(K),Y1(K) )
    20      CONTINUE
!           CALL TOEMAAR()
            IF (I1 .LE. NUMI) THEN
               NS = MAX( NS,MN12(I1,1) )
            ELSE
               MS = MAX( MS,MN12(I1,1) )
            ENDIF
    10   CONTINUE

         NCR = (NS - 1)*NFAC + 1
         MCR = (MS - 1)*MFAC + 1
         IF (MCR .GE. MMAX-1) THEN
             CALL READYY(' ',-1d0)
             CALL QNERROR('TOO MANY GRIDPOINTS IN', 'M-DIRECTION',' ')
             CALL RESTOREgrd()
             RETURN
         ENDIF
         IF (NCR .GE. NMAX-1) THEN
             CALL READYY(' ',-1d0)
             CALL QNERROR('TOO MANY GRIDPOINTS IN', 'N-DIRECTION',' ')
             CALL RESTOREgrd()
             RETURN
         ENDIF

!        CALL CLS1()
!        CALL TEKGRIDPUNTEN(X,Y,MCR,NCR)
!        CALL TOEMAAR()

         DO 30 I = 1, MS-1
            DO 30 J = 1, NS-1
               X1(2) = XYMIS
               X2(2) = XYMIS
               X3(2) = XYMIS
               X4(2) = XYMIS

               DO 40 K = 1,MFAC+1
                  DO 40 L = 1,NFAC+1
                     KI  = (I-1)*MFAC + K
                     LJ  = (J-1)*NFAC + L
                     IF (Xc(KI,LJ) .NE. XYMIS) THEN
                        IF (K .EQ. 1) THEN
                           X1(L) = Xc(KI,LJ)
                           Y1(L) = Yc(KI,LJ)
                        ENDIF
                        IF (K .EQ. MFAC+1) THEN
                           X2(L) = Xc(KI,LJ)
                           Y2(L) = Yc(KI,LJ)
                        ENDIF
                        IF (L .EQ. 1) THEN
                           X3(K) = Xc(KI,LJ)
                           Y3(K) = Yc(KI,LJ)
                        ENDIF
                        IF (L .EQ. NFAC+1) THEN
                           X4(K) = Xc(KI,LJ)
                           Y4(K) = Yc(KI,LJ)
                        ENDIF
                     ENDIF
    40         CONTINUE
               NO = 0
               IF (X1(2) .EQ. XYMIS) NO = 1
               IF (X2(2) .EQ. XYMIS) NO = 1
               IF (X3(2) .EQ. XYMIS) NO = 1
               IF (X4(2) .EQ. XYMIS) NO = 1
               IF (NO .EQ. 0) THEN
                  CALL TRANFN2(X1, X2, X3, X4,      &
                               Y1, Y2, Y3, Y4,      &
                               imax, mmax, nmax, XH, YH)
                  DO 60 K = 1,MFAC+1
                     DO 60 L = 1,NFAC+1
                        KI   = (I-1)*MFAC + K
                        LJ   = (J-1)*NFAC + L
                        IF (Xc(KI,LJ) .EQ. XYMIS) THEN
                           Xc(KI,LJ) = XH(K,L)
                           Yc(KI,LJ) = YH(K,L)
                        ENDIF
    60            CONTINUE
               ENDIF
    30   CONTINUE
         MC = MCR
         NC = NCR
!         CALL ISITU (Xc, Yc, MC, NC, IJC, IJYES)
      ENDIF

      CALL READYY(' ',-1d0)


    deallocate(xspc, yspc)
    call aerr('xspc, yspc', 0, -2*mmax*nmax) ! AvD: TODO
    deallocate(xh, yh, TIJ)
    call aerr('xh, yh, TIJ', 0, -3*mmax*nmax)
    deallocate(XI1, YI1, TI1,       &
               X1, Y1, X2, Y2, X3, Y3, X4, Y4, TT)
    call aerr('XI1, YI1, TI1, X/Y1..4, TT', 0, -11*imax)

    deallocate(NTYP, MN12)
    call aerr('NTYP, MN12', 0, -2*imax)

      RETURN
      END subroutine splrgfr

     SUBROUTINE SECTR (      X,      Y,    TIJ,mmax, nmax, imax,                     &
                          merr,  NUMI,                     &
                         NUMSPL,  NUMPX,   NTYP, MN12, XI, YI, XJ, YJ)
     use unstruc_colors
     use unstruc_messages
     use unstruc_display

     implicit none
     integer :: mmax, nmax, imax
     double precision, dimension(mmax,nmax), intent(inout) :: X, Y
     double precision, dimension(mmax,nmax), intent(out) :: TIJ
     integer, intent(out) :: merr, numi, numspl, numpx
     integer, dimension(imax) :: NTYP
     integer, dimension(imax,3), intent(out) :: MN12
     double precision, dimension(imax), intent(out) :: XI, YI, XJ, YJ

!      INTEGER :: NTYP(IMAX), MN12(IMAX,3)
      CHARACTER TEX1*4, TEX2*4
      double precision :: crp, ti, tj, xspc, yspc
      integer :: mcs, ncs, i, j, numpi, j2, ionbenoemd, numpj, numcro, &
                 L, jachange, icount, JK, maxm, maxn, jjlast, jj, iilast, ii
      integer :: jadubbel
      JADUBBEL = 0

      CALL CHECKSPL(X, Y, mmax, nmax, MCS, NCS)

     5 CONTINUE
      CALL NUMS  (      X, mmax, nmax, NUMSPL,  NUMPX)
      IF (NUMSPL .LT. 4) THEN
         CALL QNERROR('You Need 4 Splines or More to Create a Grid', ' ' ,' ')
         merr = 1
         RETURN
      ELSE IF (NUMSPL .GT. IMAX) THEN
         WRITE(TEX1,'(I4)') NUMSPL
         WRITE(TEX2,'(I4)') IMAX
         CALL QNERROR('Number of Splines Larger than IMAX',TEX1,TEX2)
         CALL QNERROR('REDUCE THE NUMBER OF SPLINES',' ',' ')
         MERR = 1
         RETURN
      ENDIF

      CALL NULARR(TIJ,MMAX,NMAX)
      CALL INULAR(NTYP,IMAX)
      NTYP(1) = 1

      IF (JADUBBEL .GE. 1) THEN
!        VERDUBBEL AANTAL STEUNPUNTEN ALS
         DO 15 I = 1,NUMSPL
            CALL NUMPold(X,mmax, nmax, I,NUMPI)
            CALL GETIJ( X,     XI,mmax, nmax, imax,      I,    I,      1,  NUMPI)
            CALL GETIJ( Y,     YI,mmax, nmax, imax,      I,    I,      1,  NUMPI)
            CALL SPLINE(XI,NUMPI,XJ)
            CALL SPLINE(YI,NUMPI,YJ)
            DO 16 J = 2*NUMPI-1,2,-2
               J2     = 1 + J/2
               X(I,J) = X(I,J2)
               Y(I,J) = Y(I,J2)
    16      CONTINUE
            DO 17 J = 1,NUMPI-1
               TI = J - 0.5
               J2 = 2*J
               CALL SPLINT(XI,XJ,NUMPI,TI,X(I,J2))
               CALL SPLINT(YI,YJ,NUMPI,TI,Y(I,J2))
    17      CONTINUE
    15   CONTINUE
         CALL NUMS(      X, mmax, nmax, NUMSPL,  NUMPX)
      ENDIF

      IONBENOEMD = 0
     6 CONTINUE
      DO 10 I = 1,NUMSPL
         CALL READYY(' ', 0.01d0 + 0.3d0*dble(I-1)/dble(NUMSPL) )
         DO 10 J = I+1,NUMSPL
            CALL NUMPold  (      X,     mmax, nmax, I,  NUMPI)
            CALL NUMPold  (      X,     mmax, nmax, J,  NUMPJ)
            CALL GETIJ (      X,     XI,mmax, nmax, imax,     I,    I,      1, NUMPI)
            CALL GETIJ (      Y,     YI,mmax, nmax, imax,     I,    I,      1, NUMPI)
            CALL GETIJ (      X,     XJ,mmax, nmax, imax,     J,    J,      1, NUMPJ)
            CALL GETIJ (      Y,     YJ,mmax, nmax, imax,     J,    J,      1, NUMPJ)
            CALL SECT3r(     XI,     YI,     XJ,  YJ,    mmax, nmax, imax,CRP,   &
                          NUMPI,  NUMPJ, NUMCRO,  TI,      TJ, XSPc,     YSPc)
            IF (NUMCRO .EQ. 1) THEN
               IF (NTYP(I)*NTYP(J) .EQ. 1) THEN
!                 al gelijk benoemd
                  IF (NUMPX .GT. NMAX/2) THEN
                     CALL plotSpline(X(i,:), Y(i,:),numpi, NCOLDN)
                     CALL plotSpline(X(j,:), Y(j,:),numpj, NCOLRN)
                     CALL QNERROR(' ',' ', 'Spaghetty; spline both in m- and n-direction')
                     MERR = MERR + 1
                     RETURN
                  ELSE
                     JADUBBEL = JADUBBEL + 1
                     call mess(LEVEL_DEBUG, 'SPLINE SUPPORT POINTS DOUBLED')
                     GOTO 5
                  ENDIF
               ELSE IF (NTYP(I) .EQ. 0 .AND. NTYP(J) .EQ. 0) THEN
                  call mess(LEVEL_DEBUG, ' BOTH UNDEFINED YET')
               ELSE IF (NTYP(J) .EQ. 0) THEN
                  NTYP(J) = - NTYP(I)
                  IF (CRP*NTYP(I) .LT. 0) THEN
                     call mess(LEVEL_DEBUG, ' SWITCHED J')
                     CALL SWITCH(X,Y, mmax, nmax,J,NUMPJ)
                     TJ = dble(NUMPJ) - 1 - TJ
                  ENDIF
               ELSE IF (NTYP(I) .EQ. 0) THEN
                  NTYP(I) = - NTYP(J)
                  IF (CRP*NTYP(J) .GT. 0) THEN
                     call mess(LEVEL_DEBUG, ' SWITCHED I')
                     CALL SWITCH(X,Y, mmax, nmax,I,NUMPI)
                     TI = dble(NUMPI) - 1 - TI
                  ENDIF
               ENDIF
               TIJ(I,J) = TI
               TIJ(J,I) = TJ
            ELSE IF (NUMCRO .GE. 2) THEN
               IF (NUMPX .GT. NMAX/2) THEN
                  CALL plotSpline(X(i,:), Y(i,:),numpi, NCOLDN)
                  CALL plotSpline(X(j,:), Y(j,:),numpj, NCOLRN)
                  CALL QNERROR(' ',' ', '2 splines appear to intersect more than once; modify splines')
                  MERR = MERR + 1
                  RETURN
               ELSE
                  JADUBBEL = JADUBBEL + 1
                  GOTO 5
               ENDIF
            ENDIF
    10 CONTINUE

      DO 20 I = 1,NUMSPL
         CALL NUMPold  (      X,     mmax, nmax, I,  NUMPI)
         IF (NTYP(I) .EQ. 0) THEN
            IONBENOEMD = IONBENOEMD + 1
!           IF (IONBENOEMD .GT. NUMSPL) THEN
            IF (IONBENOEMD .GT. 1000) THEN
               CALL plotSpline(X(i,:), Y(i,:),numpi, NCOLDN)
               CALL QNERROR(' ',' ', 'ONE OF THE SPLINES CANNOT BE ATTACHED IN THE GRID')
                MERR = MERR + 1
                RETURN
            ENDIF
            GOTO 6
         ENDIF
    20 CONTINUE

!     sorteren op type, eerst de horizontalen (N = CONSTANT)
      DO 40 I = 1,NUMSPL
         IF (NTYP(I) .EQ. -1) THEN
            DO 50 L = I+1,NUMSPL
               IF (NTYP(L) .EQ. 1) THEN
                  CALL CHAROW(    X  ,mmax, nmax,    I,    L, NUMPX)
                  CALL CHAROW(    Y  ,mmax, nmax,    I,    L, NUMPX)
                  CALL CHAROW(  TIJ  ,mmax, nmax,    I,    L, NUMSPL)
                  CALL CHACOL(  TIJ  ,mmax, nmax,    I,    L, NUMSPL)
                  NTYP(I) =  1
                  NTYP(L) = -1
                  GOTO 40
               ENDIF
    50      CONTINUE
         ENDIF
    40 CONTINUE

      DO 45 I = 1,NUMSPL
         IF (NTYP(I) .EQ. 1) NUMI = I
    45 CONTINUE

    59 CONTINUE
!     Sorteer de M
      JACHANGE = 0
      ICOUNT   = 0
      DO 60 I = 1,NUMI
!        CALL READYY(' ',0.35 + 0.65*REAL(I-1)/REAL(NUMSPL-1) )
         DO 60 J = NUMI+1,NUMSPL
            CALL NUMPold  (      X,     mmax, nmax, I,  NUMPI)
            CALL NUMPold  (      X,     mmax, nmax, J,  NUMPJ)
            IF (TIJ(I,J) .NE. 0) THEN
               DO 70 JK = J+1,NUMSPL
                  IF (TIJ(I,JK) .NE. 0) THEN
                     IF (TIJ(I,J) .GT. TIJ(I,JK) ) THEN
                        CALL CHAROW(    X,mmax, nmax,     J,     JK, NUMPX )
                        CALL CHAROW(    Y,mmax, nmax,     J,     JK, NUMPX )
                        CALL CHAROW(  TIJ,mmax, nmax,     J,     JK, NUMSPL)
                        CALL CHACOL(  TIJ,mmax, nmax,     J,     JK, NUMSPL)
                        JACHANGE = 1
                        ICOUNT   = ICOUNT + 1
                        IF (ICOUNT .GT. NUMSPL) THEN
                           CALL plotSpline(X(i,:), Y(i,:),numpi, NCOLDN)
                           CALL plotSpline(X(j,:), Y(j,:),numpj, NCOLRN)
                           CALL QNERROR(' ',' ', 'PROBLEM IN SPLINE ORDERING, MODIFY SPLINES')
                           MERR = MERR + 1
                        ENDIF
                        GOTO 59
                     ENDIF
                  ENDIF
    70         CONTINUE
            ENDIF
    60 CONTINUE

    79 CONTINUE
      ICOUNT = 0
!     Sorteer de N
      DO 80 I = NUMI+1,NUMSPL
!        CALL READYY(' ',0.35 + 0.65*REAL(I-1)/REAL(NUMSPL-1) )
         DO 80 J = 1,NUMI
            CALL NUMPold  (      X,     mmax, nmax, I,  NUMPI)
            CALL NUMPold  (      X,     mmax, nmax, J,  NUMPJ)
            IF (TIJ(I,J) .NE. 0) THEN
               DO 90 JK = J+1,NUMI
                  IF (TIJ(I,JK) .NE. 0) THEN
                     IF (TIJ(I,J) .GT. TIJ(I,JK) ) THEN
                        CALL CHAROW(    X,mmax, nmax,     J,     JK, NUMPX )
                        CALL CHAROW(    Y,mmax, nmax,     J,     JK, NUMPX )
                        CALL CHAROW(  TIJ,mmax, nmax,     J,     JK, NUMSPL)
                        CALL CHACOL(  TIJ,mmax, nmax,     J,     JK, NUMSPL)
                        JACHANGE = 1
                        ICOUNT   = ICOUNT + 1
                        IF (ICOUNT .GT. NUMSPL) THEN
                           CALL plotSpline(X(i,:), Y(i,:),numpi, NCOLDN)
                           CALL plotSpline(X(j,:), Y(j,:),numpj, NCOLRN)
                           CALL QNERROR(' ',' ', 'PROBLEM IN SPLINE ORDERING, MODIFY SPLINES')
                           MERR = MERR + 1
                        ENDIF
                        GOTO 79
                     ENDIF
                  ENDIF
    90         CONTINUE
            ENDIF
    80 CONTINUE
      IF (JACHANGE .EQ. 1) GOTO 59


!     Initialiseer ranking, start en eind, 1,2,3
      DO 100 I = 1,NUMSPL
         MN12(I,1) = 0
         MN12(I,2) = 0
         MN12(I,3) = 0
   100 CONTINUE

!     CALL SHOWADM(TIJ,MMAX,NMAX)

!     Eerst alles ranken in N richting
      DO 110 I  = 1,NUMI
         DO 120 J = NUMI+1, NUMSPL
            MAXN   = 0
            JJLAST = 1
            DO 130 JJ = 1,I
               IF (TIJ(J,JJ) .NE. 0) THEN
                  MAXN   = MN12(JJLAST,1) + 1
                  JJLAST = JJ
               ENDIF
   130      CONTINUE
            MN12(J,2) = MAXN
   120   CONTINUE
         MAXN = 0
         DO 140 J = NUMI+1,NUMSPL
            IF (TIJ(J,I) .NE. 0) MAXN = MAX(MN12(J,2),MAXN)
   140   CONTINUE
         MN12(I,1) = MAXN
   110 CONTINUE

!     Dan alles ranken in M richting
      DO 210 I  = NUMI+1,NUMSPL
         DO 220 J = 1, NUMI
            MAXM   = 0
            IILAST = NUMI+1
            DO 230 II = NUMI+1,I
               IF (TIJ(J,II) .NE. 0) THEN
                  MAXM   = MN12(IILAST,1) + 1
                  IILAST = II
               ENDIF
   230      CONTINUE
            MN12(J,3) = MAXM
   220   CONTINUE
         MAXM = 0
         DO 240 J = 1,NUMI
            IF (TIJ(J,I) .NE. 0) MAXM = MAX(MN12(J,3),MAXM)
   240   CONTINUE
         MN12(I,1) = MAXM
   210 CONTINUE

      DO 250 I = 1,NUMSPL
         MN12(I,2) = 0
         MN12(I,3) = 0
   250 CONTINUE

!     Daarna per spline begin- en eindpunt tellen, eerst N = constant
      DO 300 I = 1,NUMI
         DO 300 J = NUMI+1,NUMSPL
            IF (TIJ(I,J) .NE. 0) THEN
               IF (MN12(I,2) .EQ. 0) MN12(I,2) = MN12(J,1)
               MN12(I,3) = MN12(J,1)
            ENDIF
   300 CONTINUE

!     Dan M = constant
      DO 310 I = NUMI+1,NUMSPL
         DO 310 J = 1,NUMI
            IF (TIJ(I,J) .NE. 0) THEN
               IF (MN12(I,2) .EQ. 0) MN12(I,2) = MN12(J,1)
               MN12(I,3) = MN12(J,1)
            ENDIF
   310 CONTINUE
      CALL READYY(' ',0.95d0)

      DO 400 I = 1,NUMSPL
         WRITE(msgbuf,*) I, (MN12(I,J), J = 1,3)
         call dbg_flush()
   400 CONTINUE

      RETURN
      END subroutine sectr


      SUBROUTINE SWITCH(X, Y, mmax, nmax, JN, NUMPJ)
!      USE DIMENS
      implicit none
      integer :: mmax, nmax, jn, numpj
      double precision :: X(MMAX,NMAX),   Y(MMAX,NMAX)

       integer :: j
       double precision :: xh, yh

      DO 10 J = 1,NUMPJ/2
         XH              = X(JN,J)
         X(JN,J)         = X(JN,NUMPJ-J+1)
         X(JN,NUMPJ-J+1) = XH
         YH              = Y(JN,J)
         Y(JN,J)         = Y(JN,NUMPJ-J+1)
         Y(JN,NUMPJ-J+1) = YH
    10 CONTINUE
      RETURN
      END subroutine switch


!>    compute the intersection of two splines
      SUBROUTINE SECT3R(     XI,     YI,     XJ,     YJ,    mmax, nmax, imax, CRP,   &
                          NUMPI,  NUMPJ, NUMCRO,    TIV,    TJV,  XP,     YP)
      
      use m_missing
      use geometry_module, only: dbdistance, cross
      use m_sferic, only: jsferic, jasfer3D
      
      implicit none
!     BEPAAL HET SNYPUNT VAN DE 2 SPLINES NR I EN J      USE DIMENS
      
      integer,                           intent(in)    :: imax       !< array size
      integer,                           intent(in)    :: mmax, nmax !< unused
      integer,                           intent(in)    :: numpi      !< number of control points of first spline            
      integer,                           intent(in)    :: numpj      !< number of control points of second spline
      
      double precision, dimension(imax), intent(in)    :: xi, yi     !< control point coordinates of first spline
      double precision, dimension(imax), intent(in)    :: xj, yj     !< control point coordinates of second spline
      
      integer,                           intent(out)   :: numcro     !< number of intersections found
      
      double precision,                  intent(out)   :: crp        !< cross product (SPvdP: dimensional, so only look at sign)
      double precision,                  intent(out)   :: tiv        !< spline-coordinate of intersetion point on first spline (0 corresponds to first control point, 1 to second, and so on)
      double precision,                  intent(out)   :: tjv        !< spline-coordinate of intersetion point on second spline 
      double precision,                  intent(out)   :: xp, yp     !< coordinates of intersection point
      
    
      double precision :: ti, tj, tip, tjp, ti0, ti1, ti2, tj0, tj1, tj2, tii, tjj, &
                          tio, tjo, &
                          timx, tjmx, eps, eps2, xcr, ycr, crs, dis, &
                          xo, yo
      double precision :: sl, sm, xi2(imax), yi2(imax), xj2(imax), yj2(imax), xc(4), yc(4)
      integer :: i, j, jo, jacros, k
      
      double precision :: sdist, sdistmin


      NUMCRO = 0
      EPS    = 0.0001
      EPS2   = 0.000001
      TI     = -1
      TJ     = -1
      
      TI0 = 0d0
      TJ0 = 0d0
      
      sdistmin = 1d99   ! used for selecting middle intersection in case of multiple intersections

      DO I = 1,NUMPI-1                   ! SNIJDEN RECHTE LIJNSTUKKEN?
        DO J = 1,NUMPJ-1
           XC(1) = XI(I)
           XC(2) = XI(I+1)
           XC(3) = XJ(J)
           XC(4) = XJ(J+1)
           YC(1) = YI(I)
           YC(2) = YI(I+1)
           YC(3) = YJ(J)
           YC(4) = YJ(J+1)
           CRP = -1234d0
           CALL CROSS(XC(1),YC(1),XC(2),YC(2),XC(3),YC(3),XC(4),YC(4), &
                      JACROS,SL,SM,XCR,YCR,CRP, jsferic, dmiss)
          IF (JACROS .EQ. 1) THEN
             if ( numpi.eq.2 ) then
                sdist = min(sdistmin, abs(SL-0.5d0))
             else if ( numpj.eq.2 ) then
                sdist = abs(SM-0.5d0)
             else
                sdist = sdistmin
             end if
             
             if ( sdist.lt.sdistmin .or. NUMCRO.eq.0 ) then
                sdistmin = sdist
                TIP    = TI
                TJP    = TJ
!                NUMCRO = NUMCRO + 1
                NUMCRO = 1
                TI0    = I - 1
                TJ0    = J - 1
                TI     = TI0 + SL
                TJ     = TJ0 + SM
             else
!                NUMCRO = NUMCRO + 1
             end if
             
             IF (NUMCRO .GE. 2 .AND. ABS(TIP-TI) .GT. EPS .AND. ABS(TJP-TJ) .GT. EPS ) THEN
                 RETURN
             ENDIF
          ENDIF
        ENDDO
      ENDDO

      IF ( NUMCRO .EQ. 0) THEN
        DO I = 1,NUMPI-1
          JO = -999
          DO J = 1,NUMPJ-1               ! ZO NIET, GRENZEN OPREKKEN
            IF (J .GT. JO + 1) THEN      ! NA OPREKKEN GRENZEN NIET TWEE
!              JO    = J                  ! KEER NAAST ELKAAR ZOEKEN
!             SPvdP: previous line causes asymmetries
              XC(1) = XI(I)
              XC(2) = XI(I+1)
              XC(3) = XJ(J)
              XC(4) = XJ(J+1)
              YC(1) = YI(I)
              YC(2) = YI(I+1)
              YC(3) = YJ(J)
              YC(4) = YJ(J+1)
              SL = dmiss; SM = dmiss
              CALL CROSS(XC(1),YC(1),XC(2),YC(2),XC(3),YC(3),XC(4),YC(4), &
                         JACROS,SL,SM,XCR,YCR,CRP,jsferic, dmiss)
                         
!             BIJ NIEUW ZOEKEN MAG JE OOK NET BUITEN DE RECHTE LIJNSTUKKEN VALLEN 20-5-2003
              IF (SL .GT. -0.2 .AND. SL .LT. 1.2 .AND. SM .GT. -0.2 .AND. SM .LT. 1.2 ) THEN
                 NUMCRO = NUMCRO + 1
                 TI0    = I - 1
                 TJ0    = J - 1
                 TI     = TI0 + SL
                 TJ     = TJ0 + SM
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDIF
      
      TIO = TI0   ! SPvdP: TIO and TJO uninitialized, may be typing error
      TJO = TJ0

      IF (NUMCRO .EQ. 0) RETURN

      NUMCRO = 0

      TIMX  = NUMPI-1
      TJMX  = NUMPJ-1

      SL    = TI - TIO
      SM    = TJ - TJO

      TI    = MAX( 0.0, MIN(TIMX, TI))
      TJ    = MAX( 0.0, MIN(TJMX, TJ))

      CALL SPLINE(XI,NUMPI,XI2)
      CALL SPLINE(YI,NUMPI,YI2)
      CALL SPLINE(XJ,NUMPJ,XJ2)
      CALL SPLINE(YJ,NUMPJ,YJ2)

      TII = 1.0
      TJJ = 1.0
      K   = 0

    20 CONTINUE
         K   = K + 1

         IF (SL .GT. 0.  .AND. SL .LT. 1. ) THEN
            TII = 0.5*TII
         ENDIF
         IF (SM .GT. 0.  .AND. SM .LT. 1. ) THEN
            TJJ = 0.5*TJJ
         ENDIF
         TI1 = MAX( 0.0, MIN(TIMX, TI - TII/2) )
         TI2 = MAX( 0.0, MIN(TIMX, TI + TII/2) )
         TJ1 = MAX( 0.0, MIN(TJMX, TJ - TJJ/2) )
         TJ2 = MAX( 0.0, MIN(TJMX, TJ + TJJ/2) )
         TII = TI2 - TI1
         TJJ = TJ2 - TJ1

         CALL SPLINT(XI,XI2,NUMPI,TI1,XC(1))
         CALL SPLINT(YI,YI2,NUMPI,TI1,YC(1))
         CALL SPLINT(XI,XI2,NUMPI,TI2,XC(2))
         CALL SPLINT(YI,YI2,NUMPI,TI2,YC(2))
         CALL SPLINT(XJ,XJ2,NUMPJ,TJ1,XC(3))
         CALL SPLINT(YJ,YJ2,NUMPJ,TJ1,YC(3))
         CALL SPLINT(XJ,XJ2,NUMPJ,TJ2,XC(4))
         CALL SPLINT(YJ,YJ2,NUMPJ,TJ2,YC(4))

!        CALL SETCOL(14*K+26)
!        CALL MOVABS(XC(1),YC(1))
!        CALL  LNABS(XC(2),YC(2))
!        CALL MOVABS(XC(3),YC(3))
!        CALL  LNABS(XC(4),YC(4))

!        CALL RCIRC(XC(1),YC(1))
!        CALL RCIRC(XC(2),YC(2))
!        CALL RCIRC(XC(3),YC(3))
!        CALL RCIRC(XC(4),YC(4))
!        CALL TOEMAAR()

         XO = XCR
         YO = YCR
         SL = dmiss; SM = dmiss
         CRS = -1234d0
         CALL CROSS(XC(1),YC(1),XC(2),YC(2),XC(3),YC(3),XC(4),YC(4), &
                      JACROS,SL,SM,XCR,YCR,CRS, jsferic, dmiss)
         IF (SL .GT. -2.  .AND. SL .LT. 3.0 .AND. SM .GT. -2.  .AND. SM .LT. 3.0 ) THEN
            TIO = TI
            TJO = TJ

            TI  = TI1 + SL*TII
            TJ  = TJ1 + SM*TJJ

            TI  = MAX( 0.0, MIN(TIMX, TI) )
            TJ  = MAX( 0.0, MIN(TJMX, TJ) )

            IF (JACROS .EQ. 1) THEN  !ZOLANG IE NOG KRUIST WORDT UITPRODUCT BEPAALD
               NUMCRO = 1
               CRP    = CRS
            ENDIF

            IF (K .LT. 20) THEN
              IF (ABS(TI-TIO) .GT. EPS .OR. ABS(TJ-TJO) .GT. EPS) THEN
!                DIS = SQRT((XCR-XO)*(XCR-XO)+(YCR-YO)*(YCR-YO))
                dis = dbdistance(xo,yo,xcr,ycr, jsferic, jasfer3D, dmiss)
                IF (DIS .GT. EPS2) GOTO 20 ! NIET VERDER VERKLEINEN ALS PUNTEN AL BIJNA IDENTIEK
               ENDIF
            ENDIF
         ENDIF

      IF (NUMCRO .EQ. 1) THEN
         XP  = XCR
         YP  = YCR
         TIV = TI
         TJV = TJ
      ENDIF
!     CALL TOEMAAR()

      RETURN
      END subroutine sect3r


      SUBROUTINE NUMS  (      X,     mmax,nmax, MC,     NC)
!     GEEF AANTAL SPLINES MC EN MAXIMUM AANTAL PUNTEN OP SPLINE NC
!      USE DIMENS
      implicit none
      double precision :: X(MMAX,NMAX)
      integer :: mc, nc, mmax, nmax
      integer :: i, numpi
      MC = 0
      NC = 0
      DO 10 I = 1,MMAX
         CALL NUMPold  (      X,      mmax, nmax, I,  NUMPI)
         IF (NUMPI .NE. 0) THEN
            MC = I
            NC = MAX(NC,NUMPI)
         ENDIF
    10 CONTINUE
      RETURN
      END subroutine nums

      SUBROUTINE NUMPold  (      X,     mmax, nmax, MP,  NUMPI)
!     GEEF AANTAL PUNTEN VAN SPLINE MP
      !USE DIMENS
      use m_missing
      implicit none
      double precision :: X(MMAX,NMAX)
      integer :: mp, numpi, mmax, nmax

      integer :: j
      NUMPI = 0
      DO 10 J = 1,NMAX
         IF (X(MP,J) .NE. XYMIS) NUMPI = J
    10 CONTINUE
      RETURN
      END subroutine numpold

     SUBROUTINE PAKTIJ(T,mmax, nmax, TH,imax,I1,I2,J1,J2,NUM)
       implicit none
!     Haal lijn uit array en geef aantal niet nul NUM
     integer :: mmax, nmax, imax, i1, i2, j1, j2, num
      double precision :: T(MMAX,NMAX),TH(IMAX)
      integer :: i, j, k, ji1
      TH = 0d0
      K   = 0
      JI1 = 0
      DO 10 I  = I1,I2
         DO 10 J  = J1,J2
         IF (T(I,J) .NE. 0) THEN
            K     = K + 1
            TH(K) = T(I,J)
         ENDIF
    10 CONTINUE
      NUM = K
      RETURN
      END subroutine paktij

!>   generate grid between fixed points on a spline that itself is defined by control points
!>     in:  t(Nt) fixed points on spline
!>          x(N)  x-coordinates of spline control points
!>          y(N)  y-coordinates of spline control points
!>          imax  maximum array size (should be .ge. n, nt and kmax)
!>          N     number of spline control points
!>          Nt    number of fixed points
!>          MNfac number of grid intervals between fixed points
!>          H     significant height, where the grid should be equidistant (>0) or disable (<=0)
!>       
!>     out: xh(kmax) x-coordinates of grid points
!>          yh(kmax) y-coordinates of grid points
!>          kmax     number of grid points = 1+MNfac*(NT-1)
!>          tt(imax) spline-coordinates of grid points
     SUBROUTINE MAKESPL(T,X,Y,imax, N,NT,MNFAC,XH,YH,KMAX,TT,H)
     use m_gridsettings
     implicit none
     ! USE DIMENS

     integer :: imax, n, nt, kmax, mnfac
      double precision :: X(IMAX), Y(IMAX), X2(IMAX), Y2(IMAX), T(IMAX), S(IMAX),   &
           S2(IMAX), SSQ(IMAX), XH(IMAX), YH(IMAX),                  &
            A(IMAX), SL(IMAX), SR(IMAX)
      double precision, intent(in) :: H   !< for curvature adapted meshing
            
      double precision, dimension(IMAX), intent(out) :: TT !< spline-coordinates of grid points
                  
      integer :: L, k1, k2, jadip, k
!      COMMON /SPLINEFAC/ SPLFAC, SPLFAC2
!     Maak interpolatie

!     Eerst splines X,Y en S aanmaken
      CALL MAKES(X,Y,X2,Y2,T,S,S2,imax, N,NT,H)

      KMAX = MNFAC*(NT - 1) + 1

      IF (NT .GE. 2) THEN
         CALL MAKESSQ(S,A,SR,SL,SSQ,NT,MNFAC,IMAX)

!         DST  = REAL(NT-1) / REAL(KMAX-1)
!         ST   = 0
!C        Spline interpolatie in afstanden
!         SPLFACORG = SPLFAC
!         SPLFAC    = SPLFAC2
!         DO 10 K = 1,KMAX
!            CALL SPLINT(S,S2,NT,ST,SSQ(K))
!            ST = ST + DST
!    1    CONTINUE
!         SPLFAC = SPLFACORG

!        Check op positief en monotoon
         DO 20 L = 1,NT-1
            K1 = MNFAC*(L - 1) + 1
            K2 = K1 + MNFAC

            JADIP = 0
    23      IF (JADIP .EQ. 1) THEN
               DO 24 K = K1+1,K2-1
                  SSQ(K) = 0.5*( SSQ(K-1) + SSQ(K+1) )
    24         CONTINUE
            ENDIF

            DO 25 K = K1,K2-1
               IF ( SSQ(K+1) .LT. SSQ(K) ) THEN
                  JADIP = 1
                  GOTO 23
               ENDIF
    25      CONTINUE

    20   CONTINUE
      ELSE
         SSQ(1) = T(1)
      ENDIF

!     Punten terug invullen in oorspronkelijke spline
      DO 30 K = 1,KMAX
         CALL GETXY(T,X,X2,Y,Y2,imax,N,NT,SSQ(K),XH(K),YH(K),TT(K),H)
    30 CONTINUE

      RETURN
      END subroutine makespl

      SUBROUTINE MAKES(X,Y,X2,Y2,T,S,S2,imax, N,NT,H)
!     maak X,Y splines + afstandsarray en splines S op basis
!     van NT snijpunten
      !USE DIMENS
      implicit none
      integer :: imax, n, nt
      double precision :: X(IMAX), Y(IMAX), X2(IMAX), Y2(IMAX), T(IMAX), S(IMAX), S2(IMAX)
      double precision, intent(in) :: H   !< for curvature adapted meshing

      integer :: i

      CALL SPLINXY(X,Y,X2,Y2,N)
      
      DO 10 I = 1,NT
          CALL GETDIS(X,Y,X2,Y2,N,T(I),S(I),H)
    10 CONTINUE
      CALL SPLINE(S,NT,S2)
      RETURN
      END subroutine makes


      SUBROUTINE MAKESR(AR,S0,S1,SR,MFAC)
      implicit none
      integer :: mfac
      double precision :: ar, s0, s1
      double precision :: SR(MFAC+1)

      double precision :: ds, fac
      integer :: k
      DS    = 1
      SR(1) = 0
      DO K = 1,MFAC
         SR(K+1) = SR(K) + DS
         DS = DS*AR
      ENDDO

      FAC  = (S1-S0) / SR(MFAC+1)
      DO K = 0,MFAC
         SR(K+1) = S0 + FAC*SR(K+1)
      ENDDO
      RETURN
      END subroutine makesr


       SUBROUTINE MAKESSQ(S,A,SR,SL,SSQ,NT,MFAC,IMAX)
       implicit none
       integer :: nt, mfac, imax
      double precision :: S(IMAX), A(IMAX), SR(IMAX), SL(IMAX), SSQ(IMAX)
      double precision :: glad
      integer :: i, k, kr
      double precision :: ar, al
      GLAD(I)   = ( S(I+1)-S(I) ) / ( S(I)-S(I-1) )
      IF (NT .EQ. 2) THEN
         DO K = 1,MFAC + 1
            SSQ(K) = S(1) + ( S(2) - S(1) ) * (dble(K-1)) / dble(MFAC)
         ENDDO
      ELSE IF (NT .GE. 3) THEN
         DO I = 2,NT-1
            A(I) = GLAD(I)
         ENDDO
         A(1)  = A(2)
         A(NT) = A(NT-1)

         DO 10 I = 1,NT-1
            AR = A(I+1)**(1.0/dble(MFAC))
            CALL MAKESR(AR,S(I),S(I+1),SR,MFAC)
            AL = A(I)**(1.0/dble(MFAC))
            CALL MAKESR(AL,S(I),S(I+1),SL,MFAC)
            DO 20 K = 1,MFAC+1
               KR   = (I-1)*MFAC + K
               AR   = dble(K-1) / dble(MFAC)
               AL   = 1 - AR
               SSQ(KR) = AR*SR(K) + AL*SL(K)

               AR   = ( SSQ(KR) - S(I) ) / ( S(I+1) - S(I) )
               AL   = 1 - AR
               SSQ(KR) = AR*SR(K) + AL*SL(K)

!              AL = ( S(I+1) - SL(K) ) / ( S(I+1) - S(I) )
!              AR = ( SR(K)  -  S(I) ) / ( S(I+1) - S(I) )
!              AT = AL + AR
!              AL = AL/AT
!              AR = AR/AT
!              SSQ(KR) = AR*SR(K) + AL*SL(K)
    20      CONTINUE
    10   CONTINUE

      ENDIF

      RETURN
      END subroutine makessq

      SUBROUTINE GETDIS(X,Y,X2,Y2,N,TS,SS,H)
      
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D
      
      implicit none
!     Bereken de afstand SS van punt TS in X,Y, tov punt met TS = 0, ofwel N=1
      double precision ::  X(N), Y(N), X2(N), Y2(N)
      double precision :: ts, ss
      integer :: n
      double precision :: dt, t0, xt0, yt0, t1, xt1, yt1, dnx, dny, dsx, dsy
      
      double precision, intent(in) :: H  !< for curvature dependent meshing (>0) or disable (<=0)
      
      double precision :: curv
      logical          :: Lcurv
      
      Lcurv = ( H.gt.1d-8 )
      
      TS  = MIN(TS,dble(N))
      DT  = 0.1
      SS  = 0
      T0  = 0
      XT0 = X(1)
      YT0 = Y(1)
    10 CONTINUE
      T1  = T0 + DT
      IF (T1 .LT. TS) THEN
         CALL SPLINTXY(X,Y,X2,Y2,N,T1,XT1,YT1)
         if ( Lcurv ) call comp_curv(N,X,Y,X2,Y2,0.5d0*(T0+T1),curv,dnx,dny,dsx,dsy)
      ELSE
         CALL SPLINTXY(X,Y,X2,Y2,N,TS,XT1,YT1)
         if ( Lcurv ) call comp_curv(N,X,Y,X2,Y2,0.5d0*(T0+TS),curv,dnx,dny,dsx,dsy)
      ENDIF
      if ( .not.Lcurv ) then
!         SS  = SS + SQRT( (XT1-XT0)**2 + (YT1-YT0)**2 )
         SS  = SS + dbdistance(xt0,yt0,xt1,yt1,jsferic, jasfer3D, dmiss)
      else
         SS  = SS + dbdistance(xt0,yt0,xt1,yt1,jsferic, jasfer3D, dmiss)*(1d0+H*curv)
      end if
      
      T0  = T1
      XT0 = XT1
      YT0 = YT1
      IF (T1 .LT. TS) GOTO 10

      RETURN
      END

      SUBROUTINE GETXY(T,X,X2,Y,Y2,imax,N,NT,SSQ,XT,YT,TT,H)
!     zoek TT in X,Y, en XT,YT met dezelfde afstand geeft als
!     SSQ
      !USE DIMENS
      implicit none
      integer :: imax, n, nt
      double precision :: ssq, xt, yt
      double precision :: X(imax), Y(imax), X2(imax), Y2(imax), T(imax)
      double precision, intent(in) :: H   !< for curvature adapted meshing
      
      double precision, intent(out) :: TT

      double precision :: ax, bx, cx, tol, dis

      AX = T(1)
      CX = T(NT)
      BX = (AX+CX)/2
      TOL = 0.00001d0
!     Dan bijhorende T zoeken
      CALL GOLDDIS(AX,BX,CX,TOL,X,X2,Y,Y2,T,N,NT,TT,DIS,SSQ,H)

!     EN punt invullen
      CALL SPLINTXY(X,Y,X2,Y2,N,TT,XT,YT)

      RETURN
      END

      SUBROUTINE SPLINXY(X,Y,X2,Y2,N)
!      USE DIMENS
    implicit none
      integer :: n
      double precision :: X(N), Y(N), X2(N), Y2(N)
      CALL SPLINE(X,N,X2)
      CALL SPLINE(Y,N,Y2)
      RETURN
      END

      SUBROUTINE SPLINTXY(X,Y,X2,Y2,N,T,XT,YT)
      implicit none
      !USE DIMENS
      integer :: n
      double precision :: T
      double precision :: X(N), Y(N), X2(N), Y2(N)
      double precision :: xt, yt

      CALL SPLINT(X,X2,N,T,XT)
      CALL SPLINT(Y,Y2,N,T,YT)
      RETURN
      END

      SUBROUTINE GOLDDIS(AX,BX,CX,TOL,P,P2,Y,Y2,T, N,NT,XMIN,DIS,SSQ,H)
      implicit none
      !USE DIMENS
      integer :: imax, n, nt
      double precision :: P(N), P2(N), Y(N), Y2(N), T(N)
      double precision :: ax, bx, cx, tol, xmin, dis, ssq
      double precision, intent(in) :: H   !< for curvature adapted meshing

      double precision, PARAMETER :: R=.61803399d0,C=.38196602d0
      double precision :: x0, x1, x2, x3, f0, f1, f2, f3, d1, d2

!     Eendimensionaal zoeken van 'gebracked' minimum
      X0=AX
      X3=CX
      IF(ABS(CX-BX).GT.ABS(BX-AX))THEN
        X1=BX
        X2=BX+C*(CX-BX)
      ELSE
        X2=BX
        X1=BX-C*(BX-AX)
      ENDIF
      CALL GETDIS(P,Y,P2,Y2,N,X1,D1,H)
      F1 = ABS(D1 - SSQ)
      CALL GETDIS(P,Y,P2,Y2,N,X2,D2,H)
      F2 = ABS(D2 - SSQ)
1     IF(ABS(X3-X0).GT.TOL*max(ABS(X1)+ABS(X2),1d-8))THEN
!     IF(ABS(X3-X0).GT.TOL) THEN
        IF(F2.LT.F1)THEN
          X0=X1
          X1=X2
          X2=R*X1+C*X3
          F0=F1
          F1=F2
          CALL GETDIS(P,Y,P2,Y2,N,X2,D2,H)
          F2 = ABS(D2 - SSQ)
        ELSE
          X3=X2
          X2=X1
          X1=R*X2+C*X0
          F3=F2
          F2=F1
          CALL GETDIS(P,Y,P2,Y2,N,X1,D1,H)
          F1 = ABS(D1 - SSQ)
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
      END subroutine golddis

     !> Checks spline points in X and Y.
     !! Counts the number of splines and the maximum length and moves all
     !! All splines with <=1 point are reset and moved to the back.
     SUBROUTINE CHECKSPL(X, Y, mmax, nmax, MCS, NCS)
      USE m_missing
      implicit none
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)
      integer :: mmax, nmax, mcs, ncs

      integer :: numspl, numpx, numpi, numpj, i, j, k

!    5CONTINUE
      NUMSPL  = 0
      NUMPX   = 0
      DO 10 I = 1,MMAX-1
         CALL NUMPold(X,mmax, nmax, I,NUMPI)
         IF (NUMPI .LE. 1) THEN
            DO 15 K = 1,NMAX
               X(I,K) = XYMIS
               Y(I,K) = XYMIS
    15      CONTINUE
            DO 20 J = I+1,MMAX
               CALL NUMPold(X,mmax, nmax, J,NUMPJ)
               IF (NUMPJ .GT. 1) THEN
                  CALL CHAROW(X,mmax, nmax,  J, J-1, NMAX)
                  CALL CHAROW(Y,mmax, nmax,  J, J-1, NMAX)
               ENDIF
    20      CONTINUE
         ELSE IF (NUMPI .GE. 2) THEN
            NUMPX  = MAX(NUMPX,NUMPI)
            NUMSPL = NUMSPL + 1
         ENDIF
    10 CONTINUE
      MCS = NUMSPL
      NCS = NUMPX
      RETURN
      END subroutine checkspl

      SUBROUTINE CHAROW(      X,     mmax, nmax, I1,     I2, NUMSPL)
!     VERWISSEL RIJ I1 EN I2
!      USE DIMENS
      implicit none
      double precision :: X(MMAX,NMAX)
      integer :: mmax, nmax, i1, i2, numspl
      integer :: j
      double precision :: xh

      DO 10 J = 1,NUMSPL
         XH      = X(I1,J)
         X(I1,J) = X(I2,J)
         X(I2,J) = XH
    10 CONTINUE
      RETURN
      END subroutine charow

      SUBROUTINE CHACOL(      X,     mmax, nmax, J1,     J2, NUMSPL)
!     VERWISSEL KOLOM J1 EN J2
!      USE DIMENS
      implicit none
      double precision :: X(MMAX,NMAX)
      integer :: mmax, nmax, j1, j2, numspl
      integer :: i
      double precision :: xh

      DO 10 I = 1,NUMSPL
         XH      = X(I,J1)
         X(I,J1) = X(I,J2)
         X(I,J2) = XH
    10 CONTINUE
      RETURN
      END subroutine chacol


      SUBROUTINE MODFLD(     XH,     YH,      X,      Y,  &
                             mmax, nmax, MC,     NC,     MP,     NP,  &
                           NUMP,   NLOC,     IN,     JN)
      use m_missing
      use m_wearelt
      implicit none
!     VELDTRANSLATIE VAN XH,YH OP BASIS X,Y RONDOM PUNT MP,NP
!     ALS NLOC IS 1, DAN LOKAAL ORTHOGONALE TRANSLATIES
!     INVLOEDSSFEER IN I,J IS RESP NUMP*IN EN NUMP*JN
      integer :: mmax, nmax, mc, nc, mp, np, nump, nloc, in, jn

      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX), XH(MMAX,NMAX), YH(MMAX,NMAX)
      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      double precision :: pi2, x0, y0, dx0, dy0, rsx, rn, fr, dx, dy, xn, yn
      integer :: m1, n1, m2, n2, ismeer, i, j

      PI2 = ASIN(1d0)
      X0  = X(MP,NP)
      Y0  = Y(MP,NP)
      DX0 = XH(MP,NP) - X(MP,NP)
      DY0 = YH(MP,NP) - Y(MP,NP)

      RSX = MAX(DSIX,SQRT(DX0*DX0 + DY0*DY0) )
      IF (IN .EQ. 1 .AND. JN .EQ. 1) THEN
         IF (NPT .GE. 2) THEN
            ISMEER = 1
            M1 = MB(3)
            M2 = MB(4)
            N1 = NB(3)
            N2 = NB(4)
         ELSE
            ISMEER = 0
            M1 = MAX(1,MP-NUMP*IN)
            M2 = MIN(MC,MP+NUMP*IN)
            N1 = MAX(1,NP-NUMP*JN)
            N2 = MIN(NC,NP+NUMP*JN)
         ENDIF
      ELSE
         IF (NPT .GE. 3) THEN
            ISMEER = 1
            M1 = MAX(MB(3),MP-10000*IN)
            M2 = MIN(MB(4),MP+10000*IN)
            N1 = MAX(NB(3),NP-10000*JN)
            N2 = MIN(NB(4),NP+10000*JN)
         ELSE
            ISMEER = 0
            M1 = MAX(1,MP-NUMP*IN)
            M2 = MIN(MC,MP+NUMP*IN)
            N1 = MAX(1,NP-NUMP*JN)
            N2 = MIN(NC,NP+NUMP*JN)
         ENDIF
      ENDIF

      IF (NLOC .EQ. 1) THEN
         CALL TOLOCL(    DX0,    DY0,      X,      Y,  mmax, nmax, MP,     NP,      0        )
      ENDIF
      DO 10 I = M1,M2
         DO 10 J = N1,N2
            XN = X(I,J)
            IF (XN .NE. XYMIS) THEN
               YN = Y(I,J)
               IF (ISMEER .EQ. 1) THEN
                  CALL SMEERFUNCTIE(I,J,MP,NP,FR,IN,JN)
                  DX = DX0*FR
                  DY = DY0*FR
                  IF (NLOC .EQ. 1) THEN
                     CALL TOLOCL(     DX,     DY,      X,      Y,  mmax, nmax, I,      J,      1        )
                  ENDIF
                  XH(I,J) = XN + DX
                  YH(I,J) = YN + DY
               ELSE
                  RN = SQRT( (XN - X0)**2 + (YN - Y0)**2 )
                  IF (RN .LT. RSX) THEN
!                    FR = (RSX - RN)/RSX
                     RN = PI2*RN/RSX
!                    FR = COS(RN)/(1.0 + SIN(RN))
                     FR = (1 + COS(2*RN) ) / 2
                     DX = DX0*FR
                     DY = DY0*FR
                     IF (NLOC .EQ. 1) THEN
                        CALL TOLOCL(     DX,     DY,      X,      Y, mmax, nmax,  I,      J,      1        )
                     ENDIF
                     XH(I,J) = XN + DX
                     YH(I,J) = YN + DY
                  ENDIF
               ENDIF
            ENDIF
    10 CONTINUE
      RETURN
      END subroutine modfld

      SUBROUTINE TOLOCL(    DX0,    DY0,      X,      Y,  mmax, nmax, MP,     NP,    NTO        )
      use m_missing
      use m_sferic
      implicit none
!     TRANSFORMEER NAAR LOCALE OF GLOBALE SYSTEEM IN EEN GRID
!     WEEG TUSSEN MAX(!) VIER MOGELIJKE MEDE RICHTINGBEPALENDE OMLIGGEND
!     NTO = 0 IS NAAR LOKAAL, NTO = 1 IS NAAR GLOBAAL
      integer :: mmax, nmax, mp, np, nto
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)

      double precision :: x0, y0, dx0, dy0, x1t, y1t, x1p, x2p, x2t, y2t, xn1, xn2, dx, dy

      X0  = X(MP,NP)
      Y0  = Y(MP,NP)
      X1T = 0
      Y1T = 0
      XN1 = 0
      X2T = 0
      Y2T = 0
      XN2 = 0
      IF (MP .NE. MMAX) THEN
         X1P = X(MP+1,NP)
         IF (X1P .NE. XYMIS) THEN
            X1T = X1P - X0
            Y1T = Y(MP+1,NP) - Y0
         ENDIF
      ENDIF
      IF (MP .NE. 1) THEN
         X1P = X(MP-1,NP)
         IF (X1P .NE. XYMIS) THEN
            X1T = X1T - X1P + X0
            Y1T = Y1T - Y(MP-1,NP) + Y0
         ENDIF
      ENDIF

      IF (JSFERIC .EQ. 1) X1T = X1T*COS( DG2RD*Y0 )

      IF (NP .NE. NMAX) THEN
         X2P = X(MP,NP+1)
         IF (X2P .NE. XYMIS) THEN
            X2T = X2P - X0
            Y2T = Y(MP,NP+1) - Y0
         ENDIF
      ENDIF
      IF (NP .NE. 1) THEN
         X2P = X(MP,NP-1)
         IF (X2P .NE. XYMIS) THEN
            X2T = X2T - X2P + X0
            Y2T = Y2T - Y(MP,NP-1) + Y0
         ENDIF
      ENDIF

      IF (JSFERIC .EQ. 1) X2T = X2T*COS( DG2RD*Y0 )

      XN1 = SQRT(X1T*X1T + Y1T*Y1T)
      XN2 = SQRT(X2T*X2T + Y2T*Y2T)
!     error bij belgen op nt4.0, onduidelijk, daarom afgevangen
      if (xn1 .ne. 0) then
         IF (NTO .EQ. 0) THEN
            DX  = (DX0*X1T + DY0*Y1T)/XN1
            DY  = (DY0*X1T - DX0*Y1T)/XN1
         ELSE
            DX  = (DX0*X1T - DY0*Y1T)/XN1
            DY  = (DX0*Y1T + DY0*X1T)/XN1
         ENDIF
      else
         dx = 0
         dy = 0
      endif
      DX0 = DX
      DY0 = DY
      RETURN
      END SUBROUTINE TOLOCL


      SUBROUTINE DOSMOOTH(NFLD)
      use m_gridsettings
      use m_grid
      use unstruc_colors
      use unstruc_messages
      implicit none
      integer :: nfld

      integer :: ndraw
      COMMON /DRAWTHIS/ ndraw(50)

      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      double precision, allocatable :: XH(:,:), YH(:,:)

      integer :: MD, ND, M1, M2, N1, N2, JS, I, J, K, JA1, JA2
      double precision :: R1, R2, R3, FR, XX, YY, X21, X22, Y21, Y22,X41, X42, Y41, Y42, &
                          A, B, TV1, TV2

      allocate(XH(MMAX,NMAX), YH(MMAX,NMAX))

      IF (NDRAW(8) .EQ. 0) CALL READYY('SMOOTHING GRID', 0d0 )
!      CALL ISITU(      X,      Y,     MC,   NC,    IJC,  IJYES)!!!Oud
      ! Deze routine dosmooth wordt alleen uit editgridlineblock aangeroepen
      ! met de xc, ijyes, etc. uit m_grid. Diezelfde m_grid wordt in isitu gebruikt
      ! dus hoeven het niet meer door te geven.
      CALL ISITU()
      CALL PUTARR(Xc,XH,MMAX,NMAX)
      CALL PUTARR(Yc,YH,MMAX,NMAX)

      MD = MB(2) - MB(1)
      ND = NB(2) - NB(1)

      M1 = MB(3)
      N1 = NB(3)
      M2 = MB(4)
      N2 = NB(4)

      JS = 1
      DO 10 K = 1,ITSMO

         IF (MD .EQ. 0 .AND. NFLD .EQ. 9) THEN
!           verticale linemodes

            DO 20 I = 1,MC
               DO 20 J = 2,NC-1
                  IF (J .GT. N1 .AND. J .LT. N2) THEN
                     IF (IJC(I,J) .EQ. 10 .OR. IJC(I,J) .EQ.  2 .OR. IJC(I,J) .EQ.  4 ) THEN
                         R1 = SQRT( (XH(I,J) - XH(I,J-1))**2 +  &
                                    (YH(I,J) - YH(I,J-1))**2 )
                         R2 = SQRT( (XH(I,J) - XH(I,J+1))**2 +  &
                                    (YH(I,J) - YH(I,J+1))**2 )

                         IF ( (M2-M1) .NE. 0) THEN
                            CALL SMEERFUNCTIE(I,J,MB(1),J,FR,1,0)
                         ELSE
                            FR = 1d0
                         ENDIF

                         IF (JS .EQ. 1) THEN
                            IF (R1 .GT. R2) THEN
                               R3 = (R1 - R2)/2
                               A  = FR*CSMO*R3/R1
                               XX = XH(I,J) + A*( XH(I,J-1) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I,J-1) - Yc(I,J) )
                            ELSE
                               R3 = (R2 - R1)/2
                               A  = FR*CSMO*R3/R2
                               XX = XH(I,J) + A*( XH(I,J+1) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I,J+1) - Yc(I,J) )
                            ENDIF
                         ELSE
                            A  = 0.1
                            IF (R1 .LT. R2) THEN
                               XX = XH(I,J) + A*( XH(I,J-1) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I,J-1) - Yc(I,J) )
                            ELSE
                               XX = XH(I,J) + A*( XH(I,J+1) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I,J+1) - Yc(I,J) )
                            ENDIF
                         ENDIF
                         Xc(I,J) = XX
                         Yc(I,J) = YY
                     ENDIF
                  ENDIF
    20      CONTINUE

         ELSE IF (ND .EQ. 0 .AND. NFLD .EQ. 9) THEN
!           horizontale linemodes

            DO 40 I = 2,MC-1
               DO 40 J = 1,NC
                  IF (I .GT. M1 .AND. I .LT. M2) THEN
                     IF (IJC(I,J) .EQ. 10 .OR. IJC(I,J) .EQ.  1 .OR. IJC(I,J) .EQ.  3 ) THEN
                         R1 = (XH(I,J) - XH(I-1,J))**2 +      &
                              (YH(I,J) - YH(I-1,J))**2
                         R2 = (XH(I,J) - XH(I+1,J))**2 +      &
                              (YH(I,J) - YH(I+1,J))**2

                         IF ( (N2-N1) .NE. 0) THEN
                            CALL SMEERFUNCTIE(I,J,I,NB(1),FR,0,1)
                         ELSE
                            FR = 1
                         ENDIF

                         IF (JS .EQ. 1) THEN
                            IF (R1 .GT. R2) THEN
                               R3 = (R1 - R2)/2
                               if ( abs(R1).lt.1d-8 ) then
                                  A = 0.5d0
                               else
                               A  = FR*CSMO*R3/R1
                               end if
                               XX = XH(I,J) + A*( XH(I-1,J) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I-1,J) - Yc(I,J) )
                            ELSE
                               R3 = (R2 - R1)/2
                               if ( abs(R2).lt.1d-8 ) then
                                  A = 0.5d0
                               else
                                  A  = FR*CSMO*R3/R2
                               end if
                               XX = XH(I,J) + A*( XH(I+1,J) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I+1,J) - Yc(I,J) )
                            ENDIF
                         ELSE
                            A  = 0.1
                            IF (R1 .LT. R2) THEN
                               XX = XH(I,J) + A*( XH(I-1,J) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I-1,J) - Yc(I,J) )
                            ELSE
                               XX = XH(I,J) + A*( XH(I+1,J) - Xc(I,J) )
                               YY = YH(I,J) + A*( YH(I+1,J) - Yc(I,J) )
                            ENDIF
                         ENDIF
                         Xc(I,J) = XX
                         Yc(I,J) = YY
                     ENDIF
                  ENDIF
    40      CONTINUE

         ELSE IF (NFLD .EQ. 17) THEN
!           blockmode

            B  = CSMO
            A  = 1 - B
            DO 60 I = 1,MC
               DO 60 J = 1,NC
                  IF (I .GE. M1 .AND. I .LE. M2 .AND.          &
                      J .GE. N1 .AND. J .LE. N2 ) THEN
                     IF (IJC(I,J) .EQ. 10) THEN
                        Xc(I,J) = A*XH(I,J) + B*(XH(I-1,J) + XH(I+1,J))/4    &
                                            + B*(XH(I,J-1) + XH(I,J+1))/4
                        Yc(I,J) = A*YH(I,J) + B*(YH(I-1,J) + YH(I+1,J))/4    &
                                            + B*(YH(I,J-1) + YH(I,J+1))/4
                     ELSE IF (IJC(I,J) .GE. 1 .AND. IJC(I,J) .LE. 4)THEN
                        IF (IJC(I,J) .EQ. 1) THEN
                           XX = A*  XH(I,J) + B*                            &
                                ( XH(I-1,J) + XH(I+1,J) + XH(I,J+1) )/3
                           YY = A*  YH(I,J) + B*                            &
                                ( YH(I-1,J) + YH(I+1,J) + YH(I,J+1) )/3
                        ELSE IF (IJC(I,J) .EQ. 3) THEN
                           XX = A*  XH(I,J) + B*                            &
                                ( XH(I-1,J) + XH(I+1,J) + XH(I,J-1) )/3
                           YY = A*  YH(I,J) + B*                            &
                                ( YH(I-1,J) + YH(I+1,J) + YH(I,J-1) )/3
                        ELSE IF (IJC(I,J) .EQ. 2) THEN
                           XX = A*  XH(I,J) + B*                            &
                                ( XH(I,J-1) + XH(I,J+1) + XH(I-1,J) )/3
                           YY = A*  YH(I,J) + B*                            &
                                ( YH(I,J-1) + YH(I,J+1) + YH(I-1,J) )/3
                        ELSE IF (IJC(I,J) .EQ. 4) THEN
                           XX = A*  XH(I,J) + B*                            &
                                ( XH(I,J-1) + XH(I,J+1) + XH(I+1,J) )/3
                           YY = A*  YH(I,J) + B*                            &
                                ( YH(I,J-1) + YH(I,J+1) + YH(I+1,J) )/3
                        ENDIF
                        CALL MOVABS(XH(I,J),YH(I,J))
                        CALL LNABS(XX,YY)
                        IF (IJC(I,J) .EQ. 1 .OR. IJC(I,J) .EQ. 3) THEN
                           X21 = XH(I-1,J)
                           Y21 = YH(I-1,J)
                           X22 = XH(I+1,J)
                           Y22 = YH(I+1,J)
                        ELSEIF(IJC(I,J) .EQ. 2 .OR.IJC(I,J).EQ.4) THEN
                           X21 = XH(I,J-1)
                           Y21 = YH(I,J-1)
                           X22 = XH(I,J+1)
                           Y22 = YH(I,J+1)
                        ENDIF
                        CALL ORTPRO2(XH(I,J),YH(I,J),X21,Y21,       &
                                       XX,YY,X41,Y41,TV1,JA1)
                        CALL ORTPRO2(XH(I,J),YH(I,J),X22,Y22,       &
                                       XX,YY,X42,Y42,TV2,JA2)
                        IF (JA1 .EQ. 1 .AND. JA2 .EQ. 1) THEN
                           IF (TV2 .GT. TV1) THEN
                               Xc(I,J) = X42
                               Yc(I,J) = Y42
                            ELSE
                               Xc(I,J) = X41
                               Yc(I,J) = Y41
                            ENDIF
                        ELSE IF (JA1 .EQ. 1) THEN
                           Xc(I,J) = X41
                           Yc(I,J) = Y41
                        ELSE IF (JA2 .EQ. 1) THEN
                           Xc(I,J) = X42
                           Yc(I,J) = Y42
                        ELSE
                           Xc(I,J) = (X41 + X42)/2
                           Yc(I,J) = (Y41 + Y42)/2
                           WRITE(msgbuf,*) 'BLOCK VORM VERLIES'; call dbg_flush()
                        ENDIF
                     ENDIF
                  ENDIF
    60      CONTINUE

         ENDIF

         CALL PUTARR(Xc,XH,MMAX,NMAX)
         CALL PUTARR(Yc,YH,MMAX,NMAX)
         IF (NDRAW(8) .EQ. 0) THEN
            CALL READYY(' ', dble(K) / dble(ITSMO) )
         ELSE
            CALL TEKGRD(Xc,Yc,mmax, nmax, M1,N1,M2,N2,NCOLDG,NDRAW(38),-1,mc) ! key=-1 is unknown (but unused anyway)
         ENDIF

    10 CONTINUE

      CALL PUTARR(XH,Xc,MMAX,NMAX)
      CALL PUTARR(YH,Yc,MMAX,NMAX)
      deallocate(XH, YH)
      IF (NDRAW(8) .EQ. 0) CALL READYY(' ', -1d0 )

      RETURN
      END subroutine dosmooth

      SUBROUTINE ORTPRO2(X1,Y1,X2,Y2,X3,Y3,X4,Y4,TV,JA)
      implicit none
      double precision :: X1, Y1, X2, Y2, X3, Y3, X4, Y4, TV
      integer :: JA

      double precision :: DX, DY, R2

      JA = -1
      DX = X2 - X1
      DY = Y2 - Y1
      R2 = (DX*DX + DY*DY)
      TV = (X3*DX + Y3*DY - X1*DX - Y1*DY) / R2
      X4 = X1 + TV*DX
      Y4 = Y1 + TV*DY
      IF (0D0 .LE. TV .AND. TV .LE. 1D0) JA = 1
      TV = TV * SQRT(R2)
      RETURN
      END SUBROUTINE ORTPRO2


      SUBROUTINE LINEMIRROR()!X, Y, mmax, nmax, MC, NC, IJC,IJYES)
      use m_missing
      use m_grid
      use m_gridsettings
      use unstruc_colors
      implicit none

!      integer :: mmax, nmax, mc, nc
!      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)
!      INTEGER IJC(MMAX,NMAX), IJYES(MMAX,NMAX)

      integer :: ndraw
      COMMON /DRAWTHIS/ ndraw(50)
      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: M1, M2, N1, N2, MD, ND, M, N
      double precision :: A, B

      CALL ISITU()

      M1 = MB(1)
      N1 = NB(1)
      M2 = MB(2)
      N2 = NB(2)
      MD = M2 - M1
      ND = N2 - N1
      A  = 1 + FACMIR
      B  = - FACMIR

      IF (MD .EQ. 0) THEN
         IF (M1 .EQ. MC) THEN
            IF (M1 .GE. MMAX-1) THEN
               CALL OKAY(0)
               CALL QNERROR('TOO MANY GRIDLINES IN M-DIRECTION',' ',' ')
               RETURN
            ENDIF
            MC = MC + 1
         ELSE
            IF (M1 .EQ. 1) THEN
               CALL SHIFXY(1, 0, M1, N1)! X, Y, mmax, nmax, MC, NC,
            ENDIF
         ENDIF
         M = M1
         DO N = N1,N2
            IF (Xc(M,N) .NE. XYMIS) THEN
               IF (Xc(M+1,N) .EQ. XYMIS) THEN
                  IF (Xc(M-1,N) .NE. XYMIS) THEN
                     Xc(M+1,N) = A*Xc(M,N) + B*Xc(M-1,N)
                     Yc(M+1,N) = A*Yc(M,N) + B*Yc(M-1,N)
                  ENDIF
               ELSE IF (Xc(M-1,N) .EQ. XYMIS) THEN
                  IF (Xc(M+1,N) .NE. XYMIS) THEN
                     Xc(M-1,N) = A*Xc(M,N) + B*Xc(M+1,N)
                     Yc(M-1,N) = A*Yc(M,N) + B*Yc(M+1,N)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ELSE IF (ND .EQ. 0) THEN
         IF (N1 .EQ. NC) THEN
            IF (N1 .GE. NMAX-1) THEN
               CALL OKAY(0)
               CALL QNERROR('TOO MANY GRIDLINES IN N-DIRECTION',' ',' ')
               RETURN
            ENDIF
            NC = NC + 1
         ELSE
            IF (N1 .EQ. 1) THEN
               CALL SHIFXY(0, 1, M1, N1)! X, Y, mmax, nmax, MC, NC,
            ENDIF
         ENDIF
         N = N1
         DO M = M1,M2
            IF (Xc(M,N) .NE. XYMIS) THEN
               IF (Xc(M,N+1) .EQ. XYMIS) THEN
                  IF (Xc(M,N-1) .NE. XYMIS) THEN
                     Xc(M,N+1) = A*Xc(M,N) + B*Xc(M,N-1)
                     Yc(M,N+1) = A*Yc(M,N) + B*Yc(M,N-1)
                  ENDIF
               ELSE IF (Xc(M,N-1) .EQ. XYMIS) THEN
                  IF (Xc(M,N+1) .NE. XYMIS) THEN
                     Xc(M,N-1) = A*Xc(M,N) + B*Xc(M,N+1)
                     Yc(M,N-1) = A*Yc(M,N) + B*Yc(M,N+1)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDIF
      RETURN
      END subroutine linemirror


      SUBROUTINE SMEERFUNCTIE(I,J,MP,NP,FR,IN,JN)
      implicit none
      integer :: i, j, mp, np, in, jn
      double precision :: fr

      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      double precision :: pi, phi, fri, frj
      PI = ACOS(-1d0)

      IF (I .EQ. MP) THEN
         PHI = 0
      ELSE IF (I .GT. MP .AND. I .LT. MB(4) ) THEN
         PHI = PI*dble(I - MP)/dble( MB(4) - MP )
      ELSE IF (I .LT. MP .AND. I .GT. MB(3)) THEN
         PHI = PI*dble(MP - I)/dble( MP - MB(3) )
      ELSE
         PHI = PI
      ENDIF
      FRI = (1 + COS(PHI) ) / 2

      IF (J .EQ. NP) THEN
         PHI = 0
      ELSE IF (J .GT. NP .AND. J .LT. NB(4) ) THEN
         PHI = PI*dble(J - NP)/dble( NB(4) - NP )
      ELSE IF (J .LT. NP .AND. J .GT. NB(3)) THEN
         PHI = PI*dble(NP - J)/dble( NP - NB(3) )
      ELSE
         PHI = PI
      ENDIF
      FRJ = (1 + COS(PHI) ) / 2

      IF (IN .EQ. 1 .AND. JN .EQ. 1) THEN
         FR = SQRT(FRI*FRJ)
      ELSE IF (JN .EQ. 1) THEN
         FR = FRJ
      ELSE IF (IN .EQ. 1) THEN
         FR = FRI
      ENDIF

      RETURN
      END subroutine smeerfunctie

      SUBROUTINE RESETB(NPUT)
      implicit none
      integer :: nput
      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE
      MB    = 0
      NB    = 0
      NPT   = 0
      MB2   = 0
      NB2   = 0
      NPT2  = 0
      NPUTO = NPUT
      RETURN
      END subroutine resetb

      SUBROUTINE RESTOREB(NPUT)
      implicit none
      integer ::  nput
      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      MB = MB2
      NB = NB2
      NPT  = NPT2
      NPUT = NPUTO
      RETURN
      END subroutine restoreb

      SUBROUTINE SAVEB(NPUT)
      implicit none
      integer ::  nput
      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      MB2 = MB
      NB2 = NB

      NPT2  = NPT
      NPUTO = NPUT
      RETURN
      END subroutine saveb

      SUBROUTINE TEKB(X,Y,MMAX,NMAX,NCOL)
      implicit none
      integer :: mmax, nmax, ncol
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)

      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: i

      IF (ITYPE .EQ. 1) THEN
         CALL TEKLN2(X, Y, mmax, nmax, MB(1), NB(1), MB(2), NB(2), NCOL)
      ENDIF
      DO 10 I = 1,6
         IF (MB(I) .NE. 0) THEN
             CALL CIRR(X(MB(I),NB(I)), Y(MB(I),NB(I)),NCOL)
         ENDIF
    10 CONTINUE
      RETURN
      END subroutine tekb

      SUBROUTINE NEWBLOCKPOINT(MP,NP,JA,IPT)
      implicit none
      integer :: mp, np, ja, ipt
      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE
!     NIEUW PUNT = 1, OUD PUNT = 0, NIEW PUNT MAAR REEDS VIER PUNTEN = -1
      integer :: i
      JA = 1
      DO 10 I=1,NPT
         IF (MP .EQ. MB(I) .AND. NP .EQ. NB(I)) THEN
           JA  = 0
           IPT = I
           RETURN
         ENDIF
    10 CONTINUE
      IPT = NPT + 1
      IF (NPT .EQ. 4) JA = -1
      RETURN
      END subroutine newblockpoint

      SUBROUTINE ONSAMELINE(IPT,MP,NP,JA)
      implicit none
      integer :: mp, np, ja, ipt
      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: md, nd
      JA = 1
      IF (ITYPE .EQ. 1) THEN
         IF (IPT .EQ. 1 .AND. MB(2) .NE. 0) THEN
            MD = MP - MB(2)
            ND = NP - NB(2)
            IF (MD .NE. 0 .AND. ND .NE. 0) JA = 0
         ELSE IF (IPT .EQ. 2) THEN
            MD = MP - MB(1)
            ND = NP - NB(1)
            IF (MD .NE. 0 .AND. ND .NE. 0) JA = 0
         ENDIF
      ENDIF
      RETURN
      END subroutine onsameline

      SUBROUTINE POSITIVEBLOK()
      implicit none
      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: mh, nh, m1, n1, m2, n2, i
      IF (NPT .LE. 1) RETURN

!     IF (ITYPE .EQ. 1) THEN
         IF (MB(2) .LT. MB(1)) THEN
            MH    = MB(1)
            MB(1) = MB(2)
            MB(2) = MH
         ENDIF
         IF (NB(2) .LT. NB(1)) THEN
            NH    = NB(1)
            NB(1) = NB(2)
            NB(2) = NH
         ENDIF
!     ENDIF

      M1 = MB(1)
      N1 = NB(1)
      M2 = MB(2)
      N2 = NB(2)
      DO 10 I = 1,NPT
         M1 = MIN(MB(I),M1)
         N1 = MIN(NB(I),N1)
         M2 = MAX(MB(I),M2)
         N2 = MAX(NB(I),N2)
    10 CONTINUE
      MB(3) = M1
      NB(3) = N1
      MB(4) = M2
      NB(4) = N2
      RETURN
      END subroutine positiveblok

      SUBROUTINE TEKLN2(X, Y, mmax, nmax, M1, N1, M2, N2, NCOL)
!     TEKEN EEN LIJN IN GRID (MET CIRKELS ROND DE UITEINDEN)
      use m_missing
      implicit none
      integer :: mmax, nmax, m1, n1, m2, n2, ncol
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)

      integer :: istart, i, j, in, jn

      CALL SETCOL(NCOL)
      ISTART = 0
      IF (M1 .NE. 0) CALL CIRR(X(M1,N1),Y(M1,N1),NCOL)
      IF (M2 .NE. 0) CALL CIRR(X(M2,N2),Y(M2,N2),NCOL)
      IF (M1 .NE. 0 .AND. M2 .NE. 0) THEN
         IN = SIGN(1,M2-M1)
         JN = SIGN(1,N2-N1)
         DO 10 I = M1,M2,IN
            DO 10 J = N1,N2,JN
               IF (X(I,J) .NE. XYMIS) THEN
                  IF (ISTART .EQ. 0) THEN
                     CALL MOVABS(X(I,J),Y(I,J))
                     ISTART = 1
                  ELSE
                     CALL LNABS(X(I,J),Y(I,J))
                  ENDIF
               ELSE
                  ISTART = 0
               ENDIF
    10   CONTINUE
      ENDIF
      RETURN
      END subroutine tekln2


      !> This routine operates directly on active grid data from m_grid
      SUBROUTINE MODGR1(NPUT, MP, NP, IN, JN)!, NCOL)!XH, YH, mmax, nmax, MC, NC,
      use m_missing
      use m_grid
      use unstruc_colors
      implicit none

      integer :: nput, mp, np, in, jn
!      double precision :: XH(MMAX,NMAX), YH(MMAX,NMAX)
!     een beetje flauw geprogrammeerd, ook tekenen bij insert mode

      integer :: ja

      IF (NPUT .EQ. -1) THEN
         JA = 0
         IF (MP .GE. MMAX-1) THEN
           call increasegrid(mp+2,nmax)
!           CALL OKAY(0)
!           CALL QNERROR('Grid Becomes too Large in M-Dimension',' ',' ')
           RETURN
         ELSE
            IF (MP .EQ. 1 .AND. IN .EQ. -1) THEN
               CALL SHIFXY(1,      0,     MP,     NP)!     XH,     YH,     mmax, nmax, MC,     NC,
            ENDIF
         ENDIF
         IF (NP .GE. NMAX-1) THEN
           call increasegrid(mmax, np+2)
!           CALL OKAY(0)
!           CALL QNERROR('Grid Becomes too Large in N-Dimension',' ',' ')
           RETURN
         ELSE
            IF (NP .EQ. 1 .AND. JN .EQ. -1) THEN
               CALL SHIFXY(0,      1,     MP,     NP)!     XH,     YH,     mmax, nmax, MC,     NC,
            ENDIF
         ENDIF

         IF (IN .EQ. 1) THEN
            IF (MP .EQ. MC-1) MC = MC + 1
            IF (Xc(MP+2,NP) .EQ. XYMIS) THEN
                Xc(MP+2,NP) = 2*Xc(MP+1,NP)  - Xc(MP,NP)
                Yc(MP+2,NP) = 2*Yc(MP+1,NP)  - Yc(MP,NP)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC,  MP+2,     NP,   NCOLDG      )
                JA = 1
            ENDIF
            IF (Xc(MP+2,NP+1) .EQ. XYMIS) THEN
                Xc(MP+2,NP+1) = 2*Xc(MP+1,NP+1)  - Xc(MP,NP+1)
                Yc(MP+2,NP+1) = 2*Yc(MP+1,NP+1)  - Yc(MP,NP+1)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC,  MP+2,   NP+1,   NCOLDG      )
                JA = 1
            ENDIF
         ELSE IF (IN .EQ. -1) THEN
            IF (Xc(MP-1,NP) .EQ. XYMIS) THEN
                Xc(MP-1,NP) = 2*Xc(MP,NP)  - Xc(MP+1,NP)
                Yc(MP-1,NP) = 2*Yc(MP,NP)  - Yc(MP+1,NP)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC, MP-1,     NP,   NCOLDG      )
                JA = 1
            ENDIF
            IF (Xc(MP-1,NP+1) .EQ. XYMIS) THEN
                Xc(MP-1,NP+1) = 2*Xc(MP,NP+1)  - Xc(MP+1,NP+1)
                Yc(MP-1,NP+1) = 2*Yc(MP,NP+1)  - Yc(MP+1,NP+1)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC, MP-1,   NP+1,   NCOLDG      )
                JA = 1
            ENDIF
         ELSE IF (JN .EQ. 1) THEN
            IF (NP .EQ. NC-1) NC = NC + 1
            IF (Xc(MP,NP+2) .EQ. XYMIS) THEN
                Xc(MP,NP+2) = 2*Xc(MP,NP+1)  - Xc(MP,NP)
                Yc(MP,NP+2) = 2*Yc(MP,NP+1)  - Yc(MP,NP)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC, MP,   NP+2,   NCOLDG      )
                JA = 1
            ENDIF
            IF (Xc(MP+1,NP+2) .EQ. XYMIS) THEN
                Xc(MP+1,NP+2) = 2*Xc(MP+1,NP+1)  - Xc(MP+1,NP)
                Yc(MP+1,NP+2) = 2*Yc(MP+1,NP+1)  - Yc(MP+1,NP)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC, MP+1,   NP+2,   NCOLDG      )
                JA = 1
            ENDIF
         ELSE IF (JN .EQ. -1) THEN
            IF (Xc(MP,NP-1) .EQ. XYMIS) THEN
                Xc(MP,NP-1) = 2*Xc(MP,NP)  - Xc(MP,NP+1)
                Yc(MP,NP-1) = 2*Yc(MP,NP)  - Yc(MP,NP+1)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC, MP,   NP-1,   NCOLDG      )
                JA = 1
            ENDIF
            IF (Xc(MP+1,NP-1) .EQ. XYMIS) THEN
                Xc(MP+1,NP-1) = 2*Xc(MP+1,NP)  - Xc(MP+1,NP+1)
                Yc(MP+1,NP-1) = 2*Yc(MP+1,NP)  - Yc(MP+1,NP+1)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC, MP+1,   NP-1,   NCOLDG      )
                JA = 1
            ENDIF
         ENDIF
         IF (JA .EQ. 1) THEN
            CALL OKAY(0)
         ELSE
            CALL OKAY(0)
         ENDIF
      ELSE IF (NPUT .EQ. -2) THEN
         Xc(MP,NP) = XYMIS
         Yc(MP,NP) = XYMIS
         IF (MP .EQ. 1 .OR. MP .EQ. MC .OR. NP .EQ. 1 .OR. NP .EQ. NC    ) THEN
            CALL ADJUST(Xc, Yc, mmax, nmax, MC, NC)
         ENDIF
      ENDIF
      RETURN
      END subroutine modgr1


      SUBROUTINE ADJUST( X, Y, mmax, nmax, MC, NC)
      USE m_missing
      implicit none
      integer :: mmax, nmax, mc, nc
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)
! TODO: Z not present, no filling with dmiss [AvD]
!     schuif data naar links en of beneden en geef nieuwe MC,NC

      integer :: i, j, ifirst, jfirst
      double precision, allocatable :: XH(:,:), YH(:,:)
      allocate(xh(MMAX,NMAX), YH(MMAX,NMAX))

      xh = x
      yh = y
      x = xymis
      y = xymis

      IFIRST = 0
      DO 10 I = 1,MC
         DO 10 J = 1,NC
            IF (XH(I,J) .NE. XYMIS .AND. IFIRST .EQ. 0) IFIRST = I
    10 CONTINUE

      JFIRST = 0
      DO 20 J = 1,NC
         DO 20 I = 1,MC
            IF (XH(I,J) .NE. XYMIS .AND. JFIRST .EQ. 0) JFIRST = J
    20 CONTINUE

      IF (IFIRST .EQ. 0 .OR. JFIRST .EQ. 0) THEN
         MC = 0
         NC = 0
      ELSE
         IFIRST = IFIRST - 1
         JFIRST = JFIRST - 1
         DO 30 I = 1,MC-IFIRST
            DO 30 J = 1,NC-JFIRST
               X(I,J) = XH(I+IFIRST,J+JFIRST)
               Y(I,J) = YH(I+IFIRST,J+JFIRST)
    30   CONTINUE
         CALL NUMS(X,mmax, nmax, MC,NC)
      ENDIF

      deallocate(xh, yh)
      RETURN
      END


      !> Operates on active grid from m_grid directly!
      SUBROUTINE SHIFXY(IS,     JS,     MP,     NP        )
      
      !     XH,     YH,     mmax, nmax, MC,     NC, IS,     JS,     MP,     NP        )
      use m_missing
      use m_grid
      use geometry_module, only: pinpok
      
      implicit none
      integer :: is, js, mp, np

      integer :: i, j
!     schuif data naar rechts of boven of beide en geef nieuwe MC,NC

      MC = MC + IS
      NC = NC + JS

      call increasegrid(mc,nc)

      MP = MP + IS
      NP = NP + JS

      DO 10 J = NC,1+JS,-1
         DO 10 I = MC,1+IS,-1
            Xc(I,J) = Xc(I-IS,J-JS)
            Yc(I,J) = Yc(I-IS,J-JS)
            Zc(I,J) = Zc(I-IS,J-JS)
    10 CONTINUE
      IF (IS .EQ. 1) THEN
         DO 20 J = 1,NC
            Xc(1,J) = XYMIS
            Yc(1,J) = XYMIS
            Zc(1,J) = XYMIS
    20   CONTINUE
      ENDIF
      IF (JS .EQ. 1) THEN
         DO 30 I = 1,MC
            Xc(I,1) = XYMIS
            Yc(I,1) = XYMIS
            Zc(I,1) = XYMIS
    30   CONTINUE
      ENDIF
      RETURN
      END subroutine shifxy


      SUBROUTINE FINDNM( XL, YL, X, Y, mmax, nmax, MC, NC, INSIDE, MV, NV, IN, JN, wf )
      use m_missing
      use geometry_module, only: pinpok
      implicit none

      integer :: mmax, nmax, mc, nc, inside, mv, nv, in, jn
      double precision :: X(MMAX,NMAX),Y(MMAX,NMAX),XX(4),YY(4),XK(3),YK(3)
      double precision :: xl, yl, wf(4)

      integer :: ishot, i, j, mz, nz, m1, m2, n1, n2, insidet, mvol, nvol, i1, i2, ier
      double precision :: dx, dy, r, rmin, xxc, yyc

      DATA MVOL /0/, NVOL /0/
      IF (MC .EQ. 0 .OR. NC .EQ. 0) RETURN
      ISHOT = 0
      RMIN  = 99d+20

     5 CONTINUE
      MV = 0
      NV = 0
      IF (MVOL .NE. 0) THEN
         MZ = MVOL
         NZ = NVOL
      ELSE
         DO 10 I = 1,MC
            DO 10 J = 1,NC
               IF (X(I,J) .NE. XYMIS) THEN
                  DX = XL - X(I,J)
                  DY = YL - Y(I,J)
                  R  = DX*DX + DY*DY
                  IF (R .LT. RMIN) THEN
                     RMIN = R
                     MZ   = I
                     NZ   = J
                  ENDIF
               ENDIF
    10   CONTINUE
      ENDIF

      M1     = MAX(1,MZ-2)
      N1     = MAX(1,NZ-2)
      M2     = MIN(MC-1 ,MZ+1)
      N2     = MIN(NC-1 ,NZ+1)
      INSIDE = 0
      MVOL   = 0
      NVOL   = 0
      DO 20 I = M1,M2
         DO 20 J = N1,N2
            XX(1) = X(I,J)
            XX(2) = X(I+1,J)
            XX(3) = X(I+1,J+1)
            XX(4) = X(I,J+1)
            YY(1) = Y(I,J)
            YY(2) = Y(I+1,J)
            YY(3) = Y(I+1,J+1)
            YY(4) = Y(I,J+1)
            IF (XX(1) .NE. XYMIS .AND. XX(2) .NE. XYMIS .AND.   &
                XX(3) .NE. XYMIS .AND. XX(4) .NE. XYMIS) THEN
               CALL PINPOK(XL, YL, 4, XX, YY, INSIDE, jins, dmiss)
               IF (INSIDE .EQ. 1) THEN
                  
                  call bilin5( xx, yy, xL, yL ,wf , ier)
               
                  MVOL = I
                  NVOL = J
                  MV   = I
                  NV   = J
!                 Bepaal kwadrant
                  XXC  = ( XX(1) + XX(2) + XX(3) + XX(4) )/4
                  YYC  = ( YY(1) + YY(2) + YY(3) + YY(4) )/4
                  IN   = 0
                  JN   = 0
                  DO 30 I1 = 1,4
                     I2    = MOD(I1,4) + 1
                     XK(1) = XX(I1)
                     YK(1) = YY(I1)
                     XK(2) = XX(I2)
                     YK(2) = YY(I2)
                     XK(3) = XXC
                     YK(3) = YYC
                     CALL PINPOK(XL, YL, 3, XK, YK, INSIDET, jins, dmiss)
                     IF (INSIDET .EQ. 1) THEN
                        IF ( I1 .EQ. 1) JN = -1
                        IF ( I1 .EQ. 2) IN =  1
                        IF ( I1 .EQ. 3) JN =  1
                        IF ( I1 .EQ. 4) IN = -1
                        RETURN
                     ELSE IF (I1 .EQ. 4) THEN
!                       WRITE(MDIA,*) 'NO KWADRANT'
                        RETURN
                     ENDIF
    30            CONTINUE
               ENDIF
            ENDIF
    20 CONTINUE

!     WRITE(MDIA,*) 'ISHOT', ISHOT, MVOL, NVOL
      IF (ISHOT .EQ. 1) RETURN
      ISHOT = 1
      GOTO 5

      RETURN
      END subroutine findnm


      SUBROUTINE NULFIELD(X,Y, mmax, nmax)
      use m_missing
      implicit none
      integer :: mmax, nmax
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)
      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: i, j

      DO 10 I = MB(3),MB(4)
         DO 10 J = NB(3),NB(4)
            X(I,J) = XYMIS
            Y(I,J) = 0d0
    10 CONTINUE
      RETURN
      END subroutine nulfield


      SUBROUTINE CUTFIELD(X,Y,mmax, nmax, MC,NC)
      use m_missing
      implicit none
      integer :: mmax, nmax, mc, nc
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)
      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: i, j

      DO 10 I = 1,MC
         DO 10 J = 1,NC
            IF (I .GE. MB(3) .AND. I .LE. MB(4) .AND. J .GE. NB(3) .AND. J .LE. NB(4) )THEN
!               mooi houwen zo
            ELSE
               X(I,J) = XYMIS
               Y(I,J) = 0d0
            ENDIF
    10 CONTINUE
      RETURN
      END subroutine cutfield


      SUBROUTINE MODGR2(     XH,     YH,      X,      Y, mmax, nmax,  MC,     NC,   NUMP)
      implicit none
      integer :: mmax, nmax, mc, nc, nump
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX), XH(MMAX,NMAX), YH(MMAX,NMAX)


      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      double precision :: EPS, dx1, dy1, dx2, dy2, fac, efac
      DATA EPS /0.00001d0/
      integer :: m1, m2, n1, n2, in, jn, i1, j1, klast, num, i, j, i2, j2, ii, jj
!     LINESHIFT

      M1 = MB(1)
      N1 = NB(1)
      M2 = MB(2)
      N2 = NB(2)

      KLAST = 1
      NUM   = 0
      IN    = MIN(1,M2-M1)
      JN    = MIN(1,N2-N1)
      I1    = M1
      J1    = N1
      DX1   = XH(I1,J1) - X(I1,J1)
      DY1   = YH(I1,J1) - Y(I1,J1)
      DO 10 I = M1+IN,M2
         DO 10 J = N1+JN,N2
            IF (ABS(XH(I,J)-X(I,J)) .GT. EPS .OR.     &
                ABS(YH(I,J)-Y(I,J)) .GT. EPS .OR.     &
                I .EQ. M2 .AND. J .EQ. N2        ) THEN
                I2  = I
                J2  = J
                DX2 = XH(I,J) - X(I,J)
                DY2 = YH(I,J) - Y(I,J)
                IF (I .EQ. M2 .AND. J .EQ. N2) KLAST = 0
                DO 20 II = I1,I2-IN*KLAST
                   DO 20 JJ = J1,J2-JN*KLAST
                      IF (IN .EQ. 1) THEN
                         FAC    = dble(II-I1) / dble(I2-I1)
                      ELSE
                         FAC    = dble(JJ-J1) / dble(J2-J1)
                      ENDIF
                      EFAC      = 1 - FAC
                      XH(II,JJ) = X(II,JJ) + EFAC*DX1 + FAC*DX2
                      YH(II,JJ) = Y(II,JJ) + EFAC*DY1 + FAC*DY2
                      CALL MODFLD(     XH,     YH,      X,      Y,   mmax, nmax,  &
                                       MC,     NC,     II,     JJ,    &
                                     NUMP,      1,     JN,     IN)
    20          CONTINUE
                I1  = I2
                J1  = J2
                DX1 = DX2
                DY1 = DY2
            ENDIF
    10 CONTINUE
      RETURN
      END subroutine modgr2


      SUBROUTINE ATTRACTREPULSE(     XH,     YH,      X,      Y,   mmax, nmax, MC,     NC,   NUMP,     JA)
      use m_missing
      use m_gridsettings
      use m_sferic
      use m_wearelt
      use geometry_module, only: dbdistance
      implicit none
      integer :: mmax, nmax, mc, nc, nump, ja
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX), XH(MMAX,NMAX), YH(MMAX,NMAX)


      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE
!     ATTRACTIE, REPULSIE

      integer :: M1, N1, M2, N2, IN, JN, I, J, II, JJ, ii1, ii2, jj1, jj2, JANU, numpi, numpj
      double precision :: rsx, teken, dx, dy, dxy, dxy0, x0, y0, xn, yn, rn, fr

      M1    = MB(1)
      N1    = NB(1)
      M2    = MB(2)
      N2    = NB(2)
!     IN    = MIN(1,M2-M1)
!     JN    = MIN(1,N2-N1)
      JN    = MIN(1,M2-M1)
      IN    = MIN(1,N2-N1)
      NUMPI = IN*NUMP
      NUMPJ = JN*NUMP
!     RSX   = DSIX
      RSX = dbDISTANCE(X1,Y1,X2,Y2, jsferic, jasfer3D, dmiss)
      RSX = RSX/6
      JANU  = JA
      DO 10 I = M1,M2
         DO 10 J = N1,N2
            X0 = X(I,J)
            Y0 = Y(I,J)
            IF (X0 .NE. XYMIS) THEN
               IF (NPT .LE. 2) THEN
                  II1 = MAX(1,I-NUMPI)
                  II2 = MIN(I+NUMPI,MC)
                  JJ1 = MAX(1,J-NUMPJ)
                  JJ2 = MIN(J+NUMPJ,NC)
               ELSE
                  II1 = MAX(MB(3),I-NUMPI*1000)
                  II2 = MIN(I+NUMPI*1000,MB(4))
                  JJ1 = MAX(NB(3),J-NUMPJ*1000)
                  JJ2 = MIN(J+NUMPJ*1000,NB(4))
               ENDIF
               DO 20 II = II1,II2
                  DO 20 JJ = JJ1,JJ2
                     XN = X(II,JJ)
                     IF (XN .NE. XYMIS .AND. .NOT. (II .EQ. I .AND. JJ .EQ. J) ) THEN
                        YN = Y(II,JJ)
                        IF (NPT .LE. 2) THEN
                           RN = dbDISTANCE(XN,YN,X0,Y0, jsferic, jasfer3D, dmiss)
!                          RN = SQRT( (XN - X0)**2 + (YN - Y0)**2 )
                           IF (RN .LT. RSX) THEN
                              FR = (RSX - RN)/RSX
                              IF (IN .EQ. 1) THEN
                                 TEKEN = dble(SIGN(1,II - I))
                              ELSE IF (JN .EQ. 1) THEN
                                 TEKEN = dble(SIGN(1,JJ - J))
                              ENDIF
                              CALL DXYB(      X,      Y,     mmax, nmax, MC,         &
                                             NC,     II,     JJ,     IN, &
                                             JN,   DXY0                )
                              DXY = RFAC*TEKEN*FR*JANU*DXY0
                              IF (JSFERIC .EQ. 1) DXY = RD2DG*DXY/RA
                              DX  = DXY*IN
                              DY  = DXY*JN
                              CALL TOLOCL(   DX,     DY,      X,      Y, mmax, nmax, &
                                                     II,     JJ,      1)
                              XH(II,JJ) = XN + DX
                              YH(II,JJ) = YN + DY
                           ENDIF
                        ELSE
                           CALL SMEERFUNCTIE(II,JJ,I,J,FR,IN,JN)
                           IF (IN .EQ. 1) THEN
                              TEKEN = dble(SIGN(1,II - I))
                           ELSE IF (JN .EQ. 1) THEN
                              TEKEN = dble(SIGN(1,JJ - J))
                           ENDIF
                           CALL DXYB(      X,      Y,     mmax, nmax, MC,            &
                                          NC,     II,     JJ,     JN,    &
                                          IN,   DXY0                )
                           DXY = RFAC*TEKEN*FR*JANU*DXY0
                           IF (JSFERIC .EQ. 1) DXY = RD2DG*DXY/RA
                           DX  = DXY*IN
                           DY  = DXY*JN
                           CALL TOLOCL(   DX,     DY,      X,      Y,    mmax, nmax, &
                                                  II,     JJ,      1)
                           XH(II,JJ) = XN + DX
                           YH(II,JJ) = YN + DY
                        ENDIF
                     ENDIF
    20         CONTINUE
            ENDIF
    10 CONTINUE
      RETURN
      END subroutine attractrepulse


      SUBROUTINE MODGR4(NUMP,LANDORSPLINE)
      use m_grid
      use m_landboundary
      USE M_SPLINES, only : mcs, splnump=>nump
      implicit none
      integer :: nump, landorspline

      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: m1, m2, n1, n2, i, j, in, jn, ncs, jdum
      double precision :: EPS, X0, Y0, XN, YN, DIS, RL
!     TO LAND
      DATA EPS /0.00001d0/
      IF (LANDORSPLINE .EQ. 1) THEN
         IF (MXLAN .EQ. 0) THEN
            CALL QNERROR('FIRST LOAD A LANDBOUNDARY',' ',' ')
            RETURN
         ENDIF
      ELSE
         call splnump(1, ncs)
         IF (MCS .LT. 1 .OR. NCS .LT. 2) THEN
            CALL QNERROR('FIRST DRAW SPLINE NR 1',' ',' ')
            RETURN
         ENDIF
      ENDIF
      M1    = MB(1)
      N1    = NB(1)
      M2    = MB(2)
      N2    = NB(2)
      IN    = MIN(1,N2-N1)
      JN    = MIN(1,M2-M1)
      DO 10 I = M1,M2
         DO 10 J = N1,N2
            X0 = Xch(I,J)
            Y0 = Ych(I,J)
            IF (LANDORSPLINE .EQ. 1) THEN
               CALL TOLAND(X0, Y0, 1, MXLAN, 1, XN, YN, DIS, JDUM, RL)
            ELSE
               CALL TOSPLINE(X0, Y0, XN, YN)
            ENDIF
            Xc(I,J) = XN
            Yc(I,J) = YN
            IF (ABS(Xch(I,J)-Xc(I,J)) .GT. EPS .OR.                      &
                ABS(Ych(I,J)-Yc(I,J)) .GT. EPS    ) THEN
                CALL MODFLD(     Xc,    Yc,     Xch,    Ych, mmax, nmax,           &
                                 MC,     NC,      I,      J,           &
                               NUMP,      1,     IN,     JN)
            ENDIF
    10 CONTINUE
      RETURN
       END subroutine modgr4


       SUBROUTINE TOSPLINE(XX, YY, XV, YV)
       USE M_SPLINES
       implicit none

       double precision :: XX, YY, XV, YV

       double precision :: XI(maxsplen), XI2(maxsplen), YI(maxsplen), YI2(maxsplen)
       double precision :: TV, DIS
       integer :: IN, NUMPI

       IN = 1 ! Pick first spline
       CALL NUMP(IN, NUMPI)
       TV = NUMPI/2d0
       CALL GETIJ (XSP, XI, maxspl, maxsplen, maxsplen, IN, IN,  1, NUMPI)
       CALL GETIJ (YSP, YI, maxspl, maxsplen, maxsplen, IN, IN,  1, NUMPI)
       CALL SPLINE(XI,NUMPI,XI2)
       CALL SPLINE(YI,NUMPI,YI2)
       CALL DISMIN(XI,XI2,YI,YI2,XX,YY,NUMPI, DIS,TV,XV,YV)
       RETURN
       END subroutine tospline


      SUBROUTINE  DXYB(      X,      Y,     mmax, nmax, MC,            &
                            NC,     II,     JJ,     IN,                &
                            JN,   DXY0                )
      use m_missing
      use geometry_module, only: dbdistance
      use m_sferic, only: jsferic, jasfer3D
      
      implicit none
      integer :: mmax, nmax, mc, nc, ii, jj, in, jn
      double precision :: dxy0
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)

      integer :: num
      double precision :: XU, YU, XD, YD, dxy1
      NUM  = 0
      DXY0 = 0

      IF (II+IN .LE. MC .AND. JJ+JN .LE. NC) THEN
         XU = X(II+IN,JJ+JN)
         IF (XU .NE. XYMIS) THEN
            YU   = Y(II+IN,JJ+JN)
            dxy0 = dbdistance(X(II,JJ),Y(II,JJ),XU,YU,jsferic, jasfer3D, dmiss)
            NUM  = NUM + 1
         ENDIF
      ENDIF

      IF (II-IN .GE. 1 .AND. JJ-JN .GE. 1) THEN
         XD = X(II-IN,JJ-JN)
         IF (XD .NE. XYMIS) THEN
            YD   = Y(II-IN,JJ-JN)
            dxy1 = dbdistance(X(II,JJ),Y(II,JJ),XD,YD,jsferic, jasfer3D, dmiss)
            NUM  = NUM + 1
            DXY0 = (DXY0 + DXY1) / dble(NUM)
         ENDIF
      ENDIF

      RETURN
      END subroutine dxyb

      SUBROUTINE CLOSPT(    X,      Y,     mmax, nmax, MC,     NC, &
                           XL,     YL,     MV,     NV)
      use m_missing
      implicit none

      integer :: mmax, nmax, mc, nc, mv, nv
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)
      double precision :: xl, yl

      double precision :: rmin, r
      integer :: i, j
      RMIN  = 1d+20

      DO 10 I = 1,MC
         DO 10 J = 1,NC
            IF (X(I,J) .NE. XYMIS) THEN
               R = ABS(XL - X(I,J) ) + ABS(YL - Y(I,J) )
               IF (R .LT. RMIN) THEN
                  RMIN = R
                  MV   = I
                  NV   = J
               ENDIF
            ENDIF
   10 CONTINUE
      RETURN
      END subroutine clospt


      SUBROUTINE LOCALREFINE(num, m1, n1, m2, n2, NOPTION)
      implicit none
      integer :: num, m1, m2, n1, n2, NOPTION

      if (NOPTION == 1) then
          CALL REFINE(M1,N1,M2,N2,NUM)
      else if (NOPTION == 2) then
          CALL DEREFINE(M1,N1,M2,N2,NUM)
      end if
      END subroutine localrefine

!***************7***  INTERPOLATION ************************************
      SUBROUTINE DEREFINE(M1, N1, M2, N2,NUM)
      use m_grid
      use m_gridsettings
      use unstruc_messages
      use m_missing
      implicit none

      integer :: m1, n1, m2, n2, num

      integer :: I, J, IR,INOW,JR,JNOW,MFA,NFA,MFAA,NFAA,MD,ND
      double precision, allocatable :: XR(:,:), YR(:,:)
      allocate(xr(mmax, nmax), yr(mmax, nmax))

      IF (MFAC .GE. MC) THEN
         CALL QNERROR('M-refinement factor larger than grid M-dimension' ,' ',' ')
         NUM = 0
         RETURN
      ENDIF
      IF (NFAC .GE. NC) THEN
         CALL QNERROR('N-refinement factor larger than grid M-dimension' ,' ',' ')
         NUM = 0
         RETURN
      ENDIF
      CALL SAVEgrd()

      call mess(LEVEL_DEBUG, 'DEREFINE BY: ', MFAC, NFAC)
      CALL READYY('DEREFINE',0d0)

      XR = dmiss
      YR = dmiss

      MD   = M2 - M1
      ND   = N2 - N1
      MFAA = MFAC
      NFAA = NFAC
      IF (MD .EQ. 0) MFAA = 1
      IF (ND .EQ. 0) NFAA = 1

      IR   = 1
      INOW = 1
      DO 10 I = 1,MC
         IF (INOW .GE. M1 .AND. INOW .LT. M2) THEN
            MFA = MFAA
         ELSE
            MFA = 1
         ENDIF
         JR   = 1
         JNOW = 1
         IF (INOW .LE. MC) THEN
            DO 20 J = 1,NC
               IF (JNOW .GE. N1 .AND. JNOW .LT. N2) THEN
                  NFA = NFAA
               ELSE
                  NFA = 1
               ENDIF
               IF (JNOW .LE. NC) THEN
                  XR(IR,JR) = Xc(INOW,JNOW)
                  YR(IR,JR) = Yc(INOW,JNOW)
                  JR   = JR + 1
                  JNOW = JNOW + NFA
               ENDIF
    20      CONTINUE
            IR   = IR + 1
            INOW = INOW + MFA
         ENDIF
    10 CONTINUE

      CALL PUTARR(XR,Xc,MMAX,NMAX)
      CALL PUTARR(YR,Yc,MMAX,NMAX)
      CALL NUMS(Xc,mmax, nmax, MC,NC)
!     MC = INOW
!     NC = JNOW

      CALL READYY('DEREFINE',1d0)
      CALL READYY('DEREFINE',-1d0)
      deallocate(XR, YR)
      RETURN
      END subroutine derefine


      SUBROUTINE REFINE(M1, N1, M2, N2, NUM)
      use m_grid ! Use m_grid directly, because isitu does this too (otherwise shadowing of ).
      USE m_gridsettings
      use unstruc_messages
      implicit none
      integer :: m1, n1, m2, n2, num

      double precision, allocatable :: XI2(:,:),XJ2(:,:),YI2(:,:),YJ2(:,:), XR(:,:), YR(:,:), XRH(:,:), YRH(:,:)

      integer :: NRM, NRN, MCR, NCR
      character*4 TEX

      call mess(LEVEL_DEBUG, 'INTERPOLATION')
      call mess(LEVEL_DEBUG, 'DIMENSIONS OF GRID : ', MC,NC)

      IF (MC .EQ. 0) THEN
         CALL QNERROR('First Create or Load a Grid',' ',' ')
         NUM = 0
         RETURN
      ENDIF

      NRM = M2-M1
      NRN = N2-N1
      MCR = MC - NRM + 1 + NRM*MFAC - 1
      NCR = NC - NRN + 1 + NRN*NFAC - 1

      CALL SAVEgrd()
      
      call increasegrid(mcr,ncr)

      allocate(xi2(mmax, nmax), xj2(mmax, nmax), yi2(mmax, nmax), yj2(mmax, nmax), &
               xr(mmax, nmax), yr(mmax, nmax), xrh(mmax, nmax), yrh(mmax, nmax))

      CALL READYY('INTERPOLATION',0d0)
      CALL ISITU ()!      X,      Y,     MC,  NC,    IJC,  IJYES)
      CALL READYY(' ',0.10d0)

      CALL GETSPL2(     Xc,    XI2,    XJ2,     MC,     NC, mmax, nmax)
      CALL READYY(' ',0.15d0)

      CALL GETSPL2(     Yc,    YI2,    YJ2,     MC,     NC, mmax, nmax)
      CALL READYY(' ',0.20d0)

      IF (MFAC .NE. 1 .OR. NFAC .NE. 1) THEN
         CALL XYSPLN(      Xc,      Yc,     XR,     YR,           &
                         XI2,    YI2,    XJ2,    YJ2, XRH, YRH, &
                        mmax,   nmax,   mnmax, &
                          M1,     N1,     M2,     N2,MC,NC,     &
                        MFAC,   NFAC,   IJYES)
         CALL READYY(' ',0.90d0)
      ENDIF

      CALL PUTARR(XR,Xc,MMAX,NMAX)
      CALL PUTARR(YR,Yc,MMAX,NMAX)

      MC = MCR
      NC = NCR

      CALL READYY(' ',1d0)
      CALL READYY(' ',-1d0)
      deallocate(XI2, XJ2, YI2, YJ2, XR, YR, XRH, YRH)

      RETURN
      END subroutine refine


      SUBROUTINE XYSPLN(      X,      Y,     XR,     YR,              &
                            XI2,    YI2,    XJ2,    YJ2,  XRH,  YRH,  &
                            mmax, nmax, imax, &
                             M1,     N1,     M2,     N2, MC, NC,      &
                           MFAC,   NFAC,   IJYES)
      use m_missing
      implicit none
!     SPLINE INTERPOLATIE BINNEN ALLE GROVE CELLEN
!     LIJN 1,2 ZIJN DE VERTICALE   CELWANDEN
!     LIJN 3,4 ZIJN DE HORIZONTALE CELWANDEN
      integer :: mmax, nmax, imax, m1, n1, m2, n2, mc, nc, mfac, nfac
      double precision :: X(MMAX,NMAX), XR(MMAX,NMAX),               &
              Y(MMAX,NMAX), YR(MMAX,NMAX),                           &
            XI2(MMAX,NMAX),XJ2(MMAX,NMAX),                           &
            YI2(MMAX,NMAX),YJ2(MMAX,NMAX),                           &
                XH1(IMAX),   XH21(IMAX),                             &
                XH2(IMAX),   XH22(IMAX),                             &
                XH3(IMAX),   XH23(IMAX),                             &
                XH4(IMAX),   XH24(IMAX),                             &
                YH1(IMAX),   YH21(IMAX),                             &
                YH2(IMAX),   YH22(IMAX),                             &
                YH3(IMAX),   YH23(IMAX),                             &
                YH4(IMAX),   YH24(IMAX),                             &
                 X1 (IMAX),      Y1(IMAX),                           &
                 X2 (IMAX),      Y2(IMAX),                           &
                 X3 (IMAX),      Y3(IMAX),                           &
                 X4 (IMAX),      Y4(IMAX),                           &
            XRH(MMAX,NMAX), YRH(MMAX,NMAX)
      INTEGER IJYES(MMAX,NMAX)

      double precision :: af, TI, TJ
      integer :: md, nd, mfa, nfa, mfaa, nfaa, ki1, i1, i2, j1, j2, &
                 KI, LJ, LJ1, K, L, dum
      XR = dmiss
      YR = dmiss

      MD   = M2 - M1
      ND   = N2 - N1
      MFAA = MFAC
      NFAA = NFAC
      IF (MD .EQ. 0) MFAA = 1
      IF (ND .EQ. 0) NFAA = 1

      KI1 = 0
      DO 40 I1 = 1,MC-1
         AF = 0.20d0 + 0.70d0*dble(I1-1)/(MC-1)
         CALL READYY(' ',AF)
         IF (I1 .GE. M1 .AND. I1 .LT. M2) THEN
            MFA = MFAA
         ELSE
            MFA = 1
         ENDIF
         I2    = I1 + 1
         CALL GETIJ(X,   XH1,mmax, nmax, imax, I1,I1,1,NC)
         CALL GETIJ(XI2,XH21,mmax, nmax, imax, I1,I1,1,NC)
         CALL GETIJ(X,   XH2,mmax, nmax, imax, I2,I2,1,NC)
         CALL GETIJ(XI2,XH22,mmax, nmax, imax, I2,I2,1,NC)
         CALL GETIJ(Y,   YH1,mmax, nmax, imax, I1,I1,1,NC)
         CALL GETIJ(YI2,YH21,mmax, nmax, imax, I1,I1,1,NC)
         CALL GETIJ(Y,   YH2,mmax, nmax, imax, I2,I2,1,NC)
         CALL GETIJ(YI2,YH22,mmax, nmax, imax, I2,I2,1,NC)
         LJ1 = 0
         DO 50 J1 = 1,NC-1
            IF (J1 .GE. N1 .AND. J1 .LT. N2) THEN
               NFA = NFAA
            ELSE
               NFA = 1
            ENDIF
            J2 = J1 + 1
            CALL GETIJ(X,   XH3,mmax, nmax, imax, 1,MC,J1,J1)
            CALL GETIJ(XJ2,XH23,mmax, nmax, imax, 1,MC,J1,J1)
            CALL GETIJ(X,   XH4,mmax, nmax, imax, 1,MC,J2,J2)
            CALL GETIJ(XJ2,XH24,mmax, nmax, imax, 1,MC,J2,J2)
            CALL GETIJ(Y,   YH3,mmax, nmax, imax, 1,MC,J1,J1)
            CALL GETIJ(YJ2,YH23,mmax, nmax, imax, 1,MC,J1,J1)
            CALL GETIJ(Y,   YH4,mmax, nmax, imax, 1,MC,J2,J2)
            CALL GETIJ(YJ2,YH24,mmax, nmax, imax, 1,MC,J2,J2)
            IF (IJYES(I1,J1) .EQ. 1) THEN

               DO 60 K = 1,MFA+1
                  TI  = (I1-1) + dble(K-1)/dble(MFA)
                  CALL SPLINT(XH3,XH23,MC,TI,X3(K))
                  CALL SPLINT(XH4,XH24,MC,TI,X4(K))
                  CALL SPLINT(YH3,YH23,MC,TI,Y3(K))
                  CALL SPLINT(YH4,YH24,MC,TI,Y4(K))
    60         CONTINUE
               DO 70 L = 1,NFA+1
                  TJ  = (J1-1) + dble(L-1)/dble(NFA)
                  CALL SPLINT(XH1,XH21,NC,TJ,X1(L))
                  CALL SPLINT(XH2,XH22,NC,TJ,X2(L))
                  CALL SPLINT(YH1,YH21,NC,TJ,Y1(L))
                  CALL SPLINT(YH2,YH22,NC,TJ,Y2(L))
!                 als je equidistant wil interpoleren
                  IF (J1 .EQ. -1) THEN
                     CALL EQDINT(XH1,imax,TJ,X1(L))
                     CALL EQDINT(XH2,imax,TJ,X2(L))
                     CALL EQDINT(YH1,imax,TJ,Y1(L))
                     CALL EQDINT(YH2,imax,TJ,Y2(L))
                  ENDIF
    70         CONTINUE
               IF (X1(1) .EQ. 0) THEN
                  DUM = 0
               ENDIF
               CALL TRANFN(     X1,     X2,     X3,     X4,      &
                                Y1,     Y2,     Y3,     Y4,      &
                              mmax,   nmax,   imax,              &
                              MFA ,   NFA ,    XRH,    YRH)
               DO 80 K = 1,MFA+1
                  DO 80 L = 1,NFA+1
                     KI  = KI1 + K
                     LJ  = LJ1 + L
                     XR(KI,LJ) = XRH(K,L)
                     YR(KI,LJ) = YRH(K,L)
    80         CONTINUE
            ENDIF
            LJ1 = LJ1 + NFA
    50   CONTINUE
         KI1 = KI1 + MFA
    40 CONTINUE
      RETURN
      END subroutine XYSPLN


      SUBROUTINE EQDINT(YH2,imax,TJ,Y2)
      implicit none
      integer :: imax
      double precision :: YH2(imax)
      double precision :: TJ, Y2
      integer :: j1, j2
      double precision :: T1, T2
      J1 = INT(TJ) + 1
      J2 = J1 + 1
      T1 = TJ - INT(TJ)
      T2 = 1 - T1
      Y2 = T2*YH2(J1) + T1*YH2(J2)
      RETURN
      END subroutine EQDINT

      SUBROUTINE TRANFN(     X1,     X2,     X3,     X4,        &
                             Y1,     Y2,     Y3,     Y4,        &
                           mmax, nmax, imax, &
                           MFAC,   NFAC,    XRH,    YRH)
      use m_missing
      implicit none
      integer :: mmax, nmax, imax, mfac, nfac
      double precision :: X1(IMAX), X2(IMAX), X3(IMAX), X4(IMAX), XRH(MMAX,NMAX),  &
           Y1(IMAX), Y2(IMAX), Y3(IMAX), Y4(IMAX), YRH(MMAX,NMAX),  &
           B1R(IMAX), B2R(IMAX), A1R(IMAX), A2R(IMAX)

      integer :: I, J
      double precision :: A1, A2, B1, B2, D, DX, DY, AIJ, BIJ, EX, EY, XA, YA, XB, YB, DEXY
!     1,2,B VERTICALEN, 3,4,A HORIZONTALEN

      CALL ABREL(X1,Y1,B1R,NFAC)
      CALL ABREL(X2,Y2,B2R,NFAC)
      CALL ABREL(X3,Y3,A1R,MFAC)
      CALL ABREL(X4,Y4,A2R,MFAC)

!     Dit is modified transfinite
      DO 10 I = 2,MFAC
         DO 10 J = 2,NFAC
            B1  = B1R(J)
            B2  = B2R(J)
            A1  = A1R(I)
            A2  = A2R(I)
            D    = 1 - (A2 - A1)*(B2 - B1)
            AIJ  = ( (1 - B1)*A1 + B1*A2 ) / D
            BIJ  = ( (1 - A1)*B1 + A1*B2 ) / D

            DX   = X2(J) - X1(J)
            DY   = Y2(J) - Y1(J)
            EX   = X4(I) - X3(I)
            EY   = Y4(I) - Y3(I)

            XA   = X1(J) + AIJ*DX
            YA   = Y1(J) + AIJ*DY
            XB   = X3(I) + BIJ*EX
            YB   = Y3(I) + BIJ*EY

            DEXY = DX*EY - EX*DY
            IF (DEXY .EQ. 0) THEN
               XRH(I,J) = XYMIS
               YRH(I,J) = XYMIS
            ELSE
               XRH(I,J) = ( (XA*DX+YA*DY)*EY - (XB*EX+YB*EY)*DY ) / DEXY
               YRH(I,J) = ( (XB*EX+YB*EY)*DX - (XA*DX+YA*DY)*EX ) / DEXY
            ENDIF
    10 CONTINUE

!     Dit is gewoon transfinite
!     X00 = X1(1)
!     X10 = X2(1)
!     X01 = X4(1)
!     X11 = X2(NFAC+1)
!
!     Y00 = Y1(1)
!     Y10 = Y2(1)
!     Y01 = Y4(1)
!     Y11 = Y2(NFAC+1)
!
!     D00 = SQRT( (X00 - X01)**2 + (Y00 - Y01)**2 )
!     D11 = SQRT( (X10 - X11)**2 + (Y10 - Y11)**2 )
!     D10 = SQRT( (X00 - X10)**2 + (Y00 - Y10)**2 )
!     D01 = SQRT( (X00 - X01)**2 + (Y00 - Y01)**2 )
!     DO 11 I = 2,MFAC
!        DO 11 J = 2,NFAC
!           B1   = B1R(J)
!           B2   = B2R(J)
!           A1   = A1R(I)
!           A2   = A2R(I)
!           RI   = REAL(I-1)/REAL(MFAC)
!           RJ   = REAL(J-1)/REAL(NFAC)
!
!           S    = (1-RJ)*A1 + RJ*A2
!           T    = (1-RI)*B1 + RI*B2
!           SM   = 1 - S
!           TM   = 1 - T
!
!           D12  = SQRT( (X1(J)-X2(J))**2 + (Y1(J)-Y2(J))**2 )
!           D34  = SQRT( (X3(J)-X4(J))**2 + (Y3(J)-Y4(J))**2 )
!
!           CALL ORTPRO(      J,      S,     X1,     Y1,
!                           D12,    X1P,    Y1P,      1,      0)
!           CALL ORTPRO(      J,     SM,     X2,     Y2,
!                           D12,    X2P,    Y2P,     -1,      0)
!
!           CALL ORTPRO(      I,      T,     X3,     Y3,
!                           D34,    X3P,    Y3P,      1,      0)
!           CALL ORTPRO(      I,     TM,     X4,     Y4,
!                           D34,    X4P,    Y4P,     -1,      0)
!
!           CALL ORTPRO(      1,      T,     X2,     Y2,
!                           D00,   X00P,   Y00P,      1,      1)
!           CALL ORTPRO(   MFAC,      T,     X2,     Y2,
!                           D00,   X00P,   Y00P,      1,      1)
!           XRH(I,J) =  (SM*X1P + S*X2P + TM*X3P + T*X4P) -
!                       (SM*TM*X00P + SM*T*X01P + S*T* X11P + S*TM*X10P)
!           YRH(I,J) =  (SM*Y1P + S*Y2P + TM*Y3P + T*Y4P) -
!                       (SM*TM*X00P + SM*T*X01P + S*T* X11P + S*TM*X10P)
!
!           XRH(I,J) =  SM*X1(J) + S*X2(J) + TM*X3(I) + T*X4(I) -
!                      (SM*TM*X00 + S*T*X11 + SM*T*X01 + S*TM*X10)
!           YRH(I,J) =  SM*Y1(J) + S*Y2(J) + TM*Y3(I) + T*Y4(I) -
!                      (SM*TM*Y00 + S*T*Y11 + SM*T*Y01 + S*TM*Y10)
!   11CONTINUE

!     vul randen in
      DO 20 I = 1,MFAC+1
         XRH(I,1)      = X3(I)
         XRH(I,NFAC+1) = X4(I)
         YRH(I,1)      = Y3(I)
         YRH(I,NFAC+1) = Y4(I)
    20 CONTINUE

      DO 30 J = 1,NFAC+1
         XRH(1,J)      = X1(J)
         XRH(MFAC+1,J) = X2(J)
         YRH(1,J)      = Y1(J)
         YRH(MFAC+1,J) = Y2(J)
    30 CONTINUE

      RETURN
      END subroutine TRANFN

      SUBROUTINE ABREL(X1,Y1,B1R,NFAC)
      implicit none
      integer :: nfac
      double precision :: X1(NFAC+1), Y1(NFAC+1), B1R(NFAC+1)
      integer :: J
      double precision :: B1
      B1 = 0
      DO 10 J = 2,NFAC+1
         B1     = B1 + SQRT( (X1(J)-X1(J-1))**2 + (Y1(J)-Y1(J-1))**2 )
         B1R(J) = B1
    10 CONTINUE

      DO 20 J = 2,NFAC+1
         B1R(J) = B1R(J)/B1R(NFAC+1)
    20 CONTINUE
      RETURN
      END subroutine abrel

      subroutine getarcinfolevel( x, y, zzz)
 use m_arcinfo
 implicit none
 double precision :: x,y,zzz
 ! locals
 integer          :: m,n

 if (mca .eq. 0) return
 m = 2 + (x - x0) / dxa
 n = 2 + (y - y0) / dxa
 zzz = d(m,n)
 end subroutine getarcinfolevel


      SUBROUTINE REAdarcinfo(Marc, ja)
      USE M_SFERIC
      use m_netw
      USE m_grid
      USE M_ARCINFO
      USE M_MISSING
      implicit none

      INTEGER            :: Marc, JA, in
      CHARACTER NAME2*76, TEX*3, REC*132, REC1*132


      logical jawel

      integer :: i, j, k, k0, l0
      integer :: jadoorladen, merr
      double precision XX(8), YY(8), ZZ(8)
      double precision :: af

      JSFERIC  = 0
      JSFERTEK = 0

      MERR  = 0
      MC    = 0

      CALL READYY('Reading arcinfo',0d0)

      CALL READYY('Reading arcinfo',1d0)


      call reaarc (marc,1)
      CALL DOCLOSE(marc)

      mc = mca ; nc = nca

      CALL INCREASEGRID(MC,NC)

      XC = DMISS
      YC = DMISS

      do i = 1,mc
         do j = 1,nc
            if (d(I,J) .ne. dmiss) then
               xc(i,j) =  x0 + dxa*(i-1)
               yc(i,j) =  y0 + dxa*(j-1)
               zc(i,j) =  d(i,j)
            endif
         enddo
      enddo

      if (allocated(d) ) deallocate(d)

!     disable grid outside selecting polygon
!      if ( NPL.gt.0 ) then
!         in = -1
!         do j=1,nc
!            do i=1,mc
!               call dbpinpol(xc(i,j),yc(i,j),in)
!               if ( in.ne.1 ) then
!                  xc(i,j) = DMISS
!                  yc(i,j) = DMISS
!               end if
!            end do
!         end do
!      end if

!      call gridtonet()
!
!      if (allocated(xc) ) then
!         deallocate(xc,yc,zc) ; mc = 0
!      endif


      CALL READYY(' ',-1d0)

      JA = 1

      END SUBROUTINE REAdarcinfo


      SUBROUTINE REAdarcsets(mlist)
      use m_netw
      USE M_ARCINFO
      USE M_MISSING

      implicit none

      INTEGER            :: Mlist
      INTEGER            :: Marc
      CHARACTER REC*132

      logical jawel

      integer :: i1, i2, j1,j2, k, L, Lp, numfil
      double precision :: af, f11, f21, f12, f22, dii, djj

      numfil = 0

      CALL READYY('Reading arcinfosets',0d0)

10    read(mlist,'(a)',end = 888) rec
      Lp = index('.',rec)
      L  = index(' ',rec(Lp:) ) - 1
      inquire (file = trim(rec(1:L)), exist = jawel)

      if (jawel) then
         numfil = numfil + 1
         goto 10
      endif

888   rewind (mlist)

20    read(mlist,'(a)',end = 888) rec
      Lp = index('.',rec)
      L  = index(' ',rec(Lp:) ) - 1
      inquire (file = trim(rec(1:L)), exist = jawel)


      if (jawel) then

         af = dble(k) / dble(numfil)
         CALL READYY('Reading arcinfosets',af)

         call oldfil (marc, rec(1:L) )
         call savepol()  ! do not use the selecting polygon to read a block from the file
         call delpol()
         call reaarc (marc,0)
         call restorepol()
         CALL DOCLOSE(marc)

         do k = 1, numk

            if (zk(k) .ne. dmiss) cycle

            i1     = (xk(k) - x0)/dxa + 1 ; i2 = i1 + 1
            if (i1 < 1 .or. i2 > mca) cycle

            j1     = (yk(k) - x0)/dxa + 1 ; i2 = i1 + 1
            if (j1 < 1 .or. j2 > nca) cycle

            dii    = xk(k) - x0 - i1*dxa
            djj    = yk(k) - y0 - j1*dxa

            f11    = (1d0-dii)*(1d0-djj)
            f21    = (    dii)*(1d0-djj)
            f22    = (    dii)*(    djj)
            f12    = (1d0-dii)*(    djj)

            zk(k)  = d(i1,j1)*f11 +    &
                     d(i2,j1)*f21 +    &
                     d(i2,j2)*f22 +    &
                     d(i1,j2)*f12
         enddo

      endif

999   if (allocated(d) ) deallocate(d)

      CALL READYY(' ',-1d0)

      END SUBROUTINE REAdarcsets





      !> Read a curvilinear grid to (ascii) grd-file.
      !! NOTE: reads 'old' (RGFGrid 3.x) and 'new' format (RGFGrid 4.x)
      !!
      !! Format:
      !! Start with at least one comment line, prefixed by '*', with optional keyword Spherical (old RGFGRID 3.x style)
      !! Next, zero or more key=value lines (all optional):
      !!   Coordinate System=Spherical
      !!   Missing Value=...
      !! First subsequent line without a '=' should be '0 0 0' (backwards compatibility)
      !! Next line should be mmax, nmax
      !! That ends the header, start reading coordinates in the usual fashion.
      SUBROUTINE REAgrid(Mrgf, FILNAM, ja)
      USE M_SFERIC
      USE m_grid
      USE M_MISSING, notinuse => xymis ! AvD: temp
      use m_netw  ! vanwege zkuni
      use unstruc_model
      USE M_ARCINFO
      
      implicit none

      CHARACTER (LEN=*)  :: FILNAM
      INTEGER            :: MRGF, JA
      double precision   :: xymis

      integer :: key
      integer :: Mbnd, mbca, mobs, mout

      CHARACTER NAME2*76, TEX*3, REC*132, REC1*132

      integer :: IPNT, mdep, merr, npart, l, k, ja2, i, j, istat

      logical :: jawel, kw_found

      ja       = 0
      JSFERIC  = 0
      JSFERTEK = 0

      MERR  = 0
      MC    = 0

      xymis = 0d0 ! this is the default for this file type

      read(mrgf,'(a)',iostat=istat) rec
      if (istat > 0) goto 888
      if (istat < 0) goto 9999
      !
      ! Backwards compatible: first line could contain spherical keyword
      if (index(rec, 'Spherical') >= 1  .or. &
          index(rec, 'SPHERICAL') >= 1  ) then
          ! grid has spherical coordinates.
          jsferic=1
      endif
      !
      ! looping keyword records, excluding comment lines
      !
      do
        kw_found = .false.
        read(mrgf,'(a)',iostat=istat) rec
        if (istat > 0) goto 888
        if (istat < 0) goto 9999
        if (rec(1:1) == '*') cycle
        !
        if (index(rec,'Coordinate System') >= 1) then
            kw_found = .true.
            i = index(rec,'=') + 1
            if (index(rec(i:), 'Spherical') >= 1) then
                ! grid has spherical coordinates, overruling previous choices.
                jsferic = 1
            endif
        endif
        !
        if (index(rec,'Missing Value') >= 1) then
            kw_found = .true.
            i = index(rec,'=') + 1
            read(rec(i:),*,iostat=istat) xymis
            if (istat > 0) goto 888
            if (istat < 0) goto 9999
        endif
        !
        if (.not. kw_found) then
            if (index(rec,'=') >= 1) kw_found = .true.
        endif
        !
        if (kw_found) then
            cycle ! read next record from file
        else
            exit  ! record contains the dimensions
        endif
    enddo
    !
    ! End loop, keywords
    !
    !
    ! First record behind the keywords contains dimension of the grid
    !
    read(rec,*,iostat=istat)  mc, nc
    !
    read(mrgf,'(a)',iostat=istat) rec ! read three zero's
    if (istat > 0) goto 888
    if (istat < 0) goto 9999
    !
    !  end read header of rgf-file
    !

      CALL READYY('Reading Grid-File',0d0)


      CALL INCREASEGRID(MC,NC)


      zc = zkuni


      CALL ECRREA(Xc,MMAX,NMAX,MC,NC,MRGF,0d0)
      CALL ECRREA(Yc,MMAX,NMAX,MC,NC,MRGF,0.5d0)

      ! Set to system-wide dxymiss where necessary.
      do i=1,mc
        do j=1,nc
            if (xc(i,j) == xymis .and. yc(i,j) == xymis) then
                xc(i,j) = dxymis
                yc(i,j) = dxymis
            end if
        end do
      end do

      call isitu()

      IPNT  = INDEX(FILNAM,'.')                     ! NOW READ *.DEP FILE, ONLY IF ORGANISED IN DEPTH POINTS
      NAME2 = FILNAM
      WRITE(NAME2(IPNT+1:),'(A)') 'DEP'
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(MDEP,NAME2)
         CALL REAMDD(MDEP,Zc,MC+1,NC+1,JA2)
         do i = 1,mc
            do j = 1,nc
               if (zc(i,j) .ne. dmiss) then
                  zc(i,j) = -1d0*zc(i,j)
               endif
            enddo
         enddo
      ENDIF
      
      WRITE(NAME2(IPNT+1:),'(A)') 'ASC'
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(MDEP,NAME2)
         call savepol() ! we do not want to use the selecting polygon
         call delpol()
         CALL REAARC(MDEP,0)
         call restorepol()
         if ( ubound(d,1).ge.MC .and. ubound(d,2).ge.NC ) then
            do i = 1,mc
               do j = 1,nc
                  zc(i,j) = d(i,j)
               enddo
            enddo
         end if
      ENDIF

      WRITE(NAME2(IPNT+1:),'(A)') 'bottom'   
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(MDEP,NAME2)
         CALL REABOT(MDEP,JA2)
      ENDIF

      WRITE(NAME2(IPNT+1:),'(A)') 'weirs'
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(MDEP,NAME2)
         CALL REAweir(MDEP,JA2)
      ENDIF

      WRITE(NAME2(IPNT+1:),'(A)') 'crs'
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(MDEP,NAME2)
         CALL REAcrs(MDEP,JA2)
      ENDIF

      WRITE(NAME2(IPNT+1:),'(A)') 'bnd'             ! also read *.bnd file and create polygonfile
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(Mbnd,NAME2)

         WRITE(NAME2(IPNT+1:),'(A)') 'bca'          ! also read *.bca file and make *.cmp files
         INQUIRE(FILE=NAME2, EXIST=JAWEL)
         mbca = 0
         if (jawel) then
            CALL OLDFIL(Mbca,NAME2)
         endif

         call reabnd2pol(mbnd,mbca) ! Old, model-specific. TODO: remove or generalize

      ENDIF

      WRITE(NAME2(IPNT+1:),'(A)') 'obs'             ! also read *.obs file and create obsxyn
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(Mobs,NAME2)
         WRITE(NAME2(IPNT+1:),'(A)') '_obs.xyn'
         call newfil(mout,name2)
         call reaobs2stat(mobs, mout)
      endif

      WRITE(NAME2(IPNT+1:),'(A)') 'thd'             ! also read *.thd file and create polygonfile
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(Mbnd,NAME2)
      
         WRITE(NAME2(IPNT:),'(A)') '_thd.pli'
         call newfil(mout,name2)
   
         call reathd2pli(mbnd,mout)

      ENDIF

      name2=filnam
      WRITE(NAME2(IPNT+1:),'(A)') 'mnbar'             ! also read *.thd file and create polygonfile
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(Mbnd,NAME2)
      
         WRITE(NAME2(IPNT:),'(A)') '_bar.pli'
        
         call newfil(mout,name2)
   
         call reabar2pli(mbnd,mout)

      ENDIF

      name2=filnam
      WRITE(NAME2(IPNT+1:),'(A)') 'dry'             ! also read *.thd file and create polygonfile
      INQUIRE(FILE=NAME2, EXIST=JAWEL)
      IF (JAWEL) THEN
         CALL OLDFIL(Mbnd,NAME2)
      
         WRITE(NAME2(IPNT:),'(A)') '_dry.pli'
        
         call newfil(mout,name2)
   
         call readry2pli(mbnd,mout)

      ENDIF

      CALL READYY(' ',-1d0)
      CALL DOCLOSE (MRGF)

      ! call gridtonet()

      ! call delgrd(key,1,0)


      ja = 1
      return

 9999 CONTINUE
      CALL READYY(' ',-1d0)
      CALL DOCLOSE (MRGF)
      RETURN

  888 CONTINUE
      CALL QNERROR('Reading Error, Try UX2DOS or DOS2UX',  ' ',' ')
      CALL READYY(' ',-1d0)
      CALL DOCLOSE (MRGF)
      RETURN

      end subroutine REAgrid

      SUBROUTINE INCREASEGRID(M,N)
      USE M_GRID
      USE M_MISSING
      use m_alloc
      implicit none
      integer :: m, n

      integer, dimension(2) :: ibounds, iboundsp1

      !if (m <= mmax .and. n <= nmax) return
      !Freshly allocate arrays, so that size fits exactly (e.g., for passing as 2D arrays to ecrrea)
!      if (allocated(xc)) deallocate (xc,yc,zc,ijc,ijyes)

!      mmax = m ; nmax = n ; MNMAX = MAX(M,N)
!      ibounds   = (/ mmax, nmax /)
!      iboundsp1 = (/ mmax+1, nmax+1 /)

      mmax = m+1 ; nmax = n+1 ; MNMAX = MAX(M,N)
      ibounds   = (/ mmax, nmax /)
      iboundsp1 = ibounds

      call realloc(xc, ibounds, fill=dxymis)
      call realloc(yc, ibounds, fill=dxymis)
      call realloc(zc, iboundsp1, fill=dxymis)
      call realloc(ijc, ibounds, fill=0)
      call realloc(ijyes, ibounds, fill=0)

      END SUBROUTINE INCREASEGRID

     subroutine gridtonet()
      use m_netw
      use m_grid
      use m_missing
      use gridoperations

      implicit none
      double precision :: af

      integer, allocatable :: mn(:,:)
      double precision XX(8), YY(8), ZZ(8), tooclose0, length
      integer :: k0, l0, ja, jadoorladen, i, j, k, l, method, ierr, key

      jadoorladen = 1

      IF (JADOORLADEN .EQ. 0) THEN
         K0 = 0
         L0 = 0
      ELSE
         K0 = NUMK
         L0 = NUML
      ENDIF

      K  = 0
      DO I = 1,MC                                    ! COUNT NR OF NODES
         DO J = 1,NC
            IF (Xc(I,J) .NE. dXYMIS) THEN
               K = K + 1
            ENDIF
         ENDDO
      ENDDO

      !IF (K0+K .GT. SIZE(XK) ) THEN
         CALL INCREASENETW(K0+K,L0+4*K)
      !ENDIF

      K  = K0
      L  = L0                                        ! COUNT MAX NR OF ATTACHED LINKS PER NODE


      CALL READYY('Arranging curvilinear grid-in network',0d0)


      if (allocated (mn) )  deallocate(mn)
      allocate ( mn(mc,nc) , stat = ierr ) ; mn = 0
      call aerr('mn(mc,nc)', ierr , mc*nc)

      DO I = 1,MC
         DO J = 1,NC
            if (xc(i,j) .ne. dxymis) then
               call addnetpointnocheck(xc(i,j), yc(i,j), zc(i,j), k )
               mn(i,j) = k
            endif
         ENDDO
      ENDDO
      numk = k

      af = 0.2d0
      CALL READYY('Arranging curvilinear grid-in network',af)


      DO I = 1,MC-1
         DO J = 1,NC
            if ( mn(i,j) .ne. 0 .and. mn(i+1,j) .ne. 0 )  then
               L       = L + 1
               kn(1,L) = mn(i,j)
               kn(2,L) = mn(i+1,j)
               KN(3,L) = 2
            endif
         ENDDO
      ENDDO

      af = 0.4d0
      CALL READYY('Arranging curvilinear grid-in network',af)


      DO I = 1,MC
         DO J = 1,NC-1
            if ( mn(i,j) .ne. 0 .and. mn(i,j+1) .ne. 0 )  then
               L       = L + 1
               kn(1,L) = mn(i,j)
               kn(2,L) = mn(i,j+1)
               KN(3,L) = 2
            endif
         ENDDO
      ENDDO

      af = 0.6d0
      CALL READYY('Arranging curvilinear grid-in network',af)

      numl = l
      call setnodadm(0)

      CALL READYY('Arranging curvilinear grid-in network',-1d0)

      ! call copydeptosam()
      
      
      if (k0 > 0) then 
      

      JA = 1

      call readyy('Merging networks', 0d0)
      call findcells(0)

!     merge nodes

      if ( tooclose.gt.1d-16 .and. k0 > 0) then
         CALL CONFRM('MERGE NODES ? ',JA)
         IF (JA == 1) call MERGENODESINPOLYGON()
      end if

!     merge boundary nodes
!      call mergenet()

      call readyy('Merging networks', -1d0)

      endif
      
!     set network status
      netstat = NETSTAT_CELLS_DIRTY

      END subroutine gridtonet




     SUBROUTINE delgrd(KEY,JASAVE,jadelpol)
!    delete grid
     use m_grid
     use m_missing
     use m_polygon, only: NPL, xpl, ypl, zpl
     use geometry_module, only: dbpinpol
     
     implicit none
     integer                :: inhul, ja, i, j
     integer, intent(in)    :: jasave, jadelpol
     integer, intent(inout) :: key
     double precision       :: xi, yi
     
     inhul = -1

     IF (JASAVE .EQ. 1) CALL SAVEgrd()
     KEY = 3
     IF (NPL .LE. 2) THEN
        IF (NPL .GE. 1) THEN
           CALL CONFRM('NO POLYON, SO DELETE all GRID POINTS ? ',JA)
           IF (JA .EQ. 0) THEN
              KEY = 0
           ELSE
              XC = 0d0 ; YC = 0d0; MC = 0 ; NC = 0
           ENDIF
        ELSE
           XC = 0d0 ; YC = 0d0; MC = 0 ; NC = 0
        ENDIF
        RETURN
     ENDIF

     DO 10 I = 1,MC
        DO 10 J = 1,NC
           IF (Xc(I,J) .NE. DXYMIS) THEN
              CALL dbpinpol(Xc(i,j), yc(i,j), INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
              IF (INHUL .EQ. 1) Xc(I,J) = XYMIS
           ENDIF
   10 CONTINUE

!      CALL ADJUST(X, Y, MC, NC, WW1, WW2)
     if (jadelpol == 1) call delpol()
     RETURN
     END SUBROUTINE delgrd

     subroutine savegrd()
        use m_grid
        use m_missing
        use m_alloc

        implicit none

        integer, dimension(2) :: ibounds
        ! Possibly resize the help grid if the actual grid is larger.
        ibounds = ubound(xc)
        call realloc(xch, ibounds, fill=dmiss)
        ibounds = ubound(yc)
        call realloc(ych, ibounds, fill=dmiss)
        ibounds = ubound(zc)
        call realloc(zch, ibounds, fill=dmiss)
        xch = xc
        ych = yc
        zch = zc
        mch = mc
        nch = nc
     end subroutine savegrd
     subroutine restoregrd()
     use m_grid
     implicit none
     if (allocated (xch) ) then 
        call increasegrid(mch,nch)
        xc = xch
        yc = ych
        zc = zch
        mc = mch
        nc = nch
     endif
     end subroutine restoregrd

      SUBROUTINE ISITU()
      use m_grid
      USE M_MISSING

      implicit none

!C     IJYES, WELKE CELLEN DOEN MEE 1 OF 0
!C     IJC  , CODE VAN PUNT, ZIE FIGUUR
!C                            0  14   3  13
!C     9  4  3               14  12  10   2
!C     6  1  2                4  10  10   2
!C     8  5  7               11   1   1  12
!C
!C     ALS IK NOG EENS TIJD HEB, ZAL IK DE NUMMERING MOOIER MAKEN
!C

      double precision :: x1, x2, x3, x4, x5, x6, x7, x8, x9, y1
      integer :: i, j, i1, i2, i3, i4, IJYES2, IJYES3, IJYES4, jaontop, jaunconnected

      if ( allocated (ijc) )   deallocate (ijc   )
      if ( allocated (ijyes) ) deallocate (ijyes )
      allocate (ijc(mmax,nmax), ijyes(mmax,nmax) )


    5 CONTINUE
      ijc = 0 ; ijyes = 0

      DO 10 I = 1,MC-1
         DO 10 J = 1,NC-1
            X1 = Xc(I,J)
            X2 = Xc(I+1,J)
            X3 = Xc(I+1,J+1)
            X4 = Xc(I,J+1)
            IF (X1 .NE. dXYMIS .AND. X2 .NE. dXYMIS .AND.   &
                X3 .NE. dXYMIS .AND. X4 .NE. dXYMIS ) IJYES(I,J) = 1
   10 CONTINUE


      DO 11 I = 1,MC
         DO 11 J = 1,NC
            X1 = Xc(I,J)
            IF (I .NE. 1)  X6 = Xc(I-1,J)
            IF (I .NE. MC) X2 = Xc(I+1,J)
            IF (J .NE. 1)  X5 = Xc(I,J-1)
            IF (J .NE. NC) X4 = Xc(I,J+1)
            IF (I .NE. MC .AND. J .NE. NC ) X3 = Xc(I+1,J+1)
            IF (I .NE.  1 .AND. J .NE.  1 ) X8 = Xc(I-1,J-1)
            IF (I .NE.  1 .AND. J .NE.  NC) X9 = Xc(I-1,J+1)
            IF (I .NE. MC .AND. J .NE.  1 ) X7 = Xc(I+1,J-1)
!           POSITIE BENOEMEN
            IF (X1 .EQ. dXYMIS) THEN
               IJC(I,J) = 0
            ELSE IF (I .EQ. 1) THEN
!              LINKS
               IF (J .EQ. 1) THEN
                  IJC(I,J) = 11
               ELSE IF (J .EQ. NC) THEN
                  IJC(I,J) = 14
               ELSE IF (X5 .EQ. dXYMIS) THEN
                  IJC(I,J) = 11
               ELSE IF (X4 .EQ. dXYMIS) THEN
                  IJC(I,J) = 14
               ELSE
                  IJC(I,J) = 4
               ENDIF
            ELSE IF (I .EQ. MC) THEN
!              RECHTS
               IF (J .EQ. 1) THEN
                  IJC(I,J) = 12
               ELSE IF (J .EQ. NC) THEN
                  IJC(I,J) = 13
               ELSE IF (X5 .EQ. dXYMIS) THEN
                  IJC(I,J) = 12
               ELSE IF (X4 .EQ. dXYMIS) THEN
                  IJC(I,J) = 13
               ELSE
                  IJC(I,J) = 2
               ENDIF
            ELSE IF (J .EQ. 1) THEN
!              ONDER
               IF (X6 .EQ. dXYMIS) THEN
                  IJC(I,J) = 11
               ELSE IF (X2 .EQ. dXYMIS) THEN
                  IJC(I,J) = 12
               ELSE
                  IJC(I,J) = 1
               ENDIF
            ELSE IF (J .EQ. NC) THEN
!              BOVEN
               IF (X6 .EQ. dXYMIS) THEN
                  IJC(I,J) = 14
               ELSE IF (X2 .EQ. dXYMIS) THEN
                  IJC(I,J) = 13
               ELSE
                  IJC(I,J) = 3
               ENDIF
            ELSE
               I1 = IJYES(I,J)
               I2 = IJYES(I-1,J)
               I3 = IJYES(I-1,J-1)
               I4 = IJYES(I,J-1)
               IF (I1.EQ.1.AND.I2.EQ.1.AND.I3.EQ.1.AND.I4.EQ.1) THEN
                  IJC(I,J) =  10
               ELSE IF(I1.EQ.0.AND.I2.EQ.1.AND.I3.EQ.1.AND.I4.EQ.1) THEN
                  IJC(I,J) =  11
               ELSE IF(I1.EQ.1.AND.I2.EQ.0.AND.I3.EQ.1.AND.I4.EQ.1) THEN
                  IJC(I,J) =  12
               ELSE IF(I1.EQ.1.AND.I2.EQ.1.AND.I3.EQ.0.AND.I4.EQ.1) THEN
                  IJC(I,J) =  13
               ELSE IF(I1.EQ.1.AND.I2.EQ.1.AND.I3.EQ.1.AND.I4.EQ.0) THEN
                  IJC(I,J) =  14
               ELSE IF(I1.EQ.1.AND.I2.EQ.1.AND.I3.EQ.0.AND.I4.EQ.0) THEN
                  IJC(I,J) =  1
               ELSE IF(I1.EQ.1.AND.I2.EQ.0.AND.I3.EQ.0.AND.I4.EQ.1) THEN
                  IJC(I,J) =  4
               ELSE IF(I1.EQ.0.AND.I2.EQ.0.AND.I3.EQ.1.AND.I4.EQ.1) THEN
                  IJC(I,J) =  3
               ELSE IF(I1.EQ.0.AND.I2.EQ.1.AND.I3.EQ.1.AND.I4.EQ.0) THEN
                  IJC(I,J) =  2
               ELSE IF(I1.EQ.1.AND.I2.EQ.0.AND.I3.EQ.0.AND.I4.EQ.0) THEN
                  IJC(I,J) =  11
               ELSE IF(I1.EQ.0.AND.I2.EQ.1.AND.I3.EQ.0.AND.I4.EQ.0) THEN
                  IJC(I,J) =  12
               ELSE IF(I1.EQ.0.AND.I2.EQ.0.AND.I3.EQ.1.AND.I4.EQ.0) THEN
                  IJC(I,J) =  13
               ELSE IF(I1.EQ.0.AND.I2.EQ.0.AND.I3.EQ.0.AND.I4.EQ.1) THEN
                  IJC(I,J) =  14
               ENDIF
            ENDIF
   11 CONTINUE



      JAUNCONNECTED = 0
      DO 20 I = 2,MC
         DO 20 J = 2,NC
            X1 = Xc(I,J)
            IF (X1 .NE. dXYMIS) THEN
!              ALS ER EEN PUNT IS, MAAR GEEN VAN DE OMLIGGENDE CELLEN IS
!              GESLOTEN
               IF (IJYES(I,J) .EQ. 0) THEN
                  IJYES2 = IJYES(I-1,J)
                  IJYES3 = IJYES(I-1,J-1)
                  IJYES4 = IJYES(I,J-1)
                  IF (IJYES2 .EQ. 0 .AND. IJYES3 .EQ. 0 .AND. &
                     IJYES4 .EQ. 0                         ) THEN
                 !    WRITE(MDIA,*)'UNCONNECTED POINT AT ',I,J,' DELETED'
                     JAUNCONNECTED = 1
                     Xc(I,J) = dXYMIS
                     Yc(I,J) = dXYMIS
                  ENDIF
               ENDIF
            ENDIF
   20 CONTINUE
      J = 1
      DO 25 I = 2,MC
         X1 = Xc(I,J)
         IF (X1 .NE. dXYMIS) THEN
            IF (IJYES(I-1,J) .EQ. 0 .AND. IJYES(I,J) .EQ. 0) THEN
             !  WRITE(MDIA,*)'UNCONNECTED POINT AT ',I,J,' DELETED'
               Xc(I,J) = dXYMIS
               Yc(I,J) = dXYMIS
            ENDIF
         ENDIF
   25 CONTINUE
      I = 1
      DO 26 J = 2,NC
         X1 = Xc(I,J)
         IF (X1 .NE. dXYMIS) THEN
            IF (IJYES(I,J-1) .EQ. 0 .AND. IJYES(I,J) .EQ. 0) THEN
            !   WRITE(MDIA,*)'UNCONNECTED POINT AT ',I,J,' DELETED'
               Xc(I,J) = dXYMIS
               Yc(I,J) = dXYMIS
            ENDIF
         ENDIF
   26 CONTINUE
      J = 1
      I = 1
      IF (IJYES(I,J) .EQ. 0 .AND. Xc(I,J) .NE. dXYMIS) THEN
        ! WRITE(MDIA,*)'UNCONNECTED POINT AT ',I,J,' DELETED'
         Xc(I,J) = dXYMIS
         Yc(I,J) = dXYMIS
      ENDIF
      IF (JAUNCONNECTED .EQ. 1) then
         GOTO 5
      endif

!     spline2curvi: do not check on corrupt grids
!!     CHECK OP CORRUPTE ROOSTERS
!      JAONTOP = 0
!      DO 30 I = 1,MC-1
!         DO 30 J = 1,NC-1
!            X1 = Xc(I,J)
!            Y1 = Yc(I,J)
!            IF (X1 .NE. dXYMIS) THEN
!               IF (X1.EQ.Xc(I,J+1)  .AND. Y1.EQ.Yc(I,J+1) .OR. &
!                   X1.EQ.Xc(I+1,J)  .AND. Y1.EQ.Yc(I+1,J) .OR. &
!                   X1.EQ.Xc(I+1,J+1).AND. Y1.EQ.Yc(I+1,J+1)) THEN
!                   JAONTOP = 1
!                   Xc(I,J)  = dXYMIS
!                   Yc(I,J)  = dXYMIS
!               ENDIF
!            ENDIF
!   30 CONTINUE
!      IF (JAONTOP .EQ. 1) THEN
!         ! CALL QNERROR('IDENTICAL NEIGHBOURPOINT DELETED,', 'CHECK GRID',' ')
!         GOTO 5
!      ENDIF

      RETURN
      END SUBROUTINE ISITU


!> convert from Bessel to WGS84
subroutine bessel2wgs84(phibes, lambes, phiwgs, lamwgs)
   implicit none
   
   double precision, intent(in)  :: phibes, lambes
   double precision, intent(out) :: phiwgs, lamwgs

   double precision, dimension(2), parameter :: A1 = (/  9.99882860000000d-01, 3.29000000000000d-06 /)
   double precision, dimension(2), parameter :: A2 = (/ -1.25000000000000d-06, 9.99853330000000d-01 /)
   double precision, dimension(2), parameter :: b  = (/  5.12891000000000d-03, 1.83250000000000d-04 /)

   phiwgs = A1(1)*phibes + A2(1)*lambes + b(1)
   lamwgs = A1(2)*phibes + A2(2)*lambes + b(2)

end subroutine bessel2wgs84


!> convert from WGS84 to Bessel
subroutine wgs842bessel(phiwgs, lamwgs, phibes, lambes)
   implicit none
   
   double precision, intent(in)  :: phiwgs, lamwgs
   double precision, intent(out) :: phibes, lambes

   double precision, dimension(2), parameter :: A1 = (/  1.00011715371927d+00,  -3.29086810736171d-06  /)
   double precision, dimension(2), parameter :: A2 = (/  1.25032982802497d-06,   1.00014669151113d+00  /)
   double precision, dimension(2), parameter :: b  = (/ -5.12951110000526d-03,  -1.83260002653070d-04  /)

   phibes = A1(1)*phiwgs + A2(1)*lamwgs + b(1)
   lambes = A1(2)*phiwgs + A2(2)*lamwgs + b(2)

end subroutine wgs842bessel
