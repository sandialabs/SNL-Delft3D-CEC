        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 27 17:36:29 2016
        MODULE UMAGI__genmod
          INTERFACE 
            FUNCTION UMAGI(XP,YP,VZ,NP,MP,KP,NMAX,MMAX,LAYT,FLOW,DEPTH, &
     &LGRID,VOL,XCOR,YCOR,LGRID2,MNMAXK,ACOMP,TCKTOT)
              REAL(KIND=4) :: XP
              REAL(KIND=4) :: YP
              REAL(KIND=4) :: VZ
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: MP
              INTEGER(KIND=4) :: KP
              INTEGER(KIND=4) :: NMAX
              INTEGER(KIND=4) :: MMAX
              INTEGER(KIND=4) :: LAYT
              REAL(KIND=4) :: FLOW(:)
              REAL(KIND=4) :: DEPTH(:)
              INTEGER(KIND=4) :: LGRID(:,:)
              REAL(KIND=4) :: VOL
              REAL(KIND=4) :: XCOR(:)
              REAL(KIND=4) :: YCOR(:)
              INTEGER(KIND=4) :: LGRID2(:,:)
              INTEGER(KIND=4) :: MNMAXK
              LOGICAL(KIND=4) :: ACOMP
              REAL(KIND=4) :: TCKTOT(:)
              REAL(KIND=4) :: UMAGI
            END FUNCTION UMAGI
          END INTERFACE 
        END MODULE UMAGI__genmod
