        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 27 17:35:50 2016
        MODULE WRTTRK__genmod
          INTERFACE 
            SUBROUTINE WRTTRK(LUNDIA,FOUT,FILNAM,ITTRKC,NTRK,NPMAX,XA,YA&
     &,ZA,XYZTRK)
              INTEGER(KIND=4), INTENT(IN) :: NPMAX
              INTEGER(KIND=4), INTENT(IN) :: LUNDIA
              LOGICAL(KIND=4), INTENT(OUT) :: FOUT
              CHARACTER(*) :: FILNAM
              INTEGER(KIND=4) :: ITTRKC
              INTEGER(KIND=4), INTENT(IN) :: NTRK
              REAL(KIND=4), INTENT(IN) :: XA(NPMAX)
              REAL(KIND=4), INTENT(IN) :: YA(NPMAX)
              REAL(KIND=4), INTENT(IN) :: ZA(NPMAX)
              REAL(KIND=4), INTENT(OUT) :: XYZTRK(3,NPMAX)
            END SUBROUTINE WRTTRK
          END INTERFACE 
        END MODULE WRTTRK__genmod