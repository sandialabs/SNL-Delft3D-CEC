        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 27 17:36:30 2016
        MODULE P10DDB__genmod
          INTERFACE 
            SUBROUTINE P10DDB(NCONN,CONN,N0,DDSHIFT,NP,MP,XP,YP)
              USE TYPOS
              INTEGER(KIND=4), INTENT(IN) :: NCONN
              TYPE (PNT), INTENT(IN) :: CONN(NCONN)
              INTEGER(KIND=4), INTENT(IN) :: N0
              INTEGER(KIND=4), INTENT(IN) :: DDSHIFT
              INTEGER(KIND=4), INTENT(OUT) :: NP
              INTEGER(KIND=4), INTENT(OUT) :: MP
              REAL(KIND=4), INTENT(INOUT) :: XP
              REAL(KIND=4), INTENT(INOUT) :: YP
            END SUBROUTINE P10DDB
          END INTERFACE 
        END MODULE P10DDB__genmod
