        !COMPILER-GENERATED INTERFACE MODULE: Mon Jun 27 17:36:27 2016
        MODULE INIPART_ASC__genmod
          INTERFACE 
            SUBROUTINE INIPART_ASC(LGRID,LGRID2,NMAX,MMAX,XCOR,YCOR,    &
     &NOPART,NOSUBS,SUBST,INI_FILE,XPOL,YPOL,WPART,XPART,CONC2,YPART,   &
     &ZPART,NPART,MPART,KPART,IPTIME,NPMAX,NROWSMAX,LUNPR)
              INTEGER(KIND=4), INTENT(IN) :: NROWSMAX
              INTEGER(KIND=4), INTENT(IN) :: NPMAX
              INTEGER(KIND=4), INTENT(INOUT) :: NOSUBS
              INTEGER(KIND=4), INTENT(IN) :: MMAX
              INTEGER(KIND=4), INTENT(IN) :: NMAX
              INTEGER(KIND=4), INTENT(IN) :: LGRID(NMAX,MMAX)
              INTEGER(KIND=4), INTENT(IN) :: LGRID2(NMAX,MMAX)
              REAL(KIND=4), INTENT(IN) :: XCOR(NMAX*MMAX)
              REAL(KIND=4), INTENT(IN) :: YCOR(NMAX*MMAX)
              INTEGER(KIND=4), INTENT(INOUT) :: NOPART
              CHARACTER(*), INTENT(IN) :: SUBST(*)
              CHARACTER(*), INTENT(IN) :: INI_FILE
              REAL(KIND=4), INTENT(OUT) :: XPOL(NROWSMAX)
              REAL(KIND=4), INTENT(OUT) :: YPOL(NROWSMAX)
              REAL(KIND=4), INTENT(OUT) :: WPART(NOSUBS,NPMAX)
              REAL(KIND=4), INTENT(OUT) :: XPART(NPMAX)
              REAL(KIND=4), INTENT(OUT) :: CONC2(NROWSMAX)
              REAL(KIND=4), INTENT(OUT) :: YPART(NPMAX)
              REAL(KIND=4), INTENT(OUT) :: ZPART(NPMAX)
              INTEGER(KIND=4), INTENT(OUT) :: NPART(NPMAX)
              INTEGER(KIND=4), INTENT(OUT) :: MPART(NPMAX)
              INTEGER(KIND=4), INTENT(OUT) :: KPART(NPMAX)
              INTEGER(KIND=4), INTENT(OUT) :: IPTIME(NPMAX)
              INTEGER(KIND=4), INTENT(IN) :: LUNPR
            END SUBROUTINE INIPART_ASC
          END INTERFACE 
        END MODULE INIPART_ASC__genmod
