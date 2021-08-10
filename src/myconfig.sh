#!/bin/bash

#./configure --with-mpi --prefix=$HOME --with-openmp=no --with-petsc=/u/pijl/petsc
#FCFLAGS='-m64 -mcmodel=large' ./configure --with-mpi --prefix=$HOME --with-openmp=no --with-petsc=/u/pijl/petsc

#./configure --disable-openmp --with-mpi --prefix=$HOME --with-petsc=/u/pijl/petsc
#./configure --disable-openmp --with-mpi --prefix=$HOME --with-petsc --disable-shared --with-petsc=$PETSC_DIR
#./configure --disable-openmp --with-mpi --prefix=$HOME --disable-shared --with-petsc=$PETSC_DIR

#DEVUX64
#./configure --disable-openmp --with-mpi --prefix=$HOME --with-petsc=$PETSC_DIR

#LISA
#CC=icc CXX=icc FCFLAGS=-lhdf5 ./configure --with-mpi --prefix=$HOME --with-petsc=$PETSC_DIR --disable-openmp

#CC=icc CXX=icc FCFLAGS="-lhdf5 -L/usr/lib/x86_64-linux-gnu -lcurl" ./configure --with-mpi --prefix=$HOME --with-petsc=$PETSC_DIR --disable-openmp

# Patch the generated lib dependencies string in libtool:
# On some systems, mglob_options_string contains confusing double quotes.
# This sed command replaces, e.g., -lutils\"  ->  -lutils
#sed -i -e 's/\(-l[^ ]*\)\\\"/\1/' libtool

#LISA, scalasca
#FC="skin mpif90" CC="icc" F77="skin mpif90" MPIFC="skin mpif90" FCFLAGS=-lhdf5 ./configure --disable-openmp --with-mpi --prefix=$HOME --with-petsc=$PETSC_DIR


#DEVUX64
#if [ "$DOGNU" = "1" ]; then
##	gfortran
#	FC=mpif90 F77=mpif90 NETCDF_FORTRAN_CFLAGS=-I${NETCDFROOT}/include NETCDF_FORTRAN_LIBS="-L${NETCDFROOT}/lib -lnetcdf -lnetcdff" ./configure --disable-openmp --with-mpi --prefix=$HOME --with-petsc=$PETSC_DIR
#else
##	ifort
##	./configure --with-mpi --prefix=$HOME --with-petsc=$PETSC_DIR --disable-openmp --with-metis=$METIS_DIR
##	FCFLAGS="-O2 -g" ./configure --with-mpi --prefix=$HOME --with-petsc=$PETSC_DIR --disable-openmp --with-metis=$METIS_DIR
##	./configure --with-mpi --prefix=$HOME --with-petsc=$PETSC_DIR --with-metis=$METIS_DIR
#fi

#tl-203
# source ./prebuild-tl-203.sh
#./configure --with-mpi --prefix=$HOME --disable-openmp --with-metis --with-petsc

#Cartesius
#NETCDF_FORTRAN_CFLAGS="-I/hpc/sw/netcdf-fortran-4.4.2-intel-seq/include -I/hpc/sw/netcdf-4.3.3.1-intel-seq/include"  NETCDF_FORTRAN_LIBS="-L/hpc/sw/netcdf-4.3.3.1-intel-seq/lib -lnetcdf -L/hpc/sw/netcdf-fortran-4.4.2-intel-seq/lib -lnetcdff" ./configure --prefix=$HOME --with-mpi --with-petsc --with-metis=$METIS_DIR

#h6
# source ./prebuild-h6.sh
#export FC=mpif90
#export F77=$FC
#export FCFLAGS="-O0 -g"
#./configure --with-mpi --prefix=$HOME --disable-openmp --with-metis --with-petsc


# DEBUG
source ./prebuild-h6_buildserver_intel16.sh
export FC=mpif90
export F77=$FC
export FCFLAGS="-O0 -g"
#export FCFLAGS="-O0 -g -threads"
#export FFLAGS="-O0 -g -threads"

LDFLAGS="-L$PROJ_DIR/lib -L$SHAPELIB_DIR/lib $LDFLAGS" CPPFLAGS="-I$PROJ_DIR/include -I$SHAPELIB_DIR/include $CPPFLAGS" ./configure --with-mpi --prefix=$HOME --disable-openmp --with-metis --with-petsc --with-shapelib=$SHAPELIB_DIR --with-proj=$PROJ_DIR --disable-gdal
