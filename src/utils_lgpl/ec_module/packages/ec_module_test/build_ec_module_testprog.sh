#!/bin/bash
# Improvised make for the ec_module_test program
# to be started from the top src directory .... 
module load automake/1.14.1
module load autoconf/2.69
module load libtool/2.4.3
#module load intel/16.0.3
#module load mpich2/3.1.4_intel_16.0.3
#module load netcdf/v4.6.1_v4.4.0_intel_16.0.3
module load intel/14.0.3
module load mpich2/3.1.4_intel_14.0.3
module load netcdf/v4.3.2_v4.4.0_intel_14.0.3
module load gcc/4.9.1
./autogen.sh

cd third_party_open/kdtree2/
./autogen.sh
cd - 

export FC=mpif90
export F77=$FC
export FCFLAGS=""
export FFLAGS=""
./configure 
make clean
make -C third_party_open
make -C utils_lgpl

