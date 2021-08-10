module load automake/1.14.1
module load autoconf/2.69
module load libtool/2.4.3
module load intel/16.0.3
module load mpich2/3.1.4_intel_16.0.3

#module load netcdf/v4.4.0_v4.4.4_intel_16.0.3
module load netcdf/v4.6.1_v4.4.0_intel_16.0.3
module load petsc/3.4.0_intel16.0.3_mpich_3.1.4
module load metis/5.1.0_intel16.0.3

module load gcc/4.9.2
module load shapelib/1.4.1_intel16.0.3
module load proj/5.2.0_intel16.0.3


export FC=mpif90
export F77=$F77
