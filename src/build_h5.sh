#! /bin/bash

#-------------------------------------------------------------------------------
#   Top-Level Build Script for Delft3D Open Source Code
#
#   There are command-line options to select Fortran compiler and debug or not.
#
#   ToDo:  Remove stripping of executables when the debug flag is set.
#   It's even debatable whether stripping belongs in the build.  I think not.
#
#   ToDo: Don't preintialize the compiler, the user should do this himself
#   so that he's aware exactly which version he's using.  Besides, we can't
#   keep up with every new compiler update.  This script should be ultra-low
#   maintanence.
#
#   Irv.Elshoff@Deltares.NL
#   2 jul 12
#
#   Copyright (C)  Stichting Deltares, 2011-2019.
#-------------------------------------------------------------------------------

# This script must be executed in the directory where it resides
orgdir=`pwd`
scriptdirname=`readlink \-f \$0`
maindir=`dirname $scriptdirname`
cd $maindir


# Default values
compiler=''
configureArgs=''
debug=0
noMake=0
platform='intel64'
useSp=0

#-------------------------------------------------------------------------------
function usage {
    echo "Usage: `basename $0` <compiler> [-debug] [-make] [-64bit] [-sp] [-configure <args>] [-?]"
    echo "Compiler is one of:"
    echo "    -gnu"
    echo "    -intel10"
    echo "    -intel11.0 (-intel11)"
    echo "    -intel11.1"
    echo "    -intel12"
    echo "    -intel14 (-intel14.0.3)"
    echo "    -intel16 (-intel16.0.3)"
    echo "WARNING: On h5 currently for Intel only -64bit is supported"
    }


#-------------------------------------------------------------------------------
# Add date time to logging info
function log {
    echo "`date +%Y%m%d.%H%M%S` :: $*"
    }



#-------------------------------------------------------------------------------
# Add a directory to an environment parameter
function addpath {
    path="$1"
    shift

    for dir in $*; do
        if [ -d $dir ]; then
            eval "export $path=\"$dir:\$$path\""
        fi
    done
    }


#-------------------------------------------------------------------------------
# Identify which program is used
function witch {
    w=`which $1`
    (
        cd `dirname $w`
        /bin/pwd
    )
    }


#===============================================================================
# Process command-line arguments

while [ $# -gt 0 ]; do
    case $1 in
        -64bit)
            platform='intel64'
            ;;
        -c|-configure)
            shift
            configureArgs="$1"
            ;;
        -d|-debug)
            debug=1
            ;;
        -gnu)
            compiler='gnu'
            ;;
        -intel10)
            compiler='intel10'
            ;;
        -intel11.0|-intel11)
            compiler='intel11.0'
            ;;
        -intel11.1)
            compiler='intel11.1'
            ;;
        -intel12)
            compiler='intel12'
            ;;
        -intel14|-intel14.0.3)
            compiler='intel14'
            ;;
        -intel16|-intel16.0.3)
            compiler='intel16'
            ;;
        -m|-make)
            noMake=1
            ;;
        -sp)
            useSp=1
            ;;
        -?)
            usage
            cd $orgdir
            exit 0
            ;;
        *)
            usage
            cd $orgdir
            exit 1
            ;;
    esac
    shift
done

if [ "$compiler" == '' ]; then
    echo "You must specify a compiler"
    usage
    cd $orgdir
    exit 1
fi

mkdir -p logs

if [ "$BASH_ENV" != '' ]; then
    echo 'Warning: Unsetting BASH_ENV'
    unset BASH_ENV
fi


#===============================================================================
# Initialize Modules
initModule1=". /usr/share/Modules/init/sh"
initModule2=". /usr/share/Modules/init/bash"
eval $initModule1
if [ $? -ne 0 ]; then
    echo 'ERROR: Module initialization(1/2) fails!'
    cd $orgdir
    exit 1
fi
eval $initModule2
if [ $? -ne 0 ]; then
    echo 'ERROR: Module initialization(2/2) fails!'
    cd $orgdir
    exit 1
fi

#===============================================================================
# Initialize Fortran compiler

fortranModule=""
case $compiler in
    gnu)
        fortranModule="gcc/4.9.1"
        ifortInit="module load $fortranModule"
        iccInit=""
        echo "Using GNU compilers in `witch gfortran`"
        ;;

    intel16)
        fortranModule="intel/16.0.3"
        ifortInit="module load $fortranModule"
        iccInit=""
        echo "Using Intel 16.0.3 Fortran ($platform) compiler"
        ;;

    intel14)
        fortranModule="intel/14.0.3"
        ifortInit="module load $fortranModule"
        iccInit=""
        echo "Using Intel 14.0.3 Fortran ($platform) compiler"
        ;;

    intel12)
        ifortInit=". /opt/intel/bin/ifortvars.sh $platform"
        iccInit=""
        echo "Using Intel 12 Fortran ($platform) compiler"
        ;;

    intel11.1)
        if [ "$platform" == 'intel64' ]; then
            if [ -d /opt/intel/Compiler/11.1/072/bin/intel64 ]; then
                ifortInit=". /opt/intel/Compiler/11.1/072/bin/intel64/ifortvars_intel64.sh $platform"
                iccInit=""
                idbInit=". /opt/intel/Compiler/11.1/072/bin/intel64/idbvars.sh"
                echo "Using Intel 11.1 Fortran ($platform) compiler"
            fi
        else
            if [ -d /opt/intel/Compiler/11.1/072 ]; then
                ifortInit=". /opt/intel/Compiler/11.1/072/bin/ifortvars.sh $platform"
                iccInit=""
                idbInit=". /opt/intel/Compiler/11.1/072/bin/$platform/idbvars.sh"
                echo "Using Intel 11.1 Fortran ($platform) compiler"
            fi
        fi
        ;;

    intel11.0)
        if [ -d /opt/intel/Compiler/11.0/081 ]; then
            ifortInit=". /opt/intel/Compiler/11.0/081/bin/ifortvars.sh $platform"
            idbInit=". /opt/intel/Compiler/11.0/081/bin/$platform/idbvars.sh"
            echo "Using Intel 11.0 Fortran ($platform) compiler"
            iccInit=". /opt/intel/Compiler/11.0/081/bin/iccvars.sh $platform"
            echo "Using Intel 11.0 C ($platform) compiler"
        fi
        ;;

    intel10)
        ifortInit='. /opt/intel/fc/10/bin/ifortvars.sh'
        iccInit=""
        idbInit='. /opt/intel/idb/10/bin/idbvars.sh'
        echo "Using Intel 10 Fortran compiler (DEPRECATED!)"
        ;;

    *)
        ifortInit='/bin/true'
        echo "Using default Linux Fortran compiler"
        iccInit=""
        ;;
esac

if [ "$ifortInit" != '' ]; then
    eval $ifortInit
    if [ $? -ne 0 ]; then
        echo 'ERROR: Initialization of the Fortran compiler fails!'
        cd $orgdir
        exit 1
    fi
fi

if [ "$iccInit" != '' ]; then
    eval $iccInit
    if [ $? -ne 0 ]; then
        echo 'ERROR: Initialization of the C compiler fails!'
        cd $orgdir
        exit 1
    fi
fi


#===============================================================================
# Use the correct Autotools

automakeModule="automake/1.14.1"
autoconfModule="autoconf/2.69"
libtoolModule="libtool/2.4.3"

initAutomake="module load $automakeModule"
initAutoconf="module load $autoconfModule"
initLibtool="module load $libtoolModule"
eval $initAutomake
if [ $? -ne 0 ]; then
    echo 'ERROR: Automake initialization fails!'
    cd $orgdir
    exit 1
fi
eval $initAutoconf
if [ $? -ne 0 ]; then
    echo 'ERROR: Autoconf initialization fails!'
    cd $orgdir
    exit 1
fi
eval $initLibtool
if [ $? -ne 0 ]; then
    echo 'ERROR: Libtool initialization fails!'
    cd $orgdir
    exit 1
fi

#===============================================================================
# Additional library settings

#---------------------
# mpich2
mpichModule=""

if [ "$compiler" = 'gnu' ]; then
    mpichModule="mpich2/3.1.4_gcc_4.9.1"
else
    # Intel compilers
    if [ "$compiler" = 'intel14' ]; then
        mpichModule="mpich2/3.1.4_intel_14.0.3"
    else
        if [ "$compiler" = 'intel16' ]; then
            mpichModule="mpich2/3.1.4_intel_16.0.3"
        fi
    fi
fi
initMpich2="module load $mpichModule"
eval $initMpich2
if [ $? -ne 0 ]; then
    echo 'ERROR: Mpich2 initialization fails!'
    cd $orgdir
    exit 1
fi


#---------------------
# PETSc
petscModule=""

if [ "$compiler" = 'gnu' ]; then
    petscModule="petsc/3.4.0_gcc4.9.1_mpich_3.1.4"
else
    # Intel compilers
    if [ "$compiler" = 'intel14' ]; then
        petscModule="petsc/3.4.0_intel14.0.3_mpich_3.1.4"
    else
        if [ "$compiler" = 'intel16' ]; then
            petscModule="petsc/3.4.0_intel16.0.3_mpich_3.1.4"
        fi
    fi
fi
initPetsc="module load $petscModule"
eval $initPetsc
if [ $? -ne 0 ]; then
    echo 'ERROR: PETSc initialization fails!'
    cd $orgdir
    exit 1
fi


#---------------------
# METIS
metisModule=""

if [ "$compiler" = 'gnu' ]; then
    metisModule="metis/5.1.0_gcc4.9.1"
else
    # Intel compilers
    if [ "$compiler" = 'intel14' ]; then
        metisModule="metis/5.1.0_intel14.0.3"
    else
        if [ "$compiler" = 'intel16' ]; then
            metisModule="metis/5.1.0_intel16.0.3"
        fi
    fi
fi
initMetis="module load $metisModule"
eval $initMetis
if [ $? -ne 0 ]; then
    echo 'ERROR: METIS initialization fails!'
    cd $orgdir
    exit 1
fi


#---------------------
# Additional compile flags
if [ "$compiler" = 'gnu' ]; then
    fflags=''
else
    # Intel compilers
    fflags=''
fi


#---------------------
# Additional link flags/libraries
if [ "$compiler" = 'gnu' ]; then
    export LDFLAGSMT_ADDITIONAL=" "
else
    # Intel compilers
    export LDFLAGSMT_ADDITIONAL="-lifcoremt"
fi


#---------------------
# netcdf
netcdfModule=""
if [ "$compiler" = 'gnu' ]; then
    netcdfModule="netcdf/v4.3.2_v4.4.0_gcc_4.9.1"
else
    # Intel compilers
    if [ "$compiler" = 'intel14' ]; then
        netcdfModule="netcdf/v4.3.2_v4.4.0_intel_14.0.3"
    else
        if [ "$compiler" = 'intel16' ]; then
            netcdfModule="netcdf/v4.6.1_v4.4.0_intel_16.0.3"
        fi
    fi
fi
initNetcdf="module load $netcdfModule"
eval $initNetcdf
if [ $? -ne 0 ]; then
    echo 'ERROR: Netcdf initialization fails!'
    cd $orgdir
    exit 1
fi

#===============================================================================
echo
echo ===========================================================================
echo "Loaded modules:"
echo "module load $initModule1"
echo "module load $initModule2"
echo "module load $fortranModule"
echo "module load $automakeModule"
echo "module load $autoconfModule"
echo "module load $libtoolModule"
echo "module load $mpichModule"
echo "module load $petscModule"
echo "module load $metisModule"
echo "module load $netcdfModule"
echo
echo "Module display of loaded modules:"
module display $fortranModule
module display $automakeModule
module display $autoconfModule
module display $libtoolModule
module display $mpichModule
module display $petscModule
module display $metisModule
module display $netcdfModule
# echo "export ACLOCAL=\"$ACLOCAL\""
# echo "export AUTOMAKE=\"$AUTOMAKE\""
# echo "export AUTOHEADER=\"$AUTOHEADER\""
# echo "export AUTOCONF=\"$AUTOCONF\""
# echo "export AUTORECONF_FLAGS=\"$AUTORECONF_FLAGS\""
# echo "export LIBTOOLIZE=\"$LIBTOOLIZE\""
# echo "export LDFLAGS=\"$LDFLAGS\""
# echo "export LDFLAGSMT_ADDITIONAL=\"$LDFLAGSMT_ADDITIONAL\""
# echo "export LD_LIBRARY_PATH=\"$LD_LIBRARY_PATH\""
# echo "export MPIFC=\"$MPIFC\""
# echo "export MPI_INCLUDE=\"$MPI_INCLUDE\""
# echo "export MPILIBS_ADDITIONAL=\"$MPILIBS_ADDITIONAL\""
# echo "export PKG_CONFIG_PATH=\"$PKG_CONFIG_PATH\""
# echo "export PATH=\"$PATH\""
echo ===========================================================================
echo


#===============================================================================
# Single precision executables require preparation before hand

if [ $useSp -eq 1 ]; then
    (
        cd utils_lgpl/deltares_common
        command='scripts/changeprecision.tcl single'
        log "Executing \"$command\" in \"$PWD\" for single-precision executables"
        eval $command
        if [ $? -ne 0 ]; then
            log 'ABORT: Single-precision script failed'
            cd $orgdir
            exit 1
        fi
    )
fi


#===============================================================================
# autogen: sanity checks, libtoolize and autoreconf

log="`pwd`/logs/autogen.log"
command="./autogen.sh --verbose &> $log"
log "Running $command in `pwd`"
eval $command
cd third_party_open/kdtree2
log "Running $command in `pwd`"
eval $command
cd ../..

if [ $? -ne 0 ]; then
    log "ERROR: Autogen fails!"
    cd $orgdir
    exit 1
fi

#===============================================================================
# configure: Create makefiles

log='logs/configure.log'

if [ $debug -eq 1 ]; then
    flags='-g -O0'
else
    flags='-O2'
fi

# fPIC is the result of the mixing of static and libtool libraries. 
# If you want to avoid this you can use convenience libraries. 
# Don't do this for non AMD64 because it will lead to worse performance. 
# More information here:
# http://www.gentoo.org/proj/en/base/amd64/howtos/index.xml?full=1#book_part1_chap3

command=" \
    CFLAGS='$flags $CFLAGS' \
    CXXFLAGS='$flags $CXXFLAGS' \
    AM_FFLAGS='$LDFLAGSMT_ADDITIONAL $AM_FFLAGS' \
    FFLAGS='$flags $fflags $FFLAGS' \
    AM_FCFLAGS='$LDFLAGSMT_ADDITIONAL $AM_FCFLAGS' \
    FCFLAGS='$flags $fflags $FCFLAGS' \
    AM_LDFLAGS='$LDFLAGSMT_ADDITIONAL $AM_LDFLAGS' \
        ./configure --prefix=`pwd` --with-mpi --with-petsc --with-metis=$METIS_DIR $configureArgs &> $log \
    "

log "Running `echo $command | sed 's/ +/ /g'`"
eval $command

if [ $? -ne 0 ]; then
    log "ERROR: Configure fails!"
    cd $orgdir
    exit 1
fi


#===============================================================================
# make: Build and install everything

if [ $noMake -eq 1 ]; then
    log "Skipping make; execute the following command before doing manual makes:"
    echo $ifortInit
    cd $orgdir
    exit 0
fi

log='logs/make.log'
command="FC=mpif90 make ds-install &> $log"

log "Running $command"
eval $command

if [ $? -ne 0 ]; then
    log "ERROR: Make fails!"
    cd $orgdir
    exit 1
fi

# Build D-Flow FM, only when not in singlePrecision mode
if [ $useSp -eq 0 ]; then
    log='logs/make_dflowfm.log'
    command="FC=mpif90 make ds-install -C engines_gpl/dflowfm &> $log"

    log "Running $command"
    eval $command
fi

if [ $? -ne 0 ]; then
    log "ERROR: Make D-Flow FM fails!"
    cd $orgdir
    exit 1
fi


#===============================================================================
# Post-install cleaning
log "Executing python script 'dimr_artifacts.py' to clean up installation directory"
log='logs/post-install.log'
command="python $maindir/engines_gpl/dimr/scripts/dimr_artifacts.py $maindir &> $log"

log "Running $command"
eval $command

if [ $? -ne 0 ]; then
    log "ERROR: post-install fails!"
    cd $orgdir
    exit 1
fi

log "Build finished"
cd $orgdir
exit 0

