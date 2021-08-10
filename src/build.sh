#! /bin/bash

#-------------------------------------------------------------------------------
#   Top-Level Build Script for Delft3D Open Source Code
#
#   There are command-line options to select Fortran compiler and debug or not.
#
#   adri.mourits@deltares.nl
#   02 Sep 2016
#
#   Copyright (C)  Stichting Deltares, 2011-2019.
#-------------------------------------------------------------------------------
#
#   WARNINGS WARNINGS WARNINGS WARNINGS WARNINGS WARNINGS WARNINGS WARNINGS
#
#   This script contains references to Deltares specific systems.
#   Use this script as an example and modify it to fit to your system.
#   See file README for compiling without using this script.
#
#   This script does not work on Mac
#   
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
# Initialize Fortran compiler

case $compiler in
    gnu)
        ifortInit=""
        iccInit=""
        addpath PATH /opt/gcc/bin
        addpath LD_LIBRARY_PATH /opt/gcc/lib /opt/gcc/lib64
        echo "Using GNU compilers in `witch gfortran`"
        ;;

    intel16)
        ifortInit=". /opt/intel/parallel_studio_cluster_2016_up3/bin/compilervars.sh $platform"
        iccInit=""
        echo "Using Intel 16.0.3 Fortran ($platform) compiler"
        ;;

    intel14)
        ifortInit=". /opt/intel/composer_xe_2013_sp1.3.174/bin/compilervars.sh $platform"
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

# When the autotools are not installed in the default location,
# point to them explicitly
addpath PATH \
    /opt/automake/bin \
    /opt/autoconf/bin \
    /opt/libtool/bin


#===============================================================================
# Additional library settings

#---------------------
# mpich2
if [ "$compiler" = 'gnu' ]; then
    addpath PATH /opt/mpich2-1.4.1-gcc-4.6.2/bin
    export MPI_INCLUDE=/opt/mpich2-1.4.1-gcc-4.6.2/include
    export MPILIBS_ADDITIONAL="-L/opt/mpich2-1.4.1-gcc-4.6.2/lib -lfmpich -lmpich -lmpl"
    # export MPILIBS_ADDITIONAL=" "
    export MPIFC=/opt/mpich2-1.4.1-gcc-4.6.2/bin/mpif90  
else
    # Intel compilers
    addpath PATH /opt/mpich2-1.0.8-intel64/bin
    export MPI_INCLUDE=/opt/mpich2-1.0.8-intel64-PIC/include
    export MPILIBS_ADDITIONAL="-L/opt/mpich2-1.0.8-intel64-PIC/lib -lfmpich -lmpich"
    if [ "$platform" = 'intel64' ]; then
        export MPIFC=/opt/mpich2-1.0.8-intel64-PIC/bin/mpif90  
    fi
fi


#---------------------
# Additional compile flags
if [ "$compiler" = 'gnu' ]; then
    fflags=''
else
    # Intel compilers
    fflags='-threads'
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
 export NETCDFROOT=/p/delft3d/opt/netcdf-4.1.3mt/intel11.1
 export PKG_CONFIG_PATH=$NETCDFROOT/lib/pkgconfig:$PKG_CONFIG_PATH
 export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$NETCDFROOT/lib

#===============================================================================
echo "Current settings:"
echo "export ACLOCAL=\"$ACLOCAL\""
echo "export AUTOMAKE=\"$AUTOMAKE\""
echo "export AUTOHEADER=\"$AUTOHEADER\""
echo "export AUTOCONF=\"$AUTOCONF\""
echo "export AUTORECONF_FLAGS=\"$AUTORECONF_FLAGS\""
echo "export LIBTOOLIZE=\"$LIBTOOLIZE\""
echo "export LDFLAGS=\"$LDFLAGS\""
echo "export LDFLAGSMT_ADDITIONAL=\"$LDFLAGSMT_ADDITIONAL\""
echo "export LD_LIBRARY_PATH=\"$LD_LIBRARY_PATH\""
echo "export MPIFC=\"$MPIFC\""
echo "export MPI_INCLUDE=\"$MPI_INCLUDE\""
echo "export MPILIBS_ADDITIONAL=\"$MPILIBS_ADDITIONAL\""
echo "export PKG_CONFIG_PATH=\"$PKG_CONFIG_PATH\""
echo "export PATH=\"$PATH\""
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
        ./configure --prefix=`pwd` $configureArgs &> $log \
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
command="make ds-install &> $log"

log "Running $command"
eval $command

# Build D-Flow FM, only when not in singlePrecision mode
if [ $useSp -eq 0 ]; then
    log='logs/make_dflowfm.log'
    command="make ds-install -C engines_gpl/dflowfm &> $log"

    log "Running $command"
    eval $command
fi

if [ $? -ne 0 ]; then
    log "ERROR: Make fails!"
    cd $orgdir
    exit 1
fi

log "Build finished"
cd $orgdir
exit 0
