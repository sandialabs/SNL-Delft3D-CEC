#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs Delft3D-FLOW in parallel online with Delft3D-WAVE on Linux
    # Adapt and use it for your own purpose
    #

function print_usage_info {
    echo "Usage: ${0##*/} n -w <mdw-file> [OPTION]..."
    echo "Run a Delft3D-FLOW model in parallel online with Delft3D-WAVE on Linux."
    echo
    echo "n: integer, number of partitions"
    echo "-w, --mdwfile <mdw-file>"
    echo "        Name of mdw-file"
    echo
    echo "Options:"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo "--dockerparallel"
    echo "       A parallel run inside docker"
    echo "<filename>"
    echo "       Delft3D-FLOW configuration filename, default config_d_hydro.xml"
    exit 1
}


# ============
# === MAIN ===
# ============

#
## Defaults
NPART=0
configfile=config_d_hydro.xml
mdwfile=
dockerprl=0
D3D_HOME=
ulimit -s unlimited


#
## Start processing command line options:

NPART="$1"
shift
while [[ $# -ge 1 ]]
do
key="$1"
shift

case $key in
    -h|--help)
    print_usage_info
    ;;
    -w|--mdwfile)
    mdwfile="$1"
    shift
    ;;
    --dockerparallel)
    dockerprl=1
    ;;
    --D3D_HOME)
    D3D_HOME="$1"
    shift
    ;;
    --NNODES)
    NNODES="$1"
    shift
    ;;
    *)
    configfile="$key"
    break
    ;;
esac
done


if [ ! -f $configfile ]; then
    echo "ERROR: configfile $configfile does not exist"
    print_usage_info
fi
if [ ! -f $mdwfile ]; then
    echo "ERROR: mdwfile $mdwfile does not exist"
    print_usage_info
fi



workdir=`pwd`

if [ -z "${D3D_HOME}" ]; then
    scriptdirname=`readlink \-f \$0`
    scriptdir=`dirname $scriptdirname`
    D3D_HOME=$scriptdir/..
else
    # D3D_HOME is passed through via argument --D3D_HOME
    # Commonly its value is "/some/path/bin/.."
    # Scriptdir: remove "/.." at the end of the string
    scriptdir=${D3D_HOME%"/.."}
fi
if [ ! -d $D3D_HOME ]; then
    echo "ERROR: directory $D3D_HOME does not exist"
    print_usage_info
fi
export D3D_HOME
 
echo "    Configfile       : $configfile"
echo "    mdw-file         : $mdwfile"
echo "    D3D_HOME         : $D3D_HOME"
echo "    Working directory: $workdir"
echo "    nr of parts      : $NPART"
echo "    Docker parallel  : $dockerprl"
echo 

    #
    # Set the directories containing the binaries
    #

bindir=$D3D_HOME/bin
libdir=$D3D_HOME/lib



export LD_LIBRARY_PATH=$libdir:$LD_LIBRARY_PATH
export PATH="$bindir:${PATH}"
export NHOSTS=1

if [ $dockerprl -eq 1 ]; then
    #
    # Parallel in Docker
    # Assumption: 1 node
    export PATH=/usr/lib64/mpich/bin:$PATH
    echo "Starting mpd..."
    mpd &
    mpdboot -n $NPART --rsh=/usr/bin/rsh

    node_number=$NPART
    while [ $node_number -ge 1 ]; do
       node_number=`expr $node_number - 1`
       ln -s /dev/null log$node_number.irlog
    done

    echo "executing in the background:"
    echo "mpirun -np $NPART $bindir/d_hydro $configfile &"
          mpirun -np $NPART $bindir/d_hydro $configfile &

    echo "executing in the foreground:"
    echo "$bindir/wave $mdwfile 1"
          $bindir/wave $mdwfile 1
else 
    #
    # Not in Docker
    export PATH="/opt/mpich2/bin:${PATH}"
    # Start mpi
    echo " ">$(pwd)/machinefile
    mpd &
    mpdboot -n $NHOSTS

    # link mpich debug rubbish to /dev/null
    node_number=$NPART
    while test $node_number -ge 1
    do
       node_number=`expr $node_number - 1`
       ln -s /dev/null log$node_number.irlog
    done

    # Run
    echo "executing in the background:"
    echo "mpirun -np $NPART $bindir/d_hydro $configfile &"
    echo 
    mpirun -np $NPART $bindir/d_hydro $configfile &

    echo "executing in the foreground:"
    echo "$bindir/wave $mdwfile 1"
          $bindir/wave $mdwfile 1

    rm -f log*.irlog
fi


if [[ $dockerprl -eq 1 ]]; then
    mpdallexit
fi

    # Wait until all child processes are finished
wait

    # Nefis files don't get write permission for the group bit
    # Add it explicitly, only when stderr = 0
if [ $? -eq 0 ]; then
    chmod -R g+rw *.dat *.def &>/dev/null || true
fi
