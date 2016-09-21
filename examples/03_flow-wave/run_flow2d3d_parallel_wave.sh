#!/bin/bash

    # Known bug:
    # See http://oss.deltares.nl/web/delft3d/release-notes#3058

    #
    # This script starts a single-domain Delft3D-FLOW(6.00) computation (Linux) in parallel mode,
    # running online with a Delft3D-WAVE
    #
    # !!!!! IMPORTANT !!!!!
    # When using mpich2 for the first time:
    # In case the error "unable to find mpd.conf" occurs:
    # Your home directory MUST contain file .mpd.conf with contents:
    # secretword=bla
    # and with file access permissions:
    # -r--------
    #
    # adri.mourits@deltares.nl
    # menno.genseberger@deltares.nl
    # 29 Oct 2013 

export NHOSTS=1
export processes_per_node=4


    #
    # Set the config file and mdw file
argfile=config_d_hydro.xml
mdwfile=r17.mdw


    #
    # Set the directories containing d_hydro, wave, swan, swan.sh here
export ARCH=lnx64
curdir=`pwd`
export D3D_HOME=$curdir/../../bin
flowexedir=$D3D_HOME/$ARCH/flow2d3d/bin
waveexedir=$D3D_HOME/$ARCH/wave/bin
swanexedir=$D3D_HOME/$ARCH/swan/bin
swanbatdir=$D3D_HOME/$ARCH/swan/scripts 

    #
    # No adaptions needed below
    #

    #
    # Set some (environment) parameters
    # Use the same mpich2 version, as used during the compilation
export PATH="/opt/mpich2-1.0.8-intel64/bin:${PATH}"
export NSLOTS=`expr $NHOSTS \* $processes_per_node`


echo Contents of machinefile:
cat $(pwd)/machinefile
echo ----------------------------------------------------------------------


    #
    # Start mpich2

    ### The use of hydra instead of mpd is adviced. hydra is the default for mpich2 version 1.4.1
    ### From the README:
    ### hydra
    ### -----
    ### Hydra is the default process management framework that uses existing 
    ### daemons on nodes (e.g., ssh, pbs, slurm, sge) to start MPI processes. 
    ### More information on Hydra can be found at 
    ### http://wiki.mcs.anl.gov/mpich2/index.php/Using_the_Hydra_Process_Manager
    ### 
    ### mpd
    ### ---
    ### MPD was the traditional process manager in MPICH2. The file 
    ### mpich2-1.4.1/src/pm/mpd/README has more information about interactive commands 
    ### for managing the ring of MPDs. The MPD process manager is now deprecated.
    # Deltares cluster: mpd is already started
mpd &

    # mpdboot: optional: --ncpus=$processes_per_node (adapted machinefile needed)
mpdboot -n $NHOSTS -f $(pwd)/machinefile --rsh=/usr/bin/rsh


    #
    # link mpich debug rubbish to /dev/null
node_number=$NSLOTS
while [ $node_number -ge 1 ]; do
   node_number=`expr $node_number - 1`
   ln -s /dev/null log$node_number.irlog
done


    #
    # Start FLOW
export LD_LIBRARY_PATH=$flowexedir:$LD_LIBRARY_PATH
mpirun -np $NSLOTS $flowexedir/d_hydro.exe $argfile &

    #
    # Start WAVE
export LD_LIBRARY_PATH=$swanbatdir:$swanexedir:$waveexedir:$LD_LIBRARY_PATH 
export PATH=$swanbatdir:$PATH 
$waveexedir/wave.exe $mdwfile 1

    #
    # Clean up, finish MPICH2 network
rm -f log*.irlog
mpdallexit 

