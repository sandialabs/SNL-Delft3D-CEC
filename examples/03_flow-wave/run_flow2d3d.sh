#!/bin/bash
    #
    # This script is an example for running Delft3D-FLOW
    # Adapt and use it for your own purpose
    #
    # adri.mourits@deltares.nl
    # 01 Mar 2011
    # 
    #
    # This script starts a single-domain Delft3D-FLOW computation online with Delft3D-WAVE on Linux
    #


    #
    # Set the config file and mdw file
    # 
argfile=config_d_hydro.xml
mdwfile=r17.mdw




    #
    # Set the directory containing delftflow.exe
    #
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

    # Set some (environment) parameters

    # Run
export LD_LIBRARY_PATH=$flowexedir:$LD_LIBRARY_PATH 
export PATH=$flowexedir:$PATH 
$flowexedir/d_hydro.exe $argfile &

export LD_LIBRARY_PATH=$swanbatdir:$swanexedir:$waveexedir:$LD_LIBRARY_PATH 
export PATH=$swanbatdir:$PATH 
$waveexedir/wave.exe $mdwfile 1

