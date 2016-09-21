#!/bin/bash
    #
    # This script is an example for running Delft3D-WAVE
    # Adapt and use it for your own purpose
    #
    # adri.mourits@deltares.nl
    # 28 Jun 2013
    # 
    #
    # This script starts a Delft3D-WAVE calculation on Linux
    #


    #
    # Set the config file and mdw file
    # 
mdwfile=obw.mdw




    #
    # Set the directory containing delftflow.exe
    #
export ARCH=lnx64
curdir=`pwd`
export D3D_HOME=$curdir/../../bin
waveexedir=$D3D_HOME/$ARCH/wave/bin
swanexedir=$D3D_HOME/$ARCH/swan/bin
swanbatdir=$D3D_HOME/$ARCH/swan/scripts
 
    #
    # No adaptions needed below
    #

    # Set some (environment) parameters

    # Run
export LD_LIBRARY_PATH=$swanbatdir:$swanexedir:$waveexedir:$LD_LIBRARY_PATH 
export PATH=$swanbatdir:$PATH 
$waveexedir/wave.exe $mdwfile 0

