#!/bin/bash
    #
    # This script is an example for running 
    # 
    # DELWAQ D-Water Quality
    #
    # on Linux.
    #
    # Adapt and use it for your own purpose
    #
    # michel.jeuken@deltares.nl
    # 17 Oct 2014
    # 


    #
    # Set the mdp file here
    # 
mdpfile=fti_tracer.mdp

currentdir=`pwd`
echo $currentdir
argfile=$currentdir/$mdpfile

    #
    # Set the directory containing delpar
    #
exedir=$currentdir/../../bin/lnx64/part/bin
export LD_LIBRARY_PATH=$exedir:$LD_LIBRARY_PATH 

    #
    # Run delpar
    #
$exedir/delpar $argfile
