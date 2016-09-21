#!/bin/bash
    #
    # This script starts a Delft3D-FLOW (4.00.00 or higher) computation on Linux
    # running a Fluid Mud calculation
    #


    #
    # Specify the config files to be used here
    # 
argfilesed=config_d_hydro_sed.xml
argfilemud=config_d_hydro_mud.xml

    #
    # Set the directory containing deltares_hydro.exe and libflow2d3d.so here
    #
export ARCH=lnx64
export D3D_HOME=../../bin
exedir=$D3D_HOME/$ARCH/flow2d3d/bin
useSharedMem=1




 

    #
    # No adaptions needed below
    #

    # Set some (environment) parameters
export LD_LIBRARY_PATH=$exedir:$LD_LIBRARY_PATH 


    #
    # Create shared memory block
if [ $useSharedMem -eq 1 ]; then
    esmContext=`$exedir/esm_create 2> /dev/null`
    if [ $? -eq 0 ]; then
        # use context
        DIO_SHM_ESM=$esmContext
        export DIO_SHM_ESM
        echo "ESM context created: $esmContext"
    else
        echo "Cannot create ESM context:"
        $exedir/esm_create
    fi
fi

    #
    # Run
$exedir/d_hydro.exe $argfilesed &
    #echo press enter to continue
    #read dummy
echo waiting 5 seconds
sleep 5
$exedir/d_hydro.exe $argfilemud

    #
    # delete context
if [ $useSharedMem -eq 1 ]; then
    $flowdir/esm_delete $esmContext 2> /dev/null
    if [ $? -ne 0 ]; then
        echo "Cannot delete ESM context $esmContext:"
        $exedir/esm_delete $esmContext
    else
        echo "ESM context deleted: $esmContext"
    fi
fi


