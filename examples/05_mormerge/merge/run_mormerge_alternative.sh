#! /bin/bash

    #
    # This is an alternative way to start a mormerge calculation on Windows.
    # It does not use the script "mormerge.tcl" to start all processes.
    # Disadvantage is that you have to prepare the calculation manually.
    # Only use this script if you are an experienced user.
    #
    # Needed manual preparations:
    # - Create the condition subdirectories and copy the (correct) input files in it
    # - Adapt this script for your environment and model. Currently, it is tuned for:
    #   https://svn.oss.deltares.nl/repos/delft3d/trunk/examples/05_mormerge
    #
    # adri.mourits@deltares.nl
    # 25 Aug 2015
    #

curdir=`pwd`
cd ../../../bin
export D3D_HOME=`pwd`
cd $curdir

export ARCH=lnx64

    #
    # To make this script better readable
scriptdirname=`readlink \-f \$0`
workdir=`dirname $scriptdirname`
cd $workdir/..
workdir=`pwd`
cd $D3D_HOME/$ARCH
exedir=`pwd`
condition1=0deg
condition2=45deg
subdomain1=det
subdomain2=ove
mdwfile=bas.mdw
echo workdir:$workdir
echo exedir:$exedir

    #
    # Prepare environment and logdir
export LD_LIBRARY_PATH=$exedir/swan/scripts:$exedir/swan/bin:$exedir/wave/bin:$exedir/flow2d3d/bin:$LD_LIBRARY_PATH 
export PATH=$exedir/swan/scripts:$exedir/swan/bin:$exedir/wave/bin:$exedir/flow2d3d/bin:$PATH
rm -rf $workdir/merge/sync
mkdir $workdir/merge/sync


    #
    # Start 1 mormerge for each subdomain
echo starting 2 mormerge executables ...
cd $workdir/merge
$exedir/flow2d3d/bin/mormerge.exe -i basin_linux.mm -w . -r $subdomain1 >mormerge_$subdomain1.scr 2>&1 &
$exedir/flow2d3d/bin/mormerge.exe -i basin_linux.mm -w . -r $subdomain2 >mormerge_$subdomain2.scr 2>&1 &


    #
    # Start FLOW/WAVE for condition1
echo starting FLOW and WAVE in $condition1 ...
cd $workdir/$condition1
subname=stream
echo ../merge/$condition1$subname >streamfile
$exedir/wave/bin/wave.exe $mdwfile 1 >wave.scr 2>&1 &
$exedir/flow2d3d/bin/d_hydro.exe config_d_hydro.xml >d_hydro.scr 2>&1 &



    #
    # Start FLOW/WAVE condition2
echo starting FLOW and WAVE in $condition2 ...
cd $workdir/$condition2
subname=stream
echo ../merge/$condition2$subname >streamfile
$exedir/wave/bin/wave.exe $mdwfile 1 >wave.scr 2>&1 &
$exedir/flow2d3d/bin/d_hydro.exe config_d_hydro.xml >d_hydro.scr 2>&1 &

    # Wait until all child processes are finished
wait
