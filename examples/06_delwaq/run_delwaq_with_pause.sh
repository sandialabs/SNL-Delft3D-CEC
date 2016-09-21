#!/bin/bash
    #
    # This script is an example for running DELWAQ D-Water Quality
    # Adapt and use it for your own purpose
    #
    # michel.jeuken@deltares.nl
    # 11 Mrt 2013
    # 
    #
    # This script starts 3D DELWAQ D-Water Quality computation on Linux
    #


    #
    # Set the config file here
    # 
inpfile=com-tut_fti_waq.inp

currentdir=`pwd`
echo $currentdir
argfile=$currentdir/$inpfile

    #
    # Set the directory containing delwaq1 and delwaq2 and
    # the directory containing the proc_def and bloom files here
    #
exedir=$currentdir/../../bin/lnx64/waq/bin
export LD_LIBRARY_PATH=$exedir:$LD_LIBRARY_PATH 
procfile=$currentdir/../../bin/lnx64/waq/default/proc_def

    #
    # Run delwaq 1
    #
$exedir/delwaq1 $argfile -p "$procfile"

    #
    # Wait for any key to run delwaq 2
    #
if [ $? == 0 ]
  then
    echo ""
    echo "Delwaq1 did run without errors."
    echo ""
    read -p "Press enter to continue with delwaq2 . . ."

    #
    # Run delwaq 2
    #
    echo ""
    $exedir/delwaq2 $argfile

    if [ $? -eq 0 ]
      then
        echo ""
        echo "Delwaq2 did run without errors."
      else
        echo ""
        echo "Delwaq2 did not run correctly."
    fi
else
    echo ""
    echo "Delwaq1 did not run correctly, ending calculation"
fi

    #
    # Prevent the script from disappearing immediately
    #
echo ""
read -p "Press enter to exit run_delwaq . . ."
