@ echo off
    rem
    rem This script is an example for running Delft3D-FLOW
    rem Adapt and use it for your own purpose
    rem
    rem edwin.spee@deltares.nl
    rem adri.mourits@deltares.nl
    rem 17 sep 2014
    rem 
    rem
    rem This script starts on Windows a parallel single-domain Delft3D-FLOW 
    rem computation online with Delft3D-WAVE
    rem
    rem When using mpich2 for the first time on a machine:
    rem Execute "smpd -install"
    rem 

    rem
    rem Set the config file and mdw file here
    rem 
set argfile=config_d_hydro.xml
set mdwfile=r17.mdw




    rem
    rem Set the directory containing ALL exes/dlls here (mpiexec.exe, delftflow.exe, flow2d3d.dll, mpich-dlls, DelftOnline dlls etc.)
    rem
set ARCH=win64
set D3D_HOME=..\..\bin
  rem set D3D_HOME=c:\Program Files (x86)\Deltares\Delft3D 4.01.00
set flowexedir=%D3D_HOME%\%ARCH%\flow2d3d\bin
rem set flowexedir=d:\src\trunk_os\src\engines_gpl\d_hydro\bin\Debug
set waveexedir=%D3D_HOME%\%ARCH%\wave\bin
set swanexedir=%D3D_HOME%\%ARCH%\swan\bin
set swanbatdir=%D3D_HOME%\%ARCH%\swan\scripts

    rem
    rem No adaptions needed below
    rem

    rem Set some (environment) parameters
set PATH=%flowexedir%;%PATH%
    rem mpiexec is in %flowexedir%
    rem For some users, it is necessary to use the locally installed mpiexec:
set MPIPATH=%flowexedir%
    rem set MPIPATH="C:\Program Files (x86)\Common Files\Intel\Shared Libraries\redist\intel64\mpirt"


    rem Run
    rem start computation on all your local cores (2 for dual core; 4 for quad core etc.)
    rem note the flag "-localonly" which may be needed to avoid "Aborting: unable to connect to machinename.local"
start "Hydrodynamic simulation" "%MPIPATH%\mpiexec" -n %NUMBER_OF_PROCESSORS% -localonly "%flowexedir%\d_hydro.exe" %argfile%

title Wave simulation
set PATH=%waveexedir%;%swanbatdir%;%swanexedir%;%PATH%
"%waveexedir%\wave.exe" %mdwfile% 1
title %CD%


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
