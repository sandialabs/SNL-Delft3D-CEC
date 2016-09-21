@ echo off
    rem
    rem This script is an example for running Delft3D-FLOW 6.00 online with Delft3D-WAVE (Windows)
    rem Adapt and use it for your own purpose
    rem
    rem adri.mourits@deltares.nl
    rem 17 sep 2014
    rem 
    rem
    rem This script starts a single-domain Delft3D-FLOW computation online with Delft3D-WAVE on Windows
    rem


    rem
    rem Set the config file and mdw file
    rem 
set argfile=config_d_hydro.xml
set mdwfile=r17.mdw




    rem
    rem Set the directories containing the executables
    rem
set ARCH=win64
set D3D_HOME=..\..\bin
  rem set D3D_HOME=c:\Program Files (x86)\Deltares\Delft3D 4.01.00
set flowexedir=%D3D_HOME%\%ARCH%\flow2d3d\bin
set waveexedir=%D3D_HOME%\%ARCH%\wave\bin
set swanexedir=%D3D_HOME%\%ARCH%\swan\bin
set swanbatdir=%D3D_HOME%\%ARCH%\swan\scripts

    rem
    rem No adaptions needed below
    rem


    rem Start FLOW
set PATH=%flowexedir%;%PATH%
start "Hydrodynamic simulation" "%flowexedir%\d_hydro.exe" %argfile%

    rem Start WAVE
title Wave simulation
set PATH=%waveexedir%;%swanbatdir%;%swanexedir%;%PATH%
"%waveexedir%\wave.exe" %mdwfile% 1
title %CD%

    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
