@ echo off
    rem
    rem This script is an example for running Delft3D-FLOW
    rem Adapt and use it for your own purpose
    rem
    rem adri.mourits@deltares.nl
    rem 17 sep 2014
    rem 
    rem
    rem This script starts a single-domain Delft3D-FLOW computation on Windows
    rem


    rem
    rem Set the config file here
    rem 
set argfilesed=config_d_hydro_sed.xml
set argfilemud=config_d_hydro_mud.xml





    rem
    rem Set the directory containing delftflow.exe here
    rem
set ARCH=win64
set D3D_HOME=..\..\bin
  rem set D3D_HOME=c:\Program Files (x86)\Deltares\Delft3D 4.01.00
set exedir=%D3D_HOME%\%ARCH%\flow2d3d\bin

    rem
    rem No adaptions needed below
    rem

    rem Set some (environment) parameters
set PATH=%exedir%;%PATH%

    rem Run
start %exedir%\d_hydro.exe %argfilesed%
"%exedir%\d_hydro.exe" %argfilemud%


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
