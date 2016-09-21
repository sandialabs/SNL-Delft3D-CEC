@ echo off
    rem
    rem This script is an example for running
    rem
    rem Delft3D D-Water Quality Particle Tracking
    rem
    rem on Windows.
    rem
    rem Adapt and use it for your own purpose
    rem
    rem michel.jeuken@deltares.nl
    rem 17 Oct 2014
    rem 

    rem
    rem Set the mdp file here
    rem 
set mdpfile=fti_tracer.mdp

set currentdir=%~dp0%
set argfile=%currentdir%%mdpfile%

    rem
    rem Set the directory containing delpar.exe
    rem
set exedir=%currentdir%..\..\bin\win64\part\bin

    rem
    rem Run delpar
    rem
echo.
"%exedir%\delpar.exe" "%argfile%" 
echo.
