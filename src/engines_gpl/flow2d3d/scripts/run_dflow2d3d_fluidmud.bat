@ echo off
title run_flow2d3d_fluidmud
    rem
    rem This script runs two Delft3D-FLOW instances on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

set waterconfigfile=config_d_hydro_sed.xml
set mudconfigfile=config_d_hydro_mud.xml
    rem
    rem Set the config files
    rem
if [%1] EQU [] (
    rem Default config names are used
) else (
    if [%1] EQU [-wconfig] (
        if [%2] EQU [] (
            goto usage
        )
        set waterconfigfile=%2
        if [%3] EQU [-mconfig] (
            if [%4] EQU [] (
                goto usage
            )
            set mudconfigfile=%4
        )
    ) else (
        if [%1] EQU [--help] (
            goto usage
        ) else (
            if [%1] EQU [-mconfig] (
                if [%2] EQU [] (
                    goto usage
                )
                set mudconfigfile=%2
                if [%3] EQU [-wconfig] (
                    if [%4] EQU [] (
                        goto usage
                    )
                    set waterconfigfile=%4
                )
            ) else (
                goto usage
            )
        )
    )
)
echo Water configfile:%waterconfigfile%
if not exist %waterconfigfile% (
    echo ERROR: configfile "%waterconfigfile%" does not exist
    goto usage
)
echo Mud configfile:%mudconfigfile%
if not exist %mudconfigfile% (
    echo ERROR: configfile "%mudconfigfile%" does not exist
    goto usage
)


set workdir=%CD%
echo Working directory: %workdir%
    rem
    rem Set the directories containing the binaries
    rem
set D3D_HOME=%~dp0..\..\..

rem Remove "\dflow2d3d\scripts\..\..\.." from D3D_HOME
set D3DT=%D3D_HOME:~0,-27%
rem last directory will be the architecture directory
for %%f in ("%D3DT%") do set ARCH=%%~nxf

set dflow2d3ddir=%D3D_HOME%\%ARCH%\dflow2d3d\bin
set sharedir=%D3D_HOME%\%ARCH%\share\bin


    rem
    rem No adaptions needed below
    rem

    rem Start FLOW for water phase
set PATH=%dflow2d3ddir%;%sharedir%
echo executing in separate window: "%dflow2d3ddir%\d_hydro.exe" %waterconfigfile%
start "Delft3D-FLOW water" "%dflow2d3ddir%\d_hydro.exe" %waterconfigfile%

    rem Start FLOW for mud phase
title Delft3D-FLOW mud simulation
set PATH=%dflow2d3ddir%;%sharedir%
echo executing in this window: "%dflow2d3ddir%\d_hydro.exe" %mudconfigfile%
"%dflow2d3ddir%\d_hydro.exe" %mudconfigfile%
title %CD%

goto end

:usage
echo Usage:
echo run_dflow2d3d_fluidmud.bat [--help] [-wconfig config_d_hydro_sed.xml] [-mconfig config_d_hydro_mud.xml]
echo     --help            : (Optional) show this usage
echo     -wconfig          : (Optional) config file for water phase, default: config_d_hydro_sed.xml
echo     -mconfig          : (Optional) config file for mud   phase, default: config_d_hydro_mud.xml

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
