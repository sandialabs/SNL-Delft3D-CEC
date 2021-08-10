@ echo off
title run_dimr_parallel
    rem When using mpich2 for the first time on a machine:
    rem Execute "smpd -install" as administrator:
    rem     Preparation: Check that your Delft3D installation contains "...\x64\share\bin\smpd.exe". Optionally copy it to a local directory (it will run as a service).
    rem     "Start" -> "All programs" -> "Accessories", right-click "Command Prompt", "Run as Administrator"
    rem     In this command box:
    rem         cd ...\x64\share\bin
    rem         smpd -install
    rem     When there is an smpd already running on the machine, it must be ended first, using the Microsoft Task Manager, 
    rem     or in the command  box: smpd -uninstall

    rem
    rem This script runs dimr in parallel mode on Windows
    rem Adapt and use it for your own purpose
    rem
    rem Usage example:
    rem Execute in the working directory:
    rem path\to\delft3d\installation\x64\dimr\scripts\run_dimr_parallel.bat
    rem More examples: check run scripts in https://svn.oss.deltares.nl/repos/delft3d/trunk/examples/*

setlocal enabledelayedexpansion
set debuglevel=-1

    rem
    rem Read arguments

    rem No arguments:
if [%1] EQU [] (
    set numpar=%NUMBER_OF_PROCESSORS%
    set argfile=dimr_config.xml
    goto readyreading
)

    rem --help:
if [%1] EQU [--help] ( goto usage )

    rem number of partitions:
set numpar=%1

    rem debuglevel and or configfile
if [%2] EQU [-d] (
    set debuglevel=%3
    if [%4] EQU [] (
        set argfile=dimr_config.xml
        goto readyreading
    ) else (
        set argfile=%4
        goto readyreading
    )
) else (
    set argfile=%2
)
if [%3] EQU [-d] (
    set debuglevel=%4
    goto readyreading
)

:readyreading

    rem Check configfile
echo Configfile:%argfile%
if not exist %argfile% (
    echo ERROR: configfile "%argfile%" does not exist
    goto usage
)

    rem Check debuglevel, translate into argument for dimr
if  %debuglevel% EQU -1 (
    set debugarg=
) else (
    set debugarg=-d !debuglevel!
)

    rem Sets the number of threads if it is not defined
if defined OMP_NUM_THREADS (
echo OMP_NUM_THREADS is already defined
) else ( 
   rem Getting and setting the number of physical cores  
   for /F "tokens=2 delims==" %%C in ('wmic cpu get NumberOfCores /value ^| findstr NumberOfCores') do set NumberOfPhysicalCores=%%C
   set /A OMP_NUM_THREADS=!NumberOfPhysicalCores! - 2
   if /I OMP_NUM_THREADS LEQ 2 ( set OMP_NUM_THREADS=2 )
)
echo OMP_NUM_THREADS is %OMP_NUM_THREADS%

echo number of partitions: %numpar%

set workdir=%CD%
echo Working directory: %workdir%
    rem
    rem Set the directories containing the binaries
    rem
set D3D_HOME=%~dp0..\..\..

    rem Remove "\dimr\scripts\..\..\.." from D3D_HOME
set D3DT=%D3D_HOME:~0,-22%
    rem last directory will be the architecture directory
for %%f in ("%D3DT%") do set ARCH=%%~nxf

set delwaqexedir=%D3D_HOME%\%ARCH%\dwaq\bin
set dflowfmexedir=%D3D_HOME%\%ARCH%\dflowfm\bin
set dimrexedir=%D3D_HOME%\%ARCH%\dimr\bin
set esmfexedir=%D3D_HOME%\%ARCH%\esmf\bin
set esmfbatdir=%D3D_HOME%\%ARCH%\esmf\scripts
set flow1dexedir=%D3D_HOME%\%ARCH%\dflow1d\bin
set flow1d2dexedir=%D3D_HOME%\%ARCH%\dflow1d2d\bin
set rrexedir=%D3D_HOME%\%ARCH%\drr\bin
set rtctoolsexedir=%D3D_HOME%\%ARCH%\drtc\bin
set swanexedir=%D3D_HOME%\%ARCH%\swan\bin
set swanbatdir=%D3D_HOME%\%ARCH%\swan\scripts
set sharedir=%D3D_HOME%\%ARCH%\share\bin
set waveexedir=%D3D_HOME%\%ARCH%\dwaves\bin


    rem
    rem No adaptions needed below
    rem

    rem Run
set PATH=%dimrexedir%;%delwaqexedir%;%dflowfmexedir%;%flow1dexedir%;%flow1d2dexedir%;%rtctoolsexedir%;%rrexedir%;%waveexedir%;%swanbatdir%;%swanexedir%;%esmfbatdir%;%esmfexedir%;%sharedir%
echo executing: "%sharedir%\mpiexec.exe" -n %numpar% -localonly "%dimrexedir%\dimr.exe" %debugarg% %argfile%
"%sharedir%\mpiexec.exe" -n %numpar% -localonly "%dimrexedir%\dimr.exe" %debugarg% %argfile%

goto end

:usage
echo Usage:
echo run_dimr_parallel.bat [--help] [n] [-d debuglevel] [dimr_config.xml]
echo     --help         : (Optional) show this usage
echo     n              : (Optional) integer, number of partitions. Must match with the prepared D-Flow FM calculation.
echo                      Default value: NUMBER_OF_PROCESSORS
echo     -d debuglevel  : (Optional) debuglevel=0:ALL, 6:SILENT
echo     dimr_config.xml: (Optional) default: dimr_config.xml

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
