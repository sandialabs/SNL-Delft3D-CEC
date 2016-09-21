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
    rem This script starts a single-domain Delft3D-FLOW computation on Windows
    rem parallel
    rem
    rem When using mpich2 for the first time on a machine:
    rem Execute "smpd -install" as administrator:
    rem     "Start" -> "All programs" -> "Accessories", right-click "Command Prompt", "Run as Administrator"
    rem     In this command box:
    rem         cd ..\..\bin\win64\flow2d3d\bin
    rem         (or when not compiled yet)
    rem         cd ..\..\src\third_party_open\mpich2\bin
    rem         (both paths above are relative to the location of this script "run_flow2d3d_parallel.bat")
    rem         smpd -install
    rem     When there is an smpd already running on the machine, it must be ended first, using the Microsoft Task Manager



    rem
    rem Set the config file here
    rem 
set argfile=config_d_hydro.xml





    rem
    rem Set the directory containing ALL exes/dlls here (mpiexec.exe, delftflow.exe, flow2d3d.dll, mpich-dlls, DelftOnline dlls etc.)
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
    rem mpiexec is in %exedir%
    rem For some users, it is necessary to use the locally installed mpiexec:
set MPIPATH=%exedir%
    rem set MPIPATH="C:\Program Files (x86)\Common Files\Intel\Shared Libraries\redist\intel64\mpirt"


    rem Run
    rem start computation on all your local cores (2 for dual core; 4 for quad core etc.)
    rem note the flag "-localonly" which may be needed to avoid "Aborting: unable to connect to machinename.local"
set NPROC=%NUMBER_OF_PROCESSORS%
if %NPROC% gtr 5 set NPROC=5
"%MPIPATH%\mpiexec" -n %NPROC% -localonly "%exedir%\d_hydro.exe" %argfile%


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
