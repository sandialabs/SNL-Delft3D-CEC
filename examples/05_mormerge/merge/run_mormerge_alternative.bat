@ echo off

    rem
    rem This is an alternative way to start a mormerge calculation on Windows.
    rem It does not use the script "mormerge.tcl" to start all processes.
    rem Disadvantage is that you have to prepare the calculation manually.
    rem Only use this script if you are an experienced user.
    rem
    rem Needed manual preparations:
    rem - Create the condition subdirectories and copy the (correct) input files in it
    rem - Adapt this script for your environment and model. Currently, it is tuned for:
    rem   https://svn.oss.deltares.nl/repos/delft3d/trunk/examples/05_mormerge
    rem
    rem adri.mourits@deltares.nl
    rem

    rem
    rem
set D3D_HOME=..\..\..\src\bin
set ARCH=x64

    rem
    rem To make this script better readable
set workdir=%~dp0..
cd %D3D_HOME%\%ARCH%
set exedir=%CD%
set condition1=0deg
set condition2=45deg
set subdomain1=det
set subdomain2=ove
set mdwfile=bas.mdw
echo workdir:%workdir%
echo exedir:%exedir%

    rem
    rem Prepare environment and logdir
set PATH=%exedir%\swan\scripts;%exedir%\swan\bin;%exedir%\dwaves\bin;%exedir%\dflow2d3d\bin;%exedir%\share\bin;%PATH%
rmdir /S /Q %workdir%\merge\sync
mkdir %workdir%\merge\sync


    rem
    rem Start 1 mormerge for each subdomain
cd /D %workdir%\merge
start /B %exedir%\dflow2d3d\bin\mormerge.exe -i basin_windows.mm -w . -r %subdomain1% >mormerge_%subdomain1%.scr 2>&1
start /B %exedir%\dflow2d3d\bin\mormerge.exe -i basin_windows.mm -w . -r %subdomain2% >mormerge_%subdomain2%.scr 2>&1


    rem
    rem Start FLOW/WAVE for condition1
cd /D %workdir%\%condition1%
echo ..\merge\%condition1%stream >streamfile
start /B %exedir%\dwaves\bin\wave.exe %mdwfile% 1 >wave.scr 2>&1
start /B %exedir%\dflow2d3d\bin\d_hydro.exe config_d_hydro.xml >d_hydro.scr 2>&1



    rem
    rem Start FLOW/WAVE condition2
cd /D %workdir%\%condition2%
echo ..\merge\%condition2%stream >streamfile
start /B %exedir%\dwaves\bin\wave.exe %mdwfile% 1 >wave.scr 2>&1
start /B %exedir%\dflow2d3d\bin\d_hydro.exe config_d_hydro.xml >d_hydro.scr 2>&1

cd %~dp0

echo Waiting until all processes are finished
    rem pause
