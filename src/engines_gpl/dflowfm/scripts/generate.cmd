set SAVEDIR=%CD%
cd ..\packages\dflowfm_lib\include\
python ..\..\..\scripts\template\generate.py ..\..\dflowfm_kernel\src\partition.F90 ..\..\dflowfm_kernel\src\modules.f90 ..\..\..\..\..\utils_lgpl\gridgeom\packages\gridgeom\src\network_data.f90 ..\..\dflowfm_kernel\src\d_flooding.f90 ..\..\dflowfm_kernel\src\monitoring.f90 ..\..\..\..\..\utils_lgpl\gridgeom\packages\gridgeom\src\generalmodules.f90 --template-dir=..\..\dflowfm_lib\templates\ --verbose
cd %SAVEDIR%
