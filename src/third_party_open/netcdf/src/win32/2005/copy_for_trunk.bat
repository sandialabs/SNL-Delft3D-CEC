echo @OFF

rem Copy the NETCDF files which need to be linked/included by program of the /ds/trunk

rem Copying the release version

copy .\libsrc\release\netcdf.dll ..\..\..\lib\win32\release\*.*
copy .\libsrc\release\netcdf.lib ..\..\..\lib\win32\release\*.*
copy .\f77_netcdf\release\f77_netcdf.lib ..\..\..\lib\win32\release\*.*
copy .\f90_netcdf\release\f90_netcdf.lib ..\..\..\lib\win32\release\*.*
copy .\f90_netcdf\release\netcdf.mod ..\..\..\lib\win32\release\*.*

rem Copying the debug version

copy .\libsrc\debug\netcdf.dll ..\..\..\lib\win32\debug\*.*
copy .\libsrc\debug\netcdf.lib ..\..\..\lib\win32\debug\*.*
copy .\f77_netcdf\debug\f77_netcdf.lib ..\..\..\lib\win32\debug\*.*
copy .\f90_netcdf\debug\f90_netcdf.lib ..\..\..\lib\win32\debug\*.*
copy .\f90_netcdf\debug\netcdf.mod ..\..\..\lib\win32\debug\*.*
