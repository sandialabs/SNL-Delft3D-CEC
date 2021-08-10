@echo off
set toolfound=false
set toolx64=false

if exist ..\..\..\..\..\..\tools_gpl\waqpb\packages\waqpb_import\Release\waqpb_import.exe     set toolfound=true
if exist ..\..\..\..\..\..\tools_gpl\waqpb\packages\waqpb_import\x64\Release\waqpb_import.exe set toolfound=true
if exist ..\..\..\..\..\..\tools_gpl\waqpb\packages\waqpb_import\x64\Release\waqpb_import.exe set toolx64=true

if %toolfound%==true  (
    echo This will execute: waqpb_import.exe
    echo.
    echo This command will 'import' changes from the proces.asc file.
	echo Make sure it exists!
    echo.
    echo Run waqpb_help.bat for more information
    echo.
    pause
    if %toolx64%==true ..\..\..\..\..\..\tools_gpl\waqpb\packages\waqpb_import\x64\Release\waqpb_import.exe
    if not %toolx64%==true ..\..\..\..\..\..\tools_gpl\waqpb\packages\waqpb_import\Release\waqpb_import.exe
    copy proc_def.* ..\..\..\..\default
    echo.
    pause
) else (
    echo.
    echo Please build the waq proces library tools first.
	echo You will find then in src\tools_gpl\waqpb\waqpb.sln
    echo or under tools_gpl - waqpb in the main solution
    echo.
    echo Run waqpb_help.bat for more information
	echo.
	pause
)
