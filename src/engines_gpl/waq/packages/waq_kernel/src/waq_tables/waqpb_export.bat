@echo off
set version=5.05
set serial=2015102601
if exist ..\..\..\..\..\..\tools_gpl\waqpb\packages\waqpb_export\Release\waqpb_export.exe (
    echo This will execute: waqpb_export.exe -version%version% -serial%serial%
    echo.
    echo This command will 'export' the data in the csv-tables to a proc_def,
	echo a procesm.asc and latex files for the manuals. The proc_def files
    echo will be copied to src\engines_gpl\waq\default\
    echo.
    echo Run waqpb_help.bat for more information
    echo.	
    pause
    ..\..\..\..\..\..\tools_gpl\waqpb\packages\waqpb_export\Release\waqpb_export.exe -version%version% -serial%serial%
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
