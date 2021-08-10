@echo off
echo.
echo About the DELWAQ proces library tools
echo.
echo Please build the waq proces library tools first. You will find
echo then in src\tools_gpl\waqpb\waqpb.sln or under tools_gpl-waqpb
echo in the main visual studio solution
echo.
echo To modify/add to the proces library:
echo   - first run waq_pb_export.bat. This will 'export' the data in
echo     the csv-tables to a proc_def, a procesm.asc and latex files
echo     for the manuals.
echo   - copy procesm.asc to proces.asc, modify the latter to include
echo     the midifications you want
echo   - run waqpb_import.bat to import changes into the csv-files
echo   - modify version information in waqpb_export.bat
echo   - run waq_pb_export.bat again to create a new proc_def, new 
echo     tables, and a new procesm.asc to see if proces.asc was
echo     imported correctly
echo.
echo P.S.: It is also possible to make (usually small) changes in the
echo       csv-files direclty. Then ou just have use waq_pb_export.bat
echo.
pause
