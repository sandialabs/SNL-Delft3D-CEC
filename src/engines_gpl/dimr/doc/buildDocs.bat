mkdir "BuildLogs"

cd "DIMR - Functional Design"
call buildDoc.bat
cd ..

cd "DIMR - Manual"
call buildDoc.bat
cd ..

cd "DIMR - Technical Design"
call buildDoc.bat
cd ..

cd "DIMR - Technical Documentation"
call buildDoc.bat
cd ..

cd "DIMR - Technical Design"
call buildDoc.bat
cd ..

cd "DIMR - Test Plan"
call buildDoc.bat
cd ..

cd "DIMR - Test Report"
call buildDoc.bat
cd ..

python parseLogs.py
