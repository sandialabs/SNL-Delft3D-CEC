pdflatex "DIMR - Test Report.tex"
bibtex "DIMR - Test Report"
pdflatex "DIMR - Test Report.tex"
pdflatex "DIMR - Test Report.tex" > DIMR_TR_Log.txt
xcopy DIMR_TR_Log.txt "..\BuildLogs" /Y
xcopy *.pdf .. /Y
