pdflatex "DIMR - Test Plan.tex"
bibtex "DIMR - Test Plan"
pdflatex "DIMR - Test Plan.tex"
pdflatex "DIMR - Test Plan.tex" > DIMR_TP_Log.txt
xcopy DIMRL_TP_Log.txt "..\BuildLogs" /Y
xcopy *.pdf .. /Y
