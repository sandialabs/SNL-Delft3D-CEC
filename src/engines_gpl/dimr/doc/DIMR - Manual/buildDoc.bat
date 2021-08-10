pdflatex "DIMR - Manual.tex"
bibtex "DIMR - Manual"
pdflatex "DIMR - Manual.tex"
pdflatex "DIMR - Manual.tex" > DIMR_M_Log.txt
xcopy DIMR_M_Log.txt "..\BuildLogs" /Y
xcopy *.pdf .. /Y
