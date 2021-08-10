pdflatex "DIMR - Functional Design.tex"
bibtex "DIMR - Functional Design"
pdflatex "DIMR - Functional Design.tex"
pdflatex "DIMR - Functional Design.tex" > DIMR_FD_Log.txt
xcopy DIMR_Log.txt "..\BuildLogs" /Y
xcopy *.pdf .. /Y
