pdflatex "DIMR - Technical Design.tex"
bibtex "DIMR - Technical Design"
pdflatex "DIMR - Technical Design.tex"
pdflatex "DIMR - Technical Design.tex" > DIMR_TD_Log.txt
xcopy DIMR_TD_Log.txt "..\BuildLogs" /Y
xcopy *.pdf .. /Y
