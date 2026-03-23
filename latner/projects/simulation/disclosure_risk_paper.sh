pdflatex -interaction batchmode disclosure_risk_paper.tex
echo "PDF1 =" $?

bibtex disclosure_risk_paper
echo "BIB1 =" $?

pdflatex -interaction batchmode disclosure_risk_paper.tex
echo "PDF2 =" $?

pdflatex -interaction batchmode disclosure_risk_paper.tex
echo "PDF3 =" $?

rm *.aux
rm *.bbl
rm *.blg
rm *.log
rm *.out



