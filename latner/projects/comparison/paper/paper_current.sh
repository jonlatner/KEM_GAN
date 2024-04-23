pdflatex -interaction batchmode paper_current.tex
echo "PDF1 =" $?

bibtex paper_current
echo "BIB1 =" $?

pdflatex -interaction batchmode paper_current.tex
echo "PDF2 =" $?

pdflatex -interaction batchmode paper_current.tex
echo "PDF3 =" $?

rm *.aux
rm *.bbl
rm *.blg
rm *.log
rm *.out



