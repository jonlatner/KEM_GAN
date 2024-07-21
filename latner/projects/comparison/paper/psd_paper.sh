pdflatex -interaction batchmode psd_paper.tex
echo "PDF1 =" $?

bibtex psd_paper
echo "BIB1 =" $?

pdflatex -interaction batchmode psd_paper.tex
echo "PDF2 =" $?

pdflatex -interaction batchmode psd_paper.tex
echo "PDF3 =" $?

rm *.aux
rm *.bbl
rm *.blg
rm *.log
rm *.out



