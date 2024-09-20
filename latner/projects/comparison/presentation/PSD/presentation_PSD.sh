pdflatex -interaction batchmode presentation_PSD.tex
echo "PDF1 =" $?

pdflatex -interaction batchmode presentation_PSD.tex
echo "PDF2 =" $?

rm *.log
rm *.nav
rm *.snm
rm *.toc
rm *.upa
rm *.aux
rm *.out




