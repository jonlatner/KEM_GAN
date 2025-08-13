pdflatex -interaction batchmode drechsler_latner_2023.tex
echo "PDF1 =" $?

pdflatex -interaction batchmode drechsler_latner_2023.tex
echo "PDF2 =" $?

rm *.log
rm *.nav
rm *.snm
rm *.toc
rm *.upa
rm *.aux
rm *.out



