pdflatex -interaction batchmode presentation_aniged_2025.tex
echo "PDF1 =" $?

pdflatex -interaction batchmode presentation_aniged_2025.tex
echo "PDF2 =" $?

rm *.log
rm *.nav
rm *.snm
rm *.toc
rm *.upa
rm *.aux
rm *.out
rm *.vrb



