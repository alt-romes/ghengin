all: main.pdf

main.pdf: main.tex
	pdflatex $<

main.tex: main.lhs
	lhs2TeX $< -o $@
