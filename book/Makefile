all: Ghengin.pdf

Ghengin.pdf: Ghengin.tex ocean/Tutorial.tex prelude/lhs2TeX.sty images/
	pdflatex $<

prelude/lhs2TeX.sty: prelude/lhs2TeX.lhs
	lhs2TeX $< -o $@

ocean/Tutorial.lhs:
	ln -sf ../ocean/ .

ocean/Tutorial.tex: ocean/Tutorial.lhs
	lhs2TeX $< -o $@

Tutorial.html: ocean/Tutorial.tex
	htlatex $< "xhtml,html5,mathml,charset=utf-8" " -cunihtf -utf8"

.PHONY: clean

clean:
	rm -f *.aux *.log *.out *.ptb *.4tc *.4ct *.tmp *.xref *.dvi *.idv *.lg
	rm -f prelude/lhs2TeX.sty
	rm -f Ghengin.pdf
	rm -f ocean/Main.tex ocean/Tutorial.tex ocean/*.aux
