all: book.pdf

book.pdf: book.tex ocean/Main.tex prelude/lhs2TeX.sty
	pdflatex $<

prelude/lhs2TeX.sty: prelude/lhs2TeX.lhs
	lhs2TeX $< -o $@

ocean/Main.tex: ocean/Main.lhs
	lhs2TeX $< -o $@

ocean/Main.lhs:
	ln -sf ../ocean/ .

.PHONY: clean

clean:
	rm -f *.aux *.log *.ptb book.pdf prelude/lhs2TeX.sty
	# rm -f ocean/*.aux ocean/*.log ocean/*.ptb ocean/Main.tex ocean/main.pdf
