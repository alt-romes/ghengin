all: article.pdf

article.pdf: article.tex
	pdflatex $<

.PHONY: clean

clean:
	rm -f article.aux article.log article.pdf
