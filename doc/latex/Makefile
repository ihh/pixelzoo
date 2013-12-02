
PAPER = pzpaper

all: $(PAPER).pdf.open

%.open: %
	open $<

%.pdf: %.tex %.bib
	pdflatex $*.tex
	bibtex $*
	pdflatex $*.tex
	pdflatex $*.tex

clean:
	rm $(PAPER).pdf *.aux *.log *.bbl *.blg

.SECONDARY:
