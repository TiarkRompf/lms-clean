# workflows:
#
#  %.dia [dia]-> %.eps [ps2pdf]-> %.pdf
#  %.dot [dot]-> %.pdf [pdftops]-> %.eps
#  %.gnumeric [save as image from gnumeric]-> %.pdf [pdftops]-> %.eps

export TEXINPUTS := $(shell pwd)/../../common/pgf_2//:$(shell pwd)/../../common/pgfplots//::$TEXINPUTS

TEX_SRCS=\
	paper.tex \
    # macros.tex \
	# 10_intro.tex \
	# references.bib \
	# acmart.cls \
#

# expecting .dia
DIA_SRCS=\
#

# expecting .dot
DOT_SRCS=\
#

# expecting .ps
GNUMERIC_SRCS=\
#

PDF_INPUT_FILES=$(DIA_SRCS:%.dia=%.pdf) $(DOT_SRCS:%.dot=%.pdf) $(GNUMERIC_SRCS:%.ps=%.pdf)
PDF_INPUT_PATHS=$(PDF_INPUT_FILES:%=build/%)
EPS_INPUT_PATHS=$(PDF_INPUT_PATHS:%.pdf=%.eps)

TEX_OPTIONS=-output-directory build

PDFLATEX=pdflatex $(TEX_OPTIONS)
BIBTEX=TEXMFOUTPUT=build bibtex
LATEX=latex $(TEX_OPTIONS)

.PHONY: default
default: all

export:
	export TEXINPUTS

.PHONY: all
all: build/paper.pdf
#all: $(BASE).tex $(BASE).pdf $(BASE).ps

$(BASE).tex: build/indexable.tex
	cp $< $@

$(BASE).pdf: build/paper.pdf
	cp $< $@

$(BASE).ps: build/paper.ps
	cp $< $@

build/paper.pdf: export $(TEX_SRCS) $(PDF_INPUT_PATHS)
	mkdir -p build
	@echo
	@echo
	@echo PDF_INPUT_PATHS=$(PDF_INPUT_PATHS)
	@echo
	@echo
	@echo
	@echo ----- $(PDFLATEX) 1... -----
	$(PDFLATEX) `basename $@ .pdf`.tex
	@echo ----- $(BIBTEX)... -----
	$(BIBTEX) build/`basename $@ .pdf`
	@echo ----- $(PDFLATEX) 2... -----
	$(PDFLATEX) `basename $@ .pdf`.tex
	@echo ----- $(PDFLATEX) 3... -----
	$(PDFLATEX) `basename $@ .pdf`.tex
	@echo ----- done -----

build/paper.dvi: export $(TEX_SRCS) $(EPS_INPUT_PATHS)
	@echo ----- $(PDFLATEX) 1... -----
	$(LATEX) `basename $@ .dvi`.tex
	@echo ----- $(BIBTEX)... -----
	$(BIBTEX) build/`basename $@ .dvi`
	@echo ----- $(PDFLATEX) 2... -----
	$(LATEX) `basename $@ .dvi`.tex
	@echo ----- $(PDFLATEX) 3... -----
	$(LATEX) `basename $@ .dvi`.tex
	@echo ----- done -----

build/paper.ps: build/paper.dvi
	dvips -o $@ $<

build/%.eps: from_dia/%.dia
	mkdir -p build
	dia --export=$@ $<

build/%.pdf: from_dot/%.dot
	mkdir -p build
	dot -Tpdf -o$@ $<

build/%.eps: from_gnumeric/%.ps
	mkdir -p build
	ps2epsi $< $@

build/%.eps: build/%.pdf
	pdftops -eps $< $@

build/%.pdf: build/%.eps
	ps2pdf -dEPSCrop $< $@

# ACM index needs the raw TEX and the *used* bibliography BBL
build/indexable.tex: paper.tex build/paper.pdf
	cat paper.tex build/paper.bbl > $@

.PHONY: clean
clean:
	rm -rf build *~

.PHONY: wordcount
wordcount:
	detex paper.tex | wc -w

grammar.txt: paper.tex
	sed -e 's/~\\ref{[^}]*}/ 42/g' -e 's/~\\cite{[^}]*}//g' paper.tex | detex | uniq | tr '[\n]' '[\\]' | sed 's/\([^\\]\)\\\([^\\]\)/\1 \2/g' | tr '[\\]' '[\n]' > grammar.txt
