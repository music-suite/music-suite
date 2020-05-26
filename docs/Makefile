
SHELL=/usr/bin/env bash

#CSS	       =$(shell pwd)/templates/pandoc-bootstrap-template/template.css
#PANDOC_TEMPLATE=$(shell pwd)/templates/pandoc-bootstrap-template/template.html
CSS	       =../templates/music-suite/template.css
PANDOC_TEMPLATE=../templates/music-suite/template.html

PANDOC=pandoc --standalone --toc
PANDOC_PDF=$(PANDOC) --write=latex
PANDOC_HTML=$(PANDOC) --write=html --toc-depth 4 --css styles.css --template $(PANDOC_TEMPLATE)

TRANSFORM    =cabal exec transf -- +RTS -N4
TRANSFORM_PDF=cabal exec transf -- --format=pdf
RESOLVE_LINKS=cabal exec hslinks


SRC=src
OUT=build
PAGE=index.html

html: transform
	pushd $(OUT) && \
		(cat 	About.md \
			User-Guide.md \
			) \
			| $(PANDOC_HTML) -o $(PAGE) && \
		cp $(CSS) styles.css && \
	popd


pdf: transform-pdf
	pushd $(OUT) && \
		(cat 	About.md \
			User-Guide.md \
			) \
			| $(PANDOC_PDF) -o ../test.pdf && \
	popd

transform:
	mkdir -p $(OUT)
	pushd $(OUT) && \
		pwd && \
		( $(TRANSFORM) | $(RESOLVE_LINKS) ) <../$(SRC)/About.md 		>About.md  && \
		( $(TRANSFORM) | $(RESOLVE_LINKS) ) <../$(SRC)/User-Guide.md  	>User-Guide.md  && \
		rm -f *.eps	 && \
		rm -f *.count	 && \
		rm -f *.tex	 && \
		rm -f *.texi && \
	popd

transform-pdf:
	mkdir -p $(OUT)
	pushd $(OUT) && \
		pwd && \
		( $(TRANSFORM_PDF) | $(RESOLVE_LINKS) ) <../$(SRC)/About.md 	>About.md  && \
		( $(TRANSFORM_PDF) | $(RESOLVE_LINKS) ) <../$(SRC)/User-Guide.md  	>User-Guide.md  && \
		rm -f *.eps	 && \
		rm -f *.count	 && \
		rm -f *.tex	 && \
		rm -f *.texi && \
	popd

clean:
	rm -f $(OUT)/*.pdf
	rm -f $(OUT)/*.ly
	rm -f $(OUT)/*.mid
	rm -f $(OUT)/*.png
	rm -f $(OUT)/*.html

clean-todo:
	rm -f *.eps
	rm -f *.count
	rm -f *.tex
	rm -f *.texi
	rm -f *.eps
	rm -f *.pdf
	rm -f *.png
	rm -f *.mid
	rm -f *.ly
