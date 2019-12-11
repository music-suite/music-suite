


CSS	       =../templates/pandoc-bootstrap-template/template.css
PANDOC_TEMPLATE=../templates/pandoc-bootstrap-template/template.html
#CSS	       =../templates/music-suite/template.css
#PANDOC_TEMPLATE=../templates/music-suite/template.html

PANDOC=pandoc --standalone --toc
PANDOC_PDF=$(PANDOC) --write=latex
PANDOC_HTML=$(PANDOC) --write=html --toc-depth 4 --css styles.css --template $(PANDOC_TEMPLATE)

CABAL_FILES=""
# CABAL_FILES=\
# 	../../music-score/music-score.cabal \
# 	../../music-pitch/music-pitch.cabal \
# 	../../music-dynamics/music-dynamics.cabal \
# 	../../music-articulation/music-articulation.cabal \
# 	../../music-parts/music-parts.cabal \
# 	../../music-pitch-literal/music-pitch-literal.cabal \
# 	../../music-dynamics-literal/music-dynamics-literal.cabal \

# Can't include prelude, as that makes all refs point to the Prelude module
# 	../../music-preludes/music-preludes.cabal

TRANSFORM    =cabal v2-exec transf -- +RTS -N4
TRANSFORM_PDF=cabal v2-exec transf -- --format=pdf
#TRANSFORM    =transf +RTS -N4
#TRANSFORM_PDF=transf --format=pdf
#RESOLVE_LINKS=GHC_PACKAGE_PATH=`music-util package-path` hslinks $(CABAL_FILES)
RESOLVE_LINKS=cat

SRC=src
OUT=build
PAGE=index.html
TUTORIAL_PAGE=tutorial.html
# MODULE_GRAPH=module-graph.png

# upload-wiki: transform
# 	pushd $(OUT) && \
# 		git add *.png *.ly *.mid && \
# 		git add *.md && \
# 		git commit -m "Updated wiki" && \
# 		git push && \
# 	popd

html: transform
	pushd $(OUT) && \
		(cat 	About.md \
			User-Guide.md \
			) \
			| $(PANDOC_HTML) -o $(PAGE) && \
		cp $(CSS) styles.css && \
		cp -R ../js js && \
	popd

#	cp ../$(MODULE_GRAPH) module-graph.png && \

html-tutorial: transform
	pushd $(OUT) && \
		(cat 	Tutorial.md \
			) \
			| $(PANDOC_HTML) -o $(TUTORIAL_PAGE) && \
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
		( $(RESOLVE_LINKS) | $(TRANSFORM) ) <../$(SRC)/About.md 		>About.md  && \
		( $(RESOLVE_LINKS) | $(TRANSFORM) ) <../$(SRC)/Usage.md 		>Usage.md   && \
		( $(RESOLVE_LINKS) | $(TRANSFORM) ) <../$(SRC)/User-Guide.md  	>User-Guide.md  && \
		( $(RESOLVE_LINKS) | $(TRANSFORM) ) <../$(SRC)/Tutorial.md  	>Tutorial.md  && \
		rm -f *.eps	 && \
		rm -f *.count	 && \
		rm -f *.tex	 && \
		rm -f *.texi && \
	popd

transform-pdf:
	mkdir -p $(OUT)
	pushd $(OUT) && \
		pwd && \
		( $(RESOLVE_LINKS) | $(TRANSFORM_PDF) ) <../$(SRC)/About.md 	>About.md  && \
		( $(RESOLVE_LINKS) | $(TRANSFORM_PDF) ) <../$(SRC)/Usage.md 	>Usage.md   && \
		( $(RESOLVE_LINKS) | $(TRANSFORM_PDF) ) <../$(SRC)/User-Guide.md  	>User-Guide.md  && \
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
