
TRANSFORM    =transf +RTS -N4
TRANSFORM_PDF=transf --format=pdf
RESOLVE_LINKS=GHC_PACKAGE_PATH=`music-util package-path` hslinks
PANDOC=pandoc
CABAL_FILES=\
	../../music-score/music-score.cabal \
	../../music-pitch/music-pitch.cabal \
	../../music-dynamics/music-dynamics.cabal \
	../../music-articulation/music-articulation.cabal \
	../../music-parts/music-parts.cabal \
	../../music-pitch-literal/music-pitch-literal.cabal \
	../../music-dynamics-literal/music-dynamics-literal.cabal

SRC=src
OUT=build
PAGE=index.html
CSS=styles.css
MODULE_GRAPH=module-graph.png

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
			| $(PANDOC) --standalone --toc --css styles.css -Thtml -o $(PAGE) && \
		cp ../$(CSS) styles.css && \
		cp ../$(MODULE_GRAPH) module-graph.png && \
	popd

pdf: transform-pdf
	pushd $(OUT) && \
		(cat 	About.md \
			User-Guide.md \
			) \
			| $(PANDOC) --standalone --toc -Tpdf -o ../test.pdf && \
	popd

transform: 
	mkdir -p $(OUT)
	pushd $(OUT) && \
		pwd && \
		( $(RESOLVE_LINKS) $(CABAL_FILES) | $(TRANSFORM) ) <../$(SRC)/About.md 	>About.md  && \
		( $(RESOLVE_LINKS) $(CABAL_FILES) | $(TRANSFORM) ) <../$(SRC)/Usage.md 	>Usage.md   && \
		( $(RESOLVE_LINKS) $(CABAL_FILES) | $(TRANSFORM) ) <../$(SRC)/User-Guide.md  	>User-Guide.md  && \
		rm -f *.eps	 && \
		rm -f *.count	 && \
		rm -f *.tex	 && \
		rm -f *.texi && \
	popd

transform-pdf: 
	mkdir -p $(OUT)
	pushd $(OUT) && \
		pwd && \
		( $(RESOLVE_LINKS) $(CABAL_FILES) | $(TRANSFORM_PDF) ) <../$(SRC)/About.md 	>About.md  && \
		( $(RESOLVE_LINKS) $(CABAL_FILES) | $(TRANSFORM_PDF) ) <../$(SRC)/Usage.md 	>Usage.md   && \
		( $(RESOLVE_LINKS) $(CABAL_FILES) | $(TRANSFORM_PDF) ) <../$(SRC)/User-Guide.md  	>User-Guide.md  && \
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