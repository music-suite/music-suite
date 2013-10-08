
TRANSFORM=transf +RTS -N4
TRANSFORM_PDF=transf --format=pdf
SRC=src
OUT=build
PAGE=index.html
CSS=styles.css

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
			| pandoc --standalone --toc --css styles.css -Thtml -o $(PAGE) && \
		cp ../$(CSS) styles.css && \
	popd

pdf: transform-pdf
	pushd $(OUT) && \
		(cat 	About.md \
			User-Guide.md \
			) \
			| pandoc --standalone --toc -Tpdf -o ../test.pdf && \
	popd

transform: 
	mkdir -p $(OUT)
	pushd $(OUT) && \
		pwd && \
		$(TRANSFORM) <../$(SRC)/About.md 	>About.md  && \
		$(TRANSFORM) <../$(SRC)/Usage.md 	>Usage.md   && \
		$(TRANSFORM) <../$(SRC)/User-Guide.md  	>User-Guide.md  && \
		rm -f *.eps	 && \
		rm -f *.count	 && \
		rm -f *.tex	 && \
		rm -f *.texi && \
	popd

transform-pdf: 
	mkdir -p $(OUT)
	pushd $(OUT) && \
		pwd && \
		$(TRANSFORM_PDF) <../$(SRC)/About.md 	>About.md  && \
		$(TRANSFORM_PDF) <../$(SRC)/Usage.md 	>Usage.md   && \
		$(TRANSFORM_PDF) <../$(SRC)/User-Guide.md  	>User-Guide.md  && \
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