
#TRANSFORM=(transf | replace -i '~~~' '\`\`\`')
TRANSFORM=(transf)
SRC=src
OUT=music-score.wiki

upload-wiki: transform
	pushd $(OUT) && \
		git add *.png *.ly *.mid && \
		git add Tutorial && \
		git commit -m "Updated wiki" && \
		git push
	popd

pdf: transform
	pushd $(OUT) && \
		(cat 	Tutorial/About.md \
			Tutorial/Getting-Started.md \
			) \
			| pandoc --standalone --toc -Tpdf -o ../test.pdf && \
	popd

html: transform
	pushd $(OUT) && \
		(cat 	Tutorial/About.md \
			Tutorial/Getting-Started.md \
			) \
			| pandoc --standalone --toc -Thtml -o test.html && \
	popd

transform:
	pushd $(OUT) && \
		mkdir -p Tutorial && \
		pwd && \
		$(TRANSFORM) <../$(SRC)/Tutorial/About.md 	       	>Tutorial/About.md  && \
		$(TRANSFORM) <../$(SRC)/Tutorial/Usage.md 	       	>Tutorial/Usage.md   && \
		$(TRANSFORM) <../$(SRC)/Tutorial/Getting-Started.md  	>Tutorial/Getting-Started.md  && \
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