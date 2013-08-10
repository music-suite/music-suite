
#TRANSFORM=(transf | replace -i '~~~' '\`\`\`')
TRANSFORM=(transf)

upload: transform
	git add *.png *.ly *.mid
	git add Tutorial
	git commit -m "Updated wiki"
	git push

pdf: transform
	(cat 	Tutorial/About.md \
		Tutorial/Getting-Started.md) \
		| pandoc --standalone --toc -Tpdf -o test.pdf
html: transform
	(cat 	Tutorial/About.md \
		Tutorial/Getting-Started.md) \
		| pandoc --standalone --toc -Thtml -o test.html

transform:                                                        
	mkdir -p Tutorial
	$(TRANSFORM) <../src/Tutorial/About.md 		 >Tutorial/About.md
	$(TRANSFORM) <../src/Tutorial/Usage.md 		 >Tutorial/Usage.md
	$(TRANSFORM) <../src/Tutorial/Getting-Started.md >Tutorial/Getting-Started.md

	rm -f *.eps	
	rm -f *.count
	rm -f *.tex
	rm -f *.texi

clean:
	rm -f *.pdf
	rm -f *.ly
	rm -f *.mid
	rm -f *.png
	rm -f *.html