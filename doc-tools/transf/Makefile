
TRANSF=dist/build/transf/transf
TRANSF_OPTIONS = +RTS+ -N8
PANDOC_OPTIONS = -s --toc -cstyles.css -s -t html --highlight-style=espresso

.PHONY: test
test:
	rm -f *.png
	rm -f *.mid                            
	rm -f out.md
	rm -f out2.md
	( $(TRANSF) $(TRANSF_OPTIONS) ) <test.md >out.md
	( pandoc $(PANDOC_OPTIONS) ) <out.md >test.html

	( $(TRANSF) $(TRANSF_OPTIONS) ) <test2.md >out2.md
	( pandoc $(PANDOC_OPTIONS) ) <out2.md >test2.html

	# rm -f *.ly
	rm -f *.eps	
	rm -f *.count
	rm -f *.tex
	rm -f *.texi

gen:
	rm -f *.png
	rm -f *.mid
	( $(TRANSF) $(TRANSF_OPTIONS) ) <test.md >test2.md
	rm -f *.ly
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