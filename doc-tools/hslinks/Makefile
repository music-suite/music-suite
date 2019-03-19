
# HSLINKS=hslinks
HSLINKS=dist/build/hslinks/hslinks
CABAL_FILES=\
	../music-score/music-score.cabal \
	../music-pitch/music-pitch.cabal \
	../music-dynamics/music-dynamics.cabal \
	../music-articulation/music-articulation.cabal \
	../music-parts/music-parts.cabal \
	../music-pitch-literal/music-pitch-literal.cabal \
	../music-dynamics-literal/music-dynamics-literal.cabal \

	# ../music-sibelius/music-sibelius.cabal \


test:
	$(HSLINKS) $(CABAL_FILES) <Test.md >Test2.md
	cat Test2.md