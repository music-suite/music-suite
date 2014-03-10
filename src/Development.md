

The packages in the suite are named as follows:

The music-X packages cover a musical aspect (i.e. pitch, dynamics etc)
The music-X-literal packages provide overloaded vocabulary (pitches, intervals, dynamics etc) etc
The music-score package provides time structures (spans, voices, behaviors, scores), a meta-data API and translations to external representations (MusicXML, Lilypond etc).
The music-preludes package provides prelude-style modules, each of which defines a ready-to-go music representation. This is also the place for examples, regression tests etc.
You can find the whole dependency graph here https://raw.github.com/hanshoglund/music-docs/master/music-suite-deps.png.

The most important thing to notice here is that each musical aspects is independent of the representation of each other aspect, and furthermore that all aspects are independent of the time representation and vice versa.

The literals are kept in separate package because one often wishes to overload the time structures point-wise. For example given the literal `eb :: IsPitch a => a`, we want to interpret it as the pitch e flat (according to some representation), or a note/voice/score where the pitch is e flat and all other parameters have default values.

 
## Status

	musicxml2				Stable
	lilypond				Stable
	abcnotation				Unstable
	music-pitch-literal		Stable
	music-dynamics-literal	Stable
	music-pitch				Stable, needs rewrite
	music-part				Unstable
	music-dynamics			Unstable
	music-articulation		Unstable
	music-score				Major rewrite going on, soon to be stable
	music-preludes			Always changing to reflect upstream development.
	
## License

The whole Suite is BSD3.

## Download and build from sources

- Install the utility program: `cabal update && cabal install music-util`

- Assure that the environment variable `MUSIC_SUITE_DIR` is set to the directory where you want to keep the music-suite sources.

- `cd $MUSIC_SUITE_DIR && music-util setup`

- To build and install the whole suite, use `music-util install music-preludes`

You should be able to pull from all packages using `music-util foreach git pull`. To push to a repo need to make a fork. For example with the `music-pitch` package you would do it as follows:

- Go to https://github.com/music-suite/music-pitch

- Press the Fork button

- In the newly forked repo, find the Clone URL frame, press SSH and then copy the URL 
(which should start with git@github.com:username)

- `cd $MUSIC_SUITE_DIR/music-pitch && git remote add fork <paste-url-here>`

You can then push using `git push fork master`.
