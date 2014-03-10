

# Design overview

<!--
To develop the Music Suite you need the following tools:

* Pandoc
* Transf
* Hslinks
* Lilypond

Most of these can be installed using `cabal install`.

There is a utility program called `music-util`, which simplifies the kind of cross-package development used throughout the Music Suite. This can be installed in the same manner as the packages, i.e. `cabal install music-util`. See [its documentation][music-util-docs] for an overview of the things it can do.

-->

TODO

The Music Suite consists of a number of packages. These can be divided into two categories: 

- *Music* packages that provide classes, types and functions related to a particular aspect of musical representation such as time, pitch, dynamics and so on. The name of these packages always begin with `music`.

- *Supporting* packages that implement a musical representation (`musicxml2`, `lilypond`, `abcnotation`), or miscellaneous functionality such as cross-platform MIDI support (`hamid`). These packages can be used as stand-alone packages but are included in the Suite for completeness.

There is no central package, instead the aim has been to separate the various issues that arise in music representations as clearly as possible. In particular, the `music-score` package, which provide scores and other temporal containers, does *not* depend on packages that provide models of musical aspects such as `music-pitch`, neither do these libraries depend on `music-score`. 

The reason for this is that we want to keep musical structure and content separate. This is a form of the [expression problem](http://en.wikipedia.org/wiki/Expression_problem): if one depended on the other we would either always force the user into a particular form of musical structure, or a particular form of musical material.

However, some packages have special roles:

- The `music-pitch-literal` and `music-dynamics-literal` are minimal packages that provide musical *literals*, i.e. common vocabulary overloaded on result type. This means other packages can import and provide instances for the literals without having to depend on a specific representation.

- The `music-preludes` provides modules that import modules from both `music-score` and `music-pitch` and its sister packages.


### Compability with other libraries

The Music Suite libraries does not profess to be compatible with any other music *representation* library^[Including Haskore, Euterpea, hts and temporal-media], and deliberately claims the whole `Music` top-level package. The aim is that functionality from these packages should eventually be included into the Music Suite packages. However it can be used with packages that implement audio processing, synthesis, interaction with musical instruments, FRP libraries and so on. 

The concepts and abstractions used in the suite overlap with some fundamental concepts form FRP, but the focus is fundamentally different. While FRP libraries focus on reacting to the external world, the Music Suite focus on modeling temporal values and musical concepts.


## Package names

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
