
# musicxml2

MusicXML library for Haskell, including a concise, strongly typed music representation 
isomorphic to MusicXML 3.0.

## Supported features

* Part- and timewise scores
* Instruments lists and part groups
* Pitched and unpitched notes, with enharmonic pitch spelling
* Cue and grace notes
* Time signatures, Key signatures and clefs
* Custom note heads
* Beaming, cross-beams and tremolo
* Ornaments and articulation
* Dynamics

The following features are currently missing or incomplete:

* Lyrics
* Chord symbols
* Fingered bass
* Tablature
* Percussion grids
* Scordatura
* Layouts 


## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

    cabal configure
    cabal install
