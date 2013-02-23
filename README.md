
# musicxml2

MusicXML library for Haskell, including a concise, strongly typed music representation 
isomorphic to MusicXML 2.0.

## Supported features

* Part- and timewise scores
* Instruments lists and part groups
* Pitched and unpitched notes, with enharmonic pitch spelling
* Cue and grace notes
* Time signatures, Key signatures and clefs
* Custom note heads
* Beaming, cross-beams and tremolo
* Slurs, staccato and misc. articulation

The following features are currently missing or incomplete:

* Lyrics
* Chord symbols
* Fingered bass
* Tablature
* Percussion grids
* Tuning
* Layouts 


## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

    cabal configure
    cabal install
