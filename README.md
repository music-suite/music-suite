
[![Build Status](https://travis-ci.org/hanshoglund/music-pitch-literal.png?branch=master)](https://travis-ci.org/hanshoglund/music-pitch-literal)

# music-pitch-literal

This package allow you to write the pitches of standard notation as expressions
overloaded on result type. This works exactly like overloaded numbers and strings.

This is a separate package so that container types can declare lifted instances without depending on `music-pitch`.

This library is part of the Haskell Music Suite, see <http://music-suite.github.io>.


## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

    cabal configure
    cabal install
