
# music-sibelius

Exporting of Sibelius scores to a compatible music representations. This export retains most features of the original scores and does not require a MusicXML export plugin.

This functionality is still experimental: you need to manually install the Sibelius plugin (see below), and run it as *Plugins > JSON > Export JSON*.

This library an exterimental addition to the Music Suite, see <http://musicsuite.github.com>.

## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

### Haskell library 

    cabal configure
    cabal install

### Sibelius plugin 

    make install-plugin