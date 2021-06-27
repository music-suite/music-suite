[![CircleCI](https://circleci.com/gh/music-suite/music-suite.svg?style=svg)](https://circleci.com/gh/music-suite/music-suite)



# Music Suite

Music Suite is a language for describing music, based on Haskell.

![image](https://user-images.githubusercontent.com/321331/111701233-6c33ab80-8832-11eb-9d26-7d0369b22a43.png)


<!-- See <http://music-suite.github.io>. -->


## How to build MusicSuite
Music Suite can be built using in a Nix environment (Linux only) or with Cabal.
If you use Nix then MusicSuite will have all the dependencies it needs
installed for you. If you choose to use Cabal then you will need to install
additional programs manually.

## Set Up the Build Environment
Befor you can build Music Suite you need to set up the build environment.
The steps are outlined separately for Nix and Cabal.

### Set up the build environment for Nix

Install Nix (2.3.1 or later).

Enter build environment using:

```
nix-shell --pure
```

You should see this prompt:

```
#
```

### Set up the build environment for Cabal
You will need to install the following:
- [https://git-scm.com/](Git)
- [http://lilypond.org/](Lilypond)
- [http://timidity.sourceforge.net/](Timidity++)

On Linux you should find these in your package manager if they're not
already installed. On OSX you can either download these, or use Homebrew, or 
MacPorts. You should be able to install these on Windows as well.

For OSX and Linux users, install ghcup using [https://www.haskell.org/ghcup/](these
instructions). If you're using Windows consult a guide on current bet
practices for installing Haskell on Windows. Some of the instructions below
may not work on Windows..

Install ghc 8.10.4:

```
ghcup install 8.10.4
```

Go to a directory where you want to use musicsuite and type the following
commands:

```
git clone https://github.com/music-suite/music-suite.git
cd music-suite
```


## Build the library and examples

The following instructions work both inside the NIX shell (if you're 
using NIX), or inside the music-suite directory (if you're just using Cabal).

```
# cabal update
# cabal build
```

### Build and run the tests

#### Standard test suite

```
# cabal test --test-show-details=streaming --test-options=--color=always
```

To run individual tests:

```
# cabal run TEST_NAME -- TEST_ARGS...
```

e.g.

```
cabal run music-suite-test-xml-parser
```

#### Doctests

```
# cabal build && doctests
```

To run doctests for individual files/directories:

```
# cabal build && cabal exec doctester --package music-suite -- src/Music/Pitch
```



### Development shell

```
# cabal build music-suite && cabal exec --package music-suite ghci
```

or

```
# cabal repl
```

### Build the documentation

#### User Guide

TODO

The output appears in `docs/build`. You can point a HTTP server to this directory.

#### API docs

```
m> cabal haddock
```


### Run example

```
cabal exec runhaskell -- examples/chopin.hs -f ly -o t.ly
```



## How to upgrade the compiler

We use Nix to pin the version of GHC and Cabal freeze files to pin the
version of all Haskell dependencies. This describes how to upgrade GHC.

Because GHC pins a version of the Haskell base library, this generally
also means upgrading your Cabal dependencies.

- Update the commit/URL and hash in `default.nix`
  - Use `$ nix-prefetch-url --unpack <url>` to obtain the hash (and verify)
- Enter new Nix shell (may take a while)
- Update the `ghc-version` field in `cabal.project` to whatever is printed by `ghc --version`
- Comment out `reject-unconstrained-dependencies` in `cabal.project`
- Update `index-state` in Cabal config to a recent time
- Run `cabal update`
- Run `rm cabal.project.freeze`
- Run `cabal freeze`
- Run `cabal test` to check that compiling/testing works (and fix errors)
- Restore `reject-unconstrained-dependencies`
- Commit your changes.


# Developer notes

## Module hierarchy

- The high-level DSL:
  - `Music.Time`: high-level DSL for time and rhythm
  - `Music.Pitch`: high-level DSL for pitch (common, scientific)
  - `Music.Dynamics`: high-level DSL for dynamics
  - `Music.Articulation`: high-level DSL for musical articulation
  - `Music.Part`: high-level DSL for instruments and parts
  - `Music.Prelude`: prelude/standard library for the Music Suite DSL

- The notation DSL:
  - `Music.Notation.Standard`: DSL for representing Common/Western music notation

- Import & Export:
  - `Data.Music.Lilypond`: AST, parsing and pretty-printing for the Lilypond language
  - `Data.Music.MusicXml`: AST, parsing and pretty-printing for MusicXML

- Utility
  - `Control.*`: miscellaneous algorithms and utilities
  - `Data.*`: miscellaneous data structures
