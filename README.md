[![CircleCI](https://circleci.com/gh/music-suite/music-suite.svg?style=svg)](https://circleci.com/gh/music-suite/music-suite)



# Music Suite

Music Suite is a language for describing music, based on Haskell.

![image](https://user-images.githubusercontent.com/321331/111701233-6c33ab80-8832-11eb-9d26-7d0369b22a43.png)


<!-- See <http://music-suite.github.io>. -->


## Build Music Suite

### Development environment

There are two ways of setting up the development environment:

1. Using Nix (recommended on Linux)
2. Manually (recommended on Windows and OS X)

#### Nix setup

Install the [Nix package manager](https://en.wikipedia.org/wiki/Nix_package_manager). We recommend using 2.3.1 or later.

Enter environment using:

```
nix-shell --pure
```

All build commands should be run in the Nix shell. You can exit the Nix shell using `Ctrl-D`.

#### Manual setup

Install the following.

- [Lilypond](http://lilypond.org/), 2.22.1 or later
- [Timidity++](https://sourceforge.net/projects/timidity/), 2.15.0 or later
- [ghcup](https://www.haskell.org/ghcup)

Make sure that `lilypond` `timidity` and `ghcup` are available your shell environment (e.g. by adding them to `PATH`).

Use `ghcup` to install GHC:

```
ghcup install 8.10.4
```

## Build the library and examples

```
$ cabal update
$ cabal build
```

### Build and run the tests

#### Standard test suite

```
$ cabal test --test-show-details=streaming --test-options=--color=always
```

To run individual tests:

```
$ cabal run TEST_NAME -- TEST_ARGS...
```

e.g.

```
$ cabal run music-suite-test-xml-parser
```

#### Doctests

Music Suite makes heavy use of [doctests](https://en.wikipedia.org/wiki/Doctest). To run all doctests, type:

```
$ cabal build music-suite && cabal exec doctester --package music-suite
```

or (Nix only):

```
doctests
```

You can also pass individual directories to run a subset of the doctests. For example to test `src/Music/Pitch`:

```
$ cabal build-music-suite && cabal exec doctester --package music-suite -- src/Music/Pitch
```



### Development shell

```
$ cabal build music-suite && cabal exec --package music-suite ghci
```

or

```
$ cabal repl
```

### Build the documentation

#### User Guide

TODO

The output appears in `docs/build`. You can point a HTTP server to this directory.

#### API docs

```
$ cabal haddock
```


### Run example

```
$ cabal exec runhaskell -- examples/chopin.hs -f ly -o t.ly
```



## How to upgrade the compiler

We use Nix to pin the version of GHC and Cabal freeze files to pin the
version of all Haskell dependencies. This describes how to upgrade GHC.

Because GHC pins a version of the Haskell base library, GHC and the Cabal dependencies need to be upgraded together. This is the recommended workflow:

1. Update the commit/URL and hash in `default.nix`
  1. Use `$ nix-prefetch-url --unpack <url>` to obtain the hash (and verify)
1. Enter new Nix shell (may take a while)
1. Update the `ghc-version` field in `cabal.project` to whatever is printed by `ghc --version`
1. Comment out `reject-unconstrained-dependencies` in `cabal.project`
1. Update `index-state` in Cabal config to a recent time
1. Run `cabal update`
1. Run `rm cabal.project.freeze`
1. Run `cabal freeze`
1. Run `cabal test` to check that compiling/testing works (and fix errors)
1. Restore `reject-unconstrained-dependencies`
1. Commit your changes.


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
