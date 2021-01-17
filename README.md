[![CircleCI](https://circleci.com/gh/music-suite/music-suite.svg?style=svg)](https://circleci.com/gh/music-suite/music-suite)

# Music Suite

Music Suite is a language for describing music, based on Haskell.

<!-- See <http://music-suite.github.io>. -->


## How to build

### Set up the build environment

Install Nix (2.3.1 or later).

Enter build environment using:

```
nix-shell --pure
```

You should see this prompt:

```
m>
```

Inside the build shell, the following commands can be used:

### Build the library and examples

```
m> cabal update
m> cabal build
```

### Build and run the tests

#### Property tests

```
m> cabal test --test-show-details=streaming --test-options=--color=always
```

To run individual tests:

```
m> cabal run TEST_NAME -- TEST_ARGS...
```

e.g.

```
cabal run music-suite-test-xml-parser
```

#### Doctests

cabal build && cabal exec doctester --package music-suite -- src/Music/Pitch

### Development shell

```
m> cabal build music-suite && cabal exec --package music-suite ghci
```

or

```
m> cabal repl
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



## How to upgrade the compiler/Nixpkgs

- Update the commit/URL and hash in `default.nix`
  - Use `$ nix-prefetch-url --unpack <url>` to obtain the hash (and verify)
- Enter new Nix shell (may take a while)
- Comment out `reject-unconstrained-dependencies` in Cabal config
- Update `index-state` in Cabal config to a recent time
- Run `cabal freeze`
- Run `cabal test` to check that compiling/testing works (and fix errors)
- Restore `reject-unconstrained-dependencies`
- Commit changes to Nix and Cabal files


# Developer notes

## Module hierarchy

- The high-level DSL:
  - `Music.Time`: high-level DSL for time and rhythm
  - `Music.Pitch`: high-level DSL for pitch (common, scientific)
  - `Music.Dynamics`: high-level DSL for dynamics
  - `Music.Articulation`: high-level DSL for musical articulation
  - `Music.Part`: high-level DSL for instruments and parts
  - `Music.Prelude.Standard`: prelude/standard library for the Music Suite DSL

- The notation DSL:
  - `Music.Notation.Standard`: DSL for representing Common/Western music notation

- Import & Export:
  - `Data.Music.Lilypond`: AST, parsing and pretty-printing for the Lilypond language
  - `Data.Music.MusicXml`: AST, parsing and pretty-printing for MusicXML

- Utility
  - `Control.*`: miscellaneous algorithms and utilities
  - `Data.*`: miscellaneous data structures
