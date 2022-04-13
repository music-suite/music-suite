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


### Testing

#### Run the test suite:

To run all tests except doctests (see below):

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

#### Expected output (regression tests)

Some tests have *expected output*, stored in `test/regression`.

If the output of these tests have changed, the diff should be manually inspected to assure that the new output is in fact correct. This may be because:

1. The output is in fact expected to change.
1. The output has changed in a way that is invisible to the end user, such as a a change to an `*.ly` output file that does not affect the appearance of the printed music.

To identify the latter it may be necessar to run `lilypond` or `timidity` on both the old and new versions of the output file. To do this manually after a failure.

1. (optional) Re-run regression tests to see that it fails: `cabal test music-suite-test-regression --test-options=--color=always`
1. For each failing file, run `lilypond` (for `*.ly` files) or `timidity` (for `*.mid` files).
1. Regenerate the expected files: `cabal test music-suite-test-regression --test-options=--color=always --test-options=--accept`
1. For each changed file, run `lilypond` or `timidity` as before.
1. Inspect the old/new version side by side.
1. If all are correct, commit the changes to `test/regression`.

> Note: after running `--accept`, you can use `git diff --name-only test/regression` to get a list of changed files, assuming the repo was clean before.

#### Doctests

Music Suite makes use of [doctests](https://en.wikipedia.org/wiki/Doctest).

You can pass any file or directory. For example to test `src/Music/Pitch`:

```
$ cabal build music-suite && cabal exec doctester --package music-suite -- src/Music/Pitch
```

To test a single file:

```
$ cabal build music-suite && cabal exec doctester --package music-suite -- src/Music/Score/Meta.hs
```

To run all doctests use (Nix only):

```
$ doctests
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

See [these instructions](docs-sphinx).

The output appears in `docs/build`. You can point a HTTP server to this directory.

#### API docs

```
$ cabal haddock
```


### Run example

```
$ cabal exec runhaskell -- examples/chopin.hs -f ly -o t.ly
```


### Continous Integration

To replicate all steps run by the CI (Nix only), run:

```
$ ci
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

## How to add a new dependency

1. Comment out `reject-unconstrained-dependencies` in `cabal.project`
1. Add the dependency to `music-suite.cabal`
1. Update `index-state` in Cabal config to a recent time
1. Run `cabal freeze`
1. Run `cabal test` to check that compiling/testing works (and fix errors)
1. Restore `reject-unconstrained-dependencies`
1. Commit your changes.

# Developer notes

## Module hierarchy

- The high-level DSL:
  - [`Music.Time`](src/Music/Time): high-level DSL for time and rhythm
  - [`Music.Pitch`](src/Music/Pitch): high-level DSL for pitch (common, scientific)
  - [`Music.Dynamics`](src/Music/Dynamics): high-level DSL for dynamics
  - [`Music.Parts`](src/Music/Parts): high-level DSL for instruments and parts
  - [`Music.Prelude`](src/Music/Prelude): prelude/standard library for the Music Suite DSL

- The notation DSL:
  - [`Music.Notation.Standard`](src/Music/Notation/Standard): DSL for representing Common/Western music notation

- Import & Export:
  - [`Data.Music.Lilypond`](https://github.com/music-suite/music-suite/tree/main/src/Data/Music/Lilypond): AST, parsing and pretty-printing for the Lilypond language
  - [`Data.Music.MusicXml`](https://github.com/music-suite/music-suite/tree/main/src/Data/Music/MusicXml): AST, parsing and pretty-printing for MusicXML

- Utility
  - `Control.*`: miscellaneous algorithms and utilities
  - `Data.*`: miscellaneous data structures
