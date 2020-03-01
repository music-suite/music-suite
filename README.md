[![CircleCI](https://circleci.com/gh/hanshoglund/music-suite.svg?style=svg)](https://circleci.com/gh/hanshoglund/music-suite)

# Music Suite

Music Suite is a language for describing music, based on Haskell.

See <http://music-suite.github.io>.

## Set up build environment

Install Nix (2.3.1 is tested).

Enter build environment using:

```
nix-shell --pure
```

Inside the build shell, the following commands can be used:

### Build everything

```
cabal update
cabal build
```

### Test everything

#### Property tests

```
cabal test
```

To run individual tests:

```
cabal run TEST_NAME -- TEST_ARGS...
```

#### Doctests

```
cabal build && cabal exec --package music-suite -- cabal run doctester PATHS
```

where `PATHS` is a list of Haskell files or directories containing Haskell files.

For now, `default-extensions` is not recognized, so you must list the extensions
explicitly in each file using a `LANGUAGE` pragma.

### Development shell

```
cabal build && cabal exec --package music-suite ghci
```

### Generate documentation

#### Reference docs

```
cabal build music-suite transf hslinks && (cd docs && make)
```

The output appears in `docs/build`. You can point a HTTP server to this directory.

#### API docs

```
cabal haddock
```
