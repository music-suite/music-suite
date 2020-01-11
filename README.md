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

```
cabal test
```

To run individual tests:

```
cabal run TEST_NAME -- TEST_ARGS...
```

### Development shell

```
cabal exec --package music-suite ghci
```

### Generate documentation

#### Reference docs

```
cabal build transf && (cd docs && make)
```

The output appears in `docs/build`. You can point a HTTP server to this directory.

#### API docs

```
cabal haddock --haddock-options=--hyperlink-source
```
