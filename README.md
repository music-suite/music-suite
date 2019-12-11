[![CircleCI](https://circleci.com/gh/hanshoglund/music-suite.svg?style=svg)](https://circleci.com/gh/hanshoglund/music-suite)

# Music Suite

Music Suite is a language for describing music, based on Haskell.

See <http://music-suite.github.io>.

## Set up build environment

Install Nix (2.3.1 is tested, others might work).

Enter build environment using:

```
nix-shell --pure
```

Inside the build shell, the following commands can be used:

### Build and test everything

```
cabal v2-update
cabal --reject-unconstrained-dependencies=all v2-build all
cabal --reject-unconstrained-dependencies=all v2-test all
```

### Generate documentation

```
(cd docs && make)
```

The output appears in `docs/build`.
