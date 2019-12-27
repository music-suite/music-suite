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
cabal update
# TODO is update necessary with reject-unconstrained-dependencies?
# TODO instead of flag, put this in repo-wide config file?
cabal --reject-unconstrained-dependencies=all build all
cabal --reject-unconstrained-dependencies=all test all
```

### Generate documentation

First run the "cabal ... build all" command, then:

```
(cd docs && make)
```

The output appears in `docs/build`. You can point a HTTP server to this directory.
