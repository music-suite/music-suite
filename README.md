[![CircleCI](https://circleci.com/gh/hanshoglund/music-suite.svg?style=svg)](https://circleci.com/gh/hanshoglund/music-suite)

# Music Suite

Music Suite is a language for describing music, based on Haskell.

See <http://music-suite.github.io>.

## Building


Install Nix (2.3.1 tested, others might work)

```
nix-shell --pure
```

Inside the build shell, run:

```
cabal v2-update
cabal --reject-unconstrained-dependencies=all v2-build
cabal --reject-unconstrained-dependencies=all v2-test
```


