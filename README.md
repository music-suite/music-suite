## Music for Haskell

See <http://music-suite.github.io>.

## Building


Install Nix (2.3.1 tested, others might work)

```
nix-shell --pure
```

Inside the build shell, run:

```
cabal v2-build
cabal v2-test
```


