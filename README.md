## Music for Haskell

See <http://music-suite.github.io>.

## Building


Install Nix (2.3.1 tested, others might work)

```
nix-shell --pure
```

Inside the build shell, run:

```
cabal v2-update
cabal --reject-unconstrained-dependencies=all v2-test
```


