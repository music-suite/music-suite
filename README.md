
# music-pitched

Overloaded pitch literals for Haskell. 

This package allow you to write the pitches of standard notation as expressions overloaded on result type. This works exactly like numeric literals (and string literals when using the `OverloadedStrings` extension).

For example:

```haskell
import Music.Pitched

frere :: Pitched a => a

frere1 =  [c, d, e, c]
       ++ [e, f, g]
       ++ [g, a, g, f, e, c]
       ++ [c, g_, c]
```


## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

    cabal configure
    cabal install
