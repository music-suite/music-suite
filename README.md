
# music-pitch-literal

This package allow you to write the pitches of standard notation as expressions
overloaded on result type. This works exactly like numeric literals.

This library is part of the Haskell Music Suite, see <http://musicsuite.github.com>.

For example:

```haskell
import Music.Pitch.Literal

song :: Pitched a => [a]
song = mempty
    |> melody [c, d, e, c]
    |> melody [e, f, g]
    |> melody [g, a, g, f, e, c]
    |> melody [c, g_, c]
```


## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

    cabal configure
    cabal install
