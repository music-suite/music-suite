

## The Music Suite

The Music Suite is a system for creating, processing or analyzing music. It is based on the [Haskell][Haskell] language. It is designed with three goals in mind:

* Describe what the music *is*, rather than how it is to be performed.
* Avoid imposing stylistic or theoretical assumptions on the music.
* Include common notation and theory as a *special case*.

The Music Suite is both a language in its own right and a Haskell library. Being embedded in Haskell has several advantages, it allow the developers to focus on the contents and the users to make use of any feature in the Haskell language.

The Music Suite uses several advanced language constructs internally and requires a relatively new Haskell compiler (see [Installing the Suite](#installing-the-suite)).


### An example

To generate music we write an *expressions* such as this one:

```music+haskell
let
    m = staccato (scat [c,d,e,c]^/2) |> ab |> b_ |> legato (d |> c)^*2
in stretch (1/8) m
```

To transform music, we write a *function*. For example the following function halves all durations and transposes all pitches up a minor sixth:

```haskell
up m6 . compress 2
```

Applied to the above music we get:

```music
let
    transform = up m6 . compress 2
    m = staccato (scat [c,d,e,c]^/2) |> ab |> b_ |> legato (d |> c)^*2
in transform $ stretch (1/8) m
```

### Input and output

The Music Suite works well with the following input and output formats.

* MusicXML
* Lilypond
* ABC notation
* MIDI

Other formats are being added in the near future, see [Import and export](#import-and-export) for a more detailed overview.

### A note on the versioning

The Music Suite consists of a group of packages released concurrently under a common [optimistic version number](http://optver.hanshoglund.org/). The library was deliberately released *prematurely* in order to encourage its developers to work on it more.

### More information

For a complete reference, see the [reference documentation](/docs/api).


<!--
For an introduction, see [User Guide](User-Guide).
-->

[Haskell]:      http://www.haskell.org/haskellwiki/Haskell
[Haskore]:      http://www.haskell.org/haskellwiki/Haskore
[Euterpea]:     http://haskell.cs.yale.edu/euterpea
[Diagrams]:     http://projects.haskell.org/diagrams
[Reactive]:     http://hackage.haskell.org/package/reactive

