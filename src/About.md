

## The Music Suite

<!--
> *Please note:* The API and docs are not particularly stable at the moment. An official release note will appear in due time.
-->

The Music Suite is a language based on [Haskell][Haskell] for creating, processing or analyzing music. It can be used on its own or as a Haskell library.

The Music Suite is designed to:

* Be usable with any kind of music. There are no stylistic or theoretical assumptions on how music should be described.
* It should include standard notation as a *special case*. 



### An example

To generate music we write an *expressions* such as this one:

```music+haskell
let
    m = staccato (scat [c,d,e,c]^/2) |> ab |> b_ |> legato (d |> c)^*2
in stretch (1/8) m
```

To transform music, we write a *function*. For example the following function halves all durations and transposes all pitches up a minor sixth:

```haskell
up (minor sixth) . compress 2
```

Applied to the above music we get:

```music
let
    transform = up (minor sixth) . compress 2
    m = staccato (scat [c,d,e,c]^/2) |> ab |> b_ |> legato (d |> c)^*2
in transform $ stretch (1/8) m
```

### Input and output

The Music Suite works well with the following input and output formats:

* MusicXML
* Lilypond
* ABC notation
* MIDI

### A note on the versioning

The Music Suite consists of a group of packages released concurrently under a common [semantic version number](http://semver.org/). The library was deliberately released *prematurely* in order to encourage its creator to work on it more: we expect the first truly stable version to be *2.0*.

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

