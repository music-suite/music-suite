

## The Music Suite

<!--
> *Please note:* The API and docs are not particularly stable at the moment. An official release note will appear in due time.
-->

The Music Suite is a declarative language embedded in [Haskell][Haskell] for generation and manipulation of music.<!-- It is is similar to [Haskore][Haskore] and [Euterpea][Euterpea], but its design and general philosophy is more inspired by [Diagrams][Diagrams] and [Reactive][Reactive]. -->

The Music Suite was designed with two key properties in mind:

* It should be usable with any kind of music. It should allow time, pitch and other properties to be represented in any way the user desires.

* It should include Western classical music theory as a *special case*, including a full representation of time, pitch, rhythm, dynamics, articulation, voice- and part structure, and so on. 

<!--
For this purpose, the Music Suite makes use of some advanced type system features in Haskell, in particular type classes and type families. Many function are generalized to work on any music representation, for example the function used to transpose music up one octave has the type `octavesUp :: (HasPitch' a, IsInterval (IntervalOf a)) => Integer -> a -> a`, meaning that octave transposition work on any type whose pitch type form an affine space.
-->


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

### More information

For a complete reference, see the [API documentation](/docs/api).

<!--
For an introduction, see [User Guide](User-Guide).
-->

[Haskell]:      http://www.haskell.org/haskellwiki/Haskell
[Haskore]:      http://www.haskell.org/haskellwiki/Haskore
[Euterpea]:     http://haskell.cs.yale.edu/euterpea
[Diagrams]:     http://projects.haskell.org/diagrams
[Reactive]:     http://hackage.haskell.org/package/reactive

