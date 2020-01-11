% Music Suite

## Music Suite

Music Suite is a language for describing music, based on Haskell.

It allow representation and manipulation of music in a very general sense, that is compatible with standard notation and supporting a variety of [import and export formats](#import-and-export). The use of Haskell allow for music to be created, transformed or analyzed using the full expressive power of the Haskell language.


### An example

To generate music we write an *expressions* such as this one:

```music+haskell
let
    m = staccato (pseq [c,d,e,c]|/2) |> ab |> b_ |> legato (d |> c)|*2
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
    m = staccato (pseq [c,d,e,c]|/2) |> ab |> b_ |> legato (d |> c)|*2
in transform $ stretch (1/8) m
```

### Input and output

The Music Suite works well with the following input and output formats.

* MusicXML
* Lilypond
* ABC notation
* MIDI

Other formats are being added in the near future, see [Import and export](#import-and-export) for a more detailed overview.

### Version numbers and stability

The Music Suite consists of a group of packages released concurrently under a common version number. The [music-suite](http://hackage.haskell.org/package/music-suite) acts as a meta-package that includes all stable packages of the Suite.

Please note that the Suite is quite usable, parts of it are still experimental, and we expect the API to change slightly with every release up to v2.0.0 (think of it as [optimistic versioning](http://semver.org)). If you have any problems with upgrading from a previous version, please post to the discussion group.


### Contributing to the Suite

If you are interested in contributing to the Suite, please join the Github organization (see the link below). In addition to code, we appreciate contributions in the form of tutorials, examples or musical compositions. Hopefully we may soon have a showcase of works created with Music Suite, like the [Diagrams gallery](http://projects.haskell.org/diagrams/gallery.html).

### More information

- All releases on [Hackage](http://hackage.haskell.org/package/music-suite)

- The [full API documentation](/docs/api)

- The [source code on Github](https://github.com/music-suite)

- For more examples, see [music-preludes/examples](https://github.com/music-suite/music-preludes/tree/master/examples) directory. You can download this directory from Hackage using `cabal unpack music-preludes`.

- For bug reports, please use the relevant Github tracker, i.e. for `music-score` use <https://github.com/music-suite/music-score/issues>

- For questions, feedback and general discussion, see [the Google discussion group](http://groups.google.com/d/forum/music-suite-discuss)



<!--
For an introduction, see [User Guide](User-Guide).
-->

[Haskell]:      http://www.haskell.org/haskellwiki/Haskell
[Haskore]:      http://www.haskell.org/haskellwiki/Haskore
[Euterpea]:     http://haskell.cs.yale.edu/euterpea
[Diagrams]:     http://projects.haskell.org/diagrams
[Reactive]:     http://hackage.haskell.org/package/reactive
[dsl]:          http://www.haskell.org/haskellwiki/Embedded_domain_specific_language
