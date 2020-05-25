% Music Suite

<a href="https://github.com/music-suite/music-suite" class="github-corner" aria-label="View source on GitHub"><svg width="80" height="80" viewBox="0 0 250 250" style="fill:#70B7FD; color:#fff; position: absolute; top: 0; border: 0; right: 0;" aria-hidden="true"><path d="M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path><path d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2" fill="currentColor" style="transform-origin: 130px 106px;" class="octo-arm"></path><path d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z" fill="currentColor" class="octo-body"></path></svg></a><style>.github-corner:hover .octo-arm{animation:octocat-wave 560ms ease-in-out}@keyframes octocat-wave{0%,100%{transform:rotate(0)}20%,60%{transform:rotate(-25deg)}40%,80%{transform:rotate(10deg)}}@media (max-width:500px){.github-corner:hover .octo-arm{animation:none}.github-corner .octo-arm{animation:octocat-wave 560ms ease-in-out}}</style>


Music Suite is a language for describing music, based on Haskell.

It allows representation and manipulation of music in a very general sense, that is compatible with standard notation and supporting a variety of [import and export formats](#import-and-export). The use of Haskell allows for music to be created, transformed or analyzed using the full expressive power of the Haskell language.

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

The Music Suite works particularly well with the following input and output formats.

* MusicXML
* Lilypond
* MIDI

For a full list of supported formats, see [Import and export](#import-and-export) for a more detailed overview.

<!--
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

-->

<!--
For an introduction, see [User Guide](User-Guide).
-->

[Haskell]:      http://www.haskell.org/haskellwiki/Haskell
[Haskore]:      http://www.haskell.org/haskellwiki/Haskore
[Euterpea]:     http://haskell.cs.yale.edu/euterpea
[Diagrams]:     http://projects.haskell.org/diagrams
[Reactive]:     http://hackage.haskell.org/package/reactive
[dsl]:          http://www.haskell.org/haskellwiki/Embedded_domain_specific_language
