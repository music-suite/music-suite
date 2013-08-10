


## Getting started

### Installation

The Music Suite depends on the [Haskell platform](http://www.haskell.org/platform/) as well as some
other Haskell libraries which can be installed automatically. To install the
suite, simply:

1. Get the latest version of the Haskell Platform from [http://www.haskell.org/platform](http://www.haskell.org/platform)
2. Run `cabal install music-preludes`

It is highly recommended to also install the following software, which will make your life much more pleasant while using the suite.

* [Lilypond](http://lilypond.org)
* [Timidity](http://timidity.sourceforge.net/)


## Generating music

A piece of music is described by a *music expressions* such as this one:

```haskell
c |> d |> e
```

The simplest way to render this expression is to save it in a file named
`foo.music` (or similar) and convert it using `music2pdf foo.music`. This
should render a file called `foo.pdf` containing the following:

```music
c |> d |> e
```

There are several programs for converting music expressions:

* `music2musicxml` converts a Music file to MusicXML
* `music2mid` converts a Music file to MIDI
* `music2ly` converts a Music file to a Lilypond file
* `music2wav` converts a Music file to audio (using Timidity)
* `music2pdf` converts a Music file to graphics (using Lilypond)

### Generating music with explicit Haskell modules

Alternatively, you can create a file called `test.hs` (or similar) with the following structure:

```haskell
import Music.Prelude.Basic

music = c |> d |> e
main = defaultMain music
```

Then either execute it using:

    $ runhaskell test.hs
    
or compile and run it with

    $ ghc --make test
    $ ./test

However, `music.hs` can also be loaded into a Haskell interpreter or compiled.
In this case the resulting program will generate and open a file called
`test.pdf` containing the output seen above.

In fact, the `music2pdf` program is a simple utility that substitutes a single expression into a Haskell module such as the one above and executes the resulting main function.


## Writing music

TODO import `Music.Prelude.Basic` etc.

The simplest music expression is just a single note.

```music+haskell
c
```

As you might have guessed, entering a single note name will render a note in
the middle octave with position zero and duration one. To enter other octaves,
positions and durations we apply transformations such as [`up`][up],
[`down`][down], [`delay`][delay] or [`stretch`][stretch].

```music+haskell
up (perfect octave) . compress 2 . delay 1 $ c
```

Music expressions can be composed with the sequential operator `|>`:

```music+haskell
c |> d |> e
```

Or in parallel using `<>`:

```music+haskell
c <> e <> g
```

Or partwise using `</>`:

```music+haskell
c </> e </> g
```

These can be combined:

```music+haskell
let
    triad a = a <> up _M3 a <> up _P5 a
in up _P8 (scat [c,d,e,f,g,a,g,f]^/8) </> (triad c)^/2 |> (triad g_)^/2
```

The `^*` and `^/` operators can be used as shorthands for `delay` and `compress`.

```music+haskell
(c |> d |> e |> c |> d^*2 |> d^*2)^/16
```

As a shorthand for `x |> y |> z ..`, we can write `scat [x, y, z]`.

For `x <> y <> z ..`, we can write `pcat [x, y, z]`.

```music+haskell
scat [c..g]^/4
```

```music+haskell
pcat [c,e..d']^/2
```


Other octaves

```music+haskell
(c_ |> b_ |> c |> b |> c')^/8
```

## Transformations

```music+haskell
let
    m = scat [c,e,g,f]^/4
in m |> retrograde m
```

```music+haskell
let
    m = scat [c,d,e,c]^/4
in times 4 m
```

```music+haskell
let
    m = scat [c,d,e,c]^/4
in [m,m,m,m] `repeated` (up $ major second)
```


## Articulation

Some basic articulation functions are [`legato`][legato], [`staccato`][staccato], [`portato`][portato], [`tenuto`][tenuto], [`separated`][separated], [`spiccato`][spiccato]:

```music+haskell
legato (scat [c..g]^/8)
    </>
staccato (scat [c..g]^/8)
    </>
portato (scat [c..g]^/8)
    </>
tenuto (scat [c..g]^/8)
    </>
separated (scat [c..g]^/8)
    </>
spiccato (scat [c..g]^/8)
```

```music+haskell
accent (scat [c..g]^/8)
    </>
marcato (scat [c..g]^/8)
```

```music+haskell
accentLast (scat [c..g]^/8)
    </>
accentAll (scat [c..g]^/8)
```

Applying articulations over multiple parts:

```music+haskell     
let
    p1 = scat [c..b]^/4
    p2 = delay (1/4) $ scat [c..b]^/4
    p3 = delay (3/4) $ scat [c..b]^/4
in (accent . legato) (p1 </> p2 </> p3)
```

## Adding dynamics


```music+haskell
dynamics ppp (times 4 (f |> e |> f |> g)^/16)
```

## Adding tremolo

```music+haskell
tremolo 2 $ times 2 $ (c |> d)^/4
```

## Slides and glissando

```music+haskell
slide $ scat [c,d]^/2
```

## Adding harmonics

```music+haskell
(harmonic 1 $ c^/2)
    </>
(harmonic 2 $ c^/2)
    </>
(harmonic 3 $ c^/2)
```

## Adding text

```music+haskell
text "pizz." $ c^/2
```

## Working with rests

Usually, scores contain only notes and *space*, i.e. everything in between notes. Sometimes it is useful to
work with scores that have a duration but no events. This kind of score is represented by `rest` and has the
type `Score (Maybe Note)`. We use [`removeRests`][removeRests] to convert a `Score (Maybe a)` into a `Score a`.

```music+haskell
removeRests $ compress 4 $ c |> rest^*2 |> d
```

## Working with chords

TODO

## Working with parts

TODO

## Working with onset and duration

As music expressions are Haskell expressions, we can use the `let` syntax.

```music+haskell
let                
    melody = asScore $ legato $ scat [scat [c,d,e,c], scat [e,f], g^*2]
    pedal  = asScore $ delayTime (onset melody) $ stretch (duration melody) $ c'
in compress 4 $ melody </> pedal
```


## Using variables

As music expressions are Haskell expressions, we can use the `let` syntax.

```music+haskell
let                
    subj   = removeRests $ scat [scat [rest, c',ab,db'], e^*2, scat [f,g], ab^*1.5, bb^/2, c'^*2]^/4
    entry1 = legato $ subj
    entry2 = legato $ delay 2 $ up (perfect fifth) subj
in entry2 </> entry1
```



[delay]:        http://musicsuite.github.io/docs/api/Music-Time-Delayable.html#v:delay
[stretch]:      http://musicsuite.github.io/docs/api/Music-Time-Stretchable.html#v:stretch
[compress]:     http://musicsuite.github.io/docs/api/Music-Time-Stretchable.html#v:compress
[removeRests]:  http://musicsuite.github.io/docs/api/Music-Score-Combinators.html#v:removeRests

[legato]:       http://musicsuite.github.io/docs/api/Music-Score-Articulation.html#v:legato
[staccato]:     http://musicsuite.github.io/docs/api/Music-Score-Articulation.html#v:staccato
[portato]:      http://musicsuite.github.io/docs/api/Music-Score-Articulation.html#v:portato
[tenuto]:       http://musicsuite.github.io/docs/api/Music-Score-Articulation.html#v:tenuto
[separated]:    http://musicsuite.github.io/docs/api/Music-Score-Articulation.html#v:separated
[spiccato]:     http://musicsuite.github.io/docs/api/Music-Score-Articulation.html#v:spiccato

[up]:           http://musicsuite.github.io/docs/api/Music-Score-Pitch.html#v:up
[down]:         http://musicsuite.github.io/docs/api/Music-Score-Pitch.html#v:down


