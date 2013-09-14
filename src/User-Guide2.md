

# User Guide

## Installation

The Music Suite depends on the [Haskell platform][HaskellPlatform].

While not strictly required,[Lilypond][Lilypond] is highly recommended as it allow you to
preview musical scores. See [Import and Export](#import-and-export) for other formats.

To install the suite with all its dependencies:

    cabal install music-preludes


## Generating music

<!--
A piece of music is described by a *expressions* such as this one:

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

## Generating music with explicit Haskell modules
-->

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



# Writing music

## Music representations

One of the main points of the Music Suite is to avoid committing to a *single*, closed music representation. Instead it provides a set of types and type constructors that can be used to construct an arbitrary representation of music. 

Usually you will not want to invent a new representation from scratch, but rather start with a standard representation and customize it when needed.

## Basics

A single note can be entered by its name. This will render a note in the middle octave with position zero and duration one. Note that note values and durations correspond exactly, a duration of `1` is a whole note, a duration of `1/2` is a half note, and so on.

```music+haskell
c
```

To change the duration of a note, use `stretch` or `compress`.

```music+haskell
stretch (1/2) c
    </>
stretch 1 c         
    </>
stretch 2 c
```

TODO delay

Offset and duration is not limited to simple numbers. Here are some more complex examples:

```music+haskell
(c^*(9/8) |> d^*(3/8))
    </>
(compress 3 (scat [c,d,e]) |> f^*(3/4))
```

The `^*` and `^/` operators can be used as shorthands for `delay` and `compress`.

```music+haskell
(c |> d |> e |> c |> d^*2 |> d^*2)^/16
```


Allthough the actual types are more general, you can think of `c` as an expression
of type `Score Note`, and the transformations as functions `Score Note -> Score Note`.

```music+haskell
up (perfect octave) . compress 2 . delay 3 $ c
```


## Composing

Music expressions can be composed [`<>`][<>]:

```music+haskell
c <> e <> g
```

TODO fundamentally, `<>` is the only way to compose music...

Or in sequence using [`|>`][|>]:

```music+haskell
c |> d |> e
```

Or partwise using [`</>`][</>]:

```music+haskell
c </> e </> g
```

Here is a more complex example:

```music+haskell
let
    triad a = a <> up _M3 a <> up _P5 a
in up _P8 (scat [c,d,e,f,g,a,g,f]^/8) </> (triad c)^/2 |> (triad g_)^/2
```

As a shorthand for `x |> y |> z ..`, we can write `scat [x, y, z]`.
[`scat`][scat] (this is short for *sequential concatenation*).

```music+haskell
scat [c,e..g]^/4
```

For `x <> y <> z ..`, we can write `pcat [x, y, z]`.
[`pcat`][pcat] (short for *parallel concatenation*).

```music+haskell
pcat [c,e..g]^/2
```



## Pitch

To facilitate the use of non-standard pitch, the standard pitch and interval names are provided as overloaded values, referred to as *literals*. This is very similar to how numeric overloading works in Haskell. The number literals `0,1,2,3,4...` can be used with any type that is an instance of `Num`, similarly, the pitch literals `c,d,e,f...` can be used with any type that is an instance of `IsPitch`.

Standard pitch names:

```music+haskell
scat [c, d, e, f, g, a, b]
```

You can change octave using `octavesUp` and `octavesDown`:

```music+haskell
octavesUp 4 c
    </>
octavesUp (-1) c
    </>
octavesDown 2 c
```

Shorter syntax for other octaves:

```music+haskell
c__ |> c_ |> c |> c' |> c''
```

Sharps and flats can be added by the functions [`sharp`][sharp] and [`flat`][sharp`][sharp] and [`flat], which are written 
*postfix* thanks to some overloading magic.

```music+haskell
c sharp |> d |> e flat
```

You can also use the ordinary (prefix) versions `sharpen` and `flatten`.

```music+haskell
sharpen c 
    </> 
(sharpen . sharpen) c
```

Shorter syntax for sharp and flat notes:

```music+haskell
(cs |> ds |> es)    -- sharp
    </>
(cb |> db |> eb)    -- flat
```

Here is an overview of all pitch notations:

```haskell
sharpen c             == c sharp       == cs
flatten d             == d flat        == ds
(sharpen . sharpen) c == c doubleSharp == css
(flatten . flatten) d == d doubleFlat  == dss
```

Note that there is no guarantee that your pitch representation use enharmonic equivalence, so `cs == db` may or may not hold.

There is nothing special about the pitch and interval literals, they are simply values exported by the `Music.Pitch.Literal` module. While this module is reexported by the standard music preludes, you can also import it qualified if you want to avoid bringing the single-letter pitch names into scope.

```haskell
Pitch.c |> Pitch.d
```


## Dynamics


[`dynamics`][dynamics]

```music+haskell
dynamics _p c
```

## Articulation

Some basic articulation functions are [`legato`][legato], [`staccato`][staccato], [`portato`][portato], [`tenuto`][tenuto], [`separated`][separated], [`spiccato`][legato`][legato], [`staccato`][staccato], [`portato`][portato], [`tenuto`][tenuto], [`separated`][separated], [`spiccato]:

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

[`accent`][accent]
[`marcato`][marcato]

```music+haskell
accent (scat [c..g]^/8)
    </>
marcato (scat [c..g]^/8)
```

[`accentLast`][accentLast]
[`accentAll`][accentAll]

```music+haskell
accentLast (scat [c..g]^/8)
    </>
accentAll (scat [c..g]^/8)
```

Applying articulations over multiple parts:

```music+haskell     
let
    p1 = scat [c..b]^/4
    p2 = delay (2/4) $ scat [c..b]^/4
    p3 = delay (5/4) $ scat [c..b]^/4
in (accent . legato) (p1 </> p2 </> p3)
```

## Tremolo

[`tremolo`][tremolo]

```music+haskell
tremolo 2 $ times 2 $ (c |> d)^/2
```

## Slides and glissando

[`slide`][slide]
[`glissando`][glissando]

```music+haskell
glissando $ scat [c,d]^/2
```

## Harmonics

Use the [`harmonic`][harmonic] function:

```music+haskell
(harmonic 1 $ c^/2)
    </>
(harmonic 2 $ c^/2)
    </>
(harmonic 3 $ c^/2)
```
[`artificial`][artificial]


## Text

[`text`][text]

```music+haskell
text "pizz." $ c^/2
```

## Rests

Sometimes it is useful to work with scores that have a duration but no events.
This kind of score is represented by `rest` and has the type `Score (Maybe
Note)`. We use [`removeRests`][removeRests] to convert a `Score (Maybe a)`
into a `Score a`.

```music+haskell
removeRests $ times 4 (accent g^*2 |> rest |> scat [d,d]^/2)^/8
```
                 



# Transformations

## Time

[`retrograde`][retrograde]

```music+haskell
let
    melody = legato $ scat [d, scat [g,fs]^/2,bb^*2]^/4
in melody |> retrograde melody
```

[`times`][times]

```music+haskell
let
    melody = legato $ scat [c,d,e,c]^/16
in times 4 $ melody
```

[`repeated`][repeated]

```music+haskell
let 
    m = legato $ scat [c,d,scat [e,d]^/2, c]^/4 
in [c,eb,ab,g] `repeated` (\p -> up (asPitch p .-. c) m)
```

### Onset and duration

```music+haskell
let                
    melody = asScore $ legato $ scat [scat [c,d,e,c], scat [e,f], g^*2]
    pedal  = asScore $ delayTime (onset melody) $ stretch (duration melody) $ c_
in compress 4 $ melody </> pedal
```

## Pitch

[`invertAround`][invertAround]

```music+haskell
(scat [c..g]^*(2/5))
    </>
(invertAround c $ scat [c..g]^*(2/5))
    </>
(invertAround e $ scat [c..g]^*(2/5))
```


### Pitches and intervals

### Name and accidental

### Spelling

### Quality and number


## Intonation

TODO

### Inspecting dissonant intervals

### Semitones and enharmonic equivalence

### Spelling

### Scales

### Chords



## Parts

### Instrument, part and sub-part

### Extracting and modifying parts

### Part composition



# Time-based structures

[`Score`][Score]
[`Voice`][Voice]
[`Track`][Track]
[`Delayable`][Delayable]
[`Stretchable`][Stretchable]


# Meta-information

## Basic information

## Time signatures          

## Key signatures

## Rehearsal marks

## Miscellaneous

# Import and export

## MIDI
## MusicXML
## Lilypond
## ABC Notation
## Guido
## Vextab


# Customizing music representation

# Examples

```music+haskell
let subj = removeRests $ scat [ 
            scat [rest,c,d,e], 
            f^*1.5, scat[g,f]^/4, scat [e,a,d], g^*1.5,
            scat [a,g,f,e,f,e,d]^/2, c^*2 
        ]^/8

in (delay (6/4) $ up (perfect fifth) subj) </> subj
```





















[scat]:         /docs/api/Music-Time-Juxtapose.html#v:scat
[pcat]:         /docs/api/Music-Time-Juxtapose.html#v:pcat
[times]:        /docs/api/Music-Time-Juxtapose.html#v:times
[<>]:           http://hackage.haskell.org/packages/archive/semigroups/0.9.2/doc/html/Data-Semigroup.html#v:-60--62-
[|>]:           /docs/api/Music-Time-Juxtapose.html#v:-124--62-
[</>]:          /docs/api/Music-Score-Combinators.html#v:-60--47--62-

[delay]:        /docs/api/Music-Time-Delayable.html#v:delay
[stretch]:      /docs/api/Music-Time-Stretchable.html#v:stretch
[compress]:     /docs/api/Music-Time-Stretchable.html#v:compress
[removeRests]:  /docs/api/Music-Score-Combinators.html#v:removeRests

[dynamics]:     /docs/api/Music-Score-Dynamics.html#v:dynamics

[Score]:        /docs/api/Music-Score-Score.html#v:Score
[Voice]:        /docs/api/Music-Score-Score.html#v:Voice
[Track]:        /docs/api/Music-Score-Score.html#v:Track
[Delayable]:    /docs/api/Music-Time-Delayable.html#v:Delayable
[Stretchable]:  /docs/api/Music-Time-Stretchable.html#v:Stretchable


[legato]:       /docs/api/Music-Score-Articulation.html#v:legato
[staccato]:     /docs/api/Music-Score-Articulation.html#v:staccato
[portato]:      /docs/api/Music-Score-Articulation.html#v:portato
[tenuto]:       /docs/api/Music-Score-Articulation.html#v:tenuto
[separated]:    /docs/api/Music-Score-Articulation.html#v:separated
[spiccato]:     /docs/api/Music-Score-Articulation.html#v:spiccato

[accent]:       /docs/api/Music-Score-Articulation.html#v:accent
[marcato]:      /docs/api/Music-Score-Articulation.html#v:marcato
[accentLast]:   /docs/api/Music-Score-Articulation.html#v:accentLast
[accentAll]:    /docs/api/Music-Score-Articulation.html#v:accentAll

[retrograde]:   /docs/api/Music-Score-Combinators.html#v:retrograde

[harmonic]:     /docs/api/Music-Score-Ornaments.html#v:harmonic
[artificial]:   /docs/api/Music-Score-Ornaments.html#v:artificial
[tremolo]:      /docs/api/Music-Score-Ornaments.html#v:tremolo
[glissando]:    /docs/api/Music-Score-Ornaments.html#v:glissando
[slide]:        /docs/api/Music-Score-Ornaments.html#v:slide
[text]:         /docs/api/Music-Score-Ornaments.html#v:text

[up]:           /docs/api/Music-Score-Pitch.html#v:up
[down]:         /docs/api/Music-Score-Pitch.html#v:down

[catMaybes]:    http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Maybe.html#v:catMaybes


[Lilypond]:         http://lilypond.org
[Timidity]:         http://timidity.sourceforge.net/
[HaskellPlatform]:  http://www.haskell.org/platform/

[`foobar`][foobar]


<!-- Unknown: <> No such identifier: <>-->


<!-- Unknown: |> No such identifier: |>-->


<!-- Unknown: </> No such identifier: </>-->

[scat]: /docs/api/Music-Time-Juxtapose.html#v:scat
[pcat]: /docs/api/Music-Time-Juxtapose.html#v:pcat

<!-- Unknown: sharp`][sharp] and [`flat No such identifier: sharp`][sharp] and [`flat-->

[dynamics]: /docs/api/Music-Score-Dynamics.html#v:dynamics

<!-- Unknown: legato`][legato], [`staccato`][staccato], [`portato`][portato], [`tenuto`][tenuto], [`separated`][separated], [`spiccato No such identifier: legato`][legato], [`staccato`][staccato], [`portato`][portato], [`tenuto`][tenuto], [`separated`][separated], [`spiccato-->

[accent]: /docs/api/Music-Score-Articulation.html#v:accent
[marcato]: /docs/api/Music-Score-Articulation.html#v:marcato
[accentLast]: /docs/api/Music-Score-Articulation.html#v:accentLast
[accentAll]: /docs/api/Music-Score-Articulation.html#v:accentAll
[tremolo]: /docs/api/Music-Score-Ornaments.html#v:tremolo
[slide]: /docs/api/Music-Score-Ornaments.html#v:slide
[glissando]: /docs/api/Music-Score-Ornaments.html#v:glissando
[harmonic]: /docs/api/Music-Score-Ornaments.html#v:harmonic
[artificial]: /docs/api/Music-Score-Ornaments.html#v:artificial
[text]: /docs/api/Music-Score-Ornaments.html#v:text
[removeRests]: /docs/api/Music-Score-Combinators.html#v:removeRests
[retrograde]: /docs/api/Music-Score-Combinators.html#v:retrograde
[times]: /docs/api/Music-Time-Juxtapose.html#v:times
[repeated]: /docs/api/Music-Time-Juxtapose.html#v:repeated
[invertAround]: /docs/api/Music-Score-Pitch.html#v:invertAround
[Score]: /docs/api/Music-Score-Score.html#t:Score
[Voice]: /docs/api/Music-Score-Voice.html#t:Voice
[Track]: /docs/api/Music-Score-Track.html#t:Track
[Delayable]: /docs/api/Music-Time-Delayable.html#t:Delayable
[Stretchable]: /docs/api/Music-Time-Stretchable.html#t:Stretchable

<!-- Unknown: foobar No such identifier: foobar-->


