
```music+haskell
c
```

## Basics

## Entering pitch

```haskell
import Music.Pitch.Literal
```
This will bring the following standard pitch literals into scope:

```haskell
c, d, e, f, g, a, b
```
Notes with accidentals can be written by adding the `s` or `b` suffices (or two for double sharps and flats).

```haskell
cs, ds, es ...    -- sharp
cb, db, eb ...    -- flat
css, dss, ess ... -- double sharp
cbb, dbb, ebb ... -- double flat
```

There is also a convenience syntax for entering pitches one octave up or down, using `'` and `_` respectively.

```haskell
g a b c'
d c b_ c
```

Because of some overloading magic, we can actually write `sharp` and `flat` as *postfix* functions. This gives a better read in most languages:

```haskell
cs == c sharp
db == c flat
```

### Pitch functions

You can of course use typical functional transformation of pitch as well. For example `sharpen` and `flatten` are the ordinary (prefix) versions of `sharp` and `flat`

```haskell
sharpen c             == c sharp       == cs
flatten d             == d flat        == ds
(sharpen . sharpen) c == c doubleSharp == css
(flatten . flatten) d == d doubleFlat  == dss
```

Note that there is no guarantee that your pitch representation use enharmonic equivalence, so `cs == db` may or may not hold.

Other important pitch functions include `trans`, `up`, `down` and `invert`.


### Other languages

There are also modules for pitch names in other languages. They are equivalent in all but name.

```haskell
import Music.Pitch.Literal.German

c, d, e, f, g, a, h
cs, ds, es ...
cb, db, eb ...
```

```haskell
import Music.Pitch.Literal.Italian

do, re, mi, fa, sol
dod, red, mid ...
dob, reb, mib ...
```

### Qualified pitch literals

You can of course also use standard qualified imports for the literals.

```haskell
import qualified Music.Pitch.Literal as Pitch

Pitch.cs
```

### Non-standard pitch

TODO



## Entering dynamics

```haskell
fff
ff
_f
_p
pp
ppp
```

```haskell
import qualified Music.Dynamics.Literal as Dynamics

Dynamics.p
```

## Fixing the note type

As the pitch and dynamics literals are polymorphic, you will need to tell the compiler which type of notes you are using. The simplest way to do this to write a fixing function.

```haskell
type Note = Integer

score :: Score Note -> Score Note
score = id
```

You can now use `score` to instruct the compiler to use your specific note type.

```haskell
hs> playMidiIO $ score $ c
```
You usually want to define something like this as well:

```haskell
play = playMidiIO . score
open = openXml . score
```

Then you can write.

```haskell
hs> play $ c
hs> show $ c
```

The `music` package provide instances for many of the ordinary Haskell types, which can be replaced by adding your own. It will interpret the integral types as twelve-tone equal temperament pitches (using 69 as A440) and fractional types as logarithmic frequencies (using 69 as 440 Hz). To use other temperaments or fundamentals, see below.


## Basic composition

### Sequential composition

```music+haskell
c |> d |> e |> f |> g
```

### Parallel composition

```music+haskell
d </> fs </> a </> c
```

### Voice composition

Like parallel composition, but keeps the notes in separate voices that can be separated later. This requires a better note type. Try

```haskell
type Note = VoiceT VoiceName Integer
```

```haskell
c |> d |> e </> e_ |> g_ |> c
```

### Utility

As a shorthand for `x |> y |> z ..`, you can write `melody [x, y, z]`.

For `x <> y <> z ..`, you can write `chord [x, y, z]`.

For `(x <> y ..) |> (p <> q ..) ..`, you can write `chords [[x, y..], [p, q]..]..`.

For `(x |> y ..) <> (p |> q ..) ..`, you can write `melodies [[x, y..], [p, q]..]..`.


## Time and duration

The suite include two basic types for representing time, `Time` for absoulute time values and `Duration` for relative time values. Time values refer to a common reference time (usually the beginning of a musical piece), while duration in completely relative. We can think of times as points and durations as distances.

```haskell
2 :: Time
2 + 1/3 :: Time

2 :: Duration
2 + 1/3 :: Duration
```

To convert between time and duration use the [AffineSpace][AffineSpace] methods `.+^` (point plus distance) and `.-.` (point difference).

```haskell
let dist = 3
let point1 = 1
let point2 = point1 .+^ dist
point2 .-. point1 === dist
```

[AffineSpace]: http://hackage.haskell.org/packages/archive/vector-space/0.8.6/doc/html/Data-AffineSpace.html#t:AffineSpace

While `Time` is absolute in *offset*, it is still relative *scale*. To get a fully absolute time representation, use something like `Seconds`. This relative scaling, and the use of rational numers to represent times internally brings us a lot of power: we can freely scale, stretch and modify notes and events in continous time. When exported, times will be explicitly converted to seconds. If you prefer another scaling, simply multiply before exporting (i.e. for exporting hours instead of seconds, multiply by `3600`).

## Duration

You can extract the duration of a score using `duration`.

```haskell
duration $ melody [c,d,e]
```

### Scaling

```haskell
a^*2
a^/2
stretch 2 a
stretchTo 10 a
```

### Moving in time

```haskell
delay
move
moveBack
moveTo
```



## Dynamics


## Articulation

You can add articulation to a score using the following functions

```haskell
accent
marcato
```

```haskell
tenuto
separated
staccato
portato
legato
spiccato
```

All functions apply over the whole score on a per-voice basis, i.e. `accent` will add an accent to the first note (if any) in every voice. 

```haskell
accentAll
marcatoAll
accentLast
marcatoLast
```


This gives us some nice laws:

```haskell
legato (x |> y) == legato x |> legato y
accent (x <> y) == accent x |> accent y
etc
```

To differentiate voices you can use `mapVoice` and friends, i.e.

```haskell
mapVoice 2 (accent . dynamics ff) (c |> d </> e |> f)
legato `firstVoice` (c |> d </> e |> f)
```

## Slides and glissando

```haskell
slide
gliss
```

## Text

```haskell
text
rehearsal
```

## Playing techniques

```haskell
harmonic
artificial
```


## Ornaments

So far, only tremolo is supported.

```haskell
hs> show $ tremolo 2 (melody [c,d,e])
```

# Music export

## Midi playback

Midi playback is supported using the `hamid` package, a simple pure cross-platform Midi library. Note that `hamid` use the same representation of Midi as the `HCodecs` package.

## Midi files

## MusicXML files

## Lilypond files

## ABC notation

## Guido files




## About

The `music` suite is a set of Haskell libraries for creation, manipulation and analysis of music in Haskell. 

The current music libraries are:

* music-pitch
    * Musical pitch representations.
* music-pitch-literal               
    * Overloaded pitch names (provided separately from representations).
* music-dynamics
    * Musical dynamic representations.
* music-dynamics-literal
    * Overloaded dynamic names (provided separately from representations).
* music-articulation
    * Musical articulation and ornaments.
* music-parts
    * Part and voice represeentations.
* music-score
    * Contains musical temporal container types including notes, voices, tracks and 
      scores and functions to create, transform and traverse them. 
* music-preludes
    * Contains prelude-style modules with types for most common settings and 
      music representations, along with examples and tests.

Associated libraries designed to be used with the `music` suite (but not included in it) are:

* hamid
  * Cross-platform (realtime) midi.
* HCodecs
  * Haskell sound and midi file access. 
* musicxml2
  * A Haskell representation of MusicXML.
* lilypond
  * A Haskell representation of Lilypond music expressions.
* reenact
  * A new FRP library, used by the music suite.


## Credits

The music-suite is inspired by many projects including:

The following Haskell libraries have been influential.

* Haskore, Euterpia and HSoM
* Diagrams
* Reactive
* The HCodecs package was originally written by
* The hmidi package was originally written by

                        