

# User Guide

## Installation

The Music Suite depends on the [Haskell platform][HaskellPlatform].

While not strictly required, [Lilypond][Lilypond] is highly recommended as it allow you to
preview musical scores. See [Import and Export](#import-and-export) for other formats.

To install the suite, simply install the Haskell platform, and then run:

    cabal install music-preludes


## Generating music

This chapter will cover how to use the Music Suite to generate music. Later on we will cover how to *import* and *transform* music.

### With music files

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

* `music2mid` 
* `music2musicxml` 
* `music2ly` 
* `music2wav` 
* `music2pdf`

### With Haskell files

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

Usually you will not want to invent a new representation from scratch, but rather start with a standard representation and customize it when needed. The default representation is defined in the `Music.Prelude.Basic` module, which is implicitly imported in all the examples below. See [Customizing the Music Representation](#customizing-music-representation) for other examples.

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

Music expressions can be composed @[<>]:

```music+haskell
c <> e <> g
```

TODO fundamentally, `<>` is the only way to compose music...

Or in sequence using @[|>]:

```music+haskell
c |> d |> e
```

Or partwise using @[</>]:

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
@[scat] (this is short for *sequential concatenation*).

```music+haskell
scat [c,e..g]^/4
```

For `x <> y <> z ..`, we can write `pcat [x, y, z]`.
@[pcat] (short for *parallel concatenation*).

```music+haskell
pcat [c,e..g]^/2
```



## Pitch

To facilitate the use of non-standard pitch, the standard pitch names are provided as overloaded values, referred to as *pitch literals*. 

To understand how this works, think about the type of numeric literal. The values $0, 1, 2$ etc. have type `Num a => a`, similarly, the pitch literals $c, d, e, f ...$ have type `IsPitch a => a`.

For Western-style pitch types, the standard pitch names can be used:

```music+haskell
scat [c, d, e, f, g, a, b]
```

Pitch names in other languages work as well, for example `ut, do, re, mi, fa, so, la, ti, si`. 

<!--
German names (using `h` and `b` instead of `b` and `bb`) can be approximated as follows:

```haskell
import Music.Preludes.Basic hiding (b)
import qualified Music.Pitch.Literal as P

h = P.b
b = P.bb
```
-->


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

Sharps and flats can be added by the functions @[sharp] and @[flat], which are written 
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

TODO overloading, explain why the following works:

```haskell
return (c::Note) == (c::Score Note)
```

## Dynamics

Dynamic values are overloaded in the same way as pitches. The dynamic literals are defined in `Music.Dynamics.Literal` and have type `IsDynamics a => a`.

An overview of the dynamic values:

```music+haskell
scat $ zipWith dynamics [fff,ff,_f,mf,mp,_p,pp,ppp] [c..]
```

TODO other ways of applying dynamics

## Articulation

Some basic articulation functions are @[legato], @[staccato], @[portato], @[tenuto], @[separated], @[spiccato]:

Some basic articulation functions are @[legato], @[staccato], @[portato], @[tenuto], @[separated], @[spiccato]:

Some basic articulation functions are @[legato], @[staccato], @[portato], @[tenuto], @[separated], @[spiccato]:

Some basic articulation functions are @[legato], @[staccato], @[portato], @[tenuto], @[separated], @[spiccato]:

Some basic articulation functions are @[legato], @[staccato], @[portato], @[tenuto], @[separated], @[spiccato]:


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

@[accent]
@[marcato]

```music+haskell
accent (scat [c..g]^/8)
    </>
marcato (scat [c..g]^/8)
```

@[accentLast]
@[accentAll]

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

## Tremolo

@[tremolo]

```music+haskell
tremolo 2 $ times 2 $ (c |> d)^/4
```

## Slides and glissando

@[slide]
@[glissando]

```music+haskell
glissando $ scat [c,d]^/2
```

## Harmonics

Use the @[harmonic] function:

```music+haskell
(harmonic 1 $ c^/2)
    </>
(harmonic 2 $ c^/2)
    </>
(harmonic 3 $ c^/2)
```
@[artificial]


## Text

@[text]

```music+haskell
text "pizz." $ c^/2
```

## Rests

Sometimes it is useful to work with scores that have a duration but no events.
This kind of score is represented by `rest` and has the type `Score (Maybe
Note)`. We use @[removeRests] to convert a `Score (Maybe a)`
into a `Score a`.

```music+haskell
removeRests $ times 4 (accent g^*2 |> rest |> scat [d,d]^/2)^/8
```
                 



# Transformations

## Time

@[rev]

```music+haskell
let
    melody = legato $ scat [d, scat [g,fs]^/2,bb^*2]^/4
in melody |> rev melody
```

@[times]

```music+haskell
let
    melody = legato $ scat [c,d,e,c]^/16
in times 4 $ melody
```

@[repeated]

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

@[invertAround]

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

@[Score]
@[Voice]
@[Track]
@[Delayable]
@[Stretchable]


# Meta-information

## Time signatures          

## Key signatures

## Rehearsal marks

## Miscellaneous

# Import and export

The standard distribution (installed as part of `music-preludes`) of the Music Suite includes a variety of input and output formats. There are also some experimental formats, which are distributed in separate packages, these are marked as experimental below.

The conventions for input or output formats is similar to the convention for properties (TODO ref above): for any type `a` and format `T a`, input formats are defined by a class or constraint `IsT`, and output by a format `HasT a`. For example, types that can be exported to Lilypond are defined by the constraint `HasLilypond a`, while types that can be imported from MIDI are defined by the constraint `IsMidi a`.

## MIDI

All standard representations support MIDI input and output. The MIDI representation uses [HCodecs](http://hackage.haskell.org/package/HCodecs) and the real-time support uses [hamid](http://hackage.haskell.org/package/hamid). 

<!--
You can read and write MIDI files using the functions @[readMidi] and @[writeMidi]. To play MIDI back in real-time, use @[playMidi] or @[playMidiIO], which uses [reenact](http://hackage.haskell.org/package/reenact).
-->

Beware that MIDI input may contain time and pitch values that yield a non-readable notation, you need a proper quantization software such as [
ScoreCleaner](http://scorecleaner.com) to convert raw MIDI input to quantized input.

## Lilypond

All standard representations support Lilypond output. The [lilypond](http://hackage.haskell.org/package/lilypond) package is used for parsing and pretty printing of Lilypond syntax. Lilypond is the recommended way of rendering music.

Lilypond input is not available yet but will hopefully be added soon.

An example:

```haskell
putStrLn $ toLyString $ asScore $ scat [c,d,e]
```

    <<
        \new Staff { <c'>1 <d'>1 <e'>1 }
    >>


## MusicXML

All standard representations support MusicXML output. The [musicxml2](http://hackage.haskell.org/package/musicxml2) package is used for 
parsing and pretty printing. 

The output is fairly complete, with some limitations ([reports][issue-tracker] welcome). There are no plans to support MusicXML import in the near future.

Beware of the extreme verboseness of XML, for example:

```haskell
putStrLn $ toXmlString $ asScore $ scat [c,d,e]
```

    <?xml version='1.0' ?>
    <score-partwise>
      <movement-title>Title</movement-title>
      <identification>
        <creator type="composer">Composer</creator>
      </identification>
      <part-list>
        <score-part id="P1">
          <part-name></part-name>
        </score-part>
      </part-list>
      <part id="P1">
        <measure number="1">
          <attributes>
            <key>
              <fifths>0</fifths>
              <mode>major</mode>
            </key>
          </attributes>
          <attributes>
            <divisions>768</divisions>
          </attributes>
          <direction>
            <direction-type>
              <metronome>
                <beat-unit>quarter</beat-unit>
                <per-minute>60</per-minute>
              </metronome>
            </direction-type>
          </direction>
          <attributes>
            <time symbol="common">
              <beats>4</beats>
              <beat-type>4</beat-type>
            </time>
          </attributes>
          <note>
            <pitch>
              <step>C</step>
              <alter>0.0</alter>
              <octave>4</octave>
            </pitch>
            <duration>3072</duration>
            <voice>1</voice>
            <type>whole</type>
          </note>
        </measure>
        <measure number="2">
          <note>
            <pitch>
              <step>D</step>
              <alter>0.0</alter>
              <octave>4</octave>
            </pitch>
            <duration>3072</duration>
            <voice>1</voice>
            <type>whole</type>
          </note>
        </measure>
        <measure number="3">
          <note>
            <pitch>
              <step>E</step>
              <alter>0.0</alter>
              <octave>4</octave>
            </pitch>
            <duration>3072</duration>
            <voice>1</voice>
            <type>whole</type>
          </note>
        </measure>
      </part>
    </score-partwise>
    

## ABC Notation

ABC notation (for use with [abcjs](http://code.google.com/p/abcjs/) or similar engines) is still experimental.

## Guido

Guido output (for use with the [GUIDO engine](http://guidolib.sourceforge.net/)) is not supported yet. This would be useful, as it allow real-time rendering of scores.

## Vextab

Vextab output (for use with [Vexflow](http://www.vexflow.com/)) is not supported yet.

## Sibelius

The [music-sibelius](http://hackage.haskell.org/package/music-sibelius) package provides experimental import of Sibelius scores (as MusicXML import is [not supported](#musicxml)).

<!--
This feature could of course also be used to convert Sibelius scores to other formats such as Guido or ABC without having to write in the ManuScript language used by Sibelius.
-->



# Customizing music representation

# Examples

Some more involved examples:

## Counterpoint

TODO about

```music+haskell
let subj = removeRests $ scat [ 
            scat [rest,c,d,e], 
            f^*1.5, scat[g,f]^/4, scat [e,a,d], g^*1.5,
            scat [a,g,f,e,f,e,d]^/2, c^*2 
        ]^/8

in (delay 1.5 . up _P5) subj </> subj
```

## Generative music

```music+haskell
let
    row = cycle [c,eb,ab,asPitch g]
    mel = asScore $ scat [d, scat [g,fs]^/2,bb^*2]^/4
in (take 25 $ row) `repeated` (\p -> up (asPitch p .-. c) mel)
```




@@@hslinks@@@