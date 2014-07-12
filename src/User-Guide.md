

# Getting Started

## Installing the Suite

The Music Suite depends on the [Haskell platform][HaskellPlatform].

While not strictly required, [Lilypond][Lilypond] is highly recommended as it allow you to
preview musical scores. See [Import and Export](#import-and-export) for other formats.

To install the suite, simply install the Haskell platform, and then run:

    cabal install music-suite


## Writing music

This chapter will cover how to use the Music Suite to generate music. Later on we will cover how to *import* and *transform* music.

One of the main points of the Music Suite is to avoid committing to a *single*, closed music representation. Instead it provides a set of types and type constructors that can be used to construct an arbitrary representation of music. 

Usually you will not want to invent a new representation from scratch, but rather start with a standard representation and customize it when needed. The default representation is defined in the `Music.Prelude.Basic` module, which is implicitly imported in all the examples below. See [Customizing the Music Representation](#customizing-music-representation) for other examples.


### With Music files

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

While most simple music files contains only a single expression, it is also possible to write music files in [declaration style](http://www.haskell.org/haskellwiki/Declaration_vs._expression_style).

In this case you can provide an arbitrary number of top-level declarations, including type, class and instance declarations. You must provide a single top-level declaration called `example` which takes the place of the top-level music expression.

```music+haskell
data Foo = Foo

instance Eq Foo where
  Foo == Foo = True

example = chord |> up d5 chord
  where
    chord = c <> e <> g
```

There are several programs for converting music files:

* `music2midi` – converts to MIDI
* `music2musicxml` – converts to MusicXML
* `music2ly` – converts to Lilypond input files
* `music2pdf` – converts to PDF (using Lilypond)
* `music2png` – converts to PNG (using Lilypond)

### With Haskell files

Alternatively, you can create a file called `test.hs` (or similar) with the following structure:

```haskell
import Music.Prelude.Basic

main = defaultMain music
music = c |> d |> e
```

Then either execute it using:

    $ runhaskell test.hs
    
or compile and run it with

    $ ghc --make test
    $ ./test

In this case the resulting program will generate and open a file called
`test.pdf` containing the output seen above.

Music files and Haskell files using `defaultMain` are equivalent in every aspect. In fact, the `music2...` programs are simple utilities that substitutes a single expression into a Haskell module such as the one above and executes the resulting main function.

### Interactive use

An advantage of Haskell files is that you can load them into a Haskell interpreter.

TODO configuration

Here is an example `.ghci` file.

```
:m + Music.Prelude
:def! open  (\x -> return $ "open  $ asScore $ "++ x)
:def! play  (\x -> return $ "play  $ asScore $ "++ x)
:def! write (\x -> return $ "write $ asScore $ "++ x)
putStrLn "Welcome to the Music Suite!"
putStrLn "Try :open <music> or :play <music>"
```

## Time and duration

A single note can be entered by its name. This will render a note in the middle octave with a duration of one. Note that note values and durations correspond exactly, a duration of `1` is a whole note, a duration of `1/2` is a half note, and so on.

```music+haskell
c
```

To change the duration of a note, use @[stretch] or @[compress]. Note that:
    
```haskell
compress x = stretch (1/x)
```

for all values of *x*.

```music+haskell
stretch (1/2) c
```

```music+haskell
stretch 2 c         
```

```music+haskell
stretch (4+1/2) c
```

TODO delay

Offset and duration is not limited to simple numbers. Here are some more complex examples:

```music+haskell
c^*(9/8) |> d^*(7/8)
```

```music+haskell
stretch (2/3) (scat [c,d,e]) |> f^*2
```

As you can see, note values, tuplets and ties are added automatically

The `^*` and `^/` operators can be used as shorthands for `delay` and `compress`.

```music+haskell
(c |> d |> e |> c |> d^*2 |> d^*2)^/16
```


Allthough the actual types are more general, you can think of `c` as an expression
of type `Score Note`, and the transformations as functions `Score Note -> Score Note`.

```music+haskell
up _P8 . compress 2 . delay 3 $ c
```


## Composition

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
    scale = scat [c,d,e,f,g,a,g,f]^/8
    triad a = a <> up _M3 a <> up _P5 a
in up _P8 scale </> (triad c)^/2 |> (triad g_)^/2
```

As a shorthand for `x |> y |> z ..`, we can write @[scat] `[x, y, z]` (short for *sequential concatenation*).

```music+haskell
scat [c,e..g]^/4
```

For `x <> y <> z ..`, we can write @[pcat] `[x, y, z]` (short for *parallel concatenation*).

```music+haskell
pcat [c,e..g]^/2
```

Actually, @[scat] and @[pcat] used to be called `melody` and `chord` back in the days, but
I figured out that these are names that you actually want to use in your own code.

## Pitch

### Pitch names

To facilitate the use of non-standard pitch, the standard pitch names are provided as overloaded values, referred to as *pitch literals*. 

To understand how this works, think about the type of numeric literal. The values $0, 1, 2$ etc. have type `Num a => a`, similarly, the pitch literals $c, d, e, f ...$ have type @[IsPitch] `a => a`.

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

There is also a shorthand for other octaves:

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

As you might expect, there is also a shorthand for sharp and flat notes:

```music+haskell
(cs |> ds |> es)    -- sharp
    </>
(cb |> db |> eb)    -- flat
```

Here is an overview of all pitch notations:

```haskell
sharpen c             == c sharp       == cs
flatten d             == d flat        == db
(sharpen . sharpen) c == c doubleSharp == css
(flatten . flatten) d == d doubleFlat  == dss
```

Note that `cs == db` may or may not hold depending on which pitch representation you use.

### Interval names

Interval names are overloaded in a manner similar to pitches, and are consequently referred to as *interval literals*. The corresponding class is called @[IsInterval].

Here and elsewhere in the Music Suite, the convention is to follow standard theoretical
notation, so *minor* and *diminished* intervals are written in lower-case, while *major*
and *perfect* intervals are written in upper-case. Unfortunately, Haskell does not support
overloaded upper-case values, so we have to adopt an underscore prefix:

```haskell
minor third      == m3
major third      == _M3
perfect fifth    == _P5
diminished fifth == d5
minor ninth      == m9
```

Similar to @[sharpen] and @[flatten], the @[augment] and @[diminish] functions can be used
to alter the size of an interval. For example:

```music+haskell
let
    intervals = [diminish _P5, (diminish . diminish) _P5]
in scat $ fmap (`up` c) intervals
```

You can add pitches and intervals using the @[.-.] and @[.+^] operators. To memorize these
operators, think of pitches and points `.` and intervals as vectors `^`.



### Qualified pitch and interval names

There is nothing special about the pitch and interval literals, they are simply values exported by the `Music.Pitch.Literal` module. While this module is reexported by the standard music preludes, you can also import it qualified if you want to avoid bringing the single-letter pitch names into scope.

```haskell
Pitch.c |> Pitch.d .+^ Interval.m3
```

TODO overloading, explain why the following works:

```haskell
return (c::Note) == (c::Score Note)
```

## Dynamics

Dynamic values are overloaded in the same way as pitches. The dynamic literals are defined in `Music.Dynamics.Literal` and have type `IsDynamics a => a`.

An overview of the dynamic values:

```music+haskell
scat $ zipWith level [fff,ff,_f,mf,mp,_p,pp,ppp] [c..]
```

TODO other ways of applying level

## Articulation

Some basic articulation functions are @[legato], @[staccato], @[portato], @[tenuto], @[separated], @[staccatissimo]:

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
staccatissimo (scat [c..g]^/8)
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
    p1 = scat [c..c']^/4
    p2 = delay (1/4) $ scat [c..c']^/4
    p3 = delay (3/4) $ scat [c..c']^/4
in (accent . legato) (p1 </> p2 </> p3)
```

## Parts

@[Division]

@[Subpart]

@[Part]

@[Instrument]

@[Solo]


## Space

TODO

## Tremolo

@[tremolo]

```music+haskell
tremolo 2 $ times 2 $ (c |> d)^/2
```

TODO chord tremolo

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

TODO artificial harmonics

@[artificial]


## Text

TODO

@[text]

```music+haskell
text "pizz." $ c^/2
```

## Chords

Note with the same onset and offset are rendered as chords by default. If you want to prevent this you must put them in separate parts.

```music+haskell
scat [c,d,e,c] <> scat [e,f,g,e] <> scat [g,a,b,g]
```

Or, equivalently:

```music+haskell
pcat [c,e,g] |> pcat [d,f,a] |> pcat [e,g,b] |> pcat [c,e,g]
```

TODO how part separation works w.r.t. division etc

@[simultaneous]

@[simult]

## Rests

Similar to chords, there is usually no need to handle rests explicitly.

TODO add explicit rests etc

@[mcatMaybes] 

```music+haskell
mcatMaybes $ times 4 (accentAll g^*2 |> rest |> scat [d,d]^/2)^/8 
```
                 



# Transformations

## Time

@[rev]

```music+haskell
let
    melody = accent $ legato $ scat [d, scat [g,fs]^/2,bb^*2]^/4
in melody |> rev melody
```

@[times]

```music+haskell
let
    melody = legato $ scat [c,d,e,c]^/16
in times 4 $ melody
```

@[sustain]

```music+haskell
scat [e,d,f,e] <> c
```


<!--
@[repeated]

```music+haskellx
let 
    m = legato $ scat [c,d,scat [e,d]^/2, c]^/4 
in [c,eb,ab,g] `repeated` (\p -> up (asPitch p .-. c) m)
```
-->

## Onset and duration

```music+haskell
let                
    melody = asScore $ legato $ scat [scat [c,d,e,c], scat [e,f], g^*2]
    pedal  = asScore $ delayTime (melody^.onset) $ stretch (melody^.duration) $ c_
in compress 4 $ melody </> pedal
```

## Pitch

@[invertPitches]

```music+haskell
(scat [c..g]^*(2/5))
    </>
(invertPitches c $ scat [c..g]^*(2/5))
    </>
(invertPitches e $ scat [c..g]^*(2/5))
```


## Pitches and intervals

TODO

## Name and accidental

TODO

## Spelling

TODO

## Quality and number

TODO


## Intonation

TODO

## Inspecting dissonant intervals

TODO

## Semitones and enharmonic equivalence

TODO

## Spelling

TODO

## Scales

TODO

## Chords

TODO



## Parts

## Instrument, part and sub-part

## Extracting and modifying parts

## Part composition


# Time and structure

@[Transformable]

@[Splittable]

@[Reversible]

@[HasPosition]

@[HasDuration]


## Time and duration

@[Time]

@[Duration]

## Spans

@[Span]

## Notes

@[Note]

## Voice

A @[Voice] represents a single voice of music. It consists of a sequence of values with duration, but no time. 

```music+haskell
stretch (1/4) $ scat [c..a]^/2 |> b |> c'^*4
```

```music+haskell
stretch (1/2) $ scat [c..e]^/3 |> f |> g^*2
```


It can be converted into a score by stretching each element and composing in sequence.

<!--
```music+haskellx
let
    x = [ (1, c),
          (1, d),
          (1, f),
          (1, e) ]^.voice

    y = join $ [ (1, x), 
                 (0.5, up _P5 x), 
                 (4, up _P8 x) ]^.voice

in stretch (1/8) $ voiceToScore $ y
```
-->

## Tracks

A @[Track] is similar to a score, except that it events have no offset or duration. It is useful for representing point-wise occurrences such as samples, cues or percussion notes.

It can be converted into a score by delaying each element and composing in parallel. An explicit duration has to be provided.

<!--
```music+haskellx
let
    x = [ (0, c), (1, d), (2, e) ]^.track
    y = join $ [ (0, x), 
                (1.5,  up _P5 x), 
                (3.25, up _P8 x) ]^.track

in trackToScore (1/8) y
```
-->

## Scores

@[Score]




# Meta-information

It is often desirable to annotate music with extraneous information, such as title, creator or, key or time signature. Also, it is often useful to mark scores with structural information such as movement numbers, rehearsal marks or general annotations. In the Music Suite these are grouped together under the common label *meta-information*.

The notion of meta-data used in the Music Suite is more extensive than just static values: any @[Transformable] container can be wrapped, and the meta-data will be transformed when the annotated value is transformed. This is why meta-data is often variable values, such as @[Reactive] or @[Behavior].

All time structures in the Suite support an arbitrary number of meta-data fields, indexed by type. All meta-information is required to satisfy the `Typeable`, so that meta-data can be packed and unpacked dynamically), and `Monoid`, so that values can be created and composed without having to worry about meta-data. The `mempty` value is implicitly chosen if no meta-information of the given type has been entered: for example the default title is empty, the default time signature is `4/4`. If two values annotated with meta-data are composed, their associated meta-data maps are composed as well, using the `<>` operator on each of the types.

The distinction between ordinary musical data and meta-data is not always clear cut. As a rule of thumb, meta-events are any kind of event that does not directly affect how the represented music sounds when performed. However they might affect the appearance of the musical notation. For example, a *clef* is meta-information, while a *slur* is not. A notable exception to this rule is meta-events affecting tempo such as metronome marks and fermatas, which usually *do* affect the performance of the music.

## Title

Title, subtitle etc is grouped together as a single type `Title`, thus an arbitrary number of nested titles is supported. The simplest way to add a title is to use the functions @[title], @[subtitle], @[subsubtitle] and so son.

```music+haskell
title "Frere Jaques" $ scat [c,d,e,c]^/4
```

## Attribution

Similar to titles, the attribution of the creators of music can be annotated according to description such as @[composer], @[lyricist], @[arranger] etc. More generally, @[attribution] or @[attributions] can be used to embed arbitrary `(profession, name)` mappings.

```music+haskell
composer "Anonymous" $ scat [c,d,e,c]
```

```music+haskell
composer "Anonymous" $ lyricist "Anonymous" $ arranger "Hans" $ scat [c,d,e,c]^/4
```

## Key signatures

@[key]

@[keySignature]

@[keySignatureDuring]

@[withKeySignature]

## Time signatures          

@[time]

@[compoundTime]

@[timeSignature]

@[timeSignatureDuring]

@[withTimeSignature]

## Tempo

@[metronome]

@[tempo]

@[tempoDuring]

@[renderTempo]

## Fermatas, caesuras and breathing marks

TODO

## Ritardando and accellerando

TODO

## Rehearsal marks

TODO

@[rehearsalMark]

@[rehearsalMarkDuring]

@[withRehearsalMark]

## Barlines and repeats

There is generally no need to enter bars explicitly, as this information can be inferred from other meta-information. Generally, the following meta-events (in any part), will force a change of bar:

* Key signature changes
* Time signature changes
* Tempo changes
* Rehearsal marks

However, the user may also enter explicit bar lines using the following functions:

@[barline]

@[doubleBarline]

@[finalBarline]

Whenever a bar line is created as a result of a meta-event, an shorted time signature may need to be inserted as in:

```music+haskell
compress 4 $ timeSignature (4/4) (scat [c,d,e,c,d,e,f,d,g,d]) |> timeSignature (3/4) (scat [a,g,f,g,f,e])
```

TODO adapt getBarDurations and getBarTimeSignatures to actually do this

TODO repeats

## Clefs

To set the clef for a whole passage, use @[clef]. The clef is used by most notation backends and ignored by audio backends.

To set the clef for a preexisting passage in an existing score, use @[clefDuring].

TODO example

## Annotations

Annotations are simply textual values attached to a specific section of the score. In contrast to other types of meta-information annotations always apply to the whole score, not to a single part. To annotate a score use @[annotate], to annotate a specific span, use @[annotateSpan].

Annotations are invisible by default. To show annotations in the generated output, use
@[showAnnotations].

```music+haskell
showAnnotations $ annotate "First note" c |> d |> annotate "Last note" d
```

## Custom meta-information

Meta-information is not restricted to the types described above. In fact, the user can add meta-information of any type that satisfies the @[AttributeClass] constraint, including user-defined types. Meta-information is required to implement `Monoid`. The `mempty` value is used as a default value for the type, while the `mappend` function is used to combine the default value and all values added by the user.

Typically, you want to use a monoid similar to `Maybe`, `First` or `Last`, but not one derived from the list type. The reason for this is that meta-scores compose, so that `getMeta (x <> y) = getMeta x <> getMeta y`.

<!--
TODO unexpected results with filter and recompose, solve by using a good Monoid
Acceptable Monoids are Maybe and Set/Map, but not lists (ordered sets/unique lists OK)
See issue 103
-->

@[HasMeta]

@[setMetaAttr]

@[setMetaTAttr]



# Import and export

The standard distribution (installed as part of `music-suite`) of the Music Suite includes a variety of input and output formats. There are also some experimental formats, which are distributed in separate packages, these are marked as experimental below.

The conventions for input or output formats is similar to the convention for properties (TODO ref above): for any type `a` and format `T a`, input formats are defined by an *is* constraint, and output format by a *has* constraint. For example, types that can be exported to Lilypond are defined by the constraint `HasLilypond a`, while types that can be imported from MIDI are defined by the constraint `IsMidi a`.

## MIDI

All standard representations support MIDI input and output. The MIDI representation uses [HCodecs](http://hackage.haskell.org/package/HCodecs) and the real-time support uses [hamid](http://hackage.haskell.org/package/hamid). 

<!--
You can read and write MIDI files using the functions @[readMidi] and @[writeMidi]. To play MIDI back in real-time, use @[playMidi] or @[playMidiIO], which uses [reenact](http://hackage.haskell.org/package/reenact).
-->

Beware that MIDI input may contain time and pitch values that yield a non-readable notation, you need an sophisticated piece of analysis software to convert raw MIDI input to quantized input.

## Lilypond

All standard representations support Lilypond output. The [lilypond](http://hackage.haskell.org/package/lilypond) package is used for parsing and pretty printing of Lilypond syntax. Lilypond is the recommended way of rendering music notation.

Lilypond input is not available yet but a subset of the Lilypond language will hopefully be added soon.

An example:

```haskell
toLilypondString $ asScore $ scat [c,d,e]
```

    <<
        \new Staff { <c'>1 <d'>1 <e'>1 }
    >>


## MusicXML

All standard representations support MusicXML output. The [musicxml2](http://hackage.haskell.org/package/musicxml2) package is used for 
parsing and pretty printing. 

The output is fairly complete, with some limitations ([reports][issue-tracker] welcome). There are no plans to support input in the near future.

Beware of the extreme verboseness of XML, for example:

```haskell
toMusicXmlString $ asScore $ scat [c,d,e]
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

ABC notation (for use with [abcjs](https://github.com/paulrosen/abcjs) or similar engines) is still experimental.

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

TODO


# Acknowledgements

The Music Suite is indebted to many other previous libraries and computer music environments, particularly [Common Music][common-music], [PWGL][pwgl], [Max/MSP][max-msp], [SuperCollider][supercollider], [nyquist][nyquist], [music21][music21], [Guido][guido], [Lilypond][lilypond] and [Abjad][abjad]. Some of the ideas for the quantization algorithms came from [Fomus][fomus].

It obviously ows a lot to the Haskell libraries that it follows including [Haskore][haskore], [Euterpea][euterpea] and [temporal-media][temporal-media]. The idea of defining a custom internal representation, but relying on standardized formats for input and output comes from [Pandoc][pandoc]. The idea of splitting the library into a set of packages (and the name) comes from the [Haskell Suite][haskell-suite]. The temporal structures, their instances and the concept of denotational design comes from [Reactive][reactive] (and its predecessors). [Diagrams][diagrams] provided the daring example and some general influences on the design.


```music-extra
(ignored)
```

@@@hslinks@@@

[Lilypond]:         http://lilypond.org
[Timidity]:         http://timidity.sourceforge.net/
[HaskellPlatform]:  http://www.haskell.org/platform/

[issue-tracker]:    https://github.com/hanshoglund/music-score/issues

[pwgl]:             http://www2.siba.fi/PWGL/
[pandoc]:           http://johnmacfarlane.net/pandoc/
[haskell-suite]:    https://github.com/haskell-suite
[music-util-docs]:  https://github.com/hanshoglund/music-util/blob/master/README.md#music-util

[common-music]:     http://commonmusic.sourceforge.net/
[temporal-media]:   http://hackage.haskell.org/package/temporal-media
[abjad]:            https://pypi.python.org/pypi/Abjad/2.3
[max-msp]:          http://cycling74.com/products/max/
[nyquist]:          http://audacity.sourceforge.net/help/nyquist
[reactive]:         http://www.haskell.org/haskellwiki/Reactive
[diagrams]:         http://projects.haskell.org/diagrams/
[supercollider]:    http://supercollider.sourceforge.net/
[music21]:          http://music21-mit.blogspot.se/
[guido]:            http://guidolib.sourceforge.net/
[lilypond]:         http://lilypond.org/
[fomus]:            http://fomus.sourceforge.net/
[haskore]:          http://www.haskell.org/haskellwiki/Haskore
[euterpea]:         http://haskell.cs.yale.edu/euterpea/
[haskell]:          http://haskell.org
[pandoc]:           http://johnmacfarlane.net/pandoc/

