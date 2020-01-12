# First steps

## Installing

TODO

## Writing music

Music Suite is an [embedded language](https://en.wikipedia.org/wiki/Domain-specific_language#External_and_Embedded_Domain_Specific_Languages), based on Haskell. A piece of music is described by a *expressions*. Much as arithmetic expressions describes numbers or other mathematical objects, music expressions describe music.

Here is a very simple expression:

```haskell+haskell
c <> e
```

This consist of the notes `c` and `e`, played simultaneously. The `<>` symbol is an operator that means "compose this music in parallel".

TODO


### Using files

```haskell
import Music.Prelude
main = defaultMain music

music =
  c |> d |> e
```

TODO the first lines here are boilerplate. The last line (`c |> d |> e`) containsthe actual music expression.

The purpose of the `import` line is to allow you to use Music Suite, as Haskell only imports its own standard libary by default. The `main` line turns whatever `music` is defined to be into a CLI program which we can execute as follows:

Then either execute it using:

    $ cabal runhaskell test.hs

TODO you can copy-paste all examples from this file into the above template. For example, this is how our original expression would look:

```haskell
import Music.Prelude
main = defaultMain music

music =
  c <> e
```


### Using an interactive environment

TODO shell, notebook or similar interactive backend. See TODO.md.


# Basic concepts

TODO explain link to Haskell concepts such as values, functions, type errors, lenses, etc?

<!--

## Lens, Prism and Iso

Music Suite use lenses as a delibrate design choice. Can be fantastically complicated. XXX just the basics:

- *Lenses* provide a way to view part a product type. Canonical example: `_1` provides a view to the first element of a `(,)`.
- *Prisms* provide a way to view part a sum type. Canonical example: `_Left` provides a view to the first element of an `Either`.
- *Isos* provide an alternative view of the whole thing that does not lose any information. I.e. curried provides a way to transorm `(a,b) -> c` into `a -> b -> c` and back.
- XXX traversal

XXX `lens` provides lots of combinators for working with these. Most importantly:

- `(.)` composes any of these objects
- `(^.)` and `view` extracts an element of a lens or iso, and of a prism if possible
- `(^?)` and `preview` extracts the first element of any optic if there is one
- `(^..)` or `toListOf` extracts all element of any optic
- `over` and `set` updates elements
- `from` turns isos around

## Semigroups, monoids, groups and vector spaces

- A *semigroup* has an associative combination operator. Examples: non-empty lists, integers with (+).
  - A *monoid* is a semigroup with an identity element. Examples: lists, integers with (+) and 0.
    - A *group*  is a monoid with a negation element. Examples: integers with (+), 0 and `negate`.
      - A *vector space* is a group whose elements can be multiplied by a  *scalar*. Example: 2D vectors with real numbers as scalars.

Most standard musical aspects are vector spaces:

- Durations
- Pitches
- Dynamics

## Separating points and vectors

Music Suite takes inspiration from diagrams in *separating points and vectors*. XXX just briefly hint why this is important.
-->


## Time and Duration

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
c|*(9/8) |> d|*(7/8)
```

```music+haskell
stretch (2/3) (pseq [c,d,e]) |> f|*2
```

As you can see, note values, tuplets and ties are added automatically.

TODO this should use nested tuplets:

```music+haskell
pseq [pseq [c,d,e] |* (2/(3)), c, d, e, f] |* (1/(5*4))
```

```music+haskell
pseq [pseq [c,d,e,f,g] |* (4/5), c, d] |* (2/(3*4))
```



The `|*` and `|/` operators can be used as shorthands for `delay` and `compress`.

```music+haskell
(c |> d |> e |> c |> d|*2 |> d|*2)|/16
```

Here is a more complex example using function composition. The dot operator `.` is used to compose the function `up _P8` (transpose the music up one octave), `compress 2` ("compress" or "diminish" the music by a factor of two) and `delay 3` (delay the music by the duration of three whole notes). The composition applies the in left to right order.

```music+haskell
(up _P8 . compress 2 . delay 3) c
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
    scale = pseq [c,d,e,f,g,a,g,f]|/8
    triad a = a <> up _M3 a <> up _P5 a
in up _P8 scale </> (triad c)|/2 |> (triad g_)|/2
```

As a shorthand for `x |> y |> z ..`, we can write @[pseq] `[x, y, z]` (short for *sequential concatenation*).

```music+haskell
pseq [c,e..g]|/4
```

For `x <> y <> z ..`, we can write @[ppar] `[x, y, z]` (short for *parallel concatenation*).

```music+haskell
ppar [c,e..g]|/2
```

## Pitch

### Pitch names

To facilitate the use of non-standard pitch, the standard pitch names are provided as overloaded values, referred to as *pitch literals*.

To understand how this works, think about the type of numeric literal. The values $0, 1, 2$ etc. have type `Num a => a`, similarly, the pitch literals $c, d, e, f ...$ have type @[IsPitch] `a => a`.

For Western-style pitch types, the standard pitch names can be used:

```music+haskell
pseq [c, d, e, f, g, a, b]
```

<!--
Pitch names in other languages work as well, for example `ut, do, re, mi, fa, so, la, ti, si`.

German names (using `h` and `b` instead of `b` and `bb`) can be approximated as follows:

```haskell
import Music.Preludes hiding (b)
import qualified Music.Pitch.Literal as P

h = P.b
b = P.bb
```
-->


You can change octave using @[octavesUp] and @[octavesDown]:

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

Sharps and flats can be added using @[sharpen] and @[flatten].

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
sharpen c             = cs
flatten d             = db
(sharpen . sharpen) c = css
(flatten . flatten) d = dss
```

Note that `cs == db` may or may not hold depending on the pitch representation to understand this, read about *overloading* in the next section.


### Overloading

TODO overloading, explain why the following works:

```haskell
return (c::Note) == (c::Score Note)
```

TODO refer back to previous table. For example `Music.Pitch.Common` distinguishes between enharmonics, while `Integer` does not.

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
in pseq $ fmap (`up` c) intervals
```

You can add pitches and intervals using the @[.-.] and @[.+^] operators. To memorize these
operators, think of pitches and points `.` and intervals as vectors `^`.

### Spelling and normalization

TODO auto-spelling/normalization

```music+haskell
pseq $ fmap (`alter` c) [-2..2]
```

TODO this breaks Lilypond output (and XML?). Add auto-normalization in export
```TODOmusic+haskell
pseq $ fmap (`alter` c) [-5..5]
```

### Qualified pitch and interval names

There is nothing special about the pitch and interval literals, they are simply values exported by the `Music.Pitch.Literal` module. While this module is reexported by the standard music preludes, you can also import it qualified. You can use this in combination with `hiding`.


```haskell
Pitch.c |> Pitch.d .+^ Interval.m3
```


## Dynamics

Dynamic values are overloaded in the same way as pitches. The dynamic literals are defined in `Music.Dynamics.Literal` and have type `IsDynamics a => a`.

@[level]

An overview of the dynamic values:

```music+haskell
over eras (stretchRelativeOnset 0.5) $ pseq $ zipWith level [fff,ff,_f,mf,mp,_p,pp,ppp] [c..]
```

TODO other ways of applying level

TODO should the phrase traversal version be the default? E.g. do we want `cresc a b = over phrases' (crescV a b)`.

```music+haskell
(over phrases' (cresc pp mf) $ pseq [c..c'] |/8)
  </>
(over phrases' (dim fff ff) $ pseq [c..c'] |/8)
```

You can give any two dynamic values to `cresc` and `dim` (e.g. they are synonyms). A crescendo/diminuendo line will be drawn as necessary.

```TODOmusic+haskell
(cresc pp mf $ pseq [c..c'] |/8)
  </>
(cresc ff pp $ pseq [c..c'] |/8)
  </>
(cresc mp mp $ pseq [c..c'] |/8)
```

TODO for long crescendo/diminuendos, render as text by default.

```music+haskell
(over phrases' (cresc pp mf) $ (times 8 $ pseq [c..g]) |/8)
```

### Understanding how dynamics are represented

TODO value at each note. No need to explicitly draw indications or cresc./dim. lines.
This means you can freely split/merge without having to worry about dynamics.

In general, a new dynamic mark is drawn at the start of each entry, that is after
each period of rests per voice. However if the dynamic has not changed the mark is only
repeated if the last entry was a few bars ago.

```music+haskell
-- Hidden
[(0<->1, c)^.event, (1.5<->3, d)^.event]^.score
  </>
-- Different
[(0<->1, c)^.event, set dynamics' ff (1.5<->3, d)^.event]^.score
  </>
-- Distant
[(0<->1, c)^.event, (3<->4, d)^.event]^.score
```

## Articulation

Some basic articulation functions are @[legato], @[staccato], @[portato], @[tenuto], @[staccatissimo]:

```music+haskell
legato (pseq [c..g]|/8)
    </>
staccato (pseq [c..g]|/8)
    </>
portato (pseq [c..g]|/8)
    </>
tenuto (pseq [c..g]|/8)
    </>
staccatissimo (pseq [c..g]|/8)
```

@[accent]
@[marcato]

```music+haskell
accent (pseq [c..g]|/8)
    </>
marcato (pseq [c..g]|/8)
```

By default, accents are only applied to the first note in each phrase. You can also explicitly specify the last note, or all the notes:

@[accentLast]
@[accentAll]

```music+haskell
accentLast (pseq [c..g]|/8)
    </>
accentAll (pseq [c..g]|/8)
```

You can apply slurs and articulation marks to scores of arbitrary complexity. The library will traverse each phrase in the score and apply the articulations separately.

For example in this example we're building up a score consisting of three parts and then apply `accent . legato`:

```music+haskell
let
    p1 = pseq [c..c']|/4
    p2 = delay (1/4) $ pseq [c..c']|/4
    p3 = delay (3/4) $ pseq [c..c']|/4
in (accent . legato) (p1 </> p2 </> p3)
```

These kind of traversals are not limited to articulation. See [Phrase traversals](#phrase-traversals) for a more general overview.

## Overloading of dynamics and artiulation

We've already seen how the expression `c` is overloaded to mean many things: the pitch C4, an event containing the pitch `c` with the default onset and offset, a score containing a single event, and so on. (TODO make sure we have)

Dynamics and articulations are overloaded too. The default articulation means "no particular separation or accentuation", and renders as a note without any markings Similarly the default dynamic renders as *mf* (mezzo-forte).

TODO show how to override/add/multiply much as with pitch.

```music+haskell
c
```

```music+haskell
set dynamics' pp c
```

```music+haskell
set articulations' (accentuation +~ 2 $ mempty) c
```

```music+haskell
over (articulations' . accentuation) (+ 2) c
```

## Parts

@[Division]

@[Subpart]

@[Part]

@[Instrument]

@[Solo]

TODO working with staves

TODO updating and merging parts. Or should we write about this in cobination with pitch/dynamic etc (as they're all traversal-based).

TODO `divide 2 violins` should yield `V1, V2`, not `VI.0, VI.1`

```music+haskell
arrangeFor stringQuartet $ rcat [c',e,g_,c_]
  where
    stringQuartet = divide 2 violins ++ [violas, cellos] -- TODO define somewhere

```



## Tremolo

@[tremolo]

```music+haskell
tremolo 2 $ times 2 $ (c |> d)|/2
```

TODO chord tremolo

## Slides and glissando

@[slide]
@[glissando]

```music+haskell
glissando $ pseq [c,d]|/2
```

## Harmonics

Use the @[harmonic] function:

```music+haskell
(harmonic 1 $ c|/2)
    </>
(harmonic 2 $ c|/2)
    </>
(harmonic 3 $ c|/2)
```

TODO artificial harmonics

@[artificial]


## Text

TODO

@[text]

```music+haskell
text "pizz." $ c|/2
```

## Chords

Note with the same onset and offset are rendered as chords by default. If you want to prevent this you must put them in separate parts.

```music+haskell
pseq [c,d,e,c] <> pseq [e,f,g,e] <> pseq [g,a,b,g]
```

Or, equivalently:

```music+haskell
ppar [c,e,g] |> ppar [d,f,a] |> ppar [e,g,b] |> ppar [c,e,g]
```

TODO how part separation works w.r.t. division etc

@[simultaneous]


## Rests

Similar to chords, there is usually no need to handle rests explicitly.

TODO show with examples how rests are added from delay/transform etc.

It is possible to add rests explicitly as follows.

@[mcatMaybes]

```TODO
times 4 (accentAll g|*2 |> rest |> pseq [d,d]|/2)|/8
```

You can also remove rests explicitly:

TODO explain how this works.

```music+haskell
mcatMaybes $ times 4 (accentAll g|*2 |> rest |> pseq [d,d]|/2)|/8
```




# Transforming music

## Time transformations

@[rev]

```music+haskell
let
    melody = accent $ legato $ pseq [d, pseq [g,fs]|/2,bb|*2]|/4
in melody |> rev melody
```

@[times]

```music+haskell
let
    melody = legato $ pseq [c,d,e,c]|/16
in times 4 $ melody
```


## Position and duration

TODO instead of using Transformable, show how to set duration explicitly

```music+haskell
let
    melody = legato $ pseq [pseq [c,d,e,c], pseq [e,f], g|*2]
    pedal = set era (melody^.era) g_
in compress 4 $ melody </> pedal
```

TODO example with stretchRelative

TODO HasPosition for scores (see TODO.d)

TODO Aligned, "floaters"

## Pitch

@[invertPitches]

```music+haskell
(pseq [c..g]|*(2/5))
    </>
(invertPitches c $ pseq [c..g]|*(2/5))
    </>
(invertPitches e $ pseq [c..g]|*(2/5))
```

TODO Transformable class.

## Pitches and intervals

TODO Transposable class. Similar to Transformable but for pitches.

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


## Parts

## Instrument, part and sub-part

## Extracting and modifying parts

## Part composition


# Musical aspects

## Pitch

@[HasPitch]

@[pitch]

@[pitch']


@[HasPitches]

@[pitches]

@[pitches']

@[up]

```music+haskell
let
  ch x = ppar [x, upDiatonic c 2 x, upDiatonic c 5 x]
in pseq $ ch <$> [c,d,e,f,g,a,g,c',b,a,g,fs,g |* 4] |/ 8
```

```music+haskell
let
  ch x = ppar [x, upDiatonic fs 2 x, upDiatonic fs 5 x]
in pseq $ ch <$> [c,d,e,f,g,a,g,c',b,a,g,fs,g |* 4] |/ 8
```

@[down]

@[above]

@[below]



@[number]
@[quality]
@[name]
@[accidental]
@[number]
@[invert]
@[simple]
@[octaves]
@[asPitch]
@[asAccidental]

TODO Ambitus, Chord and Scale

TODO spelling

TODO equal temperament and intonation

## Articulation
## Dynamics
## Parts


# Time and structure

@[Transformable]

@[Splittable]

@[Reversible]

@[HasPosition]

@[HasDuration]


## Time, duration and span

Time points and vectors are represented by two types @[Time] and @[Duration]. The difference between these types is similar to the distinction between points and vectors in ordinary geometry. One way of thinking about time vs. duration is that duration are always *relative* (i.e. the duration between the start of two notes), while *time* is absolute.

Time points form an affine space over durations, so we can use the operators @[.+^] and @[.-.] to convert between the two.

The @[Span] type represents a *slice* of time. We can represent spans in exactly three ways: as two points representing *onset* and *offset*, as one point representing *onset* and a duration, or alternatively as a point representing *offset* and a duration. To convert between these representations, we can use @[onsetAndOffset], @[onsetAndDuration] and @[durationAndOffset], which are *isomorphisms* using the definition from the `lens` package.

## Note, Chord and Event

@[Note]

## Voices

A @[Voice] represents a single voice of music. It consists of a sequence of values with duration.

```music+haskell
stretch (1/4) $ pseq [c..a]|/2 |> b |> c'|*4
```

```music+haskell
stretch (1/2) $ pseq [c..e]|/3 |> f |> g|*2
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

## Behavior and Reactive

## Aligned

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

All time structures in Music Suite support an arbitrary number of meta-data fields, indexed by type. All meta-information is required to satisfy the `Typeable`, so that meta-data can be packed and unpacked dynamically), and `Monoid`, so that values can be created and composed without having to worry about meta-data. The `mempty` value is implicitly chosen if no meta-information of the given type has been entered: for example the default title is empty, the default time signature is `4/4`. If two values annotated with meta-data are composed, their associated meta-data maps are composed as well, using the `<>` operator on each of the types.

The distinction between ordinary musical data and meta-data is not always clear-cut. As a rule of thumb, meta-events are any kind of event that does not directly affect how the represented music sounds when performed. However they might affect the appearance of the musical notation. For example, a *clef* is meta-information, while a *slur* is not. A notable exception to this rule is meta-events affecting tempo such as metronome marks and fermatas, which usually *do* affect the performance of the music.

TODO explain what meta-really is: dynamical (Typeable) vs static. All presentational/extra info is meta, all *logical/semantic/sounding* information is not. This is a trade off between static and dynamic typing.


## Title

Title, subtitle etc is grouped together as a single type `Title`, thus an arbitrary number of nested titles is supported. The simplest way to add a title is to use the functions @[title], @[subtitle], @[subsubtitle] and so son.

```music+haskell
title "Frere Jaques" $ pseq [c,d,e,c]|/4
```

## Attribution

Similar to titles, the attribution of the creators of music can be annotated according to description such as @[composer], @[lyricist], @[arranger] etc. More generally, @[attribution] or @[attributions] can be used to embed arbitrary `(profession, name)` mappings.

```music+haskell
composer "Anonymous" $ pseq [c,d,e,c]
```

```music+haskell
composer "Anonymous" $ lyricist "Anonymous" $ arranger "Hans" $ pseq [c,d,e,c]|/4
```

## Key signatures

```music+haskell
let major = True -- TODO!
in
keySignature (key db major) $ pseq [db,eb,f]
```

@[keySignatureDuring]

@[withKeySignature]

## Time signatures

```haskell
2/4 :: TimeSignature
```

```haskell
(3+2)/8 :: TimeSignature
```

Or equivalently:

```haskell
time 4 4 :: TimeSignature
```

```haskell
compoundTime [3,2] 8 :: TimeSignature
```

```music+haskell
timeSignature (3/8) $ pseq [db,eb,f]
```

@[timeSignatureDuring]

@[withTimeSignature]

### Converting from one time signature to another

```music+haskell
let
  ch = ppar [e,g,c']
  waltz = pseq [c,ch,ch,g_,ch,ch] |* (1/4)
in
timeSignature (3/4) waltz
```

```music+haskell
let
  ch = ppar [e,g,c']
  waltz = pseq [c,ch,ch,g_,ch,ch] |* (1/4)
in
timeSignature (3/8) $ compress 2 waltz
```

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
compress 4 $ timeSignature (4/4) (pseq [c,d,e,c,d,e,f,d,g,d]) |> timeSignature (3/4) (pseq [a,g,f,g,f,e])
```

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


# Querying and traversing scores

TODO

## Phrase traversals

TODO


# Scales, modes and harmony

TODO we've seen several examples of affine spaces with notions of *points* and *distances*: time points and durations, pitches and intervals, spans and transformations, and so on.

Another example is the notion of scales and chords. These are (conceptually) infinite collections of points, forming a subset of a larger pitch space. By forgetting the *root* or *fundamental* of a scale/chord we obtain what is known as a mode (for scales) or a chord type (for chords).

TODO rename (Function -> ChordType or similar). Function implies context/direction and is confusing for other reasons too.

TODO why does thirdMode not work?

```music+haskell
inspectableToMusic $
[ phrygian
, majorScale
-- , bluesMajor
, wholeTone
, octatonic
-- , thirdMode
]
```

```music+haskell
inspectableToMusic $
[ modeToScale c phrygian
, modeToScale d majorScale
-- , modeToScale e bluesMajor
, modeToScale f wholeTone
, modeToScale g octatonic
-- , modeToScale a thirdMode
]
```

## Chords


```music+haskell
inspectableToMusic $
[ majorTriad
, minorTriad
, augmentedChord
, diminishedChord
, halfDiminishedChord
]
```

```music+haskell
inspectableToMusic $
[ functionToChord g majorTriad
, functionToChord c minorTriad
, functionToChord f augmentedChord
, functionToChord eb diminishedChord
]
```

TODO all types above are also *voiced*, in other words:

  - Conceptually infinite/extensible
  - Focused down on a subset of the space (e.g. the pitches [c,e,g]): the voicing
  - We can "un-voice" the chord to forget the focused pitches
  - We can "re-voice" the chord. TODO formalize this.
    - Equivalence relations?
    - Note this is *not* the same as inversion. `[c,e,g]` and `[e,g,c']` are related by inversion, but `[c,e,g]` and `[c,g,e']` are related by revoicing.


# Patterns

# Random sources and non-determinism

# Constraints

# Counterpoint

# Orchestration

TODO defining ensembles (part lists)

# Tuning

# Sound and timbre

# Spacialization

# Interactive use

# Serialization




# Import and export

The standard distribution (installed as part of `music-suite`) of the Music Suite includes a variety of input and output formats. There are also some experimental formats, which are distributed in separate packages, these are marked as experimental below.

The conventions for input or output formats is similar to the convention for properties (TODO ref above): for any type `a` and format `T a`, input formats are defined by an *is* constraint, and output format by a *has* constraint. For example, types that can be exported to Lilypond are defined by the constraint `HasLilypond a`, while types that can be imported from MIDI are defined by the constraint `IsMidi a`.

## MIDI

TODO how to export

Beware that MIDI input may contain time and pitch values that yield a non-readable notation, you need an sophisticated piece of analysis software to convert raw MIDI input to quantized input.

## Lilypond

TODO how to export

TODO re-add toLilypondString?

```haskell
toLilypondString $ asScore $ pseq [c,d,e]
```

    <<
        \new Staff { <c'>1 <d'>1 <e'>1 }
    >>


## MusicXML

TODO export MusicXML 3.0

There are no plans to support input at the moment.

TODO re-add toMusicXmlString?

```haskell
toMusicXmlString $ asScore $ pseq [c,d,e]
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


## Sibelius

The [music-sibelius](http://hackage.haskell.org/package/music-sibelius) package provides experimental import of Sibelius scores (as MusicXML import is [not supported](#musicxml)).





# Customizing music representation

TODO explain 'Aspect' vs 'Meta'

## Adding an new representation


TODO

## Adding a new aspect

It is also possible to make Music Suite work with completely *new* aspects.

TODO

## Adding meta

TODO

## Adding a time structure

TODO

- Create a type of kind `* -> *`.
- Add instances for the standard classes @[Functor], @[Applicative] and (if possible) @[Monad] or @[Comonad].
- If your representation supports *parallel* composition it should be a trivial (non-lifted) @[Monoid]. It it also supports sequential composition, it should support @[Transformable] and @[HasPosition].
- Optionally, add instances for @[Splittable] and @[Reversible].


# Related work and Acknowledgements

The Music Suite is indebted to many other previous libraries and computer music environments, particularly [Common Music][common-music], [PWGL][pwgl], [nyquist][nyquist], [music21][music21], [Lilypond][lilypond] and [Abjad][abjad]. Some of the ideas for the quantization algorithms came from [Fomus][fomus].

The work of Paul Hudak and the the Yale Haskell group, including [Haskore][haskore], [Euterpea][euterpea] is a major influence. The  and [temporal-media][temporal-media] package is a similar take on these ideas. The popular [Tidal][tidal] language provide a way of expressing infinite time structures, similar to the ones defined in `music-score`.

The idea of defining a custom internal representation, but relying on standardized formats for input and output is influenced by [Pandoc][pandoc]. The idea of splitting the library into a set of packages (and the name) comes from the [Haskell Suite][haskell-suite].

The temporal structures, their instances and more general design philosophy comes from Conal Elliott's [Reactive][reactive] (and its predecessors). Brent Yorgey's [Diagrams][diagrams] provided the separation of points and vectors which was a main influence.


```music-extra
(ignored)
```

@@@hslinks@@@

[lilypond]:         http://lilypond.org
[timidity]:         http://timidity.sourceforge.net/
[haskell-platform]: http://www.haskell.org/platform/

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
[tidal]:            http://yaxu.org/tidal/

[declaration-style]: http://www.haskell.org/haskellwiki/Declaration_vs._expression_style

----

*Copyright Hans Jacob Höglund 2012–2015*

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />This documentation is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.
