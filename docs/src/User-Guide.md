# First steps

## Installing

TODO

## Writing music

Music Suite is an [embedded language](https://en.wikipedia.org/wiki/Domain-specific_language#External_and_Embedded_Domain_Specific_Languages), based on Haskell. A piece of music is described by a *expressions*. Much as arithmetic expressions describes numbers or other mathematical objects, music expressions describe music.

Here is a very simple expression:

```haskell+haskell
c <> e <> g
```

This consist of the notes C4, E4 and G4, played simultaneously. The `<>` symbol is an operator that means "compose this music in parallel".

Generally we want to convert our expressions representing music into some audio or graphics, such as standard music notation. There are a couple of ways of doing this.


### Using files

Using a text editor, creating a file called `Test.hs` containing the following:

```haskell
import Music.Prelude
main = defaultMain music

music =
  c <> d <> e
```

The first three lines here are standard boilerplate. The last line (`c <> d <> e`) contains the actual music expression.

The purpose of the `import` line is to allow you to use Music Suite, as Haskell only imports its own standard libary by default. The `main` line turns whatever `music` is defined to be into a command line program which we can execute as follows:

    $ cabal exec runhaskell -- Test.hs

You can copy-paste all examples from this file into the above template. Whatever value `music` is assigned to will be exported when you run the file.


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


A single note can be entered by its name, e.g. `c`, `d` and so on.

The expression `c` enters C4 (middle C) with a duration of a a whole note. Durations are measured in rational numbers: a duration of `1` is a whole note (or semibreve), a duration of `1/2` is a half note (minim), and so on.

```music+haskell
c
```

## Duration and onset

All notes we enter have duration `1` by default. To change this, we use @[stretch] and @[compress]


```music+haskell
stretch (1/2) c
```

```music+haskell
stretch 2 c
```

```music+haskell
stretch (4+1/2) c
```


We count positions from the first beat in the first bar, so in 4/4 time, `0` means the first beat, `1/4` (or `0.25`) means the second beat and so on.

All notes start at position `0` by default. We can use use @[delay] to move the onset of notes to the right.

```music+haskell
delay 1 c
```

Negative numbers work too:

```music+haskell
delay (-0.25) $ delay 1 $ c
```

<!--
Law: stretch/compress are related as follows:

```haskell
compress x = stretch (1/x)
```
-->


The `|*` and `|/` operators can be used as shorthands for `delay` and `compress`.

```music+haskell
(c |> d |> e |> c |> d|*2 |> d|*2)|/16
```

## Tuplets and ties

TODO these are added automatically

```music+haskell
c|*(9/8) |> d|*(7/8)
```

## A few examples

Here is a more complex example using function composition. The dot operator `.` is used to compose the function `up _P8` (transpose the music up one octave), `compress 2` ("compress" or "diminish" the music by a factor of two) and `delay 3` (delay the music by the duration of three whole notes). The composition applies the in left to right order.

```music+haskell
(up _P8 . compress 2 . delay 3) c
```


## Composition operators

TODO composition operators: central.

Music expressions can be composed @[<>]:

```music+haskell
c <> e <> g
```


Or in sequence using @[|>]:

```music+haskell
c |> d |> e
```

Or partwise using @[</>]:

```music+haskell
c </> e </> g
```

### Shorthands

As a shorthand for `x |> y |> z ..`, we can write @[pseq] `[x, y, z, ...]`.

```music+haskell
pseq [c,e..g] |* (1/4)
```

For `x <> y <> z ...`, we can write @[ppar] `[x, y, z, ...]` .

```music+haskell
ppar [c,e..g] |/ 2
```

For `x </> y </> ...` the syntax is @[rcat] `[x, y, z ...]`.

```music+haskell
rcat [c,e..g] |/ 2
```

Here is a more complex example using all forms of composition:

```music+haskell
let
    scale = pseq [c,d,e,f,g,a,g,f]|/8
    triad a = a <> up _M3 a <> up _P5 a
in up _P8 scale </> (triad c)|/2 |> (triad g_)|/2
```

TODO understanding that `|>` and `</>` are based on `<>`.


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

TODO round off basic intro.

## Working with Time and Duration

TODO brief intro to adding time and duration? Introduce Time/Duration/Span/HasPosition properly later on?


# Pitch

## Pitches and intervals

## Pitch names

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


### Where the literals are defined

There is nothing special about the pitch and interval literals, they are simply values exported by the `Music.Pitch.Literal` module. While this module is reexported by the standard music preludes, you can also import it qualified. You can use this in combination with `hiding`.


```haskell
Pitch.c |> Pitch.d .+^ Interval.m3
```

TODO NOTE: In this chapter, do go into details about HasPitches (see Traversals), just show example uses

@[HasPitch]

@[pitch]

@[pitch']

@[HasPitches]

@[pitches]

@[pitches']


## Transposing music

@[Transposable]

NOTE Transposable is a synonym for the type expression `(HasPitches' a, AffinePair (Interval a) (Pitch a), PitchPair (Interval a) (Pitch a))`. We will explain what this means later.

### Basic transposition

@[up]
@[down]

```music+haskell
up m3 tune
  where
    tune = pseq [c,c,g,g,a,a,g|*2] |/8
```

```music+haskell
down _A4 tune
  where
    tune = pseq [c,c,g,g,a,a,g|*2] |/8
```

Note that transposing will not automatically change the key signature. See [key signatures](#key-signatures) for how to do this explicitly.

### Parallel motion

@[above]
@[below]

```music+haskell
above m3 tune |> below m3 tune
  where
    tune = pseq [c,c,g,g,a,a,g|*2] |/8
```

### Diatonic transposition


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

### Inverting pitch

@[invertPitches]

```music+haskell
(pseq [c..g]|*(2/5))
    </>
(invertPitches c $ pseq [c..g]|*(2/5))
    </>
(invertPitches e $ pseq [c..g]|*(2/5))
```

### Qualities, numbers, names, accidentals



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

Double sharps/flats are supported:

```music+haskell
pseq $ fmap (`alter` c) [-2..2]
```

In fact we generalize the notion of double sharps/flats to an arbitrary number of alterations. As there are no symbols for these in standard notation, they are automatically re-spelled in exported music.

```music+haskell
pseq $ fmap (`alter` c) [-4..4]
```

TODO spell, usingSharps, usingFlats, simplifyPitches

## Adding pitches and intervals

TODO AdditiveGroup, VectorSpace, AffineSpace for pitch/interval.

Show how this the first example of an affine space (more to come!)



## Alternative pitch representations

THe standard pitch representation implements the pitch of common (or Western) music notation, with built-in support for the diatonic/chromatic transposition, enharmonics and spelling.

### Absolute pitch

@[Hertz]

Logarithmic scales:

@[Fifths]
@[Cents]

### Equal tempered scales

TODO equal tempered scales of any size


### Consonance and dissonance

See the [Harmony][x] chapter.

























# Dynamics and Articulation

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

## Working with articulations

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


















# Instruments and Parts

TODO Parts vs Instruments

@[Part]
@[Instrument]

By convention a name in singlular refers to the instrument, and plural to the part.

```haskell
flute :: Instrument
flutes :: Part
```

## Basic use

TODO the default part is `Piano I`.

```music+haskell
ppar [c,d,fs]
```

### Setting instrument and subpart

```music+haskell
(parts' . instrument) .~ trumpet $ ppar [c,d,fs]
```

```music+haskell
(parts' . subpart) .~ 2 $ (parts' . instrument) .~ trumpet $ ppar [c,d,fs]
```

### Multi-staff parts

TODO multi-staff part support (see TODO.md)


### The part composition operator

TODO rcat, then replacing instrument

### Updating several parts at once

TODO updating and merging parts. Or should we write about this in cobination with pitch/dynamic etc (as they're all traversal-based).

TODO `divide 2 violins` should yield `V1, V2`, not `VI.0, VI.1`

```music+haskell
arrangeFor stringOrchestra $ rcat [c',e,g_,c_]
  where
    stringOrchestra = divide 2 violins ++ [violas, cellos] -- TODO define somewhere

```

### Solo parts

The solo/tutti component is useful when working with concertante scores.

@[Solo]

```music+haskell
(parts' .~ solo violin $ pseq [c,d,e,f,g,a,g,e,ds,e,cs,d,b,bb,a,ab] |/ 16)
  <>
arrangeFor stringOrchestra (pseq [rcat [c',e,g_,c_]])
  where
    stringOrchestra = divide 2 violins ++ [violas, cellos] -- TODO define somewhere
```

### TODO further subdivision

TODO "Violin I.1", Soprano IIa etc



## Extracting and modifying parts

TODO extractPart extractPartNamed extractParts extractPartsWithInfo

```music+haskell
extractPart violas fullScore
  where
    fullScore :: Music
    fullScore =
      (parts' .~ solo violin $ pseq [c,d,e,f,g,a,g,e,ds,e,cs,d,b,bb,a,ab] |/ 16)
        <>
      arrangeFor stringOrchestra (pseq [rcat [c',e,g_,c_]])
        where
          stringOrchestra = divide 2 violins ++ [violas, cellos] -- TODO define somewhere
```

## Playing techniques

### Tremolo, trills and rolls

TODO measured vs unmeasured

@[tremolo]

```TODOmusic+haskell
tremolo 2 $ times 2 $ (c |> d)|/2
```

@[fastTremolo]

```TODOmusic+haskell
fastTremolo $ times 2 $ (c |> d)|/2
```

TODO realising tremolo/trills

### Slides and glissando

@[slide]

TODO

@[glissando]

```music+haskell
glissando $ pseq [c,d]|/2
```

### Harmonics

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


### String techniques

TODO pizz/arco

```TODOmusic+haskell
pizz $ pseq [c,c,c,c, arco d |* 2] |/ 4
```

TODO chord tremolo

TODO stopping?

### Wind techniques

TODO fingering, multiphonics

TODO key sounds, percussive attacks ("pizz"), haromonics/whistle tones

### Brass techniques

TODO stopping

TODO mutes

## Percussion

TODO working with instruments for percussion (much like normal instruments, though pitch may be ignored)

Note: for percussion we break the singular/plural naming convention and export a `Part` in the singular form. Note that the solo/tutti component is set to `Tutti` by default even though there might only be one performer in the group (the distinction would still make sense e.g. in a percussion concerto).

```music+haskell
parts' .~ snareDrum $ (`stretch` c) <$> rh [1,rh [1,1,1],1,1]
  where
    rh = stretchTo 1 . pseq -- TODO put this in the library?
```

For rolls see [the previous section](tremolo-trills-and-rolls).

TODO render e.g. snare drum parts correctly

TODO render "drum kit" staff

# Lyrics and Vocals

TODO adding lyrics (including syllables/word boundaries/melismas)

TODO soloists/character name


# Non-note Actions

While most music notation is conerned with making sound, a score may call for events which are not meant to directly produce sound. We represent these things using special events called *actions*. Like with percussion actions have no pitch.

TODO representation? Some kind of sum type in the note stack?

## Piano/Vibraphone pedalling

TODO

## Instrument change warnings

TODO

## Cues

TODO

# Text and Color

TODO warning: free form text is semantically questionable, but we provide it for cases where no other suitable representation exists.

TODO e.g. expressive marks ("dolce")

@[text]

```music+haskell
text "pizz." $ c|/2
```

TODO color
































# Meta-information

TODO define this. Meta-information is presentational and not attached to a specific part. Shorten this intro. All presentational/extra info is meta, all *logical/semantic/sounding* information is not. We represent meta-information dynamically (Typeable).


It is often desirable to annotate music with extraneous information, such as title, creator or, key or time signature. Also, it is often useful to mark scores with structural information such as movement numbers, rehearsal marks or general annotations. In the Music Suite these are grouped together under the common label *meta-information*.

The notion of meta-data used in the Music Suite is more extensive than just static values: any @[Transformable] container can be wrapped, and the meta-data will be transformed when the annotated value is transformed. This is why meta-data is often variable values, such as @[Reactive] or @[Behavior].

All time structures in Music Suite support an arbitrary number of meta-data fields, indexed by type. All meta-information is required to satisfy the `Typeable`, so that meta-data can be packed and unpacked dynamically), and `Monoid`, so that values can be created and composed without having to worry about meta-data. The `mempty` value is implicitly chosen if no meta-information of the given type has been entered: for example the default title is empty, the default time signature is `4/4`. If two values annotated with meta-data are composed, their associated meta-data maps are composed as well, using the `<>` operator on each of the types.

The distinction between ordinary musical data and meta-data is not always clear-cut. As a rule of thumb, meta-events are any kind of event that does not directly affect how the represented music sounds when performed. However they might affect the appearance of the musical notation. For example, a *clef* is meta-information, while a *slur* is not. A notable exception to this rule is meta-events affecting tempo such as metronome marks and fermatas, which usually *do* affect the performance of the music.



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

```music+haskell
tempo adagio $ pseq [c,d,e,b,c] |/ (5*8) |> d |* (3/4)
```

```music+haskell
tempo (metronome (1/4) 80) $ pseq [c,d,e,b,c] |/ (5*8) |> d |* (3/4)
```

```music+haskell
(tempo adagio $ pseq [c,d,e,b,c] |/ (5*4) |> d |* (3/4))
  |>
(tempo allegro $ pseq [c..g] |/ 4 )
```

TODO rendering tempo
@[renderTempo]

### Fermatas, caesuras and breathing marks

Fermatas indicate a certain time point (usually a strong beat) should be prolonged.

TODO representation should be: meta-mark at the first strong beat *after* the fermata-signed notes (e.g. the one to break *before*). This means we can render fermata signs on all notes where whose span overlaps the break point (including offset, not including onset).


@[fermata]

```music+haskell
fermata StandardFermata (ppar [c,e,g])
```

TODO overlapping events:
```TODOmusic+haskell
fermata StandardFermata (ppar [pseq[c,d] |/ 2,e,g]) |/ 4
```

TODO add fermata at specific position

A fermata usually implies a unison cutoff of the prolonged notes, followed by a short break before continouing to the next beat. This can be made explicit by addng caesuras or breathing marks (commas). TODO

### Ritardando and accellerando

```TODOmusic+haskell
(rit (pseq [c,d] |> e |* 2) |/ 4)
```

```TODOmusic+haskell
(acc (pseq [c,d] |> e |* 2) |/ 4)
```

TODO apply at specific position

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

TODO override the default clef selection

## Multi-movement scores

TODO

## Rehearsal marks

@[rehearsalMark]

```music+haskell
rehearsalMark $ pseq [c,d,e,d,f,e,d,c] |/ 3
```

@[rehearsalMarkAt]

```music+haskell
rehearsalMarkAt 2 $ pseq [c,d,e,d,f,e,d,c] |/3
```


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



































# Time and structure

## Basic time types

Time points and vectors are represented by two types @[Time] and @[Duration]. The difference between these types is similar to the distinction between points and vectors in ordinary geometry. One way of thinking about time vs. duration is that duration are always *relative* (i.e. the duration between the start of two notes), while *time* is absolute.

Time points form an affine space over durations, so we can use the operators @[.+^] and @[.-.] to convert between the two.

The @[Span] type represents a *slice* of time. We can represent spans in exactly three ways: as two points representing *onset* and *offset*, as one point representing *onset* and a duration, or alternatively as a point representing *offset* and a duration. To convert between these representations, we can use @[onsetAndOffset], @[onsetAndDuration] and @[durationAndOffset], which are *isomorphisms* using the definition from the `lens` package.

TODO time/span/duration examples

### Spans as transformations

TODO explain

@[Transformable]

TODO Transformable laws


## Position and duration

@[HasDuration]

Inspecting the duration


@[HasPosition]

Inspecting the position

TODO instead of using Transformable, show how to set duration explicitly via lens

```music+haskell
let
    melody = legato $ pseq [pseq [c,d,e,c], pseq [e,f], g|*2]
    pedal = c `during` melody
in compress 4 $ melody </> pedal
```

TODO example with stretchRelative, stretchTo

TODO HasPosition/HasDuration laws


## Times with values

The Note and Event types are similar to Duration, Time and Span respectively, except they also contain a *payload* of an arbitrary type. This is expressed as a type parameter (often written using a lowercase letter, as in `Note a`).  In practice the payload will usually contain (possibly overloaded) *aspects* such as part, pitch, dynamics and so on.

@[Note]

```music+haskell
inspectableToMusic @(Note Pitch) $

  c
```


@[Event]

## Voices

A @[Voice] represents a sequence of values, each tagged with duration.

```music+haskell
inspectableToMusic @(Voice Pitch) $

stretch (1/4) $ [cs, bb, a |* 2]
```

```music+haskell
inspectableToMusic @(Voice (Maybe Pitch)) $

(1/4) *| [c |* 2, rest, e]
```

```music+haskell
inspectableToMusic @(Voice Pitch) $

stretch (1/4) $ do
  x <- [c, d, e] |/ 2
  y <- [c', b, bb] |/ 2
  [x, g, y |*4 ]
```


```music+haskell
inspectableToMusic @(Voice Pitch) $

stretch (1/4) $ do
  x <- [c, d, e] |/ 2
  [x, b, c' |*4 ]
```

```music+haskell
stretch (1/2) $ pseq [c..e]|/3 |> f |> g|*2
```

TODO monad comprehensions:

```music+haskell
inspectableToMusic @(Voice [Pitch]) $

[ [x,y] | x <- [c], y <- [d,e] ]
```

```music+haskell
inspectableToMusic @(Voice [Pitch]) $

[ [x,y] | x <- [c] | y <- [d,e] ]
```

```music+haskell
inspectableToMusic @(Voice [Pitch]) $

[ [x,y,z] | x <- [c] | y <- [d,e] | z <- [f,g] ]
```


TODO we should never see Music/StandardNote in the user guide (specific/nice-looking types instead). The only purpose of Music/StandardNote is to be defaults/final objects.

```music+haskell
inspectableToMusic @(Voice [StandardNote]) $

[ dynamics' .~ d $ p
  | p <- [c, ab, fs, g]
  | d <- [ppp, ff, mp, mf]
  ]
```

TODO names of isMelodicConsonance/isConsonance is confusing

```music+haskell
inspectableToMusic @(Voice [Pitch]) $

[ [x,y] | x <- view voice (fmap fromPitch $ enumChromaticFromTo c c''), y <- [d,e]
  , isMelodicConsonance (x .-. y) && isConsonance (x .-. y) ]
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


<!--

## Tracks

A @[Track] is similar to a score, except that it events have no offset or duration. It is useful for representing point-wise occurrences such as samples, cues or percussion notes.

It can be converted into a score by delaying each element and composing in parallel. An explicit duration has to be provided.

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

TODO empty scores and rests (see (HasPosition Score) in TODO.md)

TODO Behavior and Reactive, Sampling

TODO viewing a score as a Behavior (concatB). Useful for "vertical slice view" of harmony, as in https://web.mit.edu/music21/doc/usersGuide/usersGuide_09_chordify.html

TODO Aligned, "floaters"

TODO sequential composition of aligned voices "snap to next stressed beat".
Natural way of modelling pickups/upbeats etc. Can be combined with "beat hierarchy" model


Here is an example. Without upbeat:

```music+haskell
(pseq [g_,a_,b_]|/2 |> pseq [c, c, d, d]) |/ 4
```

TODO intuitive way of setting alignment...

```music+haskell
inspectableToMusic @[Aligned (Voice Pitch)] $

delay 1

[ aligned 0 0 c
, aligned 0 (1.5/view duration v) v |/ 4
]
  where
    v = ([g_,a_,b_]|/2 <> [c, c, d, d])
```


## Patterns

TODO a Pattern can be throught of as a generalization of a rhythm or beat. They are similar to scores, but are infinite. Each pattern is created by repeating a number of layers. Every pattern will repeat itself, though the repetition frequence may be very long.

TODO more idiomatic ways of buildings patterns


TODO use proper percussion here:

```music+haskell
renderPattern (a <> b) (0 <-> 4)
  where
    a = newPattern $ fmap (const c) $ [3,3,4,2,4]^.durationsAsVoice |/ 8
    b = newPattern $ fmap (const $ parts' .~ flutes $ c) $ (take 16 [1,1..])^.durationsAsVoice |/ 8
```

```music+haskell
renderPattern (a <> b) (0.5 <-> 1.5)
  where
    a = newPattern $ fmap (const c) $ [3,3,4,2,4]^.durationsAsVoice |/ 8
    b = newPattern $ fmap (const $ parts' .~ flutes $ c) $ (take 16 [1,1..])^.durationsAsVoice |/ 8
```

TODO Patterns are Transformable, Transposing, Attenuable and so on, so many expressions that work for scores and voices also work for patterns.

```music+haskell
renderPattern (stretch 0.5 $ up m3 p) (0 <-> 2)
  where
    p = a <> b
    a = newPattern $ fmap (const c) $ [3,3,4,2,4]^.durationsAsVoice |/ 8
    b = newPattern $ fmap (const $ parts' .~ flutes $ c) $ (take 16 [1,1..])^.durationsAsVoice |/ 8
```

TODO renderPatternsRel

TODO renderPatternsAbs


## Splitting and reversing

@[split]

@[rev] reverse, retrograde

```music+haskell
let
    melody = accent $ legato $ pseq [d, pseq [g,fs]|/2,bb|*2]|/4
in melody |> rev melody
```

```music+haskell
music |> rev music
  where
    music = (1/16) *| pseq [c|*3, legato $ pseq [accent eb, fs|*3, a, b|*3], gs, f|*3, d]
```



## Repetition and variation

@[times]

```music+haskell
let
    melody = legato $ pseq [c,d,e,c]|/16
in times 4 $ melody
```









# Traversals

TODO traverals are a powerful concept

Can be used to:

- Visit elements in a score
- Querying/folding
- Updating aspects

In the functional programming commonity, traverals have a powerful generalization known as optics, which also includes concepts such as lenses, prisms, folds and isomorphisms. Music Suite defines lenses and traversals compatible with `lens` and `microlens`.

TODO monomorphic and polymorphic traversals

Folds: `toListOf`, `anyOf`, `allOf`

Traversals: `over`, `traverseOf/forOf`, arbitrary effects (e.g. State, Writer, Maybe)

## Traversing the notes in a voice

```music+haskell
inspectableToMusic @(Voice [StandardNote]) $

over t (\x -> if x^.duration > 1 then up m2 x else x) [d,d,d |* 2,d]
  where
    t = notes . each
```

```TODOmusic+haskell
inspectableToMusic @(Voice [StandardNote]) $

traverseOf t _ [d,d,d |* 2,d]
  where
    t = notes . each
```


## Traversing all the events in a score

```music+haskell
canon </> renderAlignedVoice rh
  where
    rh :: IsPitch a => Aligned (Voice a)
    rh = fmap (fmap $ const c) $ aligned 0 0 $ view durationsAsVoice (tail $ toRelativeTime onsets)


    onsets :: [Time]
    onsets = Data.List.nub $ toListOf (events . each . onset) canon

    canon = rcat
      [ theme
      , theme |* (3/2)
      , theme |* 2
      ]
    theme = pseq [e,a|*2,c',b|*2,a,gs|*3,e'] |/ 8
```

## Traversing pitches

TODO

## Traversing dynamics and articulation

TODO

## Traversing parts

TODO

## Phrase traversals

TODO

## Filtered traversals

```music+haskell
inspectableToMusic @(Voice [StandardNote]) $

over t (up m2) [d,d,d |* 2,d]
  where
    t = notes . each . filtered (\x -> x^.duration < 2)
```




# Harmony

## Scales and modes

TODO we've seen several examples of affine spaces with notions of *points* and *distances*: time points and durations, pitches and intervals, spans and transformations, and so on.

Another example is the notion of scales and chords. These are (conceptually) infinite collections of points, forming a subset of a larger pitch space. By forgetting the *root* or *fundamental* of a scale/chord we obtain what is known as a mode (for scales) or a chord type (for chords).

TODO rename (Function -> ChordType or similar). Function implies context/direction and is confusing for other reasons too.

```music+haskell
inspectableToMusic @[Mode Pitch] $

[ phrygian
, majorScale
, bluesMajor
, wholeTone
, octatonic
, thirdMode
]
```

```music+haskell
inspectableToMusic @[Scale Pitch] $

[ modeToScale c phrygian
, modeToScale d majorScale
, modeToScale e bluesMajor
, modeToScale f wholeTone
, modeToScale g octatonic
, modeToScale a thirdMode
]
```

## Chords


```music+haskell
inspectableToMusic @[Function Pitch] $

[ majorTriad
, minorTriad
, augmentedChord
, diminishedChord
, halfDiminishedChord
]
```

```music+haskell
inspectableToMusic @[Chord Pitch] $

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


TODO looking up notes in a scale/chord (infinitely, Integer ->, 0 being the tonic)

TODO getting exactly the "voiced" notes

TODO type-level sepration of voiced/unvoiced (currently this is muddled). New type for voiced chords (this does not seem to appy to scales).
  "Free X" for building sets of pitches?

Calculate dissonance of a chord (classical/"objective", by higest common fundamental)

Non-repeating/self-repeating scales (e.g. the overtone series)

### Triadic harmony

Basic triads, seventh and ninth chords

### Quartal and quintal

TODO

### Scales as chords

TODO scale-chord texture, e.g. whole tone extending augmented,

### Non-diatonic scales

TODO the Common.Pitch type has built-in support for chromatic/diatonic harmony. We can construct types that support other system instead.





# Constraints


## Logic programming

TODO MonadLogic example?

Generate two melodies from a set of pitches and compose iff there are no dissonances.

TODO

Generate/distribute notes/phrases for two musicians and compose iff and nobody is asked to play more than one actions simultaneously.

## Constraint programming

TODO FD-constraint example? http://overtond.blogspot.com/2008/07/pre.html

TODO "native Haskell" example, with search tree pruning, a la Sodoku example in Bird's "Pearls of Functional Algorithm Design"

TODO others?


# Melody and counterpoint

TODO example melodic constraints (no tritones, including outlines, no adjacent jump). See e.g. Jeppesen.

TODO curves/functions for melodic shapes, mapped to a scale

TODO representing upbeats with (Aligned Voice), preferably with smart constructor

TODO detecting parallel, constant and contrary motion

TODO parallel fifths/octaves

TODO hidden parallels

TODO working with repetitions/cues etc

TODO canon example (search for fitting melody)



# Orchestration

TODO working with ensembles

TODO dynamics in ensembles

TODO combining ensembles/voicing/playing technique (even spectral combinations?)

TODO warning/rejecting out-of range notes

TODO combining short attack sounds with sustain

TODO dovetailing (e.g. for winds), with 1 note overlap

TODO wind slurs vs string legato


# Randomness

TODO Reader monad of seed/Random generator state


# Space

TODO very simple space representation (e.g. Angle), minimal example using Ambisonics?






# Import and Export

TODO basic structure/aproach to import and export

## Native format/Serialization

TODO

## Prelude/Inspectable

TODO point here is to fix a "default" type for rendering/export purposes. This is used to monomorphize expressions written in the polymorphic combinators of the library. The default type is knonw as `Music`.

Inspectable renders a type by converting it into an exportable type. See TODO.md re: defaulting.


## Overview of formats

### MIDI

TODO how to export

Beware that MIDI input may contain time and pitch values that yield a non-readable notation, you need an sophisticated piece of analysis software to convert raw MIDI input to quantized input.

### Lilypond

TODO how to export

TODO re-add toLilypondString?

```haskell
toLilypondString $ asScore $ pseq [c,d,e]
```

    <<
        \new Staff { <c'>1 <d'>1 <e'>1 }
    >>


### MusicXML

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


### ABC Notation

TODO ABC notation (for use with [abcjs](https://github.com/paulrosen/abcjs) or similar engines) is still experimental.

## Sibelius

TODO


# Tips and tricks

### Lists and streams

TODO


### Complex rhythms

Nested tuplets.

```music+haskell
stretch (2/3) (pseq [c,d,e]) |> f |*2
```


```music+haskell
pseq [pseq [c,d,e] |* (2/(3)), c, d, e, f] |* (1/(5*4))
```

```music+haskell
pseq [pseq [c,d,e,f,g] |* (4/5), c, d] |* (2/(3*4))
```


# Wall of Shame

TODO this is not documentation, move to some other location. Listing all "bad rendering" examplesas a visual issue tracker

### Quantization

```music+haskell
(`stretch` c) <$> rh [1,rh [1,1,1],1,1]
  where
    rh = stretchTo 1 . pseq
```

```music+haskell
rcat
      [ theme
      , theme |* (3/2)
      , theme |* 2
      ]
  where
    theme = pseq [e,a|*2,c',b|*2,a,gs|*3,e'] |/ 8
```

TODO this should use nested tuplets:

```music+haskell
pseq [pseq [c,d,e] |* (2/(3)), c, d, e, f] |* (1/(5*4))
```

```music+haskell
pseq [pseq [c,d,e,f,g] |* (4/5), c, d] |* (2/(3*4))
```

```music+haskell
stretch (1/2) $ pseq [c..e]|/3 |> f |> g|*2
```

### Alignment/pickups

This should render

```music+haskell
rcat $ fmap renderAlignedVoice $
[ aligned 0 0 c
, aligned 0 (1.5/view duration v) v |/ 4
]
  where
    v = ([g_,a_,b_]|/2 <> [c, c, d, d])
```
very much like this (except without the initial rests):
```music+haskell
rcat $ fmap renderAlignedVoice $ delay 1
[ aligned 0 0 c
, aligned 0 (1.5/view duration v) v |/ 4
]
  where
    v = ([g_,a_,b_]|/2 <> [c, c, d, d])
```
In other words, scores with events before time 0 should be treated as pickups and rendered in the same time signature as the first bar (starting at 0).


### Staves

### Misc


# Acknowledgements

## Contributors

TODO thanks

## Previous work

The Music Suite is indebted to many other previous libraries and computer music environments, particularly [Common Music][common-music], [PWGL][pwgl], [nyquist][nyquist], [music21][music21], [Lilypond][lilypond] and [Abjad][abjad]. Some of the ideas for the quantization algorithms came from [Fomus][fomus].

The work of Paul Hudak and the the Yale Haskell group, including [Haskore][haskore], [Euterpea][euterpea] is a major influence. The  and [temporal-media][temporal-media] package is a similar take on these ideas. The [TidalCycles][tidal] library provided the pattern structure.

The temporal structures, their instances and more general design philosophy comes from Conal Elliott's [Reactive][reactive] (and its predecessors). Brent Yorgey's [Diagrams][diagrams] provided the separation of points and vectors and was another main influence.


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
