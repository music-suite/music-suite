# Quick Start

## Installing

### Installing via Docker

TODO Docker or other "easy" install options

### Installing from source

We'll need Git and Nix (2.3.2 or later).

```
$ git clone https://github.com/hanshoglund/music-suite
$ cd music-suite
```

Then follow the instructions in `README.md` to setup the environment and build Music Suite.


## Writing music

Music Suite is based on *expressions*. An expression may represent any piece of music, from a single note to a complex, multi-movement work.

To use Music Suite, you will need to write expressions, which Music Suite will convert into *audio* or *graphics* (or both) on your behalf. We can ask Music Suite to do this in a couple of different ways. Choose one that works for you so that you can follow along with the examples in the tutorial.

<!--
Note: While Music Suite was written with Western classical notation in mind and it is not restricted to these formats.
-->


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

We can copy-paste all examples from this file into the above template. Whatever value `music` is assigned to will be exported when you run the file.


### Using an interactive environment

TODO standard notebook format support?

TODO "visual interpreter" support. See TODO.md.


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



##  Our first score

The first musical expression we write will be simple: it consists of a single letter:

```music+haskell
c
```

This of course represents the note C. Notice that there is more information in the score than what we entered. This is because most musical aspects have a *default value*. We will see how to override them later, but for now we can note:

- The default *octave* is the octave containing "middle C", or *C4* in [scientific pitch notation](https://en.wikipedia.org/wiki/Scientific_pitch_notation).

- The default *duration* is a whole note. Durations are measured in rational numbers: a duration of `1` is a whole note (or semibreve), a duration of `1/2` is a half note (or minim), and so on.

- The default *dynamic value* is *mf* (meaning *mezzo-forte*, "medium loud").

- The default *instrument* is *Piano*.


> Note: In Haskell, numbers are *overloaded*. The syntax do not convey any type information: `0.25` and `1/4` are equivalent. In Music Suite, all time values are implemented using arbitrary-precision integers and rational numbers, so you do not have to worry about rounding errors.

By default note have no *accidentals* or *articulation marks*. We will see how to add those later as well.


## Duration and onset

All notes we enter have duration `1` by default. To change this, we use @[stretch] and @[compress]:


```music+haskell
stretch (1/2) c
```

```music+haskell
stretch 2 c
```

```music+haskell
stretch (4+1/2) c
```

> Note: In classical theory *stretch* and *compress* are known as augmentation and diminishion, respectively.


We count positions from the first beat in the first bar, so in 4/4 time, `0` means the first beat, `1/4` (or `0.25`) means the second beat and so on.

All notes start at position `0` by default. We can use use @[delay] to move the onset of notes to the right.

```music+haskell
delay 1 c
```

Negative numbers work too:

```music+haskell
delay (-0.25) $ delay 1 $ c
```

The `|*` and `|/` operators can be used as shorthands for `stretch` and `compress`.

```music+haskell
(c |> d |> e |> c |> d|*2 |> d|*2) |/ 16
```

## Let and where

TODO let bindings first


## Octave changes

TODO

## Basic dynamics and articulations

TODO example and forward reference

## Composition operators

So far we have worked with a single note, which is not particularly interesting from a musical point of view. To combine multiple notes into a largers score we need a form of *composition*.

The basic composition operator is @[<>], which combines two pieces of music *simultaneously*. For example, we can combine the expressions `c`, `e` and `g`.

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

  TODO understanding tyes, types of the above operators and that `|>` and `</>` are based on `<>`. In other words `Semigroup` is used for all composition in Music Suite.


## Chords

  Notes with the same onset and offset are rendered as chords by default. If you want to prevent this you must put them in separate parts.

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

  We can also remove rests explicitly:

  TODO explain how this works.

  ```music+haskell
  mcatMaybes $ times 4 (accentAll g|*2 |> rest |> pseq [d,d]|/2)|/8
  ```


  There is no need to explicitly enter tuplets or ties, these are added automatically as needed.

  Any note that crosses a barline will be notated using ties:

  ```music+haskell
c |* (9/8) |> d |* (7/8)
  ```

  See also [time signatures](#time-signatures).

  Similarly, durations that do not fit into standard note durations are notated using dots or tuplets:

  ```music+haskell
  compress 4 (pseq [c |*3, d |* 3, e |* 2]) |> compress 5 (pseq [f,e,c,d,e]) |> d
  ```

## Functions and types

  TODO basic functions

  Here is a full example using function composition. The dot operator `.` is used to compose the function `up _P8` (which transpose thes the music up by one octave), `compress 2` and `delay 3`. The composed functions are applied in *left to right order*.

  ```music+haskell
  (up _P8 . compress 2 . delay 3) c
  ```

## More examples

  TODO make very clear that stretch, `_8va` etc work on arbitrarily complex scores, not just single notes as in the first examples

### Time and Duration

  TODO AffineSpace

  Maybe forward reference to Span/HasPosition?

## Summary

  We have now seen how to write basic pieces, using melody, harmony and voices. In the following chapters we will be looking at musical aspects such as pitch, dynamics and orchestration in more detail: these chapters can generally be read in any order.





# Pitch

## The Pitch type

  The @[Pitc] representation implements the pitch of common (or Western) music notation, with built-in support for the diatonic/chromatic transposition, enharmonics and spelling. As we shall see later this is not the only way of representing pitch in Music Suite, but it is common enough to be the default.

### Pitch names

  The following pitch names are used:

  ```music+haskell
  pseq [c, d, e, f, g, a, b]
  ```

### Octaves

  We can change octave using @[octavesUp] and @[octavesDown]:

  ```music+haskell
  octavesUp 4 c
  </>
  octavesUp (-1) c
  </>
  octavesDown 2 c
  ```

  There are synonyms for the most common cases:

  ```music+haskell
  _8va c <> c <> _8vb c
  ```

  The following is also a shorthand for alternative octaves:

  ```music+haskell
  c__ |> c_ |> c |> c' |> c''
  ```

### Sharps and flats

  Sharps and flats can be added using @[sharpen] and @[flatten].

  ```music+haskell
  sharpen c
  </>
  (sharpen . sharpen) c
  </>
  flatten c
  </>
  (flatten . flatten) c
  ```

  The @[alter] function is an iterated version of @[sharpen]/@[flatten]:

  ```music+haskell
  alter 1 $ pseq [c,d,e]
  ```

  Double sharps/flats are supported:

  ```music+haskell
  pseq $ fmap (`alter` c) [-2..2]
  ```

  The pitch representation used in Music Suite does in fact allow for an *arbitrary* number of sharps or flats. As there are no symbols for these in standard notation, they are automatically re-spelled in the output. Here is the note `c` written with up to 4 flats and sharps.

  ```music+haskell
  pseq $ fmap (`alter` c) [-4..4]
  ```

  There is of course also a shorthand for sharps and flats:

  ```music+haskell
  (cs |> ds |> es)    -- sharp
  </>
  (cb |> db |> eb)    -- flat
  ```

  > Note: Music Suite uses C major by default, so all altered pitches are rendered as accidentals. See [key signatures](TODO link) for how to change this.


## Pitch overloading

  To facilitate the use of non-standard pitch, the standard pitch names are provided as overloaded values, referred to as *pitch literals*.

  To understand how this works, think about the type of numeric literal. The values $0, 1, 2$ etc. have type `Num a => a`, similarly, the pitch literals $c, d, e, f ...$ have type @[IsPitch] `a => a`.


  TODO explain overloading is not limited to pitch types but also to containers types (by lifting), so the following works:


  ```haskell
return (c::Note) == (c::Score Note)
  ```

  > Hint: Use [`-XTypeApplications`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications) to restrict the type of an pitch. For example: `id @Pitch c`


## Intervals

  TODO the interval type represents common/Western classical *intervals*.

  As is common in music theory notation, *minor* and *diminished* intervals are written in lower-case, while *major*
  and *perfect* intervals are written in upper-case. Here are some examples:

```haskell
m3
_M3
_P5
d5
m9
d12
```

Similar to @[sharpen] and @[flatten], the @[augment] and @[diminish] functions can be used
to alter the size of an interval. For example:

```music+haskell
let
    intervals = [diminish _P5, (diminish . diminish) _P5]
in pseq $ fmap (`up` c) intervals
```




### Simple and compound intervals

@[invert]
@[simple]
@[octaves]

TODO simple vs compound

TODO positive vs negative: a negative interval is a compound interval with a negative octave number


### Number, quality, alteration, diatonic/chromatic

TODO intervals seen as:
- Pair of diatonic/chromatic steps (vector)
- Pair of diatonic steps + alteration
- Pair of number and quality ("major" and "third") - this is partial

@[number]
@[quality]
@[name]
@[accidental]

## Interval overloading

Interval names are overloaded in a manner similar to pitches, and are consequently referred to as *interval literals*. The corresponding class is called @[IsInterval].

> Hint: Use [`-XTypeApplications`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications) to restrict the type of an interval. For example: `id @Interval m3`



## Enharmonics

TODO @[HasSemitones]

### Pitch equality and ordering

The `Pitch` type has instances for the `Eq` and `Ord` type classes, representing equality and ordering respectively.

Equality of pitches takes spelling into account, so e.g. `cs /= db` holds. There are many ways of defining orderings on pitches: the default ordering compares diatonic steps first, alteration second.

```haskell
>>> sort [(cs :: Pitch), db, dbb]
[cs, dbb, db]
```

To get compare or sort pitches enharmonically you can use `sortOn`:

```haskell
>>> sortOn (semitones . (.-. c)) [(cs :: Pitch), db, flatten db]
[dbb, cs, db]
```

### Spelling

We can *respell* enharmonically equivalent pitches by using a *speller*.

TODO spell, usingSharps, usingFlats, simplifyPitches



## Converting between intervals and pitches

TODO AffineSpace

We can add pitches and intervals using the @[.-.] and @[.+^] operators. To memorize these
operators, think of pitches and points `.` and intervals as vectors `^`.


TODO AdditiveGroup, VectorSpace, AffineSpace for pitch/interval.

Show how this the first example of an affine space (more to come!)

TODO affine space, relative, vector-space-points

## Transposing and inverting music

TODO basic "geometry" (affine transformations) of pitch: scaling and translating

@[Transposable]

> Note: Transposable is a synonym for the type expression `(HasPitches' a, AffinePair (Interval a) (Pitch a), PitchPair (Interval a) (Pitch a))`. We will explain what this means later.

### Basic transposition

We can transpose a music expression using the @[up] and @[down] functions.

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

As you might expect `down` is the same as `up . negate` and vice versa.

```music+haskell
up (-m3) tune
  where
    tune = pseq [c,c,g,g,a,a,g|*2] |/8
```

> Note: transposing does *not* automatically change the key signature. See [key signatures](#key-signatures) for how to do this explicitly.

### Parallel motion

The @[above] and @[below] functions are similar to @[up] and @[down], but also retain the original music.

```music+haskell
above m3 tune |> below m3 tune
  where
    tune = pseq [c,c,g,g,a,a,g|*2] |/8
```

### Diatonic transposition

The @[up] and @[down] functions perform what is known as *chromatic transposition*, meaning they add *both* the diatonic and the alteration component of the given interval to all pitches in the given score.

We can also perform *diatonic transposition*, which adds only the diatonic component. Diatonic transpotion only makes sense relative a given tonic, so we provide one:

```music+haskell
let
  ch x = ppar [x, upDiatonic c 2 x, upDiatonic c 5 x]
in pseq $ ch <$> [c,d,e,f,g,a,g,c',b,a,g,fs,g |* 4] |/ 8
```

Here is same example, using a different tonic (`fs` instead of `c`):

```music+haskell
let
  ch x = ppar [x, upDiatonic fs 2 x, upDiatonic fs 5 x]
in pseq $ ch <$> [c,d,e,f,g,a,g,c',b,a,g,fs,g |* 4] |/ 8
```

### Scaling pitch

TODO

TODO intervals are vectors and can be scaled. However pithes live in an affine space without a specific origin, so we have to pick one:

```music+haskell
m
    </>
(scale 2 c m)
    </>
(scale 2 e m)
  where
    scale n p = pitches %~ relative p (n *^)
    m = pseq (fmap fromPitch [c..g]) |*(2/5)
```

Note how the origin stays the same under scaling.

### Inverting pitch

@[invertPitches]

```music+haskell
m
    </>
(invertPitches c m)
    </>
(invertPitches e m)
    </>
(invertPitches f m)
  where
    m = pseq (fmap fromPitch [c..g]) |*(2/5)
```

As with transposition we can define a *diatonic* form of inversion. The function is @[invertDiatonic].

```music+haskell
m
    </>
(invertDiatonic c m)
    </>
(invertDiatonic e m)
    </>
(invertDiatonic f m)
  where
    m = pseq (fmap fromPitch [c..g]) |*(2/5)
```

In this case, the origin is also used as the tonic of the implied diatonic scale.

## Listing and traversing pithes

TODO forward reference to traversals chapter, @[HasPitch]




# Harmony

While the `Pitch` and `Interval` types allow us to represent any pitch (the Western/classical framework), they do not tell us much about *harmony*. We need types to represent *collections* and *relationships* between pitches, including modes, chords and scales.

The simplest (and most general) way of doing this is to work with *generic* container types provided by Haskell: these include sets, lists, maps and so on. However music theory defines some very specific structures that are not always captured by generic containers. In this chapter we will look at some structures that make particular sense from a musical point of view.

TODO data/codata

## Ambitus and range

The `Ambitus` type represents a *range* of pitches.

TODO define/show/use

> Note: The mathematical term for is a *interval*, which we avoid for obvious reasons.

## Scales and chords

TODO we've seen several examples of affine spaces with notions of *points* and *distances*: time points and durations, pitches and intervals, spans and transformations, and so on.

Another example is the notion of scales and chords. These are (conceptually) infinite collections of points, forming a subset of a larger pitch space. By forgetting the *root* or *fundamental* of a scale/chord we obtain what is known as a mode (for scales) or a chord type (for chords).


```music+haskell
inspectableToMusic @[Mode Pitch] $

[ phrygian
, majorScale
, bluesMajor
, bluesMinor
, wholeTone
, octatonic
, thirdMode
]
```

```music+haskell
inspectableToMusic @[Scale Pitch] $

[ scale c phrygian
, scale d majorScale
, scale e bluesMajor
, scale f wholeTone
, scale g octatonic
, scale a thirdMode
]
```

```music+haskell
inspectableToMusic @[ChordType Pitch] $

[ majorTriad
, minorTriad
, augmentedChord
, diminishedChord
, halfDiminishedChord
]
```

```music+haskell
inspectableToMusic @[Chord Pitch] $

[ chord g majorTriad
, chord c minorTriad
, chord f augmentedChord
, chord eb diminishedChord
]
```
### Triadic harmony

Basic triads, seventh and ninth chords

### Non-octave repeating scales

TODO Quartal and quintal

Non-repeating/self-repeating scales (e.g. the overtone series). TODO create by unfold?

TODO chromatic scale

### Custom chords

TODO create from any interval sequence

## Infinite chords

TODO scales/chords as infinite/countable sets

Chords and scales are *countably infinite* sets. This means that we can map them directly to any other such sets, such as the set of integers. For example the chord "C major" is the set `{ Cn En Gn | n ∈ all octaves }`. Using C4 (or "middle C") as the starting point, 0 will map to C4, 1 to E4, 2 to G4, -1 to G3, -2 to E3, and so on.

Normal Haskell types correspond to sets as well. For example, our `Common.Pitch` type is the set of *all* pitches in the diatonic/chromatic system. Values such as `scale c major` of type `Scale p` correpond to some subset of `p`.

TODO inversion of basic chords like this is counter-intuitive:

```music+haskell
inspectableToMusic @[Chord Pitch] $
[ chord c majorTriad
, chord g majorMinorSeventhChord
, chord c majorTriad
]
```

```music+haskell
inspectableToMusic @[Chord Pitch] $
[ chord c $ majorTriad
, chord g $ invertChord (-1) majorMinorSeventhChord
, chord c $ invertChord 2    majorTriad
]
```

## Chord generators

TODO Scales and Chords as finite sets

Alternatively, we can view a Scale/Chord as simply the pitches of its *generating pitch set* (e.g. the tonic and the pitches generated by applying the *generating intervals* to the tonic).


Chord and Scale are instances of `HasPitch`. Using a suitable pitch type such as `Common.Pitch`, they are also instance of `Transposing`

```music+haskell
inspectableToMusic @[Chord Pitch] $
[          chord c majorTriad
, up _M2 $ chord c majorTriad
, up _M2 $ over pitches (relative c negateV) $ chord c majorTriad
]
```

This is useful for building chord sequences:

```music+haskell
inspectableToMusic @[Chord Pitch] $
mconcat [s1, up _M6 s1, _8vb $ up (_M6 ^* 2) s1]
  where
    s1 :: [Chord Pitch]
    s1 = [maj, dim, up _M2 hdim, up _P5 dom]

    maj = chord c majorTriad
    dim = chord c diminishedChord
    hdim = chord c halfDiminishedChord
    dom = chord c majorMinorSeventhChord
```



TODO reflection:

```music+haskell
compress 2 $ inspectableToMusic @[Chord Pitch] $
[                       chord c majorTriad
, over pitches (relative c negateV) $ chord c majorTriad

,                       chord c majorMinorSeventhChord
, over pitches (relative c negateV) $ chord c majorMinorSeventhChord

,                       chord c majorMajorSeventhChord
, over pitches (relative c negateV) $ chord c majorMajorSeventhChord

,                       chord c minorMinorSeventhChord
, over pitches (relative c negateV) $ chord c minorMinorSeventhChord

,                       chord c minorMajorSeventhChord
, over pitches (relative c negateV) $ chord c minorMajorSeventhChord
]
```



TODO looking up notes in a scale/chord (infinitely, Integer ->, 0 being the tonic)



TODO set operations on chords/scales (e.g. union/difference/intersection/isSubset/isPowerset etc).


### Scales versus Chords

TODO there is little difference: convert back/forth

TODO examples: Whole tone is a superset of augmented, octatonic a superset of dimimished and so on

TODO example: generate a "scale" by the union of two "chords"

Consider "scale-chord texture"

## Voicing

TODO rewrite the below, as in: Chord/Scale can be viewed as either infinite, or as having a default closed voicing. The Voiced type allow us to represent aribitrary voicings.

While working with infinite sets for scales and chords is convenient, this is not helpful when dealing with problems of *voicing*. To represent this, we will need to consider finite subsets of the infinite pitch sets we use for scales and chords.



The `voiced` function voices a chord as closely as possible above the tonic. Formally the pitches of the generating interval sequence, originating at the tonic. For example:

```music+haskell
inspectableToMusic @(Voiced Chord Pitch) $
  voiced (chord d majorTriad)
```

```music+haskell
inspectableToMusic @(Voiced Scale Pitch) $
  voiced (scale d majorScale)
```


We can also create custom voicings, using any combination of integers. Recall that `0` stands for the origin, `1` for the first note above the origin, `2` for the next and so on. Negative numbers repeat the pattern below the origin.

```music+haskell
inspectableToMusic @(Voiced Chord Pitch) $
  Voiced (chord d minorTriad) [0,1..6]
```

```music+haskell
inspectableToMusic @(Voiced Chord Pitch) $
  Voiced (chord d minorTriad) [0,2..6]
```

```music+haskell
inspectableToMusic @(Voiced Chord Pitch) $
  Voiced (chord d minorTriad) [-2,0,2,4]
```

TODO uneven voicing, e.g. [1,2,4,7] etc. and the reverse of that.


Voiced chords allow inversion:


```music+haskell
inspectableToMusic @[Voiced Chord Pitch] $
  fmap (`invertVoicing` vs) [ -1..4 ]
  where
    vs = voiced (chord c majorTriad)
```

```music+haskell
inspectableToMusic @[Voiced Chord Pitch] $
[ voiceIn 4 $ chord c majorTriad
, invertVoicing (-2) $ voiced $ chord g majorMinorSeventhChord
, voiceIn 4 $ chord c majorTriad
]
```


### Voiced vs unvoiced

For dealing with chords in the normal sense (e.g. pitches), use `Voiced Chord`.

For dealing with infinitely repeating fields of pitches, use `ChordType` and `Chord`.

## Consonance and dissonance

TODO relative dissonance of intervals, modes and chords

TODO resolution and leading notes. "Solve" an n-part voicing problem

Calculate dissonance of a chord (classical/"objective", by higest common fundamental)





# Absolute pitch

TODO in previous chapters we worked exclusivey with Pitch/Interval. These restrict
us to the Western/classical set of pitches and are relative (assuming, but not implying any particular tuning system). In this chapter we we will let go of these restrictions and look into working with both absolute pitch (arbitrary frequencies) and *alternative* pitch systems.

We will also see how *tuning systems* relate structured pitch representations (such as `Pitch`) to unstructured ones (such as `Hertz`).

## Alternative pitch representations

### Equal tempered scales

TODO equal tempered scales of any size

TODO 24TET ("quarter tones")

## Beyond diatonic/chromatic

TODO the Common.Pitch type has built-in support for chromatic/diatonic harmony. We can construct types that support other system instead.



## Tuning and intonation

### Absolute pitch

@[Hertz]

Logarithmic scales:

@[Fifths]
@[Cents]


## Tuning systems

## Spectral music

TODO Working "backwards" from absolute to relative pitch

TODO spectral dissonance using HCF
(https://harmonicratio.blogspot.com/2018/10/pursuing-clarity-through-openness-part_80.html)

























# Dynamics and Articulation

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

We can give any two dynamic values to `cresc` and `dim` (e.g. they are synonyms). A crescendo/diminuendo line will be drawn as necessary.

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

By default, accents are only applied to the first note in each phrase. We can also explicitly specify the last note, or all the notes:

@[accentLast]
@[accentAll]

```music+haskell
accentLast (pseq [c..g]|/8)
    </>
accentAll (pseq [c..g]|/8)
```

We can apply slurs and articulation marks to scores of arbitrary complexity. The library will traverse each phrase in the score and apply the articulations separately.

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

Dynamic values are overloaded in the same way as pitches. The dynamic literals are defined in `Music.Dynamics.Literal` and have type `IsDynamics a => a`.

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

For working with multi-part scores, we first need to look at the distinction between parts and instruments:

- The @[Part] type represent a *vocal or instrumental part in a composition* such as `Violin I.I`, `Trumpet II`, etc.

- The @[Instrument] type represents a *class of instruments* such as `Violin`, `Guitar`, `Maracas`, etc. This includes vocals types and, electronics etc.


A *part* is a record type consisting of (among other things) an *instrument* and a *subpart* such as `I.I`.


There is no need to explicitly create staves, brackets or braces. These are created automatically based on the parts present in the score.

To illustrate this, here is an example of a score with all the notes in the same part:

```music+haskell
ppar [c,d,fs]
```
Here is a score with instruments in different parts:

```music+haskell
ppar [c,parts' .~ violins $ d,fs]
```

The most common parts and instruments are predefined. By convention names in singlular refers to instruments, and names in plural to parts:

```haskell
flute  :: Instrument
flutes :: Part
```

It is also possible to create custom instruments and parts (TODO link).

## Basic use

### The default part

The default part is `Piano I`.

There is never any need to select the default part, but it is availble as `piano` for consistency. Equivalently, you can use `mempty :: Part`.


### Setting part and instrument

To choose a *non-default* parts or instruments, we use the `set` operator, or its infix version `.~`.

Setting just the part looks like this:

```music+haskell
parts' .~ trumpets $ ppar [c,d,fs]
```

Setting just the instrument:

```music+haskell
(parts' . instrument) .~ trumpet $ ppar [c,d,fs]
```

Setting just the subpart:

```music+haskell
(parts' . subpart) .~ 2 $ (parts' . instrument) .~ trumpet $ ppar [c,d,fs]
```

## Subdivision

TODO subparts allow arbitrarily deep nestings "Violin I.1.II" etc.

TODO Understand "overlapping" semantics, e.g. if notes overlap in "I" and "I.2" we have "overlapping events" (not OK in monophonic instruments, but see solo/altri below)

TODO show how to set explicitly VI.1, VI.2, VII etc.

## Partwise composition

We have already seen how the `</>` operator can be used to compose music "partwise". Now that we know about subparts we can see this works:

TODO

```music+haskell
c </> c
```

Because these instruments were in the same part (the default) TODO

```music+haskell
set parts' violins c </> set parts' violas c
```

These instruments were already in different parts. In this case `</>` is identical to `<>`.

```music+haskell
set parts' p c </> set parts' p c
  where
    p = set subpart 2 $ violins
```

TODO understand how `</>` is bracketed

```music+haskell
c </> (e </> g)
```
```music+haskell
(c </> e) </> g
```

The conclusion is that you should always associate `</>` to the left. This is what `rcat` does:

```music+haskell
rcat [c,e,g]
```


## Staves and parts

TODO understand that staves and parts are distinct. By default each part is drawn on its own staff (or staves, depending on the instrument).

The current state will gracefully handle overlapping notes in a single part, drawing them on separate staves, however it may not distribute things ideally across the staves. The final state should do better by default *and* allow customization.

It isn't (and should never be) *necessary* to select staves manually.

TODO proper multi-staff part support (see TODO.md).


## Updating several parts at once

TODO updating and merging parts. Or should we write about this in cobination with pitch/dynamic etc (as they're all traversal-based).

```music+haskell
arrangeFor stringOrchestra $ rcat [c',e,g_,c_]
  where
    stringOrchestra = divide 2 violins ++ [violas, cellos] -- TODO define somewhere

```

## Soloists

The solo/tutti component is useful when working with concertante scores.

@[Solo]

```music+haskell
(parts' .~ solo violin $ pseq [c,d,e,f,g,a,g,e,ds,e,cs,d,b,bb,a,ab] |/ 16)
  <>
arrangeFor stringOrchestra (pseq [rcat [c',e,g_,c_]])
  where
    stringOrchestra = divide 2 violins ++ [violas, cellos] -- TODO define somewhere
```

TODO by default `Tutti` is used. In chamber music there is usually no need to override this with `Solo`, the difference only make sense when you need to distinguish the solist.

TODO soloists *from* the orchestra/altri


## Extracting parts

TODO we can also *extract parts* from a score. Note that if you're working with MusicXML or Lilypond there is usually no need to do this explicitly.

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

## Transposing instruments

TODO we treat transposing instruments as different values. When one form of an instrument is more common this one is used as the "default", e.g. `trumpets` refers to trumpets in Bb. TODO add D trumpet etc.

TODO obtain transpotion infomation

TODO extract parts automatically, with transpotion? Could be a dynamic/backend option.

## Range

TODO obtaining range info for instruments

TODO automatica range checks



# Playing techniques

All instruments come with a variety of playing techniques, many of which produce fundamentally different sound types. We treat playing technique as a separate aspect from part and pitch.

TODO currently there is no way of preventing the a playing technique being used with the "wrong" instrument.

## Non-instrument specific techniques

### Tremolo, trills and rolls

A regular (measured) tremolo can be notated using the @[tremolo] function. Regular tremolo is is a shorthand for rapid iteration of a single note.

TODO `tremolo` should take a duration, not an integer!

```TODOmusic+haskell
tremolo 2 $ times 2 $ (c |> d)|/2
```

An unmeasured tremolo is notated using @[fastTremolo]. Unmeasured tremolo means "play individually, as fast as possible" and is a coloristic effet rather than a rhythmical shorthand.

Note that in keeping with traditional notation, we notate unmeasured tremolo using three beans. TODO allow use of Z-beam or other custoization.

```TODOmusic+haskell
fastTremolo $ times 2 $ (c |> d)|/2
```

## Repeating vs. alternating tremolo

The former is rare but happen e.g. when double-stopped strings play bow tremolo (without bariolage). The more common one is a rapid alteration among a set of notes. Logically we should treat both as an optional the property of a single chord. Alas in StandardNotation the latter is commonly written as two chords with half the duration (OR as a trill).

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

## Instrument-specific techniques

### String techniques

We can switch between bowed versus plucked strings using @[pizz] and @[arco]. The default is /arco/ (bowed). As in standard string notation this is indicated by text at the point of change:

```music+haskell
set parts' violins $
  pseq [arco $ staccato $ times 4 c, times 4 $ pizz g_ ] |/ 4
```

The text "arco" is used to cancel a previous "pizz". This is also inserted automatically. In the following example the first note in the second bar is using arco by default.

```music+haskell
set parts' violins $
  pseq [pizz $ pseq [c,c,c,c], d |* 2, pizz e |*2 ] |/ 4
```

Bow *position* on the string is indicated in a similar fashion. We support the following positions:

- Sul tasto: Close to the fingerboard
- Sul ponticello: Close to the stable
- Naturale: Normal position (the default)

As with pizz/arco, only changes are indicated:

```music+haskell
set parts' violins $
  pseq [sulTasto $ pseq [c,c,c,c], posNat d |* 2] |/ 4
```

As with "arco, the text "nat" is used to cancel a previous position and inserted by default.

```music+haskell
set parts' violins $
  pseq [posNat $ pseq [c,c,c,c], sulPont d |* 2] |/ 4
```

Bow *rotation* can be indated using one of the following:

- Col legno (tratto): play with the bow rotated to use the wooden part of the bow
- Col legno battuto: play with the wooden part of the bow only. This is normally only used for sharp attacks, hence the name.
- Senza legno: play with the bow hair (the default)

```music+haskell
set parts' violins $ pseq
  [ colLegno c
  , colLegnoBatt c
  , senzaLegno c
  ] |* 2
```

Finally, string mutes are indicated using:

- Con sordino: With mute
- Senza sordino: Without mute (the default)

```music+haskell
set parts' violins $ pseq
  [ conSord c
  , senzaSord c
  ]
  |/ 4
```

Here is an example using a combination of the above techniques:

```music+haskell
set parts' violins $ pseq
  [ conSord $ arco c
  , pizz c
  , pizz $ conSord $ pizz c
  , conSord $ colLegno $ pizz c
  ]
  |* 1.5
```

TODO chord tremolo


### Wind techniques

TODO fingering, multiphonics

TODO key sounds, percussive attacks ("pizz"), haromonics/whistle tones

### Brass techniques

Mutes are indicated just like string mutes:

- Con sordino: With mute
- Senza sordino: Without mute (the default)

```music+haskell
set parts' trombones $ pseq
  [ conSord g_
  , senzaSord g_
  ]
  |* (3/2)
```

TODO alternative mutes

TODO hand stopping

## Percussion

TODO working with instruments for percussion (much like normal instruments, though pitch may be ignored)

Note: for percussion we break the singular/plural naming convention and export a `Part` in the singular form.

The solo/tutti component is set to `Tutti` by default even though there might only be one performer in the group (the distinction would still make sense e.g. in a percussion concerto).

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

TODO rename meta to "global"?. Meta-information is global, rather than attached to a specific part. It is *defined at every point in the score with explicit change points (per type)* and always has a sensible default value (e.g. one (Reactive m) per type). All meta types are monoidal. Examples: key signature, time signature.

For the most part *logical/semantic/sounding* information is not meta (exception: tempo).

Meta-information is always *optional*. All meta-tracks are set to some sensible value by default. We can override this either globally or locally (during some specific time-span).

<!--
It is often desirable to annotate music with extraneous information, such as title, creator or, key or time signature. Also, it is often useful to mark scores with structural information such as movement numbers, rehearsal marks or general annotations. In Music Suite these are grouped together under the common label *meta-information*.

The notion of meta-data used in Music Suite is more extensive than just static values: any @[Transformable] container can be wrapped, and the meta-data will be transformed when the annotated value is transformed. This is why meta-data is often variable values, such as @[Reactive] or @[Behavior].

All time structures in Music Suite support an arbitrary number of meta-data fields, indexed by type. All meta-information is required to satisfy the `Typeable`, so that meta-data can be packed and unpacked dynamically), and `Monoid`, so that values can be created and composed without having to worry about meta-data. The `mempty` value is implicitly chosen if no meta-information of the given type has been entered: for example the default title is empty, the default time signature is `4/4`. If two values annotated with meta-data are composed, their associated meta-data maps are composed as well, using the `<>` operator on each of the types.

The distinction between ordinary musical data and meta-data is not always clear-cut. As a rule of thumb, meta-events are any kind of event that does not directly affect how the represented music sounds when performed. However they might affect the appearance of the musical notation. For example, a *clef* is meta-information, while a *slur* is not. A notable exception to this rule is meta-events affecting tempo such as metronome marks and fermatas, which usually *do* affect the performance of the music.
-->


## Title

Title, subtitle etc is grouped together as a single type `Title`, thus an arbitrary number of nested titles is supported. The simplest way to add a title is to use the functions @[title], @[subtitle], @[subsubtitle] and so son.

```music+haskell
title "Frere Jaques" $ pseq [c,d,e,c]|/4
```

Some backends may or may not render subtitles, depending on their configuration.

## Attribution

Similar to titles, the attribution of the creators of music can be annotated according to description such as @[composer], @[lyricist], @[arranger] etc. More generally, @[attribution] or @[attributions] can be used to embed arbitrary `(profession, name)` mappings.

```music+haskell
composer "Anonymous" $ pseq [c,d,e,c]
```

```music+haskell
composer "Anonymous" $ lyricist "Anonymous" $ arranger "Hans" $ pseq [c,d,e,c]|/4
```

Some backends may or may not render attribution information, depending on their configuration.

## Key signatures

By default the key signature of C is used. We can override the *global* key signature using @[keySignature].

```music+haskell
let major = True -- TODO!
in
keySignature (key db major) $ pseq [db,eb,f]
```

We can also set the key signature for a specific time span using @[keySignatureDuring].

```music+haskell
let major = True -- TODO!
in
keySignatureDuring (1 <-> 2) (key db major) $ pseq [db,eb,f]
```

A key signature change will always force a new bar.

```music+haskell
let major = True -- TODO!
in
keySignatureDuring (1.5 <-> 2) (key db major) $ pseq [db,eb,f]
```

## Time signatures

Time signatures are represented by the `TimeSignature` type. It is an instance of `Fractional`, meaning that you can use fractional literals to define it.

```haskell
2/4 :: TimeSignature
```

We also support compound time signatures:

```haskell
(3+2)/8 :: TimeSignature
```

Equivalently, we can write:

```haskell
time 4 4 :: TimeSignature
```

```haskell
compoundTime [3,2] 8 :: TimeSignature
```

The default time signature is `4/4` is used (written as *c*). We can override this globally using @[timeSignature].

```music+haskell
timeSignature (3/8) $ pseq [db,eb,f]
```

We can also set the time signature for a specific time span using  @[timeSignatureDuring]

Time signature changes will always force a new bar.

Part-specific key signatures (save for transposing instruments) are not supported.

### Converting from one time signature to another

Setting the time signature does *not* imply that the music is renotated. To accomplish this we'll need to use @[stretch] or @[compress]. For example, the following music is notated using a quarter note pulse.

```music+haskell
let
  ch = ppar [e,g,c']
  waltz = pseq [c,ch,ch,g_,ch,ch] |* (1/4)
in
timeSignature (3/4) waltz
```

To *renotate* this to eight notes, we stretch the music by `1/2` and apply the new time signature:

```music+haskell
let
  ch = ppar [e,g,c']
  waltz = pseq [c,ch,ch,g_,ch,ch] |* (1/4)
in
timeSignature (3/8) $ compress 2 waltz
```

This provide more flexibility for renotation. For example we can easily renotate a passage from `4/4` to `12/8` as follows:

```music+haskell
let
  ch = ppar [e,g,c']
  waltz = times 2 $ pseq [c,ch,ch,g_,ch,ch] |* (1/4)
in
timeSignature (4/4) $ compress 3 $ waltz
```

```music+haskell
let
  ch = ppar [e,g,c']
  waltz = times 2 $ pseq [c,ch,ch,g_,ch,ch] |* (1/4)
in
timeSignature (12/8) $ compress 2 $ waltz
```

Polymetric notation is not supported: you must pick one global time signature for each section of the score.


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

TODO rendering tempo?

Tempo changes will always force a new bar.

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

Explicitly set barlines will or course force a new bar.

## Clefs

The standard for each instrument is used by default.

TODO override the default

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

Rehearsal marks will always force a new bar.

## Annotations

Annotations are simply textual values attached to a specific section of the score. In contrast to other types of meta-information annotations always apply to the whole score, not to a single part. To annotate a score use @[annotate], to annotate a specific span, use @[annotateSpan].

Annotations are *invisible by default*. To show annotations in the generated output, use
@[showAnnotations].

```music+haskell
showAnnotations $ annotate "First note" c |> d |> annotate "Last note" d
```

## Custom meta-information

TODO works for any `Typeable` `Monoid`.

TODO Use more specicif wrappers to preserve `Transformable`, `Reversible` etc.


@[HasMeta]

@[setMetaAttr]

@[setMetaTAttr]



































# Time, rhythm and form

## Basic time types

Time points and vectors are represented by two types @[Time] and @[Duration]. The difference between these types is similar to the distinction between points and vectors in ordinary geometry. One way of thinking about time vs. duration is that duration are always *relative* (i.e. the duration between the start of two notes), while *time* is absolute.

Time points form an affine space over durations, so we can use the operators @[.+^] and @[.-.] to convert between the two.

The @[Span] type represents a *slice* of time. We can represent spans in exactly three ways: as two points representing *onset* and *offset*, as one point representing *onset* and a duration, or alternatively as a point representing *offset* and a duration. To convert between these representations, we can use @[onsetAndOffset], @[onsetAndDuration] and @[durationAndOffset], which are *isomorphisms* using the definition from the `lens` package.

TODO time/span/duration examples

### Spans as transformations

TODO explain

An alternative view of span: as an *affine transformation*.

For those familiar with linear algebra or computer graphics: Because time is one-dimensional a *linear transformation matrix* in time is a 1x1 matrix (e.g. a scalar). Its *affine transformation matrix* is a 2x2 matrix. We can understand the monoid instance for @[Span] as multiplication of 2x2 matrices.

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

Explain delay-invariant transformations: applying transformations to `Span` performs the delay/translation, applying to `Duration` does not. Note that this does *not* invalidate the laws. We can think of our time types as coming in two shapes:

- Translation-invariant types such as `Duration` are "floating" without being anchored to specific start/stop time (though they still have a duration)
- Translation-variant types such as `Span` have both a specific duration and a specific point in which they "occur" relative to ther events.

We will see much more of this distinction later on.


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


TODO viewing a score as a Behavior (concatB). Useful for "vertical slice view" of harmony, as in https://web.mit.edu/music21/doc/usersGuide/usersGuide_09_chordify.html

TODO Aligned, "floaters"

Natural way of modelling pickups/upbeats etc. Can be combined with "beat hierarchy" model


Here is an example. Without upbeat:

```music+haskell
(pseq [g_,a_,b_]|/2 |> pseq [c, c, d, d]) |/ 4
```


With upbeat.

```music+haskell
inspectableToMusic @[Aligned (Voice Pitch)] $

delay 2 -- TODO get rid of this, see wall of shame

[ av |/ 2
, av |/ 4
, av |* (2/3)
]
  where
    av = ([g_,a_,b_]|/2) ||> [c, c, d, d]
```

TODO sequential composition of aligned voices "snap to next stressed beat":
`snapTo :: (HasPosition a, Transformable a) => Stream Time -> [a] -> [a]`


## Patterns

TODO a Pattern can be throught of as a generalization of a rhythm or beat. They are similar to scores, but are infinite. Each pattern is created by repeating a number of layers. Every pattern will repeat itself, though the repetition frequence may be very long.

TODO more idiomatic ways of buildings patterns



```music+haskell
renderPattern (a <> b) (0 <-> 4)
  where
    a = parts' .~ mempty $ rhythmPattern [3,3,4,2,4] |/ 8
    b = parts' .~ flutes $ rhythmPattern [1] |/ 8
    -- TODO use claves, maracas here
```

```music+haskell
renderPattern (a <> b) (0.5 <-> 1.5)
  where
    a = parts' .~ mempty $ rhythmPattern [3,3,4,2,4] |/ 8
    b = parts' .~ flutes $ rhythmPattern [1] |/ 8
```

TODO Patterns are @[Transformable], @[Transposing], @[Attenuable] and so on, so many expressions that work for scores and voices also work for patterns.

```music+haskell
renderPattern (stretch 0.5 $ up m3 $ a <> b) (0 <-> 2)
  where
    a = parts' .~ mempty $ rhythmPattern [3,3,4,2,4] |/ 8
    b = parts' .~ flutes $ rhythmPattern [1] |/ 8
```

TODO renderPatternsRel

TODO renderPatternsAbs


TODO finish/move to examples:

```music+haskell
inspectableToMusic bachCMajChords
  where
    bachCMajChords :: Score [Pitch]
    bachCMajChords =
      [(0 <-> (1/2),[c,e,g,c',e'])^.event,((1/2) <-> 1,[c,e,g,c',e'])^.event,(1 <-> (3/2),[c,d,a,d',f'])^.event,((3/2) <->
      2,[c,d,a,d',f'])^.event,(2 <-> (5/2),[b_,d,g,d',f'])^.event,((5/2) <-> 3,[b_,d,g,d',f'])^.event,(3 <->
      (7/2),[c,e,g,c',e'])^.event,((7/2) <-> 4,[c,e,g,c',e'])^.event,(4 <-> (9/2),[c,e,a,e',a'])^.event,((9/2) <->
      5,[c,e,a,e',a'])^.event,(5 <-> (11/2),[c,d,fs,a,d'])^.event,((11/2) <-> 6,[c,d,fs,a,d'])^.event,(6 <->
      (13/2),[b_,d,g,d',g'])^.event,((13/2) <-> 7,[b_,d,g,d',g'])^.event,(7 <-> (15/2),[b_,c,e,g,c'])^.event,((15/2) <->
      8,[b_,c,e,g,c'])^.event,(8 <-> (17/2),[a_,c,e,g,c'])^.event,((17/2) <-> 9,[a_,c,e,g,c'])^.event,(9 <->
      (19/2),[d_,a_,d,fs,c'])^.event,((19/2) <-> 10,[d_,a_,d,fs,c'])^.event,(10 <-> (21/2),[g_,b_,d,g,b])^.event,((21/2) <->
      11,[g_,b_,d,g,b])^.event,(11 <-> (23/2),[g_,bb_,e,g,cs'])^.event,((23/2) <-> 12,[g_,bb_,e,g,cs'])^.event,(12 <->
      (25/2),[f_,a_,d,a,d'])^.event,((25/2) <-> 13,[f_,a_,d,a,d'])^.event,(13 <-> (27/2),[f_,ab_,d,f,b])^.event,((27/2) <->
      14,[f_,ab_,d,f,b])^.event,(14 <-> (29/2),[e_,g_,c,g,c'])^.event,((29/2) <-> 15,[e_,g_,c,g,c'])^.event,(15 <->
      (31/2),[e_,f_,a_,c,f])^.event,((31/2) <-> 16,[e_,f_,a_,c,f])^.event,(16 <-> (33/2),[d_,f_,a_,c,f])^.event,((33/2) <->
      17,[d_,f_,a_,c,f])^.event,(17 <-> (35/2),[g__,d_,g_,b_,f])^.event,((35/2) <-> 18,[g__,d_,g_,b_,f])^.event,(18 <->
      (37/2),[c_,e_,g_,c,e])^.event,((37/2) <-> 19,[c_,e_,g_,c,e])^.event,(19 <-> (39/2),[c_,g_,bb_,c,e])^.event,((39/2) <->
      20,[c_,g_,bb_,c,e])^.event,(20 <-> (41/2),[f__,f_,a_,c,e])^.event,((41/2) <-> 21,[f__,f_,a_,c,e])^.event,(21 <->
      (43/2),[fb__,c_,a_,c,eb])^.event,((43/2) <-> 22,[fb__,c_,a_,c,eb])^.event,(22 <-> (45/2),[ab__,f_,b_,c,d])^.event,((45/2)
      <-> 23,[ab__,f_,b_,c,d])^.event,(23 <-> (47/2),[g__,f_,g_,b_,d])^.event,((47/2) <-> 24,[g__,f_,g_,b_,d])^.event,(24 <->
      (49/2),[g__,e_,g_,c,e])^.event,((49/2) <-> 25,[g__,e_,g_,c,e])^.event,(25 <-> (51/2),[g__,d_,g_,c,f])^.event,((51/2) <->
      26,[g__,d_,g_,c,f])^.event,(26 <-> (53/2),[g__,d_,g_,b_,f])^.event,((53/2) <-> 27,[g__,d_,g_,b_,f])^.event,(27 <->
      (55/2),[g__,eb_,a_,c,fs])^.event,((55/2) <-> 28,[g__,eb_,a_,c,fs])^.event,(28 <-> (57/2),[g__,e_,g_,c,g])^.event,((57/2)
      <-> 29,[g__,e_,g_,c,g])^.event,(29 <-> (59/2),[g__,d_,g_,c,f])^.event,((59/2) <-> 30,[g__,d_,g_,c,f])^.event,(30 <->
      (61/2),[g__,d_,g_,b_,f])^.event,((61/2) <-> 31,[g__,d_,g_,b_,f])^.event,(31 <-> (63/2),[c__,c_,g_,bb_,e])^.event,((63/2)
      <-> 32,[c__,c_,g_,bb_,e])^.event,(32 <-> 33,[c__,c_,f_,a_,e])^.event,(33 <-> 34,[c__,b__,f,g,d'])^.event,(34 <->
      35,[c__,c_,e,g,c'])^.event]^.score


    bachCMajPattern :: (Reversible a, Num a) => Pattern a
    bachCMajPattern = newPattern $ stretchTo 1 $ (view voice) $ fmap pure [0,1,2,3,4,2,3,4]
```

## Splitting and reversing

@[Splittable]

@[split]
@[beginning]
@[ending]

```music+haskell
inspectableToMusic @[Voice Pitch] $

[ beginning 0.5 melody
, ending 0.5 melody
]
  where
    melody = {- accent $ legato -} mconcat [d, mconcat [g,fs]|/2,bb|*2]|/4
```

```music+haskell
inspectableToMusic @[Voice Pitch] $

[ beginning (1/2+1/8) melody
, ending (1/2+1/8) melody
]
  where
    melody = {- accent $ legato -} mconcat [d, mconcat [g,fs]|/2,bb|*2]|/4
```

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





## Building larger musical structures

TODO general intro on how to build/organize larger forms in Haskell/pure FP.

### Basic repetition

@[times]

```music+haskell
let
    melody = legato $ pseq [c,d,e,c]|/16
in times 4 $ melody
```


variation

- Basic repeatition: @[times], @[replicate]

- Lambdas anda abstracting out

- Infinite streams, take, drop

- "Indexed loops", zips, zipWith [0..]

- "Stateful" loops, `for`, `traverse` (state monad example)

- Randomness

- Do notation, comprehensions

## Time, change and sampling

TODO Behavior and Reactive, Sampling







# Traversals

TODO previous chapters have focused on *building* music by composing musical expressions. In this chapter we will look at various ways of *inspecting* musical expressions.

TODO explain how this works within pure FP: no change, just creating new structures

TODO traverals are a very powerful concept and we'll only

A traversal that targets exactly one element is known as a *lens*. We've already seen examples of lenses and traversals in the chapters on [dynamics](TODO) and [articulation](TODO) in the form of `.~` (or `set`) operator.

> Note: For those familiar with Haskell: Music Suite defines lenses and traversals compatible with `lens` (and `microlens`).

Can be used to:

- Visit elements in a score
- Querying/folding
- Updating aspects


TODO monomorphic and polymorphic traversals (and switch names: `pitch'` is used much more than `pitch`!)

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

## Traversing the components of a note

### Pitches, dynamics and articulations

Music Suite defines traversals and lenses for all of the standard musical aspects (pitch, dynamic, articulation and so on). If you've been following the previous chapters, you might have seen examples of these already: expressions such as `pitches .~ c`, `dynamics .~ ff` or `over dynamics (+ 1)` make use of traversals to *update* all pitches, dynamics and so on, in a given piece of music.

### Parts and playing techniques

TODO

### Traversals vs. Lenses (singular vs plural)

TODO

### Polymorphic updates

TODO introducing the polymorphic version of the lenses/traversals (`pitch`, `dynamic` etc.)

TODO polymorphic update example (e.g. `Common.Pitch` vs `Hertz`)

TODO explain the type families: Pitch, SetPitch etc.


## Phrase traversals

TODO explain how they work


Any consequtive sequence of notes will be trated as a phrase. Rests separate phrases:

```music+haskell
over (phrases' . Control.Lens._head) (up _P8) $ bar <> delay 1 bar <> delay 2 bar
  where
    bar = pseq [c,c,c] |/ 4
```

In a multi-part score phrases are traversed per part, so this works:

```music+haskell
over (phrases' . Control.Lens._head) (up _P8) $ bar </> delay (3/4) bar </> delay (5/8) bar
  where
    bar = pseq [c,c,c] |/ 4
```

Overlapping notes *in the same part* are ignored:

```music+haskell
over (phrases' . Control.Lens._head) (up _P8) $ bar <> delay (1/8) bar
  where
    bar = pseq [c,c,c] |/ 4
```

## Filtered traversals

Filtered traversals operate on the elements selected by another traversals if they match a specific predicate. This is similar to "where" clauses in query languages such as SQL:

This example transposes all notes with a duration less than `2`:

```music+haskell
inspectableToMusic @(Voice [StandardNote]) $

over t (up _P8) [d,d,d |* 2,d] |/ 4
  where
    t = notes . each . filtered (\x -> x^.duration < 2)
```

TODO `backwards`?

TODO other SQL-like constructs, e.g. LIMIT and ORDER BY.
















<!--
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

-->

# Randomness

TODO Reader monad of seed/Random generator state


# Space

TODO very simple space representation (e.g. Angle), minimal example using Ambisonics?






# Import and Export

TODO basic structure/aproach to import and export

## Prelude/Inspectable

TODO point here is to fix a "default" type for rendering/export purposes. This is used to monomorphize expressions written in the polymorphic combinators of the library. The default type is knonw as `Music`.

Inspectable renders a type by converting it into an exportable type. See TODO.md re: defaulting.

TODO how to export/pick format


## Overview of formats

### MIDI

The MIDI output backend generates type 1 (multi-track) Standard MIDI files Standard MIDI files.

The MIDI format is suitable for generating sound using a software synthesizer such as [Timidity](TODO link) or [Fluidsynth](TODO link), or for exporting music to a Digital Audio Workstation (DAW) software. It is not suitable for exporting to score writing software (use MusicXML).

### Lilypond

The Lilypond output backend generates Lilypond markup code compatible with Lilypond 2.18.2 or later.

Lilypond is a text-only music type setting program and produces very high-quality scores. The Lilypond backend is suitable if you want to generate an entire score using Music Suite. It is also suitable for small examples and is the backend used for all examples in the Music Suite documentation.


### MusicXML

The MusicXML output backend generates XML compatible with the MusicXML 3.0.

MusicXML files can be imported by most music typesetting programs, including
Sibelius, Finale and MuseScore.

The MusicXML backend is suitable if you want to use Music Suite for generating
parts of a score, but perform manual editing on the output result.




### ABC Notation

TODO

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

Should render >1 tuplet:

```music+haskell
let
  ch = ppar [e,g,c']
  waltz = times 2 $ pseq [c,ch,ch,g_,ch,ch] |* (1/4)
in
timeSignature (4/4) $ compress 3 $ waltz
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

Music Suite is indebted to many other previous libraries and computer music environments, particularly [Common Music][common-music], [PWGL][pwgl], [nyquist][nyquist], [music21][music21], [Lilypond][lilypond] and [Abjad][abjad]. Some of the ideas for the quantization algorithms came from [Fomus][fomus].

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
