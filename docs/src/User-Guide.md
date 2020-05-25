# Tutorial

## Installing

<!--
TODO Docker or other "easy" install options
-->

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


<!--
### Using an interactive environment

TODO "visual interpreter" docs
-->

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

> Note: The `|>` operator means "sequential composition". We will introduce this [properly](#composition-operators) later on.

## Let and where

The `let ... in` construct introduces a temporary name for an expression.


```music+haskell
let
  x = c |> d
  r = 1/3
in
x </> delay r x
```

The `where` construct is similar, but takes the bindings *after* the expression.

```music+haskell
x </> delay r x
  where
    x = c |> d
    r = 1/3
```

## Octaves and accidentals

We can change octaves and accidentals:

```music+haskell
_8va c
```

```music+haskell
sharpen c
```

> Note: We will lean many more ways of entering pitch in the [next chapter](#pitch).

## Basic dynamics and articulations

We can add articulations and change dynamics:

```music+haskell
accent c
```

```music+haskell
level ppp c
```


We will see more examples of this later on as well.

The `$` operator is pronounced "apply". `f $ g x` is the same as `f (g x)`, so the apply operator is an alternative to writing parentheses:

```music+haskell
staccato $ stretch (1/8) c
```

## Composition operators

So far we have worked with a single note, which is not particularly interesting from a musical point of view. To combine multiple notes into a larger score we need a form of *composition*.

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
pseq [c,e,g] |* (1/4)
```

For `x <> y <> z ...`, we can write @[ppar] `[x, y, z, ...]` .

```music+haskell
ppar [c,e,g] |/ 2
```

For `x </> y </> ...` the syntax is @[rcat] `[x, y, z ...]`.

```music+haskell
rcat [c,e,g] |/ 2
```

## Comments

Comments are the same as in regular Haskell.

```haskell
-- This is a single-line comment

{-
 This is
 a multi-line
 comment!
-}
```


## Rests, tuplets and ties

There is never any need to explicitly create rests, tuplets or ties in Music Suite. Instead, each note exists in a dedicated time span, which can be inspected and transformed. When we compose music expressions in parallel, all notes are interleaved without affecting their onset or duration.

Notes with the same onset and offset are rendered as chords by default.

```music+haskell
pseq [c,d,e,c] <> pseq [e,f,g,e] <> pseq [g,a,b,g]
```

Or, equivalently:

```music+haskell
ppar [c,e,g] |> ppar [d,f,a] |> ppar [e,g,b] |> ppar [c,e,g]
```

To prevents notes from being merged into chords we must *explicitly* put them in separate parts. The `</>` and `rcat` combinators is a simple way of doing this.

To create space in our scores we can use `rest`. Rests are empty placeholders which take up space when using sequential composition but do not show up in the final score:

```music+haskell
times 4 (accentAll g|*2 |> rest |> pseq [d,d]|/2)|/8
```

Rests can be stretched and delayed just like notes.

Any note that crosses a barline will be notated using ties:

```music+haskell
c |* (9/8) |> d |* (7/8)
```

> Note: To change the position of a barline, see [time signatures](#time-signatures).

Similarly, durations that do not fit into standard note durations are notated using dots or tuplets:

```music+haskell
compress 4 (pseq [c |*3, d |* 3, e |* 2]) |> compress 5 (pseq [f,e,c,d,e]) |> d
```



## Functions and types

So far we have written our musical expressions in terms of pre-defined expressions and functions. However the real power of using a functional language is to be able to define our own functions.

> Note: In Haskell, functions and operators are the same, apart from syntax.


TODO basic functions

### Function composition

Analogously to composition of music seen above we can compose *functions*.

Here is an example using function composition. The dot operator `.` is used to compose the function `up _P8` (which transpose thes the music up by one octave), `compress 2` and `delay 3`. The composed functions are applied in *left to right order*.

```music+haskell
(up _P8 . compress 2 . delay 3) c
```

## More examples

Of course, the combinators we have seen so far such as `stretch`, `_8va` and so on work on arbitrarily complex scores, not just single notes. We can also nest most function within applications of the composition operators.

```music+haskell
_8va $ pseq [c, d]
```

```music+haskell
let
  x = pseq [c, d]
in
pseq [x, up m3 x]
```

Here is a more complex example using all forms of composition:

```music+haskell
let
  scale = pseq [c, d, e, f, g, a, g, f] |/ 8
  triad a = a <> up _M3 a <> up _P5 a
in up _P8 scale </> (triad c) |/2 |> (triad g_) |/2
```

## Understanding composition types

Looking at the type of the composition operators, it becomes clear that `|>` and `</>` are in fact based on `<>`:

```haskell
(<>)  :: Semigroup a => a -> a -> a
```

```haskell
(|>)  :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
```

<!--
TODO refer back to previous table. For example `Music.Pitch.Common` distinguishes between enharmonics, while `Integer` does not.
-->

### Interval names

Here and elsewhere in Music Suite, the convention is to follow standard theoretical
notation, so *minor* and *diminished* intervals are written in lower-case, while *major*
and *perfect* intervals are written in upper-case.

```haskell
(</>) :: (Semigroup a, HasParts a, HasSubpart p, p ~ Part a) => a -> a -> a
```

In fact the `<>` operator is used for almost all forms of composition in Music Suite. The other operators simply peform some kind of manipulation on their values before or after composing.

### Musical aspects

While music is often though to be concerned primarily with time and pitch, Music Suite also allow representation of other "dimensions", including:

- Dynamics: the *loudness* of a sound
- Articulation: the *attack* and *relative length* of a sound
- Instruments, parts and playing techniques, all of which affect *timbre*

We'll sometimes refer to these collectively as *aspects*. Each aspect comes with its own traversal class, which is named after it, `HasPitches`, `HasDynamics`, `HasArticulations`, `HasParts`, `HasTechniques`, and so on.

## Wrapping up

This concludes the first chapter of the manual. We have seen how to write simple pieces, using melody, harmony and voices. You should now know enough to start working on your own music. You will find many more examples of pieces in the [examples directory](https://github.com/hanshoglund/music-suite/tree/master/examples).

The rest of this manual can be read as a *reference*. We will be looking at musical aspects such as pitch, dynamics, rhythm, form and orchestration in detail. In general these chapters can generally be read in any order.




# Pitch

## The Pitch type

The @[Pitch] representation implements the pitch of common (or Western) music notation, with built-in support for the diatonic/chromatic transposition, enharmonics and spelling. As we shall see later this is not the only way of representing pitch in Music Suite, but it is common enough to be the default.

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

> Note: Music Suite uses C major by default, so all altered pitches are rendered as accidentals. See [key signatures](#key-signatures) for how to change this.


### Pitch overloading

To facilitate the use of non-standard pitch, the standard pitch names are provided as overloaded values, referred to as *pitch literals*.

To understand how this works, think about the type of numeric literal. The values $0, 1, 2$ etc. have type `Num a => a`, similarly, the pitch literals $c, d, e, f ...$ have type @[IsPitch] `a => a`.

The overloading is not limited to pitch types but also to containers types such as [scores](#scores) and [voices](#voices).

```haskell
return (c::Note) == (c::Score Note)
```

> Hint: Use [`-XTypeApplications`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications) to restrict the type of an pitch. For example: `id @Pitch c`

> Hint: Use `fromPitch` to convert a concrete pitch to `IsPitch a => a`.

## The Interval type

The @[Interval] type represents common/Western classical *intervals*.

As is common in music theory notation, *minor* and *diminished* intervals are
written in lower-case, while *major* and *perfect* intervals are written in
upper-case. Here are some examples:

```music+haskell
inspectableToMusic @[Interval] $
[ m3
, _M3
, _P5
, d5
, m9
, d12
]
```

Similar to @[sharpen] and @[flatten], the @[augment] and @[diminish] functions can be used
to alter the size of an interval. For example:

```music+haskell
let
    intervals = [diminish _P5, (diminish . diminish) _P5]
in pseq $ fmap (`up` c) intervals
```



### Simple and compound intervals

A simple interval is an interval spanning less than one octave. Intervals spanning octave or more are called compound intervals, as they can be obtained by adding one or more octaves to a simple interval.

```haskell
>>> simple _P11
_P4

>>> octaves @Interval _P4
0

>>> octaves @Interval _P11
1
```

### Negative intervals

A *negative* interval is a compound interval with a negative octave number.

```haskell
>>> octaves @Interval (-_P5)
-1
```

```music+haskell
inspectableToMusic @[Interval] $
[ m3
, -m3
]
```

The *inversion* of an interval is defined as `simple . negate`:

```haskell
>>> invert _P4
_P5

>>> invert _P11
_P5

>>> invert m3
_M6
```


### Number, quality, alteration

Intervals be understood as:

- A pair of diatonic and (total) chromatic steps, *or*
- A pair of diatonic steps and alteration, *or*
- A pair of @[number] and @[quality]

Not all combinations of number and quality makes sense.

For numbers, we follow traditional music theory conventions in counting from one. In other words, a second consists of one diatonic step, a third of two diatonic steps, and so on. We can convert between these using @[diatonicSteps]:

```haskell
>>> second^.diatonicSteps
1

>>> (2 :: Number)^.diatonicSteps
1 :: DiatonicSteps

>>> (3 :: DiatonicSteps)^.from diatonicSteps
4 :: Number
```

> Warning: The number `0` is undefined.

We can extract @[name], @[accidental] and @[octave] number from a pitch:

```haskell
>>> name c
C :: Name

>>> accidental c
natural

>>> accidental cs
sharp
```


### Interval overloading

Interval names are overloaded in a manner similar to pitches, and are consequently referred to as *interval literals*. The corresponding class is called @[IsInterval].

> Hint: Use [`-XTypeApplications`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications) to restrict the type of an interval. For example: `id @Interval m3`


## Converting between intervals and pitches

We can add pitches and intervals using the @[.-.] and @[.+^] operators. This is because pitches form an @[AffineSpace], with interval as the underlying @[VectorSpace]. Later on we will see that many types in Music Suite conform to this pattern.

```haskell
>>> m3 ^+^ m3
d5 :: Interval

>>> c .+^ m3
eb :: Pitch

>>> eb .-. c
m3 :: Interval
```

> Hint: The `.` points towards the pitches in the `AffineSpace` (pitches) while the caret `^` points towards the intervals in the `VectorSpace`.

The @[relative] function lifts an interval function into pitch *relative* a given origin (hence the name).

```haskell
relative (c :: Pitch) (^* 2) :: Pitch -> Pitch
```

## Enharmonics

The @[HasSemitones] class provides the enharmonic equivalence relation.

You can use the `=:=` operator to compare for enharmonic equivalence.

```haskell
>>> id @Interval _A2 == m3
False

>>> id @Interval _A2 =:= m3
True
```

### Pitch equality and ordering

The `Pitch` type has instances for the `Eq` and `Ord` type classes, representing equality and ordering respectively.

As we have already seen, equality of pitches takes spelling into account, so e.g. `cs /= db` holds. There are many ways of defining orderings on pitches: the default ordering compares diatonic steps first, alteration second.

```haskell
>>> sort [(cs :: Pitch), db, dbb]
[cs, dbb, db]
```

To get compare or sort pitches enharmonically you can use `sortOn`:

```haskell
>>> sortOn (semitones . (.-. c)) [(cs :: Pitch), db, flatten db]
[dbb, cs, db]
```

Semitones are also used to describe "non-diatonic" intervals such as @[tone], @[tritone], etc.

### Spelling

We can *respell* enharmonically equivalent pitches by using a @[Spelling].

```haskell
>>> spell usingSharps tritone
_A4
```


```music+haskell
pseq $ fmap (\x -> over pitches' (relative c $ spell x)  $ ppar [as,cs,ds,fs])
[ usingSharps
, usingFlats
, modally
]
```

<!-- TODO simpler short cut for `over pitches' (relative c $ spell ...)`: -->

```music+haskell
x </> over pitches' (relative c $ spell modally) x
  where
    x = pseq [cs,flatten db,bs]
```


## Transposing and inverting music

The `VectorSpace` and `AffineSpace` allow us to apply *affine transformations* to pitches. Because pitch is (roughly) a one-dimensional space, this means scaling and transpotion. Let's look at some examples.

> Warning: The musical term *transposition* is known as *translation* in maths (where *transposition* means something else entirely!).



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

As we have seen intervals form a *vector space* and pitches an associated *affine space*. This implies we can define a form of scalar multiplication.

However pitches live in an affine space without a specific origin, so we have to pick one:

```music+haskell
m
    </>
(scale 2 c m)
    </>
(scale 2 e m)
  where
    scale n p = pitches %~ relative p (n *^)
    m = pseq (fmap fromPitch [c,d,e,f,g]) |*(2/5)
```

Note how the origin stays the same under scaling.

### Inverting pitch

The @[invertPitches] function is a shorthand for the special case of scaling by `-1`:

```music+haskell
m
    </>
(invertPitches c m)
    </>
(invertPitches e m)
    </>
(invertPitches f m)
  where
    m = pseq (fmap fromPitch [c,d,e,f,g]) |*(2/5)
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
    m = pseq (fmap fromPitch [c,d,e,f,g]) |*(2/5)
```

In this case, the origin is also used as the tonic of the implied diatonic scale.

## Listing and traversing pitches

You can extract all the pitches from a piece of music like this:

```music+haskell
inspectableToMusic $
  toListOf pitches' (pseq [c,d, ppar[e,g]] :: Music)
```

> Note: `pitches` is a example of a [traversal](#traversals). We'll learn more about these later on.


## The Transposable class

The type of the previous operations mention @[Transposable]:

```haskell
    up :: Transposable a => Interval a -> a -> a

    octavesDown :: (AffinePair v p, Transposable p) => Scalar (Interval a) -> a -> a

    invertPitches :: Transposable a => Pitch a -> a -> a
```

@[Transposable] is in fact a synonym for the following set of constraints:

- `HasPitches' a`, meaning that `a` is some type supporting pitch traversals
- `AffinePair (Interval a) (Pitch a)`, meaing that the pitch type is an affine space and the interval its underlying vector space
- `IsInterval` and `IsPitch`, meaning that We can lift standard pitch/interval
  names into the pitch space, so expressions such as `cs` and `m3` makes sense
- `Num (Scalar v)`, meaning that we can scale the intervals




# Harmony

While the `Pitch` and `Interval` types allow us to represent any pitch (the Western/classical framework), they do not tell us much about *harmony*. We need types to represent *collections* and *relationships* between pitches, including modes, chords and scales.

While we can of course course use regular data structures like tuples, lists and maps for this, Music Sutie also defines some structures that makes particular sense from a musical point of view.

## Ambitus

The @[Ambitus] type represents a *range* of pitches. We can think of an ambitus as an interval with a starting point, or (equivalently) as a pair of pitches.

> Note: For pitch, we use *ambitus* instead of the ambigous *range* or *interval*.

```music+haskell
inspectableToMusic @[Ambitus Interval Pitch] $

[ Ambitus c g
, Ambitus c_ c''
]
```

Note that the `Ambitus` type constructor is parameterized on both the pitch and interval type. Like most pitch contrainers it is a [bifunctor](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Bifunctor.html).

```music+haskell
inspectableToMusic @[Ambitus Interval Pitch] $

[                  Ambitus c g
, fmap (const c) $ Ambitus @Interval @Pitch c g
]
```

It is also an instance of `Transposable`, so all the pitch operations from the previous section work for @[Ambitus] as well:

```music+haskell
inspectableToMusic @[Ambitus Interval Pitch] $

[          Ambitus c g
, up _P5 $ Ambitus c g
]
```

You can extract the range of any piece of music using @[pitchRange]:

```music+haskell
pseq [c,d,fs,g,db,c,b_,c,g,c,e] |/ 8
```

```music+haskell
inspectableToMusic @(Maybe (Ambitus Interval Pitch)) $

pitchRange @Music $ pseq [c,d,fs,g,db,c,b_,c,g,c,e] |/ 8
```

## Scales and chords

The @[Scale] and @[Chord] types represent infinite collections of pitches, anchored at some absolute pitch known as the *tonic*. Like @[Ambitus], @[Scale] and @[Chord] are type constructors taking two type parameters for interval and pitch respectively.

```music+haskell
inspectableToMusic @[Scale Interval Pitch] $

[ scale c phrygian
, scale d majorScale
, scale e bluesMajor
, scale f wholeTone
, scale g octatonic
, scale a thirdMode
]
```

```music+haskell
inspectableToMusic @[Chord Interval Pitch] $

[ chord g majorTriad
, chord c minorTriad
, chord f augmentedChord
, chord eb diminishedChord
]
```

The @[scale] and @[chord] functions take two parameters: the *tonic* (e.g. the absolute pitch at which the chord is centered) and a *mode* or *chord type* describings the characteristics of the chord. Most common scale and chord types are pre-defined, but as will see later it is also possible to make up custom scales and chords.

```music+haskell
inspectableToMusic @[ChordType Interval Pitch] $

[ majorTriad
, minorTriad
, augmentedChord
, diminishedChord
, halfDiminishedChord
]
```

```music+haskell
inspectableToMusic @[Mode Interval Pitch] $

[ majorScale
, lydian
, wholeTone
, octatonic
]
```

### Scales versus Chords

As musicians we tend to think of scales and chords as distinct entities. From a structural point there is very little difference: both represent a subset of some larger pitch space. The main difference is in how they are *used*: scales provide pitch material for melodies and chords, while chords are played in parallel, after applying textures, voicings and so on.

We can convert freely between scales and chords:

```haskell
chordToScale (chord c majorTriad)
  :: Chord Interval Pitch

chordToScale (chord c majorScale)
  :: Scale Interval Pitch

chordToScale (chord c majorTriad)
  :: Scale Interval Pitch
```

<!--
TODO examples: Whole tone is a superset of augmented, octatonic a superset of dimimished and so on

TODO example: generate a "scale" by the union of two "chords"

Consider "scale-chord texture"
-->

### Chords are infinite

It's imporant to  understand that while chords and scales are conceptually infinite, they are always generated by repetition of a finite set of intervals. We refer to this as the *generator* of the chord or scale. While most common scales and chords repeat at the octave, this is not always the case: for example diminished chords repeat at a smaller interval (minor third), and many chords used in jazz such as 9th, 11th and 13th chords repeat at intervals larger than an octave.

We can extract the generating sequence of a chord or scale using @[generator]:

```haskell
>>> generator majorTriad
_M3 :| [m3,_P4] :: List.NonEmpty Interval

>>> generator minorTriad
m3 :| [_M3,_P4] :: List.NonEmpty Interval
```

The repeating interval of the chord is the sum of the vectors in the generator sequence:

We can inspect the *repeating interval* of a scale like this:

```haskell
>>> repeatingInterval majorScale
_P8
```

All scales and chords we have seen so far repeat at the octave, but this is not a hard requirement. For example @[quartal] and @[quintal] chords can be seen as one-note scales repeating at the eponymous interval:

```music+haskell
inspectableToMusic @[Voiced Chord Interval Pitch] $

[ voiceIn 5 $ chord c quartal
, voiceIn 4 $ chord c quintal
]
```

Similarly *clusters* are one-note scales repeating at the second:

```music+haskell
inspectableToMusic @[Voiced Chord Interval Pitch] $

[ voiceIn 5 $ chord c chromaticCluster
, voiceIn 7 $ chord c wholeToneCluster
]
```

And we can repeat at arbitrary intervals:

```music+haskell
inspectableToMusic @[Voiced Chord Pitch] $

[ voiceIn 3 $ chord c $ repeating m7
]
```


```haskell
>>> repeatingInterval majorScale
_P8 :: Interval
```

### Looking up pitches

We can look up pitches in any chord or scale using the `index` function. This converts any chord into a total function of `Integer`. The tonic is mapped to zero, the positive numbers to all notes *above* the tonic and negative numbers to all notes *below* the tonic. You can use

```haskell
>>> index (chord c majorTriad) 0
c

>>> fmap (index $ chord c majorTriad)) [-1,0,1,2]
[g_,c,e,g]

>>> fmap (index $ chord g majorMinorSeventhChord)) [-2,-1,0,1]
[d,f,g,b]
```

### Transforming chords

Naturally, @[Scale] and @[Chord] are instances of `Transposable`:

```music+haskell
inspectableToMusic @[Scale Pitch] $

[         scale c phrygian
, up m3 $ scale c phrygian
]
```

```music+haskell
inspectableToMusic @[Chord Pitch] $

[                     chord c majorTriad
, invertDiatonic c  $ chord c majorTriad
, invertDiatonic gb $ chord c halfDiminishedChord
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



This example shows the inversion of various chords. The inversion of a major triad is a minor triad, the inversion of a dominant 7th chord is half-diminished chord, and a minor seventh chord is its own inversion.

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

### Custom scales and chords

While Music Suite predefines all common practice modes, scales and chords, we can also create custom chords from any interval sequence.

Here is a mode that repeat at the fourth:

```music+haskell
inspectableToMusic @[ChordType Interval Pitch] $

[ Mode [_M2,_M2,m3]
]
```


<!--
### Non-repeating scales/chords

Non-repeating/self-repeating scales (e.g. the overtone series). TODO create by unfold?
-->


### Modal inversions

The @[chord] function converts a mode into a scale/chord in root position.

```music+haskell
inspectableToMusic @[Chord Pitch] $
[ chord c majorTriad
, chord g majorMinorSeventhChord
, chord c majorTriad
]
```

For chord inversions in the usual sense, see [the next section](#voicings).

<!--
TODO `chord` gives you the root position, define a version of `chord` that gives you 1st, 2nd, 3rd inversion etc. For example 4th inversion of a ninth chord

TODO this is a rotation, what does it mean:

```music+haskell
inspectableToMusic @[Chord Pitch] $
[ chord c $ majorTriad
, chord g $ invertChord (-1) majorMinorSeventhChord
, chord c $ invertChord 2    majorTriad
]
```
-->


<!--
### Set operations

TODO set operations on chords/scales (e.g. union/difference/intersection/isSubset/isPowerset etc).
-->



## Voicings

Recall that chords are infinite sets. A @[Voicing] is a finite subset of that set. For a normal (octave-repeating) chord, it defines what pitches appear and in what octave.

### Close voicing

The `voiced` function voices a chord as closely as possible above the tonic. Formally the pitches of the generating interval sequence, originating at the tonic. For example:

```music+haskell
inspectableToMusic @(Voiced Chord Pitch) $
  voiced (chord d majorTriad)
```

To generate a closed voicing with doubled notes, use `voiceIn`.

```music+haskell
inspectableToMusic @[Voiced Chord Pitch] $
[ voiceIn 4 $ chord c majorTriad
, invertVoicing (-2) $ voiced $ chord g majorMinorSeventhChord
, voiceIn 4 $ chord c majorTriad
]
```

### Operations on voicings

We extract the pitches from a voiced chord like this:

```music+haskell
pseq $ fmap fromPitch ps
  where
    ps :: [Pitch]
    ps = Data.List.NonEmpty.toList $ getVoiced v

    v :: Voiced Chord Pitch
    v = voiceIn 4 $ chord c majorTriad
```

Voiced chords allow inversion:

```music+haskell
inspectableToMusic @[Voiced Chord Pitch] $
  fmap (`invertVoicing` vs) [ -1..4 ]
  where
    vs = voiced (chord c majorTriad)
```

### Other voicings

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




<!--
## Consonance and dissonance

TODO relative dissonance of intervals, modes and chords

TODO resolution and leading notes. "Solve" an n-part voicing problem

Calculate dissonance of a chord (classical/"objective", by higest common fundamental)
-->



<!--
# Absolute pitch

In the previous chapters we worked exclusivey with Pitch/Interval. These restrict
us to the Western/classical set of pitches and are relative (assuming, but not implying any particular tuning system). In this chapter we we will let go of these restrictions and look into working with both absolute pitch (arbitrary frequencies) and *alternative* pitch systems.

We will also see how *tuning systems* relate structured pitch representations (such as `Pitch`) to unstructured ones (such as `Hertz`).

## Tuning and intonation

### Absolute pitch

@[Hertz]

Logarithmic scales:

@[Fifths]
@[Cents]

## Alternative pitch representations

### Equal tempered scales

TODO equal tempered scales of any size

TODO 24TET ("quarter tones")

## Beyond diatonic/chromatic

TODO the Common.Pitch type has built-in support for chromatic/diatonic harmony. We can construct types that support other system instead.
-->




<!--
## Spectral music

TODO Working "backwards" from absolute to relative pitch

TODO spectral dissonance using HCF
(https://harmonicratio.blogspot.com/2018/10/pursuing-clarity-through-openness-part_80.html)
-->
























# Articulation and dynamics

## Adding dynamics

Dynamics can me applied using @[level]:

```music+haskell
level ppp c
```

Here is an overview of the standard dynamic values:

```music+haskell
over eras (stretchRelativeOnset 0.5) $ pseq $ zipWith level [fff,ff,_f,mf,mp,_p,pp,ppp] (fmap fromPitch [c..])
```

We can give any two dynamic values to `cresc` and `dim` (e.g. they are synonyms). A crescendo/diminuendo line will be drawn as necessary.

```music+haskell
(cresc pp mf $ pseq [c,d,e,f,g,a,b,c'] |/8)
  </>
(dim fff ff $ pseq [c,d,e,f,g,a,b,c'] |/8)
```

Long crescendos and diminuendos are supported as well.

```music+haskell
cresc pp mf $ (times 8 $ pseq [c,d,e,f,g]) |/8
```

### How dynamics are represented

It is important to understand that dynamics are not stored as *marks and
lines*, but rather as values attached to each note. This means you can freely
split and merge without having to worry about dynamics.

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

## Adding articulations

### Staccato and legato

Standard articulations are supported:

```music+haskell
legato (pseq [c,d,e,f,g]|/8)
    </>
staccato (pseq [c,d,e,f,g]|/8)
    </>
portato (pseq [c,d,e,f,g]|/8)
    </>
tenuto (pseq [c,d,e,f,g]|/8)
    </>
staccatissimo (pseq [c,d,e,f,g]|/8)
```

### Accents

Adding accents is similar to regular articulations:

```music+haskell
accent (pseq [c,d,e,f,g]|/8)
    </>
marcato (pseq [c,d,e,f,g]|/8)
```

One difference is that by default, accents are only applied to the first note in each phrase. We can also explicitly specify the last note, or all the notes:

```music+haskell
accentLast (pseq [c,d,e,f,g]|/8)
    </>
accentAll (pseq [c,d,e,f,g]|/8)
```

### Articulations and phrases

We can apply slurs and articulation marks to scores of arbitrary complexity. The library will traverse each phrase in the score and apply the articulations separately.

For example in this example we're building up a score consisting of three parts and then apply `accent . legato`:

```music+haskell
let
    ps = fmap fromPitch [c..c']
    p1 = pseq ps |/4
    p2 = delay (1/4) $ pseq ps |/4
    p3 = delay (3/4) $ pseq ps |/4
in (accent . legato) (p1 </> p2 </> p3)
```

These kind of traversals are not limited to articulation. See [Phrase traversals](#phrase-traversals) for a more general overview.

<!--
## Overloading of articulation and dynamics

Dynamic values are overloaded in the same way as pitches. The dynamic literals have type `IsDynamics a => a`.

TODO explain overloading of articulation
-->

## More examples

A note with default articulation and dynamics:

```music+haskell
c
```

Setting dynamics:

```music+haskell
set dynamics' pp c
```

Setting the accentuation and separation components of articulation:

```music+haskell
set articulations' (accentuation +~ 2 $ mempty) c
```

```music+haskell
over (articulations' . separation) (+ 2) c
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

A @[Subpart] is a list of *divisions*. For example in the subpart *Violin I.1.II* the instrument is *violin* and the *subpart* is *I.1.II*.

Subparts are always non-empty lists. A consequence of this is that parts always have at least one subdivision: "Violin" is not a part, but "Violin I" is. When there's only one part per instrument, the subpart is hidden by default.

<!--
TODO Understand "overlapping" semantics, e.g. if notes overlap in "I" and "I.2" we have "overlapping events" (not OK in monophonic instruments, but see solo/altri below)
-->

<!--
TODO show how to set explicitly VI.1, VI.2, VII etc.
-->

## Partwise composition

We have already seen how the `</>` operator can be used to compose music "partwise". Now that we know about subparts we can see this works:

When the given expressions have overlapping notes in some part, the subpart is incremented:

```music+haskell
c </> c
```

When this is not the case, `</>` behaves like `<>`:

```music+haskell
set parts' violins c </> set parts' violas c
```

The subpart of the left side is never changed, and the right side is always assigned to the next available subpart:

```music+haskell
set parts' p c </> set parts' p c
  where
    p = set subpart 2 $ violins
```

Note that as a consequence of this `</>` is not associative. Compare:

```music+haskell
c </> (e </> g)
```

versus

```music+haskell
(c </> e) </> g
```

This is normally not a problem, as `</>` associates to the left by default. Similarly with `rcat`:

```music+haskell
rcat [c,e,g]
```


## Staves and parts

It is important to understand the difference between *parts* and *staves*. While parts have a clear semantics in terms of perfomance, staves are a way of presenting this information visually. There is usually no need to worry about staves, they are automatically created depending on the parts present in the score.

```music+haskell
set parts' flutes c
```

Most instruments are drawn on a single staff. Certain instruments are drawn on multiple staves by default, however:

```music+haskell
set parts' (tutti celesta) c
```

## Updating several parts at once

An *ensemble type* can be represented as a list of parts. We provide a few pre-defined ones such as string quartet, chamber orchestra, etc. We  can also define a custom ensemble type:

```haskell
someEnsemble = divide 2 violins ++ [trumpet, clarinet]
```

We can update several parts at once using the @[arrangeFor] function. This is useful in combination with @[rcat]:

```music+haskell
arrangeFor stringOrchestra $ rcat [c',e,g_,c_]
```

## Soloists

Each part has a @[solo] component, which is either `Solo` or `Tutti`. This is useful when working with concertante scores.

```music+haskell
(parts' .~ solo violin $ pseq [c,d,e,f,g,a,g,e,ds,e,cs,d,b,bb,a,ab] |/ 16)
  <>
arrangeFor stringOrchestra (pseq [rcat [c',e,g_,c_]])
```

The default value is `Tutti`. In chamber music there is usually no need to override this with `Solo`, as the difference only make sense when you *need* to distinguish the solist.

<!--
TODO soloists *from* the orchestra/altri
-->

## Extracting parts

We can also *extract parts* from a score. Note that if you're working with an external score writing program there is usually no need to do this explicitly in Music Suite: just export the entire score and use the part extraction mechanism in your editing application.

If you *need* to perform part extraction in Music Suite, this is easy:

```music+haskell
extractPart violas fullScore
  where
    fullScore =
      (parts' .~ solo violin $ pseq [c,d,e,f,g,a,g,e,ds,e,cs,d,b,bb,a,ab] |/ 16)
        <>
      arrangeFor stringOrchestra (pseq [rcat [c',e,g_,c_]])
```

## More about instruments

We support all instruments in the MusicXML sound set. See [the full list here](https://www.musicxml.com/for-developers/standard-sounds/).

> Note: If you are working with the MIDI backend, only the General MIDI sound set is supported.

### Transposing instruments

We can obtain transposition infomation from instruments:

```music+haskell
inspectableToMusic @[Interval] $

[ transposition violin
, transposition clarinet
, transposition doubleBass
]
```


> Note: instruments that appear in many sizes are different instruments in Music Suite. The most common type of trumpet is Bb, so `trumpet` refers to this. For other instruments use `trumpetInC`, etc.


### Range

We can obtainin range information from instruments:

```music+haskell
inspectableToMusic @[Ambitus Interval Pitch] $

[ playableRange violin
, comfortableRange violin
]
```



# Playing techniques

All instruments come with a variety of playing techniques, many of which produce fundamentally different sound types. We treat playing technique as a separate aspect from part and pitch.


## Non-instrument specific techniques

### Tremolo, trills and rolls

A regular (measured) tremolo can be notated using the @[tremolo] function. Regular tremolo is is a shorthand for rapid iteration of a single note.


```music+haskell
tremolo 2 $ times 2 $ (c |> d)|/2
```

An unmeasured tremolo is notated using @[fastTremolo]. Unmeasured tremolo means "play individually, as fast as possible" and is a coloristic effet rather than a rhythmical shorthand.

Note that in keeping with traditional notation, we notate unmeasured tremolo using three beans.

```TODOmusic+haskell
fastTremolo $ times 2 $ (c |> d)|/2
```

### Repeating vs. alternating tremolo

The former is rare but happen e.g. when double-stopped strings play bow tremolo (without bariolage). The more common one is a rapid alteration among a set of notes. Logically we should treat both as an optional the property of a single chord. Alas in standard notation the latter is commonly written as two chords with half the duration (or ins ome cases as a trill).

### Slide and glissando

```music+haskell
glissando $ pseq [c,d]|/2
```

### Harmonics

Use the @[harmonic] function. The argument is the harmonic number, with zero being the fundamental, one the first overtone, and so on. Use sounding pitch, Music Suite will automatically figure out the correct notation.

```music+haskell
(harmonic 1 $ c|/2)
    </>
(harmonic 2 $ c|/2)
    </>
(harmonic 3 $ c|/2)
```

For artificial harmonics, use @[artificial]:

```music+haskell
artificial c |/ 2
```

## Instrument-specific techniques

### String techniques


```music+haskell
set parts' violins $
  pseq [arco $ staccato $ times 4 c, times 4 $ pizz g_ ] |/ 4
```

```music+haskell
set parts' violins $
  pseq [pizz $ pseq [c,c,c,c], d |* 2, pizz e |*2 ] |/ 4
```


TODO bow position (sul tasto, sul pont, nat)

```music+haskell
set parts' violins $
  pseq [sulTasto $ pseq [c,c,c,c], posNat d |* 2] |/ 4
```

```music+haskell
set parts' violins $
  pseq [posNat $ pseq [c,c,c,c], sulPont d |* 2] |/ 4
```

```music+haskell
set parts' violins $ pseq
  [ colLegno c
  , colLegnoBatt c
  , senzaLegno c
  ] |* 2
```

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

<!--
TODO chord tremolo
-->


```music+haskell
set parts' violins $ pseq
  [ conSord c
  , senzaSord c
  ]
  |/ 4
```

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

<!--
TODO fingering, multiphonics

TODO key sounds, percussive attacks ("pizz"), haromonics/whistle tones
-->

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

<!-- TODO alternative mutes -->

<!-- TODO hand stopping -->

## Percussion

Working with percussion is much like working with normal instruments. The main difference of course is that:

- Some percussion instruments no notion of pitch, or a limited set of pitches they can play.

- Percussion players tend to double on many different types of instruments than other musicians.

We currently do not ruling out entering pitches for e.g. snare drum parts. However backends will ignore the pitch information, and the music will render on a single-line staff.

As with other instruments we currently can not represent players doubling on multiple instrumentsexplicitly. You will have to manually enter the music in different parts and manually assure that there is no overlap in parts meant to be executed by the same performer.

> Note: for percussion we break the singular/plural naming convention and export a `Part` in the singular form.

The solo/tutti component is set to `Tutti` by default even though there might only be one performer in the group (the distinction would still make sense e.g. in a percussion concerto).

```music+haskell
parts' .~ snareDrum $ (`stretch` c) <$> rh [1,rh [1,1,1],1,1]
  where
    rh = stretchTo 1 . pseq
```

For rolls see [the previous section](tremolo-trills-and-rolls).



<!--
# Lyrics and Vocals

TODO adding lyrics (including syllables/word boundaries/melismas)

TODO soloists/character name
-->

<!--


# Non-note Actions

While most music notation is conerned with making sound, a score may call for events which are not meant to directly produce sound. We represent these things using special events called *actions*. Like with percussion actions have no pitch.

TODO representation? Some kind of sum type in the note stack?

## Piano/Vibraphone pedalling

TODO

## Instrument change warnings

TODO

## Cues

TODO
-->


<!--
# Text and Color

> Warning: A core idea in Music Suite is that music expressions have clear *semantics*, based on how the sound or action they represent. Free text runs counter to this, and should be viewed as an "escape hatch". Try to use a more structured representation when possible.


@[text]

```music+haskell
text "pizz." $ c|/2
```

TODO e.g. expressive marks ("dolce")

TODO color
-->































# Meta-information

Meta-information is global, rather than attached to a specific part. It is *defined at every point in the score with explicit change points (per type)* and always has a sensible default value (e.g. one (Reactive m) per type). All meta types are monoidal. Examples: key signature, time signature.

Meta-information is always *optional*. There is always a sensible default value which can be overriden either globally or locally (i.e. during some specific time-span).

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
keySignature (key db MajorMode) $ pseq [db,eb,f]
```

We can also set the key signature for a specific time span using @[keySignatureDuring].

```music+haskell
keySignatureDuring (1 <-> 2) (key db MinorMode) $ pseq [db,eb,f]
```

A key signature change will always force a new bar.

```music+haskell
keySignatureDuring (1.5 <-> 2) (key db MajorMode) $ pseq [db,eb,f]
```

Part-specific key signatures are not supported, but transposing instruments will always use the correct relative key signature.

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

We can also set the time signature for a specific time span using  @[timeSignatureDuring]. Time signature changes will always force a new bar.


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
(tempo allegro $ pseq [c,d,e,f,g] |/ 4 )
```

Tempo changes will always force a new bar.

### Fermatas, caesuras and breathing marks

Fermatas indicate a certain time point (usually a strong beat) should be prolonged.

<!--
TODO representation should be: meta-mark at the first strong beat *after* the fermata-signed notes (e.g. the one to break *before*). This means we can render fermata signs on all notes where whose span overlaps the break point (including offset, not including onset).
-->

```music+haskell
fermata StandardFermata (ppar [c,e,g])
```

Note that a fermata attaches to a specific point known as the *sustain point* (the beginning of the given score is used by default). All notes overlapping the sustain point have a fermata drawn on them.

```music+haskell
fermata StandardFermata (ppar [pseq[c,d] |/ 2,e,g])
```

<!--
A fermata usually implies a unison cutoff of the prolonged notes, followed by a short break before continouing to the next beat. This can be made explicit by addng caesuras or breathing marks (commas).
-->

<!--
### Ritardando and accellerando

```TODOmusic+haskell
(rit (pseq [c,d] |> e |* 2) |/ 4)
```

```TODOmusic+haskell
(acc (pseq [c,d] |> e |* 2) |/ 4)
```
-->

## Barlines and repeats

There is generally no need to enter bars explicitly, as this information can be inferred from other meta-information. Generally, the following meta-events (in any part), will force a change of bar:

* Key signature changes
* Time signature changes
* Tempo changes
* Rehearsal marks


Whenever a bar line is created as a result of a meta-event, an shorted time signature may need to be inserted before the change. For example here the change of time signature to 3/4 forces the insertion of a 2/4 bar.

```music+haskell
compress 4 $ timeSignature (4/4) (pseq [c,d,e,c,d,e,f,d,g,d]) |> timeSignature (3/4) (pseq [a,g,f,g,f,e])
```

We can force a new bar lines using @[barline].

```music+haskell
compress 4 $ pseq [c,d,e] |> barline DoubleBarline (pseq [d,e,f])
```

## Clefs

The standard for each instrument is used by default. There is currently no way of overriding it.

<!--
## Multi-movement scores

TODO
-->

## Rehearsal marks

Rehearsal marks are added to the beginning of the score by default:

```music+haskell
rehearsalMark $ pseq [c,d,e,d,f,e,d,c] |/ 3
```

We can also add it to a specific position:

```music+haskell
rehearsalMarkAt 2 $ pseq [c,d,e,d,f,e,d,c] |/3
```

A rehearsal mark carry no specific meaning. Composing two scores will interleave their rehearsal marks.

```music+haskell
rehearsalMarkAt 1 (up m3 m) </> rehearsalMarkAt 2 m
  where
    m = pseq [c,d,e,c,d,f] |/ 2
```

Rehearsal marks will always force a new bar.

<!--
## Annotations

Annotations are simply textual values attached to a specific section of the score. In contrast to other types of meta-information annotations always apply to the whole score, not to a single part. To annotate a score use @[annotate], to annotate a specific span, use @[annotateSpan].

Annotations are *invisible by default*. To show annotations in the generated output, use
@[showAnnotations].

```TODOmusic+haskell
showAnnotations $ annotate "First note" c |> d |> annotate "Last note" d
```
-->

<!--
## Custom meta-information

TODO works for any `Typeable` `Monoid`.

TODO Use more specicif wrappers to preserve `Transformable`


@[HasMeta]

@[setMetaAttr]

@[setMetaTAttr]
-->


































# Time, rhythm and form

## Basic time types

### Time and Duration

Time points and vectors are represented by two types @[Time] and @[Duration]. The difference between these types is similar to the distinction between points and vectors in ordinary geometry. One way of thinking about time vs. duration is that duration are always *relative* (i.e. the duration between the start of two notes), while *time* is absolute.

Time points form an affine space over durations, so we can use the operators @[.+^] and @[.-.] to convert between the two.

```haskell
>>> 2 :: Time
2

>>> (2 :: Time) .+^ (3 :: Duration)
5 :: Time
```

### Time spans

The @[Span] type represents a non-empty *slice* of time. We can represent spans in exactly three ways: as two points representing *onset* and *offset*, as one point representing *onset* and a duration, or alternatively as a point representing *offset* and a duration.

```haskell
>>> (2 <-> 3)^.onset
2

>>> (2 <-> 3)^.offset
3

>>> (2 <-> 3)^.duration
3

```

We can also enter a span using either its *onset and offset*, its *onset and duration*, or its *duration and offset*. The three literals are equivalent:

```haskell
>>> 2 <-> 3
(2 <-> 3) :: Span

>>> 2 >-> 1
(2 <-> 3) :: Span

>>> 1 <-< 3
(2 <-> 3) :: Span
```

To convert between these representations, we can use @[onsetAndOffset], @[onsetAndDuration] and @[durationAndOffset].

### Spans as transformations

Here is an alternative view of span: as an *affine transformation*.

A span `a >-> b` represents the act of *stretching by b* followed by *delaying by a*. Spans form a group using *composition of transformations*. The identity transformation is `0 >-> 1` (scaling a by one and delaying by zero).

<!--
For those familiar with linear algebra or computer graphics: Because time is one-dimensional a *linear transformation matrix* in time is a 1x1 matrix (e.g. a scalar). Its *affine transformation matrix* is a 2x2 matrix. We can understand the monoid instance for @[Span] as multiplication of 2x2 matrices.
-->

```haskell
>>> mempty :: Span
(0 <-> 1)

>>> negateV (0 <-> 2)
(0 <-> 0.5)

>>> negateV (2 <-> 1)
(0.5 >-> 1)

>>> (0 >-> 3) <> (2 >-> 1)
(2 >-> 3)
```

TODO examples of transforming points

### The Transformable class

The @[Transformable] class represent all things that can be transformed. All instance satisfy the following laws:

- `transform mempty x = x`, e.g. applying the empty transformation changes nothing.
- `transform (s <> t) x = transform s (transform t) x`, e.g. applying a composition of transfomrations is equivalent to applying them all in sequence.
- `transform (s <> negateV s) x = x`, e.g. each transformation has an inverse.

Formally `transform @a` is a left group action on some transformable type `a`. Intuitively, transforming  a value is equivalent to transforming *all the points in the value*.

We have already seen how classical counterpoint and serial operations can be formoulated as transformations. For example *augmentation*, *diminishion* and *phasing* can be accomplished with `stretch`, `compress` and `delay`:

```music+haskell
delay 1 (stretch 2 c)
  </>
transform (1 >-> 2) c
  </>
transform (1 <-> 3) c
```

Stretching by `(-1)` is the *retrograde* operation:

```music+haskell
stretch (-1) $ pseq [c,d,e]
```

### Translation-invariant types

We can think of our time types as coming in two shapes:

- Translation-invariant types such as `Duration` are "floating" without being anchored to specific start/stop time (though they still have a duration)
- Translation-variant types such as `Span` have both a specific duration and a specific point in which they "occur" relative to ther events.

Note that this does *not* invalidate the laws.

### Spans as time intervals

The @[TimeInterval] type is similar to @[Span], but also allows for empty spans to be represented. It forms a monoid with the convex @[hull] operator.



## Position and duration

The @[HasDuration] class represents values that have a duration. The most obvious example is `Duration` itself:

```haskell
>>> _duration (2 :: Duration)
2
```

There are also instances for `Span` and, as we will see, most other time-based types:

```haskell
>>> _duration (1 <-> 3)
2
```

The @[HasPosition] class represent values that have an *absolute position* in time. The simplest example is `Span`:

```haskell
>>> _era (1 <-> 3)
Just (1 <-> 3)
```

Here `Maybe` is used to represent the "empty span". The class `HasPosition1` refines `HasPosition` by explicitly disallowing the empty span:

```haskell
>>> _era1 (1 <-> 3)
1 <-> 3
```

Values with a position allow many useful combinators to be defined. For example here we use `during` to add a pedal note underneath a melody:

```music+haskell
let
    melody = legato $ pseq [pseq [c,d,e,c], pseq [e,f], g|*2]
    pedal = c `during` melody
in compress 4 $ melody </> pedal
```

<!--
TODO example with stretchRelative, stretchTo
-->

The laws for @[HasPosition] and @[HasPosition1] are not too exciting: they assure that transforming a value also transforms its position in the same manner, and that the duration of a value is exactly the duration between its onset and offset point.




## Times with values

The Note and Event types are similar to Duration, Time and Span respectively, except they also contain a *payload* of an arbitrary type. This is expressed as a type parameter (often written using a lowercase letter, as in `Note a`).  In practice the payload will usually contain (possibly overloaded) *aspects* such as part, pitch, dynamics and so on.

A @[Note] represents a single value tagged with a *duration*:

```music+haskell
inspectableToMusic @(Note Pitch) $

c
```

An @[Event] represents a single value tagged with a *time span*:

```music+haskell
inspectableToMusic @(Event Pitch) $

c
```

Note that we can enter a single note or event as `c`, `d`, `eb`, etc. because of [pitch overloading](#pitch-overloading).

Notes and events are very similar. The main difference is that notes are translation-invariant and events are not.

```music+haskell
inspectableToMusic @(Note Pitch) $

delay 1 $ stretch 0.5 c
```

```music+haskell
inspectableToMusic @(Event Pitch) $
delay 1 $ stretch 0.5 c
```

Similarly, `Note` is an instance of `HasDuration`, but not `HasPosition`. Events have both duration and position:

```haskell
>>> (c :: Note)^.duration
1

>>> (c :: Event)^.duration
1

>>> (c :: Event)^.era
0 <-> 1
```

Notes and events have instances for `Functor`, `Foldable` and `Traversable`, which all visit the single element the contain. There are also instances for `HasPitches`, `HasDynamics` and so on, meaning that we can use most combinators from the previous chapters on notes and events.

```music+haskell
inspectableToMusic @(Note StandardNote) $

set parts' violins $ pizz $ level ff $ accentAll $ compress 4 c
```

You may wonder, what is the point of representing music with *just a single note*? The answer is that these types are used in combinations with other types. For example we can use `Note [Pitch]` to represent a melody where all notes have the same duration.




## Voices

A @[Voice] represents a *sequential composition of values*, each tagged with *duration*.

### Creating voices

TODO

```music+haskell
inspectableToMusic @(Voice Pitch) $

[cs, bb, a |* 2] |/ 4
```


As with notes and events, we can enter a single voice using `c`, `d`, `eb`, etc. because of [pitch overloading](#pitch-overloading).

You can put a rest in a voice. Notice how this changes the type:

```music+haskell
inspectableToMusic @(Voice (Maybe Pitch)) $

(1/4) *| [c |* 2, rest, e]
```

This works because of [pitch overloading](TODO link).

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

TODO create from rhythm

TODO create from list of notes

TODO create from IsPitch

TODO create using (^.voice)

### Traversing voices

TODO

### Transforming voices

TODO Transformable, HasDuration

Voices do not have a position, i.e. they are translation-invariant. In order to anchor a voice at a specific point in time use [`Aligned`](#alignment).

TODO rotation

TODO fusion and stretch

TODO take/drop

TODO filtering (MonadPlus!)

### Combining voices

TODO Monoid, Monad, Applicative, MonadZip + guard

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



```music+haskell
inspectableToMusic @(Voice [StandardNote]) $

[ dynamics' .~ d $ p
  | p <- [c, ab, fs, g]
  | d <- [ppp, ff, mp, mf]
  ]
```

```music+haskell
inspectableToMusic @(Voice [Pitch]) $

[ [x,y] | x <- view voice (fmap fromPitch $ enumChromaticFromTo c c''), y <- [d,e]
  , isMelodicConsonance (x .-. y) && isConsonance (x .-. y) ]
```



## Alignment

The @[Aligned] type adds position to anything with a duration. This is akin to alignment in computer graphis, hence the name. Alignment works by picking:

- A time point to which the value is "anchored". By default this is time zero.
- An alignment point in the duration of the value. By default this is the onset of the value.

Aligned is natural way of modelling pickups and upbeats. Consider this melody:

```music+haskell
(pseq [g_,a_,b_]|/2 |> pseq [c, c, d, d]) |/ 4
```

With @[Aligned] we can represent the fact that the first three notes are "upbeat" notes, and that the main stress of the value should fall on the fourth note:

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

The `||>` operator is similar to the normal sequential composition operator `|>`, but aligns the result to the point of composition.


<!--
TODO align and realign
-->

<!--
TODO sequential composition of aligned voices "snap to next stressed beat":
`snapTo :: (HasPosition a, Transformable a) => Stream Time -> [a] -> [a]`
-->


## Scores

A @[Score] represents a *parallel composition of values*, each tagged with *time span*.


### Creating scores

Using IsPitch
Using pure
Composition
Monad comprehensions
From a list of events

### Traversing scores
Score is Traversable, HasPitches etc

Special traversals such as mapWithSpan

Filtering with MonadPlus

### Transforming scores
Transformable, HasPosition

### Nested scores
join

### Overlapping events
TODO

### Representing rests
An empty scores has no duration, but we can represent rests using `Score (Maybe a)`.

```music+haskell
pseq [c,rest,d] |/ 4
```




## Patterns

A @[Pattern] can be throught of as a generalization of a *rhythm* or *beat*. They are similar to scores, but are infinite. Each pattern is created by repeating a number of layers. Every pattern will repeat itself (though the repeating duration may be long).

### Creating patterns

The basic way of buildings patterns are @[newPattern] and @[rhythmPattern].

TODO example

### Composing patterns

We can compose patterns in parallel using the regular composition operator @[<>].

```music+haskell
fmap Just $ renderPattern (a <> b) (0 <-> 4)
  where
    a = parts' .~ mempty $ rhythmPattern [3,3,4,2,4] |/ 8
    b = parts' .~ flutes $ rhythmPattern [1] |/ 8
    -- TODO use claves, maracas here
```

As patterns are infinite, we can compose patterns of different durations. Both patterns will just be repeated indefinately.

```music+haskell
fmap Just $ renderPattern (a <> b) (0 <-> 2)
  where
    a = parts' .~ trumpets  $ newPattern [c,d] |/ 8
    b = parts' .~ trombones $ newPattern [c,d,e] |/ 8
```

### Transforming patterns

Patterns are @[Transformable], @[Transposable], @[Attenuable] and so on, so many expressions that work for scores and voices also work for patterns. For example we can set parts and dynamics, or transpose patterns.

```music+haskell
fmap Just $ renderPattern (a <> b) (0.5 <-> 1.5)
  where
    a = parts' .~ mempty $ rhythmPattern [3,3,4,2,4] |/ 8
    b = parts' .~ flutes $ rhythmPattern [1] |/ 8
```

```music+haskell
fmap Just $ renderPattern (stretch 0.5 $ up m3 $ a <> b) (0 <-> 2)
  where
    a = parts' .~ mempty $ rhythmPattern [3,3,4,2,4] |/ 8
    b = parts' .~ flutes $ rhythmPattern [1] |/ 8
```


```music+haskell
fmap Just $ renderPattern (a <> b) (0 <-> 2)
  where
    a = parts' .~ trumpets  $ newPattern [c,d,e] |* (3/15)
    b = parts' .~ trombones $ newPattern [c,d,e] |* (3/8)
```

You can adjust the "phase" of a pattern using @[delay]. This is useful together with the composition operator:

```music+haskell
fmap Just $ renderPattern (a <> b <> delay (1/4) c <> delay (1/4) d) (0 <-> 2)
  where
    a = parts' .~ flutes    $ rhythmPattern [1/2,1/2]
    b = parts' .~ oboes     $ rhythmPattern [1,1/2,1/2]
    c = parts' .~ trumpets  $ rhythmPattern [1/2,1/2]
    d = parts' .~ trombones $ rhythmPattern [1,1/2,1/2]
```

The @[renderPattern] function returns the events of the pattern within a given time span.

<!--
TODO finish/move to examples:

```TODOmusic+haskell
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


    bachCMajPattern :: (Num a) => Pattern a
    bachCMajPattern = newPattern $ stretchTo 1 $ (view voice) $ fmap pure [0,1,2,3,4,2,3,4]
```
-->

## Time-varying values

The structures we have been dealing with so far are all discrete, capturing some (potentially infinite) set of *time points* or *notes*. We will now look at an alternative time structure where this is not necessarily the case. @[Behavior] represents a *time-varying values*, or functions of time.

TODO example

Behaviours are continous, which implies that:

- They are defined at *any point in time*. A behavior always has a value, unlike, e.g. aligned boices.

- They can change at infinitely small intervals. Just like with vector graphics, behaviours allow us to zoom in arbitrarily close, and potentially discover new changes. Thus it is (in general) nonsensical to talk about *when* a behaviour changes.

A @[Reactive] is a discrete behabior, e.g. a time-varing value that can only change at certain well-known locations. A @[Reactibe] value is similar to a @[Voice], but stretches out indefinately in both directions.

TODO exampels of Reactives

### Constant values
### Switching
### Predefined behaviors
### Transforming Behaviors and Reactives

IsPitch/Transposable
Transformable
Functor
Applicative
Monad

### Conversions
#### Reactive to Behavior
#### Behavior to Reactive
#### Score to Behavior
<!--
TODO viewing a score as a Behavior (concatB). Useful for "vertical slice view" of harmony, as in https://web.mit.edu/music21/doc/usersGuide/usersGuide_09_chordify.html
-->


<!--
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

-->




## Building larger musical structures

Let's now look at how to use the types and classes introduced in this chapter to organize larger musical forms.

### Basic repetition

@[times]

```music+haskell
let
    melody = accent $ pseq [c,d,e]|/16
in times 4 $ melody
```

<!--
- variation

- Basic repeatition: @[times], @[replicate]

- Lambdas and abstracting out common patterns

- Infinite streams, take, drop, generative music

- "Indexed loops", zips, zipWith [0..]

- "Stateful" loops, `for`, `traverse` (state monad example)

- Randomness

- Logic programming
  - Predicates
  - Searching (MonadLogic)

- Probabilistic programming
  - Like logic programming, but not just "yes" or "no"
  - E.g. we can express "it is better if": 1st species counterpoint example
-->



# Traversals

In previous chapters have focused on *composing* musical expressions. In this chapter we will look at various ways of *analyzing* and *transforming* musical expressions. The most important tool for this in Music Suite is called a *traversal*.

Traverals are a subtle and powerful concept. The basic ideas is simple: given some traverable "container" value, we have a way of visiting all of its element in some specific order. We can exploit this to:

- Accumulate computations over all the elements
- Searching and querying the elements
- Update the elements one at a time

Of course Haskell is a pure language, so whenever we refer to "change" or "update" in the context of a data structure, we are actually creating new structures on the fly.

The most common traversal is known as `traverse`, and is defined for all types that are `Traversable`. The type signature of `traverse` is highly general:

```haskell
traverse ::
  (Traversable t, Applicative f) =>
  (a -> f b) ->
  t a -> f (t b)
```

This is easier to understand if we specify some of the type variables:

```haskell
traverse ::
  Applicative f =>
  (Bool -> f Bool) ->
  [Bool] -> f [Bool]
```

The way to read this is that `traverse` transforms an effectful function operating on `Bool` to operate on `[Bool]` instead.

Here is another example of a traversal:

```haskell
traversePitches ::
  (Pitch -> f Pitch) ->
  Score Pitch -> f (Score Pitch)
```

This means, given a score of pitches and a function operating on pitches, traverse the pitches in the score one by one using the function and return a *new* score containing the transformed pitches.

To make this more readable we can use the following two type synonyms:


```haskell
type Traversal  s t a b = forall f . Applicative f => (a -> f b) -> s -> f t
type Traversal' s a     = forall f . Applicative f => (a -> f a) -> s -> f s
```

We can now write:

```haskell
traverse        :: Traversable t => Traversal (t a) (t b) a b
traverse        :: Traversal' [Bool] Bool
traversePitches :: Traversal' (Score Pitch) Pitch
```


<!--
A traversal that targets exactly one element is known as a *lens*. We've already seen examples of lenses and traversals in the chapters on [dynamics](TODO) and [articulation](TODO) in the form of `.~` (or `set`) operator.

> Note: For those familiar lenses in Haskell: Music Suite defines lenses and traversals compatible with the `lens` and `microlens` packages.
-->

## Using traversals

### Folding and accumulating

@[toListOf]

```haskell
>>> toListOf pitches' (c <> d :: Score Pitch)
[c,d] :: [Pitch]
```

@[anyOf], @[allOf]

```haskell
anyOf pitches' (> c) (b_ <> c :: Score Pitch)
False
```

@[allOf]

### Mapping and setting

@[over]

### Applicative side-effects

@[State]
@[Maybe]
@[Either A]


## Aspect traversals

TODO We have already seen these : pitches, parts, dynamics, articulation, techniques.

Music Suite defines traversals and lenses for pitch, dynamic, articulation, parts and playing technique. If you've been following the previous chapters, you might have seen examples of these already: expressions such as `pitches .~ c`, `dynamics .~ ff` or `over dynamics (+ 1)` make use of traversals to *update* all pitches, dynamics and so on, in a given piece of music.


<!--
### Traversals vs. Lenses (singular vs plural)

TODO
-->

## Polymorphic updates

TODO polymorphic update example (e.g. `Common.Pitch` vs `Hertz`)

TODO explain the type families: Pitch, SetPitch etc.


## Phrase traversals

*Phrase traversals* visit all the *phrases* in a score one at at time. They work in two steps: first they traverse each part in the score separately, and then each *consequtive* sequence of notes inside each part. Notes separated by rests are non-consequently, in other words, phrases are separated by rests.

Here is a phrase traversal applied to a single-part score:

```music+haskell
over (phrases' . Control.Lens._head) (up _P8) $ bar <> delay 1 bar <> delay 2 bar
  where
    bar = pseq [c,c,c] |/ 4
```

This multi-part score is traversed partwise:

```music+haskell
over (phrases' . Control.Lens._head) (up _P8) $ bar </> delay (3/4) bar </> delay (5/8) bar
  where
    bar = pseq [c,c,c] |/ 4
```

Any overlapping notes *within a single part* are ignored by phrase traversals:

```music+haskell
over (phrases' . Control.Lens._head) (up _P8) $
  bar <> delay (1/8) bar
  where
    bar = pseq [c,c,c] |/ 4
```

## Filtered traversals

Filtered traversals operate on the elements selected by another traversals if they match a specific predicate. This is similar to where clauses in SQL:

This example transposes all notes with a duration less than `2`:

```music+haskell
inspectableToMusic @(Voice [StandardNote]) $

over t (up _P8) [d,d,d |* 2,d] |/ 4
  where
    t = notes . each . filtered (\x -> x^.duration < 2)
```


## More examples

### Traversing the notes in a voice

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


### Traversing all the events in a score

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

<!--
# Randomness

TODO Reader monad of seed/Random generator state


# Space

TODO very simple space representation (e.g. Angle), minimal example using Ambisonics?
-->





# Import and Export

## Prelude/Inspectable

The @[Inspectable] class represents types that can be converted into a standard musical representation and exported via an *output backend*. The top-level music expression in a file needs to be inspectable.

In some cases, the generality of the Music Suite library leads to ambiguity when selecting the type of the top-level expression. The `Music` type defined in `Music.Prelude` can be used as a default.

```haskell
main = defaultMain $ inspectableToMusic (c :: Music)
```

You can write the the above to `test.hs` and invoke:

```bash
$ cabal exec runhaskell test.hs
Usage: <executable> -f [xml|ly|mid] -o PATH
```

To select e.g. the Lilypond backend:

```bash
$ cabal exec runhaskell test.hs -- -f ly -o hello.ly
```

<!-- TODO API to select backend rather than CLI -->

## Overview of backends

### MIDI

The MIDI output backend generates type 1 (multi-track) Standard MIDI files Standard MIDI files.

The MIDI format is suitable for generating sound using a software synthesizer such as [TiMidity++](https://en.wikipedia.org/wiki/TiMidity%2B%2B) or [Fluidsynth](https://en.wikipedia.org/wiki/FluidSynth), or for exporting music to a Digital Audio Workstation (DAW) software. It is not suitable for exporting to score writing software (use MusicXML).

### Lilypond

The Lilypond output backend generates Lilypond markup code compatible with Lilypond 2.18.2 or later.

Lilypond is a text-only music type setting program and produces very high-quality scores. The Lilypond backend is suitable if you want to generate an entire score using Music Suite. It is also suitable for small examples and is the backend used for all examples in the Music Suite documentation.

> Note: There is no need to edit the Lilypond code generated by Music Suite and you should not attempt this unless you are an experienced Lilypond user.

### MusicXML

The MusicXML output backend generates XML compatible with the MusicXML 3.0.

MusicXML files can be imported by most music typesetting programs, including
Sibelius, Finale and MuseScore.

The MusicXML backend is suitable if you want to use Music Suite for generating
parts of a score, but perform manual editing on the output result.




<!--
# Tips and tricks

### Lists and streams

TODO use `[]` for finite, `Stream` for infinite


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
-->


# Wall of Shame

TODO this is not documentation, move to some other location. Listing all "bad rendering" examplesas a visual issue tracker

### Quantization

```TODOmusic+haskell
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

This should use nested tuplets:

```music+haskell
pseq [pseq [c,d,e] |* (2/(3)), c, d, e, f] |* (1/(5*4))
```

```music+haskell
pseq [pseq [c,d,e,f,g] |* (4/5), c, d] |* (2/(3*4))
```

```music+haskell
stretch (1/2) $ pseq [c,d,e]|/3 |> f |> g|*2
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




# Acknowledgements

## Contributors

Music Suite was made possible by [all the contributors](https://github.com/music-suite/music-score/graphs/contributors).

## Previous work

Music Suite is indebted to many other previous libraries and computer music environments, particularly [Common Music][common-music], [PWGL][pwgl], [nyquist][nyquist], [music21][music21], [Lilypond][lilypond] and [Abjad][abjad]. Some of the ideas for the quantization algorithms came from [Fomus][fomus].

The work of Paul Hudak and the the Yale Haskell group, including [Haskore][haskore], [Euterpea][euterpea] is a major influence. The  and [temporal-media][temporal-media] package is a similar take on these ideas. The [TidalCycles][tidal] library provided the pattern structure.

The temporal structures, their instances and more general design philosophy comes from Conal Elliott's [Reactive][reactive] (and its predecessors). Brent Yorgey's [Diagrams][diagrams] provided the separation of points and vectors and was another main influence.



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

*Copyright Hans Jacob Hoeglund and others 2012–2020*

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />This documentation is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.
