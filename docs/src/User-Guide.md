
# Tutorial

In this chapter we will learn how to install Music Suite and use it to compose simple pieces.

This tutorial does not require any prerequisite knowledge, apart from basic familiarity with the  [terminal](https://en.wikipedia.org/wiki/Shell_(computing)). You should  know how to create and modify [text files](https://en.wikipedia.org/wiki/Text_file) and run [commands](https://en.wikipedia.org/wiki/Command_(computing)).

We will introduce other programming and music theory concepts as we go along. If you are already familiar with both of these, this chapter should not contain many surprises. We will go more in-depth in  future chapters.

### Other resources

There are serveral free introductions to Music Theory and compositions online:
- [Open Music Theory](http://openmusictheory.com/contents.html)
- [School of Composition](https://www.schoolofcomposition.com)
- [Music Theory on Youtube](https://www.schoolofcomposition.com/music-theory-composition-youtube-channels/)

As Music Suite is based on Haskell, you may also want to look at the following texts, aimed at new Haskell users:
- [Learn You A Haskell](http://learnyouahaskell.com/)
- [What I Wish I Knew Learning Haskell](http://dev.stephendiehl.com/hask/)

## Installing

<!--
TODO Docker or other "easy" install options
-->

### Online editor

TODO

### Windows

TODO

### MacOS

TODO

### Linux

#### Installing from source

We'll need Git and Nix (2.3.2 or later).

```
$ git clone https://github.com/music-suite/music-suite
$ cd music-suite
```

Then follow the instructions in `README.md` to setup the environment and build Music Suite.


## Writing music

Music Suite is based on *expressions*. An expression may represent any piece of music, from a single note to a complex, multi-movement work.

To use Music Suite, you will need to write expressions, which the system will convert into *audio* or *graphics* (or both) on your behalf. We can ask it to do this in a couple of different ways. Choose one that works for you so that you can follow along with the examples in the tutorial.

<!--
Note: While Music Suite was written with Western classical notation in mind and it is not restricted to these formats.
-->


### Using files

Using a text editor, creating a file called `Test.hs` containing the following:

```haskell
import Music.Prelude
main = defaultMain music

music =
  c <> e
```

The first three lines here are standard boilerplate. The last line (`c <> d <> e`) contains the actual music expression. Let's break this down line by line:

1. Import all Music Suite definitions
2. This line indicates that the expression `music` (defined below) is the *main expression* in the file. Unless indicated otherwise, this is the music which will rendered or exported by the file.
4-5. This expression represents the notes `c` and `e` played at the same time.

We can *render* the file like this:

$ cabal exec runhaskell -- Test.hs

Try replacing the definition of `music` to point to a different expression, such as `c |> stretch 2 d`. You can also try copy-pasting other examples from this file.


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

```haskell+music
c
```

This of course represents the note C. Notice that there is more information in the score than what we entered. This is because most musical aspects have a *default value*. We will see how to override them later, but for now we can note:

- The default *octave* is the octave containing "middle C", or *C4* in [scientific pitch notation](https://en.wikipedia.org/wiki/Scientific_pitch_notation).

- The default *duration* is a whole note. Durations are measured in rational numbers: a duration of `1` is a whole note (or semibreve), a duration of `1/2` is a half note (or minim), and so on.

- The default *dynamic value* is *mf* (meaning *mezzo-forte*, "medium loud").

- The default *instrument* is *Piano*.


> Note: Numbers are *overloaded*. It is important to note that the syntax do not convey any type information: `0.25` and `1/4` are completely equivalent. Time values are implemented using arbitrary-precision integers and rational numbers, so you do not have to worry about rounding errors.

By default note have no *accidentals* or *articulation marks*. We will see how to add those later as well.


## Onset, offset and duration

All notes we enter have duration `1` by default. To change this, we use @[stretch] and @[compress]:


```haskell+music
stretch (1/2) c
```

```haskell+music
stretch 2 c
```

```haskell+music
stretch (4+1/2) c
```

> Note: In classical theory *stretch* and *compress* are known as augmentation and diminishion, respectively.


We count positions from the first beat in the first bar, so in 4/4 time, `0` means the first beat, `1/4` (or `0.25`) means the second beat and so on.

All notes start at position `0` by default. We can use use @[delay] to move the onset of notes to the right.

```haskell+music
delay 1 c
```

Negative numbers work too:

```haskell+music
delay (-0.25) $ delay 1 $ c
```

The `|*` and `|/` operators can be used as shorthands for `stretch` and `compress`.

```haskell+music
(c |> d |> e |> c |> d|*2 |> d|*2) |/ 16
```

> Note: The `|>` operator means "sequential composition". We will introduce this [properly](#composition-operators) later on.

## Let and where

The `let ... in` construct introduces a temporary name for an expression.


```haskell+music
let
  x = c |> d
  r = 1/3
in
x </> delay r x
```

The `where` construct is similar, but takes the bindings *after* the expression.

```haskell+music
x </> delay r x
  where
    x = c |> d
    r = 1/3
```

## Octaves and accidentals

We can change octaves and accidentals:

```haskell+music
_8va c
```

```haskell+music
sharpen c
```

> Note: We will lean many more ways of entering pitch in the [next chapter](#pitch).

## Basic dynamics and articulations

We can add articulations and change dynamics:

```haskell+music
accent c
```

```haskell+music
level ppp c
```


We will see more examples of this later on as well.

The `$` operator is pronounced "apply". `f $ g x` is the same as `f (g x)`, so the apply operator is an alternative to writing parentheses:

```haskell+music
staccato $ stretch (1/8) c
```

## Composition operators

So far we have worked with a single note, which is not particularly interesting from a musical point of view. To combine multiple notes into a larger score we need a form of *composition*.

The basic composition operator is @[<>], which combines two pieces of music *simultaneously*. For example, we can combine the expressions `c`, `e` and `g`.

```haskell+music
c <> e <> g
```


Or in sequence using @[|>]:

```haskell+music
c |> d |> e
```

Or partwise using @[</>]:

```haskell+music
c </> e </> g
```

### Shorthands

As a shorthand for `x |> y |> z ..`, we can write @[seq] `[x, y, z, ...]`.

```haskell+music
seq [c,e,g] |* (1/4)
```

For `x <> y <> z ...`, we can write @[par] `[x, y, z, ...]` .

```haskell+music
par [c,e,g] |/ 2
```

For `x </> y </> ...` the syntax is @[rcat] `[x, y, z ...]`.

```haskell+music
rcat [c,e,g] |/ 2
```


## Rests, Tuplets and Ties

There is never any need to explicitly create rests, tuplets or ties in Music Suite. Instead, each note exists in a dedicated time span, which can be inspected and transformed. When we compose music expressions in parallel, all notes are interleaved without affecting their onset or duration.

Notes with the same onset and offset are rendered as chords by default.

```haskell+music
seq [c,d,e,c] <> seq [e,f,g,e] <> seq [g,a,b,g]
```

Or, equivalently:

```haskell+music
par [c,e,g] |> par [d,f,a] |> par [e,g,b] |> par [c,e,g]
```

To prevents notes from being merged into chords we must *explicitly* put them in separate parts. The `</>` and `rcat` combinators is a simple way of doing this.

To create space in our scores we can use `rest`. Rests are empty placeholders which take up space when using sequential composition but do not show up in the final score:

```haskell+music
times 4 (accentAll g|*2 |> rest |> seq [d,d]|/2)|/8
```

Rests can be stretched and delayed just like notes.

Any note that crosses a barline will be notated using ties:

```haskell+music
c |* (9/8) |> d |* (7/8)
```

> Note: To change the position of a barline, see [time signatures](#time-signatures).

Similarly, durations that do not fit into standard note durations are notated using dots or tuplets:

```haskell+music
compress 4 (seq [c |*3, d |* 3, e |* 2]) |> compress 5 (seq [f,e,c,d,e]) |> d
```



## Functions

So far we have written our musical expressions in terms of pre-defined operators and functions. We can also define our own functions.

### Named functions

TODO

### Anonymous functions

TODO

### Pattern matching

TODO

### Function composition

Analogously to composition of music seen above we can compose *functions*.

Here is an example using function composition. The dot operator `.` is used to compose the function `up _P8` (which transpose thes the music up by one octave), `compress 2` and `delay 3`. The composed functions are applied in *left to right order*.

```haskell+music
(up _P8 . compress 2 . delay 3) c
```


## Comments

Comments can be defined as follows:

```haskell
-- This is a single-line comment

{-
 This is
 a multi-line
 comment!
-}
```
They are completely ignored by the interpreter.

## More examples

Of course, the combinators we have seen so far such as `stretch`, `_8va` and so on work on arbitrarily complex scores, not just single notes. We can also nest most function within applications of the composition operators.

```haskell+music
_8va $ seq [c, d]
```

```haskell+music
let
  x = seq [c, d]
in
seq [x, up m3 x]
```

Here is a more complex example using all forms of composition:

```haskell+music
let
  scale = seq [c, d, e, f, g, a, g, f] |/ 8
  triad a = a <> up _M3 a <> up _P5 a
in up _P8 scale </> (triad c) |/2 |> (triad g_) |/2
```


## Types

TODO intro

## Classes and constraints

TODO intro

The most useful type classes come with *laws*, which all instances must satisfy. For example the laws for the `Monoid` class are:

- `mempty <> x = x`
	- The empty element composed with `x` is the same as `x`
- `x <> mempty = x`
	- `x` composed with the empty element is the same as `x`
- `x <> (y <> z) = (x <> y) <> z`
	- The operation is associatibe, e.g. we can rearrange brackets without changing the meaning of the expression.

As an example, we can see that this holds for the list instance:

```haskell
[] ++ [1,2,3] = [1,2,3]

[1,2,3] ++ [] = []

[1] ++ ([2,3] ++ [])
=
[1] ++ [2,3]
=
[1,2,3]
=
[1,2,3] ++ []
=
([1] ++ [2,3]) ++ []
```


### Understanding the composition operator types

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

Here and elsewhere in Music Suite, the convention is to follow standard theoretical
notation, so *minor* and *diminished* intervals are written in lower-case, while *major*
and *perfect* intervals are written in upper-case.

```haskell
(</>) :: (Semigroup a, HasParts a, HasSubpart p, p ~ Part a) => a -> a -> a
```

In fact the `<>` operator is used for almost all forms of composition in Music Suite. The other operators simply peform some kind of manipulation on their values before or after composing.

## Musical aspects

While music is often though to be concerned primarily with time and pitch, Music Suite also allow representation of other "dimensions", including:

- Dynamics: the *loudness* of a sound
- Articulation: the *attack* and *relative length* of a sound
- Instruments, parts and playing techniques, all of which affect *timbre*

We'll sometimes refer to these collectively as *aspects*. Each aspect comes with its own traversal class, which is named after it, `HasPitches`, `HasDynamics`, `HasArticulations`, `HasParts`, `HasTechniques`, and so on.

## Next steps

This concludes the first chapter of the manual. We have seen how to write simple pieces, using melody, harmony and voices. You should now know enough to start working on your own music. You will find many more examples of pieces in the [examples directory](https://github.com/hanshoglund/music-suite/tree/master/examples).

The rest of this manual can be read as a *reference*. We will be looking at musical aspects such as pitch, dynamics, rhythm, form and orchestration in detail. In general these chapters can generally be read in any order.








# Time and Rhythm

In this chapter we will learn how time and rhythm is represented in Music Suite.

## Basic time types

### Time and Duration

Time points and vectors are represented by two types @[Time] and @[Duration]. The difference between these types is similar to the distinction between [points](https://en.wikipedia.org/wiki/Point_(geometry)) and [vectors](https://en.wikipedia.org/wiki/Euclidean_vector) in geometry.  Intuitively, durations is that duration are  *relative*, while *time* is absolute.

We can use the operators @[.+^] and @[.-.] to convert between times and durations. The @[.-.]  operator returns the difference (or distance) between two time points.

```haskell
>>> (2 :: Time) .-. (0 :: Time)
2 :: Duration
```

The `.+^` operator adds a duration to a time point.

```haskell
>>> (2 :: Time) .+^ (3 :: Duration)
5 :: Time
```

Formally, time points form an affine space over durations.

### Relationship with traditional notation

In non-musical applications, time points are defined using clocks and calendars, and durations measured in seconds, minutes, hours and so on. In music, durations are measured in [*note values*](https://en.wikipedia.org/wiki/Note_value). The exact length of a note depends on the tempo, which may be indicated in the score or left to the discretion of the performer.

The following table shows the relationship between the traditional note values and the durations used in Music Suite.

|  Name 1 | Name 2 | Duration  |
|--|--|--|
| Longa | Longa | 4 |
| Breve | Breve | 2 |
| Semibreve | Whole note | 1 |
| Minim | Half note | 1/2 |
| Crotchet | Quarter note | 1/4 |
| Quaver | Eight note | 1/8 |
| Semiquaver | Sixtheenth note | 1/16 |

*Tuplets* indicate the sounding duration of some note is written note value by a multiplier. Normally the multiplier is smaller than one, meaning a note of some value written under a tuplet is *shorter* than its non-tupled counterpart. The following table shows the most common types of tuples.

| Name | Multiplier | Shorthand |
|--|--|--|
| Triplet | 2/3  | 3 |
| Quadruplet | 3/4  | 4 |
| Quintuplet | 4/5  | 5 |
| Sextuplet | 4/6  | 6 |

*Dotted notes* similarly indicate (at least nominally) duration multipliers as per below.

| Number of dots | Multiplier |
|--|--|
| 1 | 2-1/2  |
| 2 | 2-1/4  |
| 3 | 2-1/8  |


Music Suite also allows negative durations, which have no direct correspondance in traditional theory.

Time points in a score are counted in note values, starting from $0$ (the beginning of the piece). For a piece in $4/4$ this corresponds exactly to the number of measures. The first beat of the first measure is represented by $0 :: Time$, the second beat by $1/4 :: Time$ and so on. The first beat of the second measure is represented by $1 :: Time$, the second beat of the second measure by $1 + 1/4 :: Time$, and so on.

### Time spans

The @[Span] type represents a non-empty *slice* of time. We can represent spans as two points representing *onset* and *offset*. Alternatively, we can think of it as one point representing *onset* along with a *duration*, or as a point representing *offset* along with a *duration*. The three representations are equivalent.

The expression `x <-> y` denotes a span with `x` as its onset and `y`  as its offset. We can access onset, offset and duration as follows:

```haskell
>>> (2 <-> 3)^.onset
2

>>> (2 <-> 3)^.offset
3

>>> (2 <-> 3)^.duration
1
```

The expression `x >-> d` denotes a span with onset `x` and duration `d`. Similarly, the notation `d <-< y` means a span *offset* `y` and duration `d`. The following expressions all denote the same span:

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

- `transform mempty x = x`
	- The empty transformation does nothing.
- `transform (s <> t) x = transform s (transform t) x`
	- Applying a composition of two transformations is the same to applying them one at a time.

Formally `transform @a` is a [left group action](https://en.wikipedia.org/wiki/Group_action) on some transformable type `a`. Intuitively, transforming  a value is equivalent to transforming *all the points in the value*.

We have already seen how classical counterpoint and serial operations can be formoulated as transformations. For example *augmentation*, *diminishion* and *phasing* can be accomplished with `stretch`, `compress` and `delay`:

```haskell+music
delay 1 (stretch 2 c)
  </>
transform (1 >-> 2) c
  </>
transform (1 <-> 3) c
```

Stretching by `(-1)` is also known as *retrograde*:

```haskell+music
stretch (-1) $ seq [c,d,e]
```

```TODOhaskell+music
retrograde $ seq [c,d,e]
```

### Translation-invariant types

We can think of our time types as coming in two shapes:

- Translation-invariant types such as `Duration` are "floating" without being anchored to specific start/stop time (though they still have a duration)
- Translation-variant types such as `Span` have both a specific duration and a specific point in which they "occur" relative to ther events.

Note that this does *not* invalidate the laws.

### Spans as time intervals

We mentioned before that @[Span] represents *non-empty* slices of time. The @[TimeInterval] type is similar to @[Span], but also allows for empty spans to be represented. It forms a monoid with the convex @[hull] operator. Intuitively the (convex) hull of two intervals is the smallest interval that contains them both.

```haskell
>>> TimeInterval (1 <-> 2) <> TimeInterval (0 <-> 1.5)
TimeInterval (0 <-> 2)

>>> mempty :: TimeInterval
EmptyInterval

>>> EmptyInterval <> TimeInterval (3 <-> 4)
TimeInterval (3 <-> 4)
```


## Position and Duration

### Inspecting Duration

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

### Inspecting Position

The @[HasPosition] class represent values that have an *absolute position* in time. The simplest example is `Span`:

```haskell
>>> _era (1 <-> 3)
Just (1 <-> 3)
```

`Nothing` is used to represent the "empty era". This is used for scores which doesn't have any notes. The class `HasPosition1` is similar to  `HasPosition`, but is only defined for types that can not have ane empty era. Consequently, there is no such instance for the `Score` type (since empty scores are allowed).

```haskell
>>> _era1 (1 <-> 3)
1 <-> 3
```

Values with a position allow many useful combinators to be defined. For example here we use `during` to add a pedal note to a melody:

```haskell+music
let
    melody = legato $ seq [seq [c,d,e,c], seq [e,f], g|*2]
    pedal = c `during` melody
in compress 4 $ melody </> pedal
```

<!--
TODO example with stretchRelative, stretchTo

The laws for @[HasPosition] and @[HasPosition1] are not too exciting: they assure that transforming a value also transforms its position in the same manner, and that the duration of a value is exactly the duration between its onset and offset point.
-->



### Alignment

The @[Aligned] type adds position to anything with a duration. This is akin to alignment in computer graphis, hence the name. Alignment works by picking:

- A time point to which the value is "anchored". By default this is time zero.
- An alignment point in the duration of the value. By default this is the onset of the value.

Aligned is natural way of modelling pickups and upbeats. Consider this melody:

```haskell+music
(seq [g_,a_,b_]|/2 |> seq [c, c, d, d]) |/ 4
```

With @[Aligned] we can represent the fact that the first three notes are "upbeat" notes, and that the main stress of the value should fall on the fourth note:

```haskell+music
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





## Notes and Events

The Note and Event types are similar to Duration and Span respectively, except they also contain a *payload* of an arbitrary type. This is expressed as a type parameter (often written using a lowercase letter, as in `Note a`).  In practice the payload will usually contain (possibly overloaded) *aspects* such as part, pitch, dynamics and so on.

A @[Note] represents a single value tagged with a *duration*:

```haskell+music
inspectableToMusic @(Note Pitch) $

c
```

An @[Event] represents a single value tagged with a *time span*:

```haskell+music
inspectableToMusic @(Event Pitch) $

c
```

Note that we can enter a single note or event as `c`, `d`, `eb`, etc. because of [pitch overloading](#pitch-overloading).

Notes and events are very similar. The main difference is that notes are translation-invariant and events are not.

```haskell+music
inspectableToMusic @(Note Pitch) $

delay 1 $ stretch 0.5 c
```

```haskell+music
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

```haskell+music
inspectableToMusic @(Note StandardNote) $

set parts' violins $ pizz $ level ff $ accentAll $ compress 4 c
```

You may wonder, what is the point of representing music with *just a single note*? The answer is that these types are used in combinations with other types. For example we can use `Note [Pitch]` to represent a melody where all notes have the same duration.




## Voices

A @[Voice] represents a *sequential composition of values*, each tagged with *duration*.

### Creating voices


As with notes and events, we can enter a single voice using `c`, `d`, `eb`, etc. because of [pitch overloading](#pitch-overloading).

```haskell
>>> c :: Voice Pitch

>>> rest :: Voice (Maybe Pitch)

>>> Voice.singleton True

>>> pure True :: Voice Bool
```

You can also put a rests in a voice. Notice how this changes the type:

```haskell
>>> [c,d,rest,e] :: Voice (Maybe Pitch)
```

All the pitch names resolve to `Just c`, `Just d` and so on, while the rest resolves to `Nothing`.

We can also create voices from rhythms (lists of durations);

```haskell
>>> Voice.rhythm [1,2,3] :: Voice ()

>>> c <$ Voice.rhythm [1,2,3] :: Voice Pitch
```

Or from a list of notes:

```haskell
>>> view voice [c,d,e]

>>> [c,d,e]^.voice

>>> [(1,c)^.note,(2,d)^.note,(1,e)^.note]^.voice
```

TODO using comprehensions

```haskell+music
inspectableToMusic @(Voice Pitch) $

[cs, bb, a |* 2] |/ 4
```

### Traversing voices

TODO

```haskell
>>> over pitches (up m3) ([c,d,e] :: Voice Pitch) :: Voice Pitch
```

### Transforming voices

TODO Transformable, HasDuration

Voices do not have a position, i.e. they are translation-invariant (if you want to anchor a voice at a specific point in time, see [`Aligned`](#alignment)).


TODO rotation

@[rotateDurations]
@[rotateValues]

TODO fusion and stretch

@[fuse]
@[fuseBy]
@[fuseRests]

TODO take/drop

TODO filtering (MonadPlus!)

### Combining voices

TODO Monoid, Monad, Applicative, MonadZip + guard

```haskell+music
inspectableToMusic @(Voice Pitch) $

stretch (1/4) $ do
  x <- [c, d, e] |/ 2
  y <- [c', b, bb] |/ 2
  [x, g, y |*4 ]
```


```haskell+music
inspectableToMusic @(Voice Pitch) $

stretch (1/4) $ do
  x <- [c, d, e] |/ 2
  [x, b, c' |*4 ]
```


```haskell+music
inspectableToMusic @(Voice [Pitch]) $

[ [x,y] | x <- [c], y <- [d,e] ]
```

```haskell+music
inspectableToMusic @(Voice [Pitch]) $

[ [x,y] | x <- [c] | y <- [d,e] ]
```

```haskell+music
inspectableToMusic @(Voice [Pitch]) $

[ [x,y,z] | x <- [c] | y <- [d,e] | z <- [f,g] ]
```

```haskell+music
inspectableToMusic @(Voice [StandardNote]) $

[ dynamics' .~ d $ p
  | p <- [c, ab, fs, g]
  | d <- [ppp, ff, mp, mf]
  ]
```

```haskell+music
inspectableToMusic @(Voice [Pitch]) $

[ [x,y] | x <- view voice (map fromPitch $ enumChromaticFromTo c c''), y <- [d,e]
  , isMelodicConsonance (x .-. y) && isConsonance (x .-. y) ]
```



## Scores

A @[Score] represents a *parallel composition of values*, each tagged with *time span*.

We can think of a @[Score] as a @[Voice] where overlapping notes are allowed.

### Creating scores

The *empty score* has no events.

A *rest* is a score containing a single `Nothing` value. We can create

A *single-note* score can be created in a variety of ways:

- We can use [pitch literals][TODO link] to create a score of duration 1: `c`, `cs`, `d` etc. This creates a score in the *default time span* `0 <-> 1`.
- We can apply `pure` to any value. This also creates a score in the

Because of overloading we can easily mix notes and rest in literals. Note that `c == pure c == pure (Just c)`. Similarly, `rest == pure Nothing`.


TODO Composition

TODO Monad comprehensions

TODO From a list of events

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

```haskell+music
seq [c,rest,d] |/ 4
```

### Scores versus more restricted types

`Score` is an extremely flexible type. Often it is useful to work with more restrictive structures, such as `Voice`, `Note`, `Pattern`, or their `Aligned` versions.

TODO parsing scores into restricted types

TODO rendering restricted scores as type




## Patterns

A @[Pattern] can be throught of as a generalization of a *rhythm* or *beat*. They are similar to scores, but are infinite. Each pattern is created by repeating a number of layers. Every pattern will eventually repeat (though he repeating duration may be very long).

### Creating patterns

The basic way of buildings patterns are @[newPattern] and @[rhythmPattern].

TODO example

### Composing patterns

We can compose patterns in parallel using the regular composition operator @[<>].

```haskell+music
map Just $ renderPattern (a <> b) (0 <-> 4)
  where
    a = parts' .~ mempty $ Pattern.rhythm [3,3,4,2,4] |/ 8
    b = parts' .~ flutes $ Pattern.rhythm [1] |/ 8
    -- TODO use claves, maracas here
```

As patterns are infinite, we can compose patterns of different durations. Both patterns will just be repeated indefinately.

```haskell+music
map Just $ renderPattern (a <> b) (0 <-> 2)
  where
    a = parts' .~ trumpets  $ Pattern.line [c,d] |/ 8
    b = parts' .~ trombones $ Pattern.line [c,d,e] |/ 8
```

### Transforming patterns

Patterns are @[Transformable], @[Transposable], @[Attenuable] and so on, so many expressions that work for scores and voices also work for patterns. For example we can set parts and dynamics, or transpose patterns.

```haskell+music
map Just $ renderPattern (a <> b) (0.5 <-> 1.5)
  where
    a = parts' .~ mempty $ Pattern.rhythm [3,3,4,2,4] |/ 8
    b = parts' .~ flutes $ Pattern.rhythm [1] |/ 8
```

```haskell+music
map Just $ renderPattern (stretch 0.5 $ up m3 $ a <> b) (0 <-> 2)
  where
    a = parts' .~ mempty $ Pattern.rhythm [3,3,4,2,4] |/ 8
    b = parts' .~ flutes $ Pattern.rhythm [1] |/ 8
```


```haskell+music
map Just $ renderPattern (a <> b) (0 <-> 2)
  where
    a = parts' .~ trumpets  $ Pattern.line [c,d,e |* 2] |* (3/15)
    b = parts' .~ trombones $ Pattern.line [c,d,e |* 2] |* (3/8)
```

You can adjust the "phase" of a pattern using @[delay]. This is useful together with the composition operator:

```haskell+music
map Just $ renderPattern (a <> b <> delay (1/4) c <> delay (1/4) d) (0 <-> 2)
  where
    a = parts' .~ flutes    $ Pattern.rhythm [1/2,1/2]
    b = parts' .~ oboes     $ Pattern.rhythm [1,1/2,1/2]
    c = parts' .~ trumpets  $ Pattern.rhythm [1/2,1/2]
    d = parts' .~ trombones $ Pattern.rhythm [1,1/2,1/2]
```

The @[renderPattern] function returns the events of the pattern within a given time span.

<!--
TODO finish/move to examples:

```TODOhaskell+music
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
    bachCMajPattern = newPattern $ stretchTo 1 $ (view voice) $ map pure [0,1,2,3,4,2,3,4]
```
-->

<!-- TODO pattern coockbook, a la https://doc.sccode.org/Tutorials/A-Practical-Guide/PG_01_Introduction.html -->

## Signals

The structures we have been dealing with so far are all discrete, capturing some (potentially infinite) set of *time points* or *notes*. We will now look at an alternative time structure where this is not necessarily the case. @[Signals] represents a *time-varying values*, or functions of time.

TODO example

Signals are continous, which implies that:

- They are defined at *any point in time*. A behavior always has a value, unlike, e.g. aligned boices.

- They can change at infinitely small intervals. Just like with vector graphics, signal allow us to zoom in arbitrarily close, and potentially discover new changes. Thus it is (in general) nonsensical to talk about *when* a signal changes.

TODO rename Reactive -> StepSignal or similar

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

```haskell+music
inspectableToMusic @[Voice Pitch] $

[ beginning 0.5 melody
, ending 0.5 melody
]
  where
    melody = {- accent $ legato -} mconcat [d, mconcat [g,fs]|/2,bb|*2]|/4
```

```haskell+music
inspectableToMusic @[Voice Pitch] $

[ beginning (1/2+1/8) melody
, ending (1/2+1/8) melody
]
  where
    melody = {- accent $ legato -} mconcat [d, mconcat [g,fs]|/2,bb|*2]|/4
```

@[rev] reverse, retrograde

```haskell+music
let
    melody = accent $ legato $ seq [d, seq [g,fs]|/2,bb|*2]|/4
in melody |> rev melody
```

```haskell+music
music |> rev music
  where
    music = (1/16) *| seq [c|*3, legato $ seq [accent eb, fs|*3, a, b|*3], gs, f|*3, d]
```

-->






# Pitches and Intervals

In this chapter we will learn how to represent pitches and intervals and perform basic operations such as transposition and scaling. In the next chapter we will use these to build harmony concepts such as chords and scales.

## The Pitch type

The @[Pitch] representation implements the pitch of common (or Western) music notation, with built-in support for the diatonic/chromatic transposition, enharmonics and spelling. As we shall see later this is not the only way of representing pitch in Music Suite, but it is common enough to be the default.

### Pitch names

The following pitch names are used:

```haskell+music
seq [c, d, e, f, g, a, b]
```

### Octaves

We can change octave using @[octavesUp] and @[octavesDown]:

```haskell+music
octavesUp 4 c
  </>
octavesUp (-1) c
  </>
octavesDown 2 c
```

There are synonyms for the most common cases:

```haskell+music
_8va c <> c <> _8vb c
```

The following is also a shorthand for alternative octaves:

```haskell+music
c__ |> c_ |> c |> c' |> c''
```

### Sharps and flats

Sharps and flats can be added using @[sharpen] and @[flatten].

```haskell+music
sharpen c
  </>
(sharpen . sharpen) c
  </>
flatten c
  </>
(flatten . flatten) c
```

The @[alter] function is an iterated version of @[sharpen]/@[flatten]:

```haskell+music
alter 1 $ seq [c,d,e]
```

Double sharps/flats are supported:

```haskell+music
seq $ map (`alter` c) [-2..2]
```

The pitch representation used in Music Suite does in fact allow for an *arbitrary* number of sharps or flats. As there are no symbols for these in standard notation, they are automatically re-spelled in the output. Here is the note `c` written with up to 4 flats and sharps.

```haskell+music
seq $ map (`alter` c) [-4..4]
```

There is of course also a shorthand for sharps and flats:

```haskell+music
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

```haskell+music
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

```haskell+music
let
    intervals = [diminish _P5, (diminish . diminish) _P5]
in seq $ map (`up` c) intervals
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

```haskell+music
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
>>> (_A2 :: Interval) == m3
False

>>> (_A2 :: Interval) =:= m3
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


```haskell+music
seq $ map (\x -> over pitches' (relative c $ spell x)  $ par [as,cs,ds,fs])
[ usingSharps
, usingFlats
, modally
]
```

<!-- TODO simpler short cut for `over pitches' (relative c $ spell ...)`: -->

```haskell+music
x </> over pitches' (relative c $ spell modally) x
  where
    x = seq [cs,flatten db,bs]
```


## Transposing and inverting music

The `VectorSpace` and `AffineSpace` allow us to apply *affine transformations* to pitches. Because pitch is (roughly) a one-dimensional space, this means scaling and transposition. Let's look at some examples.

> Warning: The musical term *transposition* is known as *translation* in maths (where *transposition* means something else entirely!).



### Basic transposition

We can transpose a music expression using the @[up] and @[down] functions.

```haskell+music
up m3 tune
  where
    tune = seq [c,c,g,g,a,a,g|*2] |/8
```

```haskell+music
down _A4 tune
  where
    tune = seq [c,c,g,g,a,a,g|*2] |/8
```

As you might expect `down` is the same as `up . negate` and vice versa.

```haskell+music
up (-m3) tune
  where
    tune = seq [c,c,g,g,a,a,g|*2] |/8
```

> Note: transposing does *not* automatically change the key signature. See [key signatures](#key-signatures) for how to do this explicitly.

The @[up] and @[down] functions perform *chromatic transposition*. For diatonic transposition, see below.

### Parallel motion

The @[above] and @[below] functions are similar to @[up] and @[down], but also retain the original music.

```haskell+music
above m3 tune |> below m3 tune
  where
    tune = seq [c,c,g,g,a,a,g|*2] |/8
```

Note that (like `up` and `down`) these functions perform *chromatic transposition* by default. This can make extended sequences created using up and down a distinctive non-tonal sounds. For example paralallel major thirds create whole-tone fields:

```haskell+music
above _M3 [c,d,e]
```
Parallel minor thirds generates the octatonic scale:

```haskell+music
above m3 [a,b,c']
```


### Diatonic transposition

We can also perform *diatonic transposition*. As music expressions can not be presumed to have any particular *tonic* by default, we have to provide this as an extra argument. For example `upDiatonic c 2` means "transpose upwards two diatonic steps, with C as the tonic".

```haskell+music
let
  ch x = par [x, upDiatonic c 2 x, upDiatonic c 5 x]
in seq $ ch <$> [c,d,e,f,g,a,g,c',b,a,g,fs,g |* 4] |/ 8
```

Here is same example, using a different tonic (`fs` instead of `c`):

```haskell+music
let
  ch x = par [x, upDiatonic fs 2 x, upDiatonic fs 5 x]
in seq $ ch <$> [c,d,e,f,g,a,g,c',b,a,g,fs,g |* 4] |/ 8
```

### Scaling pitch

As we have seen intervals form a *vector space* and pitches an associated *affine space*. This implies we can define a form of scalar multiplication.

However pitches live in an affine space without a specific origin, so we have to pick one:

```haskell+music
m </> scale 2 c m </> scale 2 e m
  where
    scale n p = pitches %~ relative p (n *^)
    m = seq (map fromPitch [c,d,e,f,g]) |*(2/5)
```

Note how the origin stays the same under scaling.

### Inverting pitch

The @[invertPitches] function is a shorthand for the special case of scaling by `-1`:

```haskell+music
m </> invertPitches c m </> invertPitches e m
  where
    m = seq [c,d,e,f,g] |*(2/5)
```

As with transposition we can define a *diatonic* form of inversion. The function is @[invertDiatonic].

```haskell+music
m </> invertDiatonic c m </> invertDiatonic e m
  where
    m = seq [c,d,e,f,g] |*(2/5)
```

In this case, the origin is also used as the tonic of the implied diatonic scale.

## Listing and traversing pitches

You can extract all the pitches from a piece of music like this:

```haskell+music
inspectableToMusic $
  toListOf pitches' (seq [c,d, par[e,g]] :: Music)
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

While the `Pitch` and `Interval` types allow us to represent any pitch (within the Western/classical framework), they do not tell us much about *harmony*. We need types to represent *collections* and *relationships* between pitches, including modes, chords and scales.

While we can of course course use regular data structures like tuples, lists and maps for this, Music Suite also defines some structures that makes particular sense from a musical point of view.

## Ambitus

The @[Ambitus] type represents a *range* of pitches. We can think of an ambitus as an interval with a starting point, or (equivalently) as a pair of pitches.

> Note: For pitch, we use *ambitus* instead of the ambigous *range* or *interval*.

```haskell+music
inspectableToMusic @[Ambitus Interval Pitch] $

[ Ambitus c g
, Ambitus c_ c''
]
```

Note that the `Ambitus` type constructor is parameterized on both the pitch and interval type. Like most pitch contrainers it is a [bifunctor](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Bifunctor.html).

```haskell+music
inspectableToMusic @[Ambitus Interval Pitch] $

[                  Ambitus c g
, map (const c) $ Ambitus @Interval @Pitch c g
]
```

It is also an instance of `Transposable`, so all the pitch operations from the previous section work for @[Ambitus] as well:

```haskell+music
inspectableToMusic @[Ambitus Interval Pitch] $

[          Ambitus c g
, up _P5 $ Ambitus c g
]
```

You can extract the range of any piece of music using @[pitchRange]:

```haskell+music
seq [c,d,fs,g,db,c,b_,c,g,c,e] |/ 8
```

```haskell+music
inspectableToMusic @(Maybe (Ambitus Interval Pitch)) $

pitchRange @Music $ seq [c,d,fs,g,db,c,b_,c,g,c,e] |/ 8
```

## Scales and chords

The @[Scale] and @[Chord] types represent infinite collections of pitches, anchored at some absolute pitch known as the *tonic*. Like @[Ambitus], @[Scale] and @[Chord] are type constructors taking two type parameters for interval and pitch respectively.

```haskell+music
inspectableToMusic @[Scale Interval Pitch] $

[ scale c phrygian
, scale d majorScale
, scale e bluesMajor
, scale f wholeTone
, scale g octatonic
, scale a thirdMode
]
```

```haskell+music
inspectableToMusic @[Chord Interval Pitch] $

[ chord g majorTriad
, chord c minorTriad
, chord f augmentedChord
, chord eb diminishedChord
]
```

The @[scale] and @[chord] functions take two parameters: the *tonic* (e.g. the absolute pitch at which the chord is centered) and a *mode* or *chord type* describings the characteristics of the chord. Most common scale and chord types are pre-defined, but as will see later it is also possible to make up custom scales and chords.

```haskell+music
inspectableToMusic @[ChordType Interval Pitch] $

[ majorTriad
, minorTriad
, augmentedChord
, diminishedChord
, halfDiminishedChord
]
```

```haskell+music
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

```haskell+music
inspectableToMusic @[Voiced Chord Interval Pitch] $

[ voiceIn 5 $ chord c quartal
, voiceIn 4 $ chord c quintal
]
```

Similarly *clusters* are one-note scales repeating at the second:

```haskell+music
inspectableToMusic @[Voiced Chord Interval Pitch] $

[ voiceIn 5 $ chord c chromaticCluster
, voiceIn 7 $ chord c wholeToneCluster
]
```

And we can repeat at arbitrary intervals:

```haskell+music
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

>>> map (index $ chord c majorTriad)) [-1,0,1,2]
[g_,c,e,g]

>>> map (index $ chord g majorMinorSeventhChord)) [-2,-1,0,1]
[d,f,g,b]
```

### Transforming chords

Naturally, @[Scale] and @[Chord] are instances of `Transposable`:

```haskell+music
inspectableToMusic @[Scale Pitch] $

[         scale c phrygian
, up m3 $ scale c phrygian
]
```

```haskell+music
inspectableToMusic @[Chord Pitch] $

[                     chord c majorTriad
, invertDiatonic c  $ chord c majorTriad
, invertDiatonic gb $ chord c halfDiminishedChord
]
```

This is useful for building chord sequences:

```haskell+music
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

```haskell+music
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

```haskell+music
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

```haskell+music
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

```haskell+music
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

```haskell+music
inspectableToMusic @(Voiced Chord Pitch) $
  voiced (chord d majorTriad)
```

To generate a closed voicing with doubled notes, use `voiceIn`.

```haskell+music
inspectableToMusic @[Voiced Chord Pitch] $
[ voiceIn 4 $ chord c majorTriad
, invertVoicing (-2) $ voiced $ chord g majorMinorSeventhChord
, voiceIn 4 $ chord c majorTriad
]
```

### Operations on voicings

We extract the pitches from a voiced chord like this:

```haskell+music
seq $ map fromPitch ps
  where
    ps :: [Pitch]
    ps = NonEmpty.toList $ getVoiced v

    v :: Voiced Chord Pitch
    v = voiceIn 4 $ chord c majorTriad
```

Voiced chords allow inversion:

```haskell+music
inspectableToMusic @[Voiced Chord Pitch] $
  map (`invertVoicing` vs) [ -1..4 ]
  where
    vs = voiced (chord c majorTriad)
```

### Other voicings

We can also create custom voicings, using any combination of integers. Recall that `0` stands for the origin, `1` for the first note above the origin, `2` for the next and so on. Negative numbers repeat the pattern below the origin.

```haskell+music
inspectableToMusic @(Voiced Chord Pitch) $
  Voiced (chord d minorTriad) [0,1..6]
```

```haskell+music
inspectableToMusic @(Voiced Chord Pitch) $
  Voiced (chord d minorTriad) [0,2..6]
```

```haskell+music
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
























# Dynamics and Articulation

## Adding dynamics

Dynamics can me applied using @[level]:

```haskell+music
level ppp c
```

Here is an overview of the standard dynamic values:

```haskell+music
over eras (stretchRelativeOnset 0.5) $ seq $ zipWith level [fff,ff,_f,mf,mp,_p,pp,ppp] (map fromPitch [c..])
```

We can give any two dynamic values to `cresc` and `dim` (e.g. they are synonyms). A crescendo/diminuendo line will be drawn as necessary.

```haskell+music
(cresc pp mf $ seq [c,d,e,f,g,a,b,c'] |/8)
  </>
(dim fff ff $ seq [c,d,e,f,g,a,b,c'] |/8)
```

Long crescendos and diminuendos are supported as well.

```haskell+music
cresc pp mf $ (times 8 $ seq [c,d,e,f,g]) |/8
```

### How dynamics are represented

It is important to understand that dynamics are not stored as *marks and
lines*, but rather as values attached to each note. This means you can freely
split and merge without having to worry about dynamics.

In general, a new dynamic mark is drawn at the start of each entry, that is after
each period of rests per voice. However if the dynamic has not changed the mark is only
repeated if the last entry was a few bars ago.

```haskell+music
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

```haskell+music
legato (seq [c,d,e,f,g]|/8)
    </>
staccato (seq [c,d,e,f,g]|/8)
    </>
portato (seq [c,d,e,f,g]|/8)
    </>
tenuto (seq [c,d,e,f,g]|/8)
    </>
staccatissimo (seq [c,d,e,f,g]|/8)
```

### Accents

Adding accents is similar to regular articulations:

```haskell+music
accent (seq [c,d,e,f,g]|/8)
    </>
marcato (seq [c,d,e,f,g]|/8)
```

One difference is that by default, accents are only applied to the first note in each phrase. We can also explicitly specify the last note, or all the notes:

```haskell+music
accentLast (seq [c,d,e,f,g]|/8)
    </>
accentAll (seq [c,d,e,f,g]|/8)
```

### Articulations and phrases

We can apply slurs and articulation marks to scores of arbitrary complexity. The library will traverse each phrase in the score and apply the articulations separately.

For example in this example we're building up a score consisting of three parts and then apply `accent . legato`:

```haskell+music
let
    ps = map fromPitch [c..c']
    p1 = seq ps |/4
    p2 = delay (1/4) $ seq ps |/4
    p3 = delay (3/4) $ seq ps |/4
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

```haskell+music
c
```

Setting dynamics:

```haskell+music
set dynamics' pp c
```

Setting the accentuation and separation components of articulation:

```haskell+music
set articulations' (accentuation +~ 2 $ mempty) c
```

```haskell+music
over (articulations' . separation) (+ 2) c
```


















# Instruments and Parts

For working with multi-part scores, we first need to look at the distinction between parts and instruments:

- The @[Part] type represent a *vocal or instrumental part in a composition* such as `Violin I.I`, `Trumpet II`, etc.

- The @[Instrument] type represents a *class of instruments* such as `Violin`, `Guitar`, `Maracas`, etc. This includes vocals types and, electronics etc.


A *part* is a record type consisting of (among other things) an *instrument* and a *subpart* such as `I.I`.


There is no need to explicitly create staves, brackets or braces. These are created automatically based on the parts present in the score.

To illustrate this, here is an example of a score with all the notes in the same part:

```haskell+music
par [c,d,fs]
```
Here is a score with instruments in different parts:

```haskell+music
par [c,parts' .~ violins $ d,fs]
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

```haskell+music
parts' .~ trumpets $ par [c,d,fs]
```

Setting just the instrument:

```haskell+music
(parts' . instrument) .~ trumpet $ par [c,d,fs]
```

Setting just the subpart:

```haskell+music
(parts' . subpart) .~ 2 $ (parts' . instrument) .~ trumpet $ par [c,d,fs]
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

```haskell+music
c </> c
```

When this is not the case, `</>` behaves like `<>`:

```haskell+music
set parts' violins c </> set parts' violas c
```

The subpart of the left side is never changed, and the right side is always assigned to the next available subpart:

```haskell+music
set parts' p c </> set parts' p c
  where
    p = set subpart 2 $ violins
```

Note that as a consequence of this `</>` is not associative. Compare:

```haskell+music
c </> (e </> g)
```

versus

```haskell+music
(c </> e) </> g
```

This is normally not a problem, as `</>` associates to the left by default. Similarly with `rcat`:

```haskell+music
rcat [c,e,g]
```


## Staves and parts

It is important to understand the difference between *parts* and *staves*. While parts have a clear semantics in terms of perfomance, staves are a way of presenting this information visually. There is usually no need to worry about staves, they are automatically created depending on the parts present in the score.

```haskell+music
set parts' flutes c
```

Most instruments are drawn on a single staff. Certain instruments are drawn on multiple staves by default, however:

```haskell+music
set parts' (tutti celesta) c
```

## Updating several parts at once

An *ensemble type* can be represented as a list of parts. We provide a few pre-defined ones such as string quartet, chamber orchestra, etc. We  can also define a custom ensemble type:

```haskell
someEnsemble = divide 2 violins ++ [trumpet, clarinet]
```

We can update several parts at once using the @[arrangeFor] function. This is useful in combination with @[rcat]:

```haskell+music
arrangeFor stringOrchestra $ rcat [c',e,g_,c_]
```

## Soloists

Each part has a @[solo] component, which is either `Solo` or `Tutti`. This is useful when working with concertante scores.

```haskell+music
(parts' .~ solo violin $ seq [c,d,e,f,g,a,g,e,ds,e,cs,d,b,bb,a,ab] |/ 16)
  <>
arrangeFor stringOrchestra (seq [rcat [c',e,g_,c_]])
```

The default value is `Tutti`. In chamber music there is usually no need to override this with `Solo`, as the difference only make sense when you *need* to distinguish the solist.

<!--
TODO soloists *from* the orchestra/altri
-->

## Extracting parts

We can also *extract parts* from a score:

```haskell+music
extractPart violas fullScore |> fullScore
  where
    melody    = seq [c,d,e,f,g,a,g,e,ds,e,cs,d,b,bb,a,ab] |/ 16
    harmony   = seq [rcat [c',e,g_,c_]]
    fullScore =
      (parts' .~ solo violin melody)
        <>
      arrangeFor stringOrchestra harmony
```

 Note that if you're working with an external score writing program there is usually no need to do this in Music Suite: just export the entire score and use the part extraction mechanism in your editing application.

<!-- TODO this example needs a "multi-movement composition" operator to look nice -->

## More about instruments

We support all instruments in the MusicXML sound set. See [the full list here](https://www.musicxml.com/for-developers/standard-sounds/).

> Note: If you are working with the MIDI backend, only the General MIDI sound set is supported.

### Transposing instruments

We can obtain transposition infomation from instruments:

```haskell+music
inspectableToMusic @[Interval] $

[ transposition violin
, transposition clarinet
, transposition doubleBass
]
```


> Note: instruments that appear in many sizes are different instruments in Music Suite. The most common type of trumpet is Bb, so `trumpet` refers to this. For other instruments use `trumpetInC`, etc.


### Range

We can obtainin range information from instruments:

```haskell+music
inspectableToMusic @[Ambitus Interval Pitch] $

[ playableRange violin
, comfortableRange violin
]
```


## Playing Techniques

All instruments come with a variety of playing techniques, many of which produce fundamentally different sound types. We treat playing technique as a separate aspect from part and pitch.


### Tremolo, trills and rolls

A regular (measured) tremolo can be notated using the @[tremolo] function. Regular tremolo is is a shorthand for rapid iteration of a single note.


```haskell+music
tremolo 2 $ times 2 $ (c |> d)|/2
```

An unmeasured tremolo is notated using @[fastTremolo]. Unmeasured tremolo means "play individually, as fast as possible" and is a coloristic effet rather than a rhythmical shorthand.

Note that in keeping with traditional notation, we notate unmeasured tremolo using three beans.

```TODOhaskell+music
fastTremolo $ times 2 $ (c |> d)|/2
```

### Repeating vs. alternating tremolo

The former is rare but happen e.g. when double-stopped strings play bow tremolo (without bariolage). The more common one is a rapid alteration among a set of notes. Logically we should treat both as an optional the property of a single chord. Alas in standard notation the latter is commonly written as two chords with half the duration (or ins ome cases as a trill).

### Slide and glissando

```haskell+music
glissando $ seq [c,d]|/2
```

### Harmonics

Use the @[harmonic] function. The argument is the harmonic number, with zero being the fundamental, one the first overtone, and so on. Use sounding pitch, Music Suite will automatically figure out the correct notation.

```haskell+music
(harmonic 1 $ c|/2)
    </>
(harmonic 2 $ c|/2)
    </>
(harmonic 3 $ c|/2)
```

For artificial harmonics, use @[artificial]:

```haskell+music
artificial c |/ 2
```

## Instrument-specific techniques

### String techniques

By default string instruments play *arco* (using the bow). We can switch to *pizzicato* (plucked) using @[pizz]. Because *arco* is the default, simply applying @[arco] to an expression has no effect:

```haskell+music
set parts' violins $
  seq [arco $ staccato $ times 4 c, times 4 $ pizz g_ ] |/ 4
```

Traditionally the text "arco" is used to revert to bowed playing after a pizzicato section. This is inserted automatically.

```haskell+music
set parts' violins $
  seq [pizz $ seq [c,c,c,c], d |* 2, pizz e |*2 ] |/ 4
```

We can similarly indicate *bow* position. This instructs the performer to adjust the position of the bow to change the quality of the sound. Bow position is always relative to pressure and speed. The normal indications are:

* *Sul tasto*, closer to the bridge than normal, producing a flute-like sound.
* *Sul ponticello*, closer to the stable, producing an unstable, overtone-rich sound.


```haskell+music
set parts' violins $
  seq [sulTasto $ seq [c,c,c,c], posNat d |* 2] |/ 4
```

The text *naturale* or *nat* is used to revert to "normal" position. As with *arco*, this is inserted automatically.

```haskell+music
set parts' violins $
  seq [posNat $ seq [c,c,c,c], sulPont d |* 2] |/ 4
```


```haskell+music
set parts' violins $ seq
  [ colLegno c
  , colLegnoBatt c
  , senzaLegno c
  ] |* 2
```

We can switch between bowed versus plucked strings using @[pizz] and @[arco]. The default is /arco/ (bowed). As in standard string notation this is indicated by text at the point of change:

```haskell+music
set parts' violins $
  seq [arco $ staccato $ times 4 c, times 4 $ pizz g_ ] |/ 4
```

The text "arco" is used to cancel a previous "pizz". This is also inserted automatically. In the following example the first note in the second bar is using arco by default.

```haskell+music
set parts' violins $
  seq [pizz $ seq [c,c,c,c], d |* 2, pizz e |*2 ] |/ 4
```

Bow *position* on the string is indicated in a similar fashion. We support the following positions:

- Sul tasto: Close to the fingerboard
- Sul ponticello: Close to the stable
- Naturale: Normal position (the default)

As with pizz/arco, only changes are indicated:

```haskell+music
set parts' violins $
  seq [sulTasto $ seq [c,c,c,c], posNat d |* 2] |/ 4
```

As with "arco, the text "nat" is used to cancel a previous position and inserted by default.

```haskell+music
set parts' violins $
  seq [posNat $ seq [c,c,c,c], sulPont d |* 2] |/ 4
```

Bow *rotation* can be indated using one of the following:

- Col legno (tratto): play with the bow rotated to use the wooden part of the bow
- Col legno battuto: play with the wooden part of the bow only. This is normally only used for sharp attacks, hence the name.
- Senza legno: play with the bow hair (the default)

```haskell+music
set parts' violins $ seq
  [ colLegno c
  , colLegnoBatt c
  , senzaLegno c
  ] |* 2
```

Finally, string mutes are indicated using:

- Con sordino: With mute
- Senza sordino: Without mute (the default)

```haskell+music
set parts' violins $ seq
  [ conSord c
  , senzaSord c
  ]
  |/ 4
```

Here is an example using a combination of the above techniques:

```haskell+music
set parts' violins $ seq
  [ conSord $ arco c
  , pizz c
  , pizz $ conSord $ pizz c
  , conSord $ colLegno $ pizz c
  ]
  |* 1.5
```

<!--
TODO edit/remove duplication in this section

TODO chord tremolo
-->


```haskell+music
set parts' violins $ seq
  [ conSord c
  , senzaSord c
  ]
  |/ 4
```

```haskell+music
set parts' violins $ seq
  [ conSord $ arco c
  , pizz c
  , pizz $ conSord $ pizz c
  , conSord $ colLegno $ pizz c
  ]
  |* 1.5
```


### Wind techniques

<!--
TODO special fingerings, multiphonics, bisbigliando

TODO key sounds, percussive attacks ("pizz"), harmonics/whistle tones
-->

### Brass techniques

Standard mutes are similarly to string mutes:

- Con sordino: With mute
- Senza sordino: Without mute

The default is without mute.

```haskell+music
set parts' trombones $ seq
  [ conSord g_
  , senzaSord g_
  ]
  |* (3/2)
```

<!-- TODO alternative mutes -->

<!-- TODO hand stopping -->

<!-- TODO bells up -->

<!-- TODO vocalize/multiphonics -->

## Percussion

Working with percussion is much like working with normal instruments. There are some differences:

- Some percussion instruments no notion of pitch, or a limited set of pitches they can play.

- Percussion players tend to double on many different types of instruments than other musicians.

We currently do not ruling out entering pitches for e.g. snare drum parts. However backends will ignore the pitch information, and the music will render on a single-line staff.

As with other instruments we currently can not represent players doubling on multiple instrumentsexplicitly. You will have to manually enter the music in different parts and manually assure that there is no overlap in parts meant to be executed by the same performer.

> Note: for percussion we break the singular/plural naming convention and export a `Part` in the singular form.

The solo/tutti component is set to `Tutti` by default even though there might only be one performer in the group (the distinction would still make sense e.g. in a percussion concerto).

```haskell+music
parts' .~ snareDrum $ (`stretch` c) <$> rh [1,rh [1,1,1],1,1]
  where
    rh = stretchTo 1 . seq
```

For rolls see [the previous section](tremolo-trills-and-rolls).



<!--
# Lyrics and Vocals

TODO adding lyrics (including syllables/word boundaries/melismas)

TODO col legnoTODO soloists/character name
-->

<!--


# Non-note Actions

While most music notation is conerned with making sound, a score may call for events which are not meant to directly produce sound. We represent these things using special events called *actions*. Like with percussion actions have no pitch.

TODO representation? Some kind of sum type in the note stack?

## Piano/Vibraphone pedalling

TODO this should arguably be a property of the notes themselves, though we should provide an action for "release the pedal/damp". Woth adding a section on damping/secco/l.v. under "Percussion".

## Instrument change warnings

TODO

## Cues

TODO
-->


<!--
# Text and Color

> Warning: A core idea in Music Suite is that music expressions have clear *semantics*, based on how the sound or action they represent. Free text runs counter to this, and should be viewed as an "escape hatch". Try to use a more structured representation when possible.


@[text]

```haskell+music
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

```haskell+music
title "Frere Jaques" $ seq [c,d,e,c]|/4
```

Some backends may or may not render subtitles, depending on their configuration.

## Attribution

Similar to titles, the attribution of the creators of music can be annotated according to description such as @[composer], @[lyricist], @[arranger] etc. More generally, @[attribution] or @[attributions] can be used to embed arbitrary `(profession, name)` mappings.

```haskell+music
composer "Anonymous" $ seq [c,d,e,c]
```

```haskell+music
composer "Anonymous" $ lyricist "Anonymous" $ arranger "Hans" $ seq [c,d,e,c]|/4
```

Some backends may or may not render attribution information, depending on their configuration.

## Key signatures

By default the key signature of C is used. We can override the *global* key signature using @[keySignature].

```haskell+music
keySignature (key db MajorMode) $ seq [db,eb,f]
```

We can also set the key signature for a specific time span using @[keySignatureDuring].

```haskell+music
keySignatureDuring (1 <-> 2) (key db MinorMode) $ seq [db,eb,f]
```

A key signature change will always force a new bar.

```haskell+music
keySignatureDuring (1.5 <-> 2) (key db MajorMode) $ seq [db,eb,f]
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

```haskell+music
timeSignature (3/8) $ seq [db,eb,f]
```

We can also set the time signature for a specific time span using  @[timeSignatureDuring]. Time signature changes will always force a new bar.


### Converting between time signatures

Setting the time signature does *not* imply that the music is renotated. To accomplish this we'll need to use @[stretch] or @[compress]. For example, the following music is notated using a quarter note pulse.

```haskell+music
let
  ch = par [e,g,c']
  waltz = seq [c,ch,ch,g_,ch,ch] |* (1/4)
in
timeSignature (3/4) waltz
```

To *renotate* this to eight notes, we stretch the music by `1/2` and apply the new time signature:

```haskell+music
let
  ch = par [e,g,c']
  waltz = seq [c,ch,ch,g_,ch,ch] |* (1/4)
in
timeSignature (3/8) $ compress 2 waltz
```

This provide more flexibility for renotation. For example we can easily renotate a passage from `4/4` to `12/8` as follows:

```haskell+music
let
  ch = par [e,g,c']
  waltz = times 2 $ seq [c,ch,ch,g_,ch,ch] |* (1/4)
in
timeSignature (4/4) $ compress 3 $ waltz
```

```haskell+music
let
  ch = par [e,g,c']
  waltz = times 2 $ seq [c,ch,ch,g_,ch,ch] |* (1/4)
in
timeSignature (12/8) $ compress 2 $ waltz
```

Polymetric notation is not supported: you must pick one global time signature for each section of the score.


## Tempo



@[metronome]

@[tempo]

```haskell+music
tempo adagio $ seq [c,d,e,b,c] |/ (5*8) |> d |* (3/4)
```

```haskell+music
tempo (metronome (1/4) 80) $ seq [c,d,e,b,c] |/ (5*8) |> d |* (3/4)
```

```haskell+music
(tempo adagio $ seq [c,d,e,b,c] |/ (5*4) |> d |* (3/4))
  |>
(tempo allegro $ seq [c,d,e,f,g] |/ 4 )
```

Tempo changes will always force a new bar.

### Fermatas, caesuras and breathing marks

Fermatas indicate a certain time point (usually a strong beat) should be prolonged.

<!--
TODO representation should be: meta-mark at the first strong beat *after* the fermata-signed notes (e.g. the one to break *before*). This means we can render fermata signs on all notes where whose span overlaps the break point (including offset, not including onset).
-->

```haskell+music
fermata StandardFermata (par [c,e,g])
```

Note that a fermata attaches to a specific point known as the *sustain point* (the beginning of the given score is used by default). All notes overlapping the sustain point have a fermata drawn on them.

```haskell+music
fermata StandardFermata (par [seq[c,d] |/ 2,e,g])
```

<!--
A fermata usually implies a unison cutoff of the prolonged notes, followed by a short break before continouing to the next beat. This can be made explicit by addng caesuras or breathing marks (commas).
-->

<!--
### Ritardando and accellerando

```TODOhaskell+music
(rit (seq [c,d] |> e |* 2) |/ 4)
```

```TODOhaskell+music
(acc (seq [c,d] |> e |* 2) |/ 4)
```
-->

## Barlines and repeats

There is generally no need to enter bars explicitly, as this information can be inferred from other meta-information. Generally, the following meta-events (in any part), will force a change of bar:

* Key signature changes
* Time signature changes
* Tempo changes
* Rehearsal marks


Whenever a bar line is created as a result of a meta-event, an shorted time signature may need to be inserted before the change. For example here the change of time signature to 3/4 forces the insertion of a 2/4 bar.

```haskell+music
compress 4 $ timeSignature (4/4) (seq [c,d,e,c,d,e,f,d,g,d]) |> timeSignature (3/4) (seq [a,g,f,g,f,e])
```

We can force a new bar lines using @[barline].

```haskell+music
compress 4 $ seq [c,d,e] |> barline DoubleBarline (seq [d,e,f])
```

## Clefs

The standard for each instrument is used by default. There is currently no way of overriding it.

<!--
## Multi-movement scores

TODO
-->

## Rehearsal marks

Rehearsal marks are added to the beginning of the score by default:

```haskell+music
rehearsalMark $ seq [c,d,e,d,f,e,d,c] |/ 3
```

We can also add it to a specific position:

```haskell+music
rehearsalMarkAt 2 $ seq [c,d,e,d,f,e,d,c] |/3
```

A rehearsal mark carry no specific meaning. Composing two scores will interleave their rehearsal marks.

```haskell+music
rehearsalMarkAt 1 (up m3 m) </> rehearsalMarkAt 2 m
  where
    m = seq [c,d,e,c,d,f] |/ 2
```

Rehearsal marks will always force a new bar.

<!--
## Annotations

Annotations are simply textual values attached to a specific section of the score. In contrast to other types of meta-information annotations always apply to the whole score, not to a single part. To annotate a score use @[annotate], to annotate a specific span, use @[annotateSpan].

Annotations are *invisible by default*. To show annotations in the generated output, use
@[showAnnotations].

```TODOhaskell+music
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































<!--
# Form

### Building larger musical structures

Let's now look at how to use the types and classes introduced in this chapter to organize larger musical forms.

### Basic repetition

@[times]

```haskell+music
let
    melody = accent $ seq [c,d,e]|/16
in times 4 $ melody
```
-->
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

Traverals are a subtle and powerful concept. The basic ideas is simple: given some traverable "container" value, we have a way of *visiting its element in some order*. Traversals can be used to:

- Extract a list of the elements
- Find all elements matching a specific criteria
- Compute values by running *accumulators* over matching elements
- Change the structure by *updating* some or all of the elements

Traversals are also subject to some restrictions (TODO explain Traversal laws).

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

### Failure and state

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

TODO explain the type families: GetPitch, SetPitch, GetArticulation, SetArticulation, etc.


## Phrase traversals

*Phrase traversals* visit all the *phrases* in a score one at at time. They work in two steps: first they traverse each part in the score separately, and then each *consequtive* sequence of notes inside each part. Notes separated by rests are non-consequently, in other words, phrases are separated by rests.

Here is a phrase traversal applied to a single-part score:

```haskell+music
over (phrases' . Control.Lens._head) (up _P8) $ bar <> delay 1 bar <> delay 2 bar
  where
    bar = seq [c,c,c] |/ 4
```

This multi-part score is traversed partwise:

```haskell+music
over (phrases' . Control.Lens._head) (up _P8) $ bar </> delay (3/4) bar </> delay (5/8) bar
  where
    bar = seq [c,c,c] |/ 4
```

Any overlapping notes *within a single part* are ignored by phrase traversals:

```haskell+music
over (phrases' . Control.Lens._head) (up _P8) $
  bar <> delay (1/8) bar
  where
    bar = seq [c,c,c] |/ 4
```

## Filtered traversals

Filtered traversals operate on the elements selected by another traversals if they match a specific predicate.

This example transposes all notes with a duration less than `2`:

```haskell+music
inspectableToMusic @(Voice [StandardNote]) $

over t (up _P8) [d,d,d |* 2,d] |/ 4
  where
    t = notes . each . filtered (\x -> x^.duration < 2)
```

<!-- TODO rename (filtered -> when) -->

## More examples

### Traversing the notes in a voice

```haskell+music
inspectableToMusic @(Voice [StandardNote]) $

over t (\x -> if x^.duration > 1 then up m2 x else x) [d,d,d |* 2,d]
  where
    t = notes . each
```

```TODOhaskell+music
inspectableToMusic @(Voice [StandardNote]) $

traverseOf t _ [d,d,d |* 2,d]
  where
    t = notes . each
```


### Traversing all the events in a score

```haskell+music
canon </> renderAlignedVoice rh
  where
    rh :: IsPitch a => Aligned (Voice a)
    rh = map (map $ const c) $ aligned 0 0 $ view durationsAsVoice (tail $ toRelativeTime onsets)


    onsets :: [Time]
    onsets = Data.List.nub $ toListOf (events . each . onset) canon

    canon = rcat
      [ theme
      , theme |* (3/2)
      , theme |* 2
      ]
    theme = seq [e,a|*2,c',b|*2,a,gs|*3,e'] |/ 8
```





-->








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

## Output formats

### MIDI

The MIDI output generates  multi-track Standard MIDI files.

The MIDI format is suitable for generating sound using a software synthesizer such as [TiMidity++](https://en.wikipedia.org/wiki/TiMidity%2B%2B) or [Fluidsynth](https://en.wikipedia.org/wiki/FluidSynth), or for exporting music to a Digital Audio Workstation (DAW) software. If you want want to export your music to a visual score editor, consider using MusicXML instead.

### Lilypond

The Lilypond output generates Lilypond markup code compatible with Lilypond 2.18.2 or later.

Lilypond is a text-only music type setting program and produces very high-quality scores.  Lilypond output is suitable if you want to generate an entire score using Music Suite. It is also suitable for small examples and for rendering inline musical expressions in text documents (it is used to render the examples in this documentation).

> Note: There is no need to edit the Lilypond code generated by Music Suite and you should not attempt this unless you are an experienced Lilypond user.

### MusicXML

The MusicXML output generates XML compatible with the MusicXML 3.0.

MusicXML files can be imported by most music typesetting programs, including
Sibelius, Finale and MuseScore.

MusicXML output is suitable if you want to use Music Suite for generating
parts of a score, but perform further editing on the output result in a graphical score editor.

## Input formats

### MIDI

(Experimental)

### MusicXML

(Experimental)

### Lilypond

(Experimental)

### Sibelius

(Experimental)

<!--
# Tips and tricks

### Lists and streams

TODO use `[]` for finite, `Stream` for infinite


### Complex rhythms

Nested tuplets.

```haskell+music
stretch (2/3) (seq [c,d,e]) |> f |*2
```


```haskell+music
seq [seq [c,d,e] |* (2/(3)), c, d, e, f] |* (1/(5*4))
```

```haskell+music
seq [seq [c,d,e,f,g] |* (4/5), c, d] |* (2/(3*4))
```
-->


# Wall of Shame

TODO this is not documentation, move to some other location. Listing all "bad rendering" examplesas a visual issue tracker

### Quantization

```TODOhaskell+music
(`stretch` c) <$> rh [1,rh [1,1,1],1,1]
  where
    rh = stretchTo 1 . seq
```

```haskell+music
rcat
      [ theme
      , theme |* (3/2)
      , theme |* 2
      ]
  where
    theme = seq [e,a|*2,c',b|*2,a,gs|*3,e'] |/ 8
```

This should use nested tuplets:

```haskell+music
seq [seq [c,d,e] |* (2/(3)), c, d, e, f] |* (1/(5*4))
```

```haskell+music
seq [seq [c,d,e,f,g] |* (4/5), c, d] |* (2/(3*4))
```

```haskell+music
stretch (1/2) $ seq [c,d,e]|/3 |> f |> g|*2
```

Should render >1 tuplet:

```haskell+music
let
  ch = par [e,g,c']
  waltz = times 2 $ seq [c,ch,ch,g_,ch,ch] |* (1/4)
in
timeSignature (4/4) $ compress 3 $ waltz
```

### Alignment/pickups

This should render

```haskell+music
rcat $ map renderAlignedVoice $
[ aligned 0 0 c
, aligned 0 (1.5/view duration v) v |/ 4
]
  where
    v = ([g_,a_,b_]|/2 <> [c, c, d, d])
```
very much like this (except without the initial rests):
```haskell+music
rcat $ map renderAlignedVoice $ delay 1
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

*Copyright Music Suite contributors 2012–2020*

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />This documentation is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.
