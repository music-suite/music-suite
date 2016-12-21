% <a style="color:">Beyond the bar-beat grid</a>
% <a style="color:crimson">Notions of time in algorithmic composition</a>
%  <br/><br/> <img src="farm2014/images/logo3.svg"/>

## Music Suite

- Music Suite

- A CM system with focus on <a style="color:blue">*representation*</a> written by me and 2-3 other people

- Written in <a style="color:blue">Haskell</a> (text-based, functional language)

- music-suite.github.io


## Music Suite

- Long history, I will only cover some brief things here

- The topic of today is <a style="color:blue">time structure</a>


## Why?

- The ideas I want to explore in this presentation go back several years.

    - It's all about the strengths/limitations of various <a style="color:blue">notation systems</a>, and exploring this through
programming.

    - See the unfinished MA thesis.

- Can't talk about this without programming (yet!). <a style="color:crimson">Apologies!</a>

## Haskell brief: Values

- Haskell has values:

    - Numbers: <a style="color:blue">2</a>, <a style="color:blue">1/3</a>, <a style="color:blue">2.5</a>
    - Text: <a style="color:crimson">"hello"</a>, <a style="color:crimson">"what?"</a>
    - Symbols: C D E F G ...
    - Tuples: (<a style="color:blue">1</a>, <a style="color:blue">2</a>), (C, <a style="color:crimson">Flat</a>), (B, <a style="color:crimson">Flat</a>, <a style="color:blue">Major</a>)
    - Lists: [<a style="color:blue">1</a>,<a style="color:blue">2</a>,<a style="color:blue">3</a>], [<a style="color:crimson">"Allegro"</a>, <a style="color:crimson">"Sostenuto"</a>]

## Haskell brief: Functions

- Defined by listing cases

    - length []  = 0
    - length [x] = 1
    - length [x,y] = 2
    - \> length [1,2]
    - 2
    
- Defined recursively

    - length []      = 0
    - length [x..xs] = length xs + 1
    - \> length [1,2,3,4,5,6,7,8,9,10]
    - 10

## Haskell brief: Types

- Haskell values have types:

    - 2 :: <a style="color:crimson">Number</a>
    - "hello" :: Text
    - (1, "hello") :: (<a style="color:blue">Number</a>, Text)
    - [1,2,3] :: [<a style="color:green">Number</a>]

- Function types:

    - (+) :: <a style="color:crimson">Number</a> -> <a style="color:crimson">Number</a> -> <a style="color:crimson">Number</a>
    - length :: [a] -> <a style="color:crimson">Number</a>

## Haskell brief: Types

- Make your own types!

    - data <a style="color:crimson">PitchName</a> = C | D | E | F | G | A | B
    - type <a style="color:blue">Accidental</a> = <a style="color:crimson">Number</a>
    - type <a style="color:green">PitchClass</a> = (<a style="color:crimson">PitchName</a>, <a style="color:blue">Accidental</a>)

- Polymorphism

    - type <a style="color:green">Altered</a> a = (a, <a style="color:blue">Accidental</a>)

## Haskell brief: Done

- That's it!

## What does this means for a composer?

- <a style="color:blue">Values</a>: Music (i.e. melodies, harmonies, large-scale compositions)

- <a style="color:crimson">Functions</a>: Transformations (i.e. inversion, time stretch etc)

- <a style="color:green">Types</a>: Restrictions

- Example (invertPitches, rev, up/down, stretch)


## Back to time

- How is time usually handled in scores?

- A hierarchy: pieces, movements, bars, tuplets, beats (ties complicate things!)

## Inspired by graphics

- Vector vs. point
    - In maths and physics a vector is anything with a size and a duration
    - Consider geography: a "place" versus a "distance"

- We can use vectors to *represent* points by picking an arbitrary *point of reference*
    - Can be expressed as (R^n) where n is the number of dimensions (basis)

- In time we have only one dimension, but the vector/point distinction is still *very* useful!

## Time and Duration

```
type Time     = Number
type Duration = Number

type Span = (Time,Time)      -- (onset, offset)
          = (Time, Duration) -- (onset, duration)
          = (Duration, Time) -- (duration,offset)
```

## Vector operations

- On vectors we have +, -, *

```
(^+^) :: Duration -> Duration -> Duration
(^-^) :: Duration -> Duration -> Duration
(^*)  :: Duration -> Duration -> Duration
```


- With time we can only *add* a duration and take *difference*

```
(.+^) :: Time -> Duration -> Time
(.-.) :: Time -> Time -> Duration
```

## Transformations

```
stretch :: Transformable a => Duration -> a -> a
delay   :: Transformable a => Duration -> a -> a

stretchTo, reverse, etc
```

- What is `Transformable`?

    - Times, Durations, Spans

## Time containers: discrete

- Group durations with an *arbitrary* value

```
type Note a = (Duration, a)
type Voice a = [Note a]

type Event a = (Span, a)
type Score a = [Event a]
```

- These things are `Transformable`!

## Time containers: continuous

- <a style="color:blue">Reactives</a> and <a style="color:crimson">Behaviors</a>

```
type Behavior a = Time -> a
type Reactive a = ([Time], Time -> a)
```

- These are `Transformable` as well!

## Composition (!)

- There is only one composition function: `<>`
- How does the aforementioned types compose?

- Sequential and partwise composition: `HasPosition`


## Generalized music representation

- Separated into:
    - Time structures (i.e. previous slides!)
    - Aspects

- Aspects are (currently): Pitch, Dynamics, Articulation and Parts

- Standard representation uses CMN notions of these but you can
  replace them!

## An extra type: Pattern

type Pattern a = ?

- Constant or repeating

```
constant  :: Reversible a => a           -> Pattern a
repeat    :: Reversible a => Voice a     -> Pattern a
```

- Patterns are composable (in parallel) and transformable!

```
renderPattern :: Span -> Pattern a -> Score a
```




## Thanks!





