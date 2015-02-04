



{-
Beyond the bar-beat grid: working with time in algorithmic composition

- Music Suite
- A CM system with focus on functional programing and general representation
- Written in Haskell (text-based, pure functional language)
    music-suite.github.io


- Long history, I will oncly cover some brief things here
- The topic of today is time structure

- The goal is to have a *truly general* computer music system
  (including full standard notation, i.e. the Gould standard)

- I tried to approach this subject without talking about programming at all and
  realized I can't really do it (yet!). Apologies!


The ideas I want to explore in this presentation go back several years.
It's all about the limitations of notation systems, and exploring this through
programming.
See the unfinished MA thesis.

-}

{-
Haskell has values:

  Numbers       2   1/3   2.5
  Text          "hello"
  Symbols       C D E F G ...
  
  
  , text, arbitrary symbols (i.e. C D E F G)
-}




-- "Aspect" types
type Pitch        -- proper spelling etc. Polymorphism gives us microintonation etc.
type Dynamics     -- scalar value (continous would be nice!)
type Articulation -- for an individual note (context is a problem!)
type Part         -- instrument and subdivision!

-- There are more but I shall not speak of them here!

-- Examples?
-- Create a simple melody
-- (Monoidal) composition and transformations
-- Check durations and eras
frereJaques = 

-- The time representation

-- How is time usually handled in scores?
-- A hierarchy: pieces, movements, bars, tuplets/beats (ties), notes/chords/rests

-- Vector vs. point
-- Consider geography: a "place" versus a "distance"

-- In maths and physics a vector is anything with a size and a duration

-- We can use vectors to *represent* points by picking an arbitrary *point of reference*
-- Can be expressed as (R^n) where n is the number of dimensions (basis)
-- 

-- In time we have only one dimension, but the vector/point distinction is still useful!

type Time     -- points  i.e. bar 1, beat 1
type Duration -- vectors i.e. 1/4

type Span = (Time,Time)      -- (onset, offset), also called "era"
          = (Time, Duration) -- (onset, duration)
          = (Duration, Time) -- (duration, offset)

-- * Time containers
-- Group durations with an *arbitrary* value
type Note a = (Duration, a)
type Voice a = [Note a]

type Event a = (Span, a)
type Score a = [Event a]

-- * Reactives and Behaviors
type Behavior a = Time -> a
type Reactive a = ([Time], Time -> a)

class Transformable where
class HasDuration where
class HasPosition where

-- Power of Functor/Applicative(Monad (pure, fmap, liftA2 and join)
constant :: a -> f a
map      :: (a -> b) -> f a -> f b
map2     :: (a -> b -> c) -> f a -> f b -> f c
join     :: f (f a) -> f a

         

-- Alignment
-- Three ways to represent a Span. Are there more? Yes!
type LocalDuration -- always in [0..1]
type Alignment = (Time, LocalDuration)
align :: Alignment -> Duration -> Span

-- Splitting
split :: Duration -> Voice a -> (Voice a, Voice a)
take :: Duration -> Voice a -> Voice a
drop :: Duration -> Voice a -> Voice a

-- Behaviors, reeactive and sampling
sample :: Aligned (Voice a) -> Behavior (a -> b) -> Aligned (Voice b)




////////////////////////

30 min?

- I will talk about Music Suite, a CM system written by me in collaboration with 3-4 other people.
  - Not the whole thing but specifically how it treats *musical time*
  - Written in Haskell: I did a talk about this on a Haskell conference and they were *sceptical* to the idea
    of introducing Haskell to GSMD.
  - When I started using Haskell (after using several other languages: PHP, Java, JavaScript, ManuScript, C, Lisp)
    I was very optimistic about it. I thought all composers would want to learn it.
    I now realise this was probably a stretch: it is an easy language in some ways, but there are some things that
    are very scary and "math"-y. (I still believe that FP is the most applicable form programming for artistic
    purposes: the graphical languages of the past approximate this). An easier pure FP language is the future (what about types)?
    Any, I digress.
    Programming is not for everyone of course (that's a good thing!).
  - I will try to make this meaningful to people who are not interested in programming.

- Why did I do this?
  Good question. When I started out I imagined I would spend about 6 months on it: it ended up being almost 5 years.
  This was basically time spent making mistakes. Choosing the wrong language, the wrong representation etc.
  I learned a lot through the way, and the way I view music changed a lot as part of this process.
  When I started out I (sort of) believed that traditional notation, the symphony orchestra etc was doomed, and that new music
  would have to make use of electronics. I was also attracted to the notion of breaking down traditional scales, rhyths, forms etc
  as you can do easily electronic music and only with difficulty in instrumental music.
  I now believe that the basic hierarchical patterns of traditional music are essential – not in themselves, but as a way of keeping
  the music tangible – i.e. I don't regard music as acoustic phenomena but as a language of exptression.

- What does music look like in Music Suite? Trivial examples.
  - The idea is basically to build up complex music from small examples.
  - Output: MIDI, MusicXML, Lilypond etc.

- Introduce basic types:
  Syntax: type X = Y
  Base: Number, String, Bool [], (..)
  
  Pitch, Dynamics, Part, Articulation
  Time, Duration
  Span
  Note, Voice
  Event, Score

- Automatic quantization. Understanding how everything is "just numbers" (actually vectors and points).
- Transformations: Transformable, Reversible, Splittable
- HasDuration and Semigroup. Different types of composition. How having just <> works (composition on all levels).
- Voices: zips, fusing
- Scores: join

- BONUS:
  Behaviors, esp interpolation