

--------

type Subpart          = [Division]
type Instrumentation  = (Technique x Instrument)

-- Using MusicXML Standard Sounds and a set of technique ids
type Technique        = Z
type Instrument       = Z
data Chorus = Solo | Tutti | Choro1 | Choro2 | OnStage | OffStage | BehindAudience -- ...

type Part             = (Instrumentation, Chorus, Subpart)

type ScInstrument = -- synthdef...

--------

-- TODO represent ornamentation and degree of melismation/joining (portamento, glissando etc)

type Articulation = (R^3) -- separation, prolongation, accentuation
  instance (Semigroup, Monoid, AdditiveGroup, AffineSpace)

staccato
legato
marcato
tenuto
portato
  
--------

type Diat       = Z
type Chrom      = Z


type CommonInterval   = (Diat x Chrom)
type CommonPitch      = Pitch

type 36TETPitch  = Z
type 24TETPitch  = Z
type 12TETPitch  = Z
type Frequency  = R   -- Hz (assume 440 is actual 440)

type Scale = [Interval]
-- TODO wraparound? Is it the last by convention?
-- Or do we (more generally) allow infinite scales

type ChordFunc = [Interval]

--------

type Space      = R^3  -- Position in space
  instance (Semigroup, Monoid, AdditiveGroup, AffineSpace)
type Surround   = R^3
  instance (Semigroup, Monoid, AdditiveGroup, AffineSpace)
type 2DSpace    = R^2
  instance (Semigroup, Monoid, AdditiveGroup, AffineSpace)
type Stereo     = R    -- Position in L-R (0-1) 
  instance (Semigroup, Monoid, AdditiveGroup, AffineSpace)

--------

type Level      = Z   -- mf, f, ff etc
  instance (Semigroup, Monoid, AdditiveGroup, AffineSpace)
type Amplitude  = R   -- dB (assume som SPL as 0 db)
  instance (Semigroup, Monoid, AdditiveGroup, AffineSpace)

--------

type TMap k v = k -> v -- or (v, {k}, k -> v)

type Behavior a = Time -> a
-- Infinite, continous
-- Pointwise monad and applicative, lifted monoid
type Segment a  = Duration -> a
-- Finite, continous
-- Pointwise Monad and Applicative, lifted Monoid
-- Nice composition with Note/Stretched
type Reactive a = ({Time}, TMap Time a)




-- Isomorphic to Track/Voice?
-- TODO Can be derived from the (Adjunction Delayed Behavior) instance 
--   See http://comonad.com/reader/2008/kan-extensions-ii/
type Player a = State Time a
type Reel a   = Store Time a







type Score a    = [Note a]
-- Applicative and Monad a la list (multiplies values and composes spans)
-- |> forces strictness (due to ordering) TODO what?
-- Can this be avoided?
-- OR
type Score = TMap Span [a]
-- Applicative instance (multiply simulataneous chords, ignore others)
-- No Monad

type Track a    = [Delayed a]
type Chord a    = [Delayed a]
-- Base on List or Seq, list is probably nicer for infinite pitch spaces etc

type Voice a    = [Stretched a]
-- Should "definately" be list-based
-- Applicative and Monad a la list
-- Free Monoid (sequential catenation)


type Graces a   = (Nominal (Voice a), a, Nominal (Voice a))
-- Identity, forces invariance under transformation and removal from performance respectively
type Nominal    = Identity :: * -> *
type Cue        = Identity :: * -> *
-- For completeness
type Rest       = Note `Compose` Constant () :: * -> *
type NoteRest   = Sum Note Rest
type NEChord
Iso (Chord a) ((Sum NEChord (Sum Note Rest)) a)


-- Graph Monad/Applicative?
-- Tree Monad/Applicative
-- Streams?



-- Past/Future/Bound works like a "suspended" split
Past :: * -> *
type Past a     = (a x Min Time)    :: * -> *
  deriving (Functor, Semigroup)
type Future a   = (Max Time x a)    :: * -> *
  deriving (Functor, Semigroup)
type Bound = Future `Compose` Past  :: * -> *
  deriving (Functor, Semigroup)
-- TODO current version is wrong!

-- Note/Delayed/Stretched works like a "suspended" transform
type Note a     = (Span x a)        :: * -> *
  deriving (
    Monad, Comonad, Traversable, -- TCM from pair
    Transformable,               -- ?
    )
    M(pure)    = pure
    M(f <$> x) = f <$> M(x)
    M(f <*> x) = M(f) <*> M(x)
    M(f =<< x) = (M . f) =<< M(x)
type Delayed a  = (Time x a)        :: * -> *
  deriving (Monad, Comonad, Traversable)
type Stretched  = (Duration x a)    :: * -> *
  deriving (Monad, Comonad, Traversable)

type Span a     = (Time -> Time)    :: * -> *
  deriving (
    Eq, Ord, Show,    -- From (T^2) representation (or equivalent)
    Semigroup, Monoid -- From Endo
    Transformable     -- transform = (<>)
    -- Splittable?
  ) 
  -- TODO Iso (T -> T) (T, D)
  Iso (T x D) (T^2)
  Iso (D x T) (T^2)
  -- TODO more elegant definitions
  -- TODO factor out the linear function pattern (a la MemoTrie/Diagrams)

type T = Time; D = Duration -- for succinctness
type Time       = R
  deriving (Eq, Ord, Show) -- Transformable?, Splittable?
type Duration   = R
  deriving (Eq, Ord, Show) -- Transformable?, Splittable?

Meaning Instance <- Instance Meaning


