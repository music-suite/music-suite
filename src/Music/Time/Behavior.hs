{-# OPTIONS_GHC -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

module Music.Time.Behavior
  ( -- * Behavior type
    Behavior,

    -- * Construction
    behavior,
    sampled,
    constant,

    -- * Lookup
    (!),

    -- * Combinators
    switch,
    switch',
    latch,
    latchWithOptions,
    loop,

    -- splice,
    trim,
    trimBefore,
    trimAfter,
    concatB,

    -- * Common behaviors

    -- ** Oscillators
    line,
    sawtooth,
    square,
    sine,
    cosine,

    -- ** Impulse functions
    unit,
    impulse,
    impulse',
    turnOn,
    turnOff,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Music.Dynamics.Literal
import Music.Pitch.Alterable
import Music.Pitch.Augmentable
import Music.Pitch.Literal
import Music.Time.Event
import Music.Time.Internal.Preliminaries
import Music.Time.Internal.Transform
import Music.Time.Juxtapose
import Music.Time.Score
import Music.Time.Internal.Util (floor')

-- Behavior is 'Representable':
--
-- > ask = realToFrac <$> time
-- > localRep (- t) = delay t
-- > localRep (/ t) = stretch t

-- |
-- A time-dependent value.
newtype Behavior a = Behavior {getBehavior :: Time -> a}
  deriving (Functor, Applicative, Monad, Typeable)

-- $semantics Behavior
--
-- @
-- type Behavior a = 'Time' -> a
-- @

--

-- $musicTimeBehaviorExamples
--
-- 'behavior' let us convert any function to a behavior using '^.' or 'view'.
--
-- We can unwrap a behavior using @'from' 'behavior'@ or '!^'.
--
-- A sine function
--
-- @
-- ('sin' . (* 'tau') . 'realToFrac')^.'behavior'
-- @
--
-- A behavior that switches from (-1) to 1 at time 0
--
-- @
-- (\\t -> if t < 0 then (-1) else 1)^.'behavior'
-- @
--
-- A time-varying function applied to a value
--
-- @
-- ('+')^.'behavior' '<*>' 10
-- @

instance Show (Behavior a) where
  show _ = "<<Behavior>>"

-- instance Distributive Behavior where
-- distribute = Behavior . distribute . fmap getBehavior

-- instance Representable Behavior where
--   type Rep Behavior = Time
--   tabulate = Behavior
--   index (Behavior x) = x

instance Transformable (Behavior a) where
  transform s (Behavior a) = Behavior (a `whilst` s)
    where
      f `whilst` s = f . transform (negateV s)

deriving instance Semigroup a => Semigroup (Behavior a)

deriving instance Monoid a => Monoid (Behavior a)

deriving instance Num a => Num (Behavior a)

deriving instance Fractional a => Fractional (Behavior a)

deriving instance Floating a => Floating (Behavior a)

deriving instance AdditiveGroup a => AdditiveGroup (Behavior a)

-- TODO bad
instance Real a => Real (Behavior a) where
  toRational = toRational . (! 0)

instance IsString a => IsString (Behavior a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Behavior a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Behavior a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Behavior a) where
  fromDynamics = pure . fromDynamics

instance Alterable a => Alterable (Behavior a) where
  sharpen = fmap sharpen

  flatten = fmap flatten

instance Augmentable a => Augmentable (Behavior a) where
  augment = fmap augment

  diminish = fmap diminish

instance Eq a => Eq (Behavior a) where
  (==) = error "No overloading for behavior: (<=)"

instance Ord a => Ord (Behavior a) where
  (<=) = error "No overloading for behavior: (<=)"

  (>=) = error "No overloading for behavior: (<=)"

  (<) = error "No overloading for behavior: (<=)"

  (>) = error "No overloading for behavior: (<=)"

  max = liftA2 max

  min = liftA2 min

instance Enum a => Enum (Behavior a) where
  toEnum = pure . toEnum

  fromEnum = fromEnum . (! 0)

instance VectorSpace a => VectorSpace (Behavior a) where
  type Scalar (Behavior a) = Behavior (Scalar a)

  (*^) = liftA2 (*^)

instance AffineSpace a => AffineSpace (Behavior a) where
  type Diff (Behavior a) = Behavior (Diff a)

  (.-.) = liftA2 (.-.)

  (.+^) = liftA2 (.+^)

-- |
-- View a behavior as a time function and vice versa.
--
-- Note that this is just an alias defined to make the documentation nicer:
--
--
-- @
-- 'behavior' = 'tabulated'
-- @
behavior :: Iso (Time -> a) (Time -> b) (Behavior a) (Behavior b)
behavior = iso Behavior getBehavior

-- |
-- View a time function as a behavior.
--
-- @
-- sampled    = from behavior
-- x^.sampled = (x !)
-- @
sampled :: Iso (Behavior a) (Behavior b) (Time -> a) (Time -> b)
sampled = from behavior

-- |
-- A behavior that will always return the initial value.
constant :: a -> Behavior a
constant = pure

-- |
-- A behavior that gives the current time.
--
-- @
-- (line !) = id
-- @
line :: Fractional a => Behavior a
line = realToFrac ^. behavior

--
-- > f t = t
--

-- |
-- A behavior that varies from 0 to 1 during the same time interval and is 0 before and 1 after
-- that interval.
unit :: Fractional a => Behavior a
unit = switch 0 0 (switch 1 line 1)

-- > f t | t < 0     = 0
-- >     | t > 1     = 1
-- >     | otherwise = t

{-
-- |
-- A behavior that
interval :: (Fractional a, Transformable a) => Time -> Time -> Event (Behavior a)
interval t u = (t <-> u, line) ^. event
-}

-- |
-- A behavior that
sine :: Floating a => Behavior a
sine = sin (line * tau)

-- |
-- A behavior that
cosine :: Floating a => Behavior a
cosine = cos (line * tau)

-- |
-- A behavior that goes from 0 to 1 repeatedly with a period of 1.
sawtooth :: RealFrac a => Behavior a
sawtooth = line - fmap floor' line

-- |
-- A behavior that switches from 0 to 1 repeatedly with a period of 1.
square :: Num a => Behavior a
square = loop 1 $
  switch 0.5 1 0
-- |
-- A behavior that is 1 at time 0, and 0 at all other times.
impulse :: Num a => Behavior a
impulse = switch' 0 0 1 0

-- |
-- A behavior that is the value passed in at time 0 and mempty
-- at all other times
impulse' :: Monoid a => a-> Behavior a
impulse' x = switch' 0 mempty (pure x) mempty

-- |
-- A behavior that goes from 0 to 1 at time 0.
turnOn :: Num a => Behavior a
turnOn = switch 0 0 1

-- |
-- A behavior that goes from 1 to 0 at time 0.
turnOff :: Num a => Behavior a
turnOff = switch 0 1 0

--
-- Because the 'Time' type is fixed and unbounded in the current version, we can not
-- define a generix isomorphism from behaviors to segments. If we change the library to
-- provide multiple time representations (using TFs or similar), we should provide
-- these combinators:
--
-- > focusOnFullRange :: Bounded Time => Behavior a -> Segment a
-- > focusingOnFullRange :: Bounded Time => Iso' (Behavior a) (Segment a)
--

-- |
-- Instantly switch from one behavior to another.
--
-- >>> switch 1 10 20 ! 0
-- 10
switch :: Time -> Behavior a -> Behavior a -> Behavior a
switch t rx ry = switch' t rx ry ry

-- | Replace everthing before the given time by `mempty`.
trimBefore :: Monoid a => Time -> Behavior a -> Behavior a
trimBefore start = switch start mempty

-- | Replace everthing after the given time by `mempty`.
trimAfter :: Monoid a => Time -> Behavior a -> Behavior a
trimAfter stop x = switch stop x mempty

-- |
-- Instantly switch from one behavior to another with an optinal intermediate value.
switch' :: Time -> Behavior a -> Behavior a -> Behavior a -> Behavior a
switch' t rx ry rz = view behavior $ \u -> case u `compare` t of
  LT -> rx ! u
  EQ -> ry ! u
  GT -> rz ! u

-- | Sample the given behavior at the given time.
(!) :: Behavior a -> Time -> a
(!) = getBehavior

data Include = Included | Excluded

-- |
-- Determines whether a behavior will include the start and end of a 'behavior'.
data SpanOptions = SpanOptions{
  start:: Include, 
  end::Include
}

-- |
-- Switches to the second @Behavior@ during the span. When the @Behavior@ switches
-- is determined by @SpanEdge@. 
-- Assuming the @Span = s<->e@ and @time = t@ then if @start == Included@ 
-- the latch will switch when time @t == start@ otherwise it will start when 
-- time @t >= start@.
--
-- if @end == Included@ 
-- the latch will switch back to the original behavior when time @t == end@ 
-- otherwise it will start when time @t >= start@.
latchWithOptions :: SpanOptions -> Span -> Behavior a -> Behavior a -> Behavior a
latchWithOptions r s rx ry = case r of 
  SpanOptions{start=Excluded, end=Excluded} -> view behavior $ \u -> 
        if (u <= s ^.onset) || (u >= s ^.offset) then rx ! u 
        else ry ! u
  SpanOptions{start=Excluded, end=Included} -> view behavior $ \u -> 
        if (u <= s ^.onset) || (u > s ^.offset) then rx ! u 
        else ry ! u
  SpanOptions{start=Included, end=Excluded} -> view behavior $ \u -> 
        if (u < s ^.onset) || (u >= s ^.offset) then rx ! u 
        else ry ! u
  SpanOptions{start=Included, end=Included} -> view behavior $ \u -> 
        if (u < s ^.onset) || (u > s ^.offset) then rx ! u 
        else ry ! u

-- |
-- Switches to the second behavior during the span, otherwise first behavior.
-- If the span is `1 <-> 3` then at `1` it will switch to the second behavior
-- and at time `3` it will switch back to the first behavior.
-- 
-- If the duration of the span is zero, the given behavior is returned unchanged.
latch :: Span -> Behavior a -> Behavior a -> Behavior a
latch = latchWithOptions $ SpanOptions Included Excluded


-- | Replace everything outside the given span by 'mempty'.
trim :: Monoid a => Span -> Behavior a -> Behavior a
trim (view onsetAndOffset -> (on, off)) x = (\t -> if on <= t && t <= off then x ! t else mempty) ^. behavior

-- Treat each event as a segment in the range (0<->1) and compose.

-- | Flatten a score of behaviors. The behavior for each note is sampled in the units span @0 <-> 1@. Overlapping notes are merged using the monoid instance.
--
-- 'concatB' is a monoid homomorphism, that is:
--
-- @
-- concatB mempty   = mempty
-- concatB (a <> b) = concatB a <> concatB b
-- @
concatB :: Monoid a => Score (Behavior a) -> Behavior a
concatB = mconcat . toListOf traverse . mapWithSpan transform . fmap (trim mempty)

-- Internal

tau :: Floating a => a
tau = 2 * pi

-- |
-- Loops the behavior every @d@ seconds.
loop :: Duration -> Behavior a -> Behavior a
loop d b =
  Behavior $
      \t -> 
        let
          dx :: Duration = t .-. 0
          p :: Duration = dx - (d * fromInteger (truncate (dx/d)))
        in
          b ! (0 .+^ p)