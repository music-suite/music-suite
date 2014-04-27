
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# LANGUAGE CPP            #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Behaviors, or time-varying values.
--
-- TODO integrate better in the library
--
-------------------------------------------------------------------------------------

module Music.Time.Behavior (
    -- * Behavior type
    Behavior,
    -- ** Examples
    -- $musicTimeBehaviorExamples
    -- (!^),
    -- behavior',
    behavior,

    -- ** Combinators
    switch,
    switch',
    -- splice,
    -- trim,
    trimBefore,
    trimAfter,
    -- concatB,

    -- * Common behaviors
    time,
    unit,
    impulse,
    turnOn,
    turnOff,
    sawtooth,
    sine,
    cosine,
    
  ) where

import           Prelude                hiding (trimAfter)
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Ratio
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.VectorSpace

import           Music.Time.Split
import           Music.Time.Reverse
import           Music.Time.Bound
import           Music.Time.Note
import           Music.Time.Score
import Music.Time.Internal.Transform

import           Music.Pitch.Literal
import           Music.Pitch.Augmentable
import           Music.Pitch.Alterable

-----
import Data.Fixed
import Data.Functor.Rep.Lens
import           Data.Default
import           Data.Ratio

import           Control.Applicative
import           Control.Arrow                (first, second, (***), (&&&))
import qualified Control.Category
import           Control.Comonad
import           Control.Comonad.Env
import           Control.Lens                 hiding (Indexable, Level, above,
                                               below, index, inside, parts,
                                               reversed, transform, (|>), (<|))
import           Control.Monad
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Distributive
import           Data.Foldable                (Foldable)
import qualified Data.Foldable                as Foldable
import           Data.Functor.Rep
import qualified Data.List
import           Data.List.NonEmpty           (NonEmpty)
import           Data.Maybe
import           Data.NumInstances
import           Data.Semigroup               hiding ()
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.Traversable             (Traversable)
import qualified Data.Traversable             as T
import           Data.Typeable
import           Data.VectorSpace hiding (Sum(..))
import           Music.Dynamics.Literal
import           Music.Pitch.Literal

import qualified Data.Ratio                   as Util_Ratio
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import qualified Data.Ord as Ord
-----



-- Behavior is 'Representable':
--
-- > ask = realToFrac <$> time
-- > localRep (- t) = delay t
-- > localRep (/ t) = stretch t

-- |
-- A 'Behavior' is a value varying over time.
-- 
-- Use 'focusing' to view a particular 'Segment'.
--
-- The semantics are given by
--
-- @
-- type Behavior a = 'Time' -> a
-- @
--
newtype Behavior a  = Behavior { getBehavior :: Time -> a }   deriving (Functor, Applicative, Monad{-, Comonad-})

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
-- 
instance Show (Behavior a) where
  show _ = "<<Behavior>>"

deriving instance Typeable1 Behavior
deriving instance Distributive Behavior

instance Transformable (Behavior a) where
  transform s (Behavior a) = Behavior (a `whilst` s)
    where
      f `whilst` s = f . transform (negateV s)

instance Reversible (Behavior a) where
  rev = stretch (-1)
  -- TODO alternative
  -- rev = (stretch (-1) `whilst` undelaying 0.5)
  -- (i.e. revDefault pretending that Behaviors have era (0 <-> 1))

instance Representable Behavior where
  type Rep Behavior = Time
  tabulate = Behavior
  index (Behavior x) = x


{-
-- 
-- type instance Pitch                 (Behavior a) = Behavior (Pitch a)
-- type instance SetPitch (Behavior g) (Behavior a) = Behavior (SetPitch g a)
--
-- instance (HasPitch a a, HasPitch a b) => HasPitches (Behavior a) (Behavior b) where
--   pitches = through pitch pitch
-- instance (HasPitch a a, HasPitch a b) => HasPitch (Behavior a) (Behavior b) where
--   pitch = through pitch pitch
-- 
-}

-- TODO
-- type instance Pitch      (Behavior a) = Behavior a
-- type instance SetPitch b (Behavior a) = b
-- instance (Transformable a, Transformable b, b ~ Pitch b) => HasPitches (Behavior a) b where
--   pitches = ($)
-- instance (Transformable a, Transformable b, b ~ Pitch b) => HasPitch (Behavior a) b where
--   pitch = ($)
-- 
-- 
-- type instance Dynamic                 (Behavior a) = Behavior (Dynamic a)
-- type instance SetDynamic (Behavior g) (Behavior a) = Behavior (SetDynamic g a)
-- 
-- instance (HasDynamic a a, HasDynamic a b) => HasDynamics (Behavior a) (Behavior b) where
--   dynamics = through dynamic dynamic
-- instance (HasDynamic a a, HasDynamic a b) => HasDynamic (Behavior a) (Behavior b) where
--   dynamic = through dynamic dynamic
-- 
-- 
-- type instance Articulation                 (Behavior a) = Behavior (Articulation a)
-- type instance SetArticulation (Behavior g) (Behavior a) = Behavior (SetArticulation g a)
-- 
-- instance (HasArticulation a a, HasArticulation a b) => HasArticulations (Behavior a) (Behavior b) where
--   articulations = through articulation articulation
-- instance (HasArticulation a a, HasArticulation a b) => HasArticulation (Behavior a) (Behavior b) where
--   articulation = through articulation articulation
-- 
-- 
-- type instance Part                 (Behavior a) = Behavior (Part a)
-- type instance SetPart (Behavior g) (Behavior a) = Behavior (SetPart g a)
-- 
-- instance (HasPart a a, HasPart a b) => HasParts (Behavior a) (Behavior b) where
--   parts = through part part
-- instance (HasPart a a, HasPart a b) => HasPart (Behavior a) (Behavior b) where
--   part = through part part  



-- Needed by Reactive
deriving instance Semigroup a => Semigroup (Behavior a)
deriving instance Monoid a => Monoid (Behavior a)
deriving instance Num a => Num (Behavior a)
deriving instance Fractional a => Fractional (Behavior a)
deriving instance Floating a => Floating (Behavior a)

-- #ifdef INCLUDE_LIFTED
deriving instance AdditiveGroup a => AdditiveGroup (Behavior a)

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
  (==) = error "No fun"

instance Ord a => Ord (Behavior a) where
  (<) = error "No fun"
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
-- #endif

-- |
-- View a behavior as a time function and vice versa.
--
-- Note that this is just an alias defined to make the documentation nicer:
--
--
-- @
-- 'behavior' = 'tabulated'
-- @
--
behavior :: Iso (Time -> a) (Time -> b) (Behavior a) (Behavior b)
behavior = tabulated

-- |
-- View a time function as a behavior.
--
-- @
-- unbehavior    = from behavior
-- x^.unbehavior = (x !)
-- @
--
unbehavior :: Iso (Behavior a) (Behavior b) (Time -> a) (Time -> b)
unbehavior = from behavior

--
-- @
-- ('const' x)^.'behavior' ! t = x   forall t
-- @
--
--


-- |
-- A behavior that
--
time' :: Behavior Time
time' = id ^. tabulated

-- |
-- A behavior that gives the current time, i.e. the identity function
--
-- Should really have the type 'Behavior' 'Time', but is provided in a more general form
-- for convenience.
--
time :: Fractional a => Behavior a
time = realToFrac ^. tabulated
--
-- > f t = t
--

-- |
-- A behavior that varies from 0 to 1 during the same time interval and is 0 before and 1 after
-- that interval.
--
unit :: Fractional a => Behavior a
unit = switch 0 0 (switch 1 time 1)
-- > f t | t < 0     = 0
-- >     | t > 1     = 1
-- >     | otherwise = t
--

-- |
-- A behavior that
--
interval :: (Fractional a, Transformable a) => Time -> Time -> Note (Behavior a)
interval t u = (t <-> u, time) ^. note

-- |
-- A behavior that
--
sine :: Floating a => Behavior a
sine = sin (time*tau)

-- |
-- A behavior that
--
cosine :: Floating a => Behavior a
cosine = cos (time*tau)

-- |
-- A behavior that goes from 0 to 1 repeatedly with a period of 1.
--
sawtooth :: RealFrac a => Behavior a
sawtooth = time - fmap floor' time

-- |
-- A behavior that is 1 at time 0, and 0 at all other times.
--
impulse :: Num a => Behavior a
impulse = switch' 0 0 1 0
-- > f t | t == 0    = 1
-- >     | otherwise = 0
--

-- |
-- A behavior that goes from 0 to 1 at time 0.
--
turnOn  = switch 0 0 1

-- |
-- A behavior that goes from 1 to 0 at time 0.
--
turnOff = switch 0 1 0

--
-- TODO
--
-- Because the 'Time' type is fixed and unbounded in the current version, we can not
-- define a generix isomorphism from behaviors to segments. If we change the library to
-- provide multiple time representations (using TFs or similar), we should provide
-- these combinators:
--
-- > focusOnFullRange :: Bounded Time => Behavior a -> Segment a
-- > focusingOnFullRange :: Bounded Time => Iso' (Behavior a) (Segment a)
--

{-
-- |
-- View part of a 'Behavior' as a 'Segment'.
--
-- This can be used to modify a behavior in a specific range, as in
--
-- @
-- 'time' & 'focusing' ``on`` (2 '<->' 3) '+a~' 1
-- 'time' & 'focusingOn' (2 '<->' 3) '+~' 1
-- @
--
focusingOn :: Span -> Lens' (Behavior a) (Segment a)
focusingOn s = flip whilstM (negateV s) . focusing
-- or focusing . flip whilstM s
-}

-- focusing `on` s == focusingOn s
f `on` s = flip whilstM (negateV s) . f

-- |
-- Instantly switch from one behavior to another.
--
switch :: Time -> Behavior a -> Behavior a -> Behavior a
switch t rx ry = switch' t rx ry ry

-- | Replace everthing before the given time by `mempty`.
trimBefore :: Monoid a => Time -> Behavior a -> Behavior a
trimBefore start x = switch start mempty x

-- | Replace everthing after the given time by `mempty`.
trimAfter :: Monoid a => Time -> Behavior a -> Behavior a
trimAfter stop x = switch stop x mempty

-- |
-- Instantly switch from one behavior to another with an optinal intermediate value.
--
switch' :: Time -> Behavior a -> Behavior a -> Behavior a -> Behavior a
switch' t rx ry rz = tabulate $ \u -> case u `compare` t of
  LT -> rx ! u
  EQ -> ry ! u
  GT -> rz ! u


{-
import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Compose
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Foldable          (Foldable)
import qualified Data.Foldable          as F
import qualified Data.List              as List
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.NumInstances      ()
import           Data.Profunctor
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Traversable       (Traversable)
import qualified Data.Traversable       as T
import           Data.Typeable
import           Data.VectorSpace

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Time.Types
import           Music.Time.Transform
import           Music.Time.Duration
import           Music.Time.Position
import           Music.Time.Juxtapose
import           Music.Time.Reverse
import           Music.Time.Split

-- Inner TFun focuses on [0..1]
newtype Behavior a = Behavior { getBehavior :: Reactive (Time -> a) }
    deriving (Functor, Semigroup, Monoid)

instance Delayable (Behavior a) where
    delay n (Behavior x) = Behavior (fmap (delay n) $ delay n x)

instance Stretchable (Behavior a) where
    stretch n (Behavior x) = Behavior (fmap (stretch n) $ stretch n x)

instance Wrapped (Behavior a) where
    type Unwrapped (Behavior a) = (Reactive (Time -> a))
    _Wrapped' = iso getBehavior Behavior

instance Applicative Behavior where
    pure    = (^. _Unwrapped') . pure . pure
    ((^. _Wrapped') -> f) <*> ((^. _Wrapped') -> x) = (^. _Unwrapped') $ liftA2 (<*>) f x

instance HasBehavior Behavior where
    (?) = behAt

deriving instance Num a => Num (Behavior a)
deriving instance Fractional a => Fractional (Behavior a)
deriving instance Floating a => Floating (Behavior a)

instance IsPitch a => IsPitch (Behavior a) where
    fromPitch = pure . fromPitch
instance IsInterval a => IsInterval (Behavior a) where
    fromInterval = pure . fromInterval
instance IsDynamics a => IsDynamics (Behavior a) where
    fromDynamics = pure . fromDynamics
instance AdditiveGroup a => AdditiveGroup (Behavior a) where
    zeroV = pure zeroV
    negateV = fmap negateV
    (^+^) = liftA2 (^+^)
instance AffineSpace a => AffineSpace (Behavior a) where
    type Diff (Behavior a) = Behavior (Diff a)
    (.+^) = liftA2 (.+^)
    (.-.) = liftA2 (.-.)




-- | A constant (non-varying) behavior.
--
--   Identical to @behavior . const@ but creates more efficient behaviors.
constant :: a -> Behavior a
constant = (^. _Unwrapped') . pure . pure

-- | Create a behavior from function of (absolute) time.
--
--   You should pass a function defined for the whole range, including negative time.
behavior :: (Time -> a) -> Behavior a
behavior = (^. _Unwrapped') . pure

-- | Create a behaviour from a function of (relative) duration focused on 'sunit'.
--
--   You should pass a function defined for the whole range, including negative durations.
varying :: (Duration -> a) -> Behavior a
varying = varyingIn sunit

-- | Create a behaviour from a function of (relative) duration focused on the given span.
--
--   You should pass a function defined for the whole range, including negative durations.
varyingIn :: Span -> (Duration -> a) -> Behavior a
varyingIn s f = behavior $ sapp (sinvert s) (lmap (.-. start) f)

-- | @b ?? t@ returns the value of the behavior at time @t@.
--  Semantic function.
behAt :: Behavior a -> Time -> a
b `behAt` t = ((^. _Wrapped') b ? t) t

time :: Behavior Time
time = behavior id
-- 
-- trimBeforeB :: Monoid a => Time -> Behavior a -> Behavior a
-- trimBeforeB t = _Wrapping' Behavior %~ trimBefore t
-- 
-- switchB :: Time -> Behavior a -> Behavior a -> Behavior a
-- switchB t ((^. _Wrapped') -> x) ((^. _Wrapped') -> y) = (^. _Unwrapped') $ switch t x y
-- 
-- 
-- 
--                  
-}

tau = 2 * pi

floor' :: RealFrac a => a -> a
floor' = fromIntegral . floor

-- TODO move