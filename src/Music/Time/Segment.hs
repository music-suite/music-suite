
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

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

module Music.Time.Segment (
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
    splice,
    trim,
    trimBefore,
    trimAfter,
    concatB,

    -- * Common behaviors
    line,
    unit,
    impulse,
    turnOn,
    turnOff,
    sawtooth,
    sine,
    cosine,

    -- * Music.Time.Segment
    Segment,
    -- ** Examples
    -- $XXmusicTimeSegmentExamples
    segment,

    -- ** Combinators
    focusing,
    apSegments',
    apSegments,
    -- concatS,

    Bound,
    bounds,
    bounding,
    trim,
    splice,
    bounded',
    bounded,
  ) where

import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Clipped
import           Data.Functor.Rep.Lens
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Ratio
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.VectorSpace

import           Music.Time.Behavior
import           Music.Time.Bound
import           Music.Time.Note
import           Music.Time.Reverse
import           Music.Time.Score
import           Music.Time.Split
import           Music.Time.Stretched
import           Music.Time.Voice

import           Control.Applicative
import           Control.Arrow          (first, second, (&&&), (***))
import           Control.Lens           hiding (Indexable, Level, above, below,
                                         index, inside, parts, reversed,
                                         transform, (<|), (|>))
import           Data.Distributive
import           Data.Functor.Rep
import           Data.Functor.Rep.Lens
import           Data.Maybe
import           Data.Typeable
import           Music.Dynamics.Literal
import           Music.Pitch.Literal


-- TODO Compare Diagram's Trail and Located (and see the conal blog post)

-- |
--
-- A 'Segment' is a value varying over some unknown time span.
--
-- To give a segment an explicit duration, use 'Stretched' 'Segment'.
--
-- To place a segment in a particular time span, use 'Note' 'Segment'.
--
-- The semantics are given by
--
-- @
-- type Segment a = 'Duration' -> a
-- @
--
newtype Segment a = Segment { getSegment :: Clipped Duration -> a }
  deriving (Functor, Applicative, Monad{-, Comonad-})

--
-- TODO constant-optimize a la Conal
--

-- $musicTimeSegmentExamples
--
-- > foldr1 apSegments' $ map (view stretched) $ [(0.5,0::Segment Float), (1, timeS), (2,rev timeS), (3,-1)]
--
-- > openG $ draw $ (1, timeS :: Segment Float)^.stretched
--

instance Show (Segment a) where
  show _ = "<<Segment>>"

deriving instance Typeable1 Segment
deriving instance Distributive Segment

instance Representable Segment where
  type Rep Segment = Duration
  tabulate f = Segment (f . fromClipped)
  index    (Segment f) = f . unsafeToClipped

-- |
-- Segments are /invariant/ under transformation. To transform a timve varying value, use
-- 'fromSegment'.
--
instance Transformable (Segment a) where
  transform _ = id

instance Reversible (Segment a) where
  -- TODO in terms of Representable
  rev (Segment f) = Segment (f . unsafeToClipped . r . fromClipped)
    where
      r x = (x * (-1)) + 1

-- TODO
-- type instance Pitch                 (Segment a) = Segment (Pitch a)
-- type instance SetPitch (Segment g)  (Segment a) = Segment (SetPitch g a)
--
-- instance (HasPitch a a, HasPitch a b) => HasPitches (Segment a) (Segment b) where
--   pitches = through pitch pitch
-- instance (HasPitch a a, HasPitch a b) => HasPitch (Segment a) (Segment b) where
--   pitch = through pitch pitch
--
-- type instance Dynamic                 (Segment a) = Segment (Dynamic a)
-- type instance SetDynamic (Segment g) (Segment a) = Segment (SetDynamic g a)
--
-- instance (HasDynamic a a, HasDynamic a b) => HasDynamics (Segment a) (Segment b) where
--   dynamics = through dynamic dynamic
-- instance (HasDynamic a a, HasDynamic a b) => HasDynamic (Segment a) (Segment b) where
--   dynamic = through dynamic dynamic
--
--
-- type instance Articulation                 (Segment a) = Segment (Articulation a)
-- type instance SetArticulation (Segment g) (Segment a) = Segment (SetArticulation g a)
--
-- instance (HasArticulation a a, HasArticulation a b) => HasArticulations (Segment a) (Segment b) where
--   articulations = through articulation articulation
-- instance (HasArticulation a a, HasArticulation a b) => HasArticulation (Segment a) (Segment b) where
--   articulation = through articulation articulation
--
--
-- type instance Part                 (Segment a) = Segment (Part a)
-- type instance SetPart (Segment g) (Segment a) = Segment (SetPart g a)
--
-- instance (HasPart a a, HasPart a b) => HasParts (Segment a) (Segment b) where
--   parts = through part part
-- instance (HasPart a a, HasPart a b) => HasPart (Segment a) (Segment b) where
--   part = through part part

#ifdef INCLUDE_LIFTED
deriving instance Semigroup a => Semigroup (Segment a)
deriving instance Monoid a => Monoid (Segment a)
deriving instance Num a => Num (Segment a)
deriving instance Fractional a => Fractional (Segment a)
deriving instance Floating a => Floating (Segment a)

instance IsPitch a => IsPitch (Segment a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Segment a) where
  fromInterval = pure . fromInterval

instance Alterable a => Alterable (Segment a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Augmentable a => Augmentable (Segment a) where
    augment = fmap augment
    diminish = fmap diminish

instance Eq a => Eq (Segment a) where
  (==) = error "No fun"

instance Ord a => Ord (Segment a) where
  (<) = error "No fun"
  max = liftA2 max
  min = liftA2 min
#endif

-- |
-- View a segment as a time function and vice versa.
--
segment :: Iso (Duration -> a) (Duration -> b) (Segment a) (Segment b)
segment = tabulated

apSegments' :: Stretched (Segment a) -> Stretched (Segment a) -> Stretched (Segment a)
apSegments' (view (from stretched) -> (d1,s1)) (view (from stretched) -> (d2,s2))
  = view stretched (d1+d2, slerp (d1/(d1+d2)) s1 s2)

-- |
-- Append a voice of segments to a single stretched segment.
--
apSegments :: Voice (Segment a) -> Stretched (Segment a)
apSegments = foldr1 apSegments' . toListOf (stretcheds . each)

-- t < i && 0 <= t <= 1   ==> 0 < (t/i) < 1
-- i     is the fraction of the slerped segment spent in a
-- (1-i) is the fraction of the slerped segment spent in b
slerp :: Duration -> Segment a -> Segment a -> Segment a
slerp i a b
  | i < 0 || i >= 1    = error "slerp: Bad value"
  | otherwise = tabulate $ \t -> if t < i then a ! (t/i) else b ! ((t-i)/(1-i))

slerp2 :: (a -> a -> a) -> Duration -> Segment a -> Segment a -> Segment a
slerp2 f i a b
  | i < 0 || i >= 1    = error "slerp: Bad value"
  | otherwise = tabulate $ \t -> case t `compare` i of
      LT -> a ! (t/i)
      EQ -> (a ! 1) `f` (b ! 1)
      GT -> b ! ((t-i)/(1-i))


-- |
-- View a 'Note' 'Segment' as a 'Bound' 'Behavior' and vice versa.
--
-- This can be used to safely turn a behavior into a segment and vice
-- versa. Often 'focusing' is more convenient to use.
--
bounded' :: Iso'
  (Note (Segment a))
  (Bound (Behavior a))
bounded' = bounded

-- |
-- View a 'Note' 'Segment' as a 'Bound' 'Behavior' and vice versa.
--
-- This can be used to safely turn a behavior into a segment and vice
-- versa. Often 'focusing' is more convenient to use.
--
bounded :: Iso
  (Note (Segment a))
  (Note (Segment b))
  (Bound (Behavior a))
  (Bound (Behavior b))
bounded = iso ns2bb bb2ns
  where
    bb2ns (Bound (s, x)) = view note (s, b2s $ transform (negateV s) $ x)
    ns2bb (view (from note) -> (s, x)) = Bound (s,       transform s           $ s2b $ x)
    s2b = under tabulated (. realToFrac)
    b2s = under tabulated (. realToFrac)

--
-- Note that the isomorhism only works because of 'Bound' being abstract.
-- A function @unBound :: Bound a -> a@ could break the isomorphism
-- as follows:
--
-- > (unBound . view (from bounded . bounded) . bounds 0 1) b ! 2
-- *** Exception: Outside 0-1
--

-- |
-- Extract a bounded behavior, replacing all values outside the bound with 'mempty'.
--
-- @
-- 'trim'   = 'splice' 'mempty'
-- 'trim' x = 'trimBefore' '_onset' x . 'trimAfter' '_offset' x
-- @
--
trim :: Monoid b => Bound (Behavior b) -> Behavior b
trim = trimG
  where
    trimG :: (Monoid b, Representable f, Rep f ~ Time) => Bound (f b) -> f b
    trimG (Bound (s, x)) = tabulate (trimOutside s) `apRep` x

trimOutside :: Monoid a => Span -> Time -> a -> a
trimOutside s t x = if t `inside` s then x else mempty

-- |
-- Inserts a bounded behavior on top of another behavior.
--
-- @
-- 'trim' = 'splice' 'mempty'
-- @
--
-- (Named after the analogous tape-editing technique.)
--
splice :: Behavior a -> Bound (Behavior a) -> Behavior a
splice constant insert = fmap fromLast $ fmap toLast constant <> trim (fmap (fmap toLast) insert)
  where
    toLast   = Option . Just . Last
    fromLast = getLast . fromJust . getOption
    -- fromJust is safe here, as toLast is used to create the Maybe wrapper


concatSegment :: Monoid a => Note (Segment a) -> Behavior a
concatSegment = trim . view bounded

-- |
-- Concatenate a score of (possibly overlapping) segments.
--
-- See also 'concatB' and 'continous'.
--
concatS :: Monoid a => Score (Segment a) -> Behavior a
concatS = mconcat . map concatSegment . view notes
-- Or: mconcat.fmap trim.toListOf (notes.each.bounded)

-- |
-- Concatenate a score of (possibly overlapping) segments.
--
-- See also 'concatSegment' and 'continous'.
--
concatB :: Monoid a => Score (Behavior a) -> Behavior a
concatB = concatS . fmap (view focusing)
-- Or (more generally): mconcat.toListOf (notes.each.noteValue)


-- |
-- View part of a 'Behavior' as a 'Segment'.
--
-- @
-- 'line' & 'focusing' ``on`` (2 '<->' 3) '*~' 0
-- @
--
focusing :: Lens' (Behavior a) (Segment a)
focusing = lens get set
  where
    get = view (from bounded . noteValue) . {-pure-}bounding mempty
    set x = splice x . (view bounded) . pure


