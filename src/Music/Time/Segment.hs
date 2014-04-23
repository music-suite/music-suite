
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


module Music.Time.Segment (
  ) where

import Data.Clipped
import Data.Functor.Rep.Lens
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Ratio
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.VectorSpace

import           Music.Time.Types
import           Music.Time.Transform
import           Music.Time.Position
import           Music.Time.Duration
import           Music.Time.Split
import           Music.Time.Reverse
import           Music.Time.Bound
import           Music.Time.Behavior

-----
import Data.Fixed
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

{-
-- |
-- A behavior that gives the current time, i.e. the identity function
-- 
timeS :: Floating a => Segment a
timeS = realToFrac^.segment

sineS :: Floating a => Segment a
#ifdef INCLUDE_LIFTED
sineS = sin (timeS*tau)
#else
sineS = undefined
#endif
-}

apSegments' :: Stretched (Segment a) -> Stretched (Segment a) -> Stretched (Segment a)
apSegments' (Stretched (d1,s1)) (Stretched (d2,s2)) = Stretched (d1+d2, slerp (d1/(d1+d2)) s1 s2)

-- |
-- Append a voice of segments to a single stretched segment.
--
apSegments :: Voice (Segment a) -> Stretched (Segment a)
apSegments = foldr1 apSegments' . toListOf voiceElements

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
