
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
-- Provides reversible values.
--
-------------------------------------------------------------------------------------

module Music.Time.Reverse (
        module Music.Time.Position,

        -- * The Reversible class
        Reversible(..),

        -- * Reversing
        reversed,
        revDefault,

        -- * Utility
        NoReverse(..),
  ) where

import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Ratio
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.VectorSpace

import           Music.Time.Position

import           Control.Lens           hiding (Indexable, Level, above, below,
                                         index, inside, parts, reversed,
                                         transform, (<|), (|>))
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Semigroup         hiding ()
import           Data.Sequence          (Seq)
import qualified Data.Sequence          as Seq
import           Data.Typeable
import           Data.VectorSpace       hiding (Sum (..))

-- |
-- Class of values that can be reversed (retrograded).
--
-- For positioned values succh as 'Note', the value is reversed relative to its middle point, i.e.
-- the onset value becomes the offset value and vice versa.
--
-- For non-positioned values such as 'Stretched', the value is reversed in-place.
--
-- FIXME Second law is incompatible with 'revDefault' (and the 'Span' definition below)
--
-- Law
--
-- @
-- 'rev' ('rev' a) = a
-- @
--
-- @
-- 'abs' ('_duration' x) = _duration ('rev' x)
-- @
--
-- @
-- 'rev' s `transform` a = 'rev' (s `transform` a)
-- @
--
-- or equivalently,
--
-- @
-- 'transform' . 'rev' = 'fmap' 'rev' . 'transform'
-- @
--
-- For 'Span'
--
-- @
-- 'rev' = 'over' 'range' 'swap'
-- @
--
class Transformable a => Reversible a where

  -- | Reverse (retrograde) the given value.
  rev :: a -> a

--
-- XXX Counter-intuitive Behavior instances (just Behavior should reverse around origin,
-- while Bound (Behavior a) should reverse around the middle, like a note)
--

--
-- XXX Alternate formulation of second Reversiblee law
--
--     rev s `transform` a     = rev (s `transform` a)
-- ==> (rev s `transform`)     = rev . (s `transform`)
-- ==> transform (rev s)       = rev . (transform s)
-- ==> (transform . rev) s     = (rev .) (transform s)
-- ==> (transform . rev) s     = fmap rev (transform s)
-- ==> transform . rev         = fmap rev . transform
--

instance Reversible () where
  rev = id

instance Reversible Int where
  rev = id

instance Reversible Double where
  rev = id

instance Reversible Integer where
  rev = id

instance Reversible a => Reversible [a] where
  rev = reverse . map rev

instance Reversible a => Reversible (Seq a) where
  rev = Seq.reverse . fmap rev

instance (Ord k, Reversible a) => Reversible (Map k a) where
  rev = Map.map rev

instance Reversible Duration where
  rev = stretch (-1)

--
-- There is no instance for Reversible Time
-- as we can not satisfy the second Reversible law
--

instance Reversible Span where
  rev = revDefault

instance Reversible a => Reversible (b, a) where
  rev (s,a) = (s, rev a)

-- |
-- A default implementation of 'rev'
--
revDefault :: (HasPosition a, Transformable a) => a -> a
-- revDefault x = (stretch (-1) `whilst` undelaying (_position x 0.5 .-. 0)) x
revDefault x = stretch (-1) x


newtype NoReverse a = NoReverse { getNoReverse :: a }
  deriving (Typeable, Eq, Ord, Show)

instance Transformable (NoReverse a) where
  transform _ = id

instance Reversible (NoReverse a) where
  rev = id

-- |
-- View the reverse of a value.
--
-- >>> [1,2,3] & reversed %~ Data.List.sort
-- [3,2,1]
--
reversed :: Reversible a => Iso' a a
reversed = iso rev rev





{-
-- |
-- Reversible values.
--
-- For instances of 'Reversible' and 'HasOnset', the following laws should hold:
--
-- > onset a    = onset (rev a)
-- > duration a = duration (rev a)
--
-- For structural types, 'rev' is applied recursively, hence the constraint on
-- the 'Score' instance. 'rev' is id by default, so for a trivial type @T@ it
-- suffices to write
--
-- > instance Reversible T
--
-- For instances 'U' of 'HasOnset' and 'Transformable', a suitable instance
-- is
--
-- > instance Reversible T where
-- >     rev = withSameOnset (stretch (-1))
--
--

class Reversible a where

    -- |
    -- Reverse a value.
    --
    -- Reverse is an involution, meaning that:
    --
    -- > rev (rev a) = a
    --
    rev :: a -> a
    rev = id

-- instance Reversible Time where
    -- rev t = mirror t

instance Reversible Double
instance Reversible Float
instance Reversible Int
instance Reversible Integer
instance Reversible ()
instance Reversible (Ratio a)

instance Reversible a => Reversible [a] where
    rev = fmap rev

instance (Ord a, Reversible a) => Reversible (Set a) where
    rev = Set.map rev

instance Reversible a => Reversible (Map k a) where
    rev = fmap rev

newtype NoRev a = NoRev { getNoRev :: a }
    deriving (Eq, Ord, Enum, Show, Semigroup, Monoid,
        Delayable, Stretchable, HasOnset, HasOffset, HasDuration)

instance Reversible (NoRev a) where
    rev = id



newtype WithRev a = WithRev (a,a)
    deriving (Eq, Ord, Semigroup, Monoid)

withRev :: Reversible a => a -> WithRev a
withRev x = WithRev (rev x, x)

fromWithRev :: Reversible a => WithRev a -> a
fromWithRev (WithRev (_,x)) = x

instance Reversible a => Reversible (WithRev a) where
    rev (WithRev (r,x)) = WithRev (x,r)

-- JUNK
                                         -}


