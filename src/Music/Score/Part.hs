
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    OverloadedStrings,
    GeneralizedNewtypeDeriving #-}

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
-- Provides partwise traversal, part composition and extraction.
--
-------------------------------------------------------------------------------------


module Music.Score.Part (
        HasPart(..),
        HasPart',
        -- PartName(..),
        PartT(..),
  ) where

import Control.Monad (ap, mfilter, join, liftM, MonadPlus(..))
import Data.Semigroup
import Data.String
import Data.Foldable
import Data.Typeable
import Data.Ord (comparing)
import Data.Traversable
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace
import Data.Ratio

import Music.Time

-- |
-- Class of types with an associated part.
--
-- The part type can be any type that is orddered.
--
class HasPart a where
    -- | Associated part type. Should implement 'Ord' and 'Show'.
    type Part a :: *

    -- | Get the voice of the given note.
    getPart :: a -> Part a

    -- | Set the voice of the given note.
    setPart :: Part a -> a -> a

    -- | Modify the voice of the given note.
    modifyPart :: (Part a -> Part a) -> a -> a

    setPart n = modifyPart (const n)
    modifyPart f x = x

newtype PartT n a = PartT { getPartT :: (n, a) }
    deriving (Eq, Ord, Show, Functor, Typeable)

instance HasPart ()                            where   { type Part ()         = Integer ; getPart _ = 0 }
instance HasPart Double                        where   { type Part Double     = Integer ; getPart _ = 0 }
instance HasPart Float                         where   { type Part Float      = Integer ; getPart _ = 0 }
instance HasPart Int                           where   { type Part Int        = Integer ; getPart _ = 0 }
instance HasPart Integer                       where   { type Part Integer    = Integer ; getPart _ = 0 }
instance Integral a => HasPart (Ratio a)       where   { type Part (Ratio a)  = Integer ; getPart _ = 0 }

-- |
-- Like 'HasPart', but enforces the part to be ordered.
-- This is usually required for part separation and traversal.
--
type HasPart' a = (Ord (Part a), HasPart a)


