
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
-- Provides functions for manipulating parts.
--
-------------------------------------------------------------------------------------


module Music.Score.Part (
        -- * Part representation
        HasPart(..),
        HasPart',
        PartT(..),
        getParts,
        setParts,
  ) where

import Control.Monad.Plus
import Data.Default
import Data.Ord (comparing)
import Data.Semigroup
import Data.Ratio
import Data.Foldable
import Data.Traversable
import Data.Typeable
import qualified Data.List as List

import Music.Time

-- |
-- Class of types with an associated part.
--
-- The part type can be any ordered type. A 'Show' instance is also
-- required from printing the name of the part in output.
--
class HasPart a where

    -- | Associated part type. Should normally implement 'Ord' and 'Show'.
    type Part a :: *

    -- | Get the voice of the given note.
    getPart :: Default (Part a) => a -> Part a

    -- | Set the voice of the given note.
    setPart :: Part a -> a -> a

    -- | Modify the voice of the given note.
    modifyPart :: (Part a -> Part a) -> a -> a

    setPart n      = modifyPart (const n)
    modifyPart f x = x

newtype PartT n a = PartT { getPartT :: (n, a) }
    deriving (Eq, Ord, Show, Functor, Typeable)

instance HasPart ()                         where { type Part ()         = Integer ; getPart _ = def }
instance HasPart Double                     where { type Part Double     = Integer ; getPart _ = def }
instance HasPart Float                      where { type Part Float      = Integer ; getPart _ = def }
instance HasPart Int                        where { type Part Int        = Integer ; getPart _ = def }
instance HasPart Integer                    where { type Part Integer    = Integer ; getPart _ = def }
instance Integral a => HasPart (Ratio a)    where { type Part (Ratio a)  = Integer ; getPart _ = def }

instance HasPart (PartT n a) where
    type Part (PartT n a)       = n
    getPart (PartT (v,_))       = v
    modifyPart f (PartT (v,x))  = PartT (f v, x)

-- |
-- Like 'HasPart', but enforces the part to be ordered.
-- This is usually required for part separation and traversal.
--
type HasPart' a = (Ord (Part a), Default (Part a), HasPart a)

-- |
-- Get all parts in the given score. Returns a list of parts.
--
-- > Score a -> [Part]
--    
getParts :: (Performable a, HasPart' e, e ~ Event a) => a -> [Part e]
getParts = List.sort . List.nub . fmap getPart . performValues

-- |
-- Set all parts in the given score.
--
-- > Part -> Score a -> Score a
--
setParts :: (HasPart a, Functor s) => Part a -> s a -> s a
setParts n = fmap (setPart n)



