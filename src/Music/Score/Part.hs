
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

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
        Part,
        HasPart(..),
        HasPart',
        PartT(..),
        getParts,
  ) where

import           Control.Applicative
import           Control.Comonad
import           Control.Monad.Plus
import           Data.Default
import           Data.Foldable
import qualified Data.List           as List
import           Data.Ord            (comparing)
import           Data.PairMonad
import           Data.Ratio
import           Data.Semigroup
import           Data.Traversable
import           Data.Typeable

import           Music.Time

-- | Associated part type. Should normally implement 'Ord' and 'Show'.
type family Part a :: *


-- |
-- Class of types with an associated part.
--
-- The part type can be any ordered type. A 'Show' instance is also
-- required from printing the name of the part in output.
--
class HasPart a where
    -- | Get the voice of the given note.
    getPart :: a -> Part a

    -- | Set the voice of the given note.
    setPart :: Part a -> a -> a

    -- | Modify the voice of the given note.
    modifyPart :: (Part a -> Part a) -> a -> a

    setPart n      = modifyPart (const n)
    modifyPart f x = x

newtype PartT n a = PartT { getPartT :: (n, a) }
    deriving (Eq, Ord, Show, Functor, Applicative, Comonad, Monad, Typeable)

type instance Part () = Integer
type instance Part Double = Integer
type instance Part Float = Integer
type instance Part Int = Integer
type instance Part Integer = Integer
type instance Part (Ratio a) = Integer

instance HasPart ()                         where { getPart _ = def }
instance HasPart Double                     where { getPart _ = def }
instance HasPart Float                      where { getPart _ = def }
instance HasPart Int                        where { getPart _ = def }
instance HasPart Integer                    where { getPart _ = def }
instance Integral a => HasPart (Ratio a)    where { getPart _ = def }

type instance Part (PartT n a)       = n
instance HasPart (PartT n a) where
    getPart (PartT (v,_))       = v
    modifyPart f (PartT (v,x))  = PartT (f v, x)

type instance Part (a,b)    = Part b
instance HasPart b => HasPart (a,b) where
    getPart (a,b)      = getPart b
    modifyPart f (a,b) = (a, modifyPart f b)

-- |
-- Like 'HasPart', but enforces the part to be ordered.
-- This is usually required for part separation and traversal.
--
type HasPart' a = (Show (Part a), Ord (Part a), Default (Part a), HasPart a)

-- TODO unify with class above as in Pitch?

-- |
-- Get all parts in the given score. Returns a list of parts.
--
-- > Score a -> [Part]
--
getParts :: (Foldable t, HasPart' a) => t a -> [Part a]
getParts = List.sort . List.nub . fmap getPart . toList

