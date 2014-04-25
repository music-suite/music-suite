

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
        -- ** Articulation type functions
        Part,
        SetPart,
        -- ** Accessing parts
        HasParts(..),
        HasPart(..),
        HasPart',
        HasParts',
        part',
        parts',
        -- * Manipulating parts (TODO)
        allParts,
        extractPart,
        extractParts,
        extractParts',

        -- -- * Part representation
        -- Part,
        -- HasPart(..),
        -- HasPart',
        -- PartT(..),
        -- getParts,
  ) where

import           Control.Applicative
import Control.Lens hiding (parts, transform)
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
import qualified Data.List as List

import           Music.Time
import           Music.Score.Util (through)


-- |
-- Parts type.
--
type family Part (s :: *) :: * -- Part s   = a

-- |
-- Part type.
--
type family SetPart (b :: *) (s :: *) :: * -- Part b s = t

-- |
-- Class of types that provide a single part.
--
class (HasParts s t) => HasPart s t where

  -- | Part type.
  part :: Lens s t (Part s) (Part t)

-- |
-- Class of types that provide a part traversal.
--
class (Transformable (Part s),
       Transformable (Part t),
       SetPart (Part t) s ~ t) => HasParts s t where

  -- | Part type.
  parts :: Traversal s t (Part s) (Part t)

-- |
-- Part type.
--
part' :: (HasPart s t, s ~ t) => Lens' s (Part s)
part' = part

-- |
-- Part type.
--
parts' :: (HasParts s t, s ~ t) => Traversal' s (Part s)
parts' = parts

type instance Part Bool = Bool
type instance SetPart a Bool = a
instance (b ~ Part b, Transformable b) => HasPart Bool b where
  part = ($)
instance (b ~ Part b, Transformable b) => HasParts Bool b where
  parts = ($)

type instance Part Ordering = Ordering
type instance SetPart a Ordering = a
instance (b ~ Part b, Transformable b) => HasPart Ordering b where
  part = ($)
instance (b ~ Part b, Transformable b) => HasParts Ordering b where
  parts = ($)

type instance Part () = ()
type instance SetPart a () = a
instance (b ~ Part b, Transformable b) => HasPart () b where
  part = ($)
instance (b ~ Part b, Transformable b) => HasParts () b where
  parts = ($)

type instance Part Int = Int
type instance SetPart a Int = a
instance HasPart Int Int where
  part = ($)
instance HasParts Int Int where
  parts = ($)

type instance Part Integer = Integer
type instance SetPart a Integer = a
instance HasPart Integer Integer where
  part = ($)
instance HasParts Integer Integer where
  parts = ($)

type instance Part Float = Float
type instance SetPart a Float = a
instance HasPart Float Float where
  part = ($)
instance HasParts Float Float where
  parts = ($)

type instance Part (c,a) = Part a
type instance SetPart b (c,a) = (c,SetPart b a)

instance HasPart a b => HasPart (c, a) (c, b) where
  part = _2 . part

instance HasParts a b => HasParts (c, a) (c, b) where
  parts = traverse . parts


type instance Part [a] = Part a
type instance SetPart b [a] = [SetPart b a]

instance HasParts a b => HasParts [a] [b] where
  parts = traverse . parts


type instance Part (Note a) = Part a
type instance SetPart g (Note a) = Note (SetPart g a)

instance (HasPart a b) => HasPart (Note a) (Note b) where
  part = _Wrapped . whilstL part

instance (HasParts a b) => HasParts (Note a) (Note b) where
  parts = _Wrapped . whilstL parts

type HasPart' a = HasPart a a
type HasParts' a = HasParts a a

-- |
-- List all the parts
--
allParts :: (Ord (Part a), HasParts' a) => a -> [Part a]
allParts = List.nub . List.sort . toListOf parts

-- |
-- List all the parts
--
extractPart :: (Eq (Part a), HasPart' a) => Part a -> Score a -> Score a
extractPart = extractPartG

extractPartG :: (Eq (Part a), MonadPlus f, HasPart' a) => Part a -> f a -> f a
extractPartG p x = head $ (\p s -> filterPart (== p) s) <$> [p] <*> return x

-- |
-- List all the parts
--
extractParts :: (Ord (Part a), HasPart' a) => Score a -> [Score a]
extractParts = extractPartsG

extractPartsG
  :: (MonadPlus f,
      HasParts' (f a), HasPart' a, Part (f a) ~ Part a,
      Ord (Part a)) => f a -> [f a]
extractPartsG x = (\p s -> filterPart (== p) s) <$> allParts x <*> return x

filterPart :: (MonadPlus f, HasPart a a) => (Part a -> Bool) -> f a -> f a
filterPart p = mfilter (\x -> p (x ^. part))
                                                    
extractParts' :: (Ord (Part a), HasPart' a) => Score a -> [(Part a, Score a)]
extractParts' x = zip (allParts x) (extractParts x)





{-
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

-}

type instance Part                 (Segment a) = Segment (Part a)
type instance SetPart (Segment g) (Segment a) = Segment (SetPart g a)

instance (HasPart a a, HasPart a b) => HasParts (Segment a) (Segment b) where
  parts = through part part
instance (HasPart a a, HasPart a b) => HasPart (Segment a) (Segment b) where
  part = through part part

type instance Part                 (Behavior a) = Behavior (Part a)
type instance SetPart (Behavior g) (Behavior a) = Behavior (SetPart g a)

instance (HasPart a a, HasPart a b) => HasParts (Behavior a) (Behavior b) where
  parts = through part part
instance (HasPart a a, HasPart a b) => HasPart (Behavior a) (Behavior b) where
  part = through part part

type instance Part (Score a) = Part a
type instance SetPart g (Score a) = Score (SetPart g a)

instance (HasParts a b) => HasParts (Score a) (Score b) where
  parts = _Wrapped . traverse . _Wrapped . whilstL parts
