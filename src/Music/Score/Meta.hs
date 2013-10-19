
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction #-}

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
-- Provides meta-events.
--
-------------------------------------------------------------------------------------


module Music.Score.Meta (
        HasMeta(..),
        
        TimeSignatureT,
        KeySignatureT,
        TempoT
  ) where

import Data.Void
import Data.Maybe
import Data.Foldable
import Data.Typeable
import Data.Semigroup
import Control.Monad.Plus       
import qualified Data.List          as List
import qualified Data.List.NonEmpty as NonEmpty

import Music.Time
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Combinators

class HasMeta a where
    type Meta a
    type NonMeta a
    isMeta :: a -> Bool
    getMeta :: a -> Maybe (Meta a)
    setMeta :: Meta a -> a -> a
    modifyMeta :: (Meta a -> Meta a) -> a -> a
    isMeta = isNothing . getMeta
    setMeta x = modifyMeta (const x)
instance HasMeta (Maybe a) where
    type Meta (Maybe a) = Void
    type NonMeta (Maybe a) = a
    getMeta Nothing  = Nothing
    getMeta (Just _) = Nothing
instance HasMeta (Either m a) where
    type Meta (Either m a) = m
    type NonMeta (Either m a) = a
    getMeta (Left m) = Just m
    getMeta (Right _) = Nothing

-- instance HasChord (ChordT a) where
--     type Note (ChordT a) = a
--     getChord (ChordT as)      = as
-- 
-- -- Actually we should use NonEmpty here
-- -- Empty chords will cause error with HasPitch, among others
-- newtype ChordT a = ChordT { getChordT :: [a] }
--     deriving (Eq, Show, Ord, Monad, Functor, Monoid, Semigroup, Foldable, Typeable)
-- 

newtype MetaT m a = MetaT { getMetaT :: Either m a }
    deriving (Eq, Show, Ord, Monad, Functor, Semigroup, Typeable)

type TimeSignatureT a = MetaT (Integer, Integer) a

type KeySignatureT a = MetaT (Pitch a, Bool)

type TempoT a = MetaT Duration





