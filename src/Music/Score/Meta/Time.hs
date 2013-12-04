
{-# LANGUAGE 
    ScopedTypeVariables, 
    GeneralizedNewtypeDeriving,
    DeriveFunctor, 
    DeriveFoldable, 
    DeriveTraversable,
    DeriveDataTypeable, 
    ConstraintKinds,
    FlexibleContexts, 
    GADTs, 
    ViewPatterns,
    TypeFamilies,
    MultiParamTypeClasses, 
    FlexibleInstances #-}

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
-------------------------------------------------------------------------------------

module Music.Score.Meta.Time (
        
        TimeSignature,
        time,
        compoundTime,

        timeSignature,
        timeSignatureDuring,

        withTimeSignature,
  ) where

import Control.Arrow
import Control.Monad.Plus       
import Data.Void
import Data.Maybe
import Data.Semigroup
import Data.Monoid.WithSemigroup
import Data.Typeable
import Data.String
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Music.Time
import Music.Time.Reactive
import Music.Score.Note
import Music.Score.Voice
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Meta
import Music.Score.Score
import Music.Score.Combinators
import Music.Score.Util
import Music.Pitch.Literal

newtype TimeSignature = TimeSignature ([Integer], Integer)
    deriving (Eq, Ord, Typeable)

instance Show TimeSignature where
    show (TimeSignature (xs, x)) = List.intercalate "+" (fmap show xs) ++ "/" ++ show x

time :: Integer -> Integer -> TimeSignature
time x y = TimeSignature ([x], y)

compoundTime :: [Integer] -> Integer -> TimeSignature
compoundTime = curry TimeSignature

timeSignature :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => TimeSignature -> a -> a
timeSignature c x = timeSignatureDuring (era x) c x

timeSignatureDuring :: (HasMeta a, HasPart' a) => Span -> TimeSignature -> a -> a
timeSignatureDuring s c = addGlobalMetaNote (s =: (Option $ Just $ Last c))

withTimeSignature :: TimeSignature -> (TimeSignature -> Score a -> Score a) -> Score a -> Score a
withTimeSignature def f = withGlobalMeta (f . fromMaybe def . fmap getLast . getOption)

