
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
        getTimeSignature
  ) where

import Control.Arrow
import Control.Monad.Plus       
import Data.Ratio ((%))
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

-- TODO move
liftRational f = fromRational . f . toRational
liftRational2 f x y = fromRational $ toRational x `f` toRational y

instance Num TimeSignature where
    (+) = liftRational2 (+)
    (*) = liftRational2 (+)
    negate  = liftRational negate
    abs     = liftRational abs
    signum  = liftRational signum
    fromInteger x = TimeSignature ([x], 1)
instance Fractional TimeSignature where
    fromRational (unRatio -> (m, n)) = TimeSignature ([m], n)
instance Real TimeSignature where
    toRational (TimeSignature (xs, x)) = sum xs % x

instance Show TimeSignature where
    show (TimeSignature (xs, x)) = List.intercalate "+" (fmap show xs) ++ "/" ++ show x

time :: Integer -> Integer -> TimeSignature
time x y = TimeSignature ([x], y)

compoundTime :: [Integer] -> Integer -> TimeSignature
compoundTime = curry TimeSignature

timeSignature :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => TimeSignature -> a -> a
timeSignature c x = timeSignatureDuring (era x) c x

timeSignatureDuring :: (HasMeta a, HasPart' a) => Span -> TimeSignature -> a -> a
timeSignatureDuring s c = addGlobalMetaNote (s =: optionLast c)

getTimeSignature :: Score a -> [(Time, TimeSignature)]
getTimeSignature = activeUpdates . fmap unOptionLast . runMeta (Nothing::Maybe Int) . getScoreMeta

withTimeSignature :: TimeSignature -> (TimeSignature -> Score a -> Score a) -> Score a -> Score a
withTimeSignature def f = withGlobalMeta (f . fromMaybe def . unOptionLast)

activeUpdates = fmap (second fromJust) . filter (isJust . snd) . updates
optionLast = Option . Just . Last
unOptionLast = fmap getLast . getOption
