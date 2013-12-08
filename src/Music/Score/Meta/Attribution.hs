
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

module Music.Score.Meta.Attribution (
        
        -- * Attribution type
        Attribution,
        -- attributions,
        attribution,
        getAttribution,

        -- ** Adding attribution to scores
        composer,
        composerDuring,
        lyricist,
        lyricistDuring,
        arranger,
        arrangerDuring,

        -- ** Extracting attribution
        withAttribution,
        withAttribution',
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


-- | 
-- An attributions is a simple key-value store used to gather information such
-- as composer, lycicist, orchestrator, performer, etc.
-- 
newtype Attribution = Attribution (Map String (Option (Last String)))
    deriving (Typeable, Monoid, Semigroup)

instance Show Attribution where
    show (Attribution a) = "attributions " ++ show (Map.toList (fmap (fmap getLast . getOption) $ a))

-- | Make an 'Attribution' from keys and values.
attributions :: [(String, String)] -> Attribution
attributions = Attribution . fmap (Option . Just . Last) . Map.fromList

-- | Make an 'Attribution' a single key and value.
attribution :: String -> String -> Attribution
attribution k v = Attribution . fmap (Option . Just . Last) $ Map.singleton k v

-- | Extract an the given attributions value. Semantic function.
getAttribution :: Attribution -> String -> Maybe String
getAttribution (Attribution a) k = join $ k `Map.lookup` (fmap (fmap getLast . getOption) $ a)


-- | Set composer of the given score.
composer :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => String -> a -> a
composer t x = composerDuring (era x) t x

-- | Set composer of the given part of a score.
composerDuring :: (HasMeta a, HasPart' a) => Span -> String -> a -> a
composerDuring s c = addGlobalMetaNote (s =: attribution "composer" c)

-- | Set lyricist of the given score.
lyricist :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => String -> a -> a
lyricist t x = lyricistDuring (era x) t x

-- | Set lyricist of the given part of a score.
lyricistDuring :: (HasMeta a, HasPart' a) => Span -> String -> a -> a
lyricistDuring s c = addGlobalMetaNote (s =: attribution "lyricist" c)

-- | Set arranger of the given score.
arranger :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => String -> a -> a
arranger t x = arrangerDuring (era x) t x

-- | Set arranger of the given part of a score.
arrangerDuring :: (HasMeta a, HasPart' a) => Span -> String -> a -> a
arrangerDuring s c = addGlobalMetaNote (s =: attribution "arranger" c)

withAttribution :: String -> (String -> Score a -> Score a) -> Score a -> Score a
withAttribution name f = withAttribution' (fromMaybe id . fmap f . flip getAttribution name)

withAttribution' :: (Attribution -> Score a -> Score a) -> Score a -> Score a
withAttribution' = withGlobalMetaAtStart
