
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

module Music.Score.Meta.Key (
        -- * Key signature type
        Fifths,
        KeySignature,
        key,
        isMajorKey,
        isMinorKey,

        -- * Adding key signatures to scores
        keySignature,
        keySignatureDuring,
        
        -- * Extracting key signatures
        withKeySignature,
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

newtype Fifths = Fifths Integer
    deriving (Eq, Ord, Num, Enum, Integral, Real)

instance IsPitch Fifths where
    fromPitch (PitchL (d, fromMaybe 0 -> c, _)) = case (d,c) of
        (0,-1) -> (-7)
        (0, 0) -> 0
        (0, 1) -> 7
        
        (1,-1) -> (-5)
        (1, 0) -> 2
        (1, 1) -> 9
        
        (2,-1) -> (-3)
        (2, 0) -> 4
        (2, 1) -> 11
        
        (3,-1) -> (-8)
        (3, 0) -> (-1)
        (3, 1) -> 6
        
        (4,-1) -> (-6)
        (4, 0) -> 1
        (4, 1) -> 8
        
        (5,-1) -> (-4)
        (5, 0) -> 3
        (5, 1) -> 10

        (6,-1) -> (-2)
        (6, 0) -> 5
        (6, 1) -> 12

        _      -> error "Strange number of Fifths"

-- | A key signature, represented by number of fifths from C and mode.
newtype KeySignature = KeySignature (Fifths, Bool)
    deriving (Eq, Ord, Typeable)

-- | Create a major or minor signature.
key :: Fifths -> Bool -> KeySignature
key fifths mode = KeySignature (fifths, mode)

isMajorKey :: KeySignature -> Bool
isMajorKey (KeySignature (_,x)) = x

isMinorKey :: KeySignature -> Bool
isMinorKey = not . isMajorKey

-- | Set the key signature of the given score.
keySignature :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => KeySignature -> a -> a
keySignature c x = keySignatureDuring (era x) c x

-- | Set the key signature of the given part of a score.
keySignatureDuring :: (HasMeta a, HasPart' a) => Span -> KeySignature -> a -> a
keySignatureDuring s c = addGlobalMetaNote (s =: (Option $ Just $ Last c))

-- | Extract all key signatures from the given score, using the given default key signature. 
withKeySignature :: KeySignature -> (KeySignature -> Score a -> Score a) -> Score a -> Score a
withKeySignature def f = withGlobalMeta (f . fromMaybe def . fmap getLast . getOption)

