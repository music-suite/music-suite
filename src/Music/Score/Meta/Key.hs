
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

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
-- Provides key signature meta-data.
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

import           Control.Lens              (view)
import           Control.Monad.Plus
import           Data.Foldable             (Foldable)
import qualified Data.Foldable             as F
import qualified Data.List                 as List
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.String
import           Data.Traversable          (Traversable)
import qualified Data.Traversable          as T
import           Data.Typeable

import           Music.Pitch.Literal
import           Music.Score.Meta
import           Music.Score.Part
import           Music.Score.Pitch
import           Music.Score.Internal.Util
import           Music.Time
import           Music.Time.Reactive

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
keySignature :: (HasMeta a, HasPosition a) => KeySignature -> a -> a
keySignature c x = keySignatureDuring (_era x) c x

-- | Set the key signature of the given part of a score.
keySignatureDuring :: HasMeta a => Span -> KeySignature -> a -> a
keySignatureDuring s c = addMetaNote $ view event (s, (Option $ Just $ Last c))

-- | Extract all key signatures from the given score, using the given default key signature.
withKeySignature :: KeySignature -> (KeySignature -> Score a -> Score a) -> Score a -> Score a
withKeySignature def f = withMeta (f . fromMaybe def . fmap getLast . getOption)

