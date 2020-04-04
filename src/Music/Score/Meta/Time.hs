{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

-------------------------------------------------------------------------------------

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
-- Provides time signatures and related meta-data.
module Music.Score.Meta.Time
  ( -- * Time signature type
    TimeSignature (..),
    time,
    compoundTime,
    isSimpleTime,
    isCompoundTime,
    toSimpleTime,

    -- * Adding time signature to scores
    timeSignature,
    timeSignatureDuring,

    -- * Utility
    standardTimeSignature,
  )
where

import Control.Lens ((^.), view)
import Control.Monad.Plus
import Data.Bifunctor
import Data.Bits ((.&.))
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ratio ((%))
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Typeable
import Music.Pitch.Literal
import Music.Score.Internal.Util
import Music.Score.Meta
import Music.Score.Part
import Music.Score.Pitch
import Music.Time

-- |
-- A time signature is a sequence of beat numbers and a note value (i.e. an expression on the
-- form @(a1+a2...)/b@). For simple time signatures just one beat number is used.
--
-- TimeSignature is an instance of 'Fractional' and can be used as
-- follows:
--
-- > timeSignature (4/4)
-- > timeSignature (6/8)
-- > timeSignature ((3+2)/4)
newtype TimeSignature = TimeSignature {getTimeSignature :: ([Integer], Integer)}
  deriving (Eq, Ord, Typeable)

mapNums f (TimeSignature (m, n)) = TimeSignature (f m, n)

mapDenom f (TimeSignature (m, n)) = TimeSignature (m, f n)

isSimple (TimeSignature ([_], _)) = True
isSimple _ = False

getSimple (TimeSignature ([m], n)) = m `div` n
getSimple _ = error "getSimple: Not a simple time signature"

-- TODO move
liftRational f = fromRational . f . toRational

liftRational2 f x y = fromRational $ toRational x `f` toRational y

instance Num TimeSignature where

  x + y
    | x `haveSameDenominator` y = concatFrac x y
    | otherwise = liftRational2 (+) x y
    where
      TimeSignature (_, n1) `haveSameDenominator` TimeSignature (_, n2) = n1 == n2
      TimeSignature (m1, n) `concatFrac` TimeSignature (m2, _) = TimeSignature (m1 <> m2, n)

  x * y
    | isSimple y = mapNums (fmap (* (getSimple y))) x
    | otherwise = liftRational2 (*) x y

  negate = liftRational negate

  abs = liftRational abs

  signum = liftRational signum

  fromInteger x = TimeSignature ([x], 1)

instance Fractional TimeSignature where

  fromRational (unRatio -> (m, n)) = TimeSignature ([m], n)

  x / y
    | isSimple y = mapDenom (* (getSimple y)) x
    | otherwise = liftRational2 (/) x y

instance Real TimeSignature where
  toRational (TimeSignature (xs, x)) = sum xs % x

instance Show TimeSignature where
  show (TimeSignature ([m], n)) = show m ++ "/" ++ show n
  show (TimeSignature (xs, n)) = "(" ++ List.intercalate "+" (fmap show xs) ++ ")/" ++ show n

-- | Create a simple time signature.
time :: Integer -> Integer -> TimeSignature
time x y = TimeSignature ([x], y)

-- | Create a compound time signature.
compoundTime :: [Integer] -> Integer -> TimeSignature
compoundTime = curry TimeSignature

-- | Whether this is a simple time signature.
isSimpleTime :: TimeSignature -> Bool
isSimpleTime (TimeSignature ([_], _)) = True
isSimpleTime _ = False

-- | Whether this is a compound time signature.
isCompoundTime :: TimeSignature -> Bool
isCompoundTime = not . isSimpleTime

-- | Convert to a simple time signature by adding all numerators.
--   If given a simple time signature, returns it.
toSimpleTime :: TimeSignature -> TimeSignature
toSimpleTime = fromRational . toRational

-- | Set the time signature of the given score.
timeSignature :: (HasMeta a, HasPosition a, Transformable a) => TimeSignature -> a -> a
timeSignature c x = case _era x of
  Nothing -> x
  Just e -> timeSignatureDuring e c x

-- use (x^.onset <-> x^.offset) instead of (0 <-> x^.offset)
-- timeSignature' c x = timeSignatureDuring (era x) c x

-- | Set the time signature of the given part of a score.
timeSignatureDuring :: HasMeta a => Span -> TimeSignature -> a -> a
timeSignatureDuring s c = addMetaNote $ view event (s, optionLast c)

-- | Time signature typically used for the given duration.
--
-- Returns Nothing if the denominator of the canonical form of given duration is not a power of two.
--
-- TODO partial
standardTimeSignature :: Duration -> Maybe TimeSignature
standardTimeSignature x = case unRatio (toRational x) of
  -- (1,2) -> time 1 2
  (2, 2) -> pure $ time 2 2
  (3, 2) -> pure $ time 3 2
  (2, 1) -> pure $ time 4 2
  (5, 2) -> pure $ time 5 2
  (3, 1) -> pure $ time 6 2
  (7, 2) -> pure $ time 7 2
  (1, 4) -> pure $ time 1 4
  (1, 2) -> pure $ time 2 4
  (3, 4) -> pure $ time 3 4
  (1, 1) -> pure $ time 4 4
  (5, 4) -> pure $ time 5 4
  -- (3,2) -> pure $ time 6 4
  (7, 4) -> pure $ time 7 4
  (1, 8) -> pure $ time 1 8
  -- (1,4) -> pure $ time 2 8
  (3, 8) -> pure $ time 3 8
  -- (1,2) -> pure $ time 4 8
  (5, 8) -> pure $ time 5 8
  -- (3,4) -> pure $ time 6 8
  (7, 8) -> pure $ time 7 8
  -- TODO check divisible by 8 etc
  (m, n)
    | isPowerOfTwo n -> pure $ time m n
    | otherwise -> Nothing

isPowerOfTwo :: Integer -> Bool
isPowerOfTwo 0 = True
isPowerOfTwo 1 = False
isPowerOfTwo n = (n .&. (n -1)) == 0
{-# INLINE isPowerOfTwo #-}

-- TODO consolidate
optionLast :: a -> Option (Last a)
optionLast = Option . Just . Last

mapNums :: ([Integer] -> [Integer]) -> TimeSignature -> TimeSignature

mapDenom :: (Integer -> Integer) -> TimeSignature -> TimeSignature

isSimple :: TimeSignature -> Bool

getSimple :: TimeSignature -> Integer

liftRational :: (Fractional c, Real a) => (Rational -> Rational) -> a -> c

liftRational2 :: (Fractional a, Real a1, Real a2) => (Rational -> Rational -> Rational) -> a1 -> a2 -> a
