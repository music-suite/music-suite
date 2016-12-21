
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
-- Provides time signatures and related meta-data.
--
-------------------------------------------------------------------------------------

module Music.Score.Meta.Time (

        -- * Time signature type
        TimeSignature,
        time,
        compoundTime,
        isSimpleTime,
        isCompoundTime,
        toSimpleTime,
        getTimeSignature,

        -- * Adding time signature to scores
        timeSignature,
        timeSignatureDuring,

        -- * Extracting time signatures
        withTimeSignature,
        getTimeSignatures,
        getTimeSignatureChanges,

        -- * Utility
        getBarDurations,
        getBarTimeSignatures,
        standardTimeSignature,
  ) where

import           Control.Lens              (view, (^.))
import           Control.Monad.Plus
import           Data.Bifunctor
import           Data.Foldable             (Foldable)
import qualified Data.Foldable             as F
import qualified Data.List                 as List
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Ratio                ((%))
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
import           Music.Time                hiding (time)

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
--
newtype TimeSignature = TimeSignature ([Integer], Integer)
    deriving (Eq, Ord, Typeable)

mapNums   f (TimeSignature (m,n)) = TimeSignature (f m, n)
mapDenom  f (TimeSignature (m,n)) = TimeSignature (m, f n)

isSimple    (TimeSignature ([_],_)) = True
isSimple    _                       = False

getSimple   (TimeSignature ([m],n)) = m `div` n
getSimple   _                       = error "getSimple: Not a simple time signature"

-- TODO move
liftRational f = fromRational . f . toRational
liftRational2 f x y = fromRational $ toRational x `f` toRational y

instance Num TimeSignature where
    x + y | x `haveSameDenominator` y   = concatFrac x y
          | otherwise                   = liftRational2 (+) x y
        where
            TimeSignature (_,n1) `haveSameDenominator` TimeSignature (_,n2) = n1 == n2
            TimeSignature (m1,n) `concatFrac` TimeSignature (m2,_) = TimeSignature (m1 <> m2, n)

    x * y | isSimple y = mapNums (fmap (* (getSimple y))) x
          | otherwise  = liftRational2 (*) x y

    negate  = liftRational negate
    abs     = liftRational abs
    signum  = liftRational signum
    fromInteger x = TimeSignature ([x], 1)

instance Fractional TimeSignature where
    fromRational (unRatio -> (m, n)) = TimeSignature ([m], n)

    x / y | isSimple y = mapDenom (* (getSimple y)) x
          | otherwise  = liftRational2 (/) x y

instance Real TimeSignature where
    toRational (TimeSignature (xs, x)) = sum xs % x

instance Show TimeSignature where
    show (TimeSignature ([m], n)) = show m ++ "/" ++ show n
    show (TimeSignature (xs,  n)) = "(" ++ List.intercalate "+" (fmap show xs) ++ ")/" ++ show n

-- | Create a simple time signature.
time :: Integer -> Integer -> TimeSignature
time x y = TimeSignature ([x], y)

-- | Create a compound time signature.
compoundTime :: [Integer] -> Integer -> TimeSignature
compoundTime = curry TimeSignature

-- | Whether this is a simple time signature.
isSimpleTime :: TimeSignature -> Bool
isSimpleTime (TimeSignature ([_],_)) = True
isSimpleTime _                       = False

-- | Whether this is a compound time signature.
isCompoundTime :: TimeSignature -> Bool
isCompoundTime = not . isSimpleTime

-- | Convert to a simple time signature by adding all numerators.
--   If given a simple time signature, returns it.
toSimpleTime :: TimeSignature -> TimeSignature
toSimpleTime = fromRational . toRational

-- | Extract the components of a time signature. Semantic function.
--
-- Typically used with the @ViewPatterns@ extension, as in
--
-- > foo (getTimeSignature -> (beats, noteValue)) = ...
--
getTimeSignature :: TimeSignature -> ([Integer], Integer)
getTimeSignature (TimeSignature x) = x

-- | Set the time signature of the given score.
timeSignature :: (HasMeta a, HasPosition a, Transformable a) => TimeSignature -> a -> a
timeSignature c x = timeSignatureDuring (0 <-> x^.offset) c x

-- use (x^.onset <-> x^.offset) instead of (0 <-> x^.offset)
-- timeSignature' c x = timeSignatureDuring (era x) c x

-- | Set the time signature of the given part of a score.
timeSignatureDuring :: HasMeta a => Span -> TimeSignature -> a -> a
timeSignatureDuring s c = addMetaNote $ view event (s, optionLast c)

getTimeSignatures :: TimeSignature -> Score a -> Reactive TimeSignature
getTimeSignatures def = fmap (fromMaybe def . unOptionLast) . fromMetaReactive . (view meta)

getTimeSignatureChanges :: TimeSignature -> Score a -> [(Time, TimeSignature)]
getTimeSignatureChanges def = updates . getTimeSignatures def

-- | Extract the time signature from the given score, using the given default time signature.
withTimeSignature :: TimeSignature -> (TimeSignature -> Score a -> Score a) -> Score a -> Score a
withTimeSignature def f = withMeta (f . fromMaybe def . unOptionLast)

-- | Given a list of time signatures and the duration between them (TODO use voice), return a list of appropriate
--   bar durations.
getBarDurations :: [(TimeSignature, Duration)] -> [Duration]
getBarDurations = fmap realToFrac . getBarTimeSignatures

-- | Given a list of time signatures and the duration between them (TODO use voice), return a list of appropriate
--   time signatures for each bar.
getBarTimeSignatures :: [(TimeSignature, Duration)] -> [TimeSignature]
getBarTimeSignatures = concatMap $ uncurry getBarTimeSignatures1

getBarTimeSignatures1 :: TimeSignature -> Duration -> [TimeSignature]
getBarTimeSignatures1  ts d = let
    (n,r) = numWholeBars ts d
    -- Repeat the chosen time signature as long as possible
    -- If there is a rest duration, add a bar of that duration choosing an appropriate time signature
    in replic n ts ++ if r > 0 then [standardTimeSignature r] else []


-- | Return the number of whole bars needed to notate the given duration, as well as the remainder duration.
numWholeBars :: TimeSignature -> Duration -> (Integer, Duration)
numWholeBars ts dur = second (* barDur) $ properFraction (dur / barDur) where barDur = realToFrac ts



-- | Time signature typically used for the given duration.
standardTimeSignature :: Duration -> TimeSignature
standardTimeSignature x = case unRatio (toRational x) of
    -- (1,2) -> time 1 2
    (2,2) -> time 2 2
    (3,2) -> time 3 2
    (2,1) -> time 4 2
    (5,2) -> time 5 2
    (3,1) -> time 6 2
    (7,2) -> time 7 2

    (1,4) -> time 1 4
    (1,2) -> time 2 4
    (3,4) -> time 3 4
    (1,1) -> time 4 4
    (5,4) -> time 5 4
    -- (3,2) -> time 6 4
    (7,4) -> time 7 4

    (1,8) -> time 1 8
    -- (1,4) -> time 2 8
    (3,8) -> time 3 8
    -- (1,2) -> time 4 8
    (5,8) -> time 5 8
    -- (3,4) -> time 6 8
    (7,8) -> time 7 8

    -- TODO check divisible by 8 etc
    _        -> time 4 4
    -- _     -> error "standardTimeSignature: Stange value"

-- TODO consolidate
optionLast = Option . Just . Last
unOptionLast = fmap getLast . getOption


mapNums :: ([Integer] -> [Integer]) -> TimeSignature -> TimeSignature
mapDenom :: (Integer -> Integer) -> TimeSignature -> TimeSignature
isSimple :: TimeSignature -> Bool
getSimple :: TimeSignature -> Integer
liftRational :: (Fractional c, Real a) => (Rational -> Rational) -> a -> c
liftRational2 :: (Fractional a, Real a1, Real a2) => (Rational -> Rational -> Rational) -> a1 -> a2 -> a
optionLast :: a -> Option (Last a)
unOptionLast :: Option (Last b) -> Maybe b
