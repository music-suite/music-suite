
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    DeriveFunctor,
    DeriveFoldable #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides overloaded pitch literals.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Relative -- (
-- )
where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Control.Monad
import Control.Applicative
import qualified Data.List as List

-- type Scale = Step -> Pitch
-- type Pitch = Intonation -> Frequency
type Step       = Integer
type Alteration = Integer
type Interval    = (Step, Alteration)

newtype Pitch = Pitch { getPitch :: Interval }

newtype Scale a = Scale { getScale :: [a] } 
    deriving (Eq, Show, Functor)

step :: Scale a -> Step -> a
step (Scale xs) p = xs !! (fromIntegral p `mod` length xs)


fromStep :: (Num a, Ord a, Integral b, Num c) => Scale a -> b -> c
fromStep (Scale xs) p = fromIntegral $ fromMaybe (length xs - 1) $ List.findIndex (>= fromIntegral p) xs

scaleFromSteps :: Num a => [a] -> Scale a
scaleFromSteps = Scale . accum
    where
        accum = snd . List.mapAccumL add 0
        add a x = (a + x, a + x)

-- numberOfSteps :: Scale a -> Int
numberOfSteps = length . getScale

major :: Num a => Scale a
major = scaleFromSteps [0,2,2,1,2,2,2,1]

naturalMinor :: Num a => Scale a
naturalMinor = scaleFromSteps [0,2,1,2,2,1,2,2]

harmonicMinor :: Num a => Scale a
harmonicMinor = scaleFromSteps [0,2,1,2,2,1,3,1]

