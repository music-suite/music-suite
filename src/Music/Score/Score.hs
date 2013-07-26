
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    DeriveTraversable,
    GeneralizedNewtypeDeriving #-}

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
-- Provides the 'Score' type.
--
-------------------------------------------------------------------------------------

module Music.Score.Score (
        Score,
        -- mapTime,
  ) where

import Prelude hiding (null, length, repeat, foldr, concat, foldl, mapM, concatMap, maximum, sum, minimum)

import Data.Semigroup
import Control.Applicative
import Control.Monad (ap, join, MonadPlus(..))
import Data.Foldable
import Data.Traversable
import Data.Typeable
import Data.Maybe
import Data.Either
import Data.Pointed
import Data.Function (on)
import Data.Ord (comparing)
import Data.Ratio
import Data.VectorSpace
import Data.AffineSpace
import Test.QuickCheck (Arbitrary(..),Gen(..))
import qualified Data.Map as Map
import qualified Data.List as List

import Music.Pitch.Literal
import Music.Dynamics.Literal

import Music.Time
import Music.Score.Voice
import Music.Score.Track

-------------------------------------------------------------------------------------
-- Score type
-------------------------------------------------------------------------------------

-- |
-- A score is a list of events, i.e. time-duration-value triplets. Semantically
--
-- > type Score a = [(Time, Duration, a)]
--
-- There is no explicit representation for rests. However you can use `Score (Maybe a)` to
-- represent a score with rests. Such rests are only useful when composing scores. They
-- may be removed with 'removeRests'.
--
-- Score is a 'Monoid' under parallel composition. 'mempty' is a score of no parts.
-- For sequential composition of scores, use '|>'.
--
-- Score has an 'Applicative' instance derived from the 'Monad' instance. Not sure it is useful.
--
-- Score is an instance of 'VectorSpace' using sequential composition as addition,
-- and time scaling as scalar multiplication.
--
newtype Score a  = Score { getScore :: [(Time Score, Duration Score, a)] }
    deriving (Eq, Ord, Show, Functor, Foldable, Typeable, Traversable)

type instance Time Score = TimeT

instance Semigroup (Score a) where
    (<>) = mappend

-- Equivalent to the derived Monoid, except for the sorted invariant.
instance Monoid (Score a) where
    mempty = Score []
    Score as `mappend` Score bs = Score (as `m` bs)
        where
            m = mergeBy (comparing fst3)

-- |
-- This instance is somewhat similar to the list instance.
--
-- * 'return' creates a score containing a single note at /(0, 1)/.
-- 
-- * @s@ '>>=' @k@ maps each note to a new score, which is then scaled and delayed by the onset and
--   duration of the original note. That is, @k@ returns a score @t@ such that /0 < onset t < offset t < 1/, 
--   the resulting events will not cross the boundaries of the original note.
-- 
-- * 'join' scales and offsets each inner score to fit into the note containing it, then
--   removes the intermediate structure.
--
-- > let s = compose [(0,1,0), (1,2,1)]
-- >
-- > s >>= \x -> compose [ (0,1,toEnum $ x+65),
-- >                       (1,3,toEnum $ x+97) ] :: Score Char
-- >
-- >     ===> compose [ (1, 1, 'A'),
-- >                    (1, 3, 'a'),
-- >                    (1, 2, 'B'),
-- >                    (3, 6, 'b') ]}
-- 
instance Monad Score where
    return x = Score [(0, 1, x)]
    a >>= k = join' $ fmap k a
        where
            join' sc = {-mconcat $ toList-}fold $ mapTime (\t d -> delay' t . stretch d) sc

instance Pointed Score where
    point = return

instance Applicative Score where
    pure  = return
    (<*>) = ap

instance Alternative Score where
    empty = mempty
    (<|>) = mappend

-- Satisfies left distribution
instance MonadPlus Score where
    mzero = mempty
    mplus = mappend

instance Performable Score where
    perform = getScore

instance Stretchable (Score) where
    d `stretch` Score sc = Score $ fmap (first3 (^* fromDurationT d) . second3 (^* d)) $ sc

instance Delayable (Score) where
    d `delay` Score sc = Score . fmap (first3 (.+^ d)) $ sc

instance HasOnset (Score) where
    -- onset  (Score []) = 0
    -- onset  (Score xs) = minimum (fmap on xs)  where on  (t,d,x) = t

    -- Note: this version of onset is lazier, but depends on the invariant that the list is sorted
    onset  (Score []) = 0
    onset  (Score xs) = on (head xs) where on (t,d,x) = t

instance HasOffset (Score) where
    offset (Score []) = 0
    offset (Score xs) = maximum (fmap off xs) where off (t,d,x) = t + (fromDurationT $ d)

instance HasDuration (Score) where
    duration x = offset x .-. onset x

instance IsPitch a => IsPitch (Score a) where
    fromPitch = pure . fromPitch

instance IsDynamics a => IsDynamics (Score a) where
    fromDynamics = pure . fromDynamics

-- Utility
instance AdditiveGroup (Score a) where
    zeroV   = error "Not impl"
    (^+^)   = error "Not impl"
    negateV = error "Not impl"

instance VectorSpace (Score a) where
    type Scalar (Score a) = DurationT
    d *^ s = d `stretch` s

instance Arbitrary a => Arbitrary (Score a) where
    arbitrary = do
        x <- arbitrary
        t <- fmap toDurationT $ (arbitrary::Gen Double)
        d <- fmap toDurationT $ (arbitrary::Gen Double)
        return $ delay t $ stretch d $ (note x)

-- |
-- Create a score of duration one with the given value (same as 'return').
--
note :: a -> Score a
note = return

-- |
-- Create a score of duration one with no values.
--
rest :: Score (Maybe a)
rest = return Nothing

-- |
-- Repeat a score indefinately.
--
repeat :: Score a -> Score a
repeat a = a `plus` delay (duration a) (repeat a)
    where
        Score as `plus` Score bs = Score (as <> bs)

-- |
-- Map over all events in a score.
--
mapTime :: (Time Score -> Duration Score -> a -> b) -> Score a -> Score b
mapTime f = Score . fmap (mapEvent f) . getScore

mapEvent :: (Time Score -> Duration Score -> a -> b) -> (Time Score, Duration Score, a) -> (Time Score, Duration Score, b)
mapEvent f (t, d, x) = (t, d, f t d x)


-------------------------------------------------------------------------------------

delay' t = delay (fromTimeT t)

fst3 (a,b,c) = a

list z f [] = z
list z f xs = f xs

first f (x,y)  = (f x, y)
second f (x,y) = (x, f y)

first3 f (a,b,c) = (f a,b,c)
second3 f (a,b,c) = (a,f b,c)

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f [] ys = ys
mergeBy f xs [] = xs
mergeBy f xs'@(x:xs) ys'@(y:ys)
    | x `f` y == LT   =   x : mergeBy f xs ys'
    | x `f` y /= LT   =   y : mergeBy f xs' ys

