                              
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
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
-- Provides a musical score represenation.
--
-------------------------------------------------------------------------------------

module Music.Score.Score (
        Score(..),
        rest,
        note,    
        -- filterS,
        perform,
        performRelative
  ) where

import Prelude hiding (foldr, concat, foldl, mapM, concatMap, maximum, sum, minimum)

import Data.Semigroup
import Control.Applicative
import Control.Monad (ap, join, MonadPlus(..))
import Data.Foldable
import Data.Traversable
import Data.Maybe
import Data.Either
import Data.Function (on)
import Data.Ord (comparing)
import Data.Ratio
import Data.VectorSpace
import Data.AffineSpace
import qualified Data.Map as Map
import qualified Data.List as List

import Music.Pitch.Literal
import Music.Dynamics.Literal

import Music.Score.Time
import Music.Score.Duration

-------------------------------------------------------------------------------------
-- Score type
-------------------------------------------------------------------------------------

-- |
-- A score is a sorted list of absolute time notes and rests. A rest is a duration and 
-- a note is a value and a duration.
--
-- Score is a 'Monoid' under parallel composition. 'mempty' is a score of no parts.
-- For sequential composition of scores, use '|>'.
--
-- Score has an 'Applicative' instance derived from the 'Monad' instance. Not sure it is useful.
--
-- Score is a 'Monad'. 'return' creates a score containing a single note of
-- duration one, and '>>=' transforms the values of a score, while allowing
-- transformations of time and duration. More intuitively, 'join' scales and
-- offsets each inner score to fit into an outer score, then removes the intermediate
-- structure. 
--
-- > let s = Score [(0, 1, Just 0), (1, 2, Just 1)] :: Score Int
-- >
-- > s >>= \x -> Score [ (0, 1, Just $ toEnum $ x+65), 
-- >                     (1, 3, Just $ toEnum $ x+97) ] :: Score Char
-- >
-- >     ===> Score {getScore = [ (0 % 1, 1 % 1, Just 'A'),
-- >                              (1 % 1, 3 % 1, Just 'a'),
-- >                              (1 % 1, 2 % 1, Just 'B'),
-- >                              (3 % 1, 6 % 1, Just 'b') ]}
--
-- Score is an instance of 'VectorSpace' using sequential composition as addition, 
-- and time scaling as scalar multiplication. 
--
newtype Score a  = Score { getScore :: [(Time, Duration, Maybe a)] }
    deriving ({-Eq, Ord, -}Show, Functor, Foldable)

-- Performance equality needeed because of rests...

instance Eq a => Eq (Score a) where
    a == b = perform a == perform b

instance Ord a => Ord (Score a) where
    a `compare` b = perform a `compare` perform b

instance Semigroup (Score a) where
    (<>) = mappend

-- Equivalent to the derived Monoid, except for the sorted invariant.
instance Monoid (Score a) where
    mempty = Score []
    Score as `mappend` Score bs = Score (as `m` bs)
        where
            m = mergeBy (comparing fst3)
            fst3 (a,b,c) = a

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

instance Monad Score where
    return = note
    a >>= k = join' $ fmap k a
        where  
            join' sc = mconcat $ toList $ mapWithTimeDur (\t d -> fmap (delay t . (d*^) )) $ sc

mapWithTimeDur :: (Duration -> Duration -> Maybe a -> Maybe b) -> Score a -> Score b
mapWithTimeDur f = Score . fmap (liftTimeDur f) . getScore

liftTimeDur :: (Duration -> Duration -> Maybe a -> Maybe b) -> (Time, Duration, Maybe a) -> (Time, Duration, Maybe b)
liftTimeDur f (t,d,x) = case f (t2d t) d x of
    Nothing -> (t,d,Nothing)
    Just y  -> (t,d,Just y)
    where
        t2d = Duration . getTime

instance AdditiveGroup (Score a) where
    zeroV   = mempty
    (^+^)   = mappend
    negateV = id

instance VectorSpace (Score a) where
    type Scalar (Score a) = Duration
    d *^ Score sc = Score . fmap (first3 (^* d2t d) . second3 (^* d)) $ sc
        where
            first3 f (a,b,c) = (f a,b,c)
            second3 f (a,b,c) = (a,f b,c)                      
            d2t = Time . getDuration
            
instance Delayable (Score a) where
    d `delay` Score sc = Score . fmap (first3 (.+^ d)) $ sc
        where
            first3 f (a,b,c) = (f a,b,c)

instance HasOnset (Score a) where
    onset  (Score []) = 0
    onset  (Score xs) = minimum (fmap on xs)  where on  (t,d,x) = t
    offset (Score []) = 0
    offset (Score xs) = maximum (fmap off xs) where off (t,d,x) = t + (Time . getDuration $ d)
        
instance HasDuration (Score a) where
    duration x = offset x .-. onset x            

instance IsPitch a => IsPitch (Score a) where
    fromPitch = pure . fromPitch

instance IsDynamics a => IsDynamics (Score a) where
    fromDynamics = pure . fromDynamics


-- |
-- Create a score of duration one with no values.
--
rest :: Score a
rest = Score [(0,1,Nothing)]

-- |
-- Create a score of duration one with the given value. Equivalent to 'pure' and 'return'.
--
note :: a -> Score a
note x = Score [(0,1,Just x)]

{-
-- Use mfilter instead of this

filterS :: (a -> Bool) -> Score a -> Score a
filterS f = Score . filter g . getScore
    where
       g (t,d,Nothing)  = True
       g (t,d,(Just x)) = f x
-}

perform :: Score a -> [(Time, Duration, a)]
perform = removeRests . getScore
    where
        removeRests = catMaybes . fmap propagateRest
        propagateRest (t, d, Just x)  = Just (t, d, x)
        propagateRest (t, d, Nothing) = Nothing

performRelative :: Score a -> [(Time, Duration, a)]
performRelative = toRel . perform
    where
        toRel = snd . mapAccumL g 0
        g now (t,d,x) = (t, (t-now,d,x))







                                            

list z f [] = z
list z f xs = f xs

first f (x,y)  = (f x, y)
second f (x,y) = (x, f y)


mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f as bs = List.sortBy f $ as <> bs

