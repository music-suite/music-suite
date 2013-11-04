
{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveDataTypeable, 
    DeriveTraversable, GeneralizedNewtypeDeriving #-}

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
        -- * Score type
        Score,
  ) where

import Data.Semigroup
import Control.Applicative
import Control.Monad            (ap, join, MonadPlus(..))

import Data.Typeable
import Data.Foldable            (Foldable(..), foldMap, toList)
import Data.Traversable         (Traversable(..))
import Data.Pointed
import Data.Ord                 (comparing)
import Data.Function            (on)
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Test.QuickCheck          (Arbitrary(..), Gen(..))

import Music.Time
import Music.Pitch.Literal
import Music.Dynamics.Literal   
import Music.Score.Pitch
import Music.Score.Util

import qualified Data.List as List

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
--
-- Score is a 'Monad'. 'return' creates a score containing a single note at /(0, 1)/.
--
-- @s@ '>>=' @k@ maps each note to a new score, which is then scaled and delayed by the onset and
-- duration of the original note. That is, @k@ returns a score @t@ such that /0 < onset t < offset t < 1/,
-- the resulting events will not cross the boundaries of the original note.
--
-- 'join' scales and offsets each inner score to fit into the note containing it, then
-- removes the intermediate structure.
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
--
-- data Score a  = Score [(Time, Duration, a)]
newtype Score a  = Score [(Time, Duration, a)]
    deriving (Eq, Ord, Show, Functor, Foldable, Typeable, Traversable, Delayable, Stretchable)

getScore (Score x) = x
-- instance Delayable (Score a) where delay = undefined
-- instance Stretchable (Score a) where stretch = undefined

-- type instance Duration (Score a)  = DurationT
type instance Container (Score a) = Score
type instance Event (Score a)     = a

instance Semigroup (Score a) where
    (<>) = mappend

-- Equivalent to the derived Monoid, except for the sorted invariant.
instance Monoid (Score a) where
    mempty = Score []
    a@(Score as) `mappend` b@(Score bs)
        | offset a <= onset b   =  Score (as <> bs)
        | otherwise             =  Score (as `m` bs)
        where
            m = mergeBy (comparing fst3)

-- (onset b) above makes this diverge
loop :: Score a -> Score a
loop a = a <> loop a

instance Monad Score where
    return x = Score [(origin, 1, x)]
    a >>= k = (join' . fmap k) a
        where
            join' sc = fold $ mapTime (\t d -> delayTime t . stretch d) sc
            mapTime f = Score . fmap (mapEvent f) . getScore
            mapEvent f (t, d, x) = (t, d, f t d x)
            
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

instance HasDuration (Score a) where
    duration x = offset x .-. onset x

instance HasOnset (Score a) where
    -- onset (Score a) = list origin (minimum . map on) a where on (t,d,x) = t
    -- Note: this version of onset is lazier, but depends on the invariant that the list is sorted
    onset (Score a) = list origin (on . head) a where on (t,d,x) = t

instance HasOffset (Score a) where
    -- offset (Score a) = list origin (maximum . map off) a where off (t,d,x) = t .+^ d
    offset (Score a) = list origin (off . last) a where off (t,d,x) = t .+^ d

instance Performable (Score a) where
    perform (Score a) = a

instance Composable (Score a) where
    compose = Score . List.sortBy (comparing fst3)

instance IsPitch a => IsPitch (Score a) where
    fromPitch = pure . fromPitch

instance IsDynamics a => IsDynamics (Score a) where
    fromDynamics = pure . fromDynamics

-- This instance is just to be able to write expressions like [c..g]
instance Enum a => Enum (Score a) where
    toEnum = return . toEnum
    fromEnum = list 0 (fromEnum . head) . toList

instance AdditiveGroup (Score a) where
    zeroV   = error "Not impl"
    (^+^)   = error "Not impl"
    negateV = error "Not impl"

instance VectorSpace (Score a) where
    type Scalar (Score a) = Duration
    d *^ s = d `stretch` s

instance Arbitrary a => Arbitrary (Score a) where
    arbitrary = do
        x <- arbitrary
        t <- fmap realToFrac (arbitrary::Gen Double)
        d <- fmap realToFrac (arbitrary::Gen Double)
        return $ delay t $ stretch d $ return x

instance HasPitch a => HasPitch (Score a) where
    type Pitch (Score a) = Pitch a
    getPitches as    = foldMap getPitches as
    modifyPitch f    = fmap (modifyPitch f)

instance Reversible a => Reversible (Score a) where
    rev = fmap rev . withSameOnset (mapAll $ fmap g)
        where
            g (t,d,x) = (negateP (t .+^ d), d, x)
            negateP a = origin .-^ (a .-. origin)
            mapAll f = compose . f . perform


-------------------------------------------------------------------------------------

fst3 (a,b,c) = a

first3 f (a,b,c) = (f a,b,c)
second3 f (a,b,c) = (a,f b,c)

{-
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f [] ys = ys
mergeBy f xs [] = xs
mergeBy f xs'@(x:xs) ys'@(y:ys)
    | x `f` y == LT   =   x : mergeBy f xs ys'
    | x `f` y /= LT   =   y : mergeBy f xs' ys

-}
