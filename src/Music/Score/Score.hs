
{-# LANGUAGE 
    UndecidableInstances #-}
{-# LANGUAGE 
    ScopedTypeVariables,
    Rank2Types, 
    GeneralizedNewtypeDeriving,
    DeriveFunctor, 
    DeriveFoldable, 
    DeriveTraversable,
    DeriveDataTypeable, 
    ConstraintKinds, 
    ViewPatterns,
    TypeFamilies,
    TypeOperators,
    FlexibleContexts, 
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
-- Provides the 'Score' type.
--
-------------------------------------------------------------------------------------

module Music.Score.Score (
        -- * Score type
        Score,
        mapScore,
        reifyScore,
        getScoreMeta,
        setScoreMeta,
        
        mapWithSpan,
        filterWithSpan,
        mapFilterWithSpan,
        mapEvents,
        filterEvents,
        mapFilterEvents,

  ) where

import Data.Dynamic
import Control.Lens
import Control.Newtype                
import Data.Maybe
import Data.Ord
import Data.Semigroup
import Data.Pointed
import Data.Foldable (foldMap)
import Control.Arrow
import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Plus
import Control.Monad.Compose

import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Test.QuickCheck (Arbitrary(..), Gen(..))

import Data.Default
import Data.Typeable
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
import Music.Pitch.Literal
import Music.Dynamics.Literal   
import Music.Score.Note
import Music.Score.Meta
import Music.Score.Pitch
import Music.Score.Part
import Music.Score.Util

newtype Score a = Score { getScore :: (Meta, NScore a) }
    deriving (Functor, Semigroup, Monoid, Foldable, Traversable, Typeable)

inScore f = Score . f . getScore


-- | Map with the associated time span.
mapScore :: (Note a -> b) -> Score a -> Score b
mapScore f = inScore (second $ mapNScore f)

-- | Group each occurence with its associated time span.
-- 
-- Note: This may or may not be what you expect. Each note is /not/ repositioned
-- to start at 'sunit', so this holds
--
-- > fmap extract . reifyScore = id
--
-- while
--
-- > join . fmap (noteToScore) . reifyScore /= id
--
reifyScore :: Score a -> Score (Note a)
reifyScore = inScore (second reifyNScore)



-- TODO more generic versions of these two:

-- | Set the meta information of a score.
setScoreMeta :: Meta -> Score a -> Score a
setScoreMeta m (Score (_,a)) = Score (m,a)

-- | Get the meta information of a score.
getScoreMeta :: Score a -> Meta
getScoreMeta (Score (m,_)) = m


-- | Map over the events in a score.
mapWithSpan :: (Span -> a -> b) -> Score a -> Score b
mapWithSpan f = mapScore (uncurry f . getNote)

-- | Filter the events in a score.
filterWithSpan :: (Span -> a -> Bool) -> Score a -> Score a
filterWithSpan f = mapFilterWithSpan (partial2 f)

-- | Efficient combination of 'mapEvents' and 'filterEvents'.
mapFilterWithSpan :: (Span -> a -> Maybe b) -> Score a -> Score b
mapFilterWithSpan f = mcatMaybes . mapWithSpan f

-- | Map over the events in a score.
mapEvents :: (Time -> Duration -> a -> b) -> Score a -> Score b
mapEvents f = mapWithSpan (uncurry f . view delta)

-- | Filter the events in a score.
filterEvents   :: (Time -> Duration -> a -> Bool) -> Score a -> Score a
filterEvents f = mapFilterEvents (partial3 f)

-- | Efficient combination of 'mapEvents' and 'filterEvents'.
mapFilterEvents :: (Time -> Duration -> a -> Maybe b) -> Score a -> Score b
mapFilterEvents f = mcatMaybes . mapEvents f



-- TODO remove these, see #97
type instance Container (Score a) = Score
type instance Event (Score a)     = a

instance Newtype (Score a) (Meta, NScore a) where
    pack = Score
    unpack = getScore

instance Monad Score where
    return = pack . return . return
    xs >>= f = pack $ mbind (unpack . f) (unpack xs)

instance Applicative Score where
    pure = return
    (<*>) = ap

instance MonadPlus Score where
    mzero = mempty
    mplus = mappend

instance HasOnset (Score a) where
    onset (Score (m,x)) = onset x

instance HasOffset (Score a) where
    offset (Score (m,x)) = offset x

instance Delayable (Score a) where
    delay n (Score (m,x)) = Score (delay n m, delay n x)

instance Stretchable (Score a) where
    stretch n (Score (m,x)) = Score (stretch n m, stretch n x)

instance HasDuration (Score a) where
    duration = durationDefault

instance Reversible a => Reversible (Score a) where
    rev = fmap rev . withSameOnset (stretch (-1))

instance HasMeta (Score a) where
    applyMeta n (Score (m,x)) = Score (applyMeta n m,x)




instance Pointed Score where
    point = return

instance Performable (Score a) where
    perform = 
        fmap (\(view delta -> (t,d),x) -> (t,d,x)) . 
        List.sortBy (comparing fst) .
        F.toList . 
        fmap getNote . 
        reifyScore

instance Composable (Score a) where



-- |
-- Score without meta-events.
--
-- Semantics: a list of 'Note'. The semantics of each instances follow the instances of
-- the semantics.
-- 
newtype NScore a = NScore { getNScore :: [Note a] } -- sorted
    deriving (Functor, Foldable, Semigroup, Monoid, Traversable, Delayable, Stretchable, HasOnset, HasOffset)

inNScore f = NScore . f . getNScore

-- | Map with the associated span.
mapNScore :: (Note a -> b) -> NScore a -> NScore b
mapNScore f = inNScore (fmap $ extend f)

-- | Reify the associated span. Use with 'Traversable' to get a fold.
reifyNScore :: NScore a -> NScore (Note a)
reifyNScore = inNScore $ fmap duplicate

instance Newtype (NScore a) [Note a] where
    pack = NScore
    unpack = getNScore

instance Monad NScore where
    return = pack . return . return
    xs >>= f = pack $ mbind (unpack . f) (unpack xs)

instance Applicative NScore where
    pure = return
    (<*>) = ap

instance MonadPlus NScore where
    mzero = mempty
    mplus = mappend

instance HasDuration (Note a) where
    duration = durationDefault


-- The following instances allow us to write expressions like [c..g]

instance IsPitch a => IsPitch (Score a) where
    fromPitch = pure . fromPitch

instance IsDynamics a => IsDynamics (Score a) where
    fromDynamics = pure . fromDynamics

instance Enum a => Enum (Score a) where
    toEnum = return . toEnum
    fromEnum = list 0 (fromEnum . head) . F.toList


-- Bogus VectorSpace instance, so we can use c^*2 etc.
-- If you hate this instance, please open an issue.

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

type instance Pitch (Score a) = Pitch a
instance (HasSetPitch a b, 
            Transformable (Pitch a), 
            Transformable (Pitch b)) => 
                HasSetPitch (Score a) (Score b) where
    type SetPitch g (Score a) = Score (SetPitch g a)
    mapPitch f  = mapWithSpan (\s -> mapPitch $ sunder s f)

instance HasPart a => HasPart (Score a) where
    type Part (Score a) = Part a
    getPart         = fromMaybe def . fmap getPart . listToMaybe . F.toList
    modifyPart f    = fmap (modifyPart f)



fst3 (t, d, x) = t
trd3 (a,b,c) = c

third f (a,b,c) = (a,b,f c)
third' f (a,b,c) = (a,b,f a b c)

partial2 :: (a -> b -> Bool)      -> a -> b -> Maybe b
partial3 :: (a -> b -> c -> Bool) -> a -> b -> c -> Maybe c
partial2 f = curry  (fmap snd  . partial (uncurry f))
partial3 f = curry3 (fmap trd3 . partial (uncurry3 f))

iterating :: (a -> a) -> (a -> a) -> Int -> a -> a
iterating f g n
    | n <  0 = f . iterating f g (n + 1)
    | n == 0 = id
    | n >  0 = g . iterating f g (n - 1)

successor :: (Integral b, Enum a) => b -> a -> a
successor n = iterating pred succ (fromIntegral n)

maximum' :: (Ord a, Foldable t) => a -> t a -> a
maximum' z = option z getMax . foldMap (Option . Just . Max)

minimum' :: (Ord a, Foldable t) => a -> t a -> a
minimum' z = option z getMin . foldMap (Option . Just . Min)




