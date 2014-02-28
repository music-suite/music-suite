
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

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
        notes,
        events,
        -- mkScore,
        -- getScore,
        mapScore,
        reifyScore,

        mapWithSpan,
        filterWithSpan,
        mapFilterWithSpan,
        mapEvents,
        filterEvents,
        mapFilterEvents,

  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Comonad
import           Control.Lens
import           Control.Monad
import           Control.Monad.Compose
import           Control.Monad.Plus
import           Data.Dynamic
import           Data.Foldable          (foldMap)
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup

import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.VectorSpace
import           Test.QuickCheck        (Arbitrary (..), Gen (..))

import           Data.Default
import           Data.Foldable          (Foldable)
import qualified Data.Foldable          as F
import qualified Data.List              as List
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Traversable       (Traversable)
import qualified Data.Traversable       as T
import           Data.Typeable

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Score.Meta
import           Music.Score.Note
import           Music.Score.Part
import           Music.Score.Pitch
import           Music.Score.Util
import           Music.Time
import           Music.Time.Reactive

newtype Score a = Score { getScore' :: (Meta, NScore a) }
    deriving (Functor, Semigroup, Monoid, Foldable, Traversable, Typeable)

-- | TODO not a real iso, must be lens (meta)
notes :: Iso (Score a) (Score b) [Note a] [Note b]
notes = iso (getNScore . snd . getScore') (Score . return . NScore)

-- | TODO not a real iso, must be lens (meta)
events :: Iso (Score a) (Score b) [(Time, Duration, a)] [(Time, Duration, b)]
events = iso getScore mkScore

inScore f = Score . f . getScore'

mkScore :: [(Time, Duration, a)] -> Score a
mkScore = mconcat . fmap (uncurry3 event)
    where
        event t d x   = (delay (t .-. origin) . stretch d) (return x)

getScore :: Score a -> [(Time, Duration, a)]
getScore =
    fmap (\(view delta -> (t,d),x) -> (t,d,x)) .
    List.sortBy (comparing fst) .
    F.toList .
    fmap getNote .
    reifyScore

-- | Map with the associated time span.
mapScore :: (Note a -> b) -> Score a -> Score b
mapScore f = over unwrapped (second $ mapNScore f)

-- | Group each occurence with its associated time span.
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
reifyScore = over unwrapped (second reifyNScore)

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

instance Wrapped (Meta, NScore a) (Meta, NScore b) (Score a) (Score b) where
    wrapped = iso Score getScore'

instance Applicative Score where
    pure = return
    (<*>) = ap

instance Monad Score where
    return = (^. wrapped) . return . return
    xs >>= f = (^. wrapped) $ mbind ((^. unwrapped) . f) ((^. unwrapped) xs)

instance Alternative Score where
    empty = mempty
    (<|>) = mappend

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
    meta = unwrapped . _1




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
reifyNScore = inNScore $ fmap duplicate

instance Wrapped [Note a] [Note a] (NScore a) (NScore a) where
    wrapped = iso NScore getNScore

instance Applicative NScore where
    pure = return
    (<*>) = ap

instance Monad NScore where
    return = (^. wrapped) . return . return
    xs >>= f = (^. wrapped) $ mbind ((^. unwrapped) . f) ((^. unwrapped) xs)

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

instance IsInterval a => IsInterval (Score a) where
    fromInterval = pure . fromInterval

instance Enum a => Enum (Score a) where
    toEnum = return . toEnum
    fromEnum = list 0 (fromEnum . head) . F.toList

-- TODO
instance Num a => Num (Score a) where
    fromInteger = return . fromInteger



-- Bogus VectorSpace instance, so we can use c^*2 etc.
-- If you hate this instance, please open an issue.

instance AdditiveGroup (Score a) where
    zeroV   = error "Not impl"
    (^+^)   = error "Not impl"
    negateV = error "Not impl"

instance VectorSpace (Score a) where
    type Scalar (Score a) = Duration
    d *^ s = d `stretch` s

type instance Pitch (Score a) = Pitch a
instance (HasSetPitch a b,
            Transformable (Pitch a),
            Transformable (Pitch b)) =>
                HasSetPitch (Score a) (Score b) where
    type SetPitch g (Score a) = Score (SetPitch g a)
    -- TODO really similar to indexed maps
    -- compare lens, category-extras
    __mapPitch f = mapWithSpan (__mapPitch . (`sunder` f))


type instance Part (Score a) = Part a
instance HasPart a => HasPart (Score a) where
    getPart = error "No Score.getPart"
    modifyPart f    = fmap (modifyPart f)


-- TODO mo
partial2 :: (a -> b      -> Bool) -> a -> b      -> Maybe b
partial3 :: (a -> b -> c -> Bool) -> a -> b -> c -> Maybe c
partial2 f = curry  (fmap snd  . partial (uncurry f))
partial3 f = curry3 (fmap (view _3) . partial (uncurry3 f))
