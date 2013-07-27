
{-# LANGUAGE TypeFamilies, ConstraintKinds, FlexibleContexts #-}

import Data.Foldable
import Data.Semigroup
import Control.Monad.Plus
import Data.VectorSpace
import Data.AffineSpace

type family Time (s :: * -> *) :: *
type Duration a = Diff (Time a)

class Delayable s where
    delay :: Duration s -> s a -> s a
class Stretchable s where
    stretch :: Duration s -> s a -> s a
class HasDuration s where
    duration :: s a -> Duration s
class HasOnset s where
    onset  :: s a -> Time s
class HasOffset s where
    offset :: s a -> Time s
class Foldable s => Performable s where
    perform :: (t ~ Time s, d ~ Duration s) => s a -> [(t, d, a)]

type Monoid' a = (Monoid a, Semigroup a)
class (Stretchable s, Delayable s, AdditiveGroup (Time s), AffineSpace (Time s)) => Transformable1 s where
class (HasOnset s, HasOffset s, Transformable1 s) => Transformable s where
class (MonadPlus s, Transformable s) => Composable s where
class (Performable s, Composable s) => HasEvents s where

class HasPart a where
    type Part a :: *
type HasPart' a = (Ord (Part a), HasPart a)


(|>)            :: (Semigroup (s a), AffineSpace (Time s), HasOnset s, HasOffset s, Delayable s) =>
                s a -> s a -> s a
(<|)            :: (Semigroup (s a), AffineSpace (Time s), HasOnset s, HasOffset s, Delayable s) =>
                s a -> s a -> s a
scat            :: (Monoid' (s a), AffineSpace (Time s), HasOnset s, HasOffset s, Delayable s) =>
                [s a] -> s a
pcat            :: MonadPlus s =>
                [s a] -> s a

delay_          :: Delayable s =>
                Duration s -> s a -> s a
stretch_        :: Stretchable s =>
                Duration s -> s a -> s a
compress        :: (Stretchable s, Fractional d, d ~ Duration s) =>
                d -> s a -> s a
stretchTo       :: (Stretchable s, HasDuration s, Fractional d, d ~ Duration s) =>
                d -> s a -> s a
sustain         :: (Fractional (Duration s), Semigroup (s a), Stretchable s, HasDuration s) =>
                s a -> s a -> s a
anticipate      :: (Semigroup (s a), Transformable s, d ~ Duration s, Ord d) =>
                d -> s a -> s a -> s a
move            :: (Delayable s, d ~ Duration s) =>
                d -> s a -> s a
moveBack        :: (Delayable s, AdditiveGroup d, d ~ Duration s) =>
                d -> s a -> s a
startAt         :: (HasOnset s, Delayable s, AffineSpace t, t ~ Time s) =>
                t -> s a -> s a
stopAt          :: (HasOffset s, Delayable s, AffineSpace t, t ~ Time s) =>
                t -> s a -> s a

rest            :: MonadPlus s =>
                s (Maybe a)
removeRests     :: MonadPlus m =>
                m (Maybe a) -> m a

times           :: (Monoid' (s a), Transformable s) =>
                Int -> s a -> s a
repeated        :: (Monoid' (s b), Transformable s) =>
                [a] -> (a -> s b) -> s b
group           :: (Monoid' (s a), Transformable s, Fractional d, d ~ Duration s) =>
                Int -> s a -> s a

retrograde      :: (HasEvents s, t ~ Time s, Num t, Ord t) =>
                s a -> s a
perform_        :: (Performable s, t ~ Time s, d ~ Duration s) =>
                s a -> [(t, d, a)]
compose         :: (Composable s, d ~ Duration s, t ~ Time s) =>
                [(t, d, a)] -> s a
mapEvents       :: (HasPart' a, HasEvents s, t ~ Time s, d ~ Duration s) =>
                (t -> d -> a -> b) -> s a -> s b
filterEvents    :: (HasPart' a, HasEvents s, t ~ Time s, d ~ Duration s) =>
                (t -> d -> a -> Bool) -> s a -> s a
mapPhraseS      :: HasEvents s =>
                (a -> b) -> (a -> b) -> (a -> b) -> s a -> s b
mapPhrase       :: (HasPart' a, HasEvents s) =>
                (a -> b) -> (a -> b) -> (a -> b) -> s a -> s b
mapAllEvents    :: (HasEvents s, d ~ Duration s, t ~ Time s) =>
                ([(t, d, a)] -> [(t, d, b)]) -> s a -> s b


(
    (|>),
    (<|),
    scat,
    pcat,
    sustain,
    anticipate,
    move,
    moveBack,
    startAt,
    stopAt,
    compress,
    stretchTo,
    rest,
    removeRests,
    times,
    repeated,
    group,
    compose,
    retrograde,
    mapEvents,
    filterEvents,
    mapFilterEvents,
    mapFirst,
    mapLast,
    mapPhraseS,
    mapAllEvents,
    mapPhrase,
    delay_,
    stretch_,
    perform_
    ) = undefined