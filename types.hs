
{-# LANGUAGE TypeFamilies, ConstraintKinds, FlexibleContexts, MultiParamTypeClasses, NoMonomorphismRestriction #-}

import Data.Foldable (Foldable(..))
import Data.Semigroup
import Control.Monad.Plus
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import qualified Data.List as List

-- type family Time (s :: * -> *) :: *
type family Duration (s :: *) :: *
type family Event (s :: *) :: *
type Time a = Point (Duration a)
-- type Duration a = Diff (Time a)

class Delayable a where
    delay :: Duration a -> a -> a
class Stretchable a where
    stretch :: Duration a -> a -> a
class HasDuration a where
    duration :: a -> Duration a
class HasOnset a where
    onset  :: a -> Time a
class HasOffset a where
    offset :: a -> Time a


class HasPart a where
    type Part a :: *
    getPart :: a -> Part a
type HasPart' a = (Ord (Part a), HasPart a)

type Monoid' a          =  (Monoid a, Semigroup a)
type Transformable1 a   =  (Stretchable a, Delayable a, AffineSpace (Time a))
type Transformable a    =  (HasOnset a, HasOffset a, Transformable1 a)

class (Monoid a, Transformable1 a) => Composable a where
    note    :: Event a -> a
    compose :: [(Time a, Duration a, Event a)] -> a
    compose = mconcat . fmap event
class Performable a where
    perform :: a -> [(Time a, Duration a, Event a)]
    events :: a -> [Event a]



(|>)            :: (Semigroup a, AffineSpace (Time a), HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a
(<|)            :: (Semigroup a, AffineSpace (Time a), HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a
scat            :: (Monoid' a, AffineSpace (Time a), HasOnset a, HasOffset a, Delayable a) =>
                [a] -> a
pcat            :: Monoid' a =>
                [a] -> a

delay_          :: Delayable a =>
                Duration a -> a -> a
stretch_        :: Stretchable a =>
                Duration a -> a -> a
compress        :: (Stretchable a, Fractional d, d ~ Duration a) =>
                d -> a -> a
stretchTo       :: (Stretchable a, HasDuration a, Fractional d, d ~ Duration a) =>
                d -> a -> a
sustain         :: (Semigroup a, Stretchable a, HasDuration a, Fractional d, d ~ Duration a) =>
                a -> a -> a
anticipate      :: (Semigroup a, Transformable a, Ord d, d ~ Duration a) =>
                d -> a -> a -> a
move            :: (Delayable a, d ~ Duration a) =>
                d -> a -> a
moveBack        :: (Delayable a, AdditiveGroup d, d ~ Duration a) =>
                d -> a -> a
startAt         :: (HasOnset a, Delayable a, AffineSpace t, t ~ Time a) =>
                t ->  a -> a
stopAt          :: (HasOffset a, Delayable a, AffineSpace t, t ~ Time a) =>
                t -> a -> a

rest            :: MonadPlus m =>
                m (Maybe a)
removeRests     :: MonadPlus m =>
                m (Maybe a) -> m a

times           :: (Monoid' a, Transformable a) =>
                Int -> a -> a
repeated        :: (Monoid' b, Transformable b) =>
                [a] -> (a -> b) -> b
group           :: (Monoid' a, Transformable a, Fractional d, d ~ Duration a) =>
                Int -> a -> a

move = delay
moveBack t = delay (negateV t)
compress x = stretch (recip x)
t `stretchTo` x = (t / duration x) `stretch` x
t `startAt` x = (t .-. onset x) `delay` x
t `stopAt`  x = (t .-. offset x) `delay` x

a |> b =  a <> startAt (offset a) b
a <| b =  b |> a
scat = Prelude.foldr (|>) mempty
pcat = Prelude.foldr (<>) mempty

x `sustain` y    = x <> duration x `stretchTo` y
anticipate t a b =  a <> startAt (offset a .-^ t) b

rest = return Nothing
removeRests = mcatMaybes







-- class (MonadPlus s, Transformable (s a)) => Composable s a where
-- class (Performable s, Composable s a) => HasEvents s a where

-- retrograde      :: (HasEvents s a, t ~ Time (s a), Num t, Ord t) =>
                -- s a -> s a
-- perform_        :: (Performable s, t ~ Time s, d ~ Duration s) =>
                -- s a -> [(t, d, a)]
-- compose         :: (a s, d ~ Duration s, t ~ Time s) =>
--                 [(t, d, a)] -> s a
-- mapEvents       :: (HasPart' a, HasEvents s, t ~ Time s, d ~ Duration s) =>
--                 (t -> d -> a -> b) -> s a -> s b
-- filterEvents    :: (HasPart' a, HasEvents s, t ~ Time s, d ~ Duration s) =>
--                 (t -> d -> a -> Bool) -> s a -> s a
-- mapPhraseS      :: HasEvents s =>
--                 (a -> b) -> (a -> b) -> (a -> b) -> s a -> s b
-- mapPhrase       :: (HasPart' a, HasEvents s) =>
--                 (a -> b) -> (a -> b) -> (a -> b) -> s a -> s b
-- mapAllEvents    :: (HasEvents s, d ~ Duration s, t ~ Time s) =>
--                 ([(t, d, a)] -> [(t, d, b)]) -> s a -> s b
-- 
mapAllEvents :: (Performable a, Composable b) => ([(Time a, Duration a, Event a)] -> [(Time b, Duration b, Event b)]) -> a -> b
mapAllParts :: (MonadPlus m, a ~ Event (m a), HasPart' a, Performable (m a)) => ([m a] -> [m b]) -> m a -> m b

mapEvents f                 =              mapAllParts (liftM $ mapEventsS f)
mapFilterEvents f           = mcatMaybes . mapAllParts (liftM $ mapEventsS f)
mapEventsS f                = compose . fmap (third' f) . perform
mapAllEvents f              = compose . f . perform

mapPhrase f g h             =              mapAllParts (fmap $ mapPhraseS f g h)
mapPhraseS f g h            = compose . mapFirstMiddleLast (third f) (third g) (third h) . perform
mapFirstMiddleLast f g h    = go
    where
        go []    = []
        go [a]   = [f a]
        go [a,b] = [f a, h b]
        go xs    = [f $ head xs]          ++ 
                   map g (tail $ init xs) ++ 
                   [h $ last xs]
mapAllParts f   = msum . f . extract
    where
        extract a = fmap (`extract'` a) (getParts a)
        extract' v = mfilter ((== v) . getPart)
        getParts = List.sort . List.nub . fmap getPart . events






{-
perform         :: Performable s => 
                s a -> [(Time (s a), Duration (s a), a)]
-}                
type D = Double
type T = Point D
newtype S a = S [(T, D, a)]
type instance Duration (S a) = D
type instance Event (S a) = a

instance HasDuration (S a) where
    duration x = offset x .-. onset x
instance HasOnset (S a) where
    onset (S a) = list origin (on . head) a where on (t,d,x) = t
instance HasOffset (S a) where
    offset (S a) = list origin (maximum . map off) a where off (t,d,x) = t .+^ d
instance Delayable (S a) where
    d `delay` S a = S $ fmap (first3 (.+^ d)) $ a
instance Stretchable (S a) where
    d `stretch` S a = S $ fmap (first3 (\t -> origin .+^(t .-. origin)^*d) . second3 (^* d)) $ a
instance Monoid (S a) where
instance Monad S where
instance Performable (S a) where
    perform (S a) = a
instance Composable (S a) where
    note a = S [(origin, 1, a)]
    compose = S
 
event :: Composable a => (Time a, Duration a, Event a) -> a
event (t,d,x) = delay (t .-. origin) . stretch d $ note x




list z f [] = z
list z f xs = f xs
first3 f (a,b,c) = (f a,b,c)
second3 f (a,b,c) = (a,f b,c)
third f (a,b,c) = (a,b,f c)
third' f (a,b,c) = (a,b,f a b c)



(
    -- (|>),
    -- (<|),
    -- scat,
    -- pcat,
    -- sustain,
    -- anticipate,
    -- move,
    -- moveBack,
    -- startAt,
    -- stopAt,
    -- compress,
    -- stretchTo,
    -- rest,
    -- removeRests,
    times,
    repeated,
    group,
    -- compose,
    retrograde,
    -- mapEvents,
    -- filterEvents,
    -- mapFilterEvents,
    -- mapFirst,
    -- mapLast,
    -- mapPhraseS,
    -- mapAllEvents,
    -- mapPhrase,
    delay_,
    stretch_,
    perform_
    ) = undefined