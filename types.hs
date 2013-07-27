
{-# LANGUAGE TypeFamilies, ConstraintKinds, FlexibleContexts, MultiParamTypeClasses, NoMonomorphismRestriction #-}

import Data.Ord
import Data.Semigroup
import Data.Foldable (Foldable(..))
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

type Monoid' a         =  (Monoid a, Semigroup a)
type Transformable a   =  (Stretchable a, Delayable a, AffineSpace (Time a))

class (Monoid a, Transformable a) => Composable a where
    note    :: Event a -> a
    event   :: Time a -> Duration a -> Event a -> a
    compose :: [(Time a, Duration a, Event a)] -> a
    -- Given Num (Duration a) we have
    -- note a        = compose [(origin, 1, a)]
    event t d x   = (delay (t .-. origin) . stretch d) (note x)
    compose       = mconcat . fmap (uncurry3 event)

class Performable a where
    perform :: a -> [(Time a, Duration a, Event a)]
    events  :: a -> [Event a]
    events = fmap trd3 . perform


align           :: (AffineSpace (Time a), HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a
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
anticipate      :: (Semigroup a, Transformable a, HasOnset a, HasOffset a, Ord d, d ~ Duration a) =>
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


retrograde      :: (Performable a, Composable a, HasOnset a, Ord (Duration a)) =>
                a -> a
     
mapAll          :: (Performable a, Composable b, Duration a ~ Duration b) => 
                ([(Time a, Duration a, Event a)] -> [(Time b, Duration b, Event b)]) -> a -> b

mapEvents       :: (Performable a, Composable b, Duration a ~ Duration b) =>
                (Time a -> Duration a -> Event a -> Event b) -> a -> b

filterEvents   :: (Performable a, Composable a) =>
                (Time a -> Duration a -> Event a -> Bool) -> a -> a

filter_         :: (Performable a, Composable a) => (Event a -> Bool) -> a -> a

mapFilterEvents :: (Performable a, Composable b, Duration a ~ Duration b) =>
                (Time a -> Duration a -> Event a -> Maybe (Event b)) -> a -> b

mapPhraseSingle      :: (Performable a, Composable b, Duration a ~ Duration b) =>
                (Event a -> Event b) -> (Event a -> Event b) -> (Event a -> Event b) -> a -> b


slice           :: (Ord (Duration a), AdditiveGroup (Duration a), Performable a, Composable a) =>
                Time a -> Time a -> a -> a
after           :: (Ord (Duration a), AdditiveGroup (Duration a), Performable a, Composable a) =>
                Time a -> a -> a
before          :: (Ord (Duration a), AdditiveGroup (Duration a), Performable a, Composable a) =>
                Time a -> a -> a

mapPhrase       :: (Performable a, Composable a, Composable b, Semigroup b,
                    HasPart' (Event a), Duration a ~ Duration b
                ) =>
                (Event a -> Event b) -> (Event a -> Event b) -> (Event a -> Event b) -> a -> b

mapAllParts     :: (Monoid' b, HasPart' (Event a), Performable a, Composable a) => 
                 ([a] -> [b]) -> a -> b




move            = delay
moveBack t      = delay (negateV t)
compress x      = stretch (recip x)
t `stretchTo` x = (t / duration x) `stretch` x
t `startAt` x   = (t .-. onset x) `delay` x
t `stopAt`  x   = (t .-. offset x) `delay` x

a `align` b =  startAt (offset a) b

a |> b =  a <> a `align` b
a <| b =  b |> a
scat = Prelude.foldr (|>) mempty
pcat = Prelude.foldr (<>) mempty

x `sustain` y     = x <> duration x `stretchTo` y
anticipate t a b  =  a <> startAt (offset a .-^ t) b

rest            = return Nothing
removeRests     = mcatMaybes

retrograde = startAt origin . (mapAll $ List.sortBy (comparing fst3) . fmap g)
    where
        g (t,d,x) = (negateP (t .+^ d), d, x)
        negateP a = origin .-^ (a .-. origin)


mapAll f                    = compose . f . perform
mapEvents f                 = mapAll $ fmap (third' f)
mapFilterEvents f           = mapAll $ mcatMaybes . fmap (unM . third' f)
    where
        unM (a,b,Nothing) = Nothing
        unM (a,b,Just c)  = Just (a,b,c)
filterEvents f = mapFilterEvents (partial3 f)
filter_ p = filterEvents (\t d x -> p x)

after  a                    = filterEvents (\t d _ -> a <= t)
before b                    = filterEvents (\t d _ -> t .+^ d <= b) 
slice  a b                  = filterEvents (\t d _ -> a <= t && t .+^ d <= b)

mapPhraseSingle f g h            = mapAll (mapFirstMiddleLast (third f) (third g) (third h))
mapPhrase f g h             = mapAllParts (fmap $ mapPhraseSingle f g h)

mapAllParts f   = mconcat . f . extractParts
    where
        extractParts a = fmap (`extractPart` a) (getParts a)
        extractPart v = filter_ ((== v) . getPart)
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
 




list z f []         = z
list z f xs         = f xs
fst3 (a, b, c)      = a
trd3 (a, b, c)      = c
curry3              = curry . curry . (. trip)
uncurry3            = (. untrip) . uncurry . uncurry
first3 f (a,b,c)    = (f a,b,c)
second3 f (a,b,c)   = (a,f b,c)
third f (a,b,c)     = (a,b,f c)
third' f (a,b,c)    = (a,b,f a b c)
mapFirstMiddleLast f g h = go
    where
        go []    = []
        go [a]   = [f a]
        go [a,b] = [f a, h b]
        go xs    = [f $ head xs]          ++ 
                   map g (tail $ init xs) ++ 
                   [h $ last xs]
untrip (a,b,c)      = ((a,b),c)
trip ((a,b),c)      = (a,b,c)
partial2 f          = curry  (fmap snd  . partial (uncurry f))
partial3 f          = curry3 (fmap trd3 . partial (uncurry3 f))



(
    times,
    repeated,
    group,
    delay_,
    stretch_,
    perform_
    ) = undefined