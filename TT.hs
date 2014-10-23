
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

import           Control.Applicative
import           Control.Applicative
import           Control.Applicative
import           Control.Comonad
import           Control.Lens               hiding (Indexable, Level, above,
                                             below, index, inside, parts,
                                             reversed, transform, (<|), (|>))
import           Control.Lens               hiding (Indexable, Level, above,
                                             below, index, inside, parts,
                                             reversed, transform, (<|), (|>))
import           Control.Monad
import           Control.Monad.Compose
import           Control.Monad.Plus
import           Data.Aeson                 (ToJSON (..))
import qualified Data.Aeson                 as JSON
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Bifunctor
import           Data.Clipped
import           Data.Distributive
import           Data.Foldable              (Foldable)
import qualified Data.Foldable              as Foldable
import           Data.Functor.Adjunction    (unzipR)
import           Data.Functor.Context
import           Data.Functor.Contravariant
import           Data.Functor.Couple
import           Data.Functor.Rep           as R
import           Data.Functor.Rep.Lens
import           Data.List                  (mapAccumL, mapAccumR)
import qualified Data.List
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromJust, isNothing, listToMaybe,
                                             maybeToList)
import           Data.NumInstances          ()
import           Data.Ord                   (comparing)
import           Data.Ratio
import           Data.Semigroup
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.String
import           Data.Traversable           (Traversable)
import           Data.Typeable
import           Data.VectorSpace           hiding (Sum (..))
import           Music.Dynamics.Literal
import           Music.Pitch.Alterable
import           Music.Pitch.Augmentable
import           Music.Pitch.Literal



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Time/Dur/Span
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


type TimeBase = Rational

newtype Duration = Duration { getDuration :: TimeBase }
  deriving (Eq, Ord, Num, Enum, Fractional, Real, RealFrac, Typeable)

instance Show Duration where
  show = showRatio . realToFrac

instance AdditiveGroup Duration where
  zeroV = 0
  (^+^) = (+)
  negateV = negate

instance VectorSpace Duration where
  type Scalar Duration = Duration
  (*^) = (*)

instance Semigroup Duration where
  (<>) = (*^)

instance Monoid Duration where
  mempty  = 1
  mappend = (*^)
 -- TODO use some notion of norm rather than 1

newtype Time = Time { getTime :: TimeBase }
  deriving (Eq, Ord, Num, Enum, Fractional, Real, RealFrac, Typeable)

instance Show Time where
  show = showRatio . realToFrac

deriving instance AdditiveGroup Time

instance VectorSpace Time where
  type Scalar Time = Duration
  Duration x *^ Time y = Time (x * y)

instance AffineSpace Time where
  type Diff Time = Duration
  Time x .-. Time y   = Duration (x - y)
  Time x .+^ Duration y = Time   (x + y)

instance Semigroup Time where
  (<>) = (^+^)

instance Monoid Time where
  mempty  = zeroV
  mappend = (^+^)
  mconcat = sumV

offsetPoints :: AffineSpace a => a -> [Diff a] -> [a]
offsetPoints = scanl (.+^)

toRelativeTime :: [Time] -> [Duration]
toRelativeTime = snd . mapAccumL g 0 where g prev t = (t, t .-. prev)

toRelativeTimeN :: [Time] -> [Duration]
toRelativeTimeN [] = []
toRelativeTimeN xs = toRelativeTimeN' (last xs) xs

toRelativeTimeN' :: Time -> [Time] -> [Duration]
toRelativeTimeN' end xs = snd $ mapAccumR g end xs where g prev t = (t, prev .-. t)

toAbsoluteTime :: [Duration] -> [Time]
toAbsoluteTime = tail . offsetPoints 0

newtype Span = Delta { _delta :: (Time, Duration) }
  deriving (Eq, Ord, Typeable)

instance Show Span where
  -- show = showDelta
  show = showRange
  -- Which form should we use?

instance Semigroup Span where
  (<>) = (^+^)

instance Monoid Span where
  mempty  = zeroV
  mappend = (^+^)

instance AdditiveGroup Span where
  zeroV   = 0 <-> 1
  Delta (t1, d1) ^+^ Delta (t2, d2) = Delta (t1 ^+^ d1 *^ t2, d1*d2)
  negateV (Delta (t, d)) = Delta (-t ^/ d, recip d)

infixl 6 <->
infixl 6 >->
infixl 6 <-<

(<->) :: Time -> Time -> Span
t <-> u = t >-> (u .-. t)

(>->) :: Time -> Duration -> Span
(>->) = curry Delta

(<-<) :: Duration -> Time -> Span
a <-< b = (b .-^ a) <-> b

range :: Iso' Span (Time, Time)
range = iso _range $ uncurry (<->)
  where
    _range x = let (t, d) = _delta x in (t, t .+^ d)

delta :: Iso' Span (Time, Duration)
delta = iso _delta Delta

codelta :: Iso' Span (Duration, Time)
codelta = iso _codelta $ uncurry (<-<)
  where
    _codelta x = let (t, d) = _delta x in (d, t .+^ d)

showRange :: Span -> String
showRange (view range -> (t,u)) = show t ++ " <-> " ++ show u

showDelta :: Span -> String
showDelta (view delta -> (t,d)) = show t ++ " >-> " ++ show d

showCodelta :: Span -> String
showCodelta (view codelta -> (d,u)) = show d ++ " <-< " ++ show u

delayComponent :: Span -> Time
delayComponent x = x ^. delta . _1

stretchComponent :: Span -> Duration
stretchComponent x = x ^. delta . _2

fixedDurationSpan :: Prism' Span Time
fixedDurationSpan = prism' (\t -> view (from delta) (t, 1)) $ \x -> case view delta x of
  (t, 1) -> Just t
  _      -> Nothing

fixedOnsetSpan :: Prism' Span Duration
fixedOnsetSpan = prism' (\d -> view (from delta) (0, d)) $ \x -> case view delta x of
  (0, d) -> Just d
  _      -> Nothing

isForwardSpan :: Span -> Bool
isForwardSpan = (> 0) . signum . _durationS

isBackwardSpan :: Span -> Bool
isBackwardSpan = (< 0) . signum . _durationS

isEmptySpan :: Span -> Bool
isEmptySpan = (== 0) . signum . _durationS

reverseSpan :: Span -> Span
reverseSpan s = reflectSpan (_midpointS s) s

reflectSpan :: Time -> Span -> Span
reflectSpan p = over (range . both) (reflectThrough p)

normalizeSpan :: Span -> Span
normalizeSpan s = if isForwardSpan s then s else reverseSpan s

isProper :: Span -> Bool
isProper (view range -> (t, u)) = t < u


infixl 5 `inside`
infixl 5 `encloses`
infixl 5 `properlyEncloses`
infixl 5 `overlaps`

inside :: Time -> Span -> Bool
inside x (view range -> (t, u)) = t <= x && x <= u

encloses :: Span -> Span -> Bool
a `encloses` b = _onsetS b `inside` a && _offsetS b `inside` a

properlyEncloses :: Span -> Span -> Bool
a `properlyEncloses` b = a `encloses` b && a /= b

afterOnset :: Time -> Span -> Bool
t `afterOnset` s = t >= _onsetS s

strictlyAfterOnset :: Time -> Span -> Bool
t `strictlyAfterOnset` s = t > _onsetS s

beforeOnset :: Time -> Span -> Bool
t `beforeOnset` s = t <= _onsetS s

strictlyBeforeOnset :: Time -> Span -> Bool
t `strictlyBeforeOnset` s = t < _onsetS s

afterOffset :: Time -> Span -> Bool
t `afterOffset` s = t >= _offsetS s

strictlyAfterOffset :: Time -> Span -> Bool
t `strictlyAfterOffset` s = t > _offsetS s

beforeOffset :: Time -> Span -> Bool
t `beforeOffset` s = t <= _offsetS s

strictlyBeforeOffset :: Time -> Span -> Bool
t `strictlyBeforeOffset` s = t < _offsetS s

startsWhenStarts :: Span -> Span -> Bool
a `startsWhenStarts` b = _onsetS a == _onsetS b

startsWhenStops :: Span -> Span -> Bool
a `startsWhenStops` b = _onsetS a == _offsetS b

stopsWhenStops :: Span -> Span -> Bool
a `stopsWhenStops` b = _offsetS a == _offsetS b

stopsWhenStarts :: Span -> Span -> Bool
a `stopsWhenStarts` b = _offsetS a == _onsetS b

startsBefore :: Span -> Span -> Bool
a `startsBefore` b = _onsetS a < _onsetS b

startsLater :: Span -> Span -> Bool
a `startsLater` b = _onsetS a > _onsetS b

stopsAtTheSameTime :: Span -> Span -> Bool
a `stopsAtTheSameTime` b = _offsetS a == _offsetS b

stopsBefore :: Span -> Span -> Bool
a `stopsBefore` b = _offsetS a < _offsetS b

stopsLater :: Span -> Span -> Bool
a `stopsLater` b = _offsetS a > _offsetS b

{-
contains
curtails
delays
happensDuring
intersects
trisects
isCongruentTo
overlapsAllOf
overlapsOnlyOnsetOf
overlapsOnlyOffsetOf
overlapsOnsetOf
overlapsOffsetOf

-}

overlaps :: Span -> Span -> Bool
a `overlaps` b = not (a `isBefore` b) && not (b `isBefore` a)

isBefore :: Span -> Span -> Bool
a `isBefore` b = (_onsetS a `max` _offsetS a) <= (_onsetS b `min` _offsetS b)

_onsetS    (view range -> (t1, t2)) = t1
_offsetS   (view range -> (t1, t2)) = t2
_midpointS  s = _onsetS s .+^ _durationS s / 2
_durationS s = _offsetS s .-. _onsetS s

{-
Two alternative definitions for midpoint:

midpoint x = onset x + duration x / 2
midpoint x = (onset x + offset x) / 2

Both equivalent. Proof:

  let d = b - a
  (a + b)/2 = a + d/2
  (a + b)/2 = a + (b - a)/2
  a + b     = 2a + (b - a)
  a + b     = a + b
-}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


class Transformable a where
  transform :: Span -> a -> a

instance Transformable () where
  transform _ = id

instance Transformable Bool where
  transform _ = id

instance Transformable Ordering where
  transform _ = id

instance Transformable Char where
  transform _ = id

instance Transformable Int where
  transform _ = id

instance Transformable Integer where
  transform _ = id

instance Transformable a => Transformable (Ratio a) where
  transform _ = id

instance Transformable Float where
  transform _ = id

instance Transformable Double where
  transform _ = id

instance Transformable Duration where
  (view delta -> (_, d1)) `transform` d2 = d1 * d2

instance Transformable Time where
  (view delta -> (t1, d1)) `transform` t2 = t1 ^+^ d1 *^ t2

instance Transformable Span where
  transform = (<>)

instance Transformable a => Transformable (Option a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (Last a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (Sum a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (Product a) where
  transform s = fmap (transform s)

instance Transformable a => Transformable (b, a) where
  transform t = fmap (transform t)

instance Transformable a => Transformable [a] where
  transform t = fmap (transform t)

  -- transform t = fmap (transform t)

instance (Ord a, Transformable a) => Transformable (Set a) where
  transform t = Set.map (transform t)

instance (Ord k, Transformable a) => Transformable (Map k a) where
  transform t = Map.map (transform t)

instance (Transformable a, Transformable b) => Transformable (a -> b) where
  transform t = (`whilst` negateV t)
    where
    f `whilst` t = over (transformed t) f

transformed :: (Transformable a, Transformable b) => Span -> Iso a b a b
transformed s = iso (transform s) (transform $ negateV s)

delaying :: Duration -> Span
delaying x = (0 .+^ x) >-> 1
delayingTime x = x >-> 1

stretching :: Duration -> Span
stretching x = 0 >-> x

undelaying :: Duration -> Span
undelaying x = delaying (negate x)

compressing :: Duration -> Span
compressing x = stretching (recip x)

delay :: Transformable a => Duration -> a -> a
delay = transform . delaying

undelay :: Transformable a => Duration -> a -> a
undelay = transform . undelaying

stretch :: Transformable a => Duration -> a -> a
stretch = transform . stretching

compress :: Transformable a => Duration -> a -> a
compress = transform . compressing

delayTime :: Transformable a => Time -> a -> a
delayTime = transform . delayingTime


lead   :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
a `lead` b   = placeAt 1 (b `_position` 0) a

follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
a `follow` b = placeAt 0 (a `_position` 1) b

after :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `after` b =  a <> (a `follow` b)

before :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `before` b =  (a `lead` b) <> b

infixr 6 |>
infixr 6 <|

(|>) :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(|>) = after

(<|) :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(<|) = before

scat :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => [a] -> a
scat = Prelude.foldr (|>) mempty

pcat :: (Semigroup a, Monoid a) => [a] -> a
pcat = Prelude.foldr (<>) mempty

during :: (HasPosition a, HasPosition b, Transformable a, Transformable b) => a -> b -> a
y `during` x = set era (view era x) y

sustain :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
x `sustain` y = x <> y `during` x

times :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => Int -> a -> a
times n = scat . replicate n







class HasDuration a where
  _duration :: a -> Duration

instance HasDuration Time where
  _duration = 0

instance HasDuration Duration where
  _duration = id

instance HasDuration Span where
  _duration = snd . view delta

instance HasDuration a => HasDuration (a, b) where
  _duration (d,_) = _duration d

instance HasDuration b => HasDuration (a, b, c) where
  _duration (_,d,_) = _duration d

instance HasDuration a => HasDuration (Product a) where
  _duration (Product x) = _duration x

instance HasDuration a => HasDuration (Sum a) where
  _duration (Sum x) = _duration x

instance HasDuration a => HasDuration (Min a) where
  _duration (Min x) = _duration x

instance HasDuration a => HasDuration (Max a) where
  _duration (Max x) = _duration x

instance (HasDuration a, HasDuration b) => HasDuration (Either a b) where
  _duration (Left x)  = _duration x
  _duration (Right x) = _duration x

duration :: (Transformable a, HasDuration a) => Lens' a Duration
duration = lens _duration (flip stretchTo)

stretchTo :: (Transformable a, HasDuration a) => Duration -> a -> a
stretchTo d x = (d ^/ _duration x) `stretch` x








class HasDuration a => HasPosition a where
  -- |
  -- Return the onset of the given value, or the value between the attack and decay phases.
  --
  _position :: a -> Duration -> Time
  _position x = alerp (_onset x) (_offset x)

  -- |
  -- Return the onset of the given value, or the value between the attack and decay phases.
  --
  _onset, _offset :: a -> Time
  _onset     = (`_position` 0)
  _offset    = (`_position` 1.0)

instance HasPosition Time where
  _position = const

instance HasPosition Span where
  -- Override as an optimization:
  _onset    (view range -> (t1, t2)) = t1
  _offset   (view range -> (t1, t2)) = t2
  _position (view range -> (t1, t2)) = alerp t1 t2

instance (HasPosition a, HasDuration a) => HasDuration [a] where
  _duration x = _offset x .-. _onset x

instance (HasPosition a, HasDuration a) => HasPosition [a] where
  _onset  = foldr min 0 . fmap _onset
  _offset = foldr max 0 . fmap _offset

_era :: HasPosition a => a -> Span
_era x = _onset x <-> _offset x

position :: (HasPosition a, Transformable a) => Duration -> Lens' a Time
position d = lens (`_position` d) (flip $ placeAt d)

onset :: (HasPosition a, Transformable a) => Lens' a Time
onset = position 0

offset :: (HasPosition a, Transformable a) => Lens' a Time
offset = position 1

preOnset :: (HasPosition a, Transformable a) => Lens' a Time
preOnset = position (-0.5)

midpoint :: (HasPosition a, Transformable a) => Lens' a Time
midpoint = position 0.5

postOnset :: (HasPosition a, Transformable a) => Lens' a Time
postOnset = position 0.5

postOffset :: (HasPosition a, Transformable a) => Lens' a Time
postOffset = position 1.5

startAt :: (Transformable a, HasPosition a) => Time -> a -> a
startAt t x = (t .-. _onset x) `delay` x

stopAt  :: (Transformable a, HasPosition a) => Time -> a -> a
stopAt t x = (t .-. _offset x) `delay` x

placeAt :: (Transformable a, HasPosition a) => Duration -> Time -> a -> a
placeAt p t x = (t .-. x `_position` p) `delay` x

_setEra :: (HasPosition a, Transformable a) => Span -> a -> a
_setEra s x = transform (s ^-^ view era x) x

era :: (HasPosition a, Transformable a) => Lens' a Span
era = lens _era (flip _setEra)

stretchRelative :: (HasPosition a, Transformable a) => Duration -> Duration -> a -> a
stretchRelative p n x = over (transformed $ undelaying (realToFrac $ x^.position p)) (stretch n) x

stretchRelativeOnset :: (HasPosition a, Transformable a) => Duration -> a -> a
stretchRelativeOnset = stretchRelative 0

stretchRelativeMidpoint :: (HasPosition a, Transformable a) => Duration -> a -> a
stretchRelativeMidpoint = stretchRelative 0.5

stretchRelativeOffset :: (HasPosition a, Transformable a) => Duration -> a -> a
stretchRelativeOffset = stretchRelative 1






rest :: Applicative f => f (Maybe a)
rest = pure Nothing





--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Delayed/Stretched/Note
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


newtype Delayed a = Delayed   { _delayedValue :: (Time, a) }
  deriving (Eq,
            Ord,
            Functor,
            Applicative,
            Monad,
            -- Comonad,
            Foldable,
            Traversable,
            Typeable)

instance (Show a, Transformable a) => Show (Delayed a) where
  show x = show (x^.from delayed) ++ "^.delayed"

instance Wrapped (Delayed a) where
  type Unwrapped (Delayed a) = (Time, a)
  _Wrapped' = iso _delayedValue Delayed

instance Rewrapped (Delayed a) (Delayed b)

instance Transformable (Delayed a) where
  transform t = over (_Wrapped . _1) (transform t)

instance HasDuration (Delayed a) where
  _duration x = _offset x .-. _onset x

instance HasPosition (Delayed a) where
  x `_position` p = fst (view _Wrapped x) `_position` p

instance IsString a => IsString (Delayed a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Delayed a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Delayed a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Delayed a) where
  fromDynamics = pure . fromDynamics

delayed :: Iso (Time, a) (Time, b) (Delayed a) (Delayed b)
delayed = _Unwrapped

delayedValue :: (Transformable a, Transformable b) => Lens (Delayed a) (Delayed b) a b
delayedValue = lens runDelayed $ flip (mapDelayed . const)

runDelayed :: Transformable a => Delayed a -> a
runDelayed = uncurry delayTime . view _Wrapped

mapDelayed :: (Transformable a, Transformable b) => (a -> b) -> Delayed a -> Delayed b
mapDelayed f (Delayed (t,x)) = Delayed (t, over (transformed (t >-> 1)) f x)







newtype Stretched a = Stretched { _stretchee :: Couple Duration a }
  deriving (
    Eq,           Num,      Fractional,   Floating,
    Ord,          Real,     RealFrac,     Functor,
    Applicative,  Monad,    Foldable,     Traversable, Typeable
    )
            -- Comonad,

instance Wrapped (Stretched a) where
  type Unwrapped (Stretched a) = (Duration, a)
  _Wrapped' = iso (getCouple . _stretchee) (Stretched . Couple)

instance Rewrapped (Stretched a) (Stretched b)

instance Transformable (Stretched a) where
  transform t = over (_Wrapped . _1) (transform t)

instance Splittable a => Splittable (Stretched a) where
  beginning d = over _Wrapped $ \(s, v) -> (beginning d s, beginning d v)
  ending    d = over _Wrapped $ \(s, v) -> (ending    d s, ending    d v)

instance HasDuration (Stretched a) where
  _duration = _duration . view _Wrapped

instance IsString a => IsString (Stretched a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Stretched a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Stretched a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Stretched a) where
  fromDynamics = pure . fromDynamics

instance (Show a, Transformable a) => Show (Stretched a) where
  show x = show (x^.from stretched) ++ "^.stretched"

stretched :: Iso (Duration, a) (Duration, b) (Stretched a) (Stretched b)
stretched = _Unwrapped

stretchee :: Transformable a => Lens (Stretched a) (Stretched a) a a
stretchee = _Wrapped `dependingOn` (transformed . stretching)

durationStretched :: Iso' Duration (Stretched ())
durationStretched = iso (\d -> (d,())^.stretched) (^.duration)

stretchedComplement :: Stretched a -> Stretched a
stretchedComplement (Stretched (Couple (d,x))) = Stretched $ Couple (negateV d, x)

newtype Note a = Note { _noteValue :: (Span, a) }
  deriving (Eq,
            Functor,
            Foldable,
            Traversable,
            Comonad,
            Typeable)

instance (Show a, Transformable a) => Show (Note a) where
  show x = show (x^.from note) ++ "^.note"

deriving instance Monad Note
deriving instance Applicative Note

instance Wrapped (Note a) where
  type Unwrapped (Note a) = (Span, a)
  _Wrapped' = iso _noteValue Note

instance Rewrapped (Note a) (Note b)

instance Transformable (Note a) where
  transform t = over (_Wrapped . _1) (transform t)

instance HasDuration (Note a) where
  _duration = _duration . fst . view _Wrapped

instance HasPosition (Note a) where
  x `_position` p = fst (view _Wrapped x) `_position` p

instance IsString a => IsString (Note a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Note a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Note a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Note a) where
  fromDynamics = pure . fromDynamics

note :: ({-Transformable a, Transformable b-}) => Iso (Span, a) (Span, b) (Note a) (Note b)
note = _Unwrapped

noteValue :: (Transformable a, Transformable b) => Lens (Note a) (Note b) a b
noteValue = lens runNote (flip $ mapNote . const)
  where
    runNote = uncurry transform . view _Wrapped
    -- setNote f (view (from note) -> (s,x)) = view note (s, itransform s x)
    mapNote f (view (from note) -> (s,x)) = view note (s, f `whilst` negateV s $ x)
    f `whilst` t = over (transformed t) f


event :: Iso (Note a) (Note b) (Time, Duration, a) (Time, Duration, b)
event = from note . bimapping delta id . tripped













newtype Track a = Track { getTrack :: TrackList (TrackEv a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

type TrackList = []

type TrackEv a = Delayed a

trackEv :: Iso (Delayed a) (Delayed b) (TrackEv a) (TrackEv b)
trackEv = id

instance Applicative Track where
  pure  = return
  (<*>) = ap

instance Alternative Track where
  (<|>) = (<>)
  empty = mempty

instance Monad Track where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

instance Wrapped (Track a) where
  type Unwrapped (Track a) = (TrackList (TrackEv a))
  _Wrapped' = iso getTrack Track

instance Rewrapped (Track a) (Track b)

instance Transformable (Track a) where
  transform s = over _Wrapped' (transform s)

instance HasPosition (Track a) where
  _onset  = safeMinimum . fmap _onset . view _Wrapped'
  _offset = safeMaximum . fmap _offset . view _Wrapped'

instance HasDuration (Track a) where
  _duration x = _offset x .-. _onset x

track :: Getter [Delayed a] (Track a)
track = from unsafeTrack


delayeds :: Lens (Track a) (Track b) [Delayed a] [Delayed b]
delayeds = unsafeTrack

unsafeTrack :: Iso (Track a) (Track b) [Delayed a] [Delayed b]
unsafeTrack = _Wrapped







newtype Voice a = Voice { getVoice :: VoiceList (VoiceEv a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Eq)

instance (Show a, Transformable a) => Show (Voice a) where
  show x = show (x^.stretcheds) ++ "^.voice"

type VoiceList = []

type VoiceEv a = Stretched a

voiceEv :: Iso (Stretched a) (Stretched b) (VoiceEv a) (VoiceEv b)
voiceEv = id

instance Applicative Voice where
  pure  = return
  (<*>) = ap

instance Alternative Voice where
  (<|>) = (<>)
  empty = mempty

instance Monad Voice where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

instance MonadPlus Voice where
  mzero = mempty
  mplus = mappend

instance Wrapped (Voice a) where
  type Unwrapped (Voice a) = (VoiceList (VoiceEv a))
  _Wrapped' = iso getVoice Voice

instance Rewrapped (Voice a) (Voice b)

instance Cons (Voice a) (Voice b) (Stretched a) (Stretched b) where
  _Cons = prism (\(s,v) -> (view voice.return $ s) <> v) $ \v -> case view stretcheds v of
    []      -> Left  mempty
    (x:xs)  -> Right (x, view voice xs)

instance Snoc (Voice a) (Voice b) (Stretched a) (Stretched b) where
  _Snoc = prism (\(v,s) -> v <> (view voice.return $ s)) $ \v -> case unsnoc (view stretcheds v) of
    Nothing      -> Left  mempty
    Just (xs, x) -> Right (view voice xs, x)

instance Transformable (Voice a) where
  transform s = over _Wrapped' (transform s)

instance HasDuration (Voice a) where
  _duration = Foldable.sum . fmap _duration . view _Wrapped'

instance Splittable a => Splittable (Voice a) where
  split t x
    | t <= 0           = (mempty, x)
    | t >= _duration x = (x,      mempty)
    | otherwise        = let (a,b) = split' t {-split-} (x^._Wrapped) in (a^._Unwrapped, b^._Unwrapped)
    where
      split' = error "TODO"

instance IsString a => IsString (Voice a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Voice a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Voice a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Voice a) where
  fromDynamics = pure . fromDynamics

instance Enum a => Enum (Voice a) where
  toEnum = return . toEnum
  fromEnum = list 0 (fromEnum . head) . Foldable.toList

instance Num a => Num (Voice a) where
  fromInteger = return . fromInteger
  abs    = fmap abs
  signum = fmap signum
  (+)    = error "Not implemented"
  (-)    = error "Not implemented"
  (*)    = error "Not implemented"

instance AdditiveGroup (Voice a) where
  zeroV   = error "Not implemented"
  (^+^)   = error "Not implemented"
  negateV = error "Not implemented"

instance VectorSpace (Voice a) where
  type Scalar (Voice a) = Duration
  d *^ s = d `stretch` s

voice :: Getter [Stretched a] (Voice a)
voice = from unsafeStretcheds


stretcheds :: Lens (Voice a) (Voice b) [Stretched a] [Stretched b]
stretcheds = unsafeStretcheds


eventsV :: Lens (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
eventsV = unsafeEventsV


unsafeEventsV :: Iso (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
unsafeEventsV = iso (map (^.from stretched) . (^.stretcheds)) ((^.voice) . map (^.stretched))


unsafeStretcheds :: Iso (Voice a) (Voice b) [Stretched a] [Stretched b]
unsafeStretcheds = _Wrapped


unzipVoice :: Voice (a, b) -> (Voice a, Voice b)
unzipVoice = unzipR

zipVoice :: Voice a -> Voice b -> Voice (a, b)
zipVoice = zipVoiceWith (,)

zipVoice3 :: Voice a -> Voice b -> Voice c -> Voice (a, (b, c))
zipVoice3 a b c = zipVoice a (zipVoice b c)

zipVoice4 :: Voice a -> Voice b -> Voice c -> Voice d -> Voice (a, (b, (c, d)))
zipVoice4 a b c d = zipVoice a (zipVoice b (zipVoice c d))

zipVoice5 :: Voice a -> Voice b -> Voice c -> Voice d -> Voice e -> Voice (a, (b, (c, (d, e))))
zipVoice5 a b c d e = zipVoice a (zipVoice b (zipVoice c (zipVoice d e)))

zipVoiceNoScale :: Voice a -> Voice b -> Voice (a, b)
zipVoiceNoScale = zipVoiceWithNoScale (,)

zipVoiceNoScale3 :: Voice a -> Voice b -> Voice c -> Voice (a, (b, c))
zipVoiceNoScale3 a b c = zipVoiceNoScale a (zipVoiceNoScale b c)

zipVoiceNoScale4 :: Voice a -> Voice b -> Voice c -> Voice d -> Voice (a, (b, (c, d)))
zipVoiceNoScale4 a b c d = zipVoiceNoScale a (zipVoiceNoScale b (zipVoiceNoScale c d))

zipVoiceNoScale5 :: Voice a -> Voice b -> Voice c -> Voice d -> Voice e -> Voice (a, (b, (c, (d, e))))
zipVoiceNoScale5 a b c d e = zipVoiceNoScale a (zipVoiceNoScale b (zipVoiceNoScale c (zipVoiceNoScale d e)))

zipVoiceWith :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith = zipVoiceWith' (*)

zipVoiceWithNoScale :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWithNoScale = zipVoiceWith' const

zipVoiceWith' :: (Duration -> Duration -> Duration) -> (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith' f g
  ((unzip.view eventsV) -> (ad, as))
  ((unzip.view eventsV) -> (bd, bs))
  = let cd = zipWith f ad bd
        cs = zipWith g as bs
     in view (from unsafeEventsV) (zip cd cs)

fuse :: Eq a => Voice a -> Voice a
fuse = fuseBy (==)

fuseBy :: (a -> a -> Bool) -> Voice a -> Voice a
fuseBy p = fuseBy' p head

fuseBy' :: (a -> a -> Bool) -> ([a] -> a) -> Voice a -> Voice a
fuseBy' p g = over unsafeEventsV $ fmap foldNotes . Data.List.groupBy (inspectingBy snd p)
  where
    -- Add up durations and use a custom function to combine notes
    --
    -- Typically, the combination function us just 'head', as we know that group returns
    -- non-empty lists of equal elements.
    foldNotes (unzip -> (ds, as)) = (sum ds, g as)

fuseRests :: Voice (Maybe a) -> Voice (Maybe a)
fuseRests = fuseBy (\x y -> isNothing x && isNothing y)

coverRests :: Voice (Maybe a) -> Maybe (Voice a)
coverRests x = if hasOnlyRests then Nothing else Just (fmap fromJust $ fuseBy merge x)
  where
    norm = fuseRests x
    merge Nothing  Nothing  = error "Voice normalized, so consecutive rests are impossible"
    merge (Just x) Nothing  = True
    merge Nothing  (Just x) = True
    merge (Just x) (Just y) = False
    hasOnlyRests = all isNothing $ toListOf traverse x -- norm

withContext :: Voice a -> Voice (Ctxt a)
withContext = over valuesV addCtxt

voiceFromRhythm :: [Duration] -> Voice ()
voiceFromRhythm = mkVoice . fmap (, ())

mkVoice = view voice . fmap (view stretched)

durationsV :: Lens' (Voice a) [Duration]
durationsV = lens getDurs (flip setDurs)
  where
    getDurs :: Voice a -> [Duration]
    getDurs = map fst . view eventsV

    setDurs :: [Duration] -> Voice a -> Voice a
    setDurs ds as = zipVoiceWith' (\a b -> a) (\a b -> b) (mconcat $ map durToVoice ds) as

    durToVoice d = stretch d $ pure ()

valuesV :: Lens (Voice a) (Voice b) [a] [b]
valuesV = lens getValues (flip setValues)
  where
    -- getValues :: Voice a -> [a]
    getValues = map snd . view eventsV

    -- setValues :: [a] -> Voice b -> Voice a
    setValues as bs = zipVoiceWith' (\a b -> b) (\a b -> a) (listToVoice as) bs

    listToVoice = mconcat . map pure

-- withDurations :: ([Duration] -> [Duration]) -> Voice a -> Voice a
-- withDurations = over durationsV
--
-- withValues :: ([a] -> [b]) -> Voice a -> Voice b
-- withValues = over valuesV
--
-- rotateDurations :: Int -> Voice a -> Voice a
-- rotateDurations n = over durationsV (rotate n)
--
-- rotateValues :: Int -> Voice a -> Voice a
-- rotateValues n = over valuesV (rotate n)
--
-- reverseDurations :: Voice a -> Voice a
-- reverseDurations = over durationsV reverse
--
-- reverseValues :: Voice a -> Voice a
-- reverseValues = over valuesV reverse

voiceLens :: (s -> a) -> (b -> s -> t) -> Lens (Voice s) (Voice t) (Voice a) (Voice b)
voiceLens getter setter = lens (fmap getter) (flip $ zipVoiceWithNoScale setter)

voiceL l = voiceLens (view $ cloneLens l) (set $ cloneLens l)

-- voiceAsList :: Iso (Voice a) (Voice b) [a] [b]
-- voiceAsList = iso voiceToList listToVoice
--   where
--     voiceToList = map snd . view eventsV
--     listToVoice = mconcat . fmap pure
--
-- listAsVoice :: Iso [a] [b] (Voice a) (Voice b)
-- listAsVoice = from voiceAsList
--
-- headV, lastV :: Voice a -> Maybe (Stretched a)
-- headV = preview _head
-- lastV = preview _head
--
-- tailV, initV :: Voice a -> Maybe (Voice a)
-- tailV = preview _tail
-- initV = preview _init
--
-- consV :: Stretched a -> Voice a -> Voice a
-- unconsV :: Voice a -> Maybe (Stretched a, Voice a)
-- consV = cons
-- unconsV = uncons
--
-- snocV :: Voice a -> Stretched a -> Voice a
-- unsnocV :: Voice a -> Maybe (Voice a, Stretched a)
-- snocV = snoc
-- unsnocV = unsnoc
--
-- nullV :: Voice a -> Bool
-- nullV = nullOf eventsV
--
-- lengthV :: Voice a -> Int
-- lengthV = lengthOf eventsV
--
-- mapV :: (a -> b) -> Voice a -> Voice b
-- mapV = fmap

sameDurations :: Voice a -> Voice b -> Bool
sameDurations a b = view durationsV a == view durationsV b

mergeIfSameDuration :: Voice a -> Voice b -> Maybe (Voice (a, b))
mergeIfSameDuration = mergeIfSameDurationWith (,)

mergeIfSameDurationWith :: (a -> b -> c) -> Voice a -> Voice b -> Maybe (Voice c)
mergeIfSameDurationWith f a b
  | sameDurations a b = Just $ zipVoiceWithNoScale f a b
  | otherwise         = Nothing

splitLatterToAssureSameDuration :: Voice b -> Voice b -> Voice b
splitLatterToAssureSameDuration = splitLatterToAssureSameDurationWith dup
  where
    dup x = (x,x)

splitLatterToAssureSameDurationWith :: (b -> (b, b)) -> Voice b -> Voice b -> Voice b
splitLatterToAssureSameDurationWith = undefined

polyToHomophonic      :: [Voice a] -> Maybe (Voice [a])
polyToHomophonic = undefined

polyToHomophonicForce :: [Voice a] -> Voice [a]
polyToHomophonicForce = undefined

homoToPolyphonic      :: Voice [a] -> [Voice a]
homoToPolyphonic = undefined

changeCrossing   :: Ord a => Voice a -> Voice a -> (Voice a, Voice a)
changeCrossing = undefined

changeCrossingBy :: Ord b => (a -> b) -> Voice a -> Voice a -> (Voice a, Voice a)
changeCrossingBy = undefined

processExactOverlaps :: (a -> a -> (a, a)) -> Voice a -> Voice a -> (Voice a, Voice a)
processExactOverlaps = undefined

processExactOverlaps' :: (a -> b -> Either (a,b) (b,a)) -> Voice a -> Voice b -> (Voice (Either b a), Voice (Either a b))
processExactOverlaps' = undefined

onsetsRelative    :: Time -> Voice a -> [Time]
onsetsRelative = undefined

offsetsRelative   :: Time -> Voice a -> [Time]
offsetsRelative = undefined

midpointsRelative :: Time -> Voice a -> [Time]
midpointsRelative = undefined

erasRelative      :: Time -> Voice a -> [Span]
erasRelative = undefined

onsetMap  :: Time -> Voice a -> Map Time a
onsetMap = undefined

offsetMap :: Time -> Voice a -> Map Time a
offsetMap = undefined

midpointMap :: Time -> Voice a -> Map Time a
midpointMap = undefined

eraMap :: Time -> Voice a -> Map Span a
eraMap = undefined

durations :: Voice a -> [Duration]
durations = undefined

{-

sameDurations           :: Voice a -> Voice b -> Bool
mergeIfSameDuration     :: Voice a -> Voice b -> Maybe (Voice (a, b))
mergeIfSameDurationWith :: (a -> b -> c) -> Voice a -> Voice b -> Maybe (Voice c)
splitAt :: [Duration] -> Voice a -> [Voice a]
splitLatterToAssureSameDuration :: Voice b -> Voice b -> Voice b
splitLatterToAssureSameDurationWith :: (b -> (b, b)) -> Voice b -> Voice b -> Voice b
polyToHomophonic      :: [Voice a] -> Maybe (Voice [a])
polyToHomophonicForce :: [Voice a] -> Voice [a]
homoToPolyphonic      :: Voice [a] -> [Voice a]
joinVoice             :: Voice (Voice a) -> a
changeCrossing   :: Ord a => Voice a -> Voice a -> (Voice a, Voice a)
changeCrossingBy :: Ord b => (a -> b) -> Voice a -> Voice a -> (Voice a, Voice a)
processExactOverlaps :: (a -> a -> (a, a)) -> Voice a -> Voice a -> (Voice a, Voice a)
processExactOverlaps' :: (a -> b -> Either (a,b) (b,a)) -> Voice a -> Voice b -> (Voice (Either b a), Voice (Either a b))
onsetsRelative    :: Time -> Voice a -> [Time]
offsetsRelative   :: Time -> Voice a -> [Time]
midpointsRelative :: Time -> Voice a -> [Time]
erasRelative      :: Time -> Voice a -> [Span]
onsetMap  :: Time -> Voice a -> Map Time a
offsetMap :: Time -> Voice a -> Map Time a
midpointMap :: Time -> Voice a -> Map Time a
eraMap :: Time -> Voice a -> Map Span a
durations :: Voice a -> [Duration]
values    :: Voice a -> [a] -- Same as Foldable.toList
isPossiblyInfinite :: Voice a -> Bool
hasMelodicDissonanceWith :: (a -> a -> Bool) -> Voice a -> Bool
hasIntervalWith :: AffineSpace a => (Diff a -> Bool) -> Voice a -> Bool
hasDurationWith :: (Duration -> Bool) -> Voice a -> Bool
reifyVoice :: Voice a -> Voice (Duration, a)
mapWithIndex :: (Int -> a -> b) -> Voice a -> Voice b
mapWithDuration :: (Duration -> a -> b) -> Voice a -> Voice b
mapWithIndexAndDuration :: (Int -> Duration -> a -> b) -> Voice a -> Voice b
_ :: Iso (Voice ()) [Duration]
asingleton' :: Prism (Voice a) (Duration, a)
asingleton :: Prism (Voice a) a
separateVoicesWith :: (a -> k) -> Voice a -> Map k (Voice a)
freeVoiceR :: (forall a. -> [a] -> a)          -> Voice a -> (a, Duration)
freeVoiceRNoDur :: ([a] -> a)          -> Voice a -> a
freeVoice  :: (forall a. -> [a] -> [a])        -> Voice a -> Voice a
freeVoice2 :: (forall a. -> [a] -> [a] -> [a]) -> Voice a -> Voice a -> Voice a
empty :: Voice a
singleton :: a -> Voice a
cons :: a -> Voice a -> Voice a
snoc :: Voice a -> a -> Voice a
append :: Voice a -> Voice a -> Voice a
ap :: Voice (a -> b) -> Voice a -> Voice b
apDur :: Voice (Duration -> Duration -> a -> b) -> Voice a -> Voice b
intersperse :: Duration -> a -> Voice a -> Voice a
subsequences :: Voice a -> [Voice a]
permutations :: Voice a -> [Voice a]
iterate :: (a -> a) -> a -> Voice a
repeat :: a -> Voice a
replicate :: Int -> a -> Voice a
unfoldr :: (b -> Maybe (a, b)) -> b -> Voice a
Differences between Voice and Chord (except the obviously different composition styles):
  - Voice is a Monoid, Chord just a Semigroup (??)
  - TODO represent spanners using (Voice a, Map (Int,Int) s)
  Arguably this should be part of Voice
  TODO the MVoice/TVoice stuff
newtype MVoice = Voice (Maybe a)
newtype PVoice = [Either Duration (Voice a)]
expandRepeats :: [Voice (Variant a)] -> Voice a

-}





newtype Chord a = Chord { getChord :: ChordList (ChordEv a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

type ChordList = []

type ChordEv a = Delayed a

chordEv :: Iso (Delayed a) (Delayed b) (ChordEv a) (ChordEv b)
chordEv = id

instance Applicative Chord where
  pure  = return
  (<*>) = ap

instance Monad Chord where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

instance Wrapped (Chord a) where
  type Unwrapped (Chord a) = (ChordList (ChordEv a))
  _Wrapped' = iso getChord Chord

instance Rewrapped (Chord a) (Chord b)

instance Transformable (Chord a) where
  transform s = over _Wrapped' (transform s)

instance HasDuration (Chord a) where
  _duration = Foldable.sum . fmap _duration . view _Wrapped'

instance Splittable a => Splittable (Chord a) where
  -- TODO

chord :: Getter [Delayed a] (Chord a)
chord = from unsafeChord

unchord :: Lens (Chord a) (Chord b) [Delayed a] [Delayed b]
unchord = _Wrapped

unsafeChord :: Iso (Chord a) (Chord b) [Delayed a] [Delayed b]
unsafeChord = _Wrapped

instance IsString a => IsString (Chord a) where
  fromString = pure . fromString

deriving instance IsPitch a => IsPitch (Chord a)
deriving instance IsInterval a => IsInterval (Chord a)
deriving instance IsDynamics a => IsDynamics (Chord a)

{-
invertC :: Transposable a => Chord a -> Chord a
invertC = over chord (rotlAnd $ up _P8)

inversions :: Transposable a => Chord a -> [Chord a]
inversions = iterate invertC

chordToScore :: Chord a -> Score a
chordToScore = pcat . map pure . toListOf traverse

unchord =  toListOf traverse

arpUp3 :: Chord a -> Score a
arpUp3 x = scat $ map ((^/16) . pure) [a,b,c]
  where
    [a,b,c] = unchord x

arpDown3 :: Chord a -> Score a
arpDown3 x = scat $ map ((^/16) . pure) [c,b,a]
  where
    [a,b,c] = unchord x

arpUpDown3 x = arpUp3 x |> arpDown3 x
arpDownUp3 x = arpDown3 x |> arpUp3 x

alberti3 :: Chord a -> Score a
alberti3 x = scat $ map ((^/16) . pure) [a,c,b,c]
  where
    [a,b,c] = unchord x

triad :: Transposable a => a -> Chord a
triad x = mconcat $ map pure [x, up _M3 x, up _P5 x]

mtriad :: Transposable a => a -> Chord a
mtriad x = mconcat $ map pure [x, up m3 x, up _P5 x]

sixthChord       = down m3 . invertC . mtriad
sixthFourthChord = down _P5 . invertC . invertC . triad

fromBass :: Transposable a => String -> a -> Chord a
fromBass "" x = triad x
-}




--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Score
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


type ScoreNote a = Note a

newtype Score a = Score { getScore' :: (Meta, NScore a) }
    deriving (Functor, Semigroup, Monoid, Foldable, Traversable, Typeable{-, Show, Eq, Ord-})

instance Wrapped (Score a) where
  type Unwrapped (Score a) = (Meta, NScore a)
  _Wrapped' = iso getScore' Score

instance Rewrapped (Score a) (Score b) where

instance Applicative Score where
  pure = return
  (<*>) = ap

instance Monad Score where
  return = (^. _Unwrapped') . return . return
  xs >>= f = (^. _Unwrapped') $ mbind ((^. _Wrapped') . f) ((^. _Wrapped') xs)

instance Alternative Score where
  empty = mempty
  (<|>) = mappend

instance MonadPlus Score where
  mzero = mempty
  mplus = mappend

{-
instance FunctorWithIndex Span Score where
  imap f = over (_Wrapped._2) $ imap f

instance FoldableWithIndex Span Score where
  ifoldMap f (Score (m,x)) = ifoldMap f x

instance TraversableWithIndex Span Score where
  itraverse f (Score (m,x)) = fmap (\x -> Score (m,x)) $ itraverse f x
-}

instance Transformable (Score a) where
  transform t (Score (m,x)) = Score (transform t m, transform t x)

instance HasPosition (Score a) where
  _position = _position . snd . view _Wrapped' {-. normalizeScore'-}
  -- TODO clean up in terms of AddMeta and optimize

instance HasDuration (Score a) where
  _duration x = _offset x .-. _onset x

instance IsString a => IsString (Score a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Score a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Score a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Score a) where
  fromDynamics = pure . fromDynamics

instance Enum a => Enum (Score a) where
  toEnum = return . toEnum
  fromEnum = list 0 (fromEnum . head) . Foldable.toList

instance Num a => Num (Score a) where
  fromInteger = return . fromInteger
  abs    = fmap abs
  signum = fmap signum
  (+)    = error "Not implemented"
  (-)    = error "Not implemented"
  (*)    = error "Not implemented"

instance AdditiveGroup (Score a) where
  zeroV   = error "Not implemented"
  (^+^)   = error "Not implemented"
  negateV = error "Not implemented"

instance VectorSpace (Score a) where
  type Scalar (Score a) = Duration
  d *^ s = d `stretch` s

instance HasMeta (Score a) where
  meta = _Wrapped . _1

newtype NScore a = NScore { getNScore :: [ScoreNote a] }
  deriving ({-Eq, -}{-Ord, -}{-Show, -}Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

instance (Show a, Transformable a) => Show (Score a) where
  show x = show (x^.notes) ++ "^.score"

instance Wrapped (NScore a) where
  type Unwrapped (NScore a) = [ScoreNote a]
  _Wrapped' = iso getNScore NScore

instance Rewrapped (NScore a) (NScore b)

instance Applicative NScore where
  pure  = return
  (<*>) = ap

instance Monad NScore where
  return = (^. _Unwrapped) . pure . pure
  xs >>= f = (^. _Unwrapped) $ mbind ((^. _Wrapped') . f) ((^. _Wrapped') xs)

instance Alternative NScore where
  empty = mempty
  (<|>) = mappend

instance MonadPlus NScore where
  mzero = mempty
  mplus = mappend

instance Transformable (NScore a) where
  transform t (NScore xs) = NScore (fmap (transform t) xs)

instance HasPosition (NScore a) where
  _onset  = safeMinimum . fmap (_onset  . normalizeSpan) . toListOf (_Wrapped . each . era)
  _offset = safeMaximum . fmap (_offset . normalizeSpan) . toListOf (_Wrapped . each . era)

safeMinimum xs = if null xs then 0 else minimum xs
safeMaximum xs = if null xs then 0 else maximum xs

instance HasDuration (NScore a) where
  _duration x = _offset x .-. _onset x

score :: Getter [Note a] (Score a)
score = from unsafeNotes


notes :: Lens (Score a) (Score b) [Note a] [Note b]
notes = _Wrapped . _2 . _Wrapped . sorted
  where
    -- TODO should not have to sort...
    sorted = iso (Data.List.sortBy (Data.Ord.comparing _onset)) (Data.List.sortBy (Data.Ord.comparing _onset))


unsafeNotes :: Iso (Score a) (Score b) [Note a] [Note b]
unsafeNotes = _Wrapped . noMeta . _Wrapped . sorted
  where
    sorted = iso (Data.List.sortBy (Data.Ord.comparing _onset)) (Data.List.sortBy (Data.Ord.comparing _onset))
    noMeta = iso extract return
    -- noMeta = iso (\(_,x) -> x) (\x -> (mempty,x))

unsafeEvents :: Iso (Score a) (Score b) [(Time, Duration, a)] [(Time, Duration, b)]
unsafeEvents = iso _getScore _score
  where
    _score :: [(Time, Duration, a)] -> Score a
    _score = mconcat . fmap (uncurry3 event)
      where
        event t d x   = (delay (t .-. 0) . stretch d) (return x)

    _getScore :: {-Transformable a => -}Score a -> [(Time, Duration, a)]
    _getScore =
      fmap (\(view delta -> (t,d),x) -> (t,d,x)) .
      Data.List.sortBy (Data.Ord.comparing fst) .
      Foldable.toList .
      fmap (view $ from note) .
      reifyScore

mapScore :: (Note a -> b) -> Score a -> Score b
mapScore f = over (_Wrapped._2) (mapNScore f)
  where
    mapNScore f = over (_Wrapped.traverse) (extend f)

reifyScore :: Score a -> Score (Note a)
reifyScore = over (_Wrapped . _2 . _Wrapped) $ fmap duplicate

events :: {-Transformable a => -}Lens (Score a) (Score b) [(Time, Duration, a)] [(Time, Duration, b)]
events = notes . _zipList . through event event . from _zipList

-- mapWithSpan :: (Span -> a -> b) -> Score a -> Score b
-- mapWithSpan f = mapScore (uncurry f . view (from note))
--
-- filterWithSpan :: (Span -> a -> Bool) -> Score a -> Score a
-- filterWithSpan f = mapFilterWithSpan (partial2 f)
--
-- mapFilterWithSpan :: (Span -> a -> Maybe b) -> Score a -> Score b
-- mapFilterWithSpan f = mcatMaybes . mapWithSpan f
--
-- mapEvents :: (Time -> Duration -> a -> b) -> Score a -> Score b
-- mapEvents f = mapWithSpan (uncurry f . view delta)
--
-- filterEvents   :: (Time -> Duration -> a -> Bool) -> Score a -> Score a
-- filterEvents f = mapFilterEvents (partial3 f)
--
-- mapFilterEvents :: (Time -> Duration -> a -> Maybe b) -> Score a -> Score b
-- mapFilterEvents f = mcatMaybes . mapEvents f
--
normalizeScore :: Score a -> Score a
normalizeScore = reset . normalizeScoreDurations
  where
    reset x = set onset (view onset x `max` 0) x
    normalizeScoreDurations = over (notes . each . era) normalizeSpan

-- printEras :: Score a -> IO ()
-- printEras = mapM_ print . toListOf eras
--
-- eras :: Traversal' (Score a) Span
-- eras = notes . each . era
--
-- chordEvents :: Transformable a => Span -> Score a -> [a]
-- chordEvents s = fmap extract . filter ((== s) . view era) . view notes
--
-- simultaneous' :: Transformable a => Score a -> Score [a]
-- simultaneous' sc = (^. from unsafeEvents) vs
--   where
--     -- es :: [Era]
--     -- evs :: [[a]]
--     -- vs :: [(Time, Duration, [a])]
--     es  = Data.List.nub $ toListOf eras sc
--     evs = fmap (`chordEvents` sc) es
--     vs  = zipWith (\(view delta -> (t,d)) a -> (t,d,a)) es evs
--
-- simultaneous :: (Transformable a, Semigroup a) => Score a -> Score a
-- simultaneous = fmap (sconcat . NonEmpty.fromList) . simultaneous'
--
-- simult :: Transformable a => Lens (Score a) (Score b) (Score [a]) (Score [b])
-- simult = iso simultaneous' mscatter
--
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Future/Past/Segment/Reactive/Behavior stuff
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------



newtype Past a = Past { getPast :: (Min Time, a) }
  deriving (Eq, Ord, Functor)

newtype Future a = Future { getFuture :: (Max Time, a) }
  deriving (Eq, Ord, Functor)

instance HasDuration (Past a) where
  _duration _ = 0

instance HasDuration (Future a) where
  _duration _ = 0

instance HasPosition (Past a) where
  _position (Past (extract -> t,_)) _ = t

instance HasPosition (Future a) where
  _position (Future (extract -> t,_)) _ = t

past :: Past a -> Time -> Maybe a
past (Past (extract -> t, x)) t'
  | t' <= t    = Just x
  | otherwise  = Nothing

future :: Future a -> Time -> Maybe a
future (Future (extract -> t, x)) t'
  | t' >= t    = Just x
  | otherwise  = Nothing

indexPast :: [Past a] -> Time -> Maybe a
indexPast ps t = firstTrue $ fmap (\p -> past p t) $ Data.List.sortBy (comparing _onset) ps

firstTrue :: [Maybe a] -> Maybe a
firstTrue = listToMaybe . join . fmap maybeToList

pastSeg :: Past (Segment a) -> Behavior (Maybe a)
pastSeg = undefined

futureSeg :: Future (Segment a) -> Behavior (Maybe a)
futureSeg = undefined










newtype Segment a = Segment { getSegment :: Clipped Duration -> a }
  deriving (Functor, Applicative, Monad{-, Comonad-}, Typeable)

instance Show (Segment a) where
  show _ = "<<Segment>>"

instance Distributive Segment where
  distribute = Segment . distribute . fmap getSegment

instance Representable Segment where
  type Rep Segment = Duration
  tabulate f = Segment (f . fromClipped)
  index    (Segment f) = f . unsafeToClipped

instance Transformable (Segment a) where
  transform _ = id

segment :: Iso (Duration -> a) (Duration -> b) (Segment a) (Segment b)
segment = R.tabulated

apSegments' :: Stretched (Segment a) -> Stretched (Segment a) -> Stretched (Segment a)
apSegments' (view (from stretched) -> (d1,s1)) (view (from stretched) -> (d2,s2))
  = view stretched (d1+d2, slerp (d1/(d1+d2)) s1 s2)

apSegments :: Voice (Segment a) -> Stretched (Segment a)
apSegments = foldr1 apSegments' . toListOf (stretcheds . each)

slerp :: Duration -> Segment a -> Segment a -> Segment a
slerp i a b
  | i < 0 || i >= 1    = error "slerp: Bad value"
  | otherwise = tabulate $ \t -> if t < i then a ! (t/i) else b ! ((t-i)/(1-i))

slerp2 :: (a -> a -> a) -> Duration -> Segment a -> Segment a -> Segment a
slerp2 f i a b
  | i < 0 || i >= 1    = error "slerp: Bad value"
  | otherwise = tabulate $ \t -> case t `compare` i of
      LT -> a ! (t/i)
      EQ -> (a ! 1) `f` (b ! 1)
      GT -> b ! ((t-i)/(1-i))

bounded' :: Iso' (Note (Segment a)) (Bound (Behavior a))
bounded' = bounded

bounded :: Iso (Note (Segment a)) (Note (Segment b)) (Bound (Behavior a)) (Bound (Behavior b))
bounded = iso ns2bb bb2ns
  where
    bb2ns (Bound (s, x)) = view note (s, b2s $ transform (negateV s) $ x)
    ns2bb (view (from note) -> (s, x)) = Bound (s,       transform s           $ s2b $ x)
    s2b = under R.tabulated (. realToFrac)
    b2s = under R.tabulated (. realToFrac)

trim :: Monoid b => Bound (Behavior b) -> Behavior b
trim = trimG
  where
    trimG :: (Monoid b, Representable f, Rep f ~ Time) => Bound (f b) -> f b
    trimG (Bound (s, x)) = tabulate (trimOutside s) `apRep` x

trimOutside :: Monoid a => Span -> Time -> a -> a
trimOutside s t x = if t `inside` s then x else mempty

splice :: Behavior a -> Bound (Behavior a) -> Behavior a
splice constant insert = fmap fromLast $ fmap toLast constant <> trim (fmap (fmap toLast) insert)
  where
    toLast   = Option . Just . Last
    fromLast = getLast . fromJust . getOption
    -- fromJust is safe here, as toLast is used to create the Maybe wrapper

concatSegment :: Monoid a => Note (Segment a) -> Behavior a
concatSegment = trim . view bounded

concatS :: Monoid a => Score (Segment a) -> Behavior a
concatS = mconcat . map concatSegment . view notes

concatB :: Monoid a => Score (Behavior a) -> Behavior a
concatB = concatS . fmap (view focusing)

focusing :: Lens' (Behavior a) (Segment a)
focusing = lens get set
  where
    get = view (from bounded . noteValue) . {-pure-}bounding mempty
    set x = splice x . (view bounded) . pure











newtype Reactive a = Reactive { getReactive :: ([Time], Behavior a) }
    deriving (Functor, Semigroup, Monoid, Typeable)

instance Transformable (Reactive a) where
    transform s (Reactive (t,r)) = Reactive (transform s t, transform s r)

instance Reversible (Reactive a) where
  rev = stretch (-1)

instance Wrapped (Reactive a) where
    type Unwrapped (Reactive a) = ([Time], Behavior a)
    _Wrapped' = iso getReactive Reactive

instance Rewrapped (Reactive a) (Reactive b)
instance Applicative Reactive where
    pure  = pureDefault
      where
        pureDefault = view _Unwrapped . pure . pure

    (<*>) = apDefault
      where
        (view _Wrapped -> (tf, rf)) `apDefault` (view _Wrapped -> (tx, rx)) = view _Unwrapped (tf <> tx, rf <*> rx)

instance IsPitch a => IsPitch (Reactive a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Reactive a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Reactive a) where
  fromDynamics = pure . fromDynamics

instance Alterable a => Alterable (Reactive a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Augmentable a => Augmentable (Reactive a) where
    augment = fmap augment
    diminish = fmap diminish

initial :: Reactive a -> a
initial r = r `atTime` minB (occs r)
    where
        -- If there are no updates, just use value at time 0
        -- Otherwise pick an arbitrary time /before/ the first value
        -- It looks strange but it works
        minB []    = 0
        minB (x:_) = x - 1

updates :: Reactive a -> [(Time, a)]
updates r = (\t -> (t, r `atTime` t)) <$> (Data.List.sort . Data.List.nub) (occs r)

renderR :: Reactive a -> (a, [(Time, a)])
renderR x = (initial x, updates x)

occs :: Reactive a -> [Time]
occs = fst . (^. _Wrapped')

splitReactive :: Reactive a -> Either a ((a, Time), [Note a], (Time, a))
splitReactive r = case updates r of
    []          -> Left  (initial r)
    (t,x):[]    -> Right ((initial r, t), [], (t, x))
    (t,x):xs    -> Right ((initial r, t), fmap mkNote $ mrights (res $ (t,x):xs), head $ mlefts (res $ (t,x):xs))

    where

        mkNote (t,u,x) = (t <-> u, x)^.note

        -- Always returns a 0 or more Right followed by one left
        res :: [(Time, a)] -> [Either (Time, a) (Time, Time, a)]
        res rs = let (ts,xs) = unzip rs in
            flip fmap (withNext ts `zip` xs) $
                \ ((t, mu), x) -> case mu of
                    Nothing -> Left (t, x)
                    Just u  -> Right (t, u, x)

        -- lenght xs == length (withNext xs)
        withNext :: [a] -> [(a, Maybe a)]
        withNext = go
            where
                go []       = []
                go [x]      = [(x, Nothing)]
                go (x:y:rs) = (x, Just y) : withNext (y : rs)






atTime :: Reactive a -> Time -> a
atTime = (!) . snd . (^. _Wrapped')

final :: Reactive a -> a
final (renderR -> (i,[])) = i
final (renderR -> (i,xs)) = snd $ last xs

switchR :: Time -> Reactive a -> Reactive a -> Reactive a
switchR t (Reactive (tx, bx)) (Reactive (ty, by)) = Reactive $ (,)
    (filter (< t) tx <> [t] <> filter (> t) ty) (switch t bx by)

trimR :: Monoid a => Span -> Reactive a -> Reactive a
trimR (view range -> (t, u)) x = switchR t mempty (switchR u x mempty)

intermediate :: Transformable a => Reactive a -> [Note a]
intermediate (updates -> []) = []
intermediate (updates -> xs) = fmap (\((t1, x), (t2, _)) -> (t1 <-> t2, x)^.note) $ withNext $ xs
  where
    withNext xs = zip xs (tail xs)

discrete :: Reactive a -> Behavior a
discrete = continous . fmap pure

continous :: Reactive (Segment a) -> Behavior a

continousWith :: Segment (a -> b) -> Reactive a -> Behavior b
continousWith f x = continous $ liftA2 (<*>) (pure f) (fmap pure x)

sample   :: [Time] -> Behavior a -> Reactive a

(continous, sample) = error "Not implemented: (continous, sample)"

window :: [Time] -> Behavior a -> Reactive (Segment a)
windowed :: Iso (Behavior a) (Behavior b) (Reactive (Segment a)) (Reactive (Segment b))
(window, windowed) = error "Not implemented: (window, windowed)"







newtype Behavior a  = Behavior { getBehavior :: Time -> a }
  deriving (Functor, Applicative, Monad, Typeable)

instance Show (Behavior a) where
  show _ = "<<Behavior>>"

instance Distributive Behavior where
  distribute = Behavior . distribute . fmap getBehavior

instance Representable Behavior where
  type Rep Behavior = Time
  tabulate = Behavior
  index (Behavior x) = x

instance Transformable (Behavior a) where
  transform s (Behavior a) = Behavior (a `whilst` s)
    where
      f `whilst` s = f . transform (negateV s)

deriving instance Semigroup a => Semigroup (Behavior a)
deriving instance Monoid a => Monoid (Behavior a)
deriving instance Num a => Num (Behavior a)
deriving instance Fractional a => Fractional (Behavior a)
deriving instance Floating a => Floating (Behavior a)
deriving instance AdditiveGroup a => AdditiveGroup (Behavior a)

instance Real a => Real (Behavior a) where
  toRational = toRational . (! 0)

instance IsString a => IsString (Behavior a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Behavior a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Behavior a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Behavior a) where
  fromDynamics = pure . fromDynamics

instance Alterable a => Alterable (Behavior a) where
  sharpen = fmap sharpen
  flatten = fmap flatten

instance Augmentable a => Augmentable (Behavior a) where
  augment = fmap augment
  diminish = fmap diminish

instance Eq a => Eq (Behavior a) where
  (==) = error "No overloading for behavior: (<=)"

instance Ord a => Ord (Behavior a) where
  (<=) = error "No overloading for behavior: (<=)"
  (>=) = error "No overloading for behavior: (<=)"
  (<)  = error "No overloading for behavior: (<=)"
  (>)  = error "No overloading for behavior: (<=)"
  max  = liftA2 max
  min  = liftA2 min

instance Enum a => Enum (Behavior a) where
  toEnum = pure . toEnum
  fromEnum = fromEnum . (! 0)

instance VectorSpace a => VectorSpace (Behavior a) where
  type Scalar (Behavior a) = Behavior (Scalar a)
  (*^) = liftA2 (*^)

instance AffineSpace a => AffineSpace (Behavior a) where
  type Diff (Behavior a) = Behavior (Diff a)
  (.-.) = liftA2 (.-.)
  (.+^) = liftA2 (.+^)

behavior :: Iso (Time -> a) (Time -> b) (Behavior a) (Behavior b)
behavior = R.tabulated

unbehavior :: Iso (Behavior a) (Behavior b) (Time -> a) (Time -> b)
unbehavior = from behavior

line :: Fractional a => Behavior a
line = realToFrac ^. R.tabulated

unit :: Fractional a => Behavior a
unit = switch 0 0 (switch 1 line 1)

interval :: (Fractional a, Transformable a) => Time -> Time -> Note (Behavior a)
interval t u = (t <-> u, line) ^. note

sine :: Floating a => Behavior a
sine = sin (line*tau)

cosine :: Floating a => Behavior a
cosine = cos (line*tau)

impulse :: Num a => Behavior a
impulse = switch' 0 0 1 0

turnOn  = switch 0 0 1

turnOff = switch 0 1 0

switch :: Time -> Behavior a -> Behavior a -> Behavior a
switch t rx ry = switch' t rx ry ry

trimBefore :: Monoid a => Time -> Behavior a -> Behavior a
trimBefore start = switch start mempty

trimAfter :: Monoid a => Time -> Behavior a -> Behavior a
trimAfter stop x = switch stop x mempty

switch' :: Time -> Behavior a -> Behavior a -> Behavior a -> Behavior a
switch' t rx ry rz = tabulate $ \u -> case u `compare` t of
  LT -> rx ! u
  EQ -> ry ! u
  GT -> rz ! u

tau :: Floating a => a
tau = 2 * pi

newtype Bound a = Bound { getBound :: (Span, a) }
  deriving (Functor, Semigroup, Typeable, Eq, Show)

instance Wrapped (Bound a) where
  type Unwrapped (Bound a) = (Span, a)
  _Wrapped' = iso getBound Bound

instance Rewrapped (Bound a) (Bound b)

instance (HasPosition a, Splittable a) => Splittable (Bound a) where
  -- TODO

instance Transformable a => Transformable (Bound a) where
  transform t = over _Wrapped (transform t `bimap` transform t)

instance (HasPosition a, HasDuration a) => HasDuration (Bound a) where
  _duration x = _offset x .-. _onset x

instance HasPosition a => HasPosition (Bound a) where
  -- TODO lawless
  -- _position (Bound (view range -> (t, u), x)) d = truncating t u (_position x d)
  _position (Bound (view range -> (t, u), x)) = alerp t u

truncating :: Ord a => a -> a -> a -> a
truncating t u x = (x `max` t) `min` u

bounds :: Time -> Time -> a -> Bound a
bounds t u x = Bound (t <-> u, x)

bounding :: Span -> a -> Bound a
bounding (view range -> (t, u)) = bounds t u











reactiveToVoice' :: Span -> Reactive a -> Voice a
reactiveToVoice' (view range -> (u,v)) r = (^. voice) $ fmap (^. stretched) $ durs `zip` (fmap (r `atTime`) times)
    where
        times = 0 : filter (\t -> u < t && t < v) (occs r)
        durs  = toRelativeTimeN' v times


        scoreToVoice :: Transformable a => Score a -> Voice (Maybe a)
        scoreToVoice = (^. voice) . fmap (^. stretched) . fmap throwTime . addRests . (^. events)
            where
               throwTime (t,d,x) = (d,x)
               addRests = concat . snd . mapAccumL g 0
                   where
                       g u (t, d, x)
                           | u == t    = (t .+^ d, [(t, d, Just x)])
                           | u <  t    = (t .+^ d, [(u, t .-. u, Nothing), (t, d, Just x)])
                           | otherwise = error "scoreToVoice: Strange prevTime"


        voiceToScore :: Voice a -> Score a
        voiceToScore = scat . fmap g . (^. stretcheds) where g = (^. stretchee) . fmap return


        {-
        voicesToScore :: HasPart a => [(Part a, Voice a)] -> Score a
        voicesToScore = pcat . fmap (voiceToScore . uncurry (\n -> fmap (setPart n)))
        -}

        voiceToScore' :: Voice (Maybe a) -> Score a
        voiceToScore' = mcatMaybes . voiceToScore







type AttributeClass a = (Typeable a, Monoid a, Semigroup a)

type TAttributeClass a = (Transformable a, AttributeClass a)

data Attribute :: * where
  Attribute  :: AttributeClass a => a -> Attribute
  TAttribute :: TAttributeClass a  => a -> Attribute

wrapAttr :: AttributeClass a => a -> Attribute
wrapAttr = Attribute

wrapTAttr :: TAttributeClass a => a -> Attribute
wrapTAttr = TAttribute

unwrapAttr :: AttributeClass a => Attribute -> Maybe a
unwrapAttr (Attribute a)  = cast a
unwrapAttr (TAttribute a) = cast a

instance Semigroup Attribute where
  (Attribute a1) <> a2 = case unwrapAttr a2 of
    Nothing  -> error "Attribute.(<>) mismatch"
    Just a2' -> Attribute (a1 <> a2')
  (TAttribute a1) <> a2 = case unwrapAttr a2 of
    Nothing  -> error "Attribute.(<>) mismatch"
    Just a2' -> TAttribute (a1 <> a2')

instance Transformable Attribute where
  transform _ (Attribute a) = Attribute a
  transform s (TAttribute a) = TAttribute (transform s a)

instance Splittable Attribute where
  split _ x = (x,x)

newtype Meta = Meta (Map String Attribute)
  deriving (Transformable, Splittable)

instance Semigroup Meta where
  Meta s1 <> Meta s2 = Meta $ Map.unionWith (<>) s1 s2

instance Monoid Meta where
  mempty = Meta Map.empty
  mappend = (<>)

class HasMeta a where
  -- | Access the meta-data.
  meta :: Lens' a Meta

instance Show Meta where
  show _ = "{ meta }"

instance HasMeta Meta where
  meta = ($)

instance HasMeta a => HasMeta (Maybe a) where
  meta = lens viewM $ flip setM
    where
      viewM Nothing  = mempty
      viewM (Just x) = view meta x
      setM m = fmap (set meta m)

instance HasMeta a => HasMeta (b, a) where
  meta = _2 . meta

instance HasMeta a => HasMeta (Twain b a) where
  meta = _Wrapped . meta

getMeta :: HasMeta a => a -> Meta
getMeta = view meta

setMeta :: HasMeta a => Meta -> a -> a
setMeta = set meta

mapMeta :: HasMeta a => (Meta -> Meta) -> a -> a
mapMeta = over meta

applyMeta :: HasMeta a => Meta -> a -> a
applyMeta m = over meta (<> m)
{-

setMetaAttr :: (AttributeClass b, HasMeta a) => b -> a -> a
setMetaAttr a = applyMeta (wrapMeta a)

setMetaTAttr :: (TAttributeClass b, HasMeta a) => b -> a -> a
setMetaTAttr a = applyMeta (wrapTMeta a)

preserveMeta :: (HasMeta a, HasMeta b) => (a -> b) -> a -> b
preserveMeta f x = let m = view meta x in set meta m (f x)
-}

newtype AddMeta a = AddMeta { getAddMeta :: Twain Meta a }
  deriving (
    Show, Functor, Foldable, Typeable, Applicative, Monad, Comonad,
    Semigroup, Monoid, Num, Fractional, Floating, Enum, Bounded,
    Integral, Real, RealFrac,
    Eq, Ord
    )

instance Wrapped (AddMeta a) where
  type Unwrapped (AddMeta a) = Twain Meta a
  _Wrapped' = iso getAddMeta AddMeta

instance Rewrapped (AddMeta a) (AddMeta b)

instance HasMeta (AddMeta a) where
  -- twain, pair, element
  meta = _Wrapped . _Wrapped . _1

  -- imap f = over annotated $ imap f

instance Transformable a => Transformable (AddMeta a) where
  transform t = over meta (transform t) . over annotated (transform t)

instance Splittable a => Splittable (AddMeta a) where
  split t = unzipR . fmap (split t)

instance HasPosition a => HasPosition (AddMeta a) where
  _onset    = _onset . extract
  _offset   = _offset . extract
  _position = _position . extract

instance HasDuration a => HasDuration (AddMeta a) where
  _duration = _duration . extract

annotated :: Lens (AddMeta a) (AddMeta b) a b
annotated = unsafeAnnotated

unannotated :: Getter a (AddMeta a)
unannotated = from unsafeAnnotated

unsafeAnnotated :: Iso (AddMeta a) (AddMeta b) a b
unsafeAnnotated = _Wrapped . extracted








divideList :: Int -> [a] -> [[a]]
divideList n xs
    | length xs <= n = [xs]
    | otherwise = [take n xs] ++ (divideList n $ drop n xs)

splitWhile :: (a -> Bool) -> [a] -> [[a]]
splitWhile p xs = case splitWhile' p xs of
    []:xss -> xss
    xss    -> xss
    where
        splitWhile' p []     = [[]]
        splitWhile' p (x:xs) = case splitWhile' p xs of
            (xs:xss) -> if p x then []:(x:xs):xss else (x:xs):xss

breakList :: Int -> [a] -> [a] -> [a]
breakList n z = mconcat . Data.List.intersperse z . divideList n

mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed f as = map (uncurry f) (zip is as)
    where
        n  = length as - 1
        is = [0..n]

dup :: a -> (a,a)
dup x = (x,x)

unf :: (a -> Maybe a) -> a -> [a]
unf f = Data.List.unfoldr (fmap dup . f)

mapF f = mapFTL f id id

mapT f = mapFTL id f id

mapL f = mapFTL id id f

mapFTL :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapFTL f g h = go
    where
        go []    = []
        go [a]   = [f a]
        go [a,b] = [f a, h b]
        go xs    = [f $ head xs]          ++
                   map g (tail $ init xs) ++
                   [h $ last xs]

filterOnce :: (a -> Bool) -> [a] -> [a]
filterOnce p = Data.List.takeWhile p . Data.List.dropWhile (not . p)

rots :: [a] -> [[a]]
rots xs = init (zipWith (++) (Data.List.tails xs) (Data.List.inits xs))

rotl :: [a] -> [a]
rotl []     = []
rotl (x:xs) = xs ++ [x]

rotr :: [a] -> [a]
rotr [] = []
rotr xs = last xs : init xs

rotated :: Int -> [a] -> [a]
rotated = go
    where
        go n as
            | n >= 0 = iterate rotr as !! n
            | n <  0 = iterate rotl as !! abs n

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 = curry . curry . (. tripl)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 = (. untripl) . uncurry . uncurry

untripl :: (a,b,c) -> ((a,b),c)
untripl (a,b,c) = ((a,b),c)

tripl :: ((a,b),c) -> (a,b,c)
tripl ((a,b),c) = (a,b,c)

tripr :: (a,(b,c)) -> (a,b,c)
tripr (a,(b,c)) = (a,b,c)

partial2 :: (a -> b      -> Bool) -> a -> b      -> Maybe b
partial3 :: (a -> b -> c -> Bool) -> a -> b -> c -> Maybe c
partial2 f = curry  (fmap snd  . partial (uncurry f))
partial3 f = curry3 (fmap (view _3) . partial (uncurry3 f))

list :: r -> ([a] -> r) -> [a] -> r
list z f [] = z
list z f xs = f xs

merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f = mergeBy' $ (fmap.fmap) orderingToBool f
    where
        orderingToBool LT = True
        orderingToBool EQ = True
        orderingToBool GT = False

mergeBy' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeBy' pred xs []         = xs
mergeBy' pred [] ys         = ys
mergeBy' pred (x:xs) (y:ys) =
    case pred x y of
        True  -> x: mergeBy' pred xs (y:ys)
        False -> y: mergeBy' pred (x:xs) ys

composed :: [b -> b] -> b -> b
composed = Prelude.foldr (.) id

unRatio :: Integral a => Data.Ratio.Ratio a -> (a, a)
unRatio x = (Data.Ratio.numerator x, Data.Ratio.denominator x)

showRatio :: (Integral a, Show a) => Data.Ratio.Ratio a -> String
showRatio (realToFrac -> (unRatio -> (x, 1))) = show x
showRatio (realToFrac -> (unRatio -> (x, y))) = "(" ++ show x ++ "/" ++ show y ++ ")"

retainUpdates :: Eq a => [a] -> [Maybe a]
retainUpdates = snd . Data.List.mapAccumL g Nothing where
    g Nothing  x = (Just x, Just x)
    g (Just p) x = (Just x, if p == x then Nothing else Just x)

replic :: Integral a => a -> b -> [b]
replic n = replicate (fromIntegral n)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

withNext :: [a] -> [(a, Maybe a)]
withNext = fmap (\(p,c,n) -> (c,n)) . withPrevNext

withPrev :: [a] -> [(Maybe a, a)]
withPrev = fmap (\(p,c,n) -> (p,c)) . withPrevNext

withPrevNext :: [a] -> [(Maybe a, a, Maybe a)]
withPrevNext xs = zip3 (pure Nothing ++ fmap Just xs) xs (fmap Just (tail xs) ++ repeat Nothing)

mapWithNext :: (a -> Maybe a -> b) -> [a] -> [b]
mapWithNext f = map (uncurry f) . withNext

mapWithPrev :: (Maybe a -> a -> b) -> [a] -> [b]
mapWithPrev f = map (uncurry f) . withPrev

mapWithPrevNext :: (Maybe a -> a -> Maybe a -> b) -> [a] -> [b]
mapWithPrevNext f = map (uncurry3 f) . withPrevNext

rotate :: Int -> [a] -> [a]
rotate n xs = drop n' xs ++ take n' xs
  where
    n' = negate n `mod` length xs

toDouble :: Real a => a -> Double
toDouble = realToFrac

through :: Applicative f =>
  Lens' s a
  -> Lens s t a b
  -> Lens (f s) (f t) (f a) (f b)
through lens1 lens2 = lens getter (flip setter)
  where
    getter = fmap (view lens1)
    setter = liftA2 (set lens2)


_zipList :: Iso [a] [b] (ZipList a) (ZipList b)
_zipList = iso ZipList getZipList

single :: Prism' [a] a
single = prism' return $ \xs -> case xs of
  [x] -> Just x
  _   -> Nothing


tripped :: Iso ((a, b), c) ((a', b'), c') (a, b, c) (a', b', c')
tripped = iso tripl untripl


inspecting :: Eq a => (b -> a) -> b -> b -> Bool
inspecting f x y = f x == f y

inspectingBy :: (b -> a) -> (a -> a -> Bool) -> (b -> b -> Bool)
inspectingBy f e = getEquivalence $ contramap f $ Equivalence e













class Transformable a => Reversible a where

  -- | Reverse (retrograde) the given value.
  rev :: a -> a

instance Reversible () where
  rev = id

instance Reversible Int where
  rev = id

instance Reversible Double where
  rev = id

instance Reversible Integer where
  rev = id

instance Reversible a => Reversible [a] where
  rev = reverse . map rev

  -- rev = Seq.reverse . fmap rev

instance (Ord k, Reversible a) => Reversible (Map k a) where
  rev = Map.map rev

instance Reversible Duration where
  rev = stretch (-1)

instance Reversible Span where
  rev = revDefault

instance Reversible a => Reversible (b, a) where
  rev (s,a) = (s, rev a)

revDefault :: (HasPosition a, Transformable a) => a -> a
revDefault x = stretch (-1) x

newtype NoReverse a = NoReverse { getNoReverse :: a }
  deriving (Typeable, Eq, Ord, Show)

instance Transformable (NoReverse a) where
  transform _ = id

instance Reversible (NoReverse a) where
  rev = id

reversed :: Reversible a => Iso' a a
reversed = iso rev rev









class Splittable a where
  split      :: Duration -> a -> (a, a)
  beginning  :: Duration -> a -> a
  ending     :: Duration -> a -> a
  split   d x = (beginning d x, ending d x)
  beginning d = fst . split d
  ending    d = snd . split d

instance Splittable () where
  split _ x = (x, x)

instance Splittable Duration where
  -- Directly from the laws
  -- Guard against t < 0
  split t x = (t' `min` x, x ^-^ (t' `min` x))
    where t' = t `max` 0

instance Splittable Span where
  -- Splitting a span splits the duration
  split pos (view delta -> (t, d)) = (t >-> d1, (t .+^ d1) >-> d2)
    where (d1, d2) = split pos d

instance (Ord k, Splittable a) => Splittable (Map k a) where
  split d = unzipR . Map.map (split d)

chunks :: (Splittable a, HasDuration a) => Duration -> a -> [a]
chunks d xs = if _duration xs <= 0 then [] else chunks' d xs
  where
    chunks' d (split d -> (x, xs)) = [x] ++ chunks d xs

splitAbs :: (HasPosition a, Splittable a) => Time -> a -> (a, a)
splitAbs t x = split (t .-. _onset x) x











extracted :: (Applicative m, Comonad m) => Iso (m a) (m b) a b
extracted = iso extract pure

extractedRep :: (Representable m, w ~ Rep m, Monoid w) => Iso (m a) (m b) a b
extractedRep = iso extractRep pureRep









newtype Graces f a = Graces { getGraces :: (Nominal f a, f a, Nominal f a) }
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Alternative f => Alternative (Nominal f) where
  empty = Nominal empty
  Nominal a <|> Nominal b = Nominal (a <|> b)

instance (Applicative f, Alternative f) => Applicative (Graces f) where
  pure x = Graces (empty, pure x, empty)

newtype Nominal f a = Nominal { getNominal :: f a }
  deriving (
  	Eq,
  	Ord,
  	Read,
  	Show,
  	Functor,
  	Foldable,
  	Traversable,
  	Monad
  )

instance Applicative f => Applicative (Nominal f) where
  pure = Nominal . pure
  Nominal f <*> Nominal x = Nominal (f <*> x)

instance Transformable (Nominal f a) where
  transform _ = id








dependingOn :: Lens' s (x,a) -> (x -> Lens' a c) -> Lens' s c
dependingOn l f = lens getter setter
  where
    getter s = let
      (x,a) = view l s
      l2    = f x
      in view l2 a
    setter s b = let
      (x,_) = view l s
      l2    = f x
      in set (l._2.l2) b s

