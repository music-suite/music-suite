
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE RankNTypes                 #-}

module T (
    Pitch,
    Interval,
    Time,
    Duration,

    HasPosition,
    Transformable,
    D,
    T,
    T2,
    unit,           -- D
    start,          -- T
    stop,           -- T
    unitInterv,     -- T2

    -- era,            -- (PositionableHasOrigin a, Num s, s ~ Scalar T) => a -> T2

    delaying,       -- D -> Transformation D
    stretching,     -- D -> Transformation D
    (<->),          -- T -> T -> T2
    (>->),          -- T -> D -> T2
    range,          -- Iso T2 (T,T)
    delta,          -- Iso T2 (T,D)
    image,          -- T2 -> Transformation D

    Transformation,
    ident,          -- (HasBasis v, HasTrie b, b ~ Basis v) => Transformation v
    (^.^),

) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Plus
import           Control.Monad.Writer   hiding ((<>))
import           Control.MonadPlus.Free


import           Control.Lens
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Basis
import           Data.LinearMap
import           Data.MemoTrie
import           Data.Semigroup
import           Data.VectorSpace       hiding (Sum)
import           Music.Score.Util       (showRatio)

------------------------------------------------------------------------------------------

type family Part a

type family Pitch a
type Interval a = Diff (Pitch a)

type family Amplitude a
type Volume a = Diff (Amplitude a)

type family Time a
type Duration a = Diff (Time a)

class HasPitch a where -- pitch :: Lens a (Pitch a)
class HasDynamics a where -- dynamics :: Lens a (Amplitude a)
class HasArticulation a where -- articulation :: Lens a (Articulation a)
-- Tremolo, Text, Harmonic, Slide, Tiable

class HasMeta a where -- meta :: Lens a Meta

------------------------------------------------------------------------------------------
{-
    Stretchable/Delayable           -> Transformable
    HasOnset/HasOffset/HasDuration  -> HasPosition/HasDuration
    N/A                                Segmented
    Reversible                         Reverse
-}

class Transformable a where
    type V
    transform :: Transformation (V a) -> a -> a

idT :: Transformation v
compT :: Transformation v -> Transformation v -> Transformation v
idT = undefined
compT = undefined
under, over :: Transformable a => Transformation (V a) -> (a -> a) -> a -> a
under = undefined
over = undefined
    

-- > duration a = offset a .-. onset a
-- > position x = alerp (onset x) (offset x)


-- For things that have a duration but no position (voices, events etc) 
class HasDuration a where
    duration :: a -> Duration a

class HasPosition a where
    -- Maps "local" time to "global" time
    -- Affected by time transformations
    -- TODO LAW Must be monotonic
    position :: a -> Scalar (Duration a) -> Time a

class Segmented a where
    erase :: Monoid a => (Time a -> Bool) -> a -> a
    split   :: (Monoid a, Ord (Time a)) => Time a -> a -> (a, a)
    split t x = (erase (> t) x, erase (< t) x)

class Reverse a where
    -- > rev . rev = id
    rev :: a -> a

onset :: (HasPosition a, Fractional s, s ~ (Scalar (Duration a))) => a -> Time a
offset :: (HasPosition a, Fractional s, s ~ (Scalar (Duration a))) => a -> Time a
preOnset    = (`position` (-0.5))
onset       = (`position` 0)
postOnset   = (`position` 0.5)
offset      = (`position` 1.0)
postOffset  = (`position` 1.5)

-- era :: (HasPosition a, Time a ~ T) => a -> T2
-- era x = onset x <-> offset x

-- type instance Time () = T
-- instance HasPosition () where
    -- position () = lerp 0 1

------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------






newtype D = D { unD :: Rational }
  deriving (Eq, Ord, AdditiveGroup)
instance VectorSpace D where { type Scalar D = D; (D x) *^ (D y) = D (x *^ y) }
instance HasBasis D where { type Basis D = (); basisValue () = 1; decompose s    = [((),s)]; decompose' s   = const s }
instance Num D
instance Enum D
instance Fractional D
instance Real D
instance RealFrac D
instance Show D where show = showRatio . unD

newtype T = T { unT :: Point D } deriving (Eq, Ord)
instance AffineSpace T where type Diff T = D
instance Num T
instance Enum T
instance Fractional T
instance Real T
instance RealFrac T
instance Show T where show = show . unT

type instance Time T = T
type instance Time D = D

unit :: D
unit = 1

start :: T
start = T origin

stop :: T
stop  = start .+^ unit





data Transformation v = Transformation (v :-* v) v

ident :: (HasBasis v, HasTrie b, b ~ Basis v) => Transformation v
ident = Transformation idL zeroV

(^.^) :: (HasBasis v, HasTrie b, b ~ Basis v) => Transformation v -> Transformation v -> Transformation v
Transformation d1 t1 ^.^ Transformation d2 t2 = Transformation (d1 *.* d2) (t1 ^+^ t2)

delaying :: D -> Transformation D
delaying t = Transformation (linear id) t

stretching :: D -> Transformation D
stretching d = Transformation (linear (^* d)) zeroV

delay :: D -> a -> a
delay = undefined

undelay :: D -> a -> a
undelay = undefined

stretch :: D -> a -> a
stretch = undefined

compress :: D -> a -> a
compress = undefined

moveTo :: T -> a -> a
moveTo = undefined

stretchTo :: D -> a -> a
stretchTo = undefined

fitInto :: T2 -> a -> a
fitInto = undefined

-- instance Transformable T where
    -- transform (Transformation df tf) d = lapply df ^+^ tf






newtype T2 = T2 { unT2 :: (T, D) }
    deriving (Eq, Ord)

instance Monoid T2 where
    mempty = T2 (0,1)
    T2 (t1,d1) `mappend` T2 (t2,d2) = T2 (t1+t2,d1*d2)

(<->) :: T -> T -> T2
t <-> u = t >-> (u .-. t)

(>->) :: T -> D -> T2
t >-> d = T2 (t,d)

range :: Iso T2 T2 (T,T) (T,T)
range = iso range' $ uncurry (<->) where range' x = let (t, d) = unT2 x in (t, t .+^ d)

delta :: Iso T2 T2 (T,D) (T,D)
delta = iso unT2 $ uncurry (>->)

unitInterv :: T2
unitInterv = mempty

image :: T2 -> Transformation D
image (view delta -> (t,d)) = delaying (t .-. start) ^.^ stretching d

-- era :: (HasOnset a, HasOffset a) => a -> T2
-- era x = onset x <-> offset x



type Event  a = (D, a)
type Future a = (T, a)
type Past   a = (a, T)
type Note   a = (T2, a)

type Score a  = [Note a]
type Track a  = [Future a]
type Voice a  = [Event a]
type Beh   a  = T -> a
type Seg   a  = Note (Beh a)
type Active a = Monoid a => [Seg a]

-- TODO Note, Event, Future, Past, Track, Voice, SVoice, Score
-- TODO Behavior, Reactive, Segment










