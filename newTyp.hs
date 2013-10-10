{-# LANGUAGE 
    TypeFamilies,
    TypeOperators,
    FlexibleInstances, -- For Newtypes
    MultiParamTypeClasses,
    DeriveFunctor,
    GeneralizedNewtypeDeriving 
    #-}

import Control.Newtype
import Data.MemoTrie
import Data.Pointed
import Data.Basis
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.LinearMap
import Data.Semigroup hiding (Sum)

import Diagrams.Core.Transform

newtype Duration = D Double deriving (Eq, Ord, Num, Show, Fractional)
instance AdditiveGroup Duration where zeroV = 0 ; negateV = negate ; (^+^) = (+)
instance VectorSpace Duration where type Scalar Duration = Duration ; (*^) = (*)
instance AffineSpace Duration where type Diff Duration = Duration ; (.-.) = (-) ; (.+^) = (+)
instance HasTrie Duration
instance HasBasis Duration where type Basis Duration = Duration
instance InnerSpace Duration where
    D a <.> D b = D (a <.> b)

type Time = Point Duration


newtype Era = Era (Time, Time) -- (s,t) such that s < t
    deriving (Eq, Ord)
instance AffineSpace Era where 
    type Diff Era   = (Duration, Duration)
    Era (p,q) .-. Era (p',q') = (p .-. p', q .-. q')
    Era (p,q) .+^ (u,v)       = Era (p .+^ u, q .+^ v)


-- Should be (Sum Time), but this requires Num (Time)
newtype Span = Span (Sum Duration, Product Duration)
    deriving (Eq, Ord, Show, Monoid)


newtype Note a  = Note (Span, a)
    deriving (Show, Functor, Monoid)
instance Pointed Note where
    point x = Note (mempty, x)
-- TODO Note is also a monad:
-- Note (t1,d1) (Note (t2,d2) x) = Note (t1<>t2, d1<>d2) x

    
newtype Score a = Score [Note a] -- Event
    deriving (Show, Functor)
instance Newtype (Score a) [Note a] where pack = Score ; unpack (Score x) = x
instance Monad Score where
    return = pack . return . point
    x >>= f = (join' . fmap f) x
        where
            join' :: Score (Score a) -> Score a
            join' = undefined
            joinNote :: Note (Score a) -> Score a
            joinNote (Note (s,x)) = x -- TODO apply span












newtype Interval = I Double deriving (Eq, Ord, Num, Show, Fractional)
instance AdditiveGroup Interval where zeroV = 0 ; negateV = negate ; (^+^) = (+)
instance VectorSpace Interval where type Scalar Interval = Interval ; (*^) = (*)
instance AffineSpace Interval where type Diff Interval = Interval ; (.-.) = (-) ; (.+^) = (+)
instance HasTrie Interval
instance HasBasis Interval where type Basis Interval = Interval
instance InnerSpace Interval where
    I a <.> I b = I (a <.> b)

type Pitch = Point Interval


















w, h, q, z :: Duration
[w, h, q, z] = [1,1/2,1/4,0]

m2, _M2, m3, _M3, _P4, a4, _P5, m6, _M6, m7, _M7 :: Interval
m2 : _M2 : m3 : _M3 : _P4 : a4 : _P5 : m6 : _M6 : m7 : _M7 : _ = [ (I x) | x <- [0..] ]

c, d, e, f, g, a, b :: Pitch
c : cs : d : ds : e : f : fs : g : gs : a : as : b : _ = [ P (I x) | x <- [0..] ]


type Stretch   = Duration :-* Duration
type Delay     = Time :-* Time
type Transpose = Pitch :-* Pitch
-- type Transf    = Note :-* Note

-- stretched :: Duration -> Delay
-- stretched = undefined

-- stretch :: Duration -> Time -> Time
-- stretch d = lapply (stretched d)



-- lerp, alerp
