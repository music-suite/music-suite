{-# LANGUAGE 
    TypeFamilies,
    TypeOperators,
    FlexibleInstances, -- For Newtypes
    MultiParamTypeClasses,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    RankNTypes,
    StandaloneDeriving,
    GeneralizedNewtypeDeriving 
    #-}
import Prelude hiding (sequence)

import Control.Monad hiding (sequence)
import Control.Newtype
import Control.Applicative
import Data.Foldable (Foldable)
import Data.Traversable
import Data.MemoTrie
import Data.Pointed
import Data.Basis
import Data.VectorSpace hiding (Sum, getSum)
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.LinearMap
import Data.Semigroup

import Diagrams.Core.Transform



-- newtype Sum' a = Sum' { getSum' :: a }
--   deriving (Eq, Ord, Read, Show, Bounded)
-- instance Functor Sum' where
--   fmap f (Sum' a) = Sum' (f a)
-- instance Applicative Sum' where
-- instance AdditiveGroup a => Semigroup (Sum' a) where
-- instance AdditiveGroup a => Monoid (Sum' a) where



class S_ a b where
    s_ :: a -> b


-- newtype Duration = D Double deriving (Eq, Ord, Num, Show, Fractional)
-- instance AdditiveGroup Duration where zeroV = 0 ; negateV = negate ; (^+^) = (+)
-- instance VectorSpace Duration where type Scalar Duration = Duration ; (*^) = (*)
-- instance AffineSpace Duration where type Diff Duration = Duration ; (.-.) = (-) ; (.+^) = (+)
-- instance HasTrie Duration
-- instance HasBasis Duration where type Basis Duration = Duration
-- instance InnerSpace Duration where
--     D a <.> D b = D (a <.> b)
type Duration = Double
type Time = Point Duration
instance AdditiveGroup Time where zeroV = P 0 ; negateV = fmap negate ; {-(^+^) = liftA2 (+)-}

newtype Era = Era (Min Time, Max Time)
    deriving (Eq, Ord, Semigroup) -- FIXME Monoid requires Bounded

newtype Span = Span ({-Sum' Time-}Sum Duration, Product Duration)
    deriving (Eq, Ord, Show, Semigroup, Monoid)

instance S_ Duration Double where s_ = id
instance S_ Time     Double where s_ = unPoint
instance S_ Era (Time, Time) where s_ (Era ((Min t1),(Max t2))) = (t1, t2)



-- TODO why?
deriving instance Foldable ((,) Span)
deriving instance Traversable ((,) Span)

newtype Note a  = Note (Span, a)
    deriving (Show, Functor, Foldable, Traversable, Monoid)
instance AdditiveGroup (Note a) where -- FIXME
instance VectorSpace (Note a) where -- FIXME
instance HasBasis (Note a) where type Basis (Note a) = Int -- FIXME
instance Monad Note where
    return x = Note (mempty, x)
    x >>= f = join' . fmap f $ x where
        join' (Note (s1,(Note (s2,x)))) = Note (s1 <> s2,x)

-- TODO Note is also a monad:
-- join (Note (t1,d1) (Note (t2,d2) x)) = Note (t1<>t2, d1<>d2) x

    
newtype Score a = Score [Note a] -- Event
    deriving (Show, Semigroup, Monoid, Functor)
unscore (Score y) = y

instance MonadPlus Score where
    mzero = mempty
    mplus = mappend

instance Monad Score where
    return = Score . return . return
    x >>= f = (join' . fmap f) x
        where
            join' :: Score (Score a) -> Score a
            join' = Score . concatMap (fmap join . sequence . fmap unscore) . unscore



-- TODO overloaded time transformations

s1 :: Score (Score Pitch)
s1 = Score [Note (Span (Sum {getSum = 10.0},Product {getProduct = 3.0}),
    Score [Note (Span (Sum {getSum = 2.0},Product {getProduct = 2.0}),P 0.0)] )]







-- newtype Interval = I Double deriving (Eq, Ord, Num, Show, Fractional)
-- instance AdditiveGroup Interval where zeroV = 0 ; negateV = negate ; (^+^) = (+)
-- instance VectorSpace Interval where type Scalar Interval = Interval ; (*^) = (*)
-- instance AffineSpace Interval where type Diff Interval = Interval ; (.-.) = (-) ; (.+^) = (+)
-- instance HasTrie Interval
-- instance HasBasis Interval where type Basis Interval = Interval
-- instance InnerSpace Interval where
--     I a <.> I b = I (a <.> b)
-- interval = I
interval = id
type Interval = Double
type Pitch = Point Interval


















w, h, q, z :: Duration
[w, h, q, z] = [1,1/2,1/4,0]

m2, _M2, m3, _M3, _P4, a4, _P5, m6, _M6, m7, _M7 :: Interval
m2 : _M2 : m3 : _M3 : _P4 : a4 : _P5 : m6 : _M6 : m7 : _M7 : _ = [ (interval x) | x <- [0..] ]

c, d, e, f, g, a, b :: Pitch
c : cs : d : ds : e : f : fs : g : gs : a : as : b : _ = [ P (interval x) | x <- [0..] ]


type Stretch   = Duration :-* Duration
type Delay     = Time :-* Time
type Transpose = Pitch :-* Pitch
type Transf    = forall a . Note a :-* Note a

stretched :: Duration -> Transf
stretched = undefined

stretch :: Duration -> Note a -> Note a
stretch d = lapply (stretched d)



-- lerp, alerp
