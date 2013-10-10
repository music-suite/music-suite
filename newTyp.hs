{-# LANGUAGE     
    NoMonomorphismRestriction,
    TypeFamilies,
    TypeOperators,
    ConstraintKinds,
    FlexibleContexts,
    FlexibleInstances, -- For Newtypes
    MultiParamTypeClasses,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    RankNTypes,
    StandaloneDeriving,
    GeneralizedNewtypeDeriving 
    #-}
import Prelude hiding (sequence, span)

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

-- import Diagrams.Core.Transform



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
instance AdditiveGroup Time where zeroV = P 0 ; negateV = fmap negate ; (^+^) = inPoint2 (+)

newtype Era = Era (Min Time, Max Time)
    deriving (Eq, Ord, Semigroup) -- FIXME Monoid requires Bounded

-- TODO add special onset monoid that uses origin and (.+^)
newtype Span = Span ({-Sum' Time-}Sum Duration, Product Duration)
    deriving (Eq, Ord, Show, Semigroup, Monoid)
span t d = Span (Sum t, Product d)

instance S_ Duration Double where s_ = id
instance S_ Time     Double where s_ = unPoint
instance S_ Era (Time, Time) where s_ (Era ((Min t1),(Max t2))) = (t1, t2)



-- TODO why?
deriving instance Foldable ((,) Span)
deriving instance Traversable ((,) Span)

newtype Note a = Note (Span, a)
    deriving (Show, Functor, Foldable, Traversable, Monoid)
instance Monad Note where
    return x = Note (mempty, x)
    x >>= f = join' . fmap f $ x where
        join' (Note (s1,(Note (s2,x)))) = Note (s1 <> s2,x)

performNote :: Note t -> (Point Duration, Duration, t)
performNote (Note (Span (t,d), x)) = (P $ getSum t, getProduct d, x)
-- TODO is this not just the writer monad?

    
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

-- TODO what sort of Monad is this?

perform :: Score a -> [(Time, Duration, a)]
perform (Score xs) = fmap performNote xs
perform_ = mapM_ (putStrLn.show) . perform

-- TODO overloaded time transformations




-- s1 :: Score (Score Pitch)
-- s1 = Score [Note (Span (Sum {getSum = 10.0},Product {getProduct = 3.0}),
--     Score [Note (Span (Sum {getSum = 2.0},Product {getProduct = 2.0}),P 0.0)] )]
-- 






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

data Transformation v = Transformation (v :-* v)       
app (Transformation x) = lapply x
type family V a :: *

class Transformable t where
    transform :: Transformation (V t) -> t -> t

type HasLinearMap a = (HasTrie (Basis a), HasBasis a)

-- stretched :: (HasLinearMap (V t), Transformable t) => Scalar v -> Transformation v
stretched d = Transformation (linear (^* d))

stretch :: (HasLinearMap (V t), Transformable t) => Scalar (V t) -> t -> t
stretch d = transform (stretched d)



-- lerp, alerp






(~>) :: (a' -> a) -> (b -> b') -> ((a -> b) -> (a' -> b'))
(i ~> o) f = o . f . i
inPoint = unPoint ~> P
inPoint2 = unPoint ~> inPoint