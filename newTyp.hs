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
module NewTyp (
    Duration,
    Time,
    Era,
    Span,
    Note,
    play,
    Score,
    perform,
    perform_,
    
    -- ** TODO
    Delayable(..),
) where

import Prelude hiding (span)

import Control.Monad
import Control.Monad.Writer hiding ((<>))
import Control.Newtype
import Control.Applicative
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Traversable as Traversable
import Data.MemoTrie
import Data.Pointed
import Data.Basis
import Data.VectorSpace hiding (Sum, getSum)
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.LinearMap
import Data.Semigroup

-- import Diagrams.Core.Transform

class S_ a b where
    s_ :: a -> b
instance S_ Duration Double where s_ = id
instance S_ Time     Double where s_ = unPoint
instance S_ Era (Time, Time) where s_ (Era ((Min t1),(Max t2))) = (t1, t2)


-- | Vector in time space.
type Duration = Double

-- | Point in the time space.
type Time = Point Duration
instance AdditiveGroup Time where zeroV = P 0 ; negateV = fmap negate ; (^+^) = inPoint2 (+)

-- | Two points in time space.
newtype Era = Era (Min Time, Max Time)
    deriving (Eq, Ord, Semigroup)

-- | Point and vector in time space.
newtype Span = Span { getSpan :: ({-Sum' Time-}Sum Duration, Product Duration) }
    deriving (Eq, Ord, Show, Semigroup, Monoid)
span t d = Span (Sum t, Product d)
-- TODO add special onset monoid that uses origin and (.+^)





{-
newtype Note a = Note (Span, a)
    deriving (Show, Functor, Foldable, Traversable, Monoid)
instance Monad Note where
    return x = Note (mempty, x)
    x >>= f = join' . fmap f $ x where
        join' (Note (s1,(Note (s2,x)))) = Note (s1 <> s2,x)

play :: Note t -> (Point Duration, Duration, t)
play (Note (Span (t,d), x)) = (P $ getSum t, getProduct d, x)
-}

-- | Value with associated time span.
--   
--   * 'Functor': transforms values.
--   
--   * 'Monad': 'return' uses span 'mempty' and '>>=' combines spans pointwise with 'mappend'.
--   
newtype Note a = Note { getNote :: Writer Span a }
    deriving (Functor, Foldable, Monad, Traversable)
instance Show a => Show (Note a) where
    show = show . snd . runWriter . getNote

play :: Note a -> (Time, Duration, a)
play n = let (x, Span (t,d)) = runWriter $ getNote n
    in (P $ getSum t, getProduct d, x)
    
-- | Possibly empty sequence of values with possibly overlapping time spans.
--   
--   * 'Semigroup': composes in parallel.
--   
--   * 'Monoid': empty score has no notes.
--   
--   * 'Functor': transforms values.
--   
--   * 'Monad': defined in terms of the 'Note' instance.
--   
newtype Score a = Score { getScore :: [Note a] } -- Event
    deriving (Show, Semigroup, Monoid, Functor, Foldable, Traversable)

instance MonadPlus Score where
    mzero = mempty
    mplus = mappend
instance Monad Score where
    return = Score . return . return
    x >>= f = (join' . fmap f) x
        where
            join' :: Score (Score a) -> Score a
            join' = inScore $ concatMap (fmap join . Traversable.sequence . fmap getScore)

-- TODO what sort of Monad is this?

perform :: Score a -> [(Time, Duration, a)]
perform (Score xs) = fmap play xs

perform_ :: Show a => Score a -> IO ()
perform_ = mapM_ (putStrLn.show) . perform






{-
TODO overloaded time transformations
Note: This is the general strategy.
What we really want is to join all of

    Stretchable (Score a)	 
    Delayable (Score a)	 
    HasOffset (Score a)	 
    HasOnset (Score a)	 
    HasDuration (Score a)	 
    Reversible a => Reversible (Score a)	 

into a single class or type+class pair.
-}

class Delayable a where
    delay :: Duration -> a -> a
instance Delayable Time where
    delay = flip (.+^)
instance Delayable Duration where
    delay = (^+^)
instance Delayable a => Delayable (Sum a) where
    delay t (Sum x) = Sum (delay t x)
instance Delayable Span where
    delay t = inSpan $ first (delay t)
instance Delayable (Note a) where
    delay t = inNote $ first (delay t)
instance Delayable (Score a) where
    delay t = inScore $ fmap (delay t)


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
inSpan = getSpan ~> Span
inScore = getScore ~> Score

inNote :: ((Span, a) -> (Span, b)) -> Note a -> Note b
inNote f = Note . mapWriter (swap . f . swap) . getNote


first f = swap . fmap f . swap
swap (x,y) = (y,x)
