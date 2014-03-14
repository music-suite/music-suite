
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TimeTypes where
import Data.Typeable
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Semigroup
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Control.Monad.Plus
import Control.Lens()

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import Data.Int

import qualified Data.Ratio as Util_Ratio


-- Misc instances

instance Monoid b => Monad ((,) b) where
  return = pure
  (>>=) = flip (=<<)
    where 
      (=<<) = (join .) . fmap
      join (b, (b', a)) = (b `mappend` b', a)

-- These are already in 'lens'
-- deriving instance Foldable ((,) o)
-- deriving instance Traversable ((,) o)










-- Types etc

{-
    LAWS Semigroup
        a <> (b <> c)    = (a <> b) <> c
        a      <> mempty = a
        mempty <> a      = a

    LAWS AdditiveGroup
        a ^+^ (b ^+^ c)    = (a ^+^ b) ^+^ c
        a      ^+^ zeroV   = a
        zeroV  ^+^ a       = a
        a      ^+^ negateV a = zeroV
        negateV a ^+^ a      = zeroV
        a ^+^ b              = b ^+^ a
-}

newtype Duration = Duration { getDuration :: Rational }
instance Show Duration where
    show = showRatio . getDuration
deriving instance Typeable Duration
deriving instance Eq Duration
deriving instance Ord Duration
deriving instance Num Duration
deriving instance Enum Duration
deriving instance Fractional Duration
deriving instance Real Duration
deriving instance RealFrac Duration
deriving instance AdditiveGroup Duration
instance VectorSpace Duration where
    type Scalar Duration = Duration
    (*^) = (*)
instance Semigroup Duration where
    (<>) = (*^)
instance Monoid Duration where
    mempty  = 1 -- TODO use some notion of norm
    mappend = (*^)

newtype Time = Time { getTime :: Rational }
instance Show Time where
    show = showRatio . getTime
deriving instance Typeable Time
deriving instance Eq Time
deriving instance Ord Time
deriving instance Num Time
deriving instance Enum Time
deriving instance Fractional Time
deriving instance Real Time
deriving instance RealFrac Time
deriving instance AdditiveGroup Time
instance AffineSpace Time where
    type Diff Time = Duration
    Time x .-. Time y     = Duration (x - y)
    Time x .+^ Duration y = Time     (x + y)
instance Semigroup Time where
    (<>) = (^+^)
instance Monoid Time where
    mempty  = zeroV
    mappend = (^+^)
    mconcat = sumV


newtype Span = Span (Time, Duration)
    deriving (Eq, Ord, Show)

(<->) :: Time -> Time -> Span
(>->) :: Time -> Duration -> Span
t <-> u = t >-> (u .-. t)
t >-> d = Span (t, d)















-- newtype Time -- Semigroup, Monoid (sum)
-- newtype Span -- Semigroup, Monoid, AdditiveGroup (composition)
--
-- spans :: Iso (Time^2) (Time, Dur)
-- spans = iso (\(t,d) -> (t,t.+^d)) (\(t,u) -> (t,u.-.t))
--
--
--
--
newtype Note a      = Note      (Span, a)
newtype Delayed a   = Delayed   (Time, a)
newtype Stretched a = Stretched (Duration, a)

newtype Segment a = Segment (Duration -> a)
-- Defined 0-1

newtype Behavior a  = Behavior (Time -> a)
-- Defined throughout, "focused" on 0-1

-- newinstance Functor Behavior
-- -- Distributive?
-- -- XXX potentially slow, improve by memoization/const optimization
-- instance Representable Behavior where
--     type Rep = Time
-- instance Applicative Behavior
-- instance Monad Behavior
-- instance Monoid a => Monoid (Behavior a)
--
-- newtype Track a = [(Time, a)]
--     -- XXX Start time, laziness
--     -- Functor, Monad
-- newtype Score a = [(Span, a)]
-- -- XXX Start time, laziness
--     -- Functor, Monad
--
--
-- newtype Reactive a = (Time -> (a, Duration^2))
-- -- XXX Start time, laziness
-- -- Distributive?
-- instance Representable Reactive where
--     type Rep = Time
-- instance Applicative Reactive
-- instance Monad Reactive
-- instance Monoid a => Monoid (Reactive a)


-- Fre monad of ?
{-
data Score s a
    = SOne a
    | SPlus s [Score a]
-}
newtype Trans s a = Trans (s, [a]) deriving (Functor)
instance Monoid s => Monad (Trans s) where
    return = Trans . return . return
    -- TODO the usual >>=

type Score s a = Free (Trans s) a

renderScore :: Monoid s => Score s a -> [(s, a)]
renderScore x = case retract x of
    Trans (s,as) -> zip (repeat s) as


-- Free monad of (a,a)
{-
data Tree a
    = One a
    | Plus (Tree a) (Tree a)
-}
data Pair a = Pair a a deriving (Functor)
newtype MaybePair a = MaybePair (Maybe (Pair a)) deriving (Functor) -- Use compose
type Tree a = Free MaybePair a

-- CPS-version of Tree
newtype Search a = Search { getSearch :: forall r . (a -> Tree r) -> Tree r }









class HasDuration a where
    duration :: a -> Duration
class HasPosition a where
    position :: a -> {-Scalar-} Duration -> Time

onset :: (HasPosition a{-, Fractional s, s ~ (Scalar (Duration))-}) => a -> Time
offset :: (HasPosition a{-, Fractional s, s ~ (Scalar (Duration))-}) => a -> Time
preOnset    = (`position` (-0.5))
onset       = (`position` 0)
postOnset   = (`position` 0.5)
offset      = (`position` 1.0)
postOffset  = (`position` 1.5)

class Split a where
    split :: Time -> a -> (a, a)
class Reverse a where
    rev :: a -> a
class Transformable a where
    sapp :: Span -> a -> a

delaying x   = (0 .+^ x) >-> 1
stretching x = 0         >-> x
delay   = sapp . delaying
stretch = sapp . stretching

stretchTo :: (Transformable a, HasDuration a) => Duration -> a -> a
stretchTo t x = (t / duration x) `stretch` x

startAt :: (Transformable a, HasPosition a) => Time -> a -> a
stopAt  :: (Transformable a, HasPosition a) => Time -> a -> a
startAt t x   = (t .-. onset x) `delay` x
stopAt t  x   = (t .-. offset x) `delay` x

-- > alignAt 0 == startAt
-- > alignAt 1 == stopAt
alignAt :: (Transformable a, HasPosition a) => Duration -> Time -> a -> a
alignAt p t x   = (t .-. x `position` p) `delay` x


-- a `lead`   b  moves a so that (offset a' == onset b)
-- a `follow` b  moves b so that (offset a  == onset b')
-- lead   :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
-- follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
-- a `lead` b   = stopAt (onset b) a
-- a `follow` b = startAt (offset a) b




-- -- Monoid/Semigroup
--
--
-- -- Has... Pitch Dynamics Articulation Part Chord?? Clef Slide Tremolo Text Harmonic Meta
-- -- Has+Is ... Midi/MusicXml
-- -- Is ... Pitch Interval Dynamic
--
--
--
-- reverse
-- split
--     take
--     drop
-- duration
-- position
--     onset
--     offset
-- transform
--     delay
--     stretch
-- scat
-- pcat
--
-- -- a `lead`   b  moves a so that (offset a' == onset b)
-- -- a `follow` b  moves b so that (offset a  == onset b')
-- lead   :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
-- follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
--











-- Utility

showRatio :: (Integral a, Show a) => Util_Ratio.Ratio a -> String
showRatio (realToFrac -> (unRatio -> (x, 1))) = show x
showRatio (realToFrac -> (unRatio -> (x, y))) = "(" ++ show x ++ "/" ++ show y ++ ")"

unRatio :: Integral a => Util_Ratio.Ratio a -> (a, a)
unRatio x = (Util_Ratio.numerator x, Util_Ratio.denominator x)

-- Same as @flip const@, useful to fix the type of the first argument.
assuming :: a -> b -> b
assuming = flip const

sameType :: a -> a -> ()
sameType = undefined

instance Reverse () where
    rev () = ()
instance Reverse [a] where
    rev = reverse


-- sc_semigroup :: (Semigroup a, Typeable a, Eq a, Serial IO a) => a -> TestTree
-- sc_semigroup x = testGroup ("Semigroup " ++ show (typeOf x)) [
    -- testProperty "mempty <> a == a" $ \a -> mempty <> a == (a :: a)
    -- ]
    
newtype BadMonoid a = BadMonoid [a]
    deriving (Eq, Ord, Show, Typeable)
instance Monoid (BadMonoid a) where
    BadMonoid x `mappend` BadMonoid y = BadMonoid (y `mappend` reverse x) -- lawless
    mempty = BadMonoid []
instance Functor BadMonoid where
    fmap f (BadMonoid xs) = BadMonoid (fmap f $ reverse $ xs) -- lawless

data BadFunctor a = BF1 | BF2
    deriving (Eq, Ord, Show, Typeable)

instance Functor BadFunctor where
    fmap f BF1 = BF2 -- lawless
    fmap f BF2 = BF2

instance Serial IO a => Serial IO (BadFunctor a) where
    series = cons0 BF1 \/ cons0 BF2
instance Serial IO a => Serial IO (BadMonoid a) where
    series = newtypeCons BadMonoid
instance Serial IO Int8 where
    series = msum $ fmap return [0..2]
instance Serial IO Time where
    series = msum $ fmap return [0..10]
instance Serial IO Duration where
    series = msum $ fmap return [0..10]

monoid :: (Monoid t, Eq t, Show t, Typeable t, Serial IO t) => t -> TestTree
monoid typ = testGroup ("instance Monoid " ++ show (typeOf typ)) $ [
    testProperty "x <> (y <> z) == (x <> y) <> z" $ \x y z -> assuming (sameType typ x) 
                  x <> (y <> z) == (x <> y) <> z,

    testProperty "mempty <> x == x"               $ \x     -> assuming (sameType typ x) 
                  mempty <> x == x,

    testProperty "x <> mempty == x"               $ \x     -> assuming (sameType typ x) 
                 (x <> mempty == x)
    ]
    where
        (<>) = mappend

functor :: (Functor f, Eq (f b), Show (f b), Typeable b, Typeable1 f, Serial IO (f b)) => f b -> TestTree
functor typ = testGroup ("instance Functor " ++ show (typeOf typ)) $ [
    testProperty "fmap id = id" $ \x -> assuming (sameType typ x) 
                 (fmap id x == id x)
    ]

-- applicative :: (Applicative f, Eq (f b), Show (f b), Typeable b, Typeable1 f, Serial IO (f b)) => f b -> TestTree
-- applicative typ = testGroup ("instance Applicative " ++ show (typeOf typ)) $ [
-- 
--     testProperty "pure id <*> v = v" $ \x -> assuming (sameType typ x) ((pure id <*> x) == x),
-- 
--     testProperty "pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" 
--         $ \u v w -> assuming (sameType typ w) 
--             ((pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w)))
-- 
--     ]


ap2 u v w = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

main = defaultMain $ testGroup "" $ [
    testProperty "rev . rev == id" $ \(x :: ())    -> rev (rev x) == x,
    testProperty "rev . rev == id" $ \(x :: [Int]) -> rev (rev x) == x,

    monoid (undefined :: ()),
    monoid (undefined :: Maybe ()),
    monoid (undefined :: [()]),
    monoid (undefined :: BadMonoid Int8),
    
    monoid (undefined :: Time),
    monoid (undefined :: Duration),
    
    functor (undefined :: BadFunctor Int8),
    functor (undefined :: BadMonoid Int8)

    ]





