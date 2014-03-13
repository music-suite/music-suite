
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}

module TimeTypes where
import Data.VectorSpace
import Data.AffineSpace
import Data.Semigroup
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Control.Applicative
import Control.Monad
import Control.Monad.Plus
import Control.Lens()
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

newtype Duration = Duration { getDuration :: Rational }
instance Show Duration where
    show = showRatio . getDuration
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
    mempty  = 1
    mappend = (^+^)
    mconcat = sumV


newtype Span = Span (Time, Duration)
    deriving (Eq, Ord, Show)

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
--
--
--
--
--
--
--
--
--
--
--
--
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

(<->) :: Time -> Time -> Span
(>->) :: Time -> Duration -> Span
(<->) = undefined
(>->) = undefined
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
alignAt j t x   = (t .-. x `position` j) `delay` x


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
--
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
