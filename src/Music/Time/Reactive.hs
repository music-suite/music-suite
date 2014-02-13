
{-# LANGUAGE 
    ScopedTypeVariables, 
    GeneralizedNewtypeDeriving,
    DeriveFunctor, 
    DeriveFoldable, 
    DeriveTraversable,
    DeriveDataTypeable, 
    ConstraintKinds, 
    GADTs, 
    ViewPatterns,
    TypeFamilies, 
    MultiParamTypeClasses, 
    FlexibleInstances #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Reactive values, or piecewise functions of time.
--
-- Similar to Conal's definition in <http://conal.net/blog/posts/reactive-normal-form>,
-- but defined in negative time as well. Its semantics function is either 'occs' @&&&@ '?'
-- /or/ 'initial' @&&&@ 'updates', where 'intial' is the value from negative infinity
-- to the first update.
--
-- TODO integrate better in the library
--
-------------------------------------------------------------------------------------

module Music.Time.Reactive (
        Reactive,
        
        -- * Create, isos etc (TODO)
        renderR,
        renderR',
        initial,
        final,
        updates,
        occs,
        -- splitReactive,
        -- noteToReactive,

        -- * Combinators
        step,
        switch,
        trim,
        trimBefore,
        trimAfter,

        -- printR,
  ) where

import Control.Lens
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Plus       
import Control.Monad.Compose
import Data.String
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Typeable
import Data.Semigroup
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Music.Time.Time
import Music.Time.Delayable
import Music.Time.Stretchable
import Music.Time.Span
import Music.Time.Time
-- import Music.Score.Note
-- import Music.Score.Track
-- import Music.Score.Pitch
-- import Music.Score.Util
-- import Music.Pitch.Literal
-- import Music.Dynamics.Literal   

newtype Reactive a = Reactive { getReactive :: ([Time], Time -> a) }
    deriving (Functor, Semigroup, Monoid)

instance Delayable (Reactive a) where
    delay n (Reactive (t,r)) = Reactive (delay n t, delay n r)

instance Stretchable (Reactive a) where
    stretch n (Reactive (t,r)) = Reactive (stretch n t, stretch n r)

-- instance Newtype (Reactive a) ([Time], Time -> a) where
    -- (^. wrapped) = Reactive
    -- un(^. wrapped) = getReactive

instance Wrapped ([Time], Time -> a) ([Time], Time -> a) (Reactive a) (Reactive a) where
    wrapped = iso Reactive getReactive

instance Applicative Reactive where
    pure    = (^. wrapped) . pure . pure
    ((^. unwrapped) -> (tf, rf)) <*> ((^. unwrapped) -> (tx, rx)) = (^. wrapped) (tf <> tx, rf <*> rx)

instance HasBehavior Reactive where
    (?) = atTime

instance IsString a => IsString (Reactive a) where
    fromString = pure . fromString

instance Eq (Reactive b) where
    (==) = error "(==)"
    (/=) = error "(/=)"

instance Ord b => Ord (Reactive b) where
    min = liftA2 min
    max = liftA2 max

instance Enum a => Enum (Reactive a) where
    succ           = fmap succ
    pred           = fmap pred
    toEnum         = pure . toEnum
    fromEnum       = error "fromEnum"
    enumFrom       = error "enumFrom"
    enumFromThen   = error "enumFromThen"
    enumFromTo     = error "enumFromTo"
    enumFromThenTo = error "enumFromThenTo"

instance Num a => Num (Reactive a) where
    (+)         = liftA2 (+)
    (*)         = liftA2 (*)
    (-)         = liftA2 (-)
    abs         = fmap abs
    signum      = fmap signum
    fromInteger = pure . fromInteger

instance (Num a, Ord a) => Real (Reactive a) where
  toRational = error "toRational"

instance Integral a => Integral (Reactive a) where
    quot      = liftA2 quot
    rem       = liftA2 rem
    div       = liftA2 div
    mod       = liftA2 mod
    quotRem   = (fmap.fmap) unzip' (liftA2 quotRem)
    divMod    = (fmap.fmap) unzip' (liftA2 divMod)
    toInteger = error "toInteger"

instance Fractional b => Fractional (Reactive b) where
    recip        = fmap recip
    fromRational = pure . fromRational

instance Floating b => Floating (Reactive b) where
    pi    = pure pi
    sqrt  = fmap sqrt
    exp   = fmap exp
    log   = fmap log
    sin   = fmap sin
    cos   = fmap cos
    asin  = fmap asin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh

instance AdditiveGroup v => AdditiveGroup (Reactive v) where
    zeroV   = pure   zeroV
    (^+^)   = liftA2 (^+^)
    negateV = liftA   negateV

instance VectorSpace v => VectorSpace (Reactive v) where
    type Scalar (Reactive v) = Scalar v
    (*^) s = fmap (s *^)


-- | Get the time of all updatese.
occs :: Reactive a -> [Time]
occs = fst . (^. unwrapped)

-- | @b ?? t@ returns the value of the reactive at time @t@.
--  Semantic function.
atTime :: Reactive a -> Time -> a
atTime = snd . (^. unwrapped)

-- | Get the initial value.
initial :: Reactive a -> a
initial r = r ? minB (occs r)
    where
        -- If there are no updates, just use value at time 0
        -- Otherwise pick an arbitrary time /before/ the first value
        -- It looks strange but it works
        minB []    = 0
        minB (x:_) = x - 1

-- | Get the final value.
final :: Reactive a -> a
final (renderR -> (i,[])) = i
final (renderR -> (i,xs)) = snd $ last xs

-- | Get the time of all updates and the value switched to at this point.
updates :: Reactive a -> [(Time, a)]
updates r = (\t -> (t, r ? t)) <$> (List.sort . List.nub) (occs r)

-- | @switch t a b@ behaves as @a@ before time @t@, then as @b@.
switch :: Time -> Reactive a -> Reactive a -> Reactive a
switch t (Reactive (tx, rx)) (Reactive (ty, ry)) = Reactive $ (,)
    (filter (< t) tx <> [t] <> filter (> t) ty)
    (\u -> if u < t then rx u else ry u)

isConstant = null . occs

-- | The unit step function, which goes from 0 to 1 at 'start'.
step :: (AdditiveGroup a, Fractional a) => Reactive a
step = switch start zeroV 1.0 -- TODO some overloaded unit

-- | Replace everthing outside the given span by `mempty`.
trim :: Monoid a => Span -> Reactive a -> Reactive a
trim (view range -> (t,u)) = trimBefore t . trimAfter u

-- | Replace everthing before the given time by `mempty`.
trimBefore :: Monoid a => Time -> Reactive a -> Reactive a
trimBefore start x = switch start mempty x

-- | Replace everthing after the given time by `mempty`.
trimAfter :: Monoid a => Time -> Reactive a -> Reactive a
trimAfter stop x = switch stop x mempty

-- | Semantic function. 
renderR :: Reactive a -> (a, [(Time, a)])
renderR = initial &&& updates

-- | Semantic function. 
renderR' :: Reactive a -> ([Time], Time -> a)
renderR' = occs &&& (?)


printR :: Show a => Reactive a -> IO ()
printR r = let (x, xs) = renderR r in do
    print x
    mapM_ print xs

unzip' :: Functor f => f (a, b) -> (f a, f b)
unzip' r = (fst <$> r, snd <$> r)
