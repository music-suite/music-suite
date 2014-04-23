
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ViewPatterns               #-}

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
      -- * Music.Time.Reactive
      Reactive,
      initial,
      final,
      intermediate,
      discrete,
      updates,
      occs,
      switchR,
      continous,
      continousWith,
      sample,
      -- TODO
      -- window,
      -- windowed,
  ) where

import           Music.Time.Split
import           Music.Time.Reverse
import           Music.Time.Bound
import           Music.Time.Behavior
import           Music.Time.Note
import           Music.Time.Segment

----
import Data.Fixed
import           Data.Default
import           Data.Ratio

import           Control.Applicative
import           Control.Arrow                (first, second, (***), (&&&))
import qualified Control.Category
import           Control.Comonad
import           Control.Comonad.Env
import           Control.Lens                 hiding (Indexable, Level, above,
                                               below, index, inside, parts,
                                               reversed, transform, (|>), (<|))
import           Control.Monad
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Distributive
import           Data.Foldable                (Foldable)
import qualified Data.Foldable                as Foldable
import           Data.Functor.Rep
import           Data.Functor.Rep.Lens
import qualified Data.List
import           Data.List.NonEmpty           (NonEmpty)
import           Data.Maybe
import           Data.NumInstances
import           Data.Semigroup               hiding ()
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.Traversable             (Traversable)
import qualified Data.Traversable             as T
import           Data.Typeable
import           Data.VectorSpace hiding (Sum(..))
import           Music.Dynamics.Literal
import           Music.Pitch.Literal

import qualified Data.Ratio                   as Util_Ratio
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import qualified Data.Ord as Ord
-----




-- |
-- Forms an applicative as per 'Behavior', but only switches at discrete points.
--
-- The semantics are given by
--
-- @
-- type Reactive a = (a, Time, Voice a)
-- @
--
newtype Reactive a = Reactive { getReactive :: ([Time], Behavior a) }
    deriving (Functor, Semigroup, Monoid)
--
-- TODO Define a more compact representation and reimplement Behavior as (Reactive Segment).
-- 
-- Possible approach:
-- 
--  * Implement PosReactive (no negative values) and define Reactive = Delayed (PosReactive)
-- 
--  * Implement liftA2 for PosReactive (preferably with a single traversal)
-- 

instance Transformable (Reactive a) where
    transform s (Reactive (t,r)) = Reactive (transform s t, transform s r)

instance Wrapped (Reactive a) where
    type Unwrapped (Reactive a) = ([Time], Behavior a)
    _Wrapped' = iso getReactive Reactive

instance Rewrapped (Reactive a) (Reactive b)
instance Applicative Reactive where
    pure  = pureDefault
    (<*>) = apDefault

(view _Wrapped -> (tf, rf)) `apDefault` (view _Wrapped -> (tx, rx)) = view _Unwrapped (tf <> tx, rf <*> rx)
pureDefault = view _Unwrapped . pure . pure

-- |
-- Get the initial value.
--
initial :: Reactive a -> a
initial r = r `atTime` minB (occs r)
    where
        -- If there are no updates, just use value at time 0
        -- Otherwise pick an arbitrary time /before/ the first value
        -- It looks strange but it works
        minB []    = 0
        minB (x:_) = x - 1

-- | Get the time of all updates and the value switched to at this point.
updates :: Reactive a -> [(Time, a)]
updates r = (\t -> (t, r `atTime` t)) <$> (Data.List.sort . Data.List.nub) (occs r)

renderR :: Reactive a -> (a, [(Time, a)])
renderR = initial &&& updates

occs :: Reactive a -> [Time]
occs = fst . (^. _Wrapped')

{-# DEPRECATED updates "" #-}
{-# DEPRECATED occs "" #-}

atTime :: Reactive a -> Time -> a
atTime = (!) . snd . (^. _Wrapped')

-- |
-- Get the final value.
--
final :: Reactive a -> a
final (renderR -> (i,[])) = i
final (renderR -> (i,xs)) = snd $ last xs

-- | @switch t a b@ behaves as @a@ before time @t@, then as @b@.
switchR :: Time -> Reactive a -> Reactive a -> Reactive a
switchR t (Reactive (tx, bx)) (Reactive (ty, by)) = Reactive $ (,)
    (filter (< t) tx <> [t] <> filter (> t) ty) (switch t bx by)

-- |
-- Get all intermediate values.
--
intermediate :: Transformable a => Reactive a -> [Note a]
intermediate (updates -> []) = []
intermediate (updates -> xs) = fmap (\((t1, x), (t2, _)) -> (t1 <-> t2, x)^.note) $ withNext $ xs
  where
    withNext xs = zip xs (tail xs)

-- |
-- Realize a 'Reactive' value as a discretely changing behavior.
--
discrete :: Reactive a -> Behavior a
discrete = continous . fmap pure

-- |
-- Realize a 'Reactive' value as an continous behavior.
--
-- See also 'concatSegment' and 'concatB'.
--
continous :: Reactive (Segment a) -> Behavior a

-- |
-- Realize a 'Reactive' value as an continous behavior.
--
-- See also 'concatSegment' and 'concatB'.
--
continousWith :: Segment (a -> b) -> Reactive a -> Behavior b
continousWith f x = continous $ liftA2 (<*>) (pure f) (fmap pure x)

-- |
-- Sample a 'Behavior' into a reactive.
--
sample   :: [Time] -> Behavior a -> Reactive a

-- TODO linear approximation
(continous, sample) = error "Not implemented: (continous, sample)"


window :: [Time] -> Behavior a -> Reactive (Segment a)
windowed :: Iso (Behavior a) (Behavior b) (Reactive (Segment a)) (Reactive (Segment b))
(window, windowed) = error "Not implemented: (window, windowed)"

{-

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

viewScore :: Monoid s => Score s a -> [(s, a)]
viewScore x = case retract x of
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
   -}


{-
import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Compose
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Foldable          (Foldable)
import qualified Data.Foldable          as F
import qualified Data.List              as List
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.String
import           Data.Traversable       (Traversable)
import qualified Data.Traversable       as T
import           Data.Typeable
import           Data.VectorSpace

import           Music.Time.Types
import           Music.Time.Transform
import           Music.Time.Duration
import           Music.Time.Position
import           Music.Time.Juxtapose
import           Music.Time.Reverse
import           Music.Time.Split

newtype Reactive a = Reactive { getReactive :: ([Time], Time -> a) }
    deriving (Functor, Semigroup, Monoid)

instance Delayable (Reactive a) where
    delay n (Reactive (t,r)) = Reactive (delay n t, delay n r)

instance Stretchable (Reactive a) where
    stretch n (Reactive (t,r)) = Reactive (stretch n t, stretch n r)

instance Wrapped (Reactive a) where
    type Unwrapped (Reactive a) = ([Time], Time -> a)
    _Wrapped' = iso getReactive Reactive

instance Applicative Reactive where
    pure    = (^. _Unwrapped') . pure . pure
    ((^. _Wrapped') -> (tf, rf)) <*> ((^. _Wrapped') -> (tx, rx)) = (^. _Unwrapped') (tf <> tx, rf <*> rx)

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
occs = fst . (^. _Wrapped')

-- | @b ?? t@ returns the value of the reactive at time @t@.
--  Semantic function.
atTime :: Reactive a -> Time -> a
atTime = snd . (^. _Wrapped')

-- | Get the initial value.
initial :: Reactive a -> a
initial r = r ? minB (occs r)
    where
        -- If there are no updates, just use value at time 0
        -- Otherwise pick an arbitrary time /before/ the first value
        -- It looks strange but it works
        minB []    = 0
        minB (x:_) = x - 1

-- | Get the time of all updates and the value switched to at this point.
updates :: Reactive a -> [Future a]
updates r = (\t -> (t, r ? t)) <$> (List.sort . List.nub) (occs r)

-- | Get the final value.
final :: Reactive a -> a
final (renderR -> (i,[])) = i
final (renderR -> (i,xs)) = snd $ last xs

-- | @switch t a b@ behaves as @a@ before time @t@, then as @b@.
switch :: Time -> Reactive a -> Reactive a -> Reactive a
switch t (Reactive (tx, rx)) (Reactive (ty, ry)) = Reactive $ (,)
    (filter (< t) tx <> [t] <> filter (> t) ty)
    (\u -> if u < t then rx u else ry u)



-- TODO separate these with newtype
type Future a = (Time, a)
type Past   a = (Time, a)

isConstant :: Reactive a -> Bool
isConstant = null . occs

isVariable :: Reactive a -> Bool
isVariable = not . isConstant

resets :: Reactive a -> [Past a]
resets = error "resets: Not impl"

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
                                     -}
