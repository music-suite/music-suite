
{-# LANGUAGE
    ScopedTypeVariables,
    GeneralizedNewtypeDeriving,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    DeriveDataTypeable,
    StandaloneDeriving,
    ConstraintKinds,
    GADTs,
    
    ViewPatterns,
    TypeFamilies,

    -- For Newtype
    MultiParamTypeClasses,
    FlexibleInstances 
    #-}

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
-- Provides reactive values.
--
-------------------------------------------------------------------------------------

module Music.Time.Reactive where

import Data.Dynamic
import Control.Newtype                
import Data.Maybe
import Data.Ord
import Data.Semigroup
import Data.Pointed
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Compose

import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Test.QuickCheck (Arbitrary(..), Gen(..))

import Data.Typeable
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Music.Time
import Music.Pitch.Literal
import Music.Dynamics.Literal   
import Music.Score.Note
import Music.Score.Pitch
import Music.Score.Util

newtype Reactive a = Reactive { getReactive :: ([Time], Time -> a) }
    deriving (Functor, Semigroup, Monoid)

instance Delayable a => Delayable (Reactive a) where
    delay n (Reactive (t,r)) = Reactive (delay n t, delay n r)
instance Stretchable a => Stretchable (Reactive a) where
    stretch n (Reactive (t,r)) = Reactive (stretch n t, stretch n r)

instance Newtype (Reactive a) ([Time], Time -> a) where
    pack = Reactive
    unpack = getReactive
instance Applicative Reactive where
    pure    = pack . pure . pure
    (unpack -> (tf, rf)) <*> (unpack -> (tx, rx)) = pack (tf <> tx, rf <*> rx)

occs :: Reactive a -> [Time]
occs = fst . unpack

(?) :: Reactive a -> Time -> a
(?) = ($) . snd . unpack

-- | @switch t a b@ behaves as @a@ before time @t@, then as @b@.
switch :: Time -> Reactive a -> Reactive a -> Reactive a
switch t (Reactive (tx, rx)) (Reactive (ty, ry)) = Reactive (
    filter (< t) tx <> [t] <> filter (> t) ty,
    \u -> if u < t then rx u else ry u
    )

activate :: Note (Reactive a) -> Reactive a -> Reactive a
activate (Note (range -> (start,stop),x)) y = switch start y (switch stop x y)

noteToReact :: Monoid a => Note a -> Reactive a
noteToReact n = (pure <$> n) `activate` pure mempty


initial :: Reactive a -> a
initial r = r ? minB (occs r)
    where
        -- If there are no updates, just use value at time 0
        -- Otherwise pick an arbitrary time /before/ the first value
        -- It looks strange but it works
        minB []    = 0
        minB (x:_) = x - 1

updates :: Reactive a -> [(Time, a)]
updates r = (\t -> (t, r ? t)) <$> (List.sort . List.nub) (occs r)

renderR :: Reactive a -> (a, [(Time, a)])
renderR r = (initial r, updates r)

printR :: Show a => Reactive a -> IO ()
printR r = let (x, xs) = renderR r in do
    print x
    mapM_ print xs
