
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
-- Reactive values.
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
        initial,
        updates,
        occs,
        (?),
        switch,
        activate,
        noteToReactive,
        during,
        since,
        until,
        -- renderR,
        -- printR,
        splitReactive,
  ) where

import Prelude hiding (until)

import Control.Lens
-- import Control.Newtype                
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Plus       
import Control.Monad.Compose
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

import Music.Time
import Music.Score.Note
import Music.Score.Track
import Music.Score.Pitch
import Music.Score.Util
import Music.Pitch.Literal
import Music.Dynamics.Literal   

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

occs :: Reactive a -> [Time]
occs = fst . (^. unwrapped)

-- | @b ?? t@ returns the value of the reactive at time @t@.
--  Semantic function.
(?) :: Reactive a -> Time -> a
(?) = snd . (^. unwrapped)

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

-- | @switch t a b@ behaves as @a@ before time @t@, then as @b@.
switch :: Time -> Reactive a -> Reactive a -> Reactive a
switch t (Reactive (tx, rx)) (Reactive (ty, ry)) = Reactive $ (,)
    (filter (< t) tx <> [t] <> filter (> t) ty)
    (\u -> if u < t then rx u else ry u)

-- TODO proper ISOs here
asOccs   = occs &&& (?)
asInitUp = initial &&& updates

-- TODO rename during
noteToReactive :: Monoid a => Note a -> Reactive a
noteToReactive n = (pure <$> n) `activate` pure mempty


activate :: Note (Reactive a) -> Reactive a -> Reactive a
activate (getNote -> (view range -> (start,stop), x)) y = y `turnOn` (x `turnOff` y)
    where
        turnOn  = switch start
        turnOff = switch stop

during :: Monoid a => Note a -> Reactive a
during = noteToReactive

since :: Monoid a => Time -> Reactive a -> Reactive a
since start x = switch start mempty x

until :: Monoid a => Time -> Reactive a -> Reactive a
until stop x = switch stop x mempty

renderR :: Reactive a -> (a, [(Time, a)])
renderR = initial &&& updates

renderR2 :: Reactive a -> ([Time], Time -> a)
renderR2 = occs &&& (?)

printR :: Show a => Reactive a -> IO ()
printR r = let (x, xs) = renderR r in do
    print x
    mapM_ print xs

-- | Split a reactive into notes, as well as the values before and after the first/last update
splitReactive :: Reactive a -> Either a ((a, Time), [Note a], (Time, a))
splitReactive r = case updates r of
    []          -> Left  (initial r)
    (t,x):[]    -> Right ((initial r, t), [], (t, x))
    (t,x):xs    -> Right ((initial r, t), fmap note $ mrights (res $ (t,x):xs), head $ mlefts (res $ (t,x):xs))

    where

        note (t,u,x) = t <-> u =: x

        -- Always returns a 0 or more Right followed by one left
        res :: [(Time, a)] -> [Either (Time, a) (Time, Time, a)]    
        res rs = let (ts,xs) = unzip rs in 
            flip fmap (withNext ts `zip` xs) $ 
                \ ((t, mu), x) -> case mu of
                    Nothing -> Left (t, x)
                    Just u  -> Right (t, u, x)

        -- lenght xs == length (withNext xs)
        withNext :: [a] -> [(a, Maybe a)]
        withNext = go
            where
                go []       = []
                go [x]      = [(x, Nothing)]
                go (x:y:rs) = (x, Just y) : withNext (y : rs)      

