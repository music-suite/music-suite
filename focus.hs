
{-# LANGUAGE     
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    NoMonomorphismRestriction,
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    TypeFamilies,
    ViewPatterns,
    RankNTypes,

    MultiParamTypeClasses,
    
    OverloadedStrings,
    TypeOperators,
    FlexibleContexts,
    
    TemplateHaskell
    #-}


module Data.SpanList where

import Prelude hiding (span) -- TODO

import Control.Compose ((~>))
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Plus

import Control.Lens
import Data.Key
import Data.Maybe
import Data.Tree
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Foldable
import Data.Traversable
import qualified Data.Foldable as F
import qualified Data.Traversable as T

-- type family RelPos (a :: * -> *) :: *
-- type family Pos  (a :: * -> *) :: *
-- 
-- type instance RelPos [] = Maybe Pos -- Nothing means above
-- type instance Pos    [] = Integer
-- 
-- data TreeRelPos = Maybe TreePos -- Nothing means up
-- data TreePos    = Here | Up | Child Int TreePos
-- type instance RelPos Tree = Int
-- -- type instance Pos Tree = Int
-- 
-- 



newtype Write m a = Write (a, m)
    deriving (Show, Functor, Foldable, Traversable, Eq)
instance Monoid m => Monad (Write m) where
    return x = Write (x, mempty)
    Write (x1,m1) >>= f = let
        Write (x2,m2) = f x1
        in Write (x2,m1 `mappend` m2)

    Write (x1,m1) >> y = let
        Write (x2,m2) = y
        in Write (x2,m1 `mappend` m2)

writeFst (Write x) = fst x
writeSnd (Write x) = snd x
written f (Write (a, m)) = Write (a, f m)


{-
    The Key type family associates "indexed" types with their index.
    Instances include container-like types, functions and their composition.

    A /focused/ value a value of such a type paired up with its index. This
    is similar to a zipper, or a data structure paired with a lens into the
    structure.
-}

-- TODO move
mcompose :: (Monad m, Monad n, Functor m, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
mcompose = (join .) . fmap . (fmap join .) . T.mapM


-- | Value with a focus.
newtype Focus' k f a = Focus' { unFocus' :: Write k (f a) }
    deriving (Functor, Foldable, Traversable, Eq, Show)
inFocus' = unFocus' ~> Focus'
instance (Monoid k, k ~ Key f, Monad f, Traversable f) => Monad (Focus' k f) where
    return = Focus' . return . return
    Focus' xs >>= f = Focus' $ mcompose (unFocus' . f) $ xs

type Focus f a = Focus' (Key f) f a


-- TODO should be called focus
-- | Create a focus by picking an element.
pick :: Key f -> f a -> Focus f a
pick n xs = Focus' $ Write (xs, n)

unFocus :: Focus f a -> f a
unFocus (Focus' (Write (x,_))) = x

-- | Move the focus.
step :: (Key f -> Key f) -> Focus f a -> Focus f a
step f = inFocus' (written f)

previous, next :: Enum (Key f) => Focus f a -> Focus f a
-- | Move the focus to the previous value.
previous = step pred
-- | Move the focus to the next value.
next = step succ

past, future :: Enum (Key f) => Focus f a -> [Focus f a]
future = iterate next
past   = iterate previous

get :: Lookup f => Focus f a -> Maybe a
get x = Data.Key.lookup (getKey x) (unFocus x)

getKey :: Focus f a -> Key f
getKey (Focus' (Write (x,k))) = k

pastKeys, futureKeys :: Enum (Key f) => Focus f a -> [Key f]
futureKeys  = fmap getKey . future
pastKeys    = fmap getKey . past



