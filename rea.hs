
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    DeriveDataTypeable,
    StandaloneDeriving,

    ViewPatterns,
    TypeFamilies,

    -- For Newtype
    MultiParamTypeClasses,
    FlexibleInstances #-}

import Data.Ord
import qualified Data.List as List
import Control.Newtype
import Control.Monad
import Control.Arrow
import Control.Applicative
import Data.Semigroup
import Music.Time
import Music.Score.Note         
import Data.Set (Set)
import qualified Data.Set as Set
import Music.Score.Track
import Data.TotalMap

{-
    Semantics:
        R a = ([Time], Time -> a)
        Informally, R is a time-varying value in which we know all the updates.
-}


-- Function impl

newtype Reactive a = Reactive { getReactive :: ([Time], Time -> a) }
    deriving (Functor, Semigroup, Monoid)
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


-- TMap impl

-- newtype Reactive a = Reactive { getReactive :: ([Time], TMap Time a) }
--     deriving (Functor, Monoid)
-- instance Monoid a => Semigroup (Reactive a) where
--     (<>) = mappend
-- instance Newtype (Reactive a) ([Time], TMap Time a) where
--     pack = Reactive
--     unpack = getReactive
-- instance Applicative Reactive where
--     pure    = pack . pure . pure
--     (unpack -> (tf, rf)) <*> (unpack -> (tx, rx)) = pack (tf <> tx, rf <*> rx)
-- 
-- occs :: Reactive a -> [Time]
-- occs = fst . unpack
-- 
-- (?) :: Reactive a -> Time -> a
-- (?) = (!) . snd . unpack
-- 
-- switch :: Time -> Reactive a -> Reactive a -> Reactive a
-- switch t x@(Reactive (tx, rx)) (Reactive (ty, ry)) = Reactive (
--     ks,
--     tabulate (initial x) (Set.fromList ks) $ \u -> if u < t then rx ! u else ry ! u
--     )
--     where
--         ks = filter (< t) tx <> [t] <> filter (> t) ty


-- API

after :: Time -> a -> Reactive a -> Reactive a
after t x r = switch t r (pure x) 

initial :: Reactive a -> a
initial r = r ? minB (occs r)
    where
        -- If there are no updates, just use value at time 0
        -- Otherwise pick an arbitrary time /before/ the first value
        -- It looks strange but it works
        minB []    = 0
        minB (x:_) = x - 1

updates :: Reactive a -> [(Time, a)]
updates r = (\t -> (t, r ? t)) <$> occs r

renderR :: Reactive a -> (a, [(Time, a)])
renderR r = (initial r, updates r)

printR :: Show a => Reactive a -> IO ()
printR r = let (x, xs) = renderR r in do
    print x
    mapM_ print xs



