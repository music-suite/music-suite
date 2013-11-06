
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

newtype R a = R { getR :: ([Time], Time -> a) }
    deriving (Functor, Semigroup, Monoid)
instance Newtype (R a) ([Time], Time -> a) where
    pack = R
    unpack = getR
instance Applicative R where
    pure    = pack . pure . pure
    (unpack -> (tf, rf)) <*> (unpack -> (tx, rx)) = pack (tf <> tx, rf <*> rx)

occs :: R a -> [Time]
occs = fst . unpack

at :: R a -> Time -> a
at = ($) . snd . unpack

switch :: Time -> R a -> R a -> R a
switch t (R (tx, rx)) (R (ty, ry)) = R (
    filter (< t) tx <> [t] <> filter (> t) ty,
    \u -> if u < t then rx u else ry u
    )


-- TMap impl

-- newtype R a = R { getR :: ([Time], TMap Time a) }
--     deriving (Functor, Monoid)
-- instance Monoid a => Semigroup (R a) where
--     (<>) = mappend
-- instance Newtype (R a) ([Time], TMap Time a) where
--     pack = R
--     unpack = getR
-- instance Applicative R where
--     pure    = pack . pure . pure
--     (unpack -> (tf, rf)) <*> (unpack -> (tx, rx)) = pack (tf <> tx, rf <*> rx)
-- 
-- occs :: R a -> [Time]
-- occs = fst . unpack
-- 
-- at :: R a -> Time -> a
-- at = (!) . snd . unpack
-- 
-- switch :: Time -> R a -> R a -> R a
-- switch t x@(R (tx, rx)) (R (ty, ry)) = R (
--     ks,
--     tabulate (defa x) (Set.fromList ks) $ \u -> if u < t then rx ! u else ry ! u
--     )
--     where
--         ks = filter (< t) tx <> [t] <> filter (> t) ty


-- API

after :: Time -> a -> R a -> R a
after t x r = switch t r (pure x) 
-- after t x (R (tx, rx)) = R (tx <> [t], \u -> if u < t then rx u else x)

until :: Span -> a -> R a -> R a
until (range -> (t,t2)) x = ((after t2 id $ after t (const x) $ pure id) <*>)


printR r = let (x, xs) = renderR r in do
    print x
    mapM_ print xs

defa :: R a -> a
defa r = r `at` minB (occs r)
    where
        minB []    = 0
        minB (x:_) = x - 1 -- strange but it works

rest :: R a -> [(Time, a)]
rest r = (\t -> (t, r `at` t)) <$> occs r

renderR :: R a -> (a, [(Time, a)])
renderR r = (defa r, rest r)


