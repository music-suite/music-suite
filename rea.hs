
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




after :: Time -> a -> R a -> R a
after t x r = switch t r (pure x) 
-- after t x (R (tx, rx)) = R (tx <> [t], \u -> if u < t then rx u else x)

until :: Span -> a -> R a -> R a
until (range -> (t,t2)) x = ((after t2 id $ after t (const x) $ pure id) <*>)


printR r = let (x, xs) = renderR r in do
    print x
    mapM_ print xs

renderR :: R a -> (a, [(Time, a)])
renderR r = (r `at` minB (occs r), (\t -> (t, r `at` t)) <$> occs r)
    where
        minB []    = 0
        minB (x:_) = x - 1 -- strange but it works

-- 
-- 
-- printR r = do
--     print (def r)
--     mapM_ print $ List.sortBy (comparing fst) $ getTrack $ tr r
-- 
-- 
-- def :: R a -> a
-- def = snd . getR
-- 
-- tr :: R a -> Track a
-- tr = fst . getR



-- newtype R a = R { getR :: (Set Time, Time -> a) }
--     deriving (Functor)
-- instance Newtype (R a) (Set Time, Time -> a) where
--     pack = R
--     unpack = getR
-- 
-- instance Applicative R where
--     pure = return
--     (<*>) = ap
-- instance Monad R where
--     return = pureR
--     x >>= f = (joinR . fmap f) x
-- 
-- 
-- pureR :: a -> R a
-- pureR x = pack (Set.empty, const x)
-- 
-- joinR :: R (R a) -> R a
-- joinR (unpack -> (ts, r)) = pack (ts' <> tsn, r')
--     where                                
--         tsn = fst . unpack . r $ (-10000) -- FIXME
--         ts' = if Set.null ts 
--                 then Set.empty
--                 else Set.fromList . concat . fmap (Set.toList . fst . unpack . r) $ Set.toList ts
--         r'  = join (snd . unpack . r)
-- 
-- wh :: Time -> Duration -> (a -> a) -> R a -> R a
-- wh t d f = noteR (Note (t --> d, f))
-- 
-- noteR :: Note (a -> a) -> R a -> R a
-- noteR (Note (range -> (t,u),f)) (unpack -> (ts, r)) = 
--     pack (Set.fromList [t,u] <> ts, 
--         \n -> if t <= n && n < u then f (r n) else r n
--         )
-- -- Monoid m => R m
-- -- Monoid m => Note m -> R m -> R m
-- 
-- 
-- 
-- 
-- printR r = let (x, xs) = renderR r in do
--     print x
--     mapM_ print xs
-- 
-- renderR :: R a -> (a, [(Time, a)])
-- renderR r = (r `at` minB (occs r), (\t -> (t, r `at` t)) <$> occs r)
--     where
--         minB []    = 0
--         minB (x:_) = x - 1 -- strange but it works
--     
-- -- or equivalently
-- 
-- renderR' :: R a -> ([Time], Time -> a)
-- renderR' = (Set.toList *** id) . unpack

