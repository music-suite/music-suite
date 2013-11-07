
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    ViewPatterns,
    GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction #-}

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
-- 
--
-------------------------------------------------------------------------------------


module Music.Score.Meta (
        TimeSignature,
        KeySignature,
        Tempo
  ) where

import Data.Void
import Data.Maybe
import Data.Foldable
import Data.Typeable
import Data.Semigroup
import Control.Arrow
import Control.Monad.Plus       
import qualified Data.List          as List
import qualified Data.List.NonEmpty as NonEmpty

import Music.Time
import Music.Score.Note
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Combinators

type TimeSignature = ([Integer], Integer)

type KeySignature = (Integer, Bool)

type Tempo = Duration


-- Same as updates, but adding the initial value at 0 (unless there is an update at 0)
-- updatesZero :: Reactive a -> [(Time, a)]
-- updatesZero r = case updates r of
--     []         -> [(0, initial r)]
--     x@(t,_):xs -> if t > 0 then (0,initial r) : x : xs else x : xs

-- Split a reactive into notes, as well as the values before and after the first/last update
splitReactive :: Reactive a -> Either a ((a, Time), [Note a], (Time, a))
splitReactive r = case updates r of
    []          -> Left $ initial r
    (t,x):[]    -> Right $ ((initial r, t), [], (t, x))
    (t,x):xs    -> Right $ ((initial r, t), fmap note' $ mrights (res $ (t,x):xs), head $ mlefts (res $ (t,x):xs))

    where
        note' (t,u,x) = t <-> u =: x
        -- Always returns a 0 or more Right followed by one left
        res :: [(Time, a)] -> [Either (Time, a) (Time, Time, a)]    
        res rs = let (ts,xs) = unzip rs
            in (flip fmap) (withNext ts `zip` xs) $ \((t, mu), x) -> case mu of
                Nothing -> Left (t, x)
                Just u  -> Right (t, u, x)

        -- lenght xs == length (withNext xs)
        withNext :: [a] -> [(a, Maybe a)]
        withNext = go
            where
                go []       = []
                go [x]      = [(x, Nothing)]
                go (x:y:rs) = (x, Just y) : withNext (y : rs)      

withSpan :: Score a -> Score (Span, a)
withSpan = mapEvents (\t d x -> (t-->d,x))
withTime = mapEvents (\t d x -> (t,x))

inSpan t' (range -> (t,u)) = t <= t' && t' < u

mapBefore :: Time -> (Score a -> Score a) -> Score a -> Score a
mapDuring :: Span -> (Score a -> Score a) -> Score a -> Score a
mapAfter :: Time -> (Score a -> Score a) -> Score a -> Score a                            
mapBefore t f x = let (y,n) = (fmap snd *** fmap snd) $ mpartition (\(t2,x) -> t2 < t) (withTime x) in (f y <> n)
mapDuring s f x = let (y,n) = (fmap snd *** fmap snd) $ mpartition (\(t,x) -> t `inSpan` s) (withTime x) in (f y <> n)
mapAfter t f x = let (y,n) = (fmap snd *** fmap snd) $ mpartition (\(t2,x) -> t2 >= t) (withTime x) in (f y <> n)

withMeta :: (Monoid a, AttributeClass a) => (a -> Score b -> Score b) -> Score b -> Score b
withMeta f x = let
    r = (getM x)
    in case splitReactive r of
        Left  a -> f a x
        Right ((a1,t1),as,(t2,a2)) -> mapBefore t1 (f a1) . comp (fmap (\(unnote -> (s,a)) -> mapDuring s (f a)) as) . mapAfter t2 (f a2) $ x
    
    -- Get out the meta :: Reactive a
    -- Iter through events to get span (not just start)
    -- Split it the score into sections


comp = Prelude.foldr (.) id

unnote :: Note a -> (Span, a)
unnote (Note x) = x

(=:) :: Span -> a -> Note a
s =: x  =  Note (s,x)
