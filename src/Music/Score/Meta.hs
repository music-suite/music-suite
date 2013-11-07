
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
        Tempo,
        Clef(..),
        addClef,
        addClefDuring,
        withMeta
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
import Music.Time.Reactive
import Music.Score.Note
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Combinators
import Music.Score.Util

-- TODO
data Clef = GClef | CClef | FClef
    deriving (Eq, Ord, Show, Typeable)

addClef :: Clef -> Score a -> Score a
addClef c x = addClefDuring (onset x <-> offset x) c x

addClefDuring :: Span -> Clef -> Score a -> Score a
addClefDuring s c = addM (s =: (Option $ Just $ Last c))

type TimeSignature = ([Integer], Integer)

type KeySignature = (Integer, Bool)

type Tempo = Duration

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

-- Transform the score with the current value of some meta-information
-- Each "update chunk" of the meta-info is processed separately 
-- FIXME don't kill the meta-track...
withMeta :: (Monoid a, AttributeClass a) => (a -> Score b -> Score b) -> Score b -> Score b
withMeta f x = let
    r = getM x
    in case splitReactive r of
        Left  a -> f a x
        Right ((a1,t1),as,(t2,a2)) -> mapBefore t1 (f a1) . composed (fmap (\(unnote -> (s,a)) -> mapDuring s (f a)) as) . mapAfter t2 (f a2) $ x


