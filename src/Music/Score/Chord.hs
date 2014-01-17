
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    ViewPatterns,
    TypeOperators,
    TypeFamilies,
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
-- Provides a representation for chords.
--
-------------------------------------------------------------------------------------


module Music.Score.Chord (
        -- * Chord representation
        HasChord(..),
        ChordT(..),      

        -- * Voice separation
        separateVoices,
        mergePossible,
        
        -- * Chord transformations
        takeNoteInChord,
        dropNoteInChord,
        takeNotesInChord,
        dropNotesInChord,
        mapSimultaneous,
        simultaneous,
        simultaneous',
  ) where

import Prelude hiding (any, mapM_)

import Data.Ord
import Data.Pointed
import Data.Foldable
import Data.Typeable
import Data.Semigroup
import Control.Monad.Plus hiding (mapM_)       
import qualified Data.List          as List
import qualified Data.List.NonEmpty as NonEmpty

import Music.Time
import Music.Score.Convert
import Music.Score.Note
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Part
import Music.Score.Combinators

class HasChord a where
    type ChordNote a :: *
    -- TODO use NonEmpty
    getChord :: a -> [ChordNote a]

instance HasChord [a] where
    type ChordNote [a] = a
    getChord = id

instance HasChord (ChordT a) where
    type ChordNote (ChordT a) = a
    getChord (ChordT as)      = as

-- TODO Use NonEmpty
newtype ChordT a = ChordT { getChordT :: [a] }
    deriving (Eq, Show, Ord, Monad, Functor, Monoid, Semigroup, Foldable, Typeable)

overlaps :: (HasOnset a, HasOffset a, HasOnset b, HasOffset b) => a -> b -> Bool
overlaps t u = not $ offset t <= onset u || offset u <= onset t

overlapsAny :: (Foldable t, HasOnset a, HasOffset a, HasOnset b, HasOffset b) => a -> t b -> Bool
overlapsAny x = any (overlaps x) 

notOverlaps :: (HasOnset a, HasOnset b, HasOffset a, HasOffset b) => a -> b -> Bool
x `notOverlaps` y = not (x `overlaps` y)

hasOverlapping :: Score a -> Bool
hasOverlapping x = let ns = scoreToNotes x in not $ null [(x,y) | x <- ns, y <- ns, x `overlaps` y, era x /= era y]

-- | Heuristically merge voices if possible
mergePossible :: [Score a] -> [Score a]
mergePossible []  = []
mergePossible (x:xs) = let
    pick = x
    (res, rest) = List.foldr mergeMaybe' (x, []) xs
    in res : mergePossible rest 

mergeMaybe' x (y,rest) = if hasOverlapping (x <> y) then (x, y:rest) else (x <> y, rest)    

mergeMaybe x y = if hasOverlapping (x <> y) then (x, Just y) else (x <> y, Nothing)    



notOverlapsHead :: (HasOnset a, HasOnset b, HasOffset a, HasOffset b) => a -> [b] -> Bool
x `notOverlapsHead` [] = True
x `notOverlapsHead` xs = x `notOverlaps` head xs

class Null a where
    isNull :: a -> Bool
-- > isNull mempty
instance Null [a] where
    isNull = null
nonNull = not . isNull
class Divisible a where
    divide :: a -> (a, a)



data Tower a = Tower [a] a [a]
    deriving (Functor, Eq, Show)

tower x = Tower (repeat mempty) x (repeat mempty)
moveUp   (Tower (a:as) x (b:bs)) = Tower (x:a:as) b bs
moveDown (Tower (a:as) x (b:bs)) = Tower as a (x:b:bs)

top :: (Monoid a, Null a) => Tower a -> [a]
top (Tower as x sa) = List.takeWhile nonNull sa

middle :: Tower a -> a
middle (Tower as x sa) = x

bottom :: (Monoid a, Null a) => Tower a -> [a]
bottom (Tower as x sa) = reverse (List.takeWhile nonNull as)

-- semantic function
floors :: (Monoid a, Null a) => Tower a -> ([a], a, [a])
floors t = (bottom t, middle t, top t)


compareHead x []    = EQ
compareHead x (y:_) = x `compare` y
comparingHead p x y = compareHead (p x) (fmap p y)
compareHeadVal = comparingHead getNoteValue

pushNote :: Ord a => Note a -> Tower [Note a] -> Tower [Note a]
pushNote n t = if n `notOverlapsHead` middle t then pushMiddle n t else
        case n `compareHeadVal` middle t of
            GT -> moveUp   $ pushNote n (moveDown t)
            _  -> moveDown $ pushNote n (moveUp t)

pushMiddle :: a -> Tower [a] -> Tower [a]
pushMiddle x (Tower as a sa) = Tower as (x:a) sa
    
separateVoices :: Ord a => Score a -> [Score ( a)]
separateVoices = {-fmap scoreToVoice . -}fmap notesToScore . (\(as,x,bs) -> as++[x]++bs) . floors . List.foldr pushNote (tower []) 
    . List.sortBy (comparing getNoteSpan) . scoreToNotes

-- DEBUG
instance Num a => Num (Score a) where
    fromInteger = return . fromInteger

-- Note:                                                    
--
-- The HasChord instance (for other transformer types) takes care to transform strucuture *above* the chord representation
--      In particular, getChord will extract the chord from below and transform each note (or only the first etc) 
--      as appropriate for the given type.
-- The ChordT instances (of other transformer classes) transforms structure *below* the chord representation
--      For example, it allow us to use functions such as up, down, legato etc on chords.

-- TODO rewrite, generalize?

takeNotesInChord n = mapSimultaneous (fmap $ take n)
dropNotesInChord n = mapSimultaneous (fmap $ drop n)

takeNoteInChord n = mapSimultaneous $ (fmap $ take 1) . (fmap $ drop (n - 1))
dropNoteInChord n = mapSimultaneous $ (fmap $ drop1 n)

drop1 n xs = take (n - 1) xs <> drop n xs


-- | 
-- Process all simultaneous events.
-- 
-- Two events /a/ and /b/ are considered simultaneous if and only if they have the same
-- era, that is if @`era` a == `era` b@
-- 
mapSimultaneous :: (Score [a] -> Score [b]) -> Score a -> Score b
mapSimultaneous f = mscatter . f . simultaneous'

-- |
-- Merge all simultaneous events using their 'Semigroup' instance.
--
-- Two events /a/ and /b/ are considered simultaneous if and only if they have the same
-- era, that is if @`era` a == `era` b@
--
simultaneous :: Semigroup a => Score a -> Score a
simultaneous = fmap (sconcat . NonEmpty.fromList) . simultaneous'

-- |
-- Group simultaneous events as lists.
--
-- Two events /a/ and /b/ are considered simultaneous if and only if they have the same
-- era, that is if @`era` a == `era` b@
--
-- Note that 'simultaneous' is identical to 'simultaneous' @.@ 'fmap' 'return'
--
simultaneous' :: Score a -> Score [a]
simultaneous' sc = setScoreMeta m $ compose vs
    where     
        m = getScoreMeta sc
        -- es :: [Era]
        -- evs :: [[a]]
        -- vs :: [(Time, Duration, [a])]
        es  = List.nub $ eras sc
        evs = fmap (`events` sc) es
        vs  = zipWith (\(delta -> (t,d)) a -> (t,d,a)) es evs


-- TODO (re)move these

eras :: Score a -> [Span]
eras sc = fmap getSpan . perform $ sc

events :: Span -> Score a -> [a]
events era sc = fmap getValue . filter (\ev -> getSpan ev == era) . perform $ sc

getValue :: (Time, Duration, a) -> a
getValue (t,d,a) = a

getSpan :: (Time, Duration, a) -> Span
getSpan (t,d,a) = t >-> d        
        
        
     


