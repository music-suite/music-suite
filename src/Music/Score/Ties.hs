
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    GeneralizedNewtypeDeriving #-}

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
-- Provides a representation for tied notes, and a way to split a single note
-- into a pair of tied notes.
--
-------------------------------------------------------------------------------------

module Music.Score.Ties (
        -- * Tiable class
        Tiable(..),
        TieT(..),        
        
        -- * Splitting tied notes in scores
        -- splitTies,
        -- splitTiesSingle,
        splitTiesVoice,
        splitTiesVoiceAt,
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Plus
import Data.Default
import Data.Maybe
import Data.Ratio
import Data.Foldable hiding (concat)
import Data.Typeable
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace

import Music.Score.Voice
import Music.Score.Score
import Music.Score.Combinators
import Music.Score.Convert
import Music.Score.Part
import Music.Time

-- |
-- Class of types that can be tied. Ties are added to a score by splitting a single note
-- into two and annotating them with a /begin tie/ and /end tie/ mark respectively.
-- 
--
-- Minimal definition: 'toTied', or both 'beginTie' and 'endTie'.
--
class Tiable a where
    -- |
    -- Modify a note to be the first note in a tied note pair.
    -- 
    beginTie :: a -> a
    beginTie = fst . toTied

    -- |
    -- Modify a note to be the second note in a tied note pair.
    -- 
    endTie :: a -> a
    endTie = snd . toTied

    -- | 
    -- Split a single note into a pair of tied notes.
    --
    -- The first returned element should have the original 'onset' and the second
    -- element should have the original 'offset'. Formally
    --
    -- > (onset . fst . toTied) a = onset a
    -- > (offset . snd . toTied) a = offset a
    --
    toTied    :: a -> (a, a)
    toTied a = (beginTie a, endTie a)

newtype TieT a = TieT { getTieT :: (Bool, a, Bool) }
    deriving (Eq, Ord, Show, Functor, Foldable, Typeable)

instance Tiable Double      where { beginTie = id ; endTie = id }
instance Tiable Float       where { beginTie = id ; endTie = id }
instance Tiable Int         where { beginTie = id ; endTie = id }
instance Tiable Integer     where { beginTie = id ; endTie = id }
instance Tiable ()          where { beginTie = id ; endTie = id }
instance Tiable (Ratio a)   where { beginTie = id ; endTie = id }

instance Tiable a => Tiable (Maybe a) where
    beginTie = fmap beginTie
    endTie   = fmap endTie

instance Tiable a => Tiable (TieT a) where
    toTied (TieT (prevTie, a, nextTie))   = (TieT (prevTie, b, True), TieT (True, c, nextTie))
         where (b,c) = toTied a

-- |
-- Split all notes that cross a barlines into a pair of tied notes.
--
splitTiesVoice :: Tiable a => Voice a -> Voice a
splitTiesVoice = voice . concat . snd . List.mapAccumL g 0 . getVoice
    where
        g t (d, x) = (t + d, occs)
            where
                (_, barTime) = properFraction t
                remBarTime   = 1 - barTime
                occs         = splitDurThen remBarTime 1 (d,x)

-- |
-- Split all voice into bars, using the given bar durations. Music that does not
-- fit into the given durations is discarded.
--
-- Notes that cross a barlines are split into tied notes.
--
splitTiesVoiceAt :: Tiable a => [Duration] -> Voice a -> [Voice a]
splitTiesVoiceAt barDurs x = fmap voice $ splitTiesVoiceAt' barDurs (getVoice x)

splitTiesVoiceAt' :: Tiable a => [Duration] -> [(Duration, a)] -> [[(Duration, a)]]
splitTiesVoiceAt' []  _  =  []
splitTiesVoiceAt' _  []  =  []
splitTiesVoiceAt' (barDur : rbarDur) occs = case splitDurFor barDur occs of
    (barOccs, [])       -> barOccs : []
    (barOccs, restOccs) -> barOccs : splitTiesVoiceAt' rbarDur restOccs

tsplitTiesVoiceAt :: [Duration] -> [Duration] -> [[(Duration, Char)]]
tsplitTiesVoiceAt barDurs = fmap getVoice . splitTiesVoiceAt barDurs . voice . fmap (\x -> (x,'_'))

-- |
-- Split an event into one chunk of the duration @s@, followed parts shorter than duration @t@.
--
-- The returned list is always non-empty. All elements but the first and the last must have duration @t@.
--
-- > sum $ fmap fst $ splitDur s (x,a) = x
--
splitDurThen :: Tiable a => Duration -> Duration -> (Duration, a) -> [(Duration, a)]
splitDurThen s t x = case splitDur s x of
    (a, Nothing) -> [a]
    (a, Just b)  -> a : splitDurThen t t b


-- | 
-- Extract as many notes or parts of notes as possible in the given positive duration, and
-- return it with remaining notes.
--
-- The extracted notes always fit into the given duration, i.e.
--
-- > sum $ fmap duration $ fst $ splitDurFor maxDur xs <= maxDur
--
-- If there are remaining notes, they always fit exactly, i.e.
--
-- > sum $ fmap duration $ fst $ splitDurFor maxDur xs == maxDur  iff  (not $ null $ snd $ splitDurFor maxDur xs)
--
splitDurFor :: Tiable a => Duration -> [(Duration, a)] -> ([(Duration, a)], [(Duration, a)])
splitDurFor remDur []       = ([], [])
splitDurFor remDur (x : xs) = case splitDur remDur x of
    (x@(d,_), Nothing)   -> 
        if d < remDur then
            first (x:) $ splitDurFor (remDur - d) xs
        else -- d == remDur
            ([x], xs)
    (x@(d,_), Just rest) -> ([x], rest : xs)

tsplitDurFor :: Duration -> [Duration] -> ([(Duration,Char)], [(Duration,Char)])
tsplitDurFor maxDur xs = splitDurFor maxDur $ fmap (\x -> (x,'_')) xs
instance Tiable Char where
    toTied _ = ('(',')')

-- |
-- Split a note if it is longer than the given duration. Returns the first part of the
-- note (which always <= s) and the rest.
--
-- > splitDur maxDur (d,a)
--
splitDur :: Tiable a => Duration -> (Duration, a) -> ((Duration, a), Maybe (Duration, a))
splitDur maxDur (d,a)
    | maxDur <= 0 = error "splitDur: maxDur must be > 0"
    | d <= maxDur =  ((d, a), Nothing)
    | d >  maxDur =  ((maxDur, b), Just (d - maxDur, c)) where (b,c) = toTied a




