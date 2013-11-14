
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
-- Provides a representation for chords.
--
-------------------------------------------------------------------------------------


module Music.Score.Chord (
        -- * Chord representation
        HasChord(..),
        ChordT(..),      
        
        -- * Chord transformations
        simultaneous,
        simultaneous',
  ) where

import Data.Foldable
import Data.Typeable
import Data.Semigroup
import Control.Monad.Plus       
import qualified Data.List          as List
import qualified Data.List.NonEmpty as NonEmpty

import Music.Time
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Part
import Music.Score.Combinators

class HasChord a where
    type ChordNote a :: *
    getChord :: a -> [ChordNote a]
    -- modifyChord :: (ChordNote a -> ChordNote a) -> a -> a

instance HasChord [a] where
    type ChordNote [a] = a
    getChord = id

instance HasChord (ChordT a) where
    type ChordNote (ChordT a) = a
    getChord (ChordT as)      = as

-- Actually we should use NonEmpty here
-- Empty chords will cause error with HasPitch, among others
newtype ChordT a = ChordT { getChordT :: [a] }
    deriving (Eq, Show, Ord, Monad, Functor, Monoid, Semigroup, Foldable, Typeable)


-- Note:                                                    
--
-- The HasChord instance (for other transformer types) takes care to transform strucuture *above* the chord representation
--      In particular, getChord will extract the chord from below and transform each note (or only the first etc) 
--      as appropriate for the given type.
-- The ChordT instances (of other transformer classes) transforms structure *below* the chord representation
--      For example, it allow us to use functions such as up, down, legato etc on chords.

-- TODO rewrite, generalize?

-- |
-- Group and merge simultaneous events.
--
simultaneous :: Semigroup a => Score a -> Score a
simultaneous = fmap (sconcat . NonEmpty.fromList) . simultaneous'

-- |
-- Group simultaneous events.
--
-- Two events /a/ and /b/ are considered simultaneous if and only if they have the same
-- era, that is if @`era` a == `era` b@
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


-- TODO move these

eras :: Score a -> [Span]
eras sc = fmap getSpan . perform $ sc

events :: Span -> Score a -> [a]
events era sc = fmap getValue . filter (\ev -> getSpan ev == era) . perform $ sc

getValue :: (Time, Duration, a) -> a
getValue (t,d,a) = a

getSpan :: (Time, Duration, a) -> Span
getSpan (t,d,a) = t >-> d        
        
        
     


