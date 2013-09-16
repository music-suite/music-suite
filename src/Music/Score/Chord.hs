
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
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
        renderChord,
        simultaneous,
        simultaneous',
  ) where

import Data.Ratio
import Data.Foldable (Foldable(..))
import Data.Typeable
import Data.Semigroup
import Data.List.NonEmpty (NonEmpty(..))
import Data.VectorSpace
import Data.AffineSpace
import Control.Monad.Plus       
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import Music.Score.Voice
import Music.Score.Score
import Music.Time
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Combinators

class HasChord a where
    type ChordNote a :: *
    getChord :: a -> [ChordNote a]
    -- Score a -> Score a
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

-- instance HasChord 

-- Score a -> Score (ChordT a)

-- Note:                                                    
--
-- The HasChord instance (for other transformer types) takes care to transform strucuture *above* the chord representation
--      In particular, getChord will extract the chord from below and transform each note (or only the first etc) 
--      as appropriate for the given type.
-- The ChordT instances (of other transformer classes) transforms structure *below* the chord representation
--      For example, it allow us to use functions such as up, down, legato etc on chords.

-- |
-- Render all chords of a given score into singular notes composed in parallel.
--
renderChord :: (MonadPlus m, HasChord a) => m a -> m (ChordNote a)
renderChord = mscatter . fmap' getChord
    where fmap' = liftM


-- |
-- Merge simultaneous events.
--
-- Two events are considered simultaneous iff their onset and offset are the same.
--
simultaneous :: Semigroup a => Score a -> Score a
simultaneous = fmap (sconcat . NonEmpty.fromList) . simultaneous'

-- |
-- Group simultaneous events.
--
-- Two events are considered simultaneous iff their onset and offset are the same.
--
simultaneous' :: Score a -> Score [a]
simultaneous' sc = compose vs
    where
        -- es :: [Era]
        -- evs :: [[a]]
        -- vs :: [(Time, Duration, [a])]
        es  = List.nub $ eras sc
        evs = fmap (`events` sc) es
        vs  = zipWith (\(t,d) a -> (t,d,a)) es evs

eras :: Score a -> [Era]
eras sc = fmap getEra . perform $ sc

events :: Era -> Score a -> [a]
events era sc = fmap getValue . filter (\ev -> getEra ev == era) . perform $ sc

getValue :: (Time, Duration, a) -> a
getValue (t,d,a) = a

getEra :: (Time, Duration, a) -> Era
getEra (t,d,a) = (t,d)        

type Era = (Time, Duration)

-- Fold over all consecutive events

-- consec (\a b -> if a == b then [a] else [a,b]) [1,1,1,2,3] = [1,2,3]

-- consec :: (a -> a -> [a]) -> [a] -> [a]           

-- consec f as = List.mapAccumL (\(prev, elems) current -> ( (Just x, []) , x)) (Nothing, []) as






-- consec f = go
--     where
--         go []       = []
--         go [a]      = [a]
--         go (a:b:cs) = f a b : go cs
--         
        



