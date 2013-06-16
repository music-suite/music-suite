
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
-- Provides articulation.
--
-------------------------------------------------------------------------------------


module Music.Score.Chord (
        HasChord(..),
        ChordT(..),
        flatten,
  ) where

import Data.Ratio
import Data.Foldable
import Data.Typeable
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Control.Monad.Plus

import Music.Score.Voice
import Music.Score.Score
import Music.Time
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Combinators

class HasChord a where
    type ChordNote a :: *
    getChord :: a -> [ChordNote a]

-- Actually we should use NonEmpty here
-- Empty chords will cause error with HasPitch, among others
newtype ChordT a = ChordT { getChordT :: [a] }
    deriving (Eq, Show, Ord, Functor, Foldable, Typeable)

-- instance HasChord 

-- Score a -> Score (ChordT a)

-- Note:                                                    
--
-- The HasChord instance (for various types) takes care to transform strucuture *above* the chord representation
--      In particular, getChord will extract the chord from below and transform each note (or only the first etc) 
--     as appropriate for the given type.
-- The ChordT instances transforms structure *below* the chord representation

flatten :: (MonadPlus m, HasChord a) => m a -> m (ChordNote a)
flatten = mscatter . liftM getChord


