
{-# LANGUAGE 
    ScopedTypeVariables, 
    GeneralizedNewtypeDeriving,
    DeriveFunctor, 
    DeriveFoldable, 
    DeriveTraversable,
    DeriveDataTypeable, 
    ConstraintKinds,
    FlexibleContexts, 
    GADTs, 
    ViewPatterns,
    TypeFamilies,
    MultiParamTypeClasses, 
    FlexibleInstances #-}

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
-------------------------------------------------------------------------------------

module Music.Score.Meta.Tempo (
        -- * Tempo type
        Bpm,
        NoteValue,
        Tempo,
        metronome,
        tempoNoteValue,
        tempoBeatsPerMinute,
        getTempo,

        -- * Adding tempo to scores
        tempo,
        tempoDuring,
        
        -- * Extracting tempo
        withTempo,       

        -- * Utility
        tempoToDuration,
        -- renderTempo,
  ) where


import Control.Arrow
import Control.Monad.Plus       
import Data.Default
import Data.Void
import Data.Maybe
import Data.Semigroup
import Data.Monoid.WithSemigroup
import Data.Typeable
import Data.String
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Music.Time
import Music.Time.Reactive
import Music.Score.Note
import Music.Score.Voice
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Meta
import Music.Score.Score
import Music.Score.Combinators
import Music.Score.Util
import Music.Pitch.Literal

type Bpm       = Duration
type NoteValue = Duration

-- | Represents musical tempo as a metronome mark with an optional string name.
data Tempo = Tempo (Maybe String) (Maybe Duration) Duration
    deriving (Eq, Ord, Typeable)
-- The internal representation is actually: maybeName maybeDisplayNoteValue scalingFactor

instance Show Tempo where
    show (getTempo -> (nv, bpm)) = "metronome " ++ showR nv ++ " " ++ showR bpm
        where
            showR (realToFrac -> (unRatio -> (x, 1))) = show x
            showR (realToFrac -> (unRatio -> (x, y))) = "(" ++ show x ++ "/" ++ show y ++ ")"

instance Default Tempo where
    def = metronome (1/1) 60

-- | Create a tempo from a duration and a number of beats per minute.
--   
--   For example @metronome (1/2) 48@ means 48 half notes per minute.
metronome :: Duration -> Bpm -> Tempo
metronome noteVal bpm = Tempo Nothing (Just noteVal) $ 60 / (bpm * noteVal)

-- | Get the note value indicated by a tempo.
tempoNoteValue :: Tempo -> Maybe NoteValue
tempoNoteValue (Tempo n nv d) = nv

-- | Get the number of beats per minute indicated by a tempo.
tempoBeatsPerMinute :: Tempo -> Bpm
tempoBeatsPerMinute = snd . getTempo

-- | Get the note value and number of beats per minute indicated by a tempo.
--
-- Typically used with the @ViewPatterns@ extension, as in
--
-- > foo (getTempo -> (nv, bpm)) = ...
--
getTempo :: Tempo -> (NoteValue, Bpm)
getTempo (Tempo _ Nothing x)   = ((1/4), x * 60 / (1/4))
getTempo (Tempo _ (Just nv) x) = (nv, (60 * recip x) / nv)

-- | Convert a tempo to a duration suitable for converting written to sounding durations.
-- 
-- > stretch (tempoToDuration t) notation = sounding
-- > compress (tempoToDuration t) sounding = notation
-- 
tempoToDuration :: Tempo -> Duration
tempoToDuration (Tempo _ _ x) = x

-- | Set the tempo of the given score.
tempo :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => Tempo -> a -> a
tempo c x = tempoDuring (era x) c x

-- | Set the tempo of the given part of a score.
tempoDuring :: (HasMeta a, HasPart' a) => Span -> Tempo -> a -> a
tempoDuring s c = addGlobalMetaNote (s =: (Option $ Just $ Last c))

-- | Extract all tempi from the given score, using the given default tempo. 
withTempo :: (Tempo -> Score a -> Score a) -> Score a -> Score a
withTempo f = withGlobalMeta (f . fromMaybe def . fmap getLast . getOption)

-- TODO must scale and move
renderTempo :: Score a -> Score a
renderTempo = withTempo (stretch . tempoToDuration)


