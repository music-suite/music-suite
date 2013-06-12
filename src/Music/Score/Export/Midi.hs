
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    TypeOperators,
    OverloadedStrings,
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
-------------------------------------------------------------------------------------

module Music.Score.Export.Midi (
        HasMidi(..),
        toMidi,
        toMidiTrack,
        writeMidi,
        playMidi,
        playMidiIO,
  ) where

import Prelude hiding (foldr, concat, foldl, mapM, concatMap, maximum, sum, minimum)

import Data.Semigroup
import Data.Ratio
import Data.String
import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Plus
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Typeable
import Data.Traversable
import Data.Function (on)
import Data.Ord (comparing)
import Data.VectorSpace
import Data.AffineSpace
import Data.Basis

import Control.Reactive
import Control.Reactive.Midi

import Music.Time
import Music.Pitch.Literal
import Music.Dynamics.Literal
import Music.Score.Rhythm
import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Combinators
import Music.Score.Zip
import Music.Score.Pitch
import Music.Score.Ties
import Music.Score.Part
import Music.Score.Articulation
import Music.Score.Dynamics
import Music.Score.Ornaments
import Music.Score.Export.Util

import qualified Codec.Midi as Midi
import qualified Music.MusicXml.Simple as Xml
import qualified Music.Lilypond as Lilypond
import qualified Text.Pretty as Pretty
import qualified Data.Map as Map
import qualified Data.List as List


-- |
-- Class of types that can be converted to MIDI.
--
-- Numeric types are interpreted as notes with a default velocity, pairs are
-- interpreted as @(pitch, velocity)@ pairs.
--
-- Minimal definition: 'getMidi'. Given 'getMidiScore', 'getMidi' can be implemented
-- as @getMidiScore . return@.
--
class HasMidi a where

    -- | Convert a value to a MIDI score.
    --   Typically, generates an /on/ event using 'note' followed by an optional /off/ event.
    getMidi :: a -> Score Midi.Message

    -- | Convert a score to a MIDI score.
    --   The default definition can be overriden for efficiency.
    getMidiScore :: Score a -> Score Midi.Message
    getMidiScore = (>>= getMidi)


instance HasMidi (Integer, Integer) where
    getMidi (p,v) = mempty
        |> return (Midi.NoteOn 0 (fromIntegral p) (fromIntegral v))
        |> return (Midi.NoteOff 0 (fromIntegral p) (fromIntegral v))

instance HasMidi Midi.Message               where   getMidi = return
instance HasMidi Int                        where   getMidi = getMidi . toInteger
instance HasMidi Integer                    where   getMidi = \x -> getMidi (x,100::Integer)
instance HasMidi Float                      where   getMidi = getMidi . toInteger . round
instance HasMidi Double                     where   getMidi = getMidi . toInteger . round
instance Integral a => HasMidi (Ratio a)    where   getMidi = getMidi . toInteger . round
instance HasMidi a => HasMidi (Maybe a)     where   getMidi = getMidiScore . mfromMaybe


-- |
-- Convert a score to a MIDI file representation.
--
toMidi :: HasMidi a => Score a -> Midi.Midi
toMidi score = Midi.Midi fileType divisions' [controlTrack, eventTrack]
    where
        endPos          = 10000
        fileType        = Midi.MultiTrack
        divisions       = 1024
        divisions'      = Midi.TicksPerBeat divisions
        controlTrack    = [(0, Midi.TempoChange 1000000), (endPos, Midi.TrackEnd)]
        eventTrack      = events <> [(endPos, Midi.TrackEnd)]

        events :: [(Midi.Ticks, Midi.Message)]
        events          = (\(t,_,x) -> (round (t * divisions), x)) <$> performance

        performance :: [(Time, Duration, Midi.Message)]
        performance     = (toRelative . perform) (getMidiScore score)

        -- FIXME arbitrary endTime (files won't work without this...)
        -- TODO render voices separately

-- |
-- Convert a score to a track of MIDI messages.
--
toMidiTrack :: HasMidi a => Score a -> Track Message
toMidiTrack = Track . fmap (\(t,_,m) -> (t,m)) . perform . getMidiScore

-- |
-- Convert a score MIDI and write to a file.
--
writeMidi :: HasMidi a => FilePath -> Score a -> IO ()
writeMidi path sc = Midi.exportFile path (toMidi sc)

-- |
-- Convert a score to a MIDI event.
--
playMidi :: HasMidi a => String -> Score a -> Event MidiMessage
playMidi dest x = midiOut midiDest $ playback trig (pure $ toTrack $ delay 0.2 x)
    where
        -- trig        = accumR 0 ((+ 0.005) <$ pulse 0.005)
        trig        = time
        toTrack     = fmap (\(t,_,m) -> (t,m)) . perform . getMidiScore
        midiDest    = fromJust $ unsafeGetReactive (findDestination  $ pure dest)

-- |
-- Convert a score to a MIDI event and run it.
--
playMidiIO :: HasMidi a => String -> Score a -> IO ()
playMidiIO dest = runLoop . playMidi dest

