
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

module Music.Score.Export.Midi (
    -- * Midi backend
    HasMidiProgram(..),
    Midi,
    HasMidi,
    toMidi,
    writeMidi,
    showMidi,
    openMidi,
  ) where

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import qualified Codec.Midi                    as Midi
import           Control.Comonad               (Comonad (..), extract)
import           Control.Applicative
import           Data.Colour.Names             as Color
import           Data.Default
import           Data.Foldable                 (Foldable)
import qualified Data.Foldable
import           Data.Functor.Couple
import           Data.Maybe
import           Data.Ratio
import           Data.Traversable              (Traversable, sequenceA)
import qualified Music.Lilypond                as Lilypond
import qualified Music.MusicXml.Simple         as MusicXml
import           Music.Score.Internal.Export   hiding (MVoice)
import           System.Process
import           Music.Time.Internal.Quantize
import qualified Text.Pretty                   as Pretty
import qualified Data.List
import           Music.Score.Internal.Util (composed, unRatio, swap, retainUpdates)
import Music.Score.Export.DynamicNotation
import Data.Semigroup.Instances

import Data.Functor.Identity
import Data.Semigroup
import Control.Monad
import Data.VectorSpace hiding (Sum(..))
import Data.AffineSpace
import Control.Lens (over)
import Control.Lens.Operators hiding ((|>))
import qualified Data.List as List

import Music.Time
import Music.Score.Dynamics
import Music.Score.Articulation
import Music.Score.Part
import Music.Score.Tremolo
import Music.Score.Text
import Music.Score.Harmonics
import Music.Score.Slide
import Music.Score.Color
import Music.Score.Ties
import Music.Score.Meta
import Music.Score.Meta.Tempo
import Music.Score.Export.Backend





-- | Class of part types with an associated MIDI program number.
class HasMidiProgram a where
  getMidiChannel :: a -> Midi.Channel
  getMidiProgram :: a -> Midi.Preset
  getMidiChannel _ = 0

instance HasMidiProgram () where
  getMidiProgram _ = 0

instance HasMidiProgram Double where
  getMidiProgram = fromIntegral . floor

instance HasMidiProgram Float where
  getMidiProgram = fromIntegral . floor

instance HasMidiProgram Int where
  getMidiProgram = id

instance HasMidiProgram Integer where
  getMidiProgram = fromIntegral

instance (Integral a, HasMidiProgram a) => HasMidiProgram (Ratio a) where
  getMidiProgram = fromIntegral . floor


-- | A token to represent the Midi backend.
data Midi

-- | We do not need to pass any context to the note export.
type MidiContext = Identity

-- | Every note may give rise to a number of messages. We represent this as a score of messages.
type MidiEvent = Score Midi.Message

-- | The MIDI channel allocation is somewhat simplistic.
--   We use a dedicated channel and program number for each instrument (there *will* be colissions).
type MidiInstr = (Midi.Channel, Midi.Preset)

-- | A Midi file consist of a number of tracks.
--   Channel and preset info is passed on from exportScore to finalizeExport using this type.
--
--   TODO also pass meta-info etc.
--
data MidiScore a = MidiScore [(MidiInstr, Score a)]
  deriving Functor

instance HasBackend Midi where
  type BackendScore   Midi    = MidiScore
  type BackendContext Midi    = MidiContext
  type BackendNote    Midi    = MidiEvent
  type BackendMusic   Midi    = Midi.Midi

  finalizeExport _ (MidiScore trs) = let
    controlTrack  = [(0, Midi.TempoChange 1000000), (endDelta, Midi.TrackEnd)]
    mainTracks    = fmap (uncurry translMidiTrack . fmap join) trs
    in
    Midi.Midi fileType (Midi.TicksPerBeat divisions) (controlTrack : mainTracks)

    where
      translMidiTrack :: MidiInstr -> Score Midi.Message -> [(Int, Midi.Message)]
      translMidiTrack (ch, p) = addTrackEnd
        . setProgramChannel ch p
        . scoreToMidiTrack

      -- Each track needs TrackEnd
      -- We place it a long time after last event just in case (necessary?)
      addTrackEnd :: [(Int, Midi.Message)] -> [(Int, Midi.Message)]
      addTrackEnd = (<> [(endDelta, Midi.TrackEnd)])

      setProgramChannel :: Midi.Channel -> Midi.Preset -> Midi.Track Midi.Ticks -> Midi.Track Midi.Ticks
      setProgramChannel ch prg = ([(0, Midi.ProgramChange ch prg)] <>) . fmap (fmap $ setC ch)

      scoreToMidiTrack :: Score Midi.Message -> Midi.Track Midi.Ticks
      scoreToMidiTrack = fmap (\(t,_,x) -> (round ((t .-. 0) ^* divisions), x)) . toRelative . (^. triples)

      -- Hardcoded values for Midi export
      -- We always generate MultiTrack (type 1) files with division 1024
      fileType    = Midi.MultiTrack
      divisions   = 1024
      endDelta    = 10000

      -- | Convert absolute to relative durations.
      -- TODO replace by something more generic
      toRelative :: [(Time, Duration, b)] -> [(Time, Duration, b)]
      toRelative = snd . List.mapAccumL g 0
          where
              g now (t,d,x) = (t, (0 .+^ (t .-. now),d,x))


instance (HasPart' a, HasMidiProgram (Part a)) => HasBackendScore Midi (Voice a) where
  type BackendScoreEvent Midi (Voice a) = a
  -- exportScore _ xs = MidiScore [((getMidiChannel (xs^?!parts), getMidiProgram (xs^?!parts)), fmap <$> voiceToScore xs)]
  exportScore _ xs = MidiScore [((getMidiChannel (xs^?!parts), getMidiProgram (xs^?!parts)), fmap Identity $ voiceToScore xs)]
    where
      voiceToScore :: Voice a -> Score a
      voiceToScore = error "FIXME"

instance (HasPart' a, Ord (Part a), HasMidiProgram (Part a)) => HasBackendScore Midi (Score a) where
  type BackendScoreEvent Midi (Score a) = a
  exportScore _ xs = MidiScore (map (\(p,sc) -> ((getMidiChannel p, getMidiProgram p), fmap Identity sc)) 
    $ extractPartsWithInfo $ fixTempo $ normalizeScore xs)
    where
      -- We actually want to extract *all* tempo changes and transform the score appropriately
      -- For the time being, we assume the whole score has the same tempo
      fixTempo = stretch (tempoToDuration (metaAtStart xs))

instance HasBackendNote Midi a => HasBackendNote Midi [a] where
  exportNote b ps = mconcat $ map (exportNote b) $ sequenceA ps

instance HasBackendNote Midi Int where
  exportNote _ (Identity pv) = mkMidiNote pv

instance HasBackendNote Midi Integer where
  exportNote _ (Identity pv) = mkMidiNote (fromIntegral pv)

instance HasBackendNote Midi Float where
  exportNote b = exportNote b . fmap (toInteger . round)
  exportChord b = exportChord b . fmap (fmap (toInteger . round))

instance HasBackendNote Midi Double where
  exportNote b = exportNote b . fmap (toInteger . round)
  exportChord b = exportChord b . fmap (fmap (toInteger . round))

instance HasBackendNote Midi a => HasBackendNote Midi (Behavior a) where
  exportNote b = exportNote b . fmap (! 0)
  exportChord b = exportChord b . fmap (fmap (! 0))


instance (Real d, HasBackendNote Midi a) => HasBackendNote Midi (DynamicT d a) where
  -- TODO
  -- We have not standarized dynamic levels
  -- Assume for now -6.5 to 6.5 where (-3.5 is ppp, -0.5 is mp, 0.5 is mf, 3.5 is fff etc)
  exportNote b (Identity (DynamicT (realToFrac -> d, x))) = setV (dynLevel d) <$> exportNote b (Identity x)


dynLevel :: Double -> Midi.Velocity
dynLevel x = round $ (\x -> x * 58.5 + 64) $ f $ inRange (-1,1) (x/3.5)
  where
    f = id
    -- f x = (x^3)
    inRange (m,n) x = (m `max` x) `min` n
  

-- instance (Transformable d, HasBackendNote Midi (DynamicT d a)) => HasBackendNote Midi (DynamicT (Product d) a) where
--   exportNote b = exportNote b . fmap (over dynamic getProduct)
-- 
-- instance (Transformable d, HasBackendNote Midi (DynamicT d a)) => HasBackendNote Midi (DynamicT (Sum d) a) where
--   exportNote b = exportNote b . fmap (over dynamic getSum)


instance HasBackendNote Midi a => HasBackendNote Midi (ArticulationT b a) where
  exportNote b (Identity (ArticulationT (_, x))) = exportNote b (Identity x)

instance HasBackendNote Midi a => HasBackendNote Midi (PartT n a) where
  -- Part structure is handled by HasMidiBackendScore instances, so this is just an identity
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Midi a => HasBackendNote Midi (TremoloT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Midi a => HasBackendNote Midi (TextT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Midi a => HasBackendNote Midi (HarmonicT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Midi a => HasBackendNote Midi (SlideT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Midi a => HasBackendNote Midi (TieT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Midi a => HasBackendNote Midi (ColorT a) where
  exportNote b = exportNote b . fmap extract

mkMidiNote :: Int -> Score Midi.Message
mkMidiNote p = mempty
    |> pure (Midi.NoteOn 0 (fromIntegral $ p + 60) 64)
    |> pure (Midi.NoteOff 0 (fromIntegral $ p + 60) 64)

setV :: Midi.Velocity -> Midi.Message -> Midi.Message
setV v = go
  where
    go (Midi.NoteOff c k _)       = Midi.NoteOff c k v
    go (Midi.NoteOn c k _)        = Midi.NoteOn c k v
    go (Midi.KeyPressure c k _)   = Midi.KeyPressure c k v
    go (Midi.ControlChange c n v) = Midi.ControlChange c n v
    go (Midi.ProgramChange c p)   = Midi.ProgramChange c p
    go (Midi.ChannelPressure c p) = Midi.ChannelPressure c p
    go (Midi.PitchWheel c w)      = Midi.PitchWheel c w
    go (Midi.ChannelPrefix c)     = Midi.ChannelPrefix c

setC :: Midi.Channel -> Midi.Message -> Midi.Message
setC c = go
  where
    go (Midi.NoteOff _ k v)       = Midi.NoteOff c k v
    go (Midi.NoteOn _ k v)        = Midi.NoteOn c k v
    go (Midi.KeyPressure _ k v)   = Midi.KeyPressure c k v
    go (Midi.ControlChange _ n v) = Midi.ControlChange c n v
    go (Midi.ProgramChange _ p)   = Midi.ProgramChange c p
    go (Midi.ChannelPressure _ p) = Midi.ChannelPressure c p
    go (Midi.PitchWheel _ w)      = Midi.PitchWheel c w
    go (Midi.ChannelPrefix _)     = Midi.ChannelPrefix c

-- |
-- Constraint for types that has a MIDI representation.
--
type HasMidi a = (HasBackendNote Midi (BackendScoreEvent Midi a), HasBackendScore Midi a)

toMidi :: HasMidi a => a -> Midi.Midi
toMidi = export (undefined::Midi)

writeMidi :: HasMidi a => FilePath -> a -> IO ()
writeMidi path sc = Midi.exportFile path (toMidi sc)

showMidi :: HasMidi a => a -> IO ()
showMidi = print . toMidi

openMidi :: HasMidi a => a -> IO ()
openMidi score = do
    writeMidi "test.mid" score
    void $ runCommand "timidity test.mid" >>= waitForProcess

