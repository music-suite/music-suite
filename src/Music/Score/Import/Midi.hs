
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}

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
-- Provides MIDI import.
--
-- /Warning/ Experimental module.
--
-------------------------------------------------------------------------------------
module Music.Score.Import.Midi (
        IsMidi(..),
        fromMidi,
        readMidi,
        readMidiMaybe,
        readMidiEither
  ) where

import           Music.Pitch.Literal       (IsPitch)

import           Codec.Midi                hiding (Track)
import           Control.Applicative
import           Control.Lens
-- import           Control.Reactive          hiding (Event)
-- import qualified Control.Reactive          as R
-- import           Control.Reactive.Midi

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Score.Articulation
import           Music.Score.Dynamics
import           Music.Score.Export.Common
import           Music.Score.Harmonics
import           Music.Score.Part
import           Music.Score.Pitch
import           Music.Score.Rhythm
import           Music.Score.Slide
import           Music.Score.Text
import           Music.Score.Ties
import           Music.Score.Tremolo
import           Music.Time

import qualified Codec.Midi                as Midi
import qualified Data.List                 as List
import qualified Data.Map                  as Map
import qualified Music.Lilypond            as Lilypond
import qualified Music.MusicXml.Simple     as Xml
import qualified Text.Pretty               as Pretty

import qualified Music.Pitch.Literal       as Pitch
-- import qualified Data.ByteString.Lazy as ByteString

-- |
-- This constraint includes all note types that can be constructed from a Midi representation.
--
type IsMidi a = (
    -- TODO
    IsPitch a,
    HasPart' a,
    Ord (Part a),
    Enum (Part a),
    -- HasPitch a,
    Num (Pitch a),
    HasTremolo a,
    HasArticulation a a,
    Tiable a
    )


-- |
-- Convert a score from a Midi representation.
--
fromMidi :: IsMidi a => Midi -> Score a
fromMidi = undefined
    -- Map each track to a part (scanning for ProgramChange, name etc)
    -- Subdivide parts based on channels
    -- Set channel 10 tracks to "percussion"

    -- Remove all non-used messages (KeyPressure, ChannelPressure, ProgramChange)
    -- Create reactives from variable values
    -- Create notes
    -- Superimpose variable values

    -- Compose
    -- Add meta-information

-- TODO

-- |
-- Read a Midi score from a file. Fails if the file could not be read or if a parsing
-- error occurs.
--
readMidi :: IsMidi a => FilePath -> IO (Score a)
readMidi path = fmap (either (\x -> error $ "Could not read MIDI file" ++ x) id) $ readMidiEither path

-- |
-- Read a Midi score from a file. Fails if the file could not be read, and returns
-- @Nothing@ if a parsing error occurs.
--
readMidiMaybe :: IsMidi a => FilePath -> IO (Maybe (Score a))
readMidiMaybe path = fmap (either (const Nothing) Just) $ readMidiEither path

-- |
-- Read a Midi score from a file. Fails if the file could not be read, and returns
-- @Left m@ if a parsing error occurs.
--
readMidiEither :: IsMidi a => FilePath -> IO (Either String (Score a))
readMidiEither path = fmap (fmap fromMidi) $ importFile path


