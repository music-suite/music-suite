{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------

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
-- Provides MIDI import.
--
-- /Warning/ Experimental module.
module Music.Score.Import.Midi
  ( IsMidi (..),
    fromMidi,
    readMidi,
    readMidiMaybe,
    readMidiEither,
  )
where

import Codec.Midi (Midi)
-- import           Control.Reactive          hiding (Event)
-- import qualified Control.Reactive          as R
-- import           Control.Reactive.Midi

import qualified Codec.Midi as Midi
import Control.Applicative
import Control.Lens
import Control.Monad.Plus
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map as Map
import qualified Data.Maybe
import Data.Monoid
import Music.Dynamics.Literal
import Music.Pitch.Literal (IsPitch)
import Music.Pitch.Literal
import qualified Music.Pitch.Literal as Pitch
import Music.Score.Articulation
import Music.Score.Dynamics
import Music.Score.Harmonics
import Music.Score.Internal.Export
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Slide
import Music.Score.Text
import Music.Score.Ties
import Music.Score.Tremolo
import Music.Time
import qualified Text.Pretty as Pretty

-- import qualified Data.ByteString.Lazy as ByteString

-- |
-- This constraint includes all note types that can be constructed from a Midi representation.
type IsMidi a =
  ( -- TODO
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

-- -- type SimpleMidi = [[(Time, Bool, Int, Int)]]  -- outer: track, inner: channel, time, on/off, pitch, vel
-- -- -- Ignore offset velocities (can't represent them)
-- -- type SimpleMidi2 = [[(Span, Int, Int)]] -- outer: track, inner: channel, time, pitch, vel
-- --
-- -- foo :: SimpleMidi2
-- -- foo = undefined
--
-- mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
-- mapWithIndex f = zipWith f [0..]
--
-- mapWithIndex2 :: (Int -> Int -> a -> b) -> [[a]] -> [b]
-- mapWithIndex2 f xss = concat $ zipWith (\m -> zipWith (f m) [0..]) [0..] xss
--
-- -- Last time the given key was pressed but not released (non-existant means it is not pressed)
-- type ChanMap = Map Int Time
--
-- -- |
-- -- Convert a score from a Midi representation.
-- --
fromMidi :: IsMidi a => Midi -> Score a
fromMidi m = undefined

--   where
--
-- toAspects :: [[Event (Midi.Channel,Midi.Key,Midi.Velocity)]] -> [Event (Part,Int,Int)]
-- toAspects = mapWithIndex (\trackN events -> over (mapped.event) (\(s,(ch,key,vel)) -> undefined))
--
-- getMidi :: Midi.Midi -> [[Event (Midi.Channel,Midi.Key,Midi.Velocity)]]
-- getMidi (Midi.Midi fileType timeDiv tracks) = id
--       $ compress (ticksp timeDiv)
--       $ fmap mcatMaybes
--       $ fmap snd
--       $ fmap (List.mapAccumL g mempty)
--       $ fmap mcatMaybes $ over (mapped.mapped) getMsg tracks
--   where
--     g keyStatus (t,onOff,c,p,v) =
--       ( updateKeys onOff p (fromIntegral t) keyStatus
--       , (if onOff then Nothing else Just (
--         (Data.Maybe.fromMaybe 0 (Map.lookup (fromIntegral t) keyStatus)<->fromIntegral t,(c,p,60))^.event))
--       )
--     -- TODO also store dynamics in pitch map (to use onset value rather than offset value)
--     -- For now just assume 60
--     updateKeys True  p t = Map.insert p t
--     updateKeys False p _ = Map.delete p
--
--     -- Amount to compress time (after initially treating each tick as duration 1)
--     ticksp (Midi.TicksPerBeat n)     = 1 / fromIntegral n
--     ticksp (Midi.TicksPerSecond _ _) = error "fromMidi: Can not parse TickePerSecond-based files"
--
-- getMsg (t, Midi.NoteOff c p v) = Just (t,False,c,p,v)
-- getMsg (t, Midi.NoteOn c p 0)  = Just (t,False,c,p,0)
-- getMsg (t, Midi.NoteOn c p v)  = Just (t,True,c,p,v)
-- -- TODO key pressure
-- -- control change
-- -- program change
-- -- channel pressure
-- -- pitch wheel
-- -- etc.
-- getMsg _ = Nothing
--

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
readMidi :: IsMidi a => FilePath -> IO (Score a)
readMidi path = fmap (either (\x -> error $ "Could not read MIDI file" ++ x) id) $ readMidiEither path

-- |
-- Read a Midi score from a file. Fails if the file could not be read, and returns
-- @Nothing@ if a parsing error occurs.
readMidiMaybe :: IsMidi a => FilePath -> IO (Maybe (Score a))
readMidiMaybe path = fmap (either (const Nothing) Just) $ readMidiEither path

-- |
-- Read a Midi score from a file. Fails if the file could not be read, and returns
-- @Left m@ if a parsing error occurs.
readMidiEither :: IsMidi a => FilePath -> IO (Either String (Score a))
readMidiEither path = fmap (fmap fromMidi) $ Midi.importFile path
