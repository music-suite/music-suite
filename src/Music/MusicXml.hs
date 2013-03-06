
-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-- A Haskell representation of MusicXML.
--
-- For an introduction, see <http://www.makemusic.com/musicxml/tutorial>.
--
-------------------------------------------------------------------------------------

module Music.MusicXml (

        -- * Score
        Score(..),
        ScoreHeader(..),
        Identification(..),
        Creator(..),
        Defaults(..),
        ScoreAttrs(..),
        PartAttrs(..),
        MeasureAttrs(..),

        -- ** Part list
        PartList,
        PartListElem(..),

        -- * Music
        Music,
        MusicElem(..),

        -- ** Attributes
        Attributes(..),
        TimeSignature(..),
        ClefSign(..),

        -- ** Notes
        Note(..),
        FullNote(..),
        IsChord,
        noChord,
        Tie(..),
        noTies,
        NoteProps(..),

        -- TODO rewrite these
        mapNoteProps,
        mapNoteProps2,

        -- ** Notations
        Notation(..),
        Articulation(..),
        Ornament(..),
        Technical(..),

        -- ** Directions
        Direction(..),

        -- ** Lyrics
        Lyric(..),




        -- * Basic types

        -- ** Pitch
        Pitch(..),
        DisplayPitch(..),
        PitchClass,
        Semitones(..),
        noSemitones,

        Octaves(..),
        Fifths(..),
        Line(..),

        Mode(..),
        Accidental(..),

        -- ** Time
        Duration(..),
        NoteType(..),

        Divs(..),
        NoteVal(..),
        NoteSize(..),

        Beat(..),
        BeatType(..),


        -- ** Dynamics
        Dynamics,


        -- ** Misc
        StemDirection(..),
        NoteHead(..),

        Level(..),

        BeamType(..),
        StartStopContinue(..),


        -- * Import and export functions
        toXml,
        showXml

  ) where

import Text.XML.Light hiding (Line)

import Music.MusicXml.Score
import Music.MusicXml.Time
import Music.MusicXml.Pitch
import Music.MusicXml.Dynamics
import Music.MusicXml.Read
import Music.MusicXml.Write

import Music.MusicXml.Write.Score

-- --------------------------------------------------------------------------------
-- Import and export functions
-- --------------------------------------------------------------------------------

-- |
-- Render a score as a MusicXML string.
showXml :: Score -> String
showXml = ppTopElement . toXml

-- |
-- Render a score as MusicXML.
toXml :: Score -> Element
toXml = fromSingle . write

-- --------------------------------------------------------------------------------

fromSingle :: [a] -> a
fromSingle [x] = x
fromSingle _   = error "fromSingle: non-single list"

