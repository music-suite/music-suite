
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Music.Lilypond -- (
  -- )
where

import Data.Ratio
import Data.Semigroup
import Text.Pretty
import Music.Lilypond.Pitch
import Music.Pitch.Literal

{-
data Lilypond 
    = Book      Id [BookBlock]
    | BookPart  Id [BookPartBlock]
    | Score     Id [ScoreBlock]

data BookBlock
    = Paper    OutputDef
    | Bookpart Id [BookPartBlock]
    | Score    Id [ScoreBlock]
    | Music    CompositeMusic
    -- full markup etc

data BookPartBlock
    = BookPartPaper    OutputDef
    | BookPartScore    Id [ScoreBlock]
    | BookPartMusic    CompositeMusic
    -- full markup etc
    
data ScoreBlock
    = ScoreMusic     Music
    -- full markup etc
-}

type Dur = Ratio Int

data Music    
    = Note Note (Maybe Dur) [PostEvent]         -- ^ A single note.
    | Chord [Note] (Maybe Dur) [PostEvent]      -- ^ A single chord.
    | Sequential   [Music]                      -- ^ Sequential composition.
    | Simultaneous [Music]                      -- ^ Parallel composition.
    | Repeat Bool Int Music (Maybe Music)       -- ^ Repetition (unfold, times, music, alt).
    | Transpose Interval Music                  -- ^ Transpose music
    | Times Rational Music                      -- ^ Stretch music
    | Relative Pitch Music                      -- ^ Use relative pitch
    | Clef Clef                                 -- ^ 
    | KeySignature Key                          -- ^
    | TimeSignature Int Int                     -- ^ 
    | Breathe BreathingSign                     -- ^ Breath mark (caesura)
    | MetronomeMark (Maybe String) Dur Int Int  -- ^ Metronome mark (text, duration, dots, bpm).
    | TempoMark String                          -- ^ Tempo mark.
    deriving (Eq, Show)

-- TODO tremolo
-- TODO percent repeats

instance Pretty Music where
    pretty (Note n d p)   = pretty n <> pretty d <> pretty p

    pretty (Chord ns d p) = char '<' <> (sepByS (char 'x') $ map pretty ns) <> char '>' <> pretty d <> pretty p

    pretty (Sequential xs)          = string "{" <+> prettyList xs <+> string "}"

    pretty (Simultaneous xs)        = string "<<" <+> prettyList xs <+> string ">>"

    pretty (Repeat unfold times x y)  = string "\\repeat" <+> unf unfold <+> int times <+> pretty x <+> alt y
        where 
            unf p = if p then string "unfold" else string "volta"
            alt Nothing  = empty
            alt (Just x) = string "\\alternative" <> pretty x

    pretty (Transpose intv x)       = notImpl
    pretty (Times rat x)            = notImpl
    pretty (Relative pitch x)       = notImpl
    pretty _                        = notImpl


    -- | Slur Bool                                 -- ^ Begin or end slur
    -- | Phrase Bool                               -- ^ Begin or end phrase slur

data Note
    = NotePitch Pitch (Maybe OctaveCheck)
    | DrumNotePitch (Maybe Dur)
    deriving (Eq, Show)
    -- TODO lyrics 

instance Pretty Note where
    pretty (NotePitch p Nothing)   = pretty p
    pretty (NotePitch p _)   = notImpl
    pretty (DrumNotePitch _) = notImpl

instance Pretty Pitch where
    pretty (Pitch (c,a,o)) = string $ pc c ++ acc a ++ oct (o-4)
        where
            pc C = "c"
            pc D = "d"
            pc E = "e"
            pc F = "f"
            pc G = "g"
            pc A = "a"
            pc B = "b"            
            acc n | n <  0  =  concat $ replicate (negate n) "es"
                  | n == 0  =  ""
                  | n >  0  =  concat $ replicate (n) "is"
            oct n | n <  0  =  concat $ replicate (negate n) ","
                  | n == 0  =  ""
                  | n >  0  =  concat $ replicate n "'"

instance IsPitch Pitch where
    fromPitch (PitchL (c, Nothing, o)) = Pitch (toEnum c, 0,       o)                 
    fromPitch (PitchL (c, Just a, o))  = Pitch (toEnum c, round a, o)

data Exclamation = Exclamation
    deriving (Eq, Show)

data Clef
    = Treble
    | Alto
    | Tenor
    | Bass
    | French
    | Soprano
    | MezzoSoprano
    | Baritone
    | VarBaritone
    | SubBass
    | Percussion
    | Tab
    deriving (Eq, Show)

data KeyMode = Major | Minor
    deriving (Eq, Show)

newtype Key = Key (PitchClass, KeyMode)
    deriving (Eq, Show)

data BreathingSign
    = RightVarComma
    | StraightCaesura
    | CurvedCaesura
    deriving (Eq, Show)

data Articulation
    = Accent
    | Marcato
    | Staccatissimo
    | Espressivo
    | Staccato
    | Tenuto
    | Portato
    | Upbow
    | Downbow
    | Flageolet
    | Thumb
    | LeftHeel
    | RightHeel
    | LeftToe
    | RightToe
    | Open
    | Stopped
    | Turn
    | ReverseTurn
    | Trill
    | Prall
    | Mordent
    | PrallPrall
    | PrallMordent
    | UpPrall
    | DownPrall
    | UpMordent
    | DownMordent
    | PrallDown
    | PrallUp
    | LinePrall
    | SignumCongruentie
    | ShortFermata
    | Fermata
    | LongFermata
    | VeryLongFermata
    | Segno
    | Coda
    | VarCoda
    deriving (Eq, Show)

data Question = Question
    deriving (Eq, Show)
data OctaveCheck = OctaveCheck
    deriving (Eq, Show)
data PostEvent = PostEvent
    deriving (Eq, Show)

instance Pretty PostEvent where pretty = error "PostEvent"
                                           
-- data ChangeHead
--     = NoteMode
--     | DrumMode
--     | FigureMdoe
--     | ChordMode
--     | LyricMode


notImpl = error "Not implemented"
asPitch = id
asPitch :: Pitch -> Pitch


