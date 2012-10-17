
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Music.Lilypond -- (
  -- )
where

import Text.PrettyPrint
import Music.Lilypond.Pitch

-- Doc
-- empty <> <+> hcat hsep
-- $$ $+$ vcat sep cat
-- char text
-- int integer float rational
-- parens brackets braces quotes doubleQuotes
-- mest hang punctuate
-- renderStyle

list :: a -> ([b] -> a) -> [b] -> a
list z f [] = z
list z f xs = f xs

class Pretty a where
    pretty :: a -> Doc
    pconcat :: [a] -> Doc
    pconcat = sep . map pretty

instance Pretty Integer where
    pretty = integer

instance (Pretty a, Pretty b) => Pretty (a,b) where
    pretty (x, y) = parens $ pretty x `g` pretty y
        where x `g` y = x <> char ',' <> y

instance Pretty a => Pretty [a] where
    pretty = brackets . sepBy (char ',') . map pretty

string = text

-- |
-- Join with separator.
--
-- > initBy q [x1,x2..xn] = x1 <> q <> x2 <> q .. xn.
sepBy :: Doc -> [Doc] -> Doc

-- |
-- Join with initiator.
--
-- > initBy q [x1,x2..xn] = q <> x1 <> q <> x2 <> q .. xn.
initBy :: Doc -> [Doc] -> Doc

-- |
-- Join with terminator.
--
-- > initBy q [x1,x2..xn] = x1 <> q <> x2 <> q .. xn <> q.
termBy :: Doc -> [Doc] -> Doc
sepBy  p = list empty $ \(x:xs) -> x <> initBy p xs
initBy p = hcat . map (p <>)
termBy p = hcat . map (<> p)


sepByS :: Doc -> [Doc] -> Doc
initByS :: Doc -> [Doc] -> Doc
termByS :: Doc -> [Doc] -> Doc
sepByS  p = list empty $ \(x:xs) -> x <> initByS p xs
initByS p = hcat . map (p <+>)
termByS p = hsep . map (<> p)

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

-- TODO use invervals Numeric.Inteval ?
type Interval = (Pitch, Pitch)
type Dur = Rational

data Music
    = Simple SimpleMusic                        -- ^ A single chord.
    | Simultaneous [Music]                      -- ^ Parallel composition.
    | Sequential   [Music]                      -- ^ Sequential composition.
    | Repeat String Int Music (Maybe Music)     -- ^ Repetition (text, times, music, alt).
    | Transpose Interval Music                  -- ^ Transpose music
    | Times Rational Music                      -- ^ Stretch music
    | Relative Pitch Music                      -- ^ Use relative pitch
    | Clef Clef                                 -- ^ 
    | KeySignature Key                          -- ^
    | TimeSignature Int Int                     -- ^ 
    | Breathe BreathingSign                     -- ^ Breath mark (cesura)
    | MetronomeMark (Maybe String) Dur Int Int  -- ^ Metronome mark (text, duration, dots, bpm).
    | TempoMark String                          -- ^ Tempo mark.
    deriving (Eq, Show)

instance Pretty Music where
    pretty (Simple x)               = error "Not implemented"
    pretty (Simultaneous xs)        = error "Not implemented"
    pretty (Sequential xs)          = error "Not implemented"
    pretty (Repeat text times x y)  = error "Not implemented"
    pretty (Transpose intv x)       = error "Not implemented"
    pretty (Times rat x)            = error "Not implemented"
    pretty (Relative pitch x)       = error "Not implemented"
    pretty _                        = error "Not implemented"


    -- | Slur Bool                                 -- ^ Begin or end slur
    -- | Phrase Bool                               -- ^ Begin or end phrase slur

data SimpleMusic
    = Note Note (Maybe Dur) [PostEvent]
    | Chord [Note] (Maybe Dur) [PostEvent]
    deriving (Eq, Show)

instance Pretty SimpleMusic where
    pretty (Note n d p)   = string "NOTE"
    pretty (Chord ns d p) = string "CHORD"

data Note
    = NotePitch [Exclamation] [Question] (Maybe OctaveCheck)
    | NoteDrumPitch (Maybe Dur)
    deriving (Eq, Show)
    -- TODO lyrics 

instance Pretty Note where
    pretty x = string "NOTE"

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
                                           
-- data ChangeHead
--     = NoteMode
--     | DrumMode
--     | FigureMdoe
--     | ChordMode
--     | LyricMode


