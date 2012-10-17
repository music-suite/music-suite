
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Music.Lilypond -- (
  -- )
where

import Data.Ratio
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

list :: b -> (a -> [a] -> b) -> [a] -> b
list z f [] = z
list z f (x:xs) = x `f` xs

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

instance Pretty a => Pretty (Maybe a) where
    pretty = maybe empty pretty

instance (Pretty a, Integral a) => Pretty (Ratio a) where
    pretty x = pretty (numerator x) <> char '/' <> pretty (denominator x)

string = text

-- |
-- Join with separator.
--
-- > sepBy q [x1,x2..xn] = x1 <> q <> x2 <> q .. xn.
sepBy :: Doc -> [Doc] -> Doc

-- |
-- Join with initiator.
--
-- > initBy q [x1,x2..xn] = q <> x1 <> q <> x2 <> q .. xn.
initBy :: Doc -> [Doc] -> Doc

-- |
-- Join with terminator.
--
-- > termBy q [x1,x2..xn] = x1 <> q <> x2 <> q .. xn <> q.
termBy :: Doc -> [Doc] -> Doc
sepBy  p = list empty $ \x -> (x <>) . initBy p
initBy p = hcat . map (p <>)
termBy p = hcat . map (<> p)


-- |
-- Join with separator followed by space.
--
-- > sepByS q [x1,x2..xn] = x1 <> q <+> x2 <> q <+>.. xn.
sepByS :: Doc -> [Doc] -> Doc

-- |
-- Join with initiator followed by space.
--
-- > initByS q [x1,x2..xn] = q <+> x1 <> q <+> x2 <> q <+> .. xn.
initByS :: Doc -> [Doc] -> Doc

-- |
-- Join with terminator followed by space.
--
-- > termByS q [x1,x2..xn] = x1 <> q <+> x2 <> q <+> .. xn <> q.
termByS :: Doc -> [Doc] -> Doc
sepByS  p = list empty $ \x -> (x <>) . initByS p
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
    pretty (Simple x)               = pretty x

    pretty (Sequential xs)          = string "{" <+> pconcat xs <+> string "}"

    pretty (Simultaneous xs)        = string "<<" <+> pconcat xs <+> string ">>"

    pretty (Repeat unfold times x y)  = string "\\repeat" <+> unf unfold <+> int times <+> pretty x <+> alt y
        where 
            unf p = if p then string "unfold" else string "volta"
            alt Nothing  = empty
            alt (Just x) = string "\\alternative" <> pretty x

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
    pretty (Note n d p)   = pretty n <> pretty d <> pretty p
    pretty (Chord ns d p) = char '<' <> (sepByS (char 'x') $ map pretty ns) <> char '>' <> pretty d <> pretty p

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

instance Pretty PostEvent where pretty = error "PostEvent"
                                           
-- data ChangeHead
--     = NoteMode
--     | DrumMode
--     | FigureMdoe
--     | ChordMode
--     | LyricMode


