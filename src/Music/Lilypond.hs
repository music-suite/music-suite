
{-# LANGUAGE 
    OverloadedStrings, 
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    FlexibleInstances,
    TypeFamilies,
    ScopedTypeVariables #-}

module Music.Lilypond (

        -- * Representation
        -- ** Music expressions
        Music(..),
        Note(..),
        Clef(..),
        Mode(..),

        -- ** Articulation and dynamics
        PostEvent(..),

        -- ** Text
        Articulation(..),
        Markup(..),
        HasMarkup(..),

        -- ** Miscellaneous types
        Direction(..),
        OctaveCheck(..),
        BreathingSign(..),

        -- ** Time
        Duration(..),
        -- ** Pitch
        Pitch(..),
        PitchClass(..),
        Accidental(..),
        Octaves(..),

        -- * Constructing Lilypond expresions
        -- ** Notes and rests
        rest,
        note,
        chord,
        
        -- ** Post events
        addPost,
        addText,
        addText',
        addMarkup,
        addMarkup',
        addArticulation,
        addArticulation',

        -- ** Curves and lines
        beginTie,
        beginBeam,
        endBeam,
        beginSlur,
        endSlur,
        beginPhraseSlur,
        endPhraseSlur,
        beginCresc,
        endCresc,
        beginDim,
        endDim,

        -- ** Marks
        addAccent,
        addMarcato,
        addStaccatissimo,
        addEspressivo,
        addStaccato,
        addTenuto,
        addPortato,
        addUpbow,
        addDownbow,
        addFlageolet,
        addThumb,
        addLeftHeel,
        addRightHeel,
        addLeftToe,
        addRightToe,
        addOpen,
        addStopped,
        addTurn,
        addReverseTurn,
        addTrill,
        addPrall,
        addMordent,
        addPrallPrall,
        addPrallMordent,
        addUpPrall,
        addDownPrall,
        addUpMordent,
        addDownMordent,
        addPrallDown,
        addPrallUp,
        addLinePrall,
        addSignumCongruentiae,
        addShortFermata,
        addFermata,
        addLongFermata,
        addVeryLongFermata,
        addSegno,
        addCoda,
        addVarCoda,
    )
where

import Data.Ratio
import Data.String
import Data.Default
import Data.Semigroup
import Text.Pretty hiding (Mode)
import Data.VectorSpace
import Music.Lilypond.Pitch
import Music.Pitch.Literal

import System.Process -- TODO debug

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

-- | A Lilypond music expression.
--   
--   Use the 'Pretty' instance to convert into Lilypond syntax.
--   
data Music    
    = Rest (Maybe Duration) [PostEvent]         -- ^ Single rest.
    | Note Note (Maybe Duration) [PostEvent]    -- ^ Single note.
    | Chord [Note] (Maybe Duration) [PostEvent] -- ^ Single chord.
    | Sequential   [Music]                      -- ^ Sequential composition.
    | Simultaneous Bool [Music]                 -- ^ Parallel composition (split voices?).
    | Repeat Bool Int Music (Maybe Music)       -- ^ Repetition (unfold?, times, music, alternative).
    | Tremolo Int Music                         -- ^ Tremolo (multiplier).
    | Times Rational Music                      -- ^ Stretch music (multiplier).
    | Transpose Pitch Pitch Music               -- ^ Transpose music (from to).
    | Relative Pitch Music                      -- ^ Use relative octave (octave).
    | Clef Clef                                 -- ^ Clef.
    | Key Pitch Mode                            -- ^ Key signature.
    | Time Rational                             -- ^ Time signature.
    | Breathe (Maybe BreathingSign)             -- ^ Breath mark (caesura)
    | Metronome (Maybe String) Duration Int Int -- ^ Metronome mark (text, duration, dots, bpm).
    | Tempo String                              -- ^ Tempo mark.
    deriving (Eq, Show)

-- TODO tremolo

instance Pretty Music where
    pretty (Rest d p)       = "r" <> pretty d <> prettyList p

    pretty (Note n d p)     = pretty n <> pretty d <> prettyList p

    pretty (Chord ns d p)   = "<" <> nest 4 (sepByS "" $ map pretty ns) <> char '>' 
                                  <> pretty d <> prettyList p

    pretty (Sequential xs)  = "{" <=> nest 4 ((hsep . fmap pretty) xs) <=> "}"

    pretty (Simultaneous False xs) = "<<" <//> nest 4 ((vcat . fmap pretty) xs)           <//> ">>"
    pretty (Simultaneous True xs)  = "<<" <//> nest 4 ((sepByS " \\\\" . fmap pretty) xs) <//> ">>"

    pretty (Repeat unfold times x y) = 
        "\\repeat" <=> unf unfold <=> int times <=> pretty x <=> alt y
        where 
            unf p = if p then "unfold" else "volta"
            alt Nothing  = empty
            alt (Just x) = "\\alternative" <> pretty x

    pretty (Tremolo n x) =
        "\\repeat tremolo" <+> pretty n <=> pretty x

    pretty (Times n x) = 
        "\\times" <+> frac n <=> pretty x
        where
            frac n = pretty (numerator n) <> "/" <> pretty (denominator n)

    pretty (Transpose from to x) =
        "\\transpose" <+> pretty from <=> pretty to <=> pretty x

    pretty (Relative p x) =
        "\\relative" <=> pretty p <=> pretty x

    pretty (Clef c) = "\\clef" <+> pretty c

    pretty (Key p m) = "\\clef" <+> pretty p <+> pretty m
    
    pretty (Time n) = "\\time" <+> pretty n
    
    pretty (Breathe Nothing) = "\\breathe"
    pretty (Breathe a)       = notImpl "Non-standard breath marks"

    pretty _                        = notImpl "Unknown music expression"

    prettyList                      = hsep . fmap pretty


data Note
    = NotePitch Pitch (Maybe OctaveCheck)
    | DrumNotePitch (Maybe Duration)
    deriving (Eq, Show)

instance Pretty Note where
    pretty (NotePitch p Nothing)   = pretty p
    pretty (NotePitch p _)         = notImpl "Non-standard pitch"
    pretty (DrumNotePitch _)       = notImpl "Non-standard pitch"
    prettyList                     = hsep . fmap pretty

instance Pretty Pitch where
    pretty (Pitch (c,a,o)) = string $ pc c ++ acc a ++ oct (o-4)
        where
            pc C = "c" ; pc D = "d" ; pc E = "e" ; pc F = "f"
            pc G = "g" ; pc A = "a" ; pc B = "b"            
            acc n | n <  0  =  concat $ replicate (negate n) "es"
                  | n == 0  =  ""
                  | n >  0  =  concat $ replicate (n) "is"
            oct n | n <  0  =  concat $ replicate (negate n) ","
                  | n == 0  =  ""
                  | n >  0  =  concat $ replicate n "'"

instance IsPitch Music where
    fromPitch = (\p -> Note p (Just (1/4)) []) . fromPitch

instance IsPitch Note where
    fromPitch = (\p -> (NotePitch p Nothing)) . fromPitch

instance IsPitch Pitch where
    fromPitch (PitchL (c, Nothing, o)) = Pitch (toEnum c, 0,       o)                 
    fromPitch (PitchL (c, Just a, o))  = Pitch (toEnum c, round a, o)

instance AdditiveGroup Music where
    zeroV   = Rest (Just $ 1/4) []
    a ^+^ b = Sequential [a,b]
    negateV = error "No Music.Lilypond.Music.negateV"

instance VectorSpace Music where
    type Scalar Music = Duration
    a *^ (Rest  (Just d) p)     = Rest (Just $ a*d) p
    a *^ (Note  n (Just d) p)   = Note n (Just $ a*d) p
    a *^ (Chord ns (Just d) p)  = Chord ns (Just $ a*d) p
    a *^ x                      = x

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

instance Pretty Clef where
    pretty Treble       = "treble"
    pretty Alto         = "alto"
    pretty Tenor        = "tenor"
    pretty Bass         = "bass"
    pretty French       = "french"
    pretty Soprano      = "soprano"
    pretty MezzoSoprano = "mezzosoprano"
    pretty Baritone     = "baritone"
    pretty VarBaritone  = "varbaritone"
    pretty SubBass      = "subbass"
    pretty Percussion   = "percussion"
    pretty Tab          = "tab"

data Mode = Major | Minor
    deriving (Eq, Show)

instance Pretty Mode where
    pretty Major = "\\major"
    pretty Minor = "\\minor"

data BreathingSign
    = RightVarComma
    | StraightCaesura
    | CurvedCaesura
    deriving (Eq, Show)

data PostEvent
    = Articulation Direction Articulation
    | Tie
    | BeginBeam
    | EndBeam
    | BeginSlur
    | EndSlur
    | BeginPhraseSlur
    | EndPhraseSlur
    | BeginCresc
    | BeginDim
    | EndCrescDim
    | Text Direction String
    | Markup Direction Markup
    deriving (Eq, Show)

instance Pretty PostEvent where 
    pretty (Articulation d a)   = pretty d <> pretty a
    pretty Tie                  = "~"
    pretty BeginBeam            = "["
    pretty EndBeam              = "]"
    pretty BeginSlur            = "("
    pretty EndSlur              = ")"
    pretty BeginPhraseSlur      = "\\("
    pretty EndPhraseSlur        = "\\)"
    pretty BeginCresc           = "\\<"
    pretty BeginDim             = "\\>"
    pretty EndCrescDim          = "\\!"
    pretty (Text d s)           = pretty d <> (string . show) s -- add quotes
    pretty (Markup d m)         = pretty d <> ("\\markup" <+> pretty m)
    prettyList                  = hcat . fmap pretty

data Markup
    = MarkupText String
    | MarkupList [Markup]
    | Bold Markup
    | Box Markup
    | Caps Markup
    | DynamicsFont Markup
    | FingeringFont Markup
    | Fontsize Double Markup
    | Huge Markup
    | Italic Markup
    | Large Markup
    | Larger Markup
    | Magnify Markup
    | Medium Markup
    | Roman Markup
    | Sans Markup
    | Sub Markup
    | Super Markup
    | TextFont Markup
    | Tiny Markup
    | TypewriterFont Markup
    | Upright Markup
    deriving (Eq, Show)

class HasMarkup a where
    markup :: a -> Markup

instance HasMarkup Markup where
    markup = id
instance HasMarkup a => HasMarkup [a] where
    markup = MarkupList . fmap markup
instance IsString Markup where
    fromString = MarkupText
    
instance Pretty Markup where
    pretty (MarkupText s)       = (string . show) s 
    pretty (MarkupList as)      = "{" <+> hsep (fmap pretty as) <+> "}"
    pretty (Bold a)             = "\\bold" <+> pretty a
    pretty (Box a)              = "\\box" <+> pretty a
    pretty (Caps a)             = "\\caps" <+> pretty a
    pretty (DynamicsFont a)     = "\\dynamics" <+> pretty a
    pretty (FingeringFont a)    = "\\fingering" <+> pretty a
    pretty (Fontsize n a)       = "\\fontsize" <+> ("#" <> pretty n) <+> pretty a
    pretty (Huge a)             = "\\huge" <+> pretty a
    pretty (Italic a)           = "\\italic" <+> pretty a
    pretty (Large a)            = "\\large" <+> pretty a
    pretty (Larger a)           = "\\larger" <+> pretty a
    pretty (Magnify a)          = "\\magnify" <+> pretty a
    pretty (Medium a)           = "\\medium" <+> pretty a
    pretty (Roman a)            = "\\roman" <+> pretty a
    pretty (Sans a)             = "\\sans" <+> pretty a
    pretty (Sub a)              = "\\sub" <+> pretty a
    pretty (Super a)            = "\\super" <+> pretty a
    pretty (TextFont a)         = "\\text" <+> pretty a
    pretty (Tiny a)             = "\\tiny" <+> pretty a
    pretty (TypewriterFont a)   = "\\typewriter" <+> pretty a
    pretty (Upright a)          = "\\upright" <+> pretty a


-- | Articulations. These include ornaments.
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
    | SignumCongruentiae
    | ShortFermata
    | Fermata
    | LongFermata
    | VeryLongFermata
    | Segno
    | Coda
    | VarCoda
    deriving (Eq, Show)

instance Pretty Articulation where 
    -- pretty Accent             = "\\accent"
    -- pretty Marcato            = "\\marcato"
    -- pretty Staccatissimo      = "\\staccatissimo"
    pretty Accent             = ">"
    pretty Marcato            = "^"
    pretty Staccatissimo      = "|"
    pretty Espressivo         = "\\espressivo"
    -- pretty Staccato           = "\\staccato"
    -- pretty Tenuto             = "\\tenuto"
    -- pretty Portato            = "\\portato"
    pretty Staccato           = "."
    pretty Tenuto             = "-"
    pretty Portato            = "_"
    pretty Upbow              = "\\upbow"
    pretty Downbow            = "\\downbow"
    pretty Flageolet          = "\\flageolet"
    pretty Thumb              = "\\thumb"
    pretty LeftHeel           = "\\leftheel"
    pretty RightHeel          = "\\rightheel"
    pretty LeftToe            = "\\lefttoe"
    pretty RightToe           = "\\righttoe"
    pretty Open               = "\\open"
    -- pretty Stopped            = "\\stopped"
    pretty Stopped            = "+"
    pretty Turn               = "\\turn"
    pretty ReverseTurn        = "\\reverseturn"
    pretty Trill              = "\\trill"
    pretty Prall              = "\\prall"
    pretty Mordent            = "\\mordent"
    pretty PrallPrall         = "\\prallprall"
    pretty PrallMordent       = "\\prallmordent"
    pretty UpPrall            = "\\upprall"
    pretty DownPrall          = "\\downprall"
    pretty UpMordent          = "\\upmordent"
    pretty DownMordent        = "\\downmordent"
    pretty PrallDown          = "\\pralldown"
    pretty PrallUp            = "\\prallup"
    pretty LinePrall          = "\\lineprall"
    pretty SignumCongruentiae = "\\signumCongruentiae"
    pretty ShortFermata       = "\\shortfermata"
    pretty Fermata            = "\\fermata"
    pretty LongFermata        = "\\longfermata"
    pretty VeryLongFermata    = "\\verylongfermata"
    pretty Segno              = "\\segno"
    pretty Coda               = "\\coda"
    pretty VarCoda            = "\\varcoda"
    prettyList              = hcat . fmap pretty

data Direction
    = Above
    | Default
    | Below
    deriving (Eq, Ord, Show)

instance Default Direction where
    def = Default
    
instance Pretty Direction where
    pretty Above              = "^"
    pretty Default            = "-"
    pretty Below              = "_"

data OctaveCheck = OctaveCheck
    deriving (Eq, Show)

-- data ChangeHead
--     = NoteMode
--     | DrumMode
--     | FigureMdoe
--     | ChordMode
--     | LyricMode


-- | Notated time in fractions, in @[2^^i | i <- [-10..3]]@.
newtype Duration   = Duration { getDuration :: Rational }

deriving instance Eq            Duration
deriving instance Ord           Duration
deriving instance Num           Duration
deriving instance Enum          Duration
deriving instance Fractional    Duration
deriving instance Real          Duration
deriving instance RealFrac      Duration
deriving instance Show          Duration

instance Pretty Duration where
    pretty a = string $ pnv (toRational nv) ++ pds ds
        where
            pnv 4 = "\\longa"
            pnv 2 = "\\breve"
            pnv n = show (denominator n)             
            pds n = concat $ replicate n "."
            (nv, ds) = separateDots a


rest :: Music
rest = Rest (Just $ 1/4) []

note :: Note -> Music
note n = Note n (Just $ 1/4) []

chord :: [Note] -> Music
chord ns = Chord ns (Just $ 1/4) []

addPost :: PostEvent -> Music -> Music
addPost a (Rest d es)     = Rest d (es ++ [a])
addPost a (Note n d es)   = Note n d (es ++ [a])
addPost a (Chord ns d es) = Chord ns d (es ++ [a])
addPost a m               = m

addText :: String -> Music -> Music
addText s = addPost (Text def s)

addText' :: Direction -> String -> Music -> Music
addText' d s = addPost (Text d s)

addMarkup :: HasMarkup a => a -> Music -> Music
addMarkup s = addPost (Markup def (markup s))

addMarkup' :: HasMarkup a => Direction -> a -> Music -> Music
addMarkup' d s = addPost (Markup d (markup s))

addArticulation :: Articulation -> Music -> Music
addArticulation a = addPost (Articulation def a)

addArticulation' :: Direction -> Articulation -> Music -> Music
addArticulation' d a = addPost (Articulation d a)

beginTie :: Music -> Music
beginTie = addPost Tie

beginBeam :: Music -> Music
beginBeam = addPost BeginBeam

endBeam :: Music -> Music
endBeam = addPost EndBeam

beginSlur :: Music -> Music
beginSlur = addPost BeginSlur

endSlur :: Music -> Music
endSlur = addPost EndSlur

beginPhraseSlur :: Music -> Music
beginPhraseSlur = addPost BeginPhraseSlur

endPhraseSlur :: Music -> Music
endPhraseSlur = addPost EndPhraseSlur

beginCresc :: Music -> Music
beginCresc = addPost BeginCresc

endCresc :: Music -> Music
endCresc = addPost EndCrescDim

beginDim :: Music -> Music
beginDim = addPost BeginDim

endDim :: Music -> Music
endDim = addPost EndCrescDim


addAccent :: Music -> Music
addAccent = addArticulation Accent

addMarcato :: Music -> Music
addMarcato = addArticulation Marcato

addStaccatissimo :: Music -> Music
addStaccatissimo = addArticulation Staccatissimo

addEspressivo :: Music -> Music
addEspressivo = addArticulation Espressivo

addStaccato :: Music -> Music
addStaccato = addArticulation Staccato

addTenuto :: Music -> Music
addTenuto = addArticulation Tenuto

addPortato :: Music -> Music
addPortato = addArticulation Portato

addUpbow :: Music -> Music
addUpbow = addArticulation Upbow

addDownbow :: Music -> Music
addDownbow = addArticulation Downbow

addFlageolet :: Music -> Music
addFlageolet = addArticulation Flageolet

addThumb :: Music -> Music
addThumb = addArticulation Thumb

addLeftHeel :: Music -> Music
addLeftHeel = addArticulation LeftHeel

addRightHeel :: Music -> Music
addRightHeel = addArticulation RightHeel

addLeftToe :: Music -> Music
addLeftToe = addArticulation LeftToe

addRightToe :: Music -> Music
addRightToe = addArticulation RightToe

addOpen :: Music -> Music
addOpen = addArticulation Open

addStopped :: Music -> Music
addStopped = addArticulation Stopped

addTurn :: Music -> Music
addTurn = addArticulation Turn

addReverseTurn :: Music -> Music
addReverseTurn = addArticulation ReverseTurn

addTrill :: Music -> Music
addTrill = addArticulation Trill

addPrall :: Music -> Music
addPrall = addArticulation Prall

addMordent :: Music -> Music
addMordent = addArticulation Mordent

addPrallPrall :: Music -> Music
addPrallPrall = addArticulation PrallPrall

addPrallMordent :: Music -> Music
addPrallMordent = addArticulation PrallMordent

addUpPrall :: Music -> Music
addUpPrall = addArticulation UpPrall

addDownPrall :: Music -> Music
addDownPrall = addArticulation DownPrall

addUpMordent :: Music -> Music
addUpMordent = addArticulation UpMordent

addDownMordent :: Music -> Music
addDownMordent = addArticulation DownMordent

addPrallDown :: Music -> Music
addPrallDown = addArticulation PrallDown

addPrallUp :: Music -> Music
addPrallUp = addArticulation PrallUp

addLinePrall :: Music -> Music
addLinePrall = addArticulation LinePrall

addSignumCongruentiae :: Music -> Music
addSignumCongruentiae = addArticulation SignumCongruentiae

addShortFermata :: Music -> Music
addShortFermata = addArticulation ShortFermata

addFermata :: Music -> Music
addFermata = addArticulation Fermata

addLongFermata :: Music -> Music
addLongFermata = addArticulation LongFermata

addVeryLongFermata :: Music -> Music
addVeryLongFermata = addArticulation VeryLongFermata

addSegno :: Music -> Music
addSegno = addArticulation Segno

addCoda :: Music -> Music
addCoda = addArticulation Coda

addVarCoda :: Music -> Music
addVarCoda = addArticulation VarCoda










notImpl a = error $ "Not implemented: " ++ a
asPitch = id
asPitch :: Pitch -> Pitch











separateDots :: Duration -> (Duration, Int)
separateDots = separateDots' [2/3, 6/7, 14/15, 30/31, 62/63]

separateDots' :: [Duration] -> Duration -> (Duration, Int)
separateDots' []         nv = error "separateDots: Strange"
separateDots' (div:divs) nv 
    | isDivisibleBy 2 nv = (nv,  0)
    | otherwise          = (nv', dots' + 1)
    where                                                        
        (nv', dots')    = separateDots' divs (nv*div)

logBaseR :: forall a . (RealFloat a, Floating a) => Rational -> Rational -> a
logBaseR k n 
    | isInfinite (fromRational n :: a)      = logBaseR k (n/k) + 1
logBaseR k n 
    | isDenormalized (fromRational n :: a)  = logBaseR k (n*k) - 1
logBaseR k n                         = logBase (fromRational k) (fromRational n)

isDivisibleBy :: (Real a, Real b) => a -> b -> Bool
isDivisibleBy n = (equalTo 0.0) . snd . properFraction . logBaseR (toRational n) . toRational

equalTo  = (==)

infixl <=>
a <=> b = sep [a,b]



-- TODO debug

runLy = runCommand "lilypond -f pdf test.ly"

engrave :: Music -> IO ()
engrave e = do
    writeFile "test.ly" $ show $ pretty e
    runLy
    return ()


main = engrave test

test =
    Simultaneous False 
        [ Relative g' (Sequential [
            addMarkup ([Bold "Hello", Italic (markup [MarkupText "cruel", Bold $ MarkupText "world"])]) rest,
            addArticulation Mordent $ chord [c,e,g]^*2,
            d^*1,
            e^*2,
            c^*(3/2),
            fs^*(1/2)
            ])
        , Sequential [Tremolo 4 (Sequential [c^/4,d^/4]), Tremolo 4 (Sequential [c^/4,d^/4])]
        , Sequential [rest,c^*2,d^*1,e^*2,c^*(3/2),fs^*(1/2)]
        , Sequential [rest,c^*2,d^*1,e^*2,c^*(3/2),fs^*(1/2)]
        , Relative g (Sequential [rest,c^*2,d^*1,e^*2,c^*(3/2),fs^*(1/2)])
        , Sequential 
            [ Times (4/5) (Sequential 
                [
                    rest,
                    addArticulation Accent $ addPost BeginSlur $ addPost BeginCresc $ c^*2,
                    d^*1,
                    addPost Tie $ e^*1
                ])
            , Times (4/5) (Sequential 
                [      
                    addPost BeginDim $ addPost EndCrescDim $ e^*1,
                    c^*(3/2),
                    addPost EndSlur $ fs^*(1/2),
                    addPost EndCrescDim $ c^*2
                ])
            ]
        ]
        
        
    
