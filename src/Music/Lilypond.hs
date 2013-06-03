
{-# LANGUAGE 
    OverloadedStrings, 
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    TypeFamilies,
    ScopedTypeVariables #-}

module Music.Lilypond (
        Music(..),
        Note(..),
        Clef(..),
        KeyMode(..),
        Key(..),
        BreathingSign(..),
        Articulation(..),
        OctaveCheck(..),
        PostEvent(..),
        Duration(..),
        Pitch(..),
        PitchClass(..),
        Accidental(..),
        Octaves(..)
    )
where

import Data.Ratio
import Data.Semigroup
import Text.Pretty
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

data Music    
    = Rest (Maybe Duration) [PostEvent]         -- ^ A single rest.
    | Note Note (Maybe Duration) [PostEvent]    -- ^ A single note.
    | Chord [Note] (Maybe Duration) [PostEvent] -- ^ A single chord.
    | Sequential   [Music]                      -- ^ Sequential composition.
    | Simultaneous Bool [Music]                 -- ^ Parallel composition (split voices?).
    | Repeat Bool Int Music (Maybe Music)       -- ^ Repetition (unfold, times, music, alt).
    | Transpose Pitch Pitch Music               -- ^ Transpose music
    | Times Rational Music                      -- ^ Stretch music
    | Relative Pitch Music                      -- ^ Use relative pitch
    
    | Clef Clef                                 -- ^ 
    | KeySignature Key                          -- ^
    | TimeSignature Int Int                     -- ^ 
    | Breathe BreathingSign                     -- ^ Breath mark (caesura)
    | MetronomeMark (Maybe String) Duration Int Int  -- ^ Metronome mark (text, duration, dots, bpm).
    | TempoMark String                          -- ^ Tempo mark.
    deriving (Eq, Show)

-- TODO tremolo

instance Pretty Music where
    pretty (Rest d p)       = "r" <> pretty d{- <> pretty p-}

    pretty (Note n d p)     = pretty n <> pretty d{- <> pretty p-}

    pretty (Chord ns d p)   = "<" <> nest 4 (sepByS "" $ map pretty ns) <> char '>' <> pretty d{- <> pretty p-}

    pretty (Sequential xs)  = "{" <=> nest 4 ((hsep . fmap pretty) xs) <=> "}"

    pretty (Simultaneous False xs) = "<<" <//> nest 4 ((vcat . fmap pretty) xs)           <//> ">>"
    pretty (Simultaneous True xs)  = "<<" <//> nest 4 ((sepByS " \\\\" . fmap pretty) xs) <//> ">>"

    pretty (Repeat unfold times x y) = 
        "\\repeat" <=> unf unfold <=> int times <=> pretty x <=> alt y
        where 
            unf p = if p then "unfold" else "volta"
            alt Nothing  = empty
            alt (Just x) = "\\alternative" <> pretty x

    pretty (Transpose from to x) = notImpl
        "\\transpose" <+> pretty from <=> pretty to <=> pretty x

    pretty (Times n x) = 
        "\\times" <+> frac n <=> pretty x
        where
            frac n = pretty (numerator n) <> "/" <> pretty (denominator n)

    pretty (Relative p x) =
        "\\relative" <=> pretty p <=> pretty x

    pretty _                        = notImpl

    prettyList                      = hsep . fmap pretty


    -- | Slur Bool                                 -- ^ Begin or end slur
    -- | Phrase Bool                               -- ^ Begin or end phrase slur

data Note
    = NotePitch Pitch (Maybe OctaveCheck)
    | DrumNotePitch (Maybe Duration)
    deriving (Eq, Show)
    -- TODO lyrics 

instance Pretty Note where
    pretty (NotePitch p Nothing)   = pretty p
    pretty (NotePitch p _)         = notImpl
    pretty (DrumNotePitch _)       = notImpl
    prettyList                     = hsep . fmap pretty

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
    a *^ (Rest  (Just d) p)    = Rest (Just $ a*d) p
    a *^ (Note  n (Just d) p)  = Note n (Just $ a*d) p
    a *^ (Chord ns (Just d) p) = Chord ns (Just $ a*d) p
    a *^ x                     = x

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














notImpl = error "Not implemented"
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

runLy = runCommand "lilypond -f png test.ly"

engrave :: Music -> IO ()
engrave e = do
    writeFile "test.ly" $ show $ pretty e
    runLy
    return ()

rest = Rest (Just $ 1/4) []
chord ns = Chord ns (Just $ 1/4) []


main = engrave $ 
    Simultaneous False 
        [ Relative g' (Sequential [rest,chord [c,e,g]^*2,d^*1,e^*2,c^*(3/2),fs^*(1/2)])
        , Sequential [rest,c^*2,d^*1,e^*2,c^*(3/2),fs^*(1/2)]
        , Sequential [rest,c^*2,d^*1,e^*2,c^*(3/2),fs^*(1/2)]
        , Sequential [rest,c^*2,d^*1,e^*2,c^*(3/2),fs^*(1/2)]
        , Simultaneous False 
            [ Relative g (Sequential [rest,c^*2,d^*1,e^*2,c^*(3/2),fs^*(1/2)])
            , Times (4/5) (Sequential [rest,c^*2,d^*1,e^*2,c^*(3/2),fs^*(1/2)])
            ]
        ]
        
        
    
