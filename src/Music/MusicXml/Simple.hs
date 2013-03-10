
{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

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
-- Provides smart constructors for the MusicXML representation.
--

-------------------------------------------------------------------------------------

module Music.MusicXml.Simple (

        -----------------------------------------------------------------------------
        -- * Score and parts
        -----------------------------------------------------------------------------

        -- ** Basic constructors
        fromPart,
        fromParts,

        -- ** Part lists
        partList,
        partListAbbr,
        bracket,
        brace,
               
        -- ** Measures
        measure,
        bar,

        -- -- ** Others
        -- partIds,
        -- header,
        -- setHeader,
        -- setTitle,
        -- setMvmTitle,

        -----------------------------------------------------------------------------
        -- * Top-level attributes
        -----------------------------------------------------------------------------

        -- ** Pitch
        trebleClef,
        altoClef,
        bassClef,
        defaultClef,
        clef, 
        defaultKey,
        key,

        -- ** Time
        defaultDivisions,
        divisions,        
        commonTime,
        cutTime,
        time,

        -- ** Tempo
        -- TODO tempo
        metronome,
        
        -----------------------------------------------------------------------------
        -- * Notes
        -----------------------------------------------------------------------------
        
        -- ** Basic constructors
        rest,
        note,
        chord,

        -- ** Voice
        setVoice,

        -- ** Duration
        dot,
        tuplet,
        setNoteVal,
        -- setTimeMod,
        -- beginTuplet,
        -- endTuplet,

        -- ** Beams
        beam,
        beginBeam,
        continueBeam,
        endBeam,

        -- ** Ties
        beginTie,
        endTie,

        -- ** Notations
        addNotation,

        -----------------------------------------------------------------------------
        -- * Pitch transformations
        -----------------------------------------------------------------------------
        
        -- ** Glissando
        beginGliss,
        endGliss,

        -- ** Slides
        beginSlide,
        endSlide,

        -----------------------------------------------------------------------------
        -- * Time transformations
        -----------------------------------------------------------------------------

        -- ** Accelerando and ritardando
        -- TODO accelerando,
        -- TODO ritardando,

        -- ** Fermatas and breaks
        fermata,
        breathMark,
        caesura,

        -----------------------------------------------------------------------------
        -- * Articulation
        -----------------------------------------------------------------------------
        
        -- ** Slurs
        slur,
        beginSlur,
        endSlur,   
        
        -- ** Staccato and tenuto
        staccato,
        tenuto,
        spiccato,
        staccatissimo,

        -- ** Accents
        accent,
        strongAccent,

        -- ** Miscellaneous
        scoop,
        plop,
        doit,
        falloff,
        stress,
        unstress,

        -----------------------------------------------------------------------------
        -- * Dynamics
        -----------------------------------------------------------------------------

        -- ** Crescendo and diminuendo
        cresc,
        dim,
        beginCresc,
        endCresc,
        beginDim,
        endDim,

        -- ** Dynamic levels
        dynamic,

        -- pppppp,
        -- ppppp,
        -- pppp,
        -- ppp,
        -- pp,
        -- p,
        -- mp,
        -- mf,
        -- ff,
        -- fff,
        -- ffff,
        -- fffff,
        -- ffffff, 

        -- ** Both
        crescFrom,
        crescTo,
        crescFromTo,
        dimFrom,
        dimTo,
        dimFromTo,        

        -----------------------------------------------------------------------------
        -- * Ornaments
        -----------------------------------------------------------------------------
        
        tremolo,


        -----------------------------------------------------------------------------
        -- * Text
        -----------------------------------------------------------------------------

        text,
        rehearsal,
        segno,
        coda,

  ) 
where

import Data.Default
import Data.Ratio
import Data.Monoid

import Music.MusicXml.Score
import Music.MusicXml.Time
import Music.MusicXml.Pitch
import Music.MusicXml.Dynamics
import Music.MusicXml.Read
import Music.MusicXml.Write

import qualified Data.List as List

-- ----------------------------------------------------------------------------------
-- Score and parts
-- ----------------------------------------------------------------------------------

-- | 
-- Create a single-part score.
--
-- > fromPart title composer partName measures
--
-- Example:
--
-- @ 'fromPart' \"Suite\" \"Bach\" \"Cello solo\" [] @
--
fromPart :: String -> String -> String -> [Music] -> Score
fromPart title composer partName music = 
    fromParts title composer (partList [partName]) [music]

-- | 
-- Create a multi-part score.
--
-- > fromParts title composer partList parts
--
-- Example:
--
-- @ 'fromParts' \"4'33\" \"Cage\" ('partList' [\"Violin\", \"Viola\", \"Cello\"]) [[]] @
--
fromParts :: String -> String -> PartList -> [[Music]] -> Score
fromParts title composer partList music 
    = Partwise 
        (def)
        (header title composer partList)
        (parts music)  


partIds :: [String]
partIds = [ "P" ++ show n | n <- [1..] ]

-- | 
-- Create a part list from instrument names.
--
partList :: [String] -> PartList
partList = zipWith (\partId name -> Part partId name Nothing) partIds

-- | 
-- Create a part list from instrument names and abbreviations.
--
partListAbbr :: [(String, String)] -> PartList
partListAbbr = zipWith (\partId (name,abbr) -> Part partId name (Just abbr)) partIds

-- | 
-- Enclose the given parts in a bracket.
-- 
bracket :: PartList -> PartList
bracket ps = mempty
        <> [Group 1 Start "" Nothing (Just GroupBracket) (Just GroupBarLines) False] 
        <> ps 
        <> [Group 1 Stop "" Nothing Nothing Nothing False]

-- | 
-- Enclose the given parts in a brace.
-- 
brace :: PartList -> PartList
brace ps = mempty
        <> [Group 1 Start "" Nothing (Just GroupBrace) (Just GroupBarLines) False] 
        <> ps 
        <> [Group 1 Stop "" Nothing Nothing Nothing False]


-- |
-- Convenient synonym for 'mconcat', allowing us to write things like
--
-- > measure [
-- >    beam [ 
-- >        note c  (1/8), 
-- >        note d  (1/8),
-- >        note e  (1/8),
-- >        note f  (1/8) 
-- >    ],
-- >    tuplet 3 2 [ 
-- >        note g  (1/4),
-- >        note a  (1/4),
-- >        note b  (1/4) 
-- >    ]
-- > ]
-- 
measure :: [Music] -> Music
measure = mconcat

-- |
-- Convenient synonym for 'mconcat'.
-- 
bar :: [Music] -> Music
bar = measure


header :: String -> String -> PartList -> ScoreHeader
header title composer partList = ScoreHeader Nothing (Just title) (Just (Identification [Creator "composer" composer])) partList

setHeader header (Partwise attrs _ music) = Partwise attrs header music
setHeader header (Timewise attrs _ music) = Timewise attrs header music

setTitle    title    (ScoreHeader _ mvmTitle ident partList) = ScoreHeader title mvmTitle ident partList
setMvmTitle mvmTitle (ScoreHeader title _ ident partList) = ScoreHeader title (Just mvmTitle) ident partList
-- addIdent    ident    (ScoreHeader title mvmTitle idents partList) = ScoreHeader title mvmTitle (ident:idents) partList

parts :: [[Music]] -> [(PartAttrs, [(MeasureAttrs, Music)])]
parts = zipWith (\ids mus -> (PartAttrs ids, zipWith (\ids mus -> (MeasureAttrs ids, mus)) barIds mus)) partIds'
    where
        partIds' = partIds
        barIds   = [1..]
                        

-- ----------------------------------------------------------------------------------
-- Top-level attributes
-- ----------------------------------------------------------------------------------

trebleClef, altoClef, bassClef :: Music
trebleClef = clef GClef 2
altoClef   = clef CClef 3
bassClef   = clef FClef 4

defaultClef :: Music
defaultClef = trebleClef

-- |
-- Create a clef.
--
clef :: ClefSign -> Line -> Music
clef symbol line = single $ MusicAttributes $ Clef symbol line

defaultKey :: Music
defaultKey = key 0 Major

-- |
-- Create a key signature.
--
key :: Fifths -> Mode -> Music
key n m = single $ MusicAttributes $ Key n m


-- Number of ticks per whole note (we use 768 per quarter like Sibelius).
defaultDivisionsVal :: Divs      
defaultDivisionsVal = 768 * 4

-- |
-- Set the tick division to the default value.
--
defaultDivisions    :: Music
defaultDivisions    = single $ MusicAttributes $ Divisions $ defaultDivisionsVal `div` 4

-- |
-- Define the number of ticks per quarter note.
--
divisions :: Divs -> Music
divisions n = single $ MusicAttributes $ Divisions $ n

commonTime, cutTime :: Music
commonTime = single $ MusicAttributes $ Time CommonTime
cutTime    = single $ MusicAttributes $ Time CutTime

-- |
-- Create a time signature.
--
time :: Beat -> BeatType -> Music
time a b   = single $ MusicAttributes $ Time $ DivTime a b

-- |
-- Create a metronome mark.
--
metronome :: NoteVal -> Bool -> Tempo -> Music
metronome nv dot tempo = single $ MusicDirection (Metronome nv dot tempo)

-- TODO tempo


-- ----------------------------------------------------------------------------------
-- Notes
-- ----------------------------------------------------------------------------------

-- |
-- Create a rest.
--
-- > rest (1/4)
-- > rest (3/8)
-- > rest quarter
-- > rest (dotted eight)
--
rest :: NoteVal -> Music
rest dur = single $ MusicNote (Note def (defaultDivisionsVal `div` denom) noTies (setNoteValP val def))
    where
        num   = fromIntegral $ numerator   $ toRational $ dur
        denom = fromIntegral $ denominator $ toRational $ dur
        val   = NoteVal $ toRational $ dur              

-- |
-- Create a single note.
--
-- > note c   (1/4)
-- > note fs_ (3/8)
-- > note c   quarter
-- > note (c + pure fifth) (dotted eight)
--
note :: Pitch -> NoteVal -> Music
note pitch dur = note' False pitch dur' dots
    where
        (dur', dots) = separateDots dur

-- |
-- Create a chord.
-- 
-- > chord [c,eb,fs_] (3/8)
-- > chord [c,d,e] quarter
-- > chord [c,d,e] (dotted eight)
--
chord :: [Pitch] -> NoteVal -> Music
chord [] d      = rest d
chord (p:ps) d  = note p d ++ concatMap (\p -> chordNote p d) ps


chordNote :: Pitch -> NoteVal -> Music
chordNote pitch dur = note' True pitch dur' dots
    where
        (dur', dots) = separateDots dur

note' :: Bool -> Pitch -> NoteVal -> Int -> Music
note' isChord pitch dur dots 
    = single $ MusicNote $ 
        Note 
            (Pitched isChord $ pitch) 
            (defaultDivisionsVal `div` denom) 
            noTies 
            (setNoteValP val $ addDots $ def)
    where                    
        addDots = foldl (.) id (replicate dots dotP)
        num     = fromIntegral $ numerator   $ toRational $ dur
        denom   = fromIntegral $ denominator $ toRational $ dur
        val     = NoteVal $ toRational $ dur              

separateDots :: NoteVal -> (NoteVal, Int)
separateDots = separateDots' [2/3, 6/7, 14/15, 30/31, 62/63]

separateDots' :: [NoteVal] -> NoteVal -> (NoteVal, Int)
separateDots' []         nv = errorNoteValue
separateDots' (div:divs) nv 
    | isDivisibleBy 2 nv = (nv,  0)
    | otherwise          = (nv', dots' + 1)
    where                                                        
        (nv', dots')    = separateDots' divs (nv*div)

errorNoteValue  = error "Note value must be a multiple of two or dotted"



setVoice        :: Int -> Music -> Music
setVoice n      = fmap $ mapNoteProps2 (setVoiceP n)

dot             :: Music -> Music
setNoteVal      :: NoteVal -> Music -> Music
setTimeMod      :: Int -> Int -> Music -> Music
dot             = fmap $ mapNoteProps2 dotP
setNoteVal x    = fmap $ mapNoteProps2 (setNoteValP x)
setTimeMod m n  = fmap $ mapNoteProps2 (setTimeModP m n)

addNotation  :: Notation -> Music -> Music
addNotation x = fmap $ mapNoteProps2 (addNotationP x)

-- TODO clean up, skip empty notation groups etc
mergeNotations :: [Notation] -> [Notation]
mergeNotations notations = mempty
    <> [foldOrnaments ornaments] 
    <> [foldTechnical technical] 
    <> [foldArticulations articulations]
    <> others
    where
        (ornaments,notations')  = List.partition isOrnaments notations
        (technical,notations'') = List.partition isTechnical notations'
        (articulations,others)  = List.partition isArticulations notations'

        isOrnaments (Ornaments _)         = True
        isOrnaments _                     = False
        isTechnical (Technical _)         = True
        isTechnical _                     = False
        isArticulations (Articulations _) = True
        isArticulations _                 = False
        
        (Ornaments xs) `mergeN` (Ornaments ys)         = Ornaments (xs <> ys)
        (Technical xs) `mergeN` (Technical ys)         = Technical (xs <> ys)
        (Articulations xs) `mergeN` (Articulations ys) = Articulations (xs <> ys)
        foldOrnaments     = foldr mergeN (Ornaments [])
        foldTechnical     = foldr mergeN (Technical [])
        foldArticulations = foldr mergeN (Articulations [])


beginTuplet     :: Music -> Music
endTuplet       :: Music -> Music
beginTuplet     = addNotation (Tuplet 1 Start)
endTuplet       = addNotation (Tuplet 1 Stop)

beginBeam       :: Music -> Music
continueBeam    :: Music -> Music
endBeam         :: Music -> Music
beginBeam       = fmap $ mapNoteProps2 (beginBeamP 1)
continueBeam    = fmap $ mapNoteProps2 (continueBeamP 1)
endBeam         = fmap $ mapNoteProps2 (endBeamP 1)

beginTie' = fmap beginTie''
endTie'   = fmap endTie''
beginTie'' (MusicNote (Note full dur ties props)) = (MusicNote (Note full dur (ties++[Start]) props))
endTie''   (MusicNote (Note full dur ties props)) = (MusicNote (Note full dur ([Stop]++ties) props))

beginTie    :: Music -> Music
endTie      :: Music -> Music
beginTie        = beginTie' . addNotation (Tied Start)
endTie          = endTie' . addNotation (Tied Stop)


setNoteValP v x     = x { noteType = Just (v, Nothing) }
setVoiceP n x       = x { noteVoice = Just (fromIntegral n) }
setTimeModP m n x   = x { noteTimeMod = Just (fromIntegral m, fromIntegral n) }
beginBeamP n x      = x { noteBeam = Just (fromIntegral n, BeginBeam) }
continueBeamP n x   = x { noteBeam = Just (fromIntegral n, ContinueBeam) }
endBeamP n x        = x { noteBeam = Just (fromIntegral n, EndBeam) }
dotP x@(NoteProps { noteDots = n@_ })       = x { noteDots = succ n }
addNotationP  n x@(NoteProps { noteNotations = ns@_ }) = x { noteNotations = (mergeNotations $ ns++[n]) }
mapNotationsP f x@(NoteProps { noteNotations = ns@_ }) = x { noteNotations = (f ns) }


-- ----------------------------------------------------------------------------------

beginGliss   :: Music -> Music
endGliss     :: Music -> Music
beginSlide   :: Music -> Music
endSlide     :: Music -> Music
beginGliss   = addNotation (Glissando 1 Start Solid Nothing)
endGliss     = addNotation (Glissando 1 Stop Solid Nothing)
beginSlide   = addNotation (Slide 1 Start Solid Nothing)
endSlide     = addNotation (Slide 1 Stop Solid Nothing)

-- ----------------------------------------------------------------------------------

fermata         :: FermataSign -> Music -> Music
breathMark      :: Music -> Music
caesura         :: Music -> Music
fermata         = addNotation . Fermata
breathMark      = addNotation (Articulations [BreathMark])	 
caesura         = addNotation (Articulations [Caesura])	 

-- ----------------------------------------------------------------------------------

beginSlur       :: Music -> Music
endSlur         :: Music -> Music
beginSlur       = addNotation (Slur 1 Start)
endSlur         = addNotation (Slur 1 Stop)

staccato        :: Music -> Music
tenuto          :: Music -> Music
accent          = addNotation (Articulations [Accent])	 
strongAccent    = addNotation (Articulations [StrongAccent])	 
staccato        = addNotation (Articulations [Staccato])	 
tenuto          = addNotation (Articulations [Tenuto])	 
detachedLegato  = addNotation (Articulations [DetachedLegato])	 
staccatissimo   = addNotation (Articulations [Staccatissimo])	 
spiccato        = addNotation (Articulations [Spiccato])	 
scoop           = addNotation (Articulations [Scoop])	 
plop            = addNotation (Articulations [Plop])	 
doit            = addNotation (Articulations [Doit])	 
falloff         = addNotation (Articulations [Falloff])	 
stress          = addNotation (Articulations [Stress])	 
unstress        = addNotation (Articulations [Unstress])	 

-- ----------------------------------------------------------------------------------

cresc, dim                         :: Music -> Music
crescFrom, crescTo, dimFrom, dimTo :: Dynamics -> Music -> Music 
crescFromTo, dimFromTo             :: Dynamics -> Dynamics -> Music -> Music 

cresc           = \m -> beginCresc <> m <> endCresc
dim             = \m -> beginDim   <> m <> endDim

crescFrom x     = \m -> dynamic x <> cresc m
crescTo x       = \m ->              cresc m <> dynamic x
crescFromTo x y = \m -> dynamic x <> cresc m <> dynamic y

dimFrom x       = \m -> dynamic x <> dim m
dimTo x         = \m ->              dim m <> dynamic x
dimFromTo x y   = \m -> dynamic x <> dim m <> dynamic y

beginCresc, endCresc, beginDim, endDim :: Music

beginCresc      = [MusicDirection $ Crescendo  Start]
endCresc        = [MusicDirection $ Crescendo  Stop]
beginDim        = [MusicDirection $ Diminuendo Start]
endDim          = [MusicDirection $ Diminuendo Stop]


dynamic :: Dynamics -> Music
dynamic level   = [MusicDirection $ Dynamics level]

-- pppppp      :: Music
-- ppppp       :: Music
-- pppp        :: Music
-- ppp         :: Music
-- pp          :: Music
-- p           :: Music
-- mp          :: Music
-- mf          :: Music
-- ff          :: Music
-- fff         :: Music
-- ffff        :: Music
-- fffff       :: Music
-- ffffff      :: Music
-- 
-- pppppp          = [MusicDirection $ Dynamics PPPPPP]
-- ppppp           = [MusicDirection $ Dynamics PPPPP]
-- pppp            = [MusicDirection $ Dynamics PPPP]
-- ppp             = [MusicDirection $ Dynamics PPP]
-- pp              = [MusicDirection $ Dynamics PP]
-- p               = [MusicDirection $ Dynamics P]
-- mp              = [MusicDirection $ Dynamics MP]
-- mf              = [MusicDirection $ Dynamics MF]
-- -- f               = [MusicDirection $ Dynamics F]
-- ff              = [MusicDirection $ Dynamics FF]
-- fff             = [MusicDirection $ Dynamics FFF]
-- ffff            = [MusicDirection $ Dynamics FFFF]
-- fffff           = [MusicDirection $ Dynamics FFFFF]
-- ffffff          = [MusicDirection $ Dynamics FFFFFF]


-- FIXME should scale duration by inverse
tuplet :: Int -> Int -> Music -> Music
tuplet m n []   = []
tuplet m n [xs] = [xs]
tuplet m n xs   = setTimeMod m n $ (as ++ bs ++ cs)
    where
        as  = beginTuplet [head xs]
        bs  = init (tail xs)
        cs  = endTuplet [last (tail xs)]

beam :: Music -> Music
beam []   = []
beam [xs] = [xs]
beam xs   = (as ++ bs ++ cs)
    where
        as  = beginBeam [head xs]
        bs  = continueBeam (init (tail xs))
        cs  = endBeam [last (tail xs)]

slur :: Music -> Music
slur []   = []
slur [xs] = [xs]
slur xs   = (as ++ bs ++ cs)
    where
        as  = beginSlur [head xs]
        bs  = init (tail xs)
        cs  = endSlur [last (tail xs)]
                                           
-- TODO combine tuplet, beam, slur etc



-----------------------------------------------------------------------------
-- * Ornaments
-----------------------------------------------------------------------------

tremolo :: Int -> Music -> Music
tremolo n = addNotation (Ornaments [(Tremolo $ fromIntegral n, [])])

-- ----------------------------------------------------------------------------------
-- Text
-- ----------------------------------------------------------------------------------

text :: String -> Music
rehearsal :: String -> Music

text      = single . MusicDirection . Words
rehearsal = single . MusicDirection . Rehearsal

segno, coda :: Music
segno = single . MusicDirection $ Segno
coda  = single . MusicDirection $ Coda


-- ----------------------------------------------------------------------------------

instance Default ScoreAttrs where
    def = ScoreAttrs []

instance Default ScoreHeader where
    def = ScoreHeader Nothing Nothing Nothing []

instance Default Note where
    def = Note def def [] def

instance Default Divs where
    def = defaultDivisionsVal

instance Default FullNote where
    def = Rest noChord Nothing

instance Default NoteProps where
    def = NoteProps Nothing Nothing (Just (1/4, Nothing)) 0 Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] []


-- class HasDyn a where
--     mapLevel :: (Level -> Level) -> (a -> a)
-- 
-- class HasPitch a where
--     mapPitch :: (Pitch -> Pitch) -> (a -> a)
-- 
-- class HasPitch a => HasAcc a where
--     flatten :: a -> a
--     sharpen :: a -> a
--     mapAcc  :: (Semitones -> Semitones) -> a -> a
--     flatten = mapAcc pred
--     sharpen = mapAcc succ



-------------------------------------------------------------------------------------


logBaseR :: forall a . (RealFloat a, Floating a) => Rational -> Rational -> a
logBaseR k n 
    | isInfinite (fromRational n :: a)      = logBaseR k (n/k) + 1
logBaseR k n 
    | isDenormalized (fromRational n :: a)  = logBaseR k (n*k) - 1
logBaseR k n                         = logBase (fromRational k) (fromRational n)

isDivisibleBy :: (Real a, Real b) => a -> b -> Bool
isDivisibleBy n = (equalTo 0.0) . snd . properFraction . logBaseR (toRational n) . toRational

single x = [x]
equalTo  = (==)
