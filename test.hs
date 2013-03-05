
--{-# LANGUAGE  #-}

module Main where

import Data.Default
import Data.Ratio
import Data.Semigroup
import Data.Default
import System.Posix.Process

import Music.Pitch.Literal

import Music.MusicXml hiding (chord) -- TODO
import Music.MusicXml.Pitch
import Music.MusicXml.Dynamics hiding (F)
import qualified Music.MusicXml.Dynamics as Dynamics

-- division 38880 (2^5*3^5*5)
stdDivs :: Divs
stdDivs = 768*4


stdDivisions :: MusicElem
stdDivisions = MusicAttributes $ Divisions $ stdDivs `div` 4

trebleClef :: MusicElem
altoClef   :: MusicElem
bassClef   :: MusicElem
trebleClef = MusicAttributes $ Clef GClef 2
altoClef   = MusicAttributes $ Clef CClef 3
bassClef   = MusicAttributes $ Clef FClef 4

commonTime = MusicAttributes $ Time CommonTime
cutTime    = MusicAttributes $ Time CutTime
time a b   = MusicAttributes $ Time $ DivTime a b
key n m    = MusicAttributes $ Key n m


parts :: [(String, String)] -> PartList
parts = zipWith (\partId (name,abbr) -> Part partId name (Just abbr)) partIds
    where
        partIds = [ "P" ++ show n | n <- [1..] ]

partsNoAbbr :: [String] -> PartList
partsNoAbbr = zipWith (\partId name -> Part partId name Nothing) partIds
    where
        partIds = [ "P" ++ show n | n <- [1..] ]

version xs = ScoreAttrs xs

header :: String -> String -> PartList -> ScoreHeader
header title composer partList = ScoreHeader Nothing (Just title) (Just (Identification [Creator "composer" composer])) partList





-- Classes and instances

instance Default ScoreHeader where
    def = ScoreHeader Nothing Nothing Nothing []

instance Default Note where
    def = Note def def [] def

instance Default Divs where
    def = stdDivs

instance Default FullNote where
    def = Rest noChord Nothing

instance Default NoteProps where
    def = NoteProps Nothing (Just (1/4, Nothing)) 0 Nothing


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


-- Elems

setValue :: NoteVal -> MusicElem -> MusicElem
setVoice :: Int -> MusicElem -> MusicElem
beginBeam :: Int -> MusicElem -> MusicElem
endBeam :: Int -> MusicElem -> MusicElem
addDot :: MusicElem -> MusicElem
removeDot :: MusicElem -> MusicElem

setValue x = mapNoteProps2 (setValueP x)
setVoice n = mapNoteProps2 (setVoiceP n)
beginBeam n = mapNoteProps2 (beginBeamP n)
endBeam n = mapNoteProps2 (endBeamP n)
addDot = mapNoteProps2 addDotP
removeDot = mapNoteProps2 removeDotP


-- Notes

setValueN :: NoteVal -> Note -> Note
setVoiceN :: Int -> Note -> Note
beginBeamN :: Int -> Note -> Note
endBeamN :: Int -> Note -> Note
addDotN :: Note -> Note
removeDotN :: Note -> Note

setValueN x = mapNoteProps (setValueP x)
setVoiceN n = mapNoteProps (setVoiceP n)
beginBeamN n = mapNoteProps (beginBeamP n)
endBeamN n = mapNoteProps (endBeamP n)
addDotN = mapNoteProps addDotP
removeDotN = mapNoteProps removeDotP

-- TODO function beam :: Beamable a -> [a] -> [a]

-- Note properties

setValueP :: NoteVal -> NoteProps -> NoteProps
setVoiceP :: Int -> NoteProps -> NoteProps
beginBeamP :: Int -> NoteProps -> NoteProps
endBeamP :: Int -> NoteProps -> NoteProps
addDotP :: NoteProps -> NoteProps
removeDotP :: NoteProps -> NoteProps

setValueP v x = x { noteType = Just (v, Nothing) }
setVoiceP n x = x { noteVoice = Just (fromIntegral n) }
beginBeamP n x = x { noteBeam = Just (fromIntegral n, Begin) }
endBeamP n x = x { noteBeam = Just (fromIntegral n, End) }
addDotP x@(NoteProps { noteDots = n@_ }) = x { noteDots = succ n }
removeDotP x@(NoteProps { noteDots = n@_ }) = x { noteDots = succ n }


infixr 1 &
(&) = flip ($)


-- TODO handle dots etc
rest :: NoteVal -> MusicElem
rest dur = MusicNote (Note def (stdDivs `div` denom) noTies (setValueP val def))
    where
        num   = fromIntegral $ numerator   $ toRational $ dur
        denom = fromIntegral $ denominator $ toRational $ dur
        val   = NoteVal $ toRational $ dur              

    

note :: Pitch -> NoteVal -> MusicElem
note pitch dur = note' pitch dur

note' :: Pitch -> NoteVal -> MusicElem
note' pitch dur = MusicNote $ Note (Pitched noChord $ pitch) (stdDivs `div` denom) noTies (setValueP val $ def)
    where
        num   = fromIntegral $ numerator   $ toRational $ dur
        denom = fromIntegral $ denominator $ toRational $ dur
        val   = NoteVal $ toRational $ dur              

-- (num, denom, dots, val)

-- TODO
chord :: [Pitch] -> NoteVal -> MusicElem
chord ps d = note (head ps) d
-- chord pitches dur = 
--     
--     MusicNote $ Note (Pitched noChord pitch) (stdDivs `div` denom) noTies (setValueP val $ def)
--     
--     where
--         num   = fromIntegral $ numerator   $ toRational $ dur
--         denom = fromIntegral $ denominator $ toRational $ dur
--         val   = NoteVal $ toRational $ dur              


score = Partwise
    (version [])
    (header "Frère Jaques" "Anonymous" $ parts [
        ("Violin",      "Vl."),
        ("Viola",       "Vla."),
        ("Violoncello", "Vc.")])
    [
        (PartAttrs "P1", [

            (MeasureAttrs 1, [
                -- setting attributes as this is first measure
                stdDivisions
                ,
                trebleClef,
                key (-3) Major,
                commonTime,
                note c  (1/4),
                note d  (1/4),
                note eb (1/8) & beginBeam 1,
                note d  (1/8) & endBeam 1,
                note c  (1/4)
            ])
            ,
            (MeasureAttrs 2, [
                note c  (1/4),
                note d  (1/4),
                note eb (1/8) & beginBeam 1,
                note d  (1/8) & endBeam 1,
                note c  (1/4)
            ])
            ,
            (MeasureAttrs 3, [ 
                note g  (1/8) & beginBeam 1, -- TODO handle dot here
                note ab (1/8) & endBeam 1,
                note g  (1/8) & beginBeam 1,
                note f  (1/8) & endBeam 1,
                note eb (1/8) & beginBeam 1,
                note d  (1/8) & endBeam 1,
                note c  (1/4)
            ])

        ])
        ,

        (PartAttrs "P2", [
            (MeasureAttrs 1, [
                stdDivisions,
                key (-3) Major,
                altoClef,
                note c  (1/4),
                note g_ (1/4),
                note c  (1/2)
            ])
            ,
            (MeasureAttrs 2, [
                note c  (1/4),
                note g_ (1/4),
                note c  (1/2)
            ])
            ,
            (MeasureAttrs 3, [
                rest 1
            ])
        ])
        ,

        (PartAttrs "P3", [
            (MeasureAttrs 1, [
                stdDivisions,
                key (-3) Major,
                bassClef,
                note c_ (1/1)
            ])
            ,
            (MeasureAttrs 2, [
                chord [c_, e_, g] (1/1)
            ])
            ,
            (MeasureAttrs 3, [
                rest 1
            ])
        ])
    ]




main = openScore

openScore = openSib score
-- openScore = openLy score

showScore = putStrLn $ showXml $ score

openSib :: Score -> IO ()
openSib score =
    do  writeFile "test.xml" (showXml score)
        execute "open" ["-a", "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6", "test.xml"]

openLy :: Score -> IO ()
openLy score =
    do  writeFile "test.xml" (showXml score)
        execute "musicxml2ly" ["test.xml"]
        execute "lilypond" ["test.ly"]
        execute "open" ["test.pdf"]

execute :: FilePath -> [String] -> IO ()
execute program args = do
    forkProcess $ executeFile program True args Nothing
    return ()
