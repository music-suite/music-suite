
{-# LANGUAGE ScopedTypeVariables #-}

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


kPartIds :: [String]
kPartIds = [ "P" ++ show n | n <- [1..] ]

partList :: [(String, String)] -> PartList
partList = zipWith (\partId (name,abbr) -> Part partId name (Just abbr)) kPartIds

partListNoAbbr :: [String] -> PartList
partListNoAbbr = zipWith (\partId name -> Part partId name Nothing) kPartIds

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
    def = NoteProps Nothing (Just (1/4, Nothing)) 0 Nothing Nothing Nothing Nothing Nothing Nothing []


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
setTimeModP :: Int -> Int -> NoteProps -> NoteProps

setValueP v x = x { noteType = Just (v, Nothing) }
setVoiceP n x = x { noteVoice = Just (fromIntegral n) }
beginBeamP n x = x { noteBeam = Just (fromIntegral n, Begin) }
endBeamP n x = x { noteBeam = Just (fromIntegral n, End) }
addDotP x@(NoteProps { noteDots = n@_ }) = x { noteDots = succ n }
removeDotP x@(NoteProps { noteDots = n@_ }) = x { noteDots = succ n }
setTimeModP m n x = x { noteTimeMod = Just (fromIntegral m, fromIntegral n) }


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
note pitch dur = note' False pitch dur' dots
    where
        (dur', dots) = separateDots dur

chordNote :: Pitch -> NoteVal -> MusicElem
chordNote pitch dur = note' True pitch dur' dots
    where
        (dur', dots) = separateDots dur

note' :: Bool -> Pitch -> NoteVal -> Int -> MusicElem
note' isChord pitch dur dots 
    = MusicNote $ 
        Note 
            (Pitched isChord $ pitch) 
            (stdDivs `div` denom) 
            noTies 
            (setValueP val $ addDots $ def)
    where                    
        addDots = foldl (.) id (replicate dots addDotP)
        num     = fromIntegral $ numerator   $ toRational $ dur
        denom   = fromIntegral $ denominator $ toRational $ dur
        val     = NoteVal $ toRational $ dur              

separateDots :: NoteVal -> (NoteVal, Int)
separateDots = separateDots' [2/3, 6/7, 14/15, 30/31, 62/63]

separateDots' :: [NoteVal] -> NoteVal -> (NoteVal, Int)
separateDots' []         nv                    = error "Note value must be a multiple of two or dotted"
separateDots' (div:divs) nv | divisibleBy 2 nv = (nv,  0)
                            | otherwise        = (nv', dots' + 1)
    where                            
        (nv', dots') = separateDots' divs (nv*div)
        divisibleBy n = (== 0.0) . snd . properFraction . logBaseRational n . toRational

    
chord :: [Pitch] -> NoteVal -> [MusicElem]
chord [] d      = error "No notes in chord"
chord (p:ps) d  = [note p d] ++ map (\p -> chordNote p d) ps




parts :: [[Music]] -> [(PartAttrs, [(MeasureAttrs, Music)])]
parts = zipWith (\ids mus -> (PartAttrs ids, zipWith (\ids mus -> (MeasureAttrs ids, mus)) barIds mus)) partIds
    where
        partIds = kPartIds
        barIds  = [1..]


score = Partwise
    (version [])
    (header "Frère Jaques" "Anonymous" $ partList [
        ("Violin",      "Vl."),
        ("Viola",       "Vla."),
        ("Violoncello", "Vc.")])
        
    $ parts [
        take 200 $ cycles [
            [
                stdDivisions,
                trebleClef,
                key eb Major,
                commonTime,
                note c  (1/4),
                note d  (1/4),
                note eb (1/8)       & beginBeam 1,
                note d  (1/8)       & endBeam 1,
                note c  (1/4)
            ]
            ,
            [
                note c  (1/4),
                note d  (1/4),
                note eb (1/8)       & beginBeam 1,
                note d  (1/8)       & endBeam 1,
                note c  (1/4)
            ]
            ,
            [ 
                note g  (3/16)  & beginBeam 1,
                note ab (1/16)  & endBeam 1,
                note g  (1/8)   & beginBeam 1,
                note f  (1/8)   & endBeam 1,
                note eb (1/8)   & beginBeam 1,
                note d  (1/8)   & endBeam 1,
                note c  (1/4)
            ]
        ]
        ,
        take 200 $ cycles [
            [
                stdDivisions,
                key eb Major,
                altoClef,
                note c  (1/4),
                note g_ (1/4),
                note c  (1/2)
            ]
            ,
            [
                note c  (1/4),
                note g_ (1/4),
                note c  (1/2)
            ]
        ]
        ,
        take 200 $ cycles [
            [
                stdDivisions,
                key eb Major,
                bassClef,
                note c_ (1/1)
            ]
            ,
            chord [c_, g_, eb] (1/1)
        ]
    ] 
    
cycles [] = []
cycles (x:xs) = x:(cycle xs)




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




logBaseRational :: forall a . (RealFloat a, Floating a) => Rational -> Rational -> a
logBaseRational k n 
    | isInfinite (fromRational n :: a)      = logBaseRational k (n/k) + 1
logBaseRational k n 
    | isDenormalized (fromRational n :: a)  = logBaseRational k (n*k) - 1
logBaseRational k n                         = logBase (fromRational k) (fromRational n)
