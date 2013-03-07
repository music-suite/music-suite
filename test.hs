
{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

module Main where

import Data.Default
import Data.Ratio
import Data.Semigroup
import Data.Default
import System.Posix.Process

import Music.Pitch.Literal

import Music.MusicXml
import Music.MusicXml.Pitch
import Music.MusicXml.Dynamics

-- division 38880 (2^5*3^5*5)
stdDivs :: Divs
stdDivs = 768*4


stdDivisions :: Music
stdDivisions = single $ MusicAttributes $ Divisions $ stdDivs `div` 4

trebleClef :: Music
altoClef   :: Music
bassClef   :: Music
trebleClef = single $ MusicAttributes $ Clef GClef 2
altoClef   = single $ MusicAttributes $ Clef CClef 3
bassClef   = single $ MusicAttributes $ Clef FClef 4

commonTime = single $ MusicAttributes $ Time CommonTime
cutTime    = single $ MusicAttributes $ Time CutTime
time a b   = single $ MusicAttributes $ Time $ DivTime a b
key n m    = single $ MusicAttributes $ Key n m


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


-- Elems

setVoice    :: Int -> Music -> Music
setValue    :: NoteVal -> Music -> Music
addDot      :: Music -> Music
setTimeMod  :: Int -> Int -> Music -> Music
addNotation :: Notation -> Music -> Music
beginBeam    :: Int -> Music -> Music
continueBeam :: Int -> Music -> Music
endBeam      :: Int -> Music -> Music

beginTie    :: Music -> Music
endTie      :: Music -> Music

setValue x      = fmap $ mapNoteProps2 (setValueP x)
setVoice n      = fmap $ mapNoteProps2 (setVoiceP n)
addDot          = fmap $ mapNoteProps2 addDotP
setTimeMod m n  = fmap $ mapNoteProps2 (setTimeModP m n)
addNotation x   = fmap $ mapNoteProps2 (addNotationP x)
beginBeam n     = fmap $ mapNoteProps2 (beginBeamP n)
continueBeam n     = fmap $ mapNoteProps2 (continueBeamP n)
endBeam n       = fmap $ mapNoteProps2 (endBeamP n)

beginTie' = fmap beginTie''
endTie'   = fmap endTie''
beginTie'' (MusicNote (Note full dur ties props)) = (MusicNote (Note full dur (ties++[Start]) props))
endTie''   (MusicNote (Note full dur ties props)) = (MusicNote (Note full dur ([Stop]++ties) props))



setValueP v x       = x { noteType = Just (v, Nothing) }
setVoiceP n x       = x { noteVoice = Just (fromIntegral n) }
addDotP x@(NoteProps { noteDots = n@_ })    = x { noteDots = succ n }
setTimeModP m n x   = x { noteTimeMod = Just (fromIntegral m, fromIntegral n) }
addNotationP n x@(NoteProps { noteNotations = ns@_ })    = x { noteNotations = n:ns }
beginBeamP n x      = x { noteBeam = Just (fromIntegral n, BeginBeam) }
continueBeamP n x   = x { noteBeam = Just (fromIntegral n, ContinueBeam) }
endBeamP n x        = x { noteBeam = Just (fromIntegral n, EndBeam) }


infixr 1 &
(&)  = flip ($)


-- TODO handle dots etc
rest :: NoteVal -> Music
rest dur = single $ MusicNote (Note def (stdDivs `div` denom) noTies (setValueP val def))
    where
        num   = fromIntegral $ numerator   $ toRational $ dur
        denom = fromIntegral $ denominator $ toRational $ dur
        val   = NoteVal $ toRational $ dur              

    

note :: Pitch -> NoteVal -> Music
note pitch dur = note' False pitch dur' dots
    where
        (dur', dots) = separateDots dur

chordNote :: Pitch -> NoteVal -> Music
chordNote pitch dur = note' True pitch dur' dots
    where
        (dur', dots) = separateDots dur

note' :: Bool -> Pitch -> NoteVal -> Int -> Music
note' isChord pitch dur dots 
    = single $ MusicNote $ 
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

    
chord :: [Pitch] -> NoteVal -> Music
chord [] d      = error "No notes in chord"
chord (p:ps) d  = note p d ++ concatMap (\p -> chordNote p d) ps




parts :: [[Music]] -> [(PartAttrs, [(MeasureAttrs, Music)])]
parts = zipWith (\ids mus -> (PartAttrs ids, zipWith (\ids mus -> (MeasureAttrs ids, mus)) barIds mus)) partIds
    where
        partIds = kPartIds
        barIds  = [1..]






rehearsal = single . MusicDirection . Rehearsal
segno     = single . MusicDirection $ Segno
text      = single . MusicDirection . Words
metronome nv dot tempo = single $ MusicDirection (Metronome nv dot tempo)

beginSlur = addNotation (Slur 1 Start)
endSlur   = addNotation (Slur 1 Stop)
endTuplet = addNotation (Tuplet 1 Stop)
beginTuplet = addNotation (Tuplet 1 Start)
stacc     = addNotation (Articulations [Staccato])
tenuto    = addNotation (Articulations [Tenuto])

beginTie = beginTie' . addNotation (Tied Start)
endTie   = endTie' . addNotation (Tied Stop)

tuplet :: Int -> Int -> [Music] -> Music
tuplet m n []   = []
tuplet m n [xs] = xs
tuplet m n xs   = setTimeMod m n $ concat ([a] ++ bs ++ [c])
    where
        a   = beginTuplet $ head xs
        bs  = init (tail xs)
        c   = endTuplet $ last (tail xs)

score = foo

foo = Partwise
    (version [])
    (header "Frère Jaques" "Anonymous" (partList [("Voice","Voice")]))
    $ parts [ 
        [
            concat [
                stdDivisions,
                trebleClef,
                key eb Major,
                commonTime,                       
                
                rehearsal "A",
                text "hello world!",
                
                tuplet 5 4 [
                    note c  (1/8)       & beginSlur . beginBeam 1,
                    note d  (1/8)       & endSlur . stacc . tenuto . continueBeam 1,
                    note c  (1/8)       & endSlur . stacc . tenuto . continueBeam 1,
                    note d  (1/8)       & endSlur . stacc . tenuto . continueBeam 1,
                    note c  (1/8)       & endSlur . stacc . tenuto . endBeam 1
                ],
                
                tuplet 3 2 [
                    note eb (1/4),
                    chord [d,a,fs'] (1/4),
                    note g_  (1/4)      & beginTie
                ]
            ]     
            ,
            concat [
                note g_  (1/4)      & endTie,
                segno,
                metronome (1/8) False 96
            ]
        ]    
    ]
        



-- frere = Partwise
--     (version [])
--     (header "Frère Jaques" "Anonymous" (partList [
--         ("Violin",      "Vl."),
--         ("Viola",       "Vla."),
--         ("Violoncello", "Vc.")]))
--     $ parts [
--         take 200 $ cycles [
--             [
--                 stdDivisions,
--                 trebleClef,
--                 key eb Major,
--                 commonTime,
--                 note c  (1/4),
--                 note d  (1/4),
--                 note eb (1/8)       & beginBeam 1,
--                 note d  (1/8)       & endBeam 1,
--                 note c  (1/4)
--             ]
--             ,
--             [
--                 note c  (1/4),
--                 note d  (1/4),
--                 note eb (1/8)       & beginBeam 1,
--                 note d  (1/8)       & endBeam 1,
--                 note c  (1/4)
--             ]
--             ,
--             [ 
--                 note g  (3/16)      & beginBeam 1,
--                 note ab (1/16)      & endBeam 1,
--                 note g  (1/8)       & beginBeam 1,
--                 note f  (1/8)       & endBeam 1,
--                 note eb (1/8)       & beginBeam 1,
--                 note d  (1/8)       & endBeam 1,
--                 note c  (1/4)
--             ]
--         ]
--         ,
--         take 200 $ cycles [
--             [
--                 stdDivisions,
--                 key  eb Major,
--                 altoClef,
--                 note c  (1/4),
--                 note g_ (1/4),
--                 note c  (1/2)
--             ]
--             ,
--             [
--                 note c  (1/4),
--                 note g_ (1/4),
--                 note c  (1/2)
--             ]
--         ]
--         ,
--         take 200 $ cycles [
--             [
--                 stdDivisions,
--                 key eb Major,
--                 bassClef,
--                 note c_ (1/1)
--             ]
--             ,
--             chord [c_, g_, eb] (1/1)
--         ]
--     ]       
    
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


single x = [x]