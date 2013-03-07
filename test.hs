
{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

module Main where

-- import Data.Default
-- import Data.Ratio
-- import Data.Semigroup
-- import Data.Default
import System.Posix.Process

import Music.Pitch.Literal
import Music.MusicXml
import Music.MusicXml.Pitch
import Music.MusicXml.Dynamics
import Music.MusicXml.Simple


score = foo

foo = Partwise
    (ScoreAttrs [])
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

                mp,
                
                tuplet 5 4 [
                    note c  (1/8)       & beginSlur . beginBeam 1,
                    note d  (1/8)       & endSlur . staccato . tenuto . continueBeam 1,
                    note c  (1/8)       & endSlur . staccato . tenuto . continueBeam 1,
                    note d  (1/8)       & endSlur . staccato . tenuto . continueBeam 1,
                    note c  (1/8)       & endSlur . staccato . tenuto . endBeam 1
                ],
                
                tuplet 3 2 [
                    note eb (1/4),
                    chord [d,a,fs'] (1/4),
                    note g_  (1/4)      & beginTie
                ] 
            ]     
            ,
            concat [
                segno,
                metronome (1/8) False 96,
                note g_   (1/4)      & endTie,
                note ab_  (1/4)      & endTie,
            
                beam [
                    note c  (1/16),
                    note c  (2/16),
                    note c  (1/16)
                ],
                beam [
                    note c  (3/8) & beginGliss,
                    note d  (1/8) & endGliss
                ]
            ]  
            ,  
            concat [
                pp,
                beginCresc,
                note c  (3/8),
                note d  (1/8),
                note e  (3/8),
                endCresc,
                ff,
                note f  (1/8)
            ]
        ]    
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

