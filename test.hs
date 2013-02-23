
module Main where

import Data.Semigroup
import Data.Default
import Music.MusicXml
import System.Posix.Process

-- division 38880 (2^5*3^5*5)

score = Partwise
    (ScoreAttrs [])
    (ScoreHeader 
        Nothing
        (Just "Frère Jaques") 
        (Just (Identification [Creator "composer" "Anonymous"])) 
        [
            Part "P1" "Soprano" Nothing,
            Part "P2" "Alto"    Nothing,
            Part "P3" "Tenor"   Nothing
        ])
    [
        (PartAttrs "P1", [
        
            (MeasureAttrs 1, [                
                -- setting attributes as this is first measure 
                MusicAttributes (Divisions 1024)
                ,
                MusicAttributes (Clef GClef 2)
                ,
                MusicAttributes (Key (-3) Major)
                ,
                MusicAttributes (Time CommonTime)
                ,
                            
                MusicNote (Note (Pitched noChord (60, noSemitones, 0)) 256 noTies)
                , 
                MusicNote (Note (Pitched noChord (62, noSemitones, 0)) 256 noTies)
                , 
                MusicNote (Note (Pitched noChord (63, noSemitones, 0)) 128 noTies)
                , 
                MusicNote (Note (Pitched noChord (62, noSemitones, 0)) 128 noTies)
                , 
                MusicNote (Note (Pitched noChord (60, noSemitones, 0)) 256 noTies)
            ])
            ,
            (MeasureAttrs 2, [                      
                MusicNote (Note (Pitched noChord (60, noSemitones, 0)) 256 noTies)
                , 
                MusicNote (Note (Pitched noChord (62, noSemitones, 0)) 256 noTies)
                , 
                MusicNote (Note (Pitched noChord (63, noSemitones, 0)) 128 noTies)
                , 
                MusicNote (Note (Pitched noChord (62, noSemitones, 0)) 128 noTies)
                , 
                MusicNote (Note (Pitched noChord (60, noSemitones, 0)) 256 noTies)
            ])
        ])
        ,
        
        (PartAttrs "P2", [
        ])
        ,

        (PartAttrs "P3", [
        ])
    ]





main = openScore

showScore = putStrLn $ showXml $ score
openScore = openXml $ score

openXml :: Score -> IO ()
openXml score = 
    do  writeFile "test.xml" (showXml score)
        execute "open" ["-a", "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6", "test.xml"]

execute :: FilePath -> [String] -> IO ()
execute program args = do
    forkProcess $ executeFile program True args Nothing
    return ()
