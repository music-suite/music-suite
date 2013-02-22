
module Main where

import Data.Semigroup
import Data.Default
import Music.MusicXml
import System.Posix.Process

-- division 38880 (2^5*3^5*5)

score = Partwise
    (ScoreAttrs [1])
    (ScoreHeader 
        (Just "Frère Jaques") 
        (Just "I") 
        (Just (Identification [Creator "Music" "Anonymous"])) 
        [
            Part "1" "Voice" Nothing
        ])
    [
        -- part 1
        [
            -- measure 1
            (MeasureAttrs 1, [                
                -- setting attributes as this is first measure 
                MusicAttributes (Div 256)
                ,
                MusicAttributes (Clef GClef 2)
                ,
                MusicAttributes (Key (-3) Major)
                ,
                MusicAttributes (Time CommonTime)
                ,
                            
                MusicNote (Note (Pitched chord (60, noSemitones, 0)) 256 noTies)
                , 
                MusicNote (Note (Pitched chord (62, noSemitones, 0)) 256 noTies)
                , 
                MusicNote (Note (Pitched chord (63, noSemitones, 0)) 128 noTies)
                , 
                MusicNote (Note (Pitched chord (62, noSemitones, 0)) 128 noTies)
                , 
                MusicNote (Note (Pitched chord (60, noSemitones, 0)) 256 noTies)
                ])
            ,
            (MeasureAttrs 2, [                      
                MusicNote (Note (Pitched chord (60, noSemitones, 0)) 256 noTies)
                , 
                MusicNote (Note (Pitched chord (62, noSemitones, 0)) 256 noTies)
                , 
                MusicNote (Note (Pitched chord (63, noSemitones, 0)) 128 noTies)
                , 
                MusicNote (Note (Pitched chord (62, noSemitones, 0)) 128 noTies)
                , 
                MusicNote (Note (Pitched chord (60, noSemitones, 0)) 256 noTies)
                ])
        ]
    ]





showScore = putStrLn $ showXml $ score
main = showScore

openXml :: Score -> IO ()
openXml score = 
    do  writeFile "test.xml" (showXml score)
        execute "open" ["-a", "/Applications/Sibelius/Sibelius.app", "test.xml"]


execute :: FilePath -> [String] -> IO ()
execute program args = do
    forkProcess $ executeFile program True args Nothing
    return ()
