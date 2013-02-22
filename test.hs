
module Main where

import Data.Semigroup
import Data.Default
import Music.MusicXml

-- division 38880 (2^5*3^5*5)

score = Partwise
    (ScoreAttrs [1])
    (ScoreHeader [])
    [
        -- part 1
        [
            -- measure 1
            (MeasureAttrs 1, [ 
                MusicAttr (Div 256)
                ,
                MusicAttr (Clef GClef 2)
                ,
                MusicAttr (Key (-3))
                ,
                MusicAttr (Time CommonTime)
                ,
                
            
                MusicNote (Note (Pitched True (60,Nothing,0)) (Divisions 256) [])
                , 
                MusicNote (Note (Pitched True (62,Nothing,0)) (Divisions 256) [])
                , 
                MusicNote (Note (Pitched True (63,Nothing,0)) (Divisions 128) [])
                , 
                MusicNote (Note (Pitched True (62,Nothing,0)) (Divisions 128) [])
                , 
                MusicNote (Note (Pitched True (60,Nothing,0)) (Divisions 256) [])
                ])
        ]
    ]





main = putStrLn $ showXml $ score

openXml :: Score -> IO ()
openXml = undefined -- open in Sibelius


