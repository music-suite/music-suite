
import Music.Prelude

import Control.Lens (set)

-- |
-- String quartet
--
-- Hommage a Henrik Strindberg
--

tremCanon :: Music
tremCanon = compress 4 $
    (delay 124 $ set parts' violins1 $ subjs|*1)
        <>
    (delay 120 $ set parts' violins2 $ subjs|*1)
        <>
    (delay 4 $ set parts' violas $ subjs|*2)
        <>
    (delay 0 $ set parts' cellos  $ subjs|*2)
    where
        subjs = pseq $ map (\n -> palindrome $ rev $ subj n) [1..40::Int]
        subj n
            | n < 8     = a_|*2  |> e|*1   |> a|*1
            | n < 16    = a_|*2  |> e|*1   |> a|*1   |> e|*1   |> a|*1
            | n < 24    = a_|*2  |> e|*0.5 |> a|*0.5 |> e|*0.5 |> a|*0.5
            | otherwise = e|*0.5 |> a|*0.5

mainCanon2 :: Music
mainCanon2 = palindrome mainCanon <> celloEntry

celloEntry :: Music
celloEntry = set parts' cellos e'' |*(25*5/8)

mainCanon :: Music
mainCanon = timeSignature (time 6 8) $ asScore $
    (set parts' violins1 $ harmonic 2 $ times 50 $ legato $ accentLast $
        octavesUp 2 $ pseq [a_,e,a,cs',cs',a,e,a_]|/8)

        <>
    (set parts' violins2 $ harmonic 2 $ times 50 $ legato $ accentLast $
        octavesUp 2 $ pseq [d,g,b,b,g,d]|/8)|*(3/2)

        <>
    (set parts' violas $ harmonic 2 $ times 50 $ legato $ accentLast $
        octavesUp 2 $ pseq [a,d,a,a,d,a]|/8)|*(3*2/2)

        <>
    set parts' cellos a'|*(25*5/8)

music :: Music
music = mainCanon2

main :: IO ()
main = defaultMain music

