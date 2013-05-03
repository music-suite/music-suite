
module Main where

import Music.Score

{-
fj1 = sc $ melody [c,d] |> melody [eb,d]^/2 |> c
fj2 = sc $ melody [eb,f] |> g^*2
fj3 = sc $ g^*(3/4) |> ab^*(1/4) |> melody [g,f,eb,d] ^/2 |> c
fj4 = c |> g_ |> c^*2

fj  = rep 2 fj1 |> rep 2 fj2 |> rep 2 fj3 |> rep 2 fj4

fj' = mempty
    <> setVoices "Violin I"     (rep 10 fj) 
    <> setVoices "Violin II"    (delay 8  $ (rep 10 fj)^*(2/3)) 
    <> setVoices "Viola"        (delay 16 $ (rep 10 fj)^*(4/5)) 
    <> setVoices "Violoncello"  (delay 24 $ (rep 10 fj)^*(4/7))

-- classic version...
fj'' = mempty
    <> setVoices "Violin I"     (rep 10 fj) 
    <> setVoices "Violin II"    (delay 8  $ (rep 10 fj)) 
    <> setVoices "Viola"        (delay 16 $ (rep 10 fj)) 
    <> setVoices "Violoncello"  (delay 24 $ (rep 10 fj))

-- rep 0 x = mempty
-- rep n x = x |> rep (n-1) x
grp n p = rep n p^/n

-- open$ (rep 3 $ grp 2 c |> grp 4 db |> grp 4 c  |> (rest^*3))

testArtDyn = mempty
    <> v (0+1) (rep 80 t^*(3*1))
    <> v (0+2) (rep 80 t^*(4*1)) 
    <> v (0+3) (rep 80 t^*(5*1))
    <> v (0+4) (rep 80 t^*(7*1))
    <> v (4+1) (rep 20 s^*(3*2))
    <> v (4+2) (rep 20 s^*(4*2)) 
    <> v (4+3) (rep 20 s^*(5*2))
    <> v (4+4) (rep 20 s^*(7*2))
    where
        v x = setVoices (VoiceName $ "Violin I." ++ show x)
        s, t :: Sc Double
        s = mempty
            |> (fmap (setLevel (-2.5) . setBeginSlur True . setBeginCresc True) c) 
            |> d 
            |> f 
            |> (fmap (setEndSlur True . setLevel (0.5) . setEndCresc True   ) e)
        t = mempty
            |> (fmap (setLevel (-2.5) . setBeginSlur True . setBeginCresc True) g) 
            |> a 
            |> bb 
            |> (fmap (setEndSlur True . setLevel (0.5) . setEndCresc True   ) a)

showScore :: Score Double -> String
showScore = show

play = playMidiIO . (^* (60/100)) . sc
open = openXml . (^* (1/4)) . sc

rep 0 x = mempty
rep n x = x |> rep (n-1) x
                              -}
                              
main = do
    putStrLn "Testing Music.Score"