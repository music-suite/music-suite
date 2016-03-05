
import Music.Prelude

-- |
-- Bela Bartok: Wandering (excerpt)
-- From Mikrokosmos, vol. III
-- 
-- Inspired by the Abjad transcription
-- 

music :: Music
music = let
    meta = id
      . title "Mikrokosmos (excerpt)"
      . composer "Bela Bartok"
      . timeSignature (2/4)
      . timeSignatureDuring ((2/4) >-> (5/4)) (3/4) 
    
    left = (level pp {-. legato-}) 
         (scat [a,g,f,e] |> d|*2)
      |> {-(level ((mp |> mp `cresc` mf |> mf)|*8) . legato)-}id 
         (scat [g,f,e,d] |> c |> (d |> e)|/2 |> f |> e |> d|*8)
    -- 
    right = up _P4 . delay 2 $ 
         (level pp {-. legato-}) 
         (scat [a,g,f,e] |> d|*2)
      |> (level mp {-. legato-}) 
         (scat [g,f,e,d] |> c |> (d |> e)|/2 |> f |> e |> d|*8)

  in meta $ compress 8 $ left <> set parts' cellos (down _P8 right)

main = openLilypond music
