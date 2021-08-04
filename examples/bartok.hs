{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Music.Prelude
import Control.Lens (set)

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

    left :: Music
    left = (level pp . legato)
         (pseq [a,g,f,e] |> d|*2)
      |> legato
         (level mp (pseq [g,f,e,d]) |> crescX mp mf (c |> (d |> e)|/2 |> f |> e) |> id (d|*8))

    right :: Music
    right = up _P4 . delay 2 $
         (level pp . legato)
         (pseq [a,g,f,e] |> d|*2)
      |> (level mp . legato)
         (pseq [g,f,e,d] |> c |> (d |> e)|/2 |> f |> e |> d|*8)

  in meta $ compress 8 $ left <> set parts' piano2 (down _P8 right)

[_,piano2] = divide 2 $ tutti piano

crescX :: Dynamics -> Dynamics -> Music -> Music
crescX a b = over phrases' (cresc a b)

main = defaultMain music
