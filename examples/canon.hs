
-- |
-- Simple canon on a familiar subject.
-- 
import Music.Prelude

frere :: Music
frere = mempty
  |> times 2 (scat [c,d,e,c]|/4) 
  |> times 2 (scat [e,f,g|*2]|/4) 
  |> times 2 (scat [g,a,g,f,scat [e,c]|*2]|/8)
  |> times 2 (scat [c,g_,c|*2]|/4)

frere2 = delay 2 frere <> frere
frere4 = delay 4 frere2 <> frere2

info = title "Frere Jaques" . composer "Trad." . tempo (metronome (1/4) 120)
main = open $ info $ asScore $ frere4

