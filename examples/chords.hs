
import Music.Prelude
import Control.Lens (set)

-- kStrum = 0.005
kStrum = 0

main = defaultMain music


guitar = (tutti $ fromMidiProgram 26)
alto   = (tutti $ fromMidiProgram 65)
rh     = (tutti $ fromMidiProgram 0)


-- Strum a chord
-- TODO port this to Chord module
strumUp :: [Score a] -> Score a
strumUp = ppar . zipWith (\t x -> delay t . stretchTo (x^.duration ^-^ t) $ x) [0,kStrum..]

strumDown = strumUp . reverse

data StrumDirection = Up | Down deriving (Eq, Ord, Show, Enum)

nextDirection :: StrumDirection -> StrumDirection
nextDirection Up   = Down
nextDirection Down = Up

-- 21212

strumRhythm
  :: StrumDirection -- ^ Initial direction
  -> [Duration]     -- ^ Duration pattern (repeated if necessary)
  -> [[Score a]]    -- ^ Sequence of chords to strum
  -> Score a
strumRhythm startDirection durations' values = pseq chords
  where
    directions = iterate nextDirection startDirection
    durations = cycle durations'
    chords = zipWith3 (\dir dur chord -> case dir of
      Up   -> strumUp (stretch dur chord)
      Down -> strumDown (stretch dur chord)
      ) directions durations values


strum :: [Music] -> Music
strum x = strumRhythm Up (map (/8) [2,1,2,1,2])
  [x,dropLast 1 x,level _p $ drop 1 x,level _p $ dropLast 1 x, level _p $ drop 1 x]
  where
    dropLast n = reverse . drop n . reverse

counterRh :: Music
counterRh = set parts' rh $ (removeRests $ times 4 $ octavesUp 1 $ pseq [rest |*2,g,g,g|*2,g|*2, rest|*2, pseq [g,g,g]|*2])|/8

strings :: Music
strings = set parts' (tutti violin) $ (\x -> x <> octavesUp 1 x) $
     (c_<>e_<>g_)|*4
  |> (c_<>fs_<>a_)|*4
  |> (g__<>c_<>e_)|*4
  |> (c_<>f_<>g_)|*4

melody :: Music
melody = octavesDown 1 $ set parts' (tutti horn) $
  (pseq [c',g'|*2,e',d',c'|*2,b,c'|*2,d'|*2,e',d',c'|*2]|/4)
  |>
  (pseq [c',a'|*2,e',d',c'|*2,b,c'|*2,d'|*2,eb',d',c']|/4)


music :: Music
music = pseq [music1, music2]

music1 = mempty
  -- <> (level mf $ set parts' guitar $ melody)
  -- <> level _p strings
  <> level mp counterRh
  <> gtr
music2 = mempty
  <> (level mf $ melody)
  <> level _p strings
  <> level _p counterRh
  <> gtr

gtr :: Music
gtr = set parts' guitar $
  (ppar $ take 4 $ zipWith delay [0,1..10] $ repeat $ strum [c_,e_,g_,c,e,g])
  |>
  (ppar $ take 4 $ zipWith delay [0,1..10] $ repeat $ strum [c_,fs_,a_,c,fs,a])
  |>
  (ppar $ take 4 $ zipWith delay [0,1..10] $ repeat $ strum [c_,e_,g_,c,e,g])
  |>
  (ppar $ take 4 $ zipWith delay [0,1..10] $ repeat $ strum [g_,a_,c,f,a,c'])
