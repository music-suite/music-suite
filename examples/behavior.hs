
import Music.Prelude

toPitch :: Double -> Pitch
toPitch x = c .+^ si x
  where
    si t = spell usingSharps $ (floor t :: Semitones)
    
main = open music
music = set pitches' (stretch 12 $ fmap toPitch $ sine * 7) $ times 24 c