
import Music.Prelude
import Control.Lens (set)

toPitch :: Double -> Pitch
toPitch x = c .+^ si x
  where
    si t = spell usingSharps $ (floor t :: Semitones)

music :: Score (Behavior Pitch)
music = set pitches' (stretch 12 $ fmap toPitch $ sine * 7) $ times 24 c

main = defaultMain $ fmap (error "TODO") $ music
