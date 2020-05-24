
{-
  Inspired by Belkin, Rosen et al

  Octave-changing a single pitch
  Changing metrical accent
  Inserting shorter pitches
  Outlining intervals (?)
  Diatonic/chromatic alterations

  Contrast the (usually relatively non-salient) counterpoint/serial procedures:
    Retrograde
    Inversion
    Interval scaling

  Define all as functions/morphisms with function composition etc.
  Work with very short simple melodic fragments

-}
import Control.Lens (Lens', lens, element)
import Music.Prelude
import Data.Foldable (Foldable)
import Music.Time.Internal.Util (rotate)
import qualified Data.List
import qualified Data.Ord
import qualified Data.Either
import qualified Data.Maybe
import qualified Debug.Trace
import qualified Data.Foldable
import qualified Music.Time.Internal.Convert


type Melody = Voice Pitch
type Rhythm = Voice ()

rotateR :: Int -> Rhythm -> Rhythm
rotateR n = over notes (rotate n)

setR :: Rhythm -> Melody -> Melody
setR = zipVoiceWith' (const) (flip const)

getR :: Melody -> Rhythm
getR = fmap (const ())

rhythm' :: Lens' Melody Rhythm
rhythm' = lens getR (flip setR)

rotateRhythm :: Int -> Melody -> Melody
rotateRhythm n = over rhythm' (rotateR n)

transposeSingleNote :: Int -> Interval -> Melody -> Melody
transposeSingleNote n i = over (notes . element n) (up i)

addLeading :: Interval -> Melody -> Melody
addLeading i = over notes (>>= \n -> [0.5*|n,0.5*|down i n])

addLeadingD :: Int -> Melody -> Melody
addLeadingD i = over notes (>>= \n -> [0.5*|n,0.5*|downDiatonic c (fromIntegral i) n])

music :: Music
music = times 4 $ set parts' clarinets $ fmap fromPitch $
  renderAlignedVoice $ aligned 0 0 $
    mconcat
      [ v
      -- , transposeSingleNote 0 m3 v
      -- , transposeSingleNote 1 m3 v
      -- , addLeading m3 v
      , addLeadingD 1 v
      , addLeadingD 2 v
      , addLeadingD (-1) v
      , addLeadingD (-2) v
      -- , addLeadingD 5 v
      , addLeadingD 1 (retr $ addLeadingD (-1) v)
      , addLeadingD 1 (addLeadingD 2 (retr $ addLeadingD (-1) v))
      , addLeadingD (-2) (addLeadingD (-1) (addLeadingD 1 (retr $ addLeadingD (-3) v)))
      , up _P5 $ addLeadingD (-2) (retr $ addLeadingD (-1) (addLeadingD 1 (addLeadingD (-3) v)))
      , up _P5 $ addLeadingD (-2) (addLeadingD (-1) (addLeadingD 1 (addLeadingD (-3) $ retr v)))
      ]
  -- (\x -> x <> upDiatonic c 2 x) $ downDiatonic c 2 $ renderAlignedVoice $ aligned 0 0 $transposeSingleNote 1 m3 $ addLeadingD (-1) v
  where
    v = [c,d,g|*2]^.voice

retr :: Voice a -> Voice a
retr = over notes reverse

main = defaultMain music
