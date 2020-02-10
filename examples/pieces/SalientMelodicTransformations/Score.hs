
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
addLeading i = over notes (>>= \n -> [0.75*|n,0.25*|down i n])

addLeadingD :: Int -> Melody -> Melody
addLeadingD i = over notes (>>= \n -> [0.75*|n,0.25*|downDiatonic c (fromIntegral i) n])

music :: Music
music = fmap fromPitch $
  (\x -> x <> upDiatonic c 2 x) $ downDiatonic c 2 $ renderAlignedVoice $ aligned 0 0 $transposeSingleNote 1 m3 $ addLeadingD (-1) v
  where
    v = [c,d,e]^.voice

main = defaultMain music
