
{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

-- |
-- Arvo Pärt: Cantus in Memory of Benjamin Britten (1977)
-- 
-- Inspired by the Abjad transcription
--
import Music.Prelude hiding (open)
import qualified Music.Score as Score

withTintin :: (HasPitches' a, Score.Pitch a ~ Pitch) => Pitch -> Score a -> Score a
withTintin p x = x <> tintin p x

-- | 
-- Given the a melody voice return the tintinnabuli voice.
-- 
tintin :: (HasPitches' a, Score.Pitch a ~ Pitch) => Pitch -> Score a -> Score a
tintin tonic = pitches  %~ relative tonic tintin'

-- | 
-- Given the melody interval (relative tonic), returns the tintinnabular voice interval. 
--
-- That is return the highest interval that is a member of the tonic minorTriad in any octave
-- which is also less than the given interval 
--
tintin' :: Interval -> Interval
tintin' melInterval 
    | isNegative melInterval = error "tintin: Negative interval"
    | otherwise              = last $ takeWhile (< melInterval) $ tintinStandardNotes
    where
        tintinStandardNotes = concat $ iterate (fmap (+ _P8)) minorTriad
        minorTriad = [_P1,m3,_P5]

fallingScale :: [Score StandardNote]
fallingScale = [a',g'..a_]

fallingScaleSect :: Int -> [Score StandardNote]
fallingScaleSect n = {-fmap (annotate (show n)) $-} take n $ fallingScale

mapEvensOdds :: (a -> b) -> (a -> b) -> [a] -> [b]
mapEvensOdds f g [] = []
mapEvensOdds f g [a] = [f a]
mapEvensOdds f g (a : b : cs) = f a : g b : mapEvensOdds f g cs

mainSubject :: Score StandardNote
mainSubject = stretch (1/6) $ asScore $ scat $ mapEvensOdds (accent . (|*2)) id $ concatMap fallingScaleSect [1..30]

bell :: Score StandardNote
bell = let
    cue :: Score (Maybe StandardNote)
    cue = stretchTo 1 (rest |> a) 
    in parts' .~ solo tubularBells $ text "l.v." $ removeRests $ times 40 $ scat [times 3 $ scat [cue,rest], rest|*2]

strings :: Score StandardNote
strings = pcat [
    parts' .~ violins1     $ up (_P8^*1)   $ stringPart,
    parts' .~ violins2     $ up (_P8^*0)   $ stretch 2 stringPart,
    parts' .~ violas       $ down (_P8^*1) $ stretch 4 stringPart,
    parts' .~ cellos       $ down (_P8^*2) $ stretch 8 stringPart,
    parts' .~ doubleBasses $ down (_P8^*3) $ stretch 16 stringPart]
    where
        stringPart = delay (1/2) $ withTintin (down (_P8^*4) $ (a::Pitch)) $ mainSubject

music :: Score StandardNote
music = meta $ stretch (3/2) $ {-before 60-} (mempty <> bell <> delay 6 strings)
    where
        meta = id
          . title "Cantus in Memoriam Benjamin Britten" 
          . composer "Arvo Pärt" 
          . timeSignature (6/4) 
          . tempo (metronome (1/4) 120)

openBook :: Score StandardNote -> IO ()
openBook = openLilypond' LyScoreFormat

main :: IO ()
main = openBook music
