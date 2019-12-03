
{-# LANGUAGE TypeFamilies #-}

import Music.Prelude
import Control.Lens (_1, over)
import qualified Music.Score as Score

markIf :: (HasColor a, HasPitches' a, Score.Pitch a ~ Pitch) => (Interval -> Bool) -> Score a -> Score a
markIf p     = mapIf (\x -> p $ withOrigin c $ x ^?! pitches) mark
  where
    mark         = colorRed
    mapIf p f    = uncurry mplus . over _1 f . mpartition p
    withOrigin x = (.-. x)

markPerfect   = text "Perfect consonances"   . markIf isPerfectConsonance
markImperfect = text "Imperfect consonances" . markIf isImperfectConsonance
markDiss      = text "Dissonances"           . markIf isDissonance

-- Try different subjects:
subject = fmap fromPitch [c..c']
-- subject = [c,d,cs,gs,f,fs,g_,gs_,fs,f,e,ds',c]

music :: Music
music = rcat [
    markPerfect   $ scat subject,
    markImperfect $ scat subject,
    markDiss      $ scat subject
  ]

main = pure ()
