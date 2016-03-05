
{-
  Annotate a melody with intervals.

  Written by Carlo Nucera
-}
import Music.Prelude

main :: IO ()
main = openLilypond . showAnnotations' ""
     . intervalAnnotations subjectDiff
     . scat $ map (fromPitch'') subject

subject :: [Pitch]
subject = [c, d, f, e, f, g, a, g, e, d, c]

subjectDiff :: [Interval]
subjectDiff = zipWith (.-.) (tail subject) subject

intervalAnnotations :: [Interval] -> (Score StandardNote -> Score StandardNote)
intervalAnnotations = foldr1 (.) . zipWith notate (map spanify [0..])
  where
    spanify :: Duration -> Span
    spanify t = (0 .+^ t) >-> 1

    notate :: Span -> Interval -> (Score StandardNote -> Score StandardNote)
    notate s n = annotateSpan s ("       " ++ showIntervalName n)
 
    showIntervalName = filter (/= '_') . show

