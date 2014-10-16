
module Music.Pitch.Ambitus where

import Data.Interval hiding (Interval, interval)
import qualified Data.Interval as Interval

-- | An ambitus is a closed interval (in the mathematical sense).
--
--   Also known as /range/ or /tessitura/, this type can be used to restrict the
--   range of a melody, chord or other pitch container.
--
--   It is also used in "Music.Parts" to represent the range of instruments.
--
type Ambitus a = Interval.Interval a

