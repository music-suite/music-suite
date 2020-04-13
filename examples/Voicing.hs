{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
import Music.Prelude

main = defaultMain music

dM   = chord d majorTriad
dm   = chord d minorTriad
a7   = chord a dominantSeventhChord
e7b5 = chord e halfDiminishedChord
eFS  = chord e frenchSixthChord

music = set parts' violins $ inspectableToMusic @[Voiced Chord Interval Pitch] $ mconcat
  [ fsProgression
  , dmProgression
  , dmProgression1
  , messiaenProg
  , messiaenProg1
  ]

-- Treat Messiaen's modes as chords.
-- Does not sound too pleasant in close voicing.
messiaenProg :: [Voiced Chord Interval Pitch]
messiaenProg =
  [ voiceIn 10 (chord d $ modeToChordType firstMode)
  , voiceIn 10 (chord d $ modeToChordType secondMode)
  , voiceIn 10 (chord d $ modeToChordType thirdMode)
  , voiceIn 10 (chord d $ modeToChordType fourthMode)
  , voiceIn 10 (chord d $ modeToChordType fifthMode)
  , voiceIn 10 (chord d $ modeToChordType sixthMode)
  , voiceIn 10 (chord d $ modeToChordType seventhMode)
  ]

-- But in open voicings they sound great.
messiaenProg1 :: [Voiced Chord Interval Pitch]
messiaenProg1 =
  -- [ openVoicing1 (chord d $ modeToChordType firstMode)
  -- , openVoicing1 (chord d $ modeToChordType secondMode)
  -- , openVoicing1 (chord d $ modeToChordType thirdMode)
  -- , openVoicing1 (chord d $ modeToChordType fourthMode)
  -- , openVoicing1 (chord d $ modeToChordType fifthMode)
  -- , openVoicing1 (chord d $ modeToChordType sixthMode)
  -- , openVoicing1 (chord d $ modeToChordType seventhMode)

  [ openVoicing1a (chord d $ modeToChordType firstMode)
  , openVoicing1a (chord d $ modeToChordType secondMode)
  , openVoicing1a (chord d $ modeToChordType thirdMode)
  , openVoicing1a (chord d $ modeToChordType fourthMode)
  , openVoicing1a (chord d $ modeToChordType fifthMode)
  , openVoicing1a (chord d $ modeToChordType sixthMode)
  , openVoicing1a (chord d $ modeToChordType seventhMode)

  , openVoicing2 (chord d_ $ modeToChordType firstMode)
  , openVoicing2 (chord d_ $ modeToChordType secondMode)
  , openVoicing2 (chord d_ $ modeToChordType thirdMode)
  , openVoicing2 (chord d_ $ modeToChordType fourthMode)
  , openVoicing2 (chord d_ $ modeToChordType fifthMode)
  , openVoicing2 (chord d_ $ modeToChordType sixthMode)
  , openVoicing2 (chord d_ $ modeToChordType seventhMode)

  , openVoicing3 (chord d_ $ modeToChordType firstMode)
  , openVoicing3 (chord d_ $ modeToChordType secondMode)
  , openVoicing3 (chord d_ $ modeToChordType thirdMode)
  , openVoicing3 (chord d_ $ modeToChordType fourthMode)
  , openVoicing3 (chord d_ $ modeToChordType fifthMode)
  , openVoicing3 (chord d_ $ modeToChordType sixthMode)
  , openVoicing3 (chord d_ $ modeToChordType seventhMode)

  , openVoicing4 (chord d_ $ modeToChordType firstMode)
  , openVoicing4 (chord d_ $ modeToChordType secondMode)
  , openVoicing4 (chord d_ $ modeToChordType thirdMode)
  , openVoicing4 (chord d_ $ modeToChordType fourthMode)
  , openVoicing4 (chord d_ $ modeToChordType fifthMode)
  , openVoicing4 (chord d_ $ modeToChordType sixthMode)
  , openVoicing4 (chord d_ $ modeToChordType seventhMode)
  ]

openVoicing1  = (`Voiced` [0,2,4,5,7])
openVoicing1a = (`Voiced` [0,2,4,8,10,11,13,15,17,19])
openVoicing2  = (`Voiced` [0,3,6,8,11,12,15,20,21,22])
openVoicing3  = (`Voiced` [0,4..25])
openVoicing4  = (`Voiced` [0,6,11,15,18,20,21,22,23])

-- Closed voicing
fsProgression :: [Voiced Chord Interval Pitch]
fsProgression =
  [ voiceIn 4 dm
  , voiceIn 4 eFS
  , voiceIn 4 a7
  , voiceIn 4 dM
  ]

-- Closed voicing with >4 parts
dmProgression :: [Voiced Chord Interval Pitch]
dmProgression =
  [ Voiced dm   [0,1..5]
  , Voiced e7b5 [0..5]
  , Voiced a7   [0,1..5]
  , Voiced dm   [0,1..6]
  ]

{- TODO consider doubling and leaving out notes when working with a limited set of
   pitches and voices.

   For example in a 3-note chord such as dm, an full voicing should contain
   at least one number n satisfying 3n+0, one satisfying 3n+1 and one 3n+2.
   In classical harmony out the firth (3n+2) is sometimes acceptable.
   TODO invertVoicing preserves such "complete" voicings.

    [0,1,2,3] (closed) is good
    [0,3,5,6] is bad because we're missing the 3rd: there's no 3n+1
 -}
dmProgression1 :: [Voiced Chord Interval Pitch]
dmProgression1 = _8vb $
  [ Voiced dm   [0,3,5,7]
  , Voiced e7b5 [0,5,2,3]
  , Voiced a7   [0,2,3,5]
  , Voiced dm   [0,3,4,6]
  ]

-- TODO voicings sound "better/fuller" when the intervals get larger (and more
-- consonant) towards the bottom

-- TODO voice leading concerns when dealing with chord sequences. For example in
-- classical harmony:
--
-- * Dissonant notes (relative the root) in a dominanth chords (e.g. 7th, 9th)
--   are resolved downwards.
--
-- * Leading tone resolves upwards

-- TODO "drop" voicings

