

-- mathes if the score has no overlapping notes
-- TODO is the matched MVoice allowed to begin ord end with a rest?
scoreDelayedMVoice :: Prism' (Score a) (Delayed (MVoice a))


-- mathes if the score has no overlapping notes and no rests inbetween notes
-- (i.e. all the notes "curtails"? each other)
scoreDelayedVoice :: Prism' (Score a) (Delayed (Voice a))
-- Combined with a prism to find identity delays (i.e. delay = 0):
scoreVoice :: Prism' (Score a) (Voice a)


-- Assume (Chord a) is the fancy new context-senitive parallel composition

-- All notes overlap each other
-- XXX no intuition for this
-- What about extracting a wholly simultaneous chord (i.e. all notes ahve the same era)
-- What about extracting a partially simultaneous chord (i.e. all notes have the same onset or offset)
-- What about extracting a chord where all notes overlap some point?
scoreStretchedChord :: Iso' (Score a) (Chord (Stretched a))
-- Combined with a prism to find identity stretchers (i.e. stretch = 1):
scoreChord :: Iso' (Score a) (Chord a)
