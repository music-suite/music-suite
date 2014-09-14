
{-
Great idea!

Generalize voice and score like this:
-}

type VoiceScore a = VS LocalOn LocalOff [(Duration, a)]

VS 1 0 ... = Voice ... -- standard sequential composition
VS 0 1 ... = Voice ... -- backwards sequential composition

VS 0 0 ... = Chord ... -- chord composing to the left
VS 0.5 0.5 ... = Chord ... -- chord composing in the middle
VS 1 1 ... = Chord ... -- chord composing to the right

{-
Could be implemented as Track/TPTM!

How?

-}