


data    Mode a     = Mode [Diff a] (Diff a)         -- intervals, repeat (usually octave)
data    Scale a    = Scale a (Mode a)              -- root, mode
data    Function a = Function [Diff a] (Diff a) -- intervals, repeat, repeat (usually octave)
data    Chord a    = Chord a (Function a)          -- root, function 
data    Ambitus a  = Ambitus a a | Ambitus a (Diff a) | Ambitus (Diff a) a

newtype ChromaticSteps = ChromaticSteps { getChromaticSteps :: Integer }
newtype DiatonicSteps = DiatonicSteps { getDiatonicSteps :: Integer }
newtype Octaves = Octaves { getOctaves :: Integer }
type    Semitones = ChromaticSteps
newtype Number = Number { getNumber :: Integer }
data    Quality = Major | Minor | Perfect | Augmented Integer | Diminished Integer
data    QualityType = PerfectType | MajorMinorType
newtype Accidental = Accidental { getAccidental :: Integer }
data    Name = C | D | E | F | G | A | B
data    IntervalBasis = Chromatic | Diatonic
newtype Interval = Interval { getInterval :: (ChromaticSteps, DiatonicSteps) }
newtype Pitch = Pitch { getPitch :: Interval }

