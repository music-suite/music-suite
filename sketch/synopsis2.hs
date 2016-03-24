newtype ChromaticSteps = ChromaticSteps { getChromaticSteps :: Int }
newtype DiatonicSteps = DiatonicSteps { getDiatonicSteps :: Int }
newtype Octaves = Octaves { getOctaves :: Int }
newtype Number = Number { getNumber :: Int } -- TODO int!
data Quality
  = Major
  | Minor
  | Perfect
  | Augmented Integer
  | Diminished Integer
data QualityType = PerfectType | MajorMinorType
newtype Accidental = Accidental { getAccidental :: Integer }
data Name = C | D | E | F | G | A | B
data IntervalBasis = Chromatic | Diatonic
-- Or: V2 Int!
newtype Interval = Interval { getInterval :: (ChromaticSteps, DiatonicSteps) }

-- TODO we can consolidate the octave represention for various reasons:
-- * It is OK that octaves are represented as doubles, as the maximum integral value we ever need is very small.
-- * Also octaves are not used in actual pitch representation, so no cause for confusion.
newtype Hertz = Hertz { getHertz :: Double }
newtype Octaves = Octaves { getOctaves :: Hertz }
newtype Fifths = Fifths { getFifths :: Hertz }
newtype Cents = Cents { getCents :: Hertz }

newtype Equal a = Equal { getEqual :: Int }
type PitchClass    = Semitones `Mod` 12
type IntervalClass = Semitones `Mod` 6


newtype StaffLines = StaffLines { getStaffLines :: Integer }
newtype HalfSpaces = HalfSpaces { getHalfSpaces :: Integer }
data ClefSymbol = GClef | CClef | FClef | PercClef | NeutralClef
type ClefOctave = Integer
type ClefLine   = StaffLines
newtype Clef = Clef { getClef :: (ClefSymbol, ClefOctave, ClefLine) }


newtype Ambitus a = Ambitus { getAmbitus :: (I.Interval a) }
data Mode a = Mode [Diff a] (Diff a) -- intervals, repeat (usually octave)
data Scale a = Scale a (Mode a)      -- root, mode

-- TODO melody, ornaments, chords, chord seqs, leading notes etc

-- TODO rewrite these using VS/linear
newtype Tuning a     = Tuning { getTuning :: a -> Double } -- contravariant!
newtype Intonation p = Intonation { getIntonation :: p -> Hertz }

newtype Instrument = String
newtype Division = Division { getDivision :: (Int, Int) }
data Solo
    = Solo
    | Tutti
    deriving (Eq, Show, Ord, Enum)
newtype Subpart = Subpart [Division]
-- | A part is a subdivided group of instruments of a given type.
data Part = Part
  Solo        -- Solo vs. tutti
  Instrument  -- Type of instrument
  Subpart     -- Subdivision within instrument chorus
  -- TODO Layer
    deriving (Eq, Ord)
