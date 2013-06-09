
module Music.Lilypond.Pitch (
        Pitch(..),
        PitchClass(..),
        Accidental(..),
        Octaves(..),
  ) where


newtype Pitch = Pitch { getPitch :: (PitchClass, Accidental, Octaves) }
    deriving (Eq, Ord, Show)

data PitchClass = C | D | E | F | G | A | B
    deriving (Eq, Ord, Show, Enum)

-- | For double flat -2, flat -1, natural 0, sharp 1 and double sharp 2.
type Accidental = Int 

-- | Number of octaves raised (positive) or flattened (negative).
type Octaves    = Int 
