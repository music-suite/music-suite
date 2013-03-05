
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Music.MusicXml.Pitch (

        Pitch(..),
        DisplayPitch(..),
        PitchClass(..),
        Semitones(..),
        noSemitones,

        Octaves(..),
        Fifths(..),
        Line(..),

        Accidental(..)

  ) where

type Pitch        = (PitchClass, Maybe Semitones, Octaves)
type DisplayPitch = (PitchClass, Octaves)

data Accidental   = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
data PitchClass   = C | D | E | F | G | A | B

newtype Semitones = Semitones { getSemitones :: Double }    -- ^ Semitones, i.e 100 cent
newtype Octaves   = Octaves { getOctaves :: Int }           -- ^ Octaves, i.e. 1200 cent
newtype Fifths    = Fifths { getFifths :: Int }             -- ^ Number of fifths upwards relative to C (i.e. F is -1, G is 1)
newtype Line      = Line { getLine :: Int }                 -- ^ Line number, from bottom (i.e. 1-5)


deriving instance Eq   PitchClass
deriving instance Ord  PitchClass
deriving instance Enum PitchClass
deriving instance Show PitchClass

deriving instance Eq   Accidental
deriving instance Ord  Accidental
deriving instance Enum Accidental

deriving instance Eq   Semitones
deriving instance Ord  Semitones
deriving instance Num  Semitones
deriving instance Enum Semitones
deriving instance Fractional Semitones

deriving instance Eq   Octaves
deriving instance Ord  Octaves
deriving instance Num  Octaves
deriving instance Enum Octaves
deriving instance Real Octaves
deriving instance Integral Octaves

deriving instance Eq   Fifths
deriving instance Ord  Fifths
deriving instance Num  Fifths
deriving instance Enum Fifths

deriving instance Eq   Line
deriving instance Ord  Line
deriving instance Num  Line
deriving instance Enum Line



noSemitones :: Maybe Semitones
noSemitones = Nothing
