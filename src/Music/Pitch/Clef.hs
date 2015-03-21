
-- | Clefs and staff positions.
module Music.Pitch.Clef
(
      -- * Clef representation
      ClefSymbol(..),
      ClefOctave,
      Clef,
      symbolName,
      symbolPitch,
      positionPitch,
      pitchPosition,

      -- ** Properties
      isModernClef,
      isHistoricalClef,
      isVoiceClef,

      -- * Standard clefs
      trebleClef,
      bassClef,
      sopranoClef,
      mezzoSopranoClef,
      altoClef,
      tenorClef,
      baritoneClef,
) where

import Data.Typeable

import Music.Pitch.Common
import Music.Pitch.Literal

-- | Represents staff number relative middle. Staff zero is either the middle staff, or if using an
-- even number of lines, the staff below the middle space.
--
newtype StaffLines = StaffLines { getStaffLines :: Integer }
  deriving (Eq, Ord, Read, Show, Enum,
            Num, Real, Integral, Typeable)

-- | Represents the difference betwee  staff positions (often corresponding to one diatonic step).
newtype HalfSpaces = HalfSpaces { getHalfSpaces :: Integer }
  deriving (Eq, Ord, Read, Show, Enum,
            Num, Real, Integral, Typeable)

-- | Common clef symbols
data ClefSymbol = GClef | CClef | FClef | PercClef | NeutralClef
    deriving (Eq, Ord, Show, Typeable)

type ClefOctave = Integer
type ClefLine   = StaffLines

type Clef = (ClefSymbol, ClefOctave, ClefLine)

-- | Return the English name of the given clef.
symbolName :: ClefSymbol -> String
symbolName GClef = "G clef"
symbolName CClef = "C clef"
symbolName FClef = "F clef"
symbolName PercClef = "Percussion clef"
symbolName NeutralClef = "Neutral clef"

-- | Return the pitch implied by the given clef at the middle space or line.
symbolPitch :: ClefSymbol -> Maybe Pitch
symbolPitch GClef = Just b'
symbolPitch CClef = Just c
symbolPitch FClef = Just d_
symbolPitch _     = Nothing

-- TODO consolidate with common
pitchPosition :: Clef -> Pitch -> Maybe StaffLines
pitchPosition (s,o,l) x = undefined
  where
    numbersPerOctave = 7
    referencePitch = symbolPitch s :: Maybe Pitch

positionPitch :: Clef -> StaffLines -> Maybe Pitch
positionPitch (s,o,l) x = fmap (upDiatonic relativePosition) referencePitch
  where
    numbersPerOctave = 7
    referencePitch = symbolPitch s :: Maybe Pitch
    relativePosition = fromIntegral $ (x - l) + fromIntegral (o*7)

-- TODO implement fully in Pitch.Common.Diatonic
upDiatonic :: Number -> Pitch -> Pitch
upDiatonic = upDiatonicP c . fromIntegral -- TODO Why c?

{-
TODO

IsPitch instance?
  What to do with the non-standard pitches, i.e.
  we know what (g :: ClefSymbol) is, but what about (cs :: ClefSymbol)?
  I think 1) error or 2) default to g.

Map this to Pitch.Common
  Do we make this module 1) depend on Pitch.Common or 2) the other way around?
  If 1), do we need to separate G/C/F in ClefSymbol, maybe just put a single constructor for common pitch.
  In that case, maybe a function isStandardClefPitch (i.e. c/f/g) would be appropriate.
-}

-- | Standard treble clef.
trebleClef :: Clef
-- | Standard bass clef.
bassClef :: Clef
-- | Standard soprano clef.
sopranoClef :: Clef
-- | Standard mezzo soprano clef.
mezzoSopranoClef :: Clef
-- | Standard alto clef.
altoClef :: Clef
-- | Standard tenor clef.
tenorClef :: Clef
-- | Standard baritone clef.
baritoneClef :: Clef
trebleClef        = (GClef, -1 :: ClefOctave, -1 :: ClefLine)
bassClef          = (FClef, 1  :: ClefOctave, -1 :: ClefLine)
sopranoClef       = (CClef, 0  :: ClefOctave, -2 :: ClefLine)
mezzoSopranoClef  = (CClef, 0  :: ClefOctave, -1 :: ClefLine)
altoClef          = (CClef, 0  :: ClefOctave, 0  :: ClefLine)
tenorClef         = (CClef, 0  :: ClefOctave, 1  :: ClefLine)
baritoneClef      = (CClef, 0  :: ClefOctave, 2  :: ClefLine)

-- | Is this a clef used in contemporary notation?
isModernClef :: Clef -> Bool
isModernClef x | x == trebleClef  = True
isModernClef x | x == bassClef    = True
isModernClef x | x == altoClef    = True
isModernClef x | x == tenorClef   = True
isModernClef x | otherwise        = False

-- | Is this an historical clef?
isHistoricalClef :: Clef -> Bool
isHistoricalClef _ = False

-- | Is this a traditional voice clef, i.e. a C clef on some staff.
isVoiceClef :: Clef -> Bool
isVoiceClef x | x == altoClef    = True
isVoiceClef x | x == tenorClef   = True
isVoiceClef x | otherwise        = False


