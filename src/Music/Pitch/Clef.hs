{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

-- | Clefs and staff positions.
module Music.Pitch.Clef
  ( -- * Staff lines
    StaffLines,
    HalfSpaces,
    ClefLine,

    -- * Clef representation
    ClefSymbol (..),
    ClefOctave,
    Clef (..),
    symbolName,
    symbolPitch,
    positionPitch,

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
    percussionClef,
  )
where

import Data.Typeable
import Music.Pitch.Common
import Music.Pitch.Literal

-- | Represents staff number relative middle. Staff zero is either the middle staff, or if using an
-- even number of lines, the staff below the middle space.
newtype StaffLines = StaffLines {getStaffLines :: Integer}
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Num,
      Real,
      Integral,
      Typeable
    )

-- | Represents the difference betwee  staff positions (often corresponding to one diatonic step).
newtype HalfSpaces = HalfSpaces {getHalfSpaces :: Integer}
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Num,
      Real,
      Integral,
      Typeable
    )

-- | Common clef symbols
data ClefSymbol = GClef | CClef | FClef | PercClef | NeutralClef
  deriving (Eq, Ord, Show, Typeable)

type ClefOctave = Integer

type ClefLine = StaffLines

newtype Clef = Clef {getClef :: (ClefSymbol, ClefOctave, ClefLine)}
  deriving (Eq, Ord, Typeable)

instance Show Clef where
  show x@(Clef a)
    | x == trebleClef = "trebleClef"
    | x == bassClef = "bassClef"
    | x == sopranoClef = "sopranoClef"
    | x == mezzoSopranoClef = "mezzoSopranoClef"
    | x == altoClef = "altoClef"
    | x == tenorClef = "tenorClef"
    | x == baritoneClef = "baritoneClef"
    | otherwise = show a

-- | Return the English name of the given clef.
symbolName :: ClefSymbol -> String
symbolName GClef = "G clef"
symbolName CClef = "C clef"
symbolName FClef = "F clef"
symbolName PercClef = "Percussion clef"
symbolName NeutralClef = "Neutral clef"

-- |  Return the pitch implied by the given clef at the middle space or line.
symbolPitch :: ClefSymbol -> Maybe Pitch
symbolPitch GClef = Just b'
symbolPitch CClef = Just c
symbolPitch FClef = Just d_
symbolPitch _ = Nothing

-- TODO also define the inverse of (positionPitch c) for any c
positionPitch :: Clef -> StaffLines -> Maybe Pitch
positionPitch (Clef (s, o, l)) x = fmap (upDiatonic relativePosition) referencePitch
  where
    referencePitch = symbolPitch s :: Maybe Pitch
    relativePosition = fromIntegral $ (x - l) + fromIntegral (o * 7)

-- TODO implement fully in Pitch.Common.Diatonic
upDiatonic :: Number -> Pitch -> Pitch
upDiatonic = upDiatonicP c . fromIntegral -- TODO Why c?

-- | Standard treble clef.
trebleClef :: Clef
trebleClef = Clef (GClef, -1 :: ClefOctave, -1 :: ClefLine)

-- |  Standard bass clef.
bassClef :: Clef
bassClef = Clef (FClef, 1 :: ClefOctave, 1 :: ClefLine)

-- |  Standard soprano clef.
sopranoClef :: Clef
sopranoClef = Clef (CClef, 0 :: ClefOctave, -2 :: ClefLine)

-- |  Standard mezzo soprano clef.
mezzoSopranoClef :: Clef
mezzoSopranoClef = Clef (CClef, 0 :: ClefOctave, -1 :: ClefLine)

-- |  Standard alto clef.
altoClef :: Clef
altoClef = Clef (CClef, 0 :: ClefOctave, 0 :: ClefLine)

-- |  Standard tenor clef.
tenorClef :: Clef
tenorClef = Clef (CClef, 0 :: ClefOctave, 1 :: ClefLine)

-- |  Standard baritone clef.
baritoneClef :: Clef
baritoneClef = Clef (CClef, 0 :: ClefOctave, 2 :: ClefLine)

-- |  Standard percussion clef.
percussionClef :: Clef
percussionClef = Clef (PercClef, 0 :: ClefOctave, 0 :: ClefLine)

-- | Is this a clef used in contemporary notation?
isModernClef :: Clef -> Bool
isModernClef x
  | x == trebleClef = True
  | x == bassClef = True
  | x == altoClef = True
  | x == tenorClef = True
  | otherwise = False

-- | Is this an historical clef?
isHistoricalClef :: Clef -> Bool
isHistoricalClef _ = False

-- | Is this a traditional voice clef, i.e. a C clef on some staff.
isVoiceClef :: Clef -> Bool
isVoiceClef x
  | x == altoClef = True
  | x == tenorClef = True
  | otherwise = False
