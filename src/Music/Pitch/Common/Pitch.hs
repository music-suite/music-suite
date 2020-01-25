{-# LANGUAGE FlexibleContexts #-}

-- | Common pitch.
module Music.Pitch.Common.Pitch
  ( -- * Accidentals
    Accidental,
    natural,
    flat,
    sharp,
    doubleFlat,
    doubleSharp,

    -- ** Inspecting accidentals
    isNatural,
    isFlattened,
    isSharpened,
    isStandardAccidental,

    -- ** Name
    Name (..),

    -- * Pitch
    Pitch,
    mkPitch,
    name,
    accidental,

    -- ** Diatonic and chromatic pitch
    upDiatonicP,
    downDiatonicP,
    upChromaticP,
    downChromaticP,
    invertDiatonicallyP,
    invertChromaticallyP,
  )
where

import Control.Applicative
import Control.Lens hiding (simple)
import Control.Monad
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson
import Data.AffineSpace
import Data.AffineSpace.Point
import qualified Data.Char as Char
import Data.Either
import Data.Fixed (Fixed (..), HasResolution (..))
import qualified Data.List as List
import Data.Maybe
import Data.Ratio
import Data.Semigroup
import Data.Typeable
import Data.VectorSpace
import Music.Pitch.Absolute
import Music.Pitch.Alterable
import Music.Pitch.Augmentable
import Music.Pitch.Common.Interval
import Music.Pitch.Common.Number
import Music.Pitch.Common.Semitones
import Music.Pitch.Common.Types
import Music.Pitch.Literal
import Music.Time.Transform (Transformable(..))

sharp, flat, natural, doubleFlat, doubleSharp :: Accidental

-- | The double sharp accidental.
doubleSharp = 2

-- | The sharp accidental.
sharp = 1

-- | The natural accidental.
natural = 0

-- | The flat accidental.
flat = -1

-- | The double flat accidental.
doubleFlat = -2

isNatural, isSharpened, isFlattened :: Accidental -> Bool

-- | Returns whether this is a natural accidental.
isNatural = (== 0)

-- | Returns whether this is a sharp, double sharp etc.
isSharpened = (> 0)

-- | Returns whether this is a flat, double flat etc.
isFlattened = (< 0)

-- | Returns whether this is a standard accidental, i.e.
--   either a double flat, flat, natural, sharp or double sharp.
isStandardAccidental :: Accidental -> Bool
isStandardAccidental a = abs a < 2

-- was: isStandard

-- instance IsPitch Pitch where
--   fromPitch (PitchL (c, a, o)) =
--     Pitch $ (\a b -> (fromIntegral a, fromIntegral b)^.interval') (qual a) c ^+^ (_P8^* fromIntegral o)
--     where
--       qual Nothing  = 0
--       qual (Just n) = round n

instance Enum Pitch where

  toEnum = Pitch . (\a b -> (fromIntegral a, fromIntegral b) ^. interval') 0 . fromIntegral

  fromEnum = fromIntegral . pred . number . (.-. middleC)

instance Alterable Pitch where

  sharpen (Pitch a) = Pitch (augment a)

  flatten (Pitch a) = Pitch (diminish a)

instance Show Pitch where
  show p = showName (name p) ++ showAccidental (accidental p) ++ showOctave (octaves $ getPitch p)
    where
      showName = fmap Char.toLower . show
      showOctave n
        | n > 0 = replicate (fromIntegral n) '\''
        | otherwise = replicate (negate $ fromIntegral n) '_'
      showAccidental n
        | n > 0 = replicate (fromIntegral n) 's'
        | otherwise = replicate (negate $ fromIntegral n) 'b'

instance Num Pitch where

  Pitch a + Pitch b = Pitch (a + b)

  negate (Pitch a) = Pitch (negate a)

  abs (Pitch a) = Pitch (abs a)

  (*) = error "Music.Pitch.Common.Pitch: no overloading for (*)"

  signum = error "Music.Pitch.Common.Pitch: no overloading for signum"

  fromInteger = toEnum . fromInteger

instance ToJSON Pitch where
  toJSON = toJSON . (.-. middleC)

instance IsPitch Int where
  fromPitch x = fromIntegral (fromPitch x :: Integer)

instance IsPitch Word where
  fromPitch x = fromIntegral (fromPitch x :: Integer)

instance IsPitch Float where
  fromPitch x = realToFrac (fromPitch x :: Double)

instance HasResolution a => IsPitch (Fixed a) where
  fromPitch x = realToFrac (fromPitch x :: Double)

instance Integral a => IsPitch (Ratio a) where
  fromPitch x = realToFrac (fromPitch x :: Double)

instance IsPitch Double where
  fromPitch p = fromIntegral . semitones $ (p .-. c)

instance IsPitch Integer where
  fromPitch p = fromIntegral . semitones $ (p .-. c)

instance IsPitch Pitch where
  fromPitch = id

-- TODO bootstrapping, remove when/if possible
middleC = c

instance FromJSON Pitch where
  parseJSON = fmap (middleC .+^) . parseJSON

instance Transformable Pitch where
  transform _ = id

-- |
-- Creates a pitch from name accidental.
mkPitch :: Name -> Accidental -> Pitch
mkPitch name acc = Pitch $ (\a b -> (fromIntegral a, fromIntegral b) ^. interval') (fromIntegral acc) (fromEnum name)

-- TODO name
-- TODO use this to define pitch-class equivalence
toFirstOctave :: Pitch -> Pitch
toFirstOctave p = case (name p, accidental p) of
  (n, a) -> mkPitch n a

-- |
-- Returns the name of a pitch.
--
-- To convert a pitch to a numeric type, use 'octaves', 'steps' or 'semitones'
-- on the relevant interval type, for example:
--
-- @
-- semitones ('a\'' .-. 'c')
-- @
name :: Pitch -> Name
name x
  | i == 7 = toEnum 0 -- Arises for flat C etc.
  | 0 <= i && i <= 6 = toEnum i
  | otherwise = error $ "Pitch.name: Bad value " ++ show i
  where
    i = (fromIntegral . pred . number . simple . getPitch) x

-- |
-- Returns the accidental of a pitch.
--
-- See also 'octaves', and 'steps' and 'semitones'.
accidental :: Pitch -> Accidental
accidental = fromIntegral . intervalDiff . simple . getPitch
  where
    intervalDiff = view (from interval' . _1)

upChromaticP :: Pitch -> ChromaticSteps -> Pitch -> Pitch
upChromaticP origin n = relative origin $ (_alteration +~ n)

downChromaticP :: Pitch -> ChromaticSteps -> Pitch -> Pitch
downChromaticP origin n = relative origin $ (_alteration -~ n)

upDiatonicP :: Pitch -> DiatonicSteps -> Pitch -> Pitch
upDiatonicP origin n = relative origin $ (_steps +~ n)

downDiatonicP :: Pitch -> DiatonicSteps -> Pitch -> Pitch
downDiatonicP origin n = relative origin $ (_steps -~ n)

invertDiatonicallyP :: Pitch -> Pitch -> Pitch
invertDiatonicallyP origin = relative origin $ (_steps %~ negate)

invertChromaticallyP :: Pitch -> Pitch -> Pitch
invertChromaticallyP origin = relative origin $ (_alteration %~ negate)
