
-- | Common pitch.
module Music.Pitch.Common.Pitch
(
        -- * Accidentals
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
        Name(..),

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
) where

import           Control.Applicative
import           Control.Monad
import           Control.Lens hiding (simple)
import           Data.AffineSpace
import           Data.AffineSpace.Point
import qualified Data.Char                    as Char
import           Data.Either
import qualified Data.List                    as List
import           Data.Maybe
import           Data.Semigroup
import           Data.Typeable
import           Data.VectorSpace

import           Music.Pitch.Absolute
import           Music.Pitch.Alterable
import           Music.Pitch.Augmentable
import           Music.Pitch.Common.Types
import           Music.Pitch.Common.Number
import           Music.Pitch.Common.Interval
import           Music.Pitch.Common.Semitones
import           Music.Pitch.Literal


sharp, flat, natural, doubleFlat, doubleSharp :: Accidental

-- | The double sharp accidental.
doubleSharp = 2

-- | The sharp accidental.
sharp       = 1

-- | The natural accidental.
natural     = 0

-- | The flat accidental.
flat        = -1

-- | The double flat accidental.
doubleFlat  = -2

isNatural, isSharpened, isFlattened :: Accidental -> Bool

-- | Returns whether this is a natural accidental.
isNatural   = (== 0)

-- | Returns whether this is a sharp, double sharp etc.
isSharpened = (> 0)

-- | Returns whether this is a flat, double flat etc.
isFlattened = (< 0)


-- | Returns whether this is a standard accidental, i.e.
--   either a double flat, flat, natural, sharp or double sharp.
isStandardAccidental :: Accidental -> Bool
isStandardAccidental a = abs a < 2
-- was: isStandard

-- |
-- Common pitch representation.
--
-- Intervals and pitches can be added using '.+^'. To get the interval between
-- two pitches, use '.-.'.
--
-- Pitches are normally entered using the following literals.
--
-- > c d e f g a b
--
-- Notes with accidentals can be written by adding the @s@ or @b@ suffices
-- (or two for double sharps and flats).
--
-- > cs, ds, es ...    -- sharp
-- > cb, db, eb ...    -- flat
-- > css, dss, ess ... -- double sharp
-- > cbb, dbb, ebb ... -- double flat
--
-- There is also a convenience syntax for entering pitches one octave up or
-- down, using @'@ and @_@ respectively.
--
-- > g a b c'
-- > d c b_ c
--
-- Because of some overloading magic, we can actually write @sharp@ and
-- @flat@ as /postfix/ functions. This gives a better read:
--
-- > cs == c sharp
-- > db == c flat
--
-- You can of course use typical functional transformation of pitch as well.
-- For example 'sharpen' and 'flatten' are the ordinary (prefix) versions of
-- 'sharp' and 'flat'
--
-- > sharpen c             == c sharp       == cs
-- > flatten d             == d flat        == ds
-- > (sharpen . sharpen) c == c doubleSharp == css
-- > (flatten . flatten) d == d doubleFlat  == dss
--
-- Note that there is no guarantee that your pitch representation use
-- enharmonic equivalence, so @cs == db@ may or may not hold.
--
-- > c .+^ minor third == eb
-- > f .-. c           == perfect fourth
--
-- Pitches are described by name, accidental and octave number.
--
-- > c   == fromIntegral 0
-- > _P4 == perfect fourth   == interval Perfect 5
-- > d5  == diminished fifth == diminish (perfect fifth)
--
newtype Pitch = Pitch { getPitch :: Interval }
  deriving (Eq, Ord, Typeable)

instance IsPitch Pitch where
  fromPitch (PitchL (c, a, o)) =
    Pitch $ (\a b -> (fromIntegral a, fromIntegral b)^.interval') (qual a) c ^+^ (_P8^* fromIntegral o)
    where
      qual Nothing  = 0
      qual (Just n) = round n

instance Enum Pitch where
  toEnum = Pitch . (\a b -> (fromIntegral a, fromIntegral b)^.interval') 0 . fromIntegral
  fromEnum = fromIntegral . pred . number . (.-. c)

instance Alterable Pitch where
  sharpen (Pitch a) = Pitch (augment a)
  flatten (Pitch a) = Pitch (diminish a)

instance Show Pitch where
  show p = showName (name p) ++ showAccidental (accidental p) ++ showOctave (octaves $ getPitch p)
    where
      showName = fmap Char.toLower . show
      showOctave n
        | n > 0     = replicate (fromIntegral n) '\''
        | otherwise = replicate (negate $ fromIntegral n) '_'
      showAccidental n
        | n > 0     = replicate (fromIntegral n) 's'
        | otherwise = replicate (negate $ fromIntegral n) 'b'

instance Num Pitch where
  Pitch a + Pitch b = Pitch (a + b)
  negate (Pitch a)  = Pitch (negate a)
  abs (Pitch a)     = Pitch (abs a)
  (*)           = error  "Music.Pitch.Common.Pitch: no overloading for (*)"
  signum        = error "Music.Pitch.Common.Pitch: no overloading for signum"
  fromInteger   = toEnum . fromInteger

instance AffineSpace Pitch where
  type Diff Pitch     = Interval
  Pitch a .-. Pitch b = a ^-^ b
  Pitch a .+^ b       = Pitch (a ^+^ b)

-- |
-- Creates a pitch from name accidental.
--
mkPitch :: Name -> Accidental -> Pitch
mkPitch name acc = Pitch $ (\a b -> (fromIntegral a, fromIntegral b)^.interval') (fromIntegral acc) (fromEnum name)

-- |
-- Returns the name of a pitch.
--
-- To convert a pitch to a numeric type, use 'octaves', 'steps' or 'semitones'
-- on the relevant interval type, for example:
--
-- @
-- semitones ('a\'' .-. 'c')
-- @
--
name :: Pitch -> Name
name x
  | i == 7           = toEnum 0 -- Arises for flat C etc.
  | 0 <= i && i <= 6 = toEnum i
  | otherwise        = error $ "Pitch.name: Bad value " ++ show i
  where
    i = (fromIntegral . pred . number . simple . getPitch) x

-- |
-- Returns the accidental of a pitch.
--
-- See also 'octaves', and 'steps' and 'semitones'.
--
accidental :: Pitch -> Accidental
accidental = fromIntegral . intervalDiff . simple . getPitch
  where
    intervalDiff = view (from interval'._1)

-- |
-- This is just the identity function, but is useful to fix the type of 'Pitch'.
--
asPitch :: Pitch -> Pitch
asPitch = id

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


