
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, TypeFamilies, 
    FlexibleInstances, DeriveDataTypeable #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund, Edward Lilley 2012–2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides a standard pitch representation.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Common.Pitch (
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
        isStandard,

        -- ** Name
        Name(..),

        -- * Pitch
        Pitch,
        Pitch(..),
        
        mkPitch,
        name,
        accidental,
        asPitch
  ) where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.Typeable
import Control.Monad
import Control.Applicative
import qualified Data.Char as Char
import qualified Data.List as List

import Music.Pitch.Absolute
import Music.Pitch.Literal
import Music.Pitch.Alterable
import Music.Pitch.Augmentable
import Music.Pitch.Common.Interval
import Music.Pitch.Common.Semitones

-- |
-- An accidental is either flat, natural or sharp.
--
-- This representation allows for an arbitrary number of flats or sharps rather than just
-- single and double.
--
-- The 'Num' and 'Enum' instances treat 'Accidental' as the number of altered semitones, 
-- i.e. a double flat is @-2@, natural @0@ and so on.
--
newtype Accidental = Accidental { getAccidental :: Integer }
    deriving (Eq, Ord, Num, Enum, Real, Integral)
    
instance Show Accidental where
    show n | n == 0    = "natural"
           | n == 1    = "sharp"
           | n == (-1) = "flat"
           | n == 2    = "doubleSharp"
           | n == (-2) = "doubleFlat"
           | n > 0     = "sharp * " ++ show (getAccidental n)
           | n < 0     = "flat * " ++ show (negate $ getAccidental n)

instance Alterable Accidental where
    sharpen = succ
    flatten = pred

-- | 
-- Magic instance that allow us to write @c sharp@ instead of @sharpen c@.
--
instance (IsPitch a, Alterable a) => IsPitch (Accidental -> a) where
    fromPitch l 1       = sharpen (fromPitch l)
    fromPitch l (-1)    = flatten (fromPitch l)
-- Requires FlexibleInstances

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

-- | Returns whether this is a natural accidental.
isNatural   = (== 0)

-- | Returns whether this is a sharp, double sharp etc.
isSharpened = (> 0)

-- | Returns whether this is a flat, double flat etc.
isFlattened = (< 0)


-- | Returns whether this is a standard accidental, i.e.
--   either a double flat, flat, natural, sharp or double sharp.
isStandard :: Accidental -> Bool
isStandard a = abs a < 2


-- |
-- A pitch name.
--
data Name = C | D | E | F | G | A | B
    deriving (Eq, Ord, Show, Enum)

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
    deriving (Eq, Num, Ord, Typeable)
    
instance AffineSpace Pitch where
    type Diff Pitch     = Interval
    Pitch a .-. Pitch b = a ^-^ b
    Pitch a .+^ b       = Pitch (a ^+^ b)

instance Show Pitch where
    show p = showName (name p) ++ showAccidental (accidental p) ++ showOctave (octaves $ getPitch p)
        where        
            showName = fmap Char.toLower . show
            showOctave n
                | n > 0     = replicate' n '\''
                | otherwise = replicate' (negate n) '_'
            showAccidental n
                | n > 0     = replicate' n 's'
                | otherwise = replicate' (negate n) 'b'

instance Alterable Pitch where
    sharpen (Pitch a) = Pitch (augment a)
    flatten (Pitch a) = Pitch (diminish a)

instance Enum Pitch where
    toEnum = Pitch . mkInterval' 0 . fromIntegral
    fromEnum = fromIntegral . pred . number . (.-. c)

-- |
-- This is just the identity function, but is useful to fix the type of 'Pitch'.
--
asPitch :: Pitch -> Pitch
asPitch = id

-- |
-- Creates a pitch from name accidental.
--
mkPitch :: Name -> Accidental -> Pitch
mkPitch name acc = Pitch $ mkInterval' (fromIntegral acc) (fromEnum name)

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

-- | The same as 'c', but fixed to 'Pitch'. This is useful if you want
--   to treat 'Pitch' as an affine space around middle C, that is /C4/ in Scientific Pitch Notation.
middleC :: Pitch
middleC = c

instance IsPitch Pitch where
    fromPitch (PitchL (c, a, o)) =
        Pitch $ mkInterval' (qual a) c
            ^+^
            (perfect octave^* fromIntegral o)
        where
            qual Nothing  = 0
            qual (Just n) = round n

-- midiNumber :: Pitch -> Integer
-- midiNumber = fromIntegral . semitones . getPitch

replicate' n = replicate (fromIntegral n)
