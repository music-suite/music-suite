
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, TypeFamilies, 
             DeriveDataTypeable #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
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
        doubleFlat, 
        flat, 
        natural, 
        sharp, 
        doubleSharp,

        -- ** Inspecting accidentals
        isNatural,
        isSharpened,
        isFlattened,
        isStandard,

        -- ** Name
        Name(..),

        -- * Pitch
        Pitch,    
        pitch,
        name,
        accidental,
        asPitch,
        middleC,
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
import Music.Pitch.Common.Name
import Music.Pitch.Common.Accidental
import Music.Pitch.Common.Quality
import Music.Pitch.Common.Number
import Music.Pitch.Common.Interval
import Music.Pitch.Common.Enharmonic

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
    toEnum = (c .+^) . perfect . fromIntegral
    fromEnum = fromIntegral . number . (.-. c)

-- |
-- This is just the identity function, but is useful to fix the type of 'Pitch'.
--
asPitch :: Pitch -> Pitch
asPitch = id

-- |
-- Creates a pitch from name accidental.
--
pitch :: Name -> Accidental -> Pitch
pitch name acc = Pitch $ interval' (fromIntegral acc) (fromEnum name + 1)

-- |
-- Returns the name of a pitch.
--
-- To convert a pitch to a numeric type, use 'octaves', 'steps' or 'semitones'
-- on the relevant interval type, for example:
--
-- @ semitones (a' .-. 'middleC') @
--
name :: Pitch -> Name
name = toEnum . fromIntegral . pred . number . simple . getPitch

-- |
-- Returns the accidental of a pitch.
--
-- See also 'octaves', and 'steps' and 'semitones'.
--
accidental :: Pitch -> Accidental
accidental = fromIntegral . intervalDiff . simple . getPitch

-- instance HasOctaves Pitch where
--     octaves = octaves . getPitch
-- 
-- instance HasSemitones Pitch where
--     semitones = semitones . getPitch
-- 
-- instance HasSteps Pitch where
--     steps = steps . getPitch

-- | The same as 'c', but fixed to 'Pitch'. This is useful if you want
--   to treat 'Pitch' as an affine space around middle C, that is /C4/ in Scientific Pitch Notation.
middleC :: Pitch
middleC = c

instance IsPitch Pitch where
    fromPitch (PitchL (c, a, o)) =
        Pitch $ interval' (qual a) (c + 1)
            ^+^
            (perfect octave^* fromIntegral (o - 4))
        where
            qual Nothing  = 0
            qual (Just n) = round n

-- midiNumber :: Pitch -> Integer
-- midiNumber = fromIntegral . semitones . getPitch

replicate' n = replicate (fromIntegral n)
