
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, TypeFamilies, FlexibleInstances #-}

module Music.Pitch.Relative.Pitch where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Control.Monad
import Control.Applicative
import Music.Pitch.Absolute hiding (Octaves(..), octaves)
import Music.Pitch.Literal
import qualified Data.List as List

import Music.Pitch.Relative.Interval
import Music.Pitch.Relative.Quality
import Music.Pitch.Relative.Accidental
import Music.Pitch.Relative.Semitones
import Music.Pitch.Relative.Name








-- |
-- Standard pitch representation.
-- 
-- Intervals and pitches can be added using '.+^'. To get the interval between
-- two pitches, use '.-.'. 
--
-- Notes with accidentals can be written by adding the `s` or `b` suffices
-- (or two for double sharps and flats).
-- 
-- > cs, ds, es ...    -- sharp
-- > cb, db, eb ...    -- flat
-- > css, dss, ess ... -- double sharp
-- > cbb, dbb, ebb ... -- double flat
-- 
-- There is also a convenience syntax for entering pitches one octave up or
-- down, using `'` and `_` respectively.
-- 
-- > g a b c'
-- > d c b_ c
-- 
-- Because of some overloading magic, we can actually write `sharp` and
-- `flat` as /postfix/ functions. This gives a better read:
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
deriving instance Eq Pitch	 
deriving instance Num Pitch   
deriving instance Ord Pitch	 
instance AffineSpace Pitch where
    type Diff Pitch = Interval
    Pitch a .-. Pitch b = a ^-^ b
    Pitch a .+^ b       = Pitch (a ^+^ b)
instance Show Pitch where
    show p = show (name p) ++ showAccidental (accidental p) ++ showOctave (octaves p)
        where
            showOctave n 
                | n > 0     = replicate' n '\''
                | otherwise = replicate' (negate n) '_'
            showAccidental n 
                | n > 0     = replicate' n 's'
                | otherwise = replicate' (negate n) 'b'

instance Alterable Pitch where
    sharpen (Pitch a) = Pitch (augment a)
    flatten (Pitch a) = Pitch (diminish a)

asPitch :: Pitch -> Pitch
asPitch = id

-- |
-- Creates a pitch from name accidental. 
--
pitch :: Name -> Accidental -> Pitch
pitch = undefined

-- | 
-- Returns the name of a pitch.
-- 
-- See also 'octaves', and 'steps' and 'semitones'.
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

instance HasOctaves Pitch where
    octaves = octaves . getPitch

instance HasSemitones Pitch where
    semitones = semitones . getPitch

instance HasSteps Pitch where
    steps = steps . getPitch


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

instance (IsPitch a, Alterable a) => IsPitch (Accidental -> a) where
    fromPitch l acc
        | acc == sharp  = sharpen (fromPitch l)
        | acc == flat   = flatten (fromPitch l)

instance IsPitch Pitch where
    fromPitch (PitchL (c, a, o)) = 
        Pitch $ interval' (qual a) (c + 1) 
            ^+^ 
            (_P8^* fromIntegral (o - 4))
        where
            qual Nothing  = 0
            qual (Just n) = round n

midiNumber :: Pitch -> Integer
midiNumber = getSemitones . semitones . getPitch    