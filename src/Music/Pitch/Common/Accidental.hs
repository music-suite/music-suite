
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleInstances #-}

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
-- Provides standard accidentals.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Common.Accidental (
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
  ) where

import Music.Pitch.Literal
import Music.Pitch.Alterable

-- |
-- An accidental is either flat, natural or sharp.
--
-- This representation allows for an arbitrary number of flats or sharps rather than just
-- single (the ♯ and ♭ symbols) and double (i.e. the /x/ and ♭♭ symbols).
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
-- Requires @FlexibleInstances@.
--
instance (IsPitch a, Alterable a) => IsPitch (Accidental -> a) where
    fromPitch l acc
        | acc == sharp  = sharpen (fromPitch l)
        | acc == flat   = flatten (fromPitch l)

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
--   either a double flat, flat, natural, sharp or doubl sharp.
isStandard :: Accidental -> Bool
isStandard a = abs a < 2


replicate' n = replicate (fromIntegral n)
