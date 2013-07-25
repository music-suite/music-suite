
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

module Music.Pitch.Relative.Accidental (
        -- * Alterable class
        Alterable(..),

        -- * Accidentals
        Accidental,
        doubleFlat, 
        flat, 
        natural, 
        sharp, 
        doubleSharp,
  ) where

import Music.Pitch.Literal

-- |
-- Class of things that can be altered.
--
class Alterable a where
    -- | 
    -- Increase the given pitch by one.
    -- 
    sharpen :: a -> a

    -- | 
    -- Decrease the given pitch by one.
    -- 
    flatten :: a -> a

newtype Accidental = Accidental { getAccidental :: Integer }
    deriving (Eq, Ord, Num, Enum, Real, Integral)
    
instance Show Accidental where
    show n | n == 0 = "natural"
           | n > 0  = replicate' n 's'
           | n < 0  = replicate' (negate n) 'b'

instance Alterable Accidental where
    sharpen = succ
    flatten = pred

instance Alterable Double where
    sharpen = (+ 1)
    flatten = (subtract 1)

instance Alterable Integer where
    sharpen = (+ 1)
    flatten = (subtract 1)

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


replicate' n = replicate (fromIntegral n)

