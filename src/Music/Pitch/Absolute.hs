
{-# LANGUAGE
    GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides overloaded pitch literals.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Absolute -- (
-- )
where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Control.Monad
import Control.Applicative

-- | Frequency in Hertz    
newtype Frequency = Frequency { getFrequency :: Double }
    deriving ( Show, Eq, Enum, Num, Ord, Fractional, Floating )


-- | Logarithmic pitch reprentation.
--
-- > convert (f * 2) = convert f + Octaves 1    
newtype Octaves = Octaves { getOctaves :: Frequency }
    deriving ( Show, Eq, Enum, Num, Ord, Fractional, Floating )

-- | Logarithmic pitch reprentation.
--
-- > convert (f * 2) = convert f + Octave 1    
newtype Fifths = Fifths { getFifths :: Frequency }
    deriving ( Show, Eq, Enum, Num, Ord, Fractional, Floating )

-- | Logarithmic pitch reprentation.    
--
-- > convert (f * 2) = convert f + Cent 1200    
newtype Cents = Cents { getCents :: Frequency }
    deriving ( Show, Eq, Enum, Num, Ord, Fractional, Floating )

class HasFrequence a where
    frequency :: a -> Frequency

-- instance Convert Frequency Octave where
--     convert f             =  Octave (logBase 2 f)
--     reconvert (Octave f)  =  2 ** f
-- 
-- instance Convert Cent Octave where
--     convert (Cent f)      =  Octave (f / 1200)
--     reconvert (Octave f)  =  Cent   (f * 1200)
-- 
-- instance Convert Frequency Cent where
--     convert f             =  Cent   (logBase 2 f * 1200)
--     reconvert (Cent f)    =  2 ** (f / 1200)
-- 
-- instance Convert Octave Frequency where
--     convert = reconvert
--     reconvert = convert
-- 
-- instance Convert Octave Cent where
--     convert = reconvert
--     reconvert = convert
-- 
-- instance Convert Cent Frequency where
--     convert = reconvert
--     reconvert = convert
    

cents :: HasFrequence a => a -> Cents
cents = undefined 

fifths :: HasFrequence a => a -> Fifths
fifths = undefined 

octaves :: HasFrequence a => a -> Octaves
octaves = undefined 

-- unitFrequency :: Frequency
-- unitFrequency = 1

-- unitOctave :: Octave
-- unitOctave = Octave 0

-- unitCent :: Cent
-- unitCent = Cent 0

                             