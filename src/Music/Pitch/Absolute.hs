
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

module Music.Pitch.Absolute (
        Hertz(..),
        Octaves(..),
        Cents(..),
        Fifths(..),
        HasFrequency(..),
        octaves,
        fifths,
        cents,
  ) where

import Data.Maybe
import Data.Either
import Data.Semigroup
import Control.Monad
import Control.Applicative

-- | 
-- Absolute frequency in Hertz.    
newtype Hertz = Hertz { getHertz :: Double }
    deriving ( Show, Eq, Enum, Num, Ord, Fractional, Floating )

-- | 
-- Number of pure octaves.
--
-- Octaves are a logarithmic representation of frequency such that
--
-- > frequency f * (2/1) = octaves f + 1    
newtype Octaves = Octaves { getOctaves :: Hertz }
    deriving ( Show, Eq, Enum, Num, Ord, Fractional, Floating )

-- | 
-- Number of pure octaves.
--
-- Cents are a logarithmic representation of frequency such that
--
-- > frequency f * (2/1) = cents f + 1200    
newtype Cents = Cents { getCents :: Hertz }
    deriving ( Show, Eq, Enum, Num, Ord, Fractional, Floating )

-- | 
-- Number of pure fifths.
--
-- Fifths are a logarithmic representation of frequency.
--
-- > frequency f * (3/2) = fifths f + 1    
newtype Fifths = Fifths { getFifths :: Hertz }
    deriving ( Show, Eq, Enum, Num, Ord, Fractional, Floating )


instance Semigroup Hertz    where (<>) = (*)
instance Semigroup Octaves  where (<>) = (+)
instance Semigroup Fifths   where (<>) = (+)
instance Semigroup Cents    where (<>) = (+)

instance Monoid Hertz       where { mempty  = 1 ; mappend = (*) }
instance Monoid Octaves     where { mempty  = 0 ; mappend = (+) }
instance Monoid Fifths      where { mempty  = 0 ; mappend = (+) }
instance Monoid Cents       where { mempty  = 0 ; mappend = (+) }

class HasFrequency a where
    frequency :: a -> Hertz

instance HasFrequency Hertz where
    frequency = id

instance HasFrequency Octaves where
    frequency (Octaves f)  = (2/1) ** f

instance HasFrequency Fifths where
    frequency (Fifths f)   =  (3/2) ** f

instance HasFrequency Cents where
    frequency (Cents f)    =  (2/1) ** (f / 1200)

octaves :: HasFrequency a => a -> Octaves
octaves a = Octaves $ logBase (2/1) (frequency a)

fifths :: HasFrequency a => a -> Fifths
fifths a = Fifths $ logBase (3/2) (frequency a)

cents :: HasFrequency a => a -> Cents
cents a = Cents $ logBase (2/1) (frequency a) * 1200


                             