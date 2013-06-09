
{-# LANGUAGE 
    OverloadedStrings,
    ExistentialQuantification
    #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : GHC
--
-------------------------------------------------------------------------------------

module Music.Lilypond.Value (
        Value,
        toValue,
  ) where

import Data.String
import Text.Pretty hiding (Mode)
import Music.Pitch.Literal

-- | 
-- Value of a @\\set@ command.
-- These are simply wrappers for showable things.
--
-- For example use (with @OverloadedStrings@)
--
-- > Set "Staff.instrumentName" "Violin I"
-- > Set "Staff.instrumentName" 2
--
-- to generate
--
-- > \set Staff.instrumentName = "Violin I"
-- > \set Staff.instrumentName = 2
--
data Value = forall a . Show a => Value a

instance IsString Value where
    fromString = toValue

instance Num Value where
    (+)     = noOverloading "(+)"
    (*)     = noOverloading "(*)"
    (-)     = noOverloading "(-)"
    negate  = noOverloading "negate"
    abs     = noOverloading "abs"
    signum  = noOverloading "signum"
    fromInteger = toValue

instance Fractional Value where
    (/)     = noOverloading "(/)"
    recip   = noOverloading "recip"
    fromRational = (toValue . toDouble)

instance Show Value where
    show (Value a) = show a

instance Eq Value where
    a == b  = show a == show b

toValue :: Show a => a -> Value
toValue = Value



noOverloading :: String -> a
noOverloading n = error $ "No overloading of " ++ n ++ " for Value"

toDouble :: Rational -> Double
toDouble = fromRational
