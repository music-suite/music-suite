{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : GHC
module Data.Music.Lilypond.Value
  ( Value,
    toValue,
    toLiteralValue,
  )
where

import Data.String
import Text.Pretty hiding (Mode)

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
data Value = forall a. Show a => Value a | Literal String

instance IsString Value where
  fromString = toValue

instance Num Value where

  (+) = noOverloading "(+)"

  (*) = noOverloading "(*)"

  (-) = noOverloading "(-)"

  negate = noOverloading "negate"

  abs = noOverloading "abs"

  signum = noOverloading "signum"

  fromInteger = toValue

instance Fractional Value where

  (/) = noOverloading "(/)"

  recip = noOverloading "recip"

  fromRational = (toValue . toDouble)

instance Show Value where
  show (Value a) = show a
  show (Literal x) = x

instance Eq Value where
  a == b = show a == show b

instance Pretty Value where
  pretty (Value x) = (string . show) x
  pretty (Literal x) = string x

toValue :: Show a => a -> Value
toValue = Value

-- | As 'toValue', but not quoting strings. Handy for scheme literals such as @#red@.
toLiteralValue :: String -> Value
toLiteralValue = Literal

noOverloading :: String -> a
noOverloading n = error $ "No overloading of " ++ n ++ " for Value"

toDouble :: Rational -> Double
toDouble = fromRational
