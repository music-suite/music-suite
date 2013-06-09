
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

import Text.Pretty hiding (Mode)
import Music.Pitch.Literal

data Value = forall a . Show a => Value a

instance Show Value where
    show (Value a) = show a
instance Eq Value where
    a == b  = show a == show b

toValue :: Show a => a -> Value
toValue = Value



