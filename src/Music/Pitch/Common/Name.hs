 
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

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
-- Provides standard pith names.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Common.Name (
        -- ** Name
        Name(..),
  ) where

instance Show Name where
    show C = "c"
    show D = "d"
    show E = "e"
    show F = "f"
    show G = "g"
    show A = "a"
    show B = "b"

data Name = C | D | E | F | G | A | B
    deriving (Eq, Ord, Enum)

