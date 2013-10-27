 
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

-- |
-- A pitch name.
--
data Name = C | D | E | F | G | A | B
    deriving (Eq, Ord, Show, Enum)

