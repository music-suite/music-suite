
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
-------------------------------------------------------------------------------------

module Music.Pitch.Augmentable (
        -- * Augmentable class
        Augmentable(..),
  ) where

-- |
-- Class of types that can be augmented.
--
class Augmentable a where

    -- | 
    -- Increase the size of this interval by one.
    --
    augment :: a -> a

    -- | 
    -- Decrease the size of this interval by one.
    --
    diminish :: a -> a

