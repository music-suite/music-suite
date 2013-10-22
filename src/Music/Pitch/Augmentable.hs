
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

import Data.Ratio

-- |
-- Class of types that can be augmented.
--
-- > quality (augment a)  = augment (quality a)
-- > quality (diminish a) = diminish (quality a)
-- > augment . diminish   = id
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

instance Augmentable Double where
    augment  = (+ 1)
    diminish = (subtract 1)

instance Augmentable Integer where
    augment  = (+ 1)
    diminish = (subtract 1)

instance Integral a => Augmentable (Ratio a) where
    augment  = (+ 1)
    diminish = (subtract 1)
