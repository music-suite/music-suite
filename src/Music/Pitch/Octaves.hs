
{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

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

module Music.Pitch.Octaves (
        -- * IsOctave class
        IsOctave(..),
  ) where

import Data.VectorSpace

-- |
-- Class of types that can be augmented.
--
class IsOctave a where

    -- | 
    -- Precisely one octave
    -- 
    fromOctave :: a

    -- | 
    -- The given number of octaves.
    -- 
    fromOctaves :: (VectorSpace a, Scalar a ~ Integer) => Integer -> a
    fromOctaves n = fromOctave^*n