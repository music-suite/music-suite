
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

module Music.Pitch.Alterable (
        -- * Alterable class
        Alterable(..),
        alter,
  ) where

import Data.Ratio

-- |
-- Class of things that can be altered.
--
-- > accidental (sharpen a) = sharpen (accidental a)
-- > accidental (flatten a) = flatten (accidental a)
-- > sharpen . flatten      = id
--
class Alterable a where
    -- | 
    -- Increase the given pitch by one.
    -- 
    sharpen :: a -> a

    -- | 
    -- Decrease the given pitch by one.
    -- 
    flatten :: a -> a


instance Alterable a => Alterable (b -> a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable Double where
    sharpen = (+ 1)
    flatten = (subtract 1)

instance Alterable Integer where
    sharpen = (+ 1)
    flatten = (subtract 1)

instance Integral a => Alterable (Ratio a) where
    sharpen = (+ 1)
    flatten = (subtract 1)

instance Alterable a => Alterable [a] where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (b, a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

{-
sharpened :: Alterable a => Iso' a a
sharpened = iso sharpen flatten

flattened :: Alterable a => Iso' a a
flattened = iso flatten sharpen
-}

alter :: Alterable a => Int -> a -> a
alter n x
  | n < 0  = iterate flatten x !! n
  | n == 0 = x
  | n > 0  = iterate sharpen x !! n
