
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    GeneralizedNewtypeDeriving #-} 

-------------------------------------------------------------------------------------
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

module Music.Time.Performable (
        Performable(..),
  ) where

import Music.Time.Time
import Music.Time.Duration
import Music.Time.Delayable
import Music.Time.Stretchable

-- |
-- Performable values.
-- 
class Performable s where
    perform :: s a -> [(Time, Duration, a)]
    perform = undefined

