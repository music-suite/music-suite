
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    RankNTypes,
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
        performableToList,
  ) where

import Data.Foldable (Foldable)

import Music.Time.Pos
import Music.Time.Time
import Music.Time.Duration
import Music.Time.Delayable
import Music.Time.Stretchable

-- |
-- Performable values.
-- 
class Foldable s => Performable s where
    perform :: s a -> [(Pos (s a), Dur (s a), a)]

performableToList :: Performable s => s a -> [a]
performableToList = map trd3 . perform
    where
        trd3 (a,b,c) = c