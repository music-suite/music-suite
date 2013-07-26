
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
        foldMapDefault,
  ) where

import Data.Foldable (Foldable(..))

import Data.Semigroup
import Music.Time.Pos
import Music.Time.Delayable
import Music.Time.Stretchable

-- |
-- Performable values.
--
-- Minimal complete definition: 'perform'.
-- 
class Foldable s => Performable s where

    -- | Perform a score.
    --
    -- This is the inverse of 'compose'
    --
    perform :: (t ~ Time s, d ~ Duration s) => s a -> [(t, d, a)]

    -- | Perform a score, yielding an ordered list of values.
    --
    -- Equivalent to 'Foldable.toList', but may be more efficient.
    --
    toList' :: s a -> [a]
    toList' = map trd3 . perform
        where
            trd3 (a,b,c) = c
        
-- | This function may be used as a value for 'foldMap' in a 'Foldable' instance. 
foldMapDefault :: (Performable s, Monoid m) => (a -> m) -> s a -> m
foldMapDefault f = foldMap f . toList'