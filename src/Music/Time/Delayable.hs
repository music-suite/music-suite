
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleContexts,
    ConstraintKinds,
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

module Music.Time.Delayable (
        Delayable(..),
        move,
        moveBack,
  ) where

import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace

import Music.Time.Time

-- |
-- Delayable values. 
-- 
class Delayable a where

    -- |
    -- Delay a value.
    -- > Duration -> Score a -> Score a
    -- 
    delay :: Duration a -> a -> a


-- |
-- Move a score forward in time. Equivalent to 'delay'.
--
-- > Duration -> Score a -> Score a
--
move            :: (Delayable a, d ~ Duration a) =>
                d -> a -> a

-- |
-- Move a score backward in time. Negated verison of 'delay'
--
-- > Duration -> Score a -> Score a
--
moveBack        :: (Delayable a, AdditiveGroup d, d ~ Duration a) =>
                d -> a -> a

move            = delay
moveBack t      = delay (negateV t)
