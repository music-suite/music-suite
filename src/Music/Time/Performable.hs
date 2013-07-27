
{-# LANGUAGE
    TypeFamilies,
    ConstraintKinds,
    FlexibleContexts,
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
        -- * Prerequisites
        Monoid'(..),

        Transformable(..),

        -- * The 'Performable' class
        Performable(..),

        -- * The 'Composable' class
        Composable(..),
        
        foldMapDefault
  ) where

import Data.Foldable (Foldable(..))

import Data.Semigroup
import Data.AffineSpace
import Data.AffineSpace.Point
import Music.Time.Time
import Music.Time.Delayable
import Music.Time.Stretchable

-- |
-- This pseudo-class can be used in place of 'Monoid' whenever an additional 'Semigroup'
-- constraint is needed.
--
-- Ideally, 'Monoid' should be changed to extend 'Semigroup' instead.
--
type Monoid' a         =  (Monoid a, Semigroup a)

type Transformable a   =  (Stretchable a, Delayable a, AffineSpace (Time a))

-- |
-- Composable values.
--
-- Minimal complete definition: 'note'.
-- 
class (Monoid a, Transformable a) => Composable a where
    note    :: Event a -> a
    event   :: Time a -> Duration a -> Event a -> a
    compose :: [(Time a, Duration a, Event a)] -> a
    -- Given Num (Duration a) we have
    -- note a        = compose [(origin, 1, a)]
    event t d x   = (delay (t .-. origin) . stretch d) (note x)
    compose       = mconcat . fmap (uncurry3 event)

-- |
-- Performable values.
--
-- Minimal complete definition: 'perform'.
-- 
class Performable a where

    -- | Perform a score.
    --
    -- This is the inverse of 'compose'
    --
    perform :: a -> [(Time a, Duration a, Event a)]
    events  :: a -> [Event a]
    events = fmap trd3 . perform
        
-- | This function may be used as a value for 'foldMap' in a 'Foldable' instance. 
foldMapDefault :: (Monoid c, Performable a) => (Event a -> c) -> a -> c
foldMapDefault f = foldMap f . events


untrip (a,b,c) = ((a,b),c)
uncurry3 = (. untrip) . uncurry . uncurry
trd3 (a, b, c) = c