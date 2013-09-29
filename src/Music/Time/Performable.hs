
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
-- Provides performable values. To 'perform' a score is to render it as a list
-- of time-duration-value triplets.
--
-- The 'compose' function captures the fact that a value that is a 'Monoid',
-- 'Transformable' and 'Pointed' can be reconstructed from a performance, i.e.
-- 'compose' is the inverse of 'perform'.
--
-------------------------------------------------------------------------------------

module Music.Time.Performable (
        -- * Event and container types
        Event(..),
        Container(..),

        -- * HasPoint class
        -- $hasPoint
        HasPoint(..),
        point',

        -- * Performable values
        Performable(..),
        performValues,

        -- * Composable values
        Composable(..),

        -- ** Default implementations
        memptyDefault,
        mappendDefault,
        foldMapDefault
  ) where

import Data.Foldable (Foldable(..))

import Data.Pointed
import Data.Semigroup
import Data.Monoid.WithSemigroup
import Data.AffineSpace
import Data.AffineSpace.Point
import Music.Time.Time
import Music.Time.Delayable
import Music.Time.Stretchable
import Music.Time.Juxtapose

-- |
-- This type function returns the value type for a given type.
--
type family Event (s :: *) :: *

-- |
-- This type function returns the container type for a given type.
--
type family Container (s :: *) :: * -> *

-- $hasPoint
-- 
-- The 'HasPoint' class serves only to lift the 'Pointed' class into
-- unary kinds. We could have used 'Applicative' or 'Monad' here, but
-- as we only care about 'pure' and 'return', 'Pointed' makes more sense.
--
-- Recall that:
--
-- > pure = return = point
--

type HasPoint a = (a ~ (Container a) (Event a), Pointed (Container a))

-- |
-- Equivalent to 'point', 'pure' and 'return'.
--
point' :: HasPoint a => Event a -> a
point' = point

-- |
-- Performable values.
--
-- Minimal complete definition: 'perform'.
-- 
class Performable a where

    -- |
    -- Convert a score to a list of timed values.
    --
    -- The returned list /must/ be sorted by comparing time.
    --
    perform :: a -> [(Time, Duration, Event a)]

-- |
-- Return just the values of the score.
--
performValues  :: Performable a => a -> [Event a]
performValues = fmap trd3 . perform        
    
-- |
-- Composable values.
--
-- As one might expect
-- 
-- > compose . perform = id
-- > perform . compose = id
--
-- That is 'compose' is the inverse of 'perform'.
--
-- The methods in this class have a default implementation in terms of its
-- superclasses, but are provided so that implementors may provide more
-- efficient implementations.
--
class (Monoid' a, Transformable a, HasPoint a) => Composable a where

    -- |
    -- Creates a score with the given position and duration.
    --
    -- The default definition can be overridden for efficiency.
    --
    event   :: Time -> Duration -> Event a -> a

    -- |
    -- Creates a score from a list of timed values.
    --
    -- The given list need not be sorted.
    --
    -- The default definition can be overridden for efficiency.
    --
    compose :: [(Time, Duration, Event a)] -> a

    event t d x   = (delay (t .-. origin) . stretch d) (point x)
    compose       = mconcat . fmap (uncurry3 event)

-- | 
-- This function may be used as a value for 'mempty' in a 'Monoid' instance. 
-- 
memptyDefault :: Composable a => a
memptyDefault = compose mempty

-- | 
-- This function may be used as a value for 'mappend' in a 'Monoid' instance. 
-- 
mappendDefault :: (Composable a, Performable a) => a -> a -> a
a `mappendDefault` b = compose $ perform a `mappend` perform b
        
-- | 
-- This function may be used as a value for 'foldMap' in a 'Foldable' instance. 
-- 
foldMapDefault :: (Monoid c, Performable a) => (Event a -> c) -> a -> c
foldMapDefault f = foldMap f . performValues


untrip (a,b,c) = ((a,b),c)
uncurry3 = (. untrip) . uncurry . uncurry
trd3 (a, b, c) = c

