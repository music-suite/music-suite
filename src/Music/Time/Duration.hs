
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

module Music.Time.Duration (
        module Music.Time.Transform,

        -- * The HasDuration class
        HasDuration(..),

        -- * Absolute duration
        duration,
        stretchTo,
  ) where

import           Music.Time.Transform

import           Control.Lens         hiding (Indexable, Level, above, below,
                                       index, inside, parts, reversed,
                                       transform, (<|), (|>))
import           Data.NumInstances    ()
import           Data.Semigroup       hiding ()
import           Data.VectorSpace     hiding (Sum (..))
import           Data.Functor.Contravariant

-- |
-- Class of values that have a duration.
--
-- Law Duration
--
-- @
-- '_duration' x = ('offset' x '.-.' 'onset' x)
-- @
--
class HasDuration a where
  _duration :: a -> Duration

instance HasDuration Time where
  _duration = 0

instance HasDuration Duration where
  _duration = id

instance HasDuration Span where
  _duration = snd . view delta

--
-- By convention, we treat pairs and triplets as having the form
-- (t,x), (d,x) and (t,d,x) where t has a position and d has a
-- duration. This makes it convenient to represent simple event
-- lists as [(Time, Duration, a)] without needing any special
-- structure.
--

instance HasDuration a => HasDuration (a, b) where
  _duration (d,_) = _duration d

instance HasDuration b => HasDuration (a, b, c) where
  _duration (_,d,_) = _duration d

instance HasDuration a => HasDuration (Product a) where
  _duration (Product x) = _duration x

instance HasDuration a => HasDuration (Sum a) where
  _duration (Sum x) = _duration x

instance HasDuration a => HasDuration (Min a) where
  _duration (Min x) = _duration x

instance HasDuration a => HasDuration (Max a) where
  _duration (Max x) = _duration x

-- For HasDuration [a] we assume parallel composition and
-- use the HasPosition instance, see Music.Time.Position.

instance (HasDuration a, HasDuration b) => HasDuration (Either a b) where
  _duration (Left x)  = _duration x
  _duration (Right x) = _duration x

-- |
-- Access the duration.
--
duration :: (Transformable a, HasDuration a) => Lens' a Duration
duration = lens _duration (flip stretchTo)
{-# INLINE duration #-}

-- |
-- Stretch a value to have the given duration.
--
stretchTo :: (Transformable a, HasDuration a) => Duration -> a -> a
stretchTo d x = (d ^/ _duration x) `stretch` x
{-# INLINE stretchTo #-}

{-

-- TODO more general pattern here
withDurationR :: (Functor f, HasDuration a) => f a -> f (Duration, a)
withDurationR = fmap $ \x -> (_duration x, x)

withDurationL :: (Contravariant f, HasDuration a) => f (Duration, a) -> f a
withDurationL = contramap $ \x -> (_duration x, x)

mapWithDuration :: HasDuration a => (Duration -> a -> b) -> a -> b
mapWithDuration = over dual withDurationL . uncurry
  where
    dual :: Iso (a -> b) (c -> d) (Op b a) (Op d c)
    dual = iso Op getOp

-}


