
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

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

module Music.Time.Delayed (
        -- * Delayed type
        Delayed,

        -- * Construction
        delayed,
        delayedValue,
  ) where

import           Control.Applicative
import           Control.Lens            hiding (Indexable, Level, above, below,
                                          index, inside, parts, reversed,
                                          transform, (<|), (|>))
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Bifunctor
import           Data.Foldable           (Foldable)
import qualified Data.Foldable           as Foldable
import           Data.Functor.Adjunction (unzipR)
import           Data.Functor.Couple
import           Data.String
import           Data.Typeable
import           Data.VectorSpace

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Time.Reverse
import           Music.Time.Split


-- |
-- 'Delayed' represents a value with an offset in time.
--
-- A delayed value has a known 'position', but no 'duration'.
--
-- Placing a value inside 'Delayed' does not make it invariant under 'stretch', as the
-- offset of a delayed value may be stretched with respect to the origin. However, in
-- contrast to a note the /duration/ is not stretched.
--
newtype Delayed a = Delayed   { _delayedValue :: (Time, a) }
  deriving (Eq,
            Ord,
            Functor,
            Applicative,
            Monad,
            -- Comonad,
            Foldable,
            Traversable,
            Typeable)

-- $semantics Delayed
--
-- @
-- type Delayed a = (Time, a)
-- @
--

instance (Show a, Transformable a) => Show (Delayed a) where
  show x = show (x^.from delayed) ++ "^.delayed"

instance Wrapped (Delayed a) where
  type Unwrapped (Delayed a) = (Time, a)
  _Wrapped' = iso _delayedValue Delayed

instance Rewrapped (Delayed a) (Delayed b)

instance Transformable (Delayed a) where
  transform t = over _Wrapped $ first (transform t)

instance HasDuration (Delayed a) where
  _duration x = _offset x .-. _onset x

instance HasPosition (Delayed a) where
  x `_position` p = fst (view _Wrapped x) `_position` p

instance Reversible (Delayed a) where
  rev = revDefault

instance Splittable a => Splittable (Delayed a) where
  -- TODO is this right?
  split t = unzipR . fmap (split t)

-- Lifted instances

instance IsString a => IsString (Delayed a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Delayed a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Delayed a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Delayed a) where
  fromDynamics = pure . fromDynamics

-- |
-- View a delayed value as a pair of a the original value and a delay time.
--
delayed :: Iso (Time, a) (Time, b) (Delayed a) (Delayed b)
delayed = _Unwrapped


-- |
-- View a delayed value as a pair of the original value and the transformation (and vice versa).
--
delayedValue :: (Transformable a, Transformable b)
  => Lens
      (Delayed a) (Delayed b)
      a b
delayedValue = lens runDelayed (flip $ _delayed . const)
  where
    _delayed f (Delayed (t,x)) = Delayed (t, f `whilst` delaying (t .-. 0) $ x)
{-# INLINE delayedValue #-}


runDelayed :: Transformable a => Delayed a -> a
runDelayed = uncurry delayTime . view _Wrapped
  where
    delayTime t = delay (t .-. 0)


