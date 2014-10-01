
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

module Music.Time.Stretched (
        -- * Stretched type
        Stretched,
        -- * Construction
        stretched,
        stretchee,
  ) where

import           Control.Applicative
import           Control.Lens           hiding (Indexable, Level, above, below,
                                         index, inside, parts, reversed,
                                         transform, (<|), (|>))
import           Data.Foldable          (Foldable)
import qualified Data.Foldable          as Foldable
import           Data.String
import           Data.Typeable
import           Data.VectorSpace
import           Data.Bifunctor
import           Data.Functor.Couple

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Time.Reverse
import           Music.Time.Split


-- |
-- A 'Stretched' value has a known 'duration', but no 'position'.
--
-- Placing a value inside 'Stretched' makes it invariant under 'delay', however the inner
-- value can still be delayed using @'fmap' 'delay'@.
--
newtype Stretched a = Stretched { _stretchee :: Couple Duration a }
  deriving (
    Eq,           Num,      Fractional,   Floating,
    Ord,          Real,     RealFrac,     Functor,
    Applicative,  Monad,    Foldable,     Traversable, Typeable
    )
            -- Comonad,

instance Wrapped (Stretched a) where
  type Unwrapped (Stretched a) = (Duration, a)
  _Wrapped' = iso (getCouple . _stretchee) (Stretched . Couple)

instance Rewrapped (Stretched a) (Stretched b)

-- $semantics Stretched
--
-- @
-- type Stretched = (Duration, a)
-- @
--

-- >>> stretch 2 $ (5,1)^.stretched
-- (10,1)^.stretched
--
-- >>> delay 2 $ (5,1)^.stretched
-- (5,1)^.stretched
--
instance Transformable (Stretched a) where
  transform t = over _Wrapped $ first (transform t)

instance Reversible (Stretched a) where
  rev = stretch (-1)

instance Splittable a => Splittable (Stretched a) where
  beginning d = over _Wrapped $ \(s, v) -> (beginning d s, beginning d v)
  ending    d = over _Wrapped $ \(s, v) -> (ending    d s, ending    d v)

instance HasDuration (Stretched a) where
  _duration = _duration . fst . view _Wrapped

instance IsString a => IsString (Stretched a) where
  fromString = pure . fromString

instance IsPitch a => IsPitch (Stretched a) where
  fromPitch = pure . fromPitch

instance IsInterval a => IsInterval (Stretched a) where
  fromInterval = pure . fromInterval

instance IsDynamics a => IsDynamics (Stretched a) where
  fromDynamics = pure . fromDynamics

instance (Show a, Transformable a) => Show (Stretched a) where
  show x = show (x^.from stretched) ++ "^.stretched"

-- |
-- View a stretched value as a pair of the original value and a stretch factor.
--
stretched :: Iso (Duration, a) (Duration, b) (Stretched a) (Stretched b)
stretched = _Unwrapped

-- |
-- View a stretched value as a pair of the original value and the transformation (and vice versa).
--
stretchee :: (Transformable a, Transformable b) => Lens (Stretched a) (Stretched b) a b
stretchee = lens runStretched (flip $ mapStretched . const)
  where
    mapStretched f (Stretched (Couple (d, x))) = Stretched (Couple (d, f `whilst` stretching d $ x))
    runStretched = uncurry stretch . view _Wrapped

