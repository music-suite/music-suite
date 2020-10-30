{-# OPTIONS_GHC
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides overloaded dynamic literals.
module Music.Dynamics.Literal
  ( -- * IsDynamics class
    DynamicsL (..),
    IsDynamics (..),

    -- * Literal values

    -- ** Standard dynamics
    pppppp,
    ppppp,
    pppp,
    ppp,
    pp,
    _p,
    mp,
    mf,
    _f,
    ff,
    fff,
    ffff,
    fffff,
    ffffff,
  )
where

import Control.Applicative
import Data.Fixed
import Data.Functor.Couple
import Data.Ratio
import Data.Semigroup

-- |
-- Dynamics literal.
--
-- First value is start value, second is end value.
--
-- * @(x, Nothing)@ is a constant dynamic of @x@
-- * @(x, Just y)@ is a dynamic varying from @x@ to @y@
--
-- For levels, we use @-0.5@ for /mp/, @0.5@ for /mf/ and add or remove one for each level.
-- @0@ is an unspecified middle level dynamic.
newtype DynamicsL = DynamicsL {getDynamicsL :: (Double, Maybe Double)}
  deriving (Eq, Show, Ord)

-- Like Num can be expressed using arabic numerals, instances
-- of IsDynamics can be expressed using Western pitch names (c, c sharp, c flat etc)
class IsDynamics a where
  fromDynamics :: DynamicsL -> a

instance IsDynamics DynamicsL where
  fromDynamics = id

instance IsDynamics a => IsDynamics (Maybe a) where
  fromDynamics = pure . fromDynamics

instance (Monoid b, IsDynamics a) => IsDynamics (b, a) where
  fromDynamics = pure . fromDynamics

deriving instance (Monoid b, IsDynamics a) => IsDynamics (Couple b a)

instance IsDynamics a => IsDynamics [a] where
  fromDynamics = pure . fromDynamics

instance IsDynamics Float where
  fromDynamics x = realToFrac (fromDynamics x :: Double)

instance HasResolution a => IsDynamics (Fixed a) where
  fromDynamics x = realToFrac (fromDynamics x :: Double)

instance Integral a => IsDynamics (Ratio a) where
  fromDynamics x = realToFrac (fromDynamics x :: Double)

instance IsDynamics Double where
  fromDynamics (DynamicsL (x, _)) = x

pppppp, ppppp, pppp, ppp, pp, _p, mp, mf, _f, ff, fff, ffff, fffff, ffffff :: IsDynamics a => a


pppppp = fromDynamics $ DynamicsL (-6.5, Nothing)

ppppp = fromDynamics $ DynamicsL (-5.5, Nothing)

pppp = fromDynamics $ DynamicsL (-4.5, Nothing)

ppp = fromDynamics $ DynamicsL (-3.5, Nothing)

pp = fromDynamics $ DynamicsL (-2.5, Nothing)

_p = fromDynamics $ DynamicsL (-1.5, Nothing)

mp = fromDynamics $ DynamicsL (-0.5, Nothing)

mf = fromDynamics $ DynamicsL (0.5, Nothing)

_f = fromDynamics $ DynamicsL (1.5, Nothing)

ff = fromDynamics $ DynamicsL (2.5, Nothing)

fff = fromDynamics $ DynamicsL (3.5, Nothing)

ffff = fromDynamics $ DynamicsL (4.5, Nothing)

fffff = fromDynamics $ DynamicsL (5.5, Nothing)

ffffff = fromDynamics $ DynamicsL (6.5, Nothing)

