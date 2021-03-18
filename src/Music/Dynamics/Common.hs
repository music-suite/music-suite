{-# LANGUAGE DerivingStrategies #-}
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
-- Provides relative dynamics.
module Music.Dynamics.Common
  ( Dynamics,
  )
where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Monoid.Average
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Music.Dynamics.Literal
import Data.Scientific (Scientific)

newtype Dynamics = Dynamics { getDynamics :: Average Rational }
  deriving newtype (Semigroup, Eq, Ord, Num, Real, Monoid, IsDynamics, Fractional)

instance Show Dynamics where
  -- TODO showsPrec
  show x
    | x == pppppp = "pppppp"
    | x == ppppp  = "ppppp"
    | x == pppp   = "pppp"
    | x == ppp    = "ppp"
    | x == pp     = "pp"
    | x == _p     = "_p"
    | x == mp     = "mp"
    | x == ffffff = "ffffff"
    | x == fffff  = "fffff"
    | x == ffff   = "ffff"
    | x == fff    = "fff"
    | x == ff     = "ff"
    | x == _f     = "_f"
    | x == mf     = "mf"
    | otherwise = show $ realToFrac @_ @Scientific $ average $ getDynamics x

deriving newtype instance AdditiveGroup Dynamics

instance VectorSpace Dynamics where
  type Scalar Dynamics = Rational

  s *^ Dynamics v = Dynamics (s *^ v)

instance AffineSpace Dynamics where
  type Diff Dynamics = Dynamics

  Dynamics x .-. Dynamics y = Dynamics (x .-. y)

  Dynamics p .+^ Dynamics v = Dynamics (p .+^ v)
