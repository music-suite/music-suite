{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : GHC
module Data.Music.Lilypond.Dynamics
  ( Dynamics (..),
  )
where

import qualified Data.Char as Char
import Music.Dynamics.Literal
import Text.Pretty hiding (Mode)

data Dynamics
  = PPPPP
  | PPPP
  | PPP
  | PP
  | P
  | MP
  | MF
  | F
  | FF
  | FFF
  | FFFF
  | SF
  | SFF
  | SP
  | SPP
  | SFZ
  | RFZ
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty Dynamics where
  pretty = string . ("\\" ++) . fmap Char.toLower . show

instance IsDynamics Dynamics where
  fromDynamics (DynamicsL (x, _)) = case x of
    (-5.5) -> PPPPP
    (-4.5) -> PPPP
    (-3.5) -> PPP
    (-2.5) -> PP
    (-1.5) -> P
    (-0.5) -> MP
    0.5 -> MF
    1.5 -> F
    2.5 -> FF
    3.5 -> FFF
    4.5 -> FFFF
    x -> error $ "Lilypond.Dynamics: Strange value " ++ show x
