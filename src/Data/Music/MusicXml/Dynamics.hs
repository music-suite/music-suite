{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
module Data.Music.MusicXml.Dynamics
  ( Dynamics (..),
  )
where

import Music.Dynamics.Literal

data Dynamics
  = PPPPPP
  | PPPPP
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
  | FFFFF
  | FFFFFF
  | SF
  | SFP
  | SFPP
  | FP
  | RF
  | RFZ
  | SFZ
  | SFFZ
  | FZ
  deriving (Eq, Ord, Show, Enum, Bounded, Read)

instance IsDynamics Dynamics where
  fromDynamics (DynamicsL (Just x, Nothing)) = case x of
    (-6.5) -> PPPPPP
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
    5.5 -> FFFFF
    6.5 -> FFFFFF
  fromDynamics _ = error "fromDynamics: Unsupported literal for MusicXml.Dynamics"
