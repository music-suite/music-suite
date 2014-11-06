
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Pitch class theory (also known as "Musical Set Theory").
module Music.Pitch.Class where

-- TODO
import Music.Prelude
-- import TypeUnary.Nat
import Data.Modular


-- newtype Modulo
type PitchClass    = Semitones `Mod` 12
type IntervalClass = Semitones `Mod` 6


