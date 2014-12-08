
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Music.Pitch.Common.Diatonic (
    DiatonicSteps,
  ) where

newtype DiatonicSteps = DiatonicSteps { getDiatonicSteps :: Integer }
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)
