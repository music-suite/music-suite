
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Music.Pitch.Common.Chromatic (
    ChromaticSteps,
  ) where


newtype ChromaticSteps = ChromaticSteps { getChromaticSteps :: Integer }
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

