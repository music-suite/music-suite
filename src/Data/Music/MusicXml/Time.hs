{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

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
module Data.Music.MusicXml.Time
  ( Duration (..),
    NoteType (..),
    Divs (..),
    NoteVal (..),
    NoteSize (..),
    Beat (..),
    BeatType (..),
    Tempo (..),
  )
where

import Data.Default

type Duration = Divs

type NoteType = (NoteVal, Maybe NoteSize)

newtype Divs
  = -- | Sounding time in ticks
    Divs {getDivs :: Int}

-- | By default we use 768 per quarter note, like Sibelius.
instance Default Divs where
  def = 768 * 4


newtype NoteVal
  = -- | Notated time in fractions, in @[2^^i | i <- [-10..3]]@.
    NoteVal {getNoteVal :: Rational}

data NoteSize = SizeFull | SizeCue | SizeLarge
  deriving Show

newtype Beat
  = -- | Time nominator
    Beat {getBeat :: Int}
  deriving Show

newtype BeatType
  = -- | Time denominator
    BeatType {getBeatType :: Int}
  deriving Show

newtype Tempo
  = -- | Tempo in BPM
    Tempo {getTempo :: Double}

deriving instance Eq Divs

deriving instance Ord Divs

deriving instance Num Divs

deriving instance Real Divs

deriving instance Integral Divs

deriving instance Enum Divs

deriving instance Show Divs

deriving instance Eq NoteVal

deriving instance Ord NoteVal

deriving instance Num NoteVal

deriving instance Enum NoteVal

deriving instance Fractional NoteVal

deriving instance Real NoteVal

deriving instance RealFrac NoteVal

deriving instance Show NoteVal

deriving instance Eq NoteSize

deriving instance Ord NoteSize

deriving instance Enum NoteSize

deriving instance Bounded NoteSize

deriving instance Eq Beat

deriving instance Ord Beat

deriving instance Num Beat

deriving instance Enum Beat

deriving instance Eq BeatType

deriving instance Ord BeatType

deriving instance Num BeatType

deriving instance Enum BeatType

deriving instance Eq Tempo

deriving instance Ord Tempo

deriving instance Num Tempo

deriving instance Enum Tempo

deriving instance Fractional Tempo

deriving instance Real Tempo

deriving instance RealFrac Tempo

deriving instance Show Tempo
