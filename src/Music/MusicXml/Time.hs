
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

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
-------------------------------------------------------------------------------------

module Music.MusicXml.Time (
        Duration(..),
        NoteType(..),

        Divs(..),
        NoteVal(..),
        NoteSize(..),

        Beat(..),
        BeatType(..)
  ) where

type Duration     = Divs
type NoteType     = (NoteVal, Maybe NoteSize)

newtype Divs      = Divs { getDivs :: Int }                   -- ^ Sounding time in ticks
newtype NoteVal   = NoteVal { getNoteVal :: Rational }        -- ^ Notated time in fractions, in @[2^^i | i <- [-10..3]]@.

data NoteSize     = SizeFull | SizeCue | SizeLarge

newtype Beat      = Beat { getBeat :: Int }                   -- ^ Time nominator
newtype BeatType  = BeatType { getBeatType :: Int }           -- ^ Time denominator



deriving instance Eq            Divs
deriving instance Ord           Divs
deriving instance Num           Divs
deriving instance Real          Divs
deriving instance Integral      Divs
deriving instance Enum          Divs
deriving instance Show          Divs

deriving instance Eq            NoteVal
deriving instance Ord           NoteVal
deriving instance Num           NoteVal
deriving instance Enum          NoteVal
deriving instance Fractional    NoteVal
deriving instance Real          NoteVal
deriving instance RealFrac      NoteVal
deriving instance Show          NoteVal

deriving instance Eq            NoteSize
deriving instance Ord           NoteSize
deriving instance Enum          NoteSize
deriving instance Bounded       NoteSize

deriving instance Eq            Beat
deriving instance Ord           Beat
deriving instance Num           Beat
deriving instance Enum          Beat  

deriving instance Eq            BeatType
deriving instance Ord           BeatType
deriving instance Num           BeatType
deriving instance Enum          BeatType


