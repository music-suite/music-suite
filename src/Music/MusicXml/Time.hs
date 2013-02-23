
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

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

newtype Divs      = Divs { getDivs :: Int }                      -- absolute dur in ticks
newtype NoteVal   = NoteVal { getNoteVal :: Rational }        -- relative dur in notated time

data NoteSize     = SizeFull | SizeCue | SizeLarge

newtype Beat      = Beat { getBeat :: Int }                      -- time nominator
newtype BeatType  = BeatType { getBeatType :: Int }          -- time denominator



deriving instance Eq            Divs
deriving instance Ord           Divs
deriving instance Num           Divs
deriving instance Enum          Divs

deriving instance Eq            NoteVal
deriving instance Ord           NoteVal
deriving instance Num           NoteVal
deriving instance Enum          NoteVal
deriving instance Fractional    NoteVal

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


