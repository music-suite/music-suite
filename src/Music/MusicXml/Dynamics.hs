
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Music.MusicXml.Dynamics (

    Level(..)

  ) where

data Level = PPP | PP | P | MP | MF | F | FF | FFF
    deriving (Eq, Ord, Enum, Bounded)
