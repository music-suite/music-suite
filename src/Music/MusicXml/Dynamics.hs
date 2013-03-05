
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Music.MusicXml.Dynamics (

    Dynamics(..)

  ) where

data Dynamics 
    = PPPPPP | PPPPP | PPPP | PPP | PP | P 
    | MP | MF 
    | F | FF | FFF | FFFF | FFFFF | FFFFFF
    | SF | SFP | SFPP
    | FP | RF 
    | RFZ 
    | SFZ | SFFZ | FZ
    deriving (Eq, Ord, Enum, Bounded)
