
{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : GHC
--
-------------------------------------------------------------------------------------

module Music.Lilypond.Pitch (
        Pitch(..),
        PitchClass(..),
        Accidental(..),
        Octaves(..),
        Mode(..),
  ) where

import Text.Pretty hiding (Mode)
import Music.Pitch.Literal

data PitchClass = C | D | E | F | G | A | B
    deriving (Eq, Ord, Show, Enum)

newtype Pitch = Pitch { getPitch :: (PitchClass, Accidental, Octaves) }
    deriving (Eq, Ord, Show)

instance Pretty Pitch where
    pretty (Pitch (c,a,o)) = string $ pc c ++ acc a ++ oct (o-4)
        where
            pc C = "c" ; pc D = "d" ; pc E = "e" ; pc F = "f"
            pc G = "g" ; pc A = "a" ; pc B = "b"            
            acc n | n <  0  =  concat $ replicate (negate n) "es"
                  | n == 0  =  ""
                  | n >  0  =  concat $ replicate (n) "is"
            oct n | n <  0  =  concat $ replicate (negate n) ","
                  | n == 0  =  ""
                  | n >  0  =  concat $ replicate n "'"

instance IsPitch Pitch where
    fromPitch (PitchL (c, Nothing, o)) = Pitch (toEnum c, 0,       o)                 
    fromPitch (PitchL (c, Just a, o))  = Pitch (toEnum c, round a, o)


-- | For double flat -2, flat -1, natural 0, sharp 1 and double sharp 2.
type Accidental = Int 

-- | Number of octaves raised (positive) or flattened (negative).
type Octaves    = Int 

-- | Mode (for key signatures).
data Mode = Major | Minor
    deriving (Eq, Show)

instance Pretty Mode where
    pretty Major = "\\major"
    pretty Minor = "\\minor"

