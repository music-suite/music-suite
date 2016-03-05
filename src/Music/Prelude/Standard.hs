
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Standard music representation.
--
-------------------------------------------------------------------------------------

module Music.Prelude.Standard (
        module Music.Pitch,
        module Music.Dynamics,
        module Music.Articulation,
        module Music.Parts,
        module Music.Score,
        Music,
        StandardNote,
        asScore,
        asVoice,
        asTrack,
        asNote,
        fromPitch'',
  ) where

import           Data.Typeable

import           Music.Pitch
import           Music.Dynamics
import           Music.Articulation
import           Music.Parts
import           Music.Score             hiding (Clef(..), Fifths, Interval, Part, Pitch, Dynamics, Articulation)

import           Music.Prelude.Instances ()

asNote :: StandardNote -> StandardNote
asNote = id

asScore :: Score StandardNote -> Score StandardNote
asScore = id

asVoice :: Voice StandardNote -> Voice StandardNote
asVoice = id

asTrack :: Track StandardNote -> Track StandardNote
asTrack = id

-- newtype BasicPart = BasicPart { getBasicPart :: Integer }
--     deriving (Eq, Ord, Num, Integral, Real, Enum, Typeable)
--
-- instance Default BasicPart where def = BasicPart 0
-- instance Show BasicPart where
--     show _ = ""

type StandardNote = 
  (PartT Part
    (ColorT 
      (TextT
        (TremoloT
          (HarmonicT
            (SlideT
              (ArticulationT Articulation
                (DynamicT Dynamics
                  [TieT
                    Pitch]))))))))

type Music = Score StandardNote
-- testRealize = realize (pitchSet [c,e,g,fs,gs]) undefined (Perm4 31)

fromPitch'' :: IsPitch a => Pitch -> a
fromPitch'' x = let i = x .-. c in 
  fromPitch $ PitchL ((fromIntegral $ i^._steps) `mod` 7, Just (fromIntegral (i^._alteration)), fromIntegral $ octaves i)
