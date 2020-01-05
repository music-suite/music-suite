{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

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
module Music.Prelude.Standard
  ( module Music.Pitch,
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
  )
where

import Data.Typeable
import Music.Articulation
import Music.Dynamics
import Music.Parts
import Music.Pitch
import Music.Prelude.Instances ()
import Music.Score hiding (Articulation, Clef (..), Dynamics, Fifths, Interval, Part, Pitch)
import Music.Score.Export2.StandardNotation (Asp1)

asNote :: StandardNote -> StandardNote
asNote = id

asScore :: Score StandardNote -> Score StandardNote
asScore = id

asVoice :: Voice StandardNote -> Voice StandardNote
asVoice = id

asTrack :: Track StandardNote -> Track StandardNote
asTrack = id

type StandardNote = Asp1

type Music = Score StandardNote

-- TODO remove!
fromPitch'' :: IsPitch a => Pitch -> a
fromPitch'' = fromPitch
{-# DEPRECATED fromPitch'' "Use fromPitch (no primes!)" #-}
