 
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, TypeFamilies #-}

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
-- Provides standard pith names.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Common.Spell (
        -- ** Spelling
        Spelling,
        spell,
        spellInterval,
        modal,
        sharps,
        flats,
  ) where

import Data.AffineSpace

import Music.Pitch.Absolute
import Music.Pitch.Literal
import Music.Pitch.Alterable
import Music.Pitch.Augmentable
import Music.Pitch.Common.Name
import Music.Pitch.Common.Accidental
import Music.Pitch.Common.Quality
import Music.Pitch.Common.Number
import Music.Pitch.Common.Pitch
import Music.Pitch.Common.Interval
import Music.Pitch.Common.Semitones

type Spelling = Semitones -> Number

spell :: (Num a, HasSemitones p, p ~ Diff a, AffineSpace a) => Spelling -> a -> Pitch
spell g p = origin .+^ g `spellInterval` semitones (origin2 .-. p)
    where         
        -- TODO use Data.AffineSpace.Point.origin
        -- Then we can remove Num constraint
        origin2 = 0
        origin  = c

spellInterval :: HasSemitones a => Spelling -> a -> Interval
-- spellInterval = undefined
spellInterval z = (\s -> Interval (fromIntegral $ s `div` 12, fromIntegral $ z s, fromIntegral s)) .  semitones

-- |
-- Spell pitches as @c cs d eb e f fs g gs a bb b@
--
modal :: Spelling
modal = go
    where
        go 0  = 0
        go 1  = 0
        go 2  = 1
        go 3  = 2
        go 4  = 2
        go 5  = 3
        go 6  = 3
        go 7  = 4
        go 8  = 4
        go 9  = 5
        go 10 = 6
        go 11 = 6

-- |
-- Spell pitches as @c cs d ds e f fs g gs a as b@
--
sharps :: Spelling
sharps = go
    where
        go 0  = 0
        go 1  = 0
        go 2  = 1
        go 3  = 1
        go 4  = 2
        go 5  = 3
        go 6  = 3
        go 7  = 4
        go 8  = 4
        go 9  = 5
        go 10 = 5
        go 11 = 6

-- |
-- Spell pitches as @c db d eb e f gb g ab a bb b@
--
flats :: Spelling
flats = go
    where
        go 0  = 0
        go 1  = 1
        go 2  = 1
        go 3  = 2
        go 4  = 2
        go 5  = 3
        go 6  = 4
        go 7  = 4
        go 8  = 5
        go 9  = 5
        go 10 = 6
        go 11 = 6
                         
