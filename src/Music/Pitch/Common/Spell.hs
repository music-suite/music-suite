
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund, Edward Lilley 2012–2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides pitch spellings.
--
-------------------------------------------------------------------------------------

module Music.Pitch.Common.Spell (
        -- ** Spelling
        -- * About
        -- $semitonesAndSpellings

        -- * Spelling type
        Spelling,
        spell,
        spelled,

        -- ** Standard spellings
        modally,
        usingSharps,
        usingFlats,
  ) where

import           Data.AffineSpace
import           Data.VectorSpace
import           Control.Lens

import           Music.Pitch.Absolute
import           Music.Pitch.Alterable
import           Music.Pitch.Augmentable
import           Music.Pitch.Common.Interval
import           Music.Pitch.Common.Number
import           Music.Pitch.Common.Pitch
import           Music.Pitch.Common.Semitones
import           Music.Pitch.Literal

-- $semitonesAndSpellings
--
-- TODO document better
--
-- The `semitones` function retrieves the number of Semitones in a pitch, for example
--
-- > semitones :: Interval -> Semitones
-- > semitones major third = 4
--
-- Note that semitones is surjetive. We can define a non-deterministic function `spellings`
--
-- > spellings :: Semitones -> [Interval]
-- > spellings 4 = [majorThird, diminishedFourth]
--
-- /Law/
--
-- > map semitones (spellings a) = replicate n a    for all n > 0
--
-- /Lemma/
--
-- > map semitones (spellings a)

-- |
-- A spelling provide a way of notating a semitone interval such as 'tritone'.
--
-- Examples:
--
-- > spell usingSharps tritone   == _A4
-- > spell usingFlats  tritone   == d5
-- > spell modally     tone      == _M2
--
type Spelling = Semitones -> Number


-- |
-- Spell an interval using the given 'Spelling'.
--
spell :: HasSemitones a => Spelling -> a -> Interval
spell spelling x = let
  -- TODO use Steps etc to remove fromIntegral
  (octaves, steps) = semitones x `divMod` 12
  num  = fromIntegral (spelling steps)
  diff = fromIntegral steps - fromIntegral (diatonicToChromatic num)
  in (\a b -> (fromIntegral a, fromIntegral b)^.interval') diff num ^+^ _P8^*(fromIntegral octaves)
    where
      diatonicToChromatic = go
        where
          go 0 = 0
          go 1 = 2
          go 2 = 4
          go 3 = 5
          go 4 = 7
          go 5 = 9
          go 6 = 11

-- |
-- Flipped version of 'spell'. To be used infix, as in:
--
-- > d5 `spelled` usingSharps
--
spelled :: HasSemitones a => a -> Spelling -> Interval
spelled = flip spell

-- |
-- Spell using the most the most common accidentals. Double sharps and flats are not
-- preserved.
--
-- This spelling is particularly useful for modal music where the tonic is C.
--
-- > c cs d eb e f fs g gs a bb b
--
modally :: Spelling
modally = go
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
-- Spell using sharps. Double sharps and flats are not preserved.
--
-- > c cs d ds e f fs g gs a as b
--
usingSharps :: Spelling
usingSharps = go
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
-- Spell using flats. Double sharps and flats are not preserved.
--
-- > c db d eb e f gb g ab a bb b
--
usingFlats :: Spelling
usingFlats = go
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

