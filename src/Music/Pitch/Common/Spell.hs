 
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
        spellAs,
        
        -- ** Standard spellings
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
import Music.Pitch.Common.Enharmonic

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
-- > spell (const 4) tritone == _A4
-- > spell (const 5) tritone == d5
-- > spell (const 2) 1       == m2
--
type Spelling = Semitones -> Number


-- |
-- Spell an interval using the given 'Spelling'.
--
-- FIXME does not handle compound intervals
--
spell :: HasSemitones a => Spelling -> a -> Interval
spell spelling x = let
    semi = semitones x
    num  = fromIntegral (spelling semi)
    diff = fromIntegral semi - fromIntegral (diatonicToChromatic num)
    in interval' diff (num + 1)
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
-- > d5 `spellAs` sharps
--        
spellAs :: HasSemitones a => a -> Spelling -> Interval
spellAs = flip spell

-- |
-- Spell using the most the most common accidentals. Double sharps and flats are not
-- preserved.
--
-- This spelling is particularly useful for modal music with C or F as tonic.
--
-- > c cs d eb e f fs g gs a bb b
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
-- Spell using sharps. Double sharps and flats are not preserved.
--
-- > c cs d ds e f fs g gs a as b
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
-- Spell using flats. Double sharps and flats are not preserved.
--
-- > c db d eb e f gb g ab a bb b
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
                         
