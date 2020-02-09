{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-missing-signatures
  #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------

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
-- Provides overloaded interval literals.
module Music.Pitch.Literal.Interval
  ( -- * IsInterval class
    IsInterval (..),

    -- * Literal values

    -- ** Simple intervals
    d1,
    _P1,
    _A1,
    d2,
    m2,
    _M2,
    _A2,
    d3,
    m3,
    _M3,
    _A3,
    d4,
    _P4,
    _A4,
    d5,
    _P5,
    _A5,
    d6,
    m6,
    _M6,
    _A6,
    d7,
    m7,
    _M7,
    _A7,

    -- ** One-octave compounds
    d8,
    _P8,
    _A8,
    d9,
    m9,
    _M9,
    _A9,
    d10,
    m10,
    _M10,
    _A10,
    d11,
    _P11,
    _A11,
    d12,
    _P12,
    _A12,
    d13,
    m13,
    _M13,
    _A13,
    d14,
    m14,
    _M14,
    _A14,

    -- ** Two-octave compounds
    d15,
    _P15,
    _A15,
  )
where

import Music.Pitch.Common.Types

