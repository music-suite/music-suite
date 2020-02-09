{-# LANGUAGE FlexibleContexts #-}

-- | Common pitch.
module Music.Pitch.Common.Pitch
  ( -- * Accidentals
    Accidental,
    natural,
    flat,
    sharp,
    doubleFlat,
    doubleSharp,

    -- ** Inspecting accidentals
    isNatural,
    isFlattened,
    isSharpened,
    isStandardAccidental,

    -- ** Name
    Name (..),

    -- * Pitch
    Pitch,
    mkPitch,
    name,
    accidental,

    -- ** Diatonic and chromatic pitch
    upDiatonicP,
    downDiatonicP,
    upChromaticP,
    downChromaticP,
    invertDiatonicallyP,
    invertChromaticallyP,
  )
where

import Music.Pitch.Common.Types

