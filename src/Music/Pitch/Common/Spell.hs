-- | Provides pitch spelling.
module Music.Pitch.Common.Spell
  ( -- ** Spelling

    -- * About
    -- $semitonesAndSpellings

    -- * Spelling type
    Spelling,
    spell,
    spelled,
    spellPitchRelative,

    -- ** Standard spellings
    modally,
    usingSharps,
    usingFlats,

    -- * Simplifying pitches and intervals
    useStandardQualities,
    useSimpleQualities,
    useStandardAlterations,
    useSimpleAlterations,
  )
where

import Music.Pitch.Common.Internal
