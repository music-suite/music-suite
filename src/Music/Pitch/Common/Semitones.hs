-- | Semitones and enharmonic equivalence.
module Music.Pitch.Common.Semitones
  ( -- * Types

    -- ** Octaves
    Octaves,

    -- ** Semitones
    Semitones,
    HasSemitones (..),
    semitone,
    tone,
    ditone,
    tritone,
    isSemitone,
    isTone,
    isTritone,

    -- * Enharmonic equivalence
    (=:=),
    (/:=),
  )
where

import Music.Pitch.Common.Internal

