-- | Common intervals.
module Music.Pitch.Common.Interval
  ( -- * Intervals
    Interval,

    -- ** Creating intervals
    interval,
    intervalTotalSteps,
    intervalAlterationSteps,
    _number,
    _quality,
    _steps,
    _alteration,

    -- ** Synonyms
    perfect,
    major,
    minor,
    augmented,
    diminished,
    doublyAugmented,
    doublyDiminished,

    -- ** Inspecting intervals
    isNegative,
    isPositive,
    isNonNegative,
    isPerfectUnison,
    isStep,
    isLeap,

    -- ** Simple and compound intervals
    isSimple,
    isCompound,
    separate,
    simple,
    octaves,

    -- *** Inversion
    invert,

    -- * Basis values
    IntervalBasis (..),

    -- ** Alternative basis (TODO move to seprate module?)
    convertBasis,
    convertBasisFloat,
  )
where

import Music.Pitch.Common.Internal
