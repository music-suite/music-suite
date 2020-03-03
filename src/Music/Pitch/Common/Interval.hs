-- | Common intervals.
module Music.Pitch.Common.Interval
  ( -- * Intervals
    Interval,

    -- ** Creating intervals
    interval,
    intervalTotalSteps,
    intervalAlterationSteps,
    steps,
    alteration,

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
    HasOctaves (..),

    -- *** Inversion
    invert,

    -- * Basis values
    IntervalBasis (..),
  )
where

import Music.Pitch.Common.Internal
