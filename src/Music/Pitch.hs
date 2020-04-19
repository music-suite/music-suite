-- |  Standard pitch representation.
module Music.Pitch
  ( module Data.Semigroup,
    module Data.VectorSpace,
    module Data.AffineSpace,
    module Data.AffineSpace.Point,
    module Music.Pitch.Augmentable,
    module Music.Pitch.Alterable,
    module Music.Pitch.Absolute,
    module Music.Pitch.Common,
    module Music.Pitch.Equal,
    module Music.Pitch.Clef,
    module Music.Pitch.Intonation,
    module Music.Pitch.Literal,
    module Music.Pitch.Ambitus,
    module Music.Pitch.Scale,
    MajorMinor (MajorMode, MinorMode),
  )
where

import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Semigroup
import Data.VectorSpace hiding (Sum, getSum)
import Music.Pitch.Absolute hiding (octaves)
import Music.Pitch.Alterable
import Music.Pitch.Ambitus
import Music.Pitch.Augmentable
import Music.Pitch.Clef
import Music.Pitch.Common hiding (Mode)
import Music.Pitch.Common.Names
import Music.Pitch.Equal
import Music.Pitch.Intonation
import Music.Pitch.Literal
import Music.Pitch.Scale
