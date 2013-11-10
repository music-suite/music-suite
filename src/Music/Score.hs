
-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides a musical score representation.
--
-------------------------------------------------------------------------------------

module Music.Score (
        -- * Prerequisites
        module Control.Monad,
        module Control.Monad.Plus,
        module Data.Semigroup,
        module Data.VectorSpace,
        module Data.AffineSpace,
        module Data.AffineSpace.Point,

        -- * Basic types
        module Music.Time,

        -- * Musical container types
        module Music.Score.Note,
        module Music.Score.Track,
        module Music.Score.Voice,
        module Music.Score.Score,
        module Music.Score.Convert,

        -- * Manipulation
        -- ** Combinators
        module Music.Score.Combinators,

        -- ** Structure
        module Music.Score.Ties,
        module Music.Score.Chord,

        -- ** Meta-information
        module Music.Score.Meta,
        module Music.Score.Annotations,

        -- ** Musical elements
        module Music.Score.Part,
        module Music.Score.Pitch,
        module Music.Score.Dynamics,
        module Music.Score.Articulation,
        module Music.Score.Ornaments,

        -- * Export
        module Music.Score.Export.Midi,
        module Music.Score.Export.Lilypond,
        module Music.Score.Export.MusicXml,
)
where

import Data.Semigroup
import Data.Ratio
import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Plus
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Typeable
import Data.Traversable
import Data.VectorSpace hiding (Sum, getSum)
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Basis

import Music.Time

import Music.Score.Rhythm
import Music.Score.Track
import Music.Score.Note
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Combinators
import Music.Score.Convert
import Music.Score.Chord
import Music.Score.Meta
import Music.Score.Annotations
import Music.Score.Pitch
import Music.Score.Ties
import Music.Score.Part
import Music.Score.Articulation
import Music.Score.Dynamics
import Music.Score.Ornaments
import Music.Score.Instances ()
import Music.Score.Export.Midi
import Music.Score.Export.Lilypond
import Music.Score.Export.MusicXml

