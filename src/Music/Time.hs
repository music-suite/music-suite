
-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides generic functions for manipulating time-based structures.
--
-------------------------------------------------------------------------------------

module Music.Time (
    module Data.Functor.Rep.Lens,
    
    module Music.Time.Types,
    module Music.Time.Transform,
    module Music.Time.Duration,
    module Music.Time.Position,
    module Music.Time.Split,
    module Music.Time.Reverse,
    module Music.Time.Juxtapose,

    module Music.Time.Stretched,
    module Music.Time.Delayed,
    module Music.Time.Note,
    module Music.Time.Voice,
    module Music.Time.Chord,
    module Music.Time.Track,
    module Music.Time.Score,
    module Music.Time.Segment,
    module Music.Time.Behavior,
    module Music.Time.Reactive,
  ) where

import Data.Functor.Rep
import Data.Functor.Rep.Lens

import Music.Time.Types
import Music.Time.Transform
import Music.Time.Duration
import Music.Time.Position
import Music.Time.Split
import Music.Time.Reverse
import Music.Time.Juxtapose

import Music.Time.Stretched
import Music.Time.Delayed
import Music.Time.Note
import Music.Time.Voice
import Music.Time.Chord
import Music.Time.Track
import Music.Time.Score
import Music.Time.Segment
import Music.Time.Behavior
import Music.Time.Reactive
