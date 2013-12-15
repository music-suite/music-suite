
{-# LANGUAGE 
    ScopedTypeVariables, 
    GeneralizedNewtypeDeriving,
    DeriveFunctor, 
    DeriveFoldable, 
    DeriveTraversable,
    DeriveDataTypeable, 
    ConstraintKinds,
    FlexibleContexts, 
    GADTs, 
    ViewPatterns,
    TypeFamilies,
    MultiParamTypeClasses, 
    FlexibleInstances #-}

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
-------------------------------------------------------------------------------------

module Music.Score.Meta.RehearsalMark (
        -- * Rehearsal mark type
        RehearsalMark,
        
        -- * Adding rehearsal marks to scores
        rehearsalMark,
        rehearsalMarkDuring,

        -- * Extracting rehearsal marks
        withRehearsalMark,       
  ) where


import Control.Arrow
import Control.Monad.Plus       
import Data.Default
import Data.Void
import Data.Maybe
import Data.Semigroup
import Data.Monoid.WithSemigroup
import Data.Typeable
import Data.String
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Music.Time
import Music.Time.Reactive
import Music.Score.Note
import Music.Score.Voice
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Meta
import Music.Score.Score
import Music.Score.Combinators
import Music.Score.Util
import Music.Pitch.Literal

-- | 
-- Represents 
--
data RehearsalMark = RehearsalMark (Maybe String) Int
    deriving (Eq, Ord, Typeable)
-- name level(0=standard)

instance Default RehearsalMark where
    def = RehearsalMark Nothing 0

instance Semigroup RehearsalMark where
    RehearsalMark n1 l1 <> RehearsalMark n2 l2 = RehearsalMark (n1 <> n2) (l1 `max` l2)

instance Monoid RehearsalMark where
    mempty  = def
    mappend = (<>)

instance Show RehearsalMark where
    show (RehearsalMark name level) = "A" -- TODo


-- metronome :: Duration -> Bpm -> Tempo
-- metronome noteVal bpm = Tempo Nothing (Just noteVal) $ 60 / (bpm * noteVal)

rehearsalMark :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => RehearsalMark -> a -> a
rehearsalMark c x = rehearsalMarkDuring (era x) c x

rehearsalMarkDuring :: (HasMeta a, HasPart' a) => Span -> RehearsalMark -> a -> a
rehearsalMarkDuring s x = addGlobalMetaNote (s =: x)

withRehearsalMark :: (RehearsalMark -> Score a -> Score a) -> Score a -> Score a
withRehearsalMark = withGlobalMeta

