
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

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
-- Provides tempo meta-data.
--
-- /Warning/ This is not supported by any backends yet.
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


import           Control.Arrow
import           Control.Lens              (view)
import           Control.Monad.Plus
import           Data.Default
import           Data.Foldable             (Foldable)
import qualified Data.Foldable             as F
import qualified Data.List                 as List
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.String
import           Data.Traversable          (Traversable)
import qualified Data.Traversable          as T
import           Data.Typeable
import           Data.Void

import           Music.Pitch.Literal
import           Music.Score.Meta
import           Music.Score.Part
import           Music.Score.Pitch
import           Music.Score.Internal.Util
import           Music.Time
import           Music.Time.Reactive

-- | Represents a rehearsal mark.
--
-- TODO this needs zero-duration spans to work properly.
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

rehearsalMark :: (HasMeta a, {-HasPart' a, -}HasPosition a) => RehearsalMark -> a -> a
rehearsalMark c x = rehearsalMarkDuring (_getEra x) c x

rehearsalMarkDuring :: (HasMeta a{-, HasPart' a-}) => Span -> RehearsalMark -> a -> a
rehearsalMarkDuring s x = addGlobalMetaNote $ view note (s, x)

withRehearsalMark :: (RehearsalMark -> Score a -> Score a) -> Score a -> Score a
withRehearsalMark = withGlobalMeta

