{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-------------------------------------------------------------------------------------

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
module Music.Score.Meta.RehearsalMark
  ( -- * Rehearsal mark type
    RehearsalMark,

    -- * Adding rehearsal marks to scores
    rehearsalMark,
    rehearsalMarkAt,
  )
where

import Control.Lens (view)
import Control.Monad.Plus
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Typeable
import Music.Pitch.Literal
import Music.Score.Internal.Util
import Music.Score.Meta
import Music.Score.Part
import Music.Score.Pitch
import Music.Time
import Music.Time.Reactive

-- | Represents a rehearsal mark.
data RehearsalMark = RehearsalMark
  deriving (Eq, Ord, Show, Typeable)

-- name level(0=standard)

{-
instance Default RehearsalMark where
    def = RehearsalMark Nothing 0
-}

instance Semigroup RehearsalMark where
  RehearsalMark <> RehearsalMark = RehearsalMark

instance Monoid RehearsalMark where

  mempty = RehearsalMark

  mappend = (<>)

-- metronome :: Duration -> Bpm -> Tempo
-- metronome noteVal bpm = Tempo Nothing (Just noteVal) $ 60 / (bpm * noteVal)

rehearsalMark :: (HasMeta a, HasPosition a, Transformable a) => a -> a
rehearsalMark x = case _era x of
  Nothing -> x
  Just e -> rehearsalMarkAt (view onset e) x

rehearsalMarkAt :: HasMeta a => Time -> a -> a
rehearsalMarkAt t = addMetaNote $ view event (t <-> t, RehearsalMark)
