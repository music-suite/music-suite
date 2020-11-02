{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- Provides special barlines as meta-data.
--
-- (Ordinary barlines are generated automatically, see also "Music.Score.Meta.Time").
module Music.Score.Meta.Barline
  ( -- * Barline type
    Barline (..),

    -- ** Adding barlines to scores
    barline,
    barlineDuring,
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

-- | Represents an explicitly added barline.
data Barline = StandardBarline | DoubleBarline
  deriving (Eq, Ord, Show, Typeable)

-- | Add a barline over the whole score.
barline :: (HasMeta a, HasPosition a) => Barline -> a -> a
barline c x = case _era x of
  Nothing -> x
  Just e -> barlineDuring e c x

-- | Add a barline to the given score.
barlineDuring :: HasMeta a => Span -> Barline -> a -> a
barlineDuring s c = addMetaNote $ view event (s, Option $ Just $ Last c)
