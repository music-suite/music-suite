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
-- Provides fermatas.
module Music.Score.Meta.Fermata
  ( -- * Fermata type
    FermataType (..),
    Fermata,

    -- ** Adding fermatas to scores
    fermata,
    fermataAt,

    -- ** Extracting fermatas
    withFermata,
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

-- | Represents a fermata.
--
data Fermata = StandardFermata | LongFermata | VeryLongFermata
  deriving (Eq, Ord, Show, Typeable)

-- | Add a fermata over the whole score.
fermata :: (HasMeta a, HasPosition a) => Fermata -> a -> a
fermata c x = fermataAt (_onset x) c x

-- | Add a fermata to the given score.
fermataAt :: HasMeta a => Time -> Fermata -> a -> a
fermataAt s c = addMetaNote $ view event (s <-> s, (Option $ Just $ Last c))

-- | Extract fermatas in from the given score, using the given default fermata.
withFermata :: (Fermata -> Score a -> Score a) -> Score a -> Score a
withFermata f = withMeta (maybe id f . fmap getLast . getOption)
