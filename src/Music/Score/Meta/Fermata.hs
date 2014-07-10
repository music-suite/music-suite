
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
-- Provides fermatas.
--
-------------------------------------------------------------------------------------

module Music.Score.Meta.Fermata (
        -- * Fermata type
        FermataType(..),
        Fermata,

        -- ** Adding fermatas to scores
        fermata,
        fermataDuring,

        -- ** Extracting fermatas
        withFermata,
  ) where


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

-- | Represents a fermata.
--
-- TODO where is the fermata added if the score contains multiple notes. Always the last?
data Fermata = Fermata FermataType
    deriving (Eq, Ord, Show, Typeable)

data FermataType = StandardFermata | LongFermata | VeryLongFermata
    deriving (Eq, Ord, Show, Typeable)

-- | Add a fermata over the whole score.
fermata :: (HasMeta a, HasPosition a) => Fermata -> a -> a
fermata c x = fermataDuring (_getEra x) c x

-- | Add a fermata to the given score.
fermataDuring :: HasMeta a => Span -> Fermata -> a -> a
fermataDuring s c = addMetaNote $ view note (s, (Option $ Just $ Last c))

-- | Extract fermatas in from the given score, using the given default fermata.
withFermata :: (Fermata -> Score a -> Score a) -> Score a -> Score a
withFermata f = withMeta (maybe id f . fmap getLast . getOption)
