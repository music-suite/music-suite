
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

-- | Represents a fermata.
--
-- TODO where is the fermata added if the score contains multiple notes. Always the last?
data Fermata = Fermata FermataType
    deriving (Eq, Ord, Show, Typeable)

data FermataType = StandardFermata | LongFermata | VeryLongFermata
    deriving (Eq, Ord, Show, Typeable)

-- | Add a fermata over the whole score.
fermata :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => Fermata -> a -> a
fermata c x = fermataDuring (era x) c x

-- | Add a fermata to the given score.
fermataDuring :: (HasMeta a, HasPart' a) => Span -> Fermata -> a -> a
fermataDuring s c = addMetaNote (s =: (Option $ Just $ Last c))

-- | Extract fermatas in from the given score, using the given default fermata.
withFermata :: (Fermata -> Score a -> Score a) -> Score a -> Score a
withFermata f = withGlobalMeta (maybe id f . fmap getLast . getOption)
