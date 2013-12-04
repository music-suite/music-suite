
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

module Music.Score.Meta.Clef (
        Clef(..),
        clef,
        clefDuring,
        withClef,
  ) where

import Control.Arrow
import Control.Monad.Plus       
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

data Clef = GClef | CClef | FClef
    deriving (Eq, Ord, Show, Typeable)

clef :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => Clef -> a -> a
clef c x = clefDuring (era x) c x

clefDuring :: (HasMeta a, HasPart' a) => Span -> Clef -> a -> a
clefDuring s c = addMetaNote (s =: (Option $ Just $ Last c))

withClef :: Clef -> (Clef -> Score a -> Score a) -> Score a -> Score a
withClef def f = withGlobalMeta (f . fromMaybe def . fmap getLast . getOption)
