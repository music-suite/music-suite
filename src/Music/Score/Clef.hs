
{-# LANGUAGE 
    ScopedTypeVariables, 
    GeneralizedNewtypeDeriving,
    DeriveFunctor, 
    DeriveFoldable, 
    DeriveTraversable,
    DeriveDataTypeable, 
    ConstraintKinds,
    UndecidableInstances,
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
-- Provides clefs.
--
-------------------------------------------------------------------------------------


module Music.Score.Clef (
        ClefT(..),
        HasClef(..),
  ) where

import Control.Arrow
import Control.Monad.Plus       
import Data.Pointed
import Data.Void
import Data.Maybe
import Data.Semigroup
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
import Music.Score.Score
import Music.Score.Part
import Music.Score.Combinators (withMetaNP, mapFirst)
import Music.Score.Ornaments (HasText, text)
import Music.Score.Meta
import Music.Score.Ties
import Music.Score.Util


-- TODO move
-- Put the given clef in front of the note
newtype ClefT a = ClefT { getClefT :: (Option (Last Clef), a) }
    deriving (Functor, Semigroup, Monoid)

instance HasPart a => HasPart (ClefT a) where
    type Part (ClefT a) = Part a
    getPart (ClefT (_,a)) = getPart a
    modifyPart f (ClefT (a,b)) = ClefT (a, modifyPart f b)

instance Pointed ClefT where
    point x = ClefT (mempty, x)

instance Tiable a => Tiable (ClefT a) where
    beginTie = fmap beginTie
    endTie   = fmap endTie


-- TODO
kDefClef = GClef

class HasClef a where
    applyClef :: Clef -> a -> a
    applyClefOption :: Option Clef -> a -> a
    applyClefOption c = case getOption c of
        Nothing -> applyClef kDefClef
        Just c  -> applyClef c
    applyClefMaybe :: Maybe Clef -> a -> a
    applyClefMaybe c = case c of
        Nothing -> applyClef kDefClef
        Just c  -> applyClef c
instance HasClef (ClefT a) where
    applyClef c (ClefT (_,a)) = ClefT (Option $ Just $ Last c,a)
instance HasClef a => HasClef (b,a) where
    applyClef c = fmap (applyClef c)
instance (HasPart' a, HasClef a) => HasClef (Score a) where
    applyClef c = mapFirst (applyClef c) id
                                           
