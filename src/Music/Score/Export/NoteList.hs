{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
-- A simple backend that supports rendering scores to lists of pitch and velocity.
--
-- This exists as a sanity check for the 'Backend' classes, and as an example.
module Music.Score.Export.NoteList
  ( -- * Note list backend
    NoteList,
    toNoteList,
  )
where

import qualified Codec.Midi as Midi
import Control.Applicative
import Control.Comonad (Comonad (..), extract)
import Control.Lens hiding (rewrite)
import Control.Monad
import Data.AffineSpace
import Data.Colour.Names as Color
import Data.Foldable (Foldable)
import qualified Data.Foldable
import Data.Functor.Couple
import Data.Functor.Identity
import qualified Data.List
import Data.Maybe
import Data.Ratio
import Data.Semigroup
import Data.Semigroup.Instances
import Data.Traversable (Traversable, sequenceA)
import Data.VectorSpace hiding (Sum (..))
import Music.Dynamics.Literal
import Music.Pitch.Literal
-- import Control.Lens.Operators hiding ((|>))

import Music.Score.Articulation
import Music.Score.Color
import Music.Score.Dynamics
import Music.Score.Dynamics
import Music.Score.Export.Backend
import Music.Score.Export.Backend
import Music.Score.Export.Backend
import Music.Score.Export.DynamicNotation
import Music.Score.Harmonics
import Music.Score.Internal.Export hiding (MVoice)
import Music.Score.Internal.Quantize
import Music.Score.Internal.Util (composed, retainUpdates, swap, unRatio)
import Music.Score.Meta
import Music.Score.Meta.Attribution
import Music.Score.Meta.Time
import Music.Score.Meta.Title
import Music.Score.Part
import Music.Score.Part
import Music.Score.Phrases
import Music.Score.Slide
import Music.Score.Text
import Music.Score.Ties
import Music.Score.Tremolo
import Music.Time
import Music.Time
import System.Process
import qualified Text.Pretty as Pretty

-- |
-- A token to represent the note list backend.
data NoteList

instance HasBackend NoteList where

  type BackendScore NoteList = []

  type BackendContext NoteList = Identity

  type BackendNote NoteList = [(Sum Int, Int)]

  type BackendMusic NoteList = [(Sum Int, Int)]

  finalizeExport _ = concat

instance HasBackendScore NoteList [a] where

  type BackendScoreEvent NoteList [a] = a

  exportScore _ = fmap Identity

instance HasBackendScore NoteList (Score a) where

  type BackendScoreEvent NoteList (Score a) = a

  exportScore _ = fmap Identity . toListOf traverse

instance HasBackendNote NoteList a => HasBackendNote NoteList [a] where
  exportNote b ps = mconcat $ map (exportNote b) $ sequenceA ps

instance HasBackendNote NoteList Int where
  exportNote _ (Identity p) = [(mempty, p)]

instance HasBackendNote NoteList Double where
  exportNote _ (Identity p) = [(mempty, round p)]

-- TODO prettier
instance HasBackendNote NoteList a => HasBackendNote NoteList (DynamicT b a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote NoteList a => HasBackendNote NoteList (ArticulationT b a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote NoteList a => HasBackendNote NoteList (PartT n a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote NoteList a => HasBackendNote NoteList (TremoloT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote NoteList a => HasBackendNote NoteList (TextT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote NoteList a => HasBackendNote NoteList (HarmonicT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote NoteList a => HasBackendNote NoteList (SlideT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote NoteList a => HasBackendNote NoteList (TieT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote NoteList a => HasBackendNote NoteList (ColorT a) where
  exportNote b = exportNote b . fmap extract

-- |
-- Export music as a note list.
toNoteList :: (HasBackendNote NoteList (BackendScoreEvent NoteList s), HasBackendScore NoteList s) => s -> [(Int, Int)]
toNoteList = over (mapped . _1) getSum . export (undefined :: NoteList)
