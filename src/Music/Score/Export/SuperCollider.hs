
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

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
-- A backend to generate SuperCollider code.
--
-- This is a very simple thing that generates /patterns/ (which are basically lazy event 
-- lists) in the SuperCollider language.
--
-- It would of course also be nice to have a backend based the Haskell bindings (see
-- <http://hackage.haskell.org/package/hsc3>). In that case we could bypass the
-- SuperCollider language and just use /scsynth/.
--
module Music.Score.Export.SuperCollider (
    -- * SuperCollider patterns backend
    SuperCollider,
    HasSuperCollider,
    toSuperCollider,
    writeSuperCollider,
    openSuperCollider,
  ) where

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import qualified Codec.Midi                    as Midi
import           Control.Comonad               (Comonad (..), extract)
import           Control.Applicative
import           Data.Colour.Names             as Color
import           Data.Default
import           Data.Foldable                 (Foldable)
import qualified Data.Foldable
import           Data.Functor.Couple
import           Data.Maybe
import           Data.Ratio
import           Data.Traversable              (Traversable, sequenceA)
import qualified Music.Lilypond                as Lilypond
import qualified Music.MusicXml.Simple         as MusicXml
import           Music.Score.Internal.Export   hiding (MVoice)
import           System.Process
import           Music.Time.Internal.Quantize
import qualified Text.Pretty                   as Pretty
import qualified Data.List
import           Music.Score.Internal.Util (composed, unRatio, swap, retainUpdates)
import Music.Score.Export.DynamicNotation
import Data.Semigroup.Instances

import Music.Score.Export.Backend

import Data.Functor.Identity
import Data.Semigroup
import Control.Monad
import Data.VectorSpace hiding (Sum(..))
import Data.AffineSpace
import Control.Lens hiding (rewrite)

import Music.Time
import Music.Score.Meta
import Music.Score.Meta.Title
import Music.Score.Meta.Attribution
import Music.Score.Dynamics
import Music.Score.Articulation
import Music.Score.Part
import Music.Score.Tremolo
import Music.Score.Text
import Music.Score.Harmonics
import Music.Score.Slide
import Music.Score.Color
import Music.Score.Ties
import Music.Score.Export.Backend
import Music.Score.Meta.Time
import Music.Score.Phrases


-- | A token to represent the SuperCollider backend.
data SuperCollider

-- | Pass duration to the note export.
type ScContext = Identity--ScContext Duration a deriving (Functor, Foldable, Traversable)

-- | Just \dur, \midinote, \db for now
type ScEvent = (Double, Double)

-- | Score is just a list of parallel voices.
data ScScore a = ScScore [[(Duration, Maybe a)]]
  deriving (Functor)

instance Monoid (ScScore a) where
  mempty = ScScore mempty
  ScScore a `mappend` ScScore b = ScScore (a `mappend` b)

instance HasBackend SuperCollider where
  type BackendContext SuperCollider    = ScContext
  type BackendScore   SuperCollider    = ScScore
  type BackendNote    SuperCollider    = ScEvent
  type BackendMusic   SuperCollider    = String

  finalizeExport _ (ScScore trs) = composeTracksInParallel $ map exportTrack trs
    where        
      composeTracksInParallel :: [String] -> String
      composeTracksInParallel = (\x -> "Ppar([" ++ x ++ "])") . Data.List.intercalate ", "
      
      exportTrack :: [(Duration, Maybe ScEvent)] -> String
      exportTrack events = "Pbind("
        ++ "\\dur, Pseq(" ++ show durs ++ ")"
        ++ ", "
        ++ "\\midinote, Pseq(" ++ showRestList pitches ++ ")"
        ++ ")"
        where
          showRestList = (\x -> "[" ++ x ++ "]") 
            . Data.List.intercalate ", " 
            . map (maybe "\\rest" show) 
  
          -- events :: ScEvent
          durs    :: [Double]
          pitches :: [Maybe Double]
          ampls   :: [Maybe Double]
          durs    = map (realToFrac . fst) events
          pitches = map (fmap fst . snd) events
          ampls   = map (fmap snd . snd) events
          

instance () => HasBackendScore SuperCollider (Voice (Maybe a)) where
  type BackendScoreEvent SuperCollider (Voice (Maybe a)) = a
  exportScore _ xs = Identity <$> ScScore [view eventsV xs]

instance (HasPart' a, Ord (Part a)) => HasBackendScore SuperCollider (Score a) where
  type BackendScoreEvent SuperCollider (Score a) = a
  exportScore b = mconcat
    . map (exportScore b . view singleMVoice)
    . extractParts
  
instance HasBackendNote SuperCollider a => HasBackendNote SuperCollider [a] where
  exportNote b ps = head $ map (exportNote b) $ sequenceA ps

instance HasBackendNote SuperCollider Double where
  exportNote _ (Identity x) = (x + 60, 1)

instance HasBackendNote SuperCollider Int where
  exportNote _ (Identity x) = (fromIntegral x + 60, 1)

instance HasBackendNote SuperCollider Integer where
  exportNote _ (Identity x) = (fromIntegral x + 60, 1)

instance HasBackendNote SuperCollider a => HasBackendNote SuperCollider (Behavior a) where
  exportNote b = exportNote b . fmap (! 0)
  exportChord b = exportChord b . fmap (fmap (! 0))

instance HasBackendNote SuperCollider a => HasBackendNote SuperCollider (DynamicT b a) where
  exportNote b = exportNote b . fmap extract
  -- exportNote b (Identity (DynamicT (Sum v, x))) = fmap (setV v) $ exportNote b (Identity x)

instance HasBackendNote SuperCollider a => HasBackendNote SuperCollider (ArticulationT b a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote SuperCollider a => HasBackendNote SuperCollider (PartT n a) where
  -- Part structure is handled by HasSuperColliderBackendScore instances, so this is just an identity
  exportNote b = exportNote b . fmap extract

instance HasBackendNote SuperCollider a => HasBackendNote SuperCollider (TremoloT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote SuperCollider a => HasBackendNote SuperCollider (TextT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote SuperCollider a => HasBackendNote SuperCollider (HarmonicT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote SuperCollider a => HasBackendNote SuperCollider (SlideT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote SuperCollider a => HasBackendNote SuperCollider (TieT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote SuperCollider a => HasBackendNote SuperCollider (ColorT a) where
  exportNote b = exportNote b . fmap extract


-- |
-- Constraint for types that has a SuperCollider representation.
--
type HasSuperCollider a = (HasBackendNote SuperCollider (BackendScoreEvent SuperCollider a), HasBackendScore SuperCollider a)

-- |
-- Convert music to a SuperCollider code string.
--
toSuperCollider :: HasSuperCollider a => a -> String
toSuperCollider = export (undefined::SuperCollider)

-- |
-- Write music as a SuperCollider code string to the given path.
--
-- @
-- writeSuperCollider \"test.sc\" $ scat [c,d,e]
-- @
--
writeSuperCollider :: HasSuperCollider a => FilePath -> a -> IO ()
writeSuperCollider path score =
  writeFile path ("(" ++ toSuperCollider score ++ ").play")

-- |
-- Write music as a SuperCollider code string and open it.
--
-- (This is simple wrapper around 'writeSuperCollider' that may not work well on all platforms.)
--
openSuperCollider :: HasSuperCollider a => a -> IO ()
openSuperCollider = writeSuperCollider "test.sc"

