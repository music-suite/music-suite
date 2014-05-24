


{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
-- {-# LANGUAGE FunctionalDependencies     #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Music.Score.Export.Backend (
    HasOrdPart,
    HasDynamic3,
    HasDynamicNotation,

    HasBackend(..),
    HasBackendScore(..),
    HasBackendNote(..),
    export
  ) where

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import qualified Codec.Midi                    as Midi
import           Control.Arrow                 ((***))
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
import           Music.Time.Internal.Transform (whilstLD)
import           System.Process
import           Music.Time.Internal.Quantize
import qualified Text.Pretty                   as Pretty
import qualified Data.List
import Music.Score.Convert (reactiveToVoice') -- TODO
import           Music.Score.Internal.Util (composed, unRatio, swap, retainUpdates)
import Music.Score.Export.DynamicNotation
import Data.Semigroup.Instances


import Music.Time
import Music.Score.Dynamics
import Music.Score.Part


type HasDynamic3 a a' a'' = (
  HasDynamic' a,
  HasDynamic a  a',
  HasDynamic a' a'',
  HasDynamic a  a''
  )

-- -- TODO move
-- class (
--   HasDynamic' a,
--   HasDynamic a  a',
--   HasDynamic a' a'',
--   HasDynamic a  a''
--   ) => HasDynamic3 a a' a'' where
-- 
-- instance (b ~  SetDynamic (Ctxt (Dynamic MyNote)) MyNote, c ~ SetDynamic DynamicNotation MyNote) 
--   => HasDynamic3 MyNote b c

type HasDynamicNotation a b c = (
  HasDynamic3 a b c,
  Dynamic b  ~ Ctxt (Dynamic a),
  Dynamic c ~ DynamicNotation,
  Real (Dynamic a),
  Part (SetDynamic (Dynamic a) a) ~ Part (SetDynamic DynamicNotation b)
 )
type HasOrdPart a = (HasPart' a, Ord (Part a))



{-
-- TODO move
mapWithDur :: (Duration -> a -> b) -> Rhythm a -> Rhythm b
mapWithDur f = go
  where
    go (Beat d x)            = Beat d (f d x)
    go (Dotted n (Beat d x)) = Dotted n $ Beat d (f (dotMod n * d) x)
    go (Group rs)            = Group $ fmap (mapWithDur f) rs
    go (Tuplet m r)          = Tuplet m (mapWithDur f r)        

extractTimeSignatures :: Score a -> ([Maybe TimeSignature], [Duration])
extractTimeSignatures score = (barTimeSignatures, barDurations)
  where                                          
    defaultTimeSignature = time 4 4
    timeSignatures = fmap swap 
      $ view eventsV . fuse . reactiveToVoice' (0 <-> (score^.offset)) 
      $ getTimeSignatures defaultTimeSignature score

    -- Despite the fuse above we need retainUpdates here to prevent redundant repetition of time signatures
    barTimeSignatures = retainUpdates $ getBarTimeSignatures timeSignatures
    barDurations = getBarDurations timeSignatures


-}




-- |
-- This class defines types and functions for exporting music. It provides the
-- primitive types and methods used to implement 'export'.
--
-- The backend type @b@ is just a type level tag to identify a specific backend.
-- It is typically defined as an empty data declaration.
--
-- The actual conversion is handled by the subclasses 'HasBackendScore' and
-- 'HasBackendNote', which converts the time structure, and the contained music
-- respectively. Thus structure and content are handled separately.
--
-- It is often necessary to alter the events based on their surrounding context: for
-- examples the beginning and end of spanners and beams depend on surrounding notes.
-- The 'BackendContext' type allow 'HasBackendScore' instances to provide context for
-- 'HasBackendNote' instances.
--
-- @
-- data NoteList
--
-- instance HasBackend NoteList where
--   type BackendScore NoteList     = []
--   type BackendContext NoteList   = Identity
--   type BackendNote NoteList      = [(Sum Int, Int)]
--   type BackendMusic NoteList     = [(Sum Int, Int)]
--   finalizeExport _ = concat
--
-- instance HasBackendScore NoteList [a] a where
--   exportScore _ = fmap Identity
--
-- instance HasBackendNote NoteList a => HasBackendNote NoteList [a] where
--   exportNote b ps = mconcat $ map (exportNote b) $ sequenceA ps
--
-- instance HasBackendNote NoteList Int where
--   exportNote _ (Identity p) = [(mempty ,p)]
--
-- instance HasBackendNote NoteList a => HasBackendNote NoteList (DynamicT (Sum Int) a) where
--   exportNote b (Identity (DynamicT (d,ps))) = set (mapped._1) d $ exportNote b (Identity ps)
-- -- @
--
--
class Functor (BackendScore b) => HasBackend b where
  -- | External music representation
  type BackendMusic b :: *

  -- | Notes, chords and rests, with output handled by 'HasBackendNote'
  type BackendNote b :: *

  -- | Score, voice and time structure, with output handled by 'HasBackendScore'
  type BackendScore b :: * -> *

  -- | This type may be used to pass context from 'exportScore' to 'exportNote'.
  --   If the note export is not context-sensitive, 'Identity' can be used.
  type BackendContext b :: * -> *

  finalizeExport :: b -> BackendScore b (BackendNote b) -> BackendMusic b

class (HasBackend b) => HasBackendScore b s where
  type BackendScoreEvent b s :: *
  exportScore :: b -> s -> BackendScore b (BackendContext b (BackendScoreEvent b s))

class (HasBackend b) => HasBackendNote b a where
  exportNote  :: b -> BackendContext b a   -> BackendNote b
  exportChord :: b -> BackendContext b [a] -> BackendNote b
  exportChord = error "Not implemented: exportChord"

export :: (HasBackendScore b s, HasBackendNote b (BackendScoreEvent b s)) => b -> s -> BackendMusic b
export b = finalizeExport b . export'
  where
    export' = fmap (exportNote b) . exportScore b

