
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

module Music.Score.Meta.Tempo (
        Tempo,
        metronome,
        tempo,
        tempoDuring,        
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
import Music.Score.Combinators
import Music.Score.Util
import Music.Pitch.Literal

-- Tempo _ t means that
--  stretch t notation = sounding
--  1   -> whole = 1   sec (1/1 = 60)
--  0.5 -> whole = 0.5 sec (1/1 = 120)
--  2   -> whole = 2   sec (1/4 = 120)
data Tempo = Tempo (Maybe String) Duration
    deriving (Eq, Ord, Show, Typeable)

-- beats +, tempo -, duration +
-- bpm +,   tempo +  duration -


metronome :: Duration -> Duration -> Tempo
metronome noteVal bpm = Tempo Nothing $ (noteVal * 60) / bpm

tempo :: (HasMeta a, HasPart' a, HasOnset a, HasOffset a) => Tempo -> a -> a
tempo c x = tempoDuring (era x) c x

tempoDuring :: (HasMeta a, HasPart' a) => Span -> Tempo -> a -> a
tempoDuring s c = addGlobalMetaNote (s =: (Option $ Just $ Last c))


