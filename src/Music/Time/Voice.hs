
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Music.Time.Voice (
    -- * Music.Time.Voice
    Voice,

    -- ** Substructure
    voice,
    stretcheds,
    singleStretched,

    -- ** Zips
    zipVoice,
    zipVoiceWith,
    dzipVoiceWith,
    mergeEqualNotes,
    
    -- mapDurations, -- ([Duration] -> [Duration]) -> Voice a -> Voice a
    -- mapPitches,   -- ([Pitch a]  -> [Pitch a])  -> Voice a -> Voice a
    -- etc
  ) where

import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Ratio
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.VectorSpace

import           Music.Time.Split
import           Music.Time.Reverse
import           Music.Time.Stretched

-----
import Control.Monad.Compose
import Music.Time.Util

import Data.Fixed
import           Data.Default
import           Data.Ratio

import           Control.Applicative
import           Control.Arrow                (first, second, (***), (&&&))
import qualified Control.Category
import           Control.Comonad
import           Control.Comonad.Env
import           Control.Lens                 hiding (Indexable, Level, above,
                                               below, index, inside, parts,
                                               reversed, transform, (|>), (<|))
import           Control.Monad
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Distributive
import           Data.Foldable                (Foldable)
import qualified Data.Foldable                as Foldable
import           Data.Functor.Rep
import qualified Data.List
import           Data.List.NonEmpty           (NonEmpty)
import           Data.Maybe
import           Data.NumInstances
import           Data.Semigroup               hiding ()
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.Traversable             (Traversable)
import qualified Data.Traversable             as T
import           Data.Typeable
import           Data.VectorSpace hiding (Sum(..))
import           Music.Dynamics.Literal
import           Music.Pitch.Literal

import qualified Data.Ratio                   as Util_Ratio
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import qualified Data.Ord as Ord
-----

-- |
-- A 'Voice' is a sequential composition of values. Events may not overlap.
--
-- @
-- type Voice a = [Stretched a]
-- @
--
newtype Voice a = Voice { getVoice :: VoiceList (VoiceEv a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

-- Can use [] or Seq here
type VoiceList = []

-- Can use any type as long as voiceEv provides an Iso
type VoiceEv a = Stretched a

voiceEv :: Iso (Stretched a) (Stretched b) (VoiceEv a) (VoiceEv b)
voiceEv = id

instance Applicative Voice where
  pure  = return
  (<*>) = ap

instance Alternative Voice where
  (<|>) = (<>)
  empty = mempty

instance Monad Voice where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (Voice a) where
  type Unwrapped (Voice a) = (VoiceList (VoiceEv a))
  _Wrapped' = iso getVoice Voice

instance Rewrapped (Voice a) (Voice b)

instance Transformable (Voice a) where
  transform s = over _Wrapped' (transform s)

instance HasDuration (Voice a) where
  _duration = Foldable.sum . fmap _duration . view _Wrapped'

instance Splittable a => Splittable (Voice a) where
  -- TODO

instance Reversible a => Reversible (Voice a) where
  rev = over _Wrapped' (fmap rev) -- TODO OK?

-- TODO
-- instance HasMeta (Voice a) where
  -- meta = error "Not implemented: meta" 

-- TODO
-- type instance Pitch (Voice a) = Pitch a
-- type instance SetPitch g (Voice a) = Voice (SetPitch g a)
-- instance (HasPitches a b) => HasPitches (Voice a) (Voice b) where
--   pitches = _Wrapped . traverse . from voiceEv . _Wrapped . whilstLD pitches



-- |
-- Create a score from a list of notes.
--
-- This is a getter (rather than a function) for consistency:
--
-- @
-- [ (0 '<->' 1, 10)^.'note',
--   (1 '<->' 2, 20)^.'note',
--   (3 '<->' 4, 30)^.'note' ]^.'score'
-- @
-- 
-- @
-- 'view' 'score' $ 'map' ('view' 'note') [(0 '<->' 1, 1)]
-- @
--
-- Se also 'notes'.
--
voice :: Getter [Stretched a] (Voice a)
voice = to $ flip (set stretcheds) empty
{-# INLINE voice #-}

stretcheds :: Lens (Voice a) (Voice b) [Stretched a] [Stretched b]
stretcheds = unsafeVoice

singleStretched :: Prism' (Voice a) (Stretched a)
singleStretched = unsafeVoice . single

unsafeVoice :: Iso (Voice a) (Voice b) [Stretched a] [Stretched b]
unsafeVoice = _Wrapped


{-
-- |
-- Voice
--
voiceNotes :: Traversal (Voice a) (Voice b) (Note a) (Note b)
voiceNotes = error "Not implemented: voiceNotes"
-}

-- |
-- Voice
--
voiceElements :: Traversal (Voice a) (Voice b) (Stretched a) (Stretched b)
voiceElements = _Wrapped . traverse . from voiceEv   


-- |
-- Join the given voices by multiplying durations and pairing values.
--
zipVoice :: Voice a -> Voice b -> Voice (a, b)
zipVoice = zipVoiceWith (,)

-- |
-- Join the given voices by multiplying durations and combining values using the given function.
--
zipVoiceWith :: (a -> b -> c) -> Voice a -> Voice b -> Voice c
zipVoiceWith  = error "Not implemented: zipVoiceWith"

-- |
-- Join the given voices by combining durations and values using the given function.
--
dzipVoiceWith :: (Duration -> Duration -> a -> b -> (Duration, c)) -> Voice a -> Voice b -> Voice c
dzipVoiceWith = error "Not implemented: dzipVoiceWith"


voiceList :: Iso' (Voice a) [(Duration, a)]
voiceList = error "Not implemented: voiceList"

-- |
-- Merge consecutive equal note.
--
mergeEqualNotes :: Eq a => Voice a -> Voice a
mergeEqualNotes = over voiceList $ fmap f . Data.List.groupBy (inspecting snd)
  where
    f dsAs = let (ds,as) = unzip dsAs in (sum ds, head as) 