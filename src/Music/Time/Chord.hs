
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

module Music.Time.Chord (
    -- * Music.Time.Chord
    Chord,
    -- ** Substructure
    chord,
    -- ** TODO
    
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
import           Music.Time.Delayed

import Control.Monad.Compose
import           Control.Applicative
import           Control.Arrow                (first, second, (***), (&&&))
import           Control.Lens                 hiding (Indexable, Level, above,
                                               below, index, inside, parts,
                                               reversed, transform, (|>), (<|))
import           Control.Monad
import           Control.Monad.Plus
import           Data.Foldable                (Foldable)
import qualified Data.Foldable                as Foldable
import           Data.Traversable             (Traversable)
import qualified Data.Traversable             as T
import           Data.Typeable

-- |
-- A 'Chord' is a parallel composition of values.
--
-- @
-- type Chord a = [Delayed a]
-- @
--
newtype Chord a = Chord { getChord :: ChordList (ChordEv a) }
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Typeable, Show, Eq)

-- Can use [] or Seq here
type ChordList = []

-- Can use any type as long as chordEv provides an Iso
type ChordEv a = Delayed a

chordEv :: Iso (Delayed a) (Delayed b) (ChordEv a) (ChordEv b)
chordEv = id

instance Applicative Chord where
  pure  = return
  (<*>) = ap

instance Monad Chord where
  return = view _Unwrapped . return . return
  xs >>= f = view _Unwrapped $ (view _Wrapped . f) `mbind` view _Wrapped xs

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (Chord a) where
  type Unwrapped (Chord a) = (ChordList (ChordEv a))
  _Wrapped' = iso getChord Chord

instance Rewrapped (Chord a) (Chord b)

instance Transformable (Chord a) where
  transform s = over _Wrapped' (transform s)

instance HasDuration (Chord a) where
  _duration = Foldable.sum . fmap _duration . view _Wrapped'

instance Splittable a => Splittable (Chord a) where
  -- TODO

instance Reversible a => Reversible (Chord a) where
  rev = over _Wrapped' (fmap rev) -- TODO OK?

-- TODO
-- instance HasMeta (Chord a) where
  -- meta = error "Not implemented: meta" 

-- TODO
-- type instance Pitch (Chord a) = Pitch a
-- type instance SetPitch g (Chord a) = Chord (SetPitch g a)
-- instance (HasPitches a b) => HasPitches (Chord a) (Chord b) where
--   pitches = _Wrapped . traverse . from chordEv . _Wrapped . whilstLT pitches


chord :: Lens (Chord a) (Chord b) [Delayed a] [Delayed b]
chord = _Wrapped 


