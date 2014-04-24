
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

module Music.Time.Note (
    -- * Music.Time.Note
    Note,
    note,
    getNote,    
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

import           Music.Time.Position
import           Music.Time.Split
import           Music.Time.Reverse

-----
import Data.PairMonad
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
-- A 'Note' is a value with an 'onset' and and 'offset' in time. It is an instance
-- of 'Transformable'.
--
-- You can use 'value' to apply a function in the context of the transformation,
-- i.e.
--
-- @
-- over value (* time) (delay 2 $ return time)
-- @
--
-- @
-- ('view' 'value') . 'transform' s = 'transform' s . ('view' 'value')
-- @
--
-- The semantics are given by
--
-- @
-- type Note a = (Span, a)
-- @
--
newtype Note a = Note { _getNote :: (Span, a) }

deriving instance Eq a => Eq (Note a)
deriving instance Functor Note
deriving instance Typeable1 Note
deriving instance Foldable Note
deriving instance Traversable Note
deriving instance Comonad Note

instance (Show a, Transformable a) => Show (Note a) where
  show x = show (x^.from note) ++ "^.note"

-- |
-- Note is a 'Monad' and 'Applicative' in the style of pair, with 'return' placing a value
-- at the default span 'mempty' and 'join' composing time transformations.
deriving instance Monad Note
deriving instance Applicative Note

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (Note a) where
  type Unwrapped (Note a) = (Span, a)
  _Wrapped' = iso _getNote Note

instance Rewrapped (Note a) (Note b)

instance Transformable (Note a) where
  transform t = over _Wrapped $ first (transform t)

instance HasDuration (Note a) where
  _duration = _duration . ask . view _Wrapped

instance HasPosition (Note a) where
  x `_position` p = ask (view _Wrapped x) `_position` p

instance Splittable a => Splittable (Note a) where
  -- beginning d = over _Wrapped $ \(s, v) -> (beginning d s, beginning (transform (negateV s) d) v)
  beginning d = over _Wrapped $ \(s, v) -> (beginning d s, beginning (d / _duration s) v)
  ending    d = over _Wrapped $ \(s, v) -> (ending    d s, ending    (1 - (d / _duration s)) v)

instance Reversible (Note a) where
  rev = revDefault

-- |
-- View a note as a pair of the original value and the transformation (and vice versa).
--
note :: ({-Transformable a, Transformable b-}) => 
  Iso 
    (Span, a) (Span, b) 
    (Note a) (Note b)
note = _Unwrapped

-- |
-- View the value in the note.
--
getNote :: (Transformable a, Transformable b) => 
  Lens 
    (Note a) (Note b) 
    a b
getNote = lens runNote (flip $ mapNote . const)
  where
    runNote = uncurry transform . view _Wrapped
    mapNote f (view (from note) -> (s,x)) = view note (s, f `whilst` negateV s $ x)

{-# INLINE getNote #-}

