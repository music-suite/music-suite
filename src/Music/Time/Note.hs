
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
-------------------------------------------------------------------------------------

module Music.Time.Note (
    -- * Music.Time.Note
    Note,
    note,
    -- fromNote,
    event,
    noteValue,
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

import           Music.Time.Reverse
import           Music.Time.Split

import           Control.Applicative
import           Control.Arrow          (first, second, (&&&), (***))
import           Control.Comonad
import           Control.Comonad.Env
import           Control.Lens           hiding (Indexable, Level, above, below,
                                         index, inside, parts, reversed,
                                         transform, (<|), (|>))
import           Data.Foldable          (Foldable)
import qualified Data.Foldable          as Foldable
import           Data.PairMonad
import           Data.Typeable
import           Music.Time.Util (tripped, through)

-- |
-- A 'Note' is a value with an 'onset' and and 'offset' in time. It is an instance
-- of 'Transformable'.
--
-- You can use 'value' to apply a function in the context of the transformation,
-- i.e.
--
-- @
-- over value (* line) (delay 2 $ return line)
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
newtype Note a = Note { _noteValue :: (Span, a) }
  deriving (Typeable)

deriving instance Eq a => Eq (Note a)
deriving instance Functor Note
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

-- instance ComonadEnv Span Note where
  -- ask = noteValueSpan

-- | Unsafe: Do not use 'Wrapped' instances
instance Wrapped (Note a) where
  type Unwrapped (Note a) = (Span, a)
  _Wrapped' = iso _noteValue Note

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
noteValue :: (Transformable a, Transformable b) =>
  Lens
    (Note a) (Note b)
    a b
noteValue = lens runNote (flip $ mapNote . const)
  where
    runNote = uncurry transform . view _Wrapped
    mapNote f (view (from note) -> (s,x)) = view note (s, f `whilst` negateV s $ x)
{-# INLINE noteValue #-}

fromNote = from note

-- |
-- View a note as an events, i.e. a time-duration-value triplet. 
--
event :: Iso (Note a) (Note b) (Time, Duration, a) (Time, Duration, b)
event = from note . bimapping delta id . tripped



