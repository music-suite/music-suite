
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

module Music.Time.Split (
      -- * Music.Time.Split
      -- * The Splittable class
      Splittable(..),
      chunks,      
  ) where

import Music.Score.Util
import           Music.Time.Position

-----
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
-- Class of values that can be split.
--
-- For non-positioned values such as 'Stretched', split cuts a value into pieces
-- of the given duration and the rest.
--
-- For positioned values succh as 'Note', split cuts a value relative to its onset.
--
--
-- Law
-- 
-- @
-- '_duration' ('beginning' t x) + '_duration' ('ending' t x) = '_duration' x
-- '_duration' ('beginning' t x) = t ``min`` '_duration' x
-- @
--
class HasDuration a => Splittable a where
  split      :: Duration -> a -> (a, a)
  beginning  :: Duration -> a -> a
  ending     :: Duration -> a -> a
  split   d x = (beginning d x, ending d x)
  beginning d = fst . split d
  ending    d = snd . split d

instance Splittable Duration where
  split x y = (x `min` y, y ^-^ (x `min` y))

instance Splittable Span where
  -- split d (view range -> (t1, t2)) = (t1 <-> (t1 .+^ d), (t1 .+^ d) <-> t2)
  split d' (view delta -> (t, d)) = let (d1, d2) = split d' d in (t >-> d1, (t.+^d1) >-> d2)

takeMWhile :: (Monoid a, Splittable a) => Duration -> (a -> Bool) -> a -> a
takeMWhile d p xs = if _duration xs <= 0 then mempty else takeMWhile' d p xs
  where
    takeMWhile' d p (split d -> (x, xs)) = if p x then x `mappend` takeMWhile d p xs else mempty

--     > Data.List.mapAccumL (\x t -> swap $ split t x) (0 <-> 10) (replicate 10 2)

chunks :: Splittable a => Duration -> a -> [a]
chunks d xs = if _duration xs <= 0 then [] else chunks' d xs
  where
    chunks' d (split d -> (x, xs)) = [x] ++ chunks d xs

