
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

module Music.Time.Split (
      module Music.Time.Position,

      -- * The Splittable class
      Splittable(..),
      chunks,
  ) where

import           Music.Time.Position
import           Music.Time.Util

import           Control.Lens            hiding (Indexable, Level, above, below,
                                          index, inside, parts, reversed,
                                          transform, (<|), (|>))
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Functor.Adjunction (unzipR)
import           Data.Functor.Rep
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Semigroup          hiding ()
import           Data.Sequence           (Seq)
import qualified Data.Sequence           as Seq
import           Data.VectorSpace        hiding (Sum (..))

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
class Splittable a where
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

instance (Ord k, Splittable a) => Splittable (Map k a) where
  split d = unzipR . Map.map (split d)


takeMWhile :: (Monoid a, HasDuration a, Splittable a) => Duration -> (a -> Bool) -> a -> a
takeMWhile d p xs = if _duration xs <= 0 then mempty else takeMWhile' d p xs
  where
    takeMWhile' d p (split d -> (x, xs)) = if p x then x `mappend` takeMWhile d p xs else mempty

--     > Data.List.mapAccumL (\x t -> swap $ split t x) (0 <-> 10) (replicate 10 2)

chunks :: (Splittable a, HasDuration a) => Duration -> a -> [a]
chunks d xs = if _duration xs <= 0 then [] else chunks' d xs
  where
    chunks' d (split d -> (x, xs)) = [x] ++ chunks d xs

