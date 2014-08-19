
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
        splitAbs,
    
        chunks,

  ) where

import           Music.Time.Position
import           Music.Time.Internal.Util

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
-- To split at an absolute position, see 'splitAbs'.
--
--
-- Law
--
-- @
-- '_duration' ('beginning' t x) + '_duration' ('ending' t x) = '_duration' x
-- '_duration' ('beginning' t x) = t ``min`` '_duration' x                    iff t >= 0
-- '_duration' ('ending' t x)    = '_duration' x - (t ``min`` '_duration' x)    iff t >= 0
-- @
--
-- (Note that any of these three laws can be derived from the other two, so it is
-- sufficient to prove two!).
--
-- >>> (\x -> fmap (flip split x) [-2,-1,0,0.5,1,2]) $ (1::Duration)
-- [(0,1),(0,1),(0,1),((1/2),(1/2)),(1,0),(1,0)]
--
-- >>> (\x -> fmap (flip split x) [-2,-1,0,0.5,1,2]) $ (0<->1)
-- [(0 <-> 0,0 <-> 1),(0 <-> 0,0 <-> 1),(0 <-> 0,0 <-> 1),(0 <-> (1/2),(1/2) <-> 1),(0 <-> 1,1 <-> 1),(0 <-> 1,1 <-> 1)]
--
class Splittable a where
  split      :: Duration -> a -> (a, a)
  beginning  :: Duration -> a -> a
  ending     :: Duration -> a -> a
  split   d x = (beginning d x, ending d x)
  beginning d = fst . split d
  ending    d = snd . split d
-- TODO rename beginning/ending to fstSplit/sndSplit or similar

instance Splittable () where
  split _ x = (x, x)

instance Splittable Duration where
  -- Directly from the laws
  -- Guard against t < 0
  split t x = (t' `min` x, x ^-^ (t' `min` x))
    where t' = t `max` 0

instance Splittable Span where
  -- Splitting a span splits the duration
  split pos (view delta -> (t, d)) = (t >-> d1, (t .+^ d1) >-> d2)
    where (d1, d2) = split pos d

instance (Ord k, Splittable a) => Splittable (Map k a) where
  split d = unzipR . Map.map (split d)


-- takeMWhile :: (Monoid a, HasDuration a, Splittable a) => Duration -> (a -> Bool) -> a -> a
-- takeMWhile d p xs = if _duration xs <= 0 then mempty else takeMWhile' d p xs
--   where
--     takeMWhile' d p (split d -> (x, xs)) = if p x then x `mappend` takeMWhile d p xs else mempty

chunks :: (Splittable a, HasDuration a) => Duration -> a -> [a]
chunks d xs = if _duration xs <= 0 then [] else chunks' d xs
  where
    chunks' d (split d -> (x, xs)) = [x] ++ chunks d xs


splitAbs :: (HasPosition a, Splittable a) => Time -> a -> (a, a)
splitAbs t x = split (t .-. _onset x) x


