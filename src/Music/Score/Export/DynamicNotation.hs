
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
-------------------------------------------------------------------------------------

module Music.Score.Export.DynamicNotation (
    CrescDim(..),
    DynamicNotation(..),
    crescDim,
    dynamicLevel,
    
    notateDynamic,

    -- * Utility
    removeCloseDynMarks,
  ) where

import Data.Semigroup
import Data.Functor.Context
import Data.AffineSpace
import Control.Lens -- ()
import Music.Score.Dynamics
import Music.Score.Ties
import Music.Score.Phrases
import Music.Time


data CrescDim = NoCrescDim | BeginCresc | EndCresc | BeginDim | EndDim
  deriving (Eq, Ord, Show)

instance Monoid CrescDim where
  mempty = NoCrescDim
  mappend NoCrescDim a = a
  mappend a NoCrescDim = a
  mappend a _ = a

newtype DynamicNotation
  = DynamicNotation { getDynamicNotation :: ([CrescDim], Maybe Double) }
  deriving (Eq, Ord, Show)

instance Wrapped DynamicNotation where
  type Unwrapped DynamicNotation = ([CrescDim], Maybe Double)
  _Wrapped' = iso getDynamicNotation DynamicNotation

instance Rewrapped DynamicNotation DynamicNotation

type instance Dynamic DynamicNotation = DynamicNotation

instance Transformable DynamicNotation where
  transform _ = id

instance Tiable DynamicNotation where
  toTied (DynamicNotation (beginEnd, marks))
    = (DynamicNotation (beginEnd, marks),
       DynamicNotation (mempty, Nothing))

instance Monoid DynamicNotation where
  mempty = DynamicNotation ([], Nothing)
  DynamicNotation ([], Nothing) `mappend` y = y
  x `mappend` DynamicNotation ([], Nothing) = x
  x `mappend` y = x

crescDim :: Lens' DynamicNotation [CrescDim]
crescDim = _Wrapped' . _1

dynamicLevel :: Lens' DynamicNotation (Maybe Double)
dynamicLevel = _Wrapped' . _2

-- Given a dynamic value and its context, decide:
--
--   1) Whether we should begin or end a crescendo or diminuendo
--   2) Whether we should display the current dynamic value
--
notateDynamic :: (Ord a, Real a) => Ctxt a -> DynamicNotation
notateDynamic x = DynamicNotation $ over _2 (\t -> if t then Just (realToFrac $ extractCtxt x) else Nothing) $ case getCtxt x of
  (Nothing, y, Nothing) -> ([], True)
  (Nothing, y, Just z ) -> case (y `compare` z) of
    LT      -> ([BeginCresc], True)
    EQ      -> ([],           True)
    GT      -> ([BeginDim],   True)
  (Just x,  y, Just z ) -> case (x `compare` y, y `compare` z) of
    (LT,LT) -> ([NoCrescDim], False)
    (LT,EQ) -> ([EndCresc],   True)
    (EQ,LT) -> ([BeginCresc], False{-True-})

    (GT,GT) -> ([NoCrescDim], False)
    (GT,EQ) -> ([EndDim],     True)
    (EQ,GT) -> ([BeginDim],   False{-True-})

    (EQ,EQ) -> ([],                   False)
    (LT,GT) -> ([EndCresc, BeginDim], True)
    (GT,LT) -> ([EndDim, BeginCresc], True)
  (Just x,  y, Nothing) -> case (x `compare` y) of
    LT      -> ([EndCresc],   True)
    EQ      -> ([],           False)
    GT      -> ([EndDim],     True)


removeCloseDynMarks :: (HasPhrases' s a, HasDynamics' a, Dynamic a ~ DynamicNotation, a ~ SetDynamic (Dynamic a) a) => s -> s
removeCloseDynMarks = mapPhrasesWithPrevAndCurrentOnset f
  where
    f Nothing t    = id
    f (Just t1) t2 = if (t2 .-. t1) > 1.5 then id else over (_head.mapped) removeDynMark

removeDynMark :: (HasDynamics' a, Dynamic a ~ DynamicNotation, a ~ SetDynamic (Dynamic a) a) => a -> a
removeDynMark x = set (dynamics' . _Wrapped' . _2) Nothing x
