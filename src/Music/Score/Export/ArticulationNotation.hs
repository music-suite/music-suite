{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

-- |
-- Copyright   : (c) Hans Hoglund 2012-2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
module Music.Score.Export.ArticulationNotation
  ( Slur (..),
    Mark (..),
    ArticulationNotation (..),
    slurs,
    marks,
    notateArticulation,
  )
where

import Control.Lens -- ()
import Data.Functor.Context
import Data.Semigroup
import qualified Music.Articulation
import Music.Score.Articulation (Accentuation, Articulated (..), GetArticulation, Separation)
import qualified Music.Score.Articulation
import Music.Score.Ties (Tiable (..))
import Music.Time (Transformable (..))

-- TODO need NoSlur etc?

data Slur = NoSlur | BeginSlur | EndSlur
  deriving (Eq, Ord, Show)

{-
TODO
  dashed slur
  trill
  8va and variants
-}
data Mark
  = NoMark
  | Staccato
  | MoltoStaccato
  | Marcato
  | Accent
  | Tenuto
  | Trill -- just trill, no line
  | Turn Bool -- inverted?
  | Mordent Bool -- inverted?
  | UpBow
  | DownBow
  | Circle
  deriving (Eq, Ord, Show)

instance Monoid Slur where

  mempty = NoSlur

  mappend = (<>)

instance Semigroup Slur where
  NoSlur <> a = a
  a <> _ = a

instance Monoid Mark where

  mempty = NoMark

  mappend = (<>)

instance Semigroup Mark where
  NoMark <> a = a
  a <> _ = a

newtype ArticulationNotation
  = ArticulationNotation {getArticulationNotation :: ([Slur], [Mark])}
  deriving (Eq, Ord, Show)

instance Wrapped ArticulationNotation where

  type Unwrapped ArticulationNotation = ([Slur], [Mark])

  _Wrapped' = iso getArticulationNotation ArticulationNotation

instance Rewrapped ArticulationNotation ArticulationNotation

type instance GetArticulation ArticulationNotation = ArticulationNotation

instance Transformable ArticulationNotation where
  transform _ = id

instance Tiable ArticulationNotation where
  toTied (ArticulationNotation (slur, marks)) =
    ( ArticulationNotation (slur1, marks1),
      ArticulationNotation (slur2, marks2)
    )
    where
      (marks1, marks2) = splitMarks marks
      (slur1, slur2) = splitSlurs slur
      splitSlurs = unzipR . fmap splitSlur
      splitMarks = unzipR . fmap splitMark
      splitSlur NoSlur = (mempty, mempty)
      splitSlur BeginSlur = (BeginSlur, mempty)
      splitSlur EndSlur = (mempty, EndSlur)
      splitMark NoMark = (NoMark, mempty)
      splitMark Staccato = (Staccato, mempty)
      splitMark MoltoStaccato = (MoltoStaccato, mempty)
      splitMark Marcato = (Marcato, mempty)
      splitMark Accent = (Accent, mempty)
      splitMark Tenuto = (Tenuto, mempty)
      splitMark Trill = (Trill, mempty)
      splitMark (Turn x) = (Turn x, mempty)
      splitMark (Mordent x) = (Mordent x, mempty)
      splitMark UpBow = (UpBow, mempty)
      splitMark DownBow = (DownBow, mempty)
      splitMark Circle = (Circle, mempty)

instance Monoid ArticulationNotation where

  mempty = ArticulationNotation ([], [])

  mappend = (<>)

instance Semigroup ArticulationNotation where
  ArticulationNotation ([], []) <> y = y
  x <> _ = x

slurs :: Lens' ArticulationNotation [Slur]
slurs = _Wrapped' . _1

marks :: Lens' ArticulationNotation [Mark]
marks = _Wrapped' . _2

getSeparationMarks :: Double -> [Mark]
getSeparationMarks = fst . getSeparationMarks'

hasSlur' :: Double -> Bool
hasSlur' = snd . getSeparationMarks'

getSeparationMarks' :: Double -> ([Mark], Bool)
getSeparationMarks' x
  | x <= (-1) = ([], True)
  | (-1) < x && x < 1 = ([], False)
  | 1 <= x && x < 2 = ([Staccato], False)
  | 2 <= x = ([MoltoStaccato], False)
  | otherwise = error "Impossible"

getAccentMarks :: Double -> [Mark]
getAccentMarks x
  | x <= (-1) = []
  | (-1) < x && x < 1 = []
  | 1 <= x && x < 2 = [Accent]
  | 2 <= x = [Marcato]
  | otherwise = []

hasSlur :: (Real (Separation t), Articulated t) => t -> Bool
hasSlur y = hasSlur' (realToFrac $ view separation $ y)

allMarks :: (Real (Separation t), Real (Accentuation t), Articulated t) => t -> [Mark]
allMarks y =
  mempty
    <> getSeparationMarks (realToFrac $ y ^. separation)
    <> getAccentMarks (realToFrac $ y ^. accentuation)

notateArticulation :: (Ord a, Articulated a, Real (Separation a), Real (Accentuation a)) => Ctxt a -> ArticulationNotation
notateArticulation (getCtxt -> x) = go x
  where
    go (Nothing, y, Nothing) = ArticulationNotation ([], allMarks y)
    go (Just x, y, Nothing) = ArticulationNotation (if hasSlur x && hasSlur y then [EndSlur] else [], allMarks y)
    go (Nothing, y, Just z) = ArticulationNotation (if hasSlur y && hasSlur z then [BeginSlur] else [], allMarks y)
    go (Just x, y, Just z) = ArticulationNotation (slur3 x y z, allMarks y)
      where
        slur3 x y z = case (hasSlur x, hasSlur y, hasSlur z) of
          (True, True, True) -> []
          {-ContSlur-}
          (False, True, True) -> [BeginSlur]
          (True, True, False) -> [EndSlur]
          _ -> []

unzipR :: Functor f => f (a, b) -> (f a, f b)
unzipR x = (fmap fst x, fmap snd x)
