
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

module Music.Score.Export.ArticulationNotation (
    Slur(..),
    Mark(..),
    ArticulationNotation(..),
    notateArticulation,
  ) where

import Data.Semigroup
import Data.Functor.Context
import Control.Lens -- ()
import Music.Score.Articulation
import Music.Score.Instances
import Music.Score.Ties
import Music.Time

-- TODO need NoSlur etc?

data Slur = NoSlur | BeginSlur | EndSlur
  deriving (Eq, Ord, Show)

-- data CrescDim = NoCrescDim | BeginCresc | EndCresc | BeginDim | EndDim
data Mark = NoMark | Staccato | MoltoStaccato | Marcato | Accent | Tenuto
  deriving (Eq, Ord, Show)

instance Monoid Slur where
  mempty = NoSlur
  mappend NoSlur a = a
  mappend a NoSlur = a
  mappend a _ = a

instance Monoid Mark where
  mempty = NoMark
  mappend NoMark a = a
  mappend a NoMark = a
  mappend a _ = a

newtype ArticulationNotation 
  = ArticulationNotation { getArticulationNotation :: ([Slur], [Mark]) }

instance Wrapped ArticulationNotation where
  type Unwrapped ArticulationNotation = ([Slur], [Mark])
  _Wrapped' = iso getArticulationNotation ArticulationNotation

instance Rewrapped ArticulationNotation ArticulationNotation

type instance Articulation ArticulationNotation = ArticulationNotation

instance Transformable ArticulationNotation where
  transform _ = id

instance Tiable ArticulationNotation where
  toTied (ArticulationNotation (beginEnd, marks)) 
    = (ArticulationNotation (beginEnd, marks), 
       ArticulationNotation (mempty, mempty))

instance Monoid ArticulationNotation where
  mempty = ArticulationNotation ([], [])
  ArticulationNotation ([], []) `mappend` y = y
  x `mappend` ArticulationNotation ([], []) = x
  x `mappend` y = x

-- TODO add slurs if separation is below some value...

getSeparationMarks :: Double -> [Mark]
getSeparationMarks = fst . getSeparationMarks'

hasSlur' :: Double -> Bool
hasSlur' = snd . getSeparationMarks'

getSeparationMarks' :: Double -> ([Mark], Bool)
getSeparationMarks' x
  |              x <= (-1) = ([], True)
  | (-1) <  x && x <  1    = ([], False)
  | 1    <= x && x <  2    = ([Staccato], False)
  | 2    <= x              = ([MoltoStaccato], False)

getAccentMarks :: Double -> [Mark]
getAccentMarks x
  |              x <= (-1) = []
  | (-1) <  x && x <  1    = []
  | 1    <= x && x <  2    = [Accent]
  | 2    <= x              = [Marcato]
  | otherwise           = []

hasSlur y = hasSlur' (realToFrac $ view separation $ y)
allMarks y = getSeparationMarks (realToFrac $ view separation $ y) <> getAccentMarks (realToFrac $ view accentuation $ y)

notateArticulation :: (Ord a, a ~ (Sum Double, Sum Double)) => Ctxt a -> ArticulationNotation
notateArticulation (Nothing, y, Nothing) = ArticulationNotation ([], allMarks y)
notateArticulation (Just x,  y, Nothing) = ArticulationNotation (if hasSlur x && hasSlur y then [EndSlur] else [], allMarks y)
notateArticulation (Nothing, y, Just z)  = ArticulationNotation (if hasSlur y && hasSlur z then [BeginSlur] else [], allMarks y)
notateArticulation (Just x,  y, Just z)  = ArticulationNotation (slur3 x y z, allMarks y)
  where
    slur3 x y z = case (hasSlur x, hasSlur y, hasSlur z) of
      (True, True, True)  -> [{-ContSlur-}]
      (False, True, True) -> [BeginSlur]
      (True, True, False) -> [EndSlur]
      _                   -> []

