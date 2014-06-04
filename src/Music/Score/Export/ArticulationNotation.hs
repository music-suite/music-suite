
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
getSeparationMarks x
  |              x <= (-1) = [] -- slur
  | (-1) <  x && x <  1    = []
  | 1    <= x && x <  2    = [Staccato]
  | 2    <= x              = [MoltoStaccato]

getAccentMarks :: Double -> [Mark]
getAccentMarks x
  |              x <= (-1) = []
  | (-1) <  x && x <  1    = []
  | 1    <= x && x <  2    = [Accent]
  | 2    <= x              = [Marcato]
  | otherwise           = []


notateArticulation :: (Ord a, a ~ (Sum Double, Sum Double)) => Ctxt a -> ArticulationNotation
notateArticulation x = ArticulationNotation 
  (
  [NoSlur], 
  getSeparationMarks (getSum $ view separation $ extractCtxt x) <> getAccentMarks (getSum $ view accentuation $ extractCtxt x)
  )
