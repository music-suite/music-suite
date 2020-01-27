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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
module Music.Score.Export.TechniqueNotation
  (
    TechniqueNotation (..),
    notateTechnique,
  )
where

import Control.Lens -- ()
import Data.AffineSpace
import Data.Functor.Context
import Data.Semigroup
import Music.Score.Dynamics
import Music.Score.Phrases
import Music.Score.Ties
import Music.Time
import Music.Score.Technique

-- TODO this works for pizz/arco, col legno etc, but not for all techniques...
newtype TechniqueNotation = TechniqueNotation [String]
  deriving (Semigroup, Monoid)

notateTechnique :: Ctxt SomeTechnique-> TechniqueNotation
notateTechnique _ = TechniqueNotation ["TODO"]
