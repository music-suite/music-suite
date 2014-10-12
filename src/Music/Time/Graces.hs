
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012–2014
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-------------------------------------------------------------------------------------

module Music.Time.Graces where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Compose
import           Control.Monad.Plus
import           Data.Foldable          (Foldable)
import qualified Data.Foldable          as Foldable
import           Data.Traversable       (Traversable)
import qualified Data.Traversable       as T
import           Data.Typeable

import Music.Time.Split
import Music.Time.Reverse
import Music.Time.Nominal
import Music.Time.Voice

newtype Graces f a = Graces { getGraces :: (Nominal f a, f a, Nominal f a) }
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- TODO move
instance Alternative f => Alternative (Nominal f) where
  empty = Nominal empty
  Nominal a <|> Nominal b = Nominal (a <|> b)

instance (Applicative f, Alternative f) => Applicative (Graces f) where
  pure x = Graces (empty, pure x, empty)

-- deriving instance Transformable a => Transformable (Graces a) where
-- deriving instance Splittable a => Splittable (Graces a) where
-- deriving instance Reversible a => Reversible (Graces a) where

