
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
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

module Music.Time.Nominal where

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

newtype Nominal f a = Nominal { getNominal :: f a }
  deriving (
  	Eq, 
  	Ord,
  	Read, 
  	Show, 
  	Functor, 
  	Foldable, 
  	Traversable, 
  	Monad
  )

instance Applicative f => Applicative (Nominal f) where
  pure = Nominal . pure
  Nominal f <*> Nominal x = Nominal (f <*> x)

instance Transformable (Nominal f a) where
  transform _ = id

instance Splittable (Nominal f a) where
  split _ x = (x,x)

instance Reversible (f a) => Reversible (Nominal f a) where
  rev (Nominal x) = Nominal (rev x)

