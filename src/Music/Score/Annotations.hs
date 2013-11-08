
{-# LANGUAGE 
    ScopedTypeVariables, 
    GeneralizedNewtypeDeriving,
    DeriveFunctor, 
    DeriveFoldable, 
    DeriveTraversable,
    DeriveDataTypeable, 
    ConstraintKinds,
    FlexibleContexts, 
    GADTs, 
    ViewPatterns,
    TypeFamilies,
    MultiParamTypeClasses, 
    FlexibleInstances #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides annotations
--
-------------------------------------------------------------------------------------


module Music.Score.Annotations (
        Annotation,
        annotate,
        showAnnotations,
  ) where

import Control.Arrow
import Control.Monad.Plus       
import Data.Void
import Data.Maybe
import Data.Semigroup
import Data.Typeable
import Data.String
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Music.Time
import Music.Time.Reactive
import Music.Score.Note
import Music.Score.Score
import Music.Score.Part
import Music.Score.Combinators (withMeta)
import Music.Score.Ornaments (HasText, text)
import Music.Score.Meta
import Music.Score.Util

newtype Annotation = Annotation { getAnnotation :: [String] }
    deriving (Semigroup, Monoid, Typeable)
instance IsString Annotation where fromString = Annotation . return

annotate :: String -> Score a -> Score a
annotate str x = addMetaNote (stretch (duration x) $ return $ Annotation [str]) x

showAnnotations :: (HasPart' a, HasText a) => Score a -> Score a
showAnnotations = withMeta (\x s -> foldr (text . (":"++)) s $ getAnnotation x)
