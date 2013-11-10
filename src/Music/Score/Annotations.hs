
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
        annotateSpan,
        showAnnotations,
        showAnnotations',
        withAnnotations,
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
import Music.Score.Combinators (withMetaNP)
import Music.Score.Ornaments (HasText, text)
import Music.Score.Meta
import Music.Score.Util

-- | 
--   An annotation is a simple textual values attached to parts of a score.
--   They are ignored by default, but can be collected with 'withAnnotations'.
--
newtype Annotation = Annotation { getAnnotation :: [String] }
    deriving (Semigroup, Monoid, Typeable)
instance IsString Annotation where fromString = Annotation . return

-- | Annotate the whole score.
annotate :: String -> Score a -> Score a
annotate str x = annotateSpan (start >-> duration x) str x

-- | Annotate the whole score.
annotateSpan :: Span -> String -> Score a -> Score a
annotateSpan span str x = addMetaNoteNP (sapp span $ return $ Annotation [str]) x
-- TODO partwise
-- XXX more sensible way of finding the part of a score

-- | Add the annotations to the score as text.
showAnnotations :: (HasPart' a, HasText a) => Score a -> Score a
showAnnotations = showAnnotations' ":"

-- | Add the annotations to the score as text.
showAnnotations' :: (HasPart' a, HasText a) => String -> Score a -> Score a
showAnnotations' prefix = withAnnotations (flip $ \s -> foldr (text . (prefix ++ )) s)

-- | Handle the annotations in a score.
withAnnotations :: (HasPart' a, HasText a) => ([String] -> Score a -> Score a) -> Score a -> Score a
withAnnotations f = withMetaNP (f . getAnnotation)
