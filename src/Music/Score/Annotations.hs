
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
import Data.Semigroup
import Data.Typeable
import Data.String

import Music.Time
import Music.Time.Reactive
import Music.Score.Note
import Music.Score.Score
import Music.Score.Part
import Music.Score.Combinators (withMetaNP)
import Music.Score.Ornaments (HasText, text)
import Music.Score.Meta

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

-- | Annotate a part of the score.
annotateSpan :: Span -> String -> Score a -> Score a
annotateSpan span str x = addMetaNoteNP (sapp span $ return $ Annotation [str]) x

-- | Show all annotations in the score.
showAnnotations :: (HasPart' a, HasText a) => Score a -> Score a
showAnnotations = showAnnotations' ":"

-- | Show all annotations in the score using the given prefix.
showAnnotations' :: (HasPart' a, HasText a) => String -> Score a -> Score a
showAnnotations' prefix = withAnnotations (flip $ \s -> foldr (text . (prefix ++ )) s)

-- | Handle the annotations in a score.
withAnnotations :: (HasPart' a, HasText a) => ([String] -> Score a -> Score a) -> Score a -> Score a
withAnnotations f = withMetaNP (f . getAnnotation)

