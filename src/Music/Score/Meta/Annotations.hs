{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

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
--
-- Provides a basic way annotating scores.
module Music.Score.Meta.Annotations
  ( Annotation,
    getAnnotation,
    annotate,
    annotateSpan,
    showAnnotations,
    showAnnotations',
    withAnnotations,
  )
where

import Control.Monad.Plus
import qualified Data.List
import Data.Semigroup
import Data.String
import Data.Typeable
import Music.Score.Meta
import Music.Score.Part
import Music.Score.Text (HasText, text)
import Music.Time
import Music.Time.Reactive

-- |
--   An annotation is a unique textual value attached to parts of a score.
--   They are ignored by default, but can be collected with 'withAnnotations'.
newtype Annotation = Annotation {getAnnotation_ :: [String]}
  deriving (Semigroup, Monoid, Typeable)

instance IsString Annotation where fromString = Annotation . return

getAnnotation :: Annotation -> [String]
getAnnotation = Data.List.nub . getAnnotation_

-- | Annotate the whole score.
annotate :: String -> Score a -> Score a
annotate str x = case _era x of
  Nothing -> x
  Just e -> annotateSpan e str x

-- | Annotate a part of the score.
annotateSpan :: Span -> String -> Score a -> Score a
annotateSpan span str x = addMetaNote (transform span $ return $ Annotation [str]) x

-- | Show all annotations in the score.
showAnnotations :: (HasParts' a, Ord (Part a), HasText a) => Score a -> Score a
showAnnotations = showAnnotations' ":"

-- | Show all annotations in the score using the given prefix.
showAnnotations' :: (HasParts' a, Ord (Part a), HasText a) => String -> Score a -> Score a
showAnnotations' prefix = withAnnotations (flip $ \s -> foldr (text . (prefix ++)) s)

-- | Handle the annotations in a score.
withAnnotations :: ([String] -> Score a -> Score a) -> Score a -> Score a
withAnnotations f = withMeta (f . getAnnotation)
