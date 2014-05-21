
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

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


module Music.Score.Meta.Annotations (
        Annotation,
        annotate,
        annotateSpan,
        showAnnotations,
        showAnnotations',
        withAnnotations,
  ) where

import           Control.Arrow
import           Control.Monad.Plus
import qualified Data.List
import           Data.Semigroup
import           Data.String
import           Data.Typeable
import           Data.Void

import           Music.Score.Meta
import           Music.Score.Part
import           Music.Score.Text    (HasText, text)
import           Music.Time
import           Music.Time.Reactive

-- |
--   An annotation is a unique textual value attached to parts of a score.
--   They are ignored by default, but can be collected with 'withAnnotations'.
--
newtype Annotation = Annotation { getAnnotation_ :: [String] }
    deriving (Semigroup, Monoid, Typeable)
instance IsString Annotation where fromString = Annotation . return

getAnnotation :: Annotation -> [String]
getAnnotation = Data.List.nub . getAnnotation_

-- | Annotate the whole score.
annotate :: String -> Score a -> Score a
annotate str x = annotateSpan (0 >-> _duration x) str x

-- | Annotate a part of the score.
annotateSpan :: Span -> String -> Score a -> Score a
annotateSpan span str x = addGlobalMetaNote (transform span $ return $ Annotation [str]) x

-- | Show all annotations in the score.
showAnnotations :: (HasPart' a, Ord (Part a), HasText a) => Score a -> Score a
showAnnotations = showAnnotations' ":"

-- | Show all annotations in the score using the given prefix.
showAnnotations' :: (HasPart' a, Ord (Part a), HasText a) => String -> Score a -> Score a
showAnnotations' prefix = withAnnotations (flip $ \s -> foldr (text . (prefix ++ )) s)

-- | Handle the annotations in a score.
withAnnotations :: (HasParts' a, HasText a) => ([String] -> Score a -> Score a) -> Score a -> Score a
withAnnotations f = withGlobalMeta (f . getAnnotation)

