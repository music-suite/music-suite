
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
-------------------------------------------------------------------------------------

module Music.Score.Meta.Title (

        -- * Title type
        Title,

        -- ** Creating and modifying
        -- titleFromString,
        denoteTitle,
        getTitle,
        getTitleAt,

        -- * Adding titles to scores
        title,
        titleDuring,
        subtitle,
        subtitleDuring,
        subsubtitle,
        subsubtitleDuring,

        -- * Extracting titles
        withTitle,
  ) where

import           Control.Arrow
import           Control.Lens              (view)
import           Control.Monad.Plus
import           Data.Foldable             (Foldable)
import qualified Data.Foldable             as F
import qualified Data.List                 as List
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Monoid.WithSemigroup
import           Data.Semigroup
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.String
import           Data.Traversable          (Traversable)
import qualified Data.Traversable          as T
import           Data.Typeable
import           Data.Void

import           Music.Pitch.Literal
import           Music.Score.Meta
import           Music.Score.Part
import           Music.Score.Pitch
import           Music.Score.Util
import           Music.Time
import           Music.Time.Reactive

-- |
-- A title is a sequence of 'String' values, representing the name of a work or part of a work.
-- An arbitrary depth of title sections can be used.
--
-- Title is an instance of 'IsString' and can be used with the 'OverloadedStrings' extension as
-- follows:
--
-- > title  "Le Nozze di Figaro"
-- >
-- > subtitle "Atto primo"
-- > subsubtitle "Scena I"
-- > subsubtitle "Scena II"
-- > ...
-- >
-- > subtitle "Atto secundo"
-- > ...
--
newtype Title = Title (Int -> Option (Last String))
    deriving (Typeable, Monoid, Semigroup)

instance IsString Title where
    fromString x = Title $ \n -> if n == 0 then Option (Just (Last x)) else Option Nothing

instance Show Title where
    show = List.intercalate " " . getTitle

-- | Create a title from a string. See also 'fromString'.
titleFromString :: String -> Title
titleFromString = fromString

-- | Denote a title to a lower level, i.e title becomes subtitle, subtitle becomes subsubtitle etc.
denoteTitle :: Title -> Title
denoteTitle (Title t) = Title (t . subtract 1)

-- | Extract the title as a descending list of title levels (i.e. title, subtitle, subsubtitle...).
getTitle :: Title -> [String]
getTitle t = untilFail . fmap (getTitleAt t) $ [0..]
    where
        untilFail = fmap fromJust . takeWhile isJust

-- | Extract the title of the given level. Semantic function.
getTitleAt :: Title -> Int -> Maybe String
getTitleAt (Title t) n = fmap getLast . getOption . t $ n

-- | Set title of the given score.
title :: (HasMeta a, {-HasPart' a, -}HasPosition a) => Title -> a -> a
title t x = titleDuring (_era x) t x

-- | Set title of the given part of a score.
titleDuring :: (HasMeta a{-, HasPart' a-}) => Span -> Title -> a -> a
titleDuring s t = addGlobalMetaNote $ view note (s, t)

-- | Set subtitle of the given score.
subtitle :: (HasMeta a, {-HasPart' a, -}HasPosition a) => Title -> a -> a
subtitle t x = subtitleDuring (_era x) t x

-- | Set subtitle of the given part of a score.
subtitleDuring :: (HasMeta a{-, HasPart' a-}) => Span -> Title -> a -> a
subtitleDuring s t = addGlobalMetaNote $ view note (s, denoteTitle t)

-- | Set subsubtitle of the given score.
subsubtitle :: (HasMeta a, {-HasPart' a, -}HasPosition a) => Title -> a -> a
subsubtitle t x = subsubtitleDuring (_era x) t x

-- | Set subsubtitle of the given part of a score.
subsubtitleDuring :: (HasMeta a{-, HasPart' a-}) => Span -> Title -> a -> a
subsubtitleDuring s t = addGlobalMetaNote $ view note (s, denoteTitle (denoteTitle t))

-- | Extract the title in from the given score.
withTitle :: (Title -> Score a -> Score a) -> Score a -> Score a
withTitle = withGlobalMetaAtStart

