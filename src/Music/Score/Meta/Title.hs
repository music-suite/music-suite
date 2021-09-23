{-# OPTIONS_GHC -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

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
-- Provides various forms of title, subtitle etc. and related meta-data.
module Music.Score.Meta.Title
  ( -- * Title type
    Title,

    -- ** Creating and modifying
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
  )
where

import Control.Lens (view)
import Control.Monad.Plus
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Typeable
import Music.Pitch.Literal
import Music.Score.Internal.Util
import Music.Score.Meta
import Music.Score.Part
import Music.Score.Pitch
import Music.Time
import Music.Time.Reactive

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
newtype Title = Title (Int -> Maybe (Last String))
  deriving (Typeable, Monoid, Semigroup)

instance IsString Title where
  fromString x = Title $ \n -> if n == 0 then Just (Last x) else Nothing

instance Show Title where
  show = List.intercalate " " . getTitle

-- | Denote a title to a lower level, i.e title becomes subtitle, subtitle becomes subsubtitle etc.
denoteTitle :: Title -> Title
denoteTitle (Title t) = Title (t . subtract 1)

-- | Extract the title as a descending list of title levels (i.e. title, subtitle, subsubtitle...).
getTitle :: Title -> [String]
getTitle t = untilFail . fmap (getTitleAt t) $ [0 ..]
  where
    untilFail = fmap fromJust . takeWhile isJust

-- | Extract the title of the given level. Semantic function.
getTitleAt :: Title -> Int -> Maybe String
getTitleAt (Title t) = fmap getLast . t 

-- | Set title of the given score.
title :: (HasMeta a, HasPosition a) => Title -> a -> a
title t x = case _era x of
  Nothing -> x
  Just e -> titleDuring e t x

-- | Set title of the given part of a score.
titleDuring :: HasMeta a => Span -> Title -> a -> a
titleDuring s t = addMetaEvent $ view event (s, t)

-- | Set subtitle of the given score.
subtitle :: (HasMeta a, HasPosition a) => Title -> a -> a
subtitle t x = case _era x of
  Nothing -> x
  Just e -> subtitleDuring e t x

-- | Set subtitle of the given part of a score.
subtitleDuring :: HasMeta a => Span -> Title -> a -> a
subtitleDuring s t = addMetaEvent $ view event (s, denoteTitle t)

-- | Set subsubtitle of the given score.
subsubtitle :: (HasMeta a, HasPosition a) => Title -> a -> a
subsubtitle t x = case _era x of
  Nothing -> x
  Just e -> subsubtitleDuring e t x

-- | Set subsubtitle of the given part of a score.
subsubtitleDuring :: HasMeta a => Span -> Title -> a -> a
subsubtitleDuring s t = addMetaEvent $ view event (s, denoteTitle (denoteTitle t))
