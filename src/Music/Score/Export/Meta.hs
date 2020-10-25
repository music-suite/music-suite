{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

-- Utility to extract meta-data at a specific point in time.
module Music.Score.Export.Meta where

import Control.Lens
import Control.Monad
import Music.Score.Meta.Attribution
import Music.Score.Meta.Key
import Music.Score.Meta.Tempo
import Music.Score.Meta.Time
import Music.Score.Meta.Title
import Music.Score.Part
import Music.Time
import Music.Time.Meta

-- TODO merge this module with Score.Meta or similar

attributionAt :: HasMeta a => Time -> String -> a -> Maybe String
attributionAt t n x =
  join
    $ fmap (`getAttribution` n)
    $ fmap (`atTime` t)
    $ unwrapMeta
    $ x ^. meta

-- TODO get all *set* attributions
attributionsAt :: HasMeta a => Time -> a -> [(String, Maybe String)]
attributionsAt t x =
  [ ("composer", attributionAt t "composer" x),
    ("lyricist", attributionAt t "lyricist" x)
    -- TODO etc
  ]

-- TODO rename getTitleAt etc in Meta.Title to prevent confunsion
titleAt :: HasMeta a => Time -> a -> Maybe Title
titleAt _ _ = Nothing

partNames :: (HasParts' a, Show (Part a)) => a -> [String]
partNames = fmap show . toListOf parts'

commonMetaAt :: HasMeta a => Time -> a -> [(String, Maybe String)]
commonMetaAt t x =
  ("title", join $ fmap getTitle1 $ titleAt t x)
    : ("subtitle", join $ fmap getTitle2 $ titleAt t x)
    : ("subsubtitle", join $ fmap getTitle3 $ titleAt t x)
    : attributionsAt t x
  where
    getTitle1 = (`getTitleAt` 0)
    getTitle2 = (`getTitleAt` 1)
    getTitle3 = (`getTitleAt` 2)
