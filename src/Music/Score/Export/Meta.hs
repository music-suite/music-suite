{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Utility to extract meta-data at a specific point in time.
module Music.Score.Export.Meta where

import Control.Lens
import Control.Monad
import Data.Maybe (listToMaybe)
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

attributionsAt :: HasMeta a => Time -> a -> [(String, Maybe String)]
attributionsAt t x =
  [ ("composer", attributionAt t "composer" x),
    ("lyricist", attributionAt t "lyricist" x)
  ]

partNames :: (HasParts' a, Show (GetPart a)) => a -> [String]
partNames = fmap show . toListOf parts'

