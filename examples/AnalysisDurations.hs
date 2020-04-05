{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

import Music.Prelude hiding ((</>))
import qualified Music.Score
import qualified Data.List

main = defaultMain music

music :: Music
music = times 55 (stretch (1/8) c) |> times 28 (stretch (1/16) d)

voiceDurations :: Voice a -> [Duration]
voiceDurations = fmap (view duration) . view notes

countDurationsV :: Voice a -> [(Duration, Int)]
countDurationsV = fmap (\xs -> (head xs, length xs)) . Data.List.group . Data.List.sort . voiceDurations

scoreDurations :: (HasPart' a, Ord (Music.Score.Part a)) => Score a -> [(Duration, Int)]
scoreDurations = countDurationsV . view phrases
