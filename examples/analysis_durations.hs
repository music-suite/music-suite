-- -fno-warn-typed-holes
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

import Music.Prelude hiding ((</>))
import qualified Music.Score
import qualified System.Process
import qualified Data.List

music = asScore $ times 55 (stretch (1/8) c) |> times 28 (stretch (1/16) d)

main = do
  -- open music
  putStrLn $ "Durations: "
  mapM_ (\(x,y) -> putStrLn ("  " ++ show x ++ " \t" ++ show y)) $ (scoreDurations music)


voiceDurations :: Voice a -> [Duration]
voiceDurations = view durationsV

countDurationsV :: Voice a -> [(Duration, Int)]
countDurationsV = fmap (\xs -> (head xs, length xs)) . Data.List.group . Data.List.sort . voiceDurations

scoreDurations :: (HasPart' a, Ord (Music.Score.Part a)) => Score a -> [(Duration, Int)]
scoreDurations = countDurationsV . view phrases
