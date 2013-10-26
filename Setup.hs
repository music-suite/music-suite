#! /usr/bin/env runhaskell

import Data.Monoid
import System.Process
import Distribution.Simple

main = defaultMainWithHooks simpleUserHooks  { preBuild = preBuild' }

preBuild' _ _ = do
    runCommand "hackette cp Music.Score.Util src/Music/Score/Util.hs"
    return mempty
