#! /usr/bin/env runhaskell


-- hackette cp Music.Score.Util src/Music/Score/Util.hs


import Data.Monoid
import System.Process
import Distribution.Simple

main = defaultMainWithHooks simpleUserHooks  { preBuild = preBuild' }

-- preBuild' ::  Args -> BuildFlags -> IO HookedBuildInfo
preBuild' _ _ = do
    runCommand "hackette cp Music.Score.Util src/Music/Score/Util.hs"
    return mempty
