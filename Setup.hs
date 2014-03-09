#! /usr/bin/env runhaskell

import Distribution.Simple
main = defaultMain

-- import Data.Monoid
-- import System.Process
-- import Distribution.Simple
-- 
-- main = defaultMainWithHooks simpleUserHooks  { preBuild = preBuild' }
-- 
-- preBuild' _ _ = do
--     runCommand "hackette cp Music.Score.Util src/Music/Score/Util.hs"
--     return mempty
