
module Main where

import Music.Prelude
import Util
{-
-}


music = stretchTo (20*6) $ stretch 8 $ rcat $ fmap (\n -> stretchTo 1 $ pseq $ times n $
  upChromatic c (fromIntegral n) c) [2,3,4,5,6,7,8]



main = defaultMain music
