
module Main where

import Music.Prelude
import Util
{-
-}


music = stretchTo (70*6) $ stretch 8 $ rcat $ fmap (\n -> stretchTo 1 $ pseq $ times n $ 
  upChromaticF c (fromIntegral n) c) [1,2,3,4,5,6,7,8]




