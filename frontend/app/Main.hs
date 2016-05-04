
module Main where

import Lib
import Data.String
import Lubeck.App(runAppReactive)
import Web.VirtualDom.Html(text)

main :: IO ()
main = do
  let n = someFunc
  runAppReactive $ pure $ text $ fromString $ "The number is " ++ show n
