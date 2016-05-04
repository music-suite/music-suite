
module Lib
    ( someFunc
    ) where

someFunc :: Int
someFunc = fibs !! 10

fibs :: [Int]
fibs = 1:1:zipWith (+) fibs (tail fibs)
