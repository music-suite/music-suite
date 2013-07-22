
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Music.Prelude.Basic

import Data.ByteString(ByteString)
import qualified Data.ByteString as B

fromJson :: ByteString -> Score Note
fromJson = const c

main = do
    json <- B.readFile "test.json"


    let score = fromJson json
    openLy score
    