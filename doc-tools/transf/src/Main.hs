module Main where

import Data.Default
import Data.Maybe
import Data.Semigroup hiding (Option)
import System.Console.GetOpt
import Text.Transf
  ( MusicOpts (..),
    haskellT,
    musicHaskellT,
    musicT,
  )
import Text.Transf.Process

main = defaultMain' "transf" optDesc transf

transf opts =
  haskellT
    <> musicT (getMusicOpts opts)
    <> musicHaskellT (getMusicOpts opts)

data Opt
  = Format String
  | Resolution Int
  | Resize Int
  | Prelude String

getFormat :: Opt -> Maybe String
getFormat (Format a) = Just a
getFormat _ = Nothing

getResolution (Resolution a) = Just a
getResolution _ = Nothing

getResize (Resize a) = Just a
getResize _ = Nothing

getPrelude (Prelude a) = Just a
getPrelude _ = Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

getMusicOpts :: [Opt] -> MusicOpts
getMusicOpts xs = MusicOpts
  { format = fromMaybe (format def) $ firstJust $ fmap getFormat xs,
    resolution = fromMaybe (resolution def) $ firstJust $ fmap getResolution xs,
    resize = fromMaybe (resize def) $ firstJust $ fmap getResize xs,
    prelude = fromMaybe (prelude def) $ firstJust $ fmap getPrelude xs
  }

optDesc :: [OptDescr Opt]
optDesc =
  [ Option [] ["format"] (ReqArg Format "pdf|png|ps") "Music output format",
    Option [] ["resolution"] (ReqArg (Resolution . read) "N") "Music resolution",
    Option [] ["resize"] (ReqArg (Resize . read) "N") "Music resize factor",
    Option [] ["prelude"] (ReqArg Prelude "<name>") "Music prelude name"
  ]
