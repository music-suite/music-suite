
module Workspace where


type Hash = Word128
data Blob =
  hash :: Hash
  data :: Text
  parents :: [Hash]
  created :: UTCTIme
data Workspace =
  blobs :: [Blob]
  
