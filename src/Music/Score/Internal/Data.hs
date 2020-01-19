
module Music.Score.Internal.Data (getData) where

import qualified System.IO.Unsafe
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8

-- | Get data from a fixed data file
getData :: String -> String
getData name = error "use Data.FileEmbed instead of getData"
