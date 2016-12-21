
{-# LANGUAGE CPP #-}

module Music.Score.Internal.Data (getData) where

#ifndef GHCI
#define GET_DATA_FILE Paths_music_score.getDataFileName
import qualified Paths_music_score
#else
#define GET_DATA_FILE (return . ("../music-score/"++))
#endif

import qualified System.IO.Unsafe
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8

-- | Get data from a fixed data file
getData :: String -> String
getData name = System.IO.Unsafe.unsafePerformIO $ do
  fp <- GET_DATA_FILE ("data/" ++ name)
  d <- Data.ByteString.Lazy.readFile fp
  return $ Data.ByteString.Lazy.Char8.unpack d

