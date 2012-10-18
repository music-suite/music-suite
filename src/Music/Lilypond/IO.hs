
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Music.Lilypond.IO -- (
  -- )
where

import Music.Lilypond

writeMusic :: FilePath -> Music -> IO ()
writeMusic = error "writeMusic: Not implemented"

data Format = PDF | PNG | PS

data EngraveOptions
    = EngraveOptions {
        format   :: Format,    
        include  :: FilePath,
        initFile :: FilePath,
        logFile  :: FilePath,
        logLevel :: Int
    }

writeAndEngraveMusic :: FilePath -> EngraveOptions -> Music -> IO ()
writeAndEngraveMusic = error "writeAndEngraveMusic: Not implemented"
