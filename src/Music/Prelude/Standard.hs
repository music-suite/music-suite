{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Standard music representation and export.
module Music.Prelude.Standard
  ( module Music.Pitch,
    module Music.Dynamics,
    module Music.Articulation,
    module Music.Parts,
    module Music.Score,
    Music,
    StandardNote,
    Asp1a,
    defaultMain,

    -- * Lens re-exports
    set,
    over,
    view,
    mapped,
    each,
    filtered,

    -- ** Orchestration
    singleParts,
    doubleParts,
    doublePartsF,
    doublePartsInOctave,
  )
where

import qualified Codec.Midi
import Control.Lens (each, filtered, mapped, over, set, view)
import qualified Data.Music.MusicXml
import Data.Typeable
import Music.Articulation
import Music.Dynamics
import Music.Parts
import Music.Pitch
import Music.Score hiding (Clef (..), Fifths, Interval, Part, Pitch, view)
import Music.Score.Export.StandardNotation (Asp1, Asp1a, LilypondLayout (..), LilypondOptions (..), defaultLilypondOptions, runIOExportM, toLy, toMidi, toStandardNotation, toXml)
import qualified Music.Score.Part
import qualified System.Environment
import qualified Text.Pretty
import qualified Options.Applicative as O

type StandardNote = Asp1

type Music = Score StandardNote

-- TODO set logging level
-- TODO more options?
data ExportBackend = ToXml | ToLy | ToMidi | ToLyAndMidi
data CommandLineOptions
  = CommandLineOptions
      { -- | Backend to use.
        backend :: ExportBackend,
        -- | Lilypond options, ignored when `backend` is not `ToLy`.
        lilypondOptions :: LilypondOptions,
        -- | Path to generated file.
        output :: FilePath
      }

parseOpts :: O.Parser CommandLineOptions
parseOpts =
  CommandLineOptions
    <$> O.option
      (O.eitherReader readBackend)
      (O.long "filetype" <> O.short 'f' <> O.metavar "[xml|ly|mid|ly+mid]")
    <*> parseLyOpts
    <*> (O.strOption (O.long "out" <> O.short 'o' <> O.metavar "PATH"))
  where
    readBackend = \case
      "xml" -> Right ToXml
      "mid" -> Right ToMidi
      "ly" -> Right ToLy
      "ly+mid" -> Right ToLyAndMidi
      "mid+ly" -> Right ToLyAndMidi
      s -> Left ("Backend must be one of [xml|ly|mid|ly+mid], not " <> s)

parseLyOpts :: O.Parser LilypondOptions
parseLyOpts =
  LilypondOptions
    <$> O.option
      (O.eitherReader readLayout)
      (O.long "layout" <> O.metavar "[inline|score|big-score] (default big-score)")
    <|> pure defaultLilypondOptions
  where
    readLayout = \case
      "inline" -> Right LilypondInline
      "score" -> Right LilypondScore
      "big-score" -> Right LilypondBigScore
      _ -> Left "Lilypond layout must be one of [inline|score|big-score]"



defaultMain :: Music -> IO ()
defaultMain music = do
  opts <- O.execParser $ O.info (parseOpts <**> O.helper) O.fullDesc
  handle opts
  where
    handle opts = case backend opts of
      ToLyAndMidi -> do
        -- TODO run in parallel?
        handle $ opts {backend = ToMidi, output = stripSuffix (output opts) ++ ".mid"}
        handle $ opts {backend = ToLy}
      ToMidi -> do
        midi <- runIOExportM $ toMidi music
        Codec.Midi.exportFile (output opts) midi
      ToLy -> do
        work <- runIOExportM $ toStandardNotation music
        (h, ly) <- runIOExportM $ toLy (lilypondOptions opts) work
        let ly' = h ++ show (Text.Pretty.pretty ly)
        -- TODO use ByteString/builders, not String?
        writeFile (output opts) ly'
      ToXml -> do
        work <- runIOExportM $ toStandardNotation music
        work' <- runIOExportM $ toXml work
        -- TODO use ByteString/builders, not String?
        writeFile (output opts) $ Data.Music.MusicXml.showXml work'

stripSuffix :: String -> String
stripSuffix xs
  | '.' `elem` xs = reverse $ drop 1 $ dropWhile (/= '.') $ reverse xs
  | otherwise = xs

-- TODO move

-- | Orchestrate in the given parts.
singleParts :: (Monoid a, Semigroup a, HasPosition a, Transformable a, HasParts' a) => [Music.Score.Part.Part a] -> [a] -> a
singleParts ens = ppar . zipWith (set parts') (reverse $ ens)

-- TODO move

-- | Orchestrate by doubling the given music in all given parts.
--
-- >>> doublePartsInOctave [violins,flutes] $ pseq[c,d,e]
doubleParts :: (Monoid a, HasParts' a) => [Music.Score.Part.Part a] -> a -> a
doubleParts ps x = mconcat $ fmap (\p -> set parts' p x) ps

-- TODO move
doublePartsF :: (Monoid (f a), HasParts' a, Functor f) => [Music.Score.Part.Part a] -> f a -> f a
doublePartsF ps x = mconcat $ fmap (\p -> set (mapped . parts') p x) ps

-- TODO move

-- | Orchestrate by doubling in all given parts.
--
-- >>> doublePartsInOctave [(violins,0),(flutes,1)] $ pseq[c,d,e]
doublePartsInOctave :: (Monoid a, Transposable a, HasParts' a) => [(Music.Score.Part.Part a, Int)] -> a -> a
doublePartsInOctave ps x = mconcat $ fmap (\(p, n) -> set parts' p $ octavesUp (fromIntegral n) x) ps
