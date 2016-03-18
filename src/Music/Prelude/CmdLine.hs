
{-# LANGUAGE CPP, FlexibleContexts #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides framework for building simple command-line converter programs such as
-- @music2midi@, @music2pdf@ etc.
--
-------------------------------------------------------------------------------------

module Music.Prelude.CmdLine (
        converterMain,
        lilypondConverterMain,

        translateFile,
        translateFileAndRunLilypond,

        version,
        versionString
) where

import           Control.Exception
import           Data.Version          (showVersion)
import           Data.Monoid
import           Options.Applicative
#ifndef GHCI
import qualified Paths_music_suite as Paths
#endif
import           Data.Char
import           Data.List          (intercalate, isPrefixOf)
import           Data.List.Split
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe
import           Prelude            hiding (readFile, writeFile)
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Temp
#ifndef mingw32_HOST_OS
import qualified System.Posix.Env   as PE
#endif
import           System.Process

-- | Current Music Suite version.
#ifndef GHCI
version = Paths.version
#else
version = error "Could not get version"
#endif

-- | Current Music Suite version as a string.
versionString :: String
versionString = showVersion version
-- Note that Paths_music_preludes must be in other-modules for executables
-- See haskell/cabal#1759


data ConverterOptions = ConverterOptions {
    prelude :: Maybe String,
    outFile :: Maybe FilePath,
    inFile  :: FilePath
  } deriving (Show)

converterOptions :: Parser ConverterOptions
converterOptions = liftA3 ConverterOptions
  (optional $ strOption $ mconcat [long "prelude", metavar "<name>"])
  (optional $ strOption $ mconcat [short 'o', long "output", metavar "<file>"])
  (argument str $ metavar "<input>")

-- |
-- Generates a basic converter program such as @music2ly@, @music2musicxml@ etc.
--
converterMain
  :: String   -- ^ Name of converter function.
  -> String   -- ^ Extension of generated file.
  -> IO ()
converterMain func ext = do
  pgmName <- getProgName
  options <- execParser (opts pgmName)
  runConverter func ext options
  where
    opts pgmName = info
      (helper <*> converterOptions)
      (fullDesc <> header (pgmName ++ "-" ++ versionString))
    runConverter func ext (ConverterOptions prelude outFile inFile)
      = translateFile func ext prelude (Just inFile) outFile



-- A converter program that invokes Lilypond
-- Used for music2pdf, music2svg et al
data LilypondConverterOptions = LilypondConverterOptions {
    _prelude :: Maybe String,
    _inFile  :: FilePath
  } deriving (Show)

lilypondConverterOptions :: Parser LilypondConverterOptions
lilypondConverterOptions = liftA2 LilypondConverterOptions
  (optional $ strOption $ mconcat [long "prelude", metavar "<name>"])
  (argument str $ metavar "<input>")

-- |
-- Generates a basic converter program that invokes @lilypond@ to generate
-- music notation. Used for @music2pdf@ etc.
--
lilypondConverterMain
  :: String -- ^ A file extension (supported by Lilypond).
  -> IO ()
lilypondConverterMain ext = do
  pgmName <- getProgName
  options <- execParser (opts pgmName)
  runLilypondConverter ext options
  where
    opts pgmName = info
      (helper <*> lilypondConverterOptions)
      (fullDesc <> header (pgmName ++ "-" ++ versionString))
    runLilypondConverter ext (LilypondConverterOptions prelude inFile)
      = translateFileAndRunLilypond ext prelude (Just inFile)


-- |
-- Translate an input file and invoke Lilypond on the resulting output file.
--
translateFileAndRunLilypond
  :: String         -- ^ Output file suffix/format passed to Lilypond.
  -> Maybe String   -- ^ Prelude to use.
  -> Maybe FilePath -- ^ Input file.
  -> IO ()
translateFileAndRunLilypond format preludeName' inFile' = do
  let inFile      = fromMaybe "test.music" inFile'
  let preludeName = fromMaybe "basic" preludeName'
  let lyFile      = takeBaseName inFile ++ ".ly"

  translateFile "writeLilypond" "ly" (Just preludeName) (Just inFile) (Just lyFile)
  rawSystem "lilypond" ["--" ++ format, "-o", takeBaseName inFile, lyFile]
  runCommand $ "rm -f " ++ takeBaseName inFile ++ "-*.tex " ++ takeBaseName inFile ++ "-*.texi " ++ takeBaseName inFile ++ "-*.count " ++ takeBaseName inFile ++ "-*.eps " ++ takeBaseName inFile ++ "-*.pdf " ++ takeBaseName inFile ++ ".eps"
  return ()

-- |
-- Translate an input file using the given paramters.
--
translateFile
  :: String         -- ^ Translate function (of type @'FilePath' -> music -> 'IO' ()@).
  -> String         -- ^ Output file suffix.
  -> Maybe String   -- ^ Prelude to use.
  -> Maybe FilePath -- ^ Input file.
  -> Maybe FilePath -- ^ Output file.
  -> IO ()
translateFile translationFunction outSuffix preludeName' inFile' outFile' = do
  code          <- readFile inFile
  newScore      <- return $ if isNotExpression code
    then expand declTempl (Map.fromList [
      ("prelude"   , prelude),
      ("main"      , main),
      ("scoreType" , scoreType),
      ("code"      , code),
      ("outFile"   , outFile)
      ])
    else expand exprTempl (Map.fromList [
      ("prelude"   , prelude),
      ("main"      , main),
      ("scoreType" , scoreType),
      ("score"     , code),
      ("outFile"   , outFile)
      ])
  -- putStrLn newScore
  withSystemTempDirectory "music-suite." $ \tmpDir -> do
    let tmpFile = tmpDir ++ "/" ++ takeFileName inFile
    let opts = ["-XOverloadedStrings", "-XNoMonomorphismRestriction", "-XTypeFamilies"]
    putStrLn $ "Converting music..."
    writeFile tmpFile newScore
    withMusicSuiteInScope $ do
      putStrLn $ "Writing '" ++ outFile ++ "'..."
      rawSystem "runhaskell" (opts <> [tmpFile])  >>= \e -> if e == ExitSuccess then return () else fail ("Could not convert"++inFile)

  return ()
  where
    inFile      = fromMaybe "test.music" inFile'
    preludeName = fromMaybe "basic" preludeName'
    outFile     = fromMaybe (
                      takeDirectory inFile ++ "/"
                      ++ takeBaseName inFile
                      ++ "." ++ outSuffix)
                      outFile'

    prelude   = "Music.Prelude." ++ toCamel preludeName
    scoreType = "Score " ++ toCamel preludeName ++ "Note"
    main      = translationFunction

    exprTempl = "module Main where { import $(prelude); {-# LINE 1 \"" ++ inFile ++ "\" #-}\nmain = $(main) \"$(outFile)\" ( $(score) :: $(scoreType) ) }"
    declTempl = "module Main where \nimport $(prelude) {-# LINE 1 \"" ++ inFile ++ "\" #-}\n$(code) \nmain = $(main) \"$(outFile)\" ( example  :: $(scoreType) )"

-- TODO hackish, preferably parse using haskell-src-exts or similar
isNotExpression :: String -> Bool
isNotExpression t = anyLineStartsWith "type" t || anyLineStartsWith "data" t || anyLineStartsWith "example =" t














-- |
-- >>> anyLineStartsWith "h" "ahc"
-- False
-- >>> anyLineStartsWith "h" "a\nhc"
-- True
-- >>> anyLineStartsWith "h" "hac"
--
anyLineStartsWith :: String -> String -> Bool
anyLineStartsWith t = any (t `isPrefixOf`) . lines


type Template = String

-- |
-- Simple templating system.
--
-- >>> expand "me : $(name)" (Map.fromList [("name","Hans")])
-- "me : Hans"
--
expand :: Template -> Map String String -> String
expand t vs = (composed $ fmap (expander vs) $ Map.keys $ vs) t
  where
    expander vs k = replace ("$(" ++ k ++ ")") (fromJust $ Map.lookup k vs)

composed :: [a -> a] -> a -> a
composed = foldr (.) id


replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old

toCamel [] = []
toCamel (x:xs) = toUpper x : xs


-- |
-- Wrap IO actions that invoke 'ghc', 'ghci' or 'runhaskell' using this
-- to assure that invocations will work when using a development version of
-- the suite (i.e. the Music packages are in a sandbox).
--
-- Does nothing on Windows, as we can not reliably implement withEnv for that
-- platform current HP release (needs base 4.7).
--
withMusicSuiteInScope :: IO a -> IO a
withMusicSuiteInScope k = do
  r <- try $ readProcess "music-util" ["package-path"] ""
  case r of
    Left x            -> let _ = (x::SomeException) in withEnv "GHC_PACKAGE_PATH" (const "") k
    Right packagePath -> withEnv "GHC_PACKAGE_PATH" (const packagePath) k

-- | Temporarily modfiy an environment variable (POSIX only).
--
-- @
-- withEnv varName (\oldValueIfPresent -> newValue) $ do
--    ...
-- @
--
withEnv :: String -> (Maybe String -> String) -> IO a -> IO a
#ifdef mingw32_HOST_OS
withEnv _ _ = id
#else
withEnv n f k = do
  x <- PE.getEnv n
  PE.setEnv n (f x) True
  res <- k
  case x of
    Nothing -> PE.unsetEnv n >> return res
    Just x2 -> PE.setEnv n x2 True >> return res
#endif
