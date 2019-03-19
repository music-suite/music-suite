
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveFunctor #-}

module Text.Transf.Process (
        defaultMain,
        defaultMain',
  ) where

import Control.Exception
import Control.Applicative
import Control.Monad (when)
import Control.Monad.Error hiding (mapM)
import Control.Monad.Plus hiding (mapM)
import Data.Semigroup hiding (Option)
import Data.List (find)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Traversable (mapM)
import Data.Typeable
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt
import Text.Transf

import Prelude hiding (readFile, writeFile)

-- |
-- Creates a Unix style text processor from a 'Transform'.
--
-- The resulting action should be used as the main of an application
-- and will render a program of the given name that responds to @-v@
-- and @-h@ flags. If given no flags it runs the text transformer over
-- the standard input and output streams. If an error occurs the program
-- halts and prints an error message to the standard error stream.
--
-- > defaultMain name transf
-- 
defaultMain :: String -> Transform -> IO ()
defaultMain name transf = defaultMain' name [] (const transf)

-- |
-- Like 'defaultMain', but customizes the transform based on the
-- given options.
--
-- The @help@ and @version@ flags are added automatically.
--
-- > defaultMain' name opts transf
-- 
defaultMain' :: String -> [OptDescr a] -> ([a] -> Transform) -> IO ()
defaultMain' name optDesc transf = do               
    let optDesc' = stdOptDesc ++ (fmap . mapOptDescr) User optDesc
    (opts, args, optErrs) <- getOpt Permute optDesc' <$> getArgs
    return ()

    let usage = usageInfo (header name) optDesc'
    let printUsage   = putStr (usage ++ "\n")        >> exitSuccess
    let printVersion = putStr (version name ++ "\n") >> exitSuccess
    -- 
    when (Help    `elem` opts) printUsage
    when (Version `elem` opts) printVersion
    let opts' = fmap (\(User a) -> a) opts

    runFilter (transf opts')
    return ()

    where
        version name = name ++ "-0.12"
        header  name = "Usage: "++name++" [options]\n" ++
                       "Usage: "++name++" [options] files...\n" ++
                       "\n" ++
                       "Options:"
        
        runFilter :: Transform -> IO ()
        runFilter transf = run transf stdin stdout
        
        -- stdOptDesc :: [UserOpt a]
        stdOptDesc = [
                Option ['h'] ["help"]    (NoArg Help)       "Print help and exit",
                Option ['v'] ["version"] (NoArg Version)    "Print version and exit"
            ]

        run :: Transform -> Handle -> Handle -> IO ()
        run transf fin fout = do
            res <- runContext $ do
                input  <- liftIO $ hGetContents fin
                output <- runTransform transf input
                liftIO $ hPutStr fout output
            case res of
                Left e  -> hPutStrLn stderr ("Error: " ++ e) >> exitFailure
                Right _ -> exitSuccess


mapOptDescr :: (a -> b) -> OptDescr a -> OptDescr b
mapOptDescr = fmap

data UserOpt a
    = Help
    | Version
    | User a

instance Eq (UserOpt a) where
    Help    == Help     = True
    Version == Version  = True
    _       == _        = False

