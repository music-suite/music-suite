
{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.MemoTrie as MT
import           Data.Char
import           Data.List                             (intercalate, nub, sort,
                                                        sortBy)
import           Data.Ord                              (comparing)
import           Data.Maybe
import           Data.MemoTrie
import           Data.Traversable                      (traverse)
import           System.Environment
import           System.IO
import           System.IO.Unsafe
import           System.Process
import           Text.Regex


import qualified Distribution.ModuleName               as M
import qualified Distribution.Package                  as P
import qualified Distribution.PackageDescription       as PD
import qualified Distribution.PackageDescription.Parse as PDP
import qualified Distribution.Verbosity

import           Language.Haskell.Interpreter          (ModuleName, ModuleElem(..))
import qualified Language.Haskell.Interpreter          as Hint

type Identifier = String

------------------------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= runFilter stdin stdout

runFilter :: Handle -> Handle -> [FilePath] -> IO ()
runFilter inf outf args = hGetContents inf >>= run args >>= hPutStr outf

------------------------------------------------------------------------------------------

-- TODO allow (and don't look up) qualified names

-- |
-- Given a list of cabal files, process input by replacing all text on the form
-- @[foo] with [`foo`][foo], replacing @@@hslinks@@@ with an index on the form:
--
-- [foo]:         prefix/Module-With-Foo.html#v:foo
-- [Foo]:         prefix/Module-With-Foo.html#t:Foo
--
-- etc.
--
run :: [FilePath] -> String -> IO String
run args input = do
    -- !packageNames <- packageNameInCabals args
    -- !modNames     <- modNamesInCabals args
    !allPackagesAndModules <- packageAndModNamesInCabals args

    let allIdentifiersInInput = nub $ fmap getId $ allMatches idExpr input
    let index = intercalate "\n" $ sort $ nub $ map (generateIndex allPackagesAndModules) allIdentifiersInInput

    return $ substituteElements $ substituteIndex index input

    where
        idChars   = "[^]]+"
        idExpr    = mkRegex $ "@\\[(" ++ idChars ++ ")\\]"
        indexExpr = mkRegex $ "@@@hslinks@@@"
        getId     = head . snd
        substituteElements a = subRegex idExpr    a "[`\\1`][\\1]"
        substituteIndex i a  = subRegex indexExpr a i

generateIndex :: [(PackageName, [ModuleName])] -> Identifier -> String
generateIndex allPackagesAndModules = generateIndexLink (concatMap strength $ allPackagesAndModules)

generateIndexLink :: [(PackageName, ModuleName)] -> Identifier -> String
generateIndexLink !sources ident =
    let vOrT = if isUpper (head ident) then "t" else "v" in
    case whichModule (fmap snd sources) (wrapOp ident) of
        Left e -> "[" ++ ident ++ "]: " ++ "\n<!-- Unknown: " ++ ident ++ " " ++ e ++ "-->\n"
        Right modName -> 
            -- TODO
            -- TODO This should be optional
            let package = fromJust $ whichPackage sources modName in
                ""
                ++ "[" ++ ident ++ "]: " ++ kPrefix 
                ++ package 
                ++ "/"
                ++ replace '.' '-' modName ++ ".html" 
                ++ "#" 
                ++ vOrT ++ ":" ++ handleOp ident ++ ""   
    where
        -- FIXME
        kPrefix = "/docs/api/"
        swap (x,y) = (y,x)
        whichPackage sources x = lookup x (fmap swap sources)

        -- If the given identifier is an operator, wrap it in parentheses
        -- Necessary to make the search work
        wrapOp :: Identifier -> Identifier
        wrapOp []     = []
        wrapOp as@(x:_)
            | isAlphaNum x = as
            | otherwise    = "(" ++ as ++ ")"

        -- If the given identifier is an operator, escape it
        handleOp :: Identifier -> Identifier
        handleOp []     = []
        handleOp as@(x:_)
            | isAlphaNum x = as
            | otherwise    = escapeOp as

        -- Escape an operator a la Haddock
        escapeOp = concatMap (\c -> "-" ++ show (ord c) ++ "-")

allMatches :: Regex -> String -> [(String, [String])]
allMatches reg str = case matchRegexAll reg str of
    Nothing                           -> []
    Just (before, match, after, subs) -> (match, subs) : allMatches reg after

type PackageName = String

-----------------------------------------------------------------------------------------

-- |
-- Given a list of module names "in scope", find the first module containing the
-- given identifier.
--
whichModule :: [ModuleName] -> Identifier -> Either String ModuleName
whichModule = whichModule'
  where
    -- Given a set of modules, find the topmost module in which an identifier appears
    -- A module is considered above another if it has fewer dots in its name. If the number of
    -- dots are equal, use lexiographic order.
    whichModule' :: [ModuleName] -> Identifier -> Either String ModuleName
    whichModule' modNames ident = eitherMaybe ("No such identifier: " ++ ident)
        $ fmap (listToMaybe . sortBy bottomMost) modsWithIdent
        where
            mods = modsNamed modNames
            -- modules containing the identifier
            modsWithIdent = fmap (hasIdent ident) mods

            modsNamed :: [ModuleName] -> Either String [(ModuleName, [Identifier])]
            modsNamed = traverse modNamed

            modNamed :: ModuleName -> Either String (ModuleName, [Identifier])
            modNamed = modNamed'
            modNamed' n = case identifiers n of {
                Left e    -> Left e ;
                Right ids -> Right (n, ids) ;
                }

    hasIdent :: Identifier -> [(ModuleName, [Identifier])] -> [ModuleName]
    hasIdent ident = fmap fst . filter (\(n,ids) -> ident `elem` ids)

    -- | Get all the identifiers of a module
    identifiers :: ModuleName -> Either String [Identifier]
    identifiers = memo $ unsafePerformIO . identifiers'

    -- TODO identifiers' is being called repeatedly for the same argument

    identifiers' :: ModuleName -> IO (Either String [Identifier])
    identifiers' modName = do
      hPutStrLn stderr $ "Looking up ids in " ++ modName
      return $ Right []
      fmap getElemNames $ Hint.runInterpreter $ Hint.getModuleExports modName
        where
            getElemNames = either (Left . getError) Right . fmap (concatMap getModuleElem)
            getError = show

    -- | Get all identifiers in a module element (names, class members, data constructors)
    getModuleElem :: ModuleElem -> [Identifier]
    getModuleElem (Fun a)      = [a]
    getModuleElem (Class a as) = a:as
    getModuleElem (Data a as)  = a:as

    {-
    modsInDir :: FilePath -> IO [ModuleName]
    modsInDir dir = do
        dirList <- readProcess "find" [dir, "-type", "f", "-name", "*.hs"] ""
        let dirs = lines dirList
        let mods = fmap (pathToModName . dropBaseDir) dirs
        return mods
        where
            dropBaseDir   = drop (length dir)
            pathToModName = replace '/' '.' . dropWhile (not . isUpper) . dropLast 3

    -}

-- |
-- Given a list of paths to Cabal files, return all packages and the list
-- of modules declared therein.
--
packageAndModNamesInCabals :: [FilePath] -> IO [(PackageName, [ModuleName])]
packageAndModNamesInCabals = packageAndModNamesInCabals'
  where
    packageAndModNamesInCabals' :: [FilePath] -> IO [(PackageName, [ModuleName])]
    packageAndModNamesInCabals' paths = flip mapM paths $ \path -> do
        hPutStrLn stderr "Looking up packages and modules..."
        pn  <- packageNameInCabal path
        mns <- modNamesInCabal path
        return (pn, mns)


    packageNameInCabals :: [FilePath] -> IO [PackageName]
    packageNameInCabals = mapM packageNameInCabal

    packageNameInCabal :: FilePath -> IO PackageName
    packageNameInCabal path = do
        packageDesc <- PDP.readPackageDescription Distribution.Verbosity.normal path
        return $ unPackageName $ P.pkgName $ PD.package $ PD.packageDescription packageDesc
            where
                unPackageName (P.PackageName x) = x

    modNamesInCabals :: [FilePath] -> IO [[ModuleName]]
    modNamesInCabals = mapM modNamesInCabal

    modNamesInCabal :: FilePath -> IO [ModuleName]
    modNamesInCabal path = do
        packageDesc <- PDP.readPackageDescription Distribution.Verbosity.normal path

        -- TODO Why doesn't this work?
        -- case PD.library $ PD.packageDescription packageDesc of
        --     Nothing -> return []
        --     Just library -> return $ map unModName $ PD.exposedModules library
        --     where
        --         unModName = intercalate "." . components
        
        case PD.condLibrary packageDesc of
            Nothing      -> return []
            Just library -> return $ exposedModules library
            where
                exposedModules = fmap unModName . PD.exposedModules . foldCondTree
                unModName = intercalate "." . M.components
                foldCondTree (PD.CondNode x c comp) = x -- Ignore subtrees   

-----------------------------------------------------------------------------------------

eitherMaybe :: e -> Either e (Maybe a) -> Either e a
eitherMaybe e' = go
    where
        go (Left  e)         = Left e
        go (Right Nothing)   = Left e'
        go (Right (Just a))  = Right a

-- | @replace x y xs@ replaces all instances of a @x@ in a list @xs@ with @y@.
replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map $ \z -> if z == x then y else z

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse


strength :: Functor f => (a, f b) -> f (a, b)
strength = fmap (fmap (\(x,y)->(y,x))) $ uncurry (flip strength')

strength' :: Functor f => f a -> b -> f (a,b)
strength' fa b = fmap (\a -> (a,b)) fa

bottomMost :: ModuleName -> ModuleName -> Ordering
bottomMost a b = case comparing level a b of
    LT -> GT
    EQ -> a `compare` b
    GT -> LT
    where
        level = length . filter (== '.')





