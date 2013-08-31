#!/usr/bin/env runhaskell

module Main where

import Data.Char
import Data.Maybe
import Text.Regex
import System.Process
import System.IO
import System.IO.Unsafe
import System.Environment
import Language.Haskell.Interpreter
import Data.List (sort, sortBy, intersperse)
import Distribution.PackageDescription.Parse
import Distribution.Verbosity (normal)
import Distribution.PackageDescription
import Distribution.ModuleName (components)

type Identifier = String

-- All the identifiers of a module
identifiers' :: ModuleName -> IO (Either String [Identifier])
identifiers' modName = fmap (either (Left . show) Right) $ (fmap $ fmap $ concat . fmap unElem) $ (runInterpreter $ getModuleExports modName)

unElem :: ModuleElem -> [String]
unElem (Fun a)      = [a]
unElem (Class a as) = a:as
unElem (Data a as)  = a:as


modsInDirs :: [FilePath] -> IO [ModuleName]
modsInDirs = fmap concat . mapM (modsInDir)

modsInDir :: FilePath -> IO [ModuleName]
modsInDir dir = do
    dirList <- readProcess "find" [dir, "-type", "f", "-name", "*.hs"] "" 
    let dirs = lines dirList                           
    -- return dirs
    return $ fmap (pathToModName . drop (length dir)) dirs
    where
        pathToModName = replace1 '/' '.' . dropWhile (not . isUpper) . dropLast 3

visibleModsInCabals :: [FilePath] -> IO [ModuleName]
visibleModsInCabals = fmap concat . mapM (visibleModsInCabal)

visibleModsInCabal :: FilePath -> IO [ModuleName]
visibleModsInCabal path = do
    packageDesc <- readPackageDescription normal path 
    case condLibrary packageDesc of
        Nothing -> return []
        Just libTree -> return (fmap unModName $ exposedModules $ foldCondTree libTree)
        where                                 
            unModName = concatSep "." . components
            foldCondTree (CondNode x c comp) = x -- TODO subtrees

identifiers :: ModuleName -> Either String [Identifier]
identifiers = unsafePerformIO . identifiers'

-- Given a set of modules, find the topmost module in which an identifier appears
-- A module is considered above another if it has fewer dots in its name. If the number of
-- dots are equal, use lexiographic order.
whichModule :: [ModuleName] -> Identifier -> Either String ModuleName
whichModule modNames ident = eitherMaybe ("No such identifier: " ++ ident) 
    $ fmap (listToMaybe . sortBy bottomMost) modsWithIdent
    where
        mods :: Either String [(ModuleName, [Identifier])]
        mods = sequence $ fmap (\n -> case identifiers n of {
            Left e    -> Left e ;
            Right ids -> Right (n, ids) ;
            }) modNames

        -- modules containing the identifier
        modsWithIdent :: Either String [ModuleName]
        modsWithIdent = fmap (fmap fst . filter (\(n,ids) -> ident `elem` ids)) $ mods

-- [Either e a] -> Either e [a]
-- [m a] -> m [a]

eitherMaybe :: e -> Either e (Maybe a) -> Either e a
eitherMaybe msg = go
    where       
        go (Left  e)         = Left e
        go (Right (Nothing)) = Left msg
        go (Right (Just a))  = Right a

bottomMost :: ModuleName -> ModuleName -> Ordering
bottomMost a b = case level a `compare` level b of
    LT -> GT
    EQ -> a `compare` b
    GT -> LT
    where
        level :: ModuleName -> Int
        level = length . filter (== '.')


-- FIXME
fromRight (Right a) = a

-- | Replaces all instances of a value in a list by another value.
replace1 :: Eq a =>
           a   -- ^ Value to look for
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace1 x y = map (\z -> if z == x then y else z)


takeLast n = reverse . take n . reverse
dropLast n = reverse . drop n . reverse

main = do
    cabals <- getArgs
    runStd cabals

runStd :: [FilePath] -> IO ()
runStd cabals = runFilter cabals stdin stdout

runFilter :: [FilePath] -> Handle -> Handle -> IO ()
runFilter cabals i o = hGetContents i >>= run cabals >>= hPutStr o

{--
Given a list of cabal files, process input by replacing all text on the form @@foo@@ with
[`foo`][foo], replacing @@@hslinks@@@ with an index on the form:

[foo]:         prefix/Module-With-Foo.html#v:foo
[Foo]:         prefix/Module-With-Foo.html#t:Foo

etc.

--}
run :: [FilePath] -> String -> IO String
run cabals input = do
    modNames <- visibleModsInCabals cabals

    -- TODO generate the index
    let ids = filter (/= "@hslinks@") $ (fmap head $ fmap snd $ allMatches idExpr input)
    links <- mapM (idToLink modNames) ids
    let index = concatSep "\n" $ links
    
    return $ subElems $ subIndex index $ input
    where                                
        idExpr    = mkRegex "@@(.*)@@"
        indexExpr = mkRegex "@@@hslinks@@@"

        subElems a   = subRegex idExpr    a "[`\\1`][\\1]"
        subIndex i a = subRegex indexExpr a i

-- FIXME
kPrefix = "/docs/api/"

idToLink :: [ModuleName] -> Identifier -> IO String
idToLink sources ident = do
    let vOrT = if (isUpper $ head ident) then "t" else "v"
    case whichModule sources ident of
        Left e -> return $ "\n<!-- Unknown: " ++ ident ++ " " ++ e ++ "-->\n"
        Right modName -> return $ "["++ident++"]: "++kPrefix++(replace1 '.' '-' modName)++".html#"++vOrT++":"++ident++""
    


allMatches :: Regex -> String -> [(String, [String])]
allMatches reg str = case matchRegexAll reg str of
    Nothing                           -> []
    Just (before, match, after, subs) -> (match, subs) : allMatches reg after


concatSep q = concat . intersperse q

