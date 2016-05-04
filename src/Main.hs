
{-# LANGUAGE NoImplicitPrelude #-}
import BasePrelude
import Language.Haskell.Interpreter
import qualified Data.List
import Music.Prelude


{-
Forget Alan. This code assumes:
  - It is a program compiled with the suite (same stack file)
  - It uses hint/GHC API to interpret code from user and handles musical values etc internally
  - Applications built on top of this will use `stack exec int` (or whatever name we choose)

Maintain a workspace
  - Set of (JSON,[ParentMD5Hash],StableType) pairs, stored as JSON with SHA256 name
      data StableType = [StableTypeCon]
      data StableTypeCon =
        -- For base and other packages built into compiler
        BuiltInTypeCon { packageQualTypeName :: String }
        LibraryTypeCon { gitRepo :: URI, gitRev :: String, packageQualTypeName :: String }
        -- i.e.
          [ git@github.com:music-suite/music-suite.git, 6b1bef7e157d864ec08e39702147990d2977bbfc, music-score.Music.Prelude.Voice
          , base.Prelude.Int
          ]
  - Single top level file with [Name] where
      data Name = StableName String Hash | DraftName String Hash

  - Interpret code with music suite in scope, inside some monad that gives access to the workspace,
    possibly other FS interaction, sending values back to the shell ("printing"), loading HS files (a la GHCI).

      module Storage.Workspace
      type Id   = Word256
      type Name = Text

      getWS :: Id -> WS (Maybe a)
      putWS :: {parents :: [Id]} -> a -> WS Id -- no overwrite!
      getParents :: Id -> WS (Maybe [Id])

      getWSNames :: WS (Map Name (Bool, Id))
      putWSNames :: Map Name (Bool, Id) -> WS ()
      getWSNamed :: Name -> WS (Maybe a)
      putWSNamed :: Name -> Bool -> a -> WS Bool -- potential update!
      modifyWSNamed :: Name -> (a -> a) -> WS ()

      runWS    :: {wsDir :: String} -> WS a -> IO a
      mergeWSs :: {wsDir :: String} -> {wsDir :: String} -> WS Bool

-}

main = do
  let tmpFile = "/tmp/int12938746892618734.hs"
  writeFile tmpFile $ Data.List.intercalate "\n"
    [ "module P where"
    , "import Prelude"
    , "import Music.Prelude"
    , "quux = 123456"
    ]
  r <- runInterpreter $ do
    -- setImports $ ["BasePrelude", "Music.Prelude"]
    loadModules [tmpFile]
    setTopLevelModules ["P"]


    -- eval "let xyz = c :: Voice Pitch"
    forever $ do
      line <- liftIO getLine
      -- TODO Unsafe: interpreting code in IO!
      -- This should allow:
        -- Interpret code in a "safer" monad (i.e. WS)
        -- Allow type-of/kind-of
        -- Serialize and send errors such as type errors
      -- TODO Expose a web socket API
      join $ fmap liftIO $ interpret line (undefined :: IO ())
    return ()
  print $ r
