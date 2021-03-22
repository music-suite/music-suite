{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
import Data.Text
import Numeric.Natural
import qualified Data.Map
import Data.Map (Map)
import qualified Data.Hashable
import Data.Traversable (for)
import System.Directory (doesFileExist)


-- TODO use proper collission-resistant hashing function
hashVal :: Val -> ValRef
hashVal = Data.Text.pack . show . Data.Hashable.hash . show

-- TODO use proper collission-resistant hashing function
hashGraph :: Graph -> GraphRef
hashGraph = Data.Text.pack . show . Data.Hashable.hash . show

hashOp :: (Text, [ValRef]) -> GraphRef
hashOp = Data.Text.pack . show . Data.Hashable.hash . show

-- Runs creation for a set of examples. An example is either
--
--    *  Each "hs" file in the examples/ directory
--    *  Each "music+haskell" snippet in docs/src/User-Guide.md
--
-- defaultMain is added if it doesn't exist and the whole thing is run as per "Run example"
-- in the README. This uses the CLI exposed by defaultMain


-- Each example should be rerendered iff either ((the library/deps have
-- changed) or (the example itself has changed)). Ideally the build system
-- tracks this for us.
--
-- Note: Cabal is now smart enough to track the above dependency, however
-- the executables are the final nodes in the dependency graph, and we can't
-- insert new nodes for, e.g., generating example/A.ly upon example/A.hs being
-- rebuilt.
--    * Track changes to generated code and compare with previous version
--      Brittle.
--    * Use Template Haskell snippet, firing upon recompilation.
--    * Use Cabal hooks.
--    * Track changes outside of Cabal (e.g. change to this file, or to any other file outside /examples and /docs, count as a change)



-- In principle:
--    Generate .ly files for all examples
--    Compare against putd expected results
--    If different, generate visual regression test
--
-- Issue: Running Lilypond for all these examples is slow. Solution:
--  * Parallelize
--  * Cache

-- TODO build graph with early cutoff
-- Use (hash of dist-newstyle/cache/plan.json + hash of sources obtained through `cabal sdist --list-only`, src dir only) to identify library

type GraphRef = Text -- sha256
type ValRef = Text -- sha256a

data Graph = Input Val | Op Text [GraphRef]
  deriving (Read, Show)
data Val = Nat Natural | Text Text | Record (Map Text ValRef)
  deriving (Read, Show)

-- Laws:
--    putVal, getVal returns same value
--    putGraph, getGraph returns same value
--    putEvalCache, getEvalCache: state laws, fail on conflicting write
--    putResultCache, getResultCache: state law, fail on conflicting writes
--    parallelFor: like standard Traversable laws. Do we need this for a parallel implementation?
class Monad m => MonadBuild m where
  putVal :: Val -> m ValRef

  getVal :: ValRef -> m Val

  putGraph :: Graph -> m GraphRef

  getGraph :: GraphRef -> m Graph

  getEvalCache :: GraphRef -> m (Maybe ValRef)

  putEvalCache :: GraphRef -> ValRef -> m ()

  getResultCache :: (Text, [ValRef]) -> m (Maybe ValRef)

  putResultCache :: (Text, [ValRef]) -> ValRef -> m ()

  parallelFor :: [a] -> (a -> m b) -> m [b]


prefix = ".graph/"
storePrefix = prefix ++ "store/"
evalCachePrefix = prefix ++ "eval/"
resultCachePrefix = prefix ++ "results/"

-- | For testing purposes only
instance MonadBuild IO where
  putVal v = do
    let k = hashVal v
    writeFile (storePrefix ++ Data.Text.unpack k) (show v)
    pure k
  getVal r =
    read <$> readFile (storePrefix ++ Data.Text.unpack r)
  putGraph v = do
    let k = hashGraph v
    writeFile (storePrefix ++ Data.Text.unpack k) (show v)
    pure k
  getGraph r =
    read <$> readFile (storePrefix ++ Data.Text.unpack r)
  putEvalCache k v = do
    writeFile (evalCachePrefix ++ Data.Text.unpack k) (show v)
  getEvalCache r = do
    let path = evalCachePrefix ++ Data.Text.unpack r
    there <- doesFileExist path
    if there then
        Just . read <$> readFile path
      else
        pure Nothing
  putResultCache k v = do
    writeFile (resultCachePrefix ++ Data.Text.unpack (hashOp k)) (show v)
  getResultCache r = do
    let path = resultCachePrefix ++ Data.Text.unpack (hashOp r)
    there <- doesFileExist path
    if there then
        Just . read <$> readFile (resultCachePrefix ++ Data.Text.unpack (hashOp r))
      else
        pure Nothing
  parallelFor = flip traverse -- TODO use parallel version?

-- TODO show this is what gives "early cutoff"
withResultCache :: MonadBuild m => (Text, [ValRef]) -> m ValRef -> m ValRef
withResultCache opValRefs k = do
  res <- getResultCache opValRefs
  case res of
    Just x -> pure x
    Nothing -> do
      res <- k
      putResultCache opValRefs res
      pure res

-- TODO show this is what gives "minimality"
withEvalCache :: MonadBuild m => GraphRef -> m ValRef -> m ValRef
withEvalCache graphRef k = do
  res <- getEvalCache graphRef
  case res of
    Just x -> pure x
    Nothing -> do
      res <- k
      putEvalCache graphRef res
      pure res

-- TODO step function should *only* have access to getVal, ensure in types
eval :: MonadBuild m => (Text -> [ValRef] -> m ValRef) -> GraphRef -> m ValRef
eval step graphRef = withEvalCache graphRef $ do
  graph <- getGraph graphRef
  case graph of
    Input v -> putVal v
    Op opName inputGraphs -> do
      -- TODO limit parallelism, e.g. because operation requires
      -- some resource such as a CPU
      inputValRefs <- parallelFor inputGraphs (eval step)
      withResultCache (opName, inputValRefs) $ step opName inputValRefs

arith :: (MonadBuild m, m ~ IO) => Text -> [ValRef] -> m ValRef
arith "+" xs = do
  print ("+", xs)
  ys <- for xs $ \x -> do
    v <- getVal x
    case v of
      Nat x -> pure x
      _ -> error "TODO handle non-nat"
  putVal $ Nat $ sum ys

musicExamples :: (MonadBuild m, m ~ IO) -> Text -> [ValRef] -> m ValRef
musicExamples op = case op of
  "compile" -> undefined
--    * `cabal exec` - creating LY file, taking and verifying music-suite version (plan.json+cabal sdist as per aboce), and taking source file example/Foo.hs
  "lilypond" -> undefined
--    * `lilypond` - taking .ly file, taking and verifying Ly source, producing SVG
  "verified" -> undefined
--    * Verify - taking source file example/Foo.hs and SVG, producing proof that user
--      OK'd it.
  "gallery" -> undefined
--    * Gallery - taking SVGs and producing gallery (SVGs + HTML index)

test :: IO ()
test = do
  g1 <- putGraph $ Input $ Nat 1
  g2 <- putGraph $ Input $ Nat 23
  g3 <- putGraph $ Op "+" [g1, g2]
  g  <- putGraph $ Op "+" [g3, g3]
  print g
  print =<< getVal =<< eval arith g

-- TODO make use of the above for basic "cached testing" infra
-- Add ops for
-- External process invocation:
--    * COMPLETELY sandboxed, files are copied in/out of put as given
--    * CHECKS for any input that might matter but can't be sandboxed (e.g. version)

