
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
--    Compare against stored expected results
--    If different, generate visual regression test
--
-- Issue: Running Lilypond for all these examples is slow. Solution:
--  * Parallelize
--  * Cache

-- TODO build graph with early cutoff
-- Use (hash of dist-newstyle/cache/plan.json + hash of sources obtained through `cabal sdist --list-only`, src dir only) to identify library

type GraphRef = Text -- sha256
type ValRef = Text -- sha256a

data Graph = Lit Text | Op Text [GraphRef]
data Val = Nat Natural | Text Text -- TODO etc, JSON deduplicated using ValRef

type MonadX = Monad -- TODO determine primitive effects, some subset of the below:

getGraph :: MonadX m => GraphRef -> m Graph
getGraph = undefined

getEvalCache :: MonadX m => GraphRef -> m (Maybe ValRef)
getEvalCache = undefined

storeEvalCache :: MonadX m => GraphRef -> Val -> m ()
storeInEvalCache = undefined

getResultCache :: MonadX m => (Text, [ValRef]) -> m (Maybe ValRef)
getResultCache = undefined

storeResultCache :: MonadX m => (Text, [ValRef]) -> Val -> m ()
storeInResultCache = undefined

parallelFor :: MonadX m => [a] -> (a -> m b) -> m [b]
parallelFor = undefined

withResultCache :: MonadX m => (Text, [ValRef]) -> m Val -> m Val
withResultCache opValRefs k =
  res <- getResultCache opValRefs
  case res of
    Just x -> pure x
    Nothing -> do
      res <- k
      storeInResultCache graphRef res

withEvalCache :: MonadX m => GraphRef -> m Val -> m Val
withEvalCache graphRef k = do
  res <- getEvalCache graphRef
  case res of
    Just x -> pure x
    Nothing -> do
      res <- k
      storeInEvalCache graphRef res

eval :: MonadX m => GraphRef -> m Val
eval graphRef = withEvalCache graphRef $ do
  graph <- getGraph graphRef
  case graph of
    Literal n -> LitResult n
    Op opName inputGraphs -> do
      -- TODO limit parallelism, e.g. because operation requires
      -- some resource such as a CPU
      inputValRefs <- parallelFor inputGraphs eval
      withResultCache (opName, inputValRefs) $ do
        error "TODO perform computation based on opName"

-- TODO make use of the above for basic "cached testing" infra
-- Add ops for
--    * `cabal exec` - creating LY file, taking and verifying music-suite version (plan.json+cabal sdist as per aboce), and taking source file example/Foo.hs
--    * `lilypond` - taking .ly file, taking and verifying Ly source, producing SVG
--    * Verify - taking source file example/Foo.hs and SVG, producing proof that user
--      OK'd it.
--    * Gallery - taking SVGs and producing HTML gallery
-- External process invocation:
--    * COMPLETELY sandboxed, files are copied in/out of store as given
--    * CHECKS for any input that might matter but can't be sandboxed (e.g. version)

