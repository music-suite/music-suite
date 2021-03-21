
-- Runs creation for a set of examples. An example is either
--
--    *  Each "hs" file in the examples/ directory
--    *  Each "music+haskell" snippet in docs/src/User-Guide.md
--
-- defaultMain is added if it doesn't exist and the whole thing is run as per "Run example"
-- in the README. This uses the CLI exposed by defaultMain


-- TODO run this in CI, requiring no output changes
-- On output change, require visual inspection
-- This can be "narrowed down" to comparing the "failing" test side by side
