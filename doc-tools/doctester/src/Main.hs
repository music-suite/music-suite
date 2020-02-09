
import Test.DocTest
import System.Environment

main = do
  -- TODO parse these from default-extensions in the relevant *.cabal file
  let defaultExtensions = [ "ConstraintKinds",
                            "DeriveDataTypeable",
                            "DeriveFoldable",
                            "DeriveFunctor",
                            "DeriveTraversable",
                            "GeneralizedNewtypeDeriving",
                            "MultiParamTypeClasses",
                            "NoMonomorphismRestriction",
                            "RankNTypes",
                            "StandaloneDeriving",
                            "TupleSections",
                            "TypeFamilies",
                            "TypeOperators",
                            "ViewPatterns",
                            "OverloadedStrings"
                            ]
  -- doctestWithOptions
  args <- getArgs
  doctest (args ++ fmap ("-X" ++) defaultExtensions)
