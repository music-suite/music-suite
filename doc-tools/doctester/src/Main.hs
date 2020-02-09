
import Test.DocTest
import System.Environment

main = do
  args <- getArgs
  doctest args
