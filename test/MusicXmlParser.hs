import Data.Maybe
import Data.Music.MusicXml
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  testFiles <-
    filter
      (\path -> any (`isExtensionOf` path) ["xml", "musicxml"])
      <$> listDirectory "test/musicxml"
  tests <- withCurrentDirectory "test/musicxml" $ traverse xmlTest testFiles
  defaultMain (testGroup "MusicXML parse tests" tests)

xmlTest :: FilePath -> IO TestTree
xmlTest file = do
  contents <- readFile file
  pure $ testCase file $ assertBool "" (isJust $ readXml contents)
