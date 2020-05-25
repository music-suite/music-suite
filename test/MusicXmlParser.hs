import Data.Maybe
import Data.Music.MusicXml
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
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
  contents <- T.unpack . decodeUtf8 <$> B.readFile file
  pure $ testCase file $ assertBool "" (isJust $ readXml contents)
