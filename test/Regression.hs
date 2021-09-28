
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Music.Prelude (Music, c, d, e, pseq, timeSignature, (|>))
import Music.Score.Export.StandardNotation (defaultLilypondOptions, runIOExportM, toLy, toStandardNotation)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import qualified Text.Pretty

-- | Export a score as Lilypond. Returns a UTF-8 encoded byte sequence.
toLilypondRaw :: Music -> IO ByteString
toLilypondRaw music = do
  work <- runIOExportM $ toStandardNotation music
  (h, ly) <- runIOExportM $ toLy defaultLilypondOptions work
  let ly' = h ++ show (Text.Pretty.pretty ly)
  pure $ fromStrict $ encodeUtf8 $ pack ly'

tests :: [TestTree]
tests =
  [ goldenVsString
      "tempo-change-generated"
      "test/regression/lilypond/tempo-change-generated.ly"
      $ do
        toLilypondRaw $ c |> timeSignature (3 / 4) (pseq [d, e])
  ]

main = defaultMain (testGroup "Regression tests" tests)
