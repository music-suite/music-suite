import Data.ByteString.Lazy (ByteString, fromStrict)
import qualified Data.Music.Lilypond as Lilypond
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
  toLilypondRaw' h ly

toLilypondRaw' :: String -> Lilypond.Music -> IO ByteString
toLilypondRaw' header ly = do
  let ly' = header ++ show (Text.Pretty.pretty ly)
  pure $ fromStrict $ encodeUtf8 $ pack ly'

lilypondRegresionTest name =
  goldenVsString
    name
    ("test/regression/lilypond/" ++ name ++ ".ly")

tests :: [TestTree]
tests =
  [ lilypondRegresionTest
      "tempo-change-generated" -- TODO this is time, not tempo, fix!!
      $ toLilypondRaw $ c |> timeSignature (3 / 4) (pseq [d, e]),
    lilypondRegresionTest
      "data-lilypond-tempo-change-text-generated"
      $ toLilypondRaw'
        ""
        ( Lilypond.sequential
            (Lilypond.Tempo (Just "Allegro") Nothing)
            (Lilypond.Note c Nothing [])
        ),
    lilypondRegresionTest
      "data-lilypond-tempo-change-bpm-generated"
      $ toLilypondRaw'
        ""
        ( Lilypond.sequential
            (Lilypond.Tempo Nothing (Just (1 / 4, 120)))
            (Lilypond.Note c Nothing [])
        ),
    lilypondRegresionTest
      "data-lilypond-fermata"
      $ toLilypondRaw'
        ""
        ( Lilypond.addArticulation
            Lilypond.Fermata
            (Lilypond.Note c Nothing [])
        )
  ]

main = defaultMain (testGroup "Regression tests" tests)
