import Data.ByteString.Lazy (ByteString, fromStrict)
import qualified Data.Music.Lilypond as Lilypond
import qualified Codec.Midi as Midi
import qualified Codec.ByteString.Builder
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Music.Prelude (Music, c, d, e, f, g, pseq, timeSignature, (|>), violins, trumpets, parts', solo, fromMidiProgram, stretch, delay)
import Music.Score.Export.StandardNotation (defaultLilypondOptions, runIOExportM, toLy, toStandardNotation)
import qualified Music.Score.Export.StandardNotation as StandardNotation
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString, goldenVsStringDiff)
import qualified Text.Pretty
import qualified Control.Lens

-- | Export a score as Lilypond. Returns an UTF-8 encoded byte sequence.
toLilypondRaw :: Music -> IO ByteString
toLilypondRaw music = do
  work <- runIOExportM $ toStandardNotation music
  (h, ly) <- runIOExportM $ toLy defaultLilypondOptions work
  toLilypondRaw' h ly

-- | Same as @toLilypondRaw@ but accepts @Lilypond.Music@.
toLilypondRaw' :: String -> Lilypond.Music -> IO ByteString
toLilypondRaw' header ly = do
  let ly' = header ++ show (Text.Pretty.pretty ly)
  pure $ fromStrict $ encodeUtf8 $ pack ly'

toMidi :: Music -> IO ByteString
toMidi music = do
  midi <- runIOExportM $ StandardNotation.toMidi music
  let builder = Midi.buildMidi midi
  pure $ Codec.ByteString.Builder.toLazyByteString builder

lilypondRegresionTest :: String -> IO ByteString -> TestTree
lilypondRegresionTest name =
  goldenVsString
    name
    ("test/regression/lilypond/" ++ name ++ ".ly")

midiRegressionTest :: String -> IO ByteString -> TestTree
midiRegressionTest name =
  goldenVsStringDiff
    name
    (\ref new -> ["diff", ref, new]) -- TODO better diff
    ("test/regression/midi/" ++ name ++ ".mid")

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
    midiRegressionTest
      "two-notes"
      $ toMidi $ pseq [c,d],
    midiRegressionTest
      "two-notes-parts"
      $ toMidi $ pseq [Control.Lens.set parts' violins c, Control.Lens.set parts' trumpets d],
    midiRegressionTest
      "set-parts"
      $ toMidi $
         pseq $ fmap (stretch (1/8)) $ concat $
          flip fmap [57.. 80] $ \channel ->
            flip fmap [c, d, e, f, g] $ \note ->
              Control.Lens.set parts' (solo (fromMidiProgram channel)) note
  ]

main = defaultMain (testGroup "Regression tests" tests)
