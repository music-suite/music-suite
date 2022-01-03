import Data.ByteString.Lazy (ByteString, fromStrict)
import qualified Data.Music.Lilypond as Lilypond
import qualified Codec.Midi as Midi
import Codec.Midi (Midi)
import qualified Codec.Midi.Json as Midi.Json
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
import qualified Data.List

-- TODO supress logging for runIOExportM unless flag is passed

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

toMidiJson :: Music -> IO ByteString
toMidiJson music = do
  midi <- runIOExportM $ StandardNotation.toMidi music
  pure $ Midi.Json.encode midi

lilypondRegresionTest :: String -> IO ByteString -> TestTree
lilypondRegresionTest name =
  goldenVsString
    name
    ("test/regression/lilypond/" ++ name ++ ".ly")

midiRegressionTestNoJson :: String -> Music -> TestTree
midiRegressionTestNoJson name midi =
  goldenVsStringDiff
    name
    (\ref new -> ["diff", ref, new]) -- TODO better diff
    ("test/regression/midi/" ++ name ++ ".mid")
    (toMidi midi)

midiJsonRegressionTest :: String -> Music -> TestTree
midiJsonRegressionTest name midi =
  goldenVsString
    name
    ("test/regression/midi-json/" ++ name ++ ".json")
    (toMidiJson midi)

midiRegressionTest :: String -> Music -> [TestTree]
midiRegressionTest name music =
  [ midiRegressionTestNoJson name music, midiJsonRegressionTest name music ]


tests :: [TestTree]
tests =
  [
  testGroup "Lilypond (from high-level DSL)"
    [ lilypondRegresionTest
        "tempo-change-generated" -- TODO this is time, not tempo, fix!!
        $ toLilypondRaw $ c |> timeSignature (3 / 4) (pseq [d, e])
    ],
  testGroup "Lilypond (from Data.Lilypond)"
    [
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
          )
    ],
  testGroup "Midi"
    ((\[mid,json] -> [testGroup "MIDI files" mid, testGroup "JSON files" json]) $ Data.List.transpose [
      midiRegressionTest
        "two-notes"
        $ pseq [c,d],
      midiRegressionTest
        "two-notes-parts"
        $ pseq [Control.Lens.set parts' violins c, Control.Lens.set parts' trumpets d],
      midiRegressionTest
        "set-parts"
        $ stretch (1/16)
        $ pseq $ flip fmap ([56..60]++[64..73]) $ \prog ->
           pseq $ flip fmap [c, d, e, f, g] $ \note ->
            Control.Lens.set parts' (solo $ fromMidiProgram prog) note,
      midiRegressionTest
        "set-parts-issue-91"
        $ stretch (1/16)
        $ pseq $ flip fmap [20.. 30] $ \channel ->
            pseq $ flip fmap [c, d] $ \note ->
              Control.Lens.set parts' (solo $ fromMidiProgram channel) note,
      midiRegressionTest
        "set-parts-issue-91-mini"
        $ stretch (1/16)
        $ pseq $ flip fmap [25,26] $ \channel ->
            pseq $ flip fmap [c] $ \note ->
              Control.Lens.set parts' (solo $ fromMidiProgram channel) note,
      midiRegressionTest
        "set-parts-issue-91-short"
        $ stretch (1/16)
        $ pseq $ flip fmap [20..26] $ \channel ->
            pseq $ flip fmap [c, d] $ \note ->
              Control.Lens.set parts' (solo $ fromMidiProgram channel) note

    ])
  ]

main :: IO ()
main = defaultMain (testGroup "Regression tests" tests)
