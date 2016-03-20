
module Main where

import System.Exit
import Test.Tasty
import Test.Tasty.Golden
import System.Process

main = defaultMain $ testGroup "All tests" [sanity, golden]

testMusicFile name = testGroup name [
  testMusicFileAs "mid" name,
  testMusicFileAs "xml" name,
  testMusicFileAs "ly"  name
  ]

converter ext = case ext of
  "mid" -> "music2midi"
  "xml" -> "music2musicxml"
  "ly"  -> "music2ly"

testMusicFileAs ext name =
  goldenVsFile 
    (name ++ "." ++ ext)
    ("tests/golden/" ++ name ++ "." ++ ext)
    ("tests/current/" ++ name ++ "." ++ ext)
    (convertFile ext name)

convertFile 
  :: String     -- ^ Extension
  -> FilePath   -- ^ Name
  -> IO ()
convertFile ext name =
    ((mySystem $ converter ext ++" -o tests/current/"++name++"."++ext++" tests/"++name++".music") 
      >>= \e -> if e == ExitSuccess then return () else fail ("Could not convert "++name++".music"))

mySystem str = do
  (_,_,_,p) <- createProcess $ (shell str) { delegate_ctlc = False }
  waitForProcess p   

{-
  This test will always fail if the test files have been edited.

  If you have edited one or more test files and DEFINITELY intend to make 
  your changes permanent, please assure that you are in a clean working
  directory (except for your edits), and then run:

      $ make -f tests/Makefile generate
  
  You should commit all the resulting files (in tests/golden) along with your edits.
-}
sanity = testGroup "Sanity checks" [
  testMusicFilesCheckSum,
  testGenratedFilesChecksum
  ]

testMusicFilesCheckSum =
  goldenVsFile
      "Original file checksums"
      "tests/originals_ref.sha"
      "tests/originals_check.sha"
      (system "shasum tests/*.music | shasum > tests/originals_check.sha" >> return ())

testGenratedFilesChecksum =
  goldenVsFile
      "Generated file checksums"
      "tests/generated_ref.sha"
      "tests/generated_check.sha"
      (system "shasum tests/golden/* | shasum > tests/generated_check.sha" >> return ())

golden = testGroup "Regression tests" [
  testMusicFile "articulation_all_accents",
  testMusicFile "articulation_all_separations",
  testMusicFile "articulation_legato",
  testMusicFile "articulation_portato",
  testMusicFile "articulation_staccato",
  testMusicFile "decl_style1",
  testMusicFile "dynamics_constant",
  testMusicFile "melody_chords",
  testMusicFile "meta_annotations",
  testMusicFile "meta_title",
  testMusicFile "meta_clef1",
  testMusicFile "meta_composer",
  testMusicFile "meta_time_signature",
  testMusicFile "misc_counterpoint",
  testMusicFile "octaves",
  testMusicFile "overlay_chords",
  testMusicFile "overlay_voices",
  testMusicFile "pitch_inv",
  testMusicFile "sharpen",
  testMusicFile "simple_figure",
  testMusicFile "simple_start_later",
  testMusicFile "single_note",
  testMusicFile "special_gliss",
  testMusicFile "special_harmonics",
  testMusicFile "special_text",
  testMusicFile "special_tremolo",
  testMusicFile "stretch_single_note1",
  testMusicFile "stretch_single_note2",
  testMusicFile "stretch_single_note3",
  testMusicFile "times",
  testMusicFile "track_single",
  testMusicFile "voice_single"
  ]