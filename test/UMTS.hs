{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints
  -fno-warn-unused-local-binds
  -fno-warn-noncanonical-monoid-instances #-}


{-# LANGUAGE TypeApplications #-}

-- |
-- Rendering tests based on http://lilypond.org/doc/v2.18/input/regression/musicxml/collated-files.html.
--
-- Serves to track the expressivity of StandardNotation.
import Music.Score.Export.StandardNotation
import qualified Music.Pitch.Literal as P
import qualified Music.Parts
import qualified System.Directory
import Music.Parts (Instrument)
import Music.Pitch (MajorMinor(MajorMode, MinorMode))
import qualified Music.Pitch
import qualified Data.Music.MusicXml as MusicXml
import qualified Music.Score.Meta.Key
import qualified Music.Score.Internal.Util
import qualified Music.Pitch.Literal
import qualified Music.Score.Pitch
import Music.Time
import Control.Exception (SomeException, handle)
import Music.Score.Internal.Quantize
  ( Rhythm (..),
    dotMod,
    quantize,
    rewrite,
  )
import qualified Music.Dynamics.Literal as D
import qualified Music.Score.Articulation
import Music.Score.Articulation (ArticulationT (..))
import Music.Score.Color (ColorT, runColorT)
import qualified Music.Score.Dynamics
import Music.Score.Dynamics (DynamicT (..))
import qualified Music.Score.Export.ArticulationNotation
import Music.Score.Export.ArticulationNotation (marks, slurs)
import qualified Music.Score.Export.ArticulationNotation as AN
import Music.Score.Export.DynamicNotation (crescDim, dynamicLevel)
import qualified Music.Score.Export.DynamicNotation
import qualified Music.Score.Export.DynamicNotation as DN
import qualified Music.Score.Export.TechniqueNotation as TN
import Music.Score.Harmonics (HarmonicT, runHarmonicT)
import Data.Semigroup
import qualified Music.Score.Internal.Export
import Data.IORef
import Data.Traversable
import Control.Lens ((.~), at, _1, _2)
import Data.VectorSpace
import BasePrelude hiding ((<>), First (..), first, second)

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- TODO lyrics
-- TODO chord symbols
-- TODO staff crossings
-- TODO trills
-- TODO 8va etc
-- TODO consolidate clef/key sig representations
-- TODO names as part of label tree (for piano/harp/chorus/strings etc)
-- TODO xml/render transposed staves (72a, 72b)
{-
  Need to emit transpose element like this
  Must add to musicxml2
  Add to first bar of staff based on its instrument (we don't allow mid-staff instrument changes)
  This is for Trumpet in Bb, so interval representation is the same as in Music.Pitch.Interval

    Use Parts.transposition on the instrument, which will give us that interval (-P5 for horn)

     <transpose>
      <diatonic>-1</diatonic>
      <chromatic>-2</chromatic>
    </transpose>


-}
-- TODO xml/arpeggio
-- TODO xml/special barlines
-- TODO xml/fermatas
-- TODO xml/generate nicer display-pitch for rests when using multiple pitch layers (03b)
-- TODO xml/51b staff name not printed

-- ‘01a-Pitches-Pitches.xml’
{-
All pitches from G to c'''' in
ascending steps; First without accidentals, then with a sharp and then
with a flat accidental. Double alterations and cautionary accidentals
are tested at the end.
-}
-- TODO time signature "c"
umts_01a :: Work
umts_01a =
  Work mempty
    $ pure
    $ Movement (movementTitle .~ "Pitches and accidentals" $ mempty) sysStaff
    $ Leaf staff
  where
    sysStaff = cycle [mempty]
    staff = Staff mempty $ fmap (\chords -> Bar mempty [PitchLayer $ rh4 chords]) chs
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat (1 / 4)) cs
    chs :: [[Chord]]
    chs = fmap (fmap singleNoteChord) $ divideList 4 pitches
      where
        pitches =
          mconcat
            [ baseScale,
              fmap Music.Pitch.sharpen baseScale,
              fmap Music.Pitch.flatten baseScale,
              -- TODO cx', cbb', cs', cs', cs', cs'(editorial)
              [ Music.Pitch.sharpen (Music.Pitch.sharpen Music.Pitch.c'),
                Music.Pitch.flatten (Music.Pitch.flatten Music.Pitch.c'),
                Music.Pitch.cs',
                Music.Pitch.cs',
                Music.Pitch.cs',
                Music.Pitch.cs'
              ]
            ]
    -- TODO this is fromPitch
    singleNoteChord :: Pitch -> Chord
    singleNoteChord ps = pitches .~ [ps] $ mempty
    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList
    baseScale :: [Pitch]
    baseScale =
      [ P.g__,
        P.a__,
        P.b__
      ]
        ++ Music.Score.Pitch.enumDiatonicFromTo
          P.c_
          P.c'''

-- ‘01b-Pitches-Intervals.xml’
-- TODO time signature "2/4"
umts_01b :: Work
umts_01b =
  Work mempty
    $ pure
    $ Movement (movementTitle .~ "Various pitches and interval sizes" $ mempty) sysStaff
    $ Leaf staff
  where
    sysStaff = (timeSignature .~ (Just $ First $ 2 / 4) $ mempty) : cycle [mempty]
    staff = Staff mempty $ fmap (\chords -> Bar mempty [PitchLayer $ rh4 chords]) chs
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat (1 / 4)) cs
    chs :: [[Chord]]
    chs = fmap (fmap singleNoteChord) $ divideList 2 pitches
      where
        pitches = interleave (u <> Music.Score.Pitch._8va (init u)) (d <> Music.Score.Pitch._8vb (init d))
        u =
          Music.Score.Pitch._8va
            [ P.c,
              P.cs,
              P.db,
              P.d,
              P.ds,
              P.eb,
              P.e,
              P.es,
              P.fb,
              P.f,
              P.fs,
              P.gb,
              P.g,
              P.gs,
              P.ab,
              P.a,
              P.as,
              P.bb,
              P.b,
              P.bs,
              P.cb'
            ]
        d =
          [ P.c',
            P.cb',
            P.bs,
            P.b,
            P.bb,
            P.as,
            P.a,
            P.ab,
            P.gs,
            P.g,
            P.gb,
            P.fs,
            P.f,
            P.fb,
            P.es,
            P.e,
            P.eb,
            P.ds,
            P.d,
            P.db,
            P.cs
          ]
    interleave :: [a] -> [a] -> [a]
    interleave [] ys = ys
    interleave xs [] = xs
    interleave (x : xs) (y : ys) = x : y : interleave xs ys
    singleNoteChord :: Pitch -> Chord
    singleNoteChord ps = pitches .~ [ps] $ mempty
    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList

-- ‘01c-Pitches-NoVoiceElement.xml’
umts_01c :: Work
umts_01c =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = cycle [mempty]
    staff = Staff mempty $ [Bar mempty [PitchLayer $ rh4 [singleNoteChord $ Music.Pitch.Literal.g]]]
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat 1) cs
    singleNoteChord :: Pitch -> Chord
    singleNoteChord ps = pitches .~ [ps] $ mempty
    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList

-- ‘01d-Pitches-Microtones.xml’
{-
Some microtones: c flat-and-a-half, d half-flat, e half-sharp, f sharp-and-a half.
Once in the lower and once in the upper region of the staff.
-}
umts_01d :: Work
umts_01d =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty
    pitches =
      []

{-
  Possible naming convention:
    Name    Adjustment (whole-tones)
    cbb     -1
    cqb     -3/4
    cb      -1/2
    cq      -1/4
    c       0
    cz      1/4
    cs      1/2
    czs     3/4
    css     1
-}

-- c 3/4 flat
-- d 1/4 flat
-- e 1/4 sharp
-- f 3/4 sharp
-- c' 3/4 flat
-- d' 1/4 flat
-- e' 1/4 sharp
-- f' 3/4 sharp

-- ‘01e-Pitches-ParenthesizedAccidentals.xml’
-- IGNORE

-- ‘01f-Pitches-ParenthesizedMicrotoneAccidentals.xml’
-- IGNORE

-- ‘02a-Rests-Durations.xml’
umts_02a :: Work
umts_02a =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty
    -- As specified:
    durs =
      mconcat
        [ fmap (dotMod 0 *) [2, 1, 1 / 2, 1 / 4, 1 / 8, 1 / 16, 1 / 32, 1 / 64, 1 / 128, 1 / 128],
          fmap (dotMod 1 *) [2, 1, 1 / 2, 1 / 4, 1 / 8, 1 / 16, 1 / 32, 1 / 64, 1 / 128, 1 / 128]
        ]

-- Note: this does not render as expected in Lilypond suite

-- ‘02b-Rests-PitchedRests.xml’
umts_02b :: Work
umts_02b =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = [timeSignature .~ (Just $ First (5 / 4)) $ mempty]
    staff = Staff mempty [Bar mempty [PitchLayer $ Group $ fmap (\dur -> Beat dur mempty) durs]]
    durs = fmap (/ 4) [1, 1, 1, 1, 1] :: [Duration]
    -- TODO use this info
    -- Should not be Pitch, but some kind of layout type
    restPositions :: [Pitch]
    restPositions = fmap (\n -> Music.Score.Pitch.up (Music.Pitch._P5 ^* n) Music.Pitch.b) [0, -1, 1, -2, 2]

-- ‘02c-Rests-MultiMeasureRests.xml’
umts_02c :: Work
umts_02c =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty
    durs = [3, 16, 12] :: [Duration]

-- ‘02d-Rests-Multimeasure-TimeSignatures.xml’
umts_02d :: Work
umts_02d =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf
    $ Staff mempty bars
  where
    sysStaff :: SystemStaff
    sysStaff = map (\ts -> timeSignature .~ (fmap First ts) $ mempty) timeSigs2
    bars :: [Bar]
    bars = fmap (\d -> Bar mempty [PitchLayer $ quant (pitches .~ [] $ mempty) d]) durs
    quant :: a -> Duration -> Rhythm a
    quant x d = case d of
      d
        | d == 2 / 4 -> Beat (1 / 2) x
        | d == 3 / 4 -> Dotted 1 $ Beat (1 / 2) x
        | d == 4 / 4 -> Beat 1 x
        | otherwise -> error "umts_02d: bad duration"
    timeSigs2 =
      concat $ zipWith (\n ts -> Just ts : replicate (n -1) Nothing) numBarRests timeSigs ::
        [Maybe TimeSignature]
    durs =
      concat $ zipWith (\n ts -> replicate n (realToFrac @Double ts)) numBarRests timeSigs ::
        [Duration]
    numBarRests = [2, 3, 2, 2]
    timeSigs = [4 / 4, 3 / 4, 2 / 4, 4 / 4]

-- TODO emit whole bar rests (with correct duration?) or multirests

-- ‘02e-Rests-NoType.xml’
-- IGNORE

-- ‘03a-Rhythm-Durations.xml’
{-
All note durations, from long, brevis, whole until 128th; First with their plain values,
then dotted and finally doubly-dotted.
-}
umts_03a :: Work
umts_03a =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf
    $ Staff mempty bars
  where
    sysStaff =
      [ timeSignature .~ (Just $ First (16 / 4)) $ mempty,
        mempty,
        timeSignature .~ (Just $ First (24 / 4)) $ mempty,
        mempty,
        timeSignature .~ (Just $ First (28 / 4)) $ mempty,
        mempty
      ]
    bars :: [Bar]
    bars =
      [ mkBar 0 [4],
        mkBar 0 baseDurs,
        mkBar 1 [4],
        mkBar 1 baseDurs,
        mkBar 2 [4],
        mkBar 2 baseDurs
      ]
    mkBar :: Int -> [Duration] -> Bar
    mkBar numDots ds =
      Bar mempty $ pure $ PitchLayer $ Group $
        fmap
          ( \d ->
              (if numDots > 0 then Dotted numDots else id) $
                Beat d (pitches .~ [P.c'] $ mempty)
          )
          ds
    baseDurs = [2, 1, 1 / 2, 1 / 4, 1 / 8, 1 / 16, 1 / 32, 1 / 64, 1 / 128, 1 / 256]

-- -- 2 bars of 16/4, two bars of 24/4, two bars of 28/4
-- durs :: [[Duration]]
-- durs = mconcat
--   [ (fmap.fmap) (dotMod 0 *) [[4], [2, 1, 1/2, 1/4, 1/8, 1/16, 1/32, 1/64, 1/128, 1/256]]
--   , (fmap.fmap) (dotMod 1 *) [[4], [2, 1, 1/2, 1/4, 1/8, 1/16, 1/32, 1/64, 1/128, 1/256]]
--   , (fmap.fmap) (dotMod 2 *) [[4], [2, 1, 1/2, 1/4, 1/8, 1/16, 1/32, 1/64, 1/128, 1/256]]
--   ]

-- ‘03b-Rhythm-Backup.xml’
{-
Two voices with a backup, that does not jump to the beginning for the measure for
voice 2, but somewhere in the middle. Voice 2 thus won’t have any notes or rests
for the first beat of the measures.
-}
{-
TODO when multiple layers are being used, set display-pitch
in rests to different values to minimize collissions.
-}
umts_03b :: Work
umts_03b =
  Work mempty
    $ pure
    $ Movement mempty (mempty : repeat mempty)
    $ Leaf
    $ Staff mempty
    $ pure
    $ Bar
      mempty
      [ PitchLayer $ listToRh $ fmap maybePitchToCh voice1,
        PitchLayer $ listToRh $ fmap maybePitchToCh voice2
      ]
  where
    listToRh :: [a] -> Rhythm a
    listToRh xs = Group $ fmap (Beat (1 / 4)) xs
    maybePitchToCh :: Maybe Pitch -> Chord
    maybePitchToCh Nothing = mempty
    maybePitchToCh (Just x) = pitches .~ [x] $ mempty
    -- timeSig = 4/4 :: TimeSignature
    voice1 =
      [ Just Music.Pitch.c,
        Just Music.Pitch.c,
        Nothing
      ]
    voice2 =
      [ Nothing,
        Just Music.Pitch.a_,
        Just Music.Pitch.a_
      ]

-- ‘03c-Rhythm-DivisionChange.xml’
-- IGNORE

-- ‘03d-Rhythm-DottedDurations-Factors.xml’
{-
Several durations can be written with dots. For multimeasure rests, we can also
have durations that cannot be expressed with dotted notes (like 5/8).
-}
-- IGNORE

-- ‘11a-TimeSignatures.xml’
{-
Various time signatures: 2/2 (alla breve), 4/4 (C), 2/2, 3/2, 2/4, 3/4, 4/4,
5/4, 3/8, 6/8, 12/8
-}
umts_11a :: Work
umts_11a =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf
    $ Staff mempty bars
  where
    sysStaff :: SystemStaff
    sysStaff = map (\ts -> timeSignature .~ (Just $ First ts) $ mempty) timeSigs
    bars :: [Bar]
    bars = fmap (\d -> Bar mempty [PitchLayer $ quant tie (pitches .~ [P.c'] $ mempty) d]) durs
    tie ::
      Bool -> -- begin/end
      Chord ->
      Chord
    tie True = (ties . _2) .~ Any True
    tie False = (ties . _1) .~ Any True
    quant :: (Bool -> a -> a) -> a -> Duration -> Rhythm a
    quant addTie x d = case d of
      d
        | d == (3 / 8) -> Dotted 1 $ Beat (1 / 4) x
        | d == (1 / 2) -> Beat (1 / 2) x
        | d == (3 / 4) -> Dotted 1 $ Beat (1 / 2) x
        | d == (1) -> Beat 1 x
        | d == (5 / 4) -> Group [Beat 1 (addTie True x), Beat (1 / 4) (addTie False x)] -- TODO ties
        | d == (6 / 4) -> Dotted 1 $ Beat 1 x
        | otherwise -> error "umts_11a: bad duration"
    durs :: [Duration]
    durs = fmap realToFrac timeSigs
    timeSigs :: [TimeSignature]
    timeSigs =
      [ 2 / 2, -- TODO Music.Score.cutTime
        4 / 4, -- TODO Music.Score.commonTime
        2 / 2,
        3 / 2,
        2 / 4,
        3 / 4,
        4 / 4,
        5 / 4,
        3 / 8,
        6 / 8,
        12 / 8
      ]

-- ‘11b-TimeSignatures-NoTime.xml’
umts_11b :: Work
umts_11b =
  Work mempty
    $ pure
    $ Movement mempty (repeat mempty)
    $ Leaf staff
  where
    staff :: Staff
    staff = mempty
    timeSigs :: [TimeSignature]
    timeSigs =
      [ (3 + 2) / 8,
        (5 + 3 + 1) / 4
      ]

-- ‘11c-TimeSignatures-CompoundSimple.xml’
-- IGNORE

-- ‘11d-TimeSignatures-CompoundMultiple.xml’
-- IGNORE

-- ‘11e-TimeSignatures-CompoundMixed.xml’
-- IGNORE

-- ‘11f-TimeSignatures-SymbolMeaning.xml’
-- IGNORE

-- ‘11g-TimeSignatures-SingleNumber.xml’
-- IGNORE

-- ‘11h-TimeSignatures-SenzaMisura.xml’
-- IGNORE

-- ‘12a-Clefs.xml’
{-
Various clefs: G, C, F, percussion, TAB and none; some are also possible with
transposition and on other staff lines than their default
(e.g. soprano/alto/tenor/baritone C clefs); Each measure shows a different clef
(measure 17 has the "none" clef), only measure 18 has the same treble clef as
measure 1.
-}
umts_12a :: Work
umts_12a =
  Work mempty
    $ pure
    $ Movement mempty (repeat mempty)
    $ Leaf staff
  where
    staff :: Staff
    staff = Staff (instrumentDefaultClef .~ Music.Pitch.trebleClef $ mempty) bars
    bars = fmap middleCWithClefBar clefs
    middleCWithClefBar Nothing =
      Bar
        mempty
        [PitchLayer $ Beat 1 P.c]
    middleCWithClefBar (Just clef) =
      Bar
        (at 0 .~ Just clef $ mempty)
        [PitchLayer $ Beat 1 P.c]
    clefs :: [Maybe Music.Pitch.Clef]
    clefs =
      [ Nothing, -- Verify that staff clef is being used
        Just $ Music.Pitch.altoClef,
        Just $ Music.Pitch.tenorClef,
        Just $ Music.Pitch.bassClef,
        Just $ Music.Pitch.percussionClef,
        Nothing, -- , TODO Music.Pitch.treble8vbClef
        Nothing, -- , TODO Music.Pitch.bass8vbClef
        Just $ Music.Pitch.bassClef, -- TODO 2 half-positions lower
        Just $ Music.Pitch.trebleClef, -- TODO 2 half-positions lower
        Just $ Music.Pitch.baritoneClef,
        Just $ Music.Pitch.mezzoSopranoClef,
        Just $ Music.Pitch.sopranoClef,
        Nothing, -- , TODO Music.Pitch.tabClef
        Nothing, -- , TODO Music.Pitch.treble8vaClef
        Nothing, -- , TODO Music.Pitch.bass8vaClef
        Nothing, -- , TODO Music.Pitch.tabWithTextTabClef
        Nothing, -- , TODO Music.Pitch.noClef
        Just $ Music.Pitch.trebleClef -- again!
      ]

-- ‘12b-Clefs-NoKeyOrClef.xml’
umts_12b :: Work
umts_12b =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    -- TODO the Option/Just/First pattern is too painful!
    -- I feel all meta-types should implement this natively (see above)
    -- I.e. there is the "empty time signature" called mempty
    -- and also all the other variants (which can always be written as a fractional number)
    sysStaff = (timeSignature .~ (Just $ First $ 4 / 4) $ mempty) : cycle [mempty]
    staff = Staff mempty $ [bar, bar]
    bar = Bar mempty [PitchLayer $ rh4 [singleNoteChord $ Music.Pitch.Literal.c]]
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat 1) cs
    singleNoteChord :: Pitch -> Chord
    singleNoteChord ps = pitches .~ [ps] $ mempty
    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList

-- ‘13a-KeySignatures.xml’
{-
Various key signature: from 11 flats to 11 sharps (each one first one measure in
major, then one measure in minor)
-}
-- TODO consolidate key sig between music-score and music-pitch
umts_13a :: Work
umts_13a =
  Work mempty
    $ pure
    $ Movement (movementTitle .~ "Different Key signatures" $ mempty) sysStaff
    $ Leaf staff
  where
    staff :: Staff
    staff = Staff mempty $ take (length sysStaff) $ repeat $ Bar mempty [PitchLayer $ Beat (1 / 2) $ pitches .~ [P.c] $ mempty]
    -- sysStaff = zipWith (\setTS ks -> setTS $ keySignature .~ (Option $ Just $ First $ ks) $ mempty)
    --   -- TODO just 2/4
    --   ( (timeSignature .~ (Option $ Just $ First (2/4))) : (timeSignature .~ (Option $ Just $ First (3/4))) : repeat mempty)
    --   keySigs

    -- TODO include TS too!
    sysStaff =
      fmap
        (\ks -> keySignature .~ ks $ mempty)
        keySigs
    keySigs :: [KeySignature]
    keySigs = concatMap (\i -> fmap (\m -> Music.Score.Meta.Key.key (error "TODO" i) m) modesPerBar) fifthPerTwoBars
    fifthPerTwoBars = [-11 .. 11] :: [Music.Score.Meta.Key.Fifths]
    modesPerBar = [MajorMode, MinorMode]

-- ‘13b-KeySignatures-ChurchModes.xml’
{-All different modes: major, minor, ionian, dorian, phrygian, lydian, mixolydian,
aeolian, and locrian; All modes are given with 2 sharps.-}
-- IGNORE

-- ‘13c-KeySignatures-NonTraditional.xml’
-- IGNORE

-- ‘13d-KeySignatures-Microtones.xml’
-- IGNORE

-- ‘14a-StaffDetails-LineChanges.xml’
-- IGNORE

-- ‘21a-Chord-Basic.xml’
umts_21a :: Work
umts_21a =
  Work mempty
    $ pure
    $ Movement mempty (repeat mempty)
    $ Leaf staff
  where
    staff :: Staff
    staff = Staff mempty [Bar mempty [PitchLayer notes]]
    notes :: Rhythm Chord
    notes =
      Group $
        fmap
          (\(ps, d) -> Beat d $ pitches .~ ps $ mempty)
          [ ([P.f, P.a], 1 / 4),
            ([], 1 / 4)
          ]

-- ‘21b-Chords-TwoNotes.xml’
umts_21b :: Work
umts_21b =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = [timeSignature .~ (Just $ First (1 / 4)) $ mempty, mempty]
    staff :: Staff
    staff = Staff mempty [bar, bar]
    -- Same in both bars
    bar = Bar mempty [PitchLayer notes]
    notes :: Rhythm Chord
    notes =
      Group $
        fmap
          (\(ps, d) -> Beat d $ pitches .~ ps $ mempty)
          [ ([P.f, P.a], 1 / 4),
            ([P.f, P.a], 1 / 4),
            ([P.f, P.a], 1 / 4),
            ([P.f, P.a], 1 / 4)
          ]

-- ‘21c-Chords-ThreeNotesDuration.xml’
umts_21c :: Work
umts_21c =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = [timeSignature .~ (Just $ First (1 / 4)) $ mempty, mempty]
    staff :: Staff
    staff = Staff mempty [bar1, bar2]
    bar1 = Bar mempty [PitchLayer notes1]
    bar2 = Bar mempty [PitchLayer notes2]
    notes1, notes2 :: Rhythm Chord
    notes1 =
      Group $
        fmap
          (\(dots, ps, d) -> (if dots > 0 then Dotted dots else id) $ Beat d $ pitches .~ ps $ mempty)
          [ (1, [P.f, P.a, P.c'], 1 / 4),
            (0, [P.f, P.a, P.g'], 1 / 8),
            (0, [P.f, P.a, P.c'], 1 / 4),
            (0, [P.f, P.a, P.c'], 1 / 4)
          ]
    notes2 =
      Group $
        fmap
          (\(ps, d) -> Beat d $ pitches .~ ps $ mempty)
          [ ([P.f, P.a, P.e'], 1 / 4),
            ([P.f, P.a, P.f'], 1 / 4),
            ([P.f, P.a, P.d'], 1 / 2)
          ]

-- ‘21d-Chords-SchubertStabatMater.xml’
-- IGNORE

-- ‘21e-Chords-PickupMeasures.xml’
-- TODO

-- ‘21f-Chord-ElementInBetween.xml’
-- IGNORE

-- ‘22a-Noteheads.xml’
-- IGNORE (nice to have!)

-- ‘22b-Staff-Notestyles.xml’
-- IGNORE (nice to have!)

-- ‘22c-Noteheads-Chords.xml’
-- IGNORE (nice to have!)

-- ‘22d-Parenthesized-Noteheads.xml’
-- IGNORE (nice to have!)

-- ‘23a-Tuplets.xml’
{-
Some tuplets (3:2, 3:2, 3:2, 4:2, 4:1, 7:3, 6:2) with the default tuplet bracket
displaying the number of actual notes played. The second tuplet does not have a
number attribute set.
-}
umts_23a :: Work
umts_23a =
  Work mempty $ pure
    $ Movement mempty (sysBar1 : repeat mempty)
    $ Leaf
    $ Staff mempty
    $ pure bar1
  where
    sysBar1 :: SystemBar
    sysBar1 = timeSignature .~ (Just $ First $ 14 / 4) $ mempty
    bar1 :: Bar
    bar1 =
      Bar mempty $ pure $ PitchLayer $
        Group
          [ Tuplet (2 / 3) $
              Group
                [ Beat (1 / 4) (pitches .~ [P.c] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.d] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.e] $ mempty)
                ],
            Tuplet (2 / 3) $
              Group
                [ Beat (1 / 4) (pitches .~ [P.f] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.g] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.a] $ mempty)
                ],
            Tuplet (2 / 3) $
              Group
                [ Beat (1 / 4) (pitches .~ [P.b] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.c'] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.d'] $ mempty)
                ],
            Tuplet (2 / 4) $
              Group
                [ Beat (1 / 4) (pitches .~ [P.e'] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.f'] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.g'] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.a'] $ mempty)
                ],
            Tuplet (1 / 4) $
              Group
                [ Beat (1 / 4) (pitches .~ [P.b'] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.c''] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.c''] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.b'] $ mempty)
                ],
            Tuplet (3 / 7) $
              Group
                [ Beat (1 / 4) (pitches .~ [P.a'] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.g'] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.f'] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.e'] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.d'] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.c'] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.b] $ mempty)
                ],
            Tuplet (2 / 6) $
              Group
                [ Beat (1 / 4) (pitches .~ [P.a] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.g] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.f] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.e] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.d] $ mempty),
                  Beat (1 / 4) (pitches .~ [P.c] $ mempty)
                ]
          ]

-- ‘23b-Tuplets-Styles.xml’
umts_23b :: Work
umts_23b =
  Work mempty $ pure
    $ Movement mempty (sysBar1 : repeat mempty)
    $ Leaf
    $ Staff mempty
    $ bars
  where
    sysBar1 :: SystemBar
    sysBar1 = timeSignature .~ (Just $ First $ 5 / 4) $ mempty
    bars :: [Bar]
    bars =
      [ Bar mempty $ pure $ PitchLayer $
          Group
            [ Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) (pitches .~ [P.c'] $ mempty)),
              Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) (pitches .~ [P.c'] $ mempty)),
              Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) (pitches .~ [P.c'] $ mempty)),
              Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) (pitches .~ [P.c'] $ mempty)),
              Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) (pitches .~ [P.c'] $ mempty))
            ],
        Bar mempty $ pure $ PitchLayer $
          Group
            [ Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) (pitches .~ [P.c'] $ mempty)),
              Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) (pitches .~ [P.c'] $ mempty)),
              Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) (pitches .~ [P.c'] $ mempty)),
              Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) (pitches .~ [P.c'] $ mempty)),
              Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) (pitches .~ [P.c'] $ mempty))
            ],
        Bar mempty $ pure $ PitchLayer $
          Group
            [ Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) (pitches .~ [P.c'] $ mempty)),
              Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) (pitches .~ [P.c'] $ mempty)),
              Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) (pitches .~ [P.c'] $ mempty)),
              Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) (pitches .~ [P.c'] $ mempty)),
              Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) (pitches .~ [P.c'] $ mempty))
            ],
        Bar mempty $ pure $ PitchLayer $
          Group
            [ Tuplet (3 / 4) $ Group [],
              Tuplet (3 / 17) $ Group [],
              Group []
            ]
      ]

-- ‘23c-Tuplet-Display-NonStandard.xml’
umts_23c :: Work
umts_23c =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf
    $ Staff mempty
    $ bars
  where
    sysStaff = (timeSignature .~ (Just $ First $ 4 / 4) $ mempty) : repeat mempty
    bars :: [Bar]
    bars =
      [ Bar mempty $ pure $ PitchLayer $
          Group
            [ Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) ch),
              Tuplet (2 / 3) $ Group (replicate 3 $ Dotted 1 $ Beat (1 / 4) ch)
            ],
        Bar mempty $ pure $ PitchLayer $
          Group
            [ Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) ch),
              Tuplet (2 / 3) $ Group (replicate 3 $ Dotted 1 $ Beat (1 / 4) ch)
            ],
        Bar mempty $ pure $ PitchLayer $
          Group
            [ Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) ch),
              Tuplet (2 / 3) $ Group (replicate 3 $ Dotted 1 $ Beat (1 / 4) ch)
            ],
        Bar mempty $ pure $ PitchLayer $
          Group
            [ Tuplet (2 / 3) $ Group (replicate 3 $ Beat (1 / 8) ch),
              Tuplet (2 / 3) $ Group (replicate 3 $ Dotted 1 $ Beat (1 / 4) ch)
            ]
      ]
    ch = pitches .~ [P.c'] $ mempty

-- ‘23d-Tuplets-Nested.xml’
{-
TODO crashes Sibelius

Check
  set tuplet number attribute (level of neting)
  time-modification
  duration (should be scaled down, regardless of time-modification?)
-}
umts_23d :: Work
umts_23d =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf
    $ Staff mempty
    $ bars
  where
    sysStaff = (timeSignature .~ (Just $ First $ 2 / 4) $ mempty) : repeat mempty
    bars :: [Bar]
    bars =
      [ Bar mempty $ pure $ PitchLayer $
          Group
            [ Tuplet (2 / 3) $
                Group
                  [ baseRh,
                    baseRh,
                    Tuplet (2 / 5) $
                      Group
                        [ baseRh,
                          baseRh,
                          baseRh,
                          baseRh,
                          baseRh
                        ],
                    baseRh,
                    baseRh
                  ]
            ],
        Bar mempty $ pure $ PitchLayer $
          Group
            [ Tuplet (4 / 15) $
                Group
                  [ baseRh,
                    baseRh,
                    baseRh,
                    baseRh,
                    baseRh
                  ],
              Group
                [ baseRh,
                  baseRh
                ]
            ]
      ]
    baseRh = Beat (1 / 8) (pitches .~ [P.c'] $ mempty)

-- ‘23e-Tuplets-Tremolo.xml’
umts_23e :: Work
umts_23e =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty

-- ‘23f-Tuplets-DurationButNoBracket.xml’
umts_23f :: Work
umts_23f =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty

-- ‘24a-GraceNotes.xml’
-- IGNORE (would be nice!)

-- ‘24b-ChordAsGraceNote.xml’
-- IGNORE (would be nice!)

-- ‘24c-GraceNote-MeasureEnd.xml’
-- IGNORE (would be nice!)

-- ‘24d-AfterGrace.xml’
-- IGNORE (would be nice!)

-- ‘24e-GraceNote-StaffChange.xml’
-- IGNORE (would be nice!)

-- ‘24f-GraceNote-Slur.xml’
-- IGNORE (would be nice!)

-- ‘31a-Directions.xml’
umts_31a :: Work
umts_31a =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = (timeSignature .~ (Just $ First $ 4 / 4) $ mempty) : repeat mempty
    staff = Staff mempty $ fmap (\ch -> Bar mempty [PitchLayer $ rh4 ch]) $ divideList 4 chords
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat (1 / 4)) cs
    -- TODO ties in bar 1
    chords :: [Chord]
    chords =
      [ bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        dynamicNotation . dynamicLevel .~ D._p $ bc,
        dynamicNotation . dynamicLevel .~ D.pp $ bc,
        dynamicNotation . dynamicLevel .~ D.ppp $ bc,
        dynamicNotation . dynamicLevel .~ D.pppp $ bc,
        dynamicNotation . dynamicLevel .~ D.ppppp $ bc,
        dynamicNotation . dynamicLevel .~ D.pppppp $ bc,
        dynamicNotation . dynamicLevel .~ D._f $ bc,
        dynamicNotation . dynamicLevel .~ D.ff $ bc,
        dynamicNotation . dynamicLevel .~ D.fff $ bc,
        dynamicNotation . dynamicLevel .~ D.ffff $ bc,
        dynamicNotation . dynamicLevel .~ D.fffff $ bc,
        dynamicNotation . dynamicLevel .~ D.ffffff $ bc,
        dynamicNotation . dynamicLevel .~ D.mp $ bc,
        dynamicNotation . dynamicLevel .~ D.mf $ bc,
        {-dynamicNotation.dynamicLevel .~ D.sf $-} bc, -- TODO special dynamics
        {-dynamicNotation.dynamicLevel .~ D.sfp $-}
        bc,
        {-dynamicNotation.dynamicLevel .~ D.sfpp $-} bc,
        {-dynamicNotation.dynamicLevel .~ D.fp $-} bc,
        {-dynamicNotation.dynamicLevel .~ D.rf $-} bc,
        {-dynamicNotation.dynamicLevel .~ D.rfz $-} bc,
        {-dynamicNotation.dynamicLevel .~ D.sfz $-} bc,
        {-dynamicNotation.dynamicLevel .~ D.sffz $-} bc,
        {-dynamicNotation.dynamicLevel .~ D.fz $-} bc,
        bc,
        dynamicNotation . crescDim .~ pure DN.BeginCresc $ bc,
        dynamicNotation . crescDim .~ pure DN.EndCresc $ bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        -- 11
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        nc,
        nc,
        dynamicNotation . dynamicLevel .~ D._p $ bc, -- TODO subito
        dynamicNotation . dynamicLevel .~ D.ppp $ dynamicNotation . crescDim .~ pure DN.BeginCresc $ bc, -- subito
        dynamicNotation . dynamicLevel .~ D.fff $ dynamicNotation . crescDim .~ pure DN.EndCresc $ bc, -- subito
        nc
      ]
    nc = mempty
    bc = pitches .~ [P.c] $ mempty
    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList

-- ‘31c-MetronomeMarks.xml’
umts_31c :: Work
umts_31c =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty

-- ‘32a-Notations.xml’
{-
All <notation> elements defined in MusicXML. The lyrics show the notation
assigned to each note.

- Fermatas TODO
- Arpeggio/Non-arp TODO
- Articulation marks
- Doits/Fall-offs
- Breath marks
- Trills
- Baroque Ornaments (w accidentals)
- Tremolo
- Bow marks
- Harmonics
- Fingerings/fret marks etc
- Dynamics
- Above/below
-}
umts_32a :: Work
umts_32a =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = Staff mempty $ fmap (\ch -> Bar mempty [PitchLayer $ rh4 ch]) $ divideList 4 chords
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat (1 / 4)) cs
    chords :: [Chord]
    chords =
      [ fermata .~ Fermata $ bc,
        fermata .~ Fermata $ bc,
        fermata .~ ShortFermata $ bc,
        fermata .~ LongFermata $ bc,
        fermata .~ Fermata $ bc, -- TODO inverted
        arpeggioNotation .~ Arpeggio $ bc2,
        arpeggioNotation .~ NoArpeggio $ bc2,
        bc, -- accidental mark
        articulationNotation . marks .~ [AN.Accent] $ bc,
        articulationNotation . marks .~ [AN.Marcato] $ bc,
        articulationNotation . marks .~ [AN.Staccato] $ bc,
        articulationNotation . marks .~ [AN.Tenuto] $ bc,
        articulationNotation . marks .~ [AN.Tenuto, AN.Staccato] $ bc,
        articulationNotation . marks .~ [AN.MoltoStaccato] $ bc,
        articulationNotation . marks .~ [] $ bc, -- TODO spicc
        articulationNotation . marks .~ [] $ bc, -- TODO scoop
        articulationNotation . marks .~ [] $ bc, -- plop
        articulationNotation . marks .~ [] $ bc, -- doit
        articulationNotation . marks .~ [] $ bc, -- falloff
        articulationNotation . marks .~ [] $ bc, -- breath
        articulationNotation . marks .~ [] $ bc, -- caes
        articulationNotation . marks .~ [] $ bc,
        articulationNotation . marks .~ [] $ bc,
        nc,
        -- Trills/Ornaments
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        nc,
        nc,
        -- Bowing etc
        bc, -- TODO upbow
        bc, -- TODO downbow
        harmonicNotation .~ (Any True, 1) $ bc, -- harm
        harmonicNotation .~ (Any True, 1) $ bc, -- nat harm
        harmonicNotation .~ (Any True, 1) $ bc, -- art harm
        harmonicNotation .~ (Any True, 1) $ bc, -- nat h/base
        harmonicNotation .~ (Any True, 1) $ bc, -- nat h/touching
        harmonicNotation .~ (Any True, 1) $ bc, -- nat h/soundin

        -- b13
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        -- b17
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        -- Dynamic
        -- TODO double barline before
        dynamicNotation . dynamicLevel .~ Just 1.5 $ bc,
        dynamicNotation . dynamicLevel .~ Just (-3.5) $ bc,
        bc,
        bc,
        articulationNotation . marks .~ [AN.Staccato, AN.Marcato] $ bc, -- both above
        articulationNotation . marks .~ [AN.Staccato, AN.Marcato, AN.Tenuto] $ bc, -- ab/bel/bel
        nc,
        nc
      ]
    nc = mempty
    bc = pitches .~ [P.c'] $ mempty
    bc2 = pitches .~ [P.c', P.e', P.g'] $ mempty
    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList

-- ‘32b-Articulations-Texts.xml’
umts_32b :: Work
umts_32b =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty

-- ‘32c-MultipleNotationChildren.xml’
-- IGNORE

-- ‘32d-Arpeggio.xml’
umts_32d :: Work
umts_32d =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = Staff mempty $ fmap (\ch -> Bar mempty [PitchLayer $ rh4 ch]) $ divideList 4 chords
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat (1 / 4)) cs
    chords :: [Chord]
    chords =
      [ arpeggioNotation .~ Arpeggio $ bc,
        arpeggioNotation .~ UpArpeggio $ bc,
        arpeggioNotation .~ Arpeggio $ bc,
        arpeggioNotation .~ DownArpeggio $ bc,
        arpeggioNotation .~ Arpeggio $ bc,
        arpeggioNotation .~ NoArpeggioBracket $ bc,
        arpeggioNotation .~ Arpeggio $ bc
      ]
    bc = pitches .~ [P.c, P.e', P.g'] $ mempty
    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList

-- ‘33a-Spanners.xml’
umts_33a :: Work
umts_33a =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = (timeSignature .~ (Just $ First $ 3 / 4) $ mempty) : repeat mempty
    staff = Staff mempty $ fmap (\ch -> Bar mempty [PitchLayer $ rh4 ch]) $ divideList 3 chords
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat (1 / 4)) cs
    -- TODO ties in bar 1
    chords :: [Chord]
    chords =
      -- Ignore tuplet for now (should arguably not be in this test at all)
      [ bc,
        bc,
        bc,
        -- slur
        articulationNotation . slurs .~ [AN.BeginSlur] $ bc,
        bc,
        articulationNotation . slurs .~ [AN.EndSlur] $ bc,
        -- dashed slur
        -- TODO add dash
        articulationNotation . slurs .~ [AN.BeginSlur] $ bc,
        bc,
        articulationNotation . slurs .~ [AN.EndSlur] $ bc,
        -- cresc
        dynamicNotation . crescDim .~ [DN.BeginCresc] $ bc,
        bc,
        dynamicNotation . crescDim .~ [DN.EndCresc] $ bc,
        -- dim
        dynamicNotation . crescDim .~ [DN.BeginDim] $ bc,
        bc,
        dynamicNotation . crescDim .~ [DN.EndDim] $ bc,
        -- tr (one short, one long)
        bc,
        bc,
        bc,
        -- start long tr
        bc,
        nc, -- drawn as (1/2) rest in test, though this is wrong
        nc,
        -- 8va
        bc,
        bc,
        bc,
        -- 15mb
        bc,
        bc,
        bc,
        {-
        brackets
            solid down/down
            dashed down/down
            solid none/down
            dashed none/upsolid none/none
        -}
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        bc,
        -- dashed line above
        bc,
        bc,
        bc,
        -- gliss (wavy) up
        slideNotation . beginSlide .~ Any True $ bc,
        slideNotation . endSlide .~ Any True $ bc,
        nc,
        -- bend/alter
        slideNotation . beginGliss .~ Any True $ bc,
        slideNotation . endGliss .~ Any True $ bc,
        nc,
        -- slide/gliss (solid) down
        slideNotation . beginGliss .~ Any True $ bc,
        slideNotation . endGliss .~ Any True $ bc,
        nc,
        -- grouping
        bc,
        bc,
        bc,
        -- 2 crossbeam
        tremoloNotation .~ CrossBeamTremolo (Just 2) $ bc,
        tremoloNotation .~ CrossBeamTremolo (Just 2) $ bc,
        nc,
        -- hammer-on
        bc,
        bc,
        nc,
        -- pull-off
        bc,
        bc,
        nc,
        -- pedal (down/change/up)
        bc,
        bc,
        bc
      ]
    nc = mempty
    bc = pitches .~ [P.b] $ mempty
    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList

-- ‘33b-Spanners-Tie.xml’
umts_33b :: Work
umts_33b =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = Staff mempty $ fmap (\ch -> Bar mempty [PitchLayer $ rh4 ch]) $ divideList 1 chords
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat 1) cs
    chords :: [Chord]
    chords =
      [ ties .~ (Any False, Any True) $ bc,
        ties .~ (Any True, Any False) $ bc
      ]
    -- nc  = mempty
    bc = pitches .~ [P.f] $ mempty
    -- bc2 = pitches .~ [P.c', P.e', P.g'] $ mempty

    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList

-- ‘33c-Spanners-Slurs.xml’
umts_33c :: Work
umts_33c =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = Staff mempty $ fmap (\ch -> Bar mempty [PitchLayer $ rh4 ch]) $ divideList 4 chords
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat (1 / 4)) cs
    chords :: [Chord]
    chords =
      [ articulationNotation . slurs .~ [AN.BeginSlur] $ bc P.g,
        articulationNotation . slurs .~ [AN.BeginSlur, AN.EndSlur] $ bc P.c',
        articulationNotation . slurs .~ [AN.BeginSlur, AN.EndSlur] $ bc P.a,
        articulationNotation . slurs .~ [AN.EndSlur] $ bc P.g,
        articulationNotation . slurs .~ [AN.BeginSlur] $ bc P.g,
        bc P.c',
        articulationNotation . slurs .~ [AN.EndSlur] $ bc P.a,
        bc P.g
      ]
    bc x = pitches .~ [x] $ mempty
    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList

-- ‘33d-Spanners-OctaveShifts.xml’
umts_33d :: Work
umts_33d =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = Staff mempty $ fmap (\ch -> Bar mempty [PitchLayer $ rh4 ch]) $ divideList 4 chords
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat (1 / 4)) cs
    chords :: [Chord]
    chords =
      []
    nc = mempty
    bc = pitches .~ [P.c'] $ mempty
    bc2 = pitches .~ [P.c', P.e', P.g'] $ mempty
    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList

-- ‘33e-Spanners-OctaveShifts-InvalidSize.xml’
umts_33e :: Work
umts_33e =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = Staff mempty $ fmap (\ch -> Bar mempty [PitchLayer $ rh4 ch]) $ divideList 4 chords
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat (1 / 4)) cs
    chords :: [Chord]
    chords =
      []
    nc = mempty
    bc = pitches .~ [P.c'] $ mempty
    bc2 = pitches .~ [P.c', P.e', P.g'] $ mempty
    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList

-- ‘33f-Trill-EndingOnGraceNote.xml’
umts_33f :: Work
umts_33f =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = Staff mempty $ fmap (\ch -> Bar mempty [PitchLayer $ rh4 ch]) $ divideList 4 chords
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat (1 / 4)) cs
    chords :: [Chord]
    chords =
      []
    nc = mempty
    bc = pitches .~ [P.c'] $ mempty
    bc2 = pitches .~ [P.c', P.e', P.g'] $ mempty
    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList

-- ‘33g-Slur-ChordedNotes.xml’
umts_33g :: Work
umts_33g =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = Staff mempty $ fmap (\ch -> Bar mempty [PitchLayer $ rh4 ch]) $ divideList 4 chords
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat (1 / 4)) cs
    chords :: [Chord]
    chords =
      [ articulationNotation . slurs .~ [AN.BeginSlur] $ pitches .~ [P.g, P.c', P.g'] $ mempty,
        pitches .~ [P.a, P.d'] $ mempty,
        articulationNotation . slurs .~ [AN.EndSlur, AN.BeginSlur] $ pitches .~ [P.g, P.d'] $ mempty,
        articulationNotation . slurs .~ [AN.EndSlur] $ pitches .~ [P.c'] $ mempty
      ]
    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList

-- ‘33h-Spanners-Glissando.xml’
umts_33h :: Work
umts_33h =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = Staff mempty $ fmap (\ch -> Bar mempty [PitchLayer $ rh4 ch]) $ divideList 4 chords
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat (1 / 4)) cs
    chords :: [Chord]
    chords =
      [ slideNotation .~ ((Any False, Any False), (Any False, Any False)) $ bc,
        bc2,
        bc,
        bc2,
        bc,
        bc2,
        bc,
        bc2,
        bc,
        bc2,
        bc,
        bc2,
        bc,
        bc2,
        bc,
        bc2,
        bc,
        bc2,
        bc,
        bc2
      ]
    bc = pitches .~ [P.g] $ mempty
    bc2 = pitches .~ [P.f'] $ mempty
    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList

-- ‘33i-Ties-NotEnded.xml’
umts_33i :: Work
umts_33i =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = Staff mempty $ fmap (\ch -> Bar mempty [PitchLayer $ rh4 ch]) $ divideList 4 chords
    rh4 :: [Chord] -> Rhythm Chord
    rh4 cs = mconcat $ fmap (Beat (1 / 4)) cs
    chords :: [Chord]
    chords =
      []
    nc = mempty
    bc = pitches .~ [P.c'] $ mempty
    bc2 = pitches .~ [P.c', P.e', P.g'] $ mempty
    divideList :: Int -> [a] -> [[a]]
    divideList = Music.Score.Internal.Util.divideList

-- ‘41a-MultiParts-Partorder.xml’
{-
A piece with four parts (P0, P1, P2, P3; different from what Finale creates!). Are they converted in the correct order?
-}
umts_41a :: Work
umts_41a =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ fromListLT staves
  where
    sysStaff = [keySignature .~ keySig $ mempty]
    staves = zipWith (\name -> Staff (instrumentFullName .~ name $ mempty) . pure) names bars
    keySig = Music.Score.Meta.Key.key Music.Pitch.g MajorMode
    bars =
      fmap
        ( \p ->
            Bar
              mempty
              [ PitchLayer $
                  Group
                    [ Beat (1 / 4) (pitches .~ [p] $ mempty),
                      Beat (3 / 4) (pitches .~ [] $ mempty)
                    ]
              ]
        )
        pitches_
    names = ["Part " ++ show @Integer n | n <- [1 .. 4]]
    pitches_ =
      [ Music.Pitch.c :: Music.Pitch.Pitch,
        Music.Pitch.e,
        Music.Pitch.g,
        Music.Pitch.b
      ]

-- ‘41b-MultiParts-MoreThan10.xml’
umts_41b :: Work
umts_41b =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ fromListLT staves
  where
    sysStaff = [keySignature .~ keySig $ mempty]
    staves = zipWith (\name -> Staff (instrumentFullName .~ name $ mempty) . pure) names bars
    keySig = Music.Score.Meta.Key.key Music.Pitch.g MajorMode
    bars =
      repeat $
        Bar
          mempty
          [ PitchLayer $
              Group
                [ Beat 1 (pitches .~ [] $ mempty)
                ]
          ]
    names = ["P" ++ show @Integer n | n <- [1 .. 19]]

-- ‘41c-StaffGroups.xml’
{-
  TODO names as part of label tree (for piano/harp/chorus/strings etc)

  A huge orchestra score with 28 parts and different kinds of nested bracketed groups.
  Each part/group is assigned a name and an abbreviation to be shown before the staff.
  Also, most of the groups show unbroken barlines, while the barlines are broken between
  the groups.

-}
umts_41c :: Work
umts_41c = mempty
  where
    _ls :: LabelTree BracketType String
    _ls =
      Branch
        NoBracket
        [ Branch
            Bracket
            [ Leaf "Picc",
              Leaf "Flute 1",
              Leaf "Flute 2",
              Leaf "Oboe",
              Leaf "English Horn",
              Leaf "Clarinet in Eb",
              Leaf "Clarinet in Bb 1",
              Leaf "Clarinet in Bb 2",
              Leaf "Bass Clarinet",
              Leaf "Bassoon 1",
              Leaf "Bassoon 2",
              Leaf "Contrabassoon"
            ],
          Branch
            Bracket
            [ Leaf "Horn 1",
              Leaf "Horn 2",
              Leaf "Trumpet 1",
              Leaf "Trumpet 2",
              Leaf "Trombone 1",
              Leaf "Trombone 2",
              Leaf "Tuba"
            ],
          Leaf "Timpani",
          Leaf "Percussion",
          Branch
            Brace
            [],
          -- harp

          Branch
            Brace
            [],
          -- piano

          Branch
            Bracket
            [ Leaf "Violin I",
              Leaf "Violin II",
              Leaf "Viola",
              Leaf "Cello",
              Leaf "Contrabass"
            ]
        ]

-- ‘41d-StaffGroups-Nested.xml’
umts_41d :: Work
umts_41d = mempty
  where
    _ls :: LabelTree BracketType ()
    _ls =
      Branch
        NoBracket
        [ Leaf (),
          Branch
            Bracket
            [ Leaf (),
              Branch
                Subbracket
                [ Leaf (),
                  Leaf ()
                ]
            ],
          Leaf ()
        ]

-- ‘41e-StaffGroups-InstrumentNames-Linebroken.xml’
umts_41e :: Work
umts_41e = mempty
  where
    _longName = "Long Staff Name"
    _shortName = "St. Nm."

-- ‘41f-StaffGroups-Overlapping.xml’
umts_41f :: Work
umts_41f = mempty
  where

-- ‘41g-PartNoId.xml’
umts_41g :: Work
umts_41g = mempty
  where

-- ‘41h-TooManyParts.xml’
-- IGNORE

-- ‘41i-PartNameDisplay-Override.xml’
umts_41i :: Work
umts_41i = mempty
  where

-- ‘42a-MultiVoice-TwoVoicesOnStaff-Lyrics.xml’
umts_42a :: Work
umts_42a =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty

-- ‘42b-MultiVoice-MidMeasureClefChange.xml’
umts_42b :: Work
umts_42b = mempty
  where

-- ‘43a-PianoStaff.xml’
umts_43a :: Work
umts_43a =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ staves
  where
    sysStaff = [timeSignature .~ (Just $ First $ 4 / 4) $ mempty]
    staves =
      Branch
        Brace
        [ Leaf $ Staff mempty [Bar mempty [PitchLayer $ Beat 1 $ singlePitch P.f]],
          Leaf $ Staff (instrumentDefaultClef .~ bc $ mempty) [Bar mempty [PitchLayer $ Beat 1 $ singlePitch P.b__]]
        ]
    bc = Music.Pitch.bassClef
    singlePitch x = pitches .~ [x] $ mempty

-- ‘43b-MultiStaff-DifferentKeys.xml’
umts_43b :: Work
umts_43b = mempty
  where

-- ‘43c-MultiStaff-DifferentKeysAfterBackup.xml’
umts_43c :: Work
umts_43c = mempty
  where

-- ‘43d-MultiStaff-StaffChange.xml’
umts_43d :: Work
umts_43d = mempty
  where

-- ‘43e-Multistaff-ClefDynamics.xml’
umts_43e :: Work
umts_43e = mempty
  where

-- ‘45a-SimpleRepeat.xml’
-- IGNORE (would be nice!)

-- ‘45b-RepeatWithAlternatives.xml’
-- IGNORE (would be nice!)

-- ‘45c-RepeatMultipleTimes.xml’
-- IGNORE (would be nice!)

-- ‘45d-Repeats-Nested-Alternatives.xml’
-- IGNORE (would be nice!)

-- ‘45e-Repeats-Nested-Alternatives.xml’
-- IGNORE (would be nice!)

-- ‘45f-Repeats-InvalidEndings.xml’
-- IGNORE (would be nice!)

-- ‘45g-Repeats-NotEnded.xml’
-- IGNORE (would be nice!)

-- ‘46a-Barlines.xml’
{-
Different types of (non-repeat) barlines: default (no setting), regular, dotted,
dashed, heavy, light-light, light-heavy, heavy-light, heavy-heavy, tick, short, none.
-}
umts_46a :: Work
umts_46a =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty
    _barLines =
      []

-- ‘46b-MidmeasureBarline.xml’
umts_46b :: Work
umts_46b =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty

-- ‘46c-Midmeasure-Clef.xml’
umts_46c :: Work
umts_46c =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = (timeSignature .~ (Just $ First $ 4 / 4) $ mempty) : cycle [mempty]
    staff =
      Staff
        mempty
        [ mempty,
          Bar (at (1 / 2) .~ Just Music.Pitch.mezzoSopranoClef $ mempty) chords,
          Bar (at (1 / 2) .~ Just Music.Pitch.trebleClef $ mempty) chords
        ]
    chords =
      [PitchLayer $ Group $ replicate 4 $ Beat (1 / 4) P.c]

-- ‘46e-PickupMeasure-SecondVoiceStartsLater.xml’
umts_46e :: Work
umts_46e =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty

-- ‘46f-IncompleteMeasures.xml’
umts_46f :: Work
umts_46f =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty

-- ‘46g-PickupMeasure-Chordnames-FiguredBass.xml’
umts_46g :: Work
umts_46g =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty

-- ‘51b-Header-Quotes.xml’
umts_51b :: Work
umts_51b =
  Work mempty $ pure
    $ Movement
      ( movementTitle .~ title_
          $ movementAttribution . at "composer" .~ Just composer_
          $ mempty
      )
      (repeat mempty)
    $ Leaf
    $ Staff (instrumentFullName .~ instrName_ $ mempty)
    $ pure
    $ Bar mempty
    $ pure
    $ PitchLayer
    $ Beat 1 mempty
  where
    title_ = "\"Quotes\" in header fields"
    composer_ = "Some \"Tester\" name"
    instrName_ = "Staff \"Test\""

-- ‘51c-MultipleRights.xml’
-- IGNORE

-- ‘51d-EmptyTitle.xml’
umts_51d :: Work
umts_51d =
  Work (title .~ wrkTitle $ mempty)
    $ pure
    $ Movement (movementTitle .~ mvmTitle $ mempty) sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = Staff mempty [Bar mempty [PitchLayer (Beat 1 mempty)]]
    wrkTitle = mempty
    mvmTitle = "Empty work title, non-empty movement title"

-- a single bar rest

-- ‘52a-PageLayout.xml’
-- IGNORE (would be nice!)

-- ‘52b-Breaks.xml’
-- IGNORE (would be nice!)

-- ‘61a-Lyrics.xml’
-- IGNORE (would be nice!)

-- ‘61b-MultipleLyrics.xml’
-- IGNORE (would be nice!)

-- ‘61c-Lyrics-Pianostaff.xml’
-- IGNORE (would be nice!)

-- ‘61d-Lyrics-Melisma.xml’
-- IGNORE (would be nice!)

-- ‘61e-Lyrics-Chords.xml’
-- IGNORE (would be nice!)

-- ‘61f-Lyrics-GracedNotes.xml’
-- IGNORE (would be nice!)

-- ‘61g-Lyrics-NameNumber.xml’
-- IGNORE (would be nice!)

-- ‘61h-Lyrics-BeamsMelismata.xml’
-- IGNORE (would be nice!)

-- ‘61i-Lyrics-Chords.xml’
-- IGNORE (would be nice!)

-- ‘61j-Lyrics-Elisions.xml’
-- IGNORE (would be nice!)

-- ‘61k-Lyrics-SpannersExtenders.xml’
-- IGNORE (would be nice!)

-- ‘71a-Chordnames.xml’
-- IGNORE

-- ‘71c-ChordsFrets.xml’
-- IGNORE

-- ‘71d-ChordsFrets-Multistaff.xml’
-- IGNORE

-- ‘71e-TabStaves.xml’
-- IGNORE

-- ‘71f-AllChordTypes.xml’
-- IGNORE (would be nice!)

-- ‘71g-MultipleChordnames.xml’
-- IGNORE (would be nice!)

-- ‘72a-TransposingInstruments.xml’
umts_72a :: Work
umts_72a =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty
    _pitches = [] -- TODO [c,d,e,f,g,a,b,c']
    _origKeySig = (Music.Pitch.g, True) -- G major
    _instruments :: [Instrument]
    _instruments =
      [ Music.Parts.trumpet,
        -- TODO can't represent Eb horn, use F
        Music.Parts.horn,
        Music.Parts.piano
      ]

-- ‘72b-TransposingInstruments-Full.xml’
{-
Various transposition. Each part plays a C5, just displayed in different display
pitches.

The final staff is an untransposed instrument.
-}
umts_72b :: Work
umts_72b =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ fromListLT staves
  where
    sysStaff = [keySignature .~ origKeySig $ mempty]
    staves = fmap mkStaffFromInstrument instruments
    mkStaffFromInstrument _ =
      Staff
        mempty
        [ Bar
            mempty
            [PitchLayer $ Beat 1 (pitches .~ [pitch] $ mempty)]
        ]
    ----
    pitch = Music.Pitch.c'
    origKeySig = Music.Score.Meta.Key.key Music.Pitch.g MajorMode
    instruments :: [Instrument]
    instruments =
      [ Music.Parts.ebClarinet,
        Music.Parts.clarinet,
        Music.Parts.aClarinet,
        Music.Parts.horn, -- TODO can't represent Eb horn, use F
        Music.Parts.horn, -- TODO can't represent picc in A, use Bb
        Music.Parts.piccoloTrumpet,
        Music.Parts.trumpet,
        Music.Parts.cTrumpet,
        Music.Parts.dTrumpet,
        Music.Parts.piano, -- TODO custom transposition
        Music.Parts.flute -- TODO just treble staff
      ]

-- ‘72c-TransposingInstruments-Change.xml’
umts_72c :: Work
umts_72c =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty

-- ‘73a-Percussion.xml’
umts_73a :: Work
umts_73a =
  Work mempty
    $ pure
    $ Movement mempty sysStaff
    $ Leaf staff
  where
    sysStaff = repeat mempty
    staff = mempty
    -- TODO use these:
    _timpNotes =
      [ [(1, P.e_, True)],
        [(1 / 2, P.e_, False), (1 / 2, P.a__, False)]
      ]
    _cymbalNotes =
      [ [(3 / 4, P.c, False), (1 / 4, P.c, False)],
        [(1, P.c, False)]
      ]
    _triangleNotes =
      [ [(3 / 4, P.c, False), (1 / 4, P.c, False)],
        [(1, P.c, False)]
      ]

-- ‘74a-FiguredBass.xml’
-- IGNORE (would be nice though!)

-- ‘75a-AccordionRegistrations.xml’
-- IGNORE

-- ‘90a-Compressed-MusicXML.mxl’
-- IGNORE

-- ‘99a-Sibelius5-IgnoreBeaming.xml’
-- IGNORE

-- ‘99b-Lyrics-BeamsMelismata-IgnoreBeams.xml’
-- IGNORE

umts_export :: IO ()
umts_export = do
  putStrLn $ "Starting UTMS export"
  let dir = "/tmp/music-suite/umts"
  let refDir = "/tmp/music-suite/umts-ref"
  System.Directory.createDirectoryIfMissing True dir
  System.Directory.createDirectoryIfMissing True refDir
  -- TODO use hashes to make sure output does not change
  -- currentHash <- readFile hash

  -- Generate files, counting errors
  errorCount <- newIORef @Integer 0
  _ <- for umts_all $ \(name, work) -> do
    let baseName = dir ++ "/" ++ name
        xmlName = baseName ++ ".xml"
    -- lyName  = baseName ++ ".ly"

    putStr $ name ++ ": \n"
    -- h errorCount $ do
    --   ly <- runIOExportM $ toLy work
    --   writeFile lyName $ show $ Text.Pretty.pretty ly
    -- TODO preamble

    h errorCount $ do
      xml <- runIOExportM $ toXml work
      writeFile xmlName $ MusicXml.showXml xml
  putStrLn $ "UTMS export done"
  ec <- readIORef errorCount
  putStrLn $ "  Number of errors: " ++ show ec
  where
    h c = handle $ \e -> do
      modifyIORef c succ
      putStr "          Error: "
      print (e :: SomeException)

-- h = id
{-
TODO change into map and add function that exports
these as Ly/XML files to a particular directory.
-}
umts_all :: [(String, Work)]
umts_all =
  [ ("umts_01a", umts_01a),
    ("umts_01b", umts_01b),
    ("umts_01c", umts_01c),
    ("umts_01d", umts_01d),
    ("umts_02a", umts_02a),
    ("umts_02b", umts_02b),
    ("umts_02c", umts_02c),
    ("umts_02d", umts_02d),
    ("umts_03a", umts_03a),
    ("umts_03b", umts_03b),
    ("umts_11a", umts_11a),
    ("umts_11b", umts_11b),
    ("umts_12a", umts_12a),
    ("umts_12b", umts_12b),
    ("umts_13a", umts_13a),
    ("umts_21a", umts_21a),
    ("umts_21b", umts_21b),
    ("umts_21c", umts_21c),
    ("umts_23a", umts_23a),
    ("umts_23b", umts_23b),
    ("umts_23c", umts_23c),
    ("umts_23d", umts_23d),
    ("umts_23e", umts_23e),
    ("umts_23f", umts_23f),
    ("umts_31a", umts_31a),
    ("umts_31c", umts_31c),
    ("umts_32a", umts_32a),
    ("umts_32b", umts_32b),
    ("umts_32d", umts_32d),
    ("umts_33a", umts_33a),
    ("umts_33b", umts_33b),
    ("umts_33c", umts_33c),
    ("umts_33d", umts_33d),
    ("umts_33e", umts_33e),
    ("umts_33f", umts_33f),
    ("umts_33g", umts_33g),
    ("umts_33h", umts_33h),
    ("umts_33i", umts_33i),
    ("umts_41a", umts_41a),
    ("umts_41b", umts_41b),
    ("umts_41c", umts_41c),
    ("umts_41d", umts_41d),
    ("umts_41e", umts_41e),
    ("umts_41f", umts_41f),
    ("umts_41g", umts_41g),
    ("umts_41i", umts_41i),
    ("umts_42a", umts_42a),
    ("umts_42b", umts_42b),
    ("umts_43a", umts_43a),
    ("umts_43b", umts_43b),
    ("umts_43c", umts_43c),
    ("umts_43d", umts_43d),
    ("umts_43e", umts_43e),
    ("umts_46a", umts_46a),
    ("umts_46b", umts_46b),
    ("umts_46c", umts_46c),
    ("umts_46e", umts_46e),
    ("umts_46f", umts_46f),
    ("umts_46g", umts_46g),
    ("umts_51b", umts_51b),
    ("umts_51d", umts_51d),
    ("umts_72a", umts_72a),
    ("umts_72b", umts_72b),
    ("umts_72c", umts_72c),
    ("umts_73a", umts_73a)
  ]

main :: IO ()
main = umts_export
