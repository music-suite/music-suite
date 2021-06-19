{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

-- |
-- This module defines a monomorphic representation of Western music notation.
--
-- Its main use to simplify the addition of notation-centric backends such as
-- Sibelius, MusicXML, Lilypond, ABC notation and so on.
--
-- It generally follows the conventions of MusixXML but has a slightly more
-- semantic flavor:
--
-- - Bars, staves and rhytmical structure is explicit.
--
-- - Spanners (legato, cresc dim etc) are represented by begin/end tags.
--
--   This is a consequence of representing the score as a tree rather than a graph,
--   as spanners exist independently of the /time/ hierarchy, and trees by
--   definition do not allow multiple hierarchical relationships.
--
--   It could be interesting to use some kind of graph structure instead.
--
-- - Harmoncics are represented as played, i.e. not by specifying note head shape
--   etc explicitly.
--
-- - Time and key signatures are global. Stave-specific key signatures, time
--   signatures or tempi is not allowed.
--
-- - The top level type is 'Work', which allow for representation of multi-movement
--   concert pieces as well as film and theatre music.
--
-- - Like Sibelius (and unlike MusicXML/Lilypond) we use a conceptual \"system
--   staff\" that contains information pertaining to all parts, such as key and
--   time signatures.
module Music.Score.Export.StandardNotation
  ( LabelTree (..),
    foldLabelTree,
    fromListLT,

    -- * Common music notation
    BarNumber,
    TimeSignature,
    KeySignature,
    RehearsalMark,
    TempoMark,
    BracketType (..),
    SystemBar,
    barNumbers,
    timeSignature,
    keySignature,
    rehearsalMark,
    tempoMark,
    SystemStaff,
    systemStaffLength,
    systemStaffTakeBars,
    InstrumentShortName,
    InstrumentFullName,
    Transposition,
    SibeliusFriendlyName,
    SmallOrLarge,
    ScoreOrder,
    StaffInfo,
    instrumentShortName,
    instrumentFullName,
    sibeliusFriendlyName,
    instrumentDefaultClef,
    transposition,
    smallOrLarge,
    scoreOrder,
    Pitch,
    ArpeggioNotation (..),
    TremoloNotation (..),
    BreathNotation (..),
    HarmonicNotation,
    SlideNotation,
    beginGliss,
    endGliss,
    beginSlide,
    endSlide,
    Fermata (..),
    Ties,
    Chord,
    pitches,
    arpeggioNotation,
    tremoloNotation,
    breathNotation,
    articulationNotation,
    dynamicNotation,
    fermata,
    chordColor,
    chordText,
    harmonicNotation,
    slideNotation,
    ties,
    PitchLayer (..),
    Bar (..),
    clefChanges,
    pitchLayers,
    Staff (..),
    staffInfo,
    bars,
    staffLength,
    staffTakeBars,
    Title,
    Annotations,
    Attribution,
    MovementInfo (..),
    movementTitle,
    movementAnnotations,
    movementAttribution,
    Movement (..),
    movementInfo,
    systemStaff,
    staves,
    movementAssureSameNumberOfBars,
    WorkInfo,
    title,
    annotations,
    attribution,
    Work (..),
    workInfo,
    movements,

    -- * MonadLog
    MonadLog (..),

    -- * Pure export monad
    PureExportM,
    runPureExportM,
    runPureExportMNoLog,

    -- * IO export monad
    IOExportM,
    runIOExportM,

    -- * Standard notation
    Asp1,
    Asp1a,
    Asp,
    StandardNotationExportM,
    toStandardNotation,

    -- * Lilypond backend
    LilypondLayout (..),
    LilypondOptions (..),
    defaultLilypondLayout,
    defaultLilypondOptions,
    LilypondExportM,
    toLy,

    -- * MusicXML backend
    MusicXmlExportM,
    toXml,

    -- * MIDI backend
    MidiExportM,
    toMidi,
  )
where

import BasePrelude hiding (First (..), first, second, (<>))
import qualified Codec.Midi as Midi
import Control.Lens
  ( Lens',
    at,
    from,
    over,
    preview,
    to,
    view,
    _1,
    _2,
    _head,
  )
import Control.Lens.Operators hiding ((|>))
import Control.Lens.TH (makeLenses)
import Control.Monad.Except
import Control.Monad.Log
import Control.Monad.Plus
import Control.Monad.State
import Control.Monad.Writer hiding (First (..), (<>))
import Data.AffineSpace
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8
import Data.Colour (Colour)
import Data.Colour.Names
import Data.FileEmbed
import Data.FiniteSeq (FiniteSeq)
import qualified Data.FiniteSeq
import Data.Functor.Couple
import qualified Data.Foldable
import Data.LabelTree (LabelTree (Branch, Leaf), concatLT, foldLabelTree, fromListLT)
import qualified Data.List
import qualified Data.List.NonEmpty
import qualified Data.List.Split
import Data.Map (Map)
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Music.Lilypond as L
import qualified Data.Music.Lilypond as Lilypond
import qualified Data.Music.MusicXml.Simple as MusicXml
import qualified Data.Music.MusicXml.Simple as X
import Data.Semigroup
import qualified Data.Set
import Data.VectorSpace hiding (Sum)
import Music.Articulation (Articulation)
import Music.Dynamics (Dynamics)
import Music.Dynamics.Literal (DynamicsL (..), fromDynamics)
import Music.Notation.Standard
import qualified Music.Notation.Standard.Articulation
import qualified Music.Notation.Standard.Articulation as AN
import qualified Music.Notation.Standard.Dynamic
import qualified Music.Notation.Standard.Dynamic as DN
import qualified Music.Notation.Standard.Technique as TN
import Music.Parts (Instrument, Part, ScoreLayout (..))
import qualified Music.Parts
import Music.Pitch (IsPitch (..), Pitch, fromPitch)
import qualified Music.Pitch
import qualified Music.Pitch.Literal
import qualified Music.Pitch.Literal as P
import Music.Score.Articulation (ArticulationT (..))
import qualified Music.Score.Articulation
import Music.Score.Color (ColorT, runColorT)
import Music.Score.Dynamics (DynamicT (..))
import qualified Music.Score.Dynamics
import Music.Score.Harmonics (HarmonicT, runHarmonicT)
import qualified Music.Score.Internal.Export
import Music.Score.Internal.Instances ()
import Music.Score.Internal.Quantize
  ( Rhythm (..),
    dotMod,
    quantize,
    rewrite,
  )
import Music.Score.Internal.Util (unRatio)
import qualified Music.Score.Internal.Util
import qualified Music.Score.Meta
import qualified Music.Score.Meta.Attribution
import qualified Music.Score.Meta.Key
import qualified Music.Score.Meta.RehearsalMark
import Music.Score.Meta.Tempo (Tempo)
import qualified Music.Score.Meta.Tempo
import qualified Music.Score.Meta.Time
import qualified Music.Score.Meta.Title
import Music.Score.Part (PartT (..))
import qualified Music.Score.Part
import Music.Score.Phrases (MVoice)
import qualified Music.Score.Phrases
import Music.Score.Pitch ()
import qualified Music.Score.Pitch
import Music.Score.Slide (SlideT, runSlideT)
import Music.Score.StaffNumber (StaffNumberT, runStaffNumberT)
import Music.Score.Technique (HasTechniques (techniques), Technique, TechniqueT (..))
import qualified Music.Score.Technique
import Music.Score.Text (TextT, runTextT)
import Music.Score.Ties (Tiable (..), TieT (..))
import qualified Music.Score.Ties
import Music.Score.Tremolo (TremoloT, runTremoloT)
import Music.Time

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |
-- Basic monad for exporting music.
--
-- Run as a pure computation with internal logging.
newtype PureExportM a = PureExportM {runE :: WriterT [String] (ExceptT String Identity) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      Alternative,
      MonadPlus,
      MonadError String,
      MonadWriter [String]
    )

instance MonadLog String PureExportM where
  say x = tell [x]

-- |
-- Run the computation.
--
-- Return the result and all emitted log messages, or an error message.
runPureExportM :: PureExportM a -> Either String (a, [String])
runPureExportM = runExcept . runWriterT . runE

-- |
-- Run the computation.
--
-- Return the result or an error message. The log messages are discarded.
runPureExportMNoLog :: PureExportM a -> Either String a
runPureExportMNoLog = fmap fst . runExcept . runWriterT . runE

-- |
-- Basic failure and logging in the 'IO' monad.
--
-- Logs to standard output.
newtype IOExportM a = IOExportM {runIOExportM_ :: IO a}
  deriving (Functor, Applicative, Monad, MonadFail, Alternative, MonadPlus, MonadIO)

-- TODO add reader for indentation level etc

instance MonadError String IOExportM where
  throwError e = fail e

  -- TODO
  -- catchError (IOExportM k) = IOExportM $ do
  --   catchError k (\e -> throwError (show e))
  catchError = error "MonadError String IOExportM: catchError: No implementation"

instance MonadLog String IOExportM where
  say x = liftIO $ putStrLn $ "  " ++ x

runIOExportM :: IOExportM a -> IO a
runIOExportM = runIOExportM_

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

type Template = String

-- |
-- One-function templating system.
--
-- TODO>>> expand "me : $(name)" (Map.fromList [("name","Hans")])
-- "me : Hans"
expandTemplate :: Template -> Map String String -> String
expandTemplate t vs = (composed $ fmap (expander vs) $ Data.Map.keys vs) t
  where
    expander vs k = replace ("$(" ++ k ++ ")") (Data.Maybe.fromJust $ Data.Map.lookup k vs)
    composed = foldr (.) id
    replace old new = Data.List.intercalate new . Data.List.Split.splitOn old

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

type LilypondExportM m = (MonadLog String m, MonadError String m)

data LilypondLayout
  = -- | Render all music on a single page, without titles (e.g. for inline examples
    -- in the documentation).
    LilypondInline
  | -- | Render normally.
    LilypondScore
  | -- | Render all music on a single page, with titles.
    LilypondBigScore
  deriving (Eq, Show)

defaultLilypondLayout :: LilypondLayout
defaultLilypondLayout = LilypondBigScore

data LilypondOptions = LilypondOptions
  { layout :: LilypondLayout
  }
  deriving (Eq, Show)

defaultLilypondOptions :: LilypondOptions
defaultLilypondOptions =
  LilypondOptions
    { layout = defaultLilypondLayout
    }

toLy :: (LilypondExportM m) => LilypondOptions -> Work -> m (String, Lilypond.Music)
toLy opts work = do
  -- TODO assumes one movement
  say "Lilypond: Assuming one movement only"
  firstMovement <- case work ^? movements . _head of
    Nothing -> throwError "StandardNotation: Expected a one-movement piece"
    Just x -> return x
  let headerTempl =
        Data.Map.fromList
          [ ("title", firstMovement ^. movementInfo . movementTitle),
            ( "composer",
              Data.Maybe.fromMaybe "" $
                firstMovement ^. movementInfo . movementAttribution . at "composer"
            )
          ]
  let headerData = case layout opts of
        LilypondBigScore -> $(embedFile "data/ly_big_score.ly")
        LilypondScore -> $(embedFile "data/ly_score.ly")
        LilypondInline -> $(embedFile "data/ly_inline.ly")
  let header = Data.ByteString.Char8.unpack headerData `expandTemplate` headerTempl
  say "Lilypond: Converting music"
  music <- toLyMusic firstMovement
  return (header, music)

toLyMusic :: (LilypondExportM m) => Movement -> m Lilypond.Music
toLyMusic m2 = do
  let m = movementAssureSameNumberOfBars m2
  -- We will copy system-staff info to each bar (time sigs, key sigs and so on,
  -- which seems to be what Lilypond expects), so the system staff is included
  -- in the rendering of each staff
  renderedStaves <- traverse (toLyStaff $ m ^. systemStaff) (m ^. staves)
  -- Now we still have (LabelTree BracketType), which is converted to a parallel
  -- music expression, using \StaffGroup etc
  toLyStaffGroup renderedStaves

toLyStaff :: (LilypondExportM m) => SystemStaff -> Staff -> m Lilypond.Music
toLyStaff sysBars staff =
  id
    <$> Lilypond.New "Staff" Nothing
    <$> Lilypond.Sequential
    <$> addPartName (staff ^. staffInfo . instrumentFullName)
    <$> addClef (toLyClef $ staff ^. staffInfo . instrumentDefaultClef)
    -- TODO Currently score is always in C with no oct-transp.
    -- To get a transposing score, add \transpose <written> <sounding>

    -- Lilypond key signatures have to be applied to all staves
    -- See http://lilypond.org/doc/v2.19/Documentation/learning/multiple-staves
    -- TODO correct key sig
    <$> (sequence $ zipWith toLyBar sysBars (staff ^. bars))

toLyClef :: Music.Pitch.Clef -> L.Clef
toLyClef c
  | c == Music.Pitch.trebleClef = Lilypond.Treble
  | c == Music.Pitch.altoClef = Lilypond.Alto
  | c == Music.Pitch.tenorClef = Lilypond.Tenor
  | c == Music.Pitch.bassClef = Lilypond.Bass
  | otherwise = Lilypond.Treble

addClef :: L.Clef -> [L.Music] -> [L.Music]
addClef c xs = Lilypond.Clef c : xs

addPartName :: String -> [L.Music] -> [L.Music]
addPartName partName xs = longName : shortName : xs
  where
    longName = Lilypond.Set "Staff.instrumentName" (Lilypond.toValue partName)
    shortName = Lilypond.Set "Staff.shortInstrumentName" (Lilypond.toValue partName)

toLyBar :: (LilypondExportM m) => SystemBar -> Bar -> m Lilypond.Music
toLyBar sysBar bar = do
  let layers = bar ^. pitchLayers
  -- TODO emit \new Voice for each layer?
  sim <$> sysStuff <$> traverse (toLyLayer . getPitchLayer) layers
  where
    -- System information need not be replicated in all layers
    -- TODO other system stuff (reh marks, special barlines etc)
    --
    -- TODO is this really necessary? Is duplicating system info
    -- actually a problem? Is the empty case correct?
    sysStuff [] = []
    sysStuff (x : xs) =
      ( ( lySeq $
            addTimeSignature (sysBar ^. timeSignature) $
              addKeySignature (sysBar ^. keySignature) $
                pure x
        ) :
        xs
      )
    sim [x] = x
    sim xs = Lilypond.Simultaneous False xs

addKeySignature ::
  KeySignature ->
  [Lilypond.Music] ->
  [Lilypond.Music]
addKeySignature (Music.Score.Meta.Key.KeySignature (Data.Monoid.First (Just (tonic, isMajor)))) =
  -- TODO convert
  ( Lilypond.Key
      (Music.Pitch.Literal.fromPitch tonic)
      (case isMajor of Music.Pitch.MajorMode -> Lilypond.Major; Music.Pitch.MinorMode -> Lilypond.Minor)
      :
  )
addKeySignature (Music.Score.Meta.Key.KeySignature (Data.Monoid.First Nothing)) = id

addTimeSignature ::
  Maybe (First TimeSignature) ->
  [Lilypond.Music] ->
  [Lilypond.Music]
addTimeSignature (Just (First (Music.Score.Meta.Time.getTimeSignature -> (ms, n)))) =
  (Lilypond.Time (sum ms) n :)
addTimeSignature Nothing = id

lySeq :: [Lilypond.Music] -> Lilypond.Music
lySeq [x] = x
lySeq xs = Lilypond.Sequential xs

toLyLayer :: (LilypondExportM m) => Rhythm Chord -> m Lilypond.Music
toLyLayer (Beat d x) = toLyChord d x
toLyLayer (Dotted n (Beat d x)) = toLyChord (dotMod n * d) x
toLyLayer (Dotted _n _) = error "Lilypond export: (Dotted x) requires x to be (Beat _)"
toLyLayer (Group rs) = Lilypond.Sequential <$> traverse toLyLayer rs
toLyLayer (Tuplet m r) = Lilypond.Times (realToFrac m) <$> toLyLayer r

{-
TODO _arpeggioNotation::Maybe ArpeggioNotation,
TODO _tremoloNotation::Maybe TremoloNotation,
TODO _breathNotation::Maybe BreathNotation,
-}
toLyChord :: (LilypondExportM m) => Duration -> Chord -> m Lilypond.Music
toLyChord d chord =
  id
    <$> notateTies (chord ^. ties)
    <$> notateGliss (chord ^. slideNotation)
    <$> notateHarmonic (chord ^. harmonicNotation)
    <$> notateText (chord ^. chordText)
    <$> notateColor (chord ^. chordColor)
    -- <$> notateTremolo (chord^.tremoloNotation)
    <$> notateDynamicLy (chord ^. dynamicNotation)
    <$> notateArticulationLy (chord ^. articulationNotation)
    <$> notatePitches d (chord ^. pitches)

notatePitches :: (LilypondExportM m) => Duration -> [Pitch] -> m Lilypond.Music
notatePitches d pitches = case pitches of
  [] -> return $ Lilypond.Rest (Just (realToFrac d)) []
  [x] -> return $ Lilypond.Note (toLyNote x) (Just (realToFrac d)) []
  xs -> return $ Lilypond.Chord (fmap ((,[]) . toLyNote) xs) (Just (realToFrac d)) []

toLyNote :: Pitch -> Lilypond.Note
toLyNote p =
  (`Lilypond.NotePitch` Nothing) $
    Lilypond.Pitch
      ( toEnum (fromEnum $ Music.Pitch.name p),
        -- FIXME catch if (abs accidental)>2 (or simply normalize)
        fromIntegral (Music.Pitch.accidental p),
        -- Lilypond expects SPN, so middle c is octave 4
        fromIntegral $
          Music.Pitch.octaves
            (p .-. Music.Score.Pitch.octavesDown (4 + 1) Music.Pitch.Literal.c)
      )

notateDynamicLy :: DN.DynamicNotation -> Lilypond.Music -> Lilypond.Music
notateDynamicLy (DN.DynamicNotation (crescDims, level)) =
  rcomposed (fmap notateCrescDim crescDims)
    . notateLevel level

notateCrescDim :: DN.CrescDim -> Lilypond.Music -> Lilypond.Music
notateCrescDim crescDims = case crescDims of
  DN.NoCrescDim -> id
  DN.BeginCresc -> Lilypond.beginCresc
  DN.EndCresc -> Lilypond.endCresc
  DN.BeginDim -> Lilypond.beginDim
  DN.EndDim -> Lilypond.endDim

notateLevel :: Maybe Double -> Lilypond.Music -> Lilypond.Music
notateLevel showLevel = case showLevel of
  Nothing -> id
  Just lvl ->
    Lilypond.addDynamics
      ( fromDynamics
          ( DynamicsL
              ((fixLevel . realToFrac $ lvl), Nothing)
          )
      )

fixLevel :: Double -> Double
fixLevel x = fromInteger (round (x - 0.5)) + 0.5

notateArticulationLy ::
  AN.ArticulationNotation ->
  Lilypond.Music ->
  Lilypond.Music
notateArticulationLy (AN.ArticulationNotation (slurs, marks)) =
  rcomposed (fmap notateMark marks)
    . rcomposed (fmap notateSlur slurs)

notateMark ::
  AN.Mark ->
  Lilypond.Music ->
  Lilypond.Music
notateMark mark = case mark of
  AN.NoMark -> id
  AN.Staccato -> Lilypond.addStaccato
  AN.MoltoStaccato -> Lilypond.addStaccatissimo
  AN.Marcato -> Lilypond.addMarcato
  AN.Accent -> Lilypond.addAccent
  AN.Tenuto -> Lilypond.addTenuto
  -- TODO proper exception
  _ -> error "Lilypond export: Unknown articulation mark"

notateSlur :: AN.Slur -> Lilypond.Music -> Lilypond.Music
notateSlur slurs = case slurs of
  AN.NoSlur -> id
  AN.BeginSlur -> Lilypond.beginSlur
  AN.EndSlur -> Lilypond.endSlur

-- TODO This syntax might change in future Lilypond versions
-- TODO handle any color
notateColor :: Maybe (First (Colour Double)) -> Lilypond.Music -> Lilypond.Music
notateColor Nothing = id
notateColor (Just (First color)) = \x ->
  Lilypond.Sequential
    [ Lilypond.Override
        "NoteHead#' color"
        (Lilypond.toLiteralValue $ "#" ++ colorName color),
      x,
      Lilypond.Revert "NoteHead#' color"
    ]

colorName :: Colour Double -> String
colorName c
  | c == Data.Colour.Names.black = "black"
  | c == Data.Colour.Names.red = "red"
  | c == Data.Colour.Names.blue = "blue"
  | otherwise = error "Lilypond backend: Unkown color"

-- TODO not used for now
-- We need to rescale the music according to the returned duration
_notateTremolo ::
  Maybe Int ->
  Duration ->
  (Lilypond.Music -> Lilypond.Music, Duration)
_notateTremolo Nothing d = (id, d)
_notateTremolo (Just 0) d = (id, d)
_notateTremolo (Just n) d =
  let scale = 2 ^ n
      newDur = (d `min` (1 / 4)) / scale
      repeats = d / newDur
   in (Lilypond.Tremolo (round repeats), newDur)

notateText :: [String] -> Lilypond.Music -> Lilypond.Music
notateText texts = composed (fmap (Lilypond.addText' Lilypond.Above) texts)

notateHarmonic :: HarmonicNotation -> Lilypond.Music -> Lilypond.Music
notateHarmonic (Any isNat, Sum n) = case (isNat, n) of
  (_, 0) -> id
  (True, n) -> notateNatural n
  (False, n) -> notateArtificial n
  where
    notateNatural _n = Lilypond.addFlageolet -- addOpen?
    notateArtificial _n = id -- TODO

notateGliss :: SlideNotation -> Lilypond.Music -> Lilypond.Music
notateGliss ((Any _eg, Any _es), (Any bg, Any bs))
  | bg = Lilypond.beginGlissando
  | bs = Lilypond.beginGlissando
  | otherwise = id

notateTies :: Ties -> Lilypond.Music -> Lilypond.Music
notateTies (Any ta, Any tb)
  | ta && tb = Lilypond.beginTie
  | tb = Lilypond.beginTie
  | ta = id
  | otherwise = id

composed :: [a -> a] -> a -> a
composed = Music.Score.Internal.Util.composed

rcomposed :: [a -> a] -> a -> a
rcomposed = Music.Score.Internal.Util.composed . reverse

toLyStaffGroup ::
  (LilypondExportM m) =>
  LabelTree BracketType Lilypond.Music ->
  m Lilypond.Music
toLyStaffGroup = return . foldLabelTree id g
  where
    -- Note: PianoStaff is handled in toLyStaffGroup
    -- Note: Nothing for name (we dump everything inside staves, so no need to identify them)
    g NoBracket ms = k ms
    g Bracket ms = Lilypond.New "StaffGroup" Nothing $ k ms
    g Subbracket ms = Lilypond.New "GrandStaff" Nothing $ k ms
    g Brace ms = Lilypond.New "GrandStaff" Nothing $ k ms
    -- Why False? No separation mark is necessary as the wrapped music is all in separate staves
    k = Lilypond.Simultaneous False

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

{-
Refernces:
  http://lilypond.org/doc/v2.18/input/regression/musicxml/collated-files.html

  http://www.musicxml.com, specifically
  http://www.musicxml.com/UserManuals/MusicXML/MusicXML.htm

  Reference files:
    http://www.musicxml.com/music-in-musicxml/
    Also formats such as Lilypond, Finale and Sibelius can be converted

  See also reference files in:
    musicxml2/test/musicxml
    music-suite/test/extra-musicxml/tests
-}

type MusicXmlExportM m = (MonadLog String m, MonadError String m)

toXml :: (MusicXmlExportM m) => Work -> m MusicXml.Score
toXml work = do
  -- TODO assumes one movement
  say "MusicXML: Assuming one movement only"
  firstMovement <- fmap movementAssureSameNumberOfBars $
    case work ^? movements . _head of
      Nothing -> throwError "StandardNotation: Expected a one-movement piece"
      Just x -> return x
  say "MusicXML: Extracting title and composer"
  let title = firstMovement ^. movementInfo . movementTitle
  let composer = maybe "" id $ firstMovement ^. movementInfo . movementAttribution . at "composer"
  say "MusicXML: Generating part list"
  partList <- movementToPartList firstMovement
  say "MusicXML: Generating bar content"
  partWise <- movementToPartwiseXml firstMovement
  return $ MusicXml.fromParts title composer partList partWise

{-
  Returns the part list, specifying instruments, staves and gruops
  (but not the musical contents of the staves).
-}
movementToPartList :: (MusicXmlExportM m) => Movement -> m MusicXml.PartList
movementToPartList m = return $ foldLabelTree f g (m ^. staves)
  where
    -- TODO generally, what name to use?
    -- TODO use MusicXML sound id
    f s = MusicXml.partList [s ^. staffInfo . sibeliusFriendlyName]
    g bt pl = case bt of
      NoBracket -> mconcat pl
      Subbracket -> mconcat pl
      Bracket -> MusicXml.bracket $ mconcat pl
      Brace -> MusicXml.brace $ mconcat pl

{-
  Returns a matrix of bars in in row-major order, i.e. each inner list
  represents the bars of one particular MusicXML part[1].

  This is suitable for a "partwise" MusicXML score. The transpose of the
  returned matrix is suitable for a "timewise" score.

  [1]: Note that a MusicXML part can be rendered as 1 staff (default) or more,
  so there are two ways to render a piano staff:
    1) Use two MusicXML parts grouped with a brace.
    2) Use one MusicXML part with 2 staves. In this case each note must
       have a staff child element.

-}
movementToPartwiseXml :: (MusicXmlExportM m) => Movement -> m [[MusicXml.Music]]
movementToPartwiseXml movement = music
  where
    music :: (MusicXmlExportM m) => m [[MusicXml.Music]]
    music = do
      staffMusic_ <- staffMusic
      pure $ fmap (zipWith (<>) allSystemBarDirections) staffMusic_
    {-
      Old comment:
            Each entry in outer list must be prepended to the FIRST staff (whatever that is)
            We could also prepend it to other staves, but that is reduntant and makes the
            generated XML file much larger.
      Trying a new approach here by including this in all parts.

      ---
      Again, this definition is a sequnce of elements to be prepended to each bar
      (typically divisions and attributes).
    -}
    allSystemBarDirections :: [MusicXml.Music]
    allSystemBarDirections =
      zipWith3
        (\a b c -> mconcat [a, b, c])
        divisions_
        timeSignatures_
        keySignatures_
      where
        -- TODO bar numbers (?)
        -- TODO reh marks (direction)
        -- TODO tempo marks (direction)
        -- TODO merge attribute elements in the beginning of each bar?

        divisions_ :: [MusicXml.Music]
        divisions_ = MusicXml.defaultDivisions : repeat mempty

        keySignatures_ :: [MusicXml.Music]
        keySignatures_ =
          fmap (expKS . view keySignature) $
            (movement ^. systemStaff)
          where
            expKS (Music.Score.Meta.Key.KeySignature (Data.Monoid.First Nothing)) =
              mempty
            expKS (Music.Score.Meta.Key.KeySignature (Data.Monoid.First (Just ks))) =
              let (fifths, mode) = ks

                  fifths' =
                    MusicXml.Fifths $
                      fromInteger $
                        Music.Score.Meta.Key.getFifths $ fromPitch fifths

                  mode' = case mode of
                    Music.Pitch.MajorMode -> MusicXml.Major
                    Music.Pitch.MinorMode -> MusicXml.Minor
               in MusicXml.key fifths' mode'

        timeSignatures_ :: [MusicXml.Music]
        timeSignatures_ = fmap (expTS . unOF) $ fmap (^. timeSignature) (movement ^. systemStaff)
          where
            unOF = fmap getFirst
            -- TODO recognize common/cut
            expTS Nothing = mempty :: MusicXml.Music
            expTS (Just ts) =
              let (ms, n) = Music.Score.Meta.Time.getTimeSignature ts
               in MusicXml.time (fromIntegral $ sum ms) (fromIntegral n)
    -- System bar directions per bar

    {-
      A matrix similar to the one returned from movementToPartwiseXml, but
      not including information from the system staff.

      TODO we use movementAssureSameNumberOfBars
      We should do a sumilar check on the transpose of the bar/staff matrix
      to assure that all /bars/ have the same duration.
    -}
    staffMusic :: (MusicXmlExportM m) => m [[MusicXml.Music]]
    staffMusic = traverse renderStaff $ movement ^.. staves . traverse
      where
        renderClef :: Music.Pitch.Clef -> MusicXml.Music
        renderClef (Music.Pitch.Clef clef) = case clef of
          (clefSymbol, _octCh, line) ->
            X.Music $
              pure $
                X.MusicAttributes
                  [ X.Clef (exportSymbol clefSymbol) (fromIntegral line + 3) Nothing
                  ]
          where
            -- TODO add octave-adjust to musicxml2
            exportSymbol x = case x of
              Music.Pitch.GClef -> X.GClef
              Music.Pitch.CClef -> X.CClef
              Music.Pitch.FClef -> X.FClef
              Music.Pitch.PercClef -> X.PercClef
              Music.Pitch.NeutralClef -> X.TabClef
        renderStaff :: (MusicXmlExportM m) => Staff -> m [MusicXml.Music]
        renderStaff staff = do
          say "  MusicXML: Generating staff <<>>"
          fromBars <- traverse renderBar (staff ^. bars)
          let clef = renderClef initClef
          pure $ mapHead ((transposeInfo <> clef) <>) fromBars
          where
            mapHead _f [] = []
            mapHead f (x : xs) = f x : xs
            transposeInfo :: MusicXml.Music
            transposeInfo = mempty
            {-
              Always generate a clef at the beginning of the staff based =
              on the instrument default. See comments in the definition of StaffInfo.
            -}
            initClef = staff ^. staffInfo . instrumentDefaultClef
        -- TODO how to best render transposed staves (i.e. clarinets)

        {-
        backup/forward
          - Moves XML "counter" without emitting notes/rests
          - Possibly use sanity check to ensure all layers in a bar are of the
            same length (and the length expected by the time signature).
            If not, we could get some rather strange results!
          - Remember we still need to emit the correct voice (starting with 1 - I recall doing something
           about this, are we always emitting the voice?)
            YES, see setDefaultVoice below?
            How about staff, are we always emitting that?

          - TODO how does this interact with the staff-crossing feature?
            (are we always emitting staff?)
          - TODO how does it interact with clefs/other in-measure elements not
            connected to chords?

            Lots of meta-stuff here about how a bar is represented, would be nice to write up music-score
            eloquently!
        -}

        -- TODO emit line ===== comments in between measures

        renderBar :: (MusicXmlExportM m) => Bar -> m MusicXml.Music
        renderBar bar = do
          -- TODO too verbose: say "    MusicXML: Generating bar <<>>"
          case barLayersHaveEqualDuration bar of
            -- No layers in this bar
            Left [] -> pure mempty
            -- Layers have different durations
            Left _ -> throwError "Layers have different durations"
            -- One or more layers with the same duration
            Right d -> do
              let layers =
                    zipWith
                      MusicXml.setVoice
                      [1 ..]
                      (fmap renderPitchLayer (bar ^. pitchLayers))
                  clefs =
                    Data.Map.foldMapWithKey
                      (\time clef -> [atPosition time (renderClef clef)])
                      (bar ^. clefChanges)
              pure $ mconcat $ Data.List.intersperse (MusicXml.backup $ durToXmlDur d) $ layers <> clefs
          where
            atPosition :: Duration -> X.Music -> X.Music
            atPosition 0 x = x
            atPosition d x = MusicXml.forward (durToXmlDur d) <> x
            durToXmlDur :: Duration -> MusicXml.Duration
            durToXmlDur d = round (realToFrac MusicXml.defaultDivisionsVal * d)
        renderPitchLayer :: PitchLayer -> MusicXml.Music
        renderPitchLayer = renderBarMusic . fmap renderChord . getPitchLayer
        {-
        Render a rest/note/chord.

        This returns a series of <note> elements, with appropriate <chord> tags.
        -}
        renderChord :: Chord -> Duration -> MusicXml.Music
        renderChord ch d = post $ case ch ^. pitches of
          -- TODO Don't emit <alter> tag if alteration is 0
          [] -> MusicXml.rest (realToFrac d)
          [p] -> MusicXml.note (fromPitch_ p) (realToFrac d)
          ps -> MusicXml.chord (fmap fromPitch_ ps) (realToFrac d)
          where
            -- Normalize pitch here if it hasn't been done before
            fromPitch_ = fromPitch . Music.Pitch.useStandardAlterations P.c
            -- TODO arpeggio, breath, color, fermata
            -- (render all constructors from Chord here, except pitch)
            post :: MusicXml.Music -> MusicXml.Music
            post =
              id
                . notateArpeggio (ch ^. arpeggioNotation)
                . notateBreath (ch ^. breathNotation)
                . notateFermata (ch ^. fermata)
                . notateDynamic (ch ^. dynamicNotation)
                . notateArticulation (ch ^. articulationNotation)
                . notateTremolo (ch ^. tremoloNotation)
                . notateText (ch ^. chordText)
                . notateHarmonic (ch ^. harmonicNotation)
                . notateSlide (ch ^. slideNotation)
                . notateTie (ch ^. ties)
        renderBarMusic :: Rhythm (Duration -> MusicXml.Music) -> MusicXml.Music
        renderBarMusic = go
          where
            go (Beat d x) = setDefaultVoice (x d)
            go (Dotted n (Beat d x)) = setDefaultVoice (x (d * dotMod n))
            go (Group rs) = mconcat $ map renderBarMusic rs
            go (Tuplet m r) = MusicXml.tuplet b a (renderBarMusic r)
              where
                (a, b) = bimap fromInteger fromInteger $ unRatio $ realToFrac m
            go _ = error "MusicXML export: (Dotted x) requires x to be (Beat _)"
        setDefaultVoice :: MusicXml.Music -> MusicXml.Music
        setDefaultVoice = MusicXml.setVoice 1
        notateDynamic :: DN.DynamicNotation -> MusicXml.Music -> MusicXml.Music
        notateDynamic (DN.DynamicNotation (crescDims, level)) =
          Music.Score.Internal.Util.composed (fmap notateCrescDim crescDims)
            . notateLevel level
          where
            notateCrescDim crescDims = case crescDims of
              DN.NoCrescDim -> id
              DN.BeginCresc -> (<>) MusicXml.beginCresc
              DN.EndCresc -> (<>) MusicXml.endCresc
              DN.BeginDim -> (<>) MusicXml.beginDim
              DN.EndDim -> (<>) MusicXml.endDim
            -- TODO these literals are not so nice...
            notateLevel showLevel = case showLevel of
              Nothing -> id
              Just lvl ->
                (<>) $
                  MusicXml.dynamic
                    ( fromDynamics
                        ( DynamicsL
                            (fixLevel . realToFrac $ lvl, Nothing)
                        )
                    )
            fixLevel :: Double -> Double
            fixLevel x = fromIntegral @Integer (floor x) + 0.5
        -- DO NOT use rcomposed as notateDynamic returns "mark" order, not application order
        -- rcomposed = composed . reverse

        notateArticulation :: AN.ArticulationNotation -> MusicXml.Music -> MusicXml.Music
        notateArticulation (AN.ArticulationNotation (slurs, marks)) =
          Music.Score.Internal.Util.composed (fmap notateMark marks)
            . Music.Score.Internal.Util.composed (fmap notateSlur slurs)
          where
            notateMark mark = case mark of
              AN.NoMark -> id
              AN.Staccato -> MusicXml.staccato
              AN.MoltoStaccato -> MusicXml.staccatissimo
              AN.Marcato -> MusicXml.strongAccent
              AN.Accent -> MusicXml.accent
              AN.Tenuto -> MusicXml.tenuto
              -- TODO proper exception
              _ -> error "MusicXML export: Unknown articulation mark"
            notateSlur slurs = case slurs of
              AN.NoSlur -> id
              AN.BeginSlur -> MusicXml.beginSlur
              AN.EndSlur -> MusicXml.endSlur
        notateArpeggio :: ArpeggioNotation -> MusicXml.Music -> MusicXml.Music
        notateArpeggio _ x = x
        notateBreath :: BreathNotation -> MusicXml.Music -> MusicXml.Music
        notateBreath _ x = x
        notateFermata :: Fermata -> MusicXml.Music -> MusicXml.Music
        notateFermata _ x = x
        notateTremolo :: TremoloNotation -> MusicXml.Music -> MusicXml.Music
        notateTremolo n = case n of
          -- TODO optionally use z cross-beam
          -- TODO support multi-pitch
          NoTremolo -> id
          CrossBeamTremolo (Just n) -> MusicXml.tremolo (fromIntegral n)
          CrossBeamTremolo Nothing -> MusicXml.tremolo 3
          MultiPitchTremolo _ -> id
        notateText :: [String] -> MusicXml.Music -> MusicXml.Music
        notateText texts a = mconcat (fmap MusicXml.text texts) <> a
        notateHarmonic :: HarmonicNotation -> MusicXml.Music -> MusicXml.Music
        notateHarmonic (Any isNat, Sum n) = notate isNat n
          where
            notate _ 0 = id
            notate True n = notateNatural n
            notate False n = notateArtificial n
            -- notateNatural n = Xml.harmonic -- openString?
            notateNatural _n = MusicXml.setNoteHead MusicXml.DiamondNoteHead
            -- Most programs do not recognize the harmonic tag
            -- We set a single diamond notehead instead, which can be manually replaced
            notateArtificial _n = id -- TODO
        notateSlide :: SlideNotation -> MusicXml.Music -> MusicXml.Music
        notateSlide ((eg, es), (bg, bs)) = notate
          where
            notate = neg . nes . nbg . nbs
            neg = if getAny eg then MusicXml.endGliss else id
            nes = if getAny es then MusicXml.endSlide else id
            nbg = if getAny bg then MusicXml.beginGliss else id
            nbs = if getAny bs then MusicXml.beginSlide else id
        notateTie :: Ties -> MusicXml.Music -> MusicXml.Music
        notateTie (Any ta, Any tb)
          | ta && tb = MusicXml.beginTie . MusicXml.endTie -- TODO flip order?
          | tb = MusicXml.beginTie
          | ta = MusicXml.endTie
          | otherwise = id

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

type MidiInstr = (Midi.Channel, Midi.Preset)

data MidiScore a = MidiScore [(MidiInstr, Score a)]
  deriving (Functor)

type MidiExportM m = (MonadLog String m, MonadError String m)

-- | Convert the given music to a Standard MIDI File.
--
-- * A MIDI file is a number of tracks to be composed in parallel.
--
-- * Each track consists of timestamped messages. Most messages are one of the
-- "normal" ones (on, off, pressure, control change, program change, pitch
-- wheel), each assigned to a channel which is a number in [0..15]. While MIDI
-- files are unlimited in the number of tracks, we are limited to 16 channels
-- because that's how MIDI messages work (MIDI messages predate Standard MIDI
-- Files).
--
-- * We do not emit any SysEx messages.
--
-- * We do not render many meta messages, only the ones required for correct
-- play. E.g. no key signature changes etc (instead we transform the delta
-- times before rendering to MIDI). The MIDI backend is meant to be used for
-- sound rendering only, *not* as a logical/score-level export format.
--
-- * For now we do not support more than 16 sounds types. We only support one
-- sound per instrument (the "standard" one, e.g. arco for violin). We perform
-- no reallocation of channels, instead we check how many instruments there are in
-- the given score and fail if there are more than 15 (we follow convention in
-- reserving channel 10 for percussion, which we currently don't render). We
-- then allocate a channel for each sound.
--
-- * TODO more than 15 sounds, non GM sounds, not just default instrument sound.
toMidi :: (MidiExportM m) => Asp -> m Midi.Midi
toMidi = fmap (finalizeExport . fmap exportNote) . exportScore . mcatMaybes

exportScore :: MidiExportM m => Score Asp1a -> m (MidiScore Asp1a)
exportScore xs = do
  pa :: PartAllocation <- allocateParts $ Data.Set.fromList $ fmap (view Music.Parts.instrument . fst) ys
  pure $
    MidiScore $
      map
        (\(p, sc) -> ((getMidiChannel pa p, getMidiProgram pa p), sc))
        ys
  where
    ys :: [(Part, Score Asp1a)]
    ys =
      Music.Score.Part.extractPartsWithInfo $
        fixTempo $
          normalizeScore xs
    -- TODO We actually want to extract *all* tempo changes and transform the score appropriately
    -- For the time being, we assume the whole score has the same tempo
    fixTempo :: Score Asp1a -> Score Asp1a
    fixTempo = stretch (Music.Score.Meta.Tempo.tempoToDuration (Music.Score.Meta.metaAtStart xs))

type PartAllocation = Map Instrument (Midi.Preset, Midi.Channel)

-- TODO if >15 sounds, allocate, otherwise set everything to piano and log error
allocateParts :: MidiExportM m => Data.Set.Set Instrument -> m PartAllocation
allocateParts ks' = do
  let ks = toList ks'
  if length ks > 15
    then do
      say "Warning: Too many sounds: could not allocate more than 15 MIDI channels"
      pure mempty
    else
      pure $
        Data.Map.fromList
          ( zipWith
              (\p ch -> (p, (fromMaybe 0 $ Music.Parts.toMidiProgram p, ch)))
              ks
              ([0 .. 8] ++ [10 .. 15])
          )

getMidiProgram :: PartAllocation -> Part -> Midi.Preset
getMidiProgram x p = case Data.Map.lookup (view Music.Parts.instrument p) x of
  Nothing -> 0
  Just (pr, _ch) -> pr

getMidiChannel :: PartAllocation -> Part -> Midi.Channel
getMidiChannel x p = case Data.Map.lookup (view Music.Parts.instrument p) x of
  Nothing -> 0
  Just (_pr, ch) -> ch

finalizeExport :: MidiScore (Score Midi.Message) -> Midi.Midi
finalizeExport (MidiScore trs) =
  let controlTrack = [(0, Midi.TempoChange 1000000), (endDelta, Midi.TrackEnd)]
      mainTracks = fmap (uncurry translMidiTrack . fmap join) trs
   in Midi.Midi fileType (Midi.TicksPerBeat divisions) (controlTrack : mainTracks)
  where
    translMidiTrack :: MidiInstr -> Score Midi.Message -> Midi.Track Midi.Ticks
    translMidiTrack (ch, p) =
      addTrackEnd
        . setProgramChannel ch p
        . scoreToMidiTrack divisions
    -- Each track needs TrackEnd
    -- We place it a long time after last event just in case (necessary?)
    addTrackEnd :: [(Int, Midi.Message)] -> [(Int, Midi.Message)]
    addTrackEnd = (<> [(endDelta, Midi.TrackEnd)])
    -- Hardcoded values for Midi export
    -- We always generate MultiTrack (type 1) files with division 1024
    fileType = Midi.MultiTrack
    divisions = 1024
    endDelta = 10000

-- | Set MIDI channel and program/preset for a given track. This changes the channel
-- of all given messages and inserts a single ProgramChange at the beginning.
setProgramChannel ::
  Midi.Channel ->
  Midi.Preset ->
  Midi.Track Midi.Ticks ->
  Midi.Track Midi.Ticks
setProgramChannel ch prg = ([(0, Midi.ProgramChange ch prg)] <>) . fmap (fmap $ setChannel ch)

scoreToMidiTrack :: Duration -> Score Midi.Message -> Midi.Track Midi.Ticks
scoreToMidiTrack divisions = fmap (\(t, _, x) -> (round ((t .-. 0) ^* divisions), x)) . toRelative . (^. triples)

toRelative :: [(Time, Duration, b)] -> [(Time, Duration, b)]
toRelative = snd . Data.List.mapAccumL g 0
  where
    g now (t, d, x) = (t, (0 .+^ (t .-. now), d, x))

exportNote :: Asp1a -> Score Midi.Message
-- TODO make use of SlideT/HarmonicT/TextT/ColorT/TremoloT information
-- For now we throw all of this away using 'snd'
--
-- Arguably these should be retought, see $minorAspect in TODO.md
exportNote (PartT (_, ((snd . runSlideT . snd . runHarmonicT . snd . runTextT . snd . runColorT . snd . runTremoloT . snd . runStaffNumberT) -> x))) = exportNoteT x
  where
    exportNoteT (TechniqueT (Couple (_, x))) = exportNoteA x
    exportNoteA (ArticulationT (_, x)) = exportNoteD x
    exportNoteD (DynamicT (d, x)) = setVelocity (dynLevel d) <$> mkMidiNote x

-- TODO move this to Music.Dynamics.Balance?
dynLevel :: Music.Dynamics.Dynamics -> Midi.Velocity
dynLevel x' = round $ (\x -> x * 58.5 + 64) $ f $ inRange (-1, 1) (x / 3.5)
  where
    x = realToFrac x' :: Double
    f = id
    -- f x = (x^3)
    inRange (m, n) x = (m `max` x) `min` n

mkMidiNote :: Pitch -> Score Midi.Message
mkMidiNote p' =
  mempty
    |> pure (Midi.NoteOn 0 (fromIntegral $ p + 60) 64)
    |> pure (Midi.NoteOff 0 (fromIntegral $ p + 60) 64)
  where
    p = pitchToInt p'
    pitchToInt p = Music.Pitch.semitones (p .-. P.c)

setVelocity :: Midi.Velocity -> Midi.Message -> Midi.Message
setVelocity v = go
  where
    go (Midi.NoteOff c k _) = Midi.NoteOff c k v
    go (Midi.NoteOn c k _) = Midi.NoteOn c k v
    go (Midi.KeyPressure c k _) = Midi.KeyPressure c k v
    go (Midi.ControlChange c n v) = Midi.ControlChange c n v
    go x = x

setChannel :: Midi.Channel -> Midi.Message -> Midi.Message
setChannel c = go
  where
    go (Midi.NoteOff _ k v) = Midi.NoteOff c k v
    go (Midi.NoteOn _ k v) = Midi.NoteOn c k v
    go (Midi.KeyPressure _ k v) = Midi.KeyPressure c k v
    go (Midi.ChannelPressure _ p) = Midi.ChannelPressure c p
    go (Midi.ControlChange _ n v) = Midi.ControlChange c n v
    go (Midi.ProgramChange _ p) = Midi.ProgramChange c p
    go (Midi.PitchWheel _ w) = Midi.PitchWheel c w
    go x = x

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- $fromAspectTranslation
-- -
-- - This piece of code performs the main translation from the internal "linear-algebra style"
-- - music representation (also known as "Asp" as in "musical aspects": pitch, dynamics etc)
-- - to Work, which is a monomorphic type representing standard/Western/common music notation.
-- - The Work type in turn ins the basis for the translation to MusicXML and Lilypond. This
-- - translation perform various notation tasks similar to FOMUS (http://fomus.sourceforge.net/).
-- -
-- - The (Score Asp1) type is used as input and contains no ties. Overlapping events are allowed.
-- -
-- - In (Score Asp2) all exactly overlapping events (in the same part) are grouped into chords,
-- - bar division is performed and ties are added.
-- -
-- - In Asp3, articulation and dynamics are renotated to use explicit context (to allow begin/end
-- - marks to be placed correctly).
-- -
-- - This function also performs quantization, voice and part separation.

type Asp1 = Maybe Asp1a

type Asp1a =
  ( PartT
      Part
      ( StaffNumberT
          ( TremoloT
              ( ColorT
                  ( TextT
                      ( HarmonicT
                          ( SlideT
                              ( TechniqueT
                                  Technique
                                  ( ArticulationT
                                      Articulation
                                      ( DynamicT
                                          Dynamics
                                          Pitch
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
          )
      )
  )

-- We require all notes in a chords to have the same kind of ties
type Asp2 =
  TieT
    ( PartT
        Part
        ( StaffNumberT
            ( TremoloT
                ( ColorT
                    ( TextT
                        ( HarmonicT
                            ( SlideT
                                ( TechniqueT
                                    Technique
                                    ( ArticulationT
                                        Articulation
                                        ( DynamicT
                                            Dynamics
                                            [Pitch]
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )

type Asp3 =
  TieT
    ( PartT
        Part
        ( StaffNumberT
            ( TremoloT
                ( ColorT
                    ( TextT
                        ( HarmonicT
                            ( SlideT
                                ( TechniqueT
                                    TN.TechniqueNotation
                                    ( ArticulationT
                                        AN.ArticulationNotation
                                        ( DynamicT
                                            DN.DynamicNotation
                                            [Pitch]
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )

type Asp = Score Asp1

type StandardNotationExportM m = (MonadLog String m, MonadError String m)

-- |
-- Partition intervals into non-overlapping subsets.
--
-- @
-- mconcat (partitionIntervals xs) = xs
-- @
--
-- TODO track the fact that the returned maps are all non-overlapping
-- in the type system?
partitionIntervals :: forall a b. Ord a => Map (a, a) b -> [Map (a, a) b]
partitionIntervals xs = fmap (Data.Map.fromList . toList) $
  snd $
    (`runState` []) $
      for sorted $ \ev@((start, _stop), _val) -> do
        -- Out list of state is the voices/resources
        -- Inner non-empty list is the spans in chronological order
        currentResources :: [NonEmpty ((a, a), b)] <- get
        let currentEndTimes :: [a] = fmap (snd . fst . Data.List.NonEmpty.head) currentResources
        let nextVoice :: Maybe Int = findIndex (<= start) currentEndTimes
        case nextVoice of
          -- A new voice is needed
          Nothing ->
            modify (++ [pure ev])
          -- This fits in current voice n (0-index)
          Just n ->
            modify (update n $ Data.List.NonEmpty.cons ev)
  where
    sorted :: [((a, a), b)]
    sorted = Data.Map.toAscList xs

update :: Int -> (a -> a) -> [a] -> [a]
update n f xs = take n xs ++ rest
  where
    rest = case drop n xs of
      [] -> []
      x : xs -> f x : xs

toLayer :: (StandardNotationExportM m) => Music.Parts.Part -> Score a -> m (FiniteSeq 4 (MVoice a))
toLayer p xs = case Data.FiniteSeq.parseList $ partitionIntervals $ scoreToMap xs of
  Nothing -> throwError $ "Part has more than four overlapping events: " ++ show p
  Just xs -> pure $ fmap (maybe (error bug) id . preview Music.Score.Phrases.singleMVoice . mapToScore) xs
  where
    bug = "partitionIntervals returned overlapping partition. Plese report this as a bug in music-suite."

{-
-- TODO:
--  Use partitionIntervals
--    If it returns >4 voices, fail (or do custom tie-based sepration?)
--
--  Use (error "partitionIntervals returned overlapping partition") for now, see notes
--  in partitionIntervals about tracking non-overlappingness in the types.
fmap list1 .
  maybe
    (throwError $ "Overlapping events in part: " ++ show p)
    return
    . preview Music.Score.Phrases.singleMVoice
-}

-- TODO move
scoreToMap :: Score a -> Map (Time, Time) a
scoreToMap = Data.Map.fromList . fmap (first (view onsetAndOffset) . view (from event)) . view events

mapToScore :: Map (Time, Time) a -> Score a
mapToScore = view score . fmap (view event . first (view $ from onsetAndOffset)) . Data.Map.toList

toStandardNotation :: (StandardNotationExportM m) => Asp -> m Work
toStandardNotation sc' = do
  say "Simplifying pitches"
  -- Simplify pitch spelling
  let postPitchSimplification = Music.Score.Pitch.simplifyPitches normScore
  -- Part extraction
  say "Extracting parts"
  let postPartExtract :: [(Music.Parts.Part, Score Asp1a)] = Music.Score.Part.extractPartsWithInfo postPitchSimplification
  say $ "  Done, " ++ show (length postPartExtract) ++ " parts"
  -- postPartExtract :: [(Music.Parts.Part,Score Asp1)]

  -- Change aspect type as we need Semigroup to compose all simultanous notes
  -- Merge simultanous notes into chords, to simplify voice-separation
  say "Merging overlapping notes into chords"
  let postChordMerge :: [(Music.Parts.Part, Score Asp2)] = (fmap . fmap) (simultaneous . fmap asp1ToAsp2) postPartExtract
  -- postChordMerge :: [(Music.Parts.Part,Score Asp2)]

  {-
    Currently we:
      - Simultaneous note merging (into chords)
      - MVoice conversion (assuming a single voice)
      - Context rewriting (dynamics, articulation, technique)
      - Bar splitting (adding ties)
      - Quantization
    We want:
      - Simultaneous note merging (into chords)
      - Context rewriting (dynamics, articulation, technique)
          TODO how? This only works for single voices as currently written
      - Bar splitting
      - Voice separation
      - Quantization
    OR:
      - Simultaneous note merging (into chords)
      - Voice sepration (whatever currently works remains a single voice)
        - TODO VS must be linear or nearly!
        - Is "greedy interval coloring" (or "interval partitioning" + posthoc good enough?
          - https://algorithmsandme.com/interval-partitioning-problem/
      - Context rewriting (dynamics, articulation, technique)
        - TODO iff the separated voices are rendered on the *same staff*
          we'll need to rethink the contextual notations carefully. Do not
          support multi-voice staves as a starting point.
      - Bar splitting (adding ties)
      - Quantization

    TODO layer sepration (which, again, does not actually happen in current code)
    should happen after bars have been split.

  -}
  say "Separating voices"
  postVoiceSeparation :: [(Part, FiniteSeq 4 (MVoice Asp2))] <-
    traverse
      ( \a@(p, _) ->
          -- TODO VS: Do actual voice separation here
          traverse (toLayer p) a
      )
      $ postChordMerge
  -- Rewrite dynamics and articulation to be context-sensitive
  -- This changes the aspect type again
  say "Notating dynamics, articulation and playing techniques"
  let postContextSensitiveNotationRewrite ::
        [(Part, FiniteSeq 4 (MVoice Asp3))] =
          (fmap . fmap . fmap) asp2ToAsp3 $ postVoiceSeparation
  -- postContextSensitiveNotationRewrite :: [(Music.Parts.Part,Voice (Maybe Asp3))]

  -- Split each part into bars, splitting notes and adding ties when necessary
  -- Resulting list is list of bars, there is no layering (yet)
  say "Dividing the score into bars"
  let postTieSplit ::
        [(Part, FiniteSeq 4 [Voice (Maybe Asp3)])] =
          (fmap . fmap . fmap) (Music.Score.Ties.splitTiesAt barDurations) $ postContextSensitiveNotationRewrite
  -- For each bar, quantize all layers. This is where tuplets/note values are generated.
  say "Rewriting rhythms"
  postQuantize ::
    [(Part, FiniteSeq 4 [Rhythm (Maybe Asp3)])] <-
    traverse (traverse (traverse (traverse quantizeBar))) postTieSplit
  -- postQuantize :: [(Music.Parts.Part,[Rhythm (Maybe Asp3)])]

  -- Group staves, generating brackets and braces
  say "Generate staff groups"
  let postStaffGrouping ::
        LabelTree BracketType (Part, FiniteSeq 4 [Rhythm (Maybe Asp3)]) =
          generateStaffGrouping postQuantize
  -- All staves with brackets/braces, with one staff generated per voice
  -- We fold those into the LabelTree using SubBracket
  -- TODO support rendering multiple voices per staff instead
  let staffVoices ::
        LabelTree BracketType [Staff] =
          fmap aspectsToStaff postStaffGrouping
  let staves ::
        LabelTree BracketType Staff =
          concatLT Subbracket staffVoices
  say $ "System staff bars: " ++ show (length systemStaff)
  say $ "Regular staff bars: " ++ show (fmap (length . _bars) . Data.Foldable.toList $ staves)
  return $ Work mempty [Movement info systemStaff staves]
  where
    info =
      id $
        movementTitle
          .~ ( Data.Maybe.fromMaybe "" $
                 flip Music.Score.Meta.Title.getTitleAt 0 $
                   Music.Score.Meta.metaAtStart sc
             )
          $ (movementAttribution . at "composer")
            .~ ( flip Music.Score.Meta.Attribution.getAttribution "composer" $ Music.Score.Meta.metaAtStart sc
               )
            $ mempty
    -- TODO also extract Barline, Key, RehearsalMark, Tempo here
    -- (all of these should force a new bar)
    systemStaff :: SystemStaff
    systemStaff =
      fmap
        ( \(_dur, ts, ks) ->
            timeSignature .~ fmap First ts $
              keySignature .~ maybe mempty id ks $
                mempty
        )
        barMeta
    (barDurations, _timeSignatureMarks, _keySignatureMarks) = unzip3 barMeta
    barMeta = Music.Score.Internal.Export.extractBars normScore
    -- Make this more prominent!
    -- This is being used for the actual score!
    normScore = normalizeScore sc -- TODO not necessarliy set to 0...
    --
    sc :: Score Asp1a
    sc = mcatMaybes sc'

asp1ToAsp2 :: Asp1a -> Asp2
asp1ToAsp2 = pure . (fmap . fmap . fmap . fmap . fmap . fmap . fmap . fmap . fmap . fmap) pure

asp2ToAsp3 :: Voice (Maybe Asp2) -> Voice (Maybe Asp3)
asp2ToAsp3 =
  id
    . ( DN.removeCloseDynMarks
          . over Music.Score.Dynamics.dynamics DN.notateDynamic
          . Music.Score.Dynamics.addDynCon
      )
    . ( over Music.Score.Articulation.articulations AN.notateArticulation
          . Music.Score.Articulation.addArtCon
      )
    . (
        -- TODO do "removeCloseDynMarks for playing techniques"
        over techniques TN.notateTechnique
          . Music.Score.Technique.addTechniqueCon
      )

-- TODO optionally log quantization
quantizeBar ::
  (StandardNotationExportM m, Tiable a) =>
  Voice (Maybe a) ->
  m (Rhythm (Maybe a))
quantizeBar = fmap rewrite . quantize' . view Music.Time.pairs
  where
    quantize' x = case quantize x of
      Left e -> throwError $ "Quantization failed: " ++ e
      Right x -> return x

-- | Convert a list of part into a 'LabelTree BracketType' of parts (e.g.
-- a tree with bracket/brace information).
generateStaffGrouping :: [(Part, a)] -> LabelTree BracketType (Part, a)
generateStaffGrouping = scoreLayoutToLabelTree . partDefault

aspectsToStaff :: (Music.Parts.Part, FiniteSeq 4 [Rhythm (Maybe Asp3)]) -> [Staff]
aspectsToStaff (part, voices) =
  fmap (singleStaff part) (Data.FiniteSeq.toList voices)

singleStaff :: Part -> [Rhythm (Maybe Asp3)] -> Staff
singleStaff part bars = Staff info (fmap aspectsToBar bars)
  where
    info =
      id $
        transposition
          .~ (part ^. Music.Parts.instrument . to Music.Parts.transposition)
          $ instrumentDefaultClef
            .~ Data.Maybe.fromMaybe
              Music.Pitch.trebleClef
              (part ^. Music.Parts.instrument . to Music.Parts.standardClef)
            $ instrumentShortName
              .~ Data.Maybe.fromMaybe "" (part ^. Music.Parts.instrument . to Music.Parts.shortName)
              $ instrumentFullName
                .~ (Data.List.intercalate " " $ Data.Maybe.catMaybes [soloStr, nameStr, subpartStr])
                $ mempty
      where
        soloStr = if (part ^. Music.Parts._solo) == Music.Parts.Solo then Just "Solo" else Nothing
        nameStr = part ^. Music.Parts.instrument . to Music.Parts.fullName
        subpartStr = Just $ show (part ^. Music.Parts.subpart)

-- | Group all parts in the default way (e.g. a standard orchestral score with woodwinds
-- on top, followed by brass, etc).
partDefault :: [(Music.Parts.Part, a)] -> ScoreLayout (Music.Parts.Part, a)
partDefault xs = Music.Parts.groupDefault $ fmap (\(p, x) -> (p ^. Music.Parts.instrument, (p, x))) xs

-- | Transform the staff grouping representation from `Music.Parts` into the one
-- used by 'Work'.
scoreLayoutToLabelTree :: ScoreLayout a -> LabelTree BracketType a
scoreLayoutToLabelTree (Single (_, a)) = Leaf a
scoreLayoutToLabelTree (Many gt _ xs) = Branch (k gt) (fmap scoreLayoutToLabelTree xs)
  where
    k Music.Parts.Bracket = Bracket
    k Music.Parts.Invisible = NoBracket
    k Music.Parts.PianoStaff = Brace
    k Music.Parts.GrandStaff = Brace
    k _ = NoBracket

aspectsToChord :: Maybe Asp3 -> Chord
aspectsToChord Nothing = mempty
aspectsToChord (Just asp) =
  id $
    ties .~ (Any endTie, Any beginTie) $
      dynamicNotation .~ (asp ^. Music.Score.Dynamics.dynamic) $
        articulationNotation .~ (asp ^. Music.Score.Articulation.articulation) $
          pitches .~ (asp ^.. Music.Score.Pitch.pitches) $
            chordText .~ TN.textualNotations (asp ^. Music.Score.Technique.technique)
            -- TODO: $ harmonicNotation .~ _ asp
            $
              mempty
  where
    (endTie, beginTie) = Music.Score.Ties.isTieEndBeginning asp

aspectsToBar :: Rhythm (Maybe Asp3) -> Bar
-- TODO handle >1 layers (see below)
-- TODO place clef changes here
aspectsToBar rh = Bar mempty [PitchLayer layer1]
  where
    layer1 = fmap aspectsToChord rh
