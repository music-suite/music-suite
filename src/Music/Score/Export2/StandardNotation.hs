
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections, DeriveDataTypeable, DeriveFoldable, ViewPatterns, DeriveFunctor
  , DeriveTraversable, TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Music.Score.Export2.StandardNotation
  (
    LabelTree(..)
  , foldLabelTree

  -- * Common music notation
  , BarNumber
  , TimeSignature
  , KeySignature
  , RehearsalMark
  , TempoMark
  , BracketType(..)
  , SpecialBarline
  , SystemBar
  , barNumbers
  , timeSignature
  , keySignature
  , rehearsalMark
  , tempoMark
  , SystemStaff
  , InstrumentShortName
  , InstrumentFullName
  , Transposition
  , SibeliusFriendlyName
  , SmallOrLarge
  , ScoreOrder
  , StaffInfo
  , instrumentShortName
  , instrumentFullName
  , sibeliusFriendlyName
  , instrumentDefaultClef
  , transposition
  , smallOrLarge
  , scoreOrder
  , Pitch
  , ArpeggioNotation(..)
  , TremoloNotation(..)
  , BreathNotation(..)
  , ArticulationNotation
  , DynamicNotation
  , HarmonicNotation
  , SlideNotation
  , Ties
  , Chord
  , pitches
  , arpeggioNotation
  , tremoloNotation
  , breathNotation
  , articulationNotation
  , dynamicNotation
  , chordColor
  , chordText
  , harmonicNotation
  , slideNotation
  , ties
  , PitchLayer
  , Bar
  , pitchLayers
  , Staff
  , staffInfo
  , bars
  , Title
  , Annotations
  , Attribution
  , MovementInfo
  , movementTitle
  , movementAnnotations
  , movementAttribution
  , Movement
  , movementInfo
  , systemStaff
  , staves
  , WorkInfo
  , title
  , annotations
  , attribution
  , Work
  , workInfo
  , movements

  -- * E monad
  , E
  , runENoLog
  -- * Asp
  , Asp1
  , Asp
  , fromAspects
  -- * Backends
  , toLy
  , toXml
  -- TODO debug
  , test2
  , test3
  )
where

import           Control.Applicative
import           Control.Lens                            (over, preview, set, to,
                                                          under, view, _head, at)
import           Control.Lens.Operators
import           Control.Lens.TH                         (makeLenses)
import           Control.Monad.Except
import           Control.Monad.Plus
import           Control.Monad.Writer                    hiding ((<>))
import           Data.AffineSpace                        hiding (Sum)
import           Data.Colour                             (Colour)
import           Data.Colour.Names                       as Color
import           Data.Foldable                           (Foldable)
import           Data.Functor.Identity                   (Identity)
import           Data.Map                                (Map)
import qualified Data.Char
import qualified Data.List
import qualified Data.Map
import qualified Data.List.Split
import qualified Data.Maybe
import Data.Bifunctor(bimap)
import qualified Data.Music.Lilypond                     as Lilypond
import qualified Data.Music.MusicXml.Simple              as MusicXml
import           Data.Semigroup
import           Data.Traversable                        (Traversable)
import qualified Data.Traversable
import           Data.VectorSpace                        hiding (Sum)
import qualified Music.Articulation
import qualified Music.Dynamics
import           Music.Dynamics.Literal                  (DynamicsL(..), fromDynamics)
import           Music.Parts                             (Group (..))
import qualified Music.Parts
import qualified Music.Pitch
import qualified Music.Pitch
import qualified Music.Pitch.Literal
import           Music.Score                             (MVoice)
import qualified Music.Score
import           Music.Score.Articulation                (ArticulationT)
import           Music.Score.Dynamics                    (DynamicT)
import qualified Music.Score.Export.ArticulationNotation
import qualified Music.Score.Export.ArticulationNotation as AN
import qualified Music.Score.Export.DynamicNotation
import qualified Music.Score.Export.DynamicNotation      as DN
import qualified Music.Score.Internal.Export
import           Music.Score.Internal.Quantize           (Rhythm (..), dotMod,
                                                          quantize, rewrite)
import qualified Music.Score.Internal.Util
import qualified Music.Score.Internal.Instances ()
import           Music.Score.Internal.Data               (getData)
import qualified Music.Score.Meta
import qualified Music.Score.Meta.Attribution
import qualified Music.Score.Meta.Title
import qualified Music.Score.Meta.Key
import qualified Music.Score.Meta.RehearsalMark
import qualified Music.Score.Meta.Tempo
import qualified Music.Score.Meta.Time
import           Music.Score.Part                        (PartT)
import           Music.Score.Pitch                       ()
import           Music.Score.Ties                        (TieT (..))
import           Music.Score.Tremolo                     (TremoloT, runTremoloT)
import           Music.Time
import           Music.Time.Meta                         (meta)
import qualified Text.Pretty                             as Pretty
import qualified System.Process --DEBUG

{-
type StandardNote =
  PartT
    Part
    (ColorT
       (TextT
          (TremoloT
             (HarmonicT
                (SlideT
                   (ArticulationT Articulation (DynamicT Dynamics [TieT Pitch])))))))
-}

-- TODO
instance Show Music.Score.Meta.Key.KeySignature where show = const "TEMPkeysig"



-- Annotated tree
data LabelTree b a = Branch b [LabelTree b a] | Leaf a
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

foldLabelTree :: (a -> c) -> (b -> [c] -> c) -> LabelTree b a -> c
foldLabelTree f g (Leaf x)      = f x
foldLabelTree f g (Branch b xs) = g b (fmap (foldLabelTree f g) xs)

-- data AltList a b = Nil | Cons a (AltList b a)
--   deriving (Eq, Ord, Show)
-- fromList :: [a] -> AltList () a
-- fromList []     = Nil
-- fromList (x:xs) = Cons () (Cons x (fromList xs))
--
-- toList Nil          = []
-- toList (Cons x Nil)  = x : toList xs
-- toList (Cons () )  = x : toList xs

type BarNumber              = Int
type TimeSignature          = Music.Score.Meta.Time.TimeSignature
type KeySignature           = Music.Score.Meta.Key.KeySignature
type RehearsalMark          = Music.Score.Meta.RehearsalMark.RehearsalMark
type TempoMark              = Music.Score.Meta.Tempo.Tempo

-- TODO w/wo connecting barlines
data BracketType            = NoBracket | Bracket | Brace | Subbracket
  deriving (Eq, Ord, Show)

type SpecialBarline         = () -- TODO
-- type BarLines               = (Maybe SpecialBarline, Maybe SpecialBarline)
-- (prev,next) biased to next

-- TODO lyrics

data SystemBar              = SystemBar {
        _barNumbers::Maybe BarNumber,
        _timeSignature::Maybe TimeSignature,
        _keySignature::Maybe KeySignature,
        _rehearsalMark::Maybe RehearsalMark,
        _tempoMark::Maybe TempoMark
        -- ,_barLines::BarLines
          -- Tricky because of ambiguity. Use balanced pair
          -- or an alt-list in SystemStaff.
        } deriving (Eq,Ord,Show)
instance Monoid SystemBar where
  mempty = SystemBar Nothing Nothing Nothing Nothing Nothing
  mappend x y
    | x == mempty = y
    | otherwise   = x
type SystemStaff            = [SystemBar]


type InstrumentShortName    = String
type InstrumentFullName     = String
type Transposition          = Music.Pitch.Interval
type SibeliusFriendlyName   = String
type SmallOrLarge           = Any -- def False
type ScoreOrder             = Sum Double -- def 0

data StaffInfo              = StaffInfo {
  -- TODO instrument part no. (I, II.1 etc)
  _instrumentShortName::InstrumentShortName,
  _instrumentFullName::InstrumentFullName,
  _sibeliusFriendlyName::SibeliusFriendlyName,
  -- TODO allow for clef/instrument changes within staff
  _instrumentDefaultClef::Music.Pitch.Clef,
  _transposition::Transposition,
  _smallOrLarge::SmallOrLarge,
  _scoreOrder::ScoreOrder
  }
  deriving (Eq,Ord,Show)
instance Monoid StaffInfo where
  mempty = StaffInfo mempty mempty mempty Music.Pitch.trebleClef mempty mempty mempty
  mappend x y
    | x == mempty = y
    | otherwise   = x
type Pitch                  = Music.Pitch.Pitch
data ArpeggioNotation       = Arpeggio | UpArpeggio | DownArpeggio
  deriving (Eq,Ord,Show)
-- As written, i.e. 1/16-notes twice, can be represented as 1/8 note with 1 beams

-- TODO No way to represent 2-pitch tremolo
data TremoloNotation      = BeamedTremolo Int | UnmeasuredTremolo
  deriving (Eq,Ord,Show)

-- type UpDown       = Up | Down
-- data CrossStaff   = NoCrossStaff | NextNoteCrossStaff UpDown | PreviousNoteCrossStaff UpDown
-- data BreathNotation = Fermata | PauseAfter | CaesuraAfter
data BreathNotation         = Comma | Caesura | CaesuraWithFermata
  deriving (Eq,Ord,Show)

type ArticulationNotation   = Music.Score.Export.ArticulationNotation.ArticulationNotation
type DynamicNotation        = Music.Score.Export.DynamicNotation.DynamicNotation
type HarmonicNotation       = (Any, Sum Int)
  -- (artificial?, partial number)
type SlideNotation          = ((Any,Any),(Any,Any))
  -- (endGliss?,endSlide?),(beginGliss?,beginSlide?)
type Ties                   = (Any,Any)
  -- (endTie?,beginTie?)

-- TODO appogiatura/acciatura
-- TODO beaming

-- Rests, single-notes and chords (most attributes are not shown for rests)
data Chord = Chord {
  _pitches::[Pitch],
  _arpeggioNotation::Maybe ArpeggioNotation,
  _tremoloNotation::Maybe TremoloNotation,
  _breathNotation::Maybe BreathNotation,
  _articulationNotation::Maybe ArticulationNotation,
    -- I'd like to put this in a separate layer, but neither Lily nor MusicXML thinks this way
  _dynamicNotation::Maybe DynamicNotation,
  _chordColor::Maybe (Colour Double),
  _chordText::[String],
  _harmonicNotation::HarmonicNotation,
  _slideNotation::SlideNotation,
  _ties::Ties
  }
  deriving (Eq, Show)
instance Monoid Chord where
  mempty = Chord [] Nothing Nothing Nothing Nothing Nothing mempty mempty mempty mempty mempty
  mappend x y
    | x == mempty = y
    | otherwise   = x

type PitchLayer             = Rhythm Chord
-- type DynamicLayer           = Rhythm (Maybe DynamicNotation)

data Bar                    = Bar    {_pitchLayers::[PitchLayer]
  {-, _dynamicLayer::DynamicLayer-}}
  deriving (Eq, Show)




data Staff                  = Staff  {_staffInfo::StaffInfo,_bars::[Bar]}
  deriving (Eq, Show)

type Title                  = String
type Annotations            = [(Span, String)]
type Attribution            = Map String String -- composer, lyricist etc

data MovementInfo = MovementInfo {
  _movementTitle::Title,
  _movementAnnotations::Annotations,
  _movementAttribution::Attribution
  }
  deriving (Eq, Show)

instance Monoid MovementInfo where
  mempty = MovementInfo mempty mempty mempty
  mappend x y
    | x == mempty = y
    | otherwise   = x

data Movement     = Movement {
  _movementInfo::MovementInfo,
  _systemStaff::SystemStaff,
  _staves::LabelTree BracketType Staff  -- Don't allow names for staff groups, only staves
  }
  deriving (Eq, Show)

data WorkInfo     = WorkInfo { _title::Title, _annotations::Annotations, _attribution::Attribution}
  deriving (Eq, Show)
instance Monoid WorkInfo where
  mempty = WorkInfo mempty mempty mempty
  mappend x y
    | x == mempty = y
    | otherwise   = x

data Work         = Work { _workInfo::WorkInfo, _movements::[Movement] }
  deriving (Show)

makeLenses ''SystemBar
makeLenses ''StaffInfo
makeLenses ''Chord
makeLenses ''Bar
makeLenses ''Staff
makeLenses ''MovementInfo
makeLenses ''Movement
makeLenses ''WorkInfo
makeLenses ''Work

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- Log and failure monad
newtype E a = E { runE :: WriterT String (ExceptT String Identity) a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadError String, MonadWriter String)

runENoLog :: E b -> Either String b
runENoLog = fmap fst . runExcept . runWriterT . runE


type Template = String

-- |
-- One-function templating system.
--
-- >>> expand "me : $(name)" (Map.fromList [("name","Hans")])
-- "me : Hans"
--
expandTemplate :: Template -> Map String String -> String
expandTemplate t vs = (composed $ fmap (expander vs) $ Data.Map.keys $ vs) t
  where
    expander vs k = replace ("$(" ++ k ++ ")") (Data.Maybe.fromJust $ Data.Map.lookup k vs)
    composed = foldr (.) id
    replace old new = Data.List.intercalate new . Data.List.Split.splitOn old
    toCamel (x:xs) = Data.Char.toUpper x : xs

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------


toLy :: Work -> E (String, Lilypond.Music)
toLy w = do

  -- TODO assumes one movement
  r <- case w^?movements._head of
    Nothing -> throwError "StandardNotation: Expected a one-movement piece"
    Just x  -> return x

  let headerTempl = Data.Map.fromList [
        ("title",    (r^.movementInfo.movementTitle)),
        ("composer", Data.Maybe.fromMaybe "" $ r^.movementInfo.movementAttribution.at "composer")
        ]
  let header = getData "ly_big_score.ily" `expandTemplate` headerTempl

  m <- toLyMusic $ r
  return (header, m)

  where

    toLyMusic :: Movement -> E Lilypond.Music
    toLyMusic m = do
      -- We will copy system-staff info to each bar (time sigs, key sigs and so on,
      -- which seems to be what Lilypond expects), so the system staff is included
      -- in the rendering of each staff
      renderedStaves <- Data.Traversable.mapM (toLyStaff $ m^.systemStaff) (m^.staves)
      -- Now we still have (LabelTree BracketType), which is converted to a parallel
      -- music expression, using \StaffGroup etc
      toLyStaffGroup renderedStaves

    toLyStaff :: SystemStaff -> Staff -> E Lilypond.Music
    toLyStaff sysBars staff = id
      <$> Lilypond.New "Staff" Nothing
      <$> Lilypond.Sequential
      <$> addPartName (staff^.staffInfo.instrumentFullName)
      <$> addClef (toLyClef $ staff^.staffInfo.instrumentDefaultClef)
      -- TODO Currently score is always in C with no oct-transp.
      -- To get a transposing score, add \transpose <written> <sounding>
      <$> (sequence $ zipWith toLyBar sysBars (staff^.bars))

    toLyClef c
      | c == Music.Pitch.trebleClef = Lilypond.Treble
      | c == Music.Pitch.altoClef   = Lilypond.Alto
      | c == Music.Pitch.tenorClef  = Lilypond.Tenor
      | c == Music.Pitch.bassClef   = Lilypond.Bass
      | otherwise                   = Lilypond.Treble

    addClef c xs = Lilypond.Clef c : xs
    addPartName partName xs = longName : shortName : xs
      where
        longName  = Lilypond.Set "Staff.instrumentName" (Lilypond.toValue partName)
        shortName = Lilypond.Set "Staff.shortInstrumentName" (Lilypond.toValue partName)

    toLyBar :: SystemBar -> Bar -> E Lilypond.Music
    toLyBar sysBar bar = do
      let layers = bar^.pitchLayers
      -- TODO emit \new Voice for eachlayer
      sim <$> sysStuff <$> mapM (toLyLayer) layers
      where
        -- System information need not be replicated in all layers
        -- TODO other system stuff (reh marks, special barlines etc)
        sysStuff [] = []
        sysStuff (x:xs) = (addTimeSignature (sysBar^.timeSignature) x:xs)

        sim [x] = x
        sim xs  = Lilypond.Simultaneous False xs

        addTimeSignature
          :: Maybe Music.Score.Meta.Time.TimeSignature
          -> Lilypond.Music
          -> Lilypond.Music
        addTimeSignature timeSignature x = (setTimeSignature `ifJust` timeSignature) x
          where
            ifJust = maybe id
            setTimeSignature (Music.Score.getTimeSignature -> (ms, n)) x =
                Lilypond.Sequential [Lilypond.Time (sum ms) n, x]


    toLyLayer :: Rhythm Chord -> E Lilypond.Music
    toLyLayer (Beat d x)            = toLyChord d x
    toLyLayer (Dotted n (Beat d x)) = toLyChord (dotMod n * d) x
    toLyLayer (Dotted n _)          = error "FIXME"
    toLyLayer (Group rs)            = Lilypond.Sequential <$> mapM toLyLayer rs
    toLyLayer (Tuplet m r)          = Lilypond.Times (realToFrac m) <$> (toLyLayer r)
      where
        (a,b) = bimap fromIntegral fromIntegral $ unRatio $ realToFrac m
        unRatio = Music.Score.Internal.Util.unRatio


    {-
    TODO _arpeggioNotation::Maybe ArpeggioNotation,
    TODO _tremoloNotation::Maybe TremoloNotation,
    TODO _breathNotation::Maybe BreathNotation,
    -}
    toLyChord :: Duration -> Chord -> E Lilypond.Music
    toLyChord d chord = id
        <$> notateTies (chord^.ties)
        <$> notateGliss (chord^.slideNotation)
        <$> notateHarmonic (chord^.harmonicNotation)
        <$> notateText (chord^.chordText)
        <$> notateColor (chord^.chordColor)
        <$> maybe id notateDynamic (chord^.dynamicNotation)
        <$> maybe id notateArticulation (chord^.articulationNotation)
        <$> notatePitches d (chord^.pitches)
      where
        notatePitches :: Duration -> [Pitch] -> E Lilypond.Music
        notatePitches d pitches = case pitches of
            []  -> return $ Lilypond.Rest                               (Just (realToFrac d)) []
            [x] -> return $ Lilypond.Note  (toLyNote x)                 (Just (realToFrac d)) []
            xs  -> return $ Lilypond.Chord (fmap ((,[]) . toLyNote) xs) (Just (realToFrac d)) []

        toLyNote :: Pitch -> Lilypond.Note
        toLyNote p = (`Lilypond.NotePitch` Nothing) $ Lilypond.Pitch (
          toEnum (fromEnum $ Music.Pitch.name p),
          -- FIXME catch if (abs accidental)>2 (or simply normalize)
          fromIntegral (Music.Pitch.accidental p),
          -- Lilypond expects SPN, so middle c is octave 4
          fromIntegral $ Music.Pitch.octaves
            (p.-.Music.Score.octavesDown (4+1) Music.Pitch.Literal.c)
          )

        notateDynamic :: DynamicNotation -> Lilypond.Music -> Lilypond.Music
        notateDynamic (DN.DynamicNotation (crescDims, level))
          = rcomposed (fmap notateCrescDim crescDims)
          . notateLevel level
          where
            notateCrescDim :: DN.CrescDim -> Lilypond.Music -> Lilypond.Music
            notateCrescDim crescDims = case crescDims of
              DN.NoCrescDim -> id
              DN.BeginCresc -> Lilypond.beginCresc
              DN.EndCresc   -> Lilypond.endCresc
              DN.BeginDim   -> Lilypond.beginDim
              DN.EndDim     -> Lilypond.endDim

            -- TODO these literals are not so nice...
            notateLevel :: Maybe Double -> Lilypond.Music -> Lilypond.Music
            notateLevel showLevel = case showLevel of
               Nothing -> id
               Just lvl -> Lilypond.addDynamics (fromDynamics (DynamicsL
                (Just (fixLevel . realToFrac $ lvl), Nothing)))

            fixLevel :: Double -> Double
            fixLevel x = fromIntegral (round (x - 0.5)) + 0.5

        notateArticulation :: ArticulationNotation -> Lilypond.Music -> Lilypond.Music
        notateArticulation (AN.ArticulationNotation (slurs, marks))
          = rcomposed (fmap notateMark marks)
          . rcomposed (fmap notateSlur slurs)
          where
            notateMark :: AN.Mark
                                  -> Lilypond.Music -> Lilypond.Music
            notateMark mark = case mark of
              AN.NoMark         -> id
              AN.Staccato       -> Lilypond.addStaccato
              AN.MoltoStaccato  -> Lilypond.addStaccatissimo
              AN.Marcato        -> Lilypond.addMarcato
              AN.Accent         -> Lilypond.addAccent
              AN.Tenuto         -> Lilypond.addTenuto

            notateSlur :: AN.Slur -> Lilypond.Music -> Lilypond.Music
            notateSlur slurs = case slurs of
              AN.NoSlur    -> id
              AN.BeginSlur -> Lilypond.beginSlur
              AN.EndSlur   -> Lilypond.endSlur

        -- TODO This syntax might change in future Lilypond versions
        -- TODO handle any color
        notateColor :: Maybe (Colour Double) -> Lilypond.Music -> Lilypond.Music
        notateColor Nothing      = id
        notateColor (Just color) = \x -> Lilypond.Sequential [
          Lilypond.Override "NoteHead#' color"
            (Lilypond.toLiteralValue $ "#" ++ colorName color),
          x,
          Lilypond.Revert "NoteHead#' color"
          ]

        colorName c
          | c == Color.black = "black"
          | c == Color.red   = "red"
          | c == Color.blue  = "blue"
          | otherwise        = error "Lilypond backend: Unkown color"

        -- Note: must use returned duration
        notateTremolo :: Maybe Int -> Duration -> (Lilypond.Music -> Lilypond.Music, Duration)
        notateTremolo Nothing d                        = (id, d)
        notateTremolo (Just 0) d = (id, d)
        notateTremolo (Just n) d = let
          scale   = 2^n
          newDur  = (d `min` (1/4)) / scale
          repeats = d / newDur
          in (Lilypond.Tremolo (round repeats), newDur)

        notateText :: [String] -> Lilypond.Music -> Lilypond.Music
        notateText texts = composed (fmap Lilypond.addText texts)

        notateHarmonic :: HarmonicNotation -> Lilypond.Music -> Lilypond.Music
        notateHarmonic (Any isNat, Sum n) = case (isNat, n) of
          (_,     0) -> id
          (True,  n) -> notateNatural n
          (False, n) -> notateArtificial n
          where
            notateNatural n = Lilypond.addFlageolet -- addOpen?
            notateArtificial n = id -- TODO

        notateGliss :: SlideNotation -> Lilypond.Music -> Lilypond.Music
        notateGliss ((Any eg, Any es),(Any bg, Any bs))
          | bg  = Lilypond.beginGlissando
          | bs  = Lilypond.beginGlissando
          | otherwise = id

        notateTies :: Ties -> Lilypond.Music -> Lilypond.Music
        notateTies (Any ta, Any tb)
          | ta && tb  = Lilypond.beginTie
          | tb        = Lilypond.beginTie
          | ta        = id
          | otherwise = id

        -- Use rcomposed as notateDynamic returns "mark" order, not application order
        composed = Music.Score.Internal.Util.composed
        rcomposed = Music.Score.Internal.Util.composed . reverse


    toLyStaffGroup :: LabelTree BracketType (Lilypond.Music) -> E Lilypond.Music
    toLyStaffGroup = return . foldLabelTree id g
      where
        -- Note: PianoStaff is handled in toLyStaffGroup
        -- Note: Nothing for name (we dump everything inside staves, so no need to identify them)
        g NoBracket  ms =                                     k ms
        g Bracket    ms = Lilypond.New "StaffGroup" Nothing $ k ms
        g Subbracket ms = Lilypond.New "GrandStaff" Nothing $ k ms
        g Brace      ms = Lilypond.New "GrandStaff" Nothing $ k ms
        -- Why False? No separation mark is necessary as the wrapped music is all in separate staves
        k = Lilypond.Simultaneous False

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
toXml :: Work -> MusicXml.Score
toXml = undefined


{-

finalizeScore :: XmlScore MusicXml.Music -> MusicXml.Score
finalizeScore (XmlScore (info, x))
  = MusicXml.fromParts title composer partList
  . map (finalizeStaff tempo) $ x
  where
    title    = scoreTitle info
    composer = scoreComposer info
    partList = scorePartList info
    tempo    = scoreTempo info

-- TODO finalizeStaffGroup

finalizeStaff :: Tempo -> XmlStaff MusicXml.Music -> [MusicXml.Music]
finalizeStaff tempo (XmlStaff (info, x))
  = id
  -- Staff name is not added here as MusicXML uses a separate part list
  . addStartInfo
  . addClef (staffClef info)
  . map finalizeBar $ x
  where
    -- TODO name
    -- TODO clef

    -- Both of these stick to the first bar
    -- TODO clean
    addClef :: (MusicXml.ClefSign, MusicXml.Line) -> [MusicXml.Music] -> [MusicXml.Music]
    addClef _                []     = []
    addClef (clefSign, line) (x:xs) = (MusicXml.clef clefSign line <> x):xs

    addStartInfo :: [MusicXml.Music] -> [MusicXml.Music]
    addStartInfo []     = []
    addStartInfo (x:xs) = (startInfo <> x):xs

    startInfo :: MusicXml.Music
    startInfo = mempty
        <> MusicXml.defaultDivisions
        <> MusicXml.defaultKey
        <> MusicXml.metronome (realToFrac nv) (realToFrac bpm)
        <> MusicXml.staves (staffCount info) -- TODO
        -- <> Xml.commonTime
    (nv, bpm) = getTempo tempo


finalizeBar :: XmlBar MusicXml.Music -> MusicXml.Music
finalizeBar (XmlBar (BarInfo timeSignature, x))
  = maybe id setBarTimeSignature timeSignature
  . renderBarMusic $ x
  where
    -- TODO key signatures
    -- TODO rehearsal marks
    -- TODO bar number change
    -- TODO compound time signatures
    setBarTimeSignature (getTimeSignature -> (ms, n)) x = mconcat
      [MusicXml.time (fromIntegral $ sum ms) (fromIntegral n), x]

renderBarMusic :: Rhythm MusicXml.Music -> MusicXml.Music
renderBarMusic = go
  where
    go (Beat d x)            = setDefaultVoice x
    go (Dotted n (Beat d x)) = setDefaultVoice x
    go (Group rs)            = mconcat $ map renderBarMusic rs
    go (Tuplet m r)          = MusicXml.tuplet b a (renderBarMusic r)
      where
        (a,b) = bimap fromIntegral fromIntegral $ unRatio $ realToFrac m

setDefaultVoice :: MusicXml.Music -> MusicXml.Music
setDefaultVoice = MusicXml.setVoice 1

{-
Sibelius quirks

Only looks at the part-name attribute (use part-name-display/display-text to override name)
Expects transposition first, i.e. "C Trumpet, Bb Clarinet"
Expects "French Horn", "Cello" and "English Horn"
Does not like numbers (I, II etc) after the instrument name
-}
toAcceptableName :: Show a => a -> (String, String)
toAcceptableName x = (overrideName defName, defName)
  where
    defName = show x
    overrideName y
      | "Clarinet"      `Data.List.isPrefixOf` y = "Bb Clarinet"
      | "Clarinet in A" `Data.List.isPrefixOf` y = "A Clarinet"
      | "Bassoon"       `Data.List.isPrefixOf` y = "Bassoon"
      | "Violoncello"   `Data.List.isPrefixOf` y = "Cello"
      | "Trumpet in Bb" `Data.List.isPrefixOf` y = "Bb Trumpet"
      | "Trumpet in C"  `Data.List.isPrefixOf` y = "C Trumpet"
      | "Trumpet in C"  `Data.List.isPrefixOf` y = "C Trumpet"
      | "Horn"          `Data.List.isPrefixOf` y = "French Horn"
      | otherwise                                = y




  exportScore b score = XmlScore
    . (ScoreInfo title composer partList tempo,)
    . map (uncurry $ exportPart timeSignatureMarks barDurations)
    . map (second (over dynamics notateDynamic))
    . map (second (preserveMeta addDynCon))
    . map (second (preserveMeta simultaneous))
    . extractPartsWithInfo
    . normalizeScore
    $ score
    where
      title    = fromMaybe "" $ flip getTitleAt 0              $ metaAtStart score
      composer = fromMaybe "" $ flip getAttribution "composer" $ metaAtStart score
      partList = MusicXml.partListDisplay (fmap toAcceptableName $ allParts score)
      tempo    = (metaAtStart score :: Tempo)
      (timeSignatureMarks, barDurations) = extractTimeSignatures score


      -- | Export a score as a single part. Overlapping notes will cause an error.
      exportPart :: (
        Tiable a,
        HasMusicXmlInstrument (Part a)
        )
        => [Maybe TimeSignature]
        -> [Duration]
        -> Part a
        -> Score a
        -> XmlStaff (XmlContext a)

      exportStaff :: Tiable a
        => [Maybe TimeSignature]
        -> [Duration]
        -> Int    -- Clef, as per Music.Parts
        -> Int    -- Number of staff lines
        -> MVoice a
        -> XmlStaff (XmlContext a)

      exportBar :: Tiable a
        => Maybe TimeSignature
        -> MVoice a
        -> XmlBar (XmlContext a)

      quantizeBar :: Tiable a
        => MVoice a
        -> Rhythm (XmlContext a)

      exportPart timeSignatureMarks barDurations part
        = exportStaff timeSignatureMarks barDurations (getMusicXmlClef part)
          (getMusicXmlNumberOfStaves part)
        . view oldSingleMVoice

      exportStaff timeSignatures barDurations clefId staffCount'
        = XmlStaff
        . addStaffInfo
        . zipWith exportBar timeSignatures
        . splitIntoBars barDurations
        where
          clef = case clefId of
            0 -> (MusicXml.GClef, 2)
            1 -> (MusicXml.CClef, 3)
            2 -> (MusicXml.FClef, 4)
          addStaffInfo  = (,) $ StaffInfo { staffClef = clef, staffCount = staffCount' }
          splitIntoBars = splitTiesAt

      exportBar timeSignature
        = XmlBar
        . addBarInfo
        . quantizeBar
       where
         addBarInfo = (,) $ BarInfo timeSignature

      quantizeBar = mapWithDur XmlContext . rewrite . handleErrors . quantize . view pairs
        where
          -- FIXME propagate quantization errors
          handleErrors (Left e)  = error $ "Quantization failed: " ++ e
          handleErrors (Right x) = x











-}

{-
  exportNote b = uncurry notate . fmap (exportNote b) . getCouple . getColorT . sequenceA
    where
      -- TODO This syntax will change in future MusicXml versions
      -- TODO handle any color
      notate (Option Nothing)             = id
      notate (Option (Just (Last color))) = \x -> MusicXml.Sequential [
        MusicXml.Override "NoteHead#' color"
          (MusicXml.toLiteralValue $ "#" ++ colorName color),
        x,
        MusicXml.Revert "NoteHead#' color"
        ]

      colorName c
        | c == Color.black = "black"
        | c == Color.red   = "red"
        | c == Color.blue  = "blue"
        | otherwise        = error "MusicXml backend: Unkown color"
-}

notateDynamic :: DN.DynamicNotation -> MusicXml.Music -> MusicXml.Music
notateDynamic (DN.DynamicNotation (crescDims, level))
  = Music.Score.Internal.Util.composed (fmap notateCrescDim crescDims)
  . notateLevel level
  where
      notateCrescDim crescDims = case crescDims of
        DN.NoCrescDim -> id
        DN.BeginCresc -> (<>) MusicXml.beginCresc
        DN.EndCresc   -> (<>) MusicXml.endCresc
        DN.BeginDim   -> (<>) MusicXml.beginDim
        DN.EndDim     -> (<>) MusicXml.endDim

      -- TODO these literals are not so nice...
      notateLevel showLevel = case showLevel of
         Nothing -> id
         Just lvl -> (<>) $ MusicXml.dynamic (fromDynamics (DynamicsL
          (Just (fixLevel . realToFrac $ lvl), Nothing)))

      fixLevel :: Double -> Double
      fixLevel x = fromIntegral (round (x - 0.5)) + 0.5

      -- DO NOT use rcomposed as notateDynamic returns "mark" order, not application order
      -- rcomposed = composed . reverse

notateArticulation :: AN.ArticulationNotation -> MusicXml.Music -> MusicXml.Music
notateArticulation (AN.ArticulationNotation (slurs, marks))
  = Music.Score.Internal.Util.composed (fmap notateMark marks)
  . Music.Score.Internal.Util.composed (fmap notateSlur slurs)
  where
      notateMark mark = case mark of
        AN.NoMark         -> id
        AN.Staccato       -> MusicXml.staccato
        AN.MoltoStaccato  -> MusicXml.staccatissimo
        AN.Marcato        -> MusicXml.strongAccent
        AN.Accent         -> MusicXml.accent
        AN.Tenuto         -> MusicXml.tenuto

      notateSlur slurs = case slurs of
        AN.NoSlur    -> id
        AN.BeginSlur -> MusicXml.beginSlur
        AN.EndSlur   -> MusicXml.endSlur

notateTremolo :: Integral a => a -> MusicXml.Music -> MusicXml.Music
notateTremolo n = case n of
  0 -> id
  n -> MusicXml.tremolo (fromIntegral n)

notateText :: [String] -> MusicXml.Music -> MusicXml.Music
notateText texts a = mconcat (fmap MusicXml.text texts) <> a


notateHarmonic :: HarmonicNotation -> MusicXml.Music -> MusicXml.Music
notateHarmonic (Any isNat, Sum n) = notate isNat n
  where
      notate _     0 = id
      notate True  n = notateNatural n
      notate False n = notateArtificial n

      -- notateNatural n = Xml.harmonic -- openString?
      notateNatural n = MusicXml.setNoteHead MusicXml.DiamondNoteHead
      -- Most programs do not recognize the harmonic tag
      -- We set a single diamond notehead instead, which can be manually replaced
      notateArtificial n = id -- TODO

notateSlide :: SlideNotation -> MusicXml.Music -> MusicXml.Music
notateSlide ((eg,es),(bg,bs)) = notate
  where
      notate = neg . nes . nbg . nbs
      neg    = if getAny eg then MusicXml.endGliss else id
      nes    = if getAny es then MusicXml.endSlide else id
      nbg    = if getAny bg then MusicXml.beginGliss else id
      nbs    = if getAny bs then MusicXml.beginSlide else id

notateTie :: Ties -> MusicXml.Music -> MusicXml.Music
notateTie (Any ta, Any tb)
  | ta && tb  = MusicXml.beginTie . MusicXml.endTie -- TODO flip order?
  | tb        = MusicXml.beginTie
  | ta        = MusicXml.endTie
  | otherwise = id

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

type Asp1 = (PartT Music.Parts.Part
  (ArticulationT Music.Articulation.Articulation
    (DynamicT Music.Dynamics.Dynamics
      Pitch)))

type Asp1B = (PartT Music.Parts.Part
  (ArticulationT AN.ArticulationNotation
    (DynamicT DN.DynamicNotation
      Pitch)))

-- We require all notes in a chords to have the same kind of ties
type Asp2 = TieT (PartT Music.Parts.Part
  (ArticulationT Music.Articulation.Articulation
    (DynamicT Music.Dynamics.Dynamics
      [Pitch])))
type Asp3 = TieT (PartT Music.Parts.Part
  (ArticulationT AN.ArticulationNotation
    (DynamicT DN.DynamicNotation
      [Pitch])))

type Asp = Score Asp1


fromAspects :: Asp -> E Work
fromAspects sc = do
   -- Part extraction
  let postPartExtract = Music.Score.extractPartsWithInfo normScore
  -- postPartExtract :: [(Music.Parts.Part,Score Asp1)]

  -- Change aspect type as we need Semigroup to compose all simultanous notes
  -- Merge simultanous notes into chords, to simplify voice-separation
  let postChordMerge = fmap2 (simultaneous . fmap asp1ToAsp2) postPartExtract
  -- postChordMerge :: [(Music.Parts.Part,Score Asp2)]

  -- Separate voices (called "layers" to avoid confusion)
  -- This is currently a trivial algorithm that assumes overlapping notes are in different parts
  postVoiceSeparation <- Data.Traversable.mapM (\a@(p,_) ->
    Data.Traversable.mapM (toLayer p) a) $ postChordMerge

  -- Rewrite dynamics and articulation to be context-sensitive
  -- This changes the aspect type again
  postContextSensitiveNotationRewrite <- return $ fmap2 asp2ToAsp3 $ postVoiceSeparation
  -- postContextSensitiveNotationRewrite :: [(Music.Parts.Part,Voice (Maybe Asp3))]

  -- Split each part into bars, splitting notes and adding ties when necessary
  -- Resulting list is list of bars, there is no layering (yet)
  let postTieSplit = fmap2 (Music.Score.splitTiesAt barDurations) $ postContextSensitiveNotationRewrite
  -- postTieSplit :: [(Music.Parts.Part,[Voice (Maybe Asp3)])]

  -- For each bar, quantize all layers. This is where tuplets/note values are generated.
  postQuantize <- Data.Traversable.mapM
    (Data.Traversable.mapM (Data.Traversable.mapM quantizeBar)) postTieSplit
  -- postQuantize :: [(Music.Parts.Part,[Rhythm (Maybe Asp3)])]

  -- TODO all steps above that start with fmap or mapM can be factored out (functor law)

  -- Group staves, generating brackets and braces
  let postStaffGrouping = generateStaffGrouping postQuantize
  -- postStaffGrouping :: LabelTree (BracketType) (Music.Parts.Part, [Rhythm (Maybe Asp3)])

  return $ Work mempty [Movement info systemStaff (fmap aspectsToStaff postStaffGrouping)]
  where
    fmap2 = fmap . fmap

    info = id
      $ movementTitle .~ (
        Data.Maybe.fromMaybe "" $ flip Music.Score.Meta.Title.getTitleAt 0 $
          Music.Score.Meta.metaAtStart sc
        )
      $ (movementAttribution.at "composer") .~ (
        flip Music.Score.Meta.Attribution.getAttribution "composer" $ Music.Score.Meta.metaAtStart sc
      ) $ mempty

    systemStaff :: SystemStaff
    systemStaff = fmap (\ts -> timeSignature .~ ts $ mempty) timeSignatureMarks

    (timeSignatureMarks, barDurations) = extractTimeSignatures normScore
    normScore = normalizeScore sc -- TODO not necessarliy set to 0...



    -- TODO log rewriting etc
    quantizeBar :: Music.Score.Tiable a => Voice (Maybe a) -> E (Rhythm (Maybe a))
    quantizeBar = fmap rewrite . quantize' . view Music.Score.pairs
      where
        quantize' x = case quantize x of
          Left e  -> throwError $ "Quantization failed: " ++ e
          Right x -> return x

    pureTieT :: a -> TieT a
    pureTieT = pure

    extractTimeSignatures
      :: Score a -> ([Maybe Music.Score.Meta.Time.TimeSignature], [Duration])
    extractTimeSignatures = Music.Score.Internal.Export.extractTimeSignatures

    generateStaffGrouping :: [(Music.Parts.Part, a)] -> LabelTree (BracketType) (Music.Parts.Part, a)
    generateStaffGrouping = groupToLabelTree . partDefault

    partDefault :: [(Music.Parts.Part, a)] -> Music.Parts.Group (Music.Parts.Part, a)
    partDefault xs = Music.Parts.groupDefault $ fmap (\(p,x) -> (p^.(Music.Parts._instrument),(p,x))) xs

    groupToLabelTree :: Group a -> LabelTree (BracketType) a
    groupToLabelTree (Single (_,a)) = Leaf a
    groupToLabelTree (Many gt _ xs) = (Branch (k gt) (fmap groupToLabelTree xs))
      where
        k Music.Parts.Bracket   = Bracket
        k Music.Parts.Invisible = NoBracket
        -- k Music.Parts.Subbracket = Just SubBracket
        k Music.Parts.PianoStaff = Brace
        k Music.Parts.GrandStaff = Brace


    asp1ToAsp2 :: Asp1 -> Asp2
    asp1ToAsp2 = pureTieT . (fmap.fmap.fmap) (:[])

    {-
    Note:
      Both addDynCon and addArtCon should *not* be used on scores for the time being, due to the faulty
      (HasPhrases Score) instance. See comment in Music.Score.Phrases.

      We use the MVoice instance here, so this is safe.
    -}
    asp2ToAsp3 :: Voice (Maybe Asp2) -> Voice (Maybe Asp3)
    asp2ToAsp3 = id
      . (DN.removeCloseDynMarks . over Music.Score.dynamics DN.notateDynamic
      . Music.Score.addDynCon) . (over Music.Score.articulations AN.notateArticulation
      . Music.Score.addArtCon)
      -- . fmap2 (over Music.Score.articulation (const ()))

    aspectsToChord :: Maybe Asp3 -> Chord
    aspectsToChord Nothing    = mempty
    aspectsToChord (Just asp) = id
      $ ties .~ (Any endTie,Any beginTie)
      $ dynamicNotation .~ (Just $ asp^.(Music.Score.dynamic))
      $ articulationNotation .~ (Just $ asp^.(Music.Score.articulation))
      $ pitches .~ (asp^..(Music.Score.pitches)) $ mempty
      where
        (endTie,beginTie) = Music.Score.isTieEndBeginning asp

    aspectsToBar :: Rhythm (Maybe Asp3) -> Bar
    aspectsToBar rh = Bar [layer1] -- TODO more layers (see below)
      where
        layer1 = fmap aspectsToChord rh

    aspectsToStaff :: (Music.Parts.Part, [Rhythm (Maybe Asp3)]) -> Staff
    aspectsToStaff (part,bars) = Staff info (fmap aspectsToBar bars)
      where
        info = id
          $ transposition  .~
            (part^.(Music.Parts._instrument).(to Music.Parts.transposition))
          $ instrumentDefaultClef  .~ Data.Maybe.fromMaybe (error "FIXME")
            (part^.(Music.Parts._instrument).(to Music.Parts.standardClef))
          $ instrumentShortName    .~
            Data.Maybe.fromMaybe "" (part^.(Music.Parts._instrument).(to Music.Parts.shortName))
          $ instrumentFullName     .~
            (Data.List.intercalate " " $ Data.Maybe.catMaybes [soloStr, nameStr, subpartStr])
          $ mempty
          where
            soloStr = if (part^.(Music.Parts._solo)) == Music.Parts.Solo then Just "Solo" else Nothing
            nameStr = (part^.(Music.Parts._instrument).(to Music.Parts.fullName))
            subpartStr = Just $ show (part^.(Music.Parts._subpart))

    toLayer :: Music.Parts.Part -> Score a -> E (MVoice a)
    toLayer p =
      maybe (throwError $ "Overlapping events in part: " ++ show p)
        return . preview Music.Score.singleMVoice


----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- Test

test = runENoLog $ toLy $
  Work mempty [Movement mempty [mempty] (
    Branch Bracket [
      Leaf (Staff mempty [Bar [Beat 1 mempty]]),
      Leaf (Staff mempty [Bar [Beat 1 mempty]])
      ])]

test2 x = runENoLog $ toLy =<< fromAspects x
test3 x = do
  let r = test2 x
  case r of
    Left e -> fail ("test3: "++e)
    Right (h,ly) -> do
      let ly2 = h ++ show (Pretty.pretty ly)
      -- putStrLn ly2
      writeFile "t.ly" $ ly2
      void $ System.Process.system "lilypond t.ly"
