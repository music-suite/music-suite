
{-# LANGUAGE TupleSections, DeriveDataTypeable, DeriveFoldable, ViewPatterns, DeriveFunctor, DeriveTraversable, TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Music.Score.Export2.StandardNotation where
import Control.Monad.Zip
import Data.Traversable (Traversable)
import qualified Data.Traversable
import Data.Foldable (Foldable)
import Music.Time
import Control.Applicative
import qualified Data.Maybe
import qualified Music.Score
import Data.Map (Map)
import Control.Lens.Operators
import Control.Lens (view, preview, set, over, under, _head)
import Control.Lens.TH (makeLenses)

import Data.Functor.Identity
import Control.Monad.Except
import Control.Monad.Writer

import Data.VectorSpace hiding (Sum)
import Data.AffineSpace hiding (Sum)
import           Data.Colour
import Music.Dynamics.Literal
import           Data.Colour.Names                       as Color
import qualified Music.Pitch
import qualified Music.Pitch.Literal
import           Data.Semigroup
import qualified Music.Score.Meta
import qualified Music.Score.Meta.Time
import qualified Music.Score.Meta.Key
import qualified Music.Score.Meta.Tempo
import qualified Music.Score.Meta.RehearsalMark
import Music.Score.Tremolo (TremoloT, runTremoloT)

import qualified Music.Score.Export.ArticulationNotation
import qualified Music.Score.Export.DynamicNotation
import qualified Music.Score.Export.ArticulationNotation as AN
import qualified Music.Score.Export.DynamicNotation as DN
import qualified Data.Music.Lilypond as Lilypond
import qualified Text.Pretty as Pretty
import qualified Data.Music.MusicXml.Simple as MusicXml
import qualified Music.Pitch
import qualified Music.Dynamics
import qualified Music.Articulation
import qualified Music.Parts
import qualified Music.Score.Internal.Util
import qualified Music.Score.Internal.Export
import Music.Score.Internal.Quantize (Rhythm(..))

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
data BracketType            = Bracket | Brace | Subbracket deriving (Eq, Ord, Show)
type SpecialBarline         = () -- TODO
-- type BarLines               = (Maybe SpecialBarline, Maybe SpecialBarline) -- (prev,next) biased to next
-- TODO lyrics
data SystemBar              = SystemBar {
        _barNumbers::Maybe BarNumber,
        _timeSignature::Maybe TimeSignature,
        _keySignature::Maybe KeySignature,
        _rehearsalMark::Maybe RehearsalMark,
        _tempoMark::Maybe TempoMark
        -- ,_barLines::BarLines -- Tricky because of ambiguity. Use balanced pair or an alt-list in SystemStaff.
        } deriving (Eq,Ord,Show)
instance Monoid SystemBar where
  mempty = SystemBar Nothing Nothing Nothing Nothing Nothing
type SystemStaff            = [SystemBar]
type InstrumentShortName    = String
type InstrumentFullName     = String
type Transposition          = Music.Pitch.Interval
type SibeliusFriendlyName   = String
type SmallOrLarge           = Any -- def False
type ScoreOrder             = Sum Double -- def 0

data StaffInfo              = StaffInfo {
  _instrumentShortName::InstrumentShortName,
  _instrumentFullName::InstrumentFullName,
  _sibeliusFriendlyName::SibeliusFriendlyName,
  _transposition::Transposition,
  _smallOrLarge::SmallOrLarge,
  _scoreOrder::ScoreOrder
  }
  deriving (Eq,Ord,Show)
instance Monoid StaffInfo where
  mempty = StaffInfo mempty mempty mempty mempty mempty mempty
type Pitch                  = Music.Pitch.Pitch
data ArpeggioNotation       = Arpeggio | UpArpeggio | DownArpeggio
  deriving (Eq,Ord,Show)
-- As written, i.e. 1/16-notes twice, can be represented as 1/8 note with 1 beams
-- TODO No way to represent 2-pitch tremolo
data TremoloNotation      = BeamedTremolo Int | UnmeasuredTremolo
  deriving (Eq,Ord,Show)
data BreathNotation         = Comma | Caesura | CaesuraWithFermata
  deriving (Eq,Ord,Show)
type ArticulationNotation   = Music.Score.Export.ArticulationNotation.ArticulationNotation
type DynamicNotation        = Music.Score.Export.DynamicNotation.DynamicNotation
type HarmonicNotation       = (Any, Sum Int)        -- (artificial?, partial number)
type SlideNotation          = ((Any,Any),(Any,Any)) -- (endGliss?,endSlide?),(beginGliss?,beginSlide?)
type Ties                   = (Any,Any)             -- (endTie?,beginTie?)
-- TODO appogiatura/acciatura
-- TODO beaming

-- Rests, single-notes and chords (most attributes are not shown for rests)
data Chord = Chord {
  _pitches::[Pitch], 
  _arpeggioNotation::Maybe ArpeggioNotation, 
  _tremoloNotation::Maybe TremoloNotation, 
  _breathNotation::Maybe BreathNotation, 
  _articulationNotation::Maybe ArticulationNotation, -- I'd like to put this in a separate layer, but neither Lily nor MusicXML thinks this way
  _dynamicNotation::Maybe DynamicNotation,
  _chordColor::Maybe (Colour Double),
  _chordText::[String],
  _harmonicNotation::HarmonicNotation,
  _slideNotation::SlideNotation,
  _ties::Ties
  }
  deriving (Eq, Show); makeLenses ''Chord
instance Monoid Chord where
  mempty = Chord [] Nothing Nothing Nothing Nothing Nothing mempty mempty mempty mempty mempty

type PitchLayer             = Rhythm Chord
-- type DynamicLayer           = Rhythm (Maybe DynamicNotation)

data Bar                    = Bar    {_pitchLayers::[PitchLayer] {-, _dynamicLayer::DynamicLayer-}}
  deriving (Eq, Show); makeLenses ''Bar

data Staff                  = Staff  {_staffInfo::StaffInfo,_bars::[Bar]}
  deriving (Eq, Show); makeLenses ''Staff

type Title                  = String
type Annotations            = [(Span, String)]
type Attribution            = Map String String -- composer, lyricist etc

data MovementInfo = MovementInfo {
  _movementTitle::Title,
  _movementAnnotations::Annotations,
  _movementAttribution::Attribution
  } 
  deriving (Eq, Show); makeLenses ''MovementInfo
instance Monoid MovementInfo where
  mempty = MovementInfo mempty mempty mempty

data Movement     = Movement { 
  _movementInfo::MovementInfo, 
  _systemStaff::SystemStaff, 
  _staves::LabelTree BracketType Staff  -- Don't allow names for staff groups, only staves
  }
  deriving (Eq, Show); makeLenses ''Movement

data WorkInfo     = WorkInfo { _title::Title, _annotations::Annotations, _attribution::Attribution} 
  deriving (Eq, Show); makeLenses ''WorkInfo
instance Monoid WorkInfo where
  mempty = WorkInfo mempty mempty mempty

data Work         = Work { _workInfo::WorkInfo, _movements::[Movement] }
  deriving (Show); makeLenses ''Work



-- type UpDown       = Up | Down
-- data CrossStaff   = NoCrossStaff | NextNoteCrossStaff UpDown | PreviousNoteCrossStaff UpDown
-- data ArpeggioNotation   = TODO
-- data TremoloNotation  = TODO
-- data BreathNotation = Fermata | PauseAfter | CaesuraAfter
-- type Bar          = [Voice ([Pitch], ArpeggioNotation, TremoloNotation, BreathNotation, ArticulationNotation, CrossStaff)], Voice DynamicNotation

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- Log and failure monad
newtype E a = E { runE :: WriterT String (ExceptT String Identity) a }
  deriving (Functor, Applicative, Monad, MonadError String, MonadWriter String)

runENoLog :: E b -> Either String b
runENoLog = fmap fst . runExcept . runWriterT . runE




toLy :: Work -> E (String, Lilypond.Music)
toLy w = do
  r <- case w^?movements._head of 
    Nothing -> throwError "StandardNotation: Expected a one-movement piece"
    Just x  -> return x
  m <- toLyMusic $ r
  return ("", m)
  -- TODO all top-level and template stuff
  -- TODO assume one movement

toLyMusic :: Movement -> E Lilypond.Music
toLyMusic m = do
  -- TODO ignore info
  -- We will copy system-staff info to each bar (time sigs, key sigs and so on, which seems to be what Lilypond expects),
  -- so the system staff is included in the rendering of each staff
  renderedStaves <- Data.Traversable.mapM (toLyStaff $ m^.systemStaff) (m^.staves)
  -- Now we still have (LabelTree BracketType), which is converted to a parallel music expression, using \StaffGroup etc
  toLyStaffGroup renderedStaves

toLyStaff :: SystemStaff -> Staff -> E Lilypond.Music
toLyStaff sysBars staff = id
  <$> Lilypond.New "Staff" Nothing
  <$> Lilypond.Sequential
  <$> (sequence $ zipWith toLyBar sysBars (staff^.bars))
  -- TODO ignoring staff info

toLyBar :: SystemBar -> Bar -> E Lilypond.Music
toLyBar sysBar bar = do
  -- TODO ignoring system bar
  let layers = bar^.pitchLayers
  error "No toLyBar"
  
  where
    go :: Rhythm a -> Lilypond.Music
    go (Beat d x)            = undefined
    go (Dotted n (Beat d x)) = undefined
    go (Dotted n _)          = error "FIXME"
    go (Group rs)            = scatL $ fmap go rs
    go (Tuplet m r)          = Lilypond.Times (realToFrac m) (go r)
      where
        (a,b) = bimap fromIntegral fromIntegral $ unRatio $ realToFrac m
    
    unRatio = Music.Score.Internal.Util.unRatio
    bimap = Music.Score.bimap

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
      fromIntegral $ Music.Pitch.octaves (p.-.Music.Score.octavesDown 4 Music.Pitch.Literal.c)
      )

    -- notateDynamic      :: Maybe DynamicNotation                -> Lilypond.Music -> Lilypond.Music
    -- notateArticulation :: Maybe ArticulationNotation           -> Lilypond.Music -> Lilypond.Music
    -- notateColor        :: Maybe (Colour Double)                -> Lilypond.Music -> Lilypond.Music
    -- notateTremolo      :: Maybe Int                -> Duration -> (Lilypond.Music -> Lilypond.Music, Duration)
    -- notateText         :: [String]                             -> Lilypond.Music -> Lilypond.Music
    -- notateHarmonic     :: (Any, Sum Int)                       -> Lilypond.Music -> Lilypond.Music
    -- notateGliss        :: ((Any, Any), (Any, Any))             -> Lilypond.Music -> Lilypond.Music
    --    (endGliss,endSlide),(beginGliss,beginSlide)
    -- notateTies          :: (Any, Any)                           -> Lilypond.Music -> Lilypond.Music
    --    (endTie,beginTie)

    notateDynamic :: DynamicNotation -> Lilypond.Music -> Lilypond.Music
    notateDynamic (DN.DynamicNotation (crescDims, level))
      = rcomposed (fmap notateCrescDim crescDims)
      . notateLevel level

    notateCrescDim crescDims = case crescDims of
      DN.NoCrescDim -> id
      DN.BeginCresc -> Lilypond.beginCresc
      DN.EndCresc   -> Lilypond.endCresc
      DN.BeginDim   -> Lilypond.beginDim
      DN.EndDim     -> Lilypond.endDim

    -- TODO these literals are not so nice...
    notateLevel showLevel = case showLevel of
       Nothing -> id
       Just lvl -> Lilypond.addDynamics (fromDynamics (DynamicsL (Just (fixLevel . realToFrac $ lvl), Nothing)))

    fixLevel :: Double -> Double
    fixLevel x = fromIntegral (round (x - 0.5)) + 0.5

    notateArticulation :: ArticulationNotation -> Lilypond.Music -> Lilypond.Music
    notateArticulation (AN.ArticulationNotation (slurs, marks))
      = rcomposed (fmap notateMark marks)
      . rcomposed (fmap notateSlur slurs)

    notateMark mark = case mark of
      AN.NoMark         -> id
      AN.Staccato       -> Lilypond.addStaccato
      AN.MoltoStaccato  -> Lilypond.addStaccatissimo
      AN.Marcato        -> Lilypond.addMarcato
      AN.Accent         -> Lilypond.addAccent
      AN.Tenuto         -> Lilypond.addTenuto

    notateSlur slurs = case slurs of
      AN.NoSlur    -> id
      AN.BeginSlur -> Lilypond.beginSlur
      AN.EndSlur   -> Lilypond.endSlur

    -- TODO This syntax will change in future Lilypond versions
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

    notateHarmonic :: (Any, Sum Int) -> Lilypond.Music -> Lilypond.Music
    notateHarmonic (Any isNat, Sum n) = case (isNat, n) of
      (_,     0) -> id
      (True,  n) -> notateNatural n
      (False, n) -> notateArtificial n
      where
        notateNatural n = Lilypond.addFlageolet -- addOpen?
        notateArtificial n = id -- TODO

    notateGliss :: ((Any, Any), (Any, Any)) -> Lilypond.Music -> Lilypond.Music
    notateGliss ((Any eg, Any es),(Any bg, Any bs))
      | bg  = Lilypond.beginGlissando
      | bs  = Lilypond.beginGlissando
      | otherwise = id

    notateTies :: (Any, Any) -> Lilypond.Music -> Lilypond.Music
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
    g Bracket    ms = Lilypond.New "StaffGroup" Nothing $ k ms
    g Subbracket ms = Lilypond.New "GrandStaff" Nothing $ k ms
    g Brace      ms = Lilypond.New "GrandStaff" Nothing $ k ms
    -- Why False? No separation mark is necessary as the wrapped music is all in separate staves
    k = Lilypond.Simultaneous False

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
toXml :: Work -> MusicXml.Score
toXml = undefined

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

fromAspects :: Score (Music.Parts.Part, Music.Articulation.Articulation, Music.Dynamics.Dynamics, Music.Pitch.Pitch) -> Work
fromAspects = undefined












-- Util
pcatL :: [Lilypond.Music] -> Lilypond.Music
pcatL = pcatL' False

pcatL' :: Bool -> [Lilypond.Music] -> Lilypond.Music
pcatL' p = foldr Lilypond.simultaneous (Lilypond.Simultaneous p [])

scatL :: [Lilypond.Music] -> Lilypond.Music
scatL = foldr Lilypond.sequential (Lilypond.Sequential [])

spellL :: Integer -> Lilypond.Note
spellL a = Lilypond.NotePitch (spellL' a) Nothing

spellL' :: Integer -> Lilypond.Pitch
spellL' p = Lilypond.Pitch (
  toEnum $ fromIntegral pc,
  fromIntegral alt,
  fromIntegral oct
  )
  where (pc,alt,oct) = Music.Score.Internal.Export.spellPitch (p + 72)

-- Test

test = runENoLog $ toLy $
  Work mempty [Movement mempty [mempty] (
    Branch Bracket [
      Leaf (Staff mempty [Bar [Beat 1 mempty]]),
      Leaf (Staff mempty [Bar [Beat 1 mempty]])
      ])] 
