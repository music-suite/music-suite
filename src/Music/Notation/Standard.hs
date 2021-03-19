{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC
  -fno-warn-unused-imports
  #-}

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
module Music.Notation.Standard
  (
    -- * Common music notation
    BarNumber,
    TimeSignature,
    KeySignature,
    RehearsalMark,
    TempoMark,
    BracketType (..),
    SpecialBarline,
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
    articulationNotation,
    movements,

    barLayersHaveEqualDuration,
  )
where

import BasePrelude hiding ((<>), First (..), first, second)
import qualified Codec.Midi as Midi
import Control.Lens
  ( Lens',
    _1,
    _2,
    _head,
    at,
    from,
    over,
    preview,
    to,
    view,
  )
import Control.Lens.Operators hiding ((|>))
import Control.Lens.TH (makeLenses)
import Control.Monad.Except
import Control.Monad.Plus
import Control.Monad.State
import Control.Monad.Log
import Control.Monad.Writer hiding ((<>), First (..))
import Data.AffineSpace
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8
import Data.Colour (Colour)
import Data.Colour.Names
import Data.FileEmbed
import Data.FiniteSeq (FiniteSeq)
import qualified Data.FiniteSeq
import Data.Functor.Couple
import qualified Data.List
import qualified Data.List.NonEmpty
import qualified Data.List.Split
import Data.Map (Map)
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Music.Lilypond as Lilypond
import qualified Data.Music.Lilypond as L
import qualified Data.Music.MusicXml.Simple as MusicXml
import qualified Data.Music.MusicXml.Simple as X
import Data.Semigroup
import qualified Data.Set
import Data.VectorSpace hiding (Sum)
import Music.Articulation (Articulation)
import Music.Dynamics (Dynamics)
import Music.Dynamics.Literal (DynamicsL (..), fromDynamics)
import Music.Parts (Instrument, Part, ScoreLayout (..))
import qualified Music.Parts
import qualified Music.Pitch
import Music.Pitch (IsPitch (..), Pitch, fromPitch)
import qualified Music.Pitch.Literal
import qualified Music.Pitch.Literal as P
import qualified Music.Score.Articulation
import Music.Score.Articulation (ArticulationT (..))
import Music.Score.Color (ColorT, runColorT)
import qualified Music.Score.Dynamics
import Music.Score.Dynamics (DynamicT (..))
import qualified Music.Score.Export.ArticulationNotation
import qualified Music.Score.Export.ArticulationNotation as AN
import qualified Music.Score.Export.DynamicNotation
import qualified Music.Score.Export.DynamicNotation as DN
import qualified Music.Score.Export.TechniqueNotation as TN
import Music.Score.Harmonics (HarmonicT, runHarmonicT)
import qualified Music.Score.Internal.Export
import Music.Score.Internal.Instances ()
import Music.Score.Internal.Quantize
  ( Rhythm (..),
    dotMod,
    quantize,
    rewrite,
  )
import qualified Music.Score.Internal.Util
import Music.Score.Internal.Util (unRatio)
import qualified Music.Score.Meta
import qualified Music.Score.Meta.Attribution
import qualified Music.Score.Meta.Key
import qualified Music.Score.Meta.RehearsalMark
import qualified Music.Score.Meta.Tempo
import Music.Score.Meta.Tempo (Tempo)
import qualified Music.Score.Meta.Time
import qualified Music.Score.Meta.Title
import qualified Music.Score.Part
import Music.Score.Part (PartT (..))
import qualified Music.Score.Phrases
import Music.Score.Phrases (MVoice)
import qualified Music.Score.Pitch
-- import Music.Score.Pitch ()
-- import Music.Score.Slide (SlideT, runSlideT)
-- import Music.Score.StaffNumber (StaffNumberT, runStaffNumberT)
-- import Music.Score.Technique (HasTechniques (techniques), Technique, TechniqueT (..))
-- import qualified Music.Score.Technique
-- import Music.Score.Text (TextT, runTextT)
-- import qualified Music.Score.Ties
-- import Music.Score.Ties (Tiable (..), TieT (..))
-- import Music.Score.Tremolo (TremoloT, runTremoloT)
import Music.Time
import Data.LabelTree (LabelTree(Branch, Leaf), fromListLT, foldLabelTree)

{-
Remember:

- Purpose here is to represent a standard/common/Western etc classical score.
- This type does /not/ have to be easy to enter manually.
- Keep it as simple as possible and CLEAR.
-

-}

-- TODO w/wo connecting barlines
-- TODO parg group names (i.e. "archi", "horns", "chorus", "fl (I && II)")
data BracketType = NoBracket | Bracket | Brace | Subbracket
  deriving (Eq, Ord, Show)

type BarNumber = Int

type TimeSignature = Music.Score.Meta.Time.TimeSignature

type KeySignature = Music.Score.Meta.Key.KeySignature

type RehearsalMark = Music.Score.Meta.RehearsalMark.RehearsalMark

type TempoMark = Music.Score.Meta.Tempo.Tempo

type SpecialBarline = () -- TODO Dashed | Double | Final
  -- type BarLines               = (Maybe SpecialBarline, Maybe SpecialBarline)
  -- (prev,next) biased to next

-- TODO lyrics

data SystemBar
  = SystemBar
      {-
      We treat all the following information as global.

      This is more restrictive than most classical notations, but greatly
      simplifies representation. In this view, a score is a matrix of bars
      (each belonging to a single staff). Each bar must have a single
      time sig/key sig/rehearsal mark/tempo mark.

      ----
      Note: Option First ~ Maybe

      Alternatively we could just make these things into Monoids such that
      mempty means "no notation at this point", and remove the "Option First"
      part here.

      Actually I'm pretty sure this is the right approach. See also #242
      -}
      { _barNumbers :: Maybe (First BarNumber),
        _timeSignature :: Maybe (First TimeSignature),
        _keySignature :: KeySignature,
        _rehearsalMark :: Maybe (First RehearsalMark),
        _tempoMark :: Maybe (First TempoMark)
        -- ,_barLines :: BarLines
        -- Tricky because of ambiguity. Use balanced pair
        -- or an alt-list in SystemStaff.
      }
  deriving (Eq, Ord, Show)

type SystemStaff = [SystemBar]

type InstrumentShortName = String

type InstrumentFullName = String

type Transposition = Music.Pitch.Interval

type SibeliusFriendlyName = String

type SmallOrLarge = Any -- def False

type ScoreOrder = Sum Double -- def 0

{-
Staff-related information (not system staff).

We do *not* allow mid-staff instrument changes, so all instrument-related
information goes here as well.

TODO add instrument part no. (I, II.1 etc)
-}
data StaffInfo
  = StaffInfo
      { _instrumentShortName :: InstrumentShortName,
        _instrumentFullName :: InstrumentFullName,
        _sibeliusFriendlyName :: SibeliusFriendlyName,
        {-
        See also clefChanges

        TODO change name of _instrumentDefaultClef
        More accurately, it represents the first clef to be used on the staff
        (and the only one if there are no changes.)

        OTOH having clef in the staff at all is redundant, specifying clef
        is optional (along with everything else) in this representation anyway.
        This is arguably wrong, as stanard notation generally requires a clef.
        -}
        _instrumentDefaultClef :: Music.Pitch.Clef,
        {-
        I.e. -P5 for horn

        Note that this representation indicates *written pitch*, not sounding (as does MusicXML),
        so this value is redundant when rendering a graphical score. OTOH if this representation
        is used to render *sound*, pitches need to be transposed acconrdingly.
        -}
        _transposition :: Transposition,
        _smallOrLarge :: SmallOrLarge,
        _scoreOrder :: ScoreOrder
      }
  deriving (Eq, Ord, Show)

type Title = String

type Annotations = [(Span, String)]

type Attribution = Map String String -- composer, lyricist etc

data MovementInfo
  = MovementInfo
      { _movementTitle :: Title,
        _movementAnnotations :: Annotations,
        _movementAttribution :: Attribution
      }
  deriving (Eq, Show)

data WorkInfo
  = WorkInfo
      { _title :: Title,
        _annotations :: Annotations,
        _attribution :: Attribution
      }
  deriving (Eq, Show)

data ArpeggioNotation
  = -- | Don't show anything
    NoArpeggio
  | -- | Show "no arpeggio" bracket
    NoArpeggioBracket
  | Arpeggio
  | UpArpeggio
  | DownArpeggio
  deriving (Eq, Ord, Show)

-- |
-- As written, i.e. 1/16-notes twice, can be represented as 1/8 note with 1 beams
--
-- These attach to chords
--
-- The distinction between MultiPitchTremolo and CrossBeamTremolo only really make sense
-- for string and keyboard instruments.
--
-- @Just n@ indicates n of extra beams/cross-beams.
-- For unmeasured tremolo, use @Nothing@.
data TremoloNotation
  = MultiPitchTremolo (Maybe Int)
  | CrossBeamTremolo (Maybe Int)
  | NoTremolo
  deriving (Eq, Ord, Show)

-- type UpDown       = Up | Down
-- data CrossStaff   = NoCrossStaff | NextNoteCrossStaff UpDown | PreviousNoteCrossStaff UpDown

-- Always apply *after* the indicated chord.
data BreathNotation = NoBreath | Comma | Caesura | CaesuraWithFermata
  deriving (Eq, Ord, Show)

type HarmonicNotation = (Any, Sum Int)

-- (artificial?, partial number)

type SlideNotation = ((Any, Any), (Any, Any))

-- (endGliss?,endSlide?),(beginGliss?,beginSlide?)

{-
For historical reasons we have two equivalent conventions for representing
begin/end of spanners:

  - (beginSlide::Any, endSlide::Any)
  - [EndCresc, BeginCresc]

These are isomorphic (at least as long as the list is considered as Set).
Both are monoids (assuming Any is being used and abscence in the set is taken
to mean "no begin/end"). Both also support the following type of access:

  endGliss :: Lens' SlideNotation Any

-}

endGliss :: Lens' SlideNotation Any
endGliss = _1 . _1

endSlide :: Lens' SlideNotation Any
endSlide = _1 . _2

beginGliss :: Lens' SlideNotation Any
beginGliss = _2 . _1

beginSlide :: Lens' SlideNotation Any
beginSlide = _2 . _2

type Ties = (Any, Any)

-- (endTie?,beginTie?)

-- TODO unify with Score.Meta.Fermata
data Fermata = NoFermata | Fermata | ShortFermata | LongFermata
  deriving (Eq, Ord, Show)

-- TODO appogiatura/acciatura
-- TODO beaming

-- Rests, single-notes and chords (most attributes are not shown for rests)

-- |
-- A chord. A possibly empty list of pitches, played simultaneously.
-- Naturally, an empty pitch list renders as a rest and a singleton list as
-- a single note.
--
-- In MusicXML our chords are rendered using <note> elements. The MusicXML convention
-- is to render each note or rest using a <note>. If the note is actually a rest,
-- it is tagged with a <rest/> element inside the note. If it is part of a chord,
-- each note except one (typically the first) is tagged with a <chord/> element.
--
-- It makes more sense if you think of the note element as representing a note/rest
-- symbol and an implicit advancement of the time counter, and he chord element as
-- an instruction to draw a note but skip advancing time (conversely backup/forward)
-- do affect the time counter but do not draw anything.
--
-- TODO arguably ties, colors, harmonics and slide/gliss should be represented
-- per notehead, and not per chord. This is simpler but may require more
-- voices/layers to represent certain music.
--
-- TODO Better text. Separate different text types (again based on graphical
-- presentation rather than semantics).
-- Note that MusicXML unfortunately makes no distinction between expressive marks
-- (i.e. dolce) versus instructions (i.e. pizz, sul pont).

-- TODO pedal notation
-- TODO 8va notations
-- TODO trills
-- TODO brackets/dashed lines?

data Chord
  = Chord
      { _pitches :: [Pitch],
        _arpeggioNotation :: ArpeggioNotation,
        _tremoloNotation :: TremoloNotation,
        _breathNotation :: BreathNotation,
        _articulationNotation :: AN.ArticulationNotation,
        -- I'd like to put dynamics in a separate layer, but neither Lily nor MusicXML thinks this way
        _dynamicNotation :: DN.DynamicNotation,
        _fermata :: Fermata,
        _chordColor :: Maybe (First (Colour Double)),
        _chordText :: [String],
        _harmonicNotation :: HarmonicNotation,
        _slideNotation :: SlideNotation,
        _ties :: Ties
      }
  deriving (Eq, Show)

-- |
-- A layer. A sequential composition of chords.
--
-- TODO we should replace 'Rhythm' with something similar that also allows
-- time-floating elements (i.e. grace notes).
--
-- Note that can /not/ use @Rhythm (Bool, Chord)@ (or similar) as that would scew up
-- the time semantics of the 'Rhythm' value, in other words: the floating nature
-- of these notes should be captured in the time type.
newtype PitchLayer = PitchLayer {getPitchLayer :: Rhythm Chord}
  deriving (Eq, Show)

-- |
-- A bar. A parallel composition of layers.
--
-- All layers that make up a bar should have the same duration.
-- An bar with no layers is allowed (this is 'mempty') and should be rendered
-- as a bar rest.
--
-- MusicXML is a part/measure matrix (typically using column-major order) to allow
-- it to be represented as a tree. Parts and measures are indexed by unique elements
-- (called id and number respectively). Neither of these are rendered in the score
-- (don't confuse with actual *visible* bar numbers), instead they are just token
-- identifiers, and are conventionally named P1,P2,P3... and 1,2,3... respectively.
--
--     <part id="P1">
--       <measure number="1">
--         ...
--       </measure>
--       <measure number="2">
--         ...
--       </measure>
--     </part>
--     ...
--
-- TODO free floating text/symbols?
data Bar
  = Bar
      { _clefChanges :: Map Duration Music.Pitch.Clef,
        _pitchLayers :: [PitchLayer]
        {-, _dynamicLayer :: DynamicLayer-}
      }
  deriving (Eq, Show)

-- |
-- A staff. A sequential composition of /bars/ with meta-information.
--
-- By this we mean an actual staff on the page, whether it correlates with a single
-- instrument or not (i.e. because it is part of a multi-staff part for i.e. piano,
-- organ or percussion).
--
-- The hireachy we use here is clear: a score contains staves which are organized
-- as a tree with bracket symbols at the branches. Traversing the tree gives us
-- the staves top-to-botom.
--
-- MusicXML by contrast includes two separate notions of parts and staves.
--
-- The score is a part/measure matrix, and the part-list element provides a
-- separate annotation for how parts are grouped (i.e. for brackets, barline convention
-- and in-between staff names). In MusicXML staves are not part of the hierarchy, but
-- are indicated separately: each part may specify a number of staves using
--
--   <attribute>
--     <staves>
--       N
--     </staves>
--   </attribute>
--
-- and certain elements (including notes note, forward and direction) [1] can
-- be tagget to indicate which staff they should be placed on.
--
-- Consequently things like a piano staff can be represented in two ways:
-- by using 2 (or more) explicist staves and a part-group, ur using a single part
-- and the staff annotations. The single-part approach seem to be favored for piano
-- music.
--
-- TODO we can also define multiple instrument parts using score-instrument,
-- and then assign individual notes (and which other objects?) to this.
--
-- [1]: http://www.musicxml.com/tutorial/notation-basics/multi-part-music-2/)
data Staff
  = Staff
      { _staffInfo :: StaffInfo,
        _bars :: [Bar]
      }
  deriving (Eq, Show)

-- |
-- A movement. A parallel composition of staves grouped by various types of brackets,
-- with meta-information.
--
-- Each staff (including the system-staff) should have the same number of bars
-- (TODO backends should truncate to enforce this).
--
-- The /system staff/ contains information that pertains to all simultaneous bars,
-- such as key and time signature changes (implying that this model does not allow
-- different time- or key singatures in different staves).
data Movement
  = Movement
      { _movementInfo :: MovementInfo,
        _systemStaff :: SystemStaff,
        -- Don't allow names for staff groups, only staves
        _staves :: LabelTree BracketType Staff
      }
  deriving (Eq, Show)

-- |
-- A musical work. A sequential composition of /movements/ with meta-information.
--
-- Called work to avoid confusion with 'Score'.
data Work
  = Work
      { _workInfo :: WorkInfo,
        _movements :: [Movement]
      }
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

instance Semigroup BracketType where
  x <> y
    | x == mempty = y
    | otherwise = x

instance Monoid BracketType where
  mempty = NoBracket

instance Semigroup SystemBar where
  (SystemBar a1 a2 a3 a4 a5) <> (SystemBar b1 b2 b3 b4 b5) =
    SystemBar (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)

instance Monoid SystemBar where
  mempty = SystemBar mempty mempty mempty mempty mempty

instance Semigroup StaffInfo where
  x <> y
    | x == mempty = y
    | otherwise = x

instance Monoid StaffInfo where
  mempty = StaffInfo mempty mempty mempty Music.Pitch.trebleClef mempty mempty mempty

instance Semigroup ArpeggioNotation where
  x <> y
    | x == mempty = y
    | otherwise = x

instance Monoid ArpeggioNotation where
  mempty = NoArpeggio

instance Semigroup TremoloNotation where
  x <> y
    | x == mempty = y
    | otherwise = x

instance Monoid TremoloNotation where
  mempty = NoTremolo

instance Semigroup BreathNotation where
  x <> y
    | x == mempty = y
    | otherwise = x

instance Monoid BreathNotation where
  mempty = NoBreath

instance Semigroup Fermata where
  x <> y
    | x == mempty = y
    | otherwise = x

instance Monoid Fermata where
  mempty = NoFermata

instance Semigroup Chord where
  x <> y
    | x == mempty = y
    | otherwise = x

instance Monoid Chord where
  mempty =
    Chord
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty

instance Semigroup Bar where
  (Bar a1 a2) <> (Bar b1 b2) = Bar (a1 <> b1) (a2 <> b2)

instance Monoid Bar where
  mempty = Bar mempty mempty

instance Semigroup Staff where
  (Staff a1 a2) <> (Staff b1 b2) = Staff (a1 <> b1) (a2 <> b2)

instance Monoid Staff where
  mempty = Staff mempty mempty

instance Semigroup MovementInfo where
  x <> y
    | x == mempty = y
    | otherwise = x

instance Monoid MovementInfo where
  mempty = MovementInfo mempty mempty mempty

instance Semigroup Work where
  (Work a1 a2) <> (Work b1 b2) = Work (a1 <> b1) (a2 <> b2)

instance Monoid Work where
  mempty = Work mempty mempty

instance Semigroup WorkInfo where
  x <> y
    | x == mempty = y
    | otherwise = x

instance Monoid WorkInfo where
  mempty = WorkInfo mempty mempty mempty

instance IsPitch Chord where
  fromPitch p = pitches .~ [p] $ mempty

systemStaffTakeBars :: Int -> SystemStaff -> SystemStaff
systemStaffTakeBars = take

systemStaffLength :: SystemStaff -> Int
systemStaffLength = length

staffTakeBars :: Int -> Staff -> Staff
staffTakeBars n (Staff i bs) = Staff i (take n bs)

staffLength :: Staff -> Int
staffLength (Staff _i bs) = length bs

-- |
-- If all layers have the same duration, return this duration wrapped in 'Right',
-- otherwise return a list of mismatching durations in 'Left'.
barLayersHaveEqualDuration :: Bar -> Either [Duration] Duration
barLayersHaveEqualDuration (Bar _ []) = Left []
barLayersHaveEqualDuration (Bar _ layers) =
  if allEqual durs
    then Right (head durs)
    else Left durs
  where
    allEqual [] = True
    allEqual (x : xs) = all (== x) xs
    durs = fmap layerDur layers
    layerDur (PitchLayer rh) = _duration rh

-- |
-- Pad all staves to the same number of bars.
--
-- All staves excepting the system staff must have a finite number of bars,
-- or this will diverge. The system staff is not taken into account when
-- calculating the number of bars (but it is padded if necessary).
movementAssureSameNumberOfBars :: Movement -> Movement
movementAssureSameNumberOfBars (Movement i ss st) =
  Movement i (addSystemBars n ss) (fmap (over bars (addBars n)) st)
  where
    emptySystemBar :: SystemBar = mempty
    emptyBar :: Bar = mempty
    addBars n = take n . (++ repeat emptyBar)
    addSystemBars n = take n . (++ repeat emptySystemBar)
    n = maximum $ 0 : numBars
    -- numSystemBars :: Int = length ss
    numBars :: [Int] = length . _bars <$> toList st


