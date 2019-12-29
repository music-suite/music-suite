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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- For MonadLog

-- For MonadLog String:

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
module Music.Score.Export2.StandardNotation
  ( LabelTree (..),
    foldLabelTree,

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
    ArticulationNotation,
    DynamicNotation,
    HarmonicNotation,
    SlideNotation,
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
    Bar,
    clefChanges,
    pitchLayers,
    Staff,
    staffInfo,
    bars,
    staffLength,
    staffTakeBars,
    Title,
    Annotations,
    Attribution,
    MovementInfo,
    movementTitle,
    movementAnnotations,
    movementAttribution,
    Movement,
    movementInfo,
    systemStaff,
    staves,
    movementAssureSameNumberOfBars,
    WorkInfo,
    title,
    annotations,
    attribution,
    Work,
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

    -- * Asp
    StandardNotationExportM,
    Asp1,
    Asp,
    fromAspects,

    -- * Backends
    LilypondExportM,
    toLy,
    MusicXmlExportM,
    toXml,
    MidiExportM,
    toMidi,

    -- * Test
    -- TODO hide/remove
    test2,
    exportLilypond,
    umts_export,
  )
where

import BasePrelude hiding ((<>), First (..), first, second)
import qualified Codec.Midi as Midi
import qualified Codec.Midi as M
import Control.Lens
  ( Lens' (..),
    _1,
    _2,
    _head,
    at,
    over,
    preview,
    set,
    to,
    under,
    view,
  )
import Control.Lens.Operators hiding ((|>))
import Control.Lens.TH (makeLenses)
import Control.Monad.Except
import Control.Monad.Plus
import Control.Monad.Writer hiding ((<>), First (..))
import Data.AffineSpace hiding (Sum)
import Data.Bifunctor (bimap, first, second)
import qualified Data.ByteString.Char8
import qualified Data.Char
import Data.Colour (Colour)
import Data.Colour.Names
import Data.FileEmbed
import Data.Functor.Identity (Identity (..))
import qualified Data.List
import qualified Data.List.Split
import Data.Map (Map)
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Music.Lilypond as Lilypond
import qualified Data.Music.Lilypond as L
import qualified Data.Music.MusicXml.Simple as MusicXml
import qualified Data.Music.MusicXml.Simple as X
import Data.Semigroup
import Data.VectorSpace hiding (Sum)
--DEBUG

import qualified Music.Articulation
import Music.Articulation (Articulation)
import qualified Music.Dynamics
import Music.Dynamics (Dynamics)
import Music.Dynamics.Literal (DynamicsL (..), fromDynamics)
import qualified Music.Dynamics.Literal as D
import Music.Parts (Group (..), Instrument, Part)
import qualified Music.Parts
import qualified Music.Pitch
import Music.Pitch (IsPitch (..), Pitch, fromPitch)
import qualified Music.Pitch.Literal
import qualified Music.Pitch.Literal as P
{-
TODO we need some instances from here, figure out which and purge this import
-}
import Music.Score ()
import qualified Music.Score.Articulation
import Music.Score.Articulation (ArticulationT (..))
import qualified Music.Score.Dynamics
import Music.Score.Dynamics (DynamicT (..))
import qualified Music.Score.Export.ArticulationNotation
import Music.Score.Export.ArticulationNotation (marks, slurs)
import qualified Music.Score.Export.ArticulationNotation as AN
import Music.Score.Export.DynamicNotation (crescDim, dynamicLevel)
import qualified Music.Score.Export.DynamicNotation
import qualified Music.Score.Export.DynamicNotation as DN
import qualified Music.Score.Internal.Export
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
import Music.Score.Phrases (MVoice (..))
import qualified Music.Score.Pitch
import Music.Score.Pitch ()
import qualified Music.Score.Ties
import Music.Score.Ties (Tiable (..))
import Music.Score.Ties (TieT (..))
import Music.Score.Tremolo (TremoloT, runTremoloT)
import Music.Time
import Music.Time.Meta (meta)
import qualified System.Directory
import qualified System.Process
import qualified Text.Pretty

-- Annotated tree
data LabelTree b a = Branch b [LabelTree b a] | Leaf a
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

fromListLT :: Monoid b => [a] -> LabelTree b a
fromListLT = Branch mempty . fmap Leaf

foldLabelTree :: (a -> c) -> (b -> [c] -> c) -> LabelTree b a -> c
foldLabelTree f g (Leaf x) = f x
foldLabelTree f g (Branch b xs) = g b (fmap (foldLabelTree f g) xs)

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
      { _barNumbers :: Option (First BarNumber),
        _timeSignature :: Option (First TimeSignature),
        _keySignature :: Option (First KeySignature),
        _rehearsalMark :: Option (First RehearsalMark),
        _tempoMark :: Option (First TempoMark)
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

type ArticulationNotation = Music.Score.Export.ArticulationNotation.ArticulationNotation

type DynamicNotation = Music.Score.Export.DynamicNotation.DynamicNotation

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
-- A chord. A possibly empty list of pitches, composed sequentially.
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
        _articulationNotation :: ArticulationNotation,
        -- I'd like to put dynamics in a separate layer, but neither Lily nor MusicXML thinks this way
        _dynamicNotation :: DynamicNotation,
        _fermata :: Fermata,
        _chordColor :: Option (First (Colour Double)),
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
-- TODO we can also define multiple instrumetns part part using score-instrument,
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

instance Monoid BracketType where

  mempty = NoBracket

  mappend x y
    | x == mempty = y
    | otherwise = x

instance Semigroup BracketType where
  (<>) = mappend

instance Semigroup SystemBar where
  (<>) = mappend

instance Monoid SystemBar where

  mempty = SystemBar mempty mempty mempty mempty mempty

  (SystemBar a1 a2 a3 a4 a5) `mappend` (SystemBar b1 b2 b3 b4 b5) =
    SystemBar (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)

instance Semigroup StaffInfo where
  (<>) = mempty

instance Monoid StaffInfo where

  mempty = StaffInfo mempty mempty mempty Music.Pitch.trebleClef mempty mempty mempty

  mappend x y
    | x == mempty = y
    | otherwise = x

instance Monoid ArpeggioNotation where

  mempty = NoArpeggio

  mappend x y
    | x == mempty = y
    | otherwise = x

instance Semigroup ArpeggioNotation where
  (<>) = mappend

instance Monoid TremoloNotation where

  mempty = NoTremolo

  mappend x y
    | x == mempty = y
    | otherwise = x

instance Semigroup TremoloNotation where
  (<>) = mappend

instance Monoid BreathNotation where

  mempty = NoBreath

  mappend x y
    | x == mempty = y
    | otherwise = x

instance Semigroup BreathNotation where
  (<>) = mappend

instance Semigroup Fermata where
  (<>) = mappend

instance Monoid Fermata where

  mempty = NoFermata

  mappend x y
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

  mappend x y
    | x == mempty = y
    | otherwise = x

instance Semigroup Chord where
  (<>) = mappend

instance IsPitch Chord where
  fromPitch p = pitches .~ [p] $ mempty

instance Monoid Bar where

  mempty = Bar mempty mempty

  mappend (Bar a1 a2) (Bar b1 b2) = Bar (a1 <> b1) (a2 <> b2)

instance Semigroup Bar where
  (<>) = mempty

instance Monoid Staff where

  mempty = Staff mempty mempty

  mappend (Staff a1 a2) (Staff b1 b2) = Staff (a1 <> b1) (a2 <> b2)

instance Semigroup Staff where
  (<>) = mappend

instance Monoid MovementInfo where

  mempty = MovementInfo mempty mempty mempty

  mappend x y
    | x == mempty = y
    | otherwise = x

instance Monoid Work where

  mempty = Work mempty mempty

  mappend (Work a1 a2) (Work b1 b2) = Work (a1 <> b1) (a2 <> b2)

instance Semigroup Work where
  (<>) = mempty

instance Semigroup WorkInfo where
  (<>) = mappend

instance Monoid WorkInfo where

  mempty = WorkInfo mempty mempty mempty

  mappend x y
    | x == mempty = y
    | otherwise = x

instance Semigroup MovementInfo where
  (<>) = mappend

systemStaffTakeBars :: Int -> SystemStaff -> SystemStaff
systemStaffTakeBars = take

systemStaffLength :: SystemStaff -> Int
systemStaffLength = length

staffTakeBars :: Int -> Staff -> Staff
staffTakeBars n (Staff i bs) = Staff i (take n bs)

staffLength :: Staff -> Int
staffLength (Staff i bs) = length bs

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
    layerDur (PitchLayer rh) = rh ^. duration

-- |
-- Check that all staves (including system-staff) has the same number of bars.
-- If not truncate to assure this.
movementAssureSameNumberOfBars :: Movement -> Movement
movementAssureSameNumberOfBars (Movement i ss st) = case Just minBars of
  Nothing -> Movement i ss st
  Just n -> Movement i (systemStaffTakeBars n ss) (fmap (staffTakeBars n) st)
  where
    -- TODO take all staves into account (and don't use Prelude.head)
    minBars = shortestListLength ss (_bars $ head $ toList st)
    shortestListLength :: [a] -> [b] -> Int
    shortestListLength xs ys = length (zip xs ys)

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- | A weaker form of 'MonadWriter' which also supports imperative logging.
class Monad m => MonadLog w m | m -> w where

  logger :: (a, w) -> m a
  logger ~(a, w) = do
    say w
    return a

  say :: w -> m ()
  say w = logger ((), w)

-- alter :: (w -> w) -> m a -> m a

-- instance MonadLog String IO where
--   logger (a, w) = do
--     putStrLn w
--     return a
-- alter f =

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
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadIO)

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
-- >>> expand "me : $(name)" (Map.fromList [("name","Hans")])
-- "me : Hans"
expandTemplate :: Template -> Map String String -> String
expandTemplate t vs = (composed $ fmap (expander vs) $ Data.Map.keys $ vs) t
  where
    expander vs k = replace ("$(" ++ k ++ ")") (Data.Maybe.fromJust $ Data.Map.lookup k vs)
    composed = foldr (.) id
    replace old new = Data.List.intercalate new . Data.List.Split.splitOn old
    toCamel [] = toCamel []
    toCamel (x : xs) = Data.Char.toUpper x : xs

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

type LilypondExportM m = (MonadLog String m, MonadError String m)

toLy :: (LilypondExportM m) => Work -> m (String, Lilypond.Music)
toLy work = do
  -- TODO assumes one movement
  say "Lilypond: Assuming one movement only"
  firstMovement <- case work ^? movements . _head of
    Nothing -> throwError "StandardNotation: Expected a one-movement piece"
    Just x -> return x
  let headerTempl =
        Data.Map.fromList
          [ ("title", (firstMovement ^. movementInfo . movementTitle)),
            ( "composer",
              Data.Maybe.fromMaybe "" $
                firstMovement ^. movementInfo . movementAttribution . at "composer"
            )
          ]
  let header = (Data.ByteString.Char8.unpack $(embedFile "data/ly_big_score.ily")) `expandTemplate` headerTempl
  say "Lilypond: Converting music"
  music <- toLyMusic $ firstMovement
  return (header, music)
  where
    toLyMusic :: (LilypondExportM m) => Movement -> m Lilypond.Music
    toLyMusic m2 = do
      let m = movementAssureSameNumberOfBars m2
      -- We will copy system-staff info to each bar (time sigs, key sigs and so on,
      -- which seems to be what Lilypond expects), so the system staff is included
      -- in the rendering of each staff
      renderedStaves <- mapM (toLyStaff $ m ^. systemStaff) (m ^. staves)
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
        <$> (sequence $ zipWith toLyBar sysBars (staff ^. bars))
    toLyClef c
      | c == Music.Pitch.trebleClef = Lilypond.Treble
      | c == Music.Pitch.altoClef = Lilypond.Alto
      | c == Music.Pitch.tenorClef = Lilypond.Tenor
      | c == Music.Pitch.bassClef = Lilypond.Bass
      | otherwise = Lilypond.Treble
    addClef c xs = Lilypond.Clef c : xs
    addPartName partName xs = longName : shortName : xs
      where
        longName = Lilypond.Set "Staff.instrumentName" (Lilypond.toValue partName)
        shortName = Lilypond.Set "Staff.shortInstrumentName" (Lilypond.toValue partName)
    toLyBar :: (LilypondExportM m) => SystemBar -> Bar -> m Lilypond.Music
    toLyBar sysBar bar = do
      let layers = bar ^. pitchLayers
      -- TODO emit \new Voice for eachlayer
      sim <$> sysStuff <$> mapM (toLyLayer . getPitchLayer) layers
      where
        -- System information need not be replicated in all layers
        -- TODO other system stuff (reh marks, special barlines etc)
        sysStuff [] = []
        sysStuff (x : xs) = (addTimeSignature (sysBar ^. timeSignature) x : xs)
        sim [x] = x
        sim xs = Lilypond.Simultaneous False xs
        addTimeSignature ::
          Option (First Music.Score.Meta.Time.TimeSignature) ->
          Lilypond.Music ->
          Lilypond.Music
        addTimeSignature timeSignature x = (setTimeSignature `ifJust` (unOF timeSignature)) x
          where
            unOF = fmap getFirst . getOption
            ifJust = maybe id
            setTimeSignature (Music.Score.Meta.Time.getTimeSignature -> (ms, n)) x =
              Lilypond.Sequential [Lilypond.Time (sum ms) n, x]
    toLyLayer :: (LilypondExportM m) => Rhythm Chord -> m Lilypond.Music
    toLyLayer (Beat d x) = toLyChord d x
    toLyLayer (Dotted n (Beat d x)) = toLyChord (dotMod n * d) x
    toLyLayer (Dotted n _) = error "FIXME"
    toLyLayer (Group rs) = Lilypond.Sequential <$> mapM toLyLayer rs
    toLyLayer (Tuplet m r) = Lilypond.Times (realToFrac m) <$> (toLyLayer r)
      where
        (a, b) = bimap fromIntegral fromIntegral $ unRatio $ realToFrac m
        unRatio = Music.Score.Internal.Util.unRatio
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
      where
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
        notateDynamicLy :: DynamicNotation -> Lilypond.Music -> Lilypond.Music
        notateDynamicLy (DN.DynamicNotation (crescDims, level)) =
          rcomposed (fmap notateCrescDim crescDims)
            . notateLevel level
          where
            notateCrescDim :: DN.CrescDim -> Lilypond.Music -> Lilypond.Music
            notateCrescDim crescDims = case crescDims of
              DN.NoCrescDim -> id
              DN.BeginCresc -> Lilypond.beginCresc
              DN.EndCresc -> Lilypond.endCresc
              DN.BeginDim -> Lilypond.beginDim
              DN.EndDim -> Lilypond.endDim
            -- TODO these literals are not so nice...
            notateLevel :: Maybe Double -> Lilypond.Music -> Lilypond.Music
            notateLevel showLevel = case showLevel of
              Nothing -> id
              Just lvl ->
                Lilypond.addDynamics
                  ( fromDynamics
                      ( DynamicsL
                          (Just (fixLevel . realToFrac $ lvl), Nothing)
                      )
                  )
            fixLevel :: Double -> Double
            fixLevel x = fromIntegral (round (x - 0.5)) + 0.5
        notateArticulationLy ::
          ArticulationNotation ->
          Lilypond.Music ->
          Lilypond.Music
        notateArticulationLy (AN.ArticulationNotation (slurs, marks)) =
          rcomposed (fmap notateMark marks)
            . rcomposed (fmap notateSlur slurs)
          where
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
        notateColor :: Option (First (Colour Double)) -> Lilypond.Music -> Lilypond.Music
        notateColor (Option Nothing) = id
        notateColor (Option (Just (First color))) = \x ->
          Lilypond.Sequential
            [ Lilypond.Override
                "NoteHead#' color"
                (Lilypond.toLiteralValue $ "#" ++ colorName color),
              x,
              Lilypond.Revert "NoteHead#' color"
            ]
        colorName c
          | c == Data.Colour.Names.black = "black"
          | c == Data.Colour.Names.red = "red"
          | c == Data.Colour.Names.blue = "blue"
          | otherwise = error "Lilypond backend: Unkown color"
        -- TODO not used for now
        -- We need to rescale the music according to the returned duration
        notateTremolo ::
          Maybe Int ->
          Duration ->
          (Lilypond.Music -> Lilypond.Music, Duration)
        notateTremolo Nothing d = (id, d)
        notateTremolo (Just 0) d = (id, d)
        notateTremolo (Just n) d =
          let scale = 2 ^ n
              newDur = (d `min` (1 / 4)) / scale
              repeats = d / newDur
           in (Lilypond.Tremolo (round repeats), newDur)
        notateText :: [String] -> Lilypond.Music -> Lilypond.Music
        notateText texts = composed (fmap Lilypond.addText texts)
        notateHarmonic :: HarmonicNotation -> Lilypond.Music -> Lilypond.Music
        notateHarmonic (Any isNat, Sum n) = case (isNat, n) of
          (_, 0) -> id
          (True, n) -> notateNatural n
          (False, n) -> notateArtificial n
          where
            notateNatural n = Lilypond.addFlageolet -- addOpen?
            notateArtificial n = id -- TODO
        notateGliss :: SlideNotation -> Lilypond.Music -> Lilypond.Music
        notateGliss ((Any eg, Any es), (Any bg, Any bs))
          | bg = Lilypond.beginGlissando
          | bs = Lilypond.beginGlissando
          | otherwise = id
        notateTies :: Ties -> Lilypond.Music -> Lilypond.Music
        notateTies (Any ta, Any tb)
          | ta && tb = Lilypond.beginTie
          | tb = Lilypond.beginTie
          | ta = id
          | otherwise = id
        -- Use rcomposed as notateDynamic returns "mark" order, not application order
        composed = Music.Score.Internal.Util.composed
        rcomposed = Music.Score.Internal.Util.composed . reverse
    toLyStaffGroup ::
      (LilypondExportM m) =>
      LabelTree BracketType (Lilypond.Music) ->
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
  where
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
            keySignatures_ = fmap (expTS . unOF) $ fmap (^. keySignature) (movement ^. systemStaff)
              where
                unOF = fmap getFirst . getOption
                -- TODO recognize common/cut
                expTS Nothing = (mempty :: MusicXml.Music)
                expTS (Just ks) =
                  let (fifths, mode) = Music.Score.Meta.Key.getKeySignature ks
                   in MusicXml.key (fromIntegral fifths) (if mode then MusicXml.Major else MusicXml.Minor)
            timeSignatures_ :: [MusicXml.Music]
            timeSignatures_ = fmap (expTS . unOF) $ fmap (^. timeSignature) (movement ^. systemStaff)
              where
                unOF = fmap getFirst . getOption
                -- TODO recognize common/cut
                expTS Nothing = (mempty :: MusicXml.Music)
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
        staffMusic = mapM renderStaff $ movement ^.. staves . traverse
          where
            renderClef :: Music.Pitch.Clef -> MusicXml.Music
            renderClef (Music.Pitch.Clef clef) = case clef of
              (clefSymbol, octCh, line) ->
                X.Music $ pure $ X.MusicAttributes $
                  X.Clef (exportSymbol clefSymbol) (fromIntegral line + 3)
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
              fromBars <- mapM renderBar (staff ^. bars)
              let clef = renderClef initClef
              pure $ mapHead ((transposeInfo <> clef) <>) fromBars
              where
                mapHead f [] = []
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
              say "    MusicXML: Generating bar <<>>"
              case barLayersHaveEqualDuration bar of
                -- No layers in this bar
                Left [] -> pure mempty
                -- Layers have different durations
                Left _ -> throwError "Layers have different durations"
                -- One or more layers with the same duration
                Right d -> do
                  let layers =
                        zipWith
                          (\voiceN music -> MusicXml.setVoice voiceN music)
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
                atPosition d x = (MusicXml.forward $ durToXmlDur d) <> x
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
                    (a, b) = bimap fromIntegral fromIntegral $ unRatio $ realToFrac m
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
                                (Just (fixLevel . realToFrac $ lvl), Nothing)
                            )
                        )
                fixLevel :: Double -> Double
                fixLevel x = fromIntegral (round (x - 0.5)) + 0.5
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
                notateNatural n = MusicXml.setNoteHead MusicXml.DiamondNoteHead
                -- Most programs do not recognize the harmonic tag
                -- We set a single diamond notehead instead, which can be manually replaced
                notateArtificial n = id -- TODO
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

--   getMidiChannel :: a -> Midi.Channel
--   getMidiProgram :: a -> Midi.Preset
--   getMidiChannel _ = 0

-- | Every note may give rise to a number of messages. We represent this as a score of messages.
type MidiEvent = Score Midi.Message

-- | The MIDI channel allocation is somewhat simplistic.
--   We use a dedicated channel and program number for each instrument (there *will* be colissions).
type MidiInstr = (Midi.Channel, Midi.Preset)

-- | A Midi file consist of a number of tracks.
--   Channel and preset info is passed on from exportScore to finalizeExport using this type.
--
--   TODO also pass meta-info etc.
data MidiScore a = MidiScore [(MidiInstr, Score a)]
  deriving (Functor)

type MidiExportM m = (MonadLog String m, MonadError String m)

toMidi :: (AbcNotationExportM m) => Asp -> m M.Midi
toMidi = pure . finalizeExport . fmap (exportNote) . exportScore
  where
    exportScore :: Score Asp1 -> MidiScore Asp1
    exportScore xs =
      MidiScore
        ( map (\(p, sc) -> ((getMidiChannel p, getMidiProgram p), sc))
            $ Music.Score.Part.extractPartsWithInfo
            $ fixTempo
            $ normalizeScore xs
        )
      where
        -- We actually want to extract *all* tempo changes and transform the score appropriately
        -- For the time being, we assume the whole score has the same tempo
        fixTempo = stretch (Music.Score.Meta.Tempo.tempoToDuration (Music.Score.Meta.metaAtStart xs))
        -- TODO
        getMidiProgram = const 0
        getMidiChannel = const 0
    finalizeExport :: MidiScore (Score Midi.Message) -> Midi.Midi
    finalizeExport (MidiScore trs) =
      let controlTrack = [(0, Midi.TempoChange 1000000), (endDelta, Midi.TrackEnd)]
          mainTracks = fmap (uncurry translMidiTrack . fmap join) trs
       in Midi.Midi fileType (Midi.TicksPerBeat divisions) (controlTrack : mainTracks)
      where
        translMidiTrack :: MidiInstr -> Score Midi.Message -> [(Int, Midi.Message)]
        translMidiTrack (ch, p) =
          addTrackEnd
            . setProgramChannel ch p
            . scoreToMidiTrack
        -- Each track needs TrackEnd
        -- We place it a long time after last event just in case (necessary?)
        addTrackEnd :: [(Int, Midi.Message)] -> [(Int, Midi.Message)]
        addTrackEnd = (<> [(endDelta, Midi.TrackEnd)])
        setProgramChannel :: Midi.Channel -> Midi.Preset -> Midi.Track Midi.Ticks -> Midi.Track Midi.Ticks
        setProgramChannel ch prg = ([(0, Midi.ProgramChange ch prg)] <>) . fmap (fmap $ setC ch)
        scoreToMidiTrack :: Score Midi.Message -> Midi.Track Midi.Ticks
        scoreToMidiTrack = fmap (\(t, _, x) -> (round ((t .-. 0) ^* divisions), x)) . toRelative . (^. triples)
        -- Hardcoded values for Midi export
        -- We always generate MultiTrack (type 1) files with division 1024
        fileType = Midi.MultiTrack
        divisions = 1024
        endDelta = 10000
        toRelative :: [(Time, Duration, b)] -> [(Time, Duration, b)]
        toRelative = snd . Data.List.mapAccumL g 0
          where
            g now (t, d, x) = (t, (0 .+^ (t .-. now), d, x))
    exportNote :: Asp1 -> Score Midi.Message
    exportNote (PartT (_, x)) = exportNoteA x
      where
        exportNoteA (ArticulationT (_, x)) = exportNoteD x
        exportNoteD (DynamicT (realToFrac -> d, x)) = setV (dynLevel d) <$> exportNoteP x
        exportNoteP pv = mkMidiNote (pitchToInt pv)
        pitchToInt p = fromIntegral $ Music.Pitch.semitones (p .-. P.c)
    dynLevel :: Double -> Midi.Velocity
    dynLevel x = round $ (\x -> x * 58.5 + 64) $ f $ inRange (-1, 1) (x / 3.5)
      where
        f = id
        -- f x = (x^3)
        inRange (m, n) x = (m `max` x) `min` n
    mkMidiNote :: Int -> Score Midi.Message
    mkMidiNote p =
      mempty
        |> pure (Midi.NoteOn 0 (fromIntegral $ p + 60) 64)
        |> pure (Midi.NoteOff 0 (fromIntegral $ p + 60) 64)
    setV :: Midi.Velocity -> Midi.Message -> Midi.Message
    setV v = go
      where
        go (Midi.NoteOff c k _) = Midi.NoteOff c k v
        go (Midi.NoteOn c k _) = Midi.NoteOn c k v
        go (Midi.KeyPressure c k _) = Midi.KeyPressure c k v
        go (Midi.ControlChange c n v) = Midi.ControlChange c n v
        -- go (Midi.ProgramChange c p)   = Midi.ProgramChange c p
        -- go (Midi.ChannelPressure c p) = Midi.ChannelPressure c p
        -- go (Midi.PitchWheel c w)      = Midi.PitchWheel c w
        -- go (Midi.ChannelPrefix c)     = Midi.ChannelPrefix c
        go x = x
    setC :: Midi.Channel -> Midi.Message -> Midi.Message
    setC c = go
      where
        go (Midi.NoteOff _ k v) = Midi.NoteOff c k v
        go (Midi.NoteOn _ k v) = Midi.NoteOn c k v
        go (Midi.KeyPressure _ k v) = Midi.KeyPressure c k v
        go (Midi.ControlChange _ n v) = Midi.ControlChange c n v
        go (Midi.ProgramChange _ p) = Midi.ProgramChange c p
        go (Midi.ChannelPressure _ p) = Midi.ChannelPressure c p
        go (Midi.PitchWheel _ w) = Midi.PitchWheel c w
        go (Midi.ChannelPrefix _) = Midi.ChannelPrefix c
        go x = x

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

type AbcNotationExportM m = (MonadLog String m, MonadError String m)

toAbc :: (AbcNotationExportM m) => Work -> m (String, ())
toAbc work = error "Not implemented"

{-
Basic implementation
-}

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

type FomusExportM m = (MonadLog String m, MonadError String m)

toFomus :: (FomusExportM m) => Work -> m (String, ())
toFomus work = error "Not implemented"

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

type Asp1 =
  ( PartT Part
      ( ArticulationT Articulation
          ( DynamicT Dynamics
              Pitch
          )
      )
  )

-- We require all notes in a chords to have the same kind of ties
type Asp2 =
  TieT
    ( PartT Part
        ( ArticulationT Articulation
            ( DynamicT Dynamics
                [Pitch]
            )
        )
    )

type Asp3 =
  TieT
    ( PartT Part
        ( ArticulationT AN.ArticulationNotation
            ( DynamicT DN.DynamicNotation
                [Pitch]
            )
        )
    )

type Asp = Score Asp1

type StandardNotationExportM m = (MonadLog String m, MonadError String m)

fromAspects :: (StandardNotationExportM m) => Asp -> m Work
fromAspects sc = do
  -- Part extraction
  say "Extracting parts"
  let postPartExtract = Music.Score.Part.extractPartsWithInfo normScore
  -- postPartExtract :: [(Music.Parts.Part,Score Asp1)]

  -- Change aspect type as we need Semigroup to compose all simultanous notes
  -- Merge simultanous notes into chords, to simplify voice-separation
  say "Merging overlapping notes into chords"
  let postChordMerge = fmap2 (simultaneous . fmap asp1ToAsp2) postPartExtract
  -- postChordMerge :: [(Music.Parts.Part,Score Asp2)]

  {-
    Separate voices (called "layers" to avoid confusion)
    This is currently a trivial algorithm that assumes overlapping notes are in different parts
  
    TODO layer sepration (which, again, does not actually happen in current code)
    should happen after ties have been split.
  -}
  say "Separating voices in parts (assuming no overlaps)"
  postVoiceSeparation <-
    mapM
      ( \a@(p, _) ->
          mapM (toLayer p) a
      )
      $ postChordMerge
  -- Rewrite dynamics and articulation to be context-sensitive
  -- This changes the aspect type again
  say "Notate dynamics and articulation"
  postContextSensitiveNotationRewrite <- return $ fmap2 asp2ToAsp3 $ postVoiceSeparation
  -- postContextSensitiveNotationRewrite :: [(Music.Parts.Part,Voice (Maybe Asp3))]

  -- Split each part into bars, splitting notes and adding ties when necessary
  -- Resulting list is list of bars, there is no layering (yet)
  say "Divide score into bars, adding ties where necessary"
  let postTieSplit = fmap2 (Music.Score.Ties.splitTiesAt barDurations) $ postContextSensitiveNotationRewrite
  -- postTieSplit :: [(Music.Parts.Part,[Voice (Maybe Asp3)])]

  -- For each bar, quantize all layers. This is where tuplets/note values are generated.
  say "Quantize rhythms (generating dotted notes and tuplets)"
  postQuantize <- mapM (mapM (mapM quantizeBar)) postTieSplit
  -- postQuantize :: [(Music.Parts.Part,[Rhythm (Maybe Asp3)])]

  -- TODO all steps above that start with fmap or mapM can be factored out (functor law)

  -- Group staves, generating brackets and braces
  say "Generate staff groups"
  let postStaffGrouping = generateStaffGrouping postQuantize
  -- postStaffGrouping :: LabelTree (BracketType) (Music.Parts.Part, [Rhythm (Maybe Asp3)])

  return $ Work mempty [Movement info systemStaff (fmap aspectsToStaff postStaffGrouping)]
  where
    fmap2 = fmap . fmap
    info =
      id
        $ movementTitle
          .~ ( Data.Maybe.fromMaybe "" $ flip Music.Score.Meta.Title.getTitleAt 0 $
                 Music.Score.Meta.metaAtStart sc
             )
        $ (movementAttribution . at "composer")
          .~ ( flip Music.Score.Meta.Attribution.getAttribution "composer" $ Music.Score.Meta.metaAtStart sc
             )
        $ mempty
    systemStaff :: SystemStaff
    systemStaff = fmap (\ts -> timeSignature .~ Option (fmap First ts) $ mempty) timeSignatureMarks
    (timeSignatureMarks, barDurations) = extractTimeSignatures normScore
    -- Make this more prominent!
    -- This is being used for the actual score!
    normScore = normalizeScore sc -- TODO not necessarliy set to 0...
    asp1ToAsp2 :: Asp1 -> Asp2
    asp1ToAsp2 = pure . (fmap . fmap . fmap) pure
    toLayer :: (StandardNotationExportM m) => Music.Parts.Part -> Score a -> m (MVoice a)
    toLayer p =
      maybe
        (throwError $ "Overlapping events in part: " ++ show p)
        return
        . preview Music.Score.Phrases.singleMVoice
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
    generateStaffGrouping :: [(Music.Parts.Part, a)] -> LabelTree (BracketType) (Music.Parts.Part, a)
    generateStaffGrouping = groupToLabelTree . partDefault
    aspectsToStaff :: (Music.Parts.Part, [Rhythm (Maybe Asp3)]) -> Staff
    aspectsToStaff (part, bars) = Staff info (fmap aspectsToBar bars)
      where
        info =
          id
            $ transposition
              .~ (part ^. (Music.Parts.instrument) . (to Music.Parts.transposition))
            $ instrumentDefaultClef
              .~ Data.Maybe.fromMaybe
                Music.Pitch.trebleClef
                (part ^. (Music.Parts.instrument) . (to Music.Parts.standardClef))
            $ instrumentShortName
              .~ Data.Maybe.fromMaybe "" (part ^. (Music.Parts.instrument) . (to Music.Parts.shortName))
            $ instrumentFullName
              .~ (Data.List.intercalate " " $ Data.Maybe.catMaybes [soloStr, nameStr, subpartStr])
            $ mempty
          where
            soloStr = if (part ^. (Music.Parts._solo)) == Music.Parts.Solo then Just "Solo" else Nothing
            nameStr = (part ^. (Music.Parts.instrument) . (to Music.Parts.fullName))
            subpartStr = Just $ show (part ^. (Music.Parts.subpart))
    extractTimeSignatures ::
      Score a -> ([Maybe Music.Score.Meta.Time.TimeSignature], [Duration])
    extractTimeSignatures = Music.Score.Internal.Export.extractTimeSignatures
    partDefault :: [(Music.Parts.Part, a)] -> Music.Parts.Group (Music.Parts.Part, a)
    partDefault xs = Music.Parts.groupDefault $ fmap (\(p, x) -> (p ^. (Music.Parts.instrument), (p, x))) xs
    groupToLabelTree :: Group a -> LabelTree (BracketType) a
    groupToLabelTree (Single (_, a)) = Leaf a
    groupToLabelTree (Many gt _ xs) = (Branch (k gt) (fmap groupToLabelTree xs))
      where
        k Music.Parts.Bracket = Bracket
        k Music.Parts.Invisible = NoBracket
        k Music.Parts.PianoStaff = Brace
        k Music.Parts.GrandStaff = Brace
        k _ = NoBracket
    aspectsToChord :: Maybe Asp3 -> Chord
    aspectsToChord Nothing = mempty
    aspectsToChord (Just asp) =
      id
        $ ties .~ (Any endTie, Any beginTie)
        $ dynamicNotation .~ (asp ^. (Music.Score.Dynamics.dynamic))
        $ articulationNotation .~ (asp ^. (Music.Score.Articulation.articulation))
        $ pitches .~ (asp ^.. (Music.Score.Pitch.pitches))
        $ mempty
      where
        (endTie, beginTie) = Music.Score.Ties.isTieEndBeginning asp
    aspectsToBar :: Rhythm (Maybe Asp3) -> Bar
    -- TODO handle >1 layers (see below)
    -- TODO place clef changes here
    aspectsToBar rh = Bar mempty [PitchLayer layer1]
      where
        layer1 = fmap aspectsToChord rh

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- Test

test =
  runPureExportMNoLog $ toLy $
    Work
      mempty
      [ Movement
          mempty
          [mempty]
          ( Branch
              Bracket
              [ Leaf (Staff mempty [Bar mempty [PitchLayer $ Beat 1 mempty]]),
                Leaf (Staff mempty [Bar mempty [PitchLayer $ Beat 1 mempty]])
              ]
          )
      ]

test2 x = runPureExportMNoLog $ toLy =<< fromAspects x

-- | Write t.ly and run Lilypond on it
exportLilypond :: Asp -> IO ()
exportLilypond x = do
  let r = test2 x
  case r of
    Left e -> fail ("test3: " ++ e)
    Right (h, ly) -> do
      let ly2 = h ++ show (Text.Pretty.pretty ly)
      -- putStrLn ly2
      writeFile "t.ly" $ ly2
      void $ System.Process.system "lilypond t.ly"

toMusicXml :: Asp -> Either String X.Score
toMusicXml x = runPureExportMNoLog $ toXml =<< fromAspects x

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
    sysStaff = (timeSignature .~ (Option $ Just $ First $ 2 / 4) $ mempty) : cycle [mempty]
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
    sysStaff = [timeSignature .~ (Option $ Just $ First (5 / 4)) $ mempty]
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
    sysStaff = map (\ts -> timeSignature .~ (Option $ fmap First ts) $ mempty) timeSigs2
    bars :: [Bar]
    bars = fmap (\d -> Bar mempty [PitchLayer $ quant (pitches .~ [] $ mempty) d]) durs
    quant :: a -> Duration -> Rhythm a
    quant x d = case d of
      d | d == 2 / 4 -> Beat (1 / 2) x
      d | d == 3 / 4 -> Dotted 1 $ Beat (1 / 2) x
      d | d == 4 / 4 -> Beat 1 x
      d | otherwise -> error "umts_02d: bad duration"
    timeSigs2 =
      concat $ zipWith (\n ts -> Just ts : replicate (n -1) Nothing) numBarRests timeSigs ::
        [Maybe TimeSignature]
    durs =
      concat $ zipWith (\n ts -> replicate n (realToFrac ts)) numBarRests timeSigs ::
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
      [ timeSignature .~ (Option $ Just $ First (16 / 4)) $ mempty,
        mempty,
        timeSignature .~ (Option $ Just $ First (24 / 4)) $ mempty,
        mempty,
        timeSignature .~ (Option $ Just $ First (28 / 4)) $ mempty,
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
    sysStaff = map (\ts -> timeSignature .~ (Option $ Just $ First ts) $ mempty) timeSigs
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
      d | d == (3 / 8) -> Dotted 1 $ Beat (1 / 4) x
      d | d == (1 / 2) -> Beat (1 / 2) x
      d | d == (3 / 4) -> Dotted 1 $ Beat (1 / 2) x
      d | d == (1) -> Beat 1 x
      d | d == (5 / 4) -> Group [Beat 1 (addTie True x), Beat (1 / 4) (addTie False x)] -- TODO ties
      d | d == (6 / 4) -> Dotted 1 $ Beat 1 x
      d | otherwise -> error "umts_11a: bad duration"
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
    sysStaff = (timeSignature .~ (Option $ Just $ First $ 4 / 4) $ mempty) : cycle [mempty]
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
    staff = Staff mempty $ repeat $ Bar mempty [PitchLayer $ Beat (1 / 2) $ pitches .~ [P.c] $ mempty]
    -- sysStaff = zipWith (\setTS ks -> setTS $ keySignature .~ (Option $ Just $ First $ ks) $ mempty)
    --   -- TODO just 2/4
    --   ( (timeSignature .~ (Option $ Just $ First (2/4))) : (timeSignature .~ (Option $ Just $ First (3/4))) : repeat mempty)
    --   keySigs

    -- TODO include TS too!
    sysStaff =
      fmap
        (\ks -> keySignature .~ (Option $ Just $ First $ ks) $ mempty)
        keySigs
    keySigs :: [KeySignature]
    keySigs = concatMap (\i -> fmap (\m -> Music.Score.Meta.Key.key i m) modesPerBar) fifthPerTwoBars
    fifthPerTwoBars = [-11 .. 11] :: [Music.Score.Meta.Key.Fifths]
    modesPerBar = [True, False]

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
    sysStaff = [timeSignature .~ (Option $ Just $ First (1 / 4)) $ mempty, mempty]
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
    sysStaff = [timeSignature .~ (Option $ Just $ First (1 / 4)) $ mempty, mempty]
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
    sysBar1 = timeSignature .~ (Option $ Just $ First $ 14 / 4) $ mempty
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
    sysBar1 = timeSignature .~ (Option $ Just $ First $ 5 / 4) $ mempty
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
    sysStaff = (timeSignature .~ (Option $ Just $ First $ 4 / 4) $ mempty) : repeat mempty
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
    sysStaff = (timeSignature .~ (Option $ Just $ First $ 2 / 4) $ mempty) : repeat mempty
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
    sysStaff = (timeSignature .~ (Option $ Just $ First $ 4 / 4) $ mempty) : repeat mempty
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
    sysStaff = (timeSignature .~ (Option $ Just $ First $ 3 / 4) $ mempty) : repeat mempty
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
    sysStaff = [keySignature .~ (Option $ Just $ First keySig) $ mempty]
    staves = zipWith (\name -> Staff (instrumentFullName .~ name $ mempty) . pure) names bars
    keySig = Music.Score.Meta.Key.key Music.Pitch.g True
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
    names = ["Part " ++ show n | n <- [1 .. 4]]
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
    sysStaff = [keySignature .~ (Option $ Just $ First keySig) $ mempty]
    staves = zipWith (\name -> Staff (instrumentFullName .~ name $ mempty) . pure) names bars
    keySig = Music.Score.Meta.Key.key Music.Pitch.g True
    bars =
      repeat $
        Bar
          mempty
          [ PitchLayer $
              Group
                [ Beat 1 (pitches .~ [] $ mempty)
                ]
          ]
    names = ["P" ++ show n | n <- [1 .. 19]]

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
    ls :: LabelTree BracketType String
    ls =
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
    ls :: LabelTree BracketType ()
    ls =
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
    longName = "Long Staff Name"
    shortName = "St. Nm."

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
    sysStaff = [timeSignature .~ (Option $ Just $ First $ 4 / 4) $ mempty]
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
    barLines =
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
    sysStaff = (timeSignature .~ (Option $ Just $ First $ 4 / 4) $ mempty) : cycle [mempty]
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
    pitches = [] -- TODO [c,d,e,f,g,a,b,c']
    origKeySig = (Music.Pitch.g, True) -- G major
    instruments :: [Instrument]
    instruments =
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
    sysStaff = [keySignature .~ (Option $ Just $ First origKeySig) $ mempty]
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
    origKeySig = Music.Score.Meta.Key.key Music.Pitch.g True
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
    timpNotes =
      [ [(1, P.e_, True)],
        [(1 / 2, P.e_, False), (1 / 2, P.a__, False)]
      ]
    cymbalNotes =
      [ [(3 / 4, P.c, False), (1 / 4, P.c, False)],
        [(1, P.c, False)]
      ]
    triangleNotes =
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
  let hash = "/tmp/music-suite/umts-hash"
  let dir = "/tmp/music-suite/umts"
  let refDir = "/tmp/music-suite/umts-ref"
  System.Directory.createDirectoryIfMissing True dir
  System.Directory.createDirectoryIfMissing True refDir
  -- TODO use hashes to make sure output does not change
  -- currentHash <- readFile hash

  -- Generate files, counting errors
  errorCount <- newIORef 0
  forM_ umts_all $ \(name, work) -> do
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

test :: Either String (String, L.Music)

test2 :: Asp -> Either String (String, L.Music)
