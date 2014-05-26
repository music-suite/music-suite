


{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Music.Score.Export.Lilypond2 (
    HasBackend(..),
    HasBackendScore(..),
    HasBackendNote(..),
    export,

    -- * Note lists
    NoteList,
    toNoteList,
    
    -- * Supercollider events
    Super,
    toSuper,

    -- * MIDI
    HasMidiProgram,
    Midi,
    toMidi,

    -- * Lilypond
    Lilypond,
    HasLilypondNEW,
    
    -- ** Converting to Lilypond
    toLilypond,
    toLilypondString,    
    
    -- ** Lilypond I/O
    showLilypond,
    openLilypond,
    writeLilypond,

    -- ** Customize Lilypond backend
    LilypondOptions(..),
    openLilypond',
    writeLilypond',
  ) where

import           Music.Dynamics.Literal
import           Music.Pitch.Literal
import           Music.Score                   hiding (HasMidiPart (..),
                                                HasMidiProgram (..), Inline,
                                                LilypondOptions, openLilypond,
                                                openLilypond', showLilypond,
                                                toLilypond, toLilypondString,
                                                toMidi, writeLilypond,
                                                writeLilypond', Lilypond)

import qualified Codec.Midi                    as Midi
import           Control.Arrow                 ((***))
import           Control.Comonad               (Comonad (..), extract)
import           Data.Colour.Names             as Color
import           Data.Default
import           Data.Foldable                 (Foldable)
import qualified Data.Foldable
import           Data.Functor.Couple
import           Data.Maybe
import           Data.Ratio
import           Data.Traversable              (Traversable, sequenceA)
import qualified Music.Lilypond                as Lilypond
import           Music.Score.Internal.Export   hiding (MVoice)
import           Music.Time.Internal.Transform (whilstLD)
import           Music.Score.Internal.Util     (composed, unRatio)
import           System.Process
import           Music.Time.Internal.Quantize
import qualified Text.Pretty                   as Pretty
import qualified Data.List
import Music.Score.Convert (reactiveToVoice') -- TODO
import Music.Score.Internal.Util (swap, retainUpdates)
import Music.Score.Export.DynamicNotation
import Data.Semigroup.Instances


-- |
-- This class defines types and functions for exporting music. It provides the
-- primitive types and methods used to implement 'export'.
--
-- The backend type @b@ is just a type level tag to identify a specific backend.
-- It is typically defined as an empty data declaration.
--
-- The actual conversion is handled by the subclasses 'HasBackendScore' and
-- 'HasBackendNote', which converts the time structure, and the contained music
-- respectively. Thus structure and content are handled separately.
--
-- It is often necessary to alter the events based on their surrounding context: for
-- examples the beginning and end of spanners and beams depend on surrounding notes.
-- The 'BackendContext' type allow 'HasBackendScore' instances to provide context for
-- 'HasBackendNote' instances.
--
-- @
-- data NoteList
--
-- instance HasBackend NoteList where
--   type BackendScore NoteList     = []
--   type BackendContext NoteList   = Identity
--   type BackendNote NoteList      = [(Sum Int, Int)]
--   type BackendMusic NoteList     = [(Sum Int, Int)]
--   finalizeExport _ = concat
--
-- instance HasBackendScore NoteList [a] a where
--   exportScore _ = fmap Identity
--
-- instance HasBackendNote NoteList a => HasBackendNote NoteList [a] where
--   exportNote b ps = mconcat $ map (exportNote b) $ sequenceA ps
--
-- instance HasBackendNote NoteList Int where
--   exportNote _ (Identity p) = [(mempty ,p)]
--
-- instance HasBackendNote NoteList a => HasBackendNote NoteList (DynamicT (Sum Int) a) where
--   exportNote b (Identity (DynamicT (d,ps))) = set (mapped._1) d $ exportNote b (Identity ps)
-- -- @
--
--
class Functor (BackendScore b) => HasBackend b where
  -- | External music representation
  type BackendMusic b :: *

  -- | Notes, chords and rests, with output handled by 'HasBackendNote'
  type BackendNote b :: *

  -- | Score, voice and time structure, with output handled by 'HasBackendScore'
  type BackendScore b :: * -> *

  -- | This type may be used to pass context from 'exportScore' to 'exportNote'.
  --   If the note export is not context-sensitive, 'Identity' can be used.
  type BackendContext b :: * -> *

  finalizeExport :: b -> BackendScore b (BackendNote b) -> BackendMusic b

class (HasBackend b) => HasBackendScore b s where
  type ScoreEvent b s :: *
  exportScore :: b -> s -> BackendScore b (BackendContext b (ScoreEvent b s))
  -- default exportScore :: (BackendContext b ~ Identity) => b -> s a -> BackendScore b (BackendContext b a)
  -- exportScore b = fmap Identity

class (HasBackend b) => HasBackendNote b a where
  exportNote  :: b -> BackendContext b a   -> BackendNote b
  exportChord :: b -> BackendContext b [a] -> BackendNote b
  exportChord = error "Not implemented: exportChord"

  -- exportNote' :: (BackendContext b ~ Identity) => b -> a -> BackendNote b
  -- exportNote' b x = exportNote b (Identity x)

export :: (HasBackendScore b s, HasBackendNote b (ScoreEvent b s)) => b -> s -> BackendMusic b
export b = finalizeExport b . export'
  where
    export' = fmap (exportNote b) . exportScore b






-- |
-- A simple backend that supports rendering scores to lists of pitch and velocity.
--
-- This exists as a sanity check for the 'Backend' classes, and as an example.
--
data NoteList

instance HasBackend NoteList where
  type BackendScore NoteList     = []
  type BackendContext NoteList   = Identity
  type BackendNote NoteList      = [(Sum Int, Int)]
  type BackendMusic NoteList     = [(Sum Int, Int)]
  finalizeExport _ = concat

instance HasBackendScore NoteList [a] where
  type ScoreEvent NoteList [a] = a
  exportScore _ = fmap Identity

instance HasBackendNote NoteList a => HasBackendNote NoteList [a] where
  exportNote b ps = mconcat $ map (exportNote b) $ sequenceA ps

instance HasBackendNote NoteList Int where
  exportNote _ (Identity p) = [(mempty ,p)]

instance HasBackendNote NoteList a => HasBackendNote NoteList (DynamicT (Sum Int) a) where
  exportNote b (Identity (DynamicT (d,ps))) = set (mapped._1) d $ exportNote b (Identity ps)

toNoteList :: (HasBackendNote NoteList (ScoreEvent NoteList s), HasBackendScore NoteList s) => s -> [(Int, Int)]
toNoteList = over (mapped._1) getSum . export (undefined::NoteList)






-- | Class of part types with an associated MIDI program number.
class HasMidiProgram a where
    getMidiChannel :: a -> Midi.Channel
    getMidiProgram :: a -> Midi.Preset
    getMidiChannel _ = 0

instance HasMidiProgram () where
    getMidiProgram _ = 0
instance HasMidiProgram Double where
    getMidiProgram = fromIntegral . floor
instance HasMidiProgram Float where
    getMidiProgram = fromIntegral . floor
instance HasMidiProgram Int where
    getMidiProgram = id
instance HasMidiProgram Integer where
    getMidiProgram = fromIntegral
instance (Integral a, HasMidiProgram a) => HasMidiProgram (Ratio a) where
    getMidiProgram = fromIntegral . floor


-- | A token to represent the Midi backend.
data Midi

-- | We do not need to pass any context to the note export.
type MidiContext = Identity

-- | Every note may give rise to a number of messages. We represent this as a score of messages.
type MidiEvent = Score Midi.Message

type MidiInstr = (Midi.Channel, Midi.Preset)

-- | A Midi file consist of a number of tracks.
--   Channel and preset info is passed on from exportScore to finalizeExport using this type.
data MidiScore a = MidiScore [(MidiInstr, Score a)]
  deriving Functor

instance HasBackend Midi where
  type BackendContext Midi    = MidiContext
  type BackendScore   Midi    = MidiScore
  type BackendNote    Midi    = MidiEvent
  type BackendMusic   Midi    = Midi.Midi

  finalizeExport _ (MidiScore trs) = let
    controlTrack  = [(0, Midi.TempoChange 1000000), (endDelta, Midi.TrackEnd)]
    mainTracks    = fmap (uncurry translMidiTrack . fmap join) trs
    in
    Midi.Midi fileType (Midi.TicksPerBeat divisions) (controlTrack : mainTracks)

    where
      translMidiTrack :: MidiInstr -> Score (Midi.Message) -> [(Int, Midi.Message)]
      translMidiTrack (ch, p) = id
        . addTrackEnd
        . setProgramChannel ch p
        . scoreToMidiTrack

      -- Each track needs TrackEnd
      -- We place it a long time after last event just in case (necessary?)
      addTrackEnd :: [(Int, Midi.Message)] -> [(Int, Midi.Message)]
      addTrackEnd = (<> [(endDelta, Midi.TrackEnd)])

      setProgramChannel :: Midi.Channel -> Midi.Preset -> Midi.Track Midi.Ticks -> Midi.Track Midi.Ticks
      setProgramChannel ch prg = ([(0, Midi.ProgramChange ch prg)] <>) . fmap (fmap $ setC ch)

      scoreToMidiTrack :: Score Midi.Message -> Midi.Track Midi.Ticks
      scoreToMidiTrack = fmap (\(t,_,x) -> (round ((t .-. 0) ^* divisions), x)) . toRelative . (^. events)

      -- Hardcoded values for Midi export
      -- We always generate MultiTrack (type 1) files with division 1024
      fileType    = Midi.MultiTrack
      divisions   = 1024
      endDelta    = 10000


instance (HasPart' a, HasMidiProgram (Part a)) => HasBackendScore Midi (Voice a) where
  type ScoreEvent Midi (Voice a) = a
  exportScore _ xs = MidiScore [((getMidiChannel (xs^?!parts), getMidiProgram (xs^?!parts)), fmap Identity $ voiceToScore xs)]
    where
      voiceToScore :: Voice a -> Score a
      voiceToScore = error "FIXME"

instance (HasPart' a, Ord (Part a), HasMidiProgram (Part a)) => HasBackendScore Midi (Score a) where
  type ScoreEvent Midi (Score a) = a
  exportScore _ xs = MidiScore (map (\(p,sc) -> ((getMidiChannel p, getMidiProgram p), fmap Identity sc)) $ extractParts' xs)

instance HasBackendNote Midi a => HasBackendNote Midi [a] where
  exportNote b ps = mconcat $ map (exportNote b) $ sequenceA ps

instance HasBackendNote Midi Int where
  exportNote _ (Identity pv) = mkMidiNote pv

instance HasBackendNote Midi a => HasBackendNote Midi (DynamicT (Sum Int) a) where
  exportNote b (Identity (DynamicT (Sum v, x))) = fmap (setV v) $ exportNote b (Identity x)

instance HasBackendNote Midi a => HasBackendNote Midi (ArticulationT b a) where
  exportNote b (Identity (ArticulationT (_, x))) = fmap id $ exportNote b (Identity x)

instance HasBackendNote Midi a => HasBackendNote Midi (PartT n a) where
  -- Part structure is handled by HasMidiBackendScore instances, so this is just an identity
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Midi a => HasBackendNote Midi (TremoloT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Midi a => HasBackendNote Midi (TextT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Midi a => HasBackendNote Midi (HarmonicT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Midi a => HasBackendNote Midi (SlideT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Midi a => HasBackendNote Midi (TieT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Midi a => HasBackendNote Midi (ColorT a) where
  exportNote b = exportNote b . fmap extract

mkMidiNote :: Int -> Score Midi.Message
mkMidiNote p = mempty
    |> pure (Midi.NoteOn 0 (fromIntegral $ p + 60) 64)
    |> pure (Midi.NoteOff 0 (fromIntegral $ p + 60) 64)

setV :: Midi.Velocity -> Midi.Message -> Midi.Message
setV v = go
  where
    go (Midi.NoteOff c k _)       = Midi.NoteOff c k v
    go (Midi.NoteOn c k _)        = Midi.NoteOn c k v
    go (Midi.KeyPressure c k _)   = Midi.KeyPressure c k v
    go (Midi.ControlChange c n v) = Midi.ControlChange c n v
    go (Midi.ProgramChange c p)   = Midi.ProgramChange c p
    go (Midi.ChannelPressure c p) = Midi.ChannelPressure c p
    go (Midi.PitchWheel c w)      = Midi.PitchWheel c w
    go (Midi.ChannelPrefix c)     = Midi.ChannelPrefix c

setC :: Midi.Channel -> Midi.Message -> Midi.Message
setC c = go
  where
    go (Midi.NoteOff _ k v)       = Midi.NoteOff c k v
    go (Midi.NoteOn _ k v)        = Midi.NoteOn c k v
    go (Midi.KeyPressure _ k v)   = Midi.KeyPressure c k v
    go (Midi.ControlChange _ n v) = Midi.ControlChange c n v
    go (Midi.ProgramChange _ p)   = Midi.ProgramChange c p
    go (Midi.ChannelPressure _ p) = Midi.ChannelPressure c p
    go (Midi.PitchWheel _ w)      = Midi.PitchWheel c w
    go (Midi.ChannelPrefix _)     = Midi.ChannelPrefix c

toMidi :: (HasBackendNote Midi (ScoreEvent Midi s), HasBackendScore Midi s) => s -> Midi.Midi
toMidi = export (undefined::Midi)














-- | A token to represent the Super backend.
data Super

-- | Pass duration to the note export.
type SuperContext = Identity--SuperContext Duration a deriving (Functor, Foldable, Traversable)

-- | Just \dur, \midinote, \db for now
type SuperEvent = (Double, Double)

-- | Score is just a list of parallel voices.
data SuperScore a = SuperScore [[(Duration, Maybe a)]]
  deriving (Functor)

instance Monoid (SuperScore a) where
  mempty = SuperScore mempty
  SuperScore a `mappend` SuperScore b = SuperScore (a `mappend` b)

instance HasBackend Super where
  type BackendContext Super    = SuperContext
  type BackendScore   Super    = SuperScore
  type BackendNote    Super    = SuperEvent
  type BackendMusic   Super    = String

  finalizeExport _ (SuperScore trs) = composeTracksInParallel $ map exportTrack trs
    where        
      composeTracksInParallel :: [String] -> String
      composeTracksInParallel = (\x -> "Ppar([" ++ x ++ "])") . Data.List.intercalate ", "
      
      exportTrack :: [(Duration, Maybe SuperEvent)] -> String
      exportTrack events = "Pbind("
        ++ "\\dur, Pseq(" ++ show durs ++ ")"
        ++ ", "
        ++ "\\midinote, Pseq(" ++ showRestList pitches ++ ")"
        ++ ")"
        where
          showRestList = id
            . (\x -> "[" ++ x ++ "]") 
            . Data.List.intercalate ", " 
            . map (maybe "\\rest" show) 
  
          -- events :: SuperEvent
          durs    :: [Double]
          pitches :: [Maybe Double]
          ampls   :: [Maybe Double]
          durs    = map (realToFrac . fst) events
          pitches = map (fmap fst . snd) events
          ampls   = map (fmap snd . snd) events
          

instance () => HasBackendScore Super (Voice (Maybe a)) where
  type ScoreEvent Super (Voice (Maybe a)) = a
  exportScore _ xs = fmap Identity $ SuperScore [view eventsV xs]

instance (HasPart' a, Ord (Part a)) => HasBackendScore Super (Score a) where
  type ScoreEvent Super (Score a) = a
  exportScore b = mconcat
    . map (exportScore b . view singleMVoice)
    . extractParts
  
instance HasBackendNote Super a => HasBackendNote Super [a] where
  exportNote b ps = head $ map (exportNote b) $ sequenceA ps

instance HasBackendNote Super Double where
  exportNote _ (Identity x) = (x + 60, 1)

instance HasBackendNote Super Int where
  exportNote _ (Identity x) = (fromIntegral x + 60, 1)

instance HasBackendNote Super Integer where
  exportNote _ (Identity x) = (fromIntegral x + 60, 1)

instance HasBackendNote Super a => HasBackendNote Super (DynamicT b a) where
  exportNote b = exportNote b . fmap extract
  -- exportNote b (Identity (DynamicT (Sum v, x))) = fmap (setV v) $ exportNote b (Identity x)

instance HasBackendNote Super a => HasBackendNote Super (ArticulationT b a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Super a => HasBackendNote Super (PartT n a) where
  -- Part structure is handled by HasSuperBackendScore instances, so this is just an identity
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Super a => HasBackendNote Super (TremoloT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Super a => HasBackendNote Super (TextT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Super a => HasBackendNote Super (HarmonicT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Super a => HasBackendNote Super (SlideT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Super a => HasBackendNote Super (TieT a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Super a => HasBackendNote Super (ColorT a) where
  exportNote b = exportNote b . fmap extract


toSuper :: (HasBackendNote Super (ScoreEvent Super s), HasBackendScore Super s) => s -> String
toSuper = export (undefined::Super)

openSuper score = do
  writeFile "test.sc" ("(" ++ toSuper score ++ ").play")















{-
  TODO rewrite so that:
    - Each bar may include voices/layers (a la Sibelius)
    - Each part may generate more than one staff (for piano etc)
-}


-- | A token to represent the Lilypond backend.
data Lilypond

-- | Hierachical representation of a Lilypond score.
--   A score is a parallel composition of staves.
data LyScore a = LyScore { getLyScore :: [LyStaff a] }
  deriving (Functor, Eq, Show)

type StaffInfo = ()

-- | A staff is a sequential composition of bars.
data LyStaff a = LyStaff { getLyStaff :: [(StaffInfo, LyBar a)] }
  deriving (Functor, Eq, Show)

type BarInfo = Maybe TimeSignature

-- | A bar is a sequential composition of chords/notes/rests.
data LyBar a = LyBar { getLyBar :: (BarInfo, Rhythm a) } 
  deriving (Functor, Eq, Show)

-- | Context passed to the note export.
--   Includes duration and note/rest distinction.
data LyContext a = LyContext Duration (Maybe a) deriving (Functor, Foldable, Traversable, Eq, Show)
instance Monoid Lilypond.Music where
  mempty = pcatLy []
  mappend x y = pcatLy [x,y]

type LyMusic = Lilypond.Music

instance HasBackend Lilypond where
  type BackendScore Lilypond   = LyScore
  type BackendContext Lilypond = LyContext
  type BackendNote Lilypond    = LyMusic
  type BackendMusic Lilypond   = LyMusic
  finalizeExport _ = finalizeScore

finalizeScore :: LyScore LyMusic -> Lilypond.Music
finalizeScore = extra . pcatLy . map finalizeStaff . getLyScore
  where
    extra = id

finalizeStaff :: LyStaff LyMusic -> LyMusic
finalizeStaff = extra . scatLy . map finalizeBar . map snd . getLyStaff
  where
    extra = addStaff . addPartName "Foo" . addClef ()
  -- TODO correct staff names
  -- TODO correct clefs

finalizeBar :: LyBar LyMusic -> LyMusic
finalizeBar (LyBar (timeSignature, music)) = setBarTimeSignature timeSignature $ renderBarRhythm $ music


{-
  getL (setL a s)   = a
  setL (getL s) s   = s
  setL a (setL b s) = setL a s
-}
type HasDynamicX s t a b = (

  -- with ImpredicativeTypes:
  -- forall a .
   Dynamic (SetDynamic a s) ~ a,

  -- SetDynamic (Dynamic t) s ~ t, -- difference?  
  SetDynamic (Dynamic t) s ~ t,

  -- with ImpredicativeTypes:
  -- forall a b .
  SetDynamic a (SetDynamic b s) ~ SetDynamic a s,
  
  () ~ ()
  )

-- TODO simplify
instance (
  ad' ~ (Maybe (Dynamic a), Dynamic a, Maybe (Dynamic a)),
  ad'' ~ DynamicNotation,
  Dynamic a'  ~ ad',
  Dynamic a'' ~ ad'',

  -- HasDynamicX s ad' ad'',

  -- We can read update the dynamics in a
  HasDynamic' a,
  -- We can add context to the dynamics in a
  HasDynamic a a',
  -- We can can replace the contextual dynamics with a notation
  -- We know from superclasses that a'' is a' with ad'' as dynamics
  --  i.e. that   
  HasDynamic a' a'',

{-
  -- GetSet a X ad' X
  Dynamic (SetDynamic ad' a) ~ ad',

  -- SetGet a a' X X
  SetDynamic (Dynamic a') a ~ a',

  -- SetSet a X ad' ad''
  SetDynamic ad'' (SetDynamic ad' a) ~ SetDynamic ad'' a,
-}

  HasDynamicX a a'' ad' ad'',

  Real (Dynamic a),
  
  HasPart' a, Ord (Part a),
  Transformable a,
  Semigroup a,

  HasPart' a'', Ord (Part a''),
  Transformable a',
  Semigroup a',
  
  Tiable a'',
  
  () ~ ()
  )
  => HasBackendScore Lilypond (Score a) where
  type ScoreEvent Lilypond (Score a) = SetDynamic DynamicNotation a
  exportScore b score = LyScore
    . map (uncurry $ exportPart timeSignatureMarks barDurations)
    . extractParts'
    . over dynamics notateDynamic 
    . preserveMeta addDynCon 
    . preserveMeta simultaneous 
    $ score
    where
      (timeSignatureMarks, barDurations) = extractTimeSignatures score 

-- | Export a score as a single part. Overlapping notes will cause an error.
exportPart :: Tiable a 
  => [Maybe TimeSignature] 
  -> [Duration] 
  -> Part a 
  -> Score a 
  -> LyStaff (LyContext a)
exportPart timeSignatureMarks barDurations part = id
  . exportStaff timeSignatureMarks barDurations
  . view singleMVoice


exportStaff :: Tiable a 
  => [Maybe TimeSignature] 
  -> [Duration] 
  -> MVoice a 
  -> LyStaff (LyContext a)
exportStaff timeSignatures barDurations 
  = LyStaff 
  . map addStaffInfo
  . zipWith exportBar timeSignatures 
  . splitIntoBars barDurations
  where         
    addStaffInfo  = ((),)
    splitIntoBars = splitTiesVoiceAt
    -- TODO rename splitTiesVoiceAt?

exportBar :: Tiable a 
  => Maybe TimeSignature 
  -> MVoice a 
  -> LyBar (LyContext a)
exportBar timeSignature music = LyBar (timeSignature, quantizeBar music)


--------------------------------------------------------------------------------
-- Called in the exportScore step
--------------------------------------------------------------------------------

extractTimeSignatures :: Score a -> ([Maybe TimeSignature], [Duration])
extractTimeSignatures score = (barTimeSignatures, barDurations)
  where                                          
    defaultTimeSignature = time 4 4
    timeSignatures = fmap swap 
      $ unvoice $ fuse 
      $ reactiveToVoice' (0 <-> (score^.offset)) 
      $ getTimeSignatures defaultTimeSignature score

    -- Despite the fuse above we need retainUpdates here to prevent redundant repetition of time signatures
    barTimeSignatures  = retainUpdates $ getBarTimeSignatures $ timeSignatures
    barDurations       = getBarDurations $ timeSignatures

quantizeBar :: Tiable a => MVoice a -> Rhythm (LyContext a)
quantizeBar = mapWithDur (\d x -> LyContext d x) . rewrite . fromRight . quantize . view unsafeEventsV
  where
    -- FIXME handle quantization errors
    fromRight (Left e)  = error $ "Quantization failed: " ++ e
    fromRight (Right x) = x

    mapWithDur :: (Duration -> a -> b) -> Rhythm a -> Rhythm b
    mapWithDur f = go
      where
        go (Beat d x)            = Beat d (f d x)
        go (Dotted n (Beat d x)) = Dotted n $ Beat d (f (dotMod n * d) x)
        go (Group rs)            = Group $ fmap (mapWithDur f) rs
        go (Tuplet m r)          = Tuplet m (mapWithDur f r)



--------------------------------------------------------------------------------
-- Called in the finalize step
--------------------------------------------------------------------------------

renderBarRhythm :: Rhythm LyMusic -> LyMusic
renderBarRhythm = go
  where
    go (Beat d x)            = noteRestToLilypond x
    go (Dotted n (Beat d x)) = noteRestToLilypond x
    go (Group rs)            = scatLy $ map renderBarRhythm rs
    go (Tuplet m r)          = Lilypond.Times (realToFrac m) (renderBarRhythm r)
      where
        (a,b) = fromIntegral *** fromIntegral $ unRatio $ realToFrac m
-- where
noteRestToLilypond = Lilypond.removeSingleChords


addStaff :: LyMusic -> LyMusic
addStaff = Lilypond.New "Staff" Nothing

addPartName :: String -> LyMusic -> LyMusic
addPartName partName xs = pcatLy [longName, shortName, xs]
  where
    longName  = Lilypond.Set "Staff.instrumentName" (Lilypond.toValue $ partName)
    shortName = Lilypond.Set "Staff.shortInstrumentName" (Lilypond.toValue $ partName)

addClef :: () -> LyMusic -> LyMusic
addClef () x = pcatLy [Lilypond.Clef Lilypond.Baritone, x]


setBarTimeSignature :: Maybe TimeSignature -> LyMusic -> LyMusic
setBarTimeSignature Nothing x = x
setBarTimeSignature (Just (getTimeSignature -> (ms, n))) x = scatLy [Lilypond.Time (sum ms) n, x]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

instance HasBackendNote Lilypond a => HasBackendNote Lilypond [a] where
  exportNote b = exportChord b

instance HasBackendNote Lilypond Integer where
  -- TODO can we get rid of exportChord alltogether and just use LyContext?
  exportNote  _ (LyContext d Nothing)    = (^*realToFrac (4*d)) $ Lilypond.rest
  exportNote  _ (LyContext d (Just x))   = (^*realToFrac (4*d)) $ Lilypond.note $ spellLy $ x
  exportChord _ (LyContext d Nothing)    = (^*realToFrac (4*d)) $ Lilypond.rest
  exportChord _ (LyContext d (Just xs))  = (^*realToFrac (4*d)) $ Lilypond.chord $ fmap spellLy $ xs

instance HasBackendNote Lilypond Int where
  exportNote b = exportNote b . fmap toInteger
  exportChord b = exportChord b . fmap (fmap (toInteger))

instance HasBackendNote Lilypond Float where
  exportNote b = exportNote b . fmap (toInteger . round)
  exportChord b = exportChord b . fmap (fmap (toInteger . round))

instance HasBackendNote Lilypond Double where
  exportNote b = exportNote b . fmap (toInteger . round)
  exportChord b = exportChord b . fmap (fmap (toInteger . round))

instance Integral a => HasBackendNote Lilypond (Ratio a) where
  exportNote b = exportNote b . fmap (toInteger . round)

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (Behavior a) where
  exportNote b = exportNote b . fmap (! 0)

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (Sum a) where
  exportNote b = exportNote b . fmap getSum

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (Product a) where
  exportNote b = exportNote b . fmap getProduct

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (PartT n a) where
  -- Part structure is handled by HasMidiBackendScore instances, so this is just an identity
  exportNote b = exportNote b . fmap extract
  exportChord b = exportChord b . fmap (fmap extract)

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (DynamicT DynamicNotation a) where
  exportNote b (LyContext d x) = notate (fmap (^.dynamic) x) $ exportNote b $ LyContext d (fmap extract x)
    where
      notate Nothing  = id
      notate (Just n) = notate' n

      notate' :: DynamicNotation -> LyMusic -> LyMusic
      notate' (DynamicNotation (cds, showLevel)) = (rcomposed $ fmap notateCrescDim $ cds) . notateLevel
        where
          -- Use rcomposed as notateDynamic returns "mark" order, not application order
          rcomposed = composed . reverse
          
          notateCrescDim x = case x of
            NoCrescDim -> id
            BeginCresc -> Lilypond.beginCresc
            EndCresc   -> Lilypond.endCresc
            BeginDim   -> Lilypond.beginDim
            EndDim     -> Lilypond.endDim

          -- TODO these literals are not so nice...
          notateLevel = case showLevel of
             Nothing -> id
             Just lvl -> Lilypond.addDynamics (fromDynamics (DynamicsL (Just (fixLevel . realToFrac $ lvl), Nothing)))
          
          fixLevel :: Double -> Double
          fixLevel x = (fromIntegral $ round (x - 0.5)) + 0.5


instance HasBackendNote Lilypond a => HasBackendNote Lilypond (ColorT a) where
  exportNote b (LyContext d x) = notate x $ exportNote b $ LyContext d (fmap extract x)
    where
      notate Nothing = id
      notate (Just (ColorT (col, x))) = notate' col

      notate' (Option Nothing)             = id
      notate' (Option (Just (Last color))) = \x -> Lilypond.Sequential [
      -- This syntax will change in future Lilypond versions
        Lilypond.Override "NoteHead#' color" (Lilypond.toLiteralValue $ "#" ++ colorName color),
        x,
        Lilypond.Revert "NoteHead#' color"
        ]

      -- TODO handle any color
      colorName c
        | c == Color.black = "black"
        | c == Color.red   = "red"
        | c == Color.blue  = "blue"
        | otherwise        = error "Lilypond backend: Unkown color"


instance HasBackendNote Lilypond a => HasBackendNote Lilypond (ArticulationT n a) where
  exportNote b = exportNote b . fmap extract

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (TremoloT a) where
  exportNote b = \(LyContext d x) ->
    (fst $ notate x d) $ exportNote b $ LyContext (snd $ notate x d) (fmap extract x)
    where
      notate Nothing d                               = (id, d)
      notate (Just (TremoloT (Couple (Max 0, _)))) d = (id, d)
      notate (Just (TremoloT (Couple (Max n, _)))) d = let
        scale   = 2^n
        newDur  = (d `min` (1/4)) / scale
        repeats = d / newDur
        in (Lilypond.Tremolo (round $ repeats), newDur)     

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (TextT a) where
  exportNote b (LyContext d x) = notate x $ exportNote b $ LyContext d (fmap extract x)
    where
      notate Nothing = id
      notate (Just (TextT (Couple (ts, _)))) = foldr (.) id (fmap Lilypond.addText ts)

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (HarmonicT a) where
  -- exportNote b = exportNote b . fmap extract
  exportNote b (LyContext d x) = notate x $ exportNote b $ LyContext d (fmap extract x)
    where
      notate Nothing = id
      notate (Just (HarmonicT (Couple ((view _Wrapped' -> isNat, view _Wrapped' -> n),x)))) = notate' isNat n

      notate' _     0 = id
      notate' True  n = notateNatural n
      notate' False n = notateArtificial n

      notateNatural n = Lilypond.addFlageolet -- addOpen?
      notateArtificial n = id -- TODO
      

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (SlideT a) where
  -- exportNote b = exportNote b . fmap extract
  exportNote b (LyContext d x) = notate x $ exportNote b $ LyContext d (fmap extract x)
    where
      notate Nothing = id
      notate (Just (SlideT (Couple (((eg,es),(bg,bs)),a)))) =
        if view _Wrapped' bg || view _Wrapped' bs then Lilypond.beginGlissando else id
      

instance HasBackendNote Lilypond a => HasBackendNote Lilypond (TieT a) where
  exportNote b (LyContext d x) = notate x $ exportNote b $ LyContext d (fmap extract x)
    where
      notate Nothing = id
      notate (Just (TieT ((Any ta, Any tb),_))) = notate' ta tb
      notate' ta tb
        | ta && tb                      = id . Lilypond.beginTie
        | tb                            = Lilypond.beginTie
        | ta                            = id
        | otherwise                     = id


-- Internal stuff
pcatLy :: [Lilypond.Music] -> Lilypond.Music
pcatLy = pcatLy' False

pcatLy' :: Bool -> [Lilypond.Music] -> Lilypond.Music
pcatLy' p = foldr Lilypond.simultaneous (Lilypond.Simultaneous p [])

scatLy :: [Lilypond.Music] -> Lilypond.Music
scatLy = foldr Lilypond.sequential (Lilypond.Sequential [])

spellLy :: Integer -> Lilypond.Note
spellLy a = Lilypond.NotePitch (spellLy' a) Nothing

spellLy' :: Integer -> Lilypond.Pitch
spellLy' p = Lilypond.Pitch (
  toEnum $ fromIntegral pc,
  fromIntegral alt,
  fromIntegral oct
  )
  where (pc,alt,oct) = spellPitch (p + 72)
-- End internal


type HasLilypondNEW a = (HasBackendNote Lilypond (ScoreEvent Lilypond a), HasBackendScore Lilypond a)

-- |
-- Convert a score to a Lilypond string.
--
toLilypondString :: HasLilypondNEW a => a -> String
toLilypondString = show . Pretty.pretty . toLilypond

-- |
-- Convert a score to a Lilypond representation.
--
toLilypond :: HasLilypondNEW a => a -> Lilypond.Music
toLilypond = export (undefined::Lilypond)


-- |
-- Convert a score to a Lilypond representaiton and print it on the standard output.
--
showLilypond :: HasLilypondNEW a => a -> IO ()
showLilypond = putStrLn . toLilypondString

-- |
-- Convert a score to a Lilypond representation and write to a file.
--
writeLilypond :: HasLilypondNEW a => FilePath -> a -> IO ()
writeLilypond = writeLilypond' def

data LilypondOptions
  = LyInlineFormat
  | LyScoreFormat

instance Default LilypondOptions where
  def = LyInlineFormat

-- |
-- Convert a score to a Lilypond representation and write to a file.
--
writeLilypond' :: HasLilypondNEW a => LilypondOptions -> FilePath -> a -> IO ()
writeLilypond' options path sc = writeFile path $ (lyFilePrefix ++) $ toLilypondString sc
  where
    -- title    = fromMaybe "" $ flip getTitleAt 0                  $ metaAtStart sc
    -- composer = fromMaybe "" $ flip getAttribution "composer"     $ metaAtStart sc
    title = ""
    composer = ""
    -- TODO generalize metaAtStart!

    lyFilePrefix = case options of
        LyInlineFormat -> lyInlinePrefix
        LyScoreFormat  -> lyScorePrefix

    lyInlinePrefix = mempty                                        ++
        "%%% Generated by music-score %%%\n"                       ++
        "\\include \"lilypond-book-preamble.ly\"\n"                ++
        "\\paper {\n"                                              ++
        "  #(define dump-extents #t)\n"                            ++
        "\n"                                                       ++
        "  indent = 0\\mm\n"                                       ++
        "  line-width = 210\\mm - 2.0 * 0.4\\in\n"                 ++
        "  ragged-right = ##t\n"                                   ++
        "  force-assignment = #\"\"\n"                             ++
        "  line-width = #(- line-width (* mm  3.000000))\n"        ++
        "}\n"                                                      ++
        "\\header {\n"                                             ++
        "  title = \"" ++ title ++ "\"\n"                          ++
        "  composer = \"" ++ composer ++ "\"\n"                    ++
        "}\n"                                                      ++
        "\\layout {\n"                                             ++
        "}"                                                        ++
        "\n\n"

    lyScorePrefix = mempty                                         ++
        "\\paper {"                                                ++
        "  indent = 0\\mm"                                         ++
        "  line-width = 210\\mm - 2.0 * 0.4\\in"                   ++
        "}"                                                        ++
        "\\header {\n"                                             ++
        "  title = \"" ++ title ++ "\"\n"                          ++
        "  composer = \"" ++ composer ++ "\"\n"                    ++
        "}\n"                                                      ++
        "\\layout {"                                               ++
        "}" ++
        "\n\n"


-- |
-- Typeset a score using Lilypond and open it.
--
-- (This is simple wrapper around 'writeLilypond' that may not work well on all platforms.)
--
openLilypond :: HasLilypondNEW a => a -> IO ()
openLilypond = openLilypond' def

-- |
-- Typeset a score using Lilypond and open it.
--
-- (This is simple wrapper around 'writeLilypond' that may not work well on all platforms.)
--
openLilypond' :: HasLilypondNEW a => LilypondOptions -> a -> IO ()
openLilypond' options sc = do
  writeLilypond' options "test.ly" sc
  runLilypond
  cleanLilypond
  runOpen
    where    
      runLilypond = void $ runCommand "lilypond -f pdf test.ly" 
        >>= waitForProcess
      cleanLilypond = void $ runCommand 
        "rm -f test-*.tex test-*.texi test-*.count test-*.eps test-*.pdf test.eps"
      runOpen = void $ runCommand 
        $ openCommand ++ " test.pdf"










-- main = putStrLn $ show $ view notes $ simultaneous
main = do
  -- showLilypond $ music
  openLilypond $ music
music = id
  --  $ over pitches' (+ 2)
  --  $ text "Hello"
  $ compress 1 $ sj </> sj^*2 </> sj^*4
  where
    sj = timesPadding 2 1 $ harmonic 1 (scat [
      color Color.blue $ level _f $ c <> d,
      cs,
      level _f ds,
      level ff fs,
      level _f a_,
      text "pizz" $ level pp gs_,
      tremolo 2 d,
      tremolo 3 e
      ::Score MyNote])^*(1+3/8)   

timesPadding n d x = mcatMaybes $ times n (fmap Just x |> rest^*d)

type MyNote = 
  (PartT Int 
    (TieT 
      (ColorT 
        (TextT 
          (TremoloT 
            (HarmonicT 
              (SlideT 
                (ArticulationT () 
                  (DynamicT 
                    (Sum Double) 
                      [Double])))))))))




open :: Score MyNote -> IO ()
open = do
  -- showLilypond
  openLilypond




-- TODO This function is a workaround
-- Whenever it is used, we should make the original function preserve meta instead
preserveMeta :: (HasMeta a, HasMeta b) => (a -> b) -> a -> b
preserveMeta f x = let m = view meta x in set meta m (f x)
