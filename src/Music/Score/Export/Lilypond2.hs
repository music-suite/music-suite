

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Music.Score.Export.Lilypond2 (
    HasBackend(..),
    HasBackendScore(..),
    HasBackendNote(..),
    export,
    HasMidiProgram,
    Midi,
    toMidi,
    Ly,
    toLilypondString,
    toLilypond,
  ) where

import Music.Pitch.Literal
import Music.Score hiding (
  toLilypond,
  toLilypondString,
  toMidi,
  HasMidiProgram(..),
  HasMidiPart(..),
  )

import qualified Codec.Midi                as Midi
import qualified Music.Lilypond as Lilypond
import qualified Text.Pretty                  as Pretty
import           Music.Score.Export.Common
import Data.Ratio
import Data.Maybe
import Data.Foldable (Foldable)
import Data.Traversable (Traversable, sequenceA)
import Music.Time.Internal.Transform (whilstLD) -- TODO move

{-
  Assume that Music is a type function that returns the underlying music
  representation for a given backend.

  Then, for each backend B we need to provide a function
    s a -> Music B
  where s is some score-like type constructor, and a is some note-like type.
  From a we need to fetch each aspect:
    pitch
    dynamic
    articulation
    part
  and convert it to the relevant representation of that aspect in B.
  For example with lilypond we need to convert to LilypondPitch, LilypondDynamic etc.
  Then we need to take s and convert it into some kind of fold for the musical types
  (usually a set of parallel, seequential compositions). Apply the folds, and we're done.
  
  
  
  
  

  chord
  behavior
  tie
  slide

  tremolo
  harmonic
  text
  clef
-}

-- TODO remove this somehow
type HasOrdPart a = (HasPart' a, Ord (Part a))
-- type HasOrdPart a = (a ~ a)



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
-- data Foo
-- 
-- instance HasBackend Foo where
--   type BackendScore Foo     = []
--   type BackendContext Foo   = Identity
--   type BackendNote Foo      = [(Sum Int, Int)]
--   type BackendMusic Foo     = [(Sum Int, Int)]
--   finalizeExport _ = concat
-- 
-- instance HasBackendScore Foo [a] a where
--   exportScore _ = fmap Identity
-- 
-- instance HasBackendNote Foo a => HasBackendNote Foo [a] where
--   exportNote b ps = mconcat $ map (exportNote b) $ sequenceA ps
-- 
-- instance HasBackendNote Foo Int where
--   exportNote _ (Identity p) = [(mempty ,p)]
-- 
-- instance HasBackendNote Foo a => HasBackendNote Foo (DynamicT (Sum Int) a) where
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



data Foo

instance HasBackend Foo where
  type BackendScore Foo     = []
  type BackendContext Foo   = Identity
  type BackendNote Foo      = [(Sum Int, Int)]
  type BackendMusic Foo     = [(Sum Int, Int)]
  finalizeExport _ = concat

instance HasBackendScore Foo [a] where
  type ScoreEvent Foo [a] = a
  exportScore _ = fmap Identity

instance HasBackendNote Foo a => HasBackendNote Foo [a] where
  exportNote b ps = mconcat $ map (exportNote b) $ sequenceA ps

instance HasBackendNote Foo Int where
  exportNote _ (Identity p) = [(mempty ,p)]

instance HasBackendNote Foo a => HasBackendNote Foo (DynamicT (Sum Int) a) where
  exportNote b (Identity (DynamicT (d,ps))) = set (mapped._1) d $ exportNote b (Identity ps)

-- main = print $ export (undefined::Foo) [DynamicT (Sum 4::Sum Int,3::Int), pure 1]





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

-- | We do not need to pass any context to the event generator.
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
      translMidiTrack (ch, p) x = id
        $ addTrackEnd 
        $ setProgramChannel ch p 
        $ scoreToMidiTrack 
        $ x

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


type instance Part (Voice a) = Part a
type instance SetPart g (Voice a) = Voice (SetPart g a)

-- TODO move
instance (HasParts a b) => HasParts (Voice a) (Voice b) where
  parts = 
    _Wrapped
    . traverse 
    . _Wrapped      -- this needed?
    . whilstLD parts

instance (HasPart' a, HasMidiProgram (Part a)) => HasBackendScore Midi (Voice a) where
  type ScoreEvent Midi (Voice a) = a
  exportScore _ xs = MidiScore [((getMidiChannel (xs^?!parts), getMidiProgram (xs^?!parts)), fmap Identity $ voiceToScore xs)]
    where
      voiceToScore :: Voice a -> Score a
      voiceToScore = error "TODO"

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
  -- TODO use Comonad.extract
  exportNote b = exportNote b . fmap (snd . getPartT)

instance HasBackendNote Midi a => HasBackendNote Midi (TremoloT a) where
  -- TODO use Comonad.extract
  exportNote b = exportNote b . fmap (snd . getTremoloT)

instance HasBackendNote Midi a => HasBackendNote Midi (TextT a) where
  -- TODO use Comonad.extract
  exportNote b = exportNote b . fmap (snd . getTextT)

instance HasBackendNote Midi a => HasBackendNote Midi (HarmonicT a) where
  -- TODO use Comonad.extract
  exportNote b = exportNote b . fmap (snd . getHarmonicT)

instance HasBackendNote Midi a => HasBackendNote Midi (SlideT a) where
  -- TODO use Comonad.extract
  exportNote b = exportNote b . fmap (snd . getSlideT)

instance HasBackendNote Midi a => HasBackendNote Midi (TieT a) where
  -- TODO use Comonad.extract
  exportNote b = exportNote b . fmap (snd . getTieT)

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










{-
  TODO clefs
  TODO part names
  TODO quantization
  TODO staff groups
-}

data Ly
data LyScore a = LyScore [LyStaff a] deriving (Functor, Eq, Show)
type LyStaff a = [a]
-- type LyBar a = a

data LyContext a = LyContext Duration (Maybe a) deriving (Functor, Foldable, Traversable, Eq, Show)
instance Monoid Lilypond.Music where
  mempty = pcatLy []
  mappend x y = pcatLy [x,y]

instance HasBackend Ly where
  type BackendScore Ly = LyScore
  type BackendContext Ly = LyContext
  type BackendNote Ly = Lilypond.Music
  type BackendMusic Ly = Lilypond.Music
  finalizeExport _ (LyScore xs) = pcatLy . fmap scatLy $ xs
  -- finalizeExport = error "No finalizeExport"

instance (HasPart' a, Ord (Part a), Tiable (SetDynamic DynamicNotation a), Dynamic (SetDynamic DynamicNotation a) ~ DynamicNotation, HasDynamics a (SetDynamic DynamicNotation a),Tiable a, Transformable a, Semigroup a, Dynamic a ~ Ctxt (Sum Double) ) 
  => HasBackendScore Ly (Score a) where
  type ScoreEvent Ly (Score a) = SetDynamic DynamicNotation a
  exportScore b = LyScore . return . fmap (LyContext 0.5) . toListOf traverse . fmap Just . fmap (fst.toTied) . over dynamics dynamicDisplay . view (extracted.element 0) . simultaneous

  
foo = undefined
foo :: Score (DynamicT (Ctxt Double) ())
foo' = over dynamics dynamicDisplay foo

-- TODO move
instance Tiable DynamicNotation where
  -- TODO important!
instance Num a => Num (Sum a) where
instance Real a => Real (Sum a) where
  toRational (Sum x) = toRational x
instance Transformable DynamicNotation


instance HasBackendNote Ly a => HasBackendNote Ly [a] where
  exportNote b = exportChord b

instance HasBackendNote Ly Integer where
  -- TODO rest
  exportNote _ (LyContext d (Just x))    = (^*realToFrac (d*4)) . Lilypond.note  . spellLy $ x
  exportChord _ (LyContext d (Just xs))  = (^*realToFrac (d*4)) . Lilypond.chord . fmap spellLy $ xs

instance HasBackendNote Ly Int where 
  exportNote b = exportNote b . fmap toInteger
  exportChord b = exportChord b . fmap (fmap (toInteger))

instance HasBackendNote Ly Float where 
  exportNote b = exportNote b . fmap (toInteger . round)
  exportChord b = exportChord b . fmap (fmap (toInteger . round))

instance HasBackendNote Ly Double where 
  exportNote b = exportNote b . fmap (toInteger . round)
  exportChord b = exportChord b . fmap (fmap (toInteger . round))

instance Integral a => HasBackendNote Ly (Ratio a) where 
  exportNote b = exportNote b . fmap (toInteger . round)

instance HasBackendNote Ly a => HasBackendNote Ly (Behavior a) where
  exportNote b = exportNote b . fmap (! 0)

instance HasBackendNote Ly a => HasBackendNote Ly (Sum a) where
  exportNote b = exportNote b . fmap getSum

instance HasBackendNote Ly a => HasBackendNote Ly (Product a) where
  exportNote b = exportNote b . fmap getProduct

instance HasBackendNote Ly a => HasBackendNote Ly (PartT n a) where
  -- Part structure is handled by HasMidiBackendScore instances, so this is just an identity
  -- TODO use Comonad.extract
  exportNote b = exportNote b . fmap (snd . getPartT)

instance HasBackendNote Ly a => HasBackendNote Ly (DynamicT DynamicNotation a) where
  exportNote b = exportNote b . fmap (snd . getDynamicT)

instance HasBackendNote Ly a => HasBackendNote Ly (ArticulationT n a) where
  exportNote b = exportNote b . fmap (snd . getArticulationT)
  
instance HasBackendNote Ly a => HasBackendNote Ly (TremoloT a) where
  exportNote b (LyContext d (Just (TremoloT (n, x)))) = exportNote b $ LyContext d (Just x) -- TODO many
    -- where
    -- getL d (TremoloT (Max 0, x)) = exportNote b (LyContext d [x])
    -- getL d (TremoloT (Max n, x)) = notate $ getLilypond newDur x
    --     where
    --         scale   = 2^n
    --         newDur  = (d `min` (1/4)) / scale
    --         repeats = d / newDur
    --         notate = Lilypond.Tremolo (round repeats)

instance HasBackendNote Ly a => HasBackendNote Ly (TextT a) where
  exportNote b (LyContext d (Just (TextT (n, x)))) = notate n (exportNote b $ LyContext d (Just x)) -- TODO many
    where
      notate ts = foldr (.) id (fmap Lilypond.addText ts)

instance HasBackendNote Ly a => HasBackendNote Ly (HarmonicT a) where
  exportNote b = exportNote b . fmap (snd . getHarmonicT)

instance HasBackendNote Ly a => HasBackendNote Ly (SlideT a) where
  exportNote b = exportNote b . fmap (snd . getSlideT)

instance HasBackendNote Ly a => HasBackendNote Ly (TieT a) where
  exportNote b = exportNote b . fmap (snd . getTieT)

-- type Lilypond = Lilypond.Music
toLilypondString :: (HasBackendNote Ly (ScoreEvent Ly s), HasBackendScore Ly s) => s -> String
toLilypondString = show . Pretty.pretty . toLilypond

toLilypond :: (HasBackendNote Ly (ScoreEvent Ly s), HasBackendScore Ly s) => s -> Lilypond.Music
toLilypond x = export (undefined::Ly) x

aScore :: Score a -> Score a
aScore = id    





-- TODO tests
-- main = putStrLn $ show $ view notes $ simultaneous 
main = putStrLn $ toLilypondString $ music
music = id
  -- $ over pitches' (+ 2)
  --  $ text "Hello"
  $ (scat [d<>d,d,e::Score (PartT Int (ArticulationT () (DynamicT (Maybe (Sum Double), Sum Double, Maybe (Sum Double)) [Double])))])^*(1/8)

instance HasPitches a b => HasPitches (Sum a) (Sum b) where
  pitches = _Wrapped . pitches
instance IsPitch a => IsPitch (Sum a) where
  fromPitch = Sum . fromPitch
type instance Pitch (Sum a) = Pitch a
type instance SetPitch b (Sum a) = Sum (SetPitch b a)







{-
-- |
-- Convert a voice score to a list of bars.
--
voiceToLilypond :: HasLilypond15 a => [Maybe TimeSignature] -> [Duration] -> Voice (Maybe a) -> [Lilypond]
voiceToLilypond barTimeSigs barDurations = zipWith setBarTimeSig barTimeSigs . fmap barToLilypond . voiceToBars' barDurations
--
-- This is where notation of a single voice takes place
--      * voiceToBars is generic for most notations outputs: it handles bar splitting and ties
--      * barToLilypond is specific: it handles quantization and notation
--
    where
        -- TODO compounds
        setBarTimeSig Nothing x = x
        setBarTimeSig (Just (getTimeSignature -> (m:_, n))) x = scatLy [Lilypond.Time m n, x]


barToLilypond :: HasLilypond15 a => [(Duration, Maybe a)] -> Lilypond
barToLilypond bar = case (fmap rewrite . quantize) bar of
    Left e   -> error $ "barToLilypond: Could not quantize this bar: " ++ show e
    Right rh -> rhythmToLilypond rh

rhythmToLilypond = uncurry ($) . rhythmToLilypond2



-- rhythmToLilypond2 :: HasLilypond15 a => Rhythm (Maybe a) -> (Lilypond -> Lilypond, Lilypond)
rhythmToLilypond2 (Beat d x)            = noteRestToLilypond2 d x
rhythmToLilypond2 (Dotted n (Beat d x)) = noteRestToLilypond2 (dotMod n * d) x
-- TODO propagate
rhythmToLilypond2 (Group rs)            = first (maybe id id) $ second scatLy $ extract1 $ map rhythmToLilypond2 $ rs
rhythmToLilypond2 (Tuplet m r)          = second (Lilypond.Times (realToFrac m)) $ (rhythmToLilypond2 r)
    where (a,b) = fromIntegral *** fromIntegral $ unRatio $ realToFrac m

-- noteRestToLilypond2 :: HasLilypond15 a => Duration -> Maybe a -> (Lilypond -> Lilypond, Lilypond)
noteRestToLilypond2 d Nothing  = ( id, Lilypond.rest^*(realToFrac d*4) )
noteRestToLilypond2 d (Just p) = second Lilypond.removeSingleChords $ getLilypondWithPrefix d p

-- extract first value of type b
-- extract1 :: [(b, a)] -> (Maybe b, [a])
extract1 []         = (Nothing, [])
extract1 ((p,x):xs) = (Just p, x : fmap snd xs)


-}






pcatLy :: [Lilypond] -> Lilypond
pcatLy = pcatLy' False

pcatLy' :: Bool -> [Lilypond] -> Lilypond
pcatLy' p = foldr Lilypond.simultaneous e
    where
        e = Lilypond.Simultaneous p []

scatLy :: [Lilypond] -> Lilypond
scatLy = foldr Lilypond.sequential e
    where
        e = Lilypond.Sequential []

spellLy :: Integer -> Lilypond.Note
spellLy a = Lilypond.NotePitch (spellLy' a) Nothing

spellLy' :: Integer -> Lilypond.Pitch
spellLy' p = Lilypond.Pitch (
    toEnum $ fromIntegral pc,
    fromIntegral alt,
    fromIntegral oct
    )
    where (pc,alt,oct) = spellPitch (p + 72)







type instance Dynamic DynamicNotation = DynamicNotation
newtype DynamicNotation = DynamicNotation { getDynamicNotation :: ([CrescDim], ShowDyn) }
instance Wrapped DynamicNotation where
  type Unwrapped DynamicNotation = ([CrescDim], ShowDyn)
  _Wrapped' = iso getDynamicNotation DynamicNotation


fixLevel :: Double -> Double
fixLevel x = (fromIntegral $ round (x - 0.5)) + 0.5

data CrescDim = NoCrescDim | BeginCresc | EndCresc | BeginDim | EndDim

instance Monoid CrescDim where
  mempty = NoCrescDim
  mappend a _ = a

type ShowDyn  = Bool

mapCtxt :: (a -> b) -> Ctxt a -> Ctxt b
mapCtxt f (a,b,c) = (fmap f a, f b, fmap f c)

extractCtxt :: Ctxt a -> a
extractCtxt (_,x,_) = x

-- Given a dynamic value and its context, decide:
--   
--   1) Whether we should begin or end a crescendo or diminuendo
--   2) Whether we should display the current dynamic value
--   
dynamicDisplay :: (Ord a, Real a) => Ctxt a -> DynamicNotation
dynamicDisplay x = DynamicNotation $ case x of
  (Nothing, y, Nothing) -> ([], True)
  (Nothing, y, Just z ) -> case (y `compare` z) of
    LT      -> ([BeginCresc], True)
    EQ      -> ([],           True)
    GT      -> ([BeginDim],   True)
  (Just x,  y, Just z ) -> case (x `compare` y, y `compare` z) of
    (LT,LT) -> ([NoCrescDim], False)
    (LT,EQ) -> ([EndCresc],   True)
    (EQ,LT) -> ([BeginCresc], False{-True-})

    (GT,GT) -> ([NoCrescDim], False)
    (GT,EQ) -> ([EndDim],     True)
    (EQ,GT) -> ([BeginDim],   False{-True-})

    (EQ,EQ) -> ([],                   False)
    (LT,GT) -> ([EndCresc, BeginDim], True)
    (GT,LT) -> ([EndDim, BeginCresc], True)


  (Just x,  y, Nothing) -> case (x `compare` y) of
    LT      -> ([EndCresc],   True)
    EQ      -> ([],           False)
    GT      -> ([EndDim],     True)
                                      