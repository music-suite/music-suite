

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
-- This class defines types and functions for exporting music in a very general way.
--
-- The backend type @b@ is just a type level tag to identify a specific backend.
--
--
-- The actual conversion is handled by the subclasses 'HasBackendScore' and
-- 'HasBackendNote', which converts the time structure, and the contained music
-- respectively. In general, parametricity ensures that structure and content are handled
-- completely separately. 
--
-- It is often necessary to
-- alter the events based on their surrounding context: for examples the beginning and end
-- of spanners and beams depend on surrounding notes. Thus, the 'BackendContext' type
-- allow 'HasBackendScore' instances to provide context for 'HasBackendNote' instances.
--
-- -- The tag is typically defined as an empty data declaration:
--
-- @
-- data Foo
-- instance HasBackend Foo where
--   type BackendMusic Foo = ...
-- @
--
--
class Functor (BackendScore b) => HasBackend b where
  -- | The full music representation
  type BackendMusic b :: *

  -- | Score, voice and time structure, with output handled by 'HasBackendScore' 
  type BackendScore b :: * -> *

  -- | Notes, chords and rests, with output handled by 'HasBackendNote' 
  type BackendNote b :: *

  -- | This type may be used to pass context from 'exportScore' to 'exportNote'.
  --   Often will typically include duration, onset or surrounding notes.
  --
  --   If the note export is not context-sensitive, 'Identity' can be used.
  type BackendContext b :: * -> *

  finalizeExport :: b -> BackendScore b (BackendNote b) -> BackendMusic b
  
class (HasBackend b) => HasBackendScore b s a | s -> a where
  exportScore :: b -> s -> BackendScore b (BackendContext b a)
  -- default exportScore :: (BackendContext b ~ Identity) => b -> s a -> BackendScore b (BackendContext b a)
  -- exportScore b = fmap Identity

class (HasBackend b) => HasBackendNote b a where
  exportNote  :: b -> BackendContext b a   -> BackendNote b
  exportChord :: b -> BackendContext b [a] -> BackendNote b
  exportChord = error "Not implemented"

  -- exportNote' :: (BackendContext b ~ Identity) => b -> a -> BackendNote b
  -- exportNote' b x = exportNote b (Identity x)

export :: (HasOrdPart a, HasBackendScore b s a, HasBackendNote b a) => b -> s -> BackendMusic b
export b = finalizeExport b . export'
  where
    export' = fmap (exportNote b) . exportScore b



data Foo
instance HasBackend Foo where
  type BackendScore Foo     = []
  type BackendContext Foo   = Identity
  type BackendNote Foo     = [(Sum Int, Int)]
  type BackendMusic Foo     = [(Sum Int, Int)]
  finalizeExport _ = concat
instance HasBackendScore Foo [a] a where
  exportScore _ = fmap Identity
instance HasBackendNote Foo a => HasBackendNote Foo [a] where
  -- exportNote b (Identity ps) = concatMap (exportNote b . Identity) ps
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

data Midi

type MidiContext  = Identity
type MidiEvent    = Score Midi.Message

data MidiScore a
  = MidiScore 
    [((Midi.Channel, 
       Midi.Preset), 
      Score a)] 
  deriving Functor

instance HasBackend Midi where
  type BackendContext Midi    = MidiContext
  type BackendScore   Midi    = MidiScore
  type BackendNote   Midi    = MidiEvent
  type BackendMusic   Midi    = Midi.Midi

  -- TODO assuming that we have now converted each part to a track and set the part
  -- FIXME
  finalizeExport _ (MidiScore trs) = let 
    mainTracks    = fmap (translMidiTrack . fmap join) trs
    controlTrack  = [(0, Midi.TempoChange 1000000), (endDelta, Midi.TrackEnd)]
    in  
    Midi.Midi fileType divisions' (controlTrack : mainTracks) 
    
    where
      -- Each track needs TrackEnd
      -- We place it a long time after last event just in case (necessary?)
      addTrackEnd :: [(Int, Midi.Message)] -> [(Int, Midi.Message)]
      addTrackEnd = (<> [(endDelta, Midi.TrackEnd)])

      translMidiTrack :: ((Midi.Channel, Midi.Preset), Score (Midi.Message)) -> [(Int, Midi.Message)]
      translMidiTrack ((ch, p), x) = id
        $ addTrackEnd 
        $ setProgramChannel ch p 
        $ scoreToMidiTrack 
        $ x

      setProgramChannel :: Midi.Channel -> Midi.Preset -> Midi.Track Midi.Ticks -> Midi.Track Midi.Ticks
      setProgramChannel ch prg = ([(0, Midi.ProgramChange ch prg)] <>) . fmap (fmap $ setC ch)

      scoreToMidiTrack :: Score Midi.Message -> Midi.Track Midi.Ticks
      scoreToMidiTrack = fmap (\(t,_,x) -> (round ((t .-. 0) ^* divisions), x)) . toRelative . (^. events)
      
      fileType    = Midi.MultiTrack
      divisions   = 1024
      divisions'  = Midi.TicksPerBeat divisions
      endDelta    = 10000


instance (HasPart' a, Ord (Part a), HasMidiProgram (Part a)) => HasBackendScore Midi (Score a) a where
  exportScore _ xs = MidiScore (map (\(p,sc) -> ((getMidiChannel p, getMidiProgram p), fmap Identity sc)) $ extractParts' xs)

instance HasBackendNote Midi a => HasBackendNote Midi [a] where
  exportNote b ps = mconcat $ map (exportNote b) $ sequenceA ps

instance HasBackendNote Midi Int where
  exportNote _ (Identity pv) = mkMidiNote pv

instance HasBackendNote Midi a => HasBackendNote Midi (DynamicT (Sum Int) a) where
  exportNote b (Identity (DynamicT (Sum v, x))) = fmap (setV v) $ exportNote b (Identity x)

instance HasBackendNote Midi a => HasBackendNote Midi (ArticulationT b a) where
  exportNote b (Identity (ArticulationT (_, x))) = fmap id $ exportNote b (Identity x)

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

toMidi :: (HasOrdPart a, HasBackendNote Midi a, HasBackendScore Midi s a) => s -> Midi.Midi
toMidi = export (undefined::Midi)










{-
  TODO clefs
  TODO part names
  TODO quantization
  TODO staff groups
-}

data Ly
data LyScore a = LyScore [[a]] deriving (Functor, Eq, Show)
data LyContext a = LyContext Duration a deriving (Functor, Foldable, Traversable, Eq, Show)
instance Monoid Lilypond.Music where
  mempty = pcatLy []
  mappend x y = pcatLy [x,y]

instance HasBackend Ly where
  type BackendScore Ly = LyScore
  type BackendContext Ly = LyContext
  type BackendNote Ly = Lilypond.Music
  type BackendMusic Ly = Lilypond.Music
  finalizeExport _ (LyScore xs) = pcatLy . fmap scatLy $ xs


instance (Transformable a, Semigroup a) => HasBackendScore Ly (Score a) a where
  -- TODO extract, ties etc
  exportScore b s = exportScore b (fmap fromJust $ (^?! singleMVoice) $ simultaneous $ s)

instance HasBackendScore Ly (Voice a) a where
  exportScore _ v = LyScore [map (\(d,x) -> LyContext d x) $ view eventsV v]


voiceToLilypond :: [Maybe TimeSignature] -> [Duration] -> Voice (Maybe a) -> [Lilypond]
voiceToLilypond = undefined





instance HasBackendNote Ly a => HasBackendNote Ly [a] where
  -- exportNote b ps = mconcat $ map (exportNote b) $ sequenceA ps
  exportNote b = exportChord b

instance HasBackendNote Ly Integer where
  -- TODO rest
  exportNote _ (LyContext d x) = (^*realToFrac (d*4)) . Lilypond.note  . spellLy $ x
  exportChord _ (LyContext d xs)  = (^*realToFrac (d*4)) . Lilypond.chord . fmap spellLy $ xs

instance HasBackendNote Ly Int where 
  exportNote b = exportNote b . fmap toInteger

instance HasBackendNote Ly Float where 
  exportNote b = exportNote b . fmap (toInteger . round)

instance HasBackendNote Ly Double where 
  exportNote b = exportNote b . fmap (toInteger . round)

instance Integral a => HasBackendNote Ly (Ratio a) where 
  exportNote b = exportNote b . fmap (toInteger . round)

instance HasBackendNote Ly a => HasBackendNote Ly (Behavior a) where
  exportNote b = exportNote b . fmap (! 0)

instance HasBackendNote Ly a => HasBackendNote Ly (Sum a) where
  exportNote b = exportNote b . fmap getSum

instance HasBackendNote Ly a => HasBackendNote Ly (Product a) where
  exportNote b = exportNote b . fmap getProduct

instance HasBackendNote Ly a => HasBackendNote Ly (PartT n a) where
  exportNote b = exportNote b . fmap (snd . getPartT)

instance HasBackendNote Ly a => HasBackendNote Ly (DynamicT n a) where
  exportNote b = exportNote b . fmap (snd . getDynamicT)

instance HasBackendNote Ly a => HasBackendNote Ly (ArticulationT n a) where
  exportNote b = exportNote b . fmap (snd . getArticulationT)
  
instance HasBackendNote Ly a => HasBackendNote Ly (TremoloT a) where
  exportNote b (LyContext d (TremoloT (n, x))) = exportNote b $ LyContext d x -- TODO many
    -- where
    -- getL d (TremoloT (Max 0, x)) = exportNote b (LyContext d [x])
    -- getL d (TremoloT (Max n, x)) = notate $ getLilypond newDur x
    --     where
    --         scale   = 2^n
    --         newDur  = (d `min` (1/4)) / scale
    --         repeats = d / newDur
    --         notate = Lilypond.Tremolo (round repeats)

instance HasBackendNote Ly a => HasBackendNote Ly (TextT a) where
  exportNote b (LyContext d (TextT (n, x))) = notate n (exportNote b $ LyContext d x) -- TODO many
    where
      notate ts = foldr (.) id (fmap Lilypond.addText ts)

instance HasBackendNote Ly a => HasBackendNote Ly (HarmonicT a) where
  exportNote b = exportNote b . fmap (snd . getHarmonicT)

instance HasBackendNote Ly a => HasBackendNote Ly (SlideT a) where
  exportNote b = exportNote b . fmap (snd . getSlideT)

instance HasBackendNote Ly a => HasBackendNote Ly (TieT a) where
  exportNote b = exportNote b . fmap (snd . getTieT)

-- type Lilypond = Lilypond.Music
toLilypondString :: (HasOrdPart a, HasBackendNote Ly a, HasBackendScore Ly s a) => s -> String
toLilypondString = show . Pretty.pretty . toLilypond

toLilypond :: (HasOrdPart a, HasBackendNote Ly a, HasBackendScore Ly s a) => s -> Lilypond.Music
toLilypond = export (undefined::Ly)




-- TODO tests
-- main = putStrLn $ show $ view notes $ simultaneous 
main = putStrLn $ toLilypondString $ simultaneous
  $ over pitches' (+ 2)
  --  $ text "Hello"
  $ (scat [c<>cs,d,e::Score (PartT Int (TextT [Integer]))])^*(1/8)









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





