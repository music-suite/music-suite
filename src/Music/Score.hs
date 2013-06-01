
{-# LANGUAGE
    CPP,
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,     
    GeneralizedNewtypeDeriving,
    FlexibleInstances,
    TypeOperators,    
    OverloadedStrings,
    FlexibleContexts, -- for IsPitch stuff
    NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides a musical score represenation.
--
-------------------------------------------------------------------------------------

module Music.Score (

        module Music.Score.Time,
        module Music.Score.Duration,

        module Music.Score.Track,
        module Music.Score.Voice,
        module Music.Score.Score,

        module Music.Score.Pitch,
        module Music.Score.Part,
        module Music.Score.Ties,

        module Music.Score.Articulation,
        module Music.Score.Dynamics,
        module Music.Score.Ornaments,

        -- ** Constructors
#ifdef __HADDOCK__
        rest,
        note,
        chord,
        melody,   
#endif

        -- ** Composing
        (|>),
        (<|),
        scat,
        -- pcat,
        -- sustain,
        -- overlap,
        -- anticipate,
        
        -- ** Transforming
        move,
        moveBack,
        startAt,
        stopAt,
        stretch,
        compress,
        stretchTo,
        
        -- ** Conversions
        scoreToTrack,
        scoreToVoice,
        -- scoreToVoices,
        voiceToScore,
        trackToScore,

        -- * Export         
        -- ** MIDI        
        HasMidi(..),
        toMidi,
        writeMidi,
        playMidi,
        playMidiIO,

        -- ** MusicXML
        XmlScore,
        XmlMusic,
        HasMusicXml(..),
        toXml,
        writeXml,
        openXml,
        toXmlVoice,
        writeXmlVoice,
        openXmlVoice,
)
where

{-
    TODO
       Split and reverse
       Zipper applicative (for appying dynamic and articulation transformations etc)
-}  

import Prelude hiding (foldr, concat, foldl, mapM, concatMap, maximum, sum, minimum)

import Data.Semigroup
import Data.Ratio
import Data.String
import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Plus
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Traversable
import Data.Function (on)
import Data.Ord (comparing)
import Data.VectorSpace
import Data.AffineSpace
import Data.Basis

import Control.Reactive
import Control.Reactive.Midi

import Music.Score.Time
import Music.Score.Duration
import Music.Score.Rhythm
import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Combinators
import Music.Score.Pitch
import Music.Score.Ties
import Music.Score.Part
import Music.Score.Articulation
import Music.Score.Dynamics
import Music.Score.Ornaments

import qualified Codec.Midi as Midi
import qualified Music.MusicXml.Simple as Xml
import qualified Data.Map as Map
import qualified Data.List as List

import System.Posix
import System.IO.Unsafe
import Music.Pitch.Literal
import Music.Dynamics.Literal


-------------------------------------------------------------------------------------

-- |
-- Class of types that can be converted to MIDI.
--
-- Numeric types are interpreted as notes with a default velocity, pairs are
-- interpreted as @(pitch, velocity)@ pairs.
--
-- Minimal definition: 'getMidi'. Given 'getMidiScore', 'getMidi' can be implemented
-- as @getMidiScore . return@.
--
class HasMidi a where
    
    -- | Convert a value to a MIDI score.
    --   Typically, generates an /on/ event using 'note' followed by an optional /off/ event.
    getMidi :: a -> Score Midi.Message

    -- | Convert a score to a MIDI score.
    --   The default definition can be overriden for efficiency.
    getMidiScore :: Score a -> Score Midi.Message
    getMidiScore = (>>= getMidi)


instance HasMidi (Integer, Integer) where
    getMidi (p,v) = mempty
        |> note (Midi.NoteOn 0 (fromIntegral p) (fromIntegral v)) 
        |> note (Midi.NoteOff 0 (fromIntegral p) (fromIntegral v))

instance HasMidi Midi.Message               where   getMidi = return
instance HasMidi Int                        where   getMidi = getMidi . toInteger    
instance HasMidi Integer                    where   getMidi = \x -> getMidi (x,100::Integer)
instance HasMidi Float                      where   getMidi = getMidi . toInteger . round
instance HasMidi Double                     where   getMidi = getMidi . toInteger . round
instance Integral a => HasMidi (Ratio a)    where   getMidi = getMidi . toInteger . round    
instance HasMidi a => HasMidi (Maybe a)     where   getMidi = getMidiScore . mfromMaybe


-- |
-- Convert a score to a MIDI file representaiton.
--    
toMidi :: HasMidi a => Score a -> Midi.Midi
toMidi score = Midi.Midi fileType divisions' [controlTrack, eventTrack]
    where                                                        
        endPos          = 10000
        fileType        = Midi.MultiTrack       
        divisions       = 1024
        divisions'      = Midi.TicksPerBeat divisions
        controlTrack    = [(0, Midi.TempoChange 1000000), (endPos, Midi.TrackEnd)]
        eventTrack      = events <> [(endPos, Midi.TrackEnd)] 

        events :: [(Midi.Ticks, Midi.Message)]
        events          = (\(t,_,x) -> (round (t * divisions), x)) <$> performance

        performance :: [(Time, Duration, Midi.Message)]
        performance     = performRelative (getMidiScore score)

        -- FIXME arbitrary endTime (files won't work without this...)
        -- TODO handle voice

-- |
-- Convert a score MIDI and write to a file.
--    
writeMidi :: HasMidi a => FilePath -> Score a -> IO ()
writeMidi path sc = Midi.exportFile path (toMidi sc)

-- |
-- Convert a score to a MIDI event.
--    
playMidi :: HasMidi a => String -> Score a -> Event MidiMessage
playMidi dest x = midiOut midiDest $ playback trig (pure $ toTrack $ delay 0.2 x)
    where
        trig        = accumR 0 ((+ 0.005) <$ pulse 0.005)        
        toTrack     = fmap (\(t,_,m) -> (t,m)) . perform . getMidiScore
        midiDest    = fromJust $ unsafeGetReactive (findDestination  $ pure dest)

-- |
-- Convert a score to a MIDI event and run it.
--    
playMidiIO :: HasMidi a => String -> Score a -> IO ()
playMidiIO dest = runLoop . playMidi dest

        


-------------------------------------------------------------------------------------

type XmlScore = Xml.Score
type XmlMusic = Xml.Music

-- |
-- Class of types that can be converted to MusicXML.
--
class Tiable a => HasMusicXml a where          
    -- | 
    -- Convert a value to MusicXML.
    --
    -- Typically, generates a 'XmlMusic' value using 'Xml.note' or 'Xml.chord', and transforms it 
    -- to add beams, slurs, dynamics, articulation etc.
    --
    getMusicXml :: Duration -> a -> XmlMusic

instance HasMusicXml Int                        where   getMusicXml d = getMusicXml d . toInteger    
instance HasMusicXml Float                      where   getMusicXml d = getMusicXml d . toInteger . round
instance HasMusicXml Double                     where   getMusicXml d = getMusicXml d . toInteger . round
instance Integral a => HasMusicXml (Ratio a)    where   getMusicXml d = getMusicXml d . toInteger . round    
-- instance HasMusicXml a => HasMusicXml (Maybe a) where   getMusicXml d = ?

instance HasMusicXml Integer where
    getMusicXml d p = Xml.note (spell (fromIntegral p)) d'
        where
            d' = (fromRational . toRational $ d)   
            
            -- FIXME arbitrary spelling, please modularize...
            spell :: Int -> Xml.Pitch
            spell p = (
                toEnum pitchClass, 
                if alteration == 0 then Nothing else Just (fromIntegral alteration), 
                fromIntegral octave
                ) 
                where
                    octave     = (p `div` 12) - 1
                    semitone   = p `mod` 12
                    pitchClass = fromStep major semitone
                    alteration = semitone - step major pitchClass

                    step xs p = xs !! (p `mod` length xs)
                    fromStep xs p = fromMaybe (length xs - 1) $ List.findIndex (>= p) xs
                    scaleFromSteps = snd . List.mapAccumL add 0
                        where
                            add a x = (a + x, a + x)
                    major = scaleFromSteps [0,2,2,1,2,2,2,1]
                    

-- |
-- Convert a score to MusicXML and write to a file. 
-- 
writeXml :: (HasMusicXml a, HasPart a, v ~ Part a, Ord v, Show v) => FilePath -> Score a -> IO ()
writeXml path sc = writeFile path (Xml.showXml $ toXml sc)

-- |
-- Convert a score to MusicXML and open it. 
-- 
openXml :: (HasMusicXml a, HasPart a, v ~ Part a, Ord v, Show v) => Score a -> IO ()
openXml sc = do
    writeXml "test.xml" sc
    execute "open" ["-a", "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6", "test.xml"]
    -- FIXME hardcode

-- |
-- Convert a score to MusicXML and write to a file. 
-- 
writeXmlVoice :: HasMusicXml a => FilePath -> Score a -> IO ()
writeXmlVoice path sc = writeFile path (Xml.showXml $ toXmlVoice sc)

-- |
-- Convert a score to MusicXML and open it. 
-- 
openXmlVoice :: HasMusicXml a => Score a -> IO ()
openXmlVoice sc = do
    writeXmlVoice "test.xml" sc
    execute "open" ["-a", "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6", "test.xml"]
    -- FIXME hardcode


-- |
-- Convert a score to a MusicXML representation. 
-- 
toXml :: (HasMusicXml a, HasPart a, v ~ Part a, Ord v, Show v) => Score a -> XmlScore
toXml sc = Xml.fromParts "Title" "Composer" pl . fmap toXmlVoice' . voices $ sc
    where
        pl = Xml.partList (fmap show $ getParts sc)

-- |
-- Convert a single-part score to a MusicXML representation. 
-- 
toXmlVoice :: HasMusicXml a => Score a -> XmlScore
toXmlVoice = Xml.fromPart "Title" "Composer" "Voice" . toXmlVoice'


toXmlVoice' :: HasMusicXml a => Score a -> [XmlMusic]
toXmlVoice' = 
    addDefaultSignatures . 
        fmap barToXml . separateBars . 
        splitTiesVoice . scoreToVoice
    where
        addDefaultSignatures []            = []
        addDefaultSignatures (bar1 : rest) = (defaultSignatures <> bar1) : rest
        defaultSignatures = mempty
            <> Xml.defaultKey
            <> Xml.defaultDivisions 
            <> Xml.metronome (1/4) 60
            <> Xml.commonTime
            
-- |
-- Given a set of absolute-time occurences, separate at each zero-time occurence.
-- Note that this require every bar to start with a zero-time occurence.
-- 
separateBars :: HasMusicXml a => Voice (Maybe a) -> [[(Duration, Maybe a)]]
separateBars = 
    fmap removeTime . fmap (fmap discardBarNumber) .
        splitAtTimeZero . fmap separateTime . perform . voiceToScore
    where  
        separateTime (t,d,x)            = ((bn,bt),d,x) where (bn,bt) = properFraction (toRational t)
        splitAtTimeZero                 = splitWhile ((== 0) . getBarTime) where getBarTime ((bn,bt),_,_) = bt
        discardBarNumber ((bn,bt),d,x)  = (fromRational bt, d, x)
        removeTime                      = fmap g where g (t,d,x) = (d,x)


barToXml :: HasMusicXml a => [(Duration, Maybe a)] -> Xml.Music
barToXml bar = case quantize bar of
    Left e   -> error $ "barToXml: Could not quantize this bar: " ++ show e
    Right rh -> rhythmToXml rh

-- FIXME dotted rests does not work...
rhythmToXml :: HasMusicXml a => Rhythm (Maybe a) -> Xml.Music
rhythmToXml (Beat d x)            = noteRestToXml d x
rhythmToXml (Dotted n (Beat d x)) = noteRestToXml (dotMod n * d) x
rhythmToXml (Tuplet m r)          = Xml.tuplet 
                                        (fromIntegral $ denominator $ getDuration m) 
                                        (fromIntegral $ numerator $ getDuration m) (rhythmToXml r)
rhythmToXml (Bound  d r)          = noteRestToXml (fromRational $ getDuration d) x <> rhythmToXml r2
    where
        (x,r2) = toTiedRhythm r
rhythmToXml (Rhythms rs)          = mconcat $ map rhythmToXml rs

toTiedRhythm :: HasMusicXml a => Rhythm (Maybe a) -> (Maybe a, Rhythm (Maybe a))
toTiedRhythm (Beat d a)       = (b, Beat d c)     where (b,c) = toTied a
toTiedRhythm (Dotted n a)     = (b, Dotted n c)   where (b,c) = toTiedRhythm a
toTiedRhythm (Tuplet m a)     = (b, Tuplet m c)   where (b,c) = toTiedRhythm a 
toTiedRhythm (Bound  d r)     = error "toXml: Nested bounded rhytms"
toTiedRhythm (Rhythms [])     = error "toXml: Bound empty rhythm"
toTiedRhythm (Rhythms (a:as)) = (b, Rhythms (c:as)) where (b,c) = toTiedRhythm a

noteRestToXml :: HasMusicXml a => Duration -> Maybe a -> Xml.Music
noteRestToXml d Nothing  = setDefaultVoice $ Xml.rest d' where d' = fromRational . toRational $ d   
noteRestToXml d (Just p) = setDefaultVoice $ getMusicXml d p

-- TODO only works for single-voice parts
setDefaultVoice :: Xml.Music -> Xml.Music
setDefaultVoice = Xml.setVoice 1

-------------------------------------------------------------------------------------
-- Transformer instances (TODO move)
-------------------------------------------------------------------------------------

-- PitchT

-- instance HasMidi a => HasMidi (PitchT p a) where
--     getMidi (PitchT (_,x))          = getMidi x
-- instance HasMusicXml a => HasMusicXml (PitchT p a) where
--     getMusicXml d (PitchT (_,x))    = getMusicXml d x
-- instance (IsPitch a, IsString n) => IsPitch (PitchT p a) where
--     fromPitch l                     = PitchT ("", fromPitch l)
-- instance (IsDynamics a, IsString n) => IsDynamics (PitchT p a) where
--     fromDynamics l                  = PitchT ("", fromDynamics l)
-- instance HasDynamic a => HasDynamic (PitchT p a) where
--     setBeginCresc n (PitchT (v,x)) = PitchT (v, setBeginCresc n x)
--     setEndCresc   n (PitchT (v,x)) = PitchT (v, setEndCresc n x)
--     setBeginDim   n (PitchT (v,x)) = PitchT (v, setBeginDim n x)
--     setEndDim     n (PitchT (v,x)) = PitchT (v, setEndDim n x)
--     setLevel      n (PitchT (v,x)) = PitchT (v, setLevel n x)
-- instance HasArticulation a => HasArticulation (PitchT p a) where
--     setEndSlur    n (PitchT (v,x)) = PitchT (v, setEndSlur n x)
--     setContSlur   n (PitchT (v,x)) = PitchT (v, setContSlur n x)
--     setBeginSlur  n (PitchT (v,x)) = PitchT (v, setBeginSlur n x)
--     setAccLevel   n (PitchT (v,x)) = PitchT (v, setAccLevel n x)
--     setStaccLevel n (PitchT (v,x)) = PitchT (v, setStaccLevel n x)
-- instance HasTremolo a => HasTremolo (PitchT p a) where
--     setTrem       n (PitchT (v,x)) = PitchT (v, setTrem n x)
-- 

-- Maybe
-- instance HasPart a => HasPart (Maybe a) where   
--     type Part (Maybe a)         = Part a
--     getPart Nothing             = error "No part"
--     getPart (Just a)            = getPart a
--     modifyPart f                = fmap (modifyPart f)
-- 
-- instance HasMusicXml a => HasMusicXml (Maybe a) where
--     getMusicXml d Nothing   = error "No note"
--     getMusicXml d (Just a)  = getMusicXml d a

instance IsPitch a => IsPitch (Maybe a) where
    fromPitch = Just . fromPitch

-- PartT


instance HasMidi a => HasMidi (PartT n a) where
    getMidi (PartT (_,x))          = getMidi x
instance HasMusicXml a => HasMusicXml (PartT n a) where
    getMusicXml d (PartT (_,x))    = getMusicXml d x
instance HasPart (PartT n a) where   
    type Part (PartT n a)         = n
    getPart (PartT (v,_))         = v
    modifyPart f (PartT (v,x))    = PartT (f v, x)
instance HasPitch a => HasPitch (PartT n a) where   
    type Pitch (PartT n a)         = Pitch a
    getPitch (PartT (v,a))         = getPitch a
    modifyPitch f (PartT (v,x))    = PartT (v, modifyPitch f x)
-- TODO IsPitch/IsDynamic with mempty/def as default as well?
instance (IsPitch a, Enum n) => IsPitch (PartT n a) where
    fromPitch l                     = PartT (toEnum 0, fromPitch l)
instance (IsDynamics a, Enum n) => IsDynamics (PartT n a) where
    fromDynamics l                  = PartT (toEnum 0, fromDynamics l)
instance Tiable a => Tiable (PartT n a) where
    toTied (PartT (v,a)) = (PartT (v,b), PartT (v,c)) where (b,c) = toTied a
instance HasDynamic a => HasDynamic (PartT n a) where
    setBeginCresc n (PartT (v,x))  = PartT (v, setBeginCresc n x)
    setEndCresc   n (PartT (v,x))  = PartT (v, setEndCresc n x)
    setBeginDim   n (PartT (v,x))  = PartT (v, setBeginDim n x)
    setEndDim     n (PartT (v,x))  = PartT (v, setEndDim n x)
    setLevel      n (PartT (v,x))  = PartT (v, setLevel n x)
instance HasArticulation a => HasArticulation (PartT n a) where
    setEndSlur    n (PartT (v,x))  = PartT (v, setEndSlur n x)
    setContSlur   n (PartT (v,x))  = PartT (v, setContSlur n x)
    setBeginSlur  n (PartT (v,x))  = PartT (v, setBeginSlur n x)
    setAccLevel   n (PartT (v,x))  = PartT (v, setAccLevel n x)
    setStaccLevel n (PartT (v,x))  = PartT (v, setStaccLevel n x)
instance HasTremolo a => HasTremolo (PartT n a) where
    setTrem       n (PartT (v,x))  = PartT (v, setTrem n x)
instance HasHarmonic a => HasHarmonic (PartT n a) where
    setHarmonic   n (PartT (v,x))  = PartT (v, setHarmonic n x)
instance HasSlide a => HasSlide (PartT n a) where
    setBeginGliss n (PartT (v,x))  = PartT (v, setBeginGliss n x)
    setBeginSlide n (PartT (v,x))  = PartT (v, setBeginSlide n x)
    setEndGliss   n (PartT (v,x))  = PartT (v, setEndGliss n x)
    setEndSlide   n (PartT (v,x))  = PartT (v, setEndSlide n x)
instance HasText a => HasText (PartT n a) where
    addText       s (PartT (v,x))  = PartT (v, addText s x)


-- TieT

instance HasMidi a => HasMidi (TieT a) where
    getMidi (TieT (_,x,_))          = getMidi x
instance HasMusicXml a => HasMusicXml (TieT a) where
    getMusicXml d (TieT (ta,x,tb))  = addTies $ getMusicXml d x
        where
            addTies | ta && tb      = Xml.endTie . Xml.beginTie
                    | tb            = Xml.beginTie
                    | ta            = Xml.endTie
                    | otherwise     = id
instance HasPart a => HasPart (TieT a) where   
    type Part (TieT a)             = Part a
    getPart (TieT (_,x,_))         = getPart x
    modifyPart f (TieT (b,x,e))    = TieT (b,modifyPart f x,e)
instance HasPitch a => HasPitch (TieT a) where   
    type Pitch (TieT a)             = Pitch a
    getPitch (TieT (_,x,_))         = getPitch x
    modifyPitch f (TieT (b,x,e))    = TieT (b,modifyPitch f x,e)
instance IsPitch a => IsPitch (TieT a) where
    fromPitch l                     = TieT (False, fromPitch l, False)
instance IsDynamics a => IsDynamics (TieT a) where
    fromDynamics l                  = TieT (False, fromDynamics l, False)
instance HasDynamic a => HasDynamic (TieT a) where
    setBeginCresc n (TieT (b,x,e))  = TieT (b,setBeginCresc n x,e)
    setEndCresc   n (TieT (b,x,e))  = TieT (b,setEndCresc n x,e)
    setBeginDim   n (TieT (b,x,e))  = TieT (b,setBeginDim n x,e)
    setEndDim     n (TieT (b,x,e))  = TieT (b,setEndDim n x,e)
    setLevel      n (TieT (b,x,e))  = TieT (b,setLevel n x,e)
instance HasArticulation a => HasArticulation (TieT a) where
    setEndSlur    n (TieT (b,x,e))  = TieT (b,setEndSlur n x,e)
    setContSlur   n (TieT (b,x,e))  = TieT (b,setContSlur n x,e)
    setBeginSlur  n (TieT (b,x,e))  = TieT (b,setBeginSlur n x,e)
    setAccLevel   n (TieT (b,x,e))  = TieT (b,setAccLevel n x,e)
    setStaccLevel n (TieT (b,x,e))  = TieT (b,setStaccLevel n x,e)
instance HasTremolo a => HasTremolo (TieT a) where
    setTrem       n (TieT (b,x,e))  = TieT (b,setTrem n x,e)
instance HasHarmonic a => HasHarmonic (TieT a) where
    setHarmonic   n (TieT (b,x,e))  = TieT (b,setHarmonic n x,e)
instance HasSlide a => HasSlide (TieT a) where
    setBeginGliss n (TieT (b,x,e))  = TieT (b,setBeginGliss n x,e)
    setBeginSlide n (TieT (b,x,e))  = TieT (b,setBeginSlide n x,e)
    setEndGliss   n (TieT (b,x,e))  = TieT (b,setEndGliss n x,e)
    setEndSlide   n (TieT (b,x,e))  = TieT (b,setEndSlide n x,e)
instance HasText a => HasText (TieT a) where
    addText       s (TieT (b,x,e))  = TieT (b, addText s x, e)


-- DynamicT

-- end cresc/dim, level, begin cresc/dim
-- newtype DynamicT a = DynamicT { getDynamicT :: (Bool, Bool, Maybe Double, a, Bool, Bool) }

instance HasMidi a => HasMidi (DynamicT a) where
    getMidi (DynamicT (ec,ed,l,a,bc,bd))        = getMidi a

instance HasMusicXml a => HasMusicXml (DynamicT a) where
    getMusicXml d (DynamicT (ec,ed,l,a,bc,bd))  = notate $ getMusicXml d a
        where
            notate x = nec <> ned <> nl <> nbc <> nbd <> x
            nec    = if ec then Xml.endCresc    else mempty
            ned    = if ed then Xml.endDim      else mempty
            nbc    = if bc then Xml.beginCresc  else mempty
            nbd    = if bd then Xml.beginDim    else mempty
            nl     = case l of 
                Nothing  -> mempty
                Just lvl -> Xml.dynamic (fromDynamics (DynamicsL (Just lvl, Nothing)))

instance Tiable a => Tiable (DynamicT a) where
    toTied (DynamicT (ec,ed,l,a,bc,bd))             = (DynamicT (ec,ed,l,b,bc,bd),
                                                       DynamicT (False,False,Nothing,c,False,False)) where (b,c) = toTied a
instance HasPart a => HasPart (DynamicT a) where   
    type Part (DynamicT a)                         = Part a
    getPart (DynamicT (ec,ed,l,a,bc,bd))           = getPart a
    modifyPart f (DynamicT (ec,ed,l,a,bc,bd))      = DynamicT (ec,ed,l,modifyPart f a,bc,bd)
instance HasPitch a => HasPitch (DynamicT a) where   
    type Pitch (DynamicT a)                         = Pitch a
    getPitch (DynamicT (ec,ed,l,a,bc,bd))           = getPitch a
    modifyPitch f (DynamicT (ec,ed,l,a,bc,bd))      = DynamicT (ec,ed,l,modifyPitch f a,bc,bd)
instance IsPitch a => IsPitch (DynamicT a) where
    fromPitch l                                     = DynamicT (False,False,Nothing,fromPitch l,False,False)
instance IsDynamics a => IsDynamics (DynamicT a) where
    fromDynamics l                                  = DynamicT (False,False,Nothing,fromDynamics l,False,False)
instance HasDynamic (DynamicT a) where
    setBeginCresc bc (DynamicT (ec,ed,l,a,_ ,bd))   = DynamicT (ec,ed,l,a,bc,bd)
    setEndCresc   ec (DynamicT (_ ,ed,l,a,bc,bd))   = DynamicT (ec,ed,l,a,bc,bd)
    setBeginDim   bd (DynamicT (ec,ed,l,a,bc,_ ))   = DynamicT (ec,ed,l,a,bc,bd)
    setEndDim     ed (DynamicT (ec,_ ,l,a,bc,bd))   = DynamicT (ec,ed,l,a,bc,bd)
    setLevel      l  (DynamicT (ec,ed,_,a,bc,bd))   = DynamicT (ec,ed,Just l,a,bc,bd)
instance HasArticulation a => HasArticulation (DynamicT a) where
    setEndSlur    n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setEndSlur n a,bc,bd)
    setContSlur   n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setContSlur n a,bc,bd)
    setBeginSlur  n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setBeginSlur n a,bc,bd)
    setAccLevel   n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setAccLevel n a,bc,bd)
    setStaccLevel n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setStaccLevel n a,bc,bd)
instance HasTremolo a => HasTremolo (DynamicT a) where
    setTrem       n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setTrem n a,bc,bd)
instance HasHarmonic a => HasHarmonic (DynamicT a) where
    setHarmonic   n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setHarmonic n a,bc,bd)
instance HasSlide a => HasSlide (DynamicT a) where
    setBeginGliss n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setBeginGliss n a,bc,bd)
    setBeginSlide n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setBeginSlide n a,bc,bd)
    setEndGliss   n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setEndGliss n a,bc,bd)
    setEndSlide   n (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,setEndSlide n a,bc,bd)
instance HasText a => HasText (DynamicT a) where
    addText       s (DynamicT (ec,ed,l,a,bc,bd))    = DynamicT (ec,ed,l,addText s a,bc,bd)


-- ArticulationT

-- end slur, cont slur, acc level, stacc level, begin slur
-- newtype ArticulationT a = ArticulationT { getArticulationT :: (Bool, Bool, Int, Int, a, Bool) }

instance HasMidi a => HasMidi (ArticulationT a) where
    getMidi (ArticulationT (es,us,al,sl,a,bs))          = getMidi a
instance HasMusicXml a => HasMusicXml (ArticulationT a) where
    getMusicXml d (ArticulationT (es,us,al,sl,a,bs))    = notate $ getMusicXml d a
        where
            notate = nes . nal . nsl . nbs
            nes    = if es then Xml.endSlur else id
            nal    = case al of
                0    -> id
                1    -> Xml.accent
                2    -> Xml.strongAccent
            nsl    = case sl of
                (-2) -> Xml.tenuto
                (-1) -> Xml.tenuto . Xml.staccato
                0    -> id
                1    -> Xml.staccato
                2    -> Xml.staccatissimo
            nbs    = if bs then Xml.beginSlur else id
                
instance Tiable a => Tiable (ArticulationT a) where
    toTied (ArticulationT (es,us,al,sl,a,bs))           = (ArticulationT (False,us,al,sl,b,bs),
                                                           ArticulationT (es,   us,0,0,c,False)) where (b,c) = toTied a
instance HasPart a => HasPart (ArticulationT a) where   
    type Part (ArticulationT a)                        = Part a
    getPart (ArticulationT (es,us,al,sl,a,bs))         = getPart a
    modifyPart f (ArticulationT (es,us,al,sl,a,bs))    = ArticulationT (es,us,al,sl,modifyPart f a,bs)
instance HasPitch a => HasPitch (ArticulationT a) where   
    type Pitch (ArticulationT a)                        = Pitch a
    getPitch (ArticulationT (es,us,al,sl,a,bs))         = getPitch a
    modifyPitch f (ArticulationT (es,us,al,sl,a,bs))    = ArticulationT (es,us,al,sl,modifyPitch f a,bs)
instance IsPitch a => IsPitch (ArticulationT a) where
    fromPitch l                                         = ArticulationT (False,False,0,0,fromPitch l,False)
instance IsDynamics a => IsDynamics (ArticulationT a) where
    fromDynamics l                                      = ArticulationT (False,False,0,0,fromDynamics l,False)
instance HasDynamic a => HasDynamic (ArticulationT a) where
    setBeginCresc n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setBeginCresc n a,bs)
    setEndCresc   n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setEndCresc n a,bs)
    setBeginDim   n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setBeginDim n a,bs)
    setEndDim     n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setEndDim n a,bs)
    setLevel      n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setLevel n a,bs)
instance HasArticulation (ArticulationT a) where
    setEndSlur    es (ArticulationT (_ ,us,al,sl,a,bs)) = ArticulationT (es,us,al,sl,a,bs)
    setContSlur   us (ArticulationT (es,_ ,al,sl,a,bs)) = ArticulationT (es,us,al,sl,a,bs)
    setBeginSlur  bs (ArticulationT (es,us,al,sl,a,_ )) = ArticulationT (es,us,al,sl,a,bs)
    setAccLevel   al (ArticulationT (es,us,_ ,sl,a,bs)) = ArticulationT (es,us,al,sl,a,bs)
    setStaccLevel sl (ArticulationT (es,us,al,_ ,a,bs)) = ArticulationT (es,us,al,sl,a,bs)
instance HasTremolo a => HasTremolo (ArticulationT a) where
    setTrem n (ArticulationT (es,us,al,sl,a,bs))        = ArticulationT (es,us,al,sl,setTrem n a,bs)
instance HasHarmonic a => HasHarmonic (ArticulationT a) where
    setHarmonic   n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setHarmonic n a,bs)
instance HasSlide a => HasSlide (ArticulationT a) where
    setBeginGliss n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setBeginGliss n a,bs)
    setBeginSlide n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setBeginSlide n a,bs)
    setEndGliss   n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setEndGliss n a,bs)
    setEndSlide   n (ArticulationT (es,us,al,sl,a,bs))  = ArticulationT (es,us,al,sl,setEndSlide n a,bs)
instance HasText a => HasText (ArticulationT a) where
    addText      s (ArticulationT (es,us,al,sl,a,bs))   = ArticulationT (es,us,al,sl,addText s a,bs)
        
 
-- TremoloT

-- newtype TremoloT a = TremoloT { getTremoloT :: (Int, a) }

instance HasMidi a => HasMidi (TremoloT a) where
    getMidi (TremoloT (_,x))            = getMidi x
instance HasMusicXml a => HasMusicXml (TremoloT a) where
    getMusicXml d (TremoloT (n,x))      = notate $ getMusicXml d x
        where
            notate = case n of 
                0 -> id
                _ -> Xml.tremolo n
            
instance Tiable a => Tiable (TremoloT a) where
    toTied (TremoloT (n,a))             = (TremoloT (n,b), TremoloT (n,c)) where (b,c) = toTied a
instance HasPart a => HasPart (TremoloT a) where   
    type Part (TremoloT a)             = Part a
    getPart (TremoloT (_,a))           = getPart a
    modifyPart f (TremoloT (n,x))      = TremoloT (n, modifyPart f x)
instance HasPitch a => HasPitch (TremoloT a) where   
    type Pitch (TremoloT a)             = Pitch a
    getPitch (TremoloT (_,a))           = getPitch a
    modifyPitch f (TremoloT (n,x))      = TremoloT (n, modifyPitch f x)
instance IsPitch a => IsPitch (TremoloT a) where
    fromPitch l                         = TremoloT (0, fromPitch l)
instance IsDynamics a => IsDynamics (TremoloT a) where
    fromDynamics l                      = TremoloT (0, fromDynamics l)
instance HasDynamic a => HasDynamic (TremoloT a) where
    setBeginCresc n (TremoloT (v,x))    = TremoloT (v, setBeginCresc n x)
    setEndCresc   n (TremoloT (v,x))    = TremoloT (v, setEndCresc n x)
    setBeginDim   n (TremoloT (v,x))    = TremoloT (v, setBeginDim n x)
    setEndDim     n (TremoloT (v,x))    = TremoloT (v, setEndDim n x)
    setLevel      n (TremoloT (v,x))    = TremoloT (v, setLevel n x)
instance HasArticulation a => HasArticulation (TremoloT a) where
    setEndSlur    n (TremoloT (v,x))    = TremoloT (v, setEndSlur n x)
    setContSlur   n (TremoloT (v,x))    = TremoloT (v, setContSlur n x)
    setBeginSlur  n (TremoloT (v,x))    = TremoloT (v, setBeginSlur n x)
    setAccLevel   n (TremoloT (v,x))    = TremoloT (v, setAccLevel n x)
    setStaccLevel n (TremoloT (v,x))    = TremoloT (v, setStaccLevel n x)
instance HasTremolo (TremoloT a) where
    setTrem      n (TremoloT (_,x))     = TremoloT (n,x)
instance HasHarmonic a => HasHarmonic (TremoloT a) where
    setHarmonic   n (TremoloT (v,x))    = TremoloT (v, setHarmonic n x)
instance HasSlide a => HasSlide (TremoloT a) where
    setBeginGliss n (TremoloT (v,x))    = TremoloT (v, setBeginGliss n x)
    setBeginSlide n (TremoloT (v,x))    = TremoloT (v, setBeginSlide n x)
    setEndGliss   n (TremoloT (v,x))    = TremoloT (v, setEndGliss n x)
    setEndSlide   n (TremoloT (v,x))    = TremoloT (v, setEndSlide n x)
instance HasText a => HasText (TremoloT a) where
    addText      s (TremoloT (n,x))     = TremoloT (n,addText s x)


-- TextT

-- newtype TextT a = TextT { getTextT :: (Int, a) }

instance HasMidi a => HasMidi (TextT a) where
    getMidi (TextT (_,x))        = getMidi x
instance HasMusicXml a => HasMusicXml (TextT a) where
    getMusicXml d (TextT (s,x))  = notate s $ getMusicXml d x
        where             
            notate ts a = mconcat (fmap Xml.text ts) <> a
            
instance Tiable a => Tiable (TextT a) where
    toTied (TextT (n,a))            = (TextT (n,b), TextT (mempty,c)) where (b,c) = toTied a
instance HasPart a => HasPart (TextT a) where   
    type Part (TextT a)            = Part a
    getPart (TextT (_,a))          = getPart a
    modifyPart f (TextT (n,x))     = TextT (n, modifyPart f x)
instance HasPitch a => HasPitch (TextT a) where   
    type Pitch (TextT a)            = Pitch a
    getPitch (TextT (_,a))          = getPitch a
    modifyPitch f (TextT (n,x))     = TextT (n, modifyPitch f x)
instance IsPitch a => IsPitch (TextT a) where
    fromPitch l                     = TextT (mempty, fromPitch l)
instance IsDynamics a => IsDynamics (TextT a) where
    fromDynamics l                  = TextT (mempty, fromDynamics l)
instance HasDynamic a => HasDynamic (TextT a) where
    setBeginCresc n (TextT (v,x))   = TextT (v, setBeginCresc n x)
    setEndCresc   n (TextT (v,x))   = TextT (v, setEndCresc n x)
    setBeginDim   n (TextT (v,x))   = TextT (v, setBeginDim n x)
    setEndDim     n (TextT (v,x))   = TextT (v, setEndDim n x)
    setLevel      n (TextT (v,x))   = TextT (v, setLevel n x)
instance HasArticulation a => HasArticulation (TextT a) where
    setEndSlur    n (TextT (v,x))   = TextT (v, setEndSlur n x)
    setContSlur   n (TextT (v,x))   = TextT (v, setContSlur n x)
    setBeginSlur  n (TextT (v,x))   = TextT (v, setBeginSlur n x)
    setAccLevel   n (TextT (v,x))   = TextT (v, setAccLevel n x)
    setStaccLevel n (TextT (v,x))   = TextT (v, setStaccLevel n x)
instance HasTremolo a => HasTremolo (TextT a) where
    setTrem       n (TextT (s,x))    = TextT (s,setTrem n x)
instance HasHarmonic a => HasHarmonic (TextT a) where
    setHarmonic   n (TextT (v,x))   = TextT (v, setHarmonic n x)
instance HasSlide a => HasSlide (TextT a) where
    setBeginGliss n (TextT (v,x))   = TextT (v, setBeginGliss n x)
    setBeginSlide n (TextT (v,x))   = TextT (v, setBeginSlide n x)
    setEndGliss   n (TextT (v,x))   = TextT (v, setEndGliss n x)
    setEndSlide   n (TextT (v,x))   = TextT (v, setEndSlide n x)
instance HasText (TextT a) where
    addText      s (TextT (t,x))    = TextT (t ++ [s],x)
                                     

-- HarmonicT

instance HasMidi a => HasMidi (HarmonicT a) where
    getMidi (HarmonicT (_,x))        = getMidi x
instance HasMusicXml a => HasMusicXml (HarmonicT a) where
    getMusicXml d (HarmonicT (n,x))  = notate $ getMusicXml d x
        where             
            notate | n /= 0     = Xml.setNoteHead Xml.DiamondNoteHead
                   | otherwise  = id
    -- TODO adjust pitch etc
            
instance Tiable a => Tiable (HarmonicT a) where
    toTied (HarmonicT (n,a))            = (HarmonicT (n,b), HarmonicT (n,c)) where (b,c) = toTied a
instance HasPart a => HasPart (HarmonicT a) where   
    type Part (HarmonicT a)            = Part a
    getPart (HarmonicT (_,a))          = getPart a
    modifyPart f (HarmonicT (n,x))     = HarmonicT (n, modifyPart f x)
instance HasPitch a => HasPitch (HarmonicT a) where   
    type Pitch (HarmonicT a)            = Pitch a
    getPitch (HarmonicT (_,a))          = getPitch a
    modifyPitch f (HarmonicT (n,x))     = HarmonicT (n, modifyPitch f x)
instance IsPitch a => IsPitch (HarmonicT a) where
    fromPitch l                         = HarmonicT (0, fromPitch l)
instance IsDynamics a => IsDynamics (HarmonicT a) where
    fromDynamics l                      = HarmonicT (0, fromDynamics l)
instance HasDynamic a => HasDynamic (HarmonicT a) where
    setBeginCresc n (HarmonicT (v,x))   = HarmonicT (v, setBeginCresc n x)
    setEndCresc   n (HarmonicT (v,x))   = HarmonicT (v, setEndCresc n x)
    setBeginDim   n (HarmonicT (v,x))   = HarmonicT (v, setBeginDim n x)
    setEndDim     n (HarmonicT (v,x))   = HarmonicT (v, setEndDim n x)
    setLevel      n (HarmonicT (v,x))   = HarmonicT (v, setLevel n x)
instance HasArticulation a => HasArticulation (HarmonicT a) where
    setEndSlur    n (HarmonicT (v,x))   = HarmonicT (v, setEndSlur n x)
    setContSlur   n (HarmonicT (v,x))   = HarmonicT (v, setContSlur n x)
    setBeginSlur  n (HarmonicT (v,x))   = HarmonicT (v, setBeginSlur n x)
    setAccLevel   n (HarmonicT (v,x))   = HarmonicT (v, setAccLevel n x)
    setStaccLevel n (HarmonicT (v,x))   = HarmonicT (v, setStaccLevel n x)
instance HasTremolo a => HasTremolo (HarmonicT a) where
    setTrem       n (HarmonicT (s,x))    = HarmonicT (s,setTrem n x)
instance HasHarmonic (HarmonicT a) where
    setHarmonic   n (HarmonicT (_,x))    = HarmonicT (n,x)
instance HasSlide a => HasSlide (HarmonicT a) where
    setBeginGliss n (HarmonicT (s,x))    = HarmonicT (s,setBeginGliss n x)
    setBeginSlide n (HarmonicT (s,x))    = HarmonicT (s,setBeginSlide n x)
    setEndGliss   n (HarmonicT (s,x))    = HarmonicT (s,setEndGliss n x)
    setEndSlide   n (HarmonicT (s,x))    = HarmonicT (s,setEndSlide n x)
instance HasText a => HasText (HarmonicT a) where
    addText      s (HarmonicT (n,x))    = HarmonicT (n,addText s x)


-- SlideT

instance HasMidi a => HasMidi (SlideT a) where
    getMidi (SlideT (_,_,a,_,_))          = getMidi a
instance HasMusicXml a => HasMusicXml (SlideT a) where
    getMusicXml d (SlideT (eg,es,a,bg,bs))    = notate $ getMusicXml d a
        where
            notate = neg . nes . nbg . nbs
            neg    = if es then Xml.endGliss else id
            nes    = if es then Xml.endSlide else id
            nbg    = if es then Xml.beginGliss else id
            nbs    = if es then Xml.beginSlide else id

                
instance Tiable a => Tiable (SlideT a) where
    toTied (SlideT (eg,es,a,bg,bs))           = (SlideT (eg,   es,   b,False,False),
                                                 SlideT (False,False,c,bg,   bs)) where (b,c) = toTied a
instance HasPart a => HasPart (SlideT a) where   
    type Part (SlideT a)                     = Part a
    getPart (SlideT (eg,es,a,bg,bs))         = getPart a
    modifyPart f (SlideT (eg,es,a,bg,bs))    = SlideT (eg,es,modifyPart f a,bg,bs)
instance HasPitch a => HasPitch (SlideT a) where   
    type Pitch (SlideT a)                     = Pitch a
    getPitch (SlideT (eg,es,a,bg,bs))         = getPitch a
    modifyPitch f (SlideT (eg,es,a,bg,bs))    = SlideT (eg,es,modifyPitch f a,bg,bs)
instance IsPitch a => IsPitch (SlideT a) where
    fromPitch l                               = SlideT (False,False,fromPitch l,False,False)
instance IsDynamics a => IsDynamics (SlideT a) where
    fromDynamics l                            = SlideT (False,False,fromDynamics l,False,False)
instance HasDynamic a => HasDynamic (SlideT a) where
    setBeginCresc n (SlideT (eg,es,a,bg,bs))  = SlideT (eg,es,setBeginCresc n a,bg,bs)
    setEndCresc   n (SlideT (eg,es,a,bg,bs))  = SlideT (eg,es,setEndCresc n a,bg,bs)
    setBeginDim   n (SlideT (eg,es,a,bg,bs))  = SlideT (eg,es,setBeginDim n a,bg,bs)
    setEndDim     n (SlideT (eg,es,a,bg,bs))  = SlideT (eg,es,setEndDim n a,bg,bs)
    setLevel      n (SlideT (eg,es,a,bg,bs))  = SlideT (eg,es,setLevel n a,bg,bs)
instance HasArticulation a => HasArticulation (SlideT a) where
    setEndSlur    n (SlideT (eg,es,a,bg,bs)) = SlideT (eg,es,setEndSlur n a,bg,bs)
    setContSlur   n (SlideT (eg,es,a,bg,bs)) = SlideT (eg,es,setContSlur n a,bg,bs)
    setBeginSlur  n (SlideT (eg,es,a,bg,bs)) = SlideT (eg,es,setBeginSlur n a,bg,bs)
    setAccLevel   n (SlideT (eg,es,a,bg,bs)) = SlideT (eg,es,setAccLevel n a,bg,bs)
    setStaccLevel n (SlideT (eg,es,a,bg,bs)) = SlideT (eg,es,setStaccLevel n a,bg,bs)
instance HasTremolo a => HasTremolo (SlideT a) where
    setTrem       n (SlideT (eg,es,a,bg,bs)) = SlideT (eg,es,setTrem n a,bg,bs)
instance HasHarmonic a => HasHarmonic (SlideT a) where
    setHarmonic   n (SlideT (eg,es,a,bg,bs)) = SlideT (eg,es,setHarmonic n a,bg,bs)
instance HasSlide (SlideT a) where
    setBeginGliss bg (SlideT (eg,es,a,_,bs)) = SlideT (eg,es,a,bg,bs)
    setBeginSlide bs (SlideT (eg,es,a,bg,_)) = SlideT (eg,es,a,bg,bs)
    setEndGliss   eg (SlideT (_,es,a,bg,bs)) = SlideT (eg,es,a,bg,bs)
    setEndSlide   es (SlideT (eg,_,a,bg,bs)) = SlideT (eg,es,a,bg,bs)
instance HasText a => HasText (SlideT a) where
    addText       s (SlideT (eg,es,a,bg,bs)) = SlideT (eg,es,addText s a,bg,bs)
                                                                                                        

-------------------------------------------------------------------------------------
-- Num, Integral, Enum and Bounded
-------------------------------------------------------------------------------------

-- PartT

instance (Enum v, Eq v, Num a) => Num (PartT v a) where
    PartT (v,a) + PartT (_,b) = PartT (v,a+b)
    PartT (v,a) * PartT (_,b) = PartT (v,a*b)
    PartT (v,a) - PartT (_,b) = PartT (v,a-b)
    abs (PartT (v,a))          = PartT (v,abs a)
    signum (PartT (v,a))       = PartT (v,signum a)
    fromInteger a               = PartT (toEnum 0,fromInteger a)
  
instance (Enum v, Enum a) => Enum (PartT v a) where
    toEnum a = PartT (toEnum 0, toEnum a) -- TODO use def, mempty or minBound?
    fromEnum (PartT (v,a)) = fromEnum a

instance (Enum v, Bounded a) => Bounded (PartT v a) where
    minBound = PartT (toEnum 0, minBound)
    maxBound = PartT (toEnum 0, maxBound)

instance (Enum v, Ord v, Num a, Ord a, Real a) => Real (PartT v a) where
    toRational (PartT (v,a)) = toRational a

instance (Enum v, Ord v, Real a, Enum a, Integral a) => Integral (PartT v a) where
    PartT (v,a) `quotRem` PartT (_,b) = (PartT (v,q), PartT (v,r)) where (q,r) = a `quotRem` b
    toInteger (PartT (v,a)) = toInteger a


-- TieT

instance Num a => Num (TieT a) where
    TieT (et,a,bt) + TieT (_,b,_) = TieT (et,a+b,bt)
    TieT (et,a,bt) * TieT (_,b,_) = TieT (et,a*b,bt)
    TieT (et,a,bt) - TieT (_,b,_) = TieT (et,a-b,bt)
    abs (TieT (et,a,bt))          = TieT (et,abs a,bt)
    signum (TieT (et,a,bt))       = TieT (et,signum a,bt)
    fromInteger a               = TieT (False,fromInteger a,False)
  
instance Enum a => Enum (TieT a) where
    toEnum a                = TieT (False,toEnum a,False) 
    fromEnum (TieT (_,a,_)) = fromEnum a

instance Bounded a => Bounded (TieT a) where
    minBound = TieT (False,minBound,False)
    maxBound = TieT (False,maxBound,False)

instance (Num a, Ord a, Real a) => Real (TieT a) where
    toRational (TieT (_,a,_)) = toRational a

instance (Real a, Enum a, Integral a) => Integral (TieT a) where
    TieT (et,a,bt) `quotRem` TieT (_,b,_) = (TieT (et,q,bt), TieT (et,r,bt)) where (q,r) = a `quotRem` b
    toInteger (TieT (_,a,_)) = toInteger a


-- DynamicT

instance Num a => Num (DynamicT a) where
    DynamicT (p,q,r,a,s,t) + DynamicT (_,_,_,b,_,_) = DynamicT (p,q,r,a+b,s,t)
    DynamicT (p,q,r,a,s,t) * DynamicT (_,_,_,b,_,_) = DynamicT (p,q,r,a*b,s,t)
    DynamicT (p,q,r,a,s,t) - DynamicT (_,_,_,b,_,_) = DynamicT (p,q,r,a-b,s,t)
    abs (DynamicT (p,q,r,a,s,t))                    = DynamicT (p,q,r,abs a,s,t)
    signum (DynamicT (p,q,r,a,s,t))                 = DynamicT (p,q,r,signum a,s,t)
    fromInteger a                                   = DynamicT (False,False,Nothing,fromInteger a,False,False)
  
instance Enum a => Enum (DynamicT a) where
    toEnum a                         = DynamicT (False,False,Nothing,toEnum a,False,False) 
    fromEnum (DynamicT (_,_,_,a,_,_)) = fromEnum a

instance Bounded a => Bounded (DynamicT a) where
    minBound = DynamicT (False,False,Nothing,minBound,False,False)
    maxBound = DynamicT (False,False,Nothing,maxBound,False,False)

instance (Num a, Ord a, Real a) => Real (DynamicT a) where
    toRational (DynamicT (_,_,_,a,_,_)) = toRational a

instance (Real a, Enum a, Integral a) => Integral (DynamicT a) where
    DynamicT (p,q,r,a,s,t) `quotRem` DynamicT (_,_,_,b,_,_) = (DynamicT (p,q,r,q',s,t), DynamicT (p,q,r,r',s,t)) where (q',r') = a `quotRem` b
    toInteger (DynamicT (_,_,_,a,_,_)) = toInteger a


-- ArticulationT

instance Num a => Num (ArticulationT a) where
    ArticulationT (p,q,r,s,a,t) + ArticulationT (_,_,_,_,b,_) = ArticulationT (p,q,r,s,a+b,t)
    ArticulationT (p,q,r,s,a,t) * ArticulationT (_,_,_,_,b,_) = ArticulationT (p,q,r,s,a*b,t)
    ArticulationT (p,q,r,s,a,t) - ArticulationT (_,_,_,_,b,_) = ArticulationT (p,q,r,s,a-b,t)
    abs (ArticulationT (p,q,r,s,a,t))                         = ArticulationT (p,q,r,s,abs a,t)
    signum (ArticulationT (p,q,r,s,a,t))                      = ArticulationT (p,q,r,s,signum a,t)
    fromInteger a                                             = ArticulationT (False,False,0,0,fromInteger a,False)
  
instance Enum a => Enum (ArticulationT a) where
    toEnum a                               = ArticulationT (False,False,0,0,toEnum a,False) 
    fromEnum (ArticulationT (_,_,_,_,a,_)) = fromEnum a

instance Bounded a => Bounded (ArticulationT a) where
    minBound = ArticulationT (False,False,0,0,minBound,False)
    maxBound = ArticulationT (False,False,0,0,maxBound,False)

instance (Num a, Ord a, Real a) => Real (ArticulationT a) where
    toRational (ArticulationT (_,_,_,_,a,_)) = toRational a

instance (Real a, Enum a, Integral a) => Integral (ArticulationT a) where
    ArticulationT (p,q,r,s,a,t) `quotRem` ArticulationT (_,_,_,_,b,_) = (ArticulationT (p,q,r,s,q',t), ArticulationT (p,q,r,s,r',t)) where (q',r') = a `quotRem` b
    toInteger (ArticulationT (_,_,_,_,a,_)) = toInteger a


-- TremoloT

instance Num a => Num (TremoloT a) where
    TremoloT (v,a) + TremoloT (_,b) = TremoloT (v,a+b)
    TremoloT (v,a) * TremoloT (_,b) = TremoloT (v,a*b)
    TremoloT (v,a) - TremoloT (_,b) = TremoloT (v,a-b)
    abs (TremoloT (v,a))          = TremoloT (v,abs a)
    signum (TremoloT (v,a))       = TremoloT (v,signum a)
    fromInteger a               = TremoloT (toEnum 0,fromInteger a)
  
instance Enum a => Enum (TremoloT a) where
    toEnum a = TremoloT (0, toEnum a) -- TODO use def, mempty or minBound?
    fromEnum (TremoloT (v,a)) = fromEnum a

instance Bounded a => Bounded (TremoloT a) where
    minBound = TremoloT (0, minBound)
    maxBound = TremoloT (0, maxBound)

instance (Num a, Real a) => Real (TremoloT a) where
    toRational (TremoloT (_,a)) = toRational a

instance (Real a, Enum a, Integral a) => Integral (TremoloT a) where
    TremoloT (v,a) `quotRem` TremoloT (_,b) = (TremoloT (v,q), TremoloT   (v,r)) where (q,r) = a `quotRem` b
    toInteger (TremoloT (_,a)) = toInteger a


-- TextT

instance Num a => Num (TextT a) where
    TextT (v,a) + TextT (_,b) = TextT (v,a+b)
    TextT (v,a) * TextT (_,b) = TextT (v,a*b)
    TextT (v,a) - TextT (_,b) = TextT (v,a-b)
    abs (TextT (v,a))          = TextT (v,abs a)
    signum (TextT (v,a))       = TextT (v,signum a)
    fromInteger a               = TextT (mempty,fromInteger a)
  
instance Enum a => Enum (TextT a) where
    toEnum a = TextT (mempty, toEnum a) -- TODO use def, mempty or minBound?
    fromEnum (TextT (v,a)) = fromEnum a

instance Bounded a => Bounded (TextT a) where
    minBound = TextT (mempty, minBound)
    maxBound = TextT (mempty, maxBound)

instance (Num a, Ord a, Real a) => Real (TextT a) where
    toRational (TextT (v,a)) = toRational a

instance (Real a, Enum a, Integral a) => Integral (TextT a) where
    TextT (v,a) `quotRem` TextT (_,b) = (TextT (v,q), TextT   (v,r)) where (q,r) = a `quotRem` b
    toInteger (TextT (v,a)) = toInteger a


-- HarmonicT

instance Num a => Num (HarmonicT a) where
    HarmonicT (v,a) + HarmonicT (_,b) = HarmonicT (v,a+b)
    HarmonicT (v,a) * HarmonicT (_,b) = HarmonicT (v,a*b)
    HarmonicT (v,a) - HarmonicT (_,b) = HarmonicT (v,a-b)
    abs (HarmonicT (v,a))          = HarmonicT (v,abs a)
    signum (HarmonicT (v,a))       = HarmonicT (v,signum a)
    fromInteger a               = HarmonicT (toEnum 0,fromInteger a)
  
instance Enum a => Enum (HarmonicT a) where
    toEnum a = HarmonicT (0, toEnum a) -- TODO use def, mempty or minBound?
    fromEnum (HarmonicT (v,a)) = fromEnum a

instance Bounded a => Bounded (HarmonicT a) where
    minBound = HarmonicT (0, minBound)
    maxBound = HarmonicT (0, maxBound)

instance (Num a, Ord a, Real a) => Real (HarmonicT a) where
    toRational (HarmonicT (v,a)) = toRational a

instance (Real a, Enum a, Integral a) => Integral (HarmonicT a) where
    HarmonicT (v,a) `quotRem` HarmonicT (_,b) = (HarmonicT (v,q), HarmonicT   (v,r)) where (q,r) = a `quotRem` b
    toInteger (HarmonicT (v,a)) = toInteger a


-- SlideT

instance Num a => Num (SlideT a) where
    SlideT (eg,es,a,bg,bs) + SlideT (_,_,b,_,_) = SlideT (eg,es,a+b,bg,bs)
    SlideT (eg,es,a,bg,bs) * SlideT (_,_,b,_,_) = SlideT (eg,es,a*b,bg,bs)
    SlideT (eg,es,a,bg,bs) - SlideT (_,_,b,_,_) = SlideT (eg,es,a-b,bg,bs)
    abs (SlideT (eg,es,a,bg,bs))                = SlideT (eg,es,abs a,bg,bs)
    signum (SlideT (eg,es,a,bg,bs))             = SlideT (eg,es,signum a,bg,bs)
    fromInteger a                               = SlideT (False,False,fromInteger a,False,False)
  
instance Enum a => Enum (SlideT a) where
    toEnum a                        = SlideT (False,False,toEnum a,False,False)
    fromEnum (SlideT (_,_,a,_,_))   = fromEnum a

instance Bounded a => Bounded (SlideT a) where
    minBound = SlideT (False,False,minBound,False,False)
    maxBound = SlideT (False,False,maxBound,False,False)  

instance (Num a, Ord a, Real a) => Real (SlideT a) where
    toRational (SlideT (_,_,a,_,_)) = toRational a

instance (Real a, Enum a, Integral a) => Integral (SlideT a) where
    SlideT (eg,es,a,bg,bs) `quotRem` SlideT (_,_,b,_,_) = (SlideT (eg,es,q',bg,bs), SlideT (eg,es,r',bg,bs)) where (q',r') = a `quotRem` b
    toInteger (SlideT (_,_,a,_,_)) = toInteger a


-------------------------------------------------------------------------------------
-- Literals
-------------------------------------------------------------------------------------

instance IsPitch Double where
    fromPitch (PitchL (pc, sem, oct)) = fromIntegral $ semitones sem + diatonic pc + (oct+1) * 12
        where
            semitones = maybe 0 round
            diatonic pc = case pc of
                0 -> 0
                1 -> 2
                2 -> 4
                3 -> 5
                4 -> 7
                5 -> 9
                6 -> 11

instance IsPitch Integer where
    fromPitch (PitchL (pc, sem, oct)) = fromIntegral $ semitones sem + diatonic pc + (oct+1) * 12
        where
            semitones = maybe 0 round
            diatonic pc = case pc of
                0 -> 0
                1 -> 2
                2 -> 4
                3 -> 5
                4 -> 7
                5 -> 9
                6 -> 11

instance IsDynamics Double where
    fromDynamics (DynamicsL (Just x, _)) = x
    fromDynamics (DynamicsL (Nothing, _)) = error "IsDynamics Double: No dynamics"


data Alteration = Sh | Fl
sharp = Sh
flat  = Fl
instance IsPitch (Alteration -> Double) where
    fromPitch l Sh = fromPitch l + 1
    fromPitch l Fl = fromPitch l - 1
instance IsPitch (Alteration -> Integer) where
    fromPitch l Sh = fromPitch l + 1
    fromPitch l Fl = fromPitch l - 1
instance IsPitch (Alteration -> a) => IsPitch (Alteration -> Score a) where
    fromPitch l = pure . fromPitch l

                                                                                                           
-------------------------------------------------------------------------------------
-- Test stuff
-------------------------------------------------------------------------------------


type Id  a = a -> a
type Note = (PartT Int (TieT
    (TremoloT (HarmonicT (SlideT
        (DynamicT (ArticulationT (TextT Integer))))))))
-- type Note = (PartT Int (ArticulationT (TieT Integer)))

asScore :: Id (Score Note)
asScore = id                     

open = openXml . asScore

-------------------------------------------------------------------------------------

list z f [] = z
list z f xs = f xs

first f (x,y)  = (f x, y)
second f (x,y) = (x, f y)

sep :: a -> [a] -> [a]
sep = List.intersperse

concatSep :: [a] -> [[a]] -> [a]
concatSep x = List.concat . sep x

-- | 
-- Group a list into sublists whereever a predicate holds. The matched element
-- is the first in the sublist.
--
-- > splitWhile isSpace "foo bar baz"
-- >    ===> ["foo"," bar"," baz"]
-- >
-- > splitWhile (> 3) [1,5,4,7,0,1,2]
-- >    ===> [[1],[5],[4],[7,0,1,2]]
--
splitWhile :: (a -> Bool) -> [a] -> [[a]]
splitWhile p xs = case splitWhile' p xs of
    []:xss -> xss
    xss    -> xss
    where
        splitWhile' p []     = [[]]
        splitWhile' p (x:xs) = case splitWhile' p xs of
            (xs:xss) -> if p x then []:(x:xs):xss else (x:xs):xss

execute :: FilePath -> [String] -> IO ()
execute program args = do
    forkProcess $ executeFile program True args Nothing
    return ()

single x = [x]            
fmap2 = fmap . fmap
fmap3 = fmap . fmap . fmap

dump = mapM putStrLn . fmap show

left f (Left x)  = Left (f x)
left f (Right y) = Right y

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f as bs = List.sortBy f $ as <> bs



