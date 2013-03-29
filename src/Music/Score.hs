
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,     
    GeneralizedNewtypeDeriving,
    FlexibleInstances,
    TypeOperators,    
    OverloadedStrings,
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
        module Music.Score.Part,
        module Music.Score.Score,

        module Music.Score.Pitch,
        module Music.Score.Voice,
        module Music.Score.Ties,

        module Music.Score.Articulation,
        module Music.Score.Dynamics,
        module Music.Score.Ornaments,

        -- ** Constructors
        rest,
        note,
        chord,
        melody,

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
        scoreToPart,
        scoreToParts,
        partToScore,
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
        toXmlPart,
        writeXmlPart,
        openXmlPart,
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
-- import Control.Compose
import Control.Monad (ap, mfilter, join, liftM, MonadPlus(..))
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
import Music.Score.Part
import Music.Score.Score
import Music.Score.Pitch
import Music.Score.Ties
import Music.Score.Voice
import Music.Score.Articulation
import Music.Score.Dynamics
import Music.Score.Ornaments

import qualified Codec.Midi as Midi
import qualified Music.MusicXml.Simple as Xml
import qualified Data.Map as Map
import qualified Data.List as List

import System.Posix             -- TODO debug
import System.IO.Unsafe         -- TODO debug
import Music.Pitch.Literal      -- TODO debug
import Music.Dynamics.Literal   -- TODO debug


-------------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------------

-- | Creates a score containing the given elements, composed in sequence.
melody :: [a] -> Score a
melody = scat . map note

-- | Creates a score containing the given elements, composed in parallel.
chord :: [a] -> Score a
chord = pcat . map note

-- | Creates a score from a the given melodies, composed in parallel.
melodies :: [[a]] -> Score a
melodies = pcat . map melody

-- | Creates a score from a the given chords, composed in sequence.
chords :: [[a]] -> Score a
chords = scat . map chord

-- | Like 'melody', but stretching each note by the given factors.
melodyStretch :: [(Duration, a)] -> Score a
melodyStretch = scat . map ( \(d, x) -> stretch d $ note x )

-- | Like 'chord', but delays each note the given amounts.
chordDelay :: [(Duration, a)] -> Score a
chordDelay = pcat . map ( \(t, x) -> delay t $ note x )

-- | Like 'chord', but delays and stretches each note the given amounts.
chordDelayStretch :: [(Duration, Duration, a)] -> Score a
chordDelayStretch = pcat . map ( \(t, d, x) -> delay t . stretch d $ note x )

-- -- | Like chord, but delaying each note the given amount.
-- arpeggio :: t -> [a] -> Score a
-- arpeggio t xs = chordDelay (zip [0, t ..] xs)


-------------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------------

-- |
-- Move a score move in time. Equivalent to 'delay'.
-- 
-- > Duration -> Score a -> Score a
-- 
move :: Delayable a => Duration -> a -> a
move = delay

-- |
-- Move a score moveBack in time. Negated verison of 'delay'
-- 
-- > Duration -> Score a -> Score a
-- 
moveBack :: Delayable a => Duration -> a -> a
moveBack t = delay (negate t)

-- |
-- Stretch a score. Equivalent to '*^'.
-- 
-- > Duration -> Score a -> Score a
-- 
stretch :: VectorSpace v => Scalar v -> v -> v
stretch = (*^)

-- |
-- Move a score to start at a specific time.
-- 
-- > Duration -> Score a -> Score a
-- 
startAt :: (Delayable a, HasOnset a) => Time -> a -> a
t `startAt` x = delay d x where d = t .-. onset x

-- |
-- Move a score to stop at a specific time.
-- 
-- > Duration -> Score a -> Score a
-- 
stopAt :: (Delayable a, HasOnset a) => Time -> a -> a
t `stopAt`  x = delay d x where d = t .-. offset x

-- |
-- Compress a score. Flipped version of '^/'.
-- 
-- > Duration -> Score a -> Score a
-- 
compress :: (VectorSpace v, s ~ Scalar v, Fractional s) => s -> v -> v
compress = flip (^/)

-- | 
-- Stretch to the given duration. 
-- 
-- > Duration -> Score a -> Score a
-- 
stretchTo :: (VectorSpace a, HasDuration a, Scalar a ~ Duration) => Duration -> a -> a
t `stretchTo` x = (t / duration x) `stretch` x 


-------------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------------

infixr 6 |>
infixr 6 <|

-- |
-- Compose in sequence.
--
-- To compose in parallel, use '<>'.
--
-- > Score a -> Score a -> Score a
(|>) :: (Semigroup a, Delayable a, HasOnset a) => a -> a -> a
a |> b =  a <> startAt (offset a) b
-- a |< b =  a <> stopAt (onset a) b


-- |
-- Compose in reverse sequence. 
--
-- To compose in parallel, use '<>'.
--
-- > Score a -> Score a -> Score a
(<|) :: (Semigroup a, Delayable a, HasOnset a) => a -> a -> a
a <| b =  b |> a

-- |
-- Sequential concatentation.
--
-- > [Score t] -> Score t
scat :: (Monoid a, Delayable a, HasOnset a) => [a] -> a
scat = unwrapMonoid . foldr (|>) mempty . fmap WrapMonoid

-- |
-- Parallel concatentation. A synonym for 'mconcat'.
--
-- > [Score t] -> Score t
pcat :: Monoid a => [a] -> a
pcat = mconcat

infixr 7 <<|
infixr 7 |>>
infixr 7 <||
infixr 7 ||>

(<||) = sustain
(||>) = flip sustain
(|>>) = overlap
(<<|) = flip overlap    

-- | 
-- Like '<>', but scaling the second agument to the duration of the first.
-- 
-- > Score a -> Score a -> Score a
--
sustain :: (Semigroup a, VectorSpace a, HasDuration a, Scalar a ~ Duration) => a -> a -> a
x `sustain` y = x <> (duration x) `stretchTo` y

-- Like '<>', but truncating the second agument to the duration of the first.
-- prolong x y = x <> before (duration x) y

-- |
-- Like '|>', but moving second argument halfway to the offset of the first.
--
-- > Score a -> Score a -> Score a
--
overlap :: (Semigroup a, Delayable a, HasDuration a) => a -> a -> a
x `overlap` y  =  x <> delay t y where t = duration x / 2    

-- |
-- Like '|>' but with a negative delay on the second element.
-- 
-- > Duration -> Score a -> Score a -> Score a
-- 
anticipate :: (Semigroup a, Delayable a, HasDuration a, HasOnset a) => Duration -> a -> a -> a
anticipate t x y = x |> delay t' y where t' = (duration x - t) `max` 0


-------------------------------------------------------------------------------------

-- mapNotes = Score . getScore

-- mapStartMiddleStop :: (a -> a) -> (a -> a) -> (a -> a) -> Score a -> Score a
mapStartMiddleStop f g h = mapVoices (\x -> x)


-------------------------------------------------------------------------------------
-- Conversion

-- |
-- Convert a score to a track by throwing away durations.
--
scoreToTrack :: Score a -> Track a
scoreToTrack = Track . fmap g . perform
    where
        g (t,d,x) = (t,x)

-- |
-- Convert a single-part score to a part.
--
scoreToPart :: Score a -> Part (Maybe a)
scoreToPart = Part . fmap g . addRests' . perform
    where
        g (t,d,x) = (d,x)

-- |
-- Convert a score to a list of parts.
--
scoreToParts :: (HasVoice a, Voice a ~ v, Ord v) => Score a -> [Part (Maybe a)]
scoreToParts = fmap scoreToPart . voices

-- |
-- Convert a part to a score.
--
partToScore :: Part a -> Score a
partToScore = scat . fmap g . getPart
    where
        g (d,x) = stretch d (note x)

-- |
-- Convert a track to a score. Each note gets an arbitrary duration of one.
--
trackToScore :: Track a -> Score a
trackToScore = pcat . fmap g . getTrack
    where
        g (t,x) = delay (t .-. 0) (note x)


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


instance HasMidi Double                     where   getMidi = getMidi . toInteger . round
instance HasMidi Float                      where   getMidi = getMidi . toInteger . round
instance HasMidi Int                        where   getMidi = getMidi . toInteger    
instance Integral a => HasMidi (Ratio a)    where   getMidi = getMidi . toInteger . round    
instance HasMidi Midi.Message               where   getMidi = return
instance HasMidi Integer                    where   getMidi = \x -> getMidi (x,100::Integer)

instance HasMidi (Integer, Integer) where
    getMidi (p,v) = mempty
        |> note (Midi.NoteOn 0 (fromIntegral p) (fromIntegral v)) 
        |> note (Midi.NoteOff 0 (fromIntegral p) (fromIntegral v))


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
playMidi :: HasMidi a => Score a -> Event MidiMessage
playMidi x = midiOut midiDest $ playback trig (pure $ toTrack $ rest^*0.2 |> x)
    where
        trig        = accumR 0 ((+ 0.005) <$ pulse 0.005)        
        toTrack     = fmap (\(t,_,m) -> (t,m)) . perform . getMidiScore
        midiDest    = fromJust $ unsafeGetReactive (findDestination  $ pure "Graphic MIDI")
        -- FIXME hardcoded output...

-- |
-- Convert a score to a MIDI event and run it.
--    
playMidiIO :: HasMidi a => Score a -> IO ()
playMidiIO = runLoop . playMidi

        


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
    getMusicXml      :: Duration -> a -> XmlMusic

instance HasMusicXml Double                     where   getMusicXml d = getMusicXml d . (toInteger . round)
instance HasMusicXml Float                      where   getMusicXml d = getMusicXml d . (toInteger . round)
instance HasMusicXml Int                        where   getMusicXml d = getMusicXml d . toInteger    
instance Integral a => HasMusicXml (Ratio a)    where   getMusicXml d = getMusicXml d . (toInteger . round)    

-- FIXME arbitrary spelling, please modularize...
instance HasMusicXml Integer where
    getMusicXml d p = Xml.note (spell (fromIntegral p)) d'
        where
            d' = (fromRational . toRational $ d)   
            
            step xs p = xs !! (p `mod` length xs)
            fromStep xs p = fromMaybe (length xs - 1) $ List.findIndex (>= p) xs
            scaleFromSteps = snd . List.mapAccumL add 0
                where
                    add a x = (a + x, a + x)
            major = scaleFromSteps [0,2,2,1,2,2,2,1]

            spell :: Int -> Xml.Pitch
            spell p = (toEnum pitchClass, if (alteration == 0) then Nothing else Just (fromIntegral alteration), fromIntegral octave) 
                where
                    octave     = (p `div` 12) - 1
                    semitone   = p `mod` 12
                    pitchClass = fromStep major semitone
                    alteration = semitone - step major pitchClass


-- |
-- Convert a score to MusicXML and write to a file. 
-- 
writeXml :: (HasMusicXml a, HasVoice a, v ~ Voice a, Ord v, Show v) => FilePath -> Score a -> IO ()
writeXml path sc = writeFile path (Xml.showXml $ toXml sc)

-- |
-- Convert a score to MusicXML and open it. 
-- 
openXml :: (HasMusicXml a, HasVoice a, v ~ Voice a, Ord v, Show v) => Score a -> IO ()
openXml sc = do
    writeXml "test.xml" sc
    execute "open" ["-a", "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6", "test.xml"]
    -- FIXME hardcode

-- |
-- Convert a score to MusicXML and write to a file. 
-- 
writeXmlPart :: HasMusicXml a => FilePath -> Score a -> IO ()
writeXmlPart path sc = writeFile path (Xml.showXml $ toXmlPart sc)

-- |
-- Convert a score to MusicXML and open it. 
-- 
openXmlPart :: HasMusicXml a => Score a -> IO ()
openXmlPart sc = do
    writeXmlPart "test.xml" sc
    execute "open" ["-a", "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6", "test.xml"]
    -- FIXME hardcode


-- |
-- Convert a score to a MusicXML representaiton. 
-- 
toXml :: (HasMusicXml a, HasVoice a, v ~ Voice a, Ord v, Show v) => Score a -> XmlScore
toXml sc = Xml.fromParts "Title" "Composer" pl . fmap toXmlPart' . voices $ sc
    where
        pl = Xml.partList (fmap show $ getVoices sc)

-- |
-- Convert a single-part score to a MusicXML representaiton. 
-- 
toXmlPart :: HasMusicXml a => Score a -> XmlScore
toXmlPart = Xml.fromPart "Title" "Composer" "Part" . toXmlPart'


toXmlPart' :: HasMusicXml a => Score a -> [XmlMusic]
toXmlPart' = id               
    . prelims
    . fmap barToXml 
    . separateBars 
    . splitTiesSingle
    . addRests
    . perform  
    where
        prelims []            = []
        prelims (bar1 : rest) = (pre <> bar1) : rest
        pre = mempty
            <> Xml.defaultKey
            <> Xml.defaultDivisions 
            <> Xml.metronome (1/4) 60
            <> Xml.commonTime

-- | 
-- Given a rest-free single-part score (such as those produced by perform), add explicit rests.
-- The result will have no empty space.
--
addRests :: [(Time, Duration, a)] -> Score a
addRests = Score . concat . snd . mapAccumL g 0
    where
        g prevTime (t, d, x) 
            | prevTime == t   =  (t .+^ d, [(t, d, Just x)])
            | prevTime <  t   =  (t .+^ d, [(prevTime, t .-. prevTime, Nothing), (t, d, Just x)])
            | otherwise       =  error "addRests: Strange prevTime"

addRests' :: [(Time, Duration, a)] -> [(Time, Duration, Maybe a)]
addRests' = concat . snd . mapAccumL g 0
    where
        g prevTime (t, d, x) 
            | prevTime == t   =  (t .+^ d, [(t, d, Just x)])
            | prevTime <  t   =  (t .+^ d, [(prevTime, t .-. prevTime, Nothing), (t, d, Just x)])
            | otherwise       =  error "addRests: Strange prevTime"
            
-- |
-- Given a set of absolute-time occurences, separate at each zero-time occurence.
-- Note that this require every bar to start with a zero-time occurence.
-- 
separateBars :: HasMusicXml a => Score a -> [[(Duration, Maybe a)]]
separateBars = fmap removeTime . fmap (fmap discardBarNumber) . splitAtTimeZero . fmap separateTime . getScore
    where  
        separateTime (t,d,x)            = ((bn,bt),d,x) where (bn,bt) = properFraction (toRational t)
        splitAtTimeZero                 = splitWhile ((== 0) . getBarTime) where getBarTime ((bn,bt),_,_) = bt
        discardBarNumber ((bn,bt),d,x)  = (fromRational bt, d, x)
        removeTime                      = fmap g where g (t,d,x) = (d,x)


barToXml :: HasMusicXml a => [(Duration, Maybe a)] -> Xml.Music
barToXml bar = case quantize bar of
    Left e   -> error $ "barToXml: Could not quantize this bar: " ++ show e
    Right rh -> rhythmToXml rh

rhythmToXml :: HasMusicXml a => Rhythm (Maybe a) -> Xml.Music
rhythmToXml (Beat d x)            = noteRestToXml (d, x)
rhythmToXml (Dotted n (Beat d x)) = noteRestToXml (dotMod n * d, x)
rhythmToXml (Tuplet m r)          = Xml.tuplet (fromIntegral $ denominator $ getDuration $ m) (fromIntegral $ numerator $ getDuration m) (rhythmToXml r)
rhythmToXml (Rhythms rs)          = mconcat $ map rhythmToXml rs

noteRestToXml :: HasMusicXml a => (Duration, Maybe a) -> Xml.Music
noteRestToXml (d, Nothing) = Xml.rest d' where d' = (fromRational . toRational $ d)   
noteRestToXml (d, Just p)  = getMusicXml d p

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

-- VoiceT


instance HasMidi a => HasMidi (VoiceT n a) where
    getMidi (VoiceT (_,x))          = getMidi x
instance HasMusicXml a => HasMusicXml (VoiceT n a) where
    getMusicXml d (VoiceT (_,x))    = getMusicXml d x
instance HasVoice (VoiceT n a) where   
    type Voice (VoiceT n a)      = n
    getVoice (VoiceT (v,_))      = v
    modifyVoice f (VoiceT (v,x)) = VoiceT (f v, x)
instance HasPitch a => HasPitch (VoiceT n a) where   
    type Pitch (VoiceT n a)      = Pitch a
    getPitch (VoiceT (v,a))      = getPitch a
    modifyPitch f (VoiceT (v,x)) = VoiceT (v, modifyPitch f x)
instance (IsPitch a, IsString n) => IsPitch (VoiceT n a) where
    fromPitch l                     = VoiceT ("", fromPitch l)
instance (IsDynamics a, IsString n) => IsDynamics (VoiceT n a) where
    fromDynamics l                  = VoiceT ("", fromDynamics l)
instance Tiable a => Tiable (VoiceT n a) where
    toTied (VoiceT (v,a)) = (VoiceT (v,b), VoiceT (v,c)) where (b,c) = toTied a
instance HasDynamic a => HasDynamic (VoiceT n a) where
    setBeginCresc n (VoiceT (v,x)) = VoiceT (v, setBeginCresc n x)
    setEndCresc   n (VoiceT (v,x)) = VoiceT (v, setEndCresc n x)
    setBeginDim   n (VoiceT (v,x)) = VoiceT (v, setBeginDim n x)
    setEndDim     n (VoiceT (v,x)) = VoiceT (v, setEndDim n x)
    setLevel      n (VoiceT (v,x)) = VoiceT (v, setLevel n x)
instance HasArticulation a => HasArticulation (VoiceT n a) where
    setEndSlur    n (VoiceT (v,x)) = VoiceT (v, setEndSlur n x)
    setContSlur   n (VoiceT (v,x)) = VoiceT (v, setContSlur n x)
    setBeginSlur  n (VoiceT (v,x)) = VoiceT (v, setBeginSlur n x)
    setAccLevel   n (VoiceT (v,x)) = VoiceT (v, setAccLevel n x)
    setStaccLevel n (VoiceT (v,x)) = VoiceT (v, setStaccLevel n x)
instance HasTremolo a => HasTremolo (VoiceT n a) where
    setTrem       n (VoiceT (v,x)) = VoiceT (v, setTrem n x)


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
instance HasVoice a => HasVoice (TieT a) where   
    type Voice (TieT a) = Voice a
    getVoice (TieT (_,x,_)) = getVoice x
    modifyVoice f (TieT (b,x,e)) = TieT (b,modifyVoice f x,e)
instance HasPitch a => HasPitch (TieT a) where   
    type Pitch (TieT a) = Pitch a
    getPitch (TieT (_,x,_)) = getPitch x
    modifyPitch f (TieT (b,x,e)) = TieT (b,modifyPitch f x,e)
instance IsPitch a => IsPitch (TieT a) where
    fromPitch l                     = TieT (False, fromPitch l, False)
instance IsDynamics a => IsDynamics (TieT a) where
    fromDynamics l                  = TieT (False, fromDynamics l, False)
instance HasDynamic a => HasDynamic (TieT a) where
    setBeginCresc n (TieT (b,x,e)) = TieT (b,setBeginCresc n x,e)
    setEndCresc   n (TieT (b,x,e)) = TieT (b,setEndCresc n x,e)
    setBeginDim   n (TieT (b,x,e)) = TieT (b,setBeginDim n x,e)
    setEndDim     n (TieT (b,x,e)) = TieT (b,setEndDim n x,e)
    setLevel      n (TieT (b,x,e)) = TieT (b,setLevel n x,e)
instance HasArticulation a => HasArticulation (TieT a) where
    setEndSlur    n (TieT (b,x,e)) = TieT (b,setEndSlur n x,e)
    setContSlur   n (TieT (b,x,e)) = TieT (b,setContSlur n x,e)
    setBeginSlur  n (TieT (b,x,e)) = TieT (b,setBeginSlur n x,e)
    setAccLevel   n (TieT (b,x,e)) = TieT (b,setAccLevel n x,e)
    setStaccLevel n (TieT (b,x,e)) = TieT (b,setStaccLevel n x,e)
instance HasTremolo a => HasTremolo (TieT a) where
    setTrem       n (TieT (b,x,e)) = TieT (b,setTrem n x,e)


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
    toTied (DynamicT (ec,ed,l,a,bc,bd))         = (DynamicT (ec,ed,l,b,bc,bd),
                                                   DynamicT (False,False,Nothing,c,False,False)) where (b,c) = toTied a
instance HasVoice a => HasVoice (DynamicT a) where   
    type Voice (DynamicT a)                     = Voice a
    getVoice (DynamicT (ec,ed,l,a,bc,bd))       = getVoice a
    modifyVoice f (DynamicT (ec,ed,l,a,bc,bd))  = DynamicT (ec,ed,l,modifyVoice f a,bc,bd)
instance HasPitch a => HasPitch (DynamicT a) where   
    type Pitch (DynamicT a)                     = Pitch a
    getPitch (DynamicT (ec,ed,l,a,bc,bd))       = getPitch a
    modifyPitch f (DynamicT (ec,ed,l,a,bc,bd))  = DynamicT (ec,ed,l,modifyPitch f a,bc,bd)
instance IsPitch a => IsPitch (DynamicT a) where
    fromPitch l                                 = DynamicT (False,False,Nothing,fromPitch l,False,False)
instance IsDynamics a => IsDynamics (DynamicT a) where
    fromDynamics l                              = DynamicT (False,False,Nothing,fromDynamics l,False,False)
instance HasDynamic (DynamicT a) where
    setBeginCresc bc (DynamicT (ec,ed,l,a,_ ,bd)) = DynamicT (ec,ed,l,a,bc,bd)
    setEndCresc   ec (DynamicT (_ ,ed,l,a,bc,bd)) = DynamicT (ec,ed,l,a,bc,bd)
    setBeginDim   bd (DynamicT (ec,ed,l,a,bc,_ )) = DynamicT (ec,ed,l,a,bc,bd)
    setEndDim     ed (DynamicT (ec,_ ,l,a,bc,bd)) = DynamicT (ec,ed,l,a,bc,bd)
    setLevel      l  (DynamicT (ec,ed,_,a,bc,bd)) = DynamicT (ec,ed,Just l,a,bc,bd)
instance HasArticulation a => HasArticulation (DynamicT a) where
    setEndSlur    n (DynamicT (ec,ed,l,a,bc,bd)) = DynamicT (ec,ed,l,setEndSlur n a,bc,bd)
    setContSlur   n (DynamicT (ec,ed,l,a,bc,bd)) = DynamicT (ec,ed,l,setContSlur n a,bc,bd)
    setBeginSlur  n (DynamicT (ec,ed,l,a,bc,bd)) = DynamicT (ec,ed,l,setBeginSlur n a,bc,bd)
    setAccLevel   n (DynamicT (ec,ed,l,a,bc,bd)) = DynamicT (ec,ed,l,setAccLevel n a,bc,bd)
    setStaccLevel n (DynamicT (ec,ed,l,a,bc,bd)) = DynamicT (ec,ed,l,setStaccLevel n a,bc,bd)
instance HasTremolo a => HasTremolo (DynamicT a) where
    setTrem    n (DynamicT (ec,ed,l,a,bc,bd)) = DynamicT (ec,ed,l,setTrem n a,bc,bd)


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
instance HasVoice a => HasVoice (ArticulationT a) where   
    type Voice (ArticulationT a)                        = Voice a
    getVoice (ArticulationT (es,us,al,sl,a,bs))         = getVoice a
    modifyVoice f (ArticulationT (es,us,al,sl,a,bs))    = ArticulationT (es,us,al,sl,modifyVoice f a,bs)
instance HasPitch a => HasPitch (ArticulationT a) where   
    type Pitch (ArticulationT a)                        = Pitch a
    getPitch (ArticulationT (es,us,al,sl,a,bs))         = getPitch a
    modifyPitch f (ArticulationT (es,us,al,sl,a,bs))    = ArticulationT (es,us,al,sl,modifyPitch f a,bs)
instance IsPitch a => IsPitch (ArticulationT a) where
    fromPitch l                                         = ArticulationT (False,False,0,0,fromPitch l,False)
instance IsDynamics a => IsDynamics (ArticulationT a) where
    fromDynamics l                                      = ArticulationT (False,False,0,0,fromDynamics l,False)
instance HasDynamic a => HasDynamic (ArticulationT a) where
    setBeginCresc n (ArticulationT (es,us,al,sl,a,bs)) = ArticulationT (es,us,al,sl,setBeginCresc n a,bs)
    setEndCresc   n (ArticulationT (es,us,al,sl,a,bs)) = ArticulationT (es,us,al,sl,setEndCresc n a,bs)
    setBeginDim   n (ArticulationT (es,us,al,sl,a,bs)) = ArticulationT (es,us,al,sl,setBeginDim n a,bs)
    setEndDim     n (ArticulationT (es,us,al,sl,a,bs)) = ArticulationT (es,us,al,sl,setEndDim n a,bs)
    setLevel      n (ArticulationT (es,us,al,sl,a,bs)) = ArticulationT (es,us,al,sl,setLevel n a,bs)
instance HasArticulation (ArticulationT a) where
    setEndSlur    es (ArticulationT (_ ,us,al,sl,a,bs)) = ArticulationT (es,us,al,sl,a,bs)
    setContSlur   us (ArticulationT (es,_ ,al,sl,a,bs)) = ArticulationT (es,us,al,sl,a,bs)
    setBeginSlur  bs (ArticulationT (es,us,al,sl,a,_ )) = ArticulationT (es,us,al,sl,a,bs)
    setAccLevel   al (ArticulationT (es,us,_ ,sl,a,bs)) = ArticulationT (es,us,al,sl,a,bs)
    setStaccLevel sl (ArticulationT (es,us,al,_ ,a,bs)) = ArticulationT (es,us,al,sl,a,bs)
instance HasTremolo a => HasTremolo (ArticulationT a) where
    setTrem n (ArticulationT (es,us,al,sl,a,bs)) = ArticulationT (es,us,al,sl,setTrem n a,bs)

 
-- TremoloT

-- newtype TremoloT a = TremoloT { getTremoloT :: (Int, a) }

instance HasMidi a => HasMidi (TremoloT a) where
    getMidi (TremoloT (_,x))        = getMidi x
instance HasMusicXml a => HasMusicXml (TremoloT a) where
    getMusicXml d (TremoloT (n,x))  = notate $ getMusicXml d x
        where
            notate = Xml.tremolo n
            
instance Tiable a => Tiable (TremoloT a) where
    toTied (TremoloT (n,a))         = (TremoloT (n,b), TremoloT (n,c)) where (b,c) = toTied a
instance HasVoice a => HasVoice (TremoloT a) where   
    type Voice (TremoloT a)         = Voice a
    getVoice (TremoloT (_,a))       = getVoice a
    modifyVoice f (TremoloT (n,x))  = TremoloT (n, modifyVoice f x)
instance HasPitch a => HasPitch (TremoloT a) where   
    type Pitch (TremoloT a)         = Pitch a
    getPitch (TremoloT (_,a))       = getPitch a
    modifyPitch f (TremoloT (n,x))  = TremoloT (n, modifyPitch f x)
instance IsPitch a => IsPitch (TremoloT a) where
    fromPitch l                     = TremoloT (0, fromPitch l)
instance IsDynamics a => IsDynamics (TremoloT a) where
    fromDynamics l                  = TremoloT (0, fromDynamics l)
instance HasDynamic a => HasDynamic (TremoloT a) where
    setBeginCresc n (TremoloT (v,x)) = TremoloT (v, setBeginCresc n x)
    setEndCresc   n (TremoloT (v,x)) = TremoloT (v, setEndCresc n x)
    setBeginDim   n (TremoloT (v,x)) = TremoloT (v, setBeginDim n x)
    setEndDim     n (TremoloT (v,x)) = TremoloT (v, setEndDim n x)
    setLevel      n (TremoloT (v,x)) = TremoloT (v, setLevel n x)
instance HasArticulation a => HasArticulation (TremoloT a) where
    setEndSlur    n (TremoloT (v,x)) = TremoloT (v, setEndSlur n x)
    setContSlur   n (TremoloT (v,x)) = TremoloT (v, setContSlur n x)
    setBeginSlur  n (TremoloT (v,x)) = TremoloT (v, setBeginSlur n x)
    setAccLevel   n (TremoloT (v,x)) = TremoloT (v, setAccLevel n x)
    setStaccLevel n (TremoloT (v,x)) = TremoloT (v, setStaccLevel n x)
instance HasTremolo (TremoloT a) where
    setTrem      n (TremoloT (_,x)) = TremoloT (n,x)

                                     
                                                                                                           
-------------------------------------------------------------------------------------
-- Test stuff
-------------------------------------------------------------------------------------


type Fun  a = a -> a
type Sc   a = Score (VoiceT VoiceName (TieT (TremoloT (DynamicT (ArticulationT a)))))

sc :: Fun (Sc Double)
sc = id

fj1 = sc $ melody [c,d] |> melody [eb,d]^/2 |> c
fj2 = sc $ melody [eb,f] |> g^*2
fj3 = sc $ g^*(3/4) |> ab^*(1/4) |> melody [g,f,eb,d] ^/2 |> c
fj4 = c |> g_ |> c^*2

fj  = rep 2 fj1 |> rep 2 fj2 |> rep 2 fj3 |> rep 2 fj4

fj' = mempty
    <> setVoices "Violin I"     (rep 10 fj) 
    <> setVoices "Violin II"    (delay 8  $ (rep 10 fj)^*(2/3)) 
    <> setVoices "Viola"        (delay 16 $ (rep 10 fj)^*(4/5)) 
    <> setVoices "Violoncello"  (delay 24 $ (rep 10 fj)^*(4/7))

-- classic version...
fj'' = mempty
    <> setVoices "Violin I"     (rep 10 fj) 
    <> setVoices "Violin II"    (delay 8  $ (rep 10 fj)) 
    <> setVoices "Viola"        (delay 16 $ (rep 10 fj)) 
    <> setVoices "Violoncello"  (delay 24 $ (rep 10 fj))


testArtDyn = mempty
    <> v (0+1) (rep 80 t^*(3*1))
    <> v (0+2) (rep 80 t^*(4*1)) 
    <> v (0+3) (rep 80 t^*(5*1))
    <> v (0+4) (rep 80 t^*(7*1))
    <> v (4+1) (rep 20 s^*(3*2))
    <> v (4+2) (rep 20 s^*(4*2)) 
    <> v (4+3) (rep 20 s^*(5*2))
    <> v (4+4) (rep 20 s^*(7*2))
    where
        v x = setVoices (VoiceName $ "Violin I." ++ show x)
        s, t :: Sc Double
        s = mempty
            |> (fmap (setLevel (-2.5) . setBeginSlur True . setBeginCresc True) c) 
            |> d 
            |> f 
            |> (fmap (setEndSlur True . setLevel (0.5) . setEndCresc True   ) e)
        t = mempty
            |> (fmap (setLevel (-2.5) . setBeginSlur True . setBeginCresc True) g) 
            |> a 
            |> bb 
            |> (fmap (setEndSlur True . setLevel (0.5) . setEndCresc True   ) a)

showScore :: Score Double -> String
showScore = show

play = playMidiIO . (^* (60/100)) . sc
open = openXml . (^* (1/4)) . sc

rep 0 x = mempty
rep n x = x |> rep (n-1) x

-- Basic literal instances
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
instance IsDynamics Double where
    fromDynamics (DynamicsL (Just x, _)) = x
    fromDynamics (DynamicsL (Nothing, _)) = error "IsDynamics Double: No dynamics"



-------------------------------------------------------------------------------------

list z f [] = z
list z f xs = f xs

first f (x,y)  = (f x, y)
second f (x,y) = (x, f y)

sep :: a -> [a] -> [a]
sep = List.intersperse

concatSep :: [a] -> [[a]] -> [a]
concatSep x = List.concat . sep x

-- Score filtering generalized to MonadPlus (TODO move!)

-- | 
-- Generalizes the 'remove' function.
-- 
mremove :: MonadPlus m => (a -> Bool) -> m a -> m a
mremove p = mfilter (not . p)

-- | 
-- Generalizes the 'partition' function.
-- 
mpartition :: MonadPlus m => (a -> Bool) -> m a -> (m a, m a)
mpartition p a = (mfilter p a, mremove p a)

-- | 
-- Pass through @Just@ occurrences.
-- Generalizes the 'catMaybes' function.
-- 
mcatMaybes :: MonadPlus m => m (Maybe a) -> m a
mcatMaybes = (>>= maybe mzero return)

-- | 
-- Modify or discard a value.
-- Generalizes the 'mapMaybe' function.
-- 
mmapMaybe :: MonadPlus m => (a -> Maybe b) -> m a -> m b
mmapMaybe f = mcatMaybes . liftM f 

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

