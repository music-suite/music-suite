
{-# LANGUAGE
    TypeFamilies,
    GeneralizedNewtypeDeriving,
    DeriveFunctor,
    DeriveFoldable,     
    ScopedTypeVariables,
    NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides a musical score represenation.
--
-- An value can have a negative onset, representing a values occuring before the
-- reference time. Durations must be positive, allthough the types does not
-- enforce this.
--
-- An value is either a /note/ or a /rest/ (represented by the 'Just' and 'Nothing'
-- constructors). Having explicit rests allow us to concatenate scores based on
-- duration even if the score is actually empty.
--
-- Scores allow overlapping values, but can be split into parts, which does not.
--
-------------------------------------------------------------------------------------

module Music.Score (

        -- * Basic types
        -- ** Voice
        Voice(..),

        -- ** Time and duration
        Time(..),
        Duration(..),

        HasDuration(..),
        HasOnset(..),
        offset,

        -- * Delayable class
        Delayable(..),

        -- * Performable class
        Performable(..),

        -- * Track type
        Track(..),
        -- * Part type
        Part(..),
        -- * Score type
        Score(..),

        -- ** Constructors
        rest,
        note,
        chord,
        melody,
        chords,
        melodies,
        -- chordDelay,
        -- melodyStretch,
        -- chordDelayStretch,

        -- ** Transforming
        delay,
        stretch,
        startAt,
        stopAt,
        compress,
        stretchTo,        

        -- ** Composing
        (|>),
        (<|),
        scat,
        pcat,
        -- (<||),
        -- (||>),
        -- (<<|),
        -- (|>>),
        -- sustain,
        -- overlap,
        -- prolong,
        -- anticipate,
        
        -- ** Decomposing
        voices,
        numVoices,
        setVoice,
        
        -- * Utility         
        -- ** MIDI export
        HasMidi(..),
        toMidi,
        writeMidi,
        playMidi,
        playMidiIO,

        -- ** MusicXML export
        XmlScore,
        XmlMusic,
        HasXml(..),
        toXml,
        writeXml,
        openXml
)
where

{-
    TODO
        Monoid instance for Part is strange
        Can be bad if used with |> etc, we should add some constraint to prevent this
        Delayable?
            'delay' is really similar to (.+^)
            But (.-.) does not really make sense, or does it?
            We could take differnce in onset, but that would require HasOnset
       Split and reverse
       Zipper applicative (for appying dynamic and articulation transformations etc)
            
-}  

import Prelude hiding (foldr, concat, foldl, mapM, concatMap, maximum, sum, minimum)

import Data.Semigroup
import Data.Ratio
import Control.Applicative
import Control.Monad (ap, join, MonadPlus(..))
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Traversable
import Data.Function (on)
import Data.Ord (comparing)

import Data.VectorSpace
import Data.AffineSpace
import Data.Basis

import Data.Functor.Identity
import Text.Parsec hiding ((<|>))
import Text.Parsec.Pos

import System.Posix -- debug
import System.IO

import Music.Pitch.Literal
import Music.Dynamics.Literal

{-
import Music.Imitator.Reactive
import Music.Imitator.Reactive.Midi
-}

import qualified Codec.Midi as Midi
import qualified Music.MusicXml.Simple as Xml
import qualified Data.Map as Map
import qualified Data.List as List


-------------------------------------------------------------------------------------
-- Voice
-------------------------------------------------------------------------------------

newtype Voice = Voice { getVoice::Int }
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral)


-------------------------------------------------------------------------------------
-- Time and duration
-------------------------------------------------------------------------------------

newtype Time = Time { getTime::Rational }
    deriving (Eq, Ord, {-Show, -}Num, Enum, Real, Fractional, RealFrac)
    -- Note: no Floating as we want to be able to switch to rational

newtype Duration = Duration { getDuration::Rational }                                  
    deriving (Eq, Ord, {-Show, -}Num, Enum, Real, Fractional, RealFrac)
    -- Note: no Floating as we want to be able to switch to rational

-- TODO for debugging
instance Show Time where show = show . getTime
instance Show Duration where show = show . getDuration

instance AdditiveGroup Time where
    zeroV = 0
    (^+^) = (+)
    negateV = negate

instance VectorSpace Time where
    type Scalar Time = Time
    (*^) = (*)

instance InnerSpace Time where (<.>) = (*)

instance  AffineSpace Time where
    type Diff Time = Duration
    a .-. b =  t2d $ a - b      where t2d = Duration . getTime
    a .+^ b =  a + d2t b        where d2t = Time . getDuration

instance AdditiveGroup Duration where
    zeroV = 0
    (^+^) = (+)
    negateV = negate

instance VectorSpace Duration where
    type Scalar Duration = Duration
    (*^) = (*)

instance InnerSpace Duration where (<.>) = (*)


class HasDuration a where
    duration :: a -> Duration

class HasOnset a where
    onset :: a -> Time

-- |
-- > offset x = onset x + duration x
-- 
offset :: (HasOnset a, HasDuration a) => a -> Time
offset x = onset x .+^ duration x


-------------------------------------------------------------------------------------
-- Delayable class
-------------------------------------------------------------------------------------

class Delayable a where

    -- |
    -- Delay a score.
    -- > Duration -> Score a -> Score a
    -- 
    delay :: Duration -> a -> a

-- |
-- Move a score to start at a specific time.
-- 
-- > Duration -> Score a -> Score a
-- 
t `startAt` x = delay d x where d = t .-. onset x

-- |
-- Move a score to stop at a specific time.
-- 
-- > Duration -> Score a -> Score a
-- 
t `stopAt`  x = delay d x where d = t .-. offset x


-------------------------------------------------------------------------------------
-- Performable class
-------------------------------------------------------------------------------------

class Performable f where
    -- |
    -- Render as a list of @(voice, time, duration, value)@ tuples.
    --
    -- Time since reference time.
    --
    perform  :: f a -> [(Voice, Time, Duration, a)]
    
    -- |
    -- Render as a list of @(voice, time, duration, value)@ tuples.
    --
    -- Time since last event.
    --
    performRelative :: f a -> [(Voice, Time, Duration, a)]
    -- TODO split and remerge parts...
    performRelative = toRel . perform
        where
            toRel = snd . mapAccumL g 0
            g now (v,t,d,x) = (t, (v,t-now,d,x))


-------------------------------------------------------------------------------------
-- Track type
-------------------------------------------------------------------------------------

-- |
-- A track is a list of absolute-time occurences.
--
-- Track is a 'Monoid' under parallel compisiton. 'mempty' is the empty track and 'mappend'
-- interleaves values.
--
-- Track has an 'Applicative' instance derived from the 'Monad' instance.
--
-- Track is a 'Monad'. 'return' creates a track containing a single value at time
-- zero, and '>>=' transforms the values of a track, allowing the addition and
-- removal of values relative to the time of the value. More intuitively,
-- 'join' delays each inner track to start at the offset of an outer track, then
-- removes the intermediate structure. 
--
-- > let s = Track [(0,65),(1,66)] 
-- > t >>= \x -> Track [(0,'a'),(10,toEnum x)]
-- >   ==> Track {getTrack = [(0.0,'a'),(1.0,'a'),(10.0,'A'),(11.0,'B')]}
--
-- Track is an instance of 'VectorSpace' using parallel composition as addition, 
-- and time scaling as scalar multiplication.
--
newtype Track a = Track { getTrack :: [(Time, a)] }
    deriving (Eq, Ord, Show, Functor, Foldable)

instance Semigroup (Track a) where
    (<>) = mappend

instance Monoid (Track a) where
    mempty = Track []
    Track as `mappend` Track bs = Track (as `merge` bs)
        where
            as `merge` bs = List.sortBy (comparing fst) $ as <> bs

instance Applicative Track where
    pure  = return
    (<*>) = ap

instance Monad Track where
    return a = Track [(0, a)]
    a >>= k = join' . fmap k $ a
        where
            join' (Track tts) = mconcat . fmap (\(t,tr) -> delay (t .-. 0) tr) $ tts

instance Alternative Track where
    empty = mempty
    (<|>) = mappend

-- Satisfies left distribution
instance MonadPlus Track where
    mzero = mempty
    mplus = mappend

instance AdditiveGroup (Track a) where
    zeroV   = mempty
    (^+^)   = mappend
    negateV = id

instance VectorSpace (Track a) where
    type Scalar (Track a) = Time
    n *^ tr = Track . (fmap (first (n*^))) . getTrack $ tr

instance Delayable (Track a) where
    d `delay` tr = Track . fmap (first (.+^ d)) . getTrack $ tr

instance HasOnset (Track a) where
    onset (Track []) = 0
    onset (Track as) = fst . head $ as


-------------------------------------------------------------------------------------
-- Part type
-------------------------------------------------------------------------------------

-- |
-- A part is a list of relative-time notes and rests.
--
-- Part is a 'Monoid' under sequential compisiton. 'mempty' is the empty part and 'mappend'
-- appends parts.
--
-- Part is an instance of 'VectorSpace' using parallel composition as addition, 
-- and time scaling as scalar multiplication.
--
newtype Part a = Part { getPart :: [(Duration, Maybe a)] }
    deriving (Eq, Ord, Show, Functor, Foldable, Monoid)

instance Semigroup (Part a) where
    (<>) = mappend

instance Applicative Part where
    pure  = return
    (<*>) = ap

instance Monad Part where
    return a = Part [(1, Just a)]
    a >>= k = join' $ fmap k a
        where
            join' (Part pps) = mconcat . fmap (\(d, p) -> maybe mempty (d *^) p) $ pps
--    -- TODO divide by sum of duration?

instance AdditiveGroup (Part a) where
    zeroV   = mempty
    (^+^)   = mappend
    negateV = id

instance VectorSpace (Part a) where
    type Scalar (Part a) = Duration
    n *^ Part as = Part (fmap (first (n*^)) as)

instance Delayable (Part a) where
    t `delay` (Part as) = Part $ (t, Nothing) : as

instance HasDuration (Part a) where
    duration (Part []) = 0
    duration (Part as) = sum . fmap fst $ as

-- instance HasOnset (Part a) where
--     onset (Part []) = Nothing
--     onset (Part xs) = Just $ sum $ fmap fst $ takeWhile isRest $ xs
--         where
--             isRest (_,Nothing) = True
--             isRest (_,Just _)  = False


-------------------------------------------------------------------------------------
-- Score type
-------------------------------------------------------------------------------------

-- |
-- A score is list of parts.
--
-- Score is a 'Monoid' under parallel compisiton. 'mempty' is a score of no parts.
-- For sequential composition of scores, use '|>'.
--
-- Score has an 'Applicative' instance derived from the 'Monad' instance. Not sure it is useful.
--
-- Score is a 'Monad'. 'return' creates a score containing a single note of
-- duration one, and '>>=' transforms the values of a score, while allowing
-- transformations of time and duration. More intuitively, 'join' scales and
-- offsets each inner score to fit into an outer score, then removes the intermediate
-- structure. 
--
-- 'Score' is an instance of 'VectorSpace' and 'HasBasis' using parallel
-- composition as addition, time scaling as scalar multiplication and rests of
-- duration 0 and 1 for 'zeroV' and 'basisValue' respectively. The values of each
-- part has a separate basis, so 'decompose' separates parts.
--
newtype Score a  = Score { getScore :: [(Voice, Part a)] }
    deriving (Show, Functor, Foldable)

instance Eq a => Eq (Score a) where
    a == b = perform a == perform b

instance Ord a => Ord (Score a) where
    a `compare` b = perform a `compare` perform b

instance Semigroup (Score a) where
    (<>) = mappend

instance Monoid (Score a) where
    mempty  = Score mempty
    Score as `mappend` Score bs = Score (as `mappend` bs)

instance Applicative Score where
    pure  = return
    (<*>) = ap

instance Alternative Score where
    empty = mempty
    (<|>) = mappend

-- Satisfies left distribution
instance MonadPlus Score where
    mzero = mempty
    mplus = mappend

instance Monad Score where
    return = note
    a >>= k = join' $ fmap k a
        where
            join' (Score xs) = pcat $ pcat $ fmap (g . f) $ xs
                where
                    -- g :: Part (Score a)  -> [Score a]
                    g = toList . mapWithTimeDur (\t d -> delay t . stretch d)

                    -- f :: (Voice, Part (Score a))  -> Part (Score a)
                    f = uncurry fmap . first setVoice

instance AdditiveGroup (Score a) where
    zeroV   = mempty
    (^+^)   = mappend
    negateV = id

instance VectorSpace (Score a) where
    type Scalar (Score a) = Duration
    n *^ Score xs = Score (fmap (second (n*^)) xs)

instance Delayable (Score a) where
    delay t (Score xs) = Score (fmap (second (delay t)) xs)

-- Experimental instance. We want to do something more sophisticated with parts later on.
instance HasBasis (Score a) where
    type Basis (Score a) = Voice
    basisValue p = Score [(p, Part [(1, Nothing)])]
    decompose' s = fromJust . (flip lookup) (decompose s)
    decompose    = fmap (second duration) . getScore

instance HasDuration (Score a) where
    duration (Score as) = maximum $ fmap (duration . snd) $ as

instance IsPitch a => IsPitch (Score a) where
    fromPitch = pure . fromPitch

instance IsDynamics a => IsDynamics (Score a) where
    fromDynamics = pure . fromDynamics

instance Performable Score where
    perform (Score ps) = List.sortBy (comparing snd4) $ concatMap (uncurry gatherPart) ps
        where            
            gatherPart :: Voice -> Part a -> [(Voice, Time, Duration, a)]
            gatherPart v = toList . fmap (second4 d2t). mapWithTimeDur ((,,,) v)
            second4 f (a,b,c,d) = (a,f b,c,d)
            snd4 (a,b,c,d) = b
            d2t = Time . getDuration
            

-------------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------------

-- |
-- Create a score of duration 1 with no values.
--
rest :: Score a
rest = Score [(0, Part [(1, Nothing)])]

-- |
-- Create a score of duration 1 with the given value.
--
-- Equivalent to 'pure' and 'return'.
--
note :: a -> Score a
note x = Score [(0, Part [(1, Just x)])]

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
-- Stretch a score. Equivalent to '*^'.
-- 
-- > Duration -> Score a -> Score a
-- 
stretch :: VectorSpace v => Scalar v -> v -> v
stretch = (*^)

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
-- To compose in parallel, use '<|>' or '<>'.
--
-- > Score a -> Score a -> Score a
(|>) :: (Semigroup a, Delayable a, HasDuration a) => a -> a -> a
a |> b = a <> delay (duration a) b

-- |
-- Compose in reverse sequence. 
--
-- To compose in parallel, use '<|>' or '<>'.
--
-- > Score a -> Score a -> Score a
(<|) :: (Semigroup a, Delayable a, HasDuration a) => a -> a -> a
a <| b = delay (duration b) a <> b

-- |
-- Sequential concatentation.
--
-- > [Score t] -> Score t
scat :: (Monoid a, Semigroup a, Delayable a, HasDuration a) => [a] -> a
scat = foldr (|>) mempty

-- |
-- Parallel concatentation. Identical to 'mconcat'.
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
anticipate :: (Semigroup a, Delayable a, HasDuration a) => Duration -> a -> a -> a
anticipate t x y = x |> delay t' y where t' = (duration x - t) `max` 0





-------------------------------------------------------------------------------------
-- Decomposition
-------------------------------------------------------------------------------------

-- | 
-- Returns the voices in the given score.
--
voices :: Score a -> [Voice]
voices = fmap fst . decompose

-- | 
-- Returns the number of voices in the given score.
--
numVoices :: Score a -> Int
numVoices = length . voices
 

-------------------------------------------------------------------------------------

-- |
-- Class of types that can be converted to MIDI scores.
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

instance HasMidi Midi.Message where
    getMidi = return

instance HasMidi ()                      where   getMidi = getMidi . toInteger . const 60
instance HasMidi Double                  where   getMidi = getMidi . toInteger . round
instance HasMidi Int                     where   getMidi = getMidi . toInteger    
instance Integral a => HasMidi (Ratio a) where   getMidi = getMidi . toInteger . round    

instance HasMidi Integer where
    getMidi x = note (Midi.NoteOn 0 (fromIntegral x) 100) |> note (Midi.NoteOff 0 (fromIntegral x) 0)

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
        events          = (\(_,t,_,x) -> (round (t * divisions), x)) <$> performance

        performance :: [(Voice, Time, Duration, Midi.Message)]
        performance     = performRelative (getMidi =<< score)

        -- FIXME arbitrary endTime (files won't work without this...)
        -- TODO handle voice

-- |
-- Convert a score to a MIDI file and write to disc.
--    
writeMidi :: HasMidi a => FilePath -> Score a -> IO ()
writeMidi path sc = Midi.exportFile path (toMidi sc)

-- |
-- Convert a score to a MIDI event.
--    
playMidi :: HasMidi a => Score a -> ()
playMidi = error "Can not use Reactivity from music-score yet..."
{-
playMidi :: HasMidi a => Score a -> Event MidiMessage
playMidi x = midiOut midiDest $ playback trig (pure $ toTrack $ rest |> x)
    where
        trig        = accumR 0 ((+ 0.01) <$ pulse 0.01)        
        toTrack     = fmap (\(v,t,_,m) -> (t,m)) . perform . (getMidi =<<)
        midiDest    = fromJust $ unsafeGetReactive (findDestination  $ pure "Graphic MIDI")
        -- FIXME hardcoded output...
-}

-- |
-- Convert a score to a MIDI event and run it.
--    
playMidiIO :: HasMidi a => Score a -> IO ()
playMidiIO = error "Can not use Reactivity from music-score yet..."
{-
playMidiIO :: HasMidi a => Score a -> IO ()
playMidiIO = runEvent . playMidi
-}

        


-------------------------------------------------------------------------------------

type XmlScore = Xml.Score
type XmlMusic = Xml.Music

class HasXml a where
   getXml :: Voice -> Duration -> a -> XmlMusic

instance HasXml Xml.Score where
--     toXml = return
instance HasXml Double where
--     toXml = toXml . toInteger . round
instance HasXml Integer where
--     toXml x = undefined   
instance HasXml () where
--     toXml x = undefined   

toXml :: Score a -> XmlScore
toXml = Xml.fromPart "Title" "Composer" "Part" . fmap translBar . separateBars . perform

separateBars :: [(Voice, Time, Duration, a)] -> [[(Voice, Duration, a)]]
separateBars = fmap (fmap discardTime) . splitAtTimeZero . map separateTime
    where  
        discardTime (v,_,_,d,x) = (v,d,x)              
        
        splitAtTimeZero = splitWhile ((== 0) . getBarTime)
        
        separateTime (v,t,d,x) = (v,bn,bt,d,x) 
            where (bn,bt) = properFraction (toRational t)
        
        getBarTime (_,_,bt,_,_) = bt
        -- FIXME assumes bar length of one
        -- FIXME must include rests. How? Can we define a separate performRests?
        -- FIXME ties?

-- translate one bar
translBar :: [(Voice, Duration, a)] -> Xml.Music
translBar = mconcat . fmap translNote
    -- FIXME find tuplets




data Rhythm a 
    = RBeat       Duration a                    -- d is divisible by 2
    | RDotted     (Rhythm a) (Rhythm a)
    | RRevDotted  (Rhythm a) (Rhythm a)
    | RTuplet     Duration (Rhythm a)           -- d is 2/3, 4/5, 4/6, 4/7, 8/9, 8/10, 8/11 ...
    | RInvTuplet  Duration (Rhythm a)           -- d is 3/2,      6/4
    | RSeq        [Rhythm a]                    
    deriving (Eq, Show)

quantizeVoice :: Int -> Score a -> Either String (Rhythm (Maybe a))
quantizeVoice n = quantize . getPart . snd . (!! n) . getScore 

quantize :: [(Duration, a)] -> Either String (Rhythm a)
quantize = quantize' rhythm


-- A RhytmParser can convert (Part a) to b 
type RhythmParser a b = Parsec [(Duration, a)] () b

quantize' :: RhythmParser a b -> [(Duration, a)] -> Either String b
quantize' p = left show . runParser p () ""

-- pt :: Show a => RhythmParser b a -> [(Duration, b)] -> IO ()
-- pt = parseTest


match :: (Duration -> a -> Bool) -> RhythmParser a (Rhythm a)
match p = tokenPrim show next test
    where
        show x        = ""
        next pos _ _  = updatePosChar pos 'x'
        test (d,x)    = if p d x then Just (RBeat d x) else Nothing

    
rhythm :: RhythmParser a (Rhythm a)
rhythm = mzero
    <|> rseq

beat :: RhythmParser a (Rhythm a)
beat = match (const . isDivisibleBy 2)

-- dotted
-- rev dotted
-- tuplet

rseq :: RhythmParser a (Rhythm a)
rseq = RSeq <$> Text.Parsec.many1 beat



{-

-- > sum (syncopate a) == a
syncopate :: Duration -> Rhythm
syncopate d = [d/4, d/2, d/4]

dot :: Duration -> Rhythm
dot d = [d*3/4, d/4]

revDot :: Duration -> Rhythm
revDot d = [d/4, d*3/4]

dotSafe :: Duration -> Maybe Rhythm
dotSafe d | isDivisibleBy 2 d = Just (dot d)
          | otherwise         = Nothing

revDotSafe :: Duration -> Maybe Rhythm
revDotSafe d | isDivisibleBy 2 d = Just (revDot d)
             | otherwise         = Nothing

          
-- > sum (divideBy n a) == a
divideBy :: Int -> Duration -> Rhythm
divideBy n d = replicate n (d/fromIntegral n)


-- 
-- combinations [[1,2],[3,4]] ==> [1,3],[1,4],[2,3],[2,4]
combinations :: [[a]] -> [[a]]
combinations = sequenceA


-- iterateBounded :: ([a] -> [a]) -> a -> [a]
-- iterateBounded = iterate 

makeAll :: Rhythm -> [Rhythm]                                                                  
makeAll = (!! 2) . (List.reverse . List.nub) . iterate ({-reverse . -}List.sort . concat . traverse divideRhythm) . single

--notTooSmall = List.all (> (1/4))
--concatWhile f = concat . takeWhile f

divideRhythm :: Rhythm -> [Rhythm]
divideRhythm rs = fmap concat $ sequenceA $ alts
    where               
        alts :: [[Rhythm]]
        alts = divideDur <$> rs

divideDur :: Duration -> [Rhythm]
divideDur ds = ks <*> [ds]
    where               
        ks = [  
                single,
                divideBy 2,
                syncopate
                -- dotSafe,     TODO must use safe versions here
                -- revDotSafe,
            ]
                                     -}


testRhSc :: Score ()
testRhSc = (^/1) $ undefined
    where           
        -- r2s :: Rhythm -> Score ()
        -- r2s = scat . fmap (\d -> (note ())^*d)








translNote :: (Voice, Duration, a) -> Xml.Music
translNote (v,d,p) = Xml.note p' d'
    where
        p' = (toEnum 0, Nothing, 4)
        d' = (fromRational . toRational $ d)


writeXml :: HasXml a => FilePath -> Score a -> IO ()
writeXml path sc = writeFile path (Xml.showXml $ toXml sc)

openXml :: HasXml a => Score a -> IO ()
openXml sc = do
    writeXml "test.xml" sc
    execute "open" ["-a", "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6", "test.xml"]
    
sc :: Score Double -> Score Double
sc = id
                                                                                                           
-------------------------------------------------------------------------------------
-- Test stuff
-------------------------------------------------------------------------------------


score :: Score a -> Score a
score = id

prettyScore :: Show a => Score a -> String
prettyScore = concatSep " <> " . fmap (\(p,x) -> "("++prettyPart x++")") . getScore

prettyPart :: Show a => Part a -> String
prettyPart = concatSep " |> " . fmap (\(d,x) -> prettyDur d ++ "*^"++ maybe "rest" prettyVal x) . getPart
    where                                                                                   
        prettyVal x = "pure " ++ show x
        prettyDur (Duration d) = "("++show (numerator d) ++ "/" ++ show (denominator d)++")"

showScore :: Score Double -> String
showScore = show

ssm :: Score Midi.Message -> IO [()]
ssm = mapM putStrLn . fmap show . perform

ssd :: Score Double -> IO [()]
ssd = mapM putStrLn . fmap show . perform

spanien :: Score Double
spanien = (c^*3 |> d |> e^*3 |> d |> c^*2 |> g_^*2 |> g_^*4)^/8

rep 0 x = mempty
rep n x = x |> rep (n-1) x


openSib :: Xml.Score -> IO ()
openSib score =
    do  writeFile "test.xml" (Xml.showXml score)
        execute "open" ["-a", "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6", "test.xml"]

openTim :: HasMidi a => Score a -> IO ()
openTim sc = do
    Midi.exportFile "test.mid" (toMidi sc)
    execute "timidity" ["test.mid"]

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



















-- Some accumulators etc. Internal for now.

-- | Accumulating over relative time in score. We do not use the affine space instance here.
mapWithTimeDur :: (Duration -> Duration -> a -> b) -> Part a -> Part b
mapWithTimeDur f = mapWithTimeDur' (\t -> fmap . f t)
mapWithTimeDur' f = Part . snd . mapAccumL (\t (d, x) -> (t + d, (d, f t d x))) 0 . getPart

setVoice :: Voice -> Score a -> Score a
setVoice v = Score . fmap (first (const v)) . getScore            


-- Generic stuff (not exported)

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




logBaseR :: forall a . (RealFloat a, Floating a) => Rational -> Rational -> a
logBaseR k n 
    | isInfinite (fromRational n :: a)      = logBaseR k (n/k) + 1
logBaseR k n 
    | isDenormalized (fromRational n :: a)  = logBaseR k (n*k) - 1
logBaseR k n                         = logBase (fromRational k) (fromRational n)

isDivisibleBy :: (Real a, Real b) => a -> b -> Bool
isDivisibleBy n = (== 0.0) . snd . properFraction . logBaseR (toRational n) . toRational

single x = [x]            
fmap2 = fmap . fmap
fmap3 = fmap . fmap . fmap

dump = mapM putStrLn . fmap show

left f (Left x)  = Left (f x)
left f (Right y) = Right y
