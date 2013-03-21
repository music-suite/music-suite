
{-# LANGUAGE
    TypeFamilies,
    GeneralizedNewtypeDeriving,
    DeriveFunctor,
    DeriveFoldable,
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
        Voice(..),
        Time(..),
        Duration(..),

        -- * Score
        Track(..),
        Part(..),
        Score(..),

        -- ** Creating
        rest,
        note,
        chord,
        melody,
        chords,
        melodies,

        -- ** Inspecting
        -- Monoid'(..),
        HasDuration(..),
        HasOnset(..),
        Delayable(..),
        Performable(..),
        offset,
        startAt,
        stopAt,

        -- ** Transforming
        stretch,
        compress,
        stretchTo,        

        -- ** Composing
        (|>),
        (<|),
        scat,
        pcat,
        sustain,
        -- prolong,
        anticipate,
        (<<|),
        (|>>),
        
        -- ** Decomposing
        voices,
        numVoices,
        setVoice,
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
            
-}  

import Prelude hiding (foldr, foldl, mapM, concatMap, maximum, sum, minimum)

import Data.Semigroup
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

import System.Posix -- debug
import System.IO

import Music.Pitch.Literal
import Music.Dynamics.Literal

import qualified Codec.Midi as Midi
import qualified Music.MusicXml.Simple as Xml
import qualified Data.Map as Map
import qualified Data.List as List


newtype Voice = Voice { getVoice::Int }
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

newtype Time = Time { getTime::Rational }
    deriving (Eq, Ord, Show, Num, Enum, Real, Fractional, RealFrac)
    -- Note: no Floating as we want to be able to switch to rational

newtype Duration = Duration { getDuration::Rational }                                  
    deriving (Eq, Ord, Show, Num, Enum, Real, Fractional, RealFrac)
    -- Note: no Floating as we want to be able to switch to rational

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

class Delayable a where
    delay :: Duration -> a -> a

class Performable f where
    perform  :: f a -> [(Voice, Time, Duration, a)]
    performR :: f a -> [(Voice, Time, Duration, a)]
    -- TODO split and remerge parts...
    performR = toRel . perform
        where
            toRel = snd . mapAccumL g 0
            g t' (v,t,d,x) = (t, (v,t-t',d,x))


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
            
-- |
-- > offset x = onset x + duration x
-- 
offset :: (HasOnset a, HasDuration a) => a -> Time
offset x = onset x .+^ duration x

t `startAt` x = delay d x where d = t .-. onset x
t `stopAt` x  = delay d x where d = t .-. offset x


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


infixr 7 |>
infixr 7 <|

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
-- Like '|>' but with a negative delay on the second element.
-- 
-- > Duration -> Score a -> Score a -> Score a
-- 
anticipate :: (Semigroup a, Delayable a, HasDuration a) => Duration -> a -> a -> a
anticipate t x y = x |> delay t' y where t' = (duration x - t) `max` 0


infixr 7 <<|
infixr 7 |>>

-- |
-- > Score a -> Score a -> Score a
--
(<<|) :: (Semigroup a, Delayable a, HasDuration a) => a -> a -> a
x <<| y  =  y |>> x    

-- |
-- > Score a -> Score a -> Score a
--
(|>>) :: (Semigroup a, Delayable a, HasDuration a) => a -> a -> a
x |>> y  =  x <> delay t y where t = duration x / 2    

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
 










-- Test stuff

score :: Score a -> Score a
score = id

prettyPart :: Show a => Part a -> String
prettyPart = concatSep " |> " . fmap (\(t,x) -> show t ++ "*^"++ maybe "rest" show x) . getPart

prettyScore :: Show a => Score a -> String
prettyScore = concatSep " <> " . fmap (\(p,x) -> "("++prettyPart x++")") . getScore

showScore :: Score Double -> String
showScore = show

ssm :: Score Midi.Message -> IO [()]
ssm = mapM putStrLn . fmap show . perform

ssd :: Score Double -> IO [()]
ssd = mapM putStrLn . fmap show . perform

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


class HasMidi a where
    toMidi :: a -> Score Midi.Message
instance HasMidi Midi.Message where
    toMidi = return
instance HasMidi Double where
    toMidi = toMidi . toInteger . round
instance HasMidi Integer where
    toMidi x = note (Midi.NoteOn 0 (fromIntegral x) 100) |> note (Midi.NoteOff 0 (fromIntegral x) 0)
    
scoreToMidi :: HasMidi a => Score a -> Midi.Midi
scoreToMidi score = Midi.Midi type' div [ctrlTr, evTr]
    where                         
        divs    = 1024
        type'   = Midi.MultiTrack       
        div     = Midi.TicksPerBeat divs
        ctrlTr  = [(0, Midi.TempoChange 1000000), (10000, Midi.TrackEnd)]
        evTr    = evs <> [(10000, Midi.TrackEnd)] 
        evs     = fmap (\(v,t,d,x) -> (round (t * divs), x)) $ performR $ (>>= toMidi) $ score
        -- TODO handle duration, voice
        -- FIXME relative time ...

playScore :: HasMidi a => Score a -> IO ()
playScore sc = do
    Midi.exportFile "test.mid" (scoreToMidi sc)
    execute "timidity" ["test.mid"]





-- class HasXml a where
--     toXml :: a -> Score Xml.Score
-- instance HasXml Xml.Score where
--     toXml = return
-- instance HasXml Double where
--     toXml = toXml . toInteger . round
-- instance HasXml Integer where
--     toXml x = undefined   

testXml :: Score Integer -> Xml.Score
testXml = Xml.fromPart "" "" "" . fmap toMusic . intoBars

-- TODO assumes length one
intoBars :: Score a -> [[(Voice, Duration, a)]]
intoBars = fmap (fmap g) . splitWhile ((== 0) . trd5) . map f . perform
    where  
        g (v,bn,bt,d,x) = (v,d,x)
        f (v,t,d,x) = (v,bn,bt,d,x) where (bn,bt) = properFraction (toRational t)
        trd5 (a,b,c,d,e) = c

openSib :: Xml.Score -> IO ()
openSib score =
    do  writeFile "test.xml" (Xml.showXml score)
        execute "open" ["-a", "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6", "test.xml"]

toMusic :: [(Voice, Duration, Integer)] -> Xml.Music
toMusic = mconcat . fmap toMusic1

toMusic1 :: (Voice, Duration, Integer) -> Xml.Music
toMusic1 (v,d,p) = Xml.note (toEnum . fromIntegral . (`mod` 6) $ p, Nothing, 4) (fromRational . toRational $ d)


splitWhile :: (a -> Bool) -> [a] -> [[a]]
splitWhile p []     = [[]]
splitWhile p (x:xs) = case splitWhile p xs of
    (xs:xss) -> if p x then []:(x:xs):xss else (x:xs):xss

(<||) = sustain
(||>) = flip sustain

list z f [] = z
list z f xs = f xs

first f (x,y)  = (f x, y)
second f (x,y) = (x, f y)


sep :: a -> [a] -> [a]
sep = List.intersperse

concatSep :: [a] -> [[a]] -> [a]
concatSep x = List.concat . sep x




-- Accumulating over relative time in score. We do not use the affine space instance here.
mapWithTimeDur :: (Duration -> Duration -> a -> b) -> Part a -> Part b
mapWithTimeDur f = mapWithTimeDur' (\t -> fmap . f t)
mapWithTimeDur' f = Part . snd . mapAccumL (\t (d, x) -> (t + d, (d, f t d x))) 0 . getPart

setVoice :: Voice -> Score a -> Score a
setVoice v = Score . fmap (first (const v)) . getScore            


execute :: FilePath -> [String] -> IO ()
execute program args = do
    forkProcess $ executeFile program True args Nothing
    return ()
