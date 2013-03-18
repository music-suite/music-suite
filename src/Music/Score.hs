
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
-- reference time. Event durations must be positive, allthough the types does not
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
        Voice,
        Time,
        Duration,

        -- * Score
        Track(..),
        Part(..),
        Score(..),

        -- ** Creating
        rest,
        note,

        -- ** Inspecting
        onset,
        offset,
        duration,

        -- ** Transforming
        delay,
        stretch,
        compress,

        -- ** Composing
        (|>),
        (<>),
        scat,
        pcat,
        
        -- ** Decomposing
        voices,
        numVoices,
        split,
        split',
        -- splitAll,
        partDuration,
        -- occsInVoice,
        -- normalizeScore,
        -- getEventTime,
        -- getEventDuration,
        -- mapVoice,
        -- getEventVoice,
        -- valueVoiceIs,
)
where

import Prelude hiding (concatMap, maximum)

import Data.Semigroup
import Control.Applicative
import Control.Monad (ap, join)
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Traversable
import Data.Ord (comparing)

import Data.VectorSpace
import Data.Basis

import Music.Pitch.Literal
import Music.Dynamics.Literal

import qualified Data.List as List

infixr 6 |>


type Voice     = Int
type Time     = Double
type Duration = Time                                  

-- |
-- A part is a list of absolute-time occurences.
--
newtype Track a = Track { getTrack :: [(Time, a)] }
    deriving (Eq, Ord, Show, Functor, Foldable, Monoid)


-- |
-- A part is a list of relative-time notes and rests.
--
newtype Part a = Part { getPart :: [(Duration, Maybe a)] }
    deriving (Eq, Ord, Show, Functor, Foldable, Monoid)

-- |
-- A score is list of parts.
--
-- Score is a 'Monoid' under parallel compisiton. 'mempty' is a score of no parts.
-- For sequential composition of scores, use '|>' or '^+^'.
--
-- Score has an 'Applicative' instance derived from the 'Monad' instance. Not sure it is useful.
--
-- Score is a 'Monad'. 'return' creates a score containing a single note of
-- duration one, and '>>=' transforms the values of a score, while allowing
-- transformations of time and duration. More intuitively, 'join' scales and
-- offsets an inner score to fit into an outer score, then removes the intermediate
-- structure. 
--
-- 'Score' is an instance of 'VectorSpace' and 'HasBasis' using sequential
-- composition as addition, time scaling as scalar multiplication and rests of
-- duration 0 and 1 for 'zeroV' and 'basisValue' respectively. The values of each
-- part has a separate basis, so 'decompose' separates parts.
--
newtype Score a  = Score { getScore :: [(Voice, Part a)] }
    deriving (Eq, Ord, Show, Functor, Foldable)

instance Semigroup (Score a) where
    Score xs <> Score ys = undefined
    -- Score xs <> Score ys = Score (leftVoices xs <> rightVoices ys)
    --     where
    --         leftVoices  = fmap $ mapVoice $ \x -> x * 2
    --         rightVoices = fmap $ mapVoice $ \x -> x * 2 + 1

instance Monoid (Score a) where
    mempty  = Score []
    mappend = (<>)

instance Applicative Score where
    pure  = return
    (<*>) = ap

-- | 
--
instance Monad Score where
    return = note
    x >>= k = join' $ k <$> x
        where
            join' sc = undefined
            -- join' sc = pcat $ ev'
            --     where
            --         ev = getScore sc
            --         ev' = catMaybes $ fmap (\(p,t,d,x) -> fmap (delay t . stretch d) x) $ ev

instance AdditiveGroup (Score a) where
    zeroV   = Score []

    Score xs ^+^ Score ys = undefined
    -- Score xs ^+^ Score ys = Score (normalizeScore $ xs <> fmap (moveTime (duration $ Score xs)) ys)
        -- where
            -- moveTime n (p,t,d,x) = (p,t+n,d,x)

    negateV (Score xs) = undefined
    -- negateV (Score xs) = Score (normalizeScore $ fmap negTime $ xs)
    --     where
    --         negTime (p,t,d,x) = (p,negate t - d,d,x)


instance VectorSpace (Score a) where
    type Scalar (Score a) = Duration
    n *^ Score xs = undefined
    -- n *^ Score xs = Score $ fmap (scaleTimeDur n) xs
    --     where
    --         scaleTimeDur n (p,t,d,x) = (p,n*t,n*d,x)

instance HasBasis (Score a) where
    type Basis (Score a) = Voice
    -- basisValue p = Score [(p,1,Nothing)]
    -- decompose sc = fmap (\p -> (p, decompose' sc p)) (parts sc)
    -- decompose'   = partDuration



instance IsPitch a => IsPitch (Score a) where
    fromPitch = pure . fromPitch

instance IsDynamics a => IsDynamics (Score a) where
    fromDynamics = pure . fromDynamics


-- |
-- A synonym for '^+^'.
--
-- > Score a -> Score a -> Score a
(|>) = (^+^)

-- |
-- A synonym for @flip '<|'@.
--
-- > Score a -> Score a -> Score a
(<|) = flip (^+^)

-- |
-- Sequential concatentation.
--
-- > [Score t] -> Score t
scat = Prelude.foldr (^+^) mempty

-- |
-- Parallel concatentation.
--
-- > [Score t] -> Score t
pcat = Prelude.foldr (<>) mempty


-- | 
-- Returns the duration of the given score.
--
-- > onset x  = time x
-- > offset x = time x + duration x
--
duration :: Score a -> Duration
duration = maximum . fmap (\(p,x) -> partDuration x p) . split

onset :: Score a -> Time
onset = undefined

offset :: Score a -> Time
offset = undefined

-- |
-- Create a score of duration 1 with no values.
--
rest :: Score a
rest = undefined
-- rest = Score [(0,1, Nothing)]

-- |
-- Create a score of duration 1 with the given value.
--
-- Equivalent to 'pure' and 'return'.
--
note :: a -> Score a
note = undefined
-- note x = Score [(0,1, Just x)]

-- |
-- Delay a score.
-- 
-- > Duration -> Score a -> Score a
delay :: (Num a, a ~ (Basis v), HasBasis v) => Scalar v -> v -> v
delay d x = rest' ^* d ^+^ x where rest' = basisValue 0

-- |
-- Stretch a score. Equivalent to '*^'.
-- 
-- > Duration -> Score a -> Score a
stretch :: VectorSpace v => Scalar v -> v -> v
stretch = (*^)

-- |
-- Compress a score. Flipped version of '^/'.
-- 
-- > Duration -> Score a -> Score a
compress :: (VectorSpace v, s ~ Scalar v, Fractional s) => s -> v -> v
compress = flip (^/)









-- | 
-- Returns the voices in the given score.
--
voices :: Score a -> [Voice]
voices = undefined
-- voices = List.nub . fmap getEventVoice . getScore

-- | 
-- Returns the number of voices in the given score.
--
numVoices :: Score a -> Int
numVoices = length . voices

-- | 
-- Split a given score into its parts.
--
split :: Score a -> [(Voice, Score a)]
split sc = fmap (\p -> (p, split' p sc)) (voices sc)

split' :: Voice -> Score a -> Score a
split' = undefined
-- split' p sc = Score . occsInVoice p $ sc

-- splitAll :: Score a -> [Score a]
-- splitAll = fmap snd . split

partDuration :: Score a -> Voice -> Duration
partDuration sc p = list 0 ((\x -> getEventTime x + getEventDuration x) . last) . occsInVoice p $ sc

occsInVoice :: Voice -> Score a -> [(Voice, Duration, Maybe a)]
occsInVoice = undefined
-- occsInVoice p = filter (valueVoiceIs p) . getScore


normalizeScore :: [(Voice, Duration, Maybe a)] -> [(Voice, Duration, Maybe a)]
normalizeScore = List.sortBy (comparing getEventTime)

getEventTime :: (Voice, Duration, Maybe a) -> Time
getEventTime = undefined
getEventDuration (p,d,x) = d

mapVoice      f  (p,d,x) = (f p,d,x)
getEventVoice    (p,d,x) = p
valueVoiceIs  p' (p,d,x) = (p' == p)






{-
showScore :: Score Int -> String
showScore = show
-}


{-
midiSc :: Score Int -> Midi
midiSc = Midi kType kDiv . (++ [controlPart]) . (fmap $ absToRel . (++ [(10000,PartEnd)])) . scToTr
    where
        kType  = MultiPart
        kDiv   = TicksPerBeat 1024 -- fps tpf
        scToTr = fmap (List.sortBy (comparing fst)) . fmap (\(c,evs) -> concatMap (occToEv c) evs)
            . fmap (second getScore) . split

        occToEv c (p,t,d,Nothing) = []
        occToEv c (p,t,d,Just x)  = [(round $ t*1024, NoteOn c x 127)]
        -- TODO carry over channel etc

        absToRel = snd . mapAccumL (\t' (t,x) -> (t, (t-t',x))) 0

        controlPart = [(0, TempoChange 1000000), (10000,PartEnd)]


second f (x,y) = (x,f y)

playSc :: Score Int -> IO ()
playSc sc = do
    exportFile "test.mid" (midiSc sc)
    execute "timidity" ["test.mid"]
                                     -}
{-
instance IsPitch Int where
    fromPitch (PitchL (pc, sem, oct)) = semitones sem + diatonic pc + (oct+1) * 12
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
-}

list z f [] = z
list z f xs = f xs
