
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
-- A score is list of events with a specified part, time and duration. We use
-- relative time so for each event @x@:
--
-- > onset x  = time x
-- > offset x = time x + duration x
--
-- An event can have a negative onset, representing a events occuring before the
-- reference time. Event durations must be positive, allthough the types does not
-- enforce this.
--
-- An event is either a /note/ or a /rest/ (represented by the 'Just' and 'Nothing'
-- constructors). Having explicit rests allow us to concatenate scores based on
-- duration even if the score is actually empty.
--
-- Scores allow overlapping events, but can be split into parts, which does not.
--
-------------------------------------------------------------------------------------

module Music.Score (

        -- * Basic types
        Part,
        Time,
        Duration,

        -- * Score
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

        -- ** Composing
        (|>),
        (<>),
        scat,
        pcat,
        
        -- ** Decomposing
        parts,
        numParts,
        split,
        split',
        -- splitAll,
        partDuration,
        -- occsInPart,
        -- normalizeScore,
        -- getEventTime,
        -- getEventDuration,
        -- mapPart,
        -- getEventPart,
        -- eventPartIs,
)
where

import Prelude hiding (concatMap, maximum)

import Data.Semigroup
import Control.Applicative
import Control.Monad (ap)

import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Traversable
import Data.Ord (comparing)

import Data.VectorSpace
import Data.Basis

-- import Music.Pitch
-- import Music.Dynamics
-- import Music.Articulation

import Music.Pitch.Literal
import Music.Dynamics.Literal
-- import Codec.Midi hiding (Time)

import qualified Data.List as List

-- import System.Posix -- DEBUG

infixr 6 |>

-- |
-- A synonym for '^+^'.
--
-- > pitches  = (c |> d |> e |> f) <> (g |> fs ) ^* 2
-- > dynamics = (p |> cresc |> ff) ^*4
-- > score    = pitches <> dynamics
--
(|>) = (^+^)

-- |
-- A synonym for @flip '<|'@.
--
(<|) = flip (^+^)


type Part     = Int
type Time     = Double
type Duration = Time
newtype Score a  = Score { getScore :: [(Part, Time, Duration, Maybe a)] }
    deriving (Eq, Ord, Show, Functor, Foldable)

instance Semigroup (Score a) where
    Score xs <> Score ys = Score (leftParts xs <> rightParts ys)
        where
            leftParts  = fmap $ mapPart $ \x -> x * 2
            rightParts = fmap $ mapPart $ \x -> x * 2 + 1

-- | 
-- @Score a@ is an instance of 'Monoid'. 'mempty' is a rest of duration zero and 'mappend' performs
-- parallel composition. For sequential composition, use @|>@ or '^+^'.
--
instance Monoid (Score a) where
    mempty  = Score []
    mappend = (<>)

-- | 
-- 'Applicative' derived from 'Monad' instance. Not sure it is useful.
instance Applicative Score where
    pure  = return
    (<*>) = ap

-- | 
-- @Score@ is an instance of 'Monad'. 'return' creates a score containing a single note of time zero and
-- duration one, and '>>=' transforms the events of a score, while allowing transformations of time and duration.
-- More intuitively, 'join' scales and offsets an inner score to fit into an outer score, then removes the
-- intermediate structure. 
--
instance Monad Score where
    return = note
    x >>= k = join' $ k <$> x
        where
            join' sc = pcat $ ev'
                where
                    ev = getScore sc
                    ev' = catMaybes $ fmap (\(p,t,d,x) -> fmap (delay t . stretch d) x) $ ev

instance AdditiveGroup (Score a) where
    zeroV   = Score []

    Score xs ^+^ Score ys = Score (normalizeScore $ xs <> fmap (moveTime (duration $ Score xs)) ys)
        where
            moveTime n (p,t,d,x) = (p,t+n,d,x)

    negateV (Score xs) = Score (normalizeScore $ fmap negTime $ xs)
        where
            negTime (p,t,d,x) = (p,negate t - d,d,x)


instance VectorSpace (Score a) where
    type Scalar (Score a) = Duration
    n *^ Score xs = Score $ fmap (scaleTimeDur n) xs
        where
            scaleTimeDur n (p,t,d,x) = (p,n*t,n*d,x)

-- | 
-- 'Score' is an instance of 'VectorSpace' and 'HasBasis' using sequential
-- composition as addition, time scaling as scalar multiplication and rests of
-- duration 0 and 1 for 'zeroV' and 'basisValue' respectively. The events of each
-- part has a separate basis, so 'decompose' separates parts.
--
instance HasBasis (Score a) where
    type Basis (Score a) = Part
    basisValue p = Score [(p,0,1,Nothing)]
    decompose sc = fmap (\p -> (p, decompose' sc p)) (parts sc)
    decompose'   = partDuration



instance IsPitch a => IsPitch (Score a) where
    fromPitch = pure . fromPitch

instance IsDynamics a => IsDynamics (Score a) where
    fromDynamics = pure . fromDynamics


-- |
-- Sequential concatentation.
--
scat :: [Score t] -> Score t
scat = Prelude.foldr (^+^) mempty

-- |
-- Parallel concatentation.
--
pcat :: [Score t] -> Score t
pcat = Prelude.foldr (<>) mempty


-- | 
-- Returns the duration of the given score.
--
duration :: Score a -> Duration
duration = maximum . fmap (\(p,x) -> partDuration x p) . split

onset :: Score a -> Time
onset = undefined

offset :: Score a -> Time
offset = undefined




-- | 
-- Returns the parts in the given score.
--
parts :: Score a -> [Part]
parts = List.nub . fmap getEventPart . getScore

-- | 
-- Returns the number of parts in the given score.
--
numParts :: Score a -> Int
numParts = length . parts

-- | 
-- Split a given score into its parts.
--
split :: Score a -> [(Part, Score a)]
split sc = fmap (\p -> (p, split' p sc)) (parts sc)

split' :: Part -> Score a -> Score a
split' p sc = Score . occsInPart p $ sc

-- splitAll :: Score a -> [Score a]
-- splitAll = fmap snd . split

partDuration :: Score a -> Part -> Duration
partDuration sc p = list 0 ((\x -> getEventTime x + getEventDuration x) . last) . occsInPart p $ sc

occsInPart :: Part -> Score a -> [(Part, Time, Duration, Maybe a)]
occsInPart p = filter (eventPartIs p) . getScore


normalizeScore = List.sortBy (comparing getEventTime)
getEventTime (p,t,d,x) = t
getEventDuration (p,t,d,x) = d

mapPart      f  (p,t,d,x) = (f p,t,d,x)
getEventPart    (p,t,d,x) = p
eventPartIs  p' (p,t,d,x) = (p' == p)


-- |
-- Create a score of duration 0 with no events.
--
-- Equivalent to 'mempty', 'mzero' and 'zeroV'.
--
rest :: Score a
rest = Score [(0,0,1, Nothing)]

-- |
-- Create a score of duration 1 with the given event.
--
-- Equivalent to 'pure' and 'return'.
--
note :: a -> Score a
note x = Score [(0,0,1, Just x)]

delay :: Duration -> Score a -> Score a
delay d x = (d *^ rest) ^+^ x

stretch :: Duration -> Score a -> Score a
stretch d x = d *^ x











{-
showScore :: Score Int -> String
showScore = show
-}


{-
midiSc :: Score Int -> Midi
midiSc = Midi kType kDiv . (++ [controlTrack]) . (fmap $ absToRel . (++ [(10000,TrackEnd)])) . scToTr
    where
        kType  = MultiTrack
        kDiv   = TicksPerBeat 1024 -- fps tpf
        scToTr = fmap (List.sortBy (comparing fst)) . fmap (\(c,evs) -> concatMap (occToEv c) evs)
            . fmap (second getScore) . split

        occToEv c (p,t,d,Nothing) = []
        occToEv c (p,t,d,Just x)  = [(round $ t*1024, NoteOn c x 127)]
        -- TODO carry over channel etc

        absToRel = snd . mapAccumL (\t' (t,x) -> (t, (t-t',x))) 0

        controlTrack = [(0, TempoChange 1000000), (10000,TrackEnd)]


second f (x,y) = (x,f y)

playSc :: Score Int -> IO ()
playSc sc = do
    exportFile "test.mid" (midiSc sc)
    execute "timidity" ["test.mid"]
                                     -}
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

list z f [] = z
list z f xs = f xs
