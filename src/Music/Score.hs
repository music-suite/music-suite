
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

        -- ** Inspecting
        Monoid'(..),
        HasDuration(..),
        HasOnset(..),
        Delayable(..),
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
)
where

import Prelude hiding (concatMap, maximum, sum, minimum)

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

import Music.Pitch.Literal
import Music.Dynamics.Literal


import qualified Codec.Midi as Midi
import qualified Data.Map as Map
import qualified Data.List as List


newtype Voice = Voice { getVoice::Int }
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

newtype Time = Time { getTime::Double }
    deriving (Eq, Ord, Show, Num, Enum, Real, Fractional, RealFrac)
    -- Note: no Floating as we want to be able to switch to rational

newtype Duration = Duration { getDuration::Double }                                  
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
    a .-. b =  t2d $ a - b
    a .+^ b =  a + d2t b

instance AdditiveGroup Duration where
    zeroV = 0
    (^+^) = (+)
    negateV = negate

instance VectorSpace Duration where
    type Scalar Duration = Duration
    (*^) = (*)

instance InnerSpace Duration where (<.>) = (*)

-- TODO factor out these and use AffineSpace instance instead
d2t = Time . getDuration
t2d = Duration . getTime


class (Monoid a, Semigroup a) => Monoid' a

class HasDuration a where
    duration :: a -> Duration

class HasOnset a where
    onset :: a -> Time

class Delayable a where
    delay :: Duration -> a -> a


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
    n *^ Track as = Track (fmap (first (n*^)) as)

instance HasOnset (Track a) where
    onset (Track []) = 0
    onset (Track xs) = fst . head $ xs

instance Delayable (Track a) where
    delay d = Track . fmap (first (.+^ d)) . getTrack

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

instance HasDuration (Part a) where
    duration (Part []) = zeroV
    duration (Part xs) = sum $ fmap fst $ xs

-- instance HasOnset (Part a) where
--     onset (Part []) = Nothing
--     onset (Part xs) = Just $ sum $ fmap fst $ takeWhile isRest $ xs
--         where
--             isRest (_,Nothing) = True
--             isRest (_,Just _)  = False

instance Delayable (Part a) where
    delay t (Part xs) = Part ((t, Nothing) : xs)

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
    deriving (Eq, Ord, Show, Functor, Foldable)

instance Semigroup (Score a) where
    (<>) = mappend

instance Monoid (Score a) where
    mempty  = Score mempty
    Score as `mappend` Score bs = Score (as `mappend` bs)
        -- where                  
        --     l = fmap (first $ (* 2))
        --     r = fmap (first $ (+ 1) . (* 2))

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

instance Delayable (Score a) where
    delay t (Score xs) = Score (fmap (second (delay t)) xs)


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
stretchTo t x 
    | duration x == t  =  x
    | otherwise        =  stretch (t / duration x) x 


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
scat :: (Monoid' b, Delayable b, HasDuration b) => [b] -> b
scat = Prelude.foldr (|>) mempty

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
sustain x y = x <> stretchTo (duration x) y


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

{-
-- loopOverlay :: Time t => Score t a -> Score t a
loopOverlay x = x |>> loopOverlay x

-- loopOverlayAll :: Time t => [Score t a] -> Score t a
loopOverlayAll xs = l xs xs
    where l xs []     = l xs xs
          l xs (y:ys) = y |>> l xs ys   

-}



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

-- midiSc :: Score Int -> Midi
-- midiSc = undefined

-- midiSc :: Score Int -> Midi
-- midiSc = Midi kType kDiv . (++ [controlPart]) . (fmap $ absToRel . (++ [(10000,PartEnd)])) . scToTr
--     where
--         kType  = MultiPart
--         kDiv   = TicksPerBeat 1024 -- fps tpf
--         scToTr = fmap (List.sortBy (comparing fst)) . fmap (\(c,evs) -> concatMap (occToEv c) evs)
--             . fmap (second getScore) . split
-- 
--         occToEv c (p,t,d,Nothing) = []
--         occToEv c (p,t,d,Just x)  = [(round $ t*1024, NoteOn c x 127)]
--         -- TODO carry over channel etc
-- 
--         absToRel = snd . mapAccumL (\t' (t,x) -> (t, (t-t',x))) 0
-- 
--         controlPart = [(0, TempoChange 1000000), (10000,PartEnd)]



-- playSc :: Score Int -> IO ()
-- playSc sc = do
--     exportFile "test.mid" (midiSc sc)
--     execute "timidity" ["test.mid"]



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
