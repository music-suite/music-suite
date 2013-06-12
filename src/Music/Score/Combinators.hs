
{-# LANGUAGE
    CPP,
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    OverloadedStrings,
    NoMonomorphismRestriction,
    GeneralizedNewtypeDeriving #-}

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
-- Combinators for manipulating scores.
--
-------------------------------------------------------------------------------------


module Music.Score.Combinators (
        -- ** Preliminaries
        Monoid',
        HasEvents,
        Delay,
        Stretch,
        DelayStretch,
        -- ** Constructing scores
        note,
        rest,
        noteRest,
        chord,
        melody,
        melodyStretch,
        chordDelay,
        chordDelayStretch,

        -- ** Composing scores
        (|>),
        (<|),
        scat,
        pcat,

        -- *** Special composition
        sustain,
        overlap,
        anticipate,

        -- ** Transforming scores
        -- *** Moving in time
        move,
        moveBack,
        startAt,
        stopAt,

        -- *** Stretching in time
        stretch,
        compress,
        stretchTo,
        retrograde,

        -- *** Rests
        removeRests,

        -- *** Repetition
        times,
        group,
        triplet,
        quadruplet,
        quintuplet,

        -- *** Phrases
        mapFirst,
        mapLast,
        mapPhrase,
        mapPhraseSingle,

        -- ** Conversion
        scoreToVoice,
        voiceToScore,
        voiceToScore',
        -- scoreToVoices,
  ) where

import Control.Monad
import Control.Monad.Plus
import Data.Semigroup
import Data.String
import Data.Foldable (Foldable)
import Data.Traversable
import Data.VectorSpace
import Data.AffineSpace
import Data.Ratio
import Data.Pointed
import Data.Ord

import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Part
import Music.Time

import qualified Data.List as List

-- | 
-- This pseudo-class can be used in place of 'Monoid' whenever an additional 'Semigroup'
-- is needed. If 'Monoid' is changed to extend 'Semigroup' it will not be needed.
--   
type Monoid' a    = (Monoid a, Semigroup a)

type Stretch t d a = (
    Stretchable a, Delayable a, HasOffset a, HasOnset a,
    AffineSpace t,
    Pos a ~ t,
    Dur a ~ d
    )

type Delay t a = (
    Delayable a, 
    HasOffset a, 
    HasOnset a,
    AdditiveGroup t,
    AffineSpace t,
    t ~ Pos a
    )

type DelayStretch t d a = (
    Stretchable a, Delayable a, 
    AdditiveGroup t, 
    AffineSpace t,
    Pos a ~ t, 
    Dur a ~ d
    )

-- | 
-- This pseudo-class denotes generalizes structures that can be decomposed into events
-- and reconstructed.
--
type HasEvents s t a  = (
    Performable s,
    MonadPlus s,
    DelayStretch t (Diff t) (s a)
    )


-------------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------------

-- | 
-- Create a score containing a single event at time zero of duration one.
-- 
-- > a -> Score a
-- 
note :: Pointed s => a -> s a
note = point

-- | 
-- Create a score containing a rest at time zero of duration one.
-- 
-- > Score (Maybe a)
-- 
rest :: Pointed s => s (Maybe a)
rest = point Nothing

-- | 
-- Create a score containing a single event at time zero of duration one.
-- 
-- > a -> Score a
-- 
noteRest :: MonadPlus s => Maybe a -> s a
noteRest = mfromMaybe

-- | Creates a score containing the given elements, composed in parallel.
-- 
-- > [a] -> Score a
-- 
chord :: (Pointed s, Monoid (s a)) => [a] -> s a
chord = pcat . map point

-- | Creates a score containing the given elements, composed in sequence.
-- 
-- > [a] -> Score a
-- 
melody :: (Pointed s, Monoid' (s a), Delay t (s a)) => [a] -> s a
melody = scat . map point

-- | Like 'melody', but stretching each note by the given factors.
-- 
-- > [(Duration, a)] -> Score a
-- 
melodyStretch :: (Pointed s, Monoid' (s a), Stretch t d (s a)) => [(d, a)] -> s a
melodyStretch = scat . map ( \(d, x) -> stretch d $ point x )

-- | Like 'chord', but delays each note the given amounts.
-- 
-- > [(Time, a)] -> Score a
-- 
chordDelay :: (Pointed s, Monoid (s a), Delay t (s a)) => [(t, a)] -> s a
chordDelay = pcat . map (\(t, x) -> delay' t $ point x)

-- | Like 'chord', but delays and stretches each note the given amounts.
-- 
-- > [(Time, Duration, a)] -> Score a
-- 
chordDelayStretch :: (Pointed s, Monoid (s a), DelayStretch t d (s a)) => [(t, d, a)] -> s a
chordDelayStretch = pcat . map (\(t, d, x) -> delay' t . stretch d $ point x)

delay' t = delay (t .-. zeroV)


-------------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------------

-- |
-- Move a score forward in time. Equivalent to 'delay'.
--
-- > Duration -> Score a -> Score a
--
move :: Delayable a => Dur a -> a -> a
move = delay

-- |
-- Move a score backward in time. Negated verison of 'delay'
--
-- > Duration -> Score a -> Score a
--
moveBack :: (AdditiveGroup (Dur a), Delayable a) => Dur a -> a -> a
moveBack t = delay (negateV t)

-- |
-- Move a score so that its onset is at the specific time.
--
-- > Duration -> Score a -> Score a
--
startAt :: (AffineSpace (Pos a), HasOnset a, Delayable a) => Pos a -> a -> a
t `startAt` x = delay d x where d = t .-. onset x

-- |
-- Move a score so that its offset is at the specific time.
--
-- > Duration -> Score a -> Score a
--
stopAt :: (AffineSpace (Pos a), HasOffset a, Delayable a) => Pos a -> a -> a
t `stopAt`  x = delay d x where d = t .-. offset x

-- |
-- Compress (diminish) a score. Flipped version of '^/'.
--
-- > Duration -> Score a -> Score a
--
compress :: (Fractional (Dur a), Stretchable a) => Dur a -> a -> a
compress x = stretch (1/x)

-- |
-- Stretch a score to fit into the given duration.
--
-- > Duration -> Score a -> Score a
--
stretchTo:: (Fractional (Dur a), Stretchable a, HasDuration a) => Dur a -> a -> a
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
(|>) :: (Semigroup a, AffineSpace (Pos a), HasOnset a, HasOffset a, Delayable a) => a -> a -> a
a |> b =  a <> startAt (offset a) b


-- |
-- Compose in reverse sequence.
--
-- To compose in parallel, use '<>'.
--
-- > Score a -> Score a -> Score a
(<|) :: (Semigroup a, AffineSpace (Pos a), HasOnset a, HasOffset a, Delayable a) => a -> a -> a
a <| b =  b |> a

-- |
-- Sequential concatentation.
--
-- > [Score t] -> Score t
scat :: (Monoid' a, AffineSpace (Pos a), HasOnset a, HasOffset a, Delayable a) => [a] -> a
scat = foldr (|>) mempty

-- |
-- Parallel concatentation. A synonym for 'mconcat'.
--
-- > [Score t] -> Score t
pcat :: Monoid a => [a] -> a
pcat = mconcat


-- |
-- Like '<>', but scaling the second agument to the duration of the first.
--
-- > Score a -> Score a -> Score a
--
sustain :: (Fractional (Dur a), Semigroup a, Stretchable a, HasDuration a) => a -> a -> a
x `sustain` y = x <> duration x `stretchTo` y

-- Like '<>', but truncating the second agument to the duration of the first.
-- prolong x y = x <> before (duration x) y

-- |
-- Like '|>', but moving second argument halfway to the offset of the first.
--
-- > Score a -> Score a -> Score a
--
overlap :: (Fractional (Dur a), Semigroup a, Delayable a, HasDuration a) => a -> a -> a
x `overlap` y  =  x <> delay t y where t = duration x / 2

-- |
-- Like '|>' but with a negative delay on the second element.
--
-- > Duration -> Score a -> Score a -> Score a
--
anticipate :: (Semigroup a, HasDuration a, Stretch t d a, Ord d) 
    => Dur a -> a -> a -> a
anticipate t x y = x |> delay t' y where t' = (duration x ^+^ (zeroV ^-^ t)) `max` zeroV



--------------------------------------------------------------------------------
-- Structure
--------------------------------------------------------------------------------

-- |
-- Repeat exact amount of times.
--
-- > Duration -> Score Note -> Score Note
--
times :: (Monoid' a, Delay t a) => Int -> a -> a
times n a = replicate (0 `max` n) undefined `repWith` const a

-- |
-- Repeat once for each element in the list.
--
-- > [a] -> (a -> Score Note) -> Score Note
--
-- Example:
--
-- > repWith [1,2,1] (c^*)
--
-- repWith :: (Monoid' c, HasOnset c, HasOffset c, Delayable c) => [a] -> (a -> c) -> c
repWith = flip (\f -> scat . fmap f)

-- |
-- Combination of 'scat' and 'fmap'. Note that
--
-- > scatMap = flip repWith
--
-- scatMap :: (Monoid' c, HasOnset c, HasOffset c, Delayable c) => (a -> c) -> [a] -> c
scatMap f = scat . fmap f

-- |
-- Repeat exact amount of times with an index.
--
-- > Duration -> (Duration -> Score Note) -> Score Note
--
-- repWithIndex :: (Enum a, Num a, Monoid' c, HasOnset c, HasOffset c, Delayable c) => a -> (a -> c) -> c
repWithIndex n = repWith [0..n-1]

-- |
-- Repeat exact amount of times with relative time.
--
-- > Duration -> (Time -> Score Note) -> Score Note
--
-- repWithTime :: (Enum a, Fractional a, Monoid' c, HasOnset c, HasOffset c, Delayable c) => a -> (a -> c) -> c
repWithTime n = repWith $ fmap (/ n') [0..(n' - 1)]
    where
        n' = n

-- |
-- Remove rests from a score.
--
-- This is just an alias for 'mcatMaybes' which reads better in certain contexts.
--
-- > Score (Maybe a) -> Score a
--
removeRests :: MonadPlus m => m (Maybe a) -> m a
removeRests = mcatMaybes

-- |
-- Repeat three times and scale down by three.
--
-- > Score a -> Score a
--
triplet :: (Monoid' a, Delay t a, Stretch t d a, t ~ Time) => a -> a
triplet = group 3

-- |
-- Repeat three times and scale down by three.
--
-- > Score a -> Score a
--
quadruplet :: (Monoid' a, Delay t a, Stretch t d a, t ~ Time) => a -> a
quadruplet  = group 4

-- |
-- Repeat three times and scale down by three.
--
-- > Score a -> Score a
--
quintuplet :: (Monoid' a, Delay t a, Stretch t d a, t ~ Time) => a -> a
quintuplet  = group 5

-- |
-- Repeat a number of times and scale down by the same amount.
--
-- > Duration -> Score a -> Score a
--
group :: (Monoid' a, Delay t a, Stretch t d a, t ~ Time) => Int -> a -> a
group n a = times n (toDuration n `compress` a)

-- |
-- Reverse a score around its middle point.
--
-- > onset a    = onset (retrograde a)
-- > duration a = duration (retrograde a)
-- > offset a   = offset (retrograde a)
--
-- > Score a -> Score a

retrograde :: (Monoid' (s a), HasEvents s t a, Num t, Ord t) => s a -> s a
retrograde = {-startAt 0 . -}retrograde'
    where
        retrograde' = chordDelayStretch' . List.sortBy (comparing getT) . fmap g . perform
        g (t,d,x) = (-(t.+^d),d,x)
        getT (t,d,x) = t            

chordDelayStretch' :: (Monoid' (s a), HasEvents s t a) => [(Pos (s a), Dur (s a), a)] -> s a
chordDelayStretch' = pcat . map ( \(t, d, x) -> delay (t .-. zeroV) . stretch d $ return x )



--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

-- |
-- Convert a score into a voice.
--
-- This function fails if the score contain overlapping events.
--
scoreToVoice :: Score a -> Voice (Maybe a)
scoreToVoice = Voice . fmap throwTime . addRests' . perform
    where
       throwTime (t,d,x) = (d,x)

-- -- |
-- -- Convert a score into a list of voices.
-- --
-- scoreToVoices :: (HasPart a, Part a ~ v, Ord v) => Score a -> [Voice (Maybe a)]
-- scoreToVoices = fmap scoreToVoice . voices

-- |
-- Convert a voice into a score.
--
voiceToScore :: Voice a -> Score a
voiceToScore = scat . fmap g . getVoice
    where
        g (d,x) = stretch d (note x)

-- |
-- Convert a voice which may contain rests into a score.
--
voiceToScore' :: Voice (Maybe a) -> Score a
voiceToScore' = mcatMaybes . voiceToScore

instance Performable Voice where
    perform = perform . voiceToScore

--------------------------------------------------------------------------------

addRests' :: [(Time, Duration, a)] -> [(Time, Duration, Maybe a)]
addRests' = concat . snd . mapAccumL g 0
    where
        g prevTime (t, d, x)
            | prevTime == t   =  (t .+^ d, [(t, d, Just x)])
            | prevTime <  t   =  (t .+^ d, [(prevTime, t .-. prevTime, Nothing), (t, d, Just x)])
            | otherwise       =  error "addRests: Strange prevTime"



{-
infixl 6 ||>

(||>) :: Score a -> Score a -> Score a
a ||> b = mcatMaybes $ padToBar (fmap Just a) |> fmap Just b

padToBar a = a |> rest^*(d'*4)
    where
        d  = snd $ properFraction $ duration a / 4
        d' = if d == 0 then 0 else 1 - d  -}



-- |
-- Map over first, middle and last elements of list.
-- Biased on first, then on first and last for short lists.
--
mapFirstMiddleLast :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapFirstMiddleLast f g h []      = []
mapFirstMiddleLast f g h [a]     = [f a]
mapFirstMiddleLast f g h [a,b]   = [f a, h b]
mapFirstMiddleLast f g h xs      = [f $ head xs] ++ map g (tail $ init xs) ++ [h $ last xs]

#define MAP_PHRASE_CONST \
    HasPart' a, \
    HasEvents s t a, \
    HasEvents s u b, \
    t ~ u

-- |
-- Map over the first, and remaining notes in each part.
--
-- If a part has only one notes, the first function is applied. 
-- If a part has no notes, the given score is returned unchanged.
--
-- > (a -> b) -> (a -> b) -> Score a -> Score b
--
mapFirst :: (MAP_PHRASE_CONST) => (a -> b) -> (a -> b) -> s a -> s b
mapFirst f g = mapPhrase f g g

-- |
-- Map over the last, and preceding notes in each part.
--
-- If a part has only one notes, the first function is applied. 
-- If a part has no notes, the given score is returned unchanged.
--
-- > (a -> b) -> (a -> b) -> Score a -> Score b
--
mapLast :: (MAP_PHRASE_CONST) => (a -> b) -> (a -> b) -> s a -> s b
mapLast f g = mapPhrase g g f

-- |
-- Map over the first, middle and last note in each part.
--
-- If a part has fewer than three notes the first takes precedence over the last,
-- and last takes precedence over the middle.
--
-- > (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
--
mapPhrase :: (MAP_PHRASE_CONST) => (a -> b) -> (a -> b) -> (a -> b) -> s a -> s b
mapPhrase f g h = mapParts (liftM $ mapPhraseSingle f g h)

-- |
-- Equivalent to 'mapPhrase' for single-voice scores.
-- Fails if the score contains overlapping events.
--
-- > (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
--
mapPhraseSingle :: (HasEvents s t a, HasEvents s t b, t ~ u) => (a -> b) -> (a -> b) -> (a -> b) -> s a -> s b
-- mapPhraseSingle f g h sc = msum . mapFirstMiddleLast (liftM f) (liftM g) (liftM h) . liftM toSc . perform $ sc
mapPhraseSingle f g h sc = msum . liftM toSc . mapFirstMiddleLast (third f) (third g) (third h) . perform $ sc
    where
        toSc (t,d,x) = delay (t.-.zeroV) . stretch d $ return x
        third f (a,b,c) = (a,b,f c)

rotl []     = []
rotl (x:xs) = xs ++ [x]

rotr [] = []
rotr xs = last xs : init xs

rotated n as | n >= 0 = iterate rotr as !! n
             | n <  0 = iterate rotl as !! abs n
