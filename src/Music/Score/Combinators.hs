                              
{-# LANGUAGE
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
        MonadPlus',
        -- ** Constructing scores
        rest,
        note,
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
        mapPhrase,
        mapPhraseSingle,

        -- ** Conversion
        scoreToVoice,
        voiceToScore,
        voiceToScore',
        -- scoreToVoices,
  ) where

import Prelude hiding (foldr, concat, foldl, mapM, concatMap, maximum, sum, minimum)

import Control.Monad (ap, mfilter, join, liftM, MonadPlus(..))
import Control.Monad.Plus
import Data.Semigroup
import Data.String
import Data.Foldable
import Data.Traversable
import qualified Data.List as List
import Data.VectorSpace
import Data.AffineSpace
import Data.Ratio  
import Data.Ord

import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Part
import Music.Time.Relative
import Music.Time.Absolute
import Music.Time.Performable
-- import Music.Score.Part
-- import Music.Score.Ties

type Monoid' a    = (Monoid a, Semigroup a)
type MonadPlus' m = (Functor m, MonadPlus m, Foldable m)


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
chordDelay :: [(Time, a)] -> Score a
chordDelay = pcat . map ( \(t, x) -> startAt t $ note x )

-- | Like 'chord', but delays and stretches each note the given amounts.
chordDelayStretch :: [(Time, Duration, a)] -> Score a
chordDelayStretch = pcat . map ( \(t, d, x) -> startAt t . stretch d $ note x )

-- -- | Like chord, but delaying each note the given amount.
-- arpeggio :: t -> [a] -> Score a
-- arpeggio t xs = chordDelay (zip [0, t ..] xs)


-------------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------------

-- |
-- Move a score forward in time. Equivalent to 'delay'.
-- 
-- > Duration -> Score a -> Score a
-- 
move :: Delayable a => Duration -> a -> a
move = delay

-- |
-- Move a score backward in time. Negated verison of 'delay'
-- 
-- > Duration -> Score a -> Score a
-- 
moveBack :: Delayable a => Duration -> a -> a
moveBack t = delay (negate t)

-- |
-- Move a score so that its onset is at the specific time.
-- 
-- > Duration -> Score a -> Score a
-- 
startAt :: (Delayable a, HasOnset a) => Time -> a -> a
t `startAt` x = delay d x where d = t .-. onset x

-- |
-- Move a score so that its offset is at the specific time.
-- 
-- > Duration -> Score a -> Score a
-- 
stopAt :: (Delayable a, HasOnset a) => Time -> a -> a
t `stopAt`  x = delay d x where d = t .-. offset x

-- |
-- Stretch (augment) a score by the given factor. Equivalent to '*^'.
-- 
-- > Duration -> Score a -> Score a
-- 
stretch :: VectorSpace v => Scalar v -> v -> v
stretch = (*^)

-- |
-- Compress (diminish) a score. Flipped version of '^/'.
-- 
-- > Duration -> Score a -> Score a
-- 
compress :: (VectorSpace v, s ~ Scalar v, Fractional s) => s -> v -> v
compress = flip (^/)

-- | 
-- Stretch a score to fit into the given duration. 
-- 
-- > Duration -> Score a -> Score a
-- 
stretchTo :: (Stretchable a, HasDuration a) => Duration -> a -> a
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
-- a >| b =  stopAt (onset b) a <> b


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
scat :: (Monoid' a, Delayable a, HasOnset a) => [a] -> a
scat = foldr (|>) mempty

-- |
-- Parallel concatentation. A synonym for 'mconcat'.
--
-- > [Score t] -> Score t
pcat :: Monoid a => [a] -> a
pcat = mconcat

-- infixr 7 <<|
-- infixr 7 |>>
-- infixr 7 <||
-- infixr 7 ||>

-- (<||) = sustain
-- (||>) = flip sustain
-- (|>>) = overlap
-- (<<|) = flip overlap    

-- | 
-- Like '<>', but scaling the second agument to the duration of the first.
-- 
-- > Score a -> Score a -> Score a
--
sustain :: (Semigroup a, Stretchable a, HasDuration a) => a -> a -> a
x `sustain` y = x <> duration x `stretchTo` y

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



--------------------------------------------------------------------------------
-- Structure
--------------------------------------------------------------------------------

-- |
-- Repeat exact amount of times.
--
-- > Duration -> Score Note -> Score Note
--
times :: (Enum a, Monoid' c, HasOnset c, Delayable c) => a -> c -> c
times n a = replicate (0 `max` fromEnum n) () `repWith` const a

-- |
-- Repeat once for each element in the list.
--
-- > [a] -> (a -> Score Note) -> Score Note
--
-- Example:
--
-- > repWith [1,2,1] (c^*)
--
repWith :: (Monoid' c, HasOnset c, Delayable c) => [a] -> (a -> c) -> c
repWith = flip (\f -> scat . fmap f)

-- |
-- Combination of 'scat' and 'fmap'. Note that
--
-- > scatMap = flip repWith
--
scatMap f = scat . fmap f

-- |
-- Repeat exact amount of times with an index.
--
-- > Duration -> (Duration -> Score Note) -> Score Note
--
repWithIndex :: (Enum a, Num a, Monoid' c, HasOnset c, Delayable c) => a -> (a -> c) -> c
repWithIndex n = repWith [0..n-1]

-- |
-- Repeat exact amount of times with relative time.
--
-- > Duration -> (Time -> Score Note) -> Score Note
--
repWithTime :: (Enum a, Fractional a, Monoid' c, HasOnset c, Delayable c) => a -> (a -> c) -> c
repWithTime n = repWith $ fmap (/ n') [0..(n' - 1)]
    where
        n' = n


-- |
-- Remove rests from a score.
--
-- Identical to 'mcatMaybes'.
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
triplet :: (Monoid' a, Stretchable a, Delayable a, HasOnset a) => a -> a
triplet     = group (3::Duration)

-- |
-- Repeat three times and scale down by three.
--
-- > Score a -> Score a
--
quadruplet :: (Monoid' a, Semigroup a, Stretchable a, Delayable a, HasOnset a) => a -> a
quadruplet  = group (4::Duration)

-- |
-- Repeat three times and scale down by three.
--
-- > Score a -> Score a
--
quintuplet :: (Monoid' a, Semigroup a, Stretchable a, Delayable a, HasOnset a) => a -> a
quintuplet  = group (5::Duration)

-- |
-- Repeat a number of times and scale down by the same amount.
--
-- > Duration -> Score a -> Score a
--
group :: (Monoid' c, Semigroup c, Stretchable c, HasOnset c, Delayable c) => Duration -> c -> c
group n a = times n (a^/n)

--
-- Repeat a number of times and scale down by the same amount.
--
-- > [Duration] -> Score a -> Score a
--
-- groupWith :: (Enum a, Fractional a, a ~ Scalar c, Monoid' c, Semigroup c, VectorSpace c, HasOnset c, Delayable c) => [a] -> c -> c
-- groupWith = flip $ \p -> scat . fmap (`group` p)

-- |
-- Reverse a score around its middle point.
--
-- > onset a    = onset (retrograde a)
-- > duration a = duration (retrograde a)
-- > offset a   = offset (retrograde a)
--
retrograde :: Score a -> Score a
retrograde = startAt 0 . retrograde'
    where
        retrograde' = chordDelayStretch . List.sortBy (comparing getT) . fmap g . perform
        g (t,d,x) = (-(t.+^d),d,x)
        getT (t,d,x) = t

-- |
-- Repeat indefinately, like repeat for lists.
--
-- > Score Note -> Score Note
--
rep :: Score a -> Score a
rep a = a `plus` delay (duration a) (rep a)
    where
        plus = (<>)
        -- Score as `plus` Score bs = Score (as <> bs)


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





--------------------------------------------------------------------------------

addRests' :: [(Time, Duration, a)] -> [(Time, Duration, Maybe a)]
addRests' = concat . snd . mapAccumL g 0
    where
        g prevTime (t, d, x) 
            | prevTime == t   =  (t .+^ d, [(t, d, Just x)])
            | prevTime <  t   =  (t .+^ d, [(prevTime, t .-. prevTime, Nothing), (t, d, Just x)])
            | otherwise       =  error "addRests: Strange prevTime"        



infixl 6 ||>

(||>) :: Score a -> Score a -> Score a
a ||> b = mcatMaybes $ padToBar (fmap Just a) |> fmap Just b

padToBar a = a |> rest^*(d'*4)
    where
        d  = snd $ properFraction $ duration a / 4
        d' = if d == 0 then 0 else 1 - d


-- | 
-- Map over first, middle and last elements of list.
-- Biased on first, then on first and last for short lists.
-- 
mapFirstMiddleLast :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapFirstMiddleLast f g h []      = []
mapFirstMiddleLast f g h [a]     = [f a]
mapFirstMiddleLast f g h [a,b]   = [f a, h b]
mapFirstMiddleLast f g h xs      = [f $ head xs] ++ map g (tail $ init xs) ++ [h $ last xs]

-- |
-- Map over the first, middle and last note in each part.
-- 
-- If a part has fewer than three notes the first takes precedence over the last, 
-- and last takes precedence over the middle.
--
mapPhrase :: (HasPart' a) => (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapPhrase f g h = mapParts (fmap $ mapPhraseSingle f g h)

-- | 
-- Equivalent to `mapPhrase` for single-voice scores.
-- Fails if the score contains overlapping events.
--
mapPhraseSingle :: (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapPhraseSingle f g h sc = mconcat . mapFirstMiddleLast (fmap f) (fmap g) (fmap h) . fmap toSc . perform $ sc
    where
        toSc (t,d,x) = delay (t .-. 0) . stretch d $ note x
        third f (a,b,c) = (a,b,f c)






-- mapPhrase2
--   :: (
--       MonadPlus' s,
--       HasPart' a, 
--       Performable s, 
--       Delayable (s a),
--       Stretchable (s a)
--       ) =>
--      (a -> b) -> (a -> b) -> (a -> b) -> s a -> s b
-- mapPhrase2 f g h = mapParts (fmap $ mapPhraseSingle2 f g h)
-- 
-- mapPhraseSingle2 f g h sc = Control.Monad.Plus.msum . mapFirstMiddleLast (fmap f) (fmap g) (fmap h) . fmap toSc . perform2 $ sc
--     where
--         toSc (t,d,x) = delay (t .-. 0) . stretch d $ return x
--         third f (a,b,c) = (a,b,f c)



rotl []     = []
rotl (x:xs) = xs ++ [x]

rotr [] = []
rotr xs = last xs : init xs

rotated n as | n >= 0 = iterate rotr as !! n
             | n <  0 = iterate rotl as !! abs n

-- splitWhile :: (a -> Bool) -> [a] -> [[a]]
-- splitWhile p xs = case splitWhile' p xs of
--     []:xss -> xss
--     xss    -> xss
--     where
--         splitWhile' p []     = [[]]
--         splitWhile' p (x:xs) = case splitWhile' p xs of
--             (xs:xss) -> if p x then []:(x:xs):xss else (x:xs):xss  

