
{-# LANGUAGE
    CPP,
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    OverloadedStrings,
    MultiParamTypeClasses,
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
        Transformable,
        Phraseable(..),

        -- ** Composing scores
        (|>),
        (<|),
        scat,
        pcat,

        -- *** Special composition
        sustain,
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

        -- *** Rests
        rest,
        removeRests,

        -- *** Repetition
        times,
        repeated,
        group,

        -- *** Transformations
        perform,
        compose,
        retrograde,
        mapEvents,
        filterEvents,
        mapFilterEvents,
        -- mapFirst,
        -- mapLast,
        mapPhrase,
        mapPhraseSingle,
        mapAll,

        -- *** Parts
        extractParts,
        extractParts',
        mapPart,
        mapAllParts,
        mapParts,
        getParts,
        setParts,
        modifyParts,

        -- ** Part composition
        (</>),
        moveParts,
        moveToPart,
        -- ** Zipper
        apply,
        snapshot,
        applySingle,
        snapshotSingle,

        -- ** Conversion
        voiceToScore,
        voiceToScore',
        scoreToVoice,
  ) where

import Control.Monad
import Control.Monad.Plus
import Data.Semigroup
import Data.String
import Data.Foldable (Foldable(..))
import Data.Traversable
import Data.VectorSpace
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Ratio
import Data.Pointed
import Data.Ord

import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Part
import Music.Time

import qualified Data.List as List
import qualified Data.Foldable as Foldable


-------------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------------

-- -- |
-- -- Create a score containing a single event.
-- --
-- -- This function uses the unit position (0, 1).
-- --
-- -- > a -> Score a
-- --
-- note :: Monad s => a -> s a
-- note = return
-- 
-- -- |
-- -- Create a score containing a rest at time zero of duration one.
-- --
-- -- This function uses the unit position (0, 1).
-- --
-- -- > Score (Maybe a)
-- --
-- rest :: MonadPlus s => s (Maybe a)
-- rest = note Nothing
-- 
-- -- |
-- -- Create a note or a rest. This is an alias for 'mfromMaybe' with a nicer reading.
-- --
-- -- This function uses the unit position (0, 1).
-- --
-- -- > a -> Score a
-- --
-- noteRest :: MonadPlus s => Maybe a -> s a
-- noteRest = mfromMaybe

{-
-- | Creates a score containing a chord.
--
-- This function uses the unit position (0, 1).
--
-- > [a] -> Score a
--
-- chord :: (Pointed s, Monoid (s a)) => [a] -> s a
chord :: (MonadPlus s, Monoid' (s a)) => [a] -> s a
chord = pcat . map note

-- | Creates a score containing the given elements, composed in sequence.
--
-- > [a] -> Score a
--
melody :: (MonadPlus s, Monoid' (s a), Transformable s) => [a] -> s a
melody = scat . map note

-- | Like 'melody', but stretching each note by the given factors.
--
-- > [(Duration, a)] -> Score a
--
melodyStretch :: (MonadPlus s, Monoid' (s a), Transformable s, d ~ Duration s) => [(d, a)] -> s a
melodyStretch = scat . map ( \(d, x) -> stretch d $ note x )

-- | Like 'chord', but delays each note the given amounts.
--
-- > [(Time, a)] -> Score a
--
chordDelay :: (MonadPlus s, Monoid (s a), Transformable s, t ~ Time s) => [(t, a)] -> s a
chordDelay = pcat . map (\(t, x) -> delay' t $ note x)

-- | Like 'chord', but delays and stretches each note the given amounts.
--
-- > [(Time, Duration, a)] -> Score a
--
chordDelayStretch :: (MonadPlus s, Monoid (s a), Transformable s, d ~ Duration s, t ~ Time s) => [(t, d, a)] -> s a
chordDelayStretch = pcat . map (\(t, d, x) -> delay' t . stretch d $ note x)
-}

-------------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------------

-- -- |
-- -- Move a score forward in time. Equivalent to 'delay'.
-- --
-- -- > Duration -> Score a -> Score a
-- --
-- move :: (Delayable s, d ~ Duration s) => d -> s a -> s a
-- move = delay
-- 
-- -- |
-- -- Move a score backward in time. Negated verison of 'delay'
-- --
-- -- > Duration -> Score a -> Score a
-- --
-- moveBack :: (Delayable s, AdditiveGroup d, d ~ Duration s) => d -> s a -> s a
-- moveBack t = delay (negateV t)
-- 
-- -- |
-- -- Move a score so that its onset is at the specific time.
-- --
-- -- > Duration -> Score a -> Score a
-- --
-- startAt :: (HasOnset s, Delayable s, AffineSpace t, t ~ Time s) => t -> s a -> s a
-- t `startAt` x = (t .-. onset x) `delay` x
-- 
-- -- |
-- -- Move a score so that its offset is at the specific time.
-- --
-- -- > Duration -> Score a -> Score a
-- --
-- stopAt :: (HasOffset s, Delayable s, AffineSpace t, t ~ Time s) => t -> s a -> s a
-- t `stopAt`  x = (t .-. offset x) `delay` x
-- 
-- -- |
-- -- Compress (diminish) a score. Flipped version of '^/'.
-- --
-- -- > Duration -> Score a -> Score a
-- --
-- compress :: (Stretchable s, Fractional d, d ~ Duration s) => d -> s a -> s a
-- compress x = stretch (recip x)
-- 
-- -- |
-- -- Stretch a score to fit into the given duration.
-- --
-- -- > Duration -> Score a -> Score a
-- --
-- stretchTo :: (Stretchable s, HasDuration s, Fractional d, d ~ Duration s) => d -> s a -> s a
-- t `stretchTo` x = (t / duration x) `stretch` x
-- 
-- 
-- -------------------------------------------------------------------------------------
-- -- Composition
-- -------------------------------------------------------------------------------------
-- 
-- infixr 6 |>
-- infixr 6 <|
-- 
-- -- |
-- -- Compose in sequence.
-- --
-- -- To compose in parallel, use '<>'.
-- --
-- -- > Score a -> Score a -> Score a
-- (|>) :: (Semigroup (s a), AffineSpace (Time s), HasOnset s, HasOffset s, Delayable s) => s a -> s a -> s a
-- a |> b =  a <> startAt (offset a) b
-- 
-- 
-- -- |
-- -- Compose in reverse sequence.
-- --
-- -- To compose in parallel, use '<>'.
-- --
-- -- > Score a -> Score a -> Score a
-- (<|) :: (Semigroup (s a), AffineSpace (Time s), HasOnset s, HasOffset s, Delayable s) => s a -> s a -> s a
-- a <| b =  b |> a
-- 
-- -- |
-- -- Sequential concatentation.
-- --
-- -- > [Score t] -> Score t
-- scat :: (Monoid' (s a), AffineSpace (Time s), HasOnset s, HasOffset s, Delayable s) => [s a] -> s a
-- scat = foldr (|>) mempty
-- 
-- -- |
-- -- Parallel concatentation. A synonym for 'mconcat'.
-- --
-- -- > [Score t] -> Score t
-- pcat :: Monoid a => [a] -> a
-- pcat = mconcat
-- 
-- 
-- -- |
-- -- Like '<>', but scaling the second agument to the duration of the first.
-- --
-- -- > Score a -> Score a -> Score a
-- --
-- sustain :: (Fractional (Duration s), Semigroup (s a), Stretchable s, HasDuration s) => s a -> s a -> s a
-- x `sustain` y = x <> duration x `stretchTo` y
-- 
-- -- Like '<>', but truncating the second agument to the duration of the first.
-- -- prolong x y = x <> before (duration x) y
-- 
-- -- |
-- -- Like '|>' but with a negative delay on the second element.
-- --
-- -- > Duration -> Score a -> Score a -> Score a
-- --
-- anticipate :: (Semigroup (s a), Transformable s, d ~ Duration s, Ord d) => d -> s a -> s a -> s a
-- anticipate t a b =  a <> startAt (offset a .-^ t) b
-- 
-- 
-- 
-- --------------------------------------------------------------------------------
-- -- Structure
-- --------------------------------------------------------------------------------
-- 
-- -- |
-- -- Repeat exact amount of times.
-- --
-- -- > Duration -> Score Note -> Score Note
-- --
-- times :: (Monoid' (s a), Transformable s) => Int -> s a -> s a
-- times n a = replicate (0 `max` n) () `repeated` const a
-- 
-- -- |
-- -- Repeat once for each element in the list.
-- --
-- -- Example:
-- --
-- -- > repeated [1,2,1] (c^*)
-- --
-- -- Simple type:
-- --
-- -- > [a] -> (a -> Score Note) -> Score Note
-- --
-- repeated :: (Monoid' (s b), Transformable s) => [a] -> (a -> s b) -> s b
-- repeated = flip (\f -> scat . fmap f)
-- 
-- 
-- {-
-- repeatedIndex n = repeated [0..n-1]
-- repeatedTime  n = repeated $ fmap (/ n) [0..(n - 1)]
-- -}
-- 
-- 
-- -- |
-- -- Remove rests from a score.
-- --
-- -- This is just an alias for 'mcatMaybes' which reads better in certain contexts.
-- --
-- -- > Score (Maybe a) -> Score a
-- --
-- removeRests :: MonadPlus m => m (Maybe a) -> m a
-- removeRests = mcatMaybes
-- 
-- -- |
-- -- Repeat a number of times and scale down by the same amount.
-- --
-- -- > Duration -> Score a -> Score a
-- --
-- group :: (Monoid' (s a), Transformable s, Fractional d, d ~ Duration s) => Int -> s a -> s a
-- group n a = times n (fromIntegral n `compress` a)
-- 
-- -- |
-- -- Reverse a score around its middle point (TODO not correct documentation w.r.t to start).
-- --
-- -- > onset a    = onset (retrograde a)
-- -- > duration a = duration (retrograde a)
-- -- > offset a   = offset (retrograde a)
-- --
-- -- > Score a -> Score a
-- --
-- retrograde :: (HasEvents s, t ~ Time s, Num t, Ord t) => s a -> s a
-- retrograde = startAt 0 . (mapAllEvents $ List.sortBy (comparing fst3) . fmap g)
--     where
--         g (t,d,x) = (-(t.+^d),d,x)
-- 
-- --------------------------------------------------------------------------------
-- -- Mapping and recomposition
-- --------------------------------------------------------------------------------
-- 
-- #define MAP_CONSTRAINT \
--     HasPart' a, \
--     HasEvents s
-- 
-- -- | Recompose a score.
-- --
-- -- This is the inverse of 'perform'
-- --
-- -- > [(Time, Duration, a)] -> Score a
-- --
-- compose :: (Composable s, d ~ Duration s, t ~ Time s) => [(t, d, a)] -> s a
-- compose = msum . liftM eventToScore
-- 
-- -- |
-- -- Map over all events in a score.
-- --
-- -- > ([(Time, Duration, a)] -> [(Time, Duration, b)]) -> Score a -> Score b
-- --
-- mapAllEvents :: (HasEvents s, d ~ Duration s, t ~ Time s) => ([(t, d, a)] -> [(t, d, b)]) -> s a -> s b
-- mapAllEvents f = compose . f . perform
-- 
-- {-
-- mapFilterAllEvents :: (HasEvents s, d ~ Duration s, t ~ Time s) => ([(t, d, a)] -> [(t, d, Maybe b)]) -> s a -> s b
-- mapFilterAllEvents f = mcatMaybes . mapAllEvents f
-- -}
-- 
-- -- |
-- -- Map over the events in a score.
-- --
-- -- > (Time -> Duration -> a -> b) -> Score a -> Score b
-- --
-- filterEvents :: (MAP_CONSTRAINT, t ~ Time s, d ~ Duration s) => (t -> d -> a -> Bool) -> s a -> s a
-- filterEvents f = mapFilterEvents (partial3 f)
-- 
-- -- |
-- -- Map over the events in a score.
-- --
-- -- > (Time -> Duration -> a -> b) -> Score a -> Score b
-- --
-- mapFilterEvents :: (MAP_CONSTRAINT, t ~ Time s, d ~ Duration s) => (t -> d -> a -> Maybe b) -> s a -> s b
-- mapFilterEvents f = mcatMaybes . mapAllParts (liftM $ mapEventsSingle f)
-- 
-- -- |
-- -- Map over the events in a score.
-- --
-- -- > (Time -> Duration -> a -> b) -> Score a -> Score b
-- --
-- mapEvents :: (MAP_CONSTRAINT, t ~ Time s, d ~ Duration s) => (t -> d -> a -> b) -> s a -> s b
-- mapEvents f = mapAllParts (liftM $ mapEventsSingle f)
-- 
-- -- |
-- -- Equivalent to 'mapEvents' for single-voice scores.
-- -- Fails if the score contains overlapping events.
-- --
-- -- > (Time -> Duration -> a -> b) -> Score a -> Score b
-- --
-- mapEventsSingle :: (HasEvents s, t ~ Time s, d ~ Duration s) => (t -> d -> a -> b) -> s a -> s b
-- mapEventsSingle f sc = compose . fmap (third' f) . perform $ sc
-- 
-- -- |
-- -- Equivalent to 'mapEvents' for single-voice scores.
-- -- Fails if the score contains overlapping events.
-- --
-- -- > ([(Time,Duration,a)] -> [b]) -> Score a -> Score b
-- --
-- -- mapAllEventsSingle :: (HasEvents s, t ~ Time s, d ~ Duration s) => ([(t,d,a)] -> b) -> s a -> s b
-- -- mapAllEventsSingle f sc = compose . fmap trd3 . f . perform $ sc
-- -- mapAllEventsSingle' :: (HasEvents s, t ~ Time s, d ~ Duration s) => ([(t,d,a)] -> [b]) -> s a -> s b
-- -- mapAllEventsSingle' f = compose . fmap trd3 . f . perform
-- 
-- mapAllEventsSingle' :: (HasEvents s, t ~ Time s, d ~ Duration s) => ([(t,d,a)] -> [(t,d,b)]) -> s a -> s b
-- mapAllEventsSingle' f = compose . f . perform
-- 
-- -- |
-- -- Map over the first, and remaining notes in each part.
-- --
-- -- If a part has only one notes, the first function is applied.
-- -- If a part has no notes, the given score is returned unchanged.
-- --
-- -- > (a -> b) -> (a -> b) -> Score a -> Score b
-- --
-- mapFirst :: (MAP_CONSTRAINT) => (a -> b) -> (a -> b) -> s a -> s b
-- mapFirst f g = mapPhrase f g g
-- 
-- -- |
-- -- Map over the last, and preceding notes in each part.
-- --
-- -- If a part has only one notes, the first function is applied.
-- -- If a part has no notes, the given score is returned unchanged.
-- --
-- -- > (a -> b) -> (a -> b) -> Score a -> Score b
-- --
-- mapLast :: (MAP_CONSTRAINT) => (a -> b) -> (a -> b) -> s a -> s b
-- mapLast f g = mapPhrase g g f
-- 
-- -- |
-- -- Map over the first, middle and last note in each part.
-- --
-- -- If a part has fewer than three notes the first takes precedence over the last,
-- -- and last takes precedence over the middle.
-- --
-- -- > (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
-- --
-- mapPhrase :: (MAP_CONSTRAINT) => (a -> b) -> (a -> b) -> (a -> b) -> s a -> s b
-- mapPhrase f g h = mapAllParts (liftM $ mapPhraseSingle f g h)
-- 
-- -- |
-- -- Equivalent to 'mapPhrase' for single-voice scores.
-- -- Fails if the score contains overlapping events.
-- --
-- -- > (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
-- --
-- mapPhraseSingle :: HasEvents s => (a -> b) -> (a -> b) -> (a -> b) -> s a -> s b
-- mapPhraseSingle f g h sc = compose . mapFirstMiddleLast (third f) (third g) (third h) . perform $ sc
-- 
-- eventToScore
--   :: (Monad s, 
--       Transformable1 s,
--       Time s ~ t, Duration s ~ d
--       ) => (t, d, a) -> s a
-- 
-- eventToScore (t,d,x) = delay' t . stretch d $ return x   

align           :: (AffineSpace (Time a), HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a
(|>)            :: (Semigroup a, AffineSpace (Time a), HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a
(<|)            :: (Semigroup a, AffineSpace (Time a), HasOnset a, HasOffset a, Delayable a) =>
                a -> a -> a
scat            :: (Monoid' a, AffineSpace (Time a), HasOnset a, HasOffset a, Delayable a) =>
                [a] -> a
pcat            :: Monoid' a =>
                [a] -> a

compress        :: (Stretchable a, Fractional d, d ~ Duration a) =>
                d -> a -> a
stretchTo       :: (Stretchable a, HasDuration a, Fractional d, d ~ Duration a) =>
                d -> a -> a
sustain         :: (Semigroup a, Stretchable a, HasDuration a, Fractional d, d ~ Duration a) =>
                a -> a -> a
anticipate      :: (Semigroup a, Transformable a, HasOnset a, HasOffset a, Ord d, d ~ Duration a) =>
                d -> a -> a -> a
move            :: (Delayable a, d ~ Duration a) =>
                d -> a -> a
moveBack        :: (Delayable a, AdditiveGroup d, d ~ Duration a) =>
                d -> a -> a
startAt         :: (HasOnset a, Delayable a, AffineSpace t, t ~ Time a) =>
                t ->  a -> a
stopAt          :: (HasOffset a, Delayable a, AffineSpace t, t ~ Time a) =>
                t -> a -> a

rest            :: MonadPlus m =>
                m (Maybe a)
removeRests     :: MonadPlus m =>
                m (Maybe a) -> m a

times           :: (Monoid' a, Transformable a) =>
                Int -> a -> a
repeated        :: (Monoid' b, Transformable b) =>
                [a] -> (a -> b) -> b
group           :: (Monoid' a, Transformable a, Fractional d, d ~ Duration a) =>
                Int -> a -> a


retrograde      :: (Performable a, Composable a, HasOnset a, Ord (Duration a)) =>
                a -> a
     
mapAll          :: (Performable a, Composable b, Duration a ~ Duration b) => 
                ([(Time a, Duration a, Event a)] -> [(Time b, Duration b, Event b)]) -> a -> b

mapEvents       :: (Performable a, Composable b, Duration a ~ Duration b) =>
                (Time a -> Duration a -> Event a -> Event b) -> a -> b

filterEvents   :: (Performable a, Composable a) =>
                (Time a -> Duration a -> Event a -> Bool) -> a -> a

filter_         :: (Performable a, Composable a) => (Event a -> Bool) -> a -> a

mapFilterEvents :: (Performable a, Composable b, Duration a ~ Duration b) =>
                (Time a -> Duration a -> Event a -> Maybe (Event b)) -> a -> b

mapPhraseSingle      :: (Performable a, Composable b, Duration a ~ Duration b) =>
                (Event a -> Event b) -> (Event a -> Event b) -> (Event a -> Event b) -> a -> b


slice           :: (Ord (Duration a), AdditiveGroup (Duration a), Performable a, Composable a) =>
                Time a -> Time a -> a -> a
after           :: (Ord (Duration a), AdditiveGroup (Duration a), Performable a, Composable a) =>
                Time a -> a -> a
before          :: (Ord (Duration a), AdditiveGroup (Duration a), Performable a, Composable a) =>
                Time a -> a -> a

type Phraseable a b = (Performable a, Composable a, Composable b, Semigroup b,
                       HasPart' (Event a), Duration a ~ Duration b)

mapPhrase       :: (Performable a, Composable a, Composable b, Semigroup b,
                    HasPart' (Event a), Duration a ~ Duration b
                ) =>
                (Event a -> Event b) -> (Event a -> Event b) -> (Event a -> Event b) -> a -> b

mapAllParts     :: (Monoid' b, HasPart' (Event a), Performable a, Composable a) => 
                 ([a] -> [b]) -> a -> b


move            = delay
moveBack t      = delay (negateV t)
compress x      = stretch (recip x)
t `stretchTo` x = (t / duration x) `stretch` x
t `startAt` x   = (t .-. onset x) `delay` x
t `stopAt`  x   = (t .-. offset x) `delay` x

a `align` b =  startAt (offset a) b

a |> b =  a <> a `align` b
a <| b =  b |> a
scat = Prelude.foldr (|>) mempty
pcat = Prelude.foldr (<>) mempty

x `sustain` y     = x <> duration x `stretchTo` y
anticipate t a b  =  a <> startAt (offset a .-^ t) b

rest            = return Nothing
removeRests     = mcatMaybes

retrograde = startAt origin . (mapAll $ List.sortBy (comparing fst3) . fmap g)
    where
        g (t,d,x) = (negateP (t .+^ d), d, x)
        negateP a = origin .-^ (a .-. origin)


mapAll f                    = compose . f . perform
mapEvents f                 = mapAll $ fmap (third' f)
mapFilterEvents f           = mapAll $ mcatMaybes . fmap (unM . third' f)
    where
        unM (a,b,Nothing) = Nothing
        unM (a,b,Just c)  = Just (a,b,c)
filterEvents f = mapFilterEvents (partial3 f)
filter_ p = filterEvents (\t d x -> p x)

after  a                    = filterEvents (\t d _ -> a <= t)
before b                    = filterEvents (\t d _ -> t .+^ d <= b) 
slice  a b                  = filterEvents (\t d _ -> a <= t && t .+^ d <= b)

mapPhraseSingle f g h       = mapAll (mapFirstMiddleLast (third f) (third g) (third h))
mapPhrase f g h             = mapAllParts (fmap $ mapPhraseSingle f g h)

mapAllParts f   = mconcat . f . extractParts

extractParts a = fmap (`extractPart` a) (getParts a)
    where
        extractPart v = filter_ ((== v) . getPart)

getParts = List.sort . List.nub . fmap getPart . events

extractParts' :: (HasPart' (Event a), Composable a, Performable a) => a -> [(Part (Event a), a)]
extractParts' = undefined


times = undefined
repeated = undefined
group = undefined

                                                                 
-------------------------------------------------------------------------------------
-- Zippers

-- |
-- Apply a time-varying function to all events in score.
--
apply :: HasPart' a => Voice (Score a -> Score b) -> Score a -> Score b
apply x = mapAllParts (fmap $ applySingle x)

-- |
-- Get all notes that start during a given note.
--
snapshot :: HasPart' a => Score b -> Score a -> Score (b, Score a)
snapshot x = mapAllParts (fmap $ snapshotSingle x)

-- |
-- Apply a time-varying function to all events in score.
--
applySingle :: Voice (Score a -> Score b) -> Score a -> Score b
applySingle fs as = notJoin $ fmap (uncurry ($)) $ sample fs $ as
    where
        -- This is not join; we simply concatenate all inner scores in parallel
        notJoin   = mconcat . Foldable.toList
        sample fs = snapshotSingle (voiceToScore fs)

-- |
-- Get all notes that start during a given note.
--
snapshotSingle :: Score a -> Score b -> Score (a, Score b)
snapshotSingle = snapshotSingleWith (,)

snapshotSingleWith :: (a -> Score b -> c) -> Score a -> Score b -> Score c
snapshotSingleWith g as bs = mapEvents ( \t d a -> g a (onsetIn t d bs) ) as


-- |
-- Filter out events that has its onset in the given time interval (inclusive start).
-- For example, onset in 1 2 filters events such that (1 <= onset x < 3)
-- onsetIn :: Time Score -> Duration Score -> Score a -> Score a
onsetIn = undefined
-- onsetIn a b = compose . filter' (\(t,d,x) -> a <= t && t < a .+^ b) . perform
    -- where
        -- filter' = filterOnce
        -- more lazy than mfilter

-- |
-- Extract the first consecutive sublist for which the predicate returns true, or
-- the empty list if no such sublist exists.
filterOnce :: (a -> Bool) -> [a] -> [a]
filterOnce p = List.takeWhile p . List.dropWhile (not . p)


-- |
-- Extract parts from the a score.
--
-- The parts are returned in the order defined the associated 'Ord' instance part type.
-- You can recompose the score with 'mconcat', i.e.
--
-- > mconcat . extract = id
--
-- Simple type
--
-- > Score a -> [Score a]
--
extractParts :: (HasPart' (Event a), Composable a, Performable a) => a -> [a]
-- extractParts a = fmap (`extractPart` a) (getParts a)
--     where
--         extractPart v = filter_ ((== v) . getPart)
--         getParts = List.sort . List.nub . fmap getPart . events




        
-- |
-- Extract parts from the a score.
--
-- The parts are returned in the order defined the associated 'Ord' instance part type.
--
-- Simple type
--
-- > Score a -> [(Part a, Score a)]
--


-- |
-- Map over a single voice in the given score.
--
-- > Part -> (Score a -> Score a) -> Score a -> Score a
--
mapPart = undefined

-- mapPart :: (Ord v, v ~ Part a, HasPart a, MonadPlus s, Performable s, Enum b) => b -> (s a -> s a) -> s a -> s a
-- mapPart n f = mapAllParts (zipWith ($) (replicate (fromEnum n) id ++ [f] ++ repeat id))

-- |
-- Map over all parts in the given score.
--
-- > ([Score a] -> [Score a]) -> Score a -> Score a
--
-- mapAllParts f   = mconcat . f . extractParts

-- |
-- Map over all parts in the given score.
--
-- > ([Score a] -> [Score a]) -> Score a -> Score a
--
mapParts = undefined
-- mapParts :: (HasPart' a, MonadPlus s, Performable s) => (s a -> s b) -> s a -> s b
-- mapParts f = mapAllParts (fmap f)

-- |
-- Get all parts in the given score. Returns a list of parts.
--
-- > Score a -> [Part]
--    
(setParts, modifyParts) = undefined
-- getParts :: (HasPart' a, Performable s) => s a -> [Part a]
-- getParts = List.sort . List.nub . fmap getPart . events

-- |
-- Set all parts in the given score.
--
-- > Part -> Score a -> Score a
--
-- setParts :: (HasPart a, Functor s) => Part a -> s a -> s a
-- setParts n = fmap (setPart n)

-- |
-- Modify all parts in the given score.
--
-- > (Part -> Part) -> Score a -> Score a
--
-- modifyParts :: (HasPart a, Functor s) => (Part a -> Part a) -> s a -> s a
-- modifyParts n = fmap (modifyPart n)



--------------------------------------------------------------------------------
-- Part composition
--------------------------------------------------------------------------------

infixr 6 </>

-- |
-- Similar to '<>', but increases parts in the second part to prevent collision.
--
(</>) :: (HasPart' (Event a), Enum (Part (Event a)), Performable a, Composable a) => a -> a -> a
(</>) = undefined
-- a </> b = a `mplus` moveParts offset b
--     where
--         -- max voice in a + 1
--         offset = succ $ maximum' 0 $ fmap fromEnum $ getParts a

-- |
-- Move down one voice (all parts).
--
moveParts :: (HasPart' a, Enum (Part a), Integral b, Functor s) => b -> s a -> s a
moveParts = undefined
-- moveParts x = modifyParts (successor x)

-- |
-- Move top-part to the specific voice (other parts follow).
--
moveToPart :: (HasPart' a, Enum (Part a), Functor s) => Part a -> s a -> s a
moveToPart
 = undefined
-- moveToPart v = moveParts (fromEnum v)




-------------------------------------------------------------------------------------

successor :: (Integral b, Enum a) => b -> a -> a
successor n | n <  0 = (!! fromIntegral (abs n)) . iterate pred
            | n >= 0 = (!! fromIntegral n)       . iterate succ

maximum' :: (Ord a, Foldable t) => a -> t a -> a
maximum' z = option z getMax . foldMap (Option . Just . Max)

minimum' :: (Ord a, Foldable t) => a -> t a -> a
minimum' z = option z getMin . foldMap (Option . Just . Min)
                                                               --------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

(scoreToVoice, voiceToScore, voiceToScore') = undefined

-- |
-- Convert a score into a voice.
--
-- This function fails if the score contain overlapping events.
--
scoreToVoice :: Score a -> Voice (Maybe a)
-- scoreToVoice = voice . fmap throwTime . addRests . perform
--     where
--        throwTime (t,d,x) = (d,x)
--        addRests = concat . snd . mapAccumL g 0
--            where
--                g u (t, d, x)
--                    | u == t    = (t .+^ d, [(t, d, Just x)])
--                    | u <  t    = (t .+^ d, [(u, t .-. u, Nothing), (t, d, Just x)])
--                    | otherwise = error "addRests: Strange prevTime"
       

-- |
-- Convert a voice into a score.
--
voiceToScore :: Voice a -> Score a
-- voiceToScore = scat . fmap g . getVoice
--     where
--         g (d,x) = stretch d (note x)

-- |
-- Convert a voice which may contain rests into a score.
--
voiceToScore' :: Voice (Maybe a) -> Score a
-- voiceToScore' = mcatMaybes . voiceToScore

-- TODO move this instance
instance Performable (Voice a) where
    perform = undefined
    -- perform = perform . voiceToScore


--------------------------------------------------------------------------------

-- |
-- Map over first, middle and last elements of list.
-- Biased on first, then on first and last for short lists.
--
mapFirstMiddleLast :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapFirstMiddleLast f g h = go
    where
        go []    = []
        go [a]   = [f a]
        go [a,b] = [f a, h b]
        go xs    = [f $ head xs]          ++ 
                   map g (tail $ init xs) ++ 
                   [h $ last xs]

delay' t = delay (t .-. zeroV)

fst3 (t, d, x) = t
trd3 (a,b,c) = c

third f (a,b,c) = (a,b,f c)
third' f (a,b,c) = (a,b,f a b c)

rotl []     = []
rotl (x:xs) = xs ++ [x]

rotr [] = []
rotr xs = last xs : init xs


curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 = curry . curry . (. trip)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 = (. untrip) . uncurry . uncurry

untrip (a,b,c) = ((a,b),c)
trip ((a,b),c) = (a,b,c)

partial2 :: (a -> b -> Bool)      -> a -> b -> Maybe b
partial3 :: (a -> b -> c -> Bool) -> a -> b -> c -> Maybe c
partial2 f = curry  (fmap snd  . partial (uncurry f))
partial3 f = curry3 (fmap trd3 . partial (uncurry3 f))

rotated :: Int -> [a] -> [a]
rotated = go
    where
        go n as 
            | n >= 0 = iterate rotr as !! n
            | n <  0 = iterate rotl as !! abs n

