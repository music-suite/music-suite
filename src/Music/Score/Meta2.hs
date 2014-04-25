
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

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
-- Combinators for manipulating scores and related structures.
--
-------------------------------------------------------------------------------------

module Music.Score.Meta2 (
--         -- * Basic
--         -- note,
--         -- rest,
--         -- noteRest,
--         removeRests,
-- 
--         -- * Mapping over events
--         mapEvents,
-- 
--         -- * Filtering events
-- 
--         -- ** Editing
--         filterEvents,
--         mapFilterEvents,
-- 
-- {-
--         -- * Editing
--         before,
--         after,
--         split,
--         slice,
--         splice,
-- -}
-- 
        -- * Meta-events
        metaAt,
        metaAtStart,
        withMeta,
        withGlobalMeta,
        withMetaAtStart,
        withGlobalMetaAtStart,




        -- -- ** Map over phrases
        --  mapFirst,
        --  mapLast,
        --  mapPhrase,
        --  mapPhraseSingle,
        -- 
        --  -- * Parts
        --  -- ** Extracting parts
        --  filterPart,
        --  extractParts,
        --  extractParts',
        -- 
        --  -- ** Map over parts
        --  -- mapPart,
        --  mapParts,
        --  mapAllParts,
        --  -- modifyParts,
        -- 
        --  -- ** Part composition
        --  (</>),
        --  rcat,
        --  -- moveParts,
        --  -- moveToPart,
        -- 
        --  -- * Zippers
        --  -- apply,
        --  -- snapshot,
        --  -- snapshotWith,
        -- 
        --  -- ** Single-part versions
        --  applySingle,
        --  -- snapshotSingle,
        --  -- snapshotWithSingle,
   ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens           hiding (perform)
import           Control.Monad
import           Control.Monad.Plus
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Foldable          (Foldable (..))
import           Data.Ord
import           Data.Ratio
import           Data.Semigroup
import           Data.String
import           Data.Traversable
import           Data.VectorSpace

import           Music.Score.Meta
import           Music.Score.Part
import           Music.Score.Util
import           Music.Time
import           Music.Time.Reactive

import qualified Data.Foldable          as Foldable
import qualified Data.List              as List

{-
-- | Create a score containing a note at time zero and duration one. This is an alias for 'return'.
note :: Monad m => a -> m a
note = return

-- | Create a score containing a rest at time zero and duration one. This is an alias for @'return' 'Nothing'@.
rest :: MonadPlus m => m (Maybe a)
rest = return Nothing

-- | Create a note or a rest at time zero and duration one. This is an alias for 'mfromMaybe'.
noteRest :: MonadPlus m => Maybe a -> m a
noteRest = mfromMaybe

-- | Remove all rests from a score. This is an alias for 'mcatMaybes'.
removeRests :: MonadPlus m => m (Maybe a) -> m a
removeRests = mcatMaybes

-- | Retain only the notes whose /offset/ does not fall after the given time.
before :: Time -> Score a -> Score a
before u = filterEvents (\t d _ -> t .+^ d <= u)

-- | Retain only the notes whose /onset/ does not fall before the given time.
after :: Time -> Score a -> Score a
after u = filterEvents (\t d _ -> u <= t)

-- | Returns notes whose /onset/ and /offset/ fall between the given times.
slice :: Time -> Time -> Score a -> Score a
slice u v = filterEvents (\t d _ -> u <= t && t .+^ d <= v)

-- | Split a score into events whose onsets
split :: Time -> Score a -> (Score a, Score a)
split t a = (before t a, after t a)

-- | Split a score into three parts
splice :: Time -> Duration -> Score a -> (Score a, Score a, Score a)
splice t d a = tripr (before t a, split (t .+^ d) a)





-- |
-- Map over the first, and remaining notes in each part.
--
-- If a part has only one notes, the first function is applied.
-- If a part has no notes, it is returned unchanged.
--
mapFirst :: HasPart' a => (a -> b) -> (a -> b) -> Score a -> Score b
mapFirst f g = mapPhrase f g g

-- |
-- Map over the last, and preceding notes in each part.
--
-- If a part has only one notes, the first function is applied.
-- If a part has no notes, it is returned unchanged.
--
mapLast :: HasPart' a => (a -> b) -> (a -> b) -> Score a -> Score b
mapLast f g = mapPhrase g g f

-- |
-- Map over the first, middle and last note in each part.
--
-- If a part has fewer than three notes the first takes precedence over the last,
-- and last takes precedence over the middle.
--
mapPhrase :: HasPart' a => (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapPhrase f g h = mapAllParts (fmap $ mapPhraseSingle f g h)

-- |
-- Equivalent to 'mapPhrase' for single-part scores.
--
-- Fails if the score contains overlapping events.
--
-- > (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
--
mapPhraseSingle :: (a -> b) -> (a -> b) -> (a -> b) -> Score a -> Score b
mapPhraseSingle f g h = mapAll (mapFTL (_3 %~ f) (_3 %~ g) (_3 %~ h))



-- |
-- Map over all events in a score.
--
mapAll :: ([(Time, Duration, a)] -> [(Time, Duration, b)]) -> Score a -> Score b
mapAll f = saveMeta $ over events f
    where
        saveMeta f x = (meta .~) ((view meta) x) $ f x


-}

--------------------------------------------------------------------------------
-- Parts
--------------------------------------------------------------------------------
{-

-- |
-- Filter a score to include only those events whose parts match a given predicate.
--
filterPart :: HasPart' a => (Part a -> Bool) -> Score a -> Score a
filterPart p = mfilter (p . getPart)

filterPartIs :: HasPart' a => Part a -> Score a -> Score a
filterPartIs = filterPart <$> (==)

-- |
-- Extract parts from the a score.
--
-- The parts are returned in the order defined the associated 'Ord' instance part type.
--
extractParts :: HasPart' a => Score a -> [Score a]
extractParts x = filterPartIs <$> getParts x <*> return x

-- |
-- Extract parts from the a score and include the part name.
--
-- The parts are returned in the order defined the associated 'Ord' instance part type.
--
extractParts' :: HasPart' a => Score a -> [(Part a, Score a)]
extractParts' x = getParts x `zip` extractParts x


-- |
-- Map over a specific part in the given score.
--
mapPart         :: (Enum (Part a), HasPart' a) => Part a -> (Score a -> Score a) -> Score a -> Score a

-- |
-- Map over all parts in the given score.
--
-- > (Score a -> Score a) -> Score a -> Score a
--
mapParts        :: HasPart' a => (Score a -> Score b) -> Score a -> Score b

-- |
-- Map over all parts in the given score.
--
-- > ([Score a] -> [Score a]) -> Score a -> Score a
--
mapAllParts     :: HasPart' a => ([Score a] -> [Score b]) -> Score a -> Score b


mapPart n f     = mapAllParts (zipWith ($) (replicate (fromEnum n) id ++ [f] ++ repeat id))
mapParts f      = mapAllParts (fmap f)
mapAllParts f   = mconcat . f . extractParts

-- |
-- Modify all parts in the given score.
--
-- > (Part -> Part) -> Score a -> Score a
--
modifyParts :: HasPart' a => (Part a -> Part a) -> Score a -> Score a
modifyParts n = fmap (modifyPart n)
-}



--------------------------------------------------------------------------------
-- Part composition
--------------------------------------------------------------------------------
{-

infixr 6 </>

-- |
-- Similar to '<>', but increases parts in the second part to prevent collision.
--
(</>) :: (HasPart' a, Enum (Part a)) => Score a -> Score a -> Score a
a </> b = a <> moveParts offset b
    where
        -- max voice in a + 1
        offset = succ $ maximum' 0 $ fmap fromEnum $ getParts a

-- |
-- Concatenate parts.
--
rcat :: (HasPart' a, Enum (Part a)) => [Score a] -> Score a
rcat = List.foldr (</>) mempty

-- |
-- Move down one voice (all parts).
--
moveParts :: (Integral b, HasPart' a, Enum (Part a)) => b -> Score a -> Score a
moveParts x = modifyParts (successor x)

-- |
-- Move top-part to the specific voice (other parts follow).
--
moveToPart :: (Enum b, HasPart' a, Enum (Part a)) => b -> Score a -> Score a
moveToPart v = moveParts (fromEnum v)



-------------------------------------------------------------------------------------
-- Zippers

-- |
-- Apply a time-varying function to all events in score.
--
apply :: HasPart' a => Voice (Score a -> Score b) -> Score a -> Score b
apply x = mapAllParts (fmap $ applySingle x)

-- |
-- Apply a time-varying function to all events in score.
--
applySingle :: Voice (Score a -> Score b) -> Score a -> Score b
applySingle fs = notJoin . fmap (uncurry ($)) . sample fs
    where
        notJoin   = mconcat . Foldable.toList
        sample fs = snapshotSingle (voiceToScore fs)

-- |
-- Get all notes that start during a given note.
--
snapshot :: HasPart' b => Score a -> Score b -> Score (a, Score b)
snapshot x = mapAllParts (fmap $ snapshotSingle x)

snapshotWith :: HasPart' b => (a -> Score b -> c) -> Score a -> Score b -> Score c
snapshotWith f x = mapAllParts (fmap $ snapshotWithSingle f x)

-- |
-- Get all notes that start during a given note.
--
snapshotSingle :: Score a -> Score b -> Score (a, Score b)
snapshotSingle = snapshotWithSingle (,)

snapshotWithSingle :: (a -> Score b -> c) -> Score a -> Score b -> Score c
snapshotWithSingle g as bs = mapEvents ( \t d a -> g a (onsetIn t d bs) ) as

-- |
-- Filter out events that has its onset in the given time interval (inclusive start).
-- For example, onset in 1 2 filters events such that (1 <= onset x < 3)
--
onsetIn :: Time -> Duration -> Score a -> Score a
onsetIn a b = mapAll $ filterOnce (\(t,d,x) -> a <= t && t < a .+^ b)
-- We could also have used mfilter. filterOnce is more lazy,
-- but depends on the events being sorted






-}

withSpan :: Score a -> Score (Span, a)
withSpan = mapEvents (\t d x -> (t >-> d,x))
withTime = mapEvents (\t d x -> (t, x))

inSpan t' (view range -> (t,u)) = t <= t' && t' < u

-- TODO clean
mapBefore :: Time -> (Score a -> Score a) -> Score a -> Score a
mapDuring :: Span -> (Score a -> Score a) -> Score a -> Score a
mapAfter :: Time -> (Score a -> Score a) -> Score a -> Score a
mapBefore t f x = let (y,n) = (fmap snd *** fmap snd) $ mpartition (\(t2,x) -> t2 < t) (withTime x) in (f y <> n)
mapDuring s f x = let (y,n) = (fmap snd *** fmap snd) $ mpartition (\(t,x) -> t `inSpan` s) (withTime x) in (f y <> n)
mapAfter t f x = let (y,n) = (fmap snd *** fmap snd) $ mpartition (\(t2,x) -> t2 >= t) (withTime x) in (f y <> n)

-- TODO
type HasParts'' a = (HasParts' a, Ord (Part a), Show (Part a))

-- Transform the score with the current value of some meta-information
-- Each "update chunk" of the meta-info is processed separately

runScoreMeta :: forall a b . (HasParts'' a, IsAttribute b) => Score a -> Reactive b
runScoreMeta = runMeta (Nothing :: Maybe a) . (view meta)

metaAt :: (HasParts'' a, IsAttribute b) => Time -> Score a -> b
metaAt x = (`atTime` x) . runScoreMeta

metaAtStart :: (HasParts'' a, IsAttribute b) => Score a -> b
metaAtStart x = _onset x `metaAt` x

withGlobalMeta :: IsAttribute a => (a -> Score b -> Score b) -> Score b -> Score b
withGlobalMeta = withMeta' (Nothing :: Maybe Int)

withMeta :: (IsAttribute a, HasParts'' b) => (a -> Score b -> Score b) -> Score b -> Score b
withMeta f x = withMeta' (Just x) f x

withMeta' :: (HasParts'' c, IsAttribute a) => Maybe c -> (a -> Score b -> Score b) -> Score b -> Score b
withMeta' part f x = let
    m = (view meta) x
    r = runMeta part m
    in case splitReactive r of
        Left  a -> f a x
        Right ((a, t), bs, (u, c)) ->
            (meta .~) m
                $ mapBefore t (f a)
                $ (composed $ fmap (\(getNote -> (s, a)) -> mapDuring s $ f a) $ bs)
                $ mapAfter u (f c)
                $ x

withGlobalMetaAtStart :: IsAttribute a => (a -> Score b -> Score b) -> Score b -> Score b
withGlobalMetaAtStart = withMetaAtStart' (Nothing :: Maybe Int)

withMetaAtStart :: (IsAttribute a, HasParts'' b) => (a -> Score b -> Score b) -> Score b -> Score b
withMetaAtStart f x = withMetaAtStart' (Just x) f x

withMetaAtStart' :: (IsAttribute b, HasParts'' p) =>
    Maybe p -> (b -> Score a -> Score a) -> Score a -> Score a
withMetaAtStart' part f x = let
    m = (view meta) x
    in f (runMeta part m `atTime` _onset x) x





-------------------------------------------------------------------------------------


-- partial2 :: (a -> b -> Bool)      -> a -> b -> Maybe b
-- partial3 :: (a -> b -> c -> Bool) -> a -> b -> c -> Maybe c
-- partial2 f = curry  (fmap snd  . partial (uncurry f))
-- partial3 f = curry3 (fmap (^. _3) . partial (uncurry3 f))

iterating :: (a -> a) -> (a -> a) -> Int -> a -> a
iterating f g n
    | n <  0 = f . iterating f g (n + 1)
    | n == 0 = id
    | n >  0 = g . iterating f g (n - 1)

successor :: (Integral b, Enum a) => b -> a -> a
successor n = iterating pred succ (fromIntegral n)

maximum' :: (Ord a, Foldable t) => a -> t a -> a
maximum' z = option z getMax . foldMap (Option . Just . Max)

minimum' :: (Ord a, Foldable t) => a -> t a -> a
minimum' z = option z getMin . foldMap (Option . Just . Min)

